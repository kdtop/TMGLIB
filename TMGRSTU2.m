TMGRSTU2 ;TMG/kst/REST web service Utilities; 4/23/14
       ;;1.0;TMG-LIB;**1**;4/21/14
       ;
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"Code dealing with sessions, and variable enviornments etc. 
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"$$MAKSESN() ;MAKE NEW SESSION
 ;"RESTORSN(HASHID,KILLPRIOR,TMGRESULT,NODENAME) ;RESTORE SESSION VARIABLES TO TABLE.
 ;"$$FREESN(HASHID,NODENAME) ;FREE A SESSION
 ;"SAVESN(HASHID,EXCLUDELIST,NODENAME) -- SAVE CURRENT SESSION VARIABLES.
 ;"VARS2REF(DESTREF,EXCLUDELIST) ;Store current variable table to reference storage location
 ;"REF2VARS(SOURCEREF,KILLPRIOR) ;Restore variables stored in REF location to system varaible table.
 ;"MAKEENV() -- Setup expected variables in enviroment. 
 ;        
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"TOUCHREC(IEN,NODENAME) ;Refresh LastAccessed$H
 ;"GETSNIEN(HASHID,NODENAME) ; GET SESSION IEN FROM SESSION HASHID
 ;"KILLDEAD(NODENAME) ;  RELEASE any old session that has expired.
 ;"KILLONE(IEN,NODENAME) --KILL 1 SESSION INFO
 ;"SUBMIN(H,MIN) ;RETURN H-MINUTES
 ;"RANDSTR(LEN) ;Make a string of random characters of specified length, not including '^' char
 ;"RANDOM(MIN,MAX) ;RETURN A RANDOM NUMBER IN RANGE MIN..MAX (inclusive)
 ;"TEMPSAVE(VARNAMES) -- save variables to ^TMP
 ;"RSTRSAVE() -- restore after TEMPSAVE
 ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"=======================================================================
 ;"Uses: 
 ;"=======================================================================
 ;
 ;"Discussion about sessions
 ;"--All RPC calls will require a session ID.  Without them, error returned.
 ;"--The session will be stored as follows:
 ;"  ^XTMP("TMG_WEB_SESSIONS",0)=PurgeDate^CreateData^Description with creater DUZ
 ;"  ^XTMP("TMG_WEB_SESSIONS",IEN,0)=HashedSessionID^LastAccess$H
 ;"  ^XTMP("TMG_WEB_SESSIONS",IEN,"VARS",#)="Z(""sub1"",""sub2"")=1" <-- sample 
 ;"  ^XTMP("TMG_WEB_SESSIONS","B",HashedSessionID)=IEN
 ;"  ^XTMP("TMG_WEB_SESSIONS","C",LastAccess$H,IEN)=""
 ;
 ;"Why store hashed ID's rather than actual session ID?  Because a client with
 ;"  CPRS access authority might execute some RPC that can retrieve nodes in a global
 ;"  and thus a user could spoof another user.  But this way, even if they get the
 ;"  hashed ID, they won't be able to get the session ID (presumming the hash
 ;"  function is not reversible).
 ;"  ADDENDUM: I don't think the above is correct.  A  rouge user could use 
 ;"     their own copy of EN^XUSHSH to convert from ID given to client to make   
 ;"    HASHID.  However, it will provide a bit of obfuscation, so I will leave in.
 ;
GNODENAM() QUIT "TMG_WEB_SESSIONS"
        ;
MAKESN(NODENAME) ;"MAKE ENTRY FOR NEW SESSION (OR OTHER STORAGE)
        ;"Result: RETURNS SESSION ID
        SET NODENAME=$GET(NODENAME) IF NODENAME="" SET NODENAME=$$GNODENAM()
        DO KILLDEAD(NODENAME)
        IF $GET(^XTMP(NODENAME,0))="" DO
        . SET ^XTMP(NODENAME,0)="9990101^"_$$NOW^XLFDT\1_"^Environmental Data for web sessions"
        NEW HASHID,RESULT
        NEW OK SET OK=0
        FOR  DO  QUIT:(OK>0)
        . SET RESULT=$$RANDSTR(16) ;"16 DIGIT RANDOM SESSION ID
        . SET HASHID=$$EN^XUSHSH(RESULT)
        . SET OK=($GET(^XTMP(NODENAME,"B",HASHID))="")
        NEW IEN SET IEN=$ORDER(^XTMP(NODENAME,"B"),-1)+1
        NEW TIME SET TIME=$H
        SET ^XTMP(NODENAME,IEN,0)=HASHID_"^"_TIME
        SET ^XTMP(NODENAME,"B",HASHID)=IEN
        SET ^XTMP(NODENAME,"C",TIME,IEN)=""
        QUIT RESULT
        ;
RESTORSN(HASHID,KILLPRIOR,TMGRESULT,KEEPLIST,NODENAME) ;"RESTORE SESSION VARIABLES TO SYSTEM VARIABLE TABLE.
        ;"INPUT: HASHID -- a hash of session ID (from $$EN^XUSHSH(SessionID) )
        ;"       KILLPRIOR: if 1 then all other variables are killed from system variable table
        ;"       TMGRESULT: PASS BY REFERNCE.  AN OUT PARAMETER.
        ;"          1 if OK, or -1^Error Message, if problem
        ;"       KEEPLIST -- Optional. A string list of variables, separated by ',' to NOT kill  
        ;"       NODENAME -- tne node in ^XTMP to use for storage
        ;"Result: none, but RESULT var is set. 
        SET NODENAME=$GET(NODENAME) IF NODENAME="" SET NODENAME=$$GNODENAM()
        DO KILLDEAD(NODENAME)
        NEW IEN SET IEN=$$GETSNIEN(HASHID,NODENAME)
        IF IEN'>0 DO  QUIT
        . SET TMGRESULT="-1^NO SESSION; Session doesn't exist, or is expired."
        DO TOUCHREC(IEN,NODENAME)
        NEW SOURCEREF SET SOURCEREF=$NAME(^XTMP(NODENAME,IEN,"VARS"))
        DO REF2VARS(SOURCEREF,KILLPRIOR,.KEEPLIST) 
        IF $GET(U)'="^" DO MAKEENV()
        KILL TMGRESULT SET TMGRESULT=1
        QUIT
        ;
SAVESN(HASHID,EXCLUDELIST,NODENAME) ;"SAVE CURRENT SESSION VARIABLES.
        ;"INPUT: HASHID -- a hash of session ID (from $$EN^XUSHSH(SessionID) )
        ;"       EXCLUDELIST -- Optional. A string list of variables, separated by ',' to NOT SAVE  
        ;"Result: 1 if OK, or -1^Error Message, if problem 
        SET NODENAME=$GET(NODENAME) IF NODENAME="" SET NODENAME=$$GNODENAM()
        NEW IEN SET IEN=$$GETSNIEN(HASHID,NODENAME)
        IF IEN'>0 GOTO SVSDN
        DO TOUCHREC(IEN,NODENAME) ;"Refresh LastAccessed$H
        NEW REF SET REF=$NAME(^XTMP(NODENAME,IEN,"VARS"))
        DO VARS2REF(REF,.EXCLUDELIST)
SVSDN   QUIT
        ;
FREESN(HASHID,NODENAME) ;"FREE A SESSION
        ;"INPUT: HASHID -- a hash of session ID (from $$EN^XUSHSH(SessionID) )
        ;"Result: 1 if OK, or -1^Error Message, if problem
        SET NODENAME=$GET(NODENAME) IF NODENAME="" SET NODENAME=$$GNODENAM()
        NEW IEN SET IEN=$$GETSNIEN(HASHID,NODENAME)
        NEW RESULT
        IF IEN'>0 SET RESULT="-1^NO SESSION; Session doesn't exist, or is expired."
        ELSE  DO
        . SET RESULT=1
        . DO KILLONE(IEN,NODENAME) ;"KILL 1 SESSION INFO
        QUIT RESULT
        ;
TOUCHREC(IEN,NODENAME) ;"Refresh LastAccessed$H
        ;"INPUT: IEN -- record in pseudo file in ^XTMP
        SET NODENAME=$GET(NODENAME) IF NODENAME="" SET NODENAME=$$GNODENAM()
        SET IEN=+$GET(IEN) IF IEN'>0 QUIT
        NEW OLDTIME SET OLDTIME=$PIECE($GET(^XTMP(NODENAME,IEN,0)),"^",2)
        IF OLDTIME'="" KILL ^XTMP(NODENAME,"C",OLDTIME,IEN)
        SET $PIECE(^XTMP(NODENAME,IEN,0),"^",2)=$H
        SET ^XTMP(NODENAME,"C",$H,IEN)=""
        QUIT
        ;
GETSNIEN(HASHID,NODENAME) ; "GET SESSION IEN FROM SESSION HASHID
        ;"INPUT: HASHID -- a hash of session ID (from $$EN^XUSHSH(SessionID) )
        ;"RESULT: IEN IN ^XTMP pseudo file, OR -1 if problem. 
        SET NODENAME=$GET(NODENAME) IF NODENAME="" SET NODENAME=$$GNODENAM()
        NEW RESULT SET RESULT=-1
        IF HASHID'="" DO
        . NEW TMP SET TMP=$GET(^XTMP(NODENAME,"B",HASHID))     
        . IF TMP'="" SET RESULT=TMP
        QUIT RESULT
        ;
KILLDEAD(NODENAME) ;  "RELEASE any old session that has expired.
        SET NODENAME=$GET(NODENAME) IF NODENAME="" SET NODENAME=$$GNODENAM()
        NEW TIMEOUT SET TIMEOUT=120 ;"HARD CODED TIMEOUT OF 2 HRS.  COULD PARAMETERIZE LATER...
        NEW CUTOFFH SET CUTOFFH=$$SUBMIN($H,TIMEOUT)
        FOR  SET CUTOFFH=$ORDER(^XTMP(NODENAME,"C",CUTOFFH),-1) QUIT:(CUTOFFH="")  DO
        . NEW IEN SET IEN=""
        . FOR  SET IEN=$ORDER(^XTMP(NODENAME,"C",CUTOFFH,IEN)) QUIT:(IEN="")  DO
        . . DO KILLONE(IEN,NODENAME)
        QUIT
        ;
KILLONE(IEN,NODENAME) ;"KILL 1 SESSION INFO
        ;"INPUT -- IEN -- record number in pseudofile in ^XTMP
        SET IEN=+$GET(IEN)
        NEW HASHID SET HASHID=$PIECE($GET(^XTMP(NODENAME,IEN,0)),"^",1)
        IF HASHID'="" KILL ^XTMP(NODENAME,"B",HASHID)
        NEW TIME SET TIME=$PIECE($GET(^XTMP(NODENAME,IEN,0)),"^",2)
        IF TIME'="" KILL ^XTMP(NODENAME,"C",TIME,IEN)
        KILL ^XTMP(NODENAME,IEN)
        QUIT
        ;        
SUBMIN(H,MIN) ;"RETURN H-MINUTES
        NEW RESULT        
        NEW SUBSEC SET SUBSEC=MIN*60
        FOR  DO  QUIT:(SUBSEC'>0)
        . NEW DAYS SET DAYS=+H
        . NEW SEC SET SEC=$PIECE(H,",",2)
        . IF SEC>SUBSEC DO
        . . SET RESULT=DAYS_","_(SEC-SUBSEC)
        . . SET SUBSEC=0
        . ELSE  DO
        . . SET SUBSEC=SUBSEC-SEC
        . . SET DAYS=DAYS-1
        . . SET H=DAYS_",86399"
        QUIT RESULT
        ;
VARS2REF(DESTREF,EXCLUDELIST) ;"Store current variable table to reference storage location
        ;"INPUT: DESTREF.  PASS BY NAME.  Name of storage location of vars. MUST BE VALID.
        ;"       EXCLUDELIST -- Optional. A string list of variables, separated by ',' to NOT SAVE  
        ;"OUTPUT  @DESTREF@(#)=<variable value expression>
        KILL @DESTREF
        SET EXCLUDELIST=$GET(EXCLUDELIST)
        SET EXCLUDELIST=EXCLUDELIST_",DESTREF,EXCLUDELIST"
        NEW ZZTMGTEMPVARS
        ZSHOW "V":ZZTMGTEMPVARS   ;"<--- how to do this for Cache'?
        IF $EXTRACT(EXCLUDELIST,1)'="," SET EXCLUDELIST=","_EXCLUDELIST 
        IF $EXTRACT(EXCLUDELIST,$LENGTH(EXCLUDELIST))'="," SET EXCLUDELIST=EXCLUDELIST_"," 
        NEW VNUM SET VNUM=""
        FOR  SET VNUM=$ORDER(ZZTMGTEMPVARS("V",VNUM)) QUIT:+VNUM'>0  DO
        . NEW LINE SET LINE=$GET(ZZTMGTEMPVARS("V",VNUM)) QUIT:LINE=""
        . NEW VAR SET VAR=$PIECE($PIECE(LINE,"=",1),"(",1) QUIT:VAR=""
        . NEW COMPVAR SET COMPVAR=","_VAR_","
        . NEW SHOULDKILL SET SHOULDKILL=(EXCLUDELIST[COMPVAR)
        . IF SHOULDKILL DO
        . . KILL ZZTMGTEMPVARS("V",VNUM)
        . . ;"DO DBGMSG^TMGRST01("Killing VAR at index: "_VNUM)
        ;" NEW IDX FOR IDX=1:1:$LENGTH(EXCLUDELIST,",") DO
        ;" . NEW EXCLVAR SET EXCLVAR=$PIECE(EXCLUDELIST,",",IDX) QUIT:EXCLVAR=""
        ;" . ;"DO DBGMSG^TMGRST01("Considering to NOT save variable: "_EXCLVAR)  
        ;" . NEW VNUM SET VNUM=""
        ;" . FOR  SET VNUM=$ORDER(ZZTMGTEMPVARS("V",VNUM)) QUIT:+VNUM'>0  DO
        ;" . . NEW LINE SET LINE=$GET(ZZTMGTEMPVARS("V",VNUM)) QUIT:LINE=""
        ;" . . ;"DO DBGMSG^TMGRST01("Considering line: "_LINE)  
        ;" . . NEW VAR SET VAR=$PIECE($PIECE(LINE,"=",1),"(",1) QUIT:VAR=""
        ;" . . NEW SHOULDKILL SET SHOULDKILL=(VAR=EXCLVAR)
        ;" . . ;"DO DBGMSG^TMGRST01("Considering if "_EXCLVAR_" = "_VAR_" --> "_SHOULDKILL)  
        ;" . . IF SHOULDKILL DO
        ;" . . . KILL ZZTMGTEMPVARS("V",VNUM)
        ;" . . . ;"DO DBGMSG^TMGRST01("Killing VAR at index: "_VNUM)  
        MERGE @DESTREF=ZZTMGTEMPVARS("V")
        ;"NEW IDX SET IDX=""
        ;"FOR  SET IDX=$ORDER(ZZTMGTEMPVARS("V",IDX)) QUIT:(IDX="")  DO
        ;". MERGE @DESTREF@(IDX)=ZZTMGTEMPVARS("V",IDX)
        QUIT
        ;
REF2VARS(SOURCEREF,KILLPRIOR,KEEPLIST) ;
        ;"Purpose: Restore variables stored in REF location to system variable table.
        ;"INPUT: SOURCEREF.  PASS BY NAME.  Name of storage location of vars, from prior call to VARS2REF
        ;"        e.g. @SOURCEREF@(#)=<variable value expression>
        ;"       KILLPRIOR: if 1 then all other variables are killed from system variable table
        ;"       KEEPLIST -- Optional. A string list of variables, separated by ',' to NOT kill  
        ;"Output: variable table is altered(!)
        ;"Result: none
        IF +$GET(KILLPRIOR) DO
        . SET KEEPLIST=$GET(KEEPLIST)
        . SET KEEPLIST=KEEPLIST_",SOURCEREF"        
        . DO TEMPSAVE(KEEPLIST)  ;"Save selected variables 
        . KILL  ;"!!KILL ALL VARS ON THE SYSTEM TABLE!!
        . DO RSTRSAVE() ;
        NEW ZZTMGTEMPI SET ZZTMGTEMPI=""
        FOR  SET ZZTMGTEMPI=$ORDER(@SOURCEREF@(ZZTMGTEMPI)) QUIT:(+ZZTMGTEMPI'>0)  DO
        . NEW ZZTMGTEMPS SET ZZTMGTEMPS=$GET(@SOURCEREF@(ZZTMGTEMPI)) QUIT:ZZTMGTEMPS=""
        . SET ZZTMGTEMPS="SET "_ZZTMGTEMPS
        . XECUTE ZZTMGTEMPS
        QUIT        
        ;
RANDSTR(LEN) ;"Make a string of random characters of specified length, not including '^' etc char
        NEW RESULT SET RESULT=""
        ;"NOTE:  I could include these chars without encoding when using in URL Web query string, if wanted: "~.-_"
        NEW OKSTR SET OKSTR="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
        NEW OKLEN SET OKLEN=$LENGTH(OKSTR)
        SET LEN=+$GET(LEN)
        NEW CT FOR CT=1:1:LEN DO
        . SET RESULT=RESULT_$EXTRACT(OKSTR,$RANDOM(OKLEN)+1)
        QUIT RESULT
        ;
RND(MIN,MAX) ;"RETURN A RANDOM NUMBER IN RANGE MIN..MAX (inclusive)
        QUIT $RANDOM(MAX-MIN+1)+MIN
        ;        
TEMPSAVE(VARNAMES) ;
        ;"Purpose: save variables to ^TMP
        ;"Input: VARNAMES -- a list of variables, separated by ','
        NEW IDX
        FOR IDX=1:1:$LENGTH(VARNAMES,",") DO
        . NEW ONEVAR SET ONEVAR=$PIECE(VARNAMES,",",IDX) QUIT:ONEVAR=""
        . MERGE ^TMP($J,"TMG TMP",ONEVAR)=@ONEVAR
        QUIT
        ;
RSTRSAVE() ;
        NEW ONEVAR SET ONEVAR=""
        FOR  SET ONEVAR=$ORDER(^TMP($J,"TMG TMP",ONEVAR)) QUIT:ONEVAR=""  DO
        . MERGE @ONEVAR=^TMP($J,"TMG TMP",ONEVAR)
        KILL ^TMP($J,"TMG TMP")
        QUIT
        ;
MAKEENV() ;"Setup expected variables in enviroment.  I'm not sure if all below is required.
  ;;"DILOCKTM=3
  ;;"DISYS=19
  ;;"DTIME=30
  ;;"DUZ=0
  ;;"DUZ(0)=""
  ;;"ERR=0
  ;;"IO="/dev/null"
  ;;"IO(0)="/dev/null"
  ;;"IO(1,"/dev/null")=""
  ;;"IO("ERROR")=""
  ;;"IO("GTM-IP")="127.0.0.1"
  ;;"IO("HOME")="47^/dev/null"
  ;;"IO("IP")="127.0.0.1"
  ;;"IOBS="$C(8)"
  ;;"IOF="#"
  ;;"IOHG=""
  ;;"IOM=132
  ;;"ION="NULL"
  ;;"IOPAR=""
  ;;"IOS=47
  ;;"IOSL=64
  ;;"IOST="P-OTHER"
  ;;"IOST(0)=16
  ;;"IOT="TRM"
  ;;"IOUPAR=""
  ;;"IOXY=""
  ;;"POP=0
  ;;"TYPE=1
  ;;"U="^"
  ;;"####
  DO DT^DICRW  ;"set up FM required vars  
  NEW DONE SET DONE=0
  NEW IDX FOR IDX=1:1 DO  QUIT:DONE
  . NEW STR SET STR=$PIECE($TEXT(MAKEENV+IDX^TMGRSTU2),";;""",2)
  . IF (STR="####")!(STR="") SET DONE=1 QUIT
  . NEW VAR SET VAR=$PIECE(STR,"=",1)
  . IF "1,11,"[$DATA(@VAR)_"," QUIT  ;"skip var if already on variable table.
  . NEW CODE SET CODE="SET "_STR 
  . XECUTE CODE
  SET DT=$$NOW^XLFDT\1
  QUIT
  ;
