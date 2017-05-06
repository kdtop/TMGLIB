TMGCOD03 ;TMG/kst-Code analysis tools  ;3/2/15
         ;;1.0;TMG-LIB;**1**;2/15/15
 ;
 ;"TMG CODE ANALYSIS OF PARSED CODE
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"COMPALL  --COMPARE EACH ENTRY IN ROUTINE FILE
 ;"STOREALL --Process through all ROUTINES and store information into file 22726
 ;"FOREACH(ACTIONFN,ERRPROC,START,END,OPTION) --PERFORM ACTION FOR EACH ENTRY IN ROUTINE FILE
 ;"BROWSE --BROWSE stored info
 ;"CHKDPND(REFOUT,ROUTINE,TAG,REFINFO,REFPRIOR,MAP)  -- Check routine dependancies
 ;
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;"COMPRTN(ROUTINE,ARR)  --CODE PARSE -> ARRAY --> REASSEMBLE --> COMPARE
 ;"COMPBLKCD(ASMBLCOD,ROUTINE,ARR)  -COMPARE ARRAY OF CODE TO ROUTINE ON DISK.
 ;"HDLCMPER(ERRSTR,ARR)  --HANDLE COMPARISON ERROR
 ;"HDLERR(ERRSTR,ARR) 
 ;"RTNCHNGD(ROUTINE,MD5SUM)  --HAS ROUTINE CHANGED?
 ;"STORERTN(ROUTINE,ARR)  --STORE INFORMATION FOR 1 ROUTINE IN FILE 22726
 ;"ADDSUBRC(FILE,IENS,FLDA,FLDB) ;"
 ;"BRWSIDX(IDX,HDR,PREFIX)  --BROWSE BY INDEX
 ;"BRWSFNP  
 ;"BRWSGOTO  
 ;"BRWSGLBL  
 ;"BRWSTXT
 ;"SENDALERT(RECIPIENT,HEADING,MSGARR,HANDLER) -- SEND ALERT
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;" TMGSTUT3, TMGCOD01, XLFSTR, TMGUSRI2, XLFDT
 ;"=======================================================================
 ;
 ;"NOTE: The original version of this file was lost.  Below is reconstruction
 ;
FOREACH(ACTIONFN,ERRPROC,START,END,OPTION)  ;"PERFORM ACTION FOR EACH ENTRY IN ROUTINE FILE
  ;"Purpose: Loop through ROUTINEs and perform ACTIONFN for each entry
  ;"Input: ACTIONFN -- NAME of function to call.  E.g. "STORERTN", without '$$'
  ;"                   Function should accept 3 parameters (ROUTINE,ARR,OPTION), and  
  ;"                   return 1 if OK, or -1^Message if problem.
  ;"                   ARR is a OUT parameter that ACTIONFN can use. 
  ;"       ERRPROC -- OPTIONAL.  NAME of procedure to call when an error is 
  ;"                   encountered.  E.g. "HNDLERR".  This should accept 3
  ;"                   parameters: (ERRSTR,ARR,OPTION).  ERRStr has error, 
  ;"                   format:  -1^ErrorMessage
  ;"       START -- OPTIONAL.  Name of routine to start with. If not provided, starts at beginning of list
  ;"       END -- OPTIONAL.  Name of routine to stop AFTER. If not provided, then goes to end of list
  ;"       OPTION --  OPTIONAL.  This is passed into ACTIONFN as an IN parameter
  ;"               OPTION("FOREACH")="Q"  If "Q" then progress bar is not shown.
  ;"Result: none
  SET ACTIONFN=$GET(ACTIONFN),ERRPROC=$GET(ERRPROC)
  SET START=$GET(START),END=$GET(END)
  IF ACTIONFN="" GOTO FEDN
  NEW MAXRTNCT SET MAXRTNCT=0
  NEW SHOWPROG SET SHOWPROG=($GET(OPTION("FOREACH"))'="Q")
  NEW ROUTINE SET ROUTINE=START
  FOR  SET ROUTINE=$ORDER(^DIC(9.8,"B",ROUTINE)) QUIT:(ROUTINE="")!((END'="")&(ROUTINE]END))  SET MAXRTNCT=MAXRTNCT+1
  NEW STARTH SET STARTH=$H
  NEW RTNCT SET RTNCT=0
  SET ROUTINE=START
  FOR  SET ROUTINE=$ORDER(^DIC(9.8,"B",ROUTINE)) QUIT:(ROUTINE="")!((END'="")&(ROUTINE]END))  DO
  . SET RTNCT=RTNCT+1
  . IF SHOWPROG,RTNCT#10=0 DO PROGBAR^TMGUSRI2(RTNCT,$$LJ^XLFSTR(ROUTINE,12),0,MAXRTNCT,70,STARTH)
  . NEW CODE,RESULT,OUT
  . SET CODE="SET RESULT=$$"_ACTIONFN_"(ROUTINE,.OUT,.OPTION)" 
  . XECUTE CODE
  . IF (+RESULT=1)!(ERRPROC="") QUIT  
  . SET CODE="DO "_ERRPROC_"(RESULT,.OUT)"
  . XECUTE CODE
FEDN  ;  
  IF SHOWPROG DO PROGBAR^TMGUSRI2(100,$$LJ^XLFSTR("(DONE)",12),0,100,70,STARTH) WRITE !
  QUIT
  ;
  ;"================================================================
  ;"================================================================
  ; 
COMPALL  ;"COMPARE EACH ENTRY IN ROUTINE FILE
  DO FOREACH("COMPRTN","HDLCMPER")
  QUIT 
  ;
COMPRTN(ROUTINE,ARR,OPTION)  ;"CODE PARSE -> ARRAY --> REASSEMBLE --> COMPARE
  ;"Input: ROUTINE -- NAME of routine to check
  ;"       ARR -- PASS BY REFERENCE.  An OUT parameter.  Filled with 
  ;"              differences during round trip check.
  ;"       OPTION -- Not used here
  ;"Result: 1 if OK, or -1^message
  NEW TESTBLK,OUT
  DO PARSEPOS^TMGCOD01(,,ROUTINE,"TESTBLK",3)
  DO ASSEMBLE^TMGCOD02("TESTBLK",,"OUT")
  DO COMPBLKCD(.OUT,ROUTINE,.ARR)
  NEW RESULT SET RESULT=1
  IF $DATA(ARR) SET RESULT="-1^Differences found"
  QUIT RESULT
  ;
COMPBLKCD(ASMBLCOD,ROUTINE,ARR)  ;"COMPARE ARRAY OF CODE TO ROUTINE ON DISK.
  NEW OFFSET SET OFFSET=""
  FOR  SET OFFSET=$ORDER(ASMBLCOD(OFFSET)) QUIT:OFFSET=""  DO
  . NEW ASMBLINE SET ASMBLINE=$GET(ASMBLCOD(OFFSET))
  . NEW REF SET REF="+"_OFFSET_"^"_ROUTINE
  . NEW CODELINE SET CODELINE=$TEXT(@REF)
  . IF $$TRIM^XLFSTR(ASMBLINE)=$$TRIM^XLFSTR(CODELINE) QUIT
  . SET ARR(OFFSET,"O")=CODELINE
  . SET ARR(OFFSET,"R")=ASMBLINE  
  QUIT
  ;
HDLCMPER(ERRSTR,ARR,OPTION)  ;"HANDLE COMPARISON ERROR
  ;"Input:  ERRSTR -- -1^Message
  ;"       ARR -- PASS BY REFERENCE.  An OUT parameter.  Filled with 
  ;"              differences during round trip check.
  ;"       OPTION -- Not used here
  IF $DATA(ARR) DO ZWRITE^TMGZWR("ARR")
  QUIT
  ;
  ;"================================================================
  ;"================================================================
  ;
STOREALL(MODE,OPTION) ;"Process through all ROUTINES and store information into file 22726
  ;"Input: MODE: OPTIONAL.  If 0 OR "" then ALL routines processed
  ;"                        If 1, then ""-"J" (non-inclusive) done
  ;"                        If 2, then "J"-"R" (non-inclusive) done
  ;"                        If 3, then "R"-""  done
  ;"       OPTION.  OPTIONAL.
  ;"               OPTION("FOREACH")="Q"  If "Q" then progress bar is not shown.
  ;"NOTE: For some reason processing ALL 90000 routines in one process can cause 
  ;"  GT.M to run out of memory.  So MODE will allow for just part of them to be done.
  ;"  CORRECTION: Now that most are already in, each run is now processing < 1000
  ;"      routines, so it seems to be OK to check ALL with each run.
  ;"Result: None.
  SET MODE=+$GET(MODE)
  NEW OPTION,START,END
  SET START(0)="" SET START(1)="" SET START(2)="J" SET START(3)="R"
  SET END(0)=""   SET END(1)="J"  SET END(2)="R"   SET END(3)=""
  DO FOREACH("STORERTN","HDLERR",START(MODE),END(MODE),.OPTION)
  NEW STORED MERGE STORED=OPTION("OUT")
  IF $DATA(STORED)=0 GOTO STALDN
  NEW USRIEN SET USRIEN=168  ;"Kevin Toppenberg
  NEW CT SET CT=0
  NEW RTN SET RTN="" FOR  SET RTN=$ORDER(STORED(RTN)) QUIT:RTN=""  SET CT=CT+1
  IF CT'>0 GOTO STALDN
  NEW HEADING SET HEADING="FYI, Updated info in file #22726 for "_CT_" routine(s) on "_$$FMTE^XLFDT($$NOW^XLFDT)
  ;"TEST
  ;" Old way, below sends as informational DO SENDALERT(USRIEN,HEADING,.STORED,"HNDLMSG^TMGCOD03")   
  NEW ROUTINE,RESULT SET ROUTINE=""
  FOR  SET ROUTINE=$ORDER(STORED(ROUTINE)) QUIT:ROUTINE=""  DO
  . SET HEADING="FYI, routine "_ROUTINE_" has been "_$GET(STORED(ROUTINE))_" on "_$$FMTE^XLFDT($$NOW^XLFDT)
  . DO INFRMALT^TMGXQAL(.RESULT,USRIEN,HEADING)
STALDN ;  
  QUIT
  ;
HDLERR(ERRSTR,ARR,OPTION) ;
  WRITE !,$PIECE(TEMP,"^",2,999),!
  QUIT
  ;
RTNCHNGD(ROUTINE,MD5SUM)  ;"HAS ROUTINE CHANGED?
  ;"Input: ROUTINE
  ;"       MD5SUM -- OPTIONAL.  PASS by reference to get back newly calculated MD5SUM for ROUTINE
  ;"Result: 1 if routine has changed compared to entry in 22726, or if no prior entry found 
  ;"        0 if no change
  NEW RESULT SET RESULT=1
  SET MD5SUM=""
  SET ROUTINE=$GET(ROUTINE) IF ROUTINE="" GOTO RTNCH
  NEW IEN SET IEN=+$ORDER(^TMG(22726,"B",ROUTINE,""))
  IF IEN'>0 GOTO RTNCH
  NEW PRIORMD5SUM SET PRIORMD5SUM=$PIECE($GET(^TMG(22726,IEN,0)),"^",3)
  SET MD5SUM=$$MD5SUM^TMGKERN1(ROUTINE)
  SET RESULT=(PRIORMD5SUM'=MD5SUM)
RTNCH ;
  QUIT RESULT
  ;
STORERTN(ROUTINE,ARR,OPTION)  ;"STORE INFORMATION FOR 1 ROUTINE IN FILE 22726
  ;"Input: ROUTINE -- name of routin
  ;"       ARR -- not used here
  ;"       OPTION -- OPTION("FORCE STORE")=1 <-- If found then, storage of info
  ;"                      if forced, even if prior record is found, and source
  ;"                      ROUTINE has not changed.  
  ;"            OPTION("OUT",<ROUTINE>)=message <-- Only if record modified. 
  ;"Result: 1 if OK, or -1^Message if problem. 
  NEW RESULT SET RESULT=1 ;"default is OK
  SET ROUTINE=$GET(ROUTINE) IF ROUTINE="" DO  GOTO SRTNDN
  . SET RESULT="-1^No routine name provided"
  NEW MD5SUM,CHANGED SET CHANGED=$$RTNCHNGD(ROUTINE,.MD5SUM)  ;"HAS ROUTINE CHANGED?
  NEW FORCE SET FORCE=+$GET(OPTION("FORCE STORE"))
  IF (CHANGED=0)&(FORCE=0) GOTO SRTNDN 
  NEW IEN SET IEN=+$ORDER(^TMG(22726,"B",ROUTINE,""))
  NEW MESSAGE SET MESSAGE="ADDED"
  IF IEN>0 DO
  . NEW DA,DIK SET DA=IEN,DIK="^TMG(22726," DO ^DIK   ;"KILL PRIOR RECORD
  . SET MESSAGE="CHANGED"
  SET OPTION("OUT",ROUTINE)=MESSAGE
  NEW OUT
  DO PARSEPOS^TMGCOD01(,,ROUTINE,"OUT",3)
  NEW TMGFDA,TMGIEN,TMGMSG
  SET TMGFDA(22726,"+1,",.01)=ROUTINE
  SET TMGFDA(22726,"+1,",.02)=$$NOW^XLFDT
  SET TMGFDA(22726,"+1,",.03)=MD5SUM
  IF $DATA(OUT("IX","@")) SET TMGFDA(22726,"+1,",.05)="Y"
  DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO SRTNDN
  . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  SET IEN=$GET(TMGIEN(1))
  IF IEN'>0 DO  GOTO SRTNDN
  . SET RESULT="-1^Unable to find IEN of added record"
x  ;  
  NEW IENS SET IENS="+1,"_IEN_","
  NEW OUTRTN SET OUTRTN=""
  FOR  SET OUTRTN=$ORDER(OUT("IX","$$FN",OUTRTN)) QUIT:(OUTRTN="")!(RESULT'=1)  DO
  . NEW FNNAME SET FNNAME=""
  . FOR  SET FNNAME=$ORDER(OUT("IX","$$FN",OUTRTN,FNNAME)) QUIT:(FNNAME="")!(RESULT'=1)  DO
  . . SET RESULT=$$ADDSUBRC(22726.01,IENS,OUTRTN,FNNAME)
  SET OUTRTN=""
  FOR  SET OUTRTN=$ORDER(OUT("IX","DO",OUTRTN)) QUIT:(OUTRTN="")!(RESULT'=1)  DO
  . NEW TAG SET TAG=""
  . FOR  SET TAG=$ORDER(OUT("IX","DO",OUTRTN,TAG)) QUIT:(TAG="")!(RESULT'=1)  DO
  . . SET RESULT=$$ADDSUBRC(22726.01,IENS,OUTRTN,TAG) 
  SET OUTRTN=""
  FOR  SET OUTRTN=$ORDER(OUT("IX","GOTO",OUTRTN)) QUIT:(OUTRTN="")!(RESULT'=1)  DO
  . NEW TAG SET TAG=""
  . FOR  SET TAG=$ORDER(OUT("IX","GOTO",OUTRTN,TAG)) QUIT:(TAG="")!(RESULT'=1)  DO
  . . SET RESULT=$$ADDSUBRC(22726.02,IENS,OUTRTN,TAG)
  NEW GLOBAL SET GLOBAL=""
  FOR  SET GLOBAL=$ORDER(OUT("IX","GLOBAL",GLOBAL)) QUIT:(GLOBAL="")!(RESULT'=1)  DO
  . SET RESULT=$$ADDSUBRC(22726.03,IENS,GLOBAL) 
  SET OUTRTN=""
  FOR  SET OUTRTN=$ORDER(OUT("IX","$TEXT",OUTRTN)) QUIT:(OUTRTN="")!(RESULT'=1)  DO
  . NEW TAG SET TAG=""
  . FOR  SET TAG=$ORDER(OUT("IX","$TEXT",OUTRTN,TAG)) QUIT:(TAG="")!(RESULT'=1)  DO
  . . SET RESULT=$$ADDSUBRC(22726.04,IENS,OUTRTN,TAG) 
SRTNDN ;    
  QUIT RESULT
  ;
ADDSUBRC(FILE,IENS,FLDA,FLDB) ;"
  ;"Result: 1 if OK, or -1^Message if problem. 
  NEW RESULT SET RESULT=1
  NEW TMGFDA,TMGIEN,TMGMSG
  SET TMGFDA(FILE,IENS,.01)=FLDA
  IF $DATA(FLDB) SET TMGFDA(FILE,IENS,.02)=FLDB
  DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  QUIT RESULT
  ;
  ;"======================================================================
  ;"======================================================================
BROWSE  ;"BROWSE stored info
  NEW MENU,USRSLCT
  SET MENU(0)="Select option to explore routine interdependancies."
  SET MENU(1)="Select & View Fn / Proc references INTO ROUTINE(s)"_$CHAR(9)_"ROUTINE"
  SET MENU(2)="Select & view GOTO references to INTO ROUTINE(s)"_$CHAR(9)_"GOTO"
  SET MENU(3)="View ^Globals referenced"_$CHAR(9)_"GLOBAL"
  SET MENU(4)="View $TEXT() references INTO ROUTINE(s)"_$CHAR(9)_"TEXT"
  SET MENU(5)="Dump record of OUTWARD references for one ROUTINE"_$CHAR(9)_"DUMP1OUT"
  SET MENU(6)="Show INWARD references for one ROUTINE"_$CHAR(9)_"DUMP1IN"
  SET MENU(7)="Show REMOTE PROCEDURE references INTO one ROUTINE"_$CHAR(9)_"RPC"
BRL1 ; 
  WRITE !
  SET USRSLCT=$$MENU^TMGUSRI2(.MENU)
  IF USRSLCT="ROUTINE" DO BRWSFNP GOTO BRL1
  IF USRSLCT="GOTO" DO BRWSGOTO GOTO BRL1
  IF USRSLCT="GLOBAL" DO BRWSGLBL GOTO BRL1
  IF USRSLCT="TEXT" DO BRWSTXT GOTO BRL1
  IF USRSLCT="DUMP1OUT" DO DUMP1OUT GOTO BRL1
  IF USRSLCT="DUMP1IN" DO DUMP1IN GOTO BRL1
  IF USRSLCT="RPC" DO RPCIN GOTO BRL1
  WRITE !,"Goodbye.",!  
  QUIT
  ;
BRWSIDX(IDX,HDR,PREFIX,DESCR)  ;"BROWSE BY INDEX
  WRITE !,"==== Browse references IN to "
  IF IDX'="AGBL" WRITE "ROUTINE"
  ELSE  WRITE "GLOBAL"
  WRITE " via index: ",IDX," ====",!
  WRITE "NOTE: if a given item is not listed then it is not",!
  WRITE "      referenced by other routines.",!
  DO PRESS2GO^TMGUSRI2
  NEW ARR,OUT
  SET PREFIX=$GET(PREFIX)
  NEW IDXVALUE SET IDXVALUE=""
  FOR  SET IDXVALUE=$ORDER(^TMG(22726,IDX,IDXVALUE)) QUIT:IDXVALUE=""  DO
  . SET ARR(PREFIX_IDXVALUE,IDXVALUE)=""
  DO SELECTR2^TMGUSRI3("ARR","OUT",HDR)
  NEW IDXARR SET IDXARR(IDX)=$GET(DESCR)
  DO BRWSARR(.OUT,.IDXARR,0)  ;"Browse items in SEL array
  QUIT
  ;
BRWSARR(SEL,IDXARR,MODE)  ;"Browse items in SEL array
  ;"Input: SEL -- PASS BY REFERENCE.  Format:
  ;"               SEL(<INDEX VALUE>)="" or SEL(<Descr>,<INDEX VALUE>)=""
  ;"       IDXARR -- PASS BY REFERENCE.  Format:
  ;"               IDXARR(<index name>)=<description>
  ;"       MODE -- 0: browse in file 22726 (TMG ROUTINE)
  ;"               1: brows in REMOTE PROCEDURE file
  NEW ARR
  SET MODE=+$GET(MODE)
  NEW GREF,TYPE,FILE
  IF MODE=1 DO
  . SET GREF=$NAME(^XWB(8994))
  . SET TYPE="REMOTE PROCEDURE"
  . SET FILE=8994
  ELSE  DO
  . SET GREF=$NAME(^TMG(22726))
  . SET TYPE="source ROUTINE"
  . SET FILE=22726
  NEW IDX SET IDX=""
  FOR  SET IDX=$ORDER(IDXARR(IDX)) QUIT:IDX=""  DO
  . NEW IDXDESCR SET IDXDESCR=$GET(IDXARR(IDX))
  . NEW IDXVALUE SET IDXVALUE=""
  . FOR  SET IDXVALUE=$ORDER(SEL(IDXVALUE)) QUIT:IDXVALUE=""  DO
  . . NEW PICK SET PICK=$ORDER(SEL(IDXVALUE,"")) IF PICK="" SET PICK=IDXVALUE
  . . NEW IEN SET IEN=0
  . . FOR  SET IEN=$ORDER(@GREF@(IDX,PICK,IEN)) QUIT:IEN=""  DO
  . . . NEW STR,NAME SET NAME=$PIECE($GET(@GREF@(IEN,0)),"^",1) QUIT:NAME=""
  . . . IF MODE=1 DO
  . . . . NEW TEMP SET TEMP=$PIECE(IDXVALUE,":",2)_"^"_$PIECE(IDXVALUE,":",1)
  . . . . SET STR=$$LJ^XLFSTR(NAME,32)_" --> "_TEMP
  . . . ELSE  DO
  . . . . SET STR=$$LJ^XLFSTR(NAME,12)_" --> "_IDXVALUE
  . . . IF IDXDESCR'="" SET STR=STR_" ("_IDXDESCR_")"
  . . . SET ARR(STR,IEN_"^"_NAME)=""
  NEW OUT
  IF $DATA(ARR) DO SELECTR2^TMGUSRI3("ARR","OUT","Select "_TYPE_" to view")
  KILL ARR
  NEW PICK SET PICK=""
  FOR  SET PICK=$ORDER(OUT(PICK)) QUIT:PICK=""  DO
  . NEW RESULT SET RESULT=$ORDER(OUT(PICK,""))
  . SET ARR(RESULT)=""
  NEW ABORT SET ABORT=0
  NEW FOUND SET FOUND=0
  NEW ITEM SET ITEM=""
  FOR  SET ITEM=$ORDER(ARR(ITEM)) QUIT:(ITEM="")!ABORT  DO
  . SET FOUND=1
  . WRITE !,"Show information about ",TYPE," '",$PIECE(ITEM,"^",2),"'"
  . NEW % SET %=1 DO YN^DICN WRITE !
  . IF %=-1 SET ABORT=1 QUIT
  . IF %=2 QUIT
  . IF MODE=1 DO
  . . DO BROWSRPC^TMGRPCUT(+ITEM)
  . ELSE  DO
  . . NEW OPTION SET OPTION("NO LOOP")=1
  . . DO ASKDUMP^TMGDEBU3(FILE,+ITEM,.OPTION)
  . QUIT  ;"----------------
  IF FOUND=0 DO
  . WRITE !,"Nothing to display...",!
  . DO PRESS2GO^TMGUSRI2
  QUIT
  ;
BRWSFNP  ;
  NEW HDR SET HDR="Pick ROUTINE to see what references it. <ESC><ESC> when done."
  DO BRWSIDX("AFNP",HDR,,"by function or procedure call")
  QUIT
  ;
BRWSGOTO  ;
  NEW HDR SET HDR="Pick ROUTINE to see what references it. <ESC><ESC> when done."
  DO BRWSIDX("AGT",HDR,,"by GOTO")
  QUIT
  ;
BRWSGLBL  ;
  NEW HDR SET HDR="Pick ^GLOBAL to see what references it. <ESC><ESC> when done."
  DO BRWSIDX("AGBL",HDR,"^","global reference")
  QUIT
  ;
BRWSTXT  ;
  NEW HDR SET HDR="Pick ROUTINE to see what references it. <ESC><ESC> when done."
  DO BRWSIDX("ATXT",HDR,,"$TEXT() reference")
  QUIT
  ;
DUMP1OUT  ;
  DO ASKDUMP^TMGDEBU3(22726)
  QUIT
  ;
DUMP1IN   ;
  NEW X,Y,DIC SET DIC=22726,DIC(0)="MAEQ"
  DO ^DIC WRITE !
  IF +Y'>0 GOTO D1IDN
  NEW SEL SET SEL($PIECE(Y,"^",2))=""
  NEW IDXARR
  SET IDXARR("AFNP")="function or procedure call"
  SET IDXARR("AGT")="GOTO"
  ;"SET IDXARR("AGBL")="^Global reference"
  SET IDXARR("ATXT")="$TEXT() reference"
  DO BRWSARR(.SEL,.IDXARR,"SELECT")  ;"Browse items in SEL array
D1IDN ;
  QUIT
  ;
RPCIN  ;  
  NEW X,Y,DIC SET DIC=22726,DIC(0)="MAEQ"
  DO ^DIC WRITE !
  IF +Y'>0 GOTO RPIDN
  NEW RTN SET RTN=$PIECE(Y,"^",2)
  NEW SEL
  NEW ITEM SET ITEM=RTN
  FOR  SET ITEM=$ORDER(^XWB(8994,"ATMGRTN",ITEM)) QUIT:(ITEM="")!(ITEM'[RTN)  DO
  . SET SEL(ITEM)=""
  NEW IDXARR SET IDXARR("ATMGRTN")="From REMOTE PROCEDURE"
  DO BRWSARR(.SEL,.IDXARR,1)
RPIDN ;
  QUIT

  ;"======================================================================
  ;"======================================================================
  ;
CHKDPND(REFOUT,ROUTINE,TAG,REFINFO,REFPRIOR,MAP)  ;"Check routine dependancies
  ;"Input: REFOUT -- PASS BY REFERANCE.  AN OUT PARAMETER.  Format:
  ;"         @REFOUT@(ROUTINE,<Dependance ROUTINE><TAG in dependancy routine)=<type>
  ;"             <type>=  $PIECE("$$FN^DO^$TEXT^GOTO","^",#)
  ;"       ROUTINE -- name of ROUTINE to check
  ;"       TAG -- OPTIONAL.  The TAG inside to ROUTINE to check.  If provided,
  ;"            then INFO will be ignored, as that presumably will contain
  ;"            parsed information for the entire file, and we are concerned
  ;"            about only a single function or procedure.
  ;"       REFINFO -- OPTIONAL.  PASS BY NAME.  Array containing parsed information
  ;"                  If INFO does not contain info, then parse will be done 
  ;"                  and put into INFO. 
  ;"       REFPRIOR -- PASS BY NAME.  AN IN AND OUT PARAMETER.  This can store
  ;"             determinations already done, to prevent having to calculate
  ;"             over and over for comman entry points.
  ;"       MAP -- PASS BY REFERENCE.  A tree structure of dependancies. Format:
  ;"           MAP(<'TAG^ROUTINE'>,<'Needed TAG^ROUTINE'>,<'Needed TAG^ROUTINE'>....)=""
  ;"NOTE: When parsing *parts* of a routine, it is not always clear when the
  ;"     part ends.  A clean function, for example, might have an entry point and
  ;"    end when the next tag is found.  But if a tag is used inside a function
  ;"   then this parser will think that code ends sooner than it actually does.
  ;"   This will miss potential dependancies.
  ;"Results: None
  SET ROUTINE=$GET(ROUTINE),TAG=$GET(TAG),REFINFO=$GET(REFINFO)
  NEW INFOBKUP
  NEW LOCALINFO IF REFINFO="" SET REFINFO="LOCALINFO"
  IF (TAG'="") DO
  . IF $DATA(@REFINFO) MERGE INFOBKKUP=@REFINFO KILL @REFINFO
  . DO PARSEPOS^TMGCOD01(TAG,,ROUTINE,REFINFO,0) ;"0=parse to next tag (NOTE: MIGHT NOT BE FAR ENOUGH)
  IF $DATA(@REFINFO)=0 DO
  . DO PARSEPOS^TMGCOD01(,,ROUTINE,REFINFO,3) ;"3=parse to end of file
  SET @REFOUT=""
  NEW IDX,TYPE,ATYPE SET TYPE="$$FN^DO^$TEXT^GOTO" 
  FOR IDX=1:1:4 SET ATYPE=$PIECE(TYPE,"^",IDX) DO
  . NEW ARTN SET ARTN=""
  . FOR  SET ARTN=$ORDER(@REFINFO@("IX",ATYPE,ARTN)) QUIT:ARTN=""  DO
  . . NEW ATAG SET ATAG=""
  . . FOR  SET ATAG=$ORDER(@REFINFO@("IX",ATYPE,ARTN,ATAG)) QUIT:ATAG=""  DO
  . . . IF ATYPE="$TEXT" QUIT  ;"Don't chain dependancies for $TEXT() reference
  . . . NEW ARTN2 SET ARTN2=ARTN IF ARTN2="[LOCAL]" SET ARTN2=ROUTINE
  . . . IF $DATA(@REFPRIOR@(ROUTINE,ARTN2,ATAG))>0 QUIT
  . . . SET @REFPRIOR@(ROUTINE,ARTN2,ATAG)=IDX  ;"stores dependancy
  . . . SET @REFOUT@(ROUTINE,ARTN2,ATAG)=IDX  ;"stores dependancy
  . . . NEW TEMPOUT,TEMPMAP
  . . . DO CHKDPND("TEMPOUT",ARTN2,ATAG,"",REFPRIOR,.TEMPMAP)
  . . . SET MAP(TAG_"^"_ROUTINE)=""
  . . . MERGE MAP(TAG_"^"_ROUTINE)=TEMPMAP
CKDPDN  ;
  IF $DATA(INFOBKUP) KILL @REFINFO MERGE @REFINFO=INFOBKUP
  QUIT
  ;"======================================================================
  ;"======================================================================
  ;
SENDALERT(RECIPIENT,HEADING,MSGARR,HANDLER) ;"SEND ALERT
  ;"Input -- RECIPIENT -- IEN in NEW PERSON file
  ;"         HEADING -- The header message (show in CPRS)
  ;"         MSGARR -- The text to send
  ;"         HANDLER -- OPTIONAL.  Default is "HNDLMSG^TMGCOD03"
  ;"Result: None
  NEW NOW SET NOW=+$TRANSLATE($H,",",".")
  FOR  QUIT:($DATA(^TMG("TMP","TMGCOD03",$J,NOW))=0)  SET NOW=NOW+0.00000001
  SET HANDLER=$GET(HANDLER,"HNDLMSG^TMGCOD03")
  IF $DATA(MSGARR)=0 GOTO SADN
  SET RECIPIENT=+$GET(RECIPIENT) IF (RECIPIENT'>0) GOTO SADN
  SET IENS=$GET(IENS)
  NEW XQA,XQAARCH,XQADATA,XQAFLG,XQAGUID,XQAID,XQAOPT,XQAROU,XQASUPV,XQASURO,XQATEXT
  SET XQA(RECIPIENT)=""   
  SET XQADATA=$J_"^"_NOW
  SET XQAID="TMG"
  SET XQAROU=HANDLER
  SET XQAMSG=HEADING
  MERGE ^TMG("TMP","TMGCOD03",$J,NOW,"ARR")=MSGARR
  NEW TEMP SET TEMP=$$SETUP1^XQALERT  
SADN ;  
  QUIT  
  ;  
HNDLMSG ;"Entry point for Alert handler.
  ;
  NEW DATA SET DATA=$GET(XQADATA)
  NEW JOBNUM SET JOBNUM=$PIECE(DATA,"^",1)
  NEW NOW SET NOW=$PIECE(DATA,"^",2)
  NEW ARR MERGE ARR=^TMG("TMP","TMGCOD03",JOBNUM,NOW,"ARR")
  IF $DATA(ARR) WRITE !,! DO ZWRITE^TMGZWR("ARR")
  DO PRESS2GO^TMGUSRI2 WRITE !
  NEW % SET %=1
  WRITE "Delete alert info" DO YN^DICN WRITE !
  IF %=1 KILL ^TMG("TMP","TMGCOD03",JOBNUM,NOW,"MSG") 
  QUIT
  ;
