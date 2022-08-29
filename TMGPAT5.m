TMGPAT5  ;TMG/kst/Patching tools ;10/19/08, 8/10/22
         ;;1.0;TMG-LIB;**1**;10/19/08
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"SHOWTG(NOPAUSE) -- SHOW ENTRIES LOADED IN TRANSPORT GLOBAL
 ;"LSTTG(OUT) -- LIST TRANSPORT GLOBALS LOADED
 ;"FIXPATCH(TRIALRUN) -- Purpose: This fixes situation when a patch with wrong $ZRO 
 ;"GBLLSTTG()  -- SHOW ENTRIES LOADED IN TRANSPORT GLOBAL
 ;"EXAMKIDS(INFO) -- Examine KIDS file, esp routines.  
 ;"KIDS2ARR(ARRAY,LOCALFILE) -- Load KIDS file into a multi-dimensional array  
 ;"PICKEXAMINE() -- Ask user for entry in file 9.7, then SHOW ENTRIES LOADED IN TRANSPORT GLOBAL
 ;"XTRACTRTN(IEN9D7,OUT) -- Scan ^XTMP("XPDI",IEN9D7,*) and extract routines into array
 ;"GETLOCALRTN(RTN,OUT) -- Extract a local routine module into OUT
 ;
 ;"=======================================================================
 ;"Private Functions
 ;"=======================================================================
 ;"EXAMRTNS(IEN9D7)  -- Examine routines from INSTALL record
 ;"DOEXAM(KIDSARR,LOCALARR,OPTION) -- Prompt user via menu to pick a result from array to examine, and then view etc.    
 ;"SHOW1RTN(ARR,NAME,NOPAUSE)  -- Show 1 routine
 ;"COMP2RTNS(KIDSARR,LOCALARR,NAME,NOPAUSE)  --COMPARE 2 ROUTINES
 ; 
 ;"=======================================================================
 ;
SHOWTG(NOPAUSE) ;"SHOW ENTRIES LOADED IN TRANSPORT GLOBAL
  NEW OUT
  WRITE !,!,"Distributions (Installs) currently loaded into Transport Global",!
  WRITE "----------------------------------------------------------------",!
  IF $$LSTTG(.OUT)>0 DO
  . NEW NAME SET NAME=""
  . FOR  SET NAME=$ORDER(OUT(NAME)) QUIT:NAME=""  DO
  . . NEW IEN SET IEN=0
  . . FOR  SET IEN=$ORDER(OUT(NAME,IEN)) QUIT:+IEN'>0  DO
  . . . WRITE "#",IEN," ",NAME,!
  ELSE  DO
  . WRITE "(NONE)",!
  WRITE !
  IF $GET(NOPAUSE)'=1 DO PRESS2GO^TMGUSRI2
  QUIT
  ;
LSTTG(OUT) ;"LIST TRANSPORT GLOBALS LOADED
  ;"Input: -- OUT -- PASS BY REFERENCE.  AN OUT PARAMETER.  Format:
  ;
  ;"Result: Count of number found
  NEW CT SET CT=0
  NEW IEN9D7 SET IEN9D7=0
  FOR  SET IEN9D7=$ORDER(^XTMP("XPDI",IEN9D7)) QUIT:+IEN9D7'>0  DO
  . NEW NAME SET NAME=$PIECE($GET(^XPD(9.7,IEN9D7,0)),"^",1)
  . SET OUT(NAME,IEN9D7)="",CT=CT+1
  QUIT CT
 ;
GBLLSTTG()  ;
  DO SHOWTG(1) ;"SHOW ENTRIES LOADED IN TRANSPORT GLOBAL
  NEW DIC,X,Y SET DIC=9.7,DIC(0)="MAEQ",DIC("S")="I '$P(^(0),U,9),$D(^XTMP(""XPDI"",Y))"
  DO ^DIC WRITE ! IF Y'>0 QUIT
  NEW REF SET REF=$$OREF^DILF($NAME(^XTMP("XPDI",+Y)))
  NEW FLAGPRM SET FLAGPRM=1  ;"used by %ZVE code 
  NEW %1 SET %1=REF      ;"used by %ZVE code
  DO EN^%ZVEMG
  QUIT
  ;
FIXPATCH(TRIALRUN) ;
  ;"Purpose: This fixes situation when a patch is applied without first
  ;"  changing the $ZRO
  ;"  This will changed the routines into the /r folder (backing up prior
  ;"    files if they exist)
  NEW PDIR SET PDIR="/opt/worldvista/EHR/p/"
  NEW RDIR SET RDIR="/opt/worldvista/EHR/r/"
  NEW SOME SET SOME=0
  SET TRIALRUN=$GET(TRIALRUN)
  IF TRIALRUN DO
  . WRITE !,"This will show which changes can be made, but will NOT actually make any changes.",!
  . DO PRESS2GO^TMGUSRI2
  NEW INIT
  FOR I=1:1:26 DO 
  . NEW INIT SET INIT=$CHAR(64+I)
  . NEW TMGFILTER SET TMGFILTER(INIT_"*")=""
  . NEW PLIST,RLIST
  . IF $$LIST^%ZISH(PDIR,"TMGFILTER","PLIST")
  . ;"IF $$LIST^%ZISH(RDIR,"TMGFILTER","RLIST")
  . NEW AFILE SET AFILE=""
  . FOR  SET AFILE=$ORDER(PLIST(AFILE)) QUIT:AFILE=""  DO
  . . NEW TYPE SET TYPE=$PIECE(AFILE,".",2)
  . . IF TYPE'="m" QUIT
  . . NEW NAMESPACE SET NAMESPACE=$EXTRACT(AFILE,1,3)
  . . IF NAMESPACE="TMG" QUIT
  . . IF $EXTRACT(AFILE,1,2)="ZZ" QUIT
  . . SET SOME=1
  . . WRITE AFILE
  . . NEW PFILE SET PFILE=PDIR_AFILE
  . . NEW RFILE SET RFILE=RDIR_AFILE
  . . IF $$ISFILE^TMGKERNL(RFILE) DO
  . . . WRITE " <-- overlap",!
  . . . NEW BACKFNAME SET BACKFNAME=""
  . . . IF 'TRIALRUN DO MAKEBAKF^TMGKERNL(RFILE,,.BACKFNAME) IF BACKFNAME="" QUIT
  . . . WRITE "  Backed up ",RFILE," --> ",BACKFNAME,!
  . . . ;"NEW % SET %=2
  . . . ;"WRITE "  Delete ",RFILE DO YN^DICN WRITE !
  . . . ;"IF %'=1 QUIT
  . . . NEW TMGDEL SET TMGDEL(AFILE)=""
  . . . NEW TEMP SET TEMP=1
  . . . IF 'TRIALRUN SET TEMP=$$DEL^%ZISH(RDIR,"TMGDEL")
  . . . IF TEMP=0 WRITE "  Delete failed...",! QUIT
  . . . WRITE "  MOVING ",PFILE," --> ",RFILE,!
  . . . IF 'TRIALRUN SET TEMP=$$MOVE^TMGKERNL(PFILE,RFILE)
  . . ELSE  DO
  . . . WRITE !
  . . . NEW % SET %=2
  . . . WRITE "MOVING ",PFILE," --> ",RFILE,!
  . . . ;"DO YN^DICN WRITE !
  . . . ;"IF %'=1 QUIT
  . . . IF 'TRIALRUN SET %=$$MOVE^TMGKERNL(PFILE,RFILE)
  IF SOME=0 WRITE !,"No files found to move.",!
  DO PRESS2GO^TMGUSRI2
  QUIT  
  ;
EXAMKIDS(INFO) ;"Examine KIDS file, esp routines.  
  ;"Input:  INFO -- PASS BY REFERENCE.  E.g. 
  ;"           INFO("KID FILE")="DG-5p3_SEQ-831_PAT-940.kids"
  ;"           INFO("PATH")="/opt/worldvista/EHR/kids/Remote_Patches-dirFor-DG/"
  ;"Result: 1^OK, or -1^ErrMessage
  NEW RESULT SET RESULT="1^OK"
  NEW FNAME SET FNAME=$GET(INFO("KID FILE")) IF FNAME="" SET RESULT="-1^Filename not provided" GOTO EXKDN
  NEW PATH SET PATH=$GET(INFO("PATH")) IF PATH="" SET RESULT="-1^Path name not provided" GOTO EXKDN
  NEW LOCALFILE SET LOCALFILE=PATH_FNAME
  NEW ARRAY
  SET RESULT=$$KIDS2ARR(.ARRAY,PATH_FNAME) ;"Examine KIDS file, esp routines.
  NEW KIDSRTNARR,ARTN SET ARTN=""
  FOR  SET ARTN=$ORDER(ARRAY("RTN",ARTN)) QUIT:ARTN=""  DO
  . NEW IDX SET IDX=0
  . FOR  SET IDX=$ORDER(ARRAY("RTN",ARTN,IDX)) QUIT:IDX'>0  DO
  . . SET KIDSRTNARR(ARTN,IDX)=$GET(ARRAY("RTN",ARTN,IDX,0))
  NEW LOCALRTNARR,ROUTINE SET ROUTINE=""
  FOR  SET ROUTINE=$ORDER(KIDSRTNARR(ROUTINE)) QUIT:ROUTINE=""!(+RESULT=-1)  DO 
  . SET RESULT=$$GETLOCALRTN(ROUTINE,.LOCALRTNARR)
  IF +RESULT=-1 GOTO EXRTDN
  NEW OPTON SET OPTION("ENABLE COMPARE TOGGLE")=1
  SET RESULT=$$DOEXAM(.KIDSRTNARR,.LOCALRTNARR,.OPTION) IF +RESULT=-1 GOTO EXRTDN
EXKDN ;  
  QUIT RESULT
  ;  
KIDS2ARR(ARRAY,LOCALFILE) ;"Load KIDS file into a multi-dimensional array  
  ;"Input:  ARRAY -- PASS BY REFERENCE.  AN OUT PARAMETER.  Kids is parsed into array 
  ;"        LOCALFILE -- Full path and filename of local file. 
  ;"Result: 1^OK, or -1^ErrMessage
  NEW RESULT SET RESULT="1^OK"
  IF $$DOS2UNIX^TMGKERNL(LOCALFILE)  ;"ignore results. 
  NEW OPTION,LINES,TEMP SET OPTION("OVERFLOW")=1
  SET TEMP=$$HFS2ARFP^TMGIOUT3(LOCALFILE,"LINES",.OPTION)
  IF TEMP=0 SET RESULT="-1^Unable to load ["_LOCALFILE_"]" GOTO K2ADN
  NEW L1,L2,IDX1,IDX2 SET (IDX1,IDX2)=0
  FOR  SET IDX1=$ORDER(LINES(IDX2)),IDX2=$ORDER(LINES(IDX1)) QUIT:(IDX1'>0)!(IDX2'>0)  DO
  . IF IDX2#2'=0 DO
  . . WRITE "WHAT'S UP??",!
  . . DO PRESS2GO^TMGUSRI2
  . SET L1=$GET(LINES(IDX1)),L2=$GET(LINES(IDX2)) 
  . IF (L1="")!(L1'[")") DO  QUIT
  . . SET ARRAY("TMG INFO",L1)=L2
  . NEW TEMP SET TEMP="ARRAY("_L1
  . DO
  . . ;"NEW $ETRAP SET $ETRAP="WRITE ""Can't set "_TEMP_"="_L2_" ..."",! SET $ETRAP="""",$ECODE="""""
  . . new $etrap set $etrap="write ""Error Trapped.)"",! set $etrap="""",$ecode="""""
  . . SET @TEMP=L2
K2ADN ;  
  QUIT RESULT
  ;
PICKEXAMINE() ;"Ask user for entry in file 9.7
  DO SHOWTG(1) ;"SHOW ENTRIES LOADED IN TRANSPORT GLOBAL
  NEW DIC,X,Y SET DIC=9.7,DIC(0)="MAEQ",DIC("S")="I '$P(^(0),U,9),$D(^XTMP(""XPDI"",Y))"
  DO ^DIC WRITE ! IF Y'>0 QUIT
  DO EXAMRTNS(+Y)
  DO PRESS2GO^TMGUSRI2
  QUIT
  ; 
EXAMRTNS(IEN9D7)  ;"Examine routines from INSTALL record
  ;"Input: IEN9D7 -- IEN in file 9.7 (INSTALL file)
  ;"Result: 1^OK -- routines returned, or -1^ErrMessage
  NEW RESULT SET RESULT="1^OK"
  NEW KIDSRTNARR SET RESULT=$$XTRACTRTN(IEN9D7,.KIDSRTNARR) IF +RESULT=-1 GOTO EXRTDN
  NEW LOCALRTNARR,ROUTINE SET ROUTINE=""
  FOR  SET ROUTINE=$ORDER(KIDSRTNARR(ROUTINE)) QUIT:ROUTINE=""!(+RESULT=-1)  DO 
  . SET RESULT=$$GETLOCALRTN(ROUTINE,.LOCALRTNARR)
  IF +RESULT=-1 GOTO EXRTDN
  SET RESULT=$$DOEXAM(.KIDSRTNARR,.LOCALRTNARR) IF +RESULT=-1 GOTO EXRTDN
EXRTDN ;  
  QUIT RESULT
  ;
XTRACTRTN(IEN9D7,OUT)  ;"Scan ^XTMP("XPDI",IEN9D7,*) and extract routines into array
  ;"Input: IEN9D7 -- IEN in file 9.7 (INSTALL file)
  ;"       OUT -- PASS BY REFERENCE.  An OUT PARAMETER.  Format
  ;"         OUT(<RTN NAME>,LINE#)=<text>
  ;"Result: 1^OK -- routines returned, or 0^OK - no problem, but no routines, or -1^ErrMessage
  ;
  ;"for entry 10332
  ;"   yottadb>zwr ^XPD(9.7,10332,*)   <--- ^XPD(9.7) IS 'INSTALL' FILE
  ;"   ^XPD(9.7,10332,0)="XU*8.0*664^3^3220801.180133^10332^1^^0^^0"
  ;"   ^XPD(9.7,10332,2)="Released XU*8*664 SEQ #525"
  ;"   ^XPD(9.7,10332,"INI",0)="^9.713^1^1"
  ;"   ^XPD(9.7,10332,"INI",1,0)="XPD PREINSTALL COMPLETED"
  ;"   ^XPD(9.7,10332,"INI","B","XPD PREINSTALL COMPLETED",1)=""
  ;"   ^XPD(9.7,10332,"INIT",0)="^9.716^2^2"
  ;"   ^XPD(9.7,10332,"INIT",1,0)="XPD POSTINSTALL COMPLETED"
  ;"   ^XPD(9.7,10332,"INIT",2,0)="XPD POSTINSTALL STARTED"
  ;"   ^XPD(9.7,10332,"INIT",2,1)="POST^XUMF664P"
  ;"   ^XPD(9.7,10332,"INIT","B","XPD POSTINSTALL COMPLETED",1)=""
  ;"   ^XPD(9.7,10332,"INIT","B","XPD POSTINSTALL STARTED",2)=""
  ;"   
  ;"   ^XTMP("XPDI",10332,"RTN","XUMF664P",83,0)=" D NODES^XUMF664(XFIEN,""DATA1253"",1
  ;"   ^XTMP("XPDI",10332,"RTN","XUMF664P",84,0)=" D ADD2^XUMF664(XFIEN,""VistA_Mapping
  ;"   ^XTMP("XPDI",10332,"RTN","XUMF664P",85,0)=" D DELMD5^XUMF664(XFILE)"
  ;"   ^XTMP("XPDI",10332,"RTN","XUMF664P",86,0)=" D ADDMD5^XUMF664(XFILE,XFIEN)"
  ;"   ^XTMP("XPDI",10332,"RTN","XUMF664P",87,0)=" D SCMD5^XUMF664(XFILE,XFIEN)"
  ;"   ^XTMP("XPDI",10332,"RTN","XUMF664P",88,0)=" D SUBMD5^XUMF664(XFILE,"".01^20^^^""
  ;"   ^XTMP("XPDI",10332,"RTN","XUMF664P",89,0)=" D SUBMD5^XUMF664(XFILE,""99.99^10^^^
  ;"   ^XTMP("XPDI",10332,"RTN","XUMF664P",90,0)=" ;D SUBMD5^XUMF664(XFILE,""99.991^40^
  ;"   ^XTMP("XPDI",10332,"RTN","XUMF664P",91,0)=" S XFIEN=120.532"
  ;"   ^XTMP("XPDI",10332,"RTN","XUMF664P",92,0)=" D SCMD5^XUMF664(XFILE,XFIEN)"
  ;"   ^XTMP("XPDI",10332,"RTN","XUMF664P",93,0)=" D SUBMD5^XUMF664(XFILE,"".01^30^^^^D
  ;"    M12053^GMRVVZRT"",XFIEN,.01)"
  ;"   ^XTMP("XPDI",10332,"RTN","XUMF664P",94,0)=" Q"
  ;"   ^XTMP("XPDI",10332,"VER")="8.0^22.0"
  ; 
  IF '$DATA(^XTMP("XPDI",IEN9D7)) DO  GOTO XRTNDN 
  . SET RESULT="-1^NO DATA FOUND FOR IEN "_IEN9D7
  NEW RESULT SET RESULT="1^OK"
  NEW RTNNAME,TEMPARR
  SET RTNNAME=""
  FOR  SET RTNNAME=$ORDER(^XTMP("XPDI",IEN9D7,"RTN",RTNNAME)) QUIT:RTNNAME=""  DO
  . NEW LINE SET LINE=0
  . FOR  SET LINE=$ORDER(^XTMP("XPDI",IEN9D7,"RTN",RTNNAME,LINE)) QUIT:LINE'>0  DO
  . . SET TEMPARR(RTNNAME,LINE)=$GET(^XTMP("XPDI",IEN9D7,"RTN",RTNNAME,LINE,0))
  . . ;"PROBABLY NOT NECESSARY BUT CHECK TO SEE IF THERE ARE ANY SUBLINES AND APPEND TO ABOVE
  . . NEW SUBIDX SET SUBIDX=0
  . . FOR  SET SUBIDX=$ORDER(^XTMP("XPDI",IEN9D7,"RTN",RTNNAME,LINE,SUBIDX)) QUIT:SUBIDX'>0  DO
  . . . NEW SUBLINE SET SUBLINE=$GET(^XTMP("XPDI",IEN9D7,"RTN",RTNNAME,LINE,SUBIDX))
  . . . SET TEMPARR(RTNNAME,LINE)=$GET(OUT(RTNNAME,LINE))_SUBLINE
  IF '$DATA(TEMPARR) SET RESULT="0^OK" GOTO XRTNDN
  MERGE OUT=TEMPARR
XRTNDN  
  QUIT RESULT
  ;
GETLOCALRTN(RTN,OUT)  ;"Extract a local routine module into OUT
  ;"Input: RTN -- the name of the local routine
  ;"       OUT -- PASS BY REFERENCE.  An OUT PARAMETER.  Format
  ;"         OUT(<RTN NAME>,LINE#)=<text>
  ;"Result: 1^OK  or 0^NO ROUTINE FOUND", or -1^ErrMessage
  NEW RESULT SET RESULT="1^OK"
  NEW DONE SET DONE=0
  NEW BLANKCNT SET BLANKCNT=0
  NEW LASTLINE SET LASTLINE=0  ;"This will keep track of the last good line
  FOR IDX=1:1 QUIT:DONE  DO
  . SET LINE=$TEXT(+IDX^@RTN)   
  . SET OUT(RTN,IDX)=LINE
  . IF LINE'="" DO
  . . SET BLANKCNT=0
  . . SET LASTLINE=IDX  ;"Set as the last good found line
  . ELSE  DO
  . . SET BLANKCNT=BLANKCNT+1
  . IF BLANKCNT>20 SET DONE=1  ;"Stop after 20 blank lines
  FOR  SET LASTLINE=$ORDER(OUT(RTN,LASTLINE)) QUIT:LASTLINE'>0  DO
  . KILL OUT(RTN,LASTLINE)  ;"Remove everything after the last good line
  IF '$DATA(OUT) SET RESULT="0^NO ROUTINE FOUND"
  QUIT RESULT
  ;
DOEXAM(KIDSARR,LOCALARR,OPTION)  ;"Prompt user via menu to pick a result from array to examine, and then view etc.    
  ;"Input: KIDSARR -- PASS BY REFERENCE.  Array of routines found in KIDS.  Format:   
  ;"         KIDSARR(<RTN NAME>,LINE#)=<text>
  ;"Input: LOCALSARR -- PASS BY REFERENCE.  Array of routines found in local system.  Format:
  ;"         KIDSARR(<RTN NAME>,LINE#)=<text>
  ;"       OPTION -- OPTIONAL.  
  ;"          OPTION("ENABLE COMPARE TOGGLE")=1  If present, user can select between working with KIDS routines vs comparing to local
  ;"Result: 1^OK or -1^ErrMessage
  NEW RESULT SET RESULT="1^OK"
  NEW MENU,IDX,USRPICK,VERB,ROUTINENAME,NOLOCAL,LOCALTAGS,CONFLICT
  NEW COMPAREMODE SET COMPAREMODE=1
  NEW TOGGLEOK SET TOGGLEOK=+$GET(OPTION("ENABLE COMPARE TOGGLE"))
DEL1 ;  
  SET IDX=1,ROUTINENAME=""
  KILL MENU,NOLOCAL SET MENU(0)="Select Routine For Examination"
  FOR  SET ROUTINENAME=$ORDER(KIDSARR(ROUTINENAME)) QUIT:ROUTINENAME=""  DO
  . IF $DATA(LOCALARR(ROUTINENAME)) DO
  . . NEW TEMP IF $$CHKLOCLRTN^TMGPAT4(ROUTINENAME,.TEMP,.LOCALTAGS)=1 DO
  . . . MERGE CONFLICT(ROUTINENAME)=TEMP
  . . IF COMPAREMODE=1 DO
  . . . NEW LCL SET LCL=$SELECT($DATA(CONFLICT(ROUTINENAME))>0:" <- LOCAL MODIFICATION",1:"")
  . . . SET MENU(IDX)="Compare KIDS vs LOCAL routine: ["_ROUTINENAME_"]"_LCL_$CHAR(9)_"COMPARE^"_ROUTINENAME,IDX=IDX+1
  . . ELSE  DO
  . . . SET MENU(IDX)="View KIDS routine:  ["_ROUTINENAME_"]"_$CHAR(9)_"VIEW^"_ROUTINENAME,IDX=IDX+1
  . . . SET MENU(IDX)="View LOCAL routine: ["_ROUTINENAME_"]"_$CHAR(9)_"VIEW_LOCAL^"_ROUTINENAME,IDX=IDX+1
  . ELSE  DO
  . . SET NOLOCAL(ROUTINENAME)=""
  NEW JDX SET JDX=1,ROUTINENAME=""
  FOR  SET ROUTINENAME=$ORDER(CONFLICT(ROUTINENAME)) QUIT:ROUTINENAME=""  DO
  . SET MENU(0,JDX)="NOTE: routine ["_ROUTINENAME_"] has code tags indicating local edits!",JDX=JDX+1
  IF JDX=1 SET MENU(0,JDX)="NOTE: no conflicting local edits found."
  SET ROUTINENAME=""
  FOR  SET ROUTINENAME=$ORDER(NOLOCAL(ROUTINENAME)) QUIT:ROUTINENAME=""  DO
  . SET MENU(IDX)="View *NEW* KIDS routine: ["_ROUTINENAME_"]"_$CHAR(9)_"VIEW^"_ROUTINENAME,IDX=IDX+1
  IF TOGGLEOK DO
  . NEW STR SET STR=$SELECT(COMPAREMODE=1:"OFF",1:"ON")
  . SET MENU(IDX)="Toggle COMPARISON mode to "_STR_$CHAR(9)_"TOGGLE_COMPARE^",IDX=IDX+1
  SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
  IF USRPICK="^" GOTO DEDN
  SET VERB=$PIECE(USRPICK,"^",1)
  SET ROUTINENAME=$PIECE(USRPICK,"^",2)
  IF VERB="VIEW" DO  GOTO DEL1
  . DO SHOW1RTN(.KIDSARR,ROUTINENAME,1)
  IF VERB="VIEW_LOCAL" DO  GOTO DEL1
  . DO SHOW1RTN(.LOCALARR,ROUTINENAME,1)
  IF VERB="COMPARE" DO  GOTO DEL1
  . DO DIFF2RTNS^TMGPAT7(.KIDSRTNARR,.LOCALRTNARR,ROUTINENAME)
  IF VERB="TOGGLE_COMPARE" DO  GOTO DEL1
  . SET COMPAREMODE='COMPAREMODE  
DEDN ;  
  QUIT RESULT
  ;
SHOW1RTN(ARR,NAME,NOPAUSE)  ;"
  NEW TMG1RTN MERGE TMG1RTN=ARR(NAME)
  IF '$DATA(TMG1RTN) DO  GOTO S1RDN
  . WRITE !,"No data to show.",!
  NEW L1 SET L1="=========================================^"
  SET L1=L1_"ROUTINE: "_NAME_" from KIDS Patch^"
  SET L1=L1_"VIEW ONLY -- Any edits to this file will be DISCARDED.^"
  SET L1=L1_"To exit, type <CTRL>X  (upper OR lowercase X)^"
  SET L1=L1_"========================================="
  DO PREFIXLINE2ARR^TMGSTUT2(.TMG1RTN,L1) 
  DO EDITARRAY^TMGKERNL(.TMG1RTN)  
  ;"DO EDITARR2^TMGKERN8("TMG1RTN","nano","#NOTE: any edits made to this file will be discarded!")
S1RDN ;  
  IF $GET(NOPAUSE)'=1 DO PRESS2GO^TMGUSRI2
  QUIT
  ;
COMP2RTNS(KIDSARR,LOCALARR,NAME,NOPAUSE)  ;"COMPARE 2 ROUTINES
  DO SHOW1RTN(.KIDSARR,NAME,1)
  WRITE !,"==========================================",!
  DO SHOW1RTN(.LOCALARR,NAME,1)  
  IF $GET(NOPAUSE)'=1 DO PRESS2GO^TMGUSRI2
  QUIT
  ;
