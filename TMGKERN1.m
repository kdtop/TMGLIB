TMGKERN1 ;TMG/kst/Mumps Distro Specific functions ;2/2/14, 3/3/17, 5/12/17
         ;;1.0;TMG-LIB;**1**;10/24/11
 ;
 ;"TMG KERNEL FUNCTIONS
 ;"I.e. functions that are specific to mumps distro (e.g. GT.M vs Cache')
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
 ;"
 ;"LASTJOBN() -- return the process number of the child process
 ;"RTNPATH(ROUTINE,MODE)  -- GET ROUTINE PATH IN HFS
 ;"MD5FILE(FPATHNAME) --CALCULATE MD5SUM FOR ARBITRARY FILE   
 ;"MD5SUM(ROUTINE)  -- CALCULATE MD5SUM FOR ROUTINE.
 ;"MD5ALL -- SHOW MD5SUM VALUES FOR ALL ENTRIES IN 'ROUTINE' FILE.
 ;"MD5ARR(REF) --CALCULATE MD5SUM OF ARRAY   
 ;"LSTLCKS(OUT) --"LIST MUMPS DATABASE GLOBAL LOCKS  
 ;"KILOKJOB ;"KILL JOBS HOLDING PARTICULAR DATABASE LOCKS 
 ; 
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"              
 ;"=======================================================================
 ;
LASTJOBN() ;
  ;"Purpose: to return the process number of the child process
  ;"         created by last JOB command
  ;"Results: Returns process number, OR -1^Message if problem
  NEW OS,RESULT
  ;"SET OS=$GET(^%ZOSF("OS"))
  ;"IF OS["OpenM" SET RESULT=$ZCHILD
  ;"ELSE  IF OS["GT.M" SET RESULT=$ZJOB
  ;"ELSE  SET RESULT="-1^Unknown MUMPS environment in LASTJOBN^TMGKERN1"
  SET RESULT=$ZJOB
  QUIT RESULT                 
  ;                
RTNPATH(ROUTINE,MODE)  ;"GET ROUTINE PATH IN HFS
  ;"NOTE: need to research if $ZDIRECTORY would do this routine automatically...
  ;"NOTE: code copied and modified from GETPATH^TMGMGRST
  ;"Purpose: To take file, and look through file path to determine which path the file
  ;"        exists in.     
  ;"        e.g. IF $ZRO="ObjDir1(SourceDir1 SourceDir2) ObjDir2(SourceDir3 SourceDir4)"
  ;"          then this function will look in SourceDir's 1..4 to see which one contains
  ;"          FILE.  Functions will return the appropriate SourceDir
  ;"Input: ROUTINE: the routine name to look for, WITHOUT extension.  e.g. "XUP", NOT "XUP.m"
  ;"       MODE:  OPTIONAL.  if 1, then full path+filename returned.  Otherwise just returns path
  ;"Result: "" if not found, or proble.
  ;"        If MODE=1, will return full filepathname
  ;"        Else Will return the source directory, e.g. /usr/local/OpenVistA/r/, or ""    
  NEW RESULT SET RESULT=""
  SET ROUTINE=$GET(ROUTINE) IF ROUTINE="" GOTO RTNPDN
  NEW FILE SET FILE=$TRANSLATE(ROUTINE_".m","%","_")
  NEW ZRO SET ZRO=$ZROUTINES
  NEW GROUP
  FOR  QUIT:(ZRO="")!(RESULT'="")  DO
  . SET GROUP=$PIECE(ZRO,")",1)
  . SET ZRO=$$TRIM^XLFSTR($PIECE(ZRO,")",2,999))
  . NEW SRC SET SRC=$PIECE(GROUP,"(",2)
  . NEW IDX FOR IDX=1:1:$LENGTH(SRC," ") DO  QUIT:RESULT'=""
  . . NEW ONEDIR SET ONEDIR=$PIECE(SRC," ",IDX) QUIT:ONEDIR=""
  . . SET ONEDIR=$$MKTRALDV^TMGIOUTL(ONEDIR,"/")
  . . IF $$FILEXIST^TMGIOUTL(ONEDIR_FILE)=0 QUIT
  . . SET RESULT=ONEDIR
  . . IF $GET(MODE)=1 SET RESULT=RESULT_FILE
RTNPDN ;  
  QUIT RESULT
  ;
MD5FILE(FPATHNAME)  ;"CALCULATE MD5SUM FOR ARBITRARY FILE
  ;"Input: FPATHNAME -- the filename to calculate
  ;"Results: returns MD5SUM string, or "" if problem.
  ;"NOTE: depends on md5sum being available on host OS (linux)
  NEW RESULT SET RESULT=""
  NEW HOOKCMD SET HOOKCMD="md5sum "_FPATHNAME
  NEW P SET P="temp"
  OPEN P:(COMMAND=HOOKCMD:readonly)::"pipe"
  USE P
  NEW LINEIN
  FOR  DO  QUIT:($ZEOF)!(RESULT'="")
  . READ LINEIN
  . SET RESULT=$PIECE(LINEIN," ",1)
  CLOSE P
  USE $P
  QUIT RESULT
  ;
MD5SUM(ROUTINE)  ;"CALCULATE MD5SUM FOR ROUTINE.
  ;"Input: ROUTINE: the routine name to look for, WITHOUT extension.  e.g. "XUP", NOT "XUP.m"
  ;"Results: returns MD5SUM string, or "" if problem.
  ;"NOTE: depends on md5sum being available on host OS (linux)
  NEW FILE SET FILE=$$RTNPATH(.ROUTINE,1)
  NEW RESULT SET RESULT=""
  IF FILE'="" SET RESULT=$$MD5FILE(FILE) 
  QUIT RESULT
  ;
MD5ALL(OUT) ;"GET / SHOW MD5SUM VALUES FOR ALL ENTRIES IN 'ROUTINE' FILE.
  NEW STARTPOINT SET STARTPOINT=""
  NEW ROUTINE SET ROUTINE=STARTPOINT
  NEW MAXRTNCT SET MAXRTNCT=0  
  FOR  SET ROUTINE=$ORDER(^DIC(9.8,"B",ROUTINE)) QUIT:(ROUTINE="")  DO
  . SET MAXRTNCT=MAXRTNCT+1
  NEW STARTTIME SET STARTTIME=$H
  SET ROUTINE=STARTPOINT                                                                          
  NEW RTNCT SET RTNCT=0
  NEW DONE SET DONE=0
  NEW OUT
  FOR  SET ROUTINE=$ORDER(^DIC(9.8,"B",ROUTINE)) QUIT:(ROUTINE="")!DONE  DO
  . SET RTNCT=RTNCT+1
  . IF RTNCT#10=0 DO
  . . DO PROGBAR^TMGUSRI2(RTNCT,$$LJ^XLFSTR(ROUTINE,9),0,MAXRTNCT,70,STARTTIME)
  . NEW RESULT SET RESULT=$$MD5SUM(ROUTINE)
  . IF RESULT="" WRITE !,"PROBLEM FOR ",ROUTINE QUIT
  . ;"WRITE ROUTINE," --> ",RESULT,!
  . SET OUT(ROUTINE)=RESULT
  IF $DATA(ARRAY) DO ZWRITE^TMGZWR("ARRAY")
  QUIT
  ;
MD5ARR0(ARR)  ;"RUN A MD5 CHECKSUM ON AN ARRAY ... DEPRECIATED, USE ONE BELOW, MORE ROBUST.  DELETE THIS LATER
  ;"Input: ARR -- PASS BY REFERENCE.  Expected format:  ARR(#)=<text>
  ;"Result: the md5 checksum, as calculated by host linux md5sum command
  ;"        or -1^message
  NEW FNAME SET FNAME=$$UNIQUE^%ZISUTL("md5sum_arr_calc.tmp")
  NEW PATH SET PATH="/tmp/"
  NEW TMGRESULT SET TMGRESULT=$$ARR2HFS^TMGIOUT3("ARR",PATH,FNAME) ;"Array to HFS
  IF TMGRESULT=0 DO  GOTO MD5ADN
  . SET TMGRESULT="-1^Error saving temp file. Path='"_PATH_"', Filename='"_FNAME_"'"
  NEW FILE SET FILE=PATH_FNAME
  SET TMGRESULT=$$MD5FILE(FILE)
MD5ADN  ;
  QUIT TMGRESULT
  ;  
MD5ARR(REF)  ;"CALCULATE MD5SUM OF ARRAY
  ;"Input: REF -- Pass by NAME.  The NAME of the reference to calculate. 
  ;"NOTE: This does include sub nodes.  E.g. 
  ;"            @REF@(1)="cat"  <-- this is included
  ;"            @REF@(1,2)="calico"   <-- also included
  ;"            @REF@(2)="dog"
  ;"NOTE2: This outputs the ZWRITE output of the array to a temp file, and does MD5SUM of that
  ;"RESULT: MD5Sum value (a 32 digit hex number), or "-1^Message" if error
  NEW TMGRESULT SET TMGRESULT="-1^UNKNOWN"
  IF $GET(REF)="" DO  GOTO MD5ARDN
  . SET TMGRESULT="-1^Name of array for md5sum calculation not provided."
  IF $DATA(@REF)=0 DO  GOTO MD5ARDN
  . SET TMGRESULT="-1^Unable to calculate md5sum because array '"_REF_"' is empty."
  NEW TMGZZZARR DO ZWR2ARR^TMGZWR(REF,"TMGZZZARR")
  IF $DATA(TMGZZZARR)=0 DO  GOTO MD5ARDN
  . SET TMGRESULT="-1^Unable to ZWRITE arrray '"_REF_"'."
  NEW PATH SET PATH="/tmp/"
  NEW FNAME SET FNAME=$$UNIQUE^%ZISUTL("tempMD5sum.tmp")
  SET TMGRESULT=$$ARR2HFS^TMGIOUT3("TMGZZZARR",PATH,FNAME)
  IF TMGRESULT=0 DO  GOTO MD5ARDN
  . SET TMGRESULT="-1^Unable to save temporary file: "_PATH_FNAME
  SET TMGRESULT=$$MD5FILE(PATH_FNAME)
  ZSYSTEM "rm "_PATH_FNAME  ;"delete file when done  
MD5ARDN ;
  QUIT TMGRESULT
  ;  
LSTLCKS(OUT)  ;"LIST MUMPS DATABASE GLOBAL LOCKS
  ;"input: OUT -- PASS BY REFERENCE.  AN OUT PARAMETER.  Format: 
  ;"         OUT(#,"REF")=name of global that is locked
  ;"         OUT(#,"PID")=the process id ($J) of the job holding lock
  ;"         OUT(#,"EXISTING")=1 if lke utility reports process as existing.  
  NEW IDX,INFO
  NEW CT SET CT=1
  OPEN "lkepipe":(shell="/bin/sh":command="$gtm_dist/lke show -all -wait":readonly)::"pipe"
  USE "lkepipe" 
  FOR IDX=1:1  READ INFO(IDX) QUIT:$ZEOF
  USE $PRINCIPAL CLOSE "lkepipe"
  SET IDX=0
  FOR  SET IDX=$ORDER(INFO(IDX)) QUIT:(+IDX'>0)  DO
  . NEW STR SET STR=$$TRIM^XLFSTR($GET(INFO(IDX))) QUIT:STR=""
  . IF $EXTRACT(STR,1)'="^" QUIT
  . NEW REF SET REF=$PIECE(STR,"Owned by PID=",1) QUIT:REF=""
  . NEW I2 SET I2=-1
  . IF STR'["Owned by PID=" DO
  . . SET I2=+$ORDER(INFO(IDX)) QUIT:I2=0
  . . SET STR=$GET(INFO(I2))
  . . SET IDX=I2
  . IF I2=0 QUIT
  . NEW PID SET PID=+$$TRIM^XLFSTR($PIECE(STR,"Owned by PID=",2))
  . NEW EXISTING SET EXISTING=(STR["which is an existing process")
  . SET OUT(CT,"REF")=REF
  . SET OUT(CT,"PID")=PID
  . SET OUT(CT,"EXISTING")=EXISTING
  . SET CT=CT+1
  QUIT
  ;
KILOKJOB ;"KILL JOBS HOLDING PARTICULAR DATABASE LOCKS
  NEW LOCKS,USRSLCT 
  NEW MENU,IDX,MENUCT 
KLJ1  ;  
  KILL LOCKS,MENU
  DO LSTLCKS(.LOCKS)
  SET MENU(0)="Select locked global that should be unlocked."
  SET MENU(0,1)=" (achieved by killing job that owns lock!)"
  SET MENUCT=1
  SET IDX=0 FOR  SET IDX=$ORDER(LOCKS(IDX)) QUIT:+IDX'>0  DO
  . NEW REF SET REF=$GET(LOCKS(IDX,"REF")) QUIT:REF=""
  . NEW PID SET PID=$GET(LOCKS(IDX,"PID")) QUIT:REF=""
  . NEW STR SET STR=REF_" $JOB="_PID_$CHAR(9)_PID
  . SET MENU(MENUCT)=STR,MENUCT=MENUCT+1
  WRITE !
  SET USRSLCT=$$MENU^TMGUSRI2(.MENU)
  IF USRSLCT="^" GOTO KLJ2
  IF +USRSLCT>0 DO  
  . NEW PID SET PID=+USRSLCT
  . WRITE "***************************************",!
  . WRITE "** CAUTION.  KILLING A PROCESS CAN  **",!
  . WRITE "** CAUSE LOSS OF PATIENT DATA OR    **",!
  . WRITE "** OTHER DATABASE PROBLEMS!         **",!
  . WRITE "**************************************",!
  . WRITE "Do you REALLY want to permanently kill process #"_PID
  . NEW % SET %=2 DO YN^DICN 
  . IF %'=1 QUIT
  . DO KILLPID^TMGKERNL(PID)
  GOTO KLJ1
  ;  
KLJ2 ;  
  WRITE !,"Goodbye.",!  
  QUIT  
  ;  
 ;"================================================================
DEV2ARR(DEVICE,OUT,FILTER,INFO)  ;"Store off a device, with all it's parameters
   ;"NOTE: This depends on YottaDB ZSHOW "D" command, which does NOT show all 
   ;"      device parameters (unfortunately).  Known to be missing are: CONVERT, HOSTSYNC, TTSYNC
   ;"INPUT:  DEVICE -- the name of the device to query.  Can call with $P or $IO etc. 
   ;"        OUT -- PASS BY REFERENCE.  AN OUT PARAMETER.  Format:
   ;"              finishe....
   ;"        FILTER -- OPTIONAL.  If provided, then string of flags for desired info to be return
   ;"                If FILTER ="" then default is "GNRS", to return ALL
   ;"                If FILTER["G" -- return GENERAL info
   ;"                If FILTER["N" -- return NON-DEFAULT info
   ;"                If FILTER["R" -- return RAW PARAMS info
   ;"                If FILTER["S" -- return STATE info
   ;"        INFO -- OPTIONAL.  PASS BY FREFERENCE.  An array that has setup information from prior run (for faster execution)
   NEW TEMP ZSHOW "D":TEMP
   NEW DONE SET DONE=0
   SET FILTER=$GET(FILTER,"GNRS")
   NEW APARAM
   NEW ARR,TOKEN,ADEVICE,MATCHDEV SET MATCHDEV=0
   NEW IDX,JDX,KDX SET IDX=0,KDX=1
   FOR  SET IDX=$ORDER(TEMP("D",IDX)) QUIT:(IDX'>0)  DO  QUIT:MATCHDEV
   . NEW ENTRY SET ENTRY=$GET(TEMP("D",IDX)) QUIT:ENTRY="" 
   . KILL ARR SET ADEVICE=""
   . FOR JDX=1:1:$LENGTH(ENTRY," ") DO
   . . SET TOKEN=$PIECE(ENTRY," ",JDX) QUIT:TOKEN=""
   . . IF JDX=1 SET OUT("GENERAL","NAME")=TOKEN,ADEVICE=TOKEN QUIT
   . . IF JDX=2 SET OUT("GENERAL","STATUS")=TOKEN QUIT
   . . IF JDX=3 SET OUT("GENERAL","TYPE")=TOKEN QUIT
   . . SET ARR(KDX)=TOKEN,KDX=KDX+1
   . SET MATCHDEV=(ADEVICE=DEVICE)
   . IF MATCHDEV=0 KILL ARR QUIT 
   IF (MATCHDEV=0)!(IDX'>0) GOTO D2ADN
   MERGE OUT("RAW PARAMS")=ARR
   FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:IDX'>0  DO
   . SET APARAM=$GET(ARR(IDX)) QUIT:APARAM=""
   . IF APARAM["=" SET APARAM=$PIECE(APARAM,"=",1)_"="
   . SET OUT("RAW PARAMS","B",APARAM)=IDX
   IF $DATA(INFO)=0 DO SETUPSAV(.INFO)
   SET IDX=0
   FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:IDX'>0  DO
   . SET APARAM=$GET(ARR(IDX)) QUIT:APARAM=""
   . NEW TRIMPARAM SET TRIMPARAM=APARAM
   . NEW KEYVAL SET KEYVAL=0
   . IF TRIMPARAM["=" DO
   . . SET TRIMPARAM=$PIECE(APARAM,"=",1)_"="
   . . SET KEYVAL=1,KEYVAL("VAL")=$PIECE(APARAM,"=",2)
   . ;"First look for easy entries that have a value if ON
   . IF $DATA(INFO("ONDISP",TRIMPARAM)) DO
   . . NEW ENTRY SET ENTRY=$ORDER(INFO("ONDISP",TRIMPARAM,"")) QUIT:ENTRY=""
   . . ;"SET ENTRY=$GET(INFO("ENTRY",ENTRY)) ;"format:  Turn_on^On_disp^Turn_off^Off_disp
   . . SET OUT("STATE",ENTRY)=$SELECT(KEYVAL=1:$GET(KEYVAL("VAL")),1:"ON")
   . ;"First look for easy entries that have a value if OFF
   . IF $DATA(INFO("OFFDISP",APARAM)) DO
   . . NEW ENTRY SET ENTRY=$ORDER(INFO("OFFDISP",TRIMPARAM,"")) QUIT:ENTRY=""
   . . SET OUT("STATE",ENTRY)=$SELECT(KEYVAL=1:$GET(KEYVAL("VAL")),1:"OFF")
   ;"Next check for all the NULL when ON entries.  
   NEW ENTRY SET ENTRY=""
   FOR  SET ENTRY=$ORDER(INFO("NULLWHENON",ENTRY)) QUIT:ENTRY=""  DO
   . NEW DATA SET DATA=$GET(INFO("ENTRY",ENTRY)) QUIT:DATA=""
   . NEW OFFVALUE SET OFFVALUE=$PIECE(DATA,"^",4)
   . IF $DATA(OUT("RAW PARAMS","B",OFFVALUE)) QUIT
   . NEW TURNONCMD SET TURNONCMD=$PIECE(DATA,"^",1)  ;"piece 1 is the TURN ON command.
   . IF TURNONCMD["""""" SET TURNONCMD=""
   . SET OUT("STATE",ENTRY)="ON"  ;"TURNONCMD
   ;"Next check for all the NULL when OFF entries.  
   NEW ENTRY SET ENTRY=""
   FOR  SET ENTRY=$ORDER(INFO("NULLWHENOFF",ENTRY)) QUIT:ENTRY=""  DO
   . NEW DATA SET DATA=$GET(INFO("ENTRY",ENTRY)) QUIT:DATA=""
   . NEW ONVALUE SET ONVALUE=$PIECE(DATA,"^",2)
   . IF $DATA(OUT("RAW PARAMS","B",ONVALUE)) QUIT
   . NEW TURNOFFCMD SET TURNOFFCMD=$PIECE(DATA,"^",3)  ;"piece 1 is the TURN OFF command.  
   . IF TURNOFFCMD["""""" SET TURNOFFCMD=""
   . SET OUT("STATE",ENTRY)="OFF"  ;"TURNOFFCMD
   ;"Next, show the entries for which we can't tell state
   FOR  SET ENTRY=$ORDER(INFO("UNKNOWN",ENTRY)) QUIT:ENTRY=""  DO
   . NEW DATA SET DATA=$GET(INFO("ENTRY",ENTRY)) QUIT:DATA=""
   . NEW TURNONCMD SET TURNONCMD=$PIECE(DATA,"^",1)  ;"piece 1 is the TURN ON command.
   . NEW TURNOFFCMD SET TURNOFFCMD=$PIECE(DATA,"^",3)  ;"piece 1 is the TURN OFF command.  
   . ;"SET OUT("STATE",ENTRY)=TURNONCMD_"/"_TURNOFFCMD_"--> ? STATUS"
   . SET OUT("STATE",ENTRY)="?"
   ;"Check Special cases
   SET ENTRY="TERMINATOR"  ;"Both "" and TERM=$C() can indicate found.  NULL means $C(13)
   IF $DATA(OUT("RAW PARAMS","B","TERM="))=0 DO
   . SET OUT("STATE","TERMINATOR")="$C(13)"
   ;"Now compare to default state
   SET APARAM=""
   FOR  SET APARAM=$ORDER(INFO("ENTRY",APARAM)) QUIT:APARAM=""  DO
   . NEW ENTRY SET ENTRY=$GET(INFO("ENTRY",APARAM)) QUIT:ENTRY=""
   . NEW DEFAULTVAL SET DEFAULTVAL=$PIECE(ENTRY,"^",5)
   . NEW CURRENTVAL SET CURRENTVAL=$GET(OUT("STATE",APARAM))
   . IF CURRENTVAL=DEFAULTVAL QUIT
   . IF DEFAULTVAL="#",CURRENTVAL=+CURRENTVAL QUIT
   . SET OUT("NON-DEFAULT",APARAM)=CURRENTVAL
   ;"Get $x,$y for device
   USE DEVICE
   SET OUT("STATE","$X")=$X,OUT("NON-DEFAULT","$X")=$X
   SET OUT("STATE","$Y")=$Y,OUT("NON-DEFAULT","$Y")=$Y
   USE $IO
   ;"Now filter out undesired nodes
   NEW FLAG FOR FLAG="G,g,GENERAL","N,n,NON-DEFAULT","R,r,RAW PARAMS","S,s,STATE" DO
   . NEW F1,F2 SET F1=$PIECE(FLAG,",",1),F2=$PIECE(FLAG,",",2)
   . IF (FILTER[F1)!(FILTER[F2) QUIT
   . NEW NODE SET NODE=$PIECE(FLAG,",",3)
   . KILL OUT(NODE)
   KILL OUT("RAW PARAMS","B")
D2ADN ;   
   QUIT
   ;
USESAVEDDEV(ARR) ;
   ;"NOTICE!! This function is designed for use with YottaDB TERMINAL devices only
  NEW PARAMSTR SET PARAMSTR=""
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(ARR("PARAMS",IDX)) QUIT:IDX'>0  DO
  . NEW APARAM SET APARAM=$GET(ARR("PARAMS",IDX)) QUIT:APARAM=""
  . IF PARAMSTR'="" SET PARAMSTR=PARAMSTR_":"
  . SET PARAMSTR=PARAMSTR_APARAM
  NEW DEVNAME SET DEVNAME=$GET(ARR("NAME"))
  ;"USE DEVNAME:(@PARAMSTR)  ;"<--- NOT CORRERCT, NEED FIX
  QUIT
  
  
SETUPSAV(ARR)  ;"Get information about TERMINAL device parameters
  ;"INPUT: ARR -- PASS BY REFERENCE, AN OUT PARAMETER. Format
  ;"        ARR("ENTRY",<ParameterLabel>)=<TurnOnCmd>^<OnDisplay>^<TurnOffCmd>^<OffDisplay>^<DefaultValue>
  ;"        ARR("ONDISP",<OnDisplay>,<ParameterLabel>)=""
  ;"        ARR("OFFDISP",<OffDisplay>,<ParameterLabel>)=""
  ;"        ARR("UNKNOWN",<OffDisplay>,<ParameterLabel>)=""  <-- entries that have NULL OnDisplay and OffDisplay values
  ;"RESULT: none
  ;
  NEW DONE SET DONE=0
  NEW IDX FOR IDX=1:1 DO  QUIT:DONE
  . NEW LINE,LABEL,TURNON,ONDISP,TURNOFF,OFFDISP,DEFAULT   
  . SET LINE=$TEXT(ZZ+IDX^TMGKERN1)
  . IF LINE["<DONE>" SET DONE=1 QUIT
  . SET LINE=$PIECE(LINE,";;""",2,99)
  . SET LABEL=$$TRIM^XLFSTR($PIECE(LINE,"/",1))
  . SET TURNON=$$TRIM^XLFSTR($PIECE(LINE,"/",2))
  . SET ONDISP=$$TRIM^XLFSTR($PIECE(LINE,"/",3))
  . SET TURNOFF=$$TRIM^XLFSTR($PIECE(LINE,"/",4))
  . SET OFFDISP=$$TRIM^XLFSTR($PIECE(LINE,"/",5))
  . SET DEFAULT=$$TRIM^XLFSTR($PIECE(LINE,"/",6))
  . SET ARR("ENTRY",LABEL)=TURNON_"^"_ONDISP_"^"_TURNOFF_"^"_OFFDISP_"^"_DEFAULT
  . IF ONDISP'="" SET ARR("ONDISP",ONDISP,LABEL)=""
  . IF OFFDISP'="" SET ARR("OFFDISP",OFFDISP,LABEL)=""
  . IF ONDISP="",OFFDISP'="" SET ARR("NULLWHENON",LABEL)=""
  . IF OFFDISP="",ONDISP'="" SET ARR("NULLWHENOFF",LABEL)=""
  . IF OFFDISP="",ONDISP="" SET ARR("UNKNOWN",LABEL)=""
  QUIT
   ;"format: 
   ;"   Label       Turn_on      On_disp     Turn_off        Off_disp  / Default
ZZ ;                                               
   ;;"EXCEPTION  / EXEC=@      / EXEC=     / EXECEPTION="" /           / OFF
   ;;"CTRLC_BRK  / CENABLE     /           / NOCENABLE     / NOCENE    / ON
   ;;"CTRL_TRAP  / CTRAP=@     / CTRA=     / CTRAP=""      /           / OFF
   ;;"EDIT_MODE  / EDITING     / EDIT      / NOEDITING     /           / OFF
   ;;"EMPTERM    / EMPTERM     / EMPTERM   / NOEMPTERM     /           / OFF
   ;;"ESC_PROS   / ESCAPE      /           / NOESCAPE      / NOESCA    / OFF
   ;;"INSERT     / INSERT      /           / NOINSERT      / NOINSE    / ON 
   ;;"PASSTHRU   / PASTHRU     / PAST      / NOPASTHRU     / NOPAST    / OFF
   ;;"TERMINATOR / TERMINATOR= / TERM=     / NOTERMINATOR  / TERM=$C() / $C(13)  
   ;;"UPPERCASE  / CONVERT     /           / NOCONVERT     /           / ?
   ;;"FILTERXY   / FILTER=     / FIL=      / NOFILTER      /           / OFF
   ;;"HOSTSYNC   / HOSTSYNC    /           / NOHOSTSYNC    /           / ?
   ;;"READSYNC   / READSYNC    / READS     / NOREADSYNC    / NOREADS   / OFF
   ;;"TTSYNC     / TTSYNC      /           / NOTTSYNC      /           / ?
   ;;"TYPEAHEAD  / TYPEAHEAD   / TYPE      / NOTYPEAHEAD   / NOTYPE    / ON
   ;;"PAGE_LEN   / LENGTH=     / LENG=     / LENGTH=0      / LENG=0    / #
   ;;"PAGE_WIDTH / WIDTH=      / WIDTH=    / WIDTH=0       / WIDTH=0   / #
   ;;"WRAP_LINE  / WRAP        /           / NOWRAP        / NOWRAP    / ON
   ;;"ECHO_INPUT / ECHO        /           / NOECHO        / NOECHO    / ON
   ;;"CANONICAL  / CANONICAL   / CANONICAL / NOCANONICAL   /           / OFF
   ;;"<DONE>
   
  
  
 