TMGPAT1B  ;TMG/kst/Patching tools ;09/17/08, 2/2/14
         ;;1.0;TMG-LIB;**1**;09/17/08
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"This is a rewrite of TMGPAT1B, using newer resources and file 22709.1
 
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"DOIENS(IENS) -- install patch, given IENS to it's entry in 22709.1
 ;
 ;"=======================================================================
 ;"Private Functions
 ;"=======================================================================
 ;"=======================================================================
 ;
DOIENS(IENS)  ;"Note: This is a rewrite and revision of DOIENS^TMGPAT1
  ;"Purpose: install patch, given IENS to it's entry in 22709.1
  ;"Input: NEXTIENS -- IENS for entry in 22709.11
  ;"Results: 1 if OK, 0 IF problem.
  NEW INFO  ;"//kt added 6/17/22
  NEW ABORT SET ABORT=0
  IF NEXTPATCH'="" DO
  . WRITE "Next patch to install is: ",?50,NEXTPATCH,!
  ELSE  DO  GOTO DINSDN
  . WRITE "No more patches available for this package.",!
  . SET ABORT=1
  . QUIT  ;  ;"//kt fix below later...
  . WRITE "View list of all patches for this package on patch repository server"
  . SET %=2
  . DO YN^DICN WRITE !
  . IF %=1 IF $$EditHFSFile^TMGKERNL(PCKDIRFNAME)  ;"EDITHFSFILE(FILEPATHNAME,EDITOR)
  . IF %=-1 SET restart=1 QUIT
  NEW OPTION SET OPTION("VERBOSE")=1
  NEW DLSUCCESS SET DLSUCCESS=0  ;"default to failure
  SET RESULT=$$ENSRLOCL^TMGPAT2(NEXTIENS,.INFO,.Msg,.OPTION,PCKINIT)
  IF RESULT=0 DO  GOTO DINSDN
  . DO ADDMSG^TMGPAT2("Unable to find patch on local file system.",1,Msg)
  . IF $$SHOWMSG^TMGPAT2(.Msg)
  ELSE  SET DLSUCCESS=1
  NEW TEMP SET TEMP=$GET(INFO("TEXT FILE"),$GET(INFO("KID FILE")))
  SET INFO("PATCH NAME")=$PIECE(TEMP,".",1)
  ;
  NEW % SET %=1
  IF $GET(INFO("KID FILE"))'="" DO PANALYZE^TMGPAT4(.INFO,.OPTION)
  IF $GET(INFO("TEXT FILE"))'="" DO  GOTO:(ABORT=1) DINSDN
  . DO ANALYZE^TMGPAT4(.INFO,.OPTION)
  . NEW TEMPMsg
  . DO ShowAnalysis^TMGPAT4(.INFO,.TEMPMsg)
  . IF $$SHOWMSG^TMGPAT2(.TEMPMsg,1)
  . KILL TEMPMsg
  . NEW FILEPATHNAME SET FILEPATHNAME=$GET(INFO("PATH"))_$GET(INFO("TEXT FILE"))
  . SET %=1
  . WRITE "View INFO FILE for patch" DO YN^DICN WRITE !
  . IF %=-1 QUIT
  . IF %=1 IF $$EditHFSFile^TMGKERNL(FILEPATHNAME)
  . NEW FNAME SET FNAME=$GET(INFO("KID FILE"))
  . IF FNAME="" DO
  . . SET DLSUCCESS=0  ;"start with failure to download
  . . IF $GET(INFO("MULTI-PATCH","FILENAME"))'="" DO
  . . . SET (FNAME,INFO("KID FILE"))=INFO("MULTI-PATCH","FILENAME")
  . . ELSE  IF ($$LISTCT^TMGMISC2($NAME(INFO("PATCH CATEGORY")))=1)&($DATA(INFO("PATCH CATEGORY","Informational"))) QUIT
  . . ELSE  DO
  . . . WRITE !,"No typical KIDS file found for this patch.",!
  . . . IF $DATA(INFO("MISC KID FILES")) DO 
  . . . . WRITE "However, ONE or more filenames were scraped from the TEXT file.  ",!
  . . . . WRITE "Having read the INFO TEXT file, see IF ONE of these names is correct.",!!
  . . . DO PICKFILE^TMGPAT1(.INFO,$NAME(INFO("MISC KID FILES")))        
  . . SET FNAME=$GET(INFO("KID FILE"))
  . . IF 1=0,FNAME="" DO
  . . . WRITE !,"No KIDS filename found for this patch.",!
  . . . WRITE "Having read the INFO TEXT file, would you like to ",!
  . . . WRITE "browse the patch server to pick the correct KIDS file"
  . . . SET %=2 DO YN^DICN WRITE !
  . . . IF %'=1 QUIT
  . . . NEW TEMP SET TEMP=$$URLPICK^TMGPAT1(.INFO)  ;"may alter INFO("KID URL")
  . . . IF +TEMP=-1 DO  SET DONE=1 QUIT
  . . . . WRITE !,"Problem: ",$PIECE(TEMP,"^",2),!
  . . . SET FNAME=$GET(INFO("KID FILE"))
  . . IF FNAME="" QUIT
  . . SET INFO("TEXT ONLY")=0
  . . SET %=2
  . . NEW TEXTURL SET TEXTURL=$GET(INFO("TEXT URL"))
  . . NEW SERVERPATH DO SPLITFPN^TMGIOUTL(TEXTURL,.SERVERPATH)
  . . IF SERVERPATH="" QUIT
  . . SET INFO("KID URL")=SERVERPATH_FNAME
  . . SET INFO("KID URL BASE PATH")=SERVERPATH
  . . NEW DESTPATH SET DESTPATH=$GET(INFO("PATH")) QUIT:DESTPATH=""
  . . NEW DONE SET DONE=0
  . . FOR  DO  QUIT:(DONE!DLSUCCESS)
  . . . SET FNAME=$GET(INFO("KID FILE"))
  . . . IF FNAME="" SET DONE=1 QUIT
  . . . NEW DESTFPNAME SET DESTFPNAME=DESTPATH_FNAME
  . . . NEW URL SET URL=$GET(INFO("KID URL"))
  . . . IF URL="" SET DONE=1 QUIT
  . . . WRITE !
  . . . WRITE " -------------------------------------------------------------",!
  . . . WRITE !,"== DOWNLOAD ===================================================",!
  . . . IF $$DownloadFile^TMGKERNL(URL,DESTPATH,1)
  . . . WRITE !,"===============================================================",!
  . . . WRITE " -------------------------------------------------------------",!        
  . . . IF $$ISFILE^TMGKERNL(DESTFPNAME)=1 SET DLSUCCESS=1 QUIT
  . . . WRITE !,"Automatic attempt to download of [",FNAME,"] seems to have FAILED.",!
  . . . SET %=1
  . . . WRITE "Manually browse web patch server to try to find file" DO YN^DICN WRITE !
  . . . IF %'=1 SET DONE=1 QUIT
  . . . NEW TEMP SET TEMP=$$URLPICK^TMGPAT1(.INFO)  ;"may alter INFO("KID URL")
  . . . IF +TEMP=-1 DO  SET DONE=1 QUIT
  . . . . WRITE !,"Problem: ",$PIECE(TEMP,"^",2),!
  . . . . DO PRESS2GO^TMGUSRI2
  IF %=-1 GOTO DINSDN
  ;
  IF $DATA(INFO("STILL NEEDED")) DO  GOTO:(ABORT=1) DINSDN
  . NEW PARRAY SET PARRAY=$NAME(INFO("STILL NEEDED"))
  . DO STORMSNG^TMGPAT3(PCKINIT,PARRAY)
  . WRITE "It seems that the system is not ready for this patch.",!
  . WRITE "(However, sometimes this can be ignored and ONE can proceed anyway.)",!
  . SET %=1
  . WRITE "Quit this patch and try another" DO YN^DICN WRITE !
  . IF %'=2 SET ABORT=1
  ;
  ;"IF $GET(INFO("TEXT ONLY"))=1 DO  GOTO DINSDN
  IF $GET(INFO("KID FILE"))="" DO  GOTO DINSDN
  . WRITE "This 'patch' doesn't have a corresponding KID file.",!
  . WRITE "Perhaps it was informational only.  I'm not smart enough to figure that out.",!
  . WRITE "If you didn't read the INFO FILE, then answer NO, and loop back and read it.",!
  . SET %=2
  . WRITE "Ready to consider the patch 'installed'" DO YN^DICN WRITE !
  . IF %'=1 QUIT
  . IF $$MakePATCHEntry^TMGPAT2(NEXTPATCH,.Msg)
  ; 
  ;"IF $GET(INFO("KID FILE"))="" DO  GOTO DINSDN
  ;". WRITE "?? No name for KID file ??",!
  ;
  NEW RESULT SET RESULT=0  ;"default to problem
  IF DLSUCCESS'=1 GOTO DINSDN
  SET %=1
  WRITE "Ready to load patch "_INFO("KID FILE")_" into system" DO YN^DICN WRITE !
  IF %'=1 SET %=-1 GOTO DINSDN
  SET RESULT=$$GO^TMGPAT1(.OPTION,.INFO,.Msg)
  ;
DINSDN  QUIT RESULT
  ;
