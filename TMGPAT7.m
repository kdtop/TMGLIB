TMGPAT7 ;TMG/kst/Patching tools ;8/21/22
         ;;1.0;TMG-LIB;**1**;8/21/22
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 8/21/22  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"DIFF2RTNS(KIDSARR,LOCALARR,NAME)  --Show Diff between 2 routines
 ;"PICKDIFF -- Allow user to pick between routines which have backups, then diff between them.  
 ;
 ;"=======================================================================
 ;"Private Functions
 ;"=======================================================================
 ;"DIFF2OLDRTN(ROUTINE)  --Run VimDiff on a routine vs an older backup file.  
 ;"GETBAKFILES(DIR,ROUTINE,FILES)  --Get listing of backup files.
 ;"ISBAK(AFILE) --return if file name is consistent with backup up filename
 ;"GETDIR(ROUTINE) --Return routine directory for routine
 ;"GETPDIR(ROUTINE) --Return routine directory for routine, FROM PATCH DIRECTORY
 ;"GETINSTRUCT(OUT,ISCUR,TOP) --Fill OUT with instructions for VIMDIFF
 ;"FILTERPDIFF(NAME,IEN,OPTION) --Call back function for PICKDIFF.
 ;"TESTDIFF 
 ; 
 ;"=======================================================================
 ;
DIFF2RTNS(KIDSARR,LOCALARR,NAME)  ;"Show Diff between 2 routines
  NEW TEMP1 MERGE TEMP1=KIDSARR(NAME)
  NEW TEMP2 MERGE TEMP2=LOCALARR(NAME)
  NEW LINE SET LINE="========================================="
  NEW ESC SET ESC="To exit, type <ESC>:qa<ENTER>"
  NEW JMP SET JMP="<Ctrl>W then <LEFT> or <RIGHT> arrows to swap window sides"
  NEW NOEDT SET NOEDT="Don't edit files.  Any changes will be DISCARDED."
  NEW L1 SET L1=LINE_"^ROUTINE: "_NAME_" from KIDS Patch^"_ESC_"^"_JMP_"^"_NOEDT_"^"_LINE
  NEW L2 SET L2=LINE_"^ROUTINE: "_NAME_" from LOCAL VISTA^"_ESC_"^"_JMP_"^"_NOEDT_"^"_LINE
  DO PREFIXLINE2ARR^TMGSTUT2(.TEMP1,L1) 
  DO PREFIXLINE2ARR^TMGSTUT2(.TEMP2,L2) 
  DO VIMADIFF^TMGKERNL(.TEMP1,.TEMP2)
  QUIT
  ; 
DIFF2OLDRTN(ROUTINE,DIFF)  ;"Run VimDiff on a routine vs an older backup file.  
  ;"Input: ROUTINE -- mumps name of routine, e.g. TMGTEST
  ;"       DIR -- OPTIONAL.  If provided, then used for routines.  If not provided, will try to determine.  
  ;"RESULT: 1^OK, or -1^ErrMessage if problem. 
  NEW MENU,IDX,USRPICK,OLDRTN
  NEW RESULT 
  SET ROUTINE=$GET(ROUTINE) IF ROUTINE="" DO  GOTO D2ODN
  . SET RESULT="-1^No routine provided"
  ;"Parse $ZRO and find file path name of routine.  Get FPNAME, with extension
  ;"  Alternatively, just allow user to browse to name of routine file... $$FBROWSE^TMGIOUT2()
  IF $GET(DIR)="" SET DIR=$$GETDIR(ROUTINE) IF DIR="" DO  GOTO D2ODN
  . SET RESULT="-1^Unable to determine HFS directory for ["_ROUTINE_"]"
  NEW CURFNAME SET CURFNAME=ROUTINE_".m"
  NEW CURFPNAME SET CURFPNAME=DIR_CURFNAME
  IF $$ISFILE^TMGKERNL(CURFPNAME)=0 DO  GOTO D2ODN
  . SET RESULT="-1^Unable to locate file: ["_CURFPNAME_"]"
  ;"Load current routine into array, and keep backup array to compare against after VIMDIFF
  NEW CURARR IF $$HFS2ARR^TMGIOUT3(DIR,CURFNAME,"CURARR")=0 DO  GOTO D2ODN
  . SET RESULT="-1^Error loading ["_DIR_CURFNAME_"]"
  NEW CURARRSAVE MERGE CURARRSAVE=CURARR  ;"save for comparison later 
  ;"Search directory for backup files, as <ROUTINE>-m_#.bak -- $$LIST^%ZISH(): List Directory
  NEW FILTER SET FILTER(ROUTINE_"*")=""           
  NEW TEMPFILES IF $$LIST^%ZISH(DIR,"FILTER","TEMPFILES")=0 DO  GOTO D2ODN
  . SET RESULT="-1^Error listing files in ["_DIR_"]"
  NEW FILES SET RESULT=$$GETBAKFILES(DIR,ROUTINE,.FILES)  ;"Get listing of backup files.
  IF RESULT'>0 GOTO D2ODN
  ;"Allow user to pick which backup file to compare to, i.e. if more than one prior backup. 
  SET IDX=0 KILL MENU,ADDED SET MENU(IDX)="COMPARE ["_ROUTINE_"] vs which backup?"
  SET AFILE="" FOR  SET AFILE=$ORDER(FILES(AFILE)) QUIT:AFILE=""  DO
  . SET IDX=IDX+1,MENU(IDX)=AFILE_$CHAR(9)_AFILE
  ;"=======================================
  IF IDX=1 SET OLDRTN=$PIECE(MENU(1),$CHAR(9),2) GOTO D2O2  ;"If only 1 option, don't ask user
  SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
  IF USRPICK="^" GOTO D2ODN      
  SET OLDRTN=USRPICK
D2O2  
  NEW OLDFPNAME SET OLDFPNAME=DIR_OLDRTN
  ;"Load old version of routine into array
  NEW OLDARR IF $$HFS2ARR^TMGIOUT3(DIR,OLDRTN,"OLDARR")=0 DO  QUIT
  . SET RESULT="-1^Error loading ["_DIR_OLDRTN_"]"
  ;"Prefix help instructions, via PREFIXARR() to top of both arrays, with lines starting with #
  NEW INSTRUCT1 DO GETINSTRUCT(.INSTRUCT1,1,1) ;"top instructions for CURR file   
  DO PREFIXARR^TMGSTUT2(.CURARRSAVE,.INSTRUCT1)  ;"This renumbers the lines. Must do to allow comparing to CURARR later  
  DO PREFIXARR^TMGSTUT2(.CURARR,.INSTRUCT1)  
  KILL INSTRUCT1 DO GETINSTRUCT(.INSTRUCT1,0,1) ;"top instructions for OLD file   
  DO PREFIXARR^TMGSTUT2(.OLDARR,.INSTRUCT1)  
  KILL INSTRUCT1 DO GETINSTRUCT(.INSTRUCT1,1,0) ;"bottom instructions for CURR file   
  DO APPENDARR^TMGSTUT2(.CURARR,.INSTRUCT1)
  KILL INSTRUCT1 DO GETINSTRUCT(.INSTRUCT1,0,0) ;"bottom instructions for OLD file   
  DO APPENDARR^TMGSTUT2(.OLDARR,.INSTRUCT1)  
  ;"call VIMADIFF^TMGKERNL <-- alter this routine so that changes are sent back in passed arrays
  SET RESULT=$$VIMADIFF^TMGKERNL(.OLDARR,.CURARR) 
  IF RESULT'>0 GOTO D2ODN  
  ;"After return, remove prefix instructions 
  DO DELINSTRUCT(.CURARRSAVE)  ;"Delete top and bottom instructions, marked by line starting with "#"
  DO DELINSTRUCT(.CURARR)  ;"Delete top and bottom instructions, marked by line starting with "#"
  DO DELINSTRUCT(.OLDARR)  ;"Delete top and bottom instructions, marked by line starting with "#"
  ;"Compare to returned backup array
  ;"Check if new version of routine has changed, 
  NEW SAME SET SAME=$$COMPARRAY^TMGMISC("CURARR","CURARRSAVE")
  ;"If changed, ask user if they want to save changes
  IF SAME=1 DO  GOTO D2ODN
  . WRITE !,"No edits made, so nothing to save.",!
  . DO PRESS2GO^TMGUSRI2
  WRITE ! NEW % SET %=2
  WRITE !,"Save and ZLINK modified routine [",ROUTINE,"]" DO YN^DICN WRITE !
  IF %'=1 GOTO D2ODN
  ;"Backup file holding original version 
  NEW SAVEDFPNAME DO MAKEBAKF^TMGKERNL(CURFPNAME,"/",.SAVEDFPNAME)
  IF SAVEDFPNAME="" DO  GOTO D2ODN
  . SET RESULT="-1^Unable to backup original file on HFS ["_CURFPNAME_"]"
  ;"then save modified routine, 
  SET TMGRESULT=$$ARR2HFS^TMGIOUT3("CURARR",DIR,CURFNAME) IF TMGRESULT'>0 GOTO D2ODN
  ;"And finally, zlink newly saved file.
  NEW ZS ZCOMPILE CURFPNAME SET ZS=$ZSTATUS  
  IF $ZCSTATUS>1 DO    
  . WRITE !,"-------------------------------------------------------------",!
  . WRITE "NOTE:  Compiling [",ROUTINE,"] generated error(s).  See above.",!
  . WRITE "ERROR --> $ZSTATUS=[",ZS,"  $ZCSTATUS=",$ZCSTATUS,!
  . WRITE "Please review situation, and edit and correct errors if needed.",!
  . DO PRESS2GO^TMGUSRI2
  ZLINK ROUTINE
  SET RESULT="1^OK"
D2ODN ;  
  QUIT RESULT
  ;
GETBAKFILES(DIR,ROUTINE,FILES)  ;"Get listing of backup files.
  ;"Input: DIR -- The directory to get listing from
  ;"       RTN -- OPTIONAL.  If provided, then only ROUTINE* files will be returned
  ;"       FILES -- PASS BY REFERENCE, AN OUT PARAMETER
  ;"Results: 1^OK, or -1^Error if problem. 
  ;"Search directory for backup files, as <ROUTINE>-m_#.bak -- $$LIST^%ZISH(): List Directory
  NEW RESULT SET RESULT="1^OK"
  SET ROUTINE=$GET(ROUTINE)
  NEW FILTER SET FILTER(ROUTINE_"*")=""           
  NEW TEMPFILES IF $$LIST^%ZISH(DIR,"FILTER","TEMPFILES")=0 DO  GOTO GBKFDN
  . SET RESULT="-1^Error listing files in ["_DIR_"]"
  NEW AFILE SET AFILE=""                      
  FOR  SET AFILE=$ORDER(TEMPFILES(AFILE)) QUIT:AFILE=""  DO
  . IF $$ISBAK(AFILE) SET FILES(AFILE)=""
GBKFDN ;
  QUIT RESULT
  ;  
ISBAK(AFILE) ;"return if file name is consistent with backup up filename
  NEW RESULT SET RESULT=0
  IF (AFILE["-m_")&(AFILE[".bak") SET RESULT=1
  IF (AFILE[".bak~") SET RESULT=0
  QUIT RESULT
  
GETDIR(ROUTINE) ;"Return routine directory for routine
  ;"NOTE: Later I could parse $ZRO and find file path name of routine.  Get FPNAME, with extension
  NEW RESULT SET RESULT="/opt/worldvista/EHR/r/"  ;"<--- hardcode for now. 
  QUIT RESULT
  ;
GETPDIR(ROUTINE) ;"Return routine directory for routine, FROM PATCH DIRECTORY
  ;"NOTE: Later I could parse $ZRO and find file path name of routine.  Get FPNAME, with extension
  NEW RESULT SET RESULT="/opt/worldvista/EHR/p/"  ;"<--- hardcode for now. 
  QUIT RESULT
  ;
GETINSTRUCT(OUT,ISCUR,TOP)  ;"Fill OUT with instructions for VIMDIFF
  ;"Input: OUT -- filled with results.  PASS BY REFERENCE, AN OUT PARAMETER
  ;"       ISCUR -- 1 if CURRENT file, or 0 if OLD file.  
  ;"       TOP -- if 1, then top instructions returned, if 0, then bottom instructions returned
  ;"Results: none  
  KILL OUT
  NEW CT SET CT=1
  IF $GET(TOP) DO
  .  SET OUT(CT)="#================================================",CT=CT+1
  .  SET OUT(CT)="# To exit, type <ESC>:qa<ENTER>",CT=CT+1
  .  SET OUT(CT)="#",CT=CT+1
  . IF ISCUR DO
  .. SET OUT(CT)="#  This is the CURRENT version of the file",CT=CT+1
  .. SET OUT(CT)="#  Edits and vim saves to this file will be KEPT.",CT=CT+1
  . ELSE  DO
  .. SET OUT(CT)="#  This is the OLD, BACKUP version of the file",CT=CT+1
  .. SET OUT(CT)="#  Edits and vim saves to this file will be DISCARDED.",CT=CT+1
  .  SET OUT(CT)="#",CT=CT+1
  .  SET OUT(CT)="#  More detailed navigation instructons at",CT=CT+1
  .  SET OUT(CT)="#  the end of this file",CT=CT+1
  .  SET OUT(CT)="#================================================",CT=CT+1
  ELSE  DO
  .  SET OUT(CT)="#================================================",CT=CT+1
  . IF ISCUR DO
  .. SET OUT(CT)="#  This is the CURRENT version of the file",CT=CT+1
  .. SET OUT(CT)="#  Edits to this file will be KEPT.",CT=CT+1
  . ELSE  DO
  .. SET OUT(CT)="#  This is the OLD, BACKUP version of the file",CT=CT+1
  .. SET OUT(CT)="#  Edits to this file will be DISCARDED.",CT=CT+1
  .  SET OUT(CT)="#",CT=CT+1
  .  SET OUT(CT)="#",CT=CT+1
  .  SET OUT(CT)="#  **CHANGING MODES",CT=CT+1
  .  SET OUT(CT)="#     i        -> Enter INSERT mode",CT=CT+1
  .  SET OUT(CT)="#     v        -> Enter VISUAL mode",CT=CT+1
  .  SET OUT(CT)="#     V        -> Enter VISUAL LINE mode",CT=CT+1
  .  SET OUT(CT)="#     CTRL+V   -> Enter INSERT BLOCK mode",CT=CT+1
  .  SET OUT(CT)="#     esc      -> Enter NORMAL mode",CT=CT+1
  .  SET OUT(CT)="#     ",CT=CT+1
  .  SET OUT(CT)="#  **NORMAL MODE",CT=CT+1
  .  SET OUT(CT)="#     /        -> Search for text",CT=CT+1
  .  SET OUT(CT)="#     u        -> undo",CT=CT+1
  .  SET OUT(CT)="#     yy       -> yank (copy) current line",CT=CT+1
  .  SET OUT(CT)="#     p        -> paste to next line",CT=CT+1
  .  SET OUT(CT)="#     P        -> paste above current line",CT=CT+1
  .  SET OUT(CT)="#     d        -> delete",CT=CT+1
  .  SET OUT(CT)="#     ggVG     -> Select all text",CT=CT+1
  .  SET OUT(CT)="#     :        -> Enter commands",CT=CT+1
  .  SET OUT(CT)="#     w        -> Save file",CT=CT+1
  .  SET OUT(CT)="#     q        -> Close current window",CT=CT+1
  .  SET OUT(CT)="#     qa       -> Close all windows",CT=CT+1
  .  SET OUT(CT)="#     ",CT=CT+1
  .  SET OUT(CT)="#  **VISUAL MODE",CT=CT+1
  .  SET OUT(CT)="#     w        -> Highlight beginning of next word",CT=CT+1
  .  SET OUT(CT)="#     $        -> Include remainder of line",CT=CT+1
  .  SET OUT(CT)="#     d        -> Delete (Cut)",CT=CT+1
  .  SET OUT(CT)="#     c        -> Change highlighted text",CT=CT+1
  .  SET OUT(CT)="#     y        -> Yank (copy)",CT=CT+1
  .  SET OUT(CT)="#     >        -> Increase indentation",CT=CT+1
  .  SET OUT(CT)="#     <        -> Decrease indentation",CT=CT+1
  .  SET OUT(CT)="#     ",CT=CT+1
  .  SET OUT(CT)="#  **WINDOWED MODE",CT=CT+1
  .  SET OUT(CT)="#     CTRL+w,w -> Next window",CT=CT+1
  .  SET OUT(CT)="#     CTRL+w,= -> Make all windows equal size",CT=CT+1
  .  SET OUT(CT)="#     do       -> Get changes from other window to current",CT=CT+1
  .  SET OUT(CT)="#     dp       -> Put changes from current window to other",CT=CT+1
  .  SET OUT(CT)="#     ",CT=CT+1
  .  SET OUT(CT)="#  **OTHER COMMANDS",CT=CT+1
  .  SET OUT(CT)="#     :set mouse=a -> Toggle mouse",CT=CT+1
  .  SET OUT(CT)="#================================================",CT=CT+1
  QUIT
  ;
DELINSTRUCT(ARR)  ;"Delete top and bottom instructions, marked by line starting with "#"
  NEW DONE SET DONE=0 
  NEW IDX SET IDX=""
  ;"Cycle through top of array until line without starting "#" found
  FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:(IDX="")!(DONE)  DO
  . IF $EXTRACT($GET(ARR(IDX)),1)'="#" SET DONE=1 QUIT
  . KILL ARR(IDX)
  ;"Cycle backwards from bottom of array until line without starting "#" found
  SET DONE=0
  SET IDX=$ORDER(ARR(""),-1)+1
  FOR  SET IDX=$ORDER(ARR(IDX),-1) QUIT:(IDX="")!(DONE)  DO
  . IF $EXTRACT($GET(ARR(IDX)),1)'="#" SET DONE=1 QUIT
  . KILL ARR(IDX)                    
  QUIT  
  ;
PICKDIFF ;"Allow user to pick between routines which have backups, then diff between them.
  NEW OPTION SET OPTION("FILTER CODE")="$$FILTERPDIFF^TMGPAT7(NAME,IEN,.OPTION)"
  NEW USRPICK SET USRPICK=$$RECSEL^TMGUSRI4(9.8,,.OPTION)
  IF USRPICK'>0 QUIT
  NEW RTN SET RTN=$PIECE(USRPICK,"^",2) QUIT:RTN=""
  NEW RESULT SET RESULT=$$DIFF2OLDRTN(RTN)  ;"Run VimDiff on a routine vs an older backup file.
  IF RESULT'>0 WRITE !,RESULT,! DO PRESS2GO^TMGUSRI2
  QUIT
  ;
FILTERPDIFF(NAME,IEN,OPTION) ;"Call back function for PICKDIFF.
  ;"RESULTS: 1 if element should be filtered, (not shown)
  IF $DATA(OPTION("RAW RECSEL FILTER"))=0 DO
  . NEW DIR SET DIR=$$GETDIR(NAME)  ;"only first NAME used.  At time of writing, this returns hard-coded dir
  . SET TEMP=$$GETBAKFILES(DIR,,.FILES)  ;"Get listing of backup files.
  . MERGE OPTION("RAW RECSEL FILTER")=FILES
  . ;" SET DIR=$$GETPDIR(NAME)  
  . ;" SET TEMP=$$GETBAKFILES(DIR,,.FILES)  ;"Get listing of backup files.
  . ;" MERGE OPTION("RAW RECSEL FILTER")=FILES
  NEW NEXT SET NEXT=$ORDER(OPTION("RAW RECSEL FILTER",NAME))
  SET NEXT=$PIECE(NEXT,"-",1)
  NEW RESULT SET RESULT=(NEXT'=NAME)
  QUIT RESULT
  ;
TESTDIFF 
  NEW RESULT SET RESULT=$$DIFF2OLDRTN("TMGTEST2",$$GETPDIR())  ;"Run VimDiff on a routine vs an older backup file.
  IF RESULT'>0 WRITE !,RESULT,! DO PRESS2GO^TMGUSRI2
  QUIT
