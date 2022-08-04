TMGKERN8 ;TMG/kst/Interface to allow use of linux editor in Fileman ;6/23/15, 8/3/22
         ;;1.0;TMG-LIB;**1**;6/23/15
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"TMG EDITOR FUNCTIONS
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"$$EDIT(EDITOR)
 ;"EDITARR(REF,EDITOR) ;--DEPRECIATED, use EDITARR2
 ;"EDITARR2(REF,EDITOR) -- use linux editor to edit an array.
 ;"LINUXEDIT(EDITOR,FULLPATHNAME)  
 ;"PRFXFILE(FPNAME,ARR)  -- PREFIX file with text
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;
EDIT(EDITOR) ;
  ;"Purpose: This will be a shell for a linux editor
  ;"Input: EDITOR -- the name of the linux editor to use (i.e. vim, joe, pico etc)
  ;"              Allowed values: joe,vim,pico,nano
  ;"Note: When this function gets called, VistA sets up some variables
  ;"      first to tell what should be edited etc.
  ;"      DIC=The global root of the WP field where the text to be edited is
  ;"              stored (or where NEW text should be stored)
  ;"              e.g. "^TMG(22702,27,DV,"
  ;"      (DV is also predefined, so reference to DV in DIC is covered.)
  ;"      There are other variables SET up re margins etc.  I will be ignoring these.
  ;"Results: none
  ;
  NEW RESULT SET RESULT=0
  NEW GLOBALP
  ;
  ;"By limiting value to certain values, it prevents a rouge user from putting a wedged
  ;"linux command into "EDITOR" and executing a system command through ZSYSTEM.
  SET EDITOR=$GET(EDITOR,"rvim")
  IF $$GOODEDITOR(EDITOR)=0 GOTO EditAbort
  ;"Only allow users with programmer access to use joe
  IF (EDITOR="joe") DO  IF RESULT=1 GOTO EditAbort
  . NEW ACCESSCODE SET ACCESSCODE=$GET(DUZ(0))
  . IF ACCESSCODE'["@" do
  . . WRITE !!,"*** Sorry.  Insufficient security clearance to use insecure 'joe' editor. ***",!
  . . WRITE "Fileman access code of @ required, because JOE can shell out to linux prompt.",!!
  . . WRITE "Please enter 'User's Toolbox', then 'Edit User Characteristics' at a menu",!
  . . WRITE " option to change PREFERRED EDITOR to something other than JOE.",!!
  . . SET RESULT=1
  ;
  NEW EDITERRFILE SET EDITERRFILE="/tmp/trashjoeoutput.txt"
  ;
  ;"set GLOBALP=$EXTRACT(DIC,1,$LENGTH(DIC)-1)_")"  ;"convert to closed form
  SET GLOBALP=$$CREF^DILF(DIC)  ;"convert to closed form
  NEW FILENAME SET FILENAME=$$UNIQUE^%ZISUTL("/tmp/vistaedit.tmp")
  SET RESULT=$$WP2HFSFP^TMGIOUT3(GLOBALP,FILENAME)
  IF RESULT=0 GOTO EditDone
  ;
  NEW HOOKCMD SET HOOKCMD=EDITOR_" "_FILENAME_" 2>"_EDITERRFILE  ;"use NULL instead??
  ZSYSTEM HOOKCMD
  SET RESULT=$ZSYSTEM&255  ;"get result of execution. (low byte only). 0=success
  IF RESULT>0 GOTO EditDone
  ;
  ;"read file back into global WP
  SET RESULT=$$HFS2WPFP^TMGIOUT3(FILENAME,GLOBALP)
  ;"if RESULT=1 do
  ;
EditDone ;
  NEW temp SET temp=$$DELFILE^TMGIOUTL(FILENAME)
  SET temp=$$DELFILE^TMGIOUTL(FILENAME_"~")  ;"joe editor copies output to filename~ as a backup
  SET temp=$$DELFILE^TMGIOUTL(EDITERRFILE)
EditAbort ;
  QUIT
  ;
EDITARR(REF,EDITOR) ;"--DEPRECIATED, use EDITARR2
  ;"Purpose: to use linux editor to edit an array.
  ;"Input: REF -- an reference (name of) to array to edit.  E.g. "ARRAY"
  ;"          ARRAY(1) -- 1st line
  ;"          ARRAY(1,2) -- 2nd line  <-- sub-nodes OK
  ;"          ARRAY(2) -- 3rd line ... etc.
  ;"          NOTE: that the array is 'flattened' into top-level indices
  ;"       EDITOR -- Optional.  Default is "vim"
  ;"              Allowed values: joe,vim,pico
  ;"Output: @REF is edited, and line numbers are reformated (flattening any sub nodes)
  NEW TMGEWP
  SET REF=$GET(REF)
  IF REF="" GOTO EADN
  DO ARRAY2WP^TMGSTUT2(REF,"TMGEWP")
  NEW DIC SET DIC="TMGEWP"
  DO EDIT(.EDITOR)
  DO WP2ARRAY^TMGSTUT2("TMGEWP",REF)
EADN ;
  QUIT
  ;
EDITARR2(REF,EDITOR,PREFIX) ;
  ;"Purpose: to use linux editor to edit an array.
  ;"NOTE: For security reasons, this will not work with Globals, only memory arrays
  ;"Input: REF -- an reference (name of) to array to edit.  E.g. "ARRAY"
  ;"          ARRAY(1) -- 1st line
  ;"          ARRAY(1,2) -- 2nd line  <-- sub-nodes OK
  ;"          ARRAY(2) -- 3rd line ... etc.
  ;"       EDITOR -- Optional.  Default is "vim"
  ;"              Allowed values: joe,vim,pico,nano
  ;"       PREFIX -- OPTIONAL. If not provided, generic instructions will be given  
  ;"           This can be a single line or an array
  ;"           of text to prefix to the edit file.  If each line
  ;"           starts with '#', then it will not be included in the 
  ;"           output back into the REF variable.  E.g.   
  ;"           PREFIX("#Instructions: Edit text below")
  ;"Output: @REF is edited
  SET REF=$GET(REF) IF REF="" GOTO EA2DN
  IF $DATA(PREFIX)=0 DO
  . SET PREFIX(1)="# Lines beginning with '#' are ignored."
  . SET PREFIX(2)="# Edit lines below.  This does not work with global variables." 
  . SET PREFIX(3)="# When done editing, exit with saving changes. "
  . SET PREFIX(4)=" " 
  NEW TMGEARRTEMP DO ZWR2ARR^TMGZWR(REF,"TMGEARRTEMP")
  NEW TMGEWP
  NEW CT SET CT=0
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(PREFIX(IDX)) QUIT:IDX'>0  DO
  . SET CT=CT+1,TMGEWP(CT)=$GET(PREFIX(IDX))        
  SET IDX=0
  FOR  SET IDX=$ORDER(TMGEARRTEMP(IDX)) QUIT:IDX'>0  DO
  . SET CT=CT+1,TMGEWP(CT)=$GET(TMGEARRTEMP(IDX))
  NEW FPNAME SET FPNAME=$$UNIQUE^%ZISUTL("/tmp/vista_arr_edit.tmp")        
  IF $$AR2HFSFP^TMGIOUT3("TMGEWP",FPNAME)=0 GOTO EA2DN ; "Array to HFS via FilePath     
  DO LINUXEDIT(EDITOR,FPNAME)        
  NEW OPTION SET OPTION("OVERFLOW")=1
  KILL TMGEWP DO HFS2ARFP^TMGIOUT3(FPNAME,"TMGEWP",.OPTION)
  KILL @REF
  NEW IDX SET IDX=""
  FOR  SET IDX=$ORDER(TMGEWP(IDX)) QUIT:IDX=""  DO
  . NEW LINE SET LINE=$$TRIM^XLFSTR($GET(TMGEWP(IDX))) QUIT:LINE'["="
  . IF $EXTRACT(LINE,1)="#" QUIT
  . NEW PARTA SET PARTA=$PIECE(LINE,"=",1) QUIT:PARTA["^"
  . SET PARTA=REF_$SELECT(PARTA["(":"("_$PIECE(PARTA,"(",2,999),1:"")
  . NEW PARTB SET PARTB=$PIECE(LINE,"=",2,99)
  . SET PARTB=$$UNQTPROT^TMGSTUT3(PARTB) ;"convert all double quotes to single quotes
  . DO
  . . NEW $ETRAP SET $ETRAP="write ""(Invalid M Code!.  Error Trapped.)"",! set $etrap="""",$ecode="""""
  . . SET @PARTA=PARTB                
  IF $$DELFILE^TMGIOUTL(FPNAME)
  IF $$DELFILE^TMGIOUTL(FPNAME_"~")  ;"joe editor copies output to filename~ as a backup
EA2DN ;
  QUIT
  ;
TESTEARR()  ;
  NEW TMGZZ
  SET TMGZZ="Test"
  SET TMGZZ(1)=1
  SET TMGZZ(1,"ANIMAL")="COW"
  SET TMGZZ("TREE",1)="APPLE TREE"
  SET TMGZZ("TREE",2)="PEAR TREE"
  DO EDITARR2("TMGZZ","pico")
  DO ZWRITE^TMGZWR("TMGZZ")
  QUIT
  ;
LINUXEDIT(EDITOR,FULLPATHNAME)   ;
  ;"Purpose: This will be a shell for a linux editor
  ;"Input: EDITOR -- the name of the linux editor to use (i.e. vim, joe, pico etc)
  ;"              Allowed values: joe,vim,pico
  ;"         FULLPATHNAME -- the path name on the Linux HFS to edit.
  ;"Results: none
  NEW GLOBALP,RESULT SET RESULT=0
  ;"Limiting value to certain values, to preventrouge user from putting a wedged
  ;"linux command into "EDITOR" and executing a system command through ZSYSTEM.
  SET EDITOR=$GET(EDITOR,"rvim")
  IF $$GOODEDITOR(EDITOR)=0 GOTO LEditAbort
  IF (EDITOR="joe") DO  IF RESULT=1 GOTO LEditAbort    ;"Only allow users with programmer access to use joe
  . NEW ACCESSCODE SET ACCESSCODE=$PIECE(^VA(200,DUZ,0),"^",4)
  . IF ACCESSCODE'="@" do
  . . WRITE !!,"*** Sorry.  Insufficient security clearance to use insecure 'joe' editor. ***",!
  . . WRITE "Please enter 'User's Toolbox', then 'Edit User Characteristics' at a menu",!
  . . WRITE " option to change PREFERRED EDITOR to something other than JOE.",!!
  . . SET RESULT=1
  ;
  NEW HOOKCMD SET HOOKCMD=EDITOR_" "_FULLPATHNAME
  ZSYSTEM HOOKCMD
  SET RESULT=$ZSYSTEM&255  ;"get result of execution. (low byte only). 0=success
LEditAbort ;
  QUIT
  ;
GOODEDITOR(EDITOR)  ;"Determine if editor name is in list of allows.
  NEW RESULT SET RESULT=1 ;"default to successw
  IF (EDITOR="rvim") GOTO GEDN
  IF (EDITOR="joe") GOTO GEDN
  IF (EDITOR="pico") GOTO GEDN
  IF (EDITOR="nano") GOTO GEDN
  SET RESULT=0
GEDN ;
  QUIT RESULT

PRFXFILE(FPNAME,ARR)  ;"PREFIX file with text
  ;"NOTE: This doesn't seem to be working.  I got it from here:
  ;" https://superuser.com/questions/246837/how-do-i-add-text-to-the-beginning-of-a-file-in-bash
  ;"But I have decided to solve my problem another way, so I will leave this here
  ;"  in case I want to work on later.  
  ;"Input: FPNAME -- the full path and file name of linux host file
  ;"       ARR -- 2 METHODS OF USE:  
  ;"          ARR=<desired text)  <-- just one line
  ;"       or
  ;"          ARR(1)=<LINE1>
  ;"          ARR(2)=<LINE2>)
  ;"          ...
  ;"Result: 1 if OK, or 0 if Linux error.  
  IF $GET(ARR)'="" DO
  . DO PRFX1FL(FPNAME,ARR)  ;"PREFIX file with 1 line of text
  ELSE  DO
  . NEW IDX SET IDX=""
  . FOR  SET IDX=$ORDER(ARR(IDX),-1) QUIT:IDX'>0  DO
  . . NEW LINE SET LINE=$GET(ARR(IDX))
  . . DO PRFX1FL(FPNAME,LINE)  ;"PREFIX file with 1 line of text
  QUIT
  ;
PRFX1FL(FPNAME,LINE)  ;"PREFIX file with 1 line of text
  ;"NOTE: This doesn't seem to be working.  I got it from here:
  ;" https://superuser.com/questions/246837/how-do-i-add-text-to-the-beginning-of-a-file-in-bash
  ;"But I have decided to solve my problem another way, so I will leave this here
  ;"  in case I want to work on later.  
  NEW HOOKCMD SET HOOKCMD="sed -i '1i"_LINE_"' "_FPNAME
  ZSYSTEM HOOKCMD
  NEW RESULT SET RESULT=($ZSYSTEM&255)=0  ;"get result of execution. (low byte only). 0=success
  QUIT RESULT
  ;