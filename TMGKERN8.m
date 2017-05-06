TMGKERN8 ;TMG/kst/Interface to allow use of linux editor in Fileman ;6/23/15
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
 ;"$$EDIT(Editor)
 ;"EDITARR(REF,EDITOR) 
 ;"LinuxEdit(Editor,FullPathName)  
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;
EDIT(Editor)
        ;"Purpose: This will be a shell for a linux editor
        ;"Input: Editor -- the name of the linux editor to use (i.e. vim, joe, pico etc)
        ;"              Allowed values: joe,vim,pico
        ;"Note: When this function gets called, VistA sets up some variables
        ;"      first to tell what should be edited etc.
        ;"      DIC=The global root of the WP field where the text to be edited is
        ;"              stored (or where NEW text should be stored)
        ;"              e.g. "^TMG(22702,27,DV,"
        ;"      (DV is also predefined, so reference to DV in DIC is covered.)
        ;"      There are other variables SET up re margins etc.  I will be ignoring these.
        ;"Results: none

        NEW result SET result=0
        NEW GlobalP

        ;"By limiting value to certain values, it prevents a rouge user from putting a wedged
        ;"linux command into "Editor" and executing a system command through zsystem.
        SET Editor=$GET(Editor,"rvim")
        IF (Editor'="rvim")&(Editor'="joe")&(Editor'="pico")&(Editor'="nano") GOTO EditAbort

        ;"Only allow users with programmer access to use joe
        IF (Editor="joe") DO  IF result=1 GOTO EditAbort
        . NEW AccessCode SET AccessCode=$GET(DUZ(0))
        . IF AccessCode'["@" do
        . . WRITE !!,"*** Sorry.  Insufficient security clearance to use insecure 'joe' editor. ***",!
        . . WRITE "Fileman access code of @ required, because JOE can shell out to linux prompt.",!!
        . . WRITE "Please enter 'User's Toolbox', then 'Edit User Characteristics' at a menu",!
        . . WRITE " option to change PREFERRED EDITOR to something other than JOE.",!!
        . . SET result=1

        NEW EditErrFile SET EditErrFile="/tmp/trashjoeoutput.txt"

        ;"set GlobalP=$EXTRACT(DIC,1,$LENGTH(DIC)-1)_")"  ;"convert to closed form
        SET GlobalP=$$CREF^DILF(DIC)  ;"convert to closed form
        NEW Filename SET Filename=$$UNIQUE^%ZISUTL("/tmp/vistaedit.tmp")
        SET result=$$WP2HFSFP^TMGIOUT3(GlobalP,Filename)
        IF result=0 GOTO EditDone


        NEW HookCmd
        SET HookCmd=Editor_" "_Filename_" 2>"_EditErrFile  ;"use NULL instead??
        zsystem HookCmd
        SET result=$ZSYSTEM&255  ;"get result of execution. (low byte only). 0=success
        IF result>0 GOTO EditDone

        ;"read file back into global WP
        SET result=$$HFS2WPFP^TMGIOUT3(Filename,GlobalP)
        ;"if result=1 do

EditDone
        NEW temp SET temp=$$DELFILE^TMGIOUTL(Filename)
        SET temp=$$DELFILE^TMGIOUTL(Filename_"~")  ;"joe editor copies output to filename~ as a backup
        SET temp=$$DELFILE^TMGIOUTL(EditErrFile)
EditAbort
        QUIT

EDITARR(REF,EDITOR) ;
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
        DO ARRAY2WP^TMGSTUTL(REF,"TMGEWP")
        NEW DIC SET DIC="TMGEWP"
        DO EDIT(.EDITOR)
        DO WP2ARRAY^TMGSTUTL("TMGEWP",REF)
EADN    QUIT

LinuxEdit(Editor,FullPathName)
        ;"Purpose: This will be a shell for a linux editor
        ;"Input: Editor -- the name of the linux editor to use (i.e. vim, joe, pico etc)
        ;"              Allowed values: joe,vim,pico
        ;"         FullPathName -- the path name on the Linux HFS to edit.
        ;"Results: none

        NEW result SET result=0
        NEW GlobalP

        ;"By limiting value to certain values, it prevents a rouge user from putting a wedged
        ;"linux command into "Editor" and executing a system command through zsystem.
        SET Editor=$GET(Editor,"rvim")
        IF (Editor'="rvim")&(Editor'="joe")&(Editor'="pico") GOTO LEditAbort

        ;"Only allow users with programmer access to use joe
        IF (Editor="joe") DO  IF result=1 GOTO EditAbort
        . NEW AccessCode
        . SET AccessCode=$PIECE(^VA(200,DUZ,0),"^",4)
        . IF AccessCode'="@" do
        . . WRITE !!,"*** Sorry.  Insufficient security clearance to use insecure 'joe' editor. ***",!
        . . WRITE "Please enter 'User's Toolbox', then 'Edit User Characteristics' at a menu",!
        . . WRITE " option to change PREFERRED EDITOR to something other than JOE.",!!
        . . SET result=1

        ;"new EditErrFile SET EditErrFile="/tmp/trashjoeoutput.txt"

        NEW Filename SET Filename=FullPathName

        NEW HookCmd
        ;"set HookCmd=Editor_" "_Filename_" 2>"_EditErrFile  ;"use NULL instead??
        SET HookCmd=Editor_" "_Filename
        zsystem HookCmd
        SET result=$ZSYSTEM&255  ;"get result of execution. (low byte only). 0=success
        IF result>0 GOTO LEditDone


LEditDone
        ;"set temp=$$DELFILE^TMGIOUTL(Filename_"~")  ;"joe editor copies output to filename~ as a backup
        ;"set temp=$$DELFILE^TMGIOUTL(EditErrFile)
LEditAbort
        QUIT


