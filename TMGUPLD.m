TMGUPLD ;TMG/kst/CUSTOM VERSION OF TIUUPLD (PARTIAL) ;03/25/06, 2/2/14
         ;;1.0;TMG-LIB;**1**;09/01/05

 ;"CUSTOM VERSION OF TIUUPLD (PARTIAL)
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
 ;" API -- Public Functions., 2/2/14
 ;"=======================================================================
 ;"MAIN           ;" upload a batch of *.vista files that contain transcribed notes
 ;"LoadTIUBuf(DA,FPName,DestDir)   ;"ask for filename, and load into a TIU buffer
 ;"ERRORS      ;"replacement function for DISPLAY^TIUEVNT

 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================


 ;"=======================================================================
MAIN
        ;"Purpose:   To upload a batch of *.vista files that contain transcribed notes
        ;"Input: None
        ;"Results: None

        NEW EOM,TIUDA,TIUERR,TIUHDR,TIULN,TIUSRC,X

        IF '$DATA(TIUPRM0)!'$DATA(TIUPRM1) DO SETPARM^TIULE
        SET TIUSRC=$PIECE($GET(TIUPRM0),U,9)
        SET EOM=$PIECE($GET(TIUPRM0),U,11)

        IF EOM']"",($PIECE(TIUPRM0,U,17)'="k") DO  QUIT
        . WRITE !,$C(7),$C(7),$C(7),"No End of Message Signal Defined - Contact IRM.",!

        SET:TIUSRC']"" TIUSRC="R"
        SET TIUHDR=$PIECE(TIUPRM0,U,10)
        IF TIUHDR']"" DO  QUIT
        . WRITE $C(7),$C(7),$C(7),"No Record Header Signal Defined - Contact IRM.",!

        NEW done SET done=1
        NEW FPName SET FPName=""
        NEW DoAll
        NEW TMGMask,TMGFiles
        NEW JustFile,JustPath
        SET JustFile="",JustPath=""
        NEW NoDestDir SET NoDestDir=" "
        NEW DestDir SET DestDir=NoDestDir
        NEW SrcDir SET SrcDir=""
        NEW defPath SET defPath="/var/local/OpenVistA_UserData/transcription"
        NEW s
        SET s="Enter name of directory containing transcription"_$CHAR(10)_$CHAR(13)
        SET FPName=$$GETFNAME^TMGIOUTL(s,defPath,"","",.SrcDir,,"Enter Directory Name (? for Help): ")

        NEW mask SET mask="*.vista"
        NEW result
        SET TMGMask(mask)=""
        SET result=$$LIST^%ZISH(SrcDir,"TMGMask","TMGFiles")
        NEW tempFNAME SET tempFNAME=$ORDER(TMGFiles(""))
        IF tempFNAME'="" FOR  DO  QUIT:(tempFNAME="")
        . IF $$ISDIR^TMGKERNL(tempFNAME) KILL TMGFiles(tempFNAME)
        . SET tempFNAME=$ORDER(TMGFiles(tempFNAME))

        SET s="Enter DESTINATION directory to move file(s) into after upload."_$CHAR(10)_$CHAR(13)
        NEW Discard
        SET Discard=$$GETFNAME^TMGIOUTL(s,defPath_"/uploaded","","",.DestDir,,"Enter Directory Name (? for Help): ")
        WRITE !
        IF DestDir=JustPath SET DestDir=NoDestDir

        SET JustFile=$ORDER(TMGFiles(""))  ;"array holds only file names, not path

        ;"--------- loop here --------------
        FOR  DO  QUIT:(JustFile="")
        . SET TIUDA=$$MAKEBUF^TIUUPLD
        . IF +TIUDA'>0 DO  QUIT
        . . WRITE $C(7),$C(7),$C(7),"Unable to create a Buffer File Record - Contact IRM.",!
        . . SET FPName=""
        . ;"
        . IF TIUSRC="R" D REMOTE^TIUUPLD(TIUDA)
        . SET FPName=SrcDir_JustFile
        . IF TIUSRC="H" D LoadTIUBuf(TIUDA,.FPName,.DestDir)
        . IF +$GET(TIUERR) DO  QUIT
        . . WRITE $C(7),$C(7),$C(7),!,"File Transfer Error: ",$GET(TIUERR),!!,"Please re-transmit the file...",!
        . . SET FPName=""
        . ;"
        . ;" Set $ZB to MAIN+14^TIUUPLD:2
        . IF +$ORDER(^TIU(8925.2,TIUDA,"TEXT",0))>0,'+$GET(TIUERR) do
        . . DO FILE^TIUUPLD(TIUDA)
        . ;"
        . IF +$ORDER(^TIU(8925.2,TIUDA,"TEXT",0))'>0!+$GET(TIUERR) do
        . . DO BUFPURGE^TIUPUTC(TIUDA)
        . ;"
        . WRITE !!
        . IF '($GET(DestDir)="")&'(DestDir=" ") do
        . . NEW Dest SET Dest=DestDir_JustFile
        . . IF $$MOVE^TMGKERNL(FPName,Dest)=0 do
        . . . WRITE "Moved ",JustFile,!," to: ",Dest,!
        . . ELSE  do
        . . . WRITE "Unable to Move ",JustFile,!," to: ",Dest,!
        . ;"
        . WRITE "Done processing: ",JustFile,!
        . NEW KeyCont read "Press Any Key to Continue (^ to Abort)",KeyCont:$GET(DTIME,3600),!
        . SET JustFile=$ORDER(TMGFiles(JustFile))
        . IF KeyCont="^" SET JustFile=""

        QUIT



LoadTIUBuf(DA,FPName,DestDir)
        ;"Purpose: to ask user for filename, and then load this into a
        ;"        TIU buffer (that already has been created)
        ;"Input: DA : the IEN (record number) in file ^TIU(8925.2), i.e.
        ;"                in file TIU UPLOAD BUFFER, that the file is
        ;"                to be loaded into.
        ;"  FPName: OPTIONAL -- a FilePathName.  If supplied then user will not be
        ;"                              prompted to chose a file name to load
        ;"                              If passed by reference, then chosen file
        ;"                              will be passed back out.
        ;"  DestDir: OPTIONAL -- a directory to move file into after upload
        ;"              IF not provided, or IF value=" ", then don't move file
        ;"              Will not move file IF upload was unsucessful
        ;"Results: none

        ;"***NOTICE !!!!!!!
        ;"        This file is called from TIUUPLD.  If this function is broken, then
        ;"        the upload process will be broken.  So, caution!

        IF '$DATA(TIUPRM0)!'$DATA(TIUPRM1) DO SETPARM^TIULE
        WRITE @IOF,!
        DO JUSTIFY^TIUU($$TITLE^TIUU("ASCII UPLOAD"),"C")
        WRITE !

        NEW defPath
        NEW result SET result=0

        IF $GET(FPName)="" do
        . SET defPath="/var/local/OpenVistA_UserData/transcription"
        . SET FPName=$$GETFNAME^TMGIOUTL("Enter name of file containing transcription",defPath)

        IF FPName'="" do
        . IF $$Dos2Unix^TMGKERNL(FPName)>0 QUIT  ;"error on conversion prob means file doesn't exist.
        . NEW name,path,BuffP
        . DO SPLITFPN^TMGIOUTL(FPName,.path,.name)
        . IF ($GET(path)="")!($GET(name)="") QUIT
        . SET BuffP="^TIU(8925.2,"_DA_",""TEXT"",1,0)"
        . IF $$FTG^%ZISH(path,name,BuffP,4) do
        . . SET result=1
        . . NEW MaxLine SET MaxLine=$ORDER(^TIU(8925.2,DA,"TEXT",""),-1)
        . . SET ^TIU(8925.2,DA,"TEXT",0)="^^"_+MaxLine_"^"_+MaxLine_"^"_DT_"^^^^"
        . . NEW index SET index=$ORDER(^TIU(8925.2,DA,"TEXT",0))
        . . FOR  DO  QUIT:index=""
        . . . IF index="" QUIT
        . . . NEW s SET s=$$STRIP^TIUUPLD(^TIU(8925.2,DA,"TEXT",index,0))
        . . . SET ^TIU(8925.2,DA,"TEXT",index,0)=s
        . . . SET index=$ORDER(^TIU(8925.2,DA,"TEXT",index))

        IF result=0 do
        . WRITE "Unsuccessful upload.",!

        QUIT



ERRORS
        ;"Purpose: This is replacement function of for DISPLAY^TIUEVNT
        ;"              This function is used in processing Alerts created from failed document
        ;"              uploads.  This function is wedged into DISPLAY^TIUEVNT to allow
        ;"              customization.
        ;"Input:   none.
        ;"           global scope variables are used:
        ;"              XQX1
        ;"              TIUPRM0,TIUPRM1
        ;"              DIRUT
        ;"              XQADATA  , e.g.:  349;FILING ERROR: NOTE  Record could not be found or created.;30853;1302
        ;"                              349 --> TIUBUF
        ;"                              30853 --> TIUEVNT and EVNTDA
        ;"                              1302 --> TIUTYPE

        NEW DIC,INQUIRE,RETRY,DWPK,EVNTDA,TIU K XQAKILL,RESCODE,TIUTYPE
        NEW TIUDONE ;"<-- this is changed elsewhere... where?
        NEW TIUEVNT,TIUSKIP,TIUBUF

        WRITE !,"TMG Custom Upload Error Handler.",!
        WRITE "---------------------------------------",!!

        IF '$DATA(TIUPRM0)!'$DATA(TIUPRM1) DO SETPARM^TIULE

        ;" Set EVNTDA for backward compatibility, TIUEVNT for PN resolve code
        SET (EVNTDA,TIUEVNT)=+$PIECE(XQADATA,";",3)

        ;" Set TIUBUF for similarity w TIURE.  DON'T SET BUFDA since
        ;" old code interprets that as SET by TIURE only:
        SET TIUBUF=+XQADATA
        SET TIUTYPE=+$PIECE(XQADATA,";",4)
        SET TIUSKIP=($DATA(DIRUT)>0)

        IF TIUTYPE>0 SET RESCODE=$$FIXCODE^TIULC1(TIUTYPE)

        NEW defInput SET defInput="1"
        NEW input
        FOR  DO  QUIT:(+input<1)!(+input>5)
        . DO WRITEHDR^TIUPEVNT(TIUEVNT)
        . WRITE !,$PIECE(XQADATA,";",2),!
        . WRITE "OPTIONS:",!
        . WRITE "1. Inquire to patient record.",!
        . WRITE "2. Create/edit patient record.",!
        . WRITE "3. Mark note for automatic patient registration.",!
        . ;"WRITE "4. Show note header again.",!
        . WRITE "5. Edit erroneous note.",!
        . WRITE "6. Retry filing buffer (and QUIT)",!
        . WRITE "7. Abort",!
        . WRITE !
        . WRITE "Select option (1-7,?,^): ",defInput,"// "
        . read input:$GET(DTIME,3600),!
        . IF input="" SET input=defInput
        . IF input["?" DO  QUIT
        . . WRITE "--Regarding option 1:"
        . . DO INQRHELP^TIUPEVNT WRITE !!
        . . WRITE "--Regarding option 2:",!
        . . WRITE "To directly edit the patient name, DOB etc, select this.",!
        . . WRITE "(Caution: only change patient entry IF you are SURE information is incorrect.)",!!
        . . WRITE "--Regarding option 3",!
        . . WRITE "This will cause the the information in the note to be used to automatically",!
        . . WRITE "register the patient.  Caution! Be careful to not cause a duplicate entry",!
        . . WRITE "in the database.  Only use this option IF you are SURE the patient is NOT",!
        . . WRITE "already registered.  Don't use IF patient is in database, but with incorrect",!
        . . WRITE "information.",!!
        . . ;"WRITE "--Regarding option 4:",!
        . . ;"WRITE "This will display the header the filer found initially.",!!
        . . WRITE "--Regarding option 5:",!
        . . WRITE "Select this option to launch a text editor to correct note",!!
        . . WRITE "--Regarding option 6:"
        . . WRITE "--Regarding option 7:",!
        . . WRITE "This will abort process.  Error and Alert will remain unchanged.",!!
        . . WRITE !
        . . SET input=1  ;"just to allow loop to continue
        . . NEW temp read "Press [ENTER] to continue.",temp:$GET(DTIME,3600),!
        . IF +input=1 DO  QUIT           ;"1. Inquire to patient record."
        . . IF $GET(RESCODE)="" DO  QUIT
        . . . WRITE !!,"Filing error resolution code could not be found for this document type.",!
        . . . WRITE "Please edit the buffered data directly and refile.",!
        . . . NEW temp read "Press [ENTER] to continue.",temp:$GET(DTIME,3600),!
        . . . SET defInput=5
        . . DO WRITEHDR^TIUPEVNT(TIUEVNT)
        . . xecute RESCODE
        . ELSE  IF +input=2 DO  QUIT  ;"2. Create/edit patient record."
        . . DO WRITEHDR^TIUPEVNT(TIUEVNT)
        . . WRITE "Hint: IF entering a patient's name brings up the wrong patient, then",!
        . . WRITE "       enter name in quotes (e.g. ""DOE,JOHN"") to force addition of a new",!
        . . WRITE "       patient with a same name as one alread registered."
        . . DO EDITPT^TMGMISC(1)
        . . SET defInput=6
        . ELSE  IF +input=3 DO  QUIT  ;"3. Mark note for automatic patient registration."
        . . ;"TMGSEX is a variable with global scope used by filer.
        . . FOR  DO  QUIT:(TMGSEX'="")
        . . . read "Is patient MALE or FEMALE? (M/F)  // ",TMGSEX:$GET(DTIME,3600),!
        . . . SET TMGSEX=$$UP^XLFSTR(TMGSEX)
        . . . IF (TMGSEX="MALE")!(TMGSEX="M") SET TMGSEX="MALE"
        . . . ELSE  IF (TMGSEX="FEMALE")!(TMGSEX="F") SET TMGSEX="FEMALE"
        . . . ELSE  IF TMGSEX="^" QUIT
        . . . ELSE  SET TMGSEX="" WRITE "??  Please enter MALE or FEMALE (or ^ to abort)",!
        . . IF TMGSEX="^" SET TMGSEX="" QUIT
        . . SET TMGFREG=1 ;"this is a signal for TMGGDFN to register patient IF not otherwise found.
        . . WRITE "Patient is marked for AUTOMATIC REGISTRATION.",!
        . . NEW temp read "Press [ENTER] to continue.",temp:$GET(DTIME,3600),!
        . . SET defInput=6
        . ;"else  IF +input=4 DO  QUIT  ;"4. Show note header again."
        . ;". DO WRITEHDR^TIUPEVNT(TIUEVNT)
        . ELSE  IF +input=5 DO  QUIT  ;"5. Edit buffer."
        . . SET DIC="^TIU(8925.2,"_TIUBUF_",""TEXT"","
        . . SET DWPK=1
        . . DO EN^DIWE
        . . SET defInput=6
        . ELSE  IF +input=6 DO  QUIT  ;"6. Retry filing buffer (and QUIT)"
        . . DO ALERTDEL^TIUPEVNT(TIUBUF)
        . . DO RESOLVE^TIUPEVNT(TIUEVNT,1)
        . . DO FILE^TIUUPLD(TIUBUF)
        . ELSE  DO  QUIT

        ;" Redundant IF all RESCODEs DO RESOLVE:
        IF +$GET(TIUDONE),+$GET(TIUEVNT) DO RESOLVE^TIUPEVNT(+$GET(TIUEVNT))

        KILL TMGFREG

DISPX
        KILL XQX1
        QUIT

