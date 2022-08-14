TMGPAT1  ;TMG/kst/Patching tools ;09/17/08, 2/2/14
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
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"DONEXT -- Ask for package, and install next appropriate patch
 ;"DONEXTPJ -- DO next patch within the same project
 ;"FIXMISEQ -- allow fixing a missing patch, based on sequence order
 ;"FIXMISPT -- allow fixing a missing patch, based on patch number
 ;"DONEXTPK(PCKINIT,VER)-- a common entry point
 ;"GO(OPTION,INFO,Msg) -- Entry point to allow automatic loading and installation of a patch.
 ;"LOAD -- Entry point to allow manual loading of a patch (like old way, but using NEW code)
 ;
 ;"=======================================================================
 ;"Private Functions
 ;"=======================================================================
 ;"LOGO
 ;"FIXMISNG(MODE) allow fixing a missing patch, even IF it is out of sequence order
 ;"FXMSINIT(PCKINIT,VER,MODE) -- Based on PCKINIT, and Version, allow fixing a missing patch, even IF it is out of sequence order
 ;"DOFIXMSG(IENS,MODE) -- fix a missing patch, even IF it is out of sequence order
 ;"DOIENS(PCKINIT,NEXTIENS,NEXTPATCH,PCKDIRFNAME) -- install patch, given IENS to it's entry in 22709.11
 ;"GETPCKVER(PCKINIT,VER) -- query user for package, and desired version
 ;"GETLSTPK(PCKINIT,VER) -- for given package initials, return the last patch applied
 ;"GETPVIEN(PCKINIT,VER,IEN9D4,IEN9D49) -- convert PCKINIT and VER into IEN's for file 9.4 ad 9.49
 ;"GETLASTINST(PCKINIT,VER,InstIEN) -- for given package initials, return the last patch applied in INSTALL file
 ;"GETVERS(PCKINIT,VERARRAY) --for given package initials, return possible versions
 ;"PICKFILE(PARRAY) -- Pick a filename from a list, in a menu
 ;
 ;"=======================================================================

LOGO    WRITE !!,"===================",!
        WRITE "TMG Patcher Helper",!
        WRITE "===================",!
        QUIT

DONEXT
        ;"Purpose: Ask for package, and install next appropriate patch
        DO LOGO
        NEW PCKINIT,VER
        DO GETPCKVER(.PCKINIT,.VER)
        IF VER="^" GOTO DNDONE
        DO DONEXTPK(PCKINIT,VER)
        GOTO DONEXT
DNDONE
        QUIT

DONEXTPJ
        ;"Purpose: to DO next patch within the same project
        DO LOGO
        NEW PCKINIT,VER
        NEW s SET s=$GET(^TMG("KIDS","PROJECT"))
        IF s'="" DO
        . SET PCKINIT=$PIECE(s,"^",1)
        . SET VER=$PIECE(s,"^",2)
        ELSE  DO
        . DO GETPCKVER(.PCKINIT,.VER)
        . SET s=PCKINIT_"^"_VER
        . SET ^TMG("KIDS","PROJECT")=s
        IF VER="^" GOTO DNDONE
        DO DONEXTPK(PCKINIT,VER)
DNPJDONE
        QUIT


FIXMISEQ
        ;"Purpose: to allow fixing a missing patch, based on sequence order
        DO FIXMISNG(1)
        QUIT


FIXMISPT
        ;"Purpose: to allow fixing a missing patch, based on patch number
        DO FIXMISNG(2)
        QUIT


FIXMISNG(MODE)
        ;"Purpose: to allow fixing a missing patch, even IF it is out of sequence order
        ;"         This will prepare, then call DOFIXMSG()
        ;"Input: MODE -- 1: search by SEQ#, 2: search by PATCH#
        DO LOGO
        WRITE "Fix missing patches, regardless of order",!
        NEW PCKINIT,VER
        DO GETPCKVER(.PCKINIT,.VER) GOTO:(VER="^") FMIDONE
        DO FXMSINIT(.PCKINIT,.VER,.MODE)
FMDONE  QUIT


FXMSINIT(PCKINIT,VER,MODE)  ;"FIX MISSING INIT
        ;"Purpose: Based on PCKINIT, and Version, allow fixing a missing patch, even IF it is out of sequence order
        ;"         This will prepare, then call DOFIXMSG()
        ;"Input: MODE -- 1: search by SEQ#, 2: search by PATCH#
        SET PCKINIT=$GET(PCKINIT) GOTO:(PCKINIT="") FMIDONE
        SET VER=$GET(VER) GOTO:(VER="") FMIDONE
        NEW IENS SET IENS=$$GETIENS2^TMGPAT2(PCKINIT_"*"_VER) GOTO:(IENS="") FMIDONE
        DO DOFIXMSG(IENS,.MODE)
FMIDONE QUIT


DOFIXMSG(IENS,MODE,NOMORE) ;
        ;"Purpose: fix a missing patch, even IF it is out of sequence order
        ;"Input: IENS -- should be IENS from file 27709.01
        ;"              i.e. 'IEN22709d01,IEN22709,'
        ;"            **OR**
        ;"            -- May be IENS to file 22709.11
        ;"              i.e. 'IEN22709d11,IEN22709d01,IEN22709,'
        ;"              (when this is provided, then MODE is ignored)
        ;"       MODE -- 1: search by SEQ#, 2: search by PATCH#
        ;"       NOMORE -- OPTIONAL.  If 1, then "Fix more?" is not asked.

        NEW PCKINIT,VER,Y,PATCHNAME,Msg,PCKDIRFNAME
        NEW SRCHNUM,DIR,FOUND
        NEW PATCHNUM SET PATCHNUM=0
        NEW SEQNUM SET SEQNUM=0
        SET IENS=$GET(IENS) GOTO:(IENS="") FMSDONE
        IF $EXTRACT(IENS,$LENGTH(IENS))'="," SET IENS=IENS_","
        NEW IEN1,IEN2,IEN3
        IF $LENGTH(IENS,",")=4 DO
        . SET IEN1=+$PIECE(IENS,",",3) GOTO:(IEN1'>0) FMSDONE
        . SET IEN2=+$PIECE(IENS,",",2) GOTO:(IEN2'>0) FMSDONE
        . SET IEN3=+$PIECE(IENS,",",1) GOTO:(IEN3'>0) FMSDONE
        . SET PATCHNUM=$PIECE($GET(^TMG(22709,IEN1,1,IEN2,1,IEN3,0)),"^",1)
        . SET SEQNUM=$PIECE($GET(^TMG(22709,IEN1,1,IEN2,1,IEN3,0)),"^",2)
        ELSE  DO
        . SET IEN1=+$PIECE(IENS,",",2) GOTO:(IEN1'>0) FMSDONE
        . SET IEN2=+$PIECE(IENS,",",1) GOTO:(IEN2'>0) FMSDONE
        . SET IEN3=0
        NEW IEN9D4 SET IEN9D4=$PIECE($GET(^TMG(22709,IEN1,0)),"^",1) GOTO:(IEN9D4'>0) FMSDONE
        SET PCKINIT=$PIECE($GET(^DIC(9.4,IEN9D4,0)),"^",2)
        SET VER=$PIECE($GET(^TMG(22709,IEN1,1,IEN2,0)),"^",1) GOTO:(VER="") FMSDONE
        NEW SAVIENS SET SAVIENS=IENS
        SET MODE=+$GET(MODE,1)

        DO MAKFRESH^TMGPAT2(PCKINIT,.Msg,.PCKDIRFNAME)
        IF IEN3>0 GOTO FM2
FMLoop  KILL SRCHNUM,DIR
        SET DIR(0)="N^1:9999:0"
        IF MODE=1 SET DIR("A")="Enter missing patch SEQUENCE NUMBER"
        ELSE  SET DIR("A")="Enter missing PATCH NUMBER"
        DO ^DIR WRITE !
        IF $GET(DIRUT) GOTO FMSDONE
        SET SRCHNUM=+Y

        ;"Search for specified PATCH/SEQ #
        SET FOUND=0
        SET IEN3=0
        SET PATCHNUM=0
        SET SEQNUM=0
        FOR  SET IEN3=$ORDER(^TMG(22709,IEN1,1,IEN2,1,IEN3)) QUIT:(IEN3'>0)!FOUND  DO
        . SET PATCHNUM=$PIECE($GET(^TMG(22709,IEN1,1,IEN2,1,IEN3,0)),"^",1)
        . SET SEQNUM=$PIECE($GET(^TMG(22709,IEN1,1,IEN2,1,IEN3,0)),"^",2)
        . IF (MODE=1),(SEQNUM=SRCHNUM) SET FOUND=IEN3
        . ELSE  IF (MODE=2),(PATCHNUM=SRCHNUM) SET FOUND=IEN3
        . IF FOUND DO
        . . SET IENS=IEN3_","_IENS
        . . SET PATCHNAME=PCKINIT_"*"_VER_"*"_PATCHNUM_" SEQ #"_SEQNUM

        IF FOUND=0 DO  GOTO FMSDONE
        . WRITE "Sorry.  Can't find that PATCH/SEQ Number in downloaded info from patch repository server",!

FM2     SET PATCHNAME=PCKINIT_"*"_VER_"*"_PATCHNUM_" SEQ #"_SEQNUM
        WRITE "Found: ",PATCHNAME," on patch repository server.",!

        ;"Now see IF already installed
        NEW DIC,X,Y,IEN9D49
        ;"SET IEN9D49=+$ORDER(^DIC(9.4,IEN9D4,22,"B",VER,""))
        SET DIC="^DIC(9.4,"_IEN9D4_",22,"
        SET DIC(0)="M"
        SET X=VER
        DO ^DIC
        SET IEN9D49=+Y
        IF IEN9D49'>0 DO  GOTO FMSDONE
        . WRITE "?? Can't find that version in the PACKAGE file",!

        SET FOUND=0
        NEW i SET i=""
        FOR  SET i=$ORDER(^DIC(9.4,IEN9D4,22,IEN9D49,"PAH","B",i)) QUIT:(i="")!FOUND  DO
        . NEW ONEPATCHNUM SET ONEPATCHNUM=$PIECE(i," ",1)
        . IF ONEPATCHNUM=PATCHNUM SET FOUND=$ORDER(^DIC(9.4,IEN9D4,22,IEN9D49,"PAH","B",i,""))

        IF FOUND DO  GOTO FMSMore
        . NEW NEWIENS SET NEWIENS=FOUND_","_IEN9D49_","_IEN9D4
        . WRITE "Sorry, that patch is already installed.  IENS='",NEWIENS,"'",!
        . DO ForceP2^TMGPAT2(PCKINIT,VER,PATCHNUM,SEQNUM)


        NEW % SET %=1
        WRITE "Do you want to work on this patch now (out of sequence order)" DO YN^DICN WRITE !
        IF %'=1 GOTO FMSDONE

        IF $$DOIENS(PCKINIT,IENS,PATCHNAME,.PCKDIRFNAME)  ;"actually process patch
        ;
FMSMore IF $GET(NOMORE)=1 GOTO FMSDONE
        SET %=1
        WRITE "Fix others in this same Package-Version" DO YN^DICN WRITE !
        IF %=1 DO  GOTO FMLoop
        . SET IENS=SAVIENS
        ;
FMSDONE QUIT


DONEXTPK(PCKINIT,VER)
        ;"Purpose: a common entry point
        ;"Input: PCKINIT -- package initials
        ;"       VER -- version e.g. '22.0'
        ;"Result: NONE

        NEW LASTPCK,NEXTIENS,NEXTPATCH,LASTINST,PCKDIRFNAME
        NEW INFO,Msg,OPTION,RESULT,restart,InstIEN,ABORT,chgVER

        SET (ABORT,restart,chgVER)=0
        DO MAKFRESH^TMGPAT2(PCKINIT,.Msg,.PCKDIRFNAME)
DNP1    SET LASTPCK=$$GETLSTPK(PCKINIT,VER)
        IF LASTPCK="" DO  GOTO AllDONE
        . WRITE "Sorry, can't find any installed packges in that version.",!
        SET LASTINST=$$GETLASTINST(PCKINIT,VER,.InstIEN)
        WRITE "The last patch installed into PACKAGE file was: ",?50,LASTPCK,!
        IF LASTINST="" GOTO DNP2
        WRITE "The last patch loaded into the INSTALL file was: ",?50,LASTINST," (FYI)",!
        IF +$PIECE(LASTPCK,"SEQ #",2)<+$PIECE(LASTINST,"SEQ #",2) DO  GOTO:(restart) AllDONE
        . NEW % SET %=2
        . WRITE !,"Undelete entry in INSTALL FILE" DO YN^DICN WRITE !
        . IF %=1 DO  QUIT
        . . DO EN1^TMGXPDIU(InstIEN)
        . . SET restart=1
        . SET %=1
        . WRITE "Try to continue with existing files" DO YN^DICN WRITE !
        . IF %'=1 SET restart=1 QUIT
        IF restart GOTO AllDONE
DNP2    SET NEXTIENS=$$GETNEXTIENS^TMGPAT2(LASTPCK,.NEXTPATCH)
        NEW PENDINGCT SET PENDINGCT=$$RPT1AVAL^TMGPAT3(LASTPCK)
        IF PENDINGCT>0 WRITE "Number of pending patches for this package:",?50,PENDINGCT,!
        ELSE  DO  GOTO:(chgVER) DNP1
        . IF $$RPTAVAIL^TMGPAT3(PCKINIT)=0 SET chgVER=0 QUIT
        . DO ASKVER(PCKINIT,.VER)
        . SET chgVER=1

        IF $$DOIENS(PCKINIT,NEXTIENS,NEXTPATCH,.PCKDIRFNAME)=0 GOTO AllDONE

More    SET %=1
        WRITE "Do more patch installations in this package" DO YN^DICN WRITE !!
        IF %=1 GOTO DNP1
AllDONE
        QUIT

DOIENS(PCKINIT,NEXTIENS,NEXTPATCH,PCKDIRFNAME)
        ;"Purpose: install patch, given IENS to it's entry in 22709.11
        ;"Input: PCKINIT -- Package initials
        ;"       NEXTIENS -- IENS for entry in 22709.11
        ;"       NEXTPATCH -- Patch Name of patch to be applied.
        ;"Results: 1 if OK, 0 IF problem.
        NEW INFO  ;"//kt added 6/17/22
        NEW ABORT SET ABORT=0
        IF NEXTPATCH'="" DO
        . WRITE "Next patch to install is: ",?50,NEXTPATCH,!
        ELSE  DO  GOTO DNPDONE
        . WRITE "No more patches available for this package.",!
        . SET ABORT=1
        . QUIT  ;  ;"//kt fix below later...
        . WRITE "View list of all patches for this package on patch repository server"
        . SET %=2
        . DO YN^DICN WRITE !
        . IF %=1 IF $$EditHFSFile^TMGKERNL(PCKDIRFNAME)
        . IF %=-1 SET restart=1 QUIT
        NEW OPTION SET OPTION("VERBOSE")=1
        NEW DLSUCCESS SET DLSUCCESS=0  ;"default to failure
        SET RESULT=$$ENSRLOCL^TMGPAT2(NEXTIENS,.INFO,.Msg,.OPTION,PCKINIT)
        IF RESULT=0 DO  GOTO DNPDONE
        . DO ADDMSG^TMGPAT2("Unable to find patch on local file system.",1,Msg)
        . IF $$SHOWMSG^TMGPAT2(.Msg)
        ELSE  SET DLSUCCESS=1
        NEW TEMP SET TEMP=$GET(INFO("TEXT FILE"),$GET(INFO("KID FILE")))
        SET INFO("PATCH NAME")=$PIECE(TEMP,".",1)
        ;
        NEW % SET %=1
        IF $GET(INFO("KID FILE"))'="" DO PANALYZE^TMGPAT4(.INFO,.OPTION)
        IF $GET(INFO("TEXT FILE"))'="" DO  GOTO:(ABORT=1) DNPDONE
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
        . . . DO PICKFILE(.INFO,$NAME(INFO("MISC KID FILES")))        
        . . SET FNAME=$GET(INFO("KID FILE"))
        . . IF 1=0,FNAME="" DO
        . . . WRITE !,"No KIDS filename found for this patch.",!
        . . . WRITE "Having read the INFO TEXT file, would you like to ",!
        . . . WRITE "browse the patch server to pick the correct KIDS file"
        . . . SET %=2 DO YN^DICN WRITE !
        . . . IF %'=1 QUIT
        . . . NEW TEMP SET TEMP=$$URLPICK(.INFO)  ;"may alter INFO("KID URL")
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
        . . . NEW TEMP SET TEMP=$$URLPICK(.INFO)  ;"may alter INFO("KID URL")
        . . . IF +TEMP=-1 DO  SET DONE=1 QUIT
        . . . . WRITE !,"Problem: ",$PIECE(TEMP,"^",2),!
        . . . . DO PRESS2GO^TMGUSRI2
        IF %=-1 GOTO DNPDONE
        ;
        IF $DATA(INFO("STILL NEEDED")) DO  GOTO:(ABORT=1) DNPDONE
        . NEW PARRAY SET PARRAY=$NAME(INFO("STILL NEEDED"))
        . DO STORMSNG^TMGPAT3(PCKINIT,PARRAY)
        . WRITE "It seems that the system is not ready for this patch.",!
        . WRITE "(However, sometimes this can be ignored and ONE can proceed anyway.)",!
        . SET %=1
        . WRITE "Quit this patch and try another" DO YN^DICN WRITE !
        . IF %'=2 SET ABORT=1
        ;
        ;"IF $GET(INFO("TEXT ONLY"))=1 DO  GOTO DNPDONE
        IF $GET(INFO("KID FILE"))="" DO  GOTO DNPDONE
        . WRITE "This 'patch' doesn't have a corresponding KID file.",!
        . WRITE "Perhaps it was informational only.  I'm not smart enough to figure that out.",!
        . WRITE "If you didn't read the INFO FILE, then answer NO, and loop back and read it.",!
        . SET %=2
        . WRITE "Ready to consider the patch 'installed'" DO YN^DICN WRITE !
        . IF %'=1 QUIT
        . IF $$MakePATCHEntry^TMGPAT2(NEXTPATCH,.Msg)
        ; 
        ;"IF $GET(INFO("KID FILE"))="" DO  GOTO DNPDONE
        ;". WRITE "?? No name for KID file ??",!
        ;
        NEW RESULT SET RESULT=0  ;"default to problem
        IF DLSUCCESS'=1 GOTO DINSDN
        SET %=1
        WRITE "Ready to load patch "_INFO("KID FILE")_" into system" DO YN^DICN WRITE !
        IF %'=1 SET %=-1 GOTO DNPDONE
        SET RESULT=$$GO(.OPTION,.INFO,.Msg)
        ;
DINSDN  QUIT RESULT
        ;
GO(OPTION,INFO,Msg)
        ;"Purpose: Entry point to allow automatic loading and installation of a patch.
        ;"Input: OPTION -- PASS BY REFERENCE.  Can be an empty array.
        ;"       INFO -- PASS BY REFERENCE. Needs:
        ;"          INFO("PATH")=<path on HFS of kid file>
        ;"          INFO("KID FILE")=<name of file on HFS, without path>
        ;"          INFO("TEXT FILE") -- OPTIONAl (may be deleted based on user choice at runtime)
        ;"       Msg -- PASS BY REFERNCE.
        ;"Results: 1 if OK, 0 IF problem.
        NEW ABORT SET ABORT=0
        SET OPTION("HFSNAME")=$GET(INFO("PATH"))_INFO("KID FILE")
        SET OPTION("FORCE CONT LOAD")=1
        SET OPTION("DO ENV CHECK")=1
        DO EN1^TMGXPDIL(.OPTION,.Msg)
        NEW errorFound SET errorFound=$$SHOWMSG^TMGPAT2(.Msg,1)
        IF errorFound GOTO DNP3
        NEW InstallNAME SET InstallNAME=$GET(OPTION("INSTALL NAME"))
        IF InstallNAME="" DO  GOTO DNPDONE
        . WRITE "No installation name found.  Aborting.",!
        . SET ABORT=1
        NEW % SET %=1
        WRITE "Proceed with installation" DO YN^DICN WRITE !
        KILL Msg
        NEW OPTION ;"... FINISH..., add presets to avoid user interactivity later...
        SET OPTION("Want to DISABLE Scheduled Options, Menu Options, and Protocols","DEFAULT")="NO"
        SET OPTION("Want KIDS to INHIBIT LOGONs during the install","DEFAULT")="NO"
        SET OPTION("Want KIDS to Rebuild Menu Trees Upon Completion of Install","DEFAULT")="NO"
        IF %=1 DO EN^TMGXPDI(InstallNAME,.OPTION,.Msg)
        IF $$SHOWMSG^TMGPAT2(.Msg,1)
DNP3    NEW TEMPNull
        IF $DATA(PCKINIT) DO
        . DO STORMSNG^TMGPAT3(PCKINIT,"TEMPNull") ;"clear out pending patches...
        ELSE  GOTO DNPDONE
        ;
        SET %=1
        WRITE "Clean up local files for this patch" DO YN^DICN WRITE !
        IF %=-1 GOTO DNPDONE
        IF %=1 DO
        . NEW path SET path=$GET(INFO("PATH"))
        . NEW FILENAME SET FILENAME=$GET(INFO("KID FILE"))
        . IF FILENAME'="" IF $$DELFILE^TMGIOUTL(path_FILENAME)
        . SET FILENAME=$GET(INFO("TEXT FILE"))
        . IF FILENAME'="" IF $$DELFILE^TMGIOUTL(path_FILENAME)
DNPDONE QUIT (ABORT=0)
        ;
LOAD    ;
        ;"Purpose: Entry point to allow manual loading of a patch (like old way, but using NEW code)
        DO LOGO^TMGPAT1 WRITE !
        NEW FPNAME,FPATH,FNAME,Msg,OPTION,INFO
        SET FPNAME=$$GETFNAME^TMGIOUTL("Select Patch to Load",,,,.FPATH,.FNAME)
        IF (FPNAME="") QUIT
        IF (FPNAME="^") QUIT
        SET INFO("PATH")=FPATH,INFO("KID FILE")=FNAME
        NEW RESULT SET RESULT=$$GO(.OPTION,.INFO,.Msg)
        QUIT
        ;
GETPCKVER(PCKINIT,VER)
        ;"Purpose: query user for package, and desired version
        ;"Input: PCKINIT - PASS BY REFERENCE.  An OUT PARAMETER. The package initials, e.g. 'DI' in the case of Fileman
        ;"       VER -- PASS BY REFERENCE.  An OUT PARAMETER.  The version of the package to match.
        ;"RESULTs: NONE;  (if user ABORTed, VER="^")
        SET VER="^"
        NEW DIC,X,Y
        SET DIC=9.4,DIC(0)="MAEQ"  ;"ask for package name
        DO ^DIC WRITE !
        IF +Y'>0 GOTO GPVDONE
        NEW PACKAGE SET PACKAGE=$PIECE(Y,"^",2)
        SET PCKINIT=$PIECE($GET(^DIC(9.4,+Y,0)),"^",2)
        IF PCKINIT="" DO  GOTO GPVDONE
        . WRITE "Error.  Unable to obtain package prefix.",!
        DO ASKVER(PCKINIT,.VER)
        ;
GPVDONE QUIT
        ;
ASKVER(PCKINIT,VER,PACKAGE)
        ;"Purpose: query user for desired version from specified package
        ;"Input: PCKINIT - The package initials, e.g. 'DI' in the case of Fileman
        ;"       VER -- PASS BY REFERENCE.  An OUT PARAMETER.  The version of the package to match.
        ;"       PACKAGE -- OPTIONAL.  Name of Package.  Default is same as PCKINIT
        ;"RESULTs: nONE;  (if user ABORTed, VER="^")
        ;
        NEW VERARRAY
        DO GETVERS(PCKINIT,.VERARRAY)
        IF $DATA(VERARRAY)=0 DO  GOTO AVrDONE
        . WRITE "Error. No version number available.",!
        IF $GET(PACKAGE)="" SET PACKAGE=PCKINIT
        ;
        NEW MENU,i,Usr
        SET MENU(0)="Select Version of "_PACKAGE
        SET VER="",i=0
        FOR  SET VER=$ORDER(VERARRAY(VER)) QUIT:(VER="")  DO
        . SET i=i+1
        . NEW TEMPPATCH SET TEMPPATCH=PCKINIT_"*"_VER_"*1"
        . NEW COUNT SET COUNT=+$$RPT1AVAL^TMGPAT3(TEMPPATCH)
        . SET MENU(i)="Version "_VER_" ("_COUNT_" patches pending)"_$C(9)_VER
        IF i=1 SET VER=$ORDER(VERARRAY(""))  ;"must be only ONE option, so skip menu
        ELSE  DO
        . SET VER=$$MENU^TMGUSRI2(.MENU,"^")
        ;
AVrDONE QUIT
        ;
GETPVIEN(PCKINIT,VER,IEN9D4,IEN9D49)
        ;"Purpose: to convert PCKINIT and VER into IEN's for file 9.4 ad 9.49
        ;"Input: PCKINIT - .  The package initials, e.g. 'DI' in the case of Fileman
        ;"       VER -- The version of the package to match.
        ;"       IEN9D4 - PASS BY REFERENCE.  The IEN in 9.4 to return
        ;"       IEN9D49 -- PASS BY REFERENCE.  The IEN in 9.49 to return
        ;"RESULT: 1 if OK, 0 IF not found.
        NEW RESULT SET RESULT=0
        ;
        SET IEN9D4=+$ORDER(^DIC(9.4,"C",PCKINIT,""))
        IF IEN9D4'>0 GOTO GPVIDne
        NEW DIC,X,Y
        SET DIC="^DIC(9.4,"_IEN9D4_",22,"
        SET DIC(0)="M"
        SET X=VER
        DO ^DIC
        SET IEN9D49=+Y
        IF IEN9D49'>0 GOTO GPVIDne
        SET RESULT=1
        ;
GPVIDne QUIT RESULT
        ;
GETLSTPK(PCKINIT,VER)  ;"GET LAST PACKAGE
        ;"Purpose: for given package initials, return the last patch applied
        ;"         This searches the PACKAGE file
        ;"Input: PCKINIT - the package initials, e.g. 'DI' in the case of Fileman
        ;"       VER -- the version of the package to match.
        ;"Results: returns e.g. 'DI*22.0*140 SEQ# 123'
        ;"         or "" IF problem or not found.
        NEW RESULT SET RESULT=""
        SET VER=$GET(VER)
        SET PCKINIT=$GET(PCKINIT)
        IF (PCKINIT="")!(VER="") GOTO GLPDONE
        NEW IEN9D4,IEN9D49
        SET IEN9D4=+$ORDER(^DIC(9.4,"C",PCKINIT,""))
        IF IEN9D4'>0 GOTO GLPDONE
        SET IEN9D49=+$ORDER(^DIC(9.4,IEN9D4,22,"B",VER,""))
        IF IEN9D49'>0 GOTO GLPDONE
        ;
        NEW i,ARRAY
        SET i=""
        FOR  SET i=$ORDER(^DIC(9.4,IEN9D4,22,IEN9D49,"PAH","B",i)) QUIT:(i="")  DO
        . NEW PATCHNUM SET PATCHNUM=$PIECE(i," ",1)
        . NEW SEQNUM SET SEQNUM=$PIECE(i,"SEQ #",2)
        . NEW TEMPNAME SET TEMPNAME=i
        . IF SEQNUM="" DO
        . . SET TEMPNAME=PCKINIT_"*"_VER_"*"_PATCHNUM
        . . SET TEMPNAME=$$GETSEQ^TMGPAT4(TEMPNAME)
        . . SET SEQNUM=$PIECE(TEMPNAME,"SEQ #",2)
        . . IF SEQNUM="" QUIT
        . . NEW IEN9D4901 SET IEN9D4901=$ORDER(^DIC(9.4,IEN9D4,22,IEN9D49,"PAH","B",i,""))
        . . ;"WRITE "The PACKAGE file entry for patch "_PCKINIT_"*"_VER_"*"_PATCHNUM_" doesn't have a SEQ #",!
        . . ;"WRITE "By comparing this to patch files available on patch repository server, it",!
        . . ;"WRITE "appears that this patch should be SEQ #",TEMPNAME,!
        . . ;"NEW % SET %=1
        . . ;"WRITE "Shall I correct this in the PACKAGE file" DO YN^DICN WRITE !
        . . ;"IF %'=1 QUIT
        . . NEW TMGFDA,TMGMSG
        . . SET TMGFDA(9.4901,IEN9D4901_","_IEN9D49_","_IEN9D4_",",.01)=PATCHNUM_" SEQ #"_SEQNUM
        . . DO FILE^DIE("K","TMGFDA","TMGMSG")
        . . ;"do SHOWDIER^TMGDEBU2(.TMGMSG)
        . ;"IF SEQNUM is still "" at this point, it won't find the patch, could be bug...
        . SET SEQNUM=$$RJ^XLFSTR(SEQNUM,6,"0")
        . IF TEMPNAME["*" SET TEMPNAME=$PIECE(TEMPNAME,"*",3)
        . SET ARRAY(SEQNUM)=TEMPNAME
        ;
        SET i=$ORDER(ARRAY(""),-1)
        IF i'="" SET RESULT=PCKINIT_"*"_VER_"*"_$GET(ARRAY(i))
        ELSE  SET RESULT=PCKINIT_"*"_VER_"*0 SEQ #0"
        ;"ELSE  SET RESULT=$PIECE($GET(^DIC(9.4,IEN9D4,0)),"^",1)_" SEQ #0"
        ;
GLPDONE QUIT RESULT
        ;
GETLASTINST(PCKINIT,VER,InstIEN)
        ;"Purpose: for given package initials, return the last patch applied
        ;"         This searches the INSTALL file
        ;"Input: PCKINIT - the package initials, e.g. 'DI' in the case of Fileman
        ;"       VER -- the version of the package to match.
        ;"       InstIEN -- Optional.  Pass by Reference, an OUT PARAMETER.
        ;"Results: returns last patch + 'SEQ# 123' (123 is example)
        ;"         or "" IF problem or not found.
        NEW RESULT SET RESULT=""
        NEW ARRAY
        SET VER=$GET(VER)
        NEW i SET i=$GET(PCKINIT)
        IF (i="")!(VER="") GOTO GLIDONE
        NEW DONE SET DONE=0
        FOR  SET i=$ORDER(^XPD(9.7,"B",i)) QUIT:(i="")!DONE  DO
        . IF $PIECE(i,"*",1)'=PCKINIT SET DONE=1 QUIT
        . IF $PIECE(i,"*",2)'=VER QUIT
        . NEW patch SET patch=$PIECE(i,"*",3)
        . SET patch=$$RJ^XLFSTR(patch,6,"0")
        . NEW IEN SET IEN=$ORDER(^XPD(9.7,"B",i,""))
        . IF IEN=0 QUIT
        . NEW seq SET seq=$$GET1^DIQ(9.7,IEN_",",42001)
        . IF seq="" QUIT
        . ;"WRITE "SEQ #"_seq_"  "_i,!
        . SET ARRAY(seq)=i_" SEQ #"_seq_"^"_IEN
        ;
        SET i=$ORDER(ARRAY(""),-1)
        SET RESULT=$GET(ARRAY(i))
        SET InstIEN=$PIECE(RESULT,"^",2)
        SET RESULT=$PIECE(RESULT,"^",1)
        ;
        IF RESULT'="" DO
        . NEW PatIEN SET PatIEN=+$ORDER(^XPD(9.7,"B",RESULT,""))
        . IF PatIEN=0 QUIT
        . NEW SEQ SET SEQ=$$GET1^DIQ(9.7,PatIEN_",",42001)
        . IF SEQ="" QUIT
        . SET RESULT=RESULT_" SEQ #"_SEQ
        ;
GLIDONE QUIT RESULT
        ;
GETVERS(PCKINIT,VERARRAY)
        ;"Purpose: for given package initials, return possible versions
        ;"Input: PCKINIT - the package initials, e.g. 'DI' in the case of Fileman
        ;"       VERARRAY -- PASS BY REFERENCE -- an OUT PARAMETER.  Format
        ;"              VERARRAY("22")=""
        ;"              VERARRAY("23")=""
        ;"Results: nONE
        NEW VER
        KILL VERARRAY
        IF $GET(PCKINIT)="" GOTO GVDONE
        NEW DONE SET DONE=0
        NEW IEN9D4 SET IEN9D4=+$ORDER(^DIC(9.4,"C",PCKINIT,"")) GOTO:(IEN9D4'>0) GVDONE
        SET VER=""
        FOR  SET VER=$ORDER(^DIC(9.4,IEN9D4,22,"B",VER)) QUIT:(VER="")!DONE  DO
        . IF VER'="" SET VERARRAY(VER)=""
GVDONE  QUIT
        ;
PICKFILE(INFO,PARRAY)
        ;"Purpose: Pick a filename from a list, in a menu
        ;"Input: INFO. PASS BY REFERENCE.  Results are put into this. 
        ;"       PARRAY -- PASS BY NAME.  format:
        ;"        @PARRAY@(NAME)=""
        ;"        @PARRAY@(NAME)=""
        ;"Result: the chosen name, or "" IF problem.
        NEW RESULT
        ;"NEW MENU SET MENU(0)="Select IF ONE of the following files is the patch file."
        NEW MENU SET MENU(0)="Select option below for patch file."
        NEW COUNT SET COUNT=1
        NEW NAME SET NAME=""
        FOR  SET NAME=$ORDER(@PARRAY@(NAME)) QUIT:(NAME="")  DO
        . SET MENU(COUNT)=NAME_$C(9)_NAME,COUNT=COUNT+1
        SET MENU(COUNT)="BROWSE server for different file name"_$C(9)_"BROWSE",COUNT=COUNT+1
        SET MENU(COUNT)="Cancel"_$C(9)_"@",COUNT=COUNT+1
        SET RESULT=$$MENU^TMGUSRI2(.MENU)
        IF RESULT="BROWSE" DO  GOTO PFDN
        . NEW TEXTURL SET TEXTURL=$GET(INFO("TEXT URL"))
        . NEW SERVERPATH DO SPLITFPN^TMGIOUTL(TEXTURL,.SERVERPATH)
        . IF $$URLPICK(.INFO,SERVERPATH)  ;"IGNORE RESULT
        . SET RESULT=$GET(INFO("KID FILE"))
        IF "@^"[RESULT SET RESULT=""
        SET INFO("KID FILE")=RESULT
PFDN    QUIT RESULT
        ;
URLPICK(INFO,STARTWEBPATH)  ;
        ;"Input: INFO: PASS BY REFERENCE.  Results are put into this.
        ;"       STARTWEBPATH: OPTIONAL.  If provided, then used as starting path on web server.  
        ;"Result: 1^OK, or -1^Message
        NEW PATHBASE SET PATCHBASE=$GET(INFO("KID URL BASE PATH"))
        IF PATCHBASE="" SET PATCHBASE=$$PATCHBASE^TMGKERN4()
        SET STARTWEBPATH=$GET(STARTWEBPATH,PATCHBASE)
        NEW RESULT SET RESULT="1^OK"
        NEW ABORT SET ABORT=0
        NEW WEBPATH,WEBFNAME
        NEW FNAME SET FNAME=$GET(INFO("KID FILE"),"[["_$GET(INFO("PATCH NAME"))_"]]")
        NEW OPTION
        SET OPTION("HEADER MSG",1)="Please search for {{HIGH}}"_FNAME_"{{NORM}}"
        SET OPTION("URL")=STARTWEBPATH
        SET OPTION("SELECT DIR")=0
        NEW USRPICK SET USRPICK=$$FBROWSE^TMGIOUT2(.OPTION,.WEBPATH,.WEBFNAME)
        SET WEBFNAME=$GET(WEBFNAME) 
        SET WEBPATH=$GET(WEBPATH)
        IF WEBFNAME'=FNAME,WEBFNAME'="" DO  GOTO:ABORT URLPDN
        . WRITE !,"Download and use: ",!
        . WRITE "  ",WEBFNAME,!
        . WRITE "instead of: ",!
        . WRITE "  ",FNAME,!
        . WRITE "OK "
        . SET %=1 DO YN^DICN WRITE !
        . IF %=-1 SET ABORT=1,RESULT="-1^User abort" QUIT
        . IF %=2 SET ABORT=1,RESULT="-1^Wrong file" QUIT
        SET INFO("KID FILE")=WEBFNAME
        SET INFO("KID URL")=USRPICK
        SET INFO("KID URL BASEPATH")=WEBPATH
        IF WEBFNAME="" SET RESULT="-1^No file"
URLPDN  QUIT RESULT
        ;
FIXPATCH ;
        ;"Purpose: This fixes situation when a patch is applied without first
        ;"  changing the $ZRO
        ;"  This will chagned the routines into the /r folder (backing up prior
        ;"    files if they exist)
        DO FIXPATCH^TMGPAT5
        QUIT
        ;
SETZRO1 ;"Restore $ZRO from save
        ;"NOTE: This function is for an Astronaut VistA layout (e.g. has /p and /r routines), on UNIX
        NEW REF SET REF=$NAME(^TMG("TMP",$J))
        IF $DATA(@REF@("SAVED $ZRO")) DO SETZRO1X(REF) ;"Restore $ZRO from save
        QUIT
        ;
SETZRO1V ;"Restore $ZRO from save
        ;"NOTE: this function is for use with normal VA KIDS patching menu, not the TMG one.
        ;"NOTE: This function is for an Astronaut VistA layout (e.g. has /p and /r routines), on UNIX
        NEW REF SET REF=$NAME(^TMG("TMP",$J,"VA"))
        DO SETZRO1X(REF) ;"Restore $ZRO from save
        QUIT
        ;
SETZRO1X(ROOT) ;"Restore $ZRO from save 
        ;"NOTE: This function is for an Astronaut VistA layout (e.g. has /p and /r routines), on UNIX
        NEW ZRO,ZPROMPT
        SET ZRO=$GET(@ROOT@("SAVED $ZRO"))
        KILL @ROOT@("SAVED $ZRO")
        IF ZRO="" DO  GOTO SZ1DN
        . WRITE !,"ERROR: called SETZRO1X^TMGPAT1, but no saved $ZRO found.",!
        . WRITE "Fix this by exiting this GT.M instance and re-run.",!
        . DO PRESS2GO^TMGUSRI2
        SET $ZROUTINES=ZRO
        SET ZPROMPT=$GET(@ROOT@("SAVED $ZPROMPT"))
        KILL @ROOT@("SAVED $ZPROMPT")
        IF ZPROMPT="" DO  GOTO SZ1DN
        . WRITE !,"ERROR: called SETZRO1X^TMGPAT1, but no saved $ZPROMPT found.",!
        . WRITE "Fix this by exiting this GT.M instance and re-run.",!
        . DO PRESS2GO^TMGUSRI2
        SET $ZPROMPT=ZPROMPT
        WRITE !,"----------------------------------------------------",!
        WRITE "$ZRO restored to:",!
        WRITE ZRO,!
        WRITE "----------------------------------------------------",!
        DO PRESS2GO^TMGUSRI2
SZ1DN   QUIT
        ;
SETZRO2 ;"Save ZRO, and set $ZRO to value for use with patching (remove "/p" directory)
        ;"NOTE: This function is for an Astronaut VistA layout (e.g. has /p and /r routines), on UNIX
        NEW % SET %=1
        NEW REF SET REF=$NAME(^TMG("TMP",$J))
        WRITE !,"Change $ZRO to correct folder for patching" DO YN^DICN WRITE !
        IF %=1 DO
        . DO SETZRO2X(REF)
        ELSE  DO
        . KILL @REF@("SAVED $ZRO")
        QUIT
        ;
SETZRO2V ;"Save ZRO, and set $ZRO to value for use with patching (remove "/p" directory)
        ;"NOTE: This function is for an Astronaut VistA layout (e.g. has /p and /r routines), on UNIX
        NEW REF SET REF=$NAME(^TMG("TMP",$J,"VA"))
        DO SETZRO2X(REF)
        QUIT
        ;
SETZRO2X(ROOT) ;"Save ZRO, and set $ZRO to value for use with patching (remove "/p" directory)
        ;"NOTE: This function is for an Astronaut VistA layout (e.g. has /p and /r routines), on UNIX
        NEW ZRO SET ZRO=$ZROUTINES
        SET @ROOT@("SAVED $ZRO")=ZRO
        SET @ROOT@("SAVED $ZPROMPT")=$ZPROMPT
        SET $ZPROMPT="PATCHING ONLY!>"
        NEW ARR DO PARSEZRO(ZRO,.ARR)
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:+IDX'>0  DO
        . NEW JDX SET JDX=0
        . FOR  SET JDX=$ORDER(ARR(IDX,"SRC",JDX)) QUIT:+JDX'>0  DO
        . . NEW SRCPATH SET SRCPATH=$GET(ARR(IDX,"SRC",JDX)) QUIT:SRCPATH=""
        . . NEW LEN SET LEN=$LENGTH(SRCPATH)
        . . IF $EXTRACT(SRCPATH,LEN-1,LEN)'="/p" QUIT   
        . . KILL ARR(IDX,"SRC",JDX)   ;"remove the /p directory
        SET ZRO=$$BUILDZRO(.ARR)
        ;"NEW STRA,STRB
        ;"SET STRA=$PIECE(ZRO,")",1)
        ;"SET STRB=$PIECE(ZRO,") ",2)
        ;"NEW OBJPATH SET OBJPATH=$PIECE(STRA,"(",1)
        ;"NEW SRCPATH SET SRCPATH=$PIECE(STRA,"(",2)
        ;"NEW SPATH2
        ;"NEW IDX FOR IDX=1:1 QUIT:(IDX>$LENGTH(SRCPATH," "))  DO
        ;". NEW APATH SET APATH=$PIECE(SRCPATH," ",IDX)
        ;". NEW L SET L=$LENGTH(APATH)
        ;". IF $EXTRACT(APATH,L-1,L)="/p" QUIT
        ;". SET SPATH2(IDX)=APATH
        ;"SET (IDX,SRCPATH)=""
        ;"FOR  SET IDX=$ORDER(SPATH2(IDX)) QUIT:IDX=""  DO
        ;". IF SRCPATH'="" SET SRCPATH=SRCPATH_" "
        ;". SET SRCPATH=SRCPATH_$GET(SPATH2(IDX))
        ;"SET ZRO=OBJPATH_"("_SRCPATH_") "_STRB
        SET $ZROUTINES=ZRO
        WRITE !,"----------------------------------------------------",!
        WRITE "NOTE: $ZRO appropriately altered for patching...",!
        WRITE "$ZRO=",ZRO,!
        WRITE "----------------------------------------------------",!
        DO PRESS2GO^TMGUSRI2
        QUIT        
        ;
PARSEZRO(ZRO,OUT)  ;"PARSE ZROUTINES STRING
        ;"Input: ZRO -- the string from $ZRO
        ;"       OUT -- PASS BY REFERENCE, AN OUT PARAMETER.
        ;"         OUT(#)=<object file directory>
        ;"         OUT(#,"SRC",1)=<1st source file directory>
        ;"         OUT(#,"SRC",2)=<2nd source file directory>
        ;"         OUT(#,"SRC",n)=<nth source file directory>
        KILL OUT
        NEW SPCTXT SET SPCTXT="{{^space^}}"
        SET ZRO=$$REPLSTR^TMGSTUT3(ZRO,"\ ",SPCTXT)  ;"protect escaped spaces, e.g. 'My\ Folder'
        NEW IDX SET IDX=0
        NEW DONE SET DONE=0
        FOR  QUIT:DONE!(ZRO="")  DO  
        . NEW OBJPATH SET OBJPATH=$$TRIM^XLFSTR($PIECE(ZRO,"(",1))
        . IF OBJPATH'="" SET IDX=IDX+1,OUT(IDX)=$$REPLSTR^TMGSTUT3(OBJPATH,SPCTXT,"\ ")
        . SET ZRO=$PIECE(ZRO,"(",2,9999)
        . NEW TEMP SET TEMP=$PIECE(ZRO,")",1)
        . SET ZRO=$$TRIM^XLFSTR($PIECE(ZRO,")",2,999))
        . NEW JDX 
        . FOR JDX=1:1:$LENGTH(TEMP," ") DO
        . . NEW APATH SET APATH=$$TRIM^XLFSTR($PIECE(TEMP," ",JDX))
        . . IF APATH="" QUIT
        . . SET OUT(IDX,"SRC",JDX)=$$REPLSTR^TMGSTUT3(APATH,SPCTXT,"\ ")
        QUIT
        ;
BUILDZRO(ARR)  ;"Build a ZRO string from array
       ;"Input: ARR -- PASS BY REFERENCE.  Array, as made by PARSEZRO
       ;"Result: OUTPUT IS THE STRING
       NEW RESULT SET RESULT=""
       NEW IDX SET IDX=0
       FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:+IDX'>0  DO
       . IF RESULT'="" SET RESULT=RESULT_" "
       . NEW OBJPATH SET OBJPATH=$GET(ARR(IDX)) QUIT:OBJPATH=""
       . SET RESULT=RESULT_OBJPATH
       . IF $DATA(ARR(IDX,"SRC"))'>0 QUIT
       . SET RESULT=RESULT_"("
       . NEW JDX SET JDX=0
       . FOR  SET JDX=$ORDER(ARR(IDX,"SRC",JDX)) QUIT:+JDX'>0  DO
       . . IF $EXTRACT(RESULT,$LENGTH(RESULT))'="(" SET RESULT=RESULT_" "
       . . NEW SRCPATH SET SRCPATH=$GET(ARR(IDX,"SRC",JDX)) QUIT:SRCPATH=""
       . . SET RESULT=RESULT_SRCPATH
       . SET RESULT=RESULT_")"
       QUIT RESULT
       ;