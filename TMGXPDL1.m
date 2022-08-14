TMGXPDL1 ;TMG/kst/Custom version of XPDIL1 ;09/17/08, 2/2/14
         ;;1.0;TMG-LIB;**1**;09/17/08
 ;"Original header....
 ;"XPDIL1  ;SFISC/RSD - cont. of load Distribution Global ;11/14/2002  07:35
 ;"       ;;8.0;KERNEL;**15,17,39,41,44,66,68,76,85,100,108,229**;Jul 10, 1995
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
PKG(XPDA,Option,Msg)
        ;"Purspose: Check Package file
        ;"Input: Options -- PASS BY REFERENCE.  Entries are required unless marked optional
        ;"              Option("FORCE CONT LOAD")=1 <-- IF not given, then load won't continue
        ;"       Msg -- PASS BY REFERENCE, an OUT PARAMETER.
        ;"              Errors are stored in Msg("ERROR",x)=Message
        ;"                                   Msg("ERROR")=count of last error
        ;"              Message are store in Msg(x)=Message
        ;"                                   Msg=count of last message+1

        N XPD,XPDCP,XPDNM,XPDNOQUE,XPDPKG,X,Y,%
        NEW abort SET abort=0
        S XPDNM=$P(XPDT(XPDIT),U,2)
        DO ADDMSG^TMGPAT2("   "_XPDNM,0,.Msg)

        ;"check KIDS version against sites version, skip IF package is Kernel
        I $$PKG^TMGXPDUT(XPDNM)'["KERNEL" D  ;"not interactive routine
        . ;"this is part of a Kernel multi package
        . Q:$O(XPDT("NM","KERNEL"))["KERNEL"
        . S Y=$G(^XTMP("XPDI",XPDA,"VER"))
        . I $$VERSION^TMGXPDUT("XU")<Y do
        . . DO ADDMSG^TMGPAT2("Need Version "_+Y_" of KERNEL!",1,.Msg)
        . . S XPDQUIT=1
        . I $$VERSION^TMGXPDUT("VA FILEMAN")<$P(Y,U,2) do
        . . DO ADDMSG^TMGPAT2("Need Version "_+$P(Y,U,2)_" of VA FILEMAN!",1,.Msg)
        . . S XPDQUIT=1
        I $D(XPDQUIT) SET abort=1 GOTO PCKDone

        ;"get national package name
        S %=$O(^XTMP("XPDI",XPDA,"PKG",0))
        SET XPDPKG(0)=$G(^(+%,0))
        SET XPDPKG=%
        ;"XPDPKG=new ien^old ien
        I XPDPKG D  S XPDPKG=+Y_U_XPDPKG
        . N D,DIC
        . S DIC="^DIC(9.4,",DIC(0)="X",X=$P(XPDPKG(0),U)
        . D ^DIC Q:Y>0
        . ;"if lookup fails try Prefix, C x-ref
        . S X=$P(XPDPKG(0),U,2),D="C"
        . D IX^DIC

        ;"add package to Install file
        I XPDPKG>0 do
        . S XPD(9.7,XPDA_",",1)=+XPDPKG
        . D FILE^DIE("","XPD")

        ;"XPDSKPE= does site want to run Environ. Check
        I '$G(XPDSKPE),($$ENV(0,.Msg)=1) GOTO PCKDone

        ;"global package can't have pre or post inits
        IF $D(XPDGP) GOTO PCKDone

        ;"create pre-init checkpoint
        S XPDCP="INI"
        I '$$NEWCP^TMGXPDUT("XPD PREINSTALL COMPLETED") SET abort=1 GOTO PCKDone
        S %=$$INRTN("INI")

        ;"check for routine, use as call back
        I $L(%),'$$NEWCP^TMGXPDUT("XPD PREINSTALL STARTED",%) SET abort=1 GOTO PCKDone

        ;"create post-init checkpoint
        S XPDCP="INIT"
        I '$$NEWCP^TMGXPDUT("XPD POSTINSTALL COMPLETED") SET abort=1 GOTO PCKDone
        S %=$$INRTN("INIT")

        I $L(%),'$$NEWCP^TMGXPDUT("XPD POSTINSTALL STARTED",%) SET abort=1 GOTO PCKDone
        ;"create fileman and components check points and file rest of data
        DO XPCK^XPDIK("FIA")
        DO XPCK^XPDIK("KRN")
PCKDone
        IF abort=1 do
        . DO ADDMSG^TMGPAT2("Aborting",1,.Msg)
        . DO ABORT^TMGXPDI(XPDA,1,,.Msg)

        Q

INST(XPDNM,Option,Msg)
        ;"Purpose: add to Install file
        ;"Input: XPDNM -- Name to match agains .01 field from file 9.7
        ;"       Options -- PASS BY REFERENCE.  Entries are required unless marked optional
        ;"              Option("FORCE CONT LOAD")=1 <-- IF not given, then load won't continue
        ;"                                              when question normally asked of  user.
        ;"       Msg -- PASS BY REFERENCE, an OUT PARAMETER.
        ;"              Errors are stored in Msg("ERROR",x)=Message
        ;"                                   Msg("ERROR")=count of last error
        ;"              Message are store in Msg(x)=Message
        ;"                                   Msg=count of last message+1
        ;"Output:
        ;"Result: 0=error, or IEN in Install File (9.7) of newly added entry

        N %X,DIC,DIR,DIRUT,DLAYGO,X,XPD,XPDA,XPDIE,XPDDIQ,Y,SH

        ;"check IF Build was already installed
        ;"XPD=0 abort install, ELSE XPD=ien in Install file
        I $D(^XPD(9.7,"B",XPDNM)) do
        . NEW IEN SET IEN=$ORDER(^XPD(9.7,"B",XPDNM,"")) ;"//kt added
        . ;"set XPDQUIT=1  ;"//kt added
        . S (SH,Y)=0
        . DO ADDMSG^TMGPAT2("Build "_XPDNM_" has been loaded before [IEN #"_IEN_" in File INSTALL (9.7)]",0,.Msg)
        . ;"do ADDMSG^TMGPAT2("Here is when: ",0,.Msg)
        . F  S Y=$O(^XPD(9.7,"B",XPDNM,Y)) Q:'Y  D
        . . Q:'$D(^XPD(9.7,Y,0))  S %=^(0)
        . . ;"do ADDMSG^TMGPAT2("   "_$P(%,U),0,.Msg)
        . . I $P(%,U,9)<3,$D(^XTMP("XPDI",Y)) DO  QUIT
        . . . DO ADDMSG^TMGPAT2("   **Transport Global already exists**",0,.Msg)
        . . . S XPD=0
        . . S %X=$X
        . . DO ADDMSG^TMGPAT2("   "_$$EXTERNAL^DILFD(9.7,.02,"",$P(%,U,9)),0,.Msg)
        . . DO ADDMSG^TMGPAT2("   "_$P(%,U)_" was loaded on "_$$FMTE^XLFDT($P($G(^XPD(9.7,Y,1)),U)),0,.Msg)
        . Q:$D(XPD)  ;"QUIT IF transport global exist
        . SET XPD=0 ;"signal QUIT -- //kt added
        IF $D(XPD) SET XPDA=XPD GOTO INSTDone

        ;"Add to Install file, must be new
        S DIC="^XPD(9.7,",DIC(0)="XL",DLAYGO=9.7,X=""""_XPDNM_""""
        D ^DIC
        I Y<0 DO  GOTO INSTDone
        . S SH=0
        . DO ADDMSG^TMGPAT2("Can't ADD Build "_XPDNM_" to Install File",1,.Msg)
        . ;"do ADDMSG^TMGPAT2($PIECE(Y,"^",2)_" already exists in INSTALLATION file (9.7), IEN=#"_+Y,1,.Msg)
        . SET XPDA=0

        ;"set starting package to Y, IF it is not already defined
        S:'XPDST XPDST=+Y
        ;"XPDT array keeps track of all packages in this distribution
        S XPDA=+Y
        SET XPDT(XPDIT)=XPDA_U_XPDNM
        SET XPDT("DA",XPDA)=XPDIT
        SET XPDT("NM",XPDNM)=XPDIT
        S %="XPDIE(9.7,"""_XPDA_","")"
        SET @%@(.02)=0          ;"STATUS
        SET @%@(2)=$$NOW^XLFDT  ;"DATE LOADED
        SET @%@(3)=XPDST        ;"STARTING PACKAGE
        SET @%@(4)=XPDIT        ;"INSTALL ORDER
        SET @%@(5)=""           ;"QUEUED TASK NUMBER
        SET @%@(6)=XPDST("H1")  ;"FILE COMMENT
        NEW TMGMSG
        D FILE^DIE("","XPDIE","TMGMSG")
        I '$D(SH) DO  ;"SH is SET when some other part of INST shows the name
        . SET Msg(Msg)="   "_XPDNM,Msg=Msg+1
INSTDone
        Q XPDA



ENV(XPDENV,Msg)
        ;"Purpose: Enviroment check & version check
        ;"Input-- XPDENV 0=loading distribution, 1=installing
        ;"       Msg -- PASS BY REFERENCE, an OUT PARAMETER.
        ;"              Errors are stored in Msg("ERROR",x)=Message
        ;"                                   Msg("ERROR")=count of last error
        ;"              Message are store in Msg(x)=Message
        ;"                                   Msg=count of last message+1
        ;"Output: Globally scoped variables SET as follows:
        ;"      XPDQUIT QUIT current package install, 1=kill global, 2=leave global
        ;"      XPDQUIT(package) QUIT package install, 1=kill, 2=leave
        ;"      XPDABORT QUIT the entire distribution, 1=kill, 2=leave
        ;"Returns: 0=ok, 1=rejected KILL global, 2=rejected leave global

        N %,DIR,XPDI,XPDQUIT,XPDABORT,XPDDONE,XPDGREF,XPDMBREQ
        M X=DUZ
        N DUZ
        M DUZ=X
        S DUZ(0)="@" ;"See that ENV check has full FM priv.
        S XPDGREF="^XTMP(""XPDI"","_XPDA_",""TEMP"")"
        S XPDMBREQ=$G(^XTMP("XPDI",XPDA,"MBREQ"))
        S $P(^XPD(9.7,XPDA,0),U,7)=XPDMBREQ
        ;"check version number
        I XPDPKG>0 D  I $G(XPDQUIT) D ABORT^TMGXPDI(XPDA,1,,.Msg) Q 1
        . N DIR,DIRUT,X,Y
        . S %=+$$VER^TMGXPDUT(XPDNM)
        . S Y=+$G(^DIC(9.4,+XPDPKG,"VERSION"))
        . S X=XPDNM["*"
        . ;"If patch, version must be the same
        . I X,%'=Y do
        . . DO ADDMSG^TMGPAT2("This Patch is for Version "_%_", you are running Version "_Y,1,.Msg)
        . . S XPDQUIT=1
        . ;"if package, version must be greater or equal
        . I 'X,%<Y do
        . . DO ADDMSG^TMGPAT2("You have a Version greater than mine!",1,.Msg)
        . . S XPDQUIT=1
        . Q:'$G(XPDQUIT)
        . I $G(XPDMBREQ) D  Q
        . . D MES^TMGXPDUT("**ABORT** Required Build "_XPDNM_", did not pass internal KIDS checks!",.Msg)
        . . D ABRTALL^TMGXPDI(1,,.Msg)
        . . D NONE^TMGXPDI
        . . S XPDQUIT=0,XPDDONE=1
        . . Q
        . ;"NEED TO CHANGE BELOW IF GOING TO MAKE NON-INTERACTIVE...
        . S DIR(0)="Y",DIR("A")="Want to continue installing this build",DIR("B")="NO"
        . D ^DIR
        . I Y K XPDQUIT
        . Q
        Q:$G(XPDDONE) 1

        S %=$$REQB(.Msg)
        I % S (XPDABORT,XPDREQAB)=% G ABORT
PRE     ;        
        NEW ARTN S ARTN=$G(^XTMP("XPDI",XPDA,"PRE")) D:ARTN]""
        . DO ADDMSG^TMGPAT2("Will first run the Environment Check Routine, "_ARTN,0,.Msg)
        . D SAVE^XPDIJ(ARTN)
        . NEW saved,DEBUG SET DEBUG=0
        . IF DEBUG=0 DO IOCapON^TMGKERNL
        . D @("^"_ARTN)
        . IF DEBUG=0 DO IOCapOFF^TMGKERNL("saved")
        . IF $DATA(saved) DO ADDMSG^TMGPAT2(.saved,0,.Msg)


ABORT   I $G(XPDABORT) D  Q XPDABORT
        . ;"if during load & leave global QUIT
        . I 'XPDENV,XPDABORT=2 Q
        . D ABRTALL^TMGXPDI(XPDABORT,,.Msg)
        Q:'$D(XPDQUIT) 0
        I $G(XPDQUIT) D ABORT^TMGXPDI(XPDA,XPDQUIT,,.Msg)
        S XPDI=""

        ;"don't DO IF loading & leave global, need to keep XPDT(array)
        F  S XPDI=$O(XPDQUIT(XPDI)) Q:XPDI=""  D:'(XPDQUIT(XPDI)=2&'XPDENV)
        . S %=$G(XPDT("NM",XPDI))
        . D:% ABORT^TMGXPDI(+XPDT(%),XPDQUIT(XPDI),,.Msg)
        S XPDQUIT=$S($G(XPDQUIT):XPDQUIT,'$O(XPDT(0))!'$D(^XTMP("XPDI",XPDA)):1,1:0)
        Q XPDQUIT
        ;


REQB(Msg)
        ;"Purpose: check for Required Builds
        ;"Input: Msg -- PASS BY REFERENCE, an OUT PARAMETER.
        ;"              Errors are stored in Msg("ERROR",x)=Message
        ;"                                   Msg("ERROR")=count of last error
        ;"              Message are store in Msg(x)=Message
        ;"                                   Msg=count of last message+1
        ;"returns 0=ok, 1=failed KILL global, 2=failed leave global

        N XPDACT,XPDBLD,XPDI,XPDQ,XPDQUIT,XPDX,XPDX0,X,Y
        S XPDBLD=$O(^XTMP("XPDI",XPDA,"BLD",0)),XPDQUIT=0,XPDI=0
        Q:'$D(^XTMP("XPDI",XPDA,"BLD",XPDBLD,"REQB")) 0
        F  S XPDI=$O(^XTMP("XPDI",XPDA,"BLD",XPDBLD,"REQB",XPDI)) Q:'XPDI  D
        . S XPDX0=^(XPDI,0)
        . S XPDQ=0,XPDX=$P(XPDX0,U),XPDACT=$P(XPDX0,U,2)
        . S X=$$PKG^TMGXPDUT(XPDX)
        . S Y=$$VER^TMGXPDUT(XPDX)
        . S Z=$$VERSION^TMGXPDUT(X)
        .;"Quit IF current version is greater than what we are checking for
        . Q:Z>Y
        . I XPDX'["*" S:Z<Y XPDQ=2
        . E  S:'$$PATCH^TMGXPDUT(XPDX) XPDQ=1
        . ;"Quit IF patch is already on system
        . Q:'XPDQ
        . ;"QUIT IF patch is sequenced prior within this build
        . I $D(XPDT("NM",XPDX)),(XPDT("NM",XPDX)<XPDT("NM",XPDNM)) S XPDQ=0 Q
        . S XPDQUIT=$S(XPDACT>XPDQUIT:XPDACT,1:XPDQUIT)
        . ;"XPDACT=0 warning, =1 abort & KILL global, =2 abort
        . NEW s SET s=$S(XPDACT:"**INSTALL ABORTED**",1:"**WARNING**")_$S(XPDQ=1:" Patch ",1:" Package ")
        . SET s=s_XPDX_" is Required "_$S(XPDACT:"to install",1:"for")_" this package!!"
        . DO ADDMSG^TMGPAT2(s,1,.Msg)
        Q:'XPDQUIT 0
        ;"Don't DO IF leave global and loading
        D:'(XPDQUIT=2&'XPDENV) ABORT^TMGXPDI(XPDA,XPDQUIT,,.Msg)
        Q XPDQUIT
        ;

INRTN(X)
        ;"return a routine that can be run
        N Y
        S Y=$G(^XTMP("XPDI",XPDA,X)) Q:Y="" ""
        S Y=$S(Y["^":Y,1:"^"_Y)
        Q Y
