TMGXPDI ;TMG/kst/Custom version of XPDIL1 ;09/17/08, 2/2/14
         ;;1.0;TMG-LIB;**1**;09/17/08
 ;"Original header....
 ;"XPDI    ;SFISC/RSD - Install Process ;9/16/02  13:29
 ;"       ;;8.0;KERNEL;**10,21,39,41,44,58,68,108,145,184,229**;Jul 10, 1995
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
SCRN()
        NEW result SET result=0
        WRITE "#",Y,": ",$P(^(0),U,1)
        WRITE " STATUS="
        NEW s SET s=$P(^(0),U,9)
        WRITE $s(s=0:"Loaded",s=1:"Queued",s=2:"Started",s=3:"completed",s=4:"de-installed",1:"??"),!
        I $P(^(0),U,9) GOTO SDone
        WRITE "INSTALL ORDER=",$P(^(0),U,5)," STARTING PACKAGE=",$P(^(0),U,4),!
        WRITE "^XPD(9.7,""ASP"",Y,*): ",!
        IF $DATA(^XPD(9.7,"ASP",Y)) DO ZWRITE^TMGZWR($NAME(^XPD(9.7,"ASP",Y)))
        ELSE  WRITE "(empty)",!
        I '$D(^XPD(9.7,"ASP",Y,1,Y)) GOTO SDone
        WRITE "^XTMP(""XPDI"",Y):",!
        IF $DATA(^XTMP("XPDI",Y)) DO ZWRITE^TMGZWR($NAME(^XTMP("XPDI",Y)))
        ELSE  WRITE "(empty)",!
        I '$D(^XTMP("XPDI",Y)) GOTO SDone
        WRITE "SUCCESS!!",!
        SET result=1
SDone
        QUIT result


EN(InstallName,Option,Msg)
        ;"Purpose: Install an entry from file 9.7 INSTALL
        ;"Input: InstallName -- Name of package to install (or the install name the user was told to use.)
        ;"       Option -- PASS BY REFERENCE.  An array filled with answers to user interaction questions.
        ;"                  Option(QuestionText)=1 for YES, 0 for NO
        ;"                  Option(QuestionText,"DEFAULT")=default user answer
        ;"       Msg -- PASS BY REFERANCE, an OUT PARAMETER
        ;"              Errors are stored in Msg("ERROR",x)=Message
        ;"                                   Msg("ERROR")=count of last error
        ;"              Message are store in Msg(x)=Message
        ;"                                   Msg=count of last message+1

        N DIR,DIRUT,POP,XPD,XPDA,XPDD,XPDIJ,XPDDIQ,XPDIT,XPDIABT
        N XPDNM,XPDNOQUE,XPDPKG,XPDREQAB,XPDST,XPDSET,XPDSET1,XPDT
        N XPDQUIT,XPDQUES,Y,ZTSK,%

        S %="I '$P(^(0),U,9),$D(^XPD(9.7,""ASP"",Y,1,Y)),$D(^XTMP(""XPDI"",Y))"
        ;"S %="new temp SET temp=$$SCRN^TMGXPDI() w ""temp="",temp,! IF temp"
        S XPDST=$$LOOK^TMGXPDI1(%,,.InstallName,.Msg)  ;"returns IEN or 0 IF fail
        I 'XPDST!$D(XPDQUIT) Q
        S XPDIT=0
        S (XPDSET,XPDSET1)=$P(^XPD(9.7,XPDST,0),U)
        K ^TMP($J)

        ;"Check each part of XPDT array
        F  S XPDIT=$O(XPDT(XPDIT)) Q:'XPDIT  D  Q:'$D(XPDT)!$D(XPDQUIT)
        . S XPDA=+XPDT(XPDIT)
        . S XPDNM=$P(XPDT(XPDIT),U,2)
        . S XPDPKG=+$P($G(^XPD(9.7,+XPDT(XPDIT),0)),U,2)
        . S %=$P(^(0),U,5)
        . DO AddMsg^TMGPAT2("Checking Install for Package "_XPDNM,0,.Msg)
        . ;"Check that Install file was created correctly
        . I '$D(^XPD(9.7,XPDA,"INI"))!'$D(^("INIT")) DO  Q
        . . DO AddMsg^TMGPAT2("**INSTALL FILE IS CORRUPTED**",1,.Msg)
        . . S XPDQUIT=1
        . ;"Run enviroment check routine
        . ;"XPDREQAB req. build missing, =2 global killed
        . I $$ENV^TMGXPDL1(1) S:$G(XPDREQAB)=2 XPDQUIT=1 Q
        . ;"save variables that are setup in environ. chck. routine
        . I $D(XPDNOQUE)!$D(XPDDIQ) D
        . . I $D(XPDNOQUE) S ^XTMP("XPDI",XPDA,"ENVVAR","XPDNOQUE")=XPDNOQUE
        . . I $D(XPDDIQ) M ^XTMP("XPDI",XPDA,"ENVVAR","XPDDIQ")=XPDDIQ
        . D QUES^TMGXPDI1(XPDA,.Option)
        . Q:'$D(XPDT(+XPDIT))!$D(XPDQUIT)
        . ;"XPDIJ=XPDA IF XPDIJ routine is part of Build
        . I $D(^XTMP("XPDI",XPDA,"RTN","XPDIJ")) S XPDIJ=XPDA
        . D XQSET^TMGXPDI1(XPDA)

        ;"NONE = no Build to install
        G NONE:'$O(XPDT(""))!$D(XPDQUIT)!($G(XPDREQAB))
        ;"check that we have all Builds to install
        S XPDA=XPDST,XPDNM=XPDSET,Y=0
        F  S Y=$O(^XPD(9.7,"ASP",XPDA,Y)) Q:'Y  S %=+$O(^(Y,0)) I '$D(XPDT("DA",%)) G NONE
        DO AddMsg^TMGPAT2(" ",0,.Msg)
        ;"See IF a Master Build
        S %=$O(^XTMP("XPDI",XPDA,"BLD",0))
        S %=$P(^(%,0),U,3)
        I %=1 S XPDT("MASTER")=XPDA

        ;"Inhibit Logon Question
        D DIR^TMGXPDIQ("XPI",,.Option) I $D(DIRUT) D ABRTALL(2) Q
        ;"disable options question
        D DIR^TMGXPDIQ("XPZ",,.Option) I $D(DIRUT) D ABRTALL(2) Q
        ;"XPDSET=set name,(also build name), of options that will be disabled
        ;"XPDSET1=setname or null IF they don't want to disable
        D  I XPDSET1="^" D ABRTALL(2) Q
        . ;if they say no, SET XPDET1=""
        . S:'$G(XPDQUES("XPZ1")) XPDSET1="",Y=0
        . S ^XTMP("XQOO",XPDSET,0)=XPDSET_" is being installed by KIDS^"_DT_U_DUZ
        . I XPDSET1]"" D  Q:XPDSET1="^"!(XPDSET1="")
        . . ;"MERGE the options/protocols that were put in ^TMP($J,"XQOO",build name)
        . . M ^XTMP("XQOO",XPDSET)=^TMP($J,"XQOO",XPDSET)
        . . D INIT^XQOO(.XPDSET1) Q:"^"[XPDSET1
        . . N DIR S DIR(0)="N^0:60:0",DIR("B")=0
        . . S DIR("A")="Delay Install (Minutes)"
        . . S DIR("?")="Enter the number of minutes to delay the installing of Routines after the Disable of Options"
        . . W ! D ^DIR
        . . I $D(DIRUT) S XPDSET1="^"
        . ;"Y is SET in the call to DIR in previous .DO
        . ;"save setname into first Build and the Delay in minutes, Y
        . K XPD
        . S XPD(9.7,XPDST_",",7)=(XPDSET1]"")_XPDSET
        . S XPD(9.7,XPDST_",",8)=Y
        . D FILE^DIE("","XPD")

        ;"check IF they want to update other CPUs
        I $G(XPDQUES("XPZ2")) D  I $D(DIRUT) D ABRTALL(2) Q
        . N DA,DIE,DIR,DR,I,XPD,X,Y,Z
        . ;"if they haven't already added Volume Sets, populate the mulitple
        . I '$O(^XPD(9.7,XPDA,"VOL",0)) D  I $D(XPD) D UPDATE^DIE("","XPD")
        . . X ^%ZOSF("UCI") S Y=$P(Y,",",2),(I,Z)=0
        . . F  S I=$O(^%ZIS(14.5,I)) Q:'I  do
        . . . S X=$G(^(I,0))
        . . . IF $P(X,U)]""&$P(X,U,11)&($P(X,U)'=Y) do
        . . . . SET Z=Z+1
        . . . . SET XPD(9.703,"+"_Z_","_XPDA_",",.01)=$P(X,U)
        . W !!,"I will Update the following VOLUME SETS:",!
        . S I=0 F  S I=$O(^XPD(9.7,XPDA,"VOL",I)) Q:'I  W ?3,$P(^(I,0),U),!
        . W !
        . S DIR(0)="Y",DIR("A")="Want to edit this list",DIR("B")="NO"
        . D ^DIR
        . I $D(DIRUT) Q
        . IF Y DO
        . . S DA=XPDA,DIE="^XPD(9.7,",DR=30,DR(2,9.703)=".01"
        . . D ^DIE
        . I '$O(^XPD(9.7,XPDA,"VOL",0)) W !!,"No VOLUME SETS selected!!" Q
        . I $$TM^%ZTLOAD Q  ;"QUIT IF Taskman is running
        . W !!,"TASKMAN is not running. If you install now, you must run the routine XPDCPU",!
        . W "in the production UCI for each of the VOLUME SETS you have listed once",!
        . W "the installation starts!!",!
        . W "If you Queue the install, the VOLUME SETS will be updated automatically.",*7,*7,!!

DEV     S POP=0
        I '$D(^DD(3.5,0)) S POP=1
        ;"check IF home device is defined
        I 'POP S IOP="",%ZIS=0 D ^%ZIS
        ;"Kernel Virgin Install
        I POP DO  G EN^XPDIJ
        . S XPDA=XPDST
        . I $G(XPDIJ) D XPDIJ^XPDI1

        ;"set XPDA=starting Build, ask for device for messages
        ;"XPDNOQUE is defined means don't let them queue output
        W !!,"Enter the Device you want to print the Install messages."
        W:'$D(XPDNOQUE) !,"You can queue the install by enter a 'Q' at the device prompt."
        W !,"Enter a '^' to abort the install.",!
        S XPDA=XPDST
        S %ZIS=$P("Q",U,'$D(XPDNOQUE))
        D ^%ZIS
        I POP G ASKABRT

        ;"reset expiration date to T+7 on transport global
        S XPDD=$$FMADD^XLFDT(DT,7)
        S ^XTMP("XPDI",0)=XPDD_U_DT
        I $D(IO("Q")) D  G ASKABRT:$D(ZTSK)[0 D XPDIJ^XPDI1:$G(XPDIJ),QUIT^XPDI1(XPDST) Q
        . N DIR,NOW S NOW=$$HTFM^XLFDT($$HADD^XLFDT($H,,,2)) ;Must be in future
        . S DIR(0)="DA^"_NOW_":"_XPDD_":AEFRSX"
        . S DIR("A")="Request Start Time: "
        . S DIR("B")=$$FMTE^XLFDT(NOW)
        . S DIR("?",1)="Enter a Date including Time"
        . S DIR("?",2)="The time must be in the future and not to exceed 7 days in the future."
        . S DIR("?")="Current date/time: "_DIR("B")
        . D ^DIR
        . Q:$D(DIRUT)
        . S ZTDTH=Y,ZTRTN="EN^XPDIJ",ZTDESC=XPDNM_" KIDS install",ZTSAVE("XPDA")=""
        . D ^%ZTLOAD,HOME^%ZIS K IO("Q")
        . Q:$D(ZTSK)[0
        . W !,"Install Queued!",!!
        . ;"save task into first Build
        . K XPD S XPD(9.7,XPDST_",",5)=ZTSK,XPDIT=0
        . F  S XPDIT=$O(XPDT(XPDIT)) Q:'XPDIT  D
        . . S XPD(9.7,+XPDT(XPDIT)_",",.02)=1
        . . D FILE^DIE("","XPD")
        . . K XPD

        ;"run install
        U IO
        I $G(XPDIJ) D XPDIJ^XPDI1
        D QUIT^XPDI1(XPDST)
        G EN^XPDIJ
        Q


ABORT(XPDA,XPDK,XPDALL,Msg)
        ;"Purpose: Abort install of Build XPDA
        ;
        ;"XPDA=ien to del, XPDK=1 KILL global, XPDALL=1 deleting all
        ;"XPDST=starting package.

        N %,DA,DIK,XPDJ,XPDNM,Y
        Q:'$D(^XPD(9.7,XPDA,0))  S XPDNM=$P(^(0),U)
        D BMES^TMGXPDUT(XPDNM_" Build will not be installed"_$S(XPDK=1:", Transport Global deleted!",1:""))
        DO MES^TMGXPDUT("               "_$$HTE^XLFDT($H))
        S DIK="^XPD(9.7,",XPDJ=XPDT("NM",XPDNM),DA=XPDA
        ;kill XPDT array, but don't KILL global IF XPDK=2
        K XPDT("NM",XPDNM),XPDT("DA",XPDA),XPDT(XPDJ),XPDT("GP") Q:XPDK=2
        K ^XTMP("XPDI",XPDA)
        ;if we are not deleting all packages and we are deleting the starting package
        ;set the next package to the starting package. It must always be 1.
        I '$G(XPDALL),XPDA=XPDST S Y=$O(XPDT(0)) D:Y
        .;unlock starting install
        .L -^XPD(9.7,XPDST)
        .S XPDST=+XPDT(Y),XPDT(1)=XPDT(Y),XPDT("DA",XPDST)=1,XPDT("NM",$P(XPDT(Y),U,2))=1,XPDIT=0
        .K XPDT(Y) N XPD
        .S %="XPD(9.7,"""_XPDST_","")",@%@(3)=XPDST,@%@(4)=1
        .;loop thru the rest of the packages and reset the starting package field
        .F  S Y=$O(XPDT(Y)) Q:'Y  D
        ..S XPD(9.7,+XPDT(Y)_",",3)=XPDST
        .D FILE^DIE("","XPD")
        D ^DIK
        Q

ASKABRT ;ask IF want to unload distribution
        N DIR,DIRUT,X,Y
        S XPDQUIT=1,DIR(0)="Y",DIR("A")="Install ABORTED, Want to remove the Transport Globals",DIR("B")="YES"
        W ! D ^DIR I Y D ABRTALL(1) Q
        L -^XPD(9.7,XPDST)
        Q

ABRTALL(XPDK,Option,Msg)
        ;"Purpose: Abort all Builds
        ;"Input: Option -- PASS BY REFERENCE.  Entries are required unless marked optional
        ;"       Msg -- PASS BY REFERENCE, an OUT PARAMETER.
        ;"              Message are store in Msg(x)=Message
        ;"                                   Msg=count of last message+1

        N XPDA
        S XPDT=0
        F  S XPDT=$O(XPDT(XPDT)) Q:'XPDT  do
        . S XPDA=+XPDT(XPDT)
        . D ABORT(XPDA,XPDK,1,.Msg)
        ;"Unlock starting install
        L -^XPD(9.7,XPDST)
        Q

NONE    W !!,"**NOTHING INSTALLED**",!
        Q
