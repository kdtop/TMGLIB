TMGMDCPY ;ISC-SF/JLI - COPY ONE USER (PRIM & SEC MENUS, KEYS, FILES) TO ANOTHER USER ;01/23/96  11:18, 2/2/14
        ;;1.0;TMG-LIB;**1**;6/23/15
        ;;8.0;KERNEL;**19**;Jul 10, 1995
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
        ;"Copied from XWSMDCPY for decompilation and modification

        S XQBOSS=0
        I $D(^XUSEC("XUMGR",DUZ)) S XQBOSS=1
        I 'XQBOSS do
        . W !!,?5,"Note: You must have been delegated these options and",!
        . W ?11,"keys to transfer them from user to user.",!
        ;"I 'XQBOSS,$O(^VA(200,DUZ,19.5,0))'>0 W !!,$C(7),"No Menus have been delegated to you to use this option",!,"If there are questions see your site manager's staff." Q
        W !!

        S DIC("A")="Select the user to be COPIED FROM: "
        S DIC=200,DIC(0)="AQEM"
        D ^DIC Q:Y'>0  S XQUSR1=+Y

        S XQUSRPM=+$G(^VA(200,XQUSR1,201))
        I XQUSRPM="" S XQUSRPM=0 W !,"The source user has no primary menu."

        I 'XQBOSS,XQUSRPM>0,'$D(^VA(200,DUZ,19.5,"B",XQUSRPM)) do
        . W !,$C(7),"You are not able to give out this user's primary menu ",$P(^DIC(19,XQUSRPM,0),U)
        . S XQUSRPM=0

        S XQUSEC(0)=""
        F I=0:0 S I=$O(^VA(200,XQUSR1,203,I)) Q:I'>0  DO  ;"cycle through secondary menu options.
        . S X=^(I,0)
        . S XQUSEC(+X)=$P(X,U,2)
        . I 'XQBOSS,'$D(^VA(200,DUZ,19.5,"B",+X)) do
        . . W !,$C(7),"Skipping secondary menu ",$P(^DIC(19,+X,0),U)
        . . K XQUSEC(+X)
        ;"I XQUSRPM'>0,$O(XQUSEC(0))'>0 W !!,$C(7),"No Primary or Secondary Menus to copy -- QUITting.",!! G EXIT

        S XQUSEK(0)=""
        F I=0:0 S I=$O(^VA(200,XQUSR1,51,I)) Q:I'>0  DO  ;"cycle through keys
        . S X=+^(I,0)
        . I $D(^DIC(19,+X,0)) do
        . . S XQUSEK(X)=X
        . . I 'XQBOSS,'$D(^VA(200,DUZ,52,"B",X)) do
        . . . W !,$C(7),"Not authorized to give ",$P(^DIC(19.1,X,0),U)," key -- skipping"
        . . . K XQUSEK(X)
        ;
        ;Get recipient user
        ;
        S DIC("A")="Select a USER to be COPIED TO: ",DIC="^VA(200,",DIC(0)="AEMQ"
        F XQI=0:0 D ^DIC Q:Y'>0  S XUSR(+Y)="",DIC("A")="Select ANOTHER USER: "
        K DIC
        ;
        GOTO:$O(XUSR(0))'>0 EXIT

        R !!,"Do you want to QUEUE this job ?  Y// ",X:DTIME
        Q:'$T!(X[U)
        S:X="" X="Y"
        I "Yy"[$E(X) D TSK G EXIT
        ;
DQ      ;  ;"Optional entry point for tasked job
        F XQI=0:0 S XQI=$O(XUSR(XQI)) Q:XQI'>0  D COPY1  ;"Cycle through Dest users.

EXIT    ;
        K %,D,D0,DA,DI,DISYS,DIC,DIE,DR,X,XQBOSS,XQI,XQJ,XQUSEK,XQUSR1,XUSR,XQUSEC,XQUSRPM,Y
        Q
        ;" ---------- Actual work below ----------------
COPY1   I XQUSRPM>0 do
        . S DIE=200
        . S DA=XQI
        . S DR="201///"_$P(^DIC(19,XQUSRPM,0),U)
        . D ^DIE
        ;"XQI is dest user's IEN
        S:'$D(^VA(200,XQI,203,0)) ^(0)="^200.03P"
        S DLAYGO=200
        F XQJ=0:0 S XQJ=$O(XQUSEC(XQJ)) Q:XQJ'>0  do
        . S DIC="^VA(200,"_XQI_",203,"
        . S DA(1)=XQI
        . S DIC("P")=200.03
        . S X=$P(^DIC(19,XQJ,0),U)
        . S DIC(0)="ML"
        . D ^DIC
        . I Y>0,'$P(Y,U,3),XQUSEC(XQJ)'="" do
        . . S DIE=DIC
        . . S DIE("P")=200.03
        . . S DA=+Y
        . . S DR="2///"_XQUSEC(XQJ)_";"
        . . D ^DIE
        S:'$D(^VA(200,XQI,51,0)) ^(0)="^200.051P^"
        S (DA,DA(1))=XQI F XQJ=0:0 S XQJ=$O(XQUSEK(XQJ)) Q:XQJ'>0  do
        . S DIC="^VA(200,"_XQI_",51,"
        . S DIC("P")=200.051
        . S DIC(0)="ML"
        . S X=$P(^DIC(19.1,XQUSEK(XQJ),0),U)
        . D ^DIC
        K DLAYGO
        Q
        ;
TSK     S ZTRTN="DQ^XQSMDCPY"  ;"the entry point for the task.
        S ZTIO=""

        S ZTDESC="XQSMD Copy User"

        ;"Save pointer to source's primary menu (e.g. '215')
        S ZTSAVE("XQUSRPM")=""

        ;"Save list of secondary menus.  Format
        ;"      XQUSEC(MenuIEN)=Menu Synonym (piece 2) (or "")
        ;"      XQUSEC(MenuIEN)=Menu Synonym (piece 2) (or "")
        S ZTSAVE("XQUSEC(")=""

        ;"Save list of keys.  Format
        ;"      XQUSEK(KeyIEN)=KeyIEN
        S ZTSAVE("XQUSEK(")=""

        ;"Save destination users.  format:
        ;"      XUSR(+Y)=""
        ;"      XUSR(+Y)=""
        S ZTSAVE("XUSR(")=""

        D ^%ZTLOAD  ;"launch task.
        Q
