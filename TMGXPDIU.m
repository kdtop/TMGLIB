TMGXPDIU ;TMG/kst/Custom version of XPDIU ;09/24/08, 2/2/14
         ;;1.0;TMG-LIB;**1**;09/24/08
 ;"Original header....
 ;"XPDIU   ;SFISC/RSD - UNload/Convert/Rollup Distribution Global ;03/23/99  08:46
 ;"        ;;8.0;KERNEL;**15,41,44,51,58,101,108**;Jul 10, 1995
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
EN1(IEN)
        ;"Purpose: Unload a distribution
        ;"Input: IEN -- optional.  IEN of entry in INSTALL (9.7) file to delete.
        N %,DA,DIK,DIR,DIRUT,X,XPD,XPDST,XPDT,XPDQ,XPDQUIT,Y
        ;"Remove dangling transport globals
        S DA=0
        F  S DA=$O(^XTMP("XPDI",DA)) Q:'DA  do
        . I '$D(^XPD(9.7,DA)) K ^XTMP("XPDI",DA)
        ;"Must be Loaded or Queued and be the starting package
        IF +$GET(IEN)>0 do
        . S (DA,XPDST)=+IEN
        . ;"Build XPDT array
        . NEW XPD,XPDIT,Y
        . S XPD=+IEN,XPDIT=0
        . I '$D(^XPD(9.7,"ASP",XPD)) D XPDT^TMGXPDI1(1,XPD) Q
        . F  S XPDIT=$O(^XPD(9.7,"ASP",XPD,XPDIT)) Q:'XPDIT  do
        . . S Y=+$O(^(XPDIT,0))
        . . D XPDT^TMGXPDI1(XPDIT,Y)
        . I '$O(XPDT(0)) S XPDQUIT=1 D QUIT^TMGXPDI1(XPD)
        ELSE  do
        . NEW Scrn
        .;" SET Scrn="I $P(^(0),U,9)<2,$D(^XPD(9.7,""ASP"",Y,1,Y))"  ;//kt original screen code.
        . SET Scrn="I $D(^XPD(9.7,""ASP"",Y,1,Y)),$D(^XTMP(""XPDI"",Y))" ;"As per suggestion from George Timpson
        . S (DA,XPDST)=$$LOOK^XPDI1(Scrn)
        Q:('DA)!($GET(XPDQUIT)=1)
        S XPDQ=^XPD(9.7,DA,0)
        S DIR(0)="Y",DIR("A")="Want to continue with the Unload of this Distribution",DIR("B")="NO"
        S DIR("?")="YES will delete the Transport Global and the entry in the Install file for these Packages."
        I $P(XPDQ,U,9)=1,$P(XPDQ,U,6) do
        . W !,"This Distribution is Queued for Install with task number ",$P(XPDQ,U,6),!
        . W "Don't forget to delete Taskman Task."
        D ^DIR WRITE !
        I 'Y!$D(DIRUT) D QUIT^XPDI1(XPDST) Q
        S XPD=0,DIK="^XPD(9.7,"
        ;"Need to KILL the XTMP("XPDI") and the entry in the install file
        F  S XPD=$O(XPDT(XPD)) Q:'XPD  do
        . S DA=+XPDT(XPD)
        . D ^DIK
        . K ^XTMP("XPDI",DA)
        ;"check IF Out-Of-Order setname is defined, KILL it
        I $D(^XTMP("XQOO",$P(XPDQ,U))) K ^($P(XPDQ,U))
        D QUIT^XPDI1(XPDST)
        Q

EN2     ;convert
        N %,DA,DIK,DIR,DIRUT,X,XPD,XPDBLD,XPDI,XPDNM,XPDPKG,XPDPMT,XPDST,XPDT,XPDQUIT,Y
        S XPDI=$$LOOK^XPDI1("I '$P(^(0),U,9),$D(^XPD(9.7,""ASP"",Y,1,Y))") Q:'XPDI
        K XPDT("DA"),XPDT("NM")
        ;make sure transport globals exist
        S XPDT=0 F  S XPDT=$O(XPDT(XPDT)) Q:'XPDT  D
        .S Y=+XPDT(XPDT) Q:$D(^XTMP("XPDI",Y))
        .W !,$P(XPDT(XPDT),U,2),"   ** Transport Global doesn't exist **",$C(7)
        .K XPDT(XPDT) S XPDQUIT=1
        I $D(XPDT)'>9!$D(XPDQUIT) D QUIT^XPDI1(XPDI) Q
        S DIR(0)="Y",DIR("A")="Want to make the Transport Globals Permanent",DIR("B")="NO"
        S DIR("?",1)="YES will leave the Transport Global so you can transport this TG in multiple Distributions."
        S DIR("?")="NO will remove the Transport Global after you transport this TG in the next Distribution."
        D ^DIR I $D(DIRUT) D QUIT^XPDI1(XPDI) Q
        S XPDPMT=Y,DIR("A")="Want to continue with the Conversion of the Package(s)",DIR("B")="NO"
        S DIR("?",1)="YES will convert the Packages to globals that can be transported.",DIR("?")="An entry will be added to the Build file and the entry in the Install file will be deleted."
        D ^DIR I 'Y!$D(DIRUT) Q
        S XPDT=0,DIK="^XPD(9.7,"
        F  S XPDT=$O(XPDT(XPDT)) Q:'XPDT  D  Q:$D(XPDQUIT)
        .;kill Install file entry
        .S XPDA=+XPDT(XPDT),XPDNM=$P(XPDT(XPDT),U,2),XPDBLD=$O(^XTMP("XPDI",XPDA,"BLD",0)),XPDPKG=+$O(^XTMP("XPDI",XPDA,"PKG",0))
        .;resolve the Package file link
        .D:XPDPKG
        ..N DIC,X,Y
        ..S DIC="^DIC(9.4,",DIC(0)="X",X=$P(^XTMP("XPDI",XPDA,"PKG",XPDPKG,0),U)
        ..D ^DIC I Y<0 S XPDPKG=0 Q
        ..S XPDPKG=+Y
        .S DA=$$BLD^XPDIP(XPDBLD) D:DA
        ..K ^XTMP("XPDT",DA)
        ..S ^XTMP("XPDT",DA)=XPDPMT M ^XTMP("XPDT",DA)=^XTMP("XPDI",XPDA)
        .I 'DA W !,XPDNM,"   ** Couldn't add to Build file **" S XPDQUIT=1 Q
        .;kill Install file entry
        .S DA=XPDA D ^DIK
        .K ^XTMP("XPDI",XPDA)
        ;set expiration date to 1 year IF global should be permanent, ELSE 30
        S ^XTMP("XPDT",0)=$$FMADD^XLFDT(DT,$S(XPDPMT:365,1:30))_U_DT
        D QUIT^XPDI1(XPDI)
        W !,"  ** DONE **",!
        Q
