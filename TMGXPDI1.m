TMGXPDI1 ;TMG/kst/Custom version of XPDI1 ;09/17/08, 2/2/14
         ;;1.0;TMG-LIB;**1**;09/17/08
 ;"Original header:
 ;"XPDI1   ;SFISC/RSD - Cont of Install Process ;10/28/2002  17:14
 ;"       ;;8.0;KERNEL;**58,61,95,108,229**;Jul 10, 1995
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
LOOK(XPDS,XPDL,InstallName,Msg) ;"Lookup Install
        ;"Purpose: Lookup into file 9.7, XPDS=DIC("S") for lookup
        ;"Input: XPDS -- screen for ^DIC
        ;"       XPDL -- optional.  Something regarding locking record.
        ;"       InstallName -- optional.  Name of install to auto-search for
        ;"       Msg -- PASS BY REFERANCE, an OUT PARAMETER
        ;"              Errors are stored in Msg("ERROR",x)=Message
        ;"                                   Msg("ERROR")=count of last error
        ;"              Message are store in Msg(x)=Message
        ;"              Msg=count of last message+1
        ;"return 0-fail or ien, XPDT=array of linked builds

        N DIC,X,Y,XPD,XPDIT,%
        S DIC="^XPD(9.7,",DIC("S")=$GET(XPDS)
        SET InstallName=$GET(InstallName)
        IF InstallName'="" do
        . S DIC(0)="QMZ"
        . SET X=InstallName
        ELSE  S DIC(0)="QAMZ"
        D ^DIC
        Q:Y<0 0

        I '$G(XPDL) L +^XPD(9.7,+Y,0):0 E  W !,"Being accessed by another user" Q 0
        S XPD=+Y,XPDIT=0
        W !!,"This Distribution was loaded on ",$$FMTE^XLFDT($P(Y(0),U,3))," with header of ",!?3,$G(^XPD(9.7,XPD,2)),!
        W ?3,"It consisted of the following Install(s):",!
        ;"Build XPDT array
        I '$D(^XPD(9.7,"ASP",XPD)) D XPDT(1,XPD) Q XPD
        F  S XPDIT=$O(^XPD(9.7,"ASP",XPD,XPDIT)) Q:'XPDIT  do
        . S Y=+$O(^(XPDIT,0))
        . D XPDT(XPDIT,Y)
        I '$O(XPDT(0)) S XPDQUIT=1 D QUIT(XPD)
        Q XPD
        ;
QUIT(Y) ;unlock ien Y
        L -^XPD(9.7,+Y) Q
        ;
XPDT(P1,P2)     ;Build XPDT array
        N % S %=$P($G(^XPD(9.7,P2,0)),U)
        I %="" W:$X ! W "**ERROR in Install, You need to remove the Distribution and reload it**",!  S XPDQUIT=1 Q
        S XPDT(P1)=P2_U_%,(XPDT("DA",P2),XPDT("NM",%))=P1 W:$X>64 ! W $J(%,15)
        Q
        ;
QUES(XPDA,Option)      ;"install questions; XPDA=ien in file 9.7
        N XPDANS,XPDFIL,XPDFILN,XPDFILO,XPDFLG,XPDNM,XPDQUES,X,Y
        S XPDNM=$P(^XPD(9.7,XPDA,0),U) W !!,"Install Questions for ",XPDNM,!
        ;"pre-init questions
        D DIR^TMGXPDIQ("PRE",,.Option) I $D(XPDQUIT) D ASKABRT^XPDI Q
        ;"file install questions
        S (XPDFIL,XPDFLG)=0
        F  S XPDFIL=$O(^XTMP("XPDI",XPDA,"FIA",XPDFIL)) Q:'XPDFIL  S X=^(XPDFIL),X(0)=^(XPDFIL,0),X(1)=^(XPDFIL),XPDFILO=^(0,1) D  Q:$D(XPDQUIT)
        .;"check for DD screening logic
        .I $G(^(10))]"" N XPDSCR S XPDSCR=^(10) ;^(10) is ref to ^XTMP("XPDI",XPDA,"FIA",XPDFIL,0,10) from prev line
        .;XPDFILN=file name^global ref^partial DD
        .;XPDANS=new file^DD screen failed^Data exists^update file name^user
        .;"doesn't want to update data  1=yes,0=no
        .S XPDFILN=X_X(0)_U_X(1),XPDANS='($D(^DIC(XPDFIL,0))#2)_"^^"_''$O(@(X(0)_"0)"))
        .I 'XPDFLG W !,"Incoming Files:" S XPDFLG=1
        .W ! D DIR^TMGXPDIQ("XPF",XPDFIL_"#",.Option) Q:$D(XPDQUIT)
        .S:$G(XPDQUES("XPF"_XPDFIL_"#2"))=0 $P(XPDANS,U,5)=1
        .S ^XTMP("XPDI",XPDA,"FIA",XPDFIL,0,2)=XPDANS
        .;"kill the answers so we can re-ask for next file
        .F I=1:1:2 K XPDQUES("XPF"_XPDFIL_"#"_I)
        ;XPDQUIT is by file questions in previous DO loop, SET in TMGXPDIQ
        I $D(XPDQUIT) D ASKABRT^XPDI Q
        ;"ask for coordinators to incoming mail groups
        S (XPDFIL,XPDFLG)=0
        F  S XPDFIL=$O(^XTMP("XPDI",XPDA,"KRN",3.8,XPDFIL)) Q:'XPDFIL  S X=^(XPDFIL,0),Y=$G(^(-1)) D  Q:$D(XPDQUIT)
        .;XPDANS=Mail Group name
        .Q:$P(Y,U)=1  ;Don't ask IF deleting
        .S XPDANS=$P(X,U)
        .I 'XPDFLG W !!,"Incoming Mail Groups:" S XPDFLG=1
        .W ! D DIR^TMGXPDIQ("XPM",XPDFIL_"#",.Option) Q:$D(XPDQUIT)
        .;kill the answers so we can re-ask for next MG
        .K XPDQUES("XPM"_XPDFIL_"#1")
        .Q
        I $D(XPDQUIT) D ASKABRT^XPDI Q
        ;"ask to rebuild menus IF Option is added
        S (XPDFIL,XPDFLG)=0
        S XPDFIL=$O(^XTMP("XPDI",XPDA,"KRN",19,XPDFIL))  D:XPDFIL
        .S X=^XTMP("XPDI",XPDA,"KRN",19,XPDFIL,0)
        .;XPDANS=Menu Rebuild Answer
        .S XPDANS=$P(X,U)
        .W ! D DIR^TMGXPDIQ("XPO",,.Option) Q:$D(XPDQUIT)
        I $D(XPDQUIT) D ASKABRT^XPDI Q
        ;"post-init questions
        W ! D DIR^TMGXPDIQ("POS",,.Option) I $D(DIRUT)!$D(XPDQUIT) D ASKABRT^XPDI Q
        Q
        ;
XQSET(XPDA)     ;get options & protocols to disable
        ;put in ^TMP($J,"XQOO",starting build name)
        N A,I,X,Y
        S I=0 F  S I=$O(^XTMP("XPDI",XPDA,"KRN",19,I)) Q:'I  S X=^(I,0),A=^(-1) D
        .S Y=$O(^DIC(19,"B",$P(X,U),0))
        .;check that option exist and 0=send,1=delete,3=MERGE or 5=disable
        .I Y,$D(^DIC(19,Y,0)),$S('A:1,1:A#2) S ^TMP($J,"XQOO",XPDSET,19,Y)=$P(^(0),U,1,2)
        S I=0 F  S I=$O(^XTMP("XPDI",XPDA,"KRN",101,I)) Q:'I  S X=^(I,0),A=^(-1) D
        .S Y=$O(^ORD(101,"B",$P(X,U),0))
        .I Y,$D(^ORD(101,Y,0)),$S(A=3:1,A=5:1,1:'A) S ^TMP($J,"XQOO",XPDSET,101,Y)=$P(^(0),U,1,2)
        Q
        ;XPDIJ need to install XPDIJ now & SET routine flag to skip
XPDIJ   N DIE,XPDA,XCM,XCN,XCS,X
        S XPDA=XPDIJ,DIE="^XTMP(""XPDI"",XPDIJ,""RTN"",""XPDIJ"",",XCN=0,X="XPDIJ"
        X ^%ZOSF("SAVE")
        S XCN=$$RTNUP^XPDUTL("XPDIJ",2)
        Q
