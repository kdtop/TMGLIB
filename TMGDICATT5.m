TMGDICATT5 ;KST/SFISC/XAK- TMG VERSION OF POINTERS ;6 JAN,2011, 2/2/14
         ;;1.0;TMG-LIB;**1**;1/6/11
         ;
         ;"Original file header below
         ;"DICATT5 ;SFISC/XAK-POINTERS ;12:04 PM  25 Jan 2000
         ;";;22.0;VA FileMan;**26**;Mar 30, 1999
         ;
         ;"Original file (as above) is Public Domain.  Modifications
         ;"made herein are Copyright Kevin Toppenberg MD Jan 6, 2011
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
7       K DIC
        SET Y=""
        SET %=$P(O,U,3)
        ;"SET DIC(0)="EFQIZ"
        ;"IF $P(O,U,2)["P"&$L(%) S Y=$S($D(@("^"_%_"0)")):$P(^(0),U),1:"")
        ;"W !,"POINT TO WHICH FILE: "
        ;"W:Y]"" Y_"// "
        ;"R X:DTIME
        ;"S:'$T DTOUT=1
        ;"IF X=U!'$T G CHECK^TMGDICATT
        ;"I Y]"",X="" S X=Y,DIC(0)=DIC(0)_"O"
        ;"SET DIC=1
        ;"SET DIC("S")="I Y'=1.1 S DIFILE=+Y,DIAC=""RD"" D ^DIAC I %"
        ;"D ^DIC
        KILL Y MERGE Y=TMGP2F  ;"set up and verified in CHK7^TMGDICATT
        K DIC,DIFILE,DIAC
        ;"IF Y<0 GOTO 7:X["?" GOTO T
        ;"S X=^(0,"GL")
        SET X=^DIC(+Y,0,"GL")
        SET DE=Y
        GOTO 77
T       K DIC
        ;"IF $D(DTOUT) GOTO CHECK^DICATT
        GOTO NO^DICATT2
77      S DIFILE=+Y
        SET DIAC="LAYGO"
        D ^DIAC
        IF 'DIAC!($P($G(^DD(DIFILE,0,"DI")),U,2)["Y") SET %=2
        ELSE  S %=0
        K DIFILE,DIAC
P       I % DO
        . ;"W !,$C(7)
        . ;"DO A   ;"//W "'ADDING A NEW "_$P(DE,U,2)_" FILE ENTRY' (""LAYGO"")" Q
        . ;"W !,"WILL NOT "
        . ;"DO B   ;"//W "BE ALLOWED WHEN ANSWERING THE "_F_"' QUESTION" Q
        E  DO
        . ;"S %=1+$S($P(O,U,2)["'":1,$P(O,U,2)']"":1,1:0)
        . ;"W !,"SHOULD "
        . ;"D A    ;"//W "'ADDING A NEW "_$P(DE,U,2)_" FILE ENTRY' (""LAYGO"")" Q
        . ;"W !
        . ;"D B    ;"//W "BE ALLOWED WHEN ANSWERING THE "_F_"' QUESTION" Q
        . ;"DO YN^DICN
        . ;"G T:%<1
        . IF TMGLAYGO="Y" SET %=1
        . ELSE  SET %=2
        SET Z="P"_+DE_$E("'",%=2)_X
        SET C="Q",L=9,E=X
        IF DUZ(0)'="@" GOTO H
        DO S
        IF X=U GOTO T
        GOTO H
S ;
        SET D=$GET(^DD(A,DA,12.1))  ;"SET D=$S($D(^DD(A,DA,12.1)):^(12.1),1:"")
        SET %=2-(D]"")
        SET P=$GET(^(12))  ;"P=$S($D(^(12)):^(12),1:"")
        SET I=$GET(^(12.2))  ;"I=$S($D(^(12.2)):^(12.2),1:"")
        ;"W !,"SHOULD '"_$P(DE,U,2)_"' ENTRIES BE SCREENED"
        ;"D YN^DICN
        ;"S:%<0 X=U
        ;"Q:X=U
        ;"I '% DO  GOTO S
        ;". W !?5,"Answer YES IF there is a condition which should prohibit",!?5,"selection of some entries."
        IF TMGIPSCRN'="" SET %=1
        ELSE  SET %=2
        I %=2 DO  QUIT
        . K ^(12.1),^(12),^(12.2)
        GOTO M ;" W !,"ENTER A TRUTH-VALUED EXPRESSION WHICH MUST BE TRUE OF ANY ENTRY POINTED TO:",!?4 I I]"" W I_"// " W:$X>35 !?4
        ;
        ;"R X:DTIME S:'$T DTOUT=1 G T:X=U!'$T S:X="" X=I I X="" G M:DUZ(0)="@",S
        ;"K DG,K S ^(12.2)=X,K=100,DQI="Y(",DG(K)=K,K(1,1)=K,(DLV,DLV0)=K,J(K)=+DE,I(K)=E,K=0 D EN^DICOMP
        ;"G S:'$D(X) I $D(X)>1!(X[" ^DIC") W $C(7),!,"TOO COMPLICATED!" G S
        ;"S I=0 I 'DBOOL W $C(7),!?8,"WARNING-- THIS DOESN'T LOOK LIKE A TRUTH-VALUED EXPRESSION"
D0      S I=$F(X,E_"D0",I) I I S X=$E(X,1,I-3)_"Y"_$E(X,I,999) G D0
Q       S I=$F(X,"""",I) I I S X=$E(X,1,I-1)_""""_$E(X,I,999),I=I+1 G Q
        S (D,X)="S DIC(""S"")="""_X_" I X""" G E:DUZ(0)'="@"
M       ;"W !,"MUMPS CODE THAT WILL SET 'DIC(""S"")': "
        ;"IF D]"" WRITE D
        SET Y=D
        ;"IF D]"" DO RW^DIR2
        ;"IF X="@" GOTO S
        ;"IF D']"" DO  Q:X=U!'$T
        ;". R X:DTIME
        ;". S:'$T DTOUT=1
        SET X=TMGIPSCRN
        ;"IF X="" SET X=D IF X="" GOTO S
        ;"IF X?."?" DO  GOTO M
        ;". D HELP^DICATT4
        ;"DO ^DIM:'$T  ;"<--- checks screening code.  Already done in CHKSCRN^TMGDICATT
        ;"IF '$D(X) S X="" G S
        ;"IF X'["DIC(""S"")" DO
        ;". W $C(7),!,?8,"WARNING - Screen Does Not Contain DIC(""S"")"
E       ;"WRITE !,"EXPLANATION OF SCREEN: "
        ;"IF P]"" WRITE P_"// "
        ;"READ %:DTIME
        SET %=TMGIPSDESCR
        ;"SET:'$T %=U,DTOUT=1
        ;"IF %="" SET %=P
        ;"IF %=U GOTO S
        ;"I %?.P DO  G E
        ;". W !?5,$C(7),"An explanation must be entered."
        I $D(^DD(A,DA,12.1)) S:X'=^(12.1) M(1)=0
        SET ^DD(A,DA,12)=%
        SET ^(12.1)=X
        SET Z="*"_Z
        IF Z?1"*P".E SET C=X_" D ^DIC K DIC S DIC=DIE,X=+Y K:Y<0 X"
        QUIT
        ;
H       S DIZ=Z
        G ^TMGDICATT1
 ;
A       ;"W "'ADDING A NEW "_$P(DE,U,2)_" FILE ENTRY' (""LAYGO"")"
        Q
B       ;"W "BE ALLOWED WHEN ANSWERING THE "_F_"' QUESTION"
        Q
