TMGDICATT1 ;KST/SFISC/GFT,XAK- TMG VERSION OF NODE AND PIECE, SUBFILE ;6 JAN,2011
         ;;1.0;TMG-LIB;**1**;1/6/11
         ;
         ;"Original file header below
         ;"DICATT1 ;SFISC/GFT,XAK-NODE AND PIECE, SUBFILE ;21APR2008
         ;";;22.0;VA FileMan;**1032**;Mar 30, 1999
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
        I DA=.001 S W=" " GOTO 2
        SET (DG,W)=$P(O,U,4)
        IF W="" GOTO M
        SET T=0,DP=DA,Y=$P(W,";"),N=$P(W,";",2)
        DO MX
        SET L=L-T
        DO MAX
        IF T+3<$G(^DD("STRING_LIMIT"),255) S W=DG G ^TMGDICATT2
        DO TOO
        G NO^TMGDICATT2
        ;
M       K DE,DG
        SET %=2
        ;"W !,"WILL "_F_" FIELD BE MULTIPLE"
        ;"D YN^DICN
        IF (TMGMULT="Y") SET %=1
        SET V=(%=1)  ;"V == is multiple
        IF %<0 GOTO BACK
        GOTO SUB
        ;"W !,"FOR A GIVEN ENTRY, WILL THERE BE MORE THAN 1 "_F,!," ON FILE AT ONCE?"
        ;"GOTO M
E ;
        SET V=0
        SET DE(3)=$S($D(^(3)):^(3),1:"")
        SET T=0,DP=E,N=$P($P(DE,U,4),";",2)
        DO MX
        SET L=T
SUB     IF $P(DIZ,"^")["K" SET V=1
        SET T=0
        FOR Y=0:1 Q:'$D(^DD(A,"GL",Y+1))
        IF 'V DO MAX
        IF (T>245)!$D(^DD(A,"GL",Y,0))!V DO
        . SET Y=$S(+Y=Y:Y+1,1:$C($A(Y)+1))
        IF DUZ(0)'="@" GOTO SB
        ;"W !!,"SUBSCRIPT: ",Y,"// "
        ;"R X:DTIME
        ;"S:'$T X=U,DTOUT=1
        ;"S:X="" X=Y
        SET X=TMGSUBSCR
        ;"I X'?.ANP DO  GOTO BACK
        ;". SET DTOUT=1
        ;". SET TMGRESULT="1^Control Characters are not allowed."
        ;". ;"W !?5,$C(7),"Control Characters are not allowed." G SUB
        ;"I +X=X GOTO SUB2
        ;"GOTO BACK:X[U
        ;"GOTO DICATT1^DIQQQ:X["?"
        ;"IF (X?1P.E)!(X[",")!(X[":")!(X[S)!(X[Q)!(X["=") GOTO SUB
SUB2    I Y=X GOTO SB
        S Y=X
        D MAX
        IF T+5>$G(^DD("STRING_LIMIT"),255) D TOO GOTO BACK  ;"was SUB
SB      S W=Y,X=0
        IF V GOTO V  ;"V== is multiple
        IF $D(^DD(A,"GL",W,0)) GOTO U
PIECE   SET Y=1,P=0
PC      SET X=$O(^DD(A,"GL",W,X))
        IF X'="" DO  GOTO PC
        . SET P=$P(X,",",2)
        . SET Y=$S(Y>P:Y,1:P+1)
        SET X=-1
        IF P SET Y="E"_Y_","_(L+Y-1)
        E  F Y=1:1 Q:'$D(^(Y))
        S P=Y
        I DUZ(0)'="@" GOTO PC2
        ;"W !,"^-PIECE POSITION: ",Y,"// "
        ;"R P:DTIME S:'$T DTOUT=1
        ;"IF $D(DTOUT) GOTO CHECK^TMGDICATT
        SET P=TMGPIECE
        ;"S:P="" P=Y
PC2     ;"G PQ:P["?"
        IF P'?1"E"1N.N1","1N.N GOTO USED
        S N=$P(P,",",2)-$E(P,2,9)+1
        IF N'<L GOTO USED
        ;"W $C(7),!,"CAN'T BE <",L
        SET TMGRESULT="-1^Subscript CAN'T BE <"_L
        SET DTOUT=1
        GOTO BACK
        ;"I P>0,P<100,P\1=P G USED
        ;"S W="" I X'[U W $C(7),"??"
        ;"G SUB
        ;
BACK    IF $D(DTOUT)!(+TMGRESULT<0) GOTO CHECK^TMGDICATT
        GOTO TYPE^TMGDICATT2
 ;
PQ      ;"W "  TYPE A NUMBER FROM 1 TO 99"
        ;"I Y=1 W !?9,"OR AN $EXTRACT RANGE (E.G., ""E2,4"")"
        ;"ELSE  DO
        ;". W !?15,"CURRENTLY ASSIGNED:",!
        ;". S Y="" F P=0:0 S Y=$O(^DD(A,"GL",W,Y)) Q:Y=""  DO
        ;". . S P=$O(^(Y,0))
        ;". . I $D(^DD(A,P,0)) W ?11,$S(Y:"PIECE ",1:"")_Y,?22,"FIELD #"_P_", '"_$P(^(0),U,1)_"'",!
        ;"G PIECE
 ;
USED    SET W=W_S_P
        SET X=P
        IF '$D(^(X)) GOTO DE
U       ;"W !,$C(7),X_" ALREADY USED FOR FIELD: "_$P(^DD(A,$O(^(X,0)),0),U,1)
        DO
        . NEW USEDBY SET USEDBY="ALREADY USED FOR "_$P(^DD(A,$O(^(X,0)),0),U,1)
        . IF X=0 DO
        . . SET TMGRESULT="-1^INVALID STORAGE LOCATION.  FOR SUBSCRIPT '"_TMGSUBSCR_"', THE ENTIRE NODE IS "_USEDBY
        . ELSE  DO
        . . SET TMGRESULT="-1^INVALID STORAGE LOCATION.  FOR SUBSCRIPT '"_TMGSUBSCR_"', PIECE "_X_" IS "_USEDBY
        SET DTOUT=1
        GOTO BACK
 ;
MAX     SET N=0
        FOR T=L:0 SET N=$O(^DD(A,"GL",Y,N)) Q:N=""  DO
        . SET DP=$O(^(N,0))
        . DO MX
        SET N=-1
        QUIT
        ;
MX      IF N?1"E".E DO
        . SET T=T+$P(N,",",2)-$E(N,2,9)+1
        IF 'N QUIT
        SET P=$P(^DD(A,DP,0),U,2)
        SET W=$S(P["J":$P(P,"J",2),P["P":9,P["N":14,P["D":7,1:0)
        IF W GOTO W
        IF P'["S" GOTO MX2
        FOR P=1:1 DO  IF 'X GOTO W
        . S X=$L($P($P($P(^(0),U,3),";",P),":",1))
        . S:X>W W=X
MX2     SET W=$P(^(0),"$L(X)>",2)
        SET W='W*30+W
W       SET T=T+W+1
        QUIT
 ;
V       IF $D(^DD(A,"GL",W)) DO  GOTO BACK  ;"was SUB
        . ;"W $C(7),!?9,"CAN'T STORE A "_$S($P(DIZ,U)["K":"MUMPS",1:"MULTIPLE")_" FIELD IN AN ALREADY-USED SUBSCRIPT!"
        . SET TMGRESULT="-1^CAN'T STORE A "_$S($P(DIZ,U)["K":"MUMPS",1:"MULTIPLE")_" FIELD IN AN ALREADY-USED SUBSCRIPT!"
        . SET DTOUT=1
        IF $P(Z,U)'["K" S W=W_S_0 S:$P(DIZ,U)["K" W=$P(W,";")_";E1,245"
DE      IF '$D(DE) GOTO 2
        SET ^DD(A,DA,0)=F_U_$P(DE,U,2,3)_U_W_U_$P(DE,U,5,99)
        SET DIK="^DD(A,"
        SET DA(1)=A
        SET ^(3)=DE(3)
        SET ^("DT")=DT
        DO IX1^DIK
        GOTO N^TMGDICATT
        ;
2       IF $P(Z,U)["K" DO
        . SET V=0
        . SET W=W_";E1,245"
        . SET M="This is Standard MUMPS code."
        GOTO ^TMGDICATT2
 ;
TOO     ;"W $C(7),!," TOO MUCH TO STORE AT THAT SUBSCRIPT!"
        SET TMGRESULT="-1^TOO MUCH TO STORE AT THAT SUBSCRIPT!"
        SET DTOUT=1
        QUIT
