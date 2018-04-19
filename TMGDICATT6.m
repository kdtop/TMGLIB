TMGDICATT6 ;KST/SFISC/GFT,XAK- TMG VERSION OF SETS,FREE TEXT ;6 JAN,2011
         ;;1.0;TMG-LIB;**1**;1/6/11
         ;
         ;"Original file header below
         ;"DICATT6 ;SFISC/XAK-SETS,FREE TEXT ;17MAY2005
         ;";;22.0;VA FileMan;**76,127,1014**;Mar 30, 1999
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
        GOTO @N
        ;
3       SET Z="",L=1,P=0,Y="INTERNALLY-STORED CODE: "
        SET TMGSETCODE=""
P       SET P=P+1
        SET C=$P($P(O,U,3),S,P)
        SET TMGSETCODE=$ORDER(TMGSET(TMGSETCODE))
        SET TMGSETNM=$GET(TMGSET(TMGSETCODE))
        ;"WRITE !,Y
        ;"IF C]"" W $P(C,":",1)_"// "
        ;"READ T:DTIME
        SET T=TMGSETCODE
        ;"GOTO T:'$T
        ;"IF T_C']"" GOTO P2
        ;"IF T="@" GOTO P
        ;"IF T="" SET T=$P(C,":",1)
        SET X=T
        SET L=$S($L(X)>L:$L(X),1:L)
        ;"DO C  ;input verification.  Already done in CHK3^TMGDICATT"
        ;"IF '$D(X) GOTO P2
        IF X="" GOTO P2
        ;"WRITE "  WILL STAND FOR: "
        ;"IF C]"" WRITE $P(C,":",2),"// "
        ;"READ X:DTIME
        SET X=TMGSETNM
        ;"GOTO:'$T T
        ;"IF X="" SET X=$P(C,":",2)
        ;"DO C  ;input verification.  Already done in CHK3^TMGDICATT"
        ;"IF '$D(X) GOTO P2
        IF X="" GOTO P2
        IF $L(Z)+$L(T)+$L(X)+$L(F)>235 GOTO TOO
        SET Z=Z_T_":"_X_S
        IF X'="" GOTO P
        GOTO T
        ;
P2      IF Z=""!'$D(X) GOTO T
        SET (DIZ,Z)="S^"_Z
        IF DUZ(0)'="@" GOTO P3
        SET DE="^"_F
        DO S^TMGDICATT5
        K DE
        IF $D(DTOUT)!(X=U) GOTO CHECK^TMGDICATT
P3      SET C="Q"
        GOTO H
 ;
C       I X["?",P=1 K X W !,"For Example: Internal Code 'M' could stand for 'MALE'",! Q
        I X[":"!(X[U)!(X[S)!(X[Q)!(X["=") K X W $C(7),!,"SORRY, ';' ':' '^' '""' AND '=' AREN'T ALLOWED IN SETS!",! Q
        I X'?.ANP W !,$C(7),"Cannot use CONTROL CHARACTERS!" K X
        Q
 ;
TOO     ;"W $C(7),!,"TOO MUCH!! -- SHOULD BE 'POINTER', NOT 'SET'"
        SET TMGRESULT="-1^Too much to store."
T       ;"W !
        IF '$D(X) GOTO NO^TMGDICATT2
        S DTOUT=1
        G CHECK^TMGDICATT
 ;
4       ;"Entry point for adding data type 4 (FREE TEXT)
        ;"Input: uses globally scoped vars: TMGMINL,TMGMAXL,TMGPATMATCH
        KILL DG,DE,M
        SET L=250,P=$P($P($P(^DD(A,DA,0),U,4),";",2),"E",2)
        IF P DO
        . SET M=$P(P,",",2)
        . IF M SET L=M-P+1
        SET DL=1,DP=-1
        ;"SET DQ(1)="MINIMUM LENGTH^NR^^1^K:X\1'=X!(X<1) X"
        SET DG(1)=TMGMINL
        ;"SET DQ(2)="MAXIMUM LENGTH^RN^^2^K:X\1'=X!(X>"_L_")!(DG(1)>X) X"
        SET DG(2)=TMGMAXL
        SET T="",L=1,P=" X"
        ;"SET DQ(3)="(OPTIONAL) PATTERN MATCH (IN 'X')^^^3^S X=""I ""_X D ^DIM S:$D(X) X=$E(X,3,999) I $D(X) K:X?.NAC X"
        ;"SET DQ(3,3)="EXAMPLE: ""X?1A.A"" OR ""X'?.P"""
        IF TMGPATMATCH'="" SET DG(3)=TMGPATMATCH
        IF 'O GOTO DIED
        IF C'?.E1"K:$L".E1" X" GOTO DG
        SET T=$P(C,"K:$L",1)
        SET DE(2)=+$P(C,"$L(X)>",2)
        SET DE(1)=+$P(C,"$L(X)<",2)
        SET Y=0,I=0
        SET Z=$P(C,")!'(",2,99)
        IF Z="" DO  GOTO DG
        . IF 'DE(2) KILL DE(2)
L       SET I=I+1,X=$E(Z,I)
        IF X'?.P GOTO L
        IF X="" GOTO DG
        IF X=Q SET Y='Y GOTO L
        IF Y GOTO L
        IF X="(" S L=L+1
        IF X'=")" GOTO L
        SET L=L-1
        IF L GOTO L
        SET DE(3)=$E(Z,1,I-1),P=$E(Z,I+1,999)
DG      IF $D(^DD(A,DA,3)) SET M=^(3)
        FOR L=1,2,3 DO
        . IF $D(DE(L)) SET DG(L)=DE(L)
DIED    KILL Y SET DM=0
        ;"DO DQ^DIED  ;<-- SETS REPLIES IN DG(n), already done above.
        KILL DQ,DM
        IF $D(DTOUT)!($D(Y)) GOTO CHECK^TMGDICATT
        SET Y=DG(1),L=DG(2),X=$S(L=Y:L,1:Y_"-"_L)
        ;"IF L<Y W $C(7),"??" GOTO 4
        SET Z="Answer must be "_X_" character"_$E("s",X'=1)_" in length."
        IF $S($D(M):M'[Z,1:1) S M=Z
        SET X=$S('$D(DG(3)):"",DG(3)="":"",1:"!'("_DG(3)_")")
        SET C=T_"K:$L(X)>"_L_"!($L(X)<"_Y_")"_X_P
Z       SET (DIZ,Z)="FJ"_L_U
H       G ^TMGDICATT1
