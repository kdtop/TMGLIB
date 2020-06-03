TMGDICATT2 ;KST/SFISC/GFT,XAK- TMG VERSION OF DEFINING MULTIPLES ;6 JAN,2011
         ;;1.0;TMG-LIB;**1**;1/6/11
         ;
         ;"Original file header below
         ;"DICATT2 ;SFISC/GFT,XAK-DEFINING MULTIPLES ;4APR2007
         ;";;22.0;VA FileMan;**89,127,152,1014**;Mar 30, 1999
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
        SET T=$E(Z)
        IF $D(DTOUT) GOTO CHECK^TMGDICATT
        FOR P="I","O","L","x" DO
        . S:$P(O,U,2)[P Z=$P(Z,U)_P_U_$P(Z,U,2)
1       KILL DS
        S:$P(Z,U)'["K" V=W[";0"
        S P=0,N=DICL,DQ=4,DP=6
        SET DQI=" S:$D(X) DINUM=+X"
        SET DREF=$F(O,DQI)-1=$L(O)
        SET DE(7,0)="NO"
        SET DG(7)="N"
        IF T="*" SET T=$S($P(Z,U)["S":"S",1:"P")
        IF DA=.001 GOTO 1^TMGDICATT22
        IF T="W" GOTO W
        S:$D(DTIME)[0 DTIME=300
        IF (T'["F"),(T'["S"),(T'["K"),('O!DREF) DO
        . IF 'DREF QUIT
        . SET DE(7,0)="YES",DG(7)="Y"
S       FOR Y=4:1:6 DO  IF ('V),DA-.01!'N QUIT
        . SET DQ(Y)=$P($T(DQ+Y),";",3)_F_$P($T(DQ+Y),";",4)_" (Y/N)^RS^Y:YES;N:NO^"_Y_"^Q"
        . IF Y=4 SET DG(4)=TMGMULT
        . IF Y=5 SET DG(5)=TMGSHOWADDNEW
        . IF Y=6 SET DG(6)=TMGMULTASK2
        IF '$DATA(DG(5)) SET DG(5)="Y"
        SET DE(4,0)="NO"
        SET DP=-1,DL=1
        IF T["P"!(T["N") S DE(5,0)="YES"
        IF 'O GOTO S2
        SET DE(6,0)=$E("NY",$P(O,U,2)["M"+1)
        IF $P(O,U,2)["R" SET DE(4,0)="Y"
        IF DA=.01,N DO
        . SET P=$O(^DD(J(N-1),"SB",A,0))
        . IF P="" SET P=-1
        . SET Y=$P(^DD(J(N-1),P,0),U,2)
        . SET DE(5,0)=$E("YN",Y["A"+1)
S2      KILL Y
        SET DIFLD=-1
        ;"DO RE^DIED  <-- PUTS REPLIES INTO DG(), already done above
        K DQ,DIFLD
        IF '$D(Y) GOTO S3
        IF $P(Z,U)["X" GOTO N^TMGDICATT
        GOTO CHECK^TMGDICATT
S3      I $D(DTOUT) K DTOUT G CHECK^TMGDICATT
        IF DG(5)="N" SET T=T_"A"
        IF DG(4)="Y",$P(Z,U)'["R" SET Z="R"_Z
        IF $GET(DG(6))="Y",$P(Z,U)'["M" S Z="M"_Z
G       S DIZ=Z
        GOTO ^TMGDICATT22
Q ;
        K T,B,A,J,DA,DIC,E,DR,W,S,Q,P,N,V,I,L,F,DQI,DIK,C,Z,Y,DE,O,DICS,DICL,DDA Q
 ;
W       ;"S %=Z["L"+1
        ;"W !,"SHALL THIS TEXT NORMALLY APPEAR IN WORD-WRAP MODE"
        ;"D YN^DICN
        IF TMGWPWRAP="Y" SET %=1
        ELSE  SET %=2
        ;"IF %<0 GOTO CHECK^TMGDICATT
        ;"IF '% DO  GOTO W
        ;". W !,"ANSWER 'YES' IF THE INTERNALLY-STORED '"_F_"' TEXT"
        ;". W !?5,"SHOULD NORMALLY BE PRINTED OUT IN FULL LINES, BREAKING AT WORD BOUNDARIES."
        ;". W !?2,"ANSWER 'NO' IF THE INTERNAL TEXT SHOULD NORMALLY BE PRINTED OUT"
        ;". W !?5,"LINE-FOR-LINE AS IT STANDS.",!
        SET Z=$P($TR(Z,"L"),U)_$E("L",%=2)_U
        GOTO WINDOW
 ;
 ;
WINDOW  ;"S %=2-(Z["x"!'O)
        ;"W !,"SHALL ""|"" CHARACTERS IN THIS TEXT BE TREATED LIKE ANY OTHER CHARACTERS"
        ;"D YN^DICN
        IF TMGWPNOVAR="Y" SET %=1
        ELSE  SET %=2
        ;"IF %<0 G CHECK^TMGDICATT
        ;"IF '% DO  G WINDOW
        ;". W !,"ANSWER 'YES' IF THE INTERNALLY-STORED '"_F_"' TEXT MAY HAVE ""|"" CHARACTERS"
        ;". W !?3,"IN IT (SUCH AS HL7 MESSAGES) THAT NEED TO DISPLAY EXACTLY AS THEY ARE STORED."
        ;". W !,"ANSWER 'NO' IF THE INTERNAL TEXT SHOULD NORMALLY BE PRINTED OUT WITH ANYTHING"
        ;". W !?3,"THAT IS DELIMITED BY ""|"" CHARACTERS INTERPRETED AS VARIABLE TEXT.",!
        I % S Z=$P($TR(Z,"x"),U)_$E("x",%=1)_U
        GOTO G
 ;
 ;
 ;
X ;
        W "   (FIELD DEFINITION IS NOT EDITABLE)"
        I N=4 K DIRUT D LENGTH(A,DA) I $D(DIRUT) K DIRUT G N^TMGDICATT
        S T=$E(^DOPT("DICATT",N,0)),Y=^DD(A,DA,0),Z=$TR($P(Y,U,2),"MR")_U_$P(Y,U,3),W=$P(Y,U,4),C=$P(Y,U,5,99) S:Z["K" V=0
        G N^TMGDICATT:N=6,1
 ;
LENGTH(DI,DIFIELD) ;
        N DIR,DICY,Y,X,A0,B0,A1,A2
        S DICY=$G(^DD(DI,DIFIELD,0)) I $P(DICY,U,2)'["F" Q
        S A0=250,A1=$P($P($P(DICY,U,4),";",2),"E",2) I A1 S A2=$P(A1,",",2) I A2 S A0=A2-A1+1,DIR("?",1)="Data is stored by '$E"_A1_"'"
        S DIR("A")="MAXIMUM LENGTH OF '"_$P(DICY,U)_"'",DIR(0)="N^1:"_A0,DIR("B")=$$FL^DIQGDDU(DI,DIFIELD)
        S DIR("?")="THIS MAXIMUM WILL BE USED FOR OUTPUT PURPOSES, BUT WILL NOT BE PART OF THE INPUT CHECK FOR THE FIELD"
        D ^DIR Q:'Y
        N F S X=$P(DICY,U,2),F=$F(X,"J") I F Q:+$E(X,F,99)=Y  F  Q:$E(X,F)'?1N  S X=$E(X,1,F-1)_$E(X,F+1,99)
        S X=$TR(X,"J")_"J"_Y,$P(^DD(DI,DIFIELD,0),U,2)=X
        I $D(DDA) S DDA="E",A0="LENGTH^.23",A1=DIR("B"),A2=Y D IT^DICATTA
        Q
 ;
NO ;
        ;"W !,$C(7),"  <DATA DEFINITION UNCHANGED>"
        DO ADDERR^TMGDICATTS("<DATA DEFINITION UNCHANGED>")
        I ($P(Z,U)["K")&(DUZ(0)'="@") G N^TMGDICATT
        GOTO TMGDONE^TMGDICATT
        ;
TYPE ;
        IF TMGRESULT<0 GOTO TMGDONE^TMGDICATT
        K Y,M,DE,DIE,DQ,DG
        IF $D(DTOUT) GOTO Q^DIB
        SET N=0
        SET DQI=DICL+9
        SET Y=^DD(A,DA,0)
        SET F=$P(Y,U)
        SET Z=""
        ;"W !!,"DATA TYPE OF ",F,": "
        ;"IF O GOTO TYPE2
        ;"READ X:DTIME
        SET Y=$P(TMGFLDTYPE,"^",1)
        SET X=$P(TMGFLDTYPE,"^",2)
        ;"SET:'$T DTOUT=1
        ;"GOTO X^TMGDICATT:(X[U)!('$T)
        IF O GOTO TYPE2  ;"//moved down few lines by //kt
        IF DUZ(0)'="@" SET DIC("S")="I Y-9"
        IF DA=.001 SET DIC("S")="I Y<4!(Y=7)"
        GOTO NEW
TYPE2   ;"W !!,"DATA TYPE OF ",F,": "
        FOR N=9:-1:5,1:1:4 Q:$P(Y,U,2)[$E("DNSFWCPVK",N)
        ;"W $P(^DOPT("DICATT",N,0),U)
        IF $P(Y,U,2)["K"&(DUZ(0)'="@") GOTO X
        IF $P(Y,U,2)["X" GOTO X
        IF N=6 GOTO 6^TMGDICATT
        ;"READ "// ",X:DTIME
        ;"SET:'$T DTOUT=1
        ;"GOTO N^TMGDICATT:X[U!'$T
        IF X="" GOTO TYPESWITCH^TMGDICATT
        ;"SET DIC("S")="I Y-6,Y-9"_$P(",Y-5",U,N\2-2!(A=B)!(DA-.01)!$O(^DD(A,DA))>0)
        ;"SET DIC("S")=DIC("S")_$S(N=7:",Y-8",N=8:",Y-7",1:"")
        DO  ;"Prior S needs to be preserved
        . NEW TEMP,S
        . SET S="I Y-6,Y-9"
        . SET TEMP=N\2-2!(A=B)!(DA-.01)!$O(^DD(A,DA))>0
        . IF TEMP=1 SET S=S_",Y-5"
        . SET S=S_$S(N=7:",Y-8",N=8:",Y-7",1:"")
        . SET DIC("S")=S
        SET Y=0
NEW     SET DIC(0)="I",DIC="^DOPT(""DICATT"","
        IF +Y'>0 DO ^DIC
        ;"SET Y=TMGFLDTYPE
        IF Y<1 DO  GOTO TMGDONE^TMGDICATT
        . SET TMGRESULT="-1^Invalid type provided."
        IF N-Y&O SET M=""
        SET O=$P(O,U,1,2)_U_U_$P(O,U,4)
        SET N=+Y
        GOTO TYPESWITCH^TMGDICATT
 ;
DQ ;;
 ;
 ;
 ;
 ;;IS ; ENTRY MANDATORY
 ;;SHOULD USER SEE AN "ADDING A NEW ;?" MESSAGE FOR NEW ENTRIES
 ;;HAVING ENTERED OR EDITED ONE ;, SHOULD USER BE ASKED ANOTHER
