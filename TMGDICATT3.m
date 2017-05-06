TMGDICATT3 ;KST/SFISC/GFT,XAK- TMG VERSION OF COMPUTED FIELDS ;6 JAN,2011, 2/2/14
         ;;1.0;TMG-LIB;**1**;1/6/11
         ;
         ;"Original file header below
         ;"DICATT3 ;SFISC/COMPUTED FIELDS ;6MAY2009
         ;";;22.0;VA FileMan;**76,118,1035**;Mar 30, 1999
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
        K DIRUT,DTOUT
        DO COMP
        IF $P(^DD(A,DA,0),U,2)["C" GOTO N^TMGDICATT
        SET DTOUT=1
        GOTO CHECK^TMGDICATT
 ;
COMP    NEW DIR,DICOMPX,DISPEC,DICMIN,DIL,DIJ,DIE,DIDEC
        SET DISPEC=$P($G(^DD(A,DA,0)),U,2)
        ;"SET DIR(0)="FU"
        ;"SET DIR("A")="'COMPUTED-FIELD' EXPRESSION"
        ;"IF O,$D(^DD(A,DA,9.1)) S DIR("B")=^(9.1)
        ;"SET DIR("?")="^D DICATT3^DIQQ"
        ;"DO ^DIR
        SET (Y,X)=TMGCOMPEXP
        ;"Q:$D(DIRUT)
        ;"IF $D(DIR("B")),DIR("B")=Y G GETTYPE
        KILL DICOMPX S DICOMPX=""
        SET DICMIN=Y
        SET DQI="Y("_A_","_DA_","
        SET DICMX="X DICMX"
        SET DICOMP="?I"
        DO ^DICOMP
        IF '$D(X) DO  GOTO 6
        . ;"W $C(7),"  ...??"
        . SET TMGRESULT="-1^Invalid COMPUTED-FIELD EXPRESSION"
        ;"IF DUZ(0)="@" W !,"TRANSLATES TO THE FOLLOWING CODE:",!,X,!
        ;"IF Y["m" W !,"FIELD IS 'MULTIPLE-VALUED'!",!
        IF O,$D(^DD(A,DA,9.01))!(DICOMPX]"") D ACOMP
        SET DISPEC=$E("D",Y["D")_$E("B",Y["B")_"C"_$S(Y'["m":"",1:"m"_$E("w",Y["w"))
        SET DISPEC=DISPEC_$S(Y["p":"p"_$S($P(Y,"p",2):+$P(Y,"p",2),1:""),1:"")
        SET DISPEC=DISPEC_$S(Y'["B":"",1:"J1")
        SET ^DD(A,DA,0)=F_U_DISPEC_"^^ ; ^"_X
        SET ^(9)=U
        SET ^(9.1)=DICMIN
        SET ^(9.01)=DICOMPX
        FOR Y=9.2:0 Q:'$D(X(Y))  S ^(Y)=X(Y),Y=$O(X(Y))
        KILL X,DICOMPX
GETTYPE KILL DIR
        ;"SET DIR(0)="SBA^S:STRING;N:NUMERIC;B:BOOLEAN;D:DATE;m:MULTIPLE;p:POINTER;mp:MULTIPLE POINTER"
        ;"SET DIR("A")="TYPE OF RESULT: "
        ;"SET DIR("B")=$P($E(DIR(0),$F(DIR(0),$$TYPE(DISPEC)_":"),99),";")
        ;"DO ^DIR
        SET (Y,X)=TMGCOMPTYP  ;"<--- Already had input checked in CHK6^TMGDICATT
        ;"IF $D(DIRUT) G END
        SET DISPEC=$TR(Y,"SN")  ;"DISPEC=Y, with S,N's removed.
        IF Y="B"!(Y="D") DO  GOTO END
        . DO P(Y)
        IF Y["p" DO  GOTO END
        . DO POINT
        ;"Here down for S,N,m
        SET DIJ=""
        SET DIE=$P($P(O,U,2),"J",2)
        FOR J=0:0 S N=$E(DIE) Q:N?.A  DO
        . SET DIE=$E(DIE,2,99)
        . SET DIJ=DIJ_N
        SET DIDEC=$P(DIJ,",",2)
        SET DIL=$S(DIJ:+DIJ,1:8)
        IF Y'="N" SET DIDEC=""
        IF DISPEC["m" DO  GOTO END
        . DO P(DISPEC)
        ;"Here down for S,N
        IF Y="N" DO
        . DO DEC
        ;"IF '$D(DIRUT) D LEN
        IF Y="S" DO
        . DO LEN
END     IF O DO  QUIT
        . SET DI=A
        . DO PZ^DIU0
        DO SDIK^TMGDICATT22
6       QUIT  ;"leave this here
 ;
 ;
DEC     NEW DG,O,M
FRAC    KILL DIR
        ;"SET DIR("A")="NUMBER OF FRACTIONAL DIGITS TO OUTPUT: "
        ;"IF DIDEC]"" S DIR("B")=DIDEC
        ;"SET DIR("?")="Enter the number of decimal digits that should normally appear in the result."
        ;"SET DIR(0)="NAO^0:14:0"
        ;"DO ^DIR
        SET Y=TMGCOMPND
        ;"Q:$D(DIRUT)
        S DIDEC=Y
        SET DG=" S X=$J(X,0,"
        SET M=$P(^DD(A,DA,0),DG)
        ;"SET %=M_DG_DIDEC_")"'=^(0)+1
        ;"W !,"SHOULD VALUE ALWAYS BE INTERNALLY ROUNDED TO ",DIDEC," DECIMAL PLACE",$E("S",DIDEC'=1)
        ;"DO YN^DICN
        IF TMGCOMPROU="Y" SET %=1
        ELSE  SET %=2
        ;"GOTO FRAC:'%
        ;"QUIT:%'>0
        SET ^DD(A,DA,0)=M_$P(DG_DIDEC_")",U,%)
S       SET DQI="Y("
        SET O=$D(^(9.02))
        SET X=^(9.1)
        K DICOMPX,^(9.02)
        Q:'$D(^(9.01))
        F Y=1:1 S M=$P(^(9.01),";",Y) Q:M=""  DO
        . SET DICOMPX(1,+M,+$P(M,U,2))="S("""_M_""")"
        . SET DICOMPX=""
        IF Y<2 QUIT
        I X'["/",X'["\" Q:X'["*"  Q:Y<3
        D ^DICOMP
        Q:$D(X)-1
        S %=2-O
        ;"W !,"WHEN TOTALLING THIS FIELD, SHOULD THE SUM BE COMPUTED FROM",!
        ;"W ?7,"THE SUMS OF THE COMPONENT FIELDS"
        ;"D YN^DICN
        IF TMGCOMPCFC'="" DO    ;"If "", then use default %, SET above.
        . IF TMGCOMPCFC="Y" SET %=1
        . ELSE  SET %=2
        I %=1 S ^DD(A,DA,9.02)=X_" S Y=X"
        ;"S:%<1 DIRUT=1
        Q
 ;
LEN     K DIR
        ;"S DIR(0)="NAO^1::0",DIR("A")="LENGTH OF FIELD: ",DIR("B")=DIL
        ;"S DIR("?")="Maximum number of character expected to be output."
        ;"D ^DIR
        IF TMGCOMPMXL="" SET Y=DIL
        ELSE  SET Y=TMGCOMPMXL
        ;"Q:$D(DIRUT)
        NEW TMGTMP SET TMGTMP=$P(DISPEC,"J")_"J"_Y_$E(",",DIDEC]"")_DIDEC_DIE
        DO P(TMGTMP)
        QUIT
 ;
POINT   K DIR
        ;"SET DIR(0)="P^1:QEF"
        ;"SET DIR("A")="POINT TO WHAT FILE"
        ;"SET DIR("S")="I $$OKFILE^DICOMPX(Y,""W"")"
        ;"SET X=$P($P(^DD(A,DA,0),U,2),"p",2)
        ;"IF 'X S X=$P($P(O,U,2),"p",2)
        ;"IF X,$D(^DIC(+X,0)) S DIR("B")=$P(^(0),U)
        ;"DO ^DIR
        SET Y=TMGCOMPP2
        ;"I '$D(DIRUT) S $P(DISPEC,"p",2)=+Y D P(DISPEC)
        SET $P(DISPEC,"p",2)=+Y
        DO P(DISPEC)
        QUIT
 ;
P(C)    ;
        SET $P(^DD(A,DA,0),U,2)="C"_$TR(C,"C^")
        QUIT
 ;
ACOMP ;"SET/KILL ACOMP NODES
        N X,I
        I $G(^DD(A,DA,9.01))]"" S X=^(9.01) X ^DD(0,9.01,1,1,2)
        I DICOMPX]"" S X=DICOMPX X ^DD(0,9.01,1,1,1)
        Q
 ;
TYPE(S) ;
 Q $S(S["D":"D",S["B":"B",S["mp":"mp",S["m":"m",S["p":"p",S'["J":"S",S[",":"N",1:"S") ;figure out TYPE OF RESULT
 ;
