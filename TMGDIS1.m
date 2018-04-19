TMGDIS1 ;TMG/kst/Custom version of DIS1 ;03/25/06 ; 5/15/10 11:15pm, 2/2/14
        ;;1.0;TMG-LIB;**1**;01/01/06
        ;----Prior header below ----------
        ;SFISC/GFT-BUILD DIS-ARRAY ;20MAR2005
        ;;22.0;VA FileMan;**6,77,97,113,144**;Mar 30, 1999;Build 5
        ;
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
DIS1    ;"Purpose: BUILD DIS-ARRAY
        KILL DIS0
        IF $D(DL)#2 SET DIS0=DL
        SET DL(0)=""
        ;"W !
        IF $D(DE)>1!$D(DJ) GOTO 1
        IF DL=1 DO
        . SET DL(0)=DL(1),DL=0 KILL DL(1)
        ELSE  DO
        . FOR P=2:1 SET Y=$P(DL(1),U,P) QUIT:Y=""  DO
        . . SET Y=U_Y_U
        . . SET X=2
        . . DO 2
        FOR X=1:1 QUIT:'$D(DL(X))  DO
        . FOR Y=X+1:1 QUIT:'$D(DL(Y))  DO
        . . IF DL(X)=DL(Y)!(DL(Y)?.P) DO
        . . . SET DL=DL-1
        . . . KILL DL(Y)
        . . . FOR P=Y:1:DL SET DL(P)=DL(P+1) KILL DL(P+1)
1       DO ENT
        IF '$D(DIAR) DO DIS2^TMGDIS2 GOTO TMGDONE   ;"Sets TMGRESULT
        DO DIS^TMGDIS2      ;"Sets TMGRESULT
        GOTO TMGDONE ;"QUIT from there
 ;
ENT     SET DK(0)=DK,Z="D0,"
        FOR DQ=0:1:DL DO
        . KILL R,M
        . DO
        . . N I SET I=""
        . . FOR  SET I=$O(DI(I)) QUIT:'I  KILL DI(I)
        . . QUIT
        . SET X=0,DQ(0)=DQ,R=-1
        . DO MAKE
        . SET %=0
        . FOR  SET R=$O(R(R)) QUIT:R=""  DO
        . . IF R(R)<2 SET DIS(R)=DIS(R)_" K D"
        SET R=-1
        QUIT
        ;
        ;"----------------------------------------
2       IF X'>DL QUIT:DL(X)'[Y  SET X=X+1 GOTO 2
        SET DL(0)=U_$P(Y,U,2)_DL(0)
        SET P=P-1
22      SET X=X-1
        SET DQ=$F(DL(X),Y)
        SET DL(X)=$E(DL(X),1,DQ-$L(Y))_$E(DL(X),DQ,999)
        GOTO 22:X>1
        QUIT
        ;
        ;"----------------------------------------
C       SET Y=Y_$S(DV="'":" I 'X",1:" I "_$$XFORM("X")_DV)
        DO SD
MAKE    SET DC=DI
        SET DQ=+DQ
        SET X=X+1
        SET Y=$P(DL(DQ),U,X+1)
        QUIT:Y=""
        SET S=+Y
        SET DN=$E("'",Y["'")
        SET Y=DC(S),D=0,DL=0
        IF $D(DJ(DQ,S)) DO
        . SET D=$P(DJ(DQ,S),U,2),DL=+DJ(DQ,S)
        . IF $D(DI(DL)) SET DC=DI(DL)
        SET DQ=DQ(DL)
        SET Z=$P(Z,",",1,D+D+1)_","
        SET DU=$P($P(Y,U),",",DL+1,99)
        SET O=DK(DL)
        SET DV=DN_$P(Y,U,2)
        IF DV?1"''".E SET DV=$E(DV,3,999)
LEV     SET DL=DL+1
        SET DN=$S($D(DE(+DQ,X,DL)):DE(+DQ,X,DL),1:1)
        SET:$G(DI(DL-1))]"" DI(DL)=DI(DL-1)
        IF DU<0,$D(DY(-DU)) GOTO X
        IF DU<0 SET Y=DA(-DU) GOTO C
        SET N=$P(^DD(O,+DU,0),U,4)
        SET DE=$P(N,";",1)
        SET Y=$P(N,";",2)
        IF Y="" SET Y="D"_D GOTO M
        IF $P(^(0),U,2)["C" SET Y=$P(^(0),U,5,99) GOTO C
        SET:+DE'=DE DE=""""_DE_""""
        SET Z=Z_DE
        SET E="$G("_DC_Z_"))"
        IF Y SET Y="$P("_E_",U,"_Y_")" GOTO M
        IF Y'=0 SET Y=$E(Y,2,99) SET:$P(Y,",",2)=+Y Y=+Y SET Y="$E("_E_","_Y_")" GOTO M
        FOR Y=65:1 SET M=DQ_$C(Y) QUIT:'$D(DIS(M))
        SET D=D+1
        SET Y="S D"_D_"=+$O("_DC_Z_",0)) X DIS("""_M_""") I $T"
        DO SD
        IF $D(DIAR) SET DIAR(DIARF,DQ)="X DIS("""_M_"A"")"
        SET DQ=M
        SET DIS(DQ)="F  X DIS("""_DQ_"A"") X:D"_D_"'>0 ""IF "_(DN=3)_""" Q:"_$E("'",DN>1)_"$T  S D"_D_"=$O("_DC_Z_",D"_D_")) Q:D"_D_"'>0"
WP      SET DQ=DQ_"A"
        SET DQ(DL)=DQ
        IF DU'["," SET DIS(DQ)="I "_$$XFORM("$G(^(D"_D_",0))")_DV GOTO MAKE
        SET O=+$P(^(0),U,2),DK(DL)=O,Z=Z_",D"_D_","
N       SET DU=$P(DU,",",2,99)
        GOTO LEV
 ;
M        DO  SET Y=Y_DV DO SD GOTO MAKE
VARPOINT .IF $P(^DD(O,+DU,0),U,2)["V" SET Y="I "_$$XFORM("$$EXTERNAL^DIDU("_O_","_+DU_","""","_Y_")") QUIT
OUTX     .IF $D(^(2)),$P(^(0),U,2)'["D",DV'["=" SET M=0,Y="S Y="_Y_" "_$$OVFL(^(2))_" I "_$$XFORM("Y") QUIT  ;**GFT 144
SET      .IF $D(DIS(U,S)) SET Y="S Y="_Y_" I $S(Y="""":"""",$D(DIS(U,"_S_",Y)):DIS(U,"_S_",Y),1:"""")" QUIT
         .SET M=Y,Y="I "_$$XFORM(Y)
 ;
XFORM(Y) IF '$D(DIS("XFORM",S)) QUIT Y
         QUIT $P(DIS("XFORM",S),";")_Y_$P(DIS("XFORM",S),";",2)
 ;
SD      IF $D(R(DQ)),R(DQ)>1 SET Y="K D "_Y_" S:$T D=1"
        IF '$D(DIS(DQ)) SET DIS(DQ)=Y QUIT
        IF $L($G(DL(DQ)))*8+$L(DIS(DQ))+$L(Y)>180 DO
        . SET Y=$$OVFL(Y)_" I $T"
        . IF $L(Y)+$L(DIS(DQ))>235 SET DIS(DQ)=$$OVFL(DIS(DQ))_" IF "
        SET DIS(DQ)=DIS(DQ)_" "_Y
        QUIT
 ;
OVFL(Y) N I,%
        FOR I=1:1 SET %=DQ_"@"_IF QUIT:'$D(DIS(%))
        SET DIS(%)=Y
        QUIT "X DIS("""_%_""")"
 ;
X       SET D=DY(-DU),O=+D,DC=U_$P(D,U,2) FOR %=66:1 SET M=DQ_$C(%) QUIT:'$D(DIS(M))
        IF $P(D,U,3) DO
        . SET M=DQ_U_$P(D,U,3)
        . SET Y="S DIXX="""_M_""" "_$P("X ""I 0"" ^I 1 ",U,DN=3+1)_$P(D,U,4,99)_" I $T"
        . SET R(M)=DN
        ELSE  SET Y=$P(D,U,4,99)_" S D0=D(0) X DIS("""_M_""") S D0=I(0,0) I $T"
        DO SD
        SET DQ=M
        SET DI(DL)=DC
        SET DK(DL)=+D
        SET DQ(DL)=DQ
        SET D=0
        SET Z="D0,"
        GOTO N
        ;
TMGDONE QUIT
