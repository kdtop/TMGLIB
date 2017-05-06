TMGDIB  ;SFISC/GFT,XAK-TMG VERSION OF CREATE A NEW FILE ;9JUN2003, 19 JAN 2011, 2/2/14
        ;;1.0;TMG-LIB;**1**;6/24/15
        ;
        ;"Original file header below
        ;"DIB ;SFISC/GFT,XAK-CREATE A NEW FILE ;9JUN2003
        ;";;22.0;VA FileMan;**107,1002**;Mar 30, 1999
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
        ;"
        ;"W !!
        KILL DLAYGO,DTOUT
        DO W^DICRW
        GOTO Q:$D(DTOUT)
        KILL DICS,DIA
        IF Y<0 GOTO Q3  ;"QUIT
1       I '$D(@(DIC_"0)")) DO  GOTO Q3
        . W !!,$C(7),"DATA GLOBAL DOES NOT EXIST!"
        . K DIC
        I $P($G(^DD(+$P(@(DIC_"0)"),U,2),0,"DI")),U,2)["Y" DO  QUIT
        . W !!,$C(7),"RESTRICTED"_$S($P(^("DI"),U)["Y":" (ARCHIVE)",1:"")_" FILE - NO EDITING ALLOWED!"
        IF $D(@(DIC_"0)")) DO
        . SET DIA=DIC
        . SET X=^(0)
        . SET (DI,J(0),DIA("P"))=+$P(X,U,2)
        DO QQ
        SET DR=""
        SET (L,DRS,DIAP,DB,DSC)=0
        SET F=-1
        SET I(0)=DIA,DXS=1
        IF $O(^DD(DI,.01))>0 DO
        . DO EN^DIA
        I $D(DR) GOTO ^DIA2
Q       K DI,DLAYGO,DIA,I,J
QQ      K ^UTILITY($J),DIAT,DIAB,DIZ,DIAO,DIAP,DIAA,IOP,DSC,DHIT,DRS,DIE,DR,DA,DG,DIC,F,DP,DQ,DV,DB,DW,D,X,Y,L,DIZZ
Q3      Q
 ;
DIE(TMGFILEN)  ;"
        ;"Purpose: Fill in required field for file.  Will also create a free text .01 field.
        ;"Input: TMGFILEN -- the number of the stubbed-in file to fill out, as created by ^DIC
        SET F=TMGFILEN
        SET (DG,X)="^DIZ("_F_","
        ;"IF DUZ(0)="@" DO
        ;". W !!,"INTERNAL GLOBAL REFERENCE: "_DG
        ;". R "// ",X:DTIME
        ;". S:'$T X="^"
        ;". IF X="" SET X=DG
        ;". I X?."?" W !,"TYPE A GLOBAL NAME, LIKE '^GLOBAL(' OR '^GLOBAL(4,'",!,"OR JUST HIT 'RETURN' TO STORE DATA IN '"_DG_"'" G DIE
        ;
        ;"IF X?1"^".E S X=$P(X,U,2,9) I X?.P G ABORT
        ;"I X?1.AN W $C(7)_"  ??" G DIE
        SET X=TMGGL
        ;
        SET DG=X
        DO VALROOT(.X,.%,.TMGRESULT)
        ;"IF %'=1 G DIE:DUZ(0)="@"&(DG'=X),ABORT
        IF %'=1 GOTO ABORT
        ;
        ;"W !
        ;"W:DG'=X !?2,"Global reference selected: ^"_X,!
        ;"S DG=U_X
 ;
SET     ;"D WAIT^DICD
        SET $P(^DIC(F,0),U,2)=F
        SET ^("%A")=DUZ_U_DT
        SET X=$P(^(0),U,1)
        SET ^(0,"GL")=DG
        IF DUZ(0)]"" F %="DD","DEL","RD","WR","LAYGO","AUDIT" DO
        . SET ^DIC(F,0,%)=DUZ(0)
        IF DUZ(0)'="@",$S($D(^VA(200,"AFOF")):1,1:$D(^DIC(3,"AFOF"))) D SET1
        SET %=""
        IF @("$D("_DG_"0))") S %=^(0)
        SET @(DG_"0)=X_U_F_U_$P(%,U,3,9)")
        KILL ^DD(F)
        SET ^(F,0)="FIELD^^.01^1"
        SET ^DD(F,.01,0)="NAME^RF^^0;1^K:$L(X)>30!(X?.N)!($L(X)<3)!'(X'?1P.E) X"
        SET ^(3)="NAME MUST BE 3-30 CHARACTERS, NOT NUMERIC OR STARTING WITH PUNCTUATION"
        ;"W !?5,"A FreeText NAME Field (#.01) has been created."
        SET DA="B"
        SET ^DD(F,.01,1,0)="^.1"
        SET ^(1,0)=F_U_DA
        SET X=DG_""""_DA_""",$E(X,1,30),DA)"
        SET ^(1)="S "_X_"=""""",^(2)="K "_X
        SET DIK="^DIC("
        SET DA=F
        DO IX1^DIK
        SET DLAYGO=F
        SET DIK="^DD(DLAYGO,"
        SET DA=.01,DA(1)=DLAYGO
        DO IX1^DIK
        QUIT
 ;
ABORT   ;Delete file and abort
        W !!?9,$C(7)_"No NEW file created!"
        S DIK="^DIC(",DA=F
        K DG
        G ^DIK
 ;
VALROOT(X,%,TMGRESULT) ;Validate the root in X
        ;Returns:
        ;  X = open root
        ;  % = 0 : invalid root  PASS BY REFERENCE
        ;      1 : valid root
        ;  TMGRESULT: PASS BY REFERENCE.  AN OUT PARAMETER
        ;
        N CREF,FNUM,N,OREF,PROMPT,QLEN,ROOT
        ;
        S (OREF,X)=$$OREF^DILF(X)
        S:$E(OREF)=U OREF=$E(OREF,2,999)
        ;
        ;Check syntax
        IF OREF?1(1A,1"%").AN1"("
        ELSE  I OREF?1(1A,1"%").AN1"("1.E1","
        ELSE  I OREF?1"["1.E1"]"1(1A,1"%").AN1"("
        ELSE  I OREF?1"["1.E1"]"1(1A,1"%").AN1"("1.E1","
        ELSE  I OREF?1"|"1.E1"|"1(1A,1"%").AN1"("
        ELSE  I OREF?1"|"1.E1"|"1(1A,1"%").AN1"("1.E1","
        ELSE  DO  QUIT
        . ;"W $C(7)_"  ?? Bad syntax"
        . SET TMGRESULT="-1^File Global Reference (GL) has bad syntax: '^"_OREF_"'"
        . SET %=0
        ;
        SET CREF=U_$$CREF^DILF(OREF)
 ;
        ;Check whether files stored in ancestors
        SET %=1
        SET QLEN=$QL($NA(@CREF))
        FOR N=QLEN:-1:0 D  Q:'%
        . SET ROOT=$NA(@CREF,N)
        . QUIT:ROOT="^DIC"&(N'=QLEN)
        . SET FNUM=+$P($P($G(@ROOT@(0)),U,2),"E")
        . IF FNUM DO  Q:'%
        . . SET OROOT=$$OREF^DILF(ROOT)
        . . IF $G(^DIC(FNUM,0,"GL"))=OROOT DO  QUIT
        . . . SET TMGRESULT="-1^File Global Reference (GL) ERROR -- "_OROOT_" already used by File #"_FNUM_"!"
        . . . SET %=0
        . IF N=QLEN,$O(@CREF@(0))]"" DO  QUIT
        . . ;"W !,$C(7)
        . . SET PROMPT=" -- ^"_OREF_" already exists!"
        . . SET TMGRESULT="-1^File Global Reference (GL) ERROR -- ^"_OREF_" already exists!"
        . . SET %=0
        . . ;"I DUZ(0)'="@" S %=0 W !,"  ERROR"_PROMPT
        . . ;"E  D YN("  WARNING"_PROMPT_"  --OK",.%)
        QUIT
 ;
 ;"YN(PROMPT,%) ;Prompt yes/no
        ;"N DIR,DTOUT,DUOUT,DIRUT,DIROUT,X,Y
        ;"S DIR(0)="Y"
        ;"S:$G(PROMPT)]"" DIR("A")=PROMPT
        ;"S DIR("B")="No"
        ;"D ^DIR
        ;"S %=Y=1
        ;"Q
 ;
EN      ;"Enter here when the user is allowed to select his fields
        SET DIC=DIE
        IF DIC SET DIC=$S($D(^DIC(DIC,0,"GL")):^("GL"),1:"")
        DO 1:DIC]""
        KILL DIC
        QUIT
 ;
SET1 ;
        IF $D(^VA(200,"AFOF")) DO
        . SET:'$D(^VA(200,DUZ,"FOF",0)) ^(0)="^200.032PA^"_+F_"^1"
        . SET ^(+F,0)=F_"^1^1^1^1^1^1"
        IF $D(^DIC(3,"AFOF")) DO
        . SET:'$D(^DIC(3,DUZ,"FOF",0)) ^(0)="^3.032PA^"_+F_"^1"
        . SET ^(+F,0)=F_"^1^1^1^1^1^1"
        SET DIK=$S($D(^VA(200)):"^VA(200,DUZ,""FOF"",",1:"^DIC(3,DUZ,""FOF"",")
        SET DA=F
        SET DA(1)=DUZ
        DO IX1^DIK
        QUIT
