TMGRPC1F ;TMG/kst-RPC Functions ;08/11/10, 2/2/14
         ;;1.0;TMG-LIB;**1**;08/11/10
 ;
 ;"TMG RPC FUNCTIONS especially related to ADT in CPRS -- CONTINUED
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;" RPC -- Public Functions.
 ;"=======================================================================
 ;"
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"
 ;"=======================================================================
 ;"Dependancies:
 ;"=======================================================================
 ;
 ;
 ;"DGPMV20 ;ALB/MIR - DISPLAY DATES FOR SELECTION ; 27 APR 90
 ;";5.3;Registration;**40**;Aug 13, 1993
DGPMV20 ;"  From ^DGPMV20  -- DISPLAY DATES FOR SELECTION
        DO ADDINFO^TMGRPC1E("CHOOSE FROM:")
        F I=1:1:6 Q:'$D(^UTILITY("DGPMVN",$J,I))  D WR
        Q
        ;
WR      ;
        NEW TMGSTR SET TMGSTR=""
        S DGX=$P(^UTILITY("DGPMVN",$J,I),"^",2,20)
        S DGIFN=+^(I)
        S Y=+DGX
        X ^DD("DD")
        SET TMGSTR=$J(I,2)_">  "_Y
        ;"W !,$J(I,2),">  ",Y
        I 'DGONE DO
        . SET TMGSTR=TMGSTR_"  "_$S('$D(^DG(405.1,+$P(DGX,"^",4),0)):"",$P(^(0),"^",7)]"":$P(^(0),"^",7),1:$E($P(^(0),"^",1),1,20))
        . ;"W ?27,$S('$D(^DG(405.1,+$P(DGX,"^",4),0)):"",$P(^(0),"^",7)]"":$P(^(0),"^",7),1:$E($P(^(0),"^",1),1,20))
        I DGPMT=4!(DGPMT=5) DO
        . S DGPMLD=$S($D(^DGPM(+DGIFN,"LD")):^("LD"),1:"")
        D @("W"_DGPMT)
        DO ADDINFO^TMGRPC1E(TMGSTR) SET TMGSTR=""
        K DGIFN,DGX,DGPMLD
        Q
        ;
W1      ;"Display for Admitions
        SET TMGSTR=TMGSTR_" TO:  "_$S($D(^DIC(42,+$P(DGX,"^",6),0)):$E($P(^(0),"^",1),1,17),1:"")
        ;"W ?50,"TO:  ",$S($D(^DIC(42,+$P(DGX,"^",6),0)):$E($P(^(0),"^",1),1,17),1:"")
        I $D(^DG(405.4,+$P(DGX,"^",7),0)) DO
        . SET TMGSTR=TMGSTR_" ["_$E($P(^(0),"^",1),1,10)_"]"
        . ;"W " [",$E($P(^(0),"^",1),1,10),"]"
        I $P(DGX,"^",18)=9 DO
        . DO ADDINFO^TMGRPC1E(TMGSTR)
        . SET TMGSTR="  FROM:  "_$S($D(^DIC(4,+$P(DGX,"^",5),0)):$P(^(0),"^",1),1:"")
        . ;"W !?23,"FROM:  ",$S($D(^DIC(4,+$P(DGX,"^",5),0)):$P(^(0),"^",1),1:"")
        Q
        ;
W2      ;"Display for Transfers
        Q:"^25^26^"[("^"_$P(DGX,"^",18)_"^")
        I "^43^45^"[("^"_$P(DGX,"^",18)_"^") DO  QUIT
        . SET TMGSTR=TMGSTR_" TO:  "_$S($D(^DIC(4,+$P(DGX,"^",5),0)):$E($P(^(0),"^",1),1,18),1:"")
        . ;"W ?50,"TO:  ",$S($D(^DIC(4,+$P(DGX,"^",5),0)):$E($P(^(0),"^",1),1,18),1:"")
        I "^1^2^3^"[("^"_$P(DGX,"^",18)_"^") DO  QUIT
        . SET TMGSTR=TMGSTR_" RETURN:  "
        . ;"W ?50,"RETURN:  "
        . S Y=$P(DGX,"^",13) X ^DD("DD")
        . SET TMGSTR=TMGSTR_Y
        . ;"W Y
        SET TMGSTR=TMGSTR_" TO:  "_$S($D(^DIC(42,+$P(DGX,"^",6),0)):$E($P(^(0),"^",1),1,17),1:"")
        ;"W ?50,"TO:  ",$S($D(^DIC(42,+$P(DGX,"^",6),0)):$E($P(^(0),"^",1),1,17),1:"")
        I $D(^DG(405.4,+$P(DGX,"^",7),0)) DO
        . SET TMGSTR=TMGSTR_" ["_$E($P(^(0),"^",1),1,10)_"]"
        . ;"W " [",$E($P(^(0),"^",1),1,10),"]"
        Q
        ;
W3      ;"Display for Discharges
        I $P(DGX,"^",18)=10 DO
        . SET TMGSTR=TMGSTR_" TO:  "_$S($D(^DIC(4,+$P(DGX,"^",5),0)):$E($P(^(0),"^",1),1,18),1:"")
        . ;"W ?50,"TO:  ",$S($D(^DIC(4,+$P(DGX,"^",5),0)):$E($P(^(0),"^",1),1,18),1:"")
        Q
        ;
W4      ;"Display for Check-ins
        S X=""
        I $P(DGX,"^",18)=5 DO
        . S X=$S($D(^DIC(42,+$P(DGX,"^",6),0)):^(0),1:"")
        I $P(DGX,"^",18)=6 DO
        . S X=$S($D(^DIC(4,+$P(DGX,"^",5),0)):^(0),1:"")
        SET TMGSTR=TMGSTR_" TO:  "_$E($P(X,"^",1),1,20)
        ;"W ?55,"TO:  ",$E($P(X,"^",1),1,20)
        I DGPMLD]"" DO
        . DO ADDINFO^TMGRPC1E(TMGSTR)
        . SET TMGSTR="REASON:  "_$S($D(^DG(406.41,+DGPMLD,0)):$E($P(^(0),"^",1),1,20),1:"")
        . ;"W !?7,"REASON:  ",$S($D(^DG(406.41,+DGPMLD,0)):$E($P(^(0),"^",1),1,20),1:"")
        . SET TMGSTR=TMGSTR_"COMMENTS:  "_$P(DGPMLD,"^",2)
        . ;"W ?35,"COMMENTS:  ",$P(DGPMLD,"^",2)
        Q
        ;
W5      ;"Display for Check-outs
        IF DGONE ;"W ?30
        ELSE  DO
        . DO ADDINFO^TMGRPC1E(TMGSTR)
        . SET TMGSTR=" "
        . ;"W !?7
        SET TMGSTR=TMGSTR_" DISPOSITION: "_$S($P(DGPMLD,"^",3)="a":"ADMITTED",$P(DGPMLD,"^",3)="d":"DISMISSED",1:"")
        ;"W "DISPOSITION: ",$S($P(DGPMLD,"^",3)="a":"ADMITTED",$P(DGPMLD,"^",3)="d":"DISMISSED",1:"")
        Q
        ;
W6      ;"Display for T.S. Transfers
        ;"IF DGONE W ?30
        ;"ELSE  W !?7
        SET TMGSTR=TMGSTR_$S($D(^DIC(45.7,+$P(DGX,"^",9),0)):$E($P(^(0),"^",1),1,18),1:"")
        ;"W "SPECIALTY:  ",$S($D(^DIC(45.7,+$P(DGX,"^",9),0)):$E($P(^(0),"^",1),1,18),1:"")
        IF DGONE DO
        . DO ADDINFO^TMGRPC1E(TMGSTR) SET TMGSTR=""
        . ;"W !?7
        ELSE  ;"W ?37
        SET TMGSTR=TMGSTR_$S($D(^VA(200,+$P(DGX,"^",8),0)):$E($P(^(0),"^",1),1,15),1:"")
        ;"W "PROVIDER :  ",$S($D(^VA(200,+$P(DGX,"^",8),0)):$E($P(^(0),"^",1),1,15),1:"")
        IF DGONE DO
        . DO ADDINFO^TMGRPC1E(TMGSTR) SET TMGSTR=""
        . ;"W !?7
        ELSE  ;"W ?33
        SET TMGSTR=TMGSTR_"ATTENDING:  "_$S($D(^VA(200,+$P(DGX,"^",19),0)):$E($P(^(0),"^",1),1,15),1:"")
        ;"W "ATTENDING:  ",$S($D(^VA(200,+$P(DGX,"^",19),0)):$E($P(^(0),"^",1),1,15),1:"")
        SET DGDX=$S($D(^DGPM(+DGIFN,"DX",1,0)):$E(^(0),1,30),1:"")
        IF DGDX]"" DO
        . IF DGONE ;"W ?37
        . ELSE  DO
        . . DO ADDINFO^TMGRPC1E(TMGSTR) SET TMGSTR=" "
        . . ;"W !?7
        . SET TMGSTR=TMGSTR_"DX:  "_DGDX
        . ;"W "DX:  ",DGDX
        KILL DGDX
        Q
        ;
 ;"=======================================================================
 ;"DGPMV3 ;ALB/MIR - ENTER TRANSACTION INFORMATION; 8 MAY 89 ; 5/23/06 8:32am
 ;";;5.3;Registration;**34,54,62,95,692,715**;Aug 13, 1993
 ;
DGPMV3  ;" ENTER TRANSACTION INFORMATION
        K ^UTILITY("DGPM",$J)
        D NOW^%DTC S DGNOW=%
        SET DGPMHY=DGPMY,DGPMOUT=0
        IF 'DGPMN GOTO DT
        S X=DGPMY
        S DGPM0ND=DGPMY_"^"_DGPMT_"^"_DFN_"^^^^^^^^^^^"_$S("^1^4^"[("^"_DGPMT_"^"):"",1:DGPMCA)
        ;
        I DGPMT=1 S $P(DGPM0ND,"^",25)=$S(DGPMSA:1,1:0)
        ;"-- provider change
        I DGPMT=6,$D(DGPMPC) DO
        . S DGPM0ND=$$PRODAT(DGPM0ND)  ;"Not interactive
        D NEWL
        IF Y'>0 GOTO QV3
        SET (DA,DGPMDA)=+Y
        IF (DGPMT=1)!(DGPMT=4) DO
        . SET DGPMCA=DA
        . SET DGPMAN=^DGPM(DA,0)
        DO VAR
        GOTO DR
        ;
DT      DO VAR
        IF DGPM1X GOTO DR
        SET (DGPMY,Y)=DGPMHY X ^DD("DD")
        W !,DGPMUC," DATE: ",Y,"// " R X:DTIME  ;"FINISH!!
        IF '$T!(X["^") GOTO QV3
        IF X="" G DR
        SET %DT="SRXE",%DT(0)="-NOW"
        IF X["?"!(Y<0) D HELP^%DTC G DT ;"FINISH
        IF X="@" GOTO OKD
        DO ^%DT
        IF Y<0 D HELP^%DTC G DT  ;"FINISH
        K %DT
        SET DGPMY=Y
        IF (X]"")&(DGPMY'=+DGPMP) DO
        . DO CHK^DGPMV30
        I $D(DGPME) DO  GOTO DT  ;"FINISH
        . Set DGPMY=DGPMHY
        . W !,DGPME
        . K DGPME
        ;
DR      ;"Select input template for transaction type
        S DIE="^DGPM("
        I "^1^4^6^"[("^"_DGPMT_"^"),DGPMN S DIE("NO^")=""
        S DGODSPT=$S('$D(^DGPM(DGPMCA,"ODS")):0,^("ODS"):1,1:0)
        S DR=$S(DGPMT=1:"[DGPM ADMIT]",DGPMT=2:"[DGPM TRANSFER]",DGPMT=3:"[DGPM DISCHARGE]",1:"")
        S:(DR="") DR=$S(DGPMT=4:"[DGPM CHECK-IN LODGER]",DGPMT=5:"[DGPM LODGER CHECK-OUT]",1:"")
        S:(DR="") DR=$S(DGPMT=6:"[DGPM SPECIALTY TRANSFER]",1:"")
        IF DR="" GOTO QV3
        K DQ,DG
        D ^DIE  ;"FINISH
        K DIE
        I $D(Y)#2 S DGPMOUT=1
        K DGZ
        S (^UTILITY("DGPM",$J,DGPMT,DGPMDA,"A"),DGPMA)=$S($D(^DGPM(DGPMDA,0)):^(0)_$S($G(^("DIR"))'="":U_^("DIR"),1:""),1:"")
        IF DGPMT'=4 DO
        . DO @("^DGPMV3"_DGPMT)
        I DGPMT=4,$S('$D(^DGPM(DGPMDA,"LD")):1,'$P(^("LD"),"^",1):1,1:0) DO
        . S DIK="^DGPM(",DA=DGPMDA
        . W !,"Incomplete check-in...deleted"
        . D ^DIK
        . K DIK
        . S DGPMA=""
        S (^UTILITY("DGPM",$J,DGPMT,DGPMDA,"A"),DGPMA)=$G(^DGPM(DGPMDA,0))_$S($G(^("DIR"))'="":U_^("DIR"),1:"")
        I DGPMT=6 DO
        . S Y=DGPMDA
        . D AFTER^DGPMV36  ;"FINISH!!
        ;
EVENTS  ;
        I DGPMT=4!(DGPMT=5) DO
        . D RESET^DGPMDDLD   ;"FINISH!!
        I DGPMT'=4&(DGPMT'=5) DO
        . D RESET^DGPMDDCN   ;"FINISH!!
        . I (DGPMT'=6) D SI^DGPMV33  ;"FINISH!!
        IF DGPMA]"" DO
        . DO START^DGPWB(DFN)  ;"FINISH!!
        D EN^DGPMVBM ;"notify building management IF room-bed change
        S DGOK=0 F I=0:0 S I=$O(^UTILITY("DGPM",$J,I)) Q:'I!DGOK  DO
        . F J=0:0 S J=$O(^UTILITY("DGPM",$J,I,J)) Q:'J!DGOK  DO
        . . I ^(J,"A")'=^("P") S DGOK=1 Q
        I DGOK D ^DGPMEVT ;"Invoke Movement Event Driver   ;FINISH!!
        GOTO QV3
        ;
OKD     K %DT
        W !
        S DGPMER=0
        S (^UTILITY("DGPM",$J,DGPMT,DGPMDA,"P"),DGPMP)=^DGPM(DGPMDA,0)
        S Y=DGPMDA
        IF DGPMT=6 DO
        . DO PRIOR^DGPMV36  ;"FINISH
        D @("D"_DGPMT_"^DGPMVDL"_$S(DGPMT>2:1,1:""))
        IF DGPMER GOTO QV3
        W !,"Are you sure you want to delete this movement" S %=2 D YN^DICN  ;"FINISH
        G Q:%<0
        G DT:%=2
        I '% DO  G OKD
        . W !?5,"Answer yes to delete this ",DGPMUC," or no to continue"
        D @(DGPMT_"^DGPMVDL"_$S(DGPMT>2:1,1:""))
        I DGPMT'=3,(DGPMT'=5) DO
        . S DIK="^DGPM(",DA=DGPMDA
        . D ^DIK:DGPMDA
        S (^UTILITY("DGPM",$J,DGPMT,DGPMDA,"A"),DGPMA)=$S($P(DGPMP,"^",18)'=47:"",1:^DGPM(+DGPMDA,0))
        I DGPMT=6 DO
        . S Y=DGPMDA
        . D AFTER^DGPMV36  ;"FINISH
        I DGPMDA,$O(^DGPM("APHY",DGPMDA,0)) DO
        . S DIK="^DGPM(",DA=+$O(^(0))
        . I $D(^DGPM(+DA,0)) DO
        . . S ^UTILITY("DGPM",$J,6,DA,"P")=^(0),^("A")="",Y=DA
        . . D PRIOR^DGPMV36  ;"FINISH
        . . D ^DIK
        . . S Y=DA
        . . D AFTER^DGPMV36 ;"FINISH
        G EVENTS
        ;
VAR     ;"Set up variables
        ;"Modified in patch dg*5.3*692 to include privacy indicator node "DIR"
        S DA=DGPMDA
        S (^UTILITY("DGPM",$J,DGPMT,DGPMDA,"P"),DGPMP)=$S(DGPMN=1:"",1:$G(^DGPM(DA,0))_$S($G(^("DIR"))'="":U_^("DIR"),1:""),1:"") ;DGPMP=Before edit
        I DGPMT=6 DO
        . S Y=DGPMDA
        . D PRIOR^DGPMV36  ;"FINISH
        S DGX=DGPMY+($P(DGPMP,"^",22)/10000000)
        S X=$O(^DGPM("APMV",DFN,DGPMCA,(9999999.9999999-DGX)))
        S X1=$O(^DGPM("APMV",DFN,DGPMCA,+X,0))
        S DGPM0=$S($D(^DGPM(+X1,0)):^(0),1:"") ;"DGPM0=prior movement
        S X=$O(^DGPM("APCA",DFN,DGPMCA,+DGX))
        S X=$O(^(+X,0))
        S DGPM2=$S($D(^DGPM(+X,0)):^(0),1:"") ;"DGPM2=next movement
        S DGPMABL=0
        I DGPM2,$D(^DG(405.2,+$P(DGPM2,"^",18),"E")) DO
        . S DGPMABL=+^("E") ;"is the next movement an absence?
        I DGPMT=6 DO
        . S Y=DGPMDA
        . D PRIOR^DGPMV36  ;"FINISH
        Q
        ;
NEWL    ;"Entry point to add a NEW entry to ^DGPM
        D NEW^DGPMV301 ; continuation of routine DGPMV3 in DGPMV301  ;"FINISH!!
        Q
 ;
PRODAT(NODE) ;"-- This function will add the ward and other data from the
        ; previous TS movement to the provider TS movement.
        ;
        NEW X,Y
        SET Y=NODE
        SET X=$O(^DGPM("ATS",DFN,DGPMCA,9999999.9999999-$P(NODE,U)))
        IF X DO
        . SET X=$O(^(X,0))
        . IF X DO
        . . SET X=$O(^(X,0))
        . . IF X SET X=^DGPM(X,0)
        SET $P(Y,U,4)=$P(X,U,4)
        SET $P(Y,U,9)=$P(X,U,9)
        Q Y
 ;
QV3     IF $D(DGPMBYP) SET DGPMBYP=DGPMDA
        K DGIDX,DGOWD,DGOTY ;"variables SET in DGPMGLC - G&L corrections
        K DGODS,DGODSPT ;"ods variables
        K %DT,DA,DGER,DGNOW,DGOK,DGPM0,DGPM0ND,DGPM2,DGPMA,DGPMAB,DGPMABL,DGPMDA
        K DGPMER,DGPMHY,DGPMNI,DGPMOC,DGPMOS,DGPMOUT,DGPMP,DGPMPHY,DGPMPHY0,DGPMPTF
        K DGPMSP,DGPMTYP,DGPMTN,DGPMWD,DGT,DGSV,DGX,DGX1
        K DIC,DIE,DIK,DR,I,I1,J,K,X,X1,X2,Y,^UTILITY("DGPM",$J)
        Q
        ;
Q      Q    ;//kt 2/26/15        
