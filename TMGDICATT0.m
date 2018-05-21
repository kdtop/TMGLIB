TMGDICATT ;KST/SFISC/GFT,XAK- TMG VERSION OF DATES, NUMERIC ;6 JAN,2011
         ;;1.0;TMG-LIB;**1**;1/6/11
         ;
         ;"Original file header below
         ;"DICATT0 ;SFISC/GFT,XAK-DATES, NUMERIC ;5/4/93  2:05 PM
         ;";;22.0;VA FileMan;;Mar 30, 1999
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
DIE     KILL Y S DP=0
        FOR  S DL=1,DP=$O(DQ(DP)) Q:DP=""  S:$D(DE(DP)) DG(DP)=DE(DP)
        SET DP=-1
        ;"D DQ^DIED  ;<--- NOTE: answers should have already been loaded into DG(#)
        KILL DQ,DICATTZ
        IF $D(Y)!$D(DTOUT) GOTO CHECK^TMGDICATT
        GOTO @(N_0)
 ;
1       SET %DT="E"
        SET DQ="^I X'?1""DT"".NP D ^%DT S X=Y K:Y<0 X"
        ;"SET DQ(1)="EARLIEST DATE (OPTIONAL)^D^^1"_DQ
        ;"SET DQ(0,2)="S:'$L(X) Y=""CAN"""
        SET DG(1)=TMGSDATE
        ;"SET DQ(3)="LATEST DATE^RD^^3"_DQ_" I $D(X),X<DG(1) K X"
        SET DG(3)=TMGEDATE
        S P="<X!("
        I C[P DO
        . SET DE(1)=$P($P(C,P,2),">X",1)
        . SET DE(3)=$P($P(C,"K:",2),P,1)
        ;"S DQ(4)="CAN DATE BE IMPRECISE (Y/N)^S^Y:YES;N:NO;^4^Q"
        SET DE(4)=$E("YN",$P(C,Q,2)["X"+1)
        ;"SET DQ(4,3)="E.G., WOULD 'FEB, 1980' BE ALLOWED?"
        SET DG(4)=TMGIMPDAT
        ;"SET DQ(5)="CAN TIME OF DAY BE ENTERED (Y/N)^S^Y:YES;N:NO;^5^S:X=""N"" (DG(7),DG(6))=X K:X=""N"" DQ(6)"
        SET DG(5)=TMGTIMEOK
        SET DE(5)=$E("NY",$P(C,Q,2)["T"+1)
        ;"SET DQ(5,3)="CAN USER ENTER TIME ALONG WITH DATE, AS IN 'JULY 20@4:30'?"
        ;"SET DQ(6)="CAN SECONDS BE ENTERED (Y/N)^S^Y:YES;N:NO;^6^S DG(6)=X"
        SET DG(6)=TMGSECOK
        SET DE(6)=$E("NY",$P(C,Q,2)["S"+1)
        ;"SET DQ(7)="IS TIME REQUIRED (Y/N)^S^Y:YES;N:NO;^7^Q"
        ;"SET DQ(7,3)="MUST USER ENTER TIME ALONG WITH DATE"
        ;"SET DQ(0,6)="I X=""N"" S Y=U,DQ=DQ+1"
        SET DE(7)=$E("NY",$P(C,Q,2)["R"+1)
        SET DG(7)=TMGTIMREQ
        S DICATTZ=1
        GOTO DIE
 ;
10      S C="S %DT=""E"_$E("S",DG(6)="Y")_$E("T",DG(5)="Y")_$E("X",DG(4)="N")_$E("R",DG(7)="Y")_""" D ^%DT S X=Y K:"
        FOR X=1,3 G ND:'$D(DG(X)) DO
        . SET Y(X)=$S(DG(X):DG(X)\10000+1700,1:DG(X))
        . IF DG(X)#100 DO
        . . SET Y(X)=DG(X)#100_"/"_Y(X)
        . . I $E(DG(X),4,5) S Y(X)=+$E(DG(X),4,5)_"/"_Y(X)
        I DG(1)]"" DO  GOTO ED
        . S M="TYPE A DATE BETWEEN "_Y(1)_" AND "_Y(3)
        . SET C=C_DG(3)_P_DG(1)_">X) X"
ND      S C=C_"Y<1 X"
ED      S Z="D^",L=DG(5)="Y"*5+7,DG(6)=""
        GOTO H
 ;
2       K DG
        SET DQ("A1")="!(X'["".""&($L(X)>15))!(X["".""&($L($P(+X,"".""))+$L($P(+X,""."",2))>15)) X"
        ;"SET DQ(1)="INCLUSIVE LOWER BOUND^R^^1^K:+X'=X"_DQ("A1")
        SET DG(1)=TMGMINN
        ;"SET DQ(2)="INCLUSIVE UPPER BOUND^R^^2^K:X<DG(1)!(+X'=X)"_DQ("A1")
        SET DG(2)=TMGMAXN
        ;"SET DQ(3)="IS THIS A DOLLAR AMOUNT (Y/N)^S^Y:YES;N:NO;^3^Q"
        SET DG(3)=TMGDOLLAR
        K DQ("A1")
        S P="1""."""
        SET Z=$S(C["$":3,1:+$P(C,P,2))
        SET DE(3)=$E("NY",C["$"+1)
        SET DE(5)=$S(Z:Z-1,1:0)
        ;"SET DQ(0,4)="S:X=""Y"" Y=U,DQ=9,DG(5)=2"
        ;"SET DQ(5)="MAXIMUM NUMBER OF FRACTIONAL DIGITS^RN^^5^K:X'?1N X"
        SET DG(5)=TMGDIGITS
        ;"IF O SET DE(1)=+$P(C,"X<",2),DE(2)=+$P(C,"X>",2)
        GOTO DIE
        ;
20      I DG(1)>DG(2) W $C(7),"??" G 2
        S M="Type a "_$P("Number^Dollar Amount",U,DG(3)="Y"+1)_" between "_DG(1)_" and "_DG(2)_", "_DG(5)_" Decimal Digit"_$E("s",DG(5)'=1)
        S C="K:+X'=X"
        SET T=DG(5)+1
        SET Z="!(X?.E"_P_T_"N.N)"
        I DG(3)="Y",DA-.001 DO
        . SET C="S:X[""$"" X=$P(X,""$"",2) K:X'?"_$P(".""-""",U,DG(1)<0)_".N."_P_".2N"
        . SET Z=""
        SET C=C_"!(X>"_DG(2)_")!(X<"_DG(1)_")"_Z_" X"
        SET L=$L(DG(2)\1)+T-(T=1)
        SET Z="NJ"_L_","_DG(5)_U
        GOTO H
        ;
H       S DIZ=Z
        G ^TMGDICATT1
