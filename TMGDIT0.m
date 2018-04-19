TMGDIT0 ;SFISC/XAK-PREPARE TO XFR ;09:21 AM  Jul 19, 1988, 2/2/14
        ;;1.0;TMG-LIB;**1**;6/24/15
        ;;22.0;VA FileMan;;Mar 30, 1999
 ;"Copied from FM, for customization
 
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 

        K Y,DIC
        S DIT=DDF(1)
        S DIC=L
        S DIC(0)="EQLAM"
        S X="DATA INTO WHICH "
        D LK  ;"--> ^DIC  Asks for destination record, and creates NEW IF needed. Y=destination record
        G Q:Y<0  ;"abort IF requested
        S DFR=+Y
        S DTO(1)=DIC_+Y_","  ;"DTO is DESTINATION info array
        ;"At this point we have:
        ;" DTO=^VA(200,
        ;" DTO(1) = ^VA(200,166,
        S DIC(0)="EQAM",X="FROM ",DIC("S")="I Y-"_+Y
        D LK  ;"--> ^DIC  Asks for source record.
        G Q:Y<0  ;"abort IF requested
S       S %=2 ;"default to NO delete
        ;"//ktW !,"   WANT TO DELETE THIS ENTRY AFTER IT'S TRANSFERRED"
        ;"//ktD YN^DICN  ;"%=1 for YES, %=2 for NO
        G Q:%<0 ;"abort IF requested
        S DH=2-%  ;"DH=1 for delete, DH=0 for NO delete
        I '% DO  GOTO S  ;"loop back
        . DO F^TMGDIT
        S ^UTILITY("DIT",$J,+Y)=DFR_";"_$E(DIC,2,999)
        S DTO=0
        S (D0,DA)=+Y
        S DIK=DIC
        S DFR(1)=DIC_DA_","
        K DIC
        D WAIT^DICD  ;"Let me put you on hold...
GO      D GO^DITR  ;"Find fields to XRef
        S DIT=DH
        D KL^TMGDIT
        D ^DIK:DH  ;"kill record IF prev requested (I think)
        S DA=DFR
        K DFR
        D IX1^DIK

        S DH=DIT
        SET %=2 ;"//kt added
        ;"//ktD ASK^DITP  ;"Ask, redirect pointers?  %: 1=yes, 2=no
        ;"//ktD PTS^DITP:%=1
Q       G Q^TMGDIT
        ;
LK      S DIC("A")="TRANSFER "_X_DFL
        G ^DIC
        ;
EN      ; PROGRAMMER CALL
        ; DIT("F") = GLOBAL ROOT OR FILE # OF FILE TO TRANSFER FROM
        ; DIT("T") = GLOBAL ROOT OR FILE # OF FILE TO TRANSFER TO
        ; DA("F")  = ENTRY # IN FILE TO TRANSFER FROM
        ; DA("T")  = ENTRY # IN FILE TO TRANSFER TO
        ;" //kt: Note: this does not delete the FROM record
        ;
        I '$D(DIT("F"))!'$D(DIT("T"))!'$D(DA("F"))!'$D(DA("T")) G FIN
        S DDF(1)=DIT("F")
        S DDT(0)=DIT("T")
        I 'DDF(1) DO  GOTO FIN:'DDF(1)
        . SET DDF(1)=$PIECE($GET(@(DDF(1)_"0")),"^",2)
        . IF DDF(1)="" SET DDF(1)=0
        . ;"S DDF(1)=$S($D(@(DDF(1)_"0)"))#2:+$P(^(0),U,2),1:0)
        . Q:'DDF(1)
        . S DFR(1)=DIT("F")
        I 'DDT(0) DO  G FIN:'DDT(0) GOTO C
        . S DDT(0)=$S($D(@(DDT(0)_"0)"))#2:+$P(^(0),U,2),1:0)
        . QUIT:'DDT(0)
        . S DTO(1)=DIT("T")
        G FIN:'$D(^DIC(+DDF(1),0,"GL"))
        S DFR(1)=^("GL")
        G FIN:'$D(^DIC(+DDT(0),0,"GL"))
        S DTO(1)=^("GL")
C       S DB=DA("F")
        S (DB1,DFR)=DA("T")
        S DIK=DTO(1)
        I $D(DA(1)) F I=1:1 G:'$D(DA(I)) SET do
        . S DRF(I)=$P(DA(I),",",1)_",1,"
        . S DOT(I)=$P(DA(I),",",2)_",1,"

DON     K DRF,DOT
        S DFR(1)=DFR(1)_DB_","
        S DTO(1)=DTO(1)_DB1_","
        S DKP=1,DMRG=1,DTO=0,DH=0
        G GO

SET     F I=I-1:-1 G:I'>0 DON do
        . S DFR(1)=DFR(1)_DRF(I)
        . S DTO(1)=DTO(1)_DOT(I)
FIN     ;
        K DDF,DFR,DDT,DTO
        Q
