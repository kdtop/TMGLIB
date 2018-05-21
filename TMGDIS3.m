TMGDIS3 ;TMG/kst/Custom version of DIS3 ;03/25/06 ; 5/15/10 6:04pm, 2/2/14
         ;;1.0;TMG-LIB;**1**;01/01/06
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
         
        ;"---- Prior header below --------
DIS3    ;SFISC/SEARCH - PROGRAMMER ENTRY POINT ;12/16/93  13:16
        ;;22.0;VA FileMan;;Mar 30, 1999
        ;Per VHA Directive 10-93-142, this routine should not be modified.
EN      ;
        NEW DIQUIET,DIFM
        SET L=$G(L)
        SET DIFM=+L
        DO CLEAN^DIEFU,INIT^DIP
        SET:$G(DIC) DIC=$G(^DIC(DIC,0,"GL"))
        GOTO QER1:$G(DIC)=""
        NEW DK
        SET DK=+$P($G(@(DIC_"0)")),U,2)
        GOTO QER1:'DK
        NEW DISV,Y
        do
        . NEW DIC,X,DIS
        . SET Y=-1,DIS=$G(DISTEMP)
        . QUIT:DIS=""
        . SET X=$S($E(DIS)="[":$P($E(DIS,2,99),"]"),1:DIS)
        . SET DIC="^DIBT("
        . SET DIC(0)="Q"
        . SET DIC("S")="I '$P(^(0),U,8),$P(^(0),U,4)=DK,$P(^(0),U,5)=DUZ!'$P(^(0),U,5),$D(^(""DIS""))"
        . DO ^DIC
        . Q
        SET DISV=+Y
        IF Y<0 SET DIC="DISTEMP" GOTO QER
        NEW DISTXT
        SET %X="^DIBT(DISV,""DIS"",",%Y="DIS("
        DO %XY^%RCR
        SET %X="^DIBT(DISV,""O"",",%Y="DISTXT("
        DO %XY^%RCR
        KILL ^DIBT(DISV,1)
        DO EN1^DIP
        GOTO EXIT
        ;"==========================================
QER1    SET DIC="DIC"
QER     DO BLD^DIALOG(201,DIC)
        DO:'$G(DIQUIET) MSG^DIALOG()
        DO Q^DIP
EXIT    KILL DIC,DISTEMP
        Q
        ;"DIALOG #201  'The input variable...is missing or invalid.'
