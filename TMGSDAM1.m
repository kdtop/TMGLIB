TMGSDAM1 ;TMG/kst/APP For SDAPI AND MAKE APPOINTMENT RPC routines;1/9/09
         ;;1.0;TMG-LIB;**1**;1/9/09
 ;
 ;"NOTE: Original header:
 ;"SDVWAPP ; VWSD VOE APP FOR SDAPI AND MAKE APPOINTMENT RPC routines
 ;"       ;  1/1/07 SDVW*3*2;;;;;Build 8
 ;"
 ;"Moved to this namespace for customization
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"SDAPI(RESULTS,SDARRAY)
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;
SDAPI(RESULTS,SDARRAY)  ;;
        ;"Purpose: SDAPI APP TEST PROGRAM ( RETURN LIST OF APPOINTMENTS)
        ;"VERSION 3.1
        ;"Input: RESULTS  -- PASS BY REFERENCE.  An OUT PARAMETER.
        ;"       SDARRAY -- PASS BY REFERENCE.  Example input:
        ;"          SDARRAY(1)="Nov 6,2006;Nov 9,2006"
        ;"          SDARRAY(2)="VWVOE RADIOLOGY CLINIC" (External Format)
        ;"          SDARRAY(3)="R;I"
        ;"          SDARRAY(4)="100001298" (EXTERNAL PATIENT ID AS SSN)
        ;"          SDARRAY("MAX")=3
        ;"          SDARRAY("FLDS")="1;2;3"
        ;"Result: None (output placed in RESULTS var above)
        ;
        NEW FIRST,SSN,SDDATE,SDLOCATE,PATIENTN,SSNPATN,AJJ3VIS,SDARRAY2,COUNTER
        NEW MSGCTRL,IER,RETURN,ORDRSORT,SSN,SDLOCATE,SDDATE,SDCOUNT,ACKCODE
        ;
        SET MSGCTRL=0
        IF $GET(SDARRAY(1))="" SET RESULTS(0)="UNDEFINED DATE RANGE ELEMENT" QUIT
        IF $GET(SDARRAY(3))="" SET RESULTS(0)="UNDEFINED ELEMENT 3 = "_"R;I" QUIT
        IF ($GET(SDARRAY(2))="")&($GET(SDARRAY(4))="") SET RESULTS(0)="UNDEFINED ELEMENTS 2 AND 4 TOGETHER" QUIT
        IF $GET(SDARRAY("MAX"))="" SET RESULTS(0)="UNDEFINED MAX RETURN ELEMENT" QUIT
        IF $GET(SDARRAY("FLDS"))="" SET RESULTS(0)="UNDEFINED FLDS ELEMENT" QUIT
        ;
        SET IER=$$TRNSDAPI^SDVWHLE2(.SDARRAY,.MSGCTRL)
        IF (IER="OK")&(MSGCTRL'=0) DO
        . SET AJJ3CNT=0
CHKAGAN . IF AJJ3CNT>27 QUIT
        . IF $D(^XTMP(MSGCTRL,"RETURN"))=0 HANG 3 SET AJJ3CNT=AJJ3CNT+1 G CHKAGAN
        . SET RETURN=^XTMP(MSGCTRL,"RETURN") ;" THIS INCLUDES ACK CODE AS AA OR AE
        . SET ACKCODE=$P(RETURN,"^",1)
        . IF ACKCODE="OK" DO
        . . SET ORDRSORT=$P(RETURN,"^",2)
        . . SET SDCOUNT=$P(RETURN,"^",3)
        . . IF SDCOUNT>0 DO
        . . . IF $E(ORDRSORT,1,1)="P" DO
        . . . . SET AJJ3VIS=0
        . . . . FOR  SET AJJ3VIS=$O(^XTMP(MSGCTRL,"SDAMA301",AJJ3VIS)) QUIT:AJJ3VIS=""  DO
        . . . . . SET SDARRAY2=^XTMP(MSGCTRL,"SDAMA301",AJJ3VIS)
        . . . . . SET SSNPATN=$P(SDARRAY2,"^",1)
        . . . . . SET SDLOCATE=$P(SDARRAY2,"^",2)
        . . . . . SET SDDATE=$P(SDARRAY2,"^",3)
        . . . . . SET FIRST=SSNPATN_"^"_SDLOCATE_"^"_SDDATE_"^"
        . . . . . SET SDARRAY2=$P(SDARRAY2,FIRST,2)
        . . . . . SET PATIENTN=$P(SSNPATN,"#",2)
        . . . . . SET SSN=$P(SSNPATN,"#",1)
        . . . . . SET COUNTER=(AJJ3VIS-1)*2
        . . . . . SET RESULTS(COUNTER+1)="APPT/UNSCHED VISIT, PATIENT="_PATIENTN_" SSN="_SSN_" HOSP LOCATION="_SDLOCATE_" DATE/TIME="_SDDATE
        . . . . . SET RESULTS(COUNTER+2)="                   DATA FIELDS="_SDARRAY2
        . . . IF $E(ORDRSORT,1,1)="C" DO
        . . . . SET AJJ3VIS=0
        . . . . FOR  SET AJJ3VIS=$O(^XTMP(MSGCTRL,"SDAMA301",AJJ3VIS)) QUIT:AJJ3VIS=""  DO
        . . . . . SET SDARRAY2=^XTMP(MSGCTRL,"SDAMA301",AJJ3VIS)
        . . . . . SET SSNPATN=$P(SDARRAY2,"^",2)
        . . . . . SET SDLOCATE=$P(SDARRAY2,"^",1)
        . . . . . SET SDDATE=$P(SDARRAY2,"^",3)
        . . . . . SET FIRST=SDLOCATE_"^"_SSNPATN_"^"_SDDATE_"^"
        . . . . . SET SDARRAY2=$P(SDARRAY2,FIRST,2)
        . . . . . SET PATIENTN=$P(SSNPATN,"#",2)
        . . . . . SET SSN=$P(SSNPATN,"#",1)
        . . . . . SET COUNTER=(AJJ3VIS-1)*2
        . . . . . SET RESULTS(COUNTER+1)="APPT/UNSCHED VISIT, HOSP LOCATION="_SDLOCATE_" PATIENT="_PATIENTN_" SSN="_SSN_" DATE/TIME="_SDDATE
        . . . . . SET RESULTS(COUNTER+2)="                   DATA FIELDS="_SDARRAY2
        . . ELSE  DO
        . . .SET RESULTS(1)="SDCOUNT="_SDCOUNT  ;LOOK AT ERRORS IN API CALL, ETC
        ELSE  DO
        . SET RESULTS(1)=RETURN ;" APP ACK CODE="AE". SOME OTHER ERRORS IN TRANSMISSION IN OTHER PIECES OF RETURN
        SET RESULTS(0)="MSGCTRL="_MSGCTRL
        IF (MSGCTRL'=0) KILL ^XTMP(MSGCTRL)
        ;
        QUIT
        ;
MKPI(RESULTS,SDARRAY1)  ;
        ;"Purpose: MAKE Appointment APP TEST PROGRAM
        ;"Input: RESULTS  -- PASS BY REFERENCE.  An OUT PARAMETER.
        ;"       SDARRAY1 -- PASS BY REFERENCE.  Example input:
        ;"              SDARRAY1("PATIENTN")="ZZ PATIENT,TEST ONE"
        ;"              SDARRAY1("SSN")="100001298"
        ;"              SDARRAY1("SD1")="JAN 24,2007@11:30" vs. "3070123.1130"  ??Which one??
        ;"              SDARRAY1("SC")="VWVOE RADIOLOGY CLINIC" vs "3" ??Which One??
        ;"              SDARRAY1("DUZ")="SCHLEHUBER,CAMERON"
        ;
        NEW MSGCTRL,IER,RETURN,DUZ1
        NEW PATIENTN,SSN,SD1,SC,STYP,OUTIN,SDVWNVAI,X,Y,X2,ACKCODE,SDARRAY
        NEW AJJ3CNT
        NEW VXSDNVAI
        SET MSGCTRL=0  ;
        ;"NEW DFN(SSN AND PATIENT NAME INSTEAD),SD1,SC(HOSP LOCATION (CLINIC) EXT FORMAT NAME INSTEAD,STYP,
        ;"NEW SDARRAY (DATE/TIMES IN EXTERNAL FORMAT),IER
        IF $GET(SDARRAY1("PATIENTN"))="" SET RESULTS(0)="NO DEFINED PATIENTN ELEMENT" QUIT
        SET PATIENTN=SDARRAY1("PATIENTN") ;" e.g. ZZ PATIENT,TEST ONE
        SET SDVWNVAI="D"  ;" NON-VA TESTING HERE WITH DISABLING THE NEED FOR ICN
        IF $GET(SDARRAY1("SSN"))="" SET RESULTS(0)="NO DEFINED SSN ELEMENT" QUIT
        SET SSN=SDARRAY1("SSN")  ;" SET SSN=100001298 ; DFN=1 NON TEST PATRIENT FOR PFSS EVENT GENERATION
        IF $GET(SDARRAY1("SD1"))="" SET RESULTS(0)="NO DEFINED SD1 APPT DATE ELEMENT"
        SET SD1=SDARRAY1("SD1")  ;" e.g. "JAN 24,2007@11:30" ; SD1=3070123.1130
        ;SET X=SD1 DO ^%DT SET SD1=Y
        IF $GET(SDARRAY1("SC"))="" SET RESULTS(0)="NO DEFINED APPOINTMENT CLINIC ELEMENT" QUIT
        SET SC=SDARRAY1("SC")   ; "VWVOE RADIOLOGY CLINIC ; SET SC=3
        SET STYP=3  ;SCHEDULED APPT
        SET OUTIN="O" ;for outpatient clinic
        ;
        DO NOW^%DTC SET X2=X\1 SET Y=X2 DO DD^%DT SET SDARRAY("DATE NOW")=Y
        SET SDARRAY("APPT TYPE")=9
        SET SDARRAY("SCHED_REQ_TYPE")="O" ;"OTHER THAN NEXT AVAIABLE
        SET SDARRAY("NEXT APPT IND")=0    ;"0 FOR NO
        SET SDARRAY("FOLLOWUP VISIT INDICATOR")=0  ;" 0 FOR NO
        ;"CHECK FOR DUZ IN SDARRAY1("DUZ") FOR DATA ENTRY CLERK
        IF $GET(SDARRAY1("DUZ"))="" SET RESULTS(0)="NO DUZ ELEMENT RETURNED" QUIT
        SET DUZ1=SDARRAY1("DUZ")
        ;"GET NAME FOR DUZ IN NEW PERSON FILE, e.g. "SCHLEHUBER,CAMERON" ; PERSON ON MACHINE MAKING APPT REMOTELY
        SET SDARRAY("DATA ENTRY CLERK")=$P($GET(^VA(200,DUZ1,0)),"^",1)
        ;"THEN PARAMETERS CONVERTED TO INTERNAL VALUE
        ;
        SET IER=$$TRNSMKPI^SDVWHLE1(PATIENTN,SSN,SD1,SC,STYP,.SDARRAY,OUTIN,.MSGCTRL,SDVWNVAI)
        ;"SDVWNVAI AS LAST PARAMETER PASSED
        IF (IER="OK")&(MSGCTRL'=0) DO
        . SET AJJ3CNT=0
CHKGAIN . IF AJJ3CNT>8 QUIT
        . IF $D(^XTMP(MSGCTRL,"RETURN"))=0 H 3 SET AJJ3CNT=AJJ3CNT+1 G CHKGAIN
        . SET RETURN=^XTMP(MSGCTRL,"RETURN") ;" THIS INCLUDES ACK CODE AS AA OR AE
        . SET ACKCODE=$P(RETURN,"^",1)
        . IF ACKCODE="AA" DO
        . . SET RESULTS(1)=ACKCODE_" MAKE APPT GOOD RETURN"
        . ELSE  DO
        . . ;"ACKCODE="AE". LOOK AT SOME OTHER ERRORS IN TRNSMISSION IN OTHER PIECES OF RETURN
        . . SET RESULTS(1)=ACKCODE_" RETURN="_RETURN
        SET RESULTS(0)="MSGCTRL="_MSGCTRL
        IF (MSGCTRL'=0) K ^XTMP(MSGCTRL,"RETURN")
MKDONE  ;
        QUIT
