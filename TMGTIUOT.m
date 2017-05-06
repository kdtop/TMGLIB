TMGTIUOT ;TMG/kst-Text objects for use in CPRS ; 7/2/14
         ;;1.0;TMG-LIB;**1,17**;7/2/14
 ;
 ;"Kevin Toppenberg MD
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
CSDBDATE ;"
        ;"Purpose: This is to test the value of *CSM Contract.
        ;"         If the date is older than 7 months, a warning will
        ;"         be added. Set from "VALUE MODIFICATION CODE" subfield in
        ;"         the ITEM PRINT NAME field of TMG TIU PXRM TABLE
        ;"Input: TMGX should be set prior to calling
        ;"Output: TMGY will contain value.
        NEW FLAG SET FLAG=$$FLAGMSG($$AGE(TMGX)>210," NEEDS UPDATE ")
        SET TMGY=TMGX_" "_FLAG
        QUIT
        ;
AGE(ADT) 
        ;"Returns dates of NOW - ADT (IN DAYS)
        NEW %DT,X,Y,RESULT,NOW
        SET X=ADT
        DO ^%DT
        IF Y=-1 SET RESULT=-1 GOTO AGEDN
        SET NOW=$$NOW^XLFDT
        SET RESULT=$$FMDIFF^XLFDT(NOW,Y,1)
AGEDN   QUIT RESULT
        ;
FLAGMSG(TEST,MSG) ;
        IF TEST QUIT $$HLIGHT_MSG_$$EHLIGHT
        QUIT ""
        ;"
HLIGHT()
        QUIT "{HTML:<B><FONT style=""BACKGROUND-COLOR:#ff0000"">}"
        ;"
EHLIGHT()
        QUIT "{HTML:</B></FONT>}"
        ;"
