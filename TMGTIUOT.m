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
HRCOLOR() 
        QUIT "#ffff99"
        ;"
HALFCOLOR() 
        QUIT "#ffd5b2"
        ;"
AUTOCOLOR()
        QUIT "#ffa55b" 
        ;"
DCCOLOR()
        QUIT "#d2ffb7"
        ;"
DCMEDTAG()
        QUIT "[HOSP D/C Rx]"
        ;"
AUTOADDB()
        QUIT "[Auto added "
AUTOADDE()
        QUIT "]"
        ;"
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
HLIGHT(COLOR)
        SET COLOR=$G(COLOR)
        IF COLOR="" SET COLOR="#ff0000"
        QUIT "{HTML:<B><FONT style=""BACKGROUND-COLOR:"_COLOR_""">}"
        ;"
EHLIGHT()
        QUIT "{HTML:</B></FONT>}"
        ;"
CHKMED(MED,AGE)  ;"
        ;"Purpose: This function checks each medication 
        SET AGE=+$G(AGE)
        SET MED=$$HTML2TXS^TMGHTM1(MED)
        IF AGE>64 SET MED=$$ISMEDHR(MED)
        SET MED=$$ISHALF(MED)
        SET MED=$$RPLCMEDS(MED)
        SET MED=$$AUTOMED(MED)
        SET MED=$$DCMED(MED)
CHDN
        QUIT MED
        ;"
ISMEDHR(MEDNAME)  ;"Is this med listed as High Risk
        NEW TABLEIEN SET TABLEIEN=+$O(^TMG(22708,"B","HIGH RISK MEDS",0))
        IF TABLEIEN'>0 QUIT MEDNAME
        NEW ONEMED SET ONEMED=""
        NEW FOUND SET FOUND=0
        FOR  SET ONEMED=$O(^TMG(22708,TABLEIEN,3,"B",ONEMED)) QUIT:(ONEMED="")!(FOUND=1)  DO
        . IF $$UP^XLFSTR(MEDNAME)[$$UP^XLFSTR(ONEMED) DO
        . . SET MEDNAME=$$WRAPTEXT(MEDNAME,$$HRCOLOR)
        . . SET FOUND=1
        QUIT MEDNAME
        ;"
ISHALF(MEDNAME)  ;"Is this med dosage for half
        NEW EXCLUDE SET EXCLUDE=$$EXCLHALF(MEDNAME)
        IF EXCLUDE=1 GOTO HALFDN
        IF (MEDNAME[" 1/2 ")!(MEDNAME["HALF") SET MEDNAME=$$WRAPTEXT(MEDNAME,$$HALFCOLOR)
HALFDN
        QUIT MEDNAME
        ;"
AUTOMED(MEDNAME)  ;"Was this med autoadded
        ;"Old method -> IF MEDNAME[$$ERXSUFFX^TMGTIUO5() SET MEDNAME=$$WRAPSECT(MEDNAME,$$AUTOCOLOR,$$ERXSUFFX^TMGTIUO5())
        IF MEDNAME[$$AUTOADDB() DO
        . NEW TOTALTAG
        . SET TOTALTAG=$$AUTOADDB_$P($P(MEDNAME,$$AUTOADDB(),2),$$AUTOADDE,1)_$$AUTOADDE
        . SET MEDNAME=$$WRAPSECT(MEDNAME,$$AUTOCOLOR,TOTALTAG)
        QUIT MEDNAME
        ;"
DCMED(MEDNAME)  ;"Was this med a discharge reconciliation med
        IF MEDNAME[$$DCMEDTAG() SET MEDNAME=$$WRAPSECT(MEDNAME,$$DCCOLOR,$$DCMEDTAG())
        QUIT MEDNAME
        ;"
WRAPTEXT(MEDNAME,COLOR)  ;"Highlight the med name with provided color
        QUIT $$HLIGHT(COLOR)_MEDNAME_$$EHLIGHT
        ;"
WRAPSECT(MEDNAME,COLOR,SECTION)  ;"Highlight a specific section
        QUIT $P(MEDNAME,SECTION,1)_$$HLIGHT(COLOR)_SECTION_$$EHLIGHT_$P(MEDNAME,SECTION,2,999)
        ;"
EXCLHALF(MEDNAME)  ;"EXCLUDE MEDICATION FROM CHECKING FOR HALF DOSE
        NEW EXCLUDE SET EXCLUDE=0
        NEW TABLEIEN SET TABLEIEN=+$O(^TMG(22708,"B","HALF DOSE EXCLUDE MEDS",0))
        IF TABLEIEN'>0 QUIT EXCLUDE
        NEW ONEMED SET ONEMED=""
        FOR  SET ONEMED=$O(^TMG(22708,TABLEIEN,3,"B",ONEMED)) QUIT:(ONEMED="")!(EXCLUDE=1)  DO
        . IF $$UP^XLFSTR(MEDNAME)[$$UP^XLFSTR(ONEMED) SET EXCLUDE=1
        QUIT EXCLUDE
        ;"
RPLCMEDS(MEDLIST)  ;"
        SET MEDLIST=$$REPLSTR^TMGSTUT3(MEDLIST,"ASA ","Aspirin ")
        QUIT MEDLIST
        ;"
