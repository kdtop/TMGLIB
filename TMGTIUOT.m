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
NOTADDRE()
        QUIT "=== HPI ISSUES BELOW WERE NOT ADDRESSED TODAY ==="
        ;"
WARNTEXT()
        QUIT "NEEDS DATA ENTERED"
        ;"
FLAG(OPTION)
        QUIT $$HLIGHT(,.OPTION)_" NEEDS UPDATE "_$$EHLIGHT(.OPTION)
        ;"
CSDBDATE(OPTION) ;"
        ;"Purpose: This is to test the value of *CSM-Database Review.
        ;"         If the date is older than 5 months, a warning will
        ;"         be added. Set from "VALUE MODIFICATION CODE" subfield in
        ;"         the ITEM PRINT NAME field of TMG TIU PXRM TABLE
        ;"NOTE: I wanted to use the reminder to determine if this needed
        ;"      updating, so systems would be always in sync.... however I quickly
        ;"      realized that this wasn't possible because the reminder
        ;"      called the medication table, and recursively came back into this
        ;"      function until the buffer crashed.        
        ;"Input: TMGX should be set prior to calling
        ;"Output: TMGY will contain value.
        NEW FLAG SET FLAG=$$FLAGMSG($$AGE(TMGX)>150," NEEDS CSDB REVIEW ",.OPTION)
        ;" NEW REMRESULT SET REMRESULT=$$DOREM^TMGPXR03(THISDFN,265,5,$$TODAY^TMGDATE)
        ;" IF REMRESULT["DUE NOW" SET FLAG=$$HLIGHT(,.OPTION)_" NEEDS CSDB REVIEW "_$$EHL IGHT(.OPTION)
        SET TMGY=TMGX_" "_FLAG
        QUIT
        ;
CONTRACT(OPTION)  ;"
        ;"Purpose: This is to test the value of *CSM Contract.
        ;"         If the date is older than 1 YEAR, a warning will
        ;"         be added. Set from "VALUE MODIFICATION CODE" subfield in
        ;"         the ITEM PRINT NAME field of TMG TIU PXRM TABLE
        ;"Input: TMGX should be set prior to calling
        ;"Output: TMGY will contain value.
        NEW FLAG SET FLAG=$$FLAGMSG($$AGE(TMGX)>364," NEEDS NEW CONTRACT ",.OPTION)
        SET TMGY=TMGX_" "_FLAG
        QUIT
        ;"
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
FLAGMSG(TEST,MSG,OPTION) ;
        IF TEST QUIT $$HLIGHT(,.OPTION)_MSG_$$EHLIGHT(.OPTION)
        QUIT ""
        ;"
HLIGHT(COLOR,OPTION)
        ;"INPUT: Color -- an HTML format color string
        ;"       OPTION -- optional.  
        ;"         OPTION("DIRECT HTML INSERTION")=1 <-- don't use {HTML:  tags.  
        SET COLOR=$G(COLOR)
        IF COLOR="" SET COLOR="#ff0000"
        ;"//kt QUIT "{HTML:<B><FONT style=""BACKGROUND-COLOR:"_COLOR_""">}"
        NEW RESULT SET RESULT="<B><FONT style=""BACKGROUND-COLOR:"_COLOR_""">"
        IF $GET(OPTION("DIRECT HTML INSERTION"))'=1 DO
        . SET RESULT="{HTML:"_RESULT_"}"
        QUIT RESULT
        ;"
EHLIGHT(OPTION)
        NEW RESULT SET RESULT="</B></FONT>"
        IF $GET(OPTION("DIRECT HTML INSERTION"))'=1 DO
        . SET RESULT="{HTML:"_RESULT_"}"
        QUIT RESULT
        ;"
CHKMED(MED,AGE,OPTION)  ;"
        ;"INPUT: MED -- a medication line string
        ;"       AGE  -- patient age (numeric)
        ;"       OPTION -- Optional.
        ;"         OPTION("DIRECT HTML INSERTION")=1 <-- don't use {HTML:  tags.  
        ;"Purpose: This function checks each medication 
        SET AGE=+$G(AGE)
        SET MED=$$HTML2TXS^TMGHTM1(MED)
        IF AGE>64 SET MED=$$ISMEDHR(MED,.OPTION)
        SET MED=$$ISHALF(MED,.OPTION)
        SET MED=$$RPLCMEDS(MED)
        SET MED=$$AUTOMED(MED,.OPTION)
        SET MED=$$DCMED(MED,.OPTION)
CHDN
        QUIT MED
        ;"
ISMEDHR(MEDNAME,OPTION)  ;"Is this med listed as High Risk
        NEW TABLEIEN SET TABLEIEN=+$O(^TMG(22708,"B","HIGH RISK MEDS",0))
        IF TABLEIEN'>0 QUIT MEDNAME
        NEW ONEMED SET ONEMED=""
        NEW FOUND SET FOUND=0
        FOR  SET ONEMED=$O(^TMG(22708,TABLEIEN,3,"B",ONEMED)) QUIT:(ONEMED="")!(FOUND=1)  DO
        . IF $$UP^XLFSTR(MEDNAME)[$$UP^XLFSTR(ONEMED) DO
        . . SET MEDNAME=$$WRAPTEXT(MEDNAME,$$HRCOLOR,.OPTION)
        . . SET FOUND=1
        QUIT MEDNAME
        ;"
ISHALF(MEDNAME,OPTION)  ;"Is this med dosage for half
        NEW EXCLUDE SET EXCLUDE=$$EXCLHALF(MEDNAME)
        IF EXCLUDE=1 GOTO HALFDN
        IF (MEDNAME[" 1/2 ")!(MEDNAME["HALF") SET MEDNAME=$$WRAPTEXT(MEDNAME,$$HALFCOLOR,.OPTION)
HALFDN
        QUIT MEDNAME
        ;"
AUTOMED(MEDNAME,OPTION)  ;"Was this med autoadded?
        ;"Old method -> IF MEDNAME[$$ERXSUFFX^TMGTIUO5() SET MEDNAME=$$WRAPSECT(MEDNAME,$$AUTOCOLOR,$$ERXSUFFX^TMGTIUO5())
        IF MEDNAME[$$AUTOADDB() DO
        . NEW TOTALTAG
        . SET TOTALTAG=$$AUTOADDB_$P($P(MEDNAME,$$AUTOADDB(),2),$$AUTOADDE,1)_$$AUTOADDE
        . SET MEDNAME=$$WRAPSECT(MEDNAME,$$AUTOCOLOR,TOTALTAG,.OPTION)
        QUIT MEDNAME
        ;"
DCMED(MEDNAME,OPTION)  ;"Was this med a discharge reconciliation med
        IF MEDNAME[$$DCMEDTAG() SET MEDNAME=$$WRAPSECT(MEDNAME,$$DCCOLOR,$$DCMEDTAG(),.OPTION)
        QUIT MEDNAME
        ;"
WRAPTEXT(MEDNAME,COLOR,OPTION)  ;"Highlight the med name with provided color
        ;"INPUT: MEDNAME -- text
        ;"       COLOR -- color
        ;"       OPTION -- Optional.
        ;"         OPTION("DIRECT HTML INSERTION")=1 <-- don't use {HTML:  tags.  
        QUIT $$HLIGHT(COLOR,.OPTION)_MEDNAME_$$EHLIGHT(.OPTION)
        ;"
WRAPSECT(MEDNAME,COLOR,SECTION,OPTION)  ;"Highlight a specific section
        QUIT $P(MEDNAME,SECTION,1)_$$HLIGHT(COLOR,.OPTION)_SECTION_$$EHLIGHT(.OPTION)_$P(MEDNAME,SECTION,2,999)
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
WARN(STR,TABLEIEN,SUBIEN)  ;"
        NEW WARNRESULT SET WARNRESULT=STR
        NEW TITLE,LINEDATA
        SET TITLE=$PIECE($GET(^TMG(22708,TABLEIEN,1,SUBIEN,0)),"^",1)
        SET LINEDATA=$$TRIM^XLFSTR($P(STR,TITLE,2))
        IF LINEDATA[":" DO
        . SET LINEDATA=$$TRIM^XLFSTR($P(LINEDATA,":",2))
        IF LINEDATA["=" DO
        . SET LINEDATA=$$TRIM^XLFSTR($P(LINEDATA,"=",2))
        IF LINEDATA="" SET WARNRESULT=STR_"NEEDS DATA"
        QUIT WARNRESULT
        ;"