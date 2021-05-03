TMGFIX4 ;TMG/kst/Fixes for converting old labs ; 9/16/13, 2/2/14, 3/24/21
         ;;1.0;TMG-LIB;**1**;9/16/13
 ;
 ;"FIXES related to TMG TIU PXRM TABLES
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
 ;" API -- Public Functions.
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"   TMGSTUT2, TMGSTUT3
 ;"=======================================================================
 ;"
 ;"=======================================================================
 ;
SCANALL(LABEL) ;"SCAN ALL PATIENTS (AND ALL NOTES FOR EACH PATIENT
        NEW STARTTIME SET STARTTIME=$H
        NEW TMGDFN SET TMGDFN=1
        NEW MAXDFN SET MAXDFN=$ORDER(^DPT("@"),-1)
        FOR  SET TMGDFN=$ORDER(^DPT(TMGDFN)) QUIT:+TMGDFN'>0  DO
        . NEW PTNAME SET PTNAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
        . IF $LENGTH(PTNAME)>20 SET PTNAME=$EXTRACT(PTNAME,1,20)
        . ELSE  SET PTNAME=$$LJ^XLFSTR(PTNAME,20," ")
        . DO PROGBAR^TMGUSRI2(TMGDFN,PTNAME_"("_TMGDFN_")",0,MAXDFN,70,STARTTIME)
        . NEW RESULT SET RESULT=$$SCANPT(TMGDFN,LABEL,.ARRAY)
        . IF +RESULT'>0 DO  QUIT
        . . WRITE "PATIENT=",PTNAME,!
        . . WRITE "ERROR: ",$PIECE(RESULT,"^",2),!
        . IF $DATA(ARRAY)=0 QUIT
        . SET RESULT=$$ARR2LABS(TMGDFN,.ARRAY) ;"SAVE LABS.INTO FILE 63.04 IN 63
        . IF +RESULT'>0 DO  QUIT
        . . WRITE "PATIENT=",$PIECE($GET(^DPT(TMGDFN,0)),"^",1),!
        . . WRITE "ERROR: ",$PIECE(RESULT,"^",2),!
        QUIT
        ;
ASKSCAN(LABEL) ;"JUST SCAN IN 1 PROGRESS NOTE, THAT USER SELECTS.
        NEW STARTTIME SET STARTTIME=$H
        NEW DIC,DIC,PTNAME,IEN8925,RESULT 
ASK1    SET DIC=8925,DIC(0)="MAEQ"
        DO ^DIC WRITE !
        SET IEN8925=+Y IF IEN8925'>0 GOTO ASKNDN
        SET TMGDFN=+$PIECE($GET(^TIU(8925,IEN8925,0)),"^",2)
        SET PTNAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
        IF $LENGTH(PTNAME)>20 SET PTNAME=$EXTRACT(PTNAME,1,20)
        ELSE  SET PTNAME=$$LJ^XLFSTR(PTNAME,20," ")
        SET RESULT=$$SCNPTDOC(TMGDFN,IEN8925,.LABEL,.ARRAY)
        IF +RESULT'>0 DO  GOTO ASKNDN
        . WRITE "PATIENT=",PTNAME,!
        . WRITE "ERROR: ",$PIECE(RESULT,"^",2),!
        IF $DATA(ARRAY)=0 GOTO ASKNDN
        SET RESULT=$$ARR2LABS(TMGDFN,.ARRAY) ;"SAVE LABS.INTO FILE 63.04 IN 63
        IF +RESULT'>0 DO  GOTO ASKNDN
        . WRITE "PATIENT=",$PIECE($GET(^DPT(TMGDFN,0)),"^",1),!
        . WRITE "ERROR: ",$PIECE(RESULT,"^",2),!
        GOTO ASK1
ASKNDN  QUIT
        ;
SCANPT(TMGDFN,LABEL,OUT) ;
        ;"Purpose: To scan all docucuments for a patient to retrieve all entries of a table.
        ;"Input: TMGDFN -- IEN 2 (patient)
        ;"       LABEL -- the name of the table.  e.g. 'LIPIDS', or '[LIPIDS]'
        ;"       OUT -- PASS BY REFERENCE.  Filled with table(s).  Format
        ;"          Output is determined by PROCESS1() and MERGELIPD()
        ;"Result: 1^OK, or -1^Message
        NEW TMGRESULT SET TMGRESULT="1^OK"
        NEW IEN8925 SET IEN8925=0
        FOR  SET IEN8925=$ORDER(^TIU(8925,"C",TMGDFN,IEN8925)) QUIT:(IEN8925'>0)!(+TMGRESULT'>0)  DO
        . SET TMGRESULT=$$SCNPTDOC(.TMGDFN,IEN8925,.LABEL,.OUT) ;
        DO MERGLBS(.OUT)
SCPTDN  QUIT TMGRESULT
        ;
SCNPTDOC(TMGDFN,IEN8925,LABEL,OUT) ;
        ;"Purpose: To scan specified docucument and patient to retrieve all entries of a table.
        ;"Input: TMGDFN -- IEN 2 (patient)
        ;"       IEN8925 -- the document to scan
        ;"       LABEL -- the name of the table.  e.g. 'LIPIDS', or '[LIPIDS]'
        ;"       OUT -- PASS BY REFERENCE.  Filled with table(s).  Format
        ;"          Output is determined by PROCESS1() and MERGELIPD()
        ;"Result: 1^OK, or -1^Message
        NEW TMGRESULT SET TMGRESULT="1^OK"
        SET IEN8925=+$GET(IEN8925)
        SET TMGDFN=+$GET(TMGDFN) IF TMGDFN'>0 DO  GOTO SCPTDDN
        . SET TMGRESULT="-1^No patient IEN provided"
        SET LABEL=$GET(LABEL) IF LABEL="" DO  GOTO SCPTDDN
        . SET TMGRESULT="-1^No table label provided"
        SET LABEL=$$TRIM^XLFSTR(LABEL)
        IF $EXTRACT(LABEL,1)'="[" SET LABEL="["_LABEL
        IF $EXTRACT(LABEL,$LENGTH(LABEL))'="]" SET LABEL=LABEL_"]"
        NEW TEMPARR,ONEARR
        IF $$XTRCTSPC^TMGTIUO5(IEN8925,LABEL,"BLANK_LINE",.TEMPARR)=0 DO  GOTO SCPTDDN  ;"extract from 1 note
        . SET TMGRESULT="-1^Problem extracting table from specified note #"_IEN8925
        DO MERGEIN^TMGTIUO5(.TEMPARR,.ONEARR)
        DO TRIMEMPT(.ONEARR) ;"Remove empty entries from array.
        IF $DATA(ONEARR)=0 QUIT
        IF LABEL="[LIPIDS]" DO
        . DO PROCSLIP(.ONEARR)
        IF LABEL="[THYROID]" DO
        . DO PROCSTHY(.ONEARR)
        IF LABEL="[DIABETIC STUDIES]" DO
        . DO PROCSDM2(.ONEARR)
        ELSE  DO  GOTO SCPTDDN
        . SET TMGRESULT="-1^Label '"_LABEL_"' not supported."
        IF $DATA(ONEARR)=0 GOTO SCPTDDN
        MERGE OUT(IEN8925)=ONEARR
        DO MERGLBS(.OUT)
        ;
SCPTDDN QUIT TMGRESULT
        ;
TRIMEMPT(ARR) ;"Remove empty entries from array.
        NEW KEY SET KEY=""
        FOR  SET KEY=$ORDER(ARR("KEY-VALUE",KEY)) QUIT:KEY=""  DO
        . NEW VALUE SET VALUE=$$UP^XLFSTR($GET(ARR("KEY-VALUE",KEY)))
        . IF (VALUE="")!(VALUE="<NO DATA>") DO
        . . KILL ARR("KEY-VALUE",KEY)
        IF $DATA(ARR("KEY-VALUE"))<10 KILL ARR
        QUIT
 ;"==================================================================
 ;"==================================================================
        ;
PROCSLIP(ARRAY) ;"PROCESS ARRAY FOR LIPIDS
        ;"INPUT: ARRAY -- an IN and OUT parameter.  PASS BY REFERENCE.
        NEW OUTARR
        NEW DATEFOUND SET DATEFOUND=0
        NEW KEY SET KEY=""
        FOR  SET KEY=$ORDER(ARRAY("KEY-VALUE",KEY)) QUIT:KEY=""  DO
        . KILL ARRAY("KEY-VALUE",KEY,"LINE")
        . NEW VALUE SET VALUE=$GET(ARRAY("KEY-VALUE",KEY)) QUIT:VALUE=""
        . NEW SHOULDKILL SET SHOULDKILL=1
        . NEW NEWKEY SET NEWKEY=""
        . IF KEY="DATE OF LAST LIPID PANEL" SET NEWKEY="DATE"
        . IF KEY="HDL CHOLESTEROL" SET NEWKEY="244^HDL"
        . IF KEY="LDL CHOLESTEROL" SET NEWKEY="901^LDL CHOLESTEROL"
        . IF KEY="TOTAL CHOLESTEROL" SET NEWKEY="183^CHOLESTEROL"
        . IF KEY="TRIGLYCERIDES" SET NEWKEY="205^TRIGLYCERIDE"
        . KILL ARRAY("KEY-VALUE",KEY)
        . IF NEWKEY="" QUIT
        . NEW PIVOT SET PIVOT=""
        . IF VALUE["<--" SET PIVOT="<--"
        . IF VALUE[";" SET PIVOT=";"
        . IF PIVOT'="" DO
        . . SET VALUE=$$TRIM^XLFSTR($PIECE(VALUE,PIVOT,1))
        . IF NEWKEY="DATE" DO  QUIT:VALUE=-1
        . . NEW %DT,X,Y SET %DT="P",X=VALUE DO ^%DT
        . . SET VALUE=Y
        . . IF VALUE>0 SET DATEFOUND=1
        . SET OUTARR(NEWKEY)=+VALUE
        KILL ARRAY
        IF DATEFOUND MERGE ARRAY=OUTARR
        QUIT
        ;
PROCSTHY(ARRAY) ;"PROCESS ARRAY FOR THYROID
        ;"INPUT: ARRAY -- an IN and OUT parameter.  PASS BY REFERENCE.
        NEW OUTARR
        NEW TEMPSAV MERGE TEMPSAV=ARRAY
        NEW DATEFOUND SET DATEFOUND=0
        NEW KEY SET KEY=""
        FOR  SET KEY=$ORDER(ARRAY("KEY-VALUE",KEY)) QUIT:KEY=""  DO
        . KILL ARRAY("KEY-VALUE",KEY,"LINE")
        . NEW VALUE SET VALUE=$GET(ARRAY("KEY-VALUE",KEY)) QUIT:VALUE=""
        . NEW SHOULDKILL SET SHOULDKILL=1
        . NEW NEWKEY SET NEWKEY=""
        . IF KEY="DATE OF LAST STUDY" DO  QUIT:NEWKEY=""
        . . SET NEWKEY="DATE"
        . . IF $DATA(OUTARR(NEWKEY)) SET NEWKEY="" ;"In case already SET by VALUE holding a date.
        . IF KEY="TSH" SET NEWKEY="110^TSH"
        . KILL ARRAY("KEY-VALUE",KEY)
        . IF NEWKEY="" QUIT
        . NEW PIVOT SET PIVOT=""
        . IF VALUE["<--" SET VALUE=$$TRIM^XLFSTR($PIECE(VALUE,"<--",1))
        . IF VALUE[";" SET VALUE=$$TRIM^XLFSTR($PIECE(VALUE,";",1))
        . IF $LENGTH(VALUE," ")>1 DO
        . . DO SPLIT2AR^TMGSTUT2(VALUE," ",.VALUE) SET VALUE=""
        . . NEW DONE SET DONE=0
        . . NEW ADATE SET ADATE=""
        . . NEW IDX SET IDX=0
        . . FOR  SET IDX=$ORDER(VALUE(IDX)) QUIT:(+IDX'>0)!(DONE)  DO
        . . . NEW AVAL SET AVAL=$$UP^XLFSTR($GET(VALUE(IDX))) QUIT:AVAL=""
        . . . IF AVAL="ON" KILL VALUE(IDX) QUIT
        . . . IF (AVAL["/"),(ADATE="") DO  QUIT
        . . . . NEW %DT,X,Y SET %DT="P",X=AVAL DO ^%DT QUIT:Y'>0
        . . . . SET ADATE=Y
        . . . IF (NEWKEY'="DATE"),($$ISNUM^TMGSTUT3(AVAL)),(VALUE="") SET VALUE=AVAL
        . . IF ADATE'="" DO
        . . . SET OUTARR("DATE")=ADATE
        . . . SET DATEFOUND=1
        . . . IF NEWKEY="DATE" SET NEWKEY=""
        . IF NEWKEY="DATE" DO  QUIT:VALUE=-1
        . . SET VALUE=$PIECE(VALUE," ",1)
        . . NEW %DT,X,Y SET %DT="P",X=VALUE DO ^%DT
        . . SET VALUE=Y
        . . IF VALUE>0 SET DATEFOUND=1
        . IF NEWKEY="" QUIT
        . IF +VALUE'>0 QUIT
        . SET OUTARR(NEWKEY)=+VALUE
        KILL ARRAY
        IF DATEFOUND MERGE ARRAY=OUTARR
        IF $DATA(ARRAY)=0 DO
        . WRITE !,"Couldn't extract TSH information from the following array.   Why??",!
        . WRITE "DFN=",$GET(TMGDFN),!
        . WRITE "IEN8925=",$GET(IEN8925),!,!
        . IF $DATA(TEMPSAV) DO ZWRITE^TMGZWR("TEMPSAV")
        . DO PRESS2GO^TMGUSRI2
        . WRITE !
        QUIT
        ;
PROCSDM2(ARRAY) ;"PROCESS ARRAY FOR DIABETIC STUDIES
        ;"INPUT: ARRAY -- an IN and OUT parameter.  PASS BY REFERENCE.
        NEW OUTARR
        NEW TEMPSAV MERGE TEMPSAV=ARRAY
        NEW DATEFOUND SET DATEFOUND=0
        NEW KEY SET KEY=""
        FOR  SET KEY=$ORDER(ARRAY("KEY-VALUE",KEY)) QUIT:KEY=""  DO
        . KILL ARRAY("KEY-VALUE",KEY,"LINE")
        . NEW VALUE SET VALUE=$GET(ARRAY("KEY-VALUE",KEY)) QUIT:VALUE=""
        . NEW SHOULDKILL SET SHOULDKILL=1
        . NEW NEWKEY SET NEWKEY=""
        . IF KEY="HGBA1C" SET NEWKEY="97^HEMOGLOBIN A1C"
        . KILL ARRAY("KEY-VALUE",KEY)
        . IF NEWKEY="" QUIT
        . NEW PIVOT SET PIVOT=""
        . IF VALUE["<--" SET VALUE=$$TRIM^XLFSTR($PIECE(VALUE,"<--",1))
        . IF VALUE[";" SET VALUE=$$TRIM^XLFSTR($PIECE(VALUE,";",1))
        . IF $LENGTH(VALUE," ")>1 DO
        . . DO SPLIT2AR^TMGSTUT2(VALUE," ",.VALUE) SET VALUE=""
        . . NEW DONE SET DONE=0
        . . NEW ADATE SET ADATE=""
        . . NEW IDX SET IDX=0
        . . FOR  SET IDX=$ORDER(VALUE(IDX)) QUIT:(+IDX'>0)!(DONE)  DO
        . . . NEW AVAL SET AVAL=$$UP^XLFSTR($GET(VALUE(IDX))) QUIT:AVAL=""
        . . . IF AVAL="ON" KILL VALUE(IDX) QUIT
        . . . IF (AVAL["/"),(ADATE="") DO  QUIT
        . . . . NEW %DT,X,Y SET %DT="P",X=AVAL DO ^%DT QUIT:Y'>0
        . . . . SET ADATE=Y
        . . . IF (NEWKEY'="DATE"),($$ISNUM^TMGSTUT3(AVAL)),(VALUE="") SET VALUE=AVAL
        . . IF ADATE'="" DO
        . . . SET OUTARR("DATE")=ADATE
        . . . SET DATEFOUND=1
        . . . IF NEWKEY="DATE" SET NEWKEY=""
        . IF NEWKEY="DATE" DO  QUIT:VALUE=-1
        . . SET VALUE=$PIECE(VALUE," ",1)
        . . NEW %DT,X,Y SET %DT="P",X=VALUE DO ^%DT
        . . SET VALUE=Y
        . . IF VALUE>0 SET DATEFOUND=1
        . IF NEWKEY="" QUIT
        . IF +VALUE'>0 QUIT
        . SET OUTARR(NEWKEY)=+VALUE
        KILL ARRAY
        IF DATEFOUND MERGE ARRAY=OUTARR
        IF $DATA(ARRAY)=0 DO
        . WRITE !,"Couldn't extract HgbA1c information from the following array.   Why??",!
        . WRITE "DFN=",$GET(TMGDFN),!
        . WRITE "IEN8925=",$GET(IEN8925),!,!
        . IF $DATA(TEMPSAV) DO ZWRITE^TMGZWR("TEMPSAV")
        . DO PRESS2GO^TMGUSRI2
        . WRITE !
        QUIT
        ;
MERGLBS(ARRAY) ;
        NEW OUTARR
        NEW IEN8925 SET IEN8925=0
        FOR  SET IEN8925=$ORDER(ARRAY(IEN8925)) QUIT:IEN8925'>0  DO
        . NEW DT SET DT=+$GET(ARRAY(IEN8925,"DATE")) QUIT:DT'>0
        . IF $DATA(OUTARR(DT)) QUIT
        . KILL ARRAY(IEN8925,"DATE")
        . MERGE OUTARR(DT)=ARRAY(IEN8925)
        . SET OUTARR(DT,"<IEN8925>")=IEN8925
        KILL ARRAY MERGE ARRAY=OUTARR
        QUIT
        ;
CHGIMAGE(MAGIEN,TMGDFN)  ;
        ;"Purpose: To redirect an image to a new patient
        ;"         This changes the name and the patient
        ;"Input: MAGIEN -- IEN of image
        ;"       TMGDFN -- IEN of patient
        ;"Output: TMGRESULT="1^SUCCESS" or "-1^"Error Message
        SET TMGRESULT="1^SUCCESS"
        NEW PTNAME,PTSSN,ZN
        SET ZN=$GET(^DPT(TMGDFN,0))
        SET PTNAME=$PIECE(ZN,"^",1)
        SET PTSSN=$PIECE(ZN,"^",9)
        SET $PIECE(^MAG(2005,MAGIEN,0),"^",1)=PTNAME_"    "_PTSSN
        SET $PIECE(^MAG(2005,MAGIEN,0),"^",7)=TMGDFN
CIDN    QUIT TMGRESULT
 ;"==================================================================
 ;"==================================================================
        ;
ARR2LABS(TMGDFN,ARRAY) ;"SAVE LABS.INTO FILE 63.04 IN 63
        ;"INPUT: TMGDFN -- PATIENT IEN
        ;"       ARRAY -- PASS BY REFERENCE.  Format:
        ;"          ARRAY(FMDT,LAB)=VALUE
        ;"              LAB format:  'IEN60^Name', or
        ;"                           '<IEN8925>' <-- used to create comment of source. Value is the actual IEN 8925
        ;"Result; 1^OK, or -1^Message
        NEW TMGRESULT SET TMGRESULT="1^OK"
        NEW IEN8925 SET IEN8925=0
        NEW PRIORLABS
        ;"Get prior values based on components of first entry.
        ;"Assumes first SET will contain all components of every other set.
        NEW FMDT SET FMDT=+$ORDER(ARRAY(0))
        IF FMDT'>0 DO  GOTO A2LDN
        . SET TMGRESULT="-1^No date time found in passed information."
        NEW LAB SET LAB=""
        FOR  SET LAB=$ORDER(ARRAY(FMDT,LAB)) QUIT:LAB=""  DO
        . IF LAB="<IEN8925>" SET IEN8925=+$GET(ARRAY(FMDT,LAB)) QUIT
        . NEW IEN60 SET IEN60=+LAB
        . DO GETVALS^TMGLRR01(TMGDFN_"^2",IEN60,.PRIORLABS)
        SET FMDT=0
        FOR  SET FMDT=+$ORDER(ARRAY(FMDT)) QUIT:(FMDT'>0)!(+TMGRESULT<1)  DO
        . SET TMGRESULT=$$STOR1LAB(TMGDFN,FMDT,.ARRAY,.PRIORLABS)
A2LDN   QUIT TMGRESULT
        ;
LABALRDY(FMDT,LAB,PRIORLABS) ;
        ;"Purpose: Determine IF lab, as found in ARRAY at FMDT already exists,
        ;"         as shown by existence in PRIORLABS
        ;"INPUT: FMDT  -- FILEMAN DT of lab in question
        ;"       LAB -- Expected format IEN60^LabName (exact name match required)
        ;"       PRIORLABS -- Array of labs already stored in file 63.
        ;"Result: 1 IF lab match found already in file 63, or   0 IF no match
        NEW TMGRESULT SET TMGRESULT=0
        NEW ADT SET ADT=0
        FOR  SET ADT=$ORDER(PRIORLABS(LAB,ADT)) QUIT:(+ADT'>0)!(TMGRESULT=1)  DO
        . NEW DIFF SET DIFF=$$FMDIFF^XLFDT(FMDT,ADT,1) ;"RETURNS # DAYS DIFF
        . IF DIFF<0 SET DIFF=-1*DIFF
        . IF DIFF<4 SET TMGRESULT=1  ;"Consider labs to be same IF dates are within 3 days of each other
        . IF FMDT=ADT SET TMGRESULT=0 ;"If exactly same, allow rewriting of data.  TEMPORARY.
        QUIT TMGRESULT
        ;
STOR1LAB(TMGDFN,FMDT,ARRAY,PRIORLABS)  ;"STORE 1 LAB SET (ALL FROM 1 DATE)
        ;"INPUT -- FMDT - DT in array to store
        ;"         ARRAY -- contains data to store
        ;"         ARRAY(FMDT,LAB)=VALUE
        ;"         ARRAY(FMDT,"<IEN8925>")=IEN_8925
        ;"             LAB format:  'IEN60^Name', or
        ;"                          '<IEN8925>' <-- used to create comment of source. Value is the actual IEN 8925
        ;"Result: 1^OK, or -1^Error Message
        NEW TMGRESULT SET TMGRESULT="1^OK"
        NEW U SET U="^"
        NEW IEN8925 SET IEN8925=+$GET(ARRAY(FMDT,"<IEN8925>"))
        IF IEN8925'>0 DO  GOTO S1LDN
        . SET TMGRESULT="-1^Unable to determine source note IEN"
        NEW DOCIEN SET DOCIEN=+$GET(^TIU(8925,IEN8925,0))
        NEW NOTETITLE SET NOTETITLE=$PIECE($GET(^TIU(8925.1,DOCIEN,0)),"^",1)
        NEW PROVIEN SET PROVIEN=$PIECE($GET(^TIU(8925,IEN8925,12)),"^",2)  ;"12;2 = AUTHOR
        NEW LOCIEN4 SET LOCIEN4=+$ORDER(^VA(200,PROVIEN,2,0)) ;"If user has more than one division, then numerically smallest is chosen.
        IF LOCIEN4'>0 DO  GOTO S1LDN
        . SET TMGRESULT="-1^Unable to determine DIVISON for user #"_PROVIEN_" in file 200."
        NEW LABOUT,IDX SET IDX=0
        SET IDX=IDX+1,LABOUT(IDX)="<METADATA>"
        SET IDX=IDX+1,LABOUT(IDX)="PATIENT = "_TMGDFN
        SET IDX=IDX+1,LABOUT(IDX)="DT_TAKEN = "_FMDT
        SET IDX=IDX+1,LABOUT(IDX)="PROVIDER = "_PROVIEN
        SET IDX=IDX+1,LABOUT(IDX)="LOCATION = "_LOCIEN4
        NEW ALABADDED SET ALABADDED=0
        NEW LAB SET LAB=""
        SET IDX=IDX+1,LABOUT(IDX)="<VALUES>"
        FOR  SET LAB=$ORDER(ARRAY(FMDT,LAB)) QUIT:(LAB="")!(+TMGRESULT'>0)  DO
        . IF LAB="<IEN8925>" QUIT
        . IF $$LABALRDY(FMDT,LAB,.PRIORLABS) QUIT
        . NEW VALUE SET VALUE=$GET(ARRAY(FMDT,LAB)) QUIT:VALUE=""
        . NEW IEN60 SET IEN60=+LAB
        . NEW SPEC DO DFLTSPEC^TMGRPCL0(.SPEC,IEN60)  ;"Get default specimen for lab.  Result in SPEC(0)
        . IF $GET(SPEC(0))'>0 SET TMGRESULT=$GET(SPEC(0),"-1^Error getting default specimen") QUIT
        . SET SPEC=+SPEC(0)
        . NEW SPECNAME SET SPECNAME=$PIECE(SPEC(0),"^",2)
        . SET IDX=IDX+1,LABOUT(IDX)=IEN60_U_VALUE_"^^0^0^"_FMDT_U_SPECNAME_U_SPEC
        . SET ALABADDED=1
        IF ALABADDED=0 GOTO S1LDN
        SET IDX=IDX+1,LABOUT(IDX)="<COMMENTS>"
        SET IDX=IDX+1,LABOUT(IDX)="NOTICE: Values entered manually. Human/typographic errors possible."
        SET IDX=IDX+1,LABOUT(IDX)="Source: "_$PIECE($GET(^VA(200,PROVIEN,0)),"^",1)_" in note "_NOTETITLE_" on "_$$FMTE^XLFDT(FMDT,"5D")
        DO POSTLABS^TMGRPCL0(.TMGRESULT,.LABOUT)
        SET TMGRESULT=$GET(TMGRESULT(0))
S1LDN   QUIT TMGRESULT
        ;
