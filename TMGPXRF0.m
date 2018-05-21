TMGPXRF0 ;TMG/kst/TMG Reminder Reports stuff ;6/7/13, 2/2/14
         ;;1.0;TMG-LIB;**1**;5/21/13
 ;
 ;"TMG REMINDER FUNCTIONS
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
 ;"PUBLIC FUNCTIONS
 ;"=======================================================================
 ;"FDOY(LIST,FIEVAL,DIFF)  -- Return the Day of the Year (1-365) of most recent finding
 ;"FMOY(LIST,FIEVAL,RESULT) ;Return the month of the Year (1-12) of most recent finding
 ;"FYR(LIST,FIEVAL,RESULT) ;Return the Year (4 digits) of most recent finding
 ;
 ;"=======================================================================
 ;"PRIVATE FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"Dependancies : XLFDT
 ;"=======================================================================
 ;
FDOY(LIST,FIEVAL,RESULT) ;Return the Day of the Year (1-365) of most recent finding
        ;"Entry point for REMINDER FUNCTION FINDING FUNCTION (#802.4)
        ;"Input: LIST -- PASS BY REFERENCE.  Format
        ;"          LIST(0)=number of parameters specified for this function
        ;"          LIST(1)="<something>"  <-- the first parameter
        ;"          LIST(2)="<something>"  <-- the second parameter (note: should happen for FDOY)
        ;"       FIEVAL -- PASS BY REFERENCE.  Format:
        ;"         FIEVAL(#,"FINDING")=vptr to a specific finding, e.g. "19;AUTTIMM("
        ;"         FIEVAL("SEX")=# (0 or 1, I think)
        ;"       RESULT -- OUT PARAMENTER (the RESULT) for the function.
        IF LIST(0)<1 SET DIFF=0 QUIT
        NEW DATE SET DATE=+$G(FIEVAL(LIST(1),"DATE"))
        NEW YEAR SET YEAR=$EXTRACT(DATE,1,3)
        SET RESULT=+$$FMDIFF^XLFDT(DATE,YEAR_"0101",1)+1  ;"RESULT is DIFF
        QUIT
        ;
FMOY(LIST,FIEVAL,RESULT) ;Return the month of the Year (1-12) of most recent finding
        ;"Entry point for REMINDER FUNCTION FINDING FUNCTION (#802.4)
        ;"Input: See FDOY above for discussion
        SET RESULT=0  ;"default
        IF LIST(0)<1 QUIT
        NEW DATE SET DATE=$G(FIEVAL(LIST(1),"DATE"))
        IF DATE'="" DO
        . SET RESULT=1
        SET RESULT=+$EXTRACT(+DATE,4,5)
        QUIT
        ;
FYR(LIST,FIEVAL,RESULT) ;Return the Year (4 digits) of most recent finding
        ;"Entry point for REMINDER FUNCTION FINDING FUNCTION (#802.4)
        ;"Input: See FDOY above for discussion
        SET RESULT=0  ;"default
        IF LIST(0)<1 QUIT
        NEW DATE SET DATE=+$G(FIEVAL(LIST(1),"DATE"))
        IF DATE'>0 QUIT
        SET RESULT=+$EXTRACT(DATE,1,3)+1700
        QUIT
        ;
SCRIPT(LIST,FIEVAL,RESULT) ;Run custom script, returning a NUMBER
        ;"Input: LIST -- PASS BY REFERENCE.  Format
        ;"          LIST(0)=number of parameters specified for this function
        ;"          LIST(1)="<something>"  <-- the first parameter
        ;"          LIST(2)="<something>"  <-- the second parameter
        ;"       FIEVAL -- PASS BY REFERENCE.  Format:
        ;"         FIEVAL(#,"FINDING")=vptr to finding, e.g. "19;AUTTIMM("
        ;"         FIEVAL(#,#2)=1 or 0  (I think)
        ;"         FIEVAL("SEX")=# 1 or 0  (I think)
        ;"       RESULT -- OUT PARAMENTER.  Result for the function -- 0 or 1 
        ;
        ;"This will be called from EVAL^PXRMFF
        ;"NOTE: Some globally-scoped variable may be depended upon, but should used as READ-ONLY
        ;"      PXRMITEM is defined, and is IEN of reminder definition being processed.
        ;"      FFN = the subIEN (field 25,FUNCTION FINDINGS).  Format: "FF#", e.g. "FF1" for subIEN=1
        ;"      DEFARR array contains the reminder definition raw field values etc.
        ;"      PXRMPDEM array contains patient demographics
        SET RESULT=0  ;"default
        NEW IEN SET IEN=+$GET(PXRMITEM) GOTO:(IEN'>0) SCRPTDN 
        NEW SUBIEN SET SUBIEN=+$PIECE($GET(FFN),"FF",2) GOTO:(SUBIEN'>0) SCRPTDN
        NEW NODE SET NODE=$GET(^PXD(811.9,IEN,25,SUBIEN,"TMG"))
        NEW EN SET EN=$PIECE(NODE,"^",2) GOTO:(EN="") SCRPTDN
        NEW ROUTINE SET ROUTINE=$PIECE(NODE,"^",1) GOTO:(ROUTINE="") SCRPTDN
        NEW REF SET REF=EN_"^"_ROUTINE
        IF $TEXT(@REF)="" GOTO SCRPTDN
        NEW CODE SET CODE="DO "_REF_"(.LIST,.FIEVAL,.RESULT)"
        XECUTE CODE  ;"Any errors will be caught / recorded by reminder engine             
SCRPTDN        QUIT
