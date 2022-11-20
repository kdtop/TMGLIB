TMGTIUO6 ;TMG/kst-Text objects for use in CPRS ; 11/25/12, 2/2/14, 5/18/18, 3/24/21
         ;;1.0;TMG-LIB;**1,17**;7/20/12
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
 ;" This is spill over code from TMGTIUOJ, to make that file size smaller.
 ;"
 ;"=======================================================================
 ;"PUBLIC FUNCTIONS
 ;"=======================================================================
 ;"GETTABL1(TMGDFN,LABEL)--Entry point for TIU objects, to return a table comprised from 1 prior table.
 ;"GETTABLX(TMGDFN,LABEL) --Entry point for TIU objects, to return a table comprised from prior notes.
 ;"GETTBLST(OUT) -- return a list of all defined tables.
 ;"FIXABREV(STR) -- Fix abbreviations in medication descriptions.    
 ;"ISREMDLG(LABEL) -- IS LABEL A REMINDER DIALOG TYPE TABLE?
 ;"ISREMDLG(LABEL) -- IS LABEL A REMINDER DIALOG TYPE TABLE?
 ;"ISINLINE(LABEL) -- IS INLINE TYPE TABLE?
 ;"TABLMODE(LABEL) -- Return mode of table.                            
 ;
 ;"=======================================================================
 ;"PRIVATE FUNCTIONS
 ;"=======================================================================
 ;"STUBRECS(TMGDFN,ARRAY,LABEL)  -- add stubs for recommended studies to Array
 ;"CHKTAGS(LABEL,ARRAY)  -- To add tags to appropriate lines
 ;"TESTLABL(VALUE,CONST) ; 
 ;"=======================================================================
 ;"Dependancies : TMGPXR02
 ;"=======================================================================
GETTBLST(OUT) ;"GET TABLE LIST
    ;"Purpose: return a list of all defined tables.
    ;"Input: OUT -- PASS BY REFERENCE, an OUT PARAMETER.  Format:
    ;"    OUT("[<tablename>]")=""
    ;"Result: none
    NEW NAME SET NAME="TMG TABLE"
    FOR  SET NAME=$ORDER(^TIU(8925.1,"B",NAME)) QUIT:(NAME'["TMG TABLE")  DO
    . NEW IEN SET IEN=$ORDER(^TIU(8925.1,"B",NAME,0)) QUIT:+IEN'>0
    . NEW CODE SET CODE=$GET(^TIU(8925.1,IEN,9)) QUIT:CODE=""
    . NEW LABEL SET LABEL=$PIECE(CODE,"[",2),LABEL=$PIECE(LABEL,"]",1)
    . IF LABEL="" QUIT  ;"4/24/20 ELH
    . SET OUT("["_LABEL_"]")=""
    QUIT
    ;
STUBRECS(TMGDFN,ARRAY,LABEL)  ;"STUB RECOMMENDATIONS
    ;"Purpose: to add stubs for recommended studies to Array
    ;"Get age from TMGDFN
    SET TMGDFN=+$GET(TMGDFN)
    IF TMGDFN=0 GOTO SRDONE
    NEW AGE SET AGE=+$$GET1^DIQ(2,TMGDFN,.033)
    NEW SEX SET SEX=$$GET1^DIQ(2,TMGDFN,.02)
    NEW DOB SET DOB=$$GET1^DIQ(2,TMGDFN,.03,"I")
    ;
    IF $$TESTLABL(LABEL,"LIPIDS") DO
    . DO ENSURE^TMGTIUO3(.ARRAY,"Total Cholesterol","=")
    . DO ENSURE^TMGTIUO3(.ARRAY,"LDL Cholesterol","=")
    . DO ENSURE^TMGTIUO3(.ARRAY,"HDL Cholesterol","=")
    . DO ENSURE^TMGTIUO3(.ARRAY,"Triglycerides","=")
    . DO ENSURE^TMGTIUO3(.ARRAY,"Date of last lipid panel")
    . DO ENSURE^TMGTIUO3(.ARRAY,"LDL Goal")
    . DO ENSURE^TMGTIUO3(.ARRAY,"Liver Enzymes")
    ELSE  IF $$TESTLABL(LABEL,"SOCIAL HX") DO
    . DO ENSURE^TMGTIUO3(.ARRAY,"Tobacco")
    . IF (AGE>13) DO ENSURE^TMGTIUO3(.ARRAY,"EtOH")
    ELSE  IF $$TESTLABL(LABEL,"THYROID") DO
    . DO ENSURE^TMGTIUO3(.ARRAY,"Date of last study")
    . DO ENSURE^TMGTIUO3(.ARRAY,"TSH","=")
    ELSE  IF $$TESTLABL(LABEL,"HYPERTENSION") DO
    . DO ENSURE^TMGTIUO3(.ARRAY,"Date of last electrolytes")
    . DO ENSURE^TMGTIUO3(.ARRAY,"EKG")
    . ;"DO ENSURE^TMGTIUO3(.ARRAY,"Med-1")
    ELSE  IF $$TESTLABL(LABEL,"ANEMIA") DO
    . DO ENSURE^TMGTIUO3(.ARRAY,"Hgb")
    . DO ENSURE^TMGTIUO3(.ARRAY,"Serum Fe")
    . DO ENSURE^TMGTIUO3(.ARRAY,"TIBC")
    . DO ENSURE^TMGTIUO3(.ARRAY,"% Sat")
    . DO ENSURE^TMGTIUO3(.ARRAY,"B12")
    . DO ENSURE^TMGTIUO3(.ARRAY,"Folate")
    . DO ENSURE^TMGTIUO3(.ARRAY,"Workup")
    ELSE  IF $$TESTLABL(LABEL,"ASTHMA") DO
    . DO ENSURE^TMGTIUO3(.ARRAY,"Peak Flow Personal Best")
    . DO ENSURE^TMGTIUO3(.ARRAY,"Rescue Inhaler Freq")
    . DO SETBYPXR^TMGTIUO3(TMGDFN,.ARRAY,"Pneumovax",":","IMMUNIZATIONS","Pneumococcal")
    . DO ENSURE^TMGTIUO3(.ARRAY,"Triggers")
    . DO ENSURE^TMGTIUO3(.ARRAY,"Smoker")
    . DO ENSURE^TMGTIUO3(.ARRAY,"Nocturnal Symptoms")
    ELSE  IF $$TESTLABL(LABEL,"COPD") DO
    . DO ENSURE^TMGTIUO3(.ARRAY,"Rescue Inhaler Freq")
    . DO SETBYPXR^TMGTIUO3(TMGDFN,.ARRAY,"Pneumovax",":","IMMUNIZATIONS","Pneumococcal")
    . DO ENSURE^TMGTIUO3(.ARRAY,"Pulmonologist")
    . DO ENSURE^TMGTIUO3(.ARRAY,"Home O2")
    . DO ENSURE^TMGTIUO3(.ARRAY,"PFT Testing")
    . DO ENSURE^TMGTIUO3(.ARRAY,"Tobacco Cessation Counselling")
    ELSE  IF $$TESTLABL(LABEL,"OSTEOPENIA/OSTEOPOROSIS") DO
    . DO ENSURE^TMGTIUO3(.ARRAY,"Bone Density")
    . DO ENSURE^TMGTIUO3(.ARRAY,"T-Score Spine/Hips")
    . DO REMOVE^TMGTIUO3(.ARRAY,"Advised Calcium ~1500 mg & Vit-D 1000-2000 IU")
    ELSE  IF $$TESTLABL(LABEL,"CHF") DO
    . DO SETBYPXR^TMGTIUO3(TMGDFN,.ARRAY,"Pneumovax",":","IMMUNIZATIONS","Pneumococcal")
    . DO ENSURE^TMGTIUO3(.ARRAY,"Cardiologist")
    . DO ENSURE^TMGTIUO3(.ARRAY,"Last echocardiogram")
    . DO ENSURE^TMGTIUO3(.ARRAY,"Educated about sodium intake")
    . DO ENSURE^TMGTIUO3(.ARRAY,"Dry Weight")
    ELSE  IF ($$TESTLABL(LABEL,"MEDICATIONS"))!($$TESTLABL(LABEL,"FINAL MEDICATIONS")) DO
    . DO ENSURE^TMGTIUO3(.ARRAY,"*Source of information")
    . DO ENSURE^TMGTIUO3(.ARRAY,"*Reconciliation date")
    . DO ENSURE^TMGTIUO3(.ARRAY,"*Allergies sync'd with ERx on date")
    . DO ENSURE^TMGTIUO3(.ARRAY,"*CSM-Database Review")
SRDONE  QUIT
    ;
CHKTAGS(LABEL,ARRAY)   ;
    ;"Purpose: To add tags to appropriate lines
    ;"Input: Label - Label for desired table
    ;"       Array - Pass by reference
    ;"Output: Array
    ;"Result: None
    IF LABEL["MEDICATION" DO
    . NEW DATE
    . SET DATE=$GET(ARRAY("KEY-VALUE","*CSM-Database Review"))
    . SET DATE=DATE_"TEST"
    . SET ARRAY("KEY-VALUE","*CSM-Database Review")=DATE
    . SET ARRAY("KEY-VALUE","*CSM-Database Review","LINE")=DATE
    QUIT
    ;
TESTLABL(VALUE,CONST) ;
    IF VALUE=CONST QUIT 1
    QUIT (VALUE=("["_CONST_"]"))
    ;
FIXABREV(STR) ;"Fix abbreviations in medication descriptions.
    NEW ARR DO SPLIT2AR^TMGSTUT2(STR," ",.ARR) KILL ARR("MAXNODE")
    NEW JDX,IDX 
    DO FIXABVW(.ARR)
    ;"SET IDX="" FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:IDX=""  DO
    ;". NEW WORD,UWORD SET WORD=$GET(ARR(IDX)),UWORD=$$UP^XLFSTR(WORD)
    ;". IF UWORD="QD" SET ARR(IDX)="Daily"
    ;". IF UWORD="QDAY" SET ARR(IDX)="Daily"
    ;". IF UWORD["MG" DO
    ;". . FOR JDX=IDX-1:-1:1 QUIT:($GET(ARR(JDX))'="")  KILL ARR(JDX) 
    ;". . NEW L SET L=$LENGTH(WORD)
    ;". . IF ($EXTRACT(UWORD,L-1,L)'="MG")!($EXTRACT(WORD,L-2)=" ") QUIT
    ;". . NEW PREFIX SET PREFIX=$$TRIM^XLFSTR($EXTRACT(WORD,1,L-2))
    ;". . IF PREFIX="" SET ARR(IDX)="mg"
    ;". . ELSE  SET ARR(IDX)=PREFIX_" mg"
    ;". IF UWORD="DAILY" SET ARR(IDX)="QDAY"
    SET (STR,IDX)="" FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:IDX=""  DO
    . SET STR=STR_$GET(ARR(IDX))_" "
    SET STR=$$TRIM^XLFSTR(STR)
    QUIT STR
    ;
FIXABVA(ARR)  ;"Fix abreviations of medications in a line array
    ;"INPUT -- ARR.  Format ARR(#)=<line of text>
    NEW JDX,IDX SET IDX=0
    FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:IDX'>0  DO
    . NEW LINE SET LINE=$GET(ARR(IDX)) QUIT:LINE=""
    . NEW WORDARR DO SPLIT2AR^TMGSTUT2(LINE," ",.WORDARR) KILL WORDARR("MAXNODE")
    . DO FIXABVW(.WORDARR)
    . SET (LINE,JDX)="" 
    . FOR  SET JDX=$ORDER(WORDARR(JDX)) QUIT:JDX=""  SET LINE=LINE_$GET(WORDARR(JDX))_" "
    . SET ARR(IDX)=$$TRIM^XLFSTR(LINE)
    QUIT
    ;
FIXABVW(ARR)  ;"Fix abreviations of medications in a word array
    ;"INPUT -- ARR.  Format ARR(#)=<1 word>
    NEW JDX,IDX SET IDX="" FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:IDX=""  DO
    . NEW WORD,UWORD SET WORD=$GET(ARR(IDX)),UWORD=$$UP^XLFSTR(WORD)
    . IF UWORD="QD" SET ARR(IDX)="Daily"
    . IF UWORD="QDAY" SET ARR(IDX)="Daily"
    . IF UWORD["MG" DO
    . . FOR JDX=IDX-1:-1:1 QUIT:($GET(ARR(JDX))'="")  KILL ARR(JDX) 
    . . NEW L SET L=$LENGTH(WORD)  ;"10mg
    . . IF ($EXTRACT(UWORD,L-1,L)'="MG")!($EXTRACT(WORD,L-2)=" ") QUIT
    . . NEW PREFIX SET PREFIX=$$TRIM^XLFSTR($EXTRACT(WORD,1,L-2))
    . . IF PREFIX="" SET ARR(IDX)="mg"
    . . ELSE  SET ARR(IDX)=PREFIX_" mg"
    . ;"//kt 6/15/18IF UWORD="DAILY" SET ARR(IDX)="QDAY"
    QUIT
    ;"
ISREMDLG(LABEL) ;"IS LABEL A REMINDER DIALOG TYPE TABLE?
    NEW MODE SET MODE=$$TABLMODE(LABEL)
    QUIT (MODE="R")
    ;
ISLABCMT(LABEL) ;"IS LABEL A LAB & COMMENTS TYPE TABLE?
    NEW MODE SET MODE=$$TABLMODE(LABEL)
    QUIT (MODE="LC")
    ;    
ISINLINE(LABEL) ;"IS INLINE TYPE TABLE?
    NEW MODE SET MODE=$$TABLMODE(LABEL)
    QUIT (MODE="I")
    ;
TABLMODE(LABEL) ;"Return mode of table.
    ;"Result: 0 IF traditional table, or IF new-mode table is disabled., or R or LC
    NEW RESULT SET RESULT=0
    IF LABEL["[" SET LABEL=$PIECE(LABEL,"[",2),LABEL=$PIECE(LABEL,"]",1)
    NEW IEN SET IEN=+$ORDER(^TMG(22708,"B",LABEL,0))
    IF IEN'>0 GOTO TBLMODE
    NEW ZN SET ZN=$GET(^TMG(22708,IEN,0))
    IF $PIECE(ZN,"^",6)="Y" GOTO TBLMODE  ;"0;6 = DISABLED field
    SET RESULT=$PIECE(ZN,"^",5) IF RESULT="" SET RESULT=0
TBLMODE QUIT RESULT
    ;    
  ;"DELOLD(ARRAY) ;"Remove old, unwanted entries in table.
  ;"    KILL ARRAY("KEY-VALUE","TDAP / TD")
  ;"    KILL ARRAY("KEY-VALUE","TD")
  ;"    KILL ARRAY("KEY-VALUE","PNEUMOVAX")  ;"Get re-added /refreshed in STUBRECS
  ;"    KILL ARRAY("KEY-VALUE","ZOSTAVAX")
  ;"    KILL ARRAY("KEY-VALUE","TDAP")              
  ;"    KILL ARRAY("KEY-VALUE","HEPATITIS C SCREEN")
  ;"    QUIT
  ;"    ;
GETTABL1(TMGDFN,LABEL,ARRAY,OPTION) ;
    ;"Purpose: A call point for TIU objects, to return a table comprised from 1 prior table.
    ;"NOTE: This type of table just gets the *LAST* table found (not a compilation)
    ;"Note: The MEDICATIONS table should retrieve the prior '[FINAL MEDICATIONS]' IF possible
    ;"Note: The FINAL MEDICATIONS table should retrieve the same as MEDICATIONS table. It will then be edited by provider.
    ;"Input: TMGDFN- the patient IEN
    ;"       LABEL -- the table label, e.g. [MEDICATIONS]
    ;"       ARRAY -- OPTIONAL.  PASS BY REFERENCE to get back array of data.
    ;"       OPTION("DT")=FMDT <-- if present, then table is to be returns AS OF given date
    ;"Result: outputs string of result, with CRLF's as needed for multiple lines
GT1 NEW RESULT SET RESULT=""
    ;"Note: the MEDICATIONS table is now a Lab&Comment table, so should is typically handled in $$GETTABLX.
    ;"//kt 1/15/17 --> IF LABEL["MEDICATIONS" SET RESULT=$$GMEDTABL(TMGDFN,LABEL,.ARRAY) GOTO GT1DONE
    IF $GET(LABEL)="" GOTO GT1DONE
    NEW SPACES SET SPACES=""
    DO GETSPECL^TMGTIUO4(TMGDFN,LABEL,"BLANK_LINE",48,.ARRAY,1,.SPACES,.OPTION)  ;"mode 1 = only last table; 2=compile
    ;"DO DELOLD(.ARRAY)  <-- no longer needed.  
    DO WRAPMISC(.ARRAY)  ;"4/12/22
    SET RESULT=SPACES_"-- "_LABEL_" ---------"_$CHAR(13)_$CHAR(10)
    DO STUBRECS(.TMGDFN,.ARRAY,LABEL)
    DO CHKTAGS(LABEL,.ARRAY)
    NEW SAVEARRAY MERGE SAVEARRAY=ARRAY
    SET RESULT=RESULT_$$ARRAY2ST^TMGTIUO4(.ARRAY,.SPACES)
    KILL ARRAY MERGE ARRAY=SAVEARRAY
GT1DONE ;
    QUIT RESULT
    ;
LOADKILL(KILLARRAY,LABEL)  ;"
    ;"Purpose: This routine will find all labels in the "LINE ITEMS TO REMOVE" field number 11
    ;"Input: KILLARRAY("LINE TO REMOVE")=""  (Return array)
    ;"       IEN - IEN of the table
    NEW IDX SET IDX=0
    IF LABEL["[" SET LABEL=$PIECE(LABEL,"[",2),LABEL=$PIECE(LABEL,"]",1)
    NEW IEN SET IEN=+$ORDER(^TMG(22708,"B",LABEL,0))
    FOR  SET IDX=$ORDER(^TMG(22708,IEN,2,IDX)) QUIT:IDX'>0  DO
    . NEW KILLNAME SET KILLNAME=$$UP^XLFSTR($G(^TMG(22708,IEN,2,IDX,0)))
    . SET KILLARRAY(KILLNAME)=""
    QUIT
    ;"
TOKILL(LINE,LABEL,KILLARRAY)  ;
    ;"Purpose: take the current line and determine if the line title is one
    ;"         that should be killed
    ;"Input: LINE - Line to check
    ;"       LABEL - LABEL of current table
    ;"       KILLARR -- OPTIONAL.  Used with repeat calls to spead execution
    ;"Result: 0 to not kill, 1 to kill
    NEW TMGRESULT SET TMGRESULT=0
    NEW HEADER SET HEADER=""
    FOR  SET HEADER=$ORDER(KILLARRAY(HEADER)) QUIT:HEADER=""  DO
    . NEW LINEHEADER 
    . IF LINE["=" SET LINEHEADER=$P(LINE,"=",1)
    . ELSE  IF LINE[":" SET LINEHEADER=$P(LINE,":",1)
    . ELSE  SET LINEHEADER=""
    . SET LINEHEADER=$$UP^XLFSTR($$TRIM^XLFSTR(LINEHEADER))
    . IF LINEHEADER=HEADER SET TMGRESULT=1
    QUIT TMGRESULT
    ;"
GETTABLX(TMGDFN,LABEL,ARRAY,OPTION) ;
    ;"Purpose: A call point for TIU objects, to return a table comprised from prior notes.
    ;"Input: TMGDFN- the patient IEN
    ;"       LABEL -- the table label, e.g. MEDICATIONS
    ;"       ARRAY -- OPTIONAL.  PASS BY REFERENCE to get back array of data.
    ;"       OPTION -- OPTIONAL.
    ;"              OPTION("DT")=FMDT <-- if present, then table is to be returns AS OF given date
    ;"       OPTION("ALL NOTES")=0 or 1  <- Use all notes (as
    ;"                               opposed to only completed notes)
    ;"            OPTION("DIRECT HTML INSERTION")=1  <-- Output should be ready to insert directly into HTML DOM
    ;"                    Note: so far, only implemented with LABCMTBL
    ;"Note: If table label matches a Reminder Dialog type table, then handling shunted elsewhere.
    ;"NOTE: This uses globally scoped variable (optional) TMGCPRSHTMLMODE
    ;"Result: outputs string of result, with CRLF's as needed for multiple lines
    NEW TMGTABLDEBUG SET TMGTABLDEBUG=0
    IF TMGTABLDEBUG DO
    . SET LABEL=$GET(^TMG("TMP","RPC","GETTABLX","LABEL"))
    . SET TMGDFN=$GET(^TMG("TMP","RPC","GETTABLX","DFN"))
    . KILL OPTION MERGE OPTION=^TMG("TMP","RPC","GETTABLX","OPTION")
    . KILL ARRAY
    ELSE  DO
    . KILL ^TMG("TMP","RPC","GETTABLX")
    . SET ^TMG("TMP","RPC","GETTABLX","LABEL")=LABEL
    . SET ^TMG("TMP","RPC","GETTABLX","DFN")=TMGDFN
    . MERGE ^TMG("TMP","RPC","GETTABLX","OPTION")=OPTION
    NEW RESULT SET RESULT=""
    IF '$DATA(OPTION("HTML")) SET OPTION("HTML")=$GET(TMGCPRSHTMLMODE,1) ;"<-- This will be set by a RPC called from CPRS during mode setting.
    IF $$SHOULDGARBLE^TMGMISC4(),(LABEL="[SOCIAL HX]")!(LABEL="[FAMILY HX]") DO  GOTO GTXDONE
    . SET RESULT="-- "_LABEL_" ---------"
    NEW IRDLG SET IRDLG=$$ISREMDLG(LABEL) 
    IF IRDLG SET RESULT=$$RMDGTABL^TMGPXR02(TMGDFN,LABEL,,.ARRAY,.OPTION) GOTO GTXDONE
    NEW ILCMT SET ILCMT=$$ISLABCMT(LABEL)
    IF ILCMT SET RESULT=$$LABCMTBL^TMGTIUO8(TMGDFN,LABEL,.ARRAY,.OPTION) GOTO GTXDONE
    NEW IILT SET IILT=$$ISINLINE(LABEL)
    IF IILT SET RESULT=$$INLNTABL^TMGTIUO8(TMGDFN,LABEL,.ARRAY,.OPTION) GOTO GTXDONE
    SET RESULT=$$GETTABL1(.TMGDFN,.LABEL,.ARRAY,.OPTION) ;  
GTXDONE QUIT RESULT
    ;
WRAPMISC(ARRAY)  ;"THIS ROUTINE WILL TAKE A LINE FORWARDED AS A MISC LINE OF THE TABLE AND DETERMINE IF  
                ;" ACTION IS REQUIRED ON IT. IF IT IS, IT WILL BE HIGHLIGHTED IN RED AND A TAG WILL
                ;" BE ADDED SO THE NOTE CANNOT BE SIGNED WITHOUT IT BEING ADDRESSED
        NEW TAGS SET TAGS="{REQ}^{REQUIRED}^{HI}^{HIGH}"
        NEW I,REQUIRED
        NEW IDX SET IDX=0
        FOR  SET IDX=$O(ARRAY(IDX)) QUIT:IDX'>0  DO
        . SET LINE=$G(ARRAY(IDX))
        . SET REQUIRED=0
        . FOR I=1:1:4  DO
        . . NEW TAG SET TAG=$P(TAGS,"^",I)
        . . IF $$UP^XLFSTR(LINE)[TAG SET REQUIRED=1
        . IF REQUIRED=1 DO
        . . SET LINE=$$HTML2TXS^TMGHTM1(LINE)
        . . SET LINE=$$WRAPTEXT^TMGTIUOT($P(LINE,"{",1),"#ff0000",.OPTION)_"{"_$P(LINE,"{",2)
        . . SET LINE=LINE_" [^^!REQUIRED FOLLOWUP ITEM!^^]"
        . . NEW OPTION ;"SET OPTION("DIRECT HTML INSERTION")=1
        . . ;"SET HTML=0  ;"HTML ON SYSTEM TABLE SHOULD NOW BE OFF
        . . SET ARRAY(IDX)=LINE
        QUIT
        ;"    
