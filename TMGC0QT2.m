TMGC0QT2 ;TMG/kst/TMG C0Q Data-test code ;10/28/12, 2/2/14, 5/14/14
         ;;1.0;TMG-LIB;**1**;10/24/12
 ;
 ;"TMG C0Q FUNCTIONS
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"NOTE: code started as code written by George Lilly.
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"TMGRX(ADFN,PROV,SDTE,EDTE,VISITS,C0QLIST,RESULT,WHY,ZYR) ;Check for medication issues
 ;"     Creates C0Q PATIENT LIST's"
 ;"       ZYR_"HasERX"
 ;"       ZYR_"HasMed"
 ;"       ZYR_"HasMedOrders"
 ;"       ZYR_"NoMed"
 ;"       ZYR_"NoMedOrders"
 ;"       ZYR_"HasMedRecon"
 ;"       ZYR_"NoMedRecon"
 ;"       ZYR_"HasERXAllergies"
 ;"       ZYR_"NoERXAllergies"
 ;"OBESITY(ADFN,PROV,SDTE,EDTE,VISITS,C0QLIST,RESULT,WHY,ZYR) ;Obesity measurements.
 ;"     Creates C0Q PATIENT LIST's"
 ;"       ZYR_"BMI_OK_or_Has_Follow_Up_Plan"
 ;"       ZYR_"BMI_NOT_OK_or_NO_Follow_Up_Plan"
 ;"       ZYR_"Over17",ADFN)=""
 ;"       ZYR_"BMI_Age_18+_Obese"
 ;"       ZYR_"BMI_Age_18+_Obese_w_FUPlan"
 ;"       ZYR_"BMI_Age_18-64"
 ;"       ZYR_"BMI_Age_18-64_OK_or_Has_F/U_Plan"
 ;"       ZYR_"BMI_Age_18-64_NOT_OK_or_NO_F/Up_Plan"
 ;"       ZYR_"BMI_Age_65+"
 ;"       ZYR_"BMI_Age_Greater_Than_Or_Equal__To_65"        
 ;"       ZYR_"BMI_Age_65+_OK_or_Has_F/Up_Plan"
 ;"       ZYR_"BMI_Age_65+_NOT_OK_or_NO_F/U_Plan"
 ;"TOBACCO(ADFN,PROV,SDTE,EDTE,VISITS,C0QLIST,RESULT,WHY,ZYR) ;
 ;"     Creates C0Q PATIENT LIST's"
 ;"       ZYR_"Over12"
 ;"       ZYR_"NeedsSmokingStatus"
 ;"       ZYR_"HasSmokingStatus"
 ;"       ZYR_"NoSmokingStatus"
 ;"       ZYR_"Over17"
 ;"       ZYR_"IsTobaccoUser"
 ;"       ZYR_"NeedsTobaccoIntervention"
 ;"       ZYR_"IsNotTobaccoUser"
 ;"       ZYR_"HasTobaccoIntervention"
 ;"       ZYR_"NoTobaccoIntervention"
 ;"HTN(ADFN,PROV,SDTE,EDTE,VISITS,C0QLIST,RESULT,WHY,ZYR) ;Assess HTN Clinical Quality Measure
 ;"     Creates C0Q PATIENT LIST's"
 ;"       ZYR_"HTN_HasDiagnosis"
 ;"       ZYR_"HTN_HasBPRecorded"
 ;"       ZYR_"HTN_NOBPRecorded"
 ;"       ZYR_"HTN_BPControlled"
 ;"       ZYR_"HTN_BPNotControlled"
 ;"DM(ADFN,PROV,SDTE,EDTE,VISITS,C0QLIST,RESULT,WHY,ZYR) ;Assess Diabetic Clinical Quality Measure
 ;"     Creates C0Q PATIENT LIST's"
 ;"       ZYR_"DM_HasDxAndAge18-75"
 ;"       ZYR_"DM_PoorControl(Above9.0)"
 ;"       ZYR_"DM_OKControl(Below8.0)"
 ;"       ZYR_"DM_GoodControl(Below7.0)" 
 ;"       (also sets values from LIPIDS below), and
 ;"       ZYR_"DM_HasDxAndAge18-75AndLDL<100"
 ;"LIPIDS(ADFN,PROV,SDTE,EDTE,VISITS,C0QLIST,RESULT,WHY,ZYR) ;Assess Lipid control
 ;"     Creates C0Q PATIENT LIST's"
 ;"       ZYR_"Lipids_LDL<70"
 ;"       ZYR_"Lipids_LDL<100"
 ;"       ZYR_"Lipids_LDL<130"
 ;"       ZYR_"Lipids_LDL<160"  
 ;"PNEUMO(ADFN,PROV,SDTE,EDTE,VISITS,C0QLIST,RESULT,WHY,ZYR) ;Assess Pneumococcal Clinical Quality Measure
 ;"     Creates C0Q PATIENT LIST's"
 ;"       ZYR_"Pneumococcal_Age65YrsOrMore"
 ;"       ZYR_"Pneumococcal_HasBeenGiven"
 ;"       ZYR_"Pneumococcal_NotGiven"
 ;"CONTCARE(ADFN,PROV,SDTE,EDTE,VISITS,C0QLIST,RESULT,WHY,ZYR) ;Assess % patients with summary record sent to other providers
 ;"     Creates C0Q PATIENT LIST's"
 ;"       ZYR_"CareTransitionRecord_Initiated" //Overloading use.  TIUIEN instead of DFN
 ;"       ZYR_"CareTransitionRecord_DueNow"    //Overloading use.  TIUIEN instead of DFN
 ;"       ZYR_"CareTransitionRecord_Sent"      //Overloading use.  TIUIEN instead of DFN
 ;"       ZYR_"CareTransitionRecord_Overdue"   //Overloading use.  TIUIEN instead of DFN        
 ;"OSTEOPOR(ADFN,PROV,SDTE,EDTE,VISITS,C0QLIST,RESULT,WHY,ZYR) ;Test screening or therapy for osteoporosis
 ;"     Creates C0Q PATIENT LIST's"
 ;"       ZYR_"Osteoporosis_NeedsScreen"
 ;"       ZYR_"Osteoporosis_DoneOrRxGiven"
 ;"       ZYR_"Osteoporosis_NOT_DoneOrRxGiven"
 ;"ADVANCDR(ADFN,PROV,SDTE,EDTE,VISITS,C0QLIST,RESULT,WHY,ZYR) ;Test Advanced directives
 ;"       ZYR_"AdvancedDirectives_NeedsDocumentation"
 ;"       ZYR_"AdvancedDirectives_Satisfied"
 ;"       ZYR_"AdvancedDirectives_NOT_Satisfied"
 ;"MAMMO(ADFN,PROV,SDTE,EDTE,VISITS,C0QLIST,RESULT,WHY,ZYR) ;Test Breast cancer screening
 ;"       ZYR_"BreastCancerScreening_NeedsScreening"
 ;"       ZYR_"BreastCancerScreening_Satisfied"
 ;"       ZYR_"BreastCancerScreening_NOT_Satisfied"
 ;"COLONCA(ADFN,PROV,SDTE,EDTE,VISITS,C0QLIST,RESULT,WHY,ZYR) ;Test colon cancer screening
 ;"       ZYR_"ColorCancerScreening_NeedsScreening"
 ;"       ZYR_"ColonCancerScreening_Satisfied"
 ;"       ZYR_"ColonCancerScreening_NOT_Satisfied"
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"SRCHERX(ADFN,DTE,EDTE) ;
 ;"GETBMI(ADFN,BMI,DT,IDEALWTS,WHY) ;-- Get BMI and calculation date
 ;"WTINTRV(ADFN,SDTE,EDTE) -- Determine IF patient has had an intervention during the time range
 ;"ADRECSNT(TIUIEN) ;Check addenda indicates records were already sent
 ;"HASADDEN(TIUIEN,LIST) ;Get list of addenda documents
 ;"RECSENT(TIUIEN) ;CHECK IF '{Records Sent}' in document
 ;"PARSEBP(STR,SYS,DIA) ;"Parse blood pressure
 ;"STR2DATES(STR,OUT) -- Convert string containing dates and other text, into array of FM dates
 ;"ISDATE(WORD,OUTFMDT) -- Determine if word is a date, and if so, turn into FM date
 ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"=======================================================================
 ;"  XLFSTR, XLFDT, C0QUTIL, TMGTIUO*, TMGC0Q* TIULO
 ;"=======================================================================
 ;"NOTE: The following routines used the following parameters
        ;"Input: ADFN -- the patient IEN (in PATIENT file)
        ;"       PROV -- the provider IEN (in NEW PERSON file)
        ;"       SDTE -- Starting date (FM format) of range to test patient in
        ;"       EDTE -- Ending date (FM format) of range to test patient in
        ;"       VISITS -- PASS BY REFERENCE.  A list of FM dates for visits in date range.
        ;"       C0QLIST -- AN OUT PARAMETER.  PASS BY REFERENCE.
        ;"       RESULT -- OPTIONAL.  PASS BY REFERENCE, AN OUT PARAMETER
        ;"                returns 1 IF successful, 0 IF failure
        ;"       WHY -- OPTIONAL.  PASS BY REFERENCE.  AN OUT PARAMETER
        ;"               Returns string explaining internal logic
        ;"       ZYR -- prefix for C0QLIST
  ;
TMGRX(ADFN,PROV,SDTE,EDTE,VISITS,C0QLIST,RESULT,WHY,ZYR)   ;" Check for medication issues
        SET RESULT=1 ;"default of success
        SET WHY="{ERx test}: "
        NEW HASMED SET HASMED=0
        NEW HASERXSYNC SET HASERXSYNC=0
        NEW HASALLERGYSYNC SET HASALLERGYSYNC=0
        NEW HASMEDREC SET HASMEDREC=0
        NEW MEDLABEL SET MEDLABEL="[MEDICATIONS]"
        NEW TABARRAY
        NEW S SET S=$$GETTABLX^TMGTIUO6(ADFN,MEDLABEL,.TABARRAY)
        NEW VALUE
        SET VALUE=$$UP^XLFSTR($GET(TABARRAY("KEY-VALUE","*ALLERGIES SYNC'D WITH ERX ON DATE")))
        IF VALUE="<NO DATA>" SET VALUE=""
        IF VALUE'="" SET HASALLERGYSYNC=1
        ;"IF HASERXSYNC=0 SET HASERXSYNC=$$SRCHERX(ADFN,DTE,EDTE)  ;...evidence was found in note for electric scripts.. "
        SET VALUE=$$UP^XLFSTR($GET(TABARRAY("KEY-VALUE","*RECONCILIATION DATE")))
        IF VALUE="<NO DATA>" SET VALUE=""
        IF VALUE'="" SET HASMEDREC=1
        KILL TABARRAY("KEY-VALUE")
        IF $DATA(TABARRAY) SET HASMED=1
        ;
        SET RESULT=(HASMED)&(HASMEDREC)&(HASALLERGYSYNC)
        IF HASMED DO
        . SET C0QLIST(PROV,ZYR_"HasERX",ADFN)=""
        . SET C0QLIST(PROV,ZYR_"HasMed",ADFN)=""
        . SET C0QLIST(PROV,ZYR_"HasMedOrders",ADFN)=""
        . IF RESULT=1 SET WHY=WHY_" Medication entry WAS found. "
        ELSE  DO  ;
        . SET C0QLIST(PROV,ZYR_"NoMed",ADFN)=""
        . SET C0QLIST(PROV,ZYR_"NoMedOrders",ADFN)=""
        . IF RESULT=0 SET WHY=WHY_$$NURSEPRE^TMGC0QT1()_" No medication entry found.] "
        IF HASMEDREC DO
        . SET C0QLIST(PROV,ZYR_"HasMedRecon",ADFN)=""
        . IF RESULT=1 SET WHY=WHY_"Reconciliation date was found in Med table. "
        ELSE  DO
        . SET C0QLIST(PROV,ZYR_"NoMedRecon",ADFN)=""
        . IF RESULT=0 SET WHY=WHY_$$NURSEPRE^TMGC0QT1()_" NO value was found for 'RECONCILIATION DATE : <value>' in [MEDICATIONS]table.] "
        IF HASALLERGYSYNC DO
        . SET C0QLIST(PROV,ZYR_"HasERXAllergies",ADFN)=""
        . IF RESULT=1 SET WHY=WHY_"Evidence was found that allergies have been sync'd to ERx. "
        ELSE  DO
        . SET C0QLIST(PROV,ZYR_"NoERXAllergies",ADFN)=""
        . IF RESULT=0 SET WHY=WHY_$$NURSEPRE^TMGC0QT1()_" NO value found for 'ALLERGIES SYNC'D WITH ERX ON DATE: <value>' in MEDICATIONS table,] "
        QUIT
        ;
OBESITY(ADFN,PROV,SDTE,EDTE,VISITS,C0QLIST,RESULT,WHY,ZYR) ;" Obesity measurements.
        SET WHY="{Obesity test}: "        
        NEW TMGWT,TMGWTTALK,HASBMI,BMI,LASTDT,DTAGE
        NEW POPULATION SET POPULATION=0
        NEW NEEDSPLAN SET NEEDSPLAN=0
        NEW ISOK SET ISOK=0
        NEW AGE SET AGE=$$AGE^C0QUTIL(ADFN)
        IF AGE<18 DO  GOTO OBDN  ;" DON'T CHECK UNDER AGE 18
        . SET WHY=WHY_" Skipping due to age <  18 yrs. "
        SET C0QLIST(PROV,ZYR_"Over17",ADFN)=""
        NEW IDEALWTS
        SET HASBMI=$$GETBMI(ADFN,.BMI,.LASTDT,.IDEALWTS,.WHY) ;
        IF AGE>64 DO
        . SET WHY=WHY_"For age ("_AGE_" YR), BMI should be 22-30. "
        . IF (BMI'<30)!(BMI<22) SET NEEDSPLAN=1
        . SET POPULATION=2
        ELSE  DO  ;"AGES 18-64
        . SET WHY=WHY_"For age ("_AGE_" YR), BMI should be 18.5-25. "
        . IF (BMI'<25)!(BMI<18.5) SET NEEDSPLAN=1
        . SET POPULATION=1
        SET WHY=WHY_"Pt's BMI="_BMI_". "
        IF NEEDSPLAN=0 DO  GOTO OBSTORE
        . SET ISOK=1
        . SET WHY=WHY_"OK. No intervention needed. "
        ELSE  DO
        . NEW BMICOMMENT SET BMICOMMENT=$$BMICOMNT^TMGTIUO4(BMI,AGE,.IDEALWTS)
        . SET WHY=WHY_"Comment: "_BMICOMMENT
        . SET C0QLIST(PROV,ZYR_"BMI_Age_18+_Obese",ADFN)=""   ;added 1/3/13
        IF HASBMI=0 DO  GOTO OBSTORE  ;"Needed BMI is missing.
        . SET WHY=WHY_"BMI missing. "
        NEW LASTVISIT SET LASTVISIT=$ORDER(VISITS(""),-1)
        IF LASTVISIT="" SET LASTVISIT=$$NOW^XLFDT
        SET DTAGE=$$FMDIFF^XLFDT(LASTVISIT,LASTDT,1) ;"Diff, in DAYS
        IF DTAGE>180 DO  GOTO OBSTORE  ;"measures > 6 months before visit of consideration
        . SET WHY=WHY_"BMI calculation > 6 months old (probably no recent weight). "
        NEW HASINTRV SET HASINTRV=$$WTINTRV(ADFN,SDTE,EDTE)
        IF HASINTRV=1 DO
        . SET ISOK=1
        . SET WHY=WHY_"Evidence of weight discussion/intervention was found."
        . SET C0QLIST(PROV,ZYR_"BMI_Age_18+_Obese_w_FUPlan",ADFN)=""   ;added 1/3/13
        ELSE  DO
        . SET WHY=WHY_"NO Evidence of weight discussion/intervention was found."
OBSTORE IF POPULATION=1 DO
        . SET C0QLIST(PROV,ZYR_"BMI_Age_18-64",ADFN)=""
        . IF ISOK DO
        . . SET C0QLIST(PROV,ZYR_"BMI_Age_18-64_OK_or_Has_F/U_Plan",ADFN)=""
        . . SET RESULT=1
        . ELSE  DO
        . . SET C0QLIST(PROV,ZYR_"BMI_Age_18-64_NOT_OK_or_NO_F/Up_Plan",ADFN)=""
        . . SET RESULT=0
        IF POPULATION=2 DO
        . SET C0QLIST(PROV,ZYR_"BMI_Age_65+",ADFN)=""
        . SET C0QLIST(PROV,ZYR_"BMI_Age_Greater_Than_Or_Equal__To_65",ADFN)=""        
        . IF ISOK DO
        . . SET C0QLIST(PROV,ZYR_"BMI_Age_65+_OK_or_Has_F/Up_Plan",ADFN)=""
        . . SET RESULT=1
        . ELSE  DO
        . . SET C0QLIST(PROV,ZYR_"BMI_Age_65+_NOT_OK_or_NO_F/U_Plan",ADFN)=""
        . . SET RESULT=0
OBDN    QUIT
        ;
TOBACCO(ADFN,PROV,SDTE,EDTE,VISITS,C0QLIST,RESULT,WHY,ZYR)   ;
        ;"NOTE: see also code in TOBACCO^TMGC0QT4 for future tests...
        SET RESULT=0 ;"default of failure
        SET WHY="{Tobacco Use test}: "
        NEW C0QSMOKE,TMGTTALK
        IF $$AGE^C0QUTIL(ADFN)<13 DO  GOTO SMKDN  ;" DON'T CHECK UNDER AGE 13
        . SET WHY=WHY_"OK. (Test skipped because patient age < 13). "
        . SET RESULT=1
        SET C0QLIST(PROV,ZYR_"Over12",ADFN)=""
        SET C0QLIST(PROV,ZYR_"NeedsSmokingStatus",ADFN)=""
        NEW C0QSYN,TMGTOBSTATUS
        SET C0QSYN=0
        NEW ARRAY,S
        SET S=$$GETTABLX^TMGTIUO6(ADFN,"[SOCIAL HX]",.ARRAY)
        SET S=$GET(ARRAY("KEY-VALUE","TOBACCO"))
        SET S=$$TRIM^XLFSTR(S)
        IF S="." SET S=""
        SET WHY=WHY_" SOCIAL HX table Tobacco status=["_S_"]. "
        IF S="<NO DATA>" SET S=""
        IF S'="" SET C0QSYN=1 
        SET TMGTOBSTATUS=S
        IF C0QSYN DO
        . SET C0QLIST(PROV,ZYR_"HasSmokingStatus",ADFN)=""
        . SET WHY=WHY_"Smoking status IS noted (OK). "
        ELSE  DO  GOTO SMKDN
        . SET C0QLIST(PROV,ZYR_"NoSmokingStatus",ADFN)=""
        . SET WHY=WHY_"Smoking status is MISSING. "
        IF $$AGE^C0QUTIL(ADFN)<18 DO  GOTO SMKDN  ;" DON'T CHECK FOR INTERVENTION UNDER AGE 18
        . SET WHY=WHY_" Skipping test for tobacco USE due to age<18 yrs. "
        . SET RESULT=1
        SET C0QLIST(PROV,ZYR_"Over17",ADFN)=""
        ;"SET WHY=WHY_"Tobacco status="_TMGTOBSTATUS_". "
        NEW ISUSER
        IF $EXTRACT(TMGTOBSTATUS,1,3)="YES" DO
        . SET C0QLIST(PROV,ZYR_"IsTobaccoUser",ADFN)=""
        . SET C0QLIST(PROV,ZYR_"NeedsTobaccoIntervention",ADFN)=""
        . SET WHY=WHY_"IS tobacco user. "
        . SET ISUSER=1
        ELSE  DO  GOTO SMKDN
        . SET C0QLIST(PROV,ZYR_"IsNotTobaccoUser",ADFN)=""
        . SET WHY=WHY_"Is NOT tobacco user (OK). "
        . SET ISUSER=0
        . SET RESULT=1
        SET TMGTTALK=$$INLSTTOB^TMGC0Q05(ADFN,SDTE,EDTE)
        IF TMGTTALK DO
        . SET C0QLIST(PROV,ZYR_"HasTobaccoIntervention",ADFN)=""
        . SET WHY=WHY_" Tobacco intervention found (OK). "
        . SET RESULT=1
        ELSE  DO
        . SET C0QLIST(PROV,ZYR_"NoTobaccoIntervention",ADFN)=""
        . SET WHY=WHY_" No tobacco intervention for during date range. "
        . SET RESULT=0 ;"redundant, default = 0
SMKDN   QUIT
        ;
HTN(ADFN,PROV,SDTE,EDTE,VISITS,C0QLIST,RESULT,WHY,ZYR) ;"Assess HTN Clinical Quality Measure
        SET RESULT=0 ;"default of failure
        SET WHY="{HTN CQM Use test}: "
        NEW AGE SET AGE=$$AGE^C0QUTIL(ADFN)
        IF (AGE<18)!(AGE>85) DO  GOTO HTNDN  ;" DON'T CHECK for some ages
        . SET WHY=WHY_"OK. (Test skipped because patient age not 18-85). "
        . SET RESULT=1
        ;"NOTE: The formal documentation for this requirement is for patients
        ;"      at least 18 years with HTN that have been see at least **TWICE**
        ;"      with a BP recorded.  Since we record a BP on **ALL** patients,
        ;"      I will make this test valid when then have been seen *once*.
        ;"IF $$LISTCT^TMGMISC2("VISITS")<2 DO  GOTO HTNDN
        ;". SET WHY=WHY_"OK. (Test skipped because patient not seen twice). "
        ;". SET RESULT=1
        NEW HASHTN SET HASHTN=$$INLSTHTN^TMGC0Q05(ADFN,SDTE,EDTE)
        IF HASHTN DO
        . SET C0QLIST(PROV,ZYR_"HTN_HasDiagnosis",ADFN)=""
        . SET WHY=WHY_"HTN diagnosis was found. "
        ELSE  DO  GOTO HTNDN
        . SET WHY=WHY_"Patient doesn't have HTN Diagnosis."
        . SET RESULT=1
        NEW BPVALUE SET BPVALUE=$$DOVITALS^TIULO(ADFN,"BP")
        IF BPVALUE'="" DO
        . SET C0QLIST(PROV,ZYR_"HTN_HasBPRecorded",ADFN)=""
        . SET WHY=WHY_"BP Value has been checked and recorded. "
        . SET RESULT=1
        . NEW SYS,DIA 
        . DO PARSEBP(BPVALUE,.SYS,.DIA) ;"Parse blood pressure
        . IF (SYS<140)&(DIA<90) DO
        . . SET C0QLIST(PROV,ZYR_"HTN_BPControlled",ADFN)=""
        . ELSE  DO
        . . SET C0QLIST(PROV,ZYR_"HTN_BPNotControlled",ADFN)=""
        ELSE  DO
        . SET C0QLIST(PROV,ZYR_"HTN_NOBPRecorded",ADFN)=""
        . SET WHY=WHY_"BP Value has NOT been checked and recorded. "
HTNDN   QUIT
        ;
DM(ADFN,PROV,SDTE,EDTE,VISITS,C0QLIST,RESULT,WHY,ZYR) ;"Assess Diabetic Clinical Quality Measure
        ;"This will be coded to check for poor control (NQF 0059), and
        ;"  OK control (NQF #0575)
        SET RESULT=0 ;"default of failure
        SET WHY="{DM CQM Use tests}: "
        NEW AGE SET AGE=$$AGE^C0QUTIL(ADFN)
        IF (AGE<18)!(AGE>75) DO  GOTO DMDN  ;" DON'T CHECK AGE <18 or >75 YEARS
        . SET WHY=WHY_"OK. (Test skipped because patient age < 18 or > 75 yrs). "
        . SET RESULT=1
        NEW HASDM SET HASDM=$$INLSTDM^TMGC0Q05(ADFN,SDTE,EDTE)
        IF HASDM DO
        . SET C0QLIST(PROV,ZYR_"DM_HasDxAndAge18-75",ADFN)=""
        . SET WHY=WHY_"DM diagnosis was found. "
        ELSE  DO  GOTO DMDN
        . SET WHY=WHY_"Patient doesn't have DM Diagnosis. "
        . SET RESULT=1
        NEW ARRAY
        SET S=$$GETTABLX^TMGTIUO6(ADFN,"[DIABETIC STUDIES]",.ARRAY)
        SET S=$GET(ARRAY("KEY-VALUE","HGBA1C"))
        IF S="" SET S=$GET(ARRAY("KEY-VALUE","HgbA1c"))
        SET S=$$TRIM^XLFSTR(S)
        IF S="." SET S=""
        SET WHY=WHY_" HgbA1c status=["_S_"]. "
        IF S="<NO DATA>" SET S=""
        IF S="" SET WHY=WHY_"HgbA1c status missing. " GOTO DMDN
        NEW VALUES,MSG
        DO PARSEA1C^TMGTIUO7(S,.VALUES,.MSG)
        IF $GET(MSG)'="" DO  GOTO DMDN
        . SET WHY=WHY_$PIECE(MSG,"^",2,99)
        NEW IDX SET IDX=+$ORDER(VALUES(0))
        IF +IDX'>0 SET WHY=WHY_"Unable to parse HgbA1c values from diabetes table.  " GOTO DMDN
        NEW LATEST SET LATEST=$GET(VALUES(IDX))
        NEW LATESTDATE SET LATESTDATE=$PIECE(LATEST,"^",2)
        IF +$EXTRACT(LATESTDATE,1,3)<+$EXTRACT(SDTE,1,3) DO  GOTO DMDN  ;"Just compare years
        . SET WHY=WHY_"Latest HgbA1c value not in this measurement year period (too old). " 
        NEW LATESTVAL SET LATESTVAL=$PIECE(LATEST,"^",1)
        IF LATESTVAL="" SET WHY=WHY_"Unable to find latest HgbA1c value. " GOTO DMDN
        IF LATESTVAL>9.0 DO
        . SET C0QLIST(PROV,ZYR_"DM_PoorControl(Above9.0)",ADFN)=""
        . SET WHY=WHY_"HgbA1c > 9.0 --> indicates poor control. "
        IF LATESTVAL<8.0 DO
        . SET C0QLIST(PROV,ZYR_"DM_OKControl(Below8.0)",ADFN)=""
        . SET WHY=WHY_"HgbA1c < 8.0 --> 'adequate' control. "
        . SET RESULT=1
        IF LATESTVAL<7.0 DO
        . SET C0QLIST(PROV,ZYR_"DM_GoodControl(Below7.0)",ADFN)=""
        . SET WHY=WHY_"HgbA1c < 7.0 --> good control. "
        . SET RESULT=1
        ;"FOR NQF 0064, must ALSO check lipids for diabetics.  Will do as follows.
        NEW TEMPRESULT,TEMPWHY
        DO LIPIDS(ADFN,PROV,SDTE,EDTE,.VISITS,.C0QLIST,.TEMPRESULT,.TEMPWHY,ZYR)  
        IF $DATA(C0QLIST(PROV,ZYR_"DM_HasDxAndAge18-75",ADFN))&$DATA(C0QLIST(PROV,ZYR_"Lipids_LDL<100",ADFN)) DO
        . SET C0QLIST(PROV,ZYR_"DM_HasDxAndAge18-75AndLDL<100",ADFN)=""
DMDN    QUIT
        ;
LIPIDS(ADFN,PROV,SDTE,EDTE,VISITS,C0QLIST,RESULT,WHY,ZYR) ;"Assess Lipid control
        ;"This will be coded to check for lipid control.  Specifically, I am working
        ;"on NQF #0064      
        SET RESULT=1 ;"default of SUCCESS
        SET WHY="{Lipid control tests}: "
        NEW AGE SET AGE=$$AGE^C0QUTIL(ADFN)
        IF (AGE<18)!(AGE>75) DO  GOTO DMDN  ;" DON'T CHECK AGE <18 or >75 YEARS
        . SET WHY=WHY_"OK. (Test skipped because patient age < 18 or > 75 yrs). "
        . SET RESULT=1
        NEW ARRAY
        SET S=$$GETTABLX^TMGTIUO6(ADFN,"[LIPIDS]",.ARRAY)
        DO PARSTABL^TMGTIUO7(.ARRAY)  ;"Parse out data values
        SET S=$GET(ARRAY("KEY-VALUE","LDL Cholesterol"))                
        SET S=$$TRIM^XLFSTR(S)
        IF S="<NO DATA>" SET S=""
        SET WHY=WHY_" LDL data=["_S_"]. "
        IF S="" SET WHY=WHY_"Lipid data missing. " GOTO LPDN
        NEW MINLDL SET MINLDL=9999
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(ARRAY("KEY-VALUE","LDL Cholesterol","PARSED",IDX)) QUIT:IDX=""  DO
        . NEW LINE SET LINE=$GET(ARRAY("KEY-VALUE","LDL Cholesterol","PARSED",IDX)) ;"Value^FMDate^SourceText"
        . NEW DT SET DT=$PIECE(LINE,"^",2) QUIT:DT=""
        . IF (DT<SDTE)!(DT>EDTE) QUIT
        . NEW VALUE SET VALUE=$PIECE(LINE,"^",1)
        . IF VALUE<MINLDL SET MINLDL=VALUE
        IF MINLDL=9999 DO  GOTO LPDN
        . SET WHY=WHY_"Lipid data missing. "
        IF MINLDL<160 DO
        . SET C0QLIST(PROV,ZYR_"Lipids_LDL<160",ADFN)=""
        IF MINLDL<130 DO
        . SET C0QLIST(PROV,ZYR_"Lipids_LDL<130",ADFN)=""
        IF MINLDL<100 DO
        . SET C0QLIST(PROV,ZYR_"Lipids_LDL<100",ADFN)=""
        IF MINLDL<70 DO
        . SET C0QLIST(PROV,ZYR_"Lipids_LDL<70",ADFN)=""
LPDN    QUIT
        ;        
PNEUMO(ADFN,PROV,SDTE,EDTE,VISITS,C0QLIST,RESULT,WHY,ZYR) ;"Assess Pneumococcal Clinical Quality Measure
        ;"This will be coded to check for pneumococcal vaccination (NQF 0043), 
        SET RESULT=0 ;"default of failure
        SET WHY="{PNEUMOCOCCAL CQM Use tests}: "
        NEW AGE SET AGE=$$AGE^C0QUTIL(ADFN)
        IF (AGE<65) DO  GOTO PVXDN  ;" DON'T CHECK AGE <65 
        . SET WHY=WHY_"OK. (Test skipped because patient age < 65 yrs). "
        . SET RESULT=1
        SET C0QLIST(PROV,ZYR_"Pneumococcal_Age65YrsOrMore",ADFN)=""
        ;SET S=$$GETTABLX^TMGTIUO6(ADFN,"[STUDIES]",.ARRAY)
        ;SET S=$GET(ARRAY("KEY-VALUE","PNEUMOVAX"))
        ;SET S=$$TRIM^XLFSTR(S)
        ;IF S="." SET S=""
        ;IF S="<NO DATA>" SET S=""
        ;IF S="" SET WHY=WHY_"Pneumovax status missing. " GOTO PVXDN
        ;SET WHY=WHY_" Pneumovax status=["_S_"]. "
        NEW VALUES,MSG
        ;DO PARSEPVX^TMGTIUO7(S,.VALUES,.MSG)
        ;IF $DATA(MSG) DO  GOTO PVXDN
        ;. SET WHY=WHY_$PIECE(MSG,"^",2,99)
        ;IF $DATA(VALUES)'>0 SET WHY=WHY_"Unable to parse Pneumovax values from [Studies] table.  " GOTO PVXDN
        NEW FOUND SET FOUND=0
        ;NEW FMDATE SET FMDATE=""
        ;"NEW PNEUIEN,DATE
        ;"SET PNEUIEN=$ORDER(^AUTTIMM("B","PNEUMOVAX",0))
        ;"SET DATE=$ORDER(^AUPNVIMM("AA",ADFN,PNEUIEN,0))
        NEW DATE,IMMARRAY,IDX,STRING
        IF $$GETTABLX^TMGTIUO6(ADFN,"[IMMUNIZATIONS]",.IMMARRAY)
        SET IDX=0
        SET DATE=$GET(IMMARRAY("KEY-VALUE","Pneumococcal"))
        IF DATE'="" SET FOUND=1
        ;FOR  SET FMDATE=$ORDER(VALUES(FMDATE)) QUIT:(+FMDATE'>0)!FOUND  DO
        ;. NEW TYPE SET TYPE=""
        ;. FOR  SET TYPE=$ORDER(VALUES(FMDATE,TYPE)) QUIT:(TYPE="")!FOUND  DO
        ;. . IF TYPE="Y" SET FOUND=1
        IF FOUND DO
        . SET C0QLIST(PROV,ZYR_"Pneumococcal_HasBeenGiven",ADFN)=""
        . SET WHY=WHY_"Pneumococcal vaccine has been given.. "
        . SET RESULT=1
        ELSE  DO
        . SET C0QLIST(PROV,ZYR_"Pneumococcal_NotGiven",ADFN)=""
        . SET WHY=WHY_"Pneumococcal vaccine has NOT been given.. "
PVXDN   QUIT
        ;
CONTCARE(ADFN,PROV,SDTE,EDTE,VISITS,C0QLIST,RESULT,WHY,ZYR) ;"Assess % patients with summary record sent to other providers
        ;"Purpose: Note: This function is different from other tests in that it is NOT
        ;"         patient specific.  However, this test will be called by the MU TIU object, so
        ;"         some reply should be returned.  This will therefore just return global % for date range.
        ;"NOTE: This not destinguish between providers because the doctor orders the
        ;"      consult, but the nurse creates the note documenting this.
        NEW NUMR,DENOM SET (NUMR,DENOM)=0
        SET RESULT=1  ;"Default to success
        SET WHY="{Records sent on transfer-of-care tests}: "
        NEW NOW SET NOW=$$NOW^XLFDT()\1
        NEW DATE SET DATE=SDTE-0.001
        FOR  SET DATE=$ORDER(^TIU(8925,"ATMGCNSLT",DATE)) QUIT:(+DATE'>0)!(DATE>EDTE)  DO
        . NEW TIUIEN SET TIUIEN=0
        . FOR  SET TIUIEN=$ORDER(^TIU(8925,"ATMGCNSLT",DATE,TIUIEN)) QUIT:(+TIUIEN'>0)  DO
        . . NEW DUEDATE SET DUEDATE=$GET(^TIU(8925,"ATMGCNSLT",DATE,TIUIEN))
        . . IF +DUEDATE'>0 QUIT     
        . . SET C0QLIST(PROV,ZYR_"CareTransitionRecord_Initiated",TIUIEN)=""  ;"//Overloading use.  TIUIEN instead of ADFN
        . . IF DUEDATE\1>NOW QUIT
        . . SET C0QLIST(PROV,ZYR_"CareTransitionRecord_DueNow",TIUIEN)=""  ;"//Overloading use.  TIUIEN instead of ADFN
        . . SET DENOM=DENOM+1
        . . NEW DONE SET DONE=0
        . . IF DUEDATE=1 SET DONE=1
        . . IF 'DONE SET DONE=$$ADRECSNT(TIUIEN)
        . . IF DONE DO
        . . . SET C0QLIST(PROV,ZYR_"CareTransitionRecord_Sent",TIUIEN)=""  ;"//Overloading use.  TIUIEN instead of ADFN
        . . . IF DUEDATE'=1 SET ^TIU(8925,"ATMGCNSLT",DATE,TIUIEN)=1
        . . . SET NUMR=NUMR+1
        . . ELSE  DO
        . . . SET C0QLIST(PROV,ZYR_"CareTransitionRecord_Overdue",TIUIEN)=""  ;"//Overloading use.  TIUIEN instead of ADFN        
        IF DENOM>0 DO
        . NEW PCT SET PCT=(NUMR/DENOM)*100
        . IF PCT'<50 DO  QUIT
        . . SET WHY=WHY_"OK. " 
        . SET RESULT=0
        . SET WHY="Current % of summary records to to other providers upon transition of care is only "_PCT_"%. "
        ELSE  DO
        . SET WHY=WHY_"OK. "
        QUIT
        ;
OSTEOPOR(ADFN,PROV,SDTE,EDTE,VISITS,C0QLIST,RESULT,WHY,ZYR) ;"Test screening or therapy for osteoporosis
        ;"This will be coded to check for screening or therapy for osteoporosis (NQF 0046)    
        SET RESULT=0 ;"default of failure
        SET WHY="{Osteoporosis Screening or Therapy}: "
        NEW AGE SET AGE=$$AGE^C0QUTIL(ADFN)
        IF (AGE<65) DO  GOTO OSTEODN  ;" DON'T CHECK AGE <65 
        . SET WHY=WHY_"OK. (Test skipped because patient age < 65 yrs). "
        . SET RESULT=1
        NEW SEX SET SEX=$PIECE($GET(^DPT(ADFN,0)),"^",2)
        IF SEX'="F" DO  GOTO OSTEODN
        . SET WHY=WHY_"OK. (Test skipped because patient is not female). "
        . SET RESULT=1
        SET C0QLIST(PROV,ZYR_"Osteoporosis_NeedsScreen",ADFN)=""
        NEW DONE SET DONE=0
        
        ;"NEED TESTING--> TEST HERE FOR DEXA SCAN PERFORMED OR ORDERED AT LEAST ONCE SINCE AGE 60        
        NEW X1,X2,X   ;"X1 IS DOB,X2 IS 60 YEARS IN DAYS, X IS AGE AT 60
        SET X1=$PIECE($GET(^DPT(ADFN,0)),"^",3)
        SET X2=60*365
        D C^%DTC
        IF $$GETHFDT^TMGPXRU1(ADFN,"TMG BONE DENSITY")>X SET DONE=1
        ;"
        IF DONE DO  GOTO OSTEODN
        . SET C0QLIST(PROV,ZYR_"Osteoporosis_DoneOrRxGiven",ADFN)=""
        . SET WHY=WHY_"Osteoporisis screening has been done. "
        . SET RESULT=1
        . SET DONE=1
        
        ;"NEED TESTING--> TEST HERE IF RX PRESCRIBED WITHIN 12 MONTHS. 
        NEW X
        D NOW^%DTC
        ;"X= TODAY, SUBTRACT 10000 TO GET DATE 1 YEAR AGO
        IF $$GETHFDT^TMGPXRU1(ADFN,"TMG BONE DENSITY RX PRESCRIBED")>(X-10000) SET DONE=1
        ;"
        IF DONE DO  GOTO OSTEODN
        . SET C0QLIST(PROV,ZYR_"Osteoporosis_DoneOrRxGiven",ADFN)=""
        . SET WHY=WHY_"Osteoporosis Rx has been precribed withing 12 months. "
        . SET RESULT=1
        . SET DONE=1
        IF 'DONE DO
        . SET C0QLIST(PROV,ZYR_"Osteoporosis_NOT_DoneOrRxGiven",ADFN)=""
        . SET WHY=WHY_"Osteoporisis screening has not been done since age 60, "
        . SET WHY=WHY_"and osteoporosis Rx has not been prescribed within 12 months. "        
OSTEODN QUIT        
        ;
ADVANCDR(ADFN,PROV,SDTE,EDTE,VISITS,C0QLIST,RESULT,WHY,ZYR) ;"Test Advanced directives
        ;"This will be coded to check for Advanced directives (NQF 0326)
        ;" -- Age >= 65 yrs
        ;" -- Have advance care plan documented in chart
        ;" -- Have surrogate decision maker documented in chart
        ;" -- or document it was discussed and patient didn't want to give plan
        ;" -- or document it was discussed and patient didn't want to name surrogate      
        SET RESULT=0 ;"default of failure
        SET WHY="{Advanced directives documentation}: "
        NEW AGE SET AGE=$$AGE^C0QUTIL(ADFN)
        IF (AGE<65) DO  GOTO ADVDDN  ;" DON'T CHECK AGE <65 
        . SET WHY=WHY_"OK. (Test skipped because patient age < 65 yrs). "
        . SET RESULT=1
        SET C0QLIST(PROV,ZYR_"AdvancedDirectives_NeedsDocumentation",ADFN)=""
        NEW DONE SET DONE=0
        
        ;"NEEDS TESTING--> TEST HERE FOR Advance care plan documented in chart.         
        IF $$GETHFDT^TMGPXRU1(ADFN,"TMG ADV CP DOCUMENTED")>0 SET DONE=1
        
        IF DONE DO  GOTO ADVD2
        . SET WHY=WHY_"Advance directives documented in chart. "
        ELSE  DO
        . SET WHY=WHY_"Advance directives are NOT documented in chart. "
        
        ;"NEEDS TESTING -->  FOR Advance care plan documented as REFUSED in chart.         
        IF $$GETHFDT^TMGPXRU1(ADFN,"TMG ADV CP DISCUSSED - NOT DESIRED")>0 SET DONE=1
        
        IF DONE DO  GOTO ADVDDN
        . SET WHY=WHY_"Advance directives documented as discussed, but patient didn't want to specify. "
        ELSE  DO
        . SET WHY=WHY_"Advance directives are NOT documented as refused/unable in chart. "
        . 
        ;"NEEDS TESTING--> TEST HERE IF surrogate decision maker documented in chart        
        IF $$GETHFDT^TMGPXRU1(ADFN,"TMG ADV SURROGATE DOCUMENTED")>0 SET DONE=1
        
        IF DONE DO  GOTO ADVDDN
        . SET WHY=WHY_"Surrogate decision maker documented in chart"
        ELSE  DO
        . SET WHY=WHY_"Surrogate decision maker NOT documented in chart"
        
        ;"NEEDS TESTING--> TEST HERE IF surrogate decision maker refused/unable         
        IF $$GETHFDT^TMGPXRU1(ADFN,"TMG ADV SURROGATE DISCUSSED - NOT NAMED")>0 SET DONE=1
        
        IF DONE DO  GOTO ADVDDN
        . SET WHY=WHY_"Surrogate decision documented as discussed, but patient didn't want to specify."
        ELSE  DO
        . SET WHY=WHY_"Surrogate decision NOT documented as discussed, but patient didn't want to specify."
        ;                
ADVD2   IF DONE DO
        . SET C0QLIST(PROV,ZYR_"AdvancedDirectives_Satisfied",ADFN)=""
        . SET RESULT=1
        ELSE  DO
        . SET C0QLIST(PROV,ZYR_"AdvancedDirectives_NOT_Satisfied",ADFN)=""
ADVDDN QUIT        
        ;
MAMMO(ADFN,PROV,SDTE,EDTE,VISITS,C0QLIST,RESULT,WHY,ZYR) ;"Test Breast cancer screening
        ;"This will be coded to check for Breast cancer screening (NQF N/A, PQRS #112 GPRO PREV-5)
        ;" -- Age 50-74 yrs
        ;" -- FEMALE
        ;" -- MAMMOGRAM DONE WITHIN 27 MONTHS.       
        SET RESULT=0 ;"default of failure
        SET WHY="{Breast Cancer screening}: "
        NEW AGE SET AGE=$$AGE^C0QUTIL(ADFN)
        IF (AGE<50)!(AGE>74) DO  GOTO ADVDDN  ;" DON'T CHECK AGE <50 OR >74 
        . SET WHY=WHY_"OK. (Test skipped because patient age not 50-74 yrs). "
        . SET RESULT=1
        NEW SEX SET SEX=$PIECE($GET(^DPT(ADFN,0)),"^",2)
        IF SEX'="F" DO  GOTO MAMODN
        . SET WHY=WHY_"OK. (Test skipped because patient is not female). "
        . SET RESULT=1
        SET C0QLIST(PROV,ZYR_"BreastCancerScreening_NeedsScreening",ADFN)=""
        NEW DONE SET DONE=0
        
        ;"NEEDS TESTING--> TEST HERE FOR Date of most recent mammogram
        ;"        Was study done after NOW-27 months?
        NEW X1,X2,X ;"X1 - TODAY, X2-27 MONTHS IN DAYS, X=NOW-27 MONTHS
        D NOW^%DTC
        SET X1=X,X2=-(27*30) ;"(27 MONTHS * 30 DAYS)
        D C^%DTC
        IF $$GETHFDT^TMGPXRU1(ADFN,"TMG MAMMOGRAM/IMAGING DONE")>X SET DONE=1
        
        IF DONE DO  
        . SET WHY=WHY_"Breast cancer screening has been done in past 27 months.. "
        . SET C0QLIST(PROV,ZYR_"BreastCancerScreening_Satisfied",ADFN)=""
        . SET RESULT=1
        ELSE  DO
        . SET C0QLIST(PROV,ZYR_"BreastCancerScreening_NOT_Satisfied",ADFN)=""
        . SET WHY=WHY_"Breast cancer screening has NOT been done in past 27 months. "
MAMODN  QUIT        
        ;                
COLONCA(ADFN,PROV,SDTE,EDTE,VISITS,C0QLIST,RESULT,WHY,ZYR) ;"Test colon cancer screening
        ;"This will be coded to check for colon cancer screening (NQF# 0304)
        ;" -- Age 50-75 yrs
        ;" -- Numerator: FOBT during measurement year
        ;"            of  colonoscopy during measurement year, or 9 yrs prior
        ;" -- Denominator: patients age 51-75 as of dec 31 of measurement year
        ;"                can exclude known CA     
        SET RESULT=0 ;"default of failure
        SET WHY="{Colon Cancer screening}: "
        NEW AGE SET AGE=$$AGE^C0QUTIL(ADFN)
        IF (AGE<50)!(AGE>75) DO  GOTO COLODN  ;" DON'T CHECK AGE <50 OR >75 
        . SET WHY=WHY_"OK. (Test skipped because patient age not 50-75 yrs). "
        . SET RESULT=1
        ;"NOTE: I am not going to code for known colon CA right now. 
        SET C0QLIST(PROV,ZYR_"ColorCancerScreening_NeedsScreening",ADFN)=""
        NEW DONE SET DONE=0
        
        ;"NEEDS TESTING--> TEST HERE FOR Date of most recent colonoscopy
        ;"        Was study done in past 10 yrs?
        ;"       or was FOBT done in past 1 yr?
        NEW X,DATE
        D NOW^%DTC
        SET DATE=$$GETHFDT^TMGPXRU1(ADFN,"TMG COLONOSCOPY COMPLETED")
        IF DATE>(X-100000) DO
        . SET DONE=1
        ELSE  DO
        . SET DATE=$$GETFOBT(ADFN)
        . IF DATE>(X-10000) SET DONE=1
     
        IF DONE DO  
        . SET WHY=WHY_"Colon cancer screening has been done. "
        . SET C0QLIST(PROV,ZYR_"ColonCancerScreening_Satisfied",ADFN)=""
        . SET RESULT=1
        ELSE  DO
        . SET C0QLIST(PROV,ZYR_"ColonCancerScreening_NOT_Satisfied",ADFN)=""
        . SET WHY=WHY_"Colon cancer screening has NOT been done.. "
COLODN  QUIT        
        ;                
  ;"------ Utility functions for these data tests above --------------
        ;
SRCHERX(ADFN,DTE,EDTE) ;
        ;"Purpose: to search through progress notes for patient, during time range
        ;"         for evidence of ERx.  This is TMG-site specific.  We copy
        ;"         the prescriptions from AllScripts into our progress notes.
        ;"         All copied scripts contain the text "- QUANTITY" and "- REFILL"
        ;"         A few may be lost due to line wrapping, but others will be found.
        ;"         Only 1 find is needed to be successful.
        ;"Input: ADFN -- the patient IEN (in PATIENT file)
        ;"       DTE -- Starting date (FM format) of range to test patient in
        ;"       EDTE -- Ending date (FM format) of range to test patient in
        ;"Result: 1 IF patient has evidence of ERx in date range, 0 IF not
        NEW RESULT SET RESULT=0
        NEW HASQUANT,HASRF
        NEW IEN SET IEN=""
        FOR  SET IEN=$ORDER(^TIU(8925,"C",ADFN,IEN),-1) QUIT:(IEN="")!(RESULT=1)  DO
        . NEW DT SET DT=+$PIECE($GET(^TIU(8925,IEN,0)),"^",7)
        . IF (DT<DTE)!(DT>EDTE) QUIT
        . SET (HASQUANT,HASRF)=0
        . NEW LN SET LN=0
        . FOR  SET LN=+$ORDER(^TIU(8925,IEN,"TEXT",LN)) QUIT:(LN'>0)!(RESULT=1)  DO
        . . NEW S SET S=$GET(^TIU(8925,IEN,"TEXT",LN,0)) QUIT:S=""
        . . IF S["- QUANTITY" SET HASQUANT=1
        . . IF S["- REFILL" SET HASRF=1
        . . IF HASQUANT&HASRF SET RESULT=1
        QUIT RESULT
        ;
GETBMI(ADFN,BMI,DT,IDEALWTS,WHY) ;
        ;"Purpose: get BMI and calculation date
        ;"NOTE: since this test is only for adults, the height will not change significantly
        ;"      So date will be based on weight calculation.
        ;"INPUT: ADFN -- PATIENT IEN
        ;"       BMI -- PASS BY REFERENCE, AN OUT PARAMETER.  RETURNS NUMERIC BMI
        ;"       DT -- PASSY BY REFERENCE, AN OUT PARAMETER, RETURNS date of measurement, in FM format; or 0 IF not found
        ;"       IDEALWTS -- OPTIONAL, AN OUT PARAMETER.  Ideal weight range-- Format : MinWt^MaxWt^PtWt
        ;"       WHY -- OPTIONAL.  PASS BY REFERENCE.  AN OUT PARAMETER
        ;"               Returns string explaining internal logic
        ;"Result: 1 IF successful, or 0 IF problem.
        NEW BMISTR,X,Y
        SET WHY=$GET(WHY)
        NEW RESULT SET RESULT=0
        NEW TEMPS
        SET BMISTR=$$BMI^TMGTIUOJ(ADFN,.BMI,.IDEALWTS,1,.WHY)
        IF BMI'>0 GOTO GBDN
        NEW DTSTR SET DTSTR=$PIECE($PIECE(BMISTR,"(",2),")",1)
        IF DTSTR="" GOTO GBDN
        SET X=DTSTR DO ^%DT SET DT=Y  ;"to FMFormat
        IF DT'>0 DO  GOTO GBDN
        . SET WHY=WHY_"Invalid date/format: "_DTSTR
        SET RESULT=1
GBDN    QUIT RESULT
        ;
WTINTRV(ADFN,SDTE,EDTE)        ;
        ;"Purpose: Determine IF patient has had an intervention during the time range
        ;"Input: ADFN -- the patient IEN (in PATIENT file)
        ;"       SDTE -- Starting date (FM format) of range to test patient in
        ;"       EDTE -- Ending date (FM format) of range to test patient in
        ;"Results: 1 IF patient does have interventions in date range, 0 IF not
        QUIT $$INLSTOBE^TMGC0Q05(ADFN,SDTE,EDTE)
        ;"
ADRECSNT(TIUIEN) ;"Check addenda indicates records were already sent
        ;"Input: TIUIEN --Parent document IEN in 8925
        ;"Result: 1 IF addendum shows records sent, 0 otherwise
        NEW LIST,RESULT SET RESULT=0 
        ;"Original -> IF $$HASADDEN^TMGC0Q03(TIUIEN,.LIST)=0 GOTO ADRSDN
        IF $$HASADDEN(TIUIEN,.LIST)=0 GOTO ADRSDN
        NEW ADDENDIEN SET ADDENDIEN=0
        FOR  SET ADDENDIEN=$ORDER(LIST(ADDENDIEN)) QUIT:(+ADDENDIEN'>0)!(RESULT=1)  DO
        . IF $$RECSENT(ADDENDIEN)=0 QUIT
        . SET RESULT=1
ADRSDN  QUIT RESULT
        ;
HASADDEN(TIUIEN,LIST) ;"Get list of addenda documents
        ;"Purpose: Return IF TIU document has addenda (and fill list of IEN's)
        ;"Input:   TIUIEN -- IEN in 8925
        ;"         LIST -- PASS BY REFERENCE, AN OUT PARAMETER.  Filled as follows
        ;"              LIST(IEN8925)=""
        ;"              LIST(IEN8925)=""
        ;"RESULTS: 0 IF no addenda, 1 IF has addenda
        MERGE LIST=^TIU(8925,"DAD",TIUIEN)
        QUIT ($DATA(LIST)'=0)
        ;
RECSENT(TIUIEN) ;"CHECK IF '{Records Sent}' in document
        ;"Input: TIUIEN -- IEN in 8925
        ;"Results: 1 IF {Records Sent} found in text of document
        NEW LINE SET LINE=0
        NEW FOUND SET FOUND=0
        FOR  SET LINE=$ORDER(^TIU(8925,TIUIEN,"TEXT",LINE)) QUIT:(+LINE'>0)!FOUND  DO
        . SET FOUND=($GET(^TIU(8925,TIUIEN,"TEXT",LINE,0))["{Records Sent}")
        QUIT FOUND
        ;
STR2DATES(STR,OUT) ;
        ;"Purpose: Convert string containing dates and other text, into array of FM dates
        ;"NOTE: may try to interpret other numbers as dates too
        ;"Input: STR -- the string to parse.  Can contain 1 or more dates
        ;"       OUT -- pass by reference.  AN OUT PARAMETER.  Format:
        ;"           OUT(FMDAT,<SourceTextOrValue>)=""
        ;"//kt note: line below got corrupted.  I fixed it, I think, 2/25/15
        SET STR=$TRANSLATE(STR,".,;-[]()<>~:'""","                ")
        NEW TMGINDENT,TEMPARR DO SPLIT2AR^TMGSTUT2(STR," ",.TEMPARR)
        NEW NOW SET NOW=$$NOW^XLFDT
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(TEMPARR(IDX)) QUIT:(+IDX'>0)  DO
        . NEW WORD SET WORD=$GET(TEMPARR(IDX))
        . IF $LENGTH(WORD)<3 QUIT
        . NEW FMDT IF $$ISDATE(WORD,.FMDT)=0 QUIT
        . IF FMDT>NOW QUIT  ;"ignore future dates.
        . SET OUT(FMDT,WORD)=""
        QUIT
        ;
ISDATE(WORD,OUTFMDT) ;
        ;"Determine if word is a date, and if so, turn into FM date
        NEW RESULT SET RESULT=0
        ;"//kt note: line below got corrupted.  I fixed it, I think, 2/25/15
        SET WORD=$TRANSLATE(WORD,"~),;?.""","")
        IF $EXTRACT(WORD,1,2)="on" SET WORD=$PIECE(WORD,"on",2)
        IF +WORD'>0 GOTO IDDN
        NEW %DT,X,Y 
        SET %DT="P",X=WORD
        IF WORD["/",$LENGTH(WORD)=2 SET %DT=%DT_"M"  ;"just month/year
        DO ^%DT
        IF Y'>0 GOTO IDDN        
        SET OUTFMDT=Y,RESULT=1        
IDDN    QUIT RESULT        
        ;        
PARSEBP(STR,SYS,DIA) ;"Parse blood pressure
        ;"Input: STR -- String, as returned from $$DOVITALS^TIULO()
        ;"       SYS -- PASS BY REFERENCE, AN OUTPARAMETER
        ;"       DIA -- PASS BY REFERENCE, AN OUTPARAMETER
        SET STR=$PIECE($GET(STR)," ",1)
        SET SYS=+$PIECE(STR,"/",1)
        SET DIA=+$PIECE(STR,"/",2)
        QUIT        
        ;
GETFOBT(ADFN) ;
        ;"Purpose: Get the last FOBT date, testing all four FOBT health factors
        NEW RESULTARR,DATE,TMGRESULT
        SET DATE=$$GETHFDT^TMGPXRU1(ADFN,"TMG FECAL OCCULT BLOOD (FOBT)  NEGATIVE")
        SET RESULTARR(DATE)=""
        SET DATE=$$GETHFDT^TMGPXRU1(ADFN,"TMG FECAL OCCULT BLOOD (FOBT)  POSITIVE")
        SET RESULTARR(DATE)=""
        SET DATE=$$GETHFDT^TMGPXRU1(ADFN,"TMG FECAL IMM OCCULT BLOOD (iFOBT)  POS")
        SET RESULTARR(DATE)=""
        SET DATE=$$GETHFDT^TMGPXRU1(ADFN,"TMG FECAL IMM OCCULT BLOOD (iFOBT)  NEG")
        SET RESULTARR(DATE)=""
        SET TMGRESULT=+$ORDER(RESULTARR(9999999),-1)
        QUIT TMGRESULT
