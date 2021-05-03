TMGC0QT1 ;TMG/kst/TMG C0Q Data-test code ;10/28/12, 2/2/14, 3/24/21
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
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"DEMO(TMGDFN,PROV,SDTE,EDTE,VISITS,C0QLIST,RESULT,WHY,ZYR) ; patient demographics
 ;"     Creates C0Q PATIENT LIST's"
 ;"       ZYR_"HasDemographics"
 ;"       ZYR_"FailedDemographics"
 ;"PROBLEM(TMGDFN,PROV,SDTE,EDTE,VISITS,C0QLIST,RESULT,WHY,ZYR) ;PATIENT PROBLEMS
 ;"     Creates C0Q PATIENT LIST's"
 ;"       ZYR_"NoProblem"
 ;"       ZYR_"HasProblem"
 ;"ALLERGY(TMGDFN,PROV,SDTE,EDTE,VISITS,C0QLIST,RESULT,WHY,ZYR) ;ALLERGY LIST
 ;"     Creates C0Q PATIENT LIST's"
 ;"       ZYR_"NoAllergy"
 ;"       ZYR_"HasAllergy"
 ;"VITALS(TMGDFN,PROV,SDTE,EDTE,VISITS,C0QLIST,RESULT,WHY,ZYR) ;
 ;"     Creates C0Q PATIENT LIST's"
 ;"       ZYR_"HasVitalSigns"
 ;"       ZYR_"NoVitalSigns"
 ;"CLINSUM(TMGDFN,PROV,SDTE,EDTE,VISITS,C0QLIST,RESULT,WHY,ZYR) ;
 ;"     Creates C0Q PATIENT LIST's"
 ;"       ZYR_"OfficeVisitEvent"
 ;"       ZYR_"SummaryGivenFor_OfficeVisitEvent"
 ;"PTREM(TMGDFN,PROV,SDTE,EDTE,VISITS,C0QLIST,RESULT,WHY,ZYR) ;REMINDERS SENT TO PATIENT
 ;"     Creates C0Q PATIENT LIST's"
 ;"       ZYR_"Reminder_WasSentToPatient"
 ;"       ZYR_"Reminder_NotSentToPatient"
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"ISOFFVST(IEN8925)  ;determine IF TIU note represents an office visit
 ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"=======================================================================
 ;"   XLFDT, DIC, DIQ, ORQQPL, ORQQAL, ORQQVI, TMGTIUOJ
 ;"=======================================================================
 ;"NOTE: The following routines used the following parameters
         ;"Input: TMGDFN -- the patient IEN (in PATIENT file)
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
DEMO(TMGDFN,PROV,SDTE,EDTE,VISITS,C0QLIST,RESULT,WHY,ZYR)  ; patient demographics
        SET RESULT=0 ;"default of failure
        SET WHY="{Demographics test}: "
        K PTDOB
        N PTNAME,PTSEX,PTHRN,PTRLANG,PTLANG,RACE,RACEDSC,ETHN,ETHNDSC,RB
        S PTNAME=$P(^DPT(TMGDFN,0),U) ;patient name
        S PTDOB=$$FMTE^XLFDT($P($G(^DPT(TMGDFN,0)),U,3)) ;date of birth
        S PTSEX=$P($G(^DPT(TMGDFN,0)),U,2) ;patient sex
        D PID^VADPT ;VADPT call to grab PISD based on PT Eligibility
        S PTHRN=$P($G(VA("PID")),U) ;health record number
        S PTRLANG=$P($G(^DPT(TMGDFN,256000)),U) ;ptr to language file
        I $G(PTRLANG)'="" S PTLANG=$P(^DI(.85,PTRLANG,0),U) ;PLS extrnl
        S RACE=""
        F  D  Q:RACE=""  ;"field #2 (RACE INFORMATION)
        . S RACE=$O(^DPT(TMGDFN,.02,"B",RACE)) ;race code IEN
        . Q:'RACE
        . S RACEDSC=$P($G(^DIC(10,RACE,0)),U) ;race description
        S ETHN=""
        F  D  Q:ETHN=""  ;"field #6 (ETHNICITY INFORMATION)
        . S ETHN=$O(^DPT(TMGDFN,.06,"B",ETHN)) ;ethnicity IEN
        . Q:'ETHN
        . S ETHNDSC=$P($G(^DIC(10.2,ETHN,0)),U) ;ethnincity description
        S RB=$P($G(^DPT(TMGDFN,.101)),U) ;room and bed
        N DEMOYN S DEMOYN=1
        I $G(PTSEX)="" DO
        . S DEMOYN=0
        . SET WHY=WHY_"Gender missing. "
        I $G(PTDOB)="" DO
        . S DEMOYN=0
        . SET WHY=WHY_"Date of birth missing. "
        I $G(PTHRN)="" DO
        . S DEMOYN=0
        . SET WHY=WHY_"Health record nmber (HRN) missing. "
        I $G(PTLANG)="" DO
        . S DEMOYN=0
        . SET WHY=WHY_"Preferred language notation missing. "
        I $G(RACEDSC)="" DO
        . S DEMOYN=0
        . SET WHY=WHY_"Race description missing. "
        I $G(ETHNDSC)="" DO
        . S DEMOYN=0
        . SET WHY=WHY_"Ethnicity missing. "
        I DEMOYN DO
        . S C0QLIST(PROV,ZYR_"HasDemographics",TMGDFN)=""
        . SET RESULT=1
        . SET WHY=WHY_"OK. "
        E  DO
        . S C0QLIST(PROV,ZYR_"FailedDemographics",TMGDFN)=""
        Q
        ; 
PROBLEM(TMGDFN,PROV,SDTE,EDTE,VISITS,C0QLIST,RESULT,WHY,ZYR) ;" PATIENT PROBLEMS
        SET RESULT=0 ;"default of failure
        SET WHY="{Problem list test}: "
        D LIST^ORQQPL(.PROBL,TMGDFN,"A")
        NEW PBCNT,PBDESC  ;"//kt added
        S PBCNT=""
        F  S PBCNT=$O(PROBL(PBCNT)) Q:PBCNT=""  D
        . S PBDESC=$P(PROBL(PBCNT),U,2) ;problem description
        I PBDESC["No problems found" DO  ;"//kt added block.  Check for Text Table 'PROBLEM LIST'
        . NEW S SET S=$$GETTABLX^TMGTIUO6(TMGDFN,"PROBLEM LIST")
        . IF S'="" SET PBDESC=S
        I PBDESC["No problems found" DO
        . S C0QLIST(PROV,ZYR_"NoProblem",TMGDFN)=""
        . SET WHY=WHY_"No problems found in Problem List. "
        E  DO
        . S C0QLIST(PROV,ZYR_"HasProblem",TMGDFN)=""
        . SET WHY=WHY_"OK. "
        . SET RESULT=1
        K PROBL
        Q
        ;
ALLERGY(TMGDFN,PROV,SDTE,EDTE,VISITS,C0QLIST,RESULT,WHY,ZYR) ; ALLERGY LIST
        SET RESULT=0 ;"default of failure
        SET WHY="{Patient Allergies test}: "
        N ALRGYL,ALDESC,ALCNT
        D LIST^ORQQAL(.ALRGYL,TMGDFN)
        S ALCNT=""
        F  S ALCNT=$O(ALRGYL(ALCNT)) Q:ALCNT=""  D
        . S ALDESC=$P(ALRGYL(ALCNT),U,2) ;allergy description
        ;"FYI: 'No Allergy Assessment' IF never asked
        ;"FYI: 'No Know Allergies' IF HAS been assessed
        I ALDESC["No Allergy" DO
        . S C0QLIST(PROV,ZYR_"NoAllergy",TMGDFN)=""
        . SET WHY=WHY_$$NURSEPRE^TMGC0QT1()_"NEEDS ALLERGY ASSESSMENT] "
        E  DO
        . S C0QLIST(PROV,ZYR_"HasAllergy",TMGDFN)=""
        . SET WHY=WHY_"OK. "
        . SET RESULT=1
        Q
        ;
VITALS(TMGDFN,PROV,SDTE,EDTE,VISITS,C0QLIST,RESULT,WHY,ZYR)   ;
        ;"note: Consider using $$GETVITLS^TMGGMRV1 in the future...
        SET RESULT=0 ;"default of failure
        SET WHY="{Vitals test}: "
        D VITALS^ORQQVI(.VITRSLT,TMGDFN,SDTE,EDTE) ;" CALL FAST VITALS
        I $D(VITRSLT),$GET(VITRSLT(1))'["No vitals found." SET RESULT=1
        IF RESULT DO
        . S C0QLIST(PROV,ZYR_"HasVitalSigns",TMGDFN)=""
        . SET WHY=WHY_"Vitals were found. "
        ELSE  DO
        . S C0QLIST(PROV,ZYR_"NoVitalSigns",TMGDFN)=""
        . SET WHY=WHY_"No vitals found during date range. "
        Q
        ;
CLINSUM(TMGDFN,PROV,SDTE,EDTE,VISITS,C0QLIST,RESULT,WHY,ZYR) ;
        ;"Note: C0QLIST(PROV,ZYR_"OfficeVisitEvent",IEN8925)=""  <-- NOTE: not a DFN
        ;"      C0QLIST(PROV,ZYR_"SummaryGivenFor_OfficeVisitEvent",IEN8925)=""  <-- NOTE: not a DFN
        ;"      C0QLIST("FILE","OfficeVisitEvent")=8925 <-- Indicates that IENs are from file 8925
        ;"      C0QLIST("FILE","SummaryGivenFor_OfficeVisitEvent")=8925 <-- Indicates that IENs are from file 8925
        ;"NOTE: the usual way that lists are created is to have lists **OF PATIENTS**
        ;"       I will overload this process to put 'visits' into this slot.
        ;"NOTE: Uses TMGOVTITLES in global scope        
        SET RESULT=0 ;"default of failure
        SET WHY="{Test for clinic summary to patient}: "
        NEW HASCC,TEMPS
        NEW ONEFOUND SET ONEFOUND=0
        NEW EVENTS
        NEW PROVINIT SET PROVINIT=$PIECE($GET(^VA(200,+PROV,0)),"^",2)  
        IF PROVINIT="" SET PROVINIT=$PIECE($GET(^VA(200,+PROV,0)),"^",1)         
        SET C0QLIST("FILE",PROVINIT_"-"_ZYR_"OfficeVisitEvent")=8925 ;"<-- Indicates that IENs are from file 8925
        SET C0QLIST("FILE",PROVINIT_"-"_ZYR_"SummaryGivenFor_OfficeVisitEvent")=8925 ;"<-- Indicates that IENs are from file 8925
        NEW IEN SET IEN=""
        FOR  SET IEN=$ORDER(^TIU(8925,"C",TMGDFN,IEN),-1) QUIT:(IEN="")  DO
        . NEW AUTHOR SET AUTHOR=+$PIECE($GET(^TIU(8925,IEN,12)),"^",2)
        . IF AUTHOR'=PROV QUIT
        . NEW DT SET DT=+$PIECE($GET(^TIU(8925,IEN,0)),"^",7)
        . SET DT=DT\1 ;"drop time element
        . IF (DT<SDTE)!(DT>EDTE) QUIT
        . IF $$ISOFFVST(IEN)=0 QUIT
        . SET HASCC=0
        . NEW LN SET LN=""
        . FOR  SET LN=+$ORDER(^TIU(8925,IEN,"TEXT",LN),-1) QUIT:(LN'>0)!(HASCC=1)  DO
        . . NEW S SET S=$GET(^TIU(8925,IEN,"TEXT",LN,0)) QUIT:S=""
        . . IF S["cc:" SET HASCC=1
        . ;"Handle multiple notes for 1 visit event. E.g. IF 2 notes on date found, and only 1 has "cc", then should count as given 
        . IF ($DATA(EVENTS(DT))=0)!($DATA(EVENTS(DT,1))=0) KILL EVENTS(DT) SET EVENTS(DT,HASCC)=IEN 
        NEW DT SET DT=""
        FOR  SET DT=$ORDER(EVENTS(DT)) QUIT:DT=""  DO
        . SET HASCC=$ORDER(EVENTS(DT,""))
        . SET IEN=$GET(EVENTS(DT,HASCC))
        . SET C0QLIST(PROV,ZYR_"OfficeVisitEvent",IEN)=""  ;"Overloading process to track events (IEN) instead of DFN
        . IF HASCC DO
        . . SET C0QLIST(PROV,ZYR_"SummaryGivenFor_OfficeVisitEvent",IEN)=""  ;"Overloading process to track events (IEN) instead of DFN
        . . SET ONEFOUND=1
        . . SET WHY=WHY_"cc: found in note #"_IEN_". "
        IF ONEFOUND=0 DO
        . SET WHY=WHY_"No 'cc:' found in any office visit type notes or addenda written by YOU during date range. "
        ELSE  SET RESULT=1
        QUIT
        ;
ISOFFVST(IEN8925)  ;
        ;"Purpose: to determine IF TIU note (or it's parent IF note is an addendum)
        ;"         represents an office visit or not
        ;"NOTE: Uses TMGOVTITLES in global scope
        ;"Input: IEN8925 -- IEN IN 8925.  NOTE: DON'T PASS BY REFERENCE (MAY BE MODIFIED)
        ;"RESULT: 1 IF is office note, 0 otherwise
        NEW PARENT SET PARENT=+$PIECE(^TIU(8925,IEN8925,0),"^",6)
        IF PARENT>0 SET IEN8925=PARENT
        NEW PTITLE SET PTITLE=+$PIECE(^TIU(8925,IEN8925,0),"^",1)
        SET RESULT=$GET(TMGOVTITLES(PTITLE))
        IF RESULT'="" GOTO IOVDN
        NEW NAME SET NAME=$PIECE($GET(^TIU(8925.1,PTITLE,0)),"^",1)
        IF NAME["OFFICE VISIT" SET RESULT=1 GOTO IOVSV
        IF NAME["COMPLETE PHYSICAL EXAM" SET RESULT=1 GOTO IOVSV
        IF NAME["ACUTE MEDICAL ISSUE VISIT" SET RESULT=1 GOTO IOVSV
        IF NAME["SICK VISIT" SET RESULT=1 GOTO IOVSV
        IF NAME["NOTE CLEANUP" SET RESULT=1 GOTO IOVSV
        IF NAME["ANNUAL WELLNESS VISIT" SET RESULT=1 GOTO IOVSV
        NEW HILITE SET HILITE=$P($G(^TIU(8925.1,PTITLE,"TMGH")),"^",1)
        IF HILITE="Y" SET RESULT=1 GOTO IOVSV
        SET RESULT=0
IOVSV   SET TMGOVTITLES(PTITLE)=RESULT
IOVDN   QUIT RESULT
        ;
PTREM(TMGDFN,PROV,SDTE,EDTE,VISITS,C0QLIST,RESULT,WHY,ZYR)  ;"REMINDERS SENT TO PATIENT 
        SET RESULT=0 ;"default of failure
        SET WHY="{Reminders sent to patient test}: "
        NEW AGE SET AGE=$$GET1^DIQ(2,TMGDFN,"AGE")
        IF (AGE>64.99)!(AGE<6) DO
        . SET C0QLIST(PROV,ZYR_"Reminder_WasNeededForPatient",TMGDFN)=""
        ELSE  DO  GOTO PTRDN
        . SET C0QLIST(PROV,ZYR_"Reminder_WasNOTNeededForPatient",TMGDFN)=""  
        . SET WHY=WHY_"OK"
        . SET RESULT=1
        NEW FOUND SET FOUND=0
        IF $DATA(^TMG(22716,TMGDFN))'>0 GOTO PTR2
        NEW DT SET DT=SDTE\1
        FOR  SET DT=+$ORDER(^TMG(22716,TMGDFN,.01,"B",DT)) QUIT:(DT'>0)!(DT>EDTE)!FOUND  DO
        . NEW IEN SET IEN=+$ORDER(^TMG(22716,TMGDFN,.01,"B",DT,0))
        . NEW ZN SET ZN=$GET(^TMG(22716,TMGDFN,.01,IEN,0))
        . IF $PIECE(ZN,"^",2)="Y" SET FOUND=1
        SET RESULT=FOUND                
PTR2    IF FOUND DO
        . S C0QLIST(PROV,ZYR_"Reminder_WasSentToPatient",TMGDFN)=""
        . SET WHY=WHY_"Evidence was found of a reminder sent to patient. "
        ELSE  DO
        . S C0QLIST(PROV,ZYR_"Reminder_NotSentToPatient",TMGDFN)=""
        . SET WHY=WHY_"No evidence was found of a reminder sent to patient. "
PTRDN   QUIT
        ;
NURSEPRE() ;
        NEW TMGRESULT
        SET TMGRESULT="[<FONT style=""BACKGROUND-COLOR:#ff0000"">***NURSE***</FONT> "
        QUIT TMGRESULT
        ;