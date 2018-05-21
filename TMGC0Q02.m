TMGC0Q02 ;TMG/kst/TMG customization of C0Q code ;10/28/12, 2/2/14
         ;;1.0;TMG-LIB;**1**;8/8/12
 ;
 ;"TMG C0Q FUNCTIONS
 ;"NOTE: code started as code written by George Lilly.
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
 ;"TESTPT(DFN,PROV,SDTE,EDTE,C0QLIST,VISITS,RESULT,WHY)  ;"Run various tests on one patient.
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"NOTES FROM PRIOR TESTING CODE: (Actual tests are now in TMGC0QT*.m
 ;"----------------------------
 ;//Below are numbered from the list of 15 meaningful use "core"(MUC) measures
 ;"----------------------------
 ;"D TMGRX^TMGC0Q03(DFN,PROV,SDTE,EDTE,.C0QLIST,.TEMPRSLT,.TEMPWHY) "//#MUC-1, MUC-2, MUC-4, MUC-5
 ;"D PROBLEM(DFN,PROV,.C0QLIST,.TEMPRSLT,.TEMPWHY)   "//#MUC-3
 ;"D ALLERGY(DFN,PROV,.C0QLIST,.TEMPRSLT,.TEMPWHY)   "//#MUC-6
 ;"D DEMO(DFN,PROV,.C0QLIST,.TEMPRSLT,.TEMPWHY)      "//#MUC-7
 ;"D VITALS(DFN,PROV,SDTE,EDTE,.C0QLIST,.TEMPRSLT,.TEMPWHY)   "//#MUC-8
 ;"D SMOKING^TMGC0Q03(DFN,PROV,SDTE,EDTE,.C0QLIST,.TEMPRSLT,.TEMPWHY)   "//#MUC-9
 ;//#MUC-10 -- report QM to CMS (not on per-patient basis)
 ;//#MUC-11 -- global Y/N IF decision rule used (not on per-patient basis)
 ;//#MUC-12 -- providing electronic copy of records (not on per-patient basis)
 ;"D CLINSUM(DFN,PROV,SDTE,EDTE,.C0QLIST,.TEMPRSLT,.TEMPWHY) "//#MUC-13
 ;//#MUC-14 -- global Y/N IF clinic exchange of information with another entity (not on per-patient basis)
 ;//#MUC-15 -- global Y/N IF protect electronic health information (not on per-patient basis)
 ;"=============================
 ;//Below are numbered from the list of 10 meaningful use "menu" (MUM) measures (5 required)
 ;//#1 -- MUM-1 -- will get via HasERX list
 ;//#2 -- MUM-3 -- global Y/N IF generated list of patiens with specific condition for quality improvement (not on per-patient basis)
 ;//#3 -- MUM-4 -- Done through telephone reminders 
 ;"DO PTREM(DFN,PROV,SDTE,EDTE,.C0QLIST,.TEMPRSLT,.TEMPWHY)  ;"REMINDERS SENT TO PATIENT 
 ;//#4 -- MUM-7 -- will get via HasMedRecon list
 ;//#5 -- MUM-8 -- Will get through consult referal note-title and addendum when sent. 
 ;"DO CONTCARE^TMGC0Q03(DFN,PROV,SDTE,EDTE,.C0QLIST,.VISITS,.TEMPRSLT,.TEMPWHY) 
 ;"=============================
 ;//Below are numbered from the list of 3 core clinical quality measures (CCQM) (3 required)
 ;//CCQM-1 -- HTN: 
 ;"DO HTN^TMGC0QT2(DFN,PROV,SDTE,EDTE,.C0QLIST,.VISITS,.TEMPRSLT,.TEMPWHY) 
 ;//CCQM-2 -- Tobacco prevention
 ;//   Will get through: HasMokingStatus/Over17 and IsTobaccoUser/HasTobaccoIntervention
 ;//CCQM-2 -- Obesity: 
 ;"DO OBESITY^TMGC0Q03(DFN,PROV,SDTE,EDTE,.C0QLIST,.VISITS,.TEMPRSLT,.TEMPWHY) "//CCQM-3 -- Weight intervention:
 ;"=============================
 ;//Below are numbered from the list of 3 of 38 optional clinical quality measures (CCQM) (3 required)
 ;//CQM-1 -- HgbA1c Poor Control
 ;//CQM-38 -- HgbA1c Control < 8.0%
 ;"DO DM^TMGC0Q03(DFN,PROV,SDTE,EDTE,.C0QLIST,.VISITS,.TEMPRSLT,.TEMPWHY) "//CQM-1 & CQM-38
 ;//CQM-6 -- Pneumococal vaccination
 ;"DO PNEUMO^TMGC0Q03(DFN,PROV,SDTE,EDTE,.C0QLIST,.VISITS,.TEMPRSLT,.TEMPWHY) "//CQM-6
 ;"----------------------------
 ; 
TESTPT(DFN,PROV,SDTE,EDTE,C0QLIST,VISITS,TMGC0QRESULT,TMGC0QWHY)  ;"Run various tests on one patient.
        ;"Purpose: to run the various tests for a given patient
        ;"NOTE:    Tests are defined in file C0Q DATA TEST (#1130580001.501)
        ;"Input: DFN -- the patient IEN (in PATIENT file)
        ;"       PROV -- the provider IEN (in NEW PERSON file)
        ;"       SDTE -- Starting date (FM format) of range to test patient in
        ;"       EDTE -- Ending date (FM format) of range to test patient in
        ;"       C0QLIST -- AN OUT PARAMETER.  PASS BY REFERENCE.
        ;"       VISITS -- PASS BY REFERENCE.  A list of FM dates for visits in date range.
        ;"       RESULT -- OPTIONAL.  PASS BY REFERENCE, AN OUT PARAMETER
        ;"                returns 1 IF successful, 0 IF failure
        ;"       WHY -- OPTIONAL.  PASS BY REFERENCE.  AN OUT PARAMETER
        ;"               Returns string explaining internal logic
        ;"NOTE: Uses ZYR,TMGOVTITLES in global scope
        ;"Results: none.
        SET TMGC0QRESULT=1
        NEW TEMPRSLT,TEMPWHY
        SET TMGC0QWHY="",TMGC0QRESULT=1
        NEW TMGC0QNAME SET TMGC0QNAME=""
        FOR  SET TMGC0QNAME=$ORDER(^C0Q(501,"B",TMGC0QNAME)) QUIT:(TMGC0QNAME="")  DO
        . NEW TMGC0QIEN SET TMGC0QIEN=0
        . FOR  SET TMGC0QIEN=$ORDER(^C0Q(501,"B",TMGC0QNAME,TMGC0QIEN)) QUIT:(+TMGC0QIEN'>0)  DO
        . . NEW ZN SET ZN=$GET(^C0Q(501,TMGC0QIEN,0)) QUIT:ZN=""
        . . IF $PIECE(ZN,"^",4)="I" QUIT
        . . NEW TAG SET TAG=$PIECE(ZN,"^",2)
        . . NEW ROUTINE SET ROUTINE=$PIECE(ZN,"^",3) QUIT:ROUTINE=""
        . . NEW TEMPRSLT SET TEMPRSLT=0
        . . NEW TEMPWHY SET TEMPWHY=""
        . . NEW CODE SET CODE="DO "_TAG_"^"_ROUTINE_"(DFN,PROV,SDTE,EDTE,.VISITS,.C0QLIST,.TEMPRSLT,.TEMPWHY,ZYR)"
        . . XECUTE CODE
        . . IF 'TEMPRSLT SET TMGC0QWHY=TMGC0QWHY_TEMPWHY
        . . SET TMGC0QRESULT=TMGC0QRESULT&TEMPRSLT
        QUIT
        ;
