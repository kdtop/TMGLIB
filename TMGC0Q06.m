TMGC0Q06 ;TMG/kst/TMG customization of C0Q code ;8/17/12
         ;;1.0;TMG-LIB;**1**;8/17/12
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
 ;"MEANFLOJ(DFN) ;" MEANINGFUL USE TEXT OBJECT
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"MEANFLUS(DFN) -- Return text showing missing items for meaningful use.
 ;"GETVISTS(DFN,SDTE,EDTE,ARRAY) -- create a list of visit dates for patient, in date range
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"=======================================================================
 ;"=======================================================================
 ;
MEANFLOJ(DFN) ;" MEANINGFUL USE TEXT OBJECT
        NEW STR SET STR=$$MEANFLUS(DFN)
        IF STR="" GOTO MNOJDN
        NEW SPACES SET SPACES="     "
        NEW SPACES2 SET SPACES2=SPACES_"  "
        NEW ARRAY
        DO SPLIT2AR^TMGSTUT2(STR,$CHAR(13,10),.ARRAY,2)
        NEW I SET I=0
        FOR  SET I=$ORDER(ARRAY(I)) QUIT:+I'>0  DO
        . NEW S,S2 SET S=$GET(ARRAY(I))
        . IF S["{Tobacco Use test}" DO  QUIT
        . . SET S2="* TOBACCO: We discussed the harmful effects of tobacco.  We discussed strategies, and patient asked to please STOP."
        . . SET I=I+0.5 SET ARRAY(I)=S2
        . IF S["{Obesity test}" DO  QUIT
        . . SET S2="* Weight: Encouraged to stay as active as possible, eat healthfully, and get adequate sleep. Will check again next visit."
        . . SET I=I+0.5 SET ARRAY(I)=S2
        SET ARRAY(1)=SPACES_"**MISSING ISSUES NEEDED FOR MEANINGFUL USE (ERASE)**"
        NEW I SET I=1
        FOR  SET I=$ORDER(ARRAY(I)) QUIT:+I'>0  DO
        . SET ARRAY(I)=SPACES2_$GET(ARRAY(I))
        . SET ARRAY(I-0.01)=SPACES2
        SET STR="",I=0
        FOR  SET I=$ORDER(ARRAY(I)) QUIT:+I'>0  DO
        . SET STR=STR_$GET(ARRAY(I))_$CHAR(13,10)
MNOJDN  QUIT STR
        ;
MEANFLUS(DFN)  ;
        ;"Purpose: Return text showing missing items for meaningful use.
        ;"Input: DFN -- Patient IEN
        ;"Result: Returns string to display, describing problems with patient.
        NEW RESULT SET RESULT=""
        NEW PERIODKEY SET PERIODKEY="LAUGHLIN-MU13"  ;"Hard coded for now
        NEW C0QPARM,C0QLIST
        DO INIT^TMGC0Q01(PERIODKEY,"C0QPARM","EP") ;" INITIALIZE PARAMETERS
        NEW DTE,EDTE
        NEW ZI SET ZI=$O(C0QPARM("")) ;"Just use 1st param.  All should have same dates.
        NEW SDTE,EDTD
        SET SDTE=C0QPARM(ZI,"EPBeginDate") ;" beginning of measurement period
        SET EDTE=C0QPARM(ZI,"EPEndDate") ;" end of measurement period
        SET EDTE=$$NOW^XLFDT()
        SET SDTE=EDTE-10000
        NEW PROV SET PROV=DUZ
        NEW VISITS DO GETVISTS(DFN,SDTE,EDTE,.VISITS)
        NEW SUCCESS,WHY
        NEW ZYR SET ZYR="XX" ;"some random value
        DO TESTPT^TMGC0Q02(DFN,PROV,SDTE,EDTE,.C0QLIST,.VISITS,.SUCCESS,.WHY)
        IF SUCCESS=1 GOTO MUDN
        NEW POS SET POS=0
        FOR  DO  QUIT:POS=0
        . SET POS=$FIND(WHY,"{",POS)
        . QUIT:(POS=2)!(POS=0)
        . SET PARTA=$EXTRACT(WHY,1,POS-2)
        . SET PARTB=$EXTRACT(WHY,POS-1,$LENGTH(WHY))
        . SET WHY=PARTA_$CHAR(13)_$CHAR(10)_PARTB
        . SET POS=POS+2
MUDN    QUIT WHY
        ;
GETVISTS(DFN,SDTE,EDTE,ARRAY) ;
        ;"Purpose: To create a list of visit dates for patient, in date range
        ;"Input: DFN -- Patient IEN
        ;"       SDTE -- Starting date, FM format
        ;"       EDTE -- Ending date, FM format
        ;"       ARRAY -- PASS BY REFERENCE, AN OUT PARAMETER.  Format:
        ;"                    ARRAY(FMDATE)=""
        ;"Result: none
        NEW RECIEN SET RECIEN=""
        FOR  SET RECIEN=$ORDER(^AUPNVSIT("C",DFN,RECIEN)) QUIT:(+RECIEN'>0)  DO
        . NEW ONEDT SET ONEDT=$PIECE($GET(^AUPNVSIT(RECIEN,0)),"^",1)
        . IF ONEDT<SDTE QUIT
        . IF ONEDT>EDTE QUIT
        . SET ARRAY(ONEDT)=""
        QUIT
        ;
