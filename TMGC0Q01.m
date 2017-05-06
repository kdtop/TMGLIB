TMGC0Q01 ;TMG/kst/TMG customization of C0Q code ;10/24/12, 2/2/14
         ;;1.0;TMG-LIB;**1**;7/15/12
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
 ;"DOEP        --EP COMPUTATIONS
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"TMGPVFN() --RETURN FILE NUM FOR PROVIDERS SUB-SUBFILE
 ;"LOCPAT(C0QLIST,PREFIX,LOC,DTE,EDTE,PROVS) -- Retrieve active outpatients
 ;"PROV4VST(VSTIEN,DFN,ARRAY) --GET PROVIDER FOR VISIT
 ;"PROVOK(VSTIEN,PROVIEN,DFN) --CHECK IF PROVIDER IS OK (Is provider associated with visit?)
 ;"NOTEOK(IEN8925) -- Determine IF note type is OK, or should be ignored.
 ;"INIT(ZMU,ZARY,ZTYP)-- INITIALIZE THE PARAMETERS FOR BUILDING PATIENT LISTS
 ;"GETKEY()  --Get the MEASUREMENT PERIOD KEY (field .02) in file C0Q PARAMETER to use for updating.
 ;"FILE(C0QLIST)  -- FILE THE PATIENT LISTS TO C0Q PATIENT LIST
 ;"VIEWLIST -- VIEW A C0Q PATIENT LIST
 ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"=======================================================================
 ;"   C0QMU12, TMGSTUT2, TMGUSRI2, TMGMISC2, TMGC0Q*
 ;"=======================================================================
DOEP    ;" DO EP COMPUTATIONS.  This is an entry point
        ;"NOTE: May use variable TMGLIMITPROV with global scope.  If found,
        ;"      then only providers entries with TMGLIMITPROV(PROVIEN)=1 are processed.
        ;"NOTE: May use variable TMGSKIPC0QFILE with global scope, to skip filing.
        ;"NOTE: May use variables CUSTSDT, CUSTEDT as custom start and end
        ;"      dates, which will override dates specified in config file.
        ;"NOTE: Uses optional TMGVERBOSE.  If 0 then nothing written to screen.
        ;"      (currently doesn't make Fileman filing process quiet)
        NEW PERIODKEY SET PERIODKEY=$$GETKEY()  ;"GET MEASUREMENT PERIOD KEY to use for update
L1      ;"Alternative entry point
        ;"NOTE: IF L1 entry label is used, then PERIODKEY variable must be 
        ;"      provided in global scope.
        IF PERIODKEY="" GOTO DEPDN 
        NEW ZYR SET ZYR=PERIODKEY_"-"
        NEW C0QPARM,C0QCLNC
        NEW TMGPROVNAME,TMGPROVS,TMGPROVINITIALS
        ;"NOTE: The following line populates C0QPARM with the parameters for ALL
        ;"      C0Q PARAMETER file records that have matching period key (e.g. MU12)
        ;"      Each with separate node (e.g. C0QPARAM(#))
        DO INIT(PERIODKEY,"C0QPARM","EP") ;" INITIALIZE PARAMETERS
        KILL C0QLIST ;" CLEAR THE LIST
        NEW DTE,EDTE
        NEW ZI SET ZI=""
        FOR  SET ZI=$O(C0QPARM(ZI)) Q:ZI=""  DO  ;" FOR EACH C0Q PARAMETER ENTRY
        . IF (+$GET(CUSTSDT)>0)&($GET(CUSTEDT)>0) DO
        . . SET DTE=+CUSTSDT
        . . SET EDTE=+CUSTEDT
        . ELSE  DO
        . . SET DTE=C0QPARM(ZI,"EPBeginDate") ; beginning of measurement period
        . . SET EDTE=C0QPARM(ZI,"EPEndDate") ; end of measurement period -- tbd use this
        . SET C0QCLNC=C0QPARM(ZI,"CLINICS",1,1) ; only one clinic for now
        . MERGE TMGPROVS=C0QPARM(ZI,"CLINICS","PROVIDERS")
        . SET PRE=ZYR_"EP-"_C0QCLNC_"-"
        . DO LOCPAT(.C0QLIST,PRE,C0QCLNC,DTE,EDTE,.TMGPROVS) ;" GET THE PATIENTS  ;"//kt added params
        . NEW PROV SET PROV=""
        . FOR  SET PROV=$ORDER(C0QLIST(PROV)) QUIT:PROV=""  DO
        . . MERGE C0QLIST(PROV,ZYR_"EP-ALL-PATIENTS")=C0QLIST(PROV,PRE_"Patient")
        . ;"//kt original --> M C0QLIST(ZYR_"EP-ALL-PATIENTS")=C0QLIST(PRE_"Patient")
        S DFN=""
        S ZYR=ZYR_"EP-"
        NEW TMGC0QDEBUG SET TMGC0QDEBUG=0  ;"can change during step through.
        IF TMGC0QDEBUG=1 DO  GOTO D2
        . KILL C0QLIST MERGE C0QLIST=^TMG("TMP","TMGC0Q01","C0QLIST")
        NEW TMGOVTITLES ;"Used by global scope
        NEW PROV SET PROV=""
        FOR  SET PROV=$ORDER(C0QLIST(PROV)) QUIT:PROV=""  DO
        . IF $DATA(TMGLIMITPROV),($GET(TMGLIMITPROV(PROV))'=1) QUIT
        . NEW PROVNAME SET PROVNAME=$PIECE($GET(^VA(200,+PROV,0)),"^",1)
        . NEW NUMPAT SET NUMPAT=$$LISTCT^TMGMISC2($NAME(C0QLIST(PROV,ZYR_"ALL-PATIENTS")))
        . IF $GET(TMGVERBOSE)'=0 WRITE !,"Processing ",NUMPAT," patients for provider: ",PROVNAME,! ;"//kt added
        . NEW STIME S STIME=$H
        . NEW PATCT SET PATCT=0
        . FOR  S DFN=$O(C0QLIST(PROV,ZYR_"ALL-PATIENTS",DFN)) Q:DFN=""  D  ; EACH PATIENT
        . . SET PATCT=PATCT+1
        . . IF (PATCT#10=1),($GET(TMGVERBOSE)'=0)  DO
        . . . NEW PATNAME SET PATNAME=$$LJ^XLFSTR($PIECE($GET(^DPT(DFN,0)),"^",1),22)
        . . . DO PROGBAR^TMGUSRI2(PATCT,"Checking "_PATNAME,1,NUMPAT,60,STIME)
        . . ;"NOTE: If more than one measurement period is specified in C0QPARAM array above,
        . . ;"      then the DTE and EDTE will NOT BE CORRECT.  Would have to
        . . ;"      somehow separate the lists by date period.
        . . NEW VISITS MERGE VISITS=C0QLIST(PROV,PRE_"Patient;Date",DFN)
        . . D TESTPT^TMGC0Q02(DFN,PROV,DTE,EDTE,.C0QLIST,.VISITS)
        . IF $GET(TMGVERBOSE)'=0 DO PROGBAR^TMGUSRI2(100,"Done",1,100,60,STIME)
D2      KILL TMGNOTELIST
        KILL ^TMG("TMP","TMGC0Q01","C0QLIST")
        MERGE ^TMG("TMP","TMGC0Q01","C0QLIST")=C0QLIST
        IF $DATA(TMGSKIPC0QFILE) GOTO DEPDN
        D FILE(.C0QLIST) ;"FILE THE PATIENT LISTS
        ;
        ; Now process eRx MU measures for these patients
        ; Check for eRx template and code first; IF they exist, run the code
        ; I $D(^C0PX("B","GETMEDS6")),$L($T(SOAP^C0PWS2)) DO  ; smh -cmm for now
        ;. N C0QDEBUG S C0QDEBUG=1 ; This causes the code to print out data;
        ;. D EN^C0QMUERX($$PATLN^C0QMU12(ZYR_"HasERX")) ; Pass the eRx patient list
        ;. K C0QDEBUG ; remove debug variable
        ;
        N C0QCIEN
        S ZI=""
        F  S ZI=$O(C0QPARM(ZI)) Q:ZI=""  D  ;
        . S C0QCIEN=C0QPARM(ZI,"EPMeasurementSet") ; ien of measurement set
        . D UPDATE^C0QUPDT(.G,C0QCIEN) ; UPDATE THE MU MEASUREMENT SET
DEPDN   Q
        ;
TMGPVFN() ;"RETURN FILE NUM FOR PROVIDERS SUB-SUBFILE ;//kt added
        Q 1130580001.4111
        ;
LOCPAT(C0QLIST,PREFIX,LOC,DTE,EDTE,PROVS)          ;"retrieve active outpatients
        ;"Copied and modified from C0QMU12
        ;"INPUT:
        ;" C0QLIST -- the output list.
        ;" PREFIX -- Used in storage name in C0QLIST
        ;" LOC -- IEN in HOSPITAL LOCATION file
        ;" DTE - beginning of measurement period  ;"//kt
        ;" EDTE - end of measurement period -- tbd use this  ;"//kt
        ;" PROVS -- Array of provider IEN's to be included.
        ;"           Format  PROVS(IEN)=""   IEN's in file #2, provider for visit
        ;"NOTE: Uses PROVIEN in global scope
        ;"NOTE: Uses optional TMGVERBOSE.  If 0 then nothing written to screen.
        ;"Output: C0QLIST(PROV,PREFIX_"Patient",DFN)=""   //kt mod, added PROVIDER)IEN,
        ;"Result: NONE
        SET PROVIEN=+$GET(PROVIEN)
        S ULOC=$O(^SC("B",LOC,"")) ; IEN OF HOSPITAL LOCATION
        I ULOC="" D  Q  ; OOPS
        . IF $GET(TMGVERBOSE)'=0 W !,"HOSPITAL LOCATION NOT FOUND: ",LOC
        NEW IDTE S IDTE=9999999-DTE ; INVERSE DATE
        SET EDTE=$GET(EDTE) IF +EDTE>0 SET EDTE=EDTE=.0001 ;"Backup up 1 second to allow $ORDER() to work  ;"//kt added
        N ZI
        ;"//kt origninal --> S ZI="" ; BEGIN AT LATEST DATE FOR THIS LOC IN VISIT FILE
        S ZI=EDTE  ;"//kt added
        F  S ZI=$O(^AUPNVSIT("AHL",ULOC,ZI)) Q:(ZI="")!(ZI>IDTE)  D  ;" FOR EACH DATE
        . NEW EVENTDT SET EVENTDT=9999999-ZI
        . IF $GET(TMGVERBOSE)'=0 W !,$$FMTE^XLFDT(EVENTDT) ;B  ;
        . I ZI="" Q  ;
        . N ZJ S ZJ=""
        . F  S ZJ=$O(^AUPNVSIT("AHL",ULOC,ZI,ZJ)) Q:ZJ=""  D  ;" FOR EACH VISIT
        . . ;"NOTE: ZJ=IEN in VISIT file.
        . . S DFN=$$GET1^DIQ(9000010,ZJ,.05,"I") ; PATIENT
        . . NEW PTNAME SET PTNAME=$PIECE($GET(^DPT(DFN,0)),"^",1)
        . . IF $EXTRACT(PTNAME,1,2)="ZZ" DO  QUIT
        . . . SET PTNAME=""
        . . IF $GET(TMGVERBOSE)'=0 WRITE " ",PTNAME                                                     ;"//kt added
        . . NEW PROVOK SET PROVOK=0                                                ;"//kt added
        . . NEW PROVIEN SET PROVIEN=0                                              ;"//kt added
        . . FOR  SET PROVIEN=$ORDER(PROVS(PROVIEN)) QUIT:(PROVIEN'>0)!PROVOK  DO   ;"//kt added
        . . . IF $DATA(TMGLIMITPROV),($GET(TMGLIMITPROV(PROVIEN))'=1) DO  QUIT     ;"//kt added
        . . . . NEW PROVINIT SET PROVINIT=$PIECE($GET(^VA(200,PROVIEN,0)),"^",2)   ;"//kt added
        . . . . IF $GET(VERBOSE) WRITE " (provider "_PROVINIT_" excluded.) "       ;"//kt added
        . . . SET PROVOK=$$PROVOK(ZJ,PROVIEN,DFN)                                  ;"//kt added
        . . . IF 'PROVOK QUIT                                                      ;"//kt added
        . . . SET C0QLIST(PROVIEN,PREFIX_"Patient",DFN)=""                         ;"//kt added
        . . . SET C0QLIST(PROVIEN,PREFIX_"Patient;Date",DFN,EVENTDT)=""            ;"//kt added
        . . IF $EXTRACT(PTNAME,1,2)="ZZ" SET PROVOK=0  ;"Provider may be OK, but patient is not.if last name starts with ZZ...
        . . IF 'PROVOK,($GET(TMGVERBOSE)'=0) WRITE " <-- this event skipped"
        . . ;"S C0QLIST(PREFIX_"Patient",DFN)=""   //kt <---- original
        . . ;"NEW PROVIDERS   ;"//kt added
        . . ;"DO PROV4VST(ZJ,DFN,.PROVIDERS)  ;"//kt added
        . . ;"NEW PROV SET PROV=""  ;"//kt added
        . . ;"FOR  SET PROV=$ORDER(PROVIDERS(PROV)) QUIT:+PROV'>0  DO  ;"//kt added
        . . ;". SET C0QLIST(PROV,PREFIX_"Patient",DFN)=""              ;"//kt added
        Q
        ;
PROV4VST(VSTIEN,DFN,ARRAY) ;"PROVIDER FOR VISIT
        ;"Return providers for a given VISIT ien
        ;"Input: VSTIEN -- IEN in VISIT file
        ;"       ARRAY -- PASS BY REFEERENCE.  AN OUT PARAMETER.
        ;"       DFN -- PATIENT
        ;"Output: ARRAY(IEN200)=""  IEN200 is IEN of provider
        ;"Results: none
        KILL ARRAY
        NEW PRV SET PRV=""
        FOR  SET PRV=$ORDER(^AUPNVPRV("AD",VSTIEN,PRV)) QUIT:(+PRV'>0)  DO
        . SET ARRAY(PRV)=""
        IF $DATA(ARRAY) GOTO P4VDN
        ;
        NEW TIUIEN SET TIUIEN=0  ;"Scan for TIU Documents with matching VISIT entry
        FOR  SET TIUIEN=$ORDER(^TIU(8925,"V",VSTIEN,TIUIEN)) QUIT:(+TIUIEN'>0)  DO
        . NEW N0 SET N0=$GET(^TIU(8925,TIUIEN,0))
        . IF $PIECE(N0,"^",2)'=DFN SET RESULT=-1 QUIT  ;" 0;2 = PATIENT
        . NEW N12 SET N12=$GET(^TIU(8925,TIUIEN,12))
        . IF +$PIECE(N12,"^",2)>0 SET ARRAY(+$PIECE(N12,"^",2))="" ;" 12;2 = AUTHOR/DICTATOR
        . IF +$PIECE(N12,"^",4)>0 SET ARRAY(+$PIECE(N12,"^",4))="" ;" 12;4 = EXPECTED SIGNER
        . IF +$PIECE(N12,"^",8)>0 SET ARRAY(+$PIECE(N12,"^",8))="" ;" 12;8 = EXPECTED COSIGNER
        . IF +$PIECE(N12,"^",9)>0 SET ARRAY(+$PIECE(N12,"^",9))="" ;" 12;9 = ATTENDING PHYSICIAN
        . NEW N15 SET N15=$GET(^TIU(8925,TIUIEN,15))
        . IF +$PIECE(N15,"^",2)>0 SET ARRAY(+$PIECE(N15,"^",2))="" ;" 15;2 = SIGNED BY
        . IF +$PIECE(N15,"^",8)>0 SET ARRAY(+$PIECE(N15,"^",8))="" ;" 15;8 = COSIGNED BY
        . NEW N16 SET N16=$GET(^TIU(8925,TIUIEN,16))
        . IF +$PIECE(N16,"^",2)>0 SET ARRAY(+$PIECE(N16,"^",2))="" ;" 16;2 = AMENDED BY
        . ;"See IF provider is an additional signer
        . NEW IEN8925D7 SET IEN8925D7=0
        . FOR  SET IEN8925D7=$ORDER(^TIU(8925.7,"B",TIUIEN,IEN8925D7)) QUIT:(+IEN8925D7'>0)  DO
        . . NEW N0 SET N0=$GET(^TIU(8925.7,IEN8925D7,0))
        . . IF +$PIECE(N0,"^",3)>0 SET ARRAY(+$PIECE(N0,"^",3))="" ;"0;3 = EXPECTED COSIGNER
        . . IF +$PIECE(N0,"^",5)>0 SET ARRAY(+$PIECE(N0,"^",5))="" ;"0;5 = ACTUAL COSIGNER
P4VDN   QUIT
        ;
PROVOK(VSTIEN,PROVIEN,DFN) ;"CHECK IF PROVIDER IS OK (Is provider associated with visit?) ;//kt added
        ;"Purpose: to determine IF provider is associated with specified VISIT entry
        ;"Input: VSTIEN -- IEN in VISIT file.
        ;"       PROVIEN -- IEN in NEW PERSON file for provider
        ;"       DFN -- Patient of the encounter
        ;"Result: 1 IF provider is associated, 0 IF not; -1 IF visit doesn't match patient
        NEW RESULT SET RESULT=0
        NEW TIUIEN SET TIUIEN=0  ;"Scan for TIU Documents with matching VISIT entry
        FOR  SET TIUIEN=$ORDER(^TIU(8925,"V",VSTIEN,TIUIEN)) QUIT:(+TIUIEN'>0)!(RESULT'=0)  DO
        . NEW N0 SET N0=$GET(^TIU(8925,TIUIEN,0))
        . IF $PIECE(N0,"^",2)'=DFN SET RESULT=-1 QUIT  ;" 0;2 = PATIENT
        . IF $$NOTEOK(TIUIEN)=0 QUIT  ;"skip note IF not an office visit type note.
        . NEW N12 SET N12=$GET(^TIU(8925,TIUIEN,12))
        . IF $PIECE(N12,"^",2)=PROVIEN SET RESULT=1 QUIT  ;" 12;2 = AUTHOR/DICTATOR
        . IF $PIECE(N12,"^",4)=PROVIEN SET RESULT=1 QUIT  ;" 12;4 = EXPECTED SIGNER
        . IF $PIECE(N12,"^",8)=PROVIEN SET RESULT=1 QUIT  ;" 12;8 = EXPECTED COSIGNER
        . IF $PIECE(N12,"^",9)=PROVIEN SET RESULT=1 QUIT  ;" 12;9 = ATTENDING PHYSICIAN
        . NEW N15 SET N15=$GET(^TIU(8925,TIUIEN,15))
        . IF $PIECE(N15,"^",2)=PROVIEN SET RESULT=1 QUIT  ;" 15;2 = SIGNED BY
        . IF $PIECE(N15,"^",8)=PROVIEN SET RESULT=1 QUIT  ;" 15;8 = COSIGNED BY
        . NEW N16 SET N16=$GET(^TIU(8925,TIUIEN,16))
        . IF $PIECE(N15,"^",2)=PROVIEN SET RESULT=1 QUIT  ;" 16;2 = AMENDED BY
        . ;"See IF provider is an additional signer
        . NEW IEN8925D7 SET IEN8925D7=0
        . FOR  SET IEN8925D7=$ORDER(^TIU(8925.7,"B",TIUIEN,IEN8925D7)) QUIT:(+IEN8925D7'>0)!(RESULT'=0)  DO
        . . NEW N0 SET N0=$GET(^TIU(8925.7,IEN8925D7,0))
        . . IF $PIECE(N0,"^",3)=PROVIEN SET RESULT=1 QUIT  ;"0;3 = EXPECTED COSIGNER
        . . IF $PIECE(N0,"^",5)=PROVIEN SET RESULT=1 QUIT  ;"0;3 = ACTUAL COSIGNER
        IF RESULT=0 DO
        . ;"WRITE " (different provider) "
        QUIT RESULT
        ;
NOTEOK(IEN8925) ;"note ok?
        ;"Purpose: determine IF note type is OK, or should be ignored.
        ;"         At the TMG/FPG facility, only will count a visit when
        ;"         note type is: OFFICE VISIT, or COMPLETE PHYSICAL EXAM, or ACUTE MEDICAL ISSUE VISIT
        ;"Input: IEN8925 -- the IEN of the note in file 8925 to check.
        ;"NOTE: uses arrray with global scope TMGNOTELIST.  This should be killed elsewhere.
        ;"Result: returns 1 IF note OK to use, 0 IF should not use.
        IF $DATA(TMGNOTELIST) GOTO NOK2
        NEW TITLE
        FOR TITLE="OFFICE VISIT","COMPLETE PHYSICAL EXAM","ACUTE MEDICAL ISSUE VISIT" DO
        . NEW IEN SET IEN=+$ORDER(^TIU(8925.1,"B",TITLE,0))
        . IF IEN>0 SET TMGNOTELIST(IEN)=""
NOK2    NEW RESULT
        NEW TITLEIEN SET TITLEIEN=+$PIECE($GET(^TIU(8925,IEN8925,0)),"^",1)
        SET RESULT=($DATA(TMGNOTELIST(TITLEIEN))>0)
        QUIT RESULT
        ;
INIT(ZMU,ZARY,ZTYP)        ; INITIALIZE THE PARAMETERS FOR BUILDING PATIENT LISTS
        ;" ZMU -- PERIOD KEY, TO MATCH AGAINST .02 field of C0Q PARAMETER file  ;"//kt added passing in.
        ;" ZARY --Output array.  PASSED BY NAME  Format:
        ;"           @ZRY@(#,"MU")=
        ;"           @ZRY@(#,"TYPE")=
        ;"           @ZRY@(#,"InpatientMeasurementSet")=
        ;"           @ZRY@(#,"InpatientBeginDate")=
        ;"           @ZRY@(#,"InpatientEndDate")=
        ;"           @ZRY@(#,"InpatientQualitySet")=
        ;"           @ZRY@(#,"EPMeasurementSet")=
        ;"           @ZRY@(#,"EPBeginDate")=
        ;"           @ZRY@(#,"EPEndDate")=
        ;"           @ZRY@(#,"EPQualitySet")=
        ;"           @ZRY@(#,"InpatientQualitySet")=
        ;"           @ZRY@(#,"CLINICS")=(array)
        ;"           @ZRY@(#,"CLINICS","PROVIDERS",PROVIEN)=""
        ;"           Example:
        ;"             C0QPARM(1,"CLINICS",0)="1^*^0^"
        ;"             C0QPARM(1,"CLINICS",0,"MAP")=".01I"
        ;"             C0QPARM(1,"CLINICS",1,1)="Laughlin_Office"
        ;"             C0QPARM(1,"CLINICS",2,1)=1
        ;"             C0QPARM(1,"CLINICS","ID",1,.01)=6
        ;"             C0QPARM(1,"CLINICS","PROVIDERS",168)=""
        ;"             C0QPARM(1,"EPBeginDate")=3120101
        ;"             C0QPARM(1,"EPEndDate")=3121231
        ;"             C0QPARM(1,"EPMeasurementSet")=3
        ;"             C0QPARM(1,"EPQualitySet")=""
        ;"             C0QPARM(1,"InpatientBeginDate")=""
        ;"             C0QPARM(1,"InpatientEndDate")=""
        ;"             C0QPARM(1,"InpatientMeasurementSet")=""
        ;"             C0QPARM(1,"InpatientQualitySet")=""
        ;"             C0QPARM(1,"MU")="MU12"
        ;"             C0QPARM(1,"TYPE")="OUTPATIENT"
        ;"             C0QPARM(2,...
        ;"             C0QPARM(3,...
        ;" ZTYP --"INP" OR "EP"
        K @ZARY ; CLEAR RETURN ARRAY
        N ZIEN,ZCNT,ZX
        I $O(^C0Q(401,"MUTYP",ZMU,ZTYP,""))="" D  Q  ; OOPS NO RECORD THERE
        . W !,"ERROR, NO PARAMETERS AVAILABLE"
        S ZIEN=""
        S ZCNT=0
        F  S ZIEN=$O(^C0Q(401,"MUTYP",ZMU,ZTYP,ZIEN)) Q:ZIEN=""  D  ;
        . S ZCNT=ZCNT+1
        . S @ZARY@(ZCNT,"MU")=$$GET1^DIQ($$C0QPFN^C0QMU12,ZIEN_",",.02)
        . S @ZARY@(ZCNT,"TYPE")=$$GET1^DIQ($$C0QPFN^C0QMU12,ZIEN_",",.03)
        . S ZX=$$GET1^DIQ($$C0QPFN^C0QMU12,ZIEN_",",1,"I")
        . S @ZARY@(ZCNT,"InpatientMeasurementSet")=ZX
        . S @ZARY@(ZCNT,"InpatientBeginDate")=$$GET1^DIQ($$C0QMFN^C0QMU12,ZX_",",.02,"I")
        . S @ZARY@(ZCNT,"InpatientEndDate")=$$GET1^DIQ($$C0QMFN^C0QMU12,ZX_",",.03,"I")
        . S @ZARY@(ZCNT,"InpatientQualitySet")=$$GET1^DIQ($$C0QPFN^C0QMU12,ZIEN_",",1.1,"I")
        . S ZX=$$GET1^DIQ($$C0QPFN^C0QMU12,ZIEN_",",2,"I")
        . S @ZARY@(ZCNT,"EPMeasurementSet")=ZX
        . S @ZARY@(ZCNT,"EPBeginDate")=$$GET1^DIQ($$C0QMFN^C0QMU12,ZX_",",.02,"I")
        . S @ZARY@(ZCNT,"EPEndDate")=$$GET1^DIQ($$C0QMFN^C0QMU12,ZX_",",.03,"I")
        . S @ZARY@(ZCNT,"EPQualitySet")=$$GET1^DIQ($$C0QPFN^C0QMU12,ZIEN_",",2.1,"I")
        . S @ZARY@(ZCNT,"InpatientQualitySet")=$$GET1^DIQ($$C0QPFN^C0QMU12,ZIEN_",",1.1,"I")
        . D CLEAN^DILF
        . D LIST^DIC($$C0QPCFN^C0QMU12,","_ZIEN_",",".01I")
        . ;"//kt mod D LIST^DIC($$C0QPCFN^C0QMU12,","_ZIEN_",",".01I;1;1I")
        . I $D(^TMP("DIERR",$J)) D  Q  ; ERROR READING CLINIC LIST
        . . W !,"ERROR READING CLINIC PARAMETER LIST"
        . M @ZARY@(ZCNT,"CLINICS")=^TMP("DILIST",$J)
        . ;"--- start //kt addition -----------
        . NEW TMPDILIST MERGE TMPDILIST=^TMP("DILIST",$J)
        . NEW LOCIDX SET LOCIDX=0
        . SET LOCIDX=$ORDER(TMPDILIST(2,LOCIDX)) QUIT:(+LOCIDX'>0)  DO
        . . NEW LOCIEN SET LOCIEN=+$GET(TMPDILIST(2,LOCIDX)) QUIT:LOCIEN'>0
        . . DO CLEAN^DILF
        . . DO LIST^DIC($$TMGPVFN,","_LOCIEN_","_ZIEN_",",".01I")
        . . NEW PROVIDX SET PROVIDX=0
        . . FOR  SET PROVIDX=$ORDER(^TMP("DILIST",$J,"ID",PROVIDX)) QUIT:(+PROVIDX'>0)  DO
        . . . NEW PROVIEN SET PROVIEN=+$GET(^TMP("DILIST",$J,"ID",PROVIDX,.01))
        . . . QUIT:PROVIEN'>0
        . . . SET @ZARY@(ZCNT,"CLINICS","PROVIDERS",PROVIEN)=""
        ;
        Q
        ;
GETKEY()  ;"GET MEASUREMENT PERIOD KEY to use for update  //kt added
        ;"Purpose: Get the MEASUREMENT PERIOD KEY (field .02) in file C0Q PARAMETER
        ;"         to use for updating.
        NEW MENU,USRPICK
        NEW RESULT
        SET MENU(0)="Pick MEASUREMENT PERIOD KEY group to update"
        SET MENU(0,1)="  All PARAMS entries with matching KEY will be updated"
        NEW CT SET CT=1
        NEW KEY SET KEY=""
        FOR  SET KEY=$ORDER(^C0Q(401,"MUTYP",KEY)) QUIT:(KEY="")  DO
        . NEW NUM SET NUM=$$LISTCT^TMGMISC2($NAME(^C0Q(401,"MUTYP",KEY,"EP")))
        . SET NUM=NUM+$$LISTCT^TMGMISC2($NAME(^C0Q(401,"MUTYP",KEY,"INP")))
        . SET NUM=NUM+$$LISTCT^TMGMISC2($NAME(^C0Q(401,"MUTYP",KEY,"ALL")))
        . SET MENU(CT)="KEY: "_KEY_" ("_NUM_" PARAM entries)"_$CHAR(9)_KEY,CT=CT+1
GKM     SET RESULT=$$MENU^TMGUSRI2(.MENU)
        IF RESULT="^" SET RESULT=""
GKDN    QUIT RESULT
        ;
FILE(C0QLIST)        ; FILE THE PATIENT LISTS TO C0Q PATIENT LIST
        ;
        I '$D(C0QLIST) Q  ;
        N LFN S LFN=$$C0QALFN^C0QMU12()
        N ZI,ZN
        NEW PROV SET PROV=""  ;"//kt
        FOR  SET PROV=$ORDER(C0QLIST(PROV)) QUIT:(+PROV'>0)  DO  ;"//kt
        . IF $DATA(TMGLIMITPROV),($GET(TMGLIMITPROV(PROV))'=1) QUIT
        . WRITE !,"Filing patient list for provider: ",$$GET1^DIQ(200,PROV,.01),!
        . NEW PROVINIT SET PROVINIT=$PIECE($GET(^VA(200,+PROV,0)),"^",2)  ;"//kt
        . IF PROVINIT="" SET PROVINIT=$PIECE($GET(^VA(200,+PROV,0)),"^",1)  ;"//kt
        . ;"NEW MAXCT SET MAXCT=$$LISTCT^TMGMISC2($NAME(C0QLIST(PROV)))
        . ;"NEW STIME SET STIME=$H
        . ;"NEW PROGCT SET PROGCT=0
        . S ZI=""
        . F  S ZI=$O(C0QLIST(PROV,ZI)) Q:ZI=""  D  ;
        . . WRITE "Filing list: ",ZI,!
        . . ;"SET PROGCT=PROGCT+1
        . . ;"IF PROGCT#10=0 DO
        . . ;". DO PROGBAR^TMGUSRI2(PROGCT,,0,MAXCT,STIME)
        . . NEW RECNAME SET RECNAME=PROVINIT_"-"_ZI
        . . ;"/kt original --> S ZN=$O(^C0Q(301,"CATTR",ZI,""))
        . . S ZN=$O(^C0Q(301,"CATTR",RECNAME,""))
        . . I ZN="" D  ; LIST NOT FOUND, CREATE IT
        . . . K C0QFDA
        . . . S FN=$$C0QPLF^C0QMU12 ; C0Q PATIENT LIST FILE
        . . . ;"/kt original --> S C0QFDA(FN,"+1,",.01)=ZI
        . . . S C0QFDA(FN,"+1,",.01)=RECNAME
        . . . ;"/kt original --> S C0QFDA(FN,"+1,",999)=ZI ; ATTRIBUTE
        . . . S C0QFDA(FN,"+1,",999)=RECNAME ;" ATTRIBUTE
        . . . W !,"CREATING ",RECNAME  ;"//kt was --> ZI
        . . . D UPDIE^C0QMU12 ; ADD THE RECORD
        . . . ;"/kt original --> S ZN=$O(^C0Q(301,"CATTR",ZI,"")) ; THE NEW IEN
        . . . S ZN=$O(^C0Q(301,"CATTR",RECNAME,"")) ;" THE NEW IEN
        . . ;I ZN="" D  Q  ; OOPS
        . . ;. W !,"ERROR, ATTRIBUTE NOT FOUND IN PATIENT LIST FILE:"_ZI
        . . ;S ZN=$$KLNCR(ZN) ; KILL AND RECREATE RECORD ZN
        . . N C0QNEW,C0QOLD,C0QRSLT
        . . S C0QNEW=$NA(C0QLIST(PROV,ZI)) ; THE NEW PATIENT LIST
        . . S C0QOLD=$NA(^C0Q(301,ZN,1,"B")) ; THE OLD PATIENT LIST
        . . D UNITY^C0QSET("C0QRSLT",C0QNEW,C0QOLD) ; FIND WHAT'S NEW
        . . N ZJ,ZK
        . . ; FIRST, DELETE THE OLD ONES - NO LONGER IN THE LIST
        . . K C0QFDA
        . . S ZJ=""
        . . F  S ZJ=$O(C0QRSLT(2,ZJ)) Q:ZJ=""  D  ; MARKED WITH A 2 FROM UNITY
        . . . S ZK=$O(@C0QOLD@(ZJ,"")) ; GET THE IEN OF THE RECORD TO DELETE
        . . . I ZK="" D  Q  ; OOPS SHOULDN'T HAPPEN
        . . . . W !,"INTERNAL ERROR FINDING A PATIENT TO DELETE"
        . . . . S $EC=",U1130580001,"  ; smh - instead of a BREAK
        . . . S C0QFDA(LFN,ZK_","_ZN_",",.01)="@"
        . . I $D(C0QFDA) D UPDIE^C0QMU12 ; PROCESS THE DELETIONS
        . . ; SECOND, PROCESS THE ADDITIONS
        . . K C0QFDA
        . . S ZJ="" S ZK=1
        . . F  S ZJ=$O(C0QRSLT(0,ZJ)) Q:ZJ=""  D  ; PATIENTS TO ADD ARE MARKED WITH 0
        . . . S C0QFDA(LFN,"+"_ZK_","_ZN_",",.01)=ZJ
        . . . S ZK=ZK+1
        . . I $D(C0QFDA) D UPDIE^C0QMU12 ; PROCESS THE ADDITIONS
        . ;. Q
        . ;. K C0QFDA
        . ;. N ZJ,ZC
        . ;. S ZJ="" S ZC=1
        . ;. F  S ZJ=$O(C0QLIST(ZI,ZJ)) Q:ZJ=""  D  ; FOR EACH PAT IN LIST
        . ;. . S C0QFDA(LFN,"?+"_ZC_","_ZN_",",.01)=ZJ
        . ;. . S ZC=ZC+1
        . ;. D UPDIE^C0QMU12
        . ;. W !,"FOUND:"_ZI
        Q
        ;
NEWYEAR   ;
        ;"Purpose: To create all meaningful use objects for the current year
        ;"         by copying the objects from the previous year.
   
        QUIT
        ;"