TMGPRE01 ;TMG/kst/Pre-visit HTML stuff ;6/17/25
	;;1.0;TMG-LIB;**1**;06/17/25
	;
	;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
	;"Copyright (c) 6/17/25  Kevin S. Toppenberg MD
	;"
	;"This file is part of the TMG LIBRARY, and may only be used in accordence
	;" to license terms outlined in separate file TMGLICNS.m, which should
	;" always be distributed with this file.;
	;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
	;
	;"================================================================================"
	;"  NODE-STYLE RPC FOR USER LOG IN
	;"================================================================================"
	;
USRLOGIN(LASTNAME,FIRSTNAME,DOB,ERR)  ;
	;"INPUT -- LASTNAME -- first name
	;"         FIRSTNAME -- Last name
	;"         DOB -- MM-DD-YYYY
	;"         ERR -- PASS BY REFERENCE, an OUT PARAMETER.  Format: "<error message>"
	;"RESULT: 1^SessionID^LASTNAME,FIRSTNAME^DOB if existing session, 2^SessionID^LASTNAME,FIRSTNAME^DOB if new session, or -1 if patient not found
	NEW RESULT SET RESULT="-1^Default Login Failure"
	SET ERR=""  ;"default to no error"
	SET LASTNAME=$$UP^XLFSTR($GET(LASTNAME))
	SET FIRSTNAME=$$UP^XLFSTR($GET(FIRSTNAME))
	SET DOB=$GET(DOB)
	NEW PTINFO
	NEW FULLNAME SET FULLNAME=LASTNAME_","_FIRSTNAME
	SET PTINFO("NAME")=FULLNAME
	SET PTINFO("DOB")=DOB
	NEW DFN SET DFN=$$GETDFN^TMGGDFN(.PTINFO)
	IF DFN'>0 DO  GOTO ULDN
	. SET ERR="Unable to locate patient '"_FULLNAME_", DOB: "_DOB
	SET RESULT=$$GETSESSION^TMGNODE1(DFN)    ;"Result: 1^SessionID  -- if session already existed,  2^SessionID -- if new session was created, default -1
	IF +RESULT>0 DO
	. ;"NOTE: If user entered an incomplete last name, or an alias, it might still be sufficient for finding unique patient.;
	. NEW ZN SET ZN=$GET(^DPT(DFN,0))
	. SET FULLNAME=$PIECE(ZN,"^",1)  ;"Get actual matched patient name
	. NEW FMDT SET FMDT=$PIECE(ZN,"^",3)  ;"Get actual matched patient DOB
	. NEW PTDOB SET PTDOB=$$FMTE^XLFDT(FMDT,"2D")
	. SET RESULT=RESULT_"^"_FULLNAME_"^"_PTDOB
ULDN    ;
	QUIT RESULT
	;
	;"================================================================================"
	;"  NODE-STYLE RPC FOR GETTING FORMS NEEDED FOR PATIENT.;
	;"================================================================================"
	;
GETPATFORMS(OUT,SESSIONID,ERR,FMDT,MODE)    ;"Get list of needed documents for patient.;
	;"INPUT:  OUT -- AN OUT PARAMETER.  FORMAT:
	;             OUT(#,"text")
	;             OUT(#,"viewName")
	;             OUT(#,"text")
	;             OUT(#,"progress")
	;             OUT(#,"iconName")
	;"        SESSIONID
	;"        ERR
	;"        FMDT -- AS OF date, optional.  Default is NOW
	;"        MODE -- optional.  If 1, then ALL forms are returned (regardless of applicability)"
	KILL OUT
	NEW RESULT SET RESULT="1^OK"
	SET ERR=""  ;"default to no error"
	SET SESSIONID=$GET(SESSIONID)
	NEW TMGDFN SET TMGDFN=$$SESSION2DFN^TMGNODE1(SESSIONID,.ERR)
	IF TMGDFN'>0 DO  GOTO GDLDN
	. SET RESULT="0^"_ERR
	NEW REF SET REF=$$ID2DATAREF^TMGNODE1(SESSIONID,.ERR)
	IF REF="" DO  GOTO GDLDN
	. SET RESULT="-1^"_ERR
	SET FMDT=+$GET(FMDT) IF FMDT'>0 SET FMDT=$$NOW^XLFDT()
	SET MODE=+$GET(MODE)
	NEW IDX SET IDX=-1
	NEW ALLFORMS
	;"                           1          2        3           4        5        6             7          8                9
	;"Format: ALLFORMS(IDX)="DisplayName^viewName^storageNode^IconName^DueFnTag^DueFnRtn^NoteWriterTag^NoteWriterRtn^TargetNoteTitle"
	;"----------------------------------------------------------------"
	SET ALLFORMS($I(IDX))="Update Medical History^hxupdate^HX^ClipboardPlus^DUEHX^TMGPRE02^WTHX^TMGPRE02^NoteTitleHERE"
	SET ALLFORMS($I(IDX))="Head-to-toe Questions^rosupdate^ROS^CoughingIcon^DUEROS^TMGPRE02^WTROS^TMGPRE02^NoteTitleHERE"
	SET ALLFORMS($I(IDX))="Medication Review^medication_review^MEDS^ClipboardCapsule^DUERX^TMGPRE02^WTRX^TMGPRE02^NoteTitleHERE"
	SET ALLFORMS($I(IDX))="Over-the-counter Suppliments, Vitamins, Meds^otc_medication_review^OTC-MEDS^HerbalBottle^DUEOTC^TMGPRE02^WTOTC^TMGPRE02^TITLE"
	SET ALLFORMS($I(IDX))="Medication Allergies Review^medication_allergy_review^ALLERGY^RxAllergy^DUEALRGY^TMGPRE02^WTALRGY^TMGPRE02^TITLE"
	SET ALLFORMS($I(IDX))="Consent to Treat^patient_consent_form^CSNT^TalkQuestionMark^DUECONSENT^TMGPRE02^WTCONSENT^TMGPRE02^TITLE"
	SET ALLFORMS($I(IDX))="Mental Health Questionnaire^phq9Quest^PHQ9^Frown^DUEMH^TMGPRE02^WTMH^TMGPRE02^TITLE"
	SET ALLFORMS($I(IDX))="Medicare Annual Wellness Visit (AWV) Questionnaire^awvQuest^AWV^HealthCurve^DUEAWV^TMGPRE02^WTAWV^TMGPRE02^TITLE"
	;"SET ALLFORMS($I(IDX))="Patient Demographics^demographics_form^??"
	;"SET ALLFORMS($I(IDX))="Insurance Information^insurance_form^??"
	;"SET ALLFORMS($I(IDX))="Test signature^sig_form^SIG1^Certificate"
	NEW JDX SET JDX=-1
	SET IDX=""
	FOR  SET IDX=$ORDER(ALLFORMS(IDX)) QUIT:IDX=""  DO
	. NEW ITEM SET ITEM=$GET(ALLFORMS(IDX))
	. IF ITEM="" QUIT
	. NEW DUE SET DUE=$SELECT(MODE=1:1,1:-1)
	. IF DUE=-1 DO   ;"Determine if form is needed for patient"
	. . NEW DUETAG SET DUETAG=$PIECE(ITEM,"^",5) QUIT:DUETAG=""
	. . NEW DUERTN SET DUERTN=$PIECE(ITEM,"^",6) QUIT:DUERTN=""
	. . NEW CODE SET CODE="SET DUE=$$"_DUETAG_"^"_DUERTN_"("_TMGDFN_","_FMDT_")"
		. . NEW $ETRAP SET $ETRAP="SET $ETRAP="""",$ECODE="""" "  ;"Ignore error.  If fails then DUE not set, so will quit loop
		. . XECUTE CODE   ;"e.g. 'SET DUE=$$DUEROS^TMGPRE02(12345,30295.121201)"
	. IF DUE'=1 QUIT
	. SET JDX=JDX+1
	. SET OUT(JDX,"text")=$PIECE(ITEM,"^",1)  ;"Display Name
	. SET OUT(JDX,"viewName")=$PIECE(ITEM,"^",2)  ;"View Name
	. NEW NODE SET NODE=$PIECE(ITEM,"^",3)  ;"Storage Node
	. SET OUT(JDX,"storeNode")=NODE
	. MERGE OUT(JDX,"progress")=@REF@(NODE_"-PROGRESS")  ;"Get progress data for this form
	. SET OUT(JDX,"iconName")=$PIECE(ITEM,"^",4)  ;"Icon Name
	. SET OUT(JDX,"writerFnTag")=$PIECE(ITEM,"^",7)  ;"WriterFn Tag
	. SET OUT(JDX,"writerFnRoutine")=$PIECE(ITEM,"^",8)  ;"WriterFn Routine
	. SET OUT(JDX,"targetNoteTitleName")=$PIECE(ITEM,"^",9)  ;"Target Title Name
GDLDN ;
	QUIT RESULT
	;
FINALIZEFORMS(SESSIONID,ERR)  ;"Save data (patient responses) from HxUpdate form.;
	NEW RESULT SET RESULT="1^OK"
	SET ERR=""  ;"default to no error"
	SET SESSIONID=$GET(SESSIONID)
	NEW TMGDFN SET TMGDFN=$$SESSION2DFN^TMGNODE1(SESSIONID,.ERR)
	IF TMGDFN'>0 DO  GOTO FFDN
	. SET RESULT="0^"_ERR
	NEW TMGREF SET TMGREF=$$ID2DATAREF^TMGNODE1(SESSIONID,.ERR)
	IF TMGREF="" DO  GOTO FFDN
	. SET RESULT="-1^"_ERR
	NEW FORMARR
	DO GETPATFORMS(.FORMARR,SESSIONID,.ERR)    ;"Get list of due documents for patient, as of NOW
	SET RESULT=$$OUTPUTFORMS^TMGPRE02(TMGREF,.FORMARR,.TMGDFN,.ERR)
FFDN  ;
	QUIT RESULT
	;
	;"================================================================================"
	;"  NODE-STYLE RPC FOR GETTING/SAVING USER HISTORY UPDATE
	;"================================================================================"
	;
GETHXDATA(OUT,OUTPROGRESS,SESSIONID,ERR)  ;"Get data for patient history form (based on patient's prior responses)
	NEW RESULT SET RESULT=$$GETCOMMON("HX",.OUT,.OUTPROGRESS,.SESSIONID,.ERR) ;"Get data for patient"
	QUIT RESULT;
	;
	;"--------------------------------------------------------------------------------""
	;
SAVEHXDATA(SESSIONID,DATA,PROGRESS,ERR)  ;"Save data (patient responses) from HxUpdate form.;
	NEW RESULT SET RESULT=$$SAVECOMMON("HX",.SESSIONID,.DATA,.PROGRESS,ERR)
	QUIT RESULT
	;
	;"================================================================================"
	;"  NODE-STYLE RPC FOR GETTING/SAVING USER ROS UPDATE
	;"================================================================================"
	;
GETROSDATA(OUT,OUTPROGRESS,SESSIONID,ERR)  ;"Get data for patient ROS form (based on patient's prior responses)
	NEW RESULT SET RESULT=$$GETCOMMON("ROS",.OUT,.OUTPROGRESS,.SESSIONID,.ERR) ;"Get data for patient"
	QUIT RESULT;
	;"--------------------------------------------------------------------------------""
SAVEROSDATA(SESSIONID,DATA,PROGRESS,ERR)  ;"Save data (patient responses) from ROS form.;
	NEW RESULT SET RESULT=$$SAVECOMMON("ROS",.SESSIONID,.DATA,.PROGRESS,ERR)
	QUIT RESULT
	;
	;"================================================================================"
	;"  NODE-STYLE RPC FOR GETTING/SAVING USER MEDICATION LIST
	;"================================================================================"
	;
GETMEDS(OUT,OUTPROGRESS,SESSIONID,ERR,MODE)  ;"Get data for patient medications form (based on patient's prior responses)
	;"INPUT:  OUT -- AN OUT PARAMETER.  FORMAT:
	;"             OUT(#,"text")=<MEDICATION LINE>
	;"             OUT(#,"otc")=1 or 0 as boolean for if over-the-counter
	;"             OUT(#,"parsed")=<medication parts with html tags for parts of the med line>
	;"             --below are examples of stored values after questions answered and saved back.  NOT returned when first starting, before first save. --
	;"             OUT(#,"areTaking")= yes
	;"             OUT(#,"isComplete")= %%bool%%true
	;"             OUT(#,"needsRefill")= no
	;"             OUT(#,"refillLocation")= %%null%%
	;"             OUT(#,"comment")= "hello"
	;"        OUTPROGRESS
	;"        SESSIONID
	;"        ERR
	;"        MODE: Optional.  Default=0.  If 0: return Meds & OTC Meds.  If 1: return Meds only.  If 2: return OTC Meds only
	NEW ZZDEBUG SET ZZDEBUG=0
	IF ZZDEBUG=1 DO
	. SET SESSIONID=$GET(^TMG("TMP","GETMEDS^TMGPRE01","SESSIONID"))
	. SET MODE=$GET(^TMG("TMP","GETMEDS^TMGPRE01","MODE"))
	. MERGE OUT=^TMG("TMP","GETMEDS^TMGPRE01","OUT")  ;"so I can see what output was last time"
	ELSE  DO
	. SET ^TMG("TMP","GETMEDS^TMGPRE01","SESSIONID")=SESSIONID
	. SET ^TMG("TMP","GETMEDS^TMGPRE01","MODE")=+$GET(MODE)
	KILL OUT,OUTPROGRESS
	NEW RESULT SET RESULT="1^OK"
	SET ERR=""  ;"default to no error"
	SET MODE=+$GET(MODE)
	NEW WANTRX SET WANTRX=((MODE=0)!(MODE=1))
	NEW WANTOTC SET WANTOTC=((MODE=0)!(MODE=2))
	NEW MEDREF,PROGREF
	NEW REF SET REF=$$ID2DATAREF^TMGNODE1(SESSIONID,.ERR)
	IF REF="" DO  GOTO GMEDDN
	. SET RESULT="-1^"_ERR
	IF MODE=2 DO   ;"OTC only
	. SET MEDREF=$NAME(@REF@("OTC-MEDS"))
	. SET PROGREF=$NAME(@REF@("OTC-MEDS-PROGRESS"))
	ELSE  DO
	. SET MEDREF=$NAME(@REF@("MEDS"))
	. SET PROGREF=$NAME(@REF@("MEDS-PROGRESS"))
	IF ($GET(@MEDREF)="%%empty%%")!($GET(@MEDREF)="%%empty_array%%") KILL @MEDREF ; Clear existing data
	IF $DATA(@MEDREF)>0 GOTO GMEL1  ;"If there is already a med list, return it
	;"Otherwise, will get meds from TIU Object
	NEW TMGDFN SET TMGDFN=$$SESSION2DFN^TMGNODE1(SESSIONID,.ERR)
	IF TMGDFN'>0 DO  GOTO GMEDDN
	. SET RESULT="0^"_ERR
	;"NEW TMP,ARR SET TMP=$$GETTABLX^TMGTIUOJ(TMGDFN,"FINAL MEDICATIONS",.ARR)
	NEW ARR DO GETRXTBL^TMGRX007(.ARR,,TMGDFN)  ;"GET MED TABLE FOR PATIENT from file 22733.2.;
	NEW OPTION SET OPTION("FOR PATIENTS")=1
	SET OPTION("HTML COLOR TAGS")=1
	NEW OTC,IDX,JDX,LAST SET JDX=-1,LAST=""
	FOR OTC=0:1:1 DO   ;"Consider regular Rx's and OTC Rx's separately, so I can force OTC Rx's to end of return array
	. IF OTC=0,WANTRX=0 QUIT  ;"OTC=0 means consider only prescript Rx's, but if WANTRX=0, skip.;
	. IF OTC=1,WANTOTC=0 QUIT ;"OTC=1 means consider only OTC Rx's, but if WANTOTC=0, skip
	. SET IDX=0
	. FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:+IDX'>0  DO
	. . IF $GET(ARR(IDX))=LAST QUIT
	. . NEW ITEM SET ITEM=$GET(ARR(IDX))
	. . IF ITEM["----" QUIT
	. . IF $EXTRACT(ITEM,1)="*" QUIT
	. . NEW RXISOTC SET RXISOTC=(ITEM["OTC ")
	. . IF OTC=0,RXISOTC QUIT   ;"if OTC=0 only process non-OTC Rx's
	. . IF OTC=1,RXISOTC=0 QUIT ;"if OTC=1 only process OTC Rx's
	. . NEW PARSED DO PARSELN^TMGRX001(.PARSED,ITEM,0,.OPTION)
	. . SET JDX=JDX+1
	. . SET @MEDREF@(JDX,"text")=ITEM
	. . SET @MEDREF@(JDX,"parsed")=$$EXTERNAL^TMGRX003(.PARSED,.OPTION)
	. . SET @MEDREF@(JDX,"otc")=+$GET(PARSED("OTC"))
	. . SET LAST=ITEM
GMEL1 ;
	MERGE OUT=@MEDREF
	MERGE OUTPROGRESS=@PROGREF
	KILL ^TMG("TMP","GETMEDS^TMGPRE01","OUT") MERGE ^TMG("TMP","GETMEDS^TMGPRE01","OUT")=OUT
GMEDDN ;
	QUIT RESULT;
	;
TESTTAGML  ;"TEST TAGGED MED LIST
	NEW DIC,X,Y,TMGDFN,STR,ARR
	SET DIC=2,DIC(0)="MAEQ" DO ^DIC WRITE ! SET TMGDFN=+Y IF TMGDFN'>0 QUIT
	NEW ARRAY,OPTION
	IF $GET(DT)>0 SET OPTION("DT")=DT
	DO PRIORRXT^TMGTIUO8(+Y,48,.ARRAY,1,.OPTION)
	KILL OPTION
	SET OPTION("FOR PATIENTS")=1
	SET OPTION("HTML COLOR TAGS")=1
	NEW IDX SET IDX=0
	FOR  SET IDX=$ORDER(ARRAY(IDX)) QUIT:IDX'>0  DO
	. NEW LINE SET LINE=$GET(ARRAY(IDX)) QUIT:LINE=""
	. NEW PARSED DO PARSELN^TMGRX001(.PARSED,LINE,0,.OPTION)
	. SET ARRAY(IDX)=$$EXTERNAL^TMGRX003(.PARSED,.OPTION)
	IF $DATA(ARRAY) ZWR ARRAY
	QUIT
	;
	;"--------------------------------------------------------------------------------""
	;
SAVEMEDS(SESSIONID,DATA,PROGRESS,ERR,MODE)  ;"Save data
	;"        MODE: Optional.  Default=0.  If 0: return Meds & OTC Meds.  If 1: return Meds only.  If 2: return OTC Meds only
	NEW ZZDEBUG SET ZZDEBUG=0
	IF ZZDEBUG=1 DO
	. SET SESSIONID=$GET(^TMG("TMP","SAVEMEDS^TMGPRE01","SESSIONID"))
	. SET MODE=$GET(^TMG("TMP","SAVEMEDS^TMGPRE01","MODE"))
	. KILL DATA MERGE DATA=^TMG("TMP","SAVEMEDS^TMGPRE01","DATA")
	ELSE  DO
	. SET ^TMG("TMP","SAVEMEDS^TMGPRE01","SESSIONID")=SESSIONID
	. SET ^TMG("TMP","SAVEMEDS^TMGPRE01","MODE")=+$GET(MODE)
	. KILL ^TMG("TMP","SAVEMEDS^TMGPRE01","DATA")
	. MERGE ^TMG("TMP","SAVEMEDS^TMGPRE01","DATA")=DATA
	NEW NODE SET NODE=$SELECT(+$GET(MODE)=2:"OTC-MEDS",1:"MEDS")
	NEW RESULT SET RESULT=$$SAVECOMMON(NODE,.SESSIONID,.DATA,.PROGRESS,ERR)
	QUIT RESULT
	;
	;"================================================================================"
	;"  NODE-STYLE RPC FOR GETTING/SAVING SIGNATURE
	;"================================================================================"
	;
GETSIG1(OUTB64,OUTPROGRESS,OUTARR,SESSIONID,ERR) ;"Get signature data for patient"
	;"OUT -- base64 encoded string of signature image
	NEW IDX SET IDX=-1
	;"LATER THIS WOULD BE DYNAMIC..."
	SET OUTARR($I(IDX))="<p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.</p>"
	SET OUTARR($I(IDX))="<p>Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum. Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo.</p>"
	SET OUTARR($I(IDX))="<p>Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt.</p>"
	NEW RESULT SET RESULT=$$GETCOMMON("SIG1",.OUTB64,.OUTPROGRESS,.SESSIONID,.ERR) ;"Get data for patient"
	QUIT RESULT
	;
	;"--------------------------------------------------------------------------------""
SAVESIG1(SESSIONID,IMAGEB64,PROGRESS,ERR) ;"Get signature data for patient"
	NEW RESULT SET RESULT=$$SAVECOMMON("SIG1",.SESSIONID,.IMAGEB64,.PROGRESS,ERR)
	QUIT RESULT
	;
	;
	;"================================================================================"
	;"  NODE-STYLE RPC FOR GETTING/SAVING CONSENT INFO
	;"================================================================================"
	;
GETCSNT(OUTDATA,OUTSIG,OUTPROGRESS,SESSIONID,ERR) ;"Get consent data for patient"
	NEW RESULT SET RESULT=$$GETCOMMON("CSNT",.OUTDATA,.OUTPROGRESS,.SESSIONID,.ERR) ;"Get consent data for patient"
	SET OUTSIG=$$SPLITGET^TMGNODE1($NAME(OUTDATA("SIG")))
	KILL OUTDATA("SIG")   ;"Sent back separately, so kill from OUTDATA
	KILL OUTDATA("HTML")  ;"this doesn't need to be sent to client
	QUIT RESULT
	;"--------------------------------------------------------------------------------""
SAVECSNT(SESSIONID,DATA,SIG,HTML,PROGRESS,ERR) ;"Get consent data for patient"
	;"SIG is Base64 ASCII encoded image of signature.  Can be large and cause problems for JSON parser upstream
	;"HTML is string of entire non-interactive version of form.  Can be large and cause problems for JSON parser upstream
	DO SPLITSET^TMGNODE1($NAME(DATA("SIG")),$GET(SIG))    ;"Set STR into @REF, splitting to smaller strings if needed.;
	DO SPLITSET^TMGNODE1($NAME(DATA("HTML")),$GET(HTML))  ;"Set STR into @REF, splitting to smaller strings if needed.;
	NEW RESULT SET RESULT=$$SAVECOMMON("CSNT",.SESSIONID,.DATA,.PROGRESS,.ERR)
	KILL DATA("SIG"),DATA("HTML")  ;"NOTE: KILL DATA <-- this caused the entire RPC system to break somehow.  Perhaps debug later...;
	SET SIG="",HTML=""  ;"No need to pass back to client -- would be wasted data transfer.;
	QUIT RESULT
	;"--------------------------------------------------------------------------------""
SAVECSNTHTML(SESSIONID,HTML,ERR) ;"Save HTML for consent form for patient"
	NEW DATA DO SPLITSET^TMGNODE1($NAME(DATA("HTML")),$GET(HTML))  ;"Set STR into @REF, splitting to smaller strings if needed.;
	NEW RESULT SET RESULT=$$SAVECOMMON2("CSNT",.SESSIONID,.DATA,.ERR)
		QUIT RESULT
	;
	;"================================================================================"
	;"  NODE-STYLE RPC FOR GETTING/SAVING USER ALLERGIES QUESTIONNAIRE
	;"================================================================================"
	;
GETALRGY(OUT,OUTPROGRESS,SESSIONID,ERR)  ;"Get data for patient ALLERGY form (based on patient's prior responses -- if 1st time, from system allergy list)
	;"INPUT:  OUT -- AN OUT PARAMETER.  FORMAT:
	;"             OUT(#,"nkda")="%%bool%%true"   -- if patient is nkda (no known drug allergies)
	;"             OUT(#,"never_assessed")="%%bool%%true" -- if allergies have never been asked or assessed
	;"             OUT(#,"item")=$PIECE(LINE,"^",1)  -- the Rx or item causing allergy
	;"             OUT(#,"date")=$PIECE(LINE,"^",2)  -- the date the allergy was entered or recorded
	;"             OUT(#,"reaction")=$PIECE(LINE,"^",3) -- comments (if any) about reaction
	;"             --below are examples of stored values after questions answered and saved back.  NOT returned when first starting, before first save. --
	;"             ... finish ...;
	;"        OUTPROGRESS
	;"        SESSIONID
	;"        ERR
	NEW ZZDEBUG SET ZZDEBUG=0
	IF ZZDEBUG=1 DO
	. SET SESSIONID=$GET(^TMG("TMP","GETALRGY^TMGPRE01","SESSIONID"))
	. MERGE OUT=^TMG("TMP","GETALRGY^TMGPRE01","OUT")  ;"so I can see what output was last time"
	ELSE  DO
	. SET ^TMG("TMP","GETALRGY^TMGPRE01","SESSIONID")=SESSIONID
	NEW RESULT SET RESULT=$$GETCOMMON("ALLERGY",.OUT,.OUTPROGRESS,.SESSIONID,.ERR) ;"Get data for patient"
	IF $DATA(OUT)>0 GOTO GAGYDN
	NEW TMGDFN SET TMGDFN=$$SESSION2DFN^TMGNODE1(SESSIONID,.ERR)
	IF TMGDFN'>0 DO  GOTO GAGYDN
	. SET RESULT="0^"_ERR
	NEW ARR DO DETALRGYARR^TMGTIUO3(.ARR,TMGDFN)  ;"Return array with allergy info
	;"ARR Format:
	;"   ARR(#)='<Rx/Agent>^<External Date Entered>^<Reaction description>'
	;"   If patient has been marked as NKDA, then will get ARR(1)="No Known Allergies"
	;"   If allergies have never been checked, then will get ARR(1)="No Allergy Assessment"
	;"   If neither above apply, but still allergy is not found for some reason, will get OUT(1)="No Allergies Found"
	NEW JDX SET JDX=0
	NEW IDX SET IDX=0
	FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:IDX'>0  DO
	. NEW LINE SET LINE=$GET(ARR(IDX)) QUIT:LINE=""
	. IF LINE="No Known Allergies" SET OUT(JDX,"nkda")="%%bool%%true"
	. ELSE  IF LINE="No Allergy Assessment" SET OUT(JDX,"never_assessed")="%%bool%%true"
	. ELSE  DO
	. . SET OUT(JDX,"text")=$PIECE(LINE,"^",1)
	. . SET OUT(JDX,"date")=$PIECE(LINE,"^",2)
	. . SET OUT(JDX,"reaction")=$PIECE(LINE,"^",3)
	. SET JDX=JDX+1
GAGYDN ;
	KILL ^TMG("TMP","GETALRGY^TMGPRE01","OUT") MERGE ^TMG("TMP","GETALRGY^TMGPRE01","OUT")=OUT
	QUIT RESULT;
	;"--------------------------------------------------------------------------------""
SAVEALRGY(SESSIONID,DATA,PROGRESS,ERR)  ;"Save data (patient responses) from allegies form.;
	NEW ZZDEBUG SET ZZDEBUG=0
	IF ZZDEBUG=1 DO
	. SET SESSIONID=$GET(^TMG("TMP","SAVEALRGY^TMGPRE01","SESSIONID"))
	. KILL DATA MERGE DATA=^TMG("TMP","SAVEALRGY^TMGPRE01","DATA")
	ELSE  DO
	. SET ^TMG("TMP","SAVEALRGY^TMGPRE01","SESSIONID")=SESSIONID
	. KILL ^TMG("TMP","SAVEALRGY^TMGPRE01","DATA")
	. MERGE ^TMG("TMP","SAVEALRGY^TMGPRE01","DATA")=DATA
	NEW RESULT SET RESULT=$$SAVECOMMON("ALLERGY",.SESSIONID,.DATA,.PROGRESS,ERR)
	QUIT RESULT
	;
	;"================================================================================"
	;"  NODE-STYLE RPC FOR GETTING/SAVING USER PHQ-9 QUESTIONNAIRE
	;"================================================================================"
	;
GETPHQ9DATA(OUT,OUTPROGRESS,SESSIONID,ERR)  ;"Get data for patient PHQ9 form (based on patient's prior responses)
	NEW RESULT SET RESULT=$$GETCOMMON("PHQ9",.OUT,.OUTPROGRESS,.SESSIONID,.ERR) ;"Get data for patient"
	QUIT RESULT;
	;"--------------------------------------------------------------------------------""
SAVEPHQ9DATA(SESSIONID,DATA,PROGRESS,ERR)  ;"Save data (patient responses) from PHQ9 form.;
	NEW RESULT SET RESULT=$$SAVECOMMON("PHQ9",.SESSIONID,.DATA,.PROGRESS,ERR)
	QUIT RESULT
	;
	;"================================================================================"
	;"  NODE-STYLE RPC FOR GETTING/SAVING USER AWV QUESTIONNAIRE
	;"================================================================================"
	;
GETAWVDATA(OUT,OUTPROGRESS,SESSIONID,ERR)  ;"Get data for patient AWV form (based on patient's prior responses)
	NEW RESULT SET RESULT=$$GETCOMMON("AWV",.OUT,.OUTPROGRESS,.SESSIONID,.ERR) ;"Get data for patient"
	QUIT RESULT;
	;"--------------------------------------------------------------------------------""
SAVEAWVDATA(SESSIONID,DATA,PROGRESS,ERR)  ;"Save data (patient responses) from PHQ9 form.;
	NEW RESULT SET RESULT=$$SAVECOMMON("AWV",.SESSIONID,.DATA,.PROGRESS,ERR)
	QUIT RESULT
	;
	;"================================================================================"
	;"  NODE-STYLE RPC COMMON CODE
	;"================================================================================"
	;
GETCOMMON(NODE,OUTDATA,OUTPROGRESS,SESSIONID,ERR) ;"Get generic data for patient"
	KILL OUTDATA,OUTPROGRESS
	NEW RESULT SET RESULT="1^OK"
	SET ERR=""  ;"default to no error"
	NEW REF SET REF=$$ID2DATAREF^TMGNODE1(SESSIONID,.ERR)
	IF REF="" DO  GOTO GCDN
	. SET RESULT="-1^"_ERR
	NEW PROGRESSNODE SET PROGRESSNODE=NODE_"-PROGRESS"
	MERGE OUTDATA=@REF@(NODE)
	MERGE OUTPROGRESS=@REF@(PROGRESSNODE)
GCDN ;
	QUIT RESULT
	;
SAVECOMMON(NODE,SESSIONID,DATA,PROGRESS,ERR)  ;common code for saving elements
	NEW RESULT SET RESULT="1^OK"
	SET ERR=""  ;"default to no error"
	NEW REF SET REF=$$ID2DATAREF^TMGNODE1(.SESSIONID,.ERR)
	IF REF="" DO  GOTO SCDN
	. SET RESULT="-1^"_ERR
	NEW PROGRESSNODE SET PROGRESSNODE=NODE_"-PROGRESS"
	KILL @REF@(NODE),@REF@(PROGRESSNODE) ; Clear existing data
	MERGE @REF@(NODE)=DATA
	MERGE @REF@(PROGRESSNODE)=PROGRESS
SCDN ;
	QUIT RESULT
	;
SAVECOMMON2(NODE,SESSIONID,DATA,ERR)  ;common code for saving just certain elements.  Doesn't kill prior elements.;
	NEW RESULT SET RESULT="1^OK"
	SET ERR=""  ;"default to no error"
	NEW REF SET REF=$$ID2DATAREF^TMGNODE1(.SESSIONID,.ERR)
	IF REF="" DO  GOTO SCDN2
	. SET RESULT="-1^"_ERR
	MERGE @REF@(NODE)=DATA
SCDN2 ;
	QUIT RESULT
	;
	;