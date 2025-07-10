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
	;"RESULT: 1^SessionID if existing session, 2^SessionID if new session, or -1 if patient not found
	NEW RESULT SET RESULT=-1 ; Default to failure
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
ULDN    ;
	QUIT RESULT
	;
	;"================================================================================"
	;"  NODE-STYLE RPC FOR GETTING FORMS NEEDED FOR PATIENT.;
	;"================================================================================"
	;
GETPATFORMS(OUT,SESSIONID,ERR)    ;"Get list of needed documents for patient.;
	KILL OUT
	NEW RESULT SET RESULT="1^OK"
	SET ERR=""  ;"default to no error"
	SET SESSIONID=$GET(SESSIONID)
	NEW DFN SET DFN=$$SESSION2DFN^TMGNODE1(SESSIONID)
	IF DFN'>0 DO  GOTO GDLDN
	. SET RESULT="0"
	. SET ERR="Patient not found for sessionID"
	NEW REF SET REF=$$ID2DATAREF^TMGNODE1(SESSIONID)
	NEW IDX SET IDX=-1
	NEW ALLFORMS
	;"Format: ALLFORMS(IDX)="DisplayName^viewName^storageNode"
	SET ALLFORMS($I(IDX))="Update Medical History^hxupdate^HX"
	SET ALLFORMS($I(IDX))="Head-to-toe Questions^rosupdate^ROS"
	SET ALLFORMS($I(IDX))="Medication Review^medication_review^MEDS"
	SET ALLFORMS($I(IDX))="Test signature^sig_form^SIG1"
	SET ALLFORMS($I(IDX))="Consent to Treat^patient_consent_form^CSNT"
	;"SET ALLFORMS($I(IDX))="Patient Demographics^demographics_form^??"
	;"SET ALLFORMS($I(IDX))="Insurance Information^insurance_form^??"
	NEW JDX SET JDX=-1
	SET IDX=""
	FOR  SET IDX=$ORDER(ALLFORMS(IDX)) QUIT:IDX=""  DO
	. NEW ITEM SET ITEM=$GET(ALLFORMS(IDX))
	. IF ITEM="" QUIT
	. ;"Logic here to determine if form is needed for patient"
	. SET JDX=JDX+1
	. SET OUT(JDX,"text")=$PIECE(ITEM,"^",1)  ;"Display Name
	. SET OUT(JDX,"viewName")=$PIECE(ITEM,"^",2)  ;"View Name
	. NEW NODE SET NODE=$PIECE(ITEM,"^",3)  ;"Storage Node
	. MERGE OUT(JDX,"progress")=@REF@(NODE_"-PROGRESS")  ;"Get progress data for this form
	. ;
	;"TO DO: Add logic to determine which forms are needed for patient
	;"  Planned format: DisplayName^viewName^totalItems^completedItems^percentComplete"
	;SET OUT(IDX)="Update Medical History^hxupdate",IDX=IDX+1
	;SET OUT(IDX)="Head-to-toe Questions^rosupdate",IDX=IDX+1
	;SET OUT(IDX)="Medication Review^medication_review",IDX=IDX+1	;"ELH added 7/1/25"
GDLDN ;
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
	NEW RESULT SET RESULT=$$SAVECOMMON("HX",SESSIONID,.DATA,.PROGRESS,ERR)
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
	NEW RESULT SET RESULT=$$SAVECOMMON("ROS",SESSIONID,.DATA,.PROGRESS,ERR)
	QUIT RESULT
	;
	;"================================================================================"
	;"  NODE-STYLE RPC FOR GETTING/SAVING USER MEDICATION LIST
	;"================================================================================"
	;
GETMEDS(OUT,OUTPROGRESS,SESSIONID,ERR)  ;"Get data for patient ROS form (based on patient's prior responses)
	KILL OUT,OUTPROGRESS
	NEW RESULT SET RESULT="1^OK"
	SET ERR=""  ;"default to no error"
	NEW REF SET REF=$$ID2DATAREF^TMGNODE1(SESSIONID)
	IF REF="" DO  GOTO GMEDDN
	. SET RESULT=-1
	. SET ERR="Error finding patient data from session ID"
	IF $GET(@REF@("MEDS"))="%%empty%%" KILL @REF@("MEDS") ; Clear existing data
	IF $DATA(@REF@("MEDS"))>0 GOTO GMEL1  ;"If there is already a med list, return it
	;"Otherwise, will get meds from TIU Object
	NEW DFN SET DFN=$$SESSION2DFN^TMGNODE1(SESSIONID)
	IF DFN'>0 DO  GOTO GMEDDN
	. SET RESULT=0
	. SET ERR="Patient not found for sessionID"
	NEW TMP,ARR SET TMP=$$GETTABLX^TMGTIUOJ(DFN,"FINAL MEDICATIONS",.ARR)
	NEW IDX,JDX,LAST SET JDX=-1,IDX=0,LAST=""
	FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:+IDX'>0  DO
	. IF $GET(ARR(IDX))=LAST QUIT
	. NEW ITEM SET ITEM=$GET(ARR(IDX))
	. IF ITEM["----" QUIT
	. IF $EXTRACT(ITEM,1)="*" QUIT
	. SET @REF@("MEDS",$I(JDX),"text")=ITEM  ;"prepare data for output
	. SET LAST=ITEM
GMEL1 ;
	MERGE OUT=@REF@("MEDS")
	MERGE OUTPROGRESS=@REF@("MEDS-PROGRESS")
GMEDDN ;
	QUIT RESULT;
	;
	;"--------------------------------------------------------------------------------""
	;
SAVEMEDS(SESSIONID,DATA,PROGRESS,ERR)  ;"Save data
	NEW RESULT SET RESULT=$$SAVECOMMON("MEDS",SESSIONID,.DATA,.PROGRESS,ERR)
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
	NEW RESULT SET RESULT=$$SAVECOMMON("SIG1",SESSIONID,.IMAGEB64,.PROGRESS,ERR)
	QUIT RESULT
	;
	;
	;"================================================================================"
	;"  NODE-STYLE RPC FOR GETTING/SAVING CONSENT INFO
	;"================================================================================"
	;
GETCSNT(OUTDATA,OUTPROGRESS,SESSIONID,ERR) ;"Get consent data for patient"
	NEW RESULT SET RESULT=$$GETCOMMON("CSNT",.OUTDATA,.OUTPROGRESS,.SESSIONID,.ERR) ;"Get consent data for patient"
	QUIT RESULT
	;"--------------------------------------------------------------------------------""
SAVECSNT(SESSIONID,DATA,PROGRESS,ERR) ;"Get consent data for patient"
	NEW RESULT SET RESULT=$$SAVECOMMON("CSNT",.SESSIONID,.DATA,.PROGRESS,.ERR)
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
	NEW REF SET REF=$$ID2DATAREF^TMGNODE1(SESSIONID)
	IF REF="" DO  GOTO GCDN
	. SET RESULT=-1
	. SET ERR="Error finding patient data from session ID"
	NEW PROGRESSNODE SET PROGRESSNODE=NODE_"-PROGRESS"
	MERGE OUTDATA=@REF@(NODE)
	MERGE OUTPROGRESS=@REF@(PROGRESSNODE)
GCDN ;
	QUIT RESULT
	;
SAVECOMMON(NODE,SESSIONID,DATA,PROGRESS,ERR)  ;common code for saving elements
	NEW RESULT SET RESULT="1^OK"
	SET ERR=""  ;"default to no error"
	NEW REF SET REF=$$ID2DATAREF^TMGNODE1(SESSIONID)
	IF REF="" DO  GOTO SCDN
	. SET RESULT=-1
	. SET ERR="Error finding patient data from session ID"
	NEW PROGRESSNODE SET PROGRESSNODE=NODE_"-PROGRESS"
	KILL @REF@(NODE),@REF@(PROGRESSNODE) ; Clear existing data
	MERGE @REF@(NODE)=DATA
	MERGE @REF@(PROGRESSNODE)=PROGRESS
SCDN ;
	QUIT RESULT
	;