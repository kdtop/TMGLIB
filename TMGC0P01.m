TMGC0P01   ; TMG - Web Service main entry points;  1/21/17, 3/24/21 
        ;;1.0;TMG E LAB ORDERG;;1/21/17
	;
	; Modifications/Additions copyright 1/21/17 Kevin Toppenberg.	
        ; Licensed under the Apache License, Version 2.0 (the "License");
	; Parts of code copied, as noted below.  Original code licensed as below
	;
        ;****NOTE: Below copied from C0PCPRS1,C0PMAIN,C0PSUB,C0PWS1****
        ;
        ;   Copyright 2009 George Lilly.
        ;   Licensed under the Apache License, Version 2.0 (the "License");
        ;   you may not use this file except in compliance with the License.
        ;   You may obtain a copy of the License at
        ;
        ;       http://www.apache.org/licenses/LICENSE-2.0
        ;
        ;   Unless required by applicable law or agreed to in writing, software
        ;   distributed under the License is distributed on an "AS IS" BASIS,
        ;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
        ;   See the License for the specific language governing permissions and
        ;   limitations under the License.
        Q
        ;
EN(RTNXML,RTNURL,C0PDUZ,C0PDFN,TID,C0PVOR,WALGY,SUBTYPE)  ; ELAB Entry PEP ; Public
        ;COPIED FROM EN^C0PMAIN
        ;Input: RTNXML -- PASSED BY NAME
        ;       RTNURL -- TODO: What's RTNURL used for? it's not referenced in the rest of the routine.
        ;       C0PDUZ -- user IEN to get data for
        ;       COPDFN -- Patient IEN to get data for
        ;       TID -- OPTIONAL. Input options: "ORDER", "STAFF", "NURSE" ,"MIDLEVEL"
        ;       C0PVOR -- IS A VARIABLE OVERRIDE ARRAY WITH IS APPLIED BEFORE MAPPING
        ;       WALGY -- IF 1, INCLUDE ALLERGIES (only relevent if SUBTYPE='ERX')
	;       SUBTYPE -- Type of suscription (e.g. ELAB, WS)
        ; RETURNS THE XML PORTION OF THE RPC RESPONSE
        ;
        ; SERVIEN is Service IEN in Subfile C0P in file 200 (.01 FIELD)
        N C0PTYPE,C0PROLE,C0PPAGE,SERVIEN,C0PVARS,C0PACCT
        ;INIT SUBSCRIBER VARIABLES -- Line below set C0PTYPE,C0PROLE,C0PPAGE in global scope
        SET SERVIEN=$$SETACCT^TMGC0P02("C0PVARS",C0PDUZ,SUBTYPE) 
        I +SERVIEN'>0 Q  ; PERSON NOT SUBSCRIBED
        I '$D(TID) D  ; SET TEMPLATE ACCORDING TO USER TYPE
        . I C0PROLE="D" S TID="LABORDER" ; DEFAULT FOR PRESCRIBER
        . I C0PROLE="M" S TID="STAFF" ; DEFAULT FOR MANAGER
        . I C0PROLE="A" S TID="STAFF" ; DEFAULT FOR ADMIN
        . I C0PROLE="N" S TID="NURSE" ; DEFAULT FOR MIDLEVEL
        . I C0PTYPE="M" S TID="MIDLEVEL" ; DEFAULT FOR MIDLEVEL
        . I C0PTYPE="P" S TID="LABORDER" ; OVERRIDE FOR PRESCRIBERS
        . I '$D(TID) S TID="LABORDER" ; ALL OTHERS
        I TID="STAFF" S WALGY=0 ; DON'T SEND ALLERGIES WITH STAFF TEMPLATE
        N UTID ;TID TO USE
        I +TID=0 D  ; IF A TEMPLATE NAME WAS PASSED INSTEAD OF AN IEN
        . S UTID=$$RESTID(C0PDUZ,TID,SUBTYPE) ;RESOLVE TEMPLATE IEN FROM NAME
        E  S UTID=TID ;
        D ENSUB^TMGC0P02("C0PVARS",C0PDUZ,SUBTYPE) ;INITIALIZE SUBSCRIBER VARIABLES
        I TID="MIDLEVEL" D  ; FOR MIDLEVELS
        . I $G(C0PRMODE)="" Q  ; NOT RENEWAL MODE
        . ; IN RENEWAL MODE, THE SUPERVISING DOCTOR IS FOUND IN C0PSUPERV
        . ;N G
        . I $G(C0PSPRV)="" S C0PSPRV=$G(C0PVARS("SUPERVISING-DOCTOR-DUZ"))
        . I C0PSPRV="" Q  ; SUPERVISING DOCTOR IS NOT SET FOR THIS MIDLEVEL
        . D EN^C0PSUB("G",$G(C0PSPRV)) ; GET VARS FOR SUPERVISOR
        . S C0PVARS("SUPERVISING-NPI")=$G(G("SUBSCRIBER-NPI"))
        . S C0PVARS("SUPERVISING-DEA")=$G(G("SUBSCRIBER-DEA"))
        . S C0PVARS("SUPERVISING-SID")=$G(G("SUBSCRIBER-SID"))
        . S C0PVARS("SUPERVISING-FAMILY-NAME")=$G(G("SUBCRIBER-FAMILY-NAME"))
        . S C0PVARS("SUPERVISING-GIVEN-NAME")=$G(G("SUBCRIBER-GIVEN-NAME"))
        . S C0PVARS("SUPERVISING-LICENSE")=$G(G("SUBSCRIBER-LICENSE"))
        . S C0PVARS("SUPERVISING-LICENSE-STATE")=$G(G("SUBSCRIBER-LICENSE-STATE"))
        . ;K G
        I $D(C0PDFN) DO
        . D EN^C0PPAT("C0PVARS",C0PDFN) ;INITIALIZE PATIENT VARIABLES
        . DO GETINS^TMGC0P02("C0PVARS",C0PDFN)  ;GET INSURANCE AND INSURED INFORMATION
        I $G(C0PVOR)'="" M C0PVARS=@C0PVOR ; VARIABLE OVERRIDES APPLIED HERE
        N C0PXP ; NEW XPATH ARRAY
        D BIND^C0PMAIN("C0PXP","C0PVARS",UTID) ; BIND TO VARIABLES
        N ZZZXML S ZZZXML=RTNXML ; SYMBOL TABLE PROBLEMS
        K @RTNXML ; MAKE SURE WE HAVE A CLEAN SLATE
        D MAP^C0PMAIN(ZZZXML,"C0PXP",UTID) ; MAP VARIABLE TO TEMPLATE
        I TID="MIDLEVEL" D  ; FOR MIDLEVELS
        . I $G(C0PRMODE)=1 Q  ; IN RENEWAL MODE
        . D DELETE^C0CXPATH(ZZZXML,"//NCScript/SupervisingDoctor") ;only for rew
        I $G(WALGY)=1 D  ; ADD ALLERGIES AND SENDMEDS FOR CLICKTHROUGH
        . D ADDALGY^C0PALGY3(ZZZXML,C0PDUZ,C0PDFN) ;ADD ALLERGIES
        . N ZSMEDS ; SEND MEDS
        . D FREETXT^C0PSMEDS("ZSMEDS",C0PDUZ,C0PDFN) ; GET MEDS TO SEND
        . I +$D(ZSMEDS)'=0 D ADD^C0PSMEDS(ZZZXML,"ZSMEDS") ; ADD TO NCSCRIPT
        N TRIMI,J,DONE S DONE=0
        F TRIMI=0:0 D  Q:DONE  ; DELETE UNTIL ALL EMPTY ELEMENTS ARE GONE
        . S J=$$TRIM^C0CXPATH(RTNXML) ; DELETE EMPTY ELEMENTS
        . I DEBUG W "TRIMMED",J,!
        . I J=0 S DONE=1 ; DONE WHEN TRIM RETURNS FALSE
        DO FIXBADID(RTNXML)  ;//kt added
        K @RTNXML@(0) ;GET RID OF LINE COUNT
        Q
        ;
FIXBADID(REF)  ;
        NEW IDX SET IDX=0
        NEW DODEL SET DODEL=0
        NEW ENDDEL SET ENDDEL="XXX"
        FOR  SET IDX=$ORDER(@REF@(IDX)) QUIT:IDX'>0  DO
        . NEW LINE SET LINE=$$TRIM^XLFSTR($GET(@REF@(IDX)))
        . IF DODEL,LINE[ENDDEL DO  QUIT
        . . SET DODEL=0
        . . KILL @REF@(IDX)
        . IF (LINE="")!(DODEL=1) KILL @REF@(IDX) QUIT
        . IF (LINE["ID="""">")!(LINE["id="""">") DO  QUIT
        . . SET ENDDEL="/"_$P($P(LINE,"<",2)," ",1)
        . . SET DODEL=1
        . . KILL @REF@(IDX)
        QUIT
        ;
SETUP(SUBTYPE) ;INITIALIZE SERVICE AND ACCOUNT VARIABLE
        I '$D(C0PDUZ) S C0PDUZ=DUZ ; smh per gpl on 5/3/2012
        N SERVIEN S SERVIEN=$$SUBINIT(C0PDUZ,SUBTYPE)
        I SERVIEN="" D ERROR^C0PMAIN(",U113059001,",$ST($ST,"PLACE"),"WS-NOSUB","Provider Not Subscribed") Q  ;
        S C0PACCT=$$GET1^DIQ($$F200C0P^C0PMAIN(),SERVIEN_","_C0PDUZ_",",1,"I")
        S C0PLOC=+$GET(^TMP("VEFAC0P1",$J,DUZ))  ;"VEFA ADDED
        I C0PLOC'>0 S C0PLOC=$$GET1^DIQ($$F200C0P^C0PMAIN(),SERVIEN_","_C0PDUZ_",",2,"I") ;"VEFA ADDED "IF"
        S C0PWS=$$GET1^DIQ($$ACCOUNTF^C0PMAIN(),C0PACCT_",",4,"I") ; WEB SERVICE POINTER
        Q SERVIEN
        ;
SUBINIT(C0PDUZ,SUBTYPE) ; GET SUBSCRIPTIONS MULTIPLE SUB-IEN IN NEW PERSON
        ;**NOTE**: Copied from SUBINIT^C0PSUB and modified
        S C0PAF=113059002 ; FILE NUMBER FOR ACCOUNT FILE
        S C0PSUBF=200.113059 ; SUBFILE NUMBER OF C0P SUBSCRIPTION MULTIPLE
        S C0PSIEN=+$O(^VA(200,C0PDUZ,"C0P","B",SUBTYPE,"")) ; ERX SUBFILE IEN
        Q C0PSIEN
        ;
RESTID(C0PDUZ,C0PTID,SUBTYPE)   ; RESOLVE TEMPLATE ID FROM SUBSCRIPTION
        ;**NOTE**: copied from RESTID^C0PWS1 and modified
        N C0PAIEN S C0PAIEN=$$SUBINIT(C0PDUZ,SUBTYPE) ;IEN OF SUBSCRIPTION
        N C0PACCT S C0PACCT=$$GET1^DIQ(C0PSUBF,C0PAIEN_","_C0PDUZ_",",1,"I") ;ACCT
        N C0PWBS S C0PWBS=$$GET1^DIQ(C0PAF,C0PACCT_",",4,"I") ;WEB SERVICE IEN
        N C0PUTID S C0PUTID=$$GETTID^C0PWS1(C0PWBS,C0PTID) ;TEMPLATE ID
        Q C0PUTID
        ;
ELRTEST() ;"GENERATE SAMPLE XML PAYLOAD TO LAUNCH E-LAB ORDERING
        NEW OUT DO ELABRPC(.OUT,263,346)
        IF $DATA(OUT) ZWR OUT
        QUIT
        ;
ELABRPC(RTN,IDUZ,IDFN) ; RPC CALL TO RETURN HTTPS POST ARRAY FOR LAB ORDERING [TMG C0P ELAB ORDER RPC]
        ;**NOTE**: Copied from ERXRPC^C0PCPRS1 and modified
        NEW SUBTYPE SET SUBTYPE="ELAB"
        N C0PXML,C0PURL
        D EN("C0PXML","C0PURL",IDUZ,IDFN,,,0,SUBTYPE) ;INCLUDE FREEFORM ALLERGIES
        D WRAP^TMGC0P02(.RTN,.C0PXML,0,SUBTYPE) ; WRAP IN HTML FOR PROCESSING IN CPRS
        Q
        ;
ELABRPC2(RTN,TMGDFN,SDT,EDT)  ; RPC CALL TO RETURN LIST IF IMAGE FILE ENTRIES. [TMG CPRS GET LABIMAGE INFO]
        ;"INPUT:  RTN -- an OUT PARAMETER.  Format: 
        ;"               RTN(0)="1^OK"
        ;"               RTN(#)=1^IEN2005^image FMDT^filename^FMDTInExternalFormat  <-- DT is as found in IMAGE file
        ;"               RTN(#)=1B^IEN2005^image FMDT^filename^FMDTInExternalFormat  <-- 1B means more images for same message as prev "1" node
        ;"               RTN(#)=2^labDT^IEN2005^filename^FMDTInExternalFormat       <-- DT is as found in LAB DATA file
        ;"        TMGDFN -- patient IEN in PATIENT file
        ;"        SDT -- OPTIONAL.  default is 0, start date of range to return
        ;"        EDT -- OPTIONAL.  dfault is 9999999, end date of range to return
        ;"Results: none
        SET SDT=+$GET(SDT) NEW RSDT SET RSDT=9999999.9999-SDT
        SET EDT=+$GET(EDT,9999999) NEW REDT SET REDT=9999999.9999-EDT
        SET TMGDFN=+$GET(TMGDFN)
        SET RTN(0)="1^OK"
        NEW IDX SET IDX=1
        NEW ARDT SET ARDT=REDT-0.000001
        IF 1=0 FOR  SET ARDT=$ORDER(^MAG(2005,"APPXDT",TMGDFN,"LAB IMAGE",ARDT)) QUIT:(+ARDT'>0)!(ARDT>RSDT)  DO
        . NEW IEN2005 SET IEN2005=0
        . FOR  SET IEN2005=$ORDER(^MAG(2005,"APPXDT",TMGDFN,"LAB IMAGE",ARDT,IEN2005)) QUIT:+IEN2005'>0  DO
        . . NEW FNAME SET FNAME=$PIECE($GET(^MAG(2005,IEN2005,0)),"^",2) ;"0;2=FILEREF
        . . NEW FMDT SET FMDT=$PIECE($GET(^MAG(2005,IEN2005,2)),"^",5)  ;"2;5=PROCEDURE/EXAM DATE/TIME
        . . SET RTN(IDX)="1^"_IEN2005_"^"_FMDT_"^"_FNAME_"^"_$$FMTE^XLFDT(FMDT),IDX=IDX+1
        NEW MSGIDX SET MSGIDX=0
        FOR  SET MSGIDX=$ORDER(^TMG(22735,TMGDFN,"MSG",MSGIDX)) QUIT:+MSGIDX'>0  DO
        . NEW FIRST SET FIRST=1
        . NEW IEN2005 SET IEN2005=0
        . FOR  SET IEN2005=$ORDER(^TMG(22735,TMGDFN,"MSG",MSGIDX,1,"B",IEN2005)) QUIT:+IEN2005'>0  DO
        . . NEW FNAME SET FNAME=$PIECE($GET(^MAG(2005,IEN2005,0)),"^",2) ;"0;2=FILEREF
        . . NEW FMDT SET FMDT=$PIECE($GET(^MAG(2005,IEN2005,2)),"^",5)  ;"2;5=PROCEDURE/EXAM DATE/TIME
        . . IF (FMDT<SDT)!(FMDT>EDT) QUIT
        . . NEW NODEID SET NODEID=$SELECT(FIRST:"1",1:"1B") 
        . . SET RTN(IDX)=NODEID_"^"_IEN2005_"^"_FMDT_"^"_FNAME_"^"_$$FMTE^XLFDT(FMDT),IDX=IDX+1
        . . SET FIRST=0
        NEW LRDFN SET LRDFN=+$GET(^DPT(TMGDFN,"LR"))
        SET ARDT=RSDT-0.000001
        FOR  SET ARDT=$ORDER(^LR(LRDFN,"CH",ARDT)) QUIT:(+ARDT'>0)!(ARDT>RSDT)  DO
        . NEW LABDT SET LABDT=9999999-ARDT
        . NEW TN SET TN=$GET(^LR(LRDFN,"CH",ARDT,"TMG"))
        . NEW IEN2005 SET IEN2005=+TN QUIT:IEN2005'>0
        . NEW FMDT SET FMDT=$PIECE($GET(^MAG(2005,IEN2005,2)),"^",5)  ;"2;5=PROCEDURE/EXAM DATE/TIME
        . NEW FNAME SET FNAME=$PIECE($GET(^MAG(2005,IEN2005,0)),"^",2) ;"0;2=FILEREF
        . SET RTN(IDX)="2^"_LABDT_"^"_FMDT_"^"_IEN2005_"^"_FNAME_"^"_$$FMTE^XLFDT(FMDT),IDX=IDX+1        
        QUIT
        ;
ELABPULL(IDUZ,FMDT,PROCMODE)  ;RPC TO PULL BACK LAB RESULTS
        ;"Copied and modified from ERXPULL^C0PCPRS1 and GETMEDS^C0PRECON
        ;" Retreives lab results from WebService
        ;"Input: IDUZ -- IEN IN 200.  Note: if user doesn't have C0P SUSBCRIPTION, will cause crash.  
        ;"       FMDT -- Fileman DateTime for messages to pull
        ;"       PROCMODE -- OPTIONAL.
        ;"            PROCMODE("PDF")=1 (default) if PDF should be generated for result
        ;"            PROCMODE("HL7")=1 (default) if HL7 message should be processed
        ;"            PROCMODE("ERR")=1 (default) if errors should generate alerts
        ;"Result: #processed^OK, or -1^ErrorMessage
        NEW SUBTYPE SET SUBTYPE="ELAB"
        NEW TMGLABS,TMGARR,TMGRESULT
        NEW TMGVARS SET TMGVARS("REPORT-DATE")=17000000+FMDT\1
        ;SOAP call for WS lab results
        SET TMGRESULT=$$SOAP^TMGC0P03("TMGLABS","TMG-GETLABS",IDUZ,0,"TMGVARS",SUBTYPE,"TMGARR") 
        IF +TMGRESULT'=1 GOTO ELPDN
        SET TMGRESULT=0
        NEW REF SET REF=$NAME(TMGLABS("GetLabResultsResponse","GetLabResultsResult","labResultDetailArray"))
        NEW ONELAB SET ONELAB=""
        FOR  SET ONELAB=$ORDER(@REF@(ONELAB)) QUIT:ONELAB=""  DO
        . NEW ARR MERGE ARR=@REF@(ONELAB)
        . NEW HL7MSG SET HL7MSG=$TRANSLATE($GET(ARR("HL7Message")),"%","")
        . IF HL7MSG'="" SET HL7MSG=$GET(@HL7MSG)
        . NEW HTMLMSG SET HTMLMSG=$TRANSLATE($GET(ARR("HtmlMessage")),"%","")
        . IF HTMLMSG'="" SET HTMLMSG=$GET(@HTMLMSG)
        . NEW TEMP SET TEMP=$$PROCESS1^TMGC0P04(.ARR,HL7MSG,HTMLMSG,.PROCMODE)
        . ;"NOTE: I will ignore results because PROCESS1 will send alert for each error
        . IF TEMP'>0 QUIT
        . SET TMGRESULT=+TMGRESULT+TEMP_"^OK"
ELPDN   QUIT TMGRESULT
         ;
ELABPULD(FMDT,PROCMODE,ERRARR) ;"E-LAB PULL FOR ALL ELIGABLE PROVIDERS BY DATE
	;"Input: FMDT -- Fileman DateTime for messages to pull
	;"       PROCMODE -- Pass by REFERENCE.  Optional.  See ELABPULL
	;"       ERRARR -- pass by REFERENCE.  Optional.  An OUT PARAMETER.  Filled with error messages	
	;"Result: #processed
	NEW TMGRESULT SET TMGRESULT=0
	NEW AUSER SET AUSER=0
	;"User must have ELAB subscription in C0P SUBSCRIPTION field, as shown by XRef
	;"CORRECTION: Change Health returns ALL results, regardless of user, so
	;"        I will only check until some results are returned, which will
	;"        mean that ALL results have been returned for date
	FOR  SET AUSER=$ORDER(^VA(200,"C0P","ELAB",AUSER)) QUIT:(AUSER'>0)!(TMGRESULT>0)  DO
	. NEW TEMP SET TEMP=$$ELABPULL(AUSER,FMDT,.PROCMODE) 
	. IF TEMP<0 DO
	. . NEW IDX SET IDX=+$ORDER(ERRARR(""),-1)+1
	. . SET ERRARR(IDX)=$PIECE(TEMP,"^",2,99)
	. IF TEMP>0 SET TMGRESULT=TMGRESULT+TEMP	
	QUIT TMGRESULT
	;
ELABPULT ;"E-LAB PULL FOR ALL ELIGABLE PROVIDERS FOR TODAY
	NEW ERRARR,FMDT
	SET FMDT=$$NOW^XLFDT\1
	WRITE !,"For date=",$$FMTE^XLFDT(FMDT),", "
	NEW TEMP SET TEMP=$$ELABPULD(FMDT,,.ERRARR)
	WRITE "processed ",+TEMP," results.",!
	IF $DATA(ERRARR) WRITE "Error messages",! ZWR ERRARR(*)
	QUIT
	;
ELABPULY ;"E-LAB PULL FOR ALL ELIGABLE PROVIDERS FOR YESTERDAY
	NEW X,X1,X2,%H 
	SET X1=$$NOW^XLFDT,X2=-1
	DO C^%DTC  ;"Substract 1 day, OUTPUT IN X
	NEW ERRARR
	WRITE !,"For date=",$$FMTE^XLFDT(X\1),", "
	NEW TEMP SET TEMP=$$ELABPULD(X\1,,.ERRARR)
	WRITE "processed ",+TEMP," results.",!
	IF $DATA(ERRARR) WRITE "Error messages",! ZWR ERRARR(*)
	QUIT
	;
	;"=============================
	;"======= TESTS BELOW =========
	;"=============================
ELABTEST  ;"PULL TEST DATA FROM DEMO SYSTEM FOR USER-SPECIFIED DATE
	NEW PROCMODE
	SET PROCMODE("PDF")=1
	SET PROCMODE("HL7")=1
	SET PROCMODE("ERR")=1
	NEW Y,%DT SET %DT="AEP",%DT("A")="Enter date for pulling labs: ",%=1
	NEW % SET %=1
	FOR  QUIT:%'=1  DO
	. DO ^%DT WRITE !
	. IF Y=-1 SET %=-1 QUIT
	. NEW TMP SET TMP=$$ELABPULD(Y,.PROCMODE)
	. WRITE "Processed ",+TMP," results.",!
	. WRITE "Pick another date?" SET %=2 DO YN^DICN WRITE !
	QUIT
	;
ELABT2  ;"PULL TEST DATA FROM DEMO SYSTEM, FINDING MOST RECENT RESULT
	WRITE "Finding most recent results...",!
	NEW PROCMODE,Y SET Y=$$NOW^XLFDT\1
	SET PROCMODE("PDF")=1 
	SET PROCMODE("HL7")=1
	SET PROCMODE("ERR")=1
	NEW TMP SET TMP=0
	FOR  QUIT:+TMP'=0  DO
	. WRITE "For date=",$$FMTE^XLFDT(Y),", "
	. NEW ERRARR
	. SET TMP=$$ELABPULD(Y,.PROCMODE,.ERRARR)
	. WRITE "processed ",+TMP," results.",!
	. IF $DATA(ERRARR) WRITE "Error messages",! ZWR ERRARR(*)
	. IF +TMP=0 SET Y=$$FMADD^XLFDT(Y,-1)
	QUIT
	;