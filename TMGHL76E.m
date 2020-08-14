TMGHL76E ;TMG/kst-HL7 Processing Error/Alert handling; 11/18/16
              ;;1.0;TMG-LIB;**1**; 11/18/16
  ;
  ;"TMG HL7 Error/Alert handling for lab messages
  ;
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;"Copyright (c) 11/18/16  Kevin S. Toppenberg MD
  ;"
  ;"This file is part of the TMG LIBRARY, and may only be used in accordence
  ;" to license terms outlined in separate file TMGLICNS.m, which should 
  ;" always be distributed with this file.
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;
  ;"=======================================================================
  ;"=======================================================================
  ;" API -- Public Functions.
  ;"=======================================================================
  ;"HNDLERR -- Handler for alert created during Radiology HL7 filing system.
  ;"SETALRT(ERRTEXT,AMSG,IEN772,IEN773) --set up alerts for error handling of Rad filer problems. 
  ;"
  ;"=======================================================================
  ;" API - Private Functions
  ;"=======================================================================
  ;"=======================================================================
  ;"Dependancies
  ;"=======================================================================
  ;"TMGSTUTL, all the HL*, LA* code that the HL processing path normally calls.
  ;"=======================================================================
  ;"==============================================================
  ;
SETALERT(ERRTEXT,AMSG,IEN772,IEN773) ;
  ;"Purpose: Set up alerts for error handling of Rad filer problems.
  ;"NOTE: called from CREATE^LA7LOG
  ;"Input: ERRTEXT -- Text of error.
  ;"       AMSG -- Additional message, IF any.
  ;"NOTE: uses some variable in global scope:
  ;"          TMGHL7MSG
  ;"Results: NONE:
  ;"Output: An alert is created. 
  ;"Restore original message
  NEW NOWH SET NOWH=$H
  SET ERRTEXT=$GET(ERRTEXT)
  IF (ERRTEXT["^")&(+ERRTEXT>0) SET ERRTEXT=$PIECE(ERRTEXT,"^",2,99)
  SET AMSG=$GET(AMSG)
  KILL MSGSTORE ;"Not needed, and clutters variable table.
  ;"NEW PTNOTFOUND SET PTNOTFOUND=(ERRTEXT["Patient not found in system:")
  NEW PTNOTFOUND SET PTNOTFOUND=$$PTNOTFOUND^TMGHL7E(ERRTEXT) 
  IF PTNOTFOUND,$$IGNORPT^TMGHL7E(ERRTEXT) GOTO SA2DN         
  NEW TMGERROR SET TMGERROR="[RAD ERR]: "_ERRTEXT
  IF AMSG'="" SET TMGERROR=TMGERROR_"; RAD MESSAGE]: "_AMSG
  NEW VTABLE 
  ZSHOW "V":VTABLE        
  MERGE ^TMG("TMP","TMGHL7E2",$J,NOWH,"ERROR VARS")=VTABLE("V")
  KILL VTABLE
  ;"MAKE AN ALERT WITH ERROR MESSAGE.
  NEW XQA,XQAARCH,XQADATA,XQAFLG,XQAGUID,XQAID,XQAOPT,XQAROU,XQASUPV,XQASURO,XQATEXT
  ;"SET XQA("LA7V IPL")=""
  SET XQA(150)=""   ;"//to Eddie Hagood
  ;"SET XQADATA=$J_"^"_NOWH_"^"_HLMTIEN_"^"_HLMTIENS
  SET XQADATA=$J_"^"_NOWH_"^"_IEN772_"^"_IEN773
  SET XQAID="TMG-HL7"
  SET XQAROU="HNDLERR^TMGHL7E2"
  SET XQAMSG=$$ERRLABEL(.TMGHL7MSG)
  SET ^TMG("TMP","TMGHL7E2",$J,NOWH,"ERROR")=TMGERROR
  SET ^TMG("TMP","TMGHL7E2","$H",NOWH,$J)=""
  DO IDXDATA 
  NEW TEMP SET TEMP=$$SETUP1^XQALERT
  IF +TEMP=1 DO
  . SET ^TMG("TMP","TMGHL7E2",$J,NOWH,"ALERT ID")=TEMP
  . DO TMGLOG^HLCSTCP1("Alert generated")  ;"//kt
  ELSE  DO
  . DO TMGLOG^HLCSTCP1("Error creating Alert: "_$GET(XQALERR))  ;"//kt
SA2DN ;
  QUIT
  ;
ERRLABEL(TMGHL7MSG)  ;
  NEW RESULT SET RESULT="Error during Rad filer process."
  NEW NAMEDOB SET NAMEDOB=$$GETNMDOB^TMGHL7U3(.TMGHL7MSG)
  IF NAMEDOB'="" SET RESULT=RESULT_" ["_NAMEDOB_"]"
  QUIT RESULT
  ;"---------------------------------------------------------------------
  ;"---------------------------------------------------------------------
IDXDATA ;
  KILL ^TMG("TMP","TMGHL7E2","$H")
  NEW JN SET JN=0  
  FOR  SET JN=$ORDER(^TMG("TMP","TMGHL7E2",JN)) QUIT:+JN'>0  DO
  . NEW DH SET DH=0
  . FOR  SET DH=$ORDER(^TMG("TMP","TMGHL7E2",JN,DH)) QUIT:+DH'>0  DO
  . . SET ^TMG("TMP","TMGHL7E2","$H",DH,JN)=""
  QUIT
  ;
KOLDDATA ;"KILL old data, older than 1 month
  DO IDXDATA
  NEW HCUTOFF SET HCUTOFF=+$H-30
  NEW DH SET DH=0
  FOR  SET DH=$ORDER(^TMG("TMP","TMGHL7E2","$H",DH)) QUIT:+DH'>0  DO
  . NEW JN SET JN=0
  . FOR  SET JN=$ORDER(^TMG("TMP","TMGHL7E2","$H",DH,JN)) QUIT:+JN'>0  DO
  . . IF ($DATA(^TMG("TMP","TMGHL73",JN,DH,"ERROR VARS"))=0)!(+DH<HCUTOFF) DO
  . . . KILL ^TMG("TMP","TMGHL73",JN,DH)
  . . . KILL ^TMG("TMP","TMGHL73","$H",DH,JN)
  QUIT
  ;"---------------------------------------------------------------------
  ;"---------------------------------------------------------------------
  ;
HNDLERR ;
  ;"Purpose -- Handler for alert created during Rad filing system.
  ;"Input: Globally scoped variable: XQADATA will hold $J^$H^ien772^ien773
  ;"       ^TMG("TMP","TMGHL7E2",$J,$H,"VARS") holds variable table at start of transform
  ;"       ^TMG("TMP","TMGHL7E2",$J,$H,"ERROR VARS") holds variable table at time of error.
  ;"       ^TMG("TMP","TMGHL7E2",$J,$H,"ERROR")=Rad filer error message.
  ;"       ^TMG("TMP","TMGHL7E2",$J,$H,"ALERT ID")=The alert handle/ID
  ;"       ^TMG("TMP","TMGHL71",$J,"ZZLOG") holds a log of run.
  NEW TMGUSERINPUT,TMGMNU,IEN22720,TMPERR,TMGIDX,TMGENV
  SET XQADATA=$GET(XQADATA)
  NEW TMGJOBN SET TMGJOBN=+$PIECE(XQADATA,"^",1)
  NEW TMGTIME SET TMGTIME=$PIECE(XQADATA,"^",2)
  NEW IEN772 SET IEN772=$PIECE(XQADATA,"^",3)
  NEW IEN773 SET IEN773=$PIECE(XQADATA,"^",4)
  NEW INDENTN SET INDENTN=0
  NEW HLMTIEN,HLMTIENS
  SET HLMTIEN=IEN772,HLMTIENS=IEN773
  NEW TMGTESTMSG,TMGHL7MSG,TMGU,TEMPRESULT
  DO LOAD772^TMGHL7S(IEN773,.TMGTESTMSG,.TMGHL7MSG,.TMGU)
  WRITE !,!,"Job that had transform problem was: ",TMGJOBN,!
  WRITE "HL7 Message header stored in file# 773, record #",IEN773,!
  WRITE "HL7 Message text stored in file# 772, record #",IEN772,!
  NEW TMGERROR SET TMGERROR=$GET(^TMG("TMP","TMGHL7E2",TMGJOBN,TMGTIME,"ERROR"))
  IF TMGERROR["^" SET TMGERROR=$PIECE(TMGERROR,"^",2,99)
  NEW PTNOTFOUND SET PTNOTFOUND=$$PTNOTFOUND^TMGHL7E(TMGERROR)  ;" (TMGERROR["Patient not found in system:")
  IF $$PREHANDLE(TMGERROR)=1 GOTO HE2DN
  ;"NEW PTNOTFOUND SET PTNOTFOUND=(TMGERROR["Patient not found in system:")
  ;"IF PTNOTFOUND,$$IGNORPT^TMGHL7E(TMGERROR) DO CLEANUP2(TMGJOBN,TMGTIME) GOTO HE2DN
  IF (IEN772=0)&(IEN773=0) NEW SKIP SET SKIP=1 DO  GOTO:SKIP=1 HE2DN
  . WRITE "Insufficient information available to handle this alert.",!
  . IF TMGERROR'="" WRITE "Message was: "_TMGERROR,!
  . NEW % SET %=1 WRITE "Delete" DO YN^DICN WRITE !
  . IF %=2 SET SKIP=0 QUIT        
  . KILL ^TMG("TMP","TMGHL7E2",TMGJOBN,TMGTIME)   
  NEW TMGRESULT SET TMGRESULT=$$SETUPENV^TMGHL7U(.TMGTESTMSG,.TMGENV,1)        
  IF TMGRESULT'>0 DO  GOTO HE2DN
  . WRITE !,"Unable to SET up environment for processing HL7 RAD error.",!
  . WRITE "Message was: ",$PIECE(TMGRESULT,"^",2),!
  . WRITE "Please fix error, and then try reprocessing this alert.",!
  . DO PRESS2GO^TMGUSRI2
  SET TMGENV("INTERACTIVE MODE")=1
M2 ;
  KILL TMGUSERINPUT,TMGMNU,TMPERR
  KILL TMGMNUI SET TMGMNUI=0
  SET TMGMNU(TMGMNUI)="Handle HL7 Rad Filing error for $JOB "_TMGJOBN_" @ "_TMGTIME
  SET TMGIDX=1
  SET TMGMNU(TMGMNUI,TMGIDX)="IEN 772="_IEN772,TMGIDX=TMGIDX+1
  SET TMGMNU(TMGMNUI,TMGIDX)="IEN 773="_IEN773,TMGIDX=TMGIDX+1
  SET TMPERR="Error: "_$GET(TMGERROR)
  FOR  QUIT:TMPERR=""  DO
  . NEW STR,WIDTH SET WIDTH=60
  . IF $LENGTH(TMPERR)<WIDTH SET STR=TMPERR,TMPERR=""
  . ELSE  DO
  . . SET STR=$EXTRACT(TMPERR,1,WIDTH)_"-"
  . . SET TMPERR=$EXTRACT(TMPERR,WIDTH+1,$LENGTH(TMPERR))
  . IF $EXTRACT(STR,1,6)'="Error:" SET STR="  "_STR
  . SET TMGMNU(TMGMNUI,TMGIDX)=STR,TMGIDX=TMGIDX+1
  SET TMGMNUI=TMGMNUI+1
  ;"-------------------------------------
  IF $DATA(TMGTESTMSG) DO 
  . SET TMGMNU(TMGMNUI)="View HL7 Message"_$CHAR(9)_"ViewMsg",TMGMNUI=TMGMNUI+1
  IF TMGERROR["Missing CPT: File# 71" DO
  . SET TMGMNU(TMGMNUI)="Fix missing CPT"_$CHAR(9)_"FixCPT",TMGMNUI=TMGMNUI+1
  SET TMGMNU(TMGMNUI)="Reprocess HL7 Message"_$CHAR(9)_"TryAgain",TMGMNUI=TMGMNUI+1
  SET TMGMNU(TMGMNUI)="Try reprocessing HL7 Message (using DEBUGGER)"_$CHAR(9)_"TryAgainDebugger",TMGMNUI=TMGMNUI+1
  SET TMGMNU(TMGMNUI)="HL7 Message FILE MENU"_$CHAR(9)_"HL7FileMenu",TMGMNUI=TMGMNUI+1
  IF PTNOTFOUND DO
  . SET TMGMNU(TMGMNUI)="Ignore missing patient"_$CHAR(9)_"IgnorePt",TMGMNUI=TMGMNUI+1
  . SET TMGMNU(TMGMNUI)="Patient Search: Find matching patient"_$CHAR(9)_"SearchPt",TMGMNUI=TMGMNUI+1
  ;
  SET TMGUSERINPUT=$$MENU^TMGUSRI2(.TMGMNU,"^")
  KILL TMGMNU ;"Prevent from cluttering variable table during debug run
  ;
  IF TMGUSERINPUT="ViewLog" DO HESHOWLOG^TMGHL7E(+TMGJOBN) GOTO M2
  IF TMGUSERINPUT="ViewMsg" DO VIEWMSG^TMGHL7U2(.TMGTESTMSG) GOTO M2
  IF TMGUSERINPUT="HL7FileMenu" DO FILEMENU^TMGHL70(.TMGTESTMSG,INDENTN+2) WRITE !,! GOTO M2  
  IF TMGUSERINPUT="FixCPT" DO FIXCPT(TMGERROR,INDENTN+2) WRITE !,! GOTO M2  
  IF TMGUSERINPUT="TryAgain" IF +$$TRYAGAN2(.TMGTESTMSG,0,IEN772,IEN773)=1 GOTO HE2DN
  IF TMGUSERINPUT="TryAgainDebugger" IF $$TRYAGAN2(.TMGTESTMSG,1,IEN772,IEN773)=1 GOTO HE2DN
  IF TMGUSERINPUT="SearchPt" NEW TMGSRCH SET TMGSRCH=0 DO  GOTO HE2DN:TMGSRCH=1,M2
  . NEW TEMP,TMGHNDLERR
  . IF $$FINDPT(.TEMP,.TMGHL7MSG)=0 QUIT
  . SET TMGHNDLERR("SSN")=$GET(TEMP("SSN"))
  . SET TMGHNDLERR("DFN")=$GET(TEMP("DFN"))
  . SET TMGSRCH=$$TRYAGAN2(.TMGTESTMSG,0,IEN772,IEN773)
  IF TMGUSERINPUT="IgnorePt" DO  GOTO M2:(TEMPRESULT'=1),HE2DN
  . SET TEMPRESULT=$$ADDIGNOR^TMGHL7E(.TMGENV,TMGERROR,INDENTN+2)
  . IF TEMPRESULT'=1 QUIT
  . DO CLEANUP2(TMGJOBN,TMGTIME)
  ;
  IF TMGUSERINPUT=0 SET TMGUSERINPUT=""
  IF TMGUSERINPUT'="^" GOTO M2
  IF $$TRYAGAN2(.TMGTESTMSG,0,,IEN772,IEN773)'=1 DO
  . DO CLEANUP2(TMGJOBN,TMGTIME) 
HE2DN  ;
  WRITE "Quitting.  Goodbye",!
  QUIT
  ;
FINDPT(OUT,TMGHL7MSG) ;
        NEW PTINFO DO GETPTINO(.PTINFO,.TMGHL7MSG)
        NEW PTARRAY DO GETPTLST(.PTINFO,.PTARRAY)
        NEW MAXNUM SET MAXNUM=$O(PTARRAY(99999),-1)
M3
        NEW TMGPTINPUT,TMGPTMNU,TMPPTERR,TMGPTMNUI,TMGPTIDX
        SET TMGPTMNUI=0
        SET TMGPTMNU(TMGPTMNUI)="PROVIDED DEMOGRAPHICS AS PROVIDED BY HL7 MESSAGE"
        SET TMGPTIDX=1
        SET TMGPTMNU(TMGPTMNUI,TMGPTIDX)="Provided Name:"_$P($G(PTINFO(5)),"^",1)_","_$P($G(PTINFO(5)),"^",2),TMGPTIDX=TMGPTIDX+1
        SET TMGPTMNU(TMGPTMNUI,TMGPTIDX)="Provided DOB: "_$$EXTDATE^TMGDATE($G(PTINFO("FMDT"))),TMGPTIDX=TMGPTIDX+1
        SET TMGPTMNU(TMGPTMNUI,TMGPTIDX)="Provided Sex: "_$G(PTINFO(8)),TMGPTIDX=TMGPTIDX+1
        SET TMGPTMNU(TMGPTMNUI,TMGPTIDX)="Provided SSN: "_$G(PTINFO(19)),TMGPTIDX=TMGPTIDX+1
        IF MAXNUM>0 DO
        . SET PROMPT="SELECT FROM THE LIST OF POSSIBLE PATIENTS BELOW OR MANUALLY SEARCH"
        ELSE  DO
        . SET PROMPT="NO PATIENTS COULD BE FOUND. PLEASE MANUALLY SEARCH"
        SET PROMPT=""
        SET TMGPTMNU(TMGPTMNUI,TMGPTIDX)=PROMPT,TMGPTIDX=TMGPTIDX+1
        SET TMGPTMNUI=TMGPTMNUI+1
        ;"
        NEW PTIDX SET PTIDX=0
        FOR  SET PTIDX=$O(PTARRAY(PTIDX)) QUIT:PTIDX'>0  DO
        . SET TMGPTMNU(PTIDX)=$G(PTARRAY(PTIDX))_$CHAR(9)_PTIDX
        ;
        SET TMGPTMNU(MAXNUM+1)="MANUAL SEARCH FOR PATIENT"_$CHAR(9)_"MANUALSRCH"
        SET TMGPTINPUT=$$MENU^TMGUSRI2(.TMGPTMNU,"^")
        IF ((+$G(TMGPTINPUT)<1)!(+$G(TMGPTINPUT)>MAXNUM))&(TMGPTINPUT'="MANUALSRCH")&(TMGPTINPUT'="^") GOTO M3
        IF TMGPTINPUT="MANUALSRCH" DO
        . NEW X,Y,DIC,ANSWER
        . SET DIC=2,DIC(0)="MAEQ"
        . D ^DIC
        . WRITE !
        . IF +Y>0 DO
        . . NEW DFN SET DFN=$P(Y,"^",1)
        . . SET OUT("SSN")=$P($G(^DPT(DFN,0)),"^",9)
        . . SET OUT("DFN")=DFN
        . . ;"ZWR OUT
        ELSE  IF TMGPTINPUT'="^" DO
        . SET OUT("SSN")=$P(PTARRAY(TMGPTINPUT),"^",3)
        . SET OUT("DFN")=$P(PTARRAY(TMGPTINPUT),"^",5)
        . ;"ZWR OUT
        KILL TMGPTMNU ;"Prevent from cluttering variable table during debug run
        QUIT
        ;"
GETPTLST(PTINFO,PTARRAY)
        ;"TEST DATA
        ;"SET PTARRAY(1)="ZZTEST,BABY^12/07/18^123456789^F^12345"
        ;"SET PTARRAY(2)="ZZTEST,DEADPOOL^12/07/18^123456789^F^98765"
        NEW IDX SET IDX=1
        NEW PTMATCHARRAY
        ;"GET PT INFO
        NEW LNAME SET LNAME=$P($G(PTINFO(5)),"^",1)
        NEW DOB SET DOB=$G(PTINFO("FMDT"))
        NEW NAME SET NAME=LNAME
        NEW DFN
        ;"Current match criteria is DOB and last name
        FOR  SET NAME=$O(^DPT("B",NAME)) QUIT:(NAME'[LNAME)!(NAME="")  DO
        . SET DFN=0
        . FOR  SET DFN=$O(^DPT("B",NAME,DFN)) QUIT:DFN'>0  DO
        . . NEW THISDOB SET THISDOB=$P($G(^DPT(DFN,0)),"^",3)
        . . IF THISDOB=DOB DO
        . . . SET PTMATCHARRAY(DFN)=""
        SET DFN=0
        FOR  SET DFN=$O(PTMATCHARRAY(DFN)) QUIT:DFN'>0  DO
        . NEW ZN SET ZN=$G(^DPT(DFN,0))
        . SET PTARRAY(IDX)=$P(ZN,"^",1)_"^"_$$EXTDATE^TMGDATE($P(ZN,"^",3))_"^"_$P(ZN,"^",9)_"^"_$P(ZN,"^",2)_"^"_DFN
        . SET IDX=IDX+1
        QUIT
        ;"
GETPTINO(OUT,TMGHL7MSG) ;
        NEW PIDSEG SET PIDSEG=+$ORDER(TMGHL7MSG("B","PID",0))
        IF PIDSEG'>0 QUIT
        MERGE OUT=TMGHL7MSG(PIDSEG)
        NEW HL7DOB SET HL7DOB=$GET(OUT(7))
        NEW FMDT SET FMDT=$$HL72FMDT^TMGHL7U3(HL7DOB)
        SET OUT("FMDT")=FMDT
        QUIT
        ;
CLEANUP2(TMGJOBN,TMGTIME)  ;
        NEW % SET %=1
        WRITE "Clear stored memory relating to this alert (can't be undone)"
        DO YN^DICN WRITE !
        IF %=1 KILL ^TMG("TMP","TMGHL7E2",TMGJOBN,TMGTIME)
        DO KOLDDATA ;"KILL old data, older than 1 month
        QUIT
        ;
TRYAGAN2(TMGMSG,DEBUG,IEN772,IEN773) ;
        ;"Purpose: Try processing again, using debugger to walk through code.
        ;"Input: TMGMSG -- Array holding HL7 message.  
        ;"       DEBUG -- OPTIONAL.  If 1, then code is launched through debugger. 
        ;"Result: 1 if OK, or -1^Abort IF aborted. 
        NEW TMGRESULT SET TMGRESULT=1
        NEW % SET %=1
        WRITE "Send HL7 message through Rad filer again" DO YN^DICN WRITE !
        IF %'=1 DO  GOTO DBDN2  ;"DEBG2DN
        . SET TMGRESULT="-1^HL7 Message Filing Aborted"        
        NEW CODE SET CODE="SET TMGRESULT=$$HLMSGIMPORT^TMGHL71(.TMGMSG,1)"
        IF +$GET(DEBUG)=1 DO
        . DO DIRDEBUG^TMGIDE(CODE)
        ELSE  DO
        . XECUTE CODE
        IF TMGRESULT<0 GOTO DEBG2DN
        WRITE "-------------------",!
        WRITE "Done with filer.  Processing seems to have been without problems.",!
DEBG2DN IF TMGRESULT<0 DO
        . WRITE $PIECE(TMGRESULT,"^",2,99),!
        . SET %=2 WRITE "Create a NEW alert for this NEW error" DO YN^DICN WRITE !
        . IF %'=1 QUIT
        . DO SETALERT(TMGRESULT,,IEN772,IEN773)
        . WRITE "Alert has been created.  Exit this handler and select NEW alert to process.",!        
        DO PRESS2GO^TMGUSRI2
DBDN2   QUIT TMGRESULT
        ;
FIXCPT(TMGERROR,INDENT) ;
        NEW TMP SET TMP=$PIECE($PIECE(TMGERROR,",",2),"#",2)
        NEW IEN71 SET IEN71=+$$TRIM^XLFSTR(TMP)
        IF IEN71'>0 DO  GOTO FCPDN
        . WRITE !,"Unable to extract IEN from message.  Got ["_TMGERROR_"]",!
        . DO PRESS2GO^TMGUSRI2
        ;"Test to see if field 9 has already been fixed
        NEW CURCPT SET CURCPT=$$GET1^DIQ(71,IEN71,9)        
        IF CURCPT'="" DO  GOTO FCPDN
        . WRITE !,"This record seems to have already been fixed and set to ",CURCPT,! 
        . DO PRESS2GO^TMGUSRI2
        WRITE !,"Below is current record",!
        DO DUMPREC^TMGDEBU3(71,IEN71)
        DO PRESS2GO^TMGUSRI2
        NEW TESTNAME SET TESTNAME=$$GET1^DIQ(71,IEN71,.01)
        NEW TEMP SET TEMP=$$SELCPT(TESTNAME)
        NEW CPTIEN SET CPTIEN=+$G(TEMP)
        IF CPTIEN>0 DO
        . NEW TMGFDA,TMGIEN,TMGMSG
        . SET TMGFDA(71,IEN71_",",9)=CPTIEN
        . DO FILE^DIE("E","TMGFDA","TMGMSG")
        . DO SHOWDIER^TMGDEBU2(.TMGMSG)
        ;"
        ;"Delete below code
        ;"NEW DIE SET DIE="^RAMIS(71,"   ;"File #71, RAD/NUC MED PROCEDURES
        ;"NEW DA SET DA=IEN71
        ;"NEW DR SET DR=9  ;"CPT CODE field
        ;"NEW REF SET REF=DIE_DA_")"
        ;"LOCK +@REF:0
        ;"IF '$TEST DO  GOTO FCPDN
        ;". WRITE "Sorry, another user is editing this entry."
        ;"DO ^DIE
        ;"LOCK -@REF:0
FCPDN   QUIT        
        ;
SELCPT(TESTNAME)  ;"interact with user to pick CPT code.   
        ;"INPUT: TESTNAME -- the name of a test that user will search for match for
        ;"Result: IEN_81^CPT Code^Description,  or -1 if aborted or error.  
        NEW SELOPT,Y
        SET SELOPT("NCS")=1 ;"not case sensitive
        SET SELOPT("HEADER",1)="Please selected a record from the CPT CODE file"
        SET SELOPT("HEADER",2)="that matches: {{RED}}"_TESTNAME_"{{NORM}}"
        SET SELOPT("HEADER",3)="{{BOLD}}NOTE: If no match is found, press ^ for chance to add NEW entry{{NORM}}"
        SET SELOPT("COLORS","NORM")="7^4"
        SET SELOPT("COLORS","BOLD")="14^4"
        SET SELOPT("COLORS","RED")="14^1"
        SET SELOPT("COLORS","FOOTER")="14^6"
        SET SELOPT("SCRN WIDTH")=70
        SET SELOPT("SCRN HEIGHT")=20
        SET SELOPT("REC NAME SETUP CODE")="$$SETUPCPTLST^TMGHL7E2(IEN)"
        SET SELOPT("ON CHANGING")="HNDLCPT1^TMGHL7E2"
        SET Y=$$RECSEL^TMGUSRI4(81,,.SELOPT) WRITE #
        IF +Y'>0 GOTO SLCPTDN
        NEW ZN SET ZN=$GET(^ICPT(+Y,0))
        NEW CPT SET CPT=$PIECE(ZN,"^",1)
        NEW DESCR SET DESCR=$PIECE(ZN,"^",2)
        WRITE "Use CPT '",CPT,"' (",DESCR,")",!,"for test '",TESTNAME,"'?"
        NEW % SET %=1 DO YN^DICN WRITE !
        IF %=1 SET Y=+Y_"^"_CPT_"^"_DESCR
        ELSE  SET Y=-1
SLCPTDN QUIT Y
        ;
SETUPCPTLST(IEN)  ;"This is a callback function
        NEW ZN SET ZN=$GET(^ICPT(IEN,0))
        NEW TMGRESULT SET TMGRESULT=$PIECE(ZN,"^",1)_" - "_$PIECE(ZN,"^",2)
        QUIT TMGRESULT
        ;
HNDLCPT1(PARRAY,OPTION,INFO) ;
        ;"Purpose: Handle ON CHANGING event from Scroller, from LISTSEL
        ;"Input: PARRAY -- PASSED BY NAME.  This is array that is displayed.  See SCROLLER^TMGUSRIF for documentation
        ;"       OPTION -- -- PASSED BY REFERENCE.  This is OPTION array.  See SCROLLER^TMGUSRIF for documentation
        ;"       INFO -- PASSED BY REFERENCE.  An Array with releventy information.
        ;"          INFO("CURRENT LINE","NUMBER")=number currently highlighted line
        ;"          INFO("CURRENT LINE","TEXT")=Text of currently highlighted line
        ;"          INFO("CURRENT LINE","RETURN")=return value of currently highlighted line
        ;"    Referenced globally scoped variables --
        ;"        TMGSCLRMSG,TMGRESULT
        ;"Result: NONE
        ;"Output: May affect globally-scoped variable TMGSCLRMSG to communicate back to Scroller
        ;"        May affect globally-scoped variable TMGRESULT
        SET TMGRESULT=$GET(INFO("CURRENT LINE","RETURN"))
        DO VCUSAV2^TMGTERM
        NEW YPOS SET YPOS=$GET(OPTION("SCRN HEIGHT"))+1
        NEW XMAX SET XMAX=$GET(OPTION("SCRN WIDTH"))
        DO CUP^TMGTERM(1,YPOS)
        NEW IEN SET IEN=+$GET(INFO("NEXT LINE","RETURN"))
        NEW LINE SET LINE=$$LJ^XLFSTR("=",XMAX,"=")
        WRITE LINE
        NEW LINECT SET LINECT=0
        NEW ARRAY MERGE ARRAY=^ICPT(IEN,"D") KILL ARRAY(0)
        DO WordWrapArray^TMGSTUTL(.ARRAY,XMAX-1)
        NEW SUBIEN SET SUBIEN=0
        FOR  SET SUBIEN=$ORDER(ARRAY(SUBIEN)) QUIT:(SUBIEN'>0)!(LINECT>10)  DO
        . NEW S SET S=$GET(ARRAY(SUBIEN,0))
        . SET S=$$LJ^XLFSTR(S,XMAX-1)_"|"
        . WRITE !,S
        . SET LINECT=LINECT+1
        FOR  QUIT:LINECT>10  DO
        . SET LINECT=LINECT+1
        . NEW S SET S=$$LJ^XLFSTR(" ",XMAX-1)_"|"
        . WRITE !,S
        WRITE !,LINE
        DO VCULOAD2^TMGTERM
        QUIT
        ;
PREHANDLE(TMGERROR) ;
        ;"RESULT: 1 if handled automatically or 0 otherwise.
        NEW TMGRESULT SET TMGRESULT=0
        NEW PTNOTFOUND SET PTNOTFOUND=$$PTNOTFOUND^TMGHL7E(TMGERROR) 
        ;"NEW PTNOTFOUND SET PTNOTFOUND=(TMGERROR["Patient not found in system:")
        IF PTNOTFOUND,$$IGNORPT^TMGHL7E(TMGERROR) DO
        . DO CLEANUP2(TMGJOBN,TMGTIME) 
        . SET TMGRESULT=1
        IF TMGERROR[" Missing CPT: File# 71" DO
        . ;"TEST HERE IF CPT STILL MISSING (NOT ALREADY FIXED)
        . ;"EXAMPLE ERROR MESSAGE:  Missing CPT: File# 71, record# 707, field# 9, does not contain CPT code.  Please fix.
        . NEW IEN SET IEN=$PIECE(TMGERROR,"record# ",2),IEN=$PIECE(IEN,",",1)
        . NEW FLD SET FLD=$PIECE(TMGERROR,"field# ",2),FLD=$PIECE(FLD,",",1)
        . ;"FINISH...
        QUIT TMGRESULT

