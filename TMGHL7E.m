TMGHL7E ;TMG/kst-HL7 Processing Error/Alert handling; 10/27/15
              ;;1.0;TMG-LIB;**1**;4/3/11
 ;
 ;"TMG HL7 Error/Alert handling for lab messages
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
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"HNDLERR2 -- Handler for alert created during POC filing system.
 ;"SETALERT(ERRTEXT,AMSG,IEN772,IEN773) --set up alerts for error handling of POC filer problems. 
 ;"
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"TMGSTUTL, all the HL*, LA* code that the HL processing path normally calls.
 ;"=======================================================================
 ;
 ;"==============================================================
SETALERT(ERRTEXT,AMSG,IEN772,IEN773) ;
        ;"Purpose: Set up alerts for error handling of POC filer problems.
        ;"Input: ERRTEXT -- Text of error.
        ;"       AMSG -- Additional message, IF any.
        ;"       IEN772
        ;"       IEN773
        ;"Results: NONE:
        ;"Output: An alert is created. 
        ;"Restore original message
        NEW NOWH SET NOWH=$H
        SET ERRTEXT=$GET(ERRTEXT)
        IF (ERRTEXT["^")&(+ERRTEXT>0) SET ERRTEXT=$PIECE(ERRTEXT,"^",2,99)
        SET AMSG=$GET(AMSG)
        KILL MSGSTORE ;"Not needed, and clutters variable table.
        NEW PTNOTFOUND SET PTNOTFOUND=$$PTNOTFOUND^TMGHL7E(ERRTEXT) 
        IF PTNOTFOUND,$$IGNORPT(ERRTEXT) GOTO SA2DN         
        NEW TMGERROR SET TMGERROR="[POC ERR]: "_ERRTEXT
        IF AMSG'="" SET TMGERROR=TMGERROR_"; POC MESSAGE]: "_AMSG
        NEW ZEF DO CHECKLONGZEF^TMGHL71(.TMGMSG,.ZEF) ;"Remove any long ZEF segment from TMGMSG, which is an embedded file, causing problems. 
        IF $DATA(ZEF) DO   ;"CODE COPIED FROM ZEF2^TMGHL73
        . NEW IDX SET IDX=0
        . FOR  SET IDX=$ORDER(ZEF(IDX)) QUIT:IDX'>0  DO
        . . NEW TMGARR,LONG
        . . SET LONG=$GET(ZEF(IDX)) QUIT:LONG=""
        . . DO DATAWRAP^TMGBINF(LONG,"TMGARR",60) ;"Cut data string into array. (NOTE : size MUST be multiple of 4)
        . . MERGE TMGMSG(IDX,"PDF")=TMGARR
        . KILL ZEF
        NEW VTABLE 
        ZSHOW "V":VTABLE        
        MERGE ^TMG("TMP","TMGHL73",$J,NOWH,"ERROR VARS")=VTABLE("V")
        KILL VTABLE
        ;"MAKE AN ALERT WITH ERROR MESSAGE.
        NEW XQA,XQAARCH,XQADATA,XQAFLG,XQAGUID,XQAID,XQAOPT,XQAROU,XQASUPV,XQASURO,XQATEXT
        SET XQA("LA7V IPL")=""
        SET XQA(168)=""   ;"//to Kevin Toppenberg (?)
        SET XQADATA=$J_"^"_NOWH_"^"_IEN772_"^"_IEN773
        SET XQAID="TMG-HL7"
        SET XQAROU="HNDLERR^TMGHL7E"
        SET XQAMSG=$$ERRLABEL()
        SET ^TMG("TMP","TMGHL73",$J,NOWH,"ERROR")=TMGERROR
        SET ^TMG("TMP","TMGHL73","$H",NOWH,$J)=""
        NEW TEMP SET TEMP=$$SETUP1^XQALERT
        IF +TEMP=1 DO
        . SET ^TMG("TMP","TMGHL73",$J,NOWH,"ALERT ID")=TEMP
        . DO TMGLOG^HLCSTCP1("Alert generated")  ;"//kt
        ELSE  DO
        . DO TMGLOG^HLCSTCP1("Error creating Alert: "_$GET(XQALERR))  ;"//kt
SA2DN   QUIT
        ;
ERRLABEL()  ;
        QUIT "Error during POC lab filer process."
  ;"---------------------------------------------------------------------
  ;"---------------------------------------------------------------------
IDXDATA ;
        KILL ^TMG("TMP","TMGHL73","$H")
        NEW JN SET JN=0  
        FOR  SET JN=$ORDER(^TMG("TMP","TMGHL73",JN)) QUIT:+JN'>0  DO
        . NEW DH SET DH=0
        . FOR  SET DH=$ORDER(^TMG("TMP","TMGHL73",JN,DH)) QUIT:+DH'>0  DO
        . . SET ^TMG("TMP","TMGHL73","$H",DH,JN)=""
        QUIT
        ;
KOLDDATA ;"KILL old data, older than 1 month
        DO IDXDATA
        NEW HCUTOFF SET HCUTOFF=+$H-30
        NEW DH SET DH=0
        FOR  SET DH=$ORDER(^TMG("TMP","TMGHL73","$H",DH)) QUIT:+DH'>0  DO
        . NEW JN SET JN=0
        . FOR  SET JN=$ORDER(^TMG("TMP","TMGHL73","$H",DH,JN)) QUIT:+JN'>0  DO
        . . IF ($DATA(^TMG("TMP","TMGHL73",JN,DH,"ERROR VARS"))=0)!(+DH<HCUTOFF) DO
        . . . KILL ^TMG("TMP","TMGHL73",JN,DH)
        . . . KILL ^TMG("TMP","TMGHL73","$H",DH,JN)
        QUIT
  ;"---------------------------------------------------------------------
  ;"---------------------------------------------------------------------
        ;
HNDLERR2 ;  
HNDLERR ;
        ;"Purpose -- Handler for alert created during POC filing system.
        ;"Input: Globally scoped variable: XQADATA will hold $J^$H^ien772^ien773
        ;"       ^TMG("TMP","TMGHL73",$J,$H,"VARS") holds variable table at start of transform
        ;"       ^TMG("TMP","TMGHL73",$J,$H,"ERROR VARS") holds variable table at time of error.
        ;"       ^TMG("TMP","TMGHL73",$J,$H,"ERROR")=POC error message.
        ;"       ^TMG("TMP","TMGHL73",$J,$H,"ALERT ID")=The alert handle/ID
        ;"       ^TMG("TMP","TMGHL71",$J,"ZZLOG") holds a log of run.
        NEW TMGUSERINPUT,TMGMNU,IEN22720,TMPERR,TMGIDX,TMGENV
        SET XQADATA=$GET(XQADATA)
        NEW TMGJOBN SET TMGJOBN=+$PIECE(XQADATA,"^",1)
        NEW TMGTIME SET TMGTIME=$PIECE(XQADATA,"^",2)
        NEW IEN772 SET IEN772=$PIECE(XQADATA,"^",3) IF IEN772="" SET IEN772=-1
        NEW IEN773 SET IEN773=$PIECE(XQADATA,"^",4) IF IEN773="" SET IEN773=-1
        NEW INDENTN SET INDENTN=0
        NEW TMGTESTMSG,TMGHL7MSG,TMGU,TEMPRESULT
        WRITE !,!,"Job that had transform problem was: ",TMGJOBN,!
        DO LOADXQMSG(TMGJOBN,TMGTIME,.TMGTESTMSG,.TMGHL7MSG,.TMGU,.IEN22720)
        IF $DATA(TMGTESTMSG)=0,IEN773>0 DO
        . WRITE "Loading HL7 Message header stored in file# 773, record #",IEN773,!
        . WRITE "Loading HL7 Message text stored in file# 772, record #",IEN772,!
        . DO LOAD772^TMGHL7S(IEN773,.TMGTESTMSG,.TMGHL7MSG,.TMGU)
        NEW OPTION DO LOADOPTION(TMGJOBN,TMGTIME,.OPTION)
        NEW TMGERROR SET TMGERROR=$GET(^TMG("TMP","TMGHL73",TMGJOBN,TMGTIME,"ERROR"))
        IF TMGERROR["^" SET TMGERROR=$PIECE(TMGERROR,"^",2,99)
        IF TMGERROR["Missing CPT: File# 71" GOTO HNDLERR^TMGHL7E2
        NEW PTNOTFOUND SET PTNOTFOUND=(TMGERROR["Patient not found in system:")
        IF TMGERROR="" DO  ;"ADDED TO CHECK FOR IGNORED PATIENT WITH BLANK ERROR
        . NEW PTINFO SET PTINFO=$$GETPTINO2(.TMGHL7MSG)
        . SET TMGERROR="BLANK ERROR. : "_PTINFO
        . IF $$IGNORPT(TMGERROR) DO
        . . SET PTNOTFOUND=1
        . ELSE  DO
        . . SET TMGERROR=""
        IF TMGERROR["Unable to find DFN" DO
        . NEW PTINFO SET PTINFO=$$GETPTINO2(.TMGHL7MSG) 
        . SET TMGERROR=TMGERROR_": "_PTINFO
        . SET PTNOTFOUND=1
        IF PTNOTFOUND,$$IGNORPT(TMGERROR) DO CLEANUP2(TMGJOBN,TMGTIME,1) GOTO HE2DN
        ;"IF (IEN772=0)&(IEN773=0) NEW SKIP SET SKIP=1 DO  GOTO:SKIP=1 HE2DN
        IF $DATA(TMGTESTMSG)=0 NEW SKIP SET SKIP=1 DO  GOTO:SKIP=1 HE2DN
        . WRITE "Insufficient information available to handle this alert.",!
        . IF TMGERROR'="" WRITE "Message was: "_TMGERROR,!
        . NEW % SET %=1 WRITE "Delete" DO YN^DICN WRITE !
        . IF %=2 SET SKIP=0 QUIT        
        . KILL ^TMG("TMP","TMGHL73",TMGJOBN,TMGTIME)        
        NEW TMGRESULT SET TMGRESULT=$$SETUPENV^TMGHL7U(.TMGTESTMSG,.TMGENV,1)
        SET TMGENV("IEN 772")=IEN772 SET TMGENV("IEN 773")=IEN773
        IF TMGRESULT'>0 DO  GOTO HE2DN
        . WRITE !,"Unable to SET up environment for processing HL7 POC Lab error.",!
        . WRITE "Message was: ",$PIECE(TMGRESULT,"^",2),!
        . WRITE "Please fix error, and then try reprocessing this alert.",!
        . DO PRESS2GO^TMGUSRI2
        NEW DIRNAME,FNAME SET (DIRNAME,FNAME)=""
        SET TMGENV("INTERACTIVE MODE")=1
        IF TMGERROR="" SET TMGUSERINPUT="TryAgain" GOTO M3
        IF (TMGERROR["The value")&(TMGERROR["for field PATIENT in") SET TMGUSERINPUT="TryAgain" GOTO M3
        ;
M2      KILL TMGUSERINPUT,TMGMNU,TMPERR
        KILL TMGMNUI SET TMGMNUI=0
        SET TMGMNU(TMGMNUI)="Handle HL7 POC Filing error for $JOB "_TMGJOBN_" @ "_TMGTIME
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
        SET TMGMNU(TMGMNUI)="Check mapping and address errors"_$CHAR(9)_"TestMap",TMGMNUI=TMGMNUI+1
        IF $DATA(TMGTESTMSG) DO 
        . SET TMGMNU(TMGMNUI)="View HL7 Message"_$CHAR(9)_"ViewMsg",TMGMNUI=TMGMNUI+1
        ;"SET TMGMNU(TMGMNUI)="View Log of transform process"_$CHAR(9)_"ViewLog",TMGMNUI=TMGMNUI+1
        ;"SET TMGMNU(TMGMNUI)="<Setup Lab Test Menu>"_$CHAR(9)_"SetupTest",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="Setup MENU"_$CHAR(9)_"SetupTest",TMGMNUI=TMGMNUI+1
        ;"SET TMGMNU(TMGMNUI)="<Setup Message Transform Menu>"_$CHAR(9)_"XForm",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="Handle Invalid field Value"_$CHAR(9)_"InvalidValue",TMGMNUI=TMGMNUI+1
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
M3      IF TMGUSERINPUT="ViewLog" DO HESHOWLOG(+TMGJOBN) GOTO M2
        IF TMGUSERINPUT="ViewMsg" DO VIEWMSG^TMGHL7U2(.TMGTESTMSG) GOTO M2
        IF TMGUSERINPUT="TestMap" DO TESTMAP^TMGHL70A(.TMGENV,.TMGTESTMSG,.TMGHL7MSG) WRITE !,! GOTO M2  
        IF TMGUSERINPUT="HL7FileMenu" DO FILEMENU^TMGHL70(.TMGTESTMSG,INDENTN+2) WRITE !,! GOTO M2  
        IF TMGUSERINPUT="TryAgain" IF +$$TRYAGAN2(.TMGTESTMSG,0,.TMGENV,.OPTION)=1 GOTO HE2DN
        IF TMGUSERINPUT="TryAgainDebugger" IF $$TRYAGAN2(.TMGTESTMSG,1,.TMGENV,.OPTION)=1 GOTO HE2DN
        IF TMGUSERINPUT="InvalidValue" DO INVAL(IEN22720,.TMGERROR,INDENTN+2) GOTO M2
        IF TMGUSERINPUT="SetupTest" DO HESUTST2(.TMGENV,.TMGTESTMSG,INDENTN+2) GOTO M2 
        IF TMGUSERINPUT="SearchPt" NEW TMGSRCH SET TMGSRCH=0 DO  GOTO HE2DN:TMGSRCH=1,M2
        . NEW TEMP,TMGHNDLERR
        . IF $$FINDPT(.TEMP,.TMGHL7MSG)=0 QUIT
        . SET TMGHNDLERR("SSN")=$GET(TEMP("SSN"))
        . SET TMGHNDLERR("DFN")=$GET(TEMP("DFN"))
        . SET TMGSRCH=$$TRYAGAN2(.TMGTESTMSG,0,.TMGENV,.OPTION)
        IF TMGUSERINPUT="IgnorePt" DO  GOTO M2:(TEMPRESULT'=1),HE2DN
        . SET TEMPRESULT=$$ADDIGNOR(.TMGENV,TMGERROR,INDENTN+2)
        . IF TEMPRESULT'=1 QUIT
        . DO CLEANUP2(TMGJOBN,TMGTIME,1)
        ;"IF TMGUSERINPUT="XForm" DO HXFMSUT2(.TMGENV,.TMGTESTMSG,INDENTN) GOTO M2   
        ;
        IF TMGUSERINPUT=0 SET TMGUSERINPUT=""
        IF TMGUSERINPUT'="^" GOTO M2
        IF $$TRYAGAN2(.TMGTESTMSG,0,.TMGENV,.OPTION)'=1 DO
        . DO CLEANUP2(TMGJOBN,TMGTIME) 
HE2DN   WRITE "Quitting.  Goodbye",!
        KILL TMGHNDLERR  ;"just in case it was set here. 
        KILL TMGCLEANED
        QUIT
        ;
CLEANUP2(TMGJOBN,TMGTIME,FORCE)  ;
        ;"11/15/19 ADDED FORCE
        SET FORCE=+$G(FORCE)
        IF FORCE=1 DO  QUIT
        . KILL ^TMG("TMP","TMGHL73",TMGJOBN,TMGTIME)
        . SET TMGCLEANED=1
        IF $GET(TMGCLEANED)=1 QUIT
        NEW % SET %=1
        WRITE "Clear stored memory relating to this alert (can't be undone)"
        DO YN^DICN WRITE !
        IF %=1 DO
        . KILL ^TMG("TMP","TMGHL73",TMGJOBN,TMGTIME)
        . SET TMGCLEANED=1
        QUIT
        ;
FINDPT(OUT,TMGHL7MSG) ;
        ;"FINISH
        NEW PTINFO DO GETPTINO(.PTINFO,.TMGHL7MSG) 
        NEW PTARRAY DO GETPTLST(.PTINFO,.PTARRAY)
        NEW MAXNUM SET MAXNUM=$O(PTARRAY(99999),-1)
M4
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
        IF ((+$G(TMGPTINPUT)<1)!(+$G(TMGPTINPUT)>MAXNUM))&(TMGPTINPUT'="MANUALSRCH")&(TMGPTINPUT'="^") GOTO M4
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
        NEW LNAME SET LNAME=$GET(PTINFO(5,1))
        NEW FNAME SET FNAME=$GET(PTINFO(5,2))
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
        IF $DATA(PTARRAY)>0 GOTO GPLFIN
        ;"Current match criteria last name and first name
        SET NAME=LNAME
        FOR  SET NAME=$O(^DPT("B",NAME)) QUIT:(NAME'[LNAME)!(NAME="")  DO
        . SET DFN=0
        . FOR  SET DFN=$O(^DPT("B",NAME,DFN)) QUIT:DFN'>0  DO
        . . NEW ZN SET ZN=$G(^DPT(DFN,0))
        . . NEW THISNAME SET THISNAME=$PIECE(ZN,"^",1)
        . . NEW THISFNAME SET THISFNAME=$PIECE(THISNAME,",",2)
        . . IF THISFNAME'[FNAME QUIT
        . . SET PTMATCHARRAY(DFN)=""        
GPLFIN  SET DFN=0
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
GETPTINO2(TMGHL7MSG) ;
        NEW TMGRESULT SET TMGRESULT=""
        NEW PIDSEG SET PIDSEG=+$ORDER(TMGHL7MSG("B","PID",0))
        IF PIDSEG'>0 QUIT TMGRESULT
        NEW HL7DOB SET HL7DOB=$GET(TMGHL7MSG(PIDSEG,7))
        NEW FMDT SET FMDT=$$HL72FMDT^TMGHL7U3(HL7DOB)
        NEW DATE SET DATE=$$FMTE^XLFDT(FMDT,"2DZ")
        NEW M,D,Y SET M=$P(DATE,"/",1),D=$P(DATE,"/",2),Y=$P(DATE,"/",3)
        IF $L(M)=1 SET M="0"_M
        IF $L(D)=1 SET D="0"_D
        IF $L(Y)=1 SET Y="0"_Y
        SET DATE=M_"-"_D_"-"_Y
        SET TMGRESULT=$G(TMGHL7MSG(PIDSEG,5,1))_","_$G(TMGHL7MSG(PIDSEG,5,2))_" ("_DATE_")"
        QUIT TMGRESULT
        ;        
TRYAGAN2(TMGMSG,DEBUG,TMGENV,OPTION) ;
        ;"Purpose: Try processing again, using debugger to walk through code.
        ;"Input: TMGMSG -- Array holding HL7 message.  
        ;"       DEBUG -- OPTIONAL.  If 1, then code is launched through debugger.
        ;"       TMGENV -- Required environment.  
        ;"       OPTION -- OPTIONAL var to pass to HLMSGIMPORT()
        ;"Result: 1 if OK, or -1^Abort IF aborted. 
        NEW TMGRESULT SET TMGRESULT=1
        NEW % SET %=1
        WRITE "Send HL7 message through POC filer again" SET %=+$$YNA^TMGUSRI2(%) WRITE !
        IF %'=1 DO  GOTO DBDN2  ;"DEBG2DN
        . SET TMGRESULT="-1^HL7 Message Filing Aborted"        
        NEW CODE SET CODE="SET TMGRESULT=$$HLMSGIMPORT^TMGHL71(.TMGMSG,1,.OPTION,.TMGENV)"
        IF +$GET(DEBUG)=1 DO
        . DO DIRDEBUG^TMGIDE(CODE)
        ELSE  DO
        . XECUTE CODE
        IF TMGRESULT<0 GOTO DEBG2DN
        WRITE "-------------------",!
        WRITE "Done with filer.  Processing seems to have been without problems.",!
        SET %=1 WRITE !,"Move HL7 message file to SUCCESS folder" DO YN^DICN WRITE !
        IF %=1 DO
        . NEW OPT2 MERGE OPT2=OPTION
        . NEW FNAME SET FNAME=$GET(OPTION("FAILURE STORE FNAME"))
        . NEW FPATH SET FPATH=$GET(OPTION("FAILURE STORE FPATH"))
        . SET OPT2("FILEPATHNAME")=FPATH_FNAME
        . NEW TEMPRESULT SET TEMPRESULT=$$MOVE^TMGHL71(1,.OPT2)
        . IF TEMPRESULT>0 QUIT
        . NEW TEMPRESULT2 SET TEMPRESULT2=$$MOVE^TMGHL71(1,.OPTION)
        . IF TEMPRESULT2>0 QUIT
        . WRITE $PIECE(TEMPRESULT,"^",2),!
        . WRITE $PIECE(TEMPRESULT2,"^",2),!
DEBG2DN IF TMGRESULT<0 DO
        . NEW ERR SET ERR=$PIECE(TMGRESULT,"^",2,99)
        . NEW IGNORE SET IGNORE=0
        . WRITE !,ERR,!
        . IF $$PTNOTFOUND(ERR) DO  QUIT:IGNORE
        . . SET ERR=$PIECE(ERR,".",1)
        . . NEW NOIGNORE SET NOIGNORE=0
        . . IF $$IGNORPT(ERR)=0 DO  QUIT:NOIGNORE
        . . . SET %=1 WRITE "Would you like to ignore this patient" DO YN^DICN WRITE !
        . . . IF %=1 DO
        . . . . DO ADDIGNOR(.TMGMSG,.ERR,0)
        . . . . SET TMGRESULT=1  ;"12/1/19 This needs to be tested.
        . . . ELSE  DO
        . . . . SET NOIGNORE=1
        . . DO CLEANUP2(TMGJOBN,TMGTIME,1) 
        . . SET IGNORE=1
        . SET %=2 WRITE "Create a NEW alert for this NEW error" DO YN^DICN WRITE !
        . IF %'=1 QUIT
        . NEW IEN772,IEN773
        . SET IEN772=$GET(TMGENV("IEN 772")),IEN773=$GET(TMGENV("IEN 773"))
        . DO SETALERT(TMGRESULT,,IEN772,IEN773) 
        . WRITE "Alert has been created.  Exit this handler and select NEW alert to process.",!        
        DO PRESS2GO^TMGUSRI2
DBDN2   QUIT TMGRESULT
        ;        
PTNOTFOUND(ERR) ;"Determine if error message is that patient was not found in system
        NEW TMGRESULT SET TMGRESULT=0
        IF ERR["Patient not found in system" SET TMGRESULT=1
        IF ERR["Unable to find DFN" SET TMGRESULT=1
        QUIT TMGRESULT
        ;"
HESUTST2(TMGENV,TMGMSG,INDENTN) ;
        ;"Purpose: launch menu to edit and setup tests.
        DO SETUP^TMGHL70(.TMGMSG,INDENTN)        
        QUIT        
        ;
RESTRVR2(TMGJOBN,TMGTIME) ;
        ;"Results: 1 if OK, -1^Message IF error
        ;"Purpose: restore variables to system table.
        QUIT $$RESTRREF($NAME(^TMG("TMP","TMGHL73",TMGJOBN,TMGTIME,"ERROR VARS")))
        ;
HXFMSUT2(TMGENV,TMGMSG,INDENTN) ;
        ;"Purpose: launch menu to edit and setup tests.
        DO SETUP^TMGHL7S(.TMGENV,.TMGMSG,INDENTN+2)
        QUIT        
        ;
INVAL(IEN22720,TMGERROR,INDENTN) ;
        ;"TO-DO.  TMGRESULT WILL NEED TO BE FORMATTED SUCH THAT SUMFRERR CAN HANDLE SITUATION.
        DO SUMFRERR^TMGHL7S(IEN22720,TMGERROR,.INDENTN)
        SET INDENTN=+$GET(INDENTN)
        NEW INDENTSTR SET INDENTSTR=$JUSTIFY("",INDENTN)
        WRITE !
        WRITE INDENTSTR,"This process just addressed the 1 issue.   It is recommended",!
        WRITE INDENTSTR,"that one keep selecting 'Test the HL7 message' until all",!
        WRITE INDENTSTR,"problems are resolved.",!
        WRITE INDENTSTR DO PRESS2GO^TMGUSRI2
        QUIT
        ;        
IGNORPT(PATSTR)  ;"Determine if patient should be ignored.
        NEW TMGIEN SET TMGIEN=0
        NEW TMGRESULT SET TMGRESULT=0
        SET PATSTR=$$GIGNRNAM(PATSTR)
        ;"
        ;"START NEW CODE
        NEW TEMPPT,PATNAME SET (TEMPPT,PATNAME)=$P(PATSTR," ",1)
        NEW FMDOB SET FMDOB=$P($P(PATSTR,"(",2),")",1)
        SET FMDOB=$$INTDATE^TMGDATE(FMDOB)
        IF FMDOB'>0 GOTO IGNDN
        NEW FOUND SET FOUND=0
        FOR  SET TEMPPT=$O(^TMG(22717.5,"B",TEMPPT)) QUIT:(TEMPPT'[PATNAME)!(FOUND=1)  DO
        . NEW THISDOB SET THISDOB=$P($P(TEMPPT,"(",2),")",1)
        . SET THISDOB=$$INTDATE^TMGDATE(THISDOB)
        . IF THISDOB=FMDOB DO
        . . SET TMGIEN=+$ORDER(^TMG(22717.5,"B",TEMPPT,""))
        . . SET FOUND=1
        ;"END NEW CODE
        ;"
        ;"SET TMGIEN=+$ORDER(^TMG(22717.5,"B",$EXTRACT(PATSTR,1,30),""))
        IF TMGIEN'>0 GOTO IGNDN
        NEW UNTILDT SET UNTILDT=$PIECE($GET(^TMG(22717.5,TMGIEN,0)),"^",2)
        IF UNTILDT>$$NOW^XLFDT DO  SET TMGRESULT=1 GOTO IGNDN
        . WRITE !,"Patient: ",PATSTR," has been set to be ignored until ",$$FMTE^XLFDT(UNTILDT),!
        . WRITE "NOTE: this can be edited in file 22717.5.",!
IGNDN   QUIT TMGRESULT
        ;
GIGNRNAM(PATSTR) ;"Get extracted patient name and DOB 
        SET PATSTR=$$TRIM^XLFSTR($PIECE(PATSTR,":",2))
        SET PATSTR=$PIECE(PATSTR,")",1)_")"
        QUIT PATSTR
        ;
ADDIGNOR(TMGENV,TMGERROR,INDENTN)  ;"ADD PATIENT TO IGNORE LIST.
        ;"Result: 1 if patient ignored, or -1 otherwise
        NEW TMGRESULT SET TMGRESULT=-1
        NEW PATSTR SET PATSTR=$$GIGNRNAM(TMGERROR)
        WRITE !,PATSTR,!
        NEW TMGFDA,TMGIEN,TMGMSG
        SET TMGIEN=+$ORDER(^TMG(22717.5,"B",$EXTRACT(PATSTR,1,30),""))
        IF TMGIEN>0 DO  GOTO AGN2
        . WRITE !,"Patient has been ignored before...",!
        . NEW IGNDT SET IGNDT=$PIECE($GET(^TMG(22717.5,TMGIEN,0)),"^",2)
        . WRITE "Set to be ignored until: ",$$FMTE^XLFDT(IGNDT),!
        SET TMGFDA(22717.5,"+1,",.01)=PATSTR
        DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERROR")) DO  GOTO AGNDN
        . WRITE "Error: ",$$GETERRST^TMGDEBU2(.TMGMSG),!
        SET TMGIEN=+$GET(TMGIEN(1))
        IF TMGIEN'>0 DO  GOTO AGNDN
        . WRITE "Error: Unable to determine IEN of newly added records in 22717.5",!
AGN2    NEW %DT,X,Y
        WRITE "UNTIL DATE: (T+90)// "
        READ X:($GET(DTIME,3600))
        IF X="" SET X="T+90"
        DO ^%DT
        IF Y>0 WRITE $$FMTE^XLFDT(Y),!
        ELSE  WRITE "--> Date not saved.",! GOTO AGNDN
        KILL TMGFDA
        SET TMGFDA(22717.5,TMGIEN_",",.02)=Y
        ;"DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
        DO FILE^DIE("","TMGFDA","TMGMSG")
        IF $DATA(TMGMSG("DIERROR")) DO  GOTO AGNDN
        . WRITE "Error: ",$$GETERRST^TMGDEBU2(.TMGMSG),!
        WRITE "Patient to be ignored until ",$$FMTE^XLFDT(Y),!
        SET TMGRESULT=1  ;"//SUCCESS
AGNDN   QUIT TMGRESULT        
        ;"
IGNORRPC(TMGRESULT,TMGDFN)  ;"RPC to add patient to ignore list for 90 days
        SET TMGRESULT="1^SUCCESS"
        NEW FIRST SET FIRST=1
        NEW PATSTR SET PATSTR=$P($G(^DPT(TMGDFN,0)),"^",1)  ;"NAME
        NEW DOB,DOBSTR SET DOB=$P($G(^DPT(TMGDFN,0)),"^",3)  ;"DOB
        NEW YEAR SET YEAR=$E(DOB,1,3)+1800
        SET DOBSTR=$E(DOB,4,5)_"-"_$E(DOB,6,7)_"-"_$E(YEAR,3,4)
        SET PATSTR=PATSTR_" ("_DOBSTR_")"
        WRITE PATSTR
        NEW TMGFDA,TMGIEN,TMGMSG
        SET TMGIEN=+$ORDER(^TMG(22717.5,"B",PATSTR,""))
        IF TMGIEN>0 DO  GOTO AGN3
        . SET FIRST=0
        . ;"NEW IGNDT SET IGNDT=$PIECE($GET(^TMG(22717.5,TMGIEN,0)),"^",2)
        SET TMGFDA(22717.5,"+1,",.01)=PATSTR
        DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERROR")) DO  GOTO IGRPCDN
        . SET TMGRESULT="-1^Error: "_$$GETERRST^TMGDEBU2(.TMGMSG)
        SET TMGIEN=+$GET(TMGIEN(1))
        IF TMGIEN'>0 DO  GOTO IGRPCDN
        . SET TMGRESULT="-1^Error: Unable to determine IEN of newly added records in 22717.5"
AGN3    NEW %DT,X,Y
        SET X="T+90"
        DO ^%DT
        KILL TMGFDA
        SET TMGFDA(22717.5,TMGIEN_",",.02)=Y
        DO FILE^DIE("","TMGFDA","TMGMSG")
        IF $DATA(TMGMSG("DIERROR")) DO  GOTO IGRPCDN
        . SET TMGRESULT="-1^Error: "_$$GETERRST^TMGDEBU2(.TMGMSG)
        IF FIRST=1 DO
        . SET TMGRESULT="1^PATIENT WILL BE IGNORED UNTIL "_$$FMTE^XLFDT(Y)
        ELSE  DO
        . SET TMGRESULT="1^PATIENT WAS PREVIOUSLY IGNORED. IGNORE SET TO "_$$FMTE^XLFDT(Y)
IGRPCDN
        QUIT
        ;"
LOADXQMSG(TMGJOBN,TMGTIME,TMGMSG,TMGHL7MSG,TMGU,IEN22720) ;
        ;"Purpose: to load a message saved during alert creation
        ;"Input: TMGJOBN -- $J of error
        ;"       TMGTIME -- $H of error
        ;"       TMGMSG -- PASS BY REFERENCE. AN OUT PARAMETER. 
        ;"       TMGHL7MSG -- PASS BY REFERENCE. AN OUT PARAMETER. 
        ;"       TMGU -- PASS BY REFERENCE. AN OUT PARAMETER. 
        ;"       IEN22720 -- PASS BY REFERENCE. AN OUT PARAMETER.
        ;"Input: Globally scoped variable: XQADATA will hold $J^$H^ien772^ien773
        ;"       ^TMG("TMP","TMGHL73",TMGJOBN,TMGTIME,"VARS") holds variable table at start of transform
        ;"       ^TMG("TMP","TMGHL73",TMGJOBN,TMGTIME,"ERROR VARS") holds variable table at time of error.
        ;"       ^TMG("TMP","TMGHL73",TMGJOBN,TMGTIME,"ERROR")=POC error message.
        ;"       ^TMG("TMP","TMGHL73",TMGJOBN,TMGTIME,"ALERT ID")=The alert handle/ID
        ;"       ^TMG("TMP","TMGHL71",TMGJOBN,"ZZLOG") holds a log of run.           
        ;"Accesses IEN22720 by global scope (but not required)
        ;"Result: NONE
        NEW TMGRESULT SET TMGRESULT="-1^OK"
        NEW EVARS
        MERGE EVARS=^TMG("TMP","TMGHL73",TMGJOBN,TMGTIME,"ERROR VARS")
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(EVARS(IDX)) QUIT:IDX'>0  DO
        . NEW LINE SET LINE=$GET(EVARS(IDX)) QUIT:LINE=""
        . IF $PIECE(LINE,"(",1)'="TMGMSG" QUIT
        . SET @LINE
        SET IDX=0
        FOR  SET IDX=$ORDER(TMGMSG(IDX)) QUIT:IDX'>0  DO
        . IF $DATA(TMGMSG(IDX,"PDF"))=0 QUIT
        . NEW LONG SET LONG=""
        . NEW JDX SET JDX=0
        . FOR  SET JDX=$ORDER(TMGMSG(IDX,"PDF",JDX)) QUIT:JDX'>0  DO
        . . SET LONG=LONG_$GET(TMGMSG(IDX,"PDF",JDX))
        . KILL TMGMSG(IDX,"PDF")
        . SET TMGMSG(IDX)=LONG
        IF $DATA(TMGMSG)=0 DO  GOTO LXMDN
        . SET TMGRESULT="-1^Unable to extract HL7 message from saved data"
        SET TMGRESULT=$$SETUPENV^TMGHL7U(.TMGMSG,.TMGENV) 
        IF TMGRESULT<0 GOTO LXMDN
        SET IEN22720=$GET(TMGENV("IEN 22720"))
        MERGE TMGU=TMGENV("TMGU")
        SET TMGRESULT=$$PRSEARRY^TMGHL7X2(IEN22720,.TMGMSG,.TMGHL7MSG,.TMGU) ;
LXMDN   ;        
        IF TMGRESULT<0 DO
        . WRITE $PIECE(TMGRESULT,"^",2),!
        . DO PRESS2GO^TMGUSRI2                          
        QUIT
        ;   
LOADOPTION(TMGJOBN,TMGTIME,OPTION) ;
        ;"Purpose: to load a message saved during alert creating
        ;"Input: TMGJOBN -- $J of error
        ;"       TMGTIME -- $H of error
        ;"       OPTION -- AN OUT PARAMETER
        ;"Input: Globally scoped variable: XQADATA will hold $J^$H^ien772^ien773
        ;"       ^TMG("TMP","TMGHL73",TMGJOBN,TMGTIME,"VARS") holds variable table at start of transform
        ;"       ^TMG("TMP","TMGHL73",TMGJOBN,TMGTIME,"ERROR VARS") holds variable table at time of error.
        ;"       ^TMG("TMP","TMGHL73",TMGJOBN,TMGTIME,"ERROR")=POC error message.
        ;"       ^TMG("TMP","TMGHL73",TMGJOBN,TMGTIME,"ALERT ID")=The alert handle/ID
        ;"       ^TMG("TMP","TMGHL71",TMGJOBN,"ZZLOG") holds a log of run.           
        ;"Accesses IEN22720 by global scope (but not required)
        ;"Result: none
        NEW EVARS MERGE EVARS=^TMG("TMP","TMGHL73",TMGJOBN,TMGTIME,"ERROR VARS")
        NEW LOCALOPTION
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(EVARS(IDX)) QUIT:IDX'>0  DO
        . NEW LINE SET LINE=$GET(EVARS(IDX)) QUIT:LINE=""
        . NEW P1 SET P1="^"_$PIECE(LINE,"(",1)_"^"
        . IF "^LOCALOPTION^OPTION^"[P1 DO        
        . . SET @LINE
        MERGE OPTION=LOCALOPTION
        QUIT
        ;   
  ;"---------------------------------------------------------------------
  ;"---------------------------------------------------------------------
  ;        
HESHOWLOG(TMGJOBN) ;
        ;
        NEW I SET I=""
        FOR  SET I=$ORDER(^TMG("TMP","TMGHL71",TMGJOBN,"ZZLOG",I)) QUIT:(I="")  DO
        . WRITE I,".  ",$GET(^TMG("TMP","TMGHL71",TMGJOBN,"ZZLOG",I)),!
        DO PRESS2GO^TMGUSRI2
        QUIT
        ;
RESTRREF(zzREF) ;
        ;"Results: 1 if OK, -1^Message IF error
        ;"Purpose: restore variables to system table.
        NEW zv,zzi,zo
        NEW zresult SET zresult=1
        IF $DATA(@zzREF)=0 DO  GOTO RRFVDN
        . SET zresult="-1^No variable table found for at reference: '"_zzREF_"'"
        KILL zv MERGE zv=@zzREF
        FOR zzi=1:1 SET zo=$GET(zv(zzi)) QUIT:(zo="")  DO
        . IF $EXTRACT(zo,1,3)="tmg" QUIT
        . SET @zo
RRFVDN  IF +zresult=-1 DO
        . WRITE $PIECE(zresult,"^",2),!
        . DO PRESS2GO^TMGUSRI2
        QUIT zresult
        ;
