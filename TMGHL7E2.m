TMGHL7E2 ;TMG/kst-HL7 Processing Error/Alert handling; 11/18/16
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
  ;"SETALRT(ERRTEXT,AMSG) --set up alerts for error handling of Rad filer problems. 
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
SETALERT(ERRTEXT,AMSG) ;
  ;"Purpose: Set up alerts for error handling of Rad filer problems.
  ;"NOTE: called from CREATE^LA7LOG
  ;"Input: ERRTEXT -- Text of error.
  ;"       AMSG -- Additional message, IF any.
  ;"NOTE: uses some variable in global scope:
  ;"          HLMTIEN (which is really an IEN772), HLMTIENS (an IEN773)
  ;"          TMGHL7MSG
  ;"Results: NONE:
  ;"Output: An alert is created. 
  ;"Restore original message
  NEW NOWH SET NOWH=$H
  SET ERRTEXT=$GET(ERRTEXT)
  IF (ERRTEXT["^")&(+ERRTEXT>0) SET ERRTEXT=$PIECE(ERRTEXT,"^",2,99)
  SET AMSG=$GET(AMSG)
  KILL MSGSTORE ;"Not needed, and clutters variable table.
  NEW PTNOTFOUND SET PTNOTFOUND=(ERRTEXT["Patient not found in system:")
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
  SET XQA(168)=""   ;"//to Kevin Toppenberg
  SET XQADATA=$J_"^"_NOWH_"^"_HLMTIEN_"^"_HLMTIENS
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
  NEW PTNOTFOUND SET PTNOTFOUND=(TMGERROR["Patient not found in system:")
  IF PTNOTFOUND,$$IGNORPT^TMGHL7E(TMGERROR) DO CLEANUP2(TMGJOBN,TMGTIME) GOTO HE2DN
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
  . WRITE "Please fix error, and then try reprocessing this alert."
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
  IF PTNOTFOUND SET TMGMNU(TMGMNUI)="Ignore missing patient"_$CHAR(9)_"IgnorePt",TMGMNUI=TMGMNUI+1
  ;
  SET TMGUSERINPUT=$$MENU^TMGUSRI2(.TMGMNU,"^")
  KILL TMGMNU ;"Prevent from cluttering variable table during debug run
  ;
  IF TMGUSERINPUT="ViewLog" DO HESHOWLOG^TMGHL7E(+TMGJOBN) GOTO M2
  IF TMGUSERINPUT="ViewMsg" DO VIEWMSG^TMGHL7U2(.TMGTESTMSG) GOTO M2
  IF TMGUSERINPUT="HL7FileMenu" DO FILEMENU^TMGHL70(.TMGTESTMSG,INDENTN+2) WRITE !,! GOTO M2  
  IF TMGUSERINPUT="FixCPT" DO FIXCPT(TMGERROR,INDENTN+2) WRITE !,! GOTO M2  
  IF TMGUSERINPUT="TryAgain" IF +$$TRYAGAN2(.TMGTESTMSG,0)=1 GOTO HE2DN
  IF TMGUSERINPUT="TryAgainDebugger" IF $$TRYAGAN2(.TMGTESTMSG,1)=1 GOTO HE2DN
  IF TMGUSERINPUT="IgnorePt" DO  GOTO M2:(TEMPRESULT'=1),HE2DN
  . SET TEMPRESULT=$$ADDIGNOR^TMGHL7E(.TMGENV,TMGERROR,INDENTN+2)
  . IF TEMPRESULT'=1 QUIT
  . DO CLEANUP2(TMGJOBN,TMGTIME)
  ;
  IF TMGUSERINPUT=0 SET TMGUSERINPUT=""
  IF TMGUSERINPUT'="^" GOTO M2
  IF $$TRYAGAN2(.TMGTESTMSG,0)'=1 DO
  . DO CLEANUP2(TMGJOBN,TMGTIME) 
HE2DN  ;
  WRITE "Quitting.  Goodbye",!
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
TRYAGAN2(TMGMSG,DEBUG) ;
        ;"Purpose: Try processing again, using debugger to walk through code.
        ;"Input: TMGMSG -- Array holding HL7 message.  
        ;"       DEBUG -- OPTIONAL.  If 1, then code is launched through debugger. 
        ;"Result: 1 if OK, or -1^Abort IF aborted. 
        NEW TMGRESULT SET TMGRESULT=1
        NEW % SET %=1
        WRITE "Send HL7 message through Rad filer again" DO YN^DICN WRITE !
        IF %'=1 DO  GOTO DBDN2  ;"DEBG2DN
        . SET TMGRESULT="-1^HL7 Message Filing Aborted"        
        NEW CODE SET CODE="SET TMGRESULT=$$HL7MSGIN^TMGHL71(.TMGMSG,1)"
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
        . DO SETALERT(TMGRESULT)
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
        WRITE !,"Below is current record",!
        DO DUMPREC^TMGDEBU3(71,IEN71)
        DO PRESS2GO^TMGUSRI2
        NEW DIE SET DIE="^RAMIS(71,"   ;"File #71, RAD/NUC MED PROCEDURES
        NEW DA SET DA=IEN71
        NEW DR SET DR=9  ;"CPT CODE field
        NEW REF SET REF=DIE_DA_")"
        LOCK +@REF:0
        IF '$TEST DO  GOTO FCPDN
        . WRITE "Sorry, another user is editing this entry."
        DO ^DIE
        LOCK -@REF:0
FCPDN   QUIT        
        

  ;"---------------------------------------------------------------------
  ;"---------------------------------------------------------------------        
