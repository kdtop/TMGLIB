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
 ;"SETALRT2(ERRTEXT,AMSG) --set up alerts for error handling of POC filer problems. 
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
SETALRT2(ERRTEXT,AMSG) ;
        ;"Purpose: Set up alerts for error handling of POC filer problems.
        ;"NOTE: called from CREATE^LA7LOG
        ;"Input: ERRTEXT -- Text of error.
        ;"       AMSG -- Additional message, IF any. 
        ;"NOTE: uses HLMTIEN (which is really an IEN772), HLMTIENS (an IEN773) in global scope
        ;"Results: NONE:
        ;"Output: An alert is created. 
        ;"Restore original message
        NEW NOWH SET NOWH=$H
        SET ERRTEXT=$GET(ERRTEXT)
        IF (ERRTEXT["^")&(+ERRTEXT>0) SET ERRTEXT=$PIECE(ERRTEXT,"^",2,99)
        SET AMSG=$GET(AMSG)
        KILL MSGSTORE ;"Not needed, and clutters variable table.
        NEW PTNOTFOUND SET PTNOTFOUND=(ERRTEXT["Patient not found in system:")
        IF PTNOTFOUND,$$IGNORPT(ERRTEXT) GOTO SA2DN         
        NEW TMGERROR SET TMGERROR="[POC ERR]: "_ERRTEXT
        IF AMSG'="" SET TMGERROR=TMGERROR_"; POC MESSAGE]: "_AMSG
        NEW VTABLE 
        ZSHOW "V":VTABLE        
        MERGE ^TMG("TMP","TMGHL73",$J,NOWH,"ERROR VARS")=VTABLE("V")
        KILL VTABLE
        ;"MAKE AN ALERT WITH ERROR MESSAGE.
        NEW XQA,XQAARCH,XQADATA,XQAFLG,XQAGUID,XQAID,XQAOPT,XQAROU,XQASUPV,XQASURO,XQATEXT
        SET XQA("LA7V IPL")=""
        SET XQA(168)=""   ;"//to Kevin Toppenberg (?)
        SET XQADATA=$J_"^"_NOWH_"^"_HLMTIEN_"^"_HLMTIENS
        SET XQAID="TMG-HL7"
        SET XQAROU="HNDLERR2^TMGHL7E"
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
        NEW TMGERROR SET TMGERROR=$GET(^TMG("TMP","TMGHL73",TMGJOBN,TMGTIME,"ERROR"))
        IF TMGERROR["^" SET TMGERROR=$PIECE(TMGERROR,"^",2,99)
        NEW PTNOTFOUND SET PTNOTFOUND=(TMGERROR["Patient not found in system:")
        IF PTNOTFOUND,$$IGNORPT(TMGERROR) DO CLEANUP2(TMGJOBN,TMGTIME) GOTO HE2DN
        IF (IEN772=0)&(IEN773=0) NEW SKIP SET SKIP=1 DO  GOTO:SKIP=1 HE2DN
        . WRITE "Insufficient information available to handle this alert.",!
        . IF TMGERROR'="" WRITE "Message was: "_TMGERROR,!
        . NEW % SET %=1 WRITE "Delete" DO YN^DICN WRITE !
        . IF %=2 SET SKIP=0 QUIT        
        . KILL ^TMG("TMP","TMGHL73",TMGJOBN,TMGTIME)        
        NEW TMGRESULT SET TMGRESULT=$$SETUPENV^TMGHL7U(.TMGTESTMSG,.TMGENV,1)        
        IF TMGRESULT'>0 DO  GOTO HE2DN
        . WRITE !,"Unable to SET up environment for processing HL7 POC Lab error.",!
        . WRITE "Message was: ",$PIECE(TMGRESULT,"^",2),!
        . WRITE "Please fix error, and then try reprocessing this alert."
        . DO PRESS2GO^TMGUSRI2
        SET TMGENV("INTERACTIVE MODE")=1
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
        IF PTNOTFOUND SET TMGMNU(TMGMNUI)="Ignore missing patient"_$CHAR(9)_"IgnorePt",TMGMNUI=TMGMNUI+1
        ;
        SET TMGUSERINPUT=$$MENU^TMGUSRI2(.TMGMNU,"^")
        KILL TMGMNU ;"Prevent from cluttering variable table during debug run
        ;
        IF TMGUSERINPUT="ViewLog" DO HESHOWLOG(+TMGJOBN) GOTO M2
        IF TMGUSERINPUT="ViewMsg" DO VIEWMSG^TMGHL7U2(.TMGTESTMSG) GOTO M2
        IF TMGUSERINPUT="TestMap" DO TESTMAP^TMGHL70A(.TMGENV,.TMGTESTMSG,.TMGHL7MSG) WRITE !,! GOTO M2  
        IF TMGUSERINPUT="HL7FileMenu" DO FILEMENU^TMGHL70(.TMGTESTMSG,INDENTN+2) WRITE !,! GOTO M2  
        IF TMGUSERINPUT="TryAgain" IF +$$TRYAGAN2(.TMGTESTMSG,0)=1 GOTO HE2DN
        IF TMGUSERINPUT="TryAgainDebugger" IF $$TRYAGAN2(.TMGTESTMSG,1)=1 GOTO HE2DN
        IF TMGUSERINPUT="InvalidValue" DO INVAL(IEN22720,.TMGERROR,INDENTN+2) GOTO M2
        IF TMGUSERINPUT="SetupTest" DO HESUTST2(.TMGENV,.TMGTESTMSG,INDENTN+2) GOTO M2 
        IF TMGUSERINPUT="IgnorePt" DO  GOTO M2:(TEMPRESULT'=1),HE2DN
        . SET TEMPRESULT=$$ADDIGNOR(.TMGENV,TMGERROR,INDENTN+2)
        . IF TEMPRESULT'=1 QUIT
        . DO CLEANUP2(TMGJOBN,TMGTIME)
        ;"IF TMGUSERINPUT="XForm" DO HXFMSUT2(.TMGENV,.TMGTESTMSG,INDENTN) GOTO M2   
        ;
        IF TMGUSERINPUT=0 SET TMGUSERINPUT=""
        IF TMGUSERINPUT'="^" GOTO M2
        IF $$TRYAGAN2(.TMGTESTMSG,0)'=1 DO
        . DO CLEANUP2(TMGJOBN,TMGTIME) 
HE2DN   WRITE "Quitting.  Goodbye",!
        QUIT
        ;
CLEANUP2(TMGJOBN,TMGTIME)  ;
        NEW % SET %=1
        WRITE "Clear stored memory relating to this alert (can't be undone)"
        DO YN^DICN WRITE !
        IF %=1 KILL ^TMG("TMP","TMGHL73",TMGJOBN,TMGTIME)
        QUIT
        ;
TRYAGAN2(TMGMSG,DEBUG) ;
        ;"Purpose: Try processing again, using debugger to walk through code.
        ;"Input: TMGMSG -- Array holding HL7 message.  
        ;"       DEBUG -- OPTIONAL.  If 1, then code is launched through debugger. 
        ;"Result: 1 if OK, or -1^Abort IF aborted. 
        NEW TMGRESULT SET TMGRESULT=1
        NEW % SET %=1
        WRITE "Send HL7 message through POC filer again" DO YN^DICN WRITE !
        IF %'=1 DO  GOTO DBDN2  ;"DEBG2DN
        . SET TMGRESULT="-1^HL7 Message Filing Aborted"        
        ;"NEW CODE SET CODE="SET TMGRESULT=$$HL7MSGIN^TMGHL71(.TMGMSG,1)"
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
        . DO SETALRT2(TMGRESULT)
        . WRITE "Alert has been created.  Exit this handler and select NEW alert to process.",!        
        DO PRESS2GO^TMGUSRI2
DBDN2   QUIT TMGRESULT
        ;        
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
        NEW TMGRESULT SET TMGRESULT=0
        SET PATSTR=$$GIGNRNAM(PATSTR)
        SET TMGIEN=+$ORDER(^TMG(22717.5,"B",$EXTRACT(PATSTR,1,30),""))
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
