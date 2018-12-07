TMGTIUT5 ;TMG/kst-TIU-related code ; 5/7/16
   ;;1.0;TMG-LIB;**1**;5/7/16
  ;
  ;"Code related to TIU components
  ;
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;"Copyright (c) 5/7/16  Kevin S. Toppenberg MD
  ;"
  ;"This file is part of the TMG LIBRARY, and may only be used in accordence
  ;" to license terms outlined in separate file TMGLICNS.m, which should 
  ;" always be distributed with this file.
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;
  ;"=======================================================================
  ;" RPC -- Public Functions. 
  ;"=======================================================================
  ;"=======================================================================
  ;"PRIVATE API FUNCTIONS
  ;"=======================================================================
  ;
  ;"=======================================================================
  ;"DEPENDENCIES: 
  ;"=======================================================================
  ;
TRIG1(IEN8925)  ;"HANDLE POST-SIGNATURE FOR TIU DOCUMENTS.
  ;"This routine is set up to fire from the POST-SIGNATURE CODE field (4.9) in file 8925
  ;"  It fires from the top level document type, CLINICAL DOCUMENTS.
  ;"  As such, it should NOT try to fire any inherited handlers, as that leads to endless loop.  
  ;"       e.g. DON'T call: FIREINH1^TMGTIUT4(.TIUIEN) 
  ;"NOTE: some documents (not all) also call TRIGGER^1^TMGC0Q04
  ;"  A Fileman search for POST-SIGNATURE CODE field in file 8925.1 will show details.
  ;
  ;"Job task off for faster foreground processing.
  SET ^TMP("TRIG1",IEN8925)=""
  JOB TRIGJOB^TMGTIUT5(+$GET(IEN8925))::10  ;"Wait for up to 10 seconds for launching background task
  ELSE  DO  ;"$TEST is set to false if JOB timesout
  . DO TRIGJOB^TMGTIUT5(+$GET(IEN8925))  ;"run in foreground task
  QUIT 
  ;
TRIGJOB(IEN8925)  ;"HANDLE TRIGGER AS SEPARATE JOB
  NEW TMPSTORE SET TMPSTORE=$NAME(^TMG("TMP","POST-SIGNATURE","TRIG1^TMGTIUT5"))
  NEW ZZDEBUG SET ZZDEBUG=0
  IF ZZDEBUG=1 DO
  . SET IEN8925=$GET(@TMPSTORE@("IEN8925"))
  ELSE  DO
  . KILL @TMPSTORE SET @TMPSTORE@("IEN8925")=$GET(IEN8925)
  SET @TMPSTORE@("LAST CALL TIME")=$$NOW^XLFDT
  IF $$ISHTML^TMGHTM1(.IEN8925) DO STRIPSCR^TMGHTM1(.IEN8925)  ;"strip <SCRIPT> ..</SCRIPT>
  NEW OUT,DFN SET DFN=+$PIECE($GET(^TIU(8925,IEN8925,0)),"^",2)
  IF DFN>0 DO SCANNOTE^TMGTIU10(DFN,IEN8925,"OUT",1)
  DO TABLDATA(IEN8925)  ;"POPULATE TMG TABLE DATA FILE
  DO HANDLTIU^TMGRX007(IEN8925)  ;"EXTRACT AND SAVE MEDICATION FILE LIST INFORMATION
  QUIT
  ;
TABLDATA(IEN8925)  ;"POPULATE TMG TABLE DATA FILE
  ;"Purpose: This routine will search the note for any tables (as defined in the 
  ;"     TMG TIU PXRM TABLE file). For each one found, an entry will be created
  ;"     or updated to reflect the most recent note location for each table
  ;"Get Note Text
  ;"Result: none
  SET IEN8925=+$GET(IEN8925)
  IF IEN8925'>0 DO SETALRT("No TIU IEN provided") GOTO TDDn
  NEW DFN SET DFN=+$PIECE($GET(^TIU(8925,IEN8925,0)),"^",2)
  IF DFN'>0 DO SETALRT("DFN for TIU IEN "_IEN8925_" could not be determined.") GOTO TDDn
  NEW NOTETEXT,LINEIDX
  SET LINEIDX=0,NOTETEXT=""
  FOR  SET LINEIDX=$ORDER(^TIU(8925,IEN8925,"TEXT",LINEIDX)) QUIT:LINEIDX'>0  DO
  . NEW LINETEXT SET LINETEXT=$GET(^TIU(8925,IEN8925,"TEXT",LINEIDX,0))
  . SET NOTETEXT=NOTETEXT_LINETEXT
  ;"Cycle through tables to see which ones are found inside the note text
  NEW TABLEIEN,TABLETITLE,TBLARRAY
  SET TABLETITLE=""
  FOR  SET TABLETITLE=$ORDER(^TMG(22708,"B",TABLETITLE)) QUIT:TABLETITLE=""  DO
  . SET TABLEIEN=$ORDER(^TMG(22708,"B",TABLETITLE,0))
  . NEW TABLEHEAD SET TABLEHEAD=$PIECE($GET(^TMG(22708,TABLEIEN,0)),"^",2)
  . IF TABLEHEAD="" SET TABLEHEAD=$PIECE($GET(^TMG(22708,TABLEIEN,0)),"^",1)
  . SET TABLEHEAD="["_TABLEHEAD_"]"  
  . NEW TBLHEAD2 SET TBLHEAD2=$$REPLSTR^TMGSTUT3(TABLEHEAD," ","&nbsp;")  ;"SOMETIMES SPACE IS CHANGED TO &nbsp; 
  . IF (NOTETEXT[TABLEHEAD)!(NOTETEXT[TBLHEAD2) DO
  . . SET TBLARRAY(TABLEIEN)=TABLEHEAD
  ;"STORE TABLES INTO 22729
  NEW TMGFDA,TMGIENS,TMGMSG,TMGIEN
  NEW TBLIDX SET TBLIDX=0
  ;"MERGE ^EDDIE("TBLARRAY")=TBLARRAY
  FOR  SET TBLIDX=$ORDER(TBLARRAY(TBLIDX)) QUIT:TBLIDX'>0  DO
  . KILL TMGFDA,TMGMSG,TMGIEN,TMGIENS
  . SET TMGIENS="+1,"
  . NEW IEN22729 SET IEN22729=+$ORDER(^TMG(22729,"C",DFN,TBLIDX,0)) 
  . IF IEN22729>0 DO
  . . ;"SET TMGFDA(22729,IEN22729_",",.03)="`"_IEN8925
  . . SET TMGFDA(22729,IEN22729_",",.03)=IEN8925
  . . ;"DO FILE^DIE("E","TMGFDA","TMGMSG")
  . . DO FILE^DIE("","TMGFDA","TMGMSG")
  . ELSE  DO
  . . SET TMGFDA(22729,TMGIENS,.01)=DFN
  . . SET TMGFDA(22729,TMGIENS,.02)=TBLIDX
  . . SET TMGFDA(22729,TMGIENS,.03)=IEN8925
  . . ;" SET TMGFDA(22729,TMGIENS,.01)="`"_DFN
  . . ;" SET TMGFDA(22729,TMGIENS,.02)="`"_TBLIDX
  . . ;" SET TMGFDA(22729,TMGIENS,.03)="`"_IEN8925
  . . ;"DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
  . . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  . IF $DATA(TMGMSG("DIERR")) DO SETALRT($$GETERRST^TMGDEBU2(.TMGMSG))
TDDn  QUIT
  ;"
SETALRT(ERRTEXT) ;
    ;"Purpose: Set up alerts for error handling of 22729 storing process
    ;"Input: ERRTEXT -- Text of error.
    ;"   AMSG -- Additional message, IF any.
    ;"Results: NONE:
    ;"Output: An alert is created.
    ;"Restore originial message
    NEW NOWH SET NOWH=$H
    SET ERRTEXT=$GET(ERRTEXT)
    KILL MSGSTORE ;"Not needed, and clutters variable table
    ;"MAKE AN ALERT WITH ERROR MESSAGE.
    NEW XQA,XQAARCH,XQADATA,XQAFLG,XQAGUID,XQAID,XQAOPT,XQAROU,XQASUPV,XQASURO,XQATEXT
    SET XQA("LA7V IPL")=""
    SET XQA(150)=""   ;"//to Kevin Toppenberg (168)
    SET XQADATA=$J_"^"_NOWH  ;"_"^"_HLMTIEN_"^"_HLMTIENS
    SET XQAID="TMG-22729"
    SET XQAROU="HNDLERR^TMGTIUT5"
    SET XQAMSG="Error storing data into file 22729 during Post-Signature process"
    SET ^TMG("TMP","TMGTIUT5",$J,NOWH,"ERROR")=ERRTEXT
    ;"SET ^TMG("TMP","TMGTIUT5","$H",NOWH,$J)=""
    NEW TEMP SET TEMP=$$SETUP1^XQALERT
SA2DN   QUIT
    ;"
HNDLERR ;"
    SET XQADATA=$GET(XQADATA)
    NEW TMGJOBN SET TMGJOBN=+$PIECE(XQADATA,"^",1)
    NEW TMGTIME SET TMGTIME=$PIECE(XQADATA,"^",2)
    NEW ERRTEXT SET ERRTEXT=$GET(^TMG("TMP","TMGTIUT5",TMGJOBN,TMGTIME,"ERROR"))
    WRITE !,"Reported error was: ",ERRTEXT,!
    WRITE "Handler not setup for this error. Goto HNDLERR^TMGTIUT5 if one is needed.",!
    DO PRESS2GO^TMGUSRI2
    QUIT
    ;"
