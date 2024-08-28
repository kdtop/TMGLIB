TMGTIUT5 ;TMG/kst-TIU-related code ; 5/7/16, 3/24/21
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
MAINTRIG(IEN8925)  ;"This will be the main Post-Signature Entry Point, and will route to the proper code.
  IF 1=0 DO 
  . SET ^TMP($J,"MAINTRIG",IEN8925)=$H
  . SET ^TMP($J,"MAINTRIG",IEN8925,"ATYPEIEN")=TYPEIEN
  DO HNDLFRGND(IEN8925)  ;"Do foreground tasks
  DO HNDLBKGND(IEN8925)  ;"Do background tasks.      
MTDN ;  
  QUIT
  ;"
HNDLFRGND(TIUIEN,ONLYOV) ;"HANDLE TRIGGER EVENTS IN **FOREGROUND** PROCESS
  ;"Moved from TRIGJOB^TMGTIUT6
  ;"Input: TIUIEN --The IEN in file 8925 (TIU DOCUMENT)
  ;"       ONLYOV -- OPTIONAL.  If 1, then only office visit type note are processed
  SET TIUIEN=+$GET(TIUIEN) GOTO:(TIUIEN'>0) TGDN  
  IF ($GET(ONLYOV)=1),($$ISOFFVST^TMGC0QT1(TIUIEN)=0) GOTO TGDN  ;"optionally skip this TIU DOCUMENT if not office visit
  NEW PROVIEN SET PROVIEN=+$PIECE($GET(^TIU(8925,TIUIEN,12)),"^",2)  
  IF (PROVIEN=168)!(PROVIEN=83) DO        ;"<-- HARD CODED PROVIDER IEN'S  
  . NEW ARRAY
  . DO SUMNOTE^TMGTIUP1(TIUIEN,.ARRAY)    ;"SUMNOTE parses the note HPI and A&P into ARRAY  
  . DO FILE1(TIUIEN,"ARRAY",.QUIET)       ;"Store ARRAY into file 22719   (TMG TIU DOCUMENT TOPICS)
  . DO FILE1B(TIUIEN,.ARRAY,.QUIET)       ;"Store ARRAY into file 22719.2 (TMG TIU DOCUMENT THREADS)    
  ;
TGDN  ;
  QUIT
  ;  
HNDLBKGND(IEN8925)  ;"HANDLE POST-SIGNATURE FOR TIU DOCUMENTS IN **BACKGROUND** JOB
  ;"SET ^TMP("HNDLBKGND",IEN8925)=""
  ;"Job task off for faster foreground processing.
  NEW ZZDEBUG SET ZZDEBUG=0
  IF ZZDEBUG=0 JOB BKJOB^TMGTIUT5(+$GET(IEN8925))::10  ;"Wait for up to 10 seconds for launching background task
  ELSE  DO  ;"$TEST is set to false if JOB times out
  . DO BKJOB^TMGTIUT5(+$GET(IEN8925))  ;"run in foreground task
  QUIT 
  ;
BKJOB(IEN8925)  ;
  NEW OUT,TMPSTORE SET TMPSTORE=$NAME(^TMG("TMP","POST-SIGNATURE","BKJOB^TMGTIUT5"))
  NEW ZZDEBUG SET ZZDEBUG=0
  IF ZZDEBUG=1 DO
  . SET IEN8925=$GET(@TMPSTORE@("IEN8925"))
  ELSE  DO
  . KILL @TMPSTORE SET @TMPSTORE@("IEN8925")=$GET(IEN8925)
  SET @TMPSTORE@("LAST CALL TIME")=$$NOW^XLFDT  
  ;"--------------------------------------------------------
  IF $$ISHTML^TMGHTM1(.IEN8925) DO STRIPSCR^TMGHTM1(.IEN8925)  ;"strip <SCRIPT> ..</SCRIPT> -- If the note is HTML, remove any <SCRIPT> tags
  ;
  ;"FILEIMM stores immunizations given with the IEN8925 into file 22741.
  ;"   I cannot show that this file is ever used. More research needs to be done, but
  ;"   nothing in the TMG*.m namespace seems to reference this file
  ;"DO FILEIMM^TMGTIUT5(.IEN8925)  ;"DEPRECIATED -- REMOVE LATER  8/27/24
  ;
  DO SCAN4FU^TMGTIU10(IEN8925,"OUT",1)  ;"SCAN4FU gets followup data and saves it into 22731 'TMG TIU FOLLOWUP DATA'
  DO TABLDATA(IEN8925)                  ;"POPULATE TMG TABLE DATA FILE  
  DO HANDLTIU^TMGRX007(IEN8925)         ;"EXTRACT AND SAVE MEDICATION FILE LIST INFORMATION
  DO HNDLMAMMO(IEN8925)                 ;"Handle mammogram type documents (if appropriate type)
  DO CHK6CIT^TMGTIUT6(TIUIEN)           ;"Ceck if note contains 6CIT. If so, add a HF and attach the score to it (STILL USED?)
  DO ADDLSIGN(IEN8925)                  ;"Used for Death Notes and Path Report (Image) TIU titles. This can be kept and have both notes run through the normal Post-Signature code
  ;"
  ;"If the note title is a CONSULT / PATIENT CONTINUITY SUMMARY, then do CHECKDT
  ;" NOTES: This functionality looks antiquated. I cannot figure out exactly what was supposed to be happening with it.
  ;"    IEN 1486 is not CONSULT / PATIENT CONTINUTY SUMMARY. It is now RECORDS SENT
  ;"    CHECKDT Looked to see if the note had a Tickler in it. If so, it saved it to ^TIU(8925,"ATMGCNSLT"
  ;"    This was the old way of handling Ticklers. We stopped in 2017, it looks like.
  ;"  New method of handling Ticklers... 
  ;"    1) TIU Object TMG TICKLER (INVISIBLE), stubs out the initial 'unsigned' entry into the TMG Tickler file 22705.5 inside
  ;"              function TICKLER^TMGTICKL
  ;"    2) HANDLE^TMGTICKL runs hourly through Taskman Scheduler. This looks at all 'unsigned' ticklers to see if the
  ;"              corresponding notes have been signed
  ;"  I think the below is useless
  ;"IF +^TIU(8925,TIUIEN,0)=$$CNSLTPT() DO CHECKDT(TIUIEN)  ;"Fix TMGCNSLT Xref entry for certain .01 field entries
  ;
  QUIT
  ;
CHECKDT(TIUIEN)   ;"DEPRECIATED.  SEEMS TO BE OLD CODE.  //kt 8/27/24        
  ;"Purpose: After note has been signed, this code will be called IF .01 field is CONSULT / PATIENT CONTINUITY SUMMARY
  ;"         OLD --> CALLED FROM: TRIGGER1^TMGC0Q04
  ;"                      SETOLD, CKDTOLD
  ;"         Will change: ^TIU(8925,"ATMGCNSLT",EpisodeBeginDate,DocIEN)="" to
  ;"                      ^TIU(8925,"ATMGCNSLT",EpisodeBeginDate,DocIEN)=<Record-Send Due Date (FM Format)>
  ;"              Note: Later, after it has been determined that records have already been completed,
  ;"                    the node will be changed to:  ^TIU(8925,"ATMGCNSLT",EpisodeBeginDate,DocIEN)=1
  ;"                    (This is done elsewhere: CONTCARE^TMGC0QT2)
  ;"Input: TIUIEN -- IEN IN 8925
  ;"Result: none
  NEW USER SET USER=DUZ
  NEW DOCDT SET DOCDT=+$PIECE($GET(^TIU(8925,+TIUIEN,0)),"^",7)
  IF DOCDT'>0 QUIT
  ;"IF $DATA(^TIU(8925,"ATMGCNSLT",DOCDT,TIUIEN))=0 QUIT
  NEW DATESTR
  NEW DUEDATE SET DUEDATE=0
  IF $$HasTickler^TMGTICKL(TIUIEN,.DATESTR,.USER) do
  . SET DUEDATE=+$GET(DATESTR("FM"))
  . IF +DUEDATE=0 SET DUEDATE=DOCDT  ;"Tickler entry now allowed to have no date, when wanted to fire immediatly.
  . SET ^TIU(8925,"ATMGCNSLT",DOCDT,TIUIEN)=DUEDATE
  QUIT
  ;
FILE1(TIUIEN,PARRAY,QUIET,OPTION)  ;"Store parsed array into Fileman file 22719
  ;"Input: TIUIEN -- The IEN in file 8925 (TIU DOCUMENT)
  ;"       PARRAY -- PASS BY NAME.  Info to be filed.  Format as below
  ;"         @PARRAY@(TIUIEN,"HPI",Item#)=Topic^First line of paragraph
  ;"         @PARRAY@(TIUIEN,"A&P",Item#)=Topic^First line of paragraph
  ;"       QUIET -- OPTIONAL.  If 1, then no screen output. DEFAULT=1
  ;"            To get back errors, PASS QUIET BY REFERENCE.  Stored as follows:
  ;"            QUIET(TIUIEN)=ErrorString
  ;"            QUIET(TIUIEN,SECTION,#)=SubRecErrorString
  ;"       OPTION -- OPTIONAL
  ;"          OPTION("SKIP REWRITE"=1 -- if should NOT write if prior data present.  
  ;"Results: NONE
  ;
  SET QUIET=+$GET(QUIET,1)
  NEW TMGFDA,TMGMSG,TMGIEN
  NEW SKIP SET SKIP=+$GET(OPTION("SKIP REWRITE"))
  NEW RECIEN SET RECIEN=+$ORDER(^TMG(22719,"B",TIUIEN,""))
  IF RECIEN>0 DO  GOTO F1B
  . IF $DATA(@PARRAY)=0 QUIT
  . IF SKIP QUIT
  . DO CLEAR1(RECIEN)
  . SET TMGFDA(22719,RECIEN_",",1)="NOW"
  . DO FILE^DIE("E","TMGFDA","TMGMSG")
  . IF $DATA(TMGMSG("DIERR"))=0 QUIT
  . NEW ERRSTR SET ERRSTR=$$GETERRST^TMGDEBU2(.TMGMSG)
  . SET QUIET(TIUIEN)=ERRSTR QUIT:QUIET
  . WRITE !,ERRSTR,!
ADD ;
  SET TMGIEN(1)=TIUIEN
  SET TMGFDA(22719,"+1,",.01)="`"_TIUIEN
  SET TMGFDA(22719,"+1,",1)="NOW"
  DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO F1DN
  . NEW ERRSTR SET ERRSTR=$$GETERRST^TMGDEBU2(.TMGMSG)
  . IF QUIET SET QUIET(TIUIEN)=ERRSTR
  . ELSE  WRITE !,ERRSTR,!
  SET RECIEN=+$GET(TMGIEN(1))
  IF RECIEN'>0 DO  
  . NEW ERRSTR SET ERRSTR="Unable to find record to file into."
  . SET QUIET(TIUIEN)=ERRSTR QUIT:QUIET
  . WRITE !,ERRSTR,!
F1B  ;
  IF $DATA(QUIET(TIUIEN)) GOTO F1DN
  IF SKIP,$DATA(^TMG(22719,TIUIEN))>0 GOTO F1DN
  NEW PROVIEN SET PROVIEN=+$PIECE($GET(^TIU(8925,TIUIEN,12)),"^",2)
  ;"IF (PROVIEN'=168)&(PROVIEN'=83) GOTO F1DN
  ;"IF $$ISOFFVST^TMGC0QT1(TIUIEN)=0 GOTO F1DN
  NEW SECTION SET SECTION=""
  FOR  SET SECTION=$ORDER(@PARRAY@(TIUIEN,SECTION)) QUIT:SECTION=""  DO
  . NEW FLD,FILE SET FLD=0
  . IF SECTION="HPI" SET FLD=2,FILE=22719.02
  . ELSE  IF SECTION="A&P" SET FLD=3,FILE=22719.03
  . IF FLD'>0 QUIT
  . NEW IDX SET IDX=0
  . FOR  SET IDX=$ORDER(@PARRAY@(TIUIEN,SECTION,IDX)) QUIT:IDX'>0  DO
  . . NEW INFO SET INFO=$GET(@PARRAY@(TIUIEN,SECTION,IDX))
  . . NEW TMGFDA,TMGMSG,TMGIEN
  . . NEW TITLE SET TITLE=$PIECE(INFO,"^",1) QUIT:TITLE=""
  . . IF $LENGTH(TITLE)>100 SET TITLE=$EXTRACT(TITLE,1,100) ;"Field length restriction.
  . . NEW CONTEXT SET CONTEXT=$PIECE(INFO,"^",2)
  . . KILL TMGFDA,TMGMSG,TMGIEN,TMGIENS
  . . SET TMGIENS="+1,"_RECIEN_","
  . . SET TMGFDA(FILE,TMGIENS,.01)=TITLE
  . . SET TMGFDA(FILE,TMGIENS,1)=CONTEXT
  . . DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
  . . IF $DATA(TMGMSG("DIERR")) DO  QUIT
  . . . NEW ERRSTR SET ERRSTR=$$GETERRST^TMGDEBU2(.TMGMSG)
  . . . IF QUIET SET QUIET(TIUIEN,SECTION,IDX)=ERRSTR
  . . . ELSE  WRITE !,"TIUIEN=#",TIUIEN," ",$$GETERRST^TMGDEBU2(.TMGMSG),!
  . . IF SECTION'="HPI" QUIT
  . . NEW THREADSTR SET THREADSTR=$GET(@PARRAY@(TIUIEN,"THREAD",IDX))
  . . SET THREADSTR=$PIECE(THREADSTR,"^",2,999)
  . . IF THREADSTR="" QUIT
  . . NEW SUBIEN SET SUBIEN=$GET(TMGIEN(1))
  . . IF SUBIEN'>0 DO  QUIT
  . . . WRITE !,"TIUIEN=#",TIUIEN," Unable to find subien of added sub record",!
  . . SET TMGIENS=SUBIEN_","_TIUIEN_","
  . . NEW TMGWP KILL TMGMSG
  . . DO STR2WP^TMGSTUT2(THREADSTR,"TMGWP",75)
  . . IF $DATA(TMGWP)=0 QUIT
  . . DO WP^DIE(FILE,TMGIENS,2,"","TMGWP","TMGMSG")
  . . IF $DATA(TMGMSG("DIERR")) DO  QUIT
  . . . NEW ERRSTR SET ERRSTR=$$GETERRST^TMGDEBU2(.TMGMSG)
  . . . IF QUIET SET QUIET(TIUIEN,SECTION,IDX)=ERRSTR
  . . . ELSE  WRITE !,"TIUIEN=#",TIUIEN," ",$$GETERRST^TMGDEBU2(.TMGMSG),!
F1DN ;
  QUIT
  ;
FILE1B(TIUIEN,ARRAY,QUIET,OPTION)  ;"Store parsed array into Fileman file 22719.2 (TMG TIU DOCUMENT THREADS)
  ;"Moved from FILE1B^TMGTIUT6
  ;"Input: TIUIEN -- The IEN in file 8925 (TIU DOCUMENT)
  ;"       ARRAY -- PASS BY REFERENCE.  Info to be filed.  Format as below
  ;"         ARRAY(TIUIEN,"THREAD",IDX#)=Topic^Thread text
  ;"       QUIET -- OPTIONAL.  If 1, then no screen output. DEFAULT=1
  ;"            To get back errors, PASS QUIET BY REFERENCE.  Stored as follows:
  ;"            QUIET(TIUIEN)=ErrorString
  ;"       OPTION -- OPTIONAL
  ;"          OPTION("SKIP REWRITE"=1 -- if should NOT write if prior data present.  
  ;"Results: NONE
  NEW TMGFDA,TMGMSG,TMGIEN,TMGIENS
  NEW ERR SET ERR=""
  NEW ZN SET ZN=$GET(^TIU(8925,TIUIEN,0))
  NEW ADFN SET ADFN=+$PIECE(ZN,"^",2) ;//GET PATIENT DFN
  IF ADFN'>0 SET ERR="Unable to get patient from TIUIEN: "_TIUIEN GOTO ERR
  NEW RECIEN SET RECIEN=ADFN
  IF $DATA(^TMG(22719.2,RECIEN))>0 GOTO F1BL2  
  ;"MAKE RECORD FOR PATIENT, WITH DINUM RECORD NUMBER
  SET TMGIEN(1)=RECIEN
  SET TMGFDA(22719.2,"+1,",.01)=ADFN
  DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) SET ERR=$$GETERRST^TMGDEBU2(.TMGMSG) GOTO ERR
F1BL2 ;  
  NEW NOTEDT SET NOTEDT=+$PIECE(ZN,"^",7)  ;"0;7=EPISODE BEGIN DATE/TIME
  IF NOTEDT'>0 SET ERR="Unable to get Episode Begin Date/Time from TIUIEN: "_TIUIEN GOTO ERR
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(ARRAY(TIUIEN,"THREAD",IDX)) QUIT:(IDX'>0)!(ERR'="")  DO
  . NEW TEMP SET TEMP=$GET(ARRAY(TIUIEN,"THREAD",IDX))
  . NEW TOPIC SET TOPIC=$PIECE(TEMP,"^",1) QUIT:TOPIC=""
  . NEW THREADSTR SET THREADSTR=$PIECE(TEMP,"^",2,999) QUIT:THREADSTR=""
  . NEW TOPICIEN SET TOPICIEN=$ORDER(^TMG(22719.2,RECIEN,1,"B",TOPIC,0))
  . IF TOPICIEN'>0  DO  QUIT:(ERR'="")!(TOPICIEN'>0)
  . . KILL TMGIEN,TMGFDA,TMGMSG
  . . SET TMGFDA(22719.21,"+1,"_RECIEN_",",.01)=TOPIC
  . . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  . . IF $DATA(TMGMSG("DIERR")) SET ERR=$$GETERRST^TMGDEBU2(.TMGMSG) QUIT
  . . SET TOPICIEN=$GET(TMGIEN(1)) IF TOPICIEN'>0 DO  QUIT
  . . . SET ERR="Unable to find subIEN added for topic: "_TOPIC
  . ;"ENSURE SUBRECORD FOR NOTEDT
  . NEW DTIEN SET DTIEN=$ORDER(^TMG(22719.2,RECIEN,1,TOPICIEN,1,"B",NOTEDT,0))
  . IF DTIEN'>0 DO  QUIT:(ERR'="")!(DTIEN'>0)  
  . . KILL TMGIEN,TMGFDA,TMGMSG
  . . SET TMGIENS="+1,"_TOPICIEN_","_RECIEN_","
  . . SET TMGFDA(22719.211,TMGIENS,.01)=NOTEDT
  . . SET TMGFDA(22719.211,TMGIENS,.02)=TIUIEN
  . . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  . . IF $DATA(TMGMSG("DIERR")) SET ERR=$$GETERRST^TMGDEBU2(.TMGMSG) QUIT
  . . SET DTIEN=$GET(TMGIEN(1)) IF DTIEN'>0 DO  QUIT
  . . . SET ERR="Unable to find subsubIEN added for NOTE DT: "_NOTEDT_", IENS="_TMGIENS
  . ;"FILE ARRAY INTO WP FIELD.  
  . NEW TMGWP KILL TMGMSG
  . DO STR2WP^TMGSTUT2(THREADSTR,"TMGWP",75)
  . IF $DATA(TMGWP)=0 QUIT
  . NEW SKIPFILE,TMGPRIORWP SET SKIPFILE=0
  . IF $DATA(^TMG(22719.2,RECIEN,1,TOPICIEN,1,DTIEN,1))>0 DO  QUIT:SKIPFILE
  . . IF $GET(OPTION("SKIP REWRITE"))>0 SET SKIPFILE=1 QUIT
  . . NEW IDX SET IDX=0
  . . NEW SAME SET SAME=1
  . . FOR  SET IDX=$ORDER(^TMG(22719.2,RECIEN,1,TOPICIEN,1,DTIEN,1,IDX)) QUIT:(IDX'>0)!(SAME=0)  DO
  . . . SET TMGPRIORWP(IDX)=$GET(^TMG(22719.2,RECIEN,1,TOPICIEN,1,DTIEN,1,IDX,0))
  . . . SET SAME=(TMGPRIORWP(IDX)=TMGWP(IDX))
  . . IF SAME SET SKIPFILE=1
  . SET TMGIENS=DTIEN_","_TOPICIEN_","_RECIEN_","
  . DO WP^DIE(22719.211,TMGIENS,1,"","TMGWP","TMGMSG")
  . IF $DATA(TMGMSG("DIERR")) DO  QUIT
  . . SET ERR=$$GETERRST^TMGDEBU2(.TMGMSG)
  GOTO DN
ERR ;
  IF $GET(QUIET)=1 DO  
  . SET QUIET(TIUIEN)=ERR
  ELSE  DO
  . WRITE !,"ERROR: ",ERR,!
DN ;
  QUIT
  ;  
CNSLTPT() ;"get hard-coded pointer for CONSULT / PATIENT CONTINUITY SUMMARY
  QUIT 1486
  ;
CLRFRMTU(TIUIEN) ;"Clear 1 record for linked TIU DOCUMENT.
  SET SAVPARENT=+$GET(SAVPARNT,0)
  SET TIUIEN=+$GET(TIUIEN)
  IF TIUIEN'>0 GOTO CFM
  NEW RECIEN SET RECIEN=+$ORDER(^TMG(22719,"B",TIUIEN,""))
  GOTO:RECIEN'>0 C1DN
  DO CLEAR1(RECIEN)
CFM  ;
  QUIT
  ;
CLEAR1(RECIEN) ;
  ;"Purpose: to clear record
  ;"Input: RECIEN -- The IEN in file 22719 to clear
  NEW DIK,DA
  SET DA(1)=RECIEN
  NEW NODE FOR NODE=2,3 DO
  . SET DIK="^TMG(22719,"_DA(1)_","_NODE_","
  . SET DA=0 FOR  SET DA=$ORDER(^TMG(22719,RECIEN,NODE,DA)) QUIT:+DA'>0  DO
  . . DO ^DIK
C1DN ;
  QUIT
  ;  
ADDLSIGN(IEN8925) ;"Add additional signers, if appropriate
  ;"NOTE: I accidentally deleted ADDLSIGN^TMGTIUT6.  sorry!!!!
  NEW ADDLSIGN
  SET ADDLSIGN(1416)="DEATH NOTE"
  SET ADDLSIGN(1420)="PATH REPORT (IMAGE)"
  ;
  IF $DATA(ADDLSIGN(IEN8925)) DO
  . ;"FINISH
  ;
  QUIT
  ;
HNDLMAMMO(IEN8925)  ;"Handle mammogram type notes, if needed.  
  NEW MAMMORSLT
  SET MAMMORSLT(649)=""  ;"MAMMOGRAM RESULTS
  NEW TYPEIEN SET TYPEIEN=$P($G(^TIU(8925,IEN8925,0)),"^",1)
  ;"
  IF $DATA(MAMMORSLT(TYPEIEN)) DO
  . DO POST^TIUCNSLT(IEN8925,"COMPLETED")
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
  NEW TMGDFN SET TMGDFN=+$PIECE($GET(^TIU(8925,IEN8925,0)),"^",2)
  IF TMGDFN'>0 DO SETALRT("DFN for TIU IEN "_IEN8925_" could not be determined.") GOTO TDDn
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
  . NEW IEN22729 SET IEN22729=+$ORDER(^TMG(22729,"C",TMGDFN,TBLIDX,0)) 
  . IF IEN22729>0 DO
  . . ;"SET TMGFDA(22729,IEN22729_",",.03)="`"_IEN8925
  . . SET TMGFDA(22729,IEN22729_",",.03)=IEN8925
  . . ;"DO FILE^DIE("E","TMGFDA","TMGMSG")
  . . DO FILE^DIE("","TMGFDA","TMGMSG")
  . ELSE  DO
  . . SET TMGFDA(22729,TMGIENS,.01)=TMGDFN
  . . SET TMGFDA(22729,TMGIENS,.02)=TBLIDX
  . . SET TMGFDA(22729,TMGIENS,.03)=IEN8925
  . . ;" SET TMGFDA(22729,TMGIENS,.01)="`"_TMGDFN
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
FILEIMM(TIUIEN)  ;"DEPRECIATED -- REMOVE LATER  8/27/24
    ;"Using TIUIEN, this function will find any associated Imminizations that were
    ;"  administered with the visit. If any are found, the details are filed
    ;"  in the TMG VACCINES ADMINISTERED FILE (22741)
    NEW TIUZN,VISITIEN
    SET TIUZN=$G(^TIU(8925,TIUIEN,0))
    SET VISITIEN=+$P(TIUZN,"^",3)
    IF VISITIEN'>0 QUIT
    NEW VISITZN SET VISITZN=$G(^AUPNVSIT(VISITIEN,0))
    NEW ADMINDATE SET ADMINDATE=$P(VISITZN,"^",1)
    NEW TMGDFN SET TMGDFN=$P(VISITZN,"^",5)
    NEW IMMIDX SET IMMIDX=0
    FOR  SET IMMIDX=$ORDER(^AUPNVIMM("AD",VISITIEN,IMMIDX)) QUIT:IMMIDX'>0  DO
    . NEW IMMIEN SET IMMIEN=$P($G(^AUPNVIMM(IMMIDX,0)),"^",1)
    . NEW IMMNAME SET IMMNAME=$P($G(^AUTTIMM(IMMIEN,0)),"^",1)
    . ;"THE BELOW CAN BE USED FOR GETTING LOT AND EXP
    . ;"BUT THE NAMES DON'T NEATLY MATCH SO IT NEEDS TO BE TWEAKED
    . ;"NEW LOT,EXP
    . ;"DO GETADMIN(TIUIEN,IMMNAME,.LOT,.EXP)
    . WRITE IMMIEN," ",IMMNAME," ",ADMINDATE,!
    . NEW TMGFDA,TMGIEN,TMGMSG,AGE
    . SET TMGFDA(22741,"+1,",.01)=TMGDFN
    . SET TMGFDA(22741,"+1,",.02)=IMMIEN
    . SET TMGFDA(22741,"+1,",.03)=ADMINDATE
    . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
    QUIT
    ;"
GETADMIN(TIUIEN,IMMNAME,LOT,EXP)  ;"
    SET LOT="",EXP=""
    SET TIULINE=0
    FOR  SET TIULINE=$ORDER(^TIU(8925,TIUIEN,"TEXT",TIULINE)) QUIT:TIULINE'>0  DO
    . SET TEXT=$GET(^TIU(8925,TIUIEN,"TEXT",TIULINE,0))
    . NEW Y SET Y=$PIECE($GET(^TIU(8925,TIUIEN,0)),"^",7)
    . D DD^%DT
    . SET DOS=$PIECE(Y,"@",1)
    . IF TEXT["IMMUNIZATION(S)" DO
    . . SET DONE=0,TEMPSTR=""
    . . NEW TEMPIDX SET TEMPIDX=TIULINE-1
    . . SET GOTADMIN=0
    . . FOR  SET TEMPIDX=$ORDER(^TIU(8925,TIUIEN,"TEXT",TEMPIDX)) QUIT:(DONE)!(TEMPIDX'>0)  DO
    . . . SET TEMPSTR=TEMPSTR_$GET(^TIU(8925,TIUIEN,"TEXT",TEMPIDX,0))
    . . . ;"IF (TEMPSTR["Manufacturer")!(TEMPSTR["NDC")!(TEMPSTR["Lot Number")!(TEMPSTR["Expiration") SET DONE=1,GOTADMIN=1
    . . . IF TEMPSTR["Expiration" SET DONE=1,GOTADMIN=1
    . . . IF (TEMPSTR["Ordered")!(TEMPSTR["Refused") SET DONE=1  ;"Not administered here
    . . SET TEMPSTR=$P($$HTML2TXS^TMGHTM1(TEMPSTR),"DOCUMENTATION:",2)
    . . SET IMM=$$TRIM^XLFSTR($PIECE(TEMPSTR,":",1))
    . . IF IMM'[IMMNAME QUIT
    . . SET IMM=$P(IMM,"-",1)
    . . IF GOTADMIN=1 DO
    . . . SET TEMPSTR=TEMPSTR_" "_$GET(^TIU(8925,TIUIEN,"TEXT",TEMPIDX,0))
    . . . SET TEMPSTR=$$HTML2TXS^TMGHTM1(TEMPSTR)
    . . . SET NDC=$$GETRSLT^TMGRPT2(TEMPSTR,"NDC: ")
    . . . ;"SET MANUFACTURER=$$GETRSLT(TEMPSTR,"Manufacturer: ")
    . . . SET LOT=$$GETRSLT^TMGRPT2(TEMPSTR,"Lot Number: ")
    . . . SET EXP=$$GETRSLT^TMGRPT2(TEMPSTR,"Date: ")
    . . . SET IMMIEN=IMMIEN+1
    QUIT
    ;"
 ;" PREVIOUS POST-SIGNATURE CODES    8/26/24
 ;"DO TRIGGER1^TMGTIUT6(TIUDA)
 ;"-----------------------------------
 ;"ACUTE MEDICAL ISSUE VISIT    1399
 ;"ADDENDUM  81    512
 ;"COMPLETE PHYSICAL EXAM    1402
 ;"FOCUSED OFFICE VISIT     1983
 ;"HOSPITAL D/C SUMM (IMAGE)  1470
 ;"HOSPITAL DISCHARGE (IMAGE)  1471
 ;"HOSPITAL FOLLOWUP VISIT   1400
 ;"OFFICE VISIT   1408
 ;"PHONE NOTE  1407
 ;"RECORDS SENT   1486
 ;"TELEMED AUDIO ACUTE  2223
 ;"TELEMED AUDIO HOSPITAL FOLLOWUP 154
 ;"TELEMED AUDIO OFFICE VISIT  2220
 ;"TELEMED VIDEO ACUTE  2224
 ;"TELEMED VIDEO AWV  2222
 ;"TELEMED VIDEO HOSPITAL FOLLOWUP  191
 ;"TELEMED VIDEO OFFICE VISIT  2221
 ;"TELEMEDICINE - ACUTE  2140
 ;"TELEMEDICINE - OFFICE VISIT   2012 
 ;"-----------------------------
 ;"
 ;"DEATH NOTE DO ADDLSIGN^TMGTIUT6(TIUDA)    199
 ;"MAMMOGRAM RESULT D POST^TIUCNSLT(DA,"COMPLETED")    649
 ;"PATH REPORT (IMAGE) DO ADDLSIGN^TMGTIUT6(TIUDA)   2080
 ;"
 ;"CLINICAL DOCUMENTS DO HNDLBKGND^TMGTIUT5(TIUDA)
 ;"CLINICAL PROCEDURES D POST^TIUCPCL(DA,"COMPLETED")
 ;"CONSULTS D POST^TIUCNSLT(DA,"COMPLETED")
 
  ;"can delete below later
  ;"NEW HANDLED SET HANDLED=0
  ;";"
  ;"NEW OVDOCS   ;"THIS WILL HOLD THE DOCUMENTS THAT SHOULD TRIGGER
  ;"SET OVDOCS(1399)=""  ;"ACUTE MEDICAL ISSUE VISIT"
  ;"SET OVDOCS(81)=""    ;"ADDENDUM"  	  
  ;"SET OVDOCS(512)=""   ;"ADDENDUM"
  ;"SET OVDOCS(1402)=""  ;"COMPLETE PHYSICAL EXAM"
  ;"SET OVDOCS(1983)=""  ;"FOCUSED OFFICE VISIT"
  ;"SET OVDOCS(1470)=""  ;"HOSPITAL D/C SUMM (IMAGE)"
  ;"SET OVDOCS(1471)=""  ;"HOSPITAL DISCHARGE (IMAGE)"
  ;"SET OVDOCS(1400)=""  ;"HOSPITAL FOLLOWUP VISIT"
  ;"SET OVDOCS(1408)=""  ;"OFFICE VISIT"
  ;"SET OVDOCS(1407)=""  ;"PHONE NOTE"
  ;"SET OVDOCS(1486)=""  ;"RECORDS SENT"
  ;"SET OVDOCS(2223)=""  ;"TELEMED AUDIO ACUTE"
  ;"SET OVDOCS(154)=""   ;"TELEMED AUDIO HOSPITAL FOLLOWUP"
  ;"SET OVDOCS(2220)=""  ;"TELEMED AUDIO OFFICE VISIT"
  ;"SET OVDOCS(2224)=""  ;"TELEMED VIDEO ACUTE"
  ;"SET OVDOCS(2222)=""  ;"TELEMED VIDEO AWV"
  ;"SET OVDOCS(191)=""   ;"TELEMED VIDEO HOSPITAL FOLLOWUP"
  ;"SET OVDOCS(2221)=""  ;"TELEMED VIDEO OFFICE VISIT"
  ;"SET OVDOCS(2140)=""  ;"TELEMEDICINE - ACUTE"
  ;"SET OVDOCS(2012)=""  ;"TELEMEDICINE - OFFICE VISIT"
  ;";"
  ;"IF $DATA(OVDOCS(TYPEIEN)) DO
  ;". ;"SET HANDLED=1  ;"WE WANT TO DO THE HNDLBKGND WITH THESE
  ;". IF OUTPUT=1 SET ^TMP($J,"MAINTRIG",IEN8925,"TRIGGER1")=$H
  ;". DO HNDLFRGND(IEN8925)
  ;";"
  ;"IF HANDLED=0 DO
  ;". IF OUTPUT=1 SET ^TMP($J,"MAINTRIG",IEN8925,"HNDLBKGND")=$H
  ;". DO HNDLBKGND(IEN8925)    ;"DEFAULT HANDLER FOR ALL OTHER NOTES
  ;"
