TMGC0Q04 ;TMG/kst/TMG meanful use util code ;2/2/14, 5/6/18, 3/24/21
         ;;1.0;TMG-LIB;**1**;8/12/12
  ;
  ;"TMG C0Q FUNCTIONS
  ;
  ;"--This code is for parsing a TIU DOCUMENT and extracting the paragraph titles
  ;"  for determing the topics discussed at a given visit.
  ;"--Results are stored in file 22719.
  ;"--There is a function here for parsing all documents at one time.  But the
  ;"  ultimate use is SET up such that a POST-SIGNATURE hook from TIU calls code
  ;"  here after a document is completed.
  ;"--Also, XREF code for TIU DOCUMENT for Consult /Continuity of Care note titles
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
  ;"TRIGALL --  process all TIU documents and trigger each one
  ;"TRIGGER1(TIUIEN,QUIET,TMGDOCSCANMODS) -- trigger for 1 document: scan, parse, store
  ;"XRCONSLT(X,MODE) -- XRef set/kill function for TIU DOCUMENT field, .01 field 
  ;"=======================================================================
  ;"PRIVATE API FUNCTIONS
  ;"=======================================================================
  ;"GETMODS(MODS)  -- load up MODS variable.
  ;"CHKDIV(LINESTR,DIV) ;
  ;"FILE1(TIUIEN,PARRAY,QUIET) store parsed array into Fileman file 22719
  ;"CLRFRMTU(TIUIEN) --Clear 1 record for linked TIU DOCUMENT.
  ;"CLEAR1(RECIEN) -- clear record
  ;
  ;"=======================================================================
  ;"DEPENDENCIES
  ;"=======================================================================
  ;
  ;"=======================================================================
  ;
SUMMALL ;
  ;"Purpose: process all TIU documents and summarize each one
  WRITE "==============================================",!
  WRITE "Office visit parser",!
  WRITE "..............................................",!
  WRITE "Will scan every TIU DOCUMENT and summarize",!
  WRITE "paragraph titles found in HPI sections.",!
  WRITE "Press ESC (perhaps multiple times) to escape loop.",!
  WRITE "==============================================",!,!
  NEW TMGOVTITLES,ARRAY,TMGDOCSCANMODS
  NEW STIME SET STIME=$H
  NEW DONE SET DONE=0
  NEW OPTION
  SET OPTION("SKIP AUTOADD","PREVENTION")=1
  SET OPTION("SKIP AUTOADD","SOCIAL")=1
  SET OPTION("SKIP AUTOADD","CONTRACEPTION")=1
  SET OPTION("THREADS")=1
  SET OPTION("SKIP REWRITE")=1   
  NEW TIUMAX SET TIUMAX=+$ORDER(^TIU(8925,"!"),-1)
  NEW TIUIEN SET TIUIEN=0
  SET TIUIEN=+$GET(^TMG("TMP","SUMMALL^TMGC0Q04"))  ;"temp, remove later....
  FOR  SET TIUIEN=$ORDER(^TIU(8925,TIUIEN)) QUIT:(+TIUIEN'>0)!DONE  DO
  . IF $DATA(^TIU(8925,TIUIEN,0))=0 DO  QUIT
  . . WRITE !,"MADNESS!  ",TIUIEN,!
  . IF TIUIEN#5=0 DO
  . . DO PROGBAR^TMGUSRI2(TIUIEN,"PROGRESS: "_TIUIEN,0,TIUMAX,60,STIME)
  . . IF $$USRABORT^TMGUSRI2("parsing notes") SET DONE=1
  . DO SUMM1(TIUIEN,.OPTION)
  . SET ^TMG("TMP","SUMMALL^TMGC0Q04")=TIUIEN
  DO PROGBAR^TMGUSRI2(TIUIEN,"Done.",TIUMAX,TIUMAX,60,STIME)
  QUIT
  ;
ASKSUMM1 ;
  NEW OPTION
  SET OPTION("SKIP AUTOADD","PREVENTION")=1
  SET OPTION("SKIP AUTOADD","SOCIAL")=1
  SET OPTION("SKIP AUTOADD","CONTRACEPTION")=1
  SET OPTION("THREADS")=1
  NEW DIC,X,Y
  SET DIC=8925,DIC(0)="MAEQ"
  DO ^DIC WRITE !
  QUIT:+Y'>0
  DO SUMM1(+Y,.OPTION)
  QUIT
  ;
SUMM1(TIUIEN,OPTION)  ;
  NEW ARRAY,PROVIEN
  NEW ONLYOV SET ONLYOV=1
  SET PROVIEN=+$PIECE($GET(^TIU(8925,TIUIEN,12)),"^",2)
  IF (ONLYOV=1),($$ISOFFVST^TMGC0QT1(TIUIEN)=0) GOTO SUMM1DN
  IF (PROVIEN'=168)&(PROVIEN'=83) GOTO SUMM1DN  ;"<-- HARD CODED PROVIDER IEN'S
  DO SUMNOTE^TMGTIUP1(TIUIEN,.ARRAY,.OPTION)
  DO FILE1(TIUIEN,"ARRAY",.QUIET,.OPTION)  
  DO FILE1B(TIUIEN,.ARRAY,.QUIET,.OPTION)  
SUMM1DN ;
  QUIT
  ;  
TRIGALL ;
  ;"Purpose: process all TIU documents and trigger each one
  WRITE "==============================================",!
  WRITE "Office visit parser",!
  WRITE "..............................................",!
  WRITE "Will scan every TIU DOCUMENT and summarize",!
  WRITE "paragraph titles found in HPI and A&P sections.",!
  WRITE "Press ESC (perhaps multiple times) to escape loop.",!
  WRITE "==============================================",!,!
  NEW TMGOVTITLES,ARRAY,TMGDOCSCANMODS
  NEW STIME SET STIME=$H
  NEW DONE SET DONE=0
  NEW TIUMAX SET TIUMAX=+$ORDER(^TIU(8925,"!"),-1)
  NEW TIUIEN SET TIUIEN=0
  FOR  SET TIUIEN=$ORDER(^TIU(8925,TIUIEN)) QUIT:(+TIUIEN'>0)!DONE  DO
  . IF TIUIEN#5=0 DO
  . . DO PROGBAR^TMGUSRI2(TIUIEN,"PROGRESS: "_TIUIEN,0,TIUMAX,60,STIME)
  . . IF $$USRABORT^TMGUSRI2("parsing notes") SET DONE=1
  . DO TRIGGER1(TIUIEN,,.TMGDOCSCANMODS,1)
  DO PROGBAR^TMGUSRI2(TIUIEN,"Done.",TIUMAX,TIUMAX,60,STIME)
  QUIT
  ;
ASKTRIG ;
  ;"PURPOSE: Ask for TIU IEN and process that one document.
  NEW DIC,X,Y
  SET DIC=8925,DIC(0)="MAEQ"
  DO ^DIC WRITE !
  QUIT:+Y'>0
  DO TRIGGER1(+Y)
  QUIT
  ;
TEST1  ;"Ask for TIU IEN and SUMM that one document.
  NEW DIC,X,Y
  SET DIC=8925,DIC(0)="MAEQ"
  DO ^DIC WRITE !
  QUIT:+Y'>0
  DO SUMNOTE^TMGTIUP1(+Y,.ARRAY)
  KILL ARRAY(+Y,"FULL")
  ZWR ARRAY
  QUIT
  ;
TRIGGER1(TIUIEN,QUIET,TMGDOCSCANMODS,ONLYOV) ;
  ;"INPUT: (see TRIGJOB documentation below)
  ;"Job task off for faster foreground processing.
  SET ^TMG("EDDIE","TRIGGER1",TIUIEN)=""
  DO TRIGJOB(+$GET(TIUIEN))
  ;JOB TRIGJOB(+$GET(TIUIEN))::10  ;"Wait for up to 10 seconds for launching background task
  ;ELSE  DO  ;"$TEST is set to false if JOB timesout
  ;. DO TRIGJOB(+$GET(TIUIEN))  ;"run in foreground task
  QUIT 
  ;
TRIGJOB(TIUIEN,QUIET,TMGDOCSCANMODS,ONLYOV) ;
  ;"Purpose: to handle a trigger event for 1 document: scan, parse, store
  ;"         Event is fired in POST SIGNATURE CODE in TIU DOCUMENT definition.
  ;"         As of this writing: fired by: ACUTE MEDICAL ISSUES VISIT title
  ;"                                       COMPLETE PHYSICAL EXAM title
  ;"                                       FOCUSED OFFICE VISIT
  ;"                                       OFFICE VISIT title
  ;"                                       RECORDS SENT
  ;"                                       TEST title
  ;"NOTE: In addition to this event, TRIG1^TMGTIUT5 is called for all documents
  ;"        that decend from CLINICAL DOCUMENTS class.  
  ;"      A Fileman search for POST-SIGNATURE CODE field in file 8925.1 will show details. 
  ;"Input: TIUIEN --The IEN in file 8925 (TIU DOCUMENT)
  ;"       QUIET -- OPTIONAL.  If 1, then no screen output. DEFAULT=1
  ;"            To get back errors, PASS QUIET BY REFERENCE.  Stored as follows:
  ;"            QUIET(TIUIEN)=ErrorString
  ;"            QUIET(TIUIEN,SECTION,#)=SubRecErrorString
  ;"       TMGDOCSCANMODS -- OPTIONAL.  Holding array for title modifiers, for effeciency with looping runs.
  ;"       ONLYOV -- OPTIONAL.  If 1, then test is made to see if note
  ;"                  is an office visit type note, and ignores if not
  DO 
  . NEW TMPSTORE SET TMPSTORE=$NAME(^TMG("TMP","POST-SIGNATURE","TRIGGER1^TMGV0Q04"))  
  . KILL @TMPSTORE SET @TMPSTORE@("LAST CALL TIME")=$$NOW^XLFDT        
  SET TIUIEN=+$GET(TIUIEN) GOTO:(TIUIEN'>0) TGDN
  DO FIREINH1^TMGTIUT4(.TIUIEN)  ;"Fire inherited post-signature event-handlers first.  //kt 5/2016
  SET ONLYOV=$GET(ONLYOV,0)
  DO FILEIMM^TMGTIUT5(.TIUIEN)  ;"<--- I think is duplicate.  TRIG1^TMGTIUT5 -> FILEIMM^TMGTIUT5
  NEW ARRAY,PROVIEN
  SET PROVIEN=+$PIECE($GET(^TIU(8925,TIUIEN,12)),"^",2)
  IF (ONLYOV=1),($$ISOFFVST^TMGC0QT1(TIUIEN)=0) GOTO TGDN
  IF (PROVIEN'=168)&(PROVIEN'=83) GOTO TGDN  ;"<-- HARD CODED PROVIDER IEN'S
  DO SUMNOTE^TMGTIUP1(TIUIEN,.ARRAY)
  DO FILE1(TIUIEN,"ARRAY",.QUIET)  
  DO FILE1B(TIUIEN,.ARRAY,.QUIET)  
  DO CHK6CIT(TIUIEN)
TGDN  ;
  IF +^TIU(8925,TIUIEN,0)=$$CNSLTPT() DO CHECKDT(TIUIEN)  ;"Fix TMGCNSLT Xref entry for certain .01 field entries
  DO ADDLSIGN(TIUIEN)  ;"11/23/21
  QUIT
  ;
  ;"SUMNOTE(TIUIEN,ARRAY) ;"Purpose: To take a given note in file 8925, and parse HPI and A&P into array
  ;"        DO SUMNOTE^TMGTIUP1(.TIUIEN,.ARRAY)
  ;"        QUIT
  ;"        ;  
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
  ;"Input: TIUIEN -- The IEN in file 8925 (TIU DOCUMENT)
  ;"       ARRAY -- PASS BY REFERENCE.  Info to be filed.  Format as below
  ;"         @PARRAY@(TIUIEN,"THREAD",IDX#)=Topic^Thread text
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
CLRFRMTU(TIUIEN) ;"Clear 1 record for linked TIU DOCUMENT.
  ;
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
XRCONSLT(X,DA,MODE)  ;"XREF entry code for file 8925, field .01 XREF "ATMGCNSLT"
  ;"Purpose: XRef set/kill function for TIU DOCUMENT field, .01 field 
  ;"NOTE: This xref will only be SET for values when X --> "CONSULT / PATIENT CONTINUITY SUMMARY"
  ;"      Because this code is site specific, on our system, this title is IEN #1486"
  ;"      And THIS WILL BE HARD CODED HEREIN
  ;"Input: X -- the value of the .01 field of file 8925 (will be pointer to file 8925.1)
  ;"       DA -- the IEN of the document being edited.
  ;"       MODE -- Shoulde be "S" for SET statement, or "K" for a KILL statement
  ;"Output: Will effect creation of XREF like this: ^TIU(8925,"ATMGCNSLT",EpisodeBeginDate,DocIEN)=""
  ;"RESULT: none
  IF $GET(X)'=$$CNSLTPT() QUIT
  IF +$GET(DA)'>0 QUIT
  SET DT=+$PIECE($GET(^TIU(8925,+DA,0)),"^",7) 
  IF DT'>0 QUIT
  SET MODE=$GET(MODE)
  IF MODE="K" KILL ^TIU(8925,"ATMGCNSLT",DT,+DA)
  ELSE  IF MODE="S" SET ^TIU(8925,"ATMGCNSLT",DT,+DA)=""
  QUIT
  ;
CNSLTPT() ;"get hard-coded pointer for CONSULT / PATIENT CONTINUITY SUMMARY
  QUIT 1486
  ;
SETOLD  ;
  ;"Purpose: Set the ATMGCNSLT index 
  NEW TIUIEN SET TIUIEN=0
  FOR  SET TIUIEN=$ORDER(^TIU(8925,"B",$$CNSLTPT(),TIUIEN)) QUIT:TIUIEN'>0  DO
  . WRITE "SETTING TIUIEN "_TIUIEN,!
  . DO XRCONSLT($$CNSLTPT(),TIUIEN,"S")
  ;"Continue into CKDTOLD to now place DueDates into index
CKDTOLD ;"PROCESS DOCS ALREADY SIGNED.
  NEW DOCDT SET DOCDT=0
  FOR  SET DOCDT=$ORDER(^TIU(8925,"ATMGCNSLT",DOCDT)) QUIT:(+DOCDT'>0)  DO
  . NEW TIUIEN SET TIUIEN=0
  . FOR  SET TIUIEN=$ORDER(^TIU(8925,"ATMGCNSLT",DOCDT,TIUIEN)) QUIT:(+TIUIEN'>0)  DO
  . . NEW STATUS SET STATUS=$$GET1^DIQ(8925,TIUIEN,.05)
  . . WRITE TIUIEN," --> ",STATUS,!
  . . IF STATUS'="COMPLETED" QUIT
  . . DO CHECKDT(TIUIEN)
  QUIT
  ;
CHECKDT(TIUIEN)   ;
  ;"Purpose: After note has been signed, this code will be called IF .01 field is CONSULT / PATIENT CONTINUITY SUMMARY
  ;"         CALLED FROM: TRIGGER1^TMGC0Q04
  ;"                      SETOLD^TMGC0Q04 / CKDTOLD^TMGC0Q04
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
TESTPT  ;"SHOW INFORMATION FOR A SELECTED PATIENT.  
  NEW X,Y,DIC,ADFN,IEN22719 SET DIC=2,DIC(0)="MAEQ" 
TP1 ;
  DO ^DIC WRITE ! IF Y'>0 QUIT
  SET ADFN=+Y,IEN22719=0
  FOR  SET IEN22719=$ORDER(^TMG(22719,"DFN",ADFN,IEN22719)) QUIT:+IEN22719'>0  DO
  . DO SHOWINFO(IEN22719)
  NEW % SET %=2 WRITE "Pick another patient" DO YN^DICN
  IF %=1 GOTO TP1 
  QUIT
SHOWINFO(IEN22719) ;
  WRITE "NOTE: ",$$GET1^DIQ(22719,IEN22719,.03),!
  WRITE "  HPI",! DO SHOWSUB(IEN22719,2)
  WRITE "  PMH",! DO SHOWSUB(IEN22719,3)         
  QUIT
SHOWSUB(IEN22719,NODE) ;
  NEW STR SET STR=""
  FOR  SET STR=$ORDER(^TMG(22719,IEN22719,NODE,"B",STR)) QUIT:STR=""  DO
  . WRITE "  --",STR,!
  QUIT
  ;
OUTXFRM(Y) ;"OUTPUT TRANSFORM FOR .01 FIELD FOR FILE 22719  **BE CAREFUL WITH EDITS!**
  ;"INPUT -- Y THE VALUE OF THE .01 FIELD
  N ZZ,REF
  NEW IEN SET IEN=+Y
  D GETS^DIQ(8925,IEN,".01;.02;.07","E","ZZ")
  S REF="ZZ(8925,"""_IEN_","")"
  SET Y=$G(@REF@(.01,"E"))_" - "_$GET(@REF@(.02,"E"))_" - "
  SET Y=Y_$P($G(@REF@(.07,"E")),"@",1)_" (`"_IEN_")"
  QUIT Y
  ;
CHK6CIT(TIUIEN)  ;"CHECK THE NOTE FOR 6CIT TEST
  ;"IF FOUND, GET THE RESULT AND SAVE A HF WITH THE
  ;"RESULT AS A COMMENT
  NEW IDX SET IDX=0
  NEW FOUND SET FOUND=0
  NEW SCORE SET SCORE=-1
  NEW TMGRESULT SET TMGRESULT=""
  FOR  SET IDX=$O(^TIU(8925,TIUIEN,"TEXT",IDX)) QUIT:(IDX'>0)!(SCORE'=-1)  DO
  . NEW LINE SET LINE=$G(^TIU(8925,TIUIEN,"TEXT",IDX,0))
  . SET LINE=LINE_$G(^TIU(8925,TIUIEN,"TEXT",IDX+1,0)) ;"GET CURRENT + NEXT LINE, IN CASE OF TRUNCATION   4/15/21
  . IF LINE["6CIT - Kingshill Version 2000" SET FOUND=1
  . IF FOUND=1 DO
  . . IF LINE["6CIT score = " DO
  . . . SET SCORE=$P(LINE,"/",1)
  . . . SET SCORE=+$P(SCORE,"6CIT score = ",2)
  IF SCORE'=-1 DO
  . ;"HERE WE ARE GOING TO ADD THE HF
  . ;"WRITE "    **ADDING 6CIT HF",!  ;"REMOVE THIS AFTERWARD
  . NEW TIUDATE,PROVIEN,PROVNAME,TMGDFN
  . SET TMGDFN=$P($G(^TIU(8925,TIUIEN,0)),"^",2)
  . SET PROVIEN=$P($G(^TIU(8925,TIUIEN,12)),"^",2)
  . SET TIUDATE=$P($G(^TIU(8925,TIUIEN,0)),"^",7)
  . SET PROVNAME=$P($G(^VA(200,PROVIEN,0)),"^",1)
  . NEW ARRDATA	
  . SET ARRDATA(1)="HDR^0^^6;"_TIUDATE_";A"
  . SET ARRDATA(2)="VST^DT^"_TIUDATE
  . SET ARRDATA(3)="VST^PT^"_TMGDFN
  . SET ARRDATA(4)="VST^HL^6"
  . SET ARRDATA(5)="VST^VC^A"
  . SET ARRDATA(6)="PRV^"_PROVIEN_"^^^"_PROVNAME_"^1"
  . SET ARRDATA(7)="HF+^2703^^TMG 6CIT DONE^@^^^^^1^"
  . SET ARRDATA(8)="COM^1^"_SCORE
  . DO SAVE^ORWPCE(.TMGRESULT,.ARRDATA,0,6)
  QUIT
  ;"
ADD6CIT  ;"
  NEW TIUIEN SET TIUIEN=0
  FOR  SET TIUIEN=$O(^TIU(8925,TIUIEN)) QUIT:+TIUIEN'>0  DO
  . WRITE "CHECKING TIUIEN: ",TIUIEN,!
  . DO CHK6CIT(TIUIEN)
  QUIT
  ;"
ADDLSIGN(TIUIEN)  ;"Add an addl signer when needed  ;"11/23/21
  NEW USERARR,RESULT
  NEW TITLEIEN SET TITLEIEN=$P($G(^TIU(8925,TIUIEN,0)),"^",1)
  ;"IF TITLEIEN=1416 DO
  ;". SET USERARR(1)="150^Hagood,Eddie L^- System Manager"
  ;". SET USERARR(2)="123^Hensley,Tammy G"
  IF TITLEIEN=1420 DO
  . SET USERARR(1)="150^Hagood,Eddie L^- System Manager"
  IF (TITLEIEN=1470)!(TITLEIEN=1471) SET USERARR(1)="259^Shipley,Sabrina^- CMA"
  IF $D(USERARR(1)) DO IDSIGNRS^TIULX(.RESULT,TIUIEN,.USERARR)
  QUIT
  ;"  
  