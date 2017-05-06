TMGC0Q04 ;TMG/kst/TMG meanful use util code ;8/12/12, 2/2/14
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
TRIGGER1(TIUIEN,QUIET,TMGDOCSCANMODS,ONLYOV) ;
        ;"Purpose: to handle a trigger event for 1 document: scan, parse, store
        ;"         Event is fired in POST SIGNATURE CODE in TIU DOCUMENT definition for consult title.
        ;"         As of this writing: fired by: ACUTE MEDICAL ISSUES VISIT title
        ;"                                       COMPLETE PHYSICAL EXAM title
        ;"                                       CONSULT /PATIENT CONTINUITY SUMMARY title
        ;"                                       OFFICE VISIT title
        ;"                                       TEST title
        ;"Input: TIUIEN --The IEN in file 8925 (TIU DOCUMENT)
        ;"       QUIET -- OPTIONAL.  If 1, then no screen output. DEFAULT=1
        ;"            To get back errors, PASS QUIET BY REFERENCE.  Stored as follows:
        ;"            QUIET(TIUIEN)=ErrorString
        ;"            QUIET(TIUIEN,SECTION,#)=SubRecErrorString
        ;"       TMGDOCSCANMODS -- OPTIONAL.  Holding array for title modifiers, for effeciency with looping runs.
        ;"       ONLYOV -- OPTIONAL.  If 1, then test is made to see if note
        ;"                  is an office visit type note, and ignores if not
        SET TIUIEN=+$GET(TIUIEN) GOTO:(TIUIEN'>0) TGDN
        DO FIREINH1^TMGTIUT4(.TIUIEN)  ;"Fire inherited post-signature event-handlers first.  //kt 5/2016
        SET ONLYOV=$GET(ONLYOV,0)
        NEW ARRAY,PROVIEN
        SET PROVIEN=+$PIECE($GET(^TIU(8925,TIUIEN,12)),"^",2)
        IF (ONLYOV=1),($$ISOFFVST^TMGC0QT1(TIUIEN)=0) GOTO TGDN
        IF (PROVIEN'=168)&(PROVIEN'=83) GOTO TGDN  ;"<-- HARD CODED PROVIDER IEN'S
        DO SUMNOTE^TMGTIUP1(TIUIEN,.ARRAY)
        DO FILE1(TIUIEN,"ARRAY",.QUIET)  ;
TGDN    IF +^TIU(8925,TIUIEN,0)=$$CNSLTPT() DO CHECKDT(TIUIEN)  ;"Fix TMGCNSLT Xref entry for certain .01 field entries
        QUIT
        ;
        ;"SUMNOTE(TIUIEN,ARRAY) ;"Purpose: To take a given note in file 8925, and parse HPI and A&P into array
        ;"        DO SUMNOTE^TMGTIUP1(.TIUIEN,.ARRAY)
        ;"        QUIT
        ;"        ;
FILE1(TIUIEN,PARRAY,QUIET)  ;
        ;"Purpose: store parsed array into Fileman file 22719
        ;"Input: TIUIEN -- The IEN in file 8925 (TIU DOCUMENT)
        ;"       PARRAY -- PASS BY NAME.  Info to be filed.  Format as below
        ;"         @PARRAY@(TIUIEN,"HPI",Item#)=Topic^First line of paragraph
        ;"         @PARRAY@(TIUIEN,"A&P",Item#)=Topic^First line of paragraph
        ;"       QUIET -- OPTIONAL.  If 1, then no screen output. DEFAULT=1
        ;"            To get back errors, PASS QUIET BY REFERENCE.  Stored as follows:
        ;"            QUIET(TIUIEN)=ErrorString
        ;"            QUIET(TIUIEN,SECTION,#)=SubRecErrorString
        ;"Results: NONE
        ;
        SET QUIET=+$GET(QUIET,1)
        NEW RECIEN SET RECIEN=+$ORDER(^TMG(22719,"B",TIUIEN,""))
        IF RECIEN>0 DO CLEAR1(RECIEN) GOTO F1B
ADD     NEW TMGFDA,TMGMSG,TMGIEN
        SET TMGIEN(1)=TIUIEN
        SET TMGFDA(22719,"+1,",.01)="`"_TIUIEN
        SET TMGFDA(22719,"+1,",1)="NOW"
        DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO F1DN
        . NEW ERRSTR SET ERRSTR=$$GETERRST^TMGDEBU2(.TMGMSG)
        . IF QUIET SET QUIET(TIUIEN)=ERRSTR
        . ELSE  WRITE !,ERRSTR,!
        SET RECIEN=+$GET(TMGIEN(1))
        IF RECIEN'>0 DO  QUIT
        . NEW ERRSTR SET ERRSTR="Unable to find record to file into."
        . IF QUIET SET QUIET(TIUIEN)=ERRSTR
        . ELSE  WRITE !,ERRSTR,!
F1B     NEW PROVIEN SET PROVIEN=+$PIECE($GET(^TIU(8925,TIUIEN,12)),"^",2)
        ;"IF (PROVIEN'=168)&(PROVIEN'=83) GOTO F1DN
        ;"IF $$ISOFFVST^TMGC0QT1(TIUIEN)=0 GOTO F1DN
        NEW SECTION SET SECTION=""
        FOR  SET SECTION=$ORDER(@PARRAY@(TIUIEN,SECTION)) QUIT:SECTION=""  DO
        . NEW FLD,FILE SET FLD=0
        . IF SECTION="HPI" SET FLD=2,FILE=22719.02
        . ELSE  IF SECTION="A&P" SET FLD=3,FILE=22719.03
        . IF FLD'>0 QUIT
        . NEW COUNT SET COUNT=0
        . FOR  SET COUNT=$ORDER(@PARRAY@(TIUIEN,SECTION,COUNT)) QUIT:COUNT'>0  DO
        . . NEW INFO SET INFO=$GET(@PARRAY@(TIUIEN,SECTION,COUNT))
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
        . . . IF QUIET SET QUIET(TIUIEN,SECTION,COUNT)=ERRSTR
        . . . ELSE  WRITE !,"TIUIEN=#",TIUIEN," ",$$GETERRST^TMGDEBU2(.TMGMSG),!
F1DN    QUIT
        ;
CLRFRMTU(TIUIEN) ;"Clear 1 record for linked TIU DOCUMENT.
        ;
        SET SAVPARENT=+$GET(SAVPARNT,0)
        SET TIUIEN=+$GET(TIUIEN)
        IF TIUIEN'>0 GOTO CFM
        NEW RECIEN SET RECIEN=+$ORDER(^TMG(22719,"B",TIUIEN,""))
        GOTO:RECIEN'>0 C1DN
        DO CLEAR1(RECIEN)
CFM     QUIT
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
C1DN    QUIT
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
TP1     DO ^DIC WRITE ! IF Y'>0 QUIT
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
        ;"=================================================================
        ;"=================================================================
        ;" Older code below. Delete if no problems by 4/11/2018
        ;"=================================================================
        ;"=================================================================
        ;
  ;"--VIEWx   ;"VIEW TITLES -- DEPRECIATED
  ;"--        NEW J SET J=+$ORDER(^TMP("NOTE SUMMARY",0))
  ;"--        NEW TMGPICK,TMGRESULT
  ;"--        IF J'>0 WRITE "CAN'T FIND DATA.",! QUIT
  ;"--        NEW TITLE,%
  ;"--VT1x    SET TITLE="" FOR  SET TITLE=$ORDER(^TMP("NOTE SUMMARY",J,"B",TITLE)) QUIT:TITLE=""  DO
  ;"--        . SET TMGPICK(TITLE)=""
  ;"--        DO SELECTR2^TMGUSRI3("TMGPICK","TMGRESULT","Pick title(s) to explore <Esc><Esc> when done.")
  ;"--        KILL TMGPICK
  ;"--        SET TITLE=""
  ;"--        FOR  SET TITLE=$ORDER(TMGRESULT(TITLE)) QUIT:TITLE=""  DO
  ;"--        . NEW IEN SET IEN=0
  ;"--        . FOR  SET IEN=$ORDER(^TMP("NOTE SUMMARY",J,"B",TITLE,IEN)) QUIT:(+IEN'>0)  DO
  ;"--        . . NEW DFN SET DFN=$PIECE($GET(^TIU(8925,IEN,0)),"^",2)
  ;"--        . . NEW PTNAME SET PTNAME=$PIECE($GET(^DPT(DFN,0)),"^",1)
  ;"--        . . SET TMGPICK(TITLE_" .. "_PTNAME)=IEN_"^"_PTNAME
  ;"--        KILL TMGRESULT
  ;"--        IF $DATA(TMGPICK) DO
  ;"--        . IF $$LISTCT^TMGMISC2("TMGPICK")=1 DO
  ;"--        . . MERGE TMGRESULT=TMGPICK
  ;"--        . ELSE  DO
  ;"--        . . DO SELECTR2^TMGUSRI3("TMGPICK","TMGRESULT","Pick title(s) to explore <Esc><Esc> when done.")
  ;"--        IF $DATA(TMGRESULT) DO
  ;"--        . DO ZWRITE^TMGZWR("TMGRESULT")
  ;"--        . NEW STR SET STR=""
  ;"--        . FOR  SET STR=$ORDER(TMGRESULT(STR)) QUIT:STR=""  DO
  ;"--        . . NEW S2 SET S2=$GET(TMGRESULT(STR))
  ;"--        . . DO SHOW1X(J,+S2)
  ;"--        . . DO PRESS2GO^TMGUSRI2
  ;"--        SET %=1
  ;"--        WRITE "View more" DO YN^DICN WRITE !
  ;"--        IF %=1 GOTO VT1x
  ;"--        QUIT
  ;"--        ;
  ;"--        ;"=================================================================
  ;"--        ;
  ;"--CHKALL  ;"--depreciated
  ;"--        WRITE "==============================================",!
  ;"--        WRITE "Office visit parser",!
  ;"--        WRITE "..............................................",!
  ;"--        WRITE "Will scan every TIU DOCUMENT and summarize",!
  ;"--        WRITE "paragraph titles found in HPI and A&P sections.",!
  ;"--        WRITE "Press ESC (perhaps multiple times) to escape loop.",!
  ;"--        WRITE "==============================================",!,!
  ;"--        NEW %
  ;"--        IF $DATA(^TMP("NOTE SUMMARY")) DO  GOTO:(%=-1) CADN
  ;"--        . SET %=2
  ;"--        . WRITE "KILL prior parsings"
  ;"--        . DO YN^DICN WRITE !
  ;"--        . IF %=-1 QUIT
  ;"--        . IF %=1 KILL ^TMP("NOTE SUMMARY")
  ;"--        NEW TMGOVTITLES,ARRAY
  ;"--        NEW STIME SET STIME=$H
  ;"--        NEW DONE SET DONE=0
  ;"--        NEW TIUMAX SET TIUMAX=+$ORDER(^TIU(8925,"!"),-1)
  ;"--        NEW TIUIEN SET TIUIEN=0
  ;"--        FOR  SET TIUIEN=$ORDER(^TIU(8925,TIUIEN)) QUIT:(+TIUIEN'>0)!DONE  DO
  ;"--        . IF TIUIEN#50=0 DO
  ;"--        . . DO PROGBAR^TMGUSRI2(TIUIEN,"PROGRESS: "_TIUIEN,0,TIUMAX,60,STIME)
  ;"--        . . IF $$USRABORT^TMGUSRI2("parsing notes") SET DONE=1
  ;"--        . NEW PROVIEN SET PROVIEN=+$PIECE($GET(^TIU(8925,TIUIEN,12)),"^",2)
  ;"--        . IF (PROVIEN'=168)&(PROVIEN'=83) QUIT
  ;"--        . IF $$ISOFFVST^TMGC0QT1(TIUIEN)=0 DO  QUIT
  ;"--        . . SET ARRAY(TIUIEN)=""
  ;"--        . DO SUMNOTE^TMGTIUP1(TIUIEN,.ARRAY)
  ;"--        DO PROGBAR^TMGUSRI2(TIUIEN,"Done.",TIUMAX,TIUMAX,60,STIME)
  ;"--        WRITE !,"Indexing all terms used...",!
  ;"--        SET TIUIEN=0 SET TIUMAX=$ORDER(ARRAY(""),-1)
  ;"--        SET STIME=$H
  ;"--        FOR  SET TIUIEN=$ORDER(ARRAY(TIUIEN)) QUIT:(+TIUIEN'>0)!DONE  DO
  ;"--        . IF TIUIEN#50=0 DO
  ;"--        . . DO PROGBAR^TMGUSRI2(TIUIEN,"PROGRESS: Doc #"_TIUIEN,0,TIUMAX,60,STIME)
  ;"--        . . IF $$USRABORT^TMGUSRI2("parsing notes") SET DONE=1
  ;"--        . NEW SECTION SET SECTION=""
  ;"--        . FOR  SET SECTION=$ORDER(ARRAY(TIUIEN,SECTION)) QUIT:SECTION=""  DO
  ;"--        . . NEW COUNT SET COUNT=0
  ;"--        . . FOR  SET COUNT=$ORDER(ARRAY(TIUIEN,SECTION,COUNT)) QUIT:COUNT'>0  DO
  ;"--        . . . NEW TITLE SET TITLE=$PIECE($GET(ARRAY(TIUIEN,SECTION,COUNT)),"^",1) QUIT:TITLE=""
  ;"--        . . . SET ARRAY("B",$E(TITLE,1,60),TIUIEN)=""
  ;"--        DO PROGBAR^TMGUSRI2(TIUIEN,"Done.",TIUMAX,TIUMAX,60,STIME)
  ;"--        WRITE !
  ;"--        ;
  ;"--        MERGE ^TMP("NOTE SUMMARY",$J)=ARRAY
  ;"--        WRITE "Stored information in ^TMP(""NOTE SUMMARY"",",$J,",*)",!
  ;"--VIEW    NEW % SET %=2
  ;"--        WRITE "View topic titles" DO YN^DICN WRITE !
  ;"--        IF %=1 DO VIEWTTLS^TMGC0Q05
  ;"--        SET %=1
  ;"--        WRITE "File topic titles into file 22719" DO YN^DICN WRITE !
  ;"--        IF %=1 DO FILETTLS(.ARRAY)
  ;"--        WRITE "Goodbye",!
  ;"--CADN    QUIT
  ;"--        ;
  ;"--SHOW1X(J,TIUIEN) ;"--depreciated
  ;"--        ;"Purpose:  Show 1 record from ^TMP("NOTE SUMMARY",J,TIUIEN)
  ;"--        ;"Input: J -- the job number of the task that created the summary
  ;"--        ;"       TIUIEN -- The document number that was the source of the information.
  ;"--        IF $DATA(^TMP("NOTE SUMMARY",J,TIUIEN))=0 GOTO S1DN
  ;"--        WRITE !,$$GET1^DIQ(8925,TIUIEN_",",.02),!
  ;"--        WRITE $$GET1^DIQ(8925,TIUIEN_",",.01),"  ",$$GET1^DIQ(8925,TIUIEN_",",.07)," (#",TIUIEN,")",!
  ;"--        WRITE "--------------------------------------------------",!
  ;"--        NEW SECTION
  ;"--        FOR SECTION="HPI","A&P" DO
  ;"--        . WRITE SECTION,"-------",!
  ;"--        . NEW I SET I=0
  ;"--        . FOR  SET I=$ORDER(^TMP("NOTE SUMMARY",J,TIUIEN,SECTION,I)) QUIT:+I'>0  DO
  ;"--        . . NEW ALINE SET ALINE=$GET(^TMP("NOTE SUMMARY",J,TIUIEN,SECTION,I))
  ;"--        . . WRITE "  * ",$PIECE(ALINE,"^",1),": ",$PIECE(ALINE,"^",2),!
  ;"--S1DN    WRITE "--------------------------------------------------",!
  ;"--        QUIT
  ;"--        ;
  ;"--JUSTFILE ;"--depreciated
  ;"--        NEW ARRAY SET ARRAY=$NAME(^TMP("NOTE SUMMARY",$J))
  ;"--        DO FILETTLS("ARRAY")
  ;"--        QUIT
  ;"--        ;
  ;"--FILETTLS(PARRAY,QUIET)  ;"--depreciated
  ;"--        ;"Purpose: store parsed array into Fileman file 22719
  ;"--        ;"       PARRAY -- PASS BY NAME.  Info to be filed.  Format as below
  ;"--        ;"         @PARRAY@(TIUIEN,"HPI",Item#)=Topic^First line of paragraph
  ;"--        ;"         @PARRAY@(TIUIEN,"A&P",Item#)=Topic^First line of paragraph
  ;"--        ;"       QUIET -- OPTIONAL.  If 1, then no screen output.  DEFAULT=0
  ;"--        ;"            To get back errors, PASS QUIET BY REFERENCE.  Stored as follows:
  ;"--        ;"            QUIET(TIUIEN)=ErrorString
  ;"--        ;"            QUIET(TIUIEN,SECTION,#)=SubRecErrorString
  ;"--        ;"Results: NONE
  ;"--        ;
  ;"--        WRITE "Filing into Fileman file 22719, skipping any record already present",!
  ;"--        NEW TIUIEN,STIME,TIUMAX
  ;"--        SET QUIET=+$GET(QUIET,0)
  ;"--        NEW DONE SET DONE=0
  ;"--        SET TIUIEN=0 SET TIUMAX=$ORDER(@PARRAY@("!"),-1)
  ;"--        SET STIME=$H
  ;"--        FOR  SET TIUIEN=$ORDER(@PARRAY@(TIUIEN)) QUIT:(+TIUIEN'>0)!DONE  DO
  ;"--        . IF TIUIEN#5=0 DO
  ;"--        . . DO PROGBAR^TMGUSRI2(TIUIEN,"PROGRESS: Doc #"_TIUIEN,0,TIUMAX,60,STIME)
  ;"--        . . IF $$USRABORT^TMGUSRI2("parsing notes") SET DONE=1
  ;"--        . IF $DATA(^TMG(22719,"B",TIUIEN)) DO  QUIT
  ;"--        . . ;"WRITE !,"Skipping #",TIUIEN,". Data already filed."
  ;"--        . DO FILE1(TIUIEN,PARRAY,.QUIET)  ;
  ;"--        DO PROGBAR^TMGUSRI2(TIUIEN,"Done.",TIUMAX,TIUMAX,60,STIME)
  ;"--        ;
  ;"--TEMP2  ;"KILL LATER.  
  ;"--       NEW DOCDT SET DOCDT=0
  ;"--       FOR  SET DOCDT=$ORDER(^TIU(8925,"ATMGCNSLT",DOCDT)) QUIT:(+DOCDT'>0)  DO
  ;"--       . NEW TIUIEN SET TIUIEN=0
  ;"--       . FOR  SET TIUIEN=$ORDER(^TIU(8925,"ATMGCNSLT",DOCDT,TIUIEN)) QUIT:(+TIUIEN'>0)  DO
  ;"--       . . NEW STATUS SET STATUS=$$GET1^DIQ(8925,TIUIEN,.05)
  ;"--       . . NEW LIST 
  ;"--       . . IF $$HASADDEN^TMGC0QT2(TIUIEN,.LIST)=0 DO  QUIT
  ;"--       . . . WRITE TIUIEN," (NO ADDENUM)",!
  ;"--       . . WRITE TIUIEN," HAS ADDENDUM: ",$ORDER(LIST(0)),!
  ;"--       . . NEW ADDENDIEN SET ADDENDIEN=0
  ;"--       . . FOR  SET ADDENDIEN=$ORDER(LIST(ADDENDIEN)) QUIT:(+ADDENDIEN'>0)  DO
  ;"--       . . . IF $$RECSENT^TMGC0QT2(ADDENDIEN)=0 QUIT
  ;"--       . . . WRITE "  ",ADDENDIEN," --> RECORDS SENT",!
  ;"--       QUIT
