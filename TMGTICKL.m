TMGTICKL ;TMG/kst-Tickler Text objects for use in CPRS ;08/09/12, 2/2/14, 3/24/21
         ;;1.0;TMG-LIB;**1**;08/27/08
 ;
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"TMG Tickler text object and surrounding support code.
 ;"
 ;"These are bits of code that return text to be included in progress notes etc.
 ;"They are called when the user puts text like this in a note:
 ;"     ... Mrs. Jone's vitals today are |VITALS|, measured in the office...
 ;"     'VITALS' would be a TIU TEXT OBJECT, managed through menu option TIUFJ CREATE OBJECTS MGR
 ;
 ;"---------------------------------------------------------------------------
 ;"PUBLIC FUNCTIONS
 ;"---------------------------------------------------------------------------
 ;"$$TICKLER^TMGTICKL(TMGDFN,.TIU) -- Entry point for TIU Text object caller
 ;"HANDLE^TMGTICKL -- entry point for Task to handle tickler messages, called at scheduled intervals
 ;"ERRSHOW^TMGTICKL -- Handle Alerts, showing details about error.
 ;
 ;"---------------------------------------------------------------------------
 ;"PRIVATE FUNCTIONS
 ;"---------------------------------------------------------------------------
 ;"$$HasTickler(DocIEN,DateStr) -- return IF TIU DOCUMENT contains the signals for a TICKLER message.
 ;"SendAddendum(DocIEN,AuthorIEN,TklIEN,TMGWP) -- place an addendum to the specified note with message
 ;"SendErrAddendum(DocIEN,TklIEN,TMGMSG) -- send an addendum to note showing database error.
 ;"SendAlert(UserIEN,TklIEN,Msg,TMGMSG) -- send a message alert to the user (for error reporting)
 ;"RescheduleTask -- reschedule task for handling the next cycle of tickler messages.
 ;"PRS2CONT -- provide a 'press key to continue' action
 ;
 ;"---------------------------------------------------------------------------
 ;"---------------------------------------------------------------------------
 ;
TICKLER(TMGDFN) ;
        ;"Purpose: A call point for TIU objects, to launch a tickler for the given note.
        ;"Input: TMGDFN -- the patient's unique ID (record#)
        ;"Result: returns text that will be put into the note in CPRS
        ;
        NEW RESULT,X,Y
        SET TMGDFN=+$GET(TMGDFN)
        IF TMGDFN=0 DO  GOTO TKDone
        . SET RESULT="ERROR: DFN not defined.  Contact IT support (Source: TMGTICKL.m)"
        ;
        SET RESULT=""
        SET RESULT=RESULT_" ======= [TICKLER MESSGE] ======="_$CHAR(13)_$CHAR(10)
        SET RESULT=RESULT_" #DUE#: Put-DUE-DATE-here        "_$CHAR(13)_$CHAR(10)
        SET RESULT=RESULT_" #RECIPIENT#: MYSELF             "_$CHAR(13)_$CHAR(10) ;"Added 8/9/12
        SET RESULT=RESULT_" ================================"_$CHAR(13)_$CHAR(10)
        SET RESULT=RESULT_" Message: ...                    "_$CHAR(13)_$CHAR(10)
        SET RESULT=RESULT_"                                 "_$CHAR(13)_$CHAR(10)
        SET RESULT=RESULT_" ================================"_$CHAR(13)_$CHAR(10)
        SET RESULT=RESULT_$CHAR(13)_$CHAR(10)
        ;
        ;"Create an entry in TMG TICKLER file, for later processing.
        ;"Processing will need to wait until after document is signed, so that due date is fixed.
        DO NOW^%DTC
        NEW TMGFDA,TMGMSG,TMGIEN
        SET TMGFDA(22705.5,"+1,",.01)=TMGDFN ;"IEN in PATIENT file
        SET TMGFDA(22705.5,"+1,",2)="U"  ;"U=Unsigned
        SET TMGFDA(22705.5,"+1,",3)=DUZ  ;"Current user
        SET TMGFDA(22705.5,"+1,",1)=%    ;"'Due Date', actually holds date created, until matched with TIU Document
        DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO TKDone
        . SET RESULT="ERROR: Fileman error creating Tickler Message.  Contact IT support (Source: TMGTICKL.m)"
        . SET RESULT=RESULT_$$GETERRST^TMGDEBU2(.TMGMSG)
        ;
TKDone  QUIT RESULT
        ;
        ;
HANDLE  ;"Purpose: An entry point for Taskman Task to handle tickler messages
        ;"         This will be called at scheduled intervals
        ;"LOCK +^TMG("HANDLE^TMGTICKL","RUNNING"):1  ;"elh added Try to stop concurrent tasks
        ;"ELSE  QUIT  ;"elh if lock timeouts, quit
        ;"IF $GET(^TMG("HANDLE^TMGTICKL","RUNNING"))=1 ;"quit if already running condition
        ;"SET ^TMG("HANDLE^TMGTICKL","RUNNING")=1 ;"elh added to signify running condition
        ;"DO RescheduleTask
HAND2   NEW X,%,TMGFDA,TMGMSG
        DO NOW^%DTC  ;"get current time into %
        ;"Remove the code below, it is for testing purposes only
        SET ^TMG("EDDIE","TMGTICKL",$J,"RUNTIME",%)=""
        ;"END DEBUG CODE
        SET TMGFDA(22705.4,"1,",3)=%
        DO FILE^DIE("","TMGFDA","TMGMSG")  ;"set time of last scan in 22705.4
        ;
        NEW DIC,Y
        SET DIC=8925.6 ;"TIU STATUS file
        SET X="COMPLETED"
        DO ^DIC
        NEW STATUSIEN SET STATUSIEN=+Y
        IF STATUSIEN'>0 DO  GOTO HandlDone
        . DO SendAlert(DUZ,0,"Tickler Error: Can't find IEN for 'COMPLETED' status")
        ;
        ;"For each TMG TICKLER entry that is UNSIGNED, and missing a DOCUMENT
        ;"pointer, a scan of all a patient's documents is carried out, looking
        ;"for one with a Tickler Message that has not already been noted.  When
        ;"found, the DOCUMENT pointer is stored.  Search is by date, in
        ;"reverse chronological order (most recent first).
        NEW TklIEN SET TklIEN=0
        FOR   SET TklIEN=$ORDER(^TMG(22705.5,"S","U",TklIEN)) QUIT:(+TklIEN'>0)  DO
        . NEW FOUND SET FOUND=0
        . NEW DocIEN SET DocIEN=+$PIECE($GET(^TMG(22705.5,TklIEN,0)),"^",4)
        . IF DocIEN>0 QUIT  ;"Document for this Tickler already found, so don't search again. SHOULDN'T EVER HAPPEN
        . NEW PtIEN SET PtIEN=+$PIECE($GET(^TMG(22705.5,TklIEN,0)),"^",1)
        . NEW UserIEN SET UserIEN=+$PIECE($GET(^TMG(22705.5,TklIEN,0)),"^",5)
        . NEW DateStr SET DateStr=""
        . NEW DocClIEN SET DocClIEN=0
        . ;"Note: ADCPT xref --> Patient,Doc CLASS,Status,InverseRefDate,DocIEN
        . FOR   SET DocClIEN=$ORDER(^TIU(8925,"ADCPT",PtIEN,DocClIEN)) QUIT:(+DocClIEN'>0)!FOUND  DO
        . . NEW RefDate SET RefDate=""
        . . FOR   SET RefDate=$ORDER(^TIU(8925,"ADCPT",PtIEN,DocClIEN,STATUSIEN,RefDate)) QUIT:(RefDate="")!FOUND  DO
        . . . SET DocIEN=""
        . . . FOR   SET DocIEN=$ORDER(^TIU(8925,"ADCPT",PtIEN,DocClIEN,STATUSIEN,RefDate,DocIEN)) QUIT:(+DocIEN'>0)!FOUND  DO
        . . . . ;"DocIEN should be a COMPLETED document for patient
        . . . . IF $DATA(^TMG(22705.5,"C",DocIEN)) QUIT  ;"document already linked by another tickler
        . . . . IF $$HasTickler(DocIEN,.DateStr,.UserIEN)=0 QUIT
        . . . . SET FOUND=1
        . . . . NEW TMGFDA,TMGMSG
        . . . . SET TMGFDA(22705.5,TklIEN_",",.05)="`"_DocIEN
        . . . . SET TMGFDA(22705.5,TklIEN_",",2)="S"  ;"S=SIGNED
        . . . . SET TMGFDA(22705.5,TklIEN_",",1)=DateStr
        . . . . SET TMGFDA(22705.5,TklIEN_",",3)="`"_UserIEN  ;"Added 8/9/12
        . . . . DO FILE^DIE("E","TMGFDA","TMGMSG")
        . . . . IF $DATA(TMGMSG("DIERR"))=0 QUIT  ;"no errors, so we are done here...
        . . . . DO SendErrAddendum(DocIEN,TklIEN,.TMGMSG)
        . IF FOUND=0 DO  ;"no match COMPLETED document found for TICKLER entry
        . . ;"Check IF patient has any non-COMPLETED documents, IF so, wait longer
        . . SET DocIEN=""
        . . FOR   SET DocIEN=$ORDER(^TIU(8925,"C",PtIEN,DocIEN)) QUIT:(+DocIEN'>0)!FOUND  DO
        . . . SET FOUND=(+$PIECE($GET(^TIU(8925,DocIEN,0)),"^",5)'=STATUSIEN)
        . . IF FOUND=0 DO  ;"TICKLER entry doesn't refer to any real message (must have been deleted in CPRS)
        . . . NEW TMGFDA,TMGMSG
        . . . SET TMGFDA(22705.5,TklIEN_",",2)="O"  ;"O=DISCARDED (ORPHANED)
        . . . DO FILE^DIE("E","TMGFDA","TMGMSG")
        . . . IF $DATA(TMGMSG("DIERR"))=0 QUIT  ;"no errors, so we are done here...
        . . . DO SendErrAddendum(DocIEN,TklIEN,.TMGMSG)
        ;
        ;"Scan all TMG TICKLER entries that have a status of SIGNED,
        ;"and IF the due date has arrived,then process.  Change status to COMPLETED, and
        ;"create an NEW document that is an ADDENDUM to the document.
        ;"Send message 'Your message is now due' etc...
        ;"ADDENDUM: I changed the external text of status (S)/SIGNED to be 'PENDING' for user clarity
        SET TklIEN=0
        NEW TMGDFN
        FOR   SET TklIEN=$ORDER(^TMG(22705.5,"S","S",TklIEN)) QUIT:(+TklIEN'>0)  DO
        . NEW DocIEN SET DocIEN=+$PIECE($GET(^TMG(22705.5,TklIEN,0)),"^",4)
        . NEW AuthorIEN SET AuthorIEN=+$PIECE($GET(^TMG(22705.5,TklIEN,0)),"^",5)  ;"0;5 = USER
        . NEW X,X1,X2,%,%Y,DueDateT,NowDateT
        . SET (X1,DueDateT)=$PIECE(^TMG(22705.5,TklIEN,0),"^",2) ;" 0;2 = DUE DATE, Field 1
        . DO NOW^%DTC SET (X2,NowDateT)=%
        . DO ^%DTC  ;"returns X=X1-X2 (ie X=DUE-NOW);  If %Y=, dates were imprecise and unworkable.
        . IF %Y=0 DO  QUIT
        . . IF DocIEN'>0 SET X=0 QUIT  ;"Bigger problem exists, will be reported below.
        . . SET s(1)="**Error Processing Dates for Tickler Message**"
        . . SET s(2)="(This note may be edited or deleted--until signed.)"
        . . SET s(3)="Date found was imprecise and unworkable, or '#DUE#:' text was not found."
        . . SET s(4)="TO FIX: Please create an addendum to the original note and add a NEW TICKLER message."
        . . DO SendAddendum(DocIEN,AuthorIEN,TklIEN,.s)
        . . ;"If we don't specified the tickler to be Completed, the error will be sent repeatedly
        . . NEW TMGFDA,TMGMSG
        . . SET TMGFDA(22705.5,TklIEN_",",2)="C"  ;"C=COMPLETED
        . . DO FILE^DIE("","TMGFDA","TMGMSG")
        . . IF $DATA(TMGMSG("DIERR"))=0 QUIT  ;"no errors, so we are done here...
        . . DO SendErrAddendum(DocIEN,TklIEN,.TMGMSG)
        . IF X'<1 QUIT  ;"Tickler not yet due, so wait longer.
        . NEW waitMore SET waitMore=0
        . IF X=0 DO  QUIT:waitMore=1
        . . NEW dueTime SET dueTime=$$LJ^XLFSTR($PIECE(DueDateT,".",2),6,"0")
        . . NEW nowTime SET nowTime=$$LJ^XLFSTR($PIECE(NowDateT,".",2),6,"0")
        . . IF dueTime>nowTime SET waitMore=1
        . ;"Success!  Tickler is due.  Send addendum
        . IF DocIEN=0 DO  QUIT
        . . DO SendAlert(AuthorIEN,TklIEN,"Can't find Document for Tickler record. (Shouldn't happen).  Check TMGTICKL.m")
        . NEW STR
        . SET STR(1)=" "
        . SET STR(2)="  * * Tickler message due date has arrived * *  "
        . SET STR(3)="================================================"
        . SET STR(4)=" This note may be edited IF needed until signed"
        . SET STR(5)=" "
        . SET STR(6)="    Please note original tickler message."
        . SET STR(7)=" "
        . ;"ELH added below code to test patient. If inactive, add message    10/25/16
        . SET TMGDFN=+$PIECE($GET(^TMG(22705.5,TklIEN,0)),"^",1)
        . IF $$ACTIVEPT^TMGPXR03(TMGDFN,3)<1 DO
        . . SET STR(8)=" <strong>==== NOTE: INACTIVE PATIENT ==== "
        . . SET STR(9)=" PLEASE REVIEW CHART CAREFULLY BEFORE SCHEDULING"
        . . SET STR(10)=" IF A TRANSFERRED PATIENT, CONSIDER NOTIFYING PATIENT'S NEW PROVIDER"
        . . SET STR(11)=" ================================ </strong>"
        . . SET STR(12)=" "
        . DO SendAddendum(DocIEN,AuthorIEN,TklIEN,.STR)
        . NEW TMGFDA,TMGMSG
        . SET TMGFDA(22705.5,TklIEN_",",2)="C"  ;"C=COMPLETED
        . DO FILE^DIE("","TMGFDA","TMGMSG")
        . IF $DATA(TMGMSG("DIERR"))=0 QUIT  ;"no errors, so we are done here...
        . DO SendErrAddendum(DocIEN,TklIEN,.TMGMSG)
        ;
HandlDone
        ;"SET ZTREQ="@"  ;"delete completed task.
        ;"HANG 11  ;"elh keep lock on for longer than normal check 
        ;"LOCK -^TMG("HANDLE^TMGTICKL","RUNNING")  ;"elh remove lock
        ;"SET ^TMG("HANDLE^TMGTICKL","RUNNING")=0  ;"elh signify not running
        QUIT
        ;
        ;
HasTickler(DocIEN,DateStr,UserIEN)  ;
        ;"Purpose: To determine IF the REPORT TEXT for the TIU DOCUMENT (DocIEN) WP field
        ;"         contains the string that signals a TICKLER message.
        ;"         Notice: The string matched here *same* string as is found in TICKLER()
        ;"Input: DocIEN -- IEN in 8925
        ;"       DateStr -- PASS BY REFERENCE, an OUT PARAMETER
        ;"                  Returns Due Date *String* from '#DUE#: <Place-Due-Date-Here>
        ;"                  on line AFTER [TICKLER MESSAGE]
        ;"                  Returns DateStr("FM")=FMDate_Format of due date
        ;"       UserIEN -- PASS BY REFERENCE, an IN/OUT PARAMETER
        ;"                  Creating user passed in, target user passed out
        ;"Result: 1 IF found, 0 IF not.
        SET DateStr=""
        SET DateStr("FM")=0
        NEW ISHTML SET ISHTML=$$ISHTML^TMGHTM1(DocIEN)
        NEW FOUND,line,DONE SET (FOUND,line,DONE)=0
        FOR   SET line=$ORDER(^TIU(8925,DocIEN,"TEXT",line)) QUIT:(+line'>0)!DONE  DO
        . NEW OneLine SET OneLine=$GET(^TIU(8925,DocIEN,"TEXT",line,0))
        . IF FOUND=0 SET FOUND=(OneLine["[TICKLER") ;"Previous line (missed header IF was broken between 2 lines) -> SET FOUND=(OneLine["[TICKLER MESSGE]")
        . IF FOUND=0 QUIT
        . IF OneLine["#RECIPIENT#:" DO  QUIT   ;"Added block 8/9/12
        . . NEW NewUser SET NewUser=$PIECE(OneLine,"#RECIPIENT#:",2)
        . . IF ISHTML DO
        . . . SET NewUser=$$TrimTags^TMGSTUTL(NewUser)
        . . . SET NewUser=$$REPLSTR^TMGSTUT3(NewUser,"&nbsp;"," ")
        . . SET NewUser=$$TRIM^XLFSTR(NewUser)
        . . IF NewUser="MYSELF" QUIT
        . . IF NewUser="(NONE)" SET DONE=1,FOUND=0 QUIT
        . . NEW X,Y,DIC SET DIC=200,DIC(0)="M"
        . . SET X=NewUser DO ^DIC
        . . IF +Y<1 QUIT
        . . SET UserIEN=+Y
        . IF OneLine["#DUE#:" DO  QUIT
        . . SET DateStr=$PIECE(OneLine,"#DUE#:",2)
        . . IF ISHTML DO
        . . . SET DateStr=$$TrimTags^TMGSTUTL(DateStr)
        . . . SET DateStr=$$REPLSTR^TMGSTUT3(DateStr,"&nbsp;"," ")
        . . SET DateStr=$$TRIM^XLFSTR(DateStr)
        . . ;"NEW ch FOR  SET ch=$EXTRACT(DateStr,1) QUIT:(ch'=" ")  DO  ;"trim off leading spaces
        . . ;". SET DateStr=$EXTRACT(DateStr,2,200)
        . . ;"FOR   QUIT:(DateStr'["@ ")  DO  ;"handle 'mm/dd/yy @ time'  format (i.e. spaces after @)
        . . ;". NEW spec SET spec("@ ")="@"
        . . ;". SET DateStr=$$REPLACE^XLFSTR(DateStr,.spec)
        . . NEW %DT,X,Y
        . . SET X=DateStr,%DT="TF"  ;"assume future dates, and time is allowed.
        . . DO ^%DT  ;"returns Y=-1, or Y=fileman date format.
        . . IF Y>-1 DO
        . . . SET DateStr("FM")=Y
        . . . DO DD^%DT
        . . . SET DateStr=Y  ;"This should be a standardized date.
        SET DateStr=$$TRIM^XLFSTR(DateStr)
        IF DateStr="" DO  ;"If no date found, or invalid date, SET due date to NOW.
        . NEW X,Y SET Y=$$NOW^XLFDT
        . DO DD^%DT
        . SET DateStr=Y
        QUIT FOUND
        ;
        ;
SendAddendum(DocIEN,AuthorIEN,TklIEN,TMGWP) ;
        ;"Purpose: To place an addendum to the specified note (or the note's parent if
        ;"        the note is itself already an addendum.
        ;"Input: DocIEN -- IEN in 8925
        ;"       AuthorIEN -- IEN in 200 of author
        ;"       TklIEN -- Tickler IEN 22705.5
        ;"       TMGWP --PASS BY REFERENCE.  message to put in addendum.
        ;"              e.g. TMGWP(1)="First line of text."
        ;"                   TMGWP(2)="Second line of text."
        ;"Result: None
        ;
        NEW RESULT SET RESULT=1  ;"default to success.
        ;
        NEW parentIEN SET parentIEN=+$PIECE($GET(^TIU(8925,DocIEN,0)),"^",6) ;"0;6= FIELD .06, PARENT
        IF parentIEN>0 SET DocIEN=parentIEN
        NEW PtIEN SET PtIEN=+$PIECE($GET(^TMG(22705.5,TklIEN,0)),"^",1)
        NEW visitIEN SET visitIEN=+$PIECE($GET(^TIU(8925,DocIEN,0)),"^",3)
        NEW locIEN SET locIEN=+$PIECE($GET(^TIU(8925,DocIEN,12)),"^",11)
        NEW HlocIEN SET HlocIEN=+$PIECE($GET(^TIU(8925,DocIEN,12)),"^",5)
        NEW divIEN SET divIEN=+$PIECE($GET(^TIU(8925,DocIEN,12)),"^",12)
        NEW serviceIEN SET serviceIEN=+$PIECE($GET(^TIU(8925,DocIEN,14)),"^",4)
        ;
        NEW DIC,X,Y
        SET DIC=8925.1
        SET DIC("S")="I $P(^(0),U,4)=""DOC"""  ;"screen for Type=Title
        SET X="ADDENDUM"
        DO ^DIC
        IF +Y'>0 DO  GOTO SendADone
        . SET RESULT=0
        . DO SendAlert(AuthorIEN,TklIEN,"Unable to find ADDENDUM Title for Tickler Note")
        NEW docTypeIEN SET docTypeIEN=+Y
        ;
        SET DIC("S")="I $P(^(0),U,4)=""DC"""  ;"screen for Type=Class
        SET X="ADDENDUM"
        DO ^DIC
        IF +Y'>0 DO  GOTO SendADone
        . SET RESULT=0
        . DO SendAlert(AuthorIEN,TklIEN,"Unable to find ADDENDUM class for Tickler Note")
        NEW DocClassIEN SET DocClassIEN=+Y
        ;
        NEW TMGFDA,TMGMSG,TMGIEN
        SET TMGFDA(8925,"+1,",.01)="`"_docTypeIEN ;".01 = DOCUMENT TYPE
        SET TMGFDA(8925,"+1,",.02)="`"_PtIEN      ;".02 = PATIENT
        SET TMGFDA(8925,"+1,",.03)="`"_visitIEN   ;".03 = VISIT
        SET TMGFDA(8925,"+1,",.04)="`"_DocClassIEN ;".04 = PARENT DOCUMENT TYPE
        SET TMGFDA(8925,"+1,",.05)="UNSIGNED"     ;".05 = STATUS
        SET TMGFDA(8925,"+1,",.06)="`"_DocIEN     ;".06 = PARENT
        SET TMGFDA(8925,"+1,",.07)="NOW"          ;".07 = EPISODE BEGIN DATE/TIME
        SET TMGFDA(8925,"+1,",.13)="A"            ;".13 = VISIT TYPE
        SET TMGFDA(8925,"+1,",1201)="NOW"         ;"1201 = ENTRY DATE/TIME
        SET TMGFDA(8925,"+1,",1202)="`"_AuthorIEN ;"1202 = AUTHOR/DICTATOR
        SET TMGFDA(8925,"+1,",1204)="`"_AuthorIEN ;"1204 = EXPECTED SIGNER
        SET TMGFDA(8925,"+1,",1205)="`"_HlocIEN   ;"1205 = HOSPITAL LOCATION
        SET TMGFDA(8925,"+1,",1211)="`"_locIEN    ;"1211 = VISIT LOCATION
        SET TMGFDA(8925,"+1,",1212)="`"_divIEN    ;"1212 = DIVISION
        SET TMGFDA(8925,"+1,",1301)="NOW"         ;"1301 = REFERENCE DATE
        SET TMGFDA(8925,"+1,",1302)="`"_AuthorIEN ;"1302 = ENTERED BY
        SET TMGFDA(8925,"+1,",1303)="direct"      ;"1303 = CAPTURE METHOD
        IF serviceIEN>0 SET TMGFDA(8925,"+1,",1404)="`"_serviceIEN ;"1404 = SERVICE
        SET TMGFDA(8925,"+1,",1506)="NO"          ;"1506 = COSIGNATURE NEEDED
        ;
        DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
        ;
        IF $DATA(TMGMSG("DIERR")) DO  GOTO SendADone
        . SET RESULT=0
        . DO SendAlert(AuthorIEN,TklIEN,"Error creating Tickler addendum.",.TMGMSG)
        ;
        NEW NEWDocIEN SET NEWDocIEN=TMGIEN(1)
        DO SEND^TIUALRT(NEWDocIEN)  ;"create alert regarding note needing to be signed.
        ;
        KILL TMGMSG
        DO WP^DIE(8925,NEWDocIEN_",",2,"","TMGWP","TMGMSG")
        ;
        IF $DATA(TMGMSG("DIERR")) DO  GOTO SendADone
        . SET RESULT=0
        . DO SendAlert(AuthorIEN,TklIEN,"Error filing message into Tickler addendum.",.TMGMSG)
        ;
SendADone ;
        QUIT
        ;
        ;
SendErrAddendum(DocIEN,TklIEN,TMGMSG)  ;
        ;"Purpose: to send an addendum to note showing database error.
        ;"Input: DocIEN: the document that should have the addendum added.
        ;"       TklIEN: the IEN of the tickler record
        ;"       TMGMSG: PASS BY REFERENCE.  The error array, as returned by fileman.
        ;"result: none.
        ;
        NEW ErrStr
        SET ErrStr(1)="Database error encountered handling tickler message."
        SET ErrStr(2)="Note: This may be deleted..."
        SET ErrStr(3)=$$GETERRST^TMGDEBU2(.TMGMSG)
        NEW AuthorIEN SET AuthorIEN=$PIECE($GET(^TMG(22705.5,TklIEN,0)),"^",5)
        DO SendAddendum(DocIEN,AuthorIEN,TklIEN,.ErrStr)
        QUIT
        ;
        ;
SendAlert(UserIEN,TklIEN,Msg,TMGMSG) ;
        ;"Purpose: to send a message alert to the user (for error reporting)
        ;"Input: UserIEN -- IEN in 200, the target of the message
        ;"       TklIEN -- the IEN of the tickler message
        ;"       Msg -- the message to send.  **ONLY UP TO 80 characters**
        ;"              No ^ allowed in the message!
        ;"       TMGMSG -- OPTIONAL, PASS BY REFERENCE.
        ;"              An error array as created by Fileman.
        ;"results: none
        ;
        ;"initialize vars for alert code
        NEW XQA,XQAARCH,XQADATA,XQAFLG,XQAGUID,XQAID,XQAMSG
        NEW XQAOPT,XQAROU,XQASUPV,XQASURO,XQATEXT
        ;
        SET XQADATA=TklIEN_"^"_Msg
        IF $DATA(TMGMSG) SET XQADATA=XQADATA_"^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        SET XQA(UserIEN)=""
        SET XQAMSG=Msg
        SET XQAROU="ERRSHOW^TMGTICKL"
        ;
        DO SETUP^XQALERT  ;"send the alert
        ;
        QUIT
        ;
ERRSHOW ;"Purpose: To show details about error.
        ;"Input: Global-scoped variable XQADATA will hold TklIEN^Msg^FMErrStr
        ;"       Note: TklIEN could be 0
        ;"Results: none
        WRITE !,!
        WRITE "Notice: There was an error processing a tickler message.",!
        WRITE "This notice is to provide as much detail as is possible,",!
        WRITE "so that the tickler message does not get lost.",!,!
        ;
        NEW TklIEN,Msg,FMErrStr
        ;
        IF $DATA(XQADATA)=0 DO  GOTO ErShDone
        . WRITE "But XQADATA doesn't hold info(??).  Aborting.",!
        . DO PRS2CONT
        ;
        SET TklIEN=+$PIECE(XQADATA,"^",1)
        SET Msg=$PIECE(XQADATA,"^",2)
        SET FMErrStr=$PIECE(XQADATA,"^",3)
        ;
        WRITE "The error message was:",!
        WRITE Msg,!
        DO PRS2CONT
        ;
        IF TklIEN>0 DO
        . WRITE !
        . WRITE "PATIENT:",$$GET1^DIQ(22705.5,TklIEN,.01),!
        . WRITE "DOCUMENT:",$$GET1^DIQ(22705.5,TklIEN,.05)," (#",$$GET1^DIQ(22705.5,TklIEN,.05,"I"),")",!
        . WRITE "DUE DATE:",$$GET1^DIQ(22705.5,TklIEN,1),!
        . WRITE "AUTHOR:",$$GET1^DIQ(22705.5,TklIEN,3),!
        . WRITE "AUTHOR:",$$GET1^DIQ(22705.5,TklIEN,3),!
        . WRITE "TICKLER STATUS:",$$GET1^DIQ(22705.5,TklIEN,2),!
        . WRITE "1st LINE OF MESSAGE:",$$GET1^DIQ(22705.5,TklIEN,5),!
        . DO PRS2CONT
        ;
        IF FMErrStr'="" DO
        . WRITE !,"The Fileman (database) error message was:",!
        . WRITE FMErrStr,!
        . DO PRS2CONT
        ;
        WRITE !,!
        WRITE "Hopefully this will be enough information for you",!
        WRITE "to fix the tickler message.",!
        WRITE "Please follow up on this NOW....",!
        WRITE "This will be the *only* reminder!",!!
        DO PRS2CONT
        ;
ErShDone ;
        QUIT
        ;
        ;
RescheduleTask  ;
        ;"Purpose: to SET up task to periodically handle tickler messages.
        ;"Result: None
        NEW temp SET temp=0  ;"elh, don't allow to set task
        IF temp=0 QUIT  ;"a debugging measure so that launching a duplicate task can be avoided
        ;
        NEW ZTRTN,ZTDESC,ZTDTH,ZTIO,ZTUCI,ZTCPU
        NEW ZTPRI,ZTSAVE,ZTKIL,ZTSYNC,ZTSK
        ;
        SET ZTRTN="HANDLE^TMGTICKL"
        SET ZTDESC="TMG TICKLER MESSAGES HANDLER"
        SET ZTIO=""
        ;
        NEW hrInterval SET hrInterval=+$PIECE($GET(^TMG(22705.4,1,0)),"^",2) ;"0;2=Interval
        IF hrInterval<1 DO  GOTO SchTDone
        . DO SendAlert(DUZ,0,"Tickler Error: Interval (field #1) in file 22705.4 < 1 hr")
        . SET ZTSK=0
        ;
        NEW X,Y,%,%DT
        SET %DT="XR" SET X="NOW+"_hrInterval_"H" DO ^%DT
        SET ZTDTH=Y  ;"schedule time.
        ;
        DO ^%ZTLOAD
SchTDone ;
        SET $PIECE(^TMG(22705.4,1,0),"^",3)=ZTSK  ;"there are no XRefs on this field, and I own it...
        QUIT
        ;
        ;
AUTOCR  ;"AUTO CHECK RUN
        ;"Purpose: A entry point for a Taskman-scheduled task that verifies
        ;"         that the tickerler task has not crashed.  If not running,
        ;"         then automatically rescheduled to run.
        GOTO ACRDN    ;"elh, don't allow to run
        NEW STATUS SET STATUS=$$TaskStatus(0) 
        IF +STATUS=1 GOTO ACRDN
        NEW PROB SET PROB=$PIECE(STATUS,"^",2)
        DO RescheduleTask
        SET STATUS=$$TaskStatus(0) ;"Check IF restart worked.
        NEW MSG 
        IF +STATUS=1 DO
        . SET MSG="FYI: Tickler task restarted after problem"
        . IF PROB'="" SET MSG=MSG_": "_PROB
        . ELSE  SET MSG=MSG_"."
        ELSE  DO
        . SET MSG="WARNING: Tickler task NOT RUNNING."
        . IF PROB'="" SET MSG=MSG_"  Init. problem: "_PROB
        . SET PROB=$PIECE(STATUS,"^",2)
        . IF PROB'="" SET MSG=MSG_"  Restart problem: "_PROB
        DO SendAlert(168,0,MSG) ;//for now HARDCODED to send to K.TOPPENBERG
ACRDN   QUIT
        ;
        ;
CHECKRUN  ;
        ;"Purpose: To check that the background processor for the Tickler is running.
        ;"         If not running, give user a chance to start it.
        ;"Input: None
        ;"Results: None.
        ;
        DO KillOldTasks
        NEW STATUS
CR1     SET STATUS=$$TaskStatus(0)
        IF +STATUS=1 DO  GOTO CRDN
        . WRITE !,"SUCCESS!  The TICKLER MESSAGES task is running.",!
        . WRITE "Details:",!
        . WRITE "  Task#: ",$PIECE(STATUS,"^",3),!
        . WRITE "  Scheduled to run next: ",$$HTE^XLFDT($PIECE(STATUS,"^",4)),!
        . DO PRS2CONT
        WRITE "There is a problem.  Task is NOT running.",!
        NEW prob SET prob=$PIECE(STATUS,"^",2)
        IF prob'="" WRITE "Problem: ",prob,!
        NEW % SET %=1
        WRITE "Try to launch task now" DO YN^DICN WRITE !
        IF %=1 DO  GOTO CR1
        . DO RescheduleTask
        ;
CRDN    QUIT
        ;
TaskStatus(Verbose)  ;
        ;"Purpose: To determine the status of the Tickler background task.
        ;"Input: Verbose : OPTIONAL.  If 1 then output shown. 0 (default) is quiet.
        ;"Output: 1^Active^TaskNumber^NextRun($H),  or -1^Message
        NEW ZTRTN,ZTDESC,ZTDTH,ZTIO,ZTUCI,ZTCPU
        NEW ZTPRI,ZTSAVE,ZTKIL,ZTSYNC,ZTSK
        ;
        SET ZTDESC="TMG TICKLER MESSAGES HANDLER"
        SET ZTIO=""
        ;
        SET Verbose=+$GET(Verbose)
        NEW RESULT SET RESULT="-1^No Task Found" ;"default to error
        NEW NextRun SET NextRun=""
        ;
        IF $$TM^%ZTLOAD=0 DO  GOTO TSDone
        . SET RESULT="-1^Taskman not running on current volume set"
        ;
        NEW TMGLIST,TSK
        DO DESC^%ZTLOAD(ZTDESC,"TMGLIST")
        NEW DONE SET DONE=0
        SET TSK=0
        FOR   SET TSK=$ORDER(TMGLIST(TSK)) QUIT:(TSK="")!DONE  DO
        . NEW ZTSK SET ZTSK=TSK
        . DO ISQED^%ZTLOAD
        . IF Verbose WRITE "Task ",ZTSK,": "
        . SET ZTSK(0)=$GET(ZTSK(0))
        . IF ZTSK(0)=1 IF Verbose WRITE "Pending/Waiting",!
        . ELSE  IF ZTSK(0)=0 DO
        . . IF Verbose WRITE "Done",!  ;"Not Pending/Waiting",!
        . ELSE  IF ZTSK(0)="" DO
        . . IF Verbose WRITE "Lookup error.",!
        . IF $DATA(ZTSK("E")) DO
        . . IF 'Verbose QUIT
        . . IF $GET(ZTSK("E"))="IT" WRITE "  The task number was not valid (0, negative, or non numeric).",! QUIT
        . . IF $GET(ZTSK("E"))="I" WRITE "  The task does not exist on the specified volume set.",! QUIT
        . . IF $GET(ZTSK("E"))="IS" WRITE "  The task SET is not listed in the VOLUME SET file (#14.5).",! QUIT
        . . IF $GET(ZTSK("E"))="LS" WRITE "  The link to that volume SET is not available.",! QUIT
        . . IF $GET(ZTSK("E"))="U" WRITE "  An unexpected error arose (e.g., disk full, protection, etc.).",!
        . IF $DATA(ZTSK("D")) DO
        . . SET NextRun=$GET(ZTSK("D"))
        . . IF 'Verbose QUIT
        . . WRITE "  Task scheduled to start: ",$$HTE^XLFDT($GET(ZTSK("D"))),!
        . KILL ZTSK SET ZTSK=TSK
        . DO STAT^%ZTLOAD
        . IF ZTSK(0)=0 DO  QUIT
        . . IF 'Verbose QUIT
        . . WRITE "?? task undefined??"
        . SET ZTSK(1)=$GET(ZTSK(1))
        . IF Verbose WRITE "  Status: ",ZTSK(1),"  ",ZTSK(2),!
        . IF (ZTSK(1)=1)&(ZTSK(2)="Active: Pending") DO  QUIT
        . . SET DONE=1
        . . SET RESULT="1^Active^"_TSK_"^"_NextRun
        ;
TSDone  QUIT RESULT
        ;
        ;
KillOldTasks  ;
        ;"Purpose: To clear out old, completed tasks
        ;"Input: none
        ;"Output:
        NEW ZTRTN,ZTDESC,ZTDTH,ZTIO,ZTUCI,ZTCPU
        NEW ZTPRI,ZTSAVE,ZTKIL,ZTSYNC,ZTSK
        NEW TMGLIST,TSK
        SET ZTDESC="TMG TICKLER MESSAGES HANDLER"
        DO DESC^%ZTLOAD(ZTDESC,"TMGLIST")
        SET TSK=0
        FOR   SET TSK=$ORDER(TMGLIST(TSK)) QUIT:(TSK="")  DO
        . NEW ZTSK SET ZTSK=TSK
        . DO ISQED^%ZTLOAD
        . SET ZTSK(0)=$GET(ZTSK(0))
        . IF $DATA(ZTSK("E")) DO  QUIT
        . . WRITE "Task ",ZTSK,": ",$GET(ZTSK("E")),!
        . IF ZTSK(0)="" WRITE "Lookup error for task: ",TSK,! QUIT
        . IF ZTSK(0)'=0 QUIT
        . KILL ZTSK SET ZTSK=TSK
        . DO STAT^%ZTLOAD
        . IF ZTSK(0)=0 WRITE "Task ",ZTSK,": ?? task undefined??" QUIT
        . IF ($GET(ZTSK(1))=3)&($GET(ZTSK(2))="Inactive: Finished") DO
        . . DO KILL^%ZTLOAD
        QUIT
        ;
  ;"===========================================================================
  ;"Below are copies of functions from TMG Libarary, put here to avoid dependancies
  ;"===========================================================================
        ;
PRS2CONT ;
        ;"Purpose: to provide a 'press key to continue' action
        ;
        WRITE "----- Press Key To Continue -----"
        NEW CH READ CH:$GET(DTIME,3600)
        WRITE !
        QUIT
        ;
