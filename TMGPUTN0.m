TMGPUTN0 ;TMG/kst/TIU Document Upload look-up function ;03/25/06; 5/2/10, 2/2/14
         ;;1.0;TMG-LIB;**1**;04/25/04

 ;"TIU Document Upload look-up function

 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;

LOOKUP(DocTitle,Autosign) ;
        ;"-----------------------------------------------------------------------------------
        ;"Upload look-up function
        ;"by Kevin Toppenberg
        ;"4-25-2004
        ;"
        ;"PURPOSE:
        ;"This code is used as look-up code by the TIU document upload routines.
        ;"It has a very specific purpose.  It was written for uploading documents
        ;" from a Medic EMR system.  Notes had been dumped out of that system, and
        ;" were to be ported into VistA
        ;"Each note has a header with patient name, dob, ssnum, chart#, provider
        ;"Addendum -- this code will also work with less extensive patient data.
        ;"
        ;"INPUT
        ;"  The variable (with global scope) listed below are expected as input.
        ;"                  Not all will be required every time, however.
        ;"  DocTitle -- this is the type of document type.  i.e. 'OFFICE VISIT'
        ;"                This will be used so that this code can service multiple
        ;"                         types, i.e. NOTE, PRESCRIPTION CALL IN, etc.
        ;"  Autosign -- [OPTIONAL] IF value=1 then document will be created as SIGNED
        ;"Results: Document number that uploaded code should be put into is returned in variable Y
        ;"
        ;"
        ;"*How it works*:
        ;"A remote computer connects to the server running VistA.  This remote computer must be
        ;"  able to upload a file using kermit.  The only way I know to DO this is to be on a PC
        ;"  using a terminal emulator program that has kermit upload ability.
        ;"From this remote session, get into the TIU menu system and navigate to the option to
        ;"  upload a document.  Note, one's upload parameters must be SET up for this to work.
        ;"The remote user will see a #N3, and use this que to acutally upload the file.
        ;"After the file is uploaded, it is then processed.  Each document specifies what 'type' it is
        ;"   for example 'OFFICE VISIT'
        ;"The server then loads up the parameters for OFFICE VISIT and processes each item in the header.
        ;"Here is an example progress note that this file can process
        ;"--------------------------------------
        ;"[NewDict]:        OFFICE VISIT
        ;"Name:        JONES,BASKETBALL
        ;"Alias:        JONES,BOB
        ;"DOB:                4/13/71
        ;"Sex:                MALE
        ;"SSNumber:        555 11 9999
        ;"ChartNumber:        10034
        ;"Date:        7/22/2002
        ;"Location:        Peds_Office
        ;"Provider:        KEVIN TOPPENBERG MD
        ;"[TEXT]
        ;"
        ;"        CHIEF COMPLAINT:  Follow up blood clot.
        ;"
        ;"        HPI:
        ;"        1.  BJ was in the eMERGEncy room 3 days ago.  He was being
        ;"            evaluated for left lower extremity pain.  He said that they did
        ;"            radiographic studies and told him that he had a blood clot in
        ;"        .... (snip)
        ;"
        ;"[END]
        ;"--------------------------------------
        ;"[NewDic] tells the system that a document header is starting
        ;"'Name' is a CAPTION, and the value for this caption is 'JONES,BASKETBALL'
        ;"The upload system will put this value into a variable.  In this case, I specified
        ;"  that the variable name TMGNAME to be used.
        ;"
        ;"Here are each caption and its cooresponding Variable:
        ;"Name <--> TMGNAME
        ;"DOB <--> TMGDOB
        ;"Sex <--> TMGSEX
        ;"SSNumber <--> TMGSSNUM
        ;"ChartNumber <--> TMGPTNUM
        ;"Date <--> TIUVDT
        ;"Provider <--> PERSON
        ;"Alias <--> TMGALIAS
        ;"Location: <--> TIULOC
        ;"
        ;"Document Title is passed to function as 'DocTitle'
        ;"
        ;"After the note has been processed and all the above variables have been set, the server
        ;"calls a 'look-up' function.  This function is supposed to return the document number where the
        ;"text is supposed to be put (the number should be put in Y)
        ;"
        ;"This look-up function has an extra twist.  I am using it to register patients on the fly
        ;"  IF needed.  I am doing this because I had about 30,000 patients in my database to transfer,
        ;"  and I had difficulty getting a separate file with just demographics etc.  So, IF a patient
        ;"  is not already in the database, they are registered here.
        ;"
        ;"Extra note:
        ;"When this function is called, the TIU upload process has already SET up some variables.
        ;"DA = the IEN in 8925.2, i.e. ^TIU(8925.2,DA,"TEXT",0) that the uploaded text was temporarily store in.
        ;"     In other words, here DA = the serial index number of the document to be uploaded
        ;"     i.e. 1 for the first, 2 for the second etc.
        ;"TIUI = the line index of the beginning of the report to be processed (i.e. the line
        ;"       that starts with [TEXT]
        ;"DUZ = Current user number.
        ;"TIUHSIG = [NewDict]  .. or whatever it has been SET to by user in upload params
        ;"TIUBGN = [TEXT] ... or whatever is has be SET to by user in upload params.

        WRITE "+-------------------------------------+",!
        WRITE "| Starting upload code...             |",!
        WRITE "+-------------------------------------+",!

        SET BuffNum=$GET(DA)    ;"Store which upload buffer we are working on.
        SET BuffIdx=$GET(TIUI)  ;"Store line number (in upload buffer) we are starting with.
        NEW cMaxNoteWidth SET cMaxNoteWidth=60

        ;"Field (f) constants
        NEW fPatient SET fPatient=.02        ;"field .02 = PATIENT
        NEW fVisit SET fVisit=.03            ;"field .03 = VISIT
        NEW fParentDoc SET fParentDoc=.04    ;"field .04 = PARENT DOCUMENT TYPE
        NEW fStatus SET fStatus=.05          ;"field .05 = STATUS
        NEW fParent SET fParent=.06          ;"field .06 = PARENT
        NEW fStartDate SET fStartDate=.07    ;"EPISODE BEGIN DATE/TIME (field .07)
        NEW fEndDate SET fEndDate=.08        ;"EPISODE END DATE/TIME (field .08)
        NEW fEntryDate SET fEntryDate=1201   ;"field 1201 = ENTRY DATE/TIME
        NEW fAuthor SET fAuthor=1202         ;"field 1202 = PERSON/DICTATOR
        NEW fExpSigner SET fExpSigner=1204   ;"field 1204 = expected Signer
        NEW fHospLoc SET fHospLoc=1205       ;"field 1205 = HOSPITAL LOCATION
        NEW fExpCosign SET fExpCosign=1208   ;"field 1208 = expected cosigner
        NEW fAttending SET fAttending=1209   ;"field 1209 = ATTENDING
        NEW fVisitLoc SET fVisitLoc=1211     ;"field 1211 = VISIT LOCATION
        NEW fRefDate SET fRefDate=1301       ;"field 1301 = REFERENCE DATE
        NEW fEnteredBy SET fEnteredBy=1302   ;"field 1302 = ENTERED BY (a pointer to file 200)
        NEW fCapMethod SET fCapMethod=1303   ;"field 1303 = CAPTURE METHOD;  U-->'upload'
        NEW fService SET fService=1404       ;"field 1404 = SERVICE
        NEW fSignedBy SET fSignedBy=1502     ;"field 1502 = signed by
        NEW fNeedCosign SET fNeedCosign=1506 ;"field 1506 = cosigniture expected.
        NEW fCharTrans SET fCharTrans=22711  ;"field 22711 = CHAR COUNT -- TRANSCRIPTIONIST
        NEW fLineCount SET fLineCout=.1      ;"field .1 = LINE COUNT

        ;" Piece (p) constants
        NEW pPatient SET pPatient=2      ;"Node 0,piece 2 = PATIENT (field .02)
        NEW pVisit SET pVisit=3          ;"Node 0,piece 3 = VISIT (field .03)
        NEW pStrtDate SET pStrtDate=7    ;"Node 0,piece 7 = EPISODE BEGIN DATE/TIME (field .07)
        NEW pEndDate SET pEndDate=8      ;"Node 0,piece 8 = EPISODE END DATE/TIME (field .08)
        NEW pExpSigner SET pExpSigner=4  ;"Node 12,piece 4 = EXPECTED SIGNER (field 1204)
        NEW pHospLoc SET pHospLoc=5      ;"Node 12,piece 5 = HOSPITAL LOCATION (field 1205)
        NEW pExpCosign SET pExpCosign=8  ;"Node 12,piece 8 = EXPECTED COSIGNER (field 1210)
        NEW pAttending SET pAttending=9  ;"Node 12,piece 9 = ATTENDING PHYSICIAN (field 1209)
        NEW pService SET pService=4      ;"Node 14,piece 4 = SERVICE (field 1404)

        IF $DATA(cAbort)#10=0 NEW cAbort SET cAbort=0

        NEW TMGDBINDENT,PriorErrorFound
        NEW Patient
        NEW DocIEN SET DocIEN=-1
        NEW Document
        NEW NewDoc SET NewDoc=0
        NEW result SET result=1  ;"cOKToCont

        DO PtArrayCreate(.Patient) ;"Load upload info into Patient array
        SET result=$$DocArrayCreate(.Document) ;"Load upload document info into Document array
        IF result=cAbort GOTO LUDone
        SET Document("DFN")=$$GETDFN^TMGGDFN(.Patient)  ;"Store DFN of patient.
        IF Document("DFN")'>0 SET result=cAbort GOTO LUDone   ;"Abort.
        SET Document("AUTO SIGN")=$GET(Autosign,1)  ;"default to YES auto-signing
        ;"06-19-05 Changed to disable autosigning.  If document is
        ;"      autosigned here, then no prompt for printing elsewhere.
        ;"9-1-05 Resuming autosigning.  Currently the outside transcriptionists are already
        ;"      printing the notes before giving them to us for upload.
        ;"      Changed default to be YES autosign
        ;"set Document("AUTO SIGN")=0 ;"override setting passed in...

        SET Document("CHARACTER COUNT - TRANSCRIPTIONIST'S")=$$BuffCharCount()   ;"Count character prior to any wrapping/merging etc.
        SET result=$$PrepUploadBuf()  ;"Do any word-wrapping etc needed in upload buffer
        IF result=cAbort GOTO LUDone
        SET DocIEN=$$PrepDoc(.Document,.NewDoc)      ;"Prepair a document to put upload into. Credits transcription

        SET Y=DocIEN
        MERGE TMGDOC=Document  ;"Create a global -- will KILL after followup code
LUDone
        ;"put result into Y.  TIU filing system looks for results in Yi
        IF result=cAbort SET Y=-1

        QUIT



 ;"-----------------------------------------------------------------------------------------------
 ;"==============================================================================================-
 ;" S U B R O U T I N E S
 ;"==============================================================================================-
 ;"-----------------------------------------------------------------------------------------------
 ;"PtArrayCreate(Array)
 ;"DocArrayCreate(Document)
 ;"PrepDoc(Document,NewDoc)
 ;"GetDocTIEN(Title)
 ;"GetLocIEN(Location)
 ;"GetService(IEN)
 ;"GetProvIEN(Provider)
 ;"GetRecord(Document,NewDoc,AskOK,Editable)
 ;"DocExists(Document)
 ;"BuffCharCount()
 ;"PrepUploadBuf()

 ;"NeedsReformat(MaxWidth)
 ;"CutNote(Array)
 ;"PasteNote(Array,NextNoteI)
 ;"CompToBuff(ExistingIEN,UplTIEN,UplDate)
 ;"CreateRec(Document) ;
 ;"StuffRec(Document,PARENT)
 ;"MakeVisit(Document)
 ;"FOLLOWUP(DocIEN) ;Post-filing code for PROGRESS NOTES


PtArrayCreate(Array)
        ;"SCOPE: Private
        ;"Purpose: To put global scope vars (i.e. TMGNAME,TMGSSNUM etc) into
        ;"        an array for easier portability
        ;"Input: Array, must be passed by reference
        ;"       The global-scope variables setup by the upload system, and are used here:
        ;"                TMGPTNUM,TMGSSNUM,TMGSSNUM,TMGNAME,TMGDOB,TMGSEX
        ;"Output: Array is loaded with info, like this:
        ;"        SET Array("SSNUM")="123-45-6789"
        ;"        SET Array("NAME")="DOE,JOHN"
        ;"        SET Array("DOB")=TMGDOB
        ;"        SET Array("PATIENTNUM")="12345677"
        ;"        SET Array("SEX")="M"
        ;"        SET Array("ALIAS")="DOE,JOHNNY"
        ;"Results: none

        IF $DATA(TMGPTNUM)#10'=0 do
        . SET TMGPTNUM=$TRANSLATE(TMGPTNUM,"PWCI*","")  ;"Clean off alpha characters -- not needed.
        . ;"set TMGPTNUM=$$Trim^TMGSTUTL(TMGPTNUM)
        . SET TMGPTNUM=$$FORMAT^TMGDPTN1(.TMGPTNUM,3,30)  ;"Use same input transform as for .01 field of PATIENT file
        . SET Array("PATIENTNUM")=TMGPTNUM

        IF $DATA(TMGSSNUM)#10'=0 do
        . SET TMGSSNUM=$TRANSLATE(TMGSSNUM," /-","")  ;"Clean delimiters
        . IF +TMGSSNUM=0 SET TMGSSNUM=""  ;was ... "P"
        . IF (TMGSSNUM="P")!(+TMGSSNUM>0) SET Array("SSNUM")=TMGSSNUM

        SET Array("NAME")=$$FormatName^TMGMISC(.TMGNAME)

        IF $DATA(TMGALIAS)#10'=0 do
        . SET TMGALIAS=$TRANSLATE(TMGALIAS,"*","")
        . SET TMGALIAS=$$FORMAT^TMGDPTN1(TMGALIAS,3,30) ;"convert to 'internal' format (strip .'s etc)
        . SET Array("ALIAS")=TMGALIAS

        IF $DATA(TMGSEX)#10'=0 do
        . SET TMGSEX=$$UP^XLFSTR($GET(TMGSEX))
        . IF TMGSEX="M" SET TMGSEX="MALE"
        . ELSE  IF TMGSEX="F" SET TMGSEX="FEMALE"
        . SET Array("SEX")=TMGSEX

        IF $DATA(TMGDOB)#10'=0 do
        . IF +TMGDOB>0 SET Array("DOB")=TMGDOB
        . ELSE  QUIT
        . NEW CurDate,CurYr
        . DO DT^DILF("E","T",.CurDate)
        . SET CurDate=$GET(CurDate(0))
        . IF CurDate="" QUIT
        . SET CurYr=$PIECE(CurDate,", ",2)
        . NEW DOBYr
        . SET DOBYr=$PIECE(TMGDOB,"/",3)
        . IF DOBYr>CurYr DO  ;"we have a Y2K problem
        . . SET DOBYr=DOBYr-100
        . . IF DOBYr'>0 QUIT
        . . SET TMGDOB=$PIECE(TMGDOB,"/",1,2)_"/"_DOBYr
        . . SET Array("DOB")=TMGDOB

        QUIT



DocArrayCreate(Document)
        ;"SCOPE: Private
        ;"Purpose: To put TIUVDT etc. etc into an array for easier portibility
        ;"Input: Document -- OUT parameter, must be passed by reference
        ;"       The global-scope variables setup by the upload system are used:
        ;"                TIUVDT,PERSON,TIULOC, (and also DocTitle)
        ;"Output: Document is loaded with info.
        ;"Results: 1=OKToCont, or cAbort

        NEW result SET result=1 ;"cOKToCont

        SET Document("PROVIDER")=$GET(PERSON)
        IF Document("PROVIDER")="" DO  GOTO DACDone
        . SET result=cAbort
        SET Document("PROVIDER IEN")=$$GetProvIEN(Document("PROVIDER"))
        SET Document("LOCATION")=$GET(TIULOC,"Main_Office")
        SET Document("DATE")=$GET(TIUVDT)
        SET Document("TITLE")=$GET(DocTitle,"NOTE")

        ;"Decide which transcriptionist is. This will be used for crediting productivity.
        ;"If transcriptionist not specified, current user (DUZ) is assumed.
        IF $DATA(TMGTRANS)#10=0 SET TMGTRANS=$PIECE($GET(^VA(200,DUZ,0)),"^",1)
        SET Document("TRANSCRIPTIONIST")=$$FormatName^TMGMISC(TMGTRANS)

        IF (Document("DATE")="")!(Document("DATE")="00/00/00") DO  GOTO DACDone
        . SET result=cAbort

DACDone
        QUIT result



PrepDoc(Document,NewDoc) ;
        ;"Scope: PRIVATE.
        ;"       Addendum 7/25/07.  Will be called by RPC call BLANKTIU^TMGRPC1
        ;"                          to return a blank document
        ;"Purpose: Prepair a document to put upload into.
        ;"Input: Document -- an array as follows:
        ;"                Document("DFN")=DFN, the record number of the patient.
        ;"                Document("PROVIDER IEN")= the IEN of the provider
        ;"                Document("LOCATION")= the location of the visit
        ;"                Document("DATE")= the date of the visit.
        ;"                Document("TITLE")= the title of the note
        ;"                Document(cVisitStr)  an OUT PARAMETER
        ;"                Document("TRANSCRIPTIONIST") -- the name of the transcriptionist
        ;"                Document("CHARACTER COUNT - TRANSCRIPTIONIST'S") -- the char count creditable to transcriptionist
        ;"    NewDoc:  OPTIONAL flag, passed back with
        ;"              NewDoc = 1 IF returned docmt is new
        ;"              NewDoc = 0 IF returned docmt already existed, timeout, etc
        ;"Results: returns record number (IEN) ready to accept upload (or -1 IF failure)
        ;"        Also Document("DOC IEN") will have this same IEN
        ;"        NOTE: IF result is -1 then errors are passed back in
        ;"              Document("ERROR") node
        ;"              Document("ERROR",n)="ERROR.. Stuffing NEW document."
        ;"              Document("ERROR","NUM")=n
        ;"              Document("ERROR","FM INFO")=MERGE with DIERR array

        ;"  PIEN = patient internal entry number
        ;"  Global-Scope variables expected:
        ;"    PERSON, TMGSSNUM etc. defined above
        ;"    TIUVDT expected
        ;"    TIULOC is also expected (i.e. 'LAUGHLIN_OFFICE')
        ;"
        ;"Output: will return document number, or -1 IF failure.
        ;"NOTES:  This originated from         ^TIUPUTPN
        ;"
        ;" Look-up code used by router/filer
        ;" Required          variables: TMGSSNUM, TIUVDT
        ;"   i.e., TMGSSNUM (Pt SS-Number) and TIUVDT (visit date) must be SET prior to call.
        ;"

        NEW cStartDate SET cStartDate="EDT"
        NEW cEndDate SET cEndDate="LDT"
        NEW cService SET cService="SVC"
        NEW cDocType SET cDocType="TYPE"
        NEW cDocTIEN SET cDocTIEN="TYPE IEN"
        NEW cHspLocIEN SET cHspLocIEN="LOC"
        NEW cVstLocIEN SET cVstLocIEN="VLOC"
        NEW cVisitStr SET cVisitStr="VSTR"
        NEW cVisitIEN SET cVisitIEN="VISIT"
        NEW cStopCode SET cStopCode="STOP"

        NEW TMG,DFN
        NEW TIUDAD,TIUEDIT
        NEW TIULDT,TIUXCRP,DocTIEN
        NEW LocIEN
        NEW result SET result=-1
        SET NewDoc=0

        SET Document(cStartDate)=$$IDATE^TIULC(Document("DATE")) ;"Convert date into internal format
        SET Document(cEndDate)=Document(cStartDate) ;"For office notes, begin and end dates will be the same.

        ;"Setup DocTIEN -- to be used below as [MAS Movement event type]
        ;"Convert Document title into IEN, i.e. OFFICE VISIT --> 128
        SET DocTIEN=$$GetDocTIEN(Document("TITLE"))
        IF +DocTIEN'>0 DO  GOTO PrepDocX
        . SET Document("ERROR",1)="ERROR: Unable to determine note type from title: "_Document("TITLE")
        . SET Document("ERROR","NUM")=1

        ;"Purpose: setup Document(cDocType)  -- used below as: Title info variable of form:
        ;" Setup string in form of:  1^title IEN^title Name
        ;" e.g.:  1^128^OFFICE VISIT^OFFICE VISIT
        SET Document(cDocTIEN)=DocTIEN
        SET Document(cDocType)=1_"^"_DocTIEN_"^"_$$PNAME^TIULC1(DocTIEN)

        ;"do MAIN^TIUVSIT(.TIU,.DFN,TMGSSNUM,Document(cStartDate),Document(cEndDate),"LAST",0,Document("LOCATION"))

        ;" setup LocIEN from HOSPITAL LOCATION file (#44)
        ;" This contains entries like 'Laughlin_Office'
        SET LocIEN=+$$GetLocIEN(Document("LOCATION"))
        IF '$DATA(^SC(LocIEN,0)) DO  GOTO PrepDocX     ;"^SC(*) is file 44, Hospital Location
        . SET Document("ERROR",1)="ERROR: Unable to process location: "_Document("LOCATION")
        . SET Document("ERROR","NUM")=1

        SET Document(cService)=$$GetService(Document("PROVIDER IEN"))        ;"i.e. FAMILY PRACTICE
        SET Document(cVisitStr)="x;x;"_DocTIEN                        ;"LOC;VDT;VTYP
        SET Document(cVisitIEN)=0                                ;"Visit File IFN
        SET Document(cHspLocIEN)=LocIEN
        SET Document(cVstLocIEN)=LocIEN
        SET Document(cStopCode)=0  ;"0=FALSE, don't worry about stop codes.

        SET result=$$GetRecord(.Document,.NewDoc,0)
        IF result'>0 DO  GOTO PrepDocX
        . NEW n SET n=+$GET(Document("ERROR","NUM"))+1
        . SET Document("ERROR",n)="ERROR.. after creating NEW document."
        . SET Document("ERROR","NUM")=n

        ;"At this point, any merging has been done (once implemented)
        ;"So a character count of now will be a total/combined character count
        SET Document("CHAR COUNT - TOTAL")=$$BuffCharCount   ;"Count character after any wrapping/merging etc.
        ;"Now, we need the standard CHARARACTERS/LINE value stored in field .03 of TIU PARAMETERS (in ^TIU(8925.99))
        ;"For my setup, I have only have one record for in this file, so I'll use IEN=1.
        NEW CharsPerLine SET CharsPerLine=$PIECE($GET(^TIU(8925.99,1,0)),"^",3)
        IF CharsPerLine'=0 do
        . NEW IntLC,LC,Delta
        . SET LC=Document("CHAR COUNT - TOTAL")\CharsPerLine
        . SET IntLC=Document("CHAR COUNT - TOTAL")\CharsPerLine  ;" \ is integer divide
        . SET Delta=(LC-IntLC)*10
        . IF Delta>4 SET IntLC=IntLC+1  ;"Round to closest integer value.
        . SET Document("LINE COUNT")=IntLC

        SET result=$$StuffRec(.Document,0)   ;"Will load Document("ERROR","FM INFO") with any FM errors
        IF +$GET(result)'>0 DO  GOTO PrepDocX
        . NEW n SET n=+$GET(Document("ERROR","NUM"))+1
        . SET Document("ERROR",n)="ERROR.. Stuffing NEW document."
        . SET Document("ERROR","NUM")=n

PrepDocX
        QUIT result  ;"result is document #


MakeVisit(Document)
        ;"Purpose -- to create a NEW entery in the VISIT file, based on info in Document.
        ;"Input -- Document -- array with following info:
        ;"                Document("DFN")=DFN, the record number of the patient.
        ;"                Document("PROVIDER")= the provider of care for the note
        ;"                Document("PROVIDER IEN")= the IEN of the provider
        ;"                Document("LOCATION")= the location of the visit
        ;"                Document("DATE")= the date of the visit.
        ;"Result -- returns IEN of visit entry

        ;"Note -- this function is not now being used...

        NEW TMGFDA
        ;set TMGFDA(9000010,"?+1,",.01)=        ;".01=VISIT/ADMIT DATE&TIME
        ;set TMGFDA(9000010,"?+1,",.02)=        ;".02=DATE VISIT CREATED
        ;set TMGFDA(9000010,"?+1,",.03)="O"     ;".02=VISIT TYPE  -- O=Other
        ;set TMGFDA(9000010,"?+1,",.05)=        ;".05=PATIENT NAME
        ;set TMGFDA(9000010,"?+1,",15001)="10C1-TEST"  ;"15001=VISIT ID
        ;LOCATION NAME --> Medical Group of Greeneville
        ;SERVICE CATEGORY: A --> AMBULATORY
        ;DSS ID: PRIMARY CARE/MEDICINE
        ;HOSPITAL LOCATION: Laughlin_Office
        ;Created by user: DUZ
        QUIT


GetDocTIEN(Title)
        ;"Purpose: To return IEN for document *type defination* / Identify document title
        ;"Input  Title -- the Text Title to look up
        ;"Results: Returns the document definition IFN (i.e. Y)

        NEW DIC,Y,X
        NEW TIUFPRIV SET TIUFPRIV=1

        SET DIC=8925.1
        SET DIC(0)="M"
        SET DIC("S")="IF $PIECE(^TIU(8925.1,+Y,0),""^"",4)=""DOC"""
        SET X=Title
        DO ^DIC
        KILL DIC("S")
        IF $find(Y,"^")>0 SET Y=$PIECE(Y,"^",1)

        QUIT Y


GetLocIEN(Location)
        ;"Scope: PRIVATE
        ;"Purpose: To return IEN for location
        ;"Input: Location -- the Location to look up.
        ;"Results: returns LocationIEN (i.e. Y)

        NEW DIC,X,Y
        SET DIC=44 ;"file 44 is HOSPITAL LOCATION
        SET DIC(0)="M"
        SET X=Location
        DO ^DIC ;" DO a         , value is returned in Y
        IF $find(Y,"^")>0 SET Y=$PIECE(Y,"^",1)

        QUIT Y


GetService(IEN)
        ;"Scope: PRIVATE
        ;"Purpose: Get the Service for the Provider
        ;"Input: IEN -- the IEN of the Provider to look up.
        ;"Results: returns the Name of the Service for provider, or "" IF not found

        NEW result SET result=""
        NEW node,SvIEN

        IF IEN=-1 GOTO GtSvDone
        SET node=$GET(^VA(200,IEN,5))  ;"^VA(200, is NEW PERSON file
        SET SvIEN=+$PIECE(node,"^",1)
        IF SvIEN=0 GOTO GtSvDone
        SET node=$GET(^DIC(49,SvIEN,0)) ;"^DIC(49, is the SERVICE/SECTION file
        SET result=$PIECE(node,"^",1)

GtSvDone
        QUIT result


GetProvIEN(Provider)
        ;"Scope: PRIVATE
        ;"Purpose: To return IEN for Provider
        ;"Input: Provider -- the Provider to look up.
        ;"Results: returns Provider's IEN (i.e. Y), or -1 IF not found

        NEW DIC,X,Y
        SET DIC=200 ;"file 200 is NEW PERSON
        SET DIC(0)="M"
        SET X=Provider
        DO ^DIC ;" DO a         , value is returned in Y
        IF $find(Y,"^")>0 SET Y=$PIECE(Y,"^",1)

        QUIT Y


GetRecord(Document,NewDoc,AskOK,Editable)
        ;"Scope: PRIVATE
        ;"PURPOSE:
        ;"  To get a record--either via creating a NEW one, or returning an existing one
        ;"  Note: If an existing one is returned, it will be emptied first...
        ;"
        ;"  Note: If I want to MERGE part of what the doctor creates with what the
        ;"        transcriptionist uploads, here what I should do
        ;"        1. Look for an existing document with same date as document being uploaded.
        ;"        2. If found, look in existing document for MERGE symbols (i.e. {{1}} }
        ;"        3. If found, then take code from existing document and current part
        ;"                of upload buffer, and create a MERGEd document.
        ;"        4. Put this MERGEd document back into the upload buffer.
        ;"        5. Empty the existing document, and return its IEN from this function
        ;"
        ;"INPUT: Document -- array with Document("DFN"), Document(cDocType) are REQUIRED.
        ;" [Document] --> Visit info array -- SHOULD PASS BE REFERENCE.
        ;"              Document("DFN") = patient DFN
        ;"              Document(cVisitStr) = LOC;VDT;VTYP  e.g. 'x;x;OFFICE VISIT'
        ;"              Document(cVisitIEN) = VISIT file IFN  e.g. 0, used for field .03 in file 8925. Pointer to file #9000010
        ;"              Document(cHspLocIEN)  i.e. Hospital location IEN. Used for field 1205 in 8925.  Pointer to file #44
        ;"              Document(cVstLocIEN) i.e. visit location IEN. Used for field 1211 in 8925.  Pointer to file #44
        ;"              Document(cStopCode) = mark to defer workload e.g. 0/FALSE=don't worry about stop codes.
        ;"                 USED FOR: Mark record for deferred crediting of stop code (fld #.11)
        ;"                   This boolean field (.11) indicates whether the stop code associated with a new
        ;"                   visit should be credited when the note is completed.
        ;"                   Note: IF Document('STOP')="", then not processed.
        ;"              Document(cDocType)=1^title DA^title Name  i.e.:  1^128^OFFICE VISIT^OFFICE VISIT
        ;"              Document(cDocTIEN)=DocTIEN (a.k.a. title DA) e.g. 128
        ;"              Document(cService)  e.g.FAMILY PRACTICE
        ;"              Document(cStartDate)   i.e. event begin time
        ;"              Document(cEndDate)  i.e. event end time
        ;" [NewDoc] --> flag, passed back with
        ;"              NewDoc = 1 IF returned docmt is new
        ;"              NewDoc = 0 IF returned docmt already existed, timeout, etc
        ;" [AskOK] -->  Ask user flag, where
        ;"              AskOK = 1: ask re edit/addend existing docmt
        ;"              (Interactive List Manager options, TRY docmt def)
        ;"              AskOK = 0: don't ask (Upload & GUI options)
        ;" [Editable]-->flag, passed back with Editable = 1 IF returned
        ;"              PREEXISTING docmt can be edited by Provider. If
        ;"              preexisting docmt returned and 'Editable, then
        ;"              docmt cannot be edited by Provider.
        ;"
        ;"Results: Returns DocIEN -- IEN of document to use, or -1 IF error etc.
        ;"                Also, Document("DOC IEN") is SET to DocIEN
        ;"         Errors will be returned in Document("ERROR")
        ;"
        ;"Note:  Code originally from GETRECNM^TIUEDI3 -- KT 5/25/04

        NEW MultOK SET MultOK=1
        NEW DocIEN SET DocIEN=-1
        SET NewDoc=0

        IF +$GET(BuffNum)'=0 SET DocIEN=$$DocExists(.Document) ;"avoid error with RPC calls
        ELSE  SET DocIEN=0
        SET Document("DOC IEN")=DocIEN
        IF DocIEN>0 DO  GOTO GRDone  ;"DocIEN>0 means that the TEXT of the report is an exact match
        . KILL ^TIU(8925,DocIEN,"TEXT")  ;"Kill the TEXT prior report, so we can overwrite it
        ELSE  do
        . SET DocIEN=$$CreateRec(.Document)
        . SET NewDoc=1

GRDone ;
        IF NewDoc,DocIEN'>0 SET NewDoc=0
        SET Document("DOC IEN")=DocIEN
        QUIT DocIEN  ;"DocIEN is document number


DocExists(Document)
        ;"PURPOSE:  To return document IEN, IF it  already EXISTS for the
        ;"                given patient, title, and visit.
        ;"INPUT:  Document -- see documentation of format in $$GetRecord
        ;"Results: returns a value for document (i.e. DocIEN), or -1 IF no prior doc is found.
        ;"
        ;"Note: The following documents are ignored:
        ;"           - docmts of status deleted or retracted
        ;"         - all docmts IF run across a docmt w/ requesting pkg
        ;"         - If REQEDIT, then also ignore docmts PERSON cannot edit.
        ;"Note: If there are more than one, get the smallest DA.

        NEW DocIEN SET DocIEN=-1
        NEW index

        IF $DATA(^TIU(8925,"C",Document("DFN")))=0 GOTO DEDone
        ;"Scan through all documents for patient (DFN)
        SET index=$ORDER(^TIU(8925,"C",Document("DFN"),""))
        IF index="" GOTO DEDone
        FOR  DO  QUIT:(index="")
        . NEW DocCompValue
        . SET DocCompValue=$$CompToBuff(index,Document(cDocTIEN),Document(cStartDate))
        . IF DocCompValue=2 DO  QUIT  ;"i.e. documents are an exact match
        . . ;"For below, the document is the same as the upload buffer.
        . . ;"We have found our answer.
        . . ;"
        . . ;"Below is code I can use to check to see IF I SHOULD be editing.
        . . ;"------------------------------------------------------
        . . ;"new CANEDIT,CANDel
        . . ;"set CANEDIT=+$$CANDO^TIULP(index,"EDIT RECORD",Document("PROVIDER IEN"))
        . . ;"set CANDel=+$$CANDO^TIULP(index,"DELETE RECORD",Document("PROVIDER IEN"))
        . . ;"if +CANEDIT>0 SET DocIEN=index
        . . SET DocIEN=index SET index="" QUIT
        . SET index=$ORDER(^TIU(8925,"C",Document("DFN"),index))

DEDone
        QUIT DocIEN


BuffCharCount()
        ;"Purpose: To count the number of characters in the current upload buffer, for the
        ;"        current document.  The upload buffer puts all the documents being uploaded
        ;"        into one big WP array.  This function will count down until the text
        ;"        signal is found to start the next documnent (e.g. '[NewDict]')
        ;"Input: none.  However, several global-scope variables are used.
        ;"        By tracing through the upload code I know that
        ;"      the following variables are SET:
        ;"        (I saved DA as BuffNum, and TIUI as BuffIdx)
        ;"        TIUHSIG = [NewDict]  .. or whatever it has been SET to by user in upload params
        ;"        TIUBGN = [TEXT] ... or whatever is has be SET to by user in upload params.
        ;"        BuffIdx = the line index of the beginning of the report to be processed (i.e. the line
        ;"       that starts with [TEXT]
        ;"        BuffNum = the index in 8925.2, i.e. ^TIU(8925.2,BuffNum,"TEXT",0)
        ;"                     In other words, here BuffNum = the serial index number of the document to
        ;"                be uploaded i.e. 1 for the first, 2 for the second etc.
        ;"Notes
        ;"  8925.2 is file: TIU UPLOAD BUFFER
        ;"  To detect the beginning of the next document, use
        ;"     IF MyLine[TIUHSIG then abort
        ;"  I trim of leading and trailing white-space before counting.
        ;"        But, otherwise spaces will be counted
        ;"
        ;"Results: Returns character count, or 0 IF none found.

        NEW index
        NEW result SET result=0
        IF $GET(TIUHSIG)="" GOTO BuffCDone

        SET index=$ORDER(^TIU(8925.2,BuffNum,"TEXT",BuffIdx))
        FOR  DO  QUIT:(index="")
        . IF index="" QUIT
        . NEW s SET s=$GET(^TIU(8925.2,BuffNum,"TEXT",index,0))
        . IF s="" SET index="" QUIT
        . IF s[TIUHSIG SET index="" QUIT
        . SET s=$$Trim^TMGSTUTL(.s)
        . SET result=result+$LENGTH(s)
        . SET index=$ORDER(^TIU(8925.2,BuffNum,"TEXT",index))

BuffCDone
        QUIT result



PrepUploadBuf()
        ;"Purpose: Ensure upload buffer is ready for processing
        ;"Background: Transcriptionist will upload a large document containing
        ;"        multiple notes for different patients etc.  This entire large
        ;"        document is stored in the TIU UPLOAD BUFFER file (8925.2)
        ;"        When this filer code is called, the TIU upload process has already
        ;"        SET up some variables.
        ;"        DA = the IEN in 8925.2, i.e. ^TIU(8925.2,DA,"TEXT",0) that
        ;"                the uploaded text was temporarily store in.
        ;"        (I save DA as BuffNum)
        ;"        TIUI = the line index of the beginning of the report to
        ;"                be processed (i.e. the line that starts with [TEXT])
        ;"        (I save TIUI as BuffIdx)
        ;"        TIUHSIG = [NewDict]  .. or whatever it has been SET to by user in upload params
        ;"        TIUBGN = [TEXT] ... or whatever is has be SET to by user in upload params.
        ;"
        ;"        I found that transcriptionists were using word-processors that automatically
        ;"        wrapped the text to a next line.  Thus paragraphs were being uploaded as
        ;"        one very long line.  Rather than try to reeducate them to consistantly hit
        ;"        enter at the end of every line, I chose to automatically wrap the text to
        ;"        a SET width.
        ;"
        ;"        A global-scope var: cMaxNoteWidth is expected to be defined/
        ;"
        ;"        So, to prepair the upload buffer, I use these steps:
        ;"                1. Scan the part of the upload buffer pertaining to the
        ;"                   current note being processed
        ;"                        - This starts with line BuffIdx, and ends with...
        ;"                        - the line containing TIUHSIG (or end of buffer)
        ;"                   See IF any line is longer than cMaxNoteWidth characters.
        ;"                        If so, mark for wrapping.
        ;"                2. If wrapping needed, extract note to a temporary array
        ;"                3. Perform reformatting/wrapping on temp array.
        ;"                4. Put temp array back into Upload buffer
        ;"
        ;"Input: None, but global-scope vars used (see above)
        ;"Output: Upload buffer may be changed
        ;"Result: 1=OKToCont or cAbort

        NEW result SET result=1
        IF $$NeedsReformat(cMaxNoteWidth) do
        . NEW CurNote
        . NEW NextNoteI
        . NEW DoSpecialIndent SET DoSpecialIndent=1  ;"I.e. use hanging indents.)
        . SET NextNoteI=$$CutNote(.CurNote)
        . DO WordWrapArray^TMGSTUTL(.CurNote,cMaxNoteWidth,DoSpecialIndent)
        . SET result=$$PasteNote(.CurNote,NextNoteI)
PULBFDone
        QUIT result


NeedsReformat(MaxWidth)
        ;"Purpose: To scan the single note being processed, to see if
        ;"        it is too wide (i.e. any line of length > MaxWidth
        ;"        I had to DO this because transcriptionists were using
        ;"        a wordprocessor that wrapped lines.  Then when uploaded
        ;"        each paragraph became one long line.
        ;"        Also, will fix extended ASCII characters
        ;"Input: MaxWidth The max length of any line (i.e. 80 for 80 chars)
        ;"        Also depends on global-scope vars
        ;"Result: 1= A line was found that is > MaxWidth
        ;"          0= no long lines found

        NEW index
        NEW result SET result=0
        IF $GET(TIUHSIG)="" GOTO NRFMDone
        IF $GET(MaxWidth)'>0 GOTO NRFMDone

        SET index=$ORDER(^TIU(8925.2,BuffNum,"TEXT",BuffIdx))
        IF index'="" FOR  DO  QUIT:(index="")
        . NEW s
        . SET s=$GET(^TIU(8925.2,BuffNum,"TEXT",index,0))
        . IF s="" SET index="" QUIT
        . ;"9/19/06 Added to remove extended ASCII characters
        . ;"set s=$TRANSLATE(s,$c(146)_$c(246)_$c(150)_$c(147)_$c(148),"'--""""")
        . IF s[TIUHSIG SET index="" QUIT
        . IF $LENGTH(s)>MaxWidth DO  QUIT
        . . SET result=1
        . . SET index=""
        . SET index=$ORDER(^TIU(8925.2,BuffNum,"TEXT",index))

NRFMDone
        QUIT result


CutNote(Array)
        ;"Purpose: To extract the current note out of the entire upload buffer
        ;"Input: Array -- MUST BE PASSED BY REFERENCE.  This is an OUT parameter
        ;"        Array will be loaded with the note, with the first line being
        ;"        put into Array(1)
        ;"        Depends on global-scope vars BuffIdx, BuffNum, TIUHSIG, SET up elsewhere.
        ;"Note: This function empties the lines in TIU UPLOAD BUFFER as it cuts out note.
        ;"Result: Returns:
        ;"                #:   index of line containing start of next note.
        ;"                -1:  Error
        ;"                  0:  Note is the last one in the upload buffer, so no next note found

        NEW index
        NEW LastI SET LastI=0
        NEW result SET result=-1
        KILL Array
        IF $GET(TIUHSIG)="" GOTO ExNDone
        NEW ArrayI SET ArrayI=0
        NEW s
        NEW Done SET Done=0

        SET index=$ORDER(^TIU(8925.2,BuffNum,"TEXT",BuffIdx))

        IF index'="" FOR  DO  QUIT:(index="")!(Done=1)
        . SET s=$GET(^TIU(8925.2,BuffNum,"TEXT",index,0))
        . IF s[TIUHSIG SET Done=1 QUIT
        . SET ArrayI=ArrayI+1
        . SET Array(ArrayI)=s
        . KILL ^TIU(8925.2,BuffNum,"TEXT",index)
        . SET LastI=index
        . SET index=$ORDER(^TIU(8925.2,BuffNum,"TEXT",index))

        SET result=+index
        IF result=0 SET result=LastI
ExNDone
        QUIT result



PasteNote(Array,NextNoteI)
        ;"Purpose: To put Array back into the upload buffer, at the correct location,
        ;"Input: Array -- Best IF PASSED BY REFERENCE.
        ;"        Array is expected to be loaded with the note, with the first line Array(1)
        ;"        NextNoteI: This is the index, in upload buffer, of the start of the next note.
        ;"Depends on global-scope vars BuffIdx, BuffNum, TIUHSIG, SET up elsewhere.
        ;"Result: 1=OKToCont IF all OK, or cAbort IF error

        NEW EntireBuf
        NEW IndexInc SET IndexInc=0.01  ;"WP^DIE does not require integer indexes.
        NEW ArrayI,PasteI
        NEW s
        NEW Done SET Done=0
        NEW result SET result=cAbort
        MERGE EntireBuf=^TIU(8925.2,BuffNum,"TEXT")
        KILL EntireBuf(0) ;"remove ^^<line count>^<line count>^<fm date>^^

        SET ArrayI=$ORDER(Array(""))
        SET PasteI=BuffIdx+1
        FOR  DO  QUIT:((Done=1)!(ArrayI=""))
        . IF $DATA(Array(ArrayI))#10=0 SET Done=1 QUIT
        . SET s=Array(ArrayI)
        . SET EntireBuff(PasteI,0)=s
        . SET PasteI=PasteI+IndexInc
        . IF PasteI>NextNoteI DO  QUIT
        . . DO SHOWERR^TMGDEBU2(PriorErrorFound,"Insufficient room to put note back into upload buffer.")
        . . SET Done=1
        . SET ArrayI=$ORDER(Array(ArrayI))

        Set result=$$WriteWP^TMGDBAPI(8925.2,BuffNum,1,.EntireBuff)

        QUIT result


CompToBuff(ExistingIEN,UplTIEN,UplDate)
        ;"PURPOSE: To compare the document being uploaded (i.e. in the file 8925.2, TIU upload buffer)
        ;"           to documents already existing in database
        ;"Input: ExistingIEN -- the document IEN of a pre-existing document in the database.
        ;"                  i.e. ^TIU(8925,ExistingIEN,*)
        ;"       UplTIEN=The type number of document being uploaded
        ;"         UplDate -- the date of the document being uploaded.
        ;"      NOTE: See also global-scope variables below that are REQUIRED
        ;"
        ;"Output: returns 0 IF TEXT or Date different
        ;"                1 IF TEXT only is the same (Title is different)
        ;"                2 IF TEXT & Title are same
        ;"
        ;"------------------------------------------------------------------------------------
        ;"Programming Note: By tracing through the upload code I know that
        ;"                  the following variables are SET:
        ;"                        (I saved DA as BuffNum, and TIUI as BuffIdx)
        ;"TIUHSIG = [NewDict]  .. or whatever it has been SET to by user in upload params
        ;"TIUBGN = [TEXT] ... or whatever is has be SET to by user in upload params.
        ;"BuffIdx = the line index of the beginning of the report to be processed (i.e. the line
        ;"       that starts with [TEXT]
        ;"BuffNum = the index in 8925.2, i.e. ^TIU(8925.2,BuffNum,"TEXT",0)
        ;"     In other words, here BuffNum = the serial index number of the document to be uploaded
        ;"     i.e. 1 for the first, 2 for the second etc.
        ;"     Note 8925.2 is file: TIU UPLOAD BUFFER
        ;"Note
        ;"  To detect the beginning of the next document, use
        ;"  IF MyLine[TIUHSIG then abort

        NEW MaxUplLine
        NEW DocLine,UplLine
        NEW DocData,UplData
        NEW result SET result=0
        NEW MaxDocLine,CompLine
        NEW DocType,DocName
        NEW Break SET Break=0
        NEW DocDate

        ;"First, see IF dates are the same.  If not, bail out.
        SET DocDate=$PIECE(^TIU(8925,ExistingIEN,0),"^",7)
        IF DocDate'=UplDate GOTO CompExit  ;"Quit with result=0

        SET MaxUplLine=$PIECE($GET(^TIU(8925.2,BuffNum,"TEXT",0)),"^",3)
        IF MaxUplLine="" GOTO CompExit
        SET MaxDocLine=$PIECE($GET(^TIU(8925,ExistingIEN,"TEXT",0)),"^",3)
        IF MaxDocLine="" GOTO CompExit

        SET UplLine=BuffIdx
        SET DocLine=0

        ;"Compare the two documents line by line.
        for i=1:1:(MaxUplLine-UplLine) DO  IF Break GOTO CompExit
        . SET UplData=$GET(^TIU(8925.2,BuffNum,"TEXT",UplLine+i,0))
        . SET DocData=$GET(^TIU(8925,ExistingIEN,"TEXT",DocLine+i,0),"x")
        . IF UplData[TIUHSIG SET i=MaxUplLine QUIT
        . IF UplData'=DocData SET Break=1 QUIT
        . QUIT

        ;"If we have gotten this far, then the text is an identical match.
        SET result=1

        ;"Now check to see IF the dictation type is the same.
        SET DocType=$PIECE($GET(^TIU(8925,ExistingIEN,0)),"^",1)
        IF DocType=UplTIEN SET result=2

CompExit
        QUIT result


 ;------------------------------------------------------------------------
CreateRec(Document) ;
        ;"Purpose: Create document record - Returns DA
        ;"Input: Document -- an array with document info.  See GetRecord for documentation
        ;"Ouput: DocIEN (internal entry number) of entry created, or -1 IF failure
        ;"       Errors (if any) returned in Document("ERROR")
        ;"
        ;"Note: This was originally taken from TIUEDI3

        NEW cAbort SET cAbort=0
        NEW result SET result=1  

        NEW DIC,DLAYGO,X,Y,DIE,DR

        NEW DocIEN SET DocIEN=-1
        NEW TMGFDA,RecNum,TMGMSG,Flags
        SET TMGFDA(8925,"+1,",.01)="`"_Document(cDocTIEN)
        SET Flags="E"

        ;"======================================================
        ;"Call UPDATE^DIE -- add NEW entries in files or subfiles.
        ;"======================================================
        do
        . NEW $ETRAP SET $ETRAP="do ErrTrp^TMGDBAPI"
        . SET ^TMP("TMG",$J,"ErrorTrap")=result
        . SET ^TMP("TMG",$J,"Caller")="UPDATE^DIE"
        . DO UPDATE^DIE(Flags,"TMGFDA","RecNum","TMGMSG")
        . SET result=^TMP("TMG",$J,"ErrorTrap")
        . KILL ^TMP("TMG",$J,"ErrorTrap")
        ;"======================================================
        ;"======================================================

        IF result'=1 GOTO CRDone  ;"1=cOKToCont
        IF $DATA(TMGMSG("DIERR")) DO  GOTO CRDone
        . DO SHOWDIER^TMGDEBU2(.TMGMSG,.PriorErrorFound)
        . SET DocIEN=-1
        . MERGE Document("ERROR","DIERR")=TMGMSG
        do
        . NEW index SET index=$ORDER(RecNum(""))
        . IF index'="" SET DocIEN=+$GET(RecNum(index))
        IF DocIEN=0 SET DocIEN=-1

CRDone
        ;"Now check for failure.  DocIEN will equal record number, or -1 IF failure
        IF DocIEN'>0 DO  GOTO CRDone
        . NEW n SET n=+$GET(Document("ERROR","NUM"))+1
        . SET Document("ERROR",n)=$PIECE(Document(cDocType),"^",3)_" record could not be created."
        SET Document("DOC IEN")=DocIEN

        QUIT DocIEN



 ;------------------------------------------------------------------------
StuffRec(Document,PARENT)
        ;"Purpose: Stuff fixed field data
        ;"INPUT:
        ;"  Document = An array containing information to put into document.
        ;"               The array should contain the following:
        ;"                Document("DOC IEN") -- the document IEN
        ;"                Document("PROVIDER IEN") -- the IEN of the provider
        ;"                Document("DFN") -- the patient IEN
        ;"                Document(cVisitIEN) -- a link to a visit entry
        ;"                Document(cStartDate)  -- episode begin date/time
        ;"                Document(cEndDate)  -- episode end date/time
        ;"                Document(cHspLocIEN) -- hospital location (Document(cVstLocIEN) used NULL)
        ;"                Document(cVstLocIEN) -- visit location.
        ;"                Document(cService) -- service (i.e. FAMILY PRACTICE)
        ;"                Document(cVisitStr)
        ;"                Document("TRANSCRIPTIONIST") -- the name of the transcriptionist
        ;"                Document("CHARACTER COUNT - TRANSCRIPTIONIST'S") -- the char count creditable to transcriptionist
        ;"                Document("LINE COUNT") -- Total line count
        ;"  PARENT:  If we are working with an addendum to a document, then
        ;"                parent is the internal entry number of the original parent document
        ;"                Note:DocID can be null IF not needed.
        ;"                Note: I don't ever pass a parent, currently
        ;"
        ;"NOTE: The following global-scope variables are also referenced
        ;"        TIUDDT
        ;"Results: Passes back document IEN, or -1 IF error.
        ;"         NOTE: IF result is -1 then errors are passed back in
        ;"              Document("ERROR") node
        ;"              Document("ERROR",n)="ERROR.. Stuffing NEW document."
        ;"              Document("ERROR","NUM")=n
        ;"              Document("ERROR","FM INFO")=MERGE with DIERR array

        NEW TMGFDA,TMGMSG
        NEW RefDate
        NEW DocIEN SET DocIEN=$GET(Document("DOC IEN"),-1)
        IF DocIEN=-1 GOTO SfRecDone
        NEW result SET result=DocIEN ;"default to success
        NEW ParentDocType

        ;"Field (f) constants
        NEW fPatient SET fPatient=.02        ;"field .02 = PATIENT
        NEW fVisit SET fVisit=.03            ;"field .03 = VISIT
        NEW fParentDoc SET fParentDoc=.04    ;"field .04 = PARENT DOCUMENT TYPE
        NEW fStatus SET fStatus=.05          ;"field .05 = STATUS
        NEW fParent SET fParent=.06          ;"field .06 = PARENT
        NEW fStartDate SET fStartDate=.07    ;"EPISODE BEGIN DATE/TIME (field .07)
        NEW fEndDate SET fEndDate=.08        ;"EPISODE END DATE/TIME (field .08)
        NEW fEntryDate SET fEntryDate=1201   ;"field 1201 = ENTRY DATE/TIME
        NEW fAuthor SET fAuthor=1202         ;"field 1202 = PERSON/DICTATOR
        NEW fExpSigner SET fExpSigner=1204   ;"field 1204 = expected Signer
        NEW fHospLoc SET fHospLoc=1205       ;"field 1205 = HOSPITAL LOCATION
        NEW fExpCosign SET fExpCosign=1208   ;"field 1208 = expected cosigner
        NEW fAttending SET fAttending=1209   ;"field 1209 = ATTENDING
        NEW fVisitLoc SET fVisitLoc=1211     ;"field 1211 = VISIT LOCATION
        NEW fRefDate SET fRefDate=1301       ;"field 1301 = REFERENCE DATE
        NEW fEnteredBy SET fEnteredBy=1302   ;"field 1302 = ENTERED BY (a pointer to file 200)
        NEW fCapMethod SET fCapMethod=1303   ;"field 1303 = CAPTURE METHOD;  U-->'upload'
        NEW fService SET fService=1404       ;"field 1404 = SERVICE
        NEW fSignedBy SET fSignedBy=1502     ;"field 1502 = signed by
        NEW fNeedCosign SET fNeedCosign=1506 ;"field 1506 = cosigniture expected.
        NEW fCharTrans SET fCharTrans=22711  ;"field 22711 = CHAR COUNT -- TRANSCRIPTIONIST
        NEW fLineCount SET fLineCount=.1      ;"field .1 = LINE COUNT

        ;"8925=TIU DOCUMENT, the file we will edit
        ;"do Set8925Value(.TMGFDA,Document("DFN"),fPatient,1)  ;"Will file separatedly below.
        DO Set8925Value(.TMGFDA,Document(cVisitIEN),fVisit,1)
        DO Set8925Value(.TMGFDA,Document("PROVIDER IEN"),fAuthor,1)
        DO Set8925Value(.TMGFDA,Document("PROVIDER IEN"),fExpSigner,1)
        ;"elh 5/6/14  setting Attending caused
        ;"Expected co-signer (1208) to be set if alert
        ;"was created  DO Set8925Value(.TMGFDA,Document("PROVIDER IEN"),fAttending,1)
        DO Set8925Value(.TMGFDA,Document(cHspLocIEN),fHospLoc,1)
        DO Set8925Value(.TMGFDA,Document(cVstLocIEN),fVisitLoc,1)
        DO Set8925Value(.TMGFDA,Document("TRANSCRIPTIONIST"),fEnteredBy,0)   ;"VA transcriptionist field
        DO Set8925Value(.TMGFDA,Document("CHARACTER COUNT - TRANSCRIPTIONIST'S"),fCharTrans,0)

        IF $DATA(Document("LINE COUNT")) do
        . DO Set8925Value(.TMGFDA,Document("LINE COUNT"),fLineCount,0)

        SET ParentDocType=$$DOCCLASS^TIULC1(+$PIECE(DocIEN,"^",2))
        IF +ParentDocType>0 DO Set8925Value(.TMGFDA,ParentDocType,fParentDoc,1)

        IF $GET(Document("AUTO SIGN"))=1 do
        . DO Set8925Value(.TMGFDA,"COMPLETED",fStatus,0)
        . DO Set8925Value(.TMGFDA,Document("PROVIDER IEN"),fSignedBy,1)
        ELSE  do
        . DO Set8925Value(.TMGFDA,"UNSIGNED",fStatus,0)

        IF +$GET(PARENT)'>0 do
        . ;"do Set8925Value(.TMGFDA,Document("DFN"),fPatient,1)
        . DO Set8925Value(.TMGFDA,Document(cVisitIEN),fVisit,1)
        . DO Set8925Value(.TMGFDA,Document(cStartDate),fStartDate,0)
        . DO Set8925Value(.TMGFDA,Document(cEndDate),fEndDate,0)
        . DO Set8925Value(.TMGFDA,Document(cService),fService,0)
        IF +$GET(PARENT)>0 do
        . NEW NodeZero SET NodeZero=$GET(^TIU(8925,+PARENT,0))
        . NEW Node12 SET Node12=$GET(^TIU(8925,+PARENT,12))
        . NEW Node14 SET Node14=$GET(^TIU(8925,+PARENT,14))
        . ;"
        . DO Set8925Value(.TMGFDA,PARENT,fParent,1)
        . DO Set8925Value(.TMGFDA,$PIECE(NodeZero,"^",pPatient),fPatient,1)
        . DO Set8925Value(.TMGFDA,$PIECE(NodeZero,"^",pVisit),fVisit,1)
        . DO Set8925Value(.TMGFDA,$PIECE(NodeZero,"^",pStrtDate),fStartDate,0)
        . DO Set8925Value(.TMGFDA,$PIECE(NodeZero,"^",pEndDate),fEndDate,0)
        . DO Set8925Value(.TMGFDA,$PIECE(Node12,"^",pHospLoc),fHospLoc,1)
        . DO Set8925Value(.TMGFDA,$PIECE(Node14,"^",pService),fService,0)

        DO Set8925Value(.TMGFDA,$$NOW^TIULC,fEntryDate,0)
        DO Set8925Value(.TMGFDA,Document(cHspLocIEN),fHospLoc,1)
        DO Set8925Value(.TMGFDA,Document(cVstLocIEN),fVisitLoc,1)
        DO Set8925Value(.TMGFDA,Document(cStartDate),fRefDate,0)
        DO Set8925Value(.TMGFDA,"U",fCapMethod,0)   ;"  U-->'upload'
        ;"do Set8925Value(.TMGFDA,3,fStatus,0)

        KILL ^TMG("TMP","EDDIE")
        ;"MERGE ^TMG("TMP","EDDIE","INSIDE DOCUMENT")=Document  ;"TEMP!!
        MERGE ^TMG("TMP","EDDIE","FDA")=TMGFDA  ;"TEMP!!

        DO FILE^DIE("EK","TMGFDA","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO SfRecDone
        . SET result=-1
        . MERGE Document("ERROR","FM INFO")=TMGMSG("DIERR")

        ;" -- [Mark record for deferred crediting of stop code (fld #.11)]: --
        IF +$GET(Document("STOP")) do
        . DO DEFER^TIUVSIT(DocIEN,+$GET(Document("STOP")))

        ;"Try storing .02 field separately to avoid weird filing error
        KILL TMGFDA
        KILL ^TMG("TMP","EDDIE")
        NEW PtDFN SET PtDFN=Document("DFN")
        IF (+PtDFN'=PtDFN),(PtDFN["`") SET PtDFN=$PIECE(PtDFN,"`",2)
        IF +PtDFN>0 do
        . SET TMGFDA(8925,DocIEN_",",.02)=PtDFN
        . MERGE ^TMG("TMP","EDDIE","FDA")=TMGFDA  ;"TEMP!!
        . DO FILE^DIE("K","TMGFDA","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) do
        . . SET result=-1
        . . MERGE Document("ERROR","FM INFO")=TMGMSG("DIERR")

SfRecDone
        QUIT result


Set8925Value(TMGFDA,Value,Field,IsIEN)
        ;"Purpose: To provide a clean means of loading values into fields, into TMGFDA(8925,DOCIEN)
        ;"Input: TMGFDA -- The array to fill
        ;"       Value -- the value to load
        ;"       Field -- the field
        ;"       IsIEN = 1 IF value is an IEN
        ;"Note: DEPENDS ON GLOBAL-SCOPE VARIABLES:  DocIEN,Document

        IF ($GET(Value)'="")&($DATA(Field)>0) do
        . IF $GET(IsIEN)>0,$EXTRACT(Value,1)'="`" SET Value="`"_+Value
        . IF Value'="`0" SET TMGFDA(8925,DocIEN_",",Field)=Value
        QUIT



 ;"-----------------------------------------------------------------------------------------------
 ;"==============================================================================================-
 ;" F O L L O W - U P   C O D E
 ;"==============================================================================================-
 ;"-----------------------------------------------------------------------------------------------

FOLLOWUP(DocIEN) ;" Post-filing code for PROGRESS NOTES
        ;"PURPOSE:
        ;"  This function is called by the TIU upload document facilities.
        ;"  it is called after the text has been put into the document
        ;"
        ;"INPUT:
        ;" DocIEN  -- is passed a value held in TIUREC("#"), i.e.
        ;"                   DO FOLLOWUP^TIUPUTN1(TIUREC("#")).

        WRITE !
        WRITE "+-------------------------------------+",!
        WRITE "| Starting Follow-up code...          |",!
        WRITE "+-------------------------------------+",!

        IF $DATA(cOKToCont)#10=0 NEW cOKToCont SET cOKToCont=1
        IF $DATA(cAbort)#10=0 NEW cAbort SET cAbort=0

        NEW TMGDBINDENT,PriorErrorFound
        NEW result SET result=1 ;" 1=cOKToCont

        NEW Document MERGE Document=TMGDOC

        NEW cStartDate SET cStartDate="EDT"
        NEW cEndDate SET cEndDate="LDT"
        NEW cService SET cService="SVC"
        NEW cDocType SET cDocType="TYPE"
        NEW cDocTIEN SET cDocTIEN="TYPE IEN"
        ;"new cDocIEN SET cDocIEN="DOC IEN"
        ;"new cPatIEN SET cPatIEN="DFN"   ;"DFN = Patient IEN
        NEW cHspLocIEN SET cHspLocIEN="LOC"
        NEW cVstLocIEN SET cVstLocIEN="VLOC"
        NEW cVisitStr SET cVisitStr="VSTR"
        NEW cVisitIEN SET cVisitIEN="VISIT"
        NEW cStopCode SET cStopCode="STOP"

        ;" 'p constants
        NEW pPatient SET pPatient=2      ;"Node 0,piece 2 = PATIENT (field .02)
        NEW pVisit SET pVisit=3          ;"Node 0,piece 3 = VISIT (field .03)
        NEW pStrtDate SET pStrtDate=7    ;"Node 0,piece 7 = EPISODE BEGIN DATE/TIME (field .07)
        NEW pEndDate SET pEndDate=8      ;"Node 0,piece 8 = EPISODE END DATE/TIME (field .08)

        NEW pAuthor SET pAuthor=2        ;"Node 12,piece 2 = AUTHOR/DICTATOR (field 1202)
        NEW pExpSigner SET pExpSigner=4  ;"Node 12,piece 4 = EXPECTED SIGNER (field 1204)
        NEW pHospLoc SET pHospLoc=5      ;"Node 12,piece 5 = field 1205 = HOSPITAL LOCATION
        NEW pAttending SET pAttending=9  ;"Node 12,piece 9 = ATTENDING PHYSICIAN (field 1209)
        NEW pExpCosign SET pExpCosign=8  ;"Node 12,piece 8 = EXPECTED COSIGNER (field 1210)
        NEW pVstLoc SET pVstLoc=11       ;"Node 12,piece 11 = field 1211 = VISIT LOCATION

        ;"Field (f) constants
        NEW fPatient SET fPatient=.02        ;"field .02 = PATIENT
        NEW fVisit SET fVisit=.03            ;"field .03 = VISIT
        NEW fParentDoc SET fParentDoc=.04    ;"field .04 = PARENT DOCUMENT TYPE
        NEW fStatus SET fStatus=.05          ;"field .05 = STATUS
        NEW fParent SET fParent=.06          ;"field .06 = PARENT
        NEW fStartDate SET fStartDate=.07    ;"EPISODE BEGIN DATE/TIME (field .07)
        NEW fEndDate SET fEndDate=.08        ;"EPISODE END DATE/TIME (field .08)
        NEW fEntryDate SET fEntryDate=1201   ;"field 1201 = ENTRY DATE/TIME
        NEW fAuthor SET fAuthor=1202         ;"field 1202 = AUTHOR/DICTATOR
        NEW fExpSigner SET fExpSigner=1204   ;"field 1204 = expected Signer
        NEW fHospLoc SET fHospLoc=1205       ;"field 1205 = HOSPITAL LOCATION
        NEW fExpCosign SET fExpCosign=1208   ;"field 1208 = expected cosigner
        NEW fVisitLoc SET fVisitLoc=1211     ;"field 1211 = VISIT LOCATION
        NEW fRefDate SET fRefDate=1301       ;"field 1301 = REFERENCE DATE
        NEW fCapMethod SET fCapMethod=1303   ;"field 1303 = CAPTURE METHOD;  U-->'upload'
        NEW fService SET fService=1404       ;"field 1404 = SERVICE
        NEW fNeedCosign SET fNeedCosign=1506 ;"field 1506 = cosigniture expected.
        NEW fSignedBy SET fSignedBy=1502     ;"field 1502 = signed by

        NEW TMGFDA,TMGMSG
        NEW DFN
        NEW Attending,ExpSigner,ExpCosign,Author
        NEW BailOut SET BailOut=0
        NEW Node12 SET Node12=$GET(^TIU(8925,DocIEN,12))
        NEW NodeZero SET NodeZero=$GET(^TIU(8925,DocIEN,0))
        IF $DATA(Document)=0 NEW Document

        SET Author=+$PIECE(Node12,"^",pAuthor)
        SET Attending=+$PIECE(Node12,"^",pAttending)
        SET ExpCosign=+$PIECE(Node12,"^",pExpCosign)
        SET ExpSigner=+$PIECE(Node12,"^",pExpSigner)

        do
        . NEW Signer SET Signer=$$WHOSIGNS^TIULC1(DocIEN)
        . DO Set8925Value(.TMGFDA,$$WHOSIGNS^TIULC1(DocIEN),fExpSigner,1)

        IF (Attending>0)&(ExpCosign=0) do
        . DO Set8925Value(.TMGFDA,$$WHOCOSIG^TIULC1(DocIEN),fExpCosign,1)

        IF (ExpCosign>0)&(ExpSigner'=ExpCosign) do
        . DO Set8925Value(.TMGFDA,1,fNeedCosign,0)

        SET result=$$dbWrite^TMGDBAPI(.TMGFDA,1)
        IF result=-1 GOTO FUDone

        DO RELEASE^TIUT(DocIEN,1)  ;"Call function to 'Release Document from transcription'
        DO AUDIT^TIUEDI1(DocIEN,0,$$CHKSUM^TIULC("^TIU(8925,"_+DocIEN_",""TEXT"")"))  ;"Update audit trail

        IF '$DATA(Document) DO  IF (BailOut=1) GOTO FUDone
        . NEW VstLocIEN,HspLocIEN,StartDate,EndDate
        . IF $DATA(NodeZero)#10=0 DO  QUIT
        . . SET BailOut=1
        . SET DFN=+$PIECE(NodeZero,"^",pPatient)
        . SET StartDate=+$PIECE(NodeZero,"^",pStrtDate)
        . SET EndDate=$$FMADD^XLFDT(StartDate,1)
        . SET Document(cHspLocIEN)=+$PIECE(Node12,"^",pHospLoc)
        . SET Document(cVstLocIEN)=+$PIECE(Node12,"^",pVstLoc)
        . SET VstLocIEN=Document(cVstLocIEN)
        . IF VstLocIEN'>0 SET VstLocIEN=Document(cHspLocIEN)
        . IF (DFN>0)&(StartDate>0)&(EndDate>0)&(VstLocIEN>0) do
        . . ;"This is an interactive visit         ....
        . . DO MAIN^TIUVSIT(.Document,DFN,"",StartDate,EndDate,"LAST",0,VstLocIEN)

        IF $DATA(Document)=0 GOTO FUDone
        IF $DATA(Document(cVisitStr))#10=0 GOTO FUDone
        IF $DATA(DFN)=0 SET DFN=$GET(Document("DFN")) IF DFN="" GOTO FUDone

        ;"Note: reviewing the code for ENQ^TIUPXAP1, it appears the following is expected:
        ;"        .TIU array
        ;"        DFN -- the patient IEN
        ;"        DA -- the IEN of the document to work on.
        ;"        TIUDA -- the doc IEN that was passed to this function.
        ;"                Note, I'm not sure how DA and TIUDA are used differently.
        ;"                In fact, IF $DATA(TIUDA)=0, then function uses DA.
        ;"                Unless I KILL TIUDA (which might cause other problems), I don't
        ;"                know IF TIUDA will hold an abherent value.  So I'll SET to DA
        do
        . NEW TIUDA SET TIUDA=DocIEN
        . NEW DA SET DA=DocIEN
        . NEW TIU MERGE TIU=Document
        . DO ENQ^TIUPXAP1 ;" Get/file VISIT

FUDone  ;
        KILL TMGDOC
        QUIT


 ;"-----------------------------------------------------------------------------------------------
 ;"==============================================================================================-
 ;" R E - F I L I N G   C O D E
 ;"==============================================================================================-
 ;"-----------------------------------------------------------------------------------------------

REFILE
        ;"Purpose: Somtimes the upload process fails because of an error in the
        ;"        upload filing code.  Rather than require a re-upload of the file,
        ;"        this function will trigger a retry of filing the TIU UPLOAD BUFFER
        ;"        (file 8925.2)
        ;"This function is called by menu option TMG REFILE UPLOAD

        NEW TIUDA SET TIUDA=""
              NEW job
        NEW DoRetry SET DoRetry=""
        NEW Abort SET Abort=0
        NEW Found SET Found=0

        WRITE !,!
        WRITE "------------------------------------------------",!
        WRITE " Refiler for failed uploads (i.e. a second try.)",!
        WRITE "------------------------------------------------",!,!

        WRITE "Here are all the failed uploads:",!,!
        SET job=$ORDER(^TIU(8925.2,"B",""))
        FOR  DO  QUIT:(job="")
        . NEW Buff,NextBuff
        . IF job="" QUIT
        . SET Buff=$ORDER(^TIU(8925.2,"B",job,""))
        . FOR  DO  QUIT:(Buff="")
        . . IF Buff="" QUIT
        . . WRITE "Buffer #"_Buff_" (created by process #"_job_")",!
        . . SET Found=1
        . . SET Buff=$ORDER(^TIU(8925.2,"B",job,Buff))
        . SET job=$ORDER(^TIU(8925.2,"B",job))

        IF Found=0 WRITE "(There are no failed uploads to process... Great!)",!
        ELSE  WRITE "------------------------------------------------",!

        SET job=$ORDER(^TIU(8925.2,"B",""))
        FOR  DO  QUIT:(job="")!(Abort=1)
        . NEW Buff,NextBuff
        . IF job="" QUIT
        . SET Buff=$ORDER(^TIU(8925.2,"B",job,""))
        . FOR  DO  QUIT:(Buff="")!(Abort=1)
        . . IF Buff="" QUIT
        . . IF DoRetry'="all" do
        . . . WRITE !,"Refile upload buffer #"_Buff_" (created by process #"_job_")? (y/n/all/^) "
        . . . read DoRetry:$GET(DTIME,300),!
        . . ELSE  do
        . . . NEW GetKey
        . . . read *GetKey:0
        . . . IF $GET(GetKey)=27 SET DoRetry="n"
        . . . ELSE  WRITE !,!,"Processing upload buffer #",Buff,!
        . . IF DoRetry="^" SET Abort=1 QUIT
        . . IF (DoRetry["y")!(DoRetry["Y")!(DoRetry="all") do
        . . . SET TIUDA=Buff
        . . . ;"These is an edited form of MAIN^TIUUPLD
        . . . N EOM,TIUERR,TIUHDR,TIULN,TIUSRC,X
        . . . I '$D(TIUPRM0)!'$D(TIUPRM1) D SETPARM^TIULE
        . . . S TIUSRC=$P($G(TIUPRM0),U,9),EOM=$P($G(TIUPRM0),U,11)
        . . . I EOM']"",($P(TIUPRM0,U,17)'="k") DO  QUIT
        . . . . W !,$C(7),$C(7),$C(7),"No End of Message Signal Defined - Contact IRM.",!
        . . . S:TIUSRC']"" TIUSRC="R"
        . . . S TIUHDR=$P(TIUPRM0,U,10)
        . . . I TIUHDR']"" DO  QUIT
        . . . . W $C(7),$C(7),$C(7),"No Record Header Signal Defined - Contact IRM.",!
        . . . NEW temp SET temp=$ORDER(^TIU(8925.2,TIUDA,"TEXT",0))
        . . . WRITE "First line of TEXT=",temp,!
        . . . I +$O(^TIU(8925.2,TIUDA,"TEXT",0))>0 do
        . . . . WRITE "Calling FILE^TIUUPLD("_TIUDA_")",!
        . . . . D FILE^TIUUPLD(TIUDA)
        . . . I +$O(^TIU(8925.2,TIUDA,"TEXT",0))'>0 D BUFPURGE^TIUPUTC(TIUDA)
        . . SET Buff=$ORDER(^TIU(8925.2,"B",job,Buff))
        . SET job=$ORDER(^TIU(8925.2,"B",job))

        WRITE !,"------------------------------------------------",!
        WRITE " All done with Refiler",!
        WRITE "------------------------------------------------",!,!

RFDone
        Q




