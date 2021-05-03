TMGTRAN1 ;TMG/kst/TRANSCRIPTION REPORT FUNCTIONS -- UI ;03/25/06, 8/8/10, 2/2/14, 3/24/21
         ;;1.0;TMG-LIB;**1**;09/01/05

 ;" TRANSCRIPTION REPORT FUNCTIONS

        ;"=======================================================================
        ;" API -- Public Functions.
        ;"=======================================================================
        ;"RPTCUR
        ;"RPTASK
        ;"RPTQUIET(OPTIONS)
        ;"FREECUR
        ;"FREEASK
        ;"ScanSign(OPTIONS,SIGNED)
        ;"PWDSNOOP(IEN)
        ;"SHOWUNSIGNED
        ;"SIGNDOC(DocIEN,OPTIONS)
        ;"PRINT(DocArray) ; Prompt and print, or array



        ;"=======================================================================
        ;" Private Functions.
        ;"=======================================================================
        ;"AskDatesRPT(Options)
        ;"FreeDocs(AuthorIEN,ShowDetails)

        ;"=======================================================================
RPTCUR
        ;"SCOPE: PUBLIC
        ;"Purpose: To report transcription productivity for the current user (DUZ)
        ;"Input: none.  User will be asked for start and end dates
        ;"Output: Produces a report to choses output channel.

        NEW Options

        WRITE !,"-- TRANSCRIPTION PRODUCTIVITY CREDIT REPORT -- ",!!
        WRITE "Showing credit for: ",$PIECE($GET(^VA(200,DUZ,0)),"^",1),!!

        SET Options("TRANS")=DUZ
        DO AskDatesRPT(.Options)

        QUIT

RPTASK
        ;"SCOPE: PUBLIC
        ;"Purpose: To report transcription productivity for a chosen user
        ;"Input: none.  User will be asked for the user to report on, and also
        ;"        start and end dates
        ;"Output: Produces a report to choses output channel.

        NEW Options

        ;"set TMGDEBUG=1  ;"TEMP!!!

        WRITE !,"-- TRANSCRIPTION PRODUCTIVITY CREDIT REPORT -- ",!!

        SET DIC=200  ;"NEW PERSON file
        SET DIC(0)="MAQE"
        SET DIC("A")="Enter name of transcriptionist (^ to abort): "
        DO ^DIC
        IF +Y=-1 DO  GOTO RADone
        . WRITE !,"No transcriptionist selected.  Aborting report.",!

        SET Options("TRANS")=+Y

        DO AskDatesRPT(.Options)
RADone
        QUIT

RPTCURA
        ;"SCOPE: PUBLIC
        ;"Purpose: To report current user's (DUZ) cost for all transcriptionists
        ;"Input: none.  User will be asked for start and end dates
        ;"Output: Produces a report to choses output channel.

        NEW Options

        WRITE !,"-- TRANSCRIPTION COST REPORT -- ",!!
        WRITE "Showing cost for: ",$PIECE($GET(^VA(200,DUZ,0)),"^",1),!!

        SET Options("AUTHOR")=DUZ
        DO AskDatesRPT(.Options)

        QUIT

RPTASKA
        ;"SCOPE: PUBLIC
        ;"Purpose: To report transcription costs for a chosen user
        ;"Input: none.  User will be asked for the user to report on, and also
        ;"        start and end dates
        ;"Output: Produces a report to choses output channel.

        NEW Options

        WRITE !,"-- TRANSCRIPTION COST REPORT -- ",!!

        SET DIC=200  ;"NEW PERSON file
        SET DIC(0)="MAQE"
        SET DIC("A")="Enter name of author (^ to abort): "
        DO ^DIC
        IF +Y=-1 DO  GOTO RAADone
        . WRITE !,"No author selected.  Aborting report.",!

        SET Options("AUTHOR")=+Y

        DO AskDatesRPT(.Options)
RAADone
        QUIT



AskDatesRPT(Options)
        ;"SCOPE: PUBLIC
        ;"Purpose: to finish the interactive report process.
        ;"Input: An array that should contain Options("TRANS")=IEN

        WRITE !!!
        WRITE "NOTE: Enter date range for note ENTRY into system, not date of service.",!
        NEW %DT
        SET %DT="AEP"
        SET %DT("A")="Enter starting date (^ to abort): "
        DO ^%DT
        IF Y=-1 DO  GOTO ADRDone
        . WRITE "Invalid date.  Aborting report.",!
        SET Options("START")=Y

        SET %DT("A")="Enter ending date (^ to abort): "
        DO ^%DT
        IF Y=-1 DO  GOTO ADRDone
        . WRITE "Invalid date.  Aborting report.",!
        SET Options("END")=Y

        NEW YN
        read !,"Show Details? YES// ",YN:$GET(DTIME,3600)
        IF YN="" SET YN="Y"
        SET Options("DETAILS")=($$UP^XLFSTR(YN)["Y")
        IF YN="^" WRITE "Aborting.",! GOTO ADRDone

        SET %ZIS("A")="Enter output printer or device (^ to abort): "
        DO ^%ZIS
        IF POP DO  GOTO ADRDone
        . WRITE !,"Error selecting output printer or device. Aborting report.",!

        use IO
        DO RPTQUIET(.Options)
        use IO(0)

        DO ^%ZISC

ADRDone
        QUIT


RPTQUIET(OPTIONS)
        ;"SCOPE: PUBLIC
        ;"Purpose: To create a report on transcription productivity based on
        ;"        options specified in OPTIONS.
        ;"Input: The following elements in OPTIONS should be defined
        ;"        0PTIONS("TRANS")  ;"the IEN of the transcriptionst (IEN from file 200)
        ;"                This term is to limit the search.  If all transcriptionsts are
        ;"                        wanted, then don't define OPTIONS("TRANS")
        ;"                If multiple transcriptionists need to be specified, use this format:
        ;"                        OPTIONS("TRANS")="*"
        ;"                        OPTIONS("TRANS",1)=IEN#1
        ;"                        OPTIONS("TRANS",2)=IEN#2
        ;"                        OPTIONS("TRANS",3)=IEN#3
        ;"        0PTIONS("AUTHOR")  ;"the IEN of the author (IEN from file 200)
        ;"                This term is to limit the search.  If all authors are
        ;"                        wanted, then don't define OPTIONS("AUTHOR")
        ;"                If multiple authors need to be specified, use this format:
        ;"                        OPTIONS("AUTHOR")="*"
        ;"                        OPTIONS("AUTHOR",1)=IEN#1
        ;"                        OPTIONS("AUTHOR",2)=IEN#2
        ;"                        OPTIONS("AUTHOR",3)=IEN#3
        ;"        OPTIONS("START") ;"Earliest date of documents, in Fileman internal format
        ;"        OPTIONS("END")   ;"Latest date of documents, in Fileman internal format
        ;"        OPTIONS("DETAILS") ;"if 1, then each document showed
        ;"Note: This will create a report by writing to the current device
        ;"        If the user wants output to go to a DEVICE, then they should call
        ;"        ^%ZIS prior to calling this function, and call ^%ZISC aftewards to close

        IF $GET(TMGDEBUG)>0 DO DebugEntry^TMGDEBUG(.DBIndent,"RPTQUIET^TMGTRANS1")

        NEW index
        NEW TransIEN,AuthorIEN
        NEW TransArrayP SET TransArrayP="OPTIONS(""TRANS"")"
        NEW AuthorArrayP SET AuthorArrayP="OPTIONS(""AUTHOR"")"
        NEW ChrCt SET ChrCt=0
        NEW LineCt SET LineCt=0
        NEW StartDT,EndDT
        NEW CtAuthor  ;"An array to subdivide lines to each doctor's account
        NEW CtTrans   ;"An array to track transcriptionists lines and income
        NEW AuthorInitials,TransInitials
        NEW ShowDetails SET ShowDetails=+$GET(OPTIONS("DETAILS"))

        SET StartDT=+$GET(OPTIONS("START"))
        IF (StartDT=0) DO  GOTO RQDone
        . WRITE "No start date specified. Aborting.",!
        SET EndDT=+$GET(OPTIONS("END"))\1  ;"  \1 removes time from date
        IF (EndDT=0) DO  GOTO RQDone
        . WRITE "No end date specified. Aborting.",!

        NEW CharsPerLine SET CharsPerLine=+$PIECE($GET(^TIU(8925.99,1,0)),"^",3)
        IF CharsPerLine=0 SET CharsPerLine=65

        WRITE !!,"   Visit;"
        WRITE $$RJ^XLFSTR("Entry Date;",15)
        WRITE $$RJ^XLFSTR("Lines@Rate=$Cost",23),"; "
        WRITE "Trn; Ath; Sgn; Patient",!
        WRITE "------------------------------------------------------------------------------",!
        SET index=$ORDER(^TIU(8925,0))
        FOR  DO  QUIT:(index="")
        . ;"WRITE "."
        . IF index="" QUIT
        . NEW k
        . use IO(0) read *k:0 use IO
        . IF k=27 DO  QUIT
        . . SET index=""
        . . WRITE "Report aborted by ESC from user.",!
        . NEW tDate SET tDate=$PIECE($GET(^TIU(8925,index,12)),"^",1)
        . SET tDate=tDate\1  ;"remove time from date
        . ;"set mC=mC+1 SET tC=tC+1 IF tC>100 WRITE mC," " SET tC=0
        . IF (tDate'<StartDT)&(tDate'>EndDT) do
        . . SET TransIEN=+$PIECE($GET(^TIU(8925,index,13)),"^",2)  ;"field 1302
        . . ;"WRITE "index=",index," "
        . . ;"WRITE "TransIEN='"
        . . ;"WRITE TransIEN,"'"
        . . IF ($DATA(OPTIONS("TRANS"))=0)!($$InList^TMGMISC(TransIEN,TransArrayP)=1) do
        . . . SET AuthorIEN=$PIECE($GET(^TIU(8925,index,12)),"^",2) ;field 1202
        . . . IF ($DATA(OPTIONS("AUTHOR"))=0)!($$InList^TMGMISC(AuthorIEN,AuthorArrayP)=1) do
        . . . . NEW tCharCt,tLineCt,Date,DateS,Pt
        . . . . NEW VDate,VDateSi
        . . . . NEW pStatus
        . . . . NEW Status SET Status="N"
        . . . . NEW Patient SET Patient=""
        . . . . SET tCharCt=+$PIECE($GET(^TIU(8925,index,"TMG")),"^",2);"field 22711=char count
        . . . . SET tLineCt=+$PIECE($GET(^TIU(8925,index,0)),"^",10)   ;"field .1 = line count
        . . . . SET pStatus=$PIECE($GET(^TIU(8925,index,0)),"^",5)     ;"field .05 is status file pointer
        . . . . IF +pStatus'=0 do
        . . . . . SET Status=$PIECE($GET(^TIU(8925.6,pStatus,0)),"^",2) ;"8925.6=TIU Status. field .02=symbol
        . . . . . IF Status="c" SET Status="Y"
        . . . . . ELSE  SET Status="N"
        . . . . IF (tLineCt=0)!(tCharCt=0) do
        . . . . . IF (tLineCt=0)&(tCharCt'=0) do
        . . . . . . SET tLineCt=(((tCharCt/CharsPerLine)*10)\1)/10
        . . . . . ELSE  IF (tCharCt=0)&(tLineCt'=0) do
        . . . . . . SET tCharCt=tLineCt*CharsPerLine
        . . . . . ELSE  do
        . . . . . . SET tLineCt=$$DocLines^TMGMISC(index,.tCharCt)
        . . . . . . IF tLineCt=0 SET tLineCt=(((tCharCt/CharsPerLine)*10)\1)/10
        . . . . . SET tLineCt=$$Round^TMGMISC(tLineCt)
        . . . . . SET tCharCt=$$Round^TMGMISC(tCharCt)
        . . . . . ;"Store values, so next time we won't have to calculate it.
        . . . . . SET $PIECE(^TIU(8925,index,0),"^",10)=+tLineCt   ;"field .1  = line count
        . . . . . SET $PIECE(^TIU(8925,index,"TMG"),"^",2)=tCharCt ;"field 22711 = char count
        . . . . SET Date=$PIECE($GET(^TIU(8925,index,12)),"^",1)   ;"field 1201 = Entry Date
        . . . . ;"set DateS=$$FMTE^XLFDT(Date,"D")
        . . . . SET DateS=$$DTFormat^TMGMISC(Date,"ww mm/dd/yy")
        . . . . SET VDate=$PIECE($GET(^TIU(8925,index,13)),"^",1)  ;"field 1301=Ref/Visit Date
        . . . . ;"set VDateS=$$FMTE^XLFDT(VDate,"D")
        . . . . SET VDateS=$$DTFormat^TMGMISC(VDate,"mm/dd/yy")
        . . . . SET AuthorInitials=$PIECE($GET(^VA(200,AuthorIEN,0)),"^",2)
        . . . . SET TransInitials=$PIECE($GET(^VA(200,TransIEN,0)),"^",2)  ;"field 1 = initials
        . . . . SET CtAuthor(AuthorIEN,"LINES")=$GET(CtAuthor(AuthorIEN,"LINES"))+tLineCt
        . . . . SET CtAuthor(AuthorIEN,"NOTES")=+$GET(CtAuthor(AuthorIEN,"NOTES"))+1
        . . . . SET CtTrans(TransIEN,"LINES")=$GET(CtTrans(TransIEN,"LINES"))+tLineCt
        . . . . SET CtTrans(TransIEN,"NOTES")=+$GET(CtTrans(TransIEN,"NOTES"))+1
        . . . . SET Pt=+$PIECE($GET(^TIU(8925,index,0)),"^",2)      ;"field .02 = patient
        . . . . IF Pt'=0 SET Patient=$PIECE($GET(^DPT(Pt,0)),"^",1) ;"field .01 = name
        . . . . NEW NoteBonus SET NoteBonus=0
        . . . . NEW PayRate SET PayRate=$$PayRate(TransIEN,Date,.NoteBonus)
        . . . . ;"new LineCost SET LineCost=$$RoundDn^TMGMISC(tLineCt*PayRate)
        . . . . ;"new LineCost SET LineCost=(tLineCt*PayRate)
        . . . . NEW LineCost SET LineCost=(tLineCt*PayRate)+NoteBonus
        . . . . SET CtAuthor(AuthorIEN,"COST")=+$GET(CtAuthor(AuthorIEN,"COST"))+LineCost
        . . . . SET CtAuthor(AuthorIEN,"BONUS")=+$GET(CtAuthor(AuthorIEN,"BONUS"))+NoteBonus
        . . . . SET CtTrans(TransIEN,"COST")=+$GET(CtTrans(TransIEN,"COST"))+LineCost
        . . . . SET CtTrans(TransIEN,"BONUS")=+$GET(CtTrans(TransIEN,"BONUS"))+NoteBonus
        . . . . IF ShowDetails do
        . . . . . WRITE VDateS,"; "
        . . . . . WRITE $$RJ^XLFSTR(DateS,13),";"
        . . . . . NEW tS SET tS=tLineCt_" @"_PayRate
        . . . . . IF NoteBonus>0 SET tS=tS_")+"_NoteBonus
        . . . . . WRITE $$RJ^XLFSTR(.tS,15)
        . . . . . SET tS=" =$"_LineCost_"; "
        . . . . . WRITE $$RJ^XLFSTR(.tS,10)
        . . . . . WRITE TransInitials,"; ",AuthorInitials,"; "
        . . . . . WRITE "  ",Status,"; "
        . . . . . WRITE $$Clip^TMGSTUTL(Patient,15),!
        . . . . SET LineCt=LineCt+tLineCt
        . SET index=+$ORDER(^TIU(8925,index))
        . IF index=0 SET index=""

        WRITE !,"Transcriptionist breakdown",!
        WRITE "-----------------------------",!
        SET index=$ORDER(CtTrans(""))
        FOR  DO  QUIT:(index="")
        . NEW TransS,Lines,Notes
        . IF index="" QUIT
        . SET TransS=$PIECE($GET(^VA(200,index,0)),"^",1)
        . IF TransS="" SET TransS="(Unknown Transcriptionist)"
        . SET Lines=+$GET(CtTrans(index,"LINES"))
        . SET Notes=+$GET(CtTrans(index,"NOTES"))
        . WRITE "  ",TransS,": ",Lines," lines in ",Notes," notes."
        . WRITE "  $",$GET(CtTrans(index,"COST"))
        . WRITE " (income)",!
        . IF +$GET(CtTrans(index,"BONUS"))>0 do
        . . NEW c SET c=+$GET(CtTrans(index,"COST"))
        . . NEW b SET b=$GET(CtTrans(index,"BONUS"))
        . . WRITE ?15,"$",c," = $",(c-b)," + $",b," per-note bonus.",!
        . SET index=$ORDER(CtTrans(index))

        WRITE !,"Author breakdown",!
        WRITE "--------------------",!
        SET index=$ORDER(CtAuthor(""))
        FOR  DO  QUIT:(index="")
        . NEW AuthorS,Lines,Notes
        . IF index="" QUIT
        . SET AuthorS=$PIECE($GET(^VA(200,index,0)),"^",1)
        . IF AuthorS="" SET AuthorS="(Unknown Author)"
        . SET Lines=+$GET(CtAuthor(index,"LINES"))
        . SET Notes=+$GET(CtAuthor(index,"NOTES"))
        . WRITE "  ",AuthorS,": ",Lines," lines in ",Notes," notes."
        . WRITE "  $",$GET(CtAuthor(index,"COST"))," (expense)",!
        . IF +$GET(CtAuthor(index,"BONUS"))>0 do
        . . NEW c SET c=+$GET(CtAuthor(index,"COST"))
        . . NEW b SET b=$GET(CtAuthor(index,"BONUS"))
        . . WRITE ?15,"$",c," = $",(c-b)," + $",b," per-note bonus.",!
        . SET index=$ORDER(CtAuthor(index))

        WRITE !!,"Done.",!

RQDone
        IF $GET(TMGDEBUG)>0 DO DebugExit^TMGDEBUG(.DBIndent,"RPTQUIET^TMGTRANS1")
        QUIT


PayRateE(TransIEN,Date)
        ;"Purpose: To provide a 'shell' for PayRate below, except external
        ;"        format of date alowed

        NEW IDate

        SET X=$GET(Date)
        ;"set IDate=

        ;"COMPLETE FUNCTION LATER...

        QUIT


PayRate(TransIEN,Date,NoteBonus)
        ;"Purpose: Get payrate in effect at time of Date
        ;"Input: TransIEN -- the record number in file 200
        ;"         Date: reference date to lookup, ** in internal fileman format **
        ;"         NoteBonus -- [OPTIONAL] This is an out parameter.  See below.
        ;"Result: The payrate found in file TMG TRANSCRIPTION PAYRATE file
        ;"                This is dollars/line
        ;"        If NoteBonus was passed by reference, then the value of the
        ;"                NOTE BONUS field (field #3) is returned, or 0 IF not found.
        ;"        Note: a result of 0 is returned IF TransIEN not found, or
        ;"                no date range covers Date

        NEW result SET result=0
        NEW bonusresult SET bonusresult=0
        NEW RateIEN
        NEW index

        IF (+$GET(TransIEN)=0)!(+$GET(Date)=0) GOTO PRDone
        SET Date=Date\1
        SET RateIEN=+$ORDER(^TMG(22704,"B",TransIEN,""))
        IF RateIEN=0 GOTO PRDone
        MERGE PayRates=^TMG(22704,RateIEN,1)
        SET index=$ORDER(^TMG(22704,RateIEN,1,0))
        FOR  DO  QUIT:(index="")
        . IF index="" QUIT
        . NEW Rate SET Rate=$GET(^TMG(22704,RateIEN,1,index,0))
        . IF Rate'="" do
        . . NEW StartDate,EndDate
        . . SET StartDate=$PIECE(Rate,"^",2)
        . . SET EndDate=$PIECE(Rate,"^",3)
        . . IF Date<StartDate DO  QUIT
        . . . ;"WRITE "Date=",Date," StartDate=",StartDate,!
        . . IF (EndDate'="")&(Date>EndDate) DO  QUIT
        . . . ;"WRITE "Date=",Date," EndDate=",EndDate,!
        . . SET result=$PIECE(Rate,"^",1)
        . . SET bonusresult=$PIECE(Rate,"^",4)  ;"field#3 (NOTE BONUS)
        . IF result'=0 SET index="" QUIT
        . SET index=$ORDER(^TMG(22704,RateIEN,1,index))

        IF result=0 do
         . ;"WRITE !,"TransIEN=",TransIEN," Date=",Date,!
PRDone
        SET NoteBonus=bonusresult
        QUIT result

        ;"=======================================================================

FREECUR
        ;"Purpose: For current user, cycle through all alerts regarding
        ;"        documents needing to be signed, and automatically sign
        ;"        them, then print IF user wants.
        ;"Input: none.  User will be asked for signature password,
        ;"        and IF they want documents printed.
        ;"Output: Produces a report to chosen output channel.

        ;"WRITE @IOF
        WRITE !!,"-- RELEASE UNSIGNED DOCUMENTS -- ",!!
        WRITE "Releasing transcription for: ",$PIECE($GET(^VA(200,DUZ,0)),"^",1),!!

        DO FreeDocs(DUZ,1)

        WRITE !,"Goodbye.",!

        QUIT


FREEASK
        ;"Purpose: Ask for chosen user, then cycle through all alerts
        ;"        regarding documents needing to be signed, and automatically
        ;"        sign them, then print IF user wants.
        ;"Input: none.  User will be asked for signature password,
        ;"        and IF they want documents printed.
        ;"Output: Produces a report to choses output channel.

        NEW Y,DIC,TransIEN,DocIEN
        SET TransIEN=-1

        ;"WRITE @IOF
        WRITE !!,"-- RELEASE UNSIGNED DOCUMENTS -- ",!!

        SET DIC=200  ;"NEW PERSON file
        SET DIC(0)="MAQE"
        SET DIC("A")="Enter name of author (^ to abort): "
        DO ^DIC
        IF +Y'>0 DO  GOTO RADone
        . WRITE !,"No author selected.  Aborting report.",!
        SET DocIEN=+Y

        WRITE !!,"OPTIONAL-- Enter name of transcriptionist to screen for.  If specified, ",!
        WRITE "only notes entered by this transcriptionist will be signed and released."
        SET DIC("A")="Enter name of transcriptionist (ENTER or ^ to skip): "
        DO ^DIC
        WRITE !!
        IF +Y'>0 SET TransIEN=+Y

        DO FreeDocs(DocIEN,1,TransIEN)

        WRITE !,"Goodbye.",!

FADone
        QUIT


FreeDocs(AuthorIEN,ShowDetails,TransIEN)
        ;"Purpose: to finish the interactive release documents process.
        ;"        This separate entry point allows restriction of the author
        ;"        whose's documents are to be released.
        ;"Input: AuthorIEN, the record number of the author in file 200
        ;"        ShowDetails: optional.  Default is to show details (1)
        ;"                0=don't show, 1=show
        ;"       TransIEN:  OPTIONAL -- the IEN of the transcriptionist.
        ;"              IF specified, then ONLY those notes created by this
        ;"              transcriptionist will be finished/released

        NEW Signed
        NEW abort SET abort=0
        NEW Options
        NEW PrintAfter
        NEW YN
        NEW SignAll

        SET Options("AUTHOR")=+$GET(AuthorIEN)
        SET Options("SIG")=0
        SET Options("DETAILS")=$GET(ShowDetails,1)
        IF +$GET(TransIEN)>0 SET Options("TRANS")=+TransIEN

        do
        . WRITE "Enter 'your' (meaning author's) signature code below."
        . NEW DUZ
        . SET DUZ=+$GET(AuthorIEN)
        . IF DUZ=0 QUIT
        . DO SIG^XUSESIG
        . WRITE !
        . IF X1'="" SET Options("SIG")=1
        IF Options("SIG")'=1 DO  GOTO FADDone
        . WRITE "Signature code incorrect. Aborting.",!

        read "Sign all notes at once (^/Y/N):  YES// ",SignAll:$GET(DTIME,3600),!
        IF SignAll="" SET SignAll="Y"
        IF SignAll="^" WRITE "Aborting.",! GOTO ADRDone
        SET Options("SIGN ALL")=($$UP^XLFSTR(SignAll)["Y")

        WRITE !,"Print Notes after signing? (^/Y/N):  YES// "
        read YN:$GET(DTIME,3600),!
        IF YN="^" WRITE "Aborting.",! GOTO ADRDone
        IF YN="" SET YN="Y"
        SET PrintAfter=($$UP^XLFSTR(YN)["Y")

        DO AlertSign(.Options,.Signed)

        WRITE "Now look at ALL documents to find any unsigned ones.",!
        SET Options("START")="0001111"
        DO NOW^%DTC
        SET Options("END")=X
        DO ScanSign(.Options,.Signed)

        MERGE ^TMG("BATCH SIGNED DOCS",$J)=Signed

        IF PrintAfter DO PRINT(.Signed)

FADDone
        QUIT


ScanSign(OPTIONS,SIGNED)
        ;"Purpose: To scan through all TIU DOCUMENTS, and release those
        ;"           that have a status of unsigned to completed
        ;"Input: The following elements in OPTIONS should be defined
        ;"        0PTIONS("AUTHOR")  ;"the IEN of the user (IEN from file 200)
        ;"        OPTIONS("START")   ;"Earliest date of documents, in Fileman internal format
        ;"                                      ;"Note IF not specified, then all dates are matched
        ;"        OPTIONS("END")     ;"Latest date of documents, in Fileman internal format
        ;"                                      ;"Note IF not specified, then all dates are matched
        ;"        OPTIONS("DETAILS") ;"if 1, then each document is shown as signed (not quiet)
        ;"        OPTIONS("SIG")     ;"1 IF signature has been verified.
        ;"        -----------Optional OPTIONS below---------------
        ;"        OPTIONS("TRANS")   ;"the IEN of note.  If specified, then note will not be signed
        ;"                           ;"unless the transcriptionist (i.e. ENTERED BY field) = this IEN
        ;"        -------------------------------------------------------
        ;"        SIGNED: OPTIONAL. This is an OUT PARAMETER -- must be passed by reference
        ;"                This will contain list of documents freed/signed, in this format:
        ;"                SIGNED(1234)=1234  with 1234 being IEN of document signed.
        ;"                SIGNED(1235)=1235  with 1235 being IEN of document signed.
        ;"                SIGNED(1236)=1236  with 1235 being IEN of document signed.

        NEW index
        NEW DocAuth,Status,EnteredBy
        NEW User,initials
        NEW NeedsCR SET NeedsCR=1
        NEW StartDT,EndDT
        NEW ShowDetails SET ShowDetails=+$GET(OPTIONS("DETAILS"))

        IF +$GET(OPTIONS("START"))=0 do
        . NEW %DT
        . SET %DT="AEP"
        . SET %DT("A")="Enter starting date (^ to abort): "
        . DO ^%DT
        . SET OPTIONS("START")=Y
        IF $GET(OPTIONS("START"))'>0 DO  GOTO SSDone
        . IF ShowDetails WRITE "START date invalid.  Aborting.",!

        IF +$GET(OPTIONS("END"))=0 do
        . SET %DT("A")="Enter ending date (^ to abort): "
        . DO ^%DT
        . SET OPTIONS("END")=Y
         IF $GET(OPTIONS("END"))'>0 DO  GOTO SSDone
        . IF ShowDetails WRITE "END date invalid.  Aborting.",!

        SET User=+$GET(OPTIONS("AUTHOR"))
        IF User=0 DO  GOTO RQDone
        . IF $GET(OPTIONS("DETAILS")) WRITE "No author IEN supplied. Aborting.",!
        SET StartDT=+$GET(OPTIONS("START"))
        SET EndDT=+$GET(OPTIONS("END"))

        IF $GET(OPTIONS("DETAILS")) do
        . WRITE !,"------------------------------------------------",!
        . WRITE "Starting scan of all documents. [ESC] will abort.",!
        . WRITE "------------------------------------------------",!

        SET initials=$PIECE($GET(^VA(200,User,0)),"^",2)   ;"field 1 = initials
        NEW sUnsigned SET sUnsigned=$ORDER(^TIU(8925.6,"B","UNSIGNED",""))
        NEW sUnverified SET sUnverified=$ORDER(^TIU(8925.6,"B","UNVERIFIED",""))

        SET index=$ORDER(^TIU(8925,0))
        FOR  DO  QUIT:(index="")
        . IF index="" QUIT
        . NEW k read *k:0
        . IF k=27 DO  QUIT
        . . SET index=""
        . . IF $GET(OPTIONS("DETAILS")) WRITE "Release aborted by ESC from user.",!
        . SET DocAuth=$PIECE($GET(^TIU(8925,index,12)),"^",2)  ;"field 1202 = Author
        . SET EnteredBy=$PIECE($GET(^TIU(8925,index,13)),"^",2)  ;"field 1302 = Entered By
        . IF (DocAuth=$GET(OPTIONS("AUTHOR"))) do
        . . IF $DATA(OPTIONS("TRANS"))&($GET(OPTIONS("TRANS"))'=EnteredBy) QUIT
        . . SET Status=$PIECE($GET(^TIU(8925,index,0)),"^",5)  ;"field .05 = Status
        . . IF (Status=sUnsigned)!(Status=sUnverified) DO   ;"*** What ELSE should go here?!!
        . . . NEW tDate
        . . . SET tDate=$PIECE($GET(^TIU(8925,index,12)),"^",1)
        . . . SET tDate=tDate\1  ;"integer round down (removes time decimal amount)
        . . . IF (StartDT=0)!(EndDT=0)!((tDate'<StartDT)&(tDate'>EndDT)) do
        . . . . IF $$SIGNDOC(index,.OPTIONS) do
        . . . . . SET SIGNED(index)=index
        . SET index=+$ORDER(^TIU(8925,index))
        . IF index=0 SET index=""

SSDone
        IF $GET(OPTIONS("DETAILS")) WRITE !,"Done scanning all documents.",!

        QUIT


AlertSign(OPTIONS,SIGNED)
        ;"Purpose: To cycle through all alerts for AUTHOR, and release TIU DOCUMENTS
        ;"          needing signature.
        ;"Input: The following elements in OPTIONS should be defined
        ;"        0PTIONS("AUTHOR")  ;"the IEN of the user (IEN from file 200)
        ;"        OPTIONS("DETAILS") ;"if 1, then each document is shown as signed (not quiet)
        ;"        OPTIONS("SIG")     ;"1 IF signature has been verified.
        ;"        OPTIONS("SIGN ALL");"if 1, then all are signed without asking each one.
        ;"        SIGNED: OPTIONAL. This is an OUT PARAMETER -- must be passed by reference
        ;"                This will contain list of documents freed/signed, in this format:
        ;"                SIGNED(1234)=1234  with 1234 being IEN of document signed.
        ;"                SIGNED(1235)=1235  with 1235 being IEN of document signed.
        ;"                SIGNED(1236)=1236  with 1235 being IEN of document signed.

        NEW index
        NEW Abort SET Abort=0
        NEW Alert
        NEW DocIEN
        NEW NumFound SET NumFound=0
        NEW SignAll SET SignAll=+$GET(OPTIONS("SIGN ALL"))

        SET User=+$GET(OPTIONS("AUTHOR"))
        IF User=0 DO  GOTO RQDone
        . IF $GET(OPTIONS("DETAILS")) WRITE "No author IEN supplied. Aborting.",!

         IF $GET(OPTIONS("DETAILS")) do
        . WRITE !,"-------------------------------------------------------",!
        . WRITE "Search for 'signature-needed' alerts. [ESC] will abort.",!
        . WRITE "-------------------------------------------------------",!

        IF SignAll'=1 DO  IF NumFound=0 GOTO ASgn2
        . WRITE !!,"-------- List of Documents to be Signed --------",!
        . SET index=$ORDER(^XTV(8992,User,"XQA",0))
        . FOR  DO  QUIT:(index="")
        . . IF index="" QUIT
        . . NEW k read *k:0
        . . IF k=27 DO  QUIT
        . . . SET index=""
        . . . IF $GET(OPTIONS("DETAILS")) WRITE "List aborted by ESC from user.",!
        . . SET Alert=$GET(^XTV(8992,User,"XQA",index,0))
        . . IF $PIECE(Alert,"^",3)["available for SIGNATURE" do
        . . . WRITE $PIECE(Alert,"^",3),!
        . . . SET NumFound=NumFound+1
        . . SET index=$ORDER(^XTV(8992,User,"XQA",index))
        . WRITE "-----------------------------------------------",!
        . WRITE !,NumFound," documents needing signature.",!!
        . IF NumFound=0 DO  QUIT
        . . WRITE "No alerts for a missing signature found.!",!

        ;"WRITE "STARTING SIGN LOOP",!
        SET NumFound=0
        SET index=$ORDER(^XTV(8992,User,"XQA",0))
        FOR  DO  QUIT:(index="")!(Abort=1)
        . NEW Title,YN
        . IF index="" QUIT
        . SET Alert=$GET(^XTV(8992,User,"XQA",index,0))
        . SET Title=$PIECE(Alert,"^",3)
        . IF Title["available for SIGNATURE" do
        . . SET NumFound=NumFound+1
        . . IF SignAll'=1 do
        . . . WRITE "Sign: ",$PIECE(Title," ",1),"? (Y/N/ALL): ALL// "
        . . . read YN:$GET(DTIME,3600),!
        . . . SET YN=$$UP^XLFSTR(YN)
        . . ELSE  SET YN="Y"
        . . IF YN="" SET YN="ALL" WRITE "ALL",!
        . . IF YN="ALL" SET SignAll=1 SET YN="Y"
        . . ELSE  IF YN["^" WRITE !,"Aborting.",! SET Abort=1 QUIT
        . . IF YN["Y" do
        . . . SET DocIEN=+$GET(^XTV(8992,User,"XQA",index,1))
        . . . IF DocIEN'=0 do
        . . . . IF $$SIGNDOC(DocIEN,.OPTIONS) do
        . . . . . SET SIGNED(DocIEN)=DocIEN
        . SET index=$ORDER(^XTV(8992,User,"XQA",index))

        IF $GET(OPTIONS("DETAILS")) do
        . WRITE !!,"Done searching for 'needed-signature' alerts.",!

ASgn2
        IF (1=0) DO   ;"if (NumFound=0) do
        . IF $GET(OPTIONS("DETAILS")) do
        . . WRITE "No alert indicating a signature is needed was found....",!
        . . WRITE "...So starting a scan of all documents to look for unsigned documents.",!
        . SET OPTIONS("START")="0001111"
        . DO NOW^%DTC
        . SET OPTIONS("END")=X
        . DO ScanSign(.OPTIONS,.Signed)

ASgnDone
        QUIT


SIGNDOC(DocIEN,OPTIONS)
        ;"Purpose: To sign one document
        ;"Input: DocIEN -- the record number of the document to sign
        ;"        OPTIONS -- An array with input values.  The following are used:
        ;"        0PTIONS("AUTHOR")  ;"the IEN of the user (IEN from file 200)
        ;"        OPTIONS("DETAILS") ;"if 1, then each document showed
        ;"        OPTIONS("SIG")     ;"1 IF signature has been verified.
        ;"Results: 1 = successful sign.  0 = failure

        NEW result SET result=0 ;"default to failure
        NEW Node0
        NEW sCompleted SET sCompleted=$ORDER(^TIU(8925.6,"B","COMPLETED",""))
        NEW NewStatus
        IF $GET(OPTIONS("SIG"))'=1 GOTO SDCDone
        IF +$GET(OPTIONS("AUTHOR"))'>0 GOTO SDCDone
        IF $GET(DocIEN)="" GOTO SDCDone

        NEW SignerS
        SET SignerS=1_"^"_$PIECE($GET(^VA(200,+OPTIONS("AUTHOR"),20)),"^",2,3)
        IF $DATA(^TIU(8925,DocIEN,0))=0 DO  GOTO SDCDone
        . WRITE "Unable to sign document #",DocIEN," because it doesn't seem to exist.",!
        DO ES^TIURS(DocIEN,SignerS)
        ;"Note: alert(s) r.e. "Note available for signature" are automatically removed

SDLoop
        SET Node0=$GET(^TIU(8925,DocIEN,0))
        SET NewStatus=$PIECE(Node0,"^",5)        ;"field .05 = Status

        NEW Date,DateS,Pt
        SET Date=$PIECE(Node0,"^",7)        ;"field .07 = Episode begin date/time
        SET DateS=$$FMTE^XLFDT(Date,"D")
        SET Pt=+$PIECE(Node0,"^",2)          ;"field .02 = patient
        IF Pt'=0 SET Patient=$PIECE($GET(^DPT(Pt,0)),"^",1)     ;"field .01 = name
        IF OPTIONS("DETAILS")=1 do
        . WRITE DateS," -- ",Patient

        IF NewStatus'=sCompleted DO  GOTO SDLoop
        . IF OPTIONS("DETAILS")=1 do
        . . NEW s
        . . SET s=$PIECE($GET(^TIU(8925.6,NewStatus,0)),"^",1)
        . . WRITE " NOT completed.  Status=",s
        . . WRITE !,"  TRYING AGAIN. (utilizing a lower-level signature method.)",!
        . . SET $PIECE(^TIU(8925,DocIEN,0),"^",5)=sCompleted

        IF OPTIONS("DETAILS")=1 do
        . WRITE " Released (auto-'signed')",!

        SET result=1  ;"success

SDCDone
        QUIT result


PRINT(DocArray) ; Prompt and print, or array
        ;"This function was copied from PRINT^TIUEPRNT, to allow modification
        ;"Function modification: changed to allow array input.
        ;"        DocArray:  This will contain list of documents to print, in this format:
        ;"                DocArray(1234)=1234  with 1234 being IEN of document to be printed.
        ;"                DocArray(1235)=1235  with 1235 being IEN of document to be printed.
        ;"                DocArray(1236)=1236  with 1235 being IEN of document to be printed.
        ;"              Note: Is appears that DocArray(IEN)="" is the needed format.

        New TIUDEV,TIUTYP,TMGDFN,TIUPMTHD,TIUD0,TIUMSG,TIUPR,TIUDARR,TIUDPRM
        NEW TIUFLAG SET TIUFLAG="x"
        New TIUPGRP,TIUPFHDR,TIUPFNBR

        NEW index SET index=$ORDER(DocArray(""))
        IF index="" GOTO PRINT1X
        FOR  DO  QUIT:(index="")
        . SET DocIEN=index
        . ;
        . If +$$ISADDNDM^TIULC1(DocIEN) Set DocIEN=$Piece($Get(^TIU(8925,+DocIEN,0)),U,6)
        . If $Get(^TIU(8925,DocIEN,21)) Set DocIEN=^TIU(8925,DocIEN,21)
        . Set TIUD0=$Get(^TIU(8925,DocIEN,0))
        . Set TIUTYP=$Piece(TIUD0,U)
        . Set TMGDFN=$Piece(TIUD0,U,2)
        . If +TIUTYP'>0 Quit
        . ;
        . Set TIUPMTHD=$$PRNTMTHD^TIULG(+TIUTYP)
        . Set TIUPGRP=$$PRNTGRP^TIULG(+TIUTYP)
        . Set TIUPFHDR=$$PRNTHDR^TIULG(+TIUTYP)
        . Set TIUPFNBR=$$PRNTNBR^TIULG(+TIUTYP)
        . ;
        . Do DOCPRM^TIULC1(+TIUTYP,.TIUDPRM,DocIEN)
        . ;
        . If +$Piece($Get(TIUDPRM(0)),U,9) do
        . . IF TIUFLAG="x" Set TIUFLAG=$$FLAG^TIUPRPN3 ;"Asks Chart vs. Work Copy? only ONCE
        . If ($Get(TIUPMTHD)]"")&(+$Get(TIUPGRP))&($Get(TIUPFHDR)]"")&($Get(TIUPFNBR)]"") do
        . . Set TIUDARR(TIUPMTHD,$Get(TIUPGRP)_"$"_TIUPFHDR_";"_TMGDFN,1,DocIEN)=TIUPFNBR
        . Else  Set TIUDARR(TIUPMTHD,TMGDFN,1,DocIEN)=""
        . ;
        . If $Get(TIUPMTHD)']"" DO  ;"Goto PRINT1X
        . . IF OPTIONS("DETAILS")=1 do
        . . . Write !,$Char(7),"No Print Method Defined for "
        . . . WRITE $Piece($Get(^TIU(8925.1,+TIUTYP,0)),U)
        . . ;"Hang 2
        . ;
        . SET index=$ORDER(DocArray(index))

        Set TIUDEV=$$DEVICE^TIUDEV(.IO) ; Get Device/allow queueing
        If ($Get(IO)']"")!(TIUDEV']"") Do ^%ZISC Quit
        If $Data(IO("Q")) Do QUE^TIUDEV("PRINTQ^TIUEPRNT",TIUDEV) Goto PRINT1X
        Do PRINTQ^TIUEPRNT
        Do ^%ZISC

PRINT1X ; Exit single document print
        Quit


SHOWUNSIGNED
        ;"Purpose: to scan through all documents and show any that are unsigned

        NEW index
        NEW DocAuth,Status,Patient,PtName
        NEW TransIEN,TransInit
        NEW User,initials,AuthName
        NEW NeedsCR SET NeedsCR=1
        NEW StartDT,EndDT

        WRITE !,"----------------------------------------------",!
        WRITE "Starting scan of documents. [ESC] will abort.",!
        WRITE "----------------------------------------------",!

        NEW sUnsigned SET sUnsigned=$ORDER(^TIU(8925.6,"B","UNSIGNED",""))
        NEW sCompleted SET sCompleted=$ORDER(^TIU(8925.6,"B","COMPLETED",""))

        SET index=$ORDER(^TIU(8925,0))
        FOR  DO  QUIT:(index="")
        . IF index="" QUIT
        . NEW k read *k:0
        . IF k=27 DO  QUIT
        . . SET index=""
        . . IF $GET(OPTIONS("DETAILS")) WRITE "Scan aborted by ESC from user.",!
        . SET Status=$PIECE($GET(^TIU(8925,index,0)),"^",5)  ;"field .05 = Status
        . IF (Status'=sCompleted) do
        . . ;"WRITE !
        . . NEW tDate
        . . SET tDate=$PIECE($GET(^TIU(8925,index,12)),"^",1)
        . . SET DocAuth=$PIECE($GET(^TIU(8925,index,12)),"^",2)  ;"field 1202 = Author
        . . SET initials=$PIECE($GET(^VA(200,DocAuth,0)),"^",2)   ;"field .02 = initials
        . . SET AuthName=$PIECE($GET(^VA(200,DocAuth,0)),"^",1)   ;"field .01 = Name
        . . SET Patient=$PIECE($GET(^TIU(8925,index,0)),"^",2)  ;"field .02 = patient IEN
        . . SET TransIEN=$PIECE($GET(^TIU(8925,index,13)),"^",2) ;"field 1302 = Entered by IEN
        . . IF +TransIEN'=0 SET TransInit=$PIECE($GET(^VA(200,TransIEN,0)),"^",2) ;" field .02 = initials
        . . ELSE  SET TransInit="???"
        . . IF +Patient'=0 SET PtName=$PIECE($GET(^DPT(Patient,0)),"^",1)      ;"field .01 is patient name
        . . ELSE  SET Patient="Name Unknown(?)"
        . . SET DateS=$$DTFormat^TMGMISC(tDate,"ww mm/dd/yy")
        . . WRITE "NOT COMPLETED. "
        . . WRITE $$RJ^XLFSTR(AuthName_"; ",20)
        . . WRITE $$RJ^XLFSTR(DateS_"; ",15)
        . . WRITE $$RJ^XLFSTR(TransInit_"; ",5)
        . . WRITE $$Clip^TMGSTUTL(PtName,20),!
        . ;"else  WRITE "."
        . SET index=+$ORDER(^TIU(8925,index))
        . IF index=0 SET index=""

        WRITE !,"Done scanning documents.",!

        QUIT



PWDSNOOP(IEN)
        ;"Purpose: To show private info for a given user
        ;"NOTICE: This function MUST be used responsibly
        ;"Input: IEN -- [OPTIONAL] the record number of the user to snoop on

        WRITE !!,"------------------------------------------------------------------",!
        WRITE "Notice: This function will unmask private password codes.",!
        WRITE "These codes can be used spoof this EMR system.  Note",!
        WRITE "that impersonating another user can be a CRIME.",!,!

        IF $DATA(IEN) GOTO IS2

        SET DIC=200  ;"NEW PERSON file
        SET DIC(0)="MAQE"
        SET DIC("A")="Enter name of user to unmask codes for (^ to abort): "
        DO ^DIC
        IF +Y=-1 DO  GOTO ISPDone
        . WRITE !,"No user selected.  Aborting report.",!

        WRITE !,!
        SET IEN=+Y

IS2
        NEW VerHash,AccHash,ESig
        IF '$DATA(IEN) GOTO ISPDone

        SET VerHash=$PIECE($GET(^VA(200,IEN,.1)),"^",2)
        SET AccHash=$PIECE($GET(^VA(200,IEN,0)),"^",3)
        SET ESig=$PIECE($GET(^VA(200,IEN,20)),"^",4)

        WRITE "Access Code=",$$UNHASH^TMGMISC(AccHash),!
        WRITE "Verify Code=",$$UNHASH^TMGMISC(VerHash),!
        WRITE "Electronic Signature=",ESig,!!

        WRITE "Remember, you are morally, ethically, and LEGALLY required to use",!
        WRITE "this information only in an appropriate manner.",!
        WRITE "------------------------------------------------------------------",!
        DO PRESS2GO^TMGUSRI2
        WRITE "Goodbye.",!!

ISPDone
        QUIT





