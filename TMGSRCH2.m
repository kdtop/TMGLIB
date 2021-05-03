TMGSRCH2 ;TMG/kst/Search API ; 6/19/10, 2/2/14, 2/15/15, 3/24/21
        ;;1.0;TMG-LIB;**1**;06/19/10
        ;
 ;"TMG Search only in TIU documents for 1 patient
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
 ;" RPC -- Public Functions.
 ;"=======================================================================
 ;"LAUNCHSR(TMGDFN,TMGSRCH) --Launch background task to achieve search
 ;"STATUS() -- Get status of background searching task
 ;"RESULTS(OUT) -- get result from background search
 ;"CLEAR -- Tell background task to stop, and clear data array
 ;"STOP --Tell background task to stop searching
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"CHNGSRCH(BKJOB,TMGDFN,TMGSRCH) -- tell background task to change search parameters
 ;"MSG(BKJOB,MSG) -- Purpose to message background task
 ;"SRCHTIU(TMGDFN,TMGSRCH,PARENTJOB) -- search all of one patient's documents for requested words
 ;"PREPSRCH(PARENTJOB,TMGDFN,TMGSRCH,WORDS,IENLIST) -- Parse search phrase, user prior runs IF possible.
 ;"PARSSRCH(TMGSRCH,WORDS) -- Separate search phrase out into array of words
 ;"SRCH1TIU(PARENTJOB,IEN,TERM) -- Search TIU DOCUMENT report text for TERM
 ;"=======================================================================
 ;"=======================================================================
 ;"Dependencies:
 ;"=======================================================================
 ;"=======================================================================
 ;
LAUNCHSR(TMGDFN,TMGSRCH) ;
        ;"Purpose: Launch background task to achieve search
        ;"Input: TMGDFN -- The patient IEN to look up.
        ;"       TMGSRCH -- Search string.  Notes:
        ;"             Each word (or partial word) to look up is separated by spaces
        ;"             All words are combined in AND fashion
        ;"             Search is NOT case sensitive.
        ;"             Exact pharases can be specified by quotes.
        ;"             Example: 'dog cat monkey "in a barrel"
        NEW THISJOB SET THISJOB=$J
        NEW STATUS SET STATUS=$GET(^TMG("TMP","SEARCH","SRCHTIU",THISJOB,"MSG"))
        IF STATUS="BKGND RUNNING" DO
        . DO CHNGSRCH(TMGDFN,TMGSRCH)
        ELSE  DO
        . NEW DEBUG SET DEBUG=0
        . IF DEBUG=0 DO   ;"Can be changed when stepping through code.
        . . KILL ^TMG("TMP","SEARCH","SRCHTIU",THISJOB)
        . . JOB SRCHTIU(TMGDFN,TMGSRCH,THISJOB)
        . . SET ^TMG("TMP","SEARCH","SRCHTIU",THISJOB,"BACKGROUND")=$ZJOB
        . ELSE  DO
        . . DO SRCHTIU(TMGDFN,TMGSRCH,THISJOB)
        QUIT
        ;
        ;
STATUS() ;
        ;"Purpose: To check status of background searching task
        ;"Input: none
        ;"Output: returns 1^status string, or "1^" IF none
        NEW RESULT
        SET RESULT=$GET(^TMG("TMP","SEARCH","SRCHTIU",$J,"MSG"))
        IF +$PIECE(RESULT,"^",1)'=$PIECE(RESULT,"^",1) SET RESULT="1^"_RESULT
STATDN  QUIT RESULT
        ;
        ;
RESULTS(OUT) ;
        ;"Purpose: To get result from background search
        ;"Input: OUT -- PASS BY REFERENCE.  An OUT PARAMETER.  Format
        ;"              OUT(0)=FoundCount^Success, or -1^Message
        ;"              OUT(1)=IEN1
        ;"              OUT(2)=IEN2 ... etc.
        NEW STATUS SET STATUS=$$STATUS()
        IF +STATUS=-1 SET OUT(0)=STATUS GOTO RSLTDN
        ;"IF STATUS'="DONE" SET OUT(0)="-1^Search not completed"
        NEW IENLIST MERGE IENLIST=^TMG("TMP","SEARCH","SRCHTIU",$J,"IEN LIST")
        NEW CT SET CT=0
        NEW IEN SET IEN=0
        FOR  SET IEN=$ORDER(IENLIST(IEN)) QUIT:(+IEN'>0)  DO
        . SET CT=CT+1
        . SET OUT(CT)=IEN
        IF $DATA(OUT)=0 SET OUT(0)="-1^No results found"
        ELSE  SET OUT(0)=CT_"^Success"  ;"//kt was CT-1  10/13/13
RSLTDN  QUIT
        ;
        ;
CLEAR   ;"Purpose: Tell background task to stop, and clear data array
        DO STOP
        KILL ^TMG("TMP","SEARCH","SRCHTIU",$J)
        QUIT
        ;
        ;
STOP    ;"Purpose: Tell background task to stop searching
        DO MSG("STOP")
        QUIT
        ;
        ;
CHNGSRCH(TMGDFN,TMGSRCH) ;CHANGE SEARCH
        ;"Purpose: to tell background task to change search parameters
        DO MSG("RESTART^"_TMGDFN_"^"_TMGSRCH)
        QUIT
        ;
        ;
MSG(MSG) ;
        ;"Purpose to message background task
        SET ^TMG("TMP","SEARCH","SRCHTIU",$J,"MSG")=MSG
        QUIT
        ;
        ;
 ;"==========================================================================
SRCHTIU(TMGDFN,TMGSRCH,PARENTJOB) ;
        ;"Purpose: To search all of one patient's documents for requested words
        ;"Input: TMGDFN -- The patient IEN to look up.
        ;"       TMGSRCH -- Search string.  Notes:
        ;"             Each word (or partial word) to look up is separated by spaces
        ;"             All words are combined in AND fashion
        ;"             Search is NOT case sensitive.
        ;"             Exact pharases can be specified by quotes.
        ;"             Example: 'dog cat monkey "in a barrel"
        ;"       PARENTJOB -- the job of the RPCBroker task that called this.
        ;"NOTE:  this routine will monitor global for messages:
        ;"             ^TMG("TMP","SEARCH","SRCHTIU",PARENTJOB,"MSG")=message
        ;"             If message of "STOP" is found, then search will be stopped.
        ;"Output: Matching documents will be stored at:
        ;"             ^TMG("TMP","SEARCH","SRCHTIU",PARENTJOB,"IEN LIST",IEN)=""
        ;"             ^TMG("TMP","SEARCH","SRCHTIU",PARENTJOB,"IEN LIST",IEN)=""
        ;"             ^TMG("TMP","SEARCH","SRCHTIU",PARENTJOB,"DFN")=DFN
        ;"             ^TMG("TMP","SEARCH","SRCHTIU",PARENTJOB,"FILTER",FilterValue)=""
        ;"             ^TMG("TMP","SEARCH","SRCHTIU",PARENTJOB,"FILTER",FilterValue)=""
        ;"        When search is done, then message will be stored as
        ;"             ^TMG("TMP","SEARCH","SRCHTIU",PARENTJOB,"MSG")="DONE"
        ;"Results: none
        ;"NOTE: This function is designed so that it can DO searching on the fly,
        ;"      as the user is typing in additional terms.  Thus, IF a prior
        ;"      search is found, and the prior search doesn't contain any terms
        ;"      that are not in the current search, then this search cycle will
        ;"      start with the results of the prior search.
        ;"      --A consequence of this is that a search of all the documents
        ;"      will be done first for search term #1, and then term #2, RATHER
        ;"      THAN searching 1 document for all the search terms.  This should
        ;"      not cause too much of a performace hit because searches for other
        ;"      terms will be limited to matches for earlier terms
        ;
        NEW ABORT,IEN,IENLIST
        NEW DEBUGI SET DEBUGI=0
        SET TMGDFN=+$GET(TMGDFN)
        SET TMGSRCH=$GET(TMGSRCH)
        NEW REF SET REF=$NAME(^TMG("TMP","SEARCH","SRCHTIU",PARENTJOB))
L1      SET @REF@("MSG")="BKGND RUNNING"
        DO PREPSRCH(PARENTJOB,TMGDFN,TMGSRCH,.WORDS,.IENLIST)
        SET ABORT=0
        NEW TERMCT,TERM
        FOR TERMCT=1:1 SET TERM=$GET(WORDS(TERMCT)) QUIT:(TERM="")!ABORT  DO
        . ;"SET @REF@("DEBUG",DEBUGI)="TERM="_TERM,DEBUGI=DEBUGI+1
        . KILL @REF@("IEN LIST") ;"List will get progressively smaller.  So KILL and reset each cycle.
        . ;"SET @REF@("DEBUG",DEBUGI)="@REF@('IENLIST') killed",DEBUGI=DEBUGI+1
        . SET IEN=0
        . FOR  SET IEN=$ORDER(IENLIST(IEN)) QUIT:(+IEN'>0)!(ABORT)  DO
        . . ;"SET @REF@("DEBUG",DEBUGI)="IEN="_IEN,DEBUGI=DEBUGI+1
        . . NEW MSG SET MSG=@REF@("MSG")
        . . IF MSG="STOP" SET ABORT=1 QUIT
        . . IF MSG="RESTART" SET ABORT=2 QUIT
        . . NEW SRCHRSLT SET SRCHRSLT=$$SRCH1TIU(PARENTJOB,IEN,TERM)
        . . ;"SET @REF@("DEBUG",DEBUGI)="Search 1 result="_SRCHRSLT,DEBUGI=DEBUGI+1
        . . IF SRCHRSLT=0 KILL IENLIST(IEN)
        . . ELSE  IF (SRCHRSLT<0) SET ABORT=-SRCHRSLT
        . . ;"SET @REF@("DEBUG",DEBUGI)="IENList count="_$$LISTCT^TMGMISC2("IENLIST"),DEBUGI=DEBUGI+1
        . IF ABORT=0 DO
        . . ;"SET @REF@("DEBUG",DEBUGI)="ABOUT TO MERGE00IENList count="_$$LISTCT^TMGMISC2("IENLIST"),DEBUGI=DEBUGI+1
        . . ;"SET @REF@("DEBUG",DEBUGI)="merging",DEBUGI=DEBUGI+1
        . . MERGE @REF@("IEN LIST")=IENLIST
        . . ;"SET @REF@("DEBUG",DEBUGI)="@REF@(IENList count="_$$LISTCT^TMGMISC2($name(@REF@("IEN LIST"))),DEBUGI=DEBUGI+1
        . . ;"SET @REF@("DEBUG",DEBUGI)="REF="_REF,DEBUGI=DEBUGI+1
        . . SET @REF@("FILTER",TERM)=""
        ;"SET @REF@("DEBUG",DEBUGI)="ABORT="_ABORT,DEBUGI=DEBUGI+1
        IF ABORT=2 GOTO L1 ;"Restart
        ;"SET @REF@("DEBUG",DEBUGI)="@REF@(IENList count="_$$LISTCT^TMGMISC2($name(@REF@("IEN LIST"))),DEBUGI=DEBUGI+1
        SET @REF@("MSG")="DONE"
        KILL @REF@("BACKGROUND")
        ;"SET @REF@("DEBUG",DEBUGI)="@REF@(IENList count="_$$LISTCT^TMGMISC2($name(@REF@("IEN LIST"))),DEBUGI=DEBUGI+1
        QUIT  ;"This will cause thie JOB'd task to exit and stop execution
        ;
        ;
PREPSRCH(PARENTJOB,TMGDFN,TMGSRCH,WORDS,IENLIST) ;
        ;"Purpose: To Parse the search phrase, and look for prior runs, and use
        ;"         that work IF possible.
        ;"Input: PARENTJOB -- the job of the RPCBroker task that called this.
        ;"       TMGDFN -- The patient IEN to look up.
        ;"       TMGSRCH -- The Search Phrase.  See docs in SRCHTIU
        ;"            e.g: 'dog cat monkey "in a barrel"
        ;"       WORDS -- PASS BY REFERENCE.  An OUT PARAMETER.  Format:
        ;"             WORDS(WordOrPhrase)=""
        ;"             e.g. WORDS(1)="DOG"
        ;"                  WORDS(2)="CAT"
        ;"                  WORDS(3)="MONKEY"
        ;"                  WORDS(4)="IN A BARREL"
        ;"             Note: If prior run is being built upon, then entries that
        ;"                   have already been searched for will be removed.
        ;"Results: none
        DO PARSSRCH(TMGSRCH,.WORDS)
        NEW REF SET REF=$NAME(^TMG("TMP","SEARCH","SRCHTIU",PARENTJOB))
        KILL IENLIST
        NEW NEWSRCH SET NEWSRCH=0  ;"Boolean for need to start over a NEW search
        IF $GET(@REF@("DFN"))'=TMGDFN SET NEWSRCH=1 GOTO NS
        ;"Look through all prior filters and see IF any filters applied that
        ;"   are not in current search
        NEW FILTERS,CT
        FOR CT=1:1 QUIT:$DATA(WORDS(CT))=0  SET FILTERS($GET(WORDS(CT)))=1
        NEW OLDFILTER MERGE OLDFILTER=@REF@("FILTER")
        NEW OFLTR SET OFLTR=""
        FOR  SET OFLTR=$ORDER(OLDFILTER(OFLTR)) QUIT:(OFLTR="")!NEWSRCH  DO
        . IF $GET(FILTERS(OFLTR))=1 KILL FILTERS(OFLTR) QUIT  ;"filter term used before, so delete from use again
        . ;"Now check IF NEW filters contain a longer verson of old term.  I.e. prior
        . ;"  filter term was 'kitt' and now it is 'kitten'
        . NEW FOUND SET FOUND=0
        . NEW F SET F=""
        . FOR  SET F=$ORDER(FILTERS(F)) QUIT:(F="")!FOUND  DO
        . . IF $EXTRACT(F,1,$LENGTH(OFLTR))'=OFLTR QUIT
        . . SET FOUND=1
        . . KILL @REF@("FILTER",OFLTR) ;"Remove old partial term from history
        . IF FOUND=0 SET NEWSRCH=1 ;"A filter was put on old SET that is not in NEW set, so start over
NS      IF NEWSRCH=1 DO
        . MERGE IENLIST=^TIU(8925,"C",TMGDFN)
        . KILL @REF@("IEN LIST")
        . KILL @REF@("FILTER")
        ELSE  DO
        . MERGE IENLIST=^TMG("TMP","SEARCH","SRCHTIU",$J,"IEN LIST")
        . ;"Recreate WORDS array as numbered list with just desired entries
        . SET CT=0 FOR  SET CT=$ORDER(WORDS(CT)) QUIT:(CT="")  DO
        . . IF $DATA(FILTERS(WORDS(CT)))=0 KILL WORDS(CT) ;"Kill all entries in WORDS not in FILTERS
        . NEW I SET I=1
        . NEW TEMP
        . SET CT=0 FOR  SET CT=$ORDER(WORDS(CT)) QUIT:(CT="")  DO
        . . SET TEMP(I)=$GET(WORDS(CT)),I=I+1
        . KILL WORDS MERGE WORDS=TEMP
        SET @REF@("DFN")=TMGDFN
        QUIT
        ;
        ;
PARSSRCH(TMGSRCH,WORDS) ;
        ;"Purpose: Separate search phrase out into array of words
        ;"Input: TMGSRCH -- The Search Phrase.  See docs in SRCHTIU
        ;"            e.g: 'dog cat monkey "in a barrel"
        ;"       WORDS -- PASS BY REFERENCE.  An OUT PARAMETER.  Format:
        ;"             WORDS(WordOrPhrase)=""
        ;"             e.g. WORDS(1)="DOG"
        ;"                  WORDS(2)="CAT"
        ;"                  WORDS(3)="MONKEY"
        ;"                  WORDS(4)="IN A BARREL"
        ;"Results: none
        KILL WORDS
        SET TMGSRCH=$GET(TMGSRCH)
        NEW CT SET CT=0
        NEW ENTRY,POS
        FOR  QUIT:(TMGSRCH="")  DO
        . SET TMGSRCH=$$TRIM^XLFSTR(TMGSRCH)
        . IF $EXTRACT(TMGSRCH,1)="""" DO
        . . SET ENTRY=$$GETWORD^TMGSTUT3(TMGSRCH,2,"""","""")
        . . IF ENTRY'="" DO
        . . . SET CT=CT+1
        . . . SET WORDS(CT)=$$UP^XLFSTR(ENTRY)
        . . SET ENTRY=""""_ENTRY
        . . IF $FIND(TMGSRCH,ENTRY_"""")>0 SET ENTRY=ENTRY_""""
        . . NEW SPEC
        . . SET SPEC(ENTRY)=""
        . . SET SPEC("  ")=" "
        . . SET TMGSRCH=$$REPLACE^XLFSTR(TMGSRCH,.SPEC)
        . SET ENTRY=$PIECE(TMGSRCH," ",1)
        . SET $PIECE(TMGSRCH," ",1)=""
        . IF ENTRY'="" DO
        . . SET CT=CT+1
        . . SET WORDS(CT)=$$UP^XLFSTR(ENTRY)
        QUIT
        ;
        ;
SRCH1TIU(PARENTJOB,IEN,TERM) ;
        ;"Purpose: Search TIU DOCUMENT report text for TERM
        ;"Input: IEN -- IEN in 8925
        ;"       TERM -- a word, or phrase, to search for in report text
        ;"NOTE: Not case sensitive
        ;"Result: 1 IF found, 0 IF not found, -1 IF Abort signal found, -2 IF RESTART signal
        NEW REF SET REF=$NAME(^TMG("TMP","SEARCH","SRCHTIU",PARENTJOB))
        NEW FOUND SET FOUND=0 ;"default to not found
        NEW LINE SET LINE=0
        NEW CT SET CT=0
        NEW ABORT SET ABORT=0
        FOR  SET LINE=$ORDER(^TIU(8925,IEN,"TEXT",LINE)) QUIT:(+LINE'>0)!FOUND!ABORT  DO
        . SET CT=CT+1
        . IF CT#5=0 DO  QUIT:ABORT  ;"Check messages every 5 lines or so
        . . SET CT=0
        . . NEW MSG SET MSG=@REF@("MSG")
        . . IF MSG="STOP" SET ABORT=1 QUIT
        . . IF MSG="RESTART" SET ABORT=2 QUIT
        . NEW ONELINE SET ONELINE=$$UP^XLFSTR($GET(^TIU(8925,IEN,"TEXT",LINE,0)))
        . IF ONELINE[TERM DO
        . . SET FOUND=1
        IF ABORT SET FOUND=-ABORT
        QUIT FOUND
  ;
