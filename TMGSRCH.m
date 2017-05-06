TMGSRCH ;TMG/kst/Search API ; 6/4/10, 2/2/14
        ;;1.0;TMG-LIB;**1**;05/19/10
        ;
 ;"TMG FILEMAN SEARCH API
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"NOTE: this function depends on NEW version of LIST^DIC, from G. Timpson Patch
 ;"=======================================================================
 ;" RPC -- Public Functions.
 ;"=======================================================================
 ;"SRCH(OUT,FILENUM,STR) --A search function, to support calls by RPC from CPRS
 ;"BKSRCH(FILENUM,STR)  -- designed to be called via JOB --> separate job thread
 ;"FMSRCH(OUT,FILENUM,COMPEXPR) --A wrapper for Fileman search call
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"PARSESTR(FILENUM,STR,ARRAY,FNUMPTR) -- Parse user input into formatted array
 ;"PARSE1(FILENUM,STR,FNUMPTR,ARRAY) --Parse a simple search term
 ;"BKPGFN(MSG,PCT) -- Callable progress function code for background thread.
 ;"DOSRCH(PTMGOUT,FILENUM,STR,PGFN) --Common search codes
 ;"=======================================================================
 ;"=======================================================================
 ;"Dependencies:
 ;" TMGDBAPI, DIE, XLFSTR, TMGSRCH0, TMGSRCH1, TMGSTUTL
 ;"=======================================================================
 ;"=======================================================================
 ;
 ;
 ;"=======================================================================
 ;"  SEARCH STRING DOCUMENTATION
 ;"=======================================================================
 ;"Search string examples:
 ;"  8925:.02(.01="SMITH,JOHN")
 ;"  1234:.01(.03in"32..55")   <-- this is a range test
 ;"  1234:.99((.01="SMITH,JOHN") OR (.01="SMITH,BILL")) AND 4567:.01(.02'=3120210) NOT (1["HAPPY")
 ;"  8925:(REPORT TEXT[DM-2)!(REPORT TEXT[HTN) AND 120.5:((VITAL TYPE=PULSE)&(RATE>70)) Targetfile=2
 ;"
 ;"SYNTAX:
 ;"  -- File specifier.  To specify searching in a file OTHER THAN target filenumber, an optional
 ;"         FILENUM:FLD[:FLD[:FLD...]] may be specified.  However, ultimately, this must point back
 ;"         to the target filenumber.  E.g. Search in file 8925, but for each entry found, use the IEN
 ;"         specified by FLD (or FLDA:FLDB or FLDA:FLDB:FLDC:...).  NOTE: If just FILENUM is provided
 ;"         without specifying FLD(s) to point to target filenumber, then the code will find a path
 ;"         (if possible), using first one found.
 ;"       FILENUM:(...)
 ;"         The logic is read from left to right, honoring parentheses.  If a filenumber
 ;"         is not specified, then the last specified filenumber is used.
 ;"         E.g. 1234:.01( LogicA ) OR 234:.99( LogicB )  AND ( LogicC )
 ;"              LogicA fields refer to file 1234:.01.
 ;"              LogicB fields refer to file 234:.99
 ;"              LogicA fields refer to file 234:.99 (last specified file number)
 ;"         E.g. 5678:.01( (LogicA1) OR 5432:.88(LogicA2) NOT (LogicA3) ) or (LogicB)
 ;"              LogicA1 fields refer to file  5678:.01
 ;"              LogicA2 fields refer to file 5432:.88
 ;"              LogicA3 fields refer to file 5432:.88 (last specified file number inside parentheses)
 ;"              LogicB fields refer to file 5678 (last specified file number at same parentheses level)
 ;"  -- Each individual search term must be enclosed in parentheses, and may contain sub-terms
 ;"     enclosed in nested parentheses
 ;"  -- Each individual search term is comprised of:
 ;"         FIELD then COMPARATOR then VALUE
 ;"          1. FIELDS -- can be name or number.  This is for currently active file (see below)
 ;"                       may also be FIELDA:FIELDB:... when FIELDA is a pointer, then FIELDB
 ;"                       is taken from the pointed-to file. If FIELDB is not provided, and FIELDA
 ;"                       is a pointer, then the .01 field of pointed-to-file.  Individual field
 ;"                       names may be inclosed in quotes
 ;"          2. COMPARATOR -- can be:
 ;"                "="                -- means exact match
 ;"                "'=", "<>",        -- any of these means Does-not-equal
 ;"                ">=", "'<"         -- means greater-than-or-equal-to (same as not-less-than)
 ;"                "<=", "'>"         -- means less-than-or-equal-to (same sa not-greater-than)
 ;"                "in","IN","In","{" -- means field is in specified rage (see Value below)
 ;"                                      When using IN, IF field name is provided by NAME (not number),
 ;"                                      then field name should be inclosed in quotes to separate the
 ;"                                      letters of the field name from the letters of 'IN'.
 ;"                "["                -- means 'contains'.  Interpreted as follows:
 ;"                         -- For Word processor (WP) fields, this means that any line in the entire field
 ;"                            can contain search term, to be matched positive.
 ;"                         -- For free text field, then just text of field is searched.
 ;"          3. VALUE -- The search term to search for.  Should be in quotes.
 ;"                      Note: IF comparator is "IN", then syntax is "Value1..Value2"
 ;"                      There should be a ".." between the two values.
 ;"                      NOTE: dates should be in Fileman format (not external format)
 ;"  -- Logical combiners of separate search terms allowed are:
 ;"            "OR" or "|" or "||" or "!"
 ;"            "AND" or "&" or "&&"
 ;"            "NOT" or "'" or "ANDNOT"
 ;"=======================================================================
 ;"=======================================================================
 ;
 ;
TEST ;
        NEW STR,OUT
        ;"SET STR="8925:(STATUS=COMPLETED)&((PATIENT[CUTSHALL)!(PATIENT[CUTSHAW))"
        SET STR="8925:(REPORT TEXT[DM-2)!(REPORT TEXT[HTN) AND 120.5:((VITAL TYPE=PULSE)&(RATE>70))"
        ;"SET STR="8925:(REPORT TEXT[DM-2) AND 120.5:((VITAL TYPE=PULSE)&(RATE>70))"
        ;"SET STR="8925:(REPORT TEXT[HTN) AND 120.5:((VITAL TYPE=PULSE)&(RATE{70..75))"
        ;"SET STR="8925:(REPORT TEXT[DM-2)!(REPORT TEXT[HTN)"
        ;"WRITE STR,!
        ;"DO SRCH(.OUT,2,STR)
        ;"NEW CT SET CT=+$GET(OUT("COUNT"))
        ;"WRITE "Found ",CT," total matches.",!
        ;
        DO BKSRCH(2,STR)
        NEW STATUS,PCT
        NEW REF SET REF=$NAME(^TMP("TMGSRCH",$J))
        FOR  DO  QUIT:(STATUS["#DONE#")
        . HANG 1
        . SET STATUS=$GET(@REF@("MSG"))
        . WRITE "STATUS: ",STATUS,!
        QUIT
 ;
SRCH(OUT,FILENUM,STR) ;
        ;"Purpose: A search function, to support calls by RPC from CPRS
        ;"Input:  OUT-- Pass by reference.  AN OUT PARAMETER.
        ;"        FILENUM -- The target file number that resulting IENs will be in
        ;"        STR -- This is a logic string for searching.  See details above.
        ;"Results:  OUT is filled in.  Format:
        ;"             OUT(0)=1    for success, or -1^Error Message
        ;"             OUT(IEN)=""
        ;"             OUT(IEN)=""
        ;"             OUT("COUNT")=Count of number of found records.
        ;"Results: None
        ;"
        DO DOSRCH("OUT",.FILENUM,.STR) ;
SRCHDN  QUIT
        ;
        ;
BKSRCH(FILENUM,STR) ;
        ;"Purpose: this function is designed to be called via JOB, to setup separate job thread
        ;"         E.g. JOB BKSRCH^TMGTMGSRCH(FILENUM,STR) NEW MSGJOB SET MSGJOB=$ZJOB
        ;"         NOTE: When job, output MSG will be "#DONE#" (see below)
        ;"Input: Filenum: This this is the target file of the search.
        ;"       STR -- This is the logic string for searching.  Format as per SRCH() docs
        ;"Output: Output will go into ^TMP("TMGSRCH",$J,"OUT")
        ;"        Messages will go into ^TMP("TMGSRCH",$J,"MSG")
        ;"        % Done  will go into ^TMP("TMGSRCH",$J,"PCT")
        ;"Results: none
        NEW PGFN SET PGFN="DO BKPGFN^TMGSRCH(.TMGSTAT,.TMGPCT)"
        NEW POUT SET POUT=$NAME(^TMP("TMGSRCH",$J,"OUT"))
        KILL @POUT
        DO DOSRCH(POUT,.FILENUM,.STR,PGFN) ;
        DO BKPGFN("#DONE#",100)
        QUIT  ;"This should terminate thread (if called by JOB as above)
        ;
BKPGFN(MSG,PCT) ;
        ;"Callable progress function code for background thread.
        SET ^TMP("TMGSRCH",$J,"MSG")=$GET(MSG)
        SET ^TMP("TMGSRCH",$J,"PCT")=$GET(PCT)
        QUIT
        ;
        ;
DOSRCH(PTMGOUT,FILENUM,STR,PGFN) ;
        ;"Common entry endpoint for search entry tags.  See docs in SRCH()
        ;"Input: PTMGOUT -- Pass by NAME.  The name of the output array
        ;"       FILENUM -- See SRCH()
        ;"       STR -- See SRCH()
        ;"       TMGPGFN -- OPTIONAL. Mumps code that will be called periodically
        ;"                            to allow display of progress of slow searches.
        ;"                            Code may depend on the following variables:
        ;"                            TMGSTAT -- The most recent status text
        ;"                            TMGPCT -- a very gross estimate of % done (0-100%)
        ;"Results -- None.
        NEW TMGARRAY,RESULT,CT
        SET RESULT=$$PARSESTR(.FILENUM,STR,.TMGARRAY)
        ;
        ;"MERGE ^TMG("TMP","RPC","TMGRPCSR","TMGARRAY")=TMGARRAY  ;"TEMP!!!
        ;
        IF +RESULT=-1 SET @PTMGOUT@(0)=RESULT GOTO DSRCHDN
        SET CT=$$ARRYSRCH^TMGSRCH0(FILENUM,PTMGOUT,.TMGARRAY,.PGFN)
        SET @PTMGOUT@("COUNT")=CT
        SET @PTMGOUT@("FILENUM")=FILENUM
        IF $GET(@PTMGOUT@(0))="" SET @PTMGOUT@(0)=1  ;"Success
DSRCHDN QUIT
        ;
        ;
PARSESTR(FILENUM,STR,ARRAY,FNUMPTR) ;
        ;"Purpose: To take user input, validate it, and parse into an formatted array
        ;"Input: FILENUM -- The file number that is the target of the search.
        ;"       STR: This is the user input string.  Format as documented in SRCH() above.
        ;"       ARRAY -- PASS BY REFERENCE.  An OUT PARAMETER.  Format as follows.
        ;"              ARRAY(1,"FNUMPTR")= FNUM:FLDA[:FLDB[:FLDC...]] FNUM is filenumber that
        ;"                                  contain search field, and then fields used to point
        ;"                                  back to *TARGET* FILENUM for entire search
        ;"              ARRAY(1,"FLD")=Fieldnumber to search
        ;"              ARRAY(1,"COMP")=Comparator, will be "=", "'=", "'<", or "'>", "[", "{", "IN"
        ;"              ARRAY(1,"SRCH")=The value of to be used in search.
        ;"              ARRAY(1,"WP")=1 IF field is a WP field
        ;"              ARRAY(2,...)  The second search term.
        ;"              ARRAY(2,"LOGIC")=#^Combiner
        ;"                          # means the SET so far.
        ;"                          Combiner will be "AND", "OR", or "NOT"
        ;"              ARRAY(3,...)  The third search term (which is comprised of sub terms)
        ;"              ARRAY(3,1,...  The first subterm (same format as higher level)
        ;"              ARRAY(3,2,...  The second subterm (same format as higher level)
        ;"              ARRAY(n,...)  The N'th search term.
        ;"    removed-> ARRAY("SETCOMP",i)= NumA^Combiner^NumB
        ;"                          NumA and NumB refer to seach term number (e.g. 1, 2, ... n above)
        ;"                          If NumA="#", then it means 'the resulting SET of results so far'
        ;"                          Combiner will be "AND", "OR", or "NOT"
        ;"                          i is the index variable, and logic should be evaluated in numerical order
        ;"       FNUMPTR: Will be used when calling self reiteratively.  Leave blank in first call.
        ;"                DON'T pass by reference.  This is 'FileNum:FLD[:FLD[:FLD...]] specifier
        ;"Results: 1 if OK, or -1^Message IF error during processing.
        ;
        NEW SUBSTRA,SUBSTRB,POS
        NEW RESULT SET RESULT=1 ;"default to success
        NEW TERMNUM SET TERMNUM=0
        SET FILENUM=+$GET(FILENUM)
        IF FILENUM'>0 DO  GOTO PSDN
        . SET RESULT="-1^Target file number not provided."
        SET FNUMPTR=$GET(FNUMPTR,FILENUM)
        SET ARRAY("FILE")=FILENUM
        NEW LOGICNUM SET LOGICNUM=0
        NEW DONE SET DONE=0
        FOR  DO  QUIT:(DONE=1)!(+RESULT=-1)
        . NEW TEMPARRAY
        . SET TERMNUM=TERMNUM+1
        . ;"--- Get file number, IF any
        . SET STR=$$TRIM^XLFSTR(STR)
        . IF +$PIECE(STR,"(",1)>0 DO  QUIT:(+RESULT=-1)
        . . SET FNUMPTR=$PIECE(STR,"(",1)  ;"Convert 1234:.01:.02:(...) --> 1234:.01:.02:
        . . IF $EXTRACT(FNUMPTR,$LENGTH(FNUMPTR))=":" SET FNUMPTR=$EXTRACT(FNUMPTR,1,$LENGTH(FNUMPTR)-1)
        . . IF ($PIECE(FNUMPTR,":",2)="")&(+FNUMPTR'=FILENUM) DO  QUIT:(+RESULT=-1)
        . . . NEW SAVPTR SET SAVPTR=FNUMPTR
        . . . SET FNUMPTR=$PIECE($$PATHTO^TMGSRCH1(+FNUMPTR,FILENUM),"^",1)
        . . . IF FNUMPTR="" SET RESULT="-1^Unable to find path to file #"_FILENUM_" from "_SAVPTR
        . . ELSE  IF $$FNPTR^TMGSRCH1(FNUMPTR)'=FILENUM DO  QUIT
        . . . SET RESULT="-1^'"_FNUMPTR_"' points to file #"_$$FNPTR^TMGSRCH1(FNUMPTR)_", not file #"_FILENUM_" as expected"
        . ;"Split STR --> SUBSTRA + SUBSTRB
        . SET SUBSTRA=$$MATCHXTR^TMGSTUT3(STR,"(",,,"(")
        . IF SUBSTRA="" SET DONE=1 QUIT
        . SET POS=$FIND(STR,SUBSTRA)  ;"Return pos of following character
        . SET SUBSTRB=$EXTRACT(STR,POS+1,9999) ;"Should be " [LOGICTERM] [SearchTerm]..."
        . ;"Process SUBSTRA, either directly IF single term, or recursively IF compound term.
        . IF $$HNQTSUB^TMGSTUTL(SUBSTRA,"(") DO
        . . SET RESULT=$$PARSESTR(FILENUM,SUBSTRA,.TEMPARRAY,FNUMPTR)
        . . SET ARRAY(TERMNUM,"SUBTERMS")=1
        . ELSE  DO
        . . SET RESULT=$$PARSE1(FILENUM,SUBSTRA,FNUMPTR,.TEMPARRAY)
        . IF +RESULT=-1 QUIT
        . SET SUBSTRA=""
        . MERGE ARRAY(TERMNUM)=TEMPARRAY
        . ;"Now get Logic term connecting this to next term (if any)
        . SET SUBSTRB=$$TRIM^XLFSTR(SUBSTRB) ;"Remove opening (and closing) spaces
        . NEW LOGICTERM SET LOGICTERM=""
        . NEW P,CH
        . NEW DNCOMB SET DNCOMB=0
        . FOR P=1:1:$LENGTH(SUBSTRB) DO  QUIT:DNCOMB!(+RESULT=-1)
        . . SET CH=$$UP^XLFSTR($EXTRACT(SUBSTRB,P))
        . . IF ("&|'!ANDORNOT"'[CH) SET DNCOMB=1 QUIT
        . . SET LOGICTERM=LOGICTERM_CH
        . SET STR=$EXTRACT(SUBSTRB,$LENGTH(LOGICTERM)+1,9999),SUBSTRB=""
        . IF LOGICTERM="" QUIT
        . SET LOGICTERM=$$FIXCOMB^TMGSRCH1(LOGICTERM,.RESULT) QUIT:(+RESULT=-1)
        . NEW CURSET SET CURSET=$SELECT(TERMNUM=1:"1",1:"#")
        . SET LOGICNUM=LOGICNUM+1
        . ;"SET ARRAY("SETCOMP",LOGICNUM)=CURSET_"^"_LOGICTERM_"^"_(TERMNUM+1) ;"will check later that TERMNUM+1 is supplied
        . SET ARRAY(TERMNUM+1,"LOGIC")="#^"_LOGICTERM
PSDN        QUIT RESULT
        ;
        ;
PARSE1(FILENUM,STR,FNUMPTR,ARRAY) ;
        ;"Purpose: Parse a simple search term (e.g. .01="SMITH,JOHN"). Also validate that field exists in file.
        ;"Input: FILENUM -- The TARGET filenumber that the entire search is referencing.
        ;"       STR: This is part of the user input string to parse
        ;"       FNUMPTR: FNUM:FLDA[:FLDB[:FLDC...]] FNUM is filenumber that contain search field, and then
        ;"                fields used to point back to *TARGET* FILENUM for entire search
        ;"       ARRAY -- PASS BY REFERENCE.  An OUT PARAMETER.  Format as follows.
        ;"              ARRAY("FNUMPTR")=Filenumber that contains field)
        ;"              ARRAY("FLD")=Fieldnumber to search
        ;"              ARRAY("COMP")=Comparator, will be "=", "'=", "'<", or "'>", "[","IN", "{"
        ;"              ARRAY("SRCH")=The value of to be used in search.
        ;"              ARRAY("WP")=1 IF field is a WP field
        ;"NOTE:  If field specifies a DATE, then the search value will be converted to FileMan format
        ;"Results: 1 if OK, or -1^Message IF error during processing.
        ;"
        NEW RESULT SET RESULT=1 ;"default to success
        NEW SAV SET SAV=STR
        SET STR=$$TRIM^XLFSTR($GET(STR))
        SET ARRAY("FNUMPTR")=FNUMPTR
        NEW FLD,FLDS SET FLDS=""
        NEW TMGTFILE SET TMGTFILE=+FNUMPTR
        FOR  QUIT:("'<>=[:({"[$EXTRACT(STR,1))!(STR="")  DO
        . SET FLD=$$GETFLD^TMGSRCH1(.STR) ;
        . NEW SAVFIL SET SAVFIL=TMGTFILE
        . NEW ONEFLD SET ONEFLD=$$FLDNUM^TMGSRCH1(.TMGTFILE,.FLD)
        . IF ONEFLD'>0 DO  QUIT
        . . SET RESULT="-1^Field ["_FLD_"] was not found in file ["_SAVFIL_"]"
        . IF FLDS'="" SET FLDS=FLDS_":"
        . SET FLDS=FLDS_ONEFLD
        IF +RESULT=-1 GOTO PS1DN
        SET ARRAY("FLD")=FLDS
        IF $$ISWPFLD^TMGDBAPI(+FNUMPTR,+FLDS) SET ARRAY("WP")=1
        NEW FLDTYPE SET FLDTYPE=$PIECE($GET(^DD(+FNUMPTR,+FLDS,0)),"^",2)
        IF FLDTYPE["M" DO  GOTO PS1DN
        . SET RESULT="-1^Searches in fields that are MULTIPLES not supported"
        SET STR=$$TRIM^XLFSTR(STR)
        NEW COMP
        IF $$UP^XLFSTR($EXTRACT(STR,1,3))="'IN" SET COMP="'IN"
        ELSE  IF $$UP^XLFSTR($EXTRACT(STR,1,2))="IN" SET COMP="IN"
        ELSE  DO
        . SET COMP="" NEW P,CH
        . FOR P=1:1:$LENGTH(STR) SET CH=$EXTRACT(STR,P) QUIT:("'!<>=[{"'[CH)  SET COMP=COMP_CH
        SET STR=$EXTRACT(STR,$LENGTH(COMP)+1,9999)
        SET COMP=$$FIXCOMP^TMGSRCH1(COMP,.RESULT)
        IF +RESULT=-1 GOTO PS1DN
        SET ARRAY("COMP")=COMP
        SET STR=$$TRIM^XLFSTR(STR) ;"Remove any spaces after comparator
        NEW SRCH SET SRCH=$$TRIM^XLFSTR(STR,,"""") ;"Trim quotes, IF any.
        IF FLDTYPE["D" DO  GOTO:(+RESULT=-1) PS1DN   ;"standardized dates
        . NEW ADATE SET ADATE=SRCH
        . NEW TEMPRSLT SET TEMPRSLT=""
        . FOR  QUIT:(ADATE="")!(+RESULT=-1)  DO
        . . IF TEMPRSLT'="" SET TEMPRSLT=TEMPRSLT_".."
        . . SET TEMPRSLT=TEMPRSLT_$$STDDATE^TMGSRCH1($PIECE(ADATE,"..",1),.RESULT)
        . . IF +RESULT=-1 QUIT
        . . SET ADATE=$PIECE(ADATE,"..",2)
        . SET SRCH=TEMPRSLT
        ELSE  IF FLDTYPE["S" DO  ;"Convert FM SET type into internal format
        . NEW OUT,TMGMSG
        . DO VAL^DIE(+FNUMPTR,"+1,",FLD,"E",SRCH,.OUT,,"TMGMSG")
        . SET SRCH=$GET(OUT)
        IF SRCH'="" SET ARRAY("SRCH")=SRCH
        ELSE  DO  GOTO PS1DN
        . SET RESULT="-1^Search value is invalid"
        ;
PS1DN   IF +RESULT=-1 SET RESULT=RESULT_", found in ["_SAV_"]"
        QUIT RESULT
        ;
        ;
FMSRCH(TMGFILE,TMGCOMPEXPR,TMGOUT,TMGOPTION)  ;
        QUIT $$FMSRCH^TMGSRCH0(.TMGFILE,.TMGCOMPEXPR,.TMGOUT,.TMGOPTION)
        ;
