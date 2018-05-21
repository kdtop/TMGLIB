TMGRPCS0 ;TMG/kst/RPC entry points for Search API ; 6/4/10, 2/2/14
        ;;1.0;TMG-LIB;**1**;05/25/10
        ;
 ;"RPC ENTRY POINTS FOR TMG FILEMAN SEARCH API
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
 ;"NOTE: this function depends on NEW version of LIST^DIC, from G. Timpson Patch
 ;"=======================================================================
 ;" RPC -- Public Functions.
 ;"=======================================================================
 ;"LAUNCH(OUT,PARAMS) -- launch background search thread, return JOB #
 ;"STATUS(OUT,JOBNUM) --Return status of background job.
 ;"IENLIST(OUT,JOBNUM) -- Return results from background search job.
 ;"PREPSB(OUT,TMGPARAMS) --Prepare an array that can be used by an TORComboBox.NeedData
 ;"                        NOTE: This should not be called until STATUS() returns #DONE# 
 ;"IENDETAL(OUT,TMGPARAMS) -- Return Detail of 1 IEN from from results from background search job.
 ;"GETRSLTSB(TMGOUT,TMGPARAMS) -- Get RESULTS list subset, for job number. 
 ;"                       NOTE: Should only be called after a successful call to PREPSB^TMGRPCS0()
 ;"CLEAR(OUT,JOBNUM) -- Clear data from background search job.
 ;"
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"
 ;"=======================================================================
 ;"=======================================================================
 ;"Dependencies:
 ;"  ^DIQ, ^XLFSTR, ^TMGSRCH
 ;"=======================================================================
 ;"=======================================================================
 ;
TEST ;
        NEW STR,OUT
        ;"SET STR="8925:(STATUS=COMPLETED)&((PATIENT[CUTSHALL)!(PATIENT[CUTSHAW))"
        ;"SET STR="8925:(REPORT TEXT[DM-2)!(REPORT TEXT[HTN) AND 120.5:((VITAL TYPE=PULSE)&(RATE>70))"
        ;"SET STR="8925:(REPORT TEXT[DM-2) AND 120.5:((VITAL TYPE=PULSE)&(RATE>70))"
        ;"SET STR="8925:(REPORT TEXT[DM-2)!(REPORT TEXT[HTN)"
        SET STR="8925:(REPORT TEXT[HTN) AND 120.5:((VITAL TYPE=PULSE)&(RATE{70..75))"
        NEW RESULT
        DO LAUNCH(.RESULT,"2^"_STR)
        SET JOBNUM=+$GET(RESULT(0))
        FOR  DO  QUIT:(STATUS["#DONE#")
        . HANG 1
        . DO STATUS(.STATUS,JOBNUM)
        . WRITE "STATUS: ",STATUS,!
        DO IENLIST(.OUT,JOBNUM) ;
        IF $DATA(OUT) DO ZWRITE^TMGZWR("OUT")
        DO PRESS2GO^TMGUSRI2
        QUIT
        ;
LAUNCH(OUT,TMGPARAM) ;
        ;"Purpose: to launch background search thread, and return its JOB number
        ;"Input: OUT -- Passed by REFERENCE.  A single value
        ;"       TMGPARAM -- Filenumber^SearchString
        ;"                Filenumber -- The file number to search for
        ;"                SearchStr -- The logic string.  See docs in TMGSRCH.m
        ;"Output: OUT=Job#  or -1^Message
        ;"Results: None
        SET ^TMG("EDDIE","LAUNCH")=TMGPARAM
        NEW FILENUM SET FILENUM=$PIECE(TMGPARAM,"^",1)
        NEW SRCHSTR SET SRCHSTR=$PIECE(TMGPARAM,"^",2)
        NEW SRCHDEBUG SET SRCHDEBUG=0
        IF SRCHDEBUG=1 DO  QUIT
        . DO SRCH^TMGSRCH(.OUT,FILENUM,SRCHSTR)
        JOB BKSRCH^TMGSRCH(FILENUM,SRCHSTR)
        SET OUT(0)="1^"_$$LASTJOBN^TMGKERN1()
        QUIT
        ;
STATUS(OUT,JOBNUM) ;
        ;"Purpose: Return status of background job.
        ;"Input: OUT -- Passed by REFERENCE.  A single value
        ;"       JOBNUM -- The job number of task to query
        ;"Output: OUT(0)=1^%Done^Message.  Will be '100^#DONE#' when task is done.
        ;"Results: None
        NEW REF SET REF=$NAME(^TMP("TMGSRCH",JOBNUM))
        NEW PCT SET PCT=$GET(@REF@("PCT"))
        NEW MSG SET MSG=$GET(@REF@("MSG"))
        NEW CNT SET CNT=$GET(@REF@("OUT","COUNT"))
        SET OUT(0)="1^"_PCT_"^"_MSG_"^"_CNT
        QUIT
        ;
IENLIST(OUT,TMGPARAM) ;
        ;"Purpose: Return IEN LIST from results from background search job.
        ;"         NOTE: This should not be called until STATUS() returns #DONE#
        ;"Input: OUT -- Passed by REFERENCE.  And out array
        ;"       TMGPARAM -- JOBNUM^FLDNUM
        ;"              JOBNUM = The job number of task to query
        ;"              FLDNUM = The desired field number. OPTIONAL.  Default is none
        ;"Output: OUT(0)=status
        ;"        OUT(index)=IEN^[VALUE] <-- Value is external value of FLD
        ;"        OUT(index)=IEN^[VALUE] <-- Value is external value of FLD
        ;"Results: None
        NEW JOBNUM SET JOBNUM=+$PIECE(TMGPARAM,"^",1)
        NEW TMGFLD SET TMGFLD=+$PIECE(TMGPARAM,"^",2)
        NEW REF SET REF=$NAME(^TMP("TMGSRCH",JOBNUM))
        NEW TMGFNUM SET TMGFNUM=+$GET(@REF@("OUT","FILENUM"))
        NEW I SET I=1
        NEW IEN SET IEN=0
        IF (TMGFLD>0),(TMGFNUM>0) GOTO IL2 ;"Handle differently
        ;"------------------------------------
        FOR  SET IEN=$ORDER(@REF@("OUT",IEN)) QUIT:(+IEN'>0)  DO
        . NEW VALUE SET VALUE=""
        . IF (TMGFLD>0),(TMGFNUM>0) SET VALUE=$$GET1^DIQ(TMGFNUM,IEN_",",TMGFLD)
        . SET OUT(I)=IEN_"^"_VALUE
        . SET I=I+1
        GOTO ILDN
        ;"------------------------------------
IL2     ;"Sort by FLD value, not IEN value
        NEW TEMP,VALUE
        FOR  SET IEN=$ORDER(@REF@("OUT",IEN)) QUIT:(+IEN'>0)  DO
        . SET VALUE=$$GET1^DIQ(TMGFNUM,IEN_",",TMGFLD)
        . SET TEMP(VALUE,IEN)=""
        SET VALUE=""
        FOR  SET VALUE=$ORDER(TEMP(VALUE)) QUIT:(VALUE="")  DO
        . SET IEN=0  FOR  SET IEN=$ORDER(TEMP(VALUE,IEN)) QUIT:(IEN="")  DO
        . . SET OUT(I)=IEN_"^"_VALUE
        . . SET I=I+1
        ;"------------------------------------
ILDN    IF $DATA(OUT)=0 SET OUT(0)="-1^NO RESULTS"
        ELSE  SET OUT(0)="1^Success"
        QUIT
        ;
PREPSB(OUT,TMGPARAMS) ;"Prep Subset
        ;"Purpose: Prepare an array that can be used by an TORComboBox.NeedData
        ;"         to return a subset of the results.
        ;"         NOTE: This should not be called until STATUS() returns #DONE#
        ;"Input: OUT -- Passed by REFERENCE.  And out array
        ;"       TMGPARAM -- JOBNUM^Field[;FLD[;FLD...]]
        ;"              JOBNUM = The job number of task to query
        ;"              Field... = The desired field number(s). OPTIONAL. DEFAULT is .01
        ;"                         If more than one supplied, then output is
        ;"                         concatinated.  Separate fieldnumbers with ';'
        ;"Output: OUT(0)=1^Success  or -1^Message
        ;"Results: None
        NEW JOBNUM SET JOBNUM=+$PIECE(TMGPARAMS,"^",1)
        NEW TMGFLDS SET TMGFLDS=$PIECE(TMGPARAMS,"^",2)
        IF TMGFLDS="" SET TMGFLDS=".01"
        NEW REF SET REF=$NAME(^TMP("TMGSRCH",JOBNUM))
        NEW TMGFNUM SET TMGFNUM=+$GET(@REF@("OUT","FILENUM"))
        IF TMGFNUM'>0 DO  GOTO PREPDN
        . SET OUT(0)="-1^Unable to find file number at "_$NAME(@REF@("OUT","FILENUM"))
        NEW IEN SET IEN=0
        NEW VALUE
        NEW TMGERR SET TMGERR=0
        FOR  SET IEN=$ORDER(@REF@("OUT",IEN)) QUIT:(+IEN'>0)!TMGERR  DO
        . SET VALUE=""
        . NEW I FOR I=1:1:$LENGTH(TMGFLDS,";") DO
        . . NEW TMG1FLD SET TMG1FLD=+$PIECE(TMGFLDS,";",I) QUIT:TMG1FLD'>0
        . . SET VALUE=VALUE_$$GET1^DIQ(TMGFNUM,IEN_",",TMG1FLD,,"TMGERR")_" "
        . . IF $DATA(TMGERR("DIERR")) DO
        . . . SET TMGERR=1
        . . . SET TMGERR("MSG")=$$GETERRST^TMGDEBU2(.TMGERR)
        . SET VALUE=$$TRIM^XLFSTR(VALUE)
        . QUIT:VALUE=""
        . SET @REF@("B",VALUE,IEN)=""
        ;"------------------------------------
        IF TMGERR SET OUT(0)="-1^"_$GET(TMGERR("MSG"))
        IF $DATA(@REF@("B"))=0 SET OUT(0)="-1^NO RESULTS"
        ELSE  SET OUT(0)="1^Success"
PREPDN  QUIT
        ;
IENDETAL(OUT,TMGPARAMS) ;
        ;"Purpose: Return Detail of 1 IEN from from results from background search job.
        ;"         NOTE: This should not be called until STATUS() returns #DONE#
        ;"         Example: Imagine that a search has been made for a PATIENT with
        ;"            an associated TIU DOCUMENT containing "HTN".  The primary
        ;"            goal of the search is to get the IEN of the found PATIENT(s)
        ;"            However, after finding this patient, one might want to be
        ;"            able to reference the particular TIU DOCUMENTS leading to
        ;"            the match.  That is the purpose of this function.  So, in
        ;"            the parameters below, the input IEN would be the IEN in
        ;"            the PATIENT file, and the output would include the file
        ;"            number for TIU DOCUMENT, and the IEN's of the entries in
        ;"            this file that lead to the final results
        ;"Input: OUT -- Passed by REFERENCE.  And out array
        ;"       TMGPARAM -- JobNum^IEN^Field(s)
        ;"                JOBNUM -- The job number of task to query
        ;"                IEN -- The End Search IEN
        ;"                Field(s) -- OPTIONAL.  Default is .01.  Format:
        ;"                  Fld#;Fld#;Fld#;... (any number of fields, separated by a ";")
        ;"                    NOTE: IF Fld is not specified, then a SET of hard-coded fields
        ;"                         will be returned, depending on what file# is encountered        
        ;"                        as follows:
        ;"                       FILE# 2 --> '.01;.03'
        ;"                       FILE# 8925 --> '.01;.07'
        ;"Output: OUT(0)=status
        ;"        OUT(index)=FileNum^IENInFile^FieldValue^FieldValue^FieldValue^...
        ;"                                     (a piece returned for each requested field)
        ;"Results: None
        SET TMGPARAMS=$GET(TMGPARAMS)
        NEW JOBNUM SET JOBNUM=+$PIECE(TMGPARAMS,"^",1)
        IF JOBNUM=0 DO  GOTO IEDDN
        . SET OUT(0)="-1^Invalid Job Number."
        NEW SRCHIEN SET SRCHIEN=+$PIECE(TMGPARAMS,"^",2)
        IF SRCHIEN=0 DO  GOTO IEDDN
        . SET OUT(0)="-1^Invalid IEN Number."
        NEW PARAMFIELDS SET PARAMFIELDS=$PIECE(TMGPARAMS,"^",3)
        NEW REF SET REF=$NAME(^TMP("TMGSRCH",JOBNUM,"OUT","DETAILS",SRCHIEN))
        NEW IDX SET IDX=0
        NEW FNUM SET FNUM=0
        FOR  SET FNUM=$ORDER(@REF@(FNUM)) QUIT:(+FNUM'>0)  DO
        . NEW FIELDS SET FIELDS=PARAMFIELDS
        . IF FIELDS="" DO
        . . IF FNUM=2 SET FIELDS=".01;.03" QUIT
        . . IF FNUM=8925 SET FIELDS=".01;.07" QUIT
        . . SET FIELDS=".01"
        . NEW FLDIDX FOR FLDIDX=1:1:$LENGTH(FIELDS,";") SET FIELDS(FLDIDX)=$PIECE(FIELDS,";",FLDIDX)
        . NEW SUPIEN SET SUPIEN=0
        . FOR  SET SUPIEN=$ORDER(@REF@(FNUM,SUPIEN)) QUIT:(+SUPIEN'>0)  DO
        . . NEW VALUE SET VALUE=$GET(@REF@(FNUM,SUPIEN))
        . . IF VALUE="" DO
        . . . NEW AVAL SET AVAL=""
        . . . NEW JDX SET JDX=0
        . . . FOR  SET JDX=$ORDER(FIELDS(JDX)) QUIT:+JDX'>0  DO
        . . . . NEW AFLD SET AFLD=$GET(FIELDS(JDX)) QUIT:+AFLD'>0
        . . . . SET AVAL=$$GET1^DIQ(FNUM,SUPIEN_",",AFLD)
        . . . . IF VALUE'="" SET VALUE=VALUE_"^"
        . . . . SET VALUE=VALUE_AVAL
        . . . SET @REF@(FNUM,SUPIEN)=VALUE
        . . SET IDX=IDX+1,OUT(IDX)=FNUM_"^"_SUPIEN_"^"_VALUE
        IF $DATA(OUT)=0 SET OUT(0)="-1^NO RESULTS"
        ELSE  SET OUT(0)="1^Success"
IEDDN   QUIT
        ;
GETRSLTSB(TMGOUT,TMGPARAMS) ;
        ;"Purpose: Get RESULTS list subset, for job number. NOTE: This should
        ;"         only be called after a successful call to PREPSB^TMGRPCS0()
        ;"         which will prepair the list.
        ;"Input: TMGPARAMS -- JobNum^ListStartValue^direction^MaxCount(optional, def=44)
        ;"              JobNum -- this is job number of results to return.
        ;"              ListStartValue -- OPTIONAL -- text to $ORDER() from
        ;"              Direction -- $ORDER(xx,Direction) direction (should be 1 or -1) -- OPTIONAL
        ;"              MaxCount -- OPTIONAL.  Default is 44 values returned.
        ;"Output: TMGRESULTS is filled as follows.
        ;"            TMGRESULT(0)="1^Success" or "-1^Message"
        ;"            TMGRESULT(1)=IENNum^RequestedFieldNames
        ;"            TMGRESULT(2)=IENNum^RequestedFieldNames
        ;"NOTE: Any files that don't have data are excluded.  Subfiles also excluded
        ;
        NEW JOBNUM SET JOBNUM=+$PIECE(TMGPARAMS,"^",1)
        IF JOBNUM'>0 DO  GOTO GAFSDN
        . SET TMGOUT(0)="-1^No Job Number Supplied"
        NEW TMGFROM SET TMGFROM=$PIECE(TMGPARAMS,"^",2)
        NEW TMGDIR SET TMGDIR=$PIECE(TMGPARAMS,"^",3)
        IF TMGDIR'=-1 SET TMGDIR=1
        NEW TMGMAXCT SET TMGMAXCT=+$PIECE(TMGPARAMS,"^",4)
        IF TMGMAXCT=0 SET TMGMAXCT=44
        ;
        NEW TMGREF SET TMGREF=$NAME(^TMP("TMGSRCH",JOBNUM))
        NEW TMGI SET TMGI=0
        FOR  SET TMGFROM=$ORDER(@TMGREF@("B",TMGFROM),TMGDIR) QUIT:(TMGFROM="")!(TMGI'<TMGMAXCT)  DO
        . NEW TMGIEN SET TMGIEN=""
        . FOR  SET TMGIEN=$ORDER(@TMGREF@("B",TMGFROM,TMGIEN),TMGDIR) QUIT:(+TMGIEN'>0)!(TMGI'<TMGMAXCT)  DO
        . . SET TMGI=TMGI+1
        . . SET TMGOUT(TMGI)=TMGIEN_"^"_TMGFROM
        ;
        IF $DATA(TMGOUT)=0 SET TMGOUT(0)="-1^NO RESULTS"
        ELSE  SET TMGOUT(0)="1^Success"
GAFSDN  QUIT
        ;
CLEAR(OUT,JOBNUM) ;
        ;"Purpose: Clear results from background search job.
        ;"Output: OUT(0)=1^Success"
        ;"Results: None
        NEW REF SET REF=$NAME(^TMP("TMGSRCH",JOBNUM))
        KILL @REF
        SET OUT(0)="1^Success"
        QUIT
        ;
