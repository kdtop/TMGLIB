TMGSIPH2 ;TMG/kst/SIPHON PROGRAM, FOR TRANSFERRING VISTA INSTANCES ;11/27/09, 2/2/14
         ;;1.0;TMG-LIB;**1**;11/27/09
 ;
 ;"TMG SIPHON PROGRAM, FOR TRANSFERRING VISTA INSTANCE
 ;"----===== SERVER-SIDE CODE ====------
 ;"Especially functions for working with the data dictionaries, POINTERS IN.
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
 ;"HNDLPTIX(FILENUM) --prepair PT XREF for all records pointing INTO specified file.
 ;"GETPTIN(PARAMS) --get a listing of all pointers INTO requested record
 ;"BAKXREF(PARAMS) --Make a xref of cross-references (a backward xref)
 ;"GETXRAGE --Return, in HOURS, the time since the ^TMG("PTXREF") array has had any modification
 ;"FLD01(PARAMS) -- return .01 field of a record.  Gets INTERNAL value, and doesn't support subfiles.
 ;"GET01FLD(PARAMS) --To SEND .01 field of a record.  Gets INTERNAL value, and doesn't support subfiles.

 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"TMGKERN2, TMGUSRIF, TMGFMUT2
 ;"=======================================================================
 ;
HNDLPTIX(FILENUM,CLSIDE) ;
        ;"Purpose: To prepair PT XREF for all records pointing INTO specified file.
        ;"Input: FILENUM -- The fileman file number to get pointers INTO.
        ;"       CLSIDE -- OPTIONAL.  If =1, then will be running on client side, and will work differently
        ;"Result: None
        SET FILENUM=+$GET(FILENUM) IF FILENUM'>0 QUIT
        SET CLSIDE=+$GET(CLSIDE,0)
        NEW TMGSTIME SET TMGSTIME=$H
        NEW PGFN,LIMITS
        IF 'CLSIDE SET PGFN="DO SEND^TMGKERN2(""#THINKING#|Organizing pointers for ""_TMGFNAME_"":  ""_TMGIEN_"" of ""_TMGMAX)"
        ELSE  DO
        . SET PGFN="WRITE ""Organizing pointers for ""_TMGFNAME_"":  ""_TMGIEN_"" of ""_TMGMAX"
        . SET LIMITS("REF")=$NAME(^TMG("TMGSIPH","DOWNLOADED"))
        DO SETPTOUT^TMGFMUT2(FILENUM,$NAME(^TMG("PTXREF")),PGFN,3000,.LIMITS)
        SET ^TMG("PTXREF","IN",FILENUM)=$H
        SET ^TMG("PTXREF")=$H
        QUIT
 ;
 ;
GETPTIN(PARAMS,CLSIDE)
        ;"Purpose: To get a listing of all pointers INTO requested record
        ;"Input: PARAMS -- this is FILENUM^IEN
        ;"       CLSIDE -- PASS BY REFERNCE.  OPTIONAL.  If =1, then will be running on client side, and will work differently
        ;"                 Will also be used as an OUT PARAMETER when CLSIDE=1.  Format:
        ;"                   CLSIDE(1)=FROMFILE^FROMIENS^FROMFLD
        ;"                   CLSIDE(2)=FROMFILE^FROMIENS^FROMFLD
        ;"                   ...
        ;"Output: Will return data to client.  Format:
        ;"               FROMFILE^FROMIENS^FROMFLD
        ;"               FROMFILE^FROMIENS^FROMFLD
        ;"               FROMFILE^FROMIENS^FROMFLD   (e.g. one line for every pointer in)
        ;"Result: None.
        NEW FILENUM SET FILENUM=+$PIECE(PARAMS,"^",1)
        IF $DATA(^TMG("PTXREF","IN",FILENUM))'>0 DO HNDLPTIX(FILENUM,.CLSIDE)
        DO GETPTIN^TMGFMUT2(PARAMS,.CLSIDE) ;
        SET CLSIDE=+$GET(CLSIDE,0) IF CLSIDE QUIT
        NEW TMGCT SET TMGCT=0
        FOR  SET TMGCT=$ORDER(CLSIDE(TMGCT)) QUIT:(TMGCT="")  DO
        . NEW TEMP SET TEMP=$GET(CLSIDE(TMGCT)) QUIT:(TEMP="")
        . DO SEND^TMGKERN2(TEMP)
        QUIT
 ;
 ;
BAKXREF(PARAMS) ;
        ;"Purpose: Make a xref of cross-references (a backward xref)
        ;"Input: PARAMS -- This is FILENUM^[KEEP]
        ;"                 FILENUM -- The fileman file to work with
        ;"                 KEEP -- optional.  DEFAULT=0;  If '1', then nothing done IF xref already exists.
        ;"Output: ^TMG("PTXREF","XREFS",FILENUM,IEN,REF)=<xref value>
        ;"        e.g. ^TMG("PTXREF","XREFS",FILENUM,113,"^VA(200,""A"",8870804679,113)")=6188
        ;"Result: none.
        ;"DO SEND^TMGKERN2("#THINKING#|Organizing server cross-reference enteries...")
        NEW PGFN
        SET PGFN="DO SEND^TMGKERN2(""#THINKING#|Processing index: ""_INDEX_"" for file #""_FILENUM)"
        DO BAKXREF^TMGFMUT2(PARAMS,PGFN)
        ;"DO SEND^TMGKERN2("#THINKING#|Completed.")
BXDN    QUIT
 ;
 ;
GETXRAGE ;
        ;"Purpose: Return, in HOURS, the time since the ^TMG("PTXREF") array has had any modification
        ;"OUTPUT: Sends 0 IF not currently defined, otherwise number of HOURS since setup.
        ;"Results: None
        DO SEND^TMGKERN2($$GETXRAGE^TMGFMUT2)
        QUIT
 ;
 ;
FLD01(PARAMS) ;
        ;"Purpose: To return .01 field of a record.
        ;"Input: PARAMS -- this is FILENUM^IEN
        ;"                 Note: FILENUM can be in format of subfilenum{parentfilenum{grandparentnum
        ;"                       In this case, IEN must be an IENS to be passed to $$GET1^DIQ
        ;"Result: returns .01 value.  Internal format (for speed), or External format IF subfile.
        NEW FILENUM SET FILENUM=$PIECE(PARAMS,"^",1)
        NEW RESULT SET RESULT=""
        IF FILENUM["{" DO
        . SET FILENUM=+FILENUM
        . NEW IENS SET IENS=$PIECE(PARAMS,"^",2)
        . SET RESULT=$$GET1^DIQ(FILENUM,IENS,.01,"E")
        ELSE  DO
        . SET FILENUM=+FILENUM
        . NEW IEN SET IEN=+$PIECE(PARAMS,"^",2)
        . NEW GREF SET GREF=$GET(^DIC(FILENUM,0,"GL"))
        . IF GREF="" SET RESULT="<ERROR>" GOTO F1DN
        . NEW CGREF SET CGREF=$$CREF^DILF(GREF)
        . NEW VALUE SET VALUE=$GET(@CGREF@(IEN,0))
        . SET RESULT=$PIECE(VALUE,"^",1)
        . IF RESULT="" SET RESULT="<NONE FOUND AT "_CGREF_"("_IEN_")>"
F1DN    QUIT RESULT
 ;
 ;
GET01FLD(PARAMS) ;
        ;"Purpose: To get .01 field of a record.
        ;"Input: PARAMS -- this is FILENUM^IEN
        ;"                    FILENUM can be File number, or SubFileNum{ParentFileNum{Grandparent...
        ;"                    IEN can be a record number, or IENS (e.g. '1,2456,')
        ;"Output: Will return data to client.  Format:
        ;"          <.01 value>
        ;"Result: None.
        NEW VALUE
        DO DEBUGMSG^TMGKERN2("In GET01FLD. PARAMS="_PARAMS)
        SET VALUE=$$FLD01(.PARAMS)
        DO DEBUGMSG^TMGKERN2("In GET01FLD. VALUE="_VALUE)
        DO SEND^TMGKERN2(VALUE)
        DO DEBUGMSG^TMGKERN2("Leaving GET01FLD.")
        QUIT
 ;
 ;
HANDIENL(PARAMS) ;
        ;"Purpose: To return a listing of all records (IEN's) in specified file.
        ;"Input : PARAMS -- this is FILENUM  (Subfiles not supported)
        ;"Output:  Will return data to client.  Format:
        ;"           <IEN>^.01 Value (internal format)
        ;"           <IEN2>^.01 Value (internal format)
        ;"           <IEN3>^.01 Value (internal format) ...
        ;"Results: None
        SET PARAMS=$GET(PARAMS)
        NEW FILENUM SET FILENUM=$PIECE(PARAMS,"^",1)
        IF +FILENUM'>0 QUIT
        NEW GREF SET GREF=$GET(^DIC(FILENUM,0,"GL"))
        IF GREF="" QUIT
        NEW CGREF SET CGREF=$$CREF^DILF(GREF)
        NEW TMGCT SET TMGCT=1
        NEW IEN SET IEN=0
        FOR  SET IEN=$ORDER(@CGREF@(IEN)) QUIT:(+IEN'>0)  DO
        . NEW VALUE SET VALUE=$PIECE($GET(@CGREF@(IEN,0)),"^",1)
        . DO SEND^TMGKERN2(IEN_"^"_VALUE)
        . SET TMGCT=TMGCT+1
        . IF TMGCT>5000 DO
        . . DO SEND^TMGKERN2("#THINKING#|Processing IEN: "_IEN_" for file #"_FILENUM)
        . . SET TMGCT=0
        QUIT
 ;
HANDLIENHDR(PARAMS) ;
        ;"Purpose: Return the Fileman records of the last record added, and highest IEN number from File
        ;"Input : PARAMS -- this is FILENUM  (Subfiles not supported)
        ;"Output:  Will return data to client.  Format:
        ;"           LastIEN^NumIENs
        ;"Results: None
        SET PARAMS=$GET(PARAMS)
        NEW FILENUM SET FILENUM=$PIECE(PARAMS,"^",1)
        IF +FILENUM'>0 QUIT
        NEW GREF SET GREF=$GET(^DIC(FILENUM,0,"GL"))
        IF GREF="" QUIT
        NEW NODE SET NODE=$GET(@(GREF_"0)"))
        NEW LASTIEN SET LASTIEN=$PIECE(NODE,"^",3)
        NEW TOTIENS SET TOTIENS=$PIECE(NODE,"^",4)
        DO SEND^TMGKERN2(LASTIEN_"^"_TOTIENS)
        QUIT
 ;
 ;