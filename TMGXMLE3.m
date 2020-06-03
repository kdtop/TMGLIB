TMGXMLE3 ;TMG/kst/XML Exporter -- Core functionality ;10/26/14
         ;;1.0;TMG-LIB;**1**;07/12/05
 ;
 ;"TMG XML EXPORT FUNCTIONS (CORE FUNCTIONALITY)
 ;"Kevin Toppenberg MD
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
 ;"WRIT1FIL(FILE,RECS,FLAGS,INDENTS,SAVFIELDINFO)
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"WRTSTNGS(FLAGS,INDENTS)  -- WRITE settings
 ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;" TMGDBAP3, TMGDEBU2, TMGMISC2, TMGSTUT2, TMGUSRIF, TMGXMLT, MXMLUTL
 ;"=======================================================================
 ;"=======================================================================
 ;
WRIT1FIL(FILE,RECS,FLAGS,INDENTS,TEMPLATE,SHOWPROG,RWRITER,FWRITER,LWRITER,WPLWRITER,SAVFIELDINFO)  ;
        ;"Scope: PUBLIC
        ;"Purpose: to dump out (in XML) one file, for specified records
        ;"Input: FILE -- name or number of file to dump
        ;"       RECS -- OPTIONAL. PASS BY REFERENCE (default is to WRITE ALL records)
        ;"          To specify records to WRITE out, use RECS array with following format:
        ;"          -------------------------------------------------------------------
        ;"          RECS(IEN,Field,FieldINFO);  (Default for all is "*")
        ;"                For format of FieldINFO, see function WRIT1REC
        ;"          RECS("Rec Exclude",IEN) <-- exclude IEN from output
        ;"          -------------------------------------------------------------------
        ;"          Example:
        ;"                  RECS(1231)=""
        ;"                  RECS(1232)=""
        ;"                  RECS(1234)=""            this would be used to print records 1231,1232,1234
        ;"                  RECS(1232)="tag=value^tag2=value2" <-- optional extra info for record
        ;"                      e.g. <Record id=1232 tag="value" tag2="value2">
        ;"
        ;"           Example:  For ALL records, output ALL fields, and ALL subfields
        ;"                   RECS("*")=""   <--- this is default IF RECS is not specified/passed
        ;"           Example:  For all records, output fields .01 and .02 and "NAME"
        ;"                   RECS("*",.01)=""
        ;"                   RECS("*",.02)=""
        ;"                   RECS("*","NAME")=""
        ;"           Example:  For record 1231, output fields .01 and .02
        ;"                          For record 1232, output field .01 only
        ;"                          For record 1234, output field "NAME" only
        ;"                          For record 1235, output ALL fields
        ;"                   RECS(1231,.01)=""
        ;"                   RECS(1231,.02)=""
        ;"                   RECS(1232,.01)=""
        ;"                   RECS(1234,"NAME")=""
        ;"                   RECS(1235,"*")=""
        ;"           Example:  For all records, output field "ENTRY", which is a multiple.  In
        ;"                           subfile, output records .01, and .02
        ;"                   RECS("*","ENTRY",.01)=""
        ;"                   RECS("*","ENTRY",.02)=""
        ;"           Example:  For ALL records, output ALL fields, and ALL subfields, with 2 exceptions
        ;"                   RECS("*")=""
        ;"                   RECS("Rec Exclude",1234)=""  <-- All records except 1234 & 1235 will be output
        ;"                   RECS("Rec Exclude",1235)=""
        ;"       FLAGS -- OPTIONAL
        ;"                    b -- show tags for ALL fields, even IF field has no data
        ;"                    i -- indent tags for pretty, but technically useless, file formating.
        ;"                    I -- output INTERNAL values
        ;"                    D -- include data dictionary for file.
        ;"                    S -- output export settings
        ;"       INDENTS -- OPTIONAL -- current string to WRITE to indent line.
        ;"                    INDENTS("INCINDENT")=INCINDENT
        ;"       TEMPLATE -- OPTIONAL.  PASS BY REFERENCE
        ;"                    This can be used for instances where the same SET of fields are desired for
        ;"                    multiple records.
        ;"                    Example:
        ;"                      RECS(1231)=""
        ;"                      RECS(1232)=""
        ;"                      RECS(1234)=""
        ;"                      with  TEMPLATE(.01)=""
        ;"                              TEMPLATE(.02)=""
        ;"                      Is the same as specifying:
        ;"                      RECS(1231,.01)=""
        ;"                      RECS(1231,.02)=""
        ;"                      RECS(1232,.01)=""
        ;"                      RECS(1232,.02)=""
        ;"                      RECS(1234,.01)=""
        ;"                      RECS(1234,.02)=""
        ;"       SHOWPROG   -- OPTIONAL -- IF >0, then a progress bar will be shown.
        ;"       RWRITER -- OPTIONAL -- the name of a custom function to use for writing
        ;"                actual starting and ending <record> </record>.  e.g.
        ;"                "MyCustomFn".  Note DO NOT include parameters.  Function named
        ;"                as custom function must accept same parameters as WTRLABEL
        ;"       FWRITER -- OPTIONAL -- the name of a custom function to use for writing
        ;"                actual line of text out.  e.g. "WTFLABEL" or
        ;"                "MyCustomFn".  Note DO NOT include parameters.  Function named
        ;"       LWRITER -- OPTIONAL -- the name of a custom function to use for writing
        ;"                actual line of text out for WP fields.  e.g. "WTLINE" or
        ;"                "MyCustomFn".  Note DO NOT include parameters.  Function named
        ;"                as custom function must accept same parameters as WTLINE
        ;"                as custom function must accept same parameters as WTFLABEL
        ;"       WPLWRITER -- OPTIONAL -- the name of a custom function to use for writing
        ;"                actual line of text out for WP fields.  If not provided, then
        ;"                LWRITER will be used instead.
        ;"                e.g. "WriteWPLINE" or "MyWPCustomFn".  Note DO NOT include parameters.
        ;"                Function named as custom function must accept same parameters as WTLINE
        ;"       SAVFIELDINFO -- OPTIONAL -- PASS BY REFERENCE.  An array to hold lookup values about
        ;"                fields, so it doesn't have to be done each time (faster)
        ;"Output: RESULTs are written to the current device.
        ;"RESULT : none
        NEW OROOT,GREF
        NEW FILENUM,FNAME
        NEW PROGCT SET PROGCT=0
        NEW PROGMAX
        NEW INCINDENT SET INCINDENT=$GET(INDENTS("INCINDENT")," ")
        IF $DATA(TEMPLATE)=0 SET TEMPLATE("*")=""
        NEW RECSSPECIFIED SET RECSSPECIFIED=(($DATA(RECS)>1)&($DATA(RECS("*"))=0))
        NEW KEYIN SET KEYIN=32
        NEW STARTTIME SET STARTTIME=$H
        SET RWRITER=$GET(RWRITER,"WTRLABEL^TMGXMLE5")
        SET INDENTS=$GET(INDENTS)
        ;
        SET FILENUM=+$GET(FILE)
        IF FILENUM=0 DO
        . SET FILENUM=$$GETFNUM^TMGXMLT2(.FILE)
        . SET FNAME=FILE
        ELSE  DO
        . SET FNAME=$ORDER(^DD(FILENUM,0,"NM",""))
        IF FILENUM=0 DO  GOTO WFDN
        . DO SHOWERR^TMGDEBU2(,"Can't convert file '"_$GET(FILE)_", to a number.")
        ;
        SET OROOT=$$GET1^DID(FILENUM,"","","GLOBAL NAME") ;" Get global root   (Thanks, Don Donati...)
        SET GREF=$$CREF^DILF(OROOT) ;" Convert open to closed root
        ;
        IF $GET(SHOWPROG) DO
        . IF RECSSPECIFIED DO
        . . SET PROGMAX=$$LISTCT^TMGMISC2("RECS")
        . ELSE  DO
        . . SET PROGMAX=0
        . . SET IEN=$ORDER(@GREF@("")) ;"count ALL records in file.
        . . FOR  DO  QUIT:(IEN'>0)
        . . . SET IEN=$ORDER(@GREF@(IEN))
        . . . IF +IEN>0 SET PROGMAX=PROGMAX+1
        ;
        SET FLAGS=$GET(FLAGS)
        IF FLAGS["i" WRITE INDENTS
        WRITE "<FILE id=""",FILENUM,""" label=""",$$SYMENC^MXMLUTL(FNAME),""">",!
        ;
        IF FLAGS["D" DO WRITEDD^TMGXMLE4(FILENUM,FLAGS,INDENTS_INCINDENT)  ;"WRITE out data dictionary file
        ;
        NEW INDS2 SET INDS2=INDENTS_INCINDENT
        NEW IEN SET IEN=0
        FOR  DO  QUIT:(IEN'>0)
        . IF $DATA(FIELDS)'>1 SET FIELDS("*")=""
        . IF RECSSPECIFIED DO
        . . SET IEN=$ORDER(RECS(IEN))  ;"Cycle through specified records
        . . NEW EXTRA SET EXTRA=$GET(RECS(IEN))
        . . IF EXTRA'="" DO  ;"parse extra info into IEN array for output
        . . . NEW STR,N,TAG,VALUE
        . . . FOR N=1:1:$LENGTH(EXTRA,"^") DO
        . . . . SET STR=$PIECE(EXTRA,"^",N)
        . . . . IF STR'["=" QUIT
        . . . . SET TAG=$PIECE(STR,"=",1)
        . . . . SET VALUE=$PIECE(STR,"=",2)
        . . . . SET IEN(TAG)=VALUE
        . ELSE  DO
        . . SET IEN=$ORDER(@GREF@(IEN)) ;"Cycle through ALL records in file.
        . IF (IEN'>0) QUIT
        . IF $DATA(RECS("Rec Exclude",IEN)) QUIT  ;"skip excluded records
        . NEW FIELDS MERGE FIELDS=RECS(IEN)
        . IF $DATA(FIELDS)'>1 MERGE FIELDS=TEMPLATE
        . IF $GET(FLAGS)["i" WRITE $GET(INDS2)
        . NEW EXECFN SET EXECFN="DO "_RWRITER_"(.IEN,0)"
        . XECUTE EXECFN
        . IF $DATA(TMGXDEBUG) DO
        . . USE $P
        . . WRITE "Writing record: ",IEN,"  PROGCT=",PROGCT," PROGMAX=",PROGMAX,!
        . . USE IO
        . DO WRIT1REC^TMGXMLE4(FILENUM,IEN,.FIELDS,.FLAGS,"","",INDS2_INCINDENT,.RWRITER,.FWRITER,.LWRITER,.WPLWRITER,.SAVFIELDINFO)
        . IF $GET(FLAGS)["i" WRITE $GET(INDS2)
        . SET EXECFN="DO "_RWRITER_"(.IEN,1)"
        . XECUTE EXECFN
        . SET PROGCT=PROGCT+1
        . IF $GET(SHOWPROG)&(PROGCT#2=1) DO
        . . USE $P
        . . DO PROGBAR^TMGUSRI2(PROGCT,"Writing "_FNAME,1,PROGMAX,,STARTTIME)
        . . USE IO
        . ;"USE $P READ *KEYIN USE IO
        . IF KEYIN=27 DO
        . . NEW ABORT
        . . USE $P
        . . WRITE PROGCT," records written so far...",!
        . . WRITE !,"Do you want to abort XML export? NO// "
        . . READ ABORT:$GET(DTIME,3600),!
        . . IF ABORT="" SET ABORT="NO"
        . . IF "YESyesYes"[ABORT SET IEN=0  ;"ABORT signal
        . . WRITE "OK.  Continuing...",!
        . . USE IO
        ;
        IF $GET(FLAGS)["i" WRITE INDENTS
        WRITE "</FILE>",!
        ;
        IF $GET(SHOWPROG) DO
        . USE $P
        . DO PROGBAR^TMGUSRI2(100,"Writing "_FNAME,1,100)
        . USE IO
        ;
WFDN    QUIT
        ; 
WRTSTNGS(FLAGS,INDENTS)  ;"WRITE settings
        ;"Scope: PRIVATE
        ;"Purpose: to output XML output settings.
        ;"Input: FLAGS -- flags as declared above.  Only "i" used here
        ;"       INDENTS -- OPTIONAL -- current string to WRITE to indent line.
        ;"          INDENTS("INCINDENT")=INCINDENT
        ;"NOTE: Uses GLOBAL SCOPED INCINDENT variable.  But setting this is OPTIONAL.
        ;"Results: none
        SET INDENTS=$GET(INDENTS)
        SET FLAGS=$GET(FLAGS)
        NEW INCINDENT SET INCINDENT=$GET(INDENTS("INCINDENT")," ")
        ;
        IF FLAGS["i" WRITE INDENTS
        WRITE "<ExportSettings>",!
        ;
        NEW FARRAY,FL
        SET FARRAY("i")="Indent_Output"
        SET FARRAY("b")="Output_Blanks"
        SET FARRAY("I")="Output_Internal_Values"
        SET FARRAY("D")="Output_Data_Dictionary"
        ;
        SET FL=""
        FOR  SET FL=$ORDER(FARRAY(FL)) QUIT:(FL="")  DO
        . IF FLAGS["i" WRITE INDENTS_INCINDENT
        . WRITE "<Setting id=""",$$SYMENC^MXMLUTL($GET(FARRAY(FL))),""">"
        . WRITE $SELECT((FLAGS[FL):"TRUE",1:"FALSE")
        . WRITE "</Setting>",!
        ;
        IF FLAGS["i" WRITE INDENTS
        WRITE "</ExportSettings>",!
        ;
        QUIT
        ;
