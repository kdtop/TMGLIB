TMGXMLE3 ;TMG/kst/XML Exporter -- Core functionality ;10/26/14, 3/17/21
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
 ;"WRIT1FIL(FILE,RECS,FLAGS,INDENTS,SHOWPROG,WRITERS,SAVFIELDINFO)  -- WRITE 1 FILE  
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
WRIT1FIL(FILE,IENS,RECS,FLAGS,INDENTS,SHOWPROG,WRITERS,SAVFIELDINFO)  ;" WRITE 1 FILE  
  ;"Scope: PUBLIC
  ;"Purpose: to dump out (in XML) one file, for specified records
  ;"Input: FILE -- number of file (or subfile) to dump
  ;"       IENS -- OPTIONAL.  Only used if FILE is a subfile. IENS for *PARENT* records.  E.g. "parent-IEN,grandparent-IEN," etc.
  ;"       RECS -- OPTIONAL. PASS BY REFERENCE (default is to WRITE ALL records, all fields)
  ;"          Format is as per documentation in ^TMGXMLE2 **EXCEPT** remove first node (which is file number)
  ;"          Quick summary: RECS(IEN,Field,FieldINFO);  (Default for all is "*")
  ;"                         RECS("TEMPLATE",...  see documentation in ^TMGXMLE2
  ;"       FLAGS -- OPTIONAL
  ;"                    b -- show tags for ALL fields, even IF field has no data
  ;"                    i -- indent tags for pretty, but technically useless, file formating.
  ;"                    I -- output INTERNAL values
  ;"                    D -- include data dictionary for file.
  ;"                    S -- output export settings
  ;"       INDENTS -- OPTIONAL -- current string to WRITE to indent line.
  ;"                    INDENTS("INCINDENT")=INCINDENT
  ;"       SHOWPROG   -- OPTIONAL -- **DONT** pass by reference.  If >0, then a progress bar will be shown.
  ;"       WRITERS -- OPTIONAL -- Array of callback functions for writing output
  ;"            NOTE: All writers will be responsible for ending with linefeed (if they want it).
  ;"         WRITERS("WRITE FILE LABEL FN") -- OPTIONAL -- the name of a custom function to use for writing
  ;"                        actual file starting and ending <file> </file>.  e.g.
  ;"                       "MyCustomFn".  Note DO NOT include parameters.  Function named
  ;"                       as custom function must accept same parameters as WTFILLBL
  ;"         WRITERS("WRITE REC LABEL FN") -- OPTIONAL -- the name of a custom function to use for writing
  ;"                        actual starting and ending <record> </record>.  e.g.
  ;"                       "MyCustomFn".  Note DO NOT include parameters.  Function named
  ;"                       as custom function must accept same parameters as WTRLABEL
  ;"         WRITERS("WRITE FLD LABEL FN") -- OPTIONAL -- the name of a custom function to use for writing
  ;"                       actual line of text out.  e.g. "WTFLABEL" or
  ;"                       "MyCustomFn".  Note DO NOT include parameters.  Function named
  ;"                       as custom function must accept same parameters as WTFLABEL
  ;"         WRITERS("WRITE LINE FN") -- OPTIONAL -- the name of a custom function to use for writing
  ;"                       actual line of text out for fields.  e.g. "WTLINE" or
  ;"                       "MyCustomFn".  Note DO NOT include parameters.  Function named
  ;"                       as custom function must accept same parameters as WTLINE
  ;"        WRITERS("WRITE WP LINE") -- OPTIONAL -- the name of a custom function to use for writing
  ;"                       actual line of text out for WP fields.  If not provided, then
  ;"                       LWRITER will be used instead.
  ;"                       e.g. "WriteWPLINE" or "MyWPCustomFn".  Note DO NOT include parameters.
  ;"                       Function named as custom function must accept same parameters as WTLINE
  ;"       SAVFIELDINFO -- OPTIONAL -- PASS BY REFERENCE.  An array to hold lookup values about
  ;"                fields, so it doesn't have to be done each time (faster)
  ;"Output: RESULTs are written to the current device.
  ;"RESULT : none
  NEW IEN,EXECFN
  NEW PROGCT SET PROGCT=0
  NEW PROGMAX SET PROGMAX=0
  SET FLAGS=$GET(FLAGS)
  SET IENS=$GET(IENS)
  NEW INCINDENT SET INCINDENT=$GET(INDENTS("INCINDENT")," ")
  SET INDENTS=$GET(INDENTS)
  NEW INDS0 MERGE INDS0=INDENTS SET INDS0=$SELECT(FLAGS["i":INDENTS,1:"")
  NEW INDS1 MERGE INDS1=INDENTS SET INDS1=INDS0_$SELECT(FLAGS["i":INCINDENT,1:"")
  NEW INDS2 MERGE INDS2=INDENTS SET INDS2=INDS1_$SELECT(FLAGS["i":INCINDENT,1:"")
  NEW TEMPLATE MERGE TEMPLATE=RECS("TEMPLATE")  ;"typicallly empty
  NEW RECSSPECIFIED SET RECSSPECIFIED=(($DATA(RECS)>1)&($DATA(RECS("*"))=0))
  NEW STARTTIME SET STARTTIME=$H
  NEW WTFILLBL SET WTFILLBL=$GET(WRITERS("WRITE FILE LABEL FN"),"WTFILLBL^TMGXMLE5")
  NEW RWRITER SET RWRITER=$GET(WRITERS("WRITE REC LABEL FN"),"WTRLABEL^TMGXMLE5")
  NEW INTROSHOWN SET INTROSHOWN=0
  ;
  NEW FILENUM SET FILENUM=+$GET(FILE)
  IF FILENUM=0 DO  GOTO WFDN
  . DO SHOWERR^TMGDEBU2(,"Can't convert file '"_$GET(FILE)_", to a number.")
  NEW FNAME SET FNAME=$ORDER(^DD(FILENUM,0,"NM",""))
  NEW ISSUBFILE SET ISSUBFILE=$$ISSUBFIL^TMGFMUT2(FILENUM) 
  SET SHOWPROG=$GET(SHOWPROG)
  IF ISSUBFILE SET SHOWPROG=0
  ;
  NEW OROOT 
  IF ISSUBFILE DO
  . SET OROOT=$$GETGREF^TMGFMUT2(FILENUM,IENS)
  ELSE  DO
  . SET OROOT=$$GET1^DID(FILENUM,"","","GLOBAL NAME") ;" Get global root   (Thanks, Don Donati...)
  NEW GREF SET GREF=$$CREF^DILF(OROOT) ;" Convert open to closed root
  ;
  IF SHOWPROG DO
  . IF RECSSPECIFIED DO
  . . SET PROGMAX=$$LISTCT^TMGMISC2("RECS")
  . ELSE  DO
  . . SET PROGMAX=0
  . . SET IEN=$ORDER(@GREF@("")) ;"count ALL records in file.
  . . FOR  DO  QUIT:(IEN'>0)
  . . . SET IEN=$ORDER(@GREF@(IEN))
  . . . IF +IEN>0 SET PROGMAX=PROGMAX+1
  ;
  SET IEN=0
  SET IEN("FILE-NUMBER")=FILENUM  ;"//kt mod 3/11/21
  FOR  DO  QUIT:(IEN'>0)
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
  . NEW FIELDS MERGE FIELDS=RECS(IEN)
  . IF $DATA(FIELDS)'>1 MERGE FIELDS=TEMPLATE
  . IF $DATA(FIELDS)'>1 SET FIELDS("*")=""
  . IF $DATA(FIELDS("Rec Exclude",IEN)) QUIT  ;"skip excluded records
  . IF INTROSHOWN=0 DO
  . . SET EXECFN="DO "_WTFILLBL_"(.INDS0,"_FILENUM_","""_FNAME_""",0)"
  . . XECUTE EXECFN ;"e.g. '<FILE ...   >'
  . . IF FLAGS["D" DO WRITEDD^TMGXMLE4(FILENUM,FLAGS,.INDS1)  ;"WRITE out data dictionary file
  . . SET INTROSHOWN=1
  . SET EXECFN="DO "_RWRITER_"(.INDS1,.IEN,0)"
  . XECUTE EXECFN  ;"record label writer
  . NEW SUBIENS SET SUBIENS=+IEN_","_IENS
  . DO WRIT1REC^TMGXMLE4(FILENUM,IEN,.FIELDS,.FLAGS,"",SUBIENS,.INDS2,.WRITERS,.SAVFIELDINFO) ;"write out actual record content
  . SET EXECFN="DO "_RWRITER_"(.INDS1,.IEN,1)"
  . XECUTE EXECFN  ;"end record label writer
  . SET PROGCT=PROGCT+1
  . IF SHOWPROG&(PROGCT#2=1) DO
  . . USE $P
  . . DO PROGBAR^TMGUSRI2(PROGCT,"Writing "_FNAME,1,PROGMAX,,STARTTIME)
  . . IF $GET(IO)'="" USE IO
  ;
  IF INTROSHOWN DO
  . SET EXECFN="DO "_WTFILLBL_"(.INDS0,"_FILENUM_","""_FNAME_""",1)"
  . XECUTE EXECFN  ;"e.g. '</FILE>"
  ;
  IF SHOWPROG DO
  . USE $P
  . DO PROGBAR^TMGUSRI2(100,"Writing "_FNAME,1,100)
  . IF $GET(IO)'="" USE IO
  ;
WFDN ;
  QUIT
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