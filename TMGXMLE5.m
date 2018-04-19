TMGXMLE5 ;TMG/kst/XML Exporter -- Core functionality ;10/26/14, 6/12/17
         ;;1.0;TMG-LIB;**1**;07/12/05
 ;
 ;"TMG XML EXPORT FUNCTIONS (CORE FUNCTIONALITY)
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
 ;"WRIT1FLD(FILENUM,IEN,FIELD,FIELDS,FLAGS,SREF,IENS,INDENTS,RWRITER,FWRITER,LWRITER,WPLWRITER,SAVFIELDINFO)
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"WTRLABEL(IEN,ENDER) -- WRITE out labels for record starting and ending.
 ;"WTFLABEL(LABEL,FIELD,TYPE,ENDER,PROPS) -- WRITE of labels etc for output
 ;"WTLINE(LINE) -- WRITE writing of labels etc for output
 ;"CVTLABEL(LABEL)  --Convert label
 ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;" TMGDBAP3, TMGDEBU2, TMGMISC2, TMGSTUT2, TMGUSRIF, TMGXMLT, MXMLUTL
 ;"=======================================================================
 ;"=======================================================================
 ;
WRIT1FLD(FILENUM,IEN,FIELD,FIELDS,FLAGS,SREF,IENS,INDENTS,RWRITER,FWRITER,LWRITER,WPLWRITER,SAVFIELDINFO) ;
        ;"Scope: PUBLIC
        ;"Purpose: To dump one field out in XML format
        ;"Input: FILENUM -- number of file containing field
        ;"       IEN -- Record number (IEN) to dump (see also IENS below).  Ignored IF IENS supplied
        ;"       FIELD -- The field number to WRITE from array below.
        ;"       FIELDS -- The field to write.
        ;"       FLAGS -- OPTIONAL
        ;"               b -- show tags for fields, even IF field has no data
        ;"               i -- indent tags for pretty, but technically useless, file formating.
        ;"               I -- output INTERNAL values
        ;"               p -- for Pointers, show record number after name, e.g. DOE,JOHN (`123)
        ;"       SREF -- OPTIONAL (Used only when calling self recursively)
        ;"       IENS -- OPTIONAL a standard IENS string
        ;"                       e.g. "IEN,parent-IEN,grandparent-IEN," etc.
        ;"                       This is used when calling self recursively, to handle subfiles
        ;"                       Late Note: IF IENS is supplied, then IEN is ignored
        ;"       INDENTS -- OPTIONAL -- current string to WRITE to indent line.
        ;"       RWRITER -- OPTIONAL -- the name of a custom function to use for writing
        ;"                        actual starting and ending <record> </record>.  e.g.
        ;"                       "MyCustomFn".  Note DO NOT include parameters.  Function named
        ;"                       as custom function must accept same parameters as WTRLABEL
        ;"       FWRITER -- OPTIONAL -- the name of a custom function to use for writing
        ;"                       actual line of text out.  e.g. "WTFLABEL" or
        ;"                       "MyCustomFn".  Note DO NOT include parameters.  Function named
        ;"                       as custom function must accept same parameters as WTFLABEL
        ;"       LWRITER -- OPTIONAL -- the name of a custom function to use for writing
        ;"                       actual line of text out for WP fields.  e.g. "WTLINE" or
        ;"                       "MyCustomFn".  Note DO NOT include parameters.  Function named
        ;"                       as custom function must accept same parameters as WTLINE
        ;"       WPLWRITER -- OPTIONAL -- the name of a custom function to use for writing
        ;"                       actual line of text out for WP fields.  If not provided, then
        ;"                       LWRITER will be used instead.
        ;"                       e.g. "WriteWPLINE" or "MyWPCustomFn".  Note DO NOT include parameters.
        ;"                       Function named as custom function must accept same parameters as WTLINE
        ;"       SAVFIELDINFO -- OPTIONAL -- PASS BY REFERENCE.  An array to hold lookup values about
        ;"                        fields, so it doesn't have to be done each time (faster)
        ;"Output: Values are written to the current device
        ;"Results: None
        ;"Note: this code began its life as a function written by Greg Woodhouse (thanks Greg!)
        NEW FLDTYPE,LABEL
        NEW FIELDINFO,TMGXMLPROPS
        ;
        IF $GET(IENS)="" SET IENS=IEN_","
        IF +$GET(FIELD)=0 GOTO W1FDN
        SET FWRITER=$GET(FWRITER,"WTFLABEL")
        SET RWRITER=$GET(RWRITER,"WTRLABEL")
        SET LWRITER=$GET(LWRITER,"WTLINE")
        SET WPLWRITER=$GET(WPLWRITER,LWRITER)
        SET FLAGS=$GET(FLAGS)
        ;
        IF 1=1 DO
        . IF $DATA(SAVFIELDINFO(FILENUM,FIELD))>0 DO
        . . MERGE FIELDINFO=SAVFIELDINFO(FILENUM,FIELD)
        . ELSE  DO
        . . DO GFLDINFO^TMGXMLT2(FILENUM,FIELD,"FIELDINFO","LABEL")
        . . MERGE SAVFIELDINFO(FILENUM,FIELD)=FIELDINFO
        ELSE  IF 1=0 DO
        . ;"try to get info directly to speed things up.... FINISH LATER
        . NEW NODE SET NODE=$GET(^DD(FILENUM,FIELD,0))
        . SET FIELDINFO("SPECIFIER")=$PIECE(NODE,"^",2)
        . SET FIELDINFO("LABEL")=$PIECE(NODE,"^",1)
        . SET FIELDINFO("MULTIPLE-VALUED")=(+FIELDINFO("SPECIFIER")>0)
        . IF FIELDINFO("SPECIFIER")["W" SET FIELDINFO("TYPE")="WORD-PROCESSING"
        . ELSE  IF FIELDINFO("SPECIFIER")["D" SET FIELDINFO("TYPE")="DATE"
        . ELSE  IF FIELDINFO("SPECIFIER")["F" SET FIELDINFO("TYPE")="FREE TEXT"
        . ELSE  IF FIELDINFO("SPECIFIER")["P" SET FIELDINFO("TYPE")="POINTER"
        . ELSE  IF FIELDINFO("SPECIFIER")["N" SET FIELDINFO("TYPE")="NUMERIC"
        . ELSE  IF FIELDINFO("SPECIFIER")["S" SET FIELDINFO("TYPE")="SET"
        . ELSE  SET FIELDINFO("TYPE")=FIELDINFO("SPECIFIER")
        ;
        SET FLDTYPE=FIELDINFO("SPECIFIER")
        IF $DATA(FIELDS("TAG NAME",FIELD))#10>1 SET LABEL=FIELDS("TAG NAME",FIELD)
        ELSE  SET LABEL=FIELDINFO("LABEL")
        ;
        IF $GET(FIELDINFO("MULTIPLE-VALUED"))=1 DO
        . IF $GET(FIELDINFO("TYPE"))="WORD-PROCESSING" DO
        . . NEW TMGWP,TMGMSG,RESULT
        . . SET RESULT=$$READWP^TMGXMLT2(FILENUM,IENS,FIELD,.TMGWP)
        . . IF +RESULT'=1 KILL TMGWP
        . . NEW IDX SET IDX=$ORDER(TMGWP(""))
        . . IF (IDX=""),(FLAGS'["b") QUIT
        . . IF FLAGS["i" WRITE $GET(INDENTS)
        . . NEW EXECFN SET EXECFN="DO "_FWRITER_"(LABEL,"""_$$QTPROTCT^TMGSTUT3(FIELD)_""","""_FIELDINFO("TYPE")_""",0,.TMGXMLPROPS)"
        . . XECUTE EXECFN
        . . WRITE !  ;"so first <LINE> will be on a separate line
        . . IF IDX="" QUIT
        . . FOR  DO  QUIT:(IDX="")
        . . . NEW LINE SET LINE=$GET(TMGWP(IDX))
        . . . SET LINE=$$STRIPCMD^TMGSTUT3(LINE)  ;"shouldn't be needed!!! ??GT.M bug??
        . . . IF FLAGS["i" WRITE $GET(INDENTS)_INCINDENT
        . . . SET EXECFN="DO "_WPLWRITER_"("""_$$QTPROTCT^TMGSTUT3(LINE)_""")"
        . . . ;"WRITE EXECFN,!
        . . . XECUTE EXECFN
        . . . SET IDX=$ORDER(TMGWP(IDX))
        . . IF FLAGS["i" WRITE $GET(INDENTS)
        . . SET EXECFN="DO "_FWRITER_"(LABEL,"""_$$QTPROTCT^TMGSTUT3(FIELD)_""","""_FIELDINFO("TYPE")_""",1)"
        . . XECUTE EXECFN
        . ELSE  DO   ;"Other multiple (subfile)
        . . SET SUBFILE=+FLDTYPE
        . . NEW ALLSUBRECS,TEMPFIELD
        . . NEW OROOT,NODE
        . . IF $GET(SREF)'="" SET OROOT=SREF
        . . ;"ELSE  SET OROOT=$GET(^DIC(FILENUM,0,"GL"))
        . . ELSE  SET OROOT=$$GETGREF^TMGFMUT2(FILENUM,IENS)  ;"7/7/13
        . . IF OROOT="" QUIT
        . . IF ALLFIELDS SET TEMPFIELD="*"
        . . ELSE  SET TEMPFIELD=LASTFILENAME
        . . SET ALLSUBRECS=($DATA(FIELDS(TEMPFIELD,"*"))>0)!($ORDER(FIELDS(TEMPFIELD,""))="")
        . . SET NODE=$PIECE($GET(FIELDINFO("STORELOC")),";",1)
        . . IF NODE="" QUIT   ;"skip computed fields
        . . IF (+NODE'=NODE) SET NODE=""""_NODE_""""  ;" enclose text indices with quotes
        . . SET SROOT=OROOT_IEN_","_NODE_","  ;"open root
        . . SET CROOT=OROOT_IEN_","_NODE_")" ;"closed root
        . . SET SUBREC=$ORDER(@CROOT@(0))
        . . IF (SUBREC'="")!(FLAGS["b") DO
        . . . IF FLAGS["i" WRITE $GET(INDENTS)
        . . . NEW EXECFN SET EXECFN="DO "_FWRITER_"("""_$$QTPROTCT^TMGSTUT3(LABEL)_""","""_$$QTPROTCT^TMGSTUT3(FIELD)_""","""_FIELDINFO("TYPE")_""",0,.TMGXMLPROPS)"
        . . . XECUTE EXECFN
        . . . WRITE !
        . . . NEW INDS2 SET INDS2=$GET(INDENTS)_INCINDENT
        . . . IF +SUBREC>0 FOR  DO  QUIT:+SUBREC'>0
        . . . . ;"descend into subfile (if allowed subrecord #)
        . . . . IF (ALLSUBRECS)!($DATA(FIELDS(TEMPFIELD,SUBREC))>0) DO
        . . . . . IF $DATA(FIELDS(TEMPFIELD,"Rec Exclude",SUBREC))>0 QUIT
        . . . . . NEW SUBIENS,SUBFIELDS,TEMPSR
        . . . . . IF ALLSUBRECS SET TEMPSR="*"
        . . . . . ELSE  SET TEMPSR=SUBREC
        . . . . . SET SUBIENS=SUBREC_","_IENS
        . . . . . MERGE SUBFIELDS=FIELDS(TEMPFIELD,TEMPSR)
        . . . . . IF (ALLFIELDS)!($DATA(SUBFIELDS)=0) SET SUBFIELDS("*")=""
        . . . . . IF FLAGS["i" WRITE $GET(INDS2)
        . . . . . NEW EXECFN SET EXECFN="DO "_RWRITER_"("_$$QTPROTCT^TMGSTUT3(SUBREC)_",0)"
        . . . . . XECUTE EXECFN
        . . . . . DO WRIT1REC^TMGXMLE4(SUBFILE,SUBREC,.SUBFIELDS,FLAGS,SROOT,SUBIENS,INDS2_INCINDENT,.RWRITER,.FWRITER,.LWRITER,.WPLWRITER,.SAVFIELDINFO)
        . . . . . IF FLAGS["i" WRITE $GET(INDS2)
        . . . . . NEW EXECFN SET EXECFN="DO "_RWRITER_"("_$$QTPROTCT^TMGSTUT3(SUBREC)_",1)"
        . . . . . XECUTE EXECFN
        . . . . SET SUBREC=$ORDER(@CROOT@(SUBREC))
        . . . IF FLAGS["i" WRITE $GET(INDENTS)
        . . . SET EXECFN="DO "_FWRITER_"("""_$$QTPROTCT^TMGSTUT3(LABEL)_""","""_$$QTPROTCT^TMGSTUT3(FIELD)_""","""_FIELDINFO("TYPE")_""",1)"
        . . . XECUTE EXECFN
        ELSE  DO  ;"the usual case here...
        . NEW LINE SET LINE=""
        . NEW CUSTXFORM SET CUSTXFORM=$GET(FIELDS("TRANSFORM",FIELD))
        . IF CUSTXFORM'="" DO
        . . NEW POS,GREF,NODE
        . . NEW FILE,FIELD,X,Y
        . . NEW INTVALUE SET INTVALUE=""
        . . IF $GET(SREF)'="" SET OROOT=SREF
        . . ELSE  SET OROOT=$GET(^DIC(FILENUM,0,"GL"))
        . . IF OROOT="" QUIT
        . . SET NODE=$PIECE($GET(FIELDINFO("STORELOC")),";",1)
        . . IF NODE="" QUIT   ;"skip computed fields
        . . IF (+NODE'=NODE) SET NODE=""""_NODE_""""  ;" enclose text indices with quotes
        . . SET POS=$PIECE($GET(FIELDINFO("STORELOC")),";",2)
        . . SET GREF=OROOT_IEN_","_NODE_")"
        . . IF +POS>0 SET INTVALUE=$PIECE($GET(@GREF),"^",POS)
        . . ;"Set up variables for use by transform code
        . . SET FILE=FILENUM
        . . SET FIELD=+FIELD
        . . SET X=INTVALUE
        . . SET Y=""
        . . NEW $ETRAP SET $ETRAP="SET Y=""(Invalid custom transform M code!.  Error Trapped.)"" SET $ETRAP="""",$ecode="""""
        . . XECUTE CUSTXFORM
        . . SET LINE=Y
        . ELSE  DO
        . . NEW GETFLAG SET GETFLAG=""
        . . IF FLAGS["I" SET GETFLAG="I"
        . . DO
        . . . NEW $ETRAP SET $ETRAP="SET LINE=""<Error reading FILE# ""_$g(FILENUM)_"", IENS=""_$g(IENS)_"", FIELD=""_$g(FIELD)_"">"" SET $ETRAP="""",$ecode="""""
        . . . SET LINE=$$GET1^DIQ(FILENUM,IENS,FIELD,GETFLAG)
        . . . IF ($GET(FIELDINFO("TYPE"))["POINTER"),(LINE'="") DO
        . . . . NEW IEN SET IEN=$$GET1^DIQ(FILENUM,IENS,FIELD,"I")
        . . . . ;"NEW P2FILE SET P2FILE=+$PIECE($GET(FIELDINFO("SPECIFIER")),"P",2)
        . . . . NEW P2FILE SET P2FILE=$GET(FIELDINFO("SPECIFIER"))
        . . . . IF P2FILE="MV",FIELDINFO("TYPE")="VARIABLE-POINTER" DO
        . . . . . NEW P2FREF SET P2FREF="^"_$PIECE(IEN,";",2),IEN=+IEN
        . . . . . SET P2FILE=+$GET(FIELDINFO("VARIABLE-POINTER","GL",P2FREF))
        . . . . ELSE  SET P2FILE=+$PIECE($GET(FIELDINFO("SPECIFIER")),"P",2)
        . . . . IF (FLAGS["p"),($GET(FIELDINFO("TYPE"))["POINTER"),(LINE'="") DO
        . . . . . SET LINE=LINE_" (`"_IEN_" in #"_P2FILE_")"
        . . . . SET TMGXMLPROPS("pointed-to-file")=P2FILE
        . . . . SET TMGXMLPROPS("pointed-to-record")=IEN
        . IF (LINE="")&(FLAGS'["b") QUIT
        . IF FLAGS["i" WRITE $GET(INDENTS)
        . NEW EXECFN SET EXECFN="DO "_FWRITER_"("""_$$QTPROTCT^TMGSTUT3(LABEL)_""","""_$$QTPROTCT^TMGSTUT3(FIELD)_""","""_FIELDINFO("TYPE")_""",0,.TMGXMLPROPS)"
        . XECUTE EXECFN
        . SET EXECFN="DO "_LWRITER_"(.LINE)"
        . XECUTE EXECFN   ;"WRITE LINE
        . IF FLAGS["i" WRITE $GET(INDENTS)
        . SET EXECFN="DO "_FWRITER_"("""_$$QTPROTCT^TMGSTUT3(LABEL)_""","""_$$QTPROTCT^TMGSTUT3(FIELD)_""","""_FIELDINFO("TYPE")_""",1)"
        . XECUTE EXECFN
        ;
W1FDN   QUIT
        ;
WTRLABEL(IEN,ENDER) ;
        ;"Purpose: To actually WRITE out labels for record starting and ending.
        ;"      IEN -- the IEN (record number) of the record
        ;"              Optional extra informat:
        ;"              IEN(tag)=value
        ;"              IEN(tag2)=value2
        ;"              If provided, will be added to output as follows:
        ;"              <Record id="IEN" tag="value" tag2="value2">
        ;"      ENDER -- OPTIONAL IF 1, then ends field.
        ;"Results: none.
        ;"Note: This is a separate function so that a different callback function can replace it
        IF +$GET(ENDER)>0 WRITE "</Record>",!
        ELSE  DO
        . WRITE "<Record id=""",IEN,""" "
        . NEW TAG SET TAG=""
        . FOR  SET TAG=$ORDER(IEN(TAG)) QUIT:(TAG="")  DO
        . . WRITE TAG,"=""",$GET(IEN(TAG)),""" "
        . WRITE ">",!
        QUIT
        ;
WTFLABEL(LABEL,FIELD,TYPE,ENDER,PROPS) ;
        ;"Purpose: This is the code that actually does writing of labels etc for output
        ;"Input: LABEL -- OPTIONAL -- Name of label, to WRITE after  'label='
        ;"       FIELD -- OPTIONAL -- Name of field, to WRITE after  'id='
        ;"       TYPE -- OPTIONAL -- Type of field, to WRITE after  'type='
        ;"       ENDER -- OPTIONAL IF 1, then ends field.
        ;"       PROPS -- OPTIONAL, PASS BY REFERENCE.  Format:
        ;"           PROPS(<property name>)=<property value>
        ;"Results: none.
        ;"Note: This is a separate function so that a different callback function can replace it
        ;
        ;"To WRITE out <FIELD label="NAME" id=".01" type="FREE TEXT"> or </FIELD>
        ;
        IF +$GET(ENDER)>0 DO
        . WRITE "</Field>",!
        ELSE  DO
        . WRITE "<Field "
        . IF $GET(FIELD)'="" WRITE "id=""",$$SYMENC^MXMLUTL(FIELD),""" "
        . IF $GET(LABEL)'="" WRITE "label=""",$$SYMENC^MXMLUTL(LABEL),""" "
        . IF $GET(TYPE)'="" WRITE "type=""",$$SYMENC^MXMLUTL(TYPE),""" "
        . NEW APROP SET APROP=""
        . FOR  SET APROP=$ORDER(PROPS(APROP)) QUIT:APROP=""  DO
        . . WRITE APROP,"=""",$GET(PROPS(APROP)),""" "
        . WRITE ">"
        ;
        QUIT
        ;
WTLINE(LINE) ;
        ;"Purpose: This is the code that actually does writing of labels etc for output
        ;"Input: LINE -- the line of text to WRITE out.
        ;"Results: none
        ;"Note: This is a separate function so that a different callback function can replace it
        SET LINE=$$SYMENC^MXMLUTL(LINE)
        WRITE "<LINE>",LINE,"</LINE>",!
        QUIT
        ;
CVTLABEL(LABEL)  ;"Convert label
        ;"Note: This function is no longer being used...
        ;"To convert the XML tag into an acceptible format for XML
        ;"
        NEW IDX
        NEW RESULT SET RESULT=""
        FOR IDX=1:1:$LENGTH(LABEL) DO
        . NEW CH SET CH=$ASCII($EXTRACT(LABEL,IDX))
        . IF ((CH>64)&(CH<91))!((CH>96)&(CH<123)) DO  QUIT
        . . SET RESULT=RESULT_$CHAR(CH)
        . IF (CH=32) SET RESULT=RESULT_"_"
        . ELSE  DO
        . . SET RESULT=RESULT_"x"
        QUIT RESULT
        ;   
