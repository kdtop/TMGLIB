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
 ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;" TMGDBAP3, TMGDEBU2, TMGMISC2, TMGSTUT2, TMGUSRIF, TMGXMLT, MXMLUTL
 ;"=======================================================================
 ;"=======================================================================
 ;
WRIT1FLD(FILENUM,IEN,FIELD,FIELDS,FLAGS,SREF,IENS,INDENTS,WRITERS,SAVFIELDINFO) ;
  ;"Scope: PUBLIC
  ;"Purpose: To dump one field out in XML format
  ;"Input: FILENUM -- number of file containing field
  ;"       IEN -- Record number (IEN) to dump (see also IENS below).  Ignored IF IENS supplied
  ;"       FIELD -- The field number to WRITE from array below.
  ;"       FIELDS -- The field to write.
  ;"       FLAGS -- OPTIONAL.  FORMAT as per WRITE1FIL^TMGXML3
  ;"       SREF -- OPTIONAL (Used only when calling self recursively)
  ;"       IENS -- OPTIONAL a standard IENS string
  ;"                       e.g. "IEN,parent-IEN,grandparent-IEN," etc.
  ;"                       This is used when calling self recursively, to handle subfiles
  ;"                       Late Note: IF IENS is supplied, then IEN is ignored
  ;"       INDENTS -- OPTIONAL -- current string to WRITE to indent line.
  ;"       WRITERS -- OPTIONAL -- Array of callback functions for writing output. FORMAT as per WRITE1FIL^TMGXML3
  ;"       SAVFIELDINFO -- OPTIONAL -- PASS BY REFERENCE.  An array to hold lookup values about
  ;"                        fields, so it doesn't have to be done each time (faster)
  ;"Output: Values are written to the current device
  ;"Results: None
  ;"Note: this code began its life as a function written by Greg Woodhouse (thanks Greg!)
  NEW FLDTYPE,LABEL
  NEW FIELDINFO,TMGXMLPROPS,EXECFN,EXECFN2,EXECFN3
  ;
  IF $GET(IENS)="" SET IENS=IEN_","
  IF +$GET(FIELD)=0 GOTO W1FDN
  NEW RWRITER SET RWRITER=$GET(WRITERS("WRITE REC LABEL FN"),"WTRLABEL^TMGXMLE5")
  NEW FWRITER SET FWRITER=$GET(WRITERS("WRITE FLD LABEL FN"),"WTFLABEL^TMGXMLE5")
  NEW LWRITER SET LWRITER=$GET(WRITERS("WRITE LINE FN"),"WTLINE^TMGXMLE5")
  SET FLAGS=$GET(FLAGS)
  IF $GET(INDENTS("INCINDENT"))="" SET INDENTS("INCINDENT")=" "
  NEW INCINDENT SET INCINDENT=$SELECT(FLAGS["i":INDENTS("INCINDENT"),1:"")
  NEW INDS0 MERGE INDS0=INDENTS SET INDS0=$GET(INDENTS),INDS0("INCINDENT")=INCINDENT
  NEW INDS1 MERGE INDS1=INDENTS SET INDS1=INDS0_INCINDENT,INDS1("INCINDENT")=INCINDENT
  NEW INDS2 MERGE INDS2=INDENTS SET INDS2=INDS1_INCINDENT,INDS2("INCINDENT")=INCINDENT
  ;
  IF $DATA(SAVFIELDINFO(FILENUM,FIELD))>0 DO
  . MERGE FIELDINFO=SAVFIELDINFO(FILENUM,FIELD)
  ELSE  DO
  . DO GFLDINFO^TMGXMLT2(FILENUM,FIELD,"FIELDINFO","LABEL")
  . MERGE SAVFIELDINFO(FILENUM,FIELD)=FIELDINFO
  ;
  ;"SET FLDTYPE=FIELDINFO("SPECIFIER")
  SET FLDTYPE=FIELDINFO("TYPE")
  IF $DATA(FIELDS("TAG NAME",FIELD))#10>1 SET LABEL=FIELDS("TAG NAME",FIELD)
  ELSE  SET LABEL=FIELDINFO("LABEL")
  ;                             
  IF $GET(FIELDINFO("MULTIPLE-VALUED"))=1 DO
  . IF $GET(FIELDINFO("TYPE"))="WORD-PROCESSING" DO
  . . DO WPWRITER(FILENUM,IENS,FIELD,FLAGS,.INDS0,.WRITERS) ;
  . ELSE  DO   ;"Other multiple (subfile)
  . . NEW SUBFILE SET SUBFILE=+FIELDINFO("SPECIFIER")
  . . IF $$HASREC^TMGDBAP3(SUBFILE,IENS)=0 QUIT
  . . NEW TEMPFIELD SET TEMPFIELD=$SELECT(ALLFIELDS:"*",1:LASTFILENAME)
  . . NEW TMGXMLPROPS
  . . ;"IF FLDTYPE="POINTER" SET FLDTYPE="SUBFILE",TMGXMLPROPS("SUBFILE-NUMBER")=SUBFILE
  . . SET FLDTYPE="SUBFILE",TMGXMLPROPS("SUBFILE-NUMBER")=SUBFILE
  . . SET EXECFN="DO "_FWRITER_"(.INDS0,"""_$$QTPROTCT^TMGSTUT3(LABEL)_""","""_$$QTPROTCT^TMGSTUT3(FIELD)_""","""_FLDTYPE_""",0,.TMGXMLPROPS)"
  . . XECUTE EXECFN  ;"write field label
  . . NEW TEMPINDS MERGE TEMPINDS=INDS1
  . . SET TEMPINDS("LABEL_END_XPOS")=$X                   ;"label writers can use if they want. 
  . . SET TEMPINDS("LABEL")=LABEL
  . . WRITE !  ;"! so first subrec will be on new line.
  . . NEW RECS SET RECS("*")=""       
  . . ;"NOTE: This should theoretically be set to get chosed subrecs from ARRAY (as per ^TMGXMLE2)... but I'm not using this functionality right now...
  . . DO WRIT1FIL^TMGXMLE3(SUBFILE,IENS,.RECS,FLAGS,.TEMPINDS,0,.WRITERS,.SAVFIELDINFO)  ;"0 = no progress    
  ELSE  DO  ;"the usual case here...
  . NEW LINE SET LINE=""
  . NEW CUSTXFORM SET CUSTXFORM=$GET(FIELDS("TRANSFORM",FIELD))
  . IF CUSTXFORM'="" DO
  . . NEW POS,GREF,NODE
  . . NEW FILE,FIELD,X,Y
  . . NEW INTVALUE SET INTVALUE=""
  . . NEW OROOT
  . . IF $GET(SREF)'="" SET OROOT=SREF
  . . ELSE  SET OROOT=$$GETGREF^TMGFMUT2(FILENUM,IEN) 
  . . ;"ELSE  SET OROOT=$GET(^DIC(FILENUM,0,"GL"))
  . . IF OROOT="" QUIT
  . . SET NODE=$PIECE($GET(FIELDINFO("STORELOC")),";",1)
  . . IF NODE="" DO  QUIT   ;"Computed field, use FM code
  . . . SET LINE=$$GET1^DIQ(FILENUM,IENS,FIELD)  
  . . IF (+NODE'=NODE) SET NODE=""""_NODE_""""  ;" enclose text indices with quotes
  . . SET POS=$PIECE($GET(FIELDINFO("STORELOC")),";",2)
  . . ;"SET GREF=OROOT_IEN_","_NODE_")"
  . . SET GREF=OROOT_","_NODE_")"
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
  . . IF $GET(FIELDINFO("SPECIFIER"))["Cm" DO  QUIT ;"//kt 3/17/21  Computed multiline
  . . . NEW TMGOUT DO GETS^DIQ(FILENUM,IENS,FIELD,"","TMGOUT")
  . . . SET LINE=""
  . . . NEW IDX SET IDX=0
  . . . FOR  SET IDX=$ORDER(TMGOUT(FILENUM,IENS,FIELD,IDX)) QUIT:IDX'>0  DO
  . . . . NEW VALUE SET VALUE=$GET(TMGOUT(FILENUM,IENS,FIELD,IDX)) QUIT:VALUE=""
  . . . . IF LINE'="" SET LINE=LINE_$CHAR(13,10)
  . . . . SET LINE=LINE_VALUE
  . . ;"the usual case.  
  . . NEW $ETRAP SET $ETRAP="SET LINE=""<Error reading FILE# ""_$g(FILENUM)_"", IENS=""_$g(IENS)_"", FIELD=""_$g(FIELD)_"">"" SET $ETRAP="""",$ecode="""""
  . . SET LINE=$$GET1^DIQ(FILENUM,IENS,FIELD,GETFLAG)
  . . IF ($GET(FIELDINFO("TYPE"))["POINTER"),(LINE'="") DO
  . . . NEW IEN SET IEN=$$GET1^DIQ(FILENUM,IENS,FIELD,"I")
  . . . ;"NEW P2FILE SET P2FILE=+$PIECE($GET(FIELDINFO("SPECIFIER")),"P",2)
  . . . NEW P2FILE SET P2FILE=$GET(FIELDINFO("SPECIFIER"))
  . . . IF P2FILE="MV",FIELDINFO("TYPE")="VARIABLE-POINTER" DO
  . . . . NEW P2FREF SET P2FREF="^"_$PIECE(IEN,";",2),IEN=+IEN
  . . . . SET P2FILE=+$GET(FIELDINFO("VARIABLE-POINTER","GL",P2FREF))
  . . . ELSE  IF P2FILE="RV",FIELDINFO("TYPE")="VARIABLE-POINTER" DO
  . . . . NEW P2FREF SET P2FREF="^"_$PIECE(IEN,";",2),IEN=+IEN
  . . . . SET P2FILE=$GET(FIELDINFO("VARIABLE-POINTER","GL",P2FREF))
  . . . . IF P2FILE'>0 DO
  . . . . . NEW ARR DO GL2FIL^TMGFMUT(P2FREF,.ARR)
  . . . . . NEW IDX SET IDX=""
  . . . . . FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:(IDX="")!(P2FILE>0)  DO
  . . . . . . NEW INFO SET INFO=$GET(ARR(IDX)) QUIT:INFO="" 
  . . . . . . IF $PIECE(INFO,";",2)'=P2FREF QUIT
  . . . . . . SET P2FILE=+INFO
  . . . . . . SET FIELDINFO("VARIABLE-POINTER","GL",P2FREF)=P2FILE        
  . . . ELSE  SET P2FILE=+$PIECE($GET(FIELDINFO("SPECIFIER")),"P",2)
  . . . IF (FLAGS["p"),($GET(FIELDINFO("TYPE"))["POINTER"),(LINE'="") DO
  . . . . SET LINE=LINE_" (`"_IEN_" in #"_P2FILE_")"
  . . . SET TMGXMLPROPS("pointed-to-file")=P2FILE
  . . . SET TMGXMLPROPS("pointed-to-record")=IEN
  . IF (LINE="")&(FLAGS'["b") QUIT
  . SET EXECFN="DO "_FWRITER_"(.INDS0,"""_$$QTPROTCT^TMGSTUT3(LABEL)_""","""_$$QTPROTCT^TMGSTUT3(FIELD)_""","""_FIELDINFO("TYPE")_""",0,.TMGXMLPROPS)"
  . NEW INDS1 MERGE INDS1=INDENTS SET INDS1=INDS0_INCINDENT,INDS1("INCINDENT")=INCINDENT ;"note: Some writers will modify INDS0, and need to carry changes forward
  . SET EXECFN2="DO "_LWRITER_"(.INDS1,.LINE)"
  . SET EXECFN3="DO "_FWRITER_"(.INDS0,"""_$$QTPROTCT^TMGSTUT3(LABEL)_""","""_$$QTPROTCT^TMGSTUT3(FIELD)_""","""_FIELDINFO("TYPE")_""",1)"
  . XECUTE EXECFN   ;"Field opening label
  . XECUTE EXECFN2  ;"WRITE LINE
  . XECUTE EXECFN3  ;"Field closing label
  ;
W1FDN  ;
  QUIT
  ;
WPWRITER(FILENUM,IENS,FIELD,FLAGS,INDENTS,WRITERS) ;" Write out content of a WP field. 
  ;
  NEW FWRITER SET FWRITER=$GET(WRITERS("WRITE FLD LABEL FN"),"WTFLABEL^TMGXMLE5")
  NEW LWRITER SET LWRITER=$GET(WRITERS("WRITE LINE FN"),"WTLINE^TMGXMLE5")
  NEW WPLWRITER SET WPLWRITER=$GET(WRITERS("WRITE WP LINE"),"WWPLINE^TMGXMLE5")
  NEW INDS0 MERGE INDS0=INDENTS SET INDS0=$SELECT(FLAGS["i":INDENTS,1:"")
  NEW INDS1 MERGE INDS1=INDENTS SET INDS1=INDS0_$SELECT(FLAGS["i":INCINDENT,1:"")
  NEW TMGWP,TMGMSG,RESULT,EXECFN,TMGXMLPROPS
  SET RESULT=$$READWP^TMGXMLT2(FILENUM,IENS,FIELD,.TMGWP)
  IF +RESULT'=1 KILL TMGWP
  NEW IDX SET IDX=$ORDER(TMGWP(""))
  IF (IDX=""),(FLAGS'["b") QUIT
  SET EXECFN="DO "_FWRITER_"(.INDS0,LABEL,"""_$$QTPROTCT^TMGSTUT3(FIELD)_""",""WORD-PROCESSING"",0,.TMGXMLPROPS)"
  XECUTE EXECFN 
  WRITE !  ;"! is so first <LINE> will be on a separate line
  IF IDX="" QUIT                         
  FOR  DO  QUIT:(IDX="")
  . NEW LINE SET LINE=$GET(TMGWP(IDX))
  . SET LINE=$$STRIPCMD^TMGSTUT3(LINE)  ;"shouldn't be needed!!! ??GT.M bug??
  . SET EXECFN="DO "_WPLWRITER_"(.INDS1,"""_$$QTPROTCT^TMGSTUT3(LINE)_""")"
  . XECUTE EXECFN
  . SET IDX=$ORDER(TMGWP(IDX))
  SET EXECFN="DO "_FWRITER_"(.INDS0,LABEL,"""_$$QTPROTCT^TMGSTUT3(FIELD)_""",""WORD-PROCESSING"",1)"
  XECUTE EXECFN
  QUIT
  ;
WTFILLBL(INDENTS,FILENUM,FILENAME,ENDER,PROPS) ;"WRITE FILE LABEL
  ;"Purpose: This is the code that actually does writing of labels etc for file output
  ;"Input: FILENUM -- Number of file, to WRITE after  'id='
  ;"       FILENAME -- OPTIONAL -- Name of FILE, to WRITE after  'label='
  ;"       ENDER -- OPTIONAL IF 1, then ends field.
  ;"       PROPS -- OPTIONAL, PASS BY REFERENCE.  Format:
  ;"           PROPS(<property name>)=<property value>
  ;"Results: none.
  ;"Note: This is a separate function so that a different callback function can replace it
  ;
  WRITE $GET(INDENTS)
  IF +$GET(ENDER)>0 DO
  . WRITE "</FILE>",!
  ELSE  DO
  . WRITE "<FILE "
  . IF $GET(FILENUM)'="" WRITE "id=""",$$SYMENC^MXMLUTL(FILENUM),""" "
  . IF $GET(FILENAME)'="" WRITE "label=""",$$SYMENC^MXMLUTL(FILENAME),""" "
  . NEW APROP SET APROP=""
  . FOR  SET APROP=$ORDER(PROPS(APROP)) QUIT:APROP=""  DO
  . . WRITE $$SYMENC^MXMLUTL(APROP),"=""",$$SYMENC^MXMLUTL($GET(PROPS(APROP))),""" "
  . WRITE ">",!
  ;
  QUIT
  ;  
WTRLABEL(INDENTS,IEN,ENDER) ;"WRITE RECORD LABEL
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
  IF +$GET(ENDER)>0 DO
  . WRITE $GET(INDENTS)
  . WRITE "</Record>",!
  ELSE  DO
  . WRITE $GET(INDENTS)
  . WRITE "<Record id=""",IEN,""" "
  . NEW TAG SET TAG=""
  . FOR  SET TAG=$ORDER(IEN(TAG)) QUIT:(TAG="")  DO
  . . WRITE TAG,"=""",$GET(IEN(TAG)),""" "
  . WRITE ">",!
  QUIT
  ;
WTFLABEL(INDENTS,LABEL,FIELD,TYPE,ENDER,PROPS) ;"WRITE FIELD LABEL
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
  NEW XPOS SET XPOS=$X
  IF +$GET(ENDER)>0 DO
  . IF XPOS<4 WRITE $GET(INDENTS)
  . WRITE "</Field>",!
  ELSE  DO
  . WRITE $GET(INDENTS)
  . WRITE "<Field "
  . IF $GET(FIELD)'="" WRITE "id=""",$$SYMENC^MXMLUTL(FIELD),""" "
  . IF $GET(LABEL)'="" WRITE "label=""",$$SYMENC^MXMLUTL(LABEL),""" "
  . IF $GET(TYPE)'="" WRITE "type=""",$$SYMENC^MXMLUTL(TYPE),""" "
  . NEW APROP SET APROP=""
  . FOR  SET APROP=$ORDER(PROPS(APROP)) QUIT:APROP=""  DO
  . . WRITE APROP,"=""",$GET(PROPS(APROP)),""" "
  . WRITE ">"
  . ;"WRITE !
  ;
  QUIT
  ;
WTLINE(INDENTS,LINE) ;"WRITE LINE VALUE
  ;"Purpose: This is the code that actually does writing of labels etc for output
  ;"Input: LINE -- the line of text to WRITE out.
  ;"Results: none
  ;"Note: This is a separate function so that a different callback function can replace it
  SET LINE=$$SYMENC^MXMLUTL(LINE)
  ;"WRITE $GET(INDENTS)
  ;"WRITE "<LINE>",LINE,"</LINE>",!
  WRITE LINE
  QUIT
  ;
WWPLINE(INDENTS,LINE) ;"WRITE A WP LINE VALUE
  ;"Purpose: This is the code that actually does writing of labels etc for output
  ;"Input: LINE -- the line of text to WRITE out.
  ;"Results: none
  ;"Note: This is a separate function so that a different callback function can replace it
  SET LINE=$$SYMENC^MXMLUTL(LINE)
  WRITE $GET(INDENTS)
  WRITE "<WPLINE>",LINE,"</WPLINE>",!
  QUIT
  ;
  