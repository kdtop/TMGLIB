TMGXMLE4 ;TMG/kst/XML Exporter -- Core functionality ;10/26/14
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
 ;"WRIT1REC(FILE,IEN,FIELDS,FLAGS,SREF,IENS,INDENTS,RWRITER,FWRITER,LWRITER,WPLWRITER,SAVFIELDINFO)
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"WRITEDD(FILENUM,FLAGS,INDENTS) ;
 ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;" TMGDBAP3, TMGDEBU2, TMGMISC2, TMGSTUT2, TMGUSRIF, TMGXMLT, MXMLUTL
 ;"=======================================================================
 ;"=======================================================================
 ;
WRIT1REC(FILE,IEN,FIELDS,FLAGS,SREF,IENS,INDENTS,WRITERS,SAVFIELDINFO) ;
        ;"Scope: PUBLIC
        ;"Purpose: To dump one record out in XML format
        ;"Input:   FILE -- number of file to dump
        ;"         IEN -- Record number (IEN) to dump (see also IENS below)
        ;"         FIELDS -- OPTIONAL.  PASS BY REFERENCE.  Array of fields to write, format at follows
        ;"            Format is as per documentation of Array in ^TMGXMLE2 **EXCEPT** ...
        ;"                 except remove 2 node (which are file number and record number)
        ;"            FIELDS(Field,[SUBRECNums,[SUBFIELDS,...]])=""
        ;"            FIELDS(Field,["Rec Exclude",Excluded IEN])=""
        ;"            FIELDS("Field Exclude",ExcludedField)=""                 <-- OPTIONAL
        ;"            FIELDS("ORDER",OrderNUM)=Field                          <-- OPTIONAL
        ;"            FIELDS("TAG NAME",FieldNumber)="Custom field name to put in XML file"  <-- OPTIONAL
        ;"         
        ;"            Example:
        ;"                FIELDS(.01)=""
        ;"                FIELDS(.02)=""
        ;"                FIELDS("NAME")=""  <--- note that field name is allowed in place of number
        ;"                FIELDS(.03)=""
        ;"          
        ;"            Example:
        ;"                FIELDS("*")=""    <--- indicates that ALL fields, ALL subrecs,and ALL subfields are wanted
        ;"          
        ;"            Example:
        ;"                FIELDS("*")=""
        ;"                FIELDS("Field Exclude",.04)=""  <-- don't show field .04
        ;"                FIELDS("Field Exclude","STATE")=""  <-- don't show field "STATE"
        ;"                                              
        ;"            Example: Field .04 is multiple. ALL sub records and ALL subfields to be written
        ;"                FIELDS(.04,"*","*")=""
        ;"                FIELDS(.04,"*")=""  <--- "*" assumed for subfields
        ;"                FIELDS(.04)=""   <-- "*" assumed for subrecords and subfields.
        ;"          
        ;"            Example: Field .03 is multiple. All sub records to be written, and .01 and .02 fields to be written
        ;"                FIELDS(.03,"*",.01)=""  <-- In all sub recs, sub field .01 is to be written
        ;"                FIELDS(.03,"*",.02)=""  <-- In all sub recs, sub field .02 is to be written
        ;"                FIELDS(.03,"Rec Exclude",5)=""  <-- Exclude subrec 5
        ;"          
        ;"            Example: Field .03 is multiple. Sub records 1,2,3 to be written, fields as below
        ;"                FIELDS(.03,1,.01)=""   <-- In sub rec 1, sub field .01 is to be written
        ;"                FIELDS(.03,1,.02)=""   <-- In sub rec 1, sub field .02 is to be written
        ;"                FIELDS(.03,2,.01)=""   <-- In sub rec 2, sub field .01 is to be written
        ;"                FIELDS(.03,3,"*")=""   <-- In sub rec 3, all sub fields are to be written
        ;"                FIELDS(.03,4)=""        <-- In sub rec 4, all sub fields are to be written (defalt)
        ;"                FIELDS(.03,5,"*")=""   <-- In sub rec 5, all sub fields are to be written, with one exception
        ;"                FIELDS(.03,5,"Field Exclude",.01)="" <-- In sub rec 5, sub fields .01 is not to be written.
        ;"          
        ;"             Example:   Shows optional substitution of a NEW tag name for a given field
        ;"                FIELDS("TAG NAME",.01)="Patent Name"  <-- use "Patient Name" instead of field name for .01 field
        ;"                FIELDS("TAG NAME",.02)="City"  <-- use "City" instead of field name for .02 field
        ;"          
        ;"             Example:
        ;"                ARRAY("TRANSFORM",.01)="write ""Custom .01 output transform M code here..."""
        ;"                ARRAY("TRANSFORM",.02)="write ""Custom .02 output transform M code here..."""
        ;"          
        ;"             Note: pattern continues for sub-sub-multiples etc.
        ;"          
        ;"             Example:
        ;"                FIELDS(.01)=""
        ;"                FIELDS(.02)=""
        ;"                FIELDS("NAME")=""  <--- note that field name is allowed in place of number
        ;"                FIELDS(.03,1,.01)=""   <-- In sub rec 1, sub field .01 is to be written
        ;"                FIELDS(.03,1,.02)=""   <-- In sub rec 1, sub field .02 is to be written
        ;"                FIELDS(.03,2,.01)=""   <-- In sub rec 2, sub field .01 is to be written
        ;"                FIELDS(.03,3,"*")=""   <-- In sub rec 3, all sub fields are to be written
        ;"                FIELDS(.03,4)=""        <-- In sub rec 4, all sub fields are to be written (defalt)
        ;"                FIELDS("ORDER",1)=.03            <-- 1st field to output
        ;"                FIELDS("ORDER",2)=.02            <-- 2nd field to output
        ;"                FIELDS("ORDER",3)="NAME"    <-- 3rd field to output
        ;"                FIELDS("ORDER",4)=.01            <-- 4th field to output
        ;"                Note: Specifying an 'ORDER' is not compatible with specifying "*" fields
        ;"                        If "ORDER" is specified, only fields with a given order will be output
        ;"                        Both Field("ORDER",x)=FieldNum *AND* Field(FieldNUM)="" should be defined
        ;"                                This will be primarily important for fields that are multiples, with sub recs.
        ;"         FLAGS -- OPTIONAL
        ;"                 b -- show tags for fields, even IF field has no data
        ;"                 i -- indent tags for pretty, but technically useless, file formating.
        ;"                 I -- output INTERNAL values
        ;"         SREF -- OPTIONAL (Used only when calling self recursively)
        ;"         IENS -- OPTIONAL a standard IENS string
        ;"                         e.g. "IEN,parent-IEN,grandparent-IEN," etc.
        ;"                         This is used when calling self recursively, to handle subfiles
        ;"         INDENTS -- OPTIONAL -- current string to WRITE to indent line.
        ;"         WRITERS -- OPTIONAL -- Array of callback functions for writing output. FORMAT as per WRITE1FIL^TMGXML3
        ;"         SAVFIELDINFO -- OPTIONAL -- PASS BY REFERENCE.  An array to hold lookup values about
        ;"                           fields, so it doesn't have to be done each time (faster)
        ;"Output: Values are written to the current device
        ;"Results: None
        ;"Note: this code began its life as a function written by Greg Woodhouse (thanks Greg!)
        IF $DATA(FIELDS)<10 SET FIELDS("*")=""
        NEW ALLFIELDS SET ALLFIELDS=($DATA(FIELDS("*"))>0)
        NEW ORDFIELDS,ORDINDEX SET ORDFIELDS=0,ORDINDEX=0
        IF $ORDER(FIELDS("ORDER"))>1 SET ALLFIELDS=0,ORDFIELDS=1
        NEW FILENUM SET FILENUM=+$GET(FILE)
        IF FILENUM=0 DO  GOTO WRDN
        . DO SHOWERR^TMGDEBU2(,"Can't convert file '"_$GET(FILE)_", to a number.")
        IF $GET(IENS)="" SET IENS=IEN_","
        NEW FIELD SET FIELD=0
        NEW LASTFILENAME SET LASTFILENAME=FIELD
        DO FRCNUMIC(.FIELDS) ;"Force field names into numeric values. 
        ;
        FOR  DO  QUIT:(+FIELD'>0)
        . IF ALLFIELDS DO
        . . SET FIELD=$ORDER(^DD(FILENUM,FIELD))
        . ELSE  IF ORDFIELDS DO  QUIT:(FIELD="")
        . . SET ORDINDEX=$ORDER(FIELDS("ORDER",ORDINDEX))
        . . SET FIELD=$GET(FIELDS("ORDER",ORDINDEX))
        . ELSE  DO  QUIT:(+FIELD'>0)
        . . SET FIELD=$ORDER(FIELDS(LASTFILENAME))
        . SET LASTFILENAME=FIELD
        . IF +FIELD=0 SET FIELD=$$FLDNUM^DILFD(FILENUM,FIELD)
        . IF $DATA(FIELDS("Field Exclude",FIELD))>0 QUIT
        . IF +FIELD=0 QUIT
        . DO WRIT1FLD^TMGXMLE5(FILENUM,IEN,FIELD,.FIELDS,.FLAGS,.SREF,.IENS,.INDENTS,.WRITERS,.SAVFIELDINFO)
        ;
WRDN    QUIT
        ;
WRITEDD(FILENUM,FLAGS,INDENTS) ;
        ;"Scope: PRIVATE
        ;"Purpose: to WRITE out data dictionary file, ^DIC,and file Header in XML format
        ;"Input: FILENUM -- the file number (not name) of the data dictionary to export
        ;"       FLAGS -- flags as declared above.  Only "i" used here
        ;"       INDENTS -- OPTIONAL -- current string to WRITE to indent line.
        ;"NOTE: Uses GLOBAL SCOPED INCINDENT variable.  But setting this is OPTIONAL.
        ;"Results: none
        NEW PROGFN
        USE $P WRITE ! IF $GET(IO)'="" USE IO
        SET INCINDENT=$GET(INCINDENT,"  ")
        ;
        SET PROGFN="USE $P DO PROGBAR^TMGUSRI2(INCVAR,""^DD("_FILENUM_")"",0,100000,,"""_$H_""") USE IO"
        DO WRITEARR^TMGXMLT($NAME(^DD(FILENUM)),"DataDictionary",FILENUM,.FLAGS,.INDENTS,.INCINDENT,.PROGFN)
        ;
        SET PROGFN="USE $P DO PROGBAR^TMGUSRI2(INCVAR,""^(DIC("_FILENUM_")"",0,1000000,,"""_$H_""") USE IO"
        NEW DIC ;"Pull just the fileman nodes.  ^DIC also contains some full files...
        MERGE DIC(FILENUM,0)=^DIC(FILENUM,0)
        MERGE DIC(FILENUM,"%")=^DIC(FILENUM,"%")
        MERGE DIC(FILENUM,"%A")=^DIC(FILENUM,"%A")
        MERGE DIC(FILENUM,"%D")=^DIC(FILENUM,"%D")
        DO WRITEARR^TMGXMLT("DIC("_FILENUM_")","DIC_FILE",FILENUM,.FLAGS,.INDENTS,.INCINDENT,.PROGFN)
        ;
        DO
        . NEW REF SET REF=$GET(^DIC(FILENUM,0,"GL"))
        . SET REF=$$CREF^DILF(REF) ;" Convert open to closed root
        . IF $GET(FLAGS)["i" WRITE INDENTS
        . WRITE "<FILE_HEADER id=""",FILENUM,""">",!
        . IF $GET(FLAGS)["i" WRITE INDENTS
        . WRITE $GET(@REF@(0)),!
        . IF $GET(FLAGS)["i" WRITE INDENTS
        . WRITE "</FILE_HEADER>",!
        ;
        ;"USE $P WRITE ! USE IO
        QUIT
        ;
FRCNUMIC(FIELDS) ;"Force field names into numeric values. 
  ;"Ensure all text exclusion fields are converted to numeric ones.
  IF $DATA(FIELDS("Field Exclude"))>0 DO
  . NEW FIELD
  . SET FIELD=$ORDER(FIELDS("Field Exclude",""))
  . IF FIELD'="" FOR  DO  QUIT:(FIELD="")
  . . IF +FIELD'=FIELD DO
  . . . NEW TEMPFIELD
  . . . SET TEMPFIELD=$$FLDNUM^DILFD(FILENUM,FIELD)
  . . . SET FIELDS("Field Exclude",TEMPFIELD)=""
  . . SET FIELD=$ORDER(FIELDS("Field Exclude",FIELD))
  ;
  ;"Ensure all custom tag field names are converted to numeric ones.
  IF $DATA(FIELDS("TAG NAME"))>0 DO
  . NEW FIELD
  . SET FIELD=$ORDER(FIELDS("TAG NAME",""))
  . IF FIELD'="" FOR  DO  QUIT:(FIELD="")
  . . IF +FIELD'=FIELD DO
  . . . NEW TEMPFIELD
  . . . SET TEMPFIELD=$$FLDNUM^DILFD(FILENUM,FIELD)
  . . . SET FIELDS("TAG NAME",TEMPFIELD)=FIELDS("TAG NAME",FIELD)
  . . SET FIELD=$ORDER(FIELDS("TAG NAME",FIELD))
  ;
  ;"Ensure all custom TRANSFORM field names are converted to numeric ones.
  IF $DATA(FIELDS("TRANSFORM"))>0 DO
  . NEW FIELD
  . SET FIELD=$ORDER(FIELDS("TRANSFORM",""))
  . IF FIELD'="" FOR  DO  QUIT:(FIELD="")
  . . IF +FIELD'=FIELD DO
  . . . NEW TEMPFIELD
  . . . SET TEMPFIELD=$$FLDNUM^DILFD(FILENUM,FIELD)
  . . . SET FIELDS("TRANSFORM",TEMPFIELD)=FIELDS("TRANSFORM",FIELD)
  . . SET FIELD=$ORDER(FIELDS("TRANSFORM",FIELD))
  QUIT