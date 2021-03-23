TMGXMLE2 ;TMG/kst/XML Exporter -- Core functionality ;10/26/14, 3/4/21  
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
 ;"WTXMLOUT(PARRAY,FLAGS,INDENTS)
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;" TMGDBAP3, TMGDEBU2, TMGMISC2, TMGSTUT2,  MXMLUTL
 ;"=======================================================================
 ;"=======================================================================
 ;
 ;"-----------------------------------------------------------------------------------------------
 ;"-----------------------------------------------------------------------------------------------
 ;"  NOTES: The basic array format is to be as follows:
 ;   
 ;"  ARRAY(FILE,Record,FIELD,subRec,SubFIELD...)=""   <--- means export this entry
 ;"  ARRAY(FILE,IEN,FieldINFO)   ; For FieldINFO, see WRIT1FIL, and WRIT1REC
 ;"  ARRAY(FILE,"TEMPLATE",FIELD)
 ;"  ARRAY(FILE,"TEMPLATE","ORDER",OrderNUM)=FIELD
 ;"  ARRAY(FILE,"TEMPLATE","TAG NAME",FIELDNumber)="Custom field name to put in XML file"
 ;"  ARRAY("FLAGS","b")=""  b -- show tags for ALL fields, even if field has no data
 ;"  ARRAY("FLAGS","i")=""  i -- indent tags for pretty, but technically useless, file formating.
 ;"  ARRAY("FLAGS","I")=""  I -- output INTERNAL values
 ;"  ARRAY("FLAGS","D")=""  D -- output the data dictionary
 ;"  ARRAY("!DOCTYPE")=MyLABEL
 ;"  ARRAY("EXPORT_SYSTEM_NAME")=LABELForExportingSystem   -- OPTIONAL
 ;"  
 ;"  Note: Field numbers can be replaced with full field NAMES, e.g.
 ;"        But FILES must be specified by NUMBER
 ;"  
 ;"  Example:  For ALL records, output ALL fields, and ALL subfields
 ;"       ARRAY(8925,"*")=""   <--- this is default if RECS is not specified/passed
 ;"  
 ;"  Example: to print from:
 ;"     file 8925, records 1234,1235,1236,1237
 ;"     file 200, ALL records
 ;"     file 22705, records 3,5
 ;"     file 2, ALL records
 ;"  
 ;"     ARRAY(8925,1234)=""
 ;"     ARRAY(8925,1235)=""
 ;"     ARRAY(8925,1236)=""
 ;"     ARRAY(8925,1237)=""
 ;"     ARRAY(200,"*")=""
 ;"     ARRAY(22705,3)=""
 ;"     ARRAY(22705,5)=""
 ;"     ARRAY(2,"*")=""
 ;"  
 ;"  Example:  
 ;"     For record 1231, output fields .01 and .02
 ;"       For record 1232, output field .01 only
 ;"       For record 1234, output field "NAME" only
 ;"       For record 1235, output ALL fields
 ;"       For record 1236, output ALL fields
 ;"     ARRAY(8925,1231,.01)=""
 ;"     ARRAY(8925,1231,.02)=""
 ;"     ARRAY(8925,1232,.01)=""
 ;"     ARRAY(8925,1234,"NAME")=""
 ;"     ARRAY(8925,1235,"*")=""
 ;"     ARRAY(8925,1236)=""
 ;"  
 ;"  Example:  Output extra info in record node
 ;"     ARRAY(8925,1232)="TAG=value^TAG2=value2" <-- optional extra info for record
 ;"       e.g. -->  <Record id=1232 TAG="value" TAG2="value2">
 ;"  
 ;"  Example:
 ;"     ARRAY(8925,"TEMPLATE",.01)=""   <-- define a template for file 8925, with fields .01,.02,.03
 ;"     ARRAY(8925,"TEMPLATE",.02)=""
 ;"     ARRAY(8925,"TEMPLATE",.03)=""
 ;"     ARRAY(8925,1234)   <-- print record 1234  (will use the template)
 ;"     ARRAY(8925,1235)   <-- print record 1235
 ;"  
 ;"  Example:
 ;"     ARRAY(8925,"TEMPLATE","*"))=""  <-- include all fields in template
 ;"     ARRAY(8925,"TEMPLATE","Field Exclude",.04)=""   <-- but exclude field .04
 ;"     ARRAY(8925,1235)   <-- print record 1235, all fields but .04
 ;"  
 ;"  Example:  For all records, output fields .01 and .02 and "NAME"
 ;"      ARRAY(8925,"*",.01)=""
 ;"      ARRAY(8925,"*",.02)=""
 ;"      ARRAY(8925,"*","NAME")=""
 ;"  
 ;"  Example:
 ;"      ARRAY(8925,1231,"*")=""    <--- indicates that ALL fields, ALL subrecs,and ALL subfields are wanted
 ;"  
 ;"  Example:  For all records, output field named "ENTRY", which is a multiple.  In
 ;"              subfile, output all records, fields  .01, and .02
 ;"      ARRAY(8925,"*","ENTRY","*",.01)=""
 ;"      ARRAY(8925,"*","ENTRY","*",.02)=""
 ;"  
 ;"  Example:  For ALL records, output ALL fields, and ALL subfields, with 2 exceptions
 ;"      ARRAY(8925,"Rec Exclude",1234)=""  <-- All records except 1234 & 1235 will be output
 ;"      ARRAY(8925,"Rec Exclude",1235)=""
 ;"      ARRAY(8925,"*")=""
 ;"  
 ;"  Example:
 ;"      ARRAY(8925,"TEMPLATE","Field Exclude",.04)=""  <-- don't show field .04
 ;"      ARRAY(8925,"TEMPLATE","Field Exclude","STATE")=""  <-- don't show field "STATE"
 ;"      ARRAY(8925,1231,"*")=""   <-- in record 1231, show all fields but .04 and "STATE"
 ;"  
 ;"  Example: Field .04 is multiple. ALL sub records and ALL subfields to be written
 ;"      ARRAY(8925,1231,.04,"*","*")=""
 ;"      ARRAY(8925,1231,.04,"*")=""  <--- "*" assumed for subfields
 ;"      ARRAY(8925,1231,.04)=""   <-- "*" assumed for subrecords and subfields.
 ;"  
 ;"  Example: Field .03 is multiple. All sub records to be written (except for #5) , and .01 and .02 fields to be written
 ;"      ARRAY(8925,1231,.03,"*",.01)=""  <-- In all sub recs, sub field .01 is to be written
 ;"      ARRAY(8925,1231,.03,"*",.02)=""  <-- In all sub recs, sub field .02 is to be written
 ;"      ARRAY(8925,1231,.03,"Rec Exclude",5)=""  <-- Exclude subrec 5
 ;"  
 ;"  Example: Field .03 is multiple. All sub records to be written, and .01 and .02 fields to be written
 ;"      ARRAY(8925,1231,"TEMPLATE",.03,"*","TEMPLATE",.01)=""  <-- In all sub recs, sub field .01 is to be written
 ;"      ARRAY(8925,1231,"TEMPLATE",.03,"*","TEMPLATE",.02)=""  <-- In all sub recs, sub field .02 is to be written
 ;   
 ;"  Example: Field .03 is multiple. Sub records 1,2,3 to be written, fields as below
 ;"      ARRAY(8925,1231,.03,1,.01)=""   <-- In sub rec 1, sub field .01 is to be written
 ;"      ARRAY(8925,1231,.03,1,.02)=""   <-- In sub rec 1, sub field .02 is to be written
 ;"      ARRAY(8925,1231,.03,2,.01)=""   <-- In sub rec 2, sub field .01 is to be written
 ;"      ARRAY(8925,1231,.03,3,"*")=""   <-- In sub rec 3, all sub fields are to be written
 ;"      ARRAY(8925,1231,.03,4)=""        <-- In sub rec 4, all sub fields are to be written (defalt)
 ;"      ARRAY(8925,1231,.03,5,"*")=""   <-- In sub rec 5, all sub fields are to be written, with one exception
 ;"      ARRAY(8925,1231,.03,5,"Field Exclude",.01)="" <-- In sub rec 5, sub fields .01 is not to be written.
 ;"  
 ;"  Example:   Shows optional substitution of a NEW TAG name for a given field
 ;"     ARRAY(8925,"TEMPLATE","TAG NAME",.01)="Patent Name"  <-- use "Patient Name" instead of field name for .01 field
 ;"     ARRAY(8925,"TEMPLATE","TAG NAME",.02)="City"  <-- use "City" instead of field name for .02 field
 ;"  
 ;"  Note: pattern continues for sub-sub-multiples etc.
 ;"  
 ;"  Example:
 ;"     ARRAY(8925,1231,.01)=""
 ;"     ARRAY(8925,1231,.02)=""
 ;"     ARRAY(8925,1231,"NAME")=""  <--- note that field name is allowed in place of number
 ;"     ARRAY(8925,1231,.03,1,.01)=""   <-- In sub rec 1, sub field .01 is to be written
 ;"     ARRAY(8925,1231,.03,1,.02)=""   <-- In sub rec 1, sub field .02 is to be written
 ;"     ARRAY(8925,1231,.03,2,.01)=""   <-- In sub rec 2, sub field .01 is to be written
 ;"     ARRAY(8925,1231,.03,3,"*")=""   <-- In sub rec 3, all sub fields are to be written
 ;"     ARRAY(8925,1231,.03,4)=""        <-- In sub rec 4, all sub fields are to be written (defalt)
 ;"  
 ;"  Example:  Field .03 is a multiple
 ;"     ARRAY(8925,1231,.03,"TEMPLATE",.01)=""
 ;"     ARRAY(8925,1231,.03,"TEMPLATE",.02)=""
 ;"     ARRAY(8925,1231,.03,1)=""   <-- In sub rec 1, export fields .01,.02 from template
 ;"     ARRAY(8925,1231,.03,2)=""   <-- In sub rec 2, export fields .01,.02 from template
 ;"     ARRAY(8925,1231,.03,4)=""   <-- In sub rec 4, export fields .01,.02 from template
 ;"  
 ;"  Example:
 ;"    ARRAY(8925,"TEMPLATE","ORDER",1)=.03            <-- 1st field to output
 ;"    ARRAY(8925,"TEMPLATE","ORDER",2)=.02            <-- 2nd field to output
 ;"    ARRAY(8925,"TEMPLATE","ORDER",3)="NAME"    <-- 3rd field to output
 ;"    ARRAY(8925,"TEMPLATE","ORDER",4)=.01            <-- 4th field to output
 ;"    Note: Specifying an 'ORDER' is not compatible with specifying "*" fields
 ;"            If "ORDER" is specified, only fields with a given order will be output
 ;"            Both Field("ORDER",x)=FieldNum *AND* Field(FieldNUM)="" should be defined
 ;"                    This will be primarily important for fields that are multiples, with sub recs.
 ;"  
 ;"  Example:
 ;"    ARRAY(8925,"TEMPLATE","TRANSFORM",.01)="write ""Custom .01 output transform M code here..."""
 ;"    ARRAY(8925,"TEMPLATE","TRANSFORM",.02)="write ""Custom .02 output transform M code here..."""
 ;   
 ;"-----------------------------------------------------------------------------------------------
 ;"-----------------------------------------------------------------------------------------------
 ;
 ;
WTXMLOUT(PARRAY,FLAGS,INDENTS,SHOWPROG)  ;
  ;"Scope: PUBLIC
  ;"Purpose: to dump out a specified set of files and records in XML Format
  ;"         Output is to current output stream (to the console if not otherwise set via USE command)
  ;"Input: PARRAY -- pointer to (i.e. name of) array containing formatting/output info.
  ;"          REQUIRED An array specifying which files and records to display
  ;"          Format as per examples and documentation above.
  ;"       FLAGS -- OPTIONAL  e.g. FLAGS="b"  or "bi"  or "ib"  or "iI" etc.
  ;"                    b -- show tags for ALL fields, even IF field has no data
  ;"                    i -- indent tags for pretty, but technically useless, file formating.
  ;"                    I -- output INTERNAL values
  ;"                    D -- output Data dictionary
  ;"                    p -- for Pointers, show record number after name, e.g. DOE,JOHN (`123)
  ;"                (Note FLAGS can also be specified with a "FLAGS" node in @PARRAY)
  ;"       INDENTS -- OPTIONAL -- current string to WRITE to indent line.
  ;"                  INDENTS("INCINDENT")=INCINDENT -- amount to increase each with each deeper level. 
  ;"       SHOWPROG -- OPTIONAL -- IF =1, then a progress bar will be shown.
  ;"Output: RESULTs are written to the current device.
  ;"RESULT : none
  ;
  NEW FILE,SAVFIELDINFO
  NEW TARRAY MERGE TARRAY=@PARRAY
  SET FLAGS=$GET(FLAGS)
  NEW INCINDENT SET INCINDENT=$GET(INDENTS("INCINDENT")," ")
  IF ($DATA(TARRAY("FLAGS","b"))>0)&(FLAGS'["b") SET FLAGS=FLAGS_"b"
  IF ($DATA(TARRAY("FLAGS","i"))>0)&(FLAGS'["i") SET FLAGS=FLAGS_"i"
  IF ($DATA(TARRAY("FLAGS","I"))>0)&(FLAGS'["I") SET FLAGS=FLAGS_"I"
  IF ($DATA(TARRAY("FLAGS","D"))>0)&(FLAGS'["D") SET FLAGS=FLAGS_"D"                                  
  IF ($DATA(TARRAY("FLAGS","S"))>0)&(FLAGS'["S") SET FLAGS=FLAGS_"S"
  IF ($DATA(TARRAY("FLAGS","p"))>0)&(FLAGS'["p") SET FLAGS=FLAGS_"p"
  ;
  DO WRITEHDR
  WRITE "<!DOCTYPE "_$GET(TARRAY("!DOCTYPE"),"UNDEFINED"),">",!
  NEW SRCNAME SET SRCNAME=$GET(TARRAY("EXPORT_SYSTEM_NAME"),"?Unnamed?")
  WRITE "<EXPORT source=""",$$SYMENC^MXMLUTL(SRCNAME),""">",!
  SET INDENTS=$GET(INDENTS)_INCINDENT  
  IF FLAGS["S" DO WRTSTNGS^TMGXMLE3(.FLAGS,.INDENTS)  ;"output writing settings
  NEW WRITERS  ;"leaving blank, so default callbacks used. 
  ;
  SET FILE=""                                                                                      
  FOR  SET FILE=$ORDER(TARRAY(FILE)) QUIT:(+FILE'>0)  DO
  . IF $DATA(TMGXDEBUG) USE $P WRITE "Writing file: ",FILE,! USE IO
  . NEW RECS MERGE RECS=TARRAY(FILE)
  . NEW IEN SET IEN=$ORDER(TARRAY(FILE,"")) QUIT:IEN=""
  . DO WRIT1FIL^TMGXMLE3(FILE,,.RECS,.FLAGS,.INDENTS,.SHOWPROG,.WRITERS,.SAVFIELDINFO)
  ;
  WRITE "</EXPORT>",!
  QUIT
  ;
WRITEHDR ;
  ;"Scope: PUBLIC
  ;"Purpose: A shell to WRITE out a proper XML header.  This should be done prior
  ;"              to writing out XML formatted data to a device
  ;"Output: Header is output to current device
  ;"Results: none
  NEW STR SET STR=$$XMLHDR^MXMLUTL
  WRITE STR,!
  QUIT
  ;
TEMPTEST  ; "Delete this bit later...
  NEW XMLARRAY,REFARR SET REFARR=$NAME(XMLARRAY)
  SET XMLARRAY(200,168)=""
  SET XMLARRAY(200,83)=""
  SET XMLARRAY(200,"TEMPLATE","*")=""
  SET XMLARRAY(8925,759)=""
  SET XMLARRAY("!DOCTYPE")="TMG_VISTA_XML_EXPORT"
  SET XMLARRAY("EXPORT_SYSTEM_NAME")="EHR:poweredge"
  SET XMLARRAY("FLAGS","i")=""
  NEW INDENTS SET INDENTS("INCINDENT")=" "
  DO WTXMLOUT^TMGXMLE2(REFARR,,.INDENTS)
  QUIT
  ;     
TESTASKDUMP() ; "Ask and dump in XML, for testing. 
  NEW OPTION
  SET OPTION("WRITE FILE LABEL FN")="WTFILLBL^TMGXMLE5"       
  SET OPTION("WRITE REC LABEL FN")="WTRLABEL^TMGXMLE5"  
  SET OPTION("WRITE FLD LABEL FN")="WTFLABEL^TMGXMLE5" 
  SET OPTION("WRITE LINE FN")="WTLINE^TMGXMLE5" 
  SET OPTION("WRITE WP LINE")="WWPLINE^TMGXMLE5"
  DO ASKDUMP^TMGDEBU3(,,.OPTION)
  QUIT
  ;
