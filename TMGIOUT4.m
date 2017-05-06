TMGIOUT4 ;TMG/kst/IO Utilities ;12/28/14, 10/14/15
         ;;1.0;TMG-LIB;**1**;10/14/15
 ;
 ;"TMG IO UTILITIES
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"NOTE: THIS CODE IS SACC COMPLIANT.  
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"LCSV2ARR(FULLPATHNAME,REF) -- LOAD CSV FILE TO ARRAY
 ;
 ;"=======================================================================
 ;"Dependancies
 ;
 ;"=======================================================================
 ;"=======================================================================
LCSV2ARR(FULLPATHNAME,REF,OPTION) ;"LOAD CSV FILE TO ARRAY
  ;"Purpose: Load a CSV file with a particular format:
  ;"         The first line contains all the field names, comma separated
  ;"         Then each subsequent line is one record.
  ;"NOTE: assumes no '^' character will be normally found in data.
  ;"Input: FULLPATHNAME -- the full path and name of the CSV file to load, e.g. "/tmp/myfiles/a/test.txt"
  ;"       REF -- PASS BY NAME -- the NAME of the array to put output into
  ;"       OPTION -- OPTIONAL.  PASS BY REFERENCE
  ;"          OPTION("DIV")=<divider character>  <-- default is ",".  E.g. of $CHAR(9) for tab-separate-values
  ;"          OPTION("PROGFN")=<code to execute for a progress function>
  ;"               Variables avail for code:
  ;"                       IDX = index of current process line
  ;"                        MAX = number of lines to progress
  ;"          OPTION("PROGFN","INTERVAL")=#, e.g. 100 for running function every 100 lines.
  ;"Result: 1^OK, or -1^Message
  ;"Output: @REF is filled as follows
  ;"             @REF@("A",<Field#>)=FieldName
  ;"             @REF@(Record#,<Field#>)=FieldValue
  NEW TMGRESULT SET TMGRESULT="1^OK"
  NEW TMGDATA
  SET FULLPATHNAME=$GET(FULLPATHNAME)
  IF $$FILEXIST^TMGIOUTL(FULLPATHNAME)'=1 DO  GOTO LC2ADN
  . SET TMGRESULT="-1^File not found: '"_FULLPATHNAME_"'"
  NEW HFSOPTION SET HFSOPTION("OVERFLOW")=1
  IF $$HFS2ARFP^TMGIOUT3(FULLPATHNAME,"TMGDATA",.HFSOPTION)=0 DO  GOTO LC2ADN
  . SET TMGRESULT="-1^Error loading file: '"_FULLPATHNAME_"'"
  NEW IDX SET IDX=$ORDER(TMGDATA(""))
  IF IDX="" DO  GOTO LC2ADN
  . SET TMGRESULT="-1^No data found in file: '"_FULLPATHNAME_"'"
  NEW MAX SET MAX=+$ORDER(TMGDATA(""),-1)
  NEW LINE SET LINE=$GET(TMGDATA(IDX))
  NEW TEMP,CT SET CT=1
  NEW DIV SET DIV=$GET(OPTION("DIV"),",")
  NEW PROGFN SET PROGFN=$GET(OPTION("PROGFN"))
  NEW PROGINTERVAL SET PROGINTERVAL=+$GET(OPTION("PROGFN","INTERVAL"))
  IF PROGINTERVAL=0 SET PROGINTERVAL=100
  NEW PROGCT SET PROGCT=0
  DO SPLIT2AR^TMGSTUT2(LINE,DIV,.TEMP)
  KILL TEMP("MAXNODE")
  MERGE @REF@("A")=TEMP
  FOR  SET IDX=$ORDER(TMGDATA(IDX)) QUIT:+IDX'>0  DO
  . SET LINE=$GET(TMGDATA(IDX)) QUIT:LINE=""
  . NEW P SET P=0
  . FOR  SET P=$FIND(LINE,DIV,P) QUIT:(P=0)  DO
  . . IF $$INQT^TMGSTUT3(LINE,P-1)=0 QUIT
  . . SET $EXTRACT(LINE,P-1)="^"  ;"protect div chars inside quotes
  . KILL TEMP DO SPLIT2AR^TMGSTUT2(LINE,DIV,.TEMP)
  . KILL TEMP("MAXNODE")
  . NEW JDX SET JDX=""
  . FOR  SET JDX=$ORDER(TEMP(JDX)) QUIT:+JDX'>0  DO
  . . NEW TS SET TS=TEMP(JDX) QUIT:TS'["^"
  . . SET TS=$TRANSLATE(TS,"^",DIV)  ;"convert '^' back to div chars
  . . IF $EXTRACT(TS,1)="""",$EXTRACT(TS,$LENGTH(TS))="""" SET TS=$EXTRACT(TS,2,$LENGTH(TS)-1)
  . . SET TEMP(JDX)=TS
  . MERGE @REF@(CT)=TEMP SET CT=CT+1
  . SET PROGCT=PROGCT+1
  . IF PROGFN'="",PROGCT>=PROGINTERVAL DO
  . . XECUTE PROGFN
  . . SET PROGCT=0
LC2ADN ;
  QUIT TMGRESULT
  ;
TEST1() ;
  NEW DATA
  NEW OPTION SET OPTION("MATCH","*.csv")=""
  NEW FULLPATHNAME SET FULLPATHNAME=$$FBROWSE^TMGIOUT2(.OPTION)
  NEW TMGRESULT
  SET TMGRESULT=$$LCSV2ARR(FULLPATHNAME,"DATA")
  IF $DATA(DATA) DO ZWRITE^TMGZWR("DATA")
  QUIT 
