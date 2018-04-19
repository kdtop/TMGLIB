TMGTIUP4 ;TMG/kst-TMG TIU NOTE PARSING FUNCTIONS ; 4/11/17
         ;;1.0;TMG-LIB;**1,17**;4/11/17
 ;
 ;"Kevin Toppenberg MD
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 4/11/17  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;"PUBLIC FUNCTIONS
 ;"=======================================================================
 ;"
 ;"=======================================================================
 ;"PRIVATE FUNCTIONS
 ;"=======================================================================
 ;"
 ;"=======================================================================
 ;"Dependancies :  
 ;"=======================================================================
 ;
TEST ;"Test one patient
  NEW DIC,X,Y
  SET DIC=2,DIC(0)="MAEQ"
  DO ^DIC WRITE !
  QUIT:+Y'>0
  NEW RESULT SET RESULT=$$LASTHPI(+Y) 
  QUIT
  ;
LASTHPI(DFN,HTMLMODE)  ;"Return the last HPI section, with processing, formatting etc.
  ;"Input: DFN -- patient IEN
  ;"       HTMLMODE -- Optional.  Default is 1
  ;"Result: a long string with last HPI (formatted) in it.  
  NEW RESULT SET RESULT=""
  SET HTMLMODE=+$GET(HTMLMODE,1) 
  NEW TIUIEN SET TIUIEN=$$LASTTIU^TMGTIUP2(DFN,"HISTORY OF PRESENT ILLNESS (HPI):")
  IF TIUIEN'>0 GOTO LHDN
  NEW ARR1 DO FORMTHPI(TIUIEN,.ARR1)
  NEW ARR3
  NEW IDX SET IDX=0 FOR  SET IDX=$ORDER(ARR1("SEQ",IDX)) QUIT:IDX'>0  DO
  . NEW TITLE SET TITLE=$GET(ARR1("SEQ",IDX)) QUIT:TITLE=""
  . NEW ARR2 MERGE ARR2=ARR1("HPI",$PIECE(TITLE,"^",1))
  . NEW TEMP,TABLES DO PRCSTITL(.TEMP,.ARR2,$PIECE(TITLE,"^",2),.TABLES)
  . MERGE ARR3(IDX)=TEMP
  . MERGE ARR3(IDX,"TABLES")=TABLES
  SET RESULT=$$ASSEMBLE(.ARR3)  ;"put it all back together for output. 
LHDN ;
  QUIT RESULT  
  ;
FORMTHPI(TIUIEN,ARR2)  ;"reformat the HPI array   
  NEW ARR
  DO SUMNOTE^TMGTIUP1(TIUIEN,.ARR)
  KILL ARR(TIUIEN,"A&P"),ARR(TIUIEN,"FULL","A&P")
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(ARR(TIUIEN,"HPI",IDX)) QUIT:IDX'>0  DO
  . NEW TEMP SET TEMP=$GET(ARR(TIUIEN,"HPI",IDX))
  . NEW TITLE SET TITLE=$PIECE(TEMP,"^",1)
  . SET $PIECE(TEMP,"^",2)=$GET(ARR(TIUIEN,"TITLE",TITLE))
  . SET ARR2("SEQ",IDX)=TEMP
  MERGE ARR2("HPI")=ARR(TIUIEN,"FULL","HPI")
  QUIT
  ;
PRCSTITL(OUT,ARR,TITLENAME,TABLESLIST)  ;"PROCESS ONE TITLE section array
  ;"Input: OUT -- PASS BY REFERENCE.  AN OUT PARAMETER.  Format:
  ;"         OUT(#)=output line
  ;"       ARR -- PASS BY REFERENCE.  Format: 
  ;"         ARR(#) test from paragraph of title section
  ;"       TITLENAME -- The title name, e.g. "Hypertension"
  ;"       TABLESLIST -- PASS BY REFERENCE, AN OUT PARAMETER.  format:
  ;"          TABLESLIST(#)=name of table appearing in section
  ;"FINISH...
  ;"Remove normal tables, keeping record of their order of appearance
  ;"trim empty lines at the end
  ;"next look for any inline tables and refresh them.  
  QUIT
  ;
ASSEMBLE(ARR,HTMLMODE) ;"
  ;"Input: ARR -- PASS BY REFERENCE.  Format:
  ;"         ARR(<title Seq#>,<Line#>)=text for paragraph
  ;"Result: long string containing last HPI    
  NEW MASTERTABLES
  NEW LF SET LF=$SELECT(HTMLMODE:"<BR>",1:$CHAR(13,10))
  NEW RESULT SET RESULT=""
  NEW SEQ SET SEQ=0 FOR  SET SEQ=$ORDER(ARR(SEQ)) QUIT:SEQ'>0  DO
  . NEW JDX,TEMPTABLES MERGE TEMPTABLES=ARR(SEQ,"TABLES") KILL ARR(SEQ,"TABLES")
  . SET JDX=0 FOR  SET JDX=$ORDER(ARR(SEQ,JDX)) QUIT:JDX'>0  DO
  . . NEW LINE SET LINE=$GET(ARR(SEQ,JDX))
  . . IF RESULT'="" SET RESULT=RESULT_LF
  . . SET RESULT=RESULT_LINE
  . SET JDX=0 FOR  SET JDX=$ORDER(TEMPTABLES(JDX)) QUIT:JDX'>0  DO
  . . NEW TABLENAME SET TABLENAME=$GET(TEMPTABLES(JDX)) QUIT:TABLENAME=""
  . . DO ADDTABL(.RESULT,TABLENAME,.MASTERTABLES)
  QUIT RESULT
  ;
ADDTABL(OUTSTR,TABLENAME,MASTERTABLES)  ;"Append table to output string
  DO ENSURTABL(TABLENAME,.MASTERTABLES)
  ;"FINISH ... add table to outstr
  QUIT 

ENSURTBL(TABLENAME,MASTERTABLES) ;"ensure that TABLES array has entry for TABLENAME
  ;"If tablename not already present, then call TABLE code to populate the table.  
  QUIT
