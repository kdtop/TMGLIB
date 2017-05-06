TMGRPCL1 ;TMG/kst/Lab RPC Call Routines ; 3/21/15
         ;;1.0;TMG-LIB;**1**;3/21/15
 ;
 ;"TMG LAB RPC ROUTINES
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
 ;
 ;"=======================================================================
 ;" Private Functions.
 ;"=======================================================================
 ;"
 ;"=======================================================================
 ;"Dependancies:
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;
GETLABS(OUT,DFN,SDT,EDT,NCM,NTNX,NTFX,NNMX,NDT,NPNL) ;
  ;"Purpose: return formatted array containing patient's labs for specified date range. 
  ;"Input: OUT -- PASS BY REFERENCE.  AN OUT PARAMETER.  Format:
  ;"          OUT(0)="1^Success"
  ;"          OUT(#)="LAB^<FMDT>^<IEN60>^TESTNAME^VAL^UNITS^FLAG^REFLO^REFHI
  ;"          OUT(#)="LAB^<FMDT>^COMMENT^<Line#>^<text of comment>"
  ;"          OUT(#)="PANELS^<IEN60>^NAME^<PNLNAME>"
  ;"          OUT(#)="PANELS^<IEN60>^<FMDT>"  <-- FMDT will be unique to 1 lab group / panel
  ;"          OUT(#)="PANELS^<IEN60>^CONTAINS^<DISP SEQ#>^<HELD IEN60>^<Held IEN60-Name>"
  ;          old-> OUT(#)="TEST^IEN60^<IEN60>^<FMDT>"  -- Index of test by date. 
  ;          old-> OUT(#)="TEST^NAME^<TEST_NAME>^IEN60^<IEN60>
  ;          old-> OUT(#)="TEST^NAME^<TEST_NAME>^<FMDT>"
  ;          old-> OUT(#)="NAMES^<TEST_FLD#>^<test name>"
  ;"          NOTE: the order shown above is the order result will return in.
  ;"       1 DFN -- Patient IEN
  ;"       2 SDT -- Optional.  Start date FMDT format.  Default is 0 (earliest)
  ;"       3 EDT -- Optional.  End date FMDT format.  Default is 9999999 (last possible)
  ;"       4 NCM  -- Optional.  if 1 then don't return comments
  ;"       5 NTNX -- Optional.  if 1 then don't return TEST NAMES index node
  ;"       6 NTFX -- Optional.  if 1 then don't return TEST FLD index node
  ;"       7 NNMX -- Optional.  if 1 then don't return NAMES index
  ;"       8 NDT -- Optional.  if 1 then don't return DT index
  ;"       9 NPNL  -- Optional.  if 1 then don't return PANELS index
  ;"Result: none
  NEW LABS,STR
  NEW OPTION
  IF $GET(NCM)=1 SET OPTION("NO COMMENTS")=1
  IF $GET(NTNX)=1 SET OPTION("NO TEST NAME INDEX")=1  ;"obsolete
  IF $GET(NTFX)=1 SET OPTION("NO TEST FLD INDEX")=1   ;"obsolete
  IF $GET(NNMX)=1 SET OPTION("NO NAMES INDEX")=1      ;"obsolete
  IF $GET(NDT)=1 SET OPTION("NO DATES")=1
  IF $GET(NPNL)=1 SET OPTION("NO PANELS")=1
  DO GETLABS^TMGLRR02(.LABS,.DFN,.SDT,.EDT,.OPTION)
  NEW IDX SET IDX=1
  NEW DT SET DT=0
  FOR  SET DT=$ORDER(LABS("DT",DT)) QUIT:(DT="")  DO
  . NEW NODE SET NODE=""
  . FOR  SET NODE=$ORDER(LABS("DT",DT,NODE)) QUIT:(NODE="")  DO
  . . IF +NODE=NODE DO
  . . . SET STR="LAB^"_DT_"^"_NODE_"^"_$GET(LABS("DT",DT,NODE))
  . . . SET OUT(IDX)=STR,IDX=IDX+1
  . . ELSE  IF NODE="COMMENT" DO
  . . . NEW JDX SET JDX=0
  . . . FOR  SET JDX=$ORDER(LABS("DT",DT,"COMMENT",JDX)) QUIT:+JDX'>0  DO
  . . . . SET STR="LAB^"_DT_"^COMMENT^"_JDX_"^"_$GET(LABS("DT",DT,"COMMENT",JDX))
  . . . . SET OUT(IDX)=STR,IDX=IDX+1
  NEW IEN60 SET IEN60=0
  FOR  SET IEN60=$ORDER(LABS("PANELS",IEN60)) QUIT:+IEN60'>0  DO
  . SET STR="PANELS^"_IEN60_"^NAME^"_$GET(LABS("PANELS",IEN60))
  . SET OUT(IDX)=STR,IDX=IDX+1
  . SET DT=0 FOR  SET DT=$ORDER(LABS("PANELS",IEN60,DT)) QUIT:+DT'>0  DO
  . . SET STR="PANELS^"_IEN60_"^"_DT
  . . SET OUT(IDX)=STR,IDX=IDX+1
  . NEW SEQ SET SEQ=0
  . FOR  SET SEQ=$ORDER(^LAB(60,IEN60,2,SEQ)) QUIT:+SEQ'>0  DO
  . . NEW HELDIEN60 SET HELDIEN60=$PIECE($GET(^LAB(60,IEN60,2,SEQ,0)),"^",1)
  . . NEW HELDNAME SET HELDNAME=$PIECE($GET(^LAB(60,HELDIEN60,0)),"^",1)
  . . SET STR="PANELS^"_IEN60_"^CONTAINS^"_SEQ_"^"_HELDIEN60_"^"_HELDNAME
  . . SET OUT(IDX)=STR,IDX=IDX+1
  ;"NEW NODE SET NODE=""
  ;"FOR  SET NODE=$ORDER(LABS("TEST",NODE)) QUIT:NODE=""  DO
  ;". IF NODE="IEN60" DO
  ;". . NEW IEN60 SET IEN60=""
  ;". . FOR  SET IEN60=$ORDER(LABS("TEST","IEN60",IEN60)) QUIT:+IEN60'>0  DO
  ;". . . NEW DT SET DT=0
  ;". . . FOR  SET DT=$ORDER(LABS("TEST","IEN60",IEN60,DT)) QUIT:+DT'>0  DO
  ;". . . . SET STR="TEST^IEN60^"_IEN60"^"_DT
  ;". . . . SET OUT(IDX)=STR,IDX=IDX+1
  ;". IF NODE="FLD" DO  ;"<-- OLD
  ;". . NEW FLDNUM SET FLDNUM=""
  ;". . FOR  SET FLDNUM=$ORDER(LABS("TEST","FLD",FLDNUM)) QUIT:+FLDNUM'>0  DO
  ;". . . NEW DT SET DT=0
  ;". . . FOR  SET DT=$ORDER(LABS("TEST","FLD",FLDNUM,DT)) QUIT:+DT'>0  DO
  ;". . . . SET STR="TEST^FLD^"_FLDNUM_"^"_DT
  ;". . . . SET OUT(IDX)=STR,IDX=IDX+1
  ;". ELSE  IF NODE="NAME" DO
  ;". . NEW LABNAME SET LABNAME=""
  ;". . FOR  SET LABNAME=$ORDER(LABS("TEST","NAME",LABNAME)) QUIT:LABNAME=""  DO
  ;". . . NEW IEN60 SET IEN60=$GET(LABS("TEST","NAME",LABNAME))
  ;". . . SET STR="TEST^NAME^"_LABNAME_"^IEN60^"_IEN60
  ;". . . SET OUT(IDX)=STR,IDX=IDX+1
  ;". . . NEW DT SET DT=0
  ;". . . FOR  SET DT=$ORDER(LABS("TEST","NAME",LABNAME,DT)) QUIT:+DT'>0  DO
  ;". . . . SET STR="TEST^NAME^"_LABNAME_"^"_DT
  ;". . . . SET OUT(IDX)=STR,IDX=IDX+1
  ;"NEW FLDNUM SET FLDNUM=0
  ;"FOR  SET FLDNUM=$ORDER(LABS("NAMES",FLDNUM)) QUIT:FLDNUM=""  DO  
  ;". SET STR="NAMES^"_FLDNUM_"^"_$GET(LABS("NAMES",FLDNUM))
  QUIT
