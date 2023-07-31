TMGUTIL0 ;TMG/kst/Utility functions ;10/25/15
         ;;1.0;TMG-LIB;**1,17**;10/25/15
 ;
 ;"TMG UTILITY FUNCTIONS
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 10/25/2015  Kevin S. Toppenberg MD
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
 ;"Private Functions
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"DEPENDENCIES: 
 ;"=======================================================================
 ;
DDSRCH(OUT,SRCHSTR,OPTION)  ;"Search through (selected) parts of the ^DD for SearchString
  ;"Input: OUT -- PASS BY REFERENCE.  Output format:
  ;"          OUT(FILE,FLD)=<value>
  ;"       SRCHSTR -- The string to search for
  ;"       OPTION -- [optional]  PASS BY REFERENCE
  ;"            OPTION("FILE")=FilemanfileNumber <-- if present, only this file is search
  ;"NOTE: This is initially designed just to searh for input transforms, but
  ;"   could later be modified via OPTION's to do more
  NEW ENDFILE SET ENDFILE=$GET(OPTION("FILE"),0)
  NEW FILE SET FILE=ENDFILE
  FOR  SET FILE=$ORDER(^DD(FILE)) QUIT:(+FILE'>0)!((ENDFILE'=0)&(FILE'=ENDFILE))  DO
  . NEW FLD SET FLD=0
  . FOR  SET FLD=$ORDER(^DD(FILE,FLD)) QUIT:+FLD'>0  DO
  . . NEW CODE SET CODE=$PIECE($GET(^DD(FILE,FLD,0)),"^",5,999) QUIT:CODE=""
  . . IF CODE'[SRCHSTR QUIT
  . . SET OUT(FILE,FLD)=CODE
  ;"this code was not finished or debugged.... 
  QUIT;
  ;
ROUND(N,DIGITS)  ;"ROUND N TO # NUMBER OF DIGITS
  NEW MULT SET MULT=$EXTRACT("100000000000000000",1,DIGITS+1)
  NEW TEMP SET TEMP=N*MULT
  NEW DELTA SET DELTA=$SELECT(TEMP>0:0.5,1:-0.5)
  SET TEMP=TEMP+DELTA
  SET TEMP=TEMP\1
  SET TEMP=TEMP/MULT
  QUIT TEMP
  ;