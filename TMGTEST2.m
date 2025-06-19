TMGTEST2 ;TMG/kst/Scratch fns for programming tests ;03/25/06, 2/2/14
          ;;1.0;TMG-LIB;**1**;09/01/05
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
T1 ;
  DO T2("HI")
  QUIT
  ;
T2(A) ;
  DO T3(A,"THERE")
  ;
T3(A,B) ;
  WRITE A_" "_B,!
  QUIT
  ;
WORD
  NEW ARR1,ARR2,LETTER1,LETTER2
  SET ARR1("Q")="",ARR1("E")=""
  SET ARR1("R")="",ARR1("D")=""
  SET ARR1("Z")="",ARR1("L")=""
  SET ARR1("X")="",ARR1("V")=""
  SET ARR1("M")=""
  SET ARR2("Q")="",ARR2("E")=""
  SET ARR2("R")="",ARR2("D")=""
  SET ARR2("Z")="",ARR2("L")=""
  SET ARR2("X")="",ARR2("V")=""
  SET ARR2("M")=""
  SET (LETTER1,LETTER2)=""
  FOR  SET LETTER1=$O(ARR1(LETTER1)) QUIT:LETTER1=""  DO
  . SET LETTER2=""
  . FOR  SET LETTER2=$O(ARR2(LETTER2)) QUIT:LETTER2=""  DO
  . . WRITE LETTER1,"L",LETTER2,"ER",!
  QUIT
  ;"
NOTETOC(TMGRESULT,TMGIN)  ;"TEMP ENTRY POINT
  ZL "TMGRPC1H"
  DO NOTETOC^TMGRPC1H(.TMGRESULT,.TMGIN)
  QUIT
  ;"