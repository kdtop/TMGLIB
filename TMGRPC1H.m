TMGRPC1H ;TMG/kst-RPC Functions ;5/1/13, 2/2/14
         ;;1.0;TMG-LIB;**1**;5/1/13
 ;
 ;"TMG RPC FUNCTIONS especially related to CPRS
 ;"  Process Note (note sent from CPRS for extra processing, then returned)
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
 ;" RPC -- Public Functions.
 ;"=======================================================================
 ;"PROCESS(TMGRESULT,TMGIN) -- Entrypoint for RPC to effect processing a note from CPRS
 ;"GETTABLS(TABLES,HTML) -- Get list of all defined text tables, minus protected ones
 ;"TABLEND(LINE,PARTB) --Determine if HTML-coded line includes the end of a table.
 ;"
 ;"=======================================================================
 ;"Dependancies:
 ;" TMGHTM1, TMGTIUOJ, TMGTIUO6, XLFSTR, TMGSTUT2, TMGSTUT3
 ;"=======================================================================
 ;
PROCESS(TMGRESULT,TMGIN,FORCE) ;"Entry point for RPC: TMG CPRS PROCESS NOTE
  MERGE ^TMP("MOVEMEDS")=TMGIN
  DO MOVEMEDS^TMGTIUP3(.TMGIN)  ;"Move MEDICATION table to FINAL MEDICATION table
  NEW OPTION SET OPTION("ALL NOTES")=1
  DO PROCESS^TMGTIUP3(.TMGRESULT,.TMGIN,.FORCE,.OPTION) ;
  QUIT
  ;
GETTABLS(TABLES,HTML) ;"DEPRECIATED (MOVED) 
  ;"Get list of all defined text tables, minus protected ones
  DO GETTABLS^TMGTIUP3(.TABLES,.HTML)
  QUIT
  ;
TABLEND(LINE,PARTB) ;"DEPRECIATED (MOVED) 
  ;" HAS TABLE END  
  QUIT $$TABLEND^TMGTIUIP3(.LINE,.PARTB)
  ;

