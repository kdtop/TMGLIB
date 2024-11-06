TMGDEBUG ;TMG/kst/Debug utilities: logging, record dump ;03/25/06, 7/11/10, 2/2/14
         ;;1.0;TMG-LIB;**1**;07/12/05
 ;
 ;"TMG DEBUG UTILITIES
 ;"Kevin Toppenberg MD
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
 ;"ASKANODES
 ;"ARRNODES(PARR)
 ;
 ;"=======================================================================
 ;"Private API functions
 ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;
ASKDUMP ;"DEPRECIATED
  GOTO ASKDUMP^TMGDEBU3  
  ; 
GETERRST(ERRARRAY) ;"DEPRECIATED
  QUIT $$GETERRST^TMGDEBU2(.ERRARRAY)
  ;  
SHOWDIER(ERRMSG,PRIORERRORFOUND) ;"DEPRECIATED
  QUIT $$SHOWDIER^TMGDEBU2(.ERRARRAY)
  ;    
ASKANODES ;
  ;"Purpose: to ask user for the name of an array, then display nodes
  NEW NAME
  READ !,"Enter name of array to display nodes in: ",NAME:$GET(DTIME,3600),!
  IF NAME="^" SET NAME=""
  IF NAME'="" DO ARRNODES(NAME)
  QUIT
  ;
ARRNODES(PARR) ;
  ;"Purpose: To display all the nodes of the given array
  ;"Input: PARR -- NAME OF array to display
  NEW TMGI
  WRITE PARR,!
  SET TMGI=$ORDER(@PARR@(""))
  IF TMGI'="" FOR  DO  QUIT:(TMGI="")
  . WRITE " +--(",TMGI,")",!
  . SET TMGI=$ORDER(@PARR@(TMGI))
  QUIT
  ;
SYSSTATUSDUMP(ARG) ;"Dump ZSHOW info to global
  ;"Input: ARG -- optional  Default is "*"
  SET ARG=$GET(ARG,"*")
  NEW zzstackDump ZSHOW ARG:^TMP("TMGIDE",$J,$H)
  QUIT