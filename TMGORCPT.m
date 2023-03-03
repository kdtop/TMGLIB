TMGORPT ;TMG/kst-Custom Code related to CPTs  ; 2/14/2023
         ;;1.0;TMG-LIB;**1**;2/14/2023
        ;
        ;"Code for handling CPT and modifiers from CPRS
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 2/14/2023  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;" RPC -- Public Functions.
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"DEPENDENCIES:
 ;"=======================================================================
 ;
CPTMODS(LST,ORCPTCOD,ORDATE)	;"Return CPT Modifiers for a CPT Code  <-- used by RPC: 'TMG ORWPCE CPTMODS'
  ;"Copied and modified from CPTMODS^ORWPCE <-- used by RPC: 'ORWPCE CPTMODS'
  ;"INPUT:  LST -- an OUT parameter
  ;"        ORCPTCOD -- Listing of one or more CPT's, used to generate list of relevant modifiers
  ;"        ORDATE -- OPTIONAL.  As-Of date for listing.  FMDT format. 
  ;"NOTE: use file 22752 TMG CPT MODIFIER EXCLUDE to filter out unwanted...  
  ;
  DO CPTMODS^ORWPCE(.LST,.ORCPTCOD,.ORDATE)
  ;"Filter out unwanted modifiers. 
  NEW IDX SET IDX=""
  FOR  SET IDX=$ORDER(LST(IDX)) QUIT:IDX=""  DO 
  . NEW ENTRY SET ENTRY=$GET(LST(IDX)) QUIT:ENTRY=""
  . NEW IEN SET IEN=+$PIECE(ENTRY,"^",1)
  . IF $ORDER(^TMG(22752,"B",IEN,0))>0 DO  ;"exclusion record present
  . . KILL LST(IDX)
  QUIT
  ;
