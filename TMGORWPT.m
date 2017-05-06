TMGORWPT ;TMG/kst-Patient Selection Custom Code  ; 12/31/12, 2/2/14
         ;;1.0;TMG-LIB;**1**;12/31/12
        ;
        ;"Code for handling event of NEW patient being selected
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
        ;"PTSEL -- Event handler for when NEW patient selected.
        ;
        ;"=======================================================================
        ;"PRIVATE API FUNCTIONS
        ;"=======================================================================
        ;
        ;"=======================================================================
        ;"DEPENDENCIES:
        ;"=======================================================================
PTSEL ;
        ;"Purpose: event handler for when NEW patient selected.
        ;"Called from VWPT2^ORWPT2
        DO NEWPT^TMGTIUT1
        QUIT
        ;
