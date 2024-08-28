TMGORWLEX	; ISL/JER - RPCs wrapping Legacy Lexicon APIs;04/11/2024
	;;1.0;ORDER ENTRY/RESULTS REPORTING;**385**;Apr 11, 2024;Build 1
 QUIT
 ;"TMG WRAPPER FUNCTION
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 4/11/2024  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
GETI10DX(ORY,ORX,ORDT)	;" RPC TMG ORWLEX GET10DX
    ;"Purpose: This RPC is a wrapper for ORWLEX GET10DX.
    ;"   After it is called, it will look at each result to see if it is an HCC Code
    DO GETI10DX^ORWLEX(.ORY,ORX,ORDT)
    ;" Look at each result:
    NEW IDX SET IDX=0 
    FOR  SET IDX=$ORDER(ORY(IDX)) QUIT:IDX'>0  DO
    . NEW LINE SET LINE=$G(ORY(IDX))
    . NEW CODE SET CODE=$P(LINE,"^",4)
    . NEW COLOR SET COLOR=$$ICDCOLOR^TMGCMS1(CODE)
    . SET $PIECE(ORY(IDX),"^",15)=COLOR  ;"Note: last used piece by VA code is #9.  We use #15 for future VA expansion
    QUIT
    ;