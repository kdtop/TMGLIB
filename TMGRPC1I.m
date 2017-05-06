TMGRPC1I ;TMG/kst- CPRS Macro ;10/17/14
         ;;1.0;TMG-LIB;**1**;10/17/14
 ;
  ;"TMG-CPRS Macro functions
 ;"  
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
 ;"MACRO(TMGRESULT,TMGIN) -- Entrypoint for RPC to effect macro processing a note from CPRS
 ;"
 ;"=======================================================================
 ;"Dependancies:
 ;" TMGHTM1, TMGTIUOJ, TMGTIUO6, XLFSTR, TMGSTUT2, TMGSTUT3
 ;"=======================================================================
MACRO(TMGRESULT,TMGIN) ;
        ;"Purpose: Entrypoint for RPC to effect macro processing a note from CPRS
        ;"         Receive entire note text and return snippet.
        ;"Input: TMGRESULT -- PASS BY REFERENCE, an OUT PARAMETER.
        ;"       TMGIN -- Input from client.  Format:
        ;"              TMGIN("DFN")=<DFN>  (IEN in PATIENT file)
        ;"              TMGIN("NAME")=<Macro name>  (Name in TMG MACRO file)
        ;"              TMGIN("TEXT",1) = 1st line of text
        ;"              TMGIN("TEXT",2) = 2nd line of text, etc
        ;"Output: TMGRESULT(0)="1^Success", or "-1^Error Message"
        ;"        TMGRESULT(1)=1st line of return text
        ;"        TMGRESULT(2)=2nd line of return text, etc.
        ;"Result: none
        KILL TMGRESULT
        NEW MACRONAME,TAG,ROUTINE,MACIEN,MACDATA
        SET MACRONAME=$GET(TMGIN("NAME"))
        SET MACIEN=$ORDER(^TMG(22718,"B",MACRONAME,0))
        IF MACIEN'>0 DO  GOTO MACDN
        . SET TMGRESULT(0)="-1^Macro name "_MACRONAME_" not found"
        SET MACDATA=$GET(^TMG(22718,MACIEN,0))
        SET TAG=$PIECE(MACDATA,"^",2)
        SET ROUTINE=$PIECE(MACDATA,"^",3)
        DO
        . NEW $ETRAP SET $ETRAP="SET TMGRESULT(0)=""Invalid tag in macro name"",! SET $ETRAP="""",$ECODE="""""
        . XECUTE "DO "_TAG_"^"_ROUTINE_"(.TMGRESULT,.TMGIN)"
MACDN   QUIT
        ;
EDDIE(TMGRESULT,TMGIN)
        KILL TMGRESULT
        ;"bMERGE ^TMG("MACROTEST")=TMGIN
        SET TMGRESULT(0)="1^Success"
        SET TMGRESULT(1)="Hello World"
        SET TMGRESULT(2)="Macroname = EDDIE TEST"
        QUIT
