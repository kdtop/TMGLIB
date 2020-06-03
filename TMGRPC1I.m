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
        ;"              TMGIN("NoteIEN") (IEN in 8925 of note being modified)
        ;"              TMGIN("MACRO")=<Macro IEN>  (IEN in TMG CPRS TEXT MACRO file 22718)
        ;"              TMGIN("SELECTION")=0 or 1  (Boolean 0=full note,1=selection)
        ;"              TMGIN("SELTEXT")=Text of selection
        ;"              TMGIN("TEXT",1) = 1st line of text
        ;"              TMGIN("TEXT",2) = 2nd line of text, etc
        ;"Output: TMGRESULT(0)="1^Success^SELECTION", or "-1^Error Message"
        ;"                   Note: SELECTION above is 0 to 1 whether the
        ;"                         selection was replaced or the whole note
        ;"                         (useful if a macro demanded the whole note to be replace (e.g.Process Note)
        ;"        TMGRESULT(1)=1st line of return text
        ;"        TMGRESULT(2)=2nd line of return text, etc.
        ;"Result: none
        KILL TMGRESULT
        NEW SELECTION SET SELECTION=+$G(TMGIN("SELECTION"))
        NEW MACROIEN SET MACROIEN=+$GET(TMGIN("MACRO"))
        IF MACROIEN'>0 DO  GOTO MACDN
        . SET TMGRESULT(0)="-1^Macro IEN "_MACROIEN_" not found"
        NEW MACDATA SET MACDATA=$GET(^TMG(22718,MACROIEN,0))
        NEW TAG SET TAG=$PIECE(MACDATA,"^",2)
        NEW ROUTINE SET ROUTINE=$PIECE(MACDATA,"^",3)
        NEW INPUTSAVE MERGE INPUTSAVE=TMGIN
        DO
        . NEW CODE SET CODE="DO "_TAG_"^"_ROUTINE_"(.TMGRESULT,.TMGIN,.SELECTION)"
        . NEW $ETRAP SET $ETRAP="SET TMGRESULT(0)=""ERROR executing ['"_CODE_"'] "",! SET $ETRAP="""",$ECODE="""""
        . XECUTE CODE
        SET TMGRESULT(0)=$G(TMGRESULT(0),"-1")_"^"_SELECTION
        ;"
        ;"SAVE TMGIN
        NEW DFN,TIUIEN
        SET DFN=+$G(TMGIN("DFN")),TIUIEN=+$G(TMGIN("NoteIEN"))
        MERGE ^TMP($J,"TMGMACRO","UNDO",DFN,TIUIEN)=INPUTSAVE("TEXT")
        ;"
MACDN   QUIT
        ;
GTMACROS(TMGRESULT,DUZ)  ;"
        ;"RPC: TMG CPRS GET MACRO LIST
        ;"     DUZ NOT CURRENTLY USED, BUT CAN IN THE FUTURE
        ;"  NOTE: If DESC starts working on client side, get all
        ;"        text from DESC WP field
        SET TMGRESULT(0)="1^SUCCESSFUL"
        ;"SET TMGRESULT(1)="1^PROCESS NOTE"
        ;"SET TMGRESULT(2)="2^UPDATE SOMETHING"
        NEW IDX,IEN,NAME,DESC
        SET IDX=1,IEN=0
        FOR  SET IEN=$O(^TMG(22718,IEN)) QUIT:IEN'>0  DO
        . SET NAME=$P($G(^TMG(22718,IEN,0)),"^",4)
        . SET DESC=$G(^TMG(22718,IEN,1,1,0))
        . SET TMGRESULT(IDX)=IEN_"^"_NAME_"^"_DESC
        . SET IDX=IDX+1
        QUIT
        ;"
EDDIE(TMGRESULT,TMGIN,SELECTION)
        KILL TMGRESULT
        ;"bMERGE ^TMG("MACROTEST")=TMGIN
        SET TMGRESULT(0)="1^Success"
        SET TMGRESULT(1)="Hello World"
        SET TMGRESULT(2)="Macroname = EDDIE TEST"
        QUIT
        ;"
PROCESS(TMGRESULT,TMGIN,SELECTION)  ;"
        ;"Entry point for Process Note Macro
        ;"SELECTION should be either 1 or 0 depending on whether it is a
        ;" partial section or the whole
        SET SELECTION=0
        DO PROCESS^TMGRPC1H(.TMGRESULT,.TMGIN,1)
        QUIT
        ;"
SAVENOTE(TMGIN)
        ;"TO DO: SAVE THIS TO A GLOBAL USING ^TMP, $J, TIUIEN, DFN
        QUIT
        ;"
UNDO(TMGRESULT,TMGIN,SELECTION)
        ;"TO DO: RETRIEVE LAST NOTE USING ^TMP, $J, TIUIEN, DFN
        SET TMGRESULT(0)="-1^NOT DONE YET"
        NEW DFN,TIUIEN
        SET DFN=+$G(TMGIN("DFN")),TIUIEN=+$G(TMGIN("NoteIEN"))
        IF $D(^TMP($J,"TMGMACRO","UNDO",DFN,TIUIEN)) DO
        . NEW IDX SET IDX=0
        . FOR  SET IDX=$O(^TMP($J,"TMGMACRO","UNDO",DFN,TIUIEN,IDX)) QUIT:IDX'>0  DO
        . . SET TMGRESULT(IDX)=$G(^TMP($J,"TMGMACRO","UNDO",DFN,TIUIEN,IDX))
        . SET TMGRESULT(0)="1^SUCCESSFUL"
        QUIT
        ;"
SORTBYA(TMGRESULT,TMGIN,SELECTION)
        SET SELECTION=0
        DO SORTBY(.TMGRESULT,.TMGIN,"A")
        QUIT
        ;"
SORTBYB(TMGRESULT,TMGIN,SELECTION)
        SET SELECTION=0 
        DO SORTBY(.TMGRESULT,.TMGIN,"B")
        QUIT
        ;"
SORTBYC(TMGRESULT,TMGIN,SELECTION)
        SET SELECTION=0 
        DO SORTBY(.TMGRESULT,.TMGIN,"C")
        QUIT
        ;"
SORTBY(TMGRESULT,TMGIN,GROUPLETTER)
        ;"HERE WE NEED TO SORT THE HPI BY THE GROUPLETTER GIVEN
        NEW TEST SET TEST=0
        IF TEST=0 DO
        . K ^TMP("TMGRPC1L","TMGIN")
        . MERGE ^TMP("TMGRPC1l","TMGIN")=TMGIN
        ELSE  DO
        . MERGE TMGIN=^TMP("TMGRPC1l","TMGIN")
        IF DUZ'=168 DO  GOTO SBDN
        . SET TMGRESULT(0)="-1^YOUR USER IS NOT SET FOR GROUP SORTING"
        NEW NOTEARR MERGE NOTEARR=TMGIN("TEXT")
        NEW ITEMARRAY,OPTION,TMGHPI,TMGHPIARR
        SET ITEMARRAY("DFN")=TMGIN("DFN")
        DO SCRUBESCRIBE^TMGTIUP2(.NOTEARR) ;"SCRUB ARRAY FOR ESCRIBE TAGS
        SET OPTION("RETURN-REST")=1
        NEW RTNNOTE
        NEW TEMP SET TEMP=$$PARSEARR^TMGTIUP2(.NOTEARR,.ITEMARRAY,.OPTION,.RTNNOTE)  ;"Parse note array into formatted arraY     
        SET OPTION("BULLETS")=$$GETINIVALUE^TMGINI01(DUZ,"Use Bullets In HPI",1)
        SET OPTION("GROUP-ORDER")=GROUPLETTER
        SET OPTION("TRAILING <BR>")=1  ;"Add blank line to end of each section
        SET OPTION("DIRECT HTML INSERTION")=1
        SET TMGHPI=$$COMPHPI^TMGTIUP2(.ITEMARRAY,.OPTION,.TMGHPIARR)  ;"COMPILE HPI
        ;"
        SET TMGRESULT(0)="1^SUCCESSFUL"
        SET TMGRESULT(1)=$PIECE(RTNNOTE,"@@TMGHPI@@",1)_TMGHPI_$PIECE(RTNNOTE,"@@TMGHPI@@",2)
        
SBDN
        QUIT
        ;"
AUTOGRP2(TMGRESULT,TMGIN,SELECTION)
        SET SELECTION=0
        DO AUTOGRP(.TMGRESULT,.TMGIN,2)
        QUIT
        ;"
AUTOGRP3(TMGRESULT,TMGIN,SELECTION)
        SET SELECTION=0
        DO AUTOGRP(.TMGRESULT,.TMGIN,3)
        QUIT
        ;"
AUTOGRP(TMGRESULT,TMGIN,NUMBER)
        SET TMGRESULT(0)="-1^TO BE COMPLETED"
        QUIT
        ;"
RFRSHTBL(TMGRESULT,TMGIN,SELECTION)
        SET SELECTION=0
        SET TMGRESULT(0)="-1^TO BE COMPLETED"
        QUIT
        ;"