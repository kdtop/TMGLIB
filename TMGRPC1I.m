TMGRPC1I ;TMG/kst- CPRS Macro ;10/17/14, 3/24/21
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
        NEW TEST SET TEST=0
        IF TEST=0 DO
        . K ^TMP("TMGRPC1I","TMGIN")
        . MERGE ^TMP("TMGRPC1I","TMGIN")=TMGIN
        ELSE  DO
        . MERGE TMGIN=^TMP("TMGRPC1I","TMGIN")
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
        . NEW $ETRAP SET $ETRAP="SET TMGRESULT(0)=""ERROR executing ['"_CODE_"'] "" SET $ETRAP="""",$ECODE="""""
        . XECUTE CODE
        SET TMGRESULT(0)=$G(TMGRESULT(0),"-1")_"^"_SELECTION
        ;"
        ;"SAVE TMGIN
        NEW TMGDFN,TIUIEN
        SET TMGDFN=+$G(TMGIN("DFN")),TIUIEN=+$G(TMGIN("NoteIEN"))
        MERGE ^TMP($J,"TMGMACRO","UNDO",TMGDFN,TIUIEN)=INPUTSAVE("TEXT")
        ;"
MACDN   QUIT
        ;
GTMACROS(TMGRESULT,DUZ)  ;
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
EDDIE(TMGRESULT,TMGIN,SELECTION) ;
        KILL TMGRESULT
        ;"bMERGE ^TMG("MACROTEST")=TMGIN
        SET TMGRESULT(0)="1^Success"
        SET TMGRESULT(1)="Hello World"
        SET TMGRESULT(2)="Macroname = EDDIE TEST"
        QUIT
        ;"
PROCESS(TMGRESULT,TMGIN,SELECTION)  ;
        ;"Entry point for Process Note Macro
        ;"SELECTION should be either 1 or 0 depending on whether it is a
        ;" partial section or the whole
        SET SELECTION=0
        DO PROCESS^TMGRPC1H(.TMGRESULT,.TMGIN,1)
        QUIT
        ;"
SAVENOTE(TMGIN) ;
        ;"TO DO: SAVE THIS TO A GLOBAL USING ^TMP, $J, TIUIEN, TMGDFN
        QUIT
        ;"
UNDO(TMGRESULT,TMGIN,SELECTION) ;
        ;"TO DO: RETRIEVE LAST NOTE USING ^TMP, $J, TIUIEN, TMGDFN
        SET TMGRESULT(0)="-1^NOT DONE YET"
        NEW TMGDFN,TIUIEN
        SET TMGDFN=+$G(TMGIN("DFN")),TIUIEN=+$G(TMGIN("NoteIEN"))
        IF $D(^TMP($J,"TMGMACRO","UNDO",TMGDFN,TIUIEN)) DO
        . NEW IDX SET IDX=0
        . FOR  SET IDX=$O(^TMP($J,"TMGMACRO","UNDO",TMGDFN,TIUIEN,IDX)) QUIT:IDX'>0  DO
        . . SET TMGRESULT(IDX)=$G(^TMP($J,"TMGMACRO","UNDO",TMGDFN,TIUIEN,IDX))
        . SET TMGRESULT(0)="1^SUCCESSFUL"
        QUIT
        ;
SORTBYA(TMGRESULT,TMGIN,SELECTION) ;
        SET SELECTION=0
        DO SORTBY(.TMGRESULT,.TMGIN,"A")
        QUIT
        ;"
SORTBYB(TMGRESULT,TMGIN,SELECTION) ;
        SET SELECTION=0 
        DO SORTBY(.TMGRESULT,.TMGIN,"B")
        QUIT
        ;"
SORTBYC(TMGRESULT,TMGIN,SELECTION) ;
        SET SELECTION=0 
        DO SORTBY(.TMGRESULT,.TMGIN,"C")
        QUIT
        ;"
SORTBY(TMGRESULT,TMGIN,GROUPLETTER) ;
        ;"HERE WE NEED TO SORT THE HPI BY THE GROUPLETTER GIVEN
        NEW TEST SET TEST=0
        IF TEST=0 DO
        . K ^TMP("TMGRPC1I","TMGIN")
        . MERGE ^TMP("TMGRPC1I","TMGIN")=TMGIN
        ELSE  DO
        . MERGE TMGIN=^TMP("TMGRPC1I","TMGIN")
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
        SET OPTION("TRAILING <br>")=1  ;"Add blank line to end of each section
        SET OPTION("DIRECT HTML INSERTION")=1
        SET TMGHPI=$$COMPHPI^TMGTIUP2(.ITEMARRAY,.OPTION,.TMGHPIARR)  ;"COMPILE HPI
        ;"
        SET TMGRESULT(0)="1^SUCCESSFUL"
        SET TMGRESULT(1)=$PIECE(RTNNOTE,"@@TMGHPI@@",1)_TMGHPI_$PIECE(RTNNOTE,"@@TMGHPI@@",2)
SBDN
        QUIT
        ;"
AUTOGRP2(TMGRESULT,TMGIN,SELECTION) ;
        SET SELECTION=0
        DO AUTOGRP(.TMGRESULT,.TMGIN,2)
        QUIT
        ;"
AUTOGRP3(TMGRESULT,TMGIN,SELECTION) ;
        SET SELECTION=0
        DO AUTOGRP(.TMGRESULT,.TMGIN,3)
        QUIT
        ;"
AUTOGRP(TMGRESULT,TMGIN,NUMBER) ;
        ;"SET TMGRESULT(0)="-1^TO BE COMPLETED"
        NEW TEST SET TEST=0
        IF TEST=0 DO
        . KILL ^TMP("TMGRPC1L","TMGIN")
        . MERGE ^TMP("TMGRPC1l","TMGIN")=TMGIN
        ELSE  DO
        . MERGE TMGIN=^TMP("TMGRPC1l","TMGIN")
        IF DUZ'=168 DO  GOTO AGDN
        . SET TMGRESULT(0)="-1^YOUR USER IS NOT SET FOR GROUP SORTING"
        NEW NOTEARR MERGE NOTEARR=TMGIN("TEXT")
        NEW ITEMARRAY,OPTION,TMGHPI,TMGHPIARR
        SET OPTION("AUTOGROUPING")=1
        SET OPTION("NUMOFGROUPS")=NUMBER
        SET OPTION("FORCEAUTOGROUP")=1
        SET ITEMARRAY("DFN")=TMGIN("DFN")
        DO SCRUBESCRIBE^TMGTIUP2(.NOTEARR) ;"SCRUB ARRAY FOR ESCRIBE TAGS
        SET OPTION("RETURN-REST")=1
        NEW RTNNOTE
        NEW TEMP SET TEMP=$$PARSEARR^TMGTIUP2(.NOTEARR,.ITEMARRAY,.OPTION,.RTNNOTE)  ;"Parse note array into formatted arraY     
        SET OPTION("BULLETS")=$$GETINIVALUE^TMGINI01(DUZ,"Use Bullets In HPI",1)
        SET OPTION("TRAILING <br>")=1  ;"Add blank line to end of each section
        SET OPTION("DIRECT HTML INSERTION")=1
        SET TMGHPI=$$COMPHPI^TMGTIUP2(.ITEMARRAY,.OPTION,.TMGHPIARR)  ;"COMPILE HPI
        ;"
        SET TMGRESULT(0)="1^SUCCESSFUL"
        SET TMGRESULT(1)=$PIECE(RTNNOTE,"@@TMGHPI@@",1)_TMGHPI_$PIECE(RTNNOTE,"@@TMGHPI@@",2)     
AGDN
        QUIT
        ;"
RFRSHTBL(TMGRESULT,TMGIN,SELECTION) ;
        SET SELECTION=0
        NEW OPTION
        SET OPTION("DIRECT HTML INSERTION")=1
        SET OPTION("HTML")=1
        ;SET TMGRESULT(0)="1^DONE"
        DO FRSHTABL^TMGTIUP3(.TMGRESULT,.TMGIN,.OPTION)
        QUIT
        ;"
KTMAMMO(TMGRESULT,TMGIN,SELECTION) ;   
        NEW TEST SET TEST=0
        IF TEST=0 DO
        . KILL ^TMP("TMGRPC1I","TMGIN")
        . MERGE ^TMP("TMGRPC1I","TMGIN")=TMGIN
        ELSE  DO
        . MERGE TMGIN=^TMP("TMGRPC1I","TMGIN")
        KILL TMGRESULT
        SET TMGRESULT(0)="1^Success"
        NEW TEXT MERGE TEXT=TMGIN("TEXT")
        SET L1=1
        NEW SRCH SET SRCH("<TD>")=""
        SET L1=$$SCANTO(.TEXT,L1,.SRCH)  ;"//find  first <TD>
        IF L1=-1 GOTO KTMABORT
        KILL SRCH SET SRCH("Exm Date:")=""
        SET L2=$$SCANTO(.TEXT,L1,.SRCH)
        IF L2=-1 GOTO KTMABORT
        DO DELBETWEEN(.TEXT,L1+2,L2-1)
        SET L1=L2     
        KILL SRCH SET SRCH("Impression:")=""
        SET L2=$$SCANTO(.TEXT,L1,.SRCH)
        IF L2=-1 GOTO KTMABORT
        DO DELBETWEEN(.TEXT,L1+1,L2-1)  ;"Delete between ExmDate and Impression
        SET L1=L2
        KILL SRCH SET SRCH("DENSITYCODE:")=""
        SET L1=$$SCANTO(.TEXT,L1,.SRCH)
        IF L1=-1 GOTO KT1
        KILL SRCH SET SRCH("</TD>")=""
        SET L2=$$SCANTO(.TEXT,L1,.SRCH)
        IF L2=-1 GOTO KTMABORT
        DO DELBETWEEN(.TEXT,L1+1,L2-1)  ;"Delete between from after DENSITYCODE and end of report
        SET L1=L2
KT1     ;        
        MERGE TMGRESULT=TEXT
        GOTO KTMDN
KTMABORT ;
        SET TMGRESULT(0)="-1^Error processing code in KTMAMMO^TMGRPC1I (Report probably doesn't fit expected pattern.)"        
KTMDN   QUIT
        ;"    
TKT ;
        NEW TEXT MERGE TEXT("TEXT")=^TMG("MACROTEST","TEXT")
        NEW OUT
        DO KTMAMMO(.OUT,.TEXT)
        QUIT
        ;
DELBETWEEN(TEXT,STARTLN,ENDLN) ;"used by KTMAMMO above
        NEW IDX SET IDX=STARTLN-1   ;"//Assumes integer indexes -- should be true
        FOR  SET IDX=$ORDER(TEXT(IDX))  QUIT:(IDX="")!(IDX>ENDLN)  DO
        . KILL TEXT(IDX)
        QUIT
        ;
SCANTO(TEXT,STARTLN,MATCHES,NEGMATCHES) ;"used by KTMAMMO above
        ;"TEXT:  TEXT(1)="line 1"  etc
        ;"STARTLN:   Line number, in TEXT array, to START scan
        ;"MATCHES:   E.g. MATCHES("MAMMO")=""    <-- These are terms to scan for
        ;"                MATCHES("Impression")=""
        ;"NEGMATCHES:  E.g. NEGMATCHES("CAR")=""    <-- These are terms EXCLUDE from result, negative matches
        ;"                NEGMATCHES("PONY")=""
        ;"OUTPUT: -1 if not found, or # for line number where terms were found.  
        NEW IDX SET IDX=STARTLN
        NEW DONE SET DONE=0
        NEW RESULT SET RESULT=-1
        FOR  DO  QUIT:DONE
        . NEW LINE SET LINE=$GET(TEXT(IDX)) 
        . IF $$MATCHES(LINE,.MATCHES,.NEGMATCHES) DO  QUIT
        . . SET DONE=1
        . . SET RESULT=IDX
        . SET IDX=$ORDER(TEXT(IDX))
        . SET DONE=(+IDX=0)
        QUIT RESULT
        ;
MATCHES(LINE,MATCHES,NEGMATCHES) ;"used by KTMAMMO above        
        ;"Params -- see SCANTO above
        ;"Results: 1 if line matches, 0 otherwise
        NEW M1 SET M1=$$MATCH1(LINE,.MATCHES)
        NEW M2 SET M2=$$MATCH1(LINE,.NEGMATCHES)
        NEW RESULT SET RESULT=M1&'M2
        QUIT RESULT
        ;
MATCH1(LINE,ARR)  ;"used by KTMAMMO above
        NEW FOUND SET FOUND=0
        NEW TERM SET TERM=""
        FOR  SET TERM=$ORDER(ARR(TERM)) QUIT:(TERM="")!(FOUND=1)  DO
        . SET FOUND=LINE[TERM
        QUIT FOUND
        ;