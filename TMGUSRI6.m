TMGUSRI6 ;TMG/kst/USER INTERFACE API FUNCTIONS ;8/30/17
         ;;1.0;TMG-LIB;**1**;02/2/2014
 ;
 ;"TMG USER INTERFACE API FUNCTIONS
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 8/30/17  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"$$EDITBOX(INITVAL,WIDTH)  -- Edit box for editing strings
 ;
 ;"=======================================================================
 ;"Private Functions
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;"DEPENDENCIES: TMGUSRI5         
 ;"=======================================================================
 ;
EDITBOX(INITVAL,WIDTH,FILLCH,X,Y)  ;"Edit box for editing strings
  NEW OPTION 
  SET OPTION("WIDTH")=$GET(WIDTH)
  SET OPTION("FILLCH")=$GET(FILLCH)
  SET OPTION("X")=$GET(X)
  SET OPTION("Y")=$GET(Y)
  QUIT $$EDITBOX2(.INITVAL,.OPTION)
  ;           
EDITBOX2(INITVAL,OPTION)  ;"Edit box for editing strings
  ;"INPUT: INITVAL -- OPTIONAL.  This is the initial value.  Default is ""
  ;"       OPTION -- OPTION.  PASS BY REFERENCE.  Supported options:   
  ;"          OPTION("WIDTH") -- OPTIONAL.  Default to 40
  ;"          OPTION("FILLCH") -- OPTIONAL.  DEFAULT IS " "
  ;"                  If is "_", then a line is shown for edit field
  ;"          OPTION("X") -- OPTION. Position for editing.  Default is $X
  ;"          OPTION("Y") -- OPTION. Position for editing.  Default is $Y
  ;"Result: Returns edited value of string
  NEW WIDTH SET WIDTH=+$GET(OPTION("WIDTH")) SET:(WIDTH'>0) WIDTH=40
  ;"NEW MAXX,MAXY IF $$GETSCRSZ^TMGKERNL(.MAXY,.MAXX) ;DROP RESULT
  ;"IF WIDTH>MAXX SET WIDTH=MAXX IF $X+WIDTH>MAXX WRITE !
  NEW VAL SET VAL=$GET(INITVAL)
  SET FILLCH=$EXTRACT($GET(FILLCH),1) IF FILLCH="" SET FILLCH=" "
  NEW HOMEX,HOMEY 
  SET HOMEX=+$GET(OPTION("X")) IF HOMEX'>0 SET HOMEX=$X+1
  SET HOMEY=+$GET(OPTION("Y")) IF HOMEY'>0 SET HOMEY=$Y
  NEW DRAWIDX SET DRAWIDX=1
  NEW CPOS SET CPOS=$LENGTH(VAL)+1  ;"Abs position in text
  NEW INPUT,STRA,STRB,CH,DONE,ABORT SET (DONE,ABORT)=0
  NEW LNUMHIDE,RNUMHIDE,LNUMDOTS,RNUMDOTS,RNUMSPC,SCRNCPOS,LENVAL
LOOP ;  
  SET LENVAL=$LENGTH(VAL)
  SET LNUMHIDE=DRAWIDX-1
  SET RNUMHIDE=LENVAL-WIDTH-LNUMHIDE SET:(RNUMHIDE<0) RNUMHIDE=0
  SET LNUMDOTS=$SELECT(LNUMHIDE>2:2,1:LNUMHIDE)
  SET RNUMDOTS=$SELECT(RNUMHIDE>2:2,1:RNUMHIDE)
  SET SCRNCPOS=CPOS-DRAWIDX+1
  DO  ;"DRAW TO SCREEN 
  . DO CUP^TMGTERM(HOMEX,HOMEY)  ;"move cursor to starting position
  . IF DRAWIDX<1 SET DRAWIDX=1
  . NEW STR,LEN SET STR=$EXTRACT(VAL,DRAWIDX,DRAWIDX+WIDTH-1),LEN=$LENGTH(STR)
  . NEW JDX FOR JDX=1:1:LNUMDOTS SET $EXTRACT(STR,JDX)="."
  . FOR JDX=1:1:RNUMDOTS SET $EXTRACT(STR,LEN-JDX+1)="."
  . SET RNUMSPC=WIDTH-$LENGTH(STR)
  . ;"IF FILLCH'=" " SET STR=$$LJ^XLFSTR(STR,WIDTH,FILLCH)
  . IF FILLCH'=" " FOR JDX=1:1:RNUMSPC SET STR=STR_FILLCH
  . WRITE STR,"  "
  . NEW TEMPX SET TEMPX=HOMEX+SCRNCPOS-1
  . DO CUP^TMGTERM(TEMPX,HOMEY)
  SET INPUT=$$READKY^TMGUSRI5("e",,1,,.ESCKEY) ;"read one char, with ESC processing
  SET STRA=$EXTRACT(VAL,1,CPOS-1)       ;"text to left of cursor
  SET CH=$EXTRACT(VAL,CPOS)             ;"text under cursor
  SET STRB=$EXTRACT(VAL,CPOS+1,LENVAL)  ;"text to right of cursor
  IF INPUT="" DO
  . IF ESCKEY="CR" SET DONE=1 QUIT
  . IF ESCKEY="TAB" SET DONE=1 QUIT
  . IF ESCKEY="BACKSPC" DO  QUIT
  . . SET STRA=$EXTRACT(STRA,1,$LENGTH(STRA)-1)     
  . . SET VAL=STRA_CH_STRB
  . . SET CPOS=CPOS-1,SCRNCPOS=SCRNCPOS-1
  . . IF CPOS<DRAWIDX SET DRAWIDX=CPOS
  . . IF RNUMSPC>0 SET DRAWIDX=DRAWIDX-1
  . . ELSE  IF SCRNCPOS'>(LNUMDOTS+1) SET DRAWIDX=DRAWIDX-1
  . IF ESCKEY="REMOVE" DO  QUIT    ;"FORWARD DEL KEY
  . . SET VAL=STRA_STRB
  . . IF RNUMHIDE=0,LNUMHIDE>0,LENVAL'>WIDTH SET DRAWIDX=DRAWIDX-1
  . . IF RNUMSPC>0 SET DRAWIDX=DRAWIDX-1
  . IF ESCKEY="LEFT" DO  QUIT
  . . IF CPOS'>1 QUIT
  . . SET CPOS=CPOS-1,SCRNCPOS=SCRNCPOS-1
  . . IF CPOS<DRAWIDX SET DRAWIDX=CPOS
  . . ELSE  IF SCRNCPOS'>LNUMDOTS SET DRAWIDX=DRAWIDX-1
  . IF ESCKEY="RIGHT" DO  QUIT
  . . IF CPOS'<(LENVAL+1) QUIT
  . . SET CPOS=CPOS+1,SCRNCPOS=SCRNCPOS+1
  . . IF SCRNCPOS>(WIDTH-RNUMDOTS) SET DRAWIDX=DRAWIDX+1
  . IF ESCKEY="FIND" DO  QUIT     ;"HOME KEY
  . . SET CPOS=1
  . . IF CPOS<DRAWIDX SET DRAWIDX=CPOS
  . IF ESCKEY="SELECT" DO  QUIT   ;"END KEY
  . . SET CPOS=LENVAL+1
  . . SET DRAWIDX=(LENVAL+1)-WIDTH
  . IF ESCKEY="" DO  QUIT
  . . DO CUP^TMGTERM(HOMEX,HOMEY+1) 
  . . SET %=2 WRITE "Abort edit" DO YN^DICN
  . . DO CUP^TMGTERM(HOMEX,HOMEY+1) 
  . . WRITE "                        "
  . . IF %'=2 SET (DONE,ABORT)=1
  ELSE  DO
  . SET VAL=STRA_INPUT_CH_STRB
  . SET CPOS=CPOS+1
  . IF CPOS>WIDTH SET DRAWIDX=DRAWIDX+1
  IF 'DONE GOTO LOOP
EBDN  ;
  IF ABORT SET VAL=""
  QUIT VAL
  ;