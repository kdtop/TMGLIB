TMGUSRI6 ;TMG/kst/USER INTERFACE API FUNCTIONS ;8/30/17, 2/7/22
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
 ;"$$EDITBOX(INITVAL,WIDTH,FILLCH,X,Y)  -- Edit box for editing strings
 ;"$$EDITBOX2(INITVAL,OPTION)  -- Edit box for editing strings
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
EDITBOX(INITVAL,WIDTH,FILLCH,X,Y,MAXSTRLEN)  ;"Edit box for editing strings
  ;"INPUT: INITVAL -- OPTIONAL.  This is the initial value.  Default is ""
  ;"       WIDTH -- This is the width of the edit field.  Default is 40   
  ;"       FILLCH -- OPTIONAL.  DEFAULT IS " "
  ;"           If is "_", then a line is shown for edit field
  ;"       X -- OPTION. X position for editing at given position on screen
  ;"       Y -- OPTION. Y position for editing at given position on screen
  ;"       MAXSTRLEN -- OPTIONAL.  Max allow length for input
  ;"Result: Returns edited value of string
  NEW OPTION 
  SET OPTION("WIDTH")=$GET(WIDTH)
  SET OPTION("FILLCH")=$GET(FILLCH)
  IF $GET(X)>0 SET OPTION("X")=X
  IF $GET(Y)>0 SET OPTION("Y")=Y
  IF $GET(MAXSTRLEN)>0 SET OPTION("MAX STR LEN")=MAXSTRLEN
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
  ;"          OPTION("ON-UP")="Q"  -- quits on UP keystroke
  ;"          OPTION("ON-DN")="Q"  -- quits on DOWN keystroke
  ;"          OPTION("ON-<other keystroke name")="Q" -- quit on other stroke.  
  ;"          OPTION("MAX STR LEN") -- OPTIONAL.  Maximal length of string.  
  ;"Output: OPTION("ESCKEY") holds last escape keystroke encountered.  
  ;"Result: Returns edited value of string
  NEW WIDTH SET WIDTH=+$GET(OPTION("WIDTH")) SET:(WIDTH'>0) WIDTH=40
  NEW VAL SET VAL=$GET(INITVAL)
  SET FILLCH=$EXTRACT($GET(OPTION("FILLCH")),1) IF FILLCH="" SET FILLCH=" "
  NEW HOMEX,HOMEY,USEY SET USEY=$DATA(OPTION("Y"))
  SET HOMEX=+$GET(OPTION("X")) IF HOMEX'>0 SET HOMEX=$X+1
  SET HOMEY=+$GET(OPTION("Y")) IF HOMEY'>0 SET HOMEY=$Y
  NEW MAXSTRL SET MAXSTRL=+$GET(OPTION("MAX STR LEN"))
  IF MAXSTRL=0 SET MAXSTRL=999999999  ;"a really long string 
  NEW DRAWIDX SET DRAWIDX=1
  NEW CPOS SET CPOS=$LENGTH(VAL)+1  ;"Abs position in text
  NEW INPUT,STRA,STRB,CH,DONE,ABORT SET (DONE,ABORT)=0
  NEW LNUMHIDE,RNUMHIDE,LNUMDOTS,RNUMDOTS,RNUMSPC,SCRNCPOS,LENVAL
LOOP ;  
  SET LENVAL=$LENGTH(VAL)
  IF DRAWIDX<1 SET DRAWIDX=1
  SET LNUMHIDE=DRAWIDX-1
  SET RNUMHIDE=LENVAL-WIDTH-LNUMHIDE SET:(RNUMHIDE<0) RNUMHIDE=0
  SET LNUMDOTS=$SELECT(LNUMHIDE>2:2,1:LNUMHIDE)
  SET RNUMDOTS=$SELECT(RNUMHIDE>2:2,1:RNUMHIDE)
  SET SCRNCPOS=CPOS-DRAWIDX+1
  DO  ;"DRAW TO SCREEN 
  . IF USEY DO CUP^TMGTERM(HOMEX,HOMEY)  ;"move cursor to starting position
  . ELSE  DO CHA^TMGTERM(HOMEX)
  . NEW STR,LEN SET STR=$EXTRACT(VAL,DRAWIDX,DRAWIDX+WIDTH-1),LEN=$LENGTH(STR)
  . NEW JDX FOR JDX=1:1:LNUMDOTS SET $EXTRACT(STR,JDX)="."
  . FOR JDX=1:1:RNUMDOTS SET $EXTRACT(STR,LEN-JDX+1)="."
  . SET RNUMSPC=WIDTH-$LENGTH(STR)
  . IF FILLCH'=" " FOR JDX=1:1:RNUMSPC SET STR=STR_FILLCH
  . WRITE STR,"  "
  . NEW TEMPX SET TEMPX=HOMEX+SCRNCPOS-1
  . IF USEY DO CUP^TMGTERM(TEMPX,HOMEY)
  . ELSE  DO CHA^TMGTERM(TEMPX)
  SET INPUT=$$READKY^TMGUSRI5("e",,1,,.ESCKEY) ;"read one char, with ESC processing
  SET OPTION("ESCKEY")=ESCKEY
  SET STRA=$EXTRACT(VAL,1,CPOS-1)       ;"text to left of cursor
  SET CH=$EXTRACT(VAL,CPOS)             ;"text under cursor
  SET STRB=$EXTRACT(VAL,CPOS+1,LENVAL)  ;"text to right of cursor
  IF INPUT="" DO
  . IF ESCKEY="CR" SET DONE=1 QUIT
  . IF ESCKEY="TAB" SET DONE=1 QUIT
  . IF ESCKEY="BACKSPC" DO  QUIT
  . . IF $X=HOMEX DO  QUIT  ;"//kt 2/4/22
  . . . WRITE $CHAR(7) ;"issues a beep sound
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
  . . DO CHA^TMGTERM(HOMEX)  ;"GO TO X POS
  . . SET %=2 WRITE "Abort edit" DO YN^DICN
  . . DO CHA^TMGTERM(HOMEX)  ;"GO TO X POS
  . . WRITE "                        "
  . . DO CHA^TMGTERM(HOMEX)  ;"GO TO X POS
  . . IF %'=2 SET (DONE,ABORT)=1
  . IF $GET(OPTION("ON-"_ESCKEY))="Q" DO  QUIT
  . . SET DONE=1
  ELSE  DO
  . IF $LENGTH(VAL)'<MAXSTRL DO  QUIT    ;"//kt 2/4/22   
  . . WRITE $CHAR(7) ;"issues a beep sound 
  . SET VAL=STRA_INPUT_CH_STRB
  . SET CPOS=CPOS+1
  . IF (CPOS>WIDTH),($LENGTH(VAL)<MAXSTRL) SET DRAWIDX=DRAWIDX+1
  IF 'DONE GOTO LOOP
EBDN  ;
  IF ABORT SET VAL=""
  QUIT VAL
  ;