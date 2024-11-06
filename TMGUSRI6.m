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
 
TESTMSGDLG  ;
  NEW MSGARR,OPTION
  SET MSGARR(1)="CAUTION"
  SET MSGARR(2)="Central Core Overload"
  SET MSGARR(3)="Imminent!"
  NEW FG,BG IF $$COLORPAIR^TMGUSRI8("YELLOW","BLUE",,.FG,.BG)  ;"ignore result
  SET OPTION("COLOR","FG")=FG,OPTION("COLOR","BG")=BG
  SET OPTION("ALT BUFFER")=1
  NEW RESULT SET RESULT=$$MESSAGEDLG(.MSGARR,.OPTION)
  QUIT
  ;  
TESTEDITDLG  ;
  NEW MSGARR,OPTION
  SET MSGARR(1)="Enter Last Name"
  SET OPTION("INIT VALUE")="Pumpernickle"
  NEW FG,BG IF $$COLORPAIR^TMGUSRI8("YELLOW","BLUE",,.FG,.BG)  ;"ignore result
  SET OPTION("COLOR","FG")=FG,OPTION("COLOR","BG")=BG
  SET OPTION("ALT BUFFER")=1
  NEW RESULT SET RESULT=$$EDITDLG(.MSGARR,.OPTION)
  WRITE !,"User entered: ",RESULT,!
  QUIT
  ;  
MESSAGEDLG(MSGARR,OPTION)  ;"Message Dialog
  SET OPTION("MODE")="MSG"
  IF $$DIALOG(.MSGARR,.OPTION)  ;"ignore result
  QUIT
  ;  
EDITDLG(MSGARR,OPTION)  ;"Message Dialog
  SET OPTION("MODE")="EDIT"
  NEW RESULT SET RESULT=$$DIALOG(.MSGARR,.OPTION)
  QUIT RESULT
  ;
DIALOG(MSGARR,OPTION)  ;"Message Dialog
  ;"INPUT: MSGARR -- Optional.  Pass by reference.  Format:  MSGARR(#)=<line of text>
  ;"                   If passed, then displayed as message above edit area.
  ;"       OPTION -- OPTION.  PASS BY REFERENCE.  Supported options:
  ;"          OPTION("MODE")="MSG" for just displaying message or "EDIT" for editor.  DEFAULT="EDIT"
  ;"          OPTION("MSG JUSTIFY")="LEFT", or "RIGHT", or "CENTER".  OPTIONAL.  Default is CENTER
  ;"          OPTION("INIT VALUE")=<string> Optional.  Initial value for edit box.  Used if MODE="EDIT"
  ;"          OPTION("ALT BUFFER")=1 OPTIONAL.  If 1, then display is changed to ALT BUF just for edit
  ;"          OPTION("POS","X")=X position of top left corner of dialog.  Default will be to center dialog on screen
  ;"          OPTION("POS","Y")=Y position of top left corner of dialog.  Default will be to center dialog on screen
  ;"          OPTION("SCREEN SIZE")=<ROWS^COLS> -- if not provided, then $$GETSCRSZ^TMGKERNL(ROWS,COLS) called to determine size.  
  ;"          OPTION("SAVE STATE")=1 Default = 1.  If 1, then cursor and attributes are saved, and restored at end of edit
  ;"          OPTION("BORDER","OPTIONS") = options for box drawing. See DRAWBOX^TMGTERM2
  ;"          OPTION("COLOR","FG")=  foreground color.  Format same as accepted by COLORS^TMGTERM
  ;"                                 If -1, then terminal color is RESET to default.  If BGCOLOR=-1, FGCOLOR is overridden  
  ;"          OPTION("COLOR","BG")=  background color.  Format same as accepted by COLORS^TMGTERM
  ;"                                 If -1, then terminal color is RESET to default.  If FGCOLOR=-1, BGCOLOR is overridden    
  ;"          OPTION("WIDTH") -- OPTIONAL.  Default to 40. NOTE: if MSGARR has wider text, MSGARR will be wrapped to WIDTH
  ;"          OPTION("FILLCH") -- OPTIONAL.  DEFAULT IS "_"   Used if MODE="EDIT"
  ;"                  If is "_", then a line is shown for edit field
  ;"          OPTION("ON-UP")="Q"  -- quits on UP keystroke
  ;"          OPTION("ON-DN")="Q"  -- quits on DOWN keystroke
  ;"          OPTION("ON-<other keystroke name")="Q" -- quit on other stroke.  
  ;"          OPTION("MAX STR LEN") -- OPTIONAL.  Maximal length of string.  
  ;"Output: OPTION("ESCKEY") holds last escape keystroke encountered.  
  ;"Results: Returns edited value.
  NEW MODE SET MODE=$GET(OPTION("MODE"))
  NEW RESULT SET RESULT=$GET(OPTION("INIT VALUE"))
  NEW SAVESTATE SET SAVESTATE=($GET(OPTION("SAVE STATE"),1)=1)
  IF SAVESTATE DO VCUSAV2^TMGTERM
  NEW ALTBUF SET ALTBUF=($GET(OPTION("ALT BUFFER"))=1)
  IF ALTBUF DO ALTBUF^TMGTERM(1)
  NEW X,Y SET X=+$GET(X),Y=+$GET(Y)
  NEW MSGWIDTH SET MSGWIDTH=$$MAXWIDTH^TMGSTUT2(.MSGARR)
  NEW MSGHEIGHT SET MSGHEIGHT=$$LISTCT^TMGMISC2("MSGARR")
  NEW EDITWIDTH SET EDITWIDTH=+$GET(OPTION("WIDTH")) IF EDITWIDTH'>0 SET EDITWIDTH=40
  IF MSGWIDTH>EDITWIDTH DO WordWrapArray^TMGSTUTL(.MSGARR,EDITWIDTH) SET MSGWIDTH=EDITWIDTH
  NEW JUSTIFY SET JUSTIFY=$$UP^XLFSTR($GET(OPTION("MSG JUSTIFY"),"CENTER"))
  DO  ;"Make every line in MSG array to be full width, and justified.   
  . NEW IDX SET IDX=0 
  . FOR  SET IDX=$ORDER(MSGARR(IDX)) QUIT:IDX'>0  DO
  . . NEW LINE SET LINE=$GET(MSGARR(IDX)) QUIT:LINE=""
  . . IF JUSTIFY="RIGHT"  SET LINE=$$RJ^XLFSTR(LINE,EDITWIDTH," "),MSGARR(IDX)=LINE QUIT
  . . IF JUSTIFY="LEFT"   SET LINE=$$LJ^XLFSTR(LINE,EDITWIDTH," "),MSGARR(IDX)=LINE QUIT
  . . IF JUSTIFY="CENTER" SET LINE=$$CJ^XLFSTR(LINE,EDITWIDTH," "),MSGARR(IDX)=LINE QUIT    
  NEW BOXWT,BOXHT SET BOXWT=EDITWIDTH+2
  NEW BOXHT SET BOXHT=1+MSGHEIGHT+2+$SELECT(MODE="EDIT":1,MODE="MSG":0,1:0)
  NEW BOXX,BOXY SET BOXX=+$GET(OPTION("POS","X")),BOXY=+$GET(OPTION("POS","Y"))
  IF (BOXX'>0)!(BOXY'>0) DO
  . NEW SCRNSIZE SET SCRNSIZE=$GET(OPTION("SCREEN SIZE"))
  . NEW SCRNX,SCRNY SET SCRNX=$PIECE(SCRNSIZE,"^",2),SCRNY=$PIECE(SCRNSIZE,"^",1)
  . IF (SCRNX'>0)!(SCRNY'>0) SET SCRNSIZE=$$GETSCRSZ^TMGKERNL(.SCRNY,.SCRNX)
  . SET BOXX=(SCRNX-BOXWT)\2
  . SET BOXY=(SCRNY-BOXHT)\2
  NEW FG SET FG=$GET(OPTION("COLOR","FG")) IF FG="" SET FG="255;255;255"  ;"WHITE 
  NEW BG SET BG=$GET(OPTION("COLOR","BG")) IF BG="" SET BG="0;0;0"        ;"BLACK
  NEW FILLSTR SET $PIECE(FILLSTR," ",EDITWIDTH)=" "
  NEW BOXOPTION MERGE BOXOPTION=OPTION("BORDER","OPTIONS")
  NEW FILLCH SET FILLCH=$GET(OPTION("FILLCH"),"_")
  ;"DRAW DIALOG
  DO COLORS^TMGTERM(FG,BG)
  NEW IDX SET IDX=1
  NEW Y FOR Y=BOXY+1:1:BOXY+BOXHT-1 DO
  . DO CUP^TMGTERM(BOXX+1,Y)
  . NEW LINE SET LINE=$GET(MSGARR(IDX)) SET IDX=IDX+1
  . IF LINE="" SET LINE=FILLSTR
  . WRITE LINE 
  DO CUP^TMGTERM(BOXX+1,BOXY+BOXHT-2) WRITE $$CJ^XLFSTR("(Press ENTER when done)",EDITWIDTH," ")
  DO DRAWBOX^TMGTERM2(BOXX,BOXY,BOXWT,BOXHT,FG,BG,.BOXOPTION)
  IF MODE="EDIT" DO
  . ;"LAUNCH EDITOR
  . NEW INITVAL SET INITVAL=$GET(OPTION("INIT VALUE"))
  . SET RESULT=$$EDITBOX(INITVAL,EDITWIDTH-1,FILLCH,BOXX+1,BOXY+BOXHT-3)
  ELSE  IF MODE="MSG" DO
  . NEW TEMP READ TEMP  ;"wait for ENTER
  ;"RESTORE SCREEN.  
  IF ALTBUF DO ALTBUF^TMGTERM(0)
  IF SAVESTATE DO
  . DO VCULOAD2^TMGTERM
  . DO COLORS^TMGTERM(-1,-1)  ;"reset colors
  QUIT RESULT 
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
  NEW RESULT SET RESULT=$$EDITBOX2(.INITVAL,.OPTION)
  QUIT RESULT 
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
  . WRITE STR," "
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