TMGUSRIF ;TMG/kst/USER INTERFACE API FUNCTIONS ;4/24/15 8/30/17
         ;;1.0;TMG-LIB;**1**;07/12/05
 ;
 ;"TMG USER INTERFACE API FUNCTIONS
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
 ;"SCROLLER(TMGPSCRLARR,OPTION) -- Provide a scroll box interface
 ;"TESTSCRL 
 ;
 ;"=======================================================================
 ;"Private Functions
 ;"=======================================================================
 ;"WCLTEXT(TEXT,SCRNW,OPTION) ;"WRITE COLORED TEXT
 ;"NOCOLEN(TEXT) ;"Length with {{color tags}} stripped
 ;"SETCOLOR(LABEL,OPTION)
 ;"PARSCOLR(TEXT,TEXTA)
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"TMGUSRI*, TMGTERM, TMGKERNL
 ;"=======================================================================
 ;
MENU(OPTION,DEFCHOICE,USERRAW) ;
  ;"Depreciatged.  Use code in ^TMGUSRI2
  QUIT $$MENU^TMGUSRI2(.OPTION,.DEFCHOICE,.USERRAW)
  ;
  ;"============================================================
  ;                                      
SCROLLER(TMGPSCRLARR,OPTION) ;
  ;"Purpose: Provide a scroll box
  ;"Input: TMGPSCRLARR -- PASS BY NAME.  format:
  ;"         @TMGPSCRLARR@(1,DisplayText)=Return Text <-- note: must be numbered 1,2,3 etc.
  ;"         @TMGPSCRLARR@(2,DisplayText)=Return Text
  ;"         @TMGPSCRLARR@(3,DisplayText)=Return Text
  ;"              NOTE: IF Display TEXT contains {{name}} then name is taken as color directive
  ;"              Example: 'Here is {{BOLD}}something{{NORM}} to see.'
  ;"              IF NAME is not defined in OPTION("COLORS",NAME), it is ignored
  ;"       OPTION -- PASS BY REFERENCE.  format:
  ;"          OPTION("HEADER",1)=Header line TEXT
  ;"          OPTION("HEADER",2)=More Header line TEXT (any number of lines)
  ;"          OPTION("FOOTER",1)=Footer line TEXT  <--- OPTION 1
  ;"          OPTION("FOOTER",1,1)=linePart <--- OPTION 2  (these will be all strung together to make one footer line.
  ;"          OPTION("FOOTER",1,2)=linePart                (can be used to display switches etc)
  ;"          OPTION("FOOTER",2)=More footer line TEXT (any number of lines)
  ;"          OPTION("SHOW INDEX")=1 OPTIONal.  If 1, then IDX is shown.
  ;"          OPTION("SCRN WIDTH")= OPTIONal screen width. (default is terminal width)
  ;"          ---- COLORs (optional) ------
  ;"          OPTION("COLORS","NORM")=FG^BG  -- default foreground (FG) and background(colors)
  ;"                 If not provided, White on Blue used.
  ;"          OPTION("COLORS","HIGH")=FG^BG  -- Highlight colors. If not provided, White on Cyan used.
  ;"          OPTION("COLORS","HEADER")=FG^BG  Header color.  NORM used IF not provided
  ;"          OPTION("COLORS","FOOTER")=FG^BG  Footer color.  NORM used IF not provided
  ;"          OPTION("COLORS","TOP LINE")=FG^BG  Top line color.  NORM used IF not provided
  ;"          OPTION("COLORS","BOTTOM LINE")=FG^BG  Bottom line color.  NORM used IF not provided
  ;"          OPTION("COLORS","INDEX")=FG^BG  Index color.  NORM used IF not provided
  ;"          OPTION("COLORS",SomeName)=FG^BG  e.g. :
  ;"                 OPTION("COLORS","BOLD")=15^0  (Any arbitrary name OK, matched to {{name}} in TEXT)
  ;"                 OPTION("COLORS","HIGH")=10^@
  ;"                 If BG="@", then default BG used. This may be used anywhere except for defining NORM
  ;"          ---- events ----
  ;"          OPTION("ON SELECT")="FnName^Module" -- code to call based on user input.  E.g. DO FnName^Module(TMGPSCRLARR,.OPTION,.INFO)
  ;"                  INFO("CURRENT LINE","NUMBER")=number currently highlighted line
  ;"                  INFO("CURRENT LINE","TEXT")=Text of currently highlighted line
  ;"                  INFO("CURRENT LINE","RETURN")=return value of currently highlighted line
  ;"          OPTION("ON CHANGING")="FnName^Module" -- code to execute for number entry  E.g. DO FnName^Module(TMGPSCRLARR,.OPTION,.INFO)
  ;"                  INFO("CURRENT LINE","NUMBER")=number currently highlighted line
  ;"                  INFO("CURRENT LINE","TEXT")=Text of currently highlighted line
  ;"                  INFO("CURRENT LINE","RETURN")=return value of currently highlighted line
  ;"                  INFO("NEXT LINE","NUMBER")=next line number. Used for ON CHANGING to show the line about to be selected
  ;"                  INFO("ALLOW CHANGE")=1, <--- RETURN RESULT.  Change to 0 to disallow move.
  ;"          OPTION("ON CMD")="FnName^Module" -- code to execute for number entry      E.g. DO FnName^Module(TMGPSCRLARR,.OPTION,.INFO)
  ;"                  INFO("USER INPUT")=UserTypedInput
  ;"          OPTION("ON KEYPRESS")="FnName^Module" -- code to execute for user key press      E.g. DO FnName^Module(TMGPSCRLARR,.OPTION,.INFO)
  ;"                  INFO("USER INPUT")=Key pressed
  ;"                  INFO("CMD")=User full input command so far (being built up)
  ;"          OPTION("ON CURSOR")="FnName^Module" -- code to execute for user cursor key press      E.g. DO FnName^Module(TMGPSCRLARR,.OPTION,.INFO)
  ;"                  INFO("CURSOR")=Cursor Key pressed
  ;"                  INFO("CURSOR","HANDLED")=1 <-- this is what called code should set if it handled
  ;"                                  the cursor event. This will prevent scroller from acting on it.   
  ;"          NOTES about events.  Functions will be called as follows:
  ;"              DO FnName^Module(TMGPSCRLARR,.OPTION,.INFO)
  ;"                TMGPSCRLARR and OPTION are the same data received by this function
  ;"                  -- thus OPTION can be used to can other custom information.
  ;"                INFO has extra info as outlined above.
  ;"              If functions may SET a globally-scoped var named TMGSCLRMSG to communicate back
  ;"                      IF TMGSCLRMSG="^" then Scroller will exit
  ;"Result: none
  ;
  NEW SCRNW,SCRNH,SCRNLINE,SPACELINE,TOPLINE,SIZEHDR,SIZEFTR
  NEW ENTRYCT,LINECT,ESCKEY,DISPHT,HIGHLINE,SHOWIDX
  NEW NEEDREFRESH,INFO
  NEW BUILDCMD SET BUILDCMD=""
  SET TOPLINE=1                                                            
  SET HIGHLINE=5
  NEW TMGSCLRMSG SET TMGSCLRMSG=""
  ;
  SET SCRNW=+$GET(OPTION("SCRN WIDTH"))
  IF SCRNW'>0 DO
  . IF $$GETSCRSZ^TMGKERNL(,.SCRNW)                                            
  . SET SCRNW=+SCRNW-4
  IF SCRNW'>0 SET SCRNW=$GET(IOM,66)-2
  SET SCRNH=$GET(IOSL,25)-2
  ;
  IF $GET(OPTION("COLORS","NORM"))="" SET OPTION("COLORS","NORM")="14^4" ;"white on blue
  IF $GET(OPTION("COLORS","HIGH"))="" SET OPTION("COLORS","HIGH")="14^6" ;"white on cyan
  IF $GET(OPTION("COLORS","HEADER"))="" SET OPTION("COLORS","HEADER")=OPTION("COLORS","NORM")
  IF $GET(OPTION("COLORS","FOOTER"))="" SET OPTION("COLORS","FOOTER")=OPTION("COLORS","NORM")
  IF $GET(OPTION("COLORS","TOP LINE"))="" SET OPTION("COLORS","TOP LINE")=OPTION("COLORS","NORM")
  IF $GET(OPTION("COLORS","BOTTOM LINE"))="" SET OPTION("COLORS","BOTTOM LINE")=OPTION("COLORS","NORM")
  IF $GET(OPTION("COLORS","INDEX"))="" SET OPTION("COLORS","INDEX")=OPTION("COLORS","NORM")
  ;
  NEW IDX SET IDX=""                          
  FOR  SET IDX=$ORDER(OPTION("COLORS",IDX)) QUIT:(IDX="")  DO
  . NEW COLORS SET COLORS=$GET(OPTION("COLORS",IDX))
  . NEW FG SET FG=$PIECE(COLORS,"^",1) IF FG="" SET FG=0
  . NEW BG SET BG=$PIECE(COLORS,"^",2) IF BG="" SET BG=1
  . SET OPTION("COLORS",IDX,"FG")=FG
  . SET OPTION("COLORS",IDX,"BG")=BG
  ;
FULL  ;
  SET SCRNLINE="" SET $PIECE(SCRNLINE,"-",SCRNW)="-"
  SET SPACELINE="" SET $PIECE(SPACELINE," ",SCRNW)=" "
  SET SIZEHDR=$$LISTCT^TMGMISC2($name(OPTION("HEADER")))+1
  SET SIZEFTR=$$LISTCT^TMGMISC2($name(OPTION("FOOTER")))+1
  SET ENTRYCT=$$LISTCT^TMGMISC2(TMGPSCRLARR)
  SET ESCKEY=""
  SET DISPHT=SCRNH-SIZEHDR-SIZEFTR
  IF TOPLINE>ENTRYCT SET TOPLINE=ENTRYCT
  IF HIGHLINE>ENTRYCT SET HIGHLINE=ENTRYCT
  SET SHOWIDX=($GET(OPTION("SHOW INDEX"))=1)
  ;
DRAW  ;
  DO HOME^TMGTERM
  IF $DATA(OPTION("HEADER")) DO
  . DO SETCOLOR("HEADER",.OPTION)
  . NEW IDX SET IDX=""
  . FOR  SET IDX=$ORDER(OPTION("HEADER",IDX)) QUIT:(IDX="")  DO
  . . NEW TEXT SET TEXT=$GET(OPTION("HEADER",IDX))
  . . NEW LEN SET LEN=$$NOCOLEN(TEXT)
  . . NEW PAD SET PAD=SCRNW-LEN
  . . NEW HALFPAD SET HALFPAD=PAD\2
  . . NEW IDX FOR IDX=1:1:HALFPAD SET TEXT=" "_TEXT
  . . IF HALFPAD'=(PAD/2) SET HALFPAD=(PAD\2)+1
  . . NEW IDX FOR IDX=1:1:HALFPAD SET TEXT=TEXT_" "
  . . DO WCLTEXT(TEXT,SCRNW,.OPTION) WRITE !
  ;
  SET LINECT=TOPLINE
  DO SETCOLOR("TOP LINE",.OPTION)
  WRITE SCRNLINE,!
  DO SETCOLOR("NORM",.OPTION)
  FOR  QUIT:(LINECT=(DISPHT+TOPLINE-1))  DO
  . IF LINECT=HIGHLINE DO SETCOLOR("HIGH",.OPTION)
  . ELSE  DO SETCOLOR("NORM",.OPTION)
  . NEW STR SET STR=""
  . IF SHOWIDX DO
  . . DO SETCOLOR("INDEX",.OPTION)
  . . WRITE $$RJ^XLFSTR(LINECT,3)_"."
  . . IF LINECT=HIGHLINE DO SETCOLOR("HIGH",.OPTION)
  . . ELSE  DO SETCOLOR("NORM",.OPTION)
  . . WRITE " "
  . NEW TEXT,TEXTA,TEXTB,TEXTCOLOR
  . SET TEXT=$ORDER(@TMGPSCRLARR@(LINECT,""))
  . DO WCLTEXT(TEXT,SCRNW,.OPTION) ;
  . WRITE $EXTRACT(SPACELINE,1,(SCRNW-$X))
  . DO SETCOLOR("RESET") WRITE !
  . SET LINECT=LINECT+1
  DO SETCOLOR("BOTTOM LINE",.OPTION)
  WRITE SCRNLINE,!
  DO SETCOLOR("FOOTER",.OPTION)
  IF $DATA(OPTION("FOOTER")) DO
  . NEW IDX SET IDX=""
  . FOR  SET IDX=$ORDER(OPTION("FOOTER",IDX)) QUIT:(IDX="")  DO
  . . NEW JDX SET JDX=$ORDER(OPTION("FOOTER",IDX,""))
  . . IF JDX'="" DO  ;"wrap line IF needed
  . . . NEW ONELINE SET ONELINE="",JDX=""
  . . . FOR  SET JDX=$ORDER(OPTION("FOOTER",IDX,JDX)) QUIT:(JDX="")  DO
  . . . . NEW ONEPART SET ONEPART=$GET(OPTION("FOOTER",IDX,JDX))_" | "
  . . . . IF $LENGTH(ONELINE_ONEPART)>SCRNW DO
  . . . . . WRITE ONELINE
  . . . . . NEW REMAINING SET REMAINING=SCRNW-$LENGTH(ONELINE)
  . . . . . WRITE $EXTRACT(SPACELINE,1,REMAINING),!
  . . . . . SET ONELINE=""
  . . . . SET ONELINE=ONELINE_ONEPART
  . . . WRITE $$LJ^XLFSTR(ONELINE,SCRNW),!
  . . ELSE  DO  ;"wrap line if needed
  . . . NEW STR SET STR=$GET(OPTION("FOOTER",IDX))
  . . . FOR  DO  QUIT:STR=""
  . . . . NEW STRA
  . . . . IF $LENGTH(STR)'>SCRNW DO
  . . . . . SET STRA=$$LJ^XLFSTR(STR,SCRNW)
  . . . . . SET STR=""
  . . . . ELSE  DO
  . . . . . SET STRA=$EXTRACT(STR,1,SCRNW)
  . . . . . SET STR=$EXTRACT(STR,SCRNW+1,$LENGTH(STR))
  . . . . WRITE STRA,!
  ;
  SET INFO("CURRENT LINE","NUMBER")=HIGHLINE
  SET INFO("CURRENT LINE","TEXT")=$ORDER(@TMGPSCRLARR@(HIGHLINE,""))
  SET INFO("CURRENT LINE","RETURN")=$GET(@TMGPSCRLARR@(HIGHLINE,INFO("CURRENT LINE","TEXT")))
  ;
  DO SETCOLOR("RESET")
  WRITE $$LJ^XLFSTR(": ",SCRNW),!
  DO CUU^TMGTERM(1) WRITE ": "_BUILDCMD
  SET NEEDREFRESH=0
USRIN ;
  SET INPUT=$$READKY^TMGUSRI5("re",,1,,.ESCKEY)
  IF (INPUT="")&(ESCKEY="") SET ESCKEY="CR"
  IF $GET(OPTION("ON CURSOR"))'="",INPUT="",$$ISCURSOR(ESCKEY) DO  GOTO:+$GET(INFO("CURSOR","HANDLED"))=1 LP2
  . SET INFO("CURSOR")=ESCKEY
  . SET INFO("CURSOR","HANDLED")=0
  . NEW CODEFN SET CODEFN=$GET(OPTION("ON CURSOR")) QUIT:(CODEFN="")
  . NEW $ETRAP SET $ETRAP="WRITE ""(Invalid M Code!.  Error Trapped.)"",! SET $ETRAP="""",$ecode="""""
  . IF CODEFN'="" SET CODEFN="DO "_CODEFN_"(TMGPSCRLARR,.OPTION,.INFO)"
  . XECUTE CODEFN
  . IF +$GET(INFO("CURSOR","HANDLED"))=1 SET NEEDREFRESH=2
  IF ESCKEY="UP" SET INPUT="UP^1"
  IF ESCKEY="PREV" SET INPUT="UP^15"
  IF ESCKEY="DOWN" SET INPUT="DOWN^1"
  IF ESCKEY="NEXT" SET INPUT="DOWN^15"
  IF ESCKEY="BACKSPC",$LENGTH(BUILDCMD)>0 DO
  . SET BUILDCMD=$EXTRACT(BUILDCMD,1,$LENGTH(BUILDCMD)-1)
  IF ESCKEY="CR" DO  GOTO LP2
  . NEW EVENT
  . IF BUILDCMD'="" SET EVENT="CMD",INFO("USER INPUT")=BUILDCMD
  . ELSE  IF INPUT'="" SET EVENT="CMD",INFO("USER INPUT")=INPUT
  . ELSE  SET EVENT="SELECT"
  . NEW CODEFN SET CODEFN=$GET(OPTION("ON "_EVENT))
  . IF CODEFN="" QUIT
  . SET CODEFN="DO "_CODEFN_"(TMGPSCRLARR,.OPTION,.INFO)"
  . NEW $ETRAP SET $ETRAP="WRITE ""(Invalid M Code!.  Error Trapped.)"",! SET $ETRAP="""",$ecode="""""
  . XECUTE CODEFN
  . SET NEEDREFRESH=2
  . SET BUILDCMD=""
  . KILL INFO("USER INPUT")
  IF INPUT="^" GOTO SCRLDN
  IF (INPUT["^") DO  GOTO LP2
  . IF $PIECE(INPUT,"^",1)="UP" DO
  . . NEW $ETRAP SET $ETRAP="WRITE ""(Invalid M Code!.  Error Trapped.)"",! SET $ETRAP="""",$ecode="""""
  . . NEW CODEFN SET CODEFN=$GET(OPTION("ON CHANGING"))
  . . IF CODEFN'="" SET CODEFN="DO "_CODEFN_"(TMGPSCRLARR,.OPTION,.INFO)"
  . . SET INFO("ALLOW CHANGE")=1
  . . SET NEEDREFRESH=1
  . . NEW JDX FOR JDX=1:1:+$PIECE(INPUT,"^",2) DO
  . . . IF HIGHLINE>TOPLINE DO
  . . . . SET INFO("NEXT LINE","NUMBER")=(HIGHLINE-1)
  . . . . IF CODEFN'="" XECUTE CODEFN QUIT:'$GET(INFO("ALLOW CHANGE"))  SET NEEDREFRESH=2
  . . . . SET HIGHLINE=HIGHLINE-1
  . . . ELSE  IF TOPLINE>1 DO
  . . . . SET INFO("NEXT LINE","NUMBER")=(TOPLINE-1)
  . . . . IF CODEFN'="" XECUTE CODEFN QUIT:'$GET(INFO("ALLOW CHANGE"))  SET NEEDREFRESH=2
  . . . . SET TOPLINE=TOPLINE-1,HIGHLINE=TOPLINE
  . ELSE  IF $PIECE(INPUT,"^",1)="DOWN" DO
  . . NEW $ETRAP SET $ETRAP="WRITE ""(Invalid M Code!.  Error Trapped.)"",! SET $ETRAP="""",$ecode="""""
  . . NEW CODEFN SET CODEFN=$GET(OPTION("ON CHANGING"))
  . . IF CODEFN'="" SET CODEFN="DO "_CODEFN_"(TMGPSCRLARR,.OPTION,.INFO)"
  . . SET INFO("ALLOW CHANGE")=1
  . . SET NEEDREFRESH=1
  . . NEW JDX FOR JDX=1:1:+$PIECE(INPUT,"^",2) DO
  . . . IF HIGHLINE<(TOPLINE+DISPHT-2) DO
  . . . . SET INFO("NEXT LINE","NUMBER")=(HIGHLINE-1)
  . . . . IF CODEFN'="" XECUTE CODEFN QUIT:'$GET(INFO("ALLOW CHANGE"))  SET NEEDREFRESH=2
  . . . . SET HIGHLINE=HIGHLINE+1
  . . . ELSE  IF (TOPLINE+DISPHT-2)<ENTRYCT DO
  . . . . SET INFO("NEXT LINE","NUMBER")=(HIGHLINE+1)
  . . . . IF CODEFN'="" XECUTE CODEFN QUIT:'$GET(INFO("ALLOW CHANGE"))  SET NEEDREFRESH=2
  . . . . SET TOPLINE=TOPLINE+1,HIGHLINE=HIGHLINE+1
  ELSE  IF INPUT="=" DO
  . SET NEEDREFRESH=2
  . NEW DIR SET DIR(0)="N^10:"_IOM
  . SET DIR("B")=SCRNW
  . WRITE "Enter Screen Width (# of columns): " DO ^DIR WRITE !
  . IF $DATA(DIRUT) WRITE # QUIT
  . SET SCRNW=Y
  . SET DIR(0)="N^5:"_(IOSL-2)
  . SET DIR("B")=SCRNH
  . WRITE "Enter Screen Height (# of rows): " DO ^DIR WRITE !
  . IF $DATA(DIRUT) WRITE # QUIT
  . SET SCRNH=Y
  . WRITE #
  ELSE  DO
  . SET NEEDREFRESH=1
  . IF (INPUT="")&(ESCKEY'="") SET INPUT="{"_ESCKEY_"}"
  . ELSE  SET BUILDCMD=BUILDCMD_INPUT
  . NEW CODEFN SET CODEFN=$GET(OPTION("ON KEYPRESS")) QUIT:(CODEFN="")
  . NEW $ETRAP SET $ETRAP="WRITE ""(Invalid M Code!.  Error Trapped.)"",! SET $ETRAP="""",$ecode="""""
  . IF CODEFN'="" SET CODEFN="DO "_CODEFN_"(TMGPSCRLARR,.OPTION,.INFO)"
  . SET INFO("USER INPUT")=INPUT
  . SET INFO("CMD")=BUILDCMD
  . XECUTE CODEFN
  . SET NEEDREFRESH=2
  ;
LP2  ;
  IF TMGSCLRMSG="^" GOTO SCRLDN
  IF NEEDREFRESH=2 GOTO FULL
  IF NEEDREFRESH=1 GOTO DRAW
  GOTO USRIN
  ;
SCRLDN  ;
  QUIT
  ;
ISCURSOR(ESCKEY) ;
  ;"FYI: FIND=home key and SELECT=end key
  QUIT "UP^DOWN^LEFT^RIGHT^PREV^NEXT^FIND^SELECT^"[ESCKEY_"^"
  ;  
WCLTEXT(TEXT,SCRNW,OPTION) ;"WRITE COLORED TEXT
  NEW TEXTA,TEXTB,TEXTCOLOR
  NEW WIDTH
  FOR  QUIT:(TEXT'["{{")!($X'<SCRNW)  DO
  . SET TEXTCOLOR=$$PARSCOLR(.TEXT,.TEXTA)  ;" Text --> TextA{{COLOR}}Text
  . SET WIDTH=$X
  . SET WIDTH=WIDTH+$LENGTH(TEXTA)-1
  . IF WIDTH>SCRNW DO
  . . WRITE $EXTRACT(TEXTA,1,(SCRNW-$X-3)+1)_"..."
  . ELSE  WRITE TEXTA
  . DO SETCOLOR(TEXTCOLOR,.OPTION)
  SET WIDTH=$X
  SET WIDTH=WIDTH+$LENGTH(TEXT)-1
  IF WIDTH>SCRNW DO
  . WRITE $EXTRACT(TEXT,1,(SCRNW-$X-3)+1)_"..."
  ELSE  WRITE TEXT
  QUIT
  ;
NOCOLEN(TEXT) ;"Length with {{color tags}} stripped
  NEW RESULT SET RESULT=0
  IF TEXT'["{{" SET RESULT=$LENGTH(TEXT) GOTO NCOLDN
  NEW FRAGLEN,SP,EP SET EP=0,SP=1
NCOL1  ;
  SET EP=$FIND(TEXT,"{{",SP)-3
  IF EP<0 SET EP=$LENGTH(TEXT)
  SET FRAGLEN=EP-SP+1
  SET RESULT=RESULT+FRAGLEN
  SET SP=$FIND(TEXT,"}}",EP)
  IF (SP>0)&(SP<$LENGTH(TEXT)) GOTO NCOL1
NCOLDN  ;
  QUIT RESULT
  ;
SETCOLOR(LABEL,OPTION) ;
  ;"Purpose: to SET color, based on LABEL name. (A utility function for Scroller)
  ;"Input: LABEL -- the name of the color, i.e. NORM, HIGH, etc.
  ;"              If LABEL=REST, then special ResetTerminal function called.
  ;"       OPTION -- PASS BY REFERENCE.  The same option array passed to Scroller, with color info
  ;"                Specifically used: OPTION('COLORS',SomeName,'FG')=foregroundCOLOR
  ;"                                 OPTION('COLORS',SomeName,'BG')=backgroundCOLOR
  ;"Note: IF color label not found, then no color change is made.
  ;
  IF LABEL="RESET" DO VTATRIB^TMGTERM(0) QUIT  ;"reset colors
  IF $DATA(OPTION("COLORS",LABEL))=0 QUIT
  NEW FG SET FG=$GET(OPTION("COLORS",LABEL,"FG"),1) ;"default to black
  NEW BG SET BG=$GET(OPTION("COLORS",LABEL,"BG"),0) ;"default to white
  IF BG="@" SET BG=$GET(OPTION("COLORS","NORM","BG"),0) ;"default to white
  DO VCOLORS^TMGTERM(FG,BG)
  QUIT
  ;
PARSCOLR(TEXT,TEXTA)  ;
  ;"Purpose: To extract a color code from TEXT
  ;"Example:  Input TEXT  = 'This is {{HIGH}}something{{NORM}} to see.'
  ;"          Output TEXT = 'something{{NORM}} to see.'
  ;"          Output TEXTA = 'This is '
  ;"            function result = 'NORM'
  ;"Input: TEXT -- PASS BY REFERENCE
  ;"         TEXTA -- PASS BY REFERENCE, and OUT PARAMETER
  ;"Result: the color name inside brackets.
  NEW STR,RESULT
  SET STR=TEXT
  SET TEXTA=$PIECE(STR,"{{",1)
  SET RESULT=$PIECE(STR,"{{",2)
  SET RESULT=$PIECE(RESULT,"}}",1)
  SET TEXT=$PIECE(STR,"}}",2,99)
  QUIT RESULT
  ;
 ;"=====================================================
 ;"Test code below
 ;"=====================================================
 ;
TESTSCRL ;
  NEW ARRAY,OPTION
  NEW IDX FOR IDX=1:1:136 DO
  . SET ARRAY(IDX,"Line "_IDX)="Result for "_IDX
  SET OPTION("HEADER",1)=" - < Here is a header line > -"
  SET OPTION("FOOTER",1)="Enter ^ to exit"
  SET OPTION("ON SELECT")="HNDONSEL^TMGUSRIF"
  SET OPTION("ON CMD")="HNDONCMD^TMGUSRIF"
  SET OPTION("ON KEYPRESS")="HNDONKP^TMGUSRIF"
  ;
  SET OPTION("COLORS","NORM")="14^4" ;"white on blue
  SET OPTION("COLORS","HIGH")="14^6" ;"white on cyan
  SET OPTION("COLORS","HEADER")="14^5"
  SET OPTION("COLORS","FOOTER")="14^5"
  SET OPTION("COLORS","TOP LINE")="5^1"
  SET OPTION("COLORS","BOTTOM LINE")="5^1"
  SET OPTION("COLORS","INDEX")="0^1"
  SET OPTION("SHOW INDEX")=1
  ;
  DO SCROLLER("ARRAY",.OPTION)
  QUIT
  ;
HNDONSEL(TMGPSCRLARR,OPTION,INFO)  ;"Part of TESTSCRL
  ;"Purpose: handle ON SELECT event from SCROLLER
  ;"Input: TMGPSCRLARR,OPTION,INFO -- see documentation in SCROLLER
  ;"       INFO has this:
  ;"          INFO("CURRENT LINE","NUMBER")=number currently highlighted line
  ;"          INFO("CURRENT LINE","TEXT")=Text of currently highlighted line
  ;"          INFO("CURRENT LINE","RETURN")=return value of currently highlighted line
  WRITE $GET(INFO("CURRENT LINE","TEXT")),!
  DO PRESS2GO^TMGUSRI2
  QUIT
  ;
HNDONKP(TMGPSCRLARR,OPTION,INFO)  ;"Part of TESTSCRL
  ;"Purpose: handle ON KEYPRESS event from SCROLLER
  ;"Input: TMGPSCRLARR,OPTION,INFO -- see documentation in SCROLLER
  ;"       INFO has this:
  ;"          INFO("CURRENT LINE","NUMBER")=number currently highlighted line
  ;"          INFO("CURRENT LINE","TEXT")=Text of currently highlighted line
  ;"          INFO("CURRENT LINE","RETURN")=return value of currently highlighted line
  WRITE $GET(INFO("USER INPUT")),!
  WRITE $GET(INFO("CMD")),!
  DO PRESS2GO^TMGUSRI2
  QUIT
  ;
HNDONCMD(TMGPSCRLARR,OPTION,INFO)  ;"Part of TESTSCRL
  ;"Purpose: handle ON SELECT event from SCROLLER
  ;"Input: TMGPSCRLARR,OPTION,INFO -- see documentation in SCROLLER
  ;"       INFO has this:
  ;"          INFO("USER INPUT")=INPUT
  ;"          INFO("CURRENT LINE","NUMBER")=number currently highlighted line
  ;"          INFO("CURRENT LINE","TEXT")=Text of currently highlighted line
  ;"          INFO("CURRENT LINE","RETURN")=return value of currently highlighted line
  WRITE !,$GET(INFO("USER INPUT")),!
  DO PRESS2GO^TMGUSRI2
  QUIT
  ;
