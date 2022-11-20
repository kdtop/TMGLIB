TMGUSRIF ;TMG/kst/USER INTERFACE API FUNCTIONS ;7/6/22
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
 ;"TESTSCRL --A DEMONSTRATION OF SCROLLER
 ;"DEMOSCRL --ANOTHER DEMONSTRATION OF SCROLLER
 ;"DEMOCOLS --DEMONSTRATE SCROLLER WITH COLUMNS.  
 ;
 ;"=======================================================================
 ;"Private Functions
 ;"=======================================================================
 ;"WCLTEXT(TEXT,MAXX,OPTION) --WRITE COLORED TEXT
 ;"NOCOLEN(TEXT) -- Length with {{color tags}} stripped
 ;"SETCOLOR(LABEL,OPTION)
 ;"PARSCOLR(TEXT,TEXTA)
 ;"HASCOLOR(TEXT) -- determine if string has {{color tags}} 
 ;"STRIPCOLOR(TEXT) -- Strip out {{color tags}}
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
  ;"              IF NAME is not defined in OPTION("COLORS",NAME), it is ignored, and code is shown. 
  ;"         @TMGPSCRLARR@("SELECTED",<#>)=1 if selected
  ;"         @TMGPSCRLARR@("COL",<COL#>,<index>,<#>)=Display text for column 2.  <index> is the # from above to link it to.
  ;"                         NOTE: This is ignored unless OPTION("COLUMNS","NUM") > 1
  ;"            e.g. @TMGPSCRLARR@("COL",2,3,1)="This text will be shown when index 3 is" 
  ;"                 @TMGPSCRLARR@("COL",2,3,2)="  selected in the left-hand column (Col#1)" 
  ;"                 @TMGPSCRLARR@("COL",2,3,3)="  ..."   
  ;"                 @TMGPSCRLARR@("COL",2,"SELECTED",<#>)=1 if selected
  ;"       OPTION -- PASS BY REFERENCE.  format:
  ;"          OPTION("HEADER",1)=Header line TEXT
  ;"          OPTION("HEADER",2)=More Header line TEXT (any number of lines)
  ;"          OPTION("FOOTER",1)=Footer line TEXT  <--- OPTION 1
  ;"          OPTION("FOOTER",1,1)=linePart <--- OPTION 2  (these will be all strung together to make one footer line.
  ;"          OPTION("FOOTER",1,2)=linePart                (can be used to display switches etc)
  ;"          OPTION("FOOTER",2)=More footer line TEXT (any number of lines)
  ;"          OPTION("SHOW INDEX")=1 OPTIONAL.  If 1, then each line displayed with line number at beginning
  ;"          OPTION("SCRN WIDTH")= OPTIONAL screen width. (default is terminal width)
  ;"          OPTION("SCRN HEIGHT")= OPTIONAL screen height. (default is terminal height (IOSL) - 2)
  ;"          ---- COLORs (optional) ------
  ;"          OPTION("COLORS","NORM")=FG^BG  -- default foreground (FG) and background(colors)
  ;"                 If not provided, White on Blue used.
  ;"          OPTION("COLORS","HIGH")=FG^BG  -- Highlight colors. If not provided, White on Cyan used.
  ;"          OPTION("COLORS","HEADER")=FG^BG  Header color.  NORM used IF not provided
  ;"          OPTION("COLORS","SELECTED")=FG^BG Color when line selected (in multi-select mode)
  ;"          OPTION("COLORS","HI-SEL")=FG^BG Color when line highlighted AND selected (in multi-select mode)
  ;"          OPTION("COLORS","FOOTER")=FG^BG  Footer color.  NORM used IF not provided
  ;"          OPTION("COLORS","TOP LINE")=FG^BG  Top line color.  NORM used IF not provided
  ;"          OPTION("COLORS","BOTTOM LINE")=FG^BG  Bottom line color.  NORM used IF not provided
  ;"          OPTION("COLORS","INDEX")=FG^BG  Index color.  NORM used IF not provided
  ;"          OPTION("COLORS",SomeName)=FG^BG  e.g. :
  ;"                 OPTION("COLORS","BOLD")=15^0  (Any arbitrary name OK, matched to {{name}} in TEXT)
  ;"                 OPTION("COLORS","HIGH")=10^@
  ;"                 If BG="@", then default BG used. This may be used anywhere except for defining NORM
  ;"          OPTION("MULTISEL")=1 -- will allow user to multi-select items.  
  ;"                 Toggle status: [INSERT] key or [+] key, or [SPACE] key as a first character  
  ;"                 CTRL-A key will toggle select status of all items.  
  ;"          OPTION("HIGHLINE")=<line number>  OPTIONAL.  Default=5.  This is line cursor is on initially. May be modified by events, see below
  ;"                 NOTE: This will be used if $G(OPTION("COLUMNS","ACTIVE"))=1 (default), 
  ;"                       otherwise OPTION("COLUMNS",<COL#>,"HIGHLINE") used 
  ;"          OPTION("LR SCROLLING") = 1   Default = 0. If 1, then left, right keys scroll text rather than passing to user functions
  ;"          ---- Multi-column mode ----
  ;"          OPTION("COLUMNS","ACTIVE") = OPTIONAL.   Default = 1. Which column user can scroll currently
  ;"          OPTION("COLUMNS","NUM") = OPTIONAL.   Default = 1. Number of columns to show
  ;"          OPTION("COLUMNS","FN TOGGLE NUM")=<FN NAME> OPTIONAL. e.g. "F9"  Default = "". Fn is shown as option to cycle through which column is active
  ;"                  Only applies if OPTION("COLUMNS","NUM")>1 
  ;"          OPTION("COLUMNS",<COL#>,"WIDTH") = OPTIONAL. e.g "40" (absolute) or "75%" (% of screen width)  
  ;"          OPTION("COLUMNS",<COL#>,"WIDTH") = OPTIONAL. e.g "20" (absolute) or "25%" (% of screen width) 
  ;"                                       NOTE: if total absolute > SCRN WIDTH, or percentages total > 100%, they will be changed to fit.
  ;"                                             If not provided, then default will be even spacing.  
  ;"          OPTION("COLUMNS",<COL#>,"SHOW INDEX")=1 OPTIONAL.  If 1, then each line displayed with line number at beginning
  ;"          OPTION("COLUMNS",<COL#>,"COLORS",(SEE ABOVE)) -- same color options as above, but for column#
  ;"                                NOTE: Columns colors don't use HEADER, FOOTER, TOPLINE,BOTTOMLINE, so would be ignored  
  ;"          OPTION("COLUMNS",<COL#>,"HIGHLINE")=<line number>  OPTIONAL.  Default=5.  This is line cursor is on initially. May be modified by events
  ;"          ---- events ----
  ;"          OPTION("ON SELECT")="FnName^Module" -- code to call based on user input.  E.g. DO FnName^Module(TMGPSCRLARR,.OPTION,.INFO)
  ;"                  Event is fired when user presses ENTER (RETURN) key, provided a command has NOT been entered by user. 
  ;"                  INFO("CURRENT LINE","NUMBER")=number currently highlighted line
  ;"                  INFO("CURRENT LINE","TEXT")=Text of currently highlighted line
  ;"                  INFO("CURRENT LINE","RETURN")=return value of currently highlighted line
  ;"          OPTION("ON CHANGING")="FnName^Module" -- code to execute for number entry  E.g. DO FnName^Module(TMGPSCRLARR,.OPTION,.INFO)
  ;"                  Event is fired when cursor is about to change UP or DN.
  ;"                  INFO("CURRENT LINE","NUMBER")=number currently highlighted line
  ;"                  INFO("CURRENT LINE","TEXT")=Text of currently highlighted line
  ;"                  INFO("CURRENT LINE","RETURN")=return value of currently highlighted line
  ;"                  INFO("NEXT LINE","NUMBER")=next line number. Used for ON CHANGING to show the line about to be selected
  ;"                  INFO("NEXT LINE","TEXT")=Text of new line
  ;"                  INFO("ALLOW CHANGE")=1, <--- RETURN RESULT.  Change to 0 to disallow move.
  ;"          OPTION("ON CMD")="FnName^Module" -- code to execute for number entry      E.g. DO FnName^Module(TMGPSCRLARR,.OPTION,.INFO)
  ;"                  Event is fired when user presses ENTER (RETURN) key, and a command has been entered by user. 
  ;"                  INFO("USER INPUT")=UserTypedInput
  ;"          OPTION("ON KEYPRESS")="FnName^Module" -- code to execute for user key press      E.g. DO FnName^Module(TMGPSCRLARR,.OPTION,.INFO)
  ;"                  Event is fired is keypress detected, not otherwise causing other events to fire. 
  ;"                  INFO("USER INPUT")=Key pressed
  ;"                  INFO("CMD")=User full input command so far (being built up)
  ;"          OPTION("ON CURSOR")="FnName^Module" -- code to execute for user cursor key press      E.g. DO FnName^Module(TMGPSCRLARR,.OPTION,.INFO)
  ;"                  Event is fired when a cursor key is pressed, before the scroller moves the highlighted line.  
  ;"                  INFO("CURSOR")=Cursor Key pressed
  ;"                  INFO("CURSOR","HANDLED")=1 <-- this is what called code should set if it handled
  ;"                                  the cursor event. This will prevent scroller from acting on it.   
  ;"          NOTES about events.  Functions will be called as follows:
  ;"              DO FnName^Module(TMGPSCRLARR,.OPTION,.INFO)
  ;"                TMGPSCRLARR and OPTION are the same data received by this function
  ;"                  -- thus OPTION can be used to can other custom information.
  ;"                INFO has extra info as outlined above.
  ;"              Functions may set a globally-scoped var named TMGSCLRMSG to communicate back
  ;"                      TMGSCLRMSG="^" --> Scroller will exit
  ;"                      TMGSCLRMSG="FULL" --> Scroller will do full screen redraw
  ;"              Functions may set OPTION("HIGHLINE")=# or OPTION("COLUMNS",<COL#>,"HIGHLINE")=# to tell 
  ;"                      the scroller to set the highlight line to #.  This will not trigger further events
  ;"Result: none
  ;
  NEW SCRNLINE,SPACELINE ;"To hold string for "   " and "---" lines, matching width of display area. 
  NEW WIDTH        ;"Array, for widths of each column: WIDTH(<col#>)=#
  NEW MAINTOP      ;"Screen Y position of first line of main area
  NEW TOPLINE      ;"Array, for starting data index of first line in main area. TOPLINE(<col#>)=<starting data index>
  NEW HIGHLINE     ;"Array, for data index of highlighted line in main area. HIGHLINE(<col#>)=<data index>
  NEW XOFFSET      ;"Array, for holding X offset, which allows left-right scrolling.  
  NEW INFO         ;"Array, for holding information to pass to event handlers     
  NEW ENTRYCT      ;"Array, for holding number of data elements for column. ENTRYCT(<col#>)=<number of data elements>
  NEW NUMIDXDIGITS ;"Array, for max number of digits if showing index numbers, per column.  NUMIDXDIGITS(<col#>)=#
  NEW MAINLINES    ;"Number of screen lines in main area.
  NEW COLS         ;"Number of columns to display
  NEW ACTIVECOL    ;"Which column is active, meaning hold user cursor
  NEW BUILDCMD SET BUILDCMD="" ;"Var to hold command as user builds it up through typing letters
  NEW TMGSCLRMSG SET TMGSCLRMSG=""  ;"Special variable available to event handlers to send back message by setting value
  NEW LASTSCRNW SET LASTSCRNW=-1 ;"Var to track if screen size has changed. 
  NEW FNCYCLE      ;"1 to enable and show instructions for FN to cycle active column  

  DO SETDEFCOLORS(.OPTION)
  NEW SCRNH DO CHECKSCRNHEIGHT(.SCRNH,.OPTION) 
  NEW SCRNW DO CHECKSCRNWIDTH(.SCRNW,.OPTION) 
  NEW DONE SET DONE=0
  NEW DRAWMODE SET DRAWMODE="FULL"
  NEW MAXCOLS SET MAXCOLS=6  ;"could increase later if need >  6 possible columns 
  ;
  FOR  DO  QUIT:DONE  ;"MAIN LOOP
  . IF "FULL"[DRAWMODE DO INITSCREEN() 
  . IF "FULL,NORMAL"[DRAWMODE DO
  . . DO CHECKSCRNWIDTH(.SCRNW,.OPTION)              ;"Check each cycle, so window can be resized between full redraws 
  . . DO SETCOLWIDTHS(.WIDTH,.COLS,SCRNW,.OPTION)    ;"Set up column widths.    
  . . DO ENSURHLONSCRN(.HIGHLINE,.TOPLINE,ACTIVECOL) ;"Ensure the highline is on screen 
  . . ;  
  . . DO DRAWHEADER(.OPTION,.SCRNW,.MAINTOP) ;"DRAW HEADER AREA
  . . DO DRAWMAIN(.OPTION,.WIDTH,.MAINTOP)   ;"DRAW MAIN AREA
  . . DO DRAWFOOTER(.OPTION)                 ;"DRAW FOOTER AREA
  . . ;
  . . DO PREPINFO(.INFO,TMGPSCRLARR,"CURRENT LINE",HIGHLINE(1))  
  . . DO DRAWCMDAREA()  
  . . ;
  . SET TMGSCLRMSG=""  ;"Initialize message variable that can be used by event handlers to send back message. 
  . NEW TEMP SET TEMP=$$USER(.OPTION)  ;"interact with users, trigger events etc. 
  . IF TMGSCLRMSG="^" SET DONE=1 QUIT
  . IF TMGSCLRMSG="FULL" SET DRAWMODE="FULL" QUIT
  . IF TEMP="^" SET DONE=1 QUIT
  . IF TEMP=2 SET DRAWMODE="FULL" QUIT
  . IF TEMP=1 SET DRAWMODE="NORMAL" QUIT
  . SET DRAWMODE="NONE" 
  . QUIT
  QUIT
  ; 
  ;"=======================================================================
  ;"=======================================================================
  ;
USER(OPTION);"Read user input and interact
  ;"NOTE: Uses vars in global scope, defined in SCROLLER scope: 
  ;"Result: 1 for draw, 2 for full draw, ^ for done.  
  ;
  NEW NEEDREFRESH SET NEEDREFRESH=0
  NEW TMGRESULT SET TMGRESULT=1
  NEW ESCKEY SET ESCKEY=""
  SET INPUT=$$READKY^TMGUSRI5("re",,1,,.ESCKEY)
  IF (INPUT="")&(ESCKEY="") SET ESCKEY="CR"
  NEW DONE SET DONE=0
  NEW HANDLED SET HANDLED=0
  IF $EXTRACT(INPUT,1)="^",$LENGTH(INPUT)>1 SET ESCKEY="CTRL-"_$EXTRACT(INPUT,2,$LENGTH(INPUT)),INPUT=""
  IF $GET(OPTION("ON CURSOR"))'="",INPUT="",$$ISCURSOR(ESCKEY) DO  GOTO:DONE USI2
  . SET DONE=$$EVENTCURSOR(ESCKEY)  ;"trigger on cursor event
  . IF DONE SET NEEDREFRESH=2
  IF FNCYCLE'="",ESCKEY=FNCYCLE DO  GOTO USI2
  . DO HNDLCOLCYCLE
  . SET RESULT=2
  IF ESCKEY="UP"   SET INPUT="CURSOR^UP^1"
  IF ESCKEY="PREV" SET INPUT="CURSOR^UP^15"
  IF ESCKEY="DOWN" SET INPUT="CURSOR^DOWN^1"
  IF ESCKEY="NEXT" SET INPUT="CURSOR^DOWN^15"
  IF ESCKEY="LEFT" SET INPUT="CURSOR^LEFT^1"
  IF ESCKEY="RIGHT" SET INPUT="CURSOR^RIGHT^1"
  IF ESCKEY="FIND" SET INPUT="CURSOR^LEFT^*"   ;"For some reason, HOME key generates "FIND"
  IF ESCKEY="SELECT" SET INPUT="CURSOR^RIGHT^*"   ;"For some reason, END key generates "SELECT"
  IF ESCKEY="BACKSPC",$LENGTH(BUILDCMD)>0 DO
  . SET BUILDCMD=$EXTRACT(BUILDCMD,1,$LENGTH(BUILDCMD)-1)
  IF ESCKEY="CR" DO  GOTO USI2
  . DO HNDLCR
  IF (ESCKEY="INSERT")!(INPUT="+")!((INPUT=" ")&(BUILDCMD="")),$GET(OPTION("MULTISEL"))=1 DO  GOTO USI2  ;"toggle selection.
  . DO HNDLSEL ;"Handle user SELECTION input
  IF (ESCKEY="CTRL-A"),$GET(OPTION("MULTISEL"))=1 DO  GOTO USI2  ;"selection all TOGGLE.
  . DO HNDLSELALL ;"Handle user Select ALL
  IF INPUT="^" SET RESULT="^" GOTO USI2
  IF (INPUT["CURSOR^") DO  GOTO USI2:HANDLED
  . SET HANDLED=$$HNDLCURSOR(INPUT,.HIGHLINE,.TOPLINE,.XOFFSET,ACTIVECOL,.OPTION)
  . IF HANDLED=0 SET INPUT="" QUIT
  . SET RESULT=1
  IF INPUT="=" DO  GOTO USI2
  . DO HNDLSETSIZE ;"Handle user setting screen size      
  ;"Default below
  SET NEEDREFRESH=1
  IF (INPUT="")&(ESCKEY'="") SET INPUT="{"_ESCKEY_"}"
  ELSE  SET BUILDCMD=BUILDCMD_INPUT
  ;
  SET INFO("USER INPUT")=INPUT
  SET INFO("CMD")=BUILDCMD
  DO EVENT(.OPTION,"ON KEYPRESS")
  SET NEEDREFRESH=2
  ;
  ;"After above events, see if event handler code requested scroller to select a particular line. 
  IF $$SETHIGHLINES(.HIGHLINE,.TOPLINE,.OPTION)>0 SET NEEDREFRESH=2 
  ;
USI2  ;        
  IF NEEDREFRESH=2 SET RESULT=2
  IF NEEDREFRESH=1 SET RESULT=1
  QUIT RESULT
  ;   
EVENTCURSOR(ESCKEY)  ;"trigger on cursor event
  ;"Note: uses var in global scope, defined in SCROLLER scope
  ;"Result: 1 if event handled cursor, 0 otherwise.  
  SET INFO("CURSOR")=ESCKEY
  SET INFO("CURSOR","HANDLED")=0
  DO EVENT(.OPTION,"ON CURSOR")
  NEW RESULT SET RESULT=(+$GET(INFO("CURSOR","HANDLED"))=1)
  QUIT RESULT
  ;
HNDLCURSOR(INPUT,HIGHLINE,TOPLINE,XOFFSET,COL,OPTION) ;"Handle user cursor input.  
  ;"Note: uses vars in global scope, defined in SCROLLER scope:
  ;"       WIDTH
  ;"Result: 1 if cursor was handled here, or 0 if not. 
  NEW RESULT SET RESULT=0
  NEW CSRDIR SET CSRDIR=$PIECE(INPUT,"^",2)
  NEW COUNT SET COUNT=$PIECE(INPUT,"^",3) 
  NEW DATAREF SET DATAREF=$$DATAREF(COL)
  IF ("UP,DOWN,LEFT,RIGHT,"[CSRDIR)=0 GOTO HNDCSRDN
  IF COUNT="*","LEFT"[CSRDIR SET COUNT=9999999
  IF COUNT="*","RIGHT"[CSRDIR DO
  . NEW TEMPXO SET TEMPXO(COL)=0
  . NEW CURTEXT SET CURTEXT=$$GETDATATEXT(COL,HIGHLINE(COL),.HIGHLINE,.TEMPXO)
  . NEW LEN SET LEN=$LENGTH(CURTEXT)  ;"e.g. 875 chars of text
  . NEW COLW SET COLW=WIDTH(COL)      ;"e.g. 100 chars wide for column
  . NEW TARGETXO SET TARGETXO=LEN-COLW ;"e.g. char position 775 should be XOffset
  . SET TARGETXO=TARGETXO+1 ;"offset 1 to right, to give a blank space at end of line. 
  . IF TARGETXO<0 SET TARGETXO=0
  . NEW CURXO SET CURXO=XOFFSET(COL)
  . SET COUNT=TARGETXO-CURXO
  IF COUNT="" SET COUNT=0
  SET INFO("ALLOW CHANGE")=1
  SET NEEDREFRESH=1
  NEW DELTA SET DELTA=0
  IF CSRDIR="DOWN"  SET DELTA=COUNT
  IF CSRDIR="UP"    SET DELTA=-COUNT
  IF CSRDIR="LEFT"  SET DELTA=-COUNT
  IF CSRDIR="RIGHT" SET DELTA=COUNT
  IF DELTA=0 GOTO HNDCSRDN
  IF "UP,DOWN"[CSRDIR DO
  . NEW TEMPHL MERGE TEMPHL=HIGHLINE
  . NEW TEMPTL MERGE TEMPTL=TOPLINE
  . SET TEMPHL(COL)=HIGHLINE(COL)+DELTA  ;"HL will be planned next highlight line
  . DO ENSURHLONSCRN(.TEMPHL,.TEMPTL,COL)
  . DO PREPINFO(.INFO,TMGPSCRLARR,"NEXT LINE",TEMPHL(COL))  
  . DO EVENT(.OPTION,"ALLOW CHANGE") IF $GET(INFO("ALLOW CHANGE"))'>0 QUIT
  . KILL INFO("NEXT LINE")
  . KILL HIGHLINE MERGE HIGHLINE=TEMPHL
  . KILL TOPLINE MERGE TOPLINE=TEMPTL
  . SET RESULT=1
  IF "LEFT,RIGHT"[CSRDIR DO
  . DO PREPINFO(.INFO,TMGPSCRLARR,"NEXT LINE",HIGHLINE(COL))  
  . DO EVENT(.OPTION,"ALLOW CHANGE") IF $GET(INFO("ALLOW CHANGE"))'>0 QUIT
  . IF $GET(OPTION("LR SCROLLING"))'=1 QUIT
  . SET XOFFSET(COL)=$$INBOUNDS(XOFFSET(COL)+DELTA,0,500)  ;"May need to find better way than hardcoded 500 later....
  . SET RESULT=1
HNDCSRDN ;  
  QUIT RESULT
  ;
HNDLCOLCYCLE ;"Hand user input of function key to cycle column
  SET ACTIVECOL=ACTIVECOL+1
  IF ACTIVECOL>COLS SET ACTIVECOL=1 
  SET OPTION("COLUMNS","ACTIVE")=ACTIVECOL
  QUIT
  ;
HNDLCR ;"Handle CR user input     
  ;"Note: uses var in global scope, defined in SCROLLER scope
  NEW EVENT
  IF BUILDCMD'="" SET EVENT="CMD",INFO("USER INPUT")=BUILDCMD
  ELSE  IF INPUT'="" SET EVENT="CMD",INFO("USER INPUT")=INPUT
  ELSE  SET EVENT="SELECT"
  NEW CODEFN SET CODEFN=$$GETCODEFN("ON "_EVENT,.OPTION) QUIT:(CODEFN="")
  NEW $ETRAP DO SETETRAP
  XECUTE CODEFN
  SET NEEDREFRESH=2
  SET BUILDCMD=""
  KILL INFO("USER INPUT")
  QUIT
  ;     
HNDLSEL ;"Handle user SELECTION input
  ;"Note: uses var in global scope, defined in SCROLLER scope
  SET @TMGPSCRLARR@("SELECTED",HIGHLINE(1))='$GET(@TMGPSCRLARR@("SELECTED",HIGHLINE(1)))
  SET NEEDREFRESH=1
  QUIT
  ;
HNDLSELALL ;"Handle user Select ALL
  ;"Note: uses var in global scope, defined in SCROLLER scope
  NEW ALLSELECTED SET ALLSELECTED=-1 
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(@TMGPSCRLARR@(IDX)) QUIT:(IDX'>0)!(ALLSELECTED=0)  DO
  . SET ALLSELECTED=(+$GET(@TMGPSCRLARR@("SELECTED",IDX))=1)
  NEW NEWSELECTEDVAL SET NEWSELECTEDVAL=$SELECT(ALLSELECTED=1:0,1:1)
  SET IDX=0 FOR  SET IDX=$ORDER(@TMGPSCRLARR@(IDX)) QUIT:(IDX'>0)  DO
  . SET @TMGPSCRLARR@("SELECTED",IDX)=NEWSELECTEDVAL
  SET NEEDREFRESH=1
  QUIT
  ;
HNDLSETSIZE ;"Handle user setting screen size
  ;"Note: uses var in global scope, defined in SCROLLER scope
  SET NEEDREFRESH=2
  NEW DIR SET DIR(0)="N^10:"_IOM
  SET DIR("B")=SCRNW
  WRITE "Enter Screen Width (# of columns): " DO ^DIR WRITE !
  IF $DATA(DIRUT) WRITE # QUIT
  SET SCRNW=Y
  SET DIR(0)="N^5:"_(IOSL-2)
  SET DIR("B")=SCRNH
  WRITE "Enter Screen Height (# of rows): " DO ^DIR WRITE !
  IF $DATA(DIRUT) WRITE # QUIT
  SET SCRNH=Y
  WRITE #
  QUIT
  ;
DRAWHEADER(OPTION,SCRNW,MAINTOP) ;
  DO HOME^TMGTERM SET MAINTOP=1  
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
  . . DO WCLTEXT(TEXT,SCRNW,.OPTION) WRITE ! SET MAINTOP=MAINTOP+1
  QUIT
  ;
DRAWMAIN(OPTION,WIDTH,MAINTOP) ;
  ;"NOTE: Uses vars in global scope, defined in SCROLLER scope: 
  ;"          SCRNLINE,COLS,HIGHLINE,TOPLINE,NUMIDXDIGITS,MAINLINES
  NEW SHOWIDX
  DO SETCOLOR("TOP LINE",.OPTION)
  WRITE SCRNLINE,! SET MAINTOP=MAINTOP+1
  NEW COLNUM FOR COLNUM=1:1:COLS DO
  . NEW XPOS SET XPOS=$$COLLEFT(.WIDTH,COLNUM)
  . NEW DATAREF SET DATAREF=$$DATAREF(COLNUM)
  . SET SHOWIDX=+$GET(@DATAREF@("SHOW INDEX"))
  . NEW COLORS DO SETCOLORS(.COLORS,COLNUM,.OPTION)
  . DO SETCOLOR("NORM",.COLORS)
  . NEW LINECT FOR LINECT=1:1:MAINLINES DO 
  . . NEW IDX SET IDX=TOPLINE(COLNUM)+LINECT-1  ;"DATA INDEX
  . . DO CUP^TMGTERM(XPOS,MAINTOP+LINECT-1)
  . . NEW TEXT SET TEXT=$$GETDATATEXT(COLNUM,IDX,.HIGHLINE,.XOFFSET)
  . . NEW SELECTED SET SELECTED=$SELECT(COLNUM=1:+$GET(@TMGPSCRLARR@("SELECTED",IDX)),1:0)
  . . NEW ONHIGHLINE SET ONHIGHLINE=(IDX=HIGHLINE(COLNUM))&(COLNUM=ACTIVECOL)
  . . NEW TEXTCOLOR IF SELECTED SET TEXTCOLOR=$SELECT(ONHIGHLINE:"HI-SEL",1:"SELECTED")
  . . ELSE  SET TEXTCOLOR=$SELECT(ONHIGHLINE:"HIGH",1:"NORM")
  . . NEW INDEXSTR SET INDEXSTR=""
  . . IF SHOWIDX SET INDEXSTR="{{INDEX}}"_$$RJ^XLFSTR(IDX,NUMIDXDIGITS(COLNUM))_"."
  . . SET TEXT=INDEXSTR_"{{"_TEXTCOLOR_"}}"_TEXT
  . . NEW TEXTLEN SET TEXTLEN=$$NOCOLEN(TEXT)
  . . NEW TAILPAD SET TAILPAD=$EXTRACT(SPACELINE,1,(WIDTH(COLNUM)-TEXTLEN))
  . . SET TEXT=TEXT_TAILPAD
  . . DO WCLTEXT(TEXT,$$COLRIGHT(.WIDTH,COLNUM),.COLORS)
  . . DO SETCOLOR("RESET",.COLORS) WRITE !
  QUIT
  ;
DRAWFOOTER(OPTION) ;
  ;"NOTE: Uses vars in global scope, defined in SCROLLER scope:
  ;"       SCRNLINE,SCRNW,FNCYCLE,COLS
  SET FNCYCLE=$GET(OPTION("COLUMNS","FN TOGGLE NUM"))
  IF (FNCYCLE'="")&(COLS>1) DO
  . SET OPTION("FOOTER",0,0)=FNCYCLE_" CYCLE ACTIVE COLUMN"
  DO SETCOLOR("BOTTOM LINE",.OPTION)
  WRITE SCRNLINE,!
  DO SETCOLOR("FOOTER",.OPTION)
  NEW IDX SET IDX=""
  FOR  SET IDX=$ORDER(OPTION("FOOTER",IDX)) QUIT:(IDX="")  DO
  . NEW JDX SET JDX=$ORDER(OPTION("FOOTER",IDX,""))
  . IF JDX'="" DO   ;"Handle data option:  OPTION("FOOTER",#,#)=<sub-parts>
  . . NEW ONELINE SET ONELINE="",JDX=""
  . . FOR  SET JDX=$ORDER(OPTION("FOOTER",IDX,JDX)) QUIT:(JDX="")  DO
  . . . NEW ONEPART SET ONEPART=$GET(OPTION("FOOTER",IDX,JDX))_" | "
  . . . IF $LENGTH(ONELINE_ONEPART)>SCRNW DO
  . . . . WRITE ONELINE
  . . . . NEW REMAINING SET REMAINING=SCRNW-$LENGTH(ONELINE)
  . . . . WRITE $EXTRACT(SPACELINE,1,REMAINING),!
  . . . . SET ONELINE=""
  . . . SET ONELINE=ONELINE_ONEPART
  . . WRITE $$LJ^XLFSTR(ONELINE,SCRNW),!
  . ELSE  DO   ;"Handle data option:  OPTION("FOOTER",#)=line
  . . NEW STR SET STR=$GET(OPTION("FOOTER",IDX))
  . . FOR  DO  QUIT:STR=""
  . . . NEW STRA
  . . . IF $LENGTH(STR)'>SCRNW DO
  . . . . SET STRA=$$LJ^XLFSTR(STR,SCRNW)
  . . . . SET STR=""
  . . . ELSE  DO
  . . . . SET STRA=$EXTRACT(STR,1,SCRNW)
  . . . . SET STR=$EXTRACT(STR,SCRNW+1,$LENGTH(STR))
  . . . WRITE STRA,!
  QUIT
  ;
DRAWCMDAREA()  ;
  DO SETCOLOR("RESET")
  WRITE $$LJ^XLFSTR(": ",SCRNW),!
  DO CUU^TMGTERM(1) WRITE ": "_BUILDCMD
  QUIT
  ;
GETDATATEXT(COLNUM,IDX,HIGHLINE,XOFFSET)
  NEW TEXT
  IF COLNUM=1 DO
  . SET TEXT=$ORDER(@TMGPSCRLARR@(IDX,""))
  ELSE  DO
  . NEW LEFTSEL SET LEFTSEL=HIGHLINE(COLNUM-1)
  . SET TEXT=$GET(@TMGPSCRLARR@("COL",COLNUM,LEFTSEL,IDX))
  IF TEXT[$CHAR(9) SET TEXT=$$REPLSTR^TMGSTUT3(TEXT,$CHAR(9),"  ")
  IF $GET(XOFFSET(COLNUM))>0 DO
  . SET TEXT=$$MIDSTRCOLOR^TMGSTUT3(TEXT,XOFFSET(COLNUM)+1,$LENGTH(TEXT))
  QUIT TEXT
  ;
INITSCREEN() ;
  ;"NOTE: Uses vars in global scope, defined in SCROLLER scope:
  ;"        MAINLINES,COLS,ACTIVECOL,HIGHLINE,TOPLINE,ENTRYCT,NUMIDXDIGITS,
  ;"        F1CYCLE,OPTION
  NEW SIZEHDR SET SIZEHDR=$$LISTCT^TMGMISC2($NAME(OPTION("HEADER")))+1
  NEW SIZEFTR SET SIZEFTR=$$LISTCT^TMGMISC2($NAME(OPTION("FOOTER")))+1
  SET MAINLINES=SCRNH-SIZEHDR-SIZEFTR   ;"This is number of lines do draw for Main part.  
  SET COLS=+$GET(OPTION("COLUMNS","NUM"),1) SET COLS=$SELECT(COLS<1:1,COLS>MAXCOLS:MAXCOLS,1:COLS)  
  SET ACTIVECOL=$GET(OPTION("COLUMNS","ACTIVE"),1)
  SET F1CYCLE=($GET(OPTION("COLUMNS","F1 TOGGLE NUM"),1)&(COLS>1))
  NEW LSTCTOPT SET LSTCTOPT("NUMERIC")=1
  NEW COLNUM FOR COLNUM=1:1:COLS DO
  . SET HIGHLINE(COLNUM)=+$GET(HIGHLINE(COLNUM),5)  ;"ensure defined
  . SET TOPLINE(COLNUM)=+$GET(TOPLINE(COLNUM),1)    ;"ensure defined
  . SET XOFFSET(COLNUM)=+$GET(XOFFSET(COLNUM))      ;"ensure defined
  . SET ENTRYCT(COLNUM)=$$LISTCT^TMGMISC2($$DATAREF(COLNUM),.LSTCTOPT)
  . SET NUMIDXDIGITS(COLNUM)=$LENGTH(ENTRYCT(COLNUM)) 
  . IF NUMIDXDIGITS(COLNUM)=0 SET NUMIDXDIGITS(COLNUM)=1 
  DO SETHIGHLINES(.HIGHLINE,.TOPLINE,.OPTION)
  QUIT
  ;
DATAREF(COLNUM) ;"return reference to data, given COLNUM
  ;"NOTE: uses TMGPSCRLARR in global scope
  NEW REF SET REF=$SELECT(COLNUM=1:TMGPSCRLARR,1:$NAME(@TMGPSCRLARR@("COL",COLNUM)))
  QUIT REF
  ;
COLLEFT(WIDTH,COLNUM) ;"Return left X position for given COLNUM
  NEW RESULT SET RESULT=1
  NEW IDX FOR IDX=1:1:(COLNUM-1) SET RESULT=RESULT+(WIDTH(IDX))
  QUIT RESULT
  ;
COLRIGHT(WIDTH,COLNUM) ;"Return right X position for given COLNUM
  NEW RESULT SET RESULT=0
  NEW IDX FOR IDX=1:1:COLNUM SET RESULT=RESULT+(WIDTH(IDX))
  QUIT RESULT
  ;
CHECKSCRNWIDTH(SCRNW,OPTION)  ;
  ;"note: Uses in global scope: LASTSCRNW, SCRNLINE, SPACELINE
  SET SCRNW=+$GET(OPTION("SCRN WIDTH"))
  IF (SCRNW'>0) IF $$GETSCRSZ^TMGKERNL(,.SCRNW) SET SCRNW=+SCRNW-1
  IF SCRNW'>0 SET SCRNW=$GET(IOM,66)-2
  ;"//kt --> removed to force query of window each time --> SET OPTION("SCRN WIDTH")=SCRNW
  IF (SCRNW'=LASTSCRNW) DO    ;"if screen is resized, then clear screen, etc
  . IF LASTSCRNW'=-1 WRITE #  ;"Clear screen if not first run  
  . SET SCRNLINE="" SET $PIECE(SCRNLINE,"-",SCRNW)="-"
  . SET SPACELINE="" SET $PIECE(SPACELINE," ",SCRNW)=" "
  SET LASTSCRNW=SCRNW  
  QUIT
  ;
CHECKSCRNHEIGHT(SCRNH,OPTION) ;
  ;"note: I don't want height to autoresize, i.e. I often don't want full height
  SET SCRNH=+$GET(OPTION("SCRN HEIGHT"))   
  IF SCRNH'>0 SET SCRNH=$GET(IOSL,25)-2 
  IF SCRNH'>0 SET SCRNH=15 SET OPTION("SCRN HEIGHT")=SCRNH
  QUIT
SETCOLWIDTHS(WIDTH,COLS,SCRNW,OPTION) ;"Set up column widths.
  ;"INPUT: WIDTH -- PASS BY REFERENCE, AN OUT PARAMETER
  ;"       COLS -- PASS BY REFERENCE, AN OUT PARAMETER
  ;"       SCRNW -- current screen width
  ;"       OPTION -- PASS BY REFERENCS
  KILL WIDTH
  NEW COLNUM FOR COLNUM=1:1:COLS DO
  . NEW NUM SET NUM=$GET(OPTION("COLUMNS",COLNUM,"WIDTH")) IF NUM<1 SET NUM=0
  . IF NUM["%" DO 
  . . SET NUM=(SCRNW*+NUM/100)\1
  . SET WIDTH(COLNUM)=+NUM
  . SET WIDTH=$GET(WIDTH)+NUM
  FOR  QUIT:(WIDTH'>SCRNW)  DO  ;"If too wide, shrink each column until fits. 
  . FOR COLNUM=1:1:COLS IF WIDTH(COLNUM)>1 SET WIDTH(COLNUM)=WIDTH(COLNUM)-1,WIDTH=WIDTH-1
  NEW DONE SET DONE=0
  FOR  DO  QUIT:DONE
  . NEW ZEROS SET ZEROS=0 FOR COLNUM=1:1:COLS IF WIDTH(COLNUM)<1 SET ZEROS=ZEROS+1,ZEROS(COLNUM)=""
  . IF ZEROS=0 SET DONE=1 QUIT
  . NEW LARGEST SET LARGEST=0 FOR COLNUM=1:1:COLS IF WIDTH(COLNUM)'<LARGEST SET LARGEST=WIDTH(COLNUM),LARGEST(0)=COLNUM
  . IF WIDTH<SCRNW DO  QUIT  ;"If too narrow, then divide available space between columns.  
  . . NEW TEMP SET TEMP=(SCRNW-WIDTH)\ZEROS
  . . FOR COLNUM=1:1:COLS IF WIDTH(COLNUM)=0 SET WIDTH(COLNUM)=TEMP,WIDTH=WIDTH+TEMP
  . . IF WIDTH<SCRNW SET WIDTH(1)=WIDTH(1)+(SCRNW-WIDTH)
  . ELSE  DO  ;"WIDTH=SCRNWIDTH, but still have a zero column.
  . . NEW IDX SET IDX=LARGEST(0)
  . . NEW W SET W=WIDTH(IDX)
  . . NEW HALF SET HALF=W\2
  . . NEW ZCOL SET ZCOL=$ORDER(ZEROS(""))
  . . SET WIDTH(IDX)=WIDTH(IDX)-HALF
  . . SET WIDTH(ZCOL)=WIDTH(ZCOL)+HALF
  QUIT
  ;  
ENSURHLONSCRN(HIGHLINE,TOPLINE,COL) ;"Ensure the highline (selected line) is on screen
  ;"Input HIGHLINE -- Pass by reference
  ;"      TOPLINE -- PASS BY REFERENCE.  Array of TOP LINES
  ;"      COL -- the current active column
  ;"NOTE: Uses MAINLINES,TMGPSCRLARR in global scope  
  NEW REF SET REF=$$DATAREF(COL)
  NEW MAXIDX SET MAXIDX=+$ORDER(@REF@("@@@@@@@@"),-1)
  SET HIGHLINE(COL)=$SELECT(HIGHLINE(COL)<1:1,HIGHLINE(COL)>MAXIDX:MAXIDX,1:HIGHLINE(COL))
  NEW COLNUM FOR COLNUM=1:1:COLS DO   ;"Ensure all in bounds
  . SET TOPLINE(COLNUM)=$$INBOUNDS(TOPLINE(COLNUM),1,ENTRYCT(COLNUM))
  . SET HIGHLINE(COLNUM)=$$INBOUNDS(HIGHLINE(COLNUM),1,ENTRYCT(COLNUM))
  . IF HIGHLINE(COLNUM)<TOPLINE(COLNUM) SET TOPLINE(COLNUM)=HIGHLINE(COLNUM)
  . ELSE  IF HIGHLINE(COLNUM)>(TOPLINE(COLNUM)+(MAINLINES-1)) DO
  . . SET TOPLINE(COLNUM)=HIGHLINE(COLNUM)-(MAINLINES-1) 
  QUIT
  ;
INBOUNDS(NUM,LOW,HI) ;"Return number, within bounds
  NEW RESULT SET RESULT=$SELECT(NUM<LOW:LOW,NUM>HI:HI,1:NUM)
  QUIT RESULT
  ;
SETHIGHLINES(HIGHLINE,TOPLINE,OPTION) ;"Transfer highlight line info from OPTION to HIGHLINE
  ;"Note: Uses vars in global scope, defined in SCROLLER scope. 
  NEW RESULT SET RESULT=0
  NEW CHANGED SET CHANGED=0
  IF $GET(OPTION("HIGHLINE"))>0 DO   ;"//kt 8/4/22
  . SET HIGHLINE(1)=OPTION("HIGHLINE")
  . KILL OPTION("HIGHLINE")
  NEW COLS SET COLS=+$GET(OPTION("COLUMNS","NUM"),1) SET COLS=$SELECT(COLS<1:1,COLS>MAXCOLS:MAXCOLS,1:COLS)  
  NEW COLNUM FOR COLNUM=1:1:COLS DO
  . NEW HL SET HL=$GET(OPTION("COLUMNS",COLNUM,"HIGHLINE"))
  . IF HL>0 DO
  . . SET HIGHLINE(COLNUM)=HL  
  . . KILL OPTION("COLUMNS",COLNUM,"HIGHLINE")
  . . SET CHANGED=1    
  IF CHANGED DO
  . DO ENSURHLONSCRN(.HIGHLINE,.TOPLINE,ACTIVECOL) ;"Ensure the highline is on screen  
  . SET NEEDREFRESH=2
  QUIT RESULT 
  ;
PREPINFO(INFO,REF,NODE,IDX) ;
  SET INFO(NODE,"NUMBER")=IDX
  SET INFO(NODE,"TEXT")=$ORDER(@REF@(IDX,""))
  SET INFO(NODE,"RETURN")=$GET(@REF@(IDX,INFO(NODE,"TEXT")))
  QUIT
  ;
ISCURSOR(ESCKEY) ;
  ;"FYI: FIND=home key and SELECT=end key
  QUIT "UP^DOWN^LEFT^RIGHT^PREV^NEXT^FIND^SELECT^"[ESCKEY_"^"
  ;  
WCLTEXT(TEXT,MAXX,OPTION) ;"WRITE (OPTIONALLY) COLORED TEXT
  NEW TEXTA,TEXTB,TEXTCOLOR,WIDTH
  FOR  QUIT:(TEXT'["{{")!($X'<MAXX)  DO
  . SET TEXTCOLOR=$$PARSCOLR(.TEXT,.TEXTA)  ;" Text --> TextA,Text and {{COLOR}} to TEXTCOLOR
  . DO TRIMWRITE(TEXTA,MAXX)
  . IF $$COLORDEFINED(TEXTCOLOR,.OPTION) DO
  . . DO SETCOLOR(TEXTCOLOR,.OPTION)
  . ELSE  WRITE "{{"_TEXTCOLOR_"}}"
  DO TRIMWRITE(TEXT,MAXX)
  QUIT
  ;
TRIMWRITE(TEXT,MAXX)  ;"trimmed write to screen
  IF $X<1 SET $X=1  ;"$X is where next char will be written.  Shouldn't be 0, but sometimes is. 
  NEW CURSORX SET CURSORX=$X   
  NEW WIDTH SET WIDTH=CURSORX+$LENGTH(TEXT)-1
  IF WIDTH>MAXX DO
  . NEW TRIMLEN SET TRIMLEN=((MAXX-CURSORX+1)-3)
  . NEW TEMPSTR SET TEMPSTR=$EXTRACT(TEXT,1,TRIMLEN)
  . IF TEMPSTR'="" WRITE TEMPSTR_"..."
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
  IF (SP>0)&(SP<=$LENGTH(TEXT)) GOTO NCOL1
NCOLDN  ;
  QUIT RESULT
  ;
GETCODEFN(LABEL,OPTION) ;"Setup CODEFN for event, if available
  NEW CODEFN SET CODEFN=$GET(OPTION(LABEL))
  IF CODEFN'="" SET CODEFN="DO "_CODEFN_"(TMGPSCRLARR,.OPTION,.INFO)"
  QUIT CODEFN
  ;
SETETRAP ;
  SET $ETRAP="WRITE ""(Invalid M Code!.  Error Trapped.)"",! SET $ETRAP="""",$ecode="""""
  QUIT
  ;
EVENT(OPTION,EVENT) ;"Trigger event, if defined.
  ;"NOTE: The event code uses INFO, which is in global scope.
  NEW CODEFN SET CODEFN=$$GETCODEFN(EVENT,.OPTION) QUIT:(CODEFN="")
  NEW $ETRAP DO SETETRAP
  XECUTE CODEFN
  QUIT
  ;
COLORDEFINED(LABEL,OPTION) ;"Return if color has been defined.
  QUIT ($DATA(OPTION("COLORS",LABEL))>0)
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
  NEW SAVEIF SET SAVEIF=$T
  IF LABEL="RESET" DO VTATRIB^TMGTERM(0) QUIT  ;"reset colors
  IF $DATA(OPTION("COLORS",LABEL))=0 DO SETDEFCOLORS(.OPTION)  ;
  NEW FG SET FG=$GET(OPTION("COLORS",LABEL,"FG"),1) ;"default to black
  NEW BG SET BG=$GET(OPTION("COLORS",LABEL,"BG"),0) ;"default to white
  IF BG="@" SET BG=$GET(OPTION("COLORS","NORM","BG"),0) ;"default to white
  DO VCOLORS^TMGTERM(FG,BG)
  IF SAVEIF  ;"restore $T
  QUIT
  ;
SETDEFCOLORS(OPTION)  ;
  IF $GET(OPTION("COLORS","NORM"))="" SET OPTION("COLORS","NORM")="14^4" ;"white on blue
  IF $GET(OPTION("COLORS","HIGH"))="" SET OPTION("COLORS","HIGH")="14^6" ;"white on cyan
  IF $GET(OPTION("COLORS","SELECTED"))="" SET OPTION("COLORS","SELECTED")="14^5" ;"white on magenta
  IF $GET(OPTION("COLORS","HI-SEL"))="" SET OPTION("COLORS","HI-SEL")="13^2" ;"Cyan  on magenta
  IF $GET(OPTION("COLORS","HEADER"))="" SET OPTION("COLORS","HEADER")=OPTION("COLORS","NORM")
  IF $GET(OPTION("COLORS","FOOTER"))="" SET OPTION("COLORS","FOOTER")=OPTION("COLORS","NORM")
  IF $GET(OPTION("COLORS","TOP LINE"))="" SET OPTION("COLORS","TOP LINE")=OPTION("COLORS","NORM")
  IF $GET(OPTION("COLORS","BOTTOM LINE"))="" SET OPTION("COLORS","BOTTOM LINE")=OPTION("COLORS","NORM")
  IF $GET(OPTION("COLORS","INDEX"))="" SET OPTION("COLORS","INDEX")=OPTION("COLORS","NORM")
  DO EXPANDCOLORS(.OPTION)
  QUIT
  ;
SETCOLORS(OUT,COLNUM,OPTION) ;"Setup colors array 
  IF COLNUM=1 MERGE OUT("COLORS")=OPTION("COLORS")
  ELSE  MERGE OUT("COLORS")=OPTION("COLUMNS",COLNUM,"COLORS")
  IF $DATA(OUT("COLORS"))=0 DO SETDEFCOLORS(.OUT)
  DO EXPANDCOLORS(.OUT)
  QUIT
  ;
EXPANDCOLORS(OPTION) ;  
  NEW IDX SET IDX=""                          
  FOR  SET IDX=$ORDER(OPTION("COLORS",IDX)) QUIT:(IDX="")  DO
  . NEW COLORS SET COLORS=$GET(OPTION("COLORS",IDX))
  . NEW FG SET FG=$PIECE(COLORS,"^",1) IF FG="" SET FG=0
  . NEW BG SET BG=$PIECE(COLORS,"^",2) IF BG="" SET BG=1
  . SET OPTION("COLORS",IDX,"FG")=FG
  . SET OPTION("COLORS",IDX,"BG")=BG
  QUIT
  ;
PARSCOLR(TEXT,TEXTA)  ;
  ;"Purpose: To extract a color code from TEXT
  ;"Example:  Input TEXT  = 'This is {{HIGH}}something{{NORM}} to see.'
  ;"          Output TEXT = 'something{{NORM}} to see.'
  ;"          Output TEXTA = 'This is '
  ;"            function result = 'HIGH'
  ;"Input: TEXT -- PASS BY REFERENCE
  ;"         TEXTA -- PASS BY REFERENCE, and OUT PARAMETER
  ;"Result: the color name inside brackets, e.g. 'HIGH'
  NEW STR,RESULT
  SET STR=TEXT
  SET TEXTA=$PIECE(STR,"{{",1)
  SET RESULT=$PIECE(STR,"{{",2)
  SET RESULT=$PIECE(RESULT,"}}",1)
  SET TEXT=$PIECE(STR,"}}",2,99)
  QUIT RESULT
  ;
HASCOLOR(TEXT)  ;
  NEW TEMP SET TEMP=$$PARSCOLR(TEXT)
  QUIT (TEMP'="")
  ;
STRIPCOLOR(TEXT) ;"Strip out {{color tags}}
  ;"Input: TEXT.  Input text.  E.g. 'This is {{HIGH}}something{{NORM}} to see.'
  ;"Result: 'This is something to see.'
  NEW TEMP SET TEMP=TEXT
  NEW COLOR,TEXTA
  FOR  DO  QUIT:TEXTA=""
  . SET COLOR=$$PARSCOLR(.TEMP,.TEXTA)
  . SET TEMP=TEXTA_TEMP
  QUIT TEMP
  ;
  ;
  ;"=====================================================
  ;"Test code below
  ;"=====================================================
  ;
TESTSCRL ;
  ;"NOTE: There is also DEMOSCRL below 
  NEW ARRAY,OPTION
  NEW IDX FOR IDX=1:1:136 DO
  . SET ARRAY(IDX,"Line "_IDX)="Result for "_IDX
  SET OPTION("HEADER",1)=" - < Here is a header line > -"
  SET OPTION("HEADER",2)="1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
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
  ;"============================================================
  ;
DEMOSCRL ;"A DEMONSTRATION OF SCROLLER
  ;"NOTE: There is also TESTSCRL above 
  NEW DEMOARR,OPTION
  NEW JDX SET JDX=1
  NEW IDX SET IDX=0
  FOR  SET IDX=IDX+1 DO  QUIT:IDX=0
  . NEW LINE SET LINE=$TEXT(+IDX^TMGUSRIF)
  . IF LINE="" SET IDX=0 QUIT
  . SET DEMOARR(JDX,LINE)=IDX,JDX=JDX+1
  SET OPTION("COLORS","BOLD")="10^1"
  SET OPTION("HEADER",1)="This is the code for {{BOLD}}TMGUSRIF{{NORM}} file"
  SET OPTION("HEADER",2)="1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
  SET OPTION("SHOW INDEX")=1
  DO SCROLLER("DEMOARR",.OPTION)
  QUIT
  ;  
DEMOCOLS ;"DEMONSTRATE SCROLLER WITH COLUMNS.  
  ;"NOTE: There is also DEMOSCRL below 
  NEW ARRAY,OPTION,TEMPCLR
  SET OPTION("COLUMNS","NUM")=3 
  SET OPTION("COLUMNS",1,"WIDTH")=30
  SET OPTION("COLUMNS","FN TOGGLE NUM")="F1"
  SET OPTION("LR SCROLLING")=1
  NEW RTN SET RTN="TMG"
  NEW IDX FOR IDX=1:1:60 DO
  . SET RTN=$ORDER(^DIC(9.8,"B",RTN)) QUIT:RTN=""
  . SET ARRAY(IDX,RTN)="Result for "_RTN
  . NEW JDX SET JDX=0
  . FOR  SET JDX=JDX+1 DO  QUIT:JDX=0
  . . NEW LINE SET LINE=$TEXT(+JDX^@RTN)
  . . IF LINE="" SET JDX=0 QUIT
  . . SET ARRAY("COL",2,IDX,JDX)=LINE
  SET OPTION("HEADER",1)=" - < Here is a header line > -"
  SET OPTION("HEADER",2)=" Have a great day!"
  SET OPTION("FOOTER",1)="Enter ^ to exit"
  SET OPTION("ON SELECT")="HNDONSEL^TMGUSRIF"
  SET OPTION("ON CMD")="HNDONCMD^TMGUSRIF"
  SET OPTION("ON CURSOR")="HNDONCURSOR^TMGUSRIF"
  ;
  SET OPTION("COLORS","NORM")=$$COLORPAIR^TMGTERM("WHITE","BLUE",.TMPCLR) 
  SET OPTION("COLORS","HIGH")=$$COLORPAIR^TMGTERM("WHITE","CYAN",.TMPCLR) 
  SET OPTION("COLORS","HEADER")=$$COLORPAIR^TMGTERM("WHITE","RED",.TMPCLR)
  SET OPTION("COLORS","FOOTER")=$$COLORPAIR^TMGTERM("WHITE","RED",.TMPCLR)
  SET OPTION("COLORS","TOP LINE")=$$COLORPAIR^TMGTERM("BLACK","RED",.TMPCLR)
  SET OPTION("COLORS","BOTTOM LINE")=$$COLORPAIR^TMGTERM("BLACK","RED",.TMPCLR)
  SET OPTION("COLORS","INDEX")=$$COLORPAIR^TMGTERM("BLACK","WHITE",.TMPCLR)
  ; 
  SET OPTION("COLUMNS",2,"COLORS","NORM")=$$COLORPAIR^TMGTERM("WHITE","RED",.TMPCLR)
  SET OPTION("COLUMNS",2,"COLORS","HIGH")=$$COLORPAIR^TMGTERM("WHITE","CYAN",.TMPCLR) 
  SET OPTION("COLUMNS",2,"COLORS","INDEX")=$$COLORPAIR^TMGTERM("BLACK","WHITE",.TMPCLR)
  ;
  SET OPTION("SHOW INDEX")=0
  ;
  DO SCROLLER("ARRAY",.OPTION)
  QUIT
  ;
HNDONCURSOR(TMGPSCRLARR,OPTION,INFO)  ;"Part of DEMOCOLS
  ;"Purpose: handle ON SELECT event from SCROLLER
  ;"Input: TMGPSCRLARR,OPTION,INFO -- see documentation in SCROLLER
  ;"       INFO has this:
  ;"          INFO("USER INPUT")=INPUT
  ;"          INFO("CURRENT LINE","NUMBER")=number currently highlighted line
  ;"          INFO("CURRENT LINE","TEXT")=Text of currently highlighted line
  ;"          INFO("CURRENT LINE","RETURN")=return value of currently highlighted line
  NEW CURSOR SET CURSOR=$GET(INFO("CURSOR"))
  NEW ACTIVECOL SET ACTIVECOL=+$GET(OPTION("COLUMNS","ACTIVE"),1)
  NEW MAXCOLS SET MAXCOLS=+$GET(OPTION("COLUMNS","NUM"),1)
  IF 1=0,CURSOR="RIGHT" DO  QUIT
  . IF ACTIVECOL>=MAXCOLS QUIT
  . SET ACTIVECOL=ACTIVECOL+1
  . SET TMGSCLRMSG="FULL"  
  . SET OPTION("COLUMNS","ACTIVE")=ACTIVECOL
  . SET INFO("CURSOR","HANDLED")=1
  IF 1=0,CURSOR="LEFT" DO  QUIT
  . IF ACTIVECOL=1 QUIT
  . SET ACTIVECOL=ACTIVECOL-1
  . SET TMGSCLRMSG="FULL"  
  . SET OPTION("COLUMNS","ACTIVE")=ACTIVECOL
  . SET INFO("CURSOR","HANDLED")=1
  QUIT
  ;"============================================================
  ;