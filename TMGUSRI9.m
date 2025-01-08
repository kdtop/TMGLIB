TMGUSRI9 ;TMG/kst/USER INTERFACE -- Table Drawing ;11/3/24
         ;;1.0;TMG-LIB;**1**;11/3/24
 ;
 ;"TMG USER INTERFACE API FUNCTIONS
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 11/3/24  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"======================================================================= 
 ;"=======================================================================
 ;"GENERAL FUNCTIONS
 ;"=======================================================================
 ;"QUERYTABLE(DATA,WIDTH) -- get dimensions for table, after it is wrapped etc.   
 ;"DRAWTABLE(LEFT,TOP,WIDTH,DATA,OPTION,CHARS) -- Draw table, with surrounding line drawing
 ;"GETLOREM(WORDCT,STARTWORD,LORSTR)  ;  
 ;"=======================================================================
 ;"Demo Functions
 ;"=======================================================================
 ;"DEMOTABLES(COUNT) ;
 ;"DEMOTABLE ;
 ;"TESTTABLE() ;
 ;
 ;"=======================================================================
 ;"Private Functions
 ;"=======================================================================
 ;"DRAWROWLINE(MODE,SPCABVREF,SPCBLWREF,LEFT,TOP,WIDTH,FGCOLOR,BGCOLOR,CHARS,OPTION)
 ;"NEXTCNCTR(TABLEXPOS,SPCABVREF,SPCBLWREF) --Get next connection position as progressing from left to right on line.
 ;"PREPDATA(DATA,WIDTH,FIRSTROW,LASTROW) 
 ;"SETCOLSPACING(ROWREF,TOTALWIDTH) 
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;"DEPENDENCIES:      
 ;"=======================================================================
 ; 
DEMOTABLES(COUNT) ;
  SET COUNT=+$GET(COUNT)
  IF COUNT'>0 SET COUNT=$RANDOM(20)+3
  DO CSRSHOW^TMGTERM(0)
  NEW IDX FOR IDX=1:1:COUNT DO
  . WRITE # 
  . DO DEMOTABLE 
  . HANG 1
  DO CSRSHOW^TMGTERM(1)
  QUIT
  ;
DEMOTABLE ;
  NEW LOREMSTR,TABLE
  NEW FG,BG
  NEW LOREMSTART SET LOREMSTART=1
  NEW ROWCT SET ROWCT=$RANDOM(5)+1
  NEW COLORS
  SET TABLE("TABLE","COLOR")=$$RANDCOLORPAIR^TMGUSRI8(.COLORS)
  NEW AROW FOR AROW=1:1:ROWCT DO
  . NEW CELLCT SET CELLCT=$RANDOM(5)+1
  . NEW ACELL FOR ACELL=1:1:CELLCT DO
  . . NEW WORDCT SET WORDCT=$RANDOM(5)+3
  . . SET TABLE(AROW,ACELL)=$$GETLOREM(WORDCT,.LOREMSTART,.LOREMSTR)
  . . SET TABLE(AROW,ACELL,"COLOR")=$$RANDCOLORPAIR^TMGUSRI8(.COLORS)
  NEW WIDTH SET WIDTH=$RANDOM(40)+20
  NEW X SET X=$RANDOM(40)+1
  NEW Y SET Y=$RANDOM(20)+1
  NEW OPTION 
  SET OPTION("THICK")=$RANDOM(2)+1  ;"1..3
  SET OPTION("ARC")=$RANDOM(2)  ;"0..1
  DO DRAWTABLE(X,Y,WIDTH,.TABLE,.OPTION)
  QUIT
 ;
TESTTABLE() ;
  NEW DATA,AROW,ROWNUM
  SET ROWNUM=1
  SET DATA(ROWNUM,1)="TITLE"    
  SET ROWNUM=ROWNUM+1 
  SET DATA(ROWNUM,1)="Cell 2,1"
  SET DATA(ROWNUM,2)="Cell 2,2 can have more text than others."
  SET DATA(ROWNUM,3)="Cell 2,3"
  SET ROWNUM=ROWNUM+1 
  SET DATA(ROWNUM,1)="Cell 3,1"
  SET DATA(ROWNUM,2)="Cell 3,2"
  SET DATA(ROWNUM,3)="Cell 3,3"
  SET DATA(ROWNUM,4)="Cell 3,4"
  SET ROWNUM=ROWNUM+1 
  SET DATA(ROWNUM,1)="Cell 4,1"
  SET DATA(ROWNUM,1,"WIDTH")=10
  SET DATA(ROWNUM,2)="Cell 4,2"
  SET DATA(ROWNUM,2,"WIDTH")=30
  SET ROWNUM=ROWNUM+1 
  SET DATA(ROWNUM,1)="Footer"
  ;
  NEW OPTION SET OPTION("ASCII")=2
  WRITE #
  NEW RESULT SET RESULT=$$DRAWTABLE(5,20,40,.DATA,.OPTION)
  QUIT RESULT
  ;
QUERYTABLE(DATA,WIDTH) ;"get dimensions for table, after it is wrapped etc  
  ;"Input:  DATA -- PASS BY REFERENCE.  see DRAWTABLE for documentation
  ;"        OPTION -- PASS BY REFERENCE.  see DRAWTABLE for documentation
  ;"RESULT:  HEIGHT^WIDTH  <--- NOTICE this is really Y^X, backwards positioning from normal.  
  NEW RESULT SET RESULT=$$PREPDATA(.DATA,.WIDTH)
  QUIT RESULT
  ;
DRAWTABLE(LEFT,TOP,WIDTH,DATA,OPTION,CHARS) ;"Draw table, with surrounding boxes
  ;"Input:  LEFT - Screen coordinates of position of TOP 
  ;"        TOP  - Screen coordinates of position of TOP
  ;"        WIDTH -- Width of line
  ;"        DATA -- PASS BY REFERENCE.  Format:
  ;"           <ROW#>,<COL#> can be any number, but must be numeric and >0
  ;"                  NOTE: each row may have different number of columns.  
  ;"           DATA(<ROW#>,<COL#>)=TEXT  <-- Pass this in as user.  Routine will move, wrapping if needed, into "TEXT" node 
  ;"             or
  ;"           DATA(<ROW#>,<COL#>,"TEXT",#)=<TEXT>  line of text
  ;"                NOTE: text will be wrapped into width for cell.  This will determine cell height.  
  ;"                      If user passes in text already in "TEXT" array, it will clipped to cell width
  ;"           DATA(<ROW#>,<COL#>,"WIDTH")=#  OPTIONAL.  Default is even division of total width
  ;"                                       If not provided, will be automatically added during processing.
  ;"                                       NOTE: A cell width should START with 1ST '|' character, and extend to 
  ;"                                       its last SPACE (not counting next '|' or right side wall) 
  ;"           DATA(<ROW#>,<COL#>,"COLOR")=FG^BG  OPTIONAL.  Default table FGCOLOR,BGCOLOR
  ;"           DATA(<ROW#>,<COL#>,"ALIGN")='L', 'R', OR 'C' OPTIONAL.  LEFT, RIGHT, OR CENTER.  Default = CENTER
  ;"           DATA(<ROW#>,"SPACING",<START POS>)=<COL #>  <-- will be automatically added during processing.
  ;"                   NOTE: When drawing, this start position will be draw with "|" cell divider.  
  ;"           DATA(<ROW#>,"HEIGHT")=# <-- will be automatically added during processing.
  ;"           DATA("TABLE","COLOR")=FG^BG 
  ;"                  FG -- foreground color.  Format same as accepted by COLORS^TMGTERM
  ;"                        If -1, then terminal color is RESET to default.  If BGCOLOR=-1, FGCOLOR is overridden  
  ;"                        This is foreground color of borders of table.  And default for text of table if not overridden by specified cell colors
  ;"                  BG -- background color.  Format same as accepted by COLORS^TMGTERM
  ;"                        If -1, then terminal color is RESET to default.  If FGCOLOR=-1, BGCOLOR is overridden  
  ;"                        This is background color of borders of table.  And default background for cell by specified cell colors
  ;"           DATA("TABLE","PREPPED")=1  Automatically added during processing.  If found, then processing not repeated.  
  ;"        OPTION -- Optional.  Format:
  ;"            OPTION("ASCII")=1 -- If found then all lines drawn with '-', '|', '+'
  ;"            OPTION("THICK") -- 1 means Light  (default)
  ;"                               2 means Heavy
  ;"                               3 means Double
  ;"            OPTION("DASH") --  0 means mode OFF       Default=0
  ;"                               1 means double dash
  ;"                               2 means triple dash
  ;"                               3 means quadruple dash
  ;"            OPTION("BUFFERED")=<Buffer name>.  If defined, output into buffer instead of to screen.
  ;"                            See TMGTERM4 for more info   
  ;"            OPTION("NO TERM SAVE")=1 -- optional. If found, then prior terminal state is NOT saved and restored
  ;"        CHARS -- OPTIONAL -- PASS BY REFERENCE.  Can be used to save time with repeat calls.  
  ;"RESULT: Returns final total height of table.    
  IF $DATA(CHARS)=0 DO GET4BOXARR^TMGTERM2(.CHARS,.OPTION)
  NEW FIRSTROW,LASTROW 
  NEW FGCOLOR,BGCOLOR SET (FGCOLOR,BGCOLOR)=-1
  IF $DATA(DATA("TABLE","COLOR")) DO SPLITCOLORPAIR^TMGUSRI8(DATA("TABLE","COLOR"),.FGCOLOR,.BGCOLOR)
  DO PREPDATA(.DATA,WIDTH,.FIRSTROW,.LASTROW)
  NEW XPOS,YPOS SET YPOS=TOP-1
  SET LEFT=+$GET(LEFT)
  NEW AROW SET AROW=0
  FOR  SET AROW=$ORDER(DATA(AROW)) QUIT:AROW'>0  DO
  . ;"NOTE: Each row cycle, will paint TOP line of row, and cell contents of row.  
  . SET YPOS=YPOS+1
  . SET XPOS=LEFT
  . IF AROW=FIRSTROW DO
  . . ;"DRAW TOP LINE, with down T's for each cell divider
  . . DO DRAWROWLINE("T","",$NAME(DATA(AROW,"SPACING")),XPOS,YPOS,WIDTH,.FGCOLOR,.BGCOLOR,.CHARS,.OPTION)
  . ELSE  DO
  . . ;"DRAW TOP OF CELL, with down T's and Up T's for dividers above and below.
  . . DO DRAWROWLINE("M",$NAME(DATA(AROW-1,"SPACING")),$NAME(DATA(AROW,"SPACING")),XPOS,YPOS,WIDTH,.FGCOLOR,.BGCOLOR,.CHARS,.OPTION)
  . NEW ROWHT SET ROWHT=DATA(AROW,"HEIGHT")
  . NEW IDX FOR IDX=1:1:ROWHT DO
  . . SET XPOS=LEFT
  . . SET YPOS=YPOS+1
  . . DO PAINTXY^TMGTERM4(XPOS,YPOS,.FGCOLOR,.BGCOLOR,CHARS("SIDE"),"OPTION") SET XPOS=XPOS+1 ;"Paint left '|' of line
  . . NEW ACOL SET ACOL=0
  . . FOR  SET ACOL=$ORDER(DATA(AROW,ACOL)) QUIT:ACOL'>0  DO
  . . . NEW CELLFGCOLOR,CELLBGCOLOR SET CELLFGCOLOR=FGCOLOR,CELLBGCOLOR=BGCOLOR
  . . . NEW LINE SET LINE=$GET(DATA(AROW,ACOL,"TEXT",IDX)) 
  . . . NEW ALIGN SET ALIGN=$GET(DATA(AROW,ACOL,"ALIGN"),"C")
  . . . ;"NOTE: A cell width should START with 1 '|' character, and extend to its last SPACE (not counting next '|' or right side wall)
  . . . NEW CELLWIDTH SET CELLWIDTH=DATA(AROW,ACOL,"WIDTH")-1  ;"1st chars of cellwidth are taken up by '|' lines, so reduce by 1
  . . . IF $LENGTH(LINE)>CELLWIDTH SET LINE=$EXTRACT(LINE,1,CELLWIDTH)  
  . . . IF ALIGN="L" DO
  . . . . SET LINE=$$LJ^XLFSTR(LINE,CELLWIDTH," ") 
  . . . IF ALIGN="R" DO
  . . . . SET LINE=$$RJ^XLFSTR(LINE,CELLWIDTH," ")
  . . . IF ALIGN="C" DO
  . . . . SET LINE=$$CJ^XLFSTR(LINE,CELLWIDTH," ")
  . . . ;"Output 1 line of text for cell
  . . . IF $DATA(DATA(AROW,ACOL,"COLOR")) DO SPLITCOLORPAIR^TMGUSRI8(DATA(AROW,ACOL,"COLOR"),.CELLFGCOLOR,.CELLBGCOLOR)
  . . . DO PAINTXY^TMGTERM4(XPOS,YPOS,.CELLFGCOLOR,.CELLBGCOLOR,LINE,"OPTION") SET XPOS=XPOS+$LENGTH(LINE)
  . . . ;"Ouput | divider for right side of cell
  . . . DO PAINTXY^TMGTERM4(XPOS,YPOS,.FGCOLOR,.BGCOLOR,CHARS("SIDE"),"OPTION") SET XPOS=XPOS+1
  . ;"On LAST row cycle, will ALSO draw bottom row
  . IF AROW=LASTROW DO
  . . SET XPOS=LEFT
  . . SET YPOS=YPOS+1
  . . ;"DRAW BOTTOM LINE, WITH Up T's for each divider
  . . DO DRAWROWLINE("B",$NAME(DATA(AROW,"SPACING")),"",XPOS,YPOS,WIDTH,.FGCOLOR,.BGCOLOR,.CHARS,.OPTION)
  DO COLORS^TMGTERM(-1,-1,.OPTION)
  QUIT (YPOS-TOP+1)
  ;
DRAWROWLINE(MODE,SPCABVREF,SPCBLWREF,LEFT,TOP,WIDTH,FGCOLOR,BGCOLOR,CHARS,OPTION)
  ;"MODE="T","M","B" for TOP, MIDDLE, BOTTOM lines
  ;"SPCABVREF -- REFERENCE for spacing array for row ABOVE line, or "" if none
  ;"SPCBLWREF -- REFERENCE for spacing array for row BELOW line, or "" if none
  ;
  SET LEFT=+$GET(LEFT)
  NEW XPOS,YPOS SET YPOS=+$GET(TOP)
  SET MODE=$GET(MODE)
  NEW LFCHNAME SET LFCHNAME=$SELECT(MODE="T":"TL",MODE="B":"BL",1:"T RT")
  NEW RTCHNAME SET RTCHNAME=$SELECT(MODE="T":"TR",MODE="B":"BR",1:"T LF")
  NEW NEXTCONNECTION SET NEXTCONNECTION=0
  NEW TABLEXPOS FOR TABLEXPOS=1:1:WIDTH DO
  . NEW CHNAME SET CHNAME=""
  . IF +NEXTCONNECTION=0 SET NEXTCONNECTION=$$NEXTCNCTR(TABLEXPOS,SPCABVREF,SPCBLWREF)
  . IF TABLEXPOS=1 SET CHNAME=LFCHNAME,NEXTCONNECTION=0
  . ELSE  IF TABLEXPOS=WIDTH SET CHNAME=RTCHNAME
  . ELSE  IF (TABLEXPOS<NEXTCONNECTION)!(NEXTCONNECTION="-1^NONE") SET CHNAME="TOP/BOT"
  . ELSE  IF TABLEXPOS=+NEXTCONNECTION DO  
  . . NEW TYPE SET TYPE=$PIECE(NEXTCONNECTION,"^",2)
  . . SET CHNAME=$SELECT(TYPE="UP":"BOT T",TYPE="DN":"TOP T",TYPE="CROSS":"CROSS",1:"") 
  . . SET NEXTCONNECTION=0  ;"signal to check for next connection
  . IF CHNAME'="" DO
  . . SET XPOS=LEFT+TABLEXPOS-1 
  . . DO PAINTXY^TMGTERM4(XPOS,YPOS,.FGCOLOR,.BGCOLOR,CHARS(CHNAME),"OPTION")
  QUIT
  ;
NEXTCNCTR(TABLEXPOS,SPCABVREF,SPCBLWREF) ;"Get next connection position as progressing from left to right on line.
  ;"TABLEXPOS -- current X position on line 
  ;"SPCABVREF -- REFERENCE for spacing array for row ABOVE line, or "" if none
  ;"SPCBLWREF -- REFERENCE for spacing array for row BELOW line, or "" if none
  ;"Result: '<TABLEXPOS>^UP', '<TABLEXPOS>^DN', '<TABLEXPOS>^CROSS', '-1^NONE'   <TABLEXPOS> is the first X position that occurs AFTER input TABLEXPOS
  NEW RESULT SET RESULT=""
  SET TABLEXPOS=+$GET(TABLEXPOS)
  NEW BIGNUM SET BIGNUM=999999
  NEW ABOVEPOS,BELOWPOS,ABOVEARR,BELOWARR SET (ABOVEARR,BELOWARR)=""
  SET SPCABVREF=$GET(SPCABVREF)
  SET SPCBLWREF=$GET(SPCBLWREF)
  IF SPCABVREF]"" MERGE ABOVEARR=@SPCABVREF
  IF SPCBLWREF]"" MERGE BELOWARR=@SPCBLWREF
  SET ABOVEPOS=$ORDER(ABOVEARR(TABLEXPOS-1))
  IF ABOVEPOS="" SET ABOVEPOS=BIGNUM
  SET BELOWPOS=$ORDER(BELOWARR(TABLEXPOS-1))
  IF BELOWPOS="" SET BELOWPOS=BIGNUM
  IF ABOVEPOS<BELOWPOS DO
  . SET RESULT=ABOVEPOS_"^UP"
  ELSE  IF BELOWPOS<ABOVEPOS DO
  . SET RESULT=BELOWPOS_"^DN"
  ELSE  IF BELOWPOS=ABOVEPOS DO
  . IF BELOWPOS=BIGNUM SET RESULT="-1^NONE" QUIT
  . SET RESULT=ABOVEPOS_"^CROSS"
  QUIT RESULT
  ;
PREPDATA(DATA,WIDTH,FIRSTROW,LASTROW) ;
  ;"Input:  DATA -- PASS BY REFERENCE.  Format:
  ;"           <ROW#>,<COL#> can be any number, but must be numeric and >0
  ;"                  NOTE: each row may have different number of columns.  
  ;"           DATA(<ROW#>,<COL#>)=TEXT  <-- Pass this in as user.  Routine will move, wrapping if needed, into "TEXT" node 
  ;"             or
  ;"           DATA(<ROW#>,<COL#>,"TEXT",#)=<TEXT>  line of text
  ;"                NOTE: text will be wrapped into width for cell.  This will determine cell height.  
  ;"                      If user passes in text already in "TEXT" array, it will clipped to cell width
  ;"           DATA(<ROW#>,<COL#>,"WIDTH")=#  OPTIONAL.  Default is even division of total width
  ;"                                       If not provided, will be automatically added during processing.
  ;"                                       NOTE: A cell width should START with 1ST '|' character, and extend to 
  ;"                                       its last SPACE (not counting next '|' or right side wall) 
  ;"           DATA(<ROW#>,<COL#>,"COLOR")=FG^BG  OPTIONAL.  Default table FGCOLOR,BGCOLOR
  ;"           DATA(<ROW#>,<COL#>,"ALIGN")='L', 'R', OR 'C' OPTIONAL.  LEFT, RIGHT, OR CENTER.  Default = CENTER
  ;"           DATA(<ROW#>,"SPACING",<START POS>)=<COL #>  <-- will be automatically added during processing.
  ;"                   NOTE: When drawing, this start position will be draw with "|" cell divider.  
  ;"           DATA(<ROW#>,"HEIGHT")=# <-- will be automatically added during processing.
  ;"           DATA("TABLE","COLOR")=FG^BG <-- will be automatically added during processing, from FGCOLOR,BGCOLOR params
  ;"           DATA("TABLE","PREPPED")=1  Automatically added during processing.  If found, then processing not repeated.  
  ;"        WIDTH -- Width of line
  ;"        FIRSTROW,LASTROW -- OUT parameters
  ;"RESULTS: <HEIGHT>^<WIDTH>  Returns total height and width of table (including dividing lines)
  IF $GET(DATA("TABLE","PREPPED"))=1 DO  GOTO PDDN  ;"Don't reprocess if done already.
  . ;"Since skipping reprocessing below, just set up FIRSTROW and LASTROW.  
  . SET (FIRSTROW,LASTROW)=0
  . NEW AROW SET AROW=0
  . FOR  SET AROW=$ORDER(DATA(AROW)) QUIT:AROW'>0  DO
  . . IF FIRSTROW=0 SET FIRSTROW=AROW
  . . SET LASTROW=AROW  ;"only last value will be saved  
  NEW AROW SET AROW=0
  ;"Setup column spacing for each row.  
  FOR  SET AROW=$ORDER(DATA(AROW)) QUIT:AROW'>0  DO
  . DO SETCOLSPACING($NAME(DATA(AROW)),WIDTH)
  ;"Wrap text for each cell if needed, and determine height of row.
  SET (FIRSTROW,LASTROW)=0
  NEW TOTALHT SET TOTALHT=0
  SET AROW=0
  FOR  SET AROW=$ORDER(DATA(AROW)) QUIT:AROW'>0  DO
  . IF FIRSTROW=0 SET FIRSTROW=AROW
  . SET LASTROW=AROW  ;"only last value will be saved
  . NEW MAXHT SET MAXHT=1
  . NEW ACOL SET ACOL=0
  . FOR  SET ACOL=$ORDER(DATA(AROW,ACOL)) QUIT:ACOL'>0  DO
  . . NEW AWIDTH SET AWIDTH=DATA(AROW,ACOL,"WIDTH")
  . . NEW TEXT SET TEXT=$GET(DATA(AROW,ACOL))
  . . IF $LENGTH(TEXT)>AWIDTH DO
  . . . NEW TEMPARR DO STR2WP^TMGSTUT2(TEXT,"TEMPARR",AWIDTH) 
  . . . MERGE DATA(AROW,ACOL,"TEXT")=TEMPARR
  . . . NEW ARRHT SET ARRHT=$$LISTCT^TMGMISC2($NAME(DATA(AROW,ACOL,"TEXT")))
  . . . IF ARRHT>MAXHT SET MAXHT=ARRHT
  . . ELSE  DO
  . . . SET DATA(AROW,ACOL,"TEXT",1)=TEXT
  . . SET DATA(AROW,ACOL)=""  ;"REMOVE original text (if any)
  . SET DATA(AROW,"HEIGHT")=MAXHT
  . SET TOTALHT=TOTALHT+MAXHT+1  ;"+1 for the line drawn ABOVE this row
  SET DATA("TABLE","DIMENSIONS")=(TOTALHT+1)_"^"_WIDTH  ;"+1 for the bottom row
  SET DATA("TABLE","PREPPED")=1
PDDN ;  
  QUIT $GET(DATA("TABLE","DIMENSIONS"))
  ;
SETCOLSPACING(ROWREF,TOTALWIDTH) ;
  ;"NOTE: A cell width should START with 1ST '|' character, and extend to its last SPACE (not counting next '|' or right side wall)
  NEW ACOL SET ACOL=0
  NEW CLAIMEDWIDTH SET CLAIMEDWIDTH=0 
  NEW NUMCELLSWITHWIDTH SET NUMCELLSWITHWIDTH=0
  NEW NUMCOLS SET NUMCOLS=0
  NEW LASTCOL SET LASTCOL=0
  ;"Survey table for user-set widths
  FOR  SET ACOL=$ORDER(@ROWREF@(ACOL)) QUIT:ACOL'>0  DO
  . SET LASTCOL=ACOL  ;"only value from last loop will be kept
  . SET NUMCOLS=NUMCOLS+1
  . NEW AWIDTH SET AWIDTH=+$GET(@ROWREF@(ACOL,"WIDTH"))
  . IF AWIDTH'>0 QUIT
  . IF CLAIMEDWIDTH+AWIDTH>(TOTALWIDTH-1) DO
  . . SET AWIDTH=(TOTALWIDTH-1)-CLAIMEDWIDTH
  . . SET @ROWREF@(ACOL,"WIDTH")=AWIDTH
  . SET CLAIMEDWIDTH=CLAIMEDWIDTH+AWIDTH
  . SET NUMCELLSWITHWIDTH=NUMCELLSWITHWIDTH+1
  ;"Calculate widths for non-specified cells
  NEW WORKINGWIDTH SET WORKINGWIDTH=TOTALWIDTH-CLAIMEDWIDTH-1  ;"Subtract 1 for (R) side wall
  NEW NUMVARWCOLS SET NUMVARWCOLS=NUMCOLS-NUMCELLSWITHWIDTH
  NEW AUTOWT SET AUTOWT=$SELECT(NUMVARWCOLS>0:WORKINGWIDTH\NUMVARWCOLS,1:0)
  ;"Specify every cell width, if not already set
  NEW POSITION SET POSITION=1
  NEW CALCWT SET CALCWT=0
  SET ACOL=0
  FOR  SET ACOL=$ORDER(@ROWREF@(ACOL)) QUIT:ACOL'>0  DO
  . NEW USRWT SET USRWT=+$GET(DATA(AROW,ACOL,"WIDTH")) ;"May already have been set by user.
  . NEW AWIDTH SET AWIDTH=$SELECT(USRWT>0:USRWT,1:AUTOWT)
  . SET CALCWT=CALCWT+AWIDTH
  . IF ACOL=LASTCOL,CALCWT<(TOTALWIDTH-1) DO  ;"Add extra width to last column if needed to match total width
  . . SET AWIDTH=AWIDTH+((TOTALWIDTH-1)-CALCWT)  
  . SET @ROWREF@(ACOL,"WIDTH")=AWIDTH
  . SET @ROWREF@("SPACING",POSITION)=ACOL
  . SET POSITION=POSITION+AWIDTH
  QUIT
  ;
GETLOREM(WORDCT,STARTWORD,LORSTR)  ;  
  ;"INPUT:  WORDCT -- number of words to return
  ;"        STARTWORD -- OPTIONAL.  Default is 1.  Can pass by reference
  ;"        LORSTR -- OPTIONAL.  String with Lorem text.  If empty, then filled from LOREMTEXT
  IF $GET(LORSTR)="" DO
  . NEW LINE
  . SET LORSTR=""
  . NEW DONE SET DONE=0
  . NEW IDX FOR IDX=1:1 DO  QUIT:DONE
  . . SET LINE=$TEXT(LOREMTEXT+IDX^TMGUSRI9)
  . . IF LINE["<DONE>" SET DONE=1 QUIT
  . . SET LINE=$PIECE(LINE,";;""",2)
  . . SET LORSTR=LORSTR_$$TRIM^XLFSTR(LINE)_" "
  SET WORDCT=+$GET(WORDCT,1)
  SET STARTWORD=+$GET(STARTWORD,1)
  NEW LOREMWORDS SET LOREMWORDS=$LENGTH(LORSTR," ")
  NEW ENDPCE SET ENDPCE=STARTWORD+WORDCT-1
  NEW RESULT
  IF ENDPCE>LOREMWORDS DO  ;"NEEDS WRAPPING
  . NEW ENDPCE1 SET ENDPCE1=LOREMWORDS-STARTWORD
  . NEW ENDPCE2 SET ENDPCE2=WORDCT-ENDPCE1
  . SET RESULT=$PIECE(LORSTR," ",STARTWORD,ENDPCE1)
  . SET RESULT=$$TRIM^XLFSTR(RESULT)_" "_$PIECE(LORSTR," ",1,ENDPCE2)
  . SET STARTWORD=ENDPCE2+1
  ELSE  DO
  . SET RESULT=$PIECE(LORSTR," ",STARTWORD,ENDPCE)
  . SET STARTWORD=ENDPCE+1
  QUIT RESULT
  ;
GETLOREMARR(OUT,WORDCT,STARTWORD,LORSTR)  ;  
  ;"INPUT:  WORDCT -- number of words to return
  ;"        STARTWORD -- OPTIONAL.  Default is 1.  Can pass by reference
  ;"        LORSTR -- OPTIONAL.  String with Lorem text.  If empty, then filled from LOREMTEXT
  NEW STR SET STR=$$GETLOREM(.WORDCT,.STARTWORD,.LORSTR)
  DO STR2WP^TMGSTUT2(STR,"OUT",80)
  QUIT
  ;
LOREMTEXT ;
 ;;"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt 
 ;;"ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco 
 ;;"laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit 
 ;;"in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat 
 ;;"cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
 ;;"<DONE>
  