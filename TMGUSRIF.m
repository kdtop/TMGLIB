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
  ;"Depreciated.  Use code in ^TMGUSRI2
  QUIT $$MENU^TMGUSRI2(.OPTION,.DEFCHOICE,.USERRAW)
  ;
  ;"============================================================
  ;
SCROLLER(TMGPSCRLARR,OPTION) ;       
  ;"Purpose: Provide a scroll box
  ;"Input: TMGPSCRLARR -- PASS BY NAME.  format:
  ;"         @TMGPSCRLARR@(1,DisplayText)=Return Text <-- note: must be numbered with INTEGERS 1,2,3 etc. (not 1.1, 1.2 etc)
  ;"         @TMGPSCRLARR@(2,DisplayText)=Return Text
  ;"         @TMGPSCRLARR@(3,DisplayText)=Return Text
  ;"              NOTE: IF Display TEXT contains {{name}} then name is taken as color directive
  ;"              Example: 'Here is {{BOLD}}something{{NORM}} to see.'
  ;"              IF NAME is not defined in OPTION("COLORS",NAME), it is ignored, and code is shown. 
  ;"         @TMGPSCRLARR@("SELECTED",<#>)=1 if selected
  ;"         @TMGPSCRLARR@("COL",<COL#>,<index_for_col_1>,<index_for_col_2>,<index_for_col_3>,...,"TXT",<LINE#>)=Display text for columns 2 or higher.
  ;"                         NOTE: This is ignored unless OPTION("COLUMNS","NUM") > 1
  ;"                         NOTE: The data for column 1 is stored normally (not in "COL" subnode)
  ;"            e.g. @TMGPSCRLARR@("COL",2,5,"TXT",1)="Hello world"  <-- This text will be shown in column 2 when index 5 is selected in column #1 
  ;"                 @TMGPSCRLARR@("COL",2,5,"TXT",2)="  2nd line of text here"   
  ;"                 @TMGPSCRLARR@("COL",2,5,"TXT",3)="  3rd line of text here"   
  ;"                 @TMGPSCRLARR@("COL",2,"SELECTED",<#>)=1 if selected
  ;"            e.g. @TMGPSCRLARR@("COL",3,5,7,"TXT",1)="All we are saying..."  <--- This shown in col 3 when index 5 selected in column #1 and index 7 selected in column #2 
  ;"                 @TMGPSCRLARR@("COL",3,5,7,"TXT",2)="  is give peace a chance" 
  ;"                 @TMGPSCRLARR@("COL",3,5,7,"TXT",3)="  3rd line can go here. "   
  ;"                 @TMGPSCRLARR@("COL",3,5,7,"TXT",3,"RETURN")=<VALUE> <-- optional return or data value for this line.     
  ;"                 @TMGPSCRLARR@("COL",3,"SELECTED",<#>)=1 if selected
  ;"                         NOTE: As of 11/2024, introducing ability to NOT store data as above, but to call
  ;"                               'ON NEED DATA' event to allow programatic or dynamic data that is delivered just-in-time.  
  ;"                               If event is defined, it will be called.  If it returns HANDLED=1, then any data
  ;"                               for that column etc defined here will be ignored.  See event details below
  ;"                               See also 'ON LOAD DATA'
  ;"       OPTION -- PASS BY REFERENCE.  format:
  ;"          OPTION("UNICODE LINES")=1  If found, then unicode drawing chars use.  Otherwise "-" used.  
  ;"          OPTION("UNICODE LINES","OPTION")=<ARRAY> OPTIONAL.  Options passed to DRAWHLINE^TMGTERM.  See details there.  
  ;"          OPTION("HEADER",1)=Header line TEXT
  ;"          OPTION("HEADER",2)=More Header line TEXT (any number of lines)
  ;"          OPTION("HEADER","COL",<COL#>)=Header for specified column #.  IGNORED unless OPTION("COLUMNS","NUM") > 1
  ;"                      NOTE: Any entry > then number of columns is ignored.  
  ;"          OPTION("FOOTER",1)=Footer line TEXT  <--- OPTION 1
  ;"          OPTION("FOOTER",1,1)=linePart <--- OPTION 2  (these will be all strung together to make one footer line.
  ;"          OPTION("FOOTER",1,2)=linePart                (can be used to display switches etc)
  ;"          OPTION("FOOTER",2)=More footer line TEXT (any number of lines)
  ;"          OPTION("FOOTER","COL",<COL#>)=<text> for footer with columns  
  ;"               NOTE: As of 11/2024, the footer has been turned into a table with variable number of col's and rows
  ;"                     So "FOOTER",#,#) is being interpreted as "FOOTER",<ROW>,<COL>).  I'll have to sort out backward compatability as it arises.    
  ;"          OPTION("SHOW INDEX")=1 OPTIONAL.  If 1, then each line displayed with line number at beginning
  ;"          OPTION("SCRN WIDTH")= OPTIONAL screen width. (default is terminal width)
  ;"          OPTION("SCRN HEIGHT")= OPTIONAL screen height. (default is terminal height (IOSL) - 2)
  ;"          OPTION("SCRN TOP OFFSET")= OPTIONAL offset # to have at top of display
  ;"                                   NOTE: OPTION("SCRN HEIGHT") should be set to shortened value to account for top blank lines.
  ;"                                   This was primarly implemented to allow debugger to run on top of screen with scroller on bottom. 
  ;"          OPTION("WRAP DATA")=# If value found then display data is wrapped to width ONCE a beginning
  ;"                                but not with dynamically added data or if columns changed or viewport changed etc.  
  ;"                                If 1, then wrapped to SCRN WIDTH-2, or if other number then wrapped to this length.  
  ;"          ---- COLORs (optional) ------
  ;"          NOTE: When writing text such as 'This is {{HIGH}}text.', the HIGH color will be set, based on definition below. 
  ;"                This scroller  doesn't intrinsically know about the names of colors, e.g. 'RED', or 'BLUE'. 
  ;"                It expect '#^#' pairs instead (or CLRVEC24^CLRVEC24)
  ;"                In order to convert names to pairs, use --> e.g. $$COLORPAIR^TMGUSRI8("WHITE","BLUE",.TMPCLR)
  ;"                ALSO, a color name can be defined; see 'SomeName' example below.   
  ;"                ALTERNATIVELY, this is supported: 'This is {{5^8}}text' which will set FG to 5 and BG to 8
  ;"                               or FG^BG can both be CLRVEC24's.  E.g. {{0;0;0^255;255;255}}  (see TMGTERM for details)
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
  ;"          OPTION("LR SCROLLING") = 1   Default = 0. If 1, then left, right keys scroll text WITHIN current column rather than passing to user functions
  ;"          OPTION("USER")  -- reserved for use by user
  ;"          ---- Multi-column mode ----
  ;"          OPTION("COLUMNS","ACTIVE") = OPTIONAL.   Default = 1. Which column user can scroll currently
  ;"          OPTION("COLUMNS","NUM") = OPTIONAL.   Default = 1. Number of columns to contained in data
  ;"          OPTION("COLUMNS","MAX DISPLAY NUM") = OPTIONAL.   Default = 4. Max number of columns to show on screen at once.
  ;"                 NOTE: This is not strictly followed.  It will be used with autosizing columns, but
  ;"                       ultimately the number of columns displayed on screen will be determined by each cols' width.  
  ;"          OPTION("COLUMNS","FN TOGGLE NUM")=<FN NAME> OPTIONAL. e.g. "F9"  Default = "". Fn is shown as option to cycle through which column is active
  ;"                  Only applies if OPTION("COLUMNS","NUM")>1 
  ;"          OPTION("COLUMNS",<COL#>,"WIDTH") = OPTIONAL. e.g "40" (absolute) or "75%" (% of screen width)  
  ;"          OPTION("COLUMNS",<COL#>,"WIDTH") = OPTIONAL. e.g "20" (absolute) or "25%" (% of screen width) 
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
  ;"          OPTION("ON CHANGING")="FnName^Module" -- code to execute for cursor input  E.g. DO FnName^Module(TMGPSCRLARR,.OPTION,.INFO)
  ;"                  Event is fired when cursor is about to change UP or DN, LF, or RT. Fired after 'ON CURSOR' event 
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
  ;"          OPTION("ON CURSOR")="FnName^Module" -- code to execute BEFORE user cursor key press processed  E.g. DO FnName^Module(TMGPSCRLARR,.OPTION,.INFO)
  ;"                  Event is fired when a cursor key is pressed, before the scroller moves the highlighted line.  
  ;"                  INFO("CURSOR")=Cursor Key pressed
  ;"                  INFO("CURSOR","HANDLED")=1 <-- this is what called code should set if it handled
  ;"                                  the cursor event. This will prevent scroller from acting on it.   
  ;"          OPTION("ON CURSOR DONE")="FnName^Module" -- code to execute AFTER user cursor key press      E.g. DO FnName^Module(TMGPSCRLARR,.OPTION,.INFO)
  ;"                  Event is fired when a cursor key is pressed, AFTER the scroller moves the highlighted line.  
  ;"                  INFO("CURSOR")=Cursor Key pressed
  ;"          OPTION("ON DRAW MAIN LINE")="FnName^Module" -- code to execute before drawing line in MAIN area     E.g. DO FnName^Module(TMGPSCRLARR,.OPTION,.INFO)
  ;"                  Event is fired before line is drawn, giving user chance to change.
  ;"                  If values in INFO shown below are modified, it will change display of line (except for read-only values can't be changed)
  ;"                  INFO("DRAW MAIN","TEXT")=TEXT
  ;"                  INFO("DRAW MAIN","SELECTED")=SELECTED
  ;"                  INFO("DRAW MAIN","ON HIGHLIGHT LINE")=ONHIGHLINE
  ;"                  INFO("DRAW MAIN","TEXT COLOR")=TEXTCOLOR
  ;"                  INFO("DRAW MAIN","SHOW INDEX")=SHOWIDX
  ;"                  INFO("DRAW MAIN","DATA INDEX")=IDX      
  ;"                  INFO("DRAW MAIN","COLUMN NUM")=COLNUM    <-- READ ONLY
  ;"                  INFO("DRAW MAIN","COLORS")=COLORS (merged array) <-- READ ONLY  
  ;"          OPTION("ON BEFORE FOOTERS")="FnName^Module" -- code to execute before Footers drawn.   E.g. DO FnName^Module(TMGPSCRLARR,.OPTION,.INFO)
  ;"          OPTION("ON NEED DATA")="FnName^Module" -- Optional.  Code to execute when data needed.  E.g. DO FnName^Module(TMGPSCRLARR,.OPTION,.INFO)
  ;"                  If defined, this will be called with data is needed for drawing a column.  This will give an alternative
  ;"                  way to provide data for drawing.  For example, if I want this scroller to be able to 
  ;"                  display 10,000 lines of data, it would be nice if all that data was not required to be loaded into OPTION
  ;"                  first.  If I was to display all the data in ^TIU(8925, for another example, this could be gigabytes
  ;"                  in size, and it is best to allow programatic fetching when needed -- without duplicating the data.
  ;"                  NOTE: This is the order used in data search:
  ;"                      1) Is ON NEED DATA handler defined?  
  ;"                         If defined, then call. If HANDLED returned 1, then use this data
  ;"                      2) Is data defined in @TMGPSCRLARR@?  If so, use this.
  ;"                      3) Is ON LOAD DATA handler defined?  
  ;"                         If defined, then call. If HANDLED returned 1, then use this data
  ;"                      4) If no data found, then draw blanks (null) data. 
  ;"                  If values in INFO shown below are modified, it will be used for data source.
  ;"                  INFO("DATA","SELECTED",<COL#>)=<Selected Index of line>  I.e. what line is selected in column 1, 2, 3, etc.  
  ;"                  INFO("DATA","SELECTED",<COL#>,"TEXT")=<Text of selected line>
  ;"                          NOTE: <COL#> will be 1..<CUR_COL>-1  I.e. columns to the LEFT of current column.  
  ;"                  INFO("DATA","CUR COLUMN")=<COL#> for which data is being needed
  ;"                  INFO("DATA","CUR COLUMN","START INDEX")=<LINE#> for beginning of lines of data which is needed
  ;"                  INFO("DATA","CUR COLUMN","END INDEX")=<LINE#> for end of lines of data which is needed
  ;"                  INFO("DATA","HANDLED")=1 if event handler has returned data.  If 0 then data will be obtained from legacy methods
  ;"                  INFO("DATA","TEXT",<LINE#>)=<DISPLAY TEXT>  <--- event handler returns data for display here.    ;"
  ;"                  INFO("DATA","TEXT",<LINE#>,"RETURN")=<A RETURN VALUE>  <--- event handler OPTIONALLY returns data here.    
  ;"          OPTION("ON LOAD DATA")="FnName^Module" -- Optional.  Code to execute ONCE when data needed.  E.g. DO FnName^Module(TMGPSCRLARR,.OPTION,.INFO)
  ;"                  If defined, this will be called with data is needed for drawing a column, BUT ONLY ONCE.
  ;"                  This will only be called if data is not found in @TMGPSCRLARR@ for a given column
  ;"                  This will give an alternative way to provide data for drawing if/when needed.
  ;"                  For example, if I want this scroller to be able to browse through a global tree, but the
  ;"                  user only goes down certain paths, then the data could be loaded only if needed.  
  ;"                  NOTE: This is the order used in data search:
  ;"                      1) Is ON NEED DATA handler defined?  
  ;"                         If defined, then call. If HANDLED returned 1, then use this data
  ;"                      2) Is data defined in @TMGPSCRLARR@?  If so, use this.
  ;"                      3) Is ON LOAD DATA handler defined?  
  ;"                         If defined, then call. If HANDLED returned 1, then use this data
  ;"                      4) If no data found, then draw blanks (null) data. 
  ;"                  INFO array is provided as shown below.  Use array to pass back data.
  ;"                  If values in INFO shown below are modified, it will be used for data source.
  ;"                  INFO("DATA","SELECTED",<COL#>)=<Selected Index of line>  I.e. what line is selected in column 1, 2, 3, etc.  
  ;"                  INFO("DATA","SELECTED",<COL#>,"TEXT")=<Text of selected line>
  ;"                          NOTE: <COL#> will be 1..<CUR_COL>-1  I.e. columns to the LEFT of current column.  
  ;"                  INFO("DATA","CUR COLUMN")=<COL#> for which data is being needed
  ;"                  INFO("DATA","CUR COLUMN","START INDEX")=<LINE#> for beginning of lines of data whicg is needed
  ;"                  INFO("DATA","CUR COLUMN","END INDEX")=<LINE#> for end of lines of data which is needed
  ;"                  INFO("DATA","HANDLED")=1 if event handler has returned data.  If 0 then data will be obtained from legacy methods
  ;"                  INFO("DATA","TEXT",<LINE#>)=<DISPLAY TEXT>  <--- event handler returns data for display here.    ;"                   
  ;"          ------------------
  ;"          NOTES about events.  Functions will be called as follows:
  ;"              DO FnName^Module(TMGPSCRLARR,.OPTION,.INFO)
  ;"                TMGPSCRLARR and OPTION are the same data received by this function
  ;"                  -- thus OPTION can be used to carry other custom information.
  ;"                INFO has extra info as outlined above.
  ;"              Functions may set a globally-scoped var named TMGSCLRMSG to communicate back
  ;"                      TMGSCLRMSG="^" --> Scroller will exit
  ;"                      TMGSCLRMSG="FULL" --> Scroller will do full screen redraw
  ;"              Functions may set OPTION("HIGHLINE")=# or OPTION("COLUMNS",<COL#>,"HIGHLINE")=# to tell 
  ;"                      the scroller to set the highlight line to #.  This will not trigger further events
  ;"              Functions may set INFO("DO FUNCTION")=<A FN> or 
  ;"                                INFO("DO FUNCTION",<seq#>)=<A FN> to call internal function.
  ;"                  Data for use by the DO FUNCTION may be put by user into 
  ;"                    INFO("DO FUNCTION","DATA")=<array of data> for use by  INFO("DO FUNCTION")=<A FN>
  ;"                    INFO("DO FUNCTION",<seq#>,"DATA")=<array of data> for use by INFO("DO FUNCTION",<seq#>)=<A FN>
  ;"                  Allowed internal functions to call include:
  ;"                    "ADD COLUMN(<AUTOSETCOLWIDTHS>)" <-- <AUTOSETCOLWIDTHS> OPTIONAL.  0 or 1. If 1 then all columns resized.
  ;"                    "SCROLL VIEWPORT(<#>)"  <-- <#> is number of chars to scroll, or 'HOME'.  #<0 is LEFT, #>0 is RIGHT, 'HOME' is to 1st column, 'END' to far right
  ;"                    "DROP COLUMN" <-- drop right-most column
  ;"                    "GROW COLUMN(<COL_NUMBER>,<AMOUNT>)"   <-- make column wider by AMOUNT.     Default for COL_NUMBER is the Active Col.  Default for AMOUNT is 1 
  ;"                    "SHRINK COLUMN(<COL_NUMBER>,<AMOUNT>)" <-- make column narrower by AMOUNT.  Default for COL_NUMBER is the Active Col.  Default for AMOUNT is 1
  ;"Result: none
  ;                                
  NEW SCRNLINE,SPACELINE            ;"To hold string for "   " and "---" lines, matching width of display area. 
  NEW CURY                          ;"Screen Y position of first line of main area
  NEW VIEWSTATE                     ;"Array to hold various parameters for display.  HOLDS:
  ;"                                  'HIGHLINE'          -- Array, for data index of highlighted line in main area. HIGHLINE(<col#>)=<data index>, HIGHLINE(<col#>,"TEXT")=<text of line>,HIGHLINE(<col#>,"TEXT","RETURN")=<data for return text> 
  ;"                                  'TOPLINE'           -- Array, for starting data index of first line in main area. TOPLINE(<col#>)=<starting data index>
  ;"                                  'XOFFSET'           -- Array, for holding X offset, which allows left-right scrolling.  
  ;"                                  'ACTIVECOL'         -- Which column is active, meaning hold user cursor
  ;"                                  'NUMIDXDIGITS'      -- Array, for max number of digits if showing index numbers, per column.  NUMIDXDIGITS(<col#>)=#
  ;"                                  'ENTRYCT'           -- Array, for holding number of data elements for column. ENTRYCT(<col#>)=<number of data elements>
  ;"                                  'WIDTH'             -- Array, for widths of each column: WIDTH(<col#>)=#
  ;"                                  'COLS'              -- Number of columns to display
  ;"                                  'VIEWPORT XOFFSET'  -- X offset of viewport.  If were same as WIDTH(1), then drawing would begin at column 2.  
  ;"                                  'MAINLINES'         -- Number of screen lines in main area.
  ;"                                  'HEADER'            -- Array to hold header table
  ;"                                  'HEADER','SOURCE'   -- copy of OPTION('HEADER') to compare against master OPTION, to see if table should be refreshed
  ;"                                  'HEADER','TABLE'    -- array of header table.  
  ;"                                  'FOOTER'            -- Array to hold header table
  ;"                                  'FOOTER','SOURCE'   -- copy of OPTION('HEADER') to compare against master OPTION, to see if table should be refreshed
  ;"                                  'HEADER','TABLE'    -- array of footer table.  
  ;"                                  'VIEWPORTX'         -- X offset of left of viewport.  If X > 0, this is the number of characters that are skipped over in columns before starting to draw .
  NEW INFO                          ;"Array, for holding information to pass to event handlers     
  NEW BUILDCMD SET BUILDCMD=""      ;"Var to hold command as user builds it up through typing letters
  NEW TMGSCLRMSG SET TMGSCLRMSG=""  ;"Special variable available to event handlers to send back message by setting value
  NEW LASTSCRNW SET LASTSCRNW=-1    ;"Var to track if screen size has changed. 
  ;
  DO SETDEFCOLORS(.OPTION)
  NEW SCRNH DO CHECKSCRNHEIGHT(.SCRNH,.OPTION) 
  NEW SCRNW DO CHECKSCRNWIDTH(.SCRNW,.OPTION) 
  NEW DONE SET DONE=0
  NEW DRAWMODE SET DRAWMODE="FULL"
  SET VIEWSTATE("VIEWPORTX")=0
  DO AUTOSETCOLWIDTHS(.VIEWSTATE,SCRNW,.OPTION,0)  
  IF $GET(OPTION("WRAP DATA"))>0 DO WRAPDATA(TMGPSCRLARR,.OPTION,SCRNW)  ;"One time wrap if requested.  
  ;
  FOR  DO  QUIT:DONE  ;"MAIN LOOP
  . IF "FULL"[DRAWMODE DO INITVIEWSTATE(.VIEWSTATE,.OPTION) 
  . IF "FULL,NORMAL"[DRAWMODE DO
  . . DO CSRSHOW^TMGTERM(0)  ;"HIDE CURSOR
  . . IF $$CHECKSCRNWIDTH(.SCRNW,.OPTION) DO         ;"Check each cycle, so window can be resized between full redraws 
  . . . ;"DO AUTOSETCOLWIDTHS(.VIEWSTATE,SCRNW,.OPTION)    ;"Set up column widths.    
  . . DO ADJUSTVS4CSR(.VIEWSTATE,.OPTION)             ;"Adjust view so that highline will be on screen 
  . . ;  
  . . DO DRAWHEADER(.VIEWSTATE,.OPTION,.SCRNW,.CURY) ;"DRAW HEADER AREA
  . . DO DRAWMAIN(.VIEWSTATE,.OPTION,.CURY)          ;"DRAW MAIN AREA
  . . DO DRAWFOOTER(.VIEWSTATE,.OPTION,.SCRNW,.CURY) ;"DRAW FOOTER AREA
  . . ;
  . . DO DRAWCMDAREA(.CURY)
  . . DO CSRSHOW^TMGTERM(1)  ;"SHOW CURSOR
  . . ;
  . SET TMGSCLRMSG=""  ;"Initialize message variable that can be used by event handlers to send back message. 
  . NEW TEMP SET TEMP=$$USER(.OPTION)  ;"interact with users, trigger events etc.
  . DO CLRCMDAREA(CURY) 
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
  ;"PROCESS FLOW for cursors.  
  ;"  NOTE: I think this is a result of multiple generations of code.  ON CURSOR and ON CHANGING seem redundant
  ;"  -- If input is a cursor, and ON CURSOR event defined, call event.  
  ;"     -- afterwards, fire event ON CURSOR DONE
  ;"     -- If ON CURSOR esor, and ON CHANGING event defined, and cursor is UP/DN, LF/RT, call event
  ;"     -- afterwards, fire event ON CURSOR DONE
  ;"     -- if ON CHANGING event had returned 'ALLOW CHANGE'=false, then quit
  ;"  -- If code gets to this point, then call ON KEYPRESS
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
  . SET DONE=$$EVENTCURSOR(ESCKEY,"ON CURSOR")  ;"trigger on cursor event
  . IF DONE DO
  . . SET NEEDREFRESH=2
  . . IF $$EVENTCURSOR(ESCKEY,"ON CURSOR DONE")  ;"trigger on cursor done event.  Ignore result
  IF ESCKEY?1"PF".N SET ESCKEY=$EXTRACT(ESCKEY,2,$LENGTH(ESCKEY)) ;"CONVERT PF# --> F#  ;"//kt 11/24
  NEW FNCYCLE SET FNCYCLE=$GET(OPTION("COLUMNS","FN TOGGLE NUM")) 
  IF FNCYCLE'="",ESCKEY=FNCYCLE DO  GOTO USI2
  . DO HNDLCOLCYCLE(.VIEWSTATE,.OPTION)
  . SET RESULT=2
  IF ESCKEY="UP"   SET INPUT="CURSOR^UP^1"
  IF ESCKEY="PREV" SET INPUT="CURSOR^UP^15"
  IF ESCKEY="DOWN" SET INPUT="CURSOR^DOWN^1"
  IF ESCKEY="NEXT" SET INPUT="CURSOR^DOWN^15"
  IF ESCKEY="LEFT" SET INPUT="CURSOR^LEFT^1"
  IF ESCKEY="RIGHT" SET INPUT="CURSOR^RIGHT^1"
  IF ESCKEY="FIND" SET INPUT="CURSOR^LEFT^*"   ;"For some reason, HOME key generates "FIND"     ;11/21/24 changed so keyboard returns HOME, not FIND now
  IF ESCKEY="HOME" SET INPUT="CURSOR^LEFT^*"   
  IF ESCKEY="SELECT" SET INPUT="CURSOR^RIGHT^*"   ;"For some reason, END key generates "SELECT" ;11/21/24 changed so keyboard returns END, not SELECT now
  IF ESCKEY="END" SET INPUT="CURSOR^RIGHT^*"   
  IF ESCKEY="BACKSPC",$LENGTH(BUILDCMD)>0 DO
  . SET BUILDCMD=$EXTRACT(BUILDCMD,1,$LENGTH(BUILDCMD)-1)
  IF ESCKEY="CR" DO  GOTO USI2
  . DO HNDLCR(INPUT,BUILDCMD)
  IF (ESCKEY="INSERT")!(INPUT="+")!((INPUT=" ")&(BUILDCMD="")),$GET(OPTION("MULTISEL"))=1 DO  GOTO USI2  ;"toggle selection.
  . DO HNDLSEL ;"Handle user SELECTION input
  IF (ESCKEY="CTRL-A"),$GET(OPTION("MULTISEL"))=1 DO  GOTO USI2  ;"selection all TOGGLE.
  . DO HNDLSELALL ;"Handle user Select ALL
  IF INPUT="^" SET RESULT="^" GOTO USI3
  IF (INPUT["CURSOR^") DO  GOTO USI2:HANDLED
  . SET HANDLED=$$HNDLCURSOR(INPUT,.VIEWSTATE,.OPTION)
  . IF $$EVENTCURSOR(ESCKEY,"ON CURSOR DONE")  ;"trigger on cursor done event.  Ignore result
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
USI2  ;    
  ;"After above events, see if event handler code requested scroller to select a particular line.
  ;"//kt moved 1 line below from above label JSI2 to below 11/24
  IF $$OPT2VIEWSTATE(.OPTION,.VIEWSTATE)>0 SET NEEDREFRESH=2    ;"Transfer viewstate from OPTION to VIEWSTATE 
  ;
  IF NEEDREFRESH=2 SET RESULT=2
  IF NEEDREFRESH=1 SET RESULT=1
USI3 ;  
  QUIT RESULT
  ;   
EVENTDRAWML(INFO,TEXT,SELECTED,ONHIGHLINE,TEXTCOLOR,SHOWIDX,IDX,COLNUM,COLORS) ;
  ;"Give user opportunity to modify line before drawing
  IF $$EVENTDEF(.OPTION,"ON DRAW MAIN LINE")=0 QUIT
  SET INFO("DRAW MAIN","TEXT")=TEXT
  SET INFO("DRAW MAIN","SELECTED")=SELECTED
  SET INFO("DRAW MAIN","ON HIGHLIGHT LINE")=ONHIGHLINE
  SET INFO("DRAW MAIN","TEXT COLOR")=TEXTCOLOR
  SET INFO("DRAW MAIN","SHOW INDEX")=SHOWIDX
  SET INFO("DRAW MAIN","DATA INDEX")=IDX
  SET INFO("DRAW MAIN","COLUMN NUM")=COLNUM    ;"<-- READ ONLY
  MERGE INFO("DRAW MAIN","COLORS")=COLORS      ;"<-- READ ONLY
  DO EVENT(.OPTION,"ON DRAW MAIN LINE")
  SET TEXT=$GET(INFO("DRAW MAIN","TEXT"))
  SET SELECTED=$GET(INFO("DRAW MAIN","SELECTED"))
  SET ONHIGHLINE=$GET(INFO("DRAW MAIN","ON HIGHLIGHT LINE"))
  SET TEXTCOLOR=$GET(INFO("DRAW MAIN","TEXT COLOR"))
  SET SHOWIDX=$GET(INFO("DRAW MAIN","SHOW INDEX"))  
  SET IDX=+$GET(INFO("DRAW MAIN","DATA INDEX"))
  QUIT
  ;
EVENTCURSOR(ESCKEY,EVENT)  ;"trigger on cursor event
  ;"Note: uses INFO, OPTION vars in global scope, defined in SCROLLER scope
  ;"Result: 1 if event handled cursor, 0 otherwise.  
  SET INFO("CURSOR")=ESCKEY
  SET INFO("CURSOR","HANDLED")=0
  DO EVENT(.OPTION,EVENT)
  NEW RESULT SET RESULT=(+$GET(INFO("CURSOR","HANDLED"))=1)
  QUIT RESULT
  ;
HNDLCURSOR(INPUT,VIEWSTATE,OPTION) ;"Handle user cursor input.  
  ;"Note: uses vars in global scope, defined in SCROLLER scope:
  ;"       INFO
  ;"Result: 1 if cursor was handled here, or 0 if not. 
  NEW RESULT SET RESULT=0
  NEW CSRDIR SET CSRDIR=$PIECE(INPUT,"^",2)
  NEW COUNT SET COUNT=$PIECE(INPUT,"^",3)
  NEW COL SET COL=+$GET(VIEWSTATE("ACTIVECOL"))
  NEW DATAREF SET DATAREF=$$DATAREF(COL)
  IF ("UP,DOWN,LEFT,RIGHT,"[CSRDIR)=0 GOTO HNDCSRDN
  IF COUNT="*","LEFT"[CSRDIR SET COUNT=9999999
  IF COUNT="*","RIGHT"[CSRDIR DO
  . NEW TEMPVS MERGE TEMPVS=VIEWSTATE SET TEMPVS("XOFFSET",COL)=0
  . NEW HIGHIDX SET HIGHIDX=VIEWSTATE("HIGHLINE",COL)
  . NEW CURTEXT SET CURTEXT=$$GETDATATEXT(COL,HIGHIDX,.TEMPVS)
  . NEW LEN SET LEN=$LENGTH(CURTEXT)  ;"e.g. 875 chars of text
  . NEW COLW SET COLW=VIEWSTATE("WIDTH",COL)      ;"e.g. 100 chars wide for column
  . NEW TARGETXO SET TARGETXO=LEN-COLW ;"e.g. char position 775 should be XOffset
  . SET TARGETXO=TARGETXO+1 ;"offset 1 to right, to give a blank space at end of line. 
  . IF TARGETXO<0 SET TARGETXO=0
  . NEW CURXO SET CURXO=VIEWSTATE("XOFFSET",COL)
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
  . NEW TEMPVS MERGE TEMPVS=VIEWSTATE 
  . SET TEMPVS("HIGHLINE",COL)=TEMPVS("HIGHLINE",COL)+DELTA  ;"HL will be planned next highlight line
  . KILL TEMPVS("HIGHLINE",COL,"TEXT"),TEMPVS("HIGHLINE",COL,"RETURN")
  . DO ADJUSTVS4CSR(.TEMPVS,.OPTION)
  . DO PREPINFO(.INFO,"NEXT LINE",COL,TEMPVS("HIGHLINE",COL))  
  . DO EVENT(.OPTION,"ON CHANGING") IF $GET(INFO("ALLOW CHANGE"))'>0 QUIT  ;"INFO is changed in global scope
  . KILL INFO("NEXT LINE")
  . KILL VIEWSTATE MERGE VIEWSTATE=TEMPVS
  . DO VIEWSTATE2OPT(.VIEWSTATE,.OPTION)  ;"//kt 11/24
  . SET RESULT=1
  IF "LEFT,RIGHT"[CSRDIR DO
  . DO PREPINFO(.INFO,"NEXT LINE",COL,VIEWSTATE("HIGHLINE",COL))  
  . DO EVENT(.OPTION,"ON CHANGING") IF $GET(INFO("ALLOW CHANGE"))'>0 QUIT   ;"//kt 11/24 changed "ALLOW CHANGE" --> "ON CHANGING"
  . IF $GET(OPTION("LR SCROLLING"))'=1 QUIT
  . SET VIEWSTATE("XOFFSET",COL)=$$INBOUNDS^TMGMISC(VIEWSTATE("XOFFSET",COL)+DELTA,0,500)  ;"May need to find better way than hardcoded 500 later....
  . SET RESULT=1
HNDCSRDN ;  
  QUIT RESULT
  ;
HNDLCOLCYCLE(VIEWSTATE,OPTION) ;"Hand user input of function key to cycle column
  NEW ACTIVECOL SET ACTIVECOL=VIEWSTATE("ACTIVECOL")
  SET ACTIVECOL=ACTIVECOL+1
  NEW COLS SET COLS=$GET(OPTION("COLUMNS","NUM"),1)
  IF ACTIVECOL>COLS SET ACTIVECOL=1 
  SET OPTION("COLUMNS","ACTIVE")=ACTIVECOL
  SET VIEWSTATE("ACTIVECOL")=ACTIVECOL
  QUIT
  ;
HNDLCR(INPUT,BUILDCMD) ;"Handle CR user input     
  ;"Note: uses vars in global scope, defined in SCROLLER scope
  NEW EVENT,INFO
  IF BUILDCMD'="" SET EVENT="ON CMD",INFO("USER INPUT")=BUILDCMD
  ELSE  IF INPUT'="" SET EVENT="ON CMD",INFO("USER INPUT")=INPUT
  ELSE  SET EVENT="ON SELECT"
  DO EVENT(.OPTION,EVENT)
  SET NEEDREFRESH=2
  SET BUILDCMD=""
  KILL INFO("USER INPUT")
  QUIT
  ;     
HNDLSEL ;"Handle user SELECTION input
  ;"Note: uses vars in global scope, defined in SCROLLER scope
  NEW ACTIVECOL SET ACTIVECOL=VIEWSTATE("ACTIVECOL")
  NEW HIGHIDX SET HIGHIDX=VIEWSTATE("HIGHLINE",ACTIVECOL)
  ;"!!TO DO -- check if this is multiple columns aware....
  SET @TMGPSCRLARR@("SELECTED",HIGHIDX)='$GET(@TMGPSCRLARR@("SELECTED",HIGHIDX))
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
DRAWHEADER(VIEWSTATE,OPTION,SCRNW,CURY) ; 
  ;"NOTE: Uses vars in global scope, defined in SCROLLER scope: 
  ;"          SCRNLINE,VIEWSTATE
  ;"      SCRNW
  ;"      CURY -- PASS BY REFERENCE, AN OUT PARAMETER
  SET CURY=$GET(OPTION("SCRN TOP OFFSET"),1)
  DO CUP^TMGTERM(1,CURY)
  NEW NUMCOLS SET NUMCOLS=+$GET(OPTION("COLUMNS","NUM"))
  NEW TABLE MERGE TABLE=VIEWSTATE("HEADER","TABLE")
  NEW TABLEOPT MERGE TABLEOPT=VIEWSTATE("HEADER","TABLE OPTIONS")
  IF $DATA(TABLE) DO
  . NEW TABLEHT SET TABLEHT=$$DRAWTABLE^TMGUSRI9(1,CURY,SCRNW,.TABLE,.TABLEOPT)
  . SET CURY=CURY+TABLEHT
  ELSE  DO
  . DO DRAWSCRNLINE(1,CURY,"TOP LINE",.OPTION)
  . SET CURY=CURY+1
  QUIT
  ;
DRAWMAIN(VIEWSTATE,OPTION,CURY) ;
  ;"NOTE: Uses vars in global scope, defined in SCROLLER scope:  INFO
  NEW SHOWIDX,XPOS,YPOS
  NEW COLNUM FOR COLNUM=1:1:VIEWSTATE("COLS") DO
  . IF $$COLVISIBLE(COLNUM,.VIEWSTATE)=0 QUIT
  . SET XPOS=$$COLLEFT(.VIEWSTATE,COLNUM)
  . NEW DATAREF SET DATAREF=$$DATAREF(COLNUM)
  . SET SHOWIDX=+$GET(@DATAREF@("SHOW INDEX"))
  . NEW COLORS DO SETCOLORS(.COLORS,COLNUM,.OPTION)
  . DO SETCOLOR("NORM",.COLORS)
  . NEW STARTIDX SET STARTIDX=VIEWSTATE("TOPLINE",COLNUM)
  . NEW ENDIDX SET ENDIDX=STARTIDX+VIEWSTATE("MAINLINES")-1
  . NEW COLDATA DO GETCOLTEXTDATA(.COLDATA,COLNUM,STARTIDX,ENDIDX,.OPTION,.VIEWSTATE)
  . NEW COLWIDTH SET COLWIDTH=$GET(VIEWSTATE("WIDTH",COLNUM),10)
  . NEW LINECT SET LINECT=0
  . NEW IDX FOR IDX=STARTIDX:1:ENDIDX DO 
  . . SET YPOS=CURY+LINECT,LINECT=LINECT+1
  . . NEW TEXT,RETURN SET TEXT=$GET(COLDATA(IDX)),RETURN=$GET(COLDATA(IDX,"RETURN"))
  . . NEW SELECTED SET SELECTED=$SELECT(COLNUM=1:+$GET(@TMGPSCRLARR@("SELECTED",IDX)),1:0)
  . . NEW ONHIGHLINE SET ONHIGHLINE=(IDX=VIEWSTATE("HIGHLINE",COLNUM))
  . . IF ONHIGHLINE SET VIEWSTATE("HIGHLINE",COLNUM,"TEXT")=TEXT,VIEWSTATE("HIGHLINE",COLNUM,"TEXT","RETURN")=RETURN
  . . NEW SHOWHIGHLINE SET SHOWHIGHLINE=ONHIGHLINE&(COLNUM=VIEWSTATE("ACTIVECOL"))
  . . NEW TEXTCOLOR IF SELECTED SET TEXTCOLOR=$SELECT(SHOWHIGHLINE:"HI-SEL",1:"SELECTED")
  . . ELSE  SET TEXTCOLOR=$SELECT(SHOWHIGHLINE:"HIGH",1:"NORM")
  . . DO EVENTDRAWML(.INFO,.TEXT,.SELECTED,.SHOWHIGHLINE,.TEXTCOLOR,.SHOWIDX,.IDX,.COLNUM,.COLORS)
  . . NEW INDEXSTR SET INDEXSTR=""
  . . IF SHOWIDX SET INDEXSTR="{{INDEX}}"_$$RJ^XLFSTR(IDX,VIEWSTATE("NUMIDXDIGITS",COLNUM))_"."
  . . IF TEXTCOLOR'="" SET TEXTCOLOR="{{"_TEXTCOLOR_"}}"
  . . SET TEXT=INDEXSTR_TEXTCOLOR_TEXT
  . . NEW TEXTLEN SET TEXTLEN=$$NOCOLEN(TEXT)  ;"-$GET(VIEWSTATE("VIEWPORTX"))
  . . NEW TAILPAD SET TAILPAD=$EXTRACT(SPACELINE,1,(COLWIDTH-TEXTLEN))
  . . SET TEXT=TEXT_TAILPAD
  . . DO VPWRITE(XPOS,YPOS,.VIEWSTATE,TEXT,COLWIDTH,.COLORS) ;"WRITE (OPTIONALLY) COLORED TEXT, CLIPPED TO CURRENT VIEWPORT
  . . DO SETCOLOR("RESET",.COLORS) WRITE !
  SET CURY=YPOS+1
  QUIT
  ;
DRAWFOOTER(VIEWSTATE,OPTION,SCRNW,CURY) ;
  SET TMGSCLRMSG=""  ;"Initialize message variable that can be used by event handlers to send back message. 
  DO EVENT(.OPTION,"ON BEFORE FOOTERS")
  IF TMGSCLRMSG="FULL" DO INITVIEWSTATE(.VIEWSTATE,.OPTION) 
  NEW TABLE MERGE TABLE=VIEWSTATE("FOOTER","TABLE")
  NEW TABLEOPT MERGE TABLEOPT=VIEWSTATE("FOOTER","TABLE OPTIONS")
  IF $DATA(TABLE) DO
  . NEW TABLEHT SET TABLEHT=$$DRAWTABLE^TMGUSRI9(1,CURY,SCRNW,.TABLE,.TABLEOPT)
  . SET CURY=CURY+TABLEHT
  ELSE  DO
  . DO DRAWSCRNLINE(1,CURY,"BOTTOM LINE",.OPTION)
  . SET CURY=CURY+1
  QUIT
  ;
DRAWSCRNLINE(LEFT,TOP,COLORMODE,OPTION) ;
  ;"NOTE: Uses vars in global scope, defined in SCROLLER scope:
  ;"       SCRNLINE
  NEW FG,BG
  DO SETCOLOR(COLORMODE,.OPTION,.FG,.BG)
  IF $GET(OPTION("UNICODE LINES"))=1 DO
  . NEW LINEOPT MERGE LINEOPT=OPTION("UNICODE LINES","OPTION")
  . DO DRAWHLINE^TMGTERM2(LEFT,TOP,$LENGTH(SCRNLINE),FG,BG,.LINEOPT)
  . WRITE !
  ELSE  DO
  . WRITE SCRNLINE,!
  QUIT
  ;
DRAWCMDAREA(CURY)  ;
  DO CLRCMDAREA(CURY) ;
  DO CUP^TMGTERM(1,CURY)
  WRITE ": "_BUILDCMD
  QUIT
  ;
CLRCMDAREA(CURY) ;
  DO SETCOLOR("RESET")
  NEW TEMP SET TEMP=$$LJ^XLFSTR("  ",SCRNW)
  DO CUP^TMGTERM(1,CURY)
  WRITE TEMP
  QUIT
  ;
COLVISIBLE(COLNUM,VIEWSTATE) ;"Is specified column visible (even if partially) in the viewport?
  ;"  |    1    |    2     |    3      |    4     |     5     |     6    |
  ;"  |      +============Viewport Window==============+      |          |
  ;"  |      :  |          |           |          |    :      |          |
  ;"  |123456:  |          |           |          |    :      |          |
  ;"  |      :  |          |           |          |    :      |          |
  ;"  |      :  |          |           |          |    :      |          |
  ;"  |      +=========================================+      |          |
  ;"  |         |          |           |          |           |          |
  ;"   0000000001111111111222222222233333333334444444444555555555566666666667
  ;"   1234567890123456789012345678901234567890123456789012345678901234567890
  ;"  E.g. SCRNW = 49 - 7 = 42
  ;"       VIEWPORTX = 7  --> VPXL=7, VPXR=49
  ;"       $$COLLEFT(N)= 1, 10,  21,  33,  44,  56,
  ;"       $$COLRIGHT(N)=  9,  20,  32,  43,  55,  67
  ;"Three tests that show visibility:         COL#   1     2     3     4    5   6
  ;" 1) is VPXL between COLLF and COLLRT             x
  ;" 2) is (VPXL < COLLF) and (VPXR > COLRT)               x     x     x 
  ;" 3) is XPXR between COLLF and COLRT                                     x
  ;
  ;"RESULT:  0 if column is not visible in viewport
  ;"         1 if column is partly visible at left side of viewport
  ;"         2 if column is fully visible in viewport
  ;"         3 if column is partly visible at right side of viewport.  
  NEW VPXL SET VPXL=$GET(VIEWSTATE("VIEWPORTX"))
  NEW VPXR SET VPXR=VPXL+SCRNW-1
  NEW COLLF SET COLLF=$$COLLEFT(.VIEWSTATE,COLNUM)
  NEW COLRT SET COLRT=$$COLRIGHT(.VIEWSTATE,COLNUM)
  NEW RESULT SET RESULT=0  ;"DEFAULT
  IF ((VPXL>=COLLF)&(VPXL<=COLRT)) SET RESULT=1 GOTO CVDN     ;"test #1
  IF ((VPXL<=COLLF)&(VPXR>=COLRT)) SET RESULT=2 GOTO CVDN     ;"test #2
  IF ((VPXR>=COLLF)&(VPXR<=COLRT)) SET RESULT=3 GOtO CVDN     ;"test #3
CVDN ;  
  QUIT RESULT
  ;
GETCOLTEXTDATA(OUT,COLNUM,STARTIDX,ENDIDX,OPTION,VIEWSTATE) ;"Return an array with all lines to be shown for column. 
  ;"INPUT:  OUT -- PASS BY REFERENCE.  Format:  OUT(<IDX>)=<line of text>
  ;"                                            OUT(<IDX>,"RETURN")=<data> <-- if any.  
  ;"        COLNUM -- the column to return data for.  
  ;"        STARTIDX -- This is line to start with.  For example, if column is scrolled such that
  ;"                    the 1st line shown is the 5th data entry, then STARTIDX=5
  ;"        ENDIDX -- This is the index of the last line to be shown.
  ;"        OPTION -- PASS BY REFERENCE.  Standard array holding event handlers etc.  
  ;"        VIEWSTATE -- PASS BY REFERENCE.  Array holding current view state.  
  ;"GLOBAL VARS USED: TMGPSCRLARR
  ;"Result: none.
  NEW INFO,HANDLED SET HANDLED=0
  SET INFO("DATA","HANDLED")=0
  MERGE INFO("DATA","SELECTED")=VIEWSTATE("HIGHLINE")
  SET INFO("DATA","CUR COLUMN")=COLNUM
  SET INFO("DATA","CUR COLUMN","START INDEX")=STARTIDX
  SET INFO("DATA","CUR COLUMN","END INDEX")=ENDIDX
  NEW DATAREF SET DATAREF=$$DATAREF(COLNUM)    
  NEW ACOL FOR ACOL=COLNUM:1:VIEWSTATE("COLS") KILL INFO("DATA","SELECTED",ACOL)  ;"only give data to LEFT of COLNUM
  ;"-- First, use ON NEED DATA if provided --
  ;"   This will overwrite any prior data stored in @TMGPSCLRARR
  IF $DATA(OPTION("ON NEED DATA")) DO  GOTO:HANDLED GCTDL1  ;"temp storage into @TMGPSCLRARR
  . DO EVENT(.OPTION,"ON NEED DATA")  ;"uses INFO in global scope
  . IF $GET(INFO("DATA","HANDLED"))'=1 QUIT
  . MERGE OUT=INFO("DATA","TEXT")
  . IF INFO("DATA","HANDLED")=1 DO
  . . KILL @DATAREF   ;"If browsing very large global array, keep clearing out local storage before reloading.   
  . . SET HANDLED=1
  ;"-- Next, use data from @TMGPSCRLARR if possible.  
  NEW FOUNDDATA SET FOUNDDATA=0
  NEW IDX FOR IDX=STARTIDX:1:ENDIDX DO
  . NEW TEXT,RTN
  . IF COLNUM=1 DO
  . . SET TEXT=$ORDER(@DATAREF@(IDX,""))
  . . SET RTN=$GET(@DATAREF@(IDX,TEXT))
  . ELSE  DO
  . . SET TEXT=$GET(@DATAREF@(IDX))      
  . . SET RTN=$GET(@DATAREF@(IDX,"RETURN"))
  . IF TEXT'="" SET FOUNDDATA=1
  . SET OUT(IDX)=TEXT
  . IF RTN'="" SET OUT(IDX,"RETURN")=RTN
  IF FOUNDDATA GOTO GCTDDN
  ;"-- Next use data from ON LOAD DATA if provided.  
  IF $DATA(OPTION("ON LOAD DATA")) DO
  . DO EVENT(.OPTION,"ON LOAD DATA")  ;"uses INFO in global scope
  . IF $GET(INFO("DATA","HANDLED"))'=1 QUIT
  . MERGE OUT=INFO("DATA","TEXT")
GCTDL1 ;  
  ;"STORE DATA INFO @DATAREF
  SET IDX=""
  FOR  SET IDX=$ORDER(OUT(IDX)) QUIT:IDX'>0  DO
  . NEW TEXT SET TEXT=OUT(IDX)
  . NEW RTN SET RTN=$GET(OUT(IDX,"RETURN"))
  . IF COLNUM=1 DO  
  . . SET @DATAREF@(IDX,TEXT)=RTN
  . ELSE  DO
  . . SET @DATAREF@(IDX)=TEXT
  . . IF RTN]"" SET @DATAREF@(IDX,"RETURN")=RTN
  DO ENSUREVSVARS(.VIEWSTATE)  ;"This will update ENTRYCT
  IF INFO("DATA","HANDLED")=1 GOTO GCTDDN
  ;" -- no data found, so return OUT, filled with "" above.    
GCTDDN ;  
  SET IDX=""
  FOR  SET IDX=$ORDER(OUT(IDX)) QUIT:IDX'>0  DO
  . NEW TEXT SET TEXT=$GET(OUT(IDX))
  . IF TEXT[$CHAR(9) SET TEXT=$$REPLSTR^TMGSTUT3(TEXT,$CHAR(9),"  ")
  . IF $GET(VIEWSTATE("XOFFSET",COLNUM))>0 DO
  . . NEW OPTION SET OPTION("KEEP TAGS")=1  
  . . SET TEXT=$$MKSTRMID^TMGSTUT3(.TEXT,VIEWSTATE("XOFFSET",COLNUM)+1,$LENGTH(TEXT),"{{","}}",.OPTION)
  . SET OUT(IDX)=TEXT
  QUIT
  ;
GETDATATEXT(COLNUM,IDX,VIEWSTATE)  ;
  NEW TEXT
  IF COLNUM=1 DO
  . SET TEXT=$ORDER(@TMGPSCRLARR@(IDX,""))
  ELSE  DO
  . NEW DATAREF SET DATAREF=$$DATAREF(COLNUM)    
  . SET TEXT=$GET(@DATAREF@(IDX))      
  IF TEXT[$CHAR(9) SET TEXT=$$REPLSTR^TMGSTUT3(TEXT,$CHAR(9),"  ")
  IF $GET(VIEWSTATE("XOFFSET",COLNUM))>0 DO
  . NEW OPTION SET OPTION("KEEP TAGS")=1  
  . SET TEXT=$$MKSTRMID^TMGSTUT3(.TEXT,VIEWSTATE("XOFFSET",COLNUM)+1,$LENGTH(TEXT),"{{","}}",.OPTION)
  QUIT TEXT        
  ;
INITVIEWSTATE(VIEWSTATE,OPTION) ;
  ;"NOTE: Uses vars in global scope, defined in SCROLLER scope: F1CYCLE,SCRNW
  SET OPTION("COLUMNS","NUM")=$GET(OPTION("COLUMNS","NUM"),1)
  ;"DO AUTOSETCOLWIDTHS(.VIEWSTATE,SCRNW,.OPTION,0)
  DO OPT2VIEWSTATE(.OPTION,.VIEWSTATE)  ;"Transfer from OPTION to VIEWSTATE
  NEW SIZEHDR SET SIZEHDR=+$GET(VIEWSTATE("HEADER","SIZE"))
  NEW SIZEFTR SET SIZEFTR=+$GET(VIEWSTATE("FOOTER","SIZE"))
  SET VIEWSTATE("MAINLINES")=SCRNH-SIZEHDR-SIZEFTR   ;"This is number of lines do draw for Main part.  
  SET F1CYCLE=($GET(OPTION("COLUMNS","F1 TOGGLE NUM"),1))&($GET(OPTION("COLUMNS","NUM"))>1)
  NEW LSTCTOPT SET LSTCTOPT("NUMERIC")=1
  DO ENSUREVSVARS(.VIEWSTATE)
  DO VIEWSTATE2OPT(.VIEWSTATE,.OPTION)  ;"//kt 11/24.  Ensure both are in sync.  
  QUIT
  ;  
ENSUREVSVARS(VIEWSTATE) ;
  SET VIEWSTATE("VIEWPORT XOFFSET")=+$GET(VIEWSTATE("VIEWPORT XOFFSET"))  ;"default to 0    
  NEW COLNUM FOR COLNUM=1:1:OPTION("COLUMNS","NUM") DO
  . SET VIEWSTATE("HIGHLINE",COLNUM)=+$GET(VIEWSTATE("HIGHLINE",COLNUM),5)  ;"ensure defined
  . SET VIEWSTATE("TOPLINE",COLNUM)=+$GET(VIEWSTATE("TOPLINE",COLNUM),1)    ;"ensure defined
  . SET VIEWSTATE("XOFFSET",COLNUM)=+$GET(VIEWSTATE("XOFFSET",COLNUM))      ;"ensure defined
  . SET VIEWSTATE("ENTRYCT",COLNUM)=$$LISTCT^TMGMISC2($$DATAREF(COLNUM),.LSTCTOPT)
  . SET VIEWSTATE("NUMIDXDIGITS",COLNUM)=$LENGTH(VIEWSTATE("ENTRYCT",COLNUM)) 
  . IF VIEWSTATE("NUMIDXDIGITS",COLNUM)=0 SET VIEWSTATE("NUMIDXDIGITS",COLNUM)=1
  QUIT
  ;
INITHFTABLE(ATABLE,VIEWSTATE,OPTION)  ;"Initialize header or footer table
  ;"INPUT:  ATABLE -- should be 'HEADER' or 'FOOTER
  ;"NOTE: Header and Footer we set up with different formats:
  ;"      OPTION("HEADER",1)=Header line TEXT
  ;"      OPTION("HEADER",2)=More Header line TEXT (any number of lines)
  ;"      OPTION("HEADER","COL",<COL#>)=Header for specified column #.  IGNORED unless OPTION("COLUMNS","NUM") > 1
  ;"                  NOTE: Any entry > then number of columns is ignored.
  ;"
  ;"      OPTION("FOOTER",1)=Footer line TEXT  <--- OPTION 1  <-- NOTE: If found, will be moved to OPTION("FOOTER",1,1)
  ;"      OPTION("FOOTER",1,1)=linePart <--- OPTION 2  (these will be all strung together to make one footer line.
  ;"      OPTION("FOOTER",1,2)=linePart                (can be used to display switches etc)
  ;"      OPTION("FOOTER",2)=More footer line TEXT (any number of lines) 
  IF $DATA(VIEWSTATE(ATABLE,"TABLE"))>0 GOTO IFTDN 
  IF $DATA(OPTION(ATABLE))=0 GOTO IFTDN 
  NEW TABLE,FG,BG
  NEW AROW SET AROW=1
  NEW HEADERCOLOR SET HEADERCOLOR=$$GETCOLOR(ATABLE,.OPTION)
  NEW IDX SET IDX=""
  FOR  SET IDX=$ORDER(OPTION(ATABLE,IDX)) QUIT:(IDX'>0)  DO
  . SET TABLE(AROW,1)=$GET(OPTION(ATABLE,IDX))
  . SET TABLE(AROW,1,"COLOR")=HEADERCOLOR
  . IF ATABLE="FOOTER" DO
  . . NEW COLNUM SET COLNUM=0
  . . IF $ORDER(OPTION("FOOTER",AROW,COLNUM))="" DO    ;"Stuff e.g. OPTION("FOOTER",#)=<sometext> into OPTION("FOOTER",#,1)=<sometext>
  . . . NEW TEXT SET TEXT=$GET(OPTION("FOOTER",AROW)) QUIT:TEXT=""
  . . . KILL OPTION("FOOTER",AROW) SET OPTION("FOOTER",AROW,1)=TEXT
  . . FOR  SET COLNUM=$ORDER(OPTION("FOOTER",AROW,COLNUM)) QUIT:COLNUM'>0  DO
  . . . NEW COLTEXT SET COLTEXT=$GET(OPTION("FOOTER",AROW,COLNUM))
  . . . SET TABLE(AROW,COLNUM)=COLTEXT
  . . . SET TABLE(AROW,COLNUM,"COLOR")=HEADERCOLOR
  . SET AROW=AROW+1
  IF $DATA(OPTION(ATABLE,"COL")),NUMCOLS>1 DO   ;"<-- I think this will only be found for HEADER, but allow to process if found in FOOTER
  . NEW TEXT SET TEXT=""
  . NEW COLNUM FOR COLNUM=1:1:NUMCOLS DO
  . . NEW W SET W=+$GET(VIEWSTATE("WIDTH",COLNUM))
  . . NEW COLTEXT SET COLTEXT=$GET(OPTION(ATABLE,"COL",COLNUM))
  . . SET TABLE(AROW,COLNUM)=COLTEXT
  . . IF W>0 SET TABLE(AROW,COLNUM,"WIDTH")=W
  . . SET TABLE(AROW,COLNUM,"COLOR")=HEADERCOLOR
  SET TABLE("TABLE","COLOR")=$$GETCOLOR("TOP LINE",.OPTION)
  NEW TABLEOPT SET TABLEOPT("ASCII")=$SELECT($GET(OPTION("UNICODE LINES"))=1:0,1:2)
  SET VIEWSTATE(ATABLE,"SIZE")=$$QUERYTABLE^TMGUSRI9(.TABLE,SCRNW)
  MERGE VIEWSTATE(ATABLE,"TABLE")=TABLE
  MERGE VIEWSTATE(ATABLE,"TABLE OPTIONS")=TABLEOPT    
IFTDN ;  
  QUIT
  ;
DATAREF(COLNUM) ;"return reference to data, given COLNUM
  ;"NOTE: uses TMGPSCRLARR,VIEWSTATE in global scope
  IF COLNUM=1 QUIT TMGPSCRLARR
  NEW REF SET REF=TMGPSCRLARR_"(""COL"","_COLNUM_","
  NEW ACOL 
  FOR ACOL=1:1:COLNUM-1 DO
  . SET REF=REF_+$GET(VIEWSTATE("HIGHLINE",ACOL))_","
  SET REF=REF_"""TXT"","
  SET REF=$$CREF^DILF(REF)
  QUIT REF
  ;
COLLEFT(VIEWSTATE,COLNUM) ;"Return left X position for given COLNUM
  NEW RESULT SET RESULT=1
  NEW IDX FOR IDX=1:1:COLNUM-1 SET RESULT=RESULT+(VIEWSTATE("WIDTH",IDX))
  QUIT RESULT
  ;
COLRIGHT(VIEWSTATE,COLNUM,CLIP2VP) ;"Return right X position for given COLNUM
  SET CLIP2VP=+$GET(CLIP2VP)
  NEW RESULT SET RESULT=0
  NEW IDX FOR IDX=1:1:COLNUM SET RESULT=RESULT+(VIEWSTATE("WIDTH",IDX))
  IF CLIP2VP DO
  . ;"ENSURE IN VIEWPORT...
  QUIT RESULT
  ;
CHECKSCRNWIDTH(SCRNW,OPTION)  ;
  ;"note: Uses in global scope: LASTSCRNW, SCRNLINE, SPACELINE
  NEW CHANGED SET CHANGED=0
  SET SCRNW=+$GET(OPTION("SCRN WIDTH"))
  IF (SCRNW'>0) IF $$GETSCRSZ^TMGKERNL(,.SCRNW) SET SCRNW=+SCRNW-2
  IF SCRNW'>0 SET SCRNW=$GET(IOM,66)-2
  ;"//kt --> removed to force query of window each time --> SET OPTION("SCRN WIDTH")=SCRNW
  IF (SCRNW'=LASTSCRNW) DO    ;"if screen is resized, then clear screen, etc
  . IF LASTSCRNW'=-1 WRITE #  ;"Clear screen if not first run  
  . SET SCRNLINE="" SET $PIECE(SCRNLINE,"-",SCRNW)="-"
  . SET SPACELINE="" SET $PIECE(SPACELINE," ",SCRNW)=" "
  . SET LASTSCRNW=SCRNW  
  . SET CHANGED=1
  DO  ;"//kt mod 3/9/24 to ensure $IO device WIDTH is not smaller than 
  . NEW %TEMP ZSHOW "D":%TEMP SET %TEMP=+$PIECE($PIECE($GET(%TEMP("D",1)),"WIDTH=",2)," ",1)   
  . IF %TEMP>0,SCRNW>(%TEMP+1) DO
  . . USE $IO:WIDTH=(SCRNW+1)
  QUIT CHANGED
  ;
CHECKSCRNHEIGHT(SCRNH,OPTION) ;
  ;"note: I don't want height to autoresize, i.e. I often don't want full height
  SET SCRNH=+$GET(OPTION("SCRN HEIGHT"))   
  NEW SHRINK SET SHRINK=3
  IF (SCRNH'>0) IF $$GETSCRSZ^TMGKERNL(.SCRNH,) SET SCRNH=+SCRNH-SHRINK
  IF SCRNH'>0 SET SCRNH=$GET(IOSL,25)-SHRINK
  IF SCRNH'>0 SET SCRNH=15 SET OPTION("SCRN HEIGHT")=SCRNH
  QUIT
  ;
AUTOSETCOLWIDTHS(VIEWSTATE,SCRNW,OPTION,OVERRIDE) ;"Set up column widths in OPTION, and VIEWSTATE
  ;"INPUT: VIEWSTATE -- PASS BY REFERENCE, AN OUT PARAMETER
  ;"       SCRNW -- current screen width
  ;"       OPTION -- PASS BY REFERENCE
  ;"       OVERRIDE -- OPTIONAL.  if 1, then prior WIDTH specification is overwritten
  ;"TO DO: FINISH....
  ;"  NOTE: OPTION("COLUMNS",<COLNUM>,"WIDTH") can specify the width needed to display the data in this column.
  ;"  VIEWSTATE("WIDTH",<display column>) will refer the display colums (?? -- figure out what to do...)
  NEW WIDTH,COLNUM   
  NEW MAXDISPCOLS SET MAXDISPCOLS=+$GET(OPTION("COLUMNS","MAX DISPLAY NUM"),4)
  NEW COLS SET COLS=$GET(OPTION("COLUMNS","NUM"),1)
  SET OVERRIDE=+$GET(OVERRIDE)
  IF MAXDISPCOLS>COLS SET MAXDISPCOLS=COLS
  ;"First, see how much with is accounted for by user-specified widths.    
  FOR COLNUM=1:1:COLS DO
  . NEW NUM SET NUM=$GET(OPTION("COLUMNS",COLNUM,"WIDTH")) IF NUM<1 SET NUM=0
  . IF NUM["%" DO 
  . . SET NUM=(SCRNW*+NUM/100)\1
  . SET WIDTH(COLNUM)=+NUM
  . SET WIDTH=$GET(WIDTH)+NUM  ;"count up width for column with specified width.
  ;"Count how many columns are have user-specified width = 0
  NEW ZEROS SET ZEROS=0 FOR COLNUM=1:1:COLS IF WIDTH(COLNUM)=0 SET ZEROS=ZEROS+1,ZEROS(COLNUM)=""  
  NEW RESIDUAL SET RESIDUAL=SCRNW-WIDTH  ;"If RESIDUAL<0 then total column with is GREATER THAN screen width
  IF ZEROS>MAXDISPCOLS SET ZEROS=MAXDISPCOLS
  NEW AUTOWIDTHPERCOL SET AUTOWIDTHPERCOL=$SELECT((ZEROS>0)&(RESIDUAL>0):RESIDUAL\ZEROS,1:0)  ;"Divide residual width between unspecified columns.
  IF AUTOWIDTHPERCOL<10 SET AUTOWIDTHPERCOL=10
  ;"Next, for any columns with user-specified width=0, assign an autosize value.  
  FOR COLNUM=1:1:COLS DO
  . IF WIDTH(COLNUM)>0 QUIT
  . SET WIDTH(COLNUM)=AUTOWIDTHPERCOL
  . SET WIDTH=WIDTH+AUTOWIDTHPERCOL
  IF WIDTH<SCRNW DO   ;"E.g. if RESIDUAL =10 and ZERO's=3 --> AUTOWIDTHPERCOL =3.33-> 3.  3*3=9, SHORT OF DESIRED 10
  . NEW SHORT SET SHORT=SCRNW-WIDTH
  . SET WIDTH(COLS)=WIDTH(COLS)+SHORT
  ;
  ;"NEW WIDTHPERCOL SET WIDTHPERCOL=SCRNW\MAXDISPCOLS
  ;"NEW COLNUM FOR COLNUM=1:1:COLS DO
  ;". NEW NUM SET NUM=$GET(OPTION("COLUMNS",COLNUM,"WIDTH")) 
  ;". IF OVERRIDE!(NUM<1) SET NUM=0
  ;". IF NUM=0 SET NUM=WIDTHPERCOL
  ;". SET OPTION("COLUMNS",COLNUM,"WIDTH")=NUM
  ;". SET WIDTH(COLNUM)=NUM
  ;
  ;;". NEW NUM SET NUM=$GET(OPTION("COLUMNS",COLNUM,"WIDTH")) IF NUM<1 SET NUM=0
  ;;"-- NOTE: I am changing code such that there can be more columns than will be shown on screen
  ;;"         So I am no longer going to make sure they all fit on the screen.  
  ;;"         Old method below....
  ;;"NEW COLNUM FOR COLNUM=1:1:COLS DO
  ;;". NEW NUM SET NUM=$GET(OPTION("COLUMNS",COLNUM,"WIDTH")) IF NUM<1 SET NUM=0
  ;;". IF NUM["%" DO 
  ;;". . SET NUM=(SCRNW*+NUM/100)\1
  ;;". SET WIDTH(COLNUM)=+NUM
  ;;". SET WIDTH=$GET(WIDTH)+NUM
  ;;"FOR  QUIT:(WIDTH'>SCRNW)  DO  ;"If too wide, shrink each column until fits. 
  ;;". FOR COLNUM=1:1:COLS IF WIDTH(COLNUM)>1 SET WIDTH(COLNUM)=WIDTH(COLNUM)-1,WIDTH=WIDTH-1
  ;;"NEW DONE SET DONE=0
  ;;"FOR  DO  QUIT:DONE
  ;;". NEW ZEROS SET ZEROS=0 FOR COLNUM=1:1:COLS IF WIDTH(COLNUM)<1 SET ZEROS=ZEROS+1,ZEROS(COLNUM)=""
  ;;". IF ZEROS=0 SET DONE=1 QUIT
  ;;". NEW LARGEST SET LARGEST=0 FOR COLNUM=1:1:COLS IF WIDTH(COLNUM)'<LARGEST SET LARGEST=WIDTH(COLNUM),LARGEST(0)=COLNUM
  ;;". IF WIDTH<SCRNW DO  QUIT  ;"If too narrow, then divide available space between columns.  
  ;;". . NEW TEMP SET TEMP=(SCRNW-WIDTH)\ZEROS
  ;;". . FOR COLNUM=1:1:COLS IF WIDTH(COLNUM)=0 SET WIDTH(COLNUM)=TEMP,WIDTH=WIDTH+TEMP
  ;;". . IF WIDTH<SCRNW SET WIDTH(1)=WIDTH(1)+(SCRNW-WIDTH)
  ;;". ELSE  DO  ;"WIDTH=SCRNWIDTH, but still have a zero column.
  ;;". . NEW IDX SET IDX=LARGEST(0)
  ;;". . NEW W SET W=WIDTH(IDX)
  ;;". . NEW HALF SET HALF=W\2
  ;;". . NEW ZCOL SET ZCOL=$ORDER(ZEROS(""))
  ;;". . SET WIDTH(IDX)=WIDTH(IDX)-HALF
  ;;". . SET WIDTH(ZCOL)=WIDTH(ZCOL)+HALF
  KILL VIEWSTATE("WIDTH") MERGE VIEWSTATE("WIDTH")=WIDTH
  SET VIEWSTATE("COLS")=COLS
  QUIT
  ;  
ADJUSTVS4CSR(VIEWSTATE,OPTION) ;"Ensure the ViewState so that cursor/highline (selected line) will be on screen
  ;"NOTE: This doesn't actually do any drawing.  It just adjusts view parameters.  
  ;"Input VIEWSTATE -- Pass by reference
  ;"      OPTION -- Pass by reference 
  ;"NOTE: Uses TMGPSCRLARR,SCRNW in global scope
  NEW COL SET COL=+$GET(VIEWSTATE("ACTIVECOL"))
  NEW DATAREF SET DATAREF=$$DATAREF(COL)
  NEW NUMCOLS SET NUMCOLS=+$GET(OPTION("COLUMNS","NUM"),1)
  NEW MAXIDX SET MAXIDX=+$ORDER(@DATAREF@("@@@@@@@@"),-1)
  NEW MAINLINES SET MAINLINES=$GET(VIEWSTATE("MAINLINES"))
  NEW HL SET HL=$GET(VIEWSTATE("HIGHLINE",COL))
  NEW USINGNEEDDATA SET USINGNEEDDATA=($DATA(OPTION("ON NEED DATA"))>0)
  IF USINGNEEDDATA=0 SET VIEWSTATE("HIGHLINE",COL)=$$INBOUNDS^TMGMISC(HL,1,MAXIDX)  ;"<-- this sets for ACTIVECOL
  NEW COLNUM FOR COLNUM=1:1:NUMCOLS DO   ;"Ensure all in bounds
  . NEW ENTRYCT SET ENTRYCT=$GET(VIEWSTATE("ENTRYCT",COLNUM),1) 
  . IF USINGNEEDDATA SET ENTRYCT=99999999  ;"for dynamic RT data, we can't know upper limit index of data.  
  . NEW TL SET TL=$GET(VIEWSTATE("TOPLINE",COLNUM))
  . NEW HL SET HL=$GET(VIEWSTATE("HIGHLINE",COLNUM))
  . SET VIEWSTATE("TOPLINE",COLNUM)=$$INBOUNDS^TMGMISC(TL,1,ENTRYCT)
  . SET VIEWSTATE("HIGHLINE",COLNUM)=$$INBOUNDS^TMGMISC(HL,1,ENTRYCT)
  . IF VIEWSTATE("HIGHLINE",COLNUM)<TL SET VIEWSTATE("TOPLINE",COLNUM)=HL
  . ELSE  IF MAINLINES>0,HL>(TL+MAINLINES-1) DO
  . . SET VIEWSTATE("TOPLINE",COLNUM)=HL-(MAINLINES-1) 
  ;"Now adjust viewport, if needed, to ensure ACTIVECOL is visible.
  NEW VISIBLE SET VISIBLE=$$COLVISIBLE(COL,.VIEWSTATE)
  IF VISIBLE=2 GOTO ADJDN       ;"fully visible in viewport.
  NEW VPCOLLF SET VPCOLLF=$$COLLEFT(.VIEWSTATE,COL)  ;"VIEWPORT COORDINATES
  NEW VPCOLRT SET VPCOLRT=$$COLRIGHT(.VIEWSTATE,COL) ;"VIEWPORT COORDINATES
  NEW SCRNCOLLF SET SCRNCOLLF=$$VPX2SCRNX(VPCOLLF)  ;"SCREEN COORDINATES
  NEW SCRNCOLRT SET SCRNCOLRT=$$VPX2SCRNX(VPCOLRT)  ;"SCREEN COORDINATES
  IF VISIBLE=0 DO  GOTO ADJDN   ;"Not visible in viewport.  
  . IF SCRNCOLLF>SCRNW DO  QUIT ;"outside viewport, on right
  . . SET VIEWSTATE("VIEWPORTX")=VPCOLRT-SCRNW
  . ELSE  DO                    ;"outside viewport, on left
  . . SET VIEWSTATE("VIEWPORTX")=VPCOLLF
  IF VISIBLE=1 DO  GOTO ADJDN   ;"partly visible on left
  . SET VIEWSTATE("VIEWPORTX")=VPCOLLF
  IF VISIBLE=3 DO  GOTO ADJDN   ;"partly visible on right
  . SET VIEWSTATE("VIEWPORTX")=VPCOLRT-SCRNW      
ADJDN ;  
  QUIT
  ;
OPT2VIEWSTATE(OPTION,VIEWSTATE) ;"Transfer viewstate from OPTION to VIEWSTATE 
  ;"Note: Uses vars in global scope, defined in SCROLLER scope. 
  NEW RESULT SET RESULT=0
  IF $GET(OPTION("COLUMNS","NUM"))>$GET(VIEWSTATE("COLS")) DO
  . SET VIEWSTATE("COLS")=$GET(OPTION("COLUMNS","NUM"),1)
  . DO ENSUREVSVARS(.VIEWSTATE)
  SET VIEWSTATE("ACTIVECOL")=$GET(OPTION("COLUMNS","ACTIVE"),1)
  NEW CHANGED SET CHANGED=0
  IF $GET(OPTION("HIGHLINE"))>0 DO   ;"//kt 8/4/22
  . IF $GET(OPTION("HIGHLINE"))=$GET(VIEWSTATE("HIGHLINE",1)) QUIT  ;"//kt 11/24
  . SET VIEWSTATE("HIGHLINE",1)=OPTION("HIGHLINE")
  . SET CHANGED=1  ;"//kt 11/24
  NEW NUMCOLS SET NUMCOLS=+$GET(OPTION("COLUMNS","NUM"),1)
  SET VIEWSTATE("COLS")=NUMCOLS
  NEW COLNUM FOR COLNUM=2:1:NUMCOLS DO
  . NEW HL SET HL=+$GET(OPTION("COLUMNS",COLNUM,"HIGHLINE")) 
  . IF HL=0 SET HL=1,OPTION("COLUMNS",COLNUM,"HIGHLINE")=HL
  . IF HL=$GET(VIEWSTATE("HIGHLINE",COLNUM)) QUIT
  . SET VIEWSTATE("HIGHLINE",COLNUM)=HL  
  . SET CHANGED=1     
  SET VIEWSTATE("ACTIVECOL")=$GET(OPTION("COLUMNS","ACTIVE"),1)
  IF CHANGED DO
  . DO ADJUSTVS4CSR(.VIEWSTATE,.OPTION) ;"Ensure the highline will be on screen  
  . SET NEEDREFRESH=2
  ;
  NEW FNCYCLE SET FNCYCLE=$GET(OPTION("COLUMNS","FN TOGGLE NUM"))
  IF (FNCYCLE'="")&(VIEWSTATE("COLS")>1) DO
  . SET OPTION("FOOTER",0.77)=FNCYCLE_" CYCLE ACTIVE COLUMN"   ;"0.77 to insert into top of list, but unlikely to collide with user's  
  ;  
  ;"If the definition of the HEADER or FOOTER has changed in OPTION, reset VIEWSTATE
  NEW ATABLE FOR ATABLE="HEADER","FOOTER" DO
  . ;"NEW TABLENAME SET TABLENAME=ATABLE_" TABLE"
  . IF $$COMPARRAY^TMGMISC($NAME(VIEWSTATE(ATABLE,"SOURCE")),$NAME(OPTION(ATABLE)))=0 DO
  . . KILL VIEWSTATE(ATABLE)
  . IF $DATA(VIEWSTATE(ATABLE,"SOURCE"))=0 DO
  . . KILL VIEWSTATE(ATABLE,"TABLE")  ;"will refresh this in INITVIEWSTATE
  . . MERGE VIEWSTATE(ATABLE,"SOURCE")=OPTION(ATABLE)
  . . DO INITHFTABLE(ATABLE,.VIEWSTATE,.OPTION)  ;"refresh table
  ;
  QUIT RESULT 
  ;
VIEWSTATE2OPT(VIEWSTATE,OPTION) ;"Transfer info from VIEWSTATE to OPTION
  NEW ACOL SET ACOL=0
  FOR  SET ACOL=$ORDER(VIEWSTATE("HIGHLINE",ACOL)) QUIT:ACOL'>0  DO
  . NEW NUM SET NUM=+$GET(VIEWSTATE("HIGHLINE",ACOL)) QUIT:NUM'>0
  . IF ACOL=1 DO  
  . . SET OPTION("HIGHLINE")=NUM
  . ELSE  DO
  . . SET OPTION("COLUMNS",ACOL,"HIGHLINE")=NUM
  SET OPTION("COLUMNS","ACTIVE")=VIEWSTATE("ACTIVECOL")
  SET OPTION("COLUMNS","NUM",1)=VIEWSTATE("COLS")        
  QUIT
  ;
PREPINFO(INFO,NODE,COL,IDX) ;
  ;"Uses TMGPSCRLARR in global scope
  SET INFO(NODE,"NUMBER")=IDX
  NEW DATAREF SET DATAREF=$$DATAREF(COL)
  IF COL=1 DO
  . SET INFO(NODE,"TEXT")=$ORDER(@DATAREF@(IDX,""))
  . SET INFO(NODE,"RETURN")=$GET(@DATAREF@(IDX,INFO(NODE,"TEXT")))
  ELSE  DO
  . SET INFO(NODE,"TEXT")=$GET(@DATAREF@(IDX))
  . SET INFO(NODE,"RETURN")=""
  QUIT
  ;
ISCURSOR(ESCKEY) ;
  ;"FYI: FIND=home key and SELECT=end key
  QUIT "UP^DOWN^LEFT^RIGHT^PREV^NEXT^FIND^SELECT^HOME^END^"[ESCKEY_"^"
  ;  
VPX2SCRNX(VPX,VIEWSTATE)  ;"Transform Viewport coordinates into Screen coordinates. 
  NEW RESULT SET RESULT=$GET(VPX)-$GET(VIEWSTATE("VIEWPORTX"))
  QUIT RESULT
  ;
VPWRITE(VPX,VPY,VIEWSTATE,TEXT,MAXSTRLEN,COLORS,CLIP) ;"'VIEWPORT_WRITE' -- WRITE (OPTIONALLY) COLORED TEXT, CLIPPED TO CURRENT VIEWPORT
  ;"INPUT:  VPX -- X POSITION **in viewport space** of start of text  -- If viewport is shifted, this will be transformed
  ;"        VPY -- Y POSITION.  Techniclly in viewport space, but viewport doesn't move up/down, so same as screen space
  ;"        VIEWSTATE -- PASS BY REFERENCE.  Array with viewstate information
  ;"        TEXT -- The string to write out
  ;"        MAXSTRLEN -- The maximum number of chars from TEXT that can be written out.  May be shortened further by viewport clipping
  ;"        COLORS -- PASS BY REFERENCE.  Array with colors for output.  
  ;"        CLIP -- OPTIONAL.  If 1, then text is just cut off, rather than trimmed, with "..."
  ;"GLOBAL SCOPE USE: SCRNW
  NEW SCRNX SET SCRNX=$$VPX2SCRNX(VPX,.VIEWSTATE)  ;"transform VP space --> screen space
  NEW MAXX SET MAXX=$$VPX2SCRNX(VPX+$GET(MAXSTRLEN)) ;"MAXX is now in screen coordinates
  IF MAXX>SCRNW SET MAXX=SCRNW
  IF SCRNX<1 DO
  . NEW NUMCH2CUT SET NUMCH2CUT=-SCRNX+1  ;"SCRNX OF 0->1, -1->2, -2->3 ETC.  
  . NEW OPT SET OPT("KEEP LEFT TAGS")=1  
  . SET TEXT=$$MKSTRMID^TMGSTUT3(.TEXT,NUMCH2CUT+1,$LENGTH(TEXT),"{{","}}",.OPT)
  . SET SCRNX=1
  DO CUP^TMGTERM(SCRNX,YPOS)
  DO WCLTEXT(.TEXT,MAXX,.COLORS,.CLIP) 
  QUIT
  ;
WCLTEXT(TEXT,MAXX,COLORS,CLIP) ;"WRITE (OPTIONALLY) COLORED TEXT  
  NEW INITTEXT SET INITTEXT=$GET(TEXT)  ;"for debugging
  NEW TEXTA,TEXTB,TEXTCOLOR,WIDTH
  FOR  QUIT:(TEXT'["{{")!($X'<MAXX)  DO
  . SET TEXTCOLOR=$$PARSCOLR(.TEXT,.TEXTA)  ;" Text --> TextA,Text and {{COLOR}} to TEXTCOLOR
  . DO TRIMWRITE(TEXTA,MAXX,.CLIP)
  . IF $$COLORDEFINED(TEXTCOLOR,.COLORS) DO
  . . DO SETCOLOR(TEXTCOLOR,.COLORS)
  . ELSE  IF $$ISCOLORPAIR^TMGUSRI8(TEXTCOLOR) DO
  . . NEW FG,BG
  . . DO SPLITCOLORPAIR^TMGUSRI8(TEXTCOLOR,.FG,.BG)
  . . ;"NEW FG,BG SET FG=$PIECE(TEXTCOLOR,"^",1),BG=$PIECE(TEXTCOLOR,"^",2)
  . . DO COLORS^TMGTERM(FG,BG)
  . ELSE  DO
  . . WRITE "{{"_TEXTCOLOR_"}}"
  DO TRIMWRITE(TEXT,MAXX,.CLIP)
  QUIT
  ;
TRIMWRITE(TEXT,MAXX,CLIP)  ;"trimmed write to screen
  ;"Input: TEXT -- text to write
  ;"       MAXX -- max width
  ;"       CLIP -- OPTIONAL.  If 1, then text is just cut off, rather than trimmed, with "..."
  IF $X<1 SET $X=1  ;"$X is where next char will be written.  Shouldn't be 0, but sometimes is. 
  NEW CURSORX SET CURSORX=$X
  SET CLIP=($GET(CLIP)=1)
  NEW WIDTH SET WIDTH=CURSORX+$LENGTH(TEXT)-1
  IF WIDTH>MAXX DO
  . IF CLIP=0 DO
  . . NEW TRIMLEN SET TRIMLEN=((MAXX-CURSORX+1)-3)
  . . NEW TEMPSTR SET TEMPSTR=$EXTRACT(TEXT,1,TRIMLEN)
  . . IF TEMPSTR'="" WRITE TEMPSTR_"..."
  . ELSE  IF CLIP=1 DO
  . . NEW TRIMLEN SET TRIMLEN=((MAXX-CURSORX+1))
  . . NEW TEMPSTR SET TEMPSTR=$EXTRACT(TEXT,1,TRIMLEN)
  . . IF TEMPSTR'="" WRITE TEMPSTR
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
WRAPDATA(REF,OPTION,SCRNW)  ;"Wrap data to width.  
  ;"Input: REF -- PASS BY NAME.  format:
  ;"         @REF@(1,DisplayText)=Return Text <-- note: must be numbered 1,2,3 etc. (INTEGERS)
  ;"         @REF@(2,DisplayText)=Return Text
  ;"         @REF@(3,DisplayText)=Return Text
  ;"       OPTION
  ;"       SCRNW
  SET WIDTH=$GET(OPTION("WRAP DATA")) QUIT:WIDTH'>0
  IF WIDTH=1 SET WIDTH=$GET(SCRNW) QUIT:WIDTH'>1
  NEW MODIFIED SET MODIFIED=0
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(@REF@(IDX)) QUIT:IDX'>0  DO
  . NEW TXT,DONE SET DONE=0 
  . SET TXT=$ORDER(@REF@(IDX,""))
  . NEW VAL SET VAL=$GET(@REF@(IDX,TXT))
  . FOR  DO  QUIT:DONE
  . . IF $$NOCOLEN(TXT)'>WIDTH SET DONE=1 QUIT
  . . ;"NOTE -- Later I can add code to ensure split doesn't take place inside a color marker. 
  . . NEW PARTA SET PARTA=$EXTRACT(TXT,1,WIDTH)
  . . NEW PARTB SET PARTB=" ~WRAP~ "_$EXTRACT(TXT,WIDTH+1,$LENGTH(TXT))
  . . KILL @REF@(IDX)
  . . SET @REF@(IDX,PARTA)=VAL
  . . SET IDX=IDX+0.000001
  . . SET @REF@(IDX,PARTB)=VAL
  . . SET TXT=PARTB
  . . SET MODIFIED=1
  IF MODIFIED DO
  . SET IDX=0
  . NEW JDX SET JDX=0
  . NEW TEMP
  . FOR  SET IDX=$ORDER(@REF@(IDX)) QUIT:IDX'>0  DO
  . . NEW TXT,DONE SET DONE=0 
  . . SET TXT=$ORDER(@REF@(IDX,""))
  . . NEW VAL SET VAL=$GET(@REF@(IDX,TXT))
  . . KILL @REF@(IDX) SET TEMP($I(JDX),TXT)=VAL
  . MERGE @REF=TEMP
  QUIT
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
EVENTDEF(OPTION,EVENT) ;"Return if event is defined.
  NEW RESULT SET RESULT=($GET(OPTION(EVENT))'="")
  QUIT RESULT
  ;
EVENT(OPTION,EVENT) ;"Trigger event, if defined.
  ;"NOTE: The event code uses INFO, which is in global scope.
  ;"GLOBAL SCOPE USED: TMGPSCRLARR,VIEWSTATE
  NEW CODEFN SET CODEFN=$$GETCODEFN(EVENT,.OPTION) QUIT:(CODEFN="")
  KILL INFO("VIEWSTATE") MERGE INFO("VIEWSTATE")=VIEWSTATE
  NEW ACTIVECOL SET ACTIVECOL=VIEWSTATE("ACTIVECOL")
  DO PREPINFO(.INFO,"CURRENT LINE",ACTIVECOL,VIEWSTATE("HIGHLINE",ACTIVECOL))  
  NEW $ETRAP DO SETETRAP
  XECUTE CODEFN
  KILL INFO("VIEWSTATE")
  IF $DATA(INFO("DO FUNCTION")) DO HNDDOFN(.INFO,.OPTION,TMGPSCRLARR)
  QUIT
  ;
HNDDOFN(INFO,OPTION,TMGPSCRLARR) ;
  NEW AFN,FOUND SET FOUND=0
  NEW SEQ SET SEQ=""
  FOR  SET SEQ=$ORDER(INFO("DO FUNCTION",SEQ)) QUIT:SEQ'>0  DO
  . SET AFN=$GET(INFO("DO FUNCTION",SEQ)) QUIT:AFN=""
  . NEW DATA MERGE DATA=INFO("DO FUNCTION",SEQ,"DATA")
  . DO FN(AFN,.DATA,.OPTION,.TMGPSCRLARR)
  . KILL INFO("DO FUNCTION",SEQ)
  . SET FOUND=1
  IF 'FOUND DO
  . SET AFN=$GET(INFO("DO FUNCTION")) QUIT:AFN=""
  . NEW DATA MERGE DATA=INFO("DO FUNCTION","DATA")
  . DO FN(AFN,.DATA,.OPTION,.TMGPSCRLARR)
  . KILL INFO("DO FUNCTION")
  QUIT;
  ;
FN(FN,DATA,OPTION,TMGPSCRLARR) ;
  ;"GLOBAL SCOPE USED:  VIEWSTATE, SCRNW
  NEW PARAMS SET PARAMS=""
  IF FN["(" SET PARAMS=$PIECE($PIECE(FN,"(",2),")",1),FN=$PIECE(FN,"(",1)
  IF FN["ADD COLUMN" DO
  . DO ADDCOLUMN(.DATA,.OPTION,.VIEWSTATE,SCRNW,PARAMS)
  . SET TMGSCLRMSG="FULL"  
  IF FN["DROP COLUMN" DO
  . DO DROPCOLUMN(.DATA,.OPTION,.VIEWSTATE,SCRNW,PARAMS)
  . SET TMGSCLRMSG="FULL"  
  IF FN["SCROLL VIEWPORT" DO
  . DO SCROLLVIEWPORT(.OPTION,.VIEWSTATE,SCRNW,PARAMS)
  . SET TMGSCLRMSG="FULL"  
  IF FN["GROW COLUMN" DO
  . DO GROWCOLUMN(.DATA,.OPTION,.VIEWSTATE,SCRNW,PARAMS)
  . SET TMGSCLRMSG="FULL"  
  IF FN["SHRINK COLUMN" DO
  . DO SHRINKCOLUMN(.DATA,.OPTION,.VIEWSTATE,SCRNW,PARAMS)
  . SET TMGSCLRMSG="FULL"    
  QUIT
  ;
SCROLLVIEWPORT(OPTION,VIEWSTATE,SCRNW,PARAMS)  ;"Implementation for 'SCROLL VIEWPORT' function
  ;"GLOBAL SCOPE: SCRNW
  NEW MAXCOL SET MAXCOL=$GET(OPTION("COLUMNS","NUM"))
  NEW VPCOLRT SET VPCOLRT=$$COLRIGHT(.VIEWSTATE,MAXCOL)  ;"COORDINATES ARE IN VIEWPORT SPACE
  ;"Example: SCRNW=100, COLRIGHT of 10'th column is 300.  
  ;"  Keeping VIEWPORTX <= 200 (which is 300-100), this will keep viewport from scrolling too far.   
  NEW VPMAX SET VPMAX=VPCOLRT-SCRNW
  SET PARAMS=$GET(PARAMS)
  IF PARAMS="HOME" DO
  . SET VIEWSTATE("VIEWPORTX")=0
  ELSE  IF PARAMS="END" DO
  . ;"IMPLEMENT LATER
  ELSE  IF +PARAMS=PARAMS DO
  . NEW DELTA SET DELTA=PARAMS
  . NEW VPX SET VPX=+$GET(VIEWSTATE("VIEWPORTX"))
  . SET VPX=VPX+DELTA 
  . IF VPX<0 SET VPX=0
  . IF VPX>VPMAX SET VPX=VPMAX
  . SET VIEWSTATE("VIEWPORTX")=VPX
  QUIT
  ;
ADDCOLUMN(DATA,OPTION,VIEWSTATE,SCRNW,PARAMS)  ;"Implementation for 'ADD COLUMN' fuction
  NEW ACTIVECOL SET ACTIVECOL=+$GET(OPTION("COLUMNS","ACTIVE"),1)  
  NEW MAXCOLS SET MAXCOLS=+$GET(OPTION("COLUMNS","NUM"),1)
  SET MAXCOLS=MAXCOLS+1
  SET OPTION("COLUMNS","NUM")=MAXCOLS
  SET OPTION("COLUMNS",MAXCOLS,"HIGHLINE")=1
  SET OPTION("COLUMNS","ACTIVE")=ACTIVECOL+1  
  NEW NODE FOR NODE="WIDTH","SHOW INDEX","COLORS","HIGHLINE" DO
  . IF $DATA(DATA(NODE)) MERGE OPTION("COLUMNS",MAXCOLS,NODE)=DATA(NODE)
  IF +PARAMS=1 DO
  . DO AUTOSETCOLWIDTHS(.VIEWSTATE,SCRNW,.OPTION,0)  ;"if 1 --> Override prior column widths
  DO ENSUREVSVARS(.VIEWSTATE)
  DO OPT2VIEWSTATE(.OPTION,.VIEWSTATE)
  QUIT
  ;
DROPCOLUMN(DATA,OPTION,VIEWSTATE,SCRNW,PARAMS)  ;"Implementation for 'DROP COLUMN' fuction
  NEW ACTIVECOL SET ACTIVECOL=+$GET(OPTION("COLUMNS","ACTIVE"),1)  
  NEW MAXCOLS SET MAXCOLS=+$GET(OPTION("COLUMNS","NUM"),1)
  IF MAXCOLS=1 QUIT
  SET MAXCOLS=MAXCOLS-1
  SET OPTION("COLUMNS","NUM")=MAXCOLS
  SET OPTION("COLUMNS",MAXCOLS,"HIGHLINE")=1
  IF ACTIVECOL>MAXCOLS SET ACTIVECOL=MAXCOLS
  SET OPTION("COLUMNS","ACTIVE")=ACTIVECOL  
  DO AUTOSETCOLWIDTHS(.VIEWSTATE,SCRNW,.OPTION,0)  ;"if 1 --> Override prior column widths
  DO ENSUREVSVARS(.VIEWSTATE)
  DO OPT2VIEWSTATE(.OPTION,.VIEWSTATE)
  QUIT
  ;
GROWCOLUMN(DATA,OPTION,VIEWSTATE,SCRNW,PARAMS)  ;"Implementation for 'GROW COLUMN' fuction
  DO DELTACOLUMN(.OPTION,.VIEWSTATE,.PARAMS,1)
  QUIT
  ;
SHRINKCOLUMN(DATA,OPTION,VIEWSTATE,SCRNW,PARAMS)  ;"Implementation for 'SHRINK COLUMN' fuction
  DO DELTACOLUMN(.OPTION,.VIEWSTATE,.PARAMS,-1)
  QUIT
  ;
DELTACOLUMN(OPTION,VIEWSTATE,PARAMS,DIR)  ;"Implementation for 'GROW COLUMN' fuction
  ;"GLOBAL SCOPE: SCRNW
  NEW ACTIVECOL SET ACTIVECOL=+$GET(OPTION("COLUMNS","ACTIVE"),1)
  NEW COLNUM SET COLNUM=+$PIECE(PARAMS,",",1) IF COLNUM=0 SET COLNUM=ACTIVECOL
  NEW AMOUNT SET AMOUNT=+$PIECE(PARAMS,",",3) IF AMOUNT=0 SET AMOUNT=1
  SET DIR=+$GET(DIR) IF (DIR'=-1)&(DIR'=1) SET DIR=1
  NEW DELTA SET DELTA=AMOUNT*DIR
  NEW WIDTH SET WIDTH=$GET(OPTION("COLUMNS",COLNUM,"WIDTH"))
  IF WIDTH["%" SET WIDTH=+WIDTH*SCRNW/100
  IF WIDTH="" SET WIDTH=$GET(VIEWSTATE("WIDTH",COLNUM))
  SET WIDTH=WIDTH+DELTA
  SET OPTION("COLUMNS",COLNUM,"WIDTH")=WIDTH   ;"This puts screen width into OPTION, which is more perminent then in VIEWSTATE
  SET VIEWSTATE("WIDTH",COLNUM)=WIDTH
  ;"DO AUTOSETCOLWIDTHS(.VIEWSTATE,SCRNW,.OPTION,0)  ;"if 1 --> Override prior column widths
  DO ENSUREVSVARS(.VIEWSTATE)
  DO OPT2VIEWSTATE(.OPTION,.VIEWSTATE)
  QUIT
  ;
COLORDEFINED(LABEL,OPTION) ;"Return if color has been defined.
  QUIT ($DATA(OPTION("COLORS",LABEL))>0)
  ;
SETCOLOR(LABEL,COLORS,FG,BG) ;
  ;"Purpose: to SET color, based on LABEL name. (A utility function for Scroller)
  ;"Input: LABEL -- the name of the color, i.e. NORM, HIGH, etc.
  ;"              If LABEL=REST, then special ResetTerminal function called.
  ;"       COLORS -- PASS BY REFERENCE.  Subset of OPTION array passed to Scroller, with color info
  ;"                Specifically used: COLORS('COLORS',SomeName,'FG')=foregroundCOLOR
  ;"                                 COLORS('COLORS',SomeName,'BG')=backgroundCOLOR
  ;"       FG,BG -- OPTIONAL.  OUT PARAMETERS.  PASS BY REFERENCE to retrieve resulting FG and BG colors.  
  ;"Note: IF color label not found, then no color change is made.
  if $$GETCOLOR(.LABEL,.COLORS,.FG,.BG)  ;"ignore result
  IF FG=-1,BG=-1 DO VTATRIB^TMGTERM(0) QUIT  ;"reset colors
  DO COLORS^TMGTERM(FG,BG)
  QUIT
  ;  
GETCOLOR(LABEL,COLORS,FG,BG) ;
  ;"Purpose: to LOAD color, based on LABEL name, into FG, BG
  ;"Input: LABEL -- the name of the color, i.e. NORM, HIGH, etc.
  ;"              If LABEL=REST, then special ResetTerminal function called.
  ;"       COLORS -- PASS BY REFERENCE.  Subset of OPTION array passed to Scroller, with color info
  ;"                Specifically used: COLORS('COLORS',SomeName,'FG')=foregroundCOLOR
  ;"                                 COLORS('COLORS',SomeName,'BG')=backgroundCOLOR
  ;"       FG,BG -- OPTIONAL.  OUT PARAMETERS.  PASS BY REFERENCE to retrieve resulting FG and BG colors.  
  ;"RESULT: Returns COLORPAI: resulting FG^BG colors. 
  NEW COLORPAIR,SAVEIF SET SAVEIF=$T
  IF LABEL="RESET" SET (FG,BG)=-1 GOTO GCDN  ;"reset colors
  IF $DATA(COLORS("COLORS",LABEL))=0 DO SETDEFCOLORS(.COLORS)  ;
  SET FG=$GET(COLORS("COLORS",LABEL,"FG"))  ;",1) ;default to black
  SET BG=$GET(COLORS("COLORS",LABEL,"BG"))  ;",0) ;default to white
  IF (FG="")!(BG="") DO
  . SET COLORPAIR=$GET(COLORS("COLORS",LABEL))
  . IF $$ISCOLORPAIR^TMGUSRI8(COLORPAIR)=0 QUIT
  . DO SPLITCOLORPAIR^TMGUSRI8(COLORPAIR,.FG,.BG)
  IF (FG="")!(BG="") SET FG=1,BG=0  ;"default fo black foreground, white background
  IF BG="@" SET BG=$GET(COLORS("COLORS","NORM","BG"),0) ;"default to white
GCDN ;  
  SET COLORPAIR=FG_"^"_BG
  IF SAVEIF  ;"restore $T
  QUIT COLORPAIR
  ;  
SETDEFCOLORS(COLORS)  ;
  IF $GET(COLORS("COLORS","NORM"))="" SET COLORS("COLORS","NORM")="14^4" ;"white on blue
  IF $GET(COLORS("COLORS","HIGH"))="" SET COLORS("COLORS","HIGH")="14^6" ;"white on cyan
  IF $GET(COLORS("COLORS","SELECTED"))="" SET COLORS("COLORS","SELECTED")="14^5" ;"white on magenta
  IF $GET(COLORS("COLORS","HI-SEL"))="" SET COLORS("COLORS","HI-SEL")="13^2" ;"Cyan  on magenta
  IF $GET(COLORS("COLORS","HEADER"))="" SET COLORS("COLORS","HEADER")=COLORS("COLORS","NORM")
  IF $GET(COLORS("COLORS","FOOTER"))="" SET COLORS("COLORS","FOOTER")=COLORS("COLORS","NORM")
  IF $GET(COLORS("COLORS","TOP LINE"))="" SET COLORS("COLORS","TOP LINE")=COLORS("COLORS","NORM")
  IF $GET(COLORS("COLORS","BOTTOM LINE"))="" SET COLORS("COLORS","BOTTOM LINE")=COLORS("COLORS","NORM")
  IF $GET(COLORS("COLORS","INDEX"))="" SET COLORS("COLORS","INDEX")=COLORS("COLORS","NORM")
  DO EXPANDCOLORS(.COLORS)
  QUIT
  ;
SETCOLORS(OUT,COLNUM,OPTION) ;"Setup colors array 
  IF COLNUM=1 MERGE OUT("COLORS")=OPTION("COLORS")
  ELSE  MERGE OUT("COLORS")=OPTION("COLUMNS",COLNUM,"COLORS")
  IF $DATA(OUT("COLORS"))=0 DO SETDEFCOLORS(.OUT)
  DO EXPANDCOLORS(.OUT)
  QUIT
  ;
EXPANDCOLORS(COLORS) ;  
  NEW IDX SET IDX=""                          
  FOR  SET IDX=$ORDER(COLORS("COLORS",IDX)) QUIT:(IDX="")  DO
  . NEW COLORS SET COLORS=$GET(COLORS("COLORS",IDX))
  . NEW FG SET FG=$PIECE(COLORS,"^",1) IF FG="" SET FG=0
  . NEW BG SET BG=$PIECE(COLORS,"^",2) IF BG="" SET BG=1
  . SET COLORS("COLORS",IDX,"FG")=FG
  . SET COLORS("COLORS",IDX,"BG")=BG
  QUIT
  ;
PARSCOLR(TEXT,TEXTA)  ;
  ;"Purpose: To extract a color code from TEXT
  ;"Example:  Input TEXT  = 'This is {{HIGH}}something{{NORM}} to see.'
  ;"          Output TEXT = 'something{{NORM}} to see.'
  ;"          Output TEXTA = 'This is '
  ;"            function result = 'HIGH'
  ;"Input: TEXT -- PASS BY REFERENCE
  ;"         TEXTA -- PASS BY REFERENCE, an OUT PARAMETER
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
  ;"============================================================
ADDNICECOLORS(OPTION,MAXCOLS) ;
  NEW CLRWHITE SET CLRWHITE="Linen"
  NEW CLRBLUE SET CLRBLUE="CornflowerBlue"
  NEW CLRBLACK SET CLRBLACK="Black"
  NEW CLRYELLOW SET CLRYELLOW="Gold"
  NEW CLRGREY SET CLRGREY="Silver"
  NEW CLRCYAN SET CLRCYAN="Teal"
  NEW CLRMAGENTA SET CLRMAGENTA="DeepPink"
  NEW CLRRED SET CLRRED="Red"  ;"LightCoral"
  NEW CLRAPPBACK SET CLRAPPBACK="Beige"
  SET OPTION("COLORS","NORM")=$$COLOR24PAIR^TMGUSRI8(CLRWHITE,CLRBLUE,.TMPWCLR) 
  SET OPTION("COLORS","HIGH")=$$COLOR24PAIR^TMGUSRI8(CLRBLACK,CLRYELLOW,.TMPWCLR) 
  SET OPTION("COLORS","INACTIVE HIGH")=$$COLOR24PAIR^TMGUSRI8(CLRWHITE,CLRGREY,.TMPWCLR) 
  SET OPTION("COLORS","HEADER")=$$COLOR24PAIR^TMGUSRI8(CLRRED,CLRAPPBACK,.TMPWCLR)
  SET OPTION("COLORS","FOOTER")=$$COLOR24PAIR^TMGUSRI8(CLRRED,CLRAPPBACK,.TMPWCLR)
  SET OPTION("COLORS","TOP LINE")=$$COLOR24PAIR^TMGUSRI8(CLRBLACK,CLRAPPBACK,.TMPWCLR)
  SET OPTION("COLORS","BOTTOM LINE")=$$COLOR24PAIR^TMGUSRI8(CLRBLACK,CLRAPPBACK,.TMPWCLR)
  SET OPTION("COLORS","INDEX")=$$COLOR24PAIR^TMGUSRI8(CLRBLACK,CLRWHITE,.TMPWCLR)
  ;
  SET MAXCOLS=+$GET(MAXCOLS)
  IF MAXCOLS>=2 DO
  . ;"Ignored if 2 or 3 column not used. 
  . SET OPTION("COLUMNS",2,"COLORS","NORM")=$$COLOR24PAIR^TMGUSRI8(CLRWHITE,CLRCYAN,.TMPWCLR)
  . SET OPTION("COLUMNS",2,"COLORS","HIGH")=$$COLOR24PAIR^TMGUSRI8(CLRBLACK,CLRYELLOW,.TMPWCLR) 
  . SET OPTION("COLUMNS",2,"COLORS","INACTIVE HIGH")=$$COLOR24PAIR^TMGUSRI8(CLRWHITE,CLRGREY,.TMPWCLR) 
  . SET OPTION("COLUMNS",2,"COLORS","INDEX")=$$COLOR24PAIR^TMGUSRI8(CLRBLACK,CLRWHITE,.TMPWCLR)
  ;
  IF MAXCOLS>=3 DO
  . SET OPTION("COLUMNS",3,"COLORS","NORM")=$$COLOR24PAIR^TMGUSRI8(CLRWHITE,CLRBLUE,.TMPWCLR)
  . SET OPTION("COLUMNS",3,"COLORS","HIGH")=$$COLOR24PAIR^TMGUSRI8(CLRBLACK,CLRYELLOW,.TMPWCLR) 
  . SET OPTION("COLUMNS",3,"COLORS","HI-SEL")=$$COLOR24PAIR^TMGUSRI8(CLRWHITE,CLRMAGENTA,.TMPWCLR) 
  . SET OPTION("COLUMNS",3,"COLORS","INDEX")=$$COLOR24PAIR^TMGUSRI8(CLRBLACK,CLRWHITE,.TMPWCLR)
  . SET OPTION("COLUMNS",3,"COLORS","SELECTED")=$$COLOR24PAIR^TMGUSRI8(CLRWHITE,CLRRED,.TMPWCLR)  
  ;
  SET OPTION("UNICODE LINES")=1
  SET OPTION("UNICODE LINES","OPTION","THICK")=1
  QUIT  
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
  ;"BELOW IS GOOD FOR DEBUGGING
  NEW ZZDEBUG SET ZZDEBUG=0
  IF ZZDEBUG=1 DO                    
  . SET OPTION("SCRN TOP OFFSET")=22
  . SET OPTION("SCRN HEIGHT")=20
  . SET OPTION("SCRN WIDTH")=130
  ;  
  DO SCROLLER("ARRAY",.OPTION)
  QUIT
  ;
HNDONSEL(TMGPSCRLARR,OPTION,INFO)  ;"Part of TESTSCRL
  ;"Purpose: handle ON SELECT event from SCROLLER
  WRITE $GET(INFO("CURRENT LINE","TEXT")),!
  DO PRESS2GO^TMGUSRI2
  QUIT
  ;
HNDONKP(TMGPSCRLARR,OPTION,INFO)  ;"Part of TESTSCRL
  ;"Purpose: handle ON KEYPRESS event from SCROLLER
  WRITE $GET(INFO("USER INPUT")),!
  WRITE $GET(INFO("CMD")),!
  DO PRESS2GO^TMGUSRI2
  QUIT
  ;
HNDONCMD(TMGPSCRLARR,OPTION,INFO)  ;"Part of TESTSCRL
  ;"Purpose: handle ON SELECT event from SCROLLER
  WRITE !,$GET(INFO("USER INPUT")),!
  DO PRESS2GO^TMGUSRI2
  QUIT
  ;
  ;"============================================================
  ;"============================================================
  ;
DEMOSCRL ;"A VERY SIMPLE DEMO OF SCROLLER
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
  SET OPTION("LR SCROLLING")=1   
  SET OPTION("WRAP DATA")=1   
  ;"BELOW IS GOOD FOR DEBUGGING
  NEW ZZDEBUG SET ZZDEBUG=1
  IF ZZDEBUG=1 DO                    
  . SET OPTION("SCRN TOP OFFSET")=22
  . SET OPTION("SCRN HEIGHT")=20
  . SET OPTION("SCRN WIDTH")=40  ;"130
  ;  
  DO ADDNICECOLORS(.OPTION) 
  DO SCROLLER("DEMOARR",.OPTION)
  QUIT
  ;  
  ;"============================================================
  ;"============================================================
  ;
DEMOCOLS ;"DEMONSTRATE SCROLLER WITH COLUMNS.  
  ;"NOTE: There is also DEMOSCRL above
  NEW ARRAY,OPTION,TEMPCLR
  SET OPTION("COLUMNS","NUM")=3 
  SET OPTION("COLUMNS",1,"WIDTH")=30
  SET OPTION("COLUMNS","FN TOGGLE NUM")="F1"
  SET OPTION("LR SCROLLING")=0
  NEW RTN SET RTN="TMG"
  NEW IDX FOR IDX=1:1:60 DO
  . SET RTN=$ORDER(^DIC(9.8,"B",RTN)) QUIT:RTN=""
  . SET ARRAY(IDX,RTN)="Result for "_RTN
  . NEW JDX SET JDX=0
  . FOR  SET JDX=JDX+1 DO  QUIT:JDX=0
  . . NEW LINE SET LINE=$TEXT(+JDX^@RTN)
  . . IF LINE="" SET JDX=0 QUIT
  . . SET ARRAY("COL",2,IDX,"TXT",JDX)=LINE
  . . ;"Note: Above is storing all possible display text for column 2 into array ahead of time, 
  . . ;"      NOT dynamically after selection from column 1
  . . ;"      HOWEVER, data for column 3 is added at runtime in HNDONCURSOR below.  Could be added here, but this demo is showing another option
  SET OPTION("HEADER",1)=" - < Here is a header line > -"
  SET OPTION("HEADER",2)=" Have a great day!"
  SET OPTION("FOOTER",1)="Enter ^ to exit"
  SET OPTION("ON SELECT")="HNDONSEL2^TMGUSRIF"
  SET OPTION("ON CMD")="HNDONCMD2^TMGUSRIF"
  SET OPTION("ON CURSOR")="HNDONCURSOR^TMGUSRIF"
  ;
  DO ADDNICECOLORS(.OPTION,3) 
  ;
  ;"BELOW IS GOOD FOR DEBUGGING
  NEW ZZDEBUG SET ZZDEBUG=1
  IF ZZDEBUG=1 DO                    
  . SET OPTION("SCRN TOP OFFSET")=22
  . SET OPTION("SCRN HEIGHT")=25
  . SET OPTION("SCRN WIDTH")=130
  ;
  SET OPTION("SHOW INDEX")=0
  ;
  WRITE #
  DO SCROLLER("ARRAY",.OPTION)
  QUIT
  ;
HNDONCURSOR(TMGPSCRLARR,OPTION,INFO)  ;"Part of DEMOCOLS
  ;"Purpose: Handle ON CURSOR event from SCROLLER
  ;"         Event is fired when a cursor key is pressed, before the scroller moves the highlighted line.  
  ;"Input: TMGPSCRLARR,OPTION,INFO -- see documentation in SCROLLER
  ;"       INFO has this:
  ;"          INFO("USER INPUT")=INPUT
  ;"          INFO("CURRENT LINE","NUMBER")=number currently highlighted line
  ;"          INFO("CURRENT LINE","TEXT")=Text of currently highlighted line
  ;"          INFO("CURRENT LINE","RETURN")=return value of currently highlighted line
  ;"          INFO("CURSOR")=Cursor Key pressed
  ;"          INFO("CURSOR","HANDLED")=1 <-- this is what called code should set if it handled
  ;"                          the cursor event. This will prevent scroller from acting on it.     
  NEW CURSOR SET CURSOR=$GET(INFO("CURSOR"))
  NEW ACTIVECOL SET ACTIVECOL=+$GET(OPTION("COLUMNS","ACTIVE"),1)
  NEW MAXCOLS SET MAXCOLS=+$GET(OPTION("COLUMNS","NUM"),1)
  IF 1=1,CURSOR="RIGHT" DO  
  . IF ACTIVECOL>=MAXCOLS QUIT
  . SET ACTIVECOL=ACTIVECOL+1
  . SET TMGSCLRMSG="FULL"  
  . SET OPTION("COLUMNS","ACTIVE")=ACTIVECOL
  . SET INFO("CURSOR","HANDLED")=1
  . ;"NOTE: see also DEMODYNCOLS for ADD COLUMN function
  IF 1=1,CURSOR="LEFT" DO  
  . IF ACTIVECOL=1 QUIT
  . SET ACTIVECOL=ACTIVECOL-1
  . SET TMGSCLRMSG="FULL"  
  . SET OPTION("COLUMNS","ACTIVE")=ACTIVECOL
  . SET INFO("CURSOR","HANDLED")=1
  IF OPTION("COLUMNS","ACTIVE")=2 DO   ;"Ensure data present for column 3
  . NEW IDX1 SET IDX1=$GET(VIEWSTATE("HIGHLINE",1),1)  ;"<---- fix -- shouldn't access viewstate()
  . NEW IDX2 SET IDX2=$GET(VIEWSTATE("HIGHLINE",2),1)
  . IF $GET(INFO("CURSOR"))="DOWN" SET IDX2=IDX2+1
  . IF $GET(INFO("CURSOR"))="UP" SET IDX2=IDX2-1 IF IDX2<1 SET IDX2=1
  . IF $DATA(@TMGPSCRLARR@("COL",3,IDX1,IDX2))>0 QUIT
  . ;"NOTE: see also newer ON LOAD DATA, and ON NEED DATA added later, demo below.  
  . SET @TMGPSCRLARR@("COL",3,IDX1,IDX2,"TXT",1)="Dynamically added data for ["_IDX1_","_IDX2_"]"
  . SET @TMGPSCRLARR@("COL",3,IDX1,IDX2,"TXT",2)=" sample text for ["_IDX1_","_IDX2_"]"
  . SET @TMGPSCRLARR@("COL",3,IDX1,IDX2,"TXT",3)=" sample text for ["_IDX1_","_IDX2_"]"
  . SET TMGSCLRMSG="FULL"
  SET VIEWSTATE("ACTIVECOL")=ACTIVECOL
  QUIT
  ;
HNDONCMD2(TMGPSCRLARR,OPTION,INFO)  ;"Part of DEMOCOLS
  ;"Purpose: handle ON CMD event from SCROLLER
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
HNDONSEL2(TMGPSCRLARR,OPTION,INFO)  ;"Part of DEMOCOLS
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
  ;"============================================================
  ;"============================================================
  ;
DEMODYNCOLS  ;"Dynamically add columns and add on-the-fly text.  
  NEW ARRAY,OPTION,TEMPCLR
  ;"NOTICE: starting out with NO data.  Will load dynamically via ON LOAD DATA. 
  SET OPTION("HEADER",1)=" - < Here is a header line > -"
  SET OPTION("FOOTER",1)="Enter ^ to exit"
  SET OPTION("ON CURSOR")="HNDONCSR3^TMGUSRIF"
  SET OPTION("ON LOAD DATA")="HNDONLOADDATA^TMGUSRIF"
  SET OPTION("ON CHANGING")="HNDONCHANGING^TMGUSRIF"
  DO ADDNICECOLORS(.OPTION,2) 
  ;
  ;"BELOW IS GOOD FOR DEBUGGING
  NEW ZZDEBUG SET ZZDEBUG=1
  IF ZZDEBUG=1 DO                    
  . SET OPTION("SCRN TOP OFFSET")=22
  . SET OPTION("SCRN HEIGHT")=20
  . SET OPTION("SCRN WIDTH")=130
  ;
  DO SCROLLER("ARRAY",.OPTION)
  QUIT
  ;  
ADDRNDTXT(AREF,MODE) ;"Add random Lorem text into @AREF
  NEW LORSTR,STARTWORD SET STARTWORD=$RANDOM(20)+1
  NEW NUMROWS SET NUMROWS=$RANDOM(5)+3
  SET MODE=+$GET(MODE,1)
  NEW AROW FOR AROW=1:1:NUMROWS DO
  . NEW TXT
  . IF AROW=1 SET TXT="Fetched at $H="_$H
  . ELSE  SET TXT=$$GETLOREM^TMGUSRI9($RANDOM(4)+3,.STARTWORD,.LORSTR)
  . IF MODE=1 SET @AREF@(AROW,TXT)=AROW
  . IF MODE=2 SET @AREF@(AROW)=TXT
  QUIT
  ;
HNDONLOADDATA(TMGPSCRLARR,OPTION,INFO)  ;"Part of DEMODYNCOLS   
  ;"  INFO("DATA","SELECTED",<COL#>)=<Selected Index of line>  I.e. what line is selected in column 1, 2, 3, etc.  
  ;"  INFO("DATA","SELECTED",<COL#>,"TEXT")=<Text of selected line>
  ;"          NOTE: <COL#> will be 1..<CUR_COL>-1  I.e. columns to the LEFT of current column.  
  ;"  INFO("DATA","CUR COLUMN")=<COL#> for which data is being needed
  ;"  INFO("DATA","CUR COLUMN","START INDEX")=<LINE#> for beginning of lines of data whicg is needed
  ;"  INFO("DATA","CUR COLUMN","END INDEX")=<LINE#> for end of lines of data which is needed
  ;"  INFO("DATA","HANDLED")=1 if event handler has returned data.  If 0 then data will be obtained from legacy methods
  ;"  INFO("DATA","TEXT",<LINE#>)=<DISPLAY TEXT>  <--- event handler returns data for display here.    ;"                   
  NEW TEMP DO ADDRNDTXT("TEMP")
  NEW AROW SET AROW=1
  NEW IDX FOR IDX=INFO("DATA","CUR COLUMN","START INDEX"):1:INFO("DATA","CUR COLUMN","END INDEX") DO
  . NEW LINE SET LINE=$ORDER(TEMP(AROW,""))
  . SET INFO("DATA","TEXT",IDX)=LINE,AROW=AROW+1
  SET INFO("DATA","HANDLED")=1
  QUIT
  ;
HNDONCHANGING(TMGPSCRLARR,OPTION,INFO)  ;"Part of DEMODYNCOLS
  ;"  INFO("CURRENT LINE","NUMBER")=number currently highlighted line
  ;"  INFO("CURRENT LINE","TEXT")=Text of currently highlighted line
  ;"  INFO("CURRENT LINE","RETURN")=return value of currently highlighted line
  ;"  INFO("NEXT LINE","NUMBER")=next line number. Used for ON CHANGING to show the line about to be selected
  ;"  INFO("NEXT LINE","TEXT")=Text of new line
  ;"  INFO("ALLOW CHANGE")=1, <--- RETURN RESULT.  Change to 0 to disallow move.
  ;
  ;"Prevent scrolling down below lines of data
  IF $GET(INFO("NEXT LINE","TEXT"))="" SET INFO("ALLOW CHANGE")=0
  QUIT
  ;
HNDONCSR3(TMGPSCRLARR,OPTION,INFO)  ;"Part of DEMODYNCOLS 
  ;"Purpose: Handle ON CURSOR event from SCROLLER
  ;"         Event is fired when a cursor key is pressed, before the scroller moves the highlighted line.  
  ;"Input: TMGPSCRLARR,OPTION,INFO -- see documentation in SCROLLER
  ;"       INFO has this:
  ;"          INFO("CURSOR")=Cursor Key pressed
  ;"          INFO("CURSOR","HANDLED")=1 <-- this is what called code should set if it handled
  ;"                          the cursor event. This will prevent scroller from acting on it.     
  NEW CURSOR SET CURSOR=$GET(INFO("CURSOR"))
  IF "LEFT,RIGHT,HOME,END,"'[CURSOR QUIT
  NEW ACTIVECOL SET ACTIVECOL=+$GET(OPTION("COLUMNS","ACTIVE"),1)
  NEW MAXCOLS SET MAXCOLS=+$GET(OPTION("COLUMNS","NUM"),1)
  NEW CMDIDX SET CMDIDX=0
  IF CURSOR="RIGHT" DO  GOTO HCSR3L2
  . IF ACTIVECOL>=MAXCOLS DO  ;"SET UP A NEW COLUMN.
  . . SET CMDIDX=$$ADDDOFN(.INFO,"ADD COLUMN(1)")   ;"1 --> AUTO SET COL WIDTHS
  . . NEW IDX SET IDX=$SELECT((ACTIVECOL+1)#2=0:2,1:1)  ;"If To-be-added column number will be EVEN --> 2, otherwise 1
  . . IF (ACTIVECOL+1)=2 QUIT  ;"COL 2 has colors set up in ADDNICECOLORS, called in DEMODYNCOLS
  . . NEW DATAREF SET DATAREF=$NAME(INFO("DO FUNCTION",CMDIDX,"DATA"))
  . . IF IDX=1 MERGE @DATAREF@("COLORS")=OPTION("COLORS")
  . . ELSE  MERGE @DATAREF@("COLORS")=OPTION("COLUMNS",IDX,"COLORS")
  . ELSE  SET ACTIVECOL=ACTIVECOL+1
  IF CURSOR="LEFT" DO  GOTO HCSR3L2
  . IF ACTIVECOL>1 DO
  . . SET ACTIVECOL=ACTIVECOL-1
  IF CURSOR="END" DO  GOTO HCSR3L2
  . SET CMDIDX=$$ADDDOFN(.INFO,"SCROLL VIEWPORT(1)")  
  IF CURSOR="HOME" DO  GOTO HCSR3L2
  . SET CMDIDX=$$ADDDOFN(.INFO,"SCROLL VIEWPORT(-1)")
HCSR3L2 ;  
  SET TMGSCLRMSG="FULL"  
  SET OPTION("COLUMNS","ACTIVE")=ACTIVECOL
  SET INFO("CURSOR","HANDLED")=1
  QUIT  
  ;
ADDDOFN(INFO,NAME) ;"ADD DO FUNCTION
  NEW IDX SET IDX=$ORDER(INFO("DO FUNCTION",""),-1)+1
  SET INFO("DO FUNCTION",IDX)=NAME
  QUIT IDX
  ;"============================================================
  ;"============================================================
  ;
DEMORTDATA  ;"Data provided at runtime, not stored in SCROLLER  
  NEW ARRAY,OPTION,TEMPCLR
  ;"NOTICE: starting out with NO data.  Will get dynamically via ON NEED DATA. 
  SET OPTION("HEADER",1)=" - < Here is a header line > -"
  SET OPTION("FOOTER",1)="Enter ^ to exit"
  SET OPTION("ON NEED DATA")="HNDONNEEDDATA^TMGUSRIF"
  DO ADDNICECOLORS(.OPTION,1) 
  ;
  ;"BELOW IS GOOD FOR DEBUGGING
  NEW ZZDEBUG SET ZZDEBUG=1
  IF ZZDEBUG=1 DO                    
  . SET OPTION("SCRN TOP OFFSET")=22
  . SET OPTION("SCRN HEIGHT")=20
  . SET OPTION("SCRN WIDTH")=130
  ;
  DO SCROLLER("ARRAY",.OPTION)
  QUIT
  ;  
HNDONNEEDDATA(TMGPSCRLARR,OPTION,INFO)  ;"Part of DEMORTDATA  
  ;"  INFO("DATA","SELECTED",<COL#>)=<Selected Index of line>  I.e. what line is selected in column 1, 2, 3, etc.  
  ;"  INFO("DATA","SELECTED",<COL#>,"TEXT")=<Text of selected line>
  ;"          NOTE: <COL#> will be 1..<CUR_COL>-1  I.e. columns to the LEFT of current column.  
  ;"  INFO("DATA","CUR COLUMN")=<COL#> for which data is being needed
  ;"  INFO("DATA","CUR COLUMN","START INDEX")=<LINE#> for beginning of lines of data whicg is needed
  ;"  INFO("DATA","CUR COLUMN","END INDEX")=<LINE#> for end of lines of data which is needed
  ;"  INFO("DATA","HANDLED")=1 if event handler has returned data.  If 0 then data will be obtained from legacy methods
  ;"  INFO("DATA","TEXT",<LINE#>)=<DISPLAY TEXT>  <--- event handler returns data for display here.    ;"                   
  NEW STARTIDX SET STARTIDX=INFO("DATA","CUR COLUMN","START INDEX")
  NEW ENDIDX SET ENDIDX=INFO("DATA","CUR COLUMN","END INDEX")
  NEW TEMP DO ZWR2ARR($NAME(^TIU(8925)),STARTIDX,(ENDIDX-STARTIDX+1),"TEMP",STARTIDX)
  NEW IDX FOR IDX=STARTIDX:1:ENDIDX DO
  . NEW LINE SET LINE=$GET(TEMP(IDX))
  . SET INFO("DATA","TEXT",IDX)=LINE
  SET INFO("DATA","HANDLED")=1
  QUIT
  ;
ZWR2ARR(NAME,OFFSETIDX,COUNT,OUTREF,STARTIDX) ;"//kt added this function
  ;"COPIED FROM TMGZWR and MODIFIED FOR DEMO PURPOSES.  **DON'T** USE THIS AS A GENERAL UTILITY FUNCTION
  ;"Purpose: Output ZWR output into an array
  ;"Input: NAME --   PASS BY NAME.  Should be a a closed reference.
  ;"       OFFSETIDX -- OPTIONAL.  DEFAULT is 0
  ;"          If > 0 then this is how many lines to skip over.
  ;"       COUNT -- NUMBER OF LINES TO RETURN.  
  ;"       OUTREF -  PASS BY NAME.  Name of array to put output into.
  ;"       STARTIDX -- OPTIONAL.  Starting index #.  Default=1
  ;"Output:  @OUTREF@(1)=First line
  ;"         @OUTREF@(2)=2nd line... etc. 
  ;"Results: None
  ;"Note: lvn and gvn are both supported. 
  ;"      ':' syntax is not supported (yet)
  SET OFFSETIDX=+$GET(OFFSETIDX)
  SET COUNT=+$GET(COUNT,10)
  NEW L SET L=$LENGTH(NAME) ;" Name length 
  IF $EXTRACT(NAME,L-2,L)=",*)" SET NAME=$EXTRACT(NAME,1,L-3)_")"
  NEW ORIGLAST SET ORIGLAST=$QSUBSCRIPT(NAME,$QLENGTH(NAME))
  NEW ORIGQL SET ORIGQL=$QLENGTH(NAME)
  SET STARTIDX=+$GET(STARTIDX) IF STARTIDX'>0 SET STARTIDX=1
  NEW IDX SET IDX=STARTIDX
  IF $DATA(@NAME)#2 DO
  . IF OFFSETIDX>0 SET OFFSETIDX=OFFSETIDX-1 QUIT
  . IF COUNT<1 QUIT
  . SET @OUTREF@(IDX)=NAME_"="_$$FORMAT^TMGZWR(@NAME),IDX=IDX+1,COUNT=COUNT-1
  FOR  SET NAME=$QUERY(@NAME) Q:(NAME="")!($QSUBSCRIPT(NAME,ORIGQL)'=ORIGLAST)!(COUNT<1)  DO
  . IF OFFSETIDX>0 SET OFFSETIDX=OFFSETIDX-1 QUIT
  . IF COUNT<1 QUIT
  . SET @OUTREF@(IDX)=NAME_"="_$$FORMAT^TMGZWR(@NAME),IDX=IDX+1,COUNT=COUNT-1
  QUIT 
  ;