TMGUSRI7 ;TMG/kst/USER INTERFACE -- Menu Display Code ;3/31/24
         ;;1.0;TMG-LIB;**1**;3/31/24
 ;
 ;"TMG USER INTERFACE API FUNCTIONS
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 3/31/24  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"RUNMENU(MENU,RUNOPT) ;  <-- THIS IS THE PRIMARY FUNCTION TO USE.  
 ;"ERASEALL(MENU) --ERASE all menus
 ;"DRAWALL(MENU) -- Draw all active menus
 ;"DRAWMENU(IDX,MENU)  -- Draw one menu box.   
 ;"=======================================================================
 ;"Demo Functions
 ;"=======================================================================
 ;"TEST1(MENU,X,Y) -- Draw 1 menu and quit
 ;"TEST2() -- Draw menu and interact with user.  
 ; 
 ;"=======================================================================
 ;"Private Functions
 ;"=======================================================================
 ;"HNDLKEY(KEY,KEYOPT,MENU)  ;"Callback for RUNMENU
 ;"HNDLCSR(NAME,KEYOPT,MENU) ;"Callback for RUNMENU     
 ;"MOVCSR(DIR,MENU,KEYOPT) ;"Move the cursor, the selected line (SELLINE) in the menu, changing the state of the menu system.  
 ;"MENUINFO(IDX,MENU,OUT) ;"Get info about menu  
 ;"SETCOLORS(MODE,ARR) ;" Set color
 ;"DEFAULTCLR(ARR)  ;"Get default color array
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;"DEPENDENCIES:      
 ;"=======================================================================
 ;
  ;"===== DEMO ================================================================
TEST1(MENU,X,Y) ;"Draw 1 menu and quit
  DO TESTSETUP(.MENU,.X,.Y)
  DO DRAWMENU(1,.MENU)  
  QUIT
  ;
TEST2(USESCRN) ;"Draw menu and interact with user.  
  NEW MENU
  DO TESTSETUP(.MENU,1,1) ;
  NEW RUNOPT
  IF +$GET(USESCRN) SET RUNOPT("ON PREDRAW")="HNDLPREDRAW^TMGUSRI7"
  NEW RESULT SET RESULT=$$RUNMENU(.MENU,.RUNOPT)  
  QUIT RESULT
  ;
TESTSETUP(MENU,X,Y) ;
  NEW TEMP      
  SET MENU(1,"TOP MENU BAR")=1
  SET MENU(1,1)="&File"
  SET MENU(1,1,"SUBMENU")=2  ;"link to menu #2
  SET MENU(1,2)="&Edit"
  SET MENU(1,2,"SUBMENU")=5  ;"link to menu #5
  SET MENU(1,3)="&Search"  
  SET MENU(1,3,"SUBMENU")=6  ;"link to menu #6
  SET MENU(1,"POS","X")=+$GET(X,1)
  SET MENU(1,"POS","Y")=+$GET(Y,25)
  ;" ---File menu -------------
  SET MENU(2,1)="&File"
  SET MENU(2,2)="O&pen"
  SET MENU(2,3)="&Save"
  SET MENU(2,4)="&Quit"
  SET MENU(2,5,"HLINE")=1
  SET MENU(2,6)="P&references"
  SET MENU(2,6,"ICON")="$25CE"  ;"bullseye char.  
  SET MENU(2,7)="Online"
  SET MENU(2,7,"CHECKED")=1
  SET MENU(2,8)="Available"
  SET MENU(2,8,"CHECKED")=0
  SET MENU(2,8,"DATA")="ABC123"
  SET MENU(2,9)="PLAIN-JANE"
  SET MENU(2,"POS","X")=+$GET(X,5)
  SET MENU(2,"POS","Y")=+$GET(Y,25)
  SET MENU(2,"SELLINE")=1  
  SET MENU(2,3,"SUBMENU")=3  ;"link to 2nd menu
  ;" ----------------
  SET MENU(3,1)="Save to &Default"
  SET MENU(3,2)="Save &As"
  SET MENU(3,2,"SUBMENU")=4  ;"link to 3rd menu
  SET MENU(3,3)="&AutoSave"  
  SET MENU(3,"SELLINE")=1
  ;" ----------------
  SET MENU(4,1)="Save as &BMP"
  SET MENU(4,2)="Save as &PNG"
  SET MENU(4,3)="Save as &TIFF"  
  SET MENU(4,"SELLINE")=1
  ;" -- Edit menu
  SET MENU(5,1)="Undo"
  SET MENU(5,2)="Redo"
  SET MENU(5,3)="Copy"
  SET MENU(5,4)="Paste"
  ;" -- Find menu
  SET MENU(6,1)="Find"
  SET MENU(6,2)="Find Next"
  SET MENU(6,3)="Find Prev"  
  ;"-- Menu drawing options
  SET MENU("OPTION","COLOR","BORDER")=$$COLORPAIR^TMGTERM("RED","BLUE",.TEMP) 
  SET MENU("OPTION","COLOR","TEXT")=$$COLORPAIR^TMGTERM("WHITE","BLUE",.TEMP) 
  SET MENU("OPTION","COLOR","SELTEXT")=$$COLORPAIR^TMGTERM("BLUE","GREY",.TEMP) 
  SET MENU("OPTION","COLOR","ALTKEY")=$$COLORPAIR^TMGTERM("RED","BLACK",.TEMP) 
  SET MENU("OPTION","COLOR","SELALTKEY")=$$COLORPAIR^TMGTERM("RED","GREY",.TEMP)  
  SET MENU("FOCUS IDX")=1  
  QUIT
  ;
HNDLPREDRAW(MENU) ;
  DO DRAWTESTSCRN(60,20)
  QUIT 1
  ;"=======================================================================
RUNMENU(MENU,RUNOPT) ;
  ;"INPUT: MENU -- PASS BY REFERENCE.  Array with Menu info.  See DRAWMENU for format
  ;"       RUNOPT -- OPTIONAL.  PASS BY REFERENCE.  Format:
  ;"            RUNOPT("ON KEY")    - Optional. "MYFUN^MYRTN"  Must take 3 parameter, e.g. $$MYFUN^MYRTN(.KEY,.OPTION,.USERDATA)
  ;"                   Fires after every key enter.  If function returns "^", then loop is exited.  
  ;"            RUNOPT("ON CURSOR") - Optional. "MYFUN^MYRTN"  Must take 3 parameters, e.g. $$MYFUN^MYRTN(.NAME,.OPTION,.USERDATA)
  ;"                   Fires if cursor direction key entered.
  ;"            RUNOPT("ON PREDRAW") -- - Optional. "MYFUN^MYRTN"  Must take 1 parameters, e.g. $$MYFUN^MYRTN(.MENU)
  ;"                   Fires just before refresh drawing. This can be used to redraw the underlying windows, which menus may have messed up.
  ;"                   +Result of function should be 1, if screen drawn.  
  NEW TMGDEVSAVE,TMGDEVINFO DO DEV2ARR^TMGKERN1($IO,.TMGDEVSAVE,,.TMGDEVINFO)
  SET RUNOPT("EOFF")=1
  IF $DATA(RUNOPT("ON KEY"))=0 SET RUNOPT("ON KEY")="HNDLKEY^TMGUSRI7"
  IF $DATA(RUNOPT("ON CURSOR"))=0 SET RUNOPT("ON CURSOR")="HNDLCSR^TMGUSRI7"
  DO MOVCSR("INIT",.MENU,.RUNOPT)  ;"will trigger DO DRAWALL(.MENU) --> Draw all active menus
  NEW RESULT SET RESULT=$$RUNKEYS^TMGUSRI5(.RUNOPT,.MENU)  
  IF $PIECE(RESULT,"^",2)="DONE" SET RESULT=$PIECE(RESULT,"^",3,4)      
  NEW SKIPERASE SET SKIPERASE=+$$EVENT(.RUNOPT,"ON PREDRAW",1,.MENU)  ;"Redraw underlying screen, if possible.
  IF SKIPERASE=0 DO ERASEALL(.MENU) 
  DO RESTORDEV^TMGKERN1(.TMGDEVSAVE,.TMGDEVINFO)  
  QUIT RESULT
  ;
HNDLKEY(KEY,KEYOPT,MENU)  ;"Callback for RUNMENU
  ;"Input: KEYOPT -- this is the OPTION passed to RUNKEYS  
  ;"       MENU -- the array with data for drawing menus. 
  SET KEY=$$UP^XLFSTR($GET(KEY))
  NEW RESULT SET RESULT="1^OK"
  IF KEY="<CR>" DO  GOTO HKDN
  . NEW CURIDX SET CURIDX=+$GET(MENU("FOCUS IDX"))
  . NEW SELLINE SET SELLINE=+$GET(MENU(CURIDX,"SELLINE"))
  . NEW SUBMENUIDX SET SUBMENUIDX=+$GET(MENU(CURIDX,SELLINE,"SUBMENU"))
  . IF SUBMENUIDX>0 DO  QUIT  ;"An ENTER on a submenu will select and move to submenu, not allowing selection of current line. 
  . . IF $$MOVCSR("RIGHT",.MENU,.KEYOPT) 
  . NEW VAL SET VAL=$GET(MENU(CURIDX,SELLINE))
  . SET VAL=$TRANSLATE(VAL,"&","")
  . SET RESULT="-1^DONE^"_VAL
  . NEW DATA SET DATA=$GET(MENU(CURIDX,SELLINE,"DATA"))
  . IF DATA'="" SET RESULT=RESULT_"^"_DATA
  IF KEY="^" DO  GOTO HKDN
  . SET RESULT="-1^ABORT"
  IF $EXTRACT(KEY,1)="<",$LENGTH(KEY)>1 QUIT  ;"ignore <DIR> KEYS  
  NEW CHANGED  SET CHANGED=$$MOVCSR("ACEL-"_KEY,.MENU,.KEYOPT)
  ;
HKDN ;  
  QUIT RESULT
  ;
HNDLCSR(NAME,KEYOPT,MENU) ;"Callback for RUNMENU     
  ;"Input: KEYOPT -- this is the OPTION passed to RUNKEYS  
  NEW CHANGED SET CHANGED=$$MOVCSR(NAME,.MENU,.KEYOPT)  ;"This will trigger repaint if needed.  
  QUIT "1^OK"
  ;  
MOVCSR(DIR,MENU,KEYOPT) ;"Move the cursor, the selected line (SELLINE) in the menu, changing the state of the menu system.  
  ;"Input: DIR -- Direction to move cursor.  Should be UP, DOWN, PREV (for page Up), NEXT (for page down), LEFT, or RIGHT
  ;"              or 'ACEL-<letter>'
  ;"               If active menu, at the point of the current cursor (SELLINE) does not have SUBMENU, then RIGHT is ignored. 
  ;"               If active menu does not have a PARENT node, then LEFT is ignored.
  ;"               If active menu does not have a matching accelerator key, then ACEL-<letter> is ignored.  
  ;"       MENU -- Array with menus.  See format in DRAWMENU
  ;"NOTE: This change the state of the menus.  It determines what menus to open as submenus etc, or to close.  
  ;"      It then calls DRAWALL to draw (or erase) all the menus
  ;"RESULT: 1 if something changed, and redraw done, otherwise 0
  ;
  ;"First determine the current state.  Do we have submenus open etc?
  NEW IDX SET IDX=+$GET(MENU("FOCUS IDX"))
  IF IDX=0 SET IDX=$ORDER(MENU("")) SET MENU("FOCUS IDX")=IDX  ;"If not passed, default to first index.
  NEW SELLINE SET SELLINE=+$GET(MENU(IDX,"SELLINE"))  ;"if 0, then menu doesn't show cursor/selected line
  NEW INITSELLINE SET INITSELLINE=SELLINE
  NEW SUBMENUIDX SET SUBMENUIDX=+$GET(MENU(IDX,SELLINE,"SUBMENU"))
  NEW SUBMENUOPEN SET SUBMENUOPEN=($GET(MENU(SUBMENUIDX,"PARENT"))=IDX)
  NEW PARENTIDX SET PARENTIDX=+$GET(MENU(IDX,"PARENT"))
  NEW TOPMENUBAR SET TOPMENUBAR=($GET(MENU(IDX,"TOP MENU BAR"))=1)
  NEW CLOSESUBMENU SET CLOSESUBMENU=0
  IF TOPMENUBAR DO  ;"convert keys so that horizontal actions of menu bar are line vertical movement for dropdown menus
  . IF DIR="LEFT" SET DIR="UP" QUIT
  . IF DIR="RIGHT" SET DIR="DOWN" QUIT 
  . IF DIR="DOWN" SET DIR="RIGHT" QUIT
  . IF DIR="UP" SET DIR="LEFT" QUIT
  IF DIR="INIT" DO   GOTO MVC2  ;"Trigger opening of any submenu during initialization.
  . SET REFRESH=2,UPDN=1  
  . IF SELLINE=0 SET SELLINE=1
  ;"Now move cursor(SELLINE)
  NEW REFRESH SET REFRESH=0  ;"Default.  0-> don't draw.  1-> draw menu, 2->draw background screen.    
  NEW MENUMAX SET MENUMAX=$ORDER(MENU(IDX,"@"),-1)
  NEW UPDN SET UPDN=0  ;"Boolean for if cursor/selline moved up or down.
  IF DIR="UP",($GET(MENU(PARENTIDX,"TOP MENU BAR"))=1),(SELLINE=1) DO
  . SET DIR="LEFT"  ;"Allow escape 'up' to top menu bar.
  . SET REFRESH=2
  IF DIR="UP" DO  GOTO MVC2
  . NEW SL2 SET SL2=$$DELTA^TMGMISC(SELLINE,-1,1,MENUMAX)
  . IF SL2=SELLINE QUIT
  . SET SELLINE=SL2,REFRESH=1,UPDN=1
  IF DIR="DOWN" DO  GOTO MVC2
  . NEW SL2 SET SL2=$$DELTA^TMGMISC(SELLINE,+1,1,MENUMAX)
  . IF SL2=SELLINE QUIT
  . SET SELLINE=SL2,REFRESH=1,UPDN=1
  IF DIR="PREV" DO  GOTO MVC2
  . NEW SL2 SET SL2=1
  . IF SL2=SELLINE QUIT
  . SET SELLINE=SL2,REFRESH=1,UPDN=1
  IF DIR="NEXT" DO  GOTO MVC2
  . NEW SL2 SET SL2=MENUMAX
  . IF SL2=SELLINE QUIT
  . SET SELLINE=SL2,REFRESH=1,UPDN=1
  IF DIR="LEFT" DO  GOTO MVC2
  . QUIT:PARENTIDX=0
  . SET MENU("FOCUS IDX")=PARENTIDX  ;"Change focus to parent menu.
  . ;"check if submenu open that must be closed before closing this one
  . IF SUBMENUOPEN SET CLOSESUBMENU=1,REFRESH=2
  . ELSE  SET REFRESH=1
  IF DIR="RIGHT" DO  GOTO MVC2
  . IF (SUBMENUIDX=0)!($DATA(MENU(SUBMENUIDX))=0) QUIT
  . SET MENU(SUBMENUIDX,"PARENT")=IDX
  . SET MENU(SUBMENUIDX,"SELLINE")=1
  . SET MENU("FOCUS IDX")=SUBMENUIDX  ;"Change focus to child submenu.  
  . SET REFRESH=1
  IF DIR["ACEL-" DO  GOTO MVC2
  . NEW KEY SET KEY=$PIECE(DIR,"ACEL-",2)
  . NEW ACELKEYS MERGE ACELKEYS=MENU(IDX,"ACELKEYS")
  . IF $DATA(ACELKEYS(KEY)) DO
  . . NEW ALINE SET ALINE=""
  . . FOR  SET ALINE=+$ORDER(ACELKEYS(KEY,ALINE)) QUIT:(ALINE'>0)!(UPDN=1)  DO
  . . . IF ALINE=SELLINE QUIT   ;"If menu has duplicate accel keys, and already on line with accel key, then keep searching                
  . . . SET SELLINE=ALINE,REFRESH=1,UPDN=1
  . . . IF SUBMENUOPEN SET REFRESH=2
  GOTO MVCDN
MVC2 ;  
  IF UPDN=1 DO
  . SET MENU(IDX,"SELLINE")=SELLINE
  . IF SUBMENUOPEN SET CLOSESUBMENU=1  ;"Moving away from line that had an open submenu.  So need to close submenu
  . NEW SUBIDX2 SET SUBIDX2=+$GET(MENU(IDX,SELLINE,"SUBMENU"))  ;"SELLINE is the NEW line that cursor/SELLINE has moved to
  . IF SUBIDX2>0 DO  ;"Now that SELLINE as moved, it is now at a submenu line.  So need to open submenu
  . . SET MENU(SUBIDX2,"PARENT")=IDX
  . . SET MENU(SUBIDX2,"DRAW MODE")=1
  . . ;"Determine positioning of submenu
  . . NEW INFO DO MENUINFO(IDX,.MENU,.INFO) ;"Get info about menu
  . . NEW X,Y
  . . SET X=+$GET(MENU(IDX,"POS","X"))
  . . SET Y=+$GET(MENU(IDX,"POS","Y"))
  . . IF TOPMENUBAR DO
  . . . NEW OFFSET SET OFFSET=+$GET(INFO("HORIZ",SELLINE))
  . . . SET X=X+OFFSET+1
  . . . SET Y=Y+1
  . . ELSE  DO
  . . . SET X=X+$GET(INFO("WT"))+2
  . . . NEW JDX SET JDX=0
  . . . FOR  SET JDX=$ORDER(MENU(IDX,JDX)) SET Y=Y+1 QUIT:(JDX'>0)!(JDX=SELLINE)
  . . . SET Y=Y-1
  . . SET MENU(SUBIDX2,"POS","X")=X
  . . SET MENU(SUBIDX2,"POS","Y")=Y
  . . IF REFRESH<1 SET REFRESH=1  ;"Probably already set, but set again to be sure.  
  IF CLOSESUBMENU DO
  . SET MENU(SUBMENUIDX,"DRAW MODE")=-1   ;"-1 means ERASE the prior show submenu
  . KILL MENU(SUBMENUIDX,"PARENT")
  . SET REFRESH=2    
  NEW SKIPERASE SET SKIPERASE=1
  IF REFRESH=2 SET SKIPERASE=+$$EVENT(.KEYOPT,"ON PREDRAW",1,.MENU)  ;"Returns 1 if screen redrawn
  IF REFRESH>0 DO DRAWALL(.MENU,SKIPERASE)
MVCDN ;
  QUIT REFRESH
  ;
ERASEALL(MENU) ;"ERASE all menus
  ;"Input: MENU -- Array with menus.  See format in DRAWMENU
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(MENU(IDX)) QUIT:IDX'>0  DO
  . SET MENU(IDX,"DRAW MODE")=-1  ;"-1 means ERASE
  DO DRAWALL(.MENU)
  QUIT 
  ;
DRAWALL(MENU,SKIPERASE) ;"Draw all active menus
  ;"Input: MENU -- Array with menus and options.  See format in DRAWMENU
  ;"NOTE: This does not change the state of any menus.  It does not determine what
  ;"      menus to open as submenus etc, or to close.  It just draws (or erases) all the menus,
  ;"      marked for drawing, or erasing.
  SET SKIPERASE=+$GET(SKIPERASE)
  NEW ATLEAST1 SET ATLEAST1=0
  NEW IDX SET IDX=0
  ;"First draw all erasures
  IF SKIPERASE=0 FOR  SET IDX=$ORDER(MENU(IDX)) QUIT:IDX'>0  DO
  . NEW DRAWMODE SET DRAWMODE=$GET(MENU(IDX,"DRAW MODE"))
  . IF (DRAWMODE="")!(DRAWMODE>-1) QUIT  ;"-1 means erase.  Only consider erase for now.  
  . DO DRAWMENU(IDX,.MENU)  
  ;"Next draw all menus set for draw
  SET IDX=0
  FOR  SET IDX=$ORDER(MENU(IDX)) QUIT:IDX'>0  DO
  . NEW DRAWMODE SET DRAWMODE=$GET(MENU(IDX,"DRAW MODE"))
  . IF DRAWMODE=-1 QUIT    ;"-1 means erase.  Erases already done, so ignore here
  . IF DRAWMODE=1 SET ATLEAST1=1
  . IF DRAWMODE="",ATLEAST1=0 SET DRAWMODE=1,MENU(IDX,"DRAW MODE")=1,ATLEAST1=1  ;"Show at least 1 menu
  . IF +DRAWMODE=0 QUIT  ;"0 --> don't draw
  . DO DRAWMENU(IDX,.MENU)  
  QUIT 
  ; 
EVENT(OPTION,NAME,NUMPARAMS,PARAM1,PARAM2,PARAM3,PARAM4) ;"Fire event, if handler defined.   
  NEW RESULT SET RESULT=$$EVENT^TMGUSRI5(.OPTION,.NAME,.NUMPARAMS,.PARAM1,.PARAM2,.PARAM3,.PARAM4) ;"Fire event, if handler defined.
  IF RESULT="" SET RESULT=0
  ELSE  SET RESULT=1_"^"_RESULT
  QUIT RESULT
  ;
DRAWMENU(IDX,MENU)  ;"Draw one menu box.   
  ;"Input: IDX -- index in MENU to draw
  ;"       MENU -- PASS BY REFERENCE.  Format:
  ;"          MENU(IDX,<line#>)=<DisplayText>  E.g. "&File"  <-- '&' triggers underline and bold
  ;"          MENU(IDX,<line#>,"SUBMENU")=<Menu#> This number corresponds to IDX number of a different menu in same MENU array.  If TOP MENU BAR, every entry should have SUBMENU
  ;"          MENU(IDX,<line#>,"ICON")=<$F3A4>  <-- Optional UNICODE codepoint (char number) for icon symbol.  Ignored if TOP MENU BAR
  ;"          MENU(IDX,<line#>,"CHECKED")=0 or 1 <-- Optional, if 1 checked box shown $2611, if 0, unchecked box shown $2601    Ignored if TOP MENU BAR
  ;"          MENU(IDX,<line#>,"HLINE")=1 <-- Optional, If 1 draw horizontal line (OVERRIDES any DisplayText)   Ignored if TOP MENU BAR
  ;"          MENU(IDX,<line#>,"COLOR","TEXT")="FG^BG"     <-- optional, overrides global colors.
  ;"          MENU(IDX,<line#>,"COLOR","SELTEXT")="FG^BG"  <-- optional, overrides global colors.
  ;"          MENU(IDX,<line#>,"COLOR","ALTKEY")="FG^BG"   <-- optional. overrides global colors.  If text is "&File", then F will be ALTKEY color
  ;"          MENU(IDX,<line#>,"COLOR","SELALTKEY")="FG^BG"   <-- optional. overrides global colors.  If text is "&File", then F will be ALTKEY color
  ;"              Note: FG and BG should be color numbers, see COLORPAIR^TMGTERM for details.  
  ;"          MENU(IDX,<line#>,"DATA")="abc123"> <-- Optional data for user use for this line.
  ;"          MENU(IDX,"TOP MENU BAR")=1 <-- This is if this menu is for the top menu bar.  Should only be 1 of these.  
  ;"          MENU(IDX,"DATA")="abc123"> <-- Optional data for user use for this menu 
  ;"          MENU(IDX,"ACELKEYS",<CHAR>,<line#>)=""  <-- this will be automatically generated as MENU is parsed.  
  ;"          MENU(IDX,"POS","X")=<X POSITION> of top-left  
  ;"          MENU(IDX,"POS","Y")=<Y POSITION> of top-left  
  ;"          MENU(IDX,"SELLINE")=<line#> of selection of menu  
  ;"          MENU(IDX,"DRAW MODE")= -1 -> erase menu, 0 -> don't draw, 1 -> draw menu    
  ;"          MENU(IDX,"PARENT")=#  This is created when this menu IDX is a sub menu.  And # is the IDX of the parent.  
  ;"          MENU("FOCUS IDX")=#  <-- Which menu IDX is the active one, the one that has input focus.  Generated automatically by MOVCSR() if not passed. 
  ;"          MENU("OPTION","COLOR","BORDER")="FG^BG"   -- optional.  Default is FG=BLACK,BG=WHITE
  ;"          MENU("OPTION","COLOR","TEXT")="FG^BG"     -- optional.  Default is FG=BLACK,BG=WHITE
  ;"          MENU("OPTION","COLOR","SELTEXT")="FG^BG"  -- optional.  Default is FG=WHITE,BG=BLACK
  ;"          MENU("OPTION","COLOR","ALTKEY")="FG^BG"   -- optional.  Default is FG=WHITE,BG=BLACK  If text is "&File", then F will be ALTKEY color 
  ;"          MENU("OPTION","COLOR","SELALTKEY")="FG^BG"   -- optional.  Default is FG=WHITE,BG=BLACK  If text is "&File", then F will be ALTKEY color 
  DO VCUSAV2^TMGTERM 
  SET IDX=+$GET(IDX)              
  IF $GET(MENU("FOCUS IDX"))="" SET MENU("FOCUS IDX")=IDX
  NEW INFO DO MENUINFO(IDX,.MENU,.INFO) ;"Get info about menu   
  IF INFO("TOP MENU BAR") DO DRAWBARMENU(.IDX,.MENU) QUIT ;"<--- If TOP MENU BAR, use separate code and then quit.   
  SET INFO("THICK")=1,INFO("ARC")=1  ;"Add settings for how to draw box border of menu
  NEW WT,HT SET WT=INFO("WT"),HT=INFO("HT") ;"HT AND WT are for size of contents.  Will need to expand for size of surrounding box
  IF $DATA(MENU("OPTION","COLOR"))=0 DO
  . NEW TEMP DO DEFAULTCLR(.TEMP) MERGE MENU("OPTION","COLOR")=TEMP KILL TEMP
  NEW X SET X=+$GET(MENU(IDX,"POS","X")) IF X<1 SET X=1
  NEW Y SET Y=+$GET(MENU(IDX,"POS","Y")) IF Y<1 SET Y=1
  NEW FOCUSED SET FOCUSED=(IDX=+$GET(MENU("FOCUS IDX")))
  NEW SELLINE SET SELLINE=+$GET(MENU(IDX,"SELLINE"))
  IF FOCUSED,SELLINE=0 SET SELLINE=1,MENU(IDX,"SELLINE")=1
  NEW COLARR MERGE COLARR=MENU("OPTION","COLOR")  
  NEW ERASE SET ERASE=INFO("ERASE")
  IF ERASE DO  
  . DO VTATRIB^TMGTERM(0) ;"clear colors for box
  . SET MENU(IDX,"DRAW MODE")=0  ;"turn off erase mode for future cycles, since handling now.
  ELSE  DO SETCOLORS("BORDER",.COLARR)
  DO DRAWBOX^TMGTERM2(X,Y,WT+2,HT+2,.INFO)  
  NEW USINGSUBMENU SET USINGSUBMENU=INFO("USING SUBMENU")
  NEW CURX,CURY SET CURX=X+1,CURY=Y
  ;"Now fill in text (or erase if in erase mode)
  NEW JDX SET JDX=0
  FOR  SET JDX=$ORDER(MENU(IDX,JDX)) QUIT:JDX'>0  DO  
  . NEW POS SET POS=1
  . SET CURY=CURY+1
  . NEW STR SET STR=$GET(MENU(IDX,JDX))
  . NEW HLINE SET HLINE=+$GET(MENU(IDX,JDX,"HLINE"))    
  . NEW ICON SET ICON=$GET(MENU(IDX,JDX,"ICON"))
  . NEW CHECK SET CHECK=$GET(MENU(IDX,JDX,"CHECKED"))
  . NEW SUBMENU SET SUBMENU=$SELECT(USINGSUBMENU:+$GET(MENU(IDX,JDX,"SUBMENU")),1:0)
  . DO CUP^TMGTERM(CURX,CURY)
  . IF ERASE DO  QUIT 
  . . DO VTATRIB^TMGTERM(0)
  . . WRITE $$LJ^XLFSTR("",WT," ")
  . NEW COLREF2 SET COLREF2=$NAME(MENU(IDX,JDX,"COLOR"))
  . NEW COLARR2 MERGE COLARR2=COLARR
  . MERGE COLARR2=MENU(IDX,JDX,"COLOR")  ;"allow override of global colors if present.
  . NEW MODE SET MODE=$SELECT((JDX=SELLINE)&FOCUSED:"SELTEXT",1:"TEXT")
  . DO SETCOLORS(MODE,.COLARR2)  ;" establish colors for selection, background etc.  
  . NEW MAX SET MAX=WT 
  . IF USINGSUBMENU SET MAX=MAX-2
  . IF INFO("USING ICON") DO
  . . SET POS=POS+1
  . . IF ICON="" WRITE " " QUIT
  . . DO UTF8WRITE^TMGSTUTL(ICON)
  . IF INFO("USING CHECK") DO
  . . SET POS=POS+2
  . . IF CHECK="" WRITE "  " QUIT
  . . NEW CHECKCHAR SET CHECKCHAR=$SELECT(CHECK=1:"$2611",1:"$2610")
  . . DO UTF8WRITE^TMGSTUTL(CHECKCHAR) 
  . . WRITE " "
  . IF HLINE DO  
  . . FOR POS=POS:1:MAX DO UTF8WRITE^TMGSTUTL("$2500")  ;"$2500^Light Horizontal
  . . SET POS=POS+1  ;"not sure why needed, but doesn't work if I don't
  . . SET STR=""
  . IF STR["&" DO
  . . NEW S1,S2 SET S1=$PIECE(STR,"&",1),S2=$PIECE(STR,"&",2)
  . . NEW L1,L2 SET L1=$LENGTH(S1),L2=$LENGTH(S2)
  . . WRITE S1
  . . SET POS=POS+$LENGTH(S1)
  . . NEW ALTMODE SET ALTMODE=$SELECT(MODE["SEL":"SELALTKEY",1:"ALTKEY")
  . . DO SETCOLORS(ALTMODE,.COLARR2)
  . . DO VTATRIB^TMGTERM(4)  ;"turn on underline
  . . NEW ACEKKEY SET ACELKEY=$EXTRACT(S2,1) 
  . . WRITE ACELKEY  ;"write hotkey character
  . . SET POS=POS+1
  . . SET MENU(IDX,"ACELKEYS",$$UP^XLFSTR(ACELKEY),JDX)=""  ;"Generate for possible use by user later.   
  . . DO VTATRIB^TMGTERM(0)  ;"turn off underline and all other attributes.  (including color changes)
  . . DO SETCOLORS(MODE,.COLARR2)
  . . NEW REMAINDER SET REMAINDER=$EXTRACT(S2,2,$LENGTH(S2))
  . . WRITE REMAINDER
  . . SET POS=POS+$LENGTH(REMAINDER)
  . ELSE  DO
  . . WRITE STR
  . . SET POS=POS+$LENGTH(STR)
  . FOR POS=POS:1:MAX WRITE " "
  . IF USINGSUBMENU WRITE $SELECT(SUBMENU>0:" >",1:"  ")
  DO VCULOAD2^TMGTERM
  DO VTATRIB^TMGTERM(0)  ;"reset attribs and colors
  QUIT  
  ;
DRAWBARMENU(IDX,MENU) ;"Draw top horizontal bar menu.
  ;"Input: see DRAWMENU for description of variables. 
  SET IDX=+$GET(IDX)              
  IF $GET(MENU("FOCUS IDX"))="" SET MENU("FOCUS IDX")=IDX
  NEW INFO DO MENUINFO(IDX,.MENU,.INFO) ;"Get info about menu   
  IF $DATA(MENU("OPTION","COLOR"))=0 DO
  . NEW TEMP DO DEFAULTCLR(.TEMP) MERGE MENU("OPTION","COLOR")=TEMP KILL TEMP
  NEW X SET X=+$GET(MENU(IDX,"POS","X")) IF X<1 SET X=1
  NEW Y SET Y=+$GET(MENU(IDX,"POS","Y")) IF Y<1 SET Y=1
  NEW FOCUSED SET FOCUSED=(IDX=+$GET(MENU("FOCUS IDX")))
  NEW SELLINE SET SELLINE=+$GET(MENU(IDX,"SELLINE"))
  IF FOCUSED,SELLINE=0 SET SELLINE=1,MENU(IDX,"SELLINE")=1
  NEW COLARR MERGE COLARR=MENU("OPTION","COLOR")  
  NEW ERASE SET ERASE=INFO("ERASE")
  IF ERASE DO  
  . DO VTATRIB^TMGTERM(0) ;"clear colors for box
  . SET MENU(IDX,"DRAW MODE")=0  ;"turn off erase mode for future cycles, since handling now.
  ELSE  DO SETCOLORS("TEXT",.COLARR)
  NEW CURX,CURY SET CURX=X+1,CURY=Y
  DO CUP^TMGTERM(CURX,CURY)  
  WRITE $SELECT(ERASE:"   ",1:"___")
  ;"Now fill in text (or erase if in erase mode)
  NEW JDX SET JDX=0
  FOR  SET JDX=$ORDER(MENU(IDX,JDX)) QUIT:JDX'>0  DO  
  . NEW STR SET STR=$GET(MENU(IDX,JDX))
  . IF ERASE DO  QUIT 
  . . DO VTATRIB^TMGTERM(0)
  . . SET STR=$TRANSLATE(STR,"&","")
  . . WRITE $$LJ^XLFSTR("",$LENGTH(STR)," ")_"    "
  . NEW COLREF2 SET COLREF2=$NAME(MENU(IDX,JDX,"COLOR"))
  . NEW COLARR2 MERGE COLARR2=COLARR
  . MERGE COLARR2=MENU(IDX,JDX,"COLOR")  ;"allow override of global colors if present.
  . NEW MODE SET MODE=$SELECT((JDX=SELLINE)&FOCUSED:"SELTEXT",1:"TEXT")
  . DO SETCOLORS(MODE,.COLARR2)  ;" establish colors for selection, background etc.  
  . IF STR["&" DO
  . . NEW S1,S2 SET S1=$PIECE(STR,"&",1),S2=$PIECE(STR,"&",2)
  . . NEW L1,L2 SET L1=$LENGTH(S1),L2=$LENGTH(S2)
  . . WRITE S1
  . . NEW ALTMODE SET ALTMODE=$SELECT(MODE["SEL":"SELALTKEY",1:"ALTKEY")
  . . DO SETCOLORS(ALTMODE,.COLARR2)
  . . DO VTATRIB^TMGTERM(4)  ;"turn on underline
  . . NEW ACEKKEY SET ACELKEY=$EXTRACT(S2,1) 
  . . WRITE ACELKEY  ;"write hotkey character
  . . SET MENU(IDX,"ACELKEYS",$$UP^XLFSTR(ACELKEY),JDX)=""  ;"Generate for possible use by user later.   
  . . DO VTATRIB^TMGTERM(0)  ;"turn off underline and all other attributes.  (including color changes)
  . . DO SETCOLORS(MODE,.COLARR2)
  . . NEW REMAINDER SET REMAINDER=$EXTRACT(S2,2,$LENGTH(S2))
  . . WRITE REMAINDER
  . ELSE  DO
  . . WRITE STR
  . DO SETCOLORS("TEXT",.COLARR2)  ;" establish colors for selection, background etc.  
  . WRITE "____"  ;"spacing between entries.  
  DO VCULOAD2^TMGTERM
  DO VTATRIB^TMGTERM(0)  ;"reset attribs and colors
  QUIT
  ;
MENUINFO(IDX,MENU,OUT) ;"Get info about menu  
  ;"INPUT:  IDX -- the active menu
  ;"        MENU -- see DRAWMENU
  ;"        OUT -- PASS BY REFERENCE, AN OUT PARAMETER.  Format:
  ;"               OUT("HT")=HT
  ;"               OUT("WT")=WT
  ;"               OUT("ERASE")=ERASE
  ;"               OUT("USING ICON")=USINGICON
  ;"               OUT("USING CHECK")=USINGCHECK
  ;"               OUT("USING SUBMENU")=USINGSUBMENU
  ;"               OUT("TOP MENU BAR")=TOPMENUBAR
  ;"RESULT: none
  NEW WT SET WT=0
  NEW HT SET HT=0
  NEW ERASE SET ERASE=(+$GET(MENU(IDX,"DRAW MODE"))=-1)
  NEW TOPMENUBAR SET TOPMENUBAR=(+$GET(MENU(IDX,"TOP MENU BAR"))=1)
  NEW USINGCHECK SET USINGCHECK=0
  NEW USINGICON SET USINGICON=0
  NEW USINGSUBMENU SET USINGSUBMENU=0
  NEW HORIZWT SET HORIZWT=3 ;"For initial gap, if menu is a TOP MENU BAR.  
  NEW JDX SET JDX=0
  FOR  SET JDX=$ORDER(MENU(IDX,JDX)) QUIT:JDX'>0  DO
  . SET HORIZWT(JDX)=HORIZWT
  . SET HT=HT+1
  . NEW STR SET STR=$GET(MENU(IDX,JDX))
  . SET STR=$TRANSLATE(STR,"&","")  ;"remove tag for accelerator char. 
  . NEW HLINE SET HLINE=+$GET(MENU(IDX,JDX,"HLINE"))    
  . NEW HASICON SET HASICON=($DATA(MENU(IDX,JDX,"ICON"))>0)
  . IF HASICON SET USINGICON=1
  . NEW HASCHECK SET HASCHECK=($DATA(MENU(IDX,JDX,"CHECKED"))>0)
  . IF HASCHECK SET USINGCHECK=1
  . NEW SUBMENU SET SUBMENU=+$GET(MENU(IDX,JDX,"SUBMENU"))
  . IF SUBMENU>0,$DATA(MENU(SUBMENU))>0 SET USINGSUBMENU=1
  . NEW LEN SET LEN=$LENGTH(STR)
  . SET HORIZWT=HORIZWT+LEN+4  ;"+4 for gap between menu entries in horizontal bar
  . IF HLINE QUIT  ;"H lines fill available space, and don't set menu width
  . IF LEN>WT SET WT=LEN
  IF USINGICON SET WT=WT+1
  IF USINGCHECK SET WT=WT+2
  IF USINGSUBMENU SET WT=WT+2
  SET OUT("TOP MENU BAR")=TOPMENUBAR
  SET OUT("ERASE")=ERASE
  IF TOPMENUBAR DO
  . SET OUT("HT")=1
  . SET OUT("WT")=HORIZWT
  . MERGE OUT("HORIZ")=HORIZWT
  ELSE  DO
  . SET OUT("HT")=HT
  . SET OUT("WT")=WT
  . SET OUT("USING ICON")=USINGICON
  . SET OUT("USING CHECK")=USINGCHECK
  . SET OUT("USING SUBMENU")=USINGSUBMENU      
  QUIT
  ;    
SETCOLORS(MODE,ARR) ;" Set color
  ;"INPUT -- MODE -- Name of color, must match entry passed in ARR, E.G. TEXT,SELTEXT,BORDER
  ;"                 OR 'RESET', which will trigger color RESET
  ;"         ARRREF -- PASS BY NAME.  Format: 
  ;"            ARR("TEXT")="FG^BG"     
  ;"            ARR("SELTEXT")="FG^BG"   
  ;"            ARR("BORDER")="FG^BG"
  ;"            ARR("ALTKEY")="FG^BG"   
  ;"            ARR("SELALTKEY")="FG^BG"   
  SET MODE=$GET(MODE)
  IF MODE="RESET" DO  QUIT
  . DO VTATRIB^TMGTERM(0)   ;"reset colors
  NEW CLRS SET CLRS=$GET(ARR(MODE))
  NEW FG SET FG=$PIECE(CLRS,"^",1)
  NEW BG SET BG=$PIECE(CLRS,"^",2)
  IF (CLRS="")!(FG="")!(BG="") DO
  . NEW DEF DO DEFAULTCLR(.DEF)
  . SET CLRS=$GET(DEF(MODE))
  . IF CLRS="" SET CLRS=DEF("DEFAULT")
  . SET FG=$PIECE(CLRS,"^",1)
  . SET BG=$PIECE(CLRS,"^",2)
  . SET ARR(MODE)=FG_"^"_BG
  DO VCOLORS^TMGTERM(FG,BG)
  QUIT
  ;
DEFAULTCLR(ARR)  ;"Get default color array
  DO SETGBLCO^TMGTERM   ;"Set Global Colors vars
  SET ARR("TEXT")=TMGCOLWHITE_"^"_TMGCOLWHITE        ;"FG^BG"     
  SET ARR("SELTEXT")=TMGCOLFGBWHITE_"^"_TMGCOLBLACK  ;"FG^BG"
  SET ARR("BORDER")=TMGCOLBLACK_"^"_TMGCOLWHITE      ;"FG^BG"
  SET ARR("DEFAULT")=TMGCOLBLACK_"^"_TMGCOLWHITE     ;"FG^BG"
  SET ARR("ALTKEY")=TMGCOLRED_"^"_TMGCOLWHITE        ;"FG^BG"
  SET ARR("SELALTKEY")=TMGCOLRED_"^"_TMGCOLBLACK     ;"FG^BG"
  DO KILLGBLC^TMGTERM   ;"Kill Global Colors vars
  QUIT
  ;
  ;"==========================================================================
  ;"==========================================================================
DRAWTESTSCRN(WT,HT) ;
  SET HT=+$GET(HT)
  SET WT=+$GET(WT)
  IF (HT=0)!(WT=0) DO GETSCRSZ^TMGKERNL(.HT,.WT) SET HT=HT-2,WT=WT-3
  NEW CHARS DO GETSHADECODES^TMGTERM2(.CHARS)
  NEW CH SET CH=$GET(CHARS("XREF","Light Shade"),".")
  ;"SET CH="."
  NEW INFO SET INFO("THICK")=1,INFO("ARC")=1  ;"Add settings for how to draw box border of menu
  DO VTATRIB^TMGTERM(0) ;"clear colors for box
  ;"WRITE WT,",",HT,!
  DO DRAWBOX^TMGTERM2(1,1,WT,HT,.INFO)
  NEW X,Y
  FOR Y=2:1:HT-1 DO
  . FOR X=2:1:WT-1 DO
  . . DO CUP^TMGTERM(X,Y)
  . . ;"WRITE CH 
  . . DO UTF8WRITE^TMGSTUTL(CH)
  QUIT;
  
  
  