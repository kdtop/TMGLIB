TMGTERM4  ;TMG/kst/Terminal interface (Double buffer painting) ;9/30/2024
         ;;1.0;TMG-LIB;**1,17**;9/30/24
  ;
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;"Copyright (c) 9/30/2024  Kevin S. Toppenberg MD
  ;"
  ;"This file is part of the TMG LIBRARY, and may only be used in accordence
  ;" to license terms outlined in separate file TMGLICNS.m, which should 
  ;" always be distributed with this file.
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ; 
  ;"=======================================================================
  ;" API -- Public Functions.
  ;"=======================================================================
  ;
  ;"=======================================================================
  ;"=======================================================================
  ;"EXPLAINATION OF BUFFER SYSTEM:
  ;"   Goal is to provide double buffer for smother output when animating terminal screen.
  ;"   If outputing with animation, a 'frame' is drawn, followed by another 'frame'.
  ;"   When drawing to screen, if one first clears screen by outputing blank
  ;"     spaces to clear last display frame before redrawing, then this can 
  ;"     cause a flicker because the blank (white) area is briefly shown. 
  ;"   Another way would be to just completely fill the entire display with the desired 
  ;"     elements of the frame.  A complete draw would include the 'empty' or 
  ;"     blank elements in places without characters.  
  ;"   Another, more effecient way would be to output just the DIFFERENCE from
  ;"     one frame to another.  Thus only parts that need to be redrawn or erased
  ;"     would have actual data put out.  It is this method that I am going to implement here. 
  ;"
  ;"   I will create a data structure, or 'buffer' that will hold a representation
  ;"     of what is to be put to the screen. And when ready, this will be actually
  ;"     sent to the terminal for display.  
  ;"   After being sent to the terminal, I will call this buffer the 'CURRENT' buffer.
  ;"   As a program is drawing into the buffer, I will be putting new data into the a
  ;"     'PENDING' buffer.  This has not yet been put out to the terminal screen.   
  ;"     Note that this PENDING buffer may include only a few changes.
  ;"   I will then, when programmatically triggered, write the PENDING buffer out 
  ;"     to the terminal screen.  Note that elements of the prior frame would still 
  ;"     be present, so not everything is rewritten.  
  ;"   I will then put all the elements from the PENDING buffer onto the CURRENT
  ;"   I then clear the PENDING buffer and repeat the cycle.
  ;"
  ;"   The structure of each buffer will be as follows:
  ;"     @BUFREF@(<bufType>,Y,X,"CH") = character to go into this x,y position.  Char can be Unicode character.  
  ;"     @BUFREF@(<bufType>,Y,X,"COLOR","FG") = Foreground color for position 
  ;"     @BUFREF@(<bufType>,Y,X,"COLOR","BG") = Foreground color for position
  ;"            E.g of bufType = "CURRENT","PENDING",
  ;"     @BUFREF@("CURSOR","X")=current output cursor position X
  ;"     @BUFREF@("CURSOR","Y")=current output cursor position X
  ;"     @BUFREF@("COLOR",FG)=current default output FG color 
  ;"     @BUFREF@("COLOR",BG)=current default output BG color 
  ;"     @BUFREF@("ORIGIN")=POS, screen coordinates of top left corner of output of buffer.   
  ;"     @BUFREF@("WIDTH")=Width of output of buffer.   
  ;"     @BUFREF@("HEIGHT")=Width of output of buffer.   
  ;"
  ;"   NOTE: The system is designed with the idea that an OPTIONS array will be passed
  ;"      around by name, e.g. OPTIONREF.  This array will contain the name of the 
  ;"      buffer to use, i.e. BUFREF -- the name of a globally scoped variable, or disk global.
  ;"      E.G. @OPTIONREF@("BUFFERED")=BUFREF
  ;"
  ;"    NOTES: 
  ;"      When calling this function, the STR will be put into the buffer
  ;"        at the current cursor position, and cursor will be advanced. No wrapping is used.
  ;"      Buffer will not store ESC sequences.  Instead it will store the desired
  ;"        output.  And when sending to the terminal, then escape sequences will
  ;"        be used to achieve this output.  
  ;  
GETBOUNDS(OPTIONREF,MINX,MAXX,MINY,MAXY) ;
  SET MAXX=+$GET(@OPTIONREF@("BOUNDS","MAXX")) IF MAXX=0 SET MAXX=99999999
  SET MAXY=+$GET(@OPTIONREF@("BOUNDS","MAXY")) IF MAXY=0 SET MAXY=99999999
  SET MINX=+$GET(@OPTIONREF@("BOUNDS","MINX")) IF MINX=0 SET MINX=1
  SET MINY=+$GET(@OPTIONREF@("BOUNDS","MINY")) IF MINY=0 SET MINY=1
  ;
  NEW USEBUF SET USEBUF=$$BUFFERING(OPTIONREF)
  NEW BUFREF SET BUFREF=$GET(@OPTIONREF@("BUFFERED")) QUIT:((USEBUF=1)&(BUFREF=""))
  IF USEBUF DO
  . NEW ORIGIN SET ORIGIN=$GET(@BUFREF@("ORIGIN"),"1^1") 
  . SET MINX=$PIECE(ORIGIN,"^",1),MINY=$PIECE(ORIGIN,"^",2)
  . NEW BUFFMAXX SET BUFFMAXX=MINX+$GET(@BUFREF@("WIDTH"))-1
  . NEW BUFFMAXY SET BUFFMAXY=MINY+$GET(@BUFREF@("HEIGHT"))-1
  . IF BUFFMAXX<MAXX SET MAXX=BUFFMAXX
  . IF BUFFMAXY<MAXY SET MAXY=BUFFMAXY
  ELSE  DO
  . NEW SCRNMAXX SET SCRNMAXX=+$GET(@OPTIONREF@("SCRN WIDTH")) IF SCRNMAXX=0 SET SCRNMAXX=99999999
  . NEW SCRNMAXY SET SCRNMAXY=+$GET(@OPTIONREF@("SCRN HEIGHT")) IF SCRNMAXY=0 SET SCRNMAXY=99999999
  . IF SCRNMAXX<MAXX SET MAXX=SCRNMAXX
  . IF SCRNMAXY<MAXY SET MAXY=SCRNMAXY
  QUIT
  ;
PAINT(POS,FGCOLOR,BGCOLOR,STR,OPTIONREF)  ;" 'PAINT', or output data to POS, directly or via buffer system
  NEW X,Y SET X=$PIECE(POS,"^",1),Y=$PIECE(POS,"^",2)
  DO PAINTXY(X,Y,.FGCOLOR,.BGCOLOR,.STR,OPTIONREF)
  QUIT
  ;
PAINTXY(X,Y,FGCOLOR,BGCOLOR,STR,OPTIONREF) ;" Output to X,Y directly or via buffer system
  ;"Input: X,Y -- screen coordinates.  -- screen coordinates of position to start paint.  E.g.  '5^12'
  ;"       FGCOLOR -- foreground color.  Format same as accepted by COLORS^TMGTERM
  ;"                  FGCOLOR **OR** BGCOLOR is -1, then terminal color is RESET to default.  
  ;"       BGCOLOR -- background color.  Format same as accepted by COLORS^TMGTERM
  ;"                  FGCOLOR **OR** BGCOLOR is -1, then terminal color is RESET to default.  
  ;"       STR -- A string (or 1 char) to write at POS.  Can be unicode character, if started with $
  ;"              Can be in format:  "abcdefg%UC%$2501;$3405%UC%xyz".  
  ;"                                 '%UC% will delimit plain text vs UniCode.
  ;"       @OPTIONREF@("BUFFERED")=<Buffer REFERENCE>.  If defined, output into buffer instead of to screen.
  ;"                               Name of var or global that contains the buffers.  
  ;"       @OPTIONREF@("DEBUG MODE")=1  If found then output sent to buffer AND directly to screen.
  ;"       @OPTIONREF@("DEBUG MODE","OFFSET")=POS to offset when in DEBUG MODE
  ;"       @OPTIONREF@("SCRN WIDTH")= OPTIONAL screen width, used for clipping.  (default is no clipping)
  ;"       @OPTIONREF@("SCRN HEIGHT")= OPTIONAL screen height, used for clipping (default is no clipping)
  ;"       @OPTIONREF@("BOUNDS","MINX")=SCREEN COORDS of allowed draw area, clip outside bounds
  ;"       @OPTIONREF@("BOUNDS","MAXX")=SCREEN COORDS of allowed draw area, clip outside bounds
  ;"       @OPTIONREF@("BOUNDS","MINY")=SCREEN COORDS of allowed draw area, clip outside bounds
  ;"       @OPTIONREF@("BOUNDS","MAXY")=SCREEN COORDS of allowed draw area, clip outside bounds    
  NEW USEBUF SET USEBUF=$$BUFFERING(OPTIONREF)
  NEW DEBUG SET DEBUG=+$GET(@OPTIONREF@("DEBUG MODE"))
  NEW BUFREF SET BUFREF=$GET(@OPTIONREF@("BUFFERED")) QUIT:((USEBUF=1)&(BUFREF=""))
  NEW MINX,MAXX,MINY,MAXY DO GETBOUNDS(OPTIONREF,.MINX,.MAXX,.MINY,.MAXY)
  IF (Y<MINY)!(Y>MAXY)!(X>MAXX) GOTO PDN  ;"clip entire STR
  ;
  NEW IDX FOR IDX=1:1:$LENGTH(STR,"%UC%") DO  ;"NOTE: %UC% not required to be present in string
  . NEW SUBSTR SET SUBSTR=$PIECE(STR,"%UC%",IDX) QUIT:SUBSTR=""
  . NEW LEN SET LEN=$$STRLEN^TMGSTUTL(SUBSTR)  ;"Unicode aware
  . IF $$CLIPIFNEEDED(X,MINX,MAXX,.SUBSTR,LEN)>0 DO
  . . IF X<MINX  DO  ;"must have been clipping on LEFT side of bounds
  . . . SET X=MINX
  . . ELSE  DO       ;"must have been clipping on RIGHT side of bounds
  . . . ;"Nothing needs to be done here.  
  . . SET LEN=$$STRLEN^TMGSTUTL(SUBSTR)  ;"Unicode aware
  . IF USEBUF DO
  . . DO SETBUFPOS(X_"^"_Y,BUFREF)
  . . DO SETBUFCOLOR(.FGCOLOR,.BGCOLOR,BUFREF)
  . . DO WRITE2BUF(SUBSTR,BUFREF)
  . IF (USEBUF=0)!(DEBUG=1) DO
  . . NEW POS SET POS=X_"^"_Y
  . . IF DEBUG DO
  . . . NEW DEBUGOFFSET SET DEBUGOFFSET=$GET(@OPTIONREF@("DEBUG MODE","OFFSET"),"0^0")
  . . . SET POS=$$ADDPOS(POS,DEBUGOFFSET)
  . . DO CUPOS^TMGTERM(POS)
  . . DO COLORS^TMGTERM(.FGCOLOR,.BGCOLOR)
  . . DO WRITE2IO(SUBSTR)
  . SET X=X+LEN
PDN ;  
  QUIT
  ;
CLIPIFNEEDED(CURX,MINX,MAXX,STR,LEN) ;
  ;"STR  is IN AND OUT PARAMETER -- should be ALL unicode or ALL normal chars
  ;"RESULT: 1 if any clip done, otherwise 0
  NEW RESULT SET RESULT=0
  NEW CLIPLEN SET CLIPLEN=0
  IF CURX+LEN-1>MAXX DO
  . SET CLIPLEN=(CURX+LEN-1)-MAXX
  . IF CLIPLEN>0 DO
  . . NEW DESIREDLEN SET DESIREDLEN=MAXX-CURX+1
  . . SET STR=$$UNICODEMIDSTR^TMGSTUT3(STR,1,DESIREDLEN)
  . . SET RESULT=1
  IF CURX<MINX DO
  . NEW START SET START=MINX-CURX+1    
  . SET STR=$$UNICODEMIDSTR^TMGSTUT3(STR,START,LEN)
  . SET RESULT=1
  QUIT RESULT
  ;
BUFFERING(BUFREF) ;"Is buffering system enabled?
  NEW RESULT SET RESULT=($GET(@BUFREF@("BUFFERED"))'="")
  QUIT RESULT
  ;"---------- Test code ------------------
TEST1  ;
  NEW OPTION,BUFARR
  DO INITBUF("1^20",30,10,"OPTION","BUFARR")
  NEW TEMP,FG,BG SET BG=$$WEBCOLOR^TMGUSRI8("LightCoral",.TMP),FG=$$WEBCOLOR^TMGUSRI8("Black",.TMP)
  DO CLRBUF("BUFARR","X",FG,BG)  ;"Fill buff with character
  DO BUF2SCRN("BUFARR") ;"Output buffer to terminal screen.   
  DO COLORS^TMGTERM(-1,-1) ;"reset colors. 
  QUIT
  ;
TEST2 ;
  NEW OPTION,BUFARR
  NEW W SET W=30
  NEW H SET H=10
  NEW CT SET CT=2000
  NEW COLARR
  DO INITBUF("1^20",30,10,"OPTION","BUFARR")
  NEW TEMP,FG,BG SET BG=$$WEBCOLOR^TMGUSRI8("LightCoral",.TMP),FG=$$WEBCOLOR^TMGUSRI8("Black",.TMP)
  DO CLRBUF("BUFARR"," ",FG,BG)  ;"Fill buff with character
  DO BUF2SCRN("BUFARR") ;"Output buffer to terminal screen.
  NEW IDX FOR IDX=1:1:CT DO
  . NEW JDX FOR JDX=1:1:10 DO
  . . NEW X SET X=$RANDOM(W)+1
  . . NEW Y SET Y=$RANDOM(H)+1
  . . NEW COLOR SET COLOR=$$RANDCOLOR^TMGUSRI8(.COLARR) SET COLOR=$PIECE(COLOR,"^",1)
  . . NEW INVCLR SET INVCLR=$$INVCLRVEC^TMGTERM(COLOR)
  . . DO PAINTXY(X,Y,INVCLR,COLOR,$CHAR($RANDOM(26)+64),"BUFARR")
  . DO BUF2SCRN("BUFARR") ;"Output buffer to terminal screen.     
  DO COLORS^TMGTERM(-1,-1) ;"reset colors. 
  QUIT
  ;
  ;"---------- BUFFER SYSTEM BELOW ------------------
  ;
INITBUF(POS,WIDTH,HEIGHT,OPTIONREF,BUFREF) ;"Initialize Buffer in array
  ;"Input: POS -- X^Y OF ORIGIN
  ;"       WIDTH -- width of area to clear
  ;"       HEIGHT  -- width of area to clear
  ;"       OPTIONREF -- name of OPTION array that will contain reference to global buffer
  ;"       BUFREF -- Name of var or global that contains the buffers.    
  IF $GET(OPTIONREF)'="" SET @OPTIONREF@("BUFFERED")=BUFREF 
  DO SETBUFORIGIN($GET(POS,"1^1"),BUFREF)
  SET @BUFREF@("WIDTH")=$GET(WIDTH,80)
  SET @BUFREF@("HEIGHT")=$GET(HEIGHT,20)
  KILL @BUFREF@("CURRENT")
  KILL @BUFREF@("PENDING")
  QUIT
  ;
CLRBUF(BUFREF,FILLCH,FG,BG) ;"Clear PENDING buffer, filling buff with FILLCH
  ;"Input: BUFREF -- Name of var or global that contains the buffers.  
  ;"       FILLCH -- OPTIONAL.  Character to fill screen with.  Default is " "
  ;"       FG -- OPTIONAL.  Foreground color.  Default is -1 (color default for screen)
  ;"       BG -- OPTIONAL.  Background color.  Default is -1 (color default for screen)
  ;
  SET FILLCH=$GET(FILLCH," "),FG=$GET(FG,-1),BG=$GET(BG,-1)
  NEW DISPHEIGHT SET DISPHEIGHT=$GET(@BUFREF@("HEIGHT"),20)
  NEW DISPWIDTH SET DISPWIDTH=$GET(@BUFREF@("WIDTH"),80)
  FOR Y=1:1:DISPHEIGHT DO
  . FOR X=1:1:DISPWIDTH DO
  . . DO SET1BUFITEM(BUFREF,FILLCH,X,Y,FG,BG)  
  QUIT
  ;
KILLBUF(BUFREF) ;"KILL entire buffer contents (but not  CURSOR, size etc)
  ;"Input: BUFREF -- Name of var or global that contains the buffers.  @BUFREF@("BUFFERED")=<Buffer name>. 
  KILL @BUFREF@("PENDING")
  KILL @BUFREF@("CURRENT")
  QUIT
  ;  
SETBUFORIGIN(POS,BUFREF) ;"Set origin position of buffer.   
  ;"Input: X,Y -- screen coordinates of position to start paint.
  ;"       BUFREF -- Name of var or global that contains the buffers.  @BUFREF@("BUFFERED")=<Buffer name>. 
  SET @BUFREF@("ORIGIN")=POS  
  QUIT
  ;
SETBUFXY(X,Y,BUFREF) ;"Set current cursor position of buffer.   
  ;"Input: X,Y -- screen coordinates of position to start paint.
  ;"       BUFREF -- Name of var or global that contains the buffers.  
  SET @BUFREF@("CURSOR","X")=+$GET(X)  
  SET @BUFREF@("CURSOR","Y")=+$GET(Y)  
  QUIT
  ;
SETBUFPOS(POS,BUFREF) ;"Set current cursor position of buffer.   
  ;"Input: POS -- format '<X>^<Y>'  -- screen coordinates of position to start paint.  E.g.  '5^12'
  ;"       BUFREF -- Name of var or global that contains the buffers. 
  NEW X SET X=+$PIECE($GET(POS),"^",1)
  NEW Y SET Y=+$PIECE($GET(POS),"^",2)
  DO SETBUFXY(X,Y,BUFREF)
  QUIT
  ;
SETBUFCOLOR(FG,BG,BUFREF) ;"Set default FG and BG color of buffer.   
  ;"Input: FG -- foreground color.  Format same as accepted by COLORS^TMGTERM
  ;"       BG -- background color.  Format same as accepted by COLORS^TMGTERM
  ;"       BUFREF -- Name of var or global that contains the buffers.  
  SET @BUFREF@("COLOR","FG")=FG  
  SET @BUFREF@("COLOR","BG")=BG  
  QUIT
  ;
WRITE2BUF(STR,BUFREF)  ;"Put STR into buffer 
  ;"Input: STR -- a string (or 1 char) to write at current cursor position
  ;"              Can be unicode character(s), if started with $.  For multiple unicode chars, separate with ';'
  ;"              Can **NOT** be in mixed format:  "abcdefg%UC%$2501;$3405%UC%xyz".   
  ;"                NOTE: to use mixed format, use PAINT()
  ;"       BUFREF -- Name of var or global that contains the buffers.  
  IF STR="" QUIT
  NEW X SET X=+$GET(@BUFREF@("CURSOR","X"))  
  NEW Y SET Y=+$GET(@BUFREF@("CURSOR","Y"))
  NEW FG SET FG=$GET(@BUFREF@("COLOR","FG"))
  NEW BG SET BG=$GET(@BUFREF@("COLOR","BG"))
  IF $$ISUNICODE^TMGSTUTL(STR) DO
  . NEW IDX FOR IDX=1:1:$LENGTH(STR,";") DO
  . . NEW SUBSTR SET SUBSTR=$PIECE(STR,";",IDX)
  . . DO SET1BUFITEM(BUFREF,SUBSTR,X,Y,FG,BG) SET X=X+1  ;"NO WRAPPING -- clipped later in BUF2SCRN
  ELSE  DO
  . NEW IDX FOR IDX=1:1:$LENGTH(STR) DO
  . . NEW SUBSTR SET SUBSTR=$EXTRACT(STR,IDX)
  . . DO SET1BUFITEM(BUFREF,SUBSTR,X,Y,FG,BG) SET X=X+1  ;"NO WRAPPING -- clipped later in BUF2SCRN
  SET @BUFREF@("CURSOR","X")=X 
  QUIT
  ;
WRITE2IO(CH) ;"Put CH out to device (NOT BUFFER)
  ;"Input: CH -- should be 1 character, 1 substring, or 1 unicode char (indicated by '$xxxx')
  IF $$ISUNICODE^TMGSTUTL(CH) DO
  . DO UTF8WRITE^TMGSTUTL(CH)  ;"write unicode chars.  Will change $X,$Y
  ELSE  DO
  . WRITE CH   ;"Will change $X,$Y   
  QUIT
  ;
SET1BUFITEM(BUFREF,CH,X,Y,FG,BG)  ;"
  ;"Input: BUFREF -- Name of var or global that contains the buffers.  
  ;"       CH -- 1 char to write at current cursor position.  No color or X,Y updating.  
  ;"             Can be SINGLE unicode character(s), if started with $.
  ;"       X,Y -- Coordinates for putting CH
  ;"       FG,BG -- colors of CH
  SET @BUFREF@("PENDING",Y,X,"CH")=CH
  SET @BUFREF@("PENDING",Y,X,"COLOR","FG")=FG
  SET @BUFREF@("PENDING",Y,X,"COLOR","BG")=BG
  QUIT
  ;
BUF2SCRINDIRECT(OPTIONREF,BUFOPT) ;"Output buffer to terminal screen.
  ;"This gets reference to actual buffer from @OPTIONREF@("BUFFERED")
  NEW BUFREF SET BUFREF=$GET(@OPTIONREF@("BUFFERED")) QUIT:BUFREF=""
  DO BUF2SCRN(BUFREF,.BUFOPT)
  QUIT
  ;
BUF2SCRN(BUFREF,BUFOPT) ;"Output buffer to terminal screen.   
  ;"--This takes name of output buffer directly  
  ;"Input: BUFREF -- PASS BY REFERENCE.  This contains the buffers.  @BUFREF@("BUFFERED")=<Buffer name>.
  ;"         @BUFREF@("PREWRITE EXECUTE") -- OPTIONAL.  If present, then executed before writing to screen.  
  ;"         @BUFREF@("POSTWRITE EXECUTE")-- OPTIONAL.  If present, then executed after writing to screen. 
  ;"       BUFOPT -- OPTIONAL
  ;"         BUFOPT("FULL") -- Optional.  If 1 then BOTH the CURRENT and the PENDING buffers are put to screen.
  ;"               This can be useful if something else has distored the background screen.  
  ;"               NOTE: Only elements of the CURRENT buffer which are NOT to be overwritten
  ;"                 by PENDING will be printed out. But ALL PENDING changes will be sent.  
  ;"         BUFOPT("KEEP PENDING")=1  Debugging option.  If found, then screen is written out 
  ;"                 but shift from PENDING to CURRENT is not done
  NEW POS SET POS=$GET(@BUFREF@("ORIGIN"))
  NEW ORIGINX,ORIGINY DO SPLITPOS(POS,.ORIGINX,.ORIGINY)
  NEW CH,FG,BG,LASTFG,LASTBG SET (LASTFG,LASTBG)=0
  NEW DISPHEIGHT SET DISPHEIGHT=$GET(@BUFREF@("HEIGHT"),80)
  NEW DISPWIDTH SET DISPWIDTH=$GET(@BUFREF@("WIDTH"),200)
  NEW HASPRECODE SET HASPRECODE("X")=$GET(@BUFREF@("PREWRITE EXECUTE")),HASPRECODE=(HASPRECODE("X")]"")
  NEW HASPOSTCODE SET HASPOSTCODE("X")=$GET(@BUFREF@("POSTWRITE EXECUTE")),HASPOSTCODE=(HASPOSTCODE("X")]"")
  SET FULL=+$GET(BUFOPT("FULL"))
  NEW KEEPPENDING SET KEEPPENDING=(+$GET(BUFOPT("KEEP PENDING"))=1)
  NEW X,Y  
  SET (X,Y)=0      
  IF FULL DO  ;"Output background CURRENT if FULL requested
  . FOR  SET Y=$ORDER(@BUFREF@("CURRENT",Y)) QUIT:(Y'>0)!(Y>DISPHEIGHT)  DO
  . . SET X=0
  . . FOR  SET X=$ORDER(@BUFREF@("CURRENT",Y,X)) QUIT:(X'>0)!(X>DISPWIDTH)  DO   
  . . . ;"IF $DATA(@BUFREF@("PENDING",Y,X)) QUIT  ;"Don't output CURRENT if PENDING will later overwrite.
  . . . SET CH=$GET(@BUFREF@("CURRENT",Y,X,"CH")," ")    
  . . . SET FG=$GET(@BUFREF@("CURRENT",Y,X,"COLOR","FG"))
  . . . SET BG=$GET(@BUFREF@("CURRENT",Y,X,"COLOR","BG"))
  . . . ;"Ensure screen color is correct
  . . . IF HASPRECODE XECUTE HASPRECODE("X")
  . . . IF (FG'=LASTFG)!(BG'=LASTBG) do
  . . . . DO COLORS^TMGTERM(FG,BG)
  . . . . SET LASTFG=FG,LASTBG=BG
  . . . ;"Ensure screen cursor set to X,Y
  . . . IF ($X'=(X+ORIGINX-1))!($Y'=(ORIGINY+Y-1)) DO
  . . . . DO CUP^TMGTERM(ORIGINX+X-1,ORIGINY+Y-1)    
  . . . DO WRITE2IO(CH) ;"Put CH out to Screen / device (NOT BUFFER)
  . . . IF HASPOSTCODE XECUTE HASPOSTCODE("X")
  ;"Now output PENDING buffer.  
  SET (X,Y)=0      
  FOR  SET Y=$ORDER(@BUFREF@("PENDING",Y)) QUIT:(Y'>0)!(Y>DISPHEIGHT)  DO
  . SET X=0
  . FOR  SET X=$ORDER(@BUFREF@("PENDING",Y,X)) QUIT:(X'>0)!(X>DISPWIDTH)  DO    
  . . SET CH=$GET(@BUFREF@("PENDING",Y,X,"CH")," ")    
  . . SET FG=$GET(@BUFREF@("PENDING",Y,X,"COLOR","FG"))
  . . SET BG=$GET(@BUFREF@("PENDING",Y,X,"COLOR","BG"))
  . . NEW SAME SET SAME=1 ;"start with TRUE  
  . . SET SAME=SAME&($GET(@BUFREF@("CURRENT",Y,X,"CH"))=CH)
  . . SET SAME=SAME&($GET(@BUFREF@("CURRENT",Y,X,"COLOR","FG"))=FG)
  . . SET SAME=SAME&($GET(@BUFREF@("CURRENT",Y,X,"COLOR","BG"))=BG)
  . . IF SAME QUIT  ;"Already present on screen, no need to write again.
  . . ;"Ensure screen color is correct
  . . IF HASPRECODE XECUTE HASPRECODE("X")
  . . IF (FG'=LASTFG)!(BG'=LASTBG) do
  . . . DO COLORS^TMGTERM(FG,BG)
  . . . SET LASTFG=FG,LASTBG=BG
  . . ;"Ensure screen cursor set to X,Y
  . . IF ($X'=(X+ORIGINX-1))!($Y'=(ORIGINY+Y-1)) DO
  . . . DO CUP^TMGTERM(ORIGINX+X-1,ORIGINY+Y-1)    
  . . DO WRITE2IO(CH) ;"Put CH out to Screen / device (NOT BUFFER)
  . . IF HASPOSTCODE XECUTE HASPOSTCODE("X")
  . . IF KEEPPENDING=1 QUIT
  . . KILL @BUFREF@("CURRENT",Y,X)
  . . MERGE @BUFREF@("CURRENT",Y,X)=@BUFREF@("PENDING",Y,X)
  IF KEEPPENDING=0 KILL @BUFREF@("PENDING") 
  QUIT
  ;
SPLITPOS(POS,X,Y) ;"LOAD X,Y FROM POS
  SET X=$$POSX(.POS),Y=$$POSY(.POS)
  QUIT
  ;
POSX(POS) ;
  NEW X SET X=$PIECE(POS,"^",1) IF X'>0 SET X=1
  QUIT X
  ;
POSY(POS) ;
  NEW Y SET Y=$PIECE(POS,"^",2) IF Y'>0 SET Y=1
  QUIT Y
  ;
XY2POS(X,Y,POS) ;
  SET X=+$GET(X,1),Y=+$GET(Y,1)
  SET POS=X_"^"_Y
  QUIT
ADDPOS(V1,V2)  ;"Return V1+V2
  NEW X SET X=$$POSX(V1)+$$POSX(V2)
  NEW Y SET Y=$$POSY(V1)+$$POSY(V2)
  QUIT X_"^"_Y
  ;
SUBPOS(V1,V2)  ;"Return V1-V2
  NEW X SET X=$$POSX(V1)-$$POSX(V2)
  NEW Y SET Y=$$POSY(V1)-$$POSY(V2)
  QUIT X_"^"_Y
  ;
