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
  ;"     @BUFREF@(BUFNAME,<bufType>,Y,X,"CH") = character to go into this x,y position.  Char can be Unicode character.  
  ;"     @BUFREF@(BUFNAME,<bufType>,Y,X,"COLOR","FG") = Foreground color for position 
  ;"     @BUFREF@(BUFNAME,<bufType>,Y,X,"COLOR","BG") = Foreground color for position
  ;"     @BUFREF@(BUFNAME,"CURSOR","X")=current output cursor position X
  ;"     @BUFREF@(BUFNAME,"CURSOR","Y")=current output cursor position X
  ;"     @BUFREF@(BUFNAME,"COLOR",FG)=current default output FG color 
  ;"     @BUFREF@(BUFNAME,"COLOR",BG)=current default output BG color 
  ;"     @BUFREF@(BUFNAME,"ORIGIN")=POS, screen coordinates of top left corner of output of buffer.   
  ;"     @BUFREF@(BUFNAME,"WIDTH")=Width of output of buffer.   
  ;"     @BUFREF@(BUFNAME,"HEIGHT")=Width of output of buffer.   
  ;"
  ;"    NOTES: 
  ;"      When calling this function, the STR will be put into the buffer
  ;"        at the current cursor position, and cursor will be advanced. No wrapping is used.
  ;"      Buffer will not store ESC sequences.  Instead it will store the desired
  ;"        output.  And when sending to the terminal, then escape sequences will
  ;"        be used to achieve this output.  
  ;  
PAINT(POS,FGCOLOR,BGCOLOR,STR,BUFREF)  ;" 'PAINT', or output data to POS, directly or via buffer system
  ;"Input: POS -- format '<X>^<Y>'  -- screen coordinates of position to start paint.  E.g.  '5^12'
  ;"       FGCOLOR -- foreground color.  Format same as accepted by COLORS^TMGTERM
  ;"                  FGCOLOR **OR** BGCOLOR is -1, then terminal color is RESET to default.  
  ;"       BGCOLOR -- background color.  Format same as accepted by COLORS^TMGTERM
  ;"                  FGCOLOR **OR** BGCOLOR is -1, then terminal color is RESET to default.  
  ;"       STR -- A string (or 1 char) to write at POS.  Can be unicode character, if started with $
  ;"              Can be in format:  "abcdefg%UC%$2501;$3405%UC%xyz".  
  ;"                                 '%UC% will delimit plain text vs UniCode.
  ;"       @BUFREF@("BUFFERED")=<Buffer name>.  If defined, output into buffer instead of to screen.
  ;"       @BUFREF@("DEBUG MODE")=1  If found then output sent to buffer AND directly to screen.
  ;"       @BUFREF@("DEBUG MODE","OFFSET")=POS to offset when in DEBUG MODE
  NEW USEBUF SET USEBUF=$$BUFFERING(BUFREF)
  NEW DEBUG SET DEBUG=+$GET(@BUFREF@("DEBUG MODE"))
  IF USEBUF DO
  . DO SETBUFPOS(POS,BUFREF)
  . DO SETBUFCOLOR(.FGCOLOR,.BGCOLOR,BUFREF)
  IF (USEBUF=0)!(DEBUG=1) DO
  . IF DEBUG DO
  . . NEW DEBUGOFFSET SET DEBUGOFFSET=$GET(@BUFREF@("DEBUG MODE","OFFSET"),"0^0")
  . . SET POS=$$ADDPOS(POS,DEBUGOFFSET)
  . DO CUPOS^TMGTERM(POS)
  . DO COLORS^TMGTERM(.FGCOLOR,.BGCOLOR)
  DO TERMWRITE(.STR,BUFREF)
  QUIT
  ;
PAINTXY(X,Y,FGCOLOR,BGCOLOR,STR,BUFREF) ;" Output to X,Y directly or via buffer system
  ;"Input: X,Y -- screen coordinates.   See PAINT() for other parameters.  
  NEW POS SET POS=X_"^"_Y
  DO PAINT(POS,.FGCOLOR,.BGCOLOR,.STR,BUFREF)
  QUIT
  ;  
TERMWRITE(STR,BUFREF) ;"Unicode friendly writing to current cursor position directly to terminal screen or to buffer. 
  ;"Input: STR -- a string (or 1 char) to write at current cursor position
  ;"              Can be unicode character(s), if started with $.  For multiple unicode chars, separate with ';'
  ;"              Can be in format:  "abcdefg%UC%$2501;$3405%UC%xyz".  
  ;"                                 '%UC%' will delimit plain text vs UniCode.
  ;"       BUFREF -- OPTIONAL.  PASS BY REFERENCE.  This may contains the buffers.    
  ;"          @BUFREF@("BUFFERED")=<Buffer name>.  If defined, output into buffer instead of to screen.
  ;"          @BUFREF@("DEBUG MODE")=1  If found then output sent to buffer AND directly to screen.
  NEW USEBUF SET USEBUF=$$BUFFERING(BUFREF)
  NEW IDX FOR IDX=1:1:$LENGTH(STR,"%UC%") DO  ;"NOTE: %UC% not required to be present in string
  . NEW SUBSTR SET SUBSTR=$PIECE(STR,"%UC%",IDX)
  . IF USEBUF DO
  . . DO WRITE2BUF(SUBSTR,BUFREF)
  . IF (USEBUF=0)!(DEBUG=1) DO 
  . . DO WRITE2IO(SUBSTR)
  QUIT
  ;
BUFFERING(BUFREF) ;"Is buffering system enabled?
  NEW RESULT SET RESULT=($GET(@BUFREF@("BUFFERED"))'="")
  QUIT RESULT
  ;"---------- Test code ------------------
TEST1  ;
  NEW BUFARR
  DO INITBUF("1^20",30,10,"BUFARR")
  NEW TEMP,FG,BG SET BG=$$WEBCOLOR^TMGUSRI8("LightCoral",.TMP),FG=$$WEBCOLOR^TMGUSRI8("Black",.TMP)
  DO CLRBUF("BUFARR","X",FG,BG)  ;"Fill buff with character
  DO BUF2SCRN("BUFARR") ;"Output buffer to terminal screen.   
  DO COLORS^TMGTERM(-1,-1) ;"reset colors. 
  QUIT
  ;
TEST2 ;
  NEW BUFARR
  NEW W SET W=30
  NEW H SET H=10
  NEW CT SET CT=2000
  NEW COLARR
  DO INITBUF("1^20",30,10,"BUFARR")
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
INITBUF(POS,WIDTH,HEIGHT,BUFREF,BUFNAME) ;"Initialize Buffer in array
  ;"Input: POS -- X^Y OF ORIGIN
  ;"       WIDTH -- width of area to clear
  ;"       HEIGHT  -- width of area to clear
  ;"       BUFREF -- PASS BY REFERENCE.  Name of variable or global that contains the buffers.  
  ;"       BUFNAME -- OPTIONAL.  Default is  'TMGTERMBUF'.  This will be used as root node in BUFARR
  SET BUFNAME=$GET(BUFNAME,"TMGTERMBUF")
  SET @BUFREF@("BUFFERED")=BUFNAME 
  DO SETBUFORIGIN($GET(POS,"1^1"),BUFREF)
  SET @BUFREF@(BUFNAME,"WIDTH")=$GET(WIDTH,80)
  SET @BUFREF@(BUFNAME,"HEIGHT")=$GET(HEIGHT,20)
  KILL @BUFREF@(BUFNAME,"CURRENT")
  KILL @BUFREF@(BUFNAME,"PENDING")
  QUIT
  ;
CLRBUF(BUFREF,FILLCH,FG,BG) ;"Clear PENDING buffer, filling buff with FILLCH
  ;"Input: BUFREF -- Name of var or global that contains the buffers.  @BUFREF@("BUFFERED")=<Buffer name>. 
  ;"       FILLCH -- OPTIONAL.  Character to fill screen with.  Default is " "
  ;"       FG -- OPTIONAL.  Foreground color.  Default is -1 (color default for screen)
  ;"       BG -- OPTIONAL.  Background color.  Default is -1 (color default for screen)
  ;
  NEW X,Y,BUFNAME SET BUFNAME=$GET(@BUFREF@("BUFFERED")) QUIT:BUFNAME=""
  SET FILLCH=$GET(FILLCH," "),FG=$GET(FG,-1),BG=$GET(BG,-1)
  NEW DISPHEIGHT SET DISPHEIGHT=$GET(@BUFREF@(BUFNAME,"HEIGHT"),20)
  NEW DISPWIDTH SET DISPWIDTH=$GET(@BUFREF@(BUFNAME,"WIDTH"),80)
  FOR Y=1:1:DISPHEIGHT DO
  . FOR X=1:1:DISPWIDTH DO
  . . DO SET1BUFITEM(BUFREF,FILLCH,X,Y,FG,BG)  
  QUIT
  ;
SETBUFORIGIN(POS,BUFREF) ;"Set origin position of buffer.   
  ;"Input: X,Y -- screen coordinates of position to start paint.
  ;"       BUFREF -- Name of var or global that contains the buffers.  @BUFREF@("BUFFERED")=<Buffer name>. 
  NEW BUFNAME SET BUFNAME=$GET(@BUFREF@("BUFFERED")) QUIT:BUFNAME=""
  SET @BUFREF@(BUFNAME,"ORIGIN")=POS  
  QUIT
  ;
SETBUFXY(X,Y,BUFREF) ;"Set current cursor position of buffer.   
  ;"Input: X,Y -- screen coordinates of position to start paint.
  ;"       BUFREF -- Name of var or global that contains the buffers.  @BUFREF@("BUFFERED")=<Buffer name>. 
  NEW BUFNAME SET BUFNAME=$GET(@BUFREF@("BUFFERED")) QUIT:BUFNAME=""
  SET @BUFREF@(BUFNAME,"CURSOR","X")=+$GET(X)  
  SET @BUFREF@(BUFNAME,"CURSOR","Y")=+$GET(Y)  
  QUIT
  ;
SETBUFPOS(POS,BUFREF) ;"Set current cursor position of buffer.   
  ;"Input: POS -- format '<X>^<Y>'  -- screen coordinates of position to start paint.  E.g.  '5^12'
  ;"       BUFREF -- Name of var or global that contains the buffers.  @BUFREF@("BUFFERED")=<Buffer name>. 
  NEW X SET X=+$PIECE($GET(POS),"^",1)
  NEW Y SET Y=+$PIECE($GET(POS),"^",2)
  DO SETBUFXY(X,Y,BUFREF)
  QUIT
  ;
SETBUFCOLOR(FG,BG,BUFREF) ;"Set default FG and BG color of buffer.   
  ;"Input: POS -- format '<X>^<Y>'  -- screen coordinates of position to start paint.  E.g.  '5^12'
  ;"       FG -- foreground color.  Format same as accepted by COLORS^TMGTERM
  ;"       BG -- background color.  Format same as accepted by COLORS^TMGTERM
  ;"       BUFREF -- Name of var or global that contains the buffers.  @BUFREF@("BUFFERED")=<Buffer name>. 
  NEW BUFNAME SET BUFNAME=$GET(@BUFREF@("BUFFERED")) QUIT:BUFNAME=""
  SET @BUFREF@(BUFNAME,"COLOR","FG")=FG  
  SET @BUFREF@(BUFNAME,"COLOR","BG")=BG  
  QUIT
  ;
WRITE2BUF(STR,BUFREF)  ;"Put STR into buffer 
  ;"Input: STR -- a string (or 1 char) to write at current cursor position
  ;"              Can be unicode character(s), if started with $.  For multiple unicode chars, separate with ';'
  ;"              Can **NOT** be in mixed format:  "abcdefg%UC%$2501;$3405%UC%xyz".   
  ;"                NOTE: to use mixed format, use TERMWRITE()
  ;"       BUFREF -- PASS BY REFERENCE.  This contains the buffers.  @BUFREF@("BUFFERED")=<Buffer name>. 
  NEW BUFNAME SET BUFNAME=$GET(@BUFREF@("BUFFERED")) QUIT:BUFNAME=""
  NEW X SET X=+$GET(@BUFREF@(BUFNAME,"CURSOR","X"))  
  NEW Y SET Y=+$GET(@BUFREF@(BUFNAME,"CURSOR","Y"))
  NEW FG SET FG=$GET(@BUFREF@(BUFNAME,"COLOR","FG"))
  NEW BG SET BG=$GET(@BUFREF@(BUFNAME,"COLOR","BG"))
  IF $$ISUNICODE(STR) DO
  . NEW IDX FOR IDX=1:1:$LENGTH(STR,";") DO
  . . NEW SUBSTR SET SUBSTR=$PIECE(STR,";",IDX)
  . . DO SET1BUFITEM(BUFREF,SUBSTR,X,Y,FG,BG) SET X=X+1  ;"NO WRAPPING -- clipped later in BUF2SCRN
  ELSE  DO
  . NEW IDX FOR IDX=1:1:$LENGTH(STR) DO
  . . NEW SUBSTR SET SUBSTR=$EXTRACT(STR,IDX)
  . . DO SET1BUFITEM(BUFREF,SUBSTR,X,Y,FG,BG) SET X=X+1  ;"NO WRAPPING -- clipped later in BUF2SCRN
  SET @BUFREF@(BUFNAME,"CURSOR","X")=X 
  QUIT
  ;
WRITE2IO(CH) ;"Put CH out to device (NOT BUFFER)
  ;"Input: CH -- should be 1 character, 1 substring, or 1 unicode char (indicated by '$xxxx')
  IF $$ISUNICODE(CH) DO
  . DO UTF8WRITE^TMGSTUTL(CH)  ;"write unicode chars.  Will change $X,$Y
  ELSE  DO
  . WRITE CH   ;"Will change $X,$Y   
  QUIT
  ;
SET1BUFITEM(BUFREF,CH,X,Y,FG,BG)  ;"
  ;"Input: BUFNAME -- reference to globally scoped buffer root.    
  ;"       CH -- 1 char to write at current cursor position.  No color or X,Y updating.  
  ;"             Can be SINGLE unicode character(s), if started with $.
  ;"       X,Y -- Coordinates for putting CH
  ;"       FG,BG -- colors of CH
  NEW BUFNAME SET BUFNAME=$GET(@BUFREF@("BUFFERED")) QUIT:BUFNAME=""
  SET @BUFREF@(BUFNAME,"PENDING",Y,X,"CH")=CH
  SET @BUFREF@(BUFNAME,"PENDING",Y,X,"COLOR","FG")=FG
  SET @BUFREF@(BUFNAME,"PENDING",Y,X,"COLOR","BG")=BG
  QUIT
  ;
ISUNICODE(ST) ;"Test if ST is string of $<unicode>; ... chars.
  NEW RESULT SET RESULT=1 ;"default TRUE
  NEW IDX FOR IDX=1:1:$LENGTH(ST,";") DO  QUIT:RESULT=0
  . NEW SUBSTR SET SUBSTR=$PIECE(ST,";",IDX)
  . SET RESULT=(SUBSTR?1"$"1.6NU)
  QUIT RESULT
  ;
BUF2SCRN(BUFREF,FULL) ;"Output buffer to terminal screen.   
  ;"Input: BUFREF -- PASS BY REFERENCE.  This contains the buffers.  @BUFREF@("BUFFERED")=<Buffer name>.
  ;"         @BUFREF@("PREWRITE EXECUTE") -- OPTIONAL.  If present, then executed before writing to screen.  
  ;"         @BUFREF@("POSTWRITE EXECUTE")-- OPTIONAL.  If present, then executed after writing to screen.  
  ;"       FULL -- Optional.  If 1 then BOTH the CURRENT and the PENDING buffers are put to screen.
  ;"               This can be useful if something else has distored the background screen.  
  ;"               NOTE: Only elements of the CURRENT buffer which are NOT to be overwritten
  ;"                 by PENDING will be printed out. But ALL PENDING changes will be sent.  
  NEW BUFNAME SET BUFNAME=$GET(@BUFREF@("BUFFERED")) QUIT:BUFNAME=""  
  NEW POS SET POS=$GET(@BUFREF@(BUFNAME,"ORIGIN"))
  NEW ORIGINX,ORIGINY DO SPLITPOS(POS,.ORIGINX,.ORIGINY)
  NEW CH,FG,BG,LASTFG,LASTBG SET (LASTFG,LASTBG)=0
  NEW DISPHEIGHT SET DISPHEIGHT=$GET(@BUFREF@(BUFNAME,"HEIGHT"),80)
  NEW DISPWIDTH SET DISPWIDTH=$GET(@BUFREF@(BUFNAME,"WIDTH"),200)
  NEW HASPRECODE SET HASPRECODE("X")=$GET(@BUFREF@("PREWRITE EXECUTE")),HASPRECODE=(HASPRECODE("X")]"")
  NEW HASPOSTCODE SET HASPOSTCODE("X")=$GET(@BUFREF@("POSTWRITE EXECUTE")),HASPOSTCODE=(HASPOSTCODE("X")]"")
  SET FULL=+$GET(FULL)
  NEW X,Y  
  SET (X,Y)=0      
  IF FULL DO  ;"Output background CURRENT if FULL requested
  . FOR  SET Y=$ORDER(@BUFREF@(BUFNAME,"CURRENT",Y)) QUIT:(Y'>0)!(Y>DISPHEIGHT)  DO
  . . SET X=0
  . . FOR  SET X=$ORDER(@BUFREF@(BUFNAME,"CURRENT",Y,X)) QUIT:(X'>0)!(X>DISPWIDTH)  DO   
  . . . ;"IF $DATA(@BUFREF@(BUFNAME,"PENDING",Y,X)) QUIT  ;"Don't output CURRENT if PENDING will later overwrite.
  . . . SET CH=$GET(@BUFREF@(BUFNAME,"CURRENT",Y,X,"CH")," ")    
  . . . SET FG=$GET(@BUFREF@(BUFNAME,"CURRENT",Y,X,"COLOR","FG"))
  . . . SET BG=$GET(@BUFREF@(BUFNAME,"CURRENT",Y,X,"COLOR","BG"))
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
  FOR  SET Y=$ORDER(@BUFREF@(BUFNAME,"PENDING",Y)) QUIT:(Y'>0)!(Y>DISPHEIGHT)  DO
  . SET X=0
  . FOR  SET X=$ORDER(@BUFREF@(BUFNAME,"PENDING",Y,X)) QUIT:(X'>0)!(X>DISPWIDTH)  DO    
  . . SET CH=$GET(@BUFREF@(BUFNAME,"PENDING",Y,X,"CH")," ")    
  . . SET FG=$GET(@BUFREF@(BUFNAME,"PENDING",Y,X,"COLOR","FG"))
  . . SET BG=$GET(@BUFREF@(BUFNAME,"PENDING",Y,X,"COLOR","BG"))
  . . NEW SAME SET SAME=1 ;"start with TRUE  
  . . SET SAME=SAME&($GET(@BUFREF@(BUFNAME,"CURRENT",Y,X,"CH"))=CH)
  . . SET SAME=SAME&($GET(@BUFREF@(BUFNAME,"CURRENT",Y,X,"COLOR","FG"))=FG)
  . . SET SAME=SAME&($GET(@BUFREF@(BUFNAME,"CURRENT",Y,X,"COLOR","BG"))=BG)
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
  . . KILL @BUFREF@(BUFNAME,"CURRENT",Y,X)
  . . MERGE @BUFREF@(BUFNAME,"CURRENT",Y,X)=@BUFREF@(BUFNAME,"PENDING",Y,X)
  KILL @BUFREF@(BUFNAME,"PENDING") 
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
  ;"---------------------------------------------------------------------
  ;
ADD2TERMBUF0(STR,OPTION)  ;"OLD VERSIONS.  DELETE LATER
  ;"Put STR into TMGTERMBUF.  See also documentation for ESCN()
  ;"NOTE: User of this function will be responsible for KILL'ing TMGTERMBUF content.
  ;"INPUT:  STR -- the string of characters to be added to buffer
  ;"        OPTION -- Optional
  ;"          OPTION("BYTES")=1  If passed, then STR added to buffer in way such that later it will be 
  ;"                           written out byte by byte.  NOTE: expected format is like this: "145,12,15,67,254"
  ;"                          NOTE: This "BYTES" entry will be killed at the end of this call.  
  ;"          OPTION("BUFFERED")=<Buffer name>.  If defined, output into buffer instead of to screen.
  ;"          @<BufferName>@("INFO")=<line# to add to>^<current # chars on line>^<buffer line max length>
  ;"          @<BufferName>@(<buffer#>,<line#>)=escape sequences, text etc to be output later, all at once.
  ;"         The idea behind this is to send ALL the screen info all at once, to hopefully avoid flickering. 
  NEW ISBYTES SET ISBYTES=($GET(OPTION("BYTES"))=1) KILL OPTION("BYTES")
  NEW BUFNAME SET BUFNAME=$GET(OPTION("BUFFERED")) QUIT:BUFNAME=""
  NEW INFO SET INFO=$GET(@BUFNAME@("INFO"))
  SET LINENUM=+$PIECE(INFO,"^",1) IF LINENUM=0 SET LINENUM=1
  NEW CURLEN SET CURLEN=+$PIECE($GET(INFO),"^",2)
  NEW MAXLEN SET MAXLEN=+$PIECE($GET(INFO),"^",3)
  IF MAXLEN=0 SET MAXLEN=1000000 
  NEW STLEN SET STLEN=$LENGTH(STR)
  IF STLEN+CURLEN>MAXLEN DO           ;"<-- note: if ALL of STR can't be added to line, then advance to next line.  
  . SET LINENUM=LINENUM+1,CURLEN=0
  NEW CURISBYTES SET CURISBYTES=($GET(@BUFNAME@(LINENUM,"BYTES"))=1)
  IF ISBYTES,(CURISBYTES=0) DO
  . IF CURLEN>0 SET LINENUM=LINENUM+1,CURLEN=0
  . SET @BUFNAME@(LINENUM,"BYTES")=1
  ELSE  IF (CURISBYTES=1),(ISBYTES=0) DO
  . SET LINENUM=LINENUM+1,CURLEN=0  ;"advance to a NON-BYTES line.  
  SET @BUFNAME@(LINENUM)=$GET(@BUFNAME@(LINENUM))_STR_$SELECT(ISBYTES:",",1:"")
  SET @BUFNAME@("INFO")=LINENUM_"^"_(CURLEN+STLEN)_"^"_MAXLEN
  QUIT
  ;
PUTBUF(BUFNAME) ;"Output buffer to current IO.    OLD CODE.  DELETE LATER
  ;"NOTE: User of this functionality is responsible for setting device parameter WIDTH to big number, or set to NOWRAP
  ;"      I think it would be inefficient to change it here and then change it back for each buffer write.
  SET BUFNAME=$GET(BUFNAME) QUIT:BUFNAME=""
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(@BUFNAME@(IDX)) QUIT:IDX'>0  DO
  . NEW CURISBYTES SET CURISBYTES=($GET(@BUFNAME@(IDX,"BYTES"))=1)  ;"NOTE: entire line will either be BYTES or not.  No mixing.  
  . IF CURISBYTES DO
  . . NEW BYTES SET BYTES=@BUFNAME@(IDX)   ;"NOTE: expected format is like this: "145,12,15,67,254"
  . . FOR JDX=1:1:$LENGTH(BYTES) SET ABYTE=$PIECE(BYTES,",",JDX) IF ABYTE>0 DO
  . . . WRITE *ABYTE
  . ELSE  WRITE @BUFNAME@(IDX)
  NEW MAXLEN SET MAXLEN=+$PIECE($GET(@BUFNAME@("INFO")),"^",3) IF MAXLEN=0 SET MAXLEN=1000000
  KILL @BUFNAME SET @BUFNAME@("INFO")="1^0^"_MAXLEN  ;"<--- reset buffer. 
  QUIT
  ;
