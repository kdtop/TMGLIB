TMGTERM2  ;TMG/kst/Terminal interface (Unicode line drawing) ;3/28/24
         ;;1.0;TMG-LIB;**1,17**;3/28/24
  ;
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;"Copyright (c) 3/28/2024  Kevin S. Toppenberg MD
  ;"
  ;"This file is part of the TMG LIBRARY, and may only be used in accordence
  ;" to license terms outlined in separate file TMGLICNS.m, which should 
  ;" always be distributed with this file.
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ; 
  ;"=======================================================================
  ;" API -- Public Functions.
  ;"=======================================================================
  ;"DEMOBOXES(COUNT) ;     
  ;"DRAWBOX(LEFT,TOP,WIDTH,HEIGHT,FGCOLOR,BGCOLOR,OPTION) -- Draw square or squircle on screen with line drawing chars 
  ;"DRAWBOX2(LEFT,TOP,WIDTH,HEIGHT,FGCOLOR,BGCOLOR,OPTIONREF) -- Draw square or squircle on screen with line drawing chars   
  ;"DRAWHLINE(LEFT,TOP,WIDTH,FGCOLOR,BGCOLOR,OPTION) -- Draw HORIZONTAL line on screen with line drawing chars
  ;"DRAWHLINE2(LEFT,TOP,WIDTH,FGCOLOR,BGCOLOR,OPTIONREF) -- Draw HORIZONTAL line on screen with line drawing chars
  ;"DRAWVLINE(LEFT,TOP,HEIGHT,FGCOLOR,BGCOLOR,OPTION) -- Draw VERTICAL line on screen with line drawing chars
  ;"DRAWVLINE2(LEFT,TOP,HEIGHT,FGCOLOR,BGCOLOR,OPTIONREF) -- Draw VERTICAL line on screen with line drawing chars
  ;"DRAWIBEAM(LEFT,TOP,HEIGHT,FGCOLOR,BGCOLOR,OPTION) -- Draw connecting VERTICAL 'I' line with line drawing chars       
  ;"DRAWCRT(LEFT,TOP,HEIGHT,FGCOLOR,BGCOLOR,OPTION) -- Draw connecting VERTICAL '[' with line drawing chars
  ;"DRAWCLF(LEFT,TOP,HEIGHT,FGCOLOR,BGCOLOR,OPTION) -- Draw connecting VERTICAL ']' with line drawing chars       
  ;"DRAWCNCTVLINE(LEFT,TOP,HEIGHT,FGCOLOR,BGCOLOR,CNCTS,OPTION) --Draw connecting VERTICAL 'I' line on screen with line drawing chars
  ;"DRAWCNCTVLINE2(LEFT,TOP,HEIGHT,FGCOLOR,BGCOLOR,CNCTS,OPTIONREF) --Draw connecting VERTICAL 'I' line on screen with line drawing chars
  ;"GET4BOXARR(OUT,OPTION)  -- Get array for characters to draw 4 boxes
  ;"GET4BOXARR2(OUT,OPTIONREF)  -- Get array for characters to draw 4 boxes
  ;"GETSQARR(OUT,OPTIONREF)  -- Get array for characters to draw square or squircle
  ;"  
  ;"=======================================================================
  ;"Private Functions
  ;"=======================================================================
  ;"GETTOP(REF,OPTION) --Get Top
  ;"GETSIDE(REF,OPTION) --Get Side  
  ;"GETBTRT(REF,OPTION) --Get Bottom Right
  ;"GETBTLF(REF,OPTION) --Get Bottom Left  
  ;"GETTOPLF(REF,OPTION) --Get Top Left  
  ;"GETTOPRT(REF,OPTION) -- Get Top Right
  ;"GETTEEDN(REF,OPTION) -- Get Tee down ("T" shape)
  ;"GETTEEUP(REF,OPTION) -- Get Tee up (upside down "T" shape)
  ;"GETTEERT(REF,OPTION) -- Get Tee RT ("T" shape, pointing to RT)
  ;"GETTEELF(REF,OPTION) -- Get Tee LF ("T" shape, pointing to LF)
  ;"GETCROSS(REF,OPTION) -- Get cross, ("+" shape)
  ;"GET2DIRS(REF,DIR1,DIR2,OPTION) ;  
  ;"GET4DIRS(REF,UP,DN,LF,RT,OPTION) ;    
  ;"COLLAPSEMODES(ARR)  --Convert ARR(<MODE>,<CODEPOINT>) --> ARR(CODEPOINT)
  ;"ADDNAMES(ARR,REF)  --(Debugging function) Add names to codepoints
  ;"OTHERDIR(OUT,DIR1,DIR2,DIR3) --Return DIR that is not in DIR1,DIR2,DIR3
  ;"NOTARR(OUT,ARR1,ARR2)  --Return elements of ARR1 that are NOT in ARR2
  ;"ANDARRS(OUT,ARR1,ARR2) --Return array of intersection (AND) of two array sets
  ;"GETLINECHARSREF()  ;
  ;"ENSURELINECODES(REF,FORCE) --Ensure codes set up in global
  ;"GETLINECODES(ARR) --Set up array of line drawing codes
  ;"PARSE1(STR,OUT)  ;  
  ;"GETSHADEARR(ARR) ;
  ;"GETSHADECHARSREF()  ;
  ;"ENSURESHADECODES(REF,FORCE) --Ensure codes set up in global
  ;"GETSHADECODES(ARR) --Set up shading drawing codes
  ;
  ;"=======================================================================
  ;"Private Testing Functions
  ;"=======================================================================
  ;"test1() ; 
  ;"TESTHLINE ;
  ;"TESTVLINE ;
  ;"TESTCTVLINE ;
  ;"TESTIBEAM ;
  ;"TESTCRT ;"Test Draw connecting VERTICAL '[' with line drawing chars 
  ;"TESTCLF ;
  ;"TEST2BOX ;
  ;
DEMOBOXES(COUNT) ;     
  DO CSRSHOW^TMGTERM(0) ;"Turn cursor ON(1) or OFF(0)(hide) 
  NEW SCRNH,SCRNW IF $$GETSCRSZ^TMGKERNL(.SCRNH,.SCRNW) ;"ignore result
  USE $P:(WIDTH=SCRNW:LENGTH=SCRNH:NOWRAP)
  NEW HT,WT,LEFT,TOP,THICK,IDX,ARC,DASH
  SET COUNT=+$GET(COUNT,10)
  FOR IDX=1:1:COUNT DO
  . SET HT=$$RAND^TMGMISC(5,25)
  . SET WT=$$RAND^TMGMISC(5,25)
  . SET LEFT=$$RAND^TMGMISC(1,SCRNW-WT-3)
  . SET TOP=$$RAND^TMGMISC(1,SCRNH-HT)
  . SET THICK=$$RAND^TMGMISC(1,3)
  . SET ARC=$$RAND^TMGMISC(0,1)
  . SET DASH=$$RAND^TMGMISC(0,3)
  . NEW OPTION 
  . SET OPTION("THICK")=THICK
  . SET OPTION("ARC")=ARC
  . SET OPTION("DASH")=DASH
  . DO DRAWBOX(LEFT,TOP,WT,HT,-1,-1,.OPTION)
  DO CSRSHOW^TMGTERM(1) ;"Turn cursor ON(1) or OFF(0)(hide) 
  QUIT
  ; 
TEST1() ; 
  NEW OPTION 
  ;"SET OPTION("THICK")=3
  ;"SET OPTION("DASH")=3
  ;"SET OPTION("ARC")=1
  SET OPTION("BOX FILL","CHAR")="*"
  SET OPTION("BOX FILL","COLORS")="0^1"
  SET OPTION("SAVE LINE CHARS")=1
  DO DRAWBOX(10,25,10,10,-1,-1,.OPTION)
  QUIT
  ;
DRAWBOX(LEFT,TOP,WIDTH,HEIGHT,FGCOLOR,BGCOLOR,OPTION) ;"Draw square or squircle on screen with line drawing chars 
  DO DRAWBOX2(.LEFT,.TOP,.WIDTH,.HEIGHT,.FGCOLOR,.BGCOLOR,"OPTION")    
  QUIT
  ;
DRAWBOX2(LEFT,TOP,WIDTH,HEIGHT,FGCOLOR,BGCOLOR,OPTIONREF) ;"Draw square or squircle on screen with line drawing chars 
  ;"Input:  LEFT - Screen coordinates of position of LEFT
  ;"        TOP  - Screen coordinates of position of TOP
  ;"        WIDTH -- Width of box
  ;"        HEIGHT -- Height of box
  ;"        FGCOLOR -- foreground color.  Format same as accepted by COLORS^TMGTERM
  ;"                  If -1, then terminal color is RESET to default.  If BGCOLOR=-1, FGCOLOR is overridden  
  ;"        BGCOLOR -- background color.  Format same as accepted by COLORS^TMGTERM
  ;"                  If -1, then terminal color is RESET to default.  If FGCOLOR=-1, BGCOLOR is overridden  
  ;"        @OPTIONREF@ -- Optional.  Format:
  ;"            @OPTIONREF@("THICK") -- 1 means Light  (default)
  ;"                               2 means Heavy
  ;"                               3 means Double
  ;"            @OPTIONREF@("ARC") -- if 1 then return ARC value, otherwise exclude.  Default=0
  ;"                           An arc gives a rounded corner
  ;"            @OPTIONREF@("DASH") --  0 means mode OFF       Default=0
  ;"                               1 means double dash
  ;"                               2 means triple dash
  ;"                               3 means quadruple dash
  ;"            @OPTIONREF@("ERASE") -- if 1, means we are ERASING box, not drawing.  
  ;"            @OPTIONREF@("BUFFERED")=<Buffer name>.  Name of var or global that contains the buffers.   
  ;"                            See TMGTERM4 for more info   
  ;"            @OPTIONREF@("SAVE LINE CHARS")=1  -- optional.  If found, then after applying options for thick, arc etc, 
  ;"                                          box line drawing chars will be stored into @OPTIONREF@("LINE CHARS") 
  ;"            @OPTIONREF@("LINE CHARS") ARRAY -- optional.  If found, then these are characters used to draw box, and
  ;"                  options for thickness, dash, arc etc are ignored.  
  ;"            @OPTIONREF@("NO TERM SAVE")=1 -- optional. If found, then prior terminal state is NOT saved and restored
  ;"            @OPTIONREF@("SCRN WIDTH")= OPTIONAL screen width, used for clipping.  (default is no clipping)
  ;"            @OPTIONREF@("SCRN HEIGHT")= OPTIONAL screen height, used for clipping (default is no clipping)
  ;"            @OPTIONREF@("BOX FILL","CHAR")=<fill char>  DEFAULT="", i.e. nothing drawn.  
  ;"            @OPTIONREF@("BOX FILL","COLORS")=CLRVEC24;CLRVEC24 for fill  OPTIONAL.  
  ;"            @OPTIONREF@("BOUNDS","MINX")=SCREEN COORDS of allowed draw area, clip outside bounds
  ;"            @OPTIONREF@("BOUNDS","MAXX")=SCREEN COORDS of allowed draw area, clip outside bounds
  ;"            @OPTIONREF@("BOUNDS","MINY")=SCREEN COORDS of allowed draw area, clip outside bounds
  ;"            @OPTIONREF@("BOUNDS","MAXY")=SCREEN COORDS of allowed draw area, clip outside bounds  
  SET WIDTH=+$GET(WIDTH) IF WIDTH<2 SET WIDTH=2
  SET HEIGHT=+$GET(HEIGHT) IF HEIGHT<2 SET HEIGHT=2                    
  SET TOP=+$GET(TOP),LEFT=+$GET(LEFT)
  NEW NOTERMSAVE SET NOTERMSAVE=($GET(@OPTIONREF@("NO TERM SAVE"))=1)
  NEW CHARS MERGE CHARS=@OPTIONREF@("LINE CHARS")
  IF $DATA(CHARS)=0 DO 
  . DO GETSQARR(.CHARS,OPTIONREF)
  . IF $GET(@OPTIONREF@("SAVE LINE CHARS"))'=1 QUIT
  . KILL @OPTIONREF@("LINE CHARS") MERGE @OPTIONREF@("LINE CHARS")=CHARS
  IF 'NOTERMSAVE DO VCUSAV2R^TMGTERM(OPTIONREF)  ;"Save Cursor & Attrs 
  NEW STR 
  ;"Write top line
  SET STR=CHARS("TL")_";"
  NEW CH SET CH=$GET(CHARS("TOP")) IF CH="" SET CH=$GET(CHARS("TOP/BOT"))
  NEW X FOR X=2:1:WIDTH-1 SET STR=STR_CH_";"
  SET STR=STR_CHARS("TR")
  DO PAINTXY^TMGTERM4(LEFT,TOP,FGCOLOR,BGCOLOR,STR,OPTIONREF)
  NEW FILLCH SET FILLCH=$GET(@OPTIONREF@("BOX FILL","CHAR"))
  NEW FILLSTR SET FILLSTR="" IF FILLCH'="" DO
  . SET FILLSTR="" SET $PIECE(FILLSTR,FILLCH,WIDTH-2)=FILLCH 
  NEW FILLCOLORPAIR SET FILLCOLORPAIR=$GET(@OPTIONREF@("BOX FILL","COLORS"))
  NEW FILLFG,FILLBG SET (FILLFG,FILLBG)="" IF FILLCOLORPAIR'="" DO
  . DO SPLITCOLORPAIR^TMGUSRI8(FILLCOLORPAIR,.FILLFG,.FILLBG)
  ;"Write sides
  NEW Y FOR Y=1:1:HEIGHT-2 DO
  . DO PAINTXY^TMGTERM4(LEFT,TOP+Y,FGCOLOR,BGCOLOR,CHARS("SIDE"),OPTIONREF)
  . IF FILLSTR]"" DO PAINTXY^TMGTERM4(LEFT+1,TOP+Y,FILLFG,FILLBG,FILLSTR,OPTIONREF)
  . DO PAINTXY^TMGTERM4(LEFT+WIDTH-1,TOP+Y,FGCOLOR,BGCOLOR,CHARS("SIDE"),OPTIONREF)
  ;"Write bottom.   
  SET STR=CHARS("BL")_";"
  NEW CH SET CH=$GET(CHARS("BOT")) IF CH="" SET CH=$GET(CHARS("TOP/BOT"))
  NEW X FOR X=2:1:WIDTH-1 SET STR=STR_CH_";"
  SET STR=STR_CHARS("BR")
  DO PAINTXY^TMGTERM4(LEFT,TOP+HEIGHT-1,FGCOLOR,BGCOLOR,STR,OPTIONREF)
  ;"Restore Cursor & Attrs
  IF 'NOTERMSAVE DO VCULOAD2R^TMGTERM(OPTIONREF)  
  QUIT
  ;
TESTHLINE ;
  WRITE #
  DO CUP^TMGTERM(5,20)
  WRITE "1234567890"
  DO DRAWHLINE(5,21,10,"0;0;0","255;255;255")
  DO CUP^TMGTERM(5,22)
  QUIT
  ;
DRAWHLINE(LEFT,TOP,WIDTH,FGCOLOR,BGCOLOR,OPTION) ;"Draw HORIZONTAL line on screen with line drawing chars
  DO DRAWHLINE2(.LEFT,.TOP,.WIDTH,.FGCOLOR,.BGCOLOR,"OPTION")
  QUIT
  ;
DRAWHLINE2(LEFT,TOP,WIDTH,FGCOLOR,BGCOLOR,OPTIONREF) ;"Draw HORIZONTAL line on screen with line drawing chars
  ;"Input:  LEFT - Screen coordinates of position of TOP 
  ;"        TOP  - Screen coordinates of position of TOP
  ;"        WIDTH -- Width of line
  ;"        FGCOLOR -- foreground color.  Format same as accepted by COLORS^TMGTERM
  ;"                  If -1, then terminal color is RESET to default.  If BGCOLOR=-1, FGCOLOR is overridden  
  ;"        BGCOLOR -- background color.  Format same as accepted by COLORS^TMGTERM
  ;"                  If -1, then terminal color is RESET to default.  If FGCOLOR=-1, BGCOLOR is overridden  
  ;"        OPTIONREF -- Optional.  Format:
  ;"            @OPTIONREF@("THICK") -- 1 means Light  (default)
  ;"                               2 means Heavy
  ;"                               3 means Double
  ;"            @OPTIONREF@("DASH") --  0 means mode OFF       Default=0
  ;"                               1 means double dash
  ;"                               2 means triple dash
  ;"                               3 means quadruple dash
  ;"            @OPTIONREF@("BUFFERED")=<Buffer name>.  If defined, output into buffer instead of to screen.
  ;"                            See TMGTERM4 for more info   
  ;"            @OPTIONREF@("NO TERM SAVE")=1 -- optional. If found, then prior terminal state is NOT saved and restored
  ;"            @OPTIONREF@("SCRN WIDTH")= OPTIONAL screen width, used for clipping.  (default is no clipping)
  ;"            @OPTIONREF@("SCRN HEIGHT")= OPTIONAL screen height, used for clipping (default is no clipping)  
  SET WIDTH=+$GET(WIDTH) IF WIDTH<1 SET WIDTH=1
  SET TOP=+$GET(TOP),LEFT=+$GET(LEFT)
  NEW NOTERMSAVE SET NOTERMSAVE=($GET(@OPTIONREF@("NO TERM SAVE"))=1)
  NEW CHARS DO GETSQARR(.CHARS,OPTIONREF)
  IF 'NOTERMSAVE DO VCUSAV2R^TMGTERM(OPTIONREF)  ;"Save Cursor & Attrs 
  NEW STR SET STR=""
  ;"Write top line
  NEW X FOR X=1:1:WIDTH SET STR=STR_CHARS("TOP/BOT")_";"
  DO PAINTXY^TMGTERM4(LEFT,TOP,FGCOLOR,BGCOLOR,STR,OPTIONREF)  
  ;"Restore Cursor & Attrs
  IF 'NOTERMSAVE DO VCULOAD2R^TMGTERM(OPTIONREF)  
  QUIT
  ;  
TESTVLINE ;
  WRITE #
  DO CUP^TMGTERM(1,20)
  WRITE "1",!
  WRITE "2",!
  WRITE "3",!
  WRITE "4",!
  WRITE "5",!
  DO DRAWVLINE(3,20,5,"0;0;0","255;255;255")
  DO CUP^TMGTERM(5,26)
  QUIT
  ;
DRAWVLINE(LEFT,TOP,HEIGHT,FGCOLOR,BGCOLOR,OPTION) ;"Draw VERTICAL line on screen with line drawing chars
  DO DRAWVLINE2(.LEFT,.TOP,.HEIGHT,.FGCOLOR,.BGCOLOR,"OPTION") 
  QUIT
  ;
DRAWVLINE2(LEFT,TOP,HEIGHT,FGCOLOR,BGCOLOR,OPTIONREF) ;"Draw VERTICAL line on screen with line drawing chars
  ;"Input:  LEFT - Screen coordinates of position of TOP 
  ;"        TOP  - Screen coordinates of position of TOP
  ;"        HEIGHT -- Height of line
  ;"        FGCOLOR -- foreground color.  Format same as accepted by COLORS^TMGTERM
  ;"                  If -1, then terminal color is RESET to default.  If BGCOLOR=-1, FGCOLOR is overridden  
  ;"        BGCOLOR -- background color.  Format same as accepted by COLORS^TMGTERM
  ;"                  If -1, then terminal color is RESET to default.  If FGCOLOR=-1, BGCOLOR is overridden  
  ;"        OPTIONREF -- Optional.  Format:
  ;"            @OPTIONREF@("THICK") -- 1 means Light  (default)
  ;"                               2 means Heavy
  ;"                               3 means Double
  ;"            @OPTIONREF@("DASH") --  0 means mode OFF       Default=0
  ;"                               1 means double dash
  ;"                               2 means triple dash
  ;"                               3 means quadruple dash
  ;"            @OPTIONREF@("ERASE") -- if 1, means we are ERASING box, not drawing.  
  ;"            @OPTIONREF@("BUFFERED")=<Buffer name>.  If defined, output into buffer instead of to screen.
  ;"                            See TMGTERM4 for more info   
  ;"            @OPTIONREF@("NO TERM SAVE")=1 -- optional. If found, then prior terminal state is NOT saved and restored  
  SET HEIGHT=+$GET(HEIGHT) IF HEIGHT<1 SET HEIGHT=1                    
  SET TOP=+$GET(TOP),LEFT=+$GET(LEFT)
  NEW NOTERMSAVE SET NOTERMSAVE=($GET(@OPTIONREF@("NO TERM SAVE"))=1)
  NEW CHARS DO GETSQARR(.CHARS,OPTIONREF)
  IF 'NOTERMSAVE DO VCUSAV2R^TMGTERM(OPTIONREF)  ;"Save Cursor & Attrs 
  NEW STR SET STR=""
  NEW Y FOR Y=0:1:HEIGHT-1 DO
  . DO PAINTXY^TMGTERM4(LEFT,TOP+Y,FGCOLOR,BGCOLOR,CHARS("SIDE"),OPTIONREF)
  ;"Restore Cursor & Attrs
  IF 'NOTERMSAVE DO VCULOAD2R^TMGTERM(OPTIONREF)  
  QUIT
  ;
TESTCTVLINE ;
  WRITE #
  DO CUP^TMGTERM(1,20)
  WRITE "1",!
  WRITE "2",!
  WRITE "3",!
  WRITE "4",!
  WRITE "5",!
  NEW CNCTS
  SET CNCTS("TOP","LEFT")=1     
  SET CNCTS("TOP","RIGHT")=0      
  SET CNCTS("BOTTOM","LEFT")=1    
  SET CNCTS("BOTTOM","RIGHT")=0   
  DO DRAWCNCTVLINE(3,20,5,"0;0;0","255;255;255",.CNCTS)
  DO CUP^TMGTERM(5,26)
  QUIT
  ;
TESTIBEAM ;
  NEW OPTION SET OPTION("THICK")=1
  DO DRAWIBEAM(2,25,3,-1,-1,.OPTION)
  QUIT
  ;
DRAWIBEAM(LEFT,TOP,HEIGHT,FGCOLOR,BGCOLOR,OPTION) ;"Draw connecting VERTICAL 'I' line with line drawing chars       
  NEW CNCTS
  SET CNCTS("TOP","LEFT")=1     
  SET CNCTS("TOP","RIGHT")=1      
  SET CNCTS("BOTTOM","LEFT")=1    
  SET CNCTS("BOTTOM","RIGHT")=1   
  DO DRAWCNCTVLINE(.LEFT,.TOP,.HEIGHT,.FGCOLOR,.BGCOLOR,.CNCTS,.OPTION)
  QUIT
  ;
TESTCRT ;"Test Draw connecting VERTICAL '[' with line drawing chars 
  NEW OPTION SET OPTION("THICK")=2
  DO DRAWCRT(2,25,3,-1,-1,.OPTION)
  QUIT
  ;
DRAWCRT(LEFT,TOP,HEIGHT,FGCOLOR,BGCOLOR,OPTION) ;"Draw connecting VERTICAL '[' with line drawing chars       
  NEW CNCTS
  SET CNCTS("TOP","LEFT")=0     
  SET CNCTS("TOP","RIGHT")=1      
  SET CNCTS("BOTTOM","LEFT")=0    
  SET CNCTS("BOTTOM","RIGHT")=1   
  DO DRAWCNCTVLINE2(.LEFT,.TOP,.HEIGHT,.FGCOLOR,.BGCOLOR,.CNCTS,"OPTION")
  QUIT
  ;
TESTCLF ;
  NEW OPTION SET OPTION("THICK")=2
  DO DRAWCLF(2,25,3,-1,-1,.OPTION)
  QUIT
  ;
DRAWCLF(LEFT,TOP,HEIGHT,FGCOLOR,BGCOLOR,OPTION) ;"Draw connecting VERTICAL ']' with line drawing chars       
  NEW CNCTS
  SET CNCTS("TOP","LEFT")=1     
  SET CNCTS("TOP","RIGHT")=0      
  SET CNCTS("BOTTOM","LEFT")=1    
  SET CNCTS("BOTTOM","RIGHT")=0   
  DO DRAWCNCTVLINE2(.LEFT,.TOP,.HEIGHT,.FGCOLOR,.BGCOLOR,.CNCTS,"OPTION")
  QUIT
  ;
DRAWCNCTVLINE(LEFT,TOP,HEIGHT,FGCOLOR,BGCOLOR,CNCTS,OPTION) ;"Draw connecting VERTICAL 'I' line on screen with line drawing chars
  DO DRAWCNCTVLINE2(.LEFT,.TOP,.HEIGHT,.FGCOLOR,.BGCOLOR,.CNCTS,"OPTION")
  QUIT
  ;
DRAWCNCTVLINE2(LEFT,TOP,HEIGHT,FGCOLOR,BGCOLOR,CNCTS,OPTIONREF) ;"Draw connecting VERTICAL 'I' line on screen with line drawing chars
  ;"Input:  LEFT - Screen coordinates of position of TOP 
  ;"        TOP  - Screen coordinates of position of TOP
  ;"        HEIGHT -- Height of line.  Must be at least 3, 1 for top+bottom connectors and one for vert line.  
  ;"        FGCOLOR -- foreground color.  Format same as accepted by COLORS^TMGTERM
  ;"                  If -1, then terminal color is RESET to default.  If BGCOLOR=-1, FGCOLOR is overridden  
  ;"        BGCOLOR -- background color.  Format same as accepted by COLORS^TMGTERM
  ;"                  If -1, then terminal color is RESET to default.  If FGCOLOR=-1, BGCOLOR is overridden
  ;"        CNCTS -- CONNECTIONS ARRAY.  Format:
  ;"            CNCTS("TOP","LEFT")=1       if found, V line connects at top to line going LEFT
  ;"            CNCTS("TOP","RIGHT")=1      if found, V line connects at top to line going RIGHT
  ;"            CNCTS("BOTTOM","LEFT")=1    if found, V line connects at bottom to line going LEFT
  ;"            CNCTS("BOTTOM","RIGHT")=1   if found, V line connects at bottom to line going RIGHT
  ;"        OPTIONREF -- Optional.  Format:
  ;"            @OPTIONREF@("THICK") -- 1 means Light  (default)
  ;"                               2 means Heavy
  ;"                               3 means Double
  ;"            @OPTIONREF@("DASH") --  0 means mode OFF       Default=0
  ;"                               1 means double dash
  ;"                               2 means triple dash
  ;"                               3 means quadruple dash                          
  ;"            @OPTIONREF@("ERASE") -- if 1, means we are ERASING box, not drawing.  
  ;"            @OPTIONREF@("BUFFERED")=<Buffer name>.  If defined, output into buffer instead of to screen.
  ;"                            See TMGTERM4 for more info   
  ;"            @OPTIONREF@("NO TERM SAVE")=1 -- optional. If found, then prior terminal state is NOT saved and restored  
  SET HEIGHT=+$GET(HEIGHT) IF HEIGHT<3 SET HEIGHT=3                    
  SET TOP=+$GET(TOP),LEFT=+$GET(LEFT)
  NEW NOTERMSAVE SET NOTERMSAVE=($GET(@OPTIONREF@("NO TERM SAVE"))=1)
  NEW CHARS DO GET4BOXARR(.CHARS,OPTIONREF)
  IF 'NOTERMSAVE DO VCUSAV2R^TMGTERM(OPTIONREF)  ;"Save Cursor & Attrs
  NEW TL SET TL=$GET(CNCTS("TOP","LEFT"))
  NEW TR SET TR=$GET(CNCTS("TOP","RIGHT"))
  NEW BL SET BL=$GET(CNCTS("BOTTOM","LEFT"))
  NEW BR SET BR=$GET(CNCTS("BOTTOM","RIGHT"))                     
  NEW STR SET STR=""
  NEW Y FOR Y=0:1:HEIGHT-1 DO
  . NEW CH SET CH=""
  . IF Y=0 DO
  . . IF (TL=1)&(TR=1) SET CH=CHARS("TOP T")
  . . ELSE  IF (TL=1)&(TR=0) SET CH=CHARS("TR")
  . . ELSE  IF (TL=0)&(TR=1) SET CH=CHARS("TL")
  . . ELSE  SET CH=CHARS("VERT")
  . ELSE  IF Y=(HEIGHT-1) DO
  . . IF (BL=1)&(BR=1) SET CH=CHARS("BOT T")
  . . ELSE  IF (BL=1)&(BR=0) SET CH=CHARS("BR")
  . . ELSE  IF (BL=0)&(BR=1) SET CH=CHARS("BL")
  . . ELSE  SET CH=CHARS("VERT")
  . ELSE  DO
  . . SET CH=CHARS("VERT")
  . DO PAINTXY^TMGTERM4(LEFT,TOP+Y,FGCOLOR,BGCOLOR,CH,OPTIONREF)
  ;"Restore Cursor & Attrs
  IF 'NOTERMSAVE DO VCULOAD2R^TMGTERM(OPTIONREF)  
  QUIT
  ;  
TEST4BOX ;
  NEW CHARS,OPTION,CT
  NEW TOP SET TOP=25
  NEW LEFT SET LEFT=5
  NEW OPTION,OPTIONREF SET OPTIONREF="OPTION"
  DO GET4BOXARR2(.CHARS,OPTIONREF)
  NEW X SET X=LEFT
  NEW Y SET Y=TOP
  ;"TOP LINE
  DO PAINTXY^TMGTERM4(X,Y,-1,-1,CHARS("TL"),OPTIONREF)        SET X=X+1
  DO PAINTXY^TMGTERM4(X,Y,-1,-1,CHARS("TOP/BOT"),OPTIONREF)   SET X=X+1
  DO PAINTXY^TMGTERM4(X,Y,-1,-1,CHARS("TOP/BOT"),OPTIONREF)   SET X=X+1
  DO PAINTXY^TMGTERM4(X,Y,-1,-1,CHARS("TOP T"),OPTIONREF)     SET X=X+1
  DO PAINTXY^TMGTERM4(X,Y,-1,-1,CHARS("TOP/BOT"),OPTIONREF)   SET X=X+1
  DO PAINTXY^TMGTERM4(X,Y,-1,-1,CHARS("TOP/BOT"),OPTIONREF)   SET X=X+1
  DO PAINTXY^TMGTERM4(X,Y,-1,-1,CHARS("TR"),OPTIONREF)        SET X=X+1              
  ;"SIDES                  
  SET X=LEFT,Y=Y+1
  DO PAINTXY^TMGTERM4(X,Y,-1,-1,CHARS("SIDE"),OPTIONREF) SET X=X+3   
  DO PAINTXY^TMGTERM4(X,Y,-1,-1,CHARS("SIDE"),OPTIONREF) SET X=X+3 
  DO PAINTXY^TMGTERM4(X,Y,-1,-1,CHARS("SIDE"),OPTIONREF)
  ;"MIDDLE LINE
  SET X=LEFT,Y=Y+1
  DO PAINTXY^TMGTERM4(X,Y,-1,-1,CHARS("T RT"),OPTIONREF)      SET X=X+1
  DO PAINTXY^TMGTERM4(X,Y,-1,-1,CHARS("TOP/BOT"),OPTIONREF)   SET X=X+1
  DO PAINTXY^TMGTERM4(X,Y,-1,-1,CHARS("TOP/BOT"),OPTIONREF)   SET X=X+1
  DO PAINTXY^TMGTERM4(X,Y,-1,-1,CHARS("CROSS"),OPTIONREF)     SET X=X+1
  DO PAINTXY^TMGTERM4(X,Y,-1,-1,CHARS("TOP/BOT"),OPTIONREF)   SET X=X+1
  DO PAINTXY^TMGTERM4(X,Y,-1,-1,CHARS("TOP/BOT"),OPTIONREF)   SET X=X+1
  DO PAINTXY^TMGTERM4(X,Y,-1,-1,CHARS("T LF"),OPTIONREF)      SET X=X+1
  ;"SIDES                  
  SET X=LEFT,Y=Y+1
  DO PAINTXY^TMGTERM4(X,Y,-1,-1,CHARS("SIDE"),OPTIONREF) SET X=X+3   
  DO PAINTXY^TMGTERM4(X,Y,-1,-1,CHARS("SIDE"),OPTIONREF) SET X=X+3 
  DO PAINTXY^TMGTERM4(X,Y,-1,-1,CHARS("SIDE"),OPTIONREF)
  ;"BOT LINE
  SET X=LEFT,Y=Y+1
  DO PAINTXY^TMGTERM4(X,Y,-1,-1,CHARS("BL"),OPTIONREF)        SET X=X+1 
  DO PAINTXY^TMGTERM4(X,Y,-1,-1,CHARS("TOP/BOT"),OPTIONREF)   SET X=X+1
  DO PAINTXY^TMGTERM4(X,Y,-1,-1,CHARS("TOP/BOT"),OPTIONREF)   SET X=X+1
  DO PAINTXY^TMGTERM4(X,Y,-1,-1,CHARS("BOT T"),OPTIONREF)     SET X=X+1
  DO PAINTXY^TMGTERM4(X,Y,-1,-1,CHARS("TOP/BOT"),OPTIONREF)   SET X=X+1
  DO PAINTXY^TMGTERM4(X,Y,-1,-1,CHARS("TOP/BOT"),OPTIONREF)   SET X=X+1
  DO PAINTXY^TMGTERM4(X,Y,-1,-1,CHARS("BR"),OPTIONREF)        SET X=X+1
  QUIT
  ;
GET4BOXARR(OUT,OPTION)  ;"Get array for characters to draw I beam
  DO GET4BOXARR2(.OUT,"OPTION")
  QUIT
  ;
GET4BOXARR2(OUT,OPTIONREF)  ;"Get array for characters to draw I beam
  ;"     +----+----+
  ;"     |    |    |
  ;"     +----+----+
  ;"     |    |    |
  ;"     +----+----+
  ;
  ;"Input:  OUT -- PASS BY REFERENCE, AN OUT PARAMETER.  Format: 
  ;"                   OUT("TOP T") 
  ;"                   OUT("VERT") <-- SAME AS OUT("SIDE") 
  ;"                   OUT("BOT T") 
  ;"                   OUT("TL") 
  ;"                   OUT("TR") 
  ;"                   OUT("BL") 
  ;"                   OUT("BR") 
  ;"                   OUT("TOP/BOT") 
  ;"                   OUT("SIDE")  
  ;"                   OUT("CROSS")  <-- CENTER CHAR.
  ;"                   OUT("T LF")
  ;"                   OUT("T RT")
  ;"        OPTIONREF -- Optional.  As passed to GET4DIRS
  IF $GET(@OPTIONREF@("GET CH DEFAULT",1))="" SET @OPTIONREF@("GET CH DEFAULT",1)=1  ;"try thickness=1 if trouble getting chars
  DO GETSQARR(.OUT,OPTIONREF)
  NEW REF SET REF=$$GETLINECHARSREF()
  SET OUT("TOP T")=$$GETTEEDN(REF,OPTIONREF) ;" Get Tee down ("T" shape)
  SET OUT("BOT T")=$$GETTEEUP(REF,OPTIONREF) ;" Get Tee up (upside down "T" shape)
  SET OUT("VERT")=OUT("SIDE")
  SET OUT("CROSS")=$$GETCROSS(REF,OPTIONREF) ;" Get cross, ("+" shape)
  SET OUT("T RT")=$$GETTEERT(REF,OPTIONREF)  ;" Get Tee RT ("T" shape, pointing to RT)
  SET OUT("T LF")=$$GETTEELF(REF,OPTIONREF)  ;" Get Tee LF ("T" shape, pointing to LF)
  QUIT
  ;
GETSQARR(OUT,OPTIONREF)  ;"Get array for characters to draw square or squircle
  ;"Input:  OUT -- PASS BY REFERENCE, AN OUT PARAMETER.  Format: 
  ;"                   OUT("TL") 
  ;"                   OUT("TR") 
  ;"                   OUT("BL") 
  ;"                   OUT("BR") 
  ;"                   OUT("TOP/BOT") 
  ;"                   OUT("SIDE") 
  ;"        OPTIONREF -- Optional.  As passed to GET2DIRS/GET4DIRS
  NEW REF SET REF=$$GETLINECHARSREF()
  SET OUT("TL")=$$GETTOPLF(REF,OPTIONREF) 
  SET OUT("TR")=$$GETTOPRT(REF,OPTIONREF) 
  SET OUT("BL")=$$GETBTLF(REF,OPTIONREF) 
  SET OUT("BR")=$$GETBTRT(REF,OPTIONREF) 
  SET OUT("TOP/BOT")=$$GETTOP(REF,OPTIONREF) 
  SET OUT("SIDE")=$$GETSIDE(REF,OPTIONREF) 
  QUIT
  ;
GETTOP(REF,OPTIONREF) ;"Get Top
  SET @OPTIONREF@("GET CH DEFAULT",2)="-"
  QUIT $$GET2DIRS(REF,"LF","RT",OPTIONREF)
  ;
GETSIDE(REF,OPTIONREF) ;"Get Side  
  SET @OPTIONREF@("GET CH DEFAULT",2)="|"
  QUIT $$GET2DIRS(REF,"DN","UP",OPTIONREF)
  ;
GETBTRT(REF,OPTIONREF) ;"Get Bottom Right
  SET @OPTIONREF@("GET CH DEFAULT",2)="+"
  QUIT $$GET2DIRS(REF,"UP","LF",OPTIONREF)
  ; 
GETBTLF(REF,OPTIONREF) ;"Get Bottom Left  
  SET @OPTIONREF@("GET CH DEFAULT",2)="+"
  QUIT $$GET2DIRS(REF,"UP","RT",OPTIONREF)
  ; 
GETTOPLF(REF,OPTIONREF) ;"Get Top Left  
  SET @OPTIONREF@("GET CH DEFAULT",2)="+"
  QUIT $$GET2DIRS(REF,"DN","RT",OPTIONREF)
  ;
GETTOPRT(REF,OPTIONREF) ;" Get Top Right
  SET @OPTIONREF@("GET CH DEFAULT",2)="+"
  QUIT $$GET2DIRS(REF,"DN","LF",OPTIONREF)
  ;
GETTEEDN(REF,OPTIONREF) ;" Get Tee down ("T" shape)
  SET @OPTIONREF@("GET CH DEFAULT",2)="+"
  NEW UP,DN,LF,RT SET (DN,LF,RT)=1
  QUIT $$GET4DIRS(REF,.UP,.DN,.LF,.RT,OPTIONREF)
  ;  
GETTEEUP(REF,OPTIONREF) ;" Get Tee up (upside down "T" shape)
  SET @OPTIONREF@("GET CH DEFAULT",2)="+"
  NEW UP,DN,LF,RT SET (UP,LF,RT)=1
  QUIT $$GET4DIRS(REF,.UP,.DN,.LF,.RT,OPTIONREF)
  ;
GETTEERT(REF,OPTIONREF) ;" Get Tee RT ("T" shape, pointing to RT)
  SET @OPTIONREF@("GET CH DEFAULT",2)="+"
  NEW UP,DN,LF,RT SET (UP,DN,RT)=1
  QUIT $$GET4DIRS(REF,.UP,.DN,.LF,.RT,OPTIONREF)
  ;  
GETTEELF(REF,OPTIONREF) ;" Get Tee LF ("T" shape, pointing to LF)
  SET @OPTIONREF@("GET CH DEFAULT",2)="+"
  NEW UP,DN,LF,RT SET (UP,DN,LF)=1
  QUIT $$GET4DIRS(REF,.UP,.DN,.LF,.RT,OPTIONREF)
  ;  
GETCROSS(REF,OPTIONREF) ;" Get cross, ("+" shape)
  SET @OPTIONREF@("GET CH DEFAULT",2)="+"
  NEW UP,DN,LF,RT SET (UP,DN,LF,RT)=1
  QUIT $$GET4DIRS(REF,.UP,.DN,.LF,.RT,OPTIONREF)
  ;
GET2DIRS(REF,DIR1,DIR2,OPTIONREF) ;
  NEW UP SET UP=(DIR1="UP")!(DIR2="UP")
  NEW DN SET DN=(DIR1="DN")!(DIR2="DN")
  NEW LF SET LF=(DIR1="LF")!(DIR2="LF")
  NEW RT SET RT=(DIR1="RT")!(DIR2="RT")
  NEW RESULT SET RESULT=$$GET4DIRS(.REF,UP,DN,LF,RT,OPTIONREF)
  QUIT RESULT
  ;
GET4DIRS(REF,UP,DN,LF,RT,OPTIONREF) ;  
  ;"INPUT: REF -- REF to stored codes array.
  ;"       UP -- 1 if wanted 0 if not
  ;"       DN -- 1 if wanted 0 if not
  ;"       LF -- 1 if wanted 0 if not
  ;"       RT -- 1 if wanted 0 if not
  ;"       OPTIONREF -- Optional.  Format:
  ;"            @OPTIONREF@("ASCII")=1,2 -- If 1, lines drawn with '*'.  If 2 then all lines drawn with '-', '|', '+'
  ;"            @OPTIONREF@("THICK") -- 1 means Light  (default)
  ;"                               2 means Heavy
  ;"                               3 means Double
  ;"            @OPTIONREF@("ARC") -- if 1 then return ARC value, otherwise exclude.  Default=0
  ;"                          An arc gives a rounded corner
  ;"            @OPTIONREF@("DASH") --  0 means mode OFF       Default=0
  ;"                               1 means double dash
  ;"                               2 means triple dash
  ;"                               3 means quadruple dash
  ;"            @OPTIONREF@("GET CH DEFAULT") -- character to return if nothing found. METHOD #1
  ;"            @OPTIONREF@("GET CH DEFAULT",1) -- #. If nothing found, try again with Thick=#,Arc=0,Dash=0.     <-- METHOD #2
  ;"            @OPTIONREF@("GET CH DEFAULT",2) -- If attempt at 1 didn't work, the return this simple char, e.g. '+'
  ;"            @OPTIONREF@("ERASE") -- if 1, means we are ERASING box, not drawing.  
  ;"Result: returns CodePoint, or default value if CodePoint not found, or space (32, or $20) if ERASE=1
  NEW RESULT SET RESULT=""
  NEW ERASE SET ERASE=+$GET(@OPTIONREF@("ERASE")) IF ERASE QUIT "$20"  
  NEW THICK SET THICK=+$GET(@OPTIONREF@("THICK")) IF "123"'[THICK SET THICK=1
  NEW MODE SET MODE=$SELECT(THICK=1:"Light",THICK=2:"Heavy",THICK=3:"Double")
  NEW ARC SET ARC=+$GET(@OPTIONREF@("ARC"))
  IF ARC=1 SET MODE="Light"  ;"Arc is only available in Light
  NEW DASH SET DASH=+$GET(@OPTIONREF@("DASH")) IF "0123"'[DASH SET DASH=0
  NEW DASHMODE SET DASHMODE=$SELECT(DASH=0:"",DASH=1:"Double",DASH=2:"Triple",DASH=3:"Quadruple")
  NEW UPARR,DNARR,LFARR,RTARR,OUT
  SET UP=$GET(UP),DN=$GET(DN),LF=$GET(LF),RT=$GET(RT)
  NEW HASVERT SET HASVERT=(UP=1)!(DN=1)
  NEW HASHORIZ SET HASHORIZ=(LF=1)!(RT=1)
  NEW ISCORNER SET ISCORNER=(HASVERT&HASHORIZ)  ;"i.e. corners or center '+'
  IF (UP&DN&LF&RT) SET ISCORNER=0  ;"Exclude '+'
  IF $GET(@OPTIONREF@("ASCII"))>0 GOTO G4L1 
  ;"Get all entries containing desired directions.                                                
  NEW TEMPREF
  IF DASHMODE="" DO
  . SET TEMPREF=$NAME(@REF@("XREF","MODE",MODE))
  ELSE  IF DASHMODE'="" DO
  . SET TEMPREF=$NAME(@REF@("XREF","DASH",DASHMODE,MODE))    
  IF UP MERGE UPARR=@TEMPREF@("UP")
  IF DN MERGE DNARR=@TEMPREF@("DN")
  IF LF MERGE LFARR=@TEMPREF@("LF")
  IF RT MERGE RTARR=@TEMPREF@("RT")
  DO ANDARRS(.OUT,.UPARR,.DNARR,.LFARR,.RTARR)  ;"may return many entries
  DO ADDNAMES(.OUT,REF)    
  ;"Exclude unwanted directions
  NEW DIRUP IF UP SET DIRUP="UP"
  NEW DIRDN IF DN SET DIRDN="DN"
  NEW DIRLF IF LF SET DIRLF="LF"
  NEW DIRRT IF RT SET DIRRT="RT"
  NEW EXCLARR DO OTHERDIR(.EXCLARR,.DIRUP,.DIRDN,.DIRLF,.DIRRT) ;"get list of directions NOT wanted.  
  NEW ADIR SET ADIR=""
  FOR  SET ADIR=$ORDER(EXCLARR(ADIR)) QUIT:ADIR=""  DO  ;"This ADIR is a direction NOT WANTED      
  . NEW XARR MERGE XARR=@REF@("XREF","DIR",ADIR)
  . DO COLLAPSEMODES(.XARR)  ;"Convert ARR(<MODE>,<CODEPOINT>) --> ARR(CODEPOINT)
  . DO ADDNAMES(.XARR,REF)
  . NEW TEMP2 DO NOTARR(.TEMP2,.OUT,.XARR) ;"Return all entries from OUT that are NOT found in XARR
  . KILL OUT MERGE OUT=TEMP2
  ;"Exclude ARC entries if not wanted.  
  IF ARC=0 DO
  . NEW XARR MERGE XARR=@REF@("XREF","ARC","Light")   ;"NOTE: ARC only comes in Light.  
  . DO COLLAPSEMODES(.XARR)  ;"Convert ARR(<DIR>,<CODEPOINT>) --> ARR(CODEPOINT)
  . DO ADDNAMES(.XARR,REF)
  . NEW TEMP2 DO NOTARR(.TEMP2,.OUT,.XARR) 
  . KILL OUT MERGE OUT=TEMP2
  ;"Exclude DASH entries if not wanted
  IF DASH=0 DO
  . NEW MULT SET MULT=""
  . FOR  SET MULT=$ORDER(@REF@("XREF","DASH",MULT)) QUIT:MULT=""  DO
  . . NEW MODE SET MODE=""
  . . FOR  SET MODE=$ORDER(@REF@("XREF","DASH",MULT,MODE)) QUIT:MODE=""  DO
  . . . NEW XARR MERGE XARR=@REF@("XREF","DASH",MULT,MODE)
  . . . DO COLLAPSEMODES(.XARR)  ;"Convert ARR(<DIR>,<CODEPOINT>) --> ARR(CODEPOINT)
  . . . DO ADDNAMES(.XARR,REF)
  . . . NEW TEMP2 DO NOTARR(.TEMP2,.OUT,.XARR) 
  . . . KILL OUT MERGE OUT=TEMP2
  NEW CODEPT SET CODEPT=""
  FOR  SET CODEPT=$ORDER(OUT(CODEPT)) QUIT:CODEPT=""  DO
  . NEW NAME SET NAME=$GET(OUT(CODEPT))
  . IF ISCORNER,ARC=1,(NAME'["Arc") KILL OUT(CODEPT) QUIT
  . IF ISCORNER,ARC=0,(NAME["Arc") KILL OUT(CODEPT) QUIT
  SET RESULT=$ORDER(OUT(""))
G4L1 ;  
  IF RESULT="" DO
  . NEW ASCII SET ASCII=$GET(@OPTIONREF@("ASCII"))
  . IF ASCII=1 SET RESULT="*"
  . IF RESULT="" SET RESULT=$GET(@OPTIONREF@("GET CH DEFAULT"))
  . IF RESULT="" SET RESULT=$GET(@OPTIONREF@("GET CH DEFAULT",1))
  . IF ASCII=2 SET RESULT=$GET(@OPTIONREF@("GET CH DEFAULT",2)) QUIT
  . IF RESULT'>0 DO    ;"WAS IF RESULT>0 DO
  . . NEW TEMPOPT
  . . SET TEMPOPT("THICK")=+RESULT
  . . SET TEMPOPT("ARC")=0,TEMPOPT("DASH")=0
  . . SET TEMPOPT("GET CH DEFAULT")=$GET(@OPTIONREF@("GET CH DEFAULT",2),"+")
  . . SET RESULT=$$GET4DIRS(REF,UP,DN,LF,RT,"TEMPOPT")
  . IF RESULT="" SET RESULT=$GET(@OPTIONREF@("GET CH DEFAULT",2)) IF RESULT'="" QUIT
  . IF $EXTRACT(RESULT,1)'="$" DO
  . . SET RESULT=$ASCII(RESULT)
  . . SET RESULT="$"_$$HEXCHR2^TMGMISC(RESULT)
G4DDN ;  
  QUIT RESULT
  ;
COLLAPSEMODES(ARR)  ;"Convert ARR(<MODE>,<CODEPOINT>) --> ARR(CODEPOINT)
  NEW TEMP
  NEW MODE SET MODE=""
  FOR  SET MODE=$ORDER(ARR(MODE)) QUIT:MODE=""  DO
  . NEW CODEPT SET CODEPT=""
  . FOR  SET CODEPT=$ORDER(ARR(MODE,CODEPT)) QUIT:CODEPT=""  DO
  . . IF $EXTRACT(CODEPT,1)'="$" QUIT
  . . SET TEMP(CODEPT)=$GET(ARR(MODE,CODEPT))
  KILL ARR MERGE ARR=TEMP
  QUIT
  ;
ADDNAMES(ARR,REF)  ;"(Debugging function) Add names to codepoints
  NEW CODEPT SET CODEPT=""
  FOR  SET CODEPT=$ORDER(ARR(CODEPT)) QUIT:CODEPT=""  DO
  . NEW NAME SET NAME=$GET(@REF@(CODEPT))
  . SET ARR(CODEPT)=NAME
  QUIT
  ;
OTHERDIR(OUT,DIR1,DIR2,DIR3,DIR4) ;"Return DIR that is not in DIR1,DIR2,DIR3
  SET OUT("UP")="",OUT("DN")="",OUT("LF")="",OUT("RT")=""
  IF $GET(DIR1)'="" KILL OUT(DIR1)
  IF $GET(DIR2)'="" KILL OUT(DIR2)
  IF $GET(DIR3)'="" KILL OUT(DIR3)
  IF $GET(DIR4)'="" KILL OUT(DIR4)
  QUIT
  ;
NOTARR(OUT,ARR1,ARR2)  ;"Return elements of ARR1 that are NOT in ARR2
  KILL OUT MERGE OUT=ARR1
  NEW CODEPT SET CODEPT=""
  FOR  SET CODEPT=$ORDER(ARR1(CODEPT)) QUIT:CODEPT=""  DO
  . IF $DATA(ARR2(CODEPT)) KILL OUT(CODEPT)
  QUIT
  ;
ANDARRS(OUT,ARR1,ARR2,ARR3,ARR4) ;"Return array of intersection (AND) of 2-4 array sets
  KILL OUT
  IF $DATA(ARR1)=0 DO
  . IF $DATA(ARR4) MERGE ARR1=ARR4 KILL ARR4 QUIT
  . IF $DATA(ARR3) MERGE ARR1=ARR3 KILL ARR3 QUIT
  . IF $DATA(ARR2) MERGE ARR1=ARR2 KILL ARR2 QUIT
  NEW CODEPT SET CODEPT=""
  FOR  SET CODEPT=$ORDER(ARR1(CODEPT)) QUIT:CODEPT=""  DO
  . IF $DATA(ARR2)>0,$DATA(ARR2(CODEPT))=0 QUIT
  . IF $DATA(ARR3)>0,$DATA(ARR3(CODEPT))=0 QUIT
  . IF $DATA(ARR4)>0,$DATA(ARR4(CODEPT))=0 QUIT
  . SET OUT(CODEPT)=""
  QUIT
  ;  
GETLINECHARSREF()  ;
  NEW REF SET REF=$NAME(^TMG("UNICODE","LINE CHARS"))
  DO ENSURELINECODES(REF)
  QUIT REF
  ;
ENSURELINECODES(REF,FORCE) ;"Ensure codes set up in global
  IF $GET(REF)="" QUIT
  IF $GET(FORCE)=1 KILL @REF
  IF $DATA(@REF)>0 QUIT
  NEW ARR DO GETLINECODES(.ARR)
  MERGE @REF=ARR
  QUIT
  ;
GETLINECODES(ARR) ;"Set up array of line drawing codes
  ;"Input: ARR -- PASS BY REFERENCE, AN OUT PARAMETER.  Format:
  ;"               ARR(<Hex CodePoint>)=<Unicode description>
  ;"               ARR(<Hex CodePoint>,<Direction>)=<Mode>
  ;"               ARR("XREF","ARC",<Mode>,<Dir>,<Hex CodePoint>)=""
  ;"               ARR("XREF","DASH",<Mult>,<Mode>,<Dir>,<Hex CodePoint>)=""
  ;"               ARR("XREF","DIR",<Dir>,<Mode>,<Hex CodePoint>)=""
  ;"               ARR("XREF","DIR",<Dir>,"ARC",<Mode>,<Hex CodePoint>>)=""
  ;"               ARR("XREF","DIR",<Dir>,"DASH",<Mult>,<Mode>,<Hex CodePoint>)=""
  ;"               ARR("XREF","MODE",<Mode>,<Dir>,<Hex CodePoint>)=""
  ;"             NOTE: <direction> will be UP, DN, LF, or RT
  ;"             NOTE: <Mode> will be Double, Single, Light, Heavy
  ;"             NOTE: <Mult> will be Quadruple, Triple, Double
  ;"Result: None. 
  NEW CODE,DONE,IDX SET DONE=0
  FOR IDX=1:1 DO  QUIT:DONE
  . NEW LINE SET LINE=$$TRIM^XLFSTR($TEXT(UNILINEREF+IDX))
  . IF LINE="" SET DONE=1 QUIT
  . SET LINE=$PIECE(LINE,";;",2,99)
  . IF LINE["<DONE>" SET DONE=1 QUIT
  . SET CODE=$PIECE(LINE,"^",1),LINE=$PIECE(LINE,"^",2,99)
  . SET ARR(CODE)=LINE
  . NEW TEMP 
  . IF LINE[" and ",LINE'["Arc" DO
  . . NEW S1 SET S1=$PIECE(LINE," and ",1)
  . . NEW S2 SET S2=$PIECE(LINE," and ",2)
  . . DO PARSE1(S1,.TEMP)
  . . NEW DIR1 SET DIR1=$ORDER(TEMP(""))
  . . NEW MODE1 SET MODE1=$GET(TEMP(DIR1))
  . . NEW TEMP2 DO PARSE1(S2,.TEMP2)
  . . NEW DIR2 SET DIR2=$ORDER(TEMP2(""))
  . . NEW MODE2 SET MODE2=$GET(TEMP2(DIR2))
  . . IF MODE2="" DO
  . . . KILL TEMP2("ZMODE","")
  . . . SET DIR2=""
  . . . FOR  SET DIR2=$ORDER(TEMP2(DIR2)) QUIT:(DIR2="")!($EXTRACT(DIR2,1)="Z")  DO
  . . . . SET TEMP2(DIR2)=MODE1
  . . . . SET TEMP2("ZMODE",MODE1,DIR2)=1
  . . MERGE TEMP=TEMP2
  . ELSE  DO
  . . DO PARSE1(LINE,.TEMP)
  . MERGE ARR(CODE)=TEMP
  NEW INDEX
  SET CODE=""
  FOR  SET CODE=$ORDER(ARR(CODE)) QUIT:(CODE="")  DO
  . NEW NAME SET NAME="" 
  . IF 1=1 SET NAME=$GET(ARR(CODE))
  . NEW MODE SET MODE=""
  . FOR  SET MODE=$ORDER(ARR(CODE,"ZMODE",MODE)) QUIT:MODE=""  DO
  . . NEW DIR SET DIR="" 
  . . FOR  SET DIR=$ORDER(ARR(CODE,"ZMODE",MODE,DIR)) QUIT:DIR=""  DO
  . . . SET INDEX("MODE",MODE,DIR,CODE)=NAME
  . . . SET INDEX("DIR",DIR,MODE,CODE)=NAME
  . . . KILL ARR(CODE,"ZMODE",MODE,DIR)
  . SET MODE=""
  . FOR  SET MODE=$ORDER(ARR(CODE,"ZARC",MODE)) QUIT:MODE=""  DO
  . . NEW DIR SET DIR="" 
  . . FOR  SET DIR=$ORDER(ARR(CODE,"ZARC",MODE,DIR)) QUIT:DIR=""  DO
  . . . SET INDEX("ARC",MODE,DIR,CODE)=NAME
  . . . SET INDEX("DIR",DIR,"ARC",MODE,CODE)=NAME
  . . . KILL ARR(CODE,"ZARC",MODE,DIR)
  . NEW MULT SET MULT=""
  . FOR  SET MULT=$ORDER(ARR(CODE,"ZDASH",MULT)) QUIT:MULT=""  DO
  . . FOR  SET MODE=$ORDER(ARR(CODE,"ZDASH",MULT,MODE)) QUIT:MODE=""  DO
  . . . NEW DIR SET DIR="" 
  . . . FOR  SET DIR=$ORDER(ARR(CODE,"ZDASH",MULT,MODE,DIR)) QUIT:DIR=""  DO
  . . . . SET INDEX("DASH",MULT,MODE,DIR,CODE)=NAME
  . . . . SET INDEX("DIR",DIR,"DASH",MULT,MODE,CODE)=NAME
  . . . . KILL ARR(CODE,"ZDASH",MULT,MODE,DIR)
  . . KILL ARR(CODE,"ZMODE")
  MERGE ARR("XREF")=INDEX  
  QUIT
  ;
PARSE1(STR,OUT)  ;  
  NEW L,R,U,D SET (L,R,U,D)=0
  NEW DASH SET DASH=""
  NEW ARC SET ARC=0
  NEW WORD
  NEW MODE SET MODE=""
  IF STR["Horizontal" DO
  . SET L=1,R=1
  . SET STR=$$TRIM^XLFSTR($$REPLSTR^TMGSTUT3(STR,"Horizontal",""))
  IF STR["Left" DO
  . SET L=1
  . SET STR=$$TRIM^XLFSTR($$REPLSTR^TMGSTUT3(STR,"Left",""))
  IF STR["Right" DO
  . SET R=1
  . SET STR=$$TRIM^XLFSTR($$REPLSTR^TMGSTUT3(STR,"Right",""))
  IF STR["Vertical" DO
  . SET U=1,D=1
  . SET STR=$$TRIM^XLFSTR($$REPLSTR^TMGSTUT3(STR,"Vertical",""))
  IF STR["Up" DO
  . SET U=1
  . SET STR=$$TRIM^XLFSTR($$REPLSTR^TMGSTUT3(STR,"Up",""))
  IF STR["Down" DO
  . SET D=1
  . SET STR=$$TRIM^XLFSTR($$REPLSTR^TMGSTUT3(STR,"Down",""))
  IF STR["Arc" DO
  . SET ARC=1
  . SET STR=$$TRIM^XLFSTR($$REPLSTR^TMGSTUT3(STR,"Arc",""))
  . SET STR=$$TRIM^XLFSTR($$REPLSTR^TMGSTUT3(STR,"and",""))
  ;"NOTE: The word DOUBLE can be used alone or as DASH modifier.  Check for dash first.  
  IF STR["Dash" DO
  . SET STR=$$TRIM^XLFSTR($$REPLSTR^TMGSTUT3(STR,"Dash",""))
  . SET DASH=""
  . NEW WORD FOR WORD="Double","Triple","Quadruple" DO
  . . IF STR'[WORD QUIT
  . . SET STR=$$TRIM^XLFSTR($$REPLSTR^TMGSTUT3(STR,WORD,""))
  . . SET DASH=WORD
  FOR WORD="Heavy","Light","Single","Double" IF STR[WORD DO
  . SET MODE=WORD              
  . SET STR=$$TRIM^XLFSTR($$REPLSTR^TMGSTUT3(STR,WORD,""))
  IF STR'="" do
  . WRITE "residual STR=",STR,!
  IF DASH'="" DO
  . IF MODE="" SET MODE="@"
  . IF U=1 SET OUT("UP","DASH")=DASH,OUT("ZDASH",DASH,MODE,"UP")=1
  . IF D=1 SET OUT("DN","DASH")=DASH,OUT("ZDASH",DASH,MODE,"DN")=1
  . IF L=1 SET OUT("LF","DASH")=DASH,OUT("ZDASH",DASH,MODE,"LF")=1
  . IF R=1 SET OUT("RT","DASH")=DASH,OUT("ZDASH",DASH,MODE,"RT")=1
  ELSE  DO
  . IF U=1 SET OUT("UP")=MODE,OUT("ZMODE",MODE,"UP")=1
  . IF D=1 SET OUT("DN")=MODE,OUT("ZMODE",MODE,"DN")=1
  . IF L=1 SET OUT("LF")=MODE,OUT("ZMODE",MODE,"LF")=1
  . IF R=1 SET OUT("RT")=MODE,OUT("ZMODE",MODE,"RT")=1
  IF ARC=1 DO
  . IF U=1 SET OUT("UP","ARC")=1,OUT("ZARC",MODE,"UP")=1
  . IF D=1 SET OUT("DN","ARC")=1,OUT("ZARC",MODE,"DN")=1
  . IF L=1 SET OUT("LF","ARC")=1,OUT("ZARC",MODE,"LF")=1
  . IF R=1 SET OUT("RT","ARC")=1,OUT("ZARC",MODE,"RT")=1
  QUIT
  ;
UNILINEREF ;
    ;;$2500^Light Horizontal                         
    ;;$2501^Heavy Horizontal                         
    ;;$2502^Light Vertical                           
    ;;$2503^Heavy Vertical                           
    ;;$2504^Light Triple Dash Horizontal             
    ;;$2505^Heavy Triple Dash Horizontal             
    ;;$2506^Light Triple Dash Vertical               
    ;;$2507^Heavy Triple Dash Vertical               
    ;;$2508^Light Quadruple Dash Horizontal          
    ;;$2509^Heavy Quadruple Dash Horizontal          
    ;;$250A^Light Quadruple Dash Vertical            
    ;;$250B^Heavy Quadruple Dash Vertical            
    ;;$250C^Light Down and Right                     
    ;;$250D^Down Light and Right Heavy               
    ;;$250E^Down Heavy and Right Light               
    ;;$250F^Heavy Down and Right                     
    ;;$2510^Light Down and Left                      
    ;;$2511^Down Light and Left Heavy                
    ;;$2512^Down Heavy and Left Light                
    ;;$2513^Heavy Down and Left                      
    ;;$2514^Light Up and Right                       
    ;;$2515^Up Light and Right Heavy                 
    ;;$2516^Up Heavy and Right Light                 
    ;;$2517^Heavy Up and Right                       
    ;;$2518^Light Up and Left                        
    ;;$2519^Up Light and Left Heavy                  
    ;;$251A^Up Heavy and Left Light                  
    ;;$251B^Heavy Up and Left                        
    ;;$251C^Light Vertical and Right                 
    ;;$251D^Vertical Light and Right Heavy           
    ;;$251E^Up Heavy and Right Down Light            
    ;;$251F^Down Heavy and Right Up Light            
    ;;$2520^Vertical Heavy and Right Light           
    ;;$2521^Down Light and Right Up Heavy            
    ;;$2522^Up Light and Right Down Heavy            
    ;;$2523^Heavy Vertical and Right                 
    ;;$2524^Light Vertical and Left                  
    ;;$2525^Vertical Light and Left Heavy            
    ;;$2526^Up Heavy and Left Down Light             
    ;;$2527^Down Heavy and Left Up Light             
    ;;$2528^Vertical Heavy and Left Light            
    ;;$2529^Down Light and Left Up Heavy             
    ;;$252A^Up Light and Left Down Heavy             
    ;;$252B^Heavy Vertical and Left                  
    ;;$252C^Light Down and Horizontal                
    ;;$252D^Left Heavy and Right Down Light          
    ;;$252E^Right Heavy and Left Down Light          
    ;;$252F^Down Light and Horizontal Heavy          
    ;;$2530^Down Heavy and Horizontal Light          
    ;;$2531^Right Light and Left Down Heavy          
    ;;$2532^Left Light and Right Down Heavy          
    ;;$2533^Heavy Down and Horizontal                
    ;;$2534^Light Up and Horizontal                  
    ;;$2535^Left Heavy and Right Up Light            
    ;;$2536^Right Heavy and Left Up Light            
    ;;$2537^Up Light and Horizontal Heavy            
    ;;$2538^Up Heavy and Horizontal Light            
    ;;$2539^Right Light and Left Up Heavy            
    ;;$253A^Left Light and Right Up Heavy            
    ;;$253B^Heavy Up and Horizontal                  
    ;;$253C^Light Vertical and Horizontal            
    ;;$253D^Left Heavy and Right Vertical Light      
    ;;$253E^Right Heavy and Left Vertical Light      
    ;;$253F^Vertical Light and Horizontal Heavy      
    ;;$2540^Up Heavy and Down Horizontal Light       
    ;;$2541^Down Heavy and Up Horizontal Light       
    ;;$2542^Vertical Heavy and Horizontal Light      
    ;;$2543^Left Up Heavy and Right Down Light       
    ;;$2544^Right Up Heavy and Left Down Light       
    ;;$2545^Left Down Heavy and Right Up Light       
    ;;$2546^Right Down Heavy and Left Up Light       
    ;;$2547^Down Light and Up Horizontal Heavy       
    ;;$2548^Up Light and Down Horizontal Heavy       
    ;;$2549^Right Light and Left Vertical Heavy      
    ;;$254A^Left Light and Right Vertical Heavy      
    ;;$254B^Heavy Vertical and Horizontal            
    ;;$254C^Light Double Dash Horizontal             
    ;;$254D^Heavy Double Dash Horizontal             
    ;;$254E^Light Double Dash Vertical               
    ;;$254F^Heavy Double Dash Vertical               
    ;;$2550^Double Horizontal                        
    ;;$2551^Double Vertical                          
    ;;$2552^Down Single and Right Double             
    ;;$2553^Down Double and Right Single             
    ;;$2554^Double Down and Right                    
    ;;$2555^Down Single and Left Double              
    ;;$2556^Down Double and Left Single              
    ;;$2557^Double Down and Left                     
    ;;$2558^Up Single and Right Double               
    ;;$2559^Up Double and Right Single               
    ;;$255A^Double Up and Right                      
    ;;$255B^Up Single and Left Double                
    ;;$255C^Up Double and Left Single                
    ;;$255D^Double Up and Left                       
    ;;$255E^Vertical Single and Right Double         
    ;;$255F^Vertical Double and Right Single         
    ;;$2560^Double Vertical and Right                
    ;;$2561^Vertical Single and Left Double          
    ;;$2562^Vertical Double and Left Single          
    ;;$2563^Double Vertical and Left                 
    ;;$2564^Down Single and Horizontal Double        
    ;;$2565^Down Double and Horizontal Single        
    ;;$2566^Double Down and Horizontal               
    ;;$2567^Up Single and Horizontal Double          
    ;;$2568^Up Double and Horizontal Single          
    ;;$2569^Double Up and Horizontal                 
    ;;$256A^Vertical Single and Horizontal Double    
    ;;$256B^Vertical Double and Horizontal Single    
    ;;$256C^Double Vertical and Horizontal           
    ;;$256D^Light Arc Down and Right                 
    ;;$256E^Light Arc Down and Left                  
    ;;$256F^Light Arc Up and Left                    
    ;;$2570^Light Arc Up and Right                   
    ;;$2574^Light Left                               
    ;;$2575^Light Up                                 
    ;;$2576^Light Right                              
    ;;$2577^Light Down                               
    ;;$2578^Heavy Left                               
    ;;$2579^Heavy Up                                 
    ;;$257A^Heavy Right                              
    ;;$257B^Heavy Down                               
    ;;$257C^Light Left and Heavy Right               
    ;;$257D^Light Up and Heavy Down                  
    ;;$257E^Heavy Left and Light Right               
    ;;$257F^Heavy Up and Light Down                  
    ;;<DONE>
    ;;$2571^Light Diagonal Upper Right To Lower Left 
    ;;$2572^Light Diagonal Upper Left To Lower Right 
    ;;$2573^Light Diagonal Cross                     
    ;
GETSHADEARR(ARR) ;
  NEW REF SET REF=$$GETSHADECHARSREF
  MERGE ARR=@REF;
  QUIT
  ;
GETSHADECHARSREF()  ;
  NEW REF SET REF=$NAME(^TMG("UNICODE","SHADE CHARS"))
  DO ENSURESHADECODES(REF)
  QUIT REF
  ;
ENSURESHADECODES(REF,FORCE) ;"Ensure codes set up in global
  IF $GET(REF)="" QUIT
  IF $GET(FORCE)=1 KILL @REF
  IF $DATA(@REF)>0 QUIT
  NEW ARR DO GETSHADECODES(.ARR)
  MERGE @REF=ARR
  QUIT
  ;  
GETSHADECODES(ARR) ;"Set up shading drawing codes
  ;"Input: ARR -- PASS BY REFERENCE, AN OUT PARAMETER.  Format:
  ;"Result: None. 
  NEW CODE,DONE,IDX SET DONE=0
  FOR IDX=1:1 DO  QUIT:DONE
  . NEW LINE SET LINE=$$TRIM^XLFSTR($TEXT(UNISHADEREF+IDX))
  . IF LINE="" SET DONE=1 QUIT
  . SET LINE=$PIECE(LINE,";;",2,99)
  . IF LINE["<DONE>" SET DONE=1 QUIT
  . SET CODE=$PIECE(LINE,"^",1),LINE=$PIECE(LINE,"^",2,99)
  . SET ARR(CODE)=LINE
  . SET ARR("XREF",LINE)=CODE
  QUIT
  ;
UNISHADEREF ;
    ;;$2591^Light Shade                  
    ;;$2592^Medium Shade               
    ;;$2593^Dark Shade                  
    ;;<DONE>
