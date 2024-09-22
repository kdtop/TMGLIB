TMGUSRI8 ;TMG/kst/USER INTERFACE -- Terminal Color Picker ;9/20/24
         ;;1.0;TMG-LIB;**1**;3/31/24
 ;
 ;"TMG USER INTERFACE API FUNCTIONS
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 9/24/24  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"======================================================================= 
 ;"COLORBOX(SETBG,SETFG)  -- WRITE a grid on the screen, showing all the color combos
 ;"PICKCOLOR24(OPTION)
 ;"COLORMENU() -- Allow user to pick color by name in a menu structure. 
 ;"WEBCOLOR(NAME,COLORARR) --Get color vector, based on standardized web color names
 ;"LIGHTERCLR(CLRVEC,LIGHTPCT) ;
 ;"DARKERCLR(CLRVEC,DARKPCT) ;
 ;"DELTACOLOR(CLRVEC,SHADEPCT) ;
 ;
 ;"=======================================================================
 ;"INDEX COLOR FUNCTIONS
 ;"=======================================================================
 ;"PICK1COL(LABEL,INITVAL)   --prompt user to pick a color
 ;"PICKFGC(FG,BG)   -- prompt user to pick a foreground color
 ;"PICKBGC(INITVAL)   --prompt user to pick a background color
 ;"PICKCLRS(FG,BG)   --prompt user to pick a FG and BG colors
 ;"COLORPAIR(FG,BG,ARR) --Return a 'FG^BG' based on names.  INDEXED COLOR MODE
 ;"SETGBLCO   --Set Global Colors
 ;"KILLGBLC   --Kill Global Colors and MAP array
 ;"=======================================================================
 ;"256 COLOR FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"24BIT COLOR FUNCTIONS
 ;"=======================================================================
 ;"DRAWCLRBOX24(RA,GA,BA,OPTION) -- Do actual drawing of 3D color box
 ;
 ;"=======================================================================
 ;"Demo Functions
 ;"=======================================================================
 ;"DEMOCOLR(OPTION)   --Show color demo. Indexed colors, 256 color, or 24bit color
 ;"DEMO24 --DEMONSTRATION OF 24 BIT COLOR 
 ;"=======================================================================
 ;"Private Functions
 ;"=======================================================================
 ;"SETUPCHARS(CHARS) 
 ;"GETIDXMAP(MAPARR) 
 ;"MAPIDXTO24BIT(IDXCOLOR,ISBKGND) 
 ;"DRAWPART(FACE,RA,GA,BA,RISMAX,GISMAX,BISMAX,OPTION) ;
 ;"DELTA1CLR(ARR,PCT) 
 ;"BYTEVAL(A) ;"
 ;"AXISPCT(A) ;"
 ;"XFRM(FACE,RA,GA,BA,OPTION) --Transform (XFRM) RGB coordinates into XY screen coordinates.  
 ;"WEBCOLOR(NAME,COLORARR) --Get color vector, based on standardized web color names
 ;"GETWEBCOLORS(COLORARR) ;
 ;"SETUPCOLORMENU(MENU) -- Setup menu array for use with RUNMENU^TMGUSRI7
 ;"DELTA1CLR(V,PCT) --PCT SHOULD BE 0-1
 ;"DELTAINT(V,DELTA) 
 ;"DRAWLINE(CHARS,AXIS,START,STOP,R1,G1,B1,STEP,NOSTARTDOT,NOSTOPDOT) ;
 ;"SPLITPOS(POS,X,Y) ;
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;"DEPENDENCIES:      
 ;"=======================================================================
 ; 
DEMOCOLR(OPTION)   ;"Show color demo
  ;"INPUT:  OPTION -- PASS BY REFERENCE.  
  ;"          OPTION("MODE") -- Optional. 
  ;"            OPTION("MODE")="INDEXED"  <-- default value if not passed.  Traditional indexed colors 
  ;"            OPTION("MODE")="256"   <-- 256 (8 bit) color -- not fully implemented. 
  ;"            OPTION("MODE")="24bit"  <-- true colors, 24 bit (3 byte)
  NEW COLORMODE SET COLORMODE=$GET(OPTION("MODE"),"INDEXED")
  IF COLORMODE="INDEXED" DO
  .  ;"Purpose: to WRITE a lines on the screen, showing all the color combos
  . DO VCUSAV2^TMGTERM
  . NEW FG,BG
  . FOR BG=0:1:15 DO
  . . FOR FG=0:1:15 DO
  . . . DO VCOLORSIDX^TMGTERM(FG,BG)
  . . . WRITE "Text with background color #",BG," and foreground color #",FG
  . . . DO VTATRIB^TMGTERM(0)
  . . . WRITE !
  . DO VCULOAD2^TMGTERM
  ELSE  IF COLORMODE="256" DO
  . ;"to be implemented.   
  ELSE  IF COLORMODE="24bit" DO
  . DO DEMO24 ;"DEMONSTRATION OF 24 BIT COLOR
  QUIT
  ;
DEMO24 ;"DEMONSTRATION OF 24 BIT COLOR
  ;"Putty must be configured             
  ;"   In Settings > Windows > Colours there is a check box for "Allow terminal to use xterm 256-colour mode".
  ;"   Change Settings -> Connection > Data > Terminal-type string to: 'xterm-24bit'  
  WRITE #
  NEW R,G,B
  NEW X,Y,Z
  NEW XMAX,YMAX,ZMAX SET XMAX=80,YMAX=25,ZMAX=20
  FOR Z=1:1:ZMAX DO
  . SET R=(255*(Z/ZMAX))\1
  . FOR Y=1:1:YMAX DO
  . . DO CUP^TMGTERM(Z,Y+Z)
  . . SET G=(255*(Y/YMAX))\1
  . . FOR X=1:1:XMAX DO
  . . . SET B=(255*(X/XMAX))\1
  . . . NEW FG SET FG=$$CLRVEC24^TMGTERM(R,G,B)
  . . . NEW BG SET BG=$$CLRVEC24^TMGTERM(255-R,255-G,255-B)
  . . . DO VCOLOR24B^TMGTERM(FG,BG)
  . . . WRITE "#" 
  . HANG 0.5
  WRITE !
  QUIT  
  ;    
COLORBOX(SETBG,SETFG)  ;"WRITE a grid on the screen, showing all the color combos. INDEXED COLORS MODE
  ;"Input: SETBG -- OPTIONAL.  If data sent, then ONLY that background will be shown.
  ;"        (i.e. for only picking a foreground color)
  ;"  SETFG -- OPTIONAL.  If data sent, then only for picking background color
  NEW FG,BG
  IF $DATA(SETFG)#10=0 DO
  . WRITE "FG:",?10
  . FOR FG=0:1:15 DO
  . . WRITE $$RJ^XLFSTR(FG,2)," "
  . WRITE !
  NEW START,FINISH
  SET START=0,FINISH=15
  IF ($DATA(SETBG)#10=1) DO
  . SET (START,FINISH)=SETBG
  FOR BG=START:1:FINISH DO
  . IF BG=0 WRITE "BG:"
  . WRITE ?7,$$RJ^XLFSTR(BG,2),?10
  . FOR FG=0:1:15 DO
  . . DO VCOLORSIDX^TMGTERM(FG,BG)
  . . IF $DATA(SETFG)#10=0 DO
  . . . WRITE " X "
  . . ELSE  DO
  . . . WRITE "   "
  . . DO VTATRIB^TMGTERM(0)
  . WRITE !
  QUIT
  ;  
  ;"=======================================================================
WEBCOLOR(NAME,COLORARR) ;"Get color vector, based on standardized web color names
  ;"Input: NAME -- NAME OF COLOR.  NOT Case sensitive
  ;"       COLORARR -- OPTIONAL.  PASS BY REFERENCE.  If provided, then data pulled from array.  Filled if empty.  
  ;"Result: color vector, e.g. '255;192;203'
  NEW UNAME SET UNAME=$$UP^XLFSTR($GET(NAME))
  IF $DATA(COLORARR)=0 DO GETWEBCOLORS(.COLORARR)
  QUIT $GET(COLORARR("NAME",UNAME))
  ;
SETUPCOLORMENU(MENU) ;"Setup menu array for use with RUNMENU^TMGUSRI7
  NEW ARR DO GETWEBCOLORS(.ARR)
  NEW IDX SET IDX=1
  NEW SUBMENU SET SUBMENU=2
  NEW SECTION SET SECTION=""
  FOR  SET SECTION=$ORDER(ARR("SECTION",SECTION)) QUIT:SECTION=""  DO
  . SET MENU(1,IDX)="&"_SECTION
  . SET MENU(1,IDX,"SUBMENU")=SUBMENU
  . SET MENU(SUBMENU,"PARENT")=1
  . NEW UNAME SET UNAME=""
  . NEW JDX SET JDX=1
  . FOR  SET UNAME=$ORDER(ARR("SECTION",SECTION,UNAME)) QUIT:UNAME=""  DO
  . . NEW ENTRY SET ENTRY=$GET(ARR("SECTION",SECTION,UNAME))
  . . NEW CLRVEC24 SET CLRVEC24=$PIECE(ENTRY,"^",1)
  . . NEW NAME SET NAME=$PIECE(ENTRY,"^",2)
  . . NEW INVCLR SET INVCLR=$$INVCLRVEC^TMGTERM(CLRVEC24)
  . . SET MENU(SUBMENU,JDX)=NAME
  . . SET MENU(SUBMENU,JDX,"DATA")=CLRVEC24
  . . SET MENU(SUBMENU,JDX,"COLOR","TEXT")=CLRVEC24_"^"_INVCLR
  . . SET MENU(SUBMENU,JDX,"COLOR","SELTEXT")=$$LIGHTERCLR(CLRVEC24,0.2)_"^"_$$LIGHTERCLR(INVCLR,0.2)
  . . SET JDX=JDX+1
  . SET SUBMENU=SUBMENU+1
  . SET IDX=IDX+1
  SET MENU(1,IDX)="^ to QUIT"
  QUIT
  ;
COLORMENU() ;"Allow user to pick color by name in a menu structure. 
  NEW MENU DO SETUPCOLORMENU(.MENU)
  NEW RESULT SET RESULT=$$RUNMENU^TMGUSRI7(.MENU)
  QUIT RESULT
  ;
GETWEBCOLORS(COLORARR) ;
  NEW LINE,NAME,UNAME,VEC
  NEW DONE SET DONE=0
  NEW SECTION SET SECTION="?"
  NEW IDX FOR IDX=1:1 DO  QUIT:DONE
  . SET LINE=$TEXT(WEBCOLORL1+IDX^TMGUSRI8)
  . IF LINE["<DONE>" SET DONE=1 QUIT
  . SET LINE=$PIECE(LINE,";;""",2)
  . IF LINE["@" DO  QUIT
  . . SET LINE=$$TRIM^XLFSTR($PIECE(LINE,"@",2))
  . . IF LINE="" QUIT
  . . IF LINE["----" QUIT
  . . SET SECTION=LINE
  . SET NAME=$PIECE(LINE,",",1)
  . SET VEC=$PIECE(LINE,",",2) 
  . SET UNAME=$$UP^XLFSTR(NAME)
  . SET COLORARR("NAME",UNAME)=VEC
  . SET COLORARR("SECTION",SECTION,UNAME)=VEC_"^"_NAME
  QUIT
  ;
WEBCOLORL1 ;  
  ;;"@Pink colors
  ;;"@------------
  ;;"MediumVioletRed,199;21;133
  ;;"DeepPink,255;20;147
  ;;"PaleVioletRed,219;112;147
  ;;"HotPink,255;105;180
  ;;"LightPink,255;182;193
  ;;"Pink,255;192;203
  ;;"@
  ;;"@Red colors
  ;;"@------------
  ;;"DarkRed,139;0;0
  ;;"Red,255;0;0
  ;;"Firebrick,178;34;34
  ;;"Crimson,220;20;60
  ;;"IndianRed,205;92;92
  ;;"LightCoral,240;128;128
  ;;"Salmon,250;128;114
  ;;"DarkSalmon,233;150;122
  ;;"LightSalmon,255;160;122
  ;;"@
  ;;"@Orange colors
  ;;"@------------
  ;;"OrangeRed,255;69;0
  ;;"Tomato,255;99;71
  ;;"DarkOrange,255;140;0
  ;;"Coral,255;127;80
  ;;"Orange,255;165;0
  ;;"@
  ;;"@Yellow colors
  ;;"@------------
  ;;"DarkKhaki,189;183;107
  ;;"Gold,255;215;0
  ;;"Khaki,240;230;140
  ;;"PeachPuff,255;218;185
  ;;"Yellow,255;255;0
  ;;"PaleGoldenrod,238;232;170
  ;;"Moccasin,255;228;181
  ;;"PapayaWhip,255;239;213
  ;;"LightGoldenrodYellow,250;250;210
  ;;"LemonChiffon,255;250;205
  ;;"LightYellow,255;255;224
  ;;"@
  ;;"@Brown colors
  ;;"@------------
  ;;"Maroon,128;0;0
  ;;"Brown,165;42;42
  ;;"SaddleBrown,139;69;19
  ;;"Sienna,160;82;45
  ;;"Chocolate,210;105;30
  ;;"DarkGoldenrod,184;134;11
  ;;"Peru,205;133;63
  ;;"RosyBrown,188;143;143
  ;;"Goldenrod,218;165;32
  ;;"SandyBrown,244;164;96
  ;;"Tan,210;180;140
  ;;"Burlywood,222;184;135
  ;;"Wheat,245;222;179
  ;;"NavajoWhite,255;222;173
  ;;"Bisque,255;228;196
  ;;"BlanchedAlmond,255;235;205
  ;;"Cornsilk,255;248;220
  ;;"@
  ;;"@Purple;violet;and magenta colors
  ;;"@------------
  ;;"Indigo,75;0;130
  ;;"Purple,128;0;128
  ;;"DarkMagenta,139;0;139
  ;;"DarkViolet,148;0;211
  ;;"DarkSlateBlue,72;61;139
  ;;"BlueViolet,138;43;226
  ;;"DarkOrchid,153;50;204
  ;;"Fuchsia,255;0;255
  ;;"Magenta,255;0;255
  ;;"SlateBlue,106;90;205
  ;;"MediumSlateBlue,123;104;238
  ;;"MediumOrchid,186;85;211
  ;;"MediumPurple,147;112;219
  ;;"Orchid,218;112;214
  ;;"Violet,238;130;238
  ;;"Plum,221;160;221
  ;;"Thistle,216;191;216
  ;;"Lavender,230;230;250
  ;;"@
  ;;"@Blue colors
  ;;"@------------
  ;;"MidnightBlue,25;25;112
  ;;"Navy,0;0;128
  ;;"DarkBlue,0;0;139
  ;;"MediumBlue,0;0;205
  ;;"Blue,0;0;255
  ;;"RoyalBlue,65;105;225
  ;;"SteelBlue,70;130;180
  ;;"DodgerBlue,30;144;255
  ;;"DeepSkyBlue,0;191;255
  ;;"CornflowerBlue,100;149;237
  ;;"SkyBlue,135;206;235
  ;;"LightSkyBlue,135;206;250
  ;;"LightSteelBlue,176;196;222
  ;;"LightBlue,173;216;230
  ;;"PowderBlue,176;224;230
  ;;"@
  ;;"@Cyan colors
  ;;"@------------
  ;;"Teal,0;128;128
  ;;"DarkCyan,0;139;139
  ;;"LightSeaGreen,32;178;170
  ;;"CadetBlue,95;158;160
  ;;"DarkTurquoise,0;206;209
  ;;"MediumTurquoise,72;209;204
  ;;"Turquoise,64;224;208
  ;;"Aqua,0;255;255
  ;;"Cyan,0;255;255
  ;;"Aquamarine,127;255;212
  ;;"PaleTurquoise,175;238;238
  ;;"LightCyan,224;255;255
  ;;"
  ;;"Green colors
  ;;"@------------
  ;;"DarkGreen,0;100;0
  ;;"Green,0;128;0
  ;;"DarkOliveGreen,85;107;47
  ;;"ForestGreen,34;139;34
  ;;"SeaGreen,46;139;87
  ;;"Olive,128;128;0
  ;;"OliveDrab,107;142;35
  ;;"MediumSeaGreen,60;179;113
  ;;"LimeGreen,50;205;50
  ;;"Lime,0;255;0
  ;;"SpringGreen,0;255;127
  ;;"MediumSpringGreen,0;250;154
  ;;"DarkSeaGreen,143;188;143
  ;;"MediumAquamarine,102;205;170
  ;;"YellowGreen,154;205;50
  ;;"LawnGreen,124;252;0
  ;;"Chartreuse,127;255;0
  ;;"LightGreen,144;238;144
  ;;"GreenYellow,173;255;47
  ;;"PaleGreen,152;251;152
  ;;"@
  ;;"@White colors
  ;;"@------------
  ;;"MistyRose,255;228;225
  ;;"AntiqueWhite,250;235;215
  ;;"Linen,250;240;230
  ;;"Beige,245;245;220
  ;;"WhiteSmoke,245;245;245
  ;;"LavenderBlush,255;240;245
  ;;"OldLace,253;245;230
  ;;"AliceBlue,240;248;255
  ;;"Seashell,255;245;238
  ;;"GhostWhite,248;248;255
  ;;"Honeydew,240;255;240
  ;;"FloralWhite,255;250;240
  ;;"Azure,240;255;255
  ;;"MintCream,245;255;250
  ;;"Snow,255;250;250
  ;;"Ivory,255;255;240
  ;;"White,255;255;255
  ;;"Gray and black colors
  ;;"Black,0;0;0
  ;;"DarkSlateGray,47;79;79
  ;;"DimGray,105;105;105
  ;;"SlateGray,112;128;144
  ;;"Gray,128;128;128
  ;;"LightSlateGray,119;136;153
  ;;"DarkGray,169;169;169
  ;;"Silver,192;192;192
  ;;"LightGray,211;211;211                                                               
  ;;"Gainsboro,220;220;220   
  ;;"<DONE>                                               
  ;  
  ;"=======================================================================
  ;"=======================================================================
COLORPAIR(FG,BG,ARR) ;"Return a 'FG^BG' based on names.  INDEXED COLOR MODE
  ;"Input: FG -- the name (as defined below) of foreground color
  ;"       BG -- the name (as defined below) of background color
  ;"       ARR -- OPTIONAL.  PASS BY REFERENCE.  Can use with repeated calls to save time.  
  IF $DATA(TMGCOLBLACK)=0 DO SETGBLCO
  IF $DATA(ARR)=0 DO       
  . SET ARR("FG","BLACK")=TMGCOLBLACK                    ;"0
  . SET ARR("FG","RED")=TMGCOLRED                        ;"1
  . SET ARR("FG","GREEN")=TMGCOLGREEN                    ;"2
  . SET ARR("FG","YELLOW")=TMGCOLYELLOW                  ;"3
  . SET ARR("FG","BLUE")=TMGCOLBLUE                      ;"4
  . SET ARR("FG","MAGENTA")=TMGCOLMAGENTA                ;"5
  . SET ARR("FG","CYAN")=TMGCOLCYAN                      ;"6
  . SET ARR("FG","GREY")=TMGCOLGREY                      ;"7
  . SET ARR("FG","BRIGHT RED")=TMGCOLBRED                ;"8
  . SET ARR("FG","BRIGHT GREEN")=TMGCOLBGREEN            ;"9
  . SET ARR("FG","BRIGHT YELLOW")=TMGCOLBYELLOW          ;"10
  . SET ARR("FG","BRIGHT BLUE")=TMGCOLBBLUE              ;"11
  . SET ARR("FG","BRIGHT MAGENTA")=TMGCOLBMAGENTA        ;"12
  . SET ARR("FG","BRIGHT CYAN")=TMGCOLBCYAN              ;"13
  . SET ARR("FG","WHITE")=TMGCOLFGBWHITE                 ;"14
  . SET ARR("FG","DARK RED")=TMGCOLDKRED                 ;"15
  . ;
  . SET ARR("BG","BLACK")=TMGCOLBLACK                    ;"0
  . SET ARR("BG","RED")=TMGCOLRED                        ;"1
  . SET ARR("BG","GREEN")=TMGCOLGREEN                    ;"2
  . SET ARR("BG","YELLOW")=TMGCOLYELLOW                  ;"3
  . SET ARR("BG","BLUE")=TMGCOLBLUE                      ;"4
  . SET ARR("BG","MAGENTA")=TMGCOLMAGENTA                ;"5
  . SET ARR("BG","CYAN")=TMGCOLCYAN                      ;"6
  . SET ARR("BG","GREY")=TMGCOLGREY                      ;"7
  . SET ARR("BG","BRIGHT RED")=TMGCOLBRED                ;"8
  . SET ARR("BG","BRIGHT GREEN")=TMGCOLBGREEN            ;"9
  . SET ARR("BG","BRIGHT YELLOW")=TMGCOLBYELLOW          ;"10
  . SET ARR("BG","BRIGHT BLUE")=TMGCOLBBLUE              ;"11
  . SET ARR("BG","BRIGHT MAGENTA")=TMGCOLBMAGENTA        ;"12
  . SET ARR("BG","BRIGHT CYAN")=TMGCOLBCYAN              ;"13
  . SET ARR("BG","DARK GREY")=TMGCOLBGREY                ;"14
  . SET ARR("BG","WHITE")=TMGCOLWHITE                    ;"15
  NEW FGC SET FGC=$GET(ARR("FG",$$UP^XLFSTR(FG)),0)
  NEW BGC SET BGC=$GET(ARR("BG",$$UP^XLFSTR(BG)),15)
  SET RESULT=FGC_"^"_BGC
  QUIT RESULT
  ;    
SETGBLCO   ;"Set Global Colors
  SET TMGCOLBLACK=0
  SET TMGCOLRED=1
  SET TMGCOLGREEN=2
  SET TMGCOLYELLOW=3
  SET TMGCOLBLUE=4
  SET TMGCOLMAGENTA=5
  SET TMGCOLCYAN=6
  SET TMGCOLGREY=7
  ;
  SET TMGCOLBRED=8       ;"'Bright' color
  SET TMGCOLBGREEN=9     ;"'Bright' color
  SET TMGCOLBYELLOW=10   ;"'Bright' color
  SET TMGCOLBBLUE=11     ;"'Bright' color
  SET TMGCOLBMAGENTA=12  ;"'Bright' color
  SET TMGCOLBCYAN=13     ;"'Bright' color
  SET TMGCOLBGREY=14     ;"//BACKGROUND COLOR
  SET TMGCOLFGBWHITE=14  ;"FOREGROUND COLOR
  SET TMGCOLWHITE=15     ;"//BACKGROUND COLOR
  SET TMGCOLDKRED=15     ;"FOREGROUND COLOR
   ;
  DO GETIDXMAP(.TMGCOLMAP)  ;"Maps indexed colors to 24bit colors
  QUIT
  ;  
MAPIDXTO24BIT(IDXCOLOR,ISBKGND) ;"
  ;"Input: IDXCOLOR -- the color index from 4 bit mode
  ;"       ISBKGND -- Optional. Default = 0 (false)
  ;"Result: CVec color triple. 
  IF $DATA(TMGCOLMAP)=0 DO GETIDXMAP(.TMGCOLMAP)
  SET ISBKGND=+$GET(ISBKGND)
  NEW NODE SET NODE=$SELECT(ISBKGND=1:"BG",1:"FG")
  NEW RESULT SET RESULT=$GET(TMGCOLMAP(NODE,IDXCOLOR))
  NEW NAME SET NAME=$GET(TMGCOLMAP(NODE,IDXCOLOR,"NAME"))
  QUIT RESULT
  ;  
KILLGBLC   ;"Kill Global Colors and MAP array
  KILL TMGCOLBLACK
  KILL TMGCOLRED
  KILL TMGCOLGREEN
  KILL TMGCOLYELLOW
  KILL TMGCOLBLUE
  KILL TMGCOLMAGENTA
  KILL TMGCOLCYAN
  KILL TMGCOLGREY
  ;
  KILL TMGCOLBRED
  KILL TMGCOLBGREEN
  KILL TMGCOLBYELLOW
  KILL TMGCOLBBLUE
  KILL TMGCOLBMAGENTA
  KILL TMGCOLBCYAN
  KILL TMGCOLBGREY
  KILL TMGCOLWHITE
  KILL TMGCOLFGBWHITE
  KILL TMGCOLDKRED  
  ;
  KILL TMGCOLMAP
  QUIT
  ;
  ;"=======================================================================
PICK1COL(LABEL,INITVAL)   ;
   ;"Purpose: prompt user to pick a color
   ;"Input: LABEL -- Foreground or background
   ;"  INITVAL.  Value to return IF nothing selected.
   ;"Results: returns value 0-15 IF selected, or -1 IF abort.
  NEW RESULT
  WRITE "Enter "_LABEL_" color number (0-15,^ to abort): "
  READ RESULT:$GET(DTIME,3600),!
  IF (RESULT="")!(+RESULT'=RESULT)!(+RESULT<0)!(+RESULT>15) SET RESULT=+$GET(INITVAL)
  QUIT RESULT
  ;
PICKFGC(FG,BG)   ;
   ;"Purpose: prompt user to pick a foreground color
   ;"Input -- FG.  Value to return IF nothing selected.
   ;"Results: returns value 0-15 IF selected, or -1 IF abort.
  DO COLORBOX(.BG)
  NEW RESULT SET RESULT=$$PICK1COL("Foreground (FG)",.FG)
  QUIT RESULT
  ;
PICKBGC(INITVAL)   ;
   ;"Purpose: prompt user to pick a background color
   ;"Input -- INITVAL.  Value to return IF nothing selected.
   ;"Results: returns value 0-15 IF selected, or -1 IF abort.
  DO COLORBOX(,1)
  NEW RESULT SET RESULT=$$PICK1COL("Background (BG)",.INITVAL)
  QUIT RESULT
  ;
PICKCLRS(FG,BG)   ;
   ;"Purpose: prompt user to pick a FG and BG colors
   ;"Results: returns value FG^BG, each 0-15
  DO COLORBOX()
  SET FG=$$PICK1COL("Foreground (FG)",.FG)
  SET BG=$$PICK1COL("Background (BG)",.BG)
  QUIT FG_"^"_BG
  ;  
  ;"=======================================================================
GETIDXMAP(MAPARR) ;
  NEW LINE,NAME,UNAME,VEC
  NEW START SET START=0
  NEW DONE SET DONE=0
  NEW IDX FOR IDX=1:1 DO  QUIT:DONE
  . SET LINE=$TEXT(INDEXMAP+IDX^TMGUSRI8)
  . IF LINE["=====" SET START=1 QUIT
  . IF START=0 QUIT
  . IF LINE["<DONE>" SET DONE=1 QUIT
  . SET LINE=$PIECE(LINE,";;""",2)
  . NEW FGIDX SET FGIDX=$$TRIM^XLFSTR($PIECE(LINE,"|",3))
  . NEW BGIDX SET BGIDX=$$TRIM^XLFSTR($PIECE(LINE,"|",4))
  . SET NAME=$$TRIM^XLFSTR($PIECE(LINE,"|",5))
  . NEW CLRVEC SET CLRVEC=$$TRIM^XLFSTR($PIECE(LINE,"|",8))  ;"Windows Powershell colors.
  . SET MAPARR("FG",FGIDX)=CLRVEC
  . SET MAPARR("FG",FGIDX,"NAME")=NAME
  . SET MAPARR("BG",BGIDX)=CLRVEC
  . SET MAPARR("BG",BGIDX,"NAME")=NAME
  QUIT
  ;
INDEXMAP ;" Taken from here: https://en.wikipedia.org/wiki/ANSI_escape_code.  NOTE: I'm not sure what to do about FG=15 and BG=7.  Mapping to WHITE for now
 ;;"        |        |         |        |                       |              |                |                 |  Visual       |               |                 |               |               |              |                |
 ;;"        |        |         |        |                       |              | Windows XP     | Windows         |  Studio       |  Windows 10   |                 |               |               |              |                | 
 ;;"FG      | BG     | FG Idx  | BG Idx | Name                  | VGA          | Console        | PowerShell      |  Code         |  Console      | Terminal.app    |  PuTTY        | mIRC          | xterm        | Ubuntu         | Eclipse Terminal 
 ;;"=====   | =====  | =====   | =====  | =====                 | =====        | =====          | =====           |  =====        |  =====        | =====           |  =====        | =====         | =====        | =====          | =====
 ;;"30      | 40     | 0       | 0      | Black                 | 0;0;0        | 0;0;0          | 0;0;0           |  0;0;0        |  12;12;12     | 0;0;0           |  0;0;0        | 0;0;0         | 0;0;0        | 1;1;1          | 0;0;0
 ;;"31      | 41     | 1       | 1      | Red                   | 170;0;0      | 128;0;0        | 128;0;0         |  205;49;49    |  197;15;31    | 153;0;0         |  187;0;0      | 127;0;0       | 205;0;0      | 222;56;43      | 205;0;0
 ;;"32      | 42     | 2       | 2      | Green                 | 0;170;0      | 0;128;0        | 0;128;0         |  13;188;121   |  19;161;14    | 0;166;0         |  0;187;0      | 0;147;0       | 0;205;0      | 57;181;74      | 0;205;0
 ;;"33      | 43     | 3       | 3      | Yellow                | 170;85;0     | 128;128;0      | 238;237;240     |  229;229;16   |  193;156;0    | 153;153;0       |  187;187;0    | 252;127;0     | 205;205;0    | 255;199;6      | 205;205;0
 ;;"34      | 44     | 4       | 4      | Blue                  | 0;0;170      | 0;0;128        | 0;0;128         |  36;114;200   |  0;55;218     | 0;0;178         |  0;0;187      | 0;0;127       | 0;0;238      | 0;111;184      | 0;0;238
 ;;"35      | 45     | 5       | 5      | Magenta               | 170;0;170    | 128;0;128      | 1;36;86         |  188;63;188   |  136;23;152   | 178;0;178       |  187;0;187    | 156;0;156     | 205;0;205    | 118;38;113     | 205;0;205
 ;;"36      | 46     | 6       | 6      | Cyan                  | 0;170;170    | 0;128;128      | 0;128;128       |  17;168;205   |  58;150;221   | 0;166;178       |  0;187;187    | 0;147;147     | 0;205;205    | 44;181;233     | 0;205;205
 ;;"37      | 47     | 15      | 7      | White                 | 170;170;170  | 192;192;192    | 192;192;192     |  229;229;229  |  204;204;204  | 191;191;191     |  187;187;187  | 210;210;210   | 229;229;229  | 204;204;204    | 229;229;229
 ;;"90      | 100    | 7       | 14     | Bright  Black (Gray)  | 85;85;85     | 128;128;128    | 128;128;128     |  102;102;102  |  118;118;118  | 102;102;102     |  85;85;85     | 127;127;127   | 127;127;127  | 128;128;128    | 0;0;0
 ;;"91      | 101    | 8       | 8      | Bright  Red           | 255;85;85    | 255;0;0        | 255;0;0         |  241;76;76    |  231;72;86    | 230;0;0         |  255;85;85    | 255;0;0       | 255;0;0      | 255;0;0        | 255;0;0           
 ;;"92      | 102    | 9       | 9      | Bright  Green         | 85;255;85    | 0;255;0        | 0;255;0         |  35;209;139   |  22;198;12    | 0;217;0         |  85;255;85    | 0;252;0       | 0;255;0      | 0;255;0        | 0;255;0
 ;;"93      | 103    | 10      | 10     | Bright  Yellow        | 255;255;85   | 255;255;0      | 255;255;0       |  245;245;67   |  249;241;165  | 230;230;0       |  255;255;85   | 255;255;0     | 255;255;0    | 255;255;0      | 255;255;0
 ;;"94      | 104    | 11      | 11     | Bright  Blue          | 85;85;255    | 0;0;255        | 0;0;255         |  59;142;234   |  59;120;255   | 0;0;255         |  85;85;255    | 0;0;252       | 92;92;255    | 0;0;255        | 92;92;255
 ;;"95      | 105    | 12      | 12     | Bright  Magenta       | 255;85;255   | 255;0;255      | 255;0;255       |  214;112;214  |  180;0;158    | 230;0;230       |  255;85;255   | 255;0;255     | 255;0;255    | 255;0;255      | 255;0;255
 ;;"96      | 106    | 13      | 13     | Bright  Cyan          | 85;255;255   | 0;255;255      | 0;255;255       |  41;184;219   |  97;214;214   | 0;230;230       |  85;255;255   | 0;255;255     | 0;255;255    | 0;255;255      | 0;255;255
 ;;"97      | 107    | 14      | 15     | Bright  White         | 255;255;255  | 255;255;255    | 255;255;255     |  229;229;229  |  242;242;242  | 230;230;230     |  255;255;255  | 255;255;255   | 255;255;255  | 255;255;255    | 255;255;255
 ;;"<DONE>                                               
 ;  
PICKCOLOR24(OPTION) ;"Display colors and allow user to pick desired color.  
  ;"Input: OPTION.  OPTIONAL.  PASS BY REFERENCE.
  ;"         OPTION("ORIGIN X"),OPTION("ORIGIN Y") -- screen coordinates for centerpoint, where R=G=B=255.  Default is (23,31)
  ;"         OPTION("WIDTH")      
  ;"         OPTION("HEIGHT")      NOTE: if value set 15 or higher, causes bug, not sure why yet.
  ;"         OPTION("DEPTH")       NOTE: if value set 15 or higher, causes bug, not sure why yet.
  ;"         OPTION("SHOW FRAME")  if 1, outer frame shown
  ;"         OPTION("SHOW SELECTED") if 1 then a box displaying color is shown
  ;"         OPTION("CLEAR BOX")   if 1 then area behind box cleared before drawing.  
  ;"RESULT: returns 24bit color vector triple, CLRVEC24.  '#;#;#'
  ;
  NEW R,G,B,INPUT,CHANGED
  SET (R,G,B)=220
  NEW DONE SET DONE=0
  SET OPTION("ORIGIN X")=$GET(OPTION("ORIGIN X"),43)   ;"screen coordinates of <R,G,B> = <0,0,0>
  SET OPTION("ORIGIN Y")=$GET(OPTION("ORIGIN Y"),31)
  SET OPTION("WIDTH")=$GET(OPTION("WIDTH"),26)     
  SET OPTION("HEIGHT")=$GET(OPTION("HEIGHT"),10)      ;"NOTE: if value set 15 or higher, causes bug, not sure why yet.
  SET OPTION("DEPTH")=$GET(OPTION("DEPTH"),10)        ;"NOTE: if value set 15 or higher, causes bug, not sure why yet.
  SET OPTION("SHOW FRAME")=1
  SET OPTION("SHOW SELECTED")=1  
  SET OPTION("CLEAR BOX")=1
  NEW TEMP DO SETUPCHARS(.TEMP) MERGE OPTION("CHARS")=TEMP
  NEW TEXTHOME SET TEXTHOME="0^"_(OPTION("ORIGIN Y")+13)
  NEW SHADEPCT SET SHADEPCT=0.15
  WRITE #  ;"clear screen
  DO CSRSHOW^TMGTERM(0)
  NEW WIDTH  SET WIDTH=$GET(OPTION("WIDTH"))     
  NEW HEIGHT SET HEIGHT=$GET(OPTION("HEIGHT"))  
  NEW DEPTH  SET DEPTH=$GET(OPTION("DEPTH"))
  NEW RSTEP SET RSTEP=255/WIDTH
  NEW GSTEP SET GSTEP=255/DEPTH
  NEW BSTEP SET BSTEP=255/HEIGHT  
CB24L1 ;  
  DO DRAWCLRBOX24(.R,.G,.B,.OPTION)
  ;"Restore colors.  
  DO VTATRIB^TMGTERM(0)
  DO CUPOS^TMGTERM(TEXTHOME)  
  NEW COL SET COL(1)=20,COL(2)=40
  NEW ARROWS
  WRITE "  LEFT/RIGHT: " DO WRITEARROW^TMGTERM3("UP-DOWN ARROW",.ARROWS) WRITE " RED",!
  WRITE "       UP/DN: " DO WRITEARROW^TMGTERM3("UP-DOWN ARROW",.ARROWS) WRITE " GREEN",!
  WRITE "  Page-Up/Dn: " DO WRITEARROW^TMGTERM3("UP-DOWN ARROW",.ARROWS) WRITE " BLUE",!
  WRITE "         A/Z: " DO WRITEARROW^TMGTERM3("UP-DOWN ARROW",.ARROWS) WRITE " Light/Dark",!
  WRITE "           ?: Pick color by NAME"
CB24L2 ;  
  SET INPUT=$$READKY^TMGUSRI5("e",,1,,.ESCKEY,1) ;"read one char, with ESC processing
  IF INPUT="" DO  GOTO:DONE CB24DN
  . IF "RIGHT,LEFT,UP,DOWN,HOME,END"[ESCKEY SET INPUT=ESCKEY QUIT
  . IF ESCKEY="PREV" SET INPUT="PGUP" QUIT
  . IF ESCKEY="NEXT" SET INPUT="PGDN" QUIT
  . IF ESCKEY="DOWN" SET INPUT="DOWN" QUIT               
  . IF ESCKEY="CR" SET DONE=1 QUIT
  . IF ESCKEY="TAB" SET DONE=1 QUIT
  SET INPUT=$$UP^XLFSTR(INPUT)
  SET CHANGED=0
  IF (INPUT="LEFT") SET CHANGED=$$DELTAINT(.R,RSTEP)
  IF (INPUT="RIGHT") SET CHANGED=$$DELTAINT(.R,-RSTEP)
  IF (INPUT="DOWN") SET CHANGED=$$DELTAINT(.G,GSTEP)
  IF (INPUT="UP") SET CHANGED=$$DELTAINT(.G,-GSTEP)
  IF (INPUT="PGUP") SET CHANGED=$$DELTAINT(.B,BSTEP)
  IF (INPUT="PGDN") SET CHANGED=$$DELTAINT(.B,-BSTEP)
  IF INPUT="Z" DO  ;"PUSH TOWARDS BLACK
  . IF R<255 DO DELTA1CLR(.R,SHADEPCT) SET CHANGED=1
  . IF G<255 DO DELTA1CLR(.G,SHADEPCT) SET CHANGED=1
  . IF B<255 DO DELTA1CLR(.B,SHADEPCT) SET CHANGED=1
  IF INPUT="A" DO  ;"PUSH TOWARDS WHITE
  . IF R>0 DO DELTA1CLR(.R,-SHADEPCT) SET CHANGED=1
  . IF G>0 DO DELTA1CLR(.G,-SHADEPCT) SET CHANGED=1
  . IF B>0 DO DELTA1CLR(.B,-SHADEPCT) SET CHANGED=1
  IF INPUT="?" DO
  . NEW TEMP SET TEMP=$$COLORMENU()
  . SET CHANGED=1  ;"force redraw
  . IF TEMP["ABORT" QUIT
  . NEW CLRVEC SET CLRVEC=$PIECE(TEMP,"^",2)
  . DO V24TORGB^TMGTERM(CLRVEC,.R,.G,.B) ;"Split CLRVEC24 to R,G,B components.
  . SET OPTION("SELECTED")=CLRVEC
  . SET OPTION("SELECTED","NAME")=$PIECE(TEMP,"^",1)
  IF CHANGED GOTO CB24L1
  IF INPUT="^" SET DONE=1 GOTO CB24DN
  GOTO CB24L2
CB24DN ;  
  SET R=$$ROUND^TMGUTIL0(R,0)
  SET G=$$ROUND^TMGUTIL0(G,0)
  SET B=$$ROUND^TMGUTIL0(B,0)
  NEW S SET S=$$RJ^XLFSTR(R,3,"0")_";"_$$RJ^XLFSTR(G,3,"0")_";"_$$RJ^XLFSTR(B,3,"0")
  DO CSRSHOW^TMGTERM(1)
  WRITE #
  QUIT S                 
  ;
LIGHTERCLR(CLRVEC,LIGHTPCT) ;
  QUIT $$DELTACOLOR(.CLRVEC,LIGHTPCT)
  ;
DARKERCLR(CLRVEC,DARKPCT) ;
  QUIT $$DELTACOLOR(.CLRVEC,-DARKPCT)
  ;
DELTACOLOR(CLRVEC,SHADEPCT) ;
  NEW R,G,B DO V24TORGB^TMGTERM(CLRVEC,.R,.G,.B) ;"Split CLRVEC24 to R,G,B components.  
  DO DELTA1CLR(.R,SHADEPCT)
  DO DELTA1CLR(.G,SHADEPCT)
  DO DELTA1CLR(.B,SHADEPCT)
  QUIT $$CLRVEC24^TMGTERM(R,G,B)
  ;
DELTA1CLR(V,PCT) ;"PCT SHOULD BE 0-1
  IF PCT<0 DO
  . NEW DELTA SET DELTA=255*(-PCT)
  . IF DELTA<1 SET DELTA=1
  . SET V=(V-DELTA)\1
  ELSE  DO
  . NEW DELTA SET DELTA=$$ROUND^TMGUTIL0(255*PCT)
  . IF DELTA<1 SET DELTA=1
  . SET V=V+DELTA
  IF V<0 SET V=0
  IF V>255 SET V=255
  QUIT
  ;  
DELTAINT(V,DELTA) ;
  NEW INITV SET INITV=V
  SET V=V+DELTA
  SET:(V<0) V=0
  SET:(V>255) V=255
  QUIT (V'=INITV)
  ;  
DRAWCLRBOX24(R,G,B,OPTION) ;"Do drawing of color cube.  
  ;"Input: R, G, B -- PASS BY REFERENCE.  Selected red, green blue axis values (RGB coordinates)
  ;"       OPTION.  OPTIONAL.  PASS BY REFERENCE.
  ;"         OPTION("CENTERX"),OPTION("CENTERY") -- screen coordinates for centerpoint, where R=G=B=255.  Default is (23,31)
  ;"         OPTION("SHOW FRAME")=1  -- if 1 then frame shown
  ;"         OPTION("SHOW SELECTED")=1  -- If 1 then selected color shown      
  ;"         OPTION("CLEAR BOX")=1 -- If 1 then white box is painted before drawing cube.
  ;
  ;"             GREEN AXIS
  ;"1  -1  0         \#########################.             
  ;"2  -9           .#\#######################.##         
  ;"3  -8          .#.#\#####################.####         
  ;"4  -7         .#.#.#\###################.######        
  ;"5  -6        .#.#.#.#\#################.########      
  ;"6  -5       .#.#.#.#.#\###############.##########      
  ;"7  -4      .#.#.#.#.#.#\#############.############    
  ;"8  -3     .#.#.#.#.#.#.#\###########.##############    
  ;"9  -2    .#.#.#.#.#.#.#.#\#########.################  
  ;"0  -1   .#.#.#.#.#.#.#.#.#\#######.################## 
  ;"1   1  .#.#.#.#.#.#.#.#.#.#*-----.-------------------   RED AXIS
  ;"2   2   .#.#.#.#.#.#.#.#.#/#######.################## 
  ;"3   3    .#.#.#.#.#.#.#.#/#########.################  
  ;"4   4     .#.#.#.#.#.#.#/###########.##############   
  ;"5   5      .#.#.#.#.#.#/#############.############    
  ;"6   6       .#.#.#.#.#/###############.##########     
  ;"7   7        .#.#.#.#/#################.########       
  ;"8   8         .#.#.#/###################.######        
  ;"9   9          .#.#/#####################.####         
  ;"0  10           .#/#######################.##         
  ;"1  11            /#########################.          
  ;"             BLUE AXIS                                  
  ;"                12345678901234567890123456         
  ;"       12345678901234567890123456789012345678901234567
  ;
  NEW POS,FG,BG,POS 
  NEW ADDLABELS SET ADDLABELS=$GET(OPTION("LABEL AXES"))
  ;
  NEW BFLX,BFLY SET POS=$$XFRM(255,255,0,.OPTION)   DO SPLITPOS(POS,.BFLX,.BFLY)  ;"BASE FRONT LEFT  CORNER. 
  NEW BBLX,BBLY SET POS=$$XFRM(255,0,0,.OPTION)     DO SPLITPOS(POS,.BBLX,.BBLY)  ;"BASE BACK  LEFT  CORNER. 
  NEW BBRX,BBRY SET POS=$$XFRM(0,0,0,.OPTION)       DO SPLITPOS(POS,.BBRX,.BBRY)  ;"BASE BACK  RIGHT CORNER. 
  NEW TBLX,TBLY SET POS=$$XFRM(255,0,255,.OPTION)   DO SPLITPOS(POS,.TBLX,.TBLY)  ;"TOP  BACK  LEFT  CORNER. 
  NEW BFRX,BFRY SET POS=$$XFRM(0,255,0,.OPTION)     DO SPLITPOS(POS,.BFRX,.BFRY)  ;"BASE FRONT RIGHT CORNER. 
  NEW TFRX,TFRY SET POS=$$XFRM(0,255,255,.OPTION)   DO SPLITPOS(POS,.TFRX,.TFRY)  ;"TOP  FRONT RIGHT CORNER. 
  NEW TBRX,TBRY SET POS=$$XFRM(0,0,255,.OPTION)     DO SPLITPOS(POS,.TBRX,.TBRY)  ;"TOP  BACK  RIGHT CORNER. 
  ;
  IF $GET(OPTION("CLEAR BOX")) DO
  . NEW X SET X=BBLX
  . NEW Y SET Y=TBLY
  . NEW WIDTH SET WIDTH=TFRX-BBLX+1
  . NEW HT SET HT=BFLY-TBLY 
  . NEW WS SET WS="" SET $PIECE(WS," ",WIDTH)=" "
  . FOR Y=TBLY:1:BFLY DO
  . . DO CUP^TMGTERM(BBLX,Y) WRITE WS
  ;
  NEW WIDTH  SET WIDTH=$GET(OPTION("WIDTH"),26)     
  NEW HEIGHT SET HEIGHT=$GET(OPTION("HEIGHT"),10)  
  NEW DEPTH  SET DEPTH=$GET(OPTION("DEPTH"),10)
  NEW RSTEP SET RSTEP=255/WIDTH
  NEW GSTEP SET GSTEP=255/DEPTH
  NEW BSTEP SET BSTEP=255/HEIGHT
  NEW TEMPCLR SET TEMPCLR=$$CLRVEC24^TMGTERM(R,G,B)
  IF $GET(OPTION("SELECTED"))'=TEMPCLR DO
  . SET OPTION("SELECTED")=TEMPCLR
  . SET OPTION("SELECTED","NAME")=""
  NEW R1,G1,B1
  SET BG=$$CLRVEC24^TMGTERM(255,0,0)  ;"255,0,0 = RED
  SET FG=$$INVCLRVEC^TMGTERM(BG)
  ;
  ;"DRAW BG FACE
  NEW GSTEP2 SET GSTEP2=GSTEP/2
  SET R1=R,G1=0
  FOR  DO  SET G1=G1+GSTEP2 QUIT:G1>G
  . NEW BSTEP2 SET BSTEP2=BSTEP/2
  . SET B1=0
  . FOR  DO  SET B1=B1+BSTEP2 QUIT:B1>B
  . . DO DRAWPART("GB",R1,G1,B1,0,0,0,.OPTION) 
  . . IF (B-B1)<BSTEP2,(B-B1)>0 SET BSTEP2=(B-B1)
  . IF (G-G1)<GSTEP2,(G-G1)>0 SET GSTEP2=(G-G1)
  ;
  ;"DRAW RG FACE
  SET GSTEP2=GSTEP
  SET B1=B,G1=0
  FOR  DO  SET G1=G1+GSTEP2 QUIT:G1>G
  . NEW RSTEP2 SET RSTEP2=RSTEP
  . SET R1=0
  . FOR  DO  SET R1=R1+RSTEP2 QUIT:R1>R
  . . DO DRAWPART("RG",R1,G1,B1,(R1=R),(G1=G),0,.OPTION)
  . . IF (R-R1)<RSTEP2,(R-R1)>0 SET RSTEP2=(R-R1)
  . IF (G-G1)<GSTEP2,(G-G1)>0 SET GSTEP2=(G-G1)
  ;  
  ;"DRAW RB FACE
  NEW BSTEP2 SET BSTEP2=BSTEP
  SET G1=G,B1=0
  FOR  DO  SET B1=B1+BSTEP2 QUIT:B1>B
  . NEW RSTEP2 SET RSTEP2=RSTEP
  . SET R1=0
  . FOR  DO  SET R1=R1+RSTEP2 QUIT:R1>R
  . . DO DRAWPART("RB",R1,G1,B1,(R1=R),0,(B1=B),.OPTION)
  . . IF (R-R1)<RSTEP2,(R-R1)>0 SET RSTEP2=(R-R1)
  . IF (B-B1)<BSTEP2,(B-B1)>0 SET BSTEP2=(B-B1)
  ;
  ;"DRAW FRAME IF WANTED
  IF $GET(OPTION("SHOW FRAME")) DO
  . NEW CHARS MERGE CHARS=OPTION("CHARS") IF $DATA(CHARS)=0 DO SETUPCHARS(.CHARS)
  . DO VTATRIB^TMGTERM(0)
  . DO DRAWLINE(.CHARS,"G",0,255+GSTEP,255+RSTEP,0,0,GSTEP)     ;"DRAW BASE LEFT EDGE
  . DO DRAWLINE(.CHARS,"R",0,255+RSTEP,0,255+GSTEP,0,RSTEP,0,0) ;"DRAW FRONT BASE HORIZONTAL LINE  
  . DO DRAWLINE(.CHARS,"R",R+RSTEP,255+RSTEP,0,0,0,RSTEP,1,1)   ;"DRAW BACK BASE HORIZONTAL LINE  !! ENDING HAS 2 DOTS.  
  . DO DRAWLINE(.CHARS,"B",0,255+BSTEP,255+RSTEP,0,0,BSTEP)     ;"DRAW BACK-LEFT VERTICAL LINE  
  . DO DRAWLINE(.CHARS,"R",0,255+RSTEP,0,0,255+BSTEP,RSTEP)     ;"DRAW TOP BACK HORIZONTAL LINE  
  . DO DRAWLINE(.CHARS,"G",G+GSTEP,255+GSTEP,0,1,0,GSTEP,1,1)   ;"DRAW BASE RIGHT EDGE  
  . DO DRAWLINE(.CHARS,"B",0,255+BSTEP,0,255+GSTEP,0,BSTEP)     ;"DRAW FRONT RIGHT VERTICAL LINE    
  . DO DRAWLINE(.CHARS,"G",0,255+GSTEP,0,0,255+BSTEP,GSTEP)     ;"DRAW TOP RIGHT EDGE  
  . DO DRAWLINE(.CHARS,"B",B+BSTEP,255+BSTEP,0,1,0,BSTEP,1,0)   ;"DRAW BACK RIGHT VERTICAL LINE    
  ;
  ;"DRAW SELECTED COLOR BOX.  
  IF $GET(OPTION("SHOW SELECTED"))=1 DO
  . NEW BOXW SET BOXW=20
  . NEW EXTRA SET EXTRA=$SELECT($GET(OPTION("LABEL AXES")):8,1:0)
  . NEW X SET X=TFRX+4+EXTRA
  . NEW Y SET Y=TBRY
  . NEW CLRNAME SET CLRNAME=$GET(OPTION("SELECTED","NAME"))
  . NEW S 
  . SET S(0)=$$CJ^XLFSTR("",BOXW," ")
  . SET S(1)=$$CJ^XLFSTR("SELECTED COLOR",BOXW," ")
  . SET S(2)=$$CJ^XLFSTR("("_$$RJ^XLFSTR(R\1,3,"0")_","_$$RJ^XLFSTR(G\1,3,"0")_","_$$RJ^XLFSTR(B\1,3,"0")_")",BOXW," ")
  . SET S(3)=$$CJ^XLFSTR(CLRNAME,BOXW," ")
  . SET BG=$$CLRVEC24^TMGTERM(R\1,G\1,B\1)  
  . SET FG=$$INVCLRVEC^TMGTERM(BG)
  . DO VCOLOR24B^TMGTERM(BG,FG)  ;"<-- I don't understand why I have to switch FG and BG to get it to display properly!
  . NEW DY FOR DY=1:1:HEIGHT+10 DO
  . . DO CUP^TMGTERM(X,Y+DY)
  . . NEW IDX SET IDX=$SELECT(DY=2:1,DY=3:2,DY=4:3,1:0)
  . . WRITE S(IDX)
  . DO
  . . NEW TEMP SET TEMP("ARC")=1
  . . DO DRAWBOX^TMGTERM2(X,Y,20,HEIGHT+12,.TEMP)
  ;  
  QUIT
  ;" 
XFRM(R,G,B,OPTION) ;"Transform (XFRM) RGB coordinates into XY screen coordinates.  
  ;"Input: R, G, B -- red, green blue values (0-255)
  ;"Result: <X>^<Y>  -- screen coordinates.  
  NEW ORIGINX SET ORIGINX=$GET(OPTION("ORIGIN X"),43)  ;"ORIGIN is for RGB = <0,0,0>, BASE BACK RIGHT CORNER
  NEW ORIGINY SET ORIGINY=$GET(OPTION("ORIGIN Y"),31)  
  NEW WIDTH  SET WIDTH=$GET(OPTION("WIDTH"),26)     
  NEW HEIGHT SET HEIGHT=$GET(OPTION("HEIGHT"),10)    
  NEW DEPTH  SET DEPTH=$GET(OPTION("DEPTH"),10)     
  NEW RSCALED SET RSCALED=(R/255)*WIDTH
  NEW GSCALED SET GSCALED=(G/255)*DEPTH
  NEW BSCALED SET BSCALED=(B/255)*HEIGHT
  NEW X,Y
  SET X=(RSCALED*-1)+(GSCALED*1)+(BSCALED*1)
  SET Y=(RSCALED*0)+(GSCALED*1)+(BSCALED*-1)
  SET X=$$ROUND^TMGUTIL0(ORIGINX+X,0)
  SET Y=$$ROUND^TMGUTIL0(ORIGINY+Y,0)
  QUIT X_"^"_Y
  ;
DRAWPART(FACE,R1,G1,B1,RISMAX,GISMAX,BISMAX,OPTION) ;
  ;"Input: FACE -- "BG"/"GB", or "GR"/"RG", or "BR"/"RB"
  ;"       R1, G1, B1 -- red, green blue values (0-255) to be drawn.  
  ;"       OPTION
  ;"   NOTE: Used in GLOBAL SCOPE: RSTEP,GSTEP,BSTEP
  NEW POS SET POS=$$XFRM(R1,G1,B1,.OPTION)
  NEW SELECTED SET SELECTED=$GET(OPTION("SELECTED"))
  NEW R,G,B DO V24TORGB^TMGTERM(SELECTED,.R,.G,.B) ;"Split CLRVEC24 to R,G,B components.  
  NEW CHAR SET CHAR=" "  ;"default char
  ;"NEW RISMAX SET RISMAX=($$ROUND^TMGUTIL0(R1,0)=$$ROUND^TMGUTIL0(R,0))
  ;"NEW GISMAX SET GISMAX=($$ROUND^TMGUTIL0(G1,0)=$$ROUND^TMGUTIL0(G,0))
  ;"NEW BISMAX SET BISMAX=($$ROUND^TMGUTIL0(B1,0)=$$ROUND^TMGUTIL0(B,0))
  NEW CHARS MERGE CHARS=OPTION("CHARS") IF $DATA(CHARS)=0 DO
  . DO SETUPCHARS(.CHARS)
  . MERGE OPTION("CHARS")=CHARS  
  IF "RG,GR"[FACE DO
  . SET CHAR=$SELECT(GISMAX&RISMAX:CHARS("#"),RISMAX:CHARS("\"),GISMAX:CHARS("-"),1:" ")      
  IF "BR,RB"[FACE DO
  . SET CHAR=$SELECT(BISMAX&RISMAX:CHARS("#"),RISMAX:CHARS("/"),BISMAX:CHARS("-"),1:" ")
  ;"NEW IGNORE SET IGNORE=0
  ;"IF "GB,BG"[FACE DO  IF IGNORE GOTO DPDN2  
  ;". ;"IF (B1+G1)[".5" SET IGNORE=1
  DO CUPOS^TMGTERM(POS)
  NEW FG SET FG=$$CLRVEC24^TMGTERM(R1\1,G1\1,B1\1)
  NEW BG SET BG=$$INVCLRVEC^TMGTERM(FG)
  DO VCOLOR24B^TMGTERM(FG,BG)
  IF $LENGTH(CHAR)>1,CHAR["$" DO
  . DO UTF8WRITE^TMGSTUTL(CHAR)
  ELSE  WRITE CHAR
DPDN2 ;  
  QUIT
  ;  
DRAWLINE(CHARS,AXIS,START,STOP,R1,G1,B1,STEP,NOSTARTDOT,NOSTOPDOT) ;
  NEW CH,V
  FOR V=START:STEP:STOP DO
  . IF AXIS="R" SET R1=V,CH=CHARS("-")
  . IF AXIS="G" SET G1=V,CH=CHARS("\")
  . IF AXIS="B" SET B1=V,CH=CHARS("/")
  . IF (V=START)&(+$GET(NOSTARTDOT)=0) SET CH=CHARS(".")
  . IF ((V=STOP)!(V+STEP>STOP))&(+$GET(NOSTOPDOT)=0) SET CH=CHARS(".")
  . SET POS=$$XFRM(R1,G1,B1,.OPTION)
  . DO CUPOS^TMGTERM(POS)
  . DO UTF8WRITE^TMGSTUTL(CH)
  QUIT
  ;
SETUPCHARS(CHARS) ;"SETUP ARRAY OF UNICODE CHARS.  
  SET CHARS("\")="$2572"
  SET CHARS("/")="$2571" 
  SET CHARS("-")="$2500" 
  SET CHARS(".")="$22C5"  ;"$2572 is dot operator, like '*'
  SET CHARS("#")="$22A1"   ;"$22A1 is squared dot,
  QUIT  
  ;
SPLITPOS(POS,X,Y) ;
  SET X=$PIECE(POS,"^",1)
  SET Y=$PIECE(POS,"^",2)
  QUIT
  ;