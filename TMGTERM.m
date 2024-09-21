TMGTERM  ;TMG/kst/Terminal interface (ANSI sequences) ;7/17/12, 4/24/15, 9/12/24
         ;;1.0;TMG-LIB;**1,17**;09/01/05
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"Terminal interface
 ;"ANSI Standard (X3.64) Control Sequences for Video Terminals and Peripherals
 ;"      in alphabetic order by mnemonic
 ;
 ;"Terminal interface
 ;"ANSI Standard (X3.64) Control Sequences for Video Terminals and Peripherals
 ;"      in alphabetic order by mnemonic
 ;
 ;"CBT(PN)     ;CBT  Cursor Backward Tab    Esc [ PN Z
 ;"CCH         ;Cancel Previous Character   Esc T
 ;"CHA(PN)     ;Cursor Horzntal Absolute    Esc [ PN G
 ;"CHT(PN)     ;Cursor Horizontal Tab       Esc [ PN I
 ;"CNL(PN)     ;Cursor Next Line            Esc [ PN E
 ;"CPL(PN)     ;Cursor Preceding Line       Esc [ PN F
 ;"CPR(PN,P2)  ;Cursor Position Report      Esc [ PN  ; PN R     VT100
 ;"CTC(PN)     ;Cursor Tab Control          Esc [ Ps W
 ;"CUB(PN)     ;Cursor Backward             Esc [ PN D          VT100
 ;"CUD(PN)     ;Cursor Down                 Esc [ PN B          VT100
 ;"CUF(PN)     ;Cursor Forward              Esc [ PN C          VT100
 ;"CUP(X,Y)    ;Cursor Position             Esc [ PN  ; PN H     VT100
 ;"CUPOS(VEC2D);Cursor POSITION from 2D vec
 ;"HOME        ;Cursor Home                 Esc [ H     ('home' is top left)
 ;"CUU(PN)     ;Cursor Up                   Esc [ PN A          VT100
 ;"CVT(PN)     ;Cursor Vertical Tab         Esc [ PN Y
 ;"DCH(PN)     ;Delete Character            Esc [ PN P
 ;"DL(PN)      ;Delete Line                 Esc [ PN M
 ;"EA(PN)      ;Erase in Area               Esc [ Ps O
 ;"ECH(PN)     ;Erase Character             Esc [ PN X
 ;"ED(PN)      ;Erase in Display            Esc [ Ps J         VT100
 ;"EF(PN)      ;Erase in Field              Esc [ Ps N
 ;"EL(PN)      ;Erase in Line               Esc [ Ps K         VT100
 ;"EPA         ;End of Protected Area       Esc W
 ;"ESA         ;End of Selected Area        Esc G
 ;"FNT(PN,P2)  ;Font Selection              Esc [ PN  ; PN Space D
 ;"GSM(PN,P2)  ;Graphic Size Modify         Esc [ PN  ; PN Space B
 ;"GSS(PN)     ;Graphic Size Selection      Esc [ PN Space C
 ;"HPA(PN)     ;Horz Position Absolute      Esc [ PN `
 ;"HPR(PN)     ;Horz Position Relative      Esc [ PN a
 ;"HTJ         ;Horz Tab w/Justification    Esc I
 ;"HTS         ;Horizontal Tab Set          Esc H             VT100
 ;"HVP(PN,P2)  ;Horz & Vertical Position    Esc [ PN  ; PN f  VT100
 ;"ICH(PN)     ;Insert Character            Esc [ PN @
 ;"IL(PN)      ;Insert Line                 Esc [ PN L
 ;"IND         ;Index                       Esc D           VT100
 ;"NEL         ;Next Line                   Esc E           VT100
 ;"NP(PN)      ;Next Page                   Esc [ PN U
 ;"PP(PN)      ;Preceding Page              Esc [ PN V
 ;"IS          ;Reset to Initial State      Esc c
 ;"RM(PN)      ;Reset Mode                  Esc [ Ps l     VT100
 ;"SD(PN)      ;Scroll Down                 Esc [ PN T
 ;"SL(PN)      ;Scroll Left                 Esc [ PN Space @
 ;"SM(PN)      ;Select Mode                 Esc [ Ps h     VT100
 ;"SPA         ;Start of Protected Area     Esc V
 ;"SPI(PN,P2)  ;Spacing Increment           Esc [ PN  ; PN Space G
 ;"SR(PN)      ;Scroll Right                Esc [ PN Space A
 ;"SA          ;Start of Selected Area      Esc F
 ;"ST          ;String Terminator           Esc \
 ;"SU(PN)      ;Scroll Up                   Esc [ PN S
 ;"TBC(PN)     ;Tab Clear                   Esc [ Ps g        VT100
 ;"VPA(PN)     ;Vert Position Absolute      Esc [ PN d
 ;"VPR(PN)     ;Vert Position Relative      Esc [ PN e
 ;"VCULOAD     ;Unsave Cursor               ESC [ u
 ;"VCUSAV2     ;Save Cursor & Attrs         ESC 7
 ;"VCULOAD2    ;Restore Cursor & Attrs      ESC 8
 ;
 ;"VT100 specific calls
 ;"--------------------
 ;"VCEL        ;Erase from cursor to end of line           Esc [ 0 K    or Esc [ K
 ;"VCBL        ;Erase from beginning of line to cursor     Esc [ 1 K
 ;"VEL         ;Erase line containing cursor               Esc [ 2 K
 ;"VCES        ;Erase from cursor to end of screen         Esc [ 0 J    or Esc [ J
 ;"VCBS        ;Erase from beginning of screen to cursor   Esc [ 1 J
 ;"VCS         ;Erase entire screen                        Esc [ 2 J
 ;"VCUSAV      ;Save Cursor                                ESC [ s
 ;"ESSCR       ;Enable scrolling for entire display
 ;"QCUP        ;Query cursor position
 ;"VTATRIB(n)  ;Set Text attributes    <ESC>[{attr1};...;{attrn}m
 ;"VCOLORIDXFG(n) ;Set Text Foreground Color  <ESC>[{attr1};...;{attrn}m
 ;"VCOLORIDXBG(n) ;Set Text Background Color  <ESC>[{attr1};...;{attrn}m
 ;"VCOLORSIDX(FG,BG)  ;Set Text Colors   <ESC>[{attr1};...;{attrn}m
 ;"SETGBLCO  //Set Global Colors
 ;"CLRVEC24(R,G,B) -- Return 24bit color vector triple
 ;"INVCLRV24(R,G,B)  --Return 24bit color vector triple, that is INVERTED from RGB
 ;"INVCLRVEC(CLRVEC) --Invert a 24bit color vector triple.
 ;"ISCLRVEC24(STR) -- Returns if STR is in format of 24bit color vector triple, CLRVEC24
 ;"=======================================================================
 ;"DEPENDENCIES: XLFSTR
 ;"=======================================================================
 ;
 ;
ESCN(NUM,N2,CMD)  ;
  NEW TEMPX,TEMPY
  SET TEMPX=$X
  SET TEMPY=$Y
  SET $X=1  ;"ensure escape chars don't cause a wrap.
  NEW ST SET ST=$CHAR(27,91)  ;"27=Esc 91=[
  IF $DATA(NUM) SET ST=ST_NUM  
  IF $DATA(N2) SET ST=ST_";"_N2
  IF $DATA(CMD) SET ST=ST_CMD
  WRITE ST
   ;"reset $X,$Y so that escape characters aren't counted for line wrapping
  SET $X=TEMPX
  SET $Y=TEMPY
  QUIT
  ;
CBT(PN)  ;"CBT  Cursor Backward Tab  Esc [ PN Z
  DO ESCN(.PN,,"Z")
  QUIT
  ;
CCH      ;"Cancel Previous Character Esc T
  WRITE $CHAR(27)_"T"
  QUIT
  ;
CHA(PN)  ;"Cursor Horizontal Absolute  Esc [ PN G
  DO ESCN(.PN,,"G")
  SET $X=PN
  QUIT
  ;
CHT(PN)  ;"Cursor Horizontal Tab     Esc [ PN I
  DO ESCN(.PN,,"I") QUIT
  ;
CNL(PN)  ;"Cursor Next Line          Esc [ PN E
  DO ESCN(.PN,,"E")
  SET $Y=$Y+1
  QUIT
  ;
CPL(PN)  ;"Cursor Preceding Line     Esc [ PN F
  DO ESCN(.PN,,"F")
  IF $Y>0 SET $Y=$Y-1
  QUIT
  ;
CPR(PN,P2)  ;"Cursor Position Report Esc [ PN  ; PN R     VT100
  ;"NOTE: this appears to something that the terminal send to the host
  ;"Evoked by command ESC [ 6 n  (according to here: http://vt100.net/docs/vt100-ug/chapter3.html)
  ;"To use this, write ESC[6n to the screen, and then do a READ to get back reply 
  ;"  device terminators will probably have to set up to get the entire response
  ;"See also QCUP  ;"Query cursor position
  DO ESCN(.PN,.P2,"R") QUIT
  ;
CTC(PN)  ;"Cursor Tab Control        Esc [ Ps W
  DO ESCN(.PN,,"W") QUIT
  ;
CUB(PN)  ;"Cursor Backward           Esc [ PN D          VT100
  DO ESCN(.PN,,"D")
  SET $X=$X-1
  QUIT
  ;
CUD(PN)  ;"Cursor Down               Esc [ PN B          VT100
  DO ESCN(.PN,,"B")
  SET $Y=$Y+1
  QUIT
  ;
CUF(PN)  ;"Cursor Forward            Esc [ PN C          VT100
  DO ESCN(.PN,,"C")
  SET $X=$X+1
  QUIT
  ;
CUP(X,Y)  ;"Cursor Position        Esc [ PN  ; PN H     VT100
  DO ESCN(.Y,.X,"H")
  SET $X=X
  SET $Y=Y
  QUIT
  ;
CUPOS(VEC2D) ;"Cursor POSITION from 2D vec  ("X^Y")
  SET VEC2D=$GET(VEC2D)
  NEW X,Y SET X=+VEC2D,Y=+$PIECE(VEC2D,"^",2)
  DO CUP(X,Y)
  QUIT
  ;
HOME     ;"Cursor Home               Esc [ H     ('home' is top left)
  ;" SET $X=1   ;"ensure characters below don't cause a wrap.
  ;" WRITE $CHAR(27,91)_"H"
  DO ESCN(,,"H")
  SET $X=1   ;"now SET $X to home value.
  SET $Y=0
  QUIT
  ;
CUU(PN)  ;"Cursor Up                 Esc [ PN A          VT100
  DO ESCN(.PN,,"A")
  SET $Y=$Y-PN  
  QUIT
  ;
CVT(PN)  ;"Cursor Vertical Tab       Esc [ PN Y
  DO ESCN(.PN,,"Y") QUIT
  ;
DCH(PN)  ;"Delete Character          Esc [ PN P
  DO ESCN(.PN,,"P") QUIT
  ;
DL(PN)   ;"Delete Line               Esc [ PN M
  DO ESCN(.PN,,"M") QUIT
  ;
EA(PN)   ;"Erase in Area             Esc [ Ps O
  DO ESCN(.PN,,"O") QUIT
  ;
ECH(PN)  ;"Erase Character           Esc [ PN X
  DO ESCN(.PN,,"X") QUIT
  ;
ED(PN)   ;"Erase in Display          Esc [ Ps J         VT100
  DO ESCN(.PN,,"J") QUIT
  ;
EF(PN)   ;"Erase in Field            Esc [ Ps N
  DO ESCN(.PN,,"N") QUIT
  ;
EL(PN)   ;"Erase in Line             Esc [ Ps K         VT100
  DO ESCN(.PN,,"K") QUIT
  ;
EPA      ;"End of Protected Area     Esc W
  WRITE $CHAR(27)_"W" QUIT
  ;
ESA      ;"End of Selected Area      Esc G
  WRITE $CHAR(27)_"G" QUIT
  ;
FNT(PN,P2)  ;"Font Selection            Esc [ PN  ; PN Space D
  DO ESCN(.PN,P2,"D") QUIT
  ;
GSM(PN,P2)  ;"Graphic Size Modify       Esc [ PN  ; PN Space B
  DO ESCN(.PN,P2,"B") QUIT
  ;
GSS(PN)  ;"Graphic Size Selection    Esc [ PN Space C
  DO ESCN(.PN,,"C") QUIT
  ;
HPA(PN)  ;"Horz Position Absolute    Esc [ PN `
  DO ESCN(.PN,,"`") QUIT
  ;
HPR(PN)  ;"Horz Position Relative    Esc [ PN a
  DO ESCN(.PN,,"a") QUIT
  ;
HTJ      ;"Horz Tab w/Justification  Esc I
  WRITE $CHAR(27)_"I" QUIT
  ;
HTS      ;"Horizontal Tab Set        Esc H             VT100
  WRITE $CHAR(27)_"H" QUIT
  ;
HVP(PN,P2)  ;"Horz & Vertical Position  Esc [ PN  ; PN f  VT100
  DO ESCN(.PN,P2,"A") QUIT
  ;
ICH(PN)  ;"Insert Character          Esc [ PN @
  DO ESCN(.PN,,"@") QUIT
  ;
IL(PN)   ;"Insert Line               Esc [ PN L
  DO ESCN(.PN,,"L") QUIT
  ;
IND      ;"Index                     Esc D           VT100
  WRITE $CHAR(27)_"D" QUIT
  ;"//kt this may be Scroll Down command for VT100
  ;
NEL      ;"Next Line                 Esc E           VT100
  WRITE $CHAR(27)_"E" QUIT
  ;
NP(PN)   ;"Next Page                 Esc [ PN U
  DO ESCN(.PN,,"U") QUIT
  ;
PP(PN)   ;"Preceding Page            Esc [ PN V
  DO ESCN(.PN,,"V") QUIT
  ;
IS       ;"Reset to Initial State    Esc c
  WRITE $CHAR(27)_"c" QUIT
  ;
RM(PN)   ;"Reset Mode                Esc [ Ps l     VT100
  DO ESCN(.PN,,"l") QUIT
  ;
SD(PN)   ;"Scroll Down               Esc [ PN T
  DO ESCN(.PN,,"T") QUIT
  ;
SL(PN)   ;"Scroll Left               Esc [ PN Space @
  DO ESCN(.PN,," @") QUIT
  ;
SM(PN)   ;"Select Mode               Esc [ Ps h     VT100
  DO ESCN(.PN,,"h") QUIT
  ;
SPA      ;"Start of Protected Area   Esc V
  WRITE $CHAR(27)_"V" QUIT
  ;
SPI(PN,P2)  ;"Spacing Increment         Esc [ PN  ; PN Space G
  DO ESCN(.PN,P2," G") QUIT
  ;
SR(PN)   ;"Scroll Right              Esc [ PN Space A
  DO ESCN(.PN,," A") QUIT
  ;
SA       ;"Start of Selected Area    Esc F
  WRITE $CHAR(27)_"F" QUIT
  ;
ST       ;"String Terminator         Esc \
  WRITE $CHAR(27)_"\" QUIT
  ;
SU(PN)   ;"Scroll Up                 Esc [ PN S
  DO ESCN(.PN,,"S") QUIT
  ;
TBC(PN)  ;"Tab Clear                 Esc [ Ps g        VT100
  DO ESCN(.PN,,"g") QUIT
  ;
VPA(PN)  ;"Vert Position Absolute    Esc [ PN d
  DO ESCN(.PN,,"d") QUIT
  ;
VPR(PN)  ;"Vert Position Relative    Esc [ PN e
  DO ESCN(.PN,,"e") QUIT    
  ;
VCULOAD  ;"Unsave Cursor                              ESC [ u
  DO ESCN(,,"u") QUIT    
  ;" WRITE $CHAR(27,91)_"u" QUIT
  ;
VCUSAV2  ;"Save Cursor & Attrs                        ESC 7
  WRITE $CHAR(27)_"7" QUIT
  ;
VCULOAD2  ;"Restore Cursor & Attrs                    ESC 8
  WRITE $CHAR(27)_"8" QUIT
  ;
CSRSHOW(ON) ;"Turn cursor ON(1) or OFF(0)(hide)      ESC [ ? 25 l/h
  SET ON=+$GET(ON,1)
  NEW CMD SET CMD=$SELECT(ON=1:"h",1:"l")
  DO ESCN("?25",,CMD) QUIT
  ;
CSRBLINK(ON) ;"Set cursor blinking ON(1) or OFF(0)    ESC [ ? 25 l/h
  ;"KT NOTE: This doesn't seem to work.  Not supported in terminal??
  SET ON=+$GET(ON,1)
  NEW CMD SET CMD=$SELECT($GET(ON)=1:"h",1:"l")
  DO ESCN("?12",,CMD) QUIT
  ;
  ;"--------------------------------------------------------------
  ;"VT100 specific calls
  ;"Terminal interface
  ;"--------------------------------------------------------------
  ;
VCEL     ;"Erase from cursor to end of line           Esc [ 0 K    or Esc [ K
  DO ESCN("0",,"K") QUIT
  ;
VCBL     ;"Erase from beginning of line to cursor     Esc [ 1 K
  DO ESCN("1",,"K") QUIT
  ;
VEL      ;"Erase line containing cursor               Esc [ 2 K
  DO ESCN("2",,"K") QUIT
  ;
VCES     ;"Erase from cursor to end of screen         Esc [ 0 J    or Esc [ J
  DO ESCN("0",,"J") QUIT
  ;
VCBS     ;"Erase from beginning of screen to cursor   Esc [ 1 J
  DO ESCN("1",,"J") QUIT
  ;
VCS      ;"Erase entire screen                        Esc [ 2 J
  DO ESCN("2",,"J") QUIT
  ;
VCUSAV   ;"Save Cursor                                ESC [ s
  WRITE $CHAR(27,91)_"s" QUIT
  ;
  ;"VCULOAD  ;"Unsave Cursor                              ESC [ u
  ;"       WRITE $CHAR(27,91)_"u" QUIT
  ;
  ;"VCUSAV2  ;"Save Cursor & Attrs                        ESC 7
  ;"       WRITE $CHAR(27)_"7" QUIT
  ;
  ;"VCULOAD2  ;"Restore Cursor & Attrs                    ESC 8
  ;"       WRITE $CHAR(27)_"8" QUIT
  ;
ESSCR   ;"Enable scrolling for entire display
  ;"I am not sure if this is VT100 specific or not
  DO ESCN(,,"r") QUIT    
  ;
QCUP  ;"Query cursor position
  ;"I am not sure if this is VT100 specific or not
  ;"NOTE: the device is suppose to reply with this: <ESC>[<ROW>;<COLUMN>R
  DO ESCN(6,,"n") QUIT
  ;
VTATRIB(N)  ;"Set Text attributes    <ESC>[{attr1};...;{attrn}m
   ;"0-Reset all attributes
   ;"1-Bright
   ;"2-Dim
   ;"4-Underscore
   ;"5-Blink
   ;"7-Reverse
   ;"8-Hidden
  DO ESCN(N,,"m") QUIT
  ;
VCOLORIDXFG(N)  ;"Set Text Foreground Color  <ESC>[{attr1};...;{attrn}m
  ;"See note about colors in VCOLORSIDX
  ;"NOTE: This is for indexed color (3 to 4 bit)
  DO VTATRIB(0)
  IF N>7 DO
  . DO VTATRIB(1)
  . SET N=N-7
  SET N=N+30
  DO ESCN(N,,"m") QUIT
  ;
VCOLORIDXBG(N)  ;"Set Text Background Color  <ESC>[{attr1};...;{attrn}m
  ;"See note about colors in VCOLORSIDX
  ;"NOTE: This is for indexed color (3 to 4 bit)
  DO VTATRIB(0)
  IF N>7 DO
  . DO VTATRIB(1)
  . SET N=N-7
  SET N=N+40
  DO ESCN(N,,"m") QUIT
  ;
VCOLORSIDX(FG,BG)  ;Set Text Colors   <ESC>[{attr1};...;{attrn}m
  ;"Note: 5/29/06  I don't know if the color numbers are working
  ;"  correctly.  The best way to determine what the color should
  ;"  be is to run DemoColor^TMGUSRI8 and pick the numbers wanted for desired colors
  ;"NOTE: This is for indexed color (3 to 4 bit)
  DO VTATRIB(0)
  IF FG>7 DO
  . DO VTATRIB(1)
  . SET FG=FG-7
  IF BG>7 DO
  . DO VTATRIB(1)
  . SET BG=BG-7
  ;
  SET FG=FG+30
  SET BG=BG+40
  DO ESCN(FG,BG,"m") QUIT
  QUIT
  ;
VFGCOLOR256(N)  ;"Set Text Foreground Color  <ESC>[38;5;<NUM>m
  ;"NOT YET TESTED   https://misc.flogisoft.com/bash/tip_colors_and_formatting
  DO VTATRIB(0)
  DO ESCN(38,5_";"_N,"m") QUIT
  ;
VBGCOLOR256(N)  ;"Set Text Background Color  <ESC>[48;5;<NUM>m
  ;"NOT YET TESTED  https://misc.flogisoft.com/bash/tip_colors_and_formatting
  DO VTATRIB(0)
  DO ESCN(48,5_";"_N,"m") QUIT
  ;
VCOLORS256(FG,BG)  ;Set Text Colors   <ESC>[[38|48];5;<NUM>m
  ;"NOT YET TESTED   https://misc.flogisoft.com/bash/tip_colors_and_formatting
  ;"FROM HERE: https://en.wikipedia.org/wiki/ANSI_escape_code#Colors
  ;"  0-  7:  standard colors (as in ESC [ 30-37 m)
  ;"  8- 15:  high intensity colors (as in ESC [ 90-97 m)
  ;" 16-231:  6 * 6 * 6 cube (216 colors): 16 + 36 * r + 6 * g + b (0 <= r, g, b <= 5)
  ;"232-255:  grayscale from black to white in 24 steps
  DO VTATRIB(0)
  DO ESCN(38,5_";"_FG,"m") 
  DO ESCN(48,5_";"_BG,"m") 
  QUIT
  ;
VCLR24FG(FG) ;"SET TEXT FOREGROUND COLOR, IN 24 BIT MODE.
  ;"From here: https://stackoverflow.com/questions/4842424/list-of-ansi-color-escape-sequences
  ;"**NOTICE**.  Because each color is an RGB triplet, this is NOT just a number
  ;"Input: FG -- FOREGROUND vector '<#;#;#>',  e.g. '134;56;122' 
  DO ESCN(48,2,";"_FG_"m")
  ;"WRITE $CHAR(27)_"[48;2;"_FG_"m" 
  QUIT
  ;
VCLR24BG(BG) ;"SET TEXT BACKGROUND COLOR, IN 24 BIT MODE.
  ;"From here: https://stackoverflow.com/questions/4842424/list-of-ansi-color-escape-sequences
  ;"**NOTICE**.  Because each color is an RGB triplet, this is NOT just a number
  ;"Input: BG -- BACKGROUND vector '<#;#;#>',  e.g. '134;56;122'
  DO ESCN(38,2,";"_BG_"m")
  ;"WRITE $CHAR(27)_"[38;2;"_BG_"m" 
  QUIT
  ;
VCOLOR24B(FG,BG) ;"SET TEXT COLORS, IN 24 BIT MODE.
  ;"From here: https://stackoverflow.com/questions/4842424/list-of-ansi-color-escape-sequences
  ;"**NOTICE**.  Because each color FG or BG color is an RGB triplet, this is NOT just a number
  ;"Input: FG -- FOREGROUND vector '<#;#;#>',  e.g. '134;56;122' 
  ;"Input: BG -- BACKGROUND vector '<#;#;#>',  e.g. '134;56;122'
  DO VCLR24FG(FG)
  DO VCLR24BG(BG)
  QUIT
  ;
VCOLOR24CLEAR ;
  DO ESCN(0,,"m")
  QUIT
  ;
COLORS(FG,BG,OPTION) ;"Unified call for setting colors (4 bit, 256 bit or 24 bit)
  ;"Most of the original code was for 4 bit colors, but support for 256 and 24bit being added later.  
  ;"Input: FG -- Foreground color.  Value will depend on color mode.  If 24bit, should be color vector (see VCLR24FG)
  ;"             NOTE: Passing FG parameter that is in CLRVEC24 format will force color mode to 24bit.  
  ;"       BG -- Background color (format same as FG)
  ;"             NOTE: Passing BG parameter that is in CLRVEC24 format will force color mode to 24bit.  
  ;"       OPTION -- PASS BY REFERENCE.  
  ;"          OPTION("MODE") -- Optional. 
  ;"            OPTION("MODE")="INDEXED"  <-- default value if not passed.  Traditional indexed colors 
  ;"            OPTION("MODE")="256"   <-- 256 (8 bit) color -- not fully implemented. 
  ;"            OPTION("MODE")="24bit"  <-- true colors, 24 bit (3 byte)
  NEW COLORMODE SET COLORMODE=$GET(OPTION("MODE"),"INDEXED")
  IF $$ISCLRVEC24(FG)&$$ISCLRVEC24(BG) SET COLORMODE="24bit"
  IF COLORMODE="INDEXED" DO
  . DO VCOLORSIDX(.FG,.BG)
  ELSE  IF COLORMODE="256" DO
  . DO VCOLORS256(.FG,.BG)  
  ELSE  IF COLORMODE="24bit" DO
  . DO VCOLOR24B(.FG,.BG) 
  QUIT
  ;
  ;"=======================================================================
CLRVEC24(R,G,B) ;"Return 24bit color vector triple
  QUIT R_";"_G_";"_B  
  ;
INVCLRV24(R,G,B)  ;"Return 24bit color vector triple, that is INVERTED from RGB
  QUIT $$CLRVEC24(255-R,255-G,255-B)
  ;
INVCLRVEC(CLRVEC) ;"Invert a 24bit color vector triple. 
  NEW R SET R=+$PIECE(CLRVEC,";",1)
  NEW G SET G=+$PIECE(CLRVEC,";",2)
  NEW B SET B=+$PIECE(CLRVEC,";",3)
  QUIT $$INVCLRV24(R,G,B)
  ;  
ISCLRVEC24(STR) ;"Returns if STR is in format of 24bit color vector triple, CLRVEC24.  '#;#;#'
  NEW RESULT,IDX,VAL SET RESULT=($LENGTH(STR,";")=3) 
  FOR IDX=1:1:3 QUIT:(RESULT=0)  SET VAL=$PIECE(STR,";",IDX),RESULT=(+VAL=VAL)
  QUIT RESULT
  ;"=======================================================================
  ;  
COLORPAIR(FG,BG,ARR) ;"DEPRECIATED, MOVED
  QUIT $$COLORPAIR(.FG,.BG,.ARR)
  ;  
SETGBLCO   ;"DEPRECIATED, MOVED
  DO SETGBLCO^TMGUSRI8
  QUIT
  ;
KILLGBLC   ;"DEPRECIATED, MOVED
  DO KILLGBLC^TMGUSRI8
  QUIT
  ;
PICK1COL(LABEL,INITVAL)   ;"DEPRECIATED, MOVED
  QUIT $$PICK1COL^TMGUSRI8(.LABEL,.INITVAL) 
  ;
PICKFGC(FG,BG)   ;"DEPRECIATED, MOVED
  QUIT $$PICKFGC^TMGUSRI8(.FG,.BC)
  ;
PICKBGC(INITVAL) ;"DEPRECIATED, MOVED.  
  QUIT $$PICKBGC^TMGUSRI8(.INITVAL)
  ;
PICKCLRS(FG,BG)   ;"DEPRECIATED. MOVED
  QUIT $$PICKCLRS^TMGUSRI8(.FG,.BG)
  ;
DEMOCOLR(OPTION)   ;"DEPRECIATED.  MOVED. 
  DO DEMOCOLR^TMGUSRI8(.OPTION)
  QUIT
  ;
COLORBOX(SETBG,SETFG)  ;"DEPRECIATED.  MOVED
  DO COLORBOX^TMGUSRI8(.SETBG,.SETFG)
  ;
COLORBOX24(OPTION) ;"Depreciated.  Moved to TMGUSRI8
  QUIT $$PICKCOLOR24^TMGUSRI8(.OPTION)
  ;
WEBCOLOR(NAME,COLORARR) ;"depreciated.  Moved to TMGUSRI8
  QUIT $$WEBCOLOR^TMGUSRI8(.NAME,.COLORARR)
  ;                                                                                   