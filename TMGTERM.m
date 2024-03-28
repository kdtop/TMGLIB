TMGTERM  ;TMG/kst/Terminal interface (ANSI sequences) ;7/17/12, 4/24/15
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
 ;"VFGCOLOR(n) ;Set Text Foreground Color  <ESC>[{attr1};...;{attrn}m
 ;"VBGCOLOR(n) ;Set Text Background Color  <ESC>[{attr1};...;{attrn}m
 ;"VCOLORS(FG,BG)  ;Set Text Colors   <ESC>[{attr1};...;{attrn}m
 ;"SETGBLCO  //Set Global Colors
 ;"KILLGBLC  //Kill Global Colors
 ;"DEMOCOLOR
 ;"$$COLORPAIR(FG,BG,ARR) --Return a 'FG^BG' based on names
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
  WRITE $CHAR(27,91)  ;"27=Esc 91=[
  IF $DATA(NUM) WRITE NUM  
  IF $DATA(N2) WRITE ";"_N2
  IF $DATA(CMD) WRITE CMD
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
VFGCOLOR(N)  ;"Set Text Foreground Color  <ESC>[{attr1};...;{attrn}m
  ;"See note about colors in VCOLORS
  DO VTATRIB(0)
  IF N>7 DO
  . DO VTATRIB(1)
  . SET N=N-7
  SET N=N+30
  DO ESCN(N,,"m") QUIT
  ;
VBGCOLOR(N)  ;"Set Text Background Color  <ESC>[{attr1};...;{attrn}m
  ;"See note about colors in VCOLORS
  DO VTATRIB(0)
  IF N>7 DO
  . DO VTATRIB(1)
  . SET N=N-7
  SET N=N+40
  DO ESCN(N,,"m") QUIT
  ;
VCOLORS(FG,BG)  ;Set Text Colors   <ESC>[{attr1};...;{attrn}m
  ;"Note: 5/29/06  I don't know if the color numbers are working
  ;"  correctly.  The best way to determine what the color should
  ;"  be is to run DemoColor and pick the numbers wanted for desired colors
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
  ;"  0-  7:  standard colors (as in ESC [ 30–37 m)
  ;"  8- 15:  high intensity colors (as in ESC [ 90–97 m)
  ;" 16-231:  6 × 6 × 6 cube (216 colors): 16 + 36 × r + 6 × g + b (0 <= r, g, b <= 5)
  ;"232-255:  grayscale from black to white in 24 steps
  DO VTATRIB(0)
  DO ESCN(38,5_";"_FG,"m") 
  DO ESCN(48,5_";"_BG,"m") 
  QUIT
  ;
COLORPAIR(FG,BG,ARR) ;"Return a 'FG^BG' based on names
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
  QUIT
  ;
KILLGBLC   ;"Kill Global Colors
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
  QUIT
  ;
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
  ;"==============================================================
  ;
DEMOCOLR   ;
   ;"Purpose: to WRITE a lines on the screen, showing all the color combos
  DO VCUSAV2
  NEW FG,BG
  FOR BG=0:1:15 DO
  . FOR FG=0:1:15 DO
  . . DO VCOLORS(FG,BG)
  . . WRITE "Text with background color #",BG," and foreground color #",FG
  . . DO VTATRIB(0)
  . . WRITE !
  DO VCULOAD2
  QUIT
  ;
COLORBOX(SETBG,SETFG)  ;
  ;"Purpose: to WRITE a grid on the screen, showing all the color combos
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
  . . DO VCOLORS(FG,BG)
  . . IF $DATA(SETFG)#10=0 DO
  . . . WRITE " X "
  . . ELSE  DO
  . . . WRITE "   "
  . . DO VTATRIB(0)
  . WRITE !
  QUIT
  ;
  ;
  ;"NOTE:  This shows demo (that works from bash command line) with 256 color!
  ;"https://misc.flogisoft.com/bash/tip_colors_and_formatting
  ;"note: see ~/256-colors.sh
  ;"Putty must be configured             
  ;"   1. Configure Putty
  ;"   
  ;"   In Settings > Windows > Colours there is a check box for "Allow terminal to use xterm 256-colour mode".
  ;"   2. Let the app know
  ;"   
  ;"   You'll probably have to change Settings -> Connection > Data > Terminal-type string to:
  ;"   
  ;"   xterm-256color
  ;"   
  ;"   if your server has a terminfo entry for putty-256color, typically in /usr/share/terminfo/p/putty-256color, you can set Putty's Terminal-Type to putty-256color instead.
  ;"   
  ;"   The main thing here is to make the server use an available Terminfo entry that most closely matches the way Putty is configured.