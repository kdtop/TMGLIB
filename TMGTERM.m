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
 ;"V24TORGB(CLRVEC,R,G,B) --Split CLRVEC24 to R,G,B components. 
 ;"ADD2TERMBUF(STR)  --Put STR into TMGTERMBUF.  See also documentation for ESCN()
 ;"PUTTERMBUF() --Output buffer to current IO.  
 
 ;"=======================================================================
 ;"DEPENDENCIES: XLFSTR
 ;"=======================================================================
 ;
 ;
ESCN(NUM,N2,CMD,OPTION)  ;"Do actual writing of ESC sequence to output (or put into buffer for later output) 
  ;"Input:  NUM -- OPTIONAL.  Part of sequence before ";"
  ;"        N2  -- OPTIONAL   Part of sequence after ";"
  ;"        CMD -- OPTIONAL   termination part of sequence.
  ;"        OPTION -- OPTIONAL.
  ;"          OPTION("BUFFERED")=<Buffer name>.  If defined, output into buffer instead of to screen.
  ;"          @<BufferName>@("INFO")=<line# to add to>^<current # chars on line>^<buffer line max length>
  ;"          @<BufferName>@(<buffer#>,<line#>)=escape sequences, text etc to be output later, all at once.
  ;"         The idea behind this is to send ALL the screen info all at once, to hopefully avoid flickering. 
  NEW ST SET ST=$CHAR(27,91)  ;"27=Esc 91=[
  IF $DATA(NUM) SET ST=ST_NUM  
  IF $DATA(N2) SET ST=ST_";"_N2
  IF $DATA(CMD) SET ST=ST_CMD
  IF $GET(OPTION("BUFFERED"))'="" DO
  . DO ADD2TERMBUF(ST,.OPTION)
  ELSE  DO 
  . NEW TEMPX,TEMPY
  . SET TEMPX=$X
  . SET TEMPY=$Y
  . SET $X=1  ;"ensure escape chars don't cause a wrap.
  . WRITE ST
  . ;"reset $X,$Y so that escape characters aren't counted for line wrapping
  . SET $X=TEMPX
  . SET $Y=TEMPY
  QUIT
  ;
ADD2TERMBUF(STR,OPTION)  ;"Put STR into TMGTERMBUF.  See also documentation for ESCN()
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
TERMWRITE(STR,OPTION) ;"Write, with optional output to buffer instead of directly to IO
  IF $GET(OPTION("BUFFERED"))'="" DO
  . DO ADD2TERMBUF(STR,.OPTION)
  ELSE  WRITE STR
  QUIT
  ;
PUTBUF(BUFNAME) ;"Output buffer to current IO.  
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
CBT(PN,OPTION)  ;"CBT  Cursor Backward Tab  Esc [ PN Z
  DO ESCN(.PN,,"Z",.OPTION)
  QUIT
  ;
CCH(OPTION)      ;"Cancel Previous Character Esc T
  DO TERMWRITE($CHAR(27)_"T",.OPTION)
  QUIT
  ;
CHA(PN,OPTION)  ;"Cursor Horizontal Absolute  Esc [ PN G
  DO ESCN(.PN,,"G",.OPTION)
  SET $X=PN
  QUIT
  ;
CHT(PN,OPTION)  ;"Cursor Horizontal Tab     Esc [ PN I
  DO ESCN(.PN,,"I",.OPTION) QUIT
  ;
CNL(PN,OPTION)  ;"Cursor Next Line          Esc [ PN E
  DO ESCN(.PN,,"E",.OPTION)
  SET $Y=$Y+1
  QUIT
  ;
CPL(PN,OPTION)  ;"Cursor Preceding Line     Esc [ PN F
  DO ESCN(.PN,,"F",.OPTION)
  IF $Y>0 SET $Y=$Y-1
  QUIT
  ;
CPR(PN,P2,OPTION)  ;"Cursor Position Report Esc [ PN  ; PN R     VT100
  ;"NOTE: this appears to something that the terminal send to the host
  ;"Evoked by command ESC [ 6 n  (according to here: http://vt100.net/docs/vt100-ug/chapter3.html)
  ;"To use this, write ESC[6n to the screen, and then do a READ to get back reply 
  ;"  device terminators will probably have to set up to get the entire response
  ;"See also QCUP  ;"Query cursor position
  DO ESCN(.PN,.P2,"R",.OPTION) QUIT
  ;
CTC(PN,OPTION)  ;"Cursor Tab Control        Esc [ Ps W
  DO ESCN(.PN,,"W",.OPTION) QUIT
  ;
CUB(PN,OPTION)  ;"Cursor Backward           Esc [ PN D          VT100
  DO ESCN(.PN,,"D",.OPTION)
  SET $X=$X-1
  QUIT
  ;
CUD(PN,OPTION)  ;"Cursor Down               Esc [ PN B          VT100
  DO ESCN(.PN,,"B",.OPTION)
  SET $Y=$Y+1
  QUIT
  ;
CUF(PN,OPTION)  ;"Cursor Forward            Esc [ PN C          VT100
  DO ESCN(.PN,,"C",.OPTION)
  SET $X=$X+1
  QUIT
  ;
CUP(X,Y,OPTION)  ;"Cursor Position        Esc [ PN  ; PN H     VT100
  SET X=$GET(X)\1,Y=$GET(Y)\1
  DO ESCN(.Y,.X,"H",.OPTION)
  SET $X=X\1
  SET $Y=Y\1
  QUIT
  ;
CUPOS(VEC2D,OPTION) ;"Cursor POSITION from 2D vec  ("X^Y")
  SET VEC2D=$GET(VEC2D)
  NEW X,Y SET X=+VEC2D,Y=+$PIECE(VEC2D,"^",2)
  DO CUP(X,Y,.OPTION)
  QUIT
  ;
HOME(OPTION)     ;"Cursor Home               Esc [ H     ('home' is top left)
  DO ESCN(,,"H",.OPTION)
  SET $X=1   ;"now SET $X to home value.
  SET $Y=0
  QUIT
  ;
CUU(PN,OPTION)  ;"Cursor Up                 Esc [ PN A          VT100
  DO ESCN(.PN,,"A",.OPTION)
  SET $Y=$Y-PN  
  QUIT
  ;
CVT(PN,OPTION)  ;"Cursor Vertical Tab       Esc [ PN Y
  DO ESCN(.PN,,"Y",.OPTION) QUIT
  ;
DCH(PN,OPTION)  ;"Delete Character          Esc [ PN P
  DO ESCN(.PN,,"P",.OPTION) QUIT
  ;
DL(PN,OPTION)   ;"Delete Line               Esc [ PN M
  DO ESCN(.PN,,"M",.OPTION) QUIT
  ;
EA(PN,OPTION)   ;"Erase in Area             Esc [ Ps O
  DO ESCN(.PN,,"O",.OPTION) QUIT
  ;
ECH(PN,OPTION)  ;"Erase Character           Esc [ PN X
  DO ESCN(.PN,,"X",.OPTION) QUIT
  ;
ED(PN,OPTION)   ;"Erase in Display          Esc [ Ps J         VT100
  DO ESCN(.PN,,"J",.OPTION) QUIT
  ;
EF(PN,OPTION)   ;"Erase in Field            Esc [ Ps N
  DO ESCN(.PN,,"N",.OPTION) QUIT
  ;
EL(PN,OPTION)   ;"Erase in Line             Esc [ Ps K         VT100
  DO ESCN(.PN,,"K",.OPTION) QUIT
  ;
EPA(OPTION)      ;"End of Protected Area     Esc W
  DO TERMWRITE($CHAR(27)_"W",.OPTION) QUIT
  ;
ESA(OPTION)      ;"End of Selected Area      Esc G
  DO TERMWRITE($CHAR(27)_"G",.OPTION) QUIT
  ;
FNT(PN,P2,OPTION)  ;"Font Selection            Esc [ PN  ; PN Space D
  DO ESCN(.PN,P2,"D",.OPTION) QUIT
  ;
GSM(PN,P2,OPTION)  ;"Graphic Size Modify       Esc [ PN  ; PN Space B
  DO ESCN(.PN,P2,"B",.OPTION) QUIT
  ;
GSS(PN,OPTION)  ;"Graphic Size Selection    Esc [ PN Space C
  DO ESCN(.PN,,"C",.OPTION) QUIT
  ;
HPA(PN,OPTION)  ;"Horz Position Absolute    Esc [ PN `
  DO ESCN(.PN,,"`",.OPTION) QUIT
  ;
HPR(PN,OPTION)  ;"Horz Position Relative    Esc [ PN a
  DO ESCN(.PN,,"a",.OPTION) QUIT
  ;
HTJ(OPTION)      ;"Horz Tab w/Justification  Esc I
  DO TERMWRITE($CHAR(27)_"I",.OPTION) QUIT
  ;
HTS(OPTION)      ;"Horizontal Tab Set        Esc H             VT100
  DO TERMWRITE($CHAR(27)_"H",.OPTION) QUIT
  ;
HVP(PN,P2,OPTION)  ;"Horz & Vertical Position  Esc [ PN  ; PN f  VT100
  DO ESCN(.PN,P2,"A",.OPTION) QUIT
  ;
ICH(PN,OPTION)  ;"Insert Character          Esc [ PN @
  DO ESCN(.PN,,"@",.OPTION) QUIT
  ;
IL(PN,OPTION)   ;"Insert Line               Esc [ PN L
  DO ESCN(.PN,,"L",.OPTION) QUIT
  ;
IND(OPTION)      ;"Index                     Esc D           VT100
  DO TERMWRITE($CHAR(27)_"D",.OPTION) QUIT
  ;"//kt this may be Scroll Down command for VT100
  ;
NEL(OPTION)      ;"Next Line                 Esc E           VT100
  DO TERMWRITE($CHAR(27)_"E",.OPTION) QUIT
  ;
NP(PN,OPTION)   ;"Next Page                 Esc [ PN U
  DO ESCN(.PN,,"U",.OPTION) QUIT
  ;
PP(PN,OPTION)   ;"Preceding Page            Esc [ PN V
  DO ESCN(.PN,,"V",.OPTION) QUIT
  ;
IS(OPTION)       ;"Reset to Initial State    Esc c
  DO TERMWRITE($CHAR(27)_"c",.OPTION) QUIT
  ;
RM(PN,OPTION)   ;"Reset Mode                Esc [ Ps l     VT100
  DO ESCN(.PN,,"l",.OPTION) QUIT
  ;
SD(PN,OPTION)   ;"Scroll Down               Esc [ PN T
  DO ESCN(.PN,,"T",.OPTION) QUIT
  ;
SL(PN,OPTION)   ;"Scroll Left               Esc [ PN Space @
  DO ESCN(.PN,," @",.OPTION) QUIT
  ;
SM(PN,OPTION)   ;"Select Mode               Esc [ Ps h     VT100
  DO ESCN(.PN,,"h",.OPTION) QUIT
  ;
SPA(OPTION)      ;"Start of Protected Area   Esc V
  DO TERMWRITE($CHAR(27)_"V",.OPTION) QUIT
  ;
SPI(PN,P2,OPTION)  ;"Spacing Increment         Esc [ PN  ; PN Space G
  DO ESCN(.PN,P2," G",.OPTION) QUIT
  ;
SR(PN,OPTION)   ;"Scroll Right              Esc [ PN Space A
  DO ESCN(.PN,," A",.OPTION) QUIT
  ;
SA(OPTION)       ;"Start of Selected Area    Esc F
  DO TERMWRITE($CHAR(27)_"F",.OPTION) QUIT
  ;
ST(OPTION)       ;"String Terminator         Esc \
  DO TERMWRITE($CHAR(27)_"\",.OPTION) QUIT
  ;
SU(PN,OPTION)   ;"Scroll Up                 Esc [ PN S
  DO ESCN(.PN,,"S",.OPTION) QUIT
  ;
TBC(PN,OPTION)  ;"Tab Clear                 Esc [ Ps g        VT100
  DO ESCN(.PN,,"g",.OPTION) QUIT
  ;
VPA(PN,OPTION)  ;"Vert Position Absolute    Esc [ PN d
  DO ESCN(.PN,,"d",.OPTION) QUIT
  ;
VPR(PN,OPTION)  ;"Vert Position Relative    Esc [ PN e
  DO ESCN(.PN,,"e",.OPTION) QUIT    
  ;
VCULOAD(OPTION)  ;"Unsave Cursor                              ESC [ u
  DO ESCN(,,"u",.OPTION) QUIT    
  ;
VCUSAV2(OPTION)  ;"Save Cursor & Attrs                        ESC 7
  DO TERMWRITE($CHAR(27)_"7",.OPTION) QUIT
  ;
VCULOAD2(OPTION)  ;"Restore Cursor & Attrs                    ESC 8
  DO TERMWRITE($CHAR(27)_"8",.OPTION) QUIT
  ;
CSRSHOW(ON,OPTION) ;"Turn cursor ON(1) or OFF(0)(hide)      ESC [ ? 25 l/h
  SET ON=+$GET(ON,1)
  NEW CMD SET CMD=$SELECT(ON=1:"h",1:"l")
  DO ESCN("?25",,CMD,.OPTION) 
  QUIT
  ;
CSRBLINK(ON,OPTION) ;"Set cursor blinking ON(1) or OFF(0)    ESC [ ? 25 l/h
  ;"KT NOTE: This doesn't seem to work.  Not supported in terminal??
  SET ON=+$GET(ON,1)
  NEW CMD SET CMD=$SELECT($GET(ON)=1:"h",1:"l")
  DO ESCN("?12",,CMD,.OPTION) 
  QUIT
  ;
  ;"--------------------------------------------------------------
  ;"VT100 specific calls
  ;"Terminal interface
  ;"--------------------------------------------------------------
  ;
VCEL(OPTION)     ;"Erase from cursor to end of line           Esc [ 0 K    or Esc [ K
  DO ESCN("0",,"K",.OPTION) QUIT
  ;
VCBL(OPTION)     ;"Erase from beginning of line to cursor     Esc [ 1 K
  DO ESCN("1",,"K",.OPTION) QUIT
  ;
VEL(OPTION)      ;"Erase line containing cursor               Esc [ 2 K
  DO ESCN("2",,"K",.OPTION) QUIT
  ;
VCES(OPTION)     ;"Erase from cursor to end of screen         Esc [ 0 J    or Esc [ J
  DO ESCN("0",,"J",.OPTION) QUIT
  ;
VCBS(OPTION)     ;"Erase from beginning of screen to cursor   Esc [ 1 J
  DO ESCN("1",,"J",.OPTION) QUIT
  ;
VCS(OPTION)      ;"Erase entire screen                        Esc [ 2 J
  DO ESCN("2",,"J",.OPTION) QUIT
  ;
VCUSAV(OPTION)   ;"Save Cursor                                ESC [ s
  DO TERMWRITE($CHAR(27,91)_"s",.OPTION) QUIT
  ;
  ;"VCULOAD  ;"Unsave Cursor                              ESC [ u
  ;"       DO TERMWRITE($CHAR(27,91)_"u",.OPTION) QUIT
  ;
  ;"VCUSAV2  ;"Save Cursor & Attrs                        ESC 7
  ;"       DO TERMWRITE($CHAR(27)_"7",.OPTION) QUIT
  ;
  ;"VCULOAD2  ;"Restore Cursor & Attrs                    ESC 8
  ;"       DO TERMWRITE($CHAR(27)_"8",.OPTION) QUIT
  ;
ESSCR(OPTION)   ;"Enable scrolling for entire display
  ;"I am not sure if this is VT100 specific or not
  DO ESCN(,,"r",.OPTION) QUIT    
  ;
QCUP(OPTION)  ;"Query cursor position
  ;"I am not sure if this is VT100 specific or not
  ;"NOTE: the device is suppose to reply with this: <ESC>[<ROW>;<COLUMN>R
  DO ESCN(6,,"n",.OPTION) QUIT
  ;
VTATRIB(N,OPTION)  ;"Set Text attributes    <ESC>[{attr1};...;{attrn}m
   ;"0-Reset all attributes
   ;"1-Bright
   ;"2-Dim
   ;"4-Underscore
   ;"5-Blink
   ;"7-Reverse
   ;"8-Hidden
  DO ESCN(N,,"m",.OPTION) QUIT
  ;
VCOLORIDXFG(N,OPTION)  ;"Set Text Foreground Color  <ESC>[{attr1};...;{attrn}m
  ;"See note about colors in VCOLORSIDX
  ;"NOTE: This is for indexed color (3 to 4 bit)
  DO VTATRIB(0,.OPTION)
  IF N>7 DO
  . DO VTATRIB(1,.OPTION)
  . SET N=N-7
  SET N=N+30
  DO ESCN(N,,"m",.OPTION) 
  QUIT
  ;
VCOLORIDXBG(N,OPTION)  ;"Set Text Background Color  <ESC>[{attr1};...;{attrn}m
  ;"See note about colors in VCOLORSIDX
  ;"NOTE: This is for indexed color (3 to 4 bit)
  DO VTATRIB(0,.OPTION)
  IF N>7 DO
  . DO VTATRIB(1,.OPTION)
  . SET N=N-7
  SET N=N+40
  DO ESCN(N,,"m",.OPTION) 
  QUIT
  ;
VCOLORSIDX(FG,BG,OPTION)  ;Set Text Colors   <ESC>[{attr1};...;{attrn}m
  ;"Note: 5/29/06  I don't know if the color numbers are working
  ;"  correctly.  The best way to determine what the color should
  ;"  be is to run DemoColor^TMGUSRI8 and pick the numbers wanted for desired colors
  ;"NOTE: This is for indexed color (3 to 4 bit)
  DO VTATRIB(0,.OPTION)
  IF FG>7 DO
  . DO VTATRIB(1,.OPTION)
  . SET FG=FG-7
  IF BG>7 DO
  . DO VTATRIB(1,.OPTION)
  . SET BG=BG-7
  ;
  SET FG=FG+30
  SET BG=BG+40
  DO ESCN(FG,BG,"m",.OPTION) 
  QUIT
  ;
VFGCOLOR256(N,OPTION)  ;"Set Text Foreground Color  <ESC>[38;5;<NUM>m
  ;"NOT YET TESTED   https://misc.flogisoft.com/bash/tip_colors_and_formatting
  DO VTATRIB(0,.OPTION)
  DO ESCN(38,5_";"_N,"m",.OPTION) 
  QUIT
  ;
VBGCOLOR256(N,OPTION)  ;"Set Text Background Color  <ESC>[48;5;<NUM>m
  ;"NOT YET TESTED  https://misc.flogisoft.com/bash/tip_colors_and_formatting
  DO VTATRIB(0,.OPTION)
  DO ESCN(48,5_";"_N,"m",.OPTION) 
  QUIT
  ;
VCOLORS256(FG,BG,OPTION)  ;Set Text Colors   <ESC>[[38|48];5;<NUM>m
  ;"NOT YET TESTED   https://misc.flogisoft.com/bash/tip_colors_and_formatting
  ;"FROM HERE: https://en.wikipedia.org/wiki/ANSI_escape_code#Colors
  ;"  0-  7:  standard colors (as in ESC [ 30-37 m)
  ;"  8- 15:  high intensity colors (as in ESC [ 90-97 m)
  ;" 16-231:  6 * 6 * 6 cube (216 colors): 16 + 36 * r + 6 * g + b (0 <= r, g, b <= 5)
  ;"232-255:  grayscale from black to white in 24 steps
  DO VTATRIB(0,.OPTION)
  DO ESCN(38,5_";"_FG,"m",.OPTION) 
  DO ESCN(48,5_";"_BG,"m",.OPTION) 
  QUIT
  ;
VCLR24FG(FG,OPTION) ;"SET TEXT FOREGROUND COLOR, IN 24 BIT MODE.
  ;"From here: https://stackoverflow.com/questions/4842424/list-of-ansi-color-escape-sequences
  ;"**NOTICE**.  Because each color is an RGB triplet, this is NOT just a number
  ;"Input: FG -- FOREGROUND vector '<#;#;#>',  e.g. '134;56;122' 
  DO ESCN(48,2,";"_FG_"m",.OPTION)
  QUIT
  ;
VCLR24BG(BG,OPTION) ;"SET TEXT BACKGROUND COLOR, IN 24 BIT MODE.
  ;"From here: https://stackoverflow.com/questions/4842424/list-of-ansi-color-escape-sequences
  ;"**NOTICE**.  Because each color is an RGB triplet, this is NOT just a number
  ;"Input: BG -- BACKGROUND vector '<#;#;#>',  e.g. '134;56;122'
  DO ESCN(38,2,";"_BG_"m",.OPTION)
  QUIT
  ;
VCOLOR24B(FG,BG,OPTION) ;"SET TEXT COLORS, IN 24 BIT MODE.
  ;"From here: https://stackoverflow.com/questions/4842424/list-of-ansi-color-escape-sequences
  ;"**NOTICE**.  Because each color FG or BG color is an RGB triplet, this is NOT just a number
  ;"Input: FG -- FOREGROUND vector '<#;#;#>',  e.g. '134;56;122' 
  ;"Input: BG -- BACKGROUND vector '<#;#;#>',  e.g. '134;56;122'
  DO VCLR24FG(FG,.OPTION)
  DO VCLR24BG(BG,.OPTION)
  QUIT
  ;
VCOLOR24CLEAR ;
  DO ESCN(0,,"m",.OPTION)
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
  . DO VCOLORSIDX(.FG,.BG,.OPTION)
  ELSE  IF COLORMODE="256" DO
  . DO VCOLORS256(.FG,.BG,.OPTION)  
  ELSE  IF COLORMODE="24bit" DO
  . DO VCOLOR24B(.FG,.BG,.OPTION) 
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
  NEW R,G,B
  DO V24TORGB(CLRVEC,.R,.G,.B) ;
  QUIT $$INVCLRV24(R,G,B)
  ;  
ISCLRVEC24(STR) ;"Returns if STR is in format of 24bit color vector triple, CLRVEC24.  '#;#;#'
  NEW RESULT,IDX,VAL SET RESULT=($LENGTH(STR,";")=3) 
  FOR IDX=1:1:3 QUIT:(RESULT=0)  SET VAL=$PIECE(STR,";",IDX),RESULT=(+VAL=VAL)
  QUIT RESULT
  ;
V24TORGB(CLRVEC,R,G,B) ;"Split CLRVEC24 to R,G,B components.  
  SET R=+$PIECE(CLRVEC,";",1)
  SET G=+$PIECE(CLRVEC,";",2)
  SET B=+$PIECE(CLRVEC,";",3)
  QUIT
  ;"=======================================================================
  ;  
COLORPAIR(FG,BG,ARR) ;"DEPRECIATED, MOVED
  QUIT $$COLORPAIR^TMGUSRI8(.FG,.BG,.ARR)
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