TMGUSRI2 ;TMG/kst/SACC-compliant USER INTERFACE API FUNCTIONS ;5/6/18, 12/2/2019
         ;;1.0;TMG-LIB;**1,17**;07/17/12
  ;
  ;"TMG USER INTERFACE API FUNCTIONS
  ;"SACC-Compliant version
  ;
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
  ;"
  ;"This file is part of the TMG LIBRARY, and may only be used in accordence
  ;" to license terms outlined in separate file TMGLICNS.m, which should 
  ;" always be distributed with this file.
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;
  ;"NOTE: This will contain SACC-compliant versions of code from TMGUSRIF
  ;"      If routine is not found here, the please migrate the code
  ;"=======================================================================
  ;" API -- Public Functions.
  ;"=======================================================================
  ;"POPUPBOX(TMGHEADER,TMGTEXT,TMGWIDTH) -- Provide an easy text output box
  ;"PUARRAY(TMGINDENTW,TMGWIDTH,TMGARRAY,TMGMODAL) -- Draw a box and display text
  ;"PRESS2GO -- Provide a 'press key to continue' action
  ;"KEYPRESD(WANTCH,WAITTIME)  -- Check for a keypress
  ;"USRABORT(ABORTLABEL) -- Checks IF user pressed ESC key.  If so, verify
  ;"YNA(INIT) -- YNA = YES/NO/ALWAYS  ... similar to YN^DICN
  ;"PROGBAR(VALUE,LABEL,MIN,MAX,WIDTH,STARTTIME)  -- Animate a progress bar
  ;"MENU(OPTIONS,DEFCHOICE,USERRAW) -- provide a simple menuing system
  ;"FINDMTCH(INPUT,OPTIONS)  ;Search OPTIONS for matching input
  ;
  ;"=======================================================================
  ;"Private Functions
  ;"=======================================================================
  ;
  ;"=======================================================================
  ;"DEPENDENCIES: TMGSTUT2, TMGTERM
  ;"              DICN, XGF, XLFDT, XLFSTR
  ;"=======================================================================
  ;
PROGTEST ;
  ;"Purpose: test progress bar.
  NEW IDX,MAX
  NEW STARTH SET STARTH=$H
  SET MAX=200
  FOR IDX=0:1:MAX DO
  . DO PROGBAR^TMGUSRI2(IDX,"%",1,MAX,60,STARTH)
  . HANG 0.1
  QUIT
  ;
SPINTEST ;
  ;"Purpose: test progress bar.
  NEW IDX,MAX
  SET MAX=3000
  FOR IDX=0:10:MAX DO
  . DO PROGBAR^TMGUSRI2(IDX,"<A Label> "_IDX,-1,-1)
  . HANG 0.1
  QUIT
  ; 
TESTMENU ;
  NEW MENU,IDX
TML1 ;                    
  SET IDX=0  
  KILL MENU SET MENU(IDX)="Select Option For Testing Menu"
  SET MENU(IDX,1)="(2nd line here)"
  SET IDX=IDX+1,MENU(IDX)="Hello world!"_$CHAR(9)_"HELLO"
  SET IDX=IDX+1,MENU(IDX)="Ask HAL 9000"_$CHAR(9)_"HAL"
  SET IDX=IDX+1,MENU(IDX)="Shrug"_$CHAR(9)_"SHRUG"  
  SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
  IF USRPICK="^" GOTO TMDN  
  IF USRPICK="HELLO" DO  GOTO TML1
  . WRITE "The world says 'Hello' back",!   
  IF USRPICK="HAL" DO  GOTO TML1
  . WRITE "I'm sorry, I can't do that, Dave.",!   
  IF USRPICK="SHRUG" DO  GOTO TML1
  . WRITE "Yeah, I'm cool with it too.",!   
  GOTO TML1
TMDN  ;
  QUIT
  ;
POPUPBOX(TMGHEADER,TMGTEXT,TMGWIDTH,TMGINDENT) ;
   ;"PUBLIC FUNCTION
   ;"Purpose: To provide easy text output box
   ;"Input: TMGHEADER -- a short string for header
   ;"       TMGTEXT - the text to display
   ;"       [TMGWIDTH] -- optional width specifier. Value=0 same as not specified
   ;"       (TMGINDENT) -- uses a var with global scope (if defined) for indent amount
   ;"Note: If text width not specified, and TMGTEXT is <= 60,
   ;"   then all will be put on one line.
   ;"        Otherwise, width is SET to 60, and text is wrapped.
   ;"        Also, text of the message can contain "\n", which will be interpreted
   ;"        as a new-line character.
   ;"Result: none
   ;
   NEW TMGNEWLN SET TMGNEWLN="\n"
   NEW TMGTEXTOUT
   NEW TMGTEXTI SET TMGTEXTI=0
   NEW TMGPARTB SET TMGPARTB=""
   NEW TMGPARTB1 SET TMGPARTB1=""
   SET TMGWIDTH=+$GET(TMGWIDTH,0)
   ;
   SET TMGTEXTOUT(TMGTEXTI)=TMGHEADER
   SET TMGTEXTI=TMGTEXTI+1
   ;
   IF TMGWIDTH=0 DO
   . NEW TMGHEADERBASED
   . NEW TMGNUMLINES
   . NEW TMGHLEN SET TMGHLEN=$LENGTH(TMGHEADER)+4
   . NEW TMGTLEN SET TMGTLEN=$LENGTH(TMGTEXT)+4
   . IF TMGTLEN>TMGHLEN DO
   . . SET TMGWIDTH=TMGTLEN
   . . SET TMGHEADERBASED=0
   . ELSE  DO
   . . SET TMGWIDTH=TMGHLEN
   . . SET TMGHEADERBASED=1
   . IF TMGWIDTH>75 SET TMGWIDTH=75
   . SET TMGNUMLINES=TMGTLEN/TMGWIDTH
   . IF TMGTLEN#TMGWIDTH>0 SET TMGNUMLINES=TMGNUMLINES+1
   . IF (TMGNUMLINES>1)&(TMGHEADERBASED=0) DO
   . . SET TMGWIDTH=(TMGTLEN\TMGNUMLINES)+4
   . . IF TMGWIDTH<TMGHLEN SET TMGWIDTH=TMGHLEN
   . IF TMGWIDTH>75 SET TMGWIDTH=75
   ;
L1 ;"Load string up into TMGTEXT array, to pass to PUARRAY
   IF TMGTEXT[TMGNEWLN DO
   . DO CLEAVSTR^TMGSTUT2(.TMGTEXT,TMGNEWLN,.TMGPARTB1)
   DO SPLITSTR^TMGSTUT2(.TMGTEXT,(TMGWIDTH-4),.TMGPARTB)
   SET TMGPARTB=TMGPARTB_TMGPARTB1 SET TMGPARTB1=""
   SET TMGTEXTOUT(TMGTEXTI)=TMGTEXT
   SET TMGTEXTI=TMGTEXTI+1
   IF $LENGTH(TMGPARTB)>0 DO  GOTO L1
   . SET TMGTEXT=TMGPARTB
   . SET TMGPARTB=""
   ;
   DO PUARRAY(.TMGINDENT,TMGWIDTH,.TMGTEXTOUT)
   QUIT
   ;
PUARRAY(TMGINDENTW,TMGWIDTH,TMGARRAY,TMGMODAL) ;
   ;"PUBLIC FUNCTION
   ;"Purpose: To draw a box, of specified TMGWIDTH, and display text
   ;"Input: TMGINDENTW = width of indent amount (how far from left margin)
   ;"   TMGWIDTH = desired width of box.
   ;"        TMGHEADER = one line of text to put in header of popup box
   ;"        TMGARRAY: an array in following format:
   ;"                TMGARRAY(0)=TMGHEADER
   ;"                TMGARRAY(1)=Text line 1
   ;"                TMGARRAY(2)=Text line 2
   ;"                ...
   ;"                TMGARRAY(n)=Text line n
   ;"        TMGMODAL - really only has meaning for those time when
   ;"                box will be passed to GUI X dialog box.
   ;"                TMGMODAL=1 means stays in foreground,
   ;"                      0 means leave box up, continue script execution.
   ;"Note: Text will be clipped to fit in box.
   ;"Result: None
   ;
   NEW TMGCMODAL SET TMGCMODAL="MODAL"
   SET TMGMODAL=$GET(TMGMODAL,TMGCMODAL)
   NEW TMGHEADER
   NEW TMGTEXT SET TMGTEXT=""
   NEW INDEX,TMGI,S
   ;
   ;"Scan array for any needed data substitution i.e. {{...}}
   NEW TMGTEMPRESULT
   SET INDEX=$ORDER(TMGARRAY(""))
   FOR  DO  QUIT:INDEX=""
   . SET S=TMGARRAY(INDEX)
   . ;"SET TMGTEMPRESULT=$$CheckREPLSTR^TMGSTUT3Data(.S)  ;"Do any data lookup needed
   . SET TMGARRAY(INDEX)=S
   . SET INDEX=$ORDER(TMGARRAY(INDEX))
   ;
   SET TMGINDENTW=$GET(TMGINDENTW,1) ;"default indent=1
   SET TMGHEADER=$GET(TMGARRAY(0)," ")
   SET TMGWIDTH=$GET(TMGWIDTH,40)   ;"default=40
   ;
   WRITE !
   ;"Draw top line
   FOR TMGI=1:1:TMGINDENTW WRITE " "
   WRITE "+"
   FOR TMGI=1:1:(TMGWIDTH-2) WRITE "="
   WRITE "+",!
   ;
   ;"Draw Header line
   DO SETSTLEN^TMGSTUT2(.TMGHEADER,TMGWIDTH-4)
   FOR TMGI=1:1:TMGINDENTW WRITE " "
   WRITE "| ",TMGHEADER," |..",!
   ;
   ;"Draw divider line
   FOR TMGI=1:1:TMGINDENTW WRITE " "
   WRITE "+"
   FOR TMGI=1:1:(TMGWIDTH-2) WRITE "-"
   WRITE "+ :",!
   ;
   ;"Put out message
   SET INDEX=$ORDER(TMGARRAY(0))
PUBLOOP ;
   IF INDEX="" GOTO BTMLINE
   SET S=$GET(TMGARRAY(INDEX)," ")
   DO SETSTLEN^TMGSTUT2(.S,TMGWIDTH-4)
   FOR TMGI=1:1:TMGINDENTW WRITE " "
   WRITE "| ",S," | :",!
   SET INDEX=$ORDER(TMGARRAY(INDEX))
   GOTO PUBLOOP
   ;
BTMLINE ;"Draw Bottom line
   FOR TMGI=1:1:TMGINDENTW WRITE " "
   WRITE "+"
   FOR TMGI=1:1:(TMGWIDTH-2) WRITE "="
   WRITE "+ :",!
   ;
   ;"Draw bottom shaddow
   FOR TMGI=1:1:TMGINDENTW WRITE " "
   WRITE "  "
   WRITE ":"
   FOR TMGI=1:1:(TMGWIDTH-2) WRITE "."
   WRITE ".",!
   ;
   WRITE !
PUADONE QUIT
   ;
PRESS2GO ;"PRESS TO CONTINUE / PRESSTOCONT
   ;"Purpose: to provide a 'press key to continue' action
   ;"RESULT: none
   ;"Output: will SET TMGPTCABORT=1 IF user entered ^
   NEW STR SET STR="----- Press Key To Continue -----"
   NEW LENSTR SET LENSTR=$LENGTH(STR)
   WRITE STR
   NEW CH SET CH=$$KEYPRESD(0,240)
   IF (CH=94) SET TMGPTCABORT=1  ;"set abort user entered ^
   ELSE  KILL TMGPTCABORT
   DO CUB^TMGTERM(LENSTR)
   SET $X=$X-LENSTR
   NEW I FOR I=1:1:LENSTR WRITE " "
   DO CUB^TMGTERM(LENSTR)
   SET $X=$X-LENSTR
   ;"WRITE !
   QUIT
   ;
KEYPRESD(WANTCH,WAITTIME)  ;
   ;"Purpose: to check for a keypress
   ;"Input: WANTCH -- OPTIONAL, if 1, then Character is returned, not ASCII value
   ;"       WAITTIME -- OPTIONAL, default is 0 (immediate return)
   ;"Result: ASCII value of key, if pressed, -1 otherwise ("" IF WANTCH=1)
   ;"Note: this does NOT wait for user to press key
   SET WAITTIME=+$GET(WAITTIME)
   ;----------------------
   ;"NOTE: 10/14/15 -- There is a linux kernel bug that causes gtm to crash
   ;"     when executing READ V#1:0 
   ;"  So for now, will short circuit this function.  Consider trying again
   ;"  after update past Ubuntu LTS 2014
   IF WAITTIME=0 QUIT -1
   ;----------------------
   NEW INITXGRT SET INITXGRT=($DATA(XGRT)'=0)
   IF 'INITXGRT DO INITKB^XGF("*")
   NEW TEMP SET TEMP=$$READ^XGF(1,WAITTIME)
   IF $GET(WANTCH)'=1 SET TEMP=$ASCII(TEMP)
   IF 'INITXGRT DO RESETKB^XGF
   QUIT TEMP
   ;
USRABORT(ABORTLABEL)  ;
   ;"Purpose: Checks IF user pressed ESC key.  If so, then ask IF abort wanted
   ;"Note: return is immediate.
   ;"Returns: 1 IF user aborted, 0 IF not.
   NEW RESULT SET RESULT=0
   IF $$KEYPRESD=27 DO
   . NEW % SET %=2
   . WRITE !,"Abort"
   . IF $GET(ABORTLABEL)'="" DO
   . . WRITE " "_ABORTLABEL
   . DO YN^DICN WRITE !
   . SET RESULT=(%=1)
   QUIT RESULT
   ;
YNA(INIT) ;" YNA = YES/NO/ALWAYS
  ;"Purpose: Similar to YN^DICN, this will ask for Yes No, but also allow for
  ;"         user to specify to for ALWAYS (as long as globally scoped var exists)
  ;"        This will achieved by allowed replies of: Y, YES, Y!, YES!, N, NO, N!, No!, ?, ^
  ;"        Input is not case sensitive. 
  ;"Input: INIT.  OPTIONAL.  If 0, default to NO, if 1, default to YES, if null, then no default
  ;"Results: format: ResultValue^AlwaysValue.
  ;"              ResultValue: 1=YES, 0 = NO, -1 = abort
  ;"              AlwaysValue: 1 = always, 0 or null = not always
  ;"NOTE: This will access globally scoped variable TMGYNAUSRI2(<stack call label>)
  NEW STACK ZSHOW "S":STACK
  NEW CALLLABEL SET CALLLABEL=$GET(STACK("S",1)) IF CALLLABEL="" SET CALLLABEL="DEFAULT"
  SET TMGYNAUSRI2(CALLLABEL)=$GET(TMGYNAUSRI2(CALLLABEL))
  IF TMGYNAUSRI2(CALLLABEL)'="" SET TMGRESULT=TMGYNAUSRI2(CALLLABEL) GOTO YNADN
  NEW TEMP,INITDISP,ALWAYS
  SET INIT=$GET(INIT)
  SET INITDISP=$SELECT(INIT=1:"YES",INIT=0:"NO",1:"")
  NEW TMGRESULT SET TMGRESULT=""
YNAL1  ;  
  IF INITDISP'="" WRITE " ",INITDISP,"// "
  WRITE "? " READ TEMP:DTIME SET TEMP=$$UP^XLFSTR(TEMP)
  IF TEMP="" SET TEMP=INITDISP IF TEMP="" SET TEMP="^"
  SET ALWAYS=(TEMP["!") IF ALWAYS SET TEMP=$TRANSLATE(TEMP,"!","")
  IF TEMP="YES" SET TEMP="Y"
  IF TEMP="NO" SET TEMP="N"
  IF ("YN?^"'[TEMP)!($LENGTH(TEMP)>1) DO  GOTO YNAL1
  . WRITE !,"    Answer with 'Yes' or 'No' or '?' or '^'",!,!
  IF TEMP["?" DO  GOTO YNAL1
  . WRITE !,"    Acceptable inputs:",!
  . WRITE "       'Y', 'YES', 'Yes', 'yes',",!
  . WRITE "       'N', 'NO', No', 'no',",!
  . WRITE "       '^' (for abort)'",!  
  . WRITE "       Include '!' to always answer this question the same",!
  . WRITE "         e.g. 'Y!', or 'NO!'",!,!
  WRITE " (",$SELECT(TEMP="Y":"Yes",TEMP="N":"No",TEMP="^":"Abort",1:"??"),$SELECT(ALWAYS:" always",1:""),")"
  SET TMGRESULT=$SELECT(TEMP="Y":1,TEMP="N":0,1:-1)
  IF ALWAYS SET TMGRESULT=TMGRESULT_"^1",TMGYNAUSRI2(CALLLABEL)=TMGRESULT
YNADN
  QUIT TMGRESULT
  ;
PROGBAR(VALUE,LABEL,MIN,MAX,WIDTH,STARTTIME)  ;"ProgressBar
   ;"Purpose: to draw a progress bar on a line of the screen
   ;"Input:   VALUE -- the current value to graph out
   ;"         LABEL -- OPTIONAL -- a label to describe progres.  Default="Progress"
   ;"         MIN -- OPTIONAL -- the minimal number that value will be.  Default is 0
   ;"                      IF MAX=-1 and MIN=-1 then turn on spin mode (see below)
   ;"         MAX -- OPTIONAL -- the max number that value will be. Default is 100
   ;"                      IF MAX=-1 and MIN=-1 then turn on spin mode (see below)
   ;"         WIDTH -- OPTIONAL -- the number of characters that the progress bar
   ;"                              will be in WIDTH.  Default is 70
   ;"         STARTTIME -- OPTIONAL -- start time of process.  If provided, it will
   ;"              be used to determine remaining time.  Format should be same as $H
   ;"Note: will use global ^TMP("TMG-PROGRESS-BAR",$J)
   ;"Note: bar will look like this:
   ;"              Progress:  27%-------->|-----------------------------------| (Time)
   ;"Note--Spin Mode: To show motion without knowing the max amount, a spin mode is needed.
   ;"         Progress:  |-----<==>--------------------------------------|
   ;"              And the bar will move back and forth.
   ;"              In this mode, value is ignored and is thus optional.
   ;"              To use this mode, SET MAX=-1,MIN=-1
   ;"Result: None
   ;
   ;"FYI -- The preexisting way to DO this, from Dave Whitten
   ;"
   ;"Did you try using the already existing function to DO this?
   ;"ie: try out this 'mini program'
   ;">; need to SET up vars like DUZ,DTIME, IO, IO(0), etc.
   ;" D INIT^XPDID
   ;" S XPDIDTOT=100
   ;" D TITLE^XPDID("hello world")
   ;" D UPDATE^XPDID(50)
   ;" F AJJ=90:1:100 D UPDATE^XPDID(I)
   ;" D EXIT^XPDID()
   ;"
   ;"The XPDID routine does modify the scroll region and make the
   ;"application seem a bit more "GUI"-like, by the way...
   ;"
   ;"David
   ;
   DO  ;"Turn off cursor display, to prevent flickering
   . NEW $ETRAP SET $ETRAP=""
   . XECUTE ^%ZOSF("TRMOFF")
   ;
   NEW PREMARK,TEMPI,POSTMARK,PCT
   NEW REFCT SET REFCT=$NAME(^TMP("TMG-PROGRESS-BAR",$J))
   SET MAX=+$GET(MAX,100),MIN=+$GET(MIN,0)
   SET WIDTH=+$GET(WIDTH,70)
   SET LABEL=$GET(LABEL,"Progress")
   ;
   NEW SPINMODE SET SPINMODE=((MAX=-1)&(MIN=-1))
   IF SPINMODE GOTO SPIN1  ;"<-- skip all this for spin mode
   ;
   IF (MAX-MIN)=0 SET PCT=0
   ELSE  SET PCT=(VALUE-MIN)/(MAX-MIN)
   IF PCT>1 SET PCT=1
   IF PCT<0 SET PCT=0
   IF (PCT<1)&($GET(STARTTIME)="") SET STARTTIME=$H
   ;
   SET STARTTIME=$GET(STARTTIME)  ;" +$GET 61053,61748 --> 61053
   ;
   NEW BARBERPOLE SET BARBERPOLE=+$GET(@REFCT@("BARBER POLE"))
   IF $GET(@REFCT@("BARBER POLE","LAST INC"))'=$H DO
   . SET BARBERPOLE=(BARBERPOLE-1)#4
   . SET @REFCT@("BARBER POLE")=BARBERPOLE ;"should be 0,1,2, or 3)
   . SET @REFCT@("BARBER POLE","LAST INC")=$H
   ;
   NEW CURRATE SET CURRATE=""
   IF $GET(@REFCT@("START-TIME"))=STARTTIME DO
   . NEW INTERVAL SET INTERVAL=$GET(@REFCT@("SAMPLING","INTERVAL"),10)
   . SET CURRATE=$GET(@REFCT@("LATEST-RATE"))
   . NEW COUNT SET COUNT=$GET(@REFCT@("SAMPLING","COUNT"))+1
   . IF COUNT#INTERVAL=0 DO
   . . NEW DELTATIME,DELTAVAL
   . . SET DELTATIME=$$HDIFF^XLFDT($H,$GET(@REFCT@("SAMPLING","REF-TIME")),2)
   . . IF DELTATIME=0 SET INTERVAL=INTERVAL*2
   . . ELSE  IF DELTATIME>1000 SET INTERVAL=INTERVAL\1.5
   . . SET DELTAVAL=VALUE-$GET(@REFCT@("SAMPLING","VALUE COUNT"))
   . . IF DELTAVAL>0 SET CURRATE=DELTATIME/DELTAVAL  ;"dT/dValue
   . . ELSE  SET CURRATE=""
   . . SET @REFCT@("LATEST-RATE")=CURRATE
   . . SET @REFCT@("SAMPLING","REF-TIME")=$H
   . . SET @REFCT@("SAMPLING","VALUE COUNT")=VALUE
   . SET @REFCT@("SAMPLING","COUNT")=COUNT#INTERVAL
   . SET @REFCT@("SAMPLING","INTERVAL")=INTERVAL
   ELSE  DO
   . KILL @REFCT
   . SET @REFCT@("START-TIME")=STARTTIME
   . SET @REFCT@("SAMPLING","COUNT")=0
   . SET @REFCT@("SAMPLING","REF-TIME")=$H
   . SET @REFCT@("SAMPLING","VALUE COUNT")=VALUE
   ;
   NEW TIMESTR SET TIMESTR="  "
   NEW REMAININGTIME SET REMAININGTIME=""
   NEW DELTA SET DELTA=0
   ;
   IF CURRATE'="" DO
   . NEW REMAINVAL SET REMAINVAL=(MAX-VALUE)
   . IF REMAINVAL'<0 DO
   . . SET REMAININGTIME=CURRATE*REMAINVAL
   . ELSE  DO
   . . SET DELTA=-1,REMAININGTIME=$$HDIFF^XLFDT($H,STARTTIME,2)
   ELSE  IF $DATA(STARTTIME) DO
   . IF PCT=0 QUIT
   . SET TIMESTR=""
   . SET DELTA=$$HDIFF^XLFDT($H,STARTTIME,2)
   . IF DELTA<0 SET REMAININGTIME=-DELTA ;"just report # sec's overrun.
   . SET REMAININGTIME=DELTA*((1/PCT)-1)
   ;
   IF REMAININGTIME'="" DO
   . NEW DAYS SET DAYS=REMAININGTIME\86400  ;"86400 sec per day.
   . IF DAYS>5 SET TIMESTR="<Stalled>  " QUIT
   . SET REMAININGTIME=REMAININGTIME#86400
   . NEW HOURS SET HOURS=REMAININGTIME\3600  ;"3600 sec per hour
   . SET REMAININGTIME=REMAININGTIME#3600
   . NEW MINUTES SET MINUTES=REMAININGTIME\60  ;"60 sec per min
   . NEW SECONDS SET SECONDS=(REMAININGTIME#60)\1
   . IF DAYS>0 SET TIMESTR=TIMESTR_DAYS_"d, "
   . IF HOURS>0 SET TIMESTR=TIMESTR_HOURS_"h:"
   . IF (MIN=0)&(SECONDS=0) DO
   . . SET TIMESTR="       "
   . ELSE  DO
   . . SET TIMESTR=TIMESTR_MINUTES_":"
   . . IF SECONDS<10 SET TIMESTR=TIMESTR_"0"
   . . SET TIMESTR=TIMESTR_SECONDS_"   "
   . IF DELTA<0 SET TIMESTR="+"_TIMESTR ;"just report # sec's overrun.
   ELSE  SET TIMESTR="?? Time"
   ;
   SET WIDTH=WIDTH-$LENGTH(LABEL)-($LENGTH(TIMESTR)+1)
   SET PREMARK=(WIDTH*PCT)\1
   SET POSTMARK=WIDTH-PREMARK
   ;
   IF (MAX-MIN)=0 SET PCT=0
   ELSE  SET PCT=(VALUE-MIN)/(MAX-MIN)
   IF PCT>1 SET PCT=1
   IF PCT<0 SET PCT=0
   IF (PCT<1)&($GET(STARTTIME)="") SET STARTTIME=$H
   ;
   WRITE LABEL,":"
   IF PCT<1 WRITE " "
   IF PCT<0.1 WRITE " "
   WRITE (PCT*100)\1,"% "
   FOR TEMPI=0:1:PREMARK-1 DO
   . IF (BARBERPOLE+TEMPI)#4=0 WRITE "~"
   . ELSE  WRITE "-"
   WRITE ">|"
   FOR TEMPI=1:1:(POSTMARK-1) WRITE "-"
   IF POSTMARK>0 WRITE "| "
   WRITE TIMESTR
   ;
   GOTO PBD1
   ;
SPIN1   NEW SPINBAR SET SPINBAR=+$GET(@REFCT@("SPIN BAR"))
   NEW SPINDIRECTION SET SPINDIRECTION=+$GET(@REFCT@("SPIN BAR","DIR")) ;"1=forward, -1=backwards
   IF SPINDIRECTION=0 SET SPINDIRECTION=1
   SET SPINBAR=SPINBAR+SPINDIRECTION
   IF SPINBAR>100 DO
   . SET SPINDIRECTION=-1
   . SET SPINBAR=100
   IF SPINBAR<0 DO
   . SET SPINDIRECTION=1
   . SET SPINBAR=0
   SET @REFCT@("SPIN BAR")=SPINBAR
   SET @REFCT@("SPIN BAR","DIR")=SPINDIRECTION
   SET @REFCT@("SPIN BAR","LAST INC")=$H
   ;
   NEW MARKER SET MARKER="<=>"
   SET WIDTH=WIDTH-$LENGTH(LABEL)-$LENGTH(MARKER)
   SET PCT=SPINBAR/100
   SET PREMARK=(WIDTH*PCT)\1
   SET POSTMARK=WIDTH-PREMARK
   ;
   WRITE LABEL," |"
   FOR TEMPI=0:1:PREMARK-1 WRITE "-"
   WRITE MARKER
   FOR TEMPI=1:1:(POSTMARK-1) WRITE "-"
   IF PCT<1 WRITE "-"
   WRITE "|"
   ;
PBD1    ;"WRITE $CHAR(13) SET $X=0
   WRITE !
   DO CUU^TMGTERM(1)
   ;
PBDONE  DO  ;"Turn cursor display back on.
   . ;"NEW $ETRAP SET $ETRAP=""
   . ;"XECUTE ^%ZOSF("TRMON")
   . ;"U $I:(TERMINATOR=$C(13,127))
   ;
   QUIT
   ;
MENU(OPTIONS,DEFCHOICE,USERRAW)  ;
   ;"Purpose: to provide a simple menuing system
   ;"Input:  OPTIONS -- PASS BY REFERENCE
   ;"        Format:
   ;"          OPTIONS(0)=Header Text   <--- optional, default is MENU
   ;"          OPTIONS(0,n)=Additional lines of header Text   <--- optional
   ;"          OPTIONS(DispNumber)=MenuText_$C(9)_ReturnValue <-- _$C(9)_ReturnValue OPTIONAL, default is DispNumber
   ;"          OPTIONS(DispNumber)=MenuText_$C(9)_ReturnValue
   ;"          OPTIONS(DispNumber)=MenuText_$C(9)_ReturnValue
   ;"          OPTIONS(-1,"COLOR","FG")=foreground color  (optional)
   ;"          OPTIONS(-1,"COLOR","BG")=foreground color  (optional)
   ;"          OPTIONS(-1,"INDENT")=# of spaces to add to beginning of each line. 
   ;"        DEFCHOICE: OPTIONAL, the default menu value
   ;"        USERRAW : OPTIONAL, PASS BY REFERENCE, an OUT PARAMETER.  Returns users raw input
   ;"Results: The selected ReturnValue (or DispNumber if no ReturnValue provided), or ^ for abort
   NEW RESULT SET RESULT="^"
   NEW S,FG,BG,HDREXTRA
   NEW WIDTH SET WIDTH=50
   NEW INDENTN SET INDENTN=+$GET(OPTIONS(-1,"INDENT"))
   NEW INDENT SET INDENT="" IF INDENTN>0 SET INDENT=$JUSTIFY(" ",INDENTN)
   NEW LINE SET $PIECE(LINE,"=",WIDTH+1)=""
MNU1 ;
   IF $DATA(OPTIONS(-1,"COLOR")) DO
   . SET FG=$GET(OPTIONS(-1,"COLOR","FG"),0)
   . SET BG=$GET(OPTIONS(-1,"COLOR","BG"),1)
   . DO VCOLORS^TMGTERM(FG,BG)
   WRITE INDENT,LINE,!
   WRITE INDENT,$GET(OPTIONS(0),"MENU"),$$PAD2POS^TMGSTUT2(WIDTH),!
   SET HDREXTRA=0
   FOR  SET HDREXTRA=$ORDER(OPTIONS(0,HDREXTRA)) QUIT:HDREXTRA=""  DO
   . WRITE INDENT,OPTIONS(0,HDREXTRA),$$PAD2POS^TMGSTUT2(WIDTH),!
   WRITE INDENT,LINE,!
   WRITE INDENT,"OPTIONS:",$$PAD2POS^TMGSTUT2(WIDTH),!
   ;
   NEW DISPNUMBER SET DISPNUMBER=$ORDER(OPTIONS(0))
   IF DISPNUMBER'="" FOR  DO  QUIT:(DISPNUMBER="")
   . SET S=$GET(OPTIONS(DISPNUMBER))
   . WRITE INDENT,$$RJ^XLFSTR(DISPNUMBER,4),".",$$PAD2POS^TMGSTUT2(6)
   . IF $DATA(OPTIONS(DISPNUMBER,"COLOR")) DO
   . . SET FG=$GET(OPTIONS(DISPNUMBER,"COLOR","FG"),0)
   . . SET BG=$GET(OPTIONS(DISPNUMBER,"COLOR","BG"),1)
   . . DO VCOLORS^TMGTERM(FG,BG)
   . WRITE " ",$PIECE(S,$CHAR(9),1),$$PAD2POS^TMGSTUT2(WIDTH-1)
   . IF $DATA(OPTIONS(DISPNUMBER,"COLOR")) DO
   . . DO VTATRIB^TMGTERM(0) ;"Reset colors
   . WRITE " ",!
   . SET DISPNUMBER=$ORDER(OPTIONS(DISPNUMBER))
   ;
   WRITE INDENT,LINE,!
   ;
   SET DEFCHOICE=$GET(DEFCHOICE) 
   IF DEFCHOICE="" SET DEFCHOICE="^"
   NEW INPUT
   WRITE INDENT,"Enter selection (^ to abort): ",DEFCHOICE,"// "
   READ INPUT:$GET(DTIME,3600),!
   IF INPUT="" SET INPUT=DEFCHOICE
   SET USERRAW=INPUT
   IF INPUT="^" GOTO MNUDONE
   ;
   IF +INPUT'=INPUT SET INPUT=$$FINDMTCH(INPUT,.OPTIONS)
   SET S=$GET(OPTIONS(INPUT))
   IF S="" SET S=$GET(OPTIONS($$UP^XLFSTR(INPUT)))
   ;"IF S="" WRITE "??",!! GOTO MNU1
   SET RESULT=$PIECE(S,$CHAR(9),2)
   IF RESULT="" SET RESULT=INPUT
   ;
MNUDONE ;
   IF $DATA(OPTIONS(-1,"COLOR")) DO VTATRIB^TMGTERM(0) ;"Reset colors
   QUIT RESULT
   ;
FINDMTCH(INPUT,OPTIONS)  ;"Search OPTIONS for matching input
  ;"Input: INPUT -- user response
  ;"       OPTIONS -- by reference.  This is the array that defines the menu
  ;"Result: Number of found match, or "?" IF no match
  NEW TMGRESULT SET TMGRESULT="?"
  NEW IDX SET IDX=0
  NEW OPTTEXT,INPUTLEN
  SET INPUTLEN=$LENGTH(INPUT)
  SET INPUT=$$UP^XLFSTR(INPUT)
  FOR  SET IDX=$ORDER(OPTIONS(IDX)) QUIT:(IDX'>0)!(TMGRESULT'="?")  DO
  . SET OPTTEXT=$GET(OPTIONS(IDX))
  . SET OPTTEXT=$EXTRACT(OPTTEXT,0,INPUTLEN)
  . SET OPTTEXT=$$UP^XLFSTR(OPTTEXT)
  . IF OPTTEXT=INPUT SET TMGRESULT=IDX
FMDN  ;
  QUIT TMGRESULT
  ; 