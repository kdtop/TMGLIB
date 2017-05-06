TMGUSRI5 ;TMG/kst/USER INTERFACE API FUNCTIONS ;2/2/14, 4/24/15
         ;;1.0;TMG-LIB;**1**;02/2/2014
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
 ;"$$READKY^TMGUSRI5(TERMINATORS,TIMEOUT,NUM,INITVAL) -- custom read function with custom terminators
 ;
 ;"=======================================================================
 ;"Private Functions
 ;"=======================================================================
 ;"FXESCTBL  --FIX ESC TABLE
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;"DEPENDENCIES: none           
 ;"=======================================================================
 ;
 ;"NOTICE: The function below can read the [Backspace] key, which READ^XGF() can't
READKY(TERMINATORS,TIMEOUT,NUM,INITVAL,ESCKEY) ;
  ;"Purpose: a custom read function with custom terminators
  ;"Input: TERMINATORS -- OPTIONAL Flags to specify characters that will signal that
  ;"                      the user is done with input.  Flags as follows:
  ;"                      r = return/enter
  ;"                      t = tab
  ;"                      s = space
  ;"                      e = escape
  ;"                      b = backspace
  ;"                      NONE = no terminators
  ;"                    e.g. 'rte' means that if user enters a return, tab, or escape
  ;"                         then input it ended, and characters (up to, but not including
  ;"                         terminator) entered are returned.
  ;"                    e.g. 'NONE' --> NO terminators.  NOTE: MUST supply a number
  ;"                         characters to read, or endless loop will result.
  ;"                         If Terminator="", then default value of 'r' is used
  ;"       TIMEOUT --   OPTIONal -- the allowed lengh of time to wait before timeout.
  ;"                      default value is 999,999 seconds (~11 days)
  ;"       NUM --       OPTIONAL -- a number of characters to read, e.g. 5 to read just
  ;"                      5 characters (or less than 5 IF terminator encountered)
  ;"       INITVAL-- OPTIONAL -- This can be a value that presents the output
  ;"                      It also allows editing of former inputs.  Note: this function
  ;"                      assumes that INITVALue has been printed to the screen before
  ;"                      calling this function.
  ;"        ESCKEY--    OPTIONAL -- PASS BY REFERENCE, an OUT PARAMETER
  ;"                      If Terminator includes "e", then ESCKEY will be filled
  ;"                      with a translated value for esc sequence, e.g. UP
  ;"                      (as found in ^XUTL("XGKB",*))
  ;"
  ;"Result: returns characters read.
  NEW RESULT SET RESULT=$GET(INITVAL)
  NEW TMGZB
  SET TIMEOUT=+$GET(TIMEOUT,999999)
  SET NUM=$GET(NUM)
  SET TERMINATORS=$GET(TERMINATORS)
  IF TERMINATORS="" SET TERMINATORS="r"
  ELSE  IF TERMINATORS="NONE" SET TERMINATORS=""
  NEW TEMP
  NEW DONE SET DONE=0
  SET ESCKEY=""
  ;
  ;"NOTE, I could rewrite this to use built in terminators functions...
  ;"e.g. U $I:(TERMINATOR=$C(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,127))"
  ;
RLOOP ;
  XECUTE ^%ZOSF("EOFF") ;"echo off
  IF TERMINATORS["e" USE $I:ESCAPE
  READ *TEMP:TIMEOUT  ;"reads the ascii number of key (92, instead of 'a')
  SET TMGZB=$ZB
  ;"WRITE "  $l(TMGZB)=",$l(TMGZB)," TMGZB=" f i=1:1:$l(TMGZB) w $ASCII($E(TMGZB,IDX)),","
  IF TERMINATORS["e" USE $I:NOESCAPE
  XECUTE ^%ZOSF("EON")
  IF (TEMP=13)&(TERMINATORS["r") DO
  . SET DONE=1
  ELSE  IF (TEMP=9)&(TERMINATORS["t") DO
  . SET DONE=1
  ELSE  IF (TEMP=32)&(TERMINATORS["s") DO
  . SET DONE=1
  ELSE  IF (TEMP=27)&(TERMINATORS["e") DO
  . SET ESCKEY=$GET(^XUTL("XGKB",TMGZB))
  . IF ESCKEY="" DO
  . . DO FXESCTBL
  . . SET ESCKEY=$GET(^XUTL("XGKB",TMGZB))
  . SET DONE=1
  ELSE  IF (TEMP=127)&(TERMINATORS["b") DO
  . SET DONE=1
  ELSE  IF (TEMP=127)&(NUM=1) DO
  . SET DONE=1
  . SET ESCKEY="BACKSPC"
  ELSE  IF (TEMP'=-1) DO
  . IF TEMP=127 DO  QUIT
  . . IF RESULT="" QUIT
  . . SET RESULT=$EXTRACT(RESULT,1,$LENGTH(RESULT)-1)
  . . WRITE $CHAR(8)," ",$CHAR(8)
  . SET RESULT=RESULT_$CHAR(TEMP)
  . WRITE $CHAR(TEMP)
  . IF NUM="" QUIT
  . IF $LENGTH(RESULT)'<+NUM SET DONE=1
  IF 'DONE GOTO RLOOP
  QUIT RESULT
  ;
FXESCTBL  ;"FIX ESC TABLE
   ;"Purpose: There is a difference between my old system and the new.  I
   ;"         don't know why, but this will fix it for me, and anyone else.
T1 ;;$C(1))="^A"
   ;;$C(2))="^B"
   ;;$C(3))="^C"
   ;;$C(4))="^D"
   ;;$C(5))="^E"
   ;;$C(6))="^F"
   ;;$C(7))="^G"
   ;;$C(8))="^H"
   ;;$C(9))="TAB"
   ;;$C(10))="^J"
   ;;$C(11))="^K"
   ;;$C(12))="^L"
   ;;$C(13))="CR"
   ;;$C(14))="^N"
   ;;$C(15))="^O"
   ;;$C(16))="^P"
   ;;$C(17))="^Q"
   ;;$C(18))="^R"
   ;;$C(19))="^S"
   ;;$C(20))="^T"
   ;;$C(21))="^U"
   ;;$C(22))="^V"
   ;;$C(23))="^W"
   ;;$C(24))="^X"
   ;;$C(25))="^Y"
   ;;$C(26))="^Z"
   ;;$C(27)_"OM")="KPENTER"
   ;;$C(27)_"OP")="PF1"
   ;;$C(27)_"OQ")="PF2"
   ;;$C(27)_"OR")="PF3"
   ;;$C(27)_"OS")="PF4"
   ;;$C(27)_"Ol")="KP+"
   ;;$C(27)_"Om")="KP-"
   ;;$C(27)_"On")="KP."
   ;;$C(27)_"Op")="KP0"
   ;;$C(27)_"Oq")="KP1"
   ;;$C(27)_"Or")="KP2"
   ;;$C(27)_"Os")="KP3"
   ;;$C(27)_"Ot")="KP4"
   ;;$C(27)_"Ou")="KP5"
   ;;$C(27)_"Ov")="KP6"
   ;;$C(27)_"Ow")="KP7"
   ;;$C(27)_"Ox")="KP8"
   ;;$C(27)_"Oy")="KP9"
   ;;$C(27)_"[15~")="F5"
   ;;$C(27)_"[17~")="F6"
   ;;$C(27)_"[18~")="F7"
   ;;$C(27)_"[19~")="F8"
   ;;$C(27)_"[1~")="FIND"
   ;;$C(27)_"[20~")="F9"
   ;;$C(27)_"[21~")="F10"
   ;;$C(27)_"[23~")="F11"
   ;;$C(27)_"[24~")="F12"
   ;;$C(27)_"[25~")="F13"
   ;;$C(27)_"[26~")="F14"
   ;;$C(27)_"[28~")="HELP"
   ;;$C(27)_"[29~")="DO"
   ;;$C(27)_"[2~")="INSERT"
   ;;$C(27)_"[31~")="F17"
   ;;$C(27)_"[32~")="F18"
   ;;$C(27)_"[33~")="F19"
   ;;$C(27)_"[34~")="F20"
   ;;$C(27)_"[3~")="REMOVE"
   ;;$C(27)_"[4~")="SELECT"
   ;;$C(27)_"[5~")="PREV"
   ;;$C(27)_"[6~")="NEXT"
   ;;$C(27)_"[A")="UP"
   ;;$C(27)_"[B")="DOWN"
   ;;$C(27)_"[C")="RIGHT"
   ;;$C(27)_"[D")="LEFT"
   ;;$C(28))="^\"
   ;;$C(29))="^]"
   ;;$C(30))="^6"
   ;;$C(31))="^_"
   ;;#DONE#
   ;
   NEW IDX,STR
   FOR IDX=0:1 DO  QUIT:(STR["#DONE#")
   . SET STR=$TEXT(T1+IDX^TMGUSRI5)
   . QUIT:(STR["#DONE#")
   . SET STR=$PIECE(STR,";;",2)
   . NEW CODE SET CODE="SET ^XUTL(""XGKB"","_STR
   . XECUTE CODE
   QUIT
   ;
