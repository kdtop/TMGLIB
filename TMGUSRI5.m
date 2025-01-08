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
 ;"$$RUNKEYS(OPTION)  --Loop to run keyboard and callback to user-defined functions
 ;"TESTRK --Demonstration of RUNKEYS()
 ;
 ;"=======================================================================
 ;"Demo Functions
 ;"=======================================================================
 ;"TESTRK --Demonstration of RUNKEYS()
 ;"HNDLKEY(KEY) -- callback for TESTRK
 ;"HNDLSTR(STR) -- callback for TESTRK
 ;"HNDLCSR(NAME) -- callback for TESTRK
 ;
 ;"=======================================================================
 ;"Private Functions
 ;"=======================================================================
 ;"FXESCTBL  --Fix escape sequences table in ^XUTL
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;"DEPENDENCIES: none           
 ;"=======================================================================
 ;
READKY(TERMINATORS,TIMEOUT,NUM,INITVAL,ESCKEY,EOFF) ;
  ;"NOTICE: The function below can read the [Backspace] key, which READ^XGF() can't
  ;"Purpose: a custom read function with custom terminators
  ;"Input: TERMINATORS -- OPTIONAL Flags to specify characters that will signal that
  ;"                      the user is done with input.  Flags as follows:
  ;"                      r = return/enter
  ;"                      t = tab
  ;"                      s = space
  ;"                      e = escape
  ;"                      b = backspace
  ;"                      NONE = no terminators
  ;"                      e.g. 'rte' means that if user enters a return, tab, or escape
  ;"                           then input it ended, and characters (up to, but not including
  ;"                           terminator) entered are returned.
  ;"                      e.g. 'NONE' --> NO terminators.  NOTE: MUST supply a number
  ;"                           characters to read, or endless loop will result.
  ;"                         If Terminator="", then default value of 'r' is used
  ;"       TIMEOUT --   OPTIONAL -- the allowed lengh of time to wait before timeout.
  ;"                      default value is 999,999 seconds (~11 days)
  ;"       NUM --       OPTIONAL -- a number of characters to read, e.g. 5 to read just
  ;"                      5 characters (or less than 5 if terminator encountered)
  ;"       INITVAL--    OPTIONAL -- This can be a value that presents the output
  ;"                      It also allows editing of former inputs.  Note: this function
  ;"                      assumes that INITVALue has been printed to the screen before
  ;"                      calling this function.
  ;"       ESCKEY--     OPTIONAL -- PASS BY REFERENCE, an OUT PARAMETER
  ;"                      If Terminator includes "e", then ESCKEY will be filled
  ;"                      with a translated value for esc sequence, e.g. UP
  ;"                      (as found in ^XUTL("XGKB",*))
  ;"       EOFF --      OPTIONAL -- If 1 then input is NOT output to screen.  //11/24 NOTE: Not apparently in use.  EOFF always used    
  ;"
  ;"NOTE: to do a single keystroke read, and get back cursor keys etc, do this:
  ;"      SET VAL=$$READKY^TMGUSRI5("e",,1,,.ESC) IF VAL="" SET VAL=ESC
  ;
  ;"      Input Key      Name returned
  ;"      ---------      -------------
  ;"      Up key     --> "UP"
  ;"      Down Key   --> "DOWN"
  ;"      Right Key  --> "RIGHT"
  ;"      Left Key   --> "LEFT"
  ;"      PgUp Key   --> "PREV"
  ;"      PgDn Key   --> "NEXT"
  ;"      Home Key   --> "HOME"
  ;"      End Key    --> "END"  
  ;
  ;"Result: returns characters read.
  NEW RESULT SET RESULT=$GET(INITVAL)
  NEW TMGZB
  SET TIMEOUT=+$GET(TIMEOUT,999999)
  SET NUM=$GET(NUM)
  SET TERMINATORS=$GET(TERMINATORS)
  IF TERMINATORS="" SET TERMINATORS="r"
  ELSE  IF TERMINATORS="NONE" DO
  . SET TERMINATORS=""
  . IF NUM'>0 SET NUM=1  ;"//kt added 8/30/17
  NEW TEMP
  NEW DONE SET DONE=0
  SET ESCKEY=""
  SET EOFF=+$GET(EOFF)
  ;
  ;"NOTE, I could rewrite this to use built in terminators functions...
  ;"e.g. U $I:(TERMINATOR=$C(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,127))"
  ;
RLOOP ;
  XECUTE ^%ZOSF("EOFF") ;"echo off
  IF TERMINATORS["e" USE $I:ESCAPE
  READ *TEMP:TIMEOUT  ;"reads the ascii number of key (92, instead of 'a')
  SET TMGZB=$ZB
  ;" --- DEBUGGING ---
  ;"w !,"TEMP=[",TEMP,"]",!  ;"debugging...
  ;"WRITE "$LENGTH(TMGZB)=",$LENGTH(TMGZB)," TMGZB="    ;"debugging...
  ;"f IDX=1:1:$l(TMGZB) w $ASCII($E(TMGZB,IDX)),","     ;"debugging...
  ;"WRITE !   ;"debugging...                            
  ;" -------------------
  IF TERMINATORS["e" USE $I:NOESCAPE
  XECUTE ^%ZOSF("EON")
  IF (TEMP=13) DO                        ;"RETURN KEY
  . IF (TERMINATORS["r") SET DONE=1
  . IF NUM=1 SET ESCKEY="CR",DONE=1
  ELSE  IF (TEMP=9) DO                   ;"TAB KEY
  . IF (TERMINATORS["t") SET DONE=1
  . IF NUM=1 SET ESCKEY="TAB",DONE=1
  ELSE  IF (TEMP=32) DO                  ;"SPACE KEY
  . IF (TERMINATORS["s") SET DONE=1
  ELSE  IF (TEMP=127) DO                 ;"BACKSPACE KEY
  . IF (TERMINATORS["b") SET DONE=1 QUIT
  . IF NUM=1 SET ESCKEY="BACKSPC",DONE=1 QUIT
  . IF RESULT'="" DO  
  . . SET RESULT=$EXTRACT(RESULT,1,$LENGTH(RESULT)-1)
  . . WRITE $CHAR(8)," ",$CHAR(8)
  . SET TEMP=-1
  ELSE  IF TEMP<32,(TEMP'=27) DO   ;"//kt 8/27/19
  . SET RESULT=$GET(^XUTL("XGKB",TEMP))
  . SET DONE=1
  . ;"WRITE !,!,"DEBUG: TEMP=",TEMP," RESULT=",RESULT,!
  ;"NOTE: The DELETE key generates an escape sequence
  ELSE  IF (TEMP=27)&(TERMINATORS["e") DO
  . SET ESCKEY=$GET(^XUTL("XGKB",TMGZB))
  . IF ESCKEY="" DO
  . . DO FXESCTBL
  . . SET ESCKEY=$GET(^XUTL("XGKB",TMGZB))
  . SET DONE=1
  IF ('DONE),(TEMP'=-1) DO
  . SET RESULT=RESULT_$CHAR(TEMP)
  . IF 'EOFF WRITE $CHAR(TEMP)
  . IF NUM="" QUIT   
  . IF $LENGTH(RESULT)'<+NUM SET DONE=1
  IF 'DONE GOTO RLOOP
  QUIT RESULT
  ;
  ;"===== DEMO ================================================================
TESTRK ;"Demonstration of RUNKEYS()
  NEW OPT 
  SET OPT("ON STRING")="HNDLSTR^TMGUSRI5"
  SET OPT("ON KEY")="HNDLKEY^TMGUSRI5"
  SET OPT("ON CURSOR")="HNDLCSR^TMGUSRI5"
  NEW RESULT SET RESULT=$$RUNKEYS(.OPT)
  QUIT
  ;
HNDLKEY(KEY,OPT) ;"Callback for TESTRK
  NEW RESULT SET RESULT="1^OK"
  IF KEY="^" SET RESULT="-1^DONE"
  IF KEY="<CR>" SET RESULT="-1^DONE"
  QUIT RESULT
  ;
HNDLSTR(STR,OPT) ;"Callback for TESTRK
  WRITE STR,!
  QUIT "1^OK"
  ;
HNDLCSR(NAME,OPT) ;"Callback for TESTRK
  WRITE NAME,!
  QUIT "1^OK"
  ;
  ;"===== END DEMO ================================================================
  ;
RUNKEYS(OPTION,USERDATA)  ;"Loop to run keyboard and callback to user-defined functions
  ;"Input:  OPTION -- PASS BY REFERENCE.  Format:
  ;"            OPTION("EOFF")=1 --OPTIONAL.  If found, then user pressing keys will NOT be echoed to output.  
  ;"            -- EVENT HANDLERS --
  ;"               NOTES: Just the name of the function and routine should be given, e.g. "MYFUN^MYRTN"
  ;"                      The number of parameters the code will accept will be specified below
  ;"                      Each function should return a result, e.g. "1^OK" or "-1^<ERR MSG>".  If it returns "-1", then loop is exited.
  ;"            OPTION("ON KEY")    - Optional. "MYFUN^MYRTN"  Must take 3 parameter, e.g. $$MYFUN^MYRTN(.KEY,.OPTION,.USERDATA)
  ;"                Fires after every key enter.  If function returns "^", then loop is exited.  
  ;"            OPTION("ON CURSOR") - Optional. "MYFUN^MYRTN"  Must take 3 parameters, e.g. $$MYFUN^MYRTN(.NAME,.OPTION,.USERDATA)
  ;"                Fires if cursor direction key entered. 
  ;"            OPTION("ON STRING") - Optional. "MYFUN^MYRTN"   Must take 3 parametes, e.g. $$MYFUN^MYRTN(.STRING,.OPTION,.USERDATA)
  ;"                Fires after user done entering a string.  
  ;"                NOTE: This RUNKEYS does NOT act as a line editor.  It will build up what
  ;"                    user enters, stopping when CR or other non-alpha key is entered.  But it 
  ;"                    will NOT allow backing up, to delete etc.
  ;"            OPTION("DATA",....) -- user area for storing date for individualized use.  Passed to event handlers with rest of .OPTION
  ;"        USERDATA -- PASS BY REFERENCE.  An array of user data to be passed to event handlers. 
  ;"RESULTS: 1^OK, or -1^ErrMessage
  NEW TMGSAVEDIO SET TMGSAVEDIO=$IO
  NEW TMGDEVSAVE,TMGDEVINFO
  NEW USRINPUT,TERMCHAR,RAW,KEY,ESC
  NEW CMD SET CMD="1^OK"
  NEW STR SET STR=""
  NEW EOFF SET EOFF=+$GET(OPTION("EOFF"))
  DO DEV2ARR^TMGKERN1($IO,.TMGDEVSAVE,,.TMGDEVINFO)
  DO INITKB^XGF()  ;"set up keyboard input escape code processing
RK1 ;
  SET RAW=$$READKY^TMGUSRI5("e",,1,,.ESC,EOFF) IF RAW="" SET RAW=ESC
  SET KEY=RAW
  IF $LENGTH(KEY)>1 SET KEY="<"_KEY_">"
  SET CMD=$$EVENT(.OPTION,"ON KEY",3,KEY,.OPTION,.USERDATA) IF CMD<0 GOTO RKDN
  IF $$ISCUSORKEY(RAW) DO  GOTO RKDN:(CMD<0)
  . SET CMD=$$EVENT(.OPTION,"ON CURSOR",3,RAW,.OPTION,.USERDATA)
  . ;"NOTE: If cursor entered, should also continue below and terminate STR input. 
  IF $$IS1CHAR(KEY) DO  GOTO RK1
  . SET STR=STR_KEY
  ;"At this point, something has been done to stop string input  
  SET CMD=$$EVENT(.OPTION,"ON STRING",3,STR,.OPTION,.USERDATA) IF CMD<0 GOTO RKDN
  SET STR=""
  GOTO RK1 
RKDN ;"Cleanup
  DO RESETKB^XGF  ;"turn off XGF escape key processing code.
  IF $DATA(TMGDEVSAVE) DO   ;"turn IO back to what it was when coming into this function.
  . DO RESTORDEV^TMGKERN1(.TMGDEVSAVE,.TMGDEVINFO)
  ELSE  IF $DATA(TMGSAVEDIO) DO    ;"turn IO back to what it was when coming into this function.
  . USE TMGSAVEDIO
  QUIT CMD
  ;
IS1CHAR(KEY) ;" Is single char?  E.g. "b" and "^C" are 1 char. "HOME" is not
  NEW L SET L=$LENGTH(KEY)
  IF L=1 QUIT 1
  IF L=2,$EXTRACT(KEY,1)="^" QUIT 1
  QUIT 0
  ;
ISCUSORKEY(KEY) ;
  NEW RESULT SET RESULT=(";PREV;NEXT;UP;DOWN;RIGHT;LEFT;END;HOME;"[(";"_KEY_";"))
  QUIT RESULT
  ;
EVENT(OPTION,NAME,NUMPARAMS,PARAM1,PARAM2,PARAM3,PARAM4) ;"Fire event, if handler defined. 
  NEW RESULT SET RESULT=""
  NEW CODE SET CODE=$GET(OPTION(NAME)) IF CODE="" GOTO EVDN
  NEW PARAMS SET PARAMS=""
  SET NUMPARAMS=+$GET(NUMPARAMS)
  IF NUMPARAMS<0 SET NUMPARAMS=0
  IF NUMPARAMS>4 SET NUMPARAMS=4  
  NEW IDX FOR IDX=1:1:NUMPARAMS SET PARAMS=PARAMS_$SELECT(PARAMS="":"",1:",")_".PARAM"_IDX
  SET CODE="SET RESULT=$$"_CODE_"("_PARAMS_")"
  DO
  . NEW $ETRAP SET $ETRAP="WRITE ""(Invalid M Code!.  Error Trapped.) "",! SET $ETRAP="""",$ecode="""",RESULT=""^"""
  . XECUTE CODE
EVDN ;  
  QUIT RESULT
  ;
FXESCTBL  ;"FIX ESC TABLE
   ;"Purpose: There is a difference between my old system and the new.  I
   ;"         don't know why, but this will fix it for me, and anyone else.
   ;"//kt 6/9/22  Added F1 to F4.  Not sure why was missing before....
   ;"//kt 4/3/24  Added HOME and END key, not sure why not added before.  
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
   ;;$C(27)_"[11~")="F1"
   ;;$C(27)_"[12~")="F2"
   ;;$C(27)_"[13~")="F3"
   ;;$C(27)_"[14~")="F4"
   ;;$C(27)_"[15~")="F5"
   ;;$C(27)_"[17~")="F6"
   ;;$C(27)_"[18~")="F7"
   ;;$C(27)_"[19~")="F8"
   ;;$C(27)_"[1~")="HOME"   ;"note: PuTTY retruns this after HOME key pressed.  Was 'FIND'.  Changed 11/21/24  
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
   ;;$C(27)_"[4~")="END"      ;"note: PuTTY retruns this after END key pressed.  Was 'SELECT'.  Changed 11/21/24
   ;;$C(27)_"[5~")="PREV"
   ;;$C(27)_"[6~")="NEXT"
   ;;$C(27)_"[A")="UP"
   ;;$C(27)_"[B")="DOWN"
   ;;$C(27)_"[C")="RIGHT"
   ;;$C(27)_"[D")="LEFT"
   ;;$C(27)_"[F")="END"
   ;;$C(27)_"[H")="HOME"
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
