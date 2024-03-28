TMGIDE4 ;TMG/kst/A debugger/tracer for GT.M (Sender code) ;6/27/12, 2/2/14
         ;;1.0;TMG-LIB;**1**;03/23/09

 ;" TMG IDE Debugger Sender
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"------------------------------------------------------------
 ;"------------------------------------------------------------
 ;"Notes:    HOW DOES IT ALL WORK?  See TMGIDE3.m notes
 ;
Sender(Quiet) ;  
  ;"Purpose: This code will be run from process to be debugged.  It will
  ;"         be controlled by another Controlling process.
  ;"Input: Quiet : OPTIONAL.  If 1 then no TMGIDE extra output from this SENDER

  NEW TMGdbgResult,TMGdbgXLine
  SET Quiet=+$GET(Quiet)
  NEW MsgSndRef SET MsgSndRef=$name(^TMG("TMGIDE","SENDER"))
  NEW % SET %=2 ;"default NO
  IF $DATA(@MsgSndRef)'=0 do
  . IF Quiet SET %=2
  . ELSE  do
  . . WRITE "Is another debugging process already running"
  . . DO YN^DICN WRITE !
  . QUIT:(%'>0)  ;"abort
  . IF %=2 KILL @MsgSndRef QUIT
  . WRITE "OK to KILL debug info and start over"
  . SET %=1 DO YN^DICN WRITE !
  . IF %=1 KILL @MsgSndRef QUIT
  . SET %=-1
  IF (%'>0) GOTO SD2 ;"QUIT
  ; 
  IF 'Quiet WRITE "Waiting up to 60 sec for a CONTROLLER process..."
  IF $$MessageOut("INQ "_$J,60)="" GOTO SendDone
  IF 'Quiet WRITE " OK",!
  SET TMGdbgResult=$$MessageOut("WRITE Welcome to the TMG debugging environment",,0)
  SET TMGdbgResult=$$MessageOut("WRITE Enter any valid M command...",,0)
SendL1  ;
  IF 'Quiet WRITE !,!,"=== TMG IDE Sender (Job# ",$J,") ===",!,!
  IF 'Quiet WRITE "Waiting for command from Controller window... (^ to abort)"
SendL2  ;
  SET TMGdbgXLine=$$MessageOut("DO PROMPT",9999,0)
  IF TMGdbgXLine="" GOTO SendL2
  IF TMGdbgXLine="^" GOTO SendDone
  ;
  IF 'Quiet WRITE !
  SET TMGRunMode=1  ;"1=Step-by-step mode
  SET $ZSTEP="N tmgTrap S tmgTrap=$$STEPTRAP^TMGIDE4($ZPOS) ZSTEP:(tmgTrap=1) into ZSTEP:(tmgTrap=2) over ZSTEP:(tmgTrap=3) outof zcontinue"
  ;
  ZSTEP into
  xecute TMGdbgXLine ;"<-- NOTE: step *INTO* this line.  Shouldn't return from this until final QUIT of that process
  SET $ZSTEP=""  ;"turn off step capture
  GOTO SendL1
  ;
SendDone  ;
  IF 'Quiet WRITE !,"Sending DONE.."
  NEW TMGtemp SET TMGtemp=$$MessageOut("DONE",1)
  IF 'Quiet WRITE TMGtemp,!
  KILL ^TMG("TMGIDE","SENDER")
SD2  ;
  QUIT
  ;
HndlCmd(Msg)  ;
  ;"Purpose: When the user enters a command from the prompt in the controlling process, then that command will be
  ;"         forwarded here.
  NEW Cmd,result
  SET result=""
  SET Cmd=$PIECE(Msg," ",2)
  SET Msg=$PIECE(Msg," ",3,999)
  IF Cmd="BKPOS" SET result=$$HndlBkPos(Msg) GOTO HCDone
  IF Cmd="RELBKPOS" SET result=$$HndlRelBkPos(Msg) GOTO HCDone
  IF Cmd="EVAL" SET result=$$HndlEval(Msg) GOTO HCDone
  IF Cmd="XECUTE" SET result=$$HndlXCod(Msg) GOTO HCDone
  IF Cmd="TABLE" SET result=$$HndlTable(Msg) GOTO HCDone
  IF Cmd="DONE" SET result="OK",$ZSTEP="" GOTO HCDone  ;"turn off debugger
  ;
HCDone  ;
  QUIT "[RSLT] "_result
  ;
HndlEval(Msg)  ;
  ;"Purpose: to evaluate a local variable and pass result back to remote controller
  NEW varName SET varName=Msg
  NEW result SET result=""
  NEW ref SET ref=$name(^TMG("TMGIDE","CONTROLLER","MSG-IN","VAR"))
  KILL @ref
  IF varName["$" do
  . NEW tempCode,$ETRAP,tempValue
  . SET $ETRAP="set $ETRAP="""",$ecode="""""
  . SET tempcode="set tempValue="_varName
  . xecute tempCode
  . MERGE @ref=tempValue
  ELSE  IF varName'="" do
  . NEW tempCode,$ETRAP,tempValue
  . SET $ETRAP="set $ETRAP="""",$ecode="""""
  . SET varName=$$CREF^DILF(varName) ;" convert open to closed format
  . MERGE ^TMG("TMGIDE","CONTROLLER","MSG-IN","VAR")=@varName
  SET result=ref
  ;
  QUIT result
  ;
HndlTable(Msg)  ;
  ;"Purpose: to copy symbol table to a global, so controller can display.
  NEW ref SET ref=$name(^TMG("TMGIDE","CONTROLLER","MSG-IN","VAR"))
  KILL @ref
  ZSHOW "*":@ref
  QUIT ref
  ;
HndlBkPos(Msg)  ;
  ;"Purpose: To SET a breakpoint in running code, as specified by remote controller.
  ;"Input Msg: Format:  '<BreakPointPosition> <Condition>'  (Condition is optional)
  ;
  ;"WRITE "Here in HndlBkPos^TMGIDE4.  Msg=",Msg,!
  NEW result SET result=0
  NEW pos SET pos=$PIECE(Msg," ",1)
  IF pos="" GOTO HBPD
  NEW condition SET condition=$PIECE(Msg," ",2)
  NEW brkLine SET brkLine=pos_":""n tmg SET tmg=$$STEPTRAP^TMGIDE4($ZPOS,1)"""
  ;"WRITE "About to SET ZBREAK code: [",brkLine,"]",!
  DO
  . NEW $ETRAP SET $ETRAP="SET $ETRAP="""",$ECODE="""""
  . ZBREAK @brkLine
  . SET result=1
HBPD ;
  QUIT result
  ;
HndlRelBkPos(msg) ;
  ;"Purpose: release a breakpoint.
  ;"Input Msg: Format:  '<BreakPointPosition>'
  NEW result SET result=0
  NEW pos SET pos=$PIECE(Msg," ",1)
  IF pos'="" do
  . NEW brkLine SET brkLine=pos_":""zcontinue"""
  . ZBREAK @brkLine
  . SET result=1
  QUIT result
  ;
HndlXCod(MCode)  ;
  ;"Purpose: To excute code in this proccess, based on request from controlling process
  ;"Result: 1 IF error, 0 IF OK
  NEW result SET result=1 ;"default to error
  DO
  . NEW $ETRAP SET $ETRAP="write ""(Invalid M Code!.  Error Trapped.)"",! SET $ETRAP="""",$ecode="""""
  . xecute MCode
  . SET result=0
  QUIT result
  ;
  ;"------------------------------------------------------------
  ;"------------------------------------------------------------
STEPTRAP(idePos,tmgMsg)  ;
  ;"Purpose: This is the line that is called by GT.M for each ZSTEP event.
  ;"      It will be used to display the current code execution point, and
  ;"      query user as to plans for future execution: run/step/ etc.
  ;"Input: idePos -- a text line containing position, as returned bye $ZPOS
  ;"        tmgMsg -- OPTIONAL -- can be used by programs to pass in info.
  ;"                  If tmgMsg=1, then this function was called without the
  ;"                  $ZSTEP value set, so this function should SET it.
  ;"Result: 1=further execution should be via ZSTEP INTO
  ;"        2=further execution should be via ZSTEP OVER
  ;"        (Anything else) -->further execution should be via ZCONTINUE
  ;
  NEW TMGdbgResult,TMGdbgMsg
  SET tmgMsg="DO TRAP "_idePos_" "_$GET(tmgMsg)
STP2 ;
  SET TMGdbgResult=$$MessageOut(tmgMsg,9999,0)
  ;
  ;"Check IF message reply which is actually a request for more info
  IF $PIECE(TMGdbgResult," ",1)="[CMD]" DO  GOTO STP2
  . NEW temp SET temp=$$HndlCmd(TMGdbgResult)
  . SET tmgMsg=temp
  ;
  QUIT TMGdbgResult
  ;
  ;"------------------------------------------------------------
  ;"------------------------------------------------------------
MessageOut(Msg,timeOutTime,ignoreReply)  ;
  ;"Purpose: to send message to Controller, and return the reply, or time out
  ;"Input: Msg --  the message to send
  ;"       timeOutTime -- OPTIONAL, default is 2 seconds
  ;"       ignoreReply -- OPTIONAL, default is 0
  ;"Output: the returned message, or "" IF timed out or no reply, or ignoreReply=1
  ;
  SET timeOutTime=$GET(timeOutTime,2)
  SET ignoreReply=$GET(ignoreReply,0)
  NEW result SET result=""
  SET ^TMG("TMGIDE","CONTROLLER","MSG-OUT")=""  ;"clear any old message
  SET ^TMG("TMGIDE","CONTROLLER","MSG-IN")=Msg  ;"DON'T DELETE THIS LINE
  IF (ignoreReply=0) FOR  DO  QUIT:(result'="")!(timeOutTime<0)
  . SET result=$GET(^TMG("TMGIDE","CONTROLLER","MSG-OUT"))
  . IF (result'="") QUIT
  . SET timeOutTime=timeOutTime-0.1
  . SET ^TMG("TMGIDE","CONTROLLER","MSG-IN")=Msg
  . HANG 0.1
  . IF $$USRABORT^TMGUSRI2("from MessageOut^TMGIDE4") SET timeOutTime=-1,result="^"
  ;
  IF timeOutTime<0 do
  . NEW tempResult SET tempResult=$$KEYPRESD^TMGUSRI2(1,1)
  . IF tempResult="^" SET result="^"
  ;
  QUIT result
  ;