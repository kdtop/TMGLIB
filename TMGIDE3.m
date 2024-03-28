TMGIDE3 ;TMG/kst/A debugger/tracer for GT.M (Controller code) ;9/9/12, 2/2/14
         ;;1.0;TMG-LIB;**1**;03/23/09
  ;" TMG IDE Debugger Controller
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
  ;"Notes:    HOW DOES IT ALL WORK?
  ;"
  ;"Here is how the system fits together:
  ;"
  ;"Below is what happens in the CONTROLLING job
  ;"=================================================
  ;" -- LaunchIntr^TMGIDE5(JobNum) sets up a signaling global and then
  ;"                     creates a 'mupip intrpt JobNum', then starts listening
  ;"                     in Controller^TMGIDE3 for communication from the interrupted job.
  ;"                     (See below about how this communication gets started)
  ;" -- Controller^TMGIDE3 polls a communicating global location and can talk back
  ;"                     and forth with the other job.  When it gets a message to
  ;"                     DO TRAP %ZPOS, it then calls STEPTRAP^TMGIDE2, and returns
  ;"                     the result of that function back to the other job.
  ;" -- STEPTRAP^TMGIDE2 is the same interface as the prior debugger.  It shows
  ;"                     the code, allows the user to move around, and interact
  ;"                     with the code.  If the user wants to query variables
  ;"                     in the other process, then a message is sent out, and
  ;"                     a copy of that variable is passed back here for display.
  ;"                     If the user wants to modify the other environment, then
  ;"                     arbitrary M code can be entered by the user, and it is passed
  ;"                     to the other job for execution in that job process space.
  ;"                     When ready to execute the next line of code, then STEPTRAP
  ;"                     QUITs with a result signalling a zstep INTO or OVER.
  ;"
  ;"Below is what happens in the OTHER job
  ;"=================================================
  ;" -- mupip intrpt JobNum --> causes the specified job to execute the code
  ;"                     stored in $ZINTERRUPT.  For VistA (or IF setup in
  ;"                     in an environmental script during GT.M launch), this
  ;"                     code is to run JOBEXAM^ZU (slightly customized)
  ;" -- JOBEXAM^ZU --> looks for signaling global, and IF found runs INTERUPT^TMGIDE5
  ;" -- INTERUPT^TMGIDE5 --> sets up $ZSTEP and then calls ZSTEP INTO and
  ;"                     QUITs out of the $ZINTERRUPT code.
  ;" -- ZSTEP --> causes GT.M to execute the code in $ZSTEP before performing
  ;"                     the next line of mumps code for the program that was
  ;"                     running at the time the interrupt request was received.
  ;" -- $ZSTEP holds instruction to run $$STEPTRAP^TMGIDE4($ZPOS)
  ;" -- STEPTRAP^TMGIDE4 --> sends message to CONTROLLING JOB and waits for reply.
  ;"                     Reply will either be a request from the user for more
  ;"                     information from this job, or a final reply that allows
  ;"                     execution to continue, either by a step INTO, OVER, or
  ;"                     a plain ZCONTINUE (which will stop further code-stepping)
  ;
Controller  ;
  ;"Purpose: This code will wait for messages from the executing process, and
  ;"         will display the code as it changes, and send messages back to
  ;"         all the user to control the process remotely.

  ;"Notice: There are

  ;"A globally-scoped var that will be checked in STEPTRAP^TMGIDE2
  IF +$GET(tmgDbgRemoteJob)'>0 SET tmgDbgRemoteJob=1

  ;"WRITE #
  NEW i for i=1:1:12 WRITE !
  WRITE "=== TMG IDE Controller (Job# "_$JOB_") ===",!,!
  WRITE "Waiting for action from SENDING (Remote) process (ESC to abort)",!
  NEW msgRef SET msgRef=$name(^TMG("TMGIDE","CONTROLLER"))
  KILL @msgRef
  SET @msgRef@("JOB")=$JOB
  ;
  NEW Msg,UsrInput,Cmd
  NEW hangDelay SET hangDelay=0.2
  ;
  NEW tmgDbgBlankLine SET $PIECE(tmgDbgBlankLine," ",78)=" "
  ;"new HxSize SET HxSize=8     ;"hard codes in history length of 8
  NEW tmgDbgLine
  NEW tmgLastLine SET tmgLastLine=""
  NEW tmgHxShowNum SET tmgHxShowNum=0
  NEW HxLine,HxLineMax,HxLineCur
  DO INITKB^XGF()  ;"set up keyboard input escape code processing
  ;
Init  ;
  SET @msgRef@("STATUS")="AVAIL"
  SET @msgRef@("MSG-OUT")=""
  NEW TMGstartH SET TMGstartH=$PIECE($H,",",2)
  NEW tempCh,%
  ;
Loop  ;
  SET Msg=$GET(@msgRef@("MSG-IN"))
  SET Cmd=$PIECE(Msg," ",1)
  ;
  IF Cmd="INQ" DO HndlINQ(Msg) GOTO Loop
  IF Cmd="LISTEN" DO HndlListen(Msg) GOTO Loop
  IF Cmd="DONE" DO HndlDone(Msg) GOTO LstnDone  ;"This is when SENDER signals a QUIT.
  IF Cmd="WRITE" DO HndlWrite(Msg) GOTO Loop
  IF Cmd="DO" GOTO:($$HndlDo(Msg)'=0) Loop GOTO LstnDone ;"Leave IF CONTROLLER signals a QUIT
  IF Cmd="READ" DO HndlRead(Msg) GOTO Loop
  IF Cmd="NEED" DO HndlNeed(Msg) GOTO Loop
  ;
  ;"Checking UserAborted grabs keystrokes, and prevents user from getting out of RUN mode
  ;"in ^TMGIDE2, so only check here after an X second delay.
  IF $PIECE($H,",",2)-TMGstartH<2 GOTO Loop
  read *tempCh:0
  IF tempCh'=27 GOTO Loop
  WRITE !,"Abort From Remote Debugging Controller"
  SET TMGstartH=$PIECE($H,",",2)
  SET %=2 DO YN^DICN WRITE !
  IF %'=1 GOTO Loop
  ;
LstnDone ;
  WRITE !,"Quitting.",!
  KILL @msgRef
  KILL tmgDbgRemoteJob
  QUIT
  ;
  ;"-------------------------------
  ;"-------------------------------
  ;
ACK ;
  SET @msgRef@("MSG-OUT")="ACK "_$J
  SET @msgRef@("MSG-IN")=""
  QUIT
  ;
HndlINQ(Msg)  ;"Expects 'INQ <Job#>'
  ;"WRITE "Msg=",Msg,!  ;"temp!!
  SET tmgDbgRemoteJob=+$PIECE(Msg," ",2)
  DO ACK
  QUIT
  ;
HndlListen(Msg)  ;
  NEW JobToControl
  SET JobToControl=+$PIECE(Msg," ",3)
  SET @msgRef@("STATUS")="LISTENING TO "_JobToControl
  SET @msgRef@("MSG-OUT")=@msgRef@("STATUS")
  DO ACK
  QUIT
  ;
HndlWrite(Msg)  ;
  WRITE $PIECE(Msg," ",2,99),!
  DO ACK
  QUIT
  ;
HndlDo(Msg)  ;
  ;"Purpose: Handle message from interrupted application to DO something.
  ;"Result: 1 = OK to continue
  ;"        0 = should QUIT controller.
  NEW result SET result=1  ;"default to continue
  NEW msgResult SET msgResult=""
  IF $PIECE(Msg," ",2)="PROMPT" do
  . SET msgResult=$$Prompt()
  ELSE  IF $PIECE(Msg," ",2)="TRAP" do
  . NEW idePos SET idePos=$PIECE(Msg," ",3)
  . NEW tmgMsg SET tmgMsg=$PIECE(Msg," ",4)
  . SET msgResult=$$STEPTRAP^TMGIDE2(idePos,tmgMsg)
  . IF msgResult=0 SET result=0  ;"STEPTRAP result of 0 means to stop controller.
  SET @msgRef@("MSG-OUT")=msgResult
  SET @msgRef@("MSG-IN")=""
  SET TMGstartH=$PIECE($H,",",2) ;"restart timer countdown before allowing user input.
  QUIT result
  ;
HndlDone(Msg)  ;
  DO ACK
  QUIT
  ;
HndlRead(Msg)  ;
  NEW result
  WRITE $PIECE(Msg," ",2,99)
  read result:$GET(DTIME,3600),!
  IF result="" SET result="<null>"
  SET @msgRef@("MSG-OUT")=result
  SET @msgRef@("MSG-IN")=""
  QUIT
  ;
HndlNeed(Msg)  ;
  SET SndJob=+$PIECE(Msg," ",3)
  SET @MsgRef@("STATUS")="CONTROLLING "_SndJob
  SET @MsgRef@("MSG-OUT")=@MsgCtrlRef@("STATUS")
  QUIT
  ;
  ;"-------------------------------------------------------------------
  ;
Prompt()  ;
  ;"Purpose: to interact with user and run through code.
  WRITE "=== TMG IDE Controller ===",!,!
Ppt2  ;
  SET tmgHxShowNum=+$GET(tmgHxShowNum)
  SET TMGStepMode="into"  ;"kt added 5/3/06
  SET HxLine=$GET(^TMG("TMGIDE","CMD HISTORY",$J,tmgHxShowNum))
  SET HxLineMax=$GET(^TMG("TMGIDE","CMD HISTORY",$J,"MAX"),0)
  ;
  WRITE "Remote command (^ to QUIT): "
  IF tmgHxShowNum=0 WRITE "^// "
  ELSE  WRITE "// ",HxLine
  ;
  SET tmgDbgLine=$$READ^TMGIDE()  ;"$$READ^XGF  ;"returns line terminator in tmgXGRT
  SET tmgXGRT=$GET(tmgXGRT) ;"ensure existence
  IF tmgDbgLine="?" DO  GOTO Ppt2
  . WRITE !,"Here you should enter any valid M command, as would normally",!
  . WRITE "entered at a GTM> prompt.",!
  . WRITE "  examples:  WRITE ""HELLO"",!  or DO ^TMGTEST",!
  ;
  IF (tmgDbgLine="")&(tmgHxShowNum>0) SET tmgDbgLine=HxLine
  ;
  IF (tmgXGRT="DOWN")!(tmgXGRT="RIGHT")!(tmgDbgLine="]") DO  GOTO Ppt2
  . SET tmgHxShowNum=tmgHxShowNum-1
  . IF tmgHxShowNum<0 SET tmgHxShowNum=HxLineMax
  . ;"WRITE "setting tmgHxShowNum=",tmgHxShowNum,!
  . DO CHA^TMGTERM(1) WRITE tmgDbgBlankLine DO CHA^TMGTERM(1)
  ;
  IF (tmgXGRT="UP")!(tmgXGRT="LEFT")!(tmgDbgLine="[") DO  GOTO Ppt2
  . SET tmgHxShowNum=tmgHxShowNum+1
  . IF tmgHxShowNum>HxLineMax SET tmgHxShowNum=0
  . ;"WRITE "setting tmgHxShowNum=",tmgHxShowNum,!
  . DO CHA^TMGTERM(1) WRITE tmgDbgBlankLine DO CHA^TMGTERM(1)
  ;
  IF tmgDbgLine="" SET tmgDbgLine="^"
  WRITE !
  ;
  ;"Save Cmd history
  SET HxLineCur=$GET(^TMG("TMGIDE","CMD HISTORY",$J,"CUR"),0)  ;"<-- points to last used, not next avail
  SET HxLineMax=$GET(^TMG("TMGIDE","CMD HISTORY",$J,"MAX"),0) ;"equals buffer size AFTER it fills
  SET HxLineCur=HxLineCur+1
  ;"if HxLineCur>HxSize SET HxLineCur=1
  SET ^TMG("TMGIDE","CMD HISTORY",$J,HxLineCur)=tmgDbgLine
  SET ^TMG("TMGIDE","CMD HISTORY",$J,"CUR")=HxLineCur
  IF HxLineCur>HxLineMax do
  . SET HxLineMax=HxLineCur
  . SET ^TMG("TMGIDE","CMD HISTORY",$J,"MAX")=HxLineMax
  ;"WRITE "Saving line in #",HxLineCur," Max=",HxLineMax,!
  ;
  QUIT tmgDbgLine
  ;