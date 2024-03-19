TMGIDE2 ;TMG/kst/A debugger/tracer for GT.M (core functionality) ;12/17/14, 6/8/16
         ;;1.0;TMG-LIB;**1**;03/23/09
 ;" GT.M  TRAP STEP
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
 ;"=======================================================================
 ;" This code module will allow tracing through code.
 ;" It is used as follows:
 ;"
 ;" SET $ZSTEP="do STEPTRAP^TMGIDE2($ZPOS) ZSTEP into zcontinue"
 ;" ZSTEP into
 ;" DO ^MyFunction   ;"<--- put the function you want to trace here
 ;"
 ;" SET $ZSTEP=""  ;"<---turn off step capture
 ;" QUIT
 ;"
 ;"Notes:
 ;"  This function will be called in between lines of the main
 ;"  program that is being traced.  Thus this function takes care
 ;"  to not DO anything that might change the environment of the
 ;"  main program.
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"STEPTRAP(tmgIDEPos,tmgMsg)
 ;"ErrTrap(tmgIDEPos)
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"CmdPrompt -- Display the command prompt, and handle user input
 ;"BlankLine -- blank out a line
 ;"ClrLine --clear out line
 ;"TranslateKeys(tmgAction,tmgXGRT) -- translate input keys into a standard output.
 ;"MoveKey(tmgAction) -- Handle movement keys
 ;"EvalWatches -- Run code that evaluates watches.
 ;"HndlMCode -- Handle option to execute arbitrary code.
 ;"HndlShow -- Handle option to show a variable.
 ;"HndlToggleMode(Mode) -- Toggle UCASE or LCASE in Options
 ;"HndlWatch(tmgAction) --Handle option to add watch
 ;"HndlQuit --To create a crash, so can QUIT debugger,
 ;"HndlDone -- To turn off the debugger, allowing program to continue full speed.
 ;"HndlScrWH --Handle option to SET screen width and height
 ;"HndlExpand -- handle option to expand one mumps like of code.
 ;"HndlStack(ShowPos,tmgViewOffset) --Handle option to show and interact with stack.
 ;"HndlNodes -- Handle option to browse a variable by nodes.
 ;"HndlBrowse -- Handle option to browse a variable.
 ;"HndlBrkCond --Handle option to browse conditional break
 ;"HndlCstBrk --Set a custom breakpoint
 ;"HndlSetBrk -- Set breakpoint at current point
 ;"HndlTable -- Handle option for Table command
 ;"HndlJmpDisp(ShowPos,tmgViewOffset) -- allow user to enter in a location to show in code displayer
 ;"HndlHelp --Handle option for help.
 ;"HlpWrite(line) ;
 ;"ShowCode() --  display code at the top of the screen
 ;"SetTempBkColor(mode)
 ;"SetColors(mode) -- SET colors in central location
 ;"Box    --Draw a box on the top of the screen.
 ;"GetStackInfo(Stack,ExecPos) -- query GTM and get back filtered Stack information
 ;"ToggleBreakpoint(pos,condition) -- SET or release the GT.M breakpoint at position
 ;"IsBreakpoint(pos) -- determine IF position is a breakpoint pos
 ;"EnsureBreakpoints() -- Ensure breakpoints after module recompiled.
 ;"SetBreakpoint(pos,condition) -- SET the GT.M breakpoint to pos position
 ;"SetBrkCond(pos,condition) -- A standardized SET for condition.
 ;"GetBrkCond(pos) -- A standardized GET for condition.
 ;"RelBreakpoint(pos) -- release a  GT.M breakpoint at position
 ;"ShouldSkip(module) -- see IF module is in hidden list
 ;"SetupSkips --  manage modules that are to be skipped over.
 ;"AddSkip -- allow user to Add a module to hidden list
 ;"RmSkip -- allow user to remove a module from hidden list
 ;"ShowSkip -- show the hidden list
 ;"MessageOut() -- when in remote-control debugging mode, send a message to SENDER
 ;"GetRemoteVar(varName) -- Pass varName to remote process, have it evaluated there, and passed back back here for display.
 ;"RemoteXecute(MCode) -- Pass M Code to remote process for execution there.
 ;
 ;"=======================================================================
 ;" Dependencies:
 ;"   Uses: ^TMGTERM,^TMGIDE
 ;"=======================================================================
 ;
STEPTRAP(tmgIDEPos,tmgMsg)
  ;"Purpose: This is the line that is called by GT.M for each ZSTEP event.
  ;"      It will be used to display the current code execution point, and
  ;"      query user as to plans for future execution: run/step/ etc.
  ;"Input: tmgIDEPos -- a text line containing position, as returned bye $ZPOS
  ;"       tmgMsg -- OPTIONAL -- can be used by programs to pass in info.
  ;"                  If tmgMsg=1, then this function was called without the
  ;"                  $ZSTEP value set, so this function should SET it.
  ;"Global-scoped vars used:
  ;"          tmgDbgRemoteJob = remote $J IF controlling a remote process
  ;"                          Won't exist (or will be 0) otherwise.
  ;"          tmgRunMode --
  ;"          tmgStepMode --
  ;"          tmgScrHeight --
  ;"          tmgScrWidth --
  ;"          tmgLROffset --
  ;"          tmgDbgHideList (an array REF) -- holds modules to hide
  ;"Result: desired mode for next time:
  ;"        1=step into
  ;"        2=step over
  ;"        3-step out of
  ;"        (anything else) -- stop debugging.  <-- I think...
  ;"        0-->signals request to stop when remote debugging.
  ;
  ;"tmgRunMode: 0=running mode      (Globally-scoped var)
  ;"            1=stepping mode
  ;"            2=Don't show code
  ;"            3=running SLOW mode
  ;"            4=Hopping (rapidly jumping between breakpoints)
  ;"            5=Data watch (monitor data watches, don't show code)
  ;"           -1=QUIT
  ;
  NEW InitZSTEPOff set InitZSTEPOff=($GET(tmgMsg)=1)
  NEW tmgdbgTruth SET tmgdbgTruth=$TEST   ;"save initial value of $TEST
  IF $DATA(tmgDbgJumpToBrkPos) DO  
  . DO RelBreakpoint(tmgDbgJumpToBrkPos)
  . KILL tmgDbgJumpToBrkPos
  IF $ZTRAP'["^TMG" DO SetErrTrap^TMGIDE  ;"ensure no redirecting of error trap
  NEW tmgDbgResult SET tmgDbgResult=1  ;"1=step into, 2=step over
  NEW tmgDbgNakedRef SET tmgDbgNakedRef=$$LGR^TMGIDE ;"save naked reference
  SET tmgDbgHangTime=+$GET(tmgDbgHangTime,0.25)
  ;
  IF $GET(tmgRunMode)="" DO  ;"Happens if code clears variable table, e.g. ^XUP
  . SET tmgRunMode=$GET(^TMG("TMGIDE",$J,"RUNMODE"),1) ;"//kt 1/7/15
  IF ("1234"[tmgRunMode)&(+$GET(tmgDbgOptions("VARTRACE"))=1) DO
  . DO RecordVTrace^TMGIDE6  ;"Keep track of changes to variable system table
  SET tmgStepMode=$GET(tmgStepMode,"into")
  SET tmgDbgRemoteJob=+$GET(tmgDbgRemoteJob)
  NEW tmgDbgJNum SET tmgDbgJNum=$J
  IF tmgDbgRemoteJob SET tmgDbgJNum=tmgDbgRemoteJob
  NEW tmgArrayName SET tmgArrayName=$name(^TMG("TMGIDE",tmgDbgJNum,"MODULES"))
  NEW %tmg SET %tmg=$GET(%)
  NEW tmgBlankLine,tmgAction,tmgKeyIn,tmgTempI,tmgDone
  NEW tmgViewOffset SET tmgViewOffset=0
  NEW tmgSavedIO,tmgSavedX,tmgSavedY,tmgDEVInfo,tmgDEVSav
  SET tmgSavedIO=$IO
  DO DEV2ARR^TMGKERN1($IO,.tmgDEVSav,.tmgDEVInfo)
  SET tmgSavedX=$X,tmgSavedY=$Y
  SET tmgScrHeight=$GET(tmgScrHeight,10)
  SET tmgScrWidth=+$GET(tmgScrWidth)
  IF (tmgScrWidth'>0)!(tmgRunMode=1) DO  ;"If pause after every show, take time to check dimensions.
  . DO RESETKB^XGF  ;"turn off XGF escape key processing code.
  . NEW tmpScrWidth IF $$GETSCRSZ^TMGKERNL(,.tmpScrWidth)  ;"drop function result
  . DO INITKB^XGF()  ;"set up keyboard input escape code processing
  . ;"//kt 6/8/16 IF ($GET(tmgScrWidth("USER SET"))=1)&(tmgScrWidth'>tmpScrWidth) QUIT 
  . IF ($GET(tmgScrWidth("USER SET"))=1) QUIT 
  . SET tmgScrWidth=tmpScrWidth
  ;"NEW LROFFSET SET LROffset=$GET(tmgLROffset,0)
  SET tmgLROffset=$GET(tmgLROffset,0)
  IF tmgRunMode'=5 GOTO SP2
  NEW tmgTEMPEVAL SET tmgTEMPEVAL=$$EVALDW^TMGIDE7(.tmgRunMode)
  IF tmgTEMPEVAL=1 GOTO SPDone
SP2 ;
  USE $P:(WIDTH=tmgScrWidth:NOWRAP)  ;"reset IO to the screen
  SET tmgBlankLine=" "
  FOR tmgTempI=1:1:tmgScrWidth-1 SET tmgBlankLine=tmgBlankLine_" "
  NEW tmgRelPos SET tmgRelPos=tmgIDEPos
  NEW tmgOrigIDEPos SET tmgOrigIDEPos=tmgIDEPos
  NEW tmgTempPos SET tmgTempPos=$$ConvertPos^TMGIDE(tmgIDEPos,tmgArrayName)
  IF tmgTempPos'="" SET tmgIDEPos=tmgTempPos
  ;"Don't show hidden modules (setup in TMGIDE module)
  IF $$ShouldSkip($PIECE(tmgIDEPos,"^",2)) GOTO SPDone
  ;"Record trace, If not a hidden module
  IF +$GET(tmgDbgOptions("TRACE"))=1 DO RecordTrace^TMGIDE6(tmgOrigIDEPos)
  ;
  ;"Note: Conditional Breakpoints: I will have to try to get this working later.
  ;"I have it such that the condition is recognized.  But now I need to
  ;"Differientate between stepping through code, and a breakpoint from
  ;"a full speed run.
  ;"Part of the problem is recognizing the breakpoint position.  Sometimes
  ;"   tmgIDEPos (derived from $ZPOS) is different than the address stored
  ;"   for breakpoints (or viewable via ZSHOW "B").  See EquivalentBreakpoint(pos1, pos2) 
  ;"   which needs to be implemented.
  ;"UPDATE: Tenatively working, as of 7/10/22, if not problems delete all these comments later....
  NEW tmgStpSkip SET tmgStpSkip=0
  NEW zzAtBkPt set zzAtBkPt=$$IsBreakpointFlex() 
  ;"IF zzAtBkPt,InitZSTEPOff DO  GOTO:(tmgStpSkip=1) SPDone
  IF zzAtBkPt DO  GOTO:(tmgStpSkip=1) SPDone
  . ;"WRITE !,"AT BREAKPOINT",!
  . NEW ifS SET ifS=$$GetBrkCondFlex() IF ifS="" QUIT
  . ;"WRITE !,"ifS=",ifS,!
  . NEW zzBkPtTest SET zzBkPtTest=$$EvalBkPtCode(ifS)
  . ;"WRITE "$$EvalBkPtCode(ifS)=",zzBkPtTest,!
  . IF zzBkPtTest=1 QUIT  ;"User Test code ==> $TEST=1, so let breakpoint stand, don't continue running full speed 
  . SET tmgStpSkip=1
  . SET tmgMsg=0  ;"ensure $ZSTEP is not turned back on, i.e. continue running.  
  . SET tmgStepMode="DONE"
  ;
  DO VCUSAV2^TMGTERM
  NEW tmgCsrOnBreakline SET tmgCsrOnBreakline=0
  IF tmgRunMode'=2 DO  ;"2=Don't show code
  . DO ShowCode(tmgIDEPos,tmgScrWidth,tmgScrHeight,,tmgViewOffset,tmgLROffset,.tmgCsrOnBreakline)
  ELSE  DO
  . DO CUP^TMGTERM(1,2)
  WRITE tmgBlankLine,!
  WRITE tmgBlankLine,!
  DO CUU^TMGTERM(2)
  IF tmgRunMode'=1 DO  ;"Not stepping mode
  . WRITE tmgBlankLine,!
  . DO CUU^TMGTERM(1)
  . DO EvalWatches
  . WRITE "(Press any key to pause"
  . IF "34"[tmgRunMode WRITE "; '+' for faster, '-' for slower)",!
  . ELSE  WRITE ")",!
  . READ *tmgKeyIn:0
  . IF "34"[tmgRunMode DO
  . . IF tmgKeyIn=43 SET tmgDbgHangTime=tmgDbgHangTime/2,tmgKeyIn=0       ;"43= '+'
  . . ELSE  IF tmgKeyIn=45 SET tmgDbgHangTime=tmgDbgHangTime*2,tmgKeyIn=0 ;"45= '-'
  . . HANG tmgDbgHangTime
  . IF (tmgKeyIn>0) DO
  . . ;"WRITE !,"KEY PRESSED",!
  . . DO SetRunMode(1)
  IF tmgRunMode'=2 DO  ;"2=Don't show code
  . DO CmdPrompt ;"display prompt and interact with user
  DO VCULOAD2^TMGTERM
  ;
SPDone ;"Finish up and return to GTM execution
  IF tmgStepMode="into" SET tmgDbgResult=1
  IF tmgStepMode="over" SET tmgDbgResult=2
  IF tmgStepMode="outof" SET tmgDbgResult=3
  ;
  IF $GET(tmgMsg)=1 DO  ;"call was without $ZSTEP set, so we should SET it.
  . NEW code SET code="N tmgTrap "
  . SET code=code_"S tmgTrap=$$STEPTRAP^TMGIDE2($ZPOS) "
  . SET code=code_"ZSTEP:(tmgTrap=1) into ZSTEP:(tmgTrap=2) over ZSTEP:(tmgTrap=3) outof "
  . SET code=code_"zcontinue"         
  . ;"SET $ZSTEP="N tmgTrap S tmgTrap=$$STEPTRAP^TMGIDE2($ZPOS) ZSTEP:(tmgTrap=1) into ZSTEP:(tmgTrap=2) over ZSTEP:(tmgTrap=3) outof zcontinue"
  . SET $ZSTEP=code
  . ZSTEP:(tmgDbgResult=1) into
  . ZSTEP:(tmgDbgResult=2) over
  . ZSTEP:(tmgDbgResult=3) outof
  ;
  ;"Restore environment
  IF $DATA(tmgDEVSav) DO   ;"turn IO back to what it was when coming into this function.
  . DO RESTORDEV^TMGKERN1(.tmgDEVSav,.tmgDEVInfo)
  ELSE  IF $DATA(tmgSavedIO) DO    ;"turn IO back to what it was when coming into this function.
  . USE tmgSavedIO
  SET $X=+$GET(tmgSavedX),$Y=+$GET(tmgSavedY)  ;"Restore screen POS variables.
  SET %=%tmg
  IF (tmgDbgNakedRef'["""""")&(tmgDbgNakedRef'="") DO   ;"If holds "" index, skip over
  . NEW discard SET discard=$GET(@tmgDbgNakedRef) ;"restore naked reference.
  IF tmgdbgTruth ;"This will restore initial value of $TEST
  ;
  QUIT tmgDbgResult
  ;
 ;"============================================================================
EvalBkPtCode(zzCode) ;
  NEW zzBkPtTest SET zzBkPtTest=0 
  DO
  . NEW $ETRAP set $ETRAP="write ""(Invalid M Code!.  Error Trapped.)"",! set $ETRAP="""",$ECODE="""""
  . XECUTE zzCode
  . SET zzBkPtTest=$TEST
  QUIT zzBkPtTest
  ; 
SetRunMode(Value,Option) ;"//kt 1/7/15
  IF ($GET(Option)=1),($GET(tmgRunMode)=4) QUIT  ;"When hopping, don't drop to step mode
  SET tmgRunMode=Value
  SET ^TMG("TMGIDE",$J,"RUNMODE")=tmgRunMode 
  QUIT
  ;
CmdPrompt  ;
  ;"Purpose: Display the command prompt, and handle user input
  ;"Note: uses some variables with global scope, because this code block
  ;"     was simply cut out of main routine above.
  ;"Result: None
  IF "41"'[tmgRunMode QUIT  ;"Only interact with user if in stepping mode (1)
  NEW $ETRAP SET $ETRAP="SET result="""",$ETRAP="""",$ecode="""""
  NEW tmgDone SET tmgDone=0
  FOR  DO  QUIT:tmgDone=1
  . DO ShowCode(tmgIDEPos,tmgScrWidth,tmgScrHeight,,tmgViewOffset,tmgLROffset,.tmgCsrOnBreakline)
  . NEW tmgTempI FOR tmgTempI=1:1:2 WRITE tmgBlankLine,!  ;"create empty space below display.
  . DO CUU^TMGTERM(2)
  . IF tmgCsrOnBreakline=1 DO
  . . NEW ifS SET ifS=$$GetBrkCond($$RelConvertPos^TMGIDE(tmgRelPos,tmgViewOffset,tmgArrayName))
  . . IF ifS="" quit
  . . NEW zzBkPtTest SET zzBkPtTest=$$EvalBkPtCode(ifS) 
  . . WRITE "Breakpoint test: [",ifS,"] --> [",zzBkPtTest,"]",!
  . WRITE "}"
  . DO EvalWatches
  . SET $X=1
  . WRITE "Action (? for help): "
  . WRITE "step "_$$UP^TMGIDE(tmgStepMode)_"// "
  . DO ClrLine
  . IF tmgRunMode=4 set tmgAction="X" 
  . ELSE  SET tmgAction=$$READ^TMGIDE() WRITE !
  . IF tmgAction="" SET tmgAction=$$UP^TMGIDE($EXTRACT(tmgStepMode,1,1))
  . NEW tmgOrigAction SET tmgOrigAction=tmgAction
  . DO TranslateKeys(.tmgAction,$GET(tmgXGRT))
  . SET tmgDone=("RLIHOXTQ"[tmgAction)
  . IF tmgAction="R" DO SetRunMode(0) QUIT         ;"Run Quickly
  . IF tmgAction="L" DO SetRunMode(3) QUIT         ;"Run slowly
  . IF tmgAction="H" DO SetRunMode(2) QUIT         ;"HIDE
  . IF tmgAction="HOP" DO SetRunMode(4) QUIT       ;"HOPPING
  . IF tmgAction="I" SET tmgStepMode="into" QUIT   ;"Step INTO
  . IF tmgAction="O" SET tmgStepMode="over" QUIT   ;"Step OVER
  . IF tmgAction="T" SET tmgStepMode="outof" QUIT  ;"Step OUTOF
  . IF tmgAction="X" DO HndlDone QUIT              ;"Turn off debugger (keep running)
  . IF tmgAction="Q" DO HndlQuit QUIT              ;"Quit from debugger (stop running)
  . IF tmgAction="M" DO HndlMCode QUIT             ;"Execute M code
  . IF tmgAction="B" DO HndlSetBrk QUIT            ;"Toggle a breakpoint at current location
  . IF tmgAction="J" DO HndlJmpBrk QUIT            ;"Toggle a Jump-to breakpoint at current location
  . IF tmgAction="E" DO HndlExpand QUIT            ;"Expand line
  . IF tmgAction="W" DO HndlWatch(tmgOrigAction) QUIT    ;"Watch
  . IF tmgAction="C" DO HndlCstBrk QUIT            ;"Custom breakpoint
  . IF tmgAction="G" DO HndlJmpDisp(.tmgIDEPos,.tmgViewOffset) QUIT  ;"Jump to NEW display location
  . IF tmgAction="BC" DO HndlBrkCond QUIT          ;"Enter a breakpoint condition (IF code)
  . IF $$MoveKey(tmgAction) QUIT
  . IF tmgAction="+" SET tmgScrWidth=$GET(tmgScrWidth)+1 QUIT
  . IF tmgAction="-" SET:(tmgScrWidth>10) tmgScrWidth=$GET(tmgScrWidth)-1 QUIT
  . IF tmgAction="=" DO HndlScrWH QUIT
  . IF tmgAction="CLS" WRITE # QUIT
  . IF tmgAction["TABLE" DO HndlTable QUIT
  . IF tmgAction["SHOW" DO HndlShow QUIT
  . IF tmgAction["ZWR" DO HndlZWR QUIT
  . IF tmgAction["BROWSE" DO HndlBrowse QUIT
  . IF tmgAction["NODES" DO HndlNodes QUIT
  . IF tmgAction["STACK" DO HndlStack(.tmgIDEPos,.tmgViewOffset) QUIT
  . IF tmgAction["RESYNC" KILL @tmgArrayName QUIT
  . IF tmgAction["HIDE" DO SetupSkips QUIT
  . IF tmgAction["FULL" DO FULL^VALM1,INITKB^XGF() QUIT
  . IF tmgAction["UCASE" DO HndlToggleMode("UCASE") QUIT
  . IF tmgAction["LCASE" DO HndlToggleMode("LCASE") QUIT
  . IF tmgAction["XCMD" DO HndlToggleMode("XCMD") QUIT
  . IF tmgAction["SCMD" DO HndlToggleMode("SCMD") QUIT
  . IF tmgAction["TRACE" DO ShowTrace^TMGIDE6 QUIT
  . IF tmgAction["TVDIFF" DO HndlToggleMode("VARTRACE") QUIT
  . IF tmgAction["VDIFF" DO ShowVTrace^TMGIDE6 QUIT
  . IF tmgAction["COLORS" DO EditColors^TMGIDE6 QUIT
  . IF tmgAction["DBK" DO DelBreaks^TMGIDE6 QUIT
  . IF tmgAction["INITKB" DO INITKB^XGF() QUIT  ;"set up keyboard input escape code processing
  . IF tmgAction["RDW" DO HndlRunDW SET tmgDone=1 QUIT
  . IF tmgAction["VARS" DO HndlVars(tmgOrigAction) SET tmgDone=1 QUIT
  . ELSE  DO HndlHelp QUIT
  QUIT
  ;
BlankLine ;
  WRITE tmgBlankLine
  DO CHA^TMGTERM(1) ;"move to x=1 on this line
  QUIT
  ;
ClrLine ;
  ;"Purpose: clear out line
  NEW loop
  NEW tempX SET tempX=$X
  FOR loop=1:1:20 WRITE " "
  FOR loop=1:1:20 WRITE $CHAR(8) ;"backspace
  SET $X=tempX
  QUIT
  ;
TranslateKeys(tmgAction,tmgXGRT)  ;
  ;"Purpose: translate input keys into a standard output.
  ;"Input: tmgAction -- PASS BY REFERENCE.
  SET tmgAction=$$UP^TMGIDE(tmgAction)
  SET tmgXGRT=$GET(tmgXGRT)
  IF tmgXGRT="UP" SET tmgAction="A"
  IF tmgXGRT="PREV" SET tmgAction="AA"
  IF tmgXGRT="DOWN" SET tmgAction="Z"
  IF tmgXGRT="NEXT" SET tmgAction="ZZ"
  IF tmgXGRT="RIGHT" SET tmgAction="]"
  IF tmgXGRT="LEFT" SET tmgAction="["
  IF (tmgAction="<AU>") SET tmgAction="<UP>"
  IF (tmgAction="A") SET tmgAction="<UP>"
  IF (tmgAction="AA") SET tmgAction="<PGUP>"
  IF (tmgAction="<AD>") SET tmgAction="<DN>"
  IF (tmgAction="Z") SET tmgAction="<DN>"
  IF (tmgAction="ZZ") SET tmgAction="<PGDN>"
  IF (tmgAction="<AL>") SET tmgAction="<LEFT>"
  IF (tmgAction="[") SET tmgAction="<LEFT>"
  IF (tmgAction="[[") SET tmgAction="<HOME>"
  IF (tmgAction="<AR>") SET tmgAction="<RIGHT>"
  IF (tmgAction="]") SET tmgAction="<RIGHT>"
  IF (tmgAction="]]") SET tmgAction="<END>"
  IF (tmgAction="^") SET tmgAction="Q"
  IF "wW"[$PIECE(tmgAction," ",1) SET tmgAction="W"
  QUIT
  ;
MoveKey(tmgAction)  ;
  ;"Purpose: Handle movement keys
  ;"result: 1 IF tmgAction is a movement key, 0 otherwise
  IF (tmgAction="<UP>") DO  QUIT 1
  . SET tmgViewOffset=tmgViewOffset-1
  IF (tmgAction="<DN>") DO  QUIT 1
  . SET tmgViewOffset=tmgViewOffset+1
  IF (tmgAction="<PGUP>") DO  QUIT 1
  . SET tmgViewOffset=tmgViewOffset-1
  . SET tmgViewOffset=tmgViewOffset-tmgScrHeight+2
  IF (tmgAction="<PGDN>") DO  QUIT 1
  . SET tmgViewOffset=tmgViewOffset+1
  . SET tmgViewOffset=tmgViewOffset+tmgScrHeight-2
  IF (tmgAction="<LEFT>") DO  QUIT 1
  . IF tmgLROffset>0 SET tmgLROffset=tmgLROffset-1
  IF (tmgAction="<HOME>") DO  QUIT 1
  . SET tmgLROffset=0
  IF tmgAction="<RIGHT>" DO  QUIT 1
  . IF tmgLROffset=0 SET tmgLROffset=1
  . SET tmgLROffset=tmgLROffset+1
  IF (tmgAction="<END>") DO  QUIT 1
  . IF tmgLROffset=0 SET tmgLROffset=1
  . SET tmgLROffset=tmgLROffset+20
  QUIT 0
  ;
EvalWatches  ;
  ;"Purpose: Run code that evaluates watches.
  IF $GET(tmgWatchLine)'="" DO
  . NEW $ETRAP SET $ETRAP="WRITE ""(Invalid M Code!.  Error Trapped.)"" SET $ETRAP="""",$ecode="""""
  . xecute tmgWatchLine
  IF $DATA(tmgDgbWatches("*")) DO ShowVTrace^TMGIDE6
  WRITE !
  QUIT
  ;
HndlMCode ;
  ;"Purpose: Handle option to execute arbitrary code.
  DO CUU^TMGTERM(1)
  DO CHA^TMGTERM(1) ;"move to x=1 on this line
  WRITE tmgBlankLine,!
  DO CUU^TMGTERM(1)
  NEW tmgTpLine
  SET tmgTpLine=$$Trim^TMGIDE($PIECE(tmgOrigAction," ",2,999))
  IF tmgTpLine="" READ " enter M code (^ to cancel): ",tmgTpLine:$GET(DTIME,3600),!
  IF (tmgTpLine'="^") DO
  . IF +$GET(tmgDbgRemoteJob) DO RemoteXecute(tmgTpLine) QUIT
  . NEW $ETRAP SET $ETRAP="WRITE ""(Invalid M Code!.  Error Trapped.)"",! SET $ETRAP="""",$ecode="""""
  . WRITE !  ;"get below bottom line for output.
  . xecute tmgTpLine
  QUIT
  ;
HndlShow ;
  ;"Purpose: Handle option to show a variable.
  DO Box
  DO SetColors("NORM")
  DO CUP^TMGTERM(1,2) ;"Cursor to line (1,2)
  ;"NEW varName SET varName=$$Trim^TMGSTUTL($EXTRACT(tmgOrigAction,5,999))
  NEW varName SET varName=$$Trim^TMGIDE($EXTRACT(tmgOrigAction,5,999))
  IF +$GET(tmgDbgRemoteJob) SET varName=$$GetRemoteVar(varName)
  WRITE !   ;"get below bottom line for output.
  NEW zbTemp SET zbTemp=0
  IF varName["$" DO
  . NEW tempCode
  . NEW $ETRAP SET $ETRAP="WRITE ""(Invalid M Code!.  Error Trapped.)"",! SET $ETRAP="""",$ecode="""""
  . WRITE varName,"='"
  . SET tempCode="do DEBUGWRITE^TMGIDE(1,"_varName_")"
  . xecute tempCode
  . WRITE "'    "
  ELSE  IF varName'="" DO
  . SET varName=$$CREF^TMGIDE(varName)  ;"convert open to closed format
  . SET zbTemp=$$ArrayDump^TMGIDE(varName)
  IF zbTemp=0 DO
  . DO SetColors("Highlight")
  . DO PRESS2GO^TMGUSRI2
  DO SetColors("Reset")
  QUIT
  ;
HndlZWR  ;
  ;"Purpose: Handle option to ZWRITE a variable.
  DO Box
  DO SetColors("NORM")
  DO CUP^TMGTERM(1,2) ;"Cursor to line (1,2)
  ;"NEW varName SET varName=$$Trim^TMGSTUTL($EXTRACT(tmgOrigAction,5,999))
  NEW varName SET varName=$$Trim^TMGIDE($EXTRACT(tmgOrigAction,5,999))
  IF +$GET(tmgDbgRemoteJob) SET varName=$$GetRemoteVar(varName)
  WRITE !   ;"get below bottom line for output.
  NEW zbTemp SET zbTemp=0
  IF varName["$" DO
  . NEW tempCode
  . NEW $ETRAP SET $ETRAP="WRITE ""(Invalid M Code!.  Error Trapped.)"",! SET $ETRAP="""",$ecode="""""
  . WRITE varName,"='"
  . SET tempCode="do DEBUGWRITE^TMGIDE(1,"_varName_")"
  . xecute tempCode
  . WRITE "'    "
  ELSE  IF varName'="" DO
  . SET varName=$$CREF^TMGIDE(varName)  ;"convert open to closed format
  . DO ZWRITE^TMGZWR(varName)  ;"ZWRITE @varName
  IF zbTemp=0 DO
  . DO SetColors("Highlight")
  . DO PRESS2GO^TMGUSRI2
  DO SetColors("Reset")
  QUIT
  ;
HndlToggleMode(tmgMode)  ;
  ;"Purpose: Toggle UCASE or LCASE in Options
  ;"This will effect the translation of all commands into forced Upper Case
  ;"or forced Lowercase, or leave as found IF both options are SET to 0
  QUIT:($GET(tmgMode)="")
  SET tmgDbgOptions(tmgMode)='+$GET(tmgDbgOptions(tmgMode))
  WRITE "Mode for "
  IF "UCASE,LCASE,XCMD,SCMD"[tmgMode DO
  . WRITE "forcing "
  . WRITE $SELECT(tmgMode="UCASE":"UPPER case",tmgMode="LCASE":"LOWER case",1:"")
  . WRITE $SELECT(tmgMode="XCMD":"expansion",tmgMode="SCMD":"shortening",1:"")
  . WRITE " of mumps command "
  IF "TRACE"[tmgMode DO
  . WRITE "recording TRACE of execution "
  WRITE "turned: "
  WRITE $SELECT(tmgDbgOptions(tmgMode)=0:"OFF",1:"ON"),"     ",!
  IF tmgDbgOptions(tmgMode)=1 DO
  . IF tmgMode="UCASE" SET tmgDbgOptions("LCASE")=0
  . IF tmgMode="LCASE" SET tmgDbgOptions("UCASE")=0
  . IF tmgMode="XCMD" SET tmgDbgOptions("SCMD")=0
  . IF tmgMode="SCMD" SET tmgDbgOptions("XCMD")=0
  ;"do PRESS2GO^TMGUSRI2
  QUIT
  ;
HndlWatch(tmgAction) ;
  ;"Purpose: Handle option to add watch
  DO CUU^TMGTERM(1)
  DO CHA^TMGTERM(1) ;"move to x=1 on this line
  WRITE tmgBlankLine,!
  DO CUU^TMGTERM(1)
  WRITE !,tmgAction ;"TEMP!
  IF (tmgAction["+")!(tmgAction["-") DO
  . NEW watchVar
  . IF (tmgAction["+") DO
  . . SET watchVar=$$Trim^TMGIDE($PIECE(tmgOrigAction,"+",2))
  . . IF watchVar="" QUIT
  . . IF watchVar="^" SET watchVar="tmgDbgNakedRef"
  . . SET tmgDgbWatches(watchVar)=""
  . . IF watchVar="*" WRITE "Watching variable CHANGES"
  . ELSE  IF (tmgAction["-") DO
  . . SET watchVar=$$Trim^TMGIDE($PIECE(tmgOrigAction,"-",2))
  . . IF watchVar="" QUIT
  . . IF watchVar="^" SET watchVar="tmgDbgNakedRef"
  . . KILL tmgDgbWatches(watchVar)
  . SET tmgWatchLine=""
  . NEW v SET v=""
  . FOR  SET v=$ORDER(tmgDgbWatches(v)) QUIT:(v="")  DO
  . . IF v="*" QUIT  ;" this signal for watching CHANGES handled elsewhere.
  . . IF $EXTRACT(v,1)="@" DO  
  . . . NEW TEMPV SET TEMPV=$$OREF^DILF($NAME(@v))_"*)"
  . . . ;"WRITE !,"TEMPV=",TEMPV,!
  . . . SET tmgWatchLine=tmgWatchLine_" WRITE """_v_" =["" ZWR:$D("_v_") "_TEMPV_" WRITE ""],"" "
  . . . ;"WRITE "tmgWatchLine=",tmgWatchLine,!
  . . ELSE  DO
  . . . SET tmgWatchLine=tmgWatchLine_" WRITE """_v_" =["",$GET("_v_"),""], """
  ELSE  DO
  . KILL tmgDgbWatches
  . NEW tempCode
  . READ "Enter M code (^ to cancel): ",tempCode:$GET(DTIME,3600),!
  . IF tempCode'="^" SET tmgWatchLine=tempCode
  QUIT
  ;
HndlHop  ;
  DO SetRunMode(4) 
  Do HndlDone
  DO PRESS2GO^TMGUSRI2
  Quit
  ;
HndlQuit  ;
  ;"Purpose: To create a crash, so can QUIT debugger, OR IF in Remote
  ;"    mode, then DO same thing as 'X' command
  IF +$GET(tmgDbgRemoteJob) GOTO HndlDone ;"QUIT will occur from there
  KILL @tmgArrayName
  SET $ETRAP=""  ;"remove error trap
  WRITE !!!!!!!!!!!
  WRITE "CREATING AN ARTIFICIAL ERROR TO STOP EXECUTION.",!
  WRITE "--->Enter 'ZGOTO' from the GTM> prompt to clear error.",!!
  SET $ZSTEP=""  ;"turn off step capture
  xecute "WRITE CrashNonVariable"
  QUIT
  ;
HndlDone ;
  ;"Purpose: To turn off the debugger, allowing program to continue full speed.
  ;"Globally-scoped vars uses: tmgDbgResult, tmgStepMode
  IF +$GET(tmgDbgRemoteJob) DO
  . NEW temp SET temp=$$MessageOut("DONE")
  . SET tmgStepMode="DONE"
  . SET tmgDbgResult=0  ;"Will signal to stop looking for remote messages in TMGIDE3
  ELSE  DO
  . SET $ZSTEP=""   ;"Turn off debugger
  SET tmgMsg=0  ;"ensure $ZSTEP is not turned back on.
  QUIT
  ;
HndlScrWH ;
  ;"Purpose: Handle option to SET screen width and height
  NEW tempVal
  WRITE "Enter screen width: "_tmgScrWidth_"//" READ tempVal:$GET(DTIME,3600),!
  IF (+tempVal>10) DO
  . SET tmgScrWidth=tempVal 
  . SET tmgScrWidth("USER SET")=1
  SET tmgBlankLine=" "
  FOR tmgTempI=1:1:tmgScrWidth-1 SET tmgBlankLine=tmgBlankLine_" "
  WRITE "Enter screen height: "_tmgScrHeight_"//" READ tempVal:$GET(DTIME,3600),!
  IF (+tempVal>5) SET tmgScrHeight=tempVal ;",ScrHeight=tempVal
  WRITE # ;"clear screen
  DO ShowCode(tmgIDEPos,tmgScrWidth,tmgScrHeight,,tmgViewOffset,tmgLROffset,.tmgCsrOnBreakline) ;"<---- not working!
  QUIT
  ;
HndlExpand ;
  ;"Purpose: handle option to expand one mumps like of code.
  NEW expPos,zbLabel,zbOffset,zbRoutine
  DO ParsePos^TMGIDE(tmgIDEPos,.zbLabel,.zbOffset,.zbRoutine)
  SET expPos=zbLabel_"+"_+(zbOffset+tmgViewOffset)_"^"_zbRoutine
  WRITE !
  DO ExpandLine^TMGIDE(expPos)
  NEW tempKey READ "        --- Press Enter To Continue--",tempKey:$GET(DTIME,3600)
  QUIT
  ;
HndlStack(ShowPos,tmgViewOffset) ;
  ;"Purpose: Handle option to show and interact with stack.
  ;"Input: ShowPos -- OPTIONAL.  PASS BY REFERENCE.  Will be changed to user selected value.
  ;"  tmgViewOffset -- OPTIONAL.  PASS BY REFERENCE.  Will be changed to 0 IF user selects NEW Pos.
  ;"Globally scoped vars used: tmgOrigIDEPos
  WRITE !   ;"get below bottom line for output.
  NEW Stack DO GetStackInfo(.Stack,tmgOrigIDEPos)
  NEW Menu SET Menu(0)="Pick Stack Entry to BROWSE TO"
  NEW menuI SET menuI=1
  NEW TMGi FOR TMGi=1:1 QUIT:($GET(Stack(TMGi))="")  DO
  . NEW $ETRAP SET $ETRAP="SET $ETRAP="""",$ecode="""""
  . NEW addr SET addr=$PIECE($$TRIM^XLFSTR(Stack(TMGi))," ",2)
  . NEW txt SET txt=$$TRIM^XLFSTR($text(@addr))
  . SET txt=$$TRIM^XLFSTR(txt,$CHAR(9))
  . NEW line SET line=addr_"   Code: "_txt
  . IF $LENGTH(line)>tmgScrWidth SET line=$EXTRACT(line,1,tmgScrWidth-10)_"..."
  . SET Menu(menuI)=line_$CHAR(9)_addr
  . SET menuI=menuI+1
  NEW UsrSlct SET UsrSlct=$$MENU^TMGUSRI2(.Menu)
  WRITE "User selection: [",UsrSlct,"]",!
  IF (UsrSlct["^")&($LENGTH(UsrSlct)>1) DO
  . SET ShowPos=UsrSlct
  . SET tmgViewOffset=0
  WRITE # ;"clr screen.
  QUIT
  ;
HndlNodes ;
  ;"Purpse: Handle option to browse a variable by nodes.
  NEW varName SET varName=$$Trim^TMGIDE($EXTRACT(tmgOrigAction,7,999))
  WRITE !   ;"get below bottom line for output.
  DO BRWSASK2^TMGMISC
  QUIT
  ;
HndlBrowse ;
  ;"Purpose: Handle option to browse a variable.
  NEW varName SET varName=$$Trim^TMGIDE($EXTRACT(tmgOrigAction,7,999))
  WRITE !   ;"get below bottom line for output.
  DO BRWSNOD2^TMGMISC(varName)
  QUIT
  ;
HndlBrkCond ;
  ;"Purpose: Handle option to browse conditional break
  NEW tmgTpLine
  WRITE "Enter CODE to set $TEST condition.  Examples: 'IF A=1'  or 'IF $$FN1^MOD(A)=2'",!
  WRITE "If execution result=1, breakpoint will stop execution.  Otherwise skips.",!
  ;"READ "Enter IF condition (^ to cancel, @ to delete): ",tmgTpLine:$GET(DTIME,3600),!
  WRITE "Enter IF condition (^ to cancel, @ to delete): ",!
  NEW ifS SET ifS=$$GetBrkCondFlex()
  SET tmgTpLine=$$EDITBOX^TMGUSRI6(ifS,80,"_")
  IF (tmgTpLine["^") QUIT
  NEW brkPos SET brkPos=$$RelConvertPos^TMGIDE(tmgRelPos,tmgViewOffset,tmgArrayName)
  DO SetBrkCond(brkPos,tmgTpLine)
  QUIT
  ;
HndlCstBrk ;
  ;"Purpose: Set a custom breakpoint
  NEW brkPos
  READ !,"Enter breakpoint (e.g. Label+8^MyFunct): ",brkPos:$GET(DTIME,3600),!
  DO SetBreakpoint(brkPos)
  QUIT
  ;
HndlSetBrk ;
  ;"Purpose: Set breakpoint at current point
  ;"WRITE !,"Trying to determine correct breakpoint.  tmgRelPos=",tmgRelPos," tmgViewOffset=",tmgViewOffset,!
  NEW brkPos SET brkPos=$$RelConvertPos^TMGIDE(tmgRelPos,tmgViewOffset,tmgArrayName)
  ;"WRITE "brkPos=",brkPos,!
  IF brkPos="" WRITE "tmgRelPos=",tmgRelPos," view offset=",tmgViewOffset," tmgArrayName=",tmgArrayName,!
  DO ToggleBreakpoint(brkPos)
  QUIT
  ;
HndlJmpBrk ;
  ;"Purpose: Set jump-to breakpoint at current point
  NEW BrkPtList DO GetGTMBrkPts^TMGIDE6(.BrkPtList)  ;"get current list of breakpoints
  ;"ELSE  WRITE "none",!
  DO HndlSetBrk ;"set breakpoint at current cursor position
  NEW AddedBrkPos SET AddedBrkPos=$$BrkPtDelta(.BrkPtList)
  ;"WRITE "Added BrkPt",!
  ;"WRITE ">",AddedBrkPos,!
  ;"DO PRESS2GO^TMGUSRI2
  IF AddedBrkPos="" GOTO HJBDN
  SET tmgDbgJumpToBrkPos=AddedBrkPos
  SET tmgDone=1
  GOTO HndlDone  ;"Should turn off debugger until breakpoint reached. 
HJBDN ;
  QUIT  
  ;
BrkPtDelta(List) ;
   ;"Purpose: Determine which breakpoint currently exists, but is not in passed list
   NEW CurBkPtList
   NEW RESULT SET RESULT=""
   DO GetGTMBrkPts^TMGIDE6(.CurBkPtList)
   NEW IDX SET IDX=""
   FOR  SET IDX=$ORDER(CurBkPtList(IDX)) QUIT:(IDX="")!(RESULT'="")  DO
   . IF $DATA(LIST(IDX)) QUIT
   . SET RESULT=IDX
   QUIT RESULT
   ;
HndlTable ;
  ;"Purpose: Handle option for Table command
  NEW tmgARGS SET tmgARGS=$PIECE(tmgOrigAction," ",2,99)
  IF +$GET(tmgDbgRemoteJob) DO
  . NEW MSG SET MSG="TABLE"
  . IF tmgARGS'="" SET MSG=MSG_tmgARGS
  . NEW temp SET temp=$$MessageOut(MSG)
  . IF temp="" QUIT
  . NEW i SET i=""
  . FOR  SET i=$ORDER(@temp@(i)) QUIT:(i="")  DO
  . . NEW j SET j=""
  . . FOR  SET j=$ORDER(@temp@(i,j)) QUIT:(j="")  DO
  . . . WRITE $GET(@temp@(i,j)),!
  ELSE  DO
  . WRITE !   ;"get below bottom line for output.
  . ;"zshow "*"
  . NEW tmgTEMP,tmgIDX
  . NEW tmgFilter SET tmgFilter=""
  . ZSHOW "V":tmgTEMP
  . IF (tmgARGS'="") DO
  . . SET tmgFilter=$PIECE(tmgARGS," ",1)
  . . SET tmgFilter=$PIECE(tmgFilter,"*",1)
  . SET tmgIDX=0
  . FOR  SET tmgIDX=$ORDER(tmgTEMP("V",tmgIDX)) QUIT:(+tmgIDX'>0)  DO
  . . NEW STR SET STR=$GET(tmgTEMP("V",tmgIDX))      
  . . IF STR["TMGCOL" QUIT
  . . IF $EXTRACT(STR,1,3)="tmg" QUIT
  . . IF (tmgFilter'=""),$EXTRACT(STR,1,$LENGTH(tmgFilter))'=tmgFilter QUIT
  . . WRITE STR,!
  NEW tempKey READ "        --- Press Enter To Continue--",tempKey:$GET(DTIME,3600)
  QUIT
  ;
HndlVars(tmgOrigAction) ;
  ;"Purpose: Handle option for VARS command
  SET tmgOrigAction=$GET(tmgOrigAction)
  NEW tmgARGS SET tmgARGS=$PIECE(tmgOrigAction," ",2,99)
  SET tmgARGS=$PIECE(tmgARGS,"*",1)
  IF +$GET(tmgDbgRemoteJob) DO
  . NEW MSG SET MSG="VARS"
  . IF tmgARGS'="" SET MSG=MSG_tmgARGS
  . NEW temp SET temp=$$MessageOut(MSG)
  . IF temp="" QUIT
  . NEW i SET i=""
  . FOR  SET i=$ORDER(@temp@(i)) QUIT:(i="")  DO
  . . NEW j SET j=""
  . . FOR  SET j=$ORDER(@temp@(i,j)) QUIT:(j="")  DO
  . . . WRITE $GET(@temp@(i,j)),!
  ELSE  DO
  . WRITE !   ;"get below bottom line for output.
  . NEW TABLEALL,VARS
  . ZSHOW "*":tmgDbgTABLEALL
  . MERGE VARS=tmgDbgTABLEALL("V")
  . NEW IDX1 SET IDX1=0
  . FOR  SET IDX1=$ORDER(VARS(IDX1)) QUIT:(+IDX1'>0)  DO
  . . NEW LINE SET LINE=$GET(VARS(IDX1)) QUIT:LINE=""
  . . NEW VARNAME SET VARNAME=$PIECE(LINE,"=",1)
  . . IF $EXTRACT(VARNAME,1,3)="tmg" QUIT
  . . IF $EXTRACT(VARNAME,1,6)="TMGCOL" QUIT
  . . IF tmgARGS'="" QUIT:($EXTRACT(VARNAME,1,$LENGTH(tmgARGS))'=tmgARGS)
  . . NEW VARVAL SET VARVAL=$PIECE(LINE,"=",2,9999)
  . . WRITE VARNAME," = ",VARVAL,!
  NEW tempKey READ "        --- Press Enter To Continue--",tempKey:$GET(DTIME,3600)
  QUIT
  ;
HndlJmpDisp(ShowPos,tmgViewOffset)
  ;"Purpose: to allow user to enter in a location to show in code displayer
  ;"Input: ShowPos : PASS BY REFERENCE.  The NEW location to change to
  ;"       tmgViewOffset : PASS BY REFERECE.  Will be changed to 0 IF ShowPos changed.
  NEW tempLoc
  WRITE "(Example: MYLABL+2^MYCODE)",!
  WRITE "Enter location to jump display to: "
  READ tempLoc:$GET(DTIME,999),!
  IF (tempLoc'="^")&(tempLoc["^")&(tempLoc'[" ") DO
  . IF $TEXT(@tempLoc)'="" DO
  . . SET ShowPos=tempLoc
  . . SET tmgViewOffset=0
  . ELSE  DO
  . . WRITE "Sorry.  No code found at ",tempLoc,!
  . . DO PRESS2GO^TMGUSRI2
  QUIT
  ;
HndlRunDW ;
   ;"Purpose: Handle resuming or starting Data Watch session mode.
   ;"         This mode is where the program runs at nearly full speed,
   ;"         with tests being checked between each line of code.
  NEW WatchMode SET WatchMode=0
  DO EditWatch^TMGIDE7(.WatchMode)
  IF WatchMode=1 DO SetRunMode(5) ;"data watch mode
  QUIT
  ;
HndlHelp ;
  ;"Purpose: Handle option for help.
  DO Box
  DO SetColors("NORM")
  DO CUP^TMGTERM(1,2) ;"Cursor to line (1,2)
  DO HlpWrite(" {L} : Run sLow mode    | {M} : exec M code       | {SHOW [var]} : show [var]")
  DO HlpWrite(" {O} : Step OVER line   | {I} : step INTO line    | {J} : Run To Cursor")
  DO HlpWrite(" {R} : Run | {T} Step OUT | {H} : Hide debug code   | {CLS} : clear screen")
  DO HlpWrite(" {X} : Turn off debug   | {Q} : Abort             | {HOP} : Hop between Brkpoints  ")
  DO HlpWrite(" {B} : Toggle Brkpoint  | {C} : Custom breakpoint | {BC} : breakpoint code")
  DO HlpWrite(" {W} : Set watch code   | {W +MyVar} :Watch MyVar | {W -MyVar} :Remove watch")
  DO HlpWrite(" {A},{AA} : Scroll up     | {Z},{ZZ} : Scroll down    | {W +^} : Add Naked Ref")
  DO HlpWrite(" {[},{[[} : Scroll left   | {]},{]]} : Scroll right   | {W +*} : Watch Var changes")
  DO SetColors("SPECIAL")
  DO PRESS2GO^TMGUSRI2
  DO Box
  DO SetColors("NORM")
  DO CUP^TMGTERM(1,2) ;"Cursor to line (1,2)
  DO HlpWrite(" {BROWSE} [var] : browse| {SHOW [var]} : shows var| {ZWR [var]} : zwrites [var]")
  DO HlpWrite(" {TABLE *} : Symbol table| {NODES} : Browse var    | {INITKB} : restore key fn")
  DO HlpWrite(" {G} : Goto display      | {FULL} : Undo Scrl Zone | {E} : expand current line")
  DO HlpWrite(" {UCASE} : Force U Case  | {LCASE} : Force L Case  | {COLORS} : Edit colors   ")
  DO HlpWrite(" {XCMD} : Force ExpndCmd | {SCMD} : Force ShrtnCmd | {TRACE} : Show Trace     ")
  DO HlpWrite(" {VDIFF} : Show Var Chng | {TVDIFF} Toggle TraceVar| {RESYNC} : sync display  ")
  DO HlpWrite(" {DBK} : Del Brk Point   | {RDW} : Run data watch  | {STACK} : stack show/jump")
  DO HlpWrite(" {-},{+} : Screen width    | {=} : Enter Width/Ht    | {HIDE} : manage/hide modules")
  DO HlpWrite(" {VARS [<Name*>]} : shows variable table ")
  ;"WRITE HlpWrite("                                                                                  "),!
  DO SetColors("SPECIAL")
  DO PRESS2GO^TMGUSRI2
  ;DO Box
  ;DO SetColors("NORM")
  ;DO CUP^TMGTERM(1,2) ;"Cursor to line (1,2)
  ;DO HlpWrite(" {VARS [<Name*>]} : Var dump | {ZWR}: ZWRITE data |                          ")
  ;DO HlpWrite("   ")
  ;DO SetColors("SPECIAL")
  ;DO PRESS2GO^TMGUSRI2
  DO SetColors("Reset")
  QUIT
  ;
HlpWrite(line)  ;
  FOR  QUIT:($LENGTH(line)'>0)  DO
  . IF $find(line,"{")>0 DO
  . . NEW part SET part=$PIECE(line,"{",1)
  . . DO SetColors("NORM")
  . . WRITE part
  . . SET line=$PIECE(line,"{",2,999)
  . . SET part=$PIECE(line,"}",1)
  . . DO SetColors("SPECIAL")
  . . WRITE part
  . . SET line=$PIECE(line,"}",2,999)
  . ELSE  DO
  . . DO SetColors("NORM")
  . . WRITE line,!
  . . SET line=""
  DO SetColors("NORM")
  QUIT
  ;
ErrTrap(tmgIDEPos)  ;
   ;"Purpose: This is the line that is called by GT.M for each ztrap event.
   ;"      It will be used to display the current code execution point
  IF $$ShouldSkip($PIECE(tmgIDEPos,"^",2)) DO
  . USE $P  ;"//kt 8/10/22
  . WRITE !,"Error at ",$P($ZSTATUS,",",2)," -- in code that debugger can't display.",!
  . WRITE "Error is: ",$P($ZSTATUS,",",3,99),!
  . WRITE !,"Dropping to command line via BREAK",!
  . BREAK
  ;"NEW tmgScrHeight,tmgScrWidth
  SET tmgScrHeight=$GET(tmgScrHeight,10)
  SET tmgScrWidth=$GET(tmgScrWidth,70)
  DO VCUSAV2^TMGTERM
  DO ShowCode(tmgIDEPos,tmgScrWidth,tmgScrHeight,0)
ETDone ;
  DO VCULOAD2^TMGTERM
  QUIT
  ;
ShowCode(ShowPos,tmgScrWidth,tmgScrHeight,Wipe,tmgViewOffset,tmgLROffset,tmgCsrOnBreakline)  ;
  ;"Purpose: This will display code at the top of the screen
  ;"Input: ShowPos -- string like this: X+2^ROUTINE[$DMOD]
  ;"      tmgScrWidth -- width of code display (Num of columns)
  ;"      tmgScrHeight -- height of code display (number of rows)
  ;"      Wipe -- OPTIONAL.  IF 1, then code area is wiped blank
  ;"      tmgViewOffset -- OPTIONAL.  If a value is supplied, then
  ;"               the display will be shifted up or down (i.e. to view
  ;"               code other than at the point of execution)
  ;"               Positive numbers will scroll page downward.
  ;"       tmgLROffset -- OPTIONAL. IF value > 0 then the display
  ;"               of each line will begin with this number character.
  ;"               (i.e. will shift screen so that long lines can be seen.)
  ;"               0->no offset; 1->no offset (start at character 1);  2->offset 1
  ;"       tmgCsrOnBreakline -- OPTIONAL. PASS BY REFERENCE.  Will return 1
  ;"               IF cursor is on a break line, otherwise 0
  NEW cdLoop,scRoutine,scLabel,scOffset,scS
  NEW LastRou,LastLabel,LastOffset
  NEW dbFGColor,bBGColor,nlFGColor,nlBGColor
  NEW StartOffset,scCursorLine,cbLineLen
  NEW zBreakIdx SET zBreakIdx=-1
  NEW tmgDbgJNum SET tmgDbgJNum=$J
  IF +$GET(tmgDbgRemoteJob) SET tmgDbgJNum=+tmgDbgRemoteJob
  NEW tmgZArrayName SET tmgZArrayName=$name(^TMG("TMGIDE",tmgDbgJNum,"MODULES"))
  SET tmgScrWidth=$GET(tmgScrWidth,80)
  SET tmgScrHeight=$GET(tmgScrHeight,10)
  SET tmgLROffset=+$GET(tmgLROffset,1)
  NEW tmgDbgBlankLine SET $PIECE(tmgDbgBlankLine," ",tmgScrWidth-1)=""
  DO CUP^TMGTERM(1,1) ;"Cursor to line (1,1)
  IF $GET(Wipe)=1 DO  GOTO SCDone  ;"Blank screen and then QUIT
  . DO SetColors("Reset")
  . FOR cdLoop=0:1:tmgScrHeight+1 WRITE tmgDbgBlankLine,!
  ;
  SET scS=$PIECE(ShowPos,"$",1)  ;"e.g. X+2^ROUTINE$DMOD-->X+2^ROUTINE
  DO ParsePos^TMGIDE(scS,.scLabel,.scOffset,.scRoutine)
  IF scRoutine="" DO  GOTO SCDone
  . WRITE !,!,"Error -- invalid position provided to ShowCode routine: ",ShowPos,!
  . WRITE "scS=",scS,!
  ;
  ;"setup to show a symbol for breakpoint
  NEW zbS SET zbS=""
  FOR  SET zbS=$ORDER(^TMG("TMGIDE",tmgDbgJNum,"ZBREAK",zbS)) QUIT:(zbS="")  DO
  . NEW zbRoutine,zbLabel,zbOffset
  . NEW tmgTempPos SET tmgTempPos=$$ConvertPos^TMGIDE(zbS,tmgZArrayName)
  . DO ParsePos^TMGIDE(tmgTempPos,.zbLabel,.zbOffset,.zbRoutine)
  . IF zbRoutine'=scRoutine QUIT
  . IF zbLabel'=scLabel QUIT
  . SET zBreakIdx(zbOffset)=1
  ;"-----Also get breakpoints stored in GT.M --------------
  NEW gtmBrkPts zshow "B":gtmBrkPts
  NEW zbI SET zbI=0
  FOR  SET zbI=$ORDER(gtmBrkPts("B",zbI)) QUIT:(zbI="")  DO
  . SET zbS=$GET(gtmBrkPts("B",zbI)) QUIT:zbS=""
  . NEW zbRoutine,zbLabel,zbOffset
  . NEW tmgTempPos SET tmgTempPos=$$ConvertPos^TMGIDE(zbS,tmgZArrayName)
  . DO ParsePos^TMGIDE(tmgTempPos,.zbLabel,.zbOffset,.zbRoutine)
  . IF zbRoutine'=scRoutine QUIT
  . IF zbLabel'=scLabel QUIT
  . SET zBreakIdx(zbOffset)=1
  ;
  IF scOffset>(tmgScrHeight) SET StartOffset=(scOffset-tmgScrHeight)+2
  ELSE  SET StartOffset=0
  SET StartOffset=StartOffset+$GET(tmgViewOffset)
  ;
  ;"====Draw the top line ==========================================
  DO SetColors("NORM")
  WRITE "=== "
  DO SetColors("SPECIAL")
  SET scS="Routine: "_scLabel_"^"_scRoutine_" "
  IF $DATA(tmgOrigIDEPos) SET scS=scS_"("_tmgOrigIDEPos_")"
  ELSE  SET scS=scS_"("_ShowPos_")"
  SET scS=scS_" Runmode: "_$get(tmgRunMode)
  WRITE scS
  DO SetColors("NORM")
  WRITE " "
  FOR cdLoop=1:1:tmgScrWidth-$LENGTH(scS)-5 WRITE "="
  DO SetColors("NORM")
  WRITE !
  ;
  ;"NEW T1,T2,T3,TMA,TMB SET (TMA,TMB)=0
  SET tmgCsrOnBreakline=0
  FOR cdLoop=StartOffset:1:(StartOffset+tmgScrHeight) DO
  . ;"SET T1=$$GETSEC^TMGIDE2() ;"======================================
  . DO SetColors("NORM")
  . DO SetTempBkColor("Reset")
  . NEW cbLine,cbRef,cbCursor,cBrkLine
  . SET cBrkLine=$DATA(zBreakIdx(cdLoop))
  . SET cbRef=scLabel_"+"_cdLoop_"^"_scRoutine
  . SET cbLine=$text(@cbRef)
  . FOR  QUIT:cbLine'[$CHAR(9)  SET cbLine=$PIECE(cbLine,$CHAR(9),1)_"        "_$PIECE(cbLine,$CHAR(9),2,999)
  . ;"IF tmgLROffset>0 SET cbLine=$EXTRACT(cbLine,tmgLROffset,999)
  . SET scCursorLine=scOffset+$GET(tmgViewOffset)
  . NEW cHighCsrPos SET cHighCsrPos=(cdLoop=scCursorLine)
  . NEW cHighExecPos SET cHighExecPos=(cdLoop=scOffset)
  . IF cHighCsrPos DO SetTempBkColor("Highlight")
  . IF cHighExecPos DO SetTempBkColor("HighExecPos")
  . IF cBrkLine DO
  . . IF (cHighCsrPos=0)&(cHighExecPos=0) DO
  . . . DO SetTempBkColor("HighBkPos")
  . . ELSE  DO
  . . . DO SetTempBkColor("BkPos")
  . . . SET tmgCsrOnBreakline=1
  . WRITE $SELECT(cdLoop=scOffset:">",cBrkLine:"#",1:" ")
  . DO SetColors("SPECIAL")
  . IF cdLoop>0 WRITE "+"_cdLoop_$SELECT(cdLoop<10:" ",1:"")
  . ELSE  WRITE "   "
  . DO SetColors("NORM")
  . ;"IF $LENGTH(cbLine)>(tmgScrWidth-1) SET cbLine=$EXTRACT(cbLine,1,tmgScrWidth-4)_"..."
  . ;"SET cbLineLen=$LENGTH(cbLine)
  . NEW StartPos SET StartPos=$X
  . IF $GET(TMGIDEDEBUG) WRITE cbLine SET cbLineLen=$LENGTH(cbLine)  ;"temp
  . ELSE  SET cbLineLen=$$ShowLine^TMGIDE6(cbLine,tmgLROffset,.tmgDbgOptions,tmgScrWidth-StartPos)
  . WRITE $EXTRACT(tmgDbgBlankLine,cbLineLen,tmgScrWidth-StartPos-1)
  . DO SetTempBkColor("Reset"),SetColors("NORM")
  . WRITE !
  ;
  ;"Draw bottom line.
  DO SetColors("NORM")
  ;"do SetColors("SPECIAL")
  FOR cdLoop=1:1:tmgScrWidth WRITE "~"
  ;"do SetColors("NORM")
  WRITE !
  ;"WRITE "Longest section was ",TM,!
SCDone ;
  DO VTATRIB^TMGTERM(0)  ;"reset colors
  QUIT
  ;
GETSEC()  ;"GET SYSTEM SECONDS
  NEW T,SEC,MCS SET T=$ZH SET SEC=$P(T,",",2),MCS=$P(T,",",3)
  FOR  QUIT:$LENGTH(MCS)=6  SET MCS="0"_MCS
  QUIT SEC_"."_MCS
  ;
MAX(A,B) ;
  IF A>B QUIT A
  QUIT B
  ;
SetTempBkColor(tmgMode)  ;
  SET tmgMode=$GET(tmgMode) QUIT:tmgMode=""
  NEW ref SET ref=$name(^TMG("TMGIDE",$J,"COLORS"))
  IF tmgMode="Reset" KILL @ref@("TEMP BACKGROUND") QUIT
  IF "Highlight,HighExecPos,BkPos,HighBkPos"'[tmgMode QUIT
  IF $DATA(@ref)=0 DO InitColors^TMGIDE6
  NEW bg SET bg=$GET(@ref@(tmgMode))
  IF bg="" QUIT
  SET @ref@("TEMP BACKGROUND")=bg
  QUIT
  ;
SetColors(tmgMode)  ;   
  ;"Purpose: SET colors in central location
  ;"Input: tmgMode -- the tmgMode to change the colors to
  ;"       bg -- OPTIONAL -- the default background.  Default=15
  ;"set ^TMG("TMP","SETCOLORS MODE",$ZH)=$get(tmgMode)
  SET tmgMode=$GET(tmgMode) 
  IF tmgMode="" SET tmgMode="Reset"
  NEW ref SET ref=$name(^TMG("TMGIDE",$J,"COLORS"))
  IF $DATA(@ref)=0 DO
  . DO InitColors^TMGIDE6
  IF tmgMode="Reset" DO  GOTO SCDn
  . DO VTATRIB^TMGTERM(0)   ;"reset colors
  NEW colorSet MERGE colorSet=@ref@(tmgMode) ;"Get colors for mode
  NEW fg SET fg=$GET(colorSet("fg"),15)
  NEW bg SET bg=$GET(colorSet("bg"),15)
  IF (bg="@") DO
  . SET bg=$GET(@ref@("TEMP BACKGROUND"),"@")
  . IF bg="@" SET bg=$GET(@ref@("BACKGROUND"),0)
  IF fg=bg DO
  . IF (fg<15) SET fg=fg+1
  . ELSE  IF (fg>0) SET fg=fg-1
  DO VCOLORS^TMGTERM(fg,bg)
SCDn ;
  QUIT
  ;
Box    ;
  ;"Purpose: Draw a box on the top of the screen.
  ;"Globals Scope Vars used: tmgScrWidth,tmgScrHeight
  SET tmgScrWidth=$GET(tmgScrWidth,80)
  SET tmgScrHeight=$GET(tmgScrHeight,10)
  NEW tmgDbgBlankLine SET $PIECE(tmgDbgBlankLine," ",tmgScrWidth)=" "
  NEW ideBarLine SET $PIECE(ideBarLine,"=",tmgScrWidth)="="
  DO CUP^TMGTERM(1,1) ;"Cursor to line (1,1)
  DO SetColors("Highlight")
  WRITE ideBarLine,!
  DO SetColors("NORM")
  NEW cdLoop FOR cdLoop=0:1:tmgScrHeight+1 WRITE tmgDbgBlankLine,!
  DO SetColors("Reset")
  QUIT
  ;
GetStackInfo(Stack,ExecPos)   ;
  ;"Purpose:  to query GTM and get back filtered Stack information
  ;"Input: Stack  -- PASS BY REFERENCE.  An array to received back info.  Old info is killed
  ;"       ExecPos -- OPTIONAL. Current execution position
  KILL Stack
  NEW i,count SET count=1
  IF $STACK<3 QUIT  ;"0-2 are steps getting into debugger
  FOR i=0:1:$STACK DO  ;"was 3:1:
  . NEW s SET s=$STACK(i,"PLACE")
  . IF s["TMGIDE" QUIT
  . IF s["GTM$DMOD" QUIT
  . IF s="@" SET s=s_""""_$STACK(i,"MCODE")_""""
  . IF s=$GET(ExecPos) SET s=s_" <--Current execution point" ;",i=$STACK+1
  . SET Stack(count)=$STACK(i)_" "_s
  . SET count=count+1
  QUIT
  ;
ToggleBreakpoint(pos,condition)   ; 
  ;"Purpose: to SET or release the GT.M breakpoint at position
  ;"Input: pos -- the position to alter
  ;"       condition -- OPTIONAL -- should be contain valid M code such that
  ;"                    IF @condition  is valid.  Examples:
  ;"                    i=1   or  $DATA(VAR)=0  or  $$MyFunct(var)=1
  ;"WRITE "Here in ToggleBreakoint",!
  IF $$IsBreakpoint(pos) DO
  . ;"WRITE " calling RelBreakpoint",!
  . DO RelBreakpoint(pos)
  ELSE  DO
  . ;"WRITE "calling Set breakpoint",!
  . DO SetBreakpoint(pos,.condition)
  QUIT
  ;   
IsBreakpointFlex() ;
  NEW p1 SET p1=$GET(tmgIDEPos)
  NEW p2 SET p2=$GET(tmgOrigIDEPos)   
  NEW zzAtBkPt set zzAtBkPt=$$IsBreakpoint(p1) 
  ;"WRITE "$$IsBreakpoint(tmgIDEPos)=",zzAtBkPt,!
  if zzAtBkPt=0 set zzAtBkPt=$$IsBreakpoint(p2) 
  ;"WRITE "$$IsBreakpoint(tmgOrigIDEPos)=",zzAtBkPt,!
  QUIT zzAtBkPt
  ;
IsBreakpoint(pos)  ;
  ;"Purpose: to determine IF position is a breakpoint pos
  ;"Note: I am concerned that pos might contain a name longer than 8 chars
  ;"      and might give a false result, or ^TMP(...) might hold a name
  ;"      longer than 8 chars.
  ;"      BUT, IF I just cut name off at 8 chars, it might not work well
  ;"      with GTM v5
  NEW result SET result=0
  NEW tmgDbgJNum SET tmgDbgJNum=$J
  IF +$GET(tmgDbgRemoteJob) SET tmgDbgJNum=+tmgDbgRemoteJob
  ;"IF $GET(pos)'="" SET result=$DATA(^TMG("TMGIDE",tmgDbgJNum,"ZBREAK",pos))
  new aPos set aPos=""
  for  set aPos=$order(^TMG("TMGIDE",tmgDbgJNum,"ZBREAK",aPos)) quit:(aPos="")!(result=1)  do
  . set result=$$EquivalentBreakpoint(pos,aPos)
  QUIT (result'=0)
  ;
EquivalentBreakpoint(pos1,pos2)   ;
   ;"PURPOSE: see if two positions are equivalent.  E.g. +35^TMGRPC1H and PROCESS+10^TMGRPC1H are really equivalent
   ;"//to do... finish
   NEW result set result=(pos1=pos2)   ;<-- implement later
   QUIT result
   ;    
EnsureBreakpoints()  ;
   ;"Purpose: When an module is recompiled, GT.M drops the breakpoints for
   ;"         that module.  However, the breakpoints are still stored for this
   ;"         debugger, meaning that the lines will still be highlighted etc,
   ;"         --but they don't work.  This function will go through stored
   ;"         breakpoints and again register them with GT.M
   NEW pos SET pos=""
   NEW tmgDbgJNum SET tmgDbgJNum=$J
   IF +$GET(tmgDbgRemoteJob) SET tmgDbgJNum=+tmgDbgRemoteJob
   FOR  SET pos=$ORDER(^TMG("TMGIDE",tmgDbgJNum,"ZBREAK",pos)) QUIT:(pos="")  DO
   . DO SetBreakpoint(pos)
   QUIT
   ;    
SetBreakpoint(pos,condition)  ;
   ;"Purpose: SET the GT.M breakpoint to pos position
   ;"Input: pos -- the position to alter
   ;"  condition -- OPTIONAL -- should be contain valid M code such that
  ;"                    IF @condition  is valid.  Examples:
  ;"                    i=1   or  $DATA(VAR)=0  or  $$MyFunct(var)=1
  ;"Globally scoped var used:
  ;"       tmgDbgRemoteJob-- OPTIONAL -- IF controlling a remote process, then = $J of that process
  ;"                       and action should not be done locally.
  IF $GET(pos)="" DO  GOTO SBkDone
  . WRITE "?? no position specified ??",!
  ;
  NEW tmgDbgJNum SET tmgDbgJNum=$J
  IF +$GET(tmgDbgRemoteJob) SET tmgDbgJNum=+tmgDbgRemoteJob
  SET ^TMG("TMGIDE",tmgDbgJNum,"ZBREAK",pos)=""
  DO SetBrkCond(pos,.condition)
  ;
  IF $GET(tmgDbgRemoteJob) DO
  . NEW temp SET temp=$$MessageOut("BKPOS "_pos_" "_$GET(condition))
  . WRITE "Results from remote process=",temp,!
  ELSE  DO
  . NEW brkLine SET brkLine=pos_":""n tmg do SetRunMode^TMGIDE2(1,1) s tmg=$$STEPTRAP^TMGIDE2($ZPOS,1)"""
  . NEW $ETRAP
  . SET $ETRAP="K ^TMG(""TMGIDE"",$J,""ZBREAK"",pos) S $ETRAP="""",$ECODE="""""
  . ZBREAK @brkLine
SBkDone ;
  QUIT
  ;    
SetBrkCond(pos,condition)  ;
  ;"Purpose: A standardized SET for condition.
  ;"Input: pos --
  ;"       condition --
  IF $GET(condition)="" QUIT
  IF $GET(pos)="" QUIT
  NEW tmgDbgJNum SET tmgDbgJNum=$J
  IF +$GET(tmgDbgRemoteJob) SET tmgDbgJNum=+tmgDbgRemoteJob
  IF condition="@" KILL ^TMG("TMGIDE",tmgDbgJNum,"ZBREAK",pos,"IF")
  ELSE  SET ^TMG("TMGIDE",tmgDbgJNum,"ZBREAK",pos,"IF")=condition
  IF $$IsBreakpoint(pos)=0 DO SetBreakpoint(pos)
  QUIT
  ;    
GetBrkCondFlex() ;
  NEW ifS 
  SET ifS=$$GetBrkCond(tmgIDEPos)
  ;"WRITE "$$GetBrkCond(tmgIDEPos)=",ifS,!
  IF ifS="" SET ifS=$$GetBrkCond($$RelConvertPos^TMGIDE(tmgIDEPos,tmgViewOffset,tmgArrayName)) do
  . ;"WRITE "$$GetBrkCond($$RelConvertPos^TMGIDE(tmgIDEPos... =",ifS,!
  IF ifS="" SET ifS=$$GetBrkCond($$RelConvertPos^TMGIDE(tmgRelPos,tmgViewOffset,tmgArrayName)) do       
  . ;"WRITE "$$GetBrkCond($$RelConvertPos^TMGIDE(tmgRelPos... =",ifS,!
  QUIT ifS
  ;
GetBrkCond(pos) ;
  ;"Purpose: A standardized GET for condition.
  ;"Results: returns condition code, or ""
  NEW result SET result=""
  NEW tmgDbgJNum SET tmgDbgJNum=$J
  IF +$GET(tmgDbgRemoteJob) SET tmgDbgJNum=+tmgDbgRemoteJob
  SET:(pos'="") result=$GET(^TMG("TMGIDE",tmgDbgJNum,"ZBREAK",pos,"IF"))
  QUIT result
  ;    
RelBreakpoint(pos)  ;
  ;"Purpose: to release a  GT.M breakpoint at position
  NEW tmgDbgJNum SET tmgDbgJNum=$J
  IF +$GET(tmgDbgRemoteJob) SET tmgDbgJNum=+tmgDbgRemoteJob
  NEW I SET I=""
  NEW GTMPTS ZSHOW "B":GTMPTS
  FOR  SET I=$ORDER(GTMPTS("B",I)) QUIT:(I="")  DO
  . NEW CODE SET CODE="ZBREAK -"_$GET(GTMPTS("B",I))
  . IF $GET(GTMPTS("B",I))'=pos QUIT
  . ;"WRITE "EXECUTING [",CODE,"]",!    
  . XECUTE CODE
  KILL ^TMG("TMGIDE",tmgDbgJNum,"ZBREAK",pos)
  QUIT
  IF $GET(tmgDbgRemoteJob) DO  GOTO SBkDone
  . NEW temp SET temp=$$MessageOut("RELBKPOS "_pos)
  ELSE  DO
  . NEW brkLine SET brkLine=pos_":""zcontinue"""
  . ZBREAK @brkLine
  ;"WRITE "released breakpoint at: ",pos,!
  QUIT
  ;
ShouldSkip(module) ;
  ;"Purpose: to see IF module is in hidden list
  NEW result SET result=0
  IF $GET(tmgDbgHideList)="" SET tmgDbgHideList=$name(^TMG("TMGIDE",$J,"HIDE LIST"))  ;"//kt 3/25/11
  IF $DATA(@tmgDbgHideList)=0 DO SetHideList^TMGIDE
  ;"if $GET(tmgDbgHideList)="" GOTO SSKDone
  NEW HideMod SET HideMod=""
  FOR  SET HideMod=$ORDER(@tmgDbgHideList@(HideMod)) QUIT:(HideMod="")!(result=1)  DO
  . IF (module=HideMod) SET result=1 QUIT
  . IF HideMod'["*" QUIT
  . NEW tempMod SET tempMod=$EXTRACT(HideMod,1,$find(HideMod,"*")-2)
  . NEW trimModule SET trimModule=$EXTRACT(module,1,$LENGTH(tempMod))
  . SET result=(trimModule=tempMod)
SSKDone ;
  QUIT result
  ;    
SetupSkips  ;
  ;"Purpose: to manage modules that are to be skipped over.
  ;"Input: none.  But this modifies variable @tmgDbgHideList with global scope
  ;"results: none
  ;"For some reason, this gets lost at times....
  IF $DATA(tmgDbgHideList)=0 SET tmgDbgHideList=$name(^TMG("TMGIDE",$J,"HIDE LIST"))
  ;
  NEW menu,option
  SET menu(0)="Pick Options for Hiding/Showing Modules"
  SET menu(1)="SHOW current hidden list"_$c(9)_"SHOW"
  SET menu(2)="ADD module to hidden list"_$c(9)_"ADD"
  SET menu(3)="REMOVE module from hidden list"_$c(9)_"REMOVE"
  SET menu(4)="Done."_$c(9)_"^"
StSkp ;
  SET option=$$MENU^TMGUSRI2(.menu)
  IF option="SHOW" DO ShowSkip
  IF option="ADD" DO AddSkip
  IF option="REMOVE" DO RmSkip
  IF option="^" GOTO StSkDone
  GOTO StSkp
StSkDone ;
  QUIT
  ;    
AddSkip ;
  ;"Purpose: to allow user to Add a module to hidden list
  ;"Input: none.  But this modifies variable @tmgDbgHideList with global scope
  ;"results: none
ASKP1 ;
  WRITE "Enter name of module to add to hidden list (? for help, ^ to abort)",!
  NEW mod
  READ "Enter module: ",mod:$GET(DTIME,3600),!
  IF mod="?" DO  GOTO ASKP1
  . WRITE "Some modules of the code are not helpful to debugging one's code.",!
  . WRITE "For example, IF one did not ever want to trace into the code stored",!
  . WRITE "in DIC, then DIC would be added as a module to be hidden.  Then, when",!
  . WRITE "debugging one's own code, all traces into ^DIC would be skipped over.",!
  . WRITE "If only part of the name is specified, then ALL modules starting with",!
  . WRITE "this name will be excluded.",!
  . DO PRESS2GO^TMGUSRI2
  IF mod="^" GOTO ASDone
  WRITE "Add '",mod,"' as a module to be skipped over"
  NEW % SET %=1
  DO YN^DICN
  IF $DATA(tmgDbgHideList)=0 SET tmgDbgHideList=$name(^TMG("TMGIDE",$J,"HIDE LIST"))
  IF %=1 SET @tmgDbgHideList@(mod)=""
ASDone  ;  
  QUIT
  ;    
RmSkip  ;
  ;"Purpose: to allow user to remove a module from hidden list
  ;"Input: none.  But this modifies variable @tmgDbgHideList with global scope
  ;"results: none
  NEW menu,option,idx
RmL1 ;
  KILL menu
  SET idx=0
  NEW mod SET mod=""
  ;"Load menu with current list.
  FOR  SET mod=$ORDER(@tmgDbgHideList@(mod)) QUIT:(mod="")  DO
  . SET idx=idx+1,menu(idx)=mod_$c(9)_mod
  IF $DATA(menu)=0 DO  GOTO RmSkipDone
  . WRITE "--The list is currently empty--"
  . DO PRESS2GO^TMGUSRI2
  SET idx=idx+1
  SET menu(idx)="Done"_$c(9)_"^"
  SET menu(0)="Pick Module to remove from hidden list"
  SET option=$$MENU^TMGUSRI2(.menu)
  IF option="^" GOTO RmSkipDone
  KILL @tmgDbgHideList@(option)
  GOTO RmL1
RmSkipDone ;
  QUIT
  ;    
ShowSkip ;
  ;"Purpose: to show the hidden list
  ;"Input: none.  But this uses variable @tmgDbgHideList with global scope
  ;"results: none
  NEW mod SET mod=""
  IF $DATA(@tmgDbgHideList)>0 DO
  . FOR  SET mod=$ORDER(@tmgDbgHideList@(mod)) QUIT:(mod="")  DO
  . . WRITE "    ",mod,!
  ELSE  DO
  . WRITE "--The list is currently empty--"
  DO PRESS2GO^TMGUSRI2
  QUIT
  ;        
  ;"=============================================
  ;" Code FOR when controlling another process
  ;"=============================================
  ;
MessageOut(Msg,timeOutTime,ignoreReply)  ;
  ;"Purpose: For use when in remote-control debugging mode.  This will
  ;"         send a message to SENDER, not waiting FOR a reply
  ;"Input: Msg --  the message to send
  ;"       timeOutTime -- OPTIONAL, default is 2 seconds
  ;"       ignoreReply -- OPTIONAL, default is 0 (don't ignore)
  ;"Output: the returned message, or "" IF timed out or no reply, or ignoreReply=1

  SET timeOutTime=$GET(timeOutTime,2)
  SET ignoreReply=$GET(ignoreReply,0)
  NEW result SET result=""
  SET Msg="[CMD] "_$GET(Msg)
  SET ^TMG("TMGIDE","CONTROLLER","MSG-OUT")=Msg
  SET ^TMG("TMGIDE","CONTROLLER","MSG-IN")=""
  IF (ignoreReply=0) FOR  DO  QUIT:(result'="")!(timeOutTime<0)
  . SET result=$GET(^TMG("TMGIDE","CONTROLLER","MSG-IN"))
  . IF (result'="") QUIT
  . SET timeOutTime=timeOutTime-0.1
  . SET ^TMG("TMGIDE","CONTROLLER","MSG-OUT")=Msg
  . HANG 0.1
  IF $PIECE(result," ",1)="[RSLT]" DO
  . SET result=$PIECE(result," ",2,999)
  ELSE  DO
  . WRITE !,"Unexpected reply: ",result,!
  . DO PRESS2GO^TMGUSRI2
  . SET result=""
  ;
  QUIT result
  ;
GetRemoteVar(varName)  ;
  ;"Purpose: Pass varName to remote process, have it evaluated there, and
  ;"         then passed back back here for display.
  ;"Input: varName -- expression (variable name, or function) to be evaluated.
  NEW temp SET temp=$$MessageOut("EVAL "_$GET(varName))
  KILL @varName
  IF (temp="")!(temp[" ") DO  GOTO GMVD
  . WRITE !,"Unexpected var name back: [",temp,"]",!
  . SET temp=""
  MERGE @varName=@temp
GMVD  ;
  QUIT varName
  ;
RemoteXecute(MCode)  ;
  ;"Purpose: Pass M Code to remote process for execution there.
  ;"Input: A line of M code, as entered by user.
  ;"Results: none
  ;"Output: Any IO of M code should be shown in other process's IO
  NEW temp SET temp=$$MessageOut("XECUTE "_$GET(MCode))
  QUIT
  ;