TMGIDE2 ;TMG/kst/A debugger/tracer for YottaDB (core functionality) ;12/17/14, 3/21/24
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
  ;" SET $ZSTEP='DO STEPTRAP^TMGIDE2($ZPOS) ZSTEP INTO'  <-- Actually not quite.  See code for details.  
  ;" ZSTEP into
  ;" DO ^MyFunction   <--- put the function you want to trace here
  ;"
  ;" SET $ZSTEP=""  <---turn off step capture
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
  ;
  ;"=======================================================================
  ;"PRIVATE API FUNCTIONS
  ;"=======================================================================
  ;"CMDPROMPT -- Display the command prompt, and handle user input
  ;"MOVEKEY(tmgAction) -- Handle movement keys
  ;"SETTEMPBKCOLOR(mode)
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
  ;"        (anything else) -- stop debugging. Effects ZCONTINUE (runn full speed)
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
  NEW tmgDbgResult,tmgDbgJNum,tmgArrayName,%tmg,tmgRelPos,tmgOrigIDEPos,tmgTempPos
  NEW tmgBlankLine,tmgAction,tmgKeyIn,tmgTempI,tmgDone,tmgViewOffset
  NEW tmgSavedIO,tmgSavedX,tmgSavedY,tmgDEVInfo,tmgDEVSav,tmgTEMPEVAL
  NEW tmgStpSkip,tmgCsrOnBreakline
  ;"----Misc tasks for each run loop -----------------------
  NEW tmgdbgTruth SET tmgdbgTruth=$TEST     ;"save initial value of $TEST
  NEW tmgDbgNakedRef SET tmgDbgNakedRef=$R  ;"save naked reference
  NEW $ESTACK  ;"Any entries into $ESTACK will be related to debugger, used for filtering $STACK
  IF 1=0 DO DEBUGLOG($GET(tmgIDEPos)_" | tmgMsg="_$GET(tmgMsg)_" | tmgRunMode="_$GET(tmgRunMode))
  IF $$ShouldSkip^TMGIDE2C($PIECE(tmgIDEPos,"^",2)) GOTO SPDN2  ;"Don't show hidden modules (setup in TMGIDE module)  
  IF $DATA(tmgDbgJumpToBrkPos) DO RelBreakpoint^TMGIDE2C(tmgDbgJumpToBrkPos) KILL tmgDbgJumpToBrkPos
  DO INITVARS   ;"also changes $IO to $P
  IF tmgTEMPEVAL=1 GOTO SPDN1 ;"set in INITVARS
  IF +$GET(tmgDbgOptions("TRACE"))=1 DO RecordTrace^TMGIDE6(tmgOrigIDEPos)  ;"Record trace, If not a hidden module  
  DO CONDBKPTS(.tmgStpSkip,.tmgMsg,.tmgStepMode) GOTO:(tmgStpSkip=1) SPDN1  ;"Check for Conditional Breakpoints 
  IF ("1234"[tmgRunMode)&(+$GET(tmgDbgOptions("VARTRACE"))=1) DO RecordVTrace^TMGIDE6  ;"Keep track of changes to variable system table
  ;
  ;"---Display and Interact with User ----------------------
  DO VCUSAV2^TMGTERM
  IF tmgRunMode=2 DO CUP^TMGTERM(1,2) GOTO SP1    ;"2=Don't show code
  ;"**** THIS IS WHERE CODE IS DISPLAYED ***
  DO SHOWCODE^TMGIDE2B(tmgIDEPos,tmgScrWidth,tmgScrHeight,,tmgViewOffset,tmgLROffset,.tmgCsrOnBreakline)
  ;
SP1 ;  
  WRITE tmgBlankLine,!,tmgBlankLine,!
  DO CUU^TMGTERM(2)
  IF tmgRunMode'=1 DO CHKSPEED^TMGIDE2A()   ;"'1=Not stepping mode
  IF tmgRunMode=2 GOTO SP2                  ;"2=Don't show code
  DO CMDPROMPT^TMGIDE2A  ;"<--- DISPLAY PROMPT AND INTERACT WITH USER 
  ;
SP2 ;  
  DO VCULOAD2^TMGTERM  
SPDN1  ;"-- Finish up ----------------------------  
  DO RESTORE ;"Restore preexisting environment  (except $TEST set in SPDN2 below)
SPDN2 ;"Finish up and return to yottadb execution
  SET tmgDbgResult=$SELECT("INTO,into"[tmgStepMode:1,"OVER,over"[tmgStepMode:2,"OUTOF,outof"[tmgStepMode:3,1:1)
  IF tmgdbgTruth ;"This will restore initial value of $TEST
  ;"NOTE!  Don't do any "IF" testing after line above, it will change enviroment for running program
  ;
  DO:($GET(tmgMsg)=1)   
  . DO SETDOLLARZSTEP^TMGIDE1  ;"Setting $ZSTEP is a key core of the debugger.
  . DO SETZSTEPMODE^TMGIDE1(tmgDbgResult) ;
  ;"NOTICE! Avoid putting any code after SETZSTEPMODE above.  It impacts TMGIDE performance.  
  QUIT tmgDbgResult
  ;
  ;"============================================================================
  ;
RESTORE  ;"Restore enviroment
  IF $DATA(tmgDEVSav) DO   ;"turn IO back to what it was when coming into this function.
  . DO RESTORDEV^TMGKERN1(.tmgDEVSav,.tmgDEVInfo)
  ELSE  IF $DATA(tmgSavedIO) DO    ;"turn IO back to what it was when coming into this function.
  . USE tmgSavedIO
  SET $X=+$GET(tmgSavedX),$Y=+$GET(tmgSavedY)  ;"Restore screen POS variables.
  SET %=%tmg
  IF (tmgDbgNakedRef'["""""")&(tmgDbgNakedRef'="") DO   ;"If holds "" index, skip over
  . NEW TEMP SET TEMP=$GET(@tmgDbgNakedRef) ;"restore naked reference.
  QUIT
  ;
INITVARS ;"INIT tmg* vars, used in global scope.  
  IF $ZTRAP'["^TMG" DO SETERRTRAP^TMGIDE2A ;"ensure no redirecting of error trap
  SET tmgDbgResult=1  ;"1=step into, 2=step over  
  SET tmgDbgHangTime=+$GET(tmgDbgHangTime,0.25)
  SET %tmg=$GET(%)  
  IF $GET(tmgRunMode)="" DO  ;"Happens if code clears variable table, e.g. ^XUP
  . SET tmgRunMode=$GET(^TMG("TMGIDE",$J,"RUNMODE"),1) 
  SET tmgStepMode=$GET(tmgStepMode,"into")
  SET tmgDbgRemoteJob=+$GET(tmgDbgRemoteJob)  
  SET tmgDbgJNum=$J
  IF tmgDbgRemoteJob SET tmgDbgJNum=tmgDbgRemoteJob
  SET tmgArrayName=$name(^TMG("TMGIDE",tmgDbgJNum,"MODULES"))
  SET tmgViewOffset=0
  SET tmgSavedIO=$IO
  DO DEV2ARR^TMGKERN1($IO,.tmgDEVSav,,.tmgDEVInfo)
  USE $P  ;"will refine USE later, but for now if IO is set to output file etc, reset to $P so we can query terminal
  SET tmgSavedX=$X,tmgSavedY=$Y
  SET tmgScrHeight=$GET(tmgScrHeight,10)
  SET tmgScrWidth=+$GET(tmgScrWidth)
  IF (tmgScrWidth'>0)!(tmgRunMode=1) DO  ;"If pause after every show, take time to check dimensions.
  . DO RESETKB^XGF  ;"turn off XGF escape key processing code.
  . NEW tmpScrWidth IF $$GETSCRSZ^TMGKERNL(,.tmpScrWidth)  ;"drop function result
  . DO INITKB^XGF()  ;"set up keyboard input escape code processing
  . IF ($GET(tmgScrWidth("USER SET"))=1) QUIT 
  . SET tmgScrWidth=tmpScrWidth
  SET tmgLROffset=$GET(tmgLROffset,0)
  SET tmgBlankLine=" "
  FOR tmgTempI=1:1:tmgScrWidth-1 SET tmgBlankLine=tmgBlankLine_" "
  SET tmgTEMPEVAL=0
  IF tmgRunMode=5 DO  GOTO:(tmgTEMPEVAL=1) IVDN  ;"will effect jump to SPDN1
  . SET tmgTEMPEVAL=$$EVALDW^TMGIDE7(.tmgRunMode)
  USE $P:(WIDTH=tmgScrWidth:NOWRAP)  ;"reset IO to the screen
  SET tmgRelPos=tmgIDEPos
  SET tmgOrigIDEPos=tmgIDEPos
  SET tmgTempPos=$$ConvertPos^TMGMISC(tmgIDEPos,tmgArrayName)
  IF tmgTempPos'="" SET tmgIDEPos=tmgTempPos  
  SET tmgCsrOnBreakline=0
IVDN ;
  QUIT
  ;
DEBUGLOG(MSG) ;
  NEW REF SET REF=$NAME(^TMP($J,"TMGIDE","DEBUGLOG"))
  NEW IDX SET IDX=$ORDER(@REF@("LOG",""),-1)+1
  SET @REF@("LOG",IDX)=MSG
  QUIT
  ;
CONDBKPTS(tmgStpSkip,tmgMsg,tmgStepMode) ;
  SET tmgStpSkip=0
  NEW tmgAtBkPt SET tmgAtBkPt=$$IsBreakpointFlex^TMGIDE2C() 
  IF tmgAtBkPt DO  
  . NEW TESTCODE SET TESTCODE=$$GetBrkCondFlex^TMGIDE2C() IF TESTCODE="" QUIT
  . NEW tmgBkPtTest SET tmgBkPtTest=$$EvalBkPtCode^TMGIDE2C(TESTCODE)
  . IF tmgBkPtTest=1 QUIT  ;"User Test code ==> $TEST=1, so let breakpoint stand, don't continue running full speed 
  . SET tmgStpSkip=1
  . SET tmgMsg=0  ;"ensure $ZSTEP is not turned back on, i.e. continue running.  
  . SET tmgStepMode="DONE"  
  QUIT ;
  ;
