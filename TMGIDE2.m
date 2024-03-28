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
  ;" SET $ZSTEP="DO STEPTRAP^TMGIDE2($ZPOS) ZSTEP INTO ZCONTINUE"
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
  ;"ERRTRAP(tmgIDEPos)
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
  NEW tmgdbgTruth SET tmgdbgTruth=$TEST   ;"save initial value of $TEST
  IF $DATA(tmgDbgJumpToBrkPos) DO  
  . DO RelBreakpoint^TMGIDE2C(tmgDbgJumpToBrkPos)
  . KILL tmgDbgJumpToBrkPos
  IF $ZTRAP'["^TMG" DO SETERRTRAP^TMGIDE  ;"ensure no redirecting of error trap
  NEW tmgDbgResult SET tmgDbgResult=1  ;"1=step into, 2=step over
  NEW tmgDbgNakedRef SET tmgDbgNakedRef=$$LGR^%ZOSV ;"save naked reference
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
  SET tmgLROffset=$GET(tmgLROffset,0)
  IF tmgRunMode'=5 GOTO SP2
  NEW tmgTEMPEVAL SET tmgTEMPEVAL=$$EVALDW^TMGIDE7(.tmgRunMode)
  IF tmgTEMPEVAL=1 GOTO SPDN
SP2 ;
  USE $P:(WIDTH=tmgScrWidth:NOWRAP)  ;"reset IO to the screen
  SET tmgBlankLine=" "
  FOR tmgTempI=1:1:tmgScrWidth-1 SET tmgBlankLine=tmgBlankLine_" "
  NEW tmgRelPos SET tmgRelPos=tmgIDEPos
  NEW tmgOrigIDEPos SET tmgOrigIDEPos=tmgIDEPos
  NEW tmgTempPos SET tmgTempPos=$$ConvertPos^TMGMISC(tmgIDEPos,tmgArrayName)
  IF tmgTempPos'="" SET tmgIDEPos=tmgTempPos
  ;"Don't show hidden modules (setup in TMGIDE module)
  IF $$ShouldSkip^TMGIDE2C($PIECE(tmgIDEPos,"^",2)) GOTO SPDN
  ;"Record trace, If not a hidden module
  IF +$GET(tmgDbgOptions("TRACE"))=1 DO RecordTrace^TMGIDE6(tmgOrigIDEPos)
  ;
  ;"-- Conditional Breakpoints-- 
  NEW tmgStpSkip SET tmgStpSkip=0
  NEW zzAtBkPt set zzAtBkPt=$$IsBreakpointFlex^TMGIDE2C() 
  IF zzAtBkPt DO  GOTO:(tmgStpSkip=1) SPDN
  . NEW ifS SET ifS=$$GetBrkCondFlex^TMGIDE2C() IF ifS="" QUIT
  . NEW zzBkPtTest SET zzBkPtTest=$$EvalBkPtCode^TMGIDE2C(ifS)
  . IF zzBkPtTest=1 QUIT  ;"User Test code ==> $TEST=1, so let breakpoint stand, don't continue running full speed 
  . SET tmgStpSkip=1
  . SET tmgMsg=0  ;"ensure $ZSTEP is not turned back on, i.e. continue running.  
  . SET tmgStepMode="DONE"
  ;
  DO VCUSAV2^TMGTERM
  NEW tmgCsrOnBreakline SET tmgCsrOnBreakline=0
  IF tmgRunMode'=2 DO  ;"2=Don't show code
  . DO SHOWCODE^TMGIDE2B(tmgIDEPos,tmgScrWidth,tmgScrHeight,,tmgViewOffset,tmgLROffset,.tmgCsrOnBreakline)
  ELSE  DO
  . DO CUP^TMGTERM(1,2)
  WRITE tmgBlankLine,!
  WRITE tmgBlankLine,!
  DO CUU^TMGTERM(2)
  IF tmgRunMode'=1 DO  ;"Not stepping mode
  . WRITE tmgBlankLine,!
  . DO CUU^TMGTERM(1)
  . DO EVALWATCHES^TMGIDE2C
  . WRITE "(Press any key to pause"
  . IF "34"[tmgRunMode WRITE "; '+' for faster, '-' for slower)",!
  . ELSE  WRITE ")",!
  . READ *tmgKeyIn:0
  . IF "34"[tmgRunMode DO
  . . IF tmgKeyIn=43 SET tmgDbgHangTime=tmgDbgHangTime/2,tmgKeyIn=0       ;"43= '+'
  . . ELSE  IF tmgKeyIn=45 SET tmgDbgHangTime=tmgDbgHangTime*2,tmgKeyIn=0 ;"45= '-'
  . . HANG tmgDbgHangTime
  . IF (tmgKeyIn>0) DO
  . . DO SETRUNMODE(1)
  IF tmgRunMode'=2 DO  ;"2=Don't show code
  . DO CMDPROMPT ;"display prompt and interact with user
  DO VCULOAD2^TMGTERM
  ;
SPDN ;"Finish up and return to GTM execution
  IF "INTO,into"[tmgStepMode SET tmgDbgResult=1
  IF "OVER,over"[tmgStepMode SET tmgDbgResult=2
  IF "OUTOF,outof"[tmgStepMode SET tmgDbgResult=3
  ;
  IF $GET(tmgMsg)=1 DO  ;"call was without $ZSTEP set, so we should SET it.
  . NEW CODE SET CODE="NEW tmgTrap "
  . SET CODE=CODE_"SET tmgTrap=$$STEPTRAP^TMGIDE2($ZPOS) "
  . SET CODE=CODE_"ZSTEP:(tmgTrap=1) INTO ZSTEP:(tmgTrap=2) OVER ZSTEP:(tmgTrap=3) OUTOF "
  . SET CODE=CODE_"ZCONTINUE"         
  . SET $ZSTEP=CODE
  . ZSTEP:(tmgDbgResult=1) INTO
  . ZSTEP:(tmgDbgResult=2) OVER
  . ZSTEP:(tmgDbgResult=3) OUTOF
  ;
  ;"Restore environment
  IF $DATA(tmgDEVSav) DO   ;"turn IO back to what it was when coming into this function.
  . DO RESTORDEV^TMGKERN1(.tmgDEVSav,.tmgDEVInfo)
  ELSE  IF $DATA(tmgSavedIO) DO    ;"turn IO back to what it was when coming into this function.
  . USE tmgSavedIO
  SET $X=+$GET(tmgSavedX),$Y=+$GET(tmgSavedY)  ;"Restore screen POS variables.
  SET %=%tmg
  IF (tmgDbgNakedRef'["""""")&(tmgDbgNakedRef'="") DO   ;"If holds "" index, skip over
  . NEW TEMP SET TEMP=$GET(@tmgDbgNakedRef) ;"restore naked reference.
  IF tmgdbgTruth ;"This will restore initial value of $TEST
  ;
  QUIT tmgDbgResult
  ;
 ;"============================================================================
SETRUNMODE(Value,Option) ;"//kt 1/7/15
  IF ($GET(Option)=1),($GET(tmgRunMode)=4) QUIT  ;"When hopping, don't drop to step mode
  SET tmgRunMode=Value
  SET ^TMG("TMGIDE",$J,"RUNMODE")=tmgRunMode 
  QUIT
  ;
CMDPROMPT  ;
  ;"Purpose: Display the command prompt, and handle user input
  ;"Note: uses some variables with global scope, because this code block
  ;"     was simply cut out of main routine above.
  ;"Result: None
  IF "41"'[tmgRunMode QUIT  ;"Only interact with user if in stepping mode (1)
  NEW $ETRAP SET $ETRAP="SET result="""",$ETRAP="""",$ecode="""""
  NEW tmgDone SET tmgDone=0
  FOR  DO  QUIT:tmgDone=1
  . DO SHOWCODE^TMGIDE2B(tmgIDEPos,tmgScrWidth,tmgScrHeight,,tmgViewOffset,tmgLROffset,.tmgCsrOnBreakline)
  . NEW tmgTempI FOR tmgTempI=1:1:2 WRITE tmgBlankLine,!  ;"create empty space below display.
  . DO CUU^TMGTERM(2)
  . IF tmgCsrOnBreakline=1 DO
  . . NEW ifS SET ifS=$$GetBrkCond^TMGIDE2C($$RelConvertPos^TMGMISC(tmgRelPos,tmgViewOffset,tmgArrayName))
  . . IF ifS="" quit
  . . NEW zzBkPtTest SET zzBkPtTest=$$EvalBkPtCode^TMGIDE2C(ifS) 
  . . WRITE "Breakpoint test: [",ifS,"] --> [",zzBkPtTest,"]",!
  . WRITE "}"
  . DO EVALWATCHES^TMGIDE2B
  . SET $X=1
  . WRITE "Action (? for help): "
  . WRITE "step "_$$UP^XLFSTR(tmgStepMode)_"// "
  . DO CLRLINE^TMGIDE2B
  . IF tmgRunMode=4 set tmgAction="X" 
  . ELSE  SET tmgAction=$$READ^TMGIDE() WRITE !
  . IF tmgAction="" SET tmgAction=$$UP^XLFSTR($EXTRACT(tmgStepMode,1,1))
  . NEW tmgOrigAction SET tmgOrigAction=tmgAction
  . DO XLTCMDKEYS^TMGIDE2B(.tmgAction,$GET(tmgXGRT),1)
  . SET tmgDone=("RLIHOXTQ"[tmgAction)
  . IF tmgAction="R" DO SETRUNMODE(0) QUIT         ;"Run Quickly
  . IF tmgAction="L" DO SETRUNMODE(3) QUIT         ;"Run slowly
  . IF tmgAction="H" DO SETRUNMODE(2) QUIT         ;"HIDE
  . IF tmgAction="HOP" DO SETRUNMODE(4) QUIT       ;"HOPPING
  . IF tmgAction="I" SET tmgStepMode="into" QUIT   ;"Step INTO
  . IF tmgAction="O" SET tmgStepMode="over" QUIT   ;"Step OVER
  . IF tmgAction="T" SET tmgStepMode="outof" QUIT  ;"Step OUTOF
  . IF tmgAction="X" DO HndlDone^TMGIDE2C QUIT              ;"Turn off debugger (keep running)
  . IF tmgAction="Q" DO HndlQuit^TMGIDE2C QUIT              ;"Quit from debugger (stop running)
  . IF tmgAction="M" DO HndlMCode^TMGIDE2C QUIT             ;"Execute M code
  . IF tmgAction="B" DO HndlSetBrk^TMGIDE2C QUIT            ;"Toggle a breakpoint at current location
  . IF tmgAction="J" DO HndlJmpBrk^TMGIDE2C QUIT            ;"Toggle a Jump-to breakpoint at current location
  . IF tmgAction="E" DO HndlExpand^TMGIDE2C QUIT            ;"Expand line
  . IF tmgAction="W" DO HndlWatch^TMGIDE2C(tmgOrigAction) QUIT    ;"Watch
  . IF tmgAction="C" DO HndlCstBrk^TMGIDE2C QUIT            ;"Custom breakpoint
  . IF tmgAction="G" DO HndlJmpDisp^TMGIDE2C(.tmgIDEPos,.tmgViewOffset) QUIT  ;"Jump to NEW display location
  . IF tmgAction="BC" DO HndlBrkCond^TMGIDE2C QUIT          ;"Enter a breakpoint condition (IF code)
  . IF $$MOVEKEY(tmgAction) QUIT
  . IF tmgAction="+" SET tmgScrWidth=$GET(tmgScrWidth)+1 QUIT
  . IF tmgAction="-" SET:(tmgScrWidth>10) tmgScrWidth=$GET(tmgScrWidth)-1 QUIT
  . IF tmgAction="=" DO HndlScrWH^TMGIDE2C QUIT
  . IF tmgAction="CLS" WRITE # QUIT
  . IF tmgAction["TABLE" DO HndlTable^TMGIDE2C QUIT
  . IF tmgAction["SHOW" DO HndlShow^TMGIDE2C QUIT
  . IF tmgAction["ZWR" DO HndlZWR^TMGIDE2C QUIT
  . IF tmgAction["BROWSE" DO HndlBrowse^TMGIDE2C QUIT
  . IF tmgAction["NODES" DO HndlNodes^TMGIDE2C QUIT
  . IF tmgAction["STACK" DO HndlStack^TMGIDE2C(.tmgIDEPos,.tmgViewOffset) QUIT
  . IF tmgAction["RESYNC" KILL @tmgArrayName QUIT
  . IF tmgAction["HIDE" DO SetupSkips^TMGIDE2C QUIT
  . IF tmgAction["FULL" DO FULL^VALM1,INITKB^XGF() QUIT
  . IF tmgAction["UCASE" DO HndlToggleMode^TMGIDE2C("UCASE") QUIT
  . IF tmgAction["LCASE" DO HndlToggleMode^TMGIDE2C("LCASE") QUIT
  . IF tmgAction["XCMD" DO HndlToggleMode^TMGIDE2C("XCMD") QUIT
  . IF tmgAction["SCMD" DO HndlToggleMode^TMGIDE2C("SCMD") QUIT
  . IF tmgAction["TRACE" DO ShowTrace^TMGIDE6 QUIT
  . IF tmgAction["TVDIFF" DO HndlToggleMode^TMGIDE2C("VARTRACE") QUIT
  . IF tmgAction["VDIFF" DO ShowVTrace^TMGIDE6 QUIT
  . IF tmgAction["COLORS" DO EditColors^TMGIDE6 QUIT
  . IF tmgAction["DBK" DO DelBreaks^TMGIDE6 QUIT
  . IF tmgAction["INITKB" DO INITKB^XGF() QUIT  ;"set up keyboard input escape code processing
  . IF tmgAction["RDW" DO HndlRunDW^TMGIDE2C SET tmgDone=1 QUIT
  . IF tmgAction["VARS" DO HndlVars^TMGIDE2C(tmgOrigAction) SET tmgDone=1 QUIT
  . ELSE  DO HndlHelp^TMGIDE2C QUIT
  QUIT
  ;
MOVEKEY(tmgAction)  ;
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
ERRTRAP(tmgIDEPos)  ;
   ;"Purpose: This is the line that is called by GT.M for each ztrap event.
   ;"      It will be used to display the current code execution point
  IF $$ShouldSkip^TMGIDE2C($PIECE(tmgIDEPos,"^",2)) DO
  . USE $P  ;"//kt 8/10/22
  . WRITE !,"Error at ",$P($ZSTATUS,",",2)," -- in code that debugger can't display.",!
  . WRITE "Error is: ",$P($ZSTATUS,",",3,99),!
  . WRITE !,"Dropping to command line via BREAK",!
  . BREAK
  ;"NEW tmgScrHeight,tmgScrWidth
  SET tmgScrHeight=$GET(tmgScrHeight,10)
  SET tmgScrWidth=$GET(tmgScrWidth,70)
  DO VCUSAV2^TMGTERM
  DO SHOWCODE^TMGIDE2B(tmgIDEPos,tmgScrWidth,tmgScrHeight,0)
ETDone ;
  DO VCULOAD2^TMGTERM
  QUIT
  ;