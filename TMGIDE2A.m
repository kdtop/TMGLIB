TMGIDE2A ;TMG/kst/A debugger/tracer for YottaDB (core functionality) ;1/12/25
         ;;1.0;TMG-LIB;**1**;1/12/25
  ;" GT.M  TRAP STEP
  ;
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;"Copyright (c) 1/12/25  Kevin S. Toppenberg MD
  ;"
  ;"This file is part of the TMG LIBRARY, and may only be used in accordence
  ;" to license terms outlined in separate file TMGLICNS.m, which should 
  ;" always be distributed with this file.
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;
  ;"=======================================================================
  ;"=======================================================================
  ;"=======================================================================
  ;" API -- Public Functions.
  ;"=======================================================================
  ;
  ;"=======================================================================
  ;"PRIVATE API FUNCTIONS
  ;"=======================================================================
  ;
  ;"=======================================================================
  ;"=======================================================================
  ;
CMDPROMPT  ;
  ;"Purpose: Display the command prompt, and handle user input
  ;"Note: uses some variables with global scope, because this code block
  ;"     was simply cut out of main routine above.
  ;"Result: None
  IF "41"'[tmgRunMode QUIT  ;"Only interact with user if in stepping mode (1) OR Hopping mode (4)
  NEW $ETRAP SET $ETRAP="SET result="""",$ETRAP="""",$ecode="""""
  NEW tmgDone SET tmgDone=0
  FOR  DO  QUIT:tmgDone=1
  . DO SHOWCODE^TMGIDE2B(tmgIDEPos,tmgScrWidth,tmgScrHeight,,tmgViewOffset,tmgLROffset,.tmgCsrOnBreakline)
  . NEW tmgTempI FOR tmgTempI=1:1:2 WRITE tmgBlankLine,!  ;"create empty space below display.
  . DO CUU^TMGTERM(2)
  . IF tmgCsrOnBreakline=1 DO
  . . NEW TESTCODE SET TESTCODE=$$GetBrkCond^TMGIDE2C($$RelConvertPos^TMGMISC(tmgRelPos,tmgViewOffset,tmgArrayName))
  . . IF TESTCODE="" quit
  . . NEW tmgBkPtTest SET tmgBkPtTest=$$EvalBkPtCode^TMGIDE2C(TESTCODE) 
  . . WRITE "Breakpoint test: [",TESTCODE,"] --> [",tmgBkPtTest,"]",!
  . WRITE "}"
  . DO EVALWATCHES^TMGIDE2B
  . SET $X=1
  . WRITE "Action (? for help): "
  . WRITE "step "_$$UP^XLFSTR(tmgStepMode)_"// "
  . DO CLRLINE^TMGIDE2B
  . IF tmgRunMode=4 set tmgAction="X" 
  . ELSE  SET tmgAction=$$READ^TMGIDE1() WRITE !
  . IF tmgAction="" SET tmgAction=$$UP^XLFSTR($EXTRACT(tmgStepMode,1,1))
  . IF ";MENU;menu;/;"[(";"_tmgAction_";") DO  QUIT:tmgAction["^" 
  . . SET tmgAction=$$HndlMenu^TMGIDE2C() 
  . NEW tmgOrigAction SET tmgOrigAction=tmgAction
  . DO XLTCMDKEYS^TMGIDE2B(.tmgAction,$GET(tmgXGRT),1)
  . SET tmgDone=("RLIHOXTQ"[tmgAction)
  . IF tmgAction="R" DO SETRUNMODE(0) QUIT                                    ;"Run Quickly
  . IF tmgAction="L" DO SETRUNMODE(3) QUIT                                    ;"Run slowly
  . IF tmgAction="H" DO SETRUNMODE(2) QUIT                                    ;"HIDE
  . IF tmgAction="HOP" DO SETRUNMODE(4) QUIT                                  ;"HOPPING
  . IF tmgAction="I" SET tmgStepMode="into" QUIT                              ;"Step INTO
  . IF tmgAction="O" SET tmgStepMode="over" QUIT                              ;"Step OVER
  . IF tmgAction="T" SET tmgStepMode="outof" QUIT                             ;"Step OUTOF
  . IF tmgAction="X" DO HndlDone^TMGIDE2C QUIT                                ;"Turn off debugger (keep running)
  . IF tmgAction="Q" DO HndlQuit^TMGIDE2C QUIT                                ;"Quit from debugger (stop running)
  . IF tmgAction="M" DO HndlMCode^TMGIDE2C QUIT                               ;"Execute M code
  . IF tmgAction="B" DO HndlSetBrk^TMGIDE2C QUIT                              ;"Toggle a breakpoint at current location
  . IF tmgAction="J" DO HndlJmpBrk^TMGIDE2C QUIT                              ;"Toggle a Jump-to breakpoint at current location
  . IF tmgAction="E" DO HndlExpand^TMGIDE2C QUIT                              ;"Expand line
  . IF tmgAction="W" DO HndlWatch^TMGIDE2C(tmgOrigAction) QUIT                ;"Watch
  . IF tmgAction="C" DO HndlCstBrk^TMGIDE2C QUIT                              ;"Custom breakpoint
  . IF tmgAction="G" DO HndlJmpDisp^TMGIDE2C(.tmgIDEPos,.tmgViewOffset) QUIT  ;"Jump to NEW display location
  . IF tmgAction="BC" DO HndlBrkCond^TMGIDE2C QUIT                            ;"Enter a breakpoint condition (IF code)
  . IF $$MOVEKEY(tmgAction) QUIT
  . IF tmgAction="+" SET tmgScrWidth=$GET(tmgScrWidth)+1 QUIT
  . IF tmgAction="-" SET:(tmgScrWidth>10) tmgScrWidth=$GET(tmgScrWidth)-1 QUIT
  . IF tmgAction="=" DO HndlScrWH^TMGIDE2C QUIT
  . IF tmgAction="CLS" WRITE # QUIT
  . IF tmgAction["TABLE" DO HndlTable^TMGIDE2C(tmgOrigAction) QUIT
  . IF tmgAction["SHOW" DO HndlShow^TMGIDE2C QUIT
  . IF tmgAction["SHWo" DO HndlShow^TMGIDE2C QUIT   ;"<-- I keep having fat fingers!!!
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
  . IF tmgAction["VARS" DO HndlVars^TMGIDE2C(tmgOrigAction) QUIT
  . IF tmgAction["RESTART" DO HndlRestart^TMGIDE2C QUIT
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
SETRUNMODE(Value,Option) ;"//kt 1/7/15
  ;"Value: 0=running mode     
  ;"       1=stepping mode
  ;"       2=Don't show code
  ;"       3=running SLOW mode
  ;"       4=Hopping (rapidly jumping between breakpoints)
  ;"       5=Data watch (monitor data watches, don't show code)
  ;"      -1=QUIT
  IF ($GET(Option)=1),($GET(tmgRunMode)=4) QUIT  ;"When hopping, don't drop to step mode
  SET tmgRunMode=Value
  SET ^TMG("TMGIDE",$J,"RUNMODE")=tmgRunMode 
  QUIT
  ;
CHKSPEED() ;
  WRITE tmgBlankLine,!
  DO CUU^TMGTERM(1)
  DO EVALWATCHES^TMGIDE2B
  WRITE "(Press any key to pause"
  IF "34"[tmgRunMode WRITE "; '+' for faster, '-' for slower)",!
  ELSE  WRITE ")",!
  READ *tmgKeyIn:0
  IF "34"[tmgRunMode DO
  . IF tmgKeyIn=43 SET tmgDbgHangTime=tmgDbgHangTime/2,tmgKeyIn=0       ;"43= '+'
  . ELSE  IF tmgKeyIn=45 SET tmgDbgHangTime=tmgDbgHangTime*2,tmgKeyIn=0 ;"45= '-'
  . HANG tmgDbgHangTime
  IF (tmgKeyIn>0) DO
  . DO SETRUNMODE^TMGIDE2A(1)
  QUIT
  ;  
SETERRTRAP()  ;  
  SET $ZTRAP="DO ERRTRAP^TMGIDE2A($ZPOS) BREAK"
  SET $ZSTATUS=""
  QUIT
  ;
ERRTRAP(tmgIDEPos)  ;
  ;"Purpose: This is the line that is called by GT.M for each ztrap event.
  ;"      It will be used to display the current code execution point
  ;
  ;"NOTE: tmgERR() is setup by $ETRAP in PROMPTLAUNCH^TMGIDE and TOPERRHNDL^TMGIDE
  ;"      Line below get correct error position.
  IF $DATA(tmgERR("ZP",1)) SET tmgIDEPos=tmgERR("ZP",1)  
  IF $$ShouldSkip^TMGIDE2C($PIECE(tmgIDEPos,"^",2)) DO
  . USE $P  
  . WRITE !,"Error at ",$P($ZSTATUS,",",2)," -- in code that debugger can't display.",!
  . WRITE "Error is: ",$P($ZSTATUS,",",3,99),!
  . WRITE !,"Dropping to command line via BREAK",!
  . BREAK
  SET tmgScrHeight=$GET(tmgScrHeight,10)
  SET tmgScrWidth=$GET(tmgScrWidth,70)    
  NEW tmgDEVSav,tmgDEVInfo DO DEV2ARR^TMGKERN1($IO,.tmgDEVSav,,.tmgDEVInfo) 
  USE $P   
  DO VCUSAV2^TMGTERM
  WRITE #  ;"clear screen to better show error messages and crash info. 
  DO SHOWCODE^TMGIDE2B(tmgIDEPos,tmgScrWidth,tmgScrHeight,0,0,-3)
  WRITE !,"-------------------------------------------",!
  WRITE "TRAPPING ERROR in ERRTRAP^TMGIDE2A...",!,"To display source code of error location",!
  IF 1=0 DO
  . NEW % FOR %=0:1:$STACK DO
  . . W (%+1),".  ",$STACK(%)," -- ",$STACK(%,"PLACE"),": ",$STACK(%,"MCODE")," - ",$STACK(%,"ECODE"),!
  WRITE "-------------------------------------------",!
  ;  
ETDone ;
  ;"DO VCULOAD2^TMGTERM
  IF $DATA(tmgDEVSav) DO   ;"turn IO back to what it was when coming into this function.
  . DO RESTORDEV^TMGKERN1(.tmgDEVSav,.tmgDEVInfo)
  QUIT
  ;  