TMGIDE2C ;TMG/kst/A debugger/tracer for YottaDB (Utility functions) ;3/21/24
         ;;1.0;TMG-LIB;**1**;3/21/2024
  ;" GT.M  TRAP STEP
  ;
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;"Copyright (c) 3/21/24  Kevin S. Toppenberg MD
  ;"
  ;"This file is part of the TMG LIBRARY, and may only be used in accordence
  ;" to license terms outlined in separate file TMGLICNS.m, which should 
  ;" always be distributed with this file.
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;
  ;"=======================================================================
  ;"=======================================================================
  ;
  ;"=======================================================================
  ;"PRIVATE API FUNCTIONS
  ;"=======================================================================
  ;"HndlMCode -- Handle option to execute arbitrary code.
  ;"HndlShow -- Handle option to show a variable.
  ;"HndlZWR  --  Handle option to ZWRITE a variable.  
  ;"HndlToggleMode(Mode) -- Toggle UCASE or LCASE in Options
  ;"HndlWatch(tmgAction) --Handle option to add watch
  ;"HndlHop --Handle Hopping mode  
  ;"HndlQuit --To create a crash, so can QUIT debugger,
  ;"HndlDone -- To turn off the debugger, allowing program to continue full speed.
  ;"HndlScrWH --Handle option to SET screen width and height
  ;"HndlExpand -- handle option to expand one mumps like of code.
  ;"HndlStack(ShowPos,tmgViewOffset) --Handle option to show and interact with stack.
  ;"GetStackInfo(Stack,ExecPos) -- query GTM and get back filtered Stack information
  ;"HndlNodes -- Handle option to browse a variable by nodes.
  ;"HndlBrowse -- Handle option to browse a variable.
  ;"HndlBrkCond --Handle option to browse conditional break
  ;"HndlCstBrk --Set a custom breakpoint
  ;"HndlSetBrk -- Set breakpoint at current point
  ;"HndlJmpBrk -- Set jump-to breakpoint at current point  
  ;"BrkPtDelta(List) -- Determine which breakpoint currently exists, but is not in passed list
  ;"EvalBkPtCode(zzCode) -- Evaluate Breakpoint code
  ;"ToggleBreakpoint(pos,condition) -- SET or release the GT.M breakpoint at position
  ;"IsBreakpoint(pos) -- determine IF position is a breakpoint pos
  ;"EnsureBreakpoints() -- Ensure breakpoints after module recompiled.
  ;"SETBKPT(pos,condition) -- SET the GT.M breakpoint to pos position
  ;"SetBrkCond(pos,condition) -- A standardized SET for condition.
  ;"GetBrkCond(pos) -- A standardized GET for condition.
  ;"HndlTable -- Handle option for Table command
  ;"HndlVars(tmgOrigAction) -- Handle option for VARS command  
  ;"HndlJmpDisp(ShowPos,tmgViewOffset) -- allow user to enter in a location to show in code displayer
  ;"HndlRunDW -- Handle resuming or starting Data Watch session mode.  
  ;"HndlHelp --Handle option for help.
  ;"HlpWrite(line) ;
  ;"SETCOLORS(mode) -- SET colors in central location
  ;"Box -- Draw a box on the top of the screen.
  ;"RelBreakpoint(pos) -- release a  GT.M breakpoint at position
  ;"ShouldSkip(module) -- see IF module is in hidden list
  ;"SetupSkips --  manage modules that are to be skipped over.
  ;"AddSkip -- allow user to Add a module to hidden list
  ;"RmSkip -- allow user to remove a module from hidden list
  ;"ShowSkip -- show the hidden list
  ;"GETSEC() -- GET SYSTEM SECONDS
  ;"MAX(A,B) -- Max of A vs B  
  ;"MessageOut() -- when in remote-control debugging mode, send a message to SENDER
  ;"GetRemoteVar(varName) -- Pass varName to remote process, have it evaluated there, and passed back back here for display.
  ;"RemoteXecute(MCode) -- Pass M Code to remote process for execution there.
  ;"ExpandLine(Pos) -- expand a line of code, found at position 'Pos', using ^XINDX8 functionality
  ;"DEBUGWRITE(INDENT,STR,ADDLF)
  ;"DEBUGINDENT(INDENT,FORCED,OUTREF)
  ;"$$ARRDUMP(ArrayP,TMGIDX,indent)
  ;
  ;"=======================================================================
  ;" Dependencies:
  ;"   Uses: ^TMGTERM,^TMGIDE
  ;"=======================================================================
  ;
HndlMCode ;
  ;"Purpose: Handle option to execute arbitrary code.
  ;"//kt 10/2024 -- DO CUU^TMGTERM(1)
  ;"//kt 10/2024 -- DO CHA^TMGTERM(1) ;"move to x=1 on this line
  ;"//kt 10/2024 -- WRITE tmgBlankLine,!
  ;"//kt 10/2024 -- DO CUU^TMGTERM(1)
  NEW TEMPX,TEMPY SET TEMPX=$X,TEMPY=$Y
  DO ALTBUF^TMGTERM(1)  ;"//kt 10/22/24   
  DO SETCOLORS("NORM")  ;"//kt 10/22/24
  DO CUP^TMGTERM(1,2)   ;"//kt 10/22/24
  NEW tmgTpLine SET tmgTpLine=$$Trim^TMGSTUTL($PIECE(tmgOrigAction," ",2,999))
  IF tmgTpLine="" READ " enter M code (^ to cancel): ",tmgTpLine:$GET(DTIME,3600),!
  IF (tmgTpLine'="^") DO
  . IF +$GET(tmgDbgRemoteJob) DO RemoteXecute(tmgTpLine) QUIT
  . NEW $ETRAP SET $ETRAP="WRITE ""(Invalid M Code!.  Error Trapped.)"",! SET $ETRAP="""",$ecode="""""
  . WRITE !  ;"get below bottom line for output.
  . XECUTE tmgTpLine
  DO PRESS2GO^TMGUSRI2  ;"//kt 10/22/24
  DO ALTBUF^TMGTERM(0)  ;"//kt 10/22/24   
  QUIT
  ;
HndlShow ;
  ;"Purpose: Handle option to show a variable.
  DO ALTBUF^TMGTERM(1)  ;"//kt 10/22/24    ;"DO Box
  DO SETCOLORS("NORM")
  DO CUP^TMGTERM(1,2) ;"Cursor to line (1,2)
  NEW tmgVarName SET tmgVarName=$$Trim^TMGSTUTL($EXTRACT(tmgOrigAction,5,999))
  IF +$GET(tmgDbgRemoteJob) SET tmgVarName=$$GetRemoteVar^TMGIDE2(tmgVarName)
  ;"WRITE !   ;"get below bottom line for output.
  DO SHOWVAR(tmgVarName)  ;"//kt 11/17/24
  ;"//kt 11/17/24   NEW zbTemp SET zbTemp=0
  ;"//kt 11/17/24   IF tmgVarName["$" DO
  ;"//kt 11/17/24   . NEW tempCode
  ;"//kt 11/17/24   . NEW $ETRAP SET $ETRAP="WRITE ""(Invalid M Code!.  Error Trapped.)"",! SET $ETRAP="""",$ecode="""""
  ;"//kt 11/17/24   . WRITE tmgVarName,"='"
  ;"//kt 11/17/24   . SET tempCode="do DEBUGWRITE^TMGIDE2C(1,"_tmgVarName_")"
  ;"//kt 11/17/24   . xecute tempCode
  ;"//kt 11/17/24   . WRITE "'    "
  ;"//kt 11/17/24   ELSE  IF tmgVarName'="" DO
  ;"//kt 11/17/24   . SET tmgVarName=$$CREF^DILF(tmgVarName)  ;"convert open to closed format
  ;"//kt 11/17/24   . IF $$ARRDUMP(tmgVarName)  ;"//kt 3/23/24,  ignore result
  ;"//kt 11/17/24   IF zbTemp=0 DO
  ;"//kt 11/17/24   . ;"//kt 10/22/24  DO SETCOLORS("Highlight")
  ;"//kt 11/17/24   . DO PRESS2GO^TMGUSRI2
  DO SETCOLORS("Reset")
  DO ALTBUF^TMGTERM(0)  ;"//kt 10/22/24
  QUIT
  ;
HndlZWR  ;
  ;"Purpose: Handle option to ZWRITE a variable.
  DO ALTBUF^TMGTERM(1)  ;"//kt 10/22/24    ;"DO Box
  DO SETCOLORS("NORM")
  DO CUP^TMGTERM(1,2) ;"Cursor to line (1,2)
  ;"NEW tmgVarName SET tmgVarName=$$Trim^TMGSTUTL($EXTRACT(tmgOrigAction,5,999))
  NEW tmgVarName SET tmgVarName=$$Trim^TMGSTUTL($EXTRACT(tmgOrigAction,5,999))
  IF +$GET(tmgDbgRemoteJob) SET tmgVarName=$$GetRemoteVar^TMGIDE2(tmgVarName)
  DO SHOWVAR(tmgVarName,"ZWR")  ;"//kt 11/17/24
  ;"//kt 11/17/24   WRITE !   ;"get below bottom line for output.
  ;"//kt 11/17/24   NEW zbTemp SET zbTemp=0
  ;"//kt 11/17/24   IF tmgVarName["$" DO
  ;"//kt 11/17/24   . NEW tempCode
  ;"//kt 11/17/24   . NEW $ETRAP SET $ETRAP="WRITE ""(Invalid M Code!.  Error Trapped.)"",! SET $ETRAP="""",$ecode="""""
  ;"//kt 11/17/24   . WRITE tmgVarName,"='"
  ;"//kt 11/17/24   . SET tempCode="do DEBUGWRITE^TMGIDE2C(1,"_tmgVarName_")"
  ;"//kt 11/17/24   . XECUTE tempCode
  ;"//kt 11/17/24   . WRITE "'    "
  ;"//kt 11/17/24   ELSE  IF tmgVarName'="" DO
  ;"//kt 11/17/24   . SET tmgVarName=$$CREF^DILF(tmgVarName)  ;"convert open to closed format
  ;"//kt 11/17/24   . DO ZWRITE^TMGZWR(tmgVarName)  ;"ZWRITE @tmgVarName
  ;"//kt 11/17/24   IF zbTemp=0 DO
  ;"//kt 11/17/24   . ;"//kt 10/22/24  DO SETCOLORS("Highlight")
  ;"//kt 11/17/24   . DO PRESS2GO^TMGUSRI2
  DO SETCOLORS("Reset")
  DO ALTBUF^TMGTERM(0)  ;"//kt 10/22/24
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
  . . SET watchVar=$$Trim^TMGSTUTL($PIECE(tmgOrigAction,"+",2))
  . . IF watchVar="" QUIT
  . . IF watchVar="^" SET watchVar="tmgDbgNakedRef"
  . . SET tmgDgbWatches(watchVar)=""
  . . IF watchVar="*" WRITE "Watching variable CHANGES"
  . ELSE  IF (tmgAction["-") DO
  . . SET watchVar=$$Trim^TMGSTUTL($PIECE(tmgOrigAction,"-",2))
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
  . . . SET tmgWatchLine=tmgWatchLine_" WRITE """_v_" =["",$$NOHIDDENSTR^TMGIDE2C($GET("_v_")),""], """
  ELSE  IF (tmgAction["@") DO
  . SET tmgWatchLine=""
  ELSE  DO
  . KILL tmgDgbWatches
  . NEW tempCode
  . READ "Enter M code (^ to cancel): ",tempCode:$GET(DTIME,3600),!
  . IF tempCode'="^" SET tmgWatchLine=tempCode
  QUIT
  ;
HndlHop  ;"Handle Hopping mode
  DO SETRUNMODE^TMGIDE2(4) 
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
  XECUTE "WRITE CrashNonVariable"
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
  DO SHOWCODE^TMGIDE2B(tmgIDEPos,tmgScrWidth,tmgScrHeight,,tmgViewOffset,tmgLROffset,.tmgCsrOnBreakline) ;"<---- not working!
  QUIT
  ;
HndlExpand ;
  ;"Purpose: handle option to expand one mumps like of code.
  NEW expPos,zbLabel,zbOffset,zbRoutine
  DO ParsePos^TMGMISC(tmgIDEPos,.zbLabel,.zbOffset,.zbRoutine)
  SET expPos=zbLabel_"+"_+(zbOffset+tmgViewOffset)_"^"_zbRoutine
  WRITE !
  DO ExpandLine(expPos)
  NEW tempKey READ "        --- Press Enter To Continue--",tempKey:$GET(DTIME,3600)
  QUIT
  ;
HndlStack(ShowPos,tmgViewOffset) ;
  ;"Purpose: Handle option to show and interact with stack.
  ;"Input: ShowPos -- OPTIONAL.  PASS BY REFERENCE.  Will be changed to user selected value.
  ;"  tmgViewOffset -- OPTIONAL.  PASS BY REFERENCE.  Will be changed to 0 IF user selects NEW Pos.
  ;"Globally scoped vars used: tmgOrigIDEPos
  DO ALTBUF^TMGTERM(1)  ;"//kt 10/22/24    
  ;"//kt 10/22/24 WRITE !   ;"get below bottom line for output.
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
  ;"//kt 10/22/24  WRITE # ;"clr screen.
  DO ALTBUF^TMGTERM(0)  ;"//kt 10/22/24
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
HndlNodes ;
  ;"Purpse: Handle option to browse a variable by nodes.
  DO ALTBUF^TMGTERM(1)  ;"//kt 10/22/24   
  DO SETCOLORS("NORM")  ;"//kt 10/22/24
  DO CUP^TMGTERM(1,2)   ;"//kt 10/22/24
  NEW varName SET varName=$$Trim^TMGSTUTL($EXTRACT(tmgOrigAction,7,999))
  WRITE !   ;"get below bottom line for output.
  DO BRWSASK2^TMGMISC
  DO PRESS2GO^TMGUSRI2  ;"//kt 10/22/24
  DO ALTBUF^TMGTERM(0)  ;"//kt 10/22/24   
  QUIT
  ;
HndlBrowse ;
  ;"Purpose: Handle option to browse a variable.
  NEW varName SET varName=$$Trim^TMGSTUTL($EXTRACT(tmgOrigAction,7,999))
  WRITE !   ;"get below bottom line for output.
  DO ALTBUF^TMGTERM(1)  ;"//kt 10/22/24   
  DO SETCOLORS("NORM")  ;"//kt 10/22/24
  DO CUP^TMGTERM(1,2)   ;"//kt 10/22/24
  DO BRWSNOD2^TMGMISC(varName)
  DO ALTBUF^TMGTERM(0)
  QUIT
  ;
HndlBrkCond ;
  ;"Purpose: Handle option to browse conditional break
  NEW tmgTpLine
  WRITE "Enter CODE to set $TEST condition.  Examples: 'IF A=1'  or 'IF $$FN1^MOD(A)=2'",!
  WRITE "If execution result=1, breakpoint will stop execution.  Otherwise skips.",!
  ;"READ "Enter IF condition (^ to cancel, @ to delete): ",tmgTpLine:$GET(DTIME,3600),!
  WRITE "Enter IF condition (^ to cancel, @ to delete): ",!
  NEW ifS SET ifS=$$GetBrkCondFlex^TMGIDE2C()
  SET tmgTpLine=$$EDITBOX^TMGUSRI6(ifS,80,"_")
  IF (tmgTpLine["^") QUIT
  NEW brkPos SET brkPos=$$RelConvertPos^TMGMISC(tmgRelPos,tmgViewOffset,tmgArrayName)
  DO SetBrkCond^TMGIDE(brkPos,tmgTpLine)
  QUIT
  ;
HndlCstBrk ;
  ;"Purpose: Set a custom breakpoint
  NEW brkPos
  READ !,"Enter breakpoint (e.g. Label+8^MyFunct): ",brkPos:$GET(DTIME,3600),!
  DO SETBKPT(brkPos)
  QUIT
  ;
HndlSetBrk ;
  ;"Purpose: Set breakpoint at current point
  ;"WRITE !,"Trying to determine correct breakpoint.  tmgRelPos=",tmgRelPos," tmgViewOffset=",tmgViewOffset,!
  NEW brkPos SET brkPos=$$RelConvertPos^TMGMISC(tmgRelPos,tmgViewOffset,tmgArrayName)
  ;"WRITE "brkPos=",brkPos,!
  IF brkPos="" WRITE "tmgRelPos=",tmgRelPos," view offset=",tmgViewOffset," tmgArrayName=",tmgArrayName,!
  DO ToggleBreakpoint(brkPos)
  QUIT
  ;
HndlJmpBrk ;
  ;"Purpose: Set jump-to breakpoint at current point
  NEW BrkPtList DO GetGTMBrkPts^TMGIDE6(.BrkPtList)  ;"get current list of breakpoints
  DO HndlSetBrk ;"set breakpoint at current cursor position
  NEW AddedBrkPos SET AddedBrkPos=$$BrkPtDelta(.BrkPtList)
  IF AddedBrkPos="" GOTO HJBDN
  SET tmgDbgJumpToBrkPos=AddedBrkPos
  SET tmgDone=1
  GOTO HndlDone  ;"Should turn off debugger until breakpoint reached. 
HJBDN ;
  QUIT  
  ;
ExpandLine(Pos)  ;
  ;"Purpose: to expand a line of code, found at position "Pos", using ^XINDX8 functionality
  ;"Input: Pos: a position as returned by $ZPOS (e.g. G+5^DIS, or +23^DIS)
  ;"Output: Writes to the currently selecte IO device and expansion of one line of code
  ;"Note: This is used for taking the very long lines of code, as found in Fileman, and
  ;"      convert them to a format with one command on each line.
  ;"      Note: it appears to DO syntax checking and shows ERROR IF syntax is not per VA
  ;"      conventions--such as commands must be UPPERCASE  etc.
  ;"--- copied and modified from XINDX8.m ---
  KILL ^UTILITY($J)
  NEW label,offset,RTN,dmod
  DO ParsePos^TMGMISC(Pos,.label,.offset,.RTN,.dmod)
  IF label'="" DO  ;"change position from one relative to label into one relative to top of file
  . NEW CodeArray
  . SET Pos=$$CONVERTPOS^TMGMISC(Pos,"CodeArray")
  . DO ParsePos^TMGMISC(Pos,.label,.offset,.RTN,.dmod)
  IF RTN="" GOTO ELDone
  NEW IND
  DO BUILD^XINDX7
  SET ^UTILITY($J,RTN)=""
  DO LOAD^XINDEX
  SET CCN=0
  DO
  . NEW I
  . FOR I=1:1:+^UTILITY($J,1,RTN,0,0) SET CCN=CCN+$L(^UTILITY($J,1,RTN,0,I,0))+2
  . SET ^UTILITY($J,1,RTN,0)=CCN
  ;"DO ^XINDX8  -- included below
  NEW Q,DDOT,LO,PG,LIN,ML,IDT
  NEW tIOSL SET tIOSL=IOSL
  SET IOSL=999999  ;"really long 'page length' prevents header printout (and error)
  SET Q=""""
  SET DDOT=0
  SET LO=0
  SET PG=+$G(PG)
  SET LC=offset
  IF $D(^UTILITY($J,1,RTN,0,LC)) DO
  . SET LIN=^(LC,0),ML=0,IDT=10
  . SET LO=LC-1
  . DO CD^XINDX8
  KILL AGR,EOC,IDT,JJ,LO,ML,OLD,SAV,TY,IND
  SET IOSL=tIOSL ;"restore saved IOSL
ELDone  ;
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
EvalBkPtCode(zzCode) ;"Evaluate Breakpoint code
  NEW zzBkPtTest SET zzBkPtTest=0 
  NEW ZZTEST SET ZZTEST=$TEST
  DO
  . NEW $ETRAP set $ETRAP="write ""(Invalid M Code!.  Error Trapped.)"",! set $ETRAP="""",$ECODE="""""
  . XECUTE zzCode
  . SET zzBkPtTest=$TEST
  IF ZZTEST  ;"Restore $TEST
  QUIT zzBkPtTest
  ;    
ToggleBreakpoint(pos,condition)   ; 
  ;"Purpose: to SET or release the GT.M breakpoint at position
  ;"Input: pos -- the position to alter
  ;"       condition -- OPTIONAL -- should be contain valid M code such that
  ;"                    IF @condition  is valid.  Examples:
  ;"                    i=1   or  $DATA(VAR)=0  or  $$MyFunct(var)=1
  IF $$IsBreakpoint(pos) DO
  . DO RelBreakpoint(pos)
  ELSE  DO
  . DO SETBKPT(pos,.condition)
  QUIT
  ;   
IsBreakpointFlex() ;
  NEW p1 SET p1=$GET(tmgIDEPos)
  NEW p2 SET p2=$GET(tmgOrigIDEPos)   
  NEW zzAtBkPt set zzAtBkPt=$$IsBreakpoint(p1) 
  if zzAtBkPt=0 set zzAtBkPt=$$IsBreakpoint(p2) 
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
  NEW aPos SET aPos=""
  FOR  SET aPos=$order(^TMG("TMGIDE",tmgDbgJNum,"ZBREAK",aPos)) quit:(aPos="")!(result=1)  DO
  . SET result=$$EquivalentBreakpoint(pos,aPos)
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
   . DO SETBKPT(pos)
   QUIT
   ;    
SETBKPT(pos,condition)  ;
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
  . NEW temp SET temp=$$MessageOut^TMGIDE2C("BKPOS "_pos_" "_$GET(condition))
  . WRITE "Results from remote process=",temp,!
  ELSE  DO
  . NEW brkLine SET brkLine=pos_":""n tmg do SETRUNMODE^TMGIDE2(1,1) s tmg=$$STEPTRAP^TMGIDE2($ZPOS,1)"""
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
  IF $$IsBreakpoint(pos)=0 DO SETBKPT(pos)
  QUIT
  ;    
GetBrkCondFlex() ;
  NEW ifS 
  SET ifS=$$GetBrkCond(tmgIDEPos)
  ;"WRITE "$$GetBrkCond(tmgIDEPos)=",ifS,!
  IF ifS="" SET ifS=$$GetBrkCond($$RelConvertPos^TMGMISC(tmgIDEPos,tmgViewOffset,tmgArrayName)) do
  . ;"WRITE "$$GetBrkCond($$RelConvertPos^TMGMISC(tmgIDEPos... =",ifS,!
  IF ifS="" SET ifS=$$GetBrkCond($$RelConvertPos^TMGMISC(tmgRelPos,tmgViewOffset,tmgArrayName)) do       
  . ;"WRITE "$$GetBrkCond($$RelConvertPos^TMGMISC(tmgRelPos... =",ifS,!
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
  ;"//kt NOTE: after upgrade to r202 from r132, format of gtmBrkPts has changed
  ;"           Now it is LABEL+OFFSET^ROUTINE>breakpoint m code.  
  FOR  SET I=$ORDER(GTMPTS("B",I)) QUIT:(I="")  DO
  . SET zbS=$GET(GTMPTS("B",I)) QUIT:zbS=""
  . SET zbS=$PIECE(zbS,">",1)  ;"//kt 1/7/24  remove execution code at zbreak
  . ;"NEW CODE SET CODE="ZBREAK -"_$GET(GTMPTS("B",I))
  . NEW CODE SET CODE="ZBREAK -"_zbS  ;"//kt 1/7/24
  . IF zbS'=pos QUIT
  . XECUTE CODE
  KILL ^TMG("TMGIDE",tmgDbgJNum,"ZBREAK",pos)
  QUIT
  IF $GET(tmgDbgRemoteJob) DO  GOTO SBkDone
  . NEW temp SET temp=$$MessageOut^TMGIDE2C("RELBKPOS "_pos)
  ELSE  DO
  . NEW brkLine SET brkLine=pos_":""zcontinue"""
  . ZBREAK @brkLine
  ;"WRITE "released breakpoint at: ",pos,!
  QUIT
  ;       
HndlTable(tmgOrigAction) ;      
  ;"Purpose: Handle option for Table command
  DO HndlVars(.tmgOrigAction) 
  ;"//kt 11/17/24  DO ALTBUF^TMGTERM(1)  ;"//kt 10/22/24   
  ;"//kt 11/17/24  DO SETCOLORS("NORM")  ;"//kt 10/22/24
  ;"//kt 11/17/24  DO CUP^TMGTERM(1,2)   ;"//kt 10/22/24
  ;"//kt 11/17/24  NEW tmgARGS SET tmgARGS=$PIECE(tmgOrigAction," ",2,99)
  ;"//kt 11/17/24  IF +$GET(tmgDbgRemoteJob) DO
  ;"//kt 11/17/24  . NEW MSG SET MSG="TABLE"
  ;"//kt 11/17/24  . IF tmgARGS'="" SET MSG=MSG_tmgARGS
  ;"//kt 11/17/24  . NEW temp SET temp=$$MessageOut(MSG)
  ;"//kt 11/17/24  . IF temp="" QUIT
  ;"//kt 11/17/24  . NEW IDX SET IDX=""
  ;"//kt 11/17/24  . FOR  SET IDX=$ORDER(@temp@(IDX)) QUIT:(IDX="")  DO
  ;"//kt 11/17/24  . . NEW JDX SET JDX=""
  ;"//kt 11/17/24  . . FOR  SET JDX=$ORDER(@temp@(IDX,JDX)) QUIT:(JDX="")  DO
  ;"//kt 11/17/24  . . . WRITE $GET(@temp@(IDX,JDX)),!
  ;"//kt 11/17/24  ELSE  DO
  ;"//kt 11/17/24  . WRITE !   ;"get below bottom line for output.
  ;"//kt 11/17/24  . NEW tmgTEMP,tmgIDX
  ;"//kt 11/17/24  . NEW tmgFilter SET tmgFilter=""
  ;"//kt 11/17/24  . ZSHOW "V":tmgTEMP
  ;"//kt 11/17/24  . IF (tmgARGS'="") DO
  ;"//kt 11/17/24  . . SET tmgFilter=$PIECE(tmgARGS," ",1)
  ;"//kt 11/17/24  . . SET tmgFilter=$PIECE(tmgFilter,"*",1)
  ;"//kt 11/17/24  . SET tmgIDX=0
  ;"//kt 11/17/24  . FOR  SET tmgIDX=$ORDER(tmgTEMP("V",tmgIDX)) QUIT:(+tmgIDX'>0)  DO
  ;"//kt 11/17/24  . . NEW STR SET STR=$GET(tmgTEMP("V",tmgIDX))      
  ;"//kt 11/17/24  . . IF STR["TMGCOL" QUIT
  ;"//kt 11/17/24  . . IF $EXTRACT(STR,1,3)="tmg" QUIT
  ;"//kt 11/17/24  . . IF (tmgFilter'=""),$EXTRACT(STR,1,$LENGTH(tmgFilter))'=tmgFilter QUIT
  ;"//kt 11/17/24  . . WRITE STR,!
  ;"//kt 11/17/24  ;"NEW tempKey READ "        --- Press Enter To Continue--",tempKey:$GET(DTIME,3600)
  ;"//kt 11/17/24  DO PRESS2GO^TMGUSRI2  ;"//kt 10/22/24
  ;"//kt 11/17/24  DO ALTBUF^TMGTERM(0)  ;"//kt 10/22/24   
  QUIT
  ;
HndlVars(tmgOrigAction) ;
  ;"Purpose: Handle option for VARS command
  DO ALTBUF^TMGTERM(1)  ;"//kt 10/22/24   
  DO SETCOLORS("NORM")  ;"//kt 10/22/24
  DO CUP^TMGTERM(1,2)   ;"//kt 10/22/24
  SET tmgOrigAction=$GET(tmgOrigAction)
  NEW tmgARGS SET tmgARGS=$PIECE(tmgOrigAction," ",2,99)
  SET tmgARGS=$PIECE(tmgARGS,"*",1)
  IF +$GET(tmgDbgRemoteJob) DO  GOTO HVL2
  . NEW MSG SET MSG="VARS"
  . IF tmgARGS'="" SET MSG=MSG_tmgARGS
  . NEW temp SET temp=$$MessageOut(MSG)
  . IF temp="" QUIT                                                 
  . NEW i SET i=""
  . FOR  SET i=$ORDER(@temp@(i)) QUIT:(i="")  DO
  . . NEW j SET j=""
  . . FOR  SET j=$ORDER(@temp@(i,j)) QUIT:(j="")  DO
  . . . WRITE $GET(@temp@(i,j)),!
  . DO PRESS2GO^TMGUSRI2  ;"//kt 10/22/24
  ELSE  DO
  . DO SHOWVAR("VariableTable","TABLE^"_tmgARGS)
  ;"//kt 11/17/24  . WRITE !   ;"get below bottom line for output.
  ;"//kt 11/17/24  . NEW TABLEALL,VARS
  ;"//kt 11/17/24  . ZSHOW "*":tmgDbgTABLEALL
  ;"//kt 11/17/24  . MERGE VARS=tmgDbgTABLEALL("V")
  ;"//kt 11/17/24  . NEW IDX1 SET IDX1=0
  ;"//kt 11/17/24  . FOR  SET IDX1=$ORDER(VARS(IDX1)) QUIT:(+IDX1'>0)  DO
  ;"//kt 11/17/24  . . NEW LINE SET LINE=$GET(VARS(IDX1)) QUIT:LINE=""
  ;"//kt 11/17/24  . . NEW VARNAME SET VARNAME=$PIECE(LINE,"=",1)
  ;"//kt 11/17/24  . . IF $EXTRACT(VARNAME,1,3)="tmg" QUIT
  ;"//kt 11/17/24  . . IF $EXTRACT(VARNAME,1,6)="TMGCOL" QUIT
  ;"//kt 11/17/24  . . IF tmgARGS'="" QUIT:($EXTRACT(VARNAME,1,$LENGTH(tmgARGS))'=tmgARGS)
  ;"//kt 11/17/24  . . NEW VARVAL SET VARVAL=$PIECE(LINE,"=",2,9999)
  ;"//kt 11/17/24  . . WRITE VARNAME," = ",VARVAL,!
  ;"//kt 11/17/24  . DO PRESS2GO^TMGUSRI2  ;"//kt 10/22/24
HVL2 ;  
  DO ALTBUF^TMGTERM(0)  ;"//kt 10/22/24   
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
  IF WatchMode=1 DO SETRUNMODE^TMGIDE2(5) ;"data watch mode
  QUIT
  ;
HndlHelp ;
  ;"Purpose: Handle option for help.
  DO Box
  DO SETCOLORS("NORM")
  DO CUP^TMGTERM(1,2) ;"Cursor to line (1,2)
  DO HlpWrite(" {L} : Run sLow mode    | {M} : exec M code       | {SHOW [var]} : show [var]")
  DO HlpWrite(" {O} : Step OVER line   | {I} : step INTO line    | {J} : Run To Cursor")
  DO HlpWrite(" {R} : Run | {T} Step OUT | {H} : Hide debug code   | {CLS} : clear screen")
  DO HlpWrite(" {X} : Turn off debug   | {Q} : Abort             | {HOP} : Hop between Brkpoints  ")
  DO HlpWrite(" {B} : Toggle Brkpoint  | {C} : Custom breakpoint | {BC} : breakpoint code")
  DO HlpWrite(" {W} : Set watch code   | {W +MyVar} :Watch MyVar | {W -MyVar} :Remove watch")
  DO HlpWrite(" {A},{AA} : Scroll up     | {Z},{ZZ} : Scroll down    | {W +^} : Add Naked Ref")
  DO HlpWrite(" {[},{[[} : Scroll left   | {]},{]]} : Scroll right   | {W +*} : Watch Var changes")
  DO SETCOLORS("SPECIAL")
  DO PRESS2GO^TMGUSRI2
  DO Box
  DO SETCOLORS("NORM")
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
  DO SETCOLORS("SPECIAL")
  DO PRESS2GO^TMGUSRI2
  DO SETCOLORS("Reset")
  QUIT
  ;
HlpWrite(line)  ;
  FOR  QUIT:($LENGTH(line)'>0)  DO
  . IF $find(line,"{")>0 DO
  . . NEW part SET part=$PIECE(line,"{",1)
  . . DO SETCOLORS("NORM")
  . . WRITE part
  . . SET line=$PIECE(line,"{",2,999)
  . . SET part=$PIECE(line,"}",1)
  . . DO SETCOLORS("SPECIAL")
  . . WRITE part
  . . SET line=$PIECE(line,"}",2,999)
  . ELSE  DO
  . . DO SETCOLORS("NORM")
  . . WRITE line,!
  . . SET line=""
  DO SETCOLORS("NORM")
  QUIT
  ;
HndlMenu()  ;
  NEW MENU
  DO SETUPMENU(.MENU,1,1) ;
  NEW RUNOPT
  SET RUNOPT("ON PREDRAW")="HNDLPREDRAW^TMGIDE2C"
  NEW RESULT SET RESULT=$$RUNMENU^TMGUSRI7(.MENU,.RUNOPT)
  SET RESULT=$PIECE(RESULT,"^",2,99)
  ;"IF RESULT["^" SET RESULT=$PIECE(RESULT,"^",2)
  IF RESULT["?" DO
  . NEW MSG,TEMP SET TEMP=""
  . IF RESULT["#" DO
  . . SET TEMP=$PIECE(RESULT,"#",2,99),RESULT=$PIECE(RESULT,"#",1)
  . . DO SPLIT2AR^TMGSTUT2(TEMP,"\n",.MSG)
  . SET OPTION("POS","X")=5
  . SET OPTION("POS","Y")=2
  . SET OPTION("ALT BUFFER")=1
  . NEW PARAM SET PARAM=$$EDITDLG^TMGUSRI6(.MSG,.OPTION)
  . SET RESULT=$PIECE(RESULT,"?",1)_PARAM
  IF RESULT["/\" SET RESULT=$$REPLSTR^TMGSTUT3(RESULT,"/\","^")
  DO SETCOLORS("Reset")
  QUIT RESULT
  ;
SETUPMENU(MENU,X,Y)  ;"
  NEW TEMP,CURMENU
  DO SETUPMENUDATA(.MENU,.X,.Y)
  SET MENU("FOCUS IDX")=1  
  ;"-- Menu drawing options
  SET MENU("OPTION","NO TERM SAVE")=1
  NEW CURREF DO GETCOLORSTOREREF^TMGIDE6(.CURREF)
  NEW SELBG SET SELBG=$$GETCOLOR24^TMGIDE6(CURREF,"MENUSELBKGROUND","bg") 
  NEW TEMP SET TEMP=$$COLORMODEPAIR^TMGIDE2C("MENUTEXT")                      ;"NormalFG^NormalBG
  SET MENU("OPTION","COLOR","TEXT")=TEMP                                      ;"NormalFG^NormalBG
  SET MENU("OPTION","COLOR","SELTEXT")=$PIECE(TEMP,"^",1)_"^"_SELBG           ;"NormalFG^SelectedBG
  SET TEMP=$$COLORMODEPAIR^TMGIDE2C("MENUALTKEY")                             ;"AltKeyFG^NormalBG
  SET MENU("OPTION","COLOR","ALTKEY")=TEMP                                    ;"AltKeyFG^NormalBG
  SET MENU("OPTION","COLOR","SELALTKEY")=$PIECE(TEMP,"^",1)_"^"_SELBG         ;"AltKeyFG^SelectedBG
  SET MENU("OPTION","COLOR","BORDER")=$$COLORMODEPAIR^TMGIDE2C("MENUBORDER")  ;"BorderFG^NormalBG
  QUIT
  ;
SETUPMENUDATA(MENU,X,Y)  ;"
  NEW OFFSET,IDX,DONE SET IDX=0,DONE=0
  NEW ARR1,ARR2
  ;"Read in lines of menu data
  FOR OFFSET=1:1 DO  QUIT:DONE
  . NEW LINE SET LINE=$TEXT(MENUDATA+OFFSET^TMGIDE2C)
  . SET LINE=$$TRIM^XLFSTR($PIECE(LINE,";;""",2))
  . IF LINE["<DONE>" SET DONE=1 QUIT
  . IF $EXTRACT(LINE,1)="#" QUIT  ;"ignore comment
  . NEW NODE SET NODE=$PIECE(LINE,"^",1) QUIT:NODE=""
  . NEW SUBLINE SET SUBLINE=$PIECE(LINE,"^",2,99) QUIT:SUBLINE=""
  . SET IDX=IDX+1,ARR1(NODE,IDX)=SUBLINE
  NEW MAP,CURMENU,NODE
  NEW MENUNUM SET MENUNUM=1,MAP("TOP")=MENUNUM
  ;"Compile MAP of named submenus into numbered submenus
  SET NODE=""
  FOR  SET NODE=$ORDER(ARR1(NODE)) QUIT:NODE=""  DO
  . SET IDX=0
  . FOR  SET IDX=$ORDER(ARR1(NODE,IDX)) QUIT:IDX'>0  DO
  . . NEW SUBLINE SET SUBLINE=$GET(ARR1(NODE,IDX)) QUIT:SUBLINE=""
  . . NEW DISPLAY SET DISPLAY=$PIECE(SUBLINE,"^",1)
  . . NEW DATA SET DATA=$PIECE(SUBLINE,"^",2)
  . . NEW SUBMENU SET SUBMENU=$PIECE(SUBLINE,"^",3) QUIT:SUBMENU=""
  . . IF $DATA(MAP(SUBMENU))=0 SET MENUNUM=MENUNUM+1,MAP(SUBMENU)=MENUNUM
  ;"Copy ARR1(named) into ARR2(numbered)  
  SET NODE=""
  FOR  SET NODE=$ORDER(ARR1(NODE)) QUIT:NODE=""  DO
  . SET MENUNUM=+$GET(MAP(NODE)) QUIT:MENUNUM'>0
  . NEW IDX,JDX SET SUBLINE="",IDX=0,JDX=0
  . FOR  SET IDX=$ORDER(ARR1(NODE,IDX)),JDX=JDX+1 QUIT:IDX'>0  DO
  . . NEW SUBLINE SET SUBLINE=$GET(ARR1(NODE,IDX)) QUIT:SUBLINE=""
  . . IF $EXTRACT(SUBLINE,1)="@" DO  QUIT
  . . . SET SUBLINE=$EXTRACT(SUBLINE,2,$LENGTH(SUBLINE))
  . . . NEW CODE SET CODE="SET ARR2(MENUNUM"_SUBLINE
  . . . XECUTE CODE
  . . NEW DISPLAY SET DISPLAY=$PIECE(SUBLINE,"^",1)
  . . NEW DATA SET DATA=$PIECE(SUBLINE,"^",2)
  . . NEW SUBMENU SET SUBMENU=$PIECE(SUBLINE,"^",3),SUBMENU=+$GET(MAP(SUBMENU))
  . . SET ARR2(MENUNUM,JDX)=DISPLAY
  . . SET ARR2(MENUNUM,JDX,"DATA")=DATA
  . . IF SUBMENU>0 SET ARR2(MENUNUM,JDX,"SUBMENU")=SUBMENU
  ;"Add CLOSE MENU option to all menus.  
  SET MENUNUM=0
  FOR  SET MENUNUM=$ORDER(ARR2(MENUNUM)) QUIT:MENUNUM'>0  DO
  . SET IDX=0
  . FOR  SET IDX=$ORDER(ARR2(MENUNUM,IDX)) QUIT:(+IDX'=IDX)
  . SET IDX=$ORDER(ARR2(MENUNUM,IDX),-1)
  . SET ARR2(MENUNUM,IDX+1)="&Close Menu"
  . SET ARR2(MENUNUM,IDX+1,"DATA")="^"  
  KILL MENU MERGE MENU=ARR2
  QUIT
  ;
MENUDATA ;"FORMAT:  Menu#^DisplayName^ReturnValue^SubMenu
  ;;"TOP^&RunMode^^RM
  ;;"TOP^&View^^VM
  ;;"TOP^&Breakpoints^^BKM
  ;;"TOP^&Vars^^VARS
  ;;"TOP^&Watch^^WM
  ;;"TOP^&Utility^^UM
  ;;"TOP^@,"TOP MENU BAR")=1  
  ;;"TOP^@,"POS","X")=+$GET(X,1)
  ;;"TOP^@,"POS","Y")=+$GET(Y,25)
  ;;
  ;;"#---RunMode menu -------------
  ;;"RM^Jump to &Cursor^J  
  ;;"RM^Run Continuous^^ARM
  ;;"RM^Step &Into^I
  ;;"RM^Step O&ver^O
  ;;"RM^Step Ou&tOf^T
  ;;"RM^Turn O&ff Debug^X
  ;;"RM^Abort^Q
  ;;"RM^Stack^STACK
  ;;
  ;;"#---Continous running mode menu ---
  ;;"ARM^Continuous &Fast mode^R
  ;;"ARM^Continuous s&Low mode^L
  ;;"ARM^Continuous &Hidden mode^H
  ;;"ARM^Continuous H&opping mode^HOP
  ;;
  ;;"# ---Breakpoints Menu -----
  ;;"BKM^&Toggle Breakpoint at Current Line^B
  ;;"BKM^C&ustom Breakpoint^C
  ;;"BKM^&Delete Breakpoints^DBK
  ;;"BKM^Breakpoint Co&de^BC^
  ;;
  ;;"#--- View menu --------------
  ;;"VM^&Clear Screen^CLS
  ;;"VM^Scroll &Left^[
  ;;"VM^Scroll &Right^]
  ;;"VM^Page &Left^[[
  ;;"VM^Page &Right^]]
  ;;"VM^Page &Down^ZZ
  ;;"VM^Page &Up^AA
  ;;"VM^Code &Display^^CDM
  ;;"VM^Change Code &Viewpoint Location^G
  ;;
  ;;"#--- Code display menu --------------
  ;;"CDM^Force &Expanded M Cmds^XCMD
  ;;"CDM^Force Shor&tened M Cmds^SCMD
  ;;"CDM^Force &Upper Case^UCASE
  ;;"CDM^Force &Lower Case^LCASE
  ;;
  ;;"#--- Vars menu --------------
  ;;"VARS^&Show Var^SHOW ? #Enter Name of Variable\nTo Show
  ;;"VARS^&Browse Var^BROWSE ? #Enter Name of Variable\nTo Browse
  ;;"VARS^&Node View Var^NODES ? #Enter Name of Variable\nFor Nodes
  ;;"VARS^&ZWRITE Var^ZWR ? #Enter Name of Variable\nTo ZWRITE
  ;;"VARS^Show Var &Table^VARS ? #OPTIONAL\nEnter Mask\nE.g. TMG* for Vars Starting with 'TMG'
  ;;"VARS^Show Var Changes^VDIFF
  ;;"VARS^Toggle Var Tracing^TVDIFF
  ;;"VARS^Show T&race
  ;;"VARS^Execute M code^M ? #Enter Valid M/Mumps Code to Execute
  ;;
  ;;"#--- Watch menu --------------
  ;;"WM^Enter Watch &M Code^W ? #Enter Valid M/Mumps Code to Execute\nFor Variable Watch\nE.g. 'WRITE "[",MYVAR,"]"'
  ;;"WM^&Add Watch Var^W +? #Enter Variable Name\nTo Add to Watch List
  ;;"WM^&Remove Watch Var^W -? #Enter Variable Name\nTo Remove from Watch List
  ;;"WM^&Clear all Watches^W @
  ;;"WM^Add Watch of &Naked Ref^W +/\
  ;;"WM^Watch Var &Changes^W +*
  ;;"WM^&Run Data Watch^RDW
  ;;
  ;;"# --- Utility -----
  ;;"UM^Edit Display Colors^COLORS
  ;;"UM^Resync Display^RESYNC
  ;;"UM^Expand Current Line^E
  ;;"UM^Manually Set Display Size^=
  ;;"UM^Manage/Hide modules^HIDE
  ;;"UM^Restore Keyboard Functionality^INITKB
  ;;"UM^Remove Terminal Scroll Areas^FULL
  ;;"<DONE>
  ;  
HNDLPREDRAW(MENU) ;
  DO REFRESHCODE^TMGIDE2B
  QUIT 1
  ;
SETCOLORS(tmgMode)  ;"Change display output colors based on names (e.g. 'NORM')   
  ;"Purpose: SET colors in central location
  ;"Input: tmgMode -- the tmgMode to change the colors to
  ;"       bg -- OPTIONAL -- the default background.  Default=15
  ;"set ^TMG("TMP","SETCOLORS MODE",$ZH)=$get(tmgMode)
  NEW fg,bg
  IF $GET(tmgMode)="Reset" DO  GOTO GFBDN
  . DO VTATRIB^TMGTERM(0)   ;"reset colors
  DO GETFGBG(.tmgMode,.fg,.bg)
  DO COLORS^TMGTERM(fg,bg)
SCDn ;
  QUIT
  ;
GETFGBG(tmgMode,fg,bg,ref)  ;"get fg and bg for Mode
  SET tmgMode=$GET(tmgMode) 
  IF tmgMode="" SET tmgMode="Reset"
  IF $GET(ref)="" DO GETCOLORSTOREREF^TMGIDE6(.ref)
  IF $DATA(@ref)=0 DO INITCOLORS^TMGIDE6
  NEW COLORMODE SET COLORMODE=$GET(@ref@("MODE"))
  IF COLORMODE="24bit" DO
  . SET fg=$$GETCOLOR24^TMGIDE6(ref,tmgMode,"fg")
  . SET bg=$$GETCOLOR24^TMGIDE6(ref,tmgMode,"bg")
  ELSE  DO
  . SET fg=$GET(@ref@(tmgMode,"fg"),15)
  . SET bg=$GET(@ref@(tmgMode,"bg"),15)
  . IF (bg="$") DO
  . . SET bg=$GET(@ref@("MENUBKGROUND"),"@")
  . IF (bg="@") DO
  . . SET bg=$GET(@ref@("TEMP BACKGROUND"),"@")
  . . IF bg="@" SET bg=$GET(@ref@("BACKGROUND"),0)
  . IF fg=bg DO
  . . IF (fg<15) SET fg=fg+1
  . . ELSE  IF (fg>0) SET fg=fg-1
GFBDN ;  
  QUIT
  ;
COLORMODEPAIR(tmgMode)  ;"Get color pair for mode.
  ;"NOTE: tmgMode should NOT be 'Reset' -- not handled here.  
  NEW fg,bg DO GETFGBG(.tmgMode,.fg,.bg)
  QUIT fg_"^"_bg
  ;
Box    ;
  ;"Purpose: Draw a box on the top of the screen.
  ;"Globals Scope Vars used: tmgScrWidth,tmgScrHeight
  SET tmgScrWidth=$GET(tmgScrWidth,80)
  SET tmgScrHeight=$GET(tmgScrHeight,10)
  NEW tmgDbgBlankLine SET $PIECE(tmgDbgBlankLine," ",tmgScrWidth)=" "
  NEW ideBarLine SET $PIECE(ideBarLine,"=",tmgScrWidth)="="
  DO CUP^TMGTERM(1,1) ;"Cursor to line (1,1)
  DO SETCOLORS("Highlight")
  WRITE ideBarLine,!
  DO SETCOLORS("NORM")
  NEW cdLoop FOR cdLoop=0:1:tmgScrHeight+1 WRITE tmgDbgBlankLine,!
  DO SETCOLORS("Reset")
  QUIT
  ;
ShouldSkip(module) ;
  ;"Purpose: to see IF module is in hidden list
  NEW result SET result=0
  IF $GET(tmgDbgHideList)="" SET tmgDbgHideList=$name(^TMG("TMGIDE",$J,"HIDE LIST"))  ;"//kt 3/25/11
  IF $DATA(@tmgDbgHideList)=0 DO SETHIDELIST^TMGIDE
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
  WRITE !,"Should removal be perminent for this session" SET %=1
  DO YN^DICN WRITE !
  IF %=1 SET ^TMP("TMGIDE NOHIDE",$J,option)=""
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
GETSEC()  ;"GET SYSTEM SECONDS
  NEW T,SEC,MCS SET T=$ZH SET SEC=$P(T,",",2),MCS=$P(T,",",3)
  FOR  QUIT:$LENGTH(MCS)=6  SET MCS="0"_MCS
  QUIT SEC_"."_MCS
  ;
MAX(A,B) ;
  IF A>B QUIT A
  QUIT B
  ;
SHOWVAR(zzREF,MODE)  ;"Show variable, using SCROLLER
  ;"MODE -- OPTIONAL.  if "ZWR' then use ZWR,
  ;"                   if "TABLE^<optional filter>" then does variable table show
  ;"                   otherwise uses ARDUMP
  NEW RESULT  ;"some code below is leaving RESULT on table, so NEW it here.   
  IF zzREF["$" DO  QUIT
  . WRITE zzREF,"='"
  . NEW CODE SET CODE="do DEBUGWRITE^TMGIDE2C(1,"_zzREF_")"
  . NEW $ETRAP SET $ETRAP="WRITE ""(Invalid M Code!.  Error Trapped.)"",! SET $ETRAP="""",$ecode="""""
  . XECUTE CODE
  . WRITE "'    "
  ;
  SET zzREF=$$CREF^DILF(zzREF)  ;"convert open to closed format
  NEW zzTEMP
  IF $GET(MODE)="ZWR" DO
  . DO ZWR2ARR^TMGZWR(zzREF,"zzTEMP")  ;"ZWRITE @zzREF  
  IF $GET(MODE)["TABLE" DO
  . NEW tmgARGS set tmgARGS=$PIECE(MODE,"^",2)
  . NEW TABLEALL,VARS
  . ZSHOW "*":tmgDbgTABLEALL
  . MERGE VARS=tmgDbgTABLEALL("V")
  . NEW IDX1 SET IDX1=0
  . FOR  SET IDX1=$ORDER(VARS(IDX1)) QUIT:(+IDX1'>0)  DO
  . . NEW LINE SET LINE=$GET(VARS(IDX1)) QUIT:LINE=""
  . . NEW VARNAME SET VARNAME=$PIECE(LINE,"=",1)
  . . IF $EXTRACT(VARNAME,1,3)="tmg" KILL VARS(IDX1) QUIT
  . . IF $EXTRACT(VARNAME,1,6)="TMGCOL" KILL VARS(IDX1) QUIT
  . . IF tmgARGS'="",($EXTRACT(VARNAME,1,$LENGTH(tmgARGS))'=tmgARGS) KILL VARS(IDX1) QUIT
  . KILL zzTEMP MERGE zzTEMP=VARS    
  ELSE  DO    
  . DO ARRDUMP(zzREF,,,"zzTEMP")
  NEW tmgARRAY,IDX,JDX SET IDX=0,JDX=1
  FOR  SET IDX=$ORDER(zzTEMP(IDX)) QUIT:IDX'>0  SET tmgARRAY(JDX,$GET(zzTEMP(IDX)))="",JDX=JDX+1
  KILL zzTEMP
  NEW tmgOPTION
  SET tmgOPTION("HEADER",1)=" - Display of ["_zzREF_"] - "
  SET tmgOPTION("FOOTER",1)="Enter ^ to exit"
  SET tmgOPTION("ON SELECT")="HNDONSEL^TMGIDE2C"
  DO ADDNICECOLORS^TMGUSRIF(.tmgOPTION,1) 
  ;
  ;"BELOW IS GOOD FOR DEBUGGING
  NEW ZZDEBUG SET ZZDEBUG=0
  IF ZZDEBUG=1 DO                    
  . SET tmgOPTION("SCRN TOP OFFSET")=22
  . SET tmgOPTION("SCRN HEIGHT")=20
  . SET tmgOPTION("SCRN WIDTH")=130
  ;
  DO SCROLLER^TMGUSRIF("tmgARRAY",.tmgOPTION)
  QUIT
  ; 
HNDONSEL(TMGPSCRLARR,OPTION,INFO)  ;"Part of SHOWVAR -- Handle ON SELECT event from SCROLLER
  SET TMGSCLRMSG="^"
  QUIT
  ;  
DEBUGINDENT(INDENT,FORCED,OUTREF)  ;
  ;"NOTE: Duplicate of function in TMGIDEDEBUG
  ;"PUBLIC FUNCTION
  ;"Purpose: to provide a unified indentation for debug messages
  ;"Input: INDENT = number of indentations
  ;"       FORCED = 1 if to indent regardless of DEBUG mode
  ;"       OUTREF -- Optional.  PASS BY NAME.  If provided, output is put into @OUTREF@(#) AND @OUTREF@=<Last line # used>
  SET FORCED=$GET(FORCED,0)  
  IF ($GET(TMGIDEDEBUG,0)=0)&(FORCED=0) QUIT
  NEW IDX
  FOR IDX=1:1:INDENT DO
  . IF FORCED DO DEBUGWRITE(INDENT,"  ",0,.OUTREF)
  . ;"ELSE  DO DEBUGWRITE(INDENT,". ")
  QUIT
  ;
DEBUGWRITE(INDENT,STR,ADDLF,OUTREF) ;
  ;"PUBLIC FUNCTION
  ;"Purpose: to WRITE debug output.  Having the proc separate will allow
  ;"        easier dump to file etc.
  ;"Input:INDENT, the amount of indentation expected for output.  NOT IMPLEMENTED....
  ;"      STR -- the text to write
  ;"      ADDLF -- boolean, 1 IF ! (i.e. newline) should be written after STR
  ;"      OUTREF -- Optional.  PASS BY NAME.  If provided, output is put into @OUTREF@(#) AND @OUTREF@=<Last line # used>
  ;"NOTE: TMGIDEDEBUG used in GLOBAL SCOPE
  ;"Note: If above values are not defined, then functionality will be ignored.
  ;"//kt 3/23/24  SET TMGIDEDEBUG=$GET(TMGIDEDEBUG,0)
  ;"//kt 3/23/24  IF TMGIDEDEBUG=0 QUIT  
  SET STR=$$NOHIDDENSTR($GET(STR))
  IF $GET(OUTREF)'="" DO  GOTO DWDN   ;"kt added block 11/17/24
  . NEW IDX SET IDX=$GET(@OUTREF)
  . IF IDX="" SET IDX=1
  . NEW PRIOR SET PRIOR=$GET(@OUTREF@(IDX))
  . SET @OUTREF@(IDX)=PRIOR_STR
  . IF $GET(ADDLF) SET IDX=IDX+1
  . SET @OUTREF=IDX    
  ;"-- older code below. 
  SET TMGIDEDEBUG=$GET(TMGIDEDEBUG)  ;"//kt 3/23/24
  IF (TMGIDEDEBUG=2)!(TMGIDEDEBUG=3),$DATA(DebugFile) USE DebugFile
  WRITE $$NOHIDDENSTR(STR)
  IF $GET(ADDLF)=1 DO
  . NEW ENDSPACE SET ENDSPACE=20
  . IF +$GET(IOM)>0,(IOM-$X)<20 SET ENDSPACE=IOM-$X
  . NEW IDX FOR IDX=1:1:ENDSPACE WRITE " "        
  . WRITE !
  IF (TMGIDEDEBUG=2)!(TMGIDEDEBUG=3) USE $PRINCIPAL
DWDN ;    
  QUIT
  ;
NOHIDDENSTR(STR) ;"Convert any hidden characters to $C(#)
  NEW RESULT SET RESULT=""
  NEW LEN SET LEN=$LENGTH(STR)
  NEW INHIDDEN SET INHIDDEN=0
  NEW IDX FOR IDX=1:1:LEN DO
  . NEW CH SET CH=$EXTRACT(STR,IDX)
  . NEW ASCII SET ASCII=$ASCII(CH)
  . IF (ASCII<32)!(ASCII>126) DO
  . . IF INHIDDEN=0 DO
  . . . SET RESULT=RESULT_"$C(",INHIDDEN=1
  . . ELSE  SET RESULT=RESULT_","
  . . SET RESULT=RESULT_ASCII
  . ELSE  DO
  . . IF INHIDDEN DO
  . . . SET RESULT=RESULT_")",INHIDDEN=0
  . . SET RESULT=RESULT_CH
  IF INHIDDEN DO
  . SET RESULT=RESULT_")",INHIDDEN=0
  QUIT RESULT
  ;
ARRDUMP(zzREF,TMGIDX,INDENT,zzrefOUT)  ;
  ;"NOTE: Similar to ARRDUMP^TMGMISC3. HOWEVER, this is fundamentally
  ;"      different because it uses DEBUGWRITE, instead of normal WRITE
  ;"PUBLIC FUNCTION
  ;"Purpose: to get a custom version of GTM's "zwr" command
  ;"Input: Uses global scope var tmgDbgIndent (if defined)
  ;"        zzREF: NAME of global to display, i.e. "^VA(200)"
  ;"        TMGIDX: initial index (i.e. 5 IF wanting to start with ^VA(200,5)
  ;"        INDENT: spacing from left margin to begin with. (A number.  Each count is 2 spaces)
  ;"          OPTIONAL: INDENT may be an array, with information about columns
  ;"                to skip.  For example:
  ;"                INDENT=3
  ;"                INDENT(2)=0 --> show | for columns 1 & 3, but NOT 2
  ;"        zzrefOUT -- Optional.  PASS BY NAME.  If provided, output is put into @zzrefOUT@(#) AND @zzrefOUT@=<Last line # used>  
  ;"Result: 0=OK to continue, 1=user aborted display
  NEW RESULT SET RESULT=0
  NEW $ETRAP SET $ETRAP="SET RESULT="""",$ETRAP="""",$ecode="""""
  SET INDENT=$GET(INDENT,0)
  IF $DATA(zzREF)=0 GOTO ADDN
  NEW ABORT SET ABORT=0
  IF (zzREF["@") DO  GOTO:(ABORT=1) ADDN
  . NEW TEMP SET TEMP=$PIECE($EXTRACT(zzREF,2,99),"@",1)
  . IF $DATA(TEMP)#10=0 SET ABORT=1
  . SET zzREF=$GET(@TEMP)
  ;"Note: I need to DO some validation to ensure zzREF doesn't have any null nodes.
  NEW X SET X="SET TEMP=$GET("_$$UP^XLFSTR(zzREF)_")"
  SET X=$$UP^XLFSTR(X)
  DO ^DIM ;"a method to ensure zzREF doesn't have an invalid reference.
  IF $GET(X)="" GOTO ADDN
  ;
  DO DEBUGINDENT(INDENT,0,.zzrefOUT)
  NEW JDX FOR JDX=1:1:INDENT-1 DO DEBUGWRITE(INDENT,$SELECT($GET(INDENT(JDX),-1)=0:" ",1:"| "),0,.zzrefOUT)
  IF INDENT>0 DO DEBUGWRITE(INDENT,"}~",,.zzrefOUT)
  ;
  SET TMGIDX=$GET(TMGIDX,"")
  IF TMGIDX'="" DO
  . IF $DATA(@zzREF@(TMGIDX))#10=1 DO
  . . NEW STR SET STR=@zzREF@(TMGIDX)
  . . IF STR="" SET STR=""""""
  . . IF $LENGTH(STR)'=$LENGTH($$TRIM^XLFSTR(STR)) SET STR=""""_STR_""""  ;"//kt 9/6/17
  . . NEW QT SET QT=""
  . . IF +TMGIDX'=TMGIDX SET qt=""""
  . . DO DEBUGWRITE(INDENT,QT_TMGIDX_QT_" = "_STR,1,.zzrefOUT)
  . ELSE  DO
  . . DO DEBUGWRITE(INDENT,TMGIDX,1,.zzrefOUT)
  . SET zzREF=$NAME(@zzREF@(TMGIDX))
  ELSE  DO
  . DO DEBUGWRITE(INDENT,zzREF,0,.zzrefOUT)
  . IF $DATA(@zzREF)#10=1 DO
  . . DO DEBUGWRITE(0,"="_$GET(@zzREF),0,.zzrefOUT)
  . DO DEBUGWRITE(0,"",1,.zzrefOUT)
  ;
  NEW COUNT SET COUNT=$GET(^TMP($J,"ARRDUMP^TMGIDE2C","COUNT"))
  SET TMGIDX=$ORDER(@zzREF@(""))
  IF TMGIDX="" GOTO ADDN
  SET INDENT=INDENT+1
  FOR  DO  QUIT:TMGIDX=""  IF RESULT=1 GOTO ADDN
  . NEW IDX SET IDX=$ORDER(@zzREF@(TMGIDX))
  . IF IDX="" SET INDENT(INDENT)=0
  . NEW TEMPINDENT MERGE TEMPINDENT=INDENT
  . SET COUNT=COUNT+1 IF COUNT#100=0 DO PROGBAR^TMGUSRI2(COUNT,"Loading "_COUNT,-1,-1)
  . SET RESULT=$$ARRDUMP(zzREF,TMGIDX,.TEMPINDENT,.zzrefOUT)  ;"Call self recursively
  . SET TMGIDX=$ORDER(@zzREF@(TMGIDX))
  SET ^TMP($J,"ARRDUMP^TMGIDE2C","COUNT")=COUNT
  ;
  ;"Put in a blank space at end of subbranch
  DO DEBUGINDENT(INDENT,0,zzrefOUT)
  IF 1=0,INDENT>0 DO
  . NEW TMGi
  . FOR TMGi=1:1:INDENT-1 DO
  . . NEW STR SET STR=""
  . . IF $GET(INDENT(TMGi),-1)=0 SET STR="  "
  . . ELSE  SET STR="| "
  . . DO DEBUGWRITE(INDENT,STR,,.zzrefOUT)
  . DO DEBUGWRITE(INDENT," ",1,.zzrefOUT)
ADDN  ;
  QUIT RESULT
  ;
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
  