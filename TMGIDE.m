TMGIDE ;TMG/kst/A debugger/tracer for GT.M ;9/6/17, 3/24/24
         ;;1.0;TMG-LIB;**1**;03/29/09
  ;
  ;" A Debug/Tracer for GT.M
  ;"
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
  ;"
  ;"This file is part of the TMG LIBRARY, and may only be used in accordence
  ;" to license terms outlined in separate file TMGLICNS.m, which should 
  ;" always be distributed with this file.
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;" This program will launch a shell for the TMG STEP TRAP debugger
  ;" It provides the user with a prompt, like this:
  ;"
  ;"      (^ to QUIT) IDE>
  ;"
  ;" Any valid M code may be entered here.  To use the tracing
  ;" ability, launch a function, like this:
  ;"
  ;"      (^ to QUIT) IDE>do ^MyFunction
  ;"
  ;" Dependancies:
  ;"     Uses TMGIDE2,TMGTERM,TMGUSRIF
  ;"           ^DIM,XGF,XINDX7,XINDX8,XINDEX  <-- VA code
  ;"            %ZVEM* (if available)
  ;"=======================================================================
  ;" API -- Public Functions.
  ;"=======================================================================
  ;"Start^TMGIDE -- launch Debugger
  ;"BKPT^TMGIDE -- SET a breakpoint
  ;"KBKPT^TMGIDE -- KILL (release) breakpoint
  ;"DIRDEBUG^TMGIDE(CodeLine) -- direct run of debugger with specified line
  ;"=======================================================================
  ;"PRIVATE API FUNCTIONS
  ;"=======================================================================
  ;"PROMPTLAUNCH
  ;"------------------------------------------------------------
  ;"------------------------------------------------------------
  ;
START  ;"Purpose: To Launch debugger.   This is the entry point
  NEW tmgDbgOptions,tmgERR
  NEW tmgMenu,tmgCt 
  DO DEBUGINIT^TMGIDE1
  NEW tmgUserSelect
M1 ;
  KILL tmgMenu
  SET tmgCt=1
  SET tmgMenu(0)="Welcome to the TMG debugging environment"
  SET tmgMenu(tmgCt)="Start debugger session."_$CHAR(9)_"AllInOne",tmgCt=tmgCt+1
  ;"SET tmgMenu(tmgCt)="Start debugger in THIS window."_$CHAR(9)_"AllInOne",tmgCt=tmgCt+1
  ;"SET tmgMenu(tmgCt)="Start debugger CONTROLLER for another Process."_$CHAR(9)_"StartController",tmgCt=tmgCt+1
  ;"SET tmgMenu(tmgCt)="Debug, SENDING control to a Controller."_$CHAR(9)_"StartSender",tmgCt=tmgCt+1
  SET tmgMenu(tmgCt)="Start a data watch session"_$CHAR(9)_"DataWatch",tmgCt=tmgCt+1
  SET tmgMenu(tmgCt)="Set a custom breakpoint"_$CHAR(9)_"SetBreakpoint",tmgCt=tmgCt+1
  SET tmgMenu(tmgCt)="Kill a custom breakpoint"_$CHAR(9)_"KillBreakpoint",tmgCt=tmgCt+1
  SET tmgMenu(tmgCt)="Edit HIDE modules"_$CHAR(9)_"EditHIDE",tmgCt=tmgCt+1
  SET tmgMenu(tmgCt)="Debug ANOTHER PROCESS"_$CHAR(9)_"Interrupt",tmgCt=tmgCt+1
  SET tmgMenu(tmgCt)="KILL ANOTHER PROCESS"_$CHAR(9)_"KillOther",tmgCt=tmgCt+1
  SET tmgMenu(tmgCt)="Run ^TMGZJOB"_$CHAR(9)_"ZJob",tmgCt=tmgCt+1
  ;"SET tmgMenu(tmgCt)="View TRACE log from last run"_$CHAR(9)_"Trace",tmgCt=tmgCt+1
  ;
  SET tmgUserSelect=$$MENU^TMGUSRI2(.tmgMenu,"^")
  KILL tmgMenu ;"Prevent from cluttering variable table during debug run
  ;
  IF tmgUserSelect="AllInOne" GOTO M2
  IF tmgUserSelect="StartController" DO Controller^TMGIDE3 GOTO M1
  IF tmgUserSelect="StartSender" DO Sender^TMGIDE4() GOTO M1
  IF tmgUserSelect="SetBreakpoint" DO BKPT GOTO M1
  IF tmgUserSelect="KillBreakpoint" DO KBKPT GOTO M1
  IF tmgUserSelect="Interrupt" DO PICKINTR^TMGIDE5 GOTO M1
  IF tmgUserSelect="KillOther" DO KILLOTHER^TMGIDE1 GOTO M1
  IF tmgUserSelect="ZJob" DO ^TMGZJOB GOTO M1
  IF tmgUserSelect="EditHIDE" DO SetupSkips^TMGIDE2C GOTO M1
  IF tmgUserSelect="Trace" DO ShowTrace^TMGIDE6 GOTO M1
  IF tmgUserSelect="DataWatch" DO  GOTO M2:(tmgDataWatchMode) GOTO M1
  . DO EditWatch^TMGIDE7(.tmgDataWatchMode)
  IF tmgUserSelect="^" GOTO M3
  IF tmgUserSelect=0 SET tmgUserSelect=""
  GOTO M1
  ;"---------------------
M2  ;
  DO
  . NEW IDX FOR IDX=1:1:10 WRITE !
  WRITE !,"Welcome to the TMG debugging environment",!
  WRITE "Enter any valid M command...",!
  DO SETERRTRAP^TMGIDE2A
  ;
  DO PROMPTLAUNCH("AllInOne")  ;"<--- this is where debugged program is run. 
  ;
M3  ;
  DO SHUTDOWN^TMGIDE1
  QUIT                    
  ;
  ;"-------------------------------------------------------------------
  ; 
PROMPTLAUNCH(tmgMode,tmgCodeLine)    ;
  ;"Purpose: to interact with user and run through code.
  ;"Input: tmgMode: OPTIONAL: Default is 'AllInOne'
  ;"          AllInOne --> debug output to same window
  ;"          SendOut --> debug output to Controller widow
  ;"       tmgCodeLine -- OPTIONAL.  If specified, then code will
  ;"                             be executed in the debugger, and
  ;"                           and when done, the debugger will exit.
  ;"Result : none
  ;
PL1 ;
  SET tmgMode=$GET(tmgMode,"AllInOne")
  NEW tmgDbgBlankLine
  NEW tmgHxI SET tmgHxI=""
  NEW tmgDbgLine SET tmgDbgLine=""
  NEW tmgLastLine,tmgUserAborting,tmgUserRestarting,tmgINITZLEVEL
  SET tmgStepMode="INTO"
  SET tmgCodeLine=$GET(tmgCodeLine)
  DO SETUPVARS^TMGIDE1
  DO INITKB^XGF()  ;"SET up keyboard input escape code processing
PL2 ;
  ;" === USER INTERACTION AND PROMPT PART ====
  IF $DATA(tmgCodeLine)=0 WRITE !,"Restoring TMGIDE variables...",! GOTO PL1
  IF tmgCodeLine="" DO
  . DO CHA^TMGTERM(1) WRITE tmgDbgBlankLine
  . DO CHA^TMGTERM(1) WRITE "(^ to QUIT) //"
  . NEW tmgDbgLineOptions
  . SET tmgDbgLineOptions("ON-UP")="Q",tmgDbgLineOptions("ON-DOWN")="Q"
  . SET tmgDbgLineOptions("WIDTH")=60,tmgDbgLineOptions("FILLCH")=" "
  . SET tmgDbgLine=$$EDITBOX2^TMGUSRI6(tmgDbgLine,.tmgDbgLineOptions)
  . IF tmgDbgLineOptions("ESCKEY")'="" SET tmgXGRT=tmgDbgLineOptions("ESCKEY")
  ELSE  DO
  . SET tmgDbgLine=tmgCodeLine
  . SET tmgXGRT=""
  DO INITKB^XGF()
  DO XLTPROMPTKEYS^TMGIDE1(.tmgDbgLine,tmgXGRT)
  ;
  IF tmgDbgLine="?" DO SHOWHELP^TMGIDE1 GOTO PL2
  IF tmgDbgLine="<DN>" SET tmgDbgLine=$$GETHX^TMGIDE1(.tmgHxI,1) GOTO PL2
  IF tmgDbgLine="<UP>" SET tmgDbgLine=$$GETHX^TMGIDE1(.tmgHxI,-1) GOTO PL2
  IF tmgDbgLine="^" SET $ZSTEP="" GOTO PLDN
  IF $$UP^XLFSTR($EXTRACT(tmgDbgLine,1,6))="OPTION" DO  GOTO:(tmgDbgLine="") PL2
  . NEW temp SET temp=tmgDbgLine
  . SET tmgDbgLine=$$GETMENUCODE^TMGIDE1($$TRIM^XLFSTR($EXTRACT(tmgDbgLine,7,999)))
  . IF tmgDbgLine="" DO  QUIT
  . . WRITE "Couldn't find OPTION code to execute.  Please try again.",!
  . WRITE "Executing code: ",tmgDbgLine,!
  ;
  WRITE !
  IF tmgCodeLine="" DO SAVEHX^TMGIDE1(tmgDbgLine)
  ;
PL3 ;  
  ;"==== START LAUNCHING PART =====
  IF $GET(tmgDataWatchMode) DO
  . DO SETRUNMODE^TMGIDE2A(4)   ;"4=Hopping mode
  ELSE  DO
  . IF $GET(tmgRunMode)=4 QUIT ;"4=Hopping mode
  . DO SETRUNMODE^TMGIDE2A(1)   ;"1=Step-by-step mode
  SET ^TMG("TMGIDE",$J,"RUNMODE")=tmgRunMode
  ;  
  DO SETDOLLARZSTEP^TMGIDE1  ;"Setting $ZSTEP is a key core of the debugger.
  DO SETZSTEPMODE^TMGIDE1(1) ;"1=ZSTEP INTO  <-- this sets mode of action for XECUTE that comes next. 
  ;
  SET tmgINITZLEVEL=$ZLEVEL
  ;"NOTE: tmgINITZLEVEL used in HndlQuit^TMGIDE2C, HndlRestart^TMGIDE2C to teleport user back here
  ;"=========================
  DO
  . ;"NOTE: $ETRAP gets called repeatedly, apparently as it repeatitively pops off levels of stack.  
  . NEW $ETRAP SET $ETRAP="DO TOPERRHNDL^TMGIDE($ZPOS) QUIT  "
  . XECUTE tmgDbgLine         ;"<---- this launches the users mumps command
  ;"=========================
  ;
  IF $GET(tmgUserRestarting)=1 DO  GOTO PL3  ;"Set up if user type Q/QUIT at command prompt.
  . SET tmgUserRestarting=0,tmgRunMode=1
  . WRITE #,"RESTARTING...",!
  . DO PRESS2GO^TMGUSRI2
  ;
  ;"At this point we have returned from user's command.
  ;"If user is aborting, we will be brought here via ZGOTO tmgINITZLEVEL in TOPERRHNDL
  SET $ZSTEP=""  ;"turn off step capture
  ;
  WRITE !
  IF $GET(tmgUserAborting)=1 GOTO PLDN  ;"Set up if user type Q/QUIT at command prompt.  
  IF $DATA(tmgDbgBlankLine)=0 DO SETUPVARS^TMGIDE1  ;"without this, crash after running ^XUP
  SET tmgDbgLine="",tmgHxI=""
  SET tmgStepMode="into"
  IF $GET(tmgCodeLine)'="" GOTO PLDN
  GOTO PL2 ;"loop for prompt again.
  ;
PLDN ;
  QUIT
  ;  
TOPERRHNDL(ZPOS)  ;"Error handler from $ETRAP in PROMPTLAUNCH()
  ;"NOTE:  This function gets called repeatedly, apparently as it repeatitively pops off levels of stack. 
  ;"       When all the levels are popped off here, then other error trap code will be run, which shows code etc.
  ;"       That error trap is set in SETERRTRAP^TMGIDE2A, and currently effects call to ERRTRAP^TMGIDE2A
  ;"       We are setting up tmgERR here for use in that code.  
  NEW % SET %=$ORDER(tmgERR("ZP",""),-1)+1
  SET tmgERR("ZP",%)=$GET(ZPOS)
  SET tmgERR("tmgDbgLine")=$GET(tmgDbgLine)
  IF 1=0 DO
  . WRITE !,"In Error Handler: TOPERRHNDL^TMGIDE.  Current STACK:",!
  . NEW % FOR %=0:1:$STACK DO
  . . W (%+1),".  ",$STACK(%)," -- ",$STACK(%,"PLACE"),": ",$STACK(%,"MCODE")," - ",$STACK(%,"ECODE"),!
  QUIT
  ;
  ;"-------------------------------------------------------------------
BKPT  ;
  ;"Purpose: To ask user for an address, and SET a breakpoint there
  ;"         This can be done from GTM prompt, and debugger will be launched
  ;"         when this address is reached during normal execution.
  ;
  READ "Enter breakpoint (e.g. Label+8^MyFunct): ",Pos:$GET(DTIME,3600),!
  DO SETBKPT^TMGIDE2C(Pos)
  SET $ZTRAP=""  ;"This makes sure that Fileman error trap is not active
  QUIT
  ;
KBKPT  ;
  ;"Purpose: To ask user for an address, and KILL (release) breakpoint there
  ;"         This can be done from GTM prompt
  READ "Enter breakpoint to be killed (released) (e.g. Label+8^MyFunct): ",Pos,!
  DO RelBreakpoint^TMGIDE2(Pos)
  QUIT
  ;
  ;"------------------------------------------------------------
  ;
DIRDEBUG(tmgCodeLine) ;"Direct Debug
  ;"Purpose: Direct run of debugger with specified line
  NEW tmgDbgOptions
  DO DEBUGINIT^TMGIDE1
  DO SETERRTRAP^TMGIDE2A
  DO PROMPTLAUNCH("AllInOne",.tmgCodeLine)
  DO SHUTDOWN^TMGIDE1
  QUIT
  ;
