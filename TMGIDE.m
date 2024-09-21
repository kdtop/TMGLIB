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
  ;"PROMPT
  ;"ShutDown
  ;"REPLACE(IN,SPEC)
  ;"READ(XGCHARS,XGTO)
  ;"READ2(XGCHARS,XGTO)
  ;"------------------------------------------------------------
  ;"------------------------------------------------------------
  ;
START  ;"Purpose: To Launch debugger.   This is the entry point
  ;
  NEW tmgDbgOptions
  DO DEBUGINIT
  NEW tmgUserSelect
M1 ;
  NEW tmgMenu
  NEW tmgCt SET tmgCt=1
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
  SET tmgMenu(tmgCt)="Run ^ZJOB"_$CHAR(9)_"ZJob",tmgCt=tmgCt+1
  ;"SET tmgMenu(tmgCt)="View TRACE log from last run"_$CHAR(9)_"Trace",tmgCt=tmgCt+1
  ;
  SET tmgUserSelect=$$MENU^TMGUSRI2(.tmgMenu,"^")
  KILL tmgMenu ;"Prevent from cluttering variable table during debug run
  ;
  IF tmgUserSelect="AllInOne" GOTO MENUDONE
  IF tmgUserSelect="StartController" DO Controller^TMGIDE3 GOTO M1
  IF tmgUserSelect="StartSender" DO Sender^TMGIDE4() GOTO M1
  IF tmgUserSelect="SetBreakpoint" DO BKPT GOTO M1
  IF tmgUserSelect="KillBreakpoint" DO KBKPT GOTO M1
  IF tmgUserSelect="Interrupt" DO PICKINTR^TMGIDE5 GOTO M1
  IF tmgUserSelect="KillOther" DO KILLOTHER GOTO M1
  IF tmgUserSelect="ZJob" DO ^ZJOB GOTO M1
  IF tmgUserSelect="EditHIDE" DO SetupSkips^TMGIDE2C GOTO M1
  IF tmgUserSelect="Trace" DO ShowTrace^TMGIDE6 GOTO M1
  IF tmgUserSelect="DataWatch" DO  GOTO MENUDONE:(tmgDataWatchMode) GOTO M1
  . DO EditWatch^TMGIDE7(.tmgDataWatchMode)
  IF tmgUserSelect="^" GOTO MENUDN2
  IF tmgUserSelect=0 SET tmgUserSelect=""
  GOTO M1
MENUDONE  ;
  DO
  . NEW IDX FOR IDX=1:1:10 WRITE !
  WRITE !,"Welcome to the TMG debugging environment",!
  WRITE "Enter any valid M command...",!
  DO SETERRTRAP
  DO PROMPT("AllInOne")
MENUDN2  ;
  DO SHUTDOWN
  QUIT
  ;
DIRDEBUG(tmgCodeLine) ;"Direct Debug
  ;"Purpose: Direct run of debugger with specified line
  NEW tmgDbgOptions
  DO DEBUGINIT
  DO SETERRTRAP
  DO PROMPT("AllInOne",.tmgCodeLine)
  DO SHUTDOWN
  QUIT
  ;
DEBUGINIT  ;
  SET tmgDbgOptions("TRACE")=0 ;"Turn off trace record by default
  SET tmgDbgOptions("VARTRACE")=0 ;"Turn off trace vars by default
  KILL ^TMG("TMGIDE",$J,"TRACE") ;"Delete former trace record when starting NEW run
  KILL ^TMG("TMGIDE",$J,"VARTRACE") ;"Delete former var trace record when starting NEW run       
  KILL ^TMG("TMGIDE",$J,"MODULES") ;"//kt 1/7/15 -- Should be like RESYNC whenever starting up.
  ;
  SET $ZSTEP="" ;"Temporarily clear, in case active from prior run. <-- doesn't work...
  DO ENSURENV ;"Ensure fileman environment setup.
  DO CLRDEADINFO  ;"clear out any old data from dead jobs.
  ;"Set up variables with global scope (used by TMGIDE2)
  IF $$GETSCRSZ^TMGKERNL(,.tmgScrWidth)
  IF $GET(tmgScrWidth)="" SET tmgScrWidth=$GET(IOM,66)-1
  IF $GET(tmgScrHeight)="" SET tmgScrHeight=10
  SET tmgLROffset=0
  SET tmgTrap=1
  SET tmgStepMode="into"
  DO SETRUNMODE^TMGIDE2(1)
  SET ^TMG("TMGIDE",$J,"RUNMODE")=tmgRunMode ;"//kt 1/7/15
  SET tmgDataWatchMode=0
  SET tmgZTRAP=$ZTRAP
  DO SETHIDELIST
  DO SETGBLCO^TMGUSRI8
  DO EnsureBreakpoints^TMGIDE2C()
  DO INITCOLORS^TMGIDE6
  QUIT
  ;
SETHIDELIST  ;
  SET tmgDbgHideList=$NAME(^TMG("TMGIDE",$J,"HIDE LIST"))
  KILL @tmgDbgHideList
  IF 1=1 DO
  . SET @tmgDbgHideList@("TMGIDE*")=""
  ELSE  DO
  . SET @tmgDbgHideList@("TMGIDE")=""
  . SET @tmgDbgHideList@("TMGIDE1")=""
  . SET @tmgDbgHideList@("TMGIDE2")=""
  . SET @tmgDbgHideList@("TMGIDE3")=""
  . SET @tmgDbgHideList@("TMGIDE4")=""
  . SET @tmgDbgHideList@("TMGIDE5")=""
  . SET @tmgDbgHideList@("TMGIDE6")=""
  SET @tmgDbgHideList@("TMGTERM")=""
  SET @tmgDbgHideList@("TMGKE*")=""
  SET @tmgDbgHideList@("TMGSTUTL")=""
  SET @tmgDbgHideList@("TMGSTUT2")=""
  SET @tmgDbgHideList@("TMGSTUT3")=""
  SET @tmgDbgHideList@("X*")=""
  ;"SET @tmgDbgHideList@("%*")=""
  SET @tmgDbgHideList@("DI*")=""
  SET @tmgDbgHideList@("%ZVE")=""
  SET @tmgDbgHideList@("%ZVEMK")=""
  SET @tmgDbgHideList@("XLFSTR")=""
  SET @tmgDbgHideList@("XGF")=""
  SET @tmgDbgHideList@("XGKB")=""
  NEW AMOD SET AMOD=""
  FOR  SET AMOD=$ORDER(^TMP("TMGIDE NOHIDE",$J,AMOD)) QUIT:AMOD=""  DO
  . KILL @tmgDbgHideList@(AMOD)
  QUIT
  ;
  ;"-------------------------------------------------------------------
  ; 
SETERRTRAP  ;  
  SET $ZTRAP="DO ERRTRAP^TMGIDE2($ZPOS) break"
  SET $ZSTATUS=""
  QUIT
  ;
PROMPT(tmgMode,tmgCodeLine)    ;
  ;"Purpose: to interact with user and run through code.
  ;"Input: tmgMode: OPTIONAL: Default is 'AllInOne'
  ;"          AllInOne --> debug output to same window
  ;"          SendOut --> debug output to Controller widow
  ;"       tmgCodeLine -- OPTIONAL.  If specified, then code will
  ;"                             be executed in the debugger, and
  ;"                           and when done, the debugger will exit.
  ;"Result : none
  ;
PPT1 ;
  SET tmgMode=$GET(tmgMode,"AllInOne")
  NEW tmgDbgBlankLine
  NEW tmgHxI SET tmgHxI=""
  NEW tmgDbgLine SET tmgDbgLine=""
  NEW tmgLastLine
  SET tmgStepMode="INTO"
  SET tmgCodeLine=$GET(tmgCodeLine)
  DO SETUPVARS
  DO INITKB^XGF()  ;"SET up keyboard input escape code processing
PPT2 ;
  IF $DATA(tmgCodeLine)=0 WRITE !,"Restoring TMGIDE variables...",! GOTO PPT1
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
  DO XLTPROMPTKEYS(.tmgDbgLine,tmgXGRT)
  ;
  IF tmgDbgLine="?" DO SHOWHELP GOTO PPT2
  IF tmgDbgLine="<DN>" SET tmgDbgLine=$$GETHX(.tmgHxI,1) GOTO PPT2
  IF tmgDbgLine="<UP>" SET tmgDbgLine=$$GETHX(.tmgHxI,-1) GOTO PPT2
  IF tmgDbgLine="^" SET $ZSTEP="" GOTO PPTDN
  IF $$UP^XLFSTR($EXTRACT(tmgDbgLine,1,6))="OPTION" DO  GOTO:(tmgDbgLine="") PPT2
  . NEW temp SET temp=tmgDbgLine
  . SET tmgDbgLine=$$GETMENUCODE($$TRIM^XLFSTR($EXTRACT(tmgDbgLine,7,999)))
  . IF tmgDbgLine="" DO  QUIT
  . . WRITE "Couldn't find OPTION code to execute.  Please try again.",!
  . WRITE "Executing code: ",tmgDbgLine,!
  ;
  WRITE !
  IF tmgCodeLine="" DO SAVEHX(tmgDbgLine)
  IF $GET(tmgDataWatchMode) DO SETRUNMODE^TMGIDE2(4)
  ELSE  DO
  . IF $GET(tmgRunMode)=4 QUIT
  . DO SETRUNMODE^TMGIDE2(1)  ;"1=Step-by-step mode
  SET ^TMG("TMGIDE",$J,"RUNMODE")=tmgRunMode ;"//kt 1/7/15
  ;
  SET $ZSTEP="N tmgTrap S tmgTrap=$$STEPTRAP^TMGIDE2($ZPOS) ZSTEP:(tmgTrap=1) into ZSTEP:(tmgTrap=2) over ZSTEP:(tmgTrap=3) outof zcontinue"
  ZSTEP INTO
  ;"consider wrapping the following line in an error trap.  But would have to be cleared
  ;"  somehow to allow QUIT command...
  XECUTE tmgDbgLine
  SET $ZSTEP=""  ;"turn off step capture
  WRITE !
  ;
  IF '$DATA(tmgDbgBlankLine) DO SETUPVARS  ;"without this, crash after running ^XUP
  SET tmgDbgLine="",tmgHxI=""
  SET tmgStepMode="into"
  IF $GET(tmgCodeLine)'="" GOTO PPTDN
  GOTO PPT2 ;"loop for prompt again.
PPTDN ;
  QUIT
  ;
XLTPROMPTKEYS(tmgUserInput,tmgXGRT) ;
  ;"Purpose: translate input keys into a standard output.
  ;"Input: tmgUserInput -- PASS BY REFERENCE.
  SET tmgUserInput=$$XLTDIRKEYS^TMGIDE2B(.tmgUserInput,.tmgXGRT)
  IF tmgUserInput="<RIGHT>" SET tmgUserInput="<DN>"
  IF tmgUserInput="<LEFT>" SET tmgUserInput="<UP>"
  IF tmgUserInput="" SET tmgUserInput="^"
  QUIT
  ;                 
GETHX(IDX,DIR)  ;
  ;"Purpose: to retrieve saved Hx
  ;"Input: IDX -- PASS BY REFERENCE.  IN and OUT parameter
  ;"               This is index of last command retrieved (or should pass as "" IF first time)
  ;"       DIR -- Optional.  Default=1.
  ;"               1 = get previous history item
  ;"              -1 = get next history item
  ;"Result: returns history item line
  NEW result SET result=""
  NEW HXREF SET HXREF=$name(^TMG("TMGIDE",$J,"CMD HISTORY"))
  SET IDX=$ORDER(@HXREF@(IDX),$GET(DIR,1))
  IF IDX'="" SET result=$GET(@HXREF@(IDX))
  QUIT result
  ;
SAVEHX(ALINE)  ;
  ;"Purpose: To provide interface to saving command line hx.
  ;"Input: ALINE -- the line to store
  ;"Output: Will store hx as follows:
  ;"       ^TMG('TMGIDE',$J,'CMD HISTORY',1)=1st line of Hx
  ;"       ^TMG('TMGIDE',$J,'CMD HISTORY',2)=2nd line of Hx
  ;"       ...
  NEW HXREF SET HXREF=$name(^TMG("TMGIDE",$J,"CMD HISTORY"))
  NEW IDX SET IDX=+$ORDER(@HXREF@(""),-1)
  IF $GET(@HXREF@(IDX))'=ALINE DO
  . SET @HXREF@(IDX+1)=ALINE
  QUIT
  ;
SHOWHELP ;
  WRITE !,"Here you should enter any valid M command, as would normally be",!
  WRITE "entered at a GTM> prompt.",!
  WRITE "  Examples:  WRITE ""HELLO"",!  or DO ^TMGTEST",!
  WRITE "Or, one can enter:",!
  WRITE "   option SomeMenuOptionName  (e.g. 'option ABSV cre')",!
  WRITE "to launch code for given menu option.",!
  QUIT
  ;
SETUPVARS  ;
  SET tmgMode=$GET(tmgMode,"AllInOne")
  SET $PIECE(tmgDbgBlankLine," ",78)=" "
  SET tmgLastLine=""
  SET tmgHxShowNum=0
  QUIT
  ;
ENSURENV  ;
  ;"Purpose: So ensure Fileman variables SET up.
  IF $text(DT^DICRW)'="" DO
  . DO DT^DICRW  ;"ensure fileman's required variables are in place
  IF +$GET(DUZ)'>0 DO
  . WRITE "Entering TMG IDE.  But first, let's SET up an environment..."
  . NEW DIC SET DIC=200
  . SET DIC(0)="MAEQ"
  . SET DIC("A")="Please type your name: "
  . SET DIC("?")="Please enter your user name, to setup environmental variables."
  . DO ^DIC WRITE !
  . IF +Y'>0 QUIT
  . DO DUZ^XUP(+Y)
  QUIT
  ;
CLRDEADINFO  ;
  ;"Purpose: to clear out any info from dead (prior) runs
  NEW LIVEJOBS
  DO MJOBS^TMGKERNL(.LIVEJOBS)
  NEW JNUM SET JNUM=""
  FOR  SET JNUM=$ORDER(^TMG("TMGIDE",JNUM)) QUIT:(+JNUM'>0)  DO
  . IF $GET(TMGIDEDEBUG) WRITE "Job ",JNUM," is "
  . IF $DATA(LIVEJOBS(JNUM)) DO  QUIT
  . . IF $GET(TMGIDEDEBUG) WRITE "still alive.",!
  . IF $GET(TMGIDEDEBUG) WRITE "still dead... killing it's info.",!
  . KILL ^TMG("TMGIDE",JNUM)
  QUIT
  ;
KILLOTHER  ;
  ;"Purpose: To show currently running jobs, and allow user to KILL on
  ;"Called from TMGIDE
  ;
  NEW tmgARR
K1 ;
  KILL tmgARR
  DO MJOBS^TMGKERNL(.tmgARR)
  KILL tmgARR($J)  ;"don't show this process
  NEW tmgMenu,tmgUserSelect
  NEW IDX,JDX SET IDX="",JDX=1
  FOR  SET IDX=$ORDER(tmgARR(IDX)) QUIT:(IDX="")  DO
  . SET tmgMenu(JDX)="Job "_$GET(tmgARR(IDX))_$CHAR(9)_IDX
  . SET JDX=JDX+1
  IF $DATA(tmgMenu)=0 GOTO KODN
  SET tmgMenu(0)="Pick Job to Kill/Terminate"
  SET tmgUserSelect=$$MENU^TMGUSRI2(.tmgMenu,"^")
  IF tmgUserSelect="^" GOTO KODN
  IF tmgUserSelect=0 SET tmgUserSelect="" GOTO K1
  IF tmgUserSelect=+tmgUserSelect DO KillPID^TMGKERNL(tmgUserSelect) GOTO K1
  GOTO K1
KODN ;
  QUIT
  ;     
GETMENUCODE(MENUNAME) ;
  ;"Purpose: to process input "OPTION: ADAM", or "OPTION ADAM"
  ;"Input: MENUNAME -- everything that comes after OPTION, e.g. ": ADAM"
  ;"Results: returns executable code, or "" IF none found.
  NEW RESULT SET RESULT=""
  NEW MENUTYPE
  NEW DONE SET DONE=0
  FOR  QUIT:DONE=1  do
  . IF ($EXTRACT(MENUNAME,1)?.1A) SET DONE=1 QUIT
  . IF (MENUNAME="") SET DONE=1 QUIT
  . SET MENUNAME=$EXTRACT(MENUNAME,2,999)
  IF MENUNAME="" GOTO GMCDN
  NEW DIC,X,Y
  SET X=MENUNAME
  SET DIC=19,DIC(0)="MEQ"
  DO ^DIC
  IF +Y>0 GOTO GMC2
  WRITE !,"Couldn't find unique match for: ",X,!
  WRITE "Please try typing ",X," again below for complete search.",!
  SET DIC(0)="MAEQ"
GMC1 ;
  DO ^DIC
  IF +Y'>0 GOTO GMCDN
GMC2  ;
  SET MENUTYPE=$PIECE($GET(^DIC(19,+Y,0)),"^",4)
  IF MENUTYPE="R" DO  GOTO:(RESULT="") GMC1
  . NEW ROUTINE SET ROUTINE=$GET(^DIC(19,+Y,25))
  . IF ROUTINE="" DO  QUIT
  . . WRITE !,"Menu option '",$PIECE(Y,"^",2),"' is of type RUN ROUTINE,",!
  . . WRITE "but has no entry for field ROUTINE!",!
  . SET RESULT="DO "_ROUTINE
  ELSE  IF MENUTYPE="A" DO  GOTO:(RESULT="") GMC1
  . SET RESULT=$GET(^DIC(19,+Y,20))
  . IF RESULT="" DO  QUIT
  . . WRITE !,"Menu option '",$PIECE(Y,"^",2),"' is of type ACTION,",!
  . . WRITE "but has no entry for field ENTRY ACTION!",!
  ELSE  DO  GOTO GMC1
  . WRITE !,"Menu option '",$PIECE(Y,"^",2),"' is not of type ACTION or RUN ROUTINE",!
  . KILL X
GMCDN  ;
  QUIT RESULT
  ;
  ;"-------------------------------------------------------------------
SHUTDOWN  ;
  DO CLEANVARS
  DO VTATRIB^TMGTERM(0)
  DO RESETKB^XGF  ;"turn off XGF escape key processing code.
  WRITE "Leaving TMG debugging environment.  Goodbye.",!
  QUIT
  ;
CLEANVARS  ;
  DO KILLGBLC^TMGUSRI8
  KILL tmgStepMode ;" 2/10/06 kt
  KILL ^TMP("TMGIDE",$J,"MODULES")
  KILL tmgLROffset,tmgScrHeight,tmgScrWidth,tmgTrap
  KILL tmgZTRAP,tmgLastLine
  KILL tmgDbgHangTime,tmgDbgRemoteJob,tmgRunMode,tmgXGRT
  KILL tmgZBSigNameLen
  KILL tmgDbgHideList
  KILL tmgDataWatchMode
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
READ()  ;
  ;"Purpose: To read user input, with knowledge of arrow keys
  ;"Result: Will return all user input up to a terminator (RETURN, or a special key)
  ;"        See code in %ZVEMKRN for possible code returns.  <xx> format
  QUIT $$READ2(,604800)  ;"SET timeout to 1 week (604800 secs).
  ;
READ2(XGCHARS,XGTO)   ;"Taken from READ^XGKB
  ;"Purpose: Read a number of characters, using escape processing.
  ;"Input: XGCHARS -- OPTIONAL.  Number of characters to read
  ;"      XGTO  -- OPTIONAL.  Timeout 
  ;"NOTE: uses tmgXGRT in global scope
  ;"Result -- User input is returned.
  ;"       -- Char that terminated the read will be in tmgXGRT
  ;" e.g.  "UP"
  ;"       "PREV"
  ;"       "DOWN"
  ;"       "NEXT"
  ;"       "RIGHT"
  ;"       "LEFT"
  NEW S,XGW1,XGT1,XGSEQ ;"string,window,timer,timer sequence
  KILL DTOUT
  SET tmgXGRT=""
  IF $G(XGTO)="" DO                   ;"set timeout value IF one wasn't passed
  . IF $DATA(XGT) DO  QUIT            ;"if timers are defined
  . . SET XGTO=$ORDER(XGT(0,""))      ;"get shortest time left of all timers
  . . SET XGW1=$PIECE(XGT(0,XGTO,$O(XGT(0,XGTO,"")),"ID"),U,1) ;"get timer's window
  . . SET XGT1=$PIECE(XGT(0,XGTO,$O(XGT(0,XGTO,"")),"ID"),U,3) ;"get timer's name
  . IF $D(XGW) SET XGTO=99999999 QUIT  ;"in emulation read forever
  . SET XGTO=$GET(DTIME,600)
  ;
  IF $G(XGCHARS)>0 READ S#XGCHARS:XGTO SET:'$T DTOUT=1 IF 1 ;"fixed length read
  ELSE  READ S:XGTO SET:'$T DTOUT=1 IF 1 ;"read as many as possible
  SET:$G(DTOUT)&('$D(XGT1)) S=U                          ;"stuff ^
  ;
  IF S="",$ZB=$CHAR(27) DO  
  . NEW S2,ZB SET ZB=$ZB
  . FOR  DO  QUIT:S2=""  
  . . READ S2#1:0 
  . . SET ZB=ZB_S2
  . SET tmgXGRT=$G(^XUTL("XGKB",ZB))
  . SET S=tmgXGRT
  ELSE  SET:$L($ZB) tmgXGRT=$G(^XUTL("XGKB",$ZB))  ;"get terminator if any
  IF $GET(DTOUT),$DATA(XGT1),$DATA(^TMP("XGW",$J,XGW1,"T",XGT1,"EVENT","TIMER")) DO  IF 1 ;"if timed out
  . DO E^XGEVNT1(XGW1,"T",XGT1,"","TIMER")
  ELSE  IF $L(tmgXGRT),$D(^TMP("XGKEY",$J,tmgXGRT)) XECUTE ^(tmgXGRT)  ;"do some action
  ;" this really should be handled by keyboard mapping -- later
  QUIT S
  ;