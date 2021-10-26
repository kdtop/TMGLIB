TMGIDE ;TMG/kst/A debugger/tracer for GT.M ;9/6/17
         ;;1.0;TMG-LIB;**1**;03/29/09
 ;
 ;" A Debug/Tracer for GT.M
 ;"
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
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
 ;"
 ;" Dependancies:
 ;"     Uses TMGIDE2,TMGTERM,TMGUSRIF
 ;"           ^DIM,XGF,XINDX7,XINDX8,XINDEX  <-- VA code
 ;"            %ZVEM* (if available)
 ;"
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
 ;"Prompt
 ;"ShutDown
 ;"ParsePos(pos,label,offset,routine,dmod)
 ;"ConvertPos(Pos,pArray)
 ;"ScanMod(Module,pArray)
 ;"BROWSENODES(current,Order,paginate,countNodes)
 ;"ShowNodes(pArray,order,paginate,countNodes)
 ;"ListCt(pArray)
 ;"TrimL(S,TrimCh)
 ;"TrimR(S,TrimCh)
 ;"Trim(S,TrimCh)
 ;"Substitute(S,Match,NewValue)
 ;"REPLACE(IN,SPEC)
 ;"DEBUGWRITE(tmgDbgIndent,s,AddNewline)
 ;"DEBUGINDENT(tmgDbgIndentForced)
 ;"$$ArrayDump(ArrayP,TMGIDX,indent)
 ;"ExpandLine(Pos)
 ;"CREF(X)
 ;"LGR()
 ;"UP(X)
 ;"READ(XGCHARS,XGTO)
 ;"READ2(XGCHARS,XGTO)

 ;"------------------------------------------------------------
 ;"------------------------------------------------------------

START  ;"Purpose: To Launch debugger.   This is the entry point
       ;
       NEW tmgDbgOptions
       DO DebugInit
       NEW tmgUserSelect
M1     NEW tmgMenu
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

       SET tmgUserSelect=$$MENU^TMGUSRI2(.tmgMenu,"^")
       KILL tmgMenu ;"Prevent from cluttering variable table during debug run

       IF tmgUserSelect="AllInOne" GOTO MenuDone
       IF tmgUserSelect="StartController" DO Controller^TMGIDE3 GOTO M1
       IF tmgUserSelect="StartSender" DO Sender^TMGIDE4() GOTO M1
       IF tmgUserSelect="SetBreakpoint" DO BKPT GOTO M1
       IF tmgUserSelect="KillBreakpoint" DO KBKPT GOTO M1
       IF tmgUserSelect="Interrupt" DO PICKINTR^TMGIDE5 GOTO M1
       IF tmgUserSelect="KillOther" DO KillOther GOTO M1
       IF tmgUserSelect="ZJob" DO ^ZJOB GOTO M1
       IF tmgUserSelect="EditHIDE" DO SetupSkips^TMGIDE2 GOTO M1
       IF tmgUserSelect="Trace" DO ShowTrace^TMGIDE6 GOTO M1
       IF tmgUserSelect="DataWatch" DO  GOTO MenuDone:(tmgDataWatchMode) GOTO M1
       . DO EditWatch^TMGIDE7(.tmgDataWatchMode)
       IF tmgUserSelect="^" GOTO Done
       IF tmgUserSelect=0 SET tmgUserSelect=""
       GOTO M1

MenuDone
       DO
       . NEW i FOR i=1:1:10 WRITE !
       WRITE !,"Welcome to the TMG debugging environment",!
       WRITE "Enter any valid M command...",!
       DO SetErrTrap
       DO Prompt("AllInOne")
Done
       DO ShutDown
       QUIT

DIRDEBUG(tmgCodeLine) ;"Direct Debug
        ;"Purpose: Direct run of debugger with specified line
       NEW tmgDbgOptions
       DO DebugInit
       DO SetErrTrap
       DO Prompt("AllInOne",.tmgCodeLine)
       DO ShutDown
       QUIT

DebugInit
       SET tmgDbgOptions("TRACE")=0 ;"Turn off trace record by default
       SET tmgDbgOptions("VARTRACE")=0 ;"Turn off trace vars by default
       KILL ^TMG("TMGIDE",$J,"TRACE") ;"Delete former trace record when starting NEW run
       KILL ^TMG("TMGIDE",$J,"VARTRACE") ;"Delete former var trace record when starting NEW run       
       KILL ^TMG("TMGIDE",$J,"MODULES") ;"//kt 1/7/15 -- Should be like RESYNC whenever starting up.
       ;
       SET $ZSTEP="" ;"Temporarily clear, in case active from prior run. <-- doesn't work...
       DO EnsureEnv ;"Ensure fileman environment setup.
       DO ClrDeadInfo  ;"clear out any old data from dead jobs.
       ;"Set up variables with global scope (used by TMGIDE2)
       IF $$GETSCRSZ^TMGKERNL(,.tmgScrWidth)
       IF $GET(tmgScrWidth)="" SET tmgScrWidth=$GET(IOM,66)-1
       IF $GET(tmgScrHeight)="" SET tmgScrHeight=10
       SET tmgLROffset=0
       SET tmgTrap=1
       SET tmgStepMode="into"
       DO SetRunMode^TMGIDE2(1)
       SET ^TMG("TMGIDE",$J,"RUNMODE")=tmgRunMode ;"//kt 1/7/15
       SET tmgDataWatchMode=0
       SET tmgZTRAP=$ZTRAP
       DO SetHideList
       DO SETGBLCO^TMGTERM
       DO EnsureBreakpoints^TMGIDE2()
       DO InitColors^TMGIDE6
       QUIT

SetHideList
       SET tmgDbgHideList=$name(^TMG("TMGIDE",$J,"HIDE LIST"))
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
       ;SET @tmgDbgHideList@("TMGSTUT3")=""
       SET @tmgDbgHideList@("X*")=""
       ;"SET @tmgDbgHideList@("%*")=""
       SET @tmgDbgHideList@("DI*")=""
       SET @tmgDbgHideList@("%ZVE")=""
       SET @tmgDbgHideList@("%ZVEMK")=""
       SET @tmgDbgHideList@("XLFSTR")=""
       SET @tmgDbgHideList@("XGF")=""
       SET @tmgDbgHideList@("XGKB")=""
       QUIT
 ;"-------------------------------------------------------------------
SetErrTrap
       SET $ZTRAP="DO ErrTrap^TMGIDE2($ZPOS) break"
       SET $ZSTATUS=""
       QUIT

Prompt(tmgMode,tmgCodeLine)
       ;"Purpose: to interact with user and run through code.
       ;"Input: tmgMode: OPTIONAL: Default is 'AllInOne'
       ;"          AllInOne --> debug output to same window
       ;"          SendOut --> debug output to Controller widow
       ;"       tmgCodeLine -- OPTIONAL.  If specified, then code will
       ;"                             be executed in the debugger, and
       ;"                           and when done, the debugger will exit.
       ;"Result : none

Ppt1   SET tmgMode=$GET(tmgMode,"AllInOne")
       NEW tmgDbgBlankLine
       NEW tmgHxI SET tmgHxI=""
       NEW tmgDbgLine SET tmgDbgLine=""
       NEW tmgLastLine
       SET tmgStepMode="into"
       SET tmgCodeLine=$GET(tmgCodeLine)
       DO SetupVars
       DO INITKB^XGF()  ;"SET up keyboard input escape code processing

Ppt2   IF $DATA(tmgCodeLine)=0 WRITE !,"Restoring TMGIDE variables...",! GOTO Ppt1
       IF tmgCodeLine="" DO
       . DO CHA^TMGTERM(1) WRITE tmgDbgBlankLine
       . ;"DO CHA^TMGTERM(1) WRITE "(^ to QUIT) //",tmgDbgLine
       . ;"SET tmgDbgLine=$$READKY^TMGUSRI5("er",1200,,tmgDbgLine,.tmgXGRT)
       . ;
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
       DO TranslateKeys(.tmgDbgLine,tmgXGRT)
       ;
       IF tmgDbgLine="?" DO SHOWHELP GOTO Ppt2
       IF tmgDbgLine="<DN>" SET tmgDbgLine=$$GetHx(.tmgHxI,1) GOTO Ppt2
       IF tmgDbgLine="<UP>" SET tmgDbgLine=$$GetHx(.tmgHxI,-1) GOTO Ppt2
       IF tmgDbgLine="^" SET $ZSTEP="" GOTO PptDne
       IF $$UP^XLFSTR($EXTRACT(tmgDbgLine,1,6))="OPTION" DO  GOTO:(tmgDbgLine="") Ppt2
       . NEW temp SET temp=tmgDbgLine
       . SET tmgDbgLine=$$GetMenuCode($$TRIM^XLFSTR($EXTRACT(tmgDbgLine,7,999)))
       . IF tmgDbgLine="" DO  QUIT
       . . WRITE "Couldn't find OPTION code to execute.  Please try again.",!
       . WRITE "Executing code: ",tmgDbgLine,!
       ;
       WRITE !
       IF tmgCodeLine="" DO SaveHx(tmgDbgLine)

       IF $GET(tmgDataWatchMode) DO SetRunMode^TMGIDE2(4)
       ELSE  DO
       . IF $GET(tmgRunMode)=4 QUIT
       . DO SetRunMode^TMGIDE2(1)  ;"1=Step-by-step mode
       SET ^TMG("TMGIDE",$J,"RUNMODE")=tmgRunMode ;"//kt 1/7/15
       
       SET $ZSTEP="N tmgTrap S tmgTrap=$$STEPTRAP^TMGIDE2($ZPOS) ZSTEP:(tmgTrap=1) into ZSTEP:(tmgTrap=2) over ZSTEP:(tmgTrap=3) outof zcontinue"
       ZSTEP into
       ;"consider wrapping the following line in an error trap.  But would have to be cleared
       ;"  somehow to allow QUIT command...
       XECUTE tmgDbgLine
       SET $ZSTEP=""  ;"turn off step capture
       WRITE !
       ;
       IF '$DATA(tmgDbgBlankLine) DO SetupVars  ;"without out this, crash after running ^XUP
       SET tmgDbgLine="",tmgHxI=""
       SET tmgStepMode="into"
       IF $GET(tmgCodeLine)'="" GOTO PptDne
       GOTO Ppt2 ;"loop for prompt again.
PptDne QUIT

TranslateKeys(tmgUserInput,tmgXGRT)
       ;"Purpose: translate input keys into a standard output.
       ;"Input: tmgUserInput -- PASS BY REFERENCE.
       SET tmgXGRT=$GET(tmgXGRT)
       IF tmgXGRT="UP" SET tmgUserInput="A"
       IF tmgXGRT="DOWN" SET tmgUserInput="Z"
       IF tmgXGRT="RIGHT" SET tmgUserInput="]"
       IF tmgXGRT="LEFT" SET tmgUserInput="["
       IF (tmgUserInput="<AU>") SET tmgUserInput="<UP>"
       IF (tmgUserInput="A") SET tmgUserInput="<UP>"
       IF (tmgUserInput="<AD>") SET tmgUserInput="<DN>"
       IF (tmgUserInput="Z") SET tmgUserInput="<DN>"
       IF (tmgUserInput="<AL>") SET tmgUserInput="<LEFT>"
       IF (tmgUserInput="[") SET tmgUserInput="<LEFT>"
       IF (tmgUserInput="<AR>") SET tmgUserInput="<RIGHT>"
       IF (tmgUserInput="]") SET tmgUserInput="<RIGHT>"
       ;"
       IF tmgUserInput="<RIGHT>" SET tmgUserInput="<DN>"
       IF tmgUserInput="<LEFT>" SET tmgUserInput="<UP>"
       IF tmgUserInput="" SET tmgUserInput="^"
       QUIT

GetHx(tmgHxI,Dir)
       ;"Purpose: to retrieve saved Hx
       ;"Input: tmgHxI -- PASS BY REFERENCE.  IN and OUT parameter
       ;"               This is index of last command retrieved (or should pass as "" IF first time)
       ;"       Dir -- Optional.  Default=1.
       ;"               1 = get previous history item
       ;"              -1 = get next history item
       ;"Result: returns history item line
       NEW result SET result=""
       NEW HxRef SET HxRef=$name(^TMG("TMGIDE",$J,"CMD HISTORY"))
       SET tmgHxI=$ORDER(@HxRef@(tmgHxI),$GET(Dir,1))
       IF tmgHxI'="" SET result=$GET(@HxRef@(tmgHxI))
       QUIT result

SaveHx(OneLine)
       ;"Purpose: To provide interface to saving command line hx.
       ;"Input: OneLine -- the line to store
       ;"Output: Will store hx as follows:
       ;"       ^TMG('TMGIDE',$J,'CMD HISTORY',1)=1st line of Hx
       ;"       ^TMG('TMGIDE',$J,'CMD HISTORY',2)=2nd line of Hx
       ;"       ...
       NEW HxRef SET HxRef=$name(^TMG("TMGIDE",$J,"CMD HISTORY"))
       NEW tmgHxI SET tmgHxI=+$ORDER(@HxRef@(""),-1)
       IF $GET(@HxRef@(tmgHxI))'=OneLine DO
       . SET @HxRef@(tmgHxI+1)=OneLine
       QUIT

SHOWHELP
       WRITE !,"Here you should enter any valid M command, as would normally be",!
       WRITE "entered at a GTM> prompt.",!
       WRITE "  Examples:  WRITE ""HELLO"",!  or DO ^TMGTEST",!
       WRITE "Or, one can enter:",!
       WRITE "   option SomeMenuOptionName  (e.g. 'option ABSV cre')",!
       WRITE "to launch code for given menu option.",!
       QUIT

SetupVars
       SET tmgMode=$GET(tmgMode,"AllInOne")
       SET $PIECE(tmgDbgBlankLine," ",78)=" "
       SET tmgLastLine=""
       SET tmgHxShowNum=0
       QUIT

EnsureEnv
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


ClrDeadInfo
        ;"Purpose: to clear out any info from dead (prior) runs
        NEW LiveJobs
        DO MJOBS^TMGKERNL(.LiveJobs)
        NEW JNum SET JNum=""
        FOR  SET JNum=$ORDER(^TMG("TMGIDE",JNum)) QUIT:(+JNum'>0)  DO
        . IF $GET(TMGIDEDEBUG) WRITE "Job ",JNum," is "
        . IF $DATA(LiveJobs(JNum)) DO  QUIT
        . . IF $GET(TMGIDEDEBUG) WRITE "still alive.",!
        . IF $GET(TMGIDEDEBUG) WRITE "still dead... killing it's info.",!
        . KILL ^TMG("TMGIDE",JNum)
        QUIT

KillOther
        ;"Purpose: To show currently running jobs, and allow user to KILL on
        ;"Called from TMGIDE
        ;
        NEW tmgArray
K1      KILL tmgArray
        DO MJOBS^TMGKERNL(.tmgArray)
        KILL tmgArray($J)  ;"don't show this process
        NEW tmgMenu,tmgUserSelect
        NEW i,j SET i="",j=1
        FOR  SET i=$ORDER(tmgArray(i)) QUIT:(i="")  DO
        . SET tmgMenu(j)="Job "_$GET(tmgArray(i))_$CHAR(9)_i
        . SET j=j+1
        IF $DATA(tmgMenu)=0 DO  GOTO KODone
        SET tmgMenu(0)="Pick Job to Kill/Terminate"
        SET tmgUserSelect=$$MENU^TMGUSRI2(.tmgMenu,"^")
        IF tmgUserSelect="^" GOTO KODone
        IF tmgUserSelect=0 SET tmgUserSelect="" GOTO K1
        IF tmgUserSelect=+tmgUserSelect DO KillPID^TMGKERNL(tmgUserSelect) GOTO K1
        GOTO K1
KODone  QUIT


GetMenuCode(MenuName) ;
        ;"Purpose: to process input "OPTION: ADAM", or "OPTION ADAM"
        ;"Input: MenuName -- everything that comes after OPTION, e.g. ": ADAM"
        ;"Results: returns executable code, or "" IF none found.
        NEW result SET result=""
        NEW MenuType
        NEW done SET done=0
        FOR  QUIT:done=1  do
        . IF ($EXTRACT(MenuName,1)?.1A) SET done=1 QUIT
        . IF (MenuName="") SET done=1 QUIT
        . SET MenuName=$EXTRACT(MenuName,2,999)
        IF MenuName="" GOTO GMCDone
        NEW DIC,X,Y
        SET X=MenuName
        SET DIC=19,DIC(0)="MEQ"
        DO ^DIC
        IF +Y>0 GOTO GMC2
        WRITE !,"Couldn't find unique match for: ",X,!
        WRITE "Please try typing ",X," again below for complete search.",!
        SET DIC(0)="MAEQ"
GMC1    DO ^DIC
        IF +Y'>0 GOTO GMCDone
GMC2    SET MenuType=$PIECE($GET(^DIC(19,+Y,0)),"^",4)
        IF MenuType="R" DO  GOTO:(result="") GMC1
        . NEW routine SET routine=$GET(^DIC(19,+Y,25))
        . IF routine="" DO  QUIT
        . . WRITE !,"Menu option '",$PIECE(Y,"^",2),"' is of type RUN ROUTINE,",!
        . . WRITE "but has no entry for field ROUTINE!",!
        . SET result="DO "_routine
        ELSE  IF MenuType="A" DO  GOTO:(result="") GMC1
        . SET result=$GET(^DIC(19,+Y,20))
        . IF result="" DO  QUIT
        . . WRITE !,"Menu option '",$PIECE(Y,"^",2),"' is of type ACTION,",!
        . . WRITE "but has no entry for field ENTRY ACTION!",!
        ELSE  DO  GOTO GMC1
        . WRITE !,"Menu option '",$PIECE(Y,"^",2),"' is not of type ACTION or RUN ROUTINE",!
        . KILL X
        ;
GMCDone QUIT result

 ;"-------------------------------------------------------------------
ShutDown
       DO CleanVars
       DO VTATRIB^TMGTERM(0)
       DO RESETKB^XGF  ;"turn off XGF escape key processing code.
       WRITE "Leaving TMG debugging environment.  Goodbye.",!
       QUIT

CleanVars
       DO KILLGBLC^TMGTERM
       KILL tmgStepMode ;" 2/10/06 kt
       KILL ^TMP("TMGIDE",$J,"MODULES")
       KILL tmgLROffset,tmgScrHeight,tmgScrWidth,tmgTrap
       KILL tmgZTRAP,tmgLastLine
       KILL tmgDbgHangTime,tmgDbgRemoteJob,tmgRunMode,tmgXGRT
       KILL tmgZBSigNameLen
       KILL tmgDbgHideList
       KILL tmgDataWatchMode
       QUIT

 ;"-------------------------------------------------------------------
BKPT
        ;"Purpose: To ask user for an address, and SET a breakpoint there
        ;"         This can be done from GTM prompt, and debugger will be launched
        ;"         when this address is reached during normal execution.

        read "Enter breakpoint (e.g. Label+8^MyFunct): ",Pos:$GET(DTIME,3600),!
        DO SetBreakpoint^TMGIDE2(Pos)
        SET $ZTRAP=""  ;"This makes sure that Fileman error trap is not active
        QUIT


KBKPT
        ;"Purpose: To ask user for an address, and KILL (release) breakpoint there
        ;"         This can be done from GTM prompt

        READ "Enter breakpoint to be killed (released) (e.g. Label+8^MyFunct): ",Pos,!
        DO RelBreakpoint^TMGIDE2(Pos)
        QUIT


 ;"------------------------------------------------------------
 ;"------------------------------------------------------------
 ;"Support Functions
 ;"
 ;"Note: I copied functions from other modules trying to reduce dependencies
 ;"------------------------------------------------------------
 ;"------------------------------------------------------------

ParsePos(pos,label,offset,routine,dmod)
        ;"NOTE: Duplicate of function in TMGMISC
        ;"Purpose: to convert a pos string (e.g. X+2^ROUTINE$DMOD) into componant parts
        ;"Input: pos -- the string, as example above
        ;"         label -- OUT PARAM, PASS BY REF, would return "x"
        ;"         offset  -- OUT PARAM, PASS BY REF, would return "+2"
        ;"         routine -- OUT PARAM, PASS BY REF, would return "ROUTINE"
        ;"         dmod -- OUT PARAM, PASS BY REF, would return "DMOD"
        ;"Results: none
        ;"Note: results are shortened to 8 characters.

       NEW s
       SET s=$GET(pos)
       SET dmod=$PIECE(s,"$",1) ;"e.g. X+2^ROUTINE$DMOD-->X+2^ROUTINE
       SET routine=$PIECE(s,"^",2)
       ;"SET routine=$EXTRACT(routine,1,8)   //kt remove 3/1/08, NEW GTM needs > 8 chars
       SET label=$PIECE(s,"^",1)
       SET offset=$PIECE(label,"+",2)
       SET label=$PIECE(label,"+",1)
       ;"SET label=$EXTRACT(label,1,8)    //kt remove 3/1/08, NEW GTM needs > 8 chars

       QUIT


ConvertPos(Pos,pArray)
        ;"NOTE: Duplicate of function in TMGMISC
        ;"Purpose: to convert a text positioning line from one that is relative to the last tag/label, into
        ;"              one that is relative to the start of the file
        ;"              e.g. START+8^MYFUNCT --> +32^MYFUNCT
        ;"Input: Pos -- a position, as returned from $ZPOS
        ;"        pArray -- pointer to (name of).  Array holding  holding tag offsets
        ;"              pArray will be in this format:
        ;"              pArray("ModuleA",1,"TAG")="ALabel1"
        ;"              pArray("ModuleA",1,"OFFSET")=1
        ;"              pArray("ModuleA",2,"TAG")="ALabel2"
        ;"              pArray("ModuleA",2,"OFFSET")=9
        ;"              pArray("ModuleA","Label1")=1
        ;"              pArray("ModuleA","Label2")=2
        ;"              pArray("ModuleA","Label3")=3
        ;"              pArray("ModuleB",1,"TAG")="BLabel1"
        ;"              pArray("ModuleB",1,"OFFSET")=4
        ;"              pArray("ModuleB",2,"TAG")="BLabel2"
        ;"              pArray("ModuleB",2,"OFFSET")=23
        ;"              pArray("ModuleB","Label1")=1
        ;"              pArray("ModuleB","Label2")=2
        ;"              pArray("ModuleB","Label3")=3
        ;"            NOTE: -- IF array passed is empty, then this function will call ScanModule to fill it
        ;"Result: returns the NEW position line, relative to the start of the file/module
        ;"

        NEW cpS
        NEW cpResult SET cpResult=""
        NEW cpRoutine,cpLabel,cpOffset

        SET cpS=$PIECE(Pos,"$",1)  ;"e.g. X+2^ROUTINE$DMOD-->X+2^ROUTINE
        IF cpS="" DO  GOTO CPDone
        . WRITE "Parse error: Nothing before $ in",cpS,!

        SET cpRoutine=$PIECE(cpS,"^",2)
        IF cpRoutine="" DO  GOTO CPDone
        . WRITE "Parse error:  No routine specified in: ",cpS,!

        SET cpS=$PIECE(cpS,"^",1)
        SET cpOffset=+$PIECE(cpS,"+",2)
        ;"IF cpOffset="" SET cpOffset=1
        ;"ELSE  SET cpOffset=+cpOffset
        SET cpLabel=$PIECE(cpS,"+",1)
        IF cpLabel="" set cpLabel="+1"  ;"//kt 6/8/16
        IF $DATA(@pArray@(cpRoutine))=0 DO
        . NEW p2Array SET p2Array=$name(@pArray@(cpRoutine))
        . DO ScanMod(cpRoutine,p2Array)
        ;"DEBUG, REMOVE LATER --> w !,"Pos='",Pos,"'",!       
        ;"DEBUG, REMOVE LATER --> w "pArray='",pArray,"', cpRoutine='",cpRoutine,"', cpLabel='",cpLabel,"'",!
        NEW cpIdx SET cpIdx=+$GET(@pArray@(cpRoutine,cpLabel))
        IF cpIdx=0 DO  GOTO CPDone
        . ;"WRITE "Parse error: Can't find ",cpRoutine,",",cpLabel," in stored source code.",!
        NEW cpGOffset SET cpGOffset=@pArray@(cpRoutine,cpIdx,"OFFSET")
        SET cpResult="+"_+(cpGOffset+cpOffset)_"^"_cpRoutine

CPDone
        QUIT cpResult


RelConvertPos(Pos,ViewOffset,pArray)
        ;"Purpose: to convert a positioning line from one that is relative to
        ;"              the start of the file to one that is relative to the
        ;"              last tag/label
        ;"              e.g. +32^MYFUNCT --> START+8^MYFUNCT
        ;"          I.e. this function in the OPPOSITE of ConvertPos
        ;"Input: Pos -- a position, as returned from $ZPOS
        ;"       ViewOffset -- the offset from the Pos to get pos for
        ;"       pArray -- pointer to (name of).  Array holding  holding tag offsets
        ;"             see Description in ConvertPos()
        ;"Result: returns the NEW position line, relative to the start of the last tag/label

        ;"WRITE !,"Here in RelConvertPos.  Pos=",Pos," ViewOffset=",ViewOffset,!
        NEW zbRelPos,zbLabel,zbOffset,zbRoutine
        DO ParsePos^TMGIDE(Pos,.zbLabel,.zbOffset,.zbRoutine)
        SET zbRelPos=zbLabel_"+"_+(zbOffset+ViewOffset)_"^"_zbRoutine
        NEW zbTemp SET zbTemp=zbRelPos
        ;"5/27/07 I don't know why following line was here. Removing.
        ;"It was breaking the setting of breakpoints.  I wonder IF I have now
        ;"broken conditional breakpoints...  Figure that out later...
        ;"SET zbRelPos=$$ConvertPos^TMGIDE(zbRelPos,pArray)
        IF zbRelPos="" DO
        . WRITE "Before ConvertPos, zbRelPos=",zbTemp,!
        . WRITE "Afterwards, zbRelPos=""""",!
        ;"WRITE "Done RelConvertPos.  Result=",zbRelPos,!
        QUIT zbRelPos


ScanMod(Module,pArray)
        ;"NOTE: Duplicate of function in TMGMISC
        ;"Purpose: To scan a module and find all the labels/entry points/Entry points
        ;"Input: Module -- The name of the module, like "XGF" (not "XGF.m" or "^XGF")
        ;"         pArray -- pointer to (NAME OF) array Will be filled like this
        ;"              pArray(1,"TAG")="Label1"
        ;"              pArray(1,"OFFSET")=1
        ;"              pArray(2,"TAG")="Label2"
        ;"              pArray(2,"OFFSET")=9
        ;"              pArray(3,"TAG")="Label3"  etc.
        ;"              pArray(3,"OFFSET")=15
        ;"              pArray("Label1")=1
        ;"              pArray("Label2")=2
        ;"              pArray("Label3")=3
        ;"
        ;"              NOTE: there seems to be a problem IF the passed pArray value is "pArray",
        ;"                      so use another name.
        ;"
        ;"Output: Results are put into array
        ;"Result: none

        NEW smIdx SET smIdx=1
        NEW LabelNum SET LabelNum=0
        NEW smLine SET smLine=""
        IF $GET(Module)="" GOTO SMDone
        ;"look for a var with global scope to see how how many characters are significant to GT.M
        IF $GET(tmgZBSigNameLen)="" DO
        . SET tmgZBSigNameLen=$$NumSigChs^TMGMISC()

        FOR  DO  QUIT:(smLine="")
        . NEW smCh
        . SET smLine=$text(+smIdx^@Module)
        . IF smLine="" QUIT
        . SET smLine=$$REPLSTR^TMGSTUT3(smLine,$Char(9),"        ") ;"replace tabs for 8 spaces
        . SET smCh=$EXTRACT(smLine,1)
        . IF (smCh'=" ")&(smCh'=";") DO
        . . NEW label
        . . SET label=$PIECE(smLine," ",1)
        . . SET label=$PIECE(label,"(",1)  ;"MyFunct(X,Y) --> MyFunct
        . . SET label=$EXTRACT(label,1,tmgZBSigNameLen)
        . . SET LabelNum=LabelNum+1
        . . SET @pArray@(LabelNum,"TAG")=label
        . . SET @pArray@(LabelNum,"OFFSET")=smIdx
        . . SET @pArray@(label)=LabelNum
        . SET smIdx=smIdx+1

SMDone
        QUIT



BROWSENODES(current,Order,paginate,countNodes)
        ;"NOTE: Duplicate of function in TMGMISC
        ;"Purpose: to display nodes of specified array
        ;"Input: Current -- The reference to display
        ;"       order -- OPTIONAL, default=1; 1 for forward, -1 for backwards order
        ;"       paginate -- OPTIONAL, default=0;  0=no pagination, 1=pause after each page
        ;"       countNodes -- OPTIONAL, default=0; 1=show number of child nodes.

        NEW parent,child
        SET parent=""
        SET order=$GET(order,1)
        SET paginate=$GET(paginate,0)
        SET countNodes=$GET(countNodes,0)

        NEW len SET len=$LENGTH(current)
        NEW lastChar SET lastChar=$EXTRACT(current,len)
        IF lastChar'=")" DO
        . IF current'["(" QUIT
        . IF lastChar="," SET current=$EXTRACT(current,1,len-1)
        . IF lastChar="(" SET current=$EXTRACT(current,1,len-1) QUIT
        . SET current=current_")"

BNLoop
        IF current="" GOTO BNDone
        SET child=$$ShowNodes(current,order,paginate,countNodes)
        IF child'="" DO
        . SET parent(child)=current
        . SET current=child
        ELSE  SET current=$GET(parent(current))
        GOTO BNLoop
BNDone
        QUIT


ShowNodes(pArray,order,paginate,countNodes)
        ;"NOTE: Duplicate of function in TMGMISC
        ;"Purpose: To display all the nodes of the given array
        ;"Input: pArray -- NAME OF array to display
        ;"       order -- OPTIONAL, default=1; 1 for forward, -1 for backwards order
        ;"       paginate -- OPTIONAL, default=0;  0=no pagination, 1=pause after each page
        ;"       countNodes -- OPTIONAL, default=0; 1=show number of child nodes.
        ;"Results: returns NAME OF next node to display (or "" IF none)

        NEW TMGi
        NEW count SET count=1
        NEW Answers
        NEW someShown SET someShown=0
        NEW abort SET abort=0
        SET paginate=$GET(paginate,0)
        NEW pageCount SET pageCount=0
        NEW pageLen SET pageLen=20
        SET countNodes=$GET(countNodes,0)

        WRITE pArray,!
        SET TMGi=$ORDER(@pArray@(""),order)
        IF TMGi'="" FOR  DO  QUIT:(TMGi="")!(abort=1)
        . WRITE count,".  +--[",TMGi,"]"
        . IF countNodes=1 WRITE "(",$$ListCt($name(@pArray@(TMGi))),")"
        . WRITE "=",$EXTRACT($GET(@pArray@(TMGi)),1,40),!
        . SET someShown=1
        . SET Answers(count)=$name(@pArray@(TMGi))
        . SET count=count+1
        . NEW zbTemp READ *zbTemp:0
        . IF zbTemp'=-1 SET abort=1
        . SET pageCount=pageCount+1
        . IF (paginate=1)&(pageCount>pageLen) DO
        . . NEW zbTemp
        . . READ "Press [ENTER] to continue (^ to stop list)...",zbTemp:$GET(DTIME,3600),!
        . . IF zbTemp="^" SET abort=1
        . . SET pageCount=0
        . SET TMGi=$ORDER(@pArray@(TMGi),order)

        IF someShown=0 WRITE "   (no data)",!
        WRITE !,"Enter # to browse (^ to backup): ^//"
        NEW zbTemp READ zbTemp:$GET(DTIME,3600),!

        NEW result SET result=$GET(Answers(zbTemp))

        QUIT result


ListCt(pArray)
        ;"NOTE: Duplicate of function in TMGMISC
        ;"SCOPE: PUBLIC
        ;"Purpose: to count the number of entries in an array
        ;"Input: pointer to (name of) array to test.
        ;"Output: the number of entries at highest level
        ;"      e.g. Array("TELEPHONE")=1234
        ;"            Array("CAR")=4764
        ;"            Array("DOG")=5213
        ;"            Array("DOG","COLLAR")=5213  <-- not highest level,not counted.
        ;"        The above array would have a count of 3
        NEW i,result SET result=0

        DO
        . NEW $ETRAP
        . SET $ETRAP="WRITE ""?? Error Trapped ??"",! SET $ECODE="""" QUIT"
        . SET i=$ORDER(@pArray@(""))
        . IF i="" QUIT
        . FOR  SET result=result+1 SET i=$ORDER(@pArray@(i)) QUIT:i=""

        QUIT result


TrimL(S,TrimCh)
        ;"NOTE: Duplicate of function in TMGSTUTL
        ;"Purpose: To a trip a string of leading white space
        ;"        i.e. convert "  hello" into "hello"
        ;"Input: S -- the string to convert.  Won't be changed IF passed by reference
        ;"      TrimCh -- OPTIONAL: Charachter to trim.  Default is " "
        ;"Results: returns modified string
        ;"Note: processing limitation is string length=1024
        SET TrimCh=$GET(TrimCh," ")
        NEW result SET result=$GET(S)
        NEW Ch SET Ch=""
        FOR  DO  QUIT:(Ch'=TrimCh)
        . SET Ch=$EXTRACT(result,1,1)
        . IF Ch=TrimCh DO
        . . SET result=$EXTRACT(result,2,1024)
        QUIT result


TrimR(S,TrimCh)
        ;"NOTE: Duplicate of function in TMGSTUTL
        ;"Purpose: To a trip a string of trailing white space
        ;"        i.e. convert "hello   " into "hello"
        ;"Input: S -- the string to convert.  Won't be changed IF passed by reference
        ;"      TrimCh -- OPTIONAL: Charachter to trim.  Default is " "
        ;"Results: returns modified string
        ;"Note: processing limitation is string length=1024

        SET TrimCh=$GET(TrimCh," ")

        NEW result SET result=$GET(S)
        NEW Ch SET Ch=""
        NEW L

        FOR  DO  QUIT:(Ch'=TrimCh)
        . SET L=$LENGTH(result)
        . SET Ch=$EXTRACT(result,L,L)
        . IF Ch=TrimCh DO
        . . SET result=$EXTRACT(result,1,L-1)

        QUIT result


Trim(S,TrimCh)
        ;"NOTE: Duplicate of function in TMGSTUTL
        ;"Purpose: To a trip a string of leading and trailing white space
        ;"        i.e. convert "    hello   " into "hello"
        ;"Input: S -- the string to convert.  Won't be changed IF passed by reference
        ;"      TrimCh -- OPTIONAL: Charachter to trim.  Default is " "
        ;"Results: returns modified string
        ;"Note: processing limitation is string length=1024

        SET TrimCh=$GET(TrimCh," ")

        NEW result SET result=$GET(S)
        SET result=$$TrimL(.result,TrimCh)
        SET result=$$TrimR(.result,TrimCh)

        QUIT result



Substitute(S,Match,NewValue)
        ;"NOTE: Duplicate of function in TMGSTUTL
        ;"PUBLIC FUNCTION
        ;"Purpose: to look for all instances of Match in S, and replace with NewValue
        ;"Input: S - string to alter.  Altered IF passed by reference
        ;"       Match -- the sequence to look for, i.e. '##'
        ;"       NewValue -- what to replace Match with, i.e. '$$'
        ;"Note: This is different than $TRANSLATE, as follows
        ;"      $TRANSLATE("ABC###DEF","###","*") --> "ABC***DEF"
        ;"      $$REPLSTR^TMGSTUT3("ABC###DEF","###","*") --> "ABC*DEF"
        ;"Result: returns altered string (if any alterations indicated)
        ;"Output: S is altered, IF passed by reference.

        NEW spec
        SET spec($GET(Match))=$GET(NewValue)
        SET S=$$REPLACE(S,.spec)
        QUIT S


REPLACE(IN,SPEC)        ;"See $$REPLACE in MDC minutes.
        ;"Taken from REPLACE^XLFSTR
        QUIT:'$D(IN) ""
        QUIT:$D(SPEC)'>9 IN
        N %1,%2,%3,%4,%5,%6,%7,%8
        SET %1=$L(IN)
        SET %7=$J("",%1)
        SET %3=""
        SET %6=9999
        FOR  SET %3=$ORDER(SPEC(%3)) QUIT:%3=""  SET %6(%6)=%3,%6=%6-1
        FOR %6=0:0 SET %6=$O(%6(%6)) QUIT:%6'>0  SET %3=%6(%6) DO:$D(SPEC(%3))#2 RE1
        SET %8=""
        FOR %2=1:1:%1 DO RE3
        QUIT %8
RE1     SET %4=$L(%3)
        SET %5=0 FOR  S %5=$F(IN,%3,%5) Q:%5<1  D RE2
        Q
RE2     Q:$E(%7,%5-%4,%5-1)["X"  S %8(%5-%4)=SPEC(%3)
        F %2=%5-%4:1:%5-1 S %7=$E(%7,1,%2-1)_"X"_$E(%7,%2+1,%1)
        Q
RE3     I $E(%7,%2)=" " S %8=%8_$E(IN,%2) Q
        S:$D(%8(%2)) %8=%8_%8(%2)
        Q


KeyPress(wantChar,waitTime)
        ;"NOTE: Duplicate of function in TMGUSRIF
        ;"Purpose: to check for a keypress
        ;"Input: wantChar -- OPTIONAL, IF 1, then Character is returned, not ASCII value
        ;"       waitTime -- OPTIONAL, default is 0 (immediate return)
        ;"Result: ASCII value of key, IF pressed, -1 otherwise ("" IF wantChar=1)
        ;"Note: this does NOT wait for user to press key

        NEW zbTemp
        SET waitTime=$GET(waitTime,0)
        READ *zbTemp:waitTime
        IF $GET(wantChar)=1 SET zbTemp=$CHAR(zbTemp)
        QUIT zbTemp



DEBUGWRITE(tmgDbgIndent,s,AddNewline)
        ;"NOTE: Duplicate of function in TMGIDEDEBUG
        ;"PUBLIC FUNCTION
        ;"Purpose: to WRITE debug output.  Having the proc separate will allow
        ;"        easier dump to file etc.
        ;"Input:tmgDbgIndent, the amount of indentation expected for output.
        ;"        s -- the text to write
        ;"      AddNewline -- boolean, 1 IF ! (i.e. newline) should be written after s

        ;"Relevant DEBUG values
        ;"        cdbNone - no debug (0)
        ;"        cdbToScrn - Debug output to screen (1)
        ;"        cdbToFile - Debug output to file (2)
        ;"        cdbToTail - Debug output to X tail dialog box. (3)
        ;"Note: If above values are not defined, then functionality will be ignored.

        SET TMGIDEDEBUG=$GET(TMGIDEDEBUG,0)
        IF TMGIDEDEBUG=0 QUIT
        IF (TMGIDEDEBUG=2)!(TMGIDEDEBUG=3),$DATA(DebugFile) use DebugFile
        WRITE s
        IF $GET(AddNewline)=1 DO
        . NEW ENDSPACE SET ENDSPACE=20
        . IF +$GET(IOM)>0,(IOM-$X)<20 SET ENDSPACE=IOM-$X
        . NEW IDX FOR IDX=1:1:ENDSPACE WRITE " "        
        . WRITE !
        IF (TMGIDEDEBUG=2)!(TMGIDEDEBUG=3) use $PRINCIPAL
        QUIT


DEBUGINDENT(tmgDbgIndent,Forced)
        ;"NOTE: Duplicate of function in TMGIDEDEBUG
        ;"PUBLIC FUNCTION
        ;"Purpose: to provide a unified indentation for debug messages
        ;"Input: tmgDbgIndent = number of indentations
        ;"       Forced = 1 IF to indent regardless of DEBUG mode

        SET Forced=$GET(Forced,0)

        IF ($GET(TMGIDEDEBUG,0)=0)&(Forced=0) QUIT
        NEW i
        FOR i=1:1:tmgDbgIndent DO
        . IF Forced DO DEBUGWRITE(tmgDbgIndent,"  ")
        . ELSE  DO DEBUGWRITE(tmgDbgIndent,". ")
        QUIT


ArrayDump(ArrayP,TMGIDX,indent)
        ;"NOTE: Similar to ARRDUMP^TMGMISC3
        ;"PUBLIC FUNCTION
        ;"Purpose: to get a custom version of GTM's "zwr" command
        ;"Input: Uses global scope var tmgDbgIndent (if defined)
        ;"        ArrayP: NAME of global to display, i.e. "^VA(200)"
        ;"        TMGIDX: initial index (i.e. 5 IF wanting to start with ^VA(200,5)
        ;"        indent: spacing from left margin to begin with. (A number.  Each count is 2 spaces)
        ;"          OPTIONAL: indent may be an array, with information about columns
        ;"                to skip.  For example:
        ;"                indent=3, indent(2)=0 --> show | for columns 1 & 3, but NOT 2
        ;"Result: 0=OK to continue, 1=user aborted display

        NEW result SET result=0
        ;"IF $$USRABORT^TMGUSRI2 SET result=1 GOTO ADDone
        NEW $ETRAP SET $ETRAP="SET result="""",$ETRAP="""",$ecode="""""

AD1     IF $DATA(ArrayP)=0 GOTO ADDone
        NEW abort SET abort=0
        IF (ArrayP["@") DO  GOTO:(abort=1) ADDone
        . NEW zbTemp SET zbTemp=$PIECE($EXTRACT(ArrayP,2,99),"@",1)
        . IF $DATA(zbTemp)#10=0 SET abort=1
        ;"Note: I need to DO some validation to ensure ArrayP doesn't have any null nodes.
        DO
        . NEW X SET X="SET ZBTEMP=$GET("_ArrayP_")"
        . SET X=$$UP(X)
        . DO ^DIM ;"a method to ensure ArrayP doesn't have an invalid reference.
        . IF $GET(X)="" SET abort=1
        IF abort GOTO ADDone

        SET tmgDbgIndent=$GET(tmgDbgIndent,0)

        ;"Force this function to output, even IF TMGIDEDEBUG is not defined.
        ;"if $DATA(TMGIDEDEBUG)=0 NEW TMGIDEDEBUG  ;"//kt 1-16-06, doesn't seem to be working
        NEW TMGIDEDEBUG  ;"//kt added 1-16-06
        SET TMGIDEDEBUG=1

        NEW ChildP,TMGi

        SET TMGIDX=$GET(TMGIDX,"")
        SET indent=$GET(indent,0)
        NEW SavIndex SET SavIndex=TMGIDX

        DO DEBUGINDENT(tmgDbgIndent)

        IF indent>0 DO
        . FOR TMGi=1:1:indent-1 DO
        . . NEW s SET s=""
        . . IF $GET(indent(TMGi),-1)=0 SET s="  "
        . . ELSE  SET s="| "
        . . DO DEBUGWRITE(tmgDbgIndent,s)
        . DO DEBUGWRITE(tmgDbgIndent,"}~")

        IF TMGIDX'="" DO
        . IF $DATA(@ArrayP@(TMGIDX))#10=1 DO
        . . NEW s SET s=@ArrayP@(TMGIDX)
        . . IF s="" SET s=""""""
        . . IF $LENGTH(s)'=$LENGTH($$TRIM^XLFSTR(s)) set s=""""_s_""""  ;"//kt 9/6/17
        . . NEW qt SET qt=""
        . . IF +TMGIDX'=TMGIDX SET qt=""""
        . . DO DEBUGWRITE(tmgDbgIndent,qt_TMGIDX_qt_" = "_s,1)
        . ELSE  DO
        . . DO DEBUGWRITE(tmgDbgIndent,TMGIDX,1)
        . SET ArrayP=$name(@ArrayP@(TMGIDX))
        ELSE  DO
        . ;"DO DEBUGWRITE(tmgDbgIndent,ArrayP_"(*)",0)
        . DO DEBUGWRITE(tmgDbgIndent,ArrayP,0)
        . IF $DATA(@ArrayP)#10=1 DO
        . . DO DEBUGWRITE(0,"="_$GET(@ArrayP),0)
        . DO DEBUGWRITE(0,"",1)

        SET TMGIDX=$ORDER(@ArrayP@(""))
        IF TMGIDX="" GOTO ADDone
        SET indent=indent+1

        FOR  DO  QUIT:TMGIDX=""  IF result=1 GOTO ADDone
        . NEW tTMGIDX SET tTMGIDX=$ORDER(@ArrayP@(TMGIDX))
        . IF tTMGIDX="" SET indent(indent)=0
        . NEW tIndent MERGE tIndent=indent
        . SET result=$$ArrayDump(ArrayP,TMGIDX,.tIndent)  ;"Call self recursively
        . SET TMGIDX=$ORDER(@ArrayP@(TMGIDX))

        ;"Put in a blank space at end of subbranch
        DO DEBUGINDENT(tmgDbgIndent)

        IF 1=0,indent>0 DO
        . FOR TMGi=1:1:indent-1 DO
        . . NEW s SET s=""
        . . IF $GET(indent(TMGi),-1)=0 SET s="  "
        . . ELSE  SET s="| "
        . . DO DEBUGWRITE(tmgDbgIndent,s)
        . DO DEBUGWRITE(tmgDbgIndent," ",1)

ADDone
        QUIT result


ExpandLine(Pos)
        ;"NOTE: this is SOLE COPY.  The one in TMGIDEDEBUG has been deleted. 
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
        DO ParsePos(Pos,.label,.offset,.RTN,.dmod)
        IF label'="" DO  ;"change position from one relative to label into one relative to top of file
        . NEW CodeArray
        . SET Pos=$$ConvertPos(Pos,"CodeArray")
        . DO ParsePos(Pos,.label,.offset,.RTN,.dmod)

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
ELDone
        QUIT



CREF(X)
        ;"Taken from CREF^DILF --> ENCREF^DIQGU
        ;"Convert an open reference to a closed reference
        NEW L,X1,X2,X3
        SET X1=$PIECE(X,"(")
        SET X2=$PIECE(X,"(",2,99)
        SET L=$LENGTH(X2)
        SET X3=$TRANSLATE($EXTRACT(X2,L),",)")
        SET X2=$EXTRACT(X2,1,(L-1))_X3

        QUIT X1_$SELECT(X2]"":"("_X2_")",1:"")


LGR()   ;
        ;"Taken from LGR^%ZOSV
        ;" Last global reference ($REFERENCE)
        QUIT $R

UP(X)   ;
        ;"Taken from UP^XLFSTR
        QUIT $TRANSLATE(X,"abcdefghijklmnopqrstuvwxyz","ABCDEFGHIJKLMNOPQRSTUVWXYZ")


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
        ;"Result -- User input is returned.
        ;"       -- Char that terminated the read will be in tmgXGRT
        ;" e.g.  "UP"
        ;"       "PREV"
        ;"       "DOWN"
        ;"       "NEXT"
        ;"       "RIGHT"
        ;"       "LEFT"

        NEW S,XGW1,XGT1,XGSEQ ;string,window,timer,timer sequence
        KILL DTOUT
        SET tmgXGRT=""
        DO:$G(XGTO)=""                      ;"set timeout value IF one wasn't passed
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
        ;"//kt added the following IF block 8/11/13 to handle when keyboard terminator mode keeps getting changed.
        ;"//kt and also the 'ELSE' at the beginning of the line following this block.         
        IF S="",$ZB=$char(27) DO  
        . NEW s2,zb SET zb=$ZB
        . FOR  DO  QUIT:s2=""  
        . . read s2#1:0 
        . . SET zb=zb_s2
        . . ;"WRITE "got [",s2,"]",!
        . SET tmgXGRT=$G(^XUTL("XGKB",zb))
        . ;"WRITE "tmgXGRT=",tmgXGRT,!
        . SET S=tmgXGRT
        ELSE  SET:$L($ZB) tmgXGRT=$G(^XUTL("XGKB",$ZB))          ;"get terminator IF any
        IF $G(DTOUT),$D(XGT1),$D(^TMP("XGW",$J,XGW1,"T",XGT1,"EVENT","TIMER")) DO  IF 1 ;if timed out
        . DO E^XGEVNT1(XGW1,"T",XGT1,"","TIMER")
        ELSE  IF $L(tmgXGRT),$D(^TMP("XGKEY",$J,tmgXGRT)) XECUTE ^(tmgXGRT)     ;do some action
        ; this really should be handled by keyboard mapping -- later
        QUIT S
