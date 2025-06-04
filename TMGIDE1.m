TMGIDE1 ;TMG/kst/A debugger/tracer for GT.M ;1/12/25
         ;;1.0;TMG-LIB;**1**;1/12/25
  ;
  ;" A Debug/Tracer for GT.M
  ;"
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;"Copyright (c) 1/12/25  Kevin S. Toppenberg MD
  ;"
  ;"This file is part of the TMG LIBRARY, and may only be used in accordence
  ;" to license terms outlined in separate file TMGLICNS.m, which should 
  ;" always be distributed with this file.
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;"=======================================================================
  ;" API -- Public Functions.
  ;"=======================================================================
  ;"
  ;"=======================================================================
  ;"PRIVATE API FUNCTIONS
  ;"=======================================================================
  ;"
  ;"DEBUGINIT  
  ;"SETHIDELIST  
  ;"ENSURENV  -- ensure Fileman variables SET up.
  ;"CLRDEADINFO  -- clear out any info from dead (prior) runs
  ;"KILLOTHER  -- show currently running jobs, and allow user to KILL on
  ;"GETMENUCODE(MENUNAME) -- process input "OPTION: ADAM", or "OPTION ADAM"
  ;"READ()  -- read user input, with knowledge of arrow keys
  ;"READ2(XGCHARS,XGTO) -- Read a number of characters, using escape processing.
  ;"SETUPVARS  
  ;"SHOWHELP 
  ;"CLEANVARS  
  ;"SHUTDOWN  
  ;"GETHX(IDX,DIR)  
  ;"SAVEHX(ALINE)  
  ;"XLTPROMPTKEYS(tmgUserInput,tmgXGRT)   
  ;"------------------------------------------------------------
  ;"------------------------------------------------------------
  ;
DEBUGINIT()  ;
  SET tmgDbgOptions("TRACE")=0 ;"Turn off trace record by default
  SET tmgDbgOptions("VARTRACE")=0 ;"Turn off trace vars by default
  KILL ^TMG("TMGIDE",$J,"TRACE") ;"Delete former trace record when starting NEW run
  KILL ^TMG("TMGIDE",$J,"VARTRACE") ;"Delete former var trace record when starting NEW run       
  KILL ^TMG("TMGIDE",$J,"MODULES") ;"//kt 1/7/15 -- Should be like RESYNC whenever starting up.
  ;
  SET $ZSTEP="" ;"Temporarily clear, in case active from prior run. <-- doesn't work...
  SET $ECODE=""  ;"clear any residual error codes from prior run (affects $STACK())
  DO ENSURENV ;"Ensure fileman environment setup.
  DO CLRDEADINFO  ;"clear out any old data from dead jobs.
  ;"Set up variables with global scope (used by TMGIDE2)
  IF $$GETSCRSZ^TMGKERNL(,.tmgScrWidth)
  IF $GET(tmgScrWidth)="" SET tmgScrWidth=$GET(IOM,66)-1
  IF $GET(tmgScrHeight)="" SET tmgScrHeight=10
  SET tmgLROffset=0
  SET tmgTrap=1
  SET tmgStepMode="into"
  DO SETRUNMODE^TMGIDE2A(1)
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
  IF 0=1 DO
  . SET @tmgDbgHideList@("TMGIDE*")=""
  ELSE  DO
  . SET @tmgDbgHideList@("TMGIDE")=""
  . SET @tmgDbgHideList@("TMGIDE1")=""
  . SET @tmgDbgHideList@("TMGIDE2")=""
  . SET @tmgDbgHideList@("TMGIDE2A")=""
  . SET @tmgDbgHideList@("TMGIDE2B")=""
  . SET @tmgDbgHideList@("TMGIDE2C")=""
  . SET @tmgDbgHideList@("TMGIDE3")=""
  . SET @tmgDbgHideList@("TMGIDE4")=""
  . SET @tmgDbgHideList@("TMGIDE5")=""
  . SET @tmgDbgHideList@("TMGIDE6")=""
  . SET @tmgDbgHideList@("TMGIDE7")=""
  SET @tmgDbgHideList@("TMGTERM")=""
  IF 0=1 DO
  . SET @tmgDbgHideList@("TMGKE*")=""
  ELSE  DO    
  . SET @tmgDbgHideList@("TMGKERN1")=""
  . SET @tmgDbgHideList@("TMGKERN2")=""      
  . SET @tmgDbgHideList@("TMGKERN3")=""
  . SET @tmgDbgHideList@("TMGKERN4")=""
  . SET @tmgDbgHideList@("TMGKERN5")=""
  . SET @tmgDbgHideList@("TMGKERN6")=""
  . SET @tmgDbgHideList@("TMGKERN7")=""
  . SET @tmgDbgHideList@("TMGKERN8")=""
  . SET @tmgDbgHideList@("TMGKERN9")=""
  . SET @tmgDbgHideList@("TMGKERNL")=""
  . SET @tmgDbgHideList@("TMGKIDS")=""
  . SET @tmgDbgHideList@("TMGKJOB")=""
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
SETDOLLARZSTEP ;
  ;"$ZSTEP contains a string value specifying the default action for the ZSTEP command. 
  ;"  $ZSTEP is used only if the ZSTEP command does not specify an action, e.g.
  ;"      ZSTEP INTO:"W $ZPOS,!" <--- This code would override code in $ZSTEP
  ;"  $ZSTEP default normally is "B", causing process to BREAK and enter direct mode.  
  ;"NOTE: Below we are setting up core of the debugger.  For each step, $ZSTEP
  ;"      will be executed.  This will call $$STEPTRAP(), which gives user the
  ;"      chance to interact with the system while the code is paused.  When
  ;"      the user is done and effects an exit from STEPTRAP, then next command
  ;"      will be ZSTEP or ZCONTINUE, depending on result returned.  
  ;"      If a ZSTEP, then the above process will be repeated.  Otherwise
  ;"      ZCONTINUE will resume full speed running without user interaction. 
  NEW TEMP
  SET TEMP="NEW % "
  SET TEMP=TEMP_"SET %=$$STEPTRAP^TMGIDE2($ZPOS) " ;"At each step, call $$SETTRAP() 
  SET TEMP=TEMP_"ZSTEP:(%=1) INTO "                ;"if returns 1, ZSTEP INTO
  SET TEMP=TEMP_"ZSTEP:(%=2) OVER "                ;"if returns 2, ZSTEP OVER
  SET TEMP=TEMP_"ZSTEP:(%=3) OUTOF "               ;"if returns 3, ZSTEP OUTOF
  SET TEMP=TEMP_"ZCONTINUE"                        ;"otherwise ZCONTINUE
  SET $ZSTEP=TEMP  ;"<-- set into $ZSTEP
  QUIT
  ;
SETZSTEPMODE(MODE) ;
  SET MODE=+$GET(MODE)
  IF MODE=1 DO
  . ZSTEP INTO
  ELSE  IF MODE=2 DO
  . ZSTEP OVER
  ELSE  IF MODE=3 DO
  . ZSTEP OUTOF
  ELSE  DO
  . ZCONTINUE
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
  NEW tmgDEVRead2Save
  DO DEV2ARR^TMGKERN1($IO,.tmgDEVRead2Save,,.tmgDEVInfo)  ;"//kt 11/23/24
  USE $IO:(ESCAPE)                                        ;"//kt 11/23/24 
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
  IF $DATA(tmgDEVRead2Save) DO   ;"turn IO back to what it was when coming into this function.    //kt 11/23/24
  . DO RESTORDEV^TMGKERN1(.tmgDEVRead2Save,.tmgDEVInfo)                                         ;"//kt 11/23/24
  QUIT S
  ;  
SETUPVARS  ;
  SET tmgMode=$GET(tmgMode,"AllInOne")
  SET $PIECE(tmgDbgBlankLine," ",78)=" "
  SET tmgLastLine=""
  SET tmgHxShowNum=0
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
SHUTDOWN  ;
  DO CLEANVARS^TMGIDE1
  DO VTATRIB^TMGTERM(0)
  DO CSRSHOW^TMGTERM(1)  ;"show cursor
  DO RESETKB^XGF  ;"turn off XGF escape key processing code.
  SET $ECODE=""  ;"clear any residual error codes from run (affects $STACK())
  WRITE "Leaving TMG debugging environment.  Goodbye.",!
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
XLTPROMPTKEYS(tmgUserInput,tmgXGRT) ;
  ;"Purpose: translate input keys into a standard output.
  ;"Input: tmgUserInput -- PASS BY REFERENCE.
  SET tmgUserInput=$$XLTDIRKEYS^TMGIDE2B(.tmgUserInput,.tmgXGRT)
  IF tmgUserInput="<RIGHT>" SET tmgUserInput="<DN>"
  IF tmgUserInput="<LEFT>" SET tmgUserInput="<UP>"
  IF tmgUserInput="" SET tmgUserInput="^"
  QUIT
  ;