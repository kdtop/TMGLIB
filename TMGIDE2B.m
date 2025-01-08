TMGIDE2B ;TMG/kst/A debugger/tracer for YottaDB (Utility functions) ;3/21/24
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
  ;"BLANKLINE -- blank out a line
  ;"CLRLINE --clear out line
  ;"XLTCMDKEYS(tmgAction,tmgXGRT,FORCEUP) -- translate input keys into a standard output.
  ;"XLTDIRKEYS(tmgUserInput,tmgXGRT) -- translate input keys into a standard output.  
  ;"EVALWATCHES -- Run code that evaluates watches.
  ;"SHOWCODE() --  display code at the top of the screen
  ;"=======================================================================
  ;  
BLANKLINE ;
  WRITE tmgBlankLine
  DO CHA^TMGTERM(1) ;"move to x=1 on this line
  QUIT
  ;
CLRLINE ;
  ;"Purpose: clear out line
  NEW loop
  NEW tempX SET tempX=$X
  FOR loop=1:1:20 WRITE " "
  FOR loop=1:1:20 WRITE $CHAR(8) ;"backspace
  SET $X=tempX
  QUIT
  ;
SETTEMPBKCOLOR(tmgMode,COLOR)  ;"Set background color that should be combined with other foreground colors
  ;"Input: tmgMode : Should be Reset,Highlight,HighExecPos,BkPos,HighBkPos,FORCE
  ;"       COLOR: OPTIONAL.  Used only if tmgMode=FORCE, used to force a color number into background 
  SET tmgMode=$GET(tmgMode) QUIT:tmgMode=""
  NEW ref DO GETCOLORSTOREREF^TMGIDE6(.ref)
  NEW COLORMODE SET COLORMODE=$GET(@ref@("MODE"))
  IF tmgMode="Reset" KILL @ref@("TEMP BACKGROUND") QUIT
  IF "Highlight,HighExecPos,BkPos,HighBkPos,FORCE"'[tmgMode QUIT
  IF $DATA(@ref)=0 DO INITCOLORS^TMGIDE6
  IF COLORMODE="24bit" DO
  . NEW bg 
  . IF tmgMode="FORCE" set bg=COLOR
  . ELSE  SET bg=$GET(@ref@(tmgMode,"24bit"))
  . IF bg="" QUIT
  . SET @ref@("TEMP BACKGROUND","24bit")=bg
  ELSE  DO  
  . NEW bg SET bg=$GET(@ref@(tmgMode))
  . IF tmgMode="FORCE" set bg=COLOR
  . IF bg="" QUIT
  . SET @ref@("TEMP BACKGROUND")=bg
  QUIT
  ;  
XLTCMDKEYS(tmgAction,tmgXGRT,FORCEUP)  ;
  ;"Purpose: translate input keys into a standard output.
  ;"Input: tmgAction -- PASS BY REFERENCE.  
  ;"       tmgXGRT
  ;"       FORCEUP -- OPTIONAL.  Default = 1. If 1, then change input to UPPER CASE
  ;"Result: none -- changes passed back through tmgAction.
  SET tmgAction=$GET(tmgAction)
  SET FORCEUP=$GET(FORCEUP,1)
  IF FORCEUP SET tmgAction=$$UP^XLFSTR(tmgAction)
  SET tmgAction=$$XLTDIRKEYS(.tmgAction,.tmgXGRT)  ;  
  IF (tmgAction="^") SET tmgAction="Q"
  IF "wW"[$PIECE(tmgAction," ",1) SET tmgAction="W"
  QUIT
  ;
XLTDIRKEYS(tmgUserInput,tmgXGRT)  ;
  ;"Purpose: translate input keys into a standard output.
  ;"Input: tmgUserInput 
  ;"       tmgXGRT
  NEW INPUT SET INPUT=$get(tmgUserInput)
  NEW UPINPUT SET UPINPUT=$$UP^XLFSTR(tmgUserInput)   
  NEW RESULT SET RESULT=INPUT
  SET tmgXGRT=$GET(tmgXGRT)
  IF tmgXGRT="UP" SET INPUT="A"  
  IF tmgXGRT="PREV" SET INPUT="AA"
  IF tmgXGRT="DOWN" SET INPUT="Z"
  IF tmgXGRT="NEXT" SET INPUT="ZZ"
  IF tmgXGRT="RIGHT" SET INPUT="]"
  IF tmgXGRT="LEFT" SET INPUT="["   
  IF $$SETIFMATCH(INPUT,UPINPUT,"<AU>","<UP>",.RESULT) GOTO XLCDN
  IF $$SETIFMATCH(INPUT,UPINPUT,"A","<UP>",.RESULT) GOTO XLCDN
  IF $$SETIFMATCH(INPUT,UPINPUT,"AA","<PGUP>",.RESULT) GOTO XLCDN
  IF $$SETIFMATCH(INPUT,UPINPUT,"<AD>","<DN>",.RESULT) GOTO XLCDN
  IF $$SETIFMATCH(INPUT,UPINPUT,"Z","<DN>",.RESULT) GOTO XLCDN
  IF $$SETIFMATCH(INPUT,UPINPUT,"ZZ","<PGDN>",.RESULT) GOTO XLCDN
  IF $$SETIFMATCH(INPUT,UPINPUT,"<AL>","<LEFT>",.RESULT) GOTO XLCDN
  IF $$SETIFMATCH(INPUT,UPINPUT,"[","<LEFT>",.RESULT) GOTO XLCDN
  IF $$SETIFMATCH(INPUT,UPINPUT,"[[","<HOME>",.RESULT) GOTO XLCDN
  IF $$SETIFMATCH(INPUT,UPINPUT,"<AR>","<RIGHT>",.RESULT) GOTO XLCDN
  IF $$SETIFMATCH(INPUT,UPINPUT,"]","<RIGHT>",.RESULT) GOTO XLCDN
  IF $$SETIFMATCH(INPUT,UPINPUT,"]]","<END>",.RESULT) GOTO XLCDN
  IF $$SETIFMATCH(INPUT,UPINPUT,"^","Q",.RESULT) GOTO XLCDN
XLCDN ;  
  QUIT RESULT
  ;  
SETIFMATCH(INPUT1,INPUT2,MATCH,OUTVAL,OUTVAR)  ;
  NEW RESULT SET RESULT=0  ;"set to 1 if match made.  
  IF $GET(INPUT1)=$GET(MATCH) SET OUTVAR=$GET(OUTVAL),RESULT=1 
  ELSE  IF $GET(INPUT2)=$GET(MATCH) SET OUTVAR=$GET(OUTVAL),RESULT=1
  QUIT RESULT
  ;
EVALWATCHES  ;
  ;"Purpose: Run code that evaluates watches.
  IF $GET(tmgWatchLine)'="" DO
  . NEW $ETRAP SET $ETRAP="WRITE ""(Invalid M Code!.  Error Trapped.)"" SET $ETRAP="""",$ecode="""""
  . xecute tmgWatchLine
  IF $DATA(tmgDgbWatches("*")) DO ShowVTrace^TMGIDE6
  WRITE !
  QUIT
  ;
REFRESHCODE  ;
  ;"Uses vars in global scope:  tmgIDEPos,tmgScrWidth,tmgScrHeight,tmgViewOffset,tmgLROffset,tmgCsrOnBreakline
  DO SHOWCODE(.tmgIDEPos,.tmgScrWidth,.tmgScrHeight,0,.tmgViewOffset,.tmgLROffset,.tmgCsrOnBreakline) 
  QUIT
  ;
SHOWCODE(ShowPos,tmgScrWidth,tmgScrHeight,Wipe,tmgViewOffset,tmgLROffset,tmgCsrOnBreakline)  ;
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
  NEW LOOP,scRoutine,scLabel,scOffset,scS
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
  . DO SETCOLORS^TMGIDE2C("Reset")
  . FOR LOOP=0:1:tmgScrHeight+1 WRITE tmgDbgBlankLine,!
  ;
  SET scS=$PIECE(ShowPos,"$",1)  ;"e.g. X+2^ROUTINE$DMOD-->X+2^ROUTINE
  DO ParsePos^TMGMISC(scS,.scLabel,.scOffset,.scRoutine)
  IF scRoutine="" DO  GOTO SCDone
  . WRITE !,!,"Error -- invalid position provided to SHOWCODE routine: ",ShowPos,!
  . WRITE "scS=",scS,!
  ;
  ;"setup to show a symbol for breakpoint
  NEW zbS SET zbS=""
  FOR  SET zbS=$ORDER(^TMG("TMGIDE",tmgDbgJNum,"ZBREAK",zbS)) QUIT:(zbS="")  DO
  . NEW zbRoutine,zbLabel,zbOffset
  . NEW tmgTempPos SET tmgTempPos=$$ConvertPos^TMGMISC(zbS,tmgZArrayName)
  . DO ParsePos^TMGMISC(tmgTempPos,.zbLabel,.zbOffset,.zbRoutine)
  . IF zbRoutine'=scRoutine QUIT
  . IF zbLabel'=scLabel QUIT
  . SET zBreakIdx(zbOffset)=1
  ;"-----Also get breakpoints stored in GT.M --------------
  NEW gtmBrkPts zshow "B":gtmBrkPts
  ;"//kt NOTE: after upgrade to r202 from r132, format of gtmBrkPts has changed
  ;"           Now it is LABEL+OFFSET^ROUTINE>breakpoint m code.  
  NEW zbI SET zbI=0
  FOR  SET zbI=$ORDER(gtmBrkPts("B",zbI)) QUIT:(zbI="")  DO
  . SET zbS=$GET(gtmBrkPts("B",zbI)) QUIT:zbS=""
  . SET zbS=$PIECE(zbS,">",1)  ;"//kt 1/7/24  remove execution code at zbreak
  . NEW zbRoutine,zbLabel,zbOffset
  . NEW tmgTempPos SET tmgTempPos=$$ConvertPos^TMGMISC(zbS,tmgZArrayName)
  . DO ParsePos^TMGMISC(tmgTempPos,.zbLabel,.zbOffset,.zbRoutine)
  . IF zbRoutine'=scRoutine QUIT
  . IF zbLabel'=scLabel QUIT
  . SET zBreakIdx(zbOffset)=1
  ;
  IF scOffset>(tmgScrHeight) SET StartOffset=(scOffset-tmgScrHeight)+2
  ELSE  SET StartOffset=0
  SET StartOffset=StartOffset+$GET(tmgViewOffset)
  ;
  ;"====Draw the top line ==========================================
  DO SETCOLORS^TMGIDE2C("NORM")
  WRITE "=== "
  DO SETCOLORS^TMGIDE2C("SPECIAL")
  SET scS="Routine: "_scLabel_"^"_scRoutine_" "
  IF $DATA(tmgOrigIDEPos) SET scS=scS_"("_tmgOrigIDEPos_")"
  ELSE  SET scS=scS_"("_ShowPos_")"
  SET scS=scS_" Runmode: "_$get(tmgRunMode)
  WRITE scS
  DO SETCOLORS^TMGIDE2C("NORM")
  WRITE " "
  FOR LOOP=1:1:tmgScrWidth-$LENGTH(scS)-5 WRITE "="
  DO SETCOLORS^TMGIDE2C("NORM")
  WRITE !
  ;
  SET tmgCsrOnBreakline=0
  FOR LOOP=StartOffset:1:(StartOffset+tmgScrHeight) DO
  . DO SETCOLORS^TMGIDE2C("NORM")
  . DO SETTEMPBKCOLOR("Reset")
  . NEW cbLine,cbRef,cbCursor,cBrkLine
  . SET cBrkLine=$DATA(zBreakIdx(LOOP))
  . SET cbRef=scLabel_"+"_LOOP_"^"_scRoutine
  . SET cbLine=$text(@cbRef)
  . FOR  QUIT:cbLine'[$CHAR(9)  SET cbLine=$PIECE(cbLine,$CHAR(9),1)_"        "_$PIECE(cbLine,$CHAR(9),2,999)
  . SET scCursorLine=scOffset+$GET(tmgViewOffset)
  . NEW cHighCsrPos SET cHighCsrPos=(LOOP=scCursorLine)
  . NEW cHighExecPos SET cHighExecPos=(LOOP=scOffset)
  . IF cHighCsrPos DO SETTEMPBKCOLOR("Highlight")
  . IF cHighExecPos DO SETTEMPBKCOLOR("HighExecPos")
  . IF cBrkLine DO
  . . IF (cHighCsrPos)!(cHighExecPos) DO    ;"was IF (cHighCsrPos=0)&(cHighExecPos=0) DO  //kt 10/18/24
  . . . DO SETTEMPBKCOLOR("HighBkPos")
  . . ELSE  DO
  . . . DO SETTEMPBKCOLOR("BkPos")
  . . . SET tmgCsrOnBreakline=1
  . WRITE $SELECT(LOOP=scOffset:">",cBrkLine:"#",1:" ")
  . DO SETCOLORS^TMGIDE2C("SPECIAL")
  . IF LOOP>0 WRITE "+"_LOOP_$SELECT(LOOP<10:" ",1:"")
  . ELSE  WRITE "   "
  . DO SETCOLORS^TMGIDE2C("NORM")
  . NEW StartPos SET StartPos=$X
  . IF $GET(TMGIDEDEBUG) WRITE cbLine SET cbLineLen=$LENGTH(cbLine)  ;"temp
  . ELSE  SET cbLineLen=$$ShowLine^TMGIDE6(cbLine,tmgLROffset,.tmgDbgOptions,tmgScrWidth-StartPos)
  . WRITE $EXTRACT(tmgDbgBlankLine,cbLineLen,tmgScrWidth-StartPos-1)
  . DO SETTEMPBKCOLOR("Reset"),SETCOLORS^TMGIDE2C("NORM")
  . WRITE !
  ;
  ;"Draw bottom line.
  DO SETCOLORS^TMGIDE2C("NORM")
  FOR LOOP=1:1:tmgScrWidth WRITE "~"
  WRITE !
SCDone ;
  DO VTATRIB^TMGTERM(0)  ;"reset colors
  QUIT
  ;