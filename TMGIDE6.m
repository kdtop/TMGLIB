TMGIDE6 ;TMG/kst/GT/M debugger Code Coloration & Utils ;11/11/15
         ;;1.0;TMG-LIB;**1**;4/4/09
 ;
 ;" TMG IDE Code Coloration and utilities
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"------------------------------------------------------------
 ;"PUBLIC API
 ;"------------------------------------------------------------
 ;"ShowLine(line,Options,BkColor) -- Encode and WRITE out a line of code with colors
 ;"WriteMLine(line,BkColor) -- WRITE out markup line, converting tags into colors
 ;"MarkupLine(line,Options) -- add markup tags that will allow coloration.
 ;"EditColors -- Enable Edit Colors
 ;"DelBreaks -- edit and delete breakpoints
 ;"ShowColorBox -- Shows a grid with all color FG and BG combinations.  
 ;"------------------------------------------------------------
 ;"PRIVATE API
 ;"------------------------------------------------------------
 ;"ShowPos(Pos)  -- A temp function to show out code at a given position.
 ;"ShowLine(line,Options,MaxChar) -- encode and WRITE out a line of code with colors
 ;"WriteMLine(line,MaxChar) -- WRITE out markup line, converting tags into colors)
 ;"DoWrite(s,CurLen,MaxLen) -- DO a controlled WRITE to the screen.
 ;"MarkupLine(line,Options) -- take an arbitrary line of code and add markup tags to allow coloration.
 ;"HndlArgs(Args) -- return a formatted arguments text
 ;"HndlCmd(Cmd,Options) -- Return formatted command
 ;"NextBlock(line,Div) --return from the begining to the next space.  Space is discarded.
 ;"INITCOLORS -- establish tmgDbgOptions globally-scoped var for colors,
 ;"TestColors
 ;"ShowTrace --  to show current trace record of execution.
 ;"RecordTrace(ExecPos) -- keep trace record of execution as program runs.
 ;"StackStr(Stack) -- Turn stack array into a single string
 ;"============== Code for VAR TRACING functionality =================
 ;"ShowVTrace -- Output changes from last step
 ;"RecordVTrace -- keep a trace of changes to the system variable table.
 ;"StoreVars(tmgRef) -- copy system variable table to a storage area
 ;"DiffVars(tmgRefCurF,tmgRefPriorF,tmgRefDelta) -- Store diff between tmgRefCurF and tmgRefPriorF
 ;"------------------------------------------------------------
 ;"------------------------------------------------------------
temp
  NEW tmgTempPos,pos,offset
  SET pos="^PSOORFIN"
  NEW Options
  SET Options("XCMD")=1
  SET Options("LCASE")=1
  FOR offset=50:1:58 do
  . SET tmgTempPos="+"_offset_pos
  . NEW line SET line=$text(@tmgTempPos)
  . WRITE offset,": " IF $$ShowLine(line,.Options,40) WRITE !
  DO VTATRIB^TMGTERM(0) ;"Reset colors
  QUIT
  ;   
ShowPos(Pos)
  ;"A temp function to show out code at a given position.
  NEW line SET line=$text(@Pos)
  WRITE Pos,": " IF $$ShowLine(line) WRITE !
  QUIT
  ;
ShowLine(line,LineOffset,Options,MaxChar)
  ;"Purpose: to encode and WRITE out a line of code with colors
  ;"Input: line -- the code line to show
  ;"       LineOffset -- IF 5, then show character 5 onward from line
  ;"       Options -- See MarkupLine for format
  ;"       MaxChar -- OPTIONAL.  Max count of characters to be allowed written.
  ;"Results: returns the actual number of chars written to screen.
  NEW temp SET temp=$$MarkupLine(line,.Options)
  SET temp=$$LTrimMS(temp,LineOffset)
  NEW Len SET Len=$$VisLen(temp)
  IF Len>MaxChar do
  . NEW RTrimAmount SET RTrimAmount=(Len-MaxChar)+3
  . SET temp=$$RTrimMS(temp,RTrimAmount)
  . SET temp=temp_"..."
  NEW result SET result=$$WriteMLine(temp,.MaxChar)
  QUIT result
  ;
LTrimMS(line,TrimAmount) ;"Left trim Markup String
  ;"Purpose: to protect colorization, but left trim start of line by TrimAmount Characters
  ;"Input: line -- the code line to show.
  ;"       TrimAmount -- The number of visible characters to trim.  **DON'T** PASS BY REFERENCE
  ;"Result: returns modified line
  ;"Output: line is changed
  NEW ResultS SET ResultS=line
  NEW Done SET Done=0
  FOR  QUIT:(TrimAmount'>0)!Done  do
  . NEW Idx SET Idx=$$FirstVis(ResultS)
  . IF Idx=0 SET Done=1 QUIT
  . SET ResultS=$EXTRACT(ResultS,1,Idx-1)_$EXTRACT(ResultS,Idx+1,9999)
  . SET TrimAmount=TrimAmount-1
  QUIT ResultS
  ;
RTrimMS(line,TrimAmount) ;"Right trim Markup String
  ;"Purpose: to protect colorization, but right trim end of line by TrimAmount Characters
  ;"Input: line -- the code line to show.
  ;"       TrimAmount -- The number of visible characters to trim.  **DON'T** PASS BY REFERENCE
  ;"Result: returns modified line
  ;"Output: line is changed
  NEW ResultS SET ResultS=line
  NEW Done SET Done=0
  FOR  QUIT:(TrimAmount'>0)!Done  do
  . NEW Idx SET Idx=$$LastVis(ResultS)
  . IF Idx=0 SET Done=1 QUIT
  . SET ResultS=$EXTRACT(ResultS,1,Idx-1)_$EXTRACT(ResultS,Idx+1,9999)
  . SET TrimAmount=TrimAmount-1
  QUIT ResultS
  ;
FirstVis(line) ;
  ;"Purpose return index of first character not inside a tag, or 0 IF problem
  NEW Result SET Result=0
  NEW InTag SET InTag=0
  NEW Idx FOR Idx=1:1 DO  QUIT:(Idx>$LENGTH(line))!(Result>0)
  . NEW Ch SET Ch=$EXTRACT(line,Idx)
  . IF Ch="{" SET InTag=1 QUIT
  . IF InTag,(Ch="}") SET InTag=0 QUIT
  . IF InTag QUIT
  . SET Result=Idx
  QUIT Result
  ;
VisLen(line) ;Return number of visible characters in markup line
  NEW Result SET Result=0
  NEW InTag SET InTag=0
  NEW Idx FOR Idx=1:1 DO  QUIT:(Idx>$LENGTH(line))
  . NEW Ch SET Ch=$EXTRACT(line,Idx)
  . ;WRITE "Idx=",Idx," Ch=",Ch," Result=",Result,!
  . IF Ch="{" SET InTag=1 QUIT
  . IF InTag,(Ch="}") SET InTag=0 QUIT
  . IF InTag QUIT
  . SET Result=Result+1
  QUIT Result
  ;
LastVis(line) ;
  ;"Purpose return index of last character not inside a tag, or 0 IF problem
  NEW Result SET Result=0
  NEW InTag SET InTag=0
  NEW Idx FOR Idx=$LENGTH(line):-1 QUIT:(Idx=0)!(Result>0)  do
  . NEW Ch SET Ch=$EXTRACT(line,Idx)
  . IF Ch="}" SET InTag=1 QUIT
  . IF InTag,(Ch="{") SET InTag=0 QUIT
  . IF InTag QUIT
  . SET Result=Idx
  QUIT Result
  ;
WriteMLine(line,MaxChar)
  ;"Purpose: to WRITE out markup line, converting tags into colors)
  ;"Input: line -- the text to show, created by MarkupLine. DON'T pass by reference
  ;"       MaxChar -- OPTIONAL.  Max count of characters to be allowed written.
  ;"result: number of actual characters written to screen (removing tags)
  NEW result SET result=0
  SET MaxChar=$GET(MaxChar,9999)
  FOR  QUIT:($LENGTH(line)'>0)!(result>MaxChar)  do
  . NEW p SET p=$find(line,"{C")
  . IF p>0 DO  ;"start color found
  . . NEW partS SET partS=$EXTRACT(line,1,p-3)
  . . DO SETCOLORS^TMGIDE2C("NORM")
  . . DO DoWrite(partS,.result,MaxChar)
  . . SET line=$EXTRACT(line,p-2,999)
  . . NEW code SET code=$$GETWORD^TMGSTUT3(line,1,"{","}")
  . . SET line=$EXTRACT(line,$LENGTH(code)+3,999) ;"shorten to after color tag onward
  . . NEW tmgMode SET tmgMode=$PIECE(code,":",2)
  . . DO SETCOLORS^TMGIDE2C(tmgMode)
  . . SET p=$find(line,"{C/}")  ;"look for close color directive
  . . IF p>0 do
  . . . SET partS=$EXTRACT(line,1,p-5) ;"get text up to closing color
  . . . DO DoWrite(partS,.result,MaxChar)
  . . . DO SETCOLORS^TMGIDE2C("NORM")
  . . . SET line=$EXTRACT(line,p,999) ;"shorten to next segment after closing color onward
  . . ELSE  do
  . . . DO DoWrite(line,.result,MaxChar)
  . . . SET line=""
  . ELSE  do
  . . DO DoWrite(line,.result,MaxChar)
  . . SET line=""
  QUIT result
  ;
DoWrite(s,CurLen,MaxLen)
  ;"Purpose: To DO a controlled WRITE to the screen.
  ;"Input: s -- the text to write
  ;"       CurLen -- PASS BY REFERENCE.  Current Num chars that have been written
  ;"       MaxLen -- the limit to chars that can be written to screen.
  NEW len SET len=$LENGTH(s)
  IF CurLen+len>MaxLen do
  . SET s=$EXTRACT(s,1,(MaxLen-CurLen))
  . SET len=$LENGTH(s)
  WRITE s
  SET CurLen=CurLen+len
  QUIT
  ;
MarkupLine(LINE,OPTIONS)
  ;"Purpose: To take an arbitrary LINE of code and parse into data structure
  ;"Input : LINE -- the line of code to consider.  DON'T pass by reference.
  ;"        OPTIONS -- PASS BY REFERENCE. 
  ;"             OPTIONS("Tab") -- optional.  Length of tabs.  Default = 5
  ;"Results: Returns line with color encoding.  
  NEW RESULT SET RESULT=""
  NEW TOKEN,CMD,ARG,TABSTR,POS,CH
  NEW TABLEN SET TABLEN=$GET(OPTIONS("Tab"),5)
  SET $PIECE(TABSTR," ",TABLEN)=""
  SET LINE=$GET(LINE)
  SET LINE=$TRANSLATE(LINE,$CHAR(9),TABSTR) ;"turn tabs into spaces
  IF $EXTRACT(LINE,1)'=" " do
  . SET TOKEN=$PIECE(LINE," ",1)
  . SET LINE=$PIECE(LINE," ",2,999)
  . SET RESULT="{C:LABEL}"_TOKEN_"{C/} "
  FOR POS=1:1 QUIT:(POS>$LENGTH(LINE))!($EXTRACT(LINE,POS)'=" ")
  SET RESULT=RESULT_$EXTRACT(LINE,1,POS-1)  ;"get leading space
  SET LINE=$EXTRACT(LINE,POS,999)
  NEW COMMENT SET COMMENT=""
  ;"Extract comments first...
  SET POS=1 FOR  SET POS=$find(LINE,";",POS) QUIT:(POS'>0)  do
  . IF $$INQT^TMGSTUT3(LINE,POS-1) QUIT
  . SET COMMENT=$EXTRACT(LINE,POS-1,999)
  . SET COMMENT="{C:#}"_COMMENT_"{C/}"
  . SET LINE=$EXTRACT(LINE,1,POS-2)
  ;"====== Loop to get COMMAND ARG pairs ===="
  FOR  QUIT:($LENGTH(LINE)'>0)  do
  . FOR  SET CH=$EXTRACT(LINE,1) QUIT:(" ."'[CH)!(CH="")  do
  . . SET RESULT=RESULT_CH,LINE=$EXTRACT(LINE,2,999)
  . QUIT:(LINE="")
  . SET TOKEN=$$NextBlock(.LINE)
  . IF TOKEN[":" do
  . . SET CMD=$$NextBlock(.TOKEN,":")
  . . SET RESULT=RESULT_$$HndlCmd(CMD,.OPTIONS)_"{C:PC}:{C/}"
  . . SET RESULT=RESULT_$$HndlArgs(TOKEN)_" "
  . ELSE  do
  . . SET RESULT=RESULT_$$HndlCmd(TOKEN,.OPTIONS)_" "
  . SET ARG=$$NextBlock(.LINE)
  . SET ARG=$$HndlArgs(ARG)
  . SET RESULT=RESULT_ARG_" "
  ;
  SET RESULT=RESULT_COMMENT  ;"add back comment (if any)
  QUIT RESULT
  ;
HndlArgs(Args)
  ;"Purpose: to return a formatted arguments text
  ;"Input: Args -- the text that supplies arguments to a command, OR
  ;"               the text that is post-conditional code
  ;"results: returns the Args with markup code.
  NEW POS SET POS=1
  FOR  SET POS=$find(Args,"$$",POS) QUIT:(POS'>0)  DO  QUIT:(POS'>0)  ;"Handle functions
  . IF $$INQT^TMGSTUT3(Args,POS-1) SET POS=$find(Args,"""",POS) QUIT
  . ;"NEW FNNAME SET FNNAME="$$"_$$GETWORD^TMGSTUT3(Args,POS,"$","():^= _'[]<>")
  . NEW FNNAME SET FNNAME="$$"_$$GETWORD^TMGSTUT3(Args,POS,"$","():^= _'[]<>, ")
  . NEW PARTA,PARTB
  . SET PARTA=$EXTRACT(Args,1,POS-3)
  . SET PARTB=$EXTRACT(Args,POS-2+$LENGTH(FNNAME),999)
  . SET Args=PARTA_"{C:FN}"_FNNAME_"{C/}"_PARTB
  . SET POS=POS+6+$LENGTH(FNNAME) ;"6=length of {C:FN}
  SET POS=1
  FOR  SET POS=$find(Args,"$",POS) QUIT:(POS'>0)  DO  QUIT:(POS'>0)  ;"Handle intrinsic functions
  . IF $$INQT^TMGSTUT3(Args,POS-1) SET POS=$find(Args,"""",POS) QUIT
  . IF $EXTRACT(Args,POS)="$" SET POS=POS+1 QUIT  ;"avoid $$ matches
  . ;"NEW FNNAME SET FNNAME="$"_$$GETWORD^TMGSTUT3(Args,POS,"$","():^= _'[]<>")
  . NEW FNNAME SET FNNAME="$"_$$GETWORD^TMGSTUT3(Args,POS,"$","():^= _'[]<>, ")
  . NEW PARTA,PARTB
  . SET PARTA=$EXTRACT(Args,1,POS-2)
  . SET PARTB=$EXTRACT(Args,POS-1+$LENGTH(FNNAME),999)
  . SET Args=PARTA_"{C:IFN}"_FNNAME_"{C/}"_PARTB
  . SET POS=POS+7+$LENGTH(FNNAME) ;"7=length of {C:IFN}
  SET POS=1
  FOR  SET POS=$find(Args,"^",POS) QUIT:(POS'>0)  DO  QUIT:(POS'>0)  ;"Handle Modules
  . IF $$INQT^TMGSTUT3(Args,POS-1) SET POS=$find(Args,"""",POS) QUIT
  . NEW MODNAME SET MODNAME="^"_$$GETWORD^TMGSTUT3(Args,POS,"^","():,= _'[]<>")
  . NEW PARTA,PARTB
  . SET PARTA=$EXTRACT(Args,1,POS-2)
  . SET PARTB=$EXTRACT(Args,POS-1+$LENGTH(MODNAME),999)
  . SET Args=PARTA_"{C:MOD}"_MODNAME_"{C/}"_PARTB
  . SET POS=POS+7+$LENGTH(MODNAME) ;"7=length of {C:MOD}
  SET POS=1
  FOR  SET POS=$find(Args,"""",POS) QUIT:(POS'>0)  DO  ;"Handle Strings
  . NEW POS2
  . IF $EXTRACT(Args,POS)="""" SET POS2=POS
  . ELSE  SET POS2=$$StrBounds^TMGSTUTL(Args,POS)
  . IF POS2=0 SET POS=999 QUIT
  . NEW PARTA,PARTB,PARTC
  . SET PARTA=$EXTRACT(Args,1,POS-2)
  . SET PARTB=$EXTRACT(Args,POS-1,POS2)
  . SET PARTC=$EXTRACT(Args,POS2+1,999)
  . SET Args=PARTA_"{C:STR}"_PARTB_"{C/}"_PARTC
  . SET POS=POS+7+$LENGTH(PARTB) ;"7=length of {C:STR}
  QUIT Args
  ;
HndlCmd(Cmd,OPTIONS)
  ;"Purpose: Return formatted command
  ;"Input: Cmd -- the mumps command
  ;"       OPTIONS -- OPTIONAL.  Format:
  ;"              OPTIONS('XCMD')=1 --> turn I --> IF etc. (expand commands)
  ;"              OPTIONS('SCMD')=1 --> turn IF --> I etc. (shrink commands)
  ;"              OPTIONS('UCASE')=1 --> turn commands into UPPER CASE
  ;"              OPTIONS('LCASE')=1 --> turn commands into LOWER CASE
  ;"Results: returns the command with markup code
  NEW RESULT SET RESULT=""
  SET Cmd=$GET(Cmd)
  NEW TEMPCMD SET TEMPCMD=$$UP^XLFSTR(Cmd)
  IF $GET(OPTIONS("XCMD")) do
  . IF TEMPCMD="AB" SET Cmd="ABLOCK" QUIT
  . IF TEMPCMD="A" SET Cmd="ASSIGN" QUIT
  . IF TEMPCMD="ASTA" SET Cmd="ASTART" QUIT
  . IF TEMPCMD="ASTO" SET Cmd="ASTOP" QUIT
  . IF TEMPCMD="AUNB" SET Cmd="AUNBLOCK" QUIT
  . IF TEMPCMD="B" SET Cmd="BREAK" QUIT
  . IF TEMPCMD="C" SET Cmd="CLOSE" QUIT
  . IF TEMPCMD="D" SET Cmd="DO" QUIT
  . IF TEMPCMD="E" SET Cmd="ELSE" QUIT
  . IF TEMPCMD="ESTA" SET Cmd="ESTART" QUIT
  . IF TEMPCMD="ESTO" SET Cmd="ESTOP" QUIT
  . IF TEMPCMD="ETR" SET Cmd="ETRIGGER" QUIT
  . IF TEMPCMD="F" SET Cmd="FOR" QUIT
  . IF TEMPCMD="G" SET Cmd="GOTO" QUIT
  . ;"if TEMPCMD="H" SET Cmd="HALT" QUIT
  . ;"if TEMPCMD="H" SET Cmd="HANG" QUIT
  . IF TEMPCMD="I" SET Cmd="IF" QUIT
  . IF TEMPCMD="J" SET Cmd="JOB" QUIT
  . IF TEMPCMD="K" SET Cmd="KILL" QUIT
  . IF TEMPCMD="KS" SET Cmd="KSUBSCRIPTS" QUIT
  . IF TEMPCMD="KV" SET Cmd="KVALUE" QUIT
  . IF TEMPCMD="L" SET Cmd="LOCK" QUIT
  . IF TEMPCMD="M" SET Cmd="MERGE" QUIT
  . IF TEMPCMD="N" SET Cmd="NEW" QUIT
  . IF TEMPCMD="O" SET Cmd="OPEN" QUIT
  . IF TEMPCMD="Q" SET Cmd="QUIT" QUIT
  . IF TEMPCMD="R" SET Cmd="READ" QUIT
  . IF TEMPCMD="RL" SET Cmd="RLOAD" QUIT
  . IF TEMPCMD="RS" SET Cmd="RSAVE" QUIT
  . IF TEMPCMD="S" SET Cmd="SET" QUIT
  . IF TEMPCMD="TC" SET Cmd="TCOMMIT" QUIT
  . IF TEMPCMD="TH" SET Cmd="THEN" QUIT
  . IF TEMPCMD="TRE" SET Cmd="TRESTART" QUIT
  . IF TEMPCMD="TRO" SET Cmd="TROLLBACK" QUIT
  . IF TEMPCMD="TS" SET Cmd="TSTART" QUIT
  . IF TEMPCMD="U" SET Cmd="USE" QUIT
  . IF TEMPCMD="V" SET Cmd="VIEW" QUIT
  . IF TEMPCMD="W" SET Cmd="WRITE" QUIT
  . IF TEMPCMD="X" SET Cmd="XECUTE" QUIT
  . IF TEMPCMD="ZWR" SET Cmd="ZWRITE" QUIT
  IF $GET(OPTIONS("SCMD")) do
  . IF TEMPCMD="ABLOCK" SET Cmd="AB" QUIT
  . IF TEMPCMD="ASSIGN" SET Cmd="A" QUIT
  . IF TEMPCMD="ASTART" SET Cmd="ASTA" QUIT
  . IF TEMPCMD="ASTOP" SET Cmd="ASTO" QUIT
  . IF TEMPCMD="AUNBLOCK" SET Cmd="AUNB" QUIT
  . IF TEMPCMD="BREAK" SET Cmd="B" QUIT
  . IF TEMPCMD="CLOSE" SET Cmd="C" QUIT
  . IF TEMPCMD="DO" SET Cmd="D" QUIT
  . IF TEMPCMD="ELSE" SET Cmd="E" QUIT
  . IF TEMPCMD="ESTART" SET Cmd="ESTA" QUIT
  . IF TEMPCMD="ESTOP" SET Cmd="ESTO" QUIT
  . IF TEMPCMD="ETRIGGER" SET Cmd="ETR" QUIT
  . IF TEMPCMD="FOR" SET Cmd="F" QUIT
  . IF TEMPCMD="GOTO" SET Cmd="G" QUIT
  . IF TEMPCMD="HALT" SET Cmd="H" QUIT
  . IF TEMPCMD="HANG" SET Cmd="H" QUIT
  . IF TEMPCMD="IF" SET Cmd="I" QUIT
  . IF TEMPCMD="JOB" SET Cmd="J" QUIT
  . IF TEMPCMD="KILL" SET Cmd="K" QUIT
  . IF TEMPCMD="KSUBSCRIPTS" SET Cmd="KS" QUIT
  . IF TEMPCMD="KVALUE" SET Cmd="KV" QUIT
  . IF TEMPCMD="LOCK" SET Cmd="L" QUIT
  . IF TEMPCMD="MERGE" SET Cmd="M" QUIT
  . IF TEMPCMD="NEW" SET Cmd="N" QUIT
  . IF TEMPCMD="OPEN" SET Cmd="O" QUIT
  . IF TEMPCMD="QUIT" SET Cmd="Q" QUIT
  . IF TEMPCMD="READ" SET Cmd="R" QUIT
  . IF TEMPCMD="RLOAD" SET Cmd="RL" QUIT
  . IF TEMPCMD="RSAVE" SET Cmd="RS" QUIT
  . IF TEMPCMD="SET" SET Cmd="S" QUIT
  . IF TEMPCMD="TCOMMIT" SET Cmd="TC" QUIT
  . IF TEMPCMD="THEN" SET Cmd="TH" QUIT
  . IF TEMPCMD="TRESTART" SET Cmd="TRE" QUIT
  . IF TEMPCMD="TROLLBACK" SET Cmd="TRO" QUIT
  . IF TEMPCMD="TSTART" SET Cmd="TS" QUIT
  . IF TEMPCMD="USE" SET Cmd="U" QUIT
  . IF TEMPCMD="VIEW" SET Cmd="V" QUIT
  . IF TEMPCMD="WRITE" SET Cmd="W" QUIT
  . IF TEMPCMD="XECUTE" SET Cmd="X" QUIT
  . IF TEMPCMD="ZWRITE" SET Cmd="ZWR" QUIT
  IF $GET(OPTIONS("UCASE")) SET Cmd=$$UP^XLFSTR(Cmd)
  IF $GET(OPTIONS("LCASE")) SET Cmd=$$LOW^XLFSTR(Cmd)
  SET RESULT="{C:CMD}"_Cmd_"{C/}"
  QUIT RESULT
  ;
NextBlock(LINE,Div)
  ;"Purpose: to return from the begining to the next space.  Space is
  ;"        discarded.
  ;"      e.g. LINE='This is a test', then function will return 'This'
  ;"           and LINE will be changed to be 'is a test'
  ;"      e.g. LINE='QUIT:(test)  do'  will return 'QUIT:(test)'
  ;"           and LINE will be changed to ' do' (with 1 space)
  ;"      e.g. LINE=' do' will return ''
  ;"           and LINE will be changed to 'do'
  ;"      e.g. LINE='test' will return 'test'
  ;"           and LINE will be changed to ''
  ;"      NO e.g. LINE='..test' will return '...'
  ;"      NO     and LINE will be changed to 'test'
  ;"Input: LINE -- PASS BY REFERENCE
  ;"       Div -- the divider of blocks.  OPTIONAL.  Default=" "
  ;"Result: the first block, see above.
  NEW RESULT SET RESULT=""
  SET Div=$GET(Div," ")
  NEW done SET done=0
  NEW POS SET POS=1
  FOR  DO  QUIT:(done)
  . SET POS=$find(LINE,Div,POS)
  . IF POS'>0 SET RESULT=LINE,LINE="",done=1 QUIT
  . IF $$INQT^TMGSTUT3(LINE,POS-1) QUIT
  . SET RESULT=$EXTRACT(LINE,1,POS-2)
  . SET LINE=$EXTRACT(LINE,POS,999)
  . SET done=1
  QUIT RESULT
  ;
INITCOLORS
  ;"Purpose: to establish tmgDbgOptions globally-scoped var for colors,
  NEW CURREF,MASTERREF,USERREF DO GETCOLORSTOREREF(.CURREF,.MASTERREF,.USERREF)
  DO INITCOLORSREF(CURREF,MASTERREF,USERREF) 
  QUIT
  ;
INITCOLORSREF(CURREF,MASTERREF,USERREF) ;
  ;"Purpose: to establish tmgDbgOptions globally-scoped var for colors,
  IF $DATA(@CURREF)>0 QUIT  ;"Colors already defined, no need for anything further.  
  IF $DATA(@USERREF)>0 DO  QUIT
  . MERGE @CURREF=@USERREF ;"copy user's default colors into job's
  IF $DATA(@MASTERREF)>0 DO  QUIT
  . MERGE @CURREF=@MASTERREF ;"copy master into job's
  ;"--------------------------------------------
  DO COLORSBOOTSTRAP(CURREF)
  MERGE @MASTERREF=@CURREF
  QUIT
  ;
COLORSBOOTSTRAP(CURREF) ;
  IF $DATA(TMGCOLBLACK)=0 DO SETGBLCO^TMGUSRI8
  SET @CURREF@("BACKGROUND")=TMGCOLBLUE
  SET @CURREF@("HighExecPos")=TMGCOLGREY
  SET @CURREF@("HighBkPos")=TMGCOLBRED
  SET @CURREF@("BkPos")=TMGCOLRED
  SET @CURREF@("Highlight")=TMGCOLFGBWHITE
  ;"-----------------------------------
  SET @CURREF@("LABEL","fg")=TMGCOLBYELLOW
  SET @CURREF@("LABEL","bg")=TMGCOLRED
  SET @CURREF@("SPECIAL","fg")=TMGCOLBYELLOW
  SET @CURREF@("SPECIAL","bg")=TMGCOLRED
  ;"-----------------------------------
  SET @CURREF@("NORM","fg")=TMGCOLFGBWHITE
  SET @CURREF@("NORM","bg")="@" ;"@ is signal to use current background color
  SET @CURREF@("CMD","fg")=TMGCOLBRED
  SET @CURREF@("CMD","bg")="@"
  SET @CURREF@("FN","fg")=TMGCOLBCYAN
  SET @CURREF@("FN","bg")="@"
  SET @CURREF@("MOD","fg")=TMGCOLBBLUE
  SET @CURREF@("MOD","bg")="@"
  SET @CURREF@("IFN","fg")=TMGCOLRED
  SET @CURREF@("IFN","bg")="@"
  SET @CURREF@("STR","fg")=TMGCOLBMAGENTA
  SET @CURREF@("STR","bg")="@"
  SET @CURREF@("PC","fg")=TMGCOLBRED
  SET @CURREF@("PC","bg")="@"
  SET @CURREF@("#","fg")=TMGCOLBYELLOW
  SET @CURREF@("#","bg")="@"
  ;"--------------------------------------------
  SET @CURREF@("MENUBKGROUND")=TMGCOLBBLUE
  SET @CURREF@("MENUSELBKGROUND")=TMGCOLGREY 
  SET @CURREF@("MENUBORDER","fg")=TMGCOLBRED 
  SET @CURREF@("MENUBORDER","bg")="$"
  SET @CURREF@("MENUTEXT","fg")=TMGCOLBLACK
  SET @CURREF@("MENUTEXT","bg")="$"  ;"$ is signal to use current menu background color
  SET @CURREF@("MENUALTKEY","fg")=TMGCOLBRED
  SET @CURREF@("MENUALTKEY","bg")="$"  ;"$ is signal to use current menu background color
  ;"--------------------------------------------
  DO ENSUR24COLS(CURREF)
  QUIT
  ;
ENSUR24COLS(REF) ;"Enusure that the color palate has been prepped for 24bit color mode, using Index mode colors as default  
  NEW BG,FG,VAL
  NEW MODE SET MODE=""
  FOR  SET MODE=$ORDER(@REF@(MODE)) QUIT:MODE=""  DO
  . IF $DATA(@REF@(MODE,"24bit")) QUIT  ;"ALREADY SET UP.  
  . SET VAL=$GET(@REF@(MODE))
  . SET FG=$GET(@REF@(MODE,"fg"))
  . SET BG=$GET(@REF@(MODE,"bg"))
  . IF VAL'="" SET @REF@(MODE,"24bit")=$$MAPIDXTO24BIT^TMGUSRI8(VAL,1)
  . IF FG'="" DO
  . . IF FG="@" SET @REF@(MODE,"24bit","fg")="@" QUIT
  . . SET @REF@(MODE,"24bit","fg")=$$MAPIDXTO24BIT^TMGUSRI8(FG,0)
  . IF BG'="" DO
  . . IF BG="@" SET @REF@(MODE,"24bit","bg")="@" QUIT
  . . SET @REF@(MODE,"24bit","bg")=$$MAPIDXTO24BIT^TMGUSRI8(BG,1)
  QUIT
  ;  
TESTEDITCOLORS ;
  NEW TEMPCOLORS,TEMPMASTER DO INITCOLORSREF("TEMPCOLORS","TEMPMASTER")
  DO EDITCOLORSREF("TEMPCOLORS")
  QUIT
  ;
TestColors ;
  DO INITCOLORS
  NEW ARR DO GETCOLORDESCR(.ARR) 
  DO SETCOLORS^TMGIDE2C("Reset")
  WRITE !,"Here are currently define colors:",!
  WRITE "----------------------------------",!
  NEW MAXLEN SET MAXLEN=0
  NEW ENTRY SET ENTRY=""
  FOR  SET ENTRY=$ORDER(ARR(ENTRY)) QUIT:ENTRY=""  DO
  . NEW LINE SET LINE=$GET(ARR(ENTRY)) QUIT:LINE=""
  . NEW STR SET STR=$PIECE(LINE,$CHAR(9),1)
  . IF $LENGTH(STR)>MAXLEN SET MAXLEN=$LENGTH(STR)  
  SET ENTRY=""
  FOR  SET ENTRY=$ORDER(ARR(ENTRY)) QUIT:ENTRY=""  DO
  . NEW LINE SET LINE=$GET(ARR(ENTRY)) QUIT:LINE=""
  . NEW STR SET STR=$PIECE(LINE,$CHAR(9),1)
  . NEW MODE SET MODE=$PIECE(LINE,$CHAR(9),2)
  . WRITE "Colors for ",STR,": ",?MAXLEN+15,"|"
  . DO SETCOLORS^TMGIDE2C(MODE)
  . WRITE "The Quick Brown Fox Jumps Over the Lazy Dog"
  . DO SETCOLORS^TMGIDE2C("Reset")
  . WRITE "|",!
  DO PRESS2GO^TMGUSRI2
  WRITE !
  QUIT
  ;  
GETCOLORSTOREREF(CURREF,MASTERREF,USERREF,SCHEMEREF) ;
  SET CURREF=$NAME(^TMG("TMGIDE",$J,"COLORS"))
  SET MASTERREF=$NAME(^TMG("TMGIDE","COLORS"))
  SET SCHEMEREF=$NAME(^TMG("TMGIDE","COLOR SCHEMES"))  ;"Format: @SCHEMEREF@(<SchemeName>)=<array of colors, format as above)
  SET USERREF=$NAME(^TMG("TMGIDE","USER PREFS",+$GET(DUZ),"COLORS"))
  QUIT
  ;
EditColors  ;"Purpose: Enable Edit Colors
  NEW CURREF,MASTERREF,USERREF,SCHEMEREF DO GETCOLORSTOREREF(.CURREF,.MASTERREF,.USERREF,.SCHEMEREF)
  NEW MENU,USRSLCT,IDX
ECM1 ;  
  KILL MENU SET IDX=0
  SET MENU(IDX)="Select Option for Colors",IDX=IDX+1
  SET MENU(IDX)="View current colors"_$CHAR(9)_"VIEW",IDX=IDX+1
  SET MENU(IDX)="Edit individual colors"_$CHAR(9)_"EDIT",IDX=IDX+1
  SET MENU(IDX)="Load default colors"_$CHAR(9)_"MASTER",IDX=IDX+1
  NEW ANAME SET ANAME=""
  FOR  SET ANAME=$ORDER(@SCHEMEREF@(ANAME)) QUIT:ANAME=""  DO
  . SET MENU(IDX)="Load color scheme: ["_ANAME_"]"_$CHAR(9)_"SCHEME:"_ANAME,IDX=IDX+1
  SET MENU(IDX)="Save current colors to..."_$CHAR(9)_"SAVE",IDX=IDX+1
  SET USRSLCT=$$MENU^TMGUSRI2(.MENU,"^")
  IF USRSLCT="EDIT" DO   ;"GOTO ECM1  <-- NEED TO BE ABLE TO FALL DOWN TO "SAVE" BELOW.  
  . DO EDITCOLORSREF(CURREF)  
  . NEW % SET %=1
  . WRITE "Save current colors for future session" DO YN^DICN WRITE !
  . IF %=1 SET USRSLCT="SAVE"
  IF USRSLCT="MASTER" DO  GOTO ECM1
  . IF $DATA(@MASTERREF)=0 QUIT
  . KILL @CURREF MERGE @CURREF=@MASTERREF
  . WRITE !,"Colors from Master Default Color Set copied into current colors",!
  . DO PRESS2GO^TMGUSRI2
  IF USRSLCT["SCHEME:" DO  GOTO ECM1
  . NEW SCHEME SET SCHEME=$PIECE(USRSLCT,"SCHEME:",2) QUIT:SCHEME=""
  . IF $DATA(@SCHEMEREF@(SCHEME))=0 QUIT
  . KILL @CURREF MERGE @CURREF=@SCHEMEREF@(SCHEME)
  . WRITE !,"Colors from Scheme [",SCHEME,"] copied into current colors",!
  . DO PRESS2GO^TMGUSRI2
  IF USRSLCT="SAVE" DO  GOTO ECM1
  . DO SAVECOLORS(CURREF,MASTERREF,USERREF,SCHEMEREF) 
  IF USRSLCT="VIEW" DO  GOTO ECM1
  . DO TestColors
  IF USRSLCT="^" GOTO ECMDN
  GOTO ECM1
ECMDN ;  
  QUIT
  ;  
SAVECOLORS(CURREF,MASTERREF,USERREF,SCHEMEREF) ;
  NEW MENU,USRSLCT,IDX
SCM1 ;  
  KILL MENU SET IDX=0
  SET MENU(IDX)="SAVE Colors To ... ",IDX=IDX+1
  SET MENU(IDX)="My Default Colors"_$CHAR(9)_"USER",IDX=IDX+1
  SET MENU(IDX)="Master Default Colors"_$CHAR(9)_"MASTER",IDX=IDX+1
  NEW ANAME SET ANAME=""
  FOR  SET ANAME=$ORDER(@SCHEMEREF@(ANAME)) QUIT:ANAME=""  DO
  . SET MENU(IDX)="Save to Color Scheme: ["_ANAME_"]"_$CHAR(9)_"SCHEME:"_ANAME,IDX=IDX+1
  SET MENU(IDX)="Save to NEW Color Scheme"_$CHAR(9)_"NEW_SCHEME",IDX=IDX+1
  SET USRSLCT=$$MENU^TMGUSRI2(.MENU,"^")
  IF USRSLCT="USER" DO
  . NEW OVERWRITE SET OVERWRITE=($DATA(@USERREF)>0)
  . IF $$CONFIRMSAVE("My Default Colors",OVERWRITE)=0 QUIT
  . KILL @USERREF MERGE @USERREF=@CURREF
  IF USRSLCT="MASTER" DO
  . NEW OVERWRITE SET OVERWRITE=($DATA(@MASTERREF)>0)
  . IF $$CONFIRMSAVE("Master Default Colors",OVERWRITE)=0 QUIT
  . KILL @MASTERREF MERGE @MASTERREF=@CURREF
  IF USRSLCT="NEW_SCHEME" DO
  . NEW SCHEME
  . FOR  DO  QUIT:(SCHEME'="")
  . . WRITE !,"Enter scheme name (e.g. 'Light', 'Dark', 'Vibrant' etc) '^' to abort: "
  . . READ SCHEME WRITE !
  . . IF ($LENGTH(SCHEME)>20) DO  QUIT
  . . . WRITE !,"Too long.  Please make 20 characters or less.",!
  . . . SET SCHEME=""
  . IF SCHEME["^" QUIT
  . SET USRSLCT="SCHEME:"_SCHEME   ;"This will fall through to below.  
  IF USRSLCT["SCHEME:" DO
  . NEW SCHEME SET SCHEME=$PIECE(USRSLCT,"SCHEME:",2) QUIT:SCHEME=""
  . NEW OVERWRITE SET OVERWRITE=($DATA(@SCHEMEREF@(SCHEME))>0)
  . IF $$CONFIRMSAVE("Color Scheme ["_SCHEME_"]",OVERWRITE)=0 QUIT
  . KILL @SCHEMEREF@(SCHEME) MERGE @SCHEMEREF@(SCHEME)=@CURREF
  IF USRSLCT="^" GOTO SCDN
  GOTO SCM1
SCDN ;
  QUIT
  ;
CONFIRMSAVE(DEST,OVERWRITE) ;
  WRITE !
  IF $GET(OVERWRITE) WRITE "NOTE: This will delete any prior saved colors",!
  NEW % SET %=2 WRITE "Confirm saving to: "_DEST DO YN^DICN WRITE !
  QUIT (%=1)
  ;
EDITCOLORSREF(CURREF)  ;
  ;"Purpose: Enable Edit Colors
  WRITE #
  NEW COLORMODE SET COLORMODE=$GET(@CURREF@("MODE"),"INDEXED") 
  NEW MENU,MENU2,UsrSlct,UsrSlct2,UsrRaw,fg,bg,menubg,ct,COLORARR
  NEW CURCOLORS
M0  
  KILL MENU DO GETCOLORDESCR(.MENU)
  SET MENU(0)="Pick Color to Edit"
  SET ct=$ORDER(MENU(""),-1)+1
  SET MENU(ct)="-------------------------------------"_$CHAR(9)_"",ct=ct+1
  SET MENU(ct)="Change COLOR MODE (currently="_COLORMODE_")"_$CHAR(9)_"COLORMODE",ct=ct+1
  NEW i
M1 ; 
  KILL CURCOLORS
  SET i=0
  ;"SET UP DISPLAY COLORS FOR MENU 
  FOR  SET i=$ORDER(MENU(i)) QUIT:(i="")  DO
  . NEW bg,fg set (bg,fg,menubg)=""
  . NEW tmgMode SET tmgMode=$PIECE(MENU(i),$CHAR(9),2) QUIT:tmgMode=""
  . IF COLORMODE="24bit" DO
  . . NEW val
  . . IF "BACKGROUND,Highlight,HighBkPos,HighExecPos,BkPos,MENUBKGROUND,MENUSELBKGROUND"[tmgMode DO
  . . . SET bg=$$GETCOLOR24(CURREF,tmgMode,"bg") 
  . . . SET fg=$$INVCLRVEC^TMGTERM(bg)
  . . ELSE  IF "SPECIAL,LABEL"[tmgMode DO
  . . . SET fg=$$GETCOLOR24(CURREF,tmgMode,"fg") 
  . . . SET bg=$$GETCOLOR24(CURREF,tmgMode,"bg") 
  . . ELSE  IF "MENUBORDER,MENUTEXT,MENUALTKEY"[tmgMode DO
  . . . SET fg=$$GETCOLOR24(CURREF,tmgMode,"fg",$$WEBCOLOR^TMGUSRI8("Black",.COLORARR)) 
  . . . SET bg=$$GETCOLOR24(CURREF,"MENUBKGROUND","bg") 
  . . ELSE  DO
  . . . SET fg=$$GETCOLOR24(CURREF,tmgMode,"fg",$$WEBCOLOR^TMGUSRI8("Black",.COLORARR)) 
  . . . SET bg=$$GETCOLOR24(CURREF,"BACKGROUND","bg") 
  . ELSE  DO  ;"INDEXED COLOR MODE
  . . IF "BACKGROUND,Highlight,HighBkPos,HighExecPos,BkPos,MENUBKGROUND,MENUSELBKGROUND"[tmgMode DO
  . . . SET bg=+$GET(@CURREF@(tmgMode),TMGCOLBGREY)
  . . . SET fg=$SELECT(bg=0:7,1:10)
  . . ELSE  DO
  . . . SET fg=$GET(@CURREF@(tmgMode,"fg"),TMGCOLFGBWHITE)
  . . . SET bg=$GET(@CURREF@(tmgMode,"bg"),TMGCOLBGREY)
  . . . IF bg="@" SET bg=$GET(@CURREF@("BACKGROUND"),TMGCOLBLACK)
  . . . IF bg="$" SET bg=$GET(@CURREF@("MENUBKGROUND"),TMGCOLBLACK)
  . IF fg'="" SET MENU(i,"COLOR","fg")=fg,CURCOLORS(tmgMode,"fg")=fg          
  . IF bg'="" SET MENU(i,"COLOR","bg")=bg,CURCOLORS(tmgMode,"bg")=bg
  ;
  SET (fg,bg,menubg)=""
  SET UsrSlct=$$MENU^TMGUSRI2(.MENU,"^",.UsrRaw)
  IF UsrSlct="^" GOTO ECDn
  IF UsrSlct="COLORMODE" DO  GOTO M0
  . SET COLORMODE=$$MENUCOLORMODE(COLORMODE,CURREF) 
  . SET @CURREF@("MODE")=COLORMODE
  IF "BACKGROUND,Highlight,HighBkPos,HighExecPos,BkPos,MENUBKGROUND,MENUSELBKGROUND"[UsrSlct DO  GOTO M1B  
  . SET bg=$GET(CURCOLORS(UsrSlct,"bg"))
  . SET bg=$$PICKBGC^TMGUSRI8(bg,.COLORMODE)
  IF "SPECIAL,LABEL"[UsrSlct DO  GOTO M1B  
  . DO EDITFGBG(CURREF,UsrSlct,.COLORMODE)  
  . SET fg=$$GETCOLOR24(CURREF,UsrSlct,"fg") 
  . SET bg=$$GETCOLOR24(CURREF,UsrSlct,"bg")   
  IF $DATA(@CURREF@(UsrSlct))=0 DO COLORSBOOTSTRAP(CURREF) ;
  IF $DATA(@CURREF@(UsrSlct)) DO  GOTO M1B      
  . SET fg=$get(CURCOLORS(UsrSlct,"fg"))
  . IF "MENUTEXT,MENUALTKEY,MENUBORDER"[UsrSlct DO
  . . SET bg="$"  ;"Signal to use menu background
  . ELSE  DO
  . . SET bg="@"  ;"Signal to use normal background.  
  . IF COLORMODE="24bit" DO
  . . SET fg=$$PICKCOLOR24^TMGUSRI8(,fg)
  . ELSE  DO
  . . NEW tempbg SET tempbg=$GET(@CURREF@("BACKGROUND"),0)  ;"This bg value should not be stored.  
  . . WRITE "Setting bg=",tempbg,!
  . . SET fg=$$PICKFGC^TMGUSRI8(fg,tempbg,.COLORMODE)   
M1B ;  
  IF COLORMODE="24bit" DO
  . IF fg'="" SET @CURREF@(UsrSlct,"24bit","fg")=fg
  . IF bg'="" DO
  . . IF "BACKGROUND,Highlight,HighBkPos,HighExecPos,BkPos,MENUBKGROUND,MENUSELBKGROUND"[UsrSlct DO
  . . . SET @CURREF@(UsrSlct,"24bit")=bg   ;"These background-only colors are not stored in "bg" according to prior indexed system
  . . ELSE  DO
  . . . SET @CURREF@(UsrSlct,"24bit","bg")=bg
  ELSE  DO
  . IF fg'="" SET @CURREF@(UsrSlct,"fg")=fg
  . IF bg'="" SET @CURREF@(UsrSlct,"bg")=bg
  GOTO M1
  ;
ECDn  ;
  QUIT
  ;  
GETCOLORDESCR(ARR) ;
  KILL ARR
  SET CT=0
  SET CT=CT+1,ARR(CT)="Window Background color"_$CHAR(9)_"BACKGROUND"         
  SET CT=CT+1,ARR(CT)="Menus Background color"_$CHAR(9)_"MENUBKGROUND"         
  SET CT=CT+1,ARR(CT)="Normal Text Foreground Color"_$CHAR(9)_"NORM"
  SET CT=CT+1,ARR(CT)="Command Foreground Color"_$CHAR(9)_"CMD"
  SET CT=CT+1,ARR(CT)="Functions Foreground Color"_$CHAR(9)_"FN"
  SET CT=CT+1,ARR(CT)="Module/Global reference Foreground Color"_$CHAR(9)_"MOD"
  SET CT=CT+1,ARR(CT)="Mumps intrinsic functions Foreground Color"_$CHAR(9)_"IFN"
  SET CT=CT+1,ARR(CT)="String Foreground Color"_$CHAR(9)_"STR"
  SET CT=CT+1,ARR(CT)="Post-conditional Foreground Color"_$CHAR(9)_"PC"
  SET CT=CT+1,ARR(CT)="Comments Foreground Color"_$CHAR(9)_"#"
  SET CT=CT+1,ARR(CT)="Menu Text Foreground Color"_$CHAR(9)_"MENUTEXT"
  SET CT=CT+1,ARR(CT)="Menu Alt Key Foreground Color"_$CHAR(9)_"MENUALTKEY"
  SET CT=CT+1,ARR(CT)="Menu Border Foreground Color"_$CHAR(9)_"MENUBORDER"
  ;"--------------------------
  SET CT=CT+1,ARR(CT)="Menu Selection Background Color"_$CHAR(9)_"MENUSELBKGROUND"
  SET CT=CT+1,ARR(CT)="Current Execution Position Background Color"_$CHAR(9)_"HighExecPos"
  SET CT=CT+1,ARR(CT)="Highlighted Breakpoint Background Color"_$CHAR(9)_"HighBkPos"
  SET CT=CT+1,ARR(CT)="Breakpoint Background Color"_$CHAR(9)_"BkPos"
  SET CT=CT+1,ARR(CT)="Highlight Background Color"_$CHAR(9)_"Highlight"
  ;"--------------------------
  SET CT=CT+1,ARR(CT)="Label Foreground & Background Color"_$CHAR(9)_"LABEL"
  SET CT=CT+1,ARR(CT)="'Special' Foreground & Background Color"_$CHAR(9)_"SPECIAL"
  QUIT
  ;  
GETCOLOR24(CURREF,ITEM,FGBG,DEFAULT) ;
  ;"NOTE: for ITEM = BACKGROUND,Highlight,HighBkPos,HighExecPos,BkPos ...
  ;"      these items are stored in @CURREF@(ITEM,"24bit"), NOT @CURREF@(ITEM,"24bit","bg")
  ;"      I will indicate this by setting FGBG to ""
  NEW RESULT
  SET FGBG=$GET(FGBG)
  IF FGBG="" DO
  . SET RESULT=$GET(@CURREF@(ITEM,"24bit"),$GET(DEFAULT))  
  ELSE  DO
  . SET RESULT=$GET(@CURREF@(ITEM,"24bit",FGBG),$GET(DEFAULT))
  . IF RESULT="" DO
  . . SET RESULT=$GET(@CURREF@(ITEM,"24bit"))
  IF RESULT="@" DO
  . SET RESULT=$GET(@CURREF@("TEMP BACKGROUND","24bit")) QUIT:RESULT'=""
  . SET RESULT=$GET(@CURREF@("BACKGROUND","24bit")) QUIT:RESULT'=""
  . SET RESULT=$GET(@CURREF@("BACKGROUND","24bit",FGBG))
  IF RESULT="$" DO
  . SET RESULT=$GET(@CURREF@("MENUBKGROUND","24bit")) QUIT:RESULT'=""
  . SET RESULT=$GET(@CURREF@("MENUBKGROUND","24bit",FGBG))
  QUIT RESULT
  ;
EDITFGBG(CURREF,UsrSlct,COLORMODE) ;
  NEW Label SET Label=$GET(MENU(UsrRaw))
  NEW MENU2,fg,bg,UsrSlct2
  SET MENU2(0)="For "_$PIECE(Label,$CHAR(9),1)_"..."
  SET MENU2(1)="Edit Foreground color"_$CHAR(9)_"fg"
  SET MENU2(2)="Edit Background color"_$CHAR(9)_"bg"        
  SET MENU2(3)="Edit BOTH colors"_$CHAR(9)_"fg&bg"
  WRITE !
M2 ;
  IF COLORMODE="24bit" DO
  . SET fg=$$GETCOLOR24(CURREF,UsrSlct,"fg")
  . SET bg=$$GETCOLOR24(CURREF,UsrSlct,"bg")
  . SET MENU2(1,"COLOR","fg")=$GET(fg)             
  . SET MENU2(1,"COLOR","bg")=$$INVCLRVEC^TMGTERM(fg)
  . ;
  . SET MENU2(2,"COLOR","bg")=bg
  . SET MENU2(2,"COLOR","fg")=$$INVCLRVEC^TMGTERM(bg)
  . ;
  . SET MENU2(3,"COLOR","fg")=fg
  . SET MENU2(3,"COLOR","bg")=bg         
  ELSE  DO                                 
  . SET fg=$GET(@CURREF@(UsrSlct,"fg"),1)
  . SET bg=$GET(@CURREF@(UsrSlct,"bg"),0)
  . DO COLORS^TMGTERM(fg,bg)
  . WRITE "Here are the current colors..."
  . DO VTATRIB^TMGTERM(0) ;"Reset colors
  . WRITE !
  SET UsrSlct2=$$MENU^TMGUSRI2(.MENU2,"^",.UsrRaw)
  IF UsrSlct2="^" GOTO EFGBGDN  
M3 ;                                        
  IF UsrSlct2="fg" DO  GOTO M4
  . SET fg=$$PICKFGC^TMGUSRI8(fg,bg,.COLORMODE)
  IF UsrSlct2="bg" DO  GOTO M4
  . SET bg=$$PICKBGC^TMGUSRI8(bg,.COLORMODE)
  IF UsrSlct2="fg&bg" DO  GOTO M4
  . IF COLORMODE="24bit" DO
  . . WRITE !,"Edit FOREGROUND color",!
  . . DO PRESS2GO^TMGUSRI2
  . . SET fg=$$PICKCOLOR24^TMGUSRI8(,fg)
  . . WRITE !,"Edit BACKGROUND color",!
  . . DO PRESS2GO^TMGUSRI2
  . . SET bg=$$PICKCOLOR24^TMGUSRI8(,bg)
  . ELSE  DO
  . . DO PICKCLRS^TMGTERM(.fg,.bg,.COLORMODE)
M4 ;  
  SET @CURREF@(UsrSlct,"fg")=fg
  SET @CURREF@(UsrSlct,"bg")=bg
  GOTO M2
EFGBGDN ;
  QUIT
  ;
MENUCOLORMODE(MODE,REF) ;
  NEW RESULT SET RESULT=MODE
  NEW MENU,UsrSlct,IDX
MCM1 ;  
  KILL MENU
  SET IDX=1
  SET MENU(0)="Pick Color Mode"
  SET MENU(IDX)="Index color (traditional, 3-4 bits)"_$CHAR(9)_"INDEXED",IDX=IDX+1
  SET MENU(IDX)="256 colors (8 bits) -- NOT FULLY IMPLEMENTED"_$CHAR(9)_"256",IDX=IDX+1
  SET MENU(IDX)="True Colors (24 bits)"_$CHAR(9)_"24bit",IDX=IDX+1
  SET UsrSlct=$$MENU^TMGUSRI2(.MENU,"^")
  IF UsrSlct="^" GOTO MCMDN
  IF "^INDEXED^256^24bit^"[UsrSlct DO  GOTO MCMDN
  . SET RESULT=UsrSlct
  GOTO MCM1
MCMDN ;  
  IF RESULT="24bit" DO ENSUR24COLS(.REF)
  QUIT RESULT
  ;
ShowColorBox ;
  ;"NOTE: COLORBOX^TMGUSRI8 is actually better than this function.
  WRITE !
  WRITE "Horizontal is Background color numbers",!
  WRITE "Vertical is Foreground color numbers",!
  NEW FG,BG
  FOR FG=-1:1:15 DO
  . FOR ROW=1,2 DO
  . . IF FG=-1 DO 
  . . . WRITE "  "
  . . IF FG>-1 DO
  . . . DO VTATRIB^TMGTERM(0)
  . . . IF ROW=1 WRITE "  " QUIT
  . . . WRITE $$RJ^XLFSTR(FG,2,"0")
  . . FOR BG=0:1:15 DO
  . . . IF FG=-1 DO  QUIT
  . . . . IF ROW=1 DO  QUIT
  . . . . . WRITE $SELECT(BG<10:" 0",1:" 1")
  . . . . ELSE  WRITE " ",BG#10
  . . . DO COLORS^TMGTERM(FG,BG)
  . . . WRITE $SELECT(ROW=1:"@#",1:"AZ")
  . . WRITE !
  DO VTATRIB^TMGTERM(0)
  QUIT
  ;
DelBreaks ;
  NEW Menu,UsrSlct,ct,Found,zbS
  ;
EBM1 ;
  KILL Menu,Found
  SET Menu(0)="Pick Breakpoint to Delete"
  SET ct=1
  ;"setup to show a symbol for breakpoint
  SET zbS=""
  ;"WRITE "Job Number: ",tmgDbgJNum,!
  FOR  SET zbS=$ORDER(^TMG("TMGIDE",tmgDbgJNum,"ZBREAK",zbS)) QUIT:(zbS="")  do
  . ;"WRITE "one found",zbS,!
  . SET Menu(ct)=zbS_$CHAR(9)_zbS,ct=ct+1
  . SET Found(zbS)=1
  ;"-----Also get breakpoints stored in GT.M --------------
  NEW gtmBrkPts zshow "B":gtmBrkPts
  NEW zbI SET zbI=0
  FOR  SET zbI=$ORDER(gtmBrkPts("B",zbI)) QUIT:(zbI="")  do
  . SET zbS=$GET(gtmBrkPts("B",zbI)) QUIT:zbS=""
  . ;"WRITE "one gtm found",zbS,!
  . IF $GET(Found(zbS))=1 QUIT
  . SET Menu(ct)=zbS_$CHAR(9)_zbS,ct=ct+1
  . SET Found(zbS)=1
  ;
EBM2  ;
  DO CUP^TMGTERM(1,2) ;"Cursor to line (1,2)
  SET UsrSlct=$$MENU^TMGUSRI2(.Menu,"^")
  IF UsrSlct="^" GOTO EBDN
  IF UsrSlct=0 GOTO EBM2
  SET zbS=UsrSlct
  xecute "ZBREAK -"_zbS
  KILL ^TMG("TMGIDE",tmgDbgJNum,"ZBREAK",zbS)
  ;
  GOTO EBM1
EBDN ;
  QUIT
  ;
GetGTMBrkPts(LIST) ;
  ;"Purpose: get a list of breakpoints, as maintained by GT.M
  ;"Input: LIST -- PASS BY REFERENCE.  AN OUT PARAMETER.  Format:
  ;"         LIST(Position)=1
  NEW gtmBrkPts zshow "B":gtmBrkPts
  NEW zbI SET zbI=0
  FOR  SET zbI=$ORDER(gtmBrkPts("B",zbI)) QUIT:(zbI="")  do
  . SET zbS=$GET(gtmBrkPts("B",zbI)) QUIT:zbS=""
  . IF $GET(LIST(zbS))=1 QUIT
  . SET LIST(zbS)=1
  QUIT
  ;
  ;"============== Code for TRACE functionality =================
  ;
ShowTrace  ;
  ;"Purpose: to show current trace record of execution.
  ;"if $GET(tmgDbgOptions("TRACE"))=1 QUIT
  NEW ref SET ref=$name(^TMG("TMGIDE",$J,"TRACE"))
  IF $DATA(@ref) do
  . WRITE "SHOW TRACE RECORDS:",!
  . NEW NumRecs SET NumRecs=$ORDER(@ref@(""),-1)
  . WRITE NumRecs," trace lines to display",!
  . NEW count SET count=1
  . NEW % SET %=1
  . WRITE "Also display code for each line" DO YN^DICN WRITE !
  . IF %=-1 QUIT
  . NEW showCode SET showCode=(%=1)
  . NEW Colorize  SET Colorize=0
  . IF %=1 DO  QUIT:(%=-1)
  . . SET %=1 WRITE "Colorize code" DO YN^DICN WRITE !
  . . SET Colorize=(%=1)
  . NEW %ZIS
  . SET %ZIS("A")="Enter Output Device: "
  . SET %ZIS("B")="HOME"
  . DO ^%ZIS  ;"standard device call
  . IF POP DO  QUIT
  . . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output.  Aborting.")
  . use IO
  . NEW i SET i=""
  . FOR  SET i=$ORDER(@ref@(i)) QUIT:(i="")!($GET(TMGPTCABORT)=1)  do
  . . NEW s SET s=$GET(@ref@(i))
  . . WRITE s
  . . IF showCode do
  . . . NEW pos SET pos=$PIECE(s,".",$LENGTH(s,"."))
  . . . IF pos="" WRITE "  ??",! QUIT
  . . . ;"WRITE "pos=",pos,!
  . . . NEW code
  . . . do
  . . . . NEW $ETRAP SET $ETRAP="set code=""Error -- ""_pos,$ETRAP="""",$ecode="""""
  . . . . SET code=$text(@pos)
  . . . WRITE ?25,":"
  . . . NEW x FOR x=1:1:$LENGTH(s,".")-1 WRITE " "
  . . . IF Colorize do
  . . . . IF $$ShowLine(code,.tmgDbgOptions)
  . . . . DO SETCOLORS^TMGIDE2C("Reset")
  . . . ELSE  WRITE code
  . . . WRITE !
  . . ELSE  WRITE "           ",!
  . . ;"set count=count+1
  . . IF count>20 do
  . . . DO PRESS2GO^TMGUSRI2 ;" will SET TMGPTCABORT=1 IF user entered ^
  . . . DO CUU^TMGTERM(1)
  . . . WRITE "                                ",!
  . . . DO CUU^TMGTERM(1)
  . . . SET count=1
  ELSE  do
  . WRITE "(No Trace record found)",!
  DO ^%ZISC  ;" Close the output device
  DO PRESS2GO^TMGUSRI2
  QUIT
  ;
RecordTrace(ExecPos)  ;
  ;"Purpose: To keep trace record of execution as program runs.
  ;"Input:ExecPos -- Current execution position
  NEW ref SET ref=$name(^TMG("TMGIDE",$J,"TRACE"))
  NEW Stack DO GetStackInfo^TMGIDE2(.Stack,ExecPos)
  NEW str SET str=$$StackStr(.Stack)
  NEW i SET i=+$GET(@ref)+1
  SET @ref@(i)=str
  SET @ref=i
  QUIT
  ;
StackStr(Stack)  ;
  ;"Purpose: Turn stack array into a single string
  ;"Input: Stack -- PASS BY REFERENCE, Numbered array, as created by GetStackInfo^TMGIDE2
  ;"Result: returns string with latest position, with
  ;"        a "." leading for each level of indenction.
  ;"
  NEW result SET result=""
  NEW count SET count=+$ORDER(Stack(""),-1)
  IF count>0 do
  . NEW x FOR x=1:1:(count-1) SET result=result_"."
  . NEW s SET s=$GET(Stack(count))
  . IF s[" <--" SET s=$PIECE(s," <--",1)
  . IF s[" " SET s=$PIECE(s," ",2)
  . SET result=result_s
  QUIT result
  ;
  ;"============== Code for VAR TRACING functionality =================
  ;
ShowVTrace  ;
  ;"Purpose: Output changes from last step
  NEW tmgRefNum SET tmgRefNum=+$ORDER(^TMG("TMGIDE",$J,"VARTRACE","DELTA",""),-1)
  NEW tmgRefDelta SET tmgRefDelta=$name(^TMG("TMGIDE",$J,"VARTRACE","DELTA",tmgRefNum))
  NEW TMG SET TMG(1)="ADDED^Additions",TMG(2)="KILLED^Kills",TMG(3)="CHANGED^Changes"
  NEW i FOR i=1,2,3 do
  . NEW node SET node=$PIECE(TMG(i),"^",1)
  . NEW title SET title=$PIECE(TMG(i),"^",2)
  . IF $DATA(@tmgRefDelta@(node)) do
  . . WRITE title,": "
  . . NEW varname SET varname=""
  . . FOR  SET varname=$ORDER(@tmgRefDelta@(node,varname)) QUIT:(varname="")  do
  . . . WRITE varname,"=",$GET(@tmgRefDelta@(node,varname))," ; "
  . . WRITE !
  QUIT
  ;
RecordVTrace  ;
  ;"Purpose: To keep a trace of changes to the system variable table.
  NEW tmgFullRef SET tmgFullRef=$name(^TMG("TMGIDE",$J,"VARTRACE","FULL"))
  NEW tmgRefNum SET tmgRefNum=+$ORDER(@tmgFullRef@(""),-1)+1
  IF tmgRefNum'>0 GOTO RVTDn
  NEW tmgRefCurF SET tmgRefCurF=$name(@tmgFullRef@(tmgRefNum))
  NEW tmgRefPriorF SET tmgRefPriorF=$name(@tmgFullRef@(tmgRefNum-1))
  NEW tmgRefDelta SET tmgRefDelta=$name(^TMG("TMGIDE",$J,"VARTRACE","DELTA",tmgRefNum))
  DO StoreVars(tmgRefCurF)
  IF $DATA(@tmgRefPriorF) do
  . DO DiffVars(tmgRefCurF,tmgRefPriorF,tmgRefDelta)
  . KILL @tmgRefPriorF
RVTDn  ;
  QUIT
  ;
StoreVars(tmgRef)  ;
  ;"Purpose: To copy system variable table to a storage area
  ;"Input:  Ref -- the NAME of the global to store table at
  ;"Results: none
  ;"NOTICE: all vars beginning with "tmg" are NOT shown.
  NEW tmgArray zshow "V":tmgArray  ;"copy system table to local variable
  NEW idx SET idx=0
  FOR  SET idx=$ORDER(tmgArray("V",idx)) QUIT:(idx="")  do
  . NEW s SET s=tmgArray("V",idx)
  . NEW varname SET varname=$PIECE(s,"=",1)
  . QUIT:(varname="")!($EXTRACT(varname,1,3)="tmg")
  . NEW value SET value=$p(s,"=",2,999)
  . SET @tmgRef@(varname)=value  ;"reformat and store in a global var
  QUIT
  ;
DiffVars(tmgRefCurF,tmgRefPriorF,tmgRefDelta)  ;
  ;"Purpose: To create a record that shows difference between tmgRefCurF and
  ;"         tmgRefPriorF, and stores the difference
  ;"Note: Possible differences:
  ;"      1. New record has a NEW variable, not previously in existence
  ;"      2. New record has same variable, but changed value
  ;"      3. New record does NOT have variable that previously existed.
  ;"Input: tmgRefCurF -- reference of current full variable store
  ;"       tmgRefPriorF -- reference of prior full viariable store
  ;"       tmgRefDelta -- reference to store changes to.  Output Format:
  ;"         @tmgRefDelta@('ADDED',varname)=value
  ;"         @tmgRefDelta@('KILLED',varname)=""
  ;"         @tmgRefDelta@('CHANGED',varname)=new value
  ;"Result: None.  But any prior entry in @tmgRefDelta is deleted and changed as above.
  ;
  KILL @tmgRefDelta
  NEW varname
  ;"First look for additions and changes
  SET varname=""
  FOR  SET varname=$ORDER(@tmgRefCurF@(varname)) QUIT:(varname="")  do
  . IF $DATA(@tmgRefPriorF@(varname)) DO  QUIT
  . . IF $GET(@tmgRefPriorF@(varname))'=$GET(@tmgRefCurF@(varname)) do
  . . . SET @tmgRefDelta@("CHANGED",varname)=$GET(@tmgRefCurF@(varname))
  . SET @tmgRefDelta@("ADDED",varname)=$GET(@tmgRefCurF@(varname))
  ;
  ;"Next, look for deletions
  SET varname=""
  FOR  SET varname=$ORDER(@tmgRefPriorF@(varname)) QUIT:(varname="")  do
  . IF $DATA(@tmgRefCurF@(varname)) QUIT
  . SET @tmgRefDelta@("KILLED",varname)=$GET(@tmgRefPriorF@(varname))
  ;
  QUIT
  ;";"Finally, look for changes
  ;"set varname=""
  ;"FOR  SET varname=$ORDER(@tmgRefCurF@(varname)) QUIT:(varname="")  do
  ;". IF $DATA(@tmgRefPriorF@(varname))=0 QUIT
  ;". IF $GET(@tmgRefPriorF@(varname))=$GET(@tmgRefCurF@(varname)) QUIT
  ;". SET @tmgRefDelta@("CHANGED",varname)=$GET(@tmgRefCurF@(varname))
  ;"QUIT
  ;