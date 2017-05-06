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
 ;"InitColors -- establish tmgDbgOptions globally-scoped var for colors,
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


ShowPos(Pos)
  ;"A temp function to show out code at a given position.
  NEW line SET line=$text(@Pos)
  WRITE Pos,": " IF $$ShowLine(line) WRITE !
  QUIT


;"ShowLine(line,Options,MaxChar)
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
        . . DO SetColors^TMGIDE2("NORM")
        . . DO DoWrite(partS,.result,MaxChar)
        . . ;"WRITE partS SET result=result+$LENGTH(partS)
        . . SET line=$EXTRACT(line,p-2,999)
        . . NEW code SET code=$$GETWORD^TMGSTUT3(line,1,"{","}")
        . . SET line=$EXTRACT(line,$LENGTH(code)+3,999) ;"shorten to after color tag onward
        . . NEW tmgMode SET tmgMode=$PIECE(code,":",2)
        . . DO SetColors^TMGIDE2(tmgMode)
        . . SET p=$find(line,"{C/}")  ;"look for close color directive
        . . IF p>0 do
        . . . SET partS=$EXTRACT(line,1,p-5) ;"get text up to closing color
        . . . DO DoWrite(partS,.result,MaxChar)
        . . . ;"WRITE partS SET result=result+$LENGTH(partS)
        . . . DO SetColors^TMGIDE2("NORM")
        . . . SET line=$EXTRACT(line,p,999) ;"shorten to next segment after closing color onward
        . . ELSE  do
        . . . DO DoWrite(line,.result,MaxChar)
        . . . ;"WRITE line SET result=result+$LENGTH(line)
        . . . SET line=""
        . ELSE  do
        . . DO DoWrite(line,.result,MaxChar)
        . . ;"WRITE line SET result=result+$LENGTH(line)
        . . SET line=""
        QUIT result

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

MarkupLine(LINE,OPTIONS)
        ;"Purpose: To take an arbitrary LINE of code and parse into data structure
        ;"Input : LINE -- the line of code to consider.  DON'T pass by reference.
        ;"        OPTIONS -- PASS BY REFERENCE.   Format:
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
InitColors
       ;"Purpose: to establish tmgDbgOptions globally-scoped var for colors,
       NEW ref SET ref=$name(^TMG("TMGIDE",$J,"COLORS"))
       NEW refMaster SET refMaster=$name(^TMG("TMGIDE","COLORS"))
       IF ($DATA(@ref)=0) do
       . IF ($DATA(@refMaster)'=0) do
       . . MERGE @ref=^TMG("TMGIDE","COLORS") ;"copy master into job's
       . ELSE  do
       . . IF $DATA(TMGCOLBLACK)=0 DO SETGBLCO^TMGTERM
       . . SET @ref@("BACKGROUND")=TMGCOLBLUE
       . . SET @ref@("HighExecPos")=TMGCOLGREY
       . . SET @ref@("HighBkPos")=TMGCOLBRED
       . . SET @ref@("BkPos")=TMGCOLRED
       . . SET @ref@("Highlight")=TMGCOLFGBWHITE
       . . ;"-----------------------------------
       . . SET @ref@("LABEL","fg")=TMGCOLBYELLOW
       . . SET @ref@("LABEL","bg")=TMGCOLRED
       . . SET @ref@("SPECIAL","fg")=TMGCOLBYELLOW
       . . SET @ref@("SPECIAL","bg")=TMGCOLRED
       . . ;"-----------------------------------
       . . SET @ref@("NORM","fg")=TMGCOLFGBWHITE
       . . SET @ref@("NORM","bg")="@" ;"signal to use current background color
       . . SET @ref@("CMD","fg")=TMGCOLBRED
       . . SET @ref@("CMD","bg")="@"
       . . SET @ref@("FN","fg")=TMGCOLBCYAN
       . . SET @ref@("FN","bg")="@"
       . . SET @ref@("MOD","fg")=TMGCOLBBLUE
       . . SET @ref@("MOD","bg")="@"
       . . SET @ref@("IFN","fg")=TMGCOLRED
       . . SET @ref@("IFN","bg")="@"
       . . SET @ref@("STR","fg")=TMGCOLBMAGENTA
       . . SET @ref@("STR","bg")="@"
       . . SET @ref@("PC","fg")=TMGCOLBRED
       . . SET @ref@("PC","bg")="@"
       . . SET @ref@("#","fg")=TMGCOLBYELLOW
       . . SET @ref@("#","bg")="@"
       . . MERGE @refMaster=@ref
       QUIT
       ;
EditColors
       ;"Purpose: Enable Edit Colors
       WRITE #
       NEW ref SET ref=$name(^TMG("TMGIDE",$J,"COLORS"))
       NEW Menu,Menu2,UsrSlct,UsrSlct2,UsrRaw,fg,bg,ct
       SET ct=1
       SET Menu(0)="Pick Color to Edit"
       SET Menu(ct)="Window Background color"_$CHAR(9)_"BACKGROUND",ct=ct+1
       SET Menu(ct)="Current Execution Position Background Color"_$CHAR(9)_"HighExecPos",ct=ct+1
       SET Menu(ct)="Highlighted Breakpoint Background Color"_$CHAR(9)_"HighBkPos",ct=ct+1
       SET Menu(ct)="Breakpoint Background Color"_$CHAR(9)_"BkPos",ct=ct+1
       SET Menu(ct)="Highlight Background Color"_$CHAR(9)_"Highlight",ct=ct+1

       SET Menu(ct)="Label Foreground & Background Color"_$CHAR(9)_"LABEL",ct=ct+1
       SET Menu(ct)="'Special' Foreground & Background Color"_$CHAR(9)_"SPECIAL",ct=ct+1

       SET Menu(ct)="Normal Text Foreground Color"_$CHAR(9)_"NORM",ct=ct+1
       SET Menu(ct)="Command Foreground Color"_$CHAR(9)_"CMD",ct=ct+1
       SET Menu(ct)="Functions Foreground Color"_$CHAR(9)_"FN",ct=ct+1
       SET Menu(ct)="Module/Global reference Foreground Color"_$CHAR(9)_"MOD",ct=ct+1
       SET Menu(ct)="Mumps intrinsic functions Foreground Color"_$CHAR(9)_"IFN",ct=ct+1
       SET Menu(ct)="String Foreground Color"_$CHAR(9)_"STR",ct=ct+1
       SET Menu(ct)="Post-conditional Foreground Color"_$CHAR(9)_"PC",ct=ct+1
       SET Menu(ct)="Comments Foreground Color"_$CHAR(9)_"#",ct=ct+1
       NEW i
M1     SET i=0
       FOR  SET i=$ORDER(Menu(i)) QUIT:(i="")  do
       . NEW bg,fg
       . NEW tmgMode SET tmgMode=$PIECE(Menu(i),$CHAR(9),2)
       . IF "BACKGROUND,Highlight,HighBkPos,HighExecPos,BkPos"[tmgMode do
       . . SET bg=$GET(@ref@(tmgMode))
       . . SET fg=$SELECT(bg=0:7,1:10)
       . ELSE  do
       . . SET fg=$GET(@ref@(tmgMode,"fg"))
       . . SET bg=$GET(@ref@(tmgMode,"bg"))
       . . IF bg="@" SET bg=$GET(@ref@("BACKGROUND"),0)
       . SET Menu(i,"COLOR","fg")=fg
       . SET Menu(i,"COLOR","bg")=bg
       ;
       SET UsrSlct=$$MENU^TMGUSRI2(.Menu,"^",.UsrRaw)
       IF UsrSlct="^" GOTO ECDn
       IF "BACKGROUND,Highlight,HighBkPos,HighExecPos,BkPos"[UsrSlct DO  GOTO M1
       . SET @ref@(UsrSlct)=$$PICKBGC^TMGTERM()
       IF UsrSlct=0 SET UsrSlct="" GOTO M1
       IF "SPECIAL,LABEL"'[UsrSlct DO  GOTO M1
       . NEW bg SET bg=$GET(@ref@("BACKGROUND"),0)
       . WRITE "Setting bg=",bg,!
       . SET @ref@(UsrSlct,"fg")=$$PICKFGC^TMGTERM(@ref@(UsrSlct,"fg"),bg)

       NEW Label SET Label=$GET(Menu(UsrRaw))
       KILL Menu2
       SET Menu2(0)="For "_$PIECE(Label,$CHAR(9),1)_"..."
       SET Menu2(1)="Edit Foreground color"_$CHAR(9)_"fg"
       SET Menu2(2)="Edit Background color"_$CHAR(9)_"bg"
       SET Menu2(3)="Edit BOTH colors"_$CHAR(9)_"fg&bg"
       WRITE !
M2     SET fg=+$GET(@ref@(UsrSlct,"fg"),1)
       SET bg=+$GET(@ref@(UsrSlct,"bg"),0)
       DO VCOLORS^TMGTERM(fg,bg)
       WRITE "Here are the current colors..."
       DO VTATRIB^TMGTERM(0) ;"Reset colors
       WRITE !
       SET UsrSlct2=$$MENU^TMGUSRI2(.Menu2,"^",.UsrRaw)
       IF UsrSlct2="^" GOTO M1

M3     IF UsrSlct2="fg" DO  GOTO M2
       . SET @ref@(UsrSlct,"fg")=$$PICKFGC^TMGTERM(@ref@(UsrSlct,"fg"),@ref@(UsrSlct,"bg"))
       IF UsrSlct2="bg" DO  GOTO M2
       . SET @ref@(UsrSlct,"bg")=$$PICKBGC^TMGTERM(@ref@(UsrSlct,"bg"))
       IF UsrSlct2="fg&bg" DO   GOTO M2
       . DO PICKCLRS^TMGTERM(.fg,.bg)
       . SET @ref@(UsrSlct,"fg")=fg
       . SET @ref@(UsrSlct,"bg")=bg
       GOTO M2

ECDn
       NEW % SET %=2
       WRITE "Set current colors as default"
       DO YN^DICN
       IF %=1 do
       . KILL ^TMG("TMGIDE","COLORS")
       . MERGE ^TMG("TMGIDE","COLORS")=^TMG("TMGIDE",$J,"COLORS")
       QUIT
       ;
       ;
TestColors
       DO InitColors
       NEW tmgMode
       FOR tmgMode="Highlight","HighExecPos","BkPos","HighBkPos","SPECIAL","NORM","LABEL","CMD","FN","MOD","IFN","STR","PC","#" do
       . DO SetColors^TMGIDE2(tmgMode)
       . WRITE "Here is text for ",tmgMode,"...."
       . DO SetColors^TMGIDE2("Reset")
       . WRITE !
       QUIT


DelBreaks ;
       NEW Menu,UsrSlct,ct,Found,zbS

EBM1   KILL Menu,Found
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
EBM2   DO CUP^TMGTERM(1,2) ;"Cursor to line (1,2)
       SET UsrSlct=$$MENU^TMGUSRI2(.Menu,"^")
       IF UsrSlct="^" GOTO EBDN
       IF UsrSlct=0 GOTO EBM2
       SET zbS=UsrSlct
       xecute "ZBREAK -"_zbS
       KILL ^TMG("TMGIDE",tmgDbgJNum,"ZBREAK",zbS)
       ;
       GOTO EBM1
       ;
EBDN   QUIT
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

 ;"============== Code for TRACE functionality =================

ShowTrace
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
        . . . . DO SetColors^TMGIDE2("Reset")
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

RecordTrace(ExecPos)
        ;"Purpose: To keep trace record of execution as program runs.
        ;"Input:ExecPos -- Current execution position
        NEW ref SET ref=$name(^TMG("TMGIDE",$J,"TRACE"))
        NEW Stack DO GetStackInfo^TMGIDE2(.Stack,ExecPos)
        NEW str SET str=$$StackStr(.Stack)
        NEW i SET i=+$GET(@ref)+1
        SET @ref@(i)=str
        SET @ref=i
        QUIT

StackStr(Stack)
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

 ;"============== Code for VAR TRACING functionality =================

ShowVTrace
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


RecordVTrace
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
RVTDn   QUIT

StoreVars(tmgRef)
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

DiffVars(tmgRefCurF,tmgRefPriorF,tmgRefDelta)
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

 ;"================================================================