TMGCOD01 ;TMG/kst-Code parsing  ;2/17/15
         ;;1.0;TMG-LIB;**1**;2/15/15
 ;
 ;"TMG CODE PARSING
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;
 ;"NOTE: Output from this routine can be reassembled via code in ^TMGCOD02
 ;"=======================================================================
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"PARSEPOS(TAG,OFFSET,ROUTINE,REFOUT,MODE)  --PARSE CODE STARTING AT POS
 ;"PARSBLK(TMGBLK,OFFSET,REFOUT)  -- take an array with code and parse it into data structure
 ;"GETBLK(TAG,OFFSET,ROUTINE,OUT,MODE)  -- GET BLOCK from source code, into array
 ; 
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;"PARSLINE(TMGBLK,OFFSET,REFOUT)  -- PARSE A LINE, including any DO block associated
 ;"GETSUBLK(INITLINE,TMGBLK,OFFSET,TMGSUBBLK)  -- pull lines of a DO block (starting with ".")
 ;"DOTCT(LINE) --DOT COUNT
 ;"PARSARGS(ARGS,REFOUT,REFROOT,OFFSET) -- PARSE A COMMAND'S ARGUMENTS
 ;"NUMBEFOR(ARR,IDX) --FIND AN AVAILABLE (NON-EXISTANT) INDEX NUMBER BEFORE IDX IN ARR
 ;"NUMAFTER(ARR,IDX) -- FIND AN AVAILABLE (NON-EXISTANT) INDEX NUMBER AFTER IDX IN ARR
 ;"SETUPTBL(TABLE)  --Set up command abbreviation <--> expansion table.
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"  TMGSTUT3, XLFSTR
 ;"=======================================================================
 ;
PARSEPOS(TAG,OFFSET,ROUTINE,REFOUT,MODE)  ;"PARSE CODE STARTING AT POS
  ;"Purpose: To take an arbitrary address position in code and parse one 
  ;"         block into data structure
  ;"Input : TAG -- Tag in routine to parse from, e.g. "EN"
  ;"        OFFSET -- used for recursive calls.  Leave blank/null when first calling
  ;"            E.g. "3"
  ;"        ROUTINE -- Routine to parse from, e.g. "TMGTEST" (not '^TMGTEST')
  ;"          NOTE: In the above example code would be parse from EN+3^TMGTEST
  ;"        REFOUT -- PASS BY NAME.  AN OUT PARAMETER.  Format:
  ;"             @REFOUT@(offset)=<full original line, minus tag>
  ;"             @REFOUT@(offset,"TAG")=<TAG>
  ;"             @REFOUT@("IX","TAG",<TAG>,offset)=""
  ;"             @REFOUT@("IX","CMD",LONG_CMD,OFFSET)=""
  ;"             @REFOUT@("IX","$$FN",<ROUTINE>,<FNNAME>,OFFSET)=""
  ;"             @REFOUT@("IX","DO",<ROUTINE>,<TAG>,OFFSET)=""
  ;"             @REFOUT@("IX","GLOBAL",<GLOBAL NAME>,<CMD or FN>)=""
  ;"             @REFOUT@("IX","@",TMGLCMD,OFFSET)=""
  ;"             @REFOUT@("IX","$TEXT",<ROUTINE>,<TAG>,OFFSET)=""
  ;"             @REFOUT@("IX","GOTO",<ROUTINE>,<TAG>,OFFSET)=""
  ;"             @REFOUT@("ARGS",... see PARSLINE
  ;"             @REFOUT@(".",... see PARSLINE
  ;"        MODE -- OPTIONAL.
  ;"             0 -- parse up to next tag.  Default
  ;"             1 -- parse up to next tag with paramegers (e.g. EN(X,Y,X) )
  ;"             2 -- parse up to next named tag.  Requires:
  ;"                  MODE("NAME")=<named tag>  <-- if not found, reverts to mode 3
  ;"             3 -- parse to end of file.
  ;"Results : none.  See REFOUT above.
  NEW TMGBLK
  DO GETBLK(.TAG,.OFFSET,ROUTINE,.TMGBLK,.MODE) 
  DO PARSBLK(.TMGBLK,"",REFOUT)
  QUIT
  ;
PARSBLK(TMGBLK,OFFSET,REFOUT,REFROOT)  ;"PARSE BLOCK
  ;"Purpose: To take an array with code and parse it into data structure
  ;"Input : TMGBLK -- PASS BY REFERENCE.  Array with code lines.
  ;"        OFFSET -- used for recursive calls.  Leave blank/null when first calling
  ;"            E.g. "3"
  ;"        REFOUT -- PASS BY NAME.  AN OUT PARAMETER.  See PARSPOS for details.
  ;"        REFROOT-- PASS BY NAME.  AN OUT PARAMETER.  On first call, this should
  ;"            be either NULL ("") or same as REFOUT.  Used in recursive calls.
  ;"Results : none.  See OUT above.
  NEW FIRST SET FIRST=1
  NEW TMGCMDABVR  ;"Will be used by sub routines in global scope
  SET OFFSET=$GET(OFFSET)
  SET REFROOT=$GET(REFROOT,REFOUT)
  FOR  SET OFFSET=$ORDER(TMGBLK(OFFSET)) QUIT:+OFFSET'=OFFSET  DO
  . ;"NEW LINE SET LINE=TMGBLK(OFFSET)
  . DO PARSLINE(.TMGBLK,.OFFSET,REFOUT,REFROOT)
  QUIT
  ;
PARSLINE(TMGBLK,OFFSET,REFOUT,REFROOT)  ;"PARSE A LINE, including any DO block associated
  ;"INPUT: TMGBLK -- PASS BY REFERENCE.  Array of text containing source lines
  ;"       OFFSET -- number index in TMGBLK array. PASS BY REFERENCE.
  ;"       REFOUT -- pass by NAME.  Output array.  See format in PARSBLK
  ;"          @REFOUT@(OFFSET,"TAG","ARGS")=ArgsPointer#, pointer to @ROOT@("ARGS",OFFSET) See below                         
  ;"          @REFOUT@(OFFSET,"COMMENT")=<comment>                         
  ;"          @REFOUT@(OFFSET,"STRINGS",#)=strings
  ;"          @REFOUT@(OFFSET,"INDENT")=<text that indents from left margin>
  ;"          @REFOUT@(OFFSET,"TRAILING")=<trailing spaces>
  ;"          @REFOUT@(OFFSET,<cmd #>,"CMD")=<CMD> as found in code
  ;"          @REFOUT@(OFFSET,<cmd #>,"CMD","ABVR")=<CMD> shortened form
  ;"          @REFOUT@(OFFSET,<cmd #>,"CMD","XPND")=<CMD> expanded form
  ;"          @REFOUT@(OFFSET,<cmd #>,"CMD","PRECEEDING")=<extra space before command, if any>
  ;"          @REFOUT@(OFFSET,<cmd #>,"CMD ARGS","@ORIG")=Original line  
  ;"          @REFOUT@(OFFSET,<cmd #>,"CMD ARGS")=ArgsPointer#, pointer to @ROOT@("ARGS",OFFSET) See below
  ;"          @REFOUT@(OFFSET,<cmd #>,"POST COND")=ArgsPointer#, pointer to @ROOT@("ARGS",OFFSET) See below
  ;"          @REFOUT@(OFFSET,<cmd #>,".")="<DOT_BLOCK>" <-- links to "." index
  ;"    ------Below is *ROOT, not *OUT-------
  ;"          @REFROOT@("IX","CMD",LONG_CMD,OFFSET)=""
  ;"          @REFROOT@("IX","$$FN",<ROUTINE>,<FNNAME>,OFFSET)=""
  ;"          @REFROOT@("IX","DO",<ROUTINE>,<TAG>,OFFSET)=""
  ;"          @REFROOT@("IX","GOTO",<ROUTINE>,<TAG>,OFFSET)=""
  ;"          @REFROOT@("IX","$TEXT",<ROUTINE>,<TAG>,OFFSET)=""
  ;"          @REFROOT@(".",OFFSET,<cmd #>,... (same pattern as above)
  ;"              OFFSET is the line number of the command that holds the original
  ;"                   argument-less DO (i.e. the TOP of the block
  ;"              NOTE: I had to move the dot blocks so that they are not nested
  ;"               because at dot depth of ~5, I was getting error of the variables
  ;"               having too many subscripts (nested too deep).
  ;"               This way, all dot blocks are attached to the ROOT (@REFROOT@)
  ;"          @REFROOT@("ARGS",OFFSET,<Pointer#>)=<args block> See format below
  ;"
  ;"        REFROOT-- PASS BY NAME.  AN OUT PARAMETER.  On first call, this should
  ;"            be either NULL ("") or same as REFOUT.  Used in recursive calls.
  ;"
  ;"    --------Format of an arguments block as below -----------------------------
  ;"          Moved... see docs in PARSARGS() 
  ;"
  ;"NOTE: uses TMGCMDABVR in global scope.  If variable is empty, then will be filled here
  ;"Results: none
  NEW LINE SET LINE=$GET(TMGBLK(OFFSET))  ;"should never contain LABEL / TAG
  SET REFROOT=$GET(REFROOT,REFOUT)
  SET @REFOUT@(OFFSET,"@ORIG")=LINE
  NEW INITLINE SET INITLINE=LINE
  NEW TOKEN,ARG,POS,CH
  ;"Extract and remove comments first...
  SET POS=1 FOR  SET POS=$FIND(LINE,";",POS) QUIT:(POS'>0)  DO
  . IF $$INQT^TMGSTUT3(LINE,POS-1) QUIT
  . SET @REFOUT@(OFFSET,"COMMENT")=$EXTRACT(LINE,POS-1,999)
  . SET LINE=$EXTRACT(LINE,1,POS-2)
  IF LINE="" GOTO PLNDN
  ;"
  ;"Extract trailing characters (tabs or spaces)
  NEW DONE SET DONE=0,CH=""
  NEW TRAILING SET TRAILING=""
  NEW IDX FOR IDX=$LENGTH(LINE):-1:1 DO  QUIT:DONE
  . SET CH=$EXTRACT(LINE,IDX)
  . IF (CH'=$CHAR(9))&(CH'=" ") SET DONE=1 QUIT
  . SET TRAILING=CH_TRAILING
  ;"SET @REFOUT@(OFFSET,"COMMENT")=TRAILING_$GET(@REFOUT@(OFFSET,"COMMENT"))
  SET @REFOUT@(OFFSET,"TRAILING")=TRAILING
  ;"SET LINE=$$TRIM^XLFSTR(LINE,"R")
  SET LINE=$EXTRACT(LINE,1,$LENGTH(LINE)-$LENGTH(TRAILING))
  ;"
  ;"Temporarily remove strings which can confuse parsing, replacing with ;<#>;
  NEW STRINGS,QTPOS,QTCOUNT SET QTPOS=0,QTCOUNT=1
  FOR  DO  QUIT:QTPOS'>0  
  . SET QTPOS=$FIND(LINE,"""")-1
  . IF QTPOS'>0 QUIT
  . NEW P2 SET P2=$$ENDQTPOS^TMGSTUT3(LINE,QTPOS)
  . IF P2'>0 SET QTPOS=0 QUIT
  . NEW PARTA,STR,PARTB
  . SET PARTA=$EXTRACT(LINE,1,QTPOS-1)
  . SET STR=$EXTRACT(LINE,QTPOS,P2)
  . SET PARTB=$EXTRACT(LINE,P2+1,$LENGTH(LINE))
  . SET @REFOUT@(OFFSET,"STRINGS",QTCOUNT)=STR 
  . SET LINE=PARTA_";"_QTCOUNT_";"_PARTB  ;"Line should not contain any other ";"'s 
  . SET QTCOUNT=QTCOUNT+1
  ;
  ;"Remove and store TAG / label 
  IF $EXTRACT(LINE,1)'=" " DO
  . NEW XFLINE SET XFLINE=$TRANSLATE(LINE,$CHAR(9)," ")
  . NEW TEMP SET TEMP=$PIECE(XFLINE," ",1)
  . SET LINE=$EXTRACT(LINE,$LENGTH(TEMP)+1,$LENGTH(LINE))
  . SET LABEL=$PIECE(TEMP,"(",1)
  . NEW ARGS SET ARGS=$EXTRACT(TEMP,$LENGTH(LABEL)+1,$LENGTH(TEMP))
  . IF ARGS'="()" SET ARGS=$PIECE($PIECE(ARGS,"(",2,999),")",1)
  . SET @REFOUT@(OFFSET,"TAG")=LABEL
  . IF ARGS'="" DO
  . . NEW NIDX SET NIDX=$$NXTARGI(REFROOT,OFFSET)
  . . NEW TEMPREF SET TEMPREF=$NAME(@REFROOT@("ARGS",OFFSET,NIDX))
  . . ;"NEW TEMPREF SET TEMPREF=$NAME(@REFOUT@(OFFSET,"TAG","ARGS"))
  . . IF ARGS="()" SET @TEMPREF="()"
  . . ELSE  DO PARSARGS(ARGS,TEMPREF,REFROOT,OFFSET)
  . . IF $DATA(@TEMPREF)>0 SET @REFOUT@(OFFSET,"TAG","ARGS")=NIDX 
  . IF LABEL'="" SET @REFOUT@("IX","TAG",LABEL,+OFFSET)=""
  ;
  ;"Extract indention characters (tabs or spaces)
  NEW CH,DONE SET DONE=0
  NEW INDENT SET INDENT=""
  NEW IDX FOR IDX=1:1:$LENGTH(LINE) DO  QUIT:DONE
  . SET CH=$EXTRACT(LINE,IDX)
  . IF (CH'=$CHAR(9))&(CH'=" ")&(CH'=".") SET DONE=1 QUIT
  . SET INDENT=INDENT_CH
  SET @REFOUT@(OFFSET,"INDENT")=INDENT
  SET LINE=$EXTRACT(LINE,$LENGTH(INDENT)+1,$LENGTH(LINE))
  ;"IF LINE="" GOTO PLNDN
  ;
  ;"====== Loop to get COMMAND ARG pairs ===="
  NEW OFFSETSAVE SET OFFSETSAVE=OFFSET 
  NEW CMDCT SET CMDCT=1
  FOR  QUIT:($LENGTH(LINE)'>0)  DO
  . NEW LEADINGCMDSPC SET LEADINGCMDSPC=""
  . FOR  QUIT:($EXTRACT(LINE,1)'=" ")!($LENGTH(LINE)=0)  DO
  . . SET LEADINGCMDSPC=LEADINGCMDSPC_" "
  . . SET LINE=$EXTRACT(LINE,2,$LENGTH(LINE))
  . IF LEADINGCMDSPC'="" SET @REFOUT@(OFFSETSAVE,CMDCT,"CMD","PRECEEDING")=LEADINGCMDSPC
  . NEW CMD SET CMD=$PIECE(LINE," ",1)
  . SET LINE=$PIECE(LINE," ",2,9999)
  . IF CMD="" QUIT
  . IF CMD[":" DO    
  . . IF $EXTRACT(CMD,$LENGTH(CMD))="^",$EXTRACT(LINE,1)="(" DO  ;"handle outdated DSM "^[space](" syntax
  . . . SET CMD=CMD_" "_$PIECE(LINE," ",1)
  . . . SET LINE=$PIECE(LINE," ",2,9999)
  . . NEW PCEXPR SET PCEXPR=$PIECE(CMD,":",2,9999)
  . . SET CMD=$PIECE(CMD,":",1)
  . . NEW NIDX SET NIDX=$$NXTARGI(REFROOT,OFFSETSAVE)
  . . NEW TEMPREF SET TEMPREF=$NAME(@REFROOT@("ARGS",OFFSETSAVE,NIDX))
  . . ;"NEW TEMPREF SET TEMPREF=$NAME(@REFOUT@(OFFSETSAVE,CMDCT,"POST COND"))
  . . DO PARSARGS(PCEXPR,TEMPREF,REFROOT,OFFSETSAVE)
  . . IF $DATA(@TEMPREF)>0 SET @REFOUT@(OFFSETSAVE,CMDCT,"POST COND")=NIDX 
  . NEW SCMD,TMGLCMD,UCMD SET UCMD=$$UP^XLFSTR(CMD)
  . DO TABLCNVT(CMD,"CMD",.SCMD,.TMGLCMD)  
  . SET @REFOUT@(OFFSETSAVE,CMDCT,"CMD")=CMD
  . SET @REFOUT@(OFFSETSAVE,CMDCT,"CMD","ABVR")=SCMD
  . SET @REFOUT@(OFFSETSAVE,CMDCT,"CMD","XPND")=TMGLCMD
  . ;"restore later --> SET @REFOUT@("IX CMD",CMD,OFFSETSAVE)=""
  . ;"restore later --> SET @REFOUT@("IX CMD S",SCMD,OFFSETSAVE)=""
  . ;"restore later --> SET @REFOUT@("IX CMD L",TMGLCMD,OFFSETSAVE)=""
  . SET @REFROOT@("IX","CMD",TMGLCMD,OFFSETSAVE)=""
  . NEW ARGS SET ARGS=$PIECE(LINE," ",1)
  . SET LINE=$PIECE(LINE," ",2,9999)  
  . IF $EXTRACT(ARGS,$LENGTH(ARGS))="^",$EXTRACT(LINE,1)="(" DO  ;"handle outdated DSM "^[space](" syntax
  . . SET ARGS=ARGS_" "_$PIECE(LINE," ",1)
  . . SET LINE=$PIECE(LINE," ",2,9999)
  . NEW NIDX SET NIDX=$$NXTARGI(REFROOT,OFFSETSAVE)
  . NEW TEMPREF SET TEMPREF=$NAME(@REFROOT@("ARGS",OFFSETSAVE,NIDX))
  . DO PARSARGS(ARGS,TEMPREF,REFROOT,OFFSETSAVE)
  . IF $DATA(@TEMPREF)>0 SET @REFOUT@(OFFSETSAVE,CMDCT,"CMD ARGS")=NIDX 
  . IF (TMGLCMD="DO"),$$TRIM^XLFSTR(ARGS)="" DO
  . . NEW SUBBLOCK DO GETSUBLK(INITLINE,.TMGBLK,.OFFSET,.SUBBLOCK)
  . . NEW TMGSUBBLKREF SET TMGSUBBLKREF=$NAME(@REFROOT@(".",OFFSETSAVE))
  . . DO PARSBLK(.SUBBLOCK,"",TMGSUBBLKREF,REFROOT)  ;"PARSE BLOCK
  . . SET @REFOUT@(OFFSETSAVE,CMDCT,".")="<DOT_BLOCK>"
  . SET CMDCT=CMDCT+1
  ;
  IF $DATA(@REFOUT@(OFFSET,1))=0,($GET(@REFOUT@(OFFSET,"INDENT"))=""),($GET(@REFOUT@(OFFSET,"TRAILING"))'="") DO
  . ;"If not indent, but has trailing, and no command, then use trailing as indent. 
  . SET @REFOUT@(OFFSET,"INDENT")=$GET(@REFOUT@(OFFSET,"TRAILING"))
  . SET @REFOUT@(OFFSET,"TRAILING")=""
PLNDN ;
  QUIT
  ;
GETSUBLK(INITLINE,TMGBLK,OFFSET,TMGSUBBLK)  ;"EXTRACT DO BLOCK.
  ;"Purpose: to pull lines of a DO block (starting with ".")
  ;"Input: INITLINE -- The line that containes the DO starting the do block.
  ;"           This line will determine the initial level of the do indentation
  ;"           amount. 
  ;"       TMGBLK -- PASS BY REFERENCE.  This is the parent block to pull from
  ;"       OFFSET -- PASS BY REFERENCE.  this should initially be the index
  ;"           in TMGBLK just **before** the dotted DO block lines.  When done
  ;"           this function will leave OFFSET pointing to the last line in
  ;"           the do block, so that the next $ORDER() will move it to the next
  ;"           appropriate line to be processed.
  ;"      TMGSUBBLK -- PASS BY REFERENCE.  This will be filled with contents of
  ;"           do block.  NOTE: 1 "." will be removed from the beginning of each
  ;"           line.  Output format:
  ;"           TMGSUBBLK(Offset)=<line trimmed of 1 ".">
  NEW DONE SET DONE=0
  NEW TMP SET TMP=$TRANSLATE($GET(INITLINE)," ","")  ;"delete all spaces
  NEW DOTCT SET DOTCT=$$DOTCT(TMP) 
  FOR  SET OFFSET=$ORDER(TMGBLK(OFFSET)) QUIT:OFFSET=""  DO  QUIT:DONE
  . NEW LINE SET LINE=TMGBLK(OFFSET)
  . IF $$DOTCT(LINE)'>DOTCT SET DONE=1 QUIT
  . NEW DONE SET DONE=0
  . ;"Leave any '.' in, it will be stored in INDENT later.
  . SET TMGSUBBLK(OFFSET)=LINE  ;"don't trim.
  SET OFFSET=$ORDER(TMGBLK(OFFSET),-1)
  QUIT
  ;
DOTCT(LINE) ;"DOT COUNT
  NEW TMP SET TMP=$GET(LINE)  ;"Should not contain any TAB's at this point.
  SET TMP=$TRANSLATE($GET(TMP)," ","")  ;"delete all spaces
  NEW DOTCT,CH FOR DOTCT=1:1 SET CH=$EXTRACT(TMP,DOTCT) QUIT:CH'="."
  QUIT DOTCT-1
  ;  
PARSARGS(ARGS,REFOUT,REFROOT,OFFSET) ;"PARSE A COMMAND'S ARGUMENTS
  ;"INPUT: Args -- string containing code with arguments
  ;"       REFOUT -- PASS BY NAME. AN OUT PARAMETER.  Format:
  ;"          @REFOUT@(<CommaNum>,<Seq#>,<Type> ....
  ;"            @REFOUT@(<CommaNum>)="<NULL>"  <-- in case of empty param, e.g. ("this",,,"that") 
  ;"            @REFOUT@(<CommaNum>,<Seq#>,"PROC","VALUE")=<Function Name / Tag> 
  ;"            @REFOUT@(<CommaNum>,<Seq#>,"PROC","ROUTINE")=<ROUTINE NAME> 
  ;"            @REFOUT@(<CommaNum>,<Seq#>,"PROC","ARGS",<CommaNum><Seq#>...
  ;"            @REFOUT@(<CommaNum>,<Seq#>,"$$FN","VALUE")=<Function Name / Tag> 
  ;"            @REFOUT@(<CommaNum>,<Seq#>,"$$FN","ROUTINE")=<ROUTINE NAME> 
  ;"            @REFOUT@(<CommaNum>,<Seq#>,"$$FN","ARGS",<CommaNum><Seq#>...
  ;"            @REFOUT@(<CommaNum>,<Seq#>,"$FN","VALUE")=<NAME> (as found in code)
  ;"            @REFOUT@(<CommaNum>,<Seq#>,"$FN","VALUE","ABVR")=Short form of function
  ;"            @REFOUT@(<CommaNum>,<Seq#>,"$FN","VALUE","XPND")=expanded form of function
  ;"            @REFOUT@(<CommaNum>,<Seq#>,"$SV","VALUE")=<NAME> (as found in code)
  ;"            @REFOUT@(<CommaNum>,<Seq#>,"$SV","VALUE","ABVR")=Short form of function
  ;"            @REFOUT@(<CommaNum>,<Seq#>,"$SV","VALUE","XPND")=expanded form of function
  ;"            @REFOUT@(<CommaNum>,<Seq#>,"$FN","ARGS",<CommaNum><Seq#>...
  ;"            @REFOUT@(<CommaNum>,<Seq#>,"POST COND",<CommaNum><Seq#>...
  ;"            @REFOUT@(<CommaNum>,<Seq#>,"NUM","VALUE")=<VALUE> 
  ;"            @REFOUT@(<CommaNum>,<Seq#>,"GLOBAL","VALUE")=<NAME>
  ;"            @REFOUT@(<CommaNum>,<Seq#>,"VAR","VALUE")=<NAME>
  ;"            @REFOUT@(<CommaNum>,<Seq#>,"VAR","PARENS")=<CommaNum><Seq#>...
  ;"            @REFOUT@(<CommaNum>,<Seq#>,"COMP/ASSIGN","VALUE")=<NAME>
  ;"            @REFOUT@(<CommaNum>,<Seq#>,"BOOL","VALUE")=<VALUE>
  ;"            @REFOUT@(<CommaNum>,<Seq#>,"CARET","VALUE")="^"
  ;"            @REFOUT@(<CommaNum>,<Seq#>,"@","VALUE")="@"
  ;"            @REFOUT@(<CommaNum>,<Seq#>,"LINEFEED","VALUE")="!"
  ;"            @REFOUT@(<CommaNum>,<Seq#>,"TAB","VALUE")="?"
  ;"            @REFOUT@(<CommaNum>,<Seq#>,":","VALUE")=":"
  ;"            @REFOUT@(<CommaNum>,<Seq#>,"CONCAT","VALUE")="_"
  ;"            @REFOUT@(<CommaNum>,<Seq#>,"STRING","NUM")=<String replacement #>
  ;"            @REFOUT@(<CommaNum>,<Seq#>,"MATH","VALUE")=<OPERATOR>
  ;"            @REFOUT@(<CommaNum>,<Seq#>,"OTHER","VALUE")=<VALUE>
  ;"NOTE: The end result of ARGS storage is this:
  ;"   @ROOT@("ARGS",<CodeLineOffset>,<Pointer#>,<CommaNum>,<Seq#>,...
  ;"NOTE: May use TMGCMDABVR in global scope.  If variable is empty, then will be filled here
  ;"      May use TMGLCMD, TMGLFN in global scope.  Optional, no problem if missing. 
  ;"Results: none
  ;"Note: ARGS should not contain any strings at this point.
  SET @REFOUT@("@ORIG")=$GET(ARGS)
  NEW PARTA,PARTB,SUBARGS,LEN
  SET TMGLCMD=$GET(TMGLCMD) ;"accessed in global scope
  SET TMGLFN=$GET(TMGLFN) ;"accessed in global scope
  NEW COMMANUM SET COMMANUM=1
  NEW TOKENARR,TOKENCT SET TOKENCT=0  ;"hold list of tokens, in order of encounter.
  NEW TOKEN SET TOKEN=""
  NEW PRIORTOKEN SET PRIORTOKEN=""
  NEW JDX SET JDX=0
  NEW IDX SET IDX=0
  NEW DONE SET DONE=0
  FOR  SET LEN=$LENGTH(ARGS) QUIT:(DONE=1)!(LEN=0)  DO
  . SET IDX=IDX+1,JDX=JDX+1
  . SET PRIORTOKEN=TOKEN
  . SET TOKEN=$$NEXTTOKN^TMGSTUT3(.ARGS)
  . IF TOKEN="" DO
  . . NEW DONE SET DONE=0
  . . NEW FIRSTCH SET FIRSTCH=$EXTRACT(ARGS,1)
  . . IF FIRSTCH=";" DO    ;" comment ';' are gone, so a ";" is a string marker
  . . . NEW POS SET POS=$FIND(ARGS,";",2) IF POS'>0 QUIT
  . . . SET TOKEN=$EXTRACT(ARGS,1,POS-1)
  . . . ;"SET ARGS=$EXTRACT(ARGS,$LENGTH(TOKEN)+1,LEN)
  . . ELSE  IF "^!&'*,-/\<>?@[]_#=: "[FIRSTCH DO
  . . . SET TOKEN=FIRSTCH
  . . ELSE  NEW CH,POS FOR POS=1:1:LEN DO  QUIT:DONE  ;"gather next non alphanumeric chars
  . . . SET CH=$EXTRACT(ARGS,POS)
  . . . SET DONE=(CH?1ULN)!(CH=";")!(CH="$") QUIT:DONE
  . . . SET TOKEN=TOKEN_CH
  . . SET ARGS=$EXTRACT(ARGS,$LENGTH(TOKEN)+1,LEN)
  . SET TOKENCT=TOKENCT+1,TOKENARR(COMMANUM,TOKENCT)=TOKEN
  . IF ($FIND(TOKEN,"$")-1)>0 DO  QUIT:(DONE'=1)  ;"Handle functions, intrinsic functions, special $Vars
  . . NEW FNVTYPE
  . . NEW FIRSTCH SET FIRSTCH=$EXTRACT(ARGS,1)
  . . IF (FIRSTCH'=""),"(^ "[FIRSTCH DO
  . . . IF TOKEN["$$" SET FNVTYPE="$$FN"
  . . . ELSE  SET FNVTYPE="$FN"
  . . ELSE  SET FNVTYPE="$SV"
  . . NEW FNVNAME SET FNVNAME=TOKEN
  . . NEW ROUTINE SET ROUTINE=""
  . . IF $EXTRACT(ARGS,1)="^" DO
  . . . SET ARGS=$EXTRACT(ARGS,2,LEN)
  . . . SET ROUTINE=$$NEXTTOKN^TMGSTUT3(.ARGS)
  . . SET @REFOUT@(COMMANUM,JDX,FNVTYPE,"VALUE")=FNVNAME
  . . NEW TMGLFN SET TMGLFN="" 
  . . IF (FNVTYPE="$FN")!(FNVTYPE="$SV") DO
  . . . NEW NODE SET NODE=$EXTRACT(FNVTYPE,2,3)
  . . . IF $DATA(TMGCMDABVR)=0 DO SETUPTBL(.TMGCMDABVR)
  . . . NEW SN,LN SET (SN,LN)=TOKEN  ;"was ="?"
  . . . IF $DATA(TMGCMDABVR(NODE,"XPND",FNVNAME)) DO
  . . . . SET SN=FNVNAME,LN=$GET(TMGCMDABVR(NODE,"XPND",FNVNAME))
  . . . IF $DATA(TMGCMDABVR(NODE,"ABVR",FNVNAME)) DO
  . . . . SET LN=FNVNAME,SN=$GET(TMGCMDABVR(NODE,"ABVR",FNVNAME))
  . . . SET @REFOUT@(COMMANUM,JDX,FNVTYPE,"VALUE","ABVR")=SN
  . . . SET @REFOUT@(COMMANUM,JDX,FNVTYPE,"VALUE","XPND")=LN
  . . . IF FNVTYPE="$FN" SET TMGLFN=LN
  . . IF FNVNAME["$$" DO
  . . . SET @REFOUT@(COMMANUM,JDX,FNVTYPE,"ROUTINE")=ROUTINE
  . . . NEW TEMPRTN SET TEMPRTN=ROUTINE IF TEMPRTN="" SET TEMPRTN="[LOCAL]"
  . . . SET @REFROOT@("IX","$$FN",TEMPRTN,$PIECE(FNVNAME,"$$",2),OFFSET)=""
  . . SET TOKENARR(COMMANUM,TOKENCT,"TYPE")=FNVTYPE
  . . IF $EXTRACT(ARGS,1)="(" DO
  . . . DO HNDFPARG(COMMANUM,JDX,FNVTYPE,.ARGS,OFFSET,REFOUT,REFROOT) ;"HANDLE FUNCTION ($ OR $$) OR PROC ARGUMENTS
  . . . ;"NEW TEMPSAVARGS SET TEMPSAVARGS=ARGS 
  . . . ;"SET SUBARGS=$$MATCHXTR^TMGSTUT3(ARGS,"(",,,"(")
  . . . ;"SET ARGS=$EXTRACT(ARGS,$LENGTH(SUBARGS)+3,LEN)  
  . . . ;"NEW NIDX SET NIDX=$$NXTARGI(REFROOT,OFFSET)
  . . . ;"NEW TEMPREF SET TEMPREF=$NAME(@REFROOT@("ARGS",OFFSET,NIDX))
  . . . ;";"NEW TEMPREF SET TEMPREF=$NAME(@REFOUT@(COMMANUM,JDX,FNVTYPE,"ARGS"))
  . . . ;"IF SUBARGS'="" DO PARSARGS(SUBARGS,TEMPREF,REFROOT,OFFSET)
  . . . ;"ELSE  SET @TEMPREF@(1,1,"VAR","VALUE")="<NULL>"
  . . . ;"IF $DATA(@TEMPREF) SET @REFOUT@(COMMANUM,JDX,FNVTYPE,"ARGS")=NIDX    
  . IF TOKEN="," DO  QUIT
  . . SET TOKENARR(COMMANUM,TOKENCT,"TYPE")="COMMA"
  . . IF $DATA(@REFOUT@(COMMANUM))=0 SET @REFOUT@(COMMANUM)="<NULL>"
  . . SET COMMANUM=COMMANUM+1,JDX=0,TOKENCT=0
  . . IF ARGS="" SET @REFOUT@(COMMANUM)="<NULL>"  ;"handle terminal comma e.g. ("this","that",)
  . ELSE  IF "@_'!&=<>[]?+-*/\#"[TOKEN DO
  . . NEW LABEL 
  . . IF TOKEN="@" DO
  . . . SET LABEL="@"
  . . . SET @REFROOT@("IX","@",TMGLCMD,OFFSET)=""
  . . ELSE  IF ($GET(TMGLCMD)="WRITE")&(TOKEN="!") SET LABEL="LINEFEED"
  . . ELSE  IF ($GET(TMGLCMD)="WRITE")&(TOKEN="?") SET LABEL="TAB"
  . . ELSE  IF TOKEN="_" SET LABEL="CONCAT"
  . . ELSE  IF "'!&"[TOKEN SET LABEL="BOOL"
  . . ELSE  IF "=<>[]?"[TOKEN SET LABEL="COMP/ASSIGN"
  . . ELSE  IF "+-*/\#"[TOKEN SET LABEL="MATH"
  . . ELSE  SET LABEL="OTHER"  
  . . SET @REFOUT@(COMMANUM,JDX,LABEL,"VALUE")=TOKEN
  . . SET TOKENARR(COMMANUM,TOKENCT,"TYPE")=LABEL
  . ELSE  IF TOKEN=":" DO   ;"POST-CONDITIONAL or parameters
  . . SET TMGLFN=$GET(TMGLFN) ;"accessed in global scop
  . . NEW ISPOSTCOND SET ISPOSTCOND=((TMGLCMD'="")&("XECUTE,DO,GOTO"[TMGLCMD))
  . . NEW ISSELCTOPT SET ISSELCTOPT=(TMGLFN="$SELECT")          
  . . IF (ISPOSTCOND=1),(ISSELCTOPT=0) DO
  . . . SET SUBARGS=$PIECE(ARGS," ",1),ARGS=$PIECE(ARGS," ",2,9999)
  . . . IF $EXTRACT(SUBARGS,$LENGTH(SUBARGS))="^",$EXTRACT(ARGS,1)="(" DO  ;"handle outdated DSM "^[space](" syntax
  . . . . SET SUBARGS=SUBARGS_" "_$PIECE(ARGS," ",1)
  . . . . SET ARGS=$PIECE(ARGS," ",2,9999)  
  . . . NEW NIDX SET NIDX=$$NXTARGI(REFROOT,OFFSET)
  . . . NEW TEMPREF SET TEMPREF=$NAME(@REFROOT@("ARGS",OFFSET,NIDX))
  . . . DO PARSARGS(SUBARGS,TEMPREF,REFROOT,OFFSET)
  . . . IF $DATA(@TEMPREF) SET @REFOUT@(COMMANUM,JDX,"POST COND")=NIDX
  . . . SET TOKENARR(COMMANUM,TOKENCT,"TYPE")="POST COND"
  . . ELSE  DO
  . . . SET @REFOUT@(COMMANUM,JDX,":","VALUE")=TOKEN
  . . . SET TOKENARR(COMMANUM,TOKENCT,"TYPE")="PARAM"  
  . ELSE  IF TOKEN?.N DO   ;"NUMBER
  . . SET @REFOUT@(COMMANUM,JDX,"NUM","VALUE")=TOKEN
  . . SET TOKENARR(COMMANUM,TOKENCT,"TYPE")="NUM"  
  . ELSE  IF TOKEN="^" DO   ;"CARET
  . . SET @REFOUT@(COMMANUM,JDX,"CARET","VALUE")=TOKEN
  . . SET TOKENARR(COMMANUM,TOKENCT,"TYPE")="CARET"  
  . ELSE  IF TOKEN?0.1"%".ULN DO   ;"Alphanumeric string, optionally starting with %
  . . SET TOKENARR(COMMANUM,TOKENCT,"TYPE")="VAR"  ;"May be changed below
  . . NEW ISGLOBAL SET ISGLOBAL=$$ISGLOBAL(.TOKENARR,COMMANUM)
  . . IF ISGLOBAL!(PRIORTOKEN="^") DO 
  . . . SET @REFOUT@(COMMANUM,JDX,"GLOBAL","VALUE")=TOKEN
  . . . IF ISGLOBAL DO
  . . . . IF TMGLFN'="" SET @REFROOT@("IX","GLOBAL",TOKEN,TMGLFN)=""
  . . . . ELSE  SET @REFROOT@("IX","GLOBAL",TOKEN,TMGLCMD)=""
  . . . ELSE  IF PRIORTOKEN="^" DO
  . . . . NEW TAG SET TAG="[NONE]" IF TOKENCT>2 SET TAG=TOKENARR(COMMANUM,TOKENCT-2)
  . . . . IF (TMGLFN="")&((TMGLCMD="DO")!(TMGLCMD="GOTO")) DO
  . . . . . SET @REFROOT@("IX",TMGLCMD,TOKEN,TAG,OFFSET)=""
  . . . . ELSE  IF (TMGLFN="$TEXT") DO
  . . . . . SET @REFROOT@("IX","$TEXT",TOKEN,TAG,OFFSET)=""
  . . . . ELSE  DO
  . . . . . NEW TEMP SET TEMP=1 ;"REMOVE LATER
  . . ELSE  IF TMGLCMD="DO" DO 
  . . . NEW ROUTINE SET ROUTINE="[LOCAL]" 
  . . . IF $EXTRACT(ARGS,1)="^" DO
  . . . . SET ARGS=$EXTRACT(ARGS,2,LEN)
  . . . . SET ROUTINE=$$NEXTTOKN^TMGSTUT3(.ARGS)
  . . . SET TOKENARR(COMMANUM,TOKENCT,"TYPE")="PROC"  
  . . . SET @REFOUT@(COMMANUM,JDX,"PROC","VALUE")=TOKEN
  . . . SET @REFOUT@(COMMANUM,JDX,"PROC","ROUTINE")=ROUTINE
  . . . SET @REFROOT@("IX","DO",ROUTINE,TOKEN,OFFSET)=""
  . . . IF $EXTRACT(ARGS,1)="(" DO
  . . . . NEW TMGLCMD  ;"Hide TMGLCMD from sub parsing. "DO" isn't active in ()
  . . . . DO HNDFPARG(COMMANUM,JDX,"PROC",.ARGS,OFFSET,REFOUT,REFROOT) ;"HANDLE FUNCTION ($ OR $$) OR PROC ARGUMENTS
  . . ELSE  DO
  . . . SET @REFOUT@(COMMANUM,JDX,"VAR","VALUE")=TOKEN
  . ELSE  IF TOKEN?1";".N1";" DO   ;"  ;#;
  . . SET @REFOUT@(COMMANUM,JDX,"STRING","NUM")=+$PIECE(TOKEN,";",2)
  . . SET TOKENARR(COMMANUM,TOKENCT,"TYPE")="STRING"  
  . ELSE  DO
  . . SET @REFOUT@(COMMANUM,JDX,"OTHER","VALUE")=TOKEN
  . . SET TOKENARR(COMMANUM,TOKENCT,"TYPE")="OTHER"  
  . IF $EXTRACT(ARGS,1)="(" DO
  . . SET SUBARGS=$$MATCHXTR^TMGSTUT3(ARGS,"(",,,"(")
  . . SET ARGS=$EXTRACT(ARGS,$LENGTH(SUBARGS)+3,LEN)
  . . SET JDX=JDX+1
  . . ;"NEW TEMPREF SET TEMPREF=$NAME(@REFOUT@(COMMANUM,JDX,"VAR","PARENS"))
  . . NEW NIDX SET NIDX=$$NXTARGI(REFROOT,OFFSET)
  . . NEW TEMPREF SET TEMPREF=$NAME(@REFROOT@("ARGS",OFFSET,NIDX))
  . . DO PARSARGS(SUBARGS,TEMPREF,REFROOT,OFFSET)
  . . IF $DATA(@TEMPREF) SET @REFOUT@(COMMANUM,JDX,"VAR","PARENS")=NIDX  
  QUIT
  ;
HNDFPARG(COMMANUM,JDX,TYPE,ARGS,OFFSET,REFOUT,REFROOT) ;"HANDLE FUNCTION ($ OR $$) OR PROC ARGUMENTS
  NEW TEMPSAVARGS SET TEMPSAVARGS=ARGS 
  NEW SUBARGS SET SUBARGS=$$MATCHXTR^TMGSTUT3(ARGS,"(",,,"(")
  SET ARGS=$EXTRACT(ARGS,$LENGTH(SUBARGS)+3,$LENGTH(ARGS))  
  NEW NIDX SET NIDX=$$NXTARGI(REFROOT,OFFSET)
  NEW TEMPREF SET TEMPREF=$NAME(@REFROOT@("ARGS",OFFSET,NIDX))
  ;"NEW TEMPREF SET TEMPREF=$NAME(@REFOUT@(COMMANUM,JDX,FNVTYPE,"ARGS"))
  IF SUBARGS'="" DO PARSARGS(SUBARGS,TEMPREF,REFROOT,OFFSET)
  ELSE  SET @TEMPREF@(1,1,"VAR","VALUE")="<NULL>"
  IF $DATA(@TEMPREF) SET @REFOUT@(COMMANUM,JDX,TYPE,"ARGS")=NIDX    

  QUIT


ISGLOBAL(ARR,IDXA,MAX)  ;"DOES ARRAY SEQUENCE INDICATE GLOBAL REFERANCE?
  ;"Look for patterns:  ^TMP  <- global
  ;"                    +^TMP <-- global
  ;"                    TAG^TMP'  <-- NOT global
  ;"                  TAG+4^TMP'  <-- NOT global
  ;"Results: 1 if global reference, 0 otherwise. 
  NEW RESULT SET RESULT=0
  NEW MAX SET MAX=+$ORDER(ARR(IDXA,""),-1)
  IF MAX'>1 GOTO IGBDN
  NEW PRIORTOKEN SET PRIORTOKEN=$GET(ARR(IDXA,MAX-1)) 
  IF PRIORTOKEN'="^" GOTO IGBDN  
  NEW BEFORECARET SET BEFORECARET=$GET(ARR(IDXA,MAX-2,"TYPE"))
  IF (BEFORECARET="")!(BEFORECARET'="VAR") DO
  . SET RESULT=1
IGBDN ;  
  QUIT RESULT
  ;
NUMBEFOR(ARR,IDX) ;"FIND AN AVAILABLE (NON-EXISTANT) INDEX NUMBER BEFORE IDX IN ARR
  NEW IDX0 SET IDX0=+$ORDER(ARR(IDX),-1)
  SET IDX0=IDX0+((IDX-IDX0)/2)
  QUIT IDX0
  ;
NUMAFTER(ARR,IDX) ;"FIND AN AVAILABLE (NON-EXISTANT) INDEX NUMBER AFTER IDX IN ARR
  NEW IDX2 SET IDX0=$ORDER(ARR(IDX)) IF IDX2="" SET IDX2=1000
  SET IDX2=IDX+((IDX2-IDX)/2)
  QUIT IDX2
  ;
GETBLK(TAG,OFFSET,ROUTINE,OUT,MODE)  ;"GET BLOCK from source code, into array
  ;"Purpose: To take an arbitrary address position in code and pull lines 
  ;"         from source code into a block, until end of file or next TAG 
  ;"         encountered OR 25 blank lines encountered (trimmed from end).
  ;"Input : TAG -- Tag in routine to pull from, e.g. "EN"
  ;"        OFFSET -- E.g. "3"
  ;"        ROUTINE -- routine to pull from, e.g. "TMGTEST" (not '^TMGTEST')
  ;"          NOTE: In the above example code would pull starting from EN+3^TMGTEST
  ;"        OUT -- PASS BY REFERENCE.  AN OUT PARAMETER.  Format:
  ;"           OUT(#)=<line of code>   NOTE: #=OFFSET for first line
  ;"        MODE -- OPTIONAL.
  ;"             0 -- parse up to next tag.  Default
  ;"             1 -- parse up to next tag with paramegers (e.g. EN(X,Y,X) )
  ;"             2 -- parse up to next named tag.  Requires:
  ;"                  MODE("NAME")=<named tag>  <-- if not found, reverts to mode 3
  ;"             3 -- parse to end of file.
  ;"Result: none.  See OUT
  SET MODE=+$GET(MODE)
  NEW BLANKCT SET BLANKCT=0
  SET TAG=$GET(TAG)
  SET ROUTINE=$GET(ROUTINE) IF ROUTINE="" GOTO GBKDN
  SET OFFSET=+$GET(OFFSET)
  IF TAG="",OFFSET=0 SET OFFSET=1
  NEW DONE SET DONE=0
  NEW FIRST SET FIRST=1
  FOR  DO  QUIT:DONE=1
  . NEW POS SET POS=TAG
  . IF OFFSET>0 SET POS=POS_"+"_OFFSET
  . ;"SET POS=POS_"^"_MODULE
  . SET POS=POS_"^"_ROUTINE 
  . ;"NEW LINE SET LINE=$TEXT(@POS)
  . NEW LINE SET LINE=$$TEXT^TMGCOD00(TAG,OFFSET,ROUTINE)
  . IF LINE="" DO  QUIT:DONE
  . . SET BLANKCT=BLANKCT+1
  . . IF BLANKCT'<25 SET DONE=1
  . ELSE  IF $EXTRACT(LINE,1)'=" " DO  QUIT:DONE  ;"LABEL FOUND
  . . IF FIRST=1 SET FIRST=0 QUIT   
  . . SET BLANKCT=0
  . . NEW TEMP SET TEMP=LINE
  . . NEW LABEL SET LABEL=$$NEXTTOKN^TMGSTUT3(.TEMP)
  . . IF MODE=1 SET DONE=($EXTRACT(TEMP,1)="(") QUIT
  . . IF MODE=2 SET DONE=(LABEL=$GET(MODE("NAME"))) QUIT
  . . IF MODE=3 QUIT  ;"ignore labels and go to end of file.
  . . SET DONE=1 QUIT
  . SET OUT(OFFSET)=LINE
  . SET OFFSET=OFFSET+1
  . SET FIRST=0
  ;"trim trailling empty lines
  SET DONE=0
  FOR  DO  QUIT:DONE
  . NEW LASTCT SET LASTCT=$ORDER(OUT(""),-1)
  . IF LASTCT="" SET DONE=1 QUIT
  . NEW LINE SET LINE=$$TRIM^XLFSTR($GET(OUT(LASTCT)))
  . IF LINE'="" SET DONE=1 QUIT
  . KILL OUT(LASTCT)
GBKDN ;  
  QUIT  
  ;
TABLCNVT(LOOKUP,NODE,SHORT,LONG) ;
  IF $DATA(TMGCMDABVR)=0 DO SETUPTBL(.TMGCMDABVR)
  SET (SHORT,LONG)=LOOKUP
  NEW UPLKUP SET UPLKUP=$$UP^XLFSTR(LOOKUP)
  IF $DATA(TMGCMDABVR(NODE,"XPND",UPLKUP)) DO
  . SET SHORT=UPLKUP,LONG=$GET(TMGCMDABVR(NODE,"XPND",UPLKUP))
  ELSE  IF $DATA(TMGCMDABVR(NODE,"ABVR",UPLKUP)) DO
  . SET LONG=UPLKUP,SHORT=$GET(TMGCMDABVR(NODE,"ABVR",UPLKUP))
  QUIT
  ;
NXTARGI(REFROOT,OFFSET) ;"NEXT ARGS INDEX
  NEW NIDX SET NIDX=+$ORDER(@REFROOT@("ARGS",OFFSET,""),-1)+1
  IF NIDX=0 SET NIDX=1
  QUIT NIDX
  ;
SETUPTBL(TABLE)  ;"Set up command abbreviation <--> expansion table. 
  ;"Input: TABLE -- PASS BY REFERENCE.
  ;"Results: none
  SET TABLE("CMD","XPND","AB")="ABLOCK"
  SET TABLE("CMD","XPND","A")="ASSIGN"
  SET TABLE("CMD","XPND","ASTA")="ASTART"
  SET TABLE("CMD","XPND","ASTO")="ASTOP"
  SET TABLE("CMD","XPND","AUNB")="AUNBLOCK"
  SET TABLE("CMD","XPND","B")="BREAK"
  SET TABLE("CMD","XPND","C")="CLOSE"
  SET TABLE("CMD","XPND","D")="DO"
  SET TABLE("CMD","XPND","E")="ELSE"
  SET TABLE("CMD","XPND","ESTA")="ESTART"
  SET TABLE("CMD","XPND","ESTO")="ESTOP"
  SET TABLE("CMD","XPND","ETR")="ETRIGGER"
  SET TABLE("CMD","XPND","F")="FOR"
  SET TABLE("CMD","XPND","G")="GOTO"
  SET TABLE("CMD","XPND","I")="IF"
  SET TABLE("CMD","XPND","J")="JOB"
  SET TABLE("CMD","XPND","K")="KILL"
  SET TABLE("CMD","XPND","KS")="KSUBSCRIPTS"
  SET TABLE("CMD","XPND","KV")="KVALUE"
  SET TABLE("CMD","XPND","L")="LOCK"
  SET TABLE("CMD","XPND","M")="MERGE"
  SET TABLE("CMD","XPND","N")="NEW"
  SET TABLE("CMD","XPND","O")="OPEN"
  SET TABLE("CMD","XPND","Q")="QUIT"
  SET TABLE("CMD","XPND","R")="READ"
  SET TABLE("CMD","XPND","RL")="RLOAD"
  SET TABLE("CMD","XPND","RS")="RSAVE"
  SET TABLE("CMD","XPND","S")="SET"
  SET TABLE("CMD","XPND","TC")="TCOMMIT"
  SET TABLE("CMD","XPND","TH")="THEN"
  SET TABLE("CMD","XPND","TRE")="TRESTART"
  SET TABLE("CMD","XPND","TRO")="TROLLBACK"
  SET TABLE("CMD","XPND","TS")="TSTART"
  SET TABLE("CMD","XPND","U")="USE"
  SET TABLE("CMD","XPND","V")="VIEW"
  SET TABLE("CMD","XPND","W")="WRITE"
  SET TABLE("CMD","XPND","X")="XECUTE"
  SET TABLE("CMD","XPND","ZWR")="ZWRITE"
  NEW ABVR SET ABVR=""
  FOR  SET ABVR=$ORDER(TABLE("CMD","XPND",ABVR)) QUIT:ABVR=""  DO
  . NEW FULL SET FULL=$GET(TABLE("CMD","XPND",ABVR)) QUIT:FULL=""
  . SET TABLE("CMD","ABVR",FULL)=ABVR
  ;"=========================================  
  SET TABLE("FN","XPND","$A")="$ASCII"
  SET TABLE("FN","XPND","$C")="$CHAR"
  SET TABLE("FN","XPND","$D")="$DATA"
  SET TABLE("FN","XPND","$DE")="$DEXTRACT"
  SET TABLE("FN","XPND","$DP")="$DPIECE"
  SET TABLE("FN","XPND","$E")="$EXTRACT"
  SET TABLE("FN","XPND","$F")="$FIND"
  SET TABLE("FN","XPND","$FN")="$FNUMBER"
  SET TABLE("FN","XPND","$G")="$GET"
  SET TABLE("FN","XPND","$H")="$HOROLOG"
  SET TABLE("FN","XPND","$J")="$JUSTIFY"
  SET TABLE("FN","XPND","$L")="$LENGTH"
  SET TABLE("FN","XPND","$M")="$MUMPS"
  SET TABLE("FN","XPND","$NA")="$NAME"
  SET TABLE("FN","XPND","$N")="$NEXT"
  SET TABLE("FN","XPND","$O")="$ORDER"
  SET TABLE("FN","XPND","$P")="$PIECE"
  SET TABLE("FN","XPND","$QL")="$QLENGTH"
  SET TABLE("FN","XPND","$QS")="$QSUBSCRIPT"
  SET TABLE("FN","XPND","$Q")="$QUERY"  
  SET TABLE("FN","XPND","$R")="$RANDOM"
  SET TABLE("FN","XPND","$RE")="$REVERSE"
  SET TABLE("FN","XPND","$S")="$SELECT"
  SET TABLE("FN","XPND","$ST")="$STACK"
  SET TABLE("FN","XPND","$T")="$TEXT"
  SET TABLE("FN","XPND","$TR")="$TRANSLATE"
  SET TABLE("FN","XPND","$V")="$VIEW"
  SET ABVR=""
  FOR  SET ABVR=$ORDER(TABLE("FN","XPND",ABVR)) QUIT:ABVR=""  DO
  . NEW FULL SET FULL=$GET(TABLE("FN","XPND",ABVR)) QUIT:FULL=""
  . SET TABLE("FN","ABVR",FULL)=ABVR
  ;"=========================================  
  SET TABLE("SV","XPND","$D")="$DEVICE"
  SET TABLE("SV","XPND","$E")="$ECODE"
  SET TABLE("SV","XPND","$ER")="$EREF"
  SET TABLE("SV","XPND","$ES")="$ESTACK"
  SET TABLE("SV","XPND","$ET")="$ETRAP"
  SET TABLE("SV","XPND","$H")="$HOROLOG"
  SET TABLE("SV","XPND","$I")="$IO"
  SET TABLE("SV","XPND","$IOR")="$IOREFERENCE"
  SET TABLE("SV","XPND","$J")="$JOB"
  SET TABLE("SV","XPND","$K")="$KEY"
  SET TABLE("SV","XPND","$PD")="$PDISPLAY"
  SET TABLE("SV","XPND","$PIOR")="$PIOREFERENCE"
  SET TABLE("SV","XPND","$P")="$PRINCIPLE"
  SET TABLE("SV","XPND","$Q")="$QUIT"
  SET TABLE("SV","XPND","$R")="$REFERENCE"
  SET TABLE("SV","XPND","$ST")="$STACK"
  SET TABLE("SV","XPND","$S")="$STORAGE"
  SET TABLE("SV","XPND","$SY")="$SYSTEM"
  SET TABLE("SV","XPND","$T")="$TEST"
  SET TABLE("SV","XPND","$TL")="$TLEVEL"  
  SET TABLE("SV","XPND","$TR")="$TRESTART"
  SET TABLE("SV","XPND","$X")="$X"
  SET TABLE("SV","XPND","$Y")="$Y"
  SET ABVR=""
  FOR  SET ABVR=$ORDER(TABLE("SV","XPND",ABVR)) QUIT:ABVR=""  DO
  . NEW FULL SET FULL=$GET(TABLE("SV","XPND",ABVR)) QUIT:FULL=""
  . SET TABLE("SV","ABVR",FULL)=ABVR  
  QUIT
  ;