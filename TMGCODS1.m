TMGCODS1 ;TMG/kst-Code Syntax parsing  ;1/19/25
         ;;1.0;TMG-LIB;**1**;1/19/25
 ;
 ;"TMG CODE PARSING and SYNTAX TREE GENERATION
 ;
 ;"NOTE:  I have copied this from TMGCOD01 because I want to change 
 ;"       functionality, and I don't want to break the system already working
 ;"       that supports checking routines for change and for web page showing
 ;"       links into and out of a routine.  
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 1/19/25  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"PARSEPOS(TAG,OFFSET,ROUTINE,REFOUT,MODE)  --PARSE CODE STARTING AT POS
 ;"PARSBLK(TMGBLK,OFFSET,REFOUT,XRREF,SCOPE)  -- take an array with code and parse it into data structure
 ;"GETBLK(TAG,OFFSET,ROUTINE,OUT,MODE)  -- GET BLOCK from source code, into array
 ; 
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;"PARSLINE(TMGBLK,OFFSET,REFOUT)  -- PARSE A LINE, including any DO block associated
 ;"PARSARGS(CMD,ARGS,REFOUT,XRREF,OFFSET,SCOPE) -- PARSE A COMMAND'S ARGUMENTS
 ;"NUMBEFOR(ARR,IDX) --FIND AN AVAILABLE (NON-EXISTANT) INDEX NUMBER BEFORE IDX IN ARR
 ;"NUMAFTER(ARR,IDX) -- FIND AN AVAILABLE (NON-EXISTANT) INDEX NUMBER AFTER IDX IN ARR
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"  TMGSTUT3, XLFSTR
 ;"=======================================================================
 ;
 ;
PARSEPOS(TAG,ROUTINE,REFOUT,MODE,SCOPE)  ;"PARSE CODE STARTING AT POS
  ;"Purpose: To take an arbitrary address position in code and parse one 
  ;"         block into data structure
  ;"Input : TAG -- Tag in routine to parse from, e.g. "EN"
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
  ;"           MODE("LINE#")=1  DEFAULT.  If found, then index#'s in @OUT will be same as line numbers in file.  
  ;"        SCOPE -- PASS BY REFERENCE. OPTIONAL.  Format:
  ;"            SCOPE(<VARNAME>)=""  variable name known to be in scope.  
  ;"Results : none.  See REFOUT above.
  NEW TMGBLK
  NEW TMGCMDABVR  ;"Will be used by sub routines in global scope
  KILL ^TMP($J,"TMGCOD00") ;"remove any cached routine text.
  SET MODE("LINE#")=+$GET(MODE("LINE#"),1)
  NEW XRREF SET XRREF=$GET(XRREF,$NAME(@REFOUT@("IX")))
  DO GETBLK^TMGCOD01(.TAG,0,ROUTINE,.TMGBLK,.MODE) 
  DO PARSBLK(.TMGBLK,"",REFOUT,XRREF,.SCOPE)
  QUIT
  ;
PARSBLK(TMGBLK,OFFSET,REFOUT,XRREF,SCOPE)  ;"PARSE BLOCK
  ;"Purpose: To take an array with code and parse it into data structure
  ;"Input : TMGBLK -- PASS BY REFERENCE.  Array with code lines.
  ;"        OFFSET -- used for recursive calls.  Leave blank/null when first calling
  ;"            E.g. "3"
  ;"        REFOUT -- PASS BY NAME.  AN OUT PARAMETER.  See PARSLINE for details.
  ;"        XRREF-- This the reference for cross references, typically @ROOT@("IX")
  ;"        SCOPE -- PASS BY NAME.  Format:
  ;"            SCOPE(<VARNAME>)=""  variable name known to be in scope.  
  ;"Results : none.  See OUT above.
  ;"NOTE: uses TMGCMDABVR in global scope.  If variable is empty, then will be filled
  NEW FIRST SET FIRST=1
  SET OFFSET=$GET(OFFSET)
  SET XRREF=$GET(XRREF,$NAME(@REFOUT@("IX")))
  FOR  SET OFFSET=$ORDER(TMGBLK(OFFSET)) QUIT:+OFFSET'=OFFSET  DO
  . DO PARSLINE(.TMGBLK,.OFFSET,REFOUT,XRREF,.SCOPE)
  QUIT
  ;
PARSLINE(TMGBLK,OFFSET,REFOUT,XRREF,SCOPE)  ;"PARSE A LINE, including any DO block associated
  ;"INPUT: TMGBLK -- PASS BY REFERENCE.  Array of text containing source lines
  ;"       OFFSET -- number index in TMGBLK array. PASS BY REFERENCE.
  ;"       REFOUT -- pass by NAME.  Output array.  See format in PARSBLK
  ;"          @REFOUT@(OFFSET,"TAG","ARGS")=ArgsPointer#, pointer to @ROOT@("ARGS",OFFSET) See below                         
  ;"          @REFOUT@(OFFSET,"COMMENT")=<comment>                         
  ;"          ;"@REFOUT@(OFFSET,"STRINGS",#)=strings
  ;"          @REFOUT@(OFFSET,"INDENT")=<text that indents from left margin>
  ;"          @REFOUT@(OFFSET,"TRAILING")=<trailing spaces>
  ;"          @REFOUT@(OFFSET,<cmd #>,"CMD")=<CMD> as found in code
  ;"          @REFOUT@(OFFSET,<cmd #>,"CMD","ABVR")=<CMD> shortened form
  ;"          @REFOUT@(OFFSET,<cmd #>,"CMD","XPND")=<CMD> expanded form
  ;"          @REFOUT@(OFFSET,<cmd #>,"CMD","PRECEEDING")=<extra space before command, if any>
  ;"          @REFOUT@((OFFSET,<cmd #>,"CMD","ARGS")=<args block> See format in PARSARGS()  //kt
  ;"          @REFOUT@(OFFSET,<cmd #>,"POST COND")=ArgsPointer#, pointer to @ROOT@("ARGS",OFFSET) See below
  ;"    ------Below is *ROOT, not *OUT-------
  ;"          ;"@XRREF@("CMD",LONG_CMD,OFFSET)=""
  ;"          @XRREF@("$$FN",<ROUTINE>,<FNNAME>,OFFSET)=""
  ;"          @XRREF@("DO",<ROUTINE>,<TAG>,OFFSET)=""
  ;"          @XRREF@("GOTO",<ROUTINE>,<TAG>,OFFSET)=""
  ;"          @XRREF@("$TEXT",<ROUTINE>,<TAG>,OFFSET)=""
  ;"       XRREF-- This the reference for cross references, typically @ROOT@("IX")
  ;"       SCOPE -- PASS BY NAME.  Format:
  ;"          SCOPE(<VARNAME>)=""  variable name known to be in scope.  
  ;"
  ;"NOTE: uses TMGCMDABVR in global scope.  If variable is empty, then will be filled here
  ;"Results: none
  NEW LINE SET LINE=$GET(TMGBLK(OFFSET))  ;"should never contain LABEL / TAG
  SET XRREF=$GET(XRREF,$NAME(@REFOUT@("IX")))
  SET @REFOUT@(OFFSET)="CODE POS OFFSET"  ;"kt
  SET @REFOUT@(OFFSET,0)="ORIG: "_LINE
  NEW INITLINE SET INITLINE=LINE
  NEW TOKEN,ARG,CH,STRINGS
  ;
  SET @REFOUT@(OFFSET,"COMMENT")=$$CUTCOMMENTS(.LINE)  ;"Extract any remove any comments first...
  SET @REFOUT@(OFFSET,"TRAILING")=$$CUTTRAILING(.LINE) ;"Extract any trailing characters (tabs or spaces)
  DO XTRACTSTRS(.LINE,.STRINGS)                        ;"replace any strings with `<#>`, e.g. `1` 
  ;
  ;"Remove and store TAG / label 
  IF ($C(9)_" ")'[$EXTRACT(LINE,1) DO
  . NEW XFLINE SET XFLINE=$TRANSLATE(LINE,$CHAR(9)," ")
  . NEW TEMP SET TEMP=$PIECE(XFLINE," ",1)
  . SET LINE=$EXTRACT(LINE,$LENGTH(TEMP)+1,$LENGTH(LINE))
  . SET LABEL=$PIECE(TEMP,"(",1)
  . NEW ARGS SET ARGS=$EXTRACT(TEMP,$LENGTH(LABEL)+1,$LENGTH(TEMP))
  . IF ARGS'="()" SET ARGS=$PIECE($PIECE(ARGS,"(",2,999),")",1)
  . SET @REFOUT@(OFFSET,"TAG")=LABEL
  . IF ARGS'="" DO
  . . NEW TEMPREF SET TEMPREF=$NAME(@REFOUT@(OFFSET,"TAG","ARGS"))
  . . IF ARGS="()" SET @TEMPREF="()"
  . . ELSE  DO PARSARGS("",ARGS,TEMPREF,XRREF,OFFSET,.SCOPE,.STRINGS)  ;"No CMD passed because this is TAG with parameters
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
  . . NEW TEMPREF SET TEMPREF=$NAME(@REFOUT@(OFFSETSAVE,CMDCT,"CMD","POST COND"))  ;"//kt
  . . DO PARSARGS(CMD,PCEXPR,TEMPREF,XRREF,OFFSETSAVE,.SCOPE,.STRINGS)  ;"FIX!!  I think this should be parseline, not parseargs...
  . NEW SCMD,TMGLCMD,UCMD SET UCMD=$$UP^XLFSTR(CMD)
  . DO TABLCNVT^TMGCOD01(CMD,"CMD",.SCMD,.TMGLCMD)  ;"May populate global scope var TMGCMDABVR
  . SET @REFOUT@(OFFSETSAVE)="CODE POS OFFSET"  ;"//kt
  . SET @REFOUT@(OFFSETSAVE,CMDCT)="CMD #"  ;"//kt
  . SET @REFOUT@(OFFSETSAVE,CMDCT,"CMD")=CMD
  . SET @REFOUT@(OFFSETSAVE,CMDCT,"CMD","ABVR")=SCMD
  . SET @REFOUT@(OFFSETSAVE,CMDCT,"CMD","XPND")=TMGLCMD
  . ;"SET @XRREF@("CMD",TMGLCMD,OFFSETSAVE)=""
  . NEW ARGS SET ARGS=$PIECE(LINE," ",1)
  . SET LINE=$PIECE(LINE," ",2,9999)  
  . IF $EXTRACT(ARGS,$LENGTH(ARGS))="^",$EXTRACT(LINE,1)="(" DO  ;"handle outdated DSM "^[space](" syntax
  . . SET ARGS=ARGS_" "_$PIECE(LINE," ",1)
  . . SET LINE=$PIECE(LINE," ",2,9999)
  . NEW TEMPREF SET TEMPREF=$NAME(@REFOUT@(OFFSETSAVE,CMDCT,"CMD","ARGS"))  ;"//kt
  . DO PARSARGS(TMGLCMD,ARGS,TEMPREF,XRREF,OFFSETSAVE,.SCOPE,.STRINGS)
  . IF (TMGLCMD="DO"),$$TRIM^XLFSTR(ARGS)="" DO
  . . NEW SUBBLOCK DO GETSUBLK^TMGCOD01(INITLINE,.TMGBLK,.OFFSET,.SUBBLOCK)
  . . ;"NEW TMGSUBBLKREF SET TMGSUBBLKREF=$NAME(@REFROOT@(".",OFFSETSAVE))
  . . NEW TMGSUBBLKREF SET TMGSUBBLKREF=$NAME(@REFOUT@(OFFSETSAVE,CMDCT,"CMD"))  ;"//kt  removing "." to save indices
  . . NEW SUBSCOPE MERGE SUBSCOPE=SCOPE  ;"This is so changes to SUBSCOPE won't be included in SCOPE
  . . DO PARSBLK(.SUBBLOCK,"",TMGSUBBLKREF,XRREF,.SUBSCOPE)  ;"PARSE BLOCK
  . . ;"SET @REFOUT@(OFFSETSAVE,CMDCT,".")="<DOT_BLOCK>"
  . SET CMDCT=CMDCT+1
  ;
  IF $DATA(@REFOUT@(OFFSET,1))=0,($GET(@REFOUT@(OFFSET,"INDENT"))=""),($GET(@REFOUT@(OFFSET,"TRAILING"))'="") DO
  . IF LINE="" QUIT
  . ;"If not indent, but has trailing, and no command, then use trailing as indent. 
  . SET @REFOUT@(OFFSET,"INDENT")=$GET(@REFOUT@(OFFSET,"TRAILING"))
  . SET @REFOUT@(OFFSET,"TRAILING")=""
PLNDN ;
  QUIT
  ;
CUTCOMMENTS(LINE) ;
  NEW POS SET POS=1
  NEW RESULT SET RESULT=""
  NEW DONE SET DONE=0
  FOR  SET POS=$FIND(LINE,";",POS) QUIT:(POS'>0)!DONE  DO
  . IF $$INQT^TMGSTUT3(LINE,POS-1) QUIT
  . SET RESULT=$EXTRACT(LINE,POS-1,999)
  . SET LINE=$EXTRACT(LINE,1,POS-2)
  . SET DONE=1
  QUIT RESULT
  ;"
CUTTRAILING(LINE)  ;  
  ;"Extract trailing characters (tabs or spaces)
  NEW RESULT SET RESULT=""
  NEW CH,DONE SET DONE=0,CH=""
  NEW TRAILING SET TRAILING=""
  NEW IDX FOR IDX=$LENGTH(LINE):-1:1 DO  QUIT:DONE
  . SET CH=$EXTRACT(LINE,IDX)
  . IF (CH'=$CHAR(9))&(CH'=" ") SET DONE=1 QUIT
  . SET TRAILING=CH_TRAILING
  SET RESULT=TRAILING
  SET LINE=$EXTRACT(LINE,1,$LENGTH(LINE)-$LENGTH(TRAILING))
  QUIT RESULT
  ;  
XTRACTSTRS(LINE,.STRINGS)  ;
  ;"Temporarily remove strings which can confuse parsing, replacing with `<#>`, e.g. `2` 
  NEW QTPOS,QTCOUNT SET QTPOS=0,QTCOUNT=1
  FOR  DO  QUIT:QTPOS'>0  
  . SET QTPOS=$FIND(LINE,"""")-1
  . IF QTPOS'>0 QUIT
  . NEW P2 SET P2=$$ENDQTPOS^TMGSTUT3(LINE,QTPOS)
  . IF P2'>0 SET QTPOS=0 QUIT
  . NEW PARTA,STR,PARTB
  . SET PARTA=$EXTRACT(LINE,1,QTPOS-1)
  . SET STR=$EXTRACT(LINE,QTPOS,P2)
  . SET PARTB=$EXTRACT(LINE,P2+1,$LENGTH(LINE))
  . ;"SET @REFOUT@(OFFSET,"STRINGS",QTCOUNT)=STR 
  . SET STRINGS(QTCOUNT)=STR
  . SET LINE=PARTA_"`"_QTCOUNT_"`"_PARTB  
  . SET QTCOUNT=QTCOUNT+1
  QUIT
  ;
PARSEXPR(CMD,ARGS,REFOUT,XRREF,OFFSET,SCOPE,STRINGS) ;"PARSE AN EXPRESSION
  ;"TO DO...
  QUIT
  ;
PARSARGS(CMD,ARGS,REFOUT,XRREF,OFFSET,SCOPE,STRINGS) ;"PARSE A COMMAND'S ARGUMENTS
  ;"INPUT: CMD -- active command, if MUMPS command.  "" otherwise
  ;"       Args -- string containing code with arguments
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
  ;"       XRREF-- This the reference for cross references, typically @ROOT@("IX")
  ;"       OFFSET --
  ;"       SCOPE -- PASS BY NAME.  Format:
  ;"          SCOPE(<VARNAME>)=""  variable name known to be in scope.  
  ;"       STRINGS -- PASS BY REFERENCE.  Array holding strings.  
  ;"NOTE: May use TMGCMDABVR in global scope.  If variable is empty, then will be filled here
  ;"      May use TMGLFN in global scope.  Optional, no problem if missing. 
  ;"Results: none
  ;"Note: ARGS should not contain any strings at this point.
  SET @REFOUT@(0)="ORIG: "_$GET(ARGS)
  NEW PARTA,PARTB,SUBARGS,LEN
  ;"SET TMGLCMD=$GET(TMGLCMD) ;"accessed in global scope
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
  . SET @REFOUT@(COMMANUM)="COMMA #"  ;"//kt
  . SET @REFOUT@(COMMANUM,JDX)="SEQ #"  ;"//kt
  . SET PRIORTOKEN=TOKEN
  . SET TOKEN=$$NEXTTOKN^TMGSTUT3(.ARGS)
  . IF TOKEN="" DO
  . . NEW DONE SET DONE=0
  . . NEW FIRSTCH SET FIRSTCH=$EXTRACT(ARGS,1)
  . . IF FIRSTCH="`" DO    ;" string marker
  . . . NEW POS SET POS=$FIND(ARGS,"`",2) IF POS'>0 QUIT
  . . . SET TOKEN=$EXTRACT(ARGS,1,POS-1)
  . . . ;"SET ARGS=$EXTRACT(ARGS,$LENGTH(TOKEN)+1,LEN)
  . . ELSE  IF "^!&'*,-/\<>?@[]_#=: "[FIRSTCH DO
  . . . SET TOKEN=FIRSTCH
  . . ELSE  NEW CH,POS FOR POS=1:1:LEN DO  QUIT:DONE  ;"gather next non alphanumeric chars
  . . . SET CH=$EXTRACT(ARGS,POS)
  . . . SET DONE=(CH?1ULN)!(CH="`")!(CH="$") QUIT:DONE
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
  . . . IF $DATA(TMGCMDABVR)=0 DO SETUPTBL^TMGCOD01(.TMGCMDABVR)
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
  . . . SET @XRREF@("$$FN",TEMPRTN,$PIECE(FNVNAME,"$$",2),OFFSET)=""
  . . SET TOKENARR(COMMANUM,TOKENCT,"TYPE")=FNVTYPE
  . . IF $EXTRACT(ARGS,1)="(" DO
  . . . NEW TEMPREF SET TEMPREF=$NAME(@REFOUT@(COMMANUM,JDX,FNVTYPE))
  . . . DO HNDFPARG(CMD,COMMANUM,JDX,FNVTYPE,.ARGS,OFFSET,TEMPREF,XRREF,.SCOPE) ;"HANDLE FUNCTION ($ OR $$) OR PROC ARGUMENTS
  . IF TOKEN="," DO  QUIT
  . . SET TOKENARR(COMMANUM,TOKENCT,"TYPE")="COMMA"
  . . IF $DATA(@REFOUT@(COMMANUM))=0 SET @REFOUT@(COMMANUM)="<NULL>"
  . . IF $DATA(@REFOUT@(COMMANUM,JDX))\10=0 KILL @REFOUT@(COMMANUM,JDX)  ;"delete @REFOUT@(COMMANUM,JDX)="SEQ #" with no children
  . . SET COMMANUM=COMMANUM+1,JDX=0,TOKENCT=0
  . . IF ARGS="" SET @REFOUT@(COMMANUM)="<NULL>"  ;"handle terminal comma e.g. ("this","that",)
  . ELSE  IF "@_'!&=<>[]?+-*/\#"[TOKEN DO
  . . NEW LABEL 
  . . IF TOKEN="@" DO
  . . . SET LABEL="@"
  . . . SET @XRREF@("@",CMD,OFFSET)=""
  . . ELSE  IF ($GET(CMD)="WRITE")&(TOKEN="!") SET LABEL="LINEFEED"
  . . ELSE  IF ($GET(CMD)="WRITE")&(TOKEN="?") SET LABEL="TAB"
  . . ELSE  IF TOKEN="_" SET LABEL="CONCAT"
  . . ELSE  IF "'!&"[TOKEN SET LABEL="BOOL"
  . . ELSE  IF "=<>[]?"[TOKEN SET LABEL="COMP/ASSIGN"
  . . ELSE  IF "+-*/\#"[TOKEN SET LABEL="MATH"
  . . ELSE  SET LABEL="OTHER"  
  . . SET @REFOUT@(COMMANUM,JDX,LABEL,"VALUE")=TOKEN
  . . SET TOKENARR(COMMANUM,TOKENCT,"TYPE")=LABEL
  . ELSE  IF TOKEN=":" DO   ;"POST-CONDITIONAL or parameters
  . . SET TMGLFN=$GET(TMGLFN) ;"accessed in global scop
  . . NEW ISPOSTCOND SET ISPOSTCOND=((CMD'="")&("XECUTE,DO,GOTO"[CMD))
  . . NEW ISSELCTOPT SET ISSELCTOPT=(TMGLFN="$SELECT")          
  . . IF (ISPOSTCOND=1),(ISSELCTOPT=0) DO
  . . . SET SUBARGS=$PIECE(ARGS," ",1),ARGS=$PIECE(ARGS," ",2,9999)
  . . . IF $EXTRACT(SUBARGS,$LENGTH(SUBARGS))="^",$EXTRACT(ARGS,1)="(" DO  ;"handle outdated DSM "^[space](" syntax
  . . . . SET SUBARGS=SUBARGS_" "_$PIECE(ARGS," ",1)
  . . . . SET ARGS=$PIECE(ARGS," ",2,9999)  
  . . . NEW TEMPREF SET TEMPREF=$NAME(@REFOUT@("ARGS"))  ;"//kt
  . . . DO PARSARGS(CMD,SUBARGS,TEMPREF,XRREF,OFFSET,.SCOPE,.STRINGS)  ;"FIX!! <--- SHOULD THIS BE PARSE LINE??
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
  . . NEW ANAME SET ANAME=TOKEN
  . . SET TOKENARR(COMMANUM,TOKENCT,"TYPE")="VAR"  ;"May be changed below
  . . NEW ISGLOBAL SET ISGLOBAL=$$ISGLOBAL(.TOKENARR,COMMANUM)
  . . IF ISGLOBAL!(PRIORTOKEN="^") DO 
  . . . SET @REFOUT@(COMMANUM,JDX,"GLOBAL","NAME")=ANAME
  . . . IF ISGLOBAL DO
  . . . . IF TMGLFN'="" SET @XRREF@("GLOBAL",ANAME,TMGLFN)=""
  . . . . ELSE  SET @XRREF@("GLOBAL",ANAME,CMD)=""
  . . . ELSE  IF PRIORTOKEN="^" DO
  . . . . NEW TAG SET TAG="[NONE]" IF TOKENCT>2 SET TAG=TOKENARR(COMMANUM,TOKENCT-2)
  . . . . IF (TMGLFN="")&((CMD="DO")!(CMD="GOTO")) DO
  . . . . . SET @XRREF@(CMD,ANAME,TAG,OFFSET)=""
  . . . . ELSE  IF (TMGLFN="$TEXT") DO
  . . . . . SET @XRREF@("$TEXT",ANAME,TAG,OFFSET)=""
  . . . . ELSE  DO
  . . . . . NEW TEMP SET TEMP=1 ;"REMOVE LATER
  . . ELSE  IF CMD="DO" DO 
  . . . NEW ROUTINE SET ROUTINE="[LOCAL]"  ;"default
  . . . IF $EXTRACT(ARGS,1)="^" DO
  . . . . SET ARGS=$EXTRACT(ARGS,2,LEN)
  . . . . SET ROUTINE=$$NEXTTOKN^TMGSTUT3(.ARGS)
  . . . SET TOKENARR(COMMANUM,TOKENCT,"TYPE")="PROC"  
  . . . SET @REFOUT@(COMMANUM,JDX,"PROC","VALUE")=ANAME
  . . . SET @REFOUT@(COMMANUM,JDX,"PROC","ROUTINE")=ROUTINE
  . . . SET @XRREF@("DO",ROUTINE,ANAME,OFFSET)=""
  . . . IF $EXTRACT(ARGS,1)="(" DO
  . . . . DO HNDFPARG("",COMMANUM,JDX,"PROC",.ARGS,OFFSET,REFOUT,XRREF,.SCOPE) ;"HANDLE FUNCTION ($ OR $$) OR PROC ARGUMENTS
  . . ELSE  IF CMD="GOTO" DO 
  . . . NEW ROUTINE SET ROUTINE="[LOCAL]"   ;"default
  . . . IF $EXTRACT(ARGS,1)="^" DO
  . . . . SET ARGS=$EXTRACT(ARGS,2,LEN)
  . . . . SET ROUTINE=$$NEXTTOKN^TMGSTUT3(.ARGS)
  . . . SET TOKENARR(COMMANUM,TOKENCT,"TYPE")="GOTO"  
  . . . SET @REFOUT@(COMMANUM,JDX,"GOTO","VALUE")=ANAME
  . . . SET @REFOUT@(COMMANUM,JDX,"GOTO","ROUTINE")=ROUTINE
  . . . SET @XRREF@("GOTO",ROUTINE,ANAME,OFFSET)=""
  . . ELSE  DO
  . . . SET @REFOUT@(COMMANUM,JDX,"VAR","NAME")=ANAME
  . . . SET @XRREF@("VAR",ANAME,OFFSET)=""
  . . . IF CMD="NEW" DO
  . . . . SET SCOPE(ANAME)=""  ;"Add variable name to scope table.  
  . . . IF $DATA(SCOPE(ANAME))=0 DO  ;"variable accessed in global scope
  . . . . SET @REFOUT@(COMMANUM,JDX,"VAR","BY GLOBAL SCOPE")=1
  . . . . SET @XRREF@("VAR","BY GLOBAL SCOPE",ANAME,OFFSET)=""
  . ELSE  IF TOKEN?1"`".N1"`" DO   ;"  `#`
  . . NEW STRNUM SET STRNUM=+$PIECE(TOKEN,"`",2)
  . . NEW STR SET STR=$GET(STRINGS(STRNUM))
  . . ;"SET @REFOUT@(COMMANUM,JDX,"STRING","NUM")=STRNUM
  . . SET @REFOUT@(COMMANUM,JDX,"STRING","VALUE")=STR
  . . SET TOKENARR(COMMANUM,TOKENCT)=STR  
  . . SET TOKENARR(COMMANUM,TOKENCT,"TYPE")="STRING"  
  . ELSE  DO
  . . SET @REFOUT@(COMMANUM,JDX,"OTHER","VALUE")=TOKEN
  . . SET TOKENARR(COMMANUM,TOKENCT,"TYPE")="OTHER"  
  . IF $EXTRACT(ARGS,1)="(" DO
  . . SET SUBARGS=$$MATCHXTR^TMGSTUT3(ARGS,"(",,,"(")
  . . SET ARGS=$EXTRACT(ARGS,$LENGTH(SUBARGS)+3,LEN)
  . . SET JDX=JDX+1
  . . NEW TEMPREF SET TEMPREF=$NAME(@REFOUT@("ARGS"))  ;"//kt
  . . DO PARSARGS(CMD,SUBARGS,TEMPREF,XRREF,OFFSET,.SCOPE,.STRINGS)
  QUIT
  ;
HNDFPARG(CMD,COMMANUM,JDX,TYPE,ARGS,OFFSET,REFOUT,XRREF,SCOPE) ;"HANDLE FUNCTION ($ OR $$) OR PROC ARGUMENTS
  NEW TEMPSAVARGS SET TEMPSAVARGS=ARGS 
  NEW SUBARGS SET SUBARGS=$$MATCHXTR^TMGSTUT3(ARGS,"(",,,"(")
  SET ARGS=$EXTRACT(ARGS,$LENGTH(SUBARGS)+3,$LENGTH(ARGS))  
  NEW TEMPREF SET TEMPREF=$NAME(@REFOUT@("ARGS")) ;"//kt
  IF SUBARGS'="" DO 
  . DO PARSARGS(CMD,SUBARGS,TEMPREF,XRREF,OFFSET,.SCOPE)  ;"no CMD passed because this is a function, not a command
  ELSE  SET @TEMPREF@(1,1,"VAR","VALUE")="<NULL>"
  QUIT
  ;
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
NXTARGI(REFROOT,OFFSET) ;"NEXT ARGS INDEX
  NEW NIDX SET NIDX=+$ORDER(@REFROOT@("ARGS",OFFSET,""),-1)+1
  IF NIDX=0 SET NIDX=1
  QUIT NIDX
  ;
COMMANDS ;     
  ;;"B[REAK][:tvexpr] [expr[:tvexpr][,...]]
  ;;"C[LOSE][:tvexpr] expr[:(keyword[=expr][:...])][,...]
  ;;"D[O][:tvexpr] [entryref[(expr|.lvn[,...])][:tvexpr][,...]]
  ;;"E[LSE]
  ;;"F[OR][lvn=expr[:numexpr1[:numexpr2]][,...]]]
  ;;"G[OTO][:tvexpr] entryref[:tvexpr][,...]
  ;;"H[ALT][:tvexpr]
  ;;"H[ANG][:tvexpr] numexpr[,...]
  ;;"I[F] [tvexpr[,...]]
  ;;"J[OB][:tvexpr] entryref[(expr[,...])][:[(keyword[=value][:...])][:numexpr]][,...]
  ;;"K[ILL][:tvexpr] [glvn | (lvn[,...]) | *lname | *lvn ]
  ;;"L[OCK][:tvexpr] [[-|+]nref|(nref[,...])[:numexpr] [,...]]
  ;;"M[ERGE][:tvexpr] glvn1=glvn2[,...]
  ;;"N[EW][:tvexpr] [[(]lvn[,...][)][,...]]
  ;;"O[PEN][:tvexpr] expr[:[(keyword[=expr][:...])] [:numexpr]][,...]
  ;;"Q[UIT][:tvexpr] [expr | *lname | *lvn]
  ;;"R[EAD][:tvexpr] (glvn|*glvn|glvn#intexpr)[:numexpr]|strlit|fcc[,...]
  ;;"S[ET][:tvexpr] setleft=expr | (setleft[,...])=expr | *lvn=lname | aliascontainer[,...]
  ;;" where  setleft == glvn | $EXTRACT(glvn,[,intexpr1[,intexpr2]]) | $PIECE(glvn,expr1[,intexpr1[,intexpr2]]) | isv 
  ;;" and aliascontainer == lvn | exfunc | exvar 
  ;;"TC[OMMIT][:tvexpr]
  ;;"TRE[START][:tvexpr]
  ;;"TRO[LLBACK][:tvexpr] [intexpr]
  ;;"TS[TART][:tvexpr] [([lvn...])|lvn|*|][:keyword|(keyword...)]
  ;;"U[SE][:tvexpr] expr[:(keyword[=expr][:...])][,...]
  ;;"V[IEW][:tvexpr] keyword[:expr2[:...]][,...]
  ;;"W[RITE][:tvexpr] expr|*intexpr|fcc[,...]
  ;;"X[ECUTE]:tvexpr expr[:tvexpr][,...]
  ;;"ZA[LLOCATE][:tvexpr] [(]nref[,...][)][:intexpr][,...]
  ;;"ZB[REAK][:tvexpr] [-]entryref[:[expr][:intexpr]][,...]
  ;;"ZCOM[PILE][:tvexpr] expr[,...]
  ;;"ZC[ONTINUE][:tvexpr]
  ;;"ZD[EALLOCATE][:tvexpr] [nref[,...]]
  ;;"ZEDIT "./file"
  ;;"ZG[OTO][:tvexpr] [[intexpr][:entryref[:tvexpr]],...]
  ;;"ZHALT[:tvexpr] [intexpr]
  ;;"ZH[ELP][:tvexpr] [expr1[:expr2],...]
  ;;"ZK[ILL][:tvexpr] glvn
  ;;"ZL[INK][:tvexpr] [expr1[:expr2][,...]]
  ;;"ZM[ESSAGE][:tvexpr] intexpr[:expr2][:...]
  ;;"ZP[RINT][:tvexpr] [entryref[:label[+intexpr]][,...]
  ;;"ZRUP[DATE][:tvexpr] expr [,...]
  ;;"ZSH[OW][:tvexpr] [expr[:glvn][,...]]
  ;;"ZST[EP][:tvexpr] [keyword[:expr]][,...]
  ;;"ZSY[STEM][:tvexpr] [expr][,...]]
  ;;"ZTC[OMMIT][:tvexpr] [intexpr]
  ;;"ZTR[IGGER] gvn
  ;;"ZTS[TART][:tvexpr]
  ;;"ZWI[THDRAW][:tvexpr] glvn
  ;;"ZWR[ITE][:tvexpr] [zwrglvn[,...]]
  ;;"<DONE>
  ;
FUNCTIONS ;  
  ;;"$A[SCII](expr[,intexpr])
  ;;"$C[HAR](intexpr[,...])
  ;;"$D[ATA](glvn)
  ;;"$E[XTRACT](expr[,intexpr1[,intexpr2]])
  ;;"$F[IND](expr1,expr2[,intexpr])
  ;;"$FN[UMBER](numexpr,expr[,intexpr])
  ;;"$G[ET](glvn[,expr])
  ;;"$INCR[EMENT](glvn[,numexpr])
  ;;"$I(glvn[,numexpr])    <-- SAME AS $INCR
  ;;"$J[USTIFY](expr,intexpr1[,intexpr2])
  ;;"$L[ENGTH](expr1[,expr2])
  ;;"$NA[ME](glvn[,intexpr])
  ;;"$N[EXT](glvn)
  ;;"$O[RDER](glvn[,expr])
  ;;"$P[IECE](expr1,expr2[,intexpr1[,intexpr2]])
  ;;"$QL[ENGTH](namevalue)
  ;;"$QS[UBSCRIPT](namevalue, intexpr)
  ;;"$Q[UERY](glvn[,expr])
  ;;"$R[ANDOM](intexpr)
  ;;"$RE[VERSE](expr)
  ;;"$S[ELECT](tvexpr:expr[,...])
  ;;"$ST[ACK](intexpr[,expr])
  ;;"$T[EXT](entryref)
  ;;"$TR[ANSLATE](expr1[,expr2[,expr3]])
  ;;"$V[IEW](expr1[,expr2])
  ;;"$ZAHANDLE()                         <----- MISSING DEFINITION....
  ;;"$ZA[SCII](expr[,intexpr])
  ;;"$ZATRANSFORM(expr,intexpr[,{0|1|2|-2}][,{0|1}])
  ;;"$ZUADITLOG(expr)
  ;;"$ZBITAND(expr1,expr2)
  ;;"$ZBITCOUNT(expr)
  ;;"$ZBITFIND(expr,tvexpr[,intexpr])
  ;;"$ZBITGET(expr,intexpr)
  ;;"$ZBITLEN(expr)
  ;;"$ZBITNOT(expr)
  ;;"$ZBITOR(expr1,expr2)
  ;;"$ZBITSET(expr,intexpr,tvexpr)
  ;;"$ZBITSTR(intexpr[,tvexpr])
  ;;"$ZBITXOR(expr1,expr2)
  ;;"$ZCH[AR](intexpr[,...])
  ;;"$ZCO[LLATE](glvn,intexpr[,{0|1}])
  ;;"$ZCO[NVERT](expr1, expr2,[expr3])
  ;;"$ZDATA()                             <---- MISSING DEFINITION
  ;;"$ZD[ATE](expr1[,expr2[,expr3[,expr4]]]])
  ;;"$ZE[XTRACT](expr[,intexpr1[,intexpr2]])
  ;;"$ZF[IND](expr1,expr2[,intexpr])
  ;;"$ZGETJPI(expr1,expr2)
  ;;"$ZJOBEXAM([expr1[,expr2]])
  ;;"$ZJ[USTIFY](expr,intexpr1[,intexpr2])
  ;;"$ZL[ENGTH](expr1[,expr2])
  ;;"$ZM[ESSAGE](intexpr)
  ;;"$ZPARSE(expr1[,expr2[,expr3[,expr4[,expr5]]]])
  ;;"$ZPEEK("mnemonic[:argument]",offset,length[,format])
  ;;"$ZPI[ECE](expr1,expr2[,intexpr1[,intexpr2]])
  ;;"$ZP[REVIOUS](glvn)
  ;;"$ZQGBLMOD(gvn)
  ;;"$ZSEARCH(expr[,intexpr])
  ;;"$ZSIGPROC(expr1,expr2)
  ;;"$ZSOCKET(expr1,expr2[,[expr3][,expr4]])
  ;;"$ZSUB[STR] (expr ,intexpr1 [,intexpr2])
  ;;"$ZSYSLOG(expr)
  ;;"$ZTR[ANSLATE](expr1[,expr2[,expr3]])
  ;;"$ZTRIgger(expr1[,expr2])
  ;;"$ZTRNLNM(expr1[,expr2[,expr3[,expr4[,expr5[,expr6]]]]])
  ;;"$ZW[IDTH] (expr)
  ;;"$ZWRITE(expr[,intexpr])
  ;;"$ZYHASH(string[,salt])
  ;;"$ZYISSQLNULL(expr)
  ;;"$ZYSU[FFIX](string)
  ;;"<DONE>
  ;
ISV  ;"Intrinsic Special Variables
  ;;'$DEVICE
  ;;'$ECODE
  ;;'$ESTACK
  ;;'$ETRAP
  ;;'$HOROLOG
  ;;'$IO
  ;;'$JOB
  ;;'$KEY
  ;;'$PRINCIPAL
  ;;'$QUIT
  ;;'$REFERENCE
  ;;'$STACK
  ;;'$STORAGE
  ;;'$SYSTEM
  ;;'$TEST
  ;;'$TLEVEL
  ;;'$TRESTART
  ;;'$X
  ;;'$Y
  ;;'$ZA
  ;;'$ZALLOCSTOR
  ;;'$ZAUDIT
  ;;'$ZB
  ;;'$ZCHSET
  ;;'$ZCLOSE
  ;;'$ZCMDLINE
  ;;'$ZCOMPILE
  ;;'$ZCSTATUS
  ;;'$ZDATEFORM
  ;;'$ZDIRECTORY
  ;;'$ZEDITOR
  ;;'$ZEOF
  ;;'$ZERROR
  ;;'$ZGBLDIR
  ;;'$ZHOROLOG
  ;;'$ZININTERRUPT
  ;;'$ZINTERRUPT
  ;;'$ZIO
  ;;'$ZJOB
  ;;'$ZKEY
  ;;'$ZLEVEL
  ;;'$ZMALLOCLIM
  ;;'$ZMAXTPTIME
  ;;'$ZMODE
  ;;'$ZONLNRLBK
  ;;'$ZPATNUMERIC
  ;;'$ZPIN
  ;;'$ZPOSITION
  ;;'$ZPOUT
  ;;'$ZPROMPT
  ;;'$ZQUIT
  ;;'$ZREALSTOR
  ;;'$ZRELDATE
  ;;'$ZROUTINES
  ;;'$ZSOURCE
  ;;'$ZSTATUS
  ;;'$ZSTEP
  ;;'$ZSTRPLLIM
  ;;'$ZSYSTEM
  ;;'$ZTEXIT
  ;;'$ZTIMEOUT
  ;;'$ZTRAP
  ;;'$ZUSEDSTOR
  ;;'$ZUT
  ;;'$ZVERSION
  ;;'$ZYERROR
  ;;'$ZYINTRSIG
  ;;'$ZYRELEASE
  ;;'$ZYSQLNULL
  ;;'$ZTDATA
  ;;'$ZTDELIM
  ;;'$ZTLEVEL
  ;;'$ZTNAME
  ;;'$ZTOLDVAL
  ;;'$ZTRIGGEROP
  ;;'$ZTSLATE
  ;;'$ZTUPDATE
  ;;'$ZTVALUE
  ;;'$ZTWORMHOLE
  ;;"<DONE>
  