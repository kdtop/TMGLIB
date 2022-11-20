TMGSTUT3 ;TMG/kst/SACC Compliant String Util Lib ;9/20/17, 9/29/22
         ;;1.0;TMG-LIB;**1,17**;7/17/12
  ;
  ;
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
  ;"
  ;"This file is part of the TMG LIBRARY, and may only be used in accordence
  ;" to license terms outlined in separate file TMGLICNS.m, which should 
  ;" always be distributed with this file.
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;
  ;"TMG STRING UTILITIES v2
  ;"SAAC Compliant Version.
  ;"This file will be used to hold SACC compliant versions of
  ;"  routines found in TMGSTUTL.
  ;"I don't initially have time to convert them all at once, so will
  ;"  slowly move them over, as needed.
  ;
  ;"=======================================================================
  ;" API -- Public Functions.
  ;"=======================================================================
  ;"NICESPLT(S,LEN,S1,S2,S2MIN,DIVCH) -- Split string to length, at spaces
  ;"$$REPLSTR(STR,MATCH,NEWVAL) --REPLACE STRING: look for all instances of MATCH in STR, and replace with NEWVAL
  ;"STRIPARR(REF,STR) --Strip STR from each line of @REF array
  ;"REPLARR(REF,SRCHSTR,REPLSTR) -- Replace each instance of SRCHSTR with REPLSTR from each line of @REF array
  ;"$$MATCHXTR(STR,DIVCH,GROUP,MAP) -- Extract a string bounded by DIVCH, honoring matching encapsulators
  ;"$$LMATCH(STR,SUBSTR) -- Does left part of STR match SUBSTR?
  ;"$$RMATCH(STR,SUBSTR) -- Does right part of STR match SUBSTR? 
  ;"MAPMATCH(STR,MAP) -- map a string with nested braces, parentheses etc (encapsulators)
  ;"$$MAKEWS(N)  -- Return a whitespace string that is n characters long
  ;"$$QTPROTCT(STR)-- Protects quotes by converting all quotes to double quotes
  ;"$$UNQTPROT(STR) --Reversed quotes protection by converting all double quotes to single quotes
  ;"$$ISALPHNUM(CH) -- is character alphanumeric?
  ;"$$ISNUM(STR) -- Return IF STR is numeric
  ;"$$NUMSTR(STR,PARTB)  --Return numeric of string, and residual back in PARTB
  ;"$$EXTRACTNUM(STR) --Extract numbers scattered through string, exluding any non-number.
  ;"$$RANDSTR(LEN,FLAGS,EXCLUDE) --Output a random string of given length, with options
  ;"$$TRIM2NUM(STR) --Trim of anything in string up to, but not including, a number
  ;"$$NUMAFTERLABEL(STR,LABEL) -- Return number following label. 
  ;"STRIPCMD(STR)  -- Strip command characters
  ;"$$POS(SUBSTR,S,COUNT)  ;return the beginning position of SUBSTR in S
  ;"$$POSSET(STR,SUBSTRSET,STARTPOS) --POSITION OF CHARACTER FROM SET -- different from $$POS()
  ;"$$ENDQTPOS(STR,P1) -- return position of closing quotes 
  ;"$$GETWORD(STR,POS,OPENDIV,CLOSEDIV) -- Extract a word from a sentance, bounded by OPENDIV,CLOSEDIV 
  ;"$$NEXTTOKN(STR) --GET NEXT TOKEN
  ;"$$NEXTWORD(STR,DIVCHS) --Get next word, based on first found divisor character 
  ;"$$PIECE2(STR,DIVCHS,IDX,IDX2,DIVUSED) Get indexed word, based on first found divisor character 
  ;"$$NEXTCH(STR,STARTPOS,A,B,C,D,E,F,G) --Get first next char (or string fragment), matching from 7 possible inputs.  
  ;"$$NEXTCH2(STR,STARTPOS,FRAGS) --Get first next character (or string fragment), matching from array of possible inputs.
  ;"$$SUBASCII(STR)  --TAKES INPUT OF AAC AND RETURNS AAB (useful for finding just before, to $ORDER to STR)
  ;"$$MIDSTRCOLOR(TEXT,START,LEN) -- similar to MidStr(), but skipping over {{color}} tags  
  ;"=======================================================================
  ;" Private Functions.
  ;"=======================================================================
  ;"$$NEEDEDWS(S,SPECIALINDENT,INDENT) -- create white space need for wrapped lines
  ;"=======================================================================
  ;"Dependancies: XLFSTR
  ;
  ;"=======================================================================
  ;"=======================================================================
  ;
NICESPLT(S,LEN,S1,S2,S2MIN,DIVCH) ;
  ;"Purpose: to split S into two strings, S1 & S2
  ;"      Furthermore, S1's length must be <= length.
  ;"      and the split will be made at spaces
  ;"Input: S -- the string to split
  ;"       LEN -- the length limit of S1
  ;"       S1 -- PASS BY REFERENCE, an OUT parameter
  ;"              receives first part of split
  ;"       S2 -- PASS BY REFERENCE, an OUT parameter
  ;"              receives the rest of string
  ;"       S2MIN -- OPTIONAL -- the minimum that
  ;"              length of S2 can be.  Note, If S2
  ;"              is "", then this is not applied
  ;"       DIVCH -- OPTIONAL, default is " ".
  ;"              This is the character to split words by
  ;"Output: S1 and S2 is filled with data
  ;"Result: none
  SET (S1,S2)=""
  IF $GET(DIVCH)="" SET DIVCH=" "
  IF $LENGTH(S)'>LEN SET S1=S GOTO NSPDN
  NEW I
  NEW DONE
  FOR I=200:-1:1 DO  QUIT:(DONE)
  . SET S1=$PIECE(S,DIVCH,1,I)_DIVCH
  . SET S2=$PIECE(S,DIVCH,I+1,999)
  . SET DONE=($LENGTH(S1)'>LEN)
  . IF DONE,+$GET(S2MIN)>0 DO
  . . IF S2="" QUIT
  . . SET DONE=($LENGTH(S2)'<S2MIN)
NSPDN ;
  QUIT
  ;
NEEDEDWS(S,SPECIALINDENT,INDENT) ;"NEEDED WHITE SPACE
  ;"Scope: PRIVATE/local
  ;"Purpose: Evaluate the line, and create the white space string
  ;"        need for wrapped lines
  ;"Input: s -- the string to eval.  i.e.
  ;"                "  John is very happy today ... .. .. .. .."
  ;"        or      "  1. John is very happy today ... .. .. .. .."
  ;"        SPECIALINDENT -- See SplItLine() discussion
  ;"        INDENT -- See SplItLine() discussion
  NEW RESULT SET RESULT=""
  IF $GET(S)="" GOTO NDWSDN
  NEW WSNUM SET WSNUM=+$GET(INDENT,0)
  SET WSNUM=WSNUM+$$NUMLWS(S)
  ;
  IF $GET(SPECIALINDENT)=1 DO
  . NEW TEMPS,FIRSTWORD
  . SET TEMPS=$$TRIM^XLFSTR(.S,"l")
  . SET FIRSTWORD=$PIECE(TEMPS," ",1)
  . IF (FIRSTWORD?.N1".")!(FIRSTWORD?1.4U1".") DO     ;"match for '#.' pattern
  . . SET WSNUM=WSNUM+$LENGTH(FIRSTWORD)
  . . SET TEMPS=$PIECE(TEMPS," ",2,9999)
  . . SET WSNUM=WSNUM+$$NUMLWS(.TEMPS)+1
  ;
  SET RESULT=$$MAKEWS(WSNUM)
NDWSDN  ;
  QUIT RESULT
  ;
NUMLWS(S)  ;"NUM LEFT WHITE SPACE
  ;"Scope: PUBLIC FUNCTION
  ;":Purpose: Count the num of white space characters on left side of the string
  NEW RESULT SET RESULT=0
  NEW I FOR I=1:1:$LENGTH(S) QUIT:$EXTRACT(S,I)'=" "  SET RESULT=RESULT+1
  QUIT RESULT
  ;
MAKEWS(N)  ;"MAKE WHITE SPACE
  ;"Scope: PUBLIC FUNCTION
  ;"Purpose: Return a whitespace string that is n characters long
  NEW RESULT SET RESULT=""
  SET N=+$GET(N)
  NEW I FOR I=1:1:N SET RESULT=RESULT_" "
  QUIT RESULT
  ;
REPLSTR(STR,MATCH,NEWVAL)  ;"REPLACE STRING 
  ;"Scope: PUBLIC FUNCTION
  ;"Purpose: to look for all instances of Match in S, and replace with NewValue
  ;"Input: STR - string to alter.  Altered if passed by reference
  ;"       Match -- the sequence to look for, i.e. '##'
  ;"       NewValue -- what to replace Match with, i.e. '$$'
  ;"Note: This is different than $TRANSLATE, as follows
  ;"      $TRANSLATE("ABC###DEF","###","$") --> "ABC$$$DEF"
  ;"      $$REPLSTR("ABC###DEF","###","$") --> "ABC$DEF"
  ;"Result: returns altered string (if any alterations indicated)
  ;"Output: STR is altered, if passed by reference.
  ;"NEW SPEC SET SPEC($GET(MATCH))=$GET(NEWVAL)
  ;"SET STR=$$REPLACE^XLFSTR(STR,.SPEC)
  ;"QUIT STR
  NEW SLEN,RLEN,POS SET SLEN=$L(MATCH),RLEN=$LENGTH(NEWVAL),POS=1
  FOR  QUIT:POS=0  DO
  . SET POS=$FIND(STR,MATCH,POS) QUIT:POS=0
  . SET STR=$EXTRACT(STR,1,POS-SLEN-1)_NEWVAL_$EXTRACT(STR,POS,$LENGTH(STR))
  . SET POS=POS-SLEN+RLEN
  QUIT STR
  ;
STRIPARR(REF,STR) ;"Strip STR from each line of ARR
  ;"INPUT: REF -- PASS BY NAME.  Expected format: @REF@(#)=<line of text>
  ;"       STR -- string to be removed from each line.
  ;"Result: 1 if something removed, otherwise 0
  QUIT $$REPLARR(REF,STR,"")
  ;
REPLARR(REF,SRCHSTR,REPLSTR) ;"REPLACE each instance of SRCHSTR with REPLSTR from each line of ARR
  ;"INPUT: REF -- PASS BY NAME.  Expected format: @REF@(#)=<line of text>
  ;"       STR -- string to be removed from each line.
  ;"Result: 1 if something removed, otherwise 0
  NEW RESULT,LINENUM SET (RESULT,LINENUM)=0  
  FOR  SET LINENUM=$ORDER(@REF@(LINENUM)) QUIT:LINENUM'>0  DO
  . ;"SET LINETEXT=$GET(@REF@(LINENUM)) QUIT:(LINETEXT'[SRCHSTR)
  . ;"FOR  QUIT:(LINETEXT'[SRCHSTR)  DO
  . ;". SET LINETEXT=$PIECE(LINETEXT,SRCHSTR,1)_REPLSTR_$PIECE(LINETEXT,SRCHSTR,2,999)
  . ;". SET RESULT=1
  . NEW LINETEXT SET LINETEXT=$GET(@REF@(LINENUM))
  . NEW LT2 SET LT2=$$REPLSTR(LINETEXT,SRCHSTR,REPLSTR) QUIT:LT2=LINETEXT
  . SET @REF@(LINENUM)=LT2,RESULT=1
  QUIT RESULT
  ;    
MATCHXTR(STR,DIVCH,GROUP,MAP,RESTRICT) ;"MATCH EXTRACT
  ;"Purpose to extract a string bounded by DIVCH, honoring matching encapsulators
  ;"Note: the following markers are honored as paired encapsulators:
  ;"      ( ),  { },  | |,  < >,  # #, [ ],
  ;"      To specify which SET to use, DIVCH should specify only OPENING character
  ;"E.g. DIVCH="{"
  ;"       s="Hello {There}" --> return "There"
  ;"       s="Hello {There {nested braces} friend}" --> return "There {nested braces} friend"
  ;"     DIVCH="|"
  ;"       s="Hello |There|" --> "There"
  ;"       s="Hello |There{|friend|}|" --> "There{|friend|}"
  ;"          Notice that the second "|" was not paired to the first, because an opening brace was first.
  ;"Input: STR -- The string to evaluate
  ;"       DIVCH -- The opening character of the encapsulator to use
  ;"       GROUP -- OPTIONAL.  Default is 1.  If line has more than one SET of encapsulated entries, which group to get from
  ;"       MAP -- OPTIONAL.  PASS BY REFERENCE.  If function is to be called multiple times,
  ;"              then a prior MAP variable can be passed to speed processing.
  ;"       RESTRICT -- OPTIONAL.  A string of allowed opening encapsulators (allows others to be ignored)
  ;"                  e.g. "{(|"  <-- will cause "<>#[]" to be ignored
  ;"Results: Returns extracted string.
  IF $DATA(MAP)=0 DO MAPMATCH(STR,.MAP,.RESTRICT)
  SET GROUP=$GET(GROUP,1)
  SET DIVCH=$GET(DIVCH)
  NEW RESULT SET RESULT=""
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(MAP(GROUP,IDX)) QUIT:(IDX="")!(RESULT'="")  DO
  . IF DIVCH'=$GET(MAP(GROUP,IDX)) QUIT
  . NEW P,JDX
  . FOR JDX=1,2 SET P(JDX)=+$GET(MAP(GROUP,IDX,"Pos",JDX))
  . SET RESULT=$EXTRACT(STR,P(1)+1,P(2)-1)
  QUIT RESULT
  ;
MAPMATCH(STR,MAP,RESTRICT)  ;"MAP MATCHING ENCAPSULATORS
  ;"Purpose to map a string with nested braces, parentheses etc (encapsulators)
  ;"Note: the following markers are honored as paired encapsulators:
  ;"      ( ),  { },  | |,  < >,  # #,  " "
  ;"Input: STR -- string to evaluate
  ;"       MAP -- PASS BY REFERENCE.  An OUT PARAMETER.  Prior values are killed.  Format:
  ;"           MAP(GROUP,Depth)=OpeningSymbol
  ;"           MAP(GROUP,Depth,"Pos",1)=index of opening symbol
  ;"           MAP(GROUP,Depth,"Pos",2)=index of paired closing symbol
  ;"       RESTRICT -- OPTIONAL.  A string of allowed opening encapsulators (allows others to be ignored)
  ;"                  e.g. "{(|"  <-- will cause "<>#[]" to be ignored
  ;"E.g.  STR="Hello |There{|friend|}|"
  ;"           MAP(1,1)="|"
  ;"           MAP(1,1,"Pos",1)=7
  ;"           MAP(1,1,"Pos",2)=23
  ;"           MAP(1,2)="{"
  ;"           MAP(1,2,"Pos",1)=13
  ;"           MAP(1,2,"Pos",2)=22
  ;"           MAP(1,3)="|"
  ;"           MAP(1,3,"Pos",1)=14
  ;"           MAP(1,3,"Pos",2)=21
  ;"Eg.   STR="Hello |There{|friend|}|  This is more (and I (want { to say} !) OK?)"
  ;"           map(1,1)="|"
  ;"           map(1,1,"Pos",1)=7
  ;"           map(1,1,"Pos",2)=23
  ;"           map(1,2)="{"
  ;"           map(1,2,"Pos",1)=13
  ;"           map(1,2,"Pos",2)=22
  ;"           map(1,3)="|"
  ;"           map(1,3,"Pos",1)=14
  ;"           map(1,3,"Pos",2)=21
  ;"           map(2,1)="("
  ;"           map(2,1,"Pos",1)=39
  ;"           map(2,1,"Pos",2)=68
  ;"           map(2,2)="("
  ;"           map(2,2,"Pos",1)=46
  ;"           map(2,2,"Pos",2)=63
  ;"           map(2,3)="{"
  ;"           map(2,3,"Pos",1)=52
  ;"           map(2,3,"Pos",2)=60
  ;"Results: none
  SET RESTRICT=$GET(RESTRICT,"({|<#""")
  NEW MATCH,DEPTH,IDX,GROUP
  IF RESTRICT["(" SET MATCH("(")=")"
  IF RESTRICT["{" SET MATCH("{")="}"
  IF RESTRICT["|" SET MATCH("|")="|"
  IF RESTRICT["<" SET MATCH("<")=">"
  IF RESTRICT["#" SET MATCH("#")="#"
  IF RESTRICT["""" SET MATCH("""")=""""
  KILL MAP
  SET DEPTH=0,GROUP=1
  FOR IDX=1:1:$LENGTH(STR) DO
  . NEW CH SET CH=$EXTRACT(STR,IDX)
  . IF CH=$GET(MAP(GROUP,DEPTH,"Closer")) DO  QUIT
  . . SET MAP(GROUP,DEPTH,"Pos",2)=IDX
  . . KILL MAP(GROUP,DEPTH,"Closer")
  . . SET DEPTH=DEPTH-1
  . . IF DEPTH=0 SET GROUP=GROUP+1
  . IF $DATA(MATCH(CH))=0 QUIT
  . SET DEPTH=DEPTH+1
  . SET MAP(GROUP,DEPTH)=CH
  . SET MAP(GROUP,DEPTH,"Closer")=MATCH(CH)
  . SET MAP(GROUP,DEPTH,"Pos",1)=IDX
  QUIT
  ;
QTPROTCT(STR) ;QUOTE PROTECT
  ;"Purpose: Protects quotes by converting all quotes to double quotes (" --> "")
  ;"Input : s -- The string to be modified.  Original string is unchanged.
  ;"Result: returns a string with all instances of single instances of quotes
  ;"        being replaced with two quotes.
  NEW TEMPS
  SET TEMPS=$$REPLSTR^TMGSTUT3($GET(STR),"""""","<^@^>")  ;"protect original double quotes
  SET TEMPS=$$REPLSTR^TMGSTUT3(TEMPS,"""","""""")
  SET TEMPS=$$REPLSTR^TMGSTUT3(TEMPS,"<^@^>","""""")  ;"reverse protection
  QUIT TEMPS
  ;
UNQTPROT(STR)  ;"REVERSE QUOTE PROTECTION
  ;"Purpose: Reversed quotes protection by converting all double quotes to single quotes ("" --> ")
  ;"         And all single quotes are removed
  ;"Input : s -- The string to be modified.  Original string is unchanged.
  ;"Result: returns a string with changes as above. 
  ;"NOTE: If this is called on a string that was NOT FIRST PROTECTED via QTPROTCT, this will break string.
  NEW TEMPS
  SET TEMPS=$$REPLSTR^TMGSTUT3($GET(STR),"""""","<^@^>")  ;"protect original double quotes
  SET TEMPS=$$REPLSTR^TMGSTUT3(TEMPS,"""","") ;"remove all single quotes
  SET TEMPS=$$REPLSTR^TMGSTUT3(TEMPS,"<^@^>","""")  ;"convert origianl double quotes to singles
  QUIT TEMPS
  ;  
ISALPHNUM(CH) ;" Test is CH is in [0..9,A..Z,a..z]
  NEW RESULT SET RESULT=0  ;"default to failure
  NEW A SET A=$ASCII(CH)
  NEW NUMERIC SET NUMERIC=((A>47)&(A<58)) ;"0..9
  NEW UPALPH  SET UPALPH=((A>64)&(A<91))  ;"A..Z
  NEW LOALPH  SET LOALPH=((A>96)&(A<123))  ;"a..z
  QUIT (NUMERIC!UPALPH!LOALPH)
  ;  
ISNUM(STR) ;" Return if STR is numeric (and a VALID numerical string.)  
  ;"NOTE:  This is different than just doing +STR.  It handles fractions and commas
  ;"Self reminder: +"9.0" --> "9" ('cardinal form')
  ;"NOTE: See TESTISNUM for examples of inputs and outputs
  NEW RESULT SET RESULT=(+STR=STR) 
  IF (RESULT=1)!(STR="") GOTO ISNMDN
  IF STR[".",$LENGTH(STR,".")>2 GOTO ISNMDN  ;"Only allow 1 decimal
  IF STR["/" DO  GOTO ISNMDN
  . IF $LENGTH(STR,"/")>2 QUIT  ;"Result=0.  Only allow 1 division
  . SET RESULT=($$ISNUM($PIECE(STR,"/",1)))&($$ISNUM($PIECE(STR,"/",2)))
  NEW S2 SET S2=STR 
  IF S2?1(1"+",1"-").E SET S2=$EXTRACT(S2,2,$LENGTH(S2))  ;"Trim any leading + or -
  NEW IDX FOR IDX=1:1:$LENGTH(S2) SET RESULT=("1234567890.,"[$EXTRACT(S2,IDX)) QUIT:RESULT=0
  IF RESULT=0 GOTO ISNMDN
  IF S2["." DO  GOTO:(RESULT=0) ISNMDN
  . NEW SA,SB SET SA=$PIECE(S2,".",1),SB=$PIECE(S2,".",2)
  . IF SA="",SB="" SET RESULT=0 QUIT  ;"An isolated "." is not a number
  . IF SB["," SET RESULT=0 QUIT      ;"No commas after decimal
  IF S2["," DO  GOTO:(RESULT=0) ISNMDN
  . NEW STRA,S3 SET S3=$PIECE(S2,".",1)  ;"ALL COMMA GROUPINGS MUST =3 EXCEPT LEFT-MOST
  . FOR IDX=$LENGTH(S3,","):-1:2 SET RESULT=($LENGTH($PIECE(S3,",",IDX))=3) QUIT:RESULT=0
  QUIT RESULT
ISNMDN  ;
  QUIT RESULT
  ;     
NUMSTR(STR,PARTB)  ;"Return numeric of string, and residual back in PARTB
  ;"Note: this will be different than +STR.  +"002" --> "2", but this would return "002"
  ;"NOTE: See TESTNUMSTR for examples of inputs and outputs
  NEW DONE SET DONE=0
  NEW LEN SET LEN=$LENGTH(STR)
  NEW NUM SET NUM="",PARTB=""
  NEW IDX FOR IDX=1:1:LEN DO  QUIT:DONE
  . NEW CH SET CH=$EXTRACT(STR,IDX)
  . IF "1234567890.,+-/"'[CH DO  QUIT
  . . SET PARTB=$EXTRACT(STR,IDX,LEN),DONE=1
  . SET NUM=NUM_CH
  FOR  QUIT:(NUM="")!($$ISNUM(NUM)=1)  DO  ;"Shorten NUM until it is a valid number string
  . NEW LEN SET LEN=$LENGTH(NUM)
  . SET PARTB=$EXTRACT(NUM,LEN)_PARTB
  . SET NUM=$EXTRACT(NUM,1,LEN-1)
  QUIT NUM
  ;
EXTRACTNUM(STR) ;"Extract numbers scattered through string, exluding any non-number.  
  ;"e.g. (123) 456-7890  --> 1234567890
  NEW IDX,RESULT SET RESULT=""
  FOR IDX=1:1:$LENGTH(STR) DO
  . NEW CH SET CH=$EXTRACT(STR,IDX)
  . IF CH?1N SET RESULT=RESULT_CH
  QUIT RESULT
  ;    
RANDSTR(LEN,FLAGS,EXCLUDE) ;"Output a random string of given length, with options
  ;"Input: LEN -- desired length of output string
  ;"       FLAGS -- OPTIONAL. default is "UL"
  ;"          If string contains flag:
  ;"           U -- include upper case chars
  ;"           L -- include lower case chars
  ;"           N -- include numbers
  ;"           P -- include punctuation
  ;"       Exclude - OPTIONAL.  any provided chars in string will NOT be included in output
  NEW RESULT SET RESULT=""
  NEW UP SET UP="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  NEW LOW SET LOW="abcdefghijklmnopqrstuvwxyz"
  NEW NUM SET NUM="0123456789"
  NEW PUNCT SET PUNCT="!#$%&'()*+,-./:;<=>?@[\]^_`{|}~"
  SET FLAGS=$$UP^XLFSTR($GET(FLAGS,"UL"))
  NEW CHARS SET CHARS=""
  IF FLAGS["U" SET CHARS=CHARS_UP 
  IF FLAGS["L" SET CHARS=CHARS_LOW 
  IF FLAGS["N" SET CHARS=CHARS_NUM 
  IF FLAGS["P" SET CHARS=CHARS_PUNCT
  IF $GET(EXCLUDE)]"" SET CHARS=$TRANSLATE(CHARS,EXCLUDE,"")
  SET LEN=+$GET(LEN)
  NEW CHARLEN SET CHARLEN=$LENGTH(CHARS)
  NEW IDX FOR IDX=1:1:LEN DO 
  . NEW R SET R=$RANDOM(CHARLEN-1)+1
  . NEW CH SET CH=$EXTRACT(CHARS,R)
  . SET RESULT=RESULT_CH
  QUIT RESULT
  ;
TRIM2NUM(STR) ;"Trim of anything in string up to, but not including, a number
  NEW DONE SET DONE=0
  NEW RESULT SET RESULT=$GET(STR)
  NEW CH,LEN SET LEN=$LENGTH(RESULT)
  FOR  QUIT:(RESULT="")!(DONE)  DO
  . SET CH=$EXTRACT(RESULT,1)
  . IF "1234567890.+-"[CH SET DONE=1 QUIT
  . SET RESULT=$EXTRACT(RESULT,2,LEN)
  QUIT RESULT
  ;
NUMAFTERLABEL(STR,LABEL)  ;"Return number following label. It is assumed various non-alphanumeric chars will follow label, before number
  SET LABEL=$GET(LABEL," ")
  SET STR=$GET(STR)
  NEW STRB SET STRB=$PIECE(STR,LABEL,2)
  SET STRB=$$TRIM2NUM^TMGSTUT3(STRB)
  SET STRB=$$NUMSTR(STRB)
  QUIT STRB
  ;
TESTISNUM ;" 
  NEW NUM
  WRITE "SHOULD ACCEPT (1):",!
  FOR NUM="5","005","5.0","5.00","5,000","5,000,000","5/10","+5","-5.00","5,000/10,000" DO
  . WRITE NUM," --> ",$$ISNUM(NUM),!
  WRITE !,"SHOULD FAIL (0):",!
  FOR NUM="5A","ABC","5,","5,00,00","5,000.000,000","9/11/17","9.11.17","--5","5-3","-4-2",".","," DO
  . WRITE NUM," --> ",$$ISNUM(NUM),!
  QUIT
  ;
TESTNUMSTR ;" 
  NEW NUM  
  FOR NUM="5","005","5.0","5.00","5,00mg","5,000,000","5/10","+5","-5.00","5,000/10,000" DO
  . WRITE NUM," --> ",$$NUMSTR(NUM,.PARTB)," residual = '",PARTB,"'",!
  FOR NUM="5A","ABC","5,apple","9/11/17","9.11.17","--5","5-3","-4-2" DO
  . WRITE NUM," --> ",$$NUMSTR(NUM,.PARTB)," residual = '",PARTB,"'",!
  QUIT
  ;
STRIPCMD(STR)  ;"Strip command characters
  ;"Purpose: Strip all characters < #32 from string.
  NEW CODES,IDX,RESULT
  SET CODES=""
  FOR IDX=1:1:31 SET CODES=CODES_$CHAR(IDX)
  SET RESULT=$TRANSLATE(STR,CODES,"")
  QUIT RESULT
  ;        
POS(SUBSTR,S,COUNT)  ;
  ;"Purpose: return the beginning position of SUBSTR in S
  ;"Input: SUBSTR -- the string to be searched for in S
  ;"       S -- the string to search
  ;"       count -- OPTIONAL, the instance to return pos of (1=1st, 2=2nd, etc.)
  ;"              if count=2 and only 1 instance exists, then 0 returned
  ;"Result: the beginning position, or 0 if not found
  ;"Note: This function differs from $find in that $find returns the pos of the
  ;"      first character AFTER the subStr
  SET COUNT=+$GET(COUNT) IF COUNT'>0 SET COUNT=1
  NEW RESULT SET RESULT=0
  NEW INSTANCE SET INSTANCE=1
PS1 ;
  SET RESULT=$FIND(S,SUBSTR,RESULT+1)
  IF RESULT>0 SET RESULT=RESULT-$LENGTH(SUBSTR)
  IF COUNT>INSTANCE SET INSTANCE=INSTANCE+1 GOTO PS1
  QUIT RESULT        
  ;
POSSET(STR,SUBSTRSET,STARTPOS) ;"POSITION OF CHARACTER FROM SET -- different from $$POS()
  ;"Input: STR -- the string to search (NOTE: the order is different from $$POS())
  ;"       SUBSTRSET -- A SET of characters to search for.  Match will be made
  ;"            if character at position is contained in SET
  ;"       STARTPOS -- OPTIONAL.  Default = 1
  ;"Result: position of character, or 0 if not found.
  NEW RESULT SET RESULT=0
  SET SUBSTRSET=$GET(SUBSTRSET)
  NEW POS SET POS=+$GET(STARTPOS) IF POS'>0 SET POS=1
  NEW LEN SET LEN=$LENGTH(STR)
  NEW FOUND SET FOUND=0
  FOR  DO  QUIT:(RESULT>0)!(POS>LEN)
  . NEW CH SET CH=$EXTRACT(STR,POS)
  . IF SUBSTRSET[CH SET RESULT=POS QUIT
  . SET POS=POS+1  
  QUIT RESULT
  ;
INQT(STR,POS)  ;" In Quote?
  ;"Purpose: to return if a given character, in string(s), is insided quotes
  ;"         e.g. s='His name is "Bill," OK?'  and if p=14, then returns 1
  ;"         (note the above string is usually stored as:
  ;"           "His name is ""Bill,"" OK?" in the text editor, BUT in the
  ;"          strings that will be passed here I will get only 1 quote character
  ;"Input: STR -- the string to scan
  ;"       POS -- the position of the character in question
  ;"Results: 0 if not inside quotes, 1 if it is.
  ;"NOTE: IF POS points to the bounding quotes, the result is 0
  NEW ISINQT SET ISINQT=0
  SET POS=+$GET(POS) IF (POS>$LENGTH(STR))!(POS<1) GOTO IQTDN
  NEW P2 SET P2=$FIND(STR,"""")-1
  IF P2<POS FOR P2=P2-1:1:POS SET:($EXTRACT(STR,P2)="""") ISINQT='ISINQT
IQTDN ;
  QUIT ISINQT
  ;
ENDQTPOS(STR,P1) ;"Ending quote position
  ;"Purpose: given position of opening quotes (and it is assumed that this IS
  ;"         CORRECTLY PASSED IN, then this function will give position of
  ;"         corresponding closing quotes
  ;"Input:STR -- string to search
  ;"      P1 -- position of opening quote. $EXTRACT(STR,P1) must be a quote
  ;"Result: ending position, or 0 if problem.
  NEW P2 SET P2=0
  SET STR=$GET(STR) IF STR="" GOTO EQPDN
  SET P1=$GET(P1) IF (P1>$LENGTH(STR))!(P1<1) GOTO EQPDN
  SET P2=P1+1
  NEW DONE SET DONE=0
  FOR  DO  QUIT:DONE
  . SET P2=$FIND(STR,"""",P2)-1
  . IF P2=0 SET DONE=1 QUIT
  . IF $EXTRACT(STR,P2+1)'="""" SET DONE=1 QUIT
  . SET P2=P2+2  
EQPDN ;
  QUIT P2
  ;        
GETWORD(STR,POS,OPENDIV,CLOSEDIV)
  ;"Purpose: Extract a word from a sentance, bounded by OPENDIV,CLOSEDIV
  ;"Example: STR="The cat is hungry", POS=14 --> returns "hungry"
  ;"Example: STR="Find('Purple')", POS=8, OPENDIV="(", CLOSEDIV=")" --> returns "'Purple'"
  ;"Input: STR -- the string containing the source sentence
  ;"       POS -- the index of a character anywhere inside desired word.
  ;"       OPENDIV -- OPTIONAL, default is " "  this is what marks the start of the word.
  ;"                NOTE: IF $LENGTH(OPENDIV)>1, then OPENDIV is considered
  ;"                      to be a SET of characters, any of which can be used
  ;"                      as a opening character.
  ;"       CLOSEDIV -- OPTIONAL, default is " "  this is what marks the end of the word.
  ;"                NOTE: IF $LENGTH(CLOSEDIV)>1, then CLOSEDIV is considered
  ;"                      to be a SET of characters, any of which can be used
  ;"                      as a closing character.
  ;"Results: returns desired word, or "" if problem.
  NEW RESULT SET RESULT=""
  SET OPENDIV=$GET(OPENDIV," "),CLOSEDIV=$GET(CLOSEDIV," ")
  SET POS=+$GET(POS) IF POS'>0 GOTO GWDN
  NEW LEN SET LEN=$LENGTH(STR)
  NEW P2 FOR P2=POS:1:LEN IF CLOSEDIV[$EXTRACT(STR,P2) SET P2=P2-1 QUIT
  NEW P1 FOR P1=POS:-1:1 IF OPENDIV[$EXTRACT(STR,P1) SET P1=P1+1 QUIT
  SET RESULT=$EXTRACT(STR,P1,P2)
GWDN ;
  QUIT RESULT
  ;
NEXTTOKN(STR) ;"GET NEXT TOKEN
  ;"INPUT: STR -- PASS BY REFERENCE.  Return trimmed from token
  ;"Result: Returns next token, and STR is modified.
  ;"Tokens can begin with $, $$, or %
  ;"Tokens end when character not in 'A'..'Z','a'..'z','0'..'9'
  NEW STARTING SET STARTING=1
  NEW DONE SET DONE=0
  NEW TOKEN SET TOKEN=""
  NEW LEN SET LEN=$LENGTH(STR)
  NEW IDX FOR IDX=1:1:LEN DO  QUIT:DONE
  . NEW CH SET CH=$EXTRACT(STR,IDX)
  . NEW SPECIAL SET SPECIAL=("$%"[CH)
  . NEW ALPHNUM SET ALPHNUM=(CH?1ULN)
  . IF (SPECIAL&STARTING) SET TOKEN=TOKEN_CH QUIT
  . SET STARTING=0
  . IF ALPHNUM SET TOKEN=TOKEN_CH QUIT
  . SET DONE=1
  SET STR=$EXTRACT(STR,$LENGTH(TOKEN)+1,$LENGTH(STR))
  QUIT TOKEN
  ;
NEXTWORD(STR,DIVCHS,DIVUSED)  ;"Get next word, based on first found divisor character
  ;"INPUT: STR -- the string to work on
  ;"       DIVCHS -- This can be passed in 1 of two ways:
  ;"                 e.g. "; ,./" , a series of single character divisors, or
  ;"                 e.g. DIVCH("{{")=""
  ;"                      DIVCH("}}")=""  And array of multiple character divisors
  ;"                      DIVCH("--")=""  
  ;"                 OR a combination of both.  
  ;"       IDX -- the index to return
  ;"       DIVUSED -- OPTIONAL.  PASS BY REFERENCE.  This will return the actual div used.    
  ;"Result: Returns string for the sought piece
  ;"        If one of the divisors is not found in string, the ENTIRE string is returned.
  NEW RESULT SET RESULT=$$PIECE2(STR,.DIVCHS,1,1,.DIVUSED)
  IF RESULT="",DIVUSED="" SET RESULT=STR
  QUIT RESULT
  ;
PIECE2(STR,DIVCHS,IDX,IDX2,DIVUSED)  ;"Get indexed word, based on first found divisor character
  ;"INPUT: STR -- the string to work on
  ;"       DIVCH -- This can be passed in 1 of two ways:
  ;"                 e.g. "; ,./" , a series of single character divisors, or
  ;"                 e.g. DIVCH("{{")=""
  ;"                      DIVCH("}}")=""  And array of multiple character divisors
  ;"                      DIVCH("--")=""  
  ;"                 OR a combination of both.  
  ;"       IDX -- the index to return
  ;"       IDX2 -- OPTIONAL.  If given, then a range of pieces returned.
  ;"       DIVUSED -- OPTIONAL.  PASS BY REFERENCE.  This will return the actual div used.    
  ;"Result: Returns string for the sought piece
  ;"        If one of the divisors is not found in string, nothing is returned.
  SET IDX2=+$GET(IDX2) IF IDX2=0 SET IDX2=IDX 
  NEW JDX FOR JDX=1:1:$LENGTH(DIVCHS) SET DIVCHS($EXTRACT(DIVCHS,JDX))=""
  SET DIVUSED=$$NEXTCH2(STR,0,.DIVCHS)
  NEW RESULT SET RESULT=$PIECE(STR,DIVUSED,IDX,IDX2)
  QUIT RESULT
  ;
NEXTCH2(STR,STARTPOS,FRAGS) ;"Get first next character (or string fragment), matching from array of possible inputs.  
  ;"Purpose: Check string to determine which string fragment comes first and return it
  ;"INPUTS: STR -- the string to check
  ;"        STARTPOS -- the index to start $FIND at, default is 0
  ;"        FRAGS. PASS BY REFERENCE.  Format:
  ;"          FRAG("test string1")=""
  ;"          FRAG("test string2")=""
  ;"Results: returns which of inputs is found first, or "" if none found.  
  NEW MAX SET MAX=$LENGTH(STR)+1
  NEW TEST,POS,MIN,IDX,JDX SET MIN=MAX,(IDX,JDX)=1
  SET STARTPOS=+$GET(STARTPOS)
  NEW TEST SET TEST=""
  FOR  SET TEST=$ORDER(FRAGS(TEST)) QUIT:TEST=""  DO
  . SET POS(IDX)=$FIND(STR,TEST,STARTPOS)-$LENGTH(TEST)
  . SET POS(IDX,"TEST")=TEST
  . IF POS(IDX)'>0 KILL POS(IDX) QUIT
  . SET IDX=IDX+1
  NEW MINIDX SET MINIDX=0
  FOR JDX=1:1:IDX-1 DO
  . IF POS(JDX)'<MIN QUIT
  . SET MIN=POS(JDX)
  . SET MINIDX=JDX
  QUIT $GET(POS(MINIDX,"TEST"))
  ;      
NEXTCH(STR,STARTPOS,A,B,C,D,E,F,G) ;"Get first next character (or string fragment), matching from 7 possible inputs.  
  ;"Purpose: Check string to determine which string fragment comes first and return it
  ;"INPUTS: STR -- the string to check
  ;"        STARTPOS -- the index to start $FIND at, default is 0
  ;"        A..G the inputs to test for.  
  ;"Results: returns which of inputs is found first, or "" if none found.
  SET A=$GET(A),B=$GET(B),C=$GET(C),D=$GET(D),E=$GET(E),F=$GET(F),G=$GET(G)
  NEW FRAGS                  SET:A'="" FRAGS(A)=""      
  SET:B'="" FRAGS(B)=""      SET:C'="" FRAGS(C)=""      
  SET:D'="" FRAGS(D)=""      SET:E'="" FRAGS(E)=""      
  SET:F'="" FRAGS(F)=""      SET:G'="" FRAGS(G)=""
  QUIT $$NEXTCH2(.STR,.STARTPOS,.FRAGS)     
  ;    
LMATCH(STR,SUBSTR,CASESPEC) ;"Does left part of STR match SUBSTR?
  SET STR=$GET(STR),SUBSTR=$GET(SUBSTR) IF (STR="")!(SUBSTR="") QUIT 0
  IF $GET(CASESPEC)'=1 SET STR=$$UP^XLFSTR(STR),SUBSTR=$$UP^XLFSTR(SUBSTR)
  NEW PARTA SET PARTA=$EXTRACT(STR,1,$LENGTH(SUBSTR))
  QUIT (PARTA=SUBSTR)  
  ;
RMATCH(STR,SUBSTR,CASESPEC) ;"Does right part of STR match SUBSTR?
  SET STR=$GET(STR),SUBSTR=$GET(SUBSTR) IF (STR="")!(SUBSTR="") QUIT 0
  IF $GET(CASESPEC)'=1 SET STR=$$UP^XLFSTR(STR),SUBSTR=$$UP^XLFSTR(SUBSTR)
  NEW L1 SET L1=$LENGTH(STR),L2=$LENGTH(SUBSTR)
  NEW PARTB SET PARTB=$EXTRACT(STR,L1-L2+1,L1)
  QUIT (PARTB=SUBSTR)    
  ;
SUBASCII(STR)  ;"TAKES INPUT OF 'AAC' AND RETURNS 'AAB' 
  ;"(useful for finding string just before input, to $ORDER to STR)
  SET STR=$GET(STR) IF STR="" QUIT ""
  NEW RESULT SET RESULT=$EXTRACT(STR,1,$LENGTH(STR)-1)
  NEW CH SET CH=$EXTRACT(STR,$LENGTH(STR)),CH=$CHAR($ASCII(CH)-1)
  SET RESULT=RESULT_CH
  QUIT RESULT
  ;  
MIDSTRCOLOR(TEXT,START,LEN) ;"SIMILAR to MidStr(), but skipping over {{color}} tags
  ;"Exmple: TEXT = 'hello {{red}} world {{blue}} and stars'
  ;"                         ^-- this is pos 10
  ;"                00000000011111111112222222222333333333
  ;"                12345678901234567890123456789012345678
  ;"        START 10
  ;"        LEN = 9999
  ;"Result: ' world  and stars'
  NEW RESULT SET RESULT=""
  NEW IDX
  NEW NONCOLORPOS SET NONCOLORPOS=0
  NEW INCOLOR SET INCOLOR=0
  FOR IDX=1:1 DO  QUIT:(IDX>=$LENGTH(TEXT))!(LEN=0)
  . NEW CH SET CH=$EXTRACT(TEXT,IDX)
  . NEW CH2 SET CH2=$EXTRACT(TEXT,IDX+1)
  . IF CH="{",CH2="{" DO  QUIT
  . . SET INCOLOR=1,IDX=IDX+1
  . IF CH="}",CH2="}" DO  QUIT
  . . SET INCOLOR=0,IDX=IDX+1
  . IF INCOLOR=0 SET NONCOLORPOS=NONCOLORPOS+1
  . IF NONCOLORPOS<START QUIT
  . IF INCOLOR QUIT
  . SET RESULT=RESULT_CH
  . SET LEN=LEN-1
  QUIT RESULT
  ;  