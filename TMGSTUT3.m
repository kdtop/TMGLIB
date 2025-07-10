TMGSTUT3 ;TMG/kst/SACC Compliant String Util Lib ;9/20/17, 11/24/24
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
  ;"$$NEEDEDWS(S,SPECIALINDENT,INDENT) --NEEDED WHITE SPACE
  ;"$$NUMLWS(S) -- NUM LEFT WHITE SPACE
  ;"$$MAKEWS(N) -- Return a whitespace string that is n characters long
  ;"$$REPLSTR(STR,MATCH,NEWVAL) --REPLACE STRING: look for all instances of MATCH in STR, and replace with NEWVAL
  ;"$$STRIPARR(REF,STR) --Strip STR from each line of @REF array
  ;"$$REPLARR(REF,SRCHSTR,REPLSTR) -- Replace each instance of SRCHSTR with REPLSTR from each line of @REF array
  ;"$$MATCHXTR(STR,DIVCH,GROUP,MAP) -- Extract a string bounded by DIVCH, honoring matching encapsulators
  ;"MAPMATCH(STR,MAP) -- map a string with nested braces, parentheses etc (encapsulators)
  ;"MAPMATCH2(STR,MAP,ENCAPS) -- MAP MATCHING ENCAPSULATORS, allowing multi-char, arbitarily-paired encapsulators
  ;"$$EXTRACTMATCHED(STR,POS,ENCAPS,ENDPOS)  --Extract substring, bound by encapsulators, starting at POS
  ;"$$STRPOSMATCH(STR,SUBSTR,POS)  --Does STR match SUBSTR, starting at POS?
  ;"MAPMATCH3(STR,MAPREF,ENCAPS) --MAP MATCHING ENCAPSULATORS, allowing multi-char, arbitarily-paired encapsulators, honoring nesting and ignoring encapsulators in strings.    
  ;"$$QTPROTCT(STR)-- Protects quotes by converting all quotes to double quotes
  ;"$$UNQTPROT(STR) --Reversed quotes protection by converting all double quotes to single quotes
  ;"$$ISALPHNUM(CH) -- is character alphanumeric?
  ;"$$ISNUM(STR) -- Return IF STR is numeric
  ;"$$NUMSTR(STR,PARTB)  --Return numeric of string, and residual back in PARTB  
  ;"$$EXTRACTNUM(STR) --Extract numbers scattered through string, exluding any non-number.
  ;"$$RANDSTR(LEN,FLAGS,EXCLUDE) --Output a random string of given length, with options
  ;"$$TRIM2NUM(STR) --Trim of anything in string up to, but not including, a number
  ;"$$NUMAFTERLABEL(STR,LABEL) -- Return number following label. 
  ;"$$STRIPCMD(STR)  -- Strip command characters
  ;"$$POS(SUBSTR,S,COUNT)  ;return the beginning position of SUBSTR in S
  ;"$$POSSET(STR,SUBSTRSET,STARTPOS) --POSITION OF CHARACTER FROM SET -- different from $$POS()
  ;"$$INQT(STR,POS)  -- In Quote?
  ;"$$ENDQTPOS(STR,P1) -- return position of closing quotes 
  ;"$$GETWORD(STR,POS,OPENDIV,CLOSEDIV) -- Extract a word from a sentance, bounded by OPENDIV,CLOSEDIV 
  ;"$$NEXTTOKN(STR) --GET NEXT TOKEN
  ;"$$NEXTWORD(STR,DIVCHS) --Get next word, based on first found divisor character 
  ;"$$PIECE2(STR,DIVCHS,IDX,IDX2,DIVUSED) Get indexed word, based on first found divisor character       
  ;"$$NEXTFRAG(STR,STARTPOS,FRAGS) --Get first next character (or string fragment), matching from array of possible inputs.
  ;"$$NEXTCH(STR,STARTPOS,A,B,C,D,E,F,G) --Get first next char (or string fragment), matching from 7 possible inputs.
  ;"$$LMATCH(STR,SUBSTR,CASESPEC) - Does left part of STR match SUBSTR?
  ;"$$RMATCH(STR,SUBSTR,CASESPEC) - Does right part of STR match SUBSTR?  
  ;"$$SUBASCII(STR)  --TAKES INPUT OF AAC AND RETURNS AAB (useful for finding just before, to $ORDER to STR)
  ;"$$UNICODEMIDSTR(TEXT,START,LEN)  --Unicode aware $MID() function  
  ;"depreciated --> $$MIDSTRCOLOR(TEXT,START,LEN) -- similar to MidStr(), but skipping over {{color}} tags
  ;"$$MKSTRMID(TEXT,START,LEN,TAGSTART,TAGEND,OPTION) --MARKUP-STR-MID().  Like MidStr() or $EXTRACT(), but for Markup strings
  ;"$$MKSTRLEN(TEXT,TAGSTART,TAGEND) --Length of Markup string (excluding tags)
  ;"$$MKSTRLTRIM(TEXT,NUM,TAGSTART,TAGEND) --MARKUP-STR-LTRIM().   
  ;"$$MKSTRRTRIM(TEXT,NUM,TAGSTART,TAGEND) --MARKUP-STR-RTRIM(). 
  ;"FOREACHTAG(TEXT,TAGSTART,TAGEND,HNDTAG) -- Cycle through each tag, calling callback Fn HNDTAG  
  ;"$$FINDDT(TEXT,SPOS,OUT,SPT,EPT) --Find date in TEXT, starting at option SPOS, return value found in DTOUT
  ;"SUBSTRMATCH(SUBSTR,STR,MATCH,SUBSTRARR,STRARR) -- Get match info of substring in string, return results in MATCH
  ;"SCANDATES(STR,OUT) -- return positions of dates, in format of ##/##/#### etc
  ;"=======================================================================
  ;" Private Functions.
  ;"=======================================================================
  ;"$$NEEDEDWS(S,SPECIALINDENT,INDENT) -- create white space need for wrapped lines
  ;"GETMATCHENCAP(OUT,ENCAP) ;  
  ;"INTAG(POS,MAP) --Based on MAP from MAPMATCH2, is POS inside a non-nested encapsulator?  
  ;"$$WIDECAPS(ENCAPS) --Returns 1 if any of the encapsulators is 'wide', i.e. multi-char (e.g. '{{')
  ;"$$CHISENCAP(CH,ENCAPS,OPEN,CLOSE) --Does CH map (or partly match) an encapsulator 
  ;"PARSESTR(STR,ARR) -- PARSE STRING -- for use in SUBSTRMATCH()
  ;"=======================================================================
  ;" Test Functions
  ;"=======================================================================
  ;"TESTMM2 ;
  ;"TESTEXTRACTMATCHED ;
  ;"TESTMM3 ;  
  ;"TESTISNUM ;"   
  ;"TESTNUMSTR ;" 
  ;"TESTUCMID ;  
  ;"TESTMKSM ;  
  ;"TESTMKLTRIM ;  
  ;"TESTMKRTRIM ;  
  ;"TESTFE ;  
  ;"TESTCALLBACK(TEXT,ATAG,POS) --CALLBACK FOR TESTFE
  ;"TESTSS  -- TEST SUBSTRMATCH()
  ;"TESTSCNDT  -- Test SCANDATES
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
MATCHXTR(STR,DIVCH,GROUP,MAP,ALLOWEDENCAPS) ;"MATCH EXTRACT
  ;"NOTE: see also EXTRACTMATCHED(STR,POS,ENCAPS,ENDPOS) <-- this make be a remake of same function
  ;"Purpose to extract a string bounded by DIVCH, honoring matching encapsulators
  ;"Note: the following markers are honored as paired encapsulators:
  ;"      ( ),  { },  | |,  < >,  # #, [ ], " ", ' '
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
  ;"       ALLOWEDENCAPS -- OPTIONAL.  A string of allowed opening encapsulators (allows others to be ignored)
  ;"                  e.g. "{(|"  <-- will cause "<>#[]" to be ignored
  ;"Results: Returns extracted string.
  ;"NOTE: If ALLOWEDENCAPS are not provided, and defaults to multiple encapsulators, and if
  ;"      multiple instances of encapsulators are found in string, can cause problems.  
  ;"      It is best to specify just one encapsulator.  
  IF $DATA(MAP)=0 DO MAPMATCH(STR,.MAP,.ALLOWEDENCAPS)
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
MAPMATCH(STR,MAP,ALLOWEDENCAPS)  ;"MAP MATCHING ENCAPSULATORS.  NOTE: See also MAPMATCH2() for extra functionality
  ;"Purpose to map a string with nested braces, parentheses etc (encapsulators)
  ;"Note: the following markers are honored as paired encapsulators:
  ;"      ( ),  { },  | |,  < >,  # #,  " ", ' ', [ ]
  ;"Input: STR -- string to evaluate
  ;"       MAP -- PASS BY REFERENCE.  An OUT PARAMETER.  Prior values are killed.  Format:
  ;"           MAP(GROUP,Depth)=OpeningSymbol
  ;"           MAP(GROUP,Depth,"Pos",1)=index of opening symbol
  ;"           MAP(GROUP,Depth,"Pos",2)=index of paired closing symbol
  ;"       ALLOWEDENCAPS -- OPTIONAL.  A string of allowed opening encapsulators (allows others to be ignored)
  ;"                  e.g. "{(|"  <-- will cause "<>#[]" to be ignored
  ;"E.g.  STR="Hello there (friend)"
  ;"           MAP(1,1)="("         <-- encapsulator found
  ;"           MAP(1,1,"Pos",1)=13  <-- start pos
  ;"           MAP(1,1,"Pos",2)=20  <-- end pos
  ;"E.g. STR="Hello there (friend), what are you doing (today)?"
  ;"           MAP(1,1)="("         <-- first group encapsulator
  ;"           MAP(1,1,"Pos",1)=13
  ;"           MAP(1,1,"Pos",2)=20
  ;"           MAP(2,1)="("         <-- second group encapsulator
  ;"           MAP(2,1,"Pos",1)=42
  ;"           MAP(2,1,"Pos",2)=48
  ;"E.g.  STR="Hello |There{|friend|}|"
  ;"           MAP(1,1)="|"        <-- first group encapsulator
  ;"           MAP(1,1,"Pos",1)=7
  ;"           MAP(1,1,"Pos",2)=23
  ;"           MAP(1,2)="{"        <-- inside group 1, we have a subgroup, depth=2
  ;"           MAP(1,2,"Pos",1)=13
  ;"           MAP(1,2,"Pos",2)=22
  ;"           MAP(1,3)="|"        <-- inside group 1, we have a sub-subgroup, depth=3
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
  ;"E.g. STR="Hello There |Friend { and | neighbors }" <-- notice that nesting is inconsistent/broken    
  ;"           MAP(1,1)="|"          <-- First group encapsulator
  ;"           MAP(1,1,"Closer")="|"
  ;"           MAP(1,1,"Pos",1)=13   <-- Opening position found, but closing not found      
  ;"           MAP(1,2)="{"          <-- inside group 1, we find start of another encapsulator, depth=2 
  ;"           MAP(1,2,"Closer")="}"
  ;"           MAP(1,2,"Pos",1)=21   <-- Opening position found, but closing not found   
  ;"           MAP(1,3)="|"          <-- NOTE: this is NOT seen as match to starting "|" because it is inside {}
  ;"           MAP(1,3,"Closer")="|" 
  ;"           MAP(1,3,"Pos",1)=27   <-- Interpreted as another opener, and no matching closer found                     
  ;"Results: none
  SET ALLOWEDENCAPS=$GET(ALLOWEDENCAPS,"({|<""#'[")
  NEW MATCH,DEPTH,IDX,GROUP
  DO GETMATCHENCAP(.MATCH,ALLOWEDENCAPS) 
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
GETMATCHENCAP(OUT,ALLOWED) ;  
  IF ALLOWED["(" SET OUT("(")=")"
  IF ALLOWED["{" SET OUT("{")="}"
  IF ALLOWED["|" SET OUT("|")="|"
  IF ALLOWED["<" SET OUT("<")=">"
  IF ALLOWED["#" SET OUT("#")="#"
  IF ALLOWED["""" SET OUT("""")=""""
  IF ALLOWED["'" SET OUT("'")="'"
  IF ALLOWED["[" SET OUT("[")="]"
  QUIT
  ;  
WIDECAPS(ENCAPS) ;"Returns 1 if any of the encapsulators is 'wide', i.e. multi-char (e.g. '{{')
  ;"INPUT:  ENCAPS -- array with encapsulators
  ;"           e.g. ENCAPS("{{")="}}"
  ;"           e.g. ENCAPS("<")=">"
  ;"           e.g. ENCAPS("ABC")="XYZ"
  NEW RESULT SET RESULT=0
  NEW CAP SET CAP=""
  FOR  SET CAP=$ORDER(ENCAPS(CAP)) QUIT:(CAP="")!(RESULT=1)  DO
  . IF $LENGTH(CAP)>1 SET RESULT=1 QUIT
  . IF $LENGTH(ENCAPS(CAP))>1 SET RESULT=1 QUIT
  QUIT RESULT
  ;
CHISENCAP(CH,ENCAPS,OPEN,CLOSE) ;"Does CH map (or partly match) an encapsulator
  ;"INPUT: CH -- CHAR TO TEST
  ;"       ENCAPS -- array with encapsulators
  ;"           e.g. ENCAPS("{{")="}}"
  ;"           e.g. ENCAPS("<")=">"
  ;"           e.g. ENCAPS("ABC")="XYZ"
  ;"       OPEN - OPTIONAL.  If passed by reference, filled with matched open encapsulator (if any)
  ;"       CLOSE- OPTIONAL.  If passed by reference, filled with matched close encapsulator (if any)
  ;"Result:  1  - matches against a single char opening encapsulator
  ;"        -1  - matches against a single char closing encapsulator
  ;"         2  - matching aginst the first character of multi-char opening encapsualtor
  ;"        -2  - matching aginst the first character of multi-char closing encapsualtor
  ;"         0  - no match
  ;
  NEW RESULT SET RESULT=0
  NEW CAP SET CAP=""
  FOR  SET CAP=$ORDER(ENCAPS(CAP)) QUIT:(CAP="")!(RESULT'=0)  DO
  . SET OPEN=CAP
  . SET CLOSE=$GET(ENCAPS(CAP))
  . IF $LENGTH(OPEN)=1 DO  QUIT:RESULT'=0
  . . IF CH'=OPEN QUIT
  . . SET RESULT=1
  . ELSE  DO  QUIT:RESULT'=0
  . . IF $EXTRACT(OPEN,1)'=CH QUIT
  . . SET RESULT=2
  . IF $LENGTH(CLOSE)=1 DO  QUIT:RESULT'=0
  . . IF CH'=CLOSE QUIT
  . . SET RESULT=-1
  . ELSE  DO  QUIT:RESULT'=0
  . . IF $EXTRACT(CLOSE,1)'=CH QUIT
  . . SET RESULT=-2
  IF RESULT=0 SET (OPEN,CLOSE)=""
  QUIT RESULT
  ;      
TESTMM2 ;
  ;"SET X="Hello there (Friend) and (Neighbor)"
  SET X="Hello |There{|friend|}|"
  NEW MAP
  DO MAPMATCH2(X,.MAP)           
  QUIT
  ;
MAPMATCH2(STR,MAP,ENCAPS)  ;" MAP MATCHING ENCAPSULATORS, allowing multi-char, arbitarily-paired encapsulators
  ;"NOTE: See also MAPMATCH3 for a different type of matching.  
  ;"Purpose to map a string with nested braces, parentheses etc (encapsulators)
  ;"Note: the following markers are honored as paired encapsulators:
  ;"      ( ),  { },  | |,  < >,  # #,  " "
  ;"Input: STR -- string to evaluate
  ;"       MAP -- PASS BY REFERENCE.  An OUT PARAMETER.  Prior values are killed.  Format:
  ;"           MAP(GROUP,Depth)=OpeningFrag^ClosingFrag 
  ;"           MAP(GROUP,Depth,"Pos",1)=index of opening symbol
  ;"           MAP(GROUP,Depth,"Pos",2)=index of paired closing symbol
  ;"       ENCAPS -- OPTIONAL.  ARRAY of opening and closing encapsulators.  Format:
  ;"           ENCAPS(<OpenEncapsulator>)=<CloseEncapsulator>
  ;"           e.g. ENCAPS("{{")="}}"
  ;"           e.g. ENCAPS("<")=">"
  ;"           e.g. ENCAPS("ABC")="XYZ"
  ;"           If not provided, then default is ( ),  { },  | |,  < >,  # #,  " ", ' ', [ ]
  ;"NOTE: Seems to fail at properly mapping this string: {"0":0,"1":{"%%node_value%%":1,"2":{"3":0}},"2":2}
  NEW TEMPSTR SET TEMPSTR=STR
  IF $DATA(ENCAPS)=0 DO GETMATCHENCAP(.ENCAPS,"({|<""#'[")
  NEW GROUP,DEPTH SET GROUP=1,DEPTH=0
  NEW IDX,CLSRS,ACLSR SET IDX="" 
  FOR  SET IDX=$ORDER(ENCAPS(IDX)) QUIT:IDX=""  DO
  . SET ACLSR=$GET(ENCAPS(IDX)) QUIT:ACLSR="" 
  . SET CLSRS(ACLSR)=IDX
  NEW TAGS MERGE TAGS=ENCAPS,TAGS=CLSRS
  NEW TAG,TAGPOS
  NEW LEN SET LEN=$LENGTH(TEMPSTR)
  NEW OPENTAG,CLOSETAG,OPENPOS,CLOSEPOS
  NEW POS SET POS=1
  NEW DONE SET DONE=0
  FOR  DO  QUIT:DONE  
  . SET OPENTAG=$$NEXTFRAG(TEMPSTR,POS,.ENCAPS,.OPENPOS)
  . SET CLOSETAG=$$NEXTFRAG(TEMPSTR,POS,.CLSRS,.CLOSEPOS)
  . IF OPENPOS>0,(OPENPOS<CLOSEPOS)!(CLOSEPOS=0) DO
  . . SET TAG=OPENTAG,TAGPOS=OPENPOS
  . ELSE  IF CLOSEPOS>0,(CLOSEPOS<OPENPOS)!(OPENPOS=0) DO
  . . SET TAG=CLOSETAG,TAGPOS=CLOSEPOS
  . ELSE  IF (OPENPOS=0)&(CLOSEPOS=0) DO  QUIT
  . . SET DONE=1
  . ELSE  IF OPENPOS=CLOSEPOS,OPENPOS>0 DO
  . . SET TAG=OPENTAG,TAGPOS=OPENPOS
  . ELSE  DO  QUIT
  . . SET DONE=1
  . SET POS=TAGPOS+1 
  . IF TAG=CLOSETAG,TAG=$GET(MAP(GROUP,DEPTH,"Closer")) DO  QUIT
  . . SET MAP(GROUP,DEPTH,"Pos",2)=TAGPOS
  . . KILL MAP(GROUP,DEPTH,"Closer")
  . . SET DEPTH=DEPTH-1
  . . IF DEPTH=0 SET GROUP=GROUP+1
  . SET DEPTH=DEPTH+1
  . NEW MATCHCLOSER SET MATCHCLOSER=$GET(ENCAPS(TAG))
  . SET MAP(GROUP,DEPTH)=TAG_"^"_MATCHCLOSER 
  . SET MAP(GROUP,DEPTH,"Closer")=MATCHCLOSER
  . SET MAP(GROUP,DEPTH,"Pos",1)=TAGPOS
  QUIT
  ;  
EXTRACTMATCHED(STR,POS,ENCAPS,ENDPOS)  ;"Extract substring, bound by encapsulators, starting at POS
  ;"NOTE: see also MATCHXTR(STR,DIVCH,GROUP,MAP,ALLOWEDENCAPS) <-- may be duplicate function.  
  ;"               MATCHXTR first maps out the entire string.  This function goes left to right.  
  ;"Input: STR -- string to evaluate
  ;"       POS -- starting position in STR.  Must be position of opening encapsulator, or starting position of multichar encapsulator.  
  ;"       ENCAPS -- OPTIONAL.  ARRAY of opening and closing encapsulators.  Format:
  ;"           ENCAPS(<OpenEncapsulator>)=<CloseEncapsulator>
  ;"           e.g. ENCAPS("{{")="}}"
  ;"           e.g. ENCAPS("<")=">"
  ;"           e.g. ENCAPS("ABC")="XYZ"
  ;"           If not provided, then default is ( ),  { },  | |,  < >,  # #,  " ", ' ', [ ]
  ;"       ENDPOS -- OPTIONAL.  If passed by reference, then will be filled with pos, from STR, immediately following closing encapsulator
  ;"Result: returns string, or ""
  NEW RESULT SET RESULT=""
  NEW ABORT SET ABORT=0
  NEW QT1 SET QT1="'"
  NEW QT2 SET QT2=""""
  NEW QTS SET QTS=QT1_QT2
  NEW INQT SET INQT=0 NEW CLOSEQT SET CLOSEQT="" 
  NEW STARTPOS SET STARTPOS=+$GET(POS) IF STARTPOS'>0 SET STARTPOS=1
  SET ENDPOS=0
  IF $DATA(ENCAPS)=0 DO GETMATCHENCAP^TMGSTUT3(.ENCAPS,"({|<""#'[")
  NEW WIDE SET WIDE=$$WIDECAPS^TMGSTUT3(.ENCAPS) ;"Returns 1 if any of the encapsulators is 'wide', i.e. multi-char (e.g. '{{')
  NEW CH SET CH=$EXTRACT(STR,POS)
  NEW OPEN,CLOSE
  NEW TEST SET TEST=$$CHISENCAP^TMGSTUT3(CH,.ENCAPS,.OPEN,.CLOSE)  ;"Does CH map (or partly match) an encapsulator
  IF TEST'>0 GOTO EMDN     ;"starting position must be on an encapsulator
  IF TEST=2 DO  GOTO:ABORT EMDN
  . SET ABORT=($$STRPOSMATCH(STR,OPEN,POS)=0)
  NEW OPENISQT SET OPENISQT=(QTS[OPEN)  ;"Is the encapsulator itself a quote char?
  IF OPENISQT DO   ;"Remove the active encapsulator from QTS string.
  . SET QTS=""
  . ;"IF OPEN=QT1 SET QTS=QT2
  . ;"ELSE  IF OPEN=QT2 SET QTS=QT1
  SET POS=POS+$LENGTH(OPEN)  ;"should be positioned directly AFTER opening. 
  ;"Loop through all chars until close encountered, ignoring if inside quotes (single or double), and counting depth
  NEW DEPTH SET DEPTH=1
  NEW DONE SET DONE=0
  ;"Example:  He said, "You must 'love' one another." OK?
  FOR POS=POS:1:$LENGTH(STR) QUIT:DONE  DO
  . NEW CH SET CH=$EXTRACT(STR,POS)
  . NEW HANDLED SET HANDLED=0
  . IF QTS[CH DO  QUIT:HANDLED
  . . IF INQT DO
  . . . IF CH=CLOSEQT DO
  . . . . SET INQT=0,CLOSEQT="",HANDLED=1
  . . ELSE  DO
  . . . IF CH=CLOSE QUIT   ;"e.g. abc'123'def  If started at pos=3, then ' at 7 is closer. Handle below
  . . . SET INQT=1,CLOSEQT=CH,HANDLED=1
  . IF INQT QUIT  ;"ignore everything until done with quotes
  . NEW MATCH SET MATCH=$$STRPOSMATCH(.STR,CLOSE,POS)
  . IF MATCH=1 DO  QUIT
  . . SET DEPTH=DEPTH-1
  . . IF DEPTH=0 SET ENDPOS=POS,DONE=1
  . SET MATCH=$$STRPOSMATCH(.STR,OPEN,POS)  ;"are we starting a new nested opening match?
  . IF MATCH=1 DO
  . . SET DEPTH=DEPTH+1
  . . SET POS=POS+$LENGTH(OPEN)-1
  IF ENDPOS>STARTPOS DO
  . SET RESULT=$EXTRACT(STR,STARTPOS,ENDPOS)
  . SET ENDPOS=ENDPOS+1  ;"position cursor to be after match.  
EMDN ;  
  QUIT RESULT
  ;
TESTEXTRACTMATCHED ;
  ;"NEW STR SET STR="{""0"":0,""1"":{""%%node_value%%"":1,""2"":{""3"":0}},""2"":2}"
  NEW STR SET STR=">""That's all folks"" is the best."
  NEW POS SET POS=2
  NEW TEST SET TEST=$$EXTRACTMATCHED(STR,POS)
  WRITE TEST,!
  QUIT
  ;
TESTMATCHXTR ;
  NEW STR SET STR="{""@"":0,""1"":{""%%node_value%%"":1,""2"":{""3"":0}},""2"":2}"
  ;"NEW STR SET STR="{There {nested braces} friend}"
  ;"NEW STR SET STR="""There {nested braces} friend"""
  NEW POS SET POS=2
  NEW TEST SET TEST=$$MATCHXTR(STR,$E(STR,POS),,,"""")  ;"<--- doesn't work correctly unless encapsulator specified
  WRITE TEST,!
  QUIT
  ;
STRPOSMATCH(STR,SUBSTR,POS)  ;"Does STR match SUBSTR, starting at POS?
  NEW LEN SET LEN=$LENGTH(SUBSTR)
  NEW TEST SET TEST=$EXTRACT(STR,POS,POS+LEN-1)
  NEW RESULT SET RESULT=(TEST=SUBSTR)
  QUIT RESULT
  ;
MAPMATCH3(STR,MAPREF,ENCAPS)  ;" MAP MATCHING ENCAPSULATORS, allowing multi-char, arbitarily-paired encapsulators, 
  ;"                          honoring nesting and ignoring encapsulators in strings.  
  ;"Purpose to map a string with nested braces, parentheses etc (encapsulators)
  ;"Note: the following markers are honored as paired encapsulators:
  ;"      ( ),  { },  | |,  < >,  # #,  " ",  ' ', [ ]
  ;"Input: STR -- string to evaluate
  ;"       MAP -- PASS BY NAME.  An OUT PARAMETER.  Prior values are killed.  Format:
  ;"          MAP(1)=<PRE TEXT WITHOUT ANY ENCAPSULATORS>
  ;"          MAP(2)=<OPEN TAG>
  ;"          MAP(2,1)=<PRE TEXT WITHOUT ANY ENCAPSULATORS>
  ;'          MAP(2,2)=<OPEN_TAG>
  ;"          MAP(2,2,1)=<INTERVAL TEXT WITHOUT ANY ENCAPSULATORS>
  ;'          MAP(2,3)=<CLOSE_TAG>
  ;"          MAP(2,4)=<INTERVAL TEXT WITHOUT ANY ENCAPSULATORS>
  ;"          MAP(2,"XREF",#)=<STRING POSITION IN SUBSTRING>
  ;"          MAP(2,"STR")=<FRAGMENT SUBSTRING>
  ;'          MAP(3)=<CLOSE_TAG>
  ;"          MAP(4)=<INTERVAL TEXT WITHOUT ANY ENCAPSULATORS>
  ;"          MAP(5)=<OPEN TAG>
  ;"          MAP(5,1)=<INTERVAL TEXT WITHOUT ANY ENCAPSULATORS>
  ;"          MAP(5,"XREF",#)=<STRING POSITION IN SUBSTRING>
  ;"          MAP(5,"STR")=<FRAGMENT SUBSTRING>
  ;"          MAP(6)=<CLOSE TAG>
  ;"          MAP(7)=<POST TEXT WITHOUT ANY ENCAPSULATORS>
  ;"          MAP("XREF",#)=<STRING POSITION IN STRING>
  ;"          MAP("STR")=<STRING>
  ;"       ENCAPS -- OPTIONAL.  ARRAY of opening and closing encapsulators.  Format:
  ;"           ENCAPS(<OpenEncapsulator>)=<CloseEncapsulator>
  ;"           e.g. ENCAPS("{{")="}}"
  ;"           e.g. ENCAPS("<")=">"
  ;"           e.g. ENCAPS("ABC")="XYZ"
  ;"           If not provided, then default is ( ),  { },  | |,  < >,  # #,  " ", ' '
  ;"NOTE: test string: {"0":0,"1":{"%%node_value%%":1,"2":{"3":0}},"2":2}
  ;"Result: none
  NEW ALLOWEDENCAPS SET ALLOWEDENCAPS="({|<""#'["
  IF $DATA(ENCAPS)=0 DO GETMATCHENCAP^TMGSTUT3(.ENCAPS,"({|<""#'[")
  NEW OPENTAG,CLOSETAG,OPENPOS,CLOSEPOS
  NEW PARTA,PARTB,PARTC
  NEW LEN SET LEN=$LENGTH(STR)
  SET @MAPREF@("STR")=STR
  NEW IDX SET IDX=0
  NEW POS SET POS=1
  NEW DONE SET DONE=0
  FOR  DO  QUIT:DONE
  . SET OPENTAG=$$NEXTFRAG(STR,POS,.ENCAPS,.OPENPOS)
  . IF OPENTAG'="" DO
  . . NEW PARTA SET PARTA=$EXTRACT(STR,POS,OPENPOS-1)
  . . NEW PARTB,ENDPOS SET PARTB=$$EXTRACTMATCHED(STR,OPENPOS,.ENCAPS,.ENDPOS)
  . . IF PARTB="" SET DONE=1 QUIT  ;"error state
  . . NEW CLOSETAG SET CLOSETAG=$GET(ENCAPS(OPENTAG))
  . . IF CLOSETAG="" SET DONE=1 QUIT  ;"error state
  . . SET PARTB=$EXTRACT(PARTB,$LENGTH(OPENTAG)+1,$LENGTH(PARTB)-$LENGTH(CLOSETAG))
  . . NEW PARTC SET PARTC=$EXTRACT(STR,ENDPOS,$LENGTH(STR))
  . . IF PARTA'="" SET @MAPREF@($I(IDX))=PARTA
  . . SET @MAPREF@($I(IDX))=OPENTAG
  . . IF PARTB'=""  DO
  . . . NEW SUBREF SET SUBREF=$NAME(@MAPREF@(IDX))
  . . . NEW SUBENCAPS MERGE SUBENCAPS=ENCAPS
  . . . IF OPENTAG="""" KILL SUBENCAPS("'")
  . . . IF OPENTAG="'" KILL SUBENCAPS("""")
  . . . DO MAPMATCH3(PARTB,SUBREF,.SUBENCAPS)
  . . . SET PARTB=""
  . . SET @MAPREF@($I(IDX))=CLOSETAG
  . . SET STR=PARTC,POS=1,LEN=$LENGTH(STR),PARTC=""
  . . ;
  . ELSE  DO
  . . SET @MAPREF@($I(IDX))=STR
  . . SET DONE=1
  . IF POS>LEN SET DONE=1
  QUIT
  ;  
TESTMM3 ;
  NEW STR SET STR="{""0"":0,""1"":{""%%node_value%%"":1,""2"":{""3"":0}},""2"":2}"
  NEW MAP DO MAPMATCH3(STR,"MAP")
  ZWR MAP
  ;"MAP(1)="{"
  ;"MAP(1,1)=""""
  ;"MAP(1,1,1)=0
  ;"MAP(1,1,"STR")=0
  ;"MAP(1,2)=""""
  ;"MAP(1,3)=":0,"
  ;"MAP(1,4)=""""
  ;"MAP(1,4,1)=1
  ;"MAP(1,4,"STR")=1
  ;"MAP(1,5)=""""
  ;"MAP(1,6)=":"
  ;"MAP(1,7)="{"
  ;"MAP(1,7,1)=""""
  ;"MAP(1,7,1,1)="%%node_value%%"
  ;"MAP(1,7,1,"STR")="%%node_value%%"
  ;"MAP(1,7,2)=""""
  ;"MAP(1,7,3)=":1,"
  ;"MAP(1,7,4)=""""
  ;"MAP(1,7,4,1)=2
  ;"MAP(1,7,4,"STR")=2
  ;"MAP(1,7,5)=""""
  ;"MAP(1,7,6)=":"
  ;"MAP(1,7,7)="{"
  ;"MAP(1,7,7,1)=""""
  ;"MAP(1,7,7,1,1)=3
  ;"MAP(1,7,7,1,"STR")=3
  ;"MAP(1,7,7,2)=""""
  ;"MAP(1,7,7,3)=":0"
  ;"MAP(1,7,7,"STR")="""3"":0"
  ;"MAP(1,7,8)="}"
  ;"MAP(1,7,"STR")="""%%node_value%%"":1,""2"":{""3"":0}"
  ;"MAP(1,8)="}"
  ;"MAP(1,9)=","
  ;"MAP(1,10)=""""
  ;"MAP(1,10,1)=2
  ;"MAP(1,10,"STR")=2
  ;"MAP(1,11)=""""
  ;"MAP(1,12)=":2"
  ;"MAP(1,"STR")="""0"":0,""1"":{""%%node_value%%"":1,""2"":{""3"":0}},""2"":2"
  ;"MAP(2)="}"
  ;"MAP("STR")="{""0"":0,""1"":{""%%node_value%%"":1,""2"":{""3"":0}},""2"":2}"    
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
  SET DIVUSED=$$NEXTFRAG(STR,0,.DIVCHS)
  NEW RESULT SET RESULT=$PIECE(STR,DIVUSED,IDX,IDX2)
  QUIT RESULT
  ;
NEXTFRAG(STR,STARTPOS,FRAGS,FOUNDPOS) ;"Get first next character (or string fragment), matching from array of possible inputs.  
  ;"Purpose: Check string to determine which string fragment comes first and return it
  ;"INPUTS: STR -- the string to check
  ;"        STARTPOS -- the index to start $FIND at, default is 0
  ;"        FRAGS. PASS BY REFERENCE.  Format:
  ;"          FRAG("test string1")=""
  ;"          FRAG("test string2")=""
  ;"        FOUNDPOS -- OPTIONAL  An OUT parameter.  Position of resulting frag
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
  SET FOUNDPOS=+$GET(POS(MINIDX))
  NEW RESULT SET RESULT=$GET(POS(MINIDX,"TEST"))
  QUIT RESULT
  ;      
NEXTCH(STR,STARTPOS,A,B,C,D,E,F,G,H,I) ;"Get first next character (or string fragment), matching from 9 possible inputs.  
  ;"Purpose: Check string to determine which string fragment comes first and return it
  ;"INPUTS: STR -- the string to check
  ;"        STARTPOS -- the index to start $FIND at, default is 0
  ;"        A..I the inputs to test for.  
  ;"Results: returns which of inputs is found first, or "" if none found.
  SET A=$GET(A),B=$GET(B),C=$GET(C),D=$GET(D),E=$GET(E),F=$GET(F),G=$GET(G),H=$GET(H),I=$GET(I)
  NEW FRAGS                  SET:A'="" FRAGS(A)=""      
  SET:B'="" FRAGS(B)=""      SET:C'="" FRAGS(C)=""      
  SET:D'="" FRAGS(D)=""      SET:E'="" FRAGS(E)=""      
  SET:F'="" FRAGS(F)=""      SET:G'="" FRAGS(G)=""
  SET:H'="" FRAGS(H)=""      SET:I'="" FRAGS(I)=""
  NEW RESULT SET RESULT=$$NEXTFRAG(.STR,.STARTPOS,.FRAGS)
  QUIT RESULT      
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
TESTUCMID ;
  NEW STR 
  ;"                                      1       111
  ;"       123    4        5678    9      0       123
  SET STR="abc%UC%$2501%UC%defg%UC%$2501;$3405%UC%xyz"
  WRITE $$UNICODEMIDSTR(STR,1,3),!   ;"Expect 'abc'
  WRITE $$UNICODEMIDSTR(STR,2,5),!   ;"Expect 'bc%UC%$2501%UC%de'
  WRITE $$UNICODEMIDSTR(STR,4,2),!   ;"Expect '%UC%$2501%UC%d'
  WRITE $$UNICODEMIDSTR(STR,9,2),!   ;"Expect '%UC%$2501;$3405%UC%'
  WRITE $$UNICODEMIDSTR(STR,10,4),!  ;"Expect '%UC%$3405%UC%xyz'
  WRITE $$UNICODEMIDSTR(STR,11,5),!  ;"Expect 'xyz'
  SET STR="$2501;$2502;$2503;$2504;$2505;$2506;"
  WRITE $$UNICODEMIDSTR(STR,3,2),!   ;"Expect '$2503;$2504;'
  QUIT  
  ;
UNICODEMIDSTR(TEXT,START,LEN)  ;"Unicode aware $MID() function
  ;"Input: TEXT -- Text with optional included unicode chars.  Format: 'abcdefg%UC%$2501;$3405%UC%xyz'
  ;"               '%UC% will delimit plain text vs UniCode.  
  ;"               ALSO, if entire string is unicode, then %UC$ is not needed. 
  ;"               Unicode chars will be hex, starting with $, and multiple chars are ';' delimited.
  ;"       START -- Starting position. This will be unicode aware.  I.e. '$2501' is treated as length=1, 
  ;"                and %UC% or ';' delimiters are not counted 
  ;"       LEN -- Number of chars to return (again, not counting delimiters).  If LEN>actual length, no extra returned
  ;"RESULT: returns desired portion of string, with delimiters maintained.  
  NEW UCTAG SET UCTAG="%UC%"
  NEW TAGFOUND SET TAGFOUND=(TEXT[UCTAG)
  NEW RESULT SET RESULT=""
  NEW RESULTMODE SET RESULTMODE="NORM"
  NEW PARTIDX FOR PARTIDX=1:1:$LENGTH(TEXT,UCTAG) QUIT:(LEN<=0)  DO
  . ;"NOTE if PARTIDX is ODD, then dealing with normal chars, if EVEN, then UNICODE -- even if string STARTS with %UC%
  . ;"  But if TEXT didn't include %UC%, then entire string could be unicode.  
  . NEW APART SET APART=$PIECE(TEXT,UCTAG,PARTIDX) QUIT:APART=""
  . NEW PARTISUNICODE IF TAGFOUND,(APART["%UC%") SET PARTISUNICODE=(IDX#2=0)
  . ELSE  SET PARTISUNICODE=$$ISUNICODE^TMGSTUTL(APART)
  . IF PARTISUNICODE DO
  . . IF $$ISUNICODE^TMGSTUTL(APART)=0 SET LEN=0 QUIT  ;"erroneous state, abort
  . . NEW PARTLEN SET PARTLEN=$LENGTH(APART,";")
  . . NEW JDX FOR JDX=1:1:PARTLEN QUIT:(LEN<1)  DO
  . . . NEW ACHAR SET ACHAR=$PIECE(APART,";",JDX)
  . . . IF START>1 SET START=START-1 QUIT
  . . . IF RESULTMODE="NORM" DO
  . . . . IF TAGFOUND SET RESULT=RESULT_UCTAG
  . . . . SET RESULTMODE="UC"
  . . . SET RESULT=RESULT_ACHAR_";"
  . . . SET LEN=LEN-1
  . . . SET START=0
  . ELSE  DO   ;"APART is normal characters
  . . NEW PARTLEN SET PARTLEN=$LENGTH(APART)
  . . IF START<=PARTLEN DO     ;"APART='abcdefg',START=4  -> 'abc' discarded and 'defg' added to RESULT
  . . . IF RESULTMODE="UC" DO
  . . . . IF TAGFOUND,(RESULT]"") SET RESULT=RESULT_UCTAG
  . . . . SET RESULTMODE="NORM"
  . . . NEW EFFECTIVESTART SET EFFECTIVESTART=$SELECT(START>0:START,1:1) 
  . . . NEW ENDPOS SET ENDPOS=EFFECTIVESTART+LEN-1 IF ENDPOS>PARTLEN SET ENDPOS=PARTLEN
  . . . SET RESULT=RESULT_$EXTRACT(APART,EFFECTIVESTART,ENDPOS),LEN=LEN-(ENDPOS-EFFECTIVESTART+1),START=0
  . . IF START>1 SET START=START-PARTLEN
  IF TAGFOUND,(RESULTMODE="UC") SET RESULT=RESULT_UCTAG
  QUIT RESULT
  ;
  ;"DELETE LATER  //kt 11/2024
  ;"DEPRECIATED -- MIDSTRCOLOR(TEXT,START,LEN) ;"SIMILAR to MidStr(), but skipping over {{color}} tags
  ;"DEPRECIATED --   ;"Exmple: TEXT = 'hello {{red}} world {{blue}} and stars'
  ;"DEPRECIATED --   ;"                         ^-- this is pos 10
  ;"DEPRECIATED --   ;"                00000000011111111112222222222333333333
  ;"DEPRECIATED --   ;"                12345678901234567890123456789012345678
  ;"DEPRECIATED --   ;"        START 10
  ;"DEPRECIATED --   ;"        LEN = 9999
  ;"DEPRECIATED --   ;"Result: ' world  and stars'
  ;"DEPRECIATED --   NEW RESULT SET RESULT=""
  ;"DEPRECIATED --   NEW IDX
  ;"DEPRECIATED --   NEW NONCOLORPOS SET NONCOLORPOS=0
  ;"DEPRECIATED --   NEW INCOLOR SET INCOLOR=0
  ;"DEPRECIATED --   FOR IDX=1:1 DO  QUIT:(IDX>=$LENGTH(TEXT))!(LEN=0)
  ;"DEPRECIATED --   . NEW CH SET CH=$EXTRACT(TEXT,IDX)
  ;"DEPRECIATED --   . NEW CH2 SET CH2=$EXTRACT(TEXT,IDX+1)
  ;"DEPRECIATED --   . IF CH="{",CH2="{" DO  QUIT
  ;"DEPRECIATED --   . . SET INCOLOR=1,IDX=IDX+1
  ;"DEPRECIATED --   . IF CH="}",CH2="}" DO  QUIT
  ;"DEPRECIATED --   . . SET INCOLOR=0,IDX=IDX+1
  ;"DEPRECIATED --   . IF INCOLOR=0 SET NONCOLORPOS=NONCOLORPOS+1
  ;"DEPRECIATED --   . IF NONCOLORPOS<START QUIT
  ;"DEPRECIATED --   . IF INCOLOR QUIT
  ;"DEPRECIATED --   . SET RESULT=RESULT_CH
  ;"DEPRECIATED --   . SET LEN=LEN-1
  ;"DEPRECIATED --   QUIT RESULT
  ;"DEPRECIATED --   ;  
TESTMKSM ;
  NEW TEXT SET TEXT="Hello {{red}} world {{blue}} and stars"
  SET OPTION("STRIP TAGS")=1
  WRITE $$MKSTRMID(TEXT,8,999,"{{","}}",.OPTION),!
  SET OPTION("STRIP TAGS")=0
  WRITE $$MKSTRMID(TEXT,8,999,"{{","}}",.OPTION),!
  KILL OPTION("STRIP TAGS")
  SET OPTION("KEEP TAGS")=1
  WRITE $$MKSTRMID(TEXT,1,5,"{{","}}",.OPTION),!
  WRITE $$MKSTRMID(TEXT,8,5,"{{","}}",.OPTION),!
  WRITE $$MKSTRMID(TEXT,15,999,"{{","}}",.OPTION),!
  QUIT
  ;
MKSTRMID(TEXT,START,LEN,TAGSTART,TAGEND,OPTION) ;"MARKUP-STR-MID().  Like MidStr() or $EXTRACT(), but for Markup strings
  ;"SIMILAR to MidStr(), but skipping over MarkupString tags, E.G. 'Hello {{red}} world {{blue}} and stars'
  ;"INPUT: TEXT -- the text to extract from
  ;"       START -- index position to START from.  This is number of NON-TAG characters.  E.g. 'ab{{TAG}}cd', index 3->'c'
  ;"             NOTE: This START is counted *differently* from MIDSTRCOLOR() above.  
  ;"       LEN -- number of NON-tag characters to return.  
  ;"       TAGSTART -- starting chars of markup tag.  E.g. "{{"
  ;"       TAGEND -- ending chars of markup tag, E.g. "}}"
  ;"       OPTION -- Optional.  
  ;"         OPTION("STRIP TAGS")=1.  If found, tags are not return with result, and are stripped out.  
  ;"                              If NOT found, then tags are returned in output string, but not counted in length returned.
  ;"         OPTION("KEEP TAGS")=1  If found, then ALL tags are returned, even those outside specified range.
  ;"         OPTION("KEEP LEFT TAGS")=1  If found, then all tags to left of range and inside specified range returned  
  NEW RESULT SET RESULT=""
  SET TAGSTART=$GET(TAGSTART) IF TAGSTART="" GOTO MSMSDN 
  SET TAGEND=$GET(TAGEND) IF TAGEND="" GOTO MSMSDN
  NEW ENCAPS SET ENCAPS(TAGSTART)=TAGEND
  NEW MAP DO MAPMATCH2(TEXT,.MAP,.ENCAPS)
  NEW TEXTLEN SET TEXTLEN=$LENGTH(TEXT)
  NEW STRIP SET STRIP=($GET(OPTION("STRIP TAGS"))=1)
  NEW KEEPTAGS SET KEEPTAGS=($GET(OPTION("KEEP TAGS"))=1)
  NEW KEEPLEFT SET KEEPLEFT=($GET(OPTION("KEEP LEFT TAGS"))=1)
  NEW OUTCT SET OUTCT=0
  NEW PLAINCT SET PLAINCT=0
  NEW STARTREACHED SET STARTREACHED=0
  NEW DONE SET DONE=0
  NEW POS,INTAG
  FOR POS=1:1:TEXTLEN DO  QUIT:(DONE=1)&(KEEPTAGS=0)
  . SET INTAG=$$INTAG(POS,.MAP)            
  . IF INTAG DO
  . . IF STRIP QUIT
  . . IF (KEEPLEFT!STARTREACHED!KEEPTAGS) SET RESULT=RESULT_$EXTRACT(TEXT,POS)
  . ELSE  DO  ;"INTAG=0
  . . SET PLAINCT=PLAINCT+1
  . . IF PLAINCT<START QUIT
  . . SET STARTREACHED=1
  . . IF OUTCT'<LEN SET DONE=1 QUIT
  . . SET RESULT=RESULT_$EXTRACT(TEXT,POS),OUTCT=OUTCT+1
MSMSDN ;
  QUIT RESULT
  ;
MKSTRLEN(TEXT,TAGSTART,TAGEND)  ;"Length of Markup string (excluding tags)
  ;"INPUT: TEXT -- the text to count
  ;"       TAGSTART -- starting chars of markup tag.  E.g. "{{"
  ;"       TAGEND -- ending chars of markup tag, E.g. "}}"
  NEW RESULT SET RESULT=0
  SET TAGSTART=$GET(TAGSTART) IF TAGSTART="" GOTO MKSLDN
  SET TAGEND=$GET(TAGEND) IF TAGEND="" GOTO MKSLDN
  NEW ENCAPS SET ENCAPS(TAGSTART)=TAGEND
  NEW MAP DO MAPMATCH2(TEXT,.MAP,.ENCAPS)
  NEW TEXTLEN SET TEXTLEN=$LENGTH(TEXT)
  NEW POS,INTAG
  FOR POS=1:1:TEXTLEN DO  
  . SET INTAG=$$INTAG(POS,.MAP) 
  . IF INTAG QUIT
  . SET RESULT=RESULT+1
MKSLDN ;
  QUIT RESULT
  ;
TESTMKLTRIM ;
  SET %="ABC{{blue}}DEFGHI"
  WRITE $$MKSTRLTRIM(%,4,"{{","}}"),!
  SET %="{{blue}}ABCDEFGHI"
  WRITE $$MKSTRLTRIM(%,4,"{{","}}"),!
  SET %="ABCD{{blue}}EFGHI"
  WRITE $$MKSTRLTRIM(%,4,"{{","}}"),!
  SET %="ABCDE{{blue}}FGHI"
  WRITE $$MKSTRLTRIM(%,4,"{{","}}"),!
  WRITE "--",!
  NEW OPTION SET OPTION("KEEP TAGS")=1
  SET %="ABC{{blue}}DEFGHI"
  WRITE $$MKSTRLTRIM(%,4,"{{","}}",.OPTION),!
  SET %="{{blue}}ABCDEFGHI"
  WRITE $$MKSTRLTRIM(%,4,"{{","}}",.OPTION),!
  SET %="ABCD{{blue}}EFGHI"
  WRITE $$MKSTRLTRIM(%,4,"{{","}}",.OPTION),!
  SET %="ABCDE{{blue}}FGHI"
  WRITE $$MKSTRLTRIM(%,4,"{{","}}",.OPTION),!
  QUIT
  ;
MKSTRLTRIM(TEXT,NUM,TAGSTART,TAGEND,OPTION) ;"MARKUP-STR-LTRIM().   
  ;"INPUT: TEXT -- the text to extract from
  ;"       NUM -- Number of NON-tag characters to trip from LEFT of TEXT
  ;"       TAGSTART -- starting chars of markup tag.  E.g. "{{"
  ;"       TAGEND -- ending chars of markup tag, E.g. "}}"
  ;"       OPTION -- Optional.  
  ;"         OPTION("KEEP TAGS")=1  If found, then ALL tags are returned, even those outside specified range.  
  ;"E.g. ABC{{blue}}DEFGHI,  trim 4 --> EFGHI
  ;"E.g. {{blue}}ABCDEFGHI,  trim 4 --> EFGHI
  ;"E.g. ABCD{{blue}}EFGHI,  trim 4 --> EFGHI  <-- NOTE: {{blue}} excluded because trimming 4, means starting at 5, which is PAST tag
  ;"E.g. ABCDE{{blue}}EFGHI,  trim 4 --> E{{blue}}FGHI 
  ;"Below are if OPTION("KEEP TAGS")=1
  ;"E.g. ABC{{blue}}DEFGHI,  trim 4 --> {{blue}}EFGHI
  ;"E.g. {{blue}}ABCDEFGHI,  trim 4 --> {{blue}}EFGHI
  ;"E.g. ABCD{{blue}}EFGHI,  trim 4 --> {{blue}}EFGHI  <-- NOTE: {{blue}} included
  ;"E.g. ABCDE{{blue}}EFGHI,  trim 4 --> E{{blue}}FGHI 
  NEW RESULT SET RESULT=$$MKSTRMID(.TEXT,NUM+1,$LENGTH(TEXT),.TAGSTART,.TAGEND,.OPTION)  
  QUIT RESULT
  ;
TESTMKRTRIM ;
  SET %="ABCDEFG{{blue}}HI"
  WRITE $$MKSTRRTRIM(%,1,"{{","}}"),!
  WRITE $$MKSTRRTRIM(%,2,"{{","}}"),!
  WRITE $$MKSTRRTRIM(%,3,"{{","}}"),!
  NEW OPTION SET OPTION("KEEP TAGS")=1
  WRITE $$MKSTRRTRIM(%,1,"{{","}}"),!
  WRITE $$MKSTRRTRIM(%,2,"{{","}}"),!
  WRITE $$MKSTRRTRIM(%,3,"{{","}}"),!
  QUIT
  ;
MKSTRRTRIM(TEXT,NUM,TAGSTART,TAGEND) ;"MARKUP-STR-RTRIM().   
  ;"INPUT: TEXT -- the text to extract from
  ;"       NUM -- Number of NON-tag characters to trip from RIGHT of TEXT
  ;"       TAGSTART -- starting chars of markup tag.  E.g. "{{"
  ;"       TAGEND -- ending chars of markup tag, E.g. "}}"
  ;"       OPTION -- Optional.  
  ;"         OPTION("KEEP TAGS")=1  If found, then ALL tags are returned, even those outside specified range.  
  ;"E.g. ABCDEFG{{blue}}HI,  trim 1 --> ABCDEFG{{blue}}H
  ;"E.g. ABCDEFG{{blue}}HI,  trim 2 --> ABCDEFG{{blue}}  <-- NOTE: {{blue}} is included. 
  ;"E.g. ABCDEFG{{blue}}HI,  trim 3 --> ABCDEF
  ;"Below are if OPTION("KEEP TAGS")=1
  ;"E.g. ABCDEFG{{blue}}HI,  trim 1 --> ABCDEFG{{blue}}H
  ;"E.g. ABCDEFG{{blue}}HI,  trim 2 --> ABCDEFG{{blue}}  
  ;"E.g. ABCDEFG{{blue}}HI,  trim 3 --> ABCDEF{{blue}}   <-- NOTE: {{blue}} is included. 
  NEW LEN SET LEN=$$MKSTRLEN(.TEXT,.TAGSTART,.TAGEND)  ;"Length of Markup string (excluding tags)
  NEW RESULT SET RESULT=$$MKSTRMID(.TEXT,1,LEN-NUM,.TAGSTART,.TAGEND,.OPTION)  
  QUIT RESULT
  ;
INTAG(POS,MAP)  ;"Based on MAP from MAPMATCH2, is POS inside an encapsulator?  Only considers top level, non-nested encapsulators.
  NEW RESULT SET RESULT=0
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(MAP(IDX)) QUIT:(IDX'>0)!(RESULT=1)  DO
  . NEW TAGS SET TAGS=$GET(MAP(IDX,1))
  . NEW OPENTAG SET OPENTAG=$PIECE(TAGS,"^",1)
  . NEW CLOSETAG SET CLOSETAG=$PIECE(TAGS,"^",2)
  . NEW STARTPOS SET STARTPOS=+$GET(MAP(IDX,1,"Pos",1))
  . NEW ENDPOS SET ENDPOS=+$GET(MAP(IDX,1,"Pos",2))+$LENGTH(CLOSETAG)-1
  . IF (POS<STARTPOS)!(POS>ENDPOS) QUIT
  . SET RESULT=1
  QUIT RESULT
  ;
TESTFE ;
  NEW % SET %="ABC{{RECORD# 1}}DEFG{{RECORD# 2}}HIJK{{RECORD# 3}}LMNOP"
  DO FOREACHTAG(%,"{{","}}","TESTCALLBACK^TMGSTUT3")
  QUIT
  ;
TESTCALLBACK(TEXT,ATAG,POS) ;"CALLBACK FOR TESTFE
  WRITE "FOUND ",ATAG,!
  NEW PARTA SET PARTA=$EXTRACT(TEXT,1,POS-1)
  NEW PARTB SET PARTB=$EXTRACT(TEXT,POS+$LENGTH(ATAG),$LENGTH(TEXT))
  SET ATAG=$PIECE(ATAG,"RECORD# ",2)
  SET TEXT=PARTA_ATAG_PARTB
  QUIT
  ;
FOREACHTAG(TEXT,TAGSTART,TAGEND,HNDTAG) ;"Cycle through each tag, calling callback Fn HNDTAG
  ;"INPUT: TEXT -- the text to process
  ;"       TAGSTART -- starting chars of markup tag.  E.g. "{{"
  ;"       TAGEND -- ending chars of markup tag, E.g. "}}"
  ;"       HNDTAG -- LABEL^MODULE for callback code.  Must be informat, e.g. MYLABEL^MYCODE(TEXT,ATAG,POS)
  ;"                TEXT will be pass by reference and code may modify it.  NOTE: scanner is cycling
  ;"                   through TEXT and will continue after tag for next cycle.  If ATAG is modified, 
  ;"                   then POS will be automatically adjusted accordingly.  
  ;"                ATAG -- The text of the tag, without TAGSTART or TAGEND.  Passed by reference, may be modified
  ;"                POS -- this is index, in TEXT, that immediately starts ATAG.  READ ONLY
  ;"                Code should NOT return result
  ;"NOTE: this code is not designed to handle nested tags.  
  ;"      Any execution errors in callback code will be caught and ignored
  SET TAGSTART=$GET(TAGSTART) IF TAGSTART="" QUIT 
  SET TAGEND=$GET(TAGEND) IF TAGEND="" QUIT
  SET HNDTAG=$GET(HNDTAG) IF HNDTAG="" QUIT
  SET TEXT=$GET(TEXT) IF TEXT="" QUIT
  NEW L2 SET L2=$LENGTH(TAGEND)
  NEW POS SET POS=1
  NEW DONE SET DONE=0
  FOR  DO  QUIT:DONE
  . SET POS=$FIND(TEXT,TAGSTART,POS) IF POS=0 SET DONE=1 QUIT
  . NEW P2 SET P2=$FIND(TEXT,TAGEND,POS) IF P2'>0 SET DONE=1 QUIT
  . NEW ATAG SET ATAG=$EXTRACT(TEXT,POS,P2-L2-1)
  . NEW CODE SET CODE="DO "_HNDTAG_"(.TEXT,.ATAG,POS)"
  . DO
  . . NEW $ETRAP SET $ETRAP="WRITE ""(Invalid M Code!  Error Trapped.)"",! SET $ETRAP="""",$ECODE="""""
  . . XECUTE CODE
  . SET POS=POS+$LENGTH(ATAG)+L2
  . IF POS>$LENGTH(TEXT) SET DONE=1  
  QUIT
  ;
FINDDT(TEXT,SPOS,OUT,SPT,EPT) ;"Find date in TEXT, starting at option SPOS, return value found in DTOUT
  ;"NOTE: at least intially, I will be looking for date in format of ##/##/## or ##/##/#### (the number of numbers does NOT matter)
  ;"      This could be expanded later....
  ;"      No support for time (i.e. @1250) at this time, could be added later
  ;"INPUT: TEXT -- text to search. 
  ;"       SPOS -- OPTIONAL.  Index of starting position for search.  
  ;"       OUT -- OPTIONAL.  PASS BY REFERENCE, AN OUT PARAMENTER.  Returns date found.
  ;"       SPT -- OPTIONAL.  PASS BY REFERENCE, AN OUT PARAMETER.  Returns index of start of date
  ;"       EPT -- OPTIONAL.  PASS BY REFERENCE, AN OUT PARAMETER.  Returns index of end of date
  ;"RESULT: Returns index, in TEXT, of first character FOLLOWING found date, or 0 if not found.
  ;"        NOTE: I am doing this to be consistent with $FIND function in mumps
  ;"                  000000000111111111122222222223333333333444444444455
  ;"                  123456789012345678901234567890123456789012345678901
  ;"        EXAMPLE: 'This is some text.  5/4/1978: And this is more text'
  ;"                                              ^---<  returns this position (29) 
  ;"                                              OUT='5/4/1978'
  ;"                                              SPT=21
  ;"                                              EPT=28
  NEW RESULT SET (RESULT,SPT,EPT)=0
  SET SPOS=+$GET(SPOS) IF SPOS'>0 SET SPOS=1
  NEW STRA,LENA SET STRA=$EXTRACT(TEXT,1,SPOS-1),LENA=$LENGTH(STRA)
  NEW STRB,LENB SET STRB=$EXTRACT(TEXT,SPOS,$LENGTH(TEXT)),LENB=$LENGTH(STRB)
  NEW P1 SET P1=$FIND(STRB,"/") IF P1'>0 GOTO FDTDN
  NEW P2 SET P2=$FIND(STRB,"/",P1) IF P2'>0 GOTO FDTDN
  ;"FIND START OF DATE STRING
  NEW P FOR P=P1-1:-1 QUIT:(P'>0)!(SPT>0)  DO
  . IF "1234567890/"[$EXTRACT(STRB,P) QUIT
  . SET SPT=P+1
  ;"FIND END OF DATE STRING
  FOR P=SPT:1 QUIT:(P>LENB)!(EPT>0)  DO
  . IF "1234567890/"[$EXTRACT(STRB,P) QUIT
  . SET EPT=P-1
  SET OUT=$EXTRACT(STRB,SPT,EPT)
  SET SPT=SPT+LENA
  SET EPT=EPT+LENA
  SET RESULT=EPT+1
FDTDN ;
  QUIT RESULT
  ;
PARSESTR(STR,ARR) ;"PARSE STRING -- for use in SUBSTRMATCH()
  ;"INPUT: STR -- STRING TO PARSE
  ;"       ARR -- PASS BY REFERENCE, AN OUT PARAMETER.  SEE SUBSTRMATCH() BELOW
  ;"OUTPUT: ARR filled as follows
  ;"         ARR(#)=<WORD>^<trailing divisors>
  ;"         ARR("IDX",<UPPERCASE OF WORD>,#)=""
  NEW DIVS SET DIVS=" ,;:.!?"
  NEW CURWORD SET CURWORD=""
  NEW WORDSTARTPOS SET WORDSTARTPOS=0
  NEW CURDIV SET CURDIV=""
  NEW OUTIDX SET OUTIDX=0
  NEW INDIV SET INDIV=0  ;"Is parsing position in a divisor character
  NEW P FOR P=1:1:$LENGTH(STR) DO
  . NEW CH SET CH=$EXTRACT(STR,P) QUIT:CH=""
  . IF DIVS[CH DO  QUIT  ;"If ch is a punctuation character
  . . SET INDIV=1
  . . SET CURDIV=CURDIV_CH
  . ELSE  DO  QUIT  
  . . IF INDIV DO
  . . . ;"last char was a divisor, so we must be starting a new word
  . . . SET ARR($I(OUTIDX))=CURWORD_"^"_CURDIV_"^"_WORDSTARTPOS
  . . . SET ARR("IDX",$$UP^XLFSTR(CURWORD),OUTIDX)=""
  . . . SET CURWORD=CH,WORDSTARTPOS=P,CURDIV="",INDIV=0
  . . ELSE  DO
  . . . IF WORDSTARTPOS=0 SET WORDSTARTPOS=P
  . . . SET CURWORD=CURWORD_CH
  IF CURWORD'="" DO
  . SET ARR($I(OUTIDX))=CURWORD_"^"_CURDIV_"^"_WORDSTARTPOS
  . SET ARR("IDX",CURWORD,OUTIDX)=""
  QUIT
  ;
SUBSTRMATCH(SUBSTR,STR,MATCH,SUBSTRARR,STRARR)  ;"Get match info of substring in string, return results in MATCH
  ;"Input: SUBSTR -- String input.  We will search for matches or partial matches of this inside STR
  ;"       STR -- String input.  This will be string that is searched for parts of SUBSTR
  ;"       MATCH -- PASS BY REFERENCE.  An OUT PARAMETER.  Format:
  ;"         MATCH(#)=<start position in STRING (not SUBSTRING)>^<match character length)^<number of words in match>^<end position of last char of match>
  ;"         MATCH(#,"STR")=<upper case matched string>
  ;"         MATCH("LENIDX",<char length>,#)=""
  ;"       SUBSTRARR -- OPTIONAL.  PASS BY REFERENCE.  If this contains data, it will be used instead of SUBSTR
  ;"                         If initially empty, then SUBSTR will be parsed into this, and results passed back.  
  ;"                    Format: Array of parsed substring, as created by PARSESTR
  ;"                    SUBSTRARR(#)=<WORD>^<trailing divisors>
  ;"                    SUBSTRARR("IDX",<UPPERCASE OF WORD>,#)=""   
  ;"                    Example: 
  ;"                      SUBSTRARR(1)="I^ ^1"
  ;"                      SUBSTRARR(2)="remember^ ^3"
  ;"                      SUBSTRARR(3)="that^ ^12"
  ;"                      SUBSTRARR(4)="Bill^ ^17"
  ;"                      SUBSTRARR(5)="and^ ^22"
  ;"                      SUBSTRARR(6)="Ted^, ^26"
  ;"                      SUBSTRARR(7)="dressed^ ^31"
  ;"                      SUBSTRARR(8)="in^ ^39"
  ;"                      SUBSTRARR(9)="costumes^, ^42"
  ;"                      SUBSTRARR(10)="went^ ^52"
  ;"                      SUBSTRARR(11)="on^ ^57"
  ;"                      SUBSTRARR(12)="an^ ^60"
  ;"                      SUBSTRARR(13)="adventure^ ^63"
  ;"                      SUBSTRARR(14)="to^ ^73"
  ;"                      SUBSTRARR(15)="France^.^76"
  ;"                      SUBSTRARR("IDX","ADVENTURE",13)=""
  ;"                      SUBSTRARR("IDX","AN",12)=""
  ;"                      SUBSTRARR("IDX","AND",5)=""
  ;"                      SUBSTRARR("IDX","BILL",4)=""
  ;"                      SUBSTRARR("IDX","COSTUMES",9)=""
  ;"                      SUBSTRARR("IDX","DRESSED",7)=""
  ;"                      SUBSTRARR("IDX","France",15)=""
  ;"                      SUBSTRARR("IDX","I",1)=""
  ;"                      SUBSTRARR("IDX","IN",8)=""
  ;"                      SUBSTRARR("IDX","ON",11)=""
  ;"                      SUBSTRARR("IDX","REMEMBER",2)=""
  ;"                      SUBSTRARR("IDX","TED",6)=""
  ;"                      SUBSTRARR("IDX","THAT",3)=""
  ;"                      SUBSTRARR("IDX","TO",14)=""
  ;"                      SUBSTRARR("IDX","WENT",10)=""  
  ;"       STRARR --    OPTIONAL.  PASS BY REFERENCE.  If this contains data, it will be used instead of STR
  ;"                         If initially empty, then STR will be parsed into this, and results passed back.  
  ;"                    Format: Array of parsed substring, as created by PARSESTR
  ;"                    STRARR(#)=<WORD>^<trailing divisors>
  ;"                    STRARR("IDX",<UPPERCASE OF WORD>,#)=""
  ;"                    Example:
  ;"                      STRARR(1)="Remember^, ^1"
  ;"                      STRARR(2)="Bill^ ^11"
  ;"                      STRARR(3)="and^ ^16"
  ;"                      STRARR(4)="Ted^, ^20"
  ;"                      STRARR(5)="dressed^ ^25"
  ;"                      STRARR(6)="in^ ^33"
  ;"                      STRARR(7)="costumes^, ^36"
  ;"                      STRARR(8)="went^ ^46"
  ;"                      STRARR(9)="on^ ^51"
  ;"                      STRARR(10)="an^ ^54"
  ;"                      STRARR(11)="adventure^ ^57"
  ;"                      STRARR(12)="to^ ^67"
  ;"                      STRARR(13)="France^. ^70"
  ;"                      STRARR(14)="Bill^ ^78"
  ;"                      STRARR(15)="got^ ^83"
  ;"                      STRARR(16)="lost^.^87"
  ;"                      STRARR("IDX","ADVENTURE",11)=""
  ;"                      STRARR("IDX","AN",10)=""
  ;"                      STRARR("IDX","AND",3)=""
  ;"                      STRARR("IDX","BILL",2)=""
  ;"                      STRARR("IDX","BILL",14)=""
  ;"                      STRARR("IDX","COSTUMES",7)=""
  ;"                      STRARR("IDX","DRESSED",5)=""
  ;"                      STRARR("IDX","FRANCE",13)=""
  ;"                      STRARR("IDX","GOT",15)=""
  ;"                      STRARR("IDX","IN",6)=""
  ;"                      STRARR("IDX","ON",9)=""
  ;"                      STRARR("IDX","REMEMBER",1)=""
  ;"                      STRARR("IDX","TED",4)=""
  ;"                      STRARR("IDX","TO",12)=""
  ;"                      STRARR("IDX","WENT",8)=""
  ;"                      STRARR("IDX","lost",16)=""                      
  ;"Discussion about how matching process will be done.  We will be looking for
  ;" all elements of a 'substring' that can be found in a 'string'.  For example,
  ;" if substring is 'Jack and Jill are friends', and the string is 
  ;" 'Jack and Jill went up a hill, then Jack and Jill are tired', then the fragment
  ;" 'Jack and Jill' would match twice in the string.  
  ;" 
  ;"Methology: 
  ;"   1) start a matching train of words from the substring.  First 'Jack', then
  ;"     'Jack and' and then 'Jack and Jill', etc...  The train will grow by one
  ;"     word each cycle, until there are no further matches found in the string.
  ;"   2) With each cycle, the matches between the current substring train and 
  ;"      the parts from the string will comprise one or more string trains.  As
  ;"      long as the growing substring train matches the growing string train, all
  ;"      will be OK.  But whenever a string train no longer matches, it will be 
  ;"      marked as 'closed' and not allowed to grow further.  And as soon as all
  ;"      string trains are closed, then the substring train will be reset to null
  ;"      and the process repeats.  
  ;"      
  ;"Example.  '[ ]' will be used to demonstrate the current matching trains
  ;"Cycle:
  ;"1) substring: [Jack] and Jill are friends 
  ;"      string: [Jack] and Jill went up a hill, then [Jack] and Jill are tired
  ;"       match train 1: [Jack]
  ;"       match train 2: [Jack]
  ;" 
  ;"2) substring: [Jack and] Jill are friends 
  ;"      string: [Jack and] Jill went up a hill, then [Jack and] Jill are tired
  ;"       match train 1: [Jack and]
  ;"       match train 2: [Jack and]
  ;"
  ;"3) substring: [Jack and Jill] are friends 
  ;"      string: [Jack and Jill] went up a hill, then [Jack and Jill] are tired
  ;"       match train 1: [Jack and Jill]
  ;"       match train 2: [Jack and Jill]
  ;"
  ;"4) substring: [Jack and Jill are] friends 
  ;"      string: [Jack and Jill] went up a hill, then [Jack and Jill are] tired
  ;"       match train 1: [Jack and Jill]  <-- closed
  ;"       match train 2: [Jack and Jill are]
  ;"      
  ;"5) substring: [Jack and Jill are friends] <-- no maches, so train will be reset
  ;"      string: [Jack and Jill] went up a hill, then [Jack and Jill] are tired
  ;"       match train 1: [Jack and Jill]  <-- closed
  ;"       match train 2: [Jack and Jill are] <-- closed    
  ;
  IF $DATA(SUBSTRARR)=0 DO PARSESTR(SUBSTR,.SUBSTRARR)
  IF $DATA(STRARR)=0 DO PARSESTR(STR,.STRARR)
  ;  
  NEW SUBSTRWORDSEQ SET SUBSTRWORDSEQ=0
  NEW ZZ
  SET ZZ("SUBSTR","TRAIN")=""
  SET ZZ("SUBSTR","TRAIN","RETURN")=""
  SET ZZ("SUBSTR","TRAIN","LEN")=0
  ;"Cycle through each word in substring
  FOR  SET SUBSTRWORDSEQ=$ORDER(SUBSTRARR(SUBSTRWORDSEQ)) QUIT:SUBSTRWORDSEQ'>0  DO
  . NEW TMP SET TMP=$GET(SUBSTRARR(SUBSTRWORDSEQ)) QUIT:TMP=""
  . NEW WORD,UWORD,DIV SET WORD=$PIECE(TMP,"^",1),DIV=$PIECE(TMP,"^",2),UWORD=$$UP^XLFSTR(WORD)
  . ;"Grow the substring train
  . SET ZZ("SUBSTR","TRAIN")=$GET(ZZ("SUBSTR","TRAIN"))_SUBSTRWORDSEQ_"-"_UWORD_"^"
  . SET ZZ("SUBSTR","TRAIN","RETURN")=$GET(ZZ("SUBSTR","TRAIN","RETURN"))_UWORD_DIV
  . SET ZZ("SUBSTR","TRAIN","LEN")=$GET(ZZ("SUBSTR","TRAIN","LEN"))+1
  . ;"Cycle through all existing string trains to see if they can match UWORD and grow
  . NEW TRAINFOUND SET TRAINFOUND=0
  . NEW TRAINIDX SET TRAINIDX=0
  . FOR  SET TRAINIDX=$ORDER(ZZ("STR","TRAIN",TRAINIDX)) QUIT:TRAINIDX'>0  DO
  . . IF $GET(ZZ("STR","TRAIN",TRAINIDX,"STATUS"))="CLOSED" QUIT
  . . IF $GET(ZZ("STR","TRAIN",TRAINIDX,"NEXT WORD","EXT"))'=UWORD DO  QUIT  
  . . . SET ZZ("STR","TRAIN",TRAINIDX,"STATUS")="CLOSED" 
  . . . KILL ZZ("STR","TRAIN",TRAINIDX,"NEXT WORD")
  . . ELSE  DO
  . . . SET TRAINFOUND=1
  . . . NEW CURNEXTINT SET CURNEXTINT=ZZ("STR","TRAIN",TRAINIDX,"NEXT WORD","INT")
  . . . NEW CURNEXTEXT SET CURNEXTEXT=ZZ("STR","TRAIN",TRAINIDX,"NEXT WORD","EXT")
  . . . NEW CURLEN SET CURLEN=+$GET(ZZ("STR","TRAIN",TRAINIDX,"LEN"))
  . . . NEW CURSEQ SET CURSEQ=+CURNEXTINT
  . . . SET ZZ("STR","TRAIN",TRAINIDX,"VALUE","INT")=ZZ("STR","TRAIN",TRAINIDX,"VALUE","INT")_CURNEXTINT_"^"
  . . . SET ZZ("STR","TRAIN",TRAINIDX,"VALUE","EXT")=ZZ("STR","TRAIN",TRAINIDX,"VALUE","EXT")_CURNEXTEXT_DIV
  . . . SET ZZ("STR","TRAIN",TRAINIDX,"SEQ","LAST")=CURSEQ
  . . . SET ZZ("STR","TRAIN",TRAINIDX,"LEN")=CURLEN+1
  . . . ;"Get the new next word, for matching against next round
  . . . KILL ZZ("STR","TRAIN",TRAINIDX,"NEXT WORD")
  . . . NEW NEWSEQ SET NEWSEQ=CURSEQ+1
  . . . NEW TMP SET TMP=$GET(STRARR(NEWSEQ))
  . . . NEW NEWNEXT SET NEWNEXT=$$UP^XLFSTR($PIECE(TMP,"^",1))
  . . . NEW NEXTDIV SET NEXTDIV=$PIECE(TMP,"^",2)
  . . . IF NEWNEXT="" DO  QUIT
  . . . . SET ZZ("STR","TRAIN",TRAINIDX,"STATUS")="CLOSED"
  . . . SET ZZ("STR","TRAIN",TRAINIDX,"NEXT WORD","INT")=NEWSEQ_"-"_NEWNEXT
  . . . SET ZZ("STR","TRAIN",TRAINIDX,"NEXT WORD","EXT")=NEWNEXT
  . IF TRAINFOUND QUIT  ;"ready to start new cycle
  . ;"OTHERWISE .... No existing train found to grow.  Can new train be started?
  . NEW WORDFOUND SET WORDFOUND=0
  . NEW STRWORDSEQ SET STRWORDSEQ=0
  . ;"look for all matches of UWORD (from substring) in string
  . FOR  SET STRWORDSEQ=$ORDER(STRARR("IDX",UWORD,STRWORDSEQ)) QUIT:STRWORDSEQ'>0  DO   
  . . SET WORDFOUND=1
  . . NEW TMP,CURUWORD,CURDIV SET TMP=$GET(STRARR(STRWORDSEQ)),CURUWORD=$$UP^XLFSTR($PIECE(TMP,"^",1)),CURDIV=$PIECE(TMP,"^",2)
  . . ;"Is this word already matched into a prior train (open or closed).  If so, quit
  . . NEW SKIP SET SKIP=0
  . . NEW TRAINIDX SET TRAINIDX=0
  . . FOR  SET TRAINIDX=$ORDER(ZZ("STR","TRAIN",TRAINIDX)) QUIT:TRAINIDX'>0  DO
  . . . NEW FIRSTSEQ SET FIRSTSEQ=+$GET(ZZ("STR","TRAIN",TRAINIDX,"SEQ","FIRST"))
  . . . NEW LASTSEQ SET LASTSEQ=+$GET(ZZ("STR","TRAIN",TRAINIDX,"SEQ","LAST"))
  . . . IF (STRWORDSEQ>=FIRSTSEQ)&(STRWORDSEQ<=LASTSEQ) DO  QUIT
  . . . . SET SKIP=1
  . . IF SKIP QUIT
  . . ;"We have a match in string, that was not part of prior train, so start new train.  
  . . SET TRAINIDX=$ORDER(ZZ("STR","TRAIN",""),-1)+1
  . . SET ZZ("STR","TRAIN",TRAINIDX,"VALUE","INT")=STRWORDSEQ_"-"_CURUWORD_"^"
  . . SET ZZ("STR","TRAIN",TRAINIDX,"VALUE","EXT")=CURUWORD_CURDIV
  . . SET ZZ("STR","TRAIN",TRAINIDX,"SEQ","FIRST")=STRWORDSEQ
  . . SET ZZ("STR","TRAIN",TRAINIDX,"SEQ","LAST")=STRWORDSEQ
  . . SET ZZ("STR","TRAIN",TRAINIDX,"STATUS")="ACTIVE"
  . . SET ZZ("STR","TRAIN",TRAINIDX,"LEN")=1
  . . ;"Get the new next word, for matching against next round
  . . NEW NEWSEQ SET NEWSEQ=STRWORDSEQ+1
  . . NEW TMP SET TMP=$GET(STRARR(NEWSEQ))
  . . NEW NEWNEXT SET NEWNEXT=$$UP^XLFSTR($PIECE(TMP,"^",1))
  . . NEW NEWNEXTDIV SET NEWNEXTDIV=$PIECE(TMP,"^",2)
  . . IF NEWNEXT="" DO  QUIT
  . . . SET ZZ("STR","TRAIN",TRAINIDX,"STATUS")="CLOSED"
  . . SET ZZ("STR","TRAIN",TRAINIDX,"NEXT WORD","INT")=NEWSEQ_"-"_NEWNEXT
  . . SET ZZ("STR","TRAIN",TRAINIDX,"NEXT WORD","EXT")=NEWNEXT
  . IF WORDFOUND=1 QUIT  ;"read to start new cycle
  . ;"Reset substring train
  . KILL ZZ("SUBSTR","TRAIN")
  . SET ZZ("SUBSTR","TRAIN")=""
  . SET ZZ("SUBSTR","TRAIN","RETURN")=""
  . SET ZZ("SUBSTR","TRAIN","LEN")=0
  ;"compile output
  NEW OUTIDX SET OUTIDX=0
  NEW TRAINIDX SET TRAINIDX=0
  FOR  SET TRAINIDX=$ORDER(ZZ("STR","TRAIN",TRAINIDX)) QUIT:TRAINIDX'>0  DO
  . NEW TRAINLEN SET TRAINLEN=$GET(ZZ("STR","TRAIN",TRAINIDX,"LEN"))
  . IF TRAINLEN<2 KILL ZZ("STR","TRAIN",TRAINIDX) QUIT
  . NEW RETURN SET RETURN=ZZ("STR","TRAIN",TRAINIDX,"VALUE","EXT")
  . SET MATCH($I(OUTIDX),"STR")=RETURN
  . NEW ATRAIN SET ATRAIN=$GET(ZZ("STR","TRAIN",TRAINIDX,"VALUE","INT"))
  . NEW FIRSTWORD,FIRSTIDX SET FIRSTWORD=$PIECE(ATRAIN,"^",1),FIRSTIDX=+FIRSTWORD
  . NEW FIRSTENTRY SET FIRSTENTRY=$GET(STRARR(FIRSTIDX))
  . NEW STARTPOS SET STARTPOS=+$PIECE(FIRSTENTRY,"^",3)
  . NEW MATCHLEN SET MATCHLEN=$LENGTH(RETURN)
  . NEW TRAINLEN SET TRAINLEN=$LENGTH(ATRAIN,"^")
  . IF $PIECE(ATRAIN,"^",TRAINLEN)="" SET TRAINLEN=TRAINLEN-1
  . NEW LASTWORD,LASTIDX SET LASTWORD=$PIECE(ATRAIN,"^",TRAINLEN),LASTIDX=+LASTWORD
  . NEW LASTENTRY SET LASTENTRY=$GET(STRARR(LASTIDX))
  . SET LASTWORD=$PIECE(LASTWORD,"-",2)
  . NEW ENDPOS SET ENDPOS=+$PIECE(LASTENTRY,"^",3)+$LENGTH(LASTWORD)-1  ;"position of last char of last word
  . SET MATCH(OUTIDX)=STARTPOS_"^"_MATCHLEN_"^"_TRAINLEN_"^"_ENDPOS
  . SET MATCH("LENIDX",MATCHLEN,OUTIDX)=""  
  QUIT
  ;
TESTSS  ;"TEST SUBSTRMATCH()
  NEW IDX SET IDX=0
  NEW TOPICS
  ;"SET TOPICS($I(IDX))="Bill and Ted went on an adventure."
  ;"SET TOPICS($I(IDX))="I remember that Bill and Ted, dressed in costumes, went on an adventure to France."
  ;"SET TOPICS($I(IDX))="Remember, Bill and Ted, dressed in costumes, went on an adventure to France. Bill got lost."
  SET TOPICS($I(IDX))="Listen to this -- Dr. office.  None recently."
  SET TOPICS($I(IDX))="Listen to this -- Dr. office.  ... None recently.  It continues to be about the same."  
  ;"SET TOPICS($I(IDX))="That Cat and dog."
  ;"SET TOPICS($I(IDX))="Cat and dog and crow."
  ;"
  SET IDX=""
  FOR  SET IDX=$ORDER(TOPICS(IDX),-1) QUIT:IDX'>0  DO
  . NEW CURARR,CURTEXT SET CURTEXT=$GET(TOPICS(IDX)) QUIT:CURTEXT=""
  . NEW JDX SET JDX=IDX
  . FOR  SET JDX=$ORDER(TOPICS(JDX),-1) QUIT:(JDX'>0)  DO
  . . NEW PRIORARR,PRIORTEXT SET PRIORTEXT=$GET(TOPICS(JDX)) QUIT:PRIORTEXT=""
  . . NEW MATCHES DO SUBSTRMATCH^TMGSTUT3(PRIORTEXT,CURTEXT,.MATCHES,.PRIORARR,.CURARR)
  . . NEW HASMATCH FOR  DO  QUIT:HASMATCH=0
  . . . SET HASMATCH=0
  . . . NEW ALEN SET ALEN=+$ORDER(MATCHES("LENIDX",""),-1) QUIT:ALEN'>0  
  . . . NEW MATCHIDX SET MATCHIDX=$ORDER(MATCHES("LENIDX",ALEN,0)) QUIT:MATCHIDX'>0
  . . . NEW AMATCH SET AMATCH=$GET(MATCHES(MATCHIDX)) QUIT:AMATCH=""
  . . . SET HASMATCH=1
  . . . NEW STARTPOS SET STARTPOS=+AMATCH
  . . . NEW LEN SET LEN=+$PIECE(AMATCH,"^",2)
  . . . NEW ENDPOS SET ENDPOS=+$PIECE(AMATCH,"^",4)
  . . . NEW PARTA SET PARTA=""
  . . . IF STARTPOS>0 SET PARTA=$EXTRACT(CURTEXT,1,STARTPOS-1)
  . . . NEW PARTB SET PARTB=$EXTRACT(CURTEXT,STARTPOS,ENDPOS)
  . . . NEW PARTC SET PARTC=""
  . . . IF LEN>0 SET PARTC=$EXTRACT(CURTEXT,ENDPOS+1,$LENGTH(CURTEXT))
  . . . SET CURTEXT=$SELECT(PARTA'="":PARTA_"...",1:"")_PARTC
  . . . SET TOPICS(IDX)=CURTEXT
  . . . KILL CURARR,MATCHES 
  . . . DO SUBSTRMATCH^TMGSTUT3(PRIORTEXT,CURTEXT,.MATCHES,.PRIORARR,.CURARR)
  QUIT
  ;
SCANDATES(STR,OUT)  ;"return positions of dates, in format of ##/##/####  Days, month can be 1 or 2 digits.  Yr can be 2 or 4 digits
  ;"INPUT:  STR -- String to scan.  
  ;"        OUT -- PASS BY REFERENCE, AN OUT PARAMETER. 
  ;"            OUT(#)=<StartPosOfDate>^<EndPosOfDate>^<DateString>
  ;"Result: 1 if any dates found, otherwise 0
  ;"Allowed date formats:
  ;"   #/#/##       e.g  1/1/24
  ;"   ##/#/##      e.g. 12/3/25
  ;"   #/##/##      e.g. 3/24/19
  ;"   ##/##/##     e.g. 10/31/18
  ;"   #/#/####     e.g  1/1/2024
  ;"   ##/#/####    e.g. 12/3/2025
  ;"   #/##/####    e.g. 3/24/2019
  ;"   ##/##/####   e.g. 10/31/2018
  ;
  ;"   ##/#/#       e.g  24/1/1
  ;"   ##/#/##      e.g. 25/3/1
  ;"   ##/##/#      e.g. 19/3/24
  ;"   ##/##/##     e.g. 18/10/31
  ;"   ####/#/#     e.g  2024/1/1
  ;"   ####/##/#    e.g. 2025/12/3
  ;"   ####/#/##    e.g. 2019/3/24
  ;"   ####/##/##   e.g. 2019/10/31
  ;
  ;"  Days, month can be 1 or 2 digits.  Yr can be 2 or 4 digits
  ;"  Year can be in 1st or last position. 
  ;         
  NEW RESULT SET RESULT=0
  NEW P1,P2 SET (P1,P2)=0
  NEW DONE SET DONE=0
  FOR  DO  QUIT:DONE
  . SET P1=$FIND(STR,"/",P1)
  . IF P1'>0 SET DONE=1 QUIT  
  . SET P2=$FIND(STR,"/",P1)
  . IF P2-P1>3 SET DONE=1 QUIT
  . ;"At this point we have ????/?/???? or ????/??/????
  . NEW MID SET MID=$EXTRACT(STR,P1,P2-2)
  . IF MID'?1.2N SET DONE=1 QUIT  ;"If not 1-2 digits of numbers, quit
  . NEW MIDLEN SET MIDLEN=$LENGTH(MID)
  . ;"At this point we have ????/#/???? or ????/##/????
  . ;"Next, scan pre area for numbers.  
  . NEW P0,PRE,CH SET PRE=""
  . NEW DN2 SET DN2=0  ;"i.e DONE2
  . FOR P0=P1-2:-1:1 DO  QUIT:DN2
  . . SET CH=$EXTRACT(STR,P0)
  . . IF CH'?1N SET DN2=1 QUIT
  . . SET PRE=CH_PRE
  . NEW PRELEN SET PRELEN=$LENGTH(PRE) IF PRELEN=0 SET DONE=1 QUIT
  . NEW PRESTART SET PRESTART=P1-1-$LENGTH(PRE)
  . ;"At this point we have ####/#/???? or ####/##/????
  . ;"Next, scan post area for numbers.  
  . NEW POST SET POST=""
  . SET DN2=0  ;"i.e DONE2
  . FOR P0=P2:1:$LENGTH(STR) DO  QUIT:DN2
  . . SET CH=$EXTRACT(STR,P0)
  . . IF CH'?1N SET DN2=1 QUIT
  . . SET POST=POST_CH
  . NEW POSTLEN SET POSTLEN=$LENGTH(POST) IF POSTLEN=0 SET DONE=1 QUIT
  . NEW POSTEND SET POSTEND=P2+$LENGTH(POST)-1
  . ;"At this point we have ####/#/#### or ####/##/####
  . ;"Now check for valid length configurations.  
  . IF (",1,2,4,"[PRELEN)=0 SET DONE=1 QUIT
  . IF (",1,2,"[MIDLEN)=0 SET DONE=1 QUIT
  . IF (",1,2,4,"[POSTLEN)=0 SET DONE=1 QUIT
  . IF PRELEN=4,POSTLEN>2 SET DONE=1  ;"If 4 digit year is first, last position must be 1 or 2 digits.
  . ;"If we got here, we have a date!!
  . NEW IDX SET IDX=$ORDER(OUT(""),-1)+1
  . NEW FRAG SET FRAG=$EXTRACT(STR,PRESTART,POSTEND)
  . SET OUT(IDX)=PRESTART_"^"_POSTEND_"^"_FRAG
  . SET P1=POSTEND+1
  QUIT
  ;
SCNDTDN ;
  QUIT RESULT
  
TESTSCNDT ;
  NEW ARR
  SET ARR(1)="pre text 1/1/24 post text 12/3/25 post text"
  SET ARR(2)="pre text 12/3/25 post text 3/24/19 post text"
  SET ARR(3)="pre text 3/24/19 post text 10/31/18 post text"
  SET ARR(4)="pre text 10/31/18 post text"
  SET ARR(5)="pre text 1/1/2024 post text"
  SET ARR(6)="pre text 12/3/2025 post text"
  SET ARR(7)="pre text 3/24/2019 post text"
  SET ARR(8)="pre text 10/31/2018 post text"
  SET ARR(9)="pre text 24/1/1 post text"
  SET ARR(10)="pre text 25/3/1 post text"
  SET ARR(11)="pre text 19/3/24 post text"
  SET ARR(12)="pre text 18/10/31 post text"
  SET ARR(13)="pre text 2024/1/1 post text"
  SET ARR(14)="pre text 2025/12/3 post text"
  SET ARR(15)="pre text 2019/3/24 post text"
  SET ARR(16)="pre text 2019/10/31 post text"
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:IDX'>0  DO
  . NEW STR SET STR=ARR(IDX)
  . NEW POSINFO DO SCANDATES(STR,.POSINFO)
  . IF $DATA(POSINFO) WRITE STR," --> ",! ZWR POSINFO
  
  QUIT