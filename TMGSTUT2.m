TMGSTUT2 ;TMG/kst/SACC ComplIant String Util LIb ;5/23/19,10/28/24
         ;;1.0;TMG-LIB;**1,17**;7/17/12
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
  ;"$$CAPWORDS(S,DIV) -- Capitalize the first letter of each word in a string
  ;"$$CAP1ST(WORD)  -- Capitalize only first letter of word.
  ;"$$CAP1STAL(SENTENCE,DIVCHS) -- Capitalize only first letter of word, for each word in sentence
  ;"REMOVENCAP(TMGTEXT,ARR,ENCAP) --Remove all parts of TMGTEXT that are in ENCAPSULATOR, replacing them with coded string, and puting vals in ARR
  ;"REMOVEQTS(TMGTEXT,ARR)  --Remove all parts of TMGTEXT that are in quotes, replacing them with coded string, and puting vals in ARR  
  ;"RESTORENCAPS(TMGTEXT,ARR) ---Undo changes done by REMOVENCAP
  ;"RESTOREQTS(TMGTEXT,ARR) --Undo changes done by REMOVEQTS  
  ;"CLEAVSTR(TMGTEXT,TMGDIV,TMGPARTB,NCS,OPTION) ;Split string by divider
  ;"SPLITSTR(TMGTEXT,TMGWIDTH,TMGPARTB) ;Wrap string to specified width.
  ;"SPLITB4(TMGTEXT,MATCH,TMGPARTB) -- Split string at match, but return match with part B
  ;"STRIPSTR(TMGTEXT,SUBSTR) --Remove SUBSTR, UNTIL ALL GONE.   
  ;"SETSTLEN(TMGTEXT,TMGWIDTH) ;Make string exactly TMGWIDTH in length
  ;"$$PAD2POS(POS,CH) -- return a string that can be used to pad up to POS
  ;"$$SPLITLN(STR,LINEARRAY,WIDTH,SPECIALINDENT,INDENT,DIVSTR) -- Wrap by WIDTH to array
  ;"SPLIT2AR(TEXT,DIVIDER,ARRAY,INITINDEX,OPTION) -- Slit into array, by DIVIDER
  ;"ARR2STR(ARR,DIVIDER)  -- COMBINE ARRAY ELEMENTS INTO LONG STRING, OPPOSITE OF SPLIT2AR  
  ;"ADDWRAPARR(ARR,STR,MAXWIDTH,NEWLINE) -- Add STR to ARR, wrapping if needed
  ;"STR2WP(STR,PARRAY,WIDTH,DIVCH,INITLINE) -- Take a long string and wrap it into WP format (NOT Fileman 0 based WP format)
  ;"WP2STR(PARRAY,DIVCH,MAXLEN,INITLINE) -- Takes a WP field, and concatenates into one long string.
  ;"WP2ARRAY(REF,OUTREF) -- Convert a Fileman WP array into a flat ARRAY
  ;"ARRAY2WP(REFARRAY,REF) -- Convert an ARRAY to a Fileman-format WP array
  ;"PREFIXLINE2ARR(ARR,LINE) -- Add formatted lines to top of ARR.  
  ;"INSERTARR(OUT,ARR1,ARR2,INSERTIDX) -- Insert merge (potentially overlapping) #-indexed ARR1 into ARR2
  ;"PREFIXARR(ARR,HEADERARR) -- PREFIX HEADER ARRAY ONTO ARR
  ;"APPENDARR(ARR,TAILARR) -- APPEND TAIL ARRAY ONTO ARR
  ;"MAXWIDTH(ARR)  -- Return length of longest line in ARR
  ;"LTRIM(S,TRIMCH) -- LEFT TRIM. 
  ;"RTRIM(S,TRIMCH) -- RIGHT TRIM. 
  ;"EXTR2CHAR(STR,CHAR,STARTPOS)  -- Extract up to (but not including) CHAR
  ;
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
  ;"------------------------------------------------------------------------
  ;"FYI, String functions in XLFSTR module:
  ;"------------------------------------------------------------------------
  ;"$$CJ^XLFSTR(s,I[,p]) -- Returns a center-justified string
  ;"        s=string, I=field size, p(optional)=pad character
  ;"$$LJ^XLFSTR(s,I[,p]) -- Returns a left-justified string
  ;"        s=string, I=field size, p(optional)=pad character
  ;"$$RJ^XLFSTR(s,I[,p]) -- Returns a right-justified string
  ;"        s=string, I=field size, p(optional)=pad character
  ;"$$INVERT^XLFSTR(s) -- returns an inverted string (i.e. "ABC"-->"CBA")
  ;"$$LOW^XLFSTR(s) -- returns string with all letters converted to lower-case
  ;"$$UP^XLFSTR(s) -- returns string with all letters converted to upper-case
  ;"$$TRIM^XLFSTR(s,[LRFlags],[char])
  ;"$$REPEAT^XLFSTR(s,Count) -- returns a string that is a repeat of s Count times
  ;"$$REPLACE^XLFSTR(s,.spec) -- Uses a multi-character $TRanslate to return a
  ;"                                string with the specified string replaced
  ;"        s=input string, spec=array passed by reference
  ;"        spec format:
  ;"        spec("Any_Search_String")="Replacement_String"
  ;"$$STRIP^XLFSTR(s,Char) -- returns string striped of all instances of Char
  ;"=======================================================================
  ;
CAPWORDS(S,DIV)  ;
  ;"Purpose: convert each word in the string: '
  ;"       'test string' --> 'Test String'
  ;"       'TEST STRING' --> 'Test String'
  ;"Input: S -- the string to convert
  ;"        DIV -- [OPTIONAL] the character used to separate string (default is ' ' [space])
  ;"Result: returns the converted string
  NEW S2,PART,IDX,RESULT SET RESULT=""
  SET DIV=$GET(DIV," ")
  SET S2=$$LOW^XLFSTR(S)
  FOR IDX=1:1 DO  QUIT:PART=""
  . SET PART=$PIECE(S2,DIV,IDX) QUIT:PART=""
  . SET $EXTRACT(PART,1)=$$UP^XLFSTR($EXTRACT(PART,1))
  . IF RESULT'="" SET RESULT=RESULT_DIV
  . SET RESULT=RESULT_PART
  QUIT RESULT
  ;
CAP1ST(WORD)  ;"Capitalize only first letter of word.    
  NEW RESULT SET RESULT=$$UP^XLFSTR($EXTRACT(WORD,1))
  SET RESULT=RESULT_$$LOW^XLFSTR($EXTRACT(WORD,2,$LENGTH(WORD)))
  QUIT RESULT
  ;
CAP1STAL(SENTENCE,DIVCHS)  ;"Capitalize only first letter of word, for each word in sentence
  ;"INPUT: SENTENCE.  A series of words, to be capitalized
  ;"       DIVCHS -- OPTIONAL.  Default is " "
  ;"         Can be passed in 1 of two ways, 
  ;"         e.g. "; ,./" , a series of single character divisors, or
  ;"         e.g. DIVCH("{{")=""
  ;"              DIVCH("}}")=""  And array of multiple character divisors
  ;"              DIVCH("--")=""  
  ;"         OR a combination of both.  
  ;"RESULT: Returns string that has been capitalized
  IF $DATA(DIVCHS)=0 SET DIVCHS=" "
  NEW RESULT,AWORD,DIV,TEMPS SET TEMPS=$GET(SENTENCE),RESULT=""
  FOR  QUIT:TEMPS=""  DO
  . SET AWORD=$$NEXTWORD^TMGSTUT3(TEMPS,.DIVCHS,.DIV)
  . SET AWORD=$$CAP1ST(AWORD)_DIV
  . SET RESULT=RESULT_AWORD
  . SET TEMPS=$EXTRACT(TEMPS,$LENGTH(AWORD)+1,$LENGTH(TEMPS))        
  QUIT RESULT
  ; 
REMOVENCAP(TMGTEXT,ARR,ENCAP)  ;"Remove all parts of TMGTEXT that are in ENCAPSULATOR, replacing them with coded string, and puting vals in ARR
  ;"See details in REMOVEQTS.  This function allows other encapsulators, other than quotes
  ;"INPUT: TMGTEXT -- text to work on
  ;"       ARR -- PASS BY REFERENCE, AN OUT PARAMETER.  
  ;"       ENCAP -- e.g. "{" or "(", or "|", or """"
  ;"RESULT: Returns encoded string.  
  ;
  NEW MAP DO MAPMATCH^TMGSTUT3(TMGTEXT,.MAP,ENCAP)
  NEW OPENTAG SET OPENTAG="%@"
  NEW CLOSETAG SET CLOSETAG="@%"
  NEW PARTA,PARTB SET (PARTA,PARTB)=""
  NEW STARTPOS SET STARTPOS=1
  NEW RESULT SET RESULT=""
  NEW IDX SET IDX=1
  NEW GROUP SET GROUP=0
  FOR  SET GROUP=$ORDER(MAP(GROUP)) QUIT:GROUP'>0  DO
  . NEW DEPTH SET DEPTH=1  ;"ONLY SUPPORTING DEPTH=1 FOR NOW
  . NEW P1 SET P1=$GET(MAP(GROUP,DEPTH,"Pos",1))
  . NEW P2 SET P2=$GET(MAP(GROUP,DEPTH,"Pos",2))
  . NEW XSTR SET XSTR=$EXTRACT(TMGTEXT,P1+1,P2-1)
  . SET RESULT=RESULT_$EXTRACT(TMGTEXT,STARTPOS,P1-1)_OPENTAG_IDX_CLOSETAG
  . SET ARR(IDX)=XSTR,IDX=IDX+1
  . SET STARTPOS=P2+1
  IF $DATA(ARR) SET ARR("ENCAPSULATOR")=ENCAP
  SET RESULT=RESULT_$EXTRACT(TMGTEXT,STARTPOS,$LENGTH(TMGTEXT))
  QUIT RESULT
  ;   
REMOVEQTS(TMGTEXT,ARR)  ;"Remove all parts of TMGTEXT that are in quotes, replacing them with coded string, and puting vals in ARR
  ;"Example: if TMGTEXT='A="APPLE JACKS" B="BILLY BOB" C="CRACKED CORN"', then output would be:
  ;"         'A=%@1@% B=%@2@% C=%@3@%' and ARR(1)="APPLE JACKS",ARR(2)="BILLY BOB",ARR(3)="CRACKED CORN"
  ;"Purpose: this is a utility function, to remove quotes, so that manpulation can be done that ignores anything inside quotes.
  ;"NOTE: Later, if I need a different replacement value than %@ @%, I could add an OPTION parameter.  
  ;"INPUT: TMGTEXT -- text to work on
  ;"       ARR -- PASS BY REFERENCE, AN OUT PARAMETER.  
  ;"RESULT: Returns encoded string.  
  ;
  NEW RESULT SET RESULT=$$REMOVENCAP(.TMGTEXT,.ARR,"""")
  QUIT RESULT
  ;"DELETE BELOW LATER...  6/2/24
  ;"NEW MAP DO MAPMATCH^TMGSTUT3(TMGTEXT,.MAP,"""")
  ;"NEW OPENTAG SET OPENTAG="%@"
  ;"NEW CLOSETAG SET CLOSETAG="@%"
  ;"NEW PARTA,PARTB SET (PARTA,PARTB)=""
  ;"NEW STARTPOS SET STARTPOS=1
  ;"NEW RESULT SET RESULT=""
  ;"NEW IDX SET IDX=1
  ;"NEW GROUP SET GROUP=0
  ;"FOR  SET GROUP=$ORDER(MAP(GROUP)) QUIT:GROUP'>0  DO
  ;". NEW DEPTH SET DEPTH=1  ;"ONLY SUPPORTING DEPTH=1 FOR NOW
  ;". NEW P1 SET P1=$GET(MAP(GROUP,DEPTH,"Pos",1))
  ;". NEW P2 SET P2=$GET(MAP(GROUP,DEPTH,"Pos",2))
  ;". NEW XSTR SET XSTR=$EXTRACT(TMGTEXT,P1+1,P2-1)
  ;". SET RESULT=RESULT_$EXTRACT(TMGTEXT,STARTPOS,P1-1)_OPENTAG_IDX_CLOSETAG
  ;". SET ARR(IDX)=XSTR,IDX=IDX+1
  ;". SET STARTPOS=P2+1
  ;"SET RESULT=RESULT_$EXTRACT(TMGTEXT,STARTPOS,$LENGTH(TMGTEXT))
  ;"QUIT RESULT
  ;
RESTORENCAPS(TMGTEXT,ARR) ;"Undo changes done by REMOVENCAP
  ;"Example: if TMGTEXT='A=%@1@% B=%@2@% C=%@3@%' and ARR(1)="APPLE JACKS",ARR(2)="BILLY BOB",ARR(3)="CRACKED CORN"
  ;"      Then output would be: 'A="APPLE JACKS" B="BILLY BOB" C="CRACKED CORN"'
  ;"Purpose: this is a utility function, to restore encapsulators, after manpulation has be done that ignores anything inside them.
  ;"NOTE: Later, if I need a different replacement value than %@ @%, I could add an OPTION parameter.  
  ;"INPUT: TMGTEXT
  ;"       ARR -- array as created by REMOVEENCAPS or REMOVEQTS
  ;"       ENCAP -- e.g. "{" or "(", or "|", or """"
  NEW OPENTAG SET OPENTAG="%@"
  NEW CLOSETAG SET CLOSETAG="@%"
  NEW RESULT SET RESULT=$GET(TMGTEXT)
  NEW ENCAP SET ENCAP=$GET(ARR("ENCAPSULATOR"),"""")
  NEW MATCH DO GETMATCHENCAP^TMGSTUT3(.MATCH,ENCAP)
  NEW CLOSEENCAP SET CLOSEENCAP=$GET(MATCH(ENCAP),"""")
  NEW DONE SET DONE=0
  FOR  DO  QUIT:DONE=1
  . SET DONE=(RESULT'[OPENTAG) QUIT:DONE=1
  . NEW PARTA SET PARTA=$PIECE(RESULT,OPENTAG,1)
  . NEW PARTB SET PARTB=$PIECE(RESULT,OPENTAG,2,999)
  . NEW IDX SET IDX=+PARTB
  . SET PARTB=$PIECE(RESULT,CLOSETAG,2,999)
  . SET RESULT=PARTA_ENCAP_$GET(ARR(IDX))_CLOSEENCAP_PARTB
  QUIT RESULT
  ;    
RESTOREQTS(TMGTEXT,ARR) ;"Undo changes done by REMOVEQTS
  QUIT $$RESTORENCAPS(.TMGTEXT,.ARR)
  ;"DELETE BELOW LATER...  6/2/24
  ;";"Example: if TMGTEXT='A=%@1@% B=%@2@% C=%@3@%' and ARR(1)="APPLE JACKS",ARR(2)="BILLY BOB",ARR(3)="CRACKED CORN"
  ;";"      Then output would be: 'A="APPLE JACKS" B="BILLY BOB" C="CRACKED CORN"'
  ;";"Purpose: this is a utility function, to restore quotes, after manpulation has be done that ignores anything inside quotes.
  ;";"NOTE: Later, if I need a different replacement value than %@ @%, I could add an OPTION parameter.  
  ;";"INPUT: TMGTEXT
  ;"NEW OPENTAG SET OPENTAG="%@"
  ;"NEW CLOSETAG SET CLOSETAG="@%"
  ;"NEW RESULT SET RESULT=$GET(TMGTEXT)
  ;"NEW DONE SET DONE=0
  ;"FOR  DO  QUIT:DONE=1
  ;". SET DONE=(RESULT'[OPENTAG) QUIT:DONE=1
  ;". NEW PARTA SET PARTA=$PIECE(RESULT,OPENTAG,1)
  ;". NEW PARTB SET PARTB=$PIECE(RESULT,OPENTAG,2,999)
  ;". NEW IDX SET IDX=+PARTB
  ;". SET PARTB=$PIECE(RESULT,CLOSETAG,2,999)
  ;". SET RESULT=PARTA_""""_$GET(ARR(IDX))_""""_PARTB
  ;"QUIT RESULT
  ;  
CLEAVSTR(TMGTEXT,TMGDIV,TMGPARTB,NCS,OPTION) ;
  ;"Purpse: To take a string, delineated by 'TMGDIV'
  ;"        and to split it into two parts: TMGTEXT and TMGPARTB
  ;"         e.g. TMGTEXT="Hello\nThere"
  ;"             TMGDIV="\n"
  ;"           Function will result in: TMGTEXT="Hello", TMGPARTB="There"
  ;"Input:  TMGTEXT - the input string **SHOULD BE PASSED BY REFERENCE.
  ;"        TMGDIV - the delineating string
  ;"                If TMGDIV="{!AN}", then string will be divided by any string of non Alphanumeric characters
  ;"                e.g. 'slow_*_down', then "_*_" would be used as delimiter since it is not made of alpha or numeric chars.
  ;"                NOTE: If passed by reference, then it will be change to divider used.  
  ;"        TMGPARTB - the string to get second part **SHOULD BE PASSED BY REFERENCE.
  ;"        NCS - OPTIONAL. NCS='NotCaseSensitive'.  If 1 then TMGDIV split is not case sensitive
  ;"        OPTION -- OPTIONAL.  
  ;"              OPTION("TRIM DIV") = 1 will cause further leading 'whitespace' divs to be trimmed from partB
  ;"              OPTION("NO SPLIT IN QUOTES")=1 will prevent splitting inside quotes.  
  ;"                  E.g. 'A="APPLE JACKS" B="BILLY BOB" C="CRACKED CORN"' and divider is " ", would not split on spaces inside quotes.  
  ;"Output: TMGTEXT and TMGPARTB will be changed
  ;"        Function will result in: TMGTEXT="Hello", TMGPARTB="There"
  ;"Result: none
  IF '$DATA(TMGTEXT) GOTO CSDONE
  IF '$DATA(TMGDIV) GOTO CSDONE
  NEW PROTECTQT SET PROTECTQT=($GET(OPTION("NO SPLIT IN QUOTES"))=1)
  NEW PROTECTARR
  IF PROTECTQT SET TMGTEXT=$$REMOVEQTS(TMGTEXT,.PROTECTARR)
  IF TMGDIV="{!AN}" DO  ;"Find first NON-ALPHA-NUMERIC segment and use as TMGDIV
  . SET TMGDIV=""
  . NEW DONE SET DONE=0
  . NEW INDIV SET INDIV=0
  . NEW TEMPS SET TEMPS=""
  . NEW IDX FOR IDX=1:1:$LENGTH(TMGTEXT) QUIT:DONE  DO
  . . NEW CH SET CH=$EXTRACT(TMGTEXT,IDX)
  . . NEW APHN SET APHN=$$ISALPHNUM^TMGSTUT3(CH)
  . . IF CH=".",(+TEMPS=TEMPS) SET APHN=1
  . . SET TEMPS=TEMPS_CH
  . . IF INDIV=1,APHN=0 SET DONE=1 QUIT
  . . IF APHN=0 SET INDIV=1,TMGDIV=TMGDIV_CH
  NEW TMGSAVE SET TMGSAVE=TMGTEXT
  SET NCS=+$GET(NCS)
  IF NCS=1 DO
  . SET TMGTEXT=$$UP^XLFSTR(TMGTEXT)
  . SET TMGDIV=$$UP^XLFSTR(TMGDIV)
  SET TMGPARTB=""
  NEW TMGPARTA
  IF TMGTEXT[TMGDIV DO
  . SET TMGPARTA=$PIECE(TMGTEXT,TMGDIV,1)
  . SET TMGPARTB=$PIECE(TMGTEXT,TMGDIV,2,256)
  . IF NCS=1 DO  ;"NCS support added 12/9/16
  . . NEW LEN SET LEN=$LENGTH(TMGPARTA)
  . . SET TMGTEXT=$EXTRACT(TMGSAVE,1,LEN)
  . . SET TMGPARTB=$EXTRACT(TMGSAVE,LEN+$LENGTH(TMGDIV)+1,$LENGTH(TMGSAVE))
  . ELSE  DO
  . . SET TMGTEXT=TMGPARTA
  IF PROTECTQT DO
  . SET TMGTEXT=$$RESTOREQTS(TMGTEXT,.PROTECTARR)
  . SET TMGPARTB=$$RESTOREQTS(TMGPARTB,.PROTECTARR)  
  IF $GET(OPTION("TRIM DIV"))=1 DO
  . NEW DIVL SET DIVL=$LENGTH(TMGDIV)
  . FOR  QUIT:$EXTRACT(TMGPARTB,1,DIVL)'=TMGDIV  SET TMGPARTB=$EXTRACT(TMGPARTB,DIVL+1,$LENGTH(TMGPARTB))
CSDONE  ;
  QUIT
  ;
SPLITSTR(TMGTEXT,TMGWIDTH,TMGPARTB) ;
  ;"PUBLIC FUNCTION
  ;"Purpose: To a string into two parts.  The first part will fit within 'TMGWIDTH'
  ;"           the second part is what is left over
  ;"          The split will be intelligent, so words are not divided (splits at a space)
  ;"Input:  TMGTEXT = input text.  **Should be passed by reference
  ;"          TMGWIDTH = the constraining wIdth
  ;"        TMGPARTB = the left over part. **Should be passed by reference
  ;"output: TMGTEXT and TMGPARTB are modified
  ;"result: none.
  NEW LEN,TMGS1
  SET TMGWIDTH=$GET(TMGWIDTH,80)
  NEW SPACEFOUND SET SPACEFOUND=0
  NEW SPLITPOINT SET SPLITPOINT=TMGWIDTH
  SET TMGTEXT=$GET(TMGTEXT)
  SET TMGPARTB=""
  ;
  SET LEN=$LENGTH(TMGTEXT)
  IF LEN>TMGWIDTH DO
  . NEW TMGCH
  . FOR SPLITPOINT=SPLITPOINT:-1:1 DO  QUIT:SPACEFOUND
  . . SET TMGCH=$EXTRACT(TMGTEXT,SPLITPOINT,SPLITPOINT)
  . . SET SPACEFOUND=(TMGCH=" ")
  . IF 'SPACEFOUND SET SPLITPOINT=TMGWIDTH
  . SET TMGS1=$EXTRACT(TMGTEXT,1,SPLITPOINT)
  . SET TMGPARTB=$EXTRACT(TMGTEXT,SPLITPOINT+1,1024)  ;"max String length=1024
  . SET TMGTEXT=TMGS1
  ;
  QUIT
  ;
SPLITB4(TMGTEXT,MATCH,TMGPARTB)  ;"Split string at match, but return match  
  ;"PUBLIC FUNCTION
  ;"Purpose: To a string into two parts.  The first part will be everything 
  ;"           up to, but NOT including MATCH.
  ;"           The second part is MATCH and everything left over
  ;"Input:  TMGTEXT = input text.       **Should be passed by reference
  ;"        TMGPARTB = the second part. **Should be passed by reference
  ;"output: TMGTEXT and TMGPARTB are modified
  ;"result: none.
  SET TMGPARTB=""
  IF TMGTEXT'[MATCH QUIT
  NEW PARTA SET PARTA=$PIECE(TMGTEXT,MATCH,1)
  SET TMGPARTB=MATCH_$PIECE(TMGTEXT,MATCH,2,99)
  SET TMGTEXT=PARTA
  QUIT
  ;
STRIPSTR(TMGTEXT,SUBSTR) ;"Remove SUBSTR, UNTIL ALL GONE. 
  ;"PUBLIC FUNCTION
  ;"Purpose: Remove SUBSTR, UNTIL ALL GONE. 
  ;"Input:  TMGTEXT = input text.      
  ;"Result: returns TMGTEXT with all instances of SUBSTR removed
  NEW TEMPSTR SET TEMPSTR=$GET(TMGTEXT)
  FOR  QUIT:(TEMPSTR'[SUBSTR)  DO
  . NEW PARTB SET PARTB=$PIECE(TEMPSTR,SUBSTR,2,9999)
  . SET TEMPSTR=$PIECE(TEMPSTR,SUBSTR,1)
  . SET TEMPSTR=TEMPSTR_PARTB
  QUIT TEMPSTR
  ;
SETSTLEN(TMGTEXT,TMGWIDTH) ;SET STRING LEN
  ;"PUBLIC FUNCTION
  ;"Purpose: To make string exactly TMGWIDTH in length
  ;"  Shorten as needed, or pad with terminal spaces as needed.
  ;"Input: TMGTEXT -- should be passed as reference.  This is string to alter.
  ;"       TMGWIDTH -- the desIred wIdth
  ;"Results: none.
  SET TMGTEXT=$GET(TMGTEXT)
  SET TMGWIDTH=$GET(TMGWIDTH,80)
  NEW TMGRESULT SET TMGRESULT=TMGTEXT
  NEW TMGI,LEN
  SET LEN=$LENGTH(TMGRESULT)
  IF LEN>TMGWIDTH DO
  . SET TMGRESULT=$EXTRACT(TMGRESULT,1,TMGWIDTH)
  ELSE  IF LEN<TMGWIDTH DO
  . FOR TMGI=1:1:(TMGWIDTH-LEN) SET TMGRESULT=TMGRESULT_" "
  SET TMGTEXT=TMGRESULT  ;"pass back changes
  QUIT
  ;
PAD2POS(POS,CH) ;
  ;"Purpose: return a string that can be used to pad from the current $X
  ;"         screen cursor position, up to POS, using char Ch (optional)
  ;"Input: POS -- a screen X cursor position, i.e. from 1-80 etc (depending on screen wIdth)
  ;"       CH -- Optional, default is " "
  ;"Result: returns string of padded characters.
  NEW WIDTH SET WIDTH=+$GET(POS)-$X IF WIDTH'>0 SET WIDTH=0
  QUIT $$LJ^XLFSTR("",WIDTH,.CH)
  ;
SPLITLN(STR,LINEARRAY,WIDTH,SPECIALINDENT,INDENT,DIVSTR)  ;"SPLIT LINE
  ;"Scope: PUBLIC FUNCTION
  ;"Purpose: To take a long line, and wrap into an array, such that each
  ;"        line is not longer than WIDTH.
  ;"        Line breaks will be made at spaces (or DIVSTR), unless there are
  ;"        no spaces (of divS) in the entire line (in which case, the line
  ;"        will be divided at WIDTH).
  ;"Input: STR= string with the long line. **If passed by reference**, then
  ;"                it WILL BE CHANGED to equal the last line of array.
  ;"       LineArray -- MUST BE PASSED BY REFERENCE. This OUT varIable will
  ;"                receive the resulting array.
  ;"                e.g. LineArray(1)=first Line.
  ;"                     LineArray(2)=Second Line. ...
  ;"       WIDTH = the desIred wrap wIdth.
  ;"       SPECIALINDENT [OPTIONAL]: If 1, then wrapping is done like this:
  ;"                "   This is a very long line......"
  ;"           will be wrapped lIke this:
  ;"                "   This is a very
  ;"                "   long line ...
  ;"          Notice that the leading space is copied subsequent line.
  ;"          Also, a line like this:
  ;"                "   1. Here is the beginning of a paragraph that is very long..."
  ;"            will be wrapped like this:
  ;"                "   1. Here is the beginning of a paragraph
  ;"                "      that is very long..."
  ;"          Notice that a pattern '#. ' causes the wrapping to match the start
  ;"                of the text on the line above.
  ;"       INDENT [OPTIONAL]: Any absolute amount that all lines should be indented by.
  ;"                This could be used If this long line is continuation of an
  ;"                indentation above it.
  ;"       DIVSTR [OPTIONAL] : Default is " ", this is the divider character
  ;"                         or string, that will represent dividers between
  ;"                         words or phrases
  ;"Result: resulting number of lines (1 If no wrap needed).
  ;
  NEW RESULT SET RESULT=0
  KILL LINEARRAY
  IF ($GET(STR)="")!($GET(WIDTH)'>0) GOTO SPDONE
  NEW INDEX SET INDEX=0
  NEW TEMPSTR,SPLITPOINT
  SET DIVSTR=$GET(DIVSTR," ")
  NEW PRESPACE SET PRESPACE=$$NEEDEDWS^TMGSTUT3(STR,.SPECIALINDENT,.INDENT)
  ;
  IF ($LENGTH(STR)>WIDTH) FOR  DO  QUIT:($LENGTH(STR)'>WIDTH)
  . FOR SPLITPOINT=1:1:WIDTH DO  QUIT:($LENGTH(TEMPSTR)>WIDTH)
  . . SET TEMPSTR=$PIECE(STR,DIVSTR,1,SPLITPOINT)
  . IF SPLITPOINT>1 DO
  . . SET TEMPSTR=$PIECE(STR,DIVSTR,1,SPLITPOINT-1)
  . . SET STR=$PIECE(STR,DIVSTR,SPLITPOINT,9999)  ;"6/12/20 4th param was WIDTH. Changed to 9999
  . ELSE  DO
  . . ;"We must have a word > WIDTH with no spaces--so just divide
  . . SET TEMPSTR=$EXTRACT(STR,1,WIDTH)
  . . SET STR=$EXTRACT(STR,WIDTH+1,9999)
  . SET INDEX=INDEX+1
  . SET LINEARRAY(INDEX)=TEMPSTR
  . SET STR=PRESPACE_STR
  ;
  SET INDEX=INDEX+1
  SET LINEARRAY(INDEX)=STR
  SET RESULT=INDEX
SPDONE  ;
  QUIT RESULT
  ;
SPLIT2AR(TEXT,DIVIDER,ARRAY,INITINDEX,OPTION)  ;"CleaveToArray
  ;"Purpose: To take a string, delineated by 'divider' and
  ;"        to split it up into all its parts, putting each part
  ;"        into an array.  e.g.:
  ;"        This/is/A/Test, with '/' divider would result in
  ;"        ARRAY(1)="This"
  ;"        ARRAY(2)="is"
  ;"        ARRAY(3)="A"
  ;"        ARRAY(4)="Test"
  ;"        ARRAY(CMAXNODE)=4    ;CMAXNODE="MAXNODE"
  ;"Input: TEXT - the input string -- should NOT be passed by reference.
  ;"       DIVIDER - the delineating string
  ;"                If DIVIDER="{!AN}", then string will be divided by any string of non Alphanumeric characters
  ;"                e.g. 'slow_*_down', then "_*_" would be used as delimiter since it is not made of alpha or numeric chars.    
  ;"       ARRAY - The array to receive output **SHOULD BE PASSED BY REFERENCE.
  ;"       INITINDEX - OPTIONAL -- The index of the array to start with, I.e. 0 or 1. Default=1
  ;"       OPTION - OPTIONAL
  ;"              OPTION("TRIM DIV") = 1 will cause repeat dividers to be ignored like whitespace.  E.g. "cat cow    duck" gives only 3 entries.
  ;"              OPTION("NO SPLIT IN QUOTES")=1 will prevent splitting inside quotes.  
  ;"                  E.g. 'A="APPLE JACKS" B="BILLY BOB" C="CRACKED CORN"' and divider is " ", would not split on spaces inside quotes.  
  ;"Output: ARRAY is changed, as outlined above
  ;"Result: none
  ;"Notes:  Note -- TEXT is NOT changed (unless passed by reference, in
  ;"                which case the next to the last piece is put into TEXT)
  ;"        ARRAY is killed, the filled with data **ONLY** IF DIVISIONS FOUND
  ;"        Limit of 256 nodes
  ;"        If CMAXNODE is not defined, "MAXNODE" will be used
  SET INITINDEX=$GET(INITINDEX,1)
  NEW PARTB
  NEW COUNT SET COUNT=INITINDEX
  NEW CMAXNODE SET CMAXNODE=$GET(CMAXNODE,"MAXNODE")
  KILL ARRAY  ;"Clear out any old data
  NEW OPT MERGE OPT=OPTION
  NEW PROTECTQT SET PROTECTQT=($GET(OPT("NO SPLIT IN QUOTES"))=1)
  NEW PROTECTARR
  IF PROTECTQT DO
  . SET TEXT=$$REMOVEQTS^TMGSTUT2(TEXT,.PROTECTARR)
  . KILL OPT("NO SPLIT IN QUOTES")  ;"don't want CLEAVSTR^TMGSTUT2 to use this since we handled. 
  ;
C2AL1  ;
  IF '(TEXT[DIVIDER) DO  GOTO C2ADN
  . SET ARRAY(COUNT)=TEXT ;"put it all into line.
  . SET ARRAY(CMAXNODE)=COUNT
  DO CLEAVSTR(.TEXT,DIVIDER,.PARTB,,.OPT)
  SET ARRAY(COUNT)=TEXT
  SET ARRAY(CMAXNODE)=COUNT
  SET COUNT=COUNT+1
  IF '(PARTB[DIVIDER) DO  GOTO C2ADN
  . SET ARRAY(COUNT)=PARTB
  . SET ARRAY(CMAXNODE)=COUNT
  ELSE  DO  GOTO C2AL1
  . SET TEXT=$GET(PARTB)
  . SET PARTB=""
C2ADN  ;
  IF PROTECTQT DO
  . NEW IDX SET IDX=INITINDEX-1
  . FOR  SET IDX=$ORDER(ARRAY(IDX)) QUIT:IDX'>0  DO
  . . NEW STR SET STR=$GET(ARRAY(IDX)) QUIT:STR=""
  . . SET STR=$$RESTOREQTS^TMGSTUT2(STR,.PROTECTARR)
  . . SET ARRAY(IDX)=STR
  QUIT
  ;
ARR2STR(ARR,DIVIDER)  ;"COMBINE ARRAY ELEMENTS INTO LONG STRING, OPPOSITE OF SPLIT2AR
  NEW RESULT SET RESULT=""
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:IDX'>0  DO
  . SET RESULT=RESULT_$GET(ARR(IDX))_DIVIDER
  QUIT RESULT
  ; 
ADDWRAPARR(ARR,MAXWIDTH,NEWLINE,STR) ;"Add STR to ARR, wrapping if needed
  ;"Input: ARR -- IN and OUT PARAMETER
  ;"       MAXWIDTH -- OPTIONAL.  If not provided, defaults to 250
  ;"       NEWLINE -- if 1, then STR added on a new line.  Otherwise added to last line in ARR
  ;"       STR -- string to add
  SET NEWLINE=+$GET(NEWLINE)
  NEW LINENUM SET LINENUM=+$ORDER(ARR(""),-1)
  IF LINENUM=0 SET LINENUM=1,NEWLINE=0
  SET MAXWIDTH=+$GET(MAXWIDTH) IF MAXWIDTH=0 SET MAXWIDTH=250
  NEW PARTB SET PARTB=""
  IF NEWLINE DO
  . SET LINENUM=LINENUM+1
  ELSE  DO
  . SET STR=$GET(ARR(LINENUM))_STR
  DO SPLITSTR(.STR,MAXWIDTH,.PARTB)
  SET ARR(LINENUM)=STR,LINENUM=LINENUM+1
  IF PARTB'="" SET ARR(LINENUM)=PARTB  
  QUIT  
  ;
STR2WP(STR,PARRAY,WIDTH,DIVCH,INITLINE)  ;
  ;"Purpose: to take a long string and wrap it into WP format  (NOT Fileman 0 based WP format)
  ;"Input: STR:  the long string to wrap into the WP array (but not 0 based)
  ;"      PARRAY: the NAME of the array to put output into.
  ;"              Any pre-existing data in this array will NOT be killed
  ;"      WIDTH: OPTIONAL -- the width to target for word wrapping. Default is 60
  ;"      DIVCH: OPTIONAL -- the character to use separate words (to allow nice wrapping). Default is " "
  ;"      INITLINE: OPTIONAL -- the line to start putting data into.  Default is 1
  ;"Output: PARRAY will be filled as follows:
  ;"          @PARRAY@(INITLINE+0)=line 1
  ;"          @PARRAY@(INITLINE+1)=line 2
  ;"          @PARRAY@(INITLINE+2)=line 3
  ;"Result: none.
  IF +$GET(WIDTH)=0 SET WIDTH=60
  IF $GET(DIVCH)="" SET DIVCH=" "
  NEW TEMPS SET TEMPS=$GET(STR)
  IF $GET(INITLINE)="" SET INITLINE=1
  NEW CURLINE SET CURLINE=+INITLINE
  FOR  DO  QUIT:(TEMPS="")
  . NEW S1,S2
  . DO NICESPLT^TMGSTUT3(TEMPS,WIDTH,.S1,.S2,,DIVCH)
  . SET @PARRAY@(CURLINE)=S1
  . SET CURLINE=CURLINE+1
  . SET TEMPS=S2
  QUIT
  ;
WP2STR(PARRAY,DIVCH,MAXLEN,INITLINE)  ;
  ;"Purpose: This is the opposite of STR2WP.  It takes a WP field, and concatenates
  ;"         each line to make one long string.
  ;"Input: PARRAY: the NAME of the array to get WP lines from. Expected format as follows
  ;"          @PARRAY@(INITLINE+0)=line 1
  ;"          @PARRAY@(INITLINE+1)=line 2
  ;"          @PARRAY@(INITLINE+2)=line 3
  ;"              -or-
  ;"          @PARRAY@(INITLINE+0,0)=line 1
  ;"          @PARRAY@(INITLINE+1,0)=line 2
  ;"          @PARRAY@(INITLINE+2,0)=line 3
  ;"       DIVCH: OPTIONAL, default is " ".  This character is appended to the end of each line, e.g
  ;"              output=output_line1_DIVCH_line2
  ;"       MAXLEN: OPTIONAL, default=255.  The maximum allowable length of the resulting string.
  ;"       INITLINE: OPTIONAL -- the line in PARRAY to start reading data from.  Default is 1
  ;"RESULT: Returns one long string representing the WP array
  ;
  NEW I,ONELINE,RESULT,LEN
  SET I=$GET(INITLINE,1)
  SET RESULT=""
  SET DIVCH=$GET(DIVCH," ")
  SET MAXLEN=$GET(MAXLEN,255)
  SET LEN=0
  FOR  DO  QUIT:(ONELINE="")!(LEN'<MAXLEN)!(+I'>0)
  . SET ONELINE=$GET(@PARRAY@(I))
  . IF ONELINE="" SET ONELINE=$GET(@PARRAY@(I,0))
  . IF ONELINE="" QUIT
  . SET LEN=$LENGTH(RESULT)+$LENGTH(DIVCH)
  . IF LEN+$LENGTH(ONELINE)>MAXLEN DO
  . . SET ONELINE=$EXTRACT(ONELINE,1,(MAXLEN-LEN))
  . SET RESULT=RESULT_ONELINE_DIVCH
  . SET LEN=LEN+$LENGTH(ONELINE)
  . SET I=$ORDER(@PARRAY@(I))
  QUIT RESULT
  ;
WP2ARRAY(REF,OUTREF) ;
  ;"Purpose: to convert a Fileman WP array into a flat ARRAY
  ;"Input:REF -- The reference to the header node (e.g.  "^TMG(22702,99,1)" for example below)
  ;"      ARRAY -- REFERENCE to OUT PARAMETER.  PrIor values killed.
  ;"Note:  The format of a WP field is as follows:
  ;"      e.g.    ^TMG(22702,99,1,0) = ^^4^4^3050118^
  ;"               ^TMG(22702,99,1,1,0) = Here is the first line of text
  ;"               ^TMG(22702,99,1,2,0) = And here is another line
  ;"               ^TMG(22702,99,1,3,0) =
  ;"               ^TMG(22702,99,1,4,0) = And here is a final line
  ;"  And the format of the 0 node is: ^^<line count>^<linecount>^<fmdate>^^
  ;"Output: @OUTREF@(1) -- 1st line
  ;"        @OUTREF@(2) -- 2nd line ... etc.
  ;"RESULTS: none
  NEW TREF SET TREF=$GET(REF)
  SET OUTREF=$GET(OUTREF)
  NEW ORIGREF SET ORIGREF=TREF
  NEW LENR SET LENR=$LENGTH(ORIGREF)
  IF OUTREF="" GOTO W2ADN
  KILL @OUTREF
  IF $DATA(@TREF)=0 GOTO W2ADN
  NEW I SET I=0
  FOR  SET TREF=$QUERY(@TREF) QUIT:(TREF="")!($$CREF^DILF($EXTRACT(TREF,1,LENR))'=ORIGREF)  DO
  . SET @OUTREF@(I)=@TREF
  . SET I=I+1
  IF $LENGTH($GET(@OUTREF@(0)),"^")>5 KILL @OUTREF@(0)
W2ADN   ;
  QUIT
  ;
ARRAY2WP(REFARRAY,REF) ;
  ;"Purpose: to convert an ARRAY to a Fileman WP array
  ;"input:REFARRAY -- Reference (aka 'name') of plain array containing text
  ;"                 data, to be loaded into a Fileman-format WP array.  E.g.
  ;"                      ARRAY(1) -- 1st line
  ;"                      ARRAY(1,2) -- 2nd line  <-- sub-nodes OK
  ;"                      ARRAY(2) -- 3rd line ... etc.
  ;"             NOTE: Although subnodes are OK for input, at storage, all are stored as same level. 
  ;"                   Consider using ZWRITE^TMGZWR to convert array into text prior to calling this, if needed.
  ;"      REF -- The reference to the header node (e.g.  "^TMG(22702,99,1)" for example below)
  ;"              Prior data in @REF is killed
  ;"Note:  The format of a WP field is as follows:
  ;"      e.g.    ^TMG(22702,99,1,0) = ^^4^4^3050118^
  ;"               ^TMG(22702,99,1,1,0) = Here is the first line of text
  ;"               ^TMG(22702,99,1,2,0) = And here is another line
  ;"               ^TMG(22702,99,1,3,0) =
  ;"               ^TMG(22702,99,1,4,0) = And here is a final line
  ;"  And the format of the 0 node is: ^^<line count>^<linecount>^<fmdate>^^
  ;"Output: @REF is stuffed with data.  No XREF's are triggered.
  ;"RESULTS: none
  SET REF=$GET(REF)
  KILL @REF
  NEW AREF SET AREF=$GET(REFARRAY)
  IF $DATA(@AREF)=0 GOTO A2WDN
  NEW I SET I=0
  FOR  SET AREF=$QUERY(@AREF) QUIT:(AREF="")  DO
  . SET I=I+1
  . SET @REF@(I,0)=$GET(@AREF)
  SET @REF@(0)="^^"_I_"^"_I_"^"_$$NOW^XLFDT
A2WDN   ;
  QUIT
  ;
INSERTARR(OUT,ARR1,ARR2,INSERTIDX) ;"Insert merge (potentially overlapping) #-indexed ARR1 into ARR2
  ;"Input: OUT -- PASS BY REFERENCE, AN OUT PARAMETER.  PRIOR CONTENTS KILLED
  ;"       ARR1 -- The array being inserted.  Format: ARR(#,...)=""
  ;"       ARR2 -- The array that ARR1 is being inserted into.   Format: ARR(#,...)=""
  ;"       INSERTIDX -- The starting index that ARR1 will be inserted BEFORE.
  ;"Results: none, but OUT is modified
  ;"Example:  ARR1(1)='APPLE'
  ;"          ARR1(2,1)='PEAR'
  ;"          ARR1(3,9,2)='ORANGE'
  ;"          ARR1(4)='GRAPE'
  ;"
  ;"          ARR2(1)='BIKE'
  ;"          ARR2(2,5)='SKATES'
  ;"          ARR2(3,'A')='FRISBEE'
  ;"          ARR2(4)='BASEBALL'
  ;"          ARR2(5)='KITE
  ;"'
  ;"          INSERTIDX=4
  ;"
  ;" --> RESULTING OUTPUT:  (notice index renumbering)
  ;"          OUT(1)='BIKE'
  ;"          OUT(2,5)='SKATES'    
  ;"          OUT(3,'A')='FRISBEE' 
  ;"          OUT(4)='APPLE'       <--original ARR1 inserted here
  ;"          OUT(5,1)='PEAR'      <--original ARR1 inserted here
  ;"          OUT(6,9,2)='ORANGE'  <--original ARR1 inserted here
  ;"          OUT(7)='GRAPE'       <--original ARR1 inserted here
  ;"          OUT(8)='BASEBALL'    
  ;"          OUT(9)='KITE         
  
  ;"Results: none.  
  KILL OUT 
  NEW CT SET CT=1
  NEW IDX2 SET IDX2=""
  ;"First get any part of ARR2 before INSERTIDX
  FOR  SET IDX2=$ORDER(ARR2(IDX2)) QUIT:(IDX2'<INSERTIDX)!(IDX2="")  DO
  . MERGE OUT(CT)=ARR2(IDX2) SET CT=CT+1
  ;"Next add ARR1
  NEW IDX1 SET IDX1=""
  FOR  SET IDX1=$ORDER(ARR1(IDX1)) QUIT:(IDX1="")  DO
  . MERGE OUT(CT)=ARR1(IDX1) SET CT=CT+1
  ;"Finally add rest of ARR2
  FOR  DO  SET IDX2=$ORDER(ARR2(IDX2)) QUIT:(IDX2="")
  . MERGE OUT(CT)=ARR2(IDX2) SET CT=CT+1
  QUIT
  ;
PREFIXLINE2ARR(ARR,LINE)  ;"Add formatted lines to top of ARR.  
  ;"Input: ARR.  Line to add to.  Format ARR(#)=<text>
  ;"       LINE -- Data to add.  Format: 'part1^part2^part3^part4...' Each part is added as separate line
  NEW HEADERARR DO SPLIT2AR^TMGSTUT2(LINE,"^",.HEADERARR)
  NEW TEMP DO INSERTARR(.TEMP,.HEADERARR,.ARR,0)
  KILL ARR MERGE ARR=TEMP  
  ;"NEW OUT
  ;"NEW CT SET CT=1
  ;"NEW IDX SET IDX=0
  ;"FOR  SET IDX=$ORDER(HEADER(IDX)) QUIT:IDX'>0  DO
  ;". NEW ALINE SET ALINE=$GET(HEADER(IDX))
  ;". SET OUT(CT)=ALINE,CT=CT+1
  ;"NEW IDX SET IDX=0
  ;"FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:IDX'>0  DO
  ;". NEW ALINE SET ALINE=$GET(ARR(IDX))
  ;". SET OUT(CT)=ALINE,CT=CT+1
  ;"KILL ARR MERGE ARR=OUT  
  QUIT
  ;  
PREFIXARR(ARR,HEADERARR) ;"PREFIX HEADER ARRAY ONTO ARR
  NEW TEMP DO INSERTARR(.TEMP,.HEADERARR,.ARR,0)
  KILL ARR MERGE ARR=TEMP
  QUIT
  ;
APPENDARR(ARR,TAILARR) ;"APPEND TAIL ARRAY ONTO ARR
  NEW LASTIDX SET LASTIDX=$ORDER(ARR(""),-1)
  NEW TEMP DO INSERTARR(.TEMP,.TAILARR,.ARR,LASTIDX+1)
  KILL ARR MERGE ARR=TEMP
  QUIT
  ;
MAXWIDTH(ARR)  ;"Return length of longest line in ARR
  NEW RESULT SET RESULT=0
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:IDX'>0  DO
  . NEW LEN SET LEN=$LENGTH(ARR(IDX))
  . IF LEN>RESULT SET RESULT=LEN
  QUIT RESULT
  ;
LTRIM(S,TRIMCH) ;"LEFT TRIM. 
  ;"Purpose: To a trip a string of leading and trailing white space
  ;"        i.e. convert "    hello   " into "hello"
  ;"Input: S -- the string to convert.  Won't be changed IF passed by reference
  ;"      TRIMCH -- OPTIONAL: Charachter to trim.  Default is " "
  ;"Results: returns modified string
  QUIT $$TRIM^XLFSTR(S,"L",.TRIMCH)
  ;
RTRIM(S,TRIMCH) ;"RIGHT TRIM. 
  ;"Purpose: To a trip a string of trailing white space
  ;"         i.e. convert "hello   " into "hello"
  ;"Input: S -- the string to convert.  Won't be changed IF passed by reference
  ;"      TRIMCH -- OPTIONAL: Charachter to trim.  Default is " "
  ;"Results: returns modified string
  QUIT $$TRIM^XLFSTR(S,"R",.TRIMCH)
  ;
EXTR2CHAR(STR,CHAR,STARTPOS)  ;"Extract up to (but not including) CHAR
  ;"INPUT: STR1 -- string to extract from.  NOT MODIFIED
  ;"       CHAR -- character (or char substring) to search in STR for.  
  ;"       STARTPOS -- optional.  Default is 1.  This is starting position for extraction.
  ;"Result: returns substring fragment, or "" if not found, or invalid input. 
  NEW RESULT SET RESULT=""
  SET STR=$GET(STR) GOTO:STR="" X2CDN
  SET CHAR=$GET(CHAR) GOTO:CHAR="" X2CDN
  SET STARTPOS=+$GET(STARTPOS)\1 IF STARTPOS'>0 SET STARTPOS=1
  NEW POS SET POS=$FIND(STR,CHAR,STARTPOS) GOTO:POS=0 X2CDN
  SET POS=POS-$LENGTH(CHAR)-1  ;"should set POS to the character BEFORE CHAR
  SET RESULT=$EXTRACT(STR,STARTPOS,POS)
X2CDN ;  
  QUIT RESULT