TMGSTUTL ;TMG/kst/String Utilities and Library ;7/17/12, 2/2/14
         ;;1.0;TMG-LIB;**1**;09/01/05
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"TMG STRING UTILITIES
 ;"NOTE: See also TMGSTUT2 for SACC-compliant versions of code
 ;"      If not there, then will slowly migrate code from here to there.
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"SetStrLen^TMGSTUTL(Text,Width)
 ;"$$NESTSPLIT(Text,OpenBracket,CloseBracket,SBefore,S,SAfter) -- Uppercase wrapper for blow. 
 ;"$$NestSplit^TMGSTUTL(Text,OpenBracket,CloseBracket,SBefore,S,SAfter)
 ;"$$FormatArray^TMGSTUTL(InArray,OutArray,Divider)
 ;"$$Trim^TMGSTUTL(S,TrimCh)  ; --> or use $$TRIM^XLFSTR
 ;"$$TrimL^TMGSTUTL(S,TrimCh)
 ;"$$TrimR^TMGSTUTL(S,TrimCh)
 ;"$$TrimRType^TMGSTUTL(S,type)
 ;"CleaveToArray^TMGSTUTL(Text,Divider,Array)
 ;"CleaveStr^TMGSTUTL(Text,Divider,PartB)
 ;"$$ADDWRAP^TMGSTUTL(PriorS,AddS,MaxWidth,IndentS) -- AddS to PriorS, but wrap first wrap IF needed)
 ;"WordWrapArray^TMGSTUTL(.Array,Width,SpecialIndent)
 ;"SplitStr^TMGSTUTL(Text,Width,PartB)
 ;"SplitLine^TMGSTUTL(s,.LineArray,Width)
 ;"WriteWP^TMGSTUTL(NodeRef)
 ;"WPINSERT(REF,LNUM,S) ;insert one line into a WP record at given line number
 ;"WPDEL(REF,LNUM) ;delete one line in a WP record at given line number
 ;"WPFIX(REF) ; fix the line numbers in a WP field to that they are all integers.
 ;"$$LPad^TMGSTUTL(S,width)   ;"NOTE: should use XLFSTR fn below
 ;"$$RPad^TMGSTUTL(S,width)   ;"NOTE: should use XLFSTR fn below
 ;"$$Center^TMGSTUTL(S,width) ;"NOTE: should use XLFSTR fn below
 ;"$$Clip^TMGSTUTL(S,width)
 ;"$$STRB2H^TMGSTUTL(s,F) Convert a string to hex characters
 ;"$$CapWords^TMGSTUTL(S,Divider) ;"capitalize the first character of each word in a string
 ;"$$LinuxStr^TMGSTUTL(S) ;"Convert string to a valid linux filename
 ;"StrToWP^TMGSTUTL(s,pArray,width,DivCh,InitLine)  ;"wrap long string into a WP array
 ;"$$WPToStr^TMGSTUTL(pArray,DivCh,MaxLen,InitLine)
 ;"Comp2Strs(s1,s2) -- compare two strings and assign an arbritrary score to their similarity
 ;"$$PosNum(s,[Num],LeadingSpace) -- return position of a number in a string
 ;"IsNumeric(s) -- deterimine if word s is a numeric
 ;"ScrubNumeric(s) -- remove numeric words from a sentence
 ;"Pos(subStr,s,count) -- return the beginning position of subStr in s
 ;"DiffPos(s1,s2) -- Return the position of the first difference between s1 and s2
 ;"DiffWords(Words1,Words2) -- Return index of first different word between Words arrays
 ;"SimStr(s1,p1,s2,p2) -- return matching string in s1 and s2, starting at position p1,p2
 ;"SimWord(Words1,p1,Words2,p2) -- return the matching words in both words array 1 and 2, starting
 ;"                              at word positions p1 and p2.
 ;"SimPos(s1,s2) -- return the first position that two strings are similar.
 ;"SimWPos(Words1,Words2,DivStr,p1,p2,MatchStr) -- return the first position that two word arrays
 ;"          are similar.  This means the first index in Words array 1 that matches to words in Words array 2.
 ;"DiffStr(s1,s2,DivChr) -- Return how s1 differs from s2.
 ;"CatArray(Words,i1,i2,DivChr) -- return concat array from index1 to index2
 ;"$$InQt(s,Pos) ;"Depreciated -- use $$INQT^TMGSTUT3
 ;"$$HNQTSUB(s,SubStr) --Same as $$HasNonQtSub
 ;"$$HasNonQtSub(s,SubStr) -- return if string s contains SubStr, but not inside quotes.
 ;"$$GetWord(s,Pos,OpenDiv,CloseDiv) ;"Depreciated.  Use $$GETWORD^TMGSTUT3
 ;"$$CmdChStrip(s) -- Strips all characters < #32 from string.
 ;"$$StrBounds(s,p) -- return position of end of string
 ;"NonWhite(s,p) -- return index of first non-whitespace character
 ;"Pad2Pos(Pos,ch) -- return a padding string from current $X up to Pos, using ch
 ;"HTML2TXT(Array) -- Take WP array that is HTML formatted, and strip <P>, and return in a format of 1 line per array node.
 ;"TrimTags(lineS) -- cut out HTML tags (e.g. <...>) from lineS, however, <no data> is protected
 ;"$$ARRTOHF(TMGARRAY,TMGFPATH,TMGFNAME) -- WRITE the array to the host file system
 ;"$$HFTOARR(TMGARRAY,TMGFPATH,TMGFNAME) -- Read array from host file system

 ;"=======================================================================
 ;"Dependancies
 ;"  uses TMGDEBUG for debug messaging.
 ;"=======================================================================
 ;"=======================================================================

 ;"------------------------------------------------------------------------
 ;"FYI, String functions in XLFSTR module:
 ;"------------------------------------------------------------------------
 ;"$$CJ^XLFSTR(s,i[,p]) -- Returns a center-justified string
 ;"        s=string, i=field size, p(optional)=pad character
 ;"$$LJ^XLFSTR(s,i[,p]) -- Returns a left-justified string
 ;"        s=string, i=field size, p(optional)=pad character
 ;"$$RJ^XLFSTR(s,i[,p]) -- Returns a right-justified string
 ;"        s=string, i=field size, p(optional)=pad character
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

SetStrLen(Text,Width)
        DO SETSTLEN^TMGSTUT2(.Text,.Width)
        QUIT

NESTSPLIT(Text,OpenBracket,CloseBracket,SBefore,S,SAfter) ;
  QUIT $$NestSplit(.Text,.OpenBracket,.CloseBracket,.SBefore,.S,.SAfter)
  ;
NestSplit(Text,OpenBracket,CloseBracket,SBefore,S,SAfter)
        ;"PUBLIC FUNCTION
        ;"Purpose: To take a string in this format:
        ;"          Text='a big black {{Data.Section[{{MVar.Num}}]}} chased me'
        ;"        OpenBracket='{{'
        ;"        CloseBracket='}}'
        ;"  and return:
        ;"        SBefore='a big black {{Data.Section['
        ;"        S='MVar.Num
        ;"        SAfter=']}} chased me'
        ;"  Notice that this function will return the INNER-MOST text inside the brackets pair
        ;"  Note: If multiple sets of brackets exist in the string, like this:
        ;"        'I am a {{MVar.Person}} who loves {{MVar.Food}} every day.
        ;"        Then the LAST SET (i.e. MVar.Food) will be returned in S
        ;"
        ;"Input:Text -- the string to operate on
        ;"        OpenBracket -- string with opening brackets (i.e. '(','{', '{{' etc.)
        ;"        CloseBracket -- string with close brackets (i.e. ')','}','}}' etc.)
        ;"        SBefore -- SHOULD BE PASSED BY REFERENCE... to receive results.
        ;"        S -- SHOULD BE PASSED BY REFERENCE... to receive results.
        ;"        SAfter -- SHOULD BE PASSED BY REFERENCE... to receive results.
        ;"Output: SBefore -- returns all text up to innermost opening brackets, or "" IF none
        ;"          S -- returns text INSIDE innermost brackets -- with brackets REMOVED, or "" IF none
        ;"          SAfter -- returns all text after innermost opening brackets, or "" IF none
        ;"          Text is NOT changed
        ;"        NOTE: Above vars must be passed by reference to recieve results.
        ;"Results: 1=valid results returned in output vars.
        ;"           0=No text found inside brackets, so output vars empty.

        SET SBefore="",S="",SAfter=""
        NEW Result SET Result=0

        IF $DATA(Text)#10=0 GOTO QNSp
        ;"do DEBUGMSG^TMGDEBU4(.TMGDBINDENT,"Looking at '",Text,"'")
        IF ($DATA(OpenBracket)#10=0)!($DATA(CloseBracket)#10=0) GOTO QNSp
        IF '((Text[OpenBracket)&(Text[CloseBracket)) GOTO QNSp


        ;"First we need to get the text after LAST instance of OpenBracket
        ;"i.e. 'MVar.Num}}]}}' chased m from 'a big black {{Data.Section[{{MVar.Num}}]}} chased me'
        NEW i SET i=2
        NEW part SET part=""
        NEW temp SET temp=""
NSL1        SET temp=$PIECE(Text,OpenBracket,i)
        IF temp'="" DO  GOTO NSL1
        . SET part=temp
        . SET SBefore=$PIECE(Text,OpenBracket,1,i-1)
        . SET i=i+1

        ;"do DEBUGMSG^TMGDEBU4(.TMGDBINDENT,"First part is: ",SBefore)

        ;"Now we find the text before the FIRST instance of CloseBracket
        ;"i.e. 'MVar.Num' from 'MVar.Num}}]}} chased me'
        ;"do DEBUGMSG^TMGDEBU4(.TMGDBINDENT,"part=",part)
        SET S=$PIECE(part,CloseBracket,1)
        SET SAfter=$PIECE(part,CloseBracket,2,128)
        ;"If we got here, we are successful
        SET Result=1

QNSp
        QUIT Result


FormatArray(InArray,OutArray,Divider)
        ;"PUBLIC FUNCTION
        ;"Purpose: The XML parser does not recognize whitespace, or end-of-line
        ;"        characters.  Thus many lines get lumped together.  However, IF there
        ;"        is a significant amount of text, then the parser will put the text into
        ;"        several lines (when get attrib text called etc.)
        ;"        SO, this function is to take an array composed of input lines (each
        ;"        with multiple sublines clumped together), and format it such that each
        ;"        line is separated in the array.
        ;"        e.g. Take this input array"
        ;"        InArray(cText,1)="line one\nline two\nline three\n
        ;"        InArray(cText,2)="line four\nline five\nline six\n
        ;"        and convert to:
        ;"        OutArray(1)="line one"
        ;"        OutArray(2)="line two"
        ;"        OutArray(3)="line three"
        ;"        OutArray(4)="line four"
        ;"        OutArray(5)="line five"
        ;"        OutArray(6)="line six"
        ;"Input: InArray, best IF passed by reference (faster) -- see example above
        ;"                Note: expected to be in format: InArray(cText,n)
        ;"        OutArray, must be passed by reference-- see example above
        ;"        Divider: the character(s) that divides lines ("\n" in this example)
        ;"Note: It is expected that InArray will be index by integers (i.e. 1, 2, 3)
        ;"        And this should be the case, as that is how XML functions pass back.
        ;"        Limit of 256 separate lines on any one InArray line
        ;"Output: OutArray is set, any prior data is killed
        ;"RESULT: 1=OK to continue, 0=abort

        SET DEBUG=$GET(DEBUG,0)
        SET cOKToCont=$GET(cOKToCont,1)
        SET cAbort=$GET(cAbort,0)

        NEW RESULT SET RESULT=cOKToCont
        NEW InIndex
        NEW OutIndex SET OutIndex=1
        NEW TempArray
        NEW Done

        KILL OutArray ;"remove any prior data

        IF DEBUG>0 DO DEBUGMSG^TMGDEBU4(.TMGDBINDENT,"Input array:")
        IF DEBUG DO ZWRITE^TMGZWR("InArray")

        IF $DATA(Divider)=0 DO  GOTO FADone
        . SET RESULT=cAbort

        SET Done=0
        FOR InIndex=1:1 DO  QUIT:Done
        . IF $DATA(InArray(cText,InIndex))=0 SET Done=1 QUIT
        . IF DEBUG>0 DO DEBUGMSG^TMGDEBU4(.TMGDBINDENT,"Converting line: ",InArray(cText,InIndex))
        . DO CleaveToArray^TMGSTUTL(InArray(cText,InIndex),Divider,.TempArray,OutIndex)
        . IF DEBUG>0 DO DEBUGMSG^TMGDEBU4(.TMGDBINDENT,"Resulting temp array:")
        . IF DEBUG DO ZWRITE^TMGZWR("TempArray")
        . SET OutIndex=TempArray(cMaxNode)+1
        . KILL TempArray(cMaxNode)
        . MERGE OutArray=TempArray
        . IF DEBUG>0 DO DEBUGMSG^TMGDEBU4(.TMGDBINDENT,"OutArray so far:")
        . IF DEBUG DO ZWRITE^TMGZWR("OutArray")

FADone
        QUIT RESULT



TrimL(S,TrimCh)
        ;"Purpose: To a trip a string of leading white space
        ;"        i.e. convert "  hello" into "hello"
        ;"Input: S -- the string to convert.  Won't be changed IF passed by reference
        ;"      TrimCh -- OPTIONAL: Charachter to trim.  Default is " "
        ;"Results: returns modified string
        ;"Note: processing limitation is string length=1024

        SET TrimCh=$GET(TrimCh," ")

        NEW RESULT SET RESULT=$GET(S)
        NEW Ch SET Ch=""
        FOR  DO  QUIT:(Ch'=TrimCh)
        . SET Ch=$EXTRACT(RESULT,1,1)
        . IF Ch=TrimCh SET RESULT=$EXTRACT(RESULT,2,1024)

        QUIT RESULT


TrimR(S,TrimCh)
        ;"Purpose: To a trip a string of trailing white space
        ;"        i.e. convert "hello   " into "hello"
        ;"Input: S -- the string to convert.  Won't be changed IF passed by reference
        ;"      TrimCh -- OPTIONAL: Charachter to trim.  Default is " "
        ;"Results: returns modified string
        ;"Note: processing limitation is string length=1024

        SET DEBUG=$GET(DEBUG,0)
        SET cOKToCont=$GET(cOKToCont,1)
        SET cAbort=$GET(cAbort,0)
        SET TrimCh=$GET(TrimCh," ")

        NEW RESULT SET RESULT=$GET(S)
        NEW Ch SET Ch=""
        NEW L

        FOR  DO  QUIT:(Ch'=TrimCh)
        . SET L=$LENGTH(RESULT)
        . SET Ch=$EXTRACT(RESULT,L,L)
        . IF Ch=TrimCh DO
        . . SET RESULT=$EXTRACT(RESULT,1,L-1)

        QUIT RESULT

Trim(S,TrimCh)
        ;"Purpose: To a trip a string of leading and trailing white space
        ;"        i.e. convert "    hello   " into "hello"
        ;"Input: S -- the string to convert.  Won't be changed IF passed by reference
        ;"      TrimCh -- OPTIONAL: Charachter to trim.  Default is " "
        ;"Results: returns modified string
        ;"Note: processing limitation is string length=1024

        ;"NOTE: this function could be replaced with $$TRIM^XLFSTR

        SET DEBUG=$GET(DEBUG,0)
        SET cOKToCont=$GET(cOKToCont,1)
        SET cAbort=$GET(cAbort,0)
        SET TrimCh=$GET(TrimCh," ")

        NEW RESULT SET RESULT=$GET(S)
        SET RESULT=$$TrimL(.RESULT,TrimCh)
        SET RESULT=$$TrimR(.RESULT,TrimCh)
        QUIT RESULT

TrimRType(S,type)
        ;"Scope: PUBLIC FUNCTION
        ;"Purpose: trim characters on the right of the string of a specified type.
        ;"         Goal, to be able to distinguish between numbers and strings.
        ;"         i.e. "1234<=" --> "1234" by trimming strings
        ;"Input: S -- The string to work on
        ;"       type -- the type of characters to TRIM: N for numbers,C for non-numbers (characters)
        ;"Results : modified string

        SET tempS=$GET(S)
        SET type=$$UP^XLFSTR($GET(type)) GOTO:(type="") TRTDone
        NEW DONE SET DONE=0
        FOR  QUIT:(tempS="")!DONE  DO
        . NEW c SET c=$EXTRACT(tempS,$LENGTH(tempS))
        . NEW cType SET cType="C"
        . IF +c=c SET cType="N"
        . IF type["N" DO
        . . IF cType="N" SET tempS=$EXTRACT(tempS,1,$LENGTH(tempS)-1) QUIT
        . . SET DONE=1
        . ELSE  IF type["C" DO
        . . IF cType="C"  SET tempS=$EXTRACT(tempS,1,$LENGTH(tempS)-1) QUIT
        . . SET DONE=1
        . ELSE  SET DONE=1

TRTDone QUIT tempS

CleaveToArray(Text,Divider,Array,InitIndex)
        DO SPLIT2AR^TMGSTUT2(.Text,.Divider,.Array,.InitIndex)
        QUIT

CleaveStr(Text,Divider,PartB)
        DO CLEAVSTR^TMGSTUT2(.Text,.Divider,.PartB)
        QUIT

SplitStr(Text,Width,PartB)
        DO SPLITSTR^TMGSTUT2(.Text,.Width,.PartB)
        QUIT

ADDWRAP(PriorS,AddS,MaxWidth,IndentS)
        ;"Purpose: to add AddS to PriorS, but wrap first wrap IF needed)
        ;"Input: PriorS : this is the total allowed width.
        ;"       AddS : this is the NEW addition string fragment to add
        ;"       MaxWidth : this is the length to wrap in.  OPTIONAL.  Default=60
        ;"       IndentS : this is the character string added to second line
        ;"                 to effect an indentations.  OPTIONAL
        ;"NOTE: To effect the wrapping of the line, the result string has
        ;"        CR_LF added between lines.
        ;"      Spaces MUST be included in AddS to allow wrapping of line, otherwise
        ;"        there will be no place to break line, and wrapping will not occur.
        ;"       It is assumed that PriorS is NOT longer than MaxWidth
        ;"Result: Returns one long string, divided by CR_LF's
        SET PriorS=$GET(PriorS)
        SET AddS=$GET(AddS)
        SET MaxWidth=+$GET(MaxWidth)
        IF MaxWidth=0 SET MaxWidth=60
        SET IndentS=$GET(IndentS)
        NEW lastLine SET lastLine=PriorS
        NEW numLines SET numLines=$LENGTH(PriorS,$CHAR(13,10))
        SET lastLine=$PIECE(PriorS,$CHAR(13,10),numLines)
        IF $LENGTH(lastLine)<$LENGTH(PriorS) DO
        . SET Result=$PIECE(PriorS,$CHAR(13,10),1,numLines-1)
        ELSE  SET Result=""

        IF $EXTRACT(lastLine,$LENGTH(lastLine))'=" " SET lastLine=lastLine_" "
        SET lastLine=lastLine_AddS
        NEW PartB
        FOR  QUIT:($LENGTH(lastLine)'>MaxWidth)  DO
        . DO SplitStr(.lastLine,MaxWidth,.PartB)
        . IF Result'="" SET Result=Result_$CHAR(13,10)
        . SET Result=Result_lastLine
        . SET lastLine=IndentS_PartB
        IF Result'="" SET Result=Result_$CHAR(13,10)
        SET Result=Result_lastLine

        QUIT Result

WordWrapArray(Array,Width,SpecialIndent)
        ;"Scope: PUBLIC FUNCTION
        ;"Purpose: To take an array and perform word wrapping such that
        ;"        no line is longer than Width.
        ;"        This function is really designed for reformatting a Fileman WP field
        ;"Input: Array MUST BE PASSED BY REFERENCE.  This contains the array
        ;"        to be reformatted.  Changes will be made to this array.
        ;"        It is expected that Array will be in this format:
        ;"                Array(1)="Some text on the first line."
        ;"                Array(2)="Some text on the second line."
        ;"                Array(3)="Some text on the third line."
        ;"                Array(4)="Some text on the fourth line."
        ;"        or
        ;"                Array(1,0)="Some text on the first line."
        ;"                Array(2,0)="Some text on the second line."
        ;"                Array(3,0)="Some text on the third line."
        ;"                Array(4,0)="Some text on the fourth line."
        ;"        Width -- the limit on the length of any line.  Default value=70
        ;"        SpecialIndent : IF 1, then wrapping is done like this:
        ;"                "   This is a very long line......"
        ;"           will be wrapped like this:
        ;"                "   This is a very
        ;"                "   long line ...
        ;"          Notice that the leading space is copied subsequent line.
        ;"          Also, a line like this:
        ;"                "   1. Here is the beginning of a paragraph that is very long..."
        ;"            will be wrapped like this:
        ;"                "   1. Here is the beginning of a paragraph
        ;"                "      that is very long..."
        ;"          Notice that a pattern '#. ' causes the wrapping to match the start of
        ;"                of the text on the line above.
        ;"          The exact rules for matching this are as follows:
        ;"                (FirstWord?.N1".")!(FirstWord?1.3E1".")
        ;"                i.e. any number of digits, followed by "."
        ;"                OR 1-4 all upper-case characters followed by a "."
        ;"                        This will allow "VIII. " pattern but not "viii. "
        ;"                        HOWEVER, might get confused with a word, like "NOTE. "
        ;"
        ;"          This, below, is not dependant on SpecialIndent setting
        ;"          Also, because some of the lines have already partly wrapped, like this:
        ;"                "   1. Here is the beginning of a paragraph that is very long..."
        ;"                "and this is a line that has already wrapped.
        ;"                So when the first line is wrapped, it would look like this:
        ;"                "   1. Here is the beginning of a paragraph
        ;"                "      that is very long..."
        ;"                "and this is a line that has already wrapped.
        ;"                But is should look like this:
        ;"                "   1. Here is the beginning of a paragraph
        ;"                "      that is very long...and this is a line
        ;"                "      that has already wrapped.
        ;"                But the next line SHOULD NOT be pulled up IF it is the start
        ;"                of a NEW paragraph.  I will tell by looking for #. paattern.


        ;"Result -- none

        NEW tempArray SET tempArray=""  ;"holds result during work.
        NEW tindex SET tindex=0
        NEW index
        SET index=$ORDER(Array(""))
        NEW s
        NEW residualS SET residualS=""
        NEW AddZero SET AddZero=0
        SET Width=$GET(Width,70)

         IF $GET(TMGDEBUG)>0 DO DEBUGMSG^TMGDEBU4(.TMGDBINDENT,"Starting loop")

        IF index'="" FOR  DO  QUIT:((index="")&(residualS=""))
        . SET s=$GET(Array(index))
        . IF s="" DO
        . . SET s=$GET(Array(index,0))
        . . SET AddZero=1
        . IF residualS'="" DO  ;"See IF should join to next line. Don't IF '#. ' pattern
        . . NEW FirstWord SET FirstWord=$PIECE($$Trim(s)," ",1)
        . . IF $GET(TMGDEBUG)>0 DO DEBUGMSG^TMGDEBU4(.TMGDBINDENT,"First Word: ",FirstWord)
        . . IF (FirstWord?.N1".")!(FirstWord?1.4U1".") DO     ;"match for '#.' pattern
        . . . ;"Here we have the next line is a NEW paragraph, so don't link to residualS
        . . . SET tindex=tindex+1
        . . . IF AddZero=0 SET tempArray(tindex)=residualS
        . . . ELSE  SET tempArray(tindex,0)=residualS
        . . . SET residualS=""
        . IF $LENGTH(residualS)+$LENGTH(s)'<256 DO
        . . IF $GET(TMGDEBUG)>0 DO DEBUGMSG^TMGDEBU4(.TMGDBINDENT,"ERROR -- string too long.")
        . SET s=residualS_s
        . SET residualS=""
        . IF $LENGTH(s)>Width DO
        . . IF $GET(TMGDEBUG)>0 DO DEBUGMSG^TMGDEBU4(.TMGDBINDENT,"Long line: ",s)
        . . NEW LineArray
        . . NEW NumLines
        . . SET NumLines=$$SplitLine(.s,.LineArray,Width,.SpecialIndent)
        . . IF $GET(TMGDEBUG)>0 DO ZWRITE^TMGZWR("LineArray")
        . . SET s=""
        . . NEW LineIndex
        . . FOR LineIndex=1:1:NumLines DO
        . . . SET tindex=tindex+1
        . . . IF AddZero=0 SET tempArray(tindex)=LineArray(LineIndex)
        . . . ELSE  SET tempArray(tindex,0)=LineArray(LineIndex)
        . . ;"long wrap probably continues into next paragraph, so link together.
        . . IF NumLines>2 DO
        . . . IF AddZero=0 SET residualS=tempArray(tindex) SET tempArray(tindex)=""
        . . . ELSE  SET residualS=tempArray(tindex,0) SET tempArray(tindex,0)=""
        . . . SET tindex=tindex-1
        . ELSE  DO
        . . SET tindex=tindex+1
        . . IF AddZero=0 SET tempArray(tindex)=s
        . . ELSE  SET tempArray(tindex,0)=s
        . SET index=$ORDER(Array(index))
        ELSE  DO
        . IF $GET(TMGDEBUG)>0 DO DEBUGMSG^TMGDEBU4(.TMGDBINDENT,"Array appears empty")


        KILL Array
        MERGE Array=tempArray

        QUIT


SplitLine(s,LineArray,Width,SpecialIndent,Indent,DivS)
        QUIT $$SPLITLN^TMGSTUT2(.s,.LineArray,.Width,.SpecialIndent,.Indent,.DivS)

WriteWP(NodeRef)
        ;"Purpose: Given a reference to a WP field, this function will print it out.
        ;"INput: NodeRef -- the name of the node to print out.
        ;"        For example, "^PS(50.605,1,1)"
        ;"Modification: 2/10/06 -- I removed need for @NodeRef@(0) to contain data.

        NEW i
        ;"if $GET(@NodeRef@(0))="" GOTO WWPDone
        SET i=$ORDER(@NodeRef@(0))
        IF i'="" FOR  DO  QUIT:(i="")
        . NEW OneLine
        . SET OneLine=$GET(@NodeRef@(i))
        . IF OneLine="" SET OneLine=$GET(@NodeRef@(i,0))
        . WRITE OneLine,!
        . SET i=$ORDER(@NodeRef@(i))

WWPDone QUIT

WPINSERT(REF,LNUM,S) ;
        ;"Purpose: to insert one line into a WP record at given line number
        ;"Input: REF -- the is the reference to the node holding the WP record.  OPEN FORMAT.
        ;"              e.g. '^HL(772,177,"IN",'
        ;"       LNUM -- The line number to insert BEFORE.
        ;"       S -- The string to insert at LNUM.
        ;"Results : none.
        SET REF=$GET(REF) IF REF="" GOTO WPIDN
        NEW HDR SET HDR=REF_"0)"
        NEW DT SET DT=$PIECE($GET(@HDR),"^",5)
        IF DT'?7N DO  GOTO WPIDN
        . ;"Could put an error message here.  Don't seem to be pointing to a WP file.
        NEW REF2 SET REF2=REF_(LNUM-0.1)_",0)"
        SET @REF2=$GET(S)
        DO WPFIX(REF)
WPIDN   QUIT

WPDEL(REF,LNUM) ;
        ;"Purpose: to delete one line in a WP record at given line number
        ;"Input: REF -- the is the reference to the node holding the WP record.  OPEN FORMAT.
        ;"              e.g. '^HL(772,177,"IN",'
        ;"       LNUM -- The line number to delete.
        ;"Results : none.
        SET REF=$GET(REF) IF REF="" GOTO WPIDN
        NEW HDR SET HDR=REF_"0)"
        NEW DT SET DT=$PIECE($GET(@HDR),"^",5)
        IF DT'?7N DO  GOTO WPDDN
        . ;"Could put an error message here.  Don't seem to be pointing to a WP file.
        NEW CREF SET CREF=$$CREF^DILF(REF)
        KILL @CREF@(LNUM)
        DO WPFIX(REF)
WPDDN   QUIT

WPFIX(REF) ;
        ;"Purpose: to fix the line numbers in a WP field to that they are all integers.
        ;"Input: REF -- the is the reference to the node holding the WP record.  OPEN FORMAT.
        ;"              e.g. '^HL(772,177,"IN",'
        SET REF=$GET(REF) IF REF="" GOTO WPFDN
        NEW HDR SET HDR=REF_"0)"
        NEW DT SET DT=$PIECE($GET(@HDR),"^",5)
        IF DT'?7N DO  GOTO WPFDN
        . ;"Could put an error message here.  Don't seem to be pointing to a WP file.
        NEW CREF SET CREF=$$CREF^DILF(REF)
        NEW TEMPA MERGE TEMPA=@CREF
        KILL @CREF
        NEW LINE SET LINE=0
        NEW I SET I=0
        FOR  SET I=$ORDER(TEMPA(I)) QUIT:+I'>0  DO
        . SET LINE=LINE+1
        . SET @CREF@(LINE,0)=$GET(TEMPA(I,0))
        SET @CREF@(0)=TEMPA(0)
        SET $P(@CREF@(0),"^",3)=LINE
        SET $P(@CREF@(0),"^",4)=LINE
WPFDN   QUIT

LPad(S,width)
        ;"Purpose: To add space ("pad") string S such that final width is per specified with.
        ;"                space is added to left side of string
        ;"Input: S : the string to pad.
        ;"        width : the desired final width
        ;"result: returns resulting string
        ;"Example: LPad("$5.23",7)="  $5.23"

        QUIT $$RJ^XLFSTR(.S,.width," ")

RPad(S,width)
        ;"Purpose: To add space ("pad") string S such that final width is per specified with.
        ;"                space is added to right side of string
        ;"Input: S : the string to pad.
        ;"        width : the desired final width
        ;"result: returns resulting string
        ;"Example: RPad("$5.23",7)="$5.23  "

        QUIT $$LJ^XLFSTR(.S,.width," ")

Center(S,width)
        ;"Purpose: to return a center justified string

        QUIT $$CJ^XLFSTR(.S,.width," ")

Clip(S,width)
        ;"Purpose: to ensure that string S is no longer than width

        NEW RESULT SET RESULT=$GET(S)
        IF RESULT'="" SET RESULT=$EXTRACT(S,1,width)
ClipDone
        QUIT RESULT


STRB2H(s,F,noSpace)
        ;"Convert a string to hex characters)
        ;"Input: s -- the input string (need not be ascii characters)
        ;"        F -- (optional) IF F>0 then will append an ascii display of string.
        ;"      noSpace -- (Optional) IF >0 then characters NOT separated by spaces
        ;"RESULT -- the converted string

        NEW i,ch
        NEW RESULT SET RESULT=""

        FOR i=1:1:$LENGTH(s) DO
        . SET ch=$EXTRACT(s,i)
        . SET RESULT=RESULT_$$HEXCHR^TMGMISC($ASCII(ch))
        . IF +$GET(noSpace)=0 SET RESULT=RESULT_" "

        IF $GET(F)>0 SET RESULT=RESULT_"   "_$$HIDECTRLS^TMGSTUTL(s)
        QUIT RESULT


HIDECTRLS(s)
        ;"hide all unprintable characters from a string
        NEW i,ch,byte
        NEW RESULT SET RESULT=""
        FOR i=1:1:$LENGTH(s) DO
        . SET ch=$e(s,i)
        . SET byte=$ASCII(ch)
        . IF (byte<32)!(byte>122) SET RESULT=RESULT_"."
        . ELSE  SET RESULT=RESULT_ch

        QUIT RESULT



CapWords(S,Divider)
        QUIT $$CAPWORDS^TMGSTUT2(.S,.Divider)

LinuxStr(S)
        ;"Purpose: convert string to a valid linux filename
        ;"      e.g. 'File Name' --> 'File\ Name'
        QUIT $$REPLSTR^TMGSTUT3(.S," ","\ ")

NiceSplit(S,Len,s1,s2,s2Min,DivCh)
        DO NICESPLT^TMGSTUT3(.S,.Len,.s1,.s2,.s2Min,.DivCh)
        QUIT

StrToWP(s,pArray,width,DivCh,InitLine)
        DO STR2WP^TMGSTUT2(.s,.pArray,.width,.DivCh,.InitLine)
        QUIT
        ;
WPToStr(pArray,DivCh,MaxLen,InitLine)
        QUIT $$WP2STR^TMGSTUT2(.pArray,.DivCh,.MaxLen,.InitLine)
        ;
Comp2Strs(s1,s2)
        ;"Purpose: To compare two strings and assign an arbritrary score to their similarity
        ;"Input: s1,s2 -- The two strings to compare
        ;"Result: a score comparing the two strings
        ;"      0.5 point for every word in s1 that is also in s2 (case specific)
        ;"      0.25 point for every word in s1 that is also in s2 (not case specific)
        ;"      0.5 point for every word in s2 that is also in s1 (case specific)
        ;"      0.25 point for every word in s2 that is also in s1 (not case specific)
        ;"      1 points IF same number of words in string (compared each way)
        ;"      2 points for each word that is in the same position in each string (case specific)
        ;"      1.5 points for each word that is in the same position in each string (not case specific)

        NEW score SET score=0
        NEW Us1 SET Us1=$$UP^XLFSTR(s1)
        NEW Us2 SET Us2=$$UP^XLFSTR(s2)

        NEW i
        FOR i=1:1:$LENGTH(s1," ") DO
        . IF s2[$PIECE(s1," ",i) SET score=score+0.5
        . ELSE  IF Us2[$PIECE(Us1," ",i) SET score=score+0.25
        . IF $PIECE(s1," ",i)=$PIECE(s2," ",i) SET score=score+1
        . ELSE  IF $PIECE(Us1," ",i)=$PIECE(Us2," ",i) SET score=score+1.5

        FOR i=1:1:$LENGTH(s2," ") DO
        . IF s1[$PIECE(s2," ",i) SET score=score+0.5
        . ELSE  IF Us1[$PIECE(Us2," ",i) SET score=score+0.25
        . IF $PIECE(s1," ",i)=$PIECE(s2," ",i) SET score=score+1
        . ELSE  IF $PIECE(Us1," ",i)=$PIECE(Us2," ",i) SET score=score+1.5

        IF $LENGTH(s1," ")=$LENGTH(s2," ") SET score=score+2

        QUIT score


PosNum(s,Num,LeadingSpace)
        ;"Purpose: To return the position of the first Number in a string
        ;"Input: S -- string to check
        ;"       Num -- OPTIONAL, default is 0-9 numbers.  number to look for.
        ;"       LeadingSpace -- OPTIONAL.  If 1 then looks for " #" or " .#", not just "#"
        ;"Results: -1 IF not found, otherwise position of found digit.

        NEW RESULT SET RESULT=-1
        NEW Leader SET Leader=""
        IF $GET(LeadingSpace)=1 SET Leader=" "

        IF $GET(Num) DO  GOTO PNDone
        . SET RESULT=$find(s,Leader_Num)-1

        NEW temp,i,decimalFound
        FOR i=0:1:9 DO
        . SET decimalFound=0
        . SET temp=$find(s,Leader_i)
        . IF (temp=0)&(Leader'="") DO
        . . SET temp=$find(s,Leader_"."_i)
        . . IF temp>-1 SET decimalFound=1
        . IF temp>-1 SET temp=temp-$LENGTH(Leader_i)
        . IF decimalFound SET temp=temp-1
        . IF (temp>0)&((temp<RESULT)!(RESULT=-1)) SET RESULT=temp

PNDone
        IF (RESULT>0)&(Leader=" ") SET RESULT=RESULT+1
        QUIT RESULT


IsNumeric(s)
        ;"Purpose: To deterimine IF word s is a numeric
        ;"      Examples of numeric words:
        ;"              10,  N-100,  0.5%,   50000UNT/ML
        ;"      the test will be if the word contains any digit 0-9
        ;"Results: 1 IF is a numeric word, 0 IF not.
        ;
        ;"NOTE: 8/11/13 -- SEE ALSO $$ISNUM^TMGSTUT3.  Doesn't DO same test, however
        QUIT ($$PosNum(.s)>0)


ScrubNumeric(s)
        ;"Purpose: This is a specialty function designed to remove numeric words
        ;"      from a sentence.  E.g.
        ;"        BELLADONNA ALK 0.3/PHENOBARB 16MG CHW TB --> BELLADONNA ALK /PHENOBARB CHW TB
        ;"        ESTROGENS,CONJUGATED 2MG/ML INJ (IN OIL) --> ESTROGENS,CONJUGATED INJ (IN OIL)

        NEW Array,i,RESULT
        SET s=$$REPLSTR^TMGSTUT3(s,"/MG","")
        SET s=$$REPLSTR^TMGSTUT3(s,"/ML","")
        SET s=$$REPLSTR^TMGSTUT3(s,"/"," / ")
        SET s=$$REPLSTR^TMGSTUT3(s,"-"," - ")
        DO CleaveToArray(s," ",.Array)
        NEW ToKill
        SET i=0 FOR  SET i=$ORDER(Array(i)) QUIT:+i'>0  DO
        . IF (Array(i)="MG")&($GET(ToKill(i-1))=1) SET ToKill(i)=1 QUIT
        . IF (Array(i)="MCG")&($GET(ToKill(i-1))=1) SET ToKill(i)=1 QUIT
        . IF (Array(i)="MEQ")&($GET(ToKill(i-1))=1) SET ToKill(i)=1 QUIT
        . IF (Array(i)="%")&($GET(ToKill(i-1))=1) SET ToKill(i)=1 QUIT
        . IF (Array(i)="MM")&($GET(ToKill(i-1))=1) SET ToKill(i)=1 QUIT
        . IF $$IsNumeric(Array(i))=0 QUIT
        . SET ToKill(i)=1
        . NEW tempS SET tempS=$GET(Array(i-1))
        . IF (tempS="/")!(tempS="-") SET ToKill(i-1)=1
        . IF (tempS="NO")!(tempS="#") SET ToKill(i-1)=1

        SET i=0 FOR  SET i=$ORDER(Array(i)) QUIT:+i'>0  DO
        . IF $GET(ToKill(i))=1 KILL Array(i)

        SET i="",RESULT=""
        FOR  SET i=$ORDER(Array(i)) QUIT:+i'>0  DO
        . SET RESULT=RESULT_Array(i)_" "

        SET RESULT=$$Trim(RESULT)
        SET RESULT=$$REPLSTR^TMGSTUT3(RESULT," / ","/")
        SET RESULT=$$REPLSTR^TMGSTUT3(RESULT," - ","-")

        QUIT RESULT


Pos(subStr,s,count) ; "DEPRECIATED use $$POS^TMGSTUT3 
        QUIT $$POS^TMGSTUT3(.subStr,.s,.count)

ArrayPos(array,s)
        ;"Purpose: return the index position of s in array

        ;"...

        QUIT

DiffPos(s1,s2)
        ;"Purpose: Return the position of the first difference between s1 and s2
        ;"Input -- s1, s2 :  The strings to compare.
        ;"RESULT:  the position (in s1) of the first difference, or 0 IF no difference

        NEW l SET l=$LENGTH(s1)
        IF $LENGTH(s2)>l SET l=$LENGTH(s2)
        NEW DONE SET DONE=0
        NEW i FOR i=1:1:l DO  QUIT:(DONE=1)
        . SET DONE=($EXTRACT(s1,1,i)'=$EXTRACT(s2,1,i))
        NEW RESULT SET RESULT=0
        IF DONE=1 SET RESULT=i
        QUIT RESULT


DiffWPos(Words1,Words2)
        ;"Purpose: Return the index of the first different word between Words arrays
        ;"Input:  Words1,Words2 -- the array of words, such as would be made
        ;"              by CleaveToArray^TMGSTUTL
        ;"Returns: Index of first different word in Words1, or 0 IF no difference

        NEW l SET l=+$GET(Words1("MAXNODE"))
        IF +$GET(Words2("MAXNODE"))>l SET l=+$GET(Words2("MAXNODE"))
        NEW DONE SET DONE=0
        NEW i FOR i=1:1:l DO  QUIT:(DONE=1)
        . SET DONE=($GET(Words1(i))'=$GET(Words2(i)))
        NEW RESULT
        IF DONE=1 SET RESULT=i
        ELSE  SET RESULT=0
        QUIT RESULT


SimStr(s1,p1,s2,p2)
        ;"Purpose: return the matching string in both s1 and s2, starting
        ;"         at positions p1 and p2.
        ;"         Example: s1='Tom is 12 years old', p1=7
        ;"                  s2='Bill will be 12 years young tomorrow' p2=13
        ;"                 would return ' 12 years '

        NEW ch1,ch2,offset,RESULT,DONE
        SET RESULT="",DONE=0
        FOR offset=0:1:9999 DO  QUIT:(DONE=1)
        . SET ch1=$EXTRACT(s1,p1+offset)
        . SET ch2=$EXTRACT(s2,p2+offset)
        . IF (ch1=ch2) SET RESULT=RESULT_ch1
        . ELSE  SET DONE=1
        QUIT RESULT


SimWord(Words1,p1,Words2,p2)
        ;"Purpose: return the matching words in both words array 1 and 2, starting
        ;"         at word positions p1 and p2.  This function is different from
        ;"         SimStr in that it works with whole words
        ;"         Example:
        ;"              Words1(1)=Tom               Words2(1)=Bill
        ;"              Words1(2)=is                Words2(2)=will
        ;"              Words1(3)=12                Words2(3)=be
        ;"              Words1(4)=years             Words2(4)=12
        ;"              Words1(5)=old               Words2(5)=years
        ;"              Words1("MAXNODE")=5         Words2(6)=young
        ;"                                          Words2(7)=tomorrow
        ;"                                          Words1("MAXNODE")=7
        ;"              This will return 3, (where '12 years' starts)
        ;"              IF p1=3 and p2=4 would return '12 years'
        ;"Note: A '|' will be used as word separator when constructing result
        ;"Input:  Words1,Words2 -- the array of words, such as would be made
        ;"              by CleaveToArray^TMGSTUTL.  e.g.
        ;"        p1,p2 -- the index of the word in Words array to start with
        ;"RESULT: (see example)

        NEW w1,w2,offset,RESULT,DONE
        SET RESULT="",DONE=0
        FOR offset=0:1:$GET(Words1("MAXNODE")) DO  QUIT:(DONE=1)
        . SET w1=$GET(Words1(offset+p1))
        . SET w2=$GET(Words2(offset+p2))
        . IF (w1=w2)&(w1'="") DO
        . . IF (RESULT'="") SET RESULT=RESULT_"|"
        . . SET RESULT=RESULT_w1
        . ELSE  SET DONE=1
        QUIT RESULT


SimPos(s1,s2,DivStr,pos1,pos2,MatchStr)
        ;"Purpose: return the first position that two strings are similar.  This means
        ;"         the first position in string s1 that characters match in s2.  A
        ;"         match will be SET to mean 3 or more characters being the same.
        ;"         Example: s1='Tom is 12 years old'
        ;"                  s2='Bill will be 12 years young tomorrow'
        ;"                  This will return 7, (where '12 years' starts)
        ;"Input: s1,s2 -- the two strings to compare
        ;"       DivStr -- OPTIONAL, the character to use to separate the answers
        ;"                        in the return string.  Default is '^'
        ;"       pos1 -- OPTIONAL, an OUT PARAMETER.  Returns Pos1 from result
        ;"       pos2 -- OPTIONAL, an OUT PARAMETER.  Returns Pos2 from result
        ;"       MatchStr -- OPTIONAL, an OUT PARAMETER.  Returns MatchStr from result
        ;"Results: Pos1^Pos2^MatchStr  Pos1=position in s1, Pos2=position in s2,
        ;"                             MatchStr=the matching Str

        SET DivStr=$GET(DivStr,"^")
        NEW startPos,subStr,found,s2Pos
        SET found=0,s2Pos=0
        FOR startPos=1:1:$LENGTH(s1) DO  QUIT:(found=1)
        . SET subStr=$EXTRACT(s1,startPos,startPos+3)
        . SET s2Pos=$$POS^TMGSTUT3(subStr,s2)
        . SET found=(s2Pos>0)

        NEW RESULT
        IF found=1 DO
        . SET pos1=startPos,pos2=s2Pos
        . SET MatchStr=$$SimStr(s1,startPos,s2,s2Pos)
        ELSE  DO
        . SET pos1=0,pos2=0,MatchStr=""

        SET RESULT=pos1_DivStr_pos2_DivStr_MatchStr

        QUIT RESULT


SimWPos(Words1,Words2,DivStr,p1,p2,MatchStr)
        ;"Purpose: return the first position that two word arrays are similar.  This means
        ;"         the first index in Words array 1 that matches to words in Words array 2.
        ;"         A match will be SET to mean the two words are equal
        ;"         Example:
        ;"              Words1(1)=Tom               Words2(1)=Bill
        ;"              Words1(2)=is                Words2(2)=will
        ;"              Words1(3)=12                Words2(3)=be
        ;"              Words1(4)=years             Words2(4)=12
        ;"              Words1(5)=old               Words2(5)=years
        ;"              Words1("MAXNODE")=5         Words2(6)=young
        ;"                                          Words2(7)=tomorrow
        ;"                                          Words2("MAXNODE")=7
        ;"              This will return 3, (where '12 years' starts)
        ;"Input: Words1,Words2 -- the two arrays to compare
        ;"       DivStr -- OPTIONAL, the character to use to separate the answers
        ;"                        in the return string.  Default is '^'
        ;"       pos1 -- OPTIONAL, an OUT PARAMETER.  Returns Pos1 from result
        ;"       pos2 -- OPTIONAL, an OUT PARAMETER.  Returns Pos2 from result
        ;"       MatchStr -- OPTIONAL, an OUT PARAMETER.  Returns MatchStr from result
        ;"Results: Pos1^Pos2^MatchStr  Pos1=position in Words1, Pos2=position in Words2,
        ;"                             MatchStr=the first matching Word or phrase
        ;"                                 Note: | will be used as a word separator for phrases.

        SET DivStr=$GET(DivStr,"^")
        NEW startPos,word1,found,w2Pos
        SET found=0,s2Pos=0
        FOR startPos=1:1:+$GET(Words1("MAXNODE")) DO  QUIT:(found=1)
        . SET word1=$GET(Words1(startPos))
        . SET w2Pos=$$IndexOf^TMGMISC($name(Words2),word1)
        . SET found=(w2Pos>0)

        IF found=1 DO
        . SET p1=startPos,p2=w2Pos
        . SET MatchStr=$$SimWord(.Words1,p1,.Words2,p2)
        ELSE  DO
        . SET p1=0,p2=0,MatchStr=""

        NEW RESULT SET RESULT=p1_DivStr_p2_DivStr_MatchStr

        QUIT RESULT


DiffStr(s1,s2,DivChr)
        ;"Purpose: Return how s1 differs from s2.  E.g.
        ;"          s1='Today was the birthday of Bill and John'
        ;"          s2='Yesterday was the birthday of Tom and Sue'
        ;"          results='Today^1^Bill^26^John^35'
        ;"          This means that 'Today', starting at pos 1 in s1 differs
        ;"            from s2.  And 'Bill' starting at pos 26 differs from s2 etc..
        ;"Input: s1,s2 -- the two strings to compare
        ;"       DivStr -- OPTIONAL, the character to use to separate the answers
        ;"                        in the return string.  Default is '^'
        ;"Results: DiffStr1^pos1^DiffStr2^pos2^...

        SET DivChr=$GET(DivChr,"^")
        NEW RESULT SET RESULT=""
        NEW offSET SET offset=0
        NEW p1,p2,matchStr,matchLen
        NEW diffStr,temp
DSLoop
        SET temp=$$SimPos(s1,s2,DivChr,.p1,.p2,.matchStr)
        ;"Returns: Pos1^Pos2^MatchStr  Pos1=pos in s1, Pos2=pos in s2, MatchStr=the matching Str
        IF p1=0 SET:(s1'="") RESULT=RESULT_s1_DivChr_(+offset) GOTO DSDone

        SET matchLen=$LENGTH(matchStr)

        IF p1>1 DO
        . SET diffStr=$EXTRACT(s1,1,p1-1)
        . SET RESULT=RESULT_diffStr_DivChr_(1+offset)_DivChr
        SET offset=offset+(p1+matchLen-1)
        SET s1=$EXTRACT(s1,p1+matchLen,9999)  ;"trim s1
        SET s2=$EXTRACT(s2,p2+matchLen,9999)  ;"trim s2
        GOTO DSLoop
DSDone
        QUIT RESULT


DiffWords(Words1,Words2,DivChr)
        ;"Purpose: Return how Word arrays Words1 differs from Words2.  E.g.
        ;"         Example:
        ;"              Words1(1)=Tom               Words2(1)=Bill
        ;"              Words1(2)=is                Words2(2)=will
        ;"              Words1(3)=12                Words2(3)=be
        ;"              Words1(4)=years             Words2(4)=12
        ;"              Words1(5)=old               Words2(5)=years
        ;"              Words1("MAXNODE")=5         Words2(6)=young
        ;"                                          Words2(7)=tomorrow
        ;"                                          Words1("MAXNODE")=7
        ;"
        ;"          s1='Today was the birthday of Bill and John'
        ;"          s2='Yesterday was the birthday of Tom and Sue'
        ;"          results='Tom is^1^old^5'
        ;"          This means that 'Tom is', starting at pos 1 in Words1 differs
        ;"            from Words2.  And 'old' starting at pos 5 differs from Words2 etc..
        ;"Input: Words1,Words2 -- PASS BY REFERENCE.  The two word arrays to compare
        ;"       DivStr -- OPTIONAL, the character to use to separate the answers
        ;"                        in the return string.  Default is '^'
        ;"Note: The words in DiffStr are divided by "|"
        ;"Results:  DiffStr1A>DiffStr1B^pos1>pos2^DiffStr2A>DiffStr2B^pos1>pos2^...
        ;"      The A DiffStr would be what the value is in Words1, and
        ;"      the B DiffStr would be what the value is in Words2, or @ IF deleted.

        SET DivChr=$GET(DivChr,"^")
        NEW RESULT SET RESULT=""
        NEW trimmed1,trimmed2 SET trimmed1=0,trimmed2=0
        NEW p1,p2,matchStr,matchLen
        NEW diffStr1,diffStr2,temp
        NEW tWords1,tWords2
        MERGE tWords1=Words1
        MERGE tWords2=Words2
        NEW i,len1,len2,trimLen1,trimLen2
        NEW diffPos1,diffPos2
        SET len1=+$GET(tWords1("MAXNODE"))
        SET len2=+$GET(tWords2("MAXNODE"))
DWLoop
        SET temp=$$SimWPos(.tWords1,.tWords2,DivChr,.p1,.p2,.matchStr)
        ;"Returns: Pos1^Pos2^MatchStr  Pos1=pos in s1, Pos2=pos in s2, MatchStr=the matching Str

        ;"Possible return options:
        ;"  p1=p2=0 -- two strings have nothing in common
        ;"  p1=p2=1 -- first word of each string is the same
        ;"  p1=p2=X -- words 1..(X-1) differ from each other.
        ;"  p1>p2 -- e.g. EXT REL TAB  -->  XR TAB
        ;"  p1<p2 -- XR TAB  -->  EXT REL TAB

        IF (p1=0)&(p2=0) DO
        . SET diffStr1=$$CatArray(.tWords1,1,len1,"|")
        . SET diffStr2=$$CatArray(.tWords2,1,len2,"|")
        . SET trimLen1=len1,trimLen2=len2
        . SET diffPos1=1+trimmed1
        . SET diffPos2=1+trimmed2
        ELSE  IF (p1=1)&(p2=1) DO
        . SET diffStr1="@",diffStr2="@"
        . SET trimLen1=1,trimLen2=1
        . SET diffPos1=0,diffPos2=0
        ELSE  DO
        . SET diffStr1=$$CatArray(.tWords1,1,p1-1,"|")
        . SET diffStr2=$$CatArray(.tWords2,1,p2-1,"|")
        . SET trimLen1=p1-1,trimLen2=p2-1
        . SET diffPos1=1+trimmed1,diffPos2=1+trimmed2

        IF diffStr1="" SET diffStr1="@"
        IF diffStr2="" SET diffStr2="@"

        IF '((diffStr1="@")&(diffStr1="@")) DO
        . SET:(RESULT'="")&($EXTRACT(RESULT,$LENGTH(RESULT))'=DivChr) RESULT=RESULT_DivChr
        . SET RESULT=RESULT_diffStr1_">"_diffStr2_DivChr
        . SET RESULT=RESULT_diffPos1_">"_diffPos2

        DO ListTrim^TMGMISC("tWords1",1,trimLen1,"MAXNODE")
        DO ListTrim^TMGMISC("tWords2",1,trimLen2,"MAXNODE")
        SET trimmed1=trimmed1+trimLen1
        SET trimmed2=trimmed2+trimLen2

        IF ($GET(tWords1("MAXNODE"))=0)&($GET(tWords2("MAXNODE"))=0) GOTO DWDone
        GOTO DWLoop

DWDone
        QUIT RESULT

CatArray(Words,i1,i2,DivChr)
        ;"Purpose: For given word array, return contatenated results from index1 to index2
        ;"Input: Words -- PASS BY REFERENCE.  Array of Words, as might be created by CleaveToArray
        ;"       i1 -- the index to start concat at
        ;"       i2 -- the last index to include in concat
        ;"       DivChr -- OPTIONAL.  The character to used to separate words.  Default=" "

        NEW RESULT SET RESULT=""
        SET DivChr=$GET(DivChr," ")
        NEW i FOR i=i1:1:i2 DO
        . NEW word SET word=$GET(Words(i))
        . IF word="" QUIT
        . SET:(RESULT'="")&($EXTRACT(RESULT,$LENGTH(RESULT))'=DivChr) RESULT=RESULT_DivChr
        . SET RESULT=RESULT_word
        QUIT RESULT

QTPROTECT(S) 
        QUIT $$QTPROTCT^TMGSTUT3(.S)
QtProtect(s)
        QUIT $$QTPROTCT^TMGSTUT3(.S)


GetStrPos(s,StartPos,P1,P2)  ;"INCOMPLETE!!
        ;"Purpose: return position of start and end of a string (marked by starting
        ;"      and ending quote.  Search is started at StartPos.
        ;"      Example: IF s='She said "Hello" to Bill', and StartPos=1
        ;"      then P1 should be returned as 10, and P2 as 16
        ;"Input: s -- the text to be
        ;"       StartPos -- the position to start the search at. Optional: default=1
        ;"       P1 -- PASS BY REFERENCE, an Out Parameter
        ;"       P2 -- PASS BY REFERENCE, an Out Parameter
        ;"Results: None
        ;"Output: P1 and P2 are returned as per example above, or 0 IF not quotes in text

        SET P1=0,P2=0
        IF s'["""" GOTO GSPDone
        SET StartPos=+$GET(StartPos,1)
        NEW tempS SET tempS=$EXTRACT(s,StartPos,$LENGTH(s))
        SET tempS=$$REPLSTR^TMGSTUT3(tempS,"""""",$CHAR(1)_$CHAR(1))

        ;"FINISH...   NOT COMPLETED...
GSPDone
        QUIT

InQt(s,Pos) ;"Depreciated.  Use $$INQT^TMGSTUT3
        QUIT $$INQT^TMGSTUT3(.s,.Pos)
        ;
HNQTSUB(s,SubStr)  ;"A ALL CAPS ENTRY POINT
        QUIT $$HasNonQtSub(.s,.SubStr)
HasNonQtSub(s,SubStr)
        ;"Purpose: Return IF string S contains SubStr, not inside quotes.
        NEW Result SET Result=0
        IF s'[SubStr GOTO HNQCDn
        NEW p SET p=1
        NEW DONE SET DONE=0
        NEW instance SET instance=0
        FOR  DO  QUIT:(DONE=1)
        . SET instance=instance+1
        . SET p=$$POS^TMGSTUT3(SubStr,s,instance)
        . IF p=0 SET DONE=1 QUIT
        . IF $$InQt(.s,p)=0 SET Result=1,DONE=1 QUIT
HNQCDn  QUIT Result

GetWord(s,Pos,OpenDiv,CloseDiv) ;"Depreciated.  Use $$GETWORD^TMGSTUT3
        QUIT $$GETWORD^TMGSTUT3(.s,.Pos,.OpenDiv,.CloseDiv)

MatchXtract(s,DivCh,Group,Map,Restrict)  ;"Depreciated.  Use $$MATCHXTR^TMGSTUT3
        ;"Purpose: Provide a SAAC compliant (all upper case) entry point) for MatchXtract
        QUIT $$MATCHXTR^TMGSTUT3(.s,.DivCh,.Group,.Map,.Restrict)

CmdChStrip(s)
        QUIT $$STRIPCMD^TMGSTUT3(.s)

StrBounds(s,p)
        ;"Purpose: given position of start of string, returns index of end of string
        ;"Input: s -- the string to eval
        ;"       p -- the index of the start of the string
        ;"Results : returns the index of the end of the string, or 0 IF not found.
        NEW RESULT SET RESULT=0
        FOR p=p+1:1 QUIT:(p>$LENGTH(s))!(RESULT>0)  DO
        . IF $EXTRACT(s,p)'="""" QUIT
        . SET p=p+1
        . IF $EXTRACT(s,p)="""" QUIT
        . SET RESULT=p-1
        QUIT RESULT

NonWhite(s,p)
        ;"Purpose: given starting position, return index of first non-whitespace character
        ;"         Note: either a " " or a TAB [$CHAR(9)] will be considered a whitespace char
        ;"RESULT: returns index IF non-whitespace, or index past end of string IF none found.
        NEW RESULT,ch,DONE
        FOR RESULT=p:1 QUIT:(RESULT>$LENGTH(s))  DO  QUIT:DONE
        . SET ch=$EXTRACT(s,RESULT)
        . SET DONE=(ch'=" ")&(ch'=$CHAR(9))
        QUIT RESULT

Pad2Pos(Pos,ch)
        QUIT $$PAD2POS^TMGSTUT2(.Pos,.ch)

HTML2TXT(Array)
        ;"NOTICE: DEPRECIATED.  USE VERSION IN ^TMGHTM1
        ;"--- THERE HAS BEEN A FORK BETWEEN THAT VERSION AND THIS...
        ;"Purpose: text a WP array that is HTML formatted, and strip <P>, and
        ;"         return in a format of 1 line per array node.
        ;"Input: Array -- PASS BY REFERENCE.  This array will be altered.
        ;"Results: none
        ;"NOTE: This conversion causes some loss of HTML tags, so a round trip
        ;"      conversion back to HTML would fail.
        ;"NOTE: NO LONGER USED.  USED TO BE CALLED FROM TMGTIUO3

        NEW outArray,outI
        SET outI=1

        ;"Clear out confusing non-breaking spaces.
        NEW spec
        SET spec("&nbsp;")=" "
        SET spec("&lt;")="<"
        SET spec("&gt;")=">"
        SET spec("&amp;")="&"
        SET spec("&quot;")=""""
        NEW line SET line=0
        FOR  SET line=$ORDER(Array(line)) QUIT:(line="")  DO
        . NEW lineS SET lineS=$GET(Array(line,0))
        . SET Array(line,0)=$$REPLACE^XLFSTR(lineS,.spec)

        NEW s2 SET s2=""
        NEW line SET line=0
        FOR  SET line=$ORDER(Array(line)) QUIT:(line="")  DO
        . NEW lineS SET lineS=s2_$GET(Array(line,0))
        . SET s2=""
        . FOR  DO  QUIT:(lineS'["<")
        . . IF (lineS["<P>")&($PIECE(lineS,"<P>",1)'["<BR>") DO  QUIT
        . . . SET outArray(outI,0)=$PIECE(lineS,"<P>",1)
        . . . SET outI=outI+1
        . . . SET outArray(outI,0)=""  ;"Add blank line to create paragraph break.
        . . . SET outI=outI+1
        . . . SET lineS=$PIECE(lineS,"<P>",2,999)
        . . IF (lineS["</P>")&($PIECE(lineS,"</P>",1)'["<BR>") DO  QUIT
        . . . SET outArray(outI,0)=$PIECE(lineS,"</P>",1)
        . . . SET outI=outI+1
        . . . SET outArray(outI,0)=""  ;"Add blank line to create paragraph break.
        . . . SET outI=outI+1
        . . . SET lineS=$PIECE(lineS,"</P>",2,999)
        . . IF (lineS["</LI>")&($PIECE(lineS,"</LI>",1)'["<BR>") DO  QUIT
        . . . SET outArray(outI,0)=$PIECE(lineS,"</LI>",1)   ;"   _"</LI>"
        . . . SET outI=outI+1
        . . . SET outArray(outI,0)=""  ;"Add blank line to create paragraph break.
        . . . SET outI=outI+1
        . . . SET lineS=$PIECE(lineS,"</LI>",2,999)
        . . IF lineS["<BR>" DO  QUIT
        . . . SET outArray(outI,0)=$PIECE(lineS,"<BR>",1)
        . . . SET outI=outI+1
        . . . SET lineS=$PIECE(lineS,"<BR>",2,999)
        . . SET s2=lineS,lineS=""
        . SET s2=s2_lineS
        IF s2'="" DO
        . SET outArray(outI,0)=s2
        . SET outI=outI+1

        KILL Array
        MERGE Array=outArray
        QUIT


TrimTags(lineS)
        ;"Purpose: To cut out HTML tags (e.g. <...>) from lineS, however, <no data> is protected
        ;"Input: lineS : the string to work on.
        ;"Results: the modified string
        ;"Called from: TMGTIUOJ.m
        NEW RESULT,key,spec
        SET spec("<no data>")="[no data]"
        SET RESULT=$$REPLACE^XLFSTR(lineS,.spec)
        FOR  QUIT:((RESULT'["<")!(RESULT'[">"))  DO
        . NEW partA,partB
        . SET partA=$PIECE(RESULT,"<",1)
        . NEW temp SET temp=$EXTRACT(RESULT,$LENGTH(partA)+1,999)
        . SET partB=$PIECE(temp,">",2,99)
        . SET RESULT=partA_partB
       QUIT RESULT

ARRTOHF(TMGARRAY,TMGFPATH,TMGFNAME) ;"Array to Host File
        ;"NOTE: This may be duplicate of ARR2HFS^TMGIOUT3
        ;"Purpose: to WRITE the array to the host file system
        ;"Input: TMGARRAY -- PASS BY REFERENCE -- The array containing the text of the output file
        ;"                  example: TMGARRAY(1)="line #1"   <-- note, **don't** use TMGARRAY(1,0)="Line#1" format
        ;"                  TMGARRAY(2)="line #2"
        ;"       TMGFPATH -- The full path, upto but not including the filename.
        ;"       TMGFNAME -- the name of the file to save to
        ;"Result: 1 if OK, -1^Message IF problem.
        NEW TMGRESULT
        NEW TMGREF SET TMGREF=$NAME(^TMP($J,"TMG STUTL HFS"))
        KILL @TMGREF
        MERGE @TMGREF=TMGARRAY
        SET TMGRESULT=$$GTF^%ZISH($NAME(@TMGREF@(1)),3,TMGFPATH,TMGFNAME)
        IF TMGRESULT'>0 SET TMGRESULT="-1^Unable to output to file: "_TMGFPATH_TMGFNAME
        KILL @TMGREF
        QUIT TMGRESULT
        ;
HFTOARR(TMGARRAY,TMGFPATH,TMGFNAME) ;"Host File to Array
        ;"NOTE: This may be duplicate of HFS2ARR^TMGIOUT3
        ;"Purpose: to WRITE the array to the host file system
        ;"Input: TMGARRAY -- PASS BY REFERENCE -- The array to containing the text reading from the file
        ;"         example: TMGARRAY(1)="line #1 from host file system"
        ;"                  TMGARRAY(2)="line #2 from host file system"
        ;"       TMGFPATH -- The full path, upto but not including the filename.
        ;"       TMGFNAME -- the name of the file to save to
        ;"Result: 1 if OK, -1^Message IF problem.
        NEW TMGRESULT
        KILL TMGARRAY
        NEW TMGREF SET TMGREF=$NAME(^TMP($J,"TMG STUTL HFS"))
        KILL @TMGREF
        SET TMGRESULT=$$FTG^%ZISH(TMGFPATH,TMGFNAME,$NAME(@TMGREF@(1)),3)
        IF TMGRESULT'>0 SET TMGRESULT="-1^Unable to read from file: "_TMGFPATH_TMGFNAME
        NEW TMGI SET TMGI=0
        FOR  SET TMGI=$ORDER(@TMGREF@(TMGI)) QUIT:(+TMGI'>0)  DO
        . SET TMGARRAY(TMGI)=$GET(@TMGREF@(TMGI))
        KILL @TMGREF
        QUIT TMGRESULT
        ;