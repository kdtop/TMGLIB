TMGRSTU1 ;TMG/kst/REST web service Utilities; 4/23/14
       ;;1.0;TMG-LIB;**1**;4/21/14
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
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"ARR2CSTR(ARR) ;Convert array into a coded long string, that can be converted back to array
 ;"REF2CSTR(REF) ;Convert array REF into a coded long string, that can be converted back to array
 ;"CSTR2REF(CSTR,VARNAME) ;Turn a coded string back into an array
 ;"REF2CARR(REF,OUTARR) ;Convert array into a coded string array
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"VAL2CSTR(LINE) ;Turn one line into <length>|<text>| format.
 ;"NEXTSTR(CSTR) ;parse off next section from coded string
 ;"MAKESET(LINE,VARNAME) ;convert line into line ready for execution
 ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"=======================================================================
 ;"Uses:  TMGZWR
 ;"=======================================================================
 ;
ARR2CSTR(ARR) ;
  ;"Purpose: convert array into a coded long string, 
  ;"   that can be converted back to array
  ;"Input: ARR --  PASS BY REFERENCE
  ;"NOTE: this will cause the output string to use 'ARR' for array name
  ;"Results: see REF2CSTR
  QUIT $$REF2CSTR("ARR")
  ;
REF2CSTR(REF) ;
  ;"Purpose: convert array into a coded long string, 
  ;"   that can be converted back to array
  ;"Input: REF --  PASS BY NAME of array to convert
  ;"Result: a string with format as follows:
  ;"   <length>|<text>|<length>|<text>|....
  ;"
  ;"   e.g. if input array x show ZWRITE like this:
  ;"     x("a")=5
  ;"     x("a","b")=7
  ;"     x("a","b","c")=1
  ;"     x("j")=""
  ;"   then the output of this would be
  ;"     8|x("a")=5|12|x("a","b")=7|16|x("a","b","c")=1|9|x("j")=""|
  ;"
  ;"NOTE: the "|" character can exist in <text> since length is used
  ;"      to parse text.  DON'T just $PIECE based on "|" unless sure
  ;"      that <text> doesn't contain "|"
  ;
  NEW TMGZZTEMP
  NEW TMGRESULT SET TMGRESULT=""
  DO ZWR2ARR^TMGZWR(REF,"TMGZZTEMP")
  NEW IDX SET IDX=""
  FOR  SET IDX=$ORDER(TMGZZTEMP(IDX)) QUIT:(IDX="")  DO
  . NEW LINE SET LINE=$GET(TMGZZTEMP(IDX))
  . SET TMGRESULT=TMGRESULT_$$VAL2CSTR(LINE)
  QUIT TMGRESULT
  ;  
VAL2CSTR(LINE) ;
  ;"Purpose: turn one line into <length>|<text>| format.
  ;"NOTE: line begins with a number that is length, then "|",
  ;"      then the text itself, then a final "|"
  SET LINE=$GET(LINE)
  NEW RESULT SET RESULT=$LENGTH(LINE)_"|"_LINE_"|"
  QUIT RESULT
  ;
CSTR2REF(CSTR,VARNAME) ;
  ;"Purpose: Turn a coded string into an array
  ;"Input: CSTR -- coded string, such as made by REF2CSTR
  ;"       VARNAME -- Name of variable to set/store array elements into
  ;"Results: none
  ;"NOTE: prior results in @VARNAME are NOT killed
  ;"E.g. if CSTR = 
  ;"  8|x("a")=5|12|x("a","b")=7|16|x("a","b","c")=1|9|x("j")=""|
  ;"   and VARNAME = "ZZTMP"
  ;"  output is as follows (variable is set in global scope)
  ;"     ZZTMP("a")=5
  ;"     ZZTMP("a","b")=7
  ;"     ZZTMP("a","b","c")=1
  ;"     ZZTMP("j")=""
  SET CSTR=$GET(CSTR)
  SET VARNAME=$GET(VARNAME)
  IF VARNAME="" QUIT
  FOR  QUIT:CSTR=""  DO
  . NEW LINE SET LINE=$$NEXTSTR(.CSTR)
  . NEW CODE SET CODE=$$MAKESET(LINE,VARNAME)
  . IF CODE="" QUIT
  . XECUTE CODE
  QUIT
  ;
NEXTSTR(CSTR) ;
  ;"Purpose: parse off next section from coded string
  ;"Input: CSTR -- PASS BY REFERENCE -- A string in format of 
  ;"   <length>|<text>|<length>|<text>|....
  ;"Result: Returns the next portion of text, which will of length <length>
  ;"      ALSO, CSTR is shortened appropriately
  SET CSTR=$GET(CSTR)
  NEW LEN SET LEN=$PIECE(CSTR,"|",1)
  NEW P1 SET P1=$LENGTH(LEN)+2
  NEW P2 SET P2=P1+LEN-1
  NEW RESULT SET RESULT=$EXTRACT(CSTR,P1,P2)
  SET CSTR=$EXTRACT(CSTR,P2+2,$LENGTH(CSTR))
  QUIT RESULT
  ;
MAKESET(LINE,VARNAME) ;
  ;"Purpose: convert line into line ready for execution
  ;"Input: LINE -- string, e.g. 'x("b")=12'
  ;"       VARNAME -- the name of the array that should old vale, e.g. "ZZTMP"
  ;"Result: e.g. 'SET ZZTMP("b")=12', or '' if problem
  SET LINE=$GET(LINE)
  NEW LVAR SET LVAR=$PIECE(LINE,"=",1)
  NEW RVAL SET RVAL=$PIECE(LINE,"=",2,999)
  NEW QTWRAP SET QTWRAP=(($EXTRACT(RVAL,1)="""")&($EXTRACT(RVAL,$LENGTH(RVAL))=""""))
  IF QTWRAP DO
  . SET RVAL=$EXTRACT(RVAL,2,$LENGTH(RVAL)-1)
  IF RVAL["""" SET RVAL=$$QTPROTCT^TMGSTUT3(RVAL)
  IF QTWRAP SET RVAL=""""_RVAL_""""
  IF LVAR["(" DO
  . SET LVAR=VARNAME_"("_$PIECE(LVAR,"(",2,999)
  ELSE  DO
  . SET LVAR=VARNAME
  NEW RESULT SET RESULT="SET "_LVAR_"="_RVAL
  QUIT RESULT
  ;
REF2CARR(REF,OUTARR) ;
  ;"Purpose: convert array into a coded string array
  ;"Input: REF --  PASS BY NAME of array to convert
  ;"       OUTARR -- PASS BY REFERENCE.  The output result.  Format:
  ;"            OUTARR(#)=<formatted string>
  ;"                 e.g. <length>|<text|    
  ;"                 each line will contain only one pair. 
  ;"            e.g. if input array x show ZWRITE like this:
  ;"              x("a")=5
  ;"              x("a","b")=7
  ;"              x("a","b","c")=1
  ;"              x("j")=""
  ;"            then the output of this would be
  ;"              OUTARR(1)='8|x("a")=5|'
  ;"              OUTARR(2)='12|x("a","b")=7|'
  ;"              OUTARR(3)='16|x("a","b","c")=1|'
  ;"              OUTARR(4)='9|x("j")=""|'
  ;"
  ;"        NOTE: the "|" character can exist in <text> since length is used
  ;"              to parse text.  DON'T just $PIECE based on "|" unless sure
  ;"              that <text> doesn't contain "|"
  ;"Result: NONE
  ;
  NEW TMGZZTEMP
  NEW TMGIDX SET TMGIDX=1
  DO ZWR2ARR^TMGZWR(REF,"TMGZZTEMP")
  NEW IDX SET IDX=""
  FOR  SET IDX=$ORDER(TMGZZTEMP(IDX)) QUIT:(IDX="")  DO
  . NEW LINE SET LINE=$GET(TMGZZTEMP(IDX))
  . SET OUTARR(TMGIDX)=$$VAL2CSTR(LINE)
  . SET TMGIDX=TMGIDX+1
  QUIT
  ;  