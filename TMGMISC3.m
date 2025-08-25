TMGMISC3 ;TMG/kst/Misc utility librar ;9/6/17, 7/24/22 
         ;;1.0;TMG-LIB;**1**;9/6/17
 ;
 ;"TMG USER INTERFACE API FUNCTIONS
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 9/6/17  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"ARRDUMP(REF,TMGIDX,INDENT) -- dump out array in tree format.  
 ;"DELTARRAY(ARR1,ARR2,DELTA) --FIND DIFFERENCE BETWEN ARRAYS
 ;"INDEX(ARR,LINE)  --Return index of line found in ARR (first match)
 ;"SHR(NUM,DIGITS)  //BINARY SHIFT RIGHT
 ;"SHL(NUM,DIGITS) //BINARY SHIFT LEFT
 ;"GCD(U, V) //GREATEST COMMON DENOMINATOR
 ;
 ;"=======================================================================
 ;"Private Functions
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;"DEPENDENCIES:          
 ;"=======================================================================
 ;
ARRDUMP(REF,TMGIDX,INDENT,OUTREF)  ;"ARRAY DUMP
  ;"NOTE: similar to ArrayDump^TMGIDE (taken from there and modified)
  ;"      But this function is different enough that I will keep separate
  ;"     See also ZWR2ARR^TMGZWR(NAME,OUTREF,STARTIDX)
  ;"PUBLIC FUNCTION
  ;"Purpose: to get a custom version of GTM's "zwr" command
  ;"Input:  REF -- NAME of global to display, i.e. "^VA(200)"
  ;"        TMGIDX --initial index (i.e. 5 if wanting to start with ^VA(200,5)
  ;"        INDENT --spacing from left margin to begin with. (Each count is 2 spaces)
  ;"           INDENT may optionally be an array, with information about columns to skip.  
  ;"            For example:
  ;"                INDENT=3
  ;"                INDENT(2)=0 --> show | for columns 1 & 3, but NOT 2
  ;"        OUTREF -- OPTIONAL.  If provided, output is put into @OUTREF@(#)=<text>, @OUTREF=<last used line #>
  ;"Result: None
  NEW RESULT SET RESULT=0
  NEW $ETRAP SET $ETRAP="SET RESULT="""",$ETRAP="""",$ecode="""""
  SET INDENT=$GET(INDENT,0)
  IF $DATA(REF)=0 GOTO ADDN
  NEW ABORT SET ABORT=0
  IF (REF["@") DO  GOTO:(ABORT=1) ADDN
  . NEW TEMP SET TEMP=$PIECE($EXTRACT(REF,2,99),"@",1)
  . IF $DATA(TEMP)#10=0 SET ABORT=1
  NEW X SET X="SET TEMP=$GET("_$$UP^XLFSTR(REF)_")"
  DO ^DIM ;"Fileman method to ensure REF doesn't have an invalid reference.
  IF $GET(X)="" GOTO ADDN
  ;  
  NEW JDX FOR JDX=1:1:INDENT-1 DO
  . DO ADWRITE($SELECT($GET(INDENT(JDX),-1)=0:" ",1:"| "),0,.OUTREF)
  IF INDENT>0 DO ADWRITE("}~",0,.OUTREF)
  ;              
  SET TMGIDX=$GET(TMGIDX)
  IF TMGIDX'="" DO
  . IF $DATA(@REF@(TMGIDX))#10=1 DO
  . . NEW STR SET STR=@REF@(TMGIDX)
  . . IF STR="" SET STR=""""""
  . . IF $LENGTH(STR)'=$LENGTH($$TRIM^XLFSTR(STR)) SET STR=""""_STR_""""  ;"//kt 9/6/17
  . . NEW QT SET QT=""
  . . IF +TMGIDX'=TMGIDX SET QT=""""
  . . DO ADWRITE(QT_TMGIDX_QT_" = "_STR,1,.OUTREF)
  . ELSE  DO
  . . DO ADWRITE(TMGIDX,1,.OUTREF)
  . SET REF=$NAME(@REF@(TMGIDX))
  ELSE  DO
  . DO ADWRITE(REF,0,.OUTREF)
  . IF $DATA(@REF)#10=1 DO ADWRITE("="_$GET(@REF),0,.OUTREF)
  . DO ADWRITE("",1,.OUTREF)
  ;
  SET TMGIDX=$ORDER(@REF@(""))
  IF TMGIDX="" GOTO ADDN                
  SET INDENT=INDENT+1
  FOR  DO  QUIT:TMGIDX=""                                    
  . NEW IDX SET IDX=$ORDER(@REF@(TMGIDX))
  . IF IDX="" SET INDENT(INDENT)=0
  . NEW TEMPINDENT MERGE TEMPINDENT=INDENT
  . DO ARRDUMP(REF,TMGIDX,.TEMPINDENT,.OUTREF)  ;"Call self recursively
  . SET TMGIDX=$ORDER(@REF@(TMGIDX))
ADDN  ;
  QUIT
  ;   
ADWRITE(STR,LINEFEED,OUTREF) ;
  IF $GET(OUTREF)="" DO 
  . WRITE STR
  . IF +$GET(LINEFEED) WRITE !
  ELSE  DO
  . NEW IDX SET IDX=$GET(@OUTREF)
  . IF IDX="" SET IDX=1
  . NEW PRIOR SET PRIOR=$GET(@OUTREF@(IDX))
  . SET @OUTREF@(IDX)=PRIOR_STR
  . IF $GET(LINEFEED) SET IDX=IDX+1
  . SET @OUTREF=IDX
  QUIT
  ;
DELTARRAY(ARR1,ARR2,DELTA) ;"FIND DIFFERENCE BETWEN ARRAYS
  ;"INPUT: ARR1 -- PASS BY REFERENCE
  ;"       ARR2 -- PASS BY REFERENCE
  ;"       DELTA -- OPTIONAL.  PASS BY REFERENCE.  Filled with changed lines.  
  ;"          DELTA(1,<EXTRA LINE NOT IN ARR2>)=""
  ;"          DELTA(2,<EXTRA LINE NOT IN ARR1>)=""
  KILL DELTA
  NEW IDX SET IDX=""
  ;"Find every line in ARR1 not in ARR2
  FOR  SET IDX=$ORDER(ARR1(IDX)) QUIT:IDX=""  DO
  . NEW LINE SET LINE=$GET(ARR1(IDX)) QUIT:LINE=""
  . NEW TMP SET TMP=$$INDEX(.ARR2,LINE) QUIT:TMP'=""
  . SET DELTA(1,LINE)=""
  ;"Find every line in ARR2 not in ARR1
  SET IDX=""
  FOR  SET IDX=$ORDER(ARR2(IDX)) QUIT:IDX=""  DO
  . NEW LINE SET LINE=$GET(ARR2(IDX)) QUIT:LINE=""
  . NEW TMP SET TMP=$$INDEX(.ARR1,LINE) QUIT:TMP'=""
  . SET DELTA(2,LINE)=""
  QUIT
  ;
INDEX(ARR,LINE)  ;"Return index of line found in ARR (first match)
  NEW RESULT SET RESULT=""
  SET LINE=$GET(LINE) IF LINE="" GOTO IDXDN
  NEW IDX SET IDX=""
  FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:(IDX="")!(RESULT'="")  DO
  . IF $GET(ARR(IDX))=LINE SET RESULT=IDX
IDXDN ;
  QUIT RESULT

SHR(NUM,DIGITS)  ;"//BINARY SHIFT RIGHT
  NEW RESULT SET RESULT=NUM
  SET DIGITS=$GET(DIGITS,1)
  NEW I FOR I=1:1:DIGITS DO
  . SET RESULT=(RESULT/2)\1
  QUIT RESULT
  ;
SHL(NUM,DIGITS) ;"//BINARY SHIFT LEFT
  NEW RESULT SET RESULT=NUM
  SET DIGITS=$GET(DIGITS,1)
  NEW I FOR I=1:1:DIGITS DO
  . SET RESULT=RESULT*2
  QUIT RESULT
  ;
GCD(U,V) ;"//GREATEST COMMON DENOMINATOR
  ;"From here: https://en.wikipedia.org/wiki/Binary_GCD_algorithm
  ;"// simple cases (termination)
  IF U=V QUIT U
  IF U=0 QUIT V
  IF V=0 QUIT U
  ;
  NEW RESULT SET RESULT=0
  ;"// look for factors of 2
  IF U#2=0 DO  ;"// u is even
  . IF V#2=1 DO  ;"// v is odd
  . . SET RESULT=$$GCD($$SHR(U),V)
  . ELSE  DO  ;"// both u and v are even
  . . SET RESULT=$$SHL($$GCD($$SHR(U),$$SHR(V)))
  ;
  ELSE  IF V#2=0 DO  ;"// u is odd, v is even
  . SET RESULT=$$GCD(U,$$SHR(V))
  ;"// reduce larger argument
  ELSE  IF U>V DO   
  . SET RESULT=$$GCD($$SHR(U-V),V)
  ELSE  SET RESULT=$$GCD($$SHR(V-U),U)
  ;
  QUIT RESULT
  ;
NOPATSEL(TMGRESULT)  ;"DEPRECIATED -- MOVED TO TMGAPPT1 -- Delete later  (changed 4/18/24)
  DO NOPATSEL^TMGAPPT1(.TMGRESULT)
  QUIT
  ;"