TMGMISC3 ;TMG/kst/Misc utility librar ;9/6/17, 5/21/18  
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
ARRDUMP(REF,TMGIDX,INDENT)  ;"ARRAY DUMP
  ;"NOTE: similar to ArrayDump^TMGIDE (taken from there and modified)
  ;"PUBLIC FUNCTION
  ;"Purpose: to get a custom version of GTM's "zwr" command
  ;"Input:  REF -- NAME of global to display, i.e. "^VA(200)"
  ;"        TMGIDX --initial index (i.e. 5 if wanting to start with ^VA(200,5)
  ;"        INDENT --spacing from left margin to begin with. (Each count is 2 spaces)
  ;"           INDENT may optionally be an array, with information about columns to skip.  
  ;"            For example:
  ;"                INDENT=3
  ;"                INDENT(2)=0 --> show | for columns 1 & 3, but NOT 2
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
  NEW JDX FOR JDX=1:1:INDENT-1 WRITE $SELECT($GET(INDENT(JDX),-1)=0:" ",1:"| ")
  WRITE "}~"
  ;              
  SET TMGIDX=$GET(TMGIDX)
  IF TMGIDX'="" DO
  . IF $DATA(@REF@(TMGIDX))#10=1 DO
  . . NEW STR SET STR=@REF@(TMGIDX)
  . . IF STR="" SET STR=""""""
  . . IF $LENGTH(STR)'=$LENGTH($$TRIM^XLFSTR(STR)) SET STR=""""_STR_""""  ;"//kt 9/6/17
  . . NEW QT SET QT=""
  . . IF +TMGIDX'=TMGIDX SET QT=""""
  . . WRITE QT_TMGIDX_QT_" = "_STR,!
  . ELSE  DO
  . . WRITE TMGIDX,!
  . SET REF=$NAME(@REF@(TMGIDX))
  ELSE  DO
  . WRITE REF
  . IF $DATA(@REF)#10=1 WRITE 0,"="_$GET(@REF)
  . WRITE !
  ;
  SET TMGIDX=$ORDER(@REF@(""))
  IF TMGIDX="" GOTO ADDN                
  SET INDENT=INDENT+1
  FOR  DO  QUIT:TMGIDX=""                                    
  . NEW IDX SET IDX=$ORDER(@REF@(TMGIDX))
  . IF IDX="" SET INDENT(INDENT)=0
  . NEW TEMPINDENT MERGE TEMPINDENT=INDENT
  . DO ARRDUMP(REF,TMGIDX,.TEMPINDENT)  ;"Call self recursively
  . SET TMGIDX=$ORDER(@REF@(TMGIDX))
ADDN  ;
  QUIT
  ;   
