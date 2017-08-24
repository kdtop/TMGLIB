TMGCOD00 ;TMG/kst-Code retrieval  ;6/26/15
         ;;1.0;TMG-LIB;**1**;6/26/15
 ;
 ;"TMG CODE RETRIEVAL
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
 ;"=======================================================================
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"$$TEXT(TAG,OFFSET,ROUTINE)  --Replacement for $TEXT()
 ; 
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;"XREFTAGS(CODEREF,XREF) -- CREATE XREFS OF TAGS  
 ;"LOADFILE(ROUTINE,REF)  -- Load filename into ^TMP global
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"  
 ;"=======================================================================
 ;
TEXT(TAG,OFFSET,ROUTINE)  ;"Replacement for $TEXT()
  ;"Purpose: This is a replacement for $TEXT().  The reason for this is that
  ;"        $TEXT() converts TAB's to SPACES, according to the mumps language
  ;"       standard.  This routine will get the file from the HFS and use that
  ;"      instead.
  ;"Result: line of text, or "" if problem.
  SET TAG=$GET(TAG)
  SET OFFSET=+$GET(OFFSET)
  NEW RESULT SET RESULT=""
  NEW REF SET REF=$NAME(^TMP($J,"TMGCOD00",ROUTINE,"CODE"))
  NEW XREF SET XREF=$NAME(^TMP($J,"TMGCOD00",ROUTINE,"TAG"))
  NEW TMP SET TMP=1
  IF ROUTINE["%" SET ROUTINE=$TRANSLATE(ROUTINE,"%","_") ;"//kt 8/13/17
  IF $DATA(@REF)<1 DO
  . SET TMP=$$LOADFILE(ROUTINE,REF) ;"First line at index 1
  . DO XREFTAGS(REF,XREF)  ;"CREATE XREFS OF TAGS  
  IF TMP'>0 GOTO TXDN
  NEW TAGIDX SET TAGIDX=0 IF TAG'="" SET TAGIDX=+$GET(@XREF@(TAG))
  SET RESULT=$GET(@REF@(TAGIDX+OFFSET))
TXDN ;  
  QUIT RESULT
  ;
XREFTAGS(CODEREF,XREF)  ;"CREATE XREFS OF TAGS  
  ;"Input:  REF -- Reference of storage location
  ;"        TAG -- name to look for
  ;"Result: <line number> or -1 if not found
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(@CODEREF@(IDX)) QUIT:(+IDX'>0)  DO
  . NEW LINE SET LINE=$GET(@CODEREF@(IDX))
  . SET LINE=$TRANSLATE(LINE,$CHAR(9)," ")
  . IF $EXTRACT(LINE,1)=" " QUIT
  . NEW LABEL SET LABEL=$PIECE(LINE," ",1)
  . IF LABEL="" QUIT
  . SET @XREF@(LABEL)=IDX
  QUIT
  ;
LOADFILE(ROUTINE,REF)  ;"Load filename into ^TMP global
  NEW FPATH SET FPATH=$$RTNPATH^TMGKERN1(ROUTINE)
  KILL @REF
  NEW OPTION SET OPTION("OVERFLOW")=1
  NEW RESULT SET RESULT=$$HFS2ARR^TMGIOUT3(FPATH,ROUTINE_".m",REF,.OPTION)
  QUIT RESULT
  ;