TMGTIUT2 ;TMG/kst-TIU TEMPLATE searching templates ; 10/26/14
   ;;1.0;TMG-LIB;**1**;10/26/14
  ;
  ;"Code for fast searching TIU TEMPLATES
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
  ;" RPC -- Public Functions.
  ;"=======================================================================
  ;
  ;"=======================================================================
  ;"PRIVATE API FUNCTIONS
  ;"=======================================================================
  ;
  ;"=======================================================================
  ;"DEPENDENCIES: 
  ;"=======================================================================
  ;
SRCHTMPL(OUT,WORDS,DUZ,CS) ;"RPC Entry point
  ;"Purpose: Quickly search through a user's templates (faster than CPRS search method)
  ;"Input: WORDS -- a string with various search fragments separated by spaces
  ;"       OUT -- PASS BY REFERENCE.  An array of matching templates found.  Format:
  ;"          OUT(0)="1^OK"
  ;"          OUT(#)=IEN;TemplateName^ParentIEN;ParentTemplateName^GrandparentIEN;GrandparentName ... etc
  ;"               e.g. 'diabet tabl'
  ;"       DUZ -- the IEN of the user to search for.  
  ;"       CS -- OPTIONAL.  If 1 then searching is case sensitive.  Default is 0.
  NEW TERMS,DATA,IDX,TEMP
  DO SPLIT2AR^TMGSTUT2(WORDS," ",.TEMP)  
  SET IDX=0 FOR  SET IDX=$ORDER(TEMP(IDX)) QUIT:+IDX'>0  DO
  . NEW T SET T=$GET(TEMP(IDX)) QUIT:T=""
  . SET TERMS("TERM",T)=""
  DO GETROOTS(.DATA,DUZ)
  NEW OPTIONS SET OPTIONS("CASE SENSITIVE")=+$GET(CS)
  SET OUT(0)="1^OK"
  SET IDX=0 FOR  SET IDX=$ORDER(DATA(IDX)) QUIT:IDX'>0  DO
  . NEW IEN SET IEN=+$PIECE($GET(DATA(IDX)),"^",1) QUIT:IEN'>0
  . DO SEARCH1(IEN,.TERMS,.OUT,.OPTIONS)
  KILL OUT("CHECKED")
  QUIT
  ;
GETROOTS(OUT,DUZ) ;Get template root info  ;"Modified from  GETROOTS^TIUSRVT.m
  NEW IDX SET IDX=0
  DO ADDNODE($ORDER(^TIU(8927,"AROOT",DUZ,0)),.OUT,.IDX)
  DO ADDNODE($ORDER(^TIU(8927,"AROOT",$$ROOTIDX^TIUDDT("R"),0)),.OUT,.IDX)
  QUIT
  ;
ADDNODE(IEN,OUT,IDX) ;"Add node data
  SET IEN=+$GET(IEN)  ;"Had situation where user had no templates, led to IEN=""  3/3/21
  SET IDX=IDX+1,OUT(IDX)=IEN_"^"_$PIECE($GET(^TIU(8927,IEN,0)),"^",1)
  QUIT
  ;
SEARCH1(IEN,TERMS,OUT,OPTIONS,PARENTINFO) ;"Search template root and children for terms
  ;"Input: IEN -- IEN in 8927 to start search in (including children)
  ;"       TERMS -- PASS BY REFERENCE.  An array of 1 or more terms to search for.  Format:
  ;"          TERMS("TERM","cat")=""
  ;"          TERMS("TERM","dog")=""
  ;"          TERMS("TERM","bird")=""   <-- search terms don't have to be complete words
  ;"          TERMS("UC TERM","CAT")=""  <-- These don't have to be sent in.  They
  ;"          TERMS("UC TERM","DOG")=""      will be automatically generated if "UC TERM" node is
  ;"          TERMS("UC TERM","BIRD")=""     not found, and OPTIONS("CASE SENSITIVE")=1 not present
  ;"       OUT -- PASS BY REFERENCE.  An array of terms found.  Format
  ;"          OUT(#)=IEN;TemplateName^ParentIEN;ParentTemplateName^GrandparentIEN;GrandparentName ... etc
  ;"          OUT("CHECKED",IEN)=""
  ;"       OPTIONS -- OPTIONAL.  PASS BY REFERENCE.  Format:
  ;"         OPTIONS("CASE SENSITIVE")=1  <-- search will be case secific.  Default is NOT case specific.
  ;"       PARENTINFO -- OPTIONAL.  Used in recursive calls. If provided, then will be used.
  ;"                     DON'T pass by reference.
  NEW CS SET CS=+$GET(OPTIONS("CASE SENSITIVE"))
  NEW TERMNODE SET TERMNODE="TERM"
  IF CS'=1 SET TERMNODE="UC TERM"
  IF $DATA(TERMS(TERMNODE))=0 DO
  . NEW T SET T="" FOR  SET T=$ORDER(TERMS("TERM",T)) QUIT:T=""  DO
  . . SET TERMS("UC TERM",$$UP^XLFSTR(T))=""
  SET PARENTINFO=$GET(PARENTINFO) IF PARENTINFO'="" SET PARENTINFO="^"_PARENTINFO
  NEW ZN SET ZN=$GET(^TIU(8927,IEN,0))
  NEW NAME SET NAME=$PIECE(ZN,"^",1)
  NEW INITNAME SET INITNAME=NAME
  IF CS'=1 SET NAME=$$UP^XLFSTR(NAME)
  NEW MATCH SET MATCH=1
  NEW T SET T=""
  NEW OUTINFO SET OUTINFO=IEN_";"_INITNAME_PARENTINFO
  FOR  SET T=$ORDER(TERMS(TERMNODE,T)) QUIT:(T="")!(MATCH=0)  IF NAME'[T SET MATCH=0
  IF MATCH DO
  . NEW IDX SET IDX=+$ORDER(OUT("@"),-1)+1
  . SET OUT(IDX)=OUTINFO
  SET OUT("CHECKED",IEN)=1
  IF +$PIECE(ZN,"^",12)=1 GOTO S1DN ;"field HIDE DIALOG ITEMS.
  IF +$PIECE(ZN,"^",13)=1 GOTO S1DN ;"field HIDE TREE ITEMS.
  ;"Now search through this nodes children
  NEW CHILDIDX SET CHILDIDX=0
  FOR  SET CHILDIDX=$ORDER(^TIU(8927,IEN,10,CHILDIDX)) QUIT:(CHILDIDX'>0)  DO
  . NEW CHILDIEN SET CHILDIEN=+$PIECE($GET(^TIU(8927,IEN,10,CHILDIDX,0)),"^",2)
  . IF +CHILDIEN'>0 QUIT
  . IF $DATA(OUT("CHECKED",CHILDIEN)) QUIT  ;"Prevent endless loops.
  . DO SEARCH1(CHILDIEN,.TERMS,.OUT,.OPTIONS,OUTINFO) 
S1DN ;  
  QUIT
  ;
