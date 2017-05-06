TMGDICF3 ;TMG/kst/Fileman alteration ;2/10/15
         ;;1.0;TMG-LIB;**1**;2/10/15
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
 ;
WEDGE(DIFLAGS,DINDEX,DIDENT,DISCREEN) ;
  ;"This adds an additional cycle and additional test to DINDEX array
  ;"It allows for classic (partial) match on names, e.g. 'Smi,jo'
  ;"It is required because partial match fails for mnemonic entries in index.
  ;"Called from DICF3
  ;"NOTE: This just test the value stored in the index.  It doesn't look up 
  ;"     the .01 value stored for the mnemonic value.  Thus is original value
  ;"     was longer than 30 characters, this could return a false positive.
  SET DIFLAGS=$GET(DIFLAGS)
  IF (DIFLAGS="")!(DIFLAGS'["C") QUIT
  NEW PART SET PART=$GET(DINDEX(1,"PART")) 
  IF (PART="")!(PART'[",") QUIT
  NEW P1 SET P1=$PIECE(PART,",",1) IF P1="" QUIT
  NEW P2 SET P2=$PIECE(PART,",",2)
  NEW MATCH SET MATCH="1"""_P1_""".E1"","""
  IF P2'="" SET MATCH=MATCH_"1"""_P2_""".E"
  NEW IDX SET IDX=0 FOR  SET IDX=$ORDER(DINDEX(1,IDX)) QUIT:+IDX'>0
  SET IDX=$ORDER(DINDEX(1,IDX),-1)+2
  SET DINDEX(1,IDX)=$PIECE(PART,",",1)
  SET DISCREEN(1,IDX)="S %=$G(DINDEX(DISUB)) I %?"_MATCH
  ;"The above screen will be executed from $$SCREEN^DICL2  (actual from S2 label block)
  ;"SET DISCREEN(1,IDX)="IF $$SCRN^TMGDICF3()"  <-- old.
  QUIT
  ;
  ;"SCRN() ;
  ;"  ;Called from $$SCREEN^DICL2  (actual from S2 label block)
  ;"  ;Input: This will rely extensively on variables in global scope. See code
  ;"  ;Results: TRUE(1) should keep item under consideration, FALSE(0) if should skip
  ;"  NEW RESULT SET RESULT=0
  ;"  NEW PART SET PART=$G(DINDEX(DISUB,"PART"))
  ;"  NEW P1 SET P1=$PIECE(PART,",",1) IF P1="" GOTO SCRNDN
  ;"  NEW P2 SET P2=$PIECE(PART,",",2)
  ;"  NEW VAL SET VAL=$GET(DINDEX(DISUB)) IF VAL="" GOTO SCRNDN
  ;"  NEW V1 SET V1=$PIECE(VAL,",",1) IF V1="" GOTO SCRNDN
  ;"  IF $EXTRACT(V1,1,$LENGTH(P1))'=P1 GOTO SCRNDN
  ;"  NEW V2 SET V2=$PIECE(VAL,",",2) 
  ;"  IF $EXTRACT(V2,1,$LENGTH(P2))'=P2 GOTO SCRNDN
  ;"  SET RESULT=1 
  ;"SCRNDN ;
  ;"  QUIT RESULT
