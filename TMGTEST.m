TMGTEST ;TMG/kst/Scratch fns for programming tests ;03/25/06, 2/2/14
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
  DO
  . WRITE "HELLO",!
  . WRITE "WORLD",!
  . DO
  . . WRITE "PEACE!",!!
  NEW array
  SET array="Fruits:"
  SET array(1)="apple"                                        
  SET array(2)="pear"
  SET array(3)="peach"
  DO ZWRITE^TMGZWR("array")
  NEW i,j,k
  for i=1:1:10 do  write "+"                                           
  .for j=1:1:10 do  write "^"
  ..for k=1:1:10 do  write "%"
  ...write "*"
  ...write "^"
  ...write "%"
  ...write "$"
  
  QUIT
  ;                                                                
B(TEST) ;                                                     
  WRITE TEST,!    
  QUIT
  ;
TLOOP(OUT) ;
  NEW CT SET CT=0
  NEW IDX,JDX SET (IDX,JDX)=0
  FOR IDX=1:1:4 DO
  . FOR JDX=1:1:4 DO
  . . SET OUT($$RJ^XLFSTR(IDX,5,"0")_","_$$RJ^XLFSTR(JDX,5,"0"))=CT,CT=CT+1
  SET IDX=0
  NEW ISTR
  FOR  SET ISTR=$$RJ^XLFSTR(IDX,5,"0")_",99999",IDX=+$ORDER(OUT(ISTR)) QUIT:IDX'>0  DO
  . WRITE "I=",IDX,!
  . SET JDX=0
  . NEW JSTR,TMP
  . NEW DONE SET DONE=0
  . FOR  SET JSTR=$$CSTR(IDX,JDX),TMP=$ORDER(OUT(JSTR)) QUIT:(+TMP'=IDX)  SET JDX=+$PIECE(TMP,",",2) QUIT:JDX'>0  DO
  . . WRITE "  J=",JDX,!
  . FOR  SET JSTR=$$CSTR(IDX,JDX),TMP=$ORDER(OUT(JSTR)) QUIT:(+TMP'=IDX)  SET JDX=+$PIECE(TMP,",",2) QUIT:JDX'>0  DO
  . . WRITE "  J=",JDX,!
  QUIT
  ;
CSTR(IDX,JDX) ;
  QUIT $$RJ^XLFSTR(IDX,5,"0")_","_$$RJ^XLFSTR(JDX,5,"0")
  ;
TEST ;
 WRITE "LOGIC TEST. BOOL=",0&$$T1()&$$T2()
 WRITE !,"DONE"
 QUIT
TEST2 ;
 WRITE "LOGIC TEST2. BOOL=",0&$S(0:$$T1(),1:1)
 WRITE !,"DONE"
 QUIT
TEST3 ;
 WRITE "LOGIC TEST3-1. BOOL=",0&$S(0:@a,1:1)
 WRITE !,"DONE"
 QUIT
 
T1() W " T1 " QUIT 0
T2() W " T2 " QUIT 0

  ;"
TESTDATE
  NEW %DT,X,Y
  SET %DT("A")="HOW LONG TO WAIT (T+90): "
  SET %DT="A"
  DO ^%DT
  IF Y=-1 DO
  . KILL %DT
  . SET X="T+90"
  . DO ^%DT
  WRITE !,Y,!
  QUIT
  ;"
TESTORN ;
  SET ORN=0
  FOR  SET ORN=$ORDER(^ORD(100.9,ORN)) QUIT:+ORN'>0  DO
  . NEW ZN SET ZN=$GET(^ORD(100.9,ORN,0))
  . WRITE ORN,":",$P(ZN,"^",1)," --> ",$$ONOFF^ORB3FN(ORN),!
  QUIT
