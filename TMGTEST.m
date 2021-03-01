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
TESTORN ;
  SET ORN=0
  FOR  SET ORN=$ORDER(^ORD(100.9,ORN)) QUIT:+ORN'>0  DO
  . NEW ZN SET ZN=$GET(^ORD(100.9,ORN,0))
  . WRITE ORN,":",$P(ZN,"^",1)," --> ",$$ONOFF^ORB3FN(ORN),!
  QUIT
  ;
Floor(x)  ;
   quit x\1
;
Round(n,digits) ;
  quit $justify(n,0,digits)
  ;"quit (n+0.5)\1
  ;
SLOPE
  new height,width,intHeight,intWidth
  set height=58.5,intHeight=height\1
  set width=2.8,intWidth=width\1
  new m1 set m1=-height/width
  new m2 set m2=-intHeight/intWidth
  write "slope 1=",m1,!
  write "slope 2=",m2,!
  new x,y
  for x=1:1:20 do
  . new a set a=$$Floor(m1*x+height)
  . new b set b=$$Floor(m2*x+intHeight)
  . set y=a-b
  . write "a=",a," b=",b,"  (",x,",",y,")",!
  quit
  
  QUIT

MissedPoints(w,h) ;
  ;"Input: w, h as real numbers
  kill ^TMP("slope",$j)
  new intW set intW=w\1
  new intH set intH=h\1 
  new m1 set m1=-h/w
  new m2 set m2=-intH/intW
  new total set total=0
  new x
  for x=1:1:w do
  . new p1,p2,p3,p4
  . set p1=$$Round(m1*x+h,2)
  . set p3=$$Round(m2*x+intH,2)
  . set p2=$$Floor(p1)
  . set p4=$$Floor(p3)
  . new temp set temp=p2-p4
  . set ^TMP("slope",$j,x)=x_","_p1_","_p2_","_p3_","_p4 ;" _"#"_x_","_(p1-p4)_","_(p2-p4)_","_(p3-p4)_","_0
  . set total=total+temp
  quit total
  ;
MP2 ;
  new temp set temp=$$MissedPoints(50.5,1234.5)
  zwr ^TMP("slope",$j,*)
  write !,temp,!
  
  NEW x,y set x=$ORDER(^TMP("slope",$j,""),-1),y=^TMP("slope",$j,x)
  new slope set slope=y/x
  set x=0
  for  set x=$order(^TMP("slope",$j,x)) quit:x'>0  do
  . write "x=",x," y=",^TMP("slope",$j,x)," (",slope*x,")",!
  write !,!
  for  set x=$order(^TMP("slope",$j,x)) quit:x'>0  do
  . write $piece(^TMP("slope",$j,x),"#",1),!
  
  quit

IntegerTriangleArea(b,h) ;  ;"//expect integer inputs
  ;"//y=mx+b
  new result set result=0
  new area set area=0
  new slope set slope=-h/b
  write "Slope=",slope,!
  new x for x=0:1:b do
  . new y set y=(slope*x+h)
  . new roundedY set roundedY=(y+0.5)\1
  . write $e("***********************************",1,y)," (",y,") (rounded:",roundedY,")",!
  . set area=area+roundedY
  write "sum area = ",area,!
  write "calc area = ",b*h/2,!
  quit area

pctarea(y0,y1) 
  new ceil set ceil=y0\1+1
  new above set above=(ceil-y0)*1
  new below set below=(y1)*1
  new delta set delta=(y0-y1)*1
  set above=above+(delta/2)
  set below=below+(delta/2)
  quit $j(below,1,2)_"^"_$j(above,1,2)
 ;
area2(b,h)  ;"expect integer inputs
  new slope set slope=-h/b
  
  new x for x=1:1:b do
  . new y0 set y0=slope*(x-1)+h
  . new y1 set y1=slope*x+h
  . new n1 set n1=y0-$p(y1,".",1)
  . new n2 set n2=y1-$p(y1,".",1)
  . write x,": ",$j(y0,1,2)," -> ",$j(y1,1,2)," ==> ",$j(n1,1,2)," ->  ",$j(n2,1,2),"  ",$$pctarea(n1,n2),!
  quit
  
TestRandom(array) ;
  new i
  for i=1:1:50 do
  . set array($$RANDOM^TMGKERNL(0,1))=i
  quit

TestRandom2() ;
  new array
  do TestRandom(.array)
  new i
  set i=-1
  for  set i=$ORDER(array(i)) quit:i=""  do
  . write "For index ",i,", value was",array(i),!
  quit
  