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
 NEW i,JDX,k
 for i=1:1:10 do  write "+" 
 .for JDX=1:1:10 do  write "^"
 ..WRITE !,JDX,!                  
 ..for k=1:1:10 do  write "%"
 ...write "*"
 ...write "^"
 ...write "%"
 ...write "$"           
 QUIT
 ;   
LOOPTEST  ;
 WRITE "HI THERE",!
 NEW I
 FOR I=1:1:100 DO    
 . DO T3("HELLO")
 . NEW J FOR J=1:1:100 DO
 . . SET TMGZZ=1 ;"in global scope
 . . NEW K FOR K=1:1:100 DO
 . . . NEW L FOR L=1:1:100 DO
 . . . . NEW Y   
 . . . . SET Y=I
 . . . . WRITE Y,!
 . SET K=1  ;"in global scope
 QUIT
 ;"  
T2 ;
     WRITE "HELLO",!
T2B  NEW KT SET KT=$$T3("WORLD")
T2C  QUIT
     ;
T3(A) ;
  WRITE $GET(A),!
  QUIT 1
  ;
T4 ;
  SET A=1/0
  QUIT
  ;
TESTORN ;
  SET ORN=0
  FOR  SET ORN=$ORDER(^ORD(100.9,ORN)) QUIT:+ORN'>0  DO
  . NEW ZN SET ZN=$GET(^ORD(100.9,ORN,0))
  . WRITE ORN,":",$PIECE(ZN,"^",1)," --> ",$$ONOFF^ORB3FN(ORN),!
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
  ;
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
  ;
TestRandom(array) ;
  new i
  for i=1:1:50 do
  . set array($$RANDOM^TMGKERNL(0,1))=i
  quit
  ;
TestRandom2() ;
  new array
  do TestRandom(.array)
  new i
  set i=-1
  for  set i=$ORDER(array(i)) quit:i=""  do
  . write "For index ",i,", value was",array(i),!
  quit
  ;
CHECKRX
  NEW CT SET CT=0
  NEW IEN SET IEN=0
  FOR  SET IEN=$ORDER(^PSDRUG(IEN)) QUIT:IEN'>0  DO
  . NEW ZN SET ZN=$GET(^PSDRUG(IEN,0))
  . NEW W SET W=$GET(^PSDRUG(IEN,"WARN"))
  . NEW OLDWARN SET OLDWARN=($PIECE(ZN,"^",8)'="")
  . NEW NEWWARN SET NEWWARN=($PIECE(W,"^",1)'="")
  . IF (OLDWARN=0)&(NEWWARN=0) QUIT
  . NEW WARN SET WARN=$$DRUG^PSSWRNA(IEN,0)
  . SET CT=CT+1
  . WRITE CT," (",IEN,") ",$PIECE(ZN,"^",1)," --> ",WARN,!
  QUIT

TESTLOOP ;
    NEW ARR,IDX FOR IDX=1:1:10 SET ARR(IDX)=IDX
    FOR IDX=1:1:$$ARRMAX(.ARR,"FOR") DO
    . WRITE IDX,"--> "
    . NEW MAX SET MAX=$$ARRMAX(.ARR,"INSIDE")
    . WRITE "KILLING ARR(",MAX,")",! KILL ARR(MAX)
    QUIT    

ARRMAX(ARR,STR) ;    
    NEW MAX SET MAX=$ORDER(ARR(""),-1)
    WRITE STR,": Array max=",MAX,!
    QUIT +MAX
    ;        
AICONSOLE ;
AIL1 ;
  NEW ARR,IDX SET IDX=1
  NEW INPUT,OUT
  FOR  DO  QUIT:INPUT=""
  . READ ">",INPUT WRITE !
  . IF INPUT="" QUIT
  . SET ARR(IDX)=INPUT,IDX=IDX+1
  IF $DATA(ARR)'>0 GOTO AIDN
  DO OPENAICURL(.ARR,.OUT)
  ZWR ARR
  ;"IF $DATA(OUT) ZWR OUT
  W "---",!
  IF $DATA(OUT("choices",1,"text")) WRITE OUT("choices",1,"text"),!
  SET IDX=0
  FOR  SET IDX=$ORDER(OUT("choices",1,"text",IDX)) QUIT:IDX'>0  DO
  . WRITE $GET(OUT("choices",1,"text",IDX)),!
  GOTO AIL1
AIDN ;  
  QUIT
  ;        
OPENAI ;
  NEW ARR,OUT
  SET ARR(1)="Tell a sarcastic joke about smashing pumpkins"
  ;"SET ARR(1)="Q: What orbits the Earth, with sarcastic reply"
  DO OPENAICURL(.ARR,.OUT,1)    
  QUIT

OPENAICURL(PROMPTARR,RESULT,VERBOSE)  ;
  NEW TMGOUT,ARR,HDR,DATA,TMGERR
  SET URL="https://api.openai.com/v1/engines/text-davinci-002/completions"
  SET KEY=$GET(^TMG("TMP","OPENAI","KEY"))
  ;
  SET HDR(1)="Content-Type: application/json"
  SET HDR(2)="Authorization: Bearer "_KEY
  ;  
  SET DATA("prompt")=$$ARR2STR^TMGSTUT2(.PROMPTARR,"\n")
  SET DATA("temperature")=0.5
  SET DATA("max_tokens")=2000
  ;
  DO LINUXCURL^TMGKERNL(.TMGOUT,URL,.ARR,.HDR,.DATA)
  DO DECODE^%webjson("TMGOUT","RESULT","TMGERR")
  ;
  IF $GET(VERBOSE) ZWRITE RESULT
  QUIT
  ;
CHKERR
   DO
   . NEW $ETRAP 
   . SET $ETRAP="write ""Error Trapped: "",$ECODE,! set $ETRAP="""",$ECODE="""""
   . ZLINK "TMGTEST2"
   QUIT

CHKMATHERR   
   DO
   . NEW $ETRAP 
   . SET $ETRAP="write ""Error Trapped: "",$ECODE,! set $ETRAP="""",$ECODE="""""
   . WRITE 1/0
   QUIT
  ;
  ;
TREE ;
  NEW WIDTH SET WIDTH=0
  NEW CENTERX SET CENTERX=30
  WRITE ?CENTERX,"*",!
  NEW I FOR I=1:1:4 DO
  . DO PYRAMID(5,CENTERX,.WIDTH)
  . SET WIDTH=WIDTH-4
  FOR I=1:1:3 WRITE ?(CENTERX-1),"##",!
  QUIT
  ;
PYRAMID(ROWS,CENTERX,WIDTH) ;
  IF WIDTH'>0 SET WIDTH=3
  NEW ROW FOR ROW=1:1:ROWS DO
  . DO AROW(CENTERX,WIDTH)
  . SET WIDTH=WIDTH+2
  QUIT
  ;
AROW(CENTERX,WIDTH) ;
  NEW PAD SET PAD=CENTERX-(WIDTH\2)
  NEW I FOR I=1:1:PAD WRITE " "
  FOR I=1:1:WIDTH DO
  . IF $RANDOM(100)<7 WRITE "O" QUIT
  . WRITE "+"
  WRITE !
  QUIT
  ;
TREE2 ;
  N I,J,K,L,W,C,R S W=3,C=30 W ?C,"*",!
  F J=1:1:4 F R=1:1:5 D  F L=1:1:3 W:(J=4)&(R=5) ?(C-1),"###",!
  . F I=1:1:C-(W\2) W " " F K=1:1:W W:(I=(C-(W\2))) $S($R(100)<7:"O",1:"+")
  . W ! S W=W+2 S:R=5 W=W-6  
  Q
  ;
SLOWLOOP  ;
  NEW I 
  FOR I=1:1:9999999999 DO
  . WRITE "." 
  . HANG 1
  QUIT
  ;
UNICODEBOX  ; 
 ;"Define Unicode characters for box drawing
 SET TOPLEFTCORNER="$250C"
 SET TOPRIGHTCORNER="$2510"
 SET BOTTOMLEFTCORNER="$2514"
 SET BOTTOMRIGHTCORNER="$2518"
 SET HORIZONTALLINE="$2500"
 SET VERTICALLINE="$2502"

 ;"Define box size
 SET BOXWIDTH=20
 SET BOXHEIGHT=10
 
 WRITE !,"123456789012345678901234567890",!
 NEW IDX 
 ;"Draw top of the box
 DO UTF8WRITE^TMGSTUTL(TOPLEFTCORNER)
 FOR IDX=2:1:(BOXWIDTH-1) DO
 . DO UTF8WRITE^TMGSTUTL(HORIZONTALLINE)
 DO UTF8WRITE^TMGSTUTL(TOPRIGHTCORNER)
 WRITE !
 
 ;"Draw sides of the box
 NEW JDX
 FOR JDX=2:1:(BOXHEIGHT-1) DO
 . DO UTF8WRITE^TMGSTUTL(VERTICALLINE)
 . FOR IDX=2:1:(BOXWIDTH-1) DO
 . . WRITE " "
 . DO UTF8WRITE^TMGSTUTL(VERTICALLINE) WRITE !
 
 ;"Draw bottom of the box
 DO UTF8WRITE^TMGSTUTL(BOTTOMLEFTCORNER)
 FOR IDX=2:1:(BOXWIDTH-1) DO
 . DO UTF8WRITE^TMGSTUTL(HORIZONTALLINE)
 DO UTF8WRITE^TMGSTUTL(BOTTOMRIGHTCORNER) WRITE !
 ;
 QUIT
 ;
ALLCHARS ;
  ;"USE $P:(WIDTH=260:CHSET="UTF-8")
  WRITE !,!,"Test output of $CHAR()",!
  WRITE "NOTE: should USE $P:WIDTH=60  and draw terminal window wide",!,!
  NEW HEX SET HEX="0123456789ABCDEF"
  NEW LEFTGAP SET LEFTGAP="   "
  NEW IDX,JDX
  WRITE LEFTGAP
  FOR IDX=1:1:16 DO
  . FOR JDX=1:1:16 DO
  . . WRITE $E(HEX,IDX)
  WRITE !,LEFTGAP
  FOR IDX=1:1:16 DO
  . FOR JDX=1:1:16 DO
  . . WRITE $E(HEX,JDX)
  WRITE !,LEFTGAP
  FOR IDX=1:1:256 WRITE "-"
  WRITE !
  NEW ROW
  NEW CT SET CT=0
  FOR ROW=0:1:15 DO
  . NEW ROWHEX SET ROWHEX=$$HEXCHR2^TMGMISC(ROW,2)
  . WRITE ROWHEX," "
  . NEW COL FOR COL=0:1:255 DO
  . . NEW CH SET CH=""
  . . IF CT<32 WRITE "*" SET CT=CT+1 QUIT
  . . DO UTF8WRITE^TMGSTUTL(CT)
  . . SET CT=CT+1
  . WRITE !
  QUIT
  ;
CHARDEMO ;
  USE $P:(CHSET="UTF-8")
  NEW IDX FOR IDX=9472:1:9580 DO
  . WRITE IDX,": " 
  . DO UTF8WRITE^TMGSTUTL(IDX) 
  . W !
  QUIT
  ;
TESTMONA ;
  NEW TEMP DO GETMONA(.TEMP)
  DO ALTBUF^TMGTERM(1)  ;"switch to alternative buffer
  NEW IDX SET IDX=""
  FOR  SET IDX=$ORDER(TEMP(IDX)) QUIT:IDX'>0  DO
  . WRITE $GET(TEMP(IDX)),!
  DO PRESS2GO^TMGUSRI2  
  DO ALTBUF^TMGTERM(0)  ;"switch back to normal buffer
  QUIT
  ;  
GETMONA(OUT)
  NEW DONE SET DONE=0
  NEW IDX FOR IDX=1:1 DO  QUIT:DONE
  . NEW LINE SET LINE=$$TRIM^XLFSTR($TEXT(MONAREF+IDX))
  . IF LINE="" SET DONE=1 QUIT
  . SET LINE=$EXTRACT(LINE,4,$L(LINE))
  . IF LINE["<DONE>" SET DONE=1 QUIT
  . SET OUT(IDX)=LINE    
  QUIT 
  ;
MONAREF
  ;;"x++x+++++++xxxxx+++++xxx+++x++xx++++x+++++++++++xxxxxx++++++xxxx+xx+xxxxxxxx+++++++++++x++xx+xxxxxxx
  ;;"+++++++xxx+++x+xx+xx+++++++x++x++++++++++++xxxX$$$$$$$$X$Xxxxxxx+xxxxxxxx+++x+x++++++++++xxxxx+xxxxx
  ;;"+++++++++++x+x++++++++++++++++xx++++++xX$$$$$$$$$$$$X$$$$$$$$$$$Xxxxxxx+++xx+++++++++++xx+xxxxxxxxxx
  ;;"++x++++++x+++++++++x+x+++++++++++xxX$$$X$XXX$$$$$$$$$$$$$$$$$$$$$$$&$xx+xxx++++++++++++++xxxx+xxx+++
  ;;"+++++++++++++++++++++++++++++++xx$$$$$XXXXXXXXX$$$$$$$$$$$&$&$$$$$$$&&&Xxxx+x+x++x++++++xx+++x++x+xx
  ;;"+++++++++++++++++++++++++++++xXX$XXXXXXXxXXXXXX$$$$X$$$$$$$$$&&&&$$$$$&&&&xx++++++x+++++++++++++xx+x
  ;;"++++++++++++++++++++++++++++xXXXXXXXXXXxxXXXXXXXXXX$$$$$$$$$$&&$&&&&&$&&&&&&xx++++++++++++++++x++xxx
  ;;"++++++++++++++++++++++++++xXXxXXXXXXXXXxXXXXXXXXXXX$$$$$$$$$$$&&&&&&&&&&&&&&&$x++++++++++++++++x++++
  ;;"+++++++++++++++++++++++++x$$XXXXXXXxx+++++++xxxxXXXX$$$$$$$$$$&&$$&&&&&&&&&&&&&x++++++++++x+x+++++++
  ;;"++++++++++++++++++++++++XXXXXxx++++;;;;+;;+++++++++xxXX$$$$$$$$&$&&&&&&&&&&&&&&&$+++++++++++++++++++
  ;;"+++++++++++;++++++++++xxX$Xxx+++;++;;;;;;;;;;;+++++xxxXXXXX$$$$$&&&&&&&&&&&&&&&&&$++++++++++++++++++
  ;;"++++++++++++;+++++++++xX$Xxx+;;;;;;:::;;::;;;;;;;;+++xxxXXXX$$$$$$&&&&&&&$&&&&&&&&$x++++++++++++++++
  ;;"++++++;++++++++++++++X$$Xx+++;;;:;;;;:;;;::;:;;;;;+++++xxXXX$$$$$$$&&&&&&&&&&&&&&&&$++++++++++++++++
  ;;"+;;;;++++;+++;++++++x$$$Xx++;;;::;:;;;:;:;;::;;;;;;;+++xxxXXX$$$$$$&&&&&&&&&&&&&&&&&$+++++++++++++++
  ;;"++;;+++++++;+++++++$$$$Xxx++;;;:::::;;;;;:::;;:;;;;;+++xxxXXXX$$$$$$$$&&&&&&&&&&&&&&&x+++++++x++++++
  ;;"++++++;+++++;+++++xx$$$Xxx++;;;;:;;;;;;;;:;;;;;;;;;++++xxxxXXXX$$$$$$&&&&&&&&&&&&&&&&&+++xxx++++++++
  ;;"+++;;+++++;;;;;+xxxX$$$Xx+++;;;;;;;;;;;;;;;;;;;;;+;+++++xxxxxXXXX$$$$&&&&&&&&&&&&&&&&&Xxxxxx++++++xx
  ;;"+;;;++++;;;;;;+xxX$$$$$Xxx++;+;;;;;;;;;;;;;;;;;;;;+++++++++xxxxxXX$$&&&&&&&&&&&&&&&&&&&Xxxxxxxxxxxxx
  ;;";;;+;;;+;;;;+;xxx$$X$$$xx+++;;;;;;;;:;;;;;;;;;;;;;;+;;++++++++xxxX$$$&&&&&&&&&&&&&&&&&&XxxxXxxxxxxxx
  ;;";;;;;;;;;;;;;;xxX$$$$$$xx++++;;;;;;;:;;;;;;;;;;;;;;;;;++++++++xxxXX$$$&&&&&&&&&&&&&&&&&$XXXXXXXXxxxX
  ;;";;;;;;;;;;;;;+xxX$$$$$$x++;;;;;+;;;;;;;;;;;;;++++++++++++++++xxxxX$$$$$&&&&&&&&&$&&&&&&$XXXX$xxxxxxx
  ;;"+;;;;;;;;;;;;xxxX$$&$$$x++;++++++++;;;;;;;++xxxxxxxxxxxxxxxxxxXXXXX$$&$&&&&&&&&&&&&&&&&&XXXXXxXxxxxx
  ;;"+;;;+;;;;;;++xXXX$$$$$$XxxxxxxxXxxx+;;;+;+xXXXXXxXXXXXXXXXXXXXXXXXX$$$$&$&&&&&&&&&&&&&&&XXXXXXXXXXXx
  ;;"+;;;;;;;+++++XxXX$$$$$$XXXXx++xx++xx+;;;+xxXXXX++xxXXXxXX$$$$$XXXXXX$$&&&&&&&&&&&&&&&&&&$XXXXXXXXXxX
  ;;";;;;;+;+;++++xX$$$$$$$$XXXXXx+X$$XXxx+;;+xXxXxxx+;xX$XXXXXXXxx++xxXX$$&$$&&&&&&&&&&&&&&&$XXX$XXXXXXX
  ;;"+;++xx+++++++x$$$$$$$$$x++x+;;+++++++;;;+xxxx++;;;;+++++xx++++++xxXX$$&&&$&&&&&&&&&&&&&$&XXXXXXXXXXX
  ;;"++xxxxXxx++++XX$$$$$$$$++++++xxXxx+++;;;+xxxx+++++++++xx+++;;;++xxXX$$&&&&&&&&&&&&&&&&&&&XXX$XXXXXXX
  ;;"xxxXxxXXXX+xXXX$$$$$&&$++;;;;;;+;;;;;;;;++xxx+;;;;;;;;;;;;;;++++xXX$$&&&&&&&&&&&&&&&&$&&&XXXXXXXXXXX
  ;;"XxXXxxXXXXxXxXxX$$$&&$$++;;;;;;;;;;;;;;;++++++;;;;:;;;;;;;++++xxxXX$$&&&&&&&&&&&&&&&&&$&&$XXXXXXXXXX
  ;;"XXXXxXXXXXxXXXxX$$$$$&$x+++;;;;;;;;;;;:;++++++;;;;;;:;;;;;++++xxXX$$$&&&&&&$&&&&&$&&&$&&&&XXXXXXXXXX
  ;;"XXXXXXX$XXX$XXX$$$$$$&$X++;;;;;;;;;;;;;;+++++++;;;;;;;;;;+++xxxXX$$$&&&&&&&$$&&&&$$&&&&&&&XxxxxxxXxx
  ;;"$XXXXXXX$$$$$$$$$&$$&$&Xx+;;;;;;;;+;;;;;+++xx+;+;;;;;;;+++++xxxXX$$$&&&&&$$$&$&$&&&&&&&&$&XX$XXX$$Xx
  ;;"XXXXX$$$XX$$$$$$$$$$$&&$x++;;;;;;++;;;;;++++xx;;;;;;;;++++xxxXXXX$$$&&&&&&&&$&&&&&$&&&$&$&$$$X$XXX$X
  ;;"XXXXXXXXX$$$$$$$&$$$&$&$Xxx++;;+;+x+;;++xXxXXX+;;;;;+++++xxxxXXXX$$$&$&$&$&$$&&&&&&&$&&&&&&&&$$$$$XX
  ;;"XXXXX$X$X$$$$X$$$$$&&$&&$xx+++++;;;+xxXX$XX$$X+;;;++++++xxxxXXXX$$$$&&&&&&&$&$&&$&&&&&&&&$&$$$$$$X$X
  ;;"XXXXXXXXXX$$$$$$$&&&$$$$$Xxxx+++;;;;;+XXXXXxxx++++++++++xxxxxXXXX$$$$&$$&$&$&$&&&&&&$&&$&$$$$$XXXX$X
  ;;"XXXX$X$$X$$$$$$$$$$$$$$&$Xxxx+++++;;;++xxxxxxxxxxxxx+++xxxxxXXXX$$$$&&&&&&&&$&&&$&&&&&&$&$$$$$X$$$$$
  ;;"XX$$$XXXX$X$$$$$$$$$$$$&&$Xx+++xxXxxx++xxxxXXXXXXx++++++xxxxXXXX$$$$&&&&$&&&$&&&&&&&&&&$&$$$$$X$$XXX
  ;;"XXX$X$$XXX$XX$$$$$$$$$$$&&Xxx+++++;;;;;+++xxxxx+++++++++xxxXXXXX$$$$&&&$&&&$&&$&$$&&&&&$&$$&&X$XX$XX
  ;;"XXXX$XXXXXX$$$$$$$$$&$$$&&&$xx++++;;+++xxxxxxxxx++++++xxxxXXXXX$$$$$$$&&&$$&&$$&&&&&&&&&&$$$$$XXX$XX
  ;;"XXXXXXXXXXX$X$$$$$$$&&&$&&&&$Xxx+++++++xxxxxxxxxxxx+xxxxXXXXXXX$$$$$$$&&&$&$$&&&&&&&&&&$&&$$XXXXXXXX
  ;;"$XXX$XXXXXX$$X$$$$&$&&$&$$$&&$$Xx+++;;;;;;;+++xxxxxxxxxXXXXX$$$$$$&$&&&$&&&&&&&&&&&&&&&$&&&$$$XXXXXX
  ;;"XXxxXxxxxXX$X$$$$$$&$$$$$$$&$&&$$X++;;;;;;;;+++xxxxxxXXXX$XXX$$$$&&$&$&$&&$&&&$$&&&&&&&&&&$$$$XXXXXX
  ;;"$XXXXXXXXXXX$$$$$$$$$$$$$$$&$$$&$$$x+++;;;++++xxxXXXXXX$$$X$$$$$$&&&$&&&&&&&$$&&&&&&&$&&&&$$$$XXXXXX
  ;;"XXXXXxxxxxxx$X$$$$$$$&$$$$$&$$&$$$&$Xxx+xxxxxxXXXXXX$X$X$$$$$$$&$$&$&&&&&&&&&&&&$&&&&&&&&&&$&$XX$XXX
  ;;"XXXxxxxxxxxx$$$$$$$$$$&&$$$&&&$&&&&$$$$$X$XXXXXXXXXXX$$$$$$$$$$$$$$$$$$&$&&&&&$&&&&&&&&$$&&$&$$X$XXX
  ;;"XXXXXxxxxxxxX$$$$&$$&$&$$&&$&&&&$$&&&$$&$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$&&&&&&&$&&&&&&&&&&&&&$$$$$$X
  ;;"xxxxxxxxxxxxxXX$$&$$$$&$&&&&&$&&$$&&&$&$$XXXXX$$$$$$$$$$$$$$$$$$$$X$$$$$&$&&&&&&&&&&&&&&&&&&$&$&$$XX
  ;;"xxx+xxxxxXxxXX$$$&$$$$$&$&$&&&&&$$$$$$$$$xxxxXXXXXX$$$$$$XXXX$XxXXXX$X$$$&&&$$&&&&&&&&&&&&&$$$$XXXXX
  ;;"xxXXXxxxxXxxxX$X&$&&$$$&$$$&&&&$$$$$$$$$$XxxxxxxXXXXX$XXXXXXXXxXXxXXXXX$$&$&&&&&&&&&&&$$&&&&&$$X$XXX
  ;;"xxxxxxxxxxxxXxXX$$&$$&$$$$$$$$$$&&$$$$$$$$x+++xxxxxxXXXXXXXXXXxxxxxXXXX$$$$$&&&&&&&&&&$&&&&&&&&$$$XX
  ;;"XXxxxXXXxxxXXXXX&$$$$$&$$$$$$$&$$$&$$$$$$$x++++++xxxxxxxxxxxXxxxxxxxxxXX$$$$$&&&&&&&&&$$&&&$&&$&$$$X
  ;;"xxxxXxXxXxxX$xxX&$$$&&&$$$&&$$$$$$&$$$$$$$Xx++++++xxxxxxxxxxxxx+x++xxxxXX$$$$$$&&&$$&$$$$&&&&&&$&&$$
  ;;"XXXXXXX$$$XX$$X$&$&$&$&&&&&&$&$&$$$$$$XXxxxx+++++++++x+xx+++++x+x++++xxxXX$$$$&$$$$$$$$$$$&&&&&$&$&&
  ;;"XX$$$$X$$$$$$$$$&$$&&&$&&$&$$$$$$$$$$Xxx++++++++++++++++++++++++++++xxxxXX$$$$$$$$$$$$$$$$&&$&&$&&&$
  ;;"$XX$$$X$$$$$&$$$&$$&&$&&$$&&$$$$$$Xxxx+++++;++;++++++;++++++;;;;++;+++++xXX$$$&$$$$X$$$$$&&&$$&$&&&$
  ;;"$$$$$$$X$$$$$$&&&&$&$$$$$$$$$$$XXxxxx++;+;;;;;;;;;;;+;;;;;;;;;;;;;;;++x+xxXXX$$$$$X$$$X$$&&$$$&$&&&&
  ;;"<DONE>                                                                                                        
  ;
TESTPIPE  
  WRITE "THIS JOB IS: ",$J,!
  NEW TMGIO SET TMGIO=$IO
  NEW STATFNAME SET STATFNAME="/tmp/status.txt"
  NEW MSGREF SET MSGREF=$NAME(^TMP($J,"BATCHMEDS"))
  WRITE !,"KILL PRIOR ITEMS @",MSGREF DO YN^DICN WRITE !,!
  IF %=1 KILL @MSGREF
  
  NEW SHELL SET SHELL="ShellHandle"
  OPEN SHELL:(SHELL="/bin/sh":COMMAND="/bin/sh":stderr="errIO")::"pipe" 
  USE SHELL
  NEW CMDSTR SET CMDSTR="tail -f "_STATFNAME
  DO ADDLOG(MSGREF,"About to send out '"_CMDSTR_"'  $J="_$J)
  WRITE CMDSTR,!  ;"<-- tail should now start monitoring file. 
  
  NEW JOBSTATUS,LOCALVARS
  NEW LINE,DONE SET DONE=0
  FOR  DO  QUIT:DONE
  . ;"DO ADDLOG(MSGREF,"MSGREF="_MSGREF)
  . ;"DO ADDLOG(MSGREF,"looping: "_$H)
  . READ LINE:1
  . NEW TIMEOUT SET TIMEOUT=($TEST=0)
  . ;"SET LINE="TEST"
  . IF 'TIMEOUT DO ADDLOG(MSGREF,"LINE='"_LINE_"'")
  . IF 'TIMEOUT USE $P WRITE "LINE=",LINE,! USE SHELL
  . ;"LOCK +@MSGREF@("RESULT"):2
  . SET JOBSTATUS=$GET(@MSGREF@("RESULT"))
  . ;"LOCK -@MSGREF@("RESULT")
  . DO ADDLOG(MSGREF,"JOBSTATUS="_JOBSTATUS)
  . IF +JOBSTATUS=0 QUIT  ;"still '0^WORKING'
  . DO ADDLOG(MSGREF,"TRYING TO QUIT")
  . ;"quit Tail command
  . DO ADDLOG(MSGREF,"About to issue $CHAR(3)")
  . WRITE $CHAR(3) ;" ^C  or end-of-tex.  Should cause tail to quit
  . DO ADDLOG(MSGREF,"SETTING DONE=1")
  . SET DONE=1
  DO ADDLOG(MSGREF,"CLOSING SHELL")
  CLOSE SHELL
  WRITE "BYE",!
  ;
  QUIT
  ;
TP2  
  NEW TMGIO SET TMGIO=$IO
  NEW STATFNAME SET STATFNAME="/tmp/status.txt"
  
  NEW SHELL SET SHELL="ShellHandle"
  OPEN SHELL:(SHELL="/bin/sh":COMMAND="/bin/sh":stderr="errIO")::"pipe" 
  USE SHELL
  NEW CMDSTR SET CMDSTR="tail -f "_STATFNAME
  WRITE CMDSTR,!  ;"<-- tail should now start monitoring file. 
  
  NEW JOBSTATUS,LOCALVARS
  NEW LINE,DONE SET DONE=0
  FOR  DO  QUIT:DONE
  . IF '$ZEOF READ LINE
  . ;"SET LINE="TEST"
  . USE $P WRITE "LINE=",LINE,! USE SHELL
  . SET DONE=$ZEOF
  . IF 'DONE HANG 1 QUIT 
  . WRITE $CHAR(3) ;" ^C  or end-of-tex.  Should cause tail to quit
  CLOSE SHELL
  WRITE "BYE",!
  ;
  QUIT
  ;
ADDLOG(REF,STR) ;
  NEW IDX SET IDX=$ORDER(@REF@("LOG",""),-1)+1
  SET @REF@("LOG",IDX)=STR
  QUIT;
  ;
CTRLPIPE ;
  WRITE !,"ENTER $J FOR OTHER JOB: " READ J WRITE !
  NEW MSGREF SET MSGREF=$NAME(^TMP(J,"BATCHMEDS"))
CP1 ;
  WRITE "SHOW LOG" DO YN^DICN WRITE !
  IF %=1 DO  GOTO CP1
  . ZWR @MSGREF@(*)
  ;"LOCK +@MSGREF@("RESULT"):2  
  SET @MSGREF@("RESULT")="1^DONE"
  ;"LOCK -@MSGREF@("RESULT")  
  QUIT
 ;
TESTPIPE2 ;
  ;"TESTING VARS
  NEW TMGIO SET TMGIO=$IO
  NEW RUNLOGFNAME SET RUNLOGFNAME="/opt/worldvista/EHR/nodejs/veradigm/debug_run_log.txt" 
  NEW STATFNAME SET STATFNAME="/opt/worldvista/EHR/nodejs/veradigm/status.txt"
  NEW SHOWDBLOG SET SHOWDBLOG=1
  NEW STATFN,DBGLOGFN SET (STATFN,DBGLOGFN)=""
  NEW MSGREF SET MSGREF="NULL"
  NEW TRIES SET TRIES=0
  ;
  ;"ORIGINAL CODE ---------
  DO
  . ;"Setup TAIL of status file to monitor progress
  . NEW STATPIPE SET STATPIPE="STATPIPEHandle"
  . OPEN STATPIPE:(SHELL="/bin/sh":COMMAND="/bin/sh":stderr="errIO")::"pipe"   ;"NOTE: errIO stream is ignored
  . USE STATPIPE WRITE "tail -f "_STATFNAME USE TMGIO ;"//send command to PIPE  <-- tail should now start monitoring file.
  . NEW LOGPIPE SET LOGPIPE="LOGPIPEHandle"
  . ;"Optionally setup TAIL of debug_run_log file to monitor progress
  . IF SHOWDBLOG DO
  . . OPEN LOGPIPE:(SHELL="/bin/sh":COMMAND="/bin/sh":stderr="err2IO")::"pipe" ;"NOTE: err2IO stream is ignored
  . . USE LOGPIPE WRITE "tail -f "_RUNLOGFNAME  USE TMGIO ;"//send command to PIPE  <-- tail should now start monitoring file.  
  . NEW LINE,GOODREAD,DONE SET DONE=0
  . FOR  DO  QUIT:DONE
  . . SET TRIES=TRIES+1  ;"added for debugging
  . . USE STATPIPE READ LINE:0 SET GOODREAD=$TEST USE TMGIO
  . . IF GOODREAD DO
  . . . IF STATFN]"" DO  QUIT         
  . . . . XECUTE "DO "_STATFN_"(LINE)"    ;"<-- SEND LINE TO CALLBACK FUNCTION IF PROVIDED 
  . . . USE TMGIO
  . . . IF SHOWDBLOG WRITE "STATUS: "   ;"<--something to differentiate between STATUS and RUN_LOG 
  . . . WRITE LINE,!  ;"DEFAULT TO CONSOLE OUTPUT.  
  . . ELSE  WRITE "_"  ;"added for debugging
  . . IF SHOWDBLOG DO
  . . . USE LOGPIPE READ LINE:0 SET GOODREAD=$TEST USE TMGIO
  . . . IF GOODREAD DO
  . . . . IF DBGLOGFN]"" DO  QUIT    
  . . . . . XECUTE "DO "_DBGLOGFN_"(LINE)"  ;"<-- SEND LINE TO CALLBACK FUNCTION IF PROVIDED  
  . . . . USE TMGIO
  . . . . WRITE "RUN_LOG: ",LINE,!  ;"DEFAULT TO CONSOLE OUTPUT.  
  . . . ELSE  WRITE "."  ;"added for debugging
  . . NEW JOBSTATUS SET JOBSTATUS=$GET(@MSGREF@("RESULT"))
  . . IF +JOBSTATUS=0 QUIT  ;"still '0^WORKING'  <-- REPEAT LOOP
  . . USE STATPIPE WRITE $CHAR(3) USE TMGIO             ;" ^C  or end-of-text (EOT).  Should cause tail to quit
  . . IF SHOWDBLOG USE LOGPIPE WRITE $CHAR(3) USE TMGIO ;" ^C  or end-of-text (EOT).  Should cause tail to quit
  . . SET DONE=1
  . . IF TRIES>100 SET DONE=1  ;"added for debugging
  . CLOSE STATPIPE
  . IF SHOWDBLOG CLOSE LOGPIPE
  ;
  USE TMGIO
  ;
  ;"-----------------
  ;
PRIME ; Calculate prime numbers up to 10,000
    NEW N,I,J,K,IsPrime
    SET N=10000
    ;
    ; Initialize an array to mark non-prime numbers
    FOR I=2:1:N SET IsPrime(I)=1
    ;
    ; Sieve of Eratosthenes
    FOR I=2:1:N IF IsPrime(I) DO
    . FOR J=2:1:N SET K=I*J QUIT:K>N  DO
    . . SET IsPrime(K)=0

    ; Collect and display the prime numbers
    WRITE "Prime numbers up to ",N,":",!
    FOR I=2:1:N IF IsPrime(I) WRITE I," "
    WRITE !
    QUIT
 ;
TERM ;
  USE $P:(NOCANONICAL) 
  WRITE "[" 
  HANG 5 
  WRITE "].. Enter key:" 
  READ *X:0.1 
  W !,X,!
  QUIT
  ;
ACCEPT(TO)      ;Read A/V and echo '*' char. (p702 Modified to accept IAM STS token)
        ;Have the Read write to flush the buffer on some systems
        N A,B,C,E K DUOUT S A="",TO=$G(TO,60),E=0
        W "//kt 1",!
        F  D  Q:E
        . w "starting loop. About to READ *C:TO  TO=",TO," sec  //kt",!
        . R "",*C:TO S:('$T) DUOUT=1 S:('$T)!(C=94) A="^"
        . w "past READ *C  //kt",!
        . I (A="^")!(C=13)!($L(A)>60) S E=1 Q
        . I C=127 Q:'$L(A)  S A=$E(A,1,$L(A)-1) W $C(8,32,8) Q
        . write "GOT: C=",C,"  //kt",!
        . S A=A_$C(C) W *42
        . write "Ready to start loop again",!
        . Q
        . w "done with loop  //kt",!
        Q A
        ;
  ;"========================================================================
TEST1  ;
   NEW T,H,E,Y,A,R,V,S,M
   NEW VAR
   FOR T=0:1:9 DO
   . SET VAR("T")=T
   . SET VAR("USED",T)=1
   . FOR H=0:1:9 DO
   . . SET VAR("H")=H
   . . IF $GET(VAR("USED",H))=1 QUIT
   . . SET VAR("USED",H)=1
   . . FOR E=0:1:9 DO
   . . . SET VAR("E")=E
   . . . IF $GET(VAR("USED",E))=1 QUIT
   . . . SET VAR("USED",E)=1
   . . . FOR Y=0:1:9 DO
   . . . . SET VAR("Y")=Y
   . . . . IF $GET(VAR("USED",Y))=1 QUIT
   . . . . SET VAR("USED",Y)=1
   . . . . FOR A=0:1:9 DO
   . . . . . SET VAR("A")=A
   . . . . . IF $GET(VAR("USED",A))=1 QUIT
   . . . . . SET VAR("USED",A)=1
   . . . . . FOR R=0:1:9 DO
   . . . . . . SET VAR("R")=R
   . . . . . . IF $GET(VAR("USED",R))=1 QUIT
   . . . . . . SET VAR("USED",R)=1
   . . . . . . FOR V=0:1:9 DO
   . . . . . . . SET VAR("V")=V
   . . . . . . . IF $GET(VAR("USED",V))=1 QUIT
   . . . . . . . SET VAR("USED",V)=1
   . . . . . . . FOR S=0:1:9 DO
   . . . . . . . . SET VAR("S")=S
   . . . . . . . . IF $GET(VAR("USED",S))=1 QUIT
   . . . . . . . . SET VAR("USED",S)=1
   . . . . . . . . FOR M=0:1:9 DO
   . . . . . . . . . SET VAR("M")=M
   . . . . . . . . . IF $GET(VAR("USED",M))=1 QUIT
   . . . . . . . . . SET VAR("USED",M)=1
   . . . . . . . . . IF $$TESTCOMBO(.VAR)=1 DO
   . . . . . . . . . . NEW V2 MERGE V2=VAR KILL V2("USED")
   . . . . . . . . . . ;"ZWR V2(*)
   . . . . . . . . . . NEW IDX,STR SET (IDX,STR)="" 
   . . . . . . . . . . FOR  SET IDX=$ORDER(V2(IDX)) QUIT:IDX=""  DO
   . . . . . . . . . . . IF STR'="" SET STR=STR_", "
   . . . . . . . . . . . SET STR=STR_IDX_"="_$GET(V2(IDX))
   . . . . . . . . . . WRITE STR,!
   . . . . . . . . . . ;"WRITE "---------------",!
   . . . . . . . . . SET VAR("USED",M)=0
   . . . . . . . . SET VAR("USED",S)=0
   . . . . . . . SET VAR("USED",V)=0
   . . . . . . SET VAR("USED",R)=0
   . . . . . SET VAR("USED",A)=0
   . . . . SET VAR("USED",Y)=0
   . . . SET VAR("USED",E)=0
   . . SET VAR("USED",H)=0
   . SET VAR("USED",T)=0
   QUIT
   ;
TESTCOMBO(V)  ;
    ;"  T H E Y
    ;"    A R E
    ;"  V E R Y
    ;"---------
    ;"S M A R T
    ;
    NEW RESULT SET RESULT=0
    NEW COL,COLIDX,TEMP,CARRY
CL1 SET TEMP=V("Y")+V("E")+V("Y")
    SET CARRY=TEMP\10 SET TEMP=TEMP#10
    IF TEMP'=V("T") GOTO TESTDN
CL2 SET TEMP=CARRY+V("E")+V("R")+V("R")   
    SET CARRY=TEMP\10 SET TEMP=TEMP#10
    IF TEMP'=V("R") GOTO TESTDN
CL3 SET TEMP=CARRY+V("H")+V("A")+V("E")    
    SET CARRY=TEMP\10 SET TEMP=TEMP#10
    IF TEMP'=V("A") GOTO TESTDN
CL4 SET TEMP=CARRY+V("T")+V("V")    
    SET CARRY=TEMP\10 SET TEMP=TEMP#10
    IF TEMP'=V("M") GOTO TESTDN
CL5 SET TEMP=CARRY  
    IF TEMP'=V("S") GOTO TESTDN
    SET RESULT=1
TESTDN ;
   QUIT RESULT
   
   
Show(solution,expression) ;
 new arr,ch,digit
 for j=1:1:$length(expression) do
 . set ch=$extract(expression,j),digit=$extract(solution,j)
 . if ch'?1a quit
 . set arr(ch)=digit
 set ch="" for  set ch=$order(arr(ch)) quit:ch=""  write ch,"=",$get(arr(ch)),", "
 write !
 quit
 ;
PuzzleCall(digits) ;
 New try Set try=$Translate(expression,callletter,digits)
 If @try do
 . Set solution(try)=$Get(solution(try))+1
 . do Show(try,expression)
 ;"else  write "FAIL: ",try,!
 Quit
 ;
Permut(in,lead) ;
 New ii,letter,next
 SET lead=$get(lead)
 If in="" Do  Quit
 . Quit:lead=""
 . do PuzzleCall(lead) quit
 . Set Permut(lead)=$Get(Permut(lead))+1
 . Quit
 For ii=1:1:$Length(in) Do
 . Set letter=$Extract(in,ii)
 . set next=in
 . set $Extract(next,ii)=""
 . Do Permut(next,lead_letter)
 . Quit
 Quit
 ; 
Puzzle(expression)  ;" E.g.: Do Puzzle("SEND+MORE=MONEY") 
 New callletter,ii,letter,solution,ch set callletter=""
 For ii=1:1:$Length(expression) do
 . set ch=$extract(expression,ii) 
 . if ch?1a,callletter'[ch set callletter=callletter_ch
 Do Permut(1234567890,"")
 Quit
 
 

