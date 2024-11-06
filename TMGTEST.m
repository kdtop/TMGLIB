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
 ;"=============================================================================
 ;"=============================================================================
 ;"---- show prime numbers.  From here: https://github.com/PlummersSoftwareLLC/Primes/tree/drag-race/PrimeM/solution_1
 ;"=============================================================================
 ;"=============================================================================
  ; Prime Sieve solution in M by Marisa Heit.
	;
	; This is some boilerplate to execute the different implementations
	; in the other files.
	;
	d main
	q
	;
main(size,showResults) n sieve,passes,hstart,hnow
	i $d(size)#10=0 s size=1000000
	i '$d(showResults) s showResults=""
	d runTest(size,showResults,"construct")
	q
	;
test0	d runTest(100000000,0,"construct")	q
	;
runTest(size,showResults,construct) n sieve,passes,hstart,hnow
	s hstart=$$zhToMicros()
	;"f passes=0:1 s hnow=$$zhToMicros() q:hnow-hstart>=5000000  d @construct@(.sieve,size) d @sieve("run")@(.sieve)
	f passes=0:1 do  q:hnow-hstart>=5000000
	. s hnow=$$zhToMicros() 
	. q:hnow-hstart>=5000000  
	. d @construct@(.sieve,size) 
	. d @sieve("run")@(.sieve)
	. HANG 1
	;"d:showResults'="" @sieve("print")@(.sieve,showResults,hnow-hstart,passes)
	if showResults'="" do
	. do @sieve("print")@(.sieve,showResults,hnow-hstart,passes)
	w sieve("label"),";",passes,";",hnow-hstart/1000000,";1;algorithm=",sieve("algorithm"),",faithful=",sieve("faithful")
	if $d(sieve("bits")) write ";bits=",sieve("bits")
	w !
	q
	;
zhToMicros() q $p($zh,",",2)*1000000+$p($zh,",",3)
	;
getReferenceResult(size)
	; Historical data for validating our results - the number of primes
	; to be found under some limit, such as 168 primes under 1000
	q $s(size=10:4,size=100:25,size=1000:168,size=10000:1229,size=100000:9592,size=1000000:78498,size=10000000:664579,size=100000000:5761455,size=1000000000:50847534,size=10000000000:455052511,1:0)
	;
  ;-----This is the sieve0 code ------------------------------------
    ; Prime Sieve implementation in M by Marisa Heit.
	;
	; This is the most faithful version, being the only one that uses 1 bit of storage
	; per number. This is not easy, because M does not officially have any bitwise
	; operators. That leaves this solution sub-optimal, but it's included because
	; Dave said he wanted as many faithful 1-bit solutions as he could get.
	;
	; Different vendors have different extensions that can work with bits. For
	; instance, GT.M/YottaDB have a set of $ZBIT functions that can work on bit
	; strings. Using them would tie this solution to a specific vendor. Moreover,
	; they offer little benefit in this context because they operate on bit
	; *strings* and not numbers. Setting and clearing a bit returns a new string
	; instead of operating on a number in-place.
	;
	; 19 bits per cell was found to give the best balance between space and
	; performance. (And at 60 bits and up, the routine is no longer able to
	; produce valid results.)
	;
	; Measurements running YottaDB r1.33 on an Intel(R) Core(TM) i3-10100 CPU:
	;
	; bits=1  Passes: 11, Time: 5266099, Avg: 478736.272727272727, Limit: 1000000, Count: 78498, Valid: 1
	; bits=2  Passes: 12, Time: 5023243, Avg: 418603.583333333333, Limit: 1000000, Count: 78498, Valid: 1
	; bits=3  Passes: 13, Time: 5076484, Avg: 390498.76923076923, Limit: 1000000, Count: 78498, Valid: 1
	; bits=4  Passes: 14, Time: 5237432, Avg: 374102.285714285714, Limit: 1000000, Count: 78498, Valid: 1
	; bits=5  Passes: 14, Time: 5097275, Avg: 364091.071428571428, Limit: 1000000, Count: 78498, Valid: 1
	; bits=6  Passes: 15, Time: 5317358, Avg: 354490.533333333333, Limit: 1000000, Count: 78498, Valid: 1
	; bits=7  Passes: 15, Time: 5267558, Avg: 351170.533333333333, Limit: 1000000, Count: 78498, Valid: 1
	; bits=8  Passes: 15, Time: 5197674, Avg: 346511.6, Limit: 1000000, Count: 78498, Valid: 1
	; bits=9  Passes: 15, Time: 5139923, Avg: 342661.533333333333, Limit: 1000000, Count: 78498, Valid: 1
	; bits=10 Passes: 15, Time: 5166230, Avg: 344415.333333333333, Limit: 1000000, Count: 78498, Valid: 1
	; bits=11 Passes: 15, Time: 5147186, Avg: 343145.733333333333, Limit: 1000000, Count: 78498, Valid: 1
	; bits=12 Passes: 15, Time: 5144634, Avg: 342975.6, Limit: 1000000, Count: 78498, Valid: 1
	; bits=13 Passes: 15, Time: 5138348, Avg: 342556.533333333333, Limit: 1000000, Count: 78498, Valid: 1
	; bits=14 Passes: 15, Time: 5152776, Avg: 343518.4, Limit: 1000000, Count: 78498, Valid: 1
	; bits=15 Passes: 15, Time: 5109012, Avg: 340600.8, Limit: 1000000, Count: 78498, Valid: 1
	; bits=16 Passes: 15, Time: 5157688, Avg: 343845.866666666666, Limit: 1000000, Count: 78498, Valid: 1
	; bits=17 Passes: 15, Time: 5148658, Avg: 343243.866666666666, Limit: 1000000, Count: 78498, Valid: 1
	; bits=18 Passes: 15, Time: 5166711, Avg: 344447.4, Limit: 1000000, Count: 78498, Valid: 1
	; bits=19 Passes: 15, Time: 5137400, Avg: 342493.333333333333, Limit: 1000000, Count: 78498, Valid: 1
	; bits=20 Passes: 14, Time: 5009095, Avg: 357792.5, Limit: 1000000, Count: 78498, Valid: 1
	; bits=21 Passes: 13, Time: 5030802, Avg: 386984.76923076923, Limit: 1000000, Count: 78498, Valid: 1
	; bits=22 Passes: 12, Time: 5117469, Avg: 426455.75, Limit: 1000000, Count: 78498, Valid: 1
	; bits=23 Passes: 12, Time: 5258810, Avg: 438234.166666666666, Limit: 1000000, Count: 78498, Valid: 1
	; bits=24 Passes: 12, Time: 5387491, Avg: 448957.583333333333, Limit: 1000000, Count: 78498, Valid: 1
	; bits=25 Passes: 12, Time: 5425287, Avg: 452107.25, Limit: 1000000, Count: 78498, Valid: 1
	; bits=26 Passes: 11, Time: 5062563, Avg: 460233, Limit: 1000000, Count: 78498, Valid: 1
	; bits=27 Passes: 11, Time: 5102893, Avg: 463899.363636363636, Limit: 1000000, Count: 78498, Valid: 1
	; bits=28 Passes: 11, Time: 5188296, Avg: 471663.272727272727, Limit: 1000000, Count: 78498, Valid: 1
	; bits=29 Passes: 11, Time: 5245356, Avg: 476850.545454545454, Limit: 1000000, Count: 78498, Valid: 1
	; bits=30 Passes: 11, Time: 5260019, Avg: 478183.545454545454, Limit: 1000000, Count: 78498, Valid: 1
	; bits=31 Passes: 11, Time: 5359233, Avg: 487203, Limit: 1000000, Count: 78498, Valid: 1
	; bits=32 Passes: 11, Time: 5435438, Avg: 494130.727272727272, Limit: 1000000, Count: 78498, Valid: 1
	; bits=33 Passes: 11, Time: 5435667, Avg: 494151.545454545454, Limit: 1000000, Count: 78498, Valid: 1
	; bits=34 Passes: 10, Time: 5139970, Avg: 513997, Limit: 1000000, Count: 78498, Valid: 1
	; bits=35 Passes: 10, Time: 5022957, Avg: 502295.7, Limit: 1000000, Count: 78498, Valid: 1
	; bits=36 Passes: 10, Time: 5102170, Avg: 510217, Limit: 1000000, Count: 78498, Valid: 1
	; bits=37 Passes: 10, Time: 5176807, Avg: 517680.7, Limit: 1000000, Count: 78498, Valid: 1
	; bits=38 Passes: 10, Time: 5219832, Avg: 521983.2, Limit: 1000000, Count: 78498, Valid: 1
	; bits=39 Passes: 10, Time: 5176330, Avg: 517633, Limit: 1000000, Count: 78498, Valid: 1
	; bits=40 Passes: 10, Time: 5270975, Avg: 527097.5, Limit: 1000000, Count: 78498, Valid: 1
	; bits=41 Passes: 10, Time: 5330863, Avg: 533086.3, Limit: 1000000, Count: 78498, Valid: 1
	; bits=42 Passes: 10, Time: 5285805, Avg: 528580.5, Limit: 1000000, Count: 78498, Valid: 1
	; bits=43 Passes: 10, Time: 5383242, Avg: 538324.2, Limit: 1000000, Count: 78498, Valid: 1
	; bits=44 Passes: 10, Time: 5432891, Avg: 543289.1, Limit: 1000000, Count: 78498, Valid: 1
	; bits=45 Passes: 10, Time: 5365276, Avg: 536527.6, Limit: 1000000, Count: 78498, Valid: 1
	; bits=46 Passes: 10, Time: 5515683, Avg: 551568.3, Limit: 1000000, Count: 78498, Valid: 1
	; bits=47 Passes: 10, Time: 5528455, Avg: 552845.5, Limit: 1000000, Count: 78498, Valid: 1
	; bits=48 Passes: 10, Time: 5532835, Avg: 553283.5, Limit: 1000000, Count: 78498, Valid: 1
	; bits=49 Passes: 10, Time: 5545391, Avg: 554539.1, Limit: 1000000, Count: 78498, Valid: 1
	; bits=50 Passes: 9, Time: 5000901, Avg: 555655.666666666666, Limit: 1000000, Count: 78498, Valid: 1
	; bits=51 Passes: 9, Time: 5024239, Avg: 558248.777777777777, Limit: 1000000, Count: 78498, Valid: 1
	; bits=52 Passes: 9, Time: 5115356, Avg: 568372.888888888888, Limit: 1000000, Count: 78498, Valid: 1
	; bits=53 Passes: 9, Time: 5142034, Avg: 571337.111111111111, Limit: 1000000, Count: 78498, Valid: 1
	; bits=54 Passes: 9, Time: 5144299, Avg: 571588.777777777777, Limit: 1000000, Count: 78498, Valid: 1
	; bits=55 Passes: 9, Time: 5104068, Avg: 567118.666666666666, Limit: 1000000, Count: 78498, Valid: 1
	; bits=56 Passes: 9, Time: 5226027, Avg: 580669.666666666666, Limit: 1000000, Count: 78498, Valid: 1
	; bits=57 Passes: 9, Time: 5202981, Avg: 578109, Limit: 1000000, Count: 78498, Valid: 1
	; bits=58 Passes: 9, Time: 5309129, Avg: 589903.222222222222, Limit: 1000000, Count: 78498, Valid: 1
	; bits=59 Passes: 9, Time: 5324139, Avg: 591571, Limit: 1000000, Count: 78498, Valid: 1
	; bits=60 Passes: 9, Time: 5598421, Avg: 622046.777777777777, Limit: 1000000, Count: 94764, Valid: 0
	;
	q
	;
construct(sieve,size) n index
	k sieve
	s sieve("size")=size
	;"s sieve("run")="run^sieve0"
	s sieve("run")="run"
	;"s sieve("print")="printResults^sieve0"
	s sieve("print")="printResults"
	s sieve("label")="rheit_m_bits"
	s sieve("algorithm")="base"
	s sieve("faithful")="yes"
	s sieve("bits")=1
	s size=size\2 ; Don't allocate storage for even numbers
	s size=(size+18)\19 ; Number of cells
	f index=0:1:size-1 s sieve(index)=0
	q
	;
getbit(sieve,bit) n cell
	s bit=bit\2
	s cell=bit\19
	s bit=bit#19
	q sieve(cell)\(2**bit)#2
	;
setbit(sieve,bit) n cell
	s bit=bit\2
	s cell=bit\19
	s bit=2**(bit#19)
	s:'(sieve(cell)\bit#2) sieve(cell)=sieve(cell)+bit
	q
	;
run(sieve) n factor,stop,num
	s factor=3
	s stop=sieve("size")**.5
	f  q:factor>stop  d  s factor=factor+2
	. f num=factor:2:sieve("size")-1 i '$$getbit(.sieve,num) s factor=num q
	. f num=factor*factor:factor*2:sieve("size")-1 d setbit(.sieve,num)
	q
	;
printResults(sieve,showResults,duration,passes) n num,count
	w:showResults "2, "
	s count=sieve("size")>=2
	f num=3:2:sieve("size")-1 i '$$getbit(.sieve,num) s count=count+1 w:showResults num,", "
	w:showResults !
	;
	w "Passes: ",passes,", Time: ",duration,", Avg: ",duration/passes
	;"w ", Limit: ",sieve("size"),", Count: ",count,", Valid: ",count=$$getReferenceResult^primes(sieve("size")),!
	w ", Limit: ",sieve("size"),", Count: ",count,", Valid: ",count=$$getReferenceResult(sieve("size")),!
	q
	;
 ;"=============================================================================
 ;"=============================================================================
 ;"---- END of show prime numbers.  From here: https://github.com/PlummersSoftwareLLC/Primes/tree/drag-race/PrimeM/solution_1
 ;"=============================================================================
 ;"=============================================================================

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
  . . IF TRIES > 100 SET DONE=1  ;"added for debugging
  . CLOSE STATPIPE
  . IF SHOWDBLOG CLOSE LOGPIPE
  ;
  USE TMGIO


PRIME ; Calculate prime numbers up to 10,000
    NEW N, I, J, IsPrime
    SET N=10000

    ; Initialize an array to mark non-prime numbers
    FOR I=2:1:N SET IsPrime(I)=1

    ; Sieve of Eratosthenes
    FOR I=2:1:N IF IsPrime(I) DO
    . FOR J=I*2:1:N SET IsPrime(J)=0

    ; Collect and display the prime numbers
    WRITE "Prime numbers up to ", N, ":", !
    FOR I=2:1:N IF IsPrime(I) WRITE I, " "
    WRITE !
    QUIT
  