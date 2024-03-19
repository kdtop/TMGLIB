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
  . WRITE CT," (",IEN,") ",$P(ZN,"^",1)," --> ",WARN,!
  QUIT

  
ArrayDump(ZZARRAYP,TMGIDX,INDENT)
        ;"NOTE: Similar to ARRDUMP^TMGMISC3
        ;"PUBLIC FUNCTION
        ;"Purpose: to get a custom version of GTM's "zwr" command
        ;"Input: Uses global scope var tmgDbgIndent (if defined)
        ;"        ZZARRAYP: NAME of global to display, i.e. "^VA(200)"
        ;"        TMGIDX: initial index (i.e. 5 IF wanting to start with ^VA(200,5)
        ;"        INDENT: spacing from left margin to begin with. (A number.  Each count is 2 spaces)
        ;"          OPTIONAL: indent may be an array, with information about columns
        ;"                to skip.  For example:
        ;"                INDENT=3, INDENT(2)=0 --> show | for columns 1 & 3, but NOT 2
        ;"Result: 0=OK to continue, 1=user aborted display
        ;
        NEW RESULT SET RESULT=0
        NEW $ETRAP SET $ETRAP="SET RESULT="""",$ETRAP="""",$ecode="""""
        ;
AD1     IF $DATA(ZZARRAYP)=0 GOTO ADDN
        NEW ABORT SET ABORT=0
        IF (ZZARRAYP["@") DO  GOTO:(ABORT=1) ADDN
        . NEW ZZTEMP SET ZZTEMP=$PIECE($EXTRACT(ZZARRAYP,2,99),"@",1)
        . IF $DATA(ZZTEMP)#10=0 SET ABORT=1
        ;"Note: I need to do some validation to ensure ZZARRAYP doesn't have any null nodes.
        DO
        . NEW X SET X="SET ZBTEMP=$GET("_ZZARRAYP_")"
        . SET X=$$UP(X)
        . DO ^DIM ;"a method to ensure ZZARRAYP doesn't have an invalid reference.
        . IF $GET(X)="" SET ABORT=1
        IF ABORT GOTO ADDN
        ;
        SET tmgDbgIndent=$GET(tmgDbgIndent,0)
        ;
        NEW TMGIDEDEBUG SET TMGIDEDEBUG=1  ;"Force this function to output, even IF TMGIDEDEBUG is not defined.
        NEW TMGIDX SET TMGIDX=$GET(TMGIDX)
        SET INDENT=$GET(INDENT,0)
        ;
        DO DEBUGINDENT(tmgDbgIndent)
        ;
        IF INDENT>0 DO
        . FOR TMGIDX=1:1:INDENT-1 DO
        . . NEW STR SET STR=""
        . . IF $GET(INDENT(TMGIDX),-1)=0 SET STR="  "
        . . ELSE  SET STR="| "
        . . DO DEBUGWRITE(tmgDbgIndent,STR)
        . DO DEBUGWRITE(tmgDbgIndent,"}~")
        ;
        IF TMGIDX'="" DO
        . IF $DATA(@ZZARRAYP@(TMGIDX))#10=1 DO
        . . NEW STR SET STR=@ZZARRAYP@(TMGIDX)
        . . IF STR="" SET STR=""""""
        . . IF $LENGTH(STR)'=$LENGTH($$TRIM^XLFSTR(STR)) set STR=""""_STR_"""" 
        . . NEW QT SET QT=""
        . . IF +TMGIDX'=TMGIDX SET QT=""""
        . . DO DEBUGWRITE(tmgDbgIndent,QT_TMGIDX_QT_" = "_STR,1)
        . ELSE  DO
        . . DO DEBUGWRITE(tmgDbgIndent,TMGIDX,1)
        . SET ZZARRAYP=$NAME(@ZZARRAYP@(TMGIDX))
        ELSE  DO
        . DO DEBUGWRITE(tmgDbgIndent,ZZARRAYP,0)
        . IF $DATA(@ZZARRAYP)#10=1 DO
        . . DO DEBUGWRITE(0,"="_$GET(@ZZARRAYP),0)
        . DO DEBUGWRITE(0,"",1)
        ;
        SET TMGIDX=$ORDER(@ZZARRAYP@(""))
        IF TMGIDX="" GOTO ADDN
        SET INDENT=INDENT+1
        ;
        FOR  DO  QUIT:TMGIDX=""  IF RESULT=1 GOTO ADDN
        . NEW TEMPIDX SET TEMPIDX=$ORDER(@ZZARRAYP@(TMGIDX))
        . IF TEMPIDX="" SET INDENT(INDENT)=0
        . NEW TEMPINDENT MERGE TEMPINDENT=INDENT
        . SET RESULT=$$ArrayDump(ZZARRAYP,TMGIDX,.TEMPINDENT)  ;"Call self recursively
        . SET TMGIDX=$ORDER(@ZZARRAYP@(TMGIDX))
        ;
        ;"Put in a blank space at end of subbranch
        DO DEBUGINDENT(tmgDbgIndent)
        ;
        IF 1=0,INDENT>0 DO
        . FOR TMGIDX=1:1:INDENT-1 DO
        . . NEW STR SET STR=""
        . . IF $GET(INDENT(TMGIDX),-1)=0 SET STR="  "
        . . ELSE  SET STR="| "
        . . DO DEBUGWRITE(tmgDbgIndent,STR)
        . DO DEBUGWRITE(tmgDbgIndent," ",1)
        ;
ADDN    QUIT RESULT
        ;
DEBUGWRITE(tmgDbgIndent,STR,AddNewline)
        ;"NOTE: Duplicate of function in TMGIDEDEBUG
        ;"PUBLIC FUNCTION
        ;"Purpose: to WRITE debug output.  Having the proc separate will allow
        ;"        easier dump to file etc.
        ;"Input:tmgDbgIndent, the amount of indentation expected for output.
        ;"        STR -- the text to write
        ;"      AddNewline -- boolean, 1 IF ! (i.e. newline) should be written after s

        ;"Relevant DEBUG values
        ;"        cdbNone - no debug (0)
        ;"        cdbToScrn - Debug output to screen (1)
        ;"        cdbToFile - Debug output to file (2)
        ;"        cdbToTail - Debug output to X tail dialog box. (3)
        ;"Note: If above values are not defined, then functionality will be ignored.

        SET TMGIDEDEBUG=$GET(TMGIDEDEBUG,0)
        IF TMGIDEDEBUG=0 QUIT
        IF (TMGIDEDEBUG=2)!(TMGIDEDEBUG=3),$DATA(DebugFile) use DebugFile
        WRITE STR
        IF $GET(AddNewline)=1 DO
        . NEW ENDSPACE SET ENDSPACE=20
        . IF +$GET(IOM)>0,(IOM-$X)<20 SET ENDSPACE=IOM-$X
        . NEW IDX FOR IDX=1:1:ENDSPACE WRITE " "        
        . WRITE !
        IF (TMGIDEDEBUG=2)!(TMGIDEDEBUG=3) use $PRINCIPAL
        QUIT


DEBUGINDENT(tmgDbgIndent,Forced)
        ;"NOTE: Duplicate of function in TMGIDEDEBUG
        ;"PUBLIC FUNCTION
        ;"Purpose: to provide a unified indentation for debug messages
        ;"Input: tmgDbgIndent = number of indentations
        ;"       Forced = 1 IF to indent regardless of DEBUG mode

        SET Forced=$GET(Forced,0)

        IF ($GET(TMGIDEDEBUG,0)=0)&(Forced=0) QUIT
        NEW i
        FOR i=1:1:tmgDbgIndent DO
        . IF Forced DO DEBUGWRITE(tmgDbgIndent,"  ")
        . ELSE  DO DEBUGWRITE(tmgDbgIndent,". ")
        QUIT
        
UP(X)   ;
        ;"Taken from UP^XLFSTR
        QUIT $TRANSLATE(X,"abcdefghijklmnopqrstuvwxyz","ABCDEFGHIJKLMNOPQRSTUVWXYZ")

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
