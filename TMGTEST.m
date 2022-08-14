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
 