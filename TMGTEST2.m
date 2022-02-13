TMGTEST2 ;TMG/kst/Scratch fns for programming tests ;03/25/06, 2/2/14
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
 ;"Scratch function for various programming tests
A       NEW Name WRITE "this is a test",!
        read "Enter name:",Name,!
        WRITE "Here is that name: ",Name,!
        QUIT

B
        NEW name
        SET name="kevin"
        read "input name",name,!
        SET ^TMG("KILL LATER")=name
        QUIT

N
        NEW n
        for n=1:1:10 do
        . WRITE n,!
        QUIT

Add1(X)
    QUIT X+1

Fn(Name)
   WRITE "That input value was: ",Name,!
   QUIT

PG
  NEW i
  NEW startTime SET startTime=$H
  WRITE !,"Lets begin...",!
  for i=0:1:100 do
  . DO PROGBAR^TMGUSRI2(i,"Progress",1,100,60,startTime)
  . hang (1)

  WRITE !,"All done!...",!
  QUIT

PB
  NEW pct
  FOR  DO  QUIT:(pct'>-1)
  .  read "enter percent: ",pct,!
  . IF pct'>-1 QUIT
  . DO PROGBAR^TMGUSRI2(pct,"Progress",0,100,60)
  . WRITE !

  QUIT


Esc
  NEW key
  FOR  DO  QUIT:(key="x")!(key=27)
  . read *key
  . IF key=27 WRITE "You escaped!"


T2
 D INIT^XPDID
 S XPDIDTOT=100
 D TITLE^XPDID("hello world")
 D UPDATE^XPDID(50)
 F I=1:1:100 D
 . DO UPDATE^XPDID(I)
 . hang (0.2)
 D EXIT^XPDID()

 QUIT


MakeFile
  NEW handle SET handle="TMGHandle"
  NEW path read "enter path: ",path,!
  NEW fname read "enter filename: ",fname,!
  WRITE "Will create a binary test file: ",path,fname,!
  NEW input
  read "Continue? (Y/N) Y// ",input,!
  IF "Yy"'[input QUIT

  SET path=$$DEFDIR^%ZISH($GET(path))
  DO OPEN^%ZISH(handle,path,fname,"W")
  IF POP QUIT
  use IO

  NEW i,j
  for i=0:1:255 do
  . for j=0:1:255 do
  . . WRITE $CHAR(j)
  . . SET $X=0

  DO CLOSE^%ZISH(handle)


  QUIT

TEST
        NEW fname,path,gref
        SET fname="triplegears.jpg"
        SET fname2="triplegears2.jpg"
        SET path="/var/local/OpenVistA_UserData/server-files/"
        SET gref="^TMP(""TMG"",""x"",1)"
        KILL ^TMP("TMG","x")

        WRITE "Reading in file: ",path,fname,!
        w $$BFTG^TMGBINF(path,fname,gref,3),!  ;"read in

        WRITE "Now let's browse the original data...",!
        DO BROWSE^TMGBVIEW(gref,3)

        WRITE "Will now encode the data...",!
        DO ENCODE^TMGRPC1(gref,3)

        WRITE "Now let's browse the encoded data...",!
        DO BROWSE^TMGBUTIL(gref,3)

        WRITE "Now let's decode the data again...",!
        DO DECODE^TMGRPC1(gref,3)

        WRITE "Now let's browse the decoded data...",!
        DO BROWSE^TMGBUTIL(gref,3)

        WRITE "will now WRITE out file to: ",path,fname2,!
        w $$GTBF^TMGBINF(gref,3,path,fname2),! ;"WRITE out

        QUIT

TESTRPC
        NEW fname,path
        SET fname="triplegears.jpg"
        SET path="/"
        NEW gref

        DO GETFILE^TMGRPC1(.gref,path,fname)
        IF $GET(@gref@(0))=0 GOTO TRPCDone
        SET gref=$name(@gref@(1))

        WRITE "Now let's browse the original (encoded) data...",!
        DO BROWSE^TMGBVIEW(gref,3)

        WRITE "Now let's decode the data again...",!
        DO DECODE^TMGRPC1(gref,3)

        WRITE "Now let's browse the decoded data...",!
        DO BROWSE^TMGBUTIL(gref,3)

TRPCDone
        WRITE "goodbye.",!

        QUIT

OR(a,b)
        NEW result SET result=0
        NEW mult SET mult=1
        FOR  DO  QUIT:(a'>0)&(b'>0)
        . SET result=result+(((a#2)!(b#2))*mult)
        . SET a=a\2,b=b\2,mult=mult*2

        QUIT result



TERMLIST(GREF)

        NEW i
        KILL ^TMP($J,"TMG-DATA")
        DO LIST^DIC(3.2)
        IF '$DATA(DIERR)  do
        . SET i=0
        . FOR  SET i=$ORDER(^TMP("DILIST",$J,2,i))  QUIT:(i="")  do
        . . SET ^TMP($J,"TMG-DATA",i)=$GET(^TMP("DILIST",$J,2,i))_"^"_$GET(^TMP("DILIST",$J,1,i))
        KILL ^TMP("DILIST",$J)
        SET GREF=$name(^TMP($J,"TMG-DATA"))
        QUIT

SIMPLE(input)
    QUIT "You said:"_input


ImageUpload

  NEW params

  SET params("NETLOCABS")="ABS^STUFFONLY"
  SET params("magDFN")="5^70685"   ;"DFN 70685 = TEST,KILLME DON'T
  SET params("OBJType")="3^1"         ;"type 1 is still image
  SET params("FileExt")="EXT^JPG"
  SET params("DateTime")="7^NOW"
  SET params("DUZ")="8^73"             ;"73 = my DUZ
  SET params("Desc")="10^A sample upload image."

  DO ADD^MAGGTIA(.results,.params)

  DO ZWRITE^TMGZWR("results")

  QUIT


FIXRX
  NEW i,OI
  SET i=""
F2
  FOR  SET i=$o(^PSDRUG(i)) DO  QUIT:(i="")
  . s i2=i
  . s i=$o(^PSDRUG(i))
  . q:i=""
  . w i2,": "
  . s name=$p($g(^PSDRUG(i2,0)),"^",1)
  . SET OI=$p($GET(^PSDRUG(i2,2)),"^",1)
  . WRITE name,"-->",OI
  . IF +OI>0 do
  . . IF $d(^PS(50.7,OI))=0 do
  . . . w " BAD LINK",!
  . . . ;"set $P(^PSDRUG(i2,2),"^",1)=""
  . . ELSE  do
  . . . WRITE " GOOD LINK",!
  . ELSE  WRITE " (no link)",!

ELHTEST
  WRITE "Hello World",!
  New address1,address2
  read "Enter street name:",address1,!
  read "Enter city/state:",address2,!
  WRITE "The address is:",!,address1,!,address2,!
  SET ^Eddie("line1")=address1
  SET ^Eddie("line2")=address2
  QUIT

ELHTEST2
  for loop=1:1:10 do
  . WRITE "Hello World",!

  QUIT

ELHTEST3
  NEW i
  SET i=1
  FOR  DO  IF i=3 QUIT
  . WRITE i,!
  . SET i=i+1


ADDPT()
        NEW TMGFDA,TMGIEN,TMGMsg

        read "Enter first name of test patient: ",FNAME,!
        IF FNAME="^" QUIT 0

        ;"Note: the "2" means file 2  (PATIENT file), and "+1" means "add entry"
        SET TMGFDA(2,"+1,",.096)="`"_DUZ          ;"field .096 = WHO ENTERED PATIENT (`DUZ=current user)
        SET TMGFDA(2,"+1,",.01)="TEST,"_FNAME    ;"field .01 = NAME
        SET TMGFDA(2,"+1,",.02)="FEMALE"          ;"field .02 = SEX
        SET TMGFDA(2,"+1,",.03)="1/1/1980"        ;"field .03 = DOB
        ;"set TMGFDA(2,"+1,",.09)="P"               ;"field .09 = SSNUM
        ;"These fields below *USED TO BE* required.  I changed the filemans status for these fields to NOT required
        SET TMGFDA(2,"+1,",1901)="NO"                           ;"field 1901 = VETERAN Y/N --For my purposes, use NO
        SET TMGFDA(2,"+1,",.301)="NO"                           ;"field .301 = "SERVICE CONNECTED?" -- required field
        SET TMGFDA(2,"+1,",391)="NON-VETERAN (OTHER)"           ;"field 391 = "TYPE" - required field

        DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMsg")

        IF $DATA(TMGMsg("DIERR")) do
        . IF $GET(TMGDEBUG)>-1 DO SHOWDIER^TMGDEBU2(.TMGMsg,.PriorErrorFound)
        . SET result=0
        . MERGE ErrArray("DIERR")=TMGMsg("DIERR")

        SET result=+$GET(TMGIEN(1))  ;"result is the added patient's IEN
        IF result'>0 GOTO ANPDone

        ;"Now, manually add a record in the file 9000001 (^AUPNPAT) with IEN (stored in result)
        ;"This is done because some PATIENT fields don't point to the PATIENT file, but instead
        ;"  point to the PATIENT/IHS file (9000001), which in turn points to the PATIENT file.
        SET ^AUPNPAT(result,0)=result
        SET ^AUPNPAT("B",result,result)=""
        IF $DATA(Entry(.09)) do
        . SET ^AUPNPAT(result,41,0)="^9000001.41P^1^1"
        . SET ^AUPNPAT(result,41,1,0)="1^"_Entry(.09)

ANPDone
         QUIT result


X
  WRITE "Hello " DO  WRITE "And Then..." DO  WRITE "Goodbye",!
  . WRITE "There "
  QUIT



TestKB
        NEW KEY,VK
        NEW i

        FOR  DO  QUIT:(VK="<ESC>")
        . S KEY=$$READ^%ZVEMKRN("",1,1)
        . S VK=VEE("K")
        . WRITE "KEY=",KEY,"   VK=",VK,!

        QUIT


P
        SET PrintArray(59610)=""
        GOTO PR3
Print
        ;"Test printing
        NEW PrintArray
        SET DIC=8925
        SET DIC(0)="MAEQ"
PR2     DO ^DIC WRITE !
        IF +Y>0 DO  GOTO PR2
        . SET PrintArray(+Y)=""
        . WRITE "Now pick another, or ^ when done picking",!
PR3
        IF $DATA(PrintArray) do
        . DO PRINT^TMGTRAN1(.PrintArray)

        QUIT


iodemo  ;; "demonstrate use of $x and wrapping
        Set file="/tmp/gtm"_$J_".tmp"
        Open file
        ;"Open file:(variable:nowrap)
        Use file
        Do io
        WRITE !!,"--------------------",!!
        Use file:(wrap:width=120:length=70)
        Use file
        Do io
        Close file
        ZSYstem "cat "_file
        ZSystem "rm "_file
        Quit
        ;
io      ;; actual IO
        For i=1:1:70 Do
        . For j=1:1:6 do
        . . Write $Justify(i,2),",",$Justify(j,2),":"
        . . WRITE " [",$Justify($x,3),",",$Justify($y,3),"] "
        . Write " EOL",!
        Quit

io2demo
        DO ^%ZIS
        use IO
        NEW i
        for i=1:1:125 do
        . WRITE i,?5,$Y,!
        DO ^%ZISC
        QUIT


i3
        DO ^%ZIS
        use IO
        NEW i
        WRITE $CHAR(27),"E"
        WRITE "Here is some text characters...",!!!
        WRITE "========================",!

        for i=32:1:128 w $CHAR(i)
        WRITE !,"========================",!
        DO ^%ZISC





JSELF1
 ;test 1 - build a temporary xref of Drug file.
  SET start=$H
  s drugRef=$$glo^view1(50)_"DrugNo)"
  s getDrug=$$GETvars^view1(50,"NtDrFlEn;PsVaPrNE(""DsgForm"");PsVaPrNE(""Strength"")")
  s DrugNo=0 f item=1:1 s DrugNo=$o(@drugRef) q:'DrugNo  do
  . s @getDrug
  . s pArray(+NtDrFlEn,+PsVaPrNE("DsgForm"),+PsVaPrNE("Strength"),DrugNo)=""
  . s pArray("BY 50",DrugNo,+NtDrFlEn,+PsVaPrNE("DsgForm"))=""
  SET end=$H
  WRITE start,!,end,!
  DO ZWRITE^TMGZWR("item")
  QUIT

JSELF2
  ;test 2 - build a temporary xref of Drug file.
  SET start=$H
  s drugRef="^PSDRUG(DrugNo)"
  s DrugNo=0 f item=1:1 s DrugNo=$o(@drugRef) q:'DrugNo  do
  . s NtDrFlEn=$$GET1^DIQ(50,DrugNo_",","20","I")
  . s PsVaPrNE("DsgForm")=$$GET1^DIQ(50,DrugNo_",","22:1","I")
  . s PsVaPrNE("Strength")=$$GET1^DIQ(50,DrugNo_",","22:2")
  . s pArray(+NtDrFlEn,+PsVaPrNE("DsgForm"),+PsVaPrNE("Strength"),DrugNo)=""
  . s pArray("BY 50",DrugNo,+NtDrFlEn,+PsVaPrNE("DsgForm"))=""
  SET end=$H
  WRITE start,!,end,!
  DO ZWRITE^TMGZWR("item")
  QUIT


Look4(IEN50)
     ;"Purpose: Look in "B" cross ref for IEN

     NEW IEN,name

     SET name=""
     FOR  SET name=$ORDER(^PSDRUG("B",name))  QUIT:(name="")  do
     . SET IEN=""
     . FOR  SET IEN=$ORDER(^PSDRUG("B",name,IEN))  QUIT:(IEN="")  do
     . . IF IEN=IEN50 do
     . . . WRITE IEN,"  ",name,!
     . . . WRITE "--",$PIECE($GET(^PSDRUG(IEN,0)),"^",1),!

     QUIT


Ensure
     ;"research

     NEW IEN SET IEN=159  ;"TEST,PERSON
     NEW TMGFDA,TMGIEN,TMGMSG
     SET TMGFDA(200.04,"?+1,"_IEN_",",.01)="BILLY"

     DO UPDATE^DIE("ES","TMGFDA","TMGIDE","TMGMSG")
     IF $$ShowIfError^TMGDBAPI(.TMGMSG) QUIT
     DO UPDATE^DIE("ES","TMGFDA","TMGIDE","TMGMSG")
     IF $$ShowIfError^TMGDBAPI(.TMGMSG) QUIT

     QUIT



READ(timeout)
        D INITKB^XGF("*") ;"turn on escape processing
        SET timeout=$GET(timeout,1)
        WRITE "Testing keyboard with timeout=",timeout," sec",!

R2      SET s=$$READ^TMGWSCR(1,3)

        IF s="^" GOTO RDone
        IF s'="" GOTO R2
        IF XGRT'="" DO  GOTO R2
        . IF XGRT'="CR" WRITE "[",XGRT,"]" QUIT
        . NEW temp SET temp=$$READ^TMGWSCR(1,timeout) ;"double clicks must occur within ~1 sec
        . IF (temp="")&(XGRT="CR") do
        . . WRITE "[","DOUBLECLICK","]"
        . ELSE  do
        . . WRITE "[CLICK]"
        . . DO UNREAD^TMGWSCR(temp,XGRT)

RDone
        DO RESETKB^XGF ;"reset keyboard(escape processing off, terminators off)

        QUIT

MathGame
     NEW n,i,st,et,tt
     NEW a,b
     NEW NCor,NWrong
     NEW NumQs SET NumQs=20
     NEW abort SET abort=0
LOOP
     SET st=$PIECE($H,",",2)
     SET NCor=0,NWrong=0
     for i=1:1:NumQs DO  QUIT:(abort=1)
     . SET a=$random(10),b=$random(10)
     . WRITE #,!!
     . WRITE "#",i," What is ",a," x ",b,"? "
     . read n,!
     . IF n="^" SET abort=1 QUIT
     . IF +n=(a*b) do
     . . WRITE "CORRECT!",!
     . . SET NCor=NCor+1
     . ELSE  do
     . . WRITE "WRONG.  It is ",a*b,!
     . . SET NWrong=NWrong+1
     . . read "Press ENTER to continue...",n,!
     SET et=$PIECE($H,",",2)
     SET tt=et-st
     WRITE "It took you ",tt," seconds to complete the game (",tt/NumQs," sec each)",!
     WRITE "You had ",NCor," correct answers and ",NWrong," wrong answers.",!
     read "Do you want to play again? (y/n)? ",n,!
     IF n="y" GOTO LOOP
     QUIT



TGT
     NEW DIC
     SET DIC=200,DIC(0)="MAEQ"
     DO ^DIC
     WRITE !,Y,!
     QUIT


DNTest
        NEW tempArray
        NEW FILE SET FILE=0
        FOR  SET FILE=$O(^DD(FILE)) QUIT:'FILE  do
        . NEW X
        . NEW field SET field=0
        . FOR  SET field=$ORDER(^DD(FILE,field)) QUIT:(+field'>0)  do
        . . IF '($D(^DD(FILE,field,0))#2) QUIT
        . . SET X=^DD(FILE,field,0)
        . . IF $P(X,U,5,99)["DINUM" do
        . . . NEW P2 SET P2=$PIECE(X,"^",2)
        . . . IF P2'["P" WRITE "!!-->",X,! QUIT
        . . . NEW targetFile
        . . . SET targetFile=+$PIECE(P2,"P",2)
        . . . IF targetFile=0 WRITE "?? --->",X,!
        . . . SET tempArray(targetFile,FILE)=""
        . . . SET tempArray("B",FILE,targetFile)=""

         ;"DO ZWRITE^TMGZWR("tempArray")

        QUIT

X12
        NEW ref
        NEW output
        SET ref="ExtraB"
        FOR  SET ref=$query(@ref) QUIT:(ref="")  do
        . NEW s1,i
        . SET s1=$qsubscript(ref,1)
        . NEW newRef SET newRef="output("""_$qs(s1,0)_""")"
        . IF $qlength(s1)>1 do
        . . for i=1:1:$qlength(s1) do
        . . . SET newRef=$name(@newRef@($qsubscript(s1,i)))
        . for i=2:1:$qlength(ref) do
        . . SET newRef=$name(@newRef@($qsubscript(ref,i)))
        . MERGE @newRef=@ref
        . ;"WRITE ref," ---- :",newRef,!

        DO ZWRITE^TMGZWR("output")

        QUIT


X13
        NEW TMGdbgLine
        DO INITKB^XGF()  ;"set up keyboard input escape code processing

        SET TMGdbgLine=$$READ^XGKB(,604800)
        ;"read TMGdbgLine,!
        WRITE "[TMGXGRT=",TMGXGRT,"]",!
        WRITE TMGdbgLine,!
        QUIT


XFR
        SET DIC=200
        SET DIC(0)="MAEQ"
        SET DIC("A")="Enter FROM person: "
        DO ^DIC WRITE !
        IF +Y'>0 QUIT
        NEW FromIEN SET FromIEN=+Y

        SET DIC("A")="Enter TO person: "
        DO ^DIC WRITE !
        IF +Y'>0 QUIT
        NEW ToIEN SET ToIEN=+Y

        NEW flags
        read "Enter mode flags (MOARX): ",flags

        DO TRNMRG^DIT(flags,200,200,FromIEN_",",ToIEN_",")

        QUIT



nums
        SET IO=$P
        DO IOCapON^TMGKERNL

        NEW i
        for i=1:1:1000 do
        . WRITE "Num #",i,!

        NEW saved
        DO IOCapOFF^TMGKERNL("saved")
        IF $DATA(saved) DO ZWRITE^TMGZWR("saved")
        DO PRESS2GO^TMGUSRI2

        QUIT



MATH(num1,num2)
        QUIT (num1+num2)**2

G(Fn,v)
        ;"Purpose: To evaluate Fn pointer
        ;"Input: Fn -- Must be NAMe of function with format as follow:
        ;"              'SomeFunctionName("abc",-4,"99",.01,var)'
        ;"              Note: the last variable may be of any name
        ;"        v -- the value to be used in place of last variable in Fn
        ;"Output: Returns curried form of Fn
        NEW S SET S=$P($P(Fn,")",1),"(",2)
        NEW L SET L=$L(S,",")
        ;"Now substitue in value for variable
        IF L>1 SET $P(S,",",L)=v
        ELSE  SET S=v
        NEW LFn SET LFn=$P(Fn,"(",1)_"("_S_")"
        NEW R SET @("R=$$"_LFn)
        QUIT R


CURRY(Fn,v)
        ;"Purpose: To create a curried form of Fn
        ;"      e.g. 'MyFunct(A,B,C,D,...)' --> 'MyFunct(99,B,C,D,...)'
        ;"Input: Fn -- Must be NAMe of function with format as follow:
        ;"              'SomeFunctionName(A,B,C,D,...)'
        ;"              Note: the first variable name may be any name
        ;"        x -- the value to be used in function
        ;"Output: Returns curried form of Fn
        NEW S SET S=$P($P(Fn,")",1),"(",2)  ;adadfsdasdf
        ;"Now substitue in value for variable
        IF $L(S,",")>1 SET $P(S,",",1)=v
        ELSE  SET S=x
        QUIT $P(Fn,"(",1)_"("_S_")"  ;"return curried form of function

GETFN()
        QUIT "MATH(X,Y)"

FNTEST
        NEW Fn SET Fn=$$GETFN()
        NEW Fn2 SET Fn2=$$CURRY(Fn,7)   ;"Fn2 SET to 'MATH(7,Y)'
        WRITE $$G(Fn2,123)  ;"Will effect MATCH(7,123)
        QUIT



CLSCHED
        WRITE !,"--- CLEAR SCHEDULE UTILITY --- CAUTION!!!",!
        NEW X,Y,DIC
        SET DIC=44
        SET DIC(0)="MAEQ"
        DO ^DIC WRITE !
        SET Y=+Y
        IF Y'>0 QUIT
        NEW % SET %=2
        WRITE "Clear out ALL AVAILABILITY slots in this location"
        DO YN^DICN WRITE !
        IF %'=1 QUIT
        NEW D SET D=0
        FOR  SET D=$ORDER(^SC(Y,"ST",D)) QUIT:(D'>0)  do
        . KILL ^SC(Y,"ST",D)
        SET D=0
        FOR  SET D=$ORDER(^SC(Y,"OST",D)) QUIT:(D'>0)  do
        . KILL ^SC(Y,"OST",D)
        SET D=0
        FOR  SET D=$ORDER(^SC(Y,"T",D)) QUIT:(D'>0)  do
        . KILL ^SC(Y,"T",D)
        NEW i
        for i=0:1:6 do
        . SET D=0
        . FOR  SET D=$ORDER(^SC(Y,"T"_i,D)) QUIT:(D'>0)  do
        . . KILL ^SC(Y,"T"_i,D)

        WRITE "done"
        QUIT



SHOWSCH
        NEW i SET i=0
        NEW L1,L2,L3 SET (L1,L2,L3)=""
        FOR  SET i=$ORDER(^SC(10,"T1",i)) QUIT:(i'>0)  do
        . NEW label SET label=$GET(^SC(10,"T1",i,1))
        . SET label=$e(label,1,7)
        . SET L1=L1_" "_$$LJ^XLFSTR(label,8)_" "
        . SET L2=L2_"+------->|"
        . SET L3=L3_$$RJ^XLFSTR(i,10)
        WRITE L1,!,L2,!,L3,!
        QUIT


TESTADD
        NEW %,TMGIEN,DOW
        SET TMGIEN=10
        SET DOW=1
        FOR  DO  QUIT:%'=1
        . DO SHOWSCH
        . SET %=1
        . WRITE "Add range" DO YN^DICN WRITE !
        . IF %'=1 QUIT
        . NEW start,end,str
        . NEW %DT SET %DT="EAF"
        . WRITE "Enter starting " DO ^%DT
        . SET start=Y
        . WRITE "   Enter ending " DO ^%DT
        . SET end=Y
        . read "   Enter string for range: ",str,!
        . DO FILTEMPL^TMGSDAVS(start,end,1,str)
        . SET %=1

        DO CLSCHED

        QUIT


ADDSCH1
        DO SHOWSCH
        NEW %
        NEW TMGIEN SET TMGIEN=10
        NEW PATRN,MODE,MSG,Date1,Date2,Y

        SET %=2
        WRITE "Clear clinic before starting" DO YN^DICN WRITE !
        IF %=-1 QUIT
        IF %=1 DO CLSCHED

        NEW %DT SET %DT="EAF"
L0      KILL PATRN
        WRITE "Enter Starting Date for template:" DO ^%DT WRITE !
        IF Y=-1 GOTO ASDone
        SET Date1=Y
        WRITE "Enter Range Ending Date ([ENTER] for 1 day only / indefinite pattern):" DO ^%DT WRITE !
        SET Date2=Y
        NEW % SET %=1
        IF Date2<1 do
        . WRITE "Use pattern indefinitely after starting date" DO YN^DICN WRITE !
        . IF %=1 SET Date2="I" QUIT
        . SET Date2=""
        IF %=-1 GOTO ASDone
        NEW TimeRange,ApptsPerSlot
        NEW Result
L1      read "Enter a time range (e.g. 0830-1145), ^ or [ENTER] IF done: ",TimeRange,!
        IF (TimeRange="^")!(TimeRange="") GOTO L2
        read "Enter Appts Per Slot: ",ApptsPerSlot,!
        IF ApptsPerSlot="^" GOTO L2
        SET PATRN(Date1_"^"_Date2,TimeRange)=ApptsPerSlot
        GOTO L1
L2      SET flags=""
        SET Result=$$SETAVAIL^TMGSDAVS(TMGIEN,.PATRN,flags,.MSG)
        IF Result=1 WRITE "Success!",!
        ELSE  do
        . WRITE "Here is message array:",!
        . DO ZWRITE^TMGZWR("MSG")

        SET %=2
        WRITE "View clinic array now" DO YN^DICN WRITE !
        IF %=-1 GOTO ASDone
        IF %=1 do
        . WRITE "Here is Clinic array now:",!
        . DO ZWRITE^TMGZWR("^SC(TMGIEN)")

        SET %=1
        WRITE "Add more patterns" DO YN^DICN WRITE !
        IF %=1 GOTO L0


ASDone
        DO CLSCHED
        QUIT

ADDSCH2
        DO SHOWSCH
        NEW TMGIEN SET TMGIEN=10
        NEW Result
        NEW PATRN,MODE,MSG,Date1,Date2,Y
        NEW %DT SET %DT=""
        NEW X
        SET X="12/15/2008" DO ^%DT SET Date1=Y
        SET PATRN(Date1,"0830-1000")=2
        SET X="12/22/2008" DO ^%DT SET Date2=Y
        SET PATRN(Date2,"0830-1000")=2
        SET flags=""
        SET Result=$$SETAVAIL^TMGSDAVS(TMGIEN,.PATRN,flags,.MSG)
        IF Result=1 WRITE "Success!"
        ELSE  do
        . WRITE "Here is message array:",!
        . DO ZWRITE^TMGZWR("MSG")

        WRITE "Here is Clinic array now:",!
        DO ZWRITE^TMGZWR("^SC(TMGIEN)")

        DO CLSCHED
        QUIT

ADDSCH3
        DO SHOWSCH
        NEW TMGIEN SET TMGIEN=10
        NEW Result
        NEW PATRN,MODE,MSG,Date1,Date2,Y
        NEW %DT SET %DT=""
        NEW X
        SET X="12/15/2008" DO ^%DT SET Date1=Y
        SET PATRN(Date1_"^I","0830-1000")=2
        SET flags=""
        SET Result=$$SETAVAIL^TMGSDAVS(TMGIEN,.PATRN,flags,.MSG)
        IF Result=1 WRITE "Success!"
        ELSE  do
        . WRITE "Here is message array:",!
        . DO ZWRITE^TMGZWR("MSG")

        WRITE "Here is Clinic array now:",!
        DO ZWRITE^TMGZWR("^SC(TMGIEN)")

        DO CLSCHED
        QUIT



xx1(var)
        WRITE var,!
        QUIT

xx2
        SET s="hello"
        DO xx1(s)
        SET s=$CHAR(9)_"hello"
        DO xx1(s)
        NEW fn SET fn="do xx1("""_s_""")"
        WRITE fn,!
        xecute fn
        QUIT

INT
        WRITE "Starting an endless cycle.  ESC to abort",!
        NEW abort SET abort=0
INT2    IF $$USRABORT^TMGUSRI2("from INT^TMGTEST") GOTO INT3
        hang 0.1
        IF $GET(TMGBRK)="??" do
        . zshow "*"
        . SET TMGBRK=""
        IF $GET(TMGBRK)'="" QUIT
        GOTO INT2
INT3    WRITE "Goodbye!",!
        QUIT


SEND(DocID)
        NEW lst,info
        ;
        SET TMGDEBUG=1
        NEW pwd
        SET pwd=" U(?Ec%U{,"
        ;"set pwd="" 3U
        SET info(1)=DocID_";1^1^1^E"
        DO SEND^ORWDX(.list,70685,73,6,pwd,.info)
        QUIT


fields
        S FILE=2,FIELD=0
        F  S FIELD=$O(^DD(FILE,FIELD)) Q:'FIELD  do
        . S NODE=$G(^(FIELD,0))
        . I NODE="" QUIT
        . S NAME=$P(NODE,U)
        . SET REQUIRED=$P(NODE,U,2)["R"
        . SET ID=''$D(^DD(FILE,0,"ID",FIELD))
        . IF REQUIRED SET FIELD("1 REQUIRED",FIELD)=NAME
        . IF ID SET FIELD("2 IDENTIFIER",FIELD)=NAME
        . IF REQUIRED&ID SET FIELD("3 REQUIRED & IDENTIFIER",FIELD)=NAME
        . ;I REQUIRED!ID S FIELD(FIELD)=NAME_U_REQUIRED_U_ID
        DO ZWRITE^TMGZWR("FIELD")
        QUIT

T1
 NEW A,B
 SET A=10,B=20
 DO ST2(.B)
 WRITE "In T1.  B=",B,!
 QUIT

ST2(V)
  NEW B
  SET B=2
  SET V="sub"
  ;"Here V is same as B from T1, but different from local B
  WRITE "In ST2.  B=",B,!
  QUIT



NATHAN
        NEW NAME
        READ "Please tell me your name?: ",NAME
        WRITE !,"Well, hello ",NAME,!
        QUIT

TSTPIPE ;
        NEW STR
        write "STARTING",!
        SET P1="TESTPIPE"
        OPEN P1:(COMMAND="wget -O - mirrors.medsphere.org/pub":NOWRAP)::"pipe"
        NEW DONE SET DONE=0
        FOR  DO  QUIT:($ZEOF)!($ZA'=0)!(+$DEVICE>0)!DONE
        . NEW $ETRAP SET $ETRAP="SET DONE=1 USE $P SET $ETRAP="""" SET $ECODE="""" "
        . ;"USE $P WRITE "$ZEOF=",$ZEOF,!
         . USE P1
        . READ STR:1
        . USE $P
        . IF STR'="" WRITE "==> ",STR,!
        . ;"WRITE "$ZA=",$ZA,!
        . ;"WRITE "$DEVICE=",$DEVICE,!
        CLOSE P1




FileSearch ;
        NEW DIR SET DIR="/opt/worldvista/EHR/r/*.m"
        NEW TAG SET TAG="PS(55,"
        NEW CMD SET CMD="grep -H """_TAG_""" "_DIR
        NEW p SET p="temp"
        open p:(COMMAND=CMD:readonly)::"pipe"
        use p
        NEW LINE
        KILL ARRAY
        NEW DATA
        NEW IDX SET IDX=1
        FOR  DO  QUIT:($zeof)
        . read LINE
        . SET DATA(IDX)=LINE
        . SET IDX=IDX+1
        close p
        use $p
        SET IDX=0
        FOR  SET IDX=$ORDER(DATA(IDX)) QUIT:(+IDX'>0)  DO
        . SET LINE=DATA(IDX)
        . new filename set filename=$piece(LINE,":",1)
        . SET LINE=$PIECE(LINE,":",2,99)
        . NEW ORIGLINE SET ORIGLINE=$$TRIM^XLFSTR(LINE)
        . for  quit:(LINE'[TAG)  do
        . . set LINE=$p(LINE,TAG,2,99)
        . . new p2 set p2=$piece(LINE,",",1)
        . . new p3 set p3=$piece(LINE,",",2)
        . . new p4 set p4=$piece(LINE,",",3)
        . . new p5 set p5=$piece(LINE,",",4) set p5=$p(p5,")",1)
        . . SET p3=$piece(p3,")",1) 
        . . ;"set p3=$tr(p3,"""","")
        . . if +p3'=5 quit
        . . ;"if ORIGLINE'["19" QUIT
        . . if p5'="0" quit
        . . new temp set temp="^"_TAG_LINE
        . . write filename,": ",temp,!
        . . SET ARRAY(IDX)=temp
        . . SET ARRAY(IDX,0)=filename
        . . set ARRAY(IDX,1)=p2
        . . set ARRAY(IDX,2)=p3
        . . set ARRAY("B",p3,filename)=ORIGLINE
        . . set ARRAY("C",p3)=""
        ; DO ZWRITE^TMGZWR("ARRAY")
        QUIT
TESTREAD
        DO OPEN^%ZISH("FILE","/tmp","IncomingFaxLog","R")
        IF POP QUIT
        USE IO
        NEW I,LINE,FILEARRAY
        F I=1:1:10000 Q:$$STATUS^%ZISH  DO
        . READ LINE
        . SET FILEARRAY(I)=LINE
        DO CLOSE^%ZISH("FILE")
        ZWR FILEARRAY
        QUIT
  
SRCH ;
  new tmg
  new ct set ct=0
  new ien set ien=0
  for  set ien=$o(^TIU(8925,ien)) quit:ien'>0  do
  . new n0 set n0=$g(^TIU(8925,ien,0))
  . new sd set sd=$p(n0,"^",7)
  . if sd<3200101 quit
  . if sd>3210701 quit
  . new n12 set n12=$g(^TIU(8925,ien,12))
  . if $p(n12,"^",2)'=168 quit
  . new ttlptr set ttlptr=$p(n0,"^",1)
  . new title set title=$p($get(^TIU(8925.1,ttlptr,0)),"^",1)
  . new found set found=0
  . if title="OFFICE VISIT" set found=1
  . if title["TELEMED" set found=1
  . if title["ACUTE" set found=1
  . if title["COMPLETE PHYSICAL" set found=1
  . if title["FOLLOWUP" set found=1
  . ;set found=1
  . if found=0 quit
  . set ct=ct+1
  . write ct,": ",ien," ",sd," ",title,!
  . set tmg(title)=$get(tmg(title))+1
  zwr tmg
  quit



WORD
  NEW ARR1,ARR2,LETTER1,LETTER2
  SET ARR1("Q")="",ARR1("E")=""
  SET ARR1("R")="",ARR1("D")=""
  SET ARR1("Z")="",ARR1("L")=""
  SET ARR1("X")="",ARR1("V")=""
  SET ARR1("M")=""
  SET ARR2("Q")="",ARR2("E")=""
  SET ARR2("R")="",ARR2("D")=""
  SET ARR2("Z")="",ARR2("L")=""
  SET ARR2("X")="",ARR2("V")=""
  SET ARR2("M")=""
  SET (LETTER1,LETTER2)=""
  FOR  SET LETTER1=$O(ARR1(LETTER1)) QUIT:LETTER1=""  DO
  . SET LETTER2=""
  . FOR  SET LETTER2=$O(ARR2(LETTER2)) QUIT:LETTER2=""  DO
  . . WRITE LETTER1,"L",LETTER2,"ER",!
  QUIT
