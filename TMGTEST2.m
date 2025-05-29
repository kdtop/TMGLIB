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
        IF $GET(@gref@(0))=0 quit    ;"what was this-> ;"GOTO TRPCDone
        SET gref=$name(@gref@(1))

        WRITE "Now let's browse the original (encoded) data...",!
        DO BROWSE^TMGBVIEW(gref,3)

        WRITE "Now let's decode the data again...",!
        DO DECODE^TMGRPC1(gref,3)

        WRITE "Now let's browse the decoded data...",!
        DO BROWSE^TMGBUTIL(gref,3)

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
  ;"
FIZZBUZZ
  NEW I
  FOR I=1:1:100 DO
  . NEW LINE SET LINE=""
  . IF I#3=0 SET LINE="Fizz"
  . IF I#5=0 SET LINE=LINE_"Buzz"
  . IF LINE="" SET LINE=I
  . WRITE LINE,!
  QUIT
  ;
CHKERR2     
  ;"SET X=
  QUIT 
  ;"
NOTETOC(TMGRESULT,TMGIN)  ;"TEMP ENTRY POINT
  ZL "TMGRPC1H"
  DO NOTETOC^TMGRPC1H(.TMGRESULT,.TMGIN)
  QUIT
  ;"
MAMORDER(TMGDFN)  ;"
  ;"
  NEW THISRESULT SET THISRESULT=""
  NEW HFARRAY,HFIEN SET HFIEN=0
  SET HFARRAY(730)="Mammogram ordered: "
  FOR  SET HFIEN=$O(HFARRAY(HFIEN)) QUIT:HFIEN'>0  DO        
  . NEW DATE SET DATE=9999999
  . NEW DONE SET DONE=0
  . FOR  SET DATE=$ORDER(^AUPNVHF("AA",TMGDFN,HFIEN,DATE),-1) QUIT:(DATE'>0)!(DONE=1)  DO        
  . . NEW FMDATE SET FMDATE=9999999-DATE
  . . SET DONE=1   ;"Only add 1
  . . IF THISRESULT'="" SET THISRESULT=THISRESULT_","
  . . NEW THISHFIEN SET THISHFIEN=+$O(^AUPNVHF("AA",TMGDFN,HFIEN,DATE,0))
  . . NEW COMMENT SET COMMENT=$G(^AUPNVHF(THISHFIEN,811))
  . . SET THISRESULT=THISRESULT_$G(HFARRAY(HFIEN))_$$EXTDATE^TMGDATE(FMDATE,1)_" """_COMMENT_""""_" [HF]"
  QUIT THISRESULT
  ;"
COMPFLDS ;
  NEW FILE SET FILE=0
  FOR  SET FILE=$ORDER(^DD(FILE)) QUIT:FILE'>0  DO
  . NEW FLD SET FLD=0
  . FOR  SET FLD=$ORDER(^DD(FILE,FLD)) QUIT:FLD'>0  DO
  . . NEW ZN SET ZN=$GET(^DD(FILE,FLD,0)) QUIT:ZN=""
  . . NEW TYPE SET TYPE=$PIECE(ZN,"^",2) QUIT:TYPE'["C"
  . . NEW LABEL SET LABEL=$PIECE(ZN,"^",1)
  . . WRITE "FILE #",FILE,", FLD #",FLD,", LABEL=",LABEL,", TYPE=",TYPE,!
  QUIT
  ;    
NODETEST(STR,ARG2) ;
  NEW RESULT SET RESULT="KEVIN"_"+"_$GET(STR)_"+"_$GET(ARG2)
  SET ARG2("TEST")="NEW VALUE"
  QUIT RESULT
  ;