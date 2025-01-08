TMGGI ;TMG/kst/ GT.M GI ;7/14/15
         ;;1.0;TMG-LIB;**1**;7/14/15 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                ;
;    Copyright 1991, 2006 Fidelity Information Services, Inc    ;
;                                ;
;    This source code contains the intellectual property    ;
;    of its copyright holder(s), and is made available    ;
;    under a license.  If you do not know the terms of    ;
;    the license, please stop and do not read further.    ;
;                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;
 ;//kt Modified for file input dialog.  Additional version: 7/14/15
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Changes Copyright (c) 7/14/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;
%GI0(OPTION) ;"TMG modified version of ^%GI
    ;"//kt NOTE 6/19/24 -- The version of the code below failed to read a .GBLs that had
    ;"                    a line > 3000 chars long.  The ^%GI provided with yottadb
    ;"                  did not have the same problem.  So I took my modifications
    ;"                and added to new version of ^%GI, and can be found as %GI() below
    ;"
    ;"INPUT: OPTION -- pass by ARRAY.  Optional.
    ;"           OPTION("FPNAME")=<FULL PATH AND FILENAME> -- specify file to load
    ;"--------------------------------------------------------
    ;"service@greystone.com %GO;19920722 21:35;global input
    ;"Load globals into database
    ;"Possible enhancements:
    ;"selection and/or exclusion by key list, range and/or wildcard
    ;"optional confirmation by global name
    ;"callable entry point
    ;"--------------------------------------------------------
    ;
    WRITE !,"Global Input Utility",!
    DO PRESS2GO^TMGUSRI2
    IF '$DATA(%zdebug) NEW $et SET $et="zg "_$zl_":ERR^TMGGI" USE $P:(ctrap=$c(3):exc="zg "_$zl_":EXIT^TMGGI")
    NEW D,NUMGBLS,NUMNODES,SAVED,X,Y,%ZD,ZFORMAT,CTLCHARS
    NEW FSIZE SET FSIZE=0
    SET CTLCHARS="" FOR D=1:1:31,127 SET CTLCHARS=CTLCHARS_$CHAR(D)
    FOR  DO  QUIT:$LENGTH(%ZD)
    . ;"READ !,"Input device: <terminal>: ",%ZD,!
    . SET %ZD=$GET(OPTION("FPNAME"))  ;"//kt
    . IF %ZD="" SET %ZD=$$GETFNAME^TMGIOUTL("Pick input file.")                 ;//kt
    . ;"IF '$LENGTH(%ZD) SET %ZD=$P QUIT
    . IF '$LENGTH(%ZD) SET %ZD="^"
    . IF %ZD="^" QUIT
    . NEW % SET %=1 WRITE "Ensure input file is in Unix format" DO YN^DICN WRITE !
    . IF %=-1 SET %ZD="^" QUIT
    . IF %=1 IF $$Dos2Unix^TMGKERNL(%ZD)
    . IF %ZD="?" DO  QUIT
    . . WRITE !!,"Select the device you want for input"
    . . WRITE !,"If you wish to exit enter a caret (^)",!
    . . SET %ZD=""
    . IF $zparse(%ZD)="" WRITE "  no such device" SET %ZD="" QUIT
    . SET FSIZE=$$FileSize^TMGKERNL(%ZD)
    . WRITE "Opening file ",%ZD," (",FSIZE," bytes) ..."
    . OPEN %ZD:(readonly:block=2048:record=2044:exception="GOTO EX0"):0
    . WRITE !
    . IF '$T  WRITE !,%ZD," is not available" SET %ZD="" QUIT
    . QUIT
EX0 . WRITE !,$P($ZS,",",2,999),! 
    . CLOSE %ZD SET %ZD=""
    QUIT:%ZD="^"
    WRITE !!
    SET SAVED="",(NUMGBLS,NUMNODES)=0
    NEW LINENUM 
    USE %ZD:exception="GOTO EOF^TMGGI"
    READ X,Y SET LINENUM=2 
    USE $P 
    WRITE !,X,!,Y,!!
    ;"READ !,"OK <Yes>? ",X,!!
    ;"IF $LENGTH(X),$EXTRACT("NO",1,$LENGTH(X))=$TRANSLATE(X,"no","NO") GOTO EXIT
    NEW % SET %=1 WRITE "OK to start load" DO YN^DICN WRITE !
    IF %'=1 GOTO EXIT
    SET ZFORMAT=Y["ZWR"
    NEW BYTES SET BYTES=0
    NEW STARTH SET STARTH=$H
    NEW LASTH SET LASTH=$H
    NEW DONE SET DONE=0
    FOR  DO  QUIT:DONE
    . IF (ZFORMAT) DO
    . . USE %ZD READ X USE $P  ;"//kt added use $P 6/19/24
    . . SET LINENUM=LINENUM+1  
    . . IF X="" SET DONE=1 QUIT
    . . SET BYTES=BYTES+$LENGTH(X)
    . . SET @X
    . . SET NUMNODES=NUMNODES+1
    . . SET X=$PIECE($PIECE(X,"="),"(")  
    . . IF X'=SAVED,X'="^" DO
    . . . SET NUMGBLS=NUMGBLS+1,SAVED=X
    . . . USE $P 
    . . . ;"WRITE:$X>70 ! 
    . . . ;"WRITE X,?$X\10+1*10
    . . . WRITE !,X,!
    . IF ('ZFORMAT) DO
    . . USE %ZD READ X,Y USE $P  ;"//kt added use $P 6/19/24  9834   /opt/worldvista/EHR/kids/Remote_Patches-dirFor-LEX/LEX_2_147.GBLs
    . . SET LINENUM=LINENUM+2 
    . . SET BYTES=BYTES+$LENGTH(X)+$LENGTH(Y)
    . . IF "*"[$EXTRACT(X) SET DONE=1 QUIT 
    . . IF $TRANSLATE(X,CTLCHARS,"")'=X DO        ;"convert control chars to $C(X) exprs
    . . . NEW CHAR,CHARPOS,NEWX SET NEWX=""
    . . . FOR CHARPOS=1:1:$LENGTH(X) DO
    . . . . SET CHAR=$EXTRACT(X,CHARPOS)
    . . . . SET NEWX=NEWX_$SELECT(CTLCHARS[CHAR:"""_$C("_$ASCII(CHAR)_")_""",1:CHAR)
    . . . SET X=NEWX    ;"use fixed 'X'
    . . SET @X=Y
    . . SET NUMNODES=NUMNODES+1,X=$PIECE(X,"(")
    . . IF X'=SAVED,X'="^" DO
    . . . SET NUMGBLS=NUMGBLS+1,SAVED=X
    . . . USE $P 
    . . . ;"WRITE:$X>70 ! 
    . . . ;"WRITE X,?$X\10+1*10
    . . . WRITE !,X,!
    . IF $$HDIFF^XLFDT($H,LASTH,2)>5 DO  ;"Update progress Q 5 seconds
    . . SET LASTH=$H
    . . USE $P DO PROGBAR^TMGUSRI2(BYTES,"Progress",1,FSIZE,70,STARTH)
EOF USE $P
    WRITE !!,"Restored ",NUMNODES," node",$s(NUMNODES=1:"",1:"s")
    WRITE " in ",NUMGBLS," global",$s(NUMGBLS=1:".",1:"s."),!
    CLOSE:%ZD'=$P %ZD USE $P:(ctrap="":exc="")
    QUIT
    ;
ERR USE $P 
     WRITE !,"ERROR ENCOUNTERED",!
     WRITE "$ZSTATUS=[",$PIECE($zs,",",2,99),"]",!
     WRITE "$ECODE=[",$ec,"]",!
    ; Warning - Fall-though
    SET $ec=""
EXIT ;
    IF $DATA(%ZD),%ZD'=$P CLOSE %ZD
    USE $P:(ctrap="":exc="")
    QUIT
  ;
%GI(OPTION) ;"TMG modified version of ^%GI
    ;"INPUT: OPTION -- pass by ARRAY.  Optional.
    ;"           OPTION("FPNAME")=<FULL PATH AND FILENAME> -- specify file to load
    ;";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;";                                ;
    ;"; Copyright (c) 1991-2018 Fidelity National Information        ;
    ;"; Services, Inc. and/or its subsidiaries. All rights reserved.    ;
    ;";                                ;
    ;";    This source code contains the intellectual property    ;
    ;";    of its copyright holder(s), and is made available    ;
    ;";    under a license.  If you do not know the terms of    ;
    ;";    the license, please stop and do not read further.    ;
    ;";                                ;
    ;";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;"  MODIFIED BY KST 6/19/24
    ;
    ;Load globals into database
    ;Possible enhancements:
    ;selection and/or exclusion by key list, range and/or wildcard
    ;optional confirmation by global name
    ;callable entry point
    ;
    NEW tmgDEVSav,tmgDEVInfo
    DO DEV2ARR^TMGKERN1($IO,.tmgDEVSav,,.tmgDEVInfo)
    NEW FSIZE SET FSIZE=0
    NEW BYTES SET BYTES=0
    NEW STARTH SET STARTH=$H
    NEW LASTH SET LASTH=$H
    NEW LINENUM 
    ;
    new c,chset,d,dos,fmt,g,i,n,q,sav,saveread,x,y,%ZD
    set d("io")=$io
    use $principal
    write !,"Global Input Utility",!
    set $zstatus=""
    if '$data(%zdebug) new $etrap set $etrap="zgoto "_$zlevel_":err^"_$text(+0) do
    . zshow "d":d                                    ; save original $p settings
    . set x=$piece($piece(d("D",1),"CTRA=",2)," ")
    . set:""=x x=""""""
    . set d("use")="$principal:(ctrap="_x_":exception=",x=$piece(d("D",1),"EXCE=",2),x=$zwrite($extract(x,2,$length(x)-1))
    . set:""=x x=""""""
    . set d("use")=d("use")_x_":"_$select($find(d("D",1),"NOCENE"):"nocenable",1:"cenable")_")"
    . use $principal:(ctrap=$char(3,4):exception="":nocenable)
    for  do  quit:$length(%ZD)
    . ;"read !,"Input device: <terminal>: ",%ZD
    . SET %ZD=$GET(OPTION("FPNAME")) 
    . IF %ZD="" SET %ZD=$$GETFNAME^TMGIOUTL("Pick input file.")                
    . IF %ZD="^" QUIT
    . NEW % SET %=1 WRITE "Ensure input file is in Unix format" DO YN^DICN WRITE !
    . IF %=-1 SET %ZD="^" QUIT
    . IF %=1 IF $$Dos2Unix^TMGKERNL(%ZD)
    . IF %ZD="?" DO  QUIT
    . . WRITE !!,"Select the device you want for input"
    . . WRITE !,"If you wish to exit enter a caret (^)",!
    . . SET %ZD=""
    . if '$length(%ZD) set %ZD=$principal quit
    . quit:"^"=%ZD
    . if "?"=%ZD do  quit
     . . write !!,"Select the device you want for input"
     . . write !,"If you wish to exit enter a caret (^)",!
     . . set %ZD=""
    . if $zparse(%ZD)="" write "  no such device" set %ZD="" quit
    . SET FSIZE=$$FileSize^TMGKERNL(%ZD)
    . open:$principal'=%ZD %ZD:(readonly:rewind:stream:recordsize=2044:ichset="M":exception="goto EX"):0
    . if '$test  write !,%ZD," is not available" set %ZD=""
    . quit
EX  . write !,$piece($ZS,",",2,999),!
    . close %ZD
    . set ($zstatus,%ZD)=""
    ;"//kt original --> if "^"=%ZD do err quit
    if "^"=%ZD goto err
    write !!
    set sav="",(g,n)=0
    if $principal'=%ZD use %ZD:(exception="zgoto "_$zlevel_":eof":ctrap=$C(3,4)) do  quit:"^"=x
    . read x,y
    . use $principal
    . set dos=($zchar(13)=$extract(x,$length(x)))                    ; the label selects dos/not for entire file
    . set:dos x=$extract(x,1,$length(x)-1),y=$extract(y,1,$length(y)-1)
    . write !,x,!,y,!!
    . set chset=$select(x["UTF-8":"UTF-8",1:"M")
    . if $zchset'=chset write "Extract CHSET ",chset," doesn't match current $ZCHSET ",$zchset,!
    . ;"read !,"OK <Yes>? ",x,!!
    . ;"if "^"=x do err quit
    . ;"if $length(x),$extract("NO",1,$length(x))=$translate(x,"no","NO") set x="^" do err quit
    . NEW % SET %=1 WRITE "OK to start load" DO YN^DICN WRITE !
    . IF %'=1 DO err SET x="^" QUIT        
    . set fmt=$get(y)["ZWR"
    else  do  if "^"=x do err quit                            ; if input is $p, no label
    . set chset=$zchset,dos=0,fmt=1
    . read !,"Format <ZWR>? ",x,!!
    . quit:"^"=x
    . if $length(x) set x=$translate($zconvert(x,"U"),"L") if $extract("GO",1,$length(x))=x set fmt=0 quit
    set x=$$read
    SET BYTES=BYTES+$LENGTH(x)
    if 'fmt do
    . if x?1"^"1(1"%",1A).30AN.1(1"("1.E1")")1"=".E set fmt=1 quit            ; looks like ZWR
    . set y=$$read          
    . SET BYTES=BYTES+$LENGTH(y)
    . do  for  set x=$$read,y=$$read if "*"'[$extract(x) do            ; GLO
    . . SET BYTES=BYTES+$LENGTH(x)
    . . SET BYTES=BYTES+$LENGTH(y)
    . . DO CHECKPROG
    . . quit:(""=x)&(""=y)
    . . set @x=y
    . . set n=n+1,x=$piece(x,"(")
    . . if x'=sav,x'="^" do
    . . . set g=g+1,sav=x
    . . . if $principal'=%ZD use $principal write:$x>70 ! write x,?$x\10+1*10
    if (fmt) do  for  set x=$$read do                        ; ZWR
    . quit:""=x
    . SET BYTES=BYTES+$LENGTH(x)
    . DO CHECKPROG
    . set (i,q)=1,y=""                                ; find first equal-sign not in quotes
    . for  set c=i,i=$find(x,"=",i) do:$extract(x,c,i-2)[""""  quit:q
    . . for c=c:1:i-2 set:""""=$extract(c) q='q
    . set y=$extract(x,1,i-2)
    . if 8193>$zlength(x) set @x
    . else  set:8192<$length(y) y=$zwrite(y,1) set @y=$zwrite($extract(x,i,$length(x)),1)
    . set n=n+1,x=$piece(y,"(")
    . if x'=sav,"^"'=x do
    . . set g=g+1,sav=x
    . . if $principal'=%ZD use $principal write:$x>70 ! write x,?$x\10+1*10
eof ;
err set $ecode="" 
    if $data(%ZD),%ZD'=$principal close %ZD
    use:$data(d("use")) @d("use")
    use:$data(d("io")) d("io")
    if ""'=$zstatus,($zstatus'["CTRAP")&($zstatus'["IOEOF") write !,"ERROR: ",$zstatus
    quit:'$get(n)
    write !!,"Restored ",n," node",$select(n=1:"",1:"s")
    write " in ",g," global",$select(g=1:".",1:"s.")
    DO RESTORDEV^TMGKERN1(.tmgDEVSav,.tmgDEVInfo)  
    quit
read()    ; concatenate reads that fill the buffer; also centralize the USE and dos <LF> stripping
    new i,e,x
    use %ZD
    set e=0,x=$get(saveread),i=1+(""'=x),saveread=""
    for i=i:1 do  quit:e
    . if $principal=$io read "> ",x(i),!
    . else  read x(i)
    . if $zeof,$increment(e) quit
    . if ""=x(i),$increment(e) quit
    . if 0=i#2,$increment(e) set saveread=x(i) quit
    . set x=x_x(i)
    . if 2044'=$zlength(x(i)),$increment(e) quit
    set:dos x=$extract(x,1,$length(x)-1)
    quit x
    ;
CHECKPROG ;" Uses LASTH,BYTES,FSIZE,STARTH, %ZD in global scope
    IF $$HDIFF^XLFDT($H,LASTH,2)>2 DO  ;"Update progress Q 5 seconds
    . SET LASTH=$H
    . USE $P DO PROGBAR^TMGUSRI2(BYTES,"Progress",1,FSIZE,70,STARTH) USE %ZD
    QUIT
    ;
