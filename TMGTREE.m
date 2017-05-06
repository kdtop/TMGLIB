TMGTREE ;TMG/kst/Text tree user interface ;03/25/06, 2/2/14
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
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"BrowseBy(CompArray,ByTag)
 ;"ShowBy(CompArray,ByTag,aOpen,bOpen,cOpen,dSelected)

 ;"=======================================================================
 ;" Private Functions.
 ;"=======================================================================



BrowseBy(CompArray,ByTag)
        ;"Purpose: Allow a user to interact with dynamic text tree
        ;"              that will open and close nodes.
        ;"Input:        CompArray -- array to browse.  Should be in this format
        ;"                      CompArray("opening tag",a,b,c,d)
        ;"               ByTag -- the name to use in for "opening tag")
        ;"Results: returns Batch/job number, or 0 IF none selected

        NEW aOpen SET aOpen=0
        NEW bOpen SET bOpen=0
        NEW cOpen SET cOpen=0
        NEW dSelected SET dSelected=0

        NEW done SET done=0

        NEW input
        NEW result SET result=0

        FOR  DO  QUIT:(done=1)
        . SET result=$$ShowBy(.CompArray,ByTag,aOpen,bOpen,cOpen,dSelected)
        . IF result>0 SET done=1 QUIT
        . read !,"Enter Number to Browse ([Enter] to backup, ^ to Quit): ",input:$GET(DTIME,3600),!
        . IF input="" SET input=0
        . IF +input>0 do
        . . IF aOpen=0 do
        . . . SET aOpen=input,bOpen=0,cOpen=0
        . . ELSE  IF bOpen=0 do
        . . . SET bOpen=input,cOpen=0
        . . ELSE  IF cOpen=0 SET cOpen=input
        . . ELSE  SET dSelected=input
        . ELSE  IF input=0 do
        . . IF cOpen'=0 SET cOpen=0,dSelected=0 QUIT
        . . IF bOpen'=0 SET bOpen=0 QUIT
        . . IF aOpen'=0 SET aOpen=0 QUIT
        . . IF aOpen=0 SET input="^"
        . IF input="^" SET done=1

      QUIT result


ShowBy(CompArray,ByTag,aOpen,bOpen,cOpen,dSelected)
        ;"Purpose: Draw current state of text tree
        ;"Input:        CompArray -- array to browse.  Should be in this format
        ;"                      CompArray("opening tag",a,b,c,d)
        ;"               ByTag -- the name to use in for "opening tag")
        ;"Result: IF aOpen,bOpen, and cOpen,dSelected are valid values, then
        ;"              will return value from CompArray, i.e.
        ;"              CompArray("opening tag",a,b,c,d)="x"  <--- will return "x"
        ;"          otherwise returns 0

        NEW a,b,c,d
        NEW acount SET acount=0
        NEW bcount SET bcount=0
        NEW ccount SET ccount=0
        NEW dcount SET dcount=0
        NEW result SET result=0

        WRITE #,!

        SET a=$ORDER(CompArray(ByTag,""))
        IF a'="" FOR  DO  QUIT:(a="")
        . SET acount=acount+1
        . NEW nexta SET nexta=$ORDER(CompArray(ByTag,a))
        . NEW Aindent
        . IF (aOpen=0) do
        . . IF acount<10 WRITE "0"
        . . WRITE acount,". "
        . ELSE  WRITE "... "
        . WRITE a,!
        . SET b=$ORDER(CompArray(ByTag,a,""))
        . IF (aOpen=acount)&(b'="") FOR  DO  QUIT:(b="")
        . . SET bcount=bcount+1
        . . NEW nextb SET nextb=$ORDER(CompArray(ByTag,a,b))
        . . NEW Bindent
        . . WRITE "    +--"
        . . IF (bOpen=0) do
        . . . IF bcount<10 WRITE "0"
        . . . WRITE bcount,". "
        . . ELSE  WRITE "... "
        . . WRITE b,!
        . . IF nextb'="" SET Aindent="    |  "
        . . ELSE  SET Aindent="       "
        . . SET c=$ORDER(CompArray(ByTag,a,b,""))
        . . IF (bOpen=bcount)&(c'="") FOR  DO  QUIT:(c="")
        . . . SET ccount=ccount+1
        . . . NEW nextc SET nextc=$ORDER(CompArray(ByTag,a,b,c))
        . . . IF nextc'="" SET Bindent="    |  "
        . . . ELSE  SET Bindent="       "
        . . . WRITE Aindent,"    +--"
        . . . IF (cOpen=0) do
        . . . . IF ccount<10 WRITE "0"
        . . . . WRITE ccount,". "
        . . . ELSE  WRITE "... "
        . . . WRITE c,!
        . . . SET d=$ORDER(CompArray(ByTag,a,b,c,""))
        . . . IF (cOpen=ccount)&(d'="") FOR  DO  QUIT:(d="")
        . . . . SET dcount=dcount+1
        . . . . WRITE Aindent,Bindent,"    +-- "
        . . . . IF dcount<10 WRITE "0"
        . . . . WRITE dcount,". "
        . . . . WRITE d,!
        . . . . IF dcount=dSelected SET result=$GET(CompArray(ByTag,a,b,c,d))
        . . . . SET d=$ORDER(CompArray(ByTag,a,b,c,d))
        . . . SET c=nextc
        . . SET b=nextb
        . SET a=nexta

SBDone
        QUIT result


