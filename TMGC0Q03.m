TMGC0Q03 ;TMG/kst/TMG customization of C0Q code ;10/24/12, 3/24/21
         ;;1.0;TMG-LIB;**1**;8/8/12
 ;
 ;"TMG C0Q FUNCTIONS
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
 ;"BRWSELST -- Ask for C0Q PATIENT LIST, and display members of list.
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;
BRWSELST ;
        ;"Purpose: ask for C0Q PATIENT LIST, and display members of list.
        NEW DIC,X,Y
        NEW MENU,USRPICK
        NEW RESULT
        SET MENU(0)="Pick how to display C0Q PATIENT LIST"
        SET MENU(1)="Browse patients in scroller"_$CHAR(9)_"SEL"
        SET MENU(2)="Dump entire record"_$CHAR(9)_"DUMP"
BRW1    SET DIC=1130580001.301,DIC(0)="MAEQ"
        DO ^DIC WRITE !
        IF +Y'>0 GOTO BRWDN
        NEW LSTNAME SET LSTNAME=$PIECE(Y,"^",2)
        NEW RECIEN SET RECIEN=+Y
BRW2    SET USRPICK=$$MENU^TMGUSRI2(.MENU,1)
        IF USRPICK="^" SET USRPICK=""
        IF USRPICK="" GOTO BRW1
        IF USRPICK="DUMP" DO  GOTO BRW1
        . NEW % SET %=2
        . WRITE "Display empty fields"
        . DO YN^DICN
        . IF %=-1 WRITE ! QUIT
        . NEW %ZIS
        . SET %ZIS("A")="Enter Output Device: "
        . SET %ZIS("B")="HOME"
        . DO ^%ZIS  ;"standard device call
        . IF POP DO  QUIT
        . . WRITE "Error opening output.  Aborting.",!
        . . DO PRESS2GO^TMGUSRI2
        . USE IO
        . WRITE ! DO DUMPREC^TMGDEBU3(1130580001.301,RECIEN,(%=1)) ;"Do the output
        . DO ^%ZISC  ;" Close the output device
        . DO PRESS2GO^TMGUSRI2
        IF USRPICK="SEL" DO  GOTO BRW1
        . NEW TMGPICK,TMGRESULT
        . NEW IEN SET IEN=0
        . NEW % SET %=2
        . FOR  SET IEN=$ORDER(^C0Q(301,RECIEN,1,IEN)) QUIT:(+IEN'>0)  DO
        . . NEW TMGDFN SET TMGDFN=+$PIECE($GET(^C0Q(301,RECIEN,1,IEN,0)),"^",1)
        . . IF TMGDFN'>0 QUIT
        . . NEW PTNAME SET PTNAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
        . . QUIT:PTNAME=""
        . . SET TMGPICK(PTNAME_"      (#"_TMGDFN_")")=TMGDFN
        . IF $DATA(TMGPICK)=0 QUIT
        . DO SELECTR2^TMGUSRI3("TMGPICK","TMGRESULT",LSTNAME_".  Press <Esc><Esc> when done.")
        . IF $DATA(TMGRESULT)=0 QUIT
        . SET PTNAME=""
        . FOR  SET PTNAME=$ORDER(TMGRESULT(PTNAME)) QUIT:(PTNAME="")!(%=-1)  DO
        . . NEW TMGDFN SET TMGDFN=+$GET(TMGRESULT(PTNAME))
        . . QUIT:TMGDFN'>0
        . . SET %=2
        . . WRITE "CHECK DETAILS FOR ",PTNAME DO YN^DICN WRITE !
        . . IF %'=1 QUIT
        . . NEW STR SET STR=$$MEANFLUS^TMGC0Q06(TMGDFN)
        . . WRITE !,STR,!
        . . DO PRESS2GO^TMGUSRI2
BRWDN   QUIT
        ;
