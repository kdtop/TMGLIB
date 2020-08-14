TMGIDE7 ;TMG/kst/A debugger/tracer for GT.M (Data watcher) ;6/27/12, 2/2/14
         ;;1.0;TMG-LIB;**1**;6/23/12
 ;
 ;" GT.M  Data watcher
 ;"
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"============================================================
 ;"PUBLIC API
 ;"============================================================
 ;"EditWatch --Add, delete, or edit data watches
 ;"EVALDW(tmgRunMode) ;Eval Data Watches
 ;
 ;"============================================================
 ;"PRIVATE API
 ;"============================================================
 ;"ADD -add a data watch entry
 ;"EDIT(tmgMumpsLine) --edit a line of code used for data watch
 ;
 ;"============================================================
 ;"============================================================
 ;"NOTE: The purpose of data watches is different from variable watches
 ;"      already implemented in TMGIDE.  Variable watches are
 ;"      variables etc that are displayed after each line of execution
 ;"      of code, as one steps through a program
 ;"      A DATA WATCH is where a test is defined, and then the program
 ;"      is launched to run at full speed.  Between each line, the
 ;"      data watch tests are evaluated.  If any one of them is found
 ;"      to be true, then the execution is interrupted, and the user
 ;"      is shown the current execution point.
 ;"
 ;"      Usage:  If you want to find where in a large program a variable
 ;"             is ste to a value of, for example, 2, one could specify
 ;"             a test like this:  'IF $G(MYVAR)=2'
 ;"------------------------------------------------------------
 ;"------------------------------------------------------------
 ;"=======================================================================
 ;"=======================================================================
 ;"Dependencies:
 ;" TMGUSRI2, DIR
 ;"=======================================================================
 ;"=======================================================================
 ;
EditWatch(WatchMode) ;
        ;"Purpose: to Add, delete, or edit data watches
        ;"Input: WatchMode -- PASS BY REFERENCE.  An OUT PARAMETER.
        ;"        If user wants to start code now to use watches, WatchMode SET to 1
        ;"Output: Changes may be made to ^TMG("TMGIDE",$J,"DATA WATCHES")
        NEW Menu,UsrSlct,ct,dataWatch,Found
        ;
EW1     KILL Menu,Found
        SET Menu(0)="Pick Data Watch Option:"
        SET ct=1
        SET dataWatch=""
        FOR  SET dataWatch=$ORDER(^TMG("TMGIDE",$J,"DATA WATCHES",dataWatch)) QUIT:(dataWatch="")  DO
        . SET Menu(ct)="EDIT: "_dataWatch_$CHAR(9)_dataWatch,ct=ct+1
        . SET Found(dataWatch)=1
        SET Menu(ct)="Add a NEW data watch"_$CHAR(9)_"ADD",ct=ct+1
        IF $DATA(^TMG("TMGIDE",$J,"DATA WATCHES")) DO
        . SET Menu(ct)="Launch code with these settings"_$CHAR(9)_"^",ct=ct+1
        SET Menu(ct)="HELP"_$CHAR(9)_"?",ct=ct+1
        ;
EW2     SET UsrSlct=$$MENU^TMGUSRI2(.Menu,"^")
        IF UsrSlct="^" GOTO EWDN
        IF UsrSlct=0 GOTO EW2
        IF UsrSlct="ADD" DO ADD GOTO EW1
        IF UsrSlct="?" DO HELP GOTO EW1
        DO EDIT(UsrSlct)
        GOTO EW1
EWDN    SET WatchMode=($DATA(^TMG("TMGIDE",$J,"DATA WATCHES"))>0)
        IF WatchMode DO
        . NEW % SET %=2
        . WRITE "Launch code NOW, applying data watches"
        . DO YN^DICN WRITE !
        . SET WatchMode=(%=1)
        QUIT
        ;
        ;
ADD ;
        ;"Purpose: add a data watch entry
        WRITE !,"Enter a NEW line of mumps code to trigger a breakpoint",!
        WRITE "Code should SET $TEST=1 IF breakpoint should triggered",!
        WRITE !,"E.g. IF $DATA(MYVAR)>1",!
        WRITE !
        WRITE "Enter ^ to abort",!,!
        NEW tmgMumpsLine SET tmgMumpsLine=""
        DO EDIT(.tmgMumpsLine)
ADDDN   QUIT
        ;
        ;
HELP    ;
        WRITE !,"DATA WATCHES",!
        WRITE "-------------",!
        WRITE "This functionality allows users to specify tests and then",!
        WRITE "launch a program.  When any of the tests turns true, then",!
        WRITE "the program will be interrupted and the user will be",!
        WRITE "dropped into debug mode.",!
        WRITE !
        WRITE "Example of use:",!
        WRITE "Imagine a complex program, with thousands of lines of code",!
        WRITE "And imagine you want to know when a particular condition ",!
        WRITE "arises.  For our example, let's say we want to know when",!
        WRITE "something is stored in ^TMP($J,""VAL"").",!
        WRITE "To do this, one could create a data watch test like this:",!
        WRITE "  IF $DATA(^TMP($J,""VAL""))>0",!
        WRITE "This test will be xecuted between every line of code,",!
        WRITE "with automatic precautions to not change the environement.",!
        WRITE "The naked indicator, IO channels etc etc will saved and",!
        WRITE "restored automatically.  However, user test code should",!
        WRITE "NOT SET or KILL variables, as such changes are not ",!
        WRITE "monitored or corrected for, and could alter flow in the",!
        WRITE "test program.",!
        DO PRESS2GO^TMGUSRI2
        QUIT

EDIT(tmgMumpsLine) ;
        ;"Purpose: edit a line of code used for data watch
        NEW DIR
        SET DIR(0)="FUO"
        SET DIR("A",1)="Enter mumps code for data watch test."
        SET DIR("A",2)="e.g. IF $DATA(Y)>0"
        SET DIR("A")="TEST CODE"
        SET DIR("B")=tmgMumpsLine
        SET DIR("?")="Enter free text mumps code that sets $TEST"
        SET DIR("?",1)="Enter         For Action"
        SET DIR("?",2)="------        ---------------------------------------------------------------"
        SET DIR("?",3)=" ...          Replace everything"
        SET DIR("?",4)=" xxx...       Replace everything, starting from the characters xxx to the end"
        SET DIR("?",5)=" ...xxx       Replace everything from the beginning up to and including the "
        SET DIR("?",6)="                characters xxx"
        SET DIR("?",7)=" xxx...yyy    Replace everything starting from xxx up to and including yyy"
        SET DIR("?",8)=" end or END   Appends what you enter at the 'With' prompt to the end of the"
        SET DIR("?",9)="                value"
        SET DIR("?",10)="@             To delete everything"
        SET DIR("?",11)="  "
        DO ^DIR WRITE !
        IF $GET(tmgMumpsLine)'="" KILL ^TMG("TMGIDE",$J,"DATA WATCHES",tmgMumpsLine)
        IF (Y'="")&(Y'="^") DO
        . SET ^TMG("TMGIDE",$J,"DATA WATCHES",Y)=""
        . SET mumpLine=Y
        QUIT
        ;
EVALDW(tmgRunMode) ;Eval Data Watches
        ;"Purpose: to evaluate all data watches (if defined) and determine IF execution
        ;"         should stop
        ;"Input: tmgRunMode -- PASS BY REFERENCE.  This is an OUTPARAMETER
        ;"                   If result=0 (interrupt/stop), then tmgRunMode will be changed to 1
        ;"Result: 1 if should continue execution, 0 if should stop and display code,
        ;"        Also, tmgRunMode may be changed.
        NEW tmgTempResult SET tmgTempResult=1
        NEW tmgMumpsLine SET tmgMumpsLine=""
        FOR  SET tmgMumpsLine=$ORDER(^TMG("TMGIDE",$J,"DATA WATCHES",tmgMumpsLine)) QUIT:(tmgMumpsLine="")!(tmgTempResult=0)  DO
        . NEW $ETRAP SET $ETRAP="SET $ETRAP="""",$ecode="""" QUIT"
        . XECUTE tmgMumpsLine
        . SET tmgTempResult='($TEST)
        ;"SET ^TMG("TMP","ZZ","tmgTempResult")=tmgTempResult  ;"<-- temp, remove later
        IF tmgTempResult=0 DO
        . DO SetRunMode^TMGIDE2(1)
        . SET tmgTempResult=0
        . ;"write !,"Test evaluated TRUE, turrning tmgRunMode to 1",!  ;//delete later...
        . ;"DO PRESS2GO^TMGUSRI2         ;//delete later...
        QUIT tmgTempResult
        ;
