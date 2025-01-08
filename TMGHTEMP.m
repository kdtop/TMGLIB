TMGHTEMP ;TMG/kst-Text objects for use in CPRS ; 7/20/12, 2/2/14, 3/24/21
         ;;1.0;TMG-LIB;**1,17**;7/20/12
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
  ;
PULLTIU()    ;
        ;
        NEW %ZIS,IOP
        SET %ZIS("A")="Enter Output Device: "
        set %ZIS("B")="HOME"
        ;SET IOP="S121-LAUGHLIN-LASER"
        DO ^%ZIS
        IF POP GOTO PTIUDN
        USE IO
        NEW TMGDFN SET TMGDFN=1
        NEW DFNARRAY
        NEW NOTEIEN SET NOTEIEN=1
        NEW ARRAY,NAME
        NEW LINE SET LINE=0
        ;GET LIST OF PATIENTS SEEN THIS YEAR
        FOR  SET NOTEIEN=$ORDER(^TIU(8925,NOTEIEN)) QUIT:(NOTEIEN="")  DO
        . IF $PIECE($GET(^TIU(8925,NOTEIEN,0)),"^",7)>3130101  DO
        . . SET DFNARRAY($PIECE($GET(^TIU(8925,NOTEIEN,0)),"^",2))=""
        FOR  SET TMGDFN=$ORDER(DFNARRAY(TMGDFN)) QUIT:(TMGDFN="")  DO
        . DO GETSPECL^TMGTIUO4(TMGDFN,"STUDIES","BLANK_LINE",99,.ARRAY,1,"")
        . IF $DATA(ARRAY) DO
        . . SET NAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
        . . ;WRITE "***********",$PIECE($GET(^DPT(TMGDFN,0)),"^",1),"************",!
        . . ;IF $DATA(ARRAY("KEY-VALUE","TDAP / TD","LINE")) DO
        . . ;. IF $GET(ARRAY("KEY-VALUE","TDAP / TD","LINE"))["13" DO
        . . ;. . WRITE NAME," HAS HAD TDAP.",!
        . . ;. . WRITE "  **",$GET(ARRAY("KEY-VALUE","TDAP / TD","LINE")),!
        . . ;IF $DATA(ARRAY("KEY-VALUE","FLU VACCINE","LINE")) DO
        . . ;. IF $GET(ARRAY("KEY-VALUE","FLU VACCINE","LINE"))["13" DO
        . . ;. . WRITE NAME," HAS HAD FLU VACCINE.",!
        . . ;. . WRITE "  **",$GET(ARRAY("KEY-VALUE","FLU VACCINE","LINE")),!,!
        . . IF $DATA(ARRAY("KEY-VALUE","ZOSTAVAX","LINE")) DO
        . . . IF $GET(ARRAY("KEY-VALUE","ZOSTAVAX","LINE"))["13" DO
        . . . . WRITE NAME," HAS HAD ZOSTAVAX.",!
        . . . . WRITE "  **",$GET(ARRAY("KEY-VALUE","ZOSTAVAX","LINE")),!,!
        . ;WRITE "***********************************************",!,!
        DO ^%ZISC
PTIUDN        QUIT
        ;
PULLPED()     ;
        NEW %ZIS,IOP
        SET %ZIS("A")="Enter Output Device: "
        set %ZIS("B")="HOME"
        ;SET IOP="S121-LAUGHLIN-LASER"
        DO ^%ZIS
        IF POP GOTO PPDN
        USE IO
        NEW TMGDFN SET TMGDFN=1
        NEW DFNARRAY
        NEW NOTEIEN SET NOTEIEN=1
        NEW ARRAY,NAME
        NEW LINE SET LINE=0
        ;GET LIST OF PATIENTS SEEN THIS YEAR
        NEW DOB,TMGDFN
        FOR  SET NOTEIEN=$ORDER(^TIU(8925,NOTEIEN)) QUIT:(NOTEIEN="")  DO
        . IF $PIECE($GET(^TIU(8925,NOTEIEN,0)),"^",7)>3110101  DO
        . . SET TMGDFN=$PIECE($GET(^TIU(8925,NOTEIEN,0)),"^",2)
        . . IF TMGDFN'="" DO
        . . . SET DOB=$PIECE($GET(^DPT(TMGDFN,0)),"^",3)
        . . . IF DOB>(DT-0180000)  DO  ;IF THE DOB IS LESS THAN TODAY - 18 YEARS
        . . . . SET DFNARRAY(TMGDFN)=""
        ;QUIT
        SET TMGDFN=0
        NEW PTDATA,AGE,Y,RESULT
        WRITE "                      Report: Pediatric Studies Table",!
        WRITE "                                From PULLPED^TMGHTEMP",!
        FOR  SET TMGDFN=$ORDER(DFNARRAY(TMGDFN)) QUIT:(TMGDFN="")  DO
        . SET RESULT=$$GETTABLX^TMGTIUO6(TMGDFN,"STUDIES",.ARRAY)
        . SET NAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
        . IF NAME["ZZ" QUIT
        . DO SELECT^ORWPT(.PTDATA,TMGDFN)
        . SET AGE=$PIECE(PTDATA,"^",15)
        . SET Y=$PIECE($GET(^DPT(TMGDFN,0)),"^",3)
        . DO DD^%DT
        . WRITE "------------------ "_NAME_" ("_Y_" - "_AGE_"y)-------------------",!
        . IF $DATA(RESULT) DO
        . . WRITE RESULT,!
        . ELSE  DO
        . . WRITE "NO DATA FOUND",!
        . ; WRITE "-------------------------------------------------------------",!,!
        . WRITE !
        DO ^%ZISC
PPDN        QUIT
        ;
        ;;<===================THE FOLLOWING HAS BEEN MOVED TO TMGPXR3==============>
REMPTRPT()  ;Prints report of all patients due for a given reminder
        ;Flesh out details here
        NEW IEN,DATE,PTARRAY
        ;Get IEN of reminder from user
        SET IEN=127
        ;Get test date
        SET DATE=3130418
        ;Run report
        NEW PTARRAY,RESULTARR
        DO GETPTLST(.PTARRAY)
        DO RUNRPT(.RESULTARR,.PTARRAY,IEN,DATE)
        QUIT
        ;
GETPTLST(OUTARRAY)   ;
        ;Purpose: Gather list of patients to run report on
        ;MERGE OUTARRAY=^DPT("B")
        NEW TMGDFN SET TMGDFN=0
        FOR  SET TMGDFN=$ORDER(^DPT(TMGDFN)) QUIT:(+TMGDFN'>0)  DO
        . SET OUTARRAY(TMGDFN)=""
        QUIT
RUNRPT(RESULTARR,PTARRAY,IEN,DATE)  ;Prints reminder report to specified device
        ;Purpose: Runs TESTREM^TMGRPC3G for all patients with given criteria
        ;Input: RESULTARR - Result Array (See Result)
        ;       IEN - IEN of the Reminder to be ran
        ;        DATE - Test date (internal format)
        ;Output: Prints to device
        ;Result: Array containing RESULT^LastGiven(External Format)  
        ;        (e.g. Result(TMGDFN)="N/A^^") 
        ;        (e.g. Result(TMGDFN)="DONE^04/03/2017^04/02/2013")
        ;        (e.g. Result(TMGDFN)="DUE NOW^DUE NOW^unknown")
        NEW TMGDFN SET TMGDFN=0
        NEW PXRHM SET PXRHM=5
        NEW PATLIST,TMGRESULT
        FOR  SET TMGDFN=$ORDER(PTARRAY(TMGDFN)) QUIT:TMGDFN=""  DO
        . ;WRITE TMGDFN
        . SET TMGRESULT=$$DOREM(TMGDFN,IEN,PXRHM,DATE)
        . ;WRITE " - ",TMGRESULT,!
        . SET RESULTARR(TMGDFN)=TMGRESULT
        QUIT
        ;
CMOUT()        ;Do formatted Clinical Maintenance output.
        N DUE,LAST,RIEN,RNAME,STATUS,TEMP
        S RIEN=$O(^TMP("PXRHM",$J,""))
        S RNAME=$O(^TMP("PXRHM",$J,RIEN,""))
        S TEMP=$G(^TMP("PXRHM",$J,RIEN,RNAME))
        S STATUS=$P(TEMP,U,1)
        S DUE=$$EDATE^PXRMDATE($P(TEMP,U,2))
        S LAST=$$EDATE^PXRMDATE($P(TEMP,U,3))
        Q STATUS_"^"_DUE_"^"_LAST
        ;
DOREM(TMGDFN,PXRMITEM,PXRMHM,DATE)        ;Do the reminder
        N DEFARR,FIEVAL,FINDING,PXRMDEBG,PXRMID,REF,TFIEVAL
        NEW TMGRESULT
        ;This is a debugging run so SET PXRMDEBG.
        S PXRMDEBG=1
        D DEF^PXRMLDR(PXRMITEM,.DEFARR)
        I +$G(DATE)=0 D EVAL^PXRM(TMGDFN,.DEFARR,PXRHM,1,.FIEVAL)
        I +$G(DATE)>0 D EVAL^PXRM(TMGDFN,.DEFARR,PXRHM,1,.FIEVAL,DATE)
        ;
        I $D(^TMP("PXRMFFDEB",$J)) M FIEVAL=^TMP("PXRMFFDEB",$J) K ^TMP("PXRMFFDEB",$J)
        ;
        S REF="FIEVAL"
        ;
        I $G(PXRMTDEB) D
        . S REF="TFIEVAL"
        . S FINDING=0
        . F  S FINDING=$O(^TMP("PXRMTDEB",$J,FINDING)) Q:FINDING=""  D
        .. K TFIEVAL M TFIEVAL(FINDING)=^TMP("PXRMTDEB",$J,FINDING)
        . K ^TMP("PXRMTDEB",$J)
        S REF="^TMP(""PXRHM"",$J)"
        NEW TMGRESULT
        I $D(^TMP("PXRHM",$J))  DO 
        . SET TMGRESULT=$$CMOUT
        K ^TMP("PXRM",$J),^TMP("PXRHM",$J),^TMP("PXRMMHVC",$J)
        Q TMGRESULT
        ;;<===================ABOVE HAS BEEN MOVED TO TMGPXR3==============>
        