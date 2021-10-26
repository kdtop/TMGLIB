TMGPXR03 ;TMG/kst/TMG Reminder Reports stuff ;4/18/13, 2/2/14, 3/24/21
         ;;1.0;TMG-LIB;**1**;4/18/13
 ;
 ;"TMG REMINDER FUNCTIONS
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
 ;"PUBLIC FUNCTIONS
 ;"=======================================================================
 ;"REMPTRPT()  --Prints report of all patients due for a given reminder
 ;"ACTIVEPT(TMGDFN,WINDOW)-- Is patient active?
 ;"RUNRPT(RESULTARR,PTARRAY,IEN,DATE,SHOWPROG,WANTNA)  ;Prints reminder report to specified device
 ;"DOREM(TMGDFN,PXRMITEM,PXRHM,DATE,WANTNA) -- CODE MODIFIED FROM ^PXRMDEV
 ;
 ;"=======================================================================
 ;"PRIVATE FUNCTIONS
 ;"=======================================================================
 ;"GETPTLST(OUTARRAY)  -- Get list of patients
 ;"GETSTLST(OUTARRAY) -- Get list of patients from SORT TEMPLATE
 ;"SAVSTLST(ARRAY)  -- SAVE ARRAY TO SORT TEMPLATE LIST 
 ;"GETALPTS(OUTARRAY) -- Gather list of ALL patients 
 ;"GETACTPTS(OUTARRAY) -- Gather list of ACTIVE patients to run report on
 ;"GETNTPTS(OUTARRAY) -- Gather list of patients with progress note between dates, to run report on
 ;"GETSWMD(MODES)  -- GET SHOW MODES
 ;"SHOWLST(ARRAY)-- Output list in formatted manner.
 ;"ACTIVEPT(TMGDFN) -- ACTIVE PATIENT
 ;
 ;"=======================================================================
 ;"Dependancies :  TMGUSRI2
 ;"=======================================================================
 ;
REMPTRPT()  ;"Prints report of all patients due for a given reminder
        WRITE !,!,"------------------------------------------",!
        WRITE "Reminder Reports for Patients",!
        WRITE "------------------------------------------",!,!
        NEW DIC,X,Y
        SET DIC=811.9,DIC(0)="MEAQ"
        DO ^DIC WRITE !
        IF +Y'>0 DO  GOTO RRPTDN
        . WRITE "No reminder selected.",!
        NEW IEN SET IEN=+Y
        NEW %DT SET %DT="AE" KILL X,Y
        SET %DT("A")="Enter EFFECTIVE DATE of the reminder: "
        DO ^%DT WRITE ! KILL %DT
        IF Y'>0 DO  GOTO RRPTDN
        . WRITE "No effective date specified.",!        
        NEW DATE SET DATE=Y
        NEW TEMP,PTARRAY SET TEMP=$$GETPTLST(.PTARRAY)
        IF TEMP'>0 GOTO RRPTDN
        NEW RESULTARR
        WRITE "Running reminder on ",$$LISTCT^TMGMISC2("PTARRAY")," selected patients",!
        DO RUNRPT(.RESULTARR,.PTARRAY,IEN,DATE,1,0)
        DO SHOWLST(.RESULTARR) ;
        NEW % SET %=2
        WRITE "Save results" DO YN^DICN WRITE !
        IF %=1 DO SAVSTLST(.RESULTARR) 
RRPTDN  WRITE "Goodbye.",!
        QUIT
        ;
GETPTLST(OUTARRAY)   ;
        ;"Result: 1^OK or -1^Message if problem
        NEW MENU,USRINPUT
        NEW TMGRESULT SET TMGRESULT="1^OK"
        NEW IDX 
GPTLST1 KILL MENU 
        SET IDX=0
        SET MENU(0)="Select source of patients for report"
        ;"SET MENU(-1,"COLOR","FG")=14  ;"WHITE
        ;"SET MENU(-1,"COLOR","BG")=4   ;"BLUE
        SET IDX=IDX+1,MENU(IDX)="All patients"_$CHAR(9)_"ALL"
        SET IDX=IDX+1,MENU(IDX)="All ACTIVE patients"_$CHAR(9)_"ACTIVE"
        SET IDX=IDX+1,MENU(IDX)="Patients from a saved SORT TEMPLATE list"_$CHAR(9)_"ST"
        SET IDX=IDX+1,MENU(IDX)="Patients with a progress note in date range"_$CHAR(9)_"NOTE"
        SET IDX=IDX+1,MENU(IDX)="Patients based on scheduled appts"_$CHAR(9)_"SCHED"
        SET USRINPUT=$$MENU^TMGUSRI2(.MENU)
        IF USRINPUT="ALL" DO  GOTO GPTLDN
        . SET TMGRESULT=$$GETALPTS(.OUTARRAY)   ;
        IF USRINPUT="ACTIVE" DO  GOTO GPTLDN
        . SET TMGRESULT=$$GETACTPTS(.OUTARRAY)   ;
        IF USRINPUT="ST" DO  GOTO GPTLDN
        . SET TMGRESULT=$$GETSTLST(.OUTARRAY) 
        IF USRINPUT="NOTE" DO  GOTO GPTLDN
        . SET TMGRESULT=$$GETNTPTS(.OUTARRAY)
        IF USRINPUT="SCHED" DO  GOTO GPTLDN
        . SET TMGRESULT=$$GETSCPTS(.OUTARRAY)
        IF USRINPUT="^" DO  GOTO GPTLDN
        . SET TMGRESULT="-1^ABORTED"
        WRITE !,"??",!
        GOTO GPTLST1           
GPTLDN  QUIT TMGRESULT        
        ;
GETSTLST(OUTARRAY) ;        
        ;"Purpose: Gather list of patients from a SORT TEMPLATE to run report on
        ;"Result: 1^OK or -1^Message if problem
        NEW TMGRESULT SET TMGRESULT="1^OK"
        NEW DIC SET DIC=.401,DIC(0)="MAEQ"
        SET DIC("S")="NEW FL SET FL=$P(^(0),U,4) IF (FL=2)!(FL=9000001)"  ;"Only allow selection of templates to file patient file
        SET DIC("A")="Select SORT TEMPLATE with patients: "
        DO ^DIC WRITE !
        IF Y'>0 SET TMGRESULT=Y GOTO GSTLDN
        MERGE OUTARRAY=^DIBT(+Y,1)
GSTLDN  QUIT TMGRESULT
        ;
SAVSTLST(ARRAY)  ; "SAVE ARRAY TO SORT TEMPLATE LIST
        ;"Input: ARRAY --  Format: ARRAY(STATUS,TMGDFN)=Status^DueDate^LastGiven  
        ;"                      Dates are External Format, or 'DUE NOW'
        ;"                      Status is: N/A, DONE, or DUE NOW.
        ;"NOTES: SORT TEMPLATE (.401) is output file
        ;"
        NEW TMGRESULT SET TMGRESULT="1^OK"
        NEW MENU,USRINPUT
        NEW TMGRESULT SET TMGRESULT="1^OK"
        NEW STIEN SET STIEN=0
        NEW MODE SET MODE="OR"
        NEW IDX SET IDX=0
        SET MENU(0)="Select option"
        IF $DATA(ARRAY("N/A")) SET IDX=IDX+1,MENU(IDX)="Save patients with 'N/A' results"_$CHAR(9)_"N/A"
        IF $DATA(ARRAY("DONE")) SET IDX=IDX+1,MENU(IDX)="Save patients with 'DONE' results"_$CHAR(9)_"DONE"
        IF $DATA(ARRAY("DUE NOW")) SET IDX=IDX+1,MENU(IDX)="Save patients with 'DUE NOW' results"_$CHAR(9)_"DUE NOW"
        SET USRINPUT=$$MENU^TMGUSRI2(.MENU)
        NEW STATUS SET STATUS=""
        IF (USRINPUT="N/A")!(USRINPUT="DONE")!(USRINPUT="DUE NOW") DO
        . SET STATUS=USRINPUT
        IF STATUS="" GOTO SVLTDN
        KILL MENU SET IDX=0
        SET MENU(0)="Select option"
        SET IDX=IDX+1,MENU(IDX)="Choose existing SORT TEMPLATE list"_$CHAR(9)_"EXISTING"
        SET IDX=IDX+1,MENU(IDX)="Make NEW SORT TEMPLATE list"_$CHAR(9)_"NEW"
        SET USRINPUT=$$MENU^TMGUSRI2(.MENU)
        IF USRINPUT="EXISTING" DO  
        . NEW DIC SET DIC=.401,DIC(0)="MAEQ"
        . SET DIC("S")="NEW FL SET FL=$P(^(0),U,4) IF (FL=2)!(FL=9000001)"  ;"Only allow selection of templates to file patient file
        . SET DIC("A")="Select SORT TEMPLATE with patients: "
        . DO ^DIC WRITE !
        . IF Y'>0 QUIT
        . SET STIEN=+Y 
        . KILL MENU SET IDX=0        
        . SET MENU(0)="Select option"
        . SET IDX=IDX+1,MENU(IDX)="Combine with prior via AND"_$CHAR(9)_"AND"
        . SET IDX=IDX+1,MENU(IDX)="Combine with prior via OR"_$CHAR(9)_"OR"
        . SET USRINPUT=$$MENU^TMGUSRI2(.MENU)
        . IF USRINPUT="^" QUIT
        . IF (USERINPUT="OR")!(USERINPUT="AND") SET MODE=USRINPUT
        IF USRINPUT="NEW" DO
        . NEW NAME 
NREC1   . WRITE "Name of new SORT TEMPLATE list? "
        . READ NAME:$GET(DTIME,3600) WRITE ! QUIT:NAME=""
        . IF ($L(NAME)<3)!($L(NAME)>30) WRITE "ERROR: Must be at 3-30 chars.  Try again",! GOTO NREC1
        . NEW X,Y,DIC SET DIC=.401,DIC(0)="L",X=NAME
        . DO ^DIC
        . IF +Y'>0 DO  QUIT
        . . SET TMGRESULT="-1^Unable to find IEN of newly added record"
        . SET STIEN=+Y
        . NEW TMGFDA,TMGIEN,TMGMSG
        . SET TMGFDA(.401,STIEN_",",2)="NOW"
        . SET TMGFDA(.401,STIEN_",",4)="`2"
        . DO FILE^DIE("E","TMGFDA","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) DO  QUIT
        . . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        IF STIEN'>0 GOTO SVLTDN
        IF MODE="OR" DO
        . MERGE ^DIBT(STIEN,1)=ARRAY(STATUS)  ;"just put values from ARRAY in to "1" node
        ELSE  DO
        . NEW TEMP MERGE TEMP=^DIBT(STIEN,1)
        . KILL ^DIBT(STIEN,1)
        . NEW ADFN SET ADFN=0
        . FOR  SET ADFN=$ORDER(ARRAY(STATUS,ADFN)) QUIT:ADFN'>0  DO
        . . IF $DATA(TEMP(ADFN))=0 QUIT
        . . SET ^DIBT(STIEN,1,ADFN)=""
SVLTDN  IF +TMGRESULT>0 WRITE "Saved.",!
        QUIT TMGRESULT
        ;
GETSCPTS(OUTARRAY)   ;
        ;"Purpose: Gather list of patients to run report on, based on scheduled appts. 
        ;"Result: 1^OK or -1^Message if problem
        NEW SDT,EDT
        NEW X,Y,%DT SET %DT="AET"
        SET %DT("A")="Enter START of date range for office appointments: "
        DO ^%DT WRITE ! ;"ASK STARTING DATE
        IF Y'>0 QUIT
        SET SDT=Y
        SET %DT("A")="Enter END of date range (ENTER for same as START): "
        DO ^%DT WRITE !  ;"ASK ENDING DATE
        IF Y'>0 SET Y=SDT
        SET EDT=Y
        SET RESULT=$$GETSCHDA(.OUTARRAY,SDT,EDT) 
        QUIT RESULT
        ;
GETSCHDA(OUTARRAY,SDT,EDT) ;
        ;"Get list of patients schedule for appt in date range
        ;"Input: OUTARRAY -- PASS BY REFERENCE, an OUT PARAMETER. 
        ;"           Format: OUTARRY(TMGDFN)=""
        ;"       SDT -- FM format for start of date range
        ;"       EDT -- FM format for end of date range
        ;"Result: 1^OK or -1^Message if problem
        ;"... FINISH
        ;"NOTE to Eddie -- Check in the code for sending SMS messages
        ;"  I think we gather patients with appts in given range.  So 
        ;"  we should reuse that code....
        NEW TEMP
        DO GETSCHED(.TEMP,SDT,EDT)
        NEW ADFN SET ADFN=0
        FOR  SET ADFN=$ORDER(TEMP(ADFN)) QUIT:+ADFN'>0  DO
        . SET OUTARRAY(ADFN)=""        
        QUIT "1^OK"
        ;
GETALPTS(OUTARRAY)   ;
        ;"Purpose: Gather list of ALL patients to run report on
        ;"Result: 1^OK or -1^Message if problem
        NEW TMGRESULT SET TMGRESULT="1^OK"
        WRITE "Gathering list of all patients",!
        NEW DFNMAX SET DFNMAX=$ORDER(^DPT("!"),-1)
        NEW STIME SET STIME=$H
        NEW DONE SET DONE=0
        NEW TMGDFN SET TMGDFN=0
        FOR  SET TMGDFN=$ORDER(^DPT(TMGDFN)) QUIT:(+TMGDFN'>0)  DO
        . SET OUTARRAY(TMGDFN)=""
        . IF TMGDFN#5=0 DO
        . . DO PROGBAR^TMGUSRI2(TMGDFN,"PROGRESS: "_TMGDFN,0,DFNMAX,60,STIME)
        DO PROGBAR^TMGUSRI2(100,"DONE: ",0,100,60,STIME)
        WRITE !        
        QUIT TMGRESULT
        ;
GETACTPTS(OUTARRAY,WINDOW)   ;
        ;"Purpose: Gather list of ACTIVE patients to run report on
        ;"Result: 1^OK or -1^Message if problem
        NEW TMGRESULT SET TMGRESULT="1^OK"
        SET WINDOW=+$G(WINDOW) IF WINDOW=0 SET WINDOW=3
        WRITE "Gathering list of all ACTIVE patients",!
        NEW DFNMAX SET DFNMAX=$ORDER(^DPT("!"),-1)
        NEW STIME SET STIME=$H
        NEW DONE SET DONE=0
        NEW TMGDFN SET TMGDFN=0
        FOR  SET TMGDFN=$ORDER(^DPT(TMGDFN)) QUIT:(+TMGDFN'>0)  DO
        . IF TMGDFN#5=0 DO
        . . DO PROGBAR^TMGUSRI2(TMGDFN,"PROGRESS: "_TMGDFN,0,DFNMAX,60,STIME)
        . IF $$ACTIVEPT(TMGDFN,WINDOW)=0 QUIT
        . SET OUTARRAY(TMGDFN)=""
        DO PROGBAR^TMGUSRI2(100,"DONE: ",0,100,60,STIME)
        WRITE !        
        QUIT TMGRESULT
        ;
GETNTPTS(OUTARRAY) ;
        ;"Purpose: Gather list of patients with progress note between dates, to run report on
        ;"Result: 1^OK or -1^Message if problem
        NEW TMGRESULT SET TMGRESULT="1^OK"
        NEW STARTDT,ENDDT,TIUIEN,TMGDFN
        NEW %DT SET %DT="AE" 
        NEW X,Y
        SET %DT("A")="Enter earliest DATE for progress note: "
        DO ^%DT WRITE !
        IF Y'>0 SET TMGRESULT=Y GOTO GNLDN
        SET STARTDT=Y-.000001
        KILL X,Y
        SET %DT("A")="Enter latest DATE for progress note: "
        DO ^%DT WRITE !
        IF Y'>0 SET TMGRESULT=Y GOTO GNLDN
        SET ENDDT=Y
        NEW DT SET DT=STARTDT
        FOR  SET DT=$ORDER(^TIU(8925,"D",DT)) QUIT:(+DT'>0)!(DT>ENDDT)  DO
        . SET TIUIEN=0
        . FOR  SET TIUIEN=$ORDER(^TIU(8925,"D",DT,TIUIEN)) QUIT:(+TIUIEN'>0)  DO
        . . SET TMGDFN=+$PIECE($GET(^TIU(8925,TIUIEN,0)),"^",2) QUIT:(+TMGDFN'>0)
        . . SET OUTARRAY(TMGDFN)=""        
GNLDN   QUIT TMGRESULT        
        ;
ACTIVEPT(TMGDFN,WINDOW) ;"ACTIVE PATIENT   **NAME ALERT** --There is also an ACTIVEPT^TMGPXR01() for a different purpose.
        ;"Input: TMGDFN -- the patient IEN
        ;"       WINDOW -- OPTIONAL.  DEFAULT=3.  Number of years of inactivity before patient considered inactive.
        ;"Results: 1 if patient is active, or 0 if not. 
        ;"NOTE: A patient will be considered to be active IF they have ANY notes
        ;"      TIU DOCUMENT (#8925) in past X yrs.
        ;"      Will SET X to be 3 years
        ;"      ALSO, I want any patient with a DEATH NOTE title or a 
        ;"        TRANSFER RECORDS title to be not active.
        NEW DIFFYR SET DIFFYR=+$GET(WINDOW) IF DIFFYR=0 SET DIFFYR=3  ;"//kt 
        NEW DIEDTITLE SET DIEDTITLE=1416   ;"<--- HARD CODED title IEN
        NEW XFERTITLE SET XFERTITLE=1426   ;"<--- HARD CODED title IEN
        NEW DCTITLE SET DCTITLE=1978       ;"<--- HARD CODED title IEN
        NEW DATE SET DATE=$$NOW^XLFDT
        NEW TMGRESULT SET TMGRESULT=0  ;"default is INACTIVE
        SET TMGDFN=+$GET(TMGDFN)  ;"//kt
        NEW NAME SET NAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)  ;"//kt
        IF $EXTRACT(NAME,1,2)="ZZ" GOTO APTDN  ;"//kt
        NEW SDTIME SET SDTIME=$$FMADD^XLFDT(DATE,-365*DIFFYR,0,0,0)
        ;"NEW REVDATE SET REVDATE=9999999-DATE
        ;"NEW REVSDT SET REVSDT=9999999-SDTIME        
        NEW TITLEIEN SET TITLEIEN=0
        NEW FOUND SET FOUND=0
        ;"NOTE: in 2 lines before, once DEATH title, or DISCHARGE title has EVER been
        ;"      added to the record, then it is irrevocable.  If this is a problem, 
        ;"      will have to refine later. 
        IF $DATA(^TIU(8925,"APT",TMGDFN,DIEDTITLE))>0 GOTO APTDN
        ;"TEST BELOW IF $DATA(^TIU(8925,"APT",TMGDFN,DCTITLE))>0 GOTO APTDN   ;"ELH added 10-5-15
        NEW XFER,XFERDT,LASTOVDT
        NEW DCED,DCDT
        SET XFER=0,XFERDT=0,LASTOVDT=0,DCED=0,DCDT=0
        ;"IF $DATA(^TIU(8925,"APT",TMGDFN,XFERTITLE))>0 GOTO APTDN ;"Transfer handled below now ELH
        FOR  SET TITLEIEN=$ORDER(^TIU(8925,"APT",TMGDFN,TITLEIEN)) QUIT:(+TITLEIEN'>0)  DO
        . NEW STATUSIEN SET STATUSIEN=0
        . FOR  SET STATUSIEN=$ORDER(^TIU(8925,"APT",TMGDFN,TITLEIEN,STATUSIEN)) QUIT:(+STATUSIEN'>0)  DO
        . . ;"//kt NEW PREVDT SET PREVDT=$ORDER(^TIU(8925,"APT",TMGDFN,TITLEIEN,STATUSIEN,REVDATE))
        . . ;"//kt IF PREVDT'>REVSDT SET FOUND=PREVDT
        . . NEW REVDT SET REVDT=0 FOR  SET REVDT=$ORDER(^TIU(8925,"APT",TMGDFN,TITLEIEN,STATUSIEN,REVDT)) QUIT:(+REVDT'>0)  DO
        . . . NEW DOCIEN SET DOCIEN=+$ORDER(^TIU(8925,"APT",TMGDFN,TITLEIEN,STATUSIEN,REVDT,""))
        . . . NEW ONEDATE SET ONEDATE=9999999-REVDT
        . . . NEW SKIP SET SKIP=0
        . . . IF TITLEIEN=1411 DO  QUIT:SKIP=1  ;"Check for an automated note that might have occurred long after last appt
        . . . . IF '((ONEDATE\1=3120817)!(ONEDATE\1=3120924)!(ONEDATE\1=3130926)) QUIT
        . . . . SET SKIP=($GET(^TIU(8925,DOCIEN,"TEXT",1,0))["Standardizing notation of")  
        . . . IF TITLEIEN=XFERTITLE DO  ;"TRANSFER DATE STORED FOR TESTING ELH
        . . . . SET XFER=1
        . . . . SET XFERDT=ONEDATE
        . . . IF TITLEIEN=DCTITLE DO  ;"ADDED IF 5/27/21
        . . . . SET DCED=1
        . . . . SET DCDT=ONEDATE
        . . . IF (TITLEIEN=1408)!(TITLEIEN=1399)!(TITLEIEN=1402)!(TITLEIEN=2140)!(TITLEIEN=2012)!(TITLEIEN=2135) DO
        . . . . IF ONEDATE>LASTOVDT SET LASTOVDT=ONEDATE
        . . . IF FOUND=0 SET FOUND=((ONEDATE'<SDTIME)&(ONEDATE'>DATE))
        IF (XFER=1)&(LASTOVDT>0) DO
        . NEW X1,X2,X
        . SET X1=LASTOVDT,X2=XFERDT
        . DO ^%DTC
        . IF X>30 SET XFER=0
        IF (DCED=1)&(LASTOVDT>0) DO  ;"ADDED IF 5/27/21
        . NEW X1,X2,X
        . SET X1=LASTOVDT,X2=DCDT
        . DO ^%DTC
        . IF X>30 SET DCED=0
        IF (XFER=0)&(DCED=0) SET TMGRESULT=(FOUND>0)
APTDN   QUIT TMGRESULT        
        ;                
RUNRPT(RESULTARR,PTARRAY,IEN,DATE,SHOWPROG,WANTNA)  ;Prints reminder report to specified device
        ;"Purpose: Runs TESTREM^TMGRPC3G for all patients with given criteria
        ;"Input: RESULTARR - Result Array (See Result)
        ;"       PTARRAY -- format is PTARRAY(TMGDFN)=""
        ;"       IEN - IEN of the Reminder to be run
        ;"       DATE - Test date (internal format)
        ;"       SHOWPROG -- optional.  Default=0, IF 1 then progress bar shown.
        ;"       WANTNA -- optional.  Default=0.  If 1 then patients with N/A results are returned.  Otherwise not. 
        ;"Output: RESULTARR Array containing:
        ;"           Format: RESULTARR(STATUS,TMGDFN)=Status^DueDate^LastGiven  
        ;"        Dates are External Format, or 'DUE NOW'
        ;"        Status is: N/A, DONE, or DUE NOW.  
        ;"        (e.g. Result("N/A",DFN)="N/A^^") 
        ;"        (e.g. Result("DONE",DFN)="DONE^04/03/2017^04/02/2013")
        ;"        (e.g. Result("DUE NOW",DFN)="DUE NOW^DUE NOW^unknown")
        ;"Result : none
        NEW STIME SET STIME=$H
        SET SHOWPROG=+$GET(SHOWPROG)
        NEW STIME SET STIME=$H
        NEW DONE SET DONE=0
        NEW TMGDFN SET TMGDFN=0
        NEW DFNMAX SET DFNMAX=$ORDER(PTARRAY(""),-1)        
        NEW PATLIST,TMGRESULT
        FOR  SET TMGDFN=$ORDER(PTARRAY(TMGDFN)) QUIT:(+TMGDFN'>0)!DONE  DO
        . IF (SHOWPROG=1),(TMGDFN#5=0) DO
        . . DO PROGBAR^TMGUSRI2(TMGDFN,"PROGRESS: "_TMGDFN,0,DFNMAX,60,STIME)
        . . IF $$USRABORT^TMGUSRI2("scanning patients") SET DONE=1
        . SET TMGRESULT=$$DOREM(TMGDFN,IEN,5,DATE,.WANTNA)
        . ;"IF TMGRESULT="" QUIT  ;"null answer is signal that it was N/A, and N/A not wanted
        . IF TMGRESULT="" SET TMGRESULT="N/A"        
        . ;"SET RESULTARR(TMGDFN)=TMGRESULT
        . NEW STATUS SET STATUS=$PIECE(TMGRESULT,U,1)
        . SET RESULTARR(STATUS,TMGDFN)=TMGRESULT
        IF SHOWPROG DO
        . DO PROGBAR^TMGUSRI2(100,"DONE: ",0,100,60,STIME)
        . WRITE !                
        QUIT
        ;
  ;"=========================================================================
  ;"CODE MODIFIED FROM ^PXRMDEV
  ;"=========================================================================        
DOREM(TMGDFN,PXRMITEM,PXRHM,DATE,WANTNA)         ;"CODE MODIFIED FROM ^PXRMDEV
        ;"Purpose: Do the reminder
        ;"Input: TMGDFN  -- Patient IEN
        ;"       PXRMITEM -- IEN in REMINDER DEFINITION file
        ;"       PXRMHM -- '5'
        ;"       DATE  -- Effective date (FM format)
        ;"       WANTNA -- optional.  Default=0.  If 1 then patients with N/A results are returned.  Otherwise not. 
        N DEFARR,FIEVAL,FINDING,PXRMDEBG,PXRMID,REF,TFIEVAL
        NEW TMGRESULT,VADM,PXRMDEFS
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
        NEW TMGRESULT SET TMGRESULT=""
        I $D(^TMP("PXRHM",$J))  DO 
        . SET TMGRESULT=$$CMOUT(.WANTNA)
        K ^TMP("PXRM",$J),^TMP("PXRHM",$J),^TMP("PXRMMHVC",$J)
        ;"--- KT ADDED 6/16 ---------
        NEW NODE SET NODE="PXR"
        FOR  SET NODE=$ORDER(^TMP(NODE)) QUIT:$EXTRACT(NODE,1,3)'="PXR"  DO
        . KILL ^TMP(NODE,$J)
        ;"---------------------------
        Q TMGRESULT
        ;
CMOUT(WANTNA)        ;"CODE MODIFIED FROM ^PXRMDEV
        ;"Purpose: Do formatted Clinical Maintenance output.
        ;"Input:  WANTNA -- optional.  Default=0.  If 1 then patients with N/A results are returned.  Otherwise not. 
        N DUE,LAST,RIEN,RNAME,STATUS,TEMP
        S RIEN=$O(^TMP("PXRHM",$J,""))
        S RNAME=$O(^TMP("PXRHM",$J,RIEN,""))
        S TEMP=$G(^TMP("PXRHM",$J,RIEN,RNAME))
        S STATUS=$P(TEMP,U,1)
        S DUE=$$EDATE^PXRMDATE($P(TEMP,U,2))
        S LAST=$$EDATE^PXRMDATE($P(TEMP,U,3))
        IF STATUS="N/A",($GET(WANTNA)'=1) QUIT ""
        Q STATUS_"^"_DUE_"^"_LAST
        ;
  ;"=========================================================================
  ;"END CODE MODIFIED FROM ^PXRMDEV
  ;"=========================================================================
  ;
BOOL(B) ;
  QUIT $SELECT($GET(B):"ON. ",1=1:"OFF.")
  ;
GETSWMD(MODES)  ;"GET SHOW MODES
        ;"Input: MODES -- PASS BY REFERENCE, AN OUT PARAMETER
        ;"Output: MODES array filled as below:
        ;"          MODES("N/A")=1 (or 0)  1=show 0=don't show
        ;"          MODES("DUE NOW")=1 (or 0)
        ;"          MODES("DONE")=1 (or 0)
        ;"Result: 1^OK or -1^Message
        NEW MENU,USRINPUT
        NEW TMGRESULT SET TMGRESULT="1^OK"
        SET MODES("DUE SOON")=1,MODES("DUE NOW")=1,MODES("N/A")=0,MODES("RESOLVED")=0 
GMDL1   KILL MENU        
        NEW IDX SET IDX=0
        SET MENU(0)="Which type of patients to show?"
        NEW LABEL FOR LABEL="DUE SOON","DUE NOW","RESOLVED","N/A" DO
        . NEW B0,B1,B2 SET B0=+$GET(MODES(LABEL)),B1=$$BOOL(B0),B2=$$BOOL('B0)
        . NEW TEMP SET TEMP=$$LJ^XLFSTR("'"_LABEL_"'",10)
        . SET IDX=IDX+1,MENU(IDX)="Display for "_TEMP_" results is "_B1_" Toggle it "_B2_$CHAR(9)_LABEL
        SET IDX=IDX+1,MENU(IDX)="Done setting display options"_$CHAR(9)_"PROCEED"
        SET USRINPUT=$$MENU^TMGUSRI2(.MENU,IDX)
        IF USRINPUT="PROCEED" GOTO GMDDN
        IF USRINPUT="^" SET TMGRESULT="-1^ABORT" GOTO GMDDN
        SET MODES(USRINPUT)='+$GET(MODES(USRINPUT))
        GOTO GMDL1
GMDDN   QUIT TMGRESULT
        ;
SHOWLST(ARRAY) ;
        ;"Purpose: Output list in formatted manner.
        ;"Input: ARRAY --  Format: ARRAY(STATUS,TMGDFN)=Status^DueDate^LastGiven  
        ;"                      Dates are External Format, or 'DUE NOW'
        ;"                      Status is: N/A, DONE, or DUE NOW.
        NEW TMGDFN,MODES
        NEW %ZIS,%       
        SET %ZIS("A")="Enter Output Device: "
        SET %ZIS("B")="HOME"
SLL1    IF +$$GETSWMD(.MODES)'>0 GOTO SLDN  ;"GET SHOW MODES
        DO ^%ZIS  ;"standard device call
        IF POP DO  GOTO SLDN
        . WRITE "Error opening output.  Aborting.",!
        USE IO
        NEW COUNT SET COUNT=0
        ;"Do the output
        NEW STATUS SET STATUS=""
        FOR  SET STATUS=$ORDER(ARRAY(STATUS)) QUIT:(STATUS="")  DO
        . IF $GET(MODES(STATUS))'=1 QUIT
        . NEW SORTARR
        . SET TMGDFN=0
        . FOR  SET TMGDFN=$ORDER(ARRAY(STATUS,TMGDFN)) QUIT:(+TMGDFN'>0)  DO
        . . NEW STR SET STR=$GET(ARRAY(STATUS,TMGDFN)) QUIT:STR=""
        . . NEW STATUS SET STATUS=$PIECE(STR,U,1)
        . . NEW DUE SET DUE=$PIECE(STR,U,2)
        . . NEW LAST SET LAST=$PIECE(STR,U,3) IF LAST="" SET LAST="unknown"
        . . NEW NAME SET NAME=$PIECE($GET(^DPT(TMGDFN,0)),U,1)
        . . NEW DOB SET DOB=$$GET1^DIQ(2,TMGDFN,.03,"E")
        . . NEW OUTSTR SET OUTSTR=NAME_" ("_DOB_")"
        . . SET OUTSTR=$$LJ^XLFSTR(OUTSTR,40)
        . . SET OUTSTR=OUTSTR_"Status: "_STATUS
        . . IF '((STATUS="DUE NOW")&(DUE="DUE NOW"))&(DUE'="") DO
        . . . SET OUTSTR=OUTSTR_", Date due: "_DUE
        . . IF LAST'="unknown" SET OUTSTR=OUTSTR_", Last given: "_LAST
        . . ;"WRITE NAME," (",DOB,") -- ",?40,"Status: ",STATUS
        . . ;"IF '((STATUS="DUE NOW")&(DUE="DUE NOW"))&(DUE'="") DO
        . . ;". WRITE ", Date due: ",DUE
        . . ;"IF LAST'="unknown" WRITE ", Last given: ",LAST
        . . ;"WRITE !
        . . SET SORTARR(OUTSTR)=""
        . NEW STR SET STR=""
        . FOR  SET STR=$ORDER(SORTARR(STR)) QUIT:(STR="")  DO
        . . WRITE STR,!
        . . SET COUNT=COUNT+1
        WRITE COUNT," patients in list",!
        DO ^%ZISC
        SET %=2
        WRITE "Print again" DO YN^DICN WRITE !
        IF %=1 GOTO SLL1        
SLDN    QUIT        
        ;"
GETCSPAT(TMGRESULT,BEGDT,ENDDT)  ;"
        ;"Purpose: this function returns an array 
        ;"         of all patients who are scheduled for
        ;"         the given dates and are on Controlled Substances
        NEW ARRAY,COUNT
        SET BEGDT=$GET(BEGDT),ENDDT=+$GET(ENDDT)
        IF ENDDT'>0 SET ENDDT=BEGDT
        DO GETSCHED(.ARRAY,BEGDT,ENDDT,"AO")
        NEW TMGDFN,RESULT
        SET TMGDFN=0
        FOR  SET TMGDFN=$O(ARRAY(TMGDFN)) QUIT:TMGDFN'>0  DO
        . NEW TEST,DATE,DATA,TEXT
        . DO PAINMEDS^TMGPXR01(TMGDFN,.TEST,.DATE,.DATA,.TEXT) 
        . IF TEST=1 DO
        . . NEW DATE SET DATE=$ORDER(ARRAY(TMGDFN,0))
        . . SET TMGRESULT(DATE,TMGDFN)=""
        QUIT
        ;"
TOMORROW(DATE)
        NEW TOMORROW
        NEW X,X1,X2
        SET X1=DATE,X2=1
        DO C^%DTC
        SET TOMORROW=X
        QUIT TOMORROW
        ;"
NEXTDATE(DATE,DAYSTOCHECK)   ;"
        ;"Purpose: This function will return the NEXT business date after
        ;"         the date provided. If not provided, then it will assume
        ;"         today.
        NEW TMGRESULT,TEMPARR,COUNT
        SET DAYSTOCHECK=$G(DAYSTOCHECK)
        IF DAYSTOCHECK=0 SET DAYSTOCHECK=30 ;"QUIT IF NO RESULTS FOUND IN SO
                                            ;"MANY DAYS, TO STOP INFINITE LOOPS
        SET DATE=+$G(DATE),COUNT=0
        IF DATE=0 DO
        . NEW X DO NOW^%DTC SET DATE=X
        SET TMGRESULT=0
        FOR  SET DATE=$$TOMORROW(DATE) QUIT:(TMGRESULT>0)!(COUNT>DAYSTOCHECK)  DO
        . IF $$GETSCHED(.TEMPARR,DATE,DATE,"AO")>0 DO
        . . SET TMGRESULT=DATE
        . SET COUNT=COUNT+1
        QUIT TMGRESULT
        ;"
GETSCHED(TMGRESULT,BEGDT,ENDDT,STATUSES,EXCLUDE)  ;"
        ;"Purpose: This function returns an array
        ;"         of all appointment inside the given 
        ;"         date range.
        ;"   TMGRESULT(Datetime,TMGDFN)=""
        ;"   BEGDT-FM START DATE
        ;"   ENDDT-(Optional)FM END DATE. Will default to BEGDT if not provided
        ;"   STATUSES-(Optional) Statuses to return. Defaults to "A[ctive]"
        ;"   EXCLUDE- (Optional) Exclude nursing appointments
        NEW NUMOFAPPTS SET NUMOFAPPTS=0
        SET EXCLUDE=$G(EXCLUDE)
        IF EXCLUDE'=0 SET EXCLUDE=1
        SET STATUSES=$$UP^XLFSTR($G(STATUSES))
        IF STATUSES="" SET STATUSES="A"
        SET BEGDT=$PIECE($GET(BEGDT),",",1)
        SET ENDDT=+$GET(ENDDT)
        IF ENDDT'>0 SET ENDDT=BEGDT
        SET ENDDT=ENDDT+.999999
        NEW DTIDX SET DTIDX=BEGDT
        FOR  SET DTIDX=$ORDER(^TMG(22723,"DT",DTIDX)) QUIT:(DTIDX>ENDDT)!(DTIDX'>0)  DO
        . NEW TMGDFN SET TMGDFN=0
        . FOR  SET TMGDFN=$ORDER(^TMG(22723,"DT",DTIDX,TMGDFN)) QUIT:TMGDFN'>0  DO
        . . NEW IDX SET IDX=$ORDER(^TMG(22723,"DT",DTIDX,TMGDFN,0))
        . . NEW STATUS SET STATUS=$GET(^TMG(22723,"DT",DTIDX,TMGDFN,IDX))
        . . ;"NEW TIMEIN SET TIMEIN=+$P($GET(^TMG(22723,TMGDFN,1,IDX,0)),"^",8)
        . . IF STATUSES[STATUS DO
        . . . IF EXCLUDE=1 DO
        . . . . NEW REASON SET REASON=$P($GET(^TMG(22723,TMGDFN,1,IDX,0)),"^",4)
        . . . . IF REASON="INJ ONLY" QUIT
        . . . . IF REASON["PROTIME" QUIT
        . . . . IF REASON["SUTRE" QUIT
        . . . . SET TMGRESULT(TMGDFN,DTIDX)=""
        . . . . SET NUMOFAPPTS=NUMOFAPPTS+1
        QUIT NUMOFAPPTS
        ;"
APPTREMS(TMGRESULT,REMARR,BEGDT,ENDDT)  ;"
        ;"Purpose: get all reminders, sent by REMARR that are due for
        ;"         scheduled patients. 
        ;"   note: reminder status is calculated by date of appointment
        ;"Input: TMGRESULT - result array
        ;"              format TMGRESULT(0)=# of results
        ;"                     TMGRESULT(REMINDER IEN,DATETIME,TMGDFN)="" , FOR EACH ONE DUE
        ;"        REMARR - Reminders to check
        ;"              format REMARR(IEN of reminder)="name"  (Name optional)
        ;"        BEGDT - beginning date of appointments
        ;"        ENDDT - ending date of appointments (optional, if not
        ;"                defined only appointment for Beginning date will be pulled)
        ;"    
        NEW ARRAY,COUNT
        SET BEGDT=$GET(BEGDT),ENDDT=+$GET(ENDDT)
        IF ENDDT'>0 SET ENDDT=BEGDT
        DO GETSCHED(.ARRAY,BEGDT,ENDDT,"AO")  ;"GET ACTIVE AND OLD BOTH
        NEW RESULT,TMGDFN
        SET TMGDFN=0,COUNT=0
        FOR  SET TMGDFN=$ORDER(ARRAY(TMGDFN)) QUIT:TMGDFN'>0  DO
        . NEW DATE SET DATE=$ORDER(ARRAY(TMGDFN,0))
        . NEW REMIEN SET REMIEN=0
        . FOR  SET REMIEN=$ORDER(REMARR(REMIEN)) QUIT:REMIEN'>0  DO
        . . SET RESULT=$$DOREM(TMGDFN,REMIEN,5,DATE)
        . . IF RESULT["DUE" DO
        . . . SET COUNT=COUNT+1
        . . . SET TMGRESULT($GET(REMIEN),DATE,TMGDFN)=""
        SET TMGRESULT(0)=COUNT
        QUIT
        ;"       
ALLREMS(TMGRESULT,REMARR,ACTIVE)  ;"
        ;"Purpose: get all reminders, sent by REMARR that are due for
        ;"         scheduled patients.
        ;"   note: reminder status is calculated by date of appointment
        ;"Input: TMGRESULT - result array
        ;"              format TMGRESULT(0)=# of results
        ;"                     TMGRESULT(REMINDER IEN,DATETIME,TMGDFN)="" , FOR EACH ONE DUE
        ;"        REMARR - Reminders to check
        ;"              format REMARR(IEN of reminder)="name"  (Name optional)
        ;"        ACTIVE - BOOLEAN (1 OR 0). If 1, check only active
        ;:                 patients, 0 for all
        ;"
        NEW COUNT,NOW,X
        DO NOW^%DTC SET NOW=X
        NEW RESULT,TMGDFN
        SET TMGDFN=0,COUNT=0
        FOR  SET TMGDFN=$ORDER(^DPT(TMGDFN)) QUIT:TMGDFN'>0  DO
        . IF (ACTIVE=1)&($$ACTIVEPT^TMGPXR03(TMGDFN,3)<1) QUIT
        . NEW REMIEN SET REMIEN=0
        . FOR  SET REMIEN=$ORDER(REMARR(REMIEN)) QUIT:REMIEN'>0  DO
        . . SET RESULT=$$DOREM(TMGDFN,REMIEN,5,NOW)
        . . IF RESULT["DUE" DO
        . . . SET COUNT=COUNT+1
        . . . SET TMGRESULT($GET(REMIEN),$P(RESULT,"^",2),TMGDFN)=RESULT
        SET TMGRESULT(0)=COUNT
        QUIT
        ;"
GTLABSOR(TMGRESULT,TMGDFN)  ;"
        ;"Purpose: This will be called from the RPC TMG GET ORDERED LABS
        ;"         It will return a string of "^" delimited lab tests to be 
        ;"         autochecked when the lab order dialog is opened.
        SET TMGRESULT="" 
        ;"Format of HFLIST is: HFLIST(IEN of HF)=Item text to check in Lab Order 
        ;"NEW HFLIST
        ;"SET HFLIST(2374)="HgbA1c"
        ;"
        NEW HFIEN SET HFIEN=0
        ;"FOR  SET HFIEN=$O(HFLIST(HFIEN)) QUIT:HFIEN'>0  DO
        FOR  SET HFIEN=$O(^TMG(22740,"B",HFIEN)) QUIT:HFIEN'>0  DO
        . NEW DATE SET DATE=0
        . NEW FOUND SET FOUND=0
        . FOR  SET DATE=$ORDER(^AUPNVHF("AA",TMGDFN,HFIEN,DATE)) QUIT:(DATE'>0)!(FOUND=1)  DO
        . . NEW THISDATE SET THISDATE=9999999-DATE
        . . IF THISDATE=$$TODAY^TMGDATE DO
        . . . IF TMGRESULT'="" SET TMGRESULT=TMGRESULT_"^"
        . . . ;"SET TMGRESULT=TMGRESULT_$G(HFLIST(HFIEN))
        . . . NEW IEN SET IEN=$O(^TMG(22740,"B",HFIEN,0))
        . . . NEW ZN SET ZN=$G(^TMG(22740,IEN,0))
        . . . SET TMGRESULT=TMGRESULT_$P(ZN,"^",2)_":"_$P(ZN,"^",3)
        . . . SET FOUND=1
        IF TMGRESULT="" SET TMGRESULT="NONE"
        QUIT
        ;"
COLODUE(TMGDFN,TEST,DATE,DATA,TEXT)  ;"IS COLONOSCOPY DUE FOR PATIENT
        SET TEST=1,DATE=$$TODAY^TMGDATE() ;"TURN ON BY DEFAULT
        NEW MRD SET MRD=$$MRDFN(TMGDFN,745)
        IF MRD=0 QUIT  ;"leaves frequency at baseline
        NEW FACTORS DO LOADHFAR("TMG COLONOSCOPY FU",.FACTORS)
        NEW NAME SET NAME=$$MRFNDN(TMGDFN,.FACTORS) ;"Name of most recent finding.
        IF NAME="" SET NAME="TMG COLONOSCOPY FU 10 YR"
        NEW YR,MO,DAY DO INTRVLST^TMGPXRF1(NAME,4,.YR,.MO,.DAY) ;"FU interval from string
        NEW FREQ SET FREQ=$SELECT((YR>0):YR_"Y",(MO>0):MO_"M",(DAY>0):DAY_"D",1:"")
        NEW DUETF SET DUETF=0
        IF FREQ["Y" DO
        . SET DUETF=+$G(FREQ)*365
        ELSE  IF FREQ["M" DO
        . SET DUETF=+$G(FREQ)*30
        IF DUETF=0 QUIT
        NEW DUEDATE,X,X1,X2
        SET DUETF=DUETF-120  ;"6/28/21 - ADDED TO ACCOUNT FOR 4M ADV TIMEFRAME TO MIRROR COLONOSCOPY
        SET X1=MRD,X2=+$GET(DUETF)
        DO C^%DTC
        SET DUEDATE=X
        IF $$TODAY^TMGDATE<DUEDATE DO
        . SET TEST=0
        . SET DATE=0
       	QUIT
        ;"this method won't work because you can't run a reminder inside
        ;"   another reminder
        ;"NEW REMIEN,RESULT
        ;"SET REMIEN=218
        ;"SET RESULT=$$DOREM(TMGDFN,REMIEN,5,$$TODAY^TMGDATE)              
        ;"IF RESULT["DUE" DO
        ;". SET TEST=1
        ;". SET DATE=$$TODAY^TMGDATE
        ;"QUIT
        ;"
FRAILDUE(TMGDFN,TEST,DATE,DATA,TEXT)  ;"IS FRAILITY DUE FOR PATIENT
        SET TEST=1,DATE=$$TODAY^TMGDATE() ;"TURN ON BY DEFAULT
        NEW HFIEN,DATE
        SET HFIEN=0
        NEW MRD SET MRD=0
        FOR  SET HFIEN=$ORDER(^AUTTHF("AC",2730,HFIEN)) QUIT:HFIEN'>0  DO
        . SET DATE=0
        . FOR  SET DATE=$ORDER(^AUPNVHF("AA",TMGDFN,HFIEN,DATE)) QUIT:DATE'>0  DO
        . . NEW THISDATE SET THISDATE=9999999-DATE
        . . IF THISDATE>MRD SET MRD=THISDATE
        NEW BDATE SET BDATE=$P($$FIRSTYR^TMGDATE,".",1)
        IF BDATE<MRD DO
        . SET TEST=0
        . SET DATE=0
        QUIT
        ;"
MRDFN(TMGDFN,HFIEN)  ;"Return most recent date of given health factor
        NEW TMGDATE SET TMGDATE=0
        SET TMGDATE=$ORDER(^AUPNVHF("AA",TMGDFN,HFIEN,TMGDATE))
        IF TMGDATE>0 SET TMGDATE=9999999-TMGDATE
        QUIT TMGDATE

        ;"
LOADHFAR(PREFIX,ARR) ;
        ;"Purpose: Load array with up all health factors that start
        NEW PRE2 SET PRE2=$EXTRACT(PREFIX,1,$LENGTH(PREFIX)-1)
        SET PRE2=PRE2_$CHAR($ASCII($EXTRACT(PREFIX,$LENGTH(PREFIX)))-1)
        NEW NAME SET NAME=PRE2
        NEW DONE SET DONE=0
        FOR  SET NAME=$ORDER(^AUTTHF("B",NAME)) QUIT:(DONE=1)!(NAME="")  DO
        . IF $EXTRACT(NAME,1,$LENGTH(PREFIX))'=PREFIX SET DONE=1 QUIT
        . NEW IDX SET IDX=0 FOR  SET IDX=$ORDER(^AUTTHF("B",NAME,IDX)) Q:IDX'>0  DO
        . . NEW STR SET STR=$PIECE($GET(^AUTTHF(IDX,0)),"^",1) QUIT:STR=""
        . . SET ARR(STR)=IDX
        QUIT
MRFNDN(TMGDFN,HFARRAY)  ;"FIND MOST RECENT HF NAME
        NEW DATE SET DATE=0
        NEW TMGRESULT SET TMGRESULT=""
        NEW HFNAME SET HFNAME=""
        FOR  SET HFNAME=$O(HFARRAY(HFNAME)) QUIT:HFNAME=""  DO
        . NEW HFIEN SET HFIEN=$G(HFARRAY(HFNAME))
        . NEW TEMPDATE SET TEMPDATE=$$MRDFN(TMGDFN,HFIEN)
        . IF TEMPDATE>DATE DO
        . . SET TMGRESULT=HFNAME
        . . SET DATE=TEMPDATE
        QUIT TMGRESULT
        ;"
WTTABLE(TMGDFN,OUTARRAY)  ;"
  NEW TMGRESULT SET TMGRESULT=""
  NEW RESULTARR
  ;"
  NEW Y1VAL,Y2VAL,Y3VAL,RECENTVAL
  NEW Y1,Y2,Y3
  SET Y1=$E($$TODAY^TMGDATE,1,3)+1700
  SET Y2=Y1-1
  SET Y3=Y1-2
  SET (Y1VAL,Y2VAL,Y3VAL,RECENTVAL)=0
  ;"
  NEW TIUVIT,TIUVT,TIUVDT,TIUVDA,TIUY,VDT,TIUI,TIUCWRAP,TIUMAXW,TIUVITC
  NEW TIUVCNT,TIUVCNT2,TIUVDATE,TIUY1,TIUVTEMP,CONV
  NEW LVDT SET LVDT=0
  SET TIUVITC="WT"
  DO VITALS^TIULO(.TIUVIT,TMGDFN,TIUVITC,"","",1000)
  SET (TIUVDT,TIUVDONE,TIUVCNT)=0
  FOR  SET TIUVDT=$O(TIUVIT(TIUVITC,TIUVDT)) QUIT:+TIUVDT'>0!TIUVDONE  DO
  . SET TIUVDA=0
  . FOR  SET TIUVDA=$O(TIUVIT(TIUVITC,TIUVDT,TIUVDA)) Q:+TIUVDA'>0  DO
  . . SET TIUVDATE=TIUVDT,TIUVCNT=TIUVCNT+1
  . . SET TIUVTEMP=$G(TIUVIT(TIUVITC,TIUVDT,TIUVDA))
  . . SET VDT=$$DATE^TIULS($P(TIUVTEMP,U,1),"MM/DD/CCYY")
  . . IF VDT=LVDT QUIT
  . . NEW THISYEAR SET THISYEAR=$P(VDT,"/",3)
  . . SET LVDT=VDT
  . . SET TIUY=$P(TIUVTEMP,U,8)
  . . QUIT:+TIUY'>0
  . . SET CONV=$J((+TIUY/2.2),3,1)
  . . SET TIUY=TIUY_" lb "
  . . SET TIUY=TIUY_"("_VDT_")"
  . . IF RECENTVAL=0 SET RECENTVAL=TIUY
  . . IF THISYEAR=Y1 SET Y1VAL=TIUY
  . . IF THISYEAR=Y2 SET Y2VAL=TIUY
  . . IF THISYEAR=Y3 SET Y3VAL=TIUY
  . . ;"IF TMGRESULT'="" SET TMGRESULT=TMGRESULT_", "
  . . ;"SET TMGRESULT=TMGRESULT_TIUY
  ;"SET TMGRESULT="Weight = "_TMGRESULT
  SET TMGRESULT="First "_Y3_" weight: "_Y3VAL_$C(13)
  SET TMGRESULT=TMGRESULT_"First "_Y2_" weight: "_Y2VAL_$C(13)
  SET TMGRESULT=TMGRESULT_"First "_Y1_" weight: "_Y1VAL_$C(13)
  SET TMGRESULT=TMGRESULT_"Most recent weight: "_RECENTVAL
  QUIT TMGRESULT
  ;"
