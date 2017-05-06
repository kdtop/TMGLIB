TMGPXR03 ;TMG/kst/TMG Reminder Reports stuff ;4/18/13, 2/2/14
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
 ;"ACTIVEPT(DFN,WINDOW)-- Is patient active?
 ;"RUNRPT(RESULTARR,PTARRAY,IEN,DATE,SHOWPROG,WANTNA)  ;Prints reminder report to specified device
 ;"DOREM(DFN,PXRMITEM,PXRHM,DATE,WANTNA) -- CODE MODIFIED FROM ^PXRMDEV
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
 ;"ACTIVEPT(DFN) -- ACTIVE PATIENT
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
        ;"Input: ARRAY --  Format: ARRAY(STATUS,DFN)=Status^DueDate^LastGiven  
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
        ;"           Format: OUTARRY(DFN)=""
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
        NEW DFN SET DFN=0
        FOR  SET DFN=$ORDER(^DPT(DFN)) QUIT:(+DFN'>0)  DO
        . SET OUTARRAY(DFN)=""
        . IF DFN#5=0 DO
        . . DO PROGBAR^TMGUSRI2(DFN,"PROGRESS: "_DFN,0,DFNMAX,60,STIME)
        DO PROGBAR^TMGUSRI2(100,"DONE: ",0,100,60,STIME)
        WRITE !        
        QUIT TMGRESULT
        ;
GETACTPTS(OUTARRAY)   ;
        ;"Purpose: Gather list of ACTIVE patients to run report on
        ;"Result: 1^OK or -1^Message if problem
        NEW TMGRESULT SET TMGRESULT="1^OK"
        WRITE "Gathering list of all ACTIVE patients",!
        NEW DFNMAX SET DFNMAX=$ORDER(^DPT("!"),-1)
        NEW STIME SET STIME=$H
        NEW DONE SET DONE=0
        NEW DFN SET DFN=0
        FOR  SET DFN=$ORDER(^DPT(DFN)) QUIT:(+DFN'>0)  DO
        . IF DFN#5=0 DO
        . . DO PROGBAR^TMGUSRI2(DFN,"PROGRESS: "_DFN,0,DFNMAX,60,STIME)
        . IF $$ACTIVEPT(DFN)=0 QUIT
        . SET OUTARRAY(DFN)=""
        DO PROGBAR^TMGUSRI2(100,"DONE: ",0,100,60,STIME)
        WRITE !        
        QUIT TMGRESULT
        ;
GETNTPTS(OUTARRAY) ;
        ;"Purpose: Gather list of patients with progress note between dates, to run report on
        ;"Result: 1^OK or -1^Message if problem
        NEW TMGRESULT SET TMGRESULT="1^OK"
        NEW STARTDT,ENDDT,TIUIEN,DFN
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
        . . SET DFN=+$PIECE($GET(^TIU(8925,TIUIEN,0)),"^",2) QUIT:(+DFN'>0)
        . . SET OUTARRAY(DFN)=""        
GNLDN   QUIT TMGRESULT        
        ;
ACTIVEPT(DFN,WINDOW) ;"ACTIVE PATIENT   **NAME ALERT** --There is also an ACTIVEPT^TMGPXR01() for a different purpose.
        ;"Input: DFN -- the patient IEN
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
        SET DFN=+$GET(DFN)  ;"//kt
        NEW NAME SET NAME=$PIECE($GET(^DPT(DFN,0)),"^",1)  ;"//kt
        IF $EXTRACT(NAME,1,2)="ZZ" GOTO APTDN  ;"//kt
        NEW SDTIME SET SDTIME=$$FMADD^XLFDT(DATE,-365*DIFFYR,0,0,0)
        ;"NEW REVDATE SET REVDATE=9999999-DATE
        ;"NEW REVSDT SET REVSDT=9999999-SDTIME        
        NEW TITLEIEN SET TITLEIEN=0
        NEW FOUND SET FOUND=0
        ;"NOTE: in 2 lines before, once DEATH title, or DISCHARGE title has EVER been
        ;"      added to the record, then it is irrevocable.  If this is a problem, 
        ;"      will have to refine later. 
        IF $DATA(^TIU(8925,"APT",DFN,DIEDTITLE))>0 GOTO APTDN
        IF $DATA(^TIU(8925,"APT",DFN,DCTITLE))>0 GOTO APTDN   ;"ELH added 10-5-15
        NEW XFER,XFERDT,LASTOVDT
        SET XFER=0,XFERDT=0,LASTOVDT=0
        ;"IF $DATA(^TIU(8925,"APT",DFN,XFERTITLE))>0 GOTO APTDN ;"Transfer handled below now ELH
        FOR  SET TITLEIEN=$ORDER(^TIU(8925,"APT",DFN,TITLEIEN)) QUIT:(+TITLEIEN'>0)  DO
        . NEW STATUSIEN SET STATUSIEN=0
        . FOR  SET STATUSIEN=$ORDER(^TIU(8925,"APT",DFN,TITLEIEN,STATUSIEN)) QUIT:(+STATUSIEN'>0)  DO
        . . ;"//kt NEW PREVDT SET PREVDT=$ORDER(^TIU(8925,"APT",DFN,TITLEIEN,STATUSIEN,REVDATE))
        . . ;"//kt IF PREVDT'>REVSDT SET FOUND=PREVDT
        . . NEW REVDT SET REVDT=0 FOR  SET REVDT=$ORDER(^TIU(8925,"APT",DFN,TITLEIEN,STATUSIEN,REVDT)) QUIT:(+REVDT'>0)  DO
        . . . NEW DOCIEN SET DOCIEN=+$ORDER(^TIU(8925,"APT",DFN,TITLEIEN,STATUSIEN,REVDT,""))
        . . . NEW ONEDATE SET ONEDATE=9999999-REVDT
        . . . NEW SKIP SET SKIP=0
        . . . IF TITLEIEN=1411 DO  QUIT:SKIP=1  ;"Check for an automated note that might have occurred long after last appt
        . . . . IF '((ONEDATE\1=3120817)!(ONEDATE\1=3120924)!(ONEDATE\1=3130926)) QUIT
        . . . . SET SKIP=($GET(^TIU(8925,DOCIEN,"TEXT",1,0))["Standardizing notation of")  
        . . . IF TITLEIEN=XFERTITLE DO  ;"TRANSFER DATE STORED FOR TESTING ELH
        . . . . SET XFER=1
        . . . . SET XFERDT=ONEDATE
        . . . IF (TITLEIEN=1408)!(TITLEIEN=1399)!(TITLEIEN=1402) DO
        . . . . IF ONEDATE>LASTOVDT SET LASTOVDT=ONEDATE
        . . . IF FOUND=0 SET FOUND=((ONEDATE'<SDTIME)&(ONEDATE'>DATE))
        IF (XFER=1)&(LASTOVDT>0) DO
        . NEW X1,X2,X
        . SET X1=LASTOVDT,X2=XFERDT
        . DO ^%DTC
        . IF X>30 SET XFER=0
        IF XFER=0 SET TMGRESULT=(FOUND>0)
APTDN   QUIT TMGRESULT        
        ;                
RUNRPT(RESULTARR,PTARRAY,IEN,DATE,SHOWPROG,WANTNA)  ;Prints reminder report to specified device
        ;"Purpose: Runs TESTREM^TMGRPC3G for all patients with given criteria
        ;"Input: RESULTARR - Result Array (See Result)
        ;"       PTARRAY -- format is PTARRAY(DFN)=""
        ;"       IEN - IEN of the Reminder to be run
        ;"       DATE - Test date (internal format)
        ;"       SHOWPROG -- optional.  Default=0, IF 1 then progress bar shown.
        ;"       WANTNA -- optional.  Default=0.  If 1 then patients with N/A results are returned.  Otherwise not. 
        ;"Output: RESULTARR Array containing:
        ;"           Format: RESULTARR(STATUS,DFN)=Status^DueDate^LastGiven  
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
        NEW DFN SET DFN=0
        NEW DFNMAX SET DFNMAX=$ORDER(PTARRAY(""),-1)        
        NEW PATLIST,TMGRESULT
        FOR  SET DFN=$ORDER(PTARRAY(DFN)) QUIT:(+DFN'>0)!DONE  DO
        . IF (SHOWPROG=1),(DFN#5=0) DO
        . . DO PROGBAR^TMGUSRI2(DFN,"PROGRESS: "_DFN,0,DFNMAX,60,STIME)
        . . IF $$USRABORT^TMGUSRI2("scanning patients") SET DONE=1
        . SET TMGRESULT=$$DOREM(DFN,IEN,5,DATE,.WANTNA)
        . ;"IF TMGRESULT="" QUIT  ;"null answer is signal that it was N/A, and N/A not wanted
        . IF TMGRESULT="" SET TMGRESULT="N/A"        
        . ;"SET RESULTARR(DFN)=TMGRESULT
        . NEW STATUS SET STATUS=$PIECE(TMGRESULT,U,1)
        . SET RESULTARR(STATUS,DFN)=TMGRESULT
        IF SHOWPROG DO
        . DO PROGBAR^TMGUSRI2(100,"DONE: ",0,100,60,STIME)
        . WRITE !                
        QUIT
        ;
  ;"=========================================================================
  ;"CODE MODIFIED FROM ^PXRMDEV
  ;"=========================================================================        
DOREM(DFN,PXRMITEM,PXRHM,DATE,WANTNA)         ;"CODE MODIFIED FROM ^PXRMDEV
        ;"Purpose: Do the reminder
        ;"Input: DFN  -- Patient IEN
        ;"       PXRMITEM -- IEN in REMINDER DEFINITION file
        ;"       PXRMHM -- '5'
        ;"       DATE  -- Effective date (FM format)
        ;"       WANTNA -- optional.  Default=0.  If 1 then patients with N/A results are returned.  Otherwise not. 
        N DEFARR,FIEVAL,FINDING,PXRMDEBG,PXRMID,REF,TFIEVAL
        NEW TMGRESULT,VADM,PXRMDEFS
        ;This is a debugging run so SET PXRMDEBG.
        S PXRMDEBG=1
        D DEF^PXRMLDR(PXRMITEM,.DEFARR)
        I +$G(DATE)=0 D EVAL^PXRM(DFN,.DEFARR,PXRHM,1,.FIEVAL)
        I +$G(DATE)>0 D EVAL^PXRM(DFN,.DEFARR,PXRHM,1,.FIEVAL,DATE)
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
        ;"Input: ARRAY --  Format: ARRAY(STATUS,DFN)=Status^DueDate^LastGiven  
        ;"                      Dates are External Format, or 'DUE NOW'
        ;"                      Status is: N/A, DONE, or DUE NOW.
        NEW DFN,MODES
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
        . SET DFN=0
        . FOR  SET DFN=$ORDER(ARRAY(STATUS,DFN)) QUIT:(+DFN'>0)  DO
        . . NEW STR SET STR=$GET(ARRAY(STATUS,DFN)) QUIT:STR=""
        . . NEW STATUS SET STATUS=$PIECE(STR,U,1)
        . . NEW DUE SET DUE=$PIECE(STR,U,2)
        . . NEW LAST SET LAST=$PIECE(STR,U,3) IF LAST="" SET LAST="unknown"
        . . NEW NAME SET NAME=$PIECE($GET(^DPT(DFN,0)),U,1)
        . . NEW DOB SET DOB=$$GET1^DIQ(2,DFN,.03,"E")
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
GETSCHED(TMGRESULT,BEGDT,ENDDT)  ;"
        ;"Purpose: This function returns an array
        ;"         of all appointment inside the given 
        ;"         date range.
        ;"   TMGRESULT(Datetime,DFN)=""
        SET BEGDT=$PIECE($GET(BEGDT),",",1)
        SET ENDDT=+$GET(ENDDT)
        IF ENDDT'>0 SET ENDDT=BEGDT
        SET ENDDT=ENDDT+.999999
        NEW DTIDX SET DTIDX=BEGDT
        FOR  SET DTIDX=$ORDER(^TMG(22723,"DT",DTIDX)) QUIT:(DTIDX>ENDDT)!(DTIDX'>0)  DO
        . NEW DFN SET DFN=0
        . FOR  SET DFN=$ORDER(^TMG(22723,"DT",DTIDX,DFN)) QUIT:DFN'>0  DO
        . . NEW IDX SET IDX=$ORDER(^TMG(22723,"DT",DTIDX,DFN,0))
        . . NEW STATUS SET STATUS=$GET(^TMG(22723,"DT",DTIDX,DFN,IDX))
        . . IF STATUS="A" DO
        . . . SET TMGRESULT(DFN,DTIDX)=""
        QUIT
        ;"
APPTREMS(TMGRESULT,REMARR,BEGDT,ENDDT)  ;"
        ;"Purpose: get all reminders, sent by REMARR that are due for
        ;"         scheduled patients. 
        ;"   note: reminder status is calculated by date of appointment
        ;"Input: TMGRESULT - result array
        ;"              format TMGRESULT(0)=# of results
        ;"                     TMGRESULT(REMINDER IEN,DATETIME,DFN)="" , FOR EACH ONE DUE
        ;"        REMARR - Reminders to check
        ;"              format REMARR(IEN of reminder)="name"  (Name optional)
        ;"        BEGDT - beginning date of appointments
        ;"        ENDDT - ending date of appointments (optional, if not
        ;"                defined only appointment for Beginning date will be pulled)
        ;"    
        NEW ARRAY,COUNT
        SET BEGDT=$GET(BEGDT),ENDDT=+$GET(ENDDT)
        IF ENDDT'>0 SET ENDDT=BEGDT
        DO GETSCHED(.ARRAY,BEGDT,ENDDT)
        NEW RESULT,DFN
        SET DFN=0,COUNT=0
        FOR  SET DFN=$ORDER(ARRAY(DFN)) QUIT:DFN'>0  DO
        . NEW DATE SET DATE=$ORDER(ARRAY(DFN,0))
        . NEW REMIEN SET REMIEN=0
        . FOR  SET REMIEN=$ORDER(REMARR(REMIEN)) QUIT:REMIEN'>0  DO
        . . SET RESULT=$$DOREM(DFN,REMIEN,5,DATE)
        . . IF RESULT["DUE" DO
        . . . SET COUNT=COUNT+1
        . . . SET TMGRESULT($GET(REMIEN),DATE,DFN)=""
        SET TMGRESULT(0)=COUNT
        QUIT
        ;"       

      