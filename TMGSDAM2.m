TMGSDAM2 ;TMG/kst/ENHANCED MAKE AN APPOINTMENT SDAPI ;1/9/09, 2/2/14
         ;;1.0;TMG-LIB;**1**;1/9/09
 ;
 ;"NOTE: Original header:
 ;"SDVWMKPI        ;ENHANCED MAKE AN APPOINTMENT SDAPI 11/18/06
 ;"       ;VWSD*3.2;;;;;Build 8
 ;"(Moved to this namespace for customization/alteration)
 ;
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"Called into from TMGRPC5
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"EN(DFN,APPTDATE,SC,STYP,SDARRAY) -- MAKE AN APPOINTMENT
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"CHKAVAIL(SAVE,APPTDATE)
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;
EN(DFN,APPTDATE,SC,STYP,SDARRAY)     ;
        ;"Purpose: MAKE AN APPOINTMENT
        ;"INPUT: DFN -- PATIENT IEN (REQUIRED)
        ;"       APPTDATE -- APPOINTMENT DATE (REQUIRED) -- FM format
        ;"       SC -- IEN of CLINIC FOR APPOINTMENT (REQUIRED) (file 44)
        ;"       STYP (REQUIRED)
        ;"            =1 C&P
        ;"            =2 10-10
        ;"            =3 SCHEDULED APPOINTMENT
        ;"            =4 UNSCHEDULED VISIT
        ;"       SDARRAY -- PASS BY REFERENCE.  Format:
        ;"         SDARRAY("DATE NOW") (REQ AT TIME REQUEST MADE) (OPTIONAL.  Defaults to NOW is not provide)
        ;"         SDARRAY("LAB DATE TIME ASSOCIATED") = "" OR DATE/TIME     (OPTIONAL)
        ;"         SDARRAY("X-RAY DATE TIME ASSOCIATED") = "" OR DATE/TIME   (OPTIONAL)
        ;"         SDARRAY("EKG DATE TIME ASSOCIATED") = "" OR DATE/TIME     (OPTIONAL)
        ;"         SDARRAY("APPT TYPE") = 9   (REQUIRED)
        ;"                9 for REGULAR APPOINTMENT TYPE   (ptr 409.1)
        ;"         SDARRAY("APPT SUB-CATEGORY") = "0" (NOT USED)
        ;"                "0" for none   (ptr 35.2)
        ;"         SDARRAY("SCHED_REQ_TYPE")='O' (REQUIRED)
        ;"                'O' FOR OTHER THAN 'NEXT AVA.' APPT.;  (set of codes)
        ;"                --I think this is for File 2, field 1900, subfield 25
        ;"                 N:'NEXT AVAILABLE' APPT.
        ;"                 C:OTHER THAN 'NEXT AVA.' (CLINICIAN REQ.)
        ;"                 P:OTHER THAN 'NEXT AVA.' (PATIENT REQ.)
        ;"                 W:WALKIN APPT.
        ;"                 M:MULTIPLE APPT. BOOKING
        ;"                 A:AUTO REBOOK
        ;"                 O:OTHER THAN 'NEXT AVA.' APPT.
        ;"         SDARRAY("NEXT APPT IND")=0 (REQUIRED)  (0 FOR NO)
        ;"         SDARRAY("DESIRED DATE TIME OF APPT")=APPTDATE (OPTIONAL)
        ;"         SDARRAY("FOLLOWUP VISIT INDICATOR")= (REQUIRED)
        ;"                 "0" FOR NO
        ;"                 "1" FOR YES
        ;"         SDARRAY("X RAY DATA FREE TEXT")=  (OPTIONAL)
        ;"         SDARRAY("OTHER DATA FREE TEXT")=  (OPTIONAL)
        ;"         SDARRAY("OTHER WARD LOCATION")=   (OPTIONAL)
        ;"         SDARRAY("DATA ENTRY CLERK")=      (DEFAULTS TO DUZ IF NOT PROVIDED)
        ;"                 DUZ OR NEW PERSON (FILE 200) PTR
        ;"         SDARRAY("PRIOR XRAY RESULTS TO CLINIC")= (OPTIONAL)
        ;"                  "Y" OR ""
        ;"         SDARRAY("CHECKED-IN DATE")=  (OPTIONAL)
        ;"               "" OR DATE APPOINTMENT MADE
        ;"               FOR AN UNSCHEDULED VISIT
        ;"
        ;"XQSDVWSI ; EXIST AS NON-INTERACTIVE SILENT NODE W/O WRITE FOR XQOR ROUTINES
        ;"SDVWNVAI ; EXIST AS NON-VA RELATED PFSS EVENT MODE
        ;"                            = "D" DISABLING THE NEED FOR ICN
        ;"                            = "O" AS OTHER NON-VA ICN SYSTEM ( FUTURE)
        ;"
        ;"RESULTS: 1 = OK,APPOINTMENT SUCCESSFULLY MADE
        ;"         NEG NUMBER= ERROR
        ;"          -101 INVALID PATIENT DFN
        ;"          -102 INVALID HOSPITAL LOCATION IEN (SC)
        ;"          -103 APPTDATE < DATE NOW
        ;"          -104 INVALID STYP or MODE
        ;"          // (removed) -105 IF $GET(SDARRAY("DATE NOW"))=""
        ;"          -106 IF $GET(SDARRAY("APPT TYPE"))=""
        ;"          -107 IF $GET(SDARRAY("SCHED_REQ_TYPE"))=""
        ;"          -108 IF $GET(SDARRAY("NEXT APPT IND"))=""
        ;"          // (removed) -109 IF $GET(SDARRAY("DATA ENTRY CLERK"))=""
        ;"          -110 IF $GET(SDARRAY("FOLLOWUP VISIT INDICATOR")=""
        ;"          -111 NO SCHEDULED SLOT WHERE SCHED APPT IS WANTED
        ;
        N TIMEDD
        N SDCL,SDT,SDDA,SDMODE,SDORG
        N CNT,SDY,DAYW,NDOW,TMG1DATE,FOUND,VAL,MULTM,START,INCRM
        N APTTIME,OV2,VAL2
        N SDSL,SL,SDSDATE,STARTDAY,SDHDL,SDEMP,SDMKHDL,SDMADE,SDLOCK,SDAPTYP,SDCOL
        N TMGRESULT SET TMGRESULT=1 ;"Default to success
        N TMGMSG
        ;
        N PURVISIT,SAVENOW,OVERBOOK,ELIGIB,OVERBOKM
        ;
        ;"VALIDATE DFN, SC AS VALID PATIENTS AND CLINIC
        IF '$D(^DPT(DFN,0)) SET TMGRESULT="-101^INVALID PATIENT DFN" GOTO ENDONE
        IF '$D(^SC(SC,0)) SET TMGRESULT="-102^INVALID HOSPITAL LOCATION IEN" GOTO ENDONE
        ;
        ;"CHECK DATE>=NOW
        IF $GET(SDARRAY("DATE NOW"))="" SET SDARRAY("DATE NOW")="NOW"
        SET SDARRAY("DATE NOW")=$$E2IDATE(SDARRAY("DATE NOW"))
        SET (SAVENOW,X)=SDARRAY("DATE NOW")
        IF APPTDATE<SAVENOW SET TMGRESULT="-103^APPTDATE < DATE NOW" GOTO ENDONE
        IF STYP=4 SET APPTDATE=X ;"If unscheduled visit, force date to NOW
        ;"FORMAT APPTDATE BELOW SHOULD BE FOUND IN NODE BELOW
        IF $GET(^SC(SC,"S",APPTDATE,0))=APPTDATE DO
        . SET CNT=0
        . FOR  SET CNT=$ORDER(^SC(SC,"S",APPTDATE,1,CNT)) Q:CNT=""  DO
        . . SET SDY=CNT+1
        ELSE  DO
        . SET SDY=1
        IF (STYP<1)!(STYP>4) SET TMGRESULT="-104^INVALID STYP or MODE" GOTO ENDONE
        ;
        IF $GET(SDARRAY("DATA ENTRY CLERK"))="" DO
        . SET SDARRAY("DATA ENTRY CLERK")=DUZ
        . ;"SET TMGRESULT=-109 GOTO ENDONE
        ;
        ;"CHECK OTHER REQUIRED VARIABLES
        IF $GET(SDARRAY("APPT TYPE"))="" DO  GOTO ENDONE
        . SET TMGRESULT="-106^APPT TYPE NOT SPECIFIED"
        IF $GET(SDARRAY("SCHED_REQ_TYPE"))="" DO  GOTO ENDONE
        . SET TMGRESULT="-107^SCHED REQ TYPE NOT SPECIFIED"
        IF $GET(SDARRAY("NEXT APPT IND"))="" DO  GOTO ENDONE
        . SET TMGRESULT="-108^NEXT APPT IND NOT SPECIFIED"
        IF $GET(SDARRAY("FOLLOWUP VISIT INDICATOR"))="" DO  GOTO ENDONE
        . SET TMGRESULT="-110^FOLLOWUP VISIT INDICATOR NOT SPECIFIED"
        ;
        ;"ENSURE EXISTANCE OF VARIABLES / CONVERT DATES IF NEEDED
        SET SDARRAY("CHECKED-IN DATE")=$$E2IDATE($GET(SDARRAY("CHECKED-IN DATE")))
        SET SDARRAY("LAB DATE TIME ASSOCIATED")=$$E2IDATE($GET(SDARRAY("LAB DATE TIME ASSOCIATED")))
        SET SDARRAY("X-RAY DATE TIME ASSOCIATED")=$$E2IDATE($GET(SDARRAY("X-RAY DATE TIME ASSOCIATED")))
        SET SDARRAY("EKG DATE TIME ASSOCIATED")=$$E2IDATE($GET(SDARRAY("EKG DATE TIME ASSOCIATED")))
        SET SDARRAY("APPT TYPE")=$GET(SDARRAY("APPT TYPE"))
        SET SDARRAY("SCHED_REQ_TYPE")=$GET(SDARRAY("SCHED_REQ_TYPE"))
        SET SDARRAY("NEXT APPT IND")=$GET(SDARRAY("NEXT APPT IND"))
        SET SDARRAY("DESIRED DATE TIME OF APPT")=$$E2IDATE($GET(SDARRAY("DESIRED DATE TIME OF APPT")))
        SET SDARRAY("FOLLOWUP VISIT INDICATOR")=$GET(SDARRAY("FOLLOWUP VISIT INDICATOR"))
        SET SDARRAY("FOLLOWUP VISIT INDICATOR")=$GET(SDARRAY("FOLLOWUP VISIT INDICATOR"))
        ;
        SET OV2=0
        IF STYP'=4 DO  ;"i.e. 1 (C&P), 2 (10-10), or 3 (SCHEDULED APPOINTMENT)
        . ;"BEFORE MAKE APPT
        . ;"THIS MAY ALSO DO CHECKIN AN APPOINTMENT
        . ;"ALSO NEED TO CHECK AGAINST SCHEDULE FOR THAT DAY
        . ;"DETERMINE LAST RELATIVE ENTRY # FOR
        . ;"THIS APPOINTMENT DATE (IF ANY) ON THIS CLINIC
        . ;
        . ;"TO SEE IF OVERBOOK MAX ACHIEVED OR APPOINTMENT NOT AVAILABLE
        . ;"FOR THAT TIME AND DATE.
        . ;"GET DATE
        . SET APTTIME=$P(APPTDATE,".",2) ;"get just time
        . FOR  Q:$L(APTTIME)>3  SET APTTIME=APTTIME_"0"  ;"PAD OUT TIME TO 4 DIGITS
        . ;"CHECK WHAT MULTIPLE OF DAY OF WEEK FOR APPOINTMENT START
        . ;"GET DAY OF WEEK
        . SET X=APPTDATE DO DW^%DTC IF X="" QUIT  ;"Must be invalid date
        . SET DAYW=$E(X,1,2)  ;"e.g. MO for MONDAY
        . SET DOW=$$DOW^XLFDT(APPTDATE,1) ;"DOW=Day of Week (0-6)
        . ;"FIND DAY OF WEEK ENTRY IN "ST" MULT , THEN FIND STARTING TIME AND
        . ;"TIME MATCH IN SAME "T" MULT.
        . SET TMG1DATE=APPTDATE\1
        . SET FOUND=0
        . FOR  DO  SET TMG1DATE=$ORDER(^SC(SC,"ST",TMG1DATE),-1) QUIT:(+TMG1DATE'>0)!(FOUND=1)
        . . SET VAL=$GET(^SC(SC,"ST",TMG1DATE,1)) ;" e.g. MO 05  |   [1 1|0 1 1 1|1 1 1 1|1 1] "
        . . IF ($E(VAL,1,2)'=DAYW) QUIT  ;"Skip IF null, or not matching day of week
        . . IF $$CHKAVAIL(TMG1DATE,APTTIME)=0 QUIT
        . . SET FOUND=1
        . . SET INCR=0
        . . ;"Now find T node that applies to APPTDATE
        . . NEW TEMPDATE SET TEMPDATE=TMG1DATE
        . . FOR  DO  QUIT:(OV2=1)  SET TEMPDATE=$ORDER(^SC(SC,"T",TEMPDATE),-1) QUIT:(+TEMPDATE'>0)
        . . . IF $DATA(^SC(SC,"T",TEMPDATE))=0 QUIT
        . . . IF $$DOW^XLFDT(TEMPDATE,1)'=DOW QUIT
        . . . FOR  SET INCR=$ORDER(^SC(SC,"T",TEMPDATE,2,INCR)) QUIT:(INCR="")!(OV2=1)  DO
        . . . . SET VAL2=$GET(^SC(SC,"T",TEMPDATE,2,INCR,0)) ;"e.g. 0830^1
        . . . . SET VAL2=$PIECE(VAL2,"^",1)
        . . . . IF VAL2=APTTIME SET OV2=1
        . IF OV2=1 DO
        . . ;"NOW CHECK IN "S" ARRAY TO SEE IF APPT ALREADY MADE
        . . IF SDY'=1 DO  ;"Should this be SDY>1 DO  ??
        . . . ;"CHECK IF OVERBOOKS ALLOWED
        . . . IF $PIECE($GET(^SC(SC,"SL")),"^",7)=0 SET OV2=0
        ELSE  DO  ;"i.e. STYP=4 UNSCHEDULED VISIT
        . ;"ALSO
        . SET OV2=1
        ;
        IF OV2'=1 DO  GOTO ENDONE
        . SET TMGRESULT="-111^NO SCHEDULED SLOT WHERE SCHED APPT IS WANTED"
        ;
        SET SDCL=SC
        SET SDT=APPTDATE
        SET SDDA=SDY
        SET SDMODE=2
        SET SDORG=1
        ;"ADDITIONAL
        SET SL=$P(^SC(SC,"SL"),"^",1)
        SET SDSDATE=APPTDATE
        ;"SET STARTDAY
        ;
        ;"START PREPARING DATA FROM SDARRAY INTO NODES
        ;
        ;"FIRST INITIAL TOP NODE FOR APPOINTMENT SUB-FILE
        SET ^DPT(DFN,"S",0)="^2.98P^^"
        LOCK +^DPT(DFN,"S",0):5
        ;"NEXT NODE 0
        SET PURVISIT=STYP
        SET TIMEDD=$P(APPTDATE,".",1)
        NEW TEMPS
        SET TEMPS=SC_"^^"_SDARRAY("LAB DATE TIME ASSOCIATED")_"^"
        SET TEMPS=TEMPS_SDARRAY("X-RAY DATE TIME ASSOCIATED")_"^"
        SET TEMPS=TEMPS_SDARRAY("EKG DATE TIME ASSOCIATED")
        SET ^DPT(DFN,"S",APPTDATE,0)=TEMPS
        SET TEMPS=^DPT(DFN,"S",APPTDATE,0)_"^^"_PURVISIT
        SET TEMPS=TEMPS_"^^^^^^^^^"_SDARRAY("APPT TYPE")_"^^^"_TIMEDD_"^^^^^"_0
        SET ^DPT(DFN,"S",APPTDATE,0)=TEMPS
        SET TEMPS=^DPT(DFN,"S",APPTDATE,0)_"^"_SDARRAY("SCHED_REQ_TYPE")_"^"
        SET TEMPS=TEMPS_SDARRAY("NEXT APPT IND")
        SET ^DPT(DFN,"S",APPTDATE,0)=TEMPS
        ;"NEXT NODE 1
        IF SDARRAY("DESIRED DATE TIME OF APPT")'="" DO
        . SET ^DPT(DFN,"S",APPTDATE,1)=SDARRAY("DESIRED DATE TIME OF APPT")_"^"_SDARRAY("FOLLOWUP VISIT INDICATOR")
        ELSE  DO
        . SET ^DPT(DFN,"S",APPTDATE,1)=TIMEDD_"^"_SDARRAY("FOLLOWUP VISIT INDICATOR")
        LOCK -^DPT(DFN,"S",0)
        ;"NOW FILE 44 MULTIPLE IN APPOINTMENT SUB-FILE
        ;"FIRST  TOP NODE IN CLINIC FOR DATE
        SET ^SC(SC,"S",0)="^44.001DA^^"
        LOCK +^SC(SC,"S",0):5
        ;"NEXT DATE MULTIPLE
        SET ^SC(SC,"S",APPTDATE,0)=APPTDATE
        ;"NEXT TOP NODE UNDER DATE FOR PATIENT
        SET ^SC(SC,"S",APPTDATE,1,0)="^44.003PA^^"
        ;"NEXT MULTIPLE ENTRY PER PATIENT
        SET ^SC(SC,"S",APPTDATE,1,SDY,0)=DFN_"^"_SL_"^"_$GET(SDARRAY("X RAY DATA FREE TEXT"))_"^"_$GET(SDARRAY("OTHER DATA FREE TEXT"))_"^"_$GET(SDARRAY("OTHER WARD LOCATION"))
        SET ^SC(SC,"S",APPTDATE,1,SDY,0)=^SC(SC,"S",APPTDATE,1,SDY,0)_"^"_$GET(SDARRAY("DATA ENTRY CLERK"))_"^"_SAVENOW
        IF STYP=4 SET ^SC(SC,"S",APPTDATE,1,SDY,0)=^SC(SC,"S",APPTDATE,1,SDY,0)_"^"_$GET(SDARRAY("PRIOR X-RAY RESULTS TO CLINIC"))
        ;
        ;"Change PATTERN ("ST") nodes to reflext currently available slots.
        IF $$ENSUR1ST^TMGSDAU(SC,APPTDATE,.TMGMSG)'=1 DO  ;"Create ST node, IF doesn't exist
        . SET TMGRESULT="-200^"_$GET(TMGMSG(1)) KILL TMGMSG
        IF $$DEC1SLOT^TMGSDAU(SC,APPTDATE,.TMGMSG)=-1 DO  ;"Don't QUIT, appt was made-->so trigger event
        . SET TMGRESULT="-200^Error updating PATTERN (ST nodes).  "_$GET(TMGMSG(1))
        . KILL TMGMSG
        ;
        ;"DETERMINE ANY OVERBOOK AND ELIGIBILITY HERE
        SET OVERBOKM=$P(^SC(SC,"SL"),"^",7)
        IF SDY>OVERBOKM DO
        . SET OVERBOOK="O"
        . SET ^SC(SC,"S",APPTDATE,1,SDY,"OB")="0"
        ELSE  DO
        . SET OVERBOOK=""
        ;"ELIGIBILITY NEXT
        DO ELIG^VADPT SET ELIGIB=$P(VAEL(1),"^",1)
        IF STYP=4 SET ^SC(SC,"S",APPTDATE,1,SDY,0)=^SC(SC,"S",APPTDATE,1,SDY,0)_"^"_"^"_"^"_ELIGIB
        ;"NOW UNSCHEDULED VISITS EXTRA DATA
        ;"REALLY LATER MAY NEED ELIGIBILITY FOR NON-VA SYSTEMS PATIENTS WITH
        ;"SCHEDULED APPTS AND UNSCHEDULED VISITS ( HUMANITARIAN, REIMBURSABLE INSURANCE, ETC)
        IF STYP=4 DO
        .SET ^SC(SC,"S",APPTDATE,1,SDY,"C")=SD_"^"_$GET(SDARRAY("DATA ENTRY CLERK"))_"^^^"_SD
        LOCK -^SC(SC,"S",0)
        ;"EVENT GENERATION ALSO FOR PFSS SYSTEM WHICH CAN BE USED WITH AN EXTERNAL SCHEDULING SYSTEM
        ;"FOR MAKE APPT EVENTS AS WELL AS CHECKIN,CHECKOUT,CANCEL,DELETE, AND OUTPATIENT ENCOUNTER DATA
        DO MAKE^SDAMEVT(DFN,SDT,SDCL,SDDA,SDMODE)
ENDONE  ;
        QUIT TMGRESULT
        ;
        ;
CHKAVAIL(TMG1DATE,APTTIME)   ;
        ;"Purpose: CHECK IF SLOT ALLOWED FOR THAT DAY/TIME SLOT
        ;"Input TMG1DATE -- DATE in FM format to check on.
        ;"      APTTIME -- Time of appt, in military format, e.g. '1345'
        ;"Globally Scoped Vars used: SC - IEN in 44
        ;"Results: 0 -- not available
        ;"         1 -- available
        ;
        N SL,VAL,DATE,STARTTIM,SL,POS,COUNT
        NEW CODES SET CODES="{}&%?#@!$* XXWVUTSRQPONMLKJIHGFEDCBA0123456789jklmnopqrstuvwxyz"
        ;
        SET SL=$P($GET(^SC(SC,"SL")),"^",1) ;"LENGTH OF APPT
        SET VAL=$GET(^SC(SC,"ST",TMG1DATE,1))   ;" e.g. 'MO 05  |   [1 1|0 1 1 1|1 1 1 1|1 1] '
        ;"START AT 9TH PIECE TO SEE IF NON-BLANK
        ;"FORMAT DATE+STARTTIME
        SET STARTTIM=$P($GET(^SC(SC,"SL")),"^",3) ;"DISPLAY START TIME
        IF $L(STARTTIM)=1 SET STARTTIM="0"_STARTTIM
        FOR  Q:$L(STARTTIM)>3  SET STARTTIM=STARTTIM_"0" ;"PAD OUT TIME TO 4 DIGITS
        FOR  Q:$L(APTTIME)>3  SET APTTIME=APTTIME_"0"    ;"PAD OUT TIME TO 4 DIGITS
        ;"SET DIFF=APTTIME-STARTTIM
        SET DIFF=$$MILDELTA^TMGSDAU1(STARTTIM,APTTIME)
        ;"SET SL=$P(^SC(SC,"SL"),"^",1)
        SET COUNT=DIFF/SL
        SET POS=9+(2*COUNT)
        ;"IF $E(VAL,POS,POS)'=" " Q 1
        NEW CH SET CH=$E(VAL,POS,POS)
        NEW NUMAVAIL SET NUMAVAIL=$FIND(CODES,CH)-$FIND(CODES,"0")
        IF NUMAVAIL>0 QUIT 1
        QUIT 0
        ;
E2IDATE(TMGDATE)
        ;"Purpose: To return a FM-format Date from TMGDATE, converting IF needed.
        ;"Input: TMGDATE: A date in external format, or FM-Date format
        ;"              Note: IF date is invalid, then "" is returned.
        NEW TMGRESULT SET TMGRESULT=$GET(TMGDATE)
        IF (TMGRESULT'=""),(+TMGRESULT'=TMGRESULT) DO
        . NEW MSG
        . DO DT^DILF("T",TMGDATE,.MSG)
        . SET TMGRESULT=MSG
        QUIT TMGRESULT
        ;