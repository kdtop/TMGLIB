TMGSDAU2 ;TMG/kst/Schedule Availability Utilities 2;12/22/08, 2/2/14
         ;;1.0;TMG-LIB;**1**;12/22/08
 ;
 ;"TMG SCHEDULING UTILITIES 2
 ;"Kevin Toppenberg MD
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
 ;"GETDFN(PATIENT) -- return DFN value for patient
 ;"GETCLIEN(CLINIC) - return Clinics IEN value for patient
 ;"GETDATE(APPT) - return a FM Date-time formated value
 ;"FILLAVAL(TMGIEN,PARRAY,TMG1DATE,TMGLIMDT,TMGERR,TMGMSG) --Fill in AVAILABILITY subfile ("T" node)
 ;"KILLAVAL(TMGIEN,TMG1DATE,TMGLIMDT,TMGFLAGS) -- Delete AVAILABILITY ("T") node, and any linked "ST" and "OST" nodes.
 ;"KILL1DATE(TMGIEN,TMG1DATE,FULL) -- remove 1 "T" node, and any linked ST and OST nodes
 ;"STR2PAT(TMGIEN,STR,PARRAY) -- Convert a template pattern into an array of times.
 ;"FRAC2TIM(TIME,HRS,MINS) -- Convert Fractional time --> Hrs & Min e.g. 3.75 --> 3 & 45 (i.e. 3:45)
 ;"CH2NAVAL(CH)-- convert a given availability character into number of slots there.
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;
GETDFN(PATIENT)
        ;"Purpose: return DFN value for patient
        ;"      This is a much simpler function that TMGGDFN, different purpose
        ;"Input: PATIENT.  Either a patient name (must be unique) or IEN, or
        ;"                   SequelMedID#^SEQL
        ;"Results: IEN in PATIENT file, or -101^Message
        ;
        NEW RESULT SET RESULT=0
        SET PATIENT=$GET(PATIENT)
        IF PATIENT="" DO  GOTO GDDONE
        . SET RESULT="-101^Patient not specified."
        IF +PATIENT>0 DO
        . NEW MODE SET MODE=$PIECE(PATIENT,"^",2)
        . IF MODE="" SET MODE="IEN"
        . IF MODE="IEN" SET RESULT=+PATIENT QUIT
        . IF MODE="SEQL" DO  QUIT
        . . SET RESULT=+$ORDER(^DPT("TMGS",+PATIENT,"")) QUIT:RESULT>0
        . . SET RESULT="-101^Unable to find patient SEQL ID#: "_+PATIENT
        ELSE  DO
        . NEW TMG2MSG
        . DO FIND^DIC(2,,".01","MP",ANAME,"*","","","","TMG2MSG")
        . NEW NUM SET NUM=+$GET(TMG2MSG("DILIST",0))
        . IF NUM=0 DO  QUIT
        . . SET RESULT="-101^Patient name: '"_PATIENT_"' NOT FOUND"
        . IF NUM>1 DO  QUIT
        . . SET TMGMSG(TMGMSG)="-101^Name: "_PATIENT_" Not specific.  Multiple patients with this name exist."
        . SET RESULT=+$GET(TMG2MSG("DILIST",1,0))
GDDONE  ;
        QUIT RESULT
        ;
        ;
GETCLIEN(CLINIC)
        ;"Purpose: return Clinics IEN value for patient
        ;"Input: CLINIC -- Name, or IEN, of Clinic for appt (file 44)
        ;"Results: IEN in HOSPITAL LOCATION (44), or -102^Message
        NEW RESULT
        SET CLINIC=$GET(CLINIC)
        IF CLINIC="" DO  GOTO GCLDONE
        . SET RESULT="-102^Clinic location not provided."
        IF +CLINIC=CLINIC SET RESULT=CLINIC
        ELSE  DO
        . NEW DIC,X,Y
        . SET DIC=44,DIC(0)="M"
        . SET X=CLINIC DO ^DIC
        . IF +Y>0 SET RESULT=+Y
        . ELSE  SET RESULT="-102^'"_CLINIC_"' clinic location NOT FOUND."
GCLDONE ;
        QUIT RESULT
        ;
        ;
GETDATE(APPT)
        ;"Purpose: return a FM Date-time formated value
        ;"Input: APPT -- Desired Appointment Date & Time -- External, or FM format
        ;"Results: FM Date-Time entry or -1^Message
        ;
        NEW RESULT
        SET APPT=$GET(APPT)
        IF APPT="" DO  GOTO GAPDONE
        . SET RESULT="-1^Date and time not provided"
        IF +APPT=APPT SET RESULT=APPT
        ELSE  DO
        . DO DT^DILF("T",APPT,.RESULT)
        . IF RESULT=-1 SET RESULT="-1^'"_APPT_"' is not a valid Date-Time"
GAPDONE ;
        QUIT RESULT
        ;
        ;
FILLAVAL(TMGIEN,PARRAY,TMG1DATE,TMGLIMDT,TMGERR,TMGMSG)
        ;"Purpose: Fill in AVAILABILITY subfile ("T" node), specifying number
        ;"         of patients allowed in each slot
        ;"       Note: This creates entries for each slot, 1 for each time slot.
        ;"           The T node does not store an ending date for the pattern.
        ;"          It appears to apply until a next date is encountered (if any)
        ;"         Also, this is SET for cases where SET days are being specified
        ;"          as well as when date ranges are specified.
        ;"Input: TMGIEN -- IEN in HOSPITAL LOCATION file.
        ;"       PARRAY -- PASS BY NAME.  Array containing time data.  e.g.:
        ;"              @PARRAY@("0800-0810")=2
        ;"              @PARRAY@("0830-0850")=1
        ;"              @PARRAY@("0900-0930")=1
        ;"              @PARRAY@("1000-1140")=1
        ;"       TMG1DATE -- Starting date of a range to put entry into
        ;"       TMGLIMDT -- Limit date of date range.
        ;"       TMGERR -- PASS BY REFERANCE
        ;"       TMGMSG -- PASS BY REFERANCE
        ;"Globally Scoped vars used: ...
        ;"Result: NONE
        ;"Note: It is presumed record locking has already occured
        ;"Note: It is assumed that prior "T" nodes are gone,
        ;"      which may be achieved by DO KILLAVAL(TMG1DATE,TMGLIMDT,TMGFLAGS)
        ;
        NEW STARTDAY SET STARTDAY=+$P($GET(^SC(TMGIEN,"SL")),"^",3) ;"SL;3=HR CLINIC DISPLAY BEGINS
        IF STARTDAY'>0 SET STARTDAY=8   ;"Default to start at 8 am
        ;
        ;"--Delete all preexisting T nodes in NEW date range--  SHOULD ALREADY BE DONE VIA KILLAVAL
        ;
FA1     ;" -- Set up T nodes for NEW date range --
        NEW LASTTIME SET LASTTIME=STARTDAY*100
        NEW COUNT SET COUNT=0
        NEW TMGTIMES SET TMGTIMES=""
        FOR  SET TMGTIMES=$ORDER(@PARRAY@(TMGTIMES)) QUIT:(TMGTIMES="")!TMGERR  DO
        . NEW T1,T2,MIN,H1,H2,M1,M2
        . SET T1=$P(TMGTIMES,"-",1)
        . SET T2=$P(TMGTIMES,"-",2)
        . ;"process individual times.
        . NEW APTSPER SET APTSPER=+$GET(@PARRAY@(TMGTIMES))
        . SET LASTTIME=T2
        . DO MILSUB^TMGSDAU1(.T2,TMGSDUR,.H2,.M2)
        . DO MILADD^TMGSDAU1(.T1,0,.H1,.M1)
        . FOR  DO  QUIT:(T1>T2)
        . . SET COUNT=COUNT+1
        . . ;"Store Time^#ApptsInSlot in "T" node
        . . SET ^SC(TMGIEN,"T",TMG1DATE,2,COUNT,0)=H1_M1_"^"_APTSPER
        . . DO MILADD^TMGSDAU1(.T1,TMGSDUR,.H1,.M1)
        SET ^SC(TMGIEN,"T",TMG1DATE,0)=TMG1DATE
        ;" -- Set subsubfile header --
        SET ^SC(TMGIEN,"T",TMG1DATE,2,0)="^44.004A^"_COUNT_"^"_COUNT
        ;
        ;" -- Set subfile header --
        NEW DATE SET DATE=0
        NEW COUNT SET COUNT=0
        NEW LAST SET LAST=0
        FOR  SET DATE=+$ORDER(^SC(TMGIEN,"T",DATE)) QUIT:(DATE'>0)  DO
        . SET LAST=DATE
        . SET COUNT=COUNT+1
        SET $PIECE(^SC(TMGIEN,"T",0),"^",3)=LAST
        SET $PIECE(^SC(TMGIEN,"T",0),"^",4)=COUNT
        ;
        QUIT
        ;
        ;
KILLAVAL(TMGIEN,TMG1DATE,TMGLIMDT,TMGFLAGS)
        ;"Purpose: Delete AVAILABILITY ("T") node, and any linked "ST" and "OST" nodes.
        ;"Input: TMGIEN -- IEN in HOSPITAL LOCATION file.
        ;"       TMG1DATE -- Starting date of a range to put entry into
        ;"       TMGLIMDT -- Limit date of date range.
        ;"       TMGFLAGS -- flags
        ;"Globally Scoped vars used: ...
        ;"Note: It is presumed record locking has already occured
        ;
        ;"Only delete "2" subnode.  Leave "0" node in place to prevent extending
        ;"date range of entry occuring before this one.
        ;"Only delete entries falling on same day of week as TMG1DATE
        ;
        IF TMGFLAGS["R" DO
        . NEW DATE SET DATE=TMG1DATE
        . FOR  DO  SET DATE=$$ADD2DATE^TMGSDAU1(DATE,7) QUIT:(DATE'<TMGLIMDT)!(DATE'<DT+50000)
        . . DO KILL1DATE(TMGIEN,DATE)
        ELSE  DO
        . DO KILL1DATE(TMGIEN,TMG1DATE)
        QUIT
        ;
        ;
KILL1DATE(TMGIEN,TMG1DATE,FULL)
        ;"Purpose: To remove 1 "T" node, and any linked ST and OST nodes
        ;"Input: TMGIEN -- IEN in HOSPITAL LOCATION file.
        ;"       TMG1DATE -- the date to remove
        ;"       FULL -- OPTIONAL.  IF 1 then entire T node removed, otherwise 0 node is left.
        ;"Globally-scoped var used: ...
        ;"Note: It is presumed record locking has already occured
        KILL ^SC(TMGIEN,"T",DATE,2)
        IF $DATA(^SC(TMGIEN,"ST",DATE)) DO
        . IF $DATA(^SC(TMGIEN,"ST",DATE,9)) DO
        . . KILL ^SC(TMGIEN,"OST",DATE)
        . KILL ^SC(TMGIEN,"ST",DATE)
        IF $GET(FULL)=1 KILL ^SC(TMGIEN,"T",DATE)
        QUIT
        ;
        ;
AVAIL4DAT(TMGIEN,TMG1DATE,PARRAY)
        ;"Purpose: To generage an array with slot time data for a given date, based on templates
        ;"         This PARRAY could be suitable for generating a "T" node entry
        ;"Input: TMGIEN -- IEN in HOSPITAL LOCATION file.
        ;"       TMG1DATE -- the appointment date to look up.  Don't pass by reference
        ;"       PARRAY -- PASS BY NAME.  An OUT PARAMETER (prior contents killed)
        ;"              Output format: Array containing time data.  e.g.:
        ;"              @PARRAY@("0800-0810")=2
        ;"              @PARRAY@("0830-0850")=1
        ;"              @PARRAY@("0900-0930")=1
        ;"              @PARRAY@("1000-1140")=1
        ;"Results: 1=success, -1^Msg=error
        ;"Output: @PARRAY is filled as above
        ;
        NEW TMGRESULT SET TMGRESULT=1 ;"Default to success
        SET TMG1DATE=TMG1DATE\1
        KILL @PARRAY
        ;
        ;"First see IF a Special pattern exists for date in "OST" note
        NEW STR SET STR=$GET(^SC(TMGIEN,"OST",TMG1DATE,1))
        IF STR'="" DO  GOTO A4DDONE
        . SET STR=$PIECE(STR,"|",2,999)
        . DO STR2PAT(TMGIEN,STR,PARRAY)
        ;
        ;"FIND APPLICABLE TEMPLATE (Tx NODE)
        NEW DOW SET DOW=$$DOW^XLFDT(TMG1DATE,1) ;"DOW=Day of Week (0-6)
        NEW DATE SET DATE=TMG1DATE
        SET DATE=$ORDER(^SC(TMGIEN,"T"_DOW,TMG1DATE))
        IF DATE="" DO  GOTO A4DDONE
        . SET TMGRESULT="-1^NO TEMPLATE FOUND FOR DATE"
        SET STR=$GET(^SC(TMGIEN,"T"_DOW,DATE,1))
        IF STR="" DO  GOTO A4DDONE
        . SET TMGRESULT="-1^NO VALID TEMPLATE FOUND FOR DATE"
        DO STR2PAT(TMGIEN,STR,PARRAY)
        ;
A4DDONE QUIT TMGRESULT
        ;
        ;
STR2PAT(TMGIEN,STR,PARRAY)
        ;"Purpose: Convert a template pattern into an array of times.
        ;"Input: TMGIEN -- IEN in file 44
        ;"       STR -- Template pattern (Note that DAY DATE is *NOT* at beginning of line)
        ;"           E.g. |     [1]   [1 1 1 1 1] [1 1 1 1 1] |          |           [1]   [1 1] [1 1 1 1 1] [1 1 1 1 1] [1 1 1] "
        ;"       PARRAY -- PASS BY NAME.  An OUT PARAMETER (prior contents killed)
        ;"              Output format: Array containing time data.  e.g.:
        ;"              @PARRAY@("0800-0810")=2
        ;"              @PARRAY@("0830-0850")=1
        ;"              @PARRAY@("0900-0930")=1
        ;"              @PARRAY@("1000-1140")=1
        ;"Globally-scoped vars used: ...
        ;"Result: 1 if OK, -1 IF error
        ;
        NEW TMGRESULT SET TMGRESULT=1 ;"Default to success
        SET STR=$GET(STR) IF STR="" SET TMGRESULT=-1 GOTO S2PDONE
        SET PARRAY=$GET(PARRAY) IF PARRAY="" SET TMGRESULT=-1 GOTO S2PDONE
        ;
        NEW TMGSPH SET TMGSPH=+$P($GET(^SC(TMGIEN,"SL")),"^",6) ;"SL;6 = DISPLAY INCS PER HOUR (Slots per Hr)
        IF TMGSPH'>0 SET TMGSPH=4       ;"Default to 4 slots/hr
        NEW APTLEN SET APTLEN=60\TMGSPH ;"Minutes length of each slot
        NEW STARTDAY SET STARTDAY=+$P($GET(^SC(TMGIEN,"SL")),"^",3) ;"SL;3=HR CLINIC DISPLAY BEGINS
        SET STARTDAY=STARTDAY_"00" ;"Make into military time, e.g. 8am --> 0800
        FOR  QUIT:$LENGTH(STARTDAY)'<4  SET STARTDAY="0"_STARTDAY
        NEW STRLEN SET STRLEN=$LENGTH(STR)
        NEW IDX FOR IDX=2:2:STRLEN DO
        . NEW TIME1,TIME2,HRS,MINS,CH,NUMAVAIL
        . SET CH=$EXTRACT(STR,IDX)
        . IF (CH="")!(CH=" ") QUIT
        . ;"CONVERT CH INFO NUMAVAIL
        . SET NUMAVAIL=$$CH2NAVAL(CH)
        . IF NUMAVAIL'>0 QUIT
        . SET TIME1=((IDX-2)/2)/TMGSPH
        . SET TIME1=$$FRAC2TIM(TIME1)
        . SET TIME1=$$MILADD2^TMGSDAU1(TIME1,STARTDAY) ;"add 2 times
        . DO MILADD^TMGSDAU1(TIME1,APTLEN,.HRS,.MINS)    ;"add time + mins
        . NEW TEMP SET TEMP=TIME1_"-"_HRS_MINS
        . SET @PARRAY@(TEMP)=NUMAVAIL
        ;
S2PDONE ;
        QUIT TMGRESULT
        ;
        ;
FRAC2TIM(TIME,HRS,MINS)
        ;"Purpose: Convert Fractional time --> Hrs & Min e.g. 3.75 --> 3 & 45 (i.e. 3:45)
        ;"Input: TIME: Time in fractional format.  E.g. 3.75
        ;"       HRS -- PASS BY REFERENCE.  An OUT PARAMETER.  Set to be resulting hours
        ;"              will ensure length it 2 digits.  i.e. 1 --> 01
        ;"       MINS -- PASS BY REFERENCE.  An OUT PARAMETER.  Set to be minutes minutes
        ;"              will ensure length it 2 digits.  i.e. 1 --> 01
        ;"Result: result in military format
        SET HRS=TIME\1  ;"Get just hrs part
        SET MINS=TIME#1 ;"Get just minutes part, e.g. 0.3 (i.e. 30 minutes)
        SET MINS=(MINS*0.6)*100 ;"convert .75 -> .45, * 100 = 45 minutes
        FOR  QUIT:$LENGTH(MINS)>1  SET MINS="0"_MINS
        FOR  QUIT:$LENGTH(HRS)>1  SET HRS="0"_HRS
        ;
        QUIT HRS_MINS
        ;
        ;
CH2NAVAL(CH)
        ;"Purpose: convert a given availability character into number of slots there.
        NEW CODES SET CODES="{}&%?#@!$* XXWVUTSRQPONMLKJIHGFEDCBA0123456789jklmnopqrstuvwxyz"
        QUIT $FIND(CODES,CH)-$FIND(CODES,"0")
        ;
        ;