TMGRPC5 ;TMG/kst/RPC Functions for Scheduling ;01/12/09, 2/2/14
         ;;1.0;TMG-LIB;**1**;01/12/09
 ;
 ;"TMG RPC FUNCTIONS for working with Scheduling GUI application
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
 ;" RPC -- Public Functions.
 ;"=======================================================================
 ;"MAKEAPPT(RESULT,PATIENT,APPT,CLINIC,MODE,INFO) -- MAKE AN APPOINTMENT
 ;"APPTLIST(RESULT,STRTDATE,ENDDATE,FILTER,TMGMSG) --return a listing of appts
 ;"GETAVAIL(RESULT,CLINIC,STRTDATE,ENDDATE) -- Return array with appt slot info
 ;"SETAVAIL(RESULT,CLINIC,PATTERN,FLAGS) -- API to SET availability for clinic
 ;"CANCAPPT(RESULT,PATIENT,APPT,CLINIC,MODE,INFO)--CANCEL AN APPOINTMENT
 ;"=======================================================================
 ;"Dependencies:
 ;" TMGSDAM2,TMGSDAG,TMGSDAVG,TMGSDAVS,TMGSDAU2
 ;"=======================================================================
 ;
MAKEAPPT(RESULT,PATIENT,APPT,CLINIC,MODE,INFO)     ;
        ;"Purpose: MAKE AN APPOINTMENT
        ;"INPUT: RESULTS -- PASS BY REFERENCE, an OUT PARAMETER (See values below)
        ;"       PATIENT -- Patient Name (must be unique) or IEN
        ;"       APPT -- Desired Appointment Date & Time -- External, or FM format
        ;"       CLINIC -- Name, or IEN, of Clinic for appt (file 44)
        ;"       MODE -- Mode.  See STYPE in EN^TMGSDAM2() below for reference
        ;"       INFO -- PASS BY REFERENCE.  See SDARRAY in EN^TMGSDAM2() below for reference
        ;"Result: None
        ;"Output: RESULTS variable filled as follows
        ;"      1 = OK,APPOINTMENT SUCCESSFULLY MADE
        ;"      NEG NUMBER= ErrorNum^ErrorMessage.  See also EN^TMGSDAM2() for values
        ;
        NEW UseStored SET UseStored=0 ;"default
        ;"set UseStored=1
        IF UseStored=0 do
        . KILL ^TMG("TMP","RPC","MAKEAPPT")
        . MERGE ^TMG("TMP","RPC","MAKEAPPT","PATIENT")=PATIENT
        . MERGE ^TMG("TMP","RPC","MAKEAPPT","APPT")=APPT
        . MERGE ^TMG("TMP","RPC","MAKEAPPT","CLINIC")=CLINIC
        . MERGE ^TMG("TMP","RPC","MAKEAPPT","MODE")=MODE
        . MERGE ^TMG("TMP","RPC","MAKEAPPT","INFO")=INFO
        ELSE  IF UseStored=1 do
        . MERGE PATIENT=^TMG("TMP","RPC","MAKEAPPT","PATIENT")
        . MERGE APPT=^TMG("TMP","RPC","MAKEAPPT","APPT")
        . MERGE CLINIC=^TMG("TMP","RPC","MAKEAPPT","CLINIC")
        . MERGE MODE=^TMG("TMP","RPC","MAKEAPPT","MODE")
        . MERGE INFO=^TMG("TMP","RPC","MAKEAPPT","INFO")
        ;
        KILL RESULT
        NEW DFN,SC,APPTDATE
        SET RESULT(0)=1 ;"Default to success.
        SET DFN=$$GETDFN^TMGSDAU2(.PATIENT)
        IF DFN<1 DO  GOTO ENRDONE
        . SET RESULT(0)="-1^Invalid Patient: "_$GET(PATIENT)
        SET SC=$$GETCLIEN^TMGSDAU2(.CLINIC)
        IF SC<1 DO  GOTO ENRDONE
        . SET RESULT(0)="-1^Can't find clinic: "_$GET(CLINIC)
        SET APPTDATE=$$GETDATE^TMGSDAU2(.APPT)
        IF APPTDATE<1 DO  GOTO ENRDONE
        . SET RESULT(0)="-1^Invalid appt Date: "_$GET(APPT)
        ;
        SET RESULT(0)=$$EN^TMGSDAM2(DFN,APPTDATE,SC,MODE,.INFO)
        ;
ENRDONE ;
        QUIT
        ;
        ;
APPTLIST(RESULT,STRTDATE,ENDDATE,FILTER)
        ;"Purpose: To return a listing of appts, based on dates and filters).  Provide a wrapper to $$SDAPI^SDAMA301
        ;"                that accepts data in external formats, and returns custom results in array.
        ;"Input:RESULTS: PASS BY REFERENCE.  An OUT PARAMATER. (See format below)
        ;"      STRTDATE -- Starting Date (in FMDate format, OR External format, e.g. Jan 1, 2005) (Time ignored)
        ;"                        If value="I" or "" (i.e. 'indefinite'), then all dates up to End Date on returned
        ;"      ENDDATE -- OPTIONAL. Ending date (in FMDate, OR External format) (Time ignored)
        ;"                        If not provided, then only dates for Starting date returned
        ;"                        If value="I" (i.e. 'indefinite'), then all dates from Start Date on returned
        ;"      FILTER -- PASS BY REFERENCE. OPTIONAL.  Format below.  Any or all of the filters can be applied at once.
        ;"                     See APPTLIST^TMGSDAG for reference
        ;"Results: None
        ;"Output: RESULT is filled with results.  Format below:
        ;"                RESULT(0)=# of found matching appts, or 0 IF none, or -1 IF error.
        ;"                RESULT(Count)=-1^ErrCode^Message <-- (if any)
        ;"                RESULT(Count)=-1^ErrCode^Message
        ;"                RESULT(Count)=-1^ErrCode^Message
        ;"                RESULT(Count)=PatientIEN;PatientName^DOB^FMFormatApptDateTime;ExtFormatApptDateTime^ClinicIEN;ClinicName^StatusCode;StatusName
        ;"                RESULT(Count)=PatientIEN;PatientName^DOB^FMFormatApptDateTime;ExtFormatApptDateTime^ClinicIEN;ClinicName^StatusCode;StatusName
        ;"                RESULT(Count)=PatientIEN;PatientName^DOB^FMFormatApptDateTime;ExtFormatApptDateTime^ClinicIEN;ClinicName^StatusCode;StatusName
        ;"              NOTE: If there are no appts for time period, then one entry will be
        ;"                      returned as follows.  This value can be used as a place holder
        ;"                      to show that a search has taken place
        ;"                RESULT(Count)="0;<NONE>^FMFormatApptDateTime"
        ;
        IF 1=0 do
        . KILL ^TMG("TMP","RPC","APPTLIST")
        . MERGE ^TMG("TMP","RPC","APPTLIST","FILTER")=FILTER
        . MERGE ^TMG("TMP","RPC","APPTLIST","STRTDATE")=STRTDATE
        . MERGE ^TMG("TMP","RPC","APPTLIST","ENDDATE")=ENDDATE
        ELSE  IF 0=1 do
        . MERGE FILTER=^TMG("TMP","RPC","APPTLIST","FILTER")
        . MERGE STRTDATE=^TMG("TMP","RPC","APPTLIST","STRTDATE")
        . MERGE ENDDATE=^TMG("TMP","RPC","APPTLIST","ENDDATE")
        ;
        NEW TMGMSG,TMGRESULT
        NEW COUNT SET COUNT=0
        SET RESULT(COUNT)=$$APPTLIST^TMGSDAG(.STRTDATE,.ENDDATE,.FILTER,.TMGRESULT,.TMGMSG)
        NEW IDX SET IDX=""
        FOR  SET IDX=$ORDER(TMGMSG(IDX)) QUIT:(IDX="")  DO
        . SET COUNT=COUNT+1
        . SET RESULT(COUNT)=TMGMSG(IDX)
        SET IDX=""
        IF $DATA(TMGRESULT)=0 DO
        . NEW TMGSTRTDT SET TMGSTRTDT=$$GETDATE^TMGSDAU2(.STRTDATE)
        . SET COUNT=COUNT+1
        . SET RESULT(COUNT)="0;<NONE>^"_TMGSTRTDT
        ELSE  FOR  SET IDX=$ORDER(TMGRESULT(IDX)) QUIT:(IDX="")  DO
        . SET COUNT=COUNT+1
        . SET RESULT(COUNT)=TMGRESULT(IDX)
        QUIT
        ;
        ;
GETAVAIL(RESULT,CLINIC,STRTDATE,ENDDATE)
        ;"Purpose: Return an array with appt slot information: time, availibility
        ;"Input:RESULTS: PASS BY REFERENCE.  An OUT PARAMATER. (See format below)
        ;"      CLINIC-- Name, or IEN, of Clinic for appt (file 44)
        ;"      STRTDATE -- The beginning of the date range requested
        ;"      ENDDATE -- The end of the date range requested
        ;"Results: None
        ;"Output: RESULT is filled with results.  Format below:
        ;"                RESULT(0)=1 for Success, 0 for Intermed success, -1^Msg for error
        ;"                RESULT(Count)=-1^Message <-- (if any)
        ;"                RESULT(Count)=-1^Message
        ;"                RESULT(Count)="INFO^APPTLEN"^LengthOfApptSlot
        ;"                RESULT(Count)=Date^SlotStartTime^NumOpenings^NumScheduled^NumTotalSlotsAtTime
        ;"                RESULT(Count)=Date^SlotStartTime^NumOpenings^NumScheduled^NumTotalSlotsAtTime
        ;"Note: If a clinic is not SET up for a given day in date range, no results will be
        ;"      returned for that invalid day.
        ;
        IF 1=0 do
        . KILL ^TMG("TMP","RPC","GETAVAIL")
        . MERGE ^TMG("TMP","RPC","GETAVAIL","CLINIC")=CLINIC
        . MERGE ^TMG("TMP","RPC","GETAVAIL","STRTDATE")=STRTDATE
        . MERGE ^TMG("TMP","RPC","GETAVAIL","ENDDATE")=ENDDATE
        ELSE  IF 1=0 do
        . MERGE CLINIC=^TMG("TMP","RPC","GETAVAIL","CLINIC")
        . MERGE STRTDATE=^TMG("TMP","RPC","GETAVAIL","STRTDATE")
        . MERGE ENDDATE=^TMG("TMP","RPC","GETAVAIL","ENDDATE")
        ;
        NEW SC SET SC=$$GETCLIEN^TMGSDAU2(.CLINIC)
        IF SC<1 DO  GOTO GADONE
        . SET RESULT(0)="-1^Bad clinic name: '"_$GET(CLINIC)_"'"
        NEW TMGSTRTDT SET TMGSTRTDT=$$GETDATE^TMGSDAU2(.STRTDATE)
        IF TMGSTRTDT<1 DO  GOTO GADONE
        . SET RESULT(0)="-1^Bad starting date: '"_$GET(TMGSTRTDT)_"'"
        NEW TMGENDDT SET TMGENDDT=$$GETDATE^TMGSDAU2(.ENDDATE)
        IF TMGENDDT<1 DO  GOTO GADONE
        . SET RESULT(0)="-1^Bad ending date: '"_$GET(TMGENDDT)_"'"
        ;
        NEW TMGAVAIL,TMGMSG
        NEW COUNT SET COUNT=0
        SET RESULT(COUNT)=$$GETAVAIL^TMGSDAVG(SC,TMGSTRTDT,TMGENDDT,.TMGAVAIL,.TMGMSG)
        ;
        NEW IDX SET IDX=""
        FOR  SET IDX=$ORDER(TMGMSG(IDX)) QUIT:(IDX="")  DO
        . SET COUNT=COUNT+1
        . SET RESULT(COUNT)=TMGMSG(IDX)
        NEW DATE SET DATE=""
        FOR  SET DATE=$ORDER(TMGAVAIL(DATE)) QUIT:(DATE="")  DO
        . NEW TIME SET TIME=""
        . FOR  SET TIME=$ORDER(TMGAVAIL(DATE,TIME)) QUIT:(TIME="")  DO
        . . SET COUNT=COUNT+1
        . . SET RESULT(COUNT)=DATE_"^"_TIME_"^"_$GET(TMGAVAIL(DATE,TIME))
GADONE  ;
        QUIT
        ;
        ;
SETAVAIL(RESULT,CLINIC,FLAGS,PATTERN) ;
        ;"Purpose: API to SET availability for a given clinic
        ;"NOTE: **See expanded notes in SETAVAIL^TMGSDAVS**
        ;"Input: CLINIC -- IEN in HOSPITAL LOCATION file.
        ;"       FLAGS -- "D" = Delete appts (if not present then appts are SET)
        ;"                "I" = Ignore existing appts when changing slots (TO BE IMPLEMENTED)
        ;"       PATTERN -- PASS BY REFERENCE.  Array Format:
        ;"         PATTERN(COUNT)=ADate^EndDate^ExtTime^ApptsPerSlot
        ;"         PATTERN(COUNT)=ADate^EndDate^ExtTime^ApptsPerSlot
        ;"         PATTERN(COUNT)=ADate^EndDate^ExtTime^ApptsPerSlot
        ;"Results: None
        ;"Output: RESULT is filled with results.  Format below:
        ;"                RESULT(Count)=-1^ErrCode^Message <-- (if any)
        ;"                RESULT(Count)=-1^ErrCode^Message
        ;"         or     RESULT(1)="1^Success"
        ;
        NEW SC SET SC=$$GETCLIEN^TMGSDAU2(.CLINIC)
        IF SC<1 DO  GOTO SDADONE
        . SET RESULT(0)="-1^Bad clinic name: '"_$GET(CLINIC)_"'"
        NEW TMGPATRN,ADATE,ENDDATE,EXTTIME,APS
        NEW COUNT SET COUNT=""
        FOR  SET COUNT=$ORDER(PATTERN(COUNT)) DO
        . NEW PAT SET PAT=$GET(PATTERN(COUNT)) ;"ADate^EndDate^ExtTime^ApptsPerSlot
        . QUIT:PAT=""
        . SET ADATE=$P(PAT,"^",1)
        . SET ENDDATE=$P(PAT,"^",2)
        . SET EXTTIME=$P(PAT,"^",3)
        . SET APS=$P(PAT,"^",4)
        . SET TMGPATRN(ADATE_"^"_ENDDATE,EXTTIME)=APS
        ;
        NEW TMGMSG,TEMP
        NEW COUNT SET COUNT=0
        SET RESULT(COUNT)=$$SETAVAIL^TMGSDAVS(SC,.TMGPATRN,FLAGS,.TMGMSG)
        ;
        NEW IDX SET IDX=""
        FOR  SET IDX=$ORDER(TMGMSG(IDX)) QUIT:(IDX="")  DO
        . SET COUNT=COUNT+1
        . SET RESULT(COUNT)=TMGMSG(IDX)
        IF COUNT=0 SET RESULT(1)="1^Success"
SDADONE ;
        QUIT
        ;
        ;
CANCAPPT(RESULT,PATIENT,APPT,CLINIC,INFO)     ;
        ;"Purpose: CANCEL AN APPOINTMENT
        ;"INPUT: RESULTS -- PASS BY REFERENCE, an OUT PARAMETER (See values below)
        ;"       PATIENT -- Patient Name (must be unique) or IEN
        ;"       APPT -- Appointment Date & Time to be cancelled -- External, or FM format
        ;"       CLINIC -- Name, or IEN, of Clinic for appt (file 44)
        ;"       INFO -- PASS BY REFERENCE.
        ;"         INFO("REASON IEN")= IEN of cancellation reason, from 409.2
        ;"         INFO("COMMENT") = Comment (3-160 chars length)     (OPTIONAL)
        ;"Result: None
        ;"Output: RESULTS variable filled as follows
        ;"      RESULT(1) = 1 --> OK,APPOINTMENT SUCCESSFULLY CANCELLED
        ;"      RESULT(1) = -1^ErrorMessage.
        ;
        IF 1=1 do
        . KILL ^TMG("TMP","RPC","CANCAPPT")
        . MERGE ^TMG("TMP","RPC","CANCAPPT","PATIENT")=PATIENT
        . MERGE ^TMG("TMP","RPC","CANCAPPT","APPT")=APPT
        . MERGE ^TMG("TMP","RPC","CANCAPPT","CLINIC")=CLINIC
        . MERGE ^TMG("TMP","RPC","CANCAPPT","INFO")=INFO
        ELSE  IF 1=0 do
        . MERGE PATIENT=^TMG("TMP","RPC","CANCAPPT","PATIENT")
        . MERGE APPT=^TMG("TMP","RPC","CANCAPPT","APPT")
        . MERGE CLINIC=^TMG("TMP","RPC","CANCAPPT","CLINIC")
        . MERGE INFO=^TMG("TMP","RPC","CANCAPPT","INFO")
        ;
        KILL RESULT
        NEW DFN,SC,APPTDATE
        SET RESULT(0)=1 ;"Default to success.
        SET DFN=$$GETDFN^TMGSDAU2(.PATIENT)
        IF DFN<1 DO  GOTO CANCDONE
        . SET RESULT(0)="-1^Invalid Patient: "_$GET(PATIENT)
        SET SC=$$GETCLIEN^TMGSDAU2(.CLINIC)
        IF SC<1 DO  GOTO CANCDONE
        . SET RESULT(0)="-1^Can't find clinic: "_$GET(CLINIC)
        SET APPTDATE=$$GETDATE^TMGSDAU2(.APPT)
        IF APPTDATE<1 DO  GOTO CANCDONE
        . SET RESULT(0)="-1^Invalid appt Date: "_$GET(APPT)
        ;
        SET RESULT(0)=$$CANCAPPT^TMGSDAC(DFN,APPTDATE,SC,.INFO)
        ;
CANCDONE ;
        QUIT