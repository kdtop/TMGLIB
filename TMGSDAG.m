TMGSDAG ;TMG/kst/API FOR GETTING LIST OF APPTS;1/11/09, 2/2/14, 3/24/21
         ;;1.0;TMG-LIB;**1**;1/11/09
 ;
 ;"Called into from TMGRPC5
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
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"APPTLIST(STRTDATE,ENDDATE,FILTER,RESULTS,TMGMSG)
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"SDAMA301,DIC
 ;"=======================================================================
 ;
APPTLIST(STRTDATE,ENDDATE,FILTER,RESULTS,TMGMSG)
        ;"Purpose: To return a listing of appts, based on dates and filters).  Provide a wrapper to $$SDAPI^SDAMA301
        ;"                that accepts data in external formats, and returns custom results in array.
        ;"Input:STRTDATE -- Starting Date (in FMDate format, OR External format, e.g. Jan 1, 2005) (Time ignored)
        ;"                        If value="I" or "" (i.e. 'indefinite'), then all dates up to End Date on returned
        ;"      ENDDATE -- OPTIONAL. Ending date (in FMDate, OR External format) (Time ignored)
        ;"                        If not provided, then only dates for Starting date returned
        ;"                        If value="I" (i.e. 'indefinite'), then all dates from Start Date on returned
        ;"      FILTER -- PASS BY REFERENCE. OPTIONAL.  Format below.  Any or all of the filters can be applied at once.
        ;"                     All filters are optional.
        ;"                     1. LOCATION FILTER: "CLINIC"
        ;"                        Specify one or more clinic names or IEN's, separated by "^", so that only
        ;"                        appointments for these clincs will be retuned
        ;"                        e.g. FILTER("CLINIC")="TEST CLINIC"
        ;"                        e.g. FILTER("CLINIC")="PULM CLINIC^GYN CLINIC"
        ;"                        e.g. FILTER("CLINIC")="123" <-- IEN in file 44 (HOSPITAL LOCATION)
        ;"                        e.g. FILTER("CLINIC")="123^234^345" etc.
        ;"                     2. STATUS FILTER - "STATUS"
        ;"                        Specify one or more status values, separated by "^", so that only
        ;"                        appointments matching these status values will be returned
        ;"                        Possible codEs:
        ;"                                R -- Appointment cept
        ;"                                I -- Inpatient
        ;"                                NS -- No Show
        ;"                                NSR -- No-show, rescheduled
        ;"                                CP -- Cancelled by patient
        ;"                                CPR -- Cancelled by patient, rescheduled
        ;"                                CC -- Cancelled by clinic
        ;"                                CCR -- Cancelled by clinic, rescheduled
        ;"                                NT -- No action taken
        ;"                        e.g. FILTER("STATUS")="NS"
        ;"                        e.g. FILTER("STATUS")="CP^CPR^I" etc.
        ;"                     3.  PATIENT FILTER -- "PATIENT"
        ;"                        Specify one or more Patient names (External format) or IEN's, separated by "^",
        ;"                        so that only appointments for these patients will be returned.
        ;"                        e.g. FILTER("PATIENT")="JONES,DAVID S"
        ;"                        e.g. FILTER("PATIENT")="JONES,DAVID S^SMITH,JOHN A" etc.
        ;"                        e.g. FILTER("PATIENT")="123456"  <-- IEN in PATIENT file
        ;"                        e.g. FILTER("PATIENT")="123456^234567^345678" etc.
        ;"                     4. STOP CODES FILTER -- "STOP CODE"
        ;"                        Specify one or more valid Primary Stop Code values (not IEN's), separated by "^",
        ;"                        so that only appointments matching these values will be returned.
        ;"                        Stop Code must be a valied AMIS REPORTING STOP CODE (field #1) on the CLINIC STOP file (#40.7)
        ;"                        e.g. FILTER("STOP CODE")="197" <-- STOP CODE, not IEN
        ;"                        e.g. FILTER("STOP CODE")="197^234^345"
        ;"                   5.   QUANTITY FILTER -- "MAX"
        ;"                        Specify a value, such that only the first N appts will be returned.  If specified, then must
        ;"                        be whole value > 0
        ;"                        e.g. FILTER("MAX")=25  <-- Only return 1st 25 found appts
        ;"                   NOTE: The $$SDAPI^SDAMA301 seems to support more filters.  I could add support for them later...
        ;"        RESULTS: PASS BY REFERENCE.  An OUT PARAMATER.
        ;"                RESULTS(Count)=PatientIEN;PatientName^DOB^FMFormatApptDateTime;ExtFormatApptDateTime^ClinicIEN;ClinicName^StatusCode;StatusName
        ;"                RESULTS(Count)=PatientIEN;PatientName^DOB^FMFormatApptDateTime;ExtFormatApptDateTime^ClinicIEN;ClinicName^StatusCode;StatusName
        ;"                RESULTS(Count)=PatientIEN;PatientName^DOB^FMFormatApptDateTime;ExtFormatApptDateTime^ClinicIEN;ClinicName^StatusCode;StatusName
        ;"      TMGMSG: PASS BY REFERENCE.  An OUT PARAMETER -- Used for passing back errors.
        ;"                TMGMSG=Max count of error messages
        ;"                TMGMSG(Count)=ErrCode^Message  (Count starts at 1,2,3...)
        ;"                TMGMSG(Count)=ErrCode^Message
        ;"                TMGMSG(Count)=ErrCode^Message
        ;"Results: Returns # of found matching appts, or 0 IF none, or -1 IF error.
        ;
        NEW TMGRESULT SET TMGRESULT=0
        SET TMGMSG=0
        NEW TMGSDATE,TMGEDATE,TMGARRAY
        KILL RESULTS
        DO SETDATES(.STRTDATE,.ENDDATE,.TMGARRAY,.TMGMSG)
        IF TMGMSG>0 SET TMGRESULT=-1 GOTO ALDONE
        ;
        IF $DATA(FILTER("CLINIC")) DO FILTR2(.FILTER,.TMGARRAY,.TMGMSG)
        IF $DATA(FILTER("STATUS")) DO FILTR3(.FILTER,.TMGARRAY,.TMGMSG)
        IF $DATA(FILTER("PATIENT")) DO FILTR4(.FILTER,.TMGARRAY,.TMGMSG)
        IF $DATA(FILTER("STOP CODE")) DO FILTR13(.FILTER,.TMGARRAY,.TMGMSG)
        IF TMGMSG>0 SET TMGRESULT=-1 GOTO ALDONE
        ;
        SET TMGARRAY("FLDS")="1;2;3"
        SET TMGARRAY("SORT")="P" ;"Specify output format"
        SET TMGRESULT=$$SDAPI^SDAMA301(.TMGARRAY)
        IF TMGRESULT<0 DO
        . DO HANDLERR(.TMGMSG)
        ELSE  DO GETRSLTS(.RESULTS)
        ;
ALDONE  QUIT TMGRESULT
        ;
        ;
SETDATES(TMGSDATE,TMGEDATE,TMGARRAY,TMGMSG)
        ;"Purpose: convert external dates into internal format, Validate dates,
        ;"           and insert into TMGARRAY(1) filter
        ;"Result: None
        NEW DATE1,DATE2
        SET TMGSDATE=$GET(TMGSDATE)
        SET TMGEDATE=$GET(TMGEDATE)
        IF TMGEDATE="" SET TMGEDATE=TMGSDATE
        IF +TMGSDATE=TMGSDATE SET DATE1=TMGSDATE
        ELSE  DO  GOTO:TMGMSG>0 SDDONE
        . IF (TMGSDATE="I")!(TMGSDATE="") SET DATE1="" QUIT
        . NEW MSG
        . DO DT^DILF("T",TMGSDATE,.MSG)
        . IF MSG=-1 DO  QUIT
        . . SET TMGMSG=TMGMSG+1
        . . SET TMGMSG(TMGMSG)="-1^Invalid Start Date"
        . SET DATE1=MSG\1
        IF +TMGEDATE=TMGEDATE SET DATE2=TMGEDATE
        ELSE  DO
        . IF (TMGEDATE="I")!(TMGEDATE="") SET DATE2="" QUIT
        . NEW MSG
        . DO DT^DILF("T",TMGEDATE,.MSG)
        . IF MSG=-1 DO  QUIT
        . . SET TMGMSG=+$GET(TMGMSG)+1
        . . SET TMGMSG(TMGMSG)="-1^Invalid End Date"
        . SET DATE2=MSG\1
        IF (DATE1="")&(DATE2="") GOTO SDDONE
        SET DATE1=DATE1\1
        SET DATE2=DATE2\1
        SET TMGARRAY(1)=DATE1_";"_DATE2
SDDONE  ;
        QUIT
        ;
        ;
FILTR2(FILTER,TMGARRAY,TMGMSG)
        ;"Purpose: To take and validate user input, and insert into TMGARRAY(2) filter
        ;
        NEW PLACES SET PLACES=$GET(FILTER("CLINIC"))
        NEW LOC,STR
        SET STR=""
        FOR  SET LOC=$PIECE(PLACES,"^",1) QUIT:(LOC="")  DO
        . IF +LOC'=LOC DO
        . . NEW DIC,X,Y
        . . SET DIC=44,DIC(0)="M"
        . . SET X=LOC DO ^DIC
        . . IF +Y>0 SET LOC=+Y
        . . ELSE  DO
        . . . SET TMGMSG=+$GET(TMGMSG)+1
        . . . SET TMGMSG(TMGMSG)="-1^'"_LOC_"' clinic location NOT FOUND."
        . IF LOC>0 DO
        . . IF STR'="" SET STR=STR_";"
        . . SET STR=STR_LOC
        . IF $LENGTH(PLACES,"^")=1 SET PLACES="" QUIT
        . ELSE  SET PLACES=$PIECE(PLACES,2,$LENGTH(PLACES,"^"))
        IF STR'="" SET TMGARRAY(2)=STR
        ;
F2DONE  QUIT
        ;
        ;
FILTR3(FILTER,TMGARRAY,TMGMSG)
        ;"Purpose: To take user input, and insert into TMGARRAY(3) filter
        NEW STR SET STR=$GET(FILTER("STATUS"))
        SET STR=$TRANSLATE(STR,"^",";")
        IF STR'="" SET TMGARRAY(3)=STR
        QUIT
        ;
        ;
FILTR4(FILTER,TMGARRAY,TMGMSG)
        ;"Purpose: To take and validate user input, and insert into TMGARRAY(4) filter
        ;
        NEW NAMES SET NAMES=$GET(FILTER("PATIENT"))
        NEW ANAME,STR,TMGDFN
        SET STR=""
        FOR  SET ANAME=$PIECE(NAMES,"^",1) QUIT:(ANAME="")  DO
        . SET TMGDFN=0
        . IF +ANAME'=ANAME DO
        . . NEW TMG2MSG
        . . DO FIND^DIC(2,,".01","MP",ANAME,"*","","","","TMG2MSG")
        . . NEW NUM SET NUM=+$GET(TMG2MSG("DILIST",0))
        . . IF NUM=0 DO  QUIT
        . . . SET TMGMSG=+$GET(TMGMSG)+1
        . . . SET TMGMSG(TMGMSG)="-1^Patient name: '"_ANAME_"' NOT FOUND"
        . . IF NUM>1 DO  QUIT
        . . . SET TMGMSG=+$GET(TMGMSG)+1
        . . . SET TMGMSG(TMGMSG)="-1^Name: "_ANAME_" Not specific.  Multiple patients with this name exist."
        . . SET TMGDFN=+$GET(TMG2MSG("DILIST",1,0))
        . IF TMGDFN>0 DO
        . . IF STR'="" SET STR=STR_";"
        . . SET STR=STR_TMGDFN
        . IF $LENGTH(NAMES,"^")=1 SET NAMES="" QUIT
        . ELSE  SET NAMES=$PIECE(NAMES,2,$LENGTH(NAMES,"^"))
        IF STR'="" SET TMGARRAY(4)=STR
        ;
F4DONE  QUIT
        ;
        ;
FILTR13(FILTER,TMGARRAY,TMGMSG)
        ;"Purpose: To take user input, and insert into TMGARRAY(13) filter
        NEW STR SET STR=$GET(FILTER("STOP CODE"))
        SET STR=$TRANSLATE(STR,"^",";")
        IF STR'="" SET TMGARRAY(13)=STR
        QUIT
        ;
        ;
HANDLERR(TMGMSG)
        ;"Purpose: repackage errors into format for this function
        ;"
        NEW CODE SET CODE=0
        FOR  SET CODE=$ORDER(^TMG($J,"SDAMA301",CODE)) QUIT:(CODE="")  DO
        . NEW DESCR SET DESCR=$GET(^TMG($J,"SDAMA301",CODE))
        . SET TMGMSG=+$GET(TMGMSG)+1
        . SET TMGMSG(TMGMSG)=CODE_"^"_DESCR
        QUIT
        ;
        ;
GETRSLTS(RESULTS)
        ;"Purpose: To repackage results into format for this function
        ;"Input: RESULTS -- PASS BY REFERENCE.  An OUT PARAMETER.
        ;"Output:  RESULTS(Count)=PatientIEN;PatientName^DOB^FMFormatApptDateTime;ExtFormatApptDateTime^ClinicIEN;ClinicName^StatusCode;StatusName
        KILL RESULTS
        NEW COUNT SET COUNT=0
        NEW TMGDFN SET TMGDFN=0
        FOR  SET TMGDFN=$ORDER(^TMP($J,"SDAMA301",TMGDFN)) QUIT:(TMGDFN="")  DO
        . NEW NAME SET NAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
        . NEW STR SET STR=TMGDFN_";"_NAME
        . NEW DOB SET DOB=$PIECE($GET(^DPT(TMGDFN,0)),"^",3)
        . SET DOB=$PIECE($$FMTE^XLFDT(DOB,+5),"@",1)
        . SET STR=STR_"^"_DOB
        . NEW APPT SET APPT=""
        . FOR  SET APPT=$ORDER(^TMP($J,"SDAMA301",TMGDFN,APPT)) QUIT:(APPT="")  DO
        . . NEW VALUE SET VALUE=$GET(^TMP($J,"SDAMA301",TMGDFN,APPT)) QUIT:VALUE=""
        . . NEW TIME SET TIME=$$FMTE^XLFDT(APPT,+5)  ;"+5 = MM/DD/YYYY@HH:MM:SS format
        . . SET $PIECE(VALUE,"^",1)=APPT_";"_TIME
        . . SET VALUE=STR_"^"_VALUE
        . . SET COUNT=COUNT+1
        . . SET RESULTS(COUNT)=VALUE
        ;
        QUIT