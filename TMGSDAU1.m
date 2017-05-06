TMGSDAU1 ;TMG/kst/Schedule Availability Utilities 1;12/22/08, 2/2/14
         ;;1.0;TMG-LIB;**1**;12/22/08
 ;
 ;"TMG SCHEDULING AVAILIBILITY UTILITIES 1
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"NOTE: Much of this code originated from SDB*.m
 ;"
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"SPLITDTS(TMGDTRNG,TMG1DATE,TMGLIMDT,TMGFLAGS,TMGRESULT,TMGMSG) -- Split a date range string into separate vars, and validate
 ;"VALDATES(TMGIEN,PARRAY,TMG1DATE,TMGERR,TMGMSG) -- Validate specified input times
 ;"APPTCHK(TMGIEN,TMG1DATE,TMGLIMDT,ST,ET)  -- Check IF active appts on DATE/Time
 ;"EXTDAT(TMGADATE) -- Get external time display
 ;"MILADD(TIME,ADDMIN,HR,MIN) -- Add time to TIME (in military format) and also return hours (HR) and minutes (MIN)
 ;"MILADD2(TIME,TIME2,HR,MIN) -- Add TIME2 to TIME (in military format) and also return hours (HR) and minutes (MIN)
 ;"MILSUB(TIME,SUBMIN,HR,MIN) -- Subtract time from TIME (in military format) and also return hours (HR) and minutes (MIN)
 ;"VALIDMIL(TMGDATE,TIME,TMGERR,TMGMSG) -- Validate time
 ;"MILDELTA(T1,T2) -- number of minutes T2-T1 (Military format)
 ;"MILSUB2(TIME,SUBTIME,HR,MIN)  --TIME-SUBTIME=RESULT  (Result returned in HR and Min)
 ;"GFMDATE(EXTDATE) -- return FM date from external date
 ;"ADD2DATE(FMDATE,INCNUM) -- return FMDate + added IncNum
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;
SPLITDTS(TMGDTRNG,TMG1DATE,TMGLIMDT,TMGFLAGS,TMGRESULT,TMGMSG)
        ;"Purpose: Split a date range string into separate vars, and validate
        ;"Input: TMGDTRNG -- Date range, as specified by user
        ;"       TMG1DATE -- PASS BY REFERENCE.  An OUT Var.  Returned FM Format date
        ;"       TMGLIMDT -- PASS BY REFERENCE.  An OUT Var.  Returned FM Format date
        ;"       TMGFLAGS -- PASS BY REFERENCE.
        ;"       TMGERR   -- Pass by REFERENCE.  An OUT Var.  A result flag
        ;"       TMGMSG   -- Pass by REFERENCE --An output Message array
        ;"Result: 0 IF OK, 1 IF ERROR.
        ;"Note: TMGLIMDT will be SET to an appropriate date, even IF a RANGE
        ;"      is not planned.  But 'R' flag will not be set.
        ;
        NEW TMGABORT SET TMGABORT=0
        SET TMG1DATE=$$GFMDATE($PIECE(TMGDTRNG,"^",1))
        IF TMG1DATE=-1 DO  GOTO SDDONE
        . SET TMGMSG(TMGDTRNG)="6^Invalid date: "_TMGDTRNG
        . SET TMGRESULT=-1
        NEW TMGENDDT SET TMGENDDT=$PIECE(TMGDTRNG,"^",2) ;"get ending date from range
        IF TMGENDDT="I" DO
        . SET TMGLIMDT=9999999 ;"Specify indefinite ending date
        . IF TMGFLAGS'["R" SET TMGFLAGS=TMGFLAGS_"R" ;"Specify a date range
        ELSE  DO  QUIT:(TMGABORT)
        . IF +TMGENDDT=0 DO
        . . SET TMGENDDT=TMG1DATE  ;"One day only
        . . IF TMGFLAGS["R" SET TMGFLAGS=$TRANSLATE(TMGFLAGS,"R","") ;"remove flag
        . . IF TMGFLAGS'["1" SET TMGFLAGS=TMGFLAGS_"1" ;"Flag to not add range flag later
        . ELSE  DO
        . . SET TMGENDDT=$$GFMDATE(TMGENDDT) ;"get ending date from range
        . . IF TMGFLAGS'["R" SET TMGFLAGS=TMGFLAGS_"R" ;"Specify a date range
        . SET TMGLIMDT=$$ADD2DATE(TMGENDDT,1) ;"*Limit* date is EndingDate+1
        . IF TMGLIMDT'>0 DO  QUIT
        . . SET TMGMSG(TMGDATE)="6^Invalid date: "_TMGENDDT
        . . SET TMGRESULT=-1,TMGABORT=1
        IF (+TMGENDDT>0)&(TMGFLAGS'["R")&(TMGFLAGS'["1") SET TMGFLAGS=TMGFLAGS_"R" ;"Specify a date range
SDDONE  ;
        QUIT TMGABORT
        ;
        ;
VALDATES(TMGIEN,PARRAY,TMG1DATE,TMGLIMDT,TMGFLAGS,TMGERR,TMGMSG)
        ;"Purpose: Validate specified input dates and times
        ;"         Checks for clinic inactivation during range
        ;"         Checks for existing appts at specified times (unless "I" flag set)
        ;"Input: TMGIEN -- IEN in file 44 to deal with.
        ;"       PARRAY -- Pass by NAME.  Name of Array containing time data
        ;"       TMG1DATE -- the 1 date to check in Array
        ;"       TMGLIMDT -- FM-format *limit* date for appointments date range
        ;"       TMGFLAGS -- "R" = Work on range from TMG1DATE up to, but not
        ;"                         including, limit date TMGLIMDT
        ;"                   "I" = Ignore existing appts when changing slots
        ;"       TMGERR -- Pass by REFERENCE.  A result flag
        ;"       TMGMSG -- Pass by REFERENCE -- An output Message array
        ;"Globally scoped vars used: ...
        ;"Result: NONE
        ;
        NEW STARTDAY SET STARTDAY=+$P($GET(^SC(TMGIEN,"SL")),"^",3) ;"SL;3=HR CLINIC DISPLAY BEGINS
        NEW STRTTIME,LASTTIME
        SET (STRTTIME,LASTTIME)=STARTDAY*100
        NEW COUNT SET COUNT=0
        NEW TMGSLNOD SET TMGSLNOD=$GET(^SC(TMGIEN,"SL"))  ;"^SC(IEN,"SL", SL node
        SET TMGSDUR=+TMGSLNOD  ;"SL;1 = field 1912  LENGTH OF APP'T
        NEW TMGSOH SET TMGSOH=($PIECE(TMGSLNOD,"^",8)="Y") ;"SOH=Schedule On Holidays.
        ;
        ;"--Check for clinic innactivation dates.
        NEW TMGINACT,TMGREACT  ;"inactivation date / reactivation date
        IF +$GET(^SC(TMGIEN,"I"))>0 DO
        . SET TMGINACT=$PIECE(^SC(TMGIEN,"I"),"^",1)
        . SET TMGREACT=$PIECE(^SC(TMGIEN,"I"),"^",2)
        ELSE  SET (TMGINACT,TMGREACT)=0
        SET TMGERR=0
        DO  GOTO:(TMGERR>0) VDDONE
        . IF TMGINACT=0 QUIT
        . IF TMGINACT'<TMGLIMDT QUIT  ;"i.e. IF InactDate >= LimitDate QUIT
        . IF TMGREACT<TMG1DATE QUIT   ;"i.e. IF ReactDate < StartDate QUIT
        . SET TMGERR=1
        . NEW STR SET STR="Clinic is inactive"
        . SET STR=STR_$S(TMGREACT:" from ",1:" as of ")_$$EXTDAT(TMGINACT)
        . SET STR=STR_$S(TMGREACT:" to ",1:"")_$$EXTDAT(TMGREACT)
        . SET STR=STR_". CONFLICTS WITH GIVEN DATE RANGE."
        . SET TMGMSG(TMG1DATE_"^"_TMGLIMDT)="5^"_STR
        ;
        ;" -- Check for scheduling on holidays, IF not allowed --
        IF TMGSOH=0 DO
        . NEW DATE SET DATE=TMG1DATE
        . FOR  DO  SET DATE=$$ADD2DATE(DATE,7) QUIT:(DATE'<TMGLIMDT)!(DATE>(DT+5000))!TMGERR  ;"Only check vacations up to +5 yrs
        . . IF $DATA(^HOLIDAY(DATE,0))=0 QUIT
        . . SET TMGMSG(TMG1DATE_"^"_TMGLIMDT)="17^"_$$EXTDAT(DATE)_" is a holiday, and Location settings don't allow scheduling."
        . . SET TMGERR=1
        ;
        NEW TMGTIMES SET TMGTIMES=""
        FOR  SET TMGTIMES=$ORDER(@PARRAY@(TMGTIMES)) QUIT:(TMGTIMES="")!TMGERR  DO
        . IF TMGTIMES'?4N1"-"4N DO  QUIT
        . . SET TMGMSG(TMG1DATE)="6^Time ["_X_"] invalid.  Expected format e.g. '0800-1200.'"
        . . SET TMGRESULT=-1,TMGERR=1
        . NEW T1,T2,MIN
        . SET T1=$P(TMGTIMES,"-",1)
        . SET T2=$P(TMGTIMES,"-",2)
        . ;"---- Validate input times ------
        . DO VALIDMIL(TMG1DATE,T1,.TMGERR,.TMGMSG) QUIT:TMGERR
        . DO VALIDMIL(TMG1DATE,T2,.TMGERR,.TMGMSG) QUIT:TMGERR
        . IF T1<STRTTIME DO  QUIT
        . . SET TMGMSG(TMG1DATE)="8^Time ["_T1_"] invalid.  Cannot be earlier than clinic start time ("_STRTTIME_")."
        . . SET TMGRESULT=-1,TMGERR=1
        . IF T1<LASTTIME DO  QUIT
        . . SET TMGMSG(TMG1DATE)="9^Time ["_T1_"] invalid.  Must begin after last ending time ("_LASTTIME_")."
        . . SET TMGRESULT=-1,TMGERR=1
        . IF T2'>T1 DO  SET TMGERR=1 QUIT
        . . SET TMGMSG(TMG1DATE)="10^Time ["_TMGTIMES_"] invalid.  Must end after begin time ("_T2_")."
        . . SET TMGRESULT=-1,TMGERR=1
        . SET MIN=$$MILDELTA(T1,T2)
        . IF (MIN\TMGSDUR)*TMGSDUR'=+MIN DO  QUIT
        . . SET TMGMSG(TMG1DATE)="11^TIME SPAN ENTERED NOT CONSISTENT WITH "_TMGSDUR_" MIN APPT LENGTH"
        . . SET TMGRESULT=-1,TMGERR=1
        . ;"-- check for conflicting pre-existing appts...
        . IF TMGFLAGS'["I" DO  QUIT:TMGERR
        . . IF $$APPTCHK(TMGIEN,TMG1DATE,TMGLIMDT,T1,T2)=0 QUIT
        . . NEW STR SET STR="16^"_$$EXTDAT(TMG1DATE)
        . . SET TMGMSG(TMG1DATE_"^"_TMGLIMDT)=STR_" HAS PENDING APPT(S) - CAN NOT ALTER SLOTS UNLESS 'I' FLAG SET"
        . . SET TMGERR=1
        . ;"process individual times.
        . NEW APTSPER SET APTSPER=+$GET(@PARRAY@(TMGTIMES))
        . IF APTSPER'>0 DO  QUIT
        . . SET TMGMSG(TMG1DATE)="12^No Appts/Slot # specified for '"_TMGTIMES_"'"
        . . SET TMGRESULT=-1,TMGERR=1
        ;
VDDONE  QUIT
        ;
        ;
APPTCHK(TMGIEN,TMG1DATE,TMGLIMDT,ST,ET)
        ;"Purpose: Ensure appts are all CANCELLED in DATE RANGE
        ;"Input: TMGIEN -- the IEN in 44 of clinic
        ;"       TMG1DATE -- the starting date of date range to check
        ;"       TMGLIMDT -- the ending date of the date range to check
        ;"       ST -- OPTIONAL.  The start time of time range (in date range)
        ;"       ET -- OPTIONAL.  The end time of time range (in date range)
        ;"                      ET should be specified IF ST is specified
        ;"Result: 1 IF any non-cancelled appts are found
        NEW ADATE SET ADATE=TMG1DATE
        SET ST=+$GET(ST) SET ET=+$GET(ET)
        NEW CONFLICT SET CONFLICT=0
        FOR  SET ADATE=+$ORDER(^SC(TMGIEN,"S",ADATE)) QUIT:(ADATE'>0)!(ADATE'<TMGLIMDT)  DO
        . NEW SKIP SET SKIP=0
        . IF (ST>0)&(ET>0) DO  QUIT:SKIP  ;"If times provided, skip IF out of range.
        . . NEW TEMPST,TEMPET
        . . SET TEMPST=(ADATE\1)_"."_ST
        . . SET TEMPET=(ADATE\1)_"."_ET
        . . IF (ADATE<TEMPST)!(ADATE>TEMPET) SET SKIP=1 QUIT
        . NEW APPT SET APPT=0
        . FOR  SET APPT=+$ORDER(^SC(TMGIEN,"S",ADATE,1,APPT)) QUIT:(APPT'>0)!CONFLICT  DO
        . . IF $P(^SC(TMGIEN,"S",ADATE,1,APPT,0),"^",9)'["C" SET CONFLICT=1
        QUIT CONFLICT
        ;
        ;
EXTDAT(TMGADATE)
        ;" Get external time display
        IF +TMGADATE'>0 QUIT ""
        QUIT $TR($$FMTE^XLFDT(TMGADATE,"5DF")," ","0")
        ;
        ;
MILADD(TIME,ADDMIN,HR,MIN)
        ;"Purpose: Add time to TIME (in military format) and also return hours (HR) and minutes (MIN)
        ;"Input: TIME -- If passed by reference, will be changed to NEW time
        ;"       ADDMIN -- minutes to add to TIME
        ;"       HR -- PASS BY REFERENCE, the hours of the resulting time.
        ;"       MIN -- PASS BY REFERENCE, the minutes of the resulting time.
        ;"Results: none
        NEW H1,M1
        SET HR=$E(TIME,1,2)
        SET MIN=$E(TIME,3,4)
        SET MIN=MIN+ADDMIN
        FOR  QUIT:(MIN'>59)  SET MIN=MIN-60,HR=HR+1
        FOR  QUIT:(HR'>24)  SET HR=HR-24
        IF MIN?1N SET MIN="0"_MIN
        IF HR?1N SET HR="0"_HR
        SET TIME=HR_MIN
        QUIT
        ;
        ;
MILADD2(TIME,TIME2,HR,MIN)
        ;"Purpose: Add TIME2 to TIME (in military format) and also return hours (HR) and minutes (MIN)
        ;"Input: TIME -- If passed by reference, will be changed to NEW time
        ;"       TIME2 -- Time to add to TIME (both in military format, .e.g. 0845)
        ;"       HR -- PASS BY REFERENCE, the hours of the resulting time.
        ;"       MIN -- PASS BY REFERENCE, the minutes of the resulting time.
        ;"Results: Returns resulting added time.
        NEW HR1,MIN1,HR2,MIN2
        SET HR1=$E(TIME,1,2)
        SET MIN1=$E(TIME,3,4)
        SET HR2=$E(TIME2,1,2)
        SET MIN2=$E(TIME2,3,4)
        SET HR=HR1+HR2
        SET MIN=MIN1+MIN2
        FOR  QUIT:(MIN'>59)  SET MIN=MIN-60,HR=HR+1
        FOR  QUIT:(HR'>24)  SET HR=HR-24
        IF MIN?1N SET MIN="0"_MIN
        IF HR?1N SET HR="0"_HR
        SET TIME=HR_MIN
        QUIT TIME
        ;
        ;
MILSUB(TIME,SUBMIN,HR,MIN)
        ;"Purpose: Subtract minutes from TIME (in military format) and also return hours (HR) and minutes (MIN)
        ;"Input: TIME -- If passed by reference, will be changed to NEW time.  E.g. 0800
        ;"       SUBMIN -- minutes to subtract from TIME
        ;"       HR -- PASS BY REFERENCE, the hours of the resulting time.
        ;"       MIN -- PASS BY REFERENCE, the minutes of the resulting time.
        ;"Results: none
        NEW H1,M1
        SET HR=$E(TIME,1,2)
        SET MIN=$E(TIME,3,4)
        SET MIN=MIN-SUBMIN
        FOR  QUIT:(MIN>0)  SET MIN=MIN+60,HR=HR-1
        FOR  QUIT:(HR>0)  SET HR=HR+24
        IF MIN?1N SET MIN="0"_MIN
        IF HR?1N SET HR="0"_HR
        SET TIME=HR_MIN
        QUIT
        ;
        ;
MILSUB2(TIME,SUBTIME,HR,MIN)  ;"Unused
        ;"Purpose: Subtract minutes from TIME (in military format) and also return hours (HR) and minutes (MIN)
        ;"         TIME-SUBTIME=RESULT  (Result returned in HR and Min)
        ;"Input: TIME -- If passed by reference, will be changed to NEW time.  E.g. 1000
        ;"       SUBtime -- minutes to subtract from TIME.  E.g. 0800
        ;"       HR -- PASS BY REFERENCE, the hours of the resulting time.
        ;"       MIN -- PASS BY REFERENCE, the minutes of the resulting time.
        ;"Results: none
        NEW H1,M1
        NEW HR1,HR2
        NEW MIN1,MIN2
        SET HR1=$E(TIME,1,2)
        SET HR2=$E(SUBTIME,1,2)
        SET HR=HR1-HR2
        SET MIN1=$E(TIME,3,4)
        SET MIN2=$E(SUBTIME,3,4)
        SET MIN=MIN-1
        FOR  QUIT:(MIN>0)  SET MIN=MIN+60,HR=HR-1
        FOR  QUIT:(HR>0)  SET HR=HR+24
        IF MIN?1N SET MIN="0"_MIN
        IF HR?1N SET HR="0"_HR
        SET TIME=HR_MIN
        QUIT
        ;
        ;
VALIDMIL(TMGDATE,TIME,TMGERR,TMGMSG)
        ;"Purpose: Validate time
        ;"Input: TMGDATE -- A date to use for error reporting
        ;"       TIME -- time to validate.  E.g. 0815
        ;"       TMGERR -- PASS BY REFERENCE.  Flag for error
        ;"       TMGMSG -- PASS BY REFERENCE.  An array for error messages.
        ;
        NEW HR,MIN
        SET HR=$E(TIME,1,2)
        SET MIN=$E(TIME,3,4)
        IF (TIME'?4N)!(MIN>59)!(TIME>2400)!(+TIME=0) DO  SET TMGERR=1 QUIT
        . SET TMGMSG(TMGDATE)="6^Time ["_TIME_"] is not a valid time in MILITARY TIME format."
        . SET TMGRESULT=-1,TMGERR=1
        IF MIN\5*5'=+MIN DO  QUIT
        . SET TMGMSG(TMGDATE)="7^Time ["_TIME_"] invalid.  Must schedule appts on 5 minute boundries."
        . SET TMGRESULT=-1,TMGERR=1
        QUIT
        ;
        ;
MILDELTA(T1,T2)
        ;"Purpose: number of minutes T2-T1
        NEW H1,H2,M1,M2,MIN
        S H1=$E(T1,1,2),H2=$E(T2,1,2)
        SET M1=$E(T1,3,4) IF M1=0 SET M1=60
        SET M2=$E(T2,3,4) IF M2=0 SET M2=60
        IF M2=60 SET H2=H2-1
        IF M1=60 SET H1=H1-1
        SET MIN=M2-M1+((H2-H1)*60)
        QUIT MIN
        ;
        ;
GFMDATE(EXTDATE)
        ;"Purpose: return FM date from external date (no time)
        ;"        OR, IF already FM date, then just strip time.
        IF +EXTDATE=EXTDATE QUIT EXTDATE\1
        NEW %DT,X,Y
        SET %DT="X",X=EXTDATE
        DO ^%DT
        QUIT Y\1  ;"not sure IF this \1 is needed or not...
        ;
ADD2DATE(FMDATE,INCNUM)
        ;"Purpose: return FMDate + added IncNum
        ;"Note: Perhaps this could be speeded up by not using C^%DTC for some dates...
        NEW X,X1,X2 SET X1=FMDATE,X2=INCNUM
        DO C^%DTC ;"returns X (equals X1+X2)
        QUIT X
        ;
        ;