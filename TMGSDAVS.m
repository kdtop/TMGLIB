TMGSDAVS ;TMG/kst/Set Schedule Availability API ;12/08/08, 2/2/14
         ;;1.0;TMG-LIB;**1**;12/08/08
 ;
 ;"TMG SCHEDULING AVAILIBILITY SETTING
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
 ;"NOTE: Much of this code originated from SDB*.m
 ;
 ;"Called into from TMGRPC5
 ;"
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"SETAVAIL(TMGIEN,TMGPATRN,TMGFLAGS,TMGMSG) -- API to SET availability for a given clinic
 ;
 ;"=======================================================================
 ;" Private Functions.
 ;"=======================================================================
 ;"ONEDAY(TMGIEN,TMG1DATE,TMGLIMDT,TMGFLAGS,PARRAY,TMGMSG) -- SET the pattern for a date or date range
 ;"AVLSTR(TMGIEN,TMG1DATE) -- return an availability string showing appts.
 ;"MOV1DATE(OLDDATE,NEWDATE) -- Move 1 "T" node, and any linked OST nodes
 ;"FILTEMPL(TMIEN,TMG1DATE,TMGLIMDT,AVAILSTR) -- fill in Tx nodes (TEMPLATE) subfiles
 ;"MAKTMPL(TMGIEN,TMGLIMDT,AVAILSTR) -- Store the Tx node
 ;"KILTMPL(TMGIEN,DATE,DOW) -- Kill the Tx node for given date.
 ;"FIL1SPL(TMGIEN,TMG1DATE,AVAILSTR) -- Fill in 1 specified date, into "OST" nodes
 ;"KILLSPL(TMGIEN,TMG1DATE,TMGLIMDT) -- delete "OST" nodes for date range (and any linked "ST" nodes), only on same day of week.
 ;"Function below no longer used...
 ;"FIXPATRN(AVAILSTR,TMG1DATE,TMGLIMDT) Sets ST (PATTERN) and OST nodes, based on existing appts. (what else?)
 ;"
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;" XLFSTR, %DTC
 ;" TMGSDAU,TMGSDAU1,
 ;"=======================================================================
 ;
SETAVAIL(TMGIEN,TMGPATRN,TMGFLAGS,TMGMSG) ;
        ;"Purpose: API to SET availability for a given clinic
        ;"Input: TMGIEN -- IEN in HOSPITAL LOCATION file.
        ;"       TMGPATRN -- PASS BY REFERENCE.  Array Format:
        ;"         TMGPATRN(ADate_"^"_EndDate,ExtTime)=ApptsPerSlot
        ;"         TMGPATRN(ADate_"^"_EndDate,ExtTime)=ApptsPerSlot
        ;"             NOTE: Dates can be in external date format or FM Format
        ;"             ADate -- the date for appts, or beginning of date span
        ;"                      ADate=0 indicates an earliest possible date range start
        ;"             EndDate -- (OPTIONAL) The date to STOP the slots. (see below)
        ;"                 If LimitDate="I", then the date range has no end
        ;"                 If LimitDate=0 or "", then slots are SET up for just 1 day
        ;"             ExtTime -- External Time range for slots (Military time format).  E.g. 0830-1145
        ;"             IMPORTANT NOTES: If ADate is a MONDAY (for example), and the EndDate
        ;"                 is for 6 months later, then the slots will be applied
        ;"                 to *MONDAYS* during this interval, NOT all days during the
        ;"                 date range.  Also, the date range includes EndDate.
        ;"             Example: To SET up a one day with multiple times as folows:
        ;"                0800-0810  2 appt/slot  (2 appts both schedulable at 0800)
        ;"                0830-0850  1 appt/slot  (if 10 min slots ==> 2 appts)
        ;"                0900-0930  1 appt/slot  (==> 3 appts)
        ;"                1000-1140  1 appt/slot  (==> 10 appts)
        ;"             For the above schedule, pass the following data:
        ;"                TMGPATRN("ADate^I","0800-0810")=2
        ;"                TMGPATRN("ADate^I","0830-0850")=1
        ;"                TMGPATRN("ADate^I","0900-0930")=1
        ;"                TMGPATRN("ADate^I","1000-1140")=1
        ;"             This result in a availability entry something like below:
        ;"             | 2     1 1   | 1 1 1       | 1 1 1 1 1 1 | 1 1 1 1     |
        ;"         TMGFLAGS -- "D" = Delete appts (if not present then appts are SET)
        ;"                     "I" = Ignore existing appts when changing slots (TO BE IMPLEMENTED)
        ;"       TMGMSG -- PASS BY REFERENCE, an OUT PARAMETER.  Format:
        ;"           TMGMSG(FMDate)=Err#^Message
        ;"           TMGMSG(FMDate)=Err#^Message
        ;"      Errors#:
        ;"         1 - IEN in file 44 no provided
        ;"         2 - Type of clinic (field 3) is not CLINIC
        ;"         3 - OOS clinics (field 50.01) not supported by this API
        ;"         4 - Fields 1912-1918 not setup for clinic"
        ;"         5 - Clinic is inactive as of DATE,   or Clinic is inactive from DATE to DATE
        ;"         6 - Time [SomeTime] invalid.'
        ;"         7 - Time [SomeTime] invalid. Must schedule appts on 5 minute boundries"
        ;"         8 - Time [SomeTime] invalid. Cannot be earlier than clinic start time (StartTime)"
        ;"         9 - Time [SomeTime] invalid. Must begin after last ending time (SomeTime)."
        ;"        10 - Time [SomeTime] invalid. Must end after begin time (SomeTime)."
        ;"        11 - TIME SPAN ENTERED NOT CONSISTENT WITH "_TMGSDUR_" MIN APPT LENGTH"
        ;"        12 - No Appts/Slot specified for '"_TMGTIMES_"'"
        ;"        13 - Time [SomeTime] invalid.  "_H1_M1_" is > "_H2_M2_"."
        ;"        14 - Time [SomeTime] invalid."
        ;"        //15 - Invalid Mode ("_TMGMODE_").  Must be 1,2,11, or 12"
        ;"        16 - "$$EXTDAT^TMGSDAU1(TMG1DATE)_" HAS PENDING APPTS - CAN NOT ALTER SLOTS UNLESS 'I' FLAG SET"
        ;"        17 - "$$EXTDAT^TMGSDAU1(TMG1DATE)_" is a holiday, but File 44, Field 1918.5 doesn't allow scheduling."
        ;"        18 - Clinic is inactive from "_$$EXTDAT^TMGSDAU1((TMGINACT)_" to "_$$EXTDAT(TMGREACT1)
        ;
        ;"Result: 1 = Success  or
        ;"       -1 = error
        ;"        0 = Intermediate success
        ;
        ;"---Setup vars etc---
        NEW TMGRESULT SET TMGRESULT=1  ;"Default to success
        SET TMGIEN=+TMGIEN
        IF TMGIEN'>0 DO  GOTO SAVDONE
        . SET TMGMSG(0)="1^IEN in file 44 no provided"
        . SET TMGRESULT=-1
        IF $PIECE($GET(^SC(TMGIEN,0)),"^",3)'="C" DO  GOTO SAVDONE
        . SET TMGMSG(0)="2^Type of clinic (field 3) is not CLINIC"
        . SET TMGRESULT=-1
        IF $GET(^SC(TMGIEN,"OOS"))'="" DO  GOTO SAVDONE
        . SET TMGMSG(0)="3^OOS clinics (field 50.01) not supported by this API"
        . SET TMGRESULT=-1
        NEW TMGDATE  ;"TMGDATE=Start of date range
        NEW TMGLIMDT ;"TMGLIMDT=Limiting end of date range.  Will be specified EndDate+1
        NEW TMGABORT SET TMGABORT=0
        SET TMGFLAGS=$GET(TMGFLAGS)
        NEW TMGSLNOD SET TMGSLNOD=$GET(^SC(TMGIEN,"SL"))  ;"^SC(IEN,"SL", SL node
        IF TMGSLNOD="" DO  GOTO SAVDONE
        . SET TMGMSG(0)="4^Fields 1912-1918 are not setup for clinic (File 44)"
        . SET TMGRESULT=-1
        ;
        ;"Ensure subfile data structure
        IF '$D(^SC(TMGIEN,"T",0)) SET ^SC(TMGIEN,"T",0)="^44.002D"
        ;
        ;"---Loop through provided date ranges and process each sequentially
        NEW TMGDTRNG SET TMGDTRNG=""   ;"DATE RANGE
        FOR  SET TMGDTRNG=$ORDER(TMGPATRN(TMGDTRNG)) QUIT:(TMGDTRNG="")!TMGABORT  DO
        . NEW TEMPFLGS SET TEMPFLGS=TMGFLAGS
        . SET TMGABORT=$$SPLITDTS^TMGSDAU1(TMGDTRNG,.TMG1DATE,.TMGLIMDT,.TEMPFLGS,.TMGRESULT,.TMGMSG)
        . IF TMGABORT QUIT
        . LOCK +^SC(TMGIEN):10  ;"LOCK HERE
        . ELSE  DO  QUIT
        . . SET TMGMSG(TMG1DATE_"^"_TMGLIMDT)="Unable to get lock on ^SC("_TMGIEN_")."
        . . SET TMGRESULT=-1,TMGABORT=1
        . NEW TEMP
        . SET TEMP=$$ONEDAY(TMGIEN,TMG1DATE,TMGLIMDT,TEMPFLGS,$NAME(TMGPATRN(TMGDTRNG)),.TMGMSG)
        . LOCK -^SC(TMGIEN)  ;"RELEASE LOCK...
        . IF TEMP=-1 SET TMGRESULT=0 ;"Continue processing despite error encountered.
SAVDONE ;
        QUIT TMGRESULT
        ;
        ;
ONEDAY(TMGIEN,TMG1DATE,TMGLIMDT,TMGFLAGS,PARRAY,TMGMSG)
        ;"Purpose: To SET the pattern for a date or date range
        ;"Input: TMGIEN -- IEN of clinic to edit, in file 44
        ;"       TMG1DATE -- FM-format Date of reference
        ;"       TMGLIMDT -- FM-format *limit* date for appointments date range
        ;"       TMGFLAGS -- "D" = Delete appts (if not present then appts are SET)
        ;"                   "R" = Work on range from TMG1DATE up to, but not
        ;"                         including, limit date TMGLIMDT
        ;"                   "I" = Ignore existing appts when changing slots
        ;"       PARRAY -- PASS BY NAME.  FORMAT:
        ;"             -- ExtTime is external time: e.g. 0800-1315
        ;"           @PARRAY@(ExtTime)=#Appts/Slot
        ;"           @PARRAY@(ExtTime)=#Appts/Slot
        ;"       TMGMSG -- PASS BY REFERENCE.  An OUT PARAMETER.  See format above.
        ;"Globally-scoped vars used: ...
        ;"Note: It is presumed record locking has already occured
        ;"Example: To SET up a one day with multiple times as folows:
        ;"      TMGFLAGS=""
        ;"      0800-0810  2 appt/slot  (2 appts both schedulable at 0800)
        ;"      0830-0850  1 appt/slot  (if 10 min slots ==> 2 appts)
        ;"      0900-0930  1 appt/slot  (==> 3 appts)
        ;"      1000-1140  1 appt/slot  (==> 10 appts)
        ;"   For the above schedule, pass the following data:
        ;"      @PARRAY@("0800-0810")=2
        ;"      @PARRAY@("0830-0850")=1
        ;"      @PARRAY@("0900-0930")=1
        ;"      @PARRAY@("1000-1140")=1
        ;"   This result in a availability entry something like below:
        ;"   | 2     1 1   | 1 1 1       | 1 1 1 1 1 1 | 1 1 1 1     |
        ;"Output: ^SC(IEN,... is modified.
        ;"Result: 1 = Success  or
        ;"       -1 = error
        ;
        NEW TMGRESULT SET TMGRESULT=1
        NEW AVAILSTR
        NEW TMGERR SET TMGERR=0         ;"Clear Error flag
        ;
        ;"--Validate user input, including check for inactivation etc etc
        DO VALDATES^TMGSDAU1(TMGIEN,PARRAY,TMG1DATE,TMGLIMDT,TMGFLAGS,.TMGERR,.TMGMSG)
        IF TMGERR SET TMGRESULT=-1 GOTO ONEDDONE  ;"(Error details SET in VALDATES)
        ;
        ;"Delete any SPECIAL PATTERNS ("OST") entries during same date.
        DO KILLSPL(TMGIEN,TMG1DATE,TMGLIMDT)
        ;
        ;"Delete AVAILABILITY ("T") node, and any linked "ST" and "OST" nodes.
        DO KILLAVAL^TMGSDAU2(TMGIEN,TMG1DATE,TMGLIMDT,TMGFLAGS)
        ;
        ;" -- If just deleting, then KILL Avail and QUIT
        IF TMGFLAGS["D" DO  GOTO ONEDDONE
        . DO KILTMPL(TMGIEN,TMGLIMDT) ;"Kill the Tx node for given date.
        ;
        ;"Load AVAILABILITY subfile ("T" node), specifying num of Pts allowed in each slot
        DO FILLAVAL^TMGSDAU2(TMGIEN,PARRAY,TMG1DATE,TMGLIMDT,.TMGERR,.TMGMSG)
        ;"//kt 6/23/15  IF TMGERR GOTO OD2   ;"(Error details SET in FILLAVL)
        IF TMGERR SET TMGRESULT=-1 GOTO ONEDDONE   ;"(Error details SET in FILLAVL)
        ;
        SET AVAILSTR=$$AVLSTR(TMGIEN,TMG1DATE)
        IF TMGFLAGS'["R" DO  ;"I.e for 1 date, NOT RANGE
        . DO FIL1SPL(TMGIEN,TMG1DATE,AVAILSTR) ;"Fill in 1 specified date, into "OST" nodes
        . SET TMGRESULT=$$MAKE1ST^TMGSDAU(TMGIEN,TMG1DATE,.TMGMSG) ;"make/remake a "ST" node
        IF TMGFLAGS["R" DO   ;"I.e. for date RANGE
        . ;"Fill template Tx nodes, also sets ST and OST nodes, based on existing appts. (what else?)
        . DO FILTEMPL(TMGIEN,TMG1DATE,TMGLIMDT,AVAILSTR)
        . SET TMGRESULT=$$FRSH7ST^TMGSDAU(TMGIEN,TMG1DATE,.TMGMSG) ;"Fill in 7 ST nodes.
        ;
        ;"Note: there was some code here to trigger auto-rebook.. Will cut out for now.
ONEDDONE ;
        Q TMGRESULT
        ;
 ;"-----------------------------------------------------------------------------
 ;"  Support functions
 ;"-----------------------------------------------------------------------------
AVLSTR(TMGIEN,TMG1DATE)
        ;"Purpose: to return an availability string showing appts slots
        ;"Input: TMGIEN -- IEN of clinic in file 44
        ;"       TMG1DATE -- date to get string for
        ;"Create Y(pos) array to represent one line of availability. (Will utimately result in something like below)
        ;"   | 2     1 1   | 1 1 1       | 1 1 1 1 1 1 | 1 1 1 1     |
        ;
        NEW HSI  ;" ?? meaning..  <something> slot increments (slots/hr)
        NEW TMGSPH ;" display slots/hr
        NEW TMGSLNOD SET TMGSLNOD=$GET(^SC(TMGIEN,"SL"))  ;"^SC(IEN,"SL", SL node
        NEW TMGSDUR SET TMGSDUR=+TMGSLNOD ;"SL;1 = field 1912  LENGTH OF APP'T
        NEW STARTDAY SET STARTDAY=+$P(TMGSLNOD,U,3) ;"SL;3=HR CLINIC DISPLAY BEGINS
        IF STARTDAY'>0 SET STARTDAY=8     ;"Default to start at 8 am
        SET TMGSPH=+$P(TMGSLNOD,U,6)      ;"SL;6 = DISPLAY INCREMENTS PER HOUR (Slots per Hr)
        IF TMGSPH'>0 SET TMGSPH=4         ;"Default to 4 slots/hr
        SET HSI=TMGSPH
        IF TMGSPH=1 SET TMGSPH=4,HSI=1
        IF TMGSPH=2 SET TMGSPH=4,HSI=2
        ;
        NEW AVLARRAY,COUNT,Y,POS
        NEW RESULT SET RESULT=""
        SET DH=TMGSDUR*TMGSPH\60  ;"Minutes/slot * Slots/hr = Minutes/hr ; \60 ==> 1 IF all even.
        SET COUNT=0
        FOR  SET COUNT=+$ORDER(^SC(TMGIEN,"T",TMG1DATE,2,COUNT)) Q:COUNT'>0  DO
        . SET Y=^SC(TMGIEN,"T",TMG1DATE,2,COUNT,0) ;"0 node holds fields .01,1; +Y=time of slot,
        . FOR D=1:1:DH DO
        . . NEW MIN,HR
        . . SET MIN=Y#100
        . . SET HR=Y\100
        . . SET POS=MIN*TMGSPH\60+(HR*TMGSPH)-(STARTDAY*TMGSPH)+D
        . . NEW NUMPAT SET NUMPAT=+$PIECE(Y,U,2)
        . . SET AVLARRAY(POS)=$E("0123456789jklmnopqrstuvwxyz",NUMPAT+1) ;"code to show how many patients in slot.
        IF $DATA(AVLARRAY)=0 DO  GOTO AVSDONE
        . ;"SET SDEL=1   --> SOMETHING NEEDS TO BE CLEARED OUT?
        IF $DATA(HSI) DO
        . IF (HSI'=1)&(HSI'=2) QUIT
        . ;"Remove elements of Y array that don't fall in increments of HSI
        . NEW X,INC,DONE,TEMPY
        . SET INC=$SELECT(HSI=1:4,1:2)
        . SET DONE=0
        . FOR X=$ORDER(Y(-1)):INC Q:(X>41)!DONE  DO
        . . IF $DATA(Y(X)) SET TEMPY(X)="" QUIT
        . . IF $ORDER(Y(X))'>0  SET DONE=1 QUIT
        . . SET X=$ORDER(Y(X-1))-INC
        . SET X=0
        . FOR  SET X=$ORDER(Y(X)) Q:X'>0  DO
        . . IF '$DATA(TEMPY(X)) KILL Y(X)
        NEW DNOW,DLAST,VALUE
        SET (DNOW,DLAST)=0,Y=1,VALUE=" "
        FOR POS=1:1 DO  IF 'DNOW,$ORDER(AVLARRAY(POS))'>0 QUIT
        . SET DNOW=$DATA(AVLARRAY(POS))
        . SET VALUE=$GET(AVLARRAY(POS)," ")
        . IF ('DNOW)&(DLAST) SET SYM="]"
        . ELSE  IF (DNOW)&('DLAST) SET SYM="["
        . ELSE  IF POS#TMGSPH=1 SET SYM="|"
        . ELSE  SET SYM=" "
        . SET RESULT=RESULT_SYM_VALUE
        . SET DLAST=DNOW
AVSDONE
        QUIT RESULT
        ;
        ;
MOV1DATE(OLDDATE,NEWDATE)  ;"Unused??
        ;"Purpose: Move 1 "T" node, and any linked OST nodes
        ;"Input: OLDDATE
        ;"       NEWDATE
        ;"Globally-scoped vars used: TMGIEN
        ;"Note: It is presumed record locking has already occured
        NEW TEMP MERGE TEMP=^SC(TMGIEN,"T",OLDDATE)
        NEW TEMP2 MERGE TEMP2=^SC(TMGIEN,"OST",OLDDATE)
        DO KILL1DATE^TMGSDAU2(OLDDATE,1)
        MERGE ^SC(TMGIEN,"T",NEWDATE)=TEMP
        SET ^SC(TMGIEN,"T",NEWDATE,0)=NEWDATE
        IF $DATA(TEMP2) DO
        . SET ^SC(TMGIEN,"OST",NEWDATE)=TEMP2
        . SET ^SC(TMGIEN,"OST",NEWDATE,0)=NEWDATE
        QUIT
        ;
        ;
FILTEMPL(TMIEN,TMG1DATE,TMGLIMDT,AVAILSTR)
        ;"Purpose: To fill in Tx nodes (TEMPLATE) subfiles
        ;"Input: TMGIEN -- IEN in file 44
        ;"       TMG1DATE -- Start of date range.  Use 0 for earliest possible
        ;"       TMGLIMDT -- Limit of date range (range is up to BUT NOT INCLUDINGE this date)
        ;"       AVAILSTR
        ;"Globally scoped vars used: ..
        ;"Result: none
        ;"Note: It is presumed record locking has already occured
        ;
        ;"Note: I am not going to screen for clinic inactivation.  If a TEMPLATE
        ;"   is SET today for the next year, and then the clinic inactivation
        ;"   is specified to occur in 6 months, I don't know how to handle that.
        ;"   There was some code to see IF there was an exact match between
        ;"   some of the dates here, and clinic inactivation dates.  But I don't
        ;"   why I should check particular days, when we are dealing with *ranges*
        ;
        ;"EXAMPLES OF POSSIBLE PATTERNS...
        ;"================================
        ;"Imagine that there exists four patterns A,B,C,E, with LIMIT
        ;"dates of LA,LB,LC,99999999
        ;"A timeline will be shown with the various limits
        ;">--------------------------------------------------99999999
        ;"E.g.   LA        LB        LC            LE
        ;">------>--------->--------->------------->---------99999999
        ;"And then the ranges will be filled with the letters for that range (see below)
        ;
        ;
        ;" ----------------------Example -----------------------------
        ;"And we add a NEW range from Start-->End (named D)
        ;"(New range overrides range 1+ another range)
        ;"         LA         LB         LC            9999999
        ;"  >aaaaaa>bbbbbbbbbb>cccccccccc>eeeeeeeeeeeee>
        ;"             +======================>D-End
        ;"Should result in...
        ;"         LA  LB                     LD        9999999
        ;"  >aaaaaa>bbb>dddddddddddddddddddddd>eeeeeeeee>
        ;"The following must happen:
        ;" Range AB is shortened so that B is at D-Start
        ;"    (i.e. next limit occuring after D-Start is changed so that
        ;"     limit is the same as D-Start)
        ;" Any LIMIT entries before D-End are removed
        ;" D-End is stored as limit of last
        ;
        ;" ----------------------Example -----------------------------
        ;"New range is entirely inside another range (dividing it into 2 parts)
        ;"         LA   LB                 LC            9999999
        ;"  >aaaaaa>bbbb>cccccccccccccccccc>eeeeeeeeeeeee>
        ;"                  +=========>D-End
        ;"Should result in...
        ;"         LA    LB LC1        LD  LC2            9999999
        ;"  >aaaaaa>bbbbb>cc>dddddddddd>ccc>eeeeeeeeeeeeee>
        ;"    Next limit occuring after D-Start is changed so that
        ;"     limit is the same as D-Start, UNLESS it is also occurs
        ;"     after D-End.  In that case it is left in place and copied
        ;"     instead.

        ;" ----------------------Example -----------------------------
        ;"New Range preceeds other ranges
        ;"                  LA             LB          LC        9999999
        ;"  >aaaaaaaaaaaaaaa>bbbbbbbbbbbbbb>ccccccccccc>eeeeeeeee>
        ;"      +=======>D-End
        ;"Should result in...
        ;"      LA       LD LA             LB          LC        9999999
        ;"  >aaaddddddddd>aa>bbbbbbbbbbbbbb>ccccccccccc>eeeeeeeee>
        ;"    Next limit occuring after D-Start is changed so that
        ;"     limit is the same as D-Start, UNLESS it is also occurs
        ;"     after D-End.  In that case it is left in place and copied
        ;"     instead.  UNLESS there is no prior limit

        ;" ----------------------Example -----------------------------
        ;"New Range should be the NEW ending range
        ;"     LA    LB      LC                        9999999
        ;"  >aa>bbbbb>ccccccc>eeeeeeeeeeeeeeeeeeeeeeeee>
        ;"                       +==========================>D-End
        ;"Should result in...
        ;"     LA    LB      LC  LE                         9999999
        ;"  >aa>bbbbb>ccccccc>eee+dddddddddddddddddddddddddd>
        ;"    Next limit occuring after D-Start is changed so that
        ;"     limit is the same as D-Start, UNLESS it is also occurs
        ;"     after D-End.  In that case it is left in place and copied
        ;"     instead.  UNLESS there is no prior limit.
        ;"

        ;"RULES TO HANDLE ABOVE.
        ;"   1. Does NEW start date=0 ?
        ;"      IF YES, then there is no earlier start dates.
        ;"              skip step 3
        ;"      IF NO, then treat at others.
        ;"   2. Get next limit after start date
        ;"       Is this next date AFTER end date?
        ;"       If YES, then NEW range is inside another.  BEGIN
        ;"           create a new, extra, entry, with limit date=start date
        ;"           Done.
        ;"       IF NO, then begin
        ;"              is there already an entry with LIMIT same as start date?
        ;"              IF NO: Move this to NEW start date. (i.e. make it's limit to equal New Start Date)
        ;"              IF YES, then is prior limit date same as this start date?
        ;"              IF NO, then just delete this entry  ?????
        ;"              IF YES, needs split... FINISH...?????
        ;"   3.  Cycle through each limit after step 2, and delete all
        ;"        that occur before OR AT (i.e. <= ) NEW end date reached.
        ;"   4.  Create NEW entry with limit date of End date.
        ;
        NEW DOW SET DOW=$$DOW^XLFDT(TMG1DATE,1) ;"DOW=Day of Week (0-6)
        NEW NEXT SET NEXT=$ORDER(^SC(TMGIEN,"T"_DOW,TMG1DATE))
        IF TMGLIMDT=0 GOTO FT2
        IF NEXT>TMGLIMDT DO  GOTO FT3
        . DO MAKTMPL(TMGIEN,TMG1DATE,$GET(^SC(TMGIEN,"T"_DOW,NEXT,1)))
        ELSE  IF +NEXT>0 DO
        . IF $DATA(^SC(TMGIEN,"T"_DOW,TMG1DATE)) DO  QUIT
        . . DO KILTMPL(TMGIEN,NEXT,DOW)  ;"Kill the Tx node for given date.
        . DO MAKTMPL(TMGIEN,TMG1DATE,$GET(^SC(TMGIEN,"T"_DOW,NEXT,1)))
        . DO KILTMPL(TMGIEN,NEXT,DOW)    ;"Kill the Tx node for given date.
FT2     FOR  SET NEXT=$ORDER(^SC(TMGIEN,"T"_DOW,NEXT)) QUIT:(+NEXT'>0)!(+NEXT>TMGLIMDT)  DO
        . DO KILTMPL(TMGIEN,NEXT,DOW)    ;"Kill the Tx node for given date.
FT3     DO MAKTMPL(TMGIEN,TMGLIMDT,AVAILSTR)
        ;"Ensure header.
        IF '$DATA(^SC(TMGIEN,"T"_DOW,0)) DO
        . SET ^SC(TMGIEN,"T"_DOW,0)="^44.0"_$S(DOW<4:DOW+6,DOW<6:"0"_DOW+4,1:"001")_"A^^"
        ;
        QUIT
        ;
        ;
MAKTMPL(TMGIEN,TMGLIMDT,AVAILSTR)
        ;"Purpose: Store the Tx node
        ;"Check for clinic inactivation should have already taken place during validation
        ;"Note: It is presumed record locking has already occured
        NEW DOW SET DOW=$$DOW^XLFDT(TMG1DATE,1) ;"DOW=Day of Week (0-6)
        SET ^SC(TMGIEN,"T"_DOW,TMGLIMDT,1)=AVAILSTR
        SET ^SC(TMGIEN,"T"_DOW,TMGLIMDT,0)=TMGLIMDT
        QUIT
        ;
        ;
KILTMPL(TMGIEN,DATE,DOW)
        ;"Purpose: Kill the Tx node for given date.
        ;"Input: TMGIEN
        ;"       DATE
        ;"       DOW -- Optional.  Day of week that DATE falls on (0-6)
        ;"Check for existing appts should have already taken place during validation
        ;"Note: It is presumed record locking has already occured
        SET DOW=$GET(DOW)
        IF (DOW="")!(+DOW>6)!(+DOW<0) SET DOW=$$DOW^XLFDT(DATE,1) ;"DOW=Day of Week (0-6)
        KILL ^SC(TMGIEN,"T"_DOW,DATE)
        QUIT
        ;
        ;
FIL1SPL(TMGIEN,TMG1DATE,AVAILSTR)
        ;"Purpose: Fill in 1 specified date, into "OST" nodes
        ;"Note: It is presumed record locking has already occured
        NEW STR SET STR=$$SPECPAT^TMGSDAU(TMGIEN,TMG1DATE,AVAILSTR)
        IF STR'="" DO
        . SET ^SC(TMGIEN,"ST",TMG1DATE,0)=TMG1DATE
        . SET ^SC(TMGIEN,"ST",TMG1DATE,1)=STR
        . IF '$DATA(^SC(TMGIEN,"ST",0)) SET ^(0)="^44.005DA^^"
        . SET ^SC(TMGIEN,"ST",TMG1DATE,9)=TMGIEN  ;"9 node --> use OST node for special availability
        . SET ^SC(TMGIEN,"OST",TMG1DATE,0)=TMG1DATE
        . SET ^SC(TMGIEN,"OST",TMG1DATE,1)=STR
        . IF '$DATA(^SC(TMGIEN,"OST",0)) SET ^(0)="^44.0002DA^^"
        IF $GET(^SC(TMGIEN,"ST",0))="" SET ^SC(TMGIEN,"ST",0)="^44.005DA^^"
        ;
        QUIT
        ;
        ;
KILLSPL(TMGIEN,TMG1DATE,TMGLIMDT)
        ;"Purpose: To delete "OST" nodes for date range (and any linked "ST" nodes), only on same day of week.
        ;"Note: It is presumed record locking has already occured
        NEW DATE SET DATE=TMG1DATE
        NEW DOW SET DOW=$$DOW^XLFDT(DATE,1)
        FOR  DO  SET DATE=$ORDER(^SC(TMGIEN,"OST",DATE)) QUIT:(+DATE'>0)!(DATE'<TMGLIMDT)!(DATE'<DT+50000)
        . IF $$DOW^XLFDT(DATE,1)'=DOW QUIT
        . KILL ^SC(TMGIEN,"ST",DATE)
        . KILL ^SC(TMGIEN,"OST",DATE)
        QUIT
        ;
        ;
 ;"Function below no longer used...
FIXPATRN(AVAILSTR,TMG1DATE,TMGLIMDT)  ;"Used to be B1^SDB1
        ;"Purpose: Sets ST (PATTERN) and OST nodes, based on existing appts. (what else?)
        ;"Input: AVAILSTR=PATTERN  (was DH)
        ;"       TMG1DATE=START DATE  (was X)
        ;"       TMGLIMDT=EXPIRATION DATE
        ;"Globally scoped vars used: TMGIEN, DT(standard environ var)
        ;"Note: It is presumed record locking has already occured
        ;
        NEW TMGSPH
        SET TMGSPH=+$P($GET(^SC(TMGIEN,"SL")),"^",6) ;"SL;6 = DISPLAY INCS PER HOUR (Slots per Hr)
        IF TMGSPH'>0 SET TMGSPH=4       ;"Default to 4 slots/hr
        NEW STARTDAY SET STARTDAY=+$P($GET(^SC(TMGIEN,"SL")),"^",3) ;"SL;3=HR CLINIC DISPLAY BEGINS
        NEW NXTAPPT SET NXTAPPT=0
        NEW SB SET SB=(STARTDAY-1)/100  ;"Eg 8 --> .07
        NEW STR SET STR="{}&%?#@!$* XXWVUTSRQPONMLKJIHGFEDCBA0123456789jklmnopqrstuvwxyz"
        NEW SDONE SET SDONE=1
        NEW TEMPPAT SET TEMPPAT=""
        NEW SDPAT SET SDPAT=""
        NEW HSI SET HSI=$S('TMGSPH:4,TMGSPH<3:8/TMGSPH,1:2)
        NEW SI SET SI=+TMGSPH
        IF SI=0 SET SI=4
        NEW SDSI SET SDSI=SI
        IF (SI=1)!(SI=2) SET SI=4
        ;
        ;"--Start loop---
        NEW DONE SET DONE=0
        SET DATE=TMG1DATE
        FOR  DO  SET DATE=$$ADD2DATE^TMGSDAU1(DATE,7) QUIT:(DATE'<TMGLIMDT)!DONE!(DATE'<(DT+50000))
        . NEW SKIP SET SKIP=0
        . SET TEMPPAT=$GET(^SC(TMGIEN,"ST",DATE,1))
        . IF TEMPPAT["**CANCELLED**"!(TEMPPAT["X") DO
        . . SET ^TMP("SDAVAIL",$J,DATE)=TEMPPAT
        . SET NXTAPPT=+$ORDER(^SC(TMGIEN,"S",DATE)) ;"Get DateTime of next appt
        . IF $DATA(^SC(TMGIEN,"ST",DATE,9)) DO    ;"Does flag for special OST node exist?
        . . SET NXTAPPT=DATE,SDSAV=0
        . ELSE  DO  QUIT:(SKIP=1)
        . . KILL ^SC(TMGIEN,"ST",DATE) ;"Del PATTERN subfile entry for start date
        . . IF NXTAPPT'>0,'$ORDER(^SC(TMGIEN,"ST",DATE)) DO  QUIT
        . . . SET DONE=1
        . . . SET SKIP=1
        . . IF DATE+1<NXTAPPT DO  QUIT
        . . . SET DATE=$$ADD2DATE^TMGSDAU1(DATE,7)
        . . . SET SKIP=1
        . . SET SDSAV=0
        . . IF (NXTAPPT\1)'=DATE DO  QUIT  ;"If next appt is on different day then keep scanning
        . . . SET DATE=$$ADD2DATE^TMGSDAU1(DATE,7)
        . . . SET SKIP=1
        . ;"-- Fix for entries on same day as NEW pattern supplied --
        . SET SM=$$SPECPAT^TMGSDAU(TMGIEN,DATE,AVAILSTR)
        . IF 'SDSAV SET SDSAV=1,SDPAT=SM
        . FOR  DO  SET NXTAPPT=+$ORDER(^SC(TMGIEN,"S",NXTAPPT)) QUIT:(NXTAPPT\1'=DATE)
        . . NEW I SET I=(NXTAPPT#1-SB)*100
        . . SET I=I#1*SI\.6+(I\1*SI)*2
        . . NEW S SET S=$EXTRACT(SM,I,999)
        . . SET SM=$EXTRACT(SM,1,I-1)
        . . SET Y=0
        . . FOR  SET Y=$ORDER(^SC(TMGIEN,"S",NXTAPPT,1,Y)) Q:Y'>0  DO
        . . . IF $PIECE(^(Y,0),"^",9)["C" QUIT  ;"ignore IF appt cancelled
        . . . SET SDSL=$PIECE(^(0),"^",2)/TMGSDUR*(TMGSDUR\(60/SDSI))*HSI-HSI
        . . . FOR I=0:HSI:SDSL DO
        . . . . SET ST=$EXTRACT(S,I+2)
        . . . . IF ST="" SET ST=" "
        . . . . SET S=$E(S,1,I+2-1)_$E(STR,$F(STR,ST)-2)_$E(S,I+3,999)
        . . . . DO   ;"WAS D OB in old code
        . . . . . SET SDSLOT=$EXTRACT(STR,$F(STR,ST)-2)
        . . . . . IF (SDSLOT?1P),(SDSLOT'?1" ") DO  QUIT
        . . . . . . SET ^SC(TMGIEN,"S",NXTAPPT,1,Y,"OB")="O"  ;"OB = overbook field
        . . . . . . KILL SDSLOT
        . . . . . KILL ^SC(TMGIEN,"S",NXTAPPT,1,Y,"OB")
        . . . . . KILL SDSLOT
        . . SET SM=SM_S
        . IF $L(SM)>SM DO
        . . SET ^SC(TMGIEN,"ST",DATE,0)=DATE
        . . SET ^SC(TMGIEN,"ST",DATE,1)=SM
        . . IF '$D(^SC(TMGIEN,"ST",0)) SET ^(0)="^44.005DA^^"
        . . IF $D(^SC(TMGIEN,"ST",DATE,9)) DO
        . . . SET ^SC(TMGIEN,"OST",DATE,0)=DATE
        . . . SET ^SC(TMGIEN,"OST",DATE,1)=SDPAT
        . . . IF '$D(^SC(TMGIEN,"OST",0)) SET ^(0)="^44.0002DA^^"
        . SET SDCAN=DATE
        . FOR  SET SDCAN=$O(^SC(TMGIEN,"SDCAN",SDCAN)) Q:(SDCAN\1-(DATE\1))!'SDCAN  DO
        . . KILL ^SC(TMGIEN,"SDCAN",SDCAN)
        ;
FPTNDONE ;
        QUIT
        ;
        ;