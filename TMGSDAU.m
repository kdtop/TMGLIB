TMGSDAU  ;TMG/kst/Schedule Availability Utilities ;1/06/09, 2/2/14
         ;;1.0;TMG-LIB;**1**;12/08/08
 ;
 ;"TMG SCHEDULING AVAILIBILITY UTILITIES
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
 ;"NOTE: Much of this code originated from SDB*.m (heavily modified!)
 ;"
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"FRSH7ST(TMGIEN,TMG1DATE,TMGMSG) -- Refresh 7 weeks of "ST" nodes, starting at given date.
 ;"ENSUR1ST(TMGIEN,TMG1DATE,TMGMSG) -- Ensure a "ST" node exists for a given date.
 ;"FORCE1ST(TMGIEN,TMG1DATE,TMGMSG) -- make/remake a "ST" node for a given date.
 ;"PAT4DAY(TMGIEN,TMG1DATE,TMGARR,TMGMSG) -- return a pattern appropriate for placing in "ST" for date.
 ;"FIX1ST(TMGIEN,TMG1DATE,TMGMSG) -- SET slot numbers to match existing appts.
 ;"DEC1SLOT(TMGIEN,APPT,TMGMSG) -- decrement the availability number for a slot at a given time
 ;"SLTINDEX(TMGIEN,APPT,SAVARR) -- return INDEX in "ST" PATTERN node for given appt slot time
 ;"SPECPAT(TMGIEN,DATE,AVAILSTR) -- Add header to AvailStr
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"=======================================================================
 ;
FRSH7ST(TMGIEN,TMG1DATE,TMGMSG)
        ;"Purpose: To Refresh 7 weeks of "ST" nodes, starting at given date.
        ;"              (All on same day of week.)
        ;"         It will copy from LIMDTate nodes IF needed, and then check for
        ;"         any existing appts on that date, and add them IF needed.
        ;"         NOTE: IF the "ST" node already exists, it Will be remade.
        ;"Input: TMGIEN -- IEN in file 44 to work on
        ;"       TMG1DATE -- the date to start refreshing ST on
        ;"       TMGMSG -- PASS BY REFERENCE, an OUT PARAMETER.  Format:
        ;"              TMGMSG=line count of error messages
        ;"              TMGMSG(1)=ErrMsg
        ;"              TMGMSG(2)=ErrMsg etc..
        ;"Globally-scoped vars used: ...
        ;"Result: 1 = Success  or
        ;"        0 = Intermediate success
        ;"       -1 = error
        ;
        NEW TMGRESULT SET TMGRESULT=1
        LOCK +^SC(TMGIEN,"ST"):10
        ELSE  DO  GOTO FR7DONE
        . SET TMGMSG=+$GET(TMGMSG)+1
        . SET TMGMSG(TMGMSG)="Unable to get lock on ^SC("_TMGIEN_",""ST"",)."
        . SET TMGRESULT=-1
        NEW COUNT,DATE
        SET DATE=TMG1DATE
        FOR COUNT=1:1:7 DO  QUIT:(TMGRESULT'=1)
        . KILL ^SC(TMGIEN,"ST",DATE)
        . SET TMGRESULT=$$FORCE1ST(TMGIEN,DATE,.TMGMSG)
        . SET DATE=$$ADD2DATE^TMGSDAU1(DATE,7)
        LOCK -^SC(TMGIEN,"ST")
FR7DONE ;
        QUIT TMGRESULT
        ;
        ;
ENSUR1ST(TMGIEN,TMG1DATE,TMGMSG)
        ;"Purpose: To Ensure a "ST" node exists for a given date.
        ;"         It will copy from LIMDTate nodes IF needed, and then check for
        ;"         any existing appts on that date, and add them IF needed.
        ;"         NOTE: IF the "ST" node already exists, it will NOT be remade.
        ;"Input: TMGIEN -- IEN in file 44 to work on
        ;"       TMG1DATE -- the date to force ST for.  Don't pass by reference
        ;"       TMGMSG -- PASS BY REFERENCE, an OUT PARAMETER.  Format:
        ;"              TMGMSG=line count of error messages
        ;"              TMGMSG(1)=ErrMsg
        ;"              TMGMSG(2)=ErrMsg etc..
        ;"Globally-scoped vars used: ...
        ;"Result: 1 = Success  or
        ;"        0 = Intermediate success
        ;"       -1 = error
        ;
        NEW TMGRESULT SET TMGRESULT=1
        SET TMG1DATE=$GET(TMG1DATE)\1
        LOCK +^SC(TMGIEN,"ST",TMG1DATE):10
        ELSE  DO  GOTO E1STDONE
        . SET TMGMSG=+$GET(TMGMSG)+1
        . SET TMGMSG(TMGMSG)="Unable to get lock on ^SC("_TMGIEN_",""ST"","_TMG1DATE_")."
        . SET TMGRESULT=-1
        IF $DATA(^SC(TMGIEN,"ST",TMG1DATE))=0 DO
        . SET TMGRESULT=$$FORCE1ST(TMGIEN,TMG1DATE,.TMGMSG)
        LOCK -^SC(TMGIEN,"ST",TMG1DATE)
E1STDONE ;
        QUIT TMGRESULT
        ;
        ;
FORCE1ST(TMGIEN,TMG1DATE,TMGMSG)
        ;"Purpose: To make/remake a "ST" node for a given date.
        ;"         It will copy from LIMDTate nodes IF needed, and then check for
        ;"         any existing appts on that date, and add them IF needed.
        ;"         NOTE: IF the "ST" node already exists, it WILL be remade.
        ;"Input: TMGIEN -- IEN in file 44 to work on
        ;"       TMG1DATE -- the date to force ST for.
        ;"       TMGMSG -- PASS BY REFERENCE, an OUT PARAMETER.  Format:
        ;"              TMGMSG=line count of error messages
        ;"              TMGMSG(1)=ErrMsg
        ;"              TMGMSG(2)=ErrMsg etc..
        ;"Globally-scoped vars used: ...
        ;"Result: 1 = Success  or
        ;"        0 = Intermediate success
        ;"       -1 = error
        ;
        NEW TMGRESULT,TMGARR
        NEW DATE SET DATE=TMG1DATE\1
        LOCK +^SC(TMGIEN,"ST",DATE):10
        ELSE  DO  GOTO M1STDONE
        . SET TMGMSG=+$GET(TMGMSG)+1
        . SET TMGMSG(TMGMSG)="Unable to get lock on ^SC("_TMGIEN_",""ST"","_DATE_")."
        . SET TMGRESULT=-1
        SET TMGRESULT=$$PAT4DAY(TMGIEN,TMG1DATE,.TMGARR,.TMGMSG)
        IF TMGRESULT'=1 GOTO M1STDONE
        KILL ^SC(TMGIEN,"ST",DATE)
        MERGE ^SC(TMGIEN,"ST",DATE)=TMGARR
        IF $$FIX1ST(TMGIEN,TMG1DATE,.TMGMSG)=-1 SET TMGRESULT=0
        LOCK -^SC(TMGIEN,"ST",TMG1DATE)
M1STDONE ;
        QUIT TMGRESULT
        ;
        ;
PAT4DAY(TMGIEN,TMG1DATE,TMGARR,TMGMSG)
        ;"Purpose: To return a pattern appropriate for placing in "ST" for date.
        ;"Input: TMGIEN -- IEN in file 44 to work on
        ;"       TMG1DATE -- the date to work on.
        ;"       TMGARR -- PASS BY REFERENCE.  An OUT PARAMETER.  Prior results killed.
        ;"                This is an array that may be MERGEd with ^SC(TMGIEN,"ST",DATE)
        ;"       TMGMSG -- PASS BY REFERENCE, an OUT PARAMETER.  Format:
        ;"              TMGMSG=line count of error messages
        ;"              TMGMSG(1)=ErrMsg
        ;"              TMGMSG(2)=ErrMsg etc..
        ;"Globally-scoped vars used: ...
        ;"Result: 1 = Success  or
        ;"       -1 = error
        ;
        KILL TMGARR
        NEW TMGRESULT SET TMGRESULT=-1 ;"default to failure
        NEW TMGSLNOD SET TMGSLNOD=$GET(^SC(TMGIEN,"SL"))  ;"^SC(IEN,"SL", SL node
        NEW TMGSOH SET TMGSOH=($PIECE(TMGSLNOD,"^",8)="Y") ;"SOH=Schedule On Holidays.
        NEW DATE SET DATE=TMG1DATE\1  ;"strip minutes
        NEW DOW SET DOW=$$DOW^XLFDT(DATE,1)#7
        ;
        IF $DATA(^HOLIDAY(DATE))&('TMGSOH) DO  GOTO P4DDONE
        . SET TMGMSG=+$GET(TMGMSG)+1
        . SET TMGMSG(TMGMSG)=$$EXTDAT^TMGSDAU1(DATE)_" is a holiday, and Location settings don't allow scheduling."
        ;
        IF $DATA(^SC(TMGIEN,"OST",DATE,1)) DO
        . MERGE TMGARR=^SC(TMGIEN,"OST",DATE)
        . SET TMGARR(9)=TMGIEN
        . SET TMGRESULT=1
        ;
        ;"IF '$DATA(^SC(TMGIEN,"ST",DATE,1)) DO  ;"Copy from TEMPLATE for this day, date
        IF TMGRESULT'=1 DO  ;"Copy from TEMPLATE for this day, date
        . NEW STR
        . NEW LIMDT SET LIMDT=+$ORDER(^SC(TMGIEN,"T"_DOW,DATE)) ;"Tx entries are LIMIT dated...
        . IF LIMDT'>0 QUIT
        . NEW TEMPL SET TEMPL=$GET(^SC(TMGIEN,"T"_DOW,LIMDT,1))
        . IF TEMPL="" QUIT
        . SET STR=$$SPECPAT(TMGIEN,DATE,TEMPL)  ;"Return string like this: MO 05  |       [2 2 2 2|2 2 2 2]
        . SET TMGARR(1)=STR
        . SET TMGARR(0)=DATE
        . SET TMGRESULT=1
        ;
        IF TMGRESULT=-1 DO
        . SET TMGMSG=+$GET(TMGMSG)+1
        . SET TMGMSG(TMGMSG)="NO TEMPLATE; Unable to find a slot pattern defined for "_$$EXTDAT^TMGSDAU1(DATE)
        ;
P4DDONE ;
        QUIT TMGRESULT
        ;
        ;
FIX1ST(TMGIEN,TMG1DATE,TMGMSG)
        ;"Purpose: To SET slot numbers to match existing appts.
        ;"IMPORTANT NOTICE: This should *only* be called after a fresh template pattern
        ;"      has been copied into the ST node.  This is because this function
        ;"      will decrease availability count for slots based on existing appts.
        ;"      If this has already been done, then calling this again will result
        ;"      in the availability count being reduced AGAIN--making it appear
        ;"      that the slot is being used, when it actually is NOT.
        ;"Input: TMGIEN -- IEN in file 44 to work on
        ;"       TMG1DATE -- the date to fix ST for.
        ;"       TMGMSG -- PASS BY REFERENCE, an OUT PARAMETER.  Format:
        ;"              TMGMSG=line count of error messages
        ;"              TMGMSG(1)=ErrMsg
        ;"              TMGMSG(2)=ErrMsg etc..
        ;"Globally-scoped vars used: TMGIEN
        ;"Result: 1 = Success  or
        ;"       -1 = error
        ;
        NEW TMGRESULT SET TMGRESULT=1
        NEW APPT SET APPT=TMG1DATE\1  ;"All appts should have time, by trimming time, will sort before actual appts
        FOR  SET APPT=$ORDER(^SC(TMGIEN,"S",APPT)) QUIT:(APPT\1'=TMG1DATE\1)!(TMGRESULT=-1)  DO  ;"Only check same day
        . IF $$NONAPPT(TMGIEN,APPT) QUIT
        . SET TMGRESULT=$$DEC1SLOT(TMGIEN,APPT,.TMGMSG)
F1STDONE ;
        QUIT TMGRESULT
        ;
        ;
DEC1SLOT(TMGIEN,APPT,TMGMSG)
        ;"Purpose: To decrement the availability number for a slot at a given time
        ;"Input: TMGIEN -- IEN in file 44 to work on
        ;"       APPT -- A FMDateTime number to indicate date & time of appt.
        ;"       TMGMSG -- PASS BY REFERENCE, an OUT PARAMETER.  Format:
        ;"              TMGMSG=line count of error messages
        ;"              TMGMSG(1)=ErrMsg
        ;"              TMGMSG(2)=ErrMsg etc..
        ;"Globally-scoped vars used: TMGIEN
        ;"Result: 1 = Success  or
        ;"       -1 = error
        ;
        NEW TMGRESULT SET TMGRESULT=1 ;"default to success
        NEW DATE SET DATE=APPT\1
        LOCK +^SC(TMGIEN,"ST",DATE):10  ;"Prevent interferance from any other process.
        ELSE  DO  GOTO D1SDONE
        . SET TMGMSG=+$GET(TMGMSG)+1
        . SET TMGMSG(TMGMSG)="Unable to get lock on ^SC("_TMGIEN_",""ST"","_DATE_")."
        . SET TMGRESULT=-1
        NEW STR SET STR=$GET(^SC(TMGIEN,"ST",DATE,1))
        IF STR="" DO  GOTO D1SUL
        . SET TMGMSG=+$GET(TMGMSG)+1
        . SET TMGMSG(TMGMSG)="Can't find a PATTERN entry for "_$$EXTDAT^TMGSDAU1(DATE)_", so can't decrease slot availability."
        . SET TMGRESULT=-1
        ;
        NEW INDX SET INDX=$$SLTINDEX(TMGIEN,APPT)
        ;"G X:(I<1!'$F(S,"["))&(S'["CAN")
        ;"I SM<7 S %=$F(S,"[",SS-1) S:'%!($P(SL,"^",6)<3) %=999 I $F(S,"]",SS)'<%!(SDDIF=2&$E(S,ST+ST+1,SS-1)["[") S SM=7
        ;
        NEW CODES SET CODES="{}&%?#@!$* XXWVUTSRQPONMLKJIHGFEDCBA0123456789jklmnopqrstuvwxyz"
        FOR  QUIT:(INDX'>$LENGTH(STR))!($LENGTH(STR)'<IOM)  SET STR=STR_" "
        ;
        ;"Note: I am not sure what SDDIF or SS is here, and trials runs only went through once, will will not loop
        ;"FOR INDX=(SLOTINDX*2):SDDIF:SS-SDDIF DO  QUIT:(TMGRESULT=-1)
        DO
        . NEW TEMP,DECCHR
        . SET TEMP=$EXTRACT(STR,INDX)
        . IF TEMP="" SET TEMP=" "
        . SET DECCHR=$EXTRACT(CODES,$FIND(CODES,TEMP)-2) ;"Return char occuring just before TEMP value in STR
        . IF (STR["CAN")!((TEMP="X")&($DATA(^SC(TMGIEN,"ST",DATE,"CAN")))) DO  QUIT
        . . SET TMGMSG=+$GET(TMGMSG)+1
        . . SET TMGMSG(TMGMSG)="Can't alter slots within a CANCELLED time period.!"
        . . SET TMGRESULT=-1
        . IF DECCHR="" DO  QUIT
        . . SET TMGMSG=+$GET(TMGMSG)+1
        . . SET TMGMSG(TMGMSG)="Error in DEC1SLOT^TMGAVLG: DECCHR=''"
        . . SET TMGRESULT=-1
        . ;"IF (DECCHR'?1NL)&(SM<6) SET SM=6  ;"Look for DECCHR as number or lowercase letter
        . SET TEMP=$EXTRACT(STR,INDX+1,999)
        . IF TEMP="" SET TEMP=" "
        . SET STR=$EXTRACT(STR,1,INDX-1)_DECCHR_TEMP
        ;
        SET ^SC(TMGIEN,"ST",DATE,1)=STR ;"Store NEW pattern.
D1SUL   LOCK -^SC(TMGIEN,"ST",DATE)  ;"Release lock
D1SDONE ;
        QUIT TMGRESULT
        ;
        ;
SLTINDEX(TMGIEN,APPT,SAVARR)
        ;"Purpose: To return INDEX in "ST" PATTERN node for given appt slot time
        ;"Input: TMGIEN -- IEN in file 44
        ;"       APPT -- FMDateTime of appointment
        ;"       SAVARR -- PASS BY REFERANCE.  A save array, so that prior lookups can be reused. Format:
        ;"          SAVARR(DateTime)=Index
        ;"          SAVARR(DateTime)=Index
        ;"          SAVARR("T",STR,MilitaryTime)=Index
        ;"          SAVARR("T",STR,MilitaryTime)=Index
        ;"Globally-scoped vars used: TMGIEN
        ;"Result: Returns 0 IF problem
        ;"        Otherwise returns index value for accessing character in "ST",1) node.
        ;
        NEW TMGRESULT SET TMGRESULT=0
        IF $DATA(SAVARR(APPT)) DO  GOTO SLIDONE  ;"Use prior lookup IF possible
        . SET TMGRESULT=+$GET(SAVARR(APPT))
        NEW DATE SET DATE=APPT\1
        NEW MILTIME SET MILTIME=(APPT#1)*1000
        NEW STR SET STR=$GET(^SC(TMGIEN,"ST",DATE,1))
        IF STR="" SET TMGRESULT=0 GOTO SLIDONE
        SET STR=$PIECE(STR,"|",2,25)
        IF $DATA(SAVARR("T",STR,MILTIME)) DO  GOTO SLIDONE
        . SET TMGRESULT=+$GET(SAVARR("T",STR,MILTIME))
        ;
        NEW TMGSPH SET TMGSPH=+$P($GET(^SC(TMGIEN,"SL")),"^",6) ;"SL;6 = DISPLAY INCS PER HOUR (Slots per Hr)
        IF TMGSPH'>0 SET TMGSPH=4       ;"Default to 4 slots/hr
        NEW STARTDAY SET STARTDAY=+$P($GET(^SC(TMGIEN,"SL")),"^",3) ;"SL;3=HR CLINIC DISPLAY BEGINS
        NEW SB SET SB=(STARTDAY-1)/100  ;"Eg 8 am --> .07
        ;
        ;"Convert Hrs.Min --> fractional hours.  e.g. 1:30 --> 1.5; 3.45 --> 3.75
        NEW HROFFSET SET HROFFSET=((APPT#1)-SB)*100 ;"HROFFSET=Num of Hrs (i.e. hrs.min 1.3=1 hr, 30 min) **past** display start time (i.e. 7am)
        ;"Note: SB is usually 1 hr before true display start time.  I.e. .07 for start time of 8 am
        ;"      I think this is to allow for the header info (e.g. 'SUN 04  |')
        NEW MINOFFST SET MINOFFST=HROFFSET#1 ;"Get just minutes part, e.g. 0.3 (i.e. 30 minutes)
        SET HROFFSET=HROFFSET\1  ;"Get just hrs part
        SET MINOFFST=MINOFFST/0.6 ;"integer divide (i.e round output) by 0.6, e.g. 1.2/0.6 --> 2. Note, 0.6 here means 60 minutes
        ;"SET MINOFFST=MINOFFST*TMGSPH ;"multiply by slots/hr, e.g. 4 --> 0.3 * 4 = 1.2 (i.e. 120 minutes)
        NEW SLOTINDX SET SLOTINDX=(HROFFSET+MINOFFST)*TMGSPH ;"Add number of hrs past display start time * slots/hr --> slot index #
        ;
        SET TMGRESULT=(SLOTINDX*2)+1  ;"x2 because of spaces etc between character values, and 1st slot begins 1 character after '|'
        ;
        SET SAVARR(APPT)=TMGRESULT
        SET SAVARR("T",STR,MILTIME)=TMGRESULT
SLIDONE ;
        QUIT TMGRESULT
        ;
        ;
SPECPAT(TMGIEN,DATE,AVAILSTR)
        ;"Purpose: Return string like this: MO 05  |       [2 2 2 2|2 2 2 2]
        ;"         ... given the original pattern string ('AvailStr'), e.g. '     [2 2 2 2|2 2 2 2]'
        NEW DOW SET DOW=$$DOW^XLFDT(DATE,1) ;"DOW=Day of Week (0-6)
        NEW TMGSPH SET TMGSPH=+$P($GET(^SC(TMGIEN,"SL")),"^",6) ;"SL;6 = DISPLAY INCS PER HOUR (Slots per Hr)
        IF TMGSPH'>0 SET TMGSPH=4       ;"Default to 4 slots/hr
        NEW SI SET SI=+TMGSPH
        IF (SI=0)!(SI=1)!(SI=2) SET SI=4
        NEW SM
        SET SM=$P("SU^MO^TU^WE^TH^FR^SA",U,DOW+1)_" "
        SET SM=SM_$EXTRACT(DATE,6,7)_$J("",SI+SI-6)
        SET SM=SM_AVAILSTR_$J("",64-$LENGTH(AVAILSTR))
        QUIT SM
        ;
        ;
NONAPPT(TMGIEN,APPT)
        ;"Purpose: To see IF appointment is inactivated (i.e. a Non-Appt)
        ;"Input: TMGIEN -- IEN in file 44
        ;"       APPT -- FMDateTime of appointment
        ;"Result: 0 IF appt is active, 1 IF cancelled etc.
        NEW TMGRESULT SET TMGRESULT=1  ;"Default to cancelled.
        NEW DFN SET DFN=+$PIECE($GET(^SC(TMGIEN,"S",APPT,1,1,0)),"^",1) ;"Patient IEN
        IF DFN'>0 SET TMGRESULT=-1 GOTO NADONE
        NEW STATUS SET STATUS=$PIECE($GET(^DPT(DFN,"S",APPT,0)),"^",2)  ;"Status field
        IF STATUS="" SET TMGRESULT=0
NADONE  QUIT TMGRESULT
        ;
        ;