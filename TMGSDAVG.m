TMGSDAVG  ;TMG/kst/Get Schedule Availability Getting API ;12/08/08, 2/2/14
         ;;1.0;TMG-LIB;**1**;12/08/08
 ;
 ;"TMG SCHEDULING AVAILIBILITY GETTING
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
 ;"Called into from TMGRPC5
 ;"
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"GETAVAIL(TMGIEN,TMGSTRTDT,TMGENDDT,TMGAVAIL,TMGMSG) -- Return an array with appt slot information: time, availibility
 ;
 ;"=======================================================================
 ;" Private Functions.
 ;"=======================================================================
 ;"GETINFO(TMGIEN,TMG1DATE,TMGAVAIL,SAVARRAY) -- extract information from ST node, and return info into TMGAVAIL array
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"=======================================================================
 ;
GETAVAIL(TMGIEN,TMGSTRTDT,TMGENDDT,TMGAVAIL,TMGMSG)
        ;"Purpose: Return an array with appt slot information: time, availibility
        ;"Input: TMGIEN -- the IEN in file 44 (HOSPITAL LOCATION) to check.
        ;"       TMGSTRTDT -- The beginning of the date range requested
        ;"       TMGENDDT -- The end of the date range requested
        ;"       TMGAVAIL -- PASS BY REFERENCE, an OUT PARAMETER.  Format below. Prior values KILLED
        ;"       TMGMSG -- PASS BY REFERENCE, an OUT PARAMETER.  Format:
        ;"              TMGMSG=line count of error messages
        ;"              TMGMSG(1)=ErrMsg
        ;"              TMGMSG(2)=ErrMsg etc..
        ;"Output: TMGAVAIL filled as follows:
        ;"          TMGAVAIL("INFO","APPTLEN")=LengthOfApptSlot
        ;"          TMGAVAIL(Date,SlotStartTime)=NumOpenings^NumTotalSlotsAtTime
        ;"          TMGAVAIL(Date,SlotStartTime)=NumOpenings^NumTotalSlotsAtTime
        ;"Note: If a clinic is not SET up for a given day in date range, no results will be
        ;"      returned for that invalid day.
        ;"Result: 1 = Success  or
        ;"        0 = Intermediate success
        ;"       -1 = error
        ;
        NEW TMGRESULT SET TMGRESULT=1
        ;"---Validate input values ---
        SET TMGIEN=+$GET(TMGIEN)
        IF (TMGIEN'>0)!($DATA(^SC(TMGIEN))=0) DO  GOTO GAVDONE
        . SET TMGMSG=+$GET(TMGMSG)+1
        . SET TMGMSG(TMGMSG)="Location value of '"_TMGIEN_"' does not appear to refer to actual location."
        . SET TMGRESULT=-1
        SET TMGSTRTDT=$GET(TMGSTRTDT)\1
        IF (TMGSTRTDT'?7N) DO  GOTO GAVDONE
        . SET TMGMSG=+$GET(TMGMSG)+1
        . SET TMGMSG(TMGMSG)="Date: "_TMGSTRTDT_" Doesn't seem to be proper start date, in Fileman format."
        . SET TMGRESULT=-1
        SET TMGENDDT=$GET(TMGENDDT)\1
        IF (TMGENDDT'?7N) DO  GOTO GAVDONE
        . SET TMGMSG=+$GET(TMGMSG)+1
        . SET TMGMSG(TMGMSG)="Date: "_TMGENDDT_" Doesn't seem to be proper start date, in Fileman format."
        . SET TMGRESULT=-1
        ;
        KILL TMGAVAIL
        NEW TMGSAV ;"Scatch temp save array to speed processing.
        NEW TMG1DATE SET TMG1DATE=TMGSTRTDT
        FOR  DO  SET TMG1DATE=$$ADD2DATE^TMGSDAU1(TMG1DATE,1) QUIT:(TMG1DATE>TMGENDDT)!(TMGRESULT=-1)
        . SET TMGRESULT=$$GETINFO(TMGIEN,TMG1DATE,.TMGAVAIL,.TMGSAV,.TMGMSG)
        . IF TMGRESULT=1 QUIT  ;"Skip error checking
        . IF $GET(TMGMSG(1))["NO TEMPLATE" DO  QUIT
        . . KILL TMGMSG
        . . SET TMGRESULT=1  ;"Ignore errors when checking days not defined in clinic.
        ;
GAVDONE ;
        QUIT TMGRESULT
        ;
        ;
GETINFO(TMGIEN,TMG1DATE,TMGAVAIL,TMGSAV,TMGMSG)
        ;"Purpose: To extract information from ST node, and return info into TMGAVAIL array
        ;"Input: TMGIEN -- IEN in file 44
        ;"       TMG1DATE -- The date to get info for
        ;"       TMGAVAIL -- PASS BY REFERENCE, an OUT PARAMETER.  Format:
        ;"          TMGAVAIL("INFO","APPTLEN")=LengthOfApptSlot
        ;"          TMGAVAIL(Date,SlotStartTime)=NumOpenings^NumScheduled^NumTotalSlotsAtTime
        ;"          TMGAVAIL(Date,SlotStartTime)=NumOpenings^NumScheduled^NumTotalSlotsAtTime
        ;"              (Date is in FMFormat)
        ;"          NOTE: IF no availibility then the following is returned:
        ;"          TMGAVAIL(Date,"0000")=0^0^0
        ;"       TMGSAV -- PASS BY REFERENCE.  This is just a speed enhancing
        ;"              array, where prior effort at prior lookups is stored for future reference
        ;"       TMGMSG -- PASS BY REFERENCE, an OUT PARAMETER.  Format:
        ;"              TMGMSG=line count of error messages
        ;"              TMGMSG(1)=ErrMsg
        ;"              TMGMSG(2)=ErrMsg etc..
        ;"Result: 1 = Success  or
        ;"        0 = Intermediate success
        ;"       -1 = error
        NEW TMGRESULT
        SET TMGRESULT=$$ENSUR1ST^TMGSDAU(TMGIEN,TMG1DATE,.TMGMSG) ;"Ensure ST node is SET up
        IF TMGRESULT=-1 GOTO GIDONE
        ;
        NEW STR SET STR=$GET(^SC(TMGIEN,"ST",TMG1DATE\1,1))
        IF STR="" SET TMGRESULT=0 GOTO GIDONE
        ;
        SET TMGAVAIL("INFO","APPTLEN")=$PIECE($GET(^SC(TMGIEN,"SL")),"^",1)
        ;
        ;"--- Find applicable T node, which holds slot information
        ;"Search backwards from specified date, looking for matching day of week.
        NEW SLOTREF SET SLOTREF=""
        NEW DOW SET DOW=$$DOW^XLFDT(TMG1DATE,1) ;"DOW=Day of Week (0-6)
        NEW DATE SET DATE=TMG1DATE
        FOR  DO  SET DATE=$ORDER(^SC(TMGIEN,"T",DATE),-1) QUIT:(DATE'>0)!(SLOTREF'="")
        . IF $DATA(^SC(TMGIEN,"T",DATE))=0 QUIT  ;"Needed for first cycle
        . IF $$DOW^XLFDT(DATE,1)'=DOW QUIT  ;"Only consider entries on same day of week
        . SET SLOTREF=$NAME(^SC(TMGIEN,"T",DATE,2))
        IF SLOTREF="" DO  GOTO GIDONE
        . SET TMGRESULT=0
        . SET TMGAVAIL(TMG1DATE\1,"0000")="0^0^0"  ;"Store arbitrary zero slot at 0000 to show we looked.
        ;
        SET DATE=TMG1DATE\1
        ;"Cycle through slots, and get openings.
        ;"Note: Another approach would be look at appts themselves in the "S" nodes instead of using
        ;"      compiled "ST" node, originating from appts.  This assumes that VistA code will keep
        ;"      "ST" nodes refreshed as NEW appts are added.
        NEW CODES SET CODES="{}&%?#@!$* XXWVUTSRQPONMLKJIHGFEDCBA0123456789jklmnopqrstuvwxyz" ;"Note 0 is 37th character
        NEW COUNT SET COUNT=0
        FOR  SET COUNT=$ORDER(@SLOTREF@(COUNT)) QUIT:(COUNT'>0)  DO
        . NEW SLENTRY,SLTIME
        . SET SLENTRY=$GET(@SLOTREF@(COUNT,0)) QUIT:SLENTRY=""
        . SET SLTIME=$PIECE(SLENTRY,"^",1) QUIT:SLTIME=""
        . NEW TOTALSL SET TOTALSL=+$PIECE(SLENTRY,"^",2)
        . NEW APPT SET APPT=DATE_"."_SLTIME
        . NEW INDEX SET INDEX=$$SLTINDEX^TMGSDAU(TMGIEN,APPT,.TMGSAV)
        . NEW CHAR SET CHAR=$EXTRACT(STR,INDEX)
        . IF CHAR="" SET CHAR=" "
        . NEW OPENSL SET OPENSL=$FIND(CODES,CHAR)-$FIND(CODES,"0")
        . NEW NUMSCH SET NUMSCH=TOTALSL-OPENSL
        . SET TMGAVAIL(DATE,SLTIME)=OPENSL_"^"_NUMSCH_"^"_TOTALSL
GIDONE  ;
        QUIT TMGRESULT
        ;
        ;