TMGSDAC ;TMG/kst/API FOR CANCELLING APPTS;2/23/09, 2/2/14
         ;;1.0;TMG-LIB;**1**;2/23/09
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
 ;"CANCAPPT(DFN,APPTDATE,CLINIC,INFO) -- CANCEL AN APPOINTMENT
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"=======================================================================
 ;
CANCAPPT(DFN,APPTDATE,CLINIC,INFO)     ;
        ;"Purpose: CANCEL AN APPOINTMENT
        ;"INPUT: DFN -- The IEN of the patient for appt to cancel
        ;"       APPTDATE -- Appointment Date & Time to be cancelled -- FM format
        ;"       CLINIC -- IEN of Clinic for appt (file 44)
        ;"       INFO -- PASS BY REFERENCE.
        ;"         INFO("REASON IEN")= IEN of cancellation reason, from 409.2
        ;"         INFO("COMMENT") = Comment (3-160 chars length)     (OPTIONAL)
        ;"Result: 1 = OK,APPOINTMENT SUCCESSFULLY CANCELLED
        ;"        NEG NUMBER= ErrorNum^ErrorMessage.
        ;
        NEW RESULT SET RESULT="-1^NO MATCHING APPT FOUND TO DELETE" ;"Default to FAILURE
        NEW DATEMADE SET DATEMADE=0
        ;
        IF $DATA(^SC(CLINIC))=0 DO  GOTO CANCDONE
        . SET RESULT="-1^INVALID CLINIC IEN: "_CLINIC
        IF $DATA(^SC(CLINIC,"S",APPTDATE))=0 DO  GOTO CANCDONE
        . SET RESULT="-1^NO APPT FOUND AT SPECIFIED TIME"
        NEW REASNIEN SET REASNIEN=+$GET(INFO("REASON IEN"))
        IF (REASNIEN=0)!($DATA(^SD(409.2,REASNIEN))=0) DO  GOTO CANCDONE
        . SET RESULT="-1^INVALID CANCELLATION IEN: "_REASNIEN
        NEW IEN SET IEN=0
        FOR  SET IEN=$ORDER(^SC(CLINIC,"S",APPTDATE,1,IEN)) QUIT:(+IEN'>0)  DO
        . NEW S SET S=$GET(^SC(CLINIC,"S",APPTDATE,1,IEN,0))
        . IF +S'=DFN QUIT  ;"Ignore appt IF patient doesn't match.
        . SET DATEMADE=+$PIECE(S,"^",7)  ;"Store for later
        . KILL ^SC(CLINIC,"S",APPTDATE,1,IEN) ;"<-- DELETION OF APPT (Part 1)
        ;
        IF DATEMADE=0 GOTO CANCDONE  ;"Apparently no matching appt found above
        IF $DATA(^DPT(DFN,"S",APPTDATE))=0 GOTO CANCDONE
        ;
        NEW TMGFDA,IENS,%,REASNSTR
        SET REASNSTR=$EXTRACT($GET(INFO("REASON IEN")),1,160)
        DO NOW^%DTC ;"Returns NOW in %
        SET IENS=APPTDATE_","_DFN_","
        SET TMGFDA(2.98,IENS,3)="C"         ;"STATUS
        SET TMGFDA(2.98,IENS,14)=DUZ        ;"NO-SHOW/CANCELLED BY
        SET TMGFDA(2.98,IENS,15)=%          ;"NO-SHOW/CANCEL DATE/TIME
        SET TMGFDA(2.98,IENS,16)=REASNIEN   ;"CANCELLATION REASON
        SET TMGFDA(2.98,IENS,19)=DUZ        ;"DATA ENTRY CLERK
        SET TMGFDA(2.98,IENS,20)=DATEMADE   ;"DATE APPT. MADE
        SET TMGFDA(2.98,IENS,17)=REASNSTR   ;"CANCELLATION REMARKS
        ;
        DO FILE^DIE("","TMGFDA","TMGMSG")  ;"File in INTERNAL format
        IF $DATA(TMGMSG) DO  GOTO CANCDONE
        . SET RESULT="-1^"_$$GETERSTR(.TMGMSG)
        ;
        ;"send event to rest of system!!!
        ;
        SET RESULT=1 ;"Change to success IF no problems at the end of the process.
        ;
CANCDONE ;
        QUIT RESULT
        ;
        ;
GETERSTR(TMGEARRAY) ;
        ;"Purpose: convert a standard DIERR array into a string for output
        ;"Input: TMGEARRAY -- PASS BY REFERENCE.  example:
        ;"      array("DIERR")="1^1"
        ;"      array("DIERR",1)=311
        ;"      array("DIERR",1,"PARAM",0)=3
        ;"      array("DIERR",1,"PARAM","FIELD")=.02
        ;"      array("DIERR",1,"PARAM","FILE")=2
        ;"      array("DIERR",1,"PARAM","IENS")="+1,"
        ;"      array("DIERR",1,"TEXT",1)="The NEW record '+1,' lacks some required identifiers."
        ;"      array("DIERR","E",311,1)=""
        ;"Results: returns one long equivalent string from above array.
        ;"Note: This is a copy of the function GETERRST^TMGDEBU2
        ;"      I copied it here so that this file has no TMG* dependencies.
        ;
        NEW TMGESTR,TMGIDX,TMGENUM
        SET TMGESTR=""
        FOR TMGENUM=1:1:+$GET(TMGEARRAY("DIERR")) DO
        . SET TMGESTR=TMGESTR_"Fileman says: '"
        . IF TMGENUM'=1 SET TMGESTR=TMGESTR_"(Error# "_TMGENUM_") "
        . SET TMGIDX=$ORDER(TMGEARRAY("DIERR",TMGENUM,"TEXT",""))
        . IF TMGIDX'="" FOR  DO  QUIT:(TMGIDX="")
        . . SET TMGESTR=TMGESTR_$GET(TMGEARRAY("DIERR",TMGENUM,"TEXT",TMGIDX))_" "
        . . SET TMGIDX=$ORDER(TMGEARRAY("DIERR",TMGENUM,"TEXT",TMGIDX))
        . IF $GET(TMGEARRAY("DIERR",TMGENUM,"PARAM",0))>0 DO
        . . SET TMGIDX=$ORDER(TMGEARRAY("DIERR",TMGENUM,"PARAM",0))
        . . SET TMGESTR=TMGESTR_"Details: "
        . . FOR  DO  QUIT:(TMGIDX="")
        . . . IF TMGIDX="" QUIT
        . . . SET TMGESTR=TMGESTR_"["_TMGIDX_"]="_$GET(TMGEARRAY("DIERR",1,"PARAM",TMGIDX))_"  "
        . . . SET TMGIDX=$ORDER(TMGEARRAY("DIERR",TMGENUM,"PARAM",TMGIDX))
        ;
        QUIT TMGESTR
        ;
 ;"------------------------------------------
 ;"I think the code below can be delete later.  It is done automatically by
 ;"  the XREF
 ;"
 ;
 ;"Input: DA(1) = DFN -- patient IEN
 ;"       DA -- appt time
 ;
 ;"K Q8 F Q7=0:0 S Q7=$N(^SC($P(^DPT(DA(1),"S",DA,0),"^"),"S",DA,1,Q7)) Q:Q7'>0  I $P(^(Q7,0),"^")=DA(1),$P(^(0),"^",9)="C" S Q8="" Q
 ;"I '$D(Q8) S ^DPT("ASDCN",$P(^DPT(DA(1),"S",DA,0),"^"),DA,DA(1))=$S($P(^DPT(DA(1),"S",DA,0),"^",2)["P":1,1:"")
 ;"K Q7,Q8 Q
 ; 
 ; 
 ;"This is called when setting STATUS field (#3, 0;2) in PATIENT file
 ;
 K Q8
 SET Q7=0
 SET SC=$P(^DPT(DFN,"S",APTTIME,0),"^",1)
 F  S Q7=$ORDER(^SC(SC,"S",APTTIME,1,Q7)) Q:Q7'>0  DO  QUIT:$DATA(Q8)
 . IF $P(^SC(SC,"S",APTTIME,1,Q7,0),"^",1)'=DFN QUIT  ;"0;1 = Patient IEN
 . IF $P(^SC(SC,"S",APTTIME,1,Q7,0),"^",9)="C" S Q8=""  ;"0;9 = APPOINTMENT CANCELLED?
 I '$D(Q8) DO  ;"i.e. DO this IF APPOINTMENT CANCELLED <> 'C'
 . NEW status SET status=$P(^DPT(DFN,"S",APTTIME,0),"^",2)  ;"0;2 = STATUS
 . SET ^DPT("ASDCN",SC,APTTIME,DFN)=$S(status["P":1,1:"")   ;"P --> cancelled by patient
 K Q7,Q8
 Q
