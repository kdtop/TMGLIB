TMGAPPT1 ;TMG/kst-Appointment Related Fns;11/08/08, 2/2/14
         ;;1.0;TMG-LIB;**1,17**;11/08/08
 ;
 ;"Kevin Toppenberg MD
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 05/22/2017  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"---------------------------------------------------------------------------
 ;"PUBLIC FUNCTIONS
 ;"---------------------------------------------------------------------------
 ;
 ;"---------------------------------------------------------------------------
 ;"PRIVATE FUNCTIONS
 ;"---------------------------------------------------------------------------
 ;
 ;"---------------------------------------------------------------------------
 ;
PROAPPT(Y,PROVIDER,ORBDATE,OREDATE)     ; RETURN LIST OF APPOINTMENTS FOR A PROVIDER
        I +$G(PROVIDER)<1 S Y(1)="^No provider identified" Q
        SET Y(1)="^NO APPOINTMENTS FOUND"
        N ORI,APPTIEN,APPTDATE
        I ORBDATE="" S Y(1)="^No beginning date sent" Q
        I OREDATE="" S Y(1)="^No ending date sent" Q
        ;
        ; Convert ORBDATE, OREDATE to FM Date/Time:
        D DT^DILF("T",ORBDATE,.ORBDATE,"","")
        D DT^DILF("T",OREDATE,.OREDATE,"","")
        I (ORBDATE=-1)!(OREDATE=-1) S Y(1)="^Error in date range." Q
        S OREDATE=$P(OREDATE,".")_.9999 ;
        SET ORI=0,APPTDATE=ORBDATE
        FOR  SET APPTDATE=$ORDER(^TMG(22723,"DT",APPTDATE)) QUIT:(APPTDATE>OREDATE)!(APPTDATE'>0)  DO
        . SET DFN=0
        . FOR  SET DFN=$ORDER(^TMG(22723,"DT",APPTDATE,DFN)) QUIT:DFN'>0  DO
        . . NEW APPTIEN SET APPTIEN=$ORDER(^TMG(22723,"DT",APPTDATE,DFN,0))
        . . NEW PATNAME,ZN,DOCTOR,STATUS,APPTTYPE
        . . SET STATUS=$GET(^TMG(22723,"DT",APPTDATE,DFN,APPTIEN))
        . . IF STATUS="C" QUIT
        . . ;"10/2/20 Per Dr. Dee, start including old appts now IF STATUS="O" QUIT  ;"9/15/20  don't include Old appts for now
        . . SET ZN=$G(^TMG(22723,DFN,1,APPTIEN,0))
        . . SET DOCTOR=$P(ZN,"^",3)
        . . IF DOCTOR'=PROVIDER QUIT
        . . SET APPTTYPE=$P(ZN,"^",4)
        . . IF $$UP^XLFSTR(APPTTYPE)["INJ ONLY" QUIT
        . . SET PATNAME=$PIECE($GET(^DPT(DFN,0)),"^",1)
        . . SET ORI=ORI+1
        . . NEW SEEN SET SEEN=$$PTSEEN(DFN,APPTDATE)
        . . SET Y(ORI)=DFN_"^"_PATNAME_SEEN_"^"_PROVIDER_"^"_APPTDATE_"^ ("_APPTTYPE_")"
        IF PROVIDER=168 DO   ;"THIS CAN EVENTUALLY BE MADE A PARAMETER
        . NEW TOPITEMS,BOTITEMS
        . NEW TOPIDX,BOTIDX
        . SET TOPIDX=0,BOTIDX=0
        . NEW IDX SET IDX=0
        . FOR  SET IDX=$O(Y(IDX)) QUIT:IDX'>0  DO
        . . IF $P($G(Y(IDX)),"^",2)["*" DO
        . . . SET BOTIDX=BOTIDX+1
        . . . SET BOTITEMS(BOTIDX)=$G(Y(IDX))
        . . ELSE  DO
        . . . SET TOPIDX=TOPIDX+1
        . . . SET TOPITEMS(TOPIDX)=$G(Y(IDX))
        . KILL Y
        . SET IDX=0,TOPIDX=0,BOTIDX=0
        . FOR  SET TOPIDX=$O(TOPITEMS(TOPIDX)) QUIT:TOPIDX'>0  DO
        . . SET IDX=IDX+1
        . . SET Y(IDX)=$G(TOPITEMS(TOPIDX))
        . FOR  SET BOTIDX=$O(BOTITEMS(BOTIDX)) QUIT:BOTIDX'>0  DO
        . . SET IDX=IDX+1
        . . SET Y(IDX)=$G(BOTITEMS(BOTIDX))
        QUIT
        ;"
PROVIDER(RESULT)  ;"RETURN ALL PROVIDERS
        SET RESULT(1)="168^Toppenberg,Kevin S"
        SET RESULT(2)="83^Toppenberg,Marcia Dee"
        QUIT
        ;
PTSEEN(DFN,APPTDATE)   ;"
        ;"Purpose: to determine if patient has been seen today,
        ;"         if so return notation, if not return nothing
        NEW TMGRESULT
        SET TMGRESULT=""
        SET DFN=+$GET(DFN)
        IF DFN'>0 GOTO PTDN
        SET APPTDATE=$PIECE(APPTDATE,".",1)
        ;"NEW PTDATA SET PTDATA=$$GETPDATA^TMGORQPT(DFN,"NOT NEEDED",APPTDATE)
        ;"IF ($P(PTDATA,",",7)'="")&($P(PTDATA,",",8)'="") SET TMGRESULT=" *"
        ;"
        NEW NOTATION SET NOTATION=$$CHKNOTES(DFN)
        IF NOTATION'="" SET TMGRESULT=NOTATION
PTDN    QUIT TMGRESULT
        ;"
CHKNOTES(DFN)  ;"
        NEW TMGRESULT SET TMGRESULT=""
        NEW NOTATION SET NOTATION=""
        NEW TIUIEN SET TIUIEN=0
        FOR  SET TIUIEN=$O(^TIU(8925,"C",DFN,TIUIEN)) QUIT:TIUIEN'>0  DO
        . NEW ZN SET ZN=$G(^TIU(8925,TIUIEN,0))
        . NEW STATUS,DATE
        . SET STATUS=$P(ZN,"^",5)
        . SET DATE=$P($P($G(^TIU(8925,TIUIEN,12)),"^",1),".",1)
        . NEW AUTHOR SET AUTHOR=$P($G(^TIU(8925,TIUIEN,12)),"^",2)
        . IF (DATE=$$TODAY^TMGDATE)&(STATUS=5)&(AUTHOR=DUZ) DO
        . . SET TMGRESULT=" +"
        . . NEW LINE SET LINE=0
        . . FOR  SET LINE=$O(^TIU(8925,TIUIEN,"TEXT",LINE)) QUIT:LINE'>0  DO
        . . . NEW TEXT SET TEXT=$G(^TIU(8925,TIUIEN,"TEXT",LINE,0))
        . . . IF TEXT["=%=" SET NOTATION=" &"
        . . . IF TEXT["=&amp;=" SET NOTATION=" &"
        . ELSE  IF (DATE=$$TODAY^TMGDATE)&(STATUS=7) DO
        . . SET TMGRESULT=" *"
        IF NOTATION'="" SET TMGRESULT=NOTATION
        QUIT TMGRESULT
        ;"
