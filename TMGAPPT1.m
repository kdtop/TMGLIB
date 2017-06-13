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
        . . SET ZN=$G(^TMG(22723,DFN,1,APPTIEN,0))
        . . SET DOCTOR=$P(ZN,"^",3)
        . . IF DOCTOR'=PROVIDER QUIT
        . . SET APPTTYPE=$P(ZN,"^",4)
        . . SET PATNAME=$PIECE($GET(^DPT(DFN,0)),"^",1)
        . . SET ORI=ORI+1
        . . SET Y(ORI)=DFN_"^"_PATNAME_"^"_PROVIDER_"^"_APPTDATE_"^ ("_APPTTYPE_")"
        QUIT
        ;"
PROVIDER(RESULT)  ;"RETURN ALL PROVIDERS
        SET RESULT(1)="168^Toppenberg,Kevin S"
        SET RESULT(2)="83^Toppenberg,Marcia Dee"
        QUIT
        ;
