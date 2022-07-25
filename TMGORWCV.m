TMGORWCV ;TMG/kst-Encounter-related code ; 5/26/22
   ;;1.0;TMG-LIB;**1**;5/26/22
  ;
  ;"Code related to TIU components
  ;
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;"Copyright (c) 5/26/2022  Kevin S. Toppenberg MD
  ;"
  ;"This file is part of the TMG LIBRARY, and may only be used in accordence
  ;" to license terms outlined in separate file TMGLICNS.m, which should 
  ;" always be distributed with this file.
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;
  ;"=======================================================================
  ;" RPC -- Public Functions. 
  ;"=======================================================================
  ;"TMGXTRA(TIUDA,TIUL,PREFIX) --Give information about child docs
  ;"FIREINH1(TIUDA) --FIRE INHERITED POST-SIGNATURE EVENT HANDLERS
  ;"FIREINHE(TIUDA,FLD)  --FIRE EVENTS INHERITED FROM TIU DOC-TYPE HEIRARCHY
;"=======================================================================
  ;"PRIVATE API FUNCTIONS
  ;"=======================================================================
  ;
  ;"=======================================================================
  ;"DEPENDENCIES: 
  ;"=======================================================================
  ;
VST(ORVISIT,TMGDFN,BEG,END,ERR,ERRMSG)	;" return visits for patient
    NEW VISITIEN SET VISITIEN=0
    NEW IDX SET IDX=0
    NEW TEMPARRAY
    FOR  SET VISITIEN=$O(^AUPNVSIT("C",TMGDFN,VISITIEN)) QUIT:VISITIEN'>0  DO
    . ;"SET ORVISIT(0)="V;3211029.1;6^3211029.1^Laughlin_Office^NON-COUNT"
    . ;"SET ORVISIT(1)="V;3211029.111027;6^3211029.111027^Laughlin_Office^NON-COUNT"
    . NEW VISITDT SET VISITDT=$P($G(^AUPNVSIT(VISITIEN,0)),"^",1)
    . NEW LOCIEN SET LOCIEN=6
    . NEW LOCNAME SET LOCNAME="Laughlin_Office"
    . IF (VISITDT>BEG)&(VISITDT<END) DO
    . . NEW TIUNOTE SET TIUNOTE=+$O(^TIU(8925,"V",VISITIEN,0))
    . . IF TIUNOTE>0 DO
    . . . SET TIUNOTE=+$P($G(^TIU(8925,TIUNOTE,0)),"^",1)
    . . . IF TIUNOTE>0 DO
    . . . . SET TIUNOTE=$P($G(^TIU(8925.1,TIUNOTE,0)),"^",1)
    . . . ELSE  DO
    . . . . SET TIUNOTE="Zxisting Visit"
    . . ELSE  DO
    . . . SET TIUNOTE="Zxisting Visit"
    . . SET TEMPARRAY($P(VISITDT,".",1),TIUNOTE,VISITDT,VISITIEN)="V;"_VISITDT_";"_LOCIEN_"^"_VISITDT_"^"_LOCNAME_"^"_TIUNOTE
    ;"
    NEW OUTIDX SET OUTIDX=0
    NEW VISITDAY SET VISITDAY=0
    FOR  SET VISITDAY=$O(TEMPARRAY(VISITDAY)) QUIT:VISITDAY=""  DO
    . NEW NOTETITLE SET NOTETITLE=""
    . NEW GOTONE SET GOTONE=0
    . FOR  SET NOTETITLE=$O(TEMPARRAY(VISITDAY,NOTETITLE)) QUIT:NOTETITLE=""  DO
    . . IF (NOTETITLE="Zxisting Visit")&(GOTONE=1) QUIT
    . . NEW VISITDT SET VISITDT=0
    . . FOR  SET VISITDT=$O(TEMPARRAY(VISITDAY,NOTETITLE,VISITDT)) QUIT:VISITDT'>0  DO
    . . . IF VISITDT'["." QUIT  ;"Visit must have time for encounter
    . . . NEW VISITIEN SET VISITIEN=0
    . . . FOR  SET VISITIEN=$O(TEMPARRAY(VISITDAY,NOTETITLE,VISITDT,VISITIEN)) QUIT:VISITIEN'>0  DO
    . . . . NEW DATA SET DATA=$G(TEMPARRAY(VISITDAY,NOTETITLE,VISITDT,VISITIEN))
    . . . . IF DATA["Zxisting Visit" DO
    . . . . . SET DATA=$TR(DATA,"Zx","Ex")
    . . . . ELSE  DO
    . . . . . SET GOTONE=1
    . . . . SET ORVISIT(OUTIDX)=DATA,OUTIDX=OUTIDX+1
    QUIT
	;"
APPTS(ORVISIT,TMGDFN,BEG,END,ERR,ERRMSG)  ;"RETURN SEQUELMED APPOINTMENTS FOR PATIENT
    ;"SET ORVISIT(0)="V;3211029.1;6^3211029.1^Laughlin_Office^NON-COUNT"
    ;"SET ORVISIT(1)="V;3211029.111027;6^3211029.111027^Laughlin_Office^NON-COUNT"
    NEW APPTIEN SET APPTIEN=0
    NEW OUTIDX SET OUTIDX=0
    FOR  SET APPTIEN=$O(^TMG(22723,TMGDFN,1,APPTIEN)) QUIT:APPTIEN'>0  DO
    . NEW APPTDT,ZN,STATUS,REASON
    . SET ZN=$G(^TMG(22723,TMGDFN,1,APPTIEN,0))
    . SET APPTDT=$P(ZN,"^",1)
    . SET STATUS=$P(ZN,"^",7)
    . IF STATUS="C" QUIT
    . NEW LOCIEN,LOCNAME
    . SET LOCIEN=6,LOCNAME="Laughlin_Office"
    . IF (APPTDT>BEG)&(APPTDT<END) DO
    . . SET REASON=$P(ZN,"^",4)
    . . SET ORVISIT(OUTIDX)="V;"_APPTDT_";"_LOCIEN_"^"_APPTDT_"^"_LOCNAME_"^"_REASON,OUTIDX=OUTIDX+1
    QUIT
    ;"
FIRSTTAB(TMGOUT,TMGDFN,TMGDUZ)  ;"Determine which tab should be shown first for the Encounter Form
    ;"The current logic is: 
    ;"  if an appointment today start with Appointment (tab 4)
    ;"  else if a previous visit exists for today (tab 3)
    ;"  else start with New Visit (tab 2)
    SET TMGOUT=2
    NEW ARRAY,BEG,END
    SET BEG=$$TODAY^TMGDATE+0.000001
    SET END=$$TODAY^TMGDATE+0.999999
    DO APPTS(.ARRAY,TMGDFN,BEG,END)
    IF $D(ARRAY) SET TMGOUT=4 GOTO FTDN
    KILL ARRAY
    DO VST(.ARRAY,TMGDFN,BEG,END)
    IF $D(ARRAY) SET TMGOUT=3
FTDN    
    QUIT
    ;"