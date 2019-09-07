TMGTIU01 ;TMG/kst-Misc TIU Related Fns;11/08/08, 2/2/14
         ;;1.0;TMG-LIB;**1,17**;11/08/08
 ;
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
 ;"---------------------------------------------------------------------------
 ;"PUBLIC FUNCTIONS
 ;"---------------------------------------------------------------------------
 ;"ENSURVST(PATIENT,VISIT,CLINIC,DOCTOR) -- Ensure that patient has an entry in the VISIT file.
 ;"GETPCEI(.ARRAY,DFN,DT,LocIEN) -- Get Patient Care Event info for patient on given date.
 ;
 ;"---------------------------------------------------------------------------
 ;"PRIVATE FUNCTIONS
 ;"---------------------------------------------------------------------------
 ;
 ;"---------------------------------------------------------------------------
 ;
T1  ;
        NEW DFN,IEN,Y,X,DIC
        SET DIC=2,DIC(0)="MAEQ"
        DO ^DIC WRITE !
        IF +Y'>0 GOTO TDONE
        SET DFN=+Y
        ;
        NEW LOCIEN
        SET DIC=44 ;"hospital location
        DO ^DIC WRITE !
        IF +Y'>0 GOTO TDONE
        SET LOCIEN=+Y
        ;
        NEW %DT,selDT
        SET %DT="AE"
        DO ^%DT WRITE !
        IF Y'>0 GOTO TDONE
        SET SETDT=Y
        ;
        NEW ARRAY
        DO GETPCEI(.ARRAY,DFN,SETDT,LOCIEN)
        IF $DATA(ARRAY) DO ZWRITE^TMGZWR("ARRAY")
        ;
TDONE   QUIT
        ;
        ;
ENSURVST(PATIENT,VISIT,CLINIC,DOCTOR) ;
        ;"NOTE: FYI, there is a function ENSURVST^TMGRPC4B that was developed separately
        ;"Purpose: To ensure that patient has an entry in the VISIT file.
        ;"Input: PATIENT: the name and DOB of the patient.  format:
        ;"              'LName,FNAME I^12/12/1912'
        ;"       VISIT: Date and time of visit.  Format: 12/12/1912@23:15 (military time)
        ;"       CLINIC: the name of the clinic, to match entry in HOSPITAL LOCATION file
        ;"       DOCTOR: the name of the provider for the appt, to match entry in NEW PERSON file
        ;"Results: 1=OK, or 0^Error Message
        ;
        NEW RESULT SET RESULT=1
        NEW IEN2,ENTRY
        SET ENTRY(.01)=$PIECE(PATIENT,"^",1)  ;"Name
        SET ENTRY(.03)=$PIECE(PATIENT,"^",2)  ;"DOB
        ;
        SET IEN2=$$GETDFN2^TMGGDFN(ENTRY,AutoRegister)
        IF IEN2'>0 DO  GOTO EVDONE
        . SET RESULT="0^Patient Name/DOB not registered"
        ;
        NEW HLIEN,DIC,X,Y
        SET DIC=44  ;"HOSPITAL LOCATION file
        SET DIC(0)="M",X=CLINIC
        DO ^DIC
        IF Y'>0 DO  GOTO EVDONE
        . SET RESULT="0^Can't find clinic '"_CLINIC_"' in HOSPITAL LOCATION file"
        ;
        SET DIC=200,X=DOCTOR
        DO ^DIC
        IF Y'>0 DO  GOTO EVDONE
        . SET RESULT="0^Can't find doctor '"_DOCTOR+"' in NEW PERSON file"
        ;
        NEW VDT,%DT KILL Y
        SET %DT="T",X=VISIT
        DO ^%DT
        IF Y'>0 DO  GOTO EVDONE
        . SET RESULT="0^Invalid visit date: '"_VISIT_"'"
        ;
EVDONE  QUIT RESULT
        ;
GETPCEI(ARRAY,DFN,DT,LocIEN)  ;"GET PCE INFO
        ;"Purpose: to get Patient Care Event info for patient on given date.
        ;"Input: ARRAY -- PASS BY REFERENCE, an OUT PARAMETER.  Prior data killed.
        ;"       DFN -- the patient IEN
        ;"       DT -- the Date of the visit in FM format.
        ;"       LocIEN -- the IEN in the HOSPITAL LOCATION file for the clinic.
        ;"Output: ARRAY is filled, format:
        ;"              ARRAY(VISITDT,#)=InfoLine
        ;"              ARRAY(VISITDT,#)=InfoLine
        ;"                        ...,1)=HDR^AllowEdit^CPTRequired^VStr^Author^hasCPT
        ;"                        ...,n)=TYP+^CODE^CAT^NARR^QUAL1^QUAL2 (QUAL1=Primary!Qty, QUAL2=Prv)
        ;"Results: None
        ;
        KILL ARRAY
        NEW VIEN SET VIEN=0
        FOR  SET VIEN=$ORDER(^AUPNVSIT("C",DFN,VIEN)) QUIT:(+VIEN'>0)  DO
        . NEW VDT SET VDT=$PIECE($GET(^AUPNVSIT(VIEN,0)),"^",1)
        . IF VDT'>0 QUIT
        . NEW %Y,X,X1,X2 SET X1=SETDT,X2=VDT
        . DO ^%DTC
        . IF (X'=0)!(%Y=0) QUIT
        . NEW TEMPARRAY
        . DO PCE4NOTE^ORWPCE3(.TEMPARRAY,,DFN,LocIEN_";"_VDT)
        . MERGE ARRAY(VDT)=TEMPARRAY
        QUIT
        ;
DISPADDL(TMGRESULT,TMGIEN8925,TMGDFN) ;" TMG CPRS DISPLAY ADDITIONAL SIGNER
        ;"Purpose: RPC call to determine IF client should automatically
        ;"         display the additional signer dialog after signing
        ;"Input: TMGRESULT - PASS BY REFERENCE, an out parameter
        ;"       TMG8925IEN - IEN of note that was signed
        ;"       TMGDFN - Current User's DFN
        ;"Output: TMGRESULT = 0 or 1 depending on IF the dialog
        ;"                    should be displayed
        ;"Results: none
        SET TMGRESULT=0
        IF +TMGIEN8925'>0 GOTO DADN
        IF +TMGDFN'>0 GOTO DADN
        NEW TMGIEN8925D1 
        SET TMGIEN8925D1=$PIECE($GET(^TIU(8925,TMGIEN8925,0)),"^",1)
        IF +TMGIEN8925D1'>0 GOTO DADN
        NEW XCODE SET XCODE=$GET(^TIU(8925.1,TMGIEN8925D1,"TMGNSC"))
        IF XCODE="" GOTO DADN
        XECUTE XCODE
        IF $T=1 SET TMGRESULT=1        
DADN    QUIT
        ;
DISPCOS()  ;
        ;"Output of function is $TEST:1 means ask for cosigner
        ;"Uses TMGDFN, which is set by DISPADDL
        ;"Note: not only must the IEN be listed below, this
        ;"      function must be SET in TMG CPRS NEEDS COSIGNER
        ;"      CODE field of 8925.1 for EACH document type.
        NEW AUTHOR
        SET AUTHOR=$PIECE($GET(^TIU(8925,TMGIEN8925,12)),"^",2)
        IF (TMGIEN8925D1=1407)&(AUTHOR=TMGDFN) GOTO DCDN  ;"IEN OF PHONE NOTE
        IF (TMGIEN8925D1=1411)&(AUTHOR=TMGDFN) GOTO DCDN  ;"IEN OF NOTE
        IF (TMGIEN8925D1=1397)&(AUTHOR=TMGDFN) GOTO DCDN  ;"IEN OF LAB/XRAYS/STUDIES RESULTS
        IF (TMGIEN8925D1=1426)&(AUTHOR=TMGDFN) GOTO DCDN  ;"IEN OF TRANSFER RECORDS
        IF (TMGIEN8925D1=1413)&(AUTHOR=TMGDFN) GOTO DCDN  ;"IEN OF NO SHOW
        IF (TMGIEN8925D1=652)&(AUTHOR=TMGDFN) GOTO DCDN   ;"IEN OF CANCELLED APPOINTMENT
        IF (TMGIEN8925D1=1455)&(AUTHOR=TMGDFN) GOTO DCDN  ;"IEN OF CANCELLED APPTS
        IF (TMGIEN8925D1=1457)&(AUTHOR=TMGDFN) GOTO DCDN  ;"IEN OF INR
        IF (TMGIEN8925D1=1479)&(AUTHOR=TMGDFN) GOTO DCDN  ;"IEN OF UA
        IF (TMGIEN8925D1=113)&(AUTHOR=TMGDFN) GOTO DCDN   ;"IEN OF UDS        
        IF (TMGIEN8925D1=70)&(AUTHOR=TMGDFN) GOTO DCDN    ;"IEN OF CONSULTANT VISIT NOT KEPT
        IF (TMGIEN8925D1=1470)&(AUTHOR=TMGDFN) GOTO DCDN  ;"IEN OF HOSPITAL D/C SUMM (IMAGE)
        IF (TMGIEN8925D1=1471)&(AUTHOR=TMGDFN) GOTO DCDN  ;"IEN OF HOSPITAL DISCHARGE (IMAGE)
        IF (TMGIEN8925D1=2031)&(AUTHOR=TMGDFN) GOTO DCDN  ;"IEN OF HOSPITAL D/C MEDICATION RECONCILIATION
        IF (TMGIEN8925D1=1410)&(AUTHOR=TMGDFN) GOTO DCDN  ;"IEN OF NURSE NOTE
        IF (TMGIEN8925D1=2048)&(AUTHOR=TMGDFN) GOTO DCDN  ;"IEN OF NURSE VISIT
DCDN    QUIT
        ;
DISPADD() ;
        ;"Output of function is $TEST:1 means ask for cosigner
        ;"This function is used for addendums
        ;"Uses TMGIEN8925 and TMGDFN, which is set by DISPADDL
        NEW PARENTIEN,AUTHOR,PNTAUTHOR
        IF +TMGIEN8925'>0 GOTO ADDDN
        SET AUTHOR=$PIECE($GET(^TIU(8925,TMGIEN8925,12)),"^",2)
        SET PARENTIEN=$PIECE($GET(^TIU(8925,TMGIEN8925,0)),"^",6)
        IF +PARENTIEN'>0 GOTO ADDDN
        SET PNTAUTHOR=$PIECE($GET(^TIU(8925,PARENTIEN,12)),"^",2)
        ;"IF PNTAUTHOR'=AUTHOR GOTO ADDDN
        IF AUTHOR=TMGDFN GOTO ADDDN
ADDDN   QUIT
        ;
POSTCODE(TMGRESULT,TMGIEN8925) ;
        NEW TMGIEN8925D1
        SET TMGIEN8925D1=$PIECE($GET(^TIU(8925,TMGIEN8925,0)),"^",1)
        SET TMGRESULT=$GET(^TIU(8925.1,TMGIEN8925D1,"TMGPSC"))
        QUIT
        ;"      
GOODBP(DFN)  ;"
        ;"Purpose: To determine if today's blood pressure is a good value
        ;"Input: DFN - patient's IEN
        ;"Output: none
        ;"Result: empty if not found or statement to be dropped into note
        NEW TMGRESULT SET TMGRESULT=""        
        NEW TODAY DO NOW^%DTC SET TODAY=X
        SET DFN=+$GET(DFN) IF DFN'>0 QUIT TMGRESULT
        NEW AGE SET AGE=$$AGE^TIULO(DFN)
        IF AGE<65 QUIT TMGRESULT
        NEW BPIEN,ZN,BP,SYS,DIA
        SET BPIEN=$ORDER(^GMRD(120.51,"C","BP",0))
        NEW VITIEN SET VITIEN=0
        FOR  SET VITIEN=$ORDER(^GMR(120.5,"C",DFN,VITIEN)) QUIT:VITIEN'>0  DO
        . SET ZN=$GET(^GMR(120.5,VITIEN,0))
        . IF $PIECE(ZN,"^",3)=BPIEN DO
        . . IF $PIECE($PIECE(ZN,"^",1),".",1)=TODAY DO
        . . . SET BP=$PIECE(ZN,"^",8)
        . . . SET SYS=$PIECE(BP,"/",1)
        . . . IF SYS<130 DO
        . . . . SET TMGRESULT="SYS BP AT GOAL ("_SYS_"). CIRCLE </FONT><FONT style=""BACKGROUND-COLOR:#ffff66"">3074F</FONT>"
        . . . ELSE  IF SYS<140 DO
        . . . . SET TMGRESULT="SYS BP AT GOAL ("_SYS_"). CIRCLE </FONT><FONT style=""BACKGROUND-COLOR:#ffff66"">3075F</FONT>"
        . . . IF TMGRESULT'="" SET TMGRESULT=TMGRESULT_" ; "
        . . . SET DIA=$PIECE(BP,"/",2)
        . . . IF TMGRESULT'="" SET TMGRESULT="<FONT style=""BACKGROUND-COLOR:#ff0000"">"_TMGRESULT
        . . . IF DIA<80 DO
        . . . . SET TMGRESULT=TMGRESULT_"<FONT style=""BACKGROUND-COLOR:#ff0000"">DIA BP AT GOAL ("_DIA_"). CIRCLE </FONT><FONT style=""BACKGROUND-COLOR:#ffff66"">3078F</FONT>"
        . . . ELSE  IF DIA<90 DO
        . . . . SET TMGRESULT=TMGRESULT_"<FONT style=""BACKGROUND-COLOR:#ff0000"">DIA BP AT GOAL ("_DIA_"). CIRCLE </FONT><FONT style=""BACKGROUND-COLOR:#ffff66"">3079F</FONT>"
        QUIT TMGRESULT
        ;"
UNLOCK(TIUIEN)  ;"
        ;"Purpose: To unlock a TIU record that has been locked.
        SET TIUIEN=+$GET(TIUIEN)
        IF TIUIEN'>0  WRITE "NO IEN PROVIDED",! GOTO UNLDN
        LOCK ^TIU(8925,TIUIEN)
UNLDN   QUIT
        ;"
CANSIGN(TMGRESULT,DFN,TIUIEN) ;"
  ;"Purpose: Determine if a note can be signed.
  ;"Input: TMGRESULT(Output) - Either "1^CAN SIGN" if okay to sign
  ;"                                    "-1^"_'Message to be displayed' if not
  ;"       DFN - Patient IEN
  ;"       TIUIEN - IEN of note to be checked
  SET TMGRESULT="1^CAN SIGN"
  NEW TMGERROR SET TMGERROR=""
  NEW PHRASE1 SET PHRASE1="NOTE: BELOW IS UNEDITED EXAM TEMPLATE."
  NEW PHRASE2 SET PHRASE2="**PHYSICAL EXAM**"
  NEW PHRASE3 SET PHRASE3="[*FOLLOWUP*]"
  NEW PHRASE4 SET PHRASE4="[*LABORDER*]"
  NEW TIULINE,TIUTEXT SET TIULINE=0,TIUTEXT=""
  FOR  SET TIULINE=$ORDER(^TIU(8925,TIUIEN,"TEXT",TIULINE)) QUIT:TIULINE'>0  DO
  . SET TIUTEXT=TIUTEXT_$GET(^TIU(8925,TIUIEN,"TEXT",TIULINE,0))
  IF TIUTEXT[PHRASE1 DO
  . SET TMGERROR="PHYSICAN EXAM DOESN'T SEEM TO BE EDITED. MUST EDIT IT BEFORE NOTE CAN BE SIGNED"
  IF TIUTEXT[PHRASE2 DO
  . IF TMGERROR'="" SET TMGERROR=TMGERROR_$C(13,10)
  . SET TMGERROR=TMGERROR_"PHYSICAN EXAM DOESN'T SEEM TO BE ENTERED. MUST ENTERED IT BEFORE NOTE CAN BE SIGNED"  
  IF TIUTEXT[PHRASE3 DO
  . IF TMGERROR'="" SET TMGERROR=TMGERROR_$C(13,10)
  . SET TMGERROR=TMGERROR_"FOLLOWUP DATA DOESN'T SEEM TO BE ENTERED INTO THE NOTE. PRESS WIN+F TO USE FOLLOWUP DIALOG"
  IF TIUTEXT[PHRASE4 DO
  . IF TMGERROR'="" SET TMGERROR=TMGERROR_$C(13,10)
  . SET TMGERROR=TMGERROR_"LAB ORDER DATA DOESN'T SEEM TO BE ENTERED INTO THE NOTE. PRESS WIN+O TO USE LAB ORDER DIALOG"
  IF TMGERROR'="" DO
  . SET TMGRESULT="-1^This note cannot be signed. The following error(s) have been returned."_$C(13,10)_$C(13,10)_TMGERROR
  QUIT
  