TMGORAM1 ;TMG/kst-Anticoagulation Stuff ;12/6/17, 3/24/21
         ;;1.0;TMG-LIB;**1**;12/6/17
 ;
 ;"TMG RPC FUNCTIONS related to Anticoagulation app
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 12/6/17  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;" RPC -- Public Functions.
 ;"=======================================================================
 ;"GETTPLST(DUZ) ;"Entry point for TMG ORAM GET TEMPL ENTRIES
 ;"GETMPLAT(OUTREF,IEN,TMGDFN,VSTR) ;"Entry point for TMG ORAM GET TEMPLATE TEXT
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;"Dependencies:
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;
GETTPLST(RESULT,DUZ) ;"GET TEMPLATES LIST --  TO USE FOR ANTICOAGULATION NOTES
  ;"Input: DUZ -- user IEN
  ;"Result:  1^OK^<IEN>;<TemplateName>^<IEN>;<TemplateName>^<IEN>;<TemplateName>^<IEN>;<TemplateName>^<IEN>;<TemplateName>
  ;"         or -1^Error message
  ;"   piece#:    3 -- IEN for Intake Note Template (TIU TEMPLATE #8927);<TemplateName>
  ;"              4 -- IEN for Interim Note Template (TIU TEMPLATE #8927);<TemplateName>
  ;"              5 -- IEN for DC Note Template (TIU TEMPLATE #8927);<TemplateName>
  ;"              6 -- IEN for Missed Appt Template (TIU TEMPLATE #8927);<TemplateName>
  ;"              7 -- IEN for Note for Patient Template (TIU TEMPLATE #8927);<TemplateName>
  NEW INTAKEIEN,INTERIMIEN,DCIEN,MISSEDIEN,PATIEN
  SET INTAKEIEN=+$$GET^XPAR("SYS","TMG ORAM INTAKE NOTE",,"I")
  SET INTAKEIEN=INTAKEIEN_";"_$PIECE($GET(^TIU(8927,INTAKEIEN,0)),"^",1)
  SET INTERIMIEN=$$GET^XPAR("SYS","TMG ORAM INTERIM NOTE",,"I")
  SET INTERIMIEN=INTERIMIEN_";"_$PIECE($GET(^TIU(8927,INTERIMIEN,0)),"^",1)
  SET DCIEN=$$GET^XPAR("SYS","TMG ORAM DC NOTE",,"I")
  SET DCIEN=DCIEN_";"_$PIECE($GET(^TIU(8927,DCIEN,0)),"^",1)
  SET MISSEDIEN=$$GET^XPAR("SYS","TMG ORAM MISSED APPT NOTE",,"I")
  SET MISSEDIEN=MISSEDIEN_";"_$PIECE($GET(^TIU(8927,MISSEDIEN,0)),"^",1)
  SET PATIEN=$$GET^XPAR("SYS","TMG ORAM PATIENT NOTE",,"I")
  SET PATIEN=PATIEN_";"_$PIECE($GET(^TIU(8927,PATIEN,0)),"^",1)
  SET RESULT="1^OK^"_INTAKEIEN_"^"_INTERIMIEN_"^"_DCIEN_"^"_MISSEDIEN_"^"_PATIEN
  QUIT
  ;
GETMPLAT(OUTREF,IEN,TMGDFN,VSTR) ;-- GET TEMPLATE TEXT, WITH OBJECTS RESOLVED
  ;"INPUT: OUTREF -- pass by reference, AN OUT PARAMETER.  This is **REF** to result
  ;"       IEN -- The IEN of the template to retrieve
  ;"       TMGDFN -- Patient IEN
  ;"       VSTR -- Visit String (should be optional)
  NEW TMGZZ SET TMGZZ=0
  IF TMGZZ=1 DO
  . SET IEN=$GET(^TMG("TMP","GETMPLAT^TMGORAM1","IEN"))
  . SET TMGDFN=$GET(^TMG("TMP","GETMPLAT^TMGORAM1","DFN"))
  . SET VSTR=$GET(^TMG("TMP","GETMPLAT^TMGORAM1","VSTR"))
  ELSE  DO
  . SET ^TMG("TMP","GETMPLAT^TMGORAM1","IEN")=$GET(IEN)
  . SET ^TMG("TMP","GETMPLAT^TMGORAM1","DFN")=$GET(TMGDFN)
  . SET ^TMG("TMP","GETMPLAT^TMGORAM1","VSTR")=$GET(VSTR)
  NEW REF,ARR
  SET IEN=+$GET(IEN)
  IF IEN>0 DO
  . DO GETBOIL^TIUSRVT(.REF,IEN)  ;"get unexpanded boilerplate text
  . NEW TEMP MERGE TEMP=@REF
  . NEW IDX SET IDX=0
  . FOR  SET IDX=$ORDER(TEMP(IDX)) QUIT:IDX'>0  SET ARR(IDX,0)=$GET(TEMP(IDX))
  . DO GETTEXT^TIUSRVT(.OUTREF,.TMGDFN,.VSTR,.ARR)
  ELSE  DO
  . SET OUTREF=$NAME(^TMP("TMG ORAM1",$J))  ;"<-- EMPTY ARRAY TO RETURN
  QUIT
  ;