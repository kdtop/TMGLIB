TMGRPU1 ;TMG/kst/Reports Utilities;10/12/16
         ;;1.0;TMG-LIB;**1**;10/12/16
  ;
  ;"TMG MISCELLANEOUS FUNCTIONS
  ;
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;"Copyright (c) 10/12/2016  Kevin S. Toppenberg MD
  ;"
  ;"This file is part of the TMG LIBRARY, and may only be used in accordence
  ;" to license terms outlined in separate file TMGLICNS.m, which should 
  ;" always be distributed with this file.
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;
  ;"=======================================================================
  ;" API -- Public Functions.
  ;"=======================================================================
  ;"HASCPT(DFN,CPT,SDT,EDT) -- boolean if patient has CPT during specified date range.
  ;"CPTIEN(CPT) -- Get CPT IEN for code, or -1 if not found
  ;"HSCPTIEN(DFN,CPTIEN,SDT,EDT) -- boolean if patient has CPTIEN during specified date range.
  ;"VISTLIST(DFN,SDT,EDT,OUT) -- list of VISIT file entries for date range.
  ;"PTWCPT(CPT,OUT) -- Get array of all patients with given CPT code. 
  ; 
  ;"=======================================================================
  ;"PRIVATE API FUNCTIONS
  ;"=======================================================================
  ;
  ;"=======================================================================
  ;
HASCPT(DFN,CPT,SDT,EDT) ;
  ;"Purpose: return boolean if patient has CPT during specified date range.
  ;"Input: DFN -- patient IEN
  ;"       CPT -- String of the CPT 
  ;"       SDT -- Starting date of range, FM format, default is 0
  ;"       EDT -- Starting date of range, FM format, default is 9999999
  ;"Result: 1 if has cpt, 0 if not, or -1^Message if error
  NEW TMGRESULT SET TMGRESULT=0
  NEW IEN81 SET IEN81=$$CPTIEN(.CPT)
  IF IEN81'>0 DO  GOTO HCPTDN
  . SET TMGRESULT="-1^CPT not found in file 81.  Got ["_CPT_"]"
  SET TMGRESULT=$$HSCPTIEN(.DFN,IEN81,.SDT,.EDT)
HCPTDN ; 
  QUIT TMGRESULT
  ;
CPTIEN(CPT) ;"Get CPT IEN for code, or -1 if not found
  NEW TMGRESULT SET TMGRESULT=-1
  SET CPT=$GET(CPT) IF CPT="" GOTO CIDN
  SET TMGRESULT=+$ORDER(^ICPT("B",CPT,0))
CIDN ; 
  QUIT TMGRESULT
  ;
GETCPT(CPTIEN) ;"Get CPT name for cpt code or '' if not found
  ;"RETURNS: CPTCODE^CPTNAME
  QUIT $PIECE($GET(^ICPT(+$GET(CPTIEN),0)),"^",1,2)
  ;
HSCPTIEN(DFN,CPTIEN,SDT,EDT) ;
  ;"Purpose: return boolean if patient has CPT during specified date range.
  ;"Input: DFN -- patient IEN
  ;"       CPIENT -- IEN in file 81 (CPT) 
  ;"       SDT -- Starting date of range, FM format, default is 0
  ;"       EDT -- Starting date of range, FM format, default is 9999999
  ;"Result: 1 if has cpt, 0 if not, or -1^Message if error
  NEW TMGRESULT SET TMGRESULT=0
  SET DFN=+$GET(DFN) IF DFN'>0 DO  GOTO CPTDN2
  . SET TMGRESULT="-1^DFN not provided.  Got ["_$GET(DFN)_"]"
  SET CPTIEN=+$GET(CPTIEN) IF CPTIEN'>0 DO  GOTO CPTDN2
  . SET TMGRESULT="-1^CPT IEN not provided.  Got ["_$GET(CPTIEN)_"]"
  SET SDT=+$GET(SDT)
  SET EDT=+$GET(EDT) IF EDT=0 SET EDT=9999999
  NEW RSDT SET RSDT=9999999-SDT
  NEW REDT SET REDT=9999999-EDT
  NEW ARDT SET ARDT=REDT-0.00000001
  NEW ARR
  FOR  SET ARDT=$ORDER(^AUPNVCPT("AA",DFN,CPTIEN,ARDT)) QUIT:(+ARDT'>0)!(+ARDT>RSDT)  DO
  . NEW IEN SET IEN=0
  . FOR  SET IEN=$ORDER(^AUPNVCPT("AA",DFN,CPTIEN,ARDT,IEN)) QUIT:+IEN'>0  DO
  . . SET ARR(IEN)=9999999-ARDT
  SET TMGRESULT=($DATA(ARR)'=0)
CPTDN2 ;   
  QUIT TMGRESULT
  ;
HASICD(DFN,ICD,SDT,EDT) ;"
  ;"Purpose: return boolean if patient has ICD during specified date range.
  ;"Input: DFN -- patient IEN
  ;"       CPT -- String of the CPT
  ;"       SDT -- Starting date of range, FM format, default is 0
  ;"       EDT -- Starting date of range, FM format, default is 9999999
  ;"Result: 1 if has cpt, 0 if not, or -1^Message if error
  NEW TMGRESULT SET TMGRESULT=0
  NEW IEN80 SET IEN80=+$ORDER(^ICD9("AB",ICD,0))
  IF IEN80'>0 DO  GOTO HICDDN
  . SET TMGRESULT="-1^ICD NOT FOUND IN FILE 80"
  SET TMGRESULT=$$HSICDIEN(.DFN,IEN80,.SDT,.EDT)  
HICDDN ;"
  QUIT TMGRESULT
  ;"
HSICDIEN(DFN,ICDIEN,SDT,EDT) ;"
  NEW TMGRESULT SET TMGRESULT=0
  SET DFN=+$GET(DFN) IF DFN'>0 DO  GOTO CPTDN2
  . SET TMGRESULT="-1^DFN not provided.  Got ["_$GET(DFN)_"]"
  SET ICDIEN=+$GET(ICDIEN) IF ICDIEN'>0 DO  GOTO ICDDN2
  . SET TMGRESULT="-1^ICD IEN not provided.  Got ["_$GET(ICDIEN)_"]"
  SET SDT=+$GET(SDT)
  SET EDT=+$GET(EDT) IF EDT=0 SET EDT=9999999
  NEW RSDT SET RSDT=9999999-SDT
  NEW REDT SET REDT=9999999-EDT
  NEW ARDT SET ARDT=REDT-0.00000001
  NEW ARR
  FOR  SET ARDT=$ORDER(^AUPNVPOV("AA",DFN,ARDT)) QUIT:(+ARDT'>0)!(+ARDT>RSDT)  DO
  . NEW IEN SET IEN=0
  . FOR  SET IEN=$ORDER(^AUPNVPOV("AA",DFN,ARDT,IEN)) QUIT:+IEN'>0  DO
  . . NEW CODE SET CODE=$P($G(^AUPNVPOV(IEN,0)),"^",1)
  . . IF CODE=ICDIEN DO
  . . . SET ARR(IEN)=9999999-ARDT
  SET TMGRESULT=($DATA(ARR)'=0)
ICDDN2  ;
  QUIT TMGRESULT
  ;"
VISTLIST(DFN,SDT,EDT,OUT)  ;"Get list of VISIT file entries for date range.
  ;"Input: DFN -- patient IEN
  ;"       SDT -- Starting date of range, FM format, default is 0
  ;"       EDT -- Starting date of range, FM format, default is 9999999
  ;"       OUT -- PASS BY REFERENCE, AN OUT PARAMETER.  Format:
  ;"           OUT(IEN9000010)=DT
  SET DFN=+$GET(DFN)
  SET SDT=+$GET(SDT)
  SET EDT=+$GET(EDT) IF EDT=0 SET EDT=9999999
  NEW RSDT SET RSDT=9999999-SDT
  NEW REDT SET REDT=9999999-EDT
  NEW ARDT SET ARDT=REDT-0.00000001
  FOR  SET ARDT=$ORDER(^AUPNVSIT("AA",DFN,ARDT)) QUIT:(+ARDT'>0)!(+ARDT>RSDT)  DO
  . NEW IEN SET IEN=0
  . FOR  SET IEN=$ORDER(^AUPNVSIT("AA",DFN,ARDT,IEN)) QUIT:+IEN'>0  DO
  . . SET OUT(IEN)=9999999-ARDT
  QUIT
  ;
PTWCPT(CPT,OUT) ;
  ;"Purpose: Get array of all patients with given CPT code. 
  ;"Input: CPT -- CPT code to find.  NOT IEN
  ;"       OUT -- PASS BY REFERENCE, AN OUT PARAMETER.  Format:
  ;"           OUT(CPT,DFN)=""
  ;"Purpose: Get list of patients with CPT code 
  NEW IEN81 SET IEN81=$$CPTIEN(.CPT)
  IF IEN81'>0 QUIT
  NEW IEN SET IEN=0
  FOR  SET IEN=$ORDER(^AUPNVCPT("B",IEN81,IEN)) QUIT:+IEN'>0  DO
  . NEW ZN SET ZN=$GET(^AUPNVCPT(IEN,0)) QUIT:ZN=""
  . NEW DFN SET DFN=+$PIECE(ZN,"^",2) QUIT:DFN'>0
  . SET OUT(CPT,DFN)=""
  QUIT
  ;
ALLCPT(OUT,SDT,EDT) ;"Get list of all cpt's on file during date range.
  ;"Input: OUT -- PASS BY REFERENCE.  Format:
  ;"           OUT(DFN)=PATIENT NAME
  ;"           OUT(DFN,CPT,DT)=CPT^CPT_NAME^EXT DATE-TIME
  ;"           OUT("CPT",CPT)=CPT_NAME
  ;"           OUT("CPT",CPT,DT,DFN)=""
  ;"           OUT("CPTIEN",CPTIEN)=CPT^CPT_NAME
  ;"           OUT("DT",DT,DFN,CPT)=""
  ;"       SDT -- Starting date of range, FM format, default is 0
  ;"       EDT -- Starting date of range, FM format, default is 9999999
  ;"Result: None
  NEW DFN SET DFN=0
  FOR  SET DFN=$ORDER(^AUPNVCPT("AA",DFN)) QUIT:+DFN'>0  DO
  . DO ALDFNCPT(.OUT,DFN,.SDT,.EDT)
  QUIT  
  ;
ALDFNCPT(OUT,DFN,SDT,EDT) ;"Get list of all cpt's FOR DFN during date range.
  ;"Input: OUT -- PASS BY REFERENCE.  Format:
  ;"           OUT(DFN)=PATIENT NAME
  ;"           OUT(DFN,CPT,DT)=CPT^CPT_NAME^EXT DATE-TIME
  ;"           OUT("CPT",CPT)=CPT_NAME
  ;"           OUT("CPT",CPT,DT,DFN)=""
  ;"           OUT("CPTIEN",CPTIEN)=CPT^CPT_NAME
  ;"           OUT("DT",DT,DFN,CPT)=""
  ;"       DFN - PATIENT
  ;"       SDT -- Starting date of range, FM format, default is 0
  ;"       EDT -- Starting date of range, FM format, default is 9999999
  ;"Result: None
  SET SDT=+$GET(SDT)
  SET EDT=+$GET(EDT) IF EDT=0 SET EDT=9999999
  NEW RSDT SET RSDT=9999999-SDT
  NEW REDT SET REDT=9999999-EDT
  NEW ARDT SET ARDT=REDT-0.00000001
  NEW PTNAME SET PTNAME=$PIECE($GET(^DPT(DFN,0)),"^",1)
  SET OUT(DFN)=PTNAME
  NEW CPTIEN SET CPTIEN=0
  FOR  SET CPTIEN=$ORDER(^AUPNVCPT("AA",DFN,CPTIEN)) QUIT:(+CPTIEN'>0)  DO
  . NEW CPT SET CPT=$$GETCPT(CPTIEN)  ;"E.G. '99215^OFFICE VISIT 5'  
  . NEW CPTCODE SET CPTCODE=$PIECE(CPT,"^",1)
  . NEW CPTNAME SET CPTNAME=$PIECE(CPT,"^",2)
  . SET OUT("CPTIEN",CPTIEN)=CPT
  . SET OUT("CPT",CPTCODE)=CPTNAME  
  . NEW ARDT SET ARDT=REDT-0.00000001
  . FOR  SET ARDT=$ORDER(^AUPNVCPT("AA",DFN,CPTIEN,ARDT)) QUIT:(+ARDT'>0)!(+ARDT>RSDT)  DO
  . . NEW ADT SET ADT=9999999-ARDT
  . . NEW DTSTR SET DTSTR=$$FMTE^XLFDT(ADT)
  . . NEW IEN SET IEN=0
  . . FOR  SET IEN=$ORDER(^AUPNVCPT("AA",DFN,CPTIEN,ARDT,IEN)) QUIT:+IEN'>0  DO
  . . . SET OUT(DFN,CPTCODE,ADT)=CPT_"^"_DTSTR
  . . . SET OUT("DT",ADT,DFN,CPTCODE)=""
  . . . SET OUT("CPT",CPTCODE,ADT,DFN)=""
  QUIT  
  ;  
