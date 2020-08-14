TMGC0QT4 ;TMG/kst/TMG Clinical tests for patient;1/12/17
         ;;1.0;TMG-LIB;**1**;1/12/17
  ;
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;"Copyright (c) 1/12/17  Kevin S. Toppenberg MD
  ;"
  ;"This file is part of the TMG LIBRARY, and may only be used in accordence
  ;" to license terms outlined in separate file TMGLICNS.m, which should 
  ;" always be distributed with this file.
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;
  ;"=======================================================================
  ;" API -- Public Functions.
  ;"=======================================================================
  ;"ONHTNTX(DFN,DT) -- Is patient on HTN treatment as of DT (if DT supplied)?
  ;"ONLIPDTX(DFN,DT) --Is patient on LIPID treatment as of DT (if DT supplied)?
  ;"ONDMTX(DFN,DT) --Is patient on LIPID treatment as of DT (if DT supplied)?
  ;"TOBACCO(DFN,DT) -- Is patient a tobacco user, as of DT, using health factors
  ;"AGE(DFN,DT)  -- Return patients age, in years, as of DT
  ;"TABHASRX(DFN,TABLNAME,OUT,DT) -- DOES TABLE HAVA ASSOCIATED MEDICATIONS?
  ; 
  ;"=======================================================================
  ;"PRIVATE API FUNCTIONS
  ;"=======================================================================
  ;
  ;"=======================================================================
  ;
ONHTNTX(DFN,DT,OUT) ;"Is patient on HTN treatment as of DT (if DT supplied)?
  ;"INPUT: DFN -- Patient IEN
  ;"       DT -- OPTIONAL.  If desire to test AS OF a given FMDT, then pass in.
  ;"       OUT -- Array containing the particular medications that are in
  ;"              the HTN table.
  ;"Result: 1 if on HTN treatment, or 0 if not.
  NEW TMGRESULT SET TMGRESULT=$$TABHASRX(DFN,"HYPERTENSION",.OUT,DT)
  QUIT (TMGRESULT=1)  
  ;
ONLIPDTX(DFN,DT) ;"Is patient on LIPID treatment as of DT (if DT supplied)?
  ;"INPUT: DFN -- Patient IEN
  ;"       DT -- OPTIONAL.  If desire to test AS OF a given FMDT, then pass in.
  ;"Result: 1 if on HTN treatment, or 0 if not.
  NEW TMGRESULT,OUT SET TMGRESULT=$$TABHASRX(DFN,"LIPIDS",.OUT,DT)
  QUIT (TMGRESULT=1)  
  ;
ONGERDTX(DFN,DT,OUT) ;"Is patient on GERD treatment as of DT (if DT supplied)?
  ;"INPUT: DFN -- Patient IEN
  ;"       DT -- OPTIONAL.  If desire to test AS OF a given FMDT, then pass in.
  ;"Result: 1 if on HTN treatment, or 0 if not.
  NEW TMGRESULT,OUT SET TMGRESULT=$$TABHASRX(DFN,"EGD COHORT MEDS",.OUT,DT)
  QUIT (TMGRESULT=1)  
  ;  
ONDMTX(DFN,DT) ;"Is patient diabetic?
  ;"INPUT: DFN -- Patient IEN
  ;"       DT -- OPTIONAL.  If desire to test AS OF a given FMDT, then pass in.
  ;"                      NOTE: DT is ignore here, not implemented...
  ;"Result: 1 if on HTN treatment, or 0 if not.
  ;
  ;"NOTE: The line below was not good.  Lisinopril would show in diabetic studies table.
  ;"      So all HTN patients on ACE/ARB etc would test as DM.
  ;"NEW TMGRESULT,OUT SET TMGRESULT=$$TABHASRX(DFN,"DIABETIC STUDIES",.OUT,DT)
  NEW BOOL DO PTHASDM^TMGPXR01(DFN,.BOOL)  
  QUIT ($GET(BOOL)=1)  
  ;
TABHASRX(DFN,TABLNAME,OUT,DT)  ;"DOES TABLE HAVE ASSOCIATED MEDICATIONS?
  ;"Input: DFN -- Patient IEN
  ;"       TABLNAME -- Name of table to check
  ;"       OUT -- OPTIONAL, PASS BY REFERENCE.  ARRAY TO GET BACK LIST OF MEDS
  ;"       DT -- OPTIONAL.  If desire to test AS OF a given FMDT, then pass in.
  ;"Result: 1 if has meds, 0 if no meds, -1 if problem.  
  NEW TMGRESULT SET TMGRESULT=-1
  DO TIMEPFIL^TMGMISC2("GETITEM^TMGTIUO8","TABHASRX^TMGC0QT4",1)  ;"RECORD START TIME  
  NEW OPTION,IEN22708
  IF $DATA(DT)#10 SET OPTION("DT")=DT
  SET IEN22708=$ORDER(^TMG(22708,"B",TABLNAME,0)) GOTO:(IEN22708'>0) THRXDN 
  SET OPTION("ONLYRELATED")=1   ;"3/17/20
  DO GTLKMDARR^TMGTIUO8(.OUT,DFN,IEN22708,.OPTION)
  SET TMGRESULT=($DATA(OUT)>0)
THRXDN ;
  DO TIMEPFIL^TMGMISC2("GETITEM^TMGTIUO8","TABHASRX^TMGC0QT4",0)  ;"RECORD END TIME  
  QUIT TMGRESULT
  ;
ONCSDBRX(DFN,DT,OUT) ;"Is patient on a medication that should trigger CSDB check?
  NEW TMGRESULT SET TMGRESULT=$$TABHASRX(DFN,"CSDB MEDICATIONS",.OUT,DT)
  QUIT (TMGRESULT=1)
  ;"
TOBACCO(DFN,DT)  ;"Is patient a tobacco user, as of DT, using health factors
  ;"NOTE: This will be different from TOBACCO^TMGC0QT2, which uses SOCIAL table
  ;"INPUT: DFN -- Patient IEN
  ;"       DT -- OPTIONAL.  If desire to test AS OF a given FMDT, then pass in.
  ;"Result: 1 if tobacco user, or 0 if not.
  NEW TMGRESULT SET TMGRESULT=0
  NEW OPTION IF $DATA(DT)#10 SET OPTION("DT")=DT
  NEW HFARR,TEMPARR,TEMP,USEDT,QUITDT
  SET TEMP=$$GETHFDT^TMGPXRU1(DFN,"TMG TOBACCO EVERYDAY USER",.TEMPARR,.OPTION)
  MERGE HFARR=TEMPARR
  SET TEMP=$$GETHFDT^TMGPXRU1(DFN,"TMG TOBACCO SOMEDAY USER",.TEMPARR,.OPTION)
  MERGE HFARR=TEMPARR
  SET USEDT=+$ORDER(HFARR(""),-1) 
  SET QUITDT=$$GETHFDT^TMGPXRU1(DFN,"TMG TOBACCO FORMER USER",.QUITARR,.OPTION)
  SET TMGRESULT=(USEDT>QUITDT)  
  QUIT TMGRESULT
  ;
AGE(DFN,DT)  ;"Return patients age, in years, as of DT
  ;"Input:  DFN -- Patient IEN
  ;"        DT -- OPTIONAL.  Default is NOW
  ;"Result: returns age in years (including fractional years as decimal)
  SET DFN=+$GET(DFN),DT=+$GET(DT) IF DT=0 SET DT=$$NOW^XLFDT
  NEW DOB SET DOB=$PIECE($GET(^DPT(DFN,0)),"^",3) 
  NEW DAYS SET DAYS=$$FMDIFF^XLFDT(DT,DOB)
  NEW YRS SET YRS=DAYS/365
  QUIT YRS
  ;
HASASCVD(DFN,DT) ;"Does patient have ASCVD health factor, as of DT?
  ;"Input:  DFN -- Patient IEN
  ;"        DT -- OPTIONAL.  Default is NOW
  ;"Result: 1 if has ASCVD health factor, -1 if HF not found, or 0 if noted NEG  
  NEW TMGRESULT SET TMGRESULT=0
  ;"HARD CODING THE IEN FOR "TMG ASCVD CATEGORY" (2251), TO PROTECT AGAINST FUTURE NAME CHANGES
  NEW HFARRAY DO GETHFGRP^TMGPXR01(DFN,2251,.HFARRAY)
  ;"THE RESULTS FROM THE ABOVE CALL ARE: HFARRAY(DATE,HFIEN)
  NEW DATE SET DATE=$ORDER(HFARRAY(9999999),-1)
  NEW LASTHF SET LASTHF=$ORDER(HFARRAY(DATE,0))
  ;"HARD CODED IEN (2253) BELOW TO PROTECT AGAINST NAME CHANGES
  IF LASTHF=2253 SET TMGRESULT=1
  QUIT TMGRESULT
  ;"