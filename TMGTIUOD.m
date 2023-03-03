TMGTIUOD ;TMG/kst-TIU OBJECTS ; 01/12/17, 5/2/18, 3/24/21
         ;;1.0;TMG-LIB;**1,17**;01/12/17
 ;"
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 1/12/17  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"Note: Data downloaded from here on 1/12/17:
 ;"      https://www.nhlbi.nih.gov/health-pro/guidelines/current/cholesterol-guidelines/quick-desk-reference-html/10-year-risk-framingham-table
 ;
 ;"=======================================================================
 ;"PUBLIC FUNCTIONS
 ;"=======================================================================
 ;"PTCVDRSK(TMGDFN,AODT)  -- 10 YR CVD RISK CALCULATOR FROM FRAMINGHAM DATA, for a specified patient, as of AODT.
 ;"CVDR10YR(YR,FEMALE,SBP,ONRX,TCH,HDL,SMK,DM2)  --10 YR CVD RISK CALCULATOR FROM FRAMINGHAM DATA
 ;"CVDRSKTX(TMGDFN,AODT)  --10 YR CVD RISK with explaination
 ;"CVDR(TMGDFN,AODT,TMGTEXT) --10 YR CVD RISK with explaination   
 ;"RISKTBL(TMGDFN)
 ;"RISKGRP(TMGDFN,AODT,TMGTEXT)  
 ;"TIUCVDR(TMGDFN) 
 ;"CVDTABLE(TMGDFN)  
 ;"INRXTAXN(IEN22733,IEN22733D3) -- IS MEDICATION IN RX TAXONOMY?  
 ;"STATNINT(TMGDFN) -- STATIN INTENSITY CLASS INFORMATION 
 ;"
 ;"TEST  
 ;"TESTPTS 
 ;"TESTGRP(ARR) 
 ;"=======================================================================
 ;"PRIVATE FUNCTIONS
 ;"=======================================================================
 ;"TFTEXT(B,VALIFTRUE,VALIFFALSE) ;
 ;"BOOL(V) ;
 ;"INRANGE(VAL,LOW,HI) 
 ;"AGE2PT(YR,FEMALE) 
 ;"HDL2PT(HDL,FEMALE) --HDL CHOLESTEROL TO POINTS. 
 ;"TCH2PT(TCH,YR,FEMALE)  --TOTAL CHOLESTEROL TO POINTS.  
 ;"SBP2PT(SBP,FEMALE,ONRX)  --SYSTOLIC BP (mmHg) TO POINTS
 ;"SMOKR2PT(VAL,YR,FEMALE)  --SMOKING STATUS TO POINTS.  VAL=1 IF SMOKER, 0 IF NOT;  YR=AGE
 ;"DM2PT(VAL)  --DIABETES STATUS TO POINTS.  VAL=1 IF DIABETIC, 0 IF NOT
 ;"PT2RISK(PTS,FEMALE)  --CONVERT POINTS TO CARDIOVASCULAR RISK %
 ;"HDL(TMGDFN,AODT)     --Return the last HDL value, prior to date provided
 ;"TCHOL(TMGDFN,AODT)   --Return the last HDL value, prior to date provided
 ;"GETLAB(TMGDFN,AODT,LABNAME,LABNUM)  
 ;"=======================================================================
 ;"Dependencies: 
 ;"=======================================================================
 ;
PTCVDRSK(TMGDFN,AODT)  ;"10 YR CVD RISK CALCULATOR FROM FRAMINGHAM DATA, for a specified patient, as of AODT.
  ;"Input: TMGDFN -- patient IEN
  ;"       AODT -- Optional.  Default is NOW.  AODT for calculation of risk.  
  ;"Results: -1 if invalid input, else number 0-30 as % risk of CVD event in next 10 yrs
  ;" NEW TMGRESULT SET TMGRESULT=-1
  ;" SET TMGDFN=+$GET(TMGDFN) IF $DATA(^DPT(TMGDFN,0))=0 GOTO PCRDN 
  ;" SET AODT=+$GET(AODT) IF AODT'>0 SET AODT=$$NOW^XLFDT
  ;" NEW ONRX SET ONRX=$$ONHTNTX^TMGC0QT4(TMGDFN,AODT)
  ;" NEW SMK SET SMK=$$TOBACCO^TMGC0QT4(TMGDFN,AODT)
  ;" NEW SBP SET SBP=$PIECE($PIECE($$BP^TMGGMRV1(TMGDFN,AODT),"^",1),"/",1)
  ;" NEW HDL SET HDL=$$HDL(TMGDFN,AODT) IF HDL'>0 GOTO PCRDN
  ;" NEW TCHOL SET TCHOL=$$TCHOL(TMGDFN,AODT) IF TCHOL'>0 GOTO PCRDN
  ;" NEW FEMALE SET FEMALE=($PIECE($GET(^DPT(TMGDFN,0)),"^",2)="F")
  ;" NEW AGE SET AGE=$$AGE^TMGC0QT4(TMGDFN,AODT)
  ;" SET TMGRESULT=$$CVDR10YR(AGE,FEMALE,SBP,ONRX,TCHOL,HDL,SMK)
  NEW TMGRESULT,TMGTEXT SET TMGRESULT=$$CVDR(.TMGDFN,.AODT,.TMGTEXT)
PCRDN ;  
  QUIT TMGRESULT
  ;
CVDRSKTX(TMGDFN,AODT)  ;"10 YR CVD RISK with explaination
  NEW TEMP SET TEMP=$$CVDR(.TMGDFN,.AODT,.TMGTEXT)
  QUIT TMGTEXT
  ;
CVDR(TMGDFN,AODT,TMGTEXT)  ;"10 YR CVD RISK with explaination  
  ;"Input: TMGDFN -- patient IEN
  ;"       AODT -- Optional.  Default is NOW.  AODT for calculation of risk.  
  ;"       TMGTEXT -- PASS BY REFERENCE.  An OUT PARAMETER.  Text to explain finding
  ;"Results: -1 if invalid input, else number 0-30 as % risk of CVD event in next 10 yrs
  NEW ZZCVDDEBUG SET ZZCVDDEBUG=1  ;"//SET TO 0 TO SHUT OFF
  NEW TMGRESULT SET TMGRESULT=-1
  NEW OUTARR
  SET TMGTEXT="(unable to determine)"
  SET TMGDFN=+$GET(TMGDFN) IF $DATA(^DPT(TMGDFN,0))=0 GOTO PCRDN2 
  SET AODT=+$GET(AODT)
  IF AODT>0 SET TMGTEXT="As of "_$$FMTE^XLFDT(AODT,"5D")_": "
  IF AODT'>0 SET AODT=$$NOW^XLFDT SET TMGTEXT=""
  IF ZZCVDDEBUG DO TIMEPFIL^TMGMISC2("GETITEM^TMGTIUO8","CVDR^ONHTNTX",1)  ;"RECORD START TIME  
  NEW ONRX SET ONRX=$$ONHTNTX^TMGC0QT4(TMGDFN,AODT,.OUTARR)
  IF ZZCVDDEBUG DO TIMEPFIL^TMGMISC2("GETITEM^TMGTIUO8","CVDR^ONHTNTX",0)  ;"RECORD END TIME  
  SET TMGTEXT=TMGTEXT_"HTN Tx="_ONRX_", "
  IF ZZCVDDEBUG DO TIMEPFIL^TMGMISC2("GETITEM^TMGTIUO8","CVDR^TOBACCO",1)  ;"RECORD START TIME  
  NEW SMK SET SMK=$$TOBACCO^TMGC0QT4(TMGDFN,AODT)
  IF ZZCVDDEBUG DO TIMEPFIL^TMGMISC2("GETITEM^TMGTIUO8","CVDR^TOBACCO",0)  ;"RECORD END TIME  
  SET TMGTEXT=TMGTEXT_"Smoker="_SMK_", "
  IF ZZCVDDEBUG DO TIMEPFIL^TMGMISC2("GETITEM^TMGTIUO8","CVDR^BP",1)  ;"RECORD START TIME  
  NEW SBP SET SBP=$PIECE($PIECE($$BP^TMGGMRV1(TMGDFN,AODT),"^",1),"/",1)
  IF ZZCVDDEBUG DO TIMEPFIL^TMGMISC2("GETITEM^TMGTIUO8","CVDR^BP",0)  ;"RECORD END TIME  
  SET TMGTEXT=TMGTEXT_"Sys BP="_SBP_", "
  IF ZZCVDDEBUG DO TIMEPFIL^TMGMISC2("GETITEM^TMGTIUO8","CVDR^HDL",1)  ;"RECORD START TIME  
  NEW HDL SET HDL=$$HDL(TMGDFN,AODT) 
  IF ZZCVDDEBUG DO TIMEPFIL^TMGMISC2("GETITEM^TMGTIUO8","CVDR^HDL",0)  ;"RECORD END TIME  
  IF HDL'>0 SET TMGTEXT=TMGTEXT_"No HDL --> can't calculate." GOTO PCRDN2
  SET TMGTEXT=TMGTEXT_"HDL="_HDL_", "
  IF ZZCVDDEBUG DO TIMEPFIL^TMGMISC2("GETITEM^TMGTIUO8","CVDR^TCHOL",1)  ;"RECORD START TIME  
  NEW TCHOL SET TCHOL=$$TCHOL(TMGDFN,AODT) 
  IF ZZCVDDEBUG DO TIMEPFIL^TMGMISC2("GETITEM^TMGTIUO8","CVDR^TCHOL",0)  ;"RECORD END TIME  
  IF TCHOL'>0 SET TMGTEXT=TMGTEXT_"No T.Chol --> can't calculate." GOTO PCRDN2
  SET TMGTEXT=TMGTEXT_"T.Chol="_TCHOL_", "
  NEW FEMALE SET FEMALE=($PIECE($GET(^DPT(TMGDFN,0)),"^",2)="F")
  NEW AGE SET AGE=$$AGE^TMGC0QT4(TMGDFN,AODT)
  IF ZZCVDDEBUG DO TIMEPFIL^TMGMISC2("GETITEM^TMGTIUO8","CVDR^CVDR10YR",1)  ;"RECORD START TIME  
  SET TMGRESULT=$$CVDR10YR(AGE,FEMALE,SBP,ONRX,TCHOL,HDL,SMK)
  IF ZZCVDDEBUG DO TIMEPFIL^TMGMISC2("GETITEM^TMGTIUO8","CVDR^CVDR10YR",0)  ;"RECORD STOP TIME
  NEW ASCVD SET ASCVD=$$HASASCVD^TMGC0QT4(TMGDFN)
  SET ASCVD=$$TFTEXT(ASCVD,"+","-")
  SET TMGTEXT=TMGTEXT_"AGE, SEX --> 10 yr CVD risk="_TMGRESULT_"%. ASCVD ("_ASCVD_")" 
PCRDN2 ;  
  DO FILECVD(TMGDFN,TMGRESULT)
  QUIT TMGRESULT
  ;
FILECVD(TMGDFN,CVDRISK)   ;"FILE VALUE AS LAB VALUE
  ;"SINCE THIS IS BEING DONE DURING THE CVD CALCULATION, NO RESULTS WILL BE PASSED
  ;"INSTEAD ERRORS WILL BE SENT AS NOTIFICATIONS TO ELH
  IF CVDRISK="-1" QUIT
  NEW DATAARR,X
  DO NOW^%DTC
  SET DATAARR(0)="<METADATA>"
  SET DATAARR(1)="DT_COMPLETED = "_X
  SET DATAARR(2)="PROVIDER = 217^Doctor,Unspecified^- Physician"
  SET DATAARR(3)="LOCATION = 69^Family Phys of Greeneville"
  SET DATAARR(4)="PATIENT = "_TMGDFN
  SET DATAARR(5)="<VALUES>"
  SET DATAARR(6)="7090^"_CVDRISK_"^^0^0^"_$$TODAY^TMGDATE_"^OTHER^8818^%"
  SET DATAARR(7)="<COMMENTS>"
  SET DATAARR(8)="NOTICE: Lab values entered automatically when CVD Risk was calculated"
  SET DATAARR(9)="        through a TMG TIU PXRM TABLE."
  NEW TMGRESULT 
  DO POSTLABS^TMGRPCL0(.TMGRESULT,.DATAARR)
  IF $P(TMGRESULT(0),"^",1)="-1" DO
  . SET TMGRESULT(0)=$P($G(^DPT(TMGDFN,0)),"^",1)_" - "_$P(TMGRESULT(0),"^",2)
  . DO INFRMALT^TMGXQAL(.TMGRESULT,"150",CVDRISK)
  QUIT
  ;"
RISKTBL(TMGDFN,OUTARRAY)
  QUIT "Target statin tx group = "_$$RISKGRP(TMGDFN)
  ;"
RISKGRP(TMGDFN,AODT,TMGTEXT)  ;
  ;"Input: TMGDFN -- patient IEN
  ;"       AODT -- Optional.  Default is NOW.  AS OF DT for calculation of risk.  
  ;"       TMGTEXT -- PASS BY REFERENCE.  An OUT PARAMETER.  Text to explain finding
  ;"Results: "high", "moderate" "low", "none", "moderate to high", or "individualized"
  NEW ZZDEBUG SET ZZDEBUG=0
  NEW TEMPREF SET TEMPREF=$NAME(^TMG("TEMP","RISKGRP^TMGTIUOD"))
  IF ZZDEBUG=1 DO
  . SET TMGDFN=$GET(@TEMPREF@("TMGDFN")),AODT=$GET(@TEMPREF@("AODT"))  
  ELSE  DO
  . KILL @TEMPREF SET @TEMPREF@("TMGDFN")=TMGDFN,@TEMPREF@("AODT")=$GET(AODT)  
  NEW RSLT 
  SET RSLT(0)="none",RSLT(0.5)="individualized"
  SET RSLT(1)="low",RSLT(2)="moderate",RSLT(3)="high"
  SET RSLT(2.5)=RSLT(2)_" to "_RSLT(3)
  NEW TMGRESULT SET TMGRESULT=RSLT(0)
  SET TMGTEXT=""
  NEW AGE SET AGE=$$AGE^TMGC0QT4(TMGDFN,.AODT)
  NEW NOTHIAGE SET NOTHIAGE=(AGE'>75)
  NEW MIDAGE SET MIDAGE=(AGE>20)&(AGE<40)
  IF MIDAGE SET TMGTEXT="AGE 21-39"  
  ELSE  SET TMGTEXT=$$TFTEXT(NOTHIAGE,"Age<=75","Age>75")
  NEW ASCVD SET ASCVD=$$HASASCVD^TMGC0QT4(TMGDFN,AODT)
  SET TMGTEXT=TMGTEXT_", "_$$TFTEXT(ASCVD,"Clinical ASCVD","No clinical ASCVD")
  IF ASCVD DO  GOTO RGDN
  . IF NOTHIAGE SET TMGRESULT=RSLT(3),TMGTEXT=TMGTEXT_" --> "_RSLT(3) QUIT
  . SET TMGRESULT=RSLT(2),TMGTEXT=TMGTEXT_" --> "_RSLT(2)
  NEW LDL SET LDL=$$LDL(TMGDFN,.AODT)
  SET TMGTEXT=TMGTEXT_", "_$$TFTEXT(LDL<190,"LDL<190","LDL>=190")
  IF LDL'<190 DO  GOTO RGDN
  . SET TMGTEXT=TMGTEXT_" -->"
  . IF NOTHIAGE SET TMGRESULT=RSLT(3),TMGTEXT=TMGTEXT_RSLT(3) QUIT
  . SET TMGRESULT=RSLT(2),TMGTEXT=TMGTEXT_RSLT(2)
  NEW DM SET DM=$$ONDMTX^TMGC0QT4(TMGDFN,.AODT) ;"Is patient on DM treatment as of AODT?
  SET TMGTEXT=TMGTEXT_", "_$$TFTEXT(DM,"Diabetic","Not diabetic")
  IF DM,MIDAGE DO  GOTO RGDN
  . SET TMGRESULT=RSLT(0.5)
  NEW CVDR,TMP SET CVDR=$$CVDR(TMGDFN,.AODT,.TMP)  ;"10 YR CVD RISK with explaination  
  SET TMGTEXT=TMGTEXT_", CVD Risk="_CVDR_"%"
  IF DM,NOTHIAGE DO  GOTO RGDN
  . IF CVDR>7.4 SET TMGRESULT=RSLT(3),TMGTEXT=TMGTEXT_" --> "_RSLT(3) QUIT
  . SET TMGRESULT=RSLT(2),TMGTEXT=TMGTEXT_" --> "_RSLT(2)
  IF CVDR>7.4,NOTHIAGE DO  GOTO RGDN
  . SET TMGRESULT=RSLT(2.5),TMGTEXT=TMGTEXT_" --> "_RSLT(2.5) QUIT
  SET TMGTEXT=TMGTEXT_" --> "_RSLT(0.5)
  SET TMGRESULT=RSLT(0.5)
RGDN ;
  QUIT TMGRESULT
  ;
TFTEXT(B,VALIFTRUE,VALIFFALSE) ;
  QUIT $SELECT($GET(B)=1:VALIFTRUE,1:VALIFFALSE)
  ;
BOOL(V) ;
  QUIT $SELECT(V=1:"T",V=-1:"?",1:"F")
 ;
CVDR10YR(YR,FEMALE,SBP,ONRX,TCH,HDL,SMK,DM2)  ;"10 YR CVD RISK CALCULATOR FROM FRAMINGHAM DATA
  ;"INPUT: YR -- age in years.  Must be >= 20 yrs
  ;"       FEMALE -- 1 if female, 0 if male
  ;"       SBP -- Systolic blood pressure (mmHg)
  ;"       ONRX -- 1 if on Rx for blood pressure (i.e. treated), 0 if not
  ;"       TCH -- Total cholesterol (mg/dl).  Must be at least 50 (added contraint to remove accidental inputs with other units)
  ;"       HDL -- HDL cholesterol (mg/dl)
  ;"       SMK -- 1 if smoker, 0 if not.  
  ;"       DM2 -- 1 if diabetic, 0 if not  --note: NOT USED in this more-up-to-date Framingham calculation.  Not sure why not...
  ;"Results: -1 if invalid input, else number 0-30 as % risk of CVD event in next 10 yrs
  NEW TMGRESULT SET TMGRESULT=-1
  SET YR=+$GET(YR)\1 IF YR<20 GOTO CVDRDN
  SET FEMALE=$GET(FEMALE) IF (FEMALE'=0)&(FEMALE'=1) GOTO CVDRDN
  SET SBP=+$GET(SBP)\1
  SET ONRX=$GET(ONRX) IF (ONRX'=0)&(ONRX'=1) GOTO CVDRDN
  SET TCH=+$GET(TCH)\1 IF TCH<50 GOTO CVDRDN
  SET HDL=+$GET(HDL)\1
  SET SMK=$GET(SMK) IF (SMK'=0)&(SMK'=1) GOTO CVDRDN
  ;"SET DM2=$GET(DM2) IF (DM2'=0)&(DM2'=1) GOTO CVDRDN
  NEW POINTS SET POINTS=0
  SET POINTS=POINTS+$$AGE2PT(YR,FEMALE)
  SET POINTS=POINTS+$$TCH2PT(TCH,YR,FEMALE)
  SET POINTS=POINTS+$$SMOKR2PT(SMK,YR,FEMALE)
  SET POINTS=POINTS+$$HDL2PT(HDL,FEMALE)
  SET POINTS=POINTS+$$SBP2PT(SBP,FEMALE,ONRX)
  SET TMGRESULT=$$PT2RISK(POINTS,FEMALE)
CVDRDN ;
  QUIT TMGRESULT
  ; 
INRANGE(VAL,LOW,HI) ;
  NEW TMGRESULT SET TMGRESULT=0
  QUIT (VAL'<LOW)&(VAL'>HI)
  ;
AGE2PT(YR,FEMALE) ;"
  SET YR=+$GET(YR)\1
  NEW TMGRESULT SET TMGRESULT=0
  IF FEMALE DO
  . IF $$INRANGE(YR,20,34) SET TMGRESULT=-7
  . ELSE  IF $$INRANGE(YR,35,39) SET TMGRESULT=-3
  . ELSE  IF $$INRANGE(YR,40,44) SET TMGRESULT=0
  . ELSE  IF $$INRANGE(YR,45,49) SET TMGRESULT=3
  . ELSE  IF $$INRANGE(YR,50,54) SET TMGRESULT=6
  . ELSE  IF $$INRANGE(YR,55,59) SET TMGRESULT=8
  . ELSE  IF $$INRANGE(YR,60,64) SET TMGRESULT=10
  . ELSE  IF $$INRANGE(YR,65,69) SET TMGRESULT=12
  . ELSE  IF $$INRANGE(YR,70,74) SET TMGRESULT=14
  . ELSE  IF $$INRANGE(YR,75,200) SET TMGRESULT=16
  ELSE  DO  ;"MALE PATIENT
  . IF $$INRANGE(YR,20,34) SET TMGRESULT=-9
  . ELSE  IF $$INRANGE(YR,35,39) SET TMGRESULT=-4
  . ELSE  IF $$INRANGE(YR,40,44) SET TMGRESULT=0
  . ELSE  IF $$INRANGE(YR,45,49) SET TMGRESULT=3
  . ELSE  IF $$INRANGE(YR,50,54) SET TMGRESULT=6
  . ELSE  IF $$INRANGE(YR,55,59) SET TMGRESULT=8
  . ELSE  IF $$INRANGE(YR,60,64) SET TMGRESULT=10
  . ELSE  IF $$INRANGE(YR,65,69) SET TMGRESULT=11
  . ELSE  IF $$INRANGE(YR,70,74) SET TMGRESULT=12
  . ELSE  IF $$INRANGE(YR,75,200) SET TMGRESULT=13
  QUIT TMGRESULT
  ;
HDL2PT(HDL,FEMALE)  ;"HDL CHOLESTEROL TO POINTS.  mg/dl
  SET HDL=+$GET(HDL)\1
  NEW TMGRESULT SET TMGRESULT=0
  IF FEMALE DO
  . IF $$INRANGE(HDL,60,999) SET TMGRESULT=-1
  . ELSE  IF $$INRANGE(HDL,50,59) SET TMGRESULT=0
  . ELSE  IF $$INRANGE(HDL,40,49) SET TMGRESULT=1
  . ELSE  IF $$INRANGE(HDL,0,39) SET TMGRESULT=2
  ELSE  DO  ;"MALE PATIENT
  . IF $$INRANGE(HDL,60,999) SET TMGRESULT=-1
  . ELSE  IF $$INRANGE(HDL,50,59) SET TMGRESULT=0
  . ELSE  IF $$INRANGE(HDL,40,49) SET TMGRESULT=1
  . ELSE  IF $$INRANGE(HDL,0,39) SET TMGRESULT=2
  QUIT TMGRESULT
  ;
TCH2PT(TCH,YR,FEMALE)  ;"TOTAL CHOLESTEROL TO POINTS.  mg/dl  
  SET TCH=+$GET(TCH)\1
  SET YR=+$GET(YR)\1
  NEW TMGRESULT SET TMGRESULT=0
  IF FEMALE DO
  . IF $$INRANGE(YR,20,39) DO
  . . IF $$INRANGE(TCH,0,159) SET TMGRESULT=0
  . . ELSE  IF $$INRANGE(TCH,160,199) SET TMGRESULT=4
  . . ELSE  IF $$INRANGE(TCH,200,239) SET TMGRESULT=8
  . . ELSE  IF $$INRANGE(TCH,240,279) SET TMGRESULT=11
  . . ELSE  IF $$INRANGE(TCH,280,999) SET TMGRESULT=13
  . IF $$INRANGE(YR,40,49) DO
  . . IF $$INRANGE(TCH,0,159) SET TMGRESULT=0
  . . ELSE  IF $$INRANGE(TCH,160,199) SET TMGRESULT=3
  . . ELSE  IF $$INRANGE(TCH,200,239) SET TMGRESULT=6
  . . ELSE  IF $$INRANGE(TCH,240,279) SET TMGRESULT=8
  . . ELSE  IF $$INRANGE(TCH,280,999) SET TMGRESULT=10
  . IF $$INRANGE(YR,50,59) DO
  . . IF $$INRANGE(TCH,0,159) SET TMGRESULT=0
  . . ELSE  IF $$INRANGE(TCH,160,199) SET TMGRESULT=2
  . . ELSE  IF $$INRANGE(TCH,200,239) SET TMGRESULT=4
  . . ELSE  IF $$INRANGE(TCH,240,279) SET TMGRESULT=5
  . . ELSE  IF $$INRANGE(TCH,280,999) SET TMGRESULT=7
  . IF $$INRANGE(YR,60,69) DO
  . . IF $$INRANGE(TCH,0,159) SET TMGRESULT=0
  . . ELSE  IF $$INRANGE(TCH,160,199) SET TMGRESULT=1
  . . ELSE  IF $$INRANGE(TCH,200,239) SET TMGRESULT=2
  . . ELSE  IF $$INRANGE(TCH,240,279) SET TMGRESULT=3
  . . ELSE  IF $$INRANGE(TCH,280,999) SET TMGRESULT=4
  . IF $$INRANGE(YR,70,200) DO
  . . IF $$INRANGE(TCH,0,159) SET TMGRESULT=0
  . . ELSE  IF $$INRANGE(TCH,160,199) SET TMGRESULT=1
  . . ELSE  IF $$INRANGE(TCH,200,239) SET TMGRESULT=1
  . . ELSE  IF $$INRANGE(TCH,240,279) SET TMGRESULT=2
  . . ELSE  IF $$INRANGE(TCH,280,999) SET TMGRESULT=2
  ELSE  DO  ;"MALE PATIENT
  . IF $$INRANGE(YR,20,39) DO
  . . IF $$INRANGE(TCH,0,159) SET TMGRESULT=0
  . . ELSE  IF $$INRANGE(TCH,160,199) SET TMGRESULT=4
  . . ELSE  IF $$INRANGE(TCH,200,239) SET TMGRESULT=7
  . . ELSE  IF $$INRANGE(TCH,240,279) SET TMGRESULT=9
  . . ELSE  IF $$INRANGE(TCH,280,999) SET TMGRESULT=11
  . IF $$INRANGE(YR,40,49) DO                       
  . . IF $$INRANGE(TCH,0,159) SET TMGRESULT=0       
  . . ELSE  IF $$INRANGE(TCH,160,199) SET TMGRESULT=3
  . . ELSE  IF $$INRANGE(TCH,200,239) SET TMGRESULT=5
  . . ELSE  IF $$INRANGE(TCH,240,279) SET TMGRESULT=6
  . . ELSE  IF $$INRANGE(TCH,280,999) SET TMGRESULT=8
  . IF $$INRANGE(YR,50,59) DO                       
  . . IF $$INRANGE(TCH,0,159) SET TMGRESULT=0       
  . . ELSE  IF $$INRANGE(TCH,160,199) SET TMGRESULT=2
  . . ELSE  IF $$INRANGE(TCH,200,239) SET TMGRESULT=3
  . . ELSE  IF $$INRANGE(TCH,240,279) SET TMGRESULT=4
  . . ELSE  IF $$INRANGE(TCH,280,999) SET TMGRESULT=5
  . IF $$INRANGE(YR,60,69) DO                       
  . . IF $$INRANGE(TCH,0,159) SET TMGRESULT=0       
  . . ELSE  IF $$INRANGE(TCH,160,199) SET TMGRESULT=1
  . . ELSE  IF $$INRANGE(TCH,200,239) SET TMGRESULT=1
  . . ELSE  IF $$INRANGE(TCH,240,279) SET TMGRESULT=2
  . . ELSE  IF $$INRANGE(TCH,280,999) SET TMGRESULT=3
  . IF $$INRANGE(YR,70,200) DO                      
  . . IF $$INRANGE(TCH,0,159) SET TMGRESULT=0       
  . . ELSE  IF $$INRANGE(TCH,160,199) SET TMGRESULT=0
  . . ELSE  IF $$INRANGE(TCH,200,239) SET TMGRESULT=0
  . . ELSE  IF $$INRANGE(TCH,240,279) SET TMGRESULT=1
  . . ELSE  IF $$INRANGE(TCH,280,999) SET TMGRESULT=1
  QUIT TMGRESULT
  ;
SBP2PT(SBP,FEMALE,ONRX)  ;"SYSTOLIC BP (mmHg) TO POINTS
  SET SBP=+$GET(SBP)
  NEW TMGRESULT SET TMGRESULT=0
  IF FEMALE DO       
  . IF ONRX DO
  . . IF $$INRANGE(SBP,0,119) SET TMGRESULT=0
  . . ELSE  IF $$INRANGE(SBP,120,129) SET TMGRESULT=3
  . . ELSE  IF $$INRANGE(SBP,130,139) SET TMGRESULT=4
  . . ELSE  IF $$INRANGE(SBP,140,149) SET TMGRESULT=5
  . . ELSE  IF $$INRANGE(SBP,150,159) SET TMGRESULT=5
  . . ELSE  IF $$INRANGE(SBP,160,999) SET TMGRESULT=6
  . ELSE  DO  ;"NOT ON RX
  . . IF $$INRANGE(SBP,0,119) SET TMGRESULT=-3
  . . ELSE  IF $$INRANGE(SBP,120,129) SET TMGRESULT=1
  . . ELSE  IF $$INRANGE(SBP,130,139) SET TMGRESULT=2
  . . ELSE  IF $$INRANGE(SBP,140,149) SET TMGRESULT=3
  . . ELSE  IF $$INRANGE(SBP,150,159) SET TMGRESULT=3
  . . ELSE  IF $$INRANGE(SBP,160,999) SET TMGRESULT=4
  ELSE  DO  ;"MALE PATIENT
  . IF ONRX DO
  . . IF $$INRANGE(SBP,0,119) SET TMGRESULT=0
  . . ELSE  IF $$INRANGE(SBP,120,129) SET TMGRESULT=1
  . . ELSE  IF $$INRANGE(SBP,130,139) SET TMGRESULT=2
  . . ELSE  IF $$INRANGE(SBP,140,159) SET TMGRESULT=2
  . . ELSE  IF $$INRANGE(SBP,160,999) SET TMGRESULT=3
  . ELSE  DO  ;"NOT ON RX
  . . IF $$INRANGE(SBP,0,119) SET TMGRESULT=0
  . . ELSE  IF $$INRANGE(SBP,120,129) SET TMGRESULT=0
  . . ELSE  IF $$INRANGE(SBP,130,139) SET TMGRESULT=1
  . . ELSE  IF $$INRANGE(SBP,140,159) SET TMGRESULT=1
  . . ELSE  IF $$INRANGE(SBP,160,999) SET TMGRESULT=2
  QUIT TMGRESULT
  ;
SMOKR2PT(VAL,YR,FEMALE)  ;"SMOKING STATUS TO POINTS.  VAL=1 IF SMOKER, 0 IF NOT;  YR=AGE
  SET VAL=+$GET(VAL)\1
  SET YR=+$GET(YR)\1
  NEW TMGRESULT SET TMGRESULT=0
  IF FEMALE DO
  . IF $$INRANGE(YR,20,39) DO
  . . IF VAL=1 SET TMGRESULT=9
  . IF $$INRANGE(YR,40,49) DO
  . . IF VAL=1 SET TMGRESULT=7
  . IF $$INRANGE(YR,50,59) DO
  . . IF VAL=1 SET TMGRESULT=4
  . IF $$INRANGE(YR,60,69) DO
  . . IF VAL=1 SET TMGRESULT=2
  . IF $$INRANGE(YR,70,200) DO
  . . IF VAL=1 SET TMGRESULT=1
  ELSE  DO  ;"MALE PATIENT
  . IF $$INRANGE(YR,20,39) DO
  . . IF VAL=1 SET TMGRESULT=8
  . IF $$INRANGE(YR,40,49) DO
  . . IF VAL=1 SET TMGRESULT=5
  . IF $$INRANGE(YR,50,59) DO
  . . IF VAL=1 SET TMGRESULT=3
  . IF $$INRANGE(YR,60,69) DO
  . . IF VAL=1 SET TMGRESULT=1
  . IF $$INRANGE(YR,70,200) DO
  . . IF VAL=1 SET TMGRESULT=1
  QUIT TMGRESULT
  ;
DM2PT(VAL)  ;"DIABETES STATUS TO POINTS.  VAL=1 IF DIABETIC, 0 IF NOT
  ;"NOTE: This table is from older Framingham data.  The more recent tables found don't seem to use this.  
  NEW TMGRESULT SET TMGRESULT=0
  IF FEMALE DO       
  . IF +$GET(VAL)=1 SET TMGRESULT=4
  ELSE  DO  ;"MALE PATIENT
  . IF +$GET(VAL)=1 SET TMGRESULT=3
  QUIT TMGRESULT
  ;
PT2RISK(PTS,FEMALE)  ;"CONVERT POINTS TO CARDIOVASCULAR RISK %
  SET PTS=+$GET(PTS)\1
  NEW TMGRESULT SET TMGRESULT=0
  IF FEMALE DO
  . IF PTS<9 SET TMGRESULT=0.999
  . ELSE  IF PTS=9 SET TMGRESULT=1
  . ELSE  IF PTS=10 SET TMGRESULT=1
  . ELSE  IF PTS=11 SET TMGRESULT=1
  . ELSE  IF PTS=12 SET TMGRESULT=1
  . ELSE  IF PTS=13 SET TMGRESULT=2
  . ELSE  IF PTS=14 SET TMGRESULT=2
  . ELSE  IF PTS=15 SET TMGRESULT=3
  . ELSE  IF PTS=16 SET TMGRESULT=4
  . ELSE  IF PTS=17 SET TMGRESULT=5
  . ELSE  IF PTS=18 SET TMGRESULT=6
  . ELSE  IF PTS=19 SET TMGRESULT=8
  . ELSE  IF PTS=20 SET TMGRESULT=11
  . ELSE  IF PTS=21 SET TMGRESULT=14
  . ELSE  IF PTS=22 SET TMGRESULT=17
  . ELSE  IF PTS=23 SET TMGRESULT=22
  . ELSE  IF PTS=24 SET TMGRESULT=27
  . ELSE  IF PTS>24 SET TMGRESULT=30
  ELSE  DO  ;"MALE PATIENT
  . IF PTS<0 SET TMGRESULT=0.999
  . ELSE  IF PTS=0 SET TMGRESULT=1
  . ELSE  IF PTS=1 SET TMGRESULT=1
  . ELSE  IF PTS=2 SET TMGRESULT=1
  . ELSE  IF PTS=3 SET TMGRESULT=1
  . ELSE  IF PTS=4 SET TMGRESULT=1
  . ELSE  IF PTS=5 SET TMGRESULT=2
  . ELSE  IF PTS=6 SET TMGRESULT=2
  . ELSE  IF PTS=7 SET TMGRESULT=3
  . ELSE  IF PTS=8 SET TMGRESULT=4
  . ELSE  IF PTS=9 SET TMGRESULT=5
  . ELSE  IF PTS=10 SET TMGRESULT=6
  . ELSE  IF PTS=11 SET TMGRESULT=8
  . ELSE  IF PTS=12 SET TMGRESULT=10
  . ELSE  IF PTS=13 SET TMGRESULT=12
  . ELSE  IF PTS=14 SET TMGRESULT=16
  . ELSE  IF PTS=15 SET TMGRESULT=20
  . ELSE  IF PTS=16 SET TMGRESULT=25
  . ELSE  IF PTS>16 SET TMGRESULT=30
  QUIT TMGRESULT                                        
  ;
HDL(TMGDFN,AODT)     ;"Return the last HDL value, prior to date provided
 QUIT $$GETLAB(.TMGDFN,.AODT,"HDL",244)
 ;"
LDL(TMGDFN,AODT)     ;"Return the last LDL value, prior to date provided
 QUIT $$GETLAB(.TMGDFN,.AODT,"LDL CHOLESTEROL",901)
 ;"
TCHOL(TMGDFN,AODT)     ;"Return the last HDL value, prior to date provided
 QUIT $$GETLAB(.TMGDFN,.AODT,"CHOLESTEROL",183) 
 ;"
GETLAB(TMGDFN,AODT,LABNAME,LABNUM)  
 ;"Input: TMGDFN - patient IEN
 ;"       AODT - optional, default is now
 ;"       LABNAME - lab name as defined in LAB DATA
 ;"       LABNUM - IEN of lab test
 ;"RESULT: value of lab, or 0 if not found. 
 NEW TMGRESULT SET TMGRESULT=0
 SET TMGDFN=+$GET(TMGDFN)
 SET AODT=+$GET(AODT) IF AODT'>0 SET AODT=$$NOW^XLFDT
 NEW LABARR DO GETPLABS^TMGLRR01(TMGDFN_"^2",LABNAME,.LABARR)
 NEW LABSTR SET LABSTR=LABNUM_"^"_LABNAME
 NEW LASTDT SET LASTDT=$ORDER(LABARR(LABSTR,AODT),-1)
 SET TMGRESULT=+$GET(LABARR(LABSTR,LASTDT))
 QUIT TMGRESULT
 ;"  
TESTTITL(YR,FEMALE,ONRX,TCH,HDL,SMK)  ;
  WRITE YR," old ",$SELECT(FEMALE:"Female",1:"Male")," "
  WRITE $SELECT(SMK:"Smoker",1:"Non-smoker")," "
  WRITE "T-chol=",TCH,", HDL=",HDL,", "
  WRITE $SELECT(ONRX:"On HTN treatment",1:"NOT on HTN treatment")," "
  QUIT;
  ;
TIUCVDR(TMGDFN) ;
  QUIT "CVD RISK: "_$$PTCVDRSK(TMGDFN)_"% in next 10 yrs"
  ;
CVDTABLE(TMGDFN,OUTARRAY)  ;
  NEW TMGRESULT
  SET TMGRESULT=$$PTCVDRSK(+$GET(TMGDFN))
  IF +TMGRESULT'>0 DO
  . SET TMGRESULT="UNABLE TO CALCULATE"
  ELSE  DO
  . SET TMGRESULT=TMGRESULT_"% in next 10 years"
  QUIT "CVD Risk = "_TMGRESULT
  ;"
  ;"=====================================================================
INRXTAXN(IEN22733,IEN22733D3)  ;"IS MEDICATION IN RX TAXONOMY?  
  NEW RESULT SET RESULT=($DATA(^TMG(22733.3,IEN22733D3,"RX","B",IEN22733)))
  QUIT RESULT
  ;
STATNINT(TMGDFN)  ;"STATIN INTENSITY CLASS INFORMATION
  NEW RESULT SET RESULT=""
  ;" NEW STATINCLASS SET STATINCLASS="CV350"  ;"ANTILIPEMIC AGENTS
  ;" ;"NOTE: This class has more than statins, e.g. ezitamide, fenofibrates)
  ;" NEW CLASSIEN SET CLASSIEN=+$ORDER(^PS(50.605,"B",STATINCLASS,0))
  ;" IF CLASSIEN'>0 GOTO STNSTDN   
  NEW IEN22733D3 SET IEN22733D3=+$ORDER(^TMG(22733.3,"B","STATINS",0))  
  NEW SUBIEN SET SUBIEN=0
  ;"FOR  SET SUBIEN=$ORDER(^TMG(22733.2,TMGDFN,"RX","ACLASS",CLASSIEN,SUBIEN)) QUIT:(SUBIEN'>0)!(RESULT'="")  DO
  FOR  SET SUBIEN=$ORDER(^TMG(22733.2,TMGDFN,"RX",SUBIEN)) QUIT:(SUBIEN'>0)!(RESULT'="")  DO
  . NEW ZN SET ZN=$GET(^TMG(22733.2,TMGDFN,"RX",SUBIEN,0))  ;"e.g.  26^atorvastatin 40 mg po QDAY^40^58
  . NEW IEN22733 SET IEN22733=+ZN
  . IF '$$INRXTAXN(IEN22733,IEN22733D3) QUIT  ;"IN RX TAXONOMY?
  . NEW STRENGTH SET STRENGTH=$PIECE(ZN,"^",3) IF STRENGTH'>0 QUIT
  . NEW RXNAME SET RXNAME=$PIECE($GET(^TMG(22733,IEN22733,0)),"^",1)
  . NEW GROUP,DESCR SET GROUP="",DESCR=""
  . IF RXNAME="ATORVASTATIN" DO
  . . SET DESCR="atorvastatin "
  . . IF STRENGTH<10 SET GROUP="LOW",DESCR=DESCR_"< 10"
  . . ELSE  IF STRENGTH<40 SET GROUP="MODERATE",DESCR=DESCR_"< 40"
  . . ELSE  SET GROUP="HIGH",DESCR=DESCR_">= 40"
  . IF RXNAME="LOVASTATIN" DO 
  . . SET DESCR="lovastatin "
  . . IF STRENGTH<40 SET GROUP="LOW",DESCR=DESCR_"< 40"
  . . ELSE  SET GROUP="MODERATE",DESCR=DESCR_">= 40"
  . IF RXNAME="PITAVASTATIN" DO
  . . SET DESCR="pitavastatin "
  . . IF STRENGTH=1 SET GROUP="LOW",DESCR=DESCR_"1"
  . . ELSE  SET GROUP="MODERATE",DESCR=DESCR_"> 1"
  . IF RXNAME="PRAVASTATIN" DO
  . . SET DESCR="pravastatin "
  . . IF STRENGTH<40 SET GROUP="LOW",DESCR=DESCR_"<40"
  . . ELSE  SET GROUP="MODERATE",DESCR=DESCR_">= 40"
  . IF RXNAME="ROSUVASTATIN" DO
  . . SET DESCR="rosuvastatin "
  . . IF STRENGTH<20 SET GROUP="MODERATE",DESCR=DESCR_"< 20"
  . . ELSE  SET GROUP="HIGH",DESCR=DESCR_">= 20"
  . IF RXNAME="SIMVASTATIN" DO
  . . SET DESCR="simvastatin "
  . . IF STRENGTH<20 SET GROUP="LOW",DESCR=DESCR_"< 20"
  . . IF STRENGTH<80 SET GROUP="MODERATE",DESCR=DESCR_"20-40"
  . . ELSE  SET GROUP="HIGH",DESCR=DESCR_"80"
  . IF RXNAME="FLUVASTATIN" DO
  . . SET DESCR="fluvastatin "  
  . . IF STRENGTH<40 SET GROUP="LOW",DESCR=DESCR_"< 40"
  . . ELSE  SET GROUP="MODERATE",DESCR=DESCR_">= 40"
  . IF GROUP="" QUIT
  . SET RESULT=GROUP_" ("_DESCR_" mg daily)"  
STNSTDN ;  
  IF RESULT="" SET RESULT="N/A"
  QUIT RESULT
  ;
  ;"=====================================================================
TEST  ;
  NEW YR,FEMALE,SBP,ONRX,TCH,HDL,SMK
  SET TCH=240
  SET HDL=40
  FOR YR=50:10:80 DO
  . FOR FEMALE=0:1:1 DO
  . . FOR SMK=0:1:1 DO
  . . . FOR ONRX=0:1:1 DO
  . . . . DO TESTTITL(YR,FEMALE,ONRX,TCH,HDL,SMK) WRITE !
  . . . . WRITE "SBP=" SET X=2
  . . . . FOR SBP=100:20:200 DO
  . . . . . SET X=X+5
  . . . . . WRITE ?X,SBP
  . . . . WRITE !,"RISK=" SET X=2
  . . . . FOR SBP=100:20:200 DO
  . . . . . SET X=X+5
  . . . . . WRITE ?X,$$CVDR10YR(YR,FEMALE,SBP,ONRX,TCH,HDL,SMK),"%"
  . . . . WRITE !,!
  QUIT
  ;
TESTPTS ;
  NEW NAME SET NAME=""
  FOR  SET NAME=$ORDER(^DPT("B",NAME)) QUIT:NAME=""  DO
  . NEW ADFN SET ADFN=0
  . FOR  SET ADFN=$ORDER(^DPT("B",NAME,ADFN)) QUIT:ADFN'>0  DO
  . . NEW CVDR SET CVDR=$$PTCVDRSK(ADFN)
  . . IF CVDR=-1 QUIT  WRITE "." QUIT
  . . WRITE !,NAME,?25," --> ",CVDR,"% risk in next 10 yrs"
  QUIT
  ;
TEST1GRP ;
  SET (X,Y)="",DIC=2,DIC(0)="MAEQ"
  DO ^DIC WRITE !
  IF +Y'>0 QUIT
  NEW TMGDFN SET TMGDFN=+Y
  NEW GROUP,TEXT SET GROUP=$$RISKGRP^TMGTIUOD(TMGDFN,,.TEXT)
  WRITE !,GROUP,!,TEXT,!
  QUIT 
  ;
TESTGRP(ARR) ;
  NEW MAXDFN SET MAXDFN=+$ORDER(^DPT("@"),-1)
  NEW STARTH SET STARTH=$H
  NEW CT SET CT=1
  NEW TMGDFN SET TMGDFN=0
  FOR  SET TMGDFN=$ORDER(^DPT(TMGDFN)) QUIT:+TMGDFN'>0  DO
  . NEW TMGTEXT
  . NEW NAME SET NAME=$P(^DPT(TMGDFN,0),"^",1)
  . NEW GROUP SET GROUP=$$RISKGRP^TMGTIUOD(TMGDFN,,.TMGTEXT)
  . SET ARR("G",GROUP,NAME,TMGTEXT)=""
  . SET ARR("N",NAME)=TMGTEXT
  . SET CT=CT+1 
  . IF CT#10'=0 QUIT
  . SET CT=1
  . DO PROGBAR^TMGUSRI2(TMGDFN,$$LJ^XLFSTR(NAME,20),0,MAXDFN,80,STARTH)
  ZWR ARR
  QUIT
  ;
TESTSTN() ;
  NEW RESULT SET RESULT=""
  SET (X,Y)="",DIC=2,DIC(0)="MAEQ"
  DO ^DIC WRITE !
  IF +Y'>0 GOTO TSTDN
  NEW TMGDFN SET TMGDFN=+Y  
  SET RESULT=$$STATNINT(TMGDFN)
TSTDN ;  
  QUIT RESULT  
  ;"
URINCULT(TMGDFN,OUTARRAY)  ;"
  NEW TMGRESULT SET TMGRESULT="-- [URINE CULTURE] ---------"
  NEW LABARR,LINE
  NEW COUNT,MAX
  SET COUNT=0,MAX=5
  DO GETLABS^TMGLRR02(.LABARR,TMGDFN,$$ADDDAYS^TMGDATE("-1825"),$$TODAY^TMGDATE,.OPTION)
  NEW LABDATE SET LABDATE=9999999
  NEW LABNAMES SET LABNAMES("URINE CULTURE")="",LABNAMES("CULTURE RESULT")=""
  NEW LABNAME SET LABNAME=""
  NEW TEMPARR ;"USED TO SET BY DATE WHEN 
  FOR  SET LABNAME=$O(LABNAMES(LABNAME)) QUIT:LABNAME=""  DO
  . FOR  SET LABDATE=$O(LABARR("TEST","NAME",LABNAME,LABDATE),-1) QUIT:(LABDATE'>0)!(COUNT>MAX)  DO
  . . SET COUNT=COUNT+1
  . . NEW EDATE,RESULT,THISARR
  . . SET EDATE=$P($$EXTDATE^TMGDATE(LABDATE),"@",1)
  . . ;"SET RESULT=$G(LABARR("DT",LABDATE,5118))
  . . MERGE THISARR=LABARR("DT",LABDATE)
  . . DO CULTRSLT(.THISARR,.RESULT)
  . . SET LINE="   "_EDATE_" = "_RESULT
  . . SET TEMPARR($$INTDATE^TMGDATE(EDATE),EDATE,RESULT)=LINE
  NEW IDATE SET IDATE=9999999
  FOR  SET IDATE=$O(TEMPARR(IDATE),-1) QUIT:IDATE'>0  DO
  . SET EDATE=""
  . FOR  SET EDATE=$O(TEMPARR(IDATE,EDATE)) QUIT:EDATE=""  DO
  . . SET RESULT=""
  . . FOR  SET RESULT=$O(TEMPARR(IDATE,EDATE,RESULT)) QUIT:RESULT=""  DO
  . . . SET LINE=$G(TEMPARR(IDATE,EDATE,RESULT))
  . . . SET OUTARRAY("KEY-VALUE",EDATE)=RESULT
  . . . SET OUTARRAY("KEY-VALUE",EDATE,"LINE")=LINE
  . . . IF TMGRESULT'="" SET TMGRESULT=TMGRESULT_$C(13)
  . . . SET TMGRESULT=TMGRESULT_LINE
  QUIT TMGRESULT
  ;"  
URINCULT2(TMGDFN,OUTARRAY)  ;"
  NEW TMGRESULT SET TMGRESULT=""
  NEW LABARR
  NEW COUNT,MAX
  SET COUNT=0,MAX=5
  DO GETLABS^TMGLRR02(.LABARR,TMGDFN,$$ADDDAYS^TMGDATE("-1825"),$$TODAY^TMGDATE,.OPTION)
  NEW LABDATE SET LABDATE=9999999
  NEW LABNAMES SET LABNAMES("URINE CULTURE")="",LABNAMES("CULTURE RESULT")=""
  NEW LABNAME SET LABNAME=""
  FOR  SET LABNAME=$O(LABNAMES(LABNAME)) QUIT:LABNAME=""  DO
  . FOR  SET LABDATE=$O(LABARR("TEST","NAME",LABNAME,LABDATE),-1) QUIT:(LABDATE'>0)!(COUNT>MAX)  DO
  . . SET COUNT=COUNT+1
  . . NEW EDATE,RESULT,THISARR
  . . SET EDATE=$P($$EXTDATE^TMGDATE(LABDATE),"@",1)
  . . ;"SET RESULT=$G(LABARR("DT",LABDATE,5118))
  . . MERGE THISARR=LABARR("DT",LABDATE)
  . . DO CULTRSLT(.THISARR,.RESULT)
  . . NEW LINE SET LINE=EDATE_" = "_RESULT
  . . SET OUTARRAY("KEY-VALUE",EDATE)=RESULT
  . . SET OUTARRAY("KEY-VALUE",EDATE,"LINE")=LINE
  . . IF TMGRESULT'="" SET TMGRESULT=TMGRESULT_$C(13)
  . . SET TMGRESULT=TMGRESULT_LINE
  QUIT TMGRESULT
  ;"
CULTRSLT(ARRAY,LINE) 
  zwr ARRAY
  NEW COMMENT,IDX
  SET IDX=0,COMMENT="",LINE=""
  IF $D(ARRAY(5122)) DO  ;"SENSITIVITY DONE
  . IF $D(ARRAY(5121)) DO  ;"E-COLI
  . . SET LINE="E.Coli    "_$P($G(ARRAY(5121)),"^",2)
  . ELSE  IF $D(ARRAY(5125)) DO  ;"E-FAECALIS
  . . SET LINE="E.Faecalis    "_$P($G(ARRAY(5125)),"^",2)
  . ELSE  IF $D(ARRAY(5139)) DO  ;"KLEBSIELLA Pneumoniae
  . . SET LINE="K. Pneumoniae    "_$P($G(ARRAY(5139)),"^",2)
  . ELSE  DO
  . . SET LINE="Sensitivity done. Table is unable to determine result."
  ELSE  DO
  . IF $D(ARRAY(5540)) DO
  . . IF $$UP^XLFSTR($G(ARRAY(5540)))["NO GROWTH" SET LINE="NO GROWTH"
  . FOR  SET IDX=$O(ARRAY("COMMENT",IDX)) QUIT:(IDX'>0)!(LINE'="")  DO
  . . SET COMMENT=$$UP^XLFSTR($G(ARRAY("COMMENT",IDX)))
  . . IF COMMENT["NO GROWTH" SET LINE="No growth"
  . . ELSE  IF COMMENT["ORGANISM" SET LINE=COMMENT
  . IF LINE="" SET LINE=$P($G(ARRAY(5118)),"^",2)
  IF LINE="" SET LINE="TABLE ERROR DETERMINING RESULT"
  QUIT 
  ;"
SMEARRLT(TMGDFN,OUTARRAY)  ;"
  NEW TMGRESULT SET TMGRESULT=""
  NEW LABARR
  NEW COUNT,MAX
  SET COUNT=0,MAX=3
  DO GETLABS^TMGLRR02(.LABARR,TMGDFN,$$ADDDAYS^TMGDATE("-1825"),$$TODAY^TMGDATE,.OPTION)
  NEW LABDATE SET LABDATE=9999999
  FOR  SET LABDATE=$O(LABARR("TEST","NAME","SLIDE REVIEW BY PATHOLOGIST",LABDATE),-1) QUIT:(LABDATE'>0)!(COUNT>MAX)  DO
  . SET COUNT=COUNT+1
  . NEW EDATE,RESULT,THISARR
  . SET EDATE=$$EXTDATE^TMGDATE(LABDATE)
  . ;"SET RESULT=$G(LABARR("DT",LABDATE,5118))
  . MERGE THISARR=LABARR("DT",LABDATE)
  . NEW IDX,FOUND SET (IDX,FOUND)=0
  . SET RESULT=""
  . FOR  SET IDX=$O(LABARR("DT",LABDATE,"COMMENT",IDX)) QUIT:IDX'>0  DO
  . . NEW COMMENT SET COMMENT=$$UP^XLFSTR($G(LABARR("DT",LABDATE,"COMMENT",IDX)))
  . . IF COMMENT["COMMENT FOR" SET FOUND=1 QUIT
  . . IF FOUND=0 QUIT
  . . IF COMMENT["====" QUIT
  . . IF RESULT'="" SET RESULT=RESULT_$C(13)_"|                       "
  . . SET RESULT=RESULT_COMMENT
  . NEW LINE SET LINE=EDATE_" = "_RESULT
  . SET OUTARRAY("KEY-VALUE",EDATE)=RESULT
  . SET OUTARRAY("KEY-VALUE",EDATE,"LINE")=LINE
  . IF TMGRESULT'="" SET TMGRESULT=TMGRESULT_$C(13)
  . SET TMGRESULT=TMGRESULT_LINE
  QUIT TMGRESULT
  ;"
URINCYTO(TMGDFN,OUTARRAY)  ;"
  NEW TESTIEN SET TESTIEN=5112
  NEW LABARR,OPTION,DATEARR,RESULT,DATE
  SET DATE=0,RESULT=""
  DO GETLABS^TMGLRR02(.LABARR,TMGDFN,$$ADDDAYS^TMGDATE("-1825"),$$TODAY^TMGDATE,.OPTION)
  ;"SOME SURGICAL PATHS ARE URINE CYTOLOGY AND SOME ARE NOT. BEGIN BY GETTING ALL SURGICAL PATHS
  FOR  SET DATE=$O(LABARR("TEST","IEN60",TESTIEN,DATE)) QUIT:DATE=""  DO
  . SET DATEARR(DATE)=""  
  SET DATE=9999999.9999
  ;"NOW REVIEW ALL TO SEE WHICH ONES ARE URINE CYTOLOGY
  FOR  SET DATE=$O(DATEARR(DATE),-1) QUIT:DATE=""  DO
  . NEW FOUND,COMMENTLINE,IDX
  . SET (FOUND,IDX)=0
  . FOR  SET IDX=$O(LABARR("DT",DATE,"COMMENT",IDX)) QUIT:(IDX="")!(FOUND=1)  DO
  . . SET COMMENTLINE=$G(LABARR("DT",DATE,"COMMENT",IDX))
  . . IF $$UP^XLFSTR(COMMENTLINE)["UROLOGY CYTOLOGY" DO
  . . . SET FOUND=1
  . . . IF RESULT'="" SET RESULT=RESULT_","
  . . . SET RESULT=RESULT_$$EXTDATE^TMGDATE(DATE,1)
  IF RESULT'="" SET RESULT="URINE CYTOLOGY="_RESULT
  QUIT RESULT
  ;"
ANACOMNT(TMGDFN,OUTARRAY)  ;"ANA COMMENT
  NEW TESTIEN SET TESTIEN=5884
  NEW LABARR,OPTION,DATEARR,RESULT,DATE
  SET DATE=0,RESULT=""
  DO GETLABS^TMGLRR02(.LABARR,TMGDFN,$$ADDDAYS^TMGDATE("-1825"),$$TODAY^TMGDATE,.OPTION)
  ;"SOME SURGICAL PATHS ARE URINE CYTOLOGY AND SOME ARE NOT. BEGIN BY GETTING ALL SURGICAL PATHS
  FOR  SET DATE=$O(LABARR("TEST","IEN60",TESTIEN,DATE)) QUIT:DATE=""  DO
  . SET DATEARR(DATE)=""  
  SET DATE=9999999.9999
  ;"NOW REVIEW ALL TO SEE WHICH ONES ARE URINE CYTOLOGY
  FOR  SET DATE=$O(DATEARR(DATE),-1) QUIT:DATE=""  DO
  . NEW FOUND,COMMENTLINE,IDX
  . NEW ANAFOUND,TOTALCOMMENT
  . SET (FOUND,IDX,ANAFOUND)=0
  . SET TOTALCOMMENT=""
  . FOR  SET IDX=$O(LABARR("DT",DATE,"COMMENT",IDX)) QUIT:(IDX="")!(FOUND=1)  DO
  . . SET COMMENTLINE=$G(LABARR("DT",DATE,"COMMENT",IDX))
  . . IF ANAFOUND=1 DO
  . . . SET TOTALCOMMENT=TOTALCOMMENT_COMMENTLINE
  . . . IF $$UP^XLFSTR(COMMENTLINE)["PATTERN" DO
  . . . . SET FOUND=1
  . . . . SET RESULT=TOTALCOMMENT
  . . . . SET RESULT=RESULT_" ("_$$EXTDATE^TMGDATE(DATE,1)_")"
  . . IF ($$UP^XLFSTR(COMMENTLINE)["COMMENT")&($$UP^XLFSTR(COMMENTLINE)["ANA") SET ANAFOUND=1
  IF RESULT'="" SET RESULT="ANA PATTERN="_RESULT
  QUIT RESULT
  ;"  