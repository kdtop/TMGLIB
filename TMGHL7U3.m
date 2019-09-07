TMGHL7U3 ;TMG/kst-HL7 utility functions ; 12/11/17
              ;;1.0;TMG-LIB;**1**;12/11/17
 ;
 ;"TMG HL7 UTILITY FUNCTIONS 
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 12/11/17  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;                                           
 ;"=======================================================================
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"HL72FMDT(DATETIME) --Convert HL7 date time format into Fileman/Timson date.
 ;"FMDT2HL7(FMDT) -- Convert Fileman/Timson date into HL7 date time    
 ;"GETNAME(TMGHL7MSG) ;"GET PATIENT NAME FROM TMGHL7MSG ARRAY
 ;"GETDOB(TMGHL7MSG) ;"GET PATIENT DOB FROM TMGHL7MSG ARRAY
 ;"GETNMDOB(TMGHL7MSG) ;"GET PATIENT NAME AND DOB FROM TMGHL7MSG ARRAY
 ;              
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"=======================================================================
 ;
 ;"FYI -- $$FMDT2RDT and $$RDT2FMDT are in TMGLRWU1
 ; 
HL72FMDT(HL7DT) ;
  ;"Purpose: Convert HL7 date time format into Fileman/Timson date.
  ;"Input: HL7DT -- Expected format YYYYMMDD[HH[MM[SS]]]
  NEW TMGRESULT SET TMGRESULT=""
  SET HL7DT=$GET(HL7DT)
  IF HL7DT="" GOTO H2FMDN
  NEW YR SET YR=$EXTRACT(HL7DT,1,4)
  SET YR=YR-1700                                    
  NEW MNTH SET MNTH=$EXTRACT(HL7DT,5,6)
  NEW DAY SET DAY=$EXTRACT(HL7DT,7,8)
  NEW TIME,HR,MIN,SEC SET (HR,MIN,SEC)=""
  SET HR=$EXTRACT(HL7DT,9,10)
  SET MIN=$EXTRACT(HL7DT,11,12)
  SET SEC=$EXTRACT(HL7DT,13,14)
  IF $$SUPPTIME^TMGHL7U2()=1 DO  ;"ELH 2/10/15, //kt mod 12/11/16
  . IF HR'="" SET HR=00
  . IF MIN'="" SET MIN=00  
  . IF SEC'="" SET SEC=00
  SET TIME=HR_MIN_SEC IF TIME'="" SET TIME="."_TIME
  SET TMGRESULT=YR_MNTH_DAY_TIME
H2FMDN ;
  QUIT TMGRESULT
  ;
FMDT2HL7(FMDT)    ;
  ;"Purpose: Convert Fileman/Timson date into HL7 date time 
  ;"Input: FMDT -- Fileman DT  Format: YYYMMDD.HHMMSS
  ;"Output: HL7 DATETIME -- format YYYYMMDD[HH[MM[SS]]]
  NEW TMGRESULT SET TMGRESULT=""
  SET FMDT=$GET(FMDT)
  IF FMDT="" GOTO FM2HDN
  NEW YR SET YR=$EXTRACT(FMDT,1,3)+1700
  NEW MNTH SET MNTH=$EXTRACT(FMDT,4,5)
  NEW DAY SET DAY=$EXTRACT(FMDT,6,7)
  NEW HR,MIN,SEC SET (MIN,SEC)=""
  SET HR=$EXTRACT(FMDT,9,10)
  IF HR'="" DO
  . SET HR=$$RJ^XLFSTR(HR,2,0)
  . SET MIN=$EXTRACT(FMDT,11,12)
  . IF MIN'="" DO
  . . SET MIN=$$RJ^XLFSTR(MIN,2,0)
  . . SET SEC=$EXTRACT(FMDT,13,14)
  . . IF SEC'="" SET SEC=$$RJ^XLFSTR(SEC,2,0)
  SET TMGRESULT=YR_MNTH_DAY_HR_MIN_SEC
FM2HDN  ;
  QUIT TMGRESULT
  ;
GETNAME(TMGHL7MSG) ;"GET PATIENT NAME FROM TMGHL7MSG ARRAY
  NEW PIDIDX SET PIDIDX=+$ORDER(TMGHL7MSG("B","PID",0))
  NEW LNAME SET LNAME=$GET(TMGHL7MSG(PIDIDX,5,1))
  NEW FNAME SET FNAME=$GET(TMGHL7MSG(PIDIDX,5,2))
  NEW MNAME SET MNAME=$GET(TMGHL7MSG(PIDIDX,5,3))
  NEW NAME SET NAME=LNAME
  IF FNAME'="" SET NAME=NAME_","_FNAME 
  IF MNAME'="" SET NAME=NAME_" "_MNAME
  QUIT NAME
  ;
GETDOB(TMGHL7MSG) ;"GET PATIENT DOB FROM TMGHL7MSG ARRAY
  NEW PIDIDX SET PIDIDX=+$ORDER(TMGHL7MSG("B","PID",0))
  NEW DOB SET DOB=$GET(TMGHL7MSG(PIDIDX,7))
  SET DOB=$$HL72FMDT^TMGHL7U3(DOB)
  SET DOB=$$FMTE^XLFDT(DOB,"5DZ") ;"//kt added Z 5/2/19
  QUIT DOB
  ;
GETNMDOB(TMGHL7MSG) ;"GET PATIENT NAME AND DOB FROM TMGHL7MSG ARRAY
  NEW RESULT SET RESULT=$$GETNAME(.TMGHL7MSG)
  NEW DOB SET DOB=$$GETDOB(.TMGHL7MSG)
  IF DOB'="" SET RESULT=RESULT_" ("_$$GETDOB(.TMGHL7MSG)_")"
  QUIT RESULT
  ;  
