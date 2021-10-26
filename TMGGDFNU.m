TMGGDFNU  ;TMG/kst-Get Patient info -- UTILITIES ;3/24/21, 4/26/21
                ;;1.0;TMG-LIB;**1**;06/04/08;Build 7
  ;
  ;"TMG GET DFN (TMGGDFN)
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
  ;"=======================================================================
  ;" API -- Public Functions.
  ;"=======================================================================
  ;"FNAME(TMGDFN) --Get a patient's first name from NAME COMPONENTS file
  ;"LNAME(TMGDFN) --Get a patient's last name from NAME COMPONENTS file
  ;"PVFNAME(PROVIEN) --Get a provider's first name from NAME COMPONENTS file
  ;"PVLNAME(PROVIEN) --Get a provider's last name from NAME COMPONENTS file
  ;"VALIDSSN(NUM)  -- Is NUM a valid SSN?
  ;
  ;"=======================================================================
  ;"PRIVATE API FUNCTIONS
  ;"=======================================================================
  ;"GETZN(TMGDFN) ;
  ;"GETPVZN(PROVIEN) ;
  ;
  ;"=======================================================================
  ;"PRIVATE FUNCTIONS
  ;"=======================================================================
  ;
GETZN(TMGDFN) ;
  NEW RESULT SET RESULT=""
  SET TMGDFN=+$GET(TMGDFN)
  NEW NCPTR SET NCPTR=+$PIECE($GET(^DPT(TMGDFN,"NAME")),"^",1)
  IF NCPTR>0 SET RESULT=$GET(^VA(20,NCPTR,1))
  QUIT RESULT
  ;
FNAME(TMGDFN) ;"Get a patient's first name from NAME COMPONENTS file
  NEW ZN SET ZN=$$GETZN(.TMGDFN)
  QUIT $PIECE(ZN,"^",2)
  ;
LNAME(TMGDFN) ;"Get a patient's last name from NAME COMPONENTS file
  NEW ZN SET ZN=$$GETZN(.TMGDFN)
  QUIT $PIECE(ZN,"^",1)
  ;
GETPVZN(PROVIEN) ;
  NEW RESULT SET RESULT=""
  SET PROVIEN=+$GET(PROVIEN)
  NEW NCPTR SET NCPTR=+$PIECE($GET(^VA(200,PROVIEN,3.1)),"^",1)
  IF NCPTR>0 SET RESULT=$GET(^VA(20,NCPTR,1))
  QUIT RESULT
  ;
PVFNAME(PROVIEN) ;"Get a provider's first name from NAME COMPONENTS file
  NEW ZN SET ZN=$$GETPVZN(.PROVIEN)
  QUIT $PIECE(ZN,"^",2)
  ;
PVLNAME(PROVIEN) ;"Get a provider's last name from NAME COMPONENTS file
  NEW ZN SET ZN=$$GETPVZN(.PROVIEN)
  QUIT $PIECE(ZN,"^",1)
  ;  
VALIDSSN(NUM)  ;"Is NUM a valid SSN?
  ;"NOTE: These tests can be improved over time.
  ;"  Sources if info:
  ;"     https://eservices.paychex.com/secure/HRO_PNG/ssn_itin_fed_id_other.html
  ;"     https://primepay.com/blog/how-determine-valid-social-security-number
  ;"Any '-' will be removed and ignored.
  ;"Result: 1^OK, or -1^Msg if problem
  NEW TMGRESULT SET TMGRESULT="1^OK"
  NEW TEMP SET TEMP=$TRANSLATE(NUM,"-","")
  IF $LENGTH(TEMP)'=9 DO  GOTO VSSDN
  . SET TMGRESULT="-1^Improper length.  Got ["_NUM_"]"
  IF (TEMP="123456789")!(TEMP="999999999") DO  GOTO VSSDN
  . SET TMGRESULT="-1^Invalid number. Got ["_NUM_"]"  
  IF (TEMP="111111111")!(TEMP="333333333") DO  GOTO VSSDN
  . SET TMGRESULT="-1^Invalid number. Got ["_NUM_"]"  
  NEW G1 SET G1=$EXTRACT(TEMP,1,3)
  IF "^000^666^900^"[("^"_G1_"^") DO  GOTO VSSDN
  . SET TMGRESULT="-1^Invalid first 3 digits.  Got ["_NUM_"]"
  NEW G2 SET G2=$EXTRACT(TEMP,4,5)
  IF G2="00" DO  GOTO VSSDN
  . SET TMGRESULT="-1^Invalid middle 2 digits.  Got ["_NUM_"]"
  NEW G3 SET G3=$EXTRACT(TEMP,6,9)
  IF G3="0000" DO  GOTO VSSDN
  . SET TMGRESULT="-1^Invalid last 4 digits.  Got ["_NUM_"]"
VSSDN  QUIT TMGRESULT
  ;
  
