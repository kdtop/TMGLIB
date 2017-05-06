TMGGDFNU  ;TMG/kst-Get Patient info -- UTILITIES ;12/19/14
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
  ;"FNAME(DFN) --Get a patient's first name from NAME COMPONENTS file
  ;"LNAME(DFN) --Get a patient's last name from NAME COMPONENTS file
  ;"PVFNAME(PROVIEN) --Get a provider's first name from NAME COMPONENTS file
  ;"PVLNAME(PROVIEN) --Get a provider's last name from NAME COMPONENTS file
  ;
  ;"=======================================================================
  ;"PRIVATE API FUNCTIONS
  ;"=======================================================================
  ;"GETZN(DFN) ;
  ;"GETPVZN(PROVIEN) ;
  ;
  ;"=======================================================================
  ;"PRIVATE FUNCTIONS
  ;"=======================================================================
  ;
GETZN(DFN) ;
  NEW RESULT SET RESULT=""
  SET DFN=+$GET(DFN)
  NEW NCPTR SET NCPTR=+$PIECE($GET(^DPT(DFN,"NAME")),"^",1)
  IF NCPTR>0 SET RESULT=$GET(^VA(20,NCPTR,1))
  QUIT RESULT
  ;
FNAME(DFN) ;"Get a patient's first name from NAME COMPONENTS file
  NEW ZN SET ZN=$$GETZN(.DFN)
  QUIT $PIECE(ZN,"^",2)
  ;
LNAME(DFN) ;"Get a patient's last name from NAME COMPONENTS file
  NEW ZN SET ZN=$$GETZN(.DFN)
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