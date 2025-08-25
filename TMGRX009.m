TMGRX009 ;TMG/kst/Patient medication listing code; 8/6/25
       ;;1.0;TMG-LIB;**1**;08/06/25
 ;
 ;"Code for dealing with saving patients medication list
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 8/6/25  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;" 
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"=======================================================================
 ;"Uses:  
 ;"=======================================================================
 ; 
MAKEALERT  ;
  ;"Entry point for Taskman event
  N XQA,XQAMSG,XQAROU,XQADATA
  S XQA(168)=""
  S XQAMSG="WEEKLY MEDICATION REVIEW IS DUE"
  S XQAROU="HNDLALERT^TMGRX009"
  D SETUP^XQALERT
  QUIT
  ;
HNDLALERT  ;
  ;"setup list for recent and upcoming patients.
  NEW MASTERREF DO GETREFS^TMGRX002(.MASTERREF)
  NEW SDT,EDT
  SET SDT=$$ADDDAYS^TMGDATE("-7")
  SET EDT=$$ADDDAYS^TMGDATE("7")
  NEW OPT SET OPT("EXCLUDE INACTIVE")=1,OPT("CHECK APPT")=1  
  KILL @MASTERREF
  DO CUSTLIST^TMGRX002(MASTERREF,SDT,EDT,.OPT)
  DO CONSOLE^TMGRX008
  QUIT