TMGTIUO9 ;TMG/kst-TIU OBJECTS ; 11/15/14
         ;;1.0;TMG-LIB;**1,17**;11/14/14
 ;"
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
 ;"=======================================================================
 ;"PUBLIC FUNCTIONS
 ;"=======================================================================
 ;"$$GETFUDTS(TMGDFN) --Return info about followup
 ;"$$GETAPPTS(TMGDFN,ASOFDT) --Return information about upcoming appts.
 ;"
 ;"=======================================================================
 ;"PRIVATE FUNCTIONS
 ;"======================================================================= 
 ;"TEST1 ;
 ;"TEST2 ;
 ;"=======================================================================
 ;"Dependancies : 
 ;"=======================================================================
TEST1 ;
  NEW X,Y,DIC SET DIC=2,DIC(0)="MAEQ"
  DO ^DIC WRITE !
  IF +Y>0 WRITE $$GETFUDTS(+Y)
  QUIT
  ;
GETFUDTS(TMGDFN)  ;"Return info about followup
  ;"ENTRY POINT for TIU TEXT OBJECT
  NEW INFO DO RPCCKDUE^TMGTIU10(.INFO,TMGDFN,1)
  NEW CUTOFFDT SET CUTOFFDT=$$ADDDAYS^TMGDATE(-180)  ;"Exclude fudts older than a year
  NEW CRLF SET CRLF=$CHAR(32,10)
  NEW TMGRESULT SET TMGRESULT="<B>FOLLOW-UP INFORMATION FROM PRIOR NOTES:</B> "_CRLF
  NEW TEMP SET TEMP=$GET(INFO(0)) KILL INFO(0)  
  NEW MONTHS SET MONTHS=$PIECE(TEMP,"^",2)
  NEW MSG SET MSG=$PIECE(TEMP,"^",3)
  IF +TEMP=-1 SET TMGRESULT=MSG GOTO GFDDN
  IF MONTHS'=-1 DO
  . SET TMGRESULT=TMGRESULT_"  "_MONTHS_" "_MSG_"."
  KILL INFO(1)
  NEW IDX SET IDX=""
  NEW COUNT SET COUNT=0
  NEW MAXCOUNT SET MAXCOUNT=2
  FOR  SET IDX=$ORDER(INFO(IDX),-1) QUIT:(+IDX'>0)!(COUNT+1>MAXCOUNT)  DO
  . NEW STR SET STR=$GET(INFO(IDX)) QUIT:STR=""
  . NEW VISITDT SET VISITDT=$PIECE(STR,"^",1)
  . NEW SVISITDT SET SVISITDT=$$FMTE^XLFDT(VISITDT,"2DZ")
  . NEW FUDT SET FUDT=$PIECE(STR,"^",2)
  . IF (FUDT>1)&(FUDT<CUTOFFDT) QUIT  ;"elh added cutoffdt  9/13/18
  . SET COUNT=COUNT+1
  . NEW SFUDT SET SFUDT="" 
  . DO
  . . IF FUDT=-1 SET SFUDT="(not found)" QUIT
  . . IF FUDT=1 SET SFUDT=" ( PRN )" QUIT
  . . SET SFUDT=$$FMTE^XLFDT(FUDT,"2DZ")
  . NEW TEXT SET TEXT=$PIECE(STR,"^",3)
  . IF TEXT["Time spent" SET TEXT=$P(TEXT,"Time spent",1)
  . IF TMGRESULT'="" SET TMGRESULT=TMGRESULT_CRLF
  . IF SFUDT["PRN" DO
  . . SET TMGRESULT=TMGRESULT_"  Note on "_SVISITDT_" --> f/u due: "_SFUDT_" """_TEXT_""""
  . ELSE  DO
  . . SET TMGRESULT=TMGRESULT_"  Note on "_SVISITDT_" --> f/u due approx.: "_SFUDT_" or after. --  """_TEXT_""""    
GFDDN ;  
  QUIT TMGRESULT
  ;
GET1FUDT(TMGRESULT,TMGDFN)  ;"Return info about followup
  ;"ENTRY POINT for TIU TEXT OBJECT
  IF +$G(TMGDFN)'>0 DO  GOTO G1FDDN
  . SET TMGRESULT=""
  NEW INFO DO RPCCKDUE^TMGTIU10(.INFO,TMGDFN,1)
  NEW CUTOFFDT SET CUTOFFDT=$$ADDDAYS^TMGDATE(-180)  ;"Exclude fudts older than a year
  NEW CRLF SET CRLF=$CHAR(32,10)
  SET TMGRESULT="No followup info found in GET1FUDT^TMGTIUO9"
  NEW TEMP SET TEMP=$GET(INFO(0)) KILL INFO(0)
  NEW MONTHS SET MONTHS=$PIECE(TEMP,"^",2)
  NEW MSG SET MSG=$PIECE(TEMP,"^",3)
  IF +TEMP=-1 DO  GOTO GFDDN
  . IF MSG="" SET MSG="Prior F/U Data: NO FOLLOWUP DATA FOUND IN VISTA"
  . SET TMGRESULT=MSG
  
  KILL INFO(1)
  NEW IDX SET IDX=""
  ;"FOR  SET IDX=$ORDER(INFO(IDX),-1) QUIT:+IDX'>0  DO
  SET IDX=$ORDER(INFO(IDX),-1)
  NEW STR SET STR=$GET(INFO(IDX))
  NEW VISITDT SET VISITDT=$PIECE(STR,"^",1)
  NEW SVISITDT SET SVISITDT=$$FMTE^XLFDT(VISITDT,"2DZ")
  NEW FUDT SET FUDT=$PIECE(STR,"^",2)
  IF FUDT>1 DO  ;"THIS DIDN'T HAVE ANYTHING UNDER IT. ADDED BUT MAY BREAK SOMETHING  3/24/22
  . NEW SFUDT SET SFUDT=""
  . SET SFUDT=$$FMTE^XLFDT(FUDT,"2DZ")
  . NEW TEXT SET TEXT=$PIECE(STR,"^",3)
  . IF SVISITDT="" DO  GOTO G1FDDN
  . . SET TMGRESULT="NO FOLLOWUP DATA FOUND IN VISA"
  . IF SFUDT["PRN" DO
  . . ;"SET TMGRESULT=TMGRESULT_"  Note on "_SVISITDT_" --> f/u due: "_SFUDT_" """_TEXT_""""
  . . SET TMGRESULT="Note on "_SVISITDT_" - "_TEXT
  . ELSE  DO
  . . ;"SET TMGRESULT=TMGRESULT_"  Note on "_SVISITDT_" --> f/u due approx.: "_SFUDT_" or after. --  """_TEXT_""""
  . . SET TMGRESULT="Note on "_SVISITDT_" - "_TEXT
G1FDDN ;
  SET TMGRESULT="Prior F/U Data: "_TMGRESULT
  QUIT
  ;"
TEST2 ;
  NEW X,Y,DIC SET DIC=2,DIC(0)="MAEQ"
  DO ^DIC WRITE !
  IF +Y>0 WRITE $$GETAPPTS(+Y)
  QUIT
  ;
GETAPPTS(TMGDFN,ASOFDT,SUPPRESS) ;"Return information about upcoming appts.
  ;"ENTRY POINT for TIU TEXT OBJECT
  ;"NOTE: This only pulls from custom file TMG SCHEDULE, not the standard
  ;"      VistA schedule file(s)
  ;"Input: TMGDFN -- the patient to pull
  ;"       ASOFDT -- OPTIONAL.  If null, then NOW is used.  This is the date
  ;"               to get upcoming appts relative to.
  ;"       SUPPRESS -- OPTIONAL. If set to 1, blank is returned instead of
  ;"               None found...
  ;"Result: returns a string with appt info.
  SET TMGDFN=+$GET(TMGDFN)
  NEW TMGRESULT SET TMGRESULT=""
  NEW CRLF SET CRLF=$CHAR(32,10)
  NEW INDENT SET INDENT=" "
  SET ASOFDT=+$GET(ASOFDT)
  SET SUPPRESS=+$GET(SUPPRESS)
  IF ASOFDT'>0 DO
  . SET ASOFDT=$$NOW^XLFDT
  . NEW X1,X2
  . SET X1=ASOFDT
  . SET X2=1
  . DO C^%DTC
  . SET ASOFDT=X
  NEW INFO SET INFO=$$GETAPPTS^TMGSEQL6(.INFO,TMGDFN,.ASOFDT)
  NEW DTIDX SET DTIDX=0
  FOR  SET DTIDX=$ORDER(INFO(TMGDFN,DTIDX)) QUIT:+DTIDX'>0  DO
  . NEW STR SET STR=$GET(INFO(TMGDFN,DTIDX))
  . IF $PIECE(STR,"^",7)'="A" QUIT  ;"only report active appts.
  . NEW EDATE SET EDATE=$$FMTE^XLFDT(DTIDX,"2DZ")_": "
  . NEW MIN SET MIN=$PIECE(STR,"^",2) IF MIN'="" SET MIN=MIN_" min "
  . NEW REASON SET REASON=$PIECE(STR,"^",4) 
  . SET:REASON="" REASON="appt." SET REASON=REASON_" "
  . NEW PROV SET PROV=$PIECE(STR,"^",3) IF PROV'="" DO
  . . IF +PROV'>0 SET PROV="" QUIT
  . . SET PROV=$PIECE($GET(^VA(200,+PROV,0)),"^",1)
  . . IF PROV'="" SET PROV="with "_PROV_" "
  . NEW COMMENT SET COMMENT=$PIECE(STR,"^",6) SET:COMMENT'="" COMMENT="'"_COMMENT_"' "
  . IF TMGRESULT'="" SET TMGRESULT=TMGRESULT_CRLF
  . SET TMGRESULT=TMGRESULT_INDENT_EDATE_MIN_REASON_PROV_COMMENT
  IF (TMGRESULT="")&(SUPPRESS=1) QUIT TMGRESULT  ;"6/18/19
  IF TMGRESULT="" SET TMGRESULT=INDENT_"(None found)"
  SET TMGRESULT="Upcoming appointment info:"_CRLF_TMGRESULT
  QUIT TMGRESULT
  ;
  