TMGGMRV1 ;TMG/kst-Get Vitals ; 03/30/15,1/12/17
         ;;1.0;TMG-LIB;**1,17**;03/30/15
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"=======================================================================
 ;"PUBLIC FUNCTIONS
 ;"=======================================================================
 ;"GETVITLS(ADFN,SDT,EDT,OUT,OPTION) -- collect vitals array for patient for date range provided
 ;"TEMP(ADFN,FMDT,MAXDTVARIANCE) -- get temp near given date
 ;"BP(ADFN,FMDT,MAXDTVARIANCE) -- get BP near given date
 ;"RESP(ADFN,FMDT,MAXDTVARIANCE)  -- get Resp near given date
 ;"PULSE(ADFN,FMDT,MAXDTVARIANCE) -- get Pulse near given date
 ;"ONEVITAL(DFN,FMDT,TYPE,MAXDTVARIANCE)  GET ONE VITAL near given date
 ;"TREND(ADFN,FMDT,VITAL,NUM,DELIMITER) ;
 ;"=======================================================================
 ;"PRIVATE FUNCTIONS
 ;"=======================================================================
 ;"TEST --TEST ENTRY POINT
 ;"TEST2  --TEST TREND
 ;"UNITARR(ARRAY)  -- Set array with vital units
 ;"=======================================================================
TEST  ;"TEST ENTRY POINT
  NEW ADFN,SDT,EDT,VTSTR
  SET (ADFN,SDT,EDT,VTSTR)=""
  WRITE "This will test the GETVITLS function",!
  NEW DIC SET DIC=2,DIC(0)="MAEQ"
  NEW X,Y DO ^DIC WRITE !
  ;"READ "PATIENT IEN:",ADFN,!
  ;"IF +ADFN'>0 SET ADFN=9182
  SET ADFN=+Y
  IF Y'>0 QUIT
  READ "START DATE: ",SDT:$GET(DTIME,3600),!
  READ "END DATE:   ",EDT:$GET(DTIME,3600),!
  READ "WHICH VITALS DO YOU WANT: ",VTSTR:$GET(DTIME,3600),!
  NEW OUT
  W $$GETVITLS(ADFN,SDT,EDT,.OUT,.VTSTR)
  IF $DATA(OUT) DO ZWRITE^TMGZWR("OUT")
  ELSE  WRITE "(NO DATA)",!
  QUIT
  ;  
TEST2  ;"TEST TREND
  NEW ADFN,SDT,EDT,VTSTR,COUNT,DELIMITER
  SET (ADFN,SDT,EDT,VTSTR,COUNT,DELIMITER)=""
  WRITE "This will test the TREND function",!
  READ "PATIENT IEN:",ADFN:$GET(DTIME,3600),!
  IF +ADFN'>0 SET ADFN=9182
  READ "END DATE:   ",EDT:$GET(DTIME,3600),!
  READ "COUNT:      ",COUNT:$GET(DTIME,3600),!
  READ "DELIMITER:  ",DELIMITER:$GET(DTIME,3600),!
  READ "WHICH VITAL DO YOU WANT: ",VTSTR:$GET(DTIME,3600),!
  NEW OUT
  W $$TREND(ADFN,EDT,VTSTR,.COUNT,.DELIMITER)
  QUIT
  ;"
GETVITLS(ADFN,SDT,EDT,OUT,OPTION) ;
  ;"Purpose: to collect vitals array for patient for date range provided
  ;"Input  ADFN -- patient IEN
  ;"       SDT -- OPTIONAL.  Start of date range, in internal or external format
  ;"              Resolution will be only to DAY level.  I.e time will be 
  ;"              stripped off, so that SDT will referance the BEGINNING
  ;"              of the day.  Default value is 0
  ;"       EDT -- OPTIONAL.  End of date range, in internal or external format
  ;"              Resolution will be only to DAY level.  I.e time will be 
  ;"              changed to .999999, to referance the END of the day.
  ;"              Default value will be 9999999.999999
  ;"       OUT -- PASS BY REFERANCE, AN OUT PARAMETER. Format:
  ;"             OUT(<VITAL TYPE>,<FMDT>,VALUE)=
  ;"             OUT(<VITAL TYPE>,"UNITS",UNITS)=
  ;"       OPTION -- PASS BY REFERENCE. Vitals to include, semicolon delimited
  ;"             e.g.  "BP;HT"
  ;"             These can include BP,T,WT,HT,T,R,PN,PO2
  ;"NOTE: Units will be:
  ;"    WT -- lb
  ;"    BP -- mmHg/mmHg
  ;"    HT -- inches
  ;"    T -- deg F
  ;"    P -- BPM
  ;"    R -- RPM
  ;"    PN -- ""  (pain scale with no units)
  ;"    POx -- %
  ;"Result: 1 if OK, or -1^Error if problem, or 0 if no values found. 
  ;"NOTE: vitals that have been marked as entered in error will not be returned.
  ;"NOTE: compare to VITARR^TMGTIUO3 at some point...
  ;
  NEW RESULT SET RESULT=0
  SET SDT=$GET(SDT) IF SDT="" SET SDT=0
  IF +SDT'=SDT DO
  . NEW %DT,X SET %DT="T",X=SDT
  . DO ^%DT SET SDT=Y
  SET EDT=$GET(EDT) IF EDT="" SET EDT=9999999.9999
  IF +EDT'=EDT DO
  . NEW %DT,X SET %DT="T",X=EDT
  . DO ^%DT SET EDT=Y
  ;"
  SET EDT=$PIECE(EDT,".",1)_".9999"
  KILL ^UTILITY($J,"GMRVD")
  NEW UNITARR DO UNITARR(.UNITARR)
  SET GMRVSTR(0)=SDT_"^"_EDT_"^999^"_0
  IF $DATA(OPTION) SET GMRVSTR=$GET(OPTION)
  IF GMRVSTR="" SET GMRVSTR="HT;WT;T;P;R;BP;PN,PO2"  ;"GET ALL
  NEW DFN SET DFN=ADFN
  NEW X,Y DO EN1^GMRVUT0
  IF '$D(^UTILITY($J,"GMRVD")) QUIT RESULT
  SET RESULT=1
  NEW VITAL,DATE,VTIEN
  SET VITAL=""
  FOR  SET VITAL=$ORDER(^UTILITY($J,"GMRVD",VITAL)) QUIT:VITAL=""  DO
  . NEW UNIT SET UNIT=$GET(UNITARR(VITAL))
  . SET OUT(VITAL,"UNITS",UNIT)=""
  . SET DATE=0
  . FOR  SET DATE=$ORDER(^UTILITY($J,"GMRVD",VITAL,DATE)) QUIT:DATE'>0  DO
  . . SET VTIEN=0
  . . FOR  SET VTIEN=$ORDER(^UTILITY($J,"GMRVD",VITAL,DATE,VTIEN)) QUIT:VTIEN'>0  DO
  . . . IF $DATA(^GMR(120.5,VTIEN,2)) QUIT  ;"SET AS EIE
  . . . NEW DATA SET DATA=$GET(^UTILITY($J,"GMRVD",VITAL,DATE,VTIEN))
  . . . SET OUT(VITAL,$PIECE(DATA,"^",1),$PIECE(DATA,"^",8))=""
  QUIT RESULT
  ;
UNITARR(ARRAY)  ;"Set array with vital units
  SET ARRAY("WT")="Lbs"
  SET ARRAY("HT")="In"
  SET ARRAY("T")="Deg F"
  SET ARRAY("P")="BPM"
  SET ARRAY("R")="RPM"
  SET ARRAY("BP")="mmHg/mmHg"
  SET ARRAY("PN")=""
  SET ARRAY("PO2")="%"
  QUIT
  ;"  
TEMP(ADFN,FMDT,MAXDTVARIANCE) ;
  ;"Input: ADFN -- Patient IEN
  ;"       FMDT -- the as of date for vitals.  NOTE: If no data on given date, the
  ;"             will next get return closer of PRIOR vs FOLLOWING data point. 
  ;"       MAXDTVARIANCE -- OPTIONAL.  See ONEVITAL() doc  
  ;"Result: VitalValue^FMDT^Units
  QUIT $$ONEVITAL(ADFN,FMDT,"T",.MAXDTVARIANCE) 
  ;
BP(ADFN,FMDT,MAXDTVARIANCE) ;
  ;"Input: ADFN -- Patient IEN
  ;"       FMDT -- the as of date for vitals.  NOTE: If no data on given date, the
  ;"             will next get return closer of PRIOR vs FOLLOWING data point. 
  ;"       MAXDTVARIANCE -- OPTIONAL.  See ONEVITAL() doc  
  ;"Result: VitalValue^FMDT^Units
  QUIT $$ONEVITAL(ADFN,FMDT,"BP",.MAXDTVARIANCE) 
  ;
RESP(ADFN,FMDT,MAXDTVARIANCE) ;
  ;"Input: ADFN -- Patient IEN
  ;"       FMDT -- the as of date for vitals.  NOTE: If no data on given date, the
  ;"             will next get return closer of PRIOR vs FOLLOWING data point. 
  ;"       MAXDTVARIANCE -- OPTIONAL.  See ONEVITAL() doc  
  ;"Result: VitalValue^FMDT^Units
  QUIT $$ONEVITAL(ADFN,FMDT,"R",.MAXDTVARIANCE) 
  ;
PULSE(ADFN,FMDT,MAXDTVARIANCE) ;
  ;"Input: ADFN -- Patient IEN
  ;"       FMDT -- the as of date for vitals.  NOTE: If no data on given date, the
  ;"             will next get return closer of PRIOR vs FOLLOWING data point. 
  ;"       MAXDTVARIANCE -- OPTIONAL.  See ONEVITAL() doc  
  ;"Result: VitalValue^FMDT^Units
  QUIT $$ONEVITAL(ADFN,FMDT,"PN",.MAXDTVARIANCE) 
  ;
ONEVITAL(DFN,FMDT,TYPE,MAXDTVARIANCE)  ;"GET ONE VITAL
  ;"Input: ADFN -- Patient IEN
  ;"       FMDT -- the as of date for vitals.  NOTE: If no data on given date, the
  ;"             will next get return closer of PRIOR vs FOLLOWING data point.
  ;"       TYPE -- Vital type, e.g. BP,T,WT,HT,T,R,PN, or PO2
  ;"       MAXDTVARIANCE -- OPTIONAL.  The max number of days that returned vital
  ;"                 can vary from specified vital date.  For example, if last 
  ;"                 availablevital measurement was 1 month prior to FMDT, and 
  ;"                 this paramwas set to 7, then empty result would be returned.  
  ;"Result: VitalValue^FMDT^Units
  NEW TEMP,ARR
  SET TEMP=$$GETVITLS(ADFN,FMDT,FMDT,.ARR,TYPE)   
  IF $DATA(ARR)=0 DO
  . SET MDTV=+$GET(MAXDTVARIANCE,9999)
  . SET TEMP=$$GETVITLS(ADFN,,,.ARR,TYPE)
  . NEW DTPRIOR,DTAFTER
  . SET DTPRIOR=+$ORDER(ARR(TYPE,FMDT),-1)
  . SET DTAFTER=+$ORDER(ARR(TYPE,FMDT))
  . NEW DELTAPRIOR SET DELTAPRIOR=$$FMDIFF^XLFDT(FMDT,DTPRIOR)
  . NEW DELTAAFTER SET DELTAAFTER=$$FMDIFF^XLFDT(DTAFTER,FMDT)
  . IF DELTAPRIOR>MDTV SET DTPRIOR=0
  . IF DELTAAFTER>MDTV SET DTAFTER=0
  . IF (DTAFTER=0),(DTPRIOR'=0) SET FMDT=DTPRIOR QUIT
  . IF (DTAFTER'=0),(DTPRIOR=0) SET FMDT=AFTER QUIT
  . IF DTPRIOR=0,DTAFTER=0 QUIT          
  . IF DELTAPRIOR'>DELTAAFTER SET FMDT=DTPRIOR
  . SET FMDT=DTAFTER
  NEW UNITARR DO UNITARR(.UNITARR)
  NEW TMGRESULT SET TMGRESULT=$ORDER(ARR(TYPE,FMDT,""))_"^"_FMDT_"^"_$GET(UNITARR(TYPE))
  QUIT TMGRESULT
  ;
TREND(ADFN,FMDT,VITAL,NUM,DELIMITER,ADDDT) ;
  ;"Purpose: return a trend of BP's staring from FMDT, and extending backwards in time
  ;"         up to NUM values, but not more than 2 yrs.  
  ;"Input:  ADFN -- Patient IEN
  ;"        FMDT -- Start date.  Internal or external format. 
  ;"        VITAL -- Type (abbrv. e.g. BP, WT, etc...)
  ;"        NUM -- OPTIONAL.  How many to return.  Defult = 99
  ;"        DELIMITER (OPTIONAL)-- Char(s) to place between values
  ;"                               Default is ";"
  ;"        ADDDT(OPTIONAL) -- Add the date to the first value
  ;"                           Default is 0
  ;"Result: returns formatted string of BP trend, or "" if none found.
  NEW RESULT SET RESULT=""
  SET ADDDT=+$G(ADDDT)
  IF $GET(VITAL)="" QUIT
  SET NUM=+$GET(NUM) IF NUM=0 SET NUM=999
  IF $GET(DELIMITER)="" SET DELIMITER=";"
  IF $GET(FMDT)="" SET FMDT="T"
  NEW OUT
  SET RESULT=$$GETVITLS(ADFN,,FMDT,.OUT,.VITAL) 
  IF +RESULT<1 QUIT RESULT
  NEW DATE SET DATE=9999999
  NEW CURNUM SET CURNUM=1
  SET RESULT=""
  FOR  SET DATE=$ORDER(OUT(VITAL,DATE),-1) QUIT:(DATE'>0)!(CURNUM>NUM)  DO
  . SET CURNUM=CURNUM+1
  . IF RESULT'="" SET RESULT=RESULT_DELIMITER
  . SET RESULT=RESULT_$ORDER(OUT(VITAL,DATE,""))
  . IF (ADDDT=1)&(CURNUM=2) SET RESULT=RESULT_" ("_$$EXTDATE^TMGDATE(DATE,1)_")"
  QUIT RESULT
  ;