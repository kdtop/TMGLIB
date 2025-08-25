TMGSSQL7 ;TMG/ELH - Handle PHQ9 data from SQL queries From SequelPMS; 7/3/25
        ;;1.0;TMG-LIB;**1**; 7/3/25
        ;"
 ;"TMG SEQUEL SQL FUNCTIONS
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"
PROCPHQD(DATAARR,LOGFILE)  ;"
 ;"This routine will process DATAARR, one patient at a time.
 ;"If the data for the patient is new, it will entered
 ;"Else Update
 NEW DATAIDX SET DATAIDX=0
 FOR  SET DATAIDX=$O(TMGARRAY(DATAIDX)) QUIT:DATAIDX'>0  DO
 . NEW ONEDATA MERGE ONEDATA=TMGARRAY(DATAIDX)
 . NEW TMGDFN
 . SET TMGDFN=$$FINDDFN(.ONEDATA)
 . IF +TMGDFN'>0 QUIT  ;"COULDN'T FIND PATIENT. IGNORE
 . ;"
 . ;" NOW WE WILL HANDLE THE DATA ITSELF
 . NEW CPT,DX1,MOD1,MOD2,PAYMENT,DATE
 . SET CPT=$G(ONEDATA(1)),MOD1=$G(ONEDATA(2)),MOD2=$G(ONEDATA(3))
 . SET DX1=$G(ONEDATA(5)),PAYMENT=$G(ONEDATA(13)),DATE=$G(ONEDATA(4))
 . ;" FILE
 . NEW DATAIDX SET DATAIDX=+$O(^TMG(22764,"B",TMGDFN,0))
 . IF DATAIDX>0 DO
 . . SET DATAIDX=DATAIDX_","
 . ELSE  DO
 . . SET DATAIDX="+1,"
 . NEW TMGFDA,TMGIEN,TMGMSG
 . SET TMGFDA(22764,DATAIDX,.01)="`"_TMGDFN
 . SET TMGFDA(22764,DATAIDX,.02)=CPT
 . SET TMGFDA(22764,DATAIDX,.03)=MOD1
 . SET TMGFDA(22764,DATAIDX,.04)=MOD2
 . SET TMGFDA(22764,DATAIDX,.05)=DX1
 . SET TMGFDA(22764,DATAIDX,.06)=DATE
 . SET TMGFDA(22764,DATAIDX,.07)=PAYMENT
 . IF DATAIDX["+" DO
 . . DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
 . ELSE  DO
 . . DO FILE^DIE("E","TMGFDA","TMGMSG")  ;" note: ignores TMGMSG or errors.
 . IF $DATA(TMGMSG("DIERR")) DO
 . . ;"  LOG THE ERROR  SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
 . ELSE  DO
 . . 
 QUIT
 ;"
FINDDFN(ONELINE)  ;"Try and find patient's DFN
  ;"Input: ONELINE -- PASS BY REFERENCE.  See IMPCPTS() for CSV format
  ;"Result: -1^error or DFN
  NEW TMGRESULT SET TMGRESULT="-1^Unknown error"
  NEW SEQLNUM SET SEQLNUM=+$GET(ONELINE(7))
  NEW LNAME SET LNAME=$GET(ONELINE(9))
  NEW FNAME SET FNAME=$GET(ONELINE(8))
  NEW TMGDFN SET TMGDFN=0
  SET TMGDFN=$ORDER(^DPT("TMGS",SEQLNUM,0))
  IF TMGDFN>0 DO  GOTO ADFNDN
  . SET TMGRESULT=TMGDFN
  ;"Unable to identify by patient SEQL number
  ;"NOTE: if needed, I could try to look up by name.
  SET TMGRESULT="-1^Unable to identify unique patient. SEQL#="_SEQLNUM
ADFNDN ;
  QUIT TMGRESULT
  ;
PHQ9CODE(TMGDFN)  ;"GET THE LAST GOOD PHQ9 CODING FOR BILLABLE ITEMS
  NEW TMGRESULT SET TMGRESULT=""   ;"<-- BLANK WILL BE ERROR STATE
  SET TMGDFN=+$G(TMGDFN)
  IF TMGDFN'>0 GOTO PHQDN
  NEW DATAIDX SET DATAIDX=+$O(^TMG(22764,"B",TMGDFN,0))
  IF DATAIDX'>0 GOTO PHQDN
  NEW ZN SET ZN=$G(^TMG(22764,DATAIDX,0))
  NEW MOD SET MOD=$P(ZN,"^",3)
  IF $P(ZN,"^",4)'="" SET MOD=MOD_"&"_$P(ZN,"^",4)
  IF MOD'="" SET MOD=" "_MOD
  SET TMGRESULT="PHQ-9 Done ("_$P(ZN,"^",2)_MOD_"-"_$P(ZN,"^",5)_")"
PHQDN
  QUIT TMGRESULT
  ;"
  ;
PAIDPHQ9(TMGDFN)  ;"GET THE LAST GOOD PHQ9 CODING FOR HEALTH FACTOR REPORT
  NEW TMGRESULT SET TMGRESULT="??"   ;"<-- BLANK WILL BE ERROR STATE
  SET TMGDFN=+$G(TMGDFN)
  IF TMGDFN'>0 GOTO PHQ9DN
  NEW DATAIDX SET DATAIDX=+$O(^TMG(22764,"B",TMGDFN,0))
  IF DATAIDX'>0 GOTO PHQ9DN
  NEW ZN SET ZN=$G(^TMG(22764,DATAIDX,0))
  NEW MOD SET MOD=$P(ZN,"^",3)
  IF $P(ZN,"^",4)'="" SET MOD=MOD_"&"_$P(ZN,"^",4)
  IF MOD'="" SET MOD=" "_MOD
  SET TMGRESULT=$$EXTDATE^TMGDATE($P(ZN,"^",6))_" $"_$P(ZN,"^",7)_" USING "_$P(ZN,"^",2)_MOD_"-"_$P(ZN,"^",5)
PHQ9DN
  QUIT TMGRESULT
  ;"  
 