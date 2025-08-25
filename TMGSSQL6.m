TMGSSQL6 ;TMG/ELH - Initiate the SQL queries From SequelPMS; 7/3/25
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
PARSE(REFDATA,NEEDINSCARD,NEEDCALL,STATS)  ;"Parse data from future appointents output, from SequelPMS
  ;"Input: REFDATA -- PASS BY NAME.  Reference for ARRAY containing appointment data
  ;"          Format: 
  ;"             @REFDATA@("A",<Field#>)=FieldName
  ;"             @REFDATA@(Record#,<Field#>)=FieldValue
  ;"          Example: 
  ;"            DATA(82)                                                                        
  ;"            }~1 = a.PROVIDER
  ;"            }~2 = a.ACCOUNT_NUM
  ;"            }~3 = a.PATIENT_LAST_NAME
  ;"            }~4 = a.PATIENT_FIRST_NAME
  ;"            }~5 = d.DOB
  ;"            }~6 = a.APPOINTMENT_DATE
  ;"            }~7 = a.REASON
  ;"            }~8 = s.DURATION
  ;"            }~9 = a.COMMENTs
  ;"            }~10 = a.APPOITMENT_STATUS
  ;"            }~11 = a.CHECKIN_BY 
  ;"            }~12 = s.COMMENTS
  ;"            }~13 = s.ENTRY_DATE
  ;"            
  ;"            DATA("A")                                                                       
  ;"            }~1 = a.PROVIDER
  ;"            }~2 = a.ACCOUNT_NUM
  ;"            }~3 = a.PATIENT_LAST_NAME
  ;"            }~4 = a.PATIENT_FIRST_NAME
  ;"            }~5 = d.DOB
  ;"            }~6 = a.APPOINTMENT_DATE
  ;"            }~7 = a.REASON
  ;"            }~8 = s.DURATION
  ;"            }~9 = a.COMMENTs
  ;"            }~10 = a.APPOITMENT_STATUS
  ;"            }~11 = a.CHECKIN_BY
  ;"            }~12 = s.COMMENTS
  ;"            }~13 = s.ENTRY_DATE
  ;"Output: the data ARRAY is modified, with the following added
  ;"    DATA("APPT",DFN,FM_DT, 
  ;"                    // removed --> "DT")=Appt start date-time (FM format)
  ;"                    "MIN")=minutes duration of appt   
  ;"                    "PROVIEN")=IEN of the provider who the appt is with 
  ;"                    "REASON")=Short name for reason for appt.        
  ;"                    "COMMENT")=Comment text  <-- optional
  ;"    DATA("DT",FMDATE,DFN)=""
  ;"    DATA("ERR",IDX)=Error message  
  ;"Result: none  
  ;
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(@REFDATA@(IDX)) QUIT:(+IDX'>0)  DO
  . DO PARSE1(REFDATA,IDX,.NEEDINSCARD,.NEEDCALL,.STATS)
  ;"ZWR STATS
  ;"ZWR NEEDCALL
  ;"ZWR NEEDINSCARD
  QUIT
  ;    
PARSE1(REFDATA,IDX,NEEDINSCARD,NEEDCALL,STATS) ;
  SET STATS("TOTAL APPTS")=+$G(STATS("TOTAL APPTS"))+1
  NEW ERR SET ERR=""
  NEW NAME SET NAME=$$TRIM^XLFSTR($GET(@REFDATA@(IDX,1)))
  SET NAME=$$GETPROVNAME^TMGSSQL5(NAME)
  NEW SEQLNUM SET SEQLNUM=+$$TRIM^XLFSTR($GET(@REFDATA@(IDX,2)))
  NEW TMGDFN SET TMGDFN=-1
  IF SEQLNUM>0 SET TMGDFN=+$ORDER(^DPT("TMGS",SEQLNUM,""))
  NEW PTLNAME,PTFNAME,PTNAME
  SET PTLNAME=$$TRIM^XLFSTR($GET(@REFDATA@(IDX,3)))
  SET PTFNAME=$$TRIM^XLFSTR($GET(@REFDATA@(IDX,4)))
  SET PTNAME=PTLNAME_","_PTFNAME
  NEW DOB SET DOB=$$TRIM^XLFSTR($PIECE($GET(@REFDATA@(IDX,5))," ",1))
  IF TMGDFN'>0 DO
  . NEW X,Y,DIC SET DIC=2,DIC(0)="M"
  . ;"NEW PTNAME SET PTNAME=$$TRIM^XLFSTR($TRANSLATE($GET(@REFDATA@(IDX,3))," ",""))
  . IF PTNAME="" QUIT
  . SET X=PTNAME DO ^DIC
  . IF +Y>0 SET TMGDFN=+Y QUIT  ;"Found
  . NEW PATIENT 
  . SET PATIENT("NAME")=PTNAME
  . SET PATIENT("SEQUELNUM")=SEQLNUM
  . IF DOB="" QUIT
  . SET PATIENT("DOB")=DOB
  . SET TMGDFN=$$GETDFN^TMGGDFN(.PATIENT,0)
  . IF (SEQLNUM>0)&(TMGDFN>0) DO
  . . ;"HERE I COULD STORE MISSING SEQLNUM INTO PATIENT RECORD...
  IF TMGDFN'>0 SET ERR="EMPT|Unable to match patient to VistA database" GOTO P1DN
  SET DIC=200,DIC(0)="M",X=NAME DO ^DIC
  IF +Y'>0 SET ERR="EMPV|Unable to locate provider IEN" GOTO P1DN
  NEW PROVIEN SET PROVIEN=+Y
  NEW APPTDT SET APPTDT=$$TRIM^XLFSTR($GET(@REFDATA@(IDX,6)))
  NEW DATE,TIME SET DATE=$PIECE(APPTDT," ",1),TIME=$PIECE(APPTDT," ",2)
  SET APPTDT=DATE_"@"_$PIECE(TIME,":",1,2)
  NEW %DT SET %DT="T" SET X=APPTDT DO ^%DT
  IF Y'>0 SET ERR="EDT|Unable to determine appt time" GOTO P1DN
  NEW APPTDT SET APPTDT=Y
  ;"
  ;"  WORK ON OUTPUTTING DATA HERE
  NEW APPTCOMMENT SET APPTCOMMENT=$$UP^XLFSTR($GET(@REFDATA@(IDX,12)))
  IF APPTCOMMENT["CONFIRMED" DO
  . SET STATS("CONFIRMED")=+$G(STATS("CONFIRMED"))+1
  ELSE  DO
  . SET STATS("NEED CALL")=+$G(STATS("NEED CALL"))+1
  . SET NEEDCALL(APPTDT,PTNAME,TMGDFN)=$$EXTDATE^TMGDATE(APPTDT)_"^"_PTNAME_"^"_APPTCOMMENT
  SET @REFDATA@("DT",APPTDT,TMGDFN)=""
  SET @REFDATA@("APPT",TMGDFN,APPTDT,"PROVIEN")=PROVIEN
  ;"SET @REFDATA@("APPT",TMGDFN,APPTDT,"DT")=Y
  SET @REFDATA@("APPT",TMGDFN,APPTDT,"REASON")=$$TRIM^XLFSTR($GET(@REFDATA@(IDX,7)))
  SET @REFDATA@("APPT",TMGDFN,APPTDT,"MIN")=$$TRIM^XLFSTR($GET(@REFDATA@(IDX,8)))
  SET @REFDATA@("APPT",TMGDFN,APPTDT,"APPTCOMMENT")=APPTCOMMENT
  NEW CMT SET CMT=$$TRIM^XLFSTR($GET(@REFDATA@(IDX,9))) 
  IF CMT'="" SET @REFDATA@("APPT",TMGDFN,APPTDT,"COMMENT")=CMT
P1DN ;
  IF ERR'="" SET @REFDATA@("ERR",IDX)=ERR
  NEW INSCARD SET INSCARD=$$NEEDINS(TMGDFN)
  IF $P(INSCARD,"^",1)="1" DO
  . SET NEEDINSCARD(PTNAME_"("_DOB_")")=$P(INSCARD,"^",2)
  . SET STATS("NEED INS CARD")=+$G(STATS("NEED INS CARD"))+1
  ELSE  DO
  . SET STATS("HAS INS CARD")=+$G(STATS("HAS INS CARD"))+1
  SET @REFDATA@("APPT",TMGDFN,APPTDT,"INSCARD")=INSCARD
  QUIT
  ;
LOAD(FULLPATHNAME,DELFILE,LOGFILE) ;
  ;"Purpose: Load file with schedule information
  ;"INPUT:  FULLPATHNAME -- fill file name with path of input file to import (csv format)
  ;"        DELFILE -- Optional.  If 1, then file will be deleted if import
  ;"                    occurred without erros. 
  NEW DATA,TMGRESULT
  NEW X DO NOW^%DTC
  SET TMGRESULT=$$LCSV2ARR^TMGIOUT4(FULLPATHNAME,"DATA")
  IF +TMGRESULT'>0 DO
  . DO DW^%DTC
  . NEW DAYS SET DAYS="MONDAY,TUESDAY,THURSDAY,FRIDAY"
  . IF DAYS'[X SET TMGRESULT="NOT PROPER DAY"
  IF TMGRESULT="NOT PROPER DAY" GOTO LDDN
  IF +TMGRESULT'>0 DO ERR^TMGSSQL5($P(TMGRESULT,"^",2)_" on "_$$FMTE^XLFDT($$NOW^XLFDT)) GOTO LDDN
  NEW NEEDINSCARD,NEEDCALL,STATS
  DO PARSE("DATA",.NEEDINSCARD,.NEEDCALL,.STATS)
  DO OUTPUTINFO(.NEEDINSCARD,.NEEDCALL,.STATS)
  IF $GET(DELFILE)=1 DO
  . DO LOGLINE^TMGSSQL1(LOGFILE,"FILING WAS SUCCESSFUL",0,0)  	   
  . IF $$DELFILE^TMGIOUTL(FULLPATHNAME)=0 DO ERR^TMGSSQL5("Error deleting file:")  
LDDN ;  
  QUIT  
  ;"
NEEDINS(TMGDFN)  ;"DOES PATIENT NEED A NEW CARD
  ;" RESULT IS 1^MESSAGE -OR- 0^PATIENT IS OKAY
  NEW TMGRESULT SET TMGRESULT="0^PATIENT IS OKAY"
  IF TMGDFN=28314 QUIT TMGRESULT    ;"EXCLUDE PATIENTS HERE. ONLY TERESA GONZALES FOR NOW.
  IF TMGDFN=74692 QUIT TMGRESULT          ;"PAMELA SADLER 
  NEW BEGDATE SET BEGDATE=$$FIRSTYR^TMGDATE
  NEW LASTINSCARD SET LASTINSCARD=0
  NEW TIUIEN SET TIUIEN=0
  FOR  SET TIUIEN=$O(^TIU(8925,"C",TMGDFN,TIUIEN)) QUIT:TIUIEN'>0  DO
  . NEW DOCTYPE SET DOCTYPE=$P($G(^TIU(8925,TIUIEN,0)),"^",1)
  . IF DOCTYPE'=2002 QUIT
  . NEW DOCDATE SET DOCDATE=$P($G(^TIU(8925,TIUIEN,13)),"^",1)
  . IF DOCDATE>LASTINSCARD SET LASTINSCARD=DOCDATE
  IF LASTINSCARD>BEGDATE QUIT TMGRESULT
  IF LASTINSCARD=0 DO
  . SET TMGRESULT="1^NO INSURANCE CARD FOUND IN CPRS"
  ELSE  DO
  . SET TMGRESULT="1^Last INS in CPRS: "_$$EXTDATE^TMGDATE(LASTINSCARD,1)
  QUIT TMGRESULT
  ;"
OUTPUTINFO(NEEDINSCARD,NEEDCALL,STATS)
  NEW %ZIS,IOP
  SET IOP="S121-LAUGHLIN-LASER"
  DO ^%ZIS  ;"standard device call
  IF POP DO  QUIT
  . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output. Aborting.")
  USE IO
  WRITE !
  WRITE "************************************************************",!
  WRITE "               UPCOMING APPOINTMENT NEEDS ",!
  WRITE "                              PLEASE DELIVER TO LINDSEY.",!
  WRITE "************************************************************",!
  WRITE "                                            (From TMGSSQL6.m)",!!
  ;"WRITE ?5,"TIME",?15,"PATIENT",?40,"LAST SCAN",!
  WRITE "-------------------------------------------------------------",!
  WRITE "-------------------------APPT STATS--------------------------",!
  WRITE "----       TOTAL APPOINTMENTS: ",+$G(STATS("TOTAL APPTS")),?57,"----",!
  WRITE "----                CONFIRMED: ",+$G(STATS("CONFIRMED")),?57,"----",!
  WRITE "----               NEED CALLS: ",+$G(STATS("NEED CALL")),?57,"----",!
  WRITE "--------",?53,"--------",!
  WRITE "----     HAS UPDATED INS CARD: ",+$G(STATS("HAS INS CARD")),?57,"----",!
  WRITE "----       NEEDS UPDATED CARD: ",+$G(STATS("NEED INS CARD")),?57,"----",!
  WRITE "-------------------------------------------------------------",!,!
  WRITE "====   THE FOLLOWING PATIENTS NEED UPDATED INS CARDS     ====",!
  NEW PTNAME SET PTNAME=""
  FOR  SET PTNAME=$O(NEEDINSCARD(PTNAME)) QUIT:PTNAME=""  DO
  . WRITE "[    ] ",PTNAME,?40,$G(NEEDINSCARD(PTNAME)),!
  WRITE !
  WRITE "====   THE FOLLOWING PATIENTS APPOINTMENT REMINDERS      ====",!
  WRITE "DONE",?7,"PATIENT",?30,"APPT DT/TIME",?50,"COMMENT",!
  ;"SET NEEDCALL(APPTDT,PTNAME,TMGDFN)
  NEW APPTDT SET APPTDT=0
  NEW CURRENTDAY SET CURRENTDAY=0
  FOR  SET APPTDT=$O(NEEDCALL(APPTDT)) QUIT:APPTDT=""  DO
  . NEW THISDAY SET THISDAY=$P(APPTDT,".",1)
  . IF THISDAY'=CURRENTDAY DO
  . . WRITE "-------------------- ",$$EXTDATE^TMGDATE(THISDAY)," --------------------",!
  . . SET CURRENTDAY=THISDAY
  . NEW PTNAME SET PTNAME=""
  . FOR  SET PTNAME=$O(NEEDCALL(APPTDT,PTNAME)) QUIT:PTNAME=""  DO
  . . NEW TMGDFN SET TMGDFN=0
  . . FOR  SET TMGDFN=$O(NEEDCALL(APPTDT,PTNAME,TMGDFN)) QUIT:TMGDFN'>0  DO
  . . . WRITE "[    ] ",PTNAME,?30,$P($$EXTDATE^TMGDATE(APPTDT),"@",2),?50,$P($G(NEEDCALL(APPTDT,PTNAME,TMGDFN)),"^",3),!
  DO ^%ZISC  ;" Close the output device
  QUIT
  ;"
  