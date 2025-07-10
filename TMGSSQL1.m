TMGSSQL1 ;TMG/ELH - Initiate the SQL queries From SequelPMS; 12/19/24
        ;;1.0;TMG-LIB;**1**; 12/19/24
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
 ;"=======================================================================
 ;"  NOTES ON FUNCTIONALITY:
 ;"  Overview - SequelMed's Oracle database utilizes a program called UViews, which allows
 ;"    connections for SQL Queries. To use this, you must have a PC with Oracle's
 ;"    11g 64-bit client installed. I have written a utility that will initiate
 ;"    the connection and make the calls.
 ;"   Details - For the initial project, the program polls a directory (V:\SQL) for 
 ;"    a file named SQLRequest.txt. The contents of this file should contain:
 ;"    the SQL Query statement^OutputFilePathName. When the program finds a file in the folder
 ;"    it will create a text file named SQLRunning.txt to alert any other processes that
 ;"    it is currently busy. Once the process is complete this file will be removed.
 ;"    
 ;"
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"
 ;"SQLREQUEST(SQLSTATEMENT,SQLOUTPATH,SQLOUTNAME,DEBUGMODE)  ;"
 ;"      Purpose: This will be the main function to submit the SQL statement and watch for the 
 ;"                process to finish.
 ;"GTSCHSTA()  ;""   GET CURRENT SCHEDULE STATUS
 ;"GTFTRSCH()  ;""   GET FUTURE APPOINTMENTS
 ;"GTPTDEMO()  ;""   GET PATIENT DEMOGRAPHICS
 ;"GTPTENC()   ;""   GET PATIENT ENCOUNTER INFO (CPTS AND ICDS)  
 ;"LOGLINE(LOGFILE,LINE,CLEARFILE) ;"" ADD NEW LINE TO LOG FILE
 ;"
 ;"
 ;"=======================================================================
 ;" CONSTANTS
 ;"=======================================================================
 ;"
 ;"SQLDIRECTORY()    ;""Directory for SQL communications 
 ;"WINDIR()          ;""Directory for the Windows save (should map directly to SQLDIRECTORY
 ;"LOGDIRECTORY()    ;""Directory for SQL communications  
 ;"SQLRUNNINGFILE()  ;""Name of SQL running file 
 ;"SQLREQUESTFILE()  ;""Name of SQL request file name 
 ;"DATAFILETYPE()    ;""Extension for data file  
 ;"LOGFILETYPE()     ;""Extension for log file 
 ;"SCHSTATF()        ;""File name for schedule status 
 ;"FUTSCHDF()        ;""File name for future appointments 
 ;"PTDEMOF()         ;""File name for patient demographics 
 ;"ENCOUNTF()        ;""File name for encounter information
 ;"SQLSTAT()         ;""SQL Statement for schedule status 
 ;"SQLAPPT()         ;""SQL Statement for future appointments 
 ;"SQLDEMO()         ;""SQL Statement for patient demographics 
 ;"SQLENC()          ;""SQL Statement for encounter information
 ;" 
 ;"=======================================================================
 ;" Private Functions.
 ;"=======================================================================
 ;"FILEWAIT(FILEPATHNAME,TIMEOUTMINS,DEBUGMODE,FORCEREMOVE)  
 ;"   Purpose: This function is used to ensure a file doesn't exists, or if it does
 ;"         wait for it to be removed.
 ;"         It will return a 1 if the file isn't there or is removed in the 
 ;"         appropriate amount of time (TIMEOUTMINS)
 ;"OPENCHCK()  ;" 
 ;" This function will determine if a process is running during a time our office is considered open
 ;"
 ;"
 ;"==================================================================================================
 ;"==========================   SQL CONSTANTS BELOW. ANY PROCESSES    ===============================
 ;"==========================  ANY FUNCTIONS SHOULD REFERENCE THESE   ===============================
 ;"================================================================================================== 
SQLDIRECTORY()  ;"Directory for SQL communications
 QUIT "/mnt/WinServer/SQL/"
 ;"
WINDIR()   ;"Directory for the Windows save (should map directly to SQLDIRECTORY
 QUIT "V:\SQL\"  
 ;"
LOGDIRECTORY()  ;"Directory for SQL communications
 NEW LOGDIR SET LOGDIR="/mnt/WinServer/SQL/LOGS/"
 NEW DATE SET DATE=$P($$HTE^XLFDT($horolog,"5Z"),"@",1)
 NEW YYYY,MM,DD SET YYYY=$P(DATE,"/",3),MM=$P(DATE,"/",1),DD=$P(DATE,"/",2)
 SET LOGDIR=LOGDIR_YYYY_"/"_MM_"/"_DD_"/"
 IF $$EnsureDir^TMGKERNL(LOGDIR)=0 SET LOGDIR="/mnt/WinServer/SQL/LOGS/"  ;"COULDN'T CREATE SO LEAVE AS ROOT
 QUIT LOGDIR
 ;" 
SQLRUNNINGFILE()  ;"Name of SQL running file
 QUIT "SQLRunning.txt"
 ;"
SQLREQUESTFILE()  ;"Name of SQL request file name
 QUIT "SQLRequest.txt"
 ;"
DATAFILETYPE()  ;"Extension for data file
 QUIT ".csv"
 ;" 
LOGFILETYPE()  ;"Extension for log file
 QUIT ".log"
 ;"
SCHSTATF()  ;"File name for schedule status
 QUIT "SQLSchStatus"
 ;"
FUTSCHDF()  ;"File name for future appointments
 QUIT "FutureAppointments"
 ;"
PTDEMOF()  ;"File name for patient demographics
 QUIT "Demographics"
 ;"
ENCOUNTF()  ;"File name for encounter information
 QUIT "EncounterData"
 ;"
RANDOMF()   ;"File name for Random SQL calls
 QUIT "Random"_$J_"Data"
 ;"
SQLSTAT()  ;"SQL Statement for schedule status
 QUIT "SELECT a.PROVIDER,a.ACCOUNT_NUM,a.APPOINTMENT_DATE,a.REASON,s.DURATION,a.CHECKIN_DATE,a.CHECKOUT_DATE,a.APPOITMENT_STATUS,a.CHECKIN_BY FROM U_PATIENT_APPOINTMENTS a JOIN U_SLOTS s ON a.SLOT_SEQ_NUM = s.SLOT_SEQ_NUM WHERE TRUNC(a.APPOINTMENT_DATE) = TRUNC(SYSDATE) AND a.APPOITMENT_STATUS = 'SCHEDULED'"
 ;"
SQLAPPT()  ;"SQL Statement for future appointments
 QUIT "SELECT a.PROVIDER,a.ACCOUNT_NUM,a.PATIENT_LAST_NAME,a.PATIENT_FIRST_NAME,d.DOB,a.APPOINTMENT_DATE,a.REASON,s.DURATION,a.COMMENTS,a.APPOITMENT_STATUS,a.CHECKIN_BY FROM U_PATIENT_APPOINTMENTS a JOIN U_SLOTS s ON a.SLOT_SEQ_NUM = s.SLOT_SEQ_NUM JOIN U_PATIENT_DEMOGRAPHIC d ON a.PATIENT_SEQ_NUM = d.PATIENT_SEQ_NUM WHERE TRUNC(a.APPOINTMENT_DATE) > TRUNC(SYSDATE) AND a.APPOITMENT_STATUS = 'SCHEDULED'"
 ;"
SQLDEMO()  ;"SQL Statement for patient demographics
 QUIT "SELECT pd.PATIENT_SEQ_NUM, pd.PATIENT_PRACTICE, pd.LAST_NAME AS PATIENT_LAST_NAME, pd.FIRST_NAME AS PATIENT_FIRST_NAME, pd.ACCOUNT_NUM, pd.ADDRESS1, pd.STATE, prp.LAST_NAME AS RESP_PARTY_LAST_NAME, prp.FIRST_NAME AS RESP_PARTY_FIRST_NAME, pd.PRACTICE_SEQ_NUM, pd.ADDRESS2, pd.PATIENT_LOCATION, pd.CITY, pd.PATIENT_PROVIDER, pd.ZIPCODE, pd.CLASS_SEQ_NUM, pd.DOB, pd.HOME_TEL, pd.CELL_NUM, pd.SSN, pd.SEX, pd.ACTIVE, (SELECT RTRIM(XMLAGG(XMLELEMENT(e, pp.PLAN || '|' || pp.PLAN_ORDER || '|' || pp.ID_NUM || '|' || pp.GROUP_NAME || '|' || pp.ACTIVE_PLAN || ', ') ORDER BY pp.PLAN).EXTRACT('//text()'), ', ') FROM U_PATIENT_PLANS pp WHERE pp.PATIENT_SEQ_NUM = pd.PATIENT_SEQ_NUM AND pp.ACTIVE_PLAN = 'Y') AS INSURANCES, pd.COMMENTS FROM U_PATIENT_DEMOGRAPHIC pd LEFT JOIN U_PATIENT_RESP_PARTY prp ON pd.PATIENT_RESP_SEQ_NUM = prp.PATIENT_RESP_SEQ_NUM WHERE pd.PATIENT_PRACTICE = 'FPG'"
 ;"QUIT "SELECT pd.PATIENT_SEQ_NUM, pd.PATIENT_PRACTICE, pd.LAST_NAME AS PATIENT_LAST_NAME, pd.FIRST_NAME AS PATIENT_FIRST_NAME, pd.ACCOUNT_NUM, pd.ADDRESS1, pd.STATE, prp.LAST_NAME AS RESP_PARTY_LAST_NAME, prp.FIRST_NAME AS RESP_PARTY_FIRST_NAME, pd.PRACTICE_SEQ_NUM, pd.ADDRESS2, pd.PATIENT_LOCATION, pd.CITY, pd.PATIENT_PROVIDER, pd.ZIPCODE, pd.CLASS_SEQ_NUM, pd.DOB, pd.HOME_TEL, pd.CELL_NUM, pd.SSN, pd.SEX, pd.ACTIVE, (SELECT RTRIM(XMLAGG(XMLELEMENT(e, pp.PLAN || '|' || pp.PLAN_ORDER || '|' || pp.ID_NUM || '|' || pp.GROUP_NAME || '|' || pp.ACTIVE_PLAN || ', ') ORDER BY pp.PLAN).EXTRACT('//text()'), ', '), pd.COMMENTS FROM U_PATIENT_PLANS pp WHERE pp.PATIENT_SEQ_NUM = pd.PATIENT_SEQ_NUM AND pp.ACTIVE_PLAN = 'Y') AS INSURANCES FROM U_PATIENT_DEMOGRAPHIC pd LEFT JOIN U_PATIENT_RESP_PARTY prp ON pd.PATIENT_RESP_SEQ_NUM = prp.PATIENT_RESP_SEQ_NUM WHERE pd.PATIENT_PRACTICE = 'FPG'"
 ;"
SQLENC()   ;"SQL Statement for encounter information
 QUIT "SELECT pd.PATIENT_SEQ_NUM,pd.PATIENT_PRACTICE,pd.LAST_NAME,pd.FIRST_NAME,pd.ACCOUNT_NUM,pd.DOB,c.VISIT_DATE_FROM,c.CPT_CODE,c.ICD_9_CODE1,c.ICD_9_CODE2,c.ICD_9_CODE3,c.ICD_9_CODE4,c.CHARGE_PROVIDER,c.ENTERED_BY  FROM U_PATIENT_DEMOGRAPHIC pd JOIN U_CHARGES c ON pd.PATIENT_SEQ_NUM = c.PATIENT_SEQ_NUM WHERE c.VISIT_DATE_FROM >= TRUNC(SYSDATE) - 14 AND c.VISIT_DATE_FROM < TRUNC(SYSDATE)"
 ;"
SQLFORMS(DATE)  ;"SQL Statement for the Scanner Barcoded forms
 QUIT "SELECT a.PROVIDER,a.APPOINTMENT_DATE,'""' || a.PATIENT_LAST_NAME || ',' || a.PATIENT_FIRST_NAME || '""' AS PatientName,a.APPOINTMENT_DATE,s.DURATION,a.REASON,a.PATIENT_LAST_NAME,a.PATIENT_FIRST_NAME,a.ACCOUNT_NUM,a.ACCOUNT_NUM,d.DOB,d.HOME_TEL,a.COMMENTS,a.CHECKIN_BY,a.CHECKIN_BY,a.CHECKIN_BY,a.CHECKIN_BY,a.CHECKIN_BY,a.CHECKIN_BY FROM U_PATIENT_APPOINTMENTS a JOIN U_SLOTS s ON a.SLOT_SEQ_NUM = s.SLOT_SEQ_NUM JOIN U_PATIENT_DEMOGRAPHIC d ON a.PATIENT_SEQ_NUM = d.PATIENT_SEQ_NUM WHERE TRUNC(a.APPOINTMENT_DATE) = DATE '"_DATE_"' AND a.APPOITMENT_STATUS = 'SCHEDULED' ORDER BY a.PROVIDER ASC, a.APPOINTMENT_DATE ASC"
 ;"
BITEMSQL(DATE)  ;"SQL Statement for getting the charges for a given date
 QUIT "SELECT pd.ACCOUNT_NUM,c.VISIT_DATE_FROM,c.CPT_CODE,c.ICD_9_CODE1,c.ICD_9_CODE2,c.ICD_9_CODE3,c.ICD_9_CODE4 FROM U_CHARGES c JOIN U_PATIENT_DEMOGRAPHIC pd ON c.PATIENT_SEQ_NUM = pd.PATIENT_SEQ_NUM WHERE c.VISIT_DATE_FROM = TO_DATE('"_DATE_"','MM/DD/YYYY')"
 ;"
FBUCKSQL()  ;"SQL Statement for getting Followup Bucket items
 QUIT "SELECT fb.VISIT_SEQ_NUM, fb.SUSPENDED, pd.ACCOUNT_NUM, pd.LAST_NAME, pd.FIRST_NAME, pd.DOB, c.VISIT_DATE_FROM, c.CPT_CODE, c.MODIFIER_CODE1, c.MODIFIER_CODE2,  c.PLAN_BALANCE FROM U_FOLLOWUP_BUCKET fb JOIN U_CHARGES c ON fb.VISIT_SEQ_NUM = c.VISIT_SEQ_NUM JOIN U_PATIENT_DEMOGRAPHIC pd ON c.PATIENT_SEQ_NUM = pd.PATIENT_SEQ_NUM WHERE c.PLAN_BALANCE > 0"
 ;"
 ;" 
 ;"==================================================================================================
 ;"===================================   MAIN SQL ROUTINES    =======================================
 ;"================================================================================================== 
FILEWAIT(FILEPATHNAME,TIMEOUTMINS,DEBUGMODE,FORCEREMOVE)  ;"
  ;"Purpose: This function is used to ensure a file doesn't exists, or if it does
  ;"         wait for it to be removed.
  ;"         It will return a 1 if the file isn't there or is removed in the 
  ;"         appropriate amount of time (TIMEOUTMINS)
  ;"Input: FILEPATHNAME - File name with path included
  ;"       TIMEOUTMINS - Number of minutes to wait before timing out
  ;"       DEBUGMODE - if 1 will write status
  ;"       FORCEREMOVE - delete the file if it still exists and is older than 1 hour
  NEW TMGRESULT SET TMGRESULT="1^OK"
  SET FORCEREMOVE=+$G(FORCEREMOVE)
  IF DEBUGMODE=1 WRITE "CHECKING FOR FILE: ",FILEPATHNAME,!
  NEW RUNCOUNT SET RUNCOUNT=12*TIMEOUTMINS  ;"SET COUNT BASED ON RUNNING EVERY 5 SECONDS (12 TIMES PER MINUTE)
  ;"
  IF $$FILEXIST^TMGIOUTL(FILEPATHNAME) DO  ;"HERE WE WILL WAIT 
  . IF DEBUGMODE=1 WRITE "FILE EXISTS. WAITING FOR IT TO BE REMOVED"
  . NEW I,DONE SET DONE=0
  . FOR I=1:1:RUNCOUNT DO  QUIT:DONE=1   
  . . HANG 5   ;"Check every 5 seconds 
  . . IF DEBUGMODE=1 WRITE "."
  . . IF '$$FILEXIST^TMGIOUTL(FILEPATHNAME) SET DONE=1  ;"File is gone an process can continue
  . IF DEBUGMODE=1 WRITE !
  ELSE  DO  GOTO FDDN
  . IF DEBUGMODE=1 WRITE "FILE DOES NOT EXIST.",!
  ;"
  ;"Check to see if the file still exists after the waiting
  IF $$FILEXIST^TMGIOUTL(FILEPATHNAME) DO
  . IF FORCEREMOVE=1 DO
  . . NEW FILEMODDT SET FILEMODDT=$$FILEMOD^TMGKERNL(FILEPATHNAME)
  . . NEW TIMEDIFF SET TIMEDIFF=$$TIMEDIFF^TMGDATE(FILEMODDT,$$TODAY^TMGDATE_"."_$$NOW^TMGDATE)
  . . IF TIMEDIFF>60 DO
  . . . DO DELFILE^TMGIOUTL(FILEPATHNAME)    ;"REMOVE THE FILE IF IT HASN'T BEEN ALTERED IN 60 MINUTES OR MORE
  . . ELSE  DO
  . . . IF DEBUGMODE=1 WRITE "FILE ",FILEPATHNAME," STILL EXISTS AFTER WAITING",!
  . . . SET TMGRESULT="-1^FILE STILL EXISTS"
  ELSE  DO
  . IF DEBUGMODE=1 WRITE "FILE ",FILEPATHNAME," WAS SUCCESSFULLY REMOVED",!
FDDN
  QUIT TMGRESULT
  ;"
SQLREQUEST(SQLSTATEMENT,SQLOUTPATH,SQLOUTNAME,DEBUGMODE,LOGFILE)  ;"
 ;"Purpose: This will be the main function to submit the SQL statement and watch for the 
 ;"         process to finish.
 ;"Input (All strings): SQLSTATEMENT - The SQL query to be ran
 ;"       SQLOUTPATH - The file path to have the data stored in
 ;"       SQLOUTNAME - The file name to have the data stored in
 ;"       DEBUGMODE - 0 or 1
 ;"       LOGFILE - The file path/name to log details for SQL Request
 ;"Output: Results will be 1^SUCCESS or 1^Error Message
 DO LOGLINE(LOGFILE,"BEGINNING SQL QUERY",0,0)
 SET DEBUGMODE=+$G(DEBUGMODE)
 NEW SQLOUTFILE SET SQLOUTFILE=SQLOUTPATH_SQLOUTNAME
 NEW RUNNINGCNT SET RUNNINGCNT=0  ;"Keep track of how many times query was attempted
 NEW TMGRESULT SET TMGRESULT="1^SUCCESS"
 NEW FLAGFILE SET FLAGFILE=$$SQLDIRECTORY+$$SQLRUNNINGFILE
 NEW REQUESTFILE SET REQUESTFILE=$$SQLDIRECTORY_$$SQLREQUESTFILE 
 NEW OUTFILE SET OUTFILE=$$SQLDIRECTORY_SQLOUTNAME
 ;"
 ;"Check for an existing request file
 NEW TMGFILERESULT
 SET TMGFILERESULT=$$FILEWAIT(REQUESTFILE,5,DEBUGMODE)  ;"LOOK FOR ANOTHER PROCESSES REQUEST FILE FIRST
 IF +TMGFILERESULT'=1 DO  GOTO SQLDN 
 . SET TMGRESULT="-1^Another request file existed and was never handled: ("_REQUESTFILE_")"  
 ;"
 ;"Check for active flag file
 SET TMGFILERESULT=$$FILEWAIT(FLAGFILE,5,DEBUGMODE)  ;"LOOK FOR ANOTHER FLAG FILE
 IF +TMGFILERESULT'=1 DO  GOTO SQLDN 
 . SET TMGRESULT="-1^Running file was never removed: ("_FLAGFILE_")"  
 ;"
 ;"No other process is running so we can continue
 ;"Does output file already exist? If so delete.
 IF DEBUGMODE=1 WRITE "CHECKING FOR OUTPUT FILE.",!
 IF $$FILEXIST^TMGIOUTL(SQLOUTFILE) DO
 . IF DEBUGMODE=1 WRITE "OUTPUT FILE FOUND. DELETING.",!
 . DO DELFILE^TMGIOUTL(SQLOUTFILE)
 ;"
 ;"Write out SQL Request file
 IF DEBUGMODE=1 WRITE "WRITING OUT SQL FILE.",!
 NEW TEXTTOSEND SET TEXTTOSEND=SQLSTATEMENT_"^"_SQLOUTFILE
 NEW HANDLE SET HANDLE="TMGSQL"
 NEW PATH SET PATH=$$SQLDIRECTORY
 NEW FILENAME SET FILENAME=$$SQLREQUESTFILE
 DO OPEN^%ZISH(HANDLE,PATH,FILENAME,"W")
 IF POP DO  GOTO SQLDN
 . SET TMGRESULT="-1^Unable to open file for writing: "_PATH_FILENAME
 USE IO
 WRITE TEXTTOSEND
 DO CLOSE^%ZISH(HANDLE)      
 ;"
 ;"Wait for processing to begin (watch for SQLRequest to be removed, which means it is in processing)

 SET TMGFILERESULT=$$FILEWAIT(REQUESTFILE,5,DEBUGMODE)  ;"LOOK FOR OUR REQUEST FILE TO BE ACCEPTED (It will be deleted)
 IF +TMGFILERESULT'=1 DO  GOTO SQLDN 
 . SET TMGRESULT="-1^Our request file was never handled: ("_REQUESTFILE_")"
 ;"
 ;"Wait for output file to exist (watch for SQLOUTFILE to be created)
 ;"
 IF DEBUGMODE=1 WRITE "WAITING FOR OUTPUT FILE TO BE CREATED: ",OUTFILE,!
 IF '$$FILEXIST^TMGIOUTL(OUTFILE) DO  ;"HERE WE WILL WAIT FOR 5 MINUTES FOR CURRENT PROCESS TO FINISH. IF WE HIT 5 MINUTES, SEND ERROR MESSAGE
 . NEW I,DONE SET DONE=0
 . FOR I=1:1:60 DO  QUIT:DONE=1   ;"Run 24 times (which will translate to 5 minutes since it hangs for 15 seconds each check)
 . . IF DEBUGMODE=1 WRITE "."
 . . HANG 5   ;"Check every 5 second 
 . . IF $$FILEXIST^TMGIOUTL(OUTFILE) SET DONE=1  ;"File is gone an process can continue
 . IF DEBUGMODE=1 WRITE !
 HANG 5 ;" ALLOW 5 SECONDS TO ENSURE WRITE IS DONE 
 IF '$$FILEXIST^TMGIOUTL(OUTFILE) DO  GOTO SQLDN
 . IF DEBUGMODE=1 WRITE "TIMED OUT WAITING... QUITTING.",!
 . SET TMGRESULT="-1^TIME OUT. OUTPUT FILE WAS NEVER CREATED. WAITED 5 MINUTES."
SQLDN 
 QUIT TMGRESULT
 ;" 
OPENCHCK()  ;" 
 ;" This function will determine if a process is running during a time our office is considered open
 NEW TMGRESULT SET TMGRESULT=1  ;"DEFAULT TO YES
 ;"
 ;"CHECK THE DAY
 NEW X DO NOW^%DTC
 DO DW^%DTC
 NEW DAYS SET DAYS="MONDAY,TUESDAY,THURSDAY,FRIDAY"
 IF DAYS'[X SET TMGRESULT=0 GOTO OCDN ;"NOT THE PROPER DAY
 ;"
 ;"IF THE APPROPRIATE DAY, CHECK THE HOUR TO ENSURE WE ARE IN THE PROPER TIMEFRAME
 NEW TIME SET TIME=$E($$NOW^TMGDATE,1,2)  ;"GET THE HOUR
 IF (TIME<8)!(TIME>18) SET TMGRESULT=0  ;"DON'T RUN BEFORE 8AM OR AFTER 6PM
OCDN
 QUIT TMGRESULT
 ;"
OPENDAY()  ;" 
 ;" This function will determine if a process is running during a day our office is considered open
 NEW TMGRESULT SET TMGRESULT=1  ;"DEFAULT TO YES
 ;"
 ;"CHECK THE DAY
 NEW X DO NOW^%DTC
 DO DW^%DTC
 NEW DAYS SET DAYS="MONDAY,TUESDAY,THURSDAY,FRIDAY"
 IF DAYS'[X SET TMGRESULT=0 ;"NOT THE PROPER DAY
 QUIT TMGRESULT 
 ;"
 ;"==================================================================================================
 ;"=============================   OPTION ENTRY POINTS ARE BELOW    =================================
 ;"==================================================================================================
GTSCHSTA();"   GET CURRENT SCHEDULE STATUS
 ;"Purpose: Get the current schedule status
 ;"
 IF $$OPENCHCK=0 QUIT   ;" DON'T RUN IF WE AREN'T OPEN
 NEW DATAFILE,LOGFILE
 SET DATAFILE=$$SCHSTATF_$$DATAFILETYPE
 SET LOGFILE=$$SCHSTATF_$$LOGFILETYPE
 ;"
 NEW TMGRESULT
 SET TMGRESULT=$$SQLREQUEST($$SQLSTAT(),$$WINDIR(),DATAFILE,1,LOGFILE)
 IF +TMGRESULT'=1 DO  QUIT
 . NEW ALRTRESULT
 . DO LOGLINE(LOGFILE,"!ERROR WITH SQL QUERY! "_$P(TMGRESULT,"^",2),0,0)
 . DO MAKEALERT("ERROR WITH SCHEDULE STATUS SQL: "_$P(TMGRESULT,"^",2)) 
 . ;"DO INFRMALT^TMGXQAL(.ALRTRESULT,150,"ERROR WITH SCHEDULE STATUS SQL: "_$P(TMGRESULT,"^",2))
 DO LOGLINE(LOGFILE,"SQL QUERY SUCCESSFUL. NOW FILING",0,0)
 DO LOADONE^TMGSSQL4(LOGFILE)
 DO LOGLINE(LOGFILE,"SCHEDULE UPDATE COMPLETE.",0,0)
 QUIT
 ;"
GTFTRSCH();"   GET FUTURE APPOINTMENTS
 ;"Purpose: Get the current schedule status
 ;"
 IF $$OPENCHCK=0 QUIT   ;" DON'T RUN IF WE AREN'T OPEN
 NEW DATAFILE,LOGFILE
 SET DATAFILE=$$FUTSCHDF_$$DATAFILETYPE
 SET LOGFILE=$$FUTSCHDF_$$LOGFILETYPE
 ;"
 NEW TMGRESULT
 SET TMGRESULT=$$SQLREQUEST($$SQLAPPT(),$$WINDIR(),DATAFILE,1,LOGFILE)
 IF +TMGRESULT'=1 DO  QUIT
 . NEW ALRTRESULT
 . DO LOGLINE(LOGFILE,"!ERROR WITH SQL QUERY! "_$P(TMGRESULT,"^",2),0,0)
 . DO MAKEALERT("ERROR WITH FUTURE SCHEDULE SQL: "_$P(TMGRESULT,"^",2))
 . ;"DO INFRMALT^TMGXQAL(.ALRTRESULT,150,"ERROR WITH FUTURE SCHEDULE SQL: "_$P(TMGRESULT,"^",2))
 DO LOGLINE(LOGFILE,"SQL QUERY SUCCESSFUL. NOW FILING",0,0)
 DO HNDLTASK^TMGSSQL5(LOGFILE)	 
 DO LOGLINE(LOGFILE,"SCHEDULE UPDATE COMPLETE.",0,0)
 QUIT
 ;" 
GTPTDEMO()  ;"   GET PATIENT DEMOGRAPHICS 
 ;"Purpose: Get the patient demographics
 ;"
 IF $$OPENDAY=0 QUIT   ;" DON'T RUN IF WE AREN'T OPEN 
 NEW TMGRESULT
 NEW DATAFILE,LOGFILE
 SET DATAFILE=$$PTDEMOF_$$DATAFILETYPE
 SET LOGFILE=$$PTDEMOF_$$LOGFILETYPE
 ;"
 SET TMGRESULT=$$SQLREQUEST($$SQLDEMO(),$$WINDIR(),DATAFILE,1,LOGFILE)
 IF +TMGRESULT'=1 DO  QUIT
 . NEW ALRTRESULT
 . DO LOGLINE(LOGFILE,"!ERROR WITH SQL QUERY! "_$P(TMGRESULT,"^",2),0,0)
 . DO MAKEALERT("ERROR WITH PATIENT DEMO SQL: "_$P(TMGRESULT,"^",2))
 . ;"DO INFRMALT^TMGXQAL(.ALRTRESULT,150,"ERROR WITH PATIENT DEMO SQL: "_$P(TMGRESULT,"^",2))
 DO LOGLINE(LOGFILE,"SQL QUERY SUCCESSFUL. NOW FILING",0,0)
 DO RUNNOW^TMGSSQL2(LOGFILE)	 
 QUIT
 ;" 
GTPTENC()  ;"   GET PATIENT ENCOUNTER INFO (CPTS AND ICDS) 
 ;"Purpose: Get the patient demographics
 ;"
 IF $$OPENDAY=0 QUIT   ;" DON'T RUN IF WE AREN'T OPEN 
 NEW DATAFILE,LOGFILE
 SET DATAFILE=$$ENCOUNTF_$$DATAFILETYPE
 SET LOGFILE=$$ENCOUNTF_$$LOGFILETYPE
 ;"
 NEW TMGRESULT
 SET TMGRESULT=$$SQLREQUEST($$SQLENC(),$$WINDIR(),DATAFILE,1,LOGFILE)
 IF +TMGRESULT'=1 DO  QUIT
 . NEW ALRTRESULT
 . DO LOGLINE(LOGFILE,"!ERROR WITH SQL QUERY! "_$P(TMGRESULT,"^",2),0,0)
 . DO MAKEALERT("ERROR WITH ENCOUNTER SQL: "_$P(TMGRESULT,"^",2))
 . ;"DO INFRMALT^TMGXQAL(.ALRTRESULT,150,"ERROR WITH ENCOUNTER SQL: "_$P(TMGRESULT,"^",2))
 DO LOGLINE(LOGFILE,"SQL QUERY SUCCESSFUL. NOW FILING",0,0)
 DO IMPCPTS^TMGSSQL3(LOGFILE)	 
 QUIT
 ;" 
GTSCH4BC(TMGRESULT,DATE)  ;"
 NEW DATAFILE,LOGFILE
 SET DATAFILE=$$RANDOMF_$$DATAFILETYPE
 SET LOGFILE=$$RANDOMF_$$LOGFILETYPE
 ;"
 SET TMGRESULT=$$SQLREQUEST($$SQLFORMS(DATE),$$WINDIR(),DATAFILE,0,LOGFILE)
 IF +TMGRESULT'=1 DO  QUIT
 . NEW ALRTRESULT
 . DO LOGLINE(LOGFILE,"!ERROR WITH SQL QUERY! "_$P(TMGRESULT,"^",2),0,0)
 . ;"DO MAKEALERT("ERROR WITH ENCOUNTER SQL: "_$P(TMGRESULT,"^",2))
 . ;"DO INFRMALT^TMGXQAL(.ALRTRESULT,150,"ERROR WITH ENCOUNTER SQL: "_$P(TMGRESULT,"^",2))
 DO LOGLINE(LOGFILE,"SQL QUERY SUCCESSFUL. NOW RETURNING",0,0)
 SET TMGRESULT=$$MOVE^TMGKERNL($$SQLDIRECTORY()_DATAFILE,"/mnt/WinPublic/vista/Scanner/Documents/appointments1.csv") 
 QUIT
 ;" 
VALIDDT(DATE,OUTSQL,OUTFM) ;"
 ;"CHECK DATE TO ENSURE IT IS VALID. IF SO RETURN IN BOTH SQL FORMAT AND FM FORMAT
 SET OUTSQL=0,OUTFM=0
 NEW TMGRESULT
 NEW %DT,X,Y
 SET %DT="E",X=DATE
 DO ^%DT
 SET OUTFM=Y
 SET TMGRESULT=OUTFM>0
 IF TMGRESULT>0 DO
 . NEW MM,DD,YYYY
 . SET MM=$E(OUTFM,4,5)
 . SET DD=$E(OUTFM,6,7)
 . SET YYYY=1700+$E(OUTFM,1,3)
 . SET OUTSQL=MM_"-"_DD_"-"_YYYY
 QUIT TMGRESULT
 ;"
BILITEMS()  ;"THIS WILL GET ALL THE BILLABLE ITEMS AND COMPARE TO WHAT HAS ALREADY BEEN POSTED
 NEW SQLDATE,FMDATE,INDATE,VALID
 SET VALID=0
 FOR  DO  QUIT:VALID=1
 . WRITE !,"Enter a date (MM/DD/YYYY): "
 . READ INDATE
 . IF INDATE="^" SET VALID=1 QUIT
 . SET VALID=$$VALIDDT(INDATE,.SQLDATE,.FMDATE)
 . IF VALID=1 QUIT
 . ELSE  WRITE "Invalid date format. Please try again.",!
 IF INDATE="^" QUIT
 ;"
 ;"W "INDATE=",INDATE,!,"SQLDATE=",SQLDATE,!,"FMDATE=",FMDATE,!
 ;"QUIT
 NEW DATAFILE,LOGFILE
 SET DATAFILE=$$RANDOMF_$$DATAFILETYPE
 SET LOGFILE=$$RANDOMF_$$LOGFILETYPE
 ;"
 WRITE !,"REQUESTING INFORMATION FROM SEQUEL",!
 SET TMGRESULT=$$SQLREQUEST($$BITEMSQL(SQLDATE),$$WINDIR(),DATAFILE,0,LOGFILE)
 IF +TMGRESULT'=1 DO  QUIT
 . NEW ALRTRESULT
 . DO LOGLINE(LOGFILE,"!ERROR WITH SQL QUERY! "_$P(TMGRESULT,"^",2),0,0)
 . ;"DO MAKEALERT("ERROR WITH ENCOUNTER SQL: "_$P(TMGRESULT,"^",2))
 . ;"DO INFRMALT^TMGXQAL(.ALRTRESULT,150,"ERROR WITH ENCOUNTER SQL: "_$P(TMGRESULT,"^",2))
 DO LOGLINE(LOGFILE,"SQL QUERY SUCCESSFUL. NOW RETURNING",0,0)
 NEW FULLPNAME,TMGARRAY SET FULLPNAME=$$SQLDIRECTORY^TMGSSQL1()_DATAFILE
 DO LCSV2ARR^TMGIOUT4(FULLPNAME,"TMGARRAY")
 NEW RSLT SET RSLT=$$RMFILE^TMGKERNL($$SQLDIRECTORY()_DATAFILE)
 ;"
 NEW BILLEDARR
 NEW IDX SET IDX=0
 FOR  SET IDX=$O(TMGARRAY(IDX)) QUIT:IDX'>0  DO
 . NEW CPT,ACCT
 . SET CPT=$G(TMGARRAY(IDX,3)),ICD=$G(TMGARRAY(IDX,4))
 . SET ACCT=$G(TMGARRAY(IDX,1))
 . SET BILLEDARR(ACCT,"CPT",CPT)=ICD
 ;"TMGARRAY NOW CONTAINS ALL THE ENTERED CHARGES FROM SEQUELMED
 ;"GET BILLABLE ITEMS
 ;" !!!!  EDDIE - THIS NEEDS TO BE REDONE TO USE ENTERED DATE"
 NEW BILLITEMS DO BILLRPC^TMGRPT2(.BILLITEMS,FMDATE,FMDATE,4)
 ;"NEW BILLITEMS DO BILLRPC^TMGRPT2(.BILLITEMS,3250325,3250325,4)
 ;"
 ;"ZWR BILLITEMS
 NEW ACCT SET ACCT=0
 FOR  SET ACCT=$O(BILLITEMS(ACCT)) QUIT:ACCT'>0  DO
 . NEW TMGDFN SET TMGDFN=+$O(^DPT("TMGS",ACCT,0))
 . NEW PTNAME SET PTNAME=$P($G(^DPT(TMGDFN,0)),"^",1)
 . NEW NAMESHOWN SET NAMESHOWN=0
 . ;"WRITE "== ACCT ",ACCT," ",PTNAME," ==",!
 . NEW CPT SET CPT=0
 . FOR  SET CPT=$O(BILLITEMS(ACCT,"CPT",CPT)) QUIT:CPT=""  DO
 . . ;"WRITE " CHECKING FOR CPT ",CPT
 . . IF ($D(BILLEDARR(ACCT,"CPT",CPT)))!($D(BILLEDARR(ACCT,"CPT","""CPT"""))) DO 
 . . . ;"WRITE ". It is billed",!
 . . ELSE  DO
 . . . IF NAMESHOWN=0 DO
 . . . . WRITE "== ACCT ",ACCT," ",PTNAME," ==",!
 . . . . SET NAMESHOWN=1
 . . . WRITE "CPT ",CPT," -- NOT BILLED!!!!",!
 QUIT
 ;" 
 ;"==================================================================================================
 ;"===================================   MAIN LOG ROUTINES    =======================================
 ;"==================================================================================================
 ;"
LOGLINE(LOGFILE,LINE,CLEARFILE,NODTSTAMP) ;"
 ;"This routine will log the sent in LINE to the LOGFILE, which should be FILENAME only
 ;"CLEARFILE - 0 OR 1, if 0 LOG will APPEND. if 1 LOG will CLEAR file
 ;"NODTSTAMP - 0 OR 1, will add a date/time stamp to each line unless this is 1
 SET CLEARFILE=+$G(CLEARFILE)
 SET NODTSTAMP=+$G(NODTSTAMP)
 NEW PREFIX SET PREFIX=$S(NODTSTAMP=0:"["_$$HTE^XLFDT($horolog,"5Z")_"] ",NODTSTAMP=1:"")
 NEW MODE SET MODE=$S(CLEARFILE=0:"A",CLEARFILE=1:"W")
 NEW HANDLE SET HANDLE="TMGSQL"
 NEW PATH SET PATH=$$LOGDIRECTORY
 NEW FILENAME SET FILENAME=LOGFILE
 DO OPEN^%ZISH(HANDLE,PATH,FILENAME,MODE)
 IF POP DO  GOTO SQLDN
 . SET TMGRESULT="-1^Unable to open file for writing: "_PATH_FILENAME
 USE IO
 WRITE PREFIX,LINE,!
 DO CLOSE^%ZISH(HANDLE)   
 QUIT
 ;"
MAKEALERT(MESSAGE)  ;"ONLY ADD AN ALERT IF IT DOESN'T EXIST
 NEW ALRTEXISTS,ALRTRESULT SET ALRTEXISTS=0
 NEW TMGDUZ SET TMGDUZ=150 ;"SET TO EDDIE
 ;
 NEW ALRTDT SET ALRTDT=0
 FOR  SET ALRTDT=$O(^XTV(8992,TMGDUZ,"XQA",ALRTDT)) QUIT:(ALRTDT="")!(ALRTEXISTS=1)  DO
 . NEW ONEMSG SET ONEMSG=$G(^XTV(8992,TMGDUZ,"XQA",ALRTDT,0))
 . SET ONEMSG=$P(ONEMSG,"^",3)
 . IF ONEMSG=MESSAGE SET ALRTEXISTS=1
 IF ALRTEXISTS=0 DO INFRMALT^TMGXQAL(.ALRTRESULT,TMGDUZ,MESSAGE)
 QUIT
 ;"
RUNSQL(TMGRESULT,SQL,FILEPATH,FILENAME)   ;"RPC: TMG SEQUEL RUN SQL
  ;" !!!!  NOTE:  FOR FUTURE USE, IF NEEDED. THIS FUNCTION HAS NOT BEEN IMPLEMENTED NOR TESTED. !!!!
 ;"Entry Point Run random SQL query
 ;"TMGRESULT: Results from query. 1^SUCCESS (file should exist) or -1^ERROR MESSAGE
 ;"SQL: SQL query to run
 ;"FILEPATH: Filepath to store the file
 ;"FILENAME: Filename to store the file in
 SET TMGRESULT="1^FILE IS STORED IN: "_FILEPATH_FILENAME
 NEW DATAFILE,LOGFILE
 SET DATAFILE=$$RANDOMF_$$DATAFILETYPE
 SET LOGFILE=$$RANDOMF_$$LOGFILETYPE
 ;"
 NEW TMGRESULT
 SET TMGRESULT=$$SQLREQUEST($$SQLENC(),$$WINDIR(),DATAFILE,1,LOGFILE)
 IF +TMGRESULT'=1 DO  QUIT
 . NEW ALRTRESULT
 . DO LOGLINE(LOGFILE,"!ERROR WITH SQL QUERY! "_$P(TMGRESULT,"^",2),0,0)
 . SET TMGRESULT="-1^ERROR WITH SQL QUERY: "_$P(TMGRESULT,"^",2)
 
 DO LOGLINE(LOGFILE,"SQL QUERY SUCCESSFUL. NOW FILING",0,0)
 QUIT
 ;"
 ;"==================================================================================================
 ;"===============================   FOLLOWUP BUCKET  ROUTINES    ===================================
 ;"================================================================================================== 
FUBUCKET()  ;"GET FOLLOWUP BUCKET ITEMS AND PRINT REPORT
 NEW DATAFILE,LOGFILE
 SET DATAFILE=$$RANDOMF_$$DATAFILETYPE
 SET LOGFILE=$$RANDOMF_$$LOGFILETYPE
 ;"
 ;" START A MENU TO ASK REPORT TYPE
 NEW MENU,IDX,USRPICK
 ;
 SET IDX=0   
 SET MENU(IDX)="Select Followup Bucket Report Type"
 SET MENU($I(IDX))="All claims by age"_$CHAR(9)_"ByAge"
 SET MENU($I(IDX))="All claims by total balance"_$CHAR(9)_"ByBalance"
 SET MENU($I(IDX))="High dollar claims (over $25 due)"_$CHAR(9)_"HighBalance"
 SET MENU($I(IDX))="Balance trend"_$CHAR(9)_"BalanceTrend"
 SET MENU($I(IDX))="Submitted > 3 times"_$CHAR(9)_"Submitted"
 WRITE !
 SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
 IF (USRPICK="^")!(USRPICK="") QUIT
 IF USRPICK="ByAge" DO
 . NEW DAYS
 . SET DAYS=0
 . FOR  DO  QUIT:(+DAYS>0)!(DAYS="^")
 . . WRITE !,"Enter an age, in days: "
 . . READ DAYS
 . . IF DAYS="^" QUIT
 . . IF +DAYS'>0 WRITE !,"Invalid number of days. Please try again.",!
 . IF DAYS="^" QUIT
 . WRITE !
 . DO BYAGE(DAYS)
 IF USRPICK="HighBalance" DO
 . NEW DOLLARAMT
 . SET DOLLARAMT=0
 . FOR  DO  QUIT:(+DOLLARAMT>0)!(DOLLARAMT="^")
 . . WRITE !,"Enter dollar amount: "
 . . READ DOLLARAMT
 . . IF DOLLARAMT="^" QUIT
 . . IF +DOLLARAMT'>0 WRITE !,"Invalid dollar amount. Please try again.",!
 . IF DOLLARAMT="^" QUIT
 . WRITE !
 . DO HIGHBAL(DOLLARAMT)
 IF USRPICK="ByBalance" DO
 . WRITE "Not done yet",!
 IF USRPICK="BalanceTrend" DO
 . WRITE "Not done yet",!
 IF USRPICK="Submitted" DO
 . WRITE "Not done yet",!
 QUIT
  ;" 
BYAGE(DAYS) 
 SET TMGRESULT=$$SQLREQUEST($$FBUCKSQL(),$$WINDIR(),DATAFILE,0,LOGFILE)
 IF +TMGRESULT'=1 DO  QUIT
 . NEW ALRTRESULT
 . DO LOGLINE(LOGFILE,"!ERROR WITH SQL QUERY! "_$P(TMGRESULT,"^",2),0,0)
 . ;"DO MAKEALERT("ERROR WITH ENCOUNTER SQL: "_$P(TMGRESULT,"^",2))
 . ;"DO INFRMALT^TMGXQAL(.ALRTRESULT,150,"ERROR WITH ENCOUNTER SQL: "_$P(TMGRESULT,"^",2))
 DO LOGLINE(LOGFILE,"SQL QUERY SUCCESSFUL. NOW RETURNING",0,0) 	 
 ;"
 NEW FULLPNAME,TMGARRAY SET FULLPNAME=$$SQLDIRECTORY^TMGSSQL1()_DATAFILE
 DO LCSV2ARR^TMGIOUT4(FULLPNAME,"TMGARRAY")
 NEW RSLT SET RSLT=$$RMFILE^TMGKERNL($$SQLDIRECTORY()_DATAFILE)
 NEW IDX SET IDX=0
 NEW OUTARR,OLDVISIT,PREVNARVISIT
 FOR  SET IDX=$O(TMGARRAY(IDX)) QUIT:IDX'>0  DO
 . NEW SUBIDX SET SUBIDX=0
 . NEW VISITNUM SET VISITNUM=$G(TMGARRAY(IDX,1))
 . NEW DOS SET DOS=$G(TMGARRAY(IDX,7))
 . NEW DAYSDIFF SET DAYSDIFF=$$DAYSDIFF^TMGDATE($$TODAY^TMGDATE(),$$INTDATE^TMGDATE(DOS))
 . SET VISITNUM=VISITNUM_" DATE OF SERVICE: "_DOS
 . NEW ACCOUNT SET ACCOUNT=$G(TMGARRAY(IDX,3))
 . NEW CPT SET CPT=$G(TMGARRAY(IDX,8))
 . NEW MOD1,MOD2 SET MOD1=$G(TMGARRAY(IDX,9)),MOD2=$G(TMGARRAY(IDX,10))
 . IF MOD1'="" SET CPT=CPT_"-"_MOD1
 . IF MOD2'="" SET CPT=CPT_"&"_MOD2
 . NEW PATIENT SET PATIENT=$G(TMGARRAY(IDX,4))_","_$G(TMGARRAY(IDX,5))_" ("_$G(TMGARRAY(IDX,6))_") - "_ACCOUNT
 . NEW BALANCE SET BALANCE=$P($G(TMGARRAY(IDX,11)),$C(13),1)
 . ;"  NOTE! I added DAYSDIFF so it would arrange by the oldest one first  4/21/25
 . IF CPT["90677" SET PREVNARVISIT(DAYSDIFF,PATIENT,VISITNUM,CPT)=BALANCE
 . ELSE  IF DAYSDIFF>DAYS SET OLDVISIT(DAYSDIFF,PATIENT,VISITNUM,CPT)=BALANCE
 . ELSE  SET OUTARR(DAYSDIFF,PATIENT,VISITNUM,CPT)=BALANCE
 ;"
 ;" NOW ADJUST PREVNAR ARRAYS
 NEW DAYSDIFF SET DAYSDIFF=0
 FOR  SET DAYSDIFF=$O(PREVNARVISIT(DAYSDIFF)) QUIT:DAYSDIFF'>0  DO
 . NEW PATIENT SET PATIENT=""
 . FOR  SET PATIENT=$O(PREVNARVISIT(DAYSDIFF,PATIENT)) QUIT:PATIENT=""  DO
 . . NEW VISITNUM SET VISITNUM=""
 . . FOR  SET VISITNUM=$O(PREVNARVISIT(DAYSDIFF,PATIENT,VISITNUM)) QUIT:VISITNUM=""  DO
 . . . IF $D(OLDVISIT(DAYSDIFF,PATIENT,VISITNUM)) DO
 . . . . MERGE PREVNARVISIT(DAYSDIFF,PATIENT,VISITNUM)=OLDVISIT(DAYSDIFF,PATIENT,VISITNUM)
 . . . . KILL OLDVISIT(DAYSDIFF,PATIENT,VISITNUM)
 ;"
 NEW %ZIS,IOP
 SET IOP="S121-LAUGHLIN-LASER"
 DO ^%ZIS  ;"standard device call
 IF POP QUIT
 USE IO
 ;"
 WRITE !
 WRITE "************************************************************",!
 WRITE "           Sequel Followup Bucket Report, By Age (>",DAYS," days old)",!
 WRITE "                     " SET Y=X DO DD^%DT WRITE Y,!
 WRITE "              Please deliver this report to Lindsey",!
 WRITE "************************************************************",!
 WRITE "                                            (From TMGSSQL1.m)",!!
 IF $D(PREVNARVISIT) DO
 . WRITE !,"**** VISITS WITH PREVNAR IMMUNIZATIONS - HIGH PRIORITY!!!! ****",!
 . DO WRITEARR(.PREVNARVISIT)
 WRITE !,"***********************************"
 WRITE !,"**** VISITS OLDER THAN ",DAYS," DAYS ****"
 WRITE !,"***********************************",!
 DO WRITEARR(.OLDVISIT)
 DO ^%ZISC  ;" Close the output device
 QUIT
 ;"
WRITEARR(OUTARR)  ;"OUTPUT FOLLOWUP BUCKET ARRAY
 NEW DAYSDIFF SET DAYSDIFF=999999
 FOR  SET DAYSDIFF=$O(OUTARR(DAYSDIFF),-1) QUIT:DAYSDIFF'>0  DO 
 . NEW PATIENT SET PATIENT=""
 . FOR  SET PATIENT=$O(OUTARR(DAYSDIFF,PATIENT)) QUIT:PATIENT=""  DO
 . . WRITE "  ==== ",PATIENT," ",DAYSDIFF," DAYS OLD ====",!
 . . NEW VISITNUM SET VISITNUM=""
 . . FOR  SET VISITNUM=$O(OUTARR(DAYSDIFF,PATIENT,VISITNUM)) QUIT:VISITNUM=""  DO
 . . . WRITE "    VISIT NUMBER -> ",VISITNUM," !! ",$G(IMPORTANT(VISITNUM)),!
 . . . NEW CPT SET CPT=""
 . . . FOR  SET CPT=$O(OUTARR(DAYSDIFF,PATIENT,VISITNUM,CPT)) QUIT:CPT=""  DO
 . . . . WRITE "       ",CPT," WITH BALANCE OF $",$G(OUTARR(DAYSDIFF,PATIENT,VISITNUM,CPT)),!
 . . . WRITE !
 . . WRITE !
 QUIT  
 ;"=============================================================
 ;"
HIGHBAL(BALANCECUTOFF)
 SET TMGRESULT=$$SQLREQUEST($$FBUCKSQL(),$$WINDIR(),DATAFILE,0,LOGFILE)
 IF +TMGRESULT'=1 DO  QUIT
 . NEW ALRTRESULT
 . DO LOGLINE(LOGFILE,"!ERROR WITH SQL QUERY! "_$P(TMGRESULT,"^",2),0,0)
 . ;"DO MAKEALERT("ERROR WITH ENCOUNTER SQL: "_$P(TMGRESULT,"^",2))
 . ;"DO INFRMALT^TMGXQAL(.ALRTRESULT,150,"ERROR WITH ENCOUNTER SQL: "_$P(TMGRESULT,"^",2))
 DO LOGLINE(LOGFILE,"SQL QUERY SUCCESSFUL. NOW RETURNING",0,0) 	 
 ;"
 NEW FULLPNAME,TMGARRAY SET FULLPNAME=$$SQLDIRECTORY^TMGSSQL1()_DATAFILE
 DO LCSV2ARR^TMGIOUT4(FULLPNAME,"TMGARRAY")
 NEW RSLT SET RSLT=$$RMFILE^TMGKERNL($$SQLDIRECTORY()_DATAFILE)
 NEW IDX SET IDX=0
 NEW OUTARR,OLDVISIT,PREVNARVISIT
 FOR  SET IDX=$O(TMGARRAY(IDX)) QUIT:IDX'>0  DO
 . NEW SUBIDX SET SUBIDX=0
 . NEW VISITNUM SET VISITNUM=$G(TMGARRAY(IDX,1))
 . NEW DOS SET DOS=$G(TMGARRAY(IDX,7))
 . NEW DAYSDIFF SET DAYSDIFF=$$DAYSDIFF^TMGDATE($$TODAY^TMGDATE(),$$INTDATE^TMGDATE(DOS))
 . SET VISITNUM=VISITNUM_" DATE OF SERVICE: "_DOS
 . NEW ACCOUNT SET ACCOUNT=$G(TMGARRAY(IDX,3))
 . NEW CPT SET CPT=$G(TMGARRAY(IDX,8))
 . NEW MOD1,MOD2 SET MOD1=$G(TMGARRAY(IDX,9)),MOD2=$G(TMGARRAY(IDX,10))
 . IF MOD1'="" SET CPT=CPT_"-"_MOD1
 . IF MOD2'="" SET CPT=CPT_"&"_MOD2
 . NEW PATIENT SET PATIENT=$G(TMGARRAY(IDX,4))_","_$G(TMGARRAY(IDX,5))_" ("_$G(TMGARRAY(IDX,6))_") - "_ACCOUNT
 . NEW BALANCE SET BALANCE=$P($G(TMGARRAY(IDX,11)),$C(13),1)
 . IF CPT["90677" SET PREVNARVISIT(DAYSDIFF,PATIENT,VISITNUM,CPT)=BALANCE
 . ELSE  IF BALANCE>BALANCECUTOFF SET OLDVISIT(DAYSDIFF,PATIENT,VISITNUM,CPT)=BALANCE
 . ELSE  SET OUTARR(DAYSDIFF,PATIENT,VISITNUM,CPT)=BALANCE
 ;"
 ;" NOW ADJUST PREVNAR ARRAYS
 NEW DAYSDIFF SET DAYSDIFF=0
 FOR  SET DAYSDIFF=$O(PREVNARVISIT(DAYSDIFF)) QUIT:DAYSDIFF'>0  DO
 . NEW PATIENT SET PATIENT=""
 . FOR  SET PATIENT=$O(PREVNARVISIT(DAYSDIFF,PATIENT)) QUIT:PATIENT=""  DO
 . . NEW VISITNUM SET VISITNUM=""
 . . FOR  SET VISITNUM=$O(PREVNARVISIT(DAYSDIFF,PATIENT,VISITNUM)) QUIT:VISITNUM=""  DO
 . . . IF $D(OLDVISIT(DAYSDIFF,PATIENT,VISITNUM)) DO
 . . . . MERGE PREVNARVISIT(DAYSDIFF,PATIENT,VISITNUM)=OLDVISIT(DAYSDIFF,PATIENT,VISITNUM)
 . . . . KILL OLDVISIT(DAYSDIFF,PATIENT,VISITNUM)
 ;"
 NEW %ZIS,IOP
 SET IOP="S121-LAUGHLIN-LASER"
 DO ^%ZIS  ;"standard device call
 IF POP QUIT
 USE IO
 ;"
 WRITE !
 WRITE "************************************************************",!
 WRITE "           Sequel Followup Bucket Report, High Balance (>$",BALANCECUTOFF,")",!
 WRITE "                     " SET Y=X DO DD^%DT WRITE Y,!
 WRITE "              Please deliver this report to Lindsey",!
 WRITE "************************************************************",!
 WRITE "                                            (From TMGSSQL1.m)",!!
 IF $D(PREVNARVISIT) DO
 . WRITE !,"**** VISITS WITH PREVNAR IMMUNIZATIONS - HIGH PRIORITY!!!! ****",!
 . DO WRITEARR(.PREVNARVISIT)
 WRITE !,"***********************************"
 WRITE !,"**** VISITS WITH BALANCES OVER $",BALANCECUTOFF," ****"
 WRITE !,"***********************************",!
 DO WRITEARR(.OLDVISIT)
 DO ^%ZISC  ;" Close the output device
 QUIT
 ;"  
 