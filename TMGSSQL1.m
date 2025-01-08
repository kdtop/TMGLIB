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
 