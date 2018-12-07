TMGSEQL4 ;TMG/kst/Interface with SequelSystems PMS for ICD codes ;8/1/12, 2/2/14
         ;;1.0;TMG-LIB;**1**;8/1/12

 ;"TMG SEQUEL ICD DIAGNOSIS CODES IMPORT FUNCTIONS
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
 ;"ASKIMPORT
 ;"RUNNOW  -- provide an entry point for running import NOW.  This will delete prior alerts
 ;"AUTOIN  -- entry point for scheduled task
 ;"QUIETIN
 ;"$$IMPORTFILE(FILEPATH,FILENAME,ERRARRAY,PROGFN,DELFILES,USERID,BADICDLIST)
 ;"$$IMPORTGLOBAL(GREF,ERRARRAY,PROGFN,USERID,BADICDLIST)
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"$$PROCESLN(ONELINE,ERRARRAY,DUZ,PARSEFN,BADICDLIST)
 ;"$$PARSELN(ONELINE,ARRAY,BADICDLIST)
 ;"UPDATEDB(PCEINFO,ERRARRAY)
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"TMGIOUTL
 ;"TMGMISC
 ;"=======================================================================
 ;"=======================================================================
 ;
DEBUGIN ;
        NEW TMPNAME
        NEW DEFPATH SET DEFPATH="/tmp/"
        NEW DEFFNAME SET DEFFNAME="demographics.csv"
        NEW FPATH,FNAME
        NEW ERRARRAY
        NEW RESULT
        ;
        SET TMPNAME=$$GETFNAME^TMGIOUTL("Please enter file to import.",.DEFPATH,.DEFFNAME,,.FPATH,.FNAME)
        IF TMPNAME="" GOTO AIDONE
        SET DEFPATH=FPATH
        ;
        NEW FILEPATH SET FILEPATH=FPATH
        NEW FILENAME SET FILENAME=FNAME
        NEW ERRARRAY
        NEW DELFILES
        NEW GREF,GREF1
        NEW RESULT
        ;
        SET GREF=$NAME(^TMP("TMG",$J,"SEQUELIMPORT","DATA",1))   ;"I use this to process array
        SET GREF1=$NAME(@GREF@(1))                   ;"I use this to load file
        KILL @GREF
        SET RESULT=$$FTG^%ZISH(FILEPATH,FILENAME,GREF1,6)  ;"load file into a global
        IF RESULT=0 GOTO IFDONE
        NEW USERID SET USERID=$GET(USERID,+$GET(DUZ))
        ;
        NEW TMGINVALID ;"Will be used as a globally-scoped variable in the module
        SET RESULT=1
        NEW DELAY SET DELAY=0
        NEW TMGCUR,TMGMAX,TMGSTART,TMGABORT ;"avail for PROGFN function
        SET TMGABORT=0
        SET TMGMAX=+$ORDER(@GREF@(""),-1)
        SET TMGSTART=$H  ;"store starting time.
        ;
        ;"---- finish conversion below....

        ;"Set up selector array
        ;" NEW TMGSel,TMGPick
        ;" NEW i SET i=0
        ;" FOR  SET i=$ORDER(@GREF@(i)) QUIT:i=""  DO
        ;" . NEW ONELINE SET ONELINE=$GET(@GREF@(i))
        ;" . NEW ID SET ID=$PIECE(ONELINE,",",1)
        ;" . NEW LName SET LName=$PIECE(ONELINE,",",3)
        ;" . NEW FNAME SET FNAME=$PIECE(ONELINE,",",4)
        ;" . SET TMGSel(LName_","_FNAME_" ("_ID_")",i)=""
        ;"
        ;" DO SELECTR2^TMGUSRI3("TMGSel","TMGPick","Pick Entries to Work On")
        ;"
        ;" NEW TMGDisplay SET TMGDisplay=""
        ;" FOR  SET TMGDisplay=$ORDER(TMGPick(TMGDisplay)) QUIT:TMGDisplay=""  DO
        ;" . NEW I SET I=0
        ;" . FOR  SET I=$ORDER(TMGPick(TMGDisplay,I)) QUIT:I=""  DO
        ;" . . NEW ONELINE SET ONELINE=$GET(@GREF@(I))
        ;" . . SET RESULT=$$PROCESLN(ONELINE,.ERRARRAY,USERID)
        ;"
        ;" KILL @GREF
        ;" KILL @G2Ref
        ;"
        ;" DO  ;"record the current time as the time of last import
        ;" . DO NOW^%DTC
        ;" . NEW TMGFDA,TMGMSG
        ;" . SET TMGFDA(22711,"1,",4)=%  ;"#4 = LAST IMPORT DATE
        ;" . DO FILE^DIE("E","TMGFDA","TMGMSG")  ;" note: ignores TMGMSG or errors.
        ;"
        QUIT
        ;
        ;
ASKIMPORT  ;
        ;"Purpose: To ask user for filename and then import ICD data.
        ;"Input: None
        ;"Output: Database is updated with data from file.
        ;"Result: None
        ;
        NEW TMPNAME,TMGKEYIN
        NEW DEFPATH SET DEFPATH="/mnt/WinServer/"
        NEW DEFFNAME SET DEFFNAME="ICD_By_Patient.csv"
        NEW FPATH,FNAME,ERRARRAY,RESULT
        NEW PROGFN SET PROGFN="DO PROGBAR^TMGUSRI2(TMGCUR,""Progress"",1,TMGMAX,,TMGSTART)"
        SET PROGFN=PROGFN_" READ *TMGKEYIN:0 SET:(TMGKEYIN=27) TMGABORT=1"
        SET TMPNAME=$$GETFNAME^TMGIOUTL("Please enter file to import.",.DEFPATH,.DEFFNAME,,.FPATH,.FNAME)
        IF TMPNAME="" GOTO AIDONE
        SET DEFPATH=FPATH
        NEW BADICDLIST
        WRITE !,!,"Importing ICD-9 codes into problem list.",!
        WRITE "Running this multiple times will NOT lead to",!
        WRITE "duplicate entries",!,!
        SET RESULT=$$IMPORTFILE(FPATH,FNAME,.ERRARRAY,PROGFN,,,.BADICDLIST)
        IF $DATA(BADICDLIST) DO
        . WRITE "Here is a list of missing ICD-9 codes in the VistA system.",!
        . DO ZWRITE^TMGZWR("BADICDLIST")
AIDONE  QUIT
        ;
        ;
RUNNOW  ;
        ;"Purpose: To provide an entry point for running import NOW.  This will delete prior alerts
        ;"Input: none.  Settings stored in File 22711 are used
        ;"Output: None.  Progress shown to console.  The database should be updated
        ;"Results: none
        ;
        WRITE !!,"Importing Sequel ICD Diagnosis codes now...",!
        NEW FNAME SET FNAME=$$GET1^DIQ(22711,"1,","IMPORT ICD DATAFILE NAME")
        NEW FPATH SET FPATH=$$GET1^DIQ(22711,"1,","IMPORT DATAFILE PATH")
        NEW DELFILES SET DELFILES=+$$GET1^DIQ(22711,"1,","DELETE DATAFILE AFTER IMPORT?","I")
        NEW USERID SET USERID=$$GET1^DIQ(22711,"1,","ALERT RECIPIENT","I")
        NEW PROGFN SET PROGFN="DO PROGBAR^TMGUSRI2(TMGCUR,""Progress"",1,TMGMAX,,TMGSTART)"
        SET PROGFN=PROGFN_" READ *TMGKEYIN:0 SET:(TMGKEYIN=27) TMGABORT=1"
        NEW BADICDLIST
        NEW RESULT SET RESULT=$$IMPORTFILE(FPATH,FNAME,,PROGFN,DELFILES,USERID,.BADICDLIST)
        QUIT
        ;
AUTOIN  ;
        ;"Purpose: To provide an entry point for a scheduled task.  This will delete prior alerts
        ;"Input: none.  Settings stored in File 22711 are used
        ;"Output: None.  There should be no console output.  The database should be updated
        ;"Results: none
        ;
        NEW INITTIME SET INITTIME=$H
        NEW USERID SET USERID=$$GET1^DIQ(22711,"1,","ALERT RECIPIENT","I")
        ;
        DO  ;"clear out 'next run task number'
        . NEW TMGFDA,TMGMSG
        . SET TMGFDA(22711,"1,",8)="@"  ;"#4 = TASK FOR NEXT RUN
        . DO FILE^DIE("E","TMGFDA","TMGMSG")  ;" note: ignores TMGMSG or errors.
        ;
        NEW TEMP SET TEMP=$$QuietClear^TMGSEQL3(USERID,"TMGSQLICDIMPORT")  ;"clear prior alerts & errors
        DO QUIETIN  ;" DO import
        ;
        ;"Here I schedule the next task to run again.
        NEW HRINT SET HRINT=$$GET1^DIQ(22711,"1,","IMPORT FREQUENCY (IN HOURS)","I")
        IF +HRINT>0 DO
        . NEW TIME SET TIME=$$HADD^XLFDT(INITTIME,0,HRINT,0)
        . NEW task SET task=$$Schedule^TMGSEQL3(TIME,"AUTOIN^TMGSEQL4","Import of ICD data from Sequel PMS.")
        . ;"store 'next run task number'
        . SET TMGFDA(22711,"1,",8)="`"_task  ;"#4 = TASK FOR NEXT RUN
        . DO FILE^DIE("E","TMGFDA","TMGMSG")  ;" note: ignores TMGMSG or errors.
        ;
        QUIT
        ;
        ;
QUIETIN ;
        ;"Purpose: To import data based on settings, with no user interaction (in or out)
        ;"Input: none.  Settings stored in File 22711 are used
        ;"Output: None.  There should be no console output.  The database should be updated
        ;"Results: none
        ;
        NEW FNAME SET FNAME=$$GET1^DIQ(22711,"1,","IMPORT ICD DATAFILE NAME")
        NEW FPATH SET FPATH=$$GET1^DIQ(22711,"1,","IMPORT DATAFILE PATH")
        NEW DELFILES SET DELFILES=+$$GET1^DIQ(22711,"1,","DELETE DATAFILE AFTER IMPORT?","I")
        NEW USERID SET USERID=$$GET1^DIQ(22711,"1,","ALERT RECIPIENT","I")
        NEW PROGFN SET PROGFN="DO PROGBAR^TMGUSRI2(TMGCUR,""Progress"",1,TMGMAX,,TMGSTART)"
        NEW BADICDLIST,ERRARRAY
        SET RESULT=$$IMPORTFILE(FPATH,FNAME,.ERRARRAY,PROGFN,DELFILES,USERID,.BADICDLIST)
        QUIT
        ;
        ;
IMPORTFILE(FILEPATH,FILENAME,ERRARRAY,PROGFN,DELFILES,USERID,BADICDLIST)
        ;"Purpose: To import data from file specified.
        ;"Input:   FILEPATH: Path of file to input.
        ;"         FILENAME: The Name of file of file to input.
        ;"              Note: This is written to import a specific file
        ;"                      created by SequelMed Systems, filled with
        ;"                      patient demographics, in CVS format
        ;"              Note: This file will be DELETED IF DELFILES=1
        ;"         ERRARRAY: PASS BY REFERENCE.  Array to receive failed data lines.
        ;"         PROGFN: OPTIONAL -- IF supplied, then M code contained in this string
        ;"              will be xecuted periodically, to allow display of a progress bar etc.
        ;"              Note: the following variables with global scope will be declared and
        ;"                      available for use: TMGCUR (current count), TMGMAX (max count),
        ;"                      TMGSTART (the start time
        ;"                      External function can signal a request an abort by setting TMGABORT=1
        ;"         DELFILES: OPTIONAL -- IF 1, then source files (FILENAME and F2Name) are deleted after import
        ;"         USERID : OPTIONAL -- user to receive alerts regarding errors.  Default is current user (DUZ)
        ;"         BADICDLIST -- PASS BY REFERENCE. This will be an accumulated list of missing ICD-9 codes.
        ;"Output: Database is updated with data from file.
        ;"Result: 1 successful completion, 0=error
        ;
        NEW GREF,GREF1,RESULT
        SET GREF=$NAME(^TMP("TMG",$J,"SEQUELIMPORT","DATA",1))   ;"I use this to process array
        SET GREF1=$NAME(@GREF@(1))                   ;"I use this to load file
        KILL @GREF
        SET RESULT=$$FTG^%ZISH(FILEPATH,FILENAME,GREF1,6)  ;"load file into a global
        IF RESULT=0 GOTO IFDONE
        SET USERID=$GET(USERID,+$GET(DUZ))
        SET RESULT=$$IMPORTGLOBAL(GREF,.ERRARRAY,.PROGFN,USERID,.BADICDLIST)  ;"Note: @GREF, killed at end of $$IMPORTGLOBAL()
        ;"Next, record the current time as the time of last import
        DO NOW^%DTC
        NEW TMGFDA,TMGMSG
        SET TMGFDA(22711,"1,",4)=%  ;"#4 = LAST IMPORT DATE
        DO FILE^DIE("E","TMGFDA","TMGMSG")  ;" note: ignores TMGMSG or errors.
        IF $GET(DELFILES)=1 DO
        . ;"Notice: After I implemented this, I realized that I have a permissions problem
        . ;"  at my site... the uploaded files belong to the uploaded user, and deletion by
        . ;"  this user is being blocked.  I'll leave in for now...
        . NEW TEMP
        . SET TEMP=$$DELFILE^TMGIOUTL(FILEPATH_FILENAME)
        . SET TEMP=$$DELFILE^TMGIOUTL(F2Path_F2Name)
        ;
IFDONE  QUIT RESULT
        ;
        ;
IMPORTGLOBAL(GREF,ERRARRAY,PROGFN,USERID,BADICDLIST)
        ;"Purpose: To import data from global specified.
        ;"Input:   GREF -- the NAME of array holding the data to import (1st file)
        ;"              Format: @GREF@(1)=ONELINE
        ;"                      @GREF@(2)=ONELINE .. etc.
        ;"              Note: This is written to import a specific file
        ;"                      created by SequelMed Systems, filled with
        ;"                      patient ICD data, in CVS format
        ;"              Note: ARRAY will be KILLED at the end of this function.
        ;"         ERRARRAY: PASS BY REFERENCE.  Array to receive failed data lines.
        ;"         PROGFN: OPTIONAL -- IF supplied, then M code contained in this string
        ;"              will be xecuted periodically, to allow display of a progress bar etc.
        ;"              Note: the following variables with global scope will be declared and
        ;"                      available for use: TMGCUR (current count), TMGMAX (max count),
        ;"                      TMGSTART (the start time
        ;"                      External function can signal a request an abort by setting TMGABORT=1
        ;"         USERID : OPTIONAL -- user to receive alerts regarding errors.  Default is current user (DUZ)
        ;"        BADICDLIST -- PASS BY REFERENCE. This will be an accumulated list of missing ICD-9 codes.
        ;"Output: Database is updated with data from file.
        ;"Result: 1 successful completion, 0=error
        ;
        NEW TMGINVALID ;"Will be used as a globally-scoped variable in the module
        NEW RESULT SET RESULT=1
        NEW DELAY SET DELAY=0
        NEW TMGCUR,TMGMAX,TMGSTART,TMGABORT ;"avail for PROGFN function
        SET TMGABORT=0
        SET TMGMAX=+$ORDER(@GREF@(""),-1)
        SET TMGSTART=$H  ;"store starting time.
        SET USERID=$GET(USERID,+$GET(DUZ))
        ;
        SET TMGCUR=$ORDER(@GREF@(""))
        IF TMGCUR'="" FOR  DO  QUIT:(TMGCUR="")!(TMGABORT=1)
        . NEW ONELINE SET ONELINE=$GET(@GREF@(TMGCUR))
        . SET RESULT=$$PROCESLN(ONELINE,.ERRARRAY,USERID,,.BADICDLIST)
        . SET DELAY=DELAY+1
        . IF (DELAY>30),$GET(PROGFN)'="" DO  ;"update progress bar every 30 cycles
        . . NEW $ETRAP SET $ETRAP="WRITE ""(Invalid M Code!.  Error Trapped.)"" SET $ETRAP="""",$ecode="""""
        . . XECUTE PROGFN  ;"call the specified progress code.
        . . SET DELAY=0
        . SET TMGCUR=$ORDER(@GREF@(TMGCUR))
        ;
        KILL @GREF
        QUIT RESULT
        ;
        ;
PROCESLN(ONELINE,ERRARRAY,DUZ,PARSEFN,BADICDLIST)
        ;"Purpose: To process one line from patient ICD/CPT import file.
        ;"Input: ONELINE-- One line from CVS file.
        ;"       ERRARRAY: PASS BY REFERENCE.  Array to receive failed data lines.
        ;"       DUZ: The user who will recieve alerts of errors
        ;"       PARSEFN:  OPTIONAL-- the name of a function to use to parse csv line
        ;"              default value is "PARSELN"
        ;"              e.g. "MyFn" or "MyFn^MyRoutine".  Must take same params as PARSELN
        ;"              This will allow this code to be used on a variety of .csv files, with
        ;"              different data-formats--each one with its own parser funtion.
        ;"       BADICDLIST -- PASS BY REFERENCE. This will be an accumulated list of missing ICD-9 codes.
        ;"Output: Data is put into database, IF it is not there already.
        ;"Result: 1=OK To continue; -1=abort or bad data
        ;
        NEW XFN
        NEW PCEINFO,ERR1ARRY
        NEW RESULT SET RESULT=1
        SET PARSEFN=$GET(PARSEFN,"PARSELN")
        SET XFN="SET RESULT=$$"_PARSEFN_"(.ONELINE,.PCEINFO,.BADICDLIST)"
        XECUTE XFN
        IF +RESULT<1 DO  GOTO PLNDN
        . NEW MSG SET MSG=$PIECE(RESULT,"^",2)
        . IF MSG="SKIP" SET RESULT=1 QUIT
        . SET ERR1ARRY(0)=MSG
        . DO MARKERR(ONELINE,.PCEINFO,.ERR1ARRY,.ERRARRAY)  ;
        SET RESULT=$$UPDATEDB(.PCEINFO,.ERR1ARRY)
        IF RESULT<1 DO MARKERR(ONELINE,.ERR1ARRY,.ERRARRAY)  ;
PLNDN   QUIT RESULT
        ;
MARKERR(SRCLINE,PCEINFO,ERR1ARRY,ERRARRAY)  ;
        ;"Purpose: report error on one line from CSV import file
        ;"Input: SRCLINE -- The original import CSV line
        ;"       PCEINFO -- Event info array
        ;"       ERR1ARRY -- PASS BY REFERENCE.  The Array containing the error information,
        ;"          with following format:
        ;"          ERR1ARRY(0)=local message (if any)
        ;"          ERR1ARRY("DIERR")=Standard fileman DIERR array.
        ;"       ERRARRAY -- PASS BY REFERENCE.  Master error array.
        ;
        NEW SKIP SET SKIP=0
        NEW MSG SET MSG=$GET(ERR1ARRY(0))
        IF MSG'="" DO  GOTO:SKIP MERDN
        . IF $DATA(ERRARRAY("MSG",MSG)) SET SKIP=1 QUIT
        . SET ERRARRAY("MSG",MSG)=""
        NEW COUNT SET COUNT=+$GET(ERRARRAY)+1
        SET ERRARRAY=COUNT
        SET ERRARRAY(COUNT)=SRCLINE
        MERGE ERRARRAY(COUNT,"INFO")=ERR1ARRY
        ;"TEMP RESTORE LATER --> DO ALERTERR^TMGSEQL2(SRCLINE,.PCEINFO,.ERR1ARRY,DUZ,"TMGSQLICDIMPORT")
        WRITE !,"HERE I WOULD ALERT ERROR:",!
        IF $DATA(ERR1ARRY) DO ZWRITE^TMGZWR("ERR1ARRY")
MERDN   QUIT
        ;
        ;
PARSELN(ONELINE,ARRAY,BADICDLIST)
        ;"Purpose: To process one line from patient demographics file.
        ;"         Also gets data into an acceptible format.
        ;"Input: ONELINE -- One line from CVS demographics file. (Format as per PROCESLN)
        ;"           Format is as follows, *** all on one line (comma delimited)
        ;"           01- Insurance_Name
        ;"           02- visit_date_from
        ;"           03- proc_cpt_code
        ;"           04- practice_name
        ;"           05- location_name
        ;"           06- provider_name
        ;"           07- total_amount_charged
        ;"           08- total_amount_paid
        ;"           09- total_adjustment
        ;"           10- icd_9_code  (may be multiple, space delimited)
        ;"           11- modifier
        ;"           12- visit_seq_num
        ;"           13- charge_seq_num
        ;"           14- patient_seq_num
        ;"           15- parent_seq_num
        ;"           16- account_num <-- matches custom field 22701 in PATIENT file
        ;"           17- last_name
        ;"           18- first_name
        ;"           19- plan_priority_type
        ;"           20- plan_id
        ;"           21- ref_provider
        ;"           22- asst_provider
        ;"           23- entry_date
        ;"           24- panel_group
        ;"           25- num_of_units
        ;"       ARRAY -- PASS BY REFERENCE. And OUT parameter.  Any prior data killed. Format:
        ;"           ARRAY("DATE")= Event date (in internal Fileman format)
        ;"           ARRAY("PROC CPT CODE")=Procedure code for event
        ;"           ARRAY("LOCATION")=Location IEN for event
        ;"           ARRAY("PROVIDER")=Provider IEN for event
        ;"           ARRAY("PATIENT")=DFN^Patient Name
        ;"           ARRAY("ICD CODE",ICD_IEN)=Text Description
        ;"           ARRAY("ICD CODE",ICD_IEN,"LEX")=IEN757.01^Lex Descr^IC9_Name^ICD_IEN^ICD Set Name
        ;"       BADICDLIST -- PASS BY REFERENCE. This will be an accumulated list of missing ICD-9 codes.
        ;"       Note: uses TMGINVALID (globally scoped var defined in this module)
        ;"Output: ARRAY is filled with Format as follows (note not all data used):
        ;"Result: 1=OK To continue; -1^Error Message -1^SKIP IF should skip this
        ;
        NEW TEMP
        NEW RESULT SET RESULT=1
        SET ONELINE=$TRANSLATE($GET(ONELINE),"""","'") ;"  convert " to ' to avoid fileman error
        KILL ARRAY
        ;"-----------------------------
        NEW DATE SET DATE=$PIECE(ONELINE,",",2)
        SET DATE=$$TRIM^XLFSTR(DATE)
        IF DATE="visit_date_from" DO  GOTO PLDN  ;"Skip header row
        . SET RESULT="-1^SKIP"
        SET DATE=$PIECE(DATE," ",1)   ;" '03/09/05 00:00' --> '03/09/05'
        NEW %DT,X,Y
        SET %DT="P",X=DATE DO ^%DT
        IF +Y<0 DO  GOTO PLDN
        . SET RESULT="-1^Date '"_DATE_"' is not valid."
        SET ARRAY("DATE")=Y
        ;"-----------------------------
        SET ARRAY("PROC CPT CODE")=$$Trim^TMGSTUTL($PIECE(ONELINE,",",3))
        ;"-----------------------------
        NEW LOC SET LOC=$$Trim^TMGSTUTL($PIECE(ONELINE,",",5))
        NEW LOCIEN SET LOCIEN=+$ORDER(^SC("TMGPMS",LOC,0))
        IF LOCIEN'>0 DO  GOTO PLDN
        . IF $$SKIPLOC(LOC) SET RESULT="-1^SKIP" QUIT
        . SET RESULT="-1^Location '"_LOC_"' not found in field 'TMG PMS NAME' for any record in HOSPITAL LOCATION file."
        SET ARRAY("LOCATION")=LOCIEN
        ;"-----------------------------
        NEW TEMPPROV SET TEMPPROV=$PIECE(ONELINE,",",6)
        IF TEMPPROV="" DO  GOTO PLDN
        . SET RESULT="-1^No Provider found for event"
        ;"SET TEMPPROV=$$ConvProvider^TMGSEQL1(TEMPPROV)
        ;"IF TEMPPROV="SKIP" DO  GOTO PLDN
        ;". SET RESULT="-1^Provider 'SKIP' invalid"
        IF $$INVALPRV^TMGSEQL1(TEMPPROV) DO  GOTO PLDN
        . SET RESULT="-1^SKIP"
        NEW PROVIEN SET PROVIEN=+$ORDER(^VA(200,"TMG",TEMPPROV,0))
        IF PROVIEN'>0 DO  GOTO PLDN
        . SET RESULT="-1^Unable to find provider '"_TEMPPROV_"'"
        SET ARRAY("PROVIDER")=PROVIEN
        ;"-----------------------------
        NEW ACCTNUM SET ACCTNUM=+$PIECE(ONELINE,",",16)
        IF ACCTNUM'>0 DO  GOTO PLDN
        . SET RESULT="-1^Patient PMS account number not provided."
        NEW DFN SET DFN=$ORDER(^DPT("TMGS",ACCTNUM,0))
        IF DFN'>0 DO  GOTO PLDN
        . SET RESULT="-1^Unable to convert PMS account number '"_ACCTNUM_"' into VistA patient number (DFN)"
        SET ARRAY("PATIENT")=DFN_"^"_$PIECE($GET(^DPT(DFN,0)),"^",1)
        ;"-----------------------------
        NEW ICDTEMP SET ICDTEMP=$PIECE(ONELINE,",",10)
        NEW I FOR I=1:1:$LENGTH(ICDTEMP," ") DO
        . NEW ANICD SET ANICD=$PIECE(ICDTEMP," ",I) QUIT:ANICD=""
        . NEW ICDIEN SET ICDIEN=+$ORDER(^ICD9("BA",ANICD_" ",0))
        . IF ICDIEN>0 DO  QUIT
        . . NEW DESCR SET DESCR=$PIECE($GET(^ICD9(ICDIEN,0)),"^",3)
        . . SET ARRAY("ICD CODE",ICDIEN)=DESCR
        . . NEW LEXARR,LEXINFO
        . . DO LEXSRCH^ORQQPL1(.LEXARR,ANICD,1,,0)
        . . SET LEXINFO=$GET(LEXARR(1))
        . . SET ARRAY("ICD CODE",ICDIEN,"LEX")=LEXINFO
        . ELSE  SET BADICDLIST(ANICD)=""
        . ;"IF RESULT=1 SET RESULT="-1^"
        . ;"SET RESULT=RESULT_"ICD-9 code '"_ANICD_"' not in VistA. "
        IF $DATA(ARRAY("ICD CODE"))=0 DO
        . SET RESULT="-1^SKIP"
        ;"-----------------------------
PLDN    QUIT RESULT
        ;
        ;
SKIPLOC(LOC)  ;
        ;"Return is location should be skipped
        ;"Results: 1 IF should be skipped, 0 IF no skipped
        NEW RESULT SET RESULT=0
        IF LOC="LAUHOSPIN" SET RESULT=1
        ELSE  IF LOC="LAUHOSPOUT" SET RESULT=1
        ELSE  IF LOC="PML" SET RESULT=1
        ELSE  IF LOC="TMG-FPG" SET RESULT=1
        QUIT RESULT
        ;
        ;
UPDATEDB(PCEINFO,ERR1ARRY)
        ;"Purpose: To put that data from the PCEINFO array into the database (if needed)
        ;"Input: PCEINFO -- array (PASS BY REFERENCE), with the following items being used:
        ;"           PCEINFO("DATE")= Event date (in internal Fileman format)
        ;"           PCEINFO("PROC CPT CODE")=Procedure code for event
        ;"           PCEINFO("LOCATION")=Location IEN for event
        ;"           PCEINFO("PROVIDER")=Provider IEN for event
        ;"           PCEINFO("PATIENT")=DFN^Patient Name
        ;"           PCEINFO("ICD CODE",ICD_IEN)=Text Description
        ;"           PCEINFO("ICD CODE",ICD_IEN,"LEX")=IEN757.01^Lex Descr^IC9_Name^ICD_IEN^ICD Set Name
        ;"      ERR1ARRY -- PASS BY REFERENCE.  And OUT parameter to get back error info.
        ;"Output: Data is put into database, IF it is not there already.
        ;"Result: 1 successful completion, -1=error
        ;
        SET RESULT=1
        NEW DFN SET DFN=+PCEINFO("PATIENT")
        NEW PROBEXISTS SET PROBEXISTS=0
        NEW ICDIEN SET ICDIEN=0
        FOR  SET ICDIEN=$ORDER(PCEINFO("ICD CODE",ICDIEN)) QUIT:ICDIEN=""  DO
        . NEW STATUS SET STATUS=$$GETSTAT(DFN,ICDIEN)
        . IF +STATUS=1 QUIT  ;"Already has ICD code in problem
        . IF +STATUS=0 SET RESULT=$$ADDPROB(ICDIEN,.PCEINFO,.ERR1ARRY)  ;"Add NEW problem
        . IF +STATUS=2 DO
        . . NEW INACTIVEDATE SET INACTIVEDATE=+$PIECE(STATUS,"^",2)
        . . NEW THISDATE SET THISDATE=PCEINFO("DATE")
        . . IF INACTIVEDATE>THISDATE QUIT
        . . NEW PROBIEN SET PROBIEN=$PIECE(STATUS,"^",2)  ;"PROBIEN = pre-existing PROBLEM record IEN
        . . SET RESULT=$$ACTVPROB(PROBIEN,.ERR1ARRY)
        QUIT RESULT
        ;
        ;
GETSTAT(DFN,ICDIEN)  ;
        ;"Purpose: determine status on an ICD code in problems for a patient
        ;"Results: 0=No problem with this ICD code exists for patient
        ;"         1^PROBIEN=problem with this ICD code exists, and is active
        ;"         2^PROBIEN^FMDATE = problem with this ICD code exists, but is INACTIVE
        ;"                    PROBIEN = IEN of the existing problem
        ;"                    FMDATE is the date the problem was inactivated.
        ;"
        NEW RESULT SET RESULT=0
        NEW PROBIEN SET PROBIEN=0
        FOR  SET PROBIEN=$ORDER(^AUPNPROB("AC",DFN,PROBIEN)) QUIT:(+PROBIEN'>0)!(RESULT=1)  DO
        . NEW THISICD SET THISICD=$PIECE($GET(^AUPNPROB(PROBIEN,0)),"^",1)
        . IF THISICD'=ICDIEN QUIT
        . SET RESULT=1_"^"_PROBIEN
        . NEW STATUS SET STATUS=$PIECE($GET(^AUPNPROB(PROBIEN,0)),"^",12)
        . IF STATUS'="I" QUIT
        . SET RESULT=2_"^"_PROBIEN_"^"_$PIECE($GET(^AUPNPROB(PROBIEN,1)),"^",7)  ;" 1;7=DATE RESOLVED
        QUIT RESULT
        ;
        ;
ACTVPROB(PROBIEN,ERR1ARRY)  ;
        ;"Purpose: Activate existing problem.
        ;"Input: PROBIEN -- The IEN of the problem in the PROBLEM file (#9000011)
        ;"      ERRARRAY -- PASS BY REFERENCE.  And OUT parameter to get back error info.
        ;"Result: 1 successful completion, -1=error
        NEW RESULT SET RESULT=1
        NEW TMGFDA,TMGMSG
        SET TMGFDA(9000011,PROBIEN_",",.12)="A"
        DO FILE^DIE("K","TMGFDA","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO
        . MERGE ERR1ARRAY=TMGMSG
        . SET RESULT=-1
        QUIT RESULT
        ;
        ;
ADDPROB(PROBIEN,PCEINFO,ERR1ARRY)  ;"Add NEW problem
        ;"Purpose: Create a NEW problem for patient, using entry for RPC 'ORQQPL ADD SAVE'
        ;"Input: PCEINFO -- PASS BY REFERENCE.  Format:
        ;"           PCEINFO("DATE")= Event date (in internal Fileman format)
        ;"           PCEINFO("PROC CPT CODE")=Procedure code for event
        ;"           PCEINFO("LOCATION")=Location IEN for event
        ;"           PCEINFO("PROVIDER")=Provider IEN for event
        ;"           PCEINFO("PATIENT")=DFN^Name
        ;"           PCEINFO("ICD CODE",ICD_IEN)=Text Description
        ;"Results: 1 if OK, or -1 IF error
        ;
        NEW RESULT SET RESULT=-1
        NEW DFN SET DFN=PCEINFO("PATIENT")
        NEW PROV SET PROV=PCEINFO("PROVIDER")
        NEW MSG SET MSG=PCEINFO("ICD CODE",PROBIEN)
        IF +$GET(DUZ(2))'>0 DO DUZ^XUP(DUZ)
        NEW FACILITY SET FACILITY=+$GET(DUZ(2))
        IF FACILITY'>0 SET FACILITY=$ORDER(^VA(200,DUZ,2,0))
        NEW LEXINFO SET LEXINFO=$GET(PCEINFO("ICD CODE",PROBIEN,"LEX"))
        IF LEXINFO'="" SET MSG=$PIECE(LEXINFO,"^",2)
        NEW X,NOWDATE DO NOW^%DTC SET NOWDATE=X
        NEW ARR1
        SET ARR1(.01)=PROBIEN
        SET ARR1(.03)="0^"
        SET ARR1(.05)="^"_MSG
        SET ARR1(.08)=PCEINFO("DATE")
        SET ARR1(.12)="A^ACTIVE"
        SET ARR1(.13)="^"
        IF LEXINFO'="" SET ARR1(1.01)=$PIECE(LEXINFO,"^",1,2)
        SET ARR1(1.02)="T"  ;"T = transcribed (and will be flagged). Or could use "P"-- permanent
        SET ARR1(1.03)=PROV
        SET ARR1(1.04)=PROV
        SET ARR1(1.05)=PROV
        SET ARR1(1.06)="^"
        SET ARR1(1.07)="^"
        SET ARR1(1.08)=PCEINFO("LOCATION")
        SET ARR1(1.09)=NOWDATE
        SET ARR1(1.1)="0^NO"
        SET ARR1(1.11)="0^NO"
        SET ARR1(1.12)="0^NO"
        SET ARR1(1.13)="0^NO"
        SET ARR1(1.14)="^"
        SET ARR1(1.15)="^"
        SET ARR1(1.16)="^"
        NEW TMGARR
        NEW FLD SET FLD=0
        NEW CNT SET CNT=1
        FOR  SET FLD=$ORDER(ARR1(FLD)) QUIT:(FLD="")  DO
        . NEW VAL SET VAL=$GET(ARR1(FLD)) QUIT:VAL=""
        . SET TMGARR(CNT)="GMPFLD("_FLD_")="""_VAL_""""
        . SET CNT=CNT+1
        ;"SET ARR1(10,0)="0"
        SET TMGARR(CNT)="GMPFLD(10,0)=""0"""
        DO ADDSAVE^ORQQPL1(.RESULT,DFN,PROV,FACILITY,.TMGARR)
        IF RESULT<1 DO
        . SET ERR1ARRY(0)="Unable to add NEW problem via ORQQPL ADD SAVE functionality"
        QUIT RESULT
        ;
