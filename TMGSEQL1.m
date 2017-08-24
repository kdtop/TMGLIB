TMGSEQL1 ;TMG/kst/Interface with SequelSystems PMS ;03/25/06, 7/10/12, 2/2/14
   ;;1.0;TMG-LIB;**1**;01/09/06
 ;
 ;"TMG SEQUEL DEMOGRAPHICS IMPORT FUNCTIONS
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
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"ASKIMPORT
 ;"RUNNOW  -- provide an entry point for running import NOW.  This will delete prior alerts
 ;"AUTOIN  -- entry point for scheduled task
 ;"QUIETIN
 ;"$$IMPORTFILE(FILEPATH,FILENAME,F2NAME,F3NAME,ERRARRAY,CHGLOG,PROGFN,DELFILES,USERID)
 ;"$$IMPRTGBL(GREF,G2REF,G3REF,ERRARRAY,CHGLOG,PROGFN,USERID)
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"$$PROCESPT(ONELINE,ERRARRAY,CHGLOG,EXTARR2,EXTARR3,DUZ)
 ;"$$CONVPROV(SEQLPROV) -- convert Sequel provider shortname to VistA file 200 name.
 ;"$$PARSLINE(ONELINE,ARRAY,EXTARR2)
 ;"UPDATEDB(PTINFO,AUTOREG,ONEERRARRAY,CHGLOG)
 ;"$$INACTVPT(PMSACCTNUM,EXTARR2)
 ;"$$INVALPRV(SEQLPROV)
 ;"$$INVALPTN(FNAME,LNAME)
 ;"$$GETDFN(PTINFO) -- interface to ^TMGGDFN functions
 ;"XTRCTEX2(G2REF,EXTARR2) -- extract info from 2nd demographics file --> SSNUMs
 ;"XTRCTEX3(G2REF,EXTARR2) -- extract info from 3rd Sequel export file
 ;"GTDEMOCD(DFN,FIELD) -- Return codes for entries in FILE
 ;"ADDDEMCD(DFN,FIELD,CODE,ERRORS) ;
 ;"DELDEMCD(DFN,FIELD,CODE,ERRORS)
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"TMGIOUTL
 ;"TMGMISC
 ;"=======================================================================
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"      Below are three custom files that are used by the TMGSEQL* code
 ;"=======================================================================
 ;
 ;
 ;"FILE: 22706 TMG DEMOGRAPHICS IMPORT ERRORS         Branch: 1
 ;"REF  NODE;PIECE     FLD NUM  FIELD NAME
 ;"===============================================================================
 ;"  1  0;1    .01  ACCOUNT NUMBER         [RNJ9,0]
 ;"  2  4;1    .02  CREATION DATE         [D]
 ;"  3  4;2    .03  PATIENT NAME          [F]
 ;"  4  0;2      1  MESSAGE               [F]
 ;"     2;0      2  IMPORT DATA           <-WP [22706.02]
 ;"  5   -0;1        .01   -IMPORT DATA         [W]
 ;"     3;0      3  DIERR MESSAGE         <-WP [22706.03]
 ;"  6   -0;1        .01   -DIERR MESSAGE             [W]
 ;"  7  4;3      4  ALERT IEN         [NJ9,0]
 ;" <> <> <>
 ;"  A.) FILE NAME:------------- TMG DEMOGRAPHICS IMPORT ERRORS
 ;"            F.) FILE ACCESS:
 ;"  B.) FILE NUMBER:----------- 22706      DD______ @
 ;"                 Read____ @
 ;"  C.) NUM OF FLDS:----------- 9          Write___ @
 ;"                 Delete__ @
 ;"  D.) DATA GLOBAL:----------- ^TMG(22706,      Laygo___ @
 ;"
 ;"  E.) TOTAL GLOBAL ENTRIES:-- 76    G.) PRINTING STATUS:-- Off
 ;"================================================================================
 ;
 ;"FILE: 22707 TMG NAME SEX                Branch: 1
 ;"REF  NODE;PIECE     FLD NUM  FIELD NAME
 ;"===============================================================================
 ;"  1  0;1    .01  FIRST NAME           [RF]
 ;"  2  0;2      1  SEX             [S]
 ;"<> <> <>
 ;"  A.) FILE NAME:------------- TMG NAME SEX
 ;"            F.) FILE ACCESS:
 ;"  B.) FILE NUMBER:----------- 22707      DD______ @
 ;"                 Read____ @
 ;"  C.) NUM OF FLDS:----------- 2          Write___ @
 ;"                 Delete__ @
 ;"  D.) DATA GLOBAL:----------- ^TMG(22707,      Laygo___ @
 ;"
 ;"  E.) TOTAL GLOBAL ENTRIES:-- 698         G.) PRINTING STATUS:-- Off
 ;"================================================================================
 ;
 ;"FILE: 22711 TMG UPLOAD SETTINGS               Branch: 1
 ;"REF  NODE;PIECE     FLD NUM  FIELD NAME
 ;"===============================================================================
 ;"  1  0;1    .01  NAME          [RFX]
 ;"  2  0;2      1  DEBUG SHOW             [NJ1,0X]
 ;"  3  1;1    1.1  DEBUG OUTPUT FILE           [F]
 ;"  4  2;1         1.15  DEBUG OUTPUT PATH           [F]
 ;"  5  1;2    1.2  DEBUG CUMULATIVE        [NJ1,0]
 ;"  6  3;1      2  IMPORT DATAFILE NAME        [F]
 ;"  7  5;1    2.1  IMPORT DATAFILE 2 NAME      [F]
 ;"  8  4;1    2.5  IMPORT DATAFILE PATH        [F]
 ;"  9  6;1      3  ALERT RECIPIENT       <-Pntr  [P200']
 ;" 10  6;2      4  LAST IMPORT DATE            [D]
 ;" 11  6;3      5  DELETE DATAFILE AFTER IMPORT?     [S]
 ;" 12  6;4      6  PICK GENDER FROM NAME?      [S]
 ;" 13  6;5      7  IMPORT FREQUENCY (IN HOURS)         [NJ4,0]
 ;" <> <> <>
 ;"  A.) FILE NAME:------------- TMG UPLOAD SETTINGS
 ;"            F.) FILE ACCESS:
 ;"  B.) FILE NUMBER:----------- 22711      DD______ @
 ;"                 Read____ @
 ;"  C.) NUM OF FLDS:----------- 12         Write___ @
 ;"                 Delete__ @
 ;"  D.) DATA GLOBAL:----------- ^TMG(22711,      Laygo___ @
 ;"
 ;"  E.) TOTAL GLOBAL ENTRIES:-- 1     G.) PRINTING STATUS:-- Off
 ;"================================================================================
 ;
ASKIMPORT ;
  ;"Purpose: To ask user for filename and then import data.
  ;"Input: None
  ;"Output: Database is updated with data from file.
  ;"Result: None
  NEW DISCARDNAME
  NEW DEFPATH SET DEFPATH="/mnt/WinServer/"
  NEW DEFFNAME SET DEFFNAME="demographics.csv"
  NEW DEFF2NAME SET DEFF2NAME="demographics2.csv"
  NEW DEFF3NAME SET DEFF3NAME="demographics2.csv"
  NEW FPATH,FNAME,F2NAME,F3NAME
  NEW ERRARRAY,CHANGLOG,RESULT
  NEW PROGFN SET PROGFN="do PROGBAR^TMGUSRI2(TMGCUR,""Progress"",1,TMGMAX,,TMGSTART)"
  SET PROGFN=PROGFN_" read *TMGKEYIN:0 SET:(TMGKEYIN=27) TMGABORT=1"
  ;
  SET DISCARDNAME=$$GETFNAME^TMGIOUTL("Please enter file to import.",.DEFPATH,.DEFFNAME,,.FPATH,.FNAME)
  IF DISCARDNAME="" GOTO AIDN
  SET DEFPATH=FPATH
  ;
  SET DISCARDNAME=$$GETFNAME^TMGIOUTL("Please enter 2nd file to import.",.DEFPATH,.DEFF2NAME,,.FPATH,.F2NAME)
  IF DISCARDNAME="" GOTO AIDN
  ;
  SET DISCARDNAME=$$GETFNAME^TMGIOUTL("Please enter 3rd file to import.",.DEFPATH,.DEFF3NAME,,.FPATH,.F3NAME)
  IF DISCARDNAME="" GOTO AIDN
  ;
  SET RESULT=$$IMPORTFILE(FPATH,FNAME,F2NAME,F3NAME,.ERRARRAY,.CHANGLOG,PROGFN)
  ;      
AIDN  ;
  QUIT
  ;
RUNNOW  ;"RUN NOW
  ;"Purpose: To provide an entry point for running import NOW.  This will delete prior alerts
  ;"Input: none.  Settings stored in FILE 22711 are used
  ;"Output: None.  Progress shown to console.  The database should be updated
  ;"Results: none
  WRITE !!,"Import Sequel Demographics Now...",!
  NEW ERRARRAY,CHANGLOG
  NEW FNAME SET FNAME=$$GET1^DIQ(22711,"1,","IMPORT DATAFILE NAME")
  NEW F2NAME SET F2NAME=$$GET1^DIQ(22711,"1,","IMPORT DATAFILE 2 NAME")
  NEW F3NAME SET F3NAME=$$GET1^DIQ(22711,"1,","IMPORT DATAFILE 3 NAME")
  NEW FPATH SET FPATH=$$GET1^DIQ(22711,"1,","IMPORT DATAFILE PATH")
  NEW DELFILES SET DELFILES=+$$GET1^DIQ(22711,"1,","DELETE DATAFILE AFTER IMPORT?","I")
  NEW USERID SET USERID=$$GET1^DIQ(22711,"1,","ALERT RECIPIENT","I")
  NEW PROGFN SET PROGFN="do PROGBAR^TMGUSRI2(TMGCUR,""Progress"",1,TMGMAX,,TMGSTART)"
  SET PROGFN=PROGFN_" read *TMGKEYIN:0 SET:(TMGKEYIN=27) TMGABORT=1"
  ;
  NEW RESULT SET RESULT=$$IMPORTFILE(FPATH,FNAME,F2NAME,F3NAME,,,PROGFN,DELFILES,USERID)
  QUIT
  ;
AUTOIN  ;
  ;"Purpose: To provide an entry point for a scheduled task.  This will delete prior alerts
  ;"Input: none.  Settings stored in FILE 22711 are used
  ;"Output: None.  There should be no console output.  The database should be updated
  ;"Results: none
  NEW STARTTIME SET STARTTIME=$H
  NEW USERID SET USERID=$$GET1^DIQ(22711,"1,","ALERT RECIPIENT","I")
  DO  ;"clear out 'next run task number'
  . NEW TMGFDA,TMGMSG
  . SET TMGFDA(22711,"1,",8)="@"  ;"#4 = TASK FOR NEXT RUN
  . DO FILE^DIE("E","TMGFDA","TMGMSG")  ;" note: ignores TMGMSG or errors.
  ;
  NEW TEMP SET TEMP=$$QuietClear^TMGSEQL3(USERID)  ;"clear prior alerts & errors
  DO QUIETIN  ;" DO import
  ;
  ;"Here I schedule the next task to run again.
  NEW HRINTERVAL SET HRINTERVAL=$$GET1^DIQ(22711,"1,","IMPORT FREQUENCY (IN HOURS)","I")
  IF +HRINTERVAL>0 DO
  . NEW TIME SET TIME=$$HADD^XLFDT(STARTTIME,0,HRINTERVAL,0)
  . NEW TASK SET TASK=$$Schedule^TMGSEQL3(TIME,"AUTOIN^TMGSEQL1","Import of demographic data from Sequel billing system.")
  . ;"store 'next run task number'
  . SET TMGFDA(22711,"1,",8)="`"_TASK  ;"#4 = TASK FOR NEXT RUN
  . DO FILE^DIE("E","TMGFDA","TMGMSG")  ;" note: ignores TMGMSG or errors.
  QUIT
  ;
QUIETIN  ;
  ;"Purpose: To import data based on settings, with no user interaction (in or out)
  ;"Input: none.  Settings stored in FILE 22711 are used
  ;"Output: None.  There should be no console output.  The database should be updated
  ;"Results: none  
  NEW FNAME SET FNAME=$$GET1^DIQ(22711,"1,","IMPORT DATAFILE NAME")
  NEW F2NAME SET F2NAME=$$GET1^DIQ(22711,"1,","IMPORT DATAFILE 2 NAME")
  NEW F3NAME SET F3NAME=$$GET1^DIQ(22711,"1,","IMPORT DATAFILE 2 NAME")
  NEW FPATH SET FPATH=$$GET1^DIQ(22711,"1,","IMPORT DATAFILE PATH")
  NEW DELFILES SET DELFILES=+$$GET1^DIQ(22711,"1,","DELETE DATAFILE AFTER IMPORT?","I")
  NEW USERID SET USERID=$$GET1^DIQ(22711,"1,","ALERT RECIPIENT","I")
  ;
  NEW ERRARRAY,CHANGLOG,RESULT
  SET RESULT=$$IMPORTFILE(FPATH,FNAME,F2NAME,F3NAME,.ERRARRAY,.CHANGLOG,,DELFILES,USERID)
  QUIT
  ;
IMPORTFILE(FILEPATH,FILENAME,F2NAME,F3NAME,ERRARRAY,CHGLOG,PROGFN,DELFILES,USERID) ;
  ;"Purpose: To import data from file specified.
  ;"Input:   FILEPATH: PATH of file to input.
  ;"   FILENAME: The Name of file of file to input.
  ;"  Note: This is written to import a specific file
  ;"    created by SequelMed Systems, filled with
  ;"    patient demographics, in CVS format
  ;"  Note: This file will be DELETED IF DELFILES=1
  ;"   F2NAME : the name of the second demographics file in input
  ;"  The reason for 2 files is because Sequel doesn't report the SSN in the
  ;"  primary demographics report.  So a second report must be used, and these
  ;"  two files are MERGEd to provide complete patient demographics.
  ;"  Note: This file will be DELETED IF DELFILES=1
  ;"   F3NAME : the name of the 3rd demographics file in input.
  ;"  (Added later, provides more info from Sequel)
  ;"   ERRARRAY: PASS BY REFERENCE.  An OUT PARAMETER.  Array to receive failed data lines.
  ;"       ERRARRAY=<count>
  ;"       ERRARRAY(#)=<data line>
  ;"       ERRARRAY(#,"INFO")=error info
  ;"   CHGLOG: PASS BY REFERENCE.  An array to receive record of changes made to database
  ;"   PROGFN: OPTIONAL -- IF supplied, then M code contained in this string
  ;"  will be xecuted periodically, to allow display of a progress bar etc.
  ;"  Note: the following variables with global scope will be declared and
  ;"    available for use: TMGCUR (current count), TMGMAX (max count),
  ;"    TMGSTART (the start time
  ;"    External function can signal a request an abort by setting TMGABORT=1
  ;"   DELFILES: OPTIONAL -- IF 1, then source files (FILENAME and F2NAME) are deleted after import
  ;"   USERID : OPTIONAL -- user to receive alerts regarding errors.  Default is current user (DUZ)
  ;"Note: I have learned that SequelMed billing system exports ALL patients in the primary
  ;"      export file, including one that have been marked inactive due to invalid data etc.
  ;"      Thus, while the second file (F2NAME) has limited info, it contains the list of
  ;"      ACTIVE patients.  So if a name is not included in the 2nd file, then its info will
  ;"      be ignored in the 1st file.
  ;"Output: Database is updated with data from file.
  ;"Result: 1 successful completion, -1^Message if error
  NEW RESULT,GREF,G2REF,G3REF     ;"I use these references when processing arrays
  SET GREF=$NAME(^TMP("TMG","SEQUELIMPORT","DATA",1,$J)) KILL @GREF 
  SET RESULT=$$FTG^%ZISH(FILEPATH,FILENAME,$NAME(@GREF@(1)),6) IF RESULT=0 GOTO IFDONE  
  SET G2REF=$NAME(^TMP("TMG","SEQUELIMPORT","DATA",2,$J)) KILL @G2REF 
  SET RESULT=$$FTG^%ZISH(FILEPATH,F2NAME,$NAME(@G2REF@(1)),6) IF RESULT=0 GOTO IFDONE  
  SET G3REF=$NAME(^TMP("TMG","SEQUELIMPORT","DATA",3,$J)) KILL @G3REF
  SET RESULT=$$FTG^%ZISH(FILEPATH,F3NAME,$NAME(@G3REF@(1)),6) 
  ;" don't report error at this time  6/5/15 IF RESULT=0 GOTO IFDONE
  ;
  SET USERID=$GET(USERID,+$GET(DUZ))
  SET RESULT=$$IMPRTGBL(GREF,G2REF,G3REF,.ERRARRAY,.CHANGLOG,.PROGFN,USERID)
  ;"Note: @GREF, @G2REF, @G3REF killed at end of $$IMPRTGBL()
  IF RESULT'>0 GOTO IFDONE
  ;
  ;"record the current time as the time of last import
  NEW TMGFDA,TMGMSG      
  SET TMGFDA(22711,"1,",4)=$$NOW^XLFDT  ;"#4 = LAST IMPORT DATE
  DO FILE^DIE("E","TMGFDA","TMGMSG")  ;" note: ignores TMGMSG or errors.
  IF $DATA(TMGMSG("DIERR")) DO  GOTO IFDONE
  . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)  
  ;  
  IF $GET(DELFILES)=1 DO      
  . ;"Notice: After I implemented this, I realized that I have a permissions problem
  . ;"  at my site... the uploaded files belong to the uploaded user, and deletion by
  . ;"  this user is being blocked.  I'll leave in for now...
  . ;"NOTICE that I am not recording errors from deletion process.
  . NEW TEMP
  . SET TEMP=$$DELFILE^TMGIOUTL(FILEPATH_FILENAME)
  . SET TEMP=$$DELFILE^TMGIOUTL(FILEPATH_F2NAME)
  . ;"F3NAME is a difficult export from Sequel (i.e. hours of screen scraping)
  . ;"So will be run perhaps only once a week.  So won't delete file.  It
  . ;"Will be replaced at time of next run.  
  . ;"SET TEMP=$$DELFILE^TMGIOUTL(FILEPATH_F3NAME)
  ;
IFDONE  ;
  QUIT RESULT
  ;
IMPRTGBL(GREF,G2REF,G3REF,ERRARRAY,CHANGLOG,PROGFN,USERID) ;"IMPORT GLOBAL
  ;"Purpose: To import data from global specified.
  ;"Input: 
  ;"  GREF -- the NAME of array holding the data to import (1st file);  KILLED at the end of this function.
  ;"      Format: @GREF@(1)=ONELINE
  ;"              @GREF@(2)=ONELINE .. etc.
  ;"      Note: This imports only specific files created by SequelMed Systems, filled with
  ;"        patient demographics, in CVS format
  ;"  G2REF -- the NAME of array holding the data to import (2nd file); KILLED at the end of this function.
  ;"  G3REF -- the NAME of array holding the data to import (3rd file); KILLED at the end of this function.
  ;"  ERRARRAY: PASS BY REFERENCE.  An OUT PARAMETER.  Array to receive failed data lines.
  ;"     ERRARRAY=<count>
  ;"     ERRARRAY(#)=<data line>
  ;"     ERRARRAY(#,"INFO")=error info
  ;"  CHGLOG: PASS BY REFERENCE.  An array to receive record of changes made to database
  ;"  PROGFN: OPTIONAL -- IF supplied, then M code contained in this string
  ;"      will be xecuted periodically, to allow display of a progress bar etc.
  ;"      Note: the following variables with global scope will be declared and
  ;"      available for use: TMGCUR (current count), TMGMAX (max count),
  ;"      TMGSTART (the start time
  ;"      External function can signal a request an abort by setting TMGABORT=1
  ;"  USERID : OPTIONAL -- user to receive alerts regarding errors.  Default is current user (DUZ)
  ;"Output: Database is updated with data from file.
  ;"Result: 1 successful completion, -1^Message if error
  NEW TMGINVALID ;"Will be used as a globally-scoped variable in the module
  NEW RESULT SET RESULT=1
  NEW DELAY SET DELAY=0
  NEW TMGCUR,TMGMAX,TMGSTART,TMGABORT ;"avail for PROGFN function
  SET TMGABORT=0
  SET TMGMAX=+$ORDER(@GREF@(""),-1)
  SET TMGSTART=$H  ;"store starting time.
  SET USERID=$GET(USERID,+$GET(DUZ))
  ;
  NEW EXTARR2,EXTARR3      
  DO XTRCTEX2(G2REF,.EXTARR2)
  DO XTRCTEX3(G3REF,.EXTARR3) 
  ;
  SET TMGCUR=""
  FOR  SET TMGCUR=$ORDER(@GREF@(TMGCUR)) QUIT:(TMGCUR="")!(TMGABORT=1)  DO
  . NEW ONELINE SET ONELINE=$GET(@GREF@(TMGCUR))
  . IF $PIECE(ONELINE,",",1)="patient_seq_num" QUIT  
  . SET RESULT=$$PROCESPT(ONELINE,.ERRARRAY,.CHGLOG,.EXTARR2,.EXTARR3,USERID)
  . SET DELAY=DELAY+1
  . IF (DELAY>30),$GET(PROGFN)'="" DO  ;"update progress bar every 30 cycles
  . . NEW $ETRAP SET $ETRAP="write ""(Invalid M Code!.  Error Trapped.)"" SET $ETRAP="""",$ecode="""""
  . . XECUTE PROGFN  ;"call the specified progress code.
  . . SET DELAY=0
  KILL @GREF,@G2REF,@G3REF
  QUIT RESULT
  ;
PROCESPT(ONELINE,ERRARRAY,CHGLOG,EXTARR2,EXTARR3,DUZ) ;
  ;"Purpose: To process one line from patient demographics file.
  ;"Input: 
  ;"  ONELINE-- One line from CVS demographics file.
  ;"  ERRARRAY: PASS BY REFERENCE.  An OUT PARAMETER.  Array to receive failed data lines.
  ;"    ERRARRAY=<count>
  ;"    ERRARRAY(#)=<data line>
  ;"    ERRARRAY(#,"INFO")=Fileman error array, or error message
  ;"  CHGLOG: PASS BY REFERENCE.  An array to receive record of changes made to database
  ;"  EXTARR2: OPTIONAL -- PASS BY REFERENCE.  An array with social security numbers (and more),
  ;"      as created by XTRCTEX2()
  ;"  EXTARR3: PASS BY REFERENCE.  An array additional Sequel Info,
  ;"      as created by XTRCTEX3()
  ;"  DUZ: The user who will recieve alerts of errors
  ;"Output: Data is put into database, IF it is not there already.
  ;"Result: 1=OK To continue; -1^Message if error, or to abort or bad data
  ;"NEW XFn
  SET ^TMP("ERROR")=DUZ
  NEW PTINFO,ONEERRARRAY
  NEW RESULT SET RESULT=1
  NEW AUTOREG SET AUTOREG=1
  SET RESULT=$$PARSLINE(.ONELINE,.PTINFO,.EXTARR2,.EXTARR3)
  ;"Result: 1=OK To continue; 2 skip, but don't store as error;  -1^Message -- abort or bad data;
  IF +RESULT=2 SET RESULT=1 GOTO PPTDN  ;"//kt 7/30/15
  IF +RESULT'=1 DO  GOTO PPTL1
  . SET ONEERRARRAY(0)=$PIECE(RESULT,"^",2)
  IF $GET(PTINFO("FACILITY"))="SAMPLE" GOTO PPTDN
  IF $GET(PTINFO("ACTIVE"))="N" GOTO PPTDN
  NEW TEMP SET TEMP=$$UPDATEDB(.PTINFO,AUTOREG,.ONEERRARRAY,.CHGLOG)
  IF TEMP'=0 GOTO PPTDN
PPTL1 ;
  NEW COUNT SET COUNT=+$GET(ERRARRAY)+1
  SET ERRARRAY=COUNT
  SET ERRARRAY(COUNT)=ONELINE
  MERGE ERRARRAY(COUNT,"INFO")=ONEERRARRAY
  DO ALERTERR^TMGSEQL2(ONELINE,.PTINFO,.ONEERRARRAY,DUZ)
  ;
PPTDN  ;
  QUIT RESULT
  ;
PARSLINE(ONELINE,ARRAY,EXTARR2,EXTARR3) ;
  ;"Purpose: To process one line from patient demographics file.
  ;"   Also gets data into an acceptible format.
  ;"Input: ONELINE -- One line from CVS demographics file. (Format as per PROCESPT)
  ;"   Format is as follows, *** all on one line (comma delimited)
  ;"   01- patient_seq_num,
  ;"   02- facility_short_name,
  ;"   03- pat_last_name,
  ;"   04- pat_first_name,
  ;"   05- pat_account_num,
  ;"   06- pat_address,
  ;"   07- state,
  ;"   08- resp_last_name,
  ;"   09- resp_first_name,
  ;"   10- facility_seq_num,
  ;"   11- register_date,
  ;"   12- location_name,
  ;"   13- city,
  ;"   14- provider_short_name,
  ;"   15- zipcode,
  ;"   16- class_name,
  ;"   17- pat_dob,
  ;"   18- ref_prov_short_name,
  ;"   19- pat_tel_num,
  ;"   20- last_visit_days,
  ;"   21- name,
  ;"   22- description
  ;"   23- active_flag
  ;"   ADDENDUM:
  ;"     sometimes SEX will be appended to line.  Format:
  ;"     previous data^MALE or previous data^FEMALE
  ;"     sometimes SSN will be appended to line.  Format:
  ;"     previous data^(sex)^SSNUM
  ;"   NOTE: IF PASSED BY REFERENCE, then line may be altered such that SSN is
  ;"  added as a 3rd piece, using ^ as a delimiter. (2nd piece used elsewhere
  ;"  to store sex.)
  ;"  When processing line, IF EXTARR2 doesn't provide a SSN for patient, then
  ;"  this 3rd piece can provide the SSN
  ;"  NOTE: now EXTARR3 is used to get SEX for patient if missing.
  ;"       ARRAY -- PASS BY REFERENCE. And OUT parameter.  Any prior data killed.
  ;"       Note: uses TMGINVALID (globally scoped var defined in this module)
  ;"       EXTARR2: OPTIONAL -- PASS BY REFERENCE.  An array with social security numbers (and more),
  ;"   as created by XTRCTEX2()
  ;"       EXTARR3: OPTIONAL -- PASS BY REFERENCE.  An array additional Sequel Info,
  ;"  as created by XTRCTEX3()
  ;"Output: ARRAY is filled with Format as follows (note not all data used):
  ;"  ARRAY("FACILITY"),  to hold 02- facility_short_name
  ;"  ARRAY("LAST NAME"), to hold 03- pat_last_name,
  ;"  ARRAY("FIRST NAME"), to hold 04- pat_first_name,
  ;"  ARRAY("PMS ACCOUNT NUM"), to hold 05- pat_account_num,
  ;"  ARRAY("ADDRESS1"), to hold 06- pat_address,
  ;"  ARRAY("ADDRESS2"), to hold 06- pat_address,
  ;"  ARRAY("ADDRESS3"), to hold 06- pat_address,
  ;"  ARRAY("STATE"), to hold 07- state,
  ;"  ARRAY("RESP LAST NAME"), to hold 08- resp_last_name,
  ;"  ARRAY("RESP FIRST NAME"), to hold 09- resp_first_name,
  ;"  ARRAY("CITY"), to hold 13- city,
  ;"  ARRAY("PROVIDER"), to hold 14- provider_short_name,
  ;"  ARRAY("ZIP CODE"), to hold 15- zipcode,
  ;"  ARRAY("DOB"), to hold 17- pat_dob,
  ;"  ARRAY("PHONE NUM"), to hold 19- pat_tel_num,
  ;"  ARRAY("SEX"), to hold Patient sex, if provided
  ;"  ARRAY("SSNUM")=Social security number
  ;"  ARRAY("FULL NAME")=FIRSTNAME LASTNAME (DOB)
  ;"  ARRAY("FULL NAME2")=LASTNAME,FIRSTNAME (DOB)
  ;"  ARRAY("FULL NAME3")=LASTNAME,FIRSTNAME
  ;"  ARRAY("ACTIVE")='Y' or 'N'  -- If 'N', then patient should be skipped
  ;"  ARRAY("LANG CODE")=LANGCODE (See XTRCTEX2 for codes used) <-- SHOULD BE IN EXTERNAL FORMAT
  ;"  ARRAY("RACE CODE")=RACECODE (See XTRCTEX2 for codes used)
  ;"  ARRAY("ETHNIC CODE")=ETHNCODE (See XTRCTEX2 for codes used)
  ;"  ARRAY("MARITAL")   = (data from 3rd Sequel export file)
  ;"  ARRAY("EMAIL")     = (data from 3rd Sequel export file)
  ;"  ARRAY("PHONE HOME")= (data from 3rd Sequel export file)
  ;"  ARRAY("PHONE WORK")= (data from 3rd Sequel export file)
  ;"  ARRAY("PHONE EXT") = (data from 3rd Sequel export file)
  ;"  ARRAY("PHONE CELL")= (data from 3rd Sequel export file)
  ;"old --> Result: 1=OK To continue; 0=abort or bad data; -1 skip, but don't store as error
  ;"Result: 1=OK To continue; 2 skip, but don't store as error;  -1^Message -- abort or bad data;
  ;  
  NEW TEMP,RESULT SET RESULT=1
  SET ONELINE=$TRANSLATE($GET(ONELINE),"""","'") ;"  convert " to ' to avoid fileman error
  KILL ARRAY
  SET ONELINE=$TRANSLATE(ONELINE,$C(13,10),"") ;"Strip any CRLF
  SET ARRAY("FACILITY")=$PIECE(ONELINE,",",2)
  SET ARRAY("LAST NAME")=$$TRIM^XLFSTR($PIECE(ONELINE,",",3))
  SET ARRAY("FIRST NAME")=$$TRIM^XLFSTR($PIECE(ONELINE,",",4))
  SET ARRAY("PMS ACCOUNT NUM")=$PIECE(ONELINE,",",5)
  SET ARRAY("ADDRESS1")=$PIECE(ONELINE,",",6)
  SET ARRAY("STATE")=$PIECE(ONELINE,",",7)
  SET ARRAY("RESP LAST NAME")=$PIECE(ONELINE,",",8)
  SET ARRAY("RESP FIRST NAME")=$PIECE(ONELINE,",",9)
  SET ARRAY("CITY")=$$TRIM^XLFSTR($PIECE(ONELINE,",",13),"""")
  SET ARRAY("PROVIDER")=$PIECE(ONELINE,",",14)
  SET ARRAY("ZIP CODE")=$PIECE(ONELINE,",",15)
  SET ARRAY("ACTIVE")=$EXTRACT($PIECE(ONELINE,",",23),1)
  NEW DOB SET DOB=$PIECE(ONELINE,",",17)
  SET DOB=$$TRIM^XLFSTR(DOB)
  IF DOB="pat_dob" SET RESULT=2 GOTO PLNDN  ;"Must be spreadsheet header line.  Ignore. 
  SET DOB=$PIECE(DOB," ",1)  ;" '03/09/05 00:00' --> '03/09/05'
  SET ARRAY("DOB")=DOB
  SET ARRAY("PHONE NUM")=$PIECE(ONELINE,",",19)
  ;"Note: if SEX not found in line below, then code later below will fix
  SET ARRAY("SEX")=$PIECE(ONELINE,"^",2)
  ;
  SET ARRAY("FULL NAME")=ARRAY("FIRST NAME")_" "_ARRAY("LAST NAME")_" ("_ARRAY("DOB")_")"
  SET ARRAY("FULL NAME2")=ARRAY("LAST NAME")_","_ARRAY("FIRST NAME")_" ("_ARRAY("DOB")_")"
  SET ARRAY("FULL NAME3")=ARRAY("LAST NAME")_","_ARRAY("FIRST NAME")
  ;
  ;"Do a lookup on abreviattion for ALL states, convert to external format
  NEW DIC,X,Y
  SET DIC=5 ;"STATE file
  SET DIC(0)="M"
  SET X=ARRAY("STATE")
  DO ^DIC
  SET ARRAY("STATE")=$PIECE(Y,"^",2)
  ;
  ;"convert Sequel format to VistA format
  IF ARRAY("PROVIDER")'="" DO
  . SET ARRAY("PROVIDER")=$$CONVPROV(ARRAY("PROVIDER"))
  ;"IF ARRAY("PROVIDER")="SKIP" SET RESULT=0 GOTO PLNDN
  IF ARRAY("PROVIDER")="SKIP" SET RESULT=2 GOTO PLNDN
  ;
  ;"  VistA address allows for:
  ;"      .111 -- address line 1
  ;"      .112 -- address line 2
  ;"      .113 -- address line 3
  ;"      BUT, each line must be 3-35 characters
  ;"  Sequel puts this all on one line.
  ;"  SO, I need to divide the Sequel line IF not 3-35
  NEW VALUE SET VALUE=ARRAY("ADDRESS1")
  IF $LENGTH(VALUE)'<35 DO
  . NEW S1,S2
  . DO NICESPLT^TMGSTUT3(VALUE,35,.S1,.S2,3)
  . SET ARRAY("ADDRESS1")=S1
  . IF $LENGTH(S2)'<35 DO
  . . DO NICESPLT^TMGSTUT3(S1,35,.S1,.S2,3)
  . . SET ARRAY("ADDRESS2")=S1  ;"<-- is this correct?
  . . IF S2'="" SET ARRAY("ADDRESS3")=$EXTRACT(S2,1,35)
  . ELSE  SET ARRAY("ADDRESS2")=S2
  ;
  ;"Ensure proper length of city.
  SET ARRAY("CITY")=$EXTRACT(ARRAY("CITY"),1,15)
  IF $LENGTH(ARRAY("CITY"))=1 SET ARRAY("CITY")=ARRAY("CITY")_" "
  ;
  ;"Ensure proper length of phone
  IF $LENGTH(ARRAY("PHONE NUM"))<7 KILL ARRAY("PHONE NUM")
  ;
  NEW ACCTNUM SET ACCTNUM=$GET(ARRAY("PMS ACCOUNT NUM"))
  NEW EXTRINFO SET EXTRINFO=$GET(EXTARR2(ACCTNUM))
  NEW SSNUM SET SSNUM=$PIECE(EXTRINFO,"^",1)
  IF SSNUM=999999999 SET SSNUM=0
  IF +SSNUM=0 DO   ;"see IF 3rd ^ piece holds SSNUM data
  . SET SSNUM=$PIECE(ONELINE,"^",3) ;"note this won't overwrite valid data from EXTARR2()
  IF SSNUM>0 DO
  . SET ARRAY("SSNUM")=SSNUM
  . SET $PIECE(ONELINE,"^",3)=SSNUM
  NEW LANGCODE SET LANGCODE=$PIECE(EXTRINFO,"^",2)
  IF LANGCODE'="" SET ARRAY("LANG CODE")=LANGCODE ;"<-- SHOULD BE IN EXTERNAL FORMAT
  NEW RACECODE SET RACECODE=$PIECE(EXTRINFO,"^",3)
  IF RACECODE'="" SET ARRAY("RACE CODE")=RACECODE
  NEW ETHNCODE SET ETHNCODE=$PIECE(EXTRINFO,"^",4)
  IF ETHNCODE'="" SET ARRAY("ETHNIC CODE")=ETHNCODE
  ;
  ;"Added 12/16/14
  IF SSNUM>0 DO
  . NEW ONEXTRA MERGE ONEXTRA=EXTARR3(SSNUM)
  . IF $DATA(ONEXTRA)=0 DO  QUIT
  . . ;"If 'MISSING ENTRY:' changed, also change in HANDLE^TMGSEQL2
  . . ;" 6/5/15 don't generate error at this time SET RESULT="-1^MISSING ENTRY: Demographics3 file missing entry for SSN='"_SSNUM_"'"
  . NEW IDX 
  . FOR IDX="MARITAL","EMAIL","PHONE HOME","PHONE WORK","PHONE EXT","PHONE CELL" DO
  . . SET ARRAY(IDX)=$GET(ONEXTRA(IDX))
  . IF $GET(ARRAY("SEX"))="" SET ARRAY("SEX")=$GET(ONEXTRA("SEX"))
  . IF $GET(ARRAY("SEX"))="" KILL ARRAY("SEX")  ;"ADDED 6/11/15
  IF +$GET(ARRAY("ZIP CODE"))=0 SET ARRAY("ZIP CODE")="" 
  ;
  IF +RESULT'=-1 DO
  . IF $$INVALPTN(ARRAY("FIRST NAME"),ARRAY("LAST NAME"))=1 SET RESULT=2 QUIT
  . IF $$INACTVPT(ARRAY("PMS ACCOUNT NUM"),.EXTARR2)=1 SET RESULT=2 QUIT
  ;
PLNDN   ;
  QUIT RESULT
  ;
PARSLIN3(ONELINE,ARRAY) ;
  ;"Purpose: To process one line from patient demographics file.
  ;"   Also gets data into an acceptible format.
  ;"Input: ONELINE -- One line from CVS demographics file. (Format as per PROCESPT)
  ;"   Expected format (pieces, comma-separated) as follows:
  ;"   01. -- SSN,
  ;"   02. -- FNAME,
  ;"   03. -- LNAME,
  ;"   04. -- MNAME,
  ;"   05. -- DOB,
  ;"   06. -- SEX,
  ;"   07. -- MARITAL,
  ;"   08. -- EMAIL,
  ;"   09. -- ADDRESS1,
  ;"   10. -- ADDRESS2,
  ;"   11. -- CITY,
  ;"   12. -- STATE,
  ;"   13. -- ZIP,
  ;"   14. -- ZIP4,
  ;"   15. -- HOMENUMBER,
  ;"   16. -- WORKNUMBER,
  ;"   17. -- EXT,
  ;"   18. -- CELLNUMBER    
  ;"       ARRAY -- PASS BY REFERENCE. And OUT parameter.  Any prior data killed.
  ;"Output: ARRAY is filled with Format as follows (note not all data used):      
  ;"  ARRAY("SSN")
  ;"  ARRAY("FIRST NAME")
  ;"  ARRAY("LAST NAME")
  ;"  ARRAY("MIDDLE NAME")
  ;"  ARRAY("DOB")
  ;"  ARRAY("SEX")
  ;"  ARRAY("MARITAL")
  ;"  ARRAY("EMAIL")
  ;"  ARRAY("ADDRESS1")
  ;"  ARRAY("ADDRESS2")
  ;"  ARRAY("CITY")
  ;"  ARRAY("STATE")
  ;"  ARRAY("ZIP")
  ;"  ARRAY("ZIP4")
  ;"  ARRAY("PHONE HOME")
  ;"  ARRAY("PHONE WORK")
  ;"  ARRAY("PHONE EXT")
  ;"  ARRAY("PHONE CELL")    
  ;"Result: None
  NEW TEMP
  SET ONELINE=$TRANSLATE($GET(ONELINE),"""","'") ;"  convert " to ' to avoid fileman error
  SET ONELINE=$TRANSLATE(ONELINE,$C(13,10),"") ;"Strip any CRLF
  NEW PARTS
  SET PARTS(1)="SSN"
  SET PARTS(2)="FIRST NAME"
  SET PARTS(3)="LAST NAME"
  SET PARTS(4)="MIDDLE NAME"
  SET PARTS(5)="DOB"
  SET PARTS(6)="SEX"
  SET PARTS(7)="MARITAL"
  SET PARTS(8)="EMAIL"
  SET PARTS(9)="ADDRESS1"       
  SET PARTS(10)="ADDRESS2"
  SET PARTS(11)="CITY"
  SET PARTS(12)="STATE"
  SET PARTS(13)="ZIP"
  SET PARTS(14)="ZIP4"
  SET PARTS(15)="PHONE HOME"
  SET PARTS(16)="PHONE WORK"
  SET PARTS(17)="PHONE EXT"
  SET PARTS(18)="PHONE CELL"    
  ;      
  KILL ARRAY
  NEW IDX SET IDX=0 
  FOR  SET IDX=$ORDER(PARTS(IDX)) QUIT:+IDX'>0  DO
  . NEW NAME SET NAME=$GET(PARTS(IDX)) QUIT:NAME=""
  . SET ARRAY(NAME)=$$TRIM^XLFSTR($PIECE(ONELINE,",",IDX))
  ;
  ;"Do a lookup on abreviattion for ALL states, convert to external format
  NEW DIC,X,Y
  SET DIC=5 ;"STATE file
  SET DIC(0)="M"
  SET X=ARRAY("STATE")
  DO ^DIC
  SET ARRAY("STATE")=$PIECE(Y,"^",2)
  ;
  ;"Ensure proper length of city.
  SET ARRAY("CITY")=$EXTRACT(ARRAY("CITY"),1,15)
  IF $LENGTH(ARRAY("CITY"))=1 SET ARRAY("CITY")=ARRAY("CITY")_" "
  ;
  ;"Ensure proper phone numbers  
  FOR IDX="PHONE HOME","PHONE WORK","PHONE EXT","PHONE CELL" DO
  . IF $DATA(ARRAY(IDX))=0 QUIT
  . IF ARRAY(IDX)[")-   -" SET ARRAY(IDX)="" 
  . IF $LENGTH(ARRAY(IDX))<7 KILL ARRAY(IDX) QUIT
  . SET ARRAY(IDX)=$TRANSLATE(ARRAY(IDX),")- ","") ;"Remove formatting chars
  SET ARRAY("SSN")=$TRANSLATE(ARRAY("SSN"),"-","")  ;"Remove dashes from SSN   
  ;
  IF +$GET(ARRAY("ZIP"))=0 KILL ARRAY("ZIP") 
  IF +$GET(ARRAY("ZIP4"))=0 KILL ARRAY("ZIP4") 
PL3Done ;
  QUIT
  ;
CONVPROV(SEQLPROV)  ;"Convert Provider
  ;"Purpose: To convert Sequel provider shortname to VistA file 200 name.
  ;"Input: SEQLPROV
  ;"Result: VistA provider name (string), or "" IF not found, or "SKIP" IF not to be used
  NEW RESULT SET RESULT=""
  IF $$INVALPRV(SEQLPROV) SET RESULT="SKIP" GOTO CPRDN
  IF SEQLPROV="SAMPLE" SET RESULT="SKIP" GOTO CPRDN
  NEW TMGARRAY,TMGMSG
  DO FIND^DIC(200,,".01",,SEQLPROV,"*","TMG",,,"TMGARRAY","TMGMSG")
  IF +TMGARRAY("DILIST",0)>0 DO
  . SET RESULT=TMGARRAY("DILIST",1,1)
  ELSE  DO
  . NEW DIC SET DIC=200
  . ;"try converting name and doing quiet lookup (KTOPPEN->TOPPEN,K)
  . SET X=$EXTRACT(SEQLPROV,2,99)_","_$EXTRACT(SEQLPROV,1)
  . DO ^DIC
  . IF (+Y=-1)&(1=0) DO  ;"<--- FEATURE TURNED OFF.  If not found, don't ask (no longer needed)
  . . IF $DATA(TMGINVALID(SEQLPROV))'=0 QUIT
  . . WRITE !,"Please help match the Sequel 'shortname' to a VistA provider name.",!
  . . WRITE "This should have to be done only once.",!
  . . WRITE "Enter ^ IF the provider name is not valid.",!
  . . WRITE "Please enter VistA provider name for: '",SEQLPROV,"'",!
  . . SET DIC(0)="AEQM"
  . . DO ^DIC
  . . WRITE !
  . IF +Y>-1 DO
  . . NEW DFN SET DFN=+Y
  . . NEW TMGFDA SET TMGFDA(200,DFN_",",22702)=SEQLPROV
  . . KILL TMGMSG
  . . DO FILE^DIE(,"TMGFDA","TMGMSG")  ;"ignore errors
  . . SET RESULT=$PIECE(Y,"^",2)
  . ELSE  DO
  . . SET TMGINVALID(SEQLPROV)=""
CPRDN   ;
  QUIT RESULT
  ;
INVALPTN(FNAME,LNAME) ;
  ;"Purpose: To determine IF the Patient name is invalid (i.e. CAP TOPPENBERG, or INSURANCE INSURANCE etc.)
  ;"Input: FNAME,LNAME -- the first and last names
  ;"Result: 1 IF name is invalid, 0 IF OK name
  NEW RESULT SET RESULT=0      
  IF FNAME="CAP" DO  ;"screen out 'CAP TOPPENBERG' etc ?? entries ??
  . IF LNAME="TOPPENBERG" SET RESULT=1 QUIT
  . NEW DIC SET DIC=200
  . SET DIC(0)="M"
  . SET X=LNAME
  . DO ^DIC               
  . IF +Y'>0 SET RESULT=1             
  IF (FNAME="INSURANCE")&(LNAME="INSURANCE") SET RESULT=1
  QUIT RESULT
  ;
INACTVPT(PMSACCTNUM,EXTARR2) ;
  ;"Purpose: to determine IF patient is inactive, and should be skipped.
  ;"      This is determined by testing for existence of AccountNumber in EXTARR2.
  ;"      EXTARR2 is created from the 2nd demographics file.  This is a list of ACTIVE patients,
  ;"      which is different from the 1st demographics file--which holds ALL patients.     
  ;"Input: PMSACCTNUM -- as stored in PTINFO("PMS ACCOUNT NUM")
  ;"       EXTARR2: PASS BY REFERENCE.  Format: EXTARR2(SequelAccountNumber)=SSNUM^LANGCODE^RACECODE^EthnicityCode
  ;"Result: 1 IF patient is INACTIVE, and should be skipped.
  ;"  0 IF OK to use
  NEW SSNUM SET SSNUM=$PIECE($GET(EXTARR2(PMSACCTNUM)),"^",1) ;"//Piece #1=SSNUM #
  NEW RESULT SET RESULT=0
  IF +SSNUM=0 DO
  . ;"SET RESULT=1
  QUIT RESULT
  ;
INVALPRV(SEQLPROV)  ;"InvalidProvider
  ;"Purpose: To return IF provider should not be used (i.e. cause data to be skipped)
  ;"Input: SEQLPROV
  ;"Result: 0: OK to use provider
  ;"  1: Don't use provider
  NEW RESULT SET RESULT=0
  IF SEQLPROV="SAMPLE" SET RESULT=1
  IF SEQLPROV="GREENEVILLE" SET RESULT=1
  IF SEQLPROV="AFOSTER" SET RESULT=1
  IF SEQLPROV="AFTON" SET RESULT=1
  IF SEQLPROV="JWRIGHT" SET RESULT=1  ;"not an active provider
  ;"These providers are leaving group, so don't import their data.
  IF SEQLPROV="CPERRY" SET RESULT=1
  IF SEQLPROV="OSWARNER" SET RESULT=1
  IF SEQLPROV="SGILES" SET RESULT=1
  IF SEQLPROV="SPENNY" SET RESULT=1
  IF SEQLPROV="TFULLER" SET RESULT=1
  IF SEQLPROV="PROTIME" SET RESULT=1
  QUIT RESULT
  ;     
UPDATEDB(PTINFO,AUTOREG,ONEERRARRAY,CHGLOG) ;
  ;"Purpose: To put that data from the PTINFO array into the database (if needed)
  ;"Input: 
  ;"  PTINFO -- array (PASS BY REFERENCE), with the following items being used:
  ;"    PTINFO("LAST NAME"), to hold 03- pat_last_name,
  ;"    PTINFO("FIRST NAME"), to hold 04- pat_first_name,
  ;"    PTINFO("PMS ACCOUNT NUM")  ----> field 22701 (custom field)
  ;"    PTINFO("ADDRESS")       ----> field .111
  ;"    PTINFO("STATE")         ----> field .115
  ;"    PTINFO("CITY")          ----> field .114
  ;"    PTINFO("ZIP CODE")      ----> field .1112
  ;"    PTINFO("PHONE NUM")     ----> field .131
  ;"    PTINFO("PROVIDER")      ----> field .1041
  ;"    PTINFO("SSNUM")         ----> field .09
  ;"    PTINFO("SEX")           ----> field .02
  ;"    PTINFO("DOB")           ----> field .03
  ;"    PTINFO("LANG CODE")     ----> field 256000
  ;"    PTINFO("RACE CODE")     ----> field #2 (a multiple)
  ;"    PTINFO("ETHNIC CODE")   ----> field #6 (a multiple)
  ;"    PTINFO("MARITAL")       ----> field .05   
  ;"    PTINFO("EMAIL")         ----> field .133
  ;"    PTINFO("PHONE HOME")    ----> (not used)
  ;"    PTINFO("PHONE WORK")    ----> field .132
  ;"    PTINFO("PHONE EXT")     ----> (not used)
  ;"    PTINFO("PHONE CELL")    ----> field .134
  ;"  AUTOREG: IF 1, then patient will be automatically added/registered
  ;"  ONEERRARRAY: PASS BY REFERENCE.  An OUT PARAMETER.  Array to receive failed data lines.
  ;"    ONEERRARRAY(0)=Message
  ;"    ONEERRARRAY("DIERR")=Fileman error array
  ;"  CHGLOG: PASS BY REFERENCE.  An array to receive record of changes made to database
  ;"Output: Data is put into database, IF it is not there already.
  ;"Result: 1 successful completion, 0=error  (error message output via ONEERRARRAY)
  NEW RESULT SET RESULT=1
  NEW ENTRY,NAME,TMGDOB,DFN,TMGARRAY,TMGMSG,NEWINFO,IENS,INDEX,FIELDS
  KILL ONEERRARRAY
  ;"NEW TMGDEBUG SET TMGDEBUG=-1 ;"//EXTRA QUIET mode --> shut down TMGDBAPI messages
  SET FIELDS(22701)="PMS ACCOUNT NUM"
  SET FIELDS(.111)="ADDRESS1"
  SET FIELDS(.112)="ADDRESS2"
  SET FIELDS(.113)="ADDRESS3"
  SET FIELDS(.115)="STATE"
  SET FIELDS(.114)="CITY"
  SET FIELDS(.1112)="ZIP CODE"
  SET FIELDS(.131)="PHONE NUM"
  SET FIELDS(.1041)="PROVIDER"
  SET FIELDS(.02)="SEX"
  SET FIELDS(.09)="SSNUM"
  SET FIELDS(256000)="LANG CODE"  ;"<-- SHOULD BE IN EXTERNAL FORMAT
  SET FIELDS(.05)="MARITAL"
  SET FIELDS(.133)="EMAIL"
  SET FIELDS(.132)="PHONE WORK"
  SET FIELDS(.134)="PHONE CELL" 
  NEW IDX,TEMP SET IDX="",TEMP=""
  FOR  SET IDX=$ORDER(FIELDS(IDX)) QUIT:IDX=""  SET:TEMP'="" TEMP=TEMP_";" SET TEMP=TEMP_IDX
  SET FIELDS=TEMP
  ;
  SET NAME=$GET(PTINFO("LAST NAME"))_","_$GET(PTINFO("FIRST NAME"))
  SET NAME=$$FRMTNAME^TMGMISC(NAME)
  SET TMGDOB=$GET(PTINFO("DOB"))
  ;
  SET ENTRY(.01)=NAME
  SET ENTRY(.03)=TMGDOB
  IF $GET(PTINFO("SEX"))'="" SET ENTRY(.02)=$GET(PTINFO("SEX"))
  IF $GET(PTINFO("SEX"))="" KILL PTINFO("SEX")  ;"ADDED 6/11/15
  SET ENTRY(.09)=$GET(PTINFO("SSNUM"))
  ;
  SET DFN=$$GETDFN(.PTINFO)
  IF (DFN<1)&($$GOODDATA(.PTINFO)=0) DO  GOTO UDBDN  ;"Does patient have info needed to register as new patient?
  . ;"NOTE: When patient can not be auto-registered, don't create error or alert, just quit  //kt 7/31/15
  . SET AUTOREG=0
  IF (DFN<1)&($GET(AUTOREG)=1) DO
  . IF $GET(ENTRY(.02))="" DO  ;"autopick gender IF missing
  . . NEW AUTOPICK
  . . SET AUTOPICK=$$GET1^DIQ(22711,"1,","PICK GENDER FROM NAME?","I")
  . . IF AUTOPICK'=1 QUIT
  . . SET ENTRY(.02)=$$GETSEX^TMGSEQL2($GET(PTINFO("FIRST NAME")))
  . ;"OK, can't find, so will add NEW patient.
  . SET DFN=$$ADDNEWPAT^TMGGDFN(.ENTRY)
  . IF +DFN>0 SET CHANGLOG(NAME_" "_TMGDOB,0)="ADDED PATIENT: "_NAME_" "_TMGDOB
  IF +DFN<1 DO  GOTO UDBDN  ;"failure
  . SET RESULT=0
  . NEW ERRMSG SET ERRMSG=$PIECE(DFN,"^",2)
  . IF ERRMSG="" SET ERRMSG=$$NAMEERR^TMGSEQL2(.ONEERRARRAY)  ;"get name IF DIERR encountered. 
  . IF ERRMSG="" SET ERRMSG="PATIENT NOT IN DATABASE:"  ;"if changed, also change in TMGSEQL2.m
  . SET ONEERRARRAY(0)=$PIECE(DFN,"^",2)
  SET IENS=DFN_","
  ;
  ;"Use DFN(IEN in file 2) to get data into database
  DO GETS^DIQ(2,IENS,FIELDS,"","TMGARRAY","TMGMSG") ;"get EXTERNAL values
  IF $DATA(TMGMSG("DIERR"))'=0 DO  GOTO UDBDN   ;"check for errors.
  . SET RESULT=0
  . SET ONEERRARRAY(0)="Fileman error"
  . MERGE ONEERRARRAY=TMGMSG("DIERR")
  KILL TMGMSG
  ;
  ;"If any data in data base differs from ARRAY, setup NEWINFO
  NEW UPDATENEEDED SET UPDATENEEDED=0
  NEW ABORT SET ABORT=0
  SET INDEX=""
  ;"FOR  DO  QUIT:(+INDEX'>0)!(ABORT=1)
  FOR  SET INDEX=$ORDER(FIELDS(INDEX)) QUIT:(+INDEX'>0)!(ABORT=1)  DO
  . NEW FIELD SET FIELD=FIELDS(INDEX)
  . ;"IF $DATA(PTINFO(FIELD)),$GET(TMGARRAY(2,IENS,INDEX))'=$GET(PTINFO(FIELD)) DO
  . NEW PRIORVAL SET PRIORVAL=$GET(TMGARRAY(2,IENS,INDEX))
  . NEW NEWVAL SET NEWVAL=$GET(PTINFO(FIELD))
  . IF (NEWVAL'=PRIORVAL)&($data(PTINFO(FIELD))'=0) DO
  . . NEW VALUE SET VALUE=$GET(PTINFO(FIELD))
  . . IF INDEX=.1112 DO
  . . . IF +VALUE'=0 SET NEWINFO(INDEX)=VALUE
  . . . ELSE  SET NEWINFO(INDEX)="@"
  . . IF INDEX=256000 DO
  . . . ;"GETS^DIQ outputs the .02 field, but .01 value is required for input.  
  . . . IF $LENGTH(VALUE)'=2 QUIT
  . . . NEW IEN SET IEN=+$ORDER(^DI(.85,"C",VALUE,"")) QUIT:IEN'>0
  . . . NEW TEMP SET TEMP=$PIECE($GET(^DI(.85,IEN,0)),"^",1) ;".01 value
  . . . IF TEMP'="" SET NEWINFO(INDEX)=TEMP
  . . ELSE  IF (INDEX=.09)&(+VALUE'=0)&(+TMGARRAY(2,IENS,INDEX)'=0) DO
  . . . IF TMGARRAY(2,IENS,INDEX)["P" DO  QUIT
  . . . . SET NEWINFO(INDEX)=VALUE
  . . . ;"we have CONFLICTING SOCIAL SECURITY NUMBERS --> PROBLEM...
  . . . ;"NOTE! IF error message format is changed, also change in TMGSEQL2
  . . . NEW MSG SET MSG="CONFLICTING SS-NUMBERS: Sequel#="_PTINFO(FIELD)
  . . . SET MSG=MSG_" vs. VistA#="_TMGARRAY(2,IENS,INDEX)
  . . . SET ONEERRARRAY(0)=MSG
  . . . ;" SET ONEERRARRAY(0)="CONFLICTING SS-NUMBERS: " 
  . . . ;" SET ONEERRARRAY(0)=ONEERRARRAY(0)_"Sequel#="_PTINFO(FIELD)
  . . . ;" SET ONEERRARRAY(0)=ONEERRARRAY(0)_" vs. VistA#="_TMGARRAY(2,IENS,INDEX)
  . . . SET ABORT=1,RESULT=0
  . . ELSE  SET NEWINFO(INDEX)=VALUE
  . . SET UPDATENEEDED=1
  ;
  IF ABORT=1 GOTO UDBDN
  IF UPDATENEEDED=0 GOTO UDDB2
  ;
  ;"Setup FDA array for database update
  NEW TMGFDA
  SET INDEX=$ORDER(NEWINFO(""))
  IF INDEX'=""  DO
  . FOR  DO  QUIT:(+INDEX'>0)
  . . SET TMGFDA(2,IENS,INDEX)=NEWINFO(INDEX)
  . . SET INDEX=$ORDER(NEWINFO(INDEX))
  . DO FILE^DIE("E","TMGFDA","TMGMSG")
  . IF $DATA(TMGMSG("DIERR"))'=0 DO  ;"GOTO UDBDN
  . . SET RESULT=0
  . . SET ONEERRARRAY(0)="Fileman error"
  . . MERGE ONEERRARRAY=TMGMSG("DIERR")
  MERGE CHANGLOG($GET(NAME,"?")_" "_$GET(TMGDOB,"?"),1)=NEWINFO
  ;
UDDB2   ;"Synchronize Race & Ethnicity information stored.
  NEW DEMOFIELD
  FOR DEMOFIELD=2,6 QUIT:(RESULT=0)  DO
  . NEW DBCODES SET DBCODES=$$GTDEMOCD(DFN,DEMOFIELD)
  . NEW PMSCODES
  . IF DEMOFIELD=2 SET PMSCODES=$GET(PTINFO("RACE CODE"))
  . ELSE  SET PMSCODES=$GET(PTINFO("ETHNIC CODE"))
  . IF PMSCODES=DBCODES QUIT
  . NEW CODES2ADD,CODES2DEL
  . SET (CODES2ADD,CODES2DEL)=""
  . FOR INDEX=1:1:$LENGTH(PMSCODES) DO
  . . NEW ONEPMSCD SET ONEPMSCD=$EXTRACT(PMSCODES,INDEX) QUIT:ONEPMSCD=""
  . . IF DBCODES'[ONEPMSCD SET CODES2ADD(ONEPMSCD)=""
  . FOR INDEX=1:1:$LENGTH(DBCODES) DO
  . . NEW ONEDBCOD SET ONEDBCOD=$EXTRACT(DBCODES,INDEX) QUIT:ONEDBCOD=""
  . . IF PMSCODES'[ONEDBCOD SET CODES2DEL(ONEDBCOD)=""
  . SET INDEX=""
  . FOR  SET INDEX=$ORDER(CODES2DEL(INDEX)) QUIT:(INDEX="")!(RESULT=0)  DO
  . . SET RESULT=$$DELDEMCD(DFN,DEMOFIELD,INDEX,.ONEERRARRAY)
  . FOR  SET INDEX=$ORDER(CODES2ADD(INDEX)) QUIT:(INDEX="")!(RESULT=0)  DO
  . . SET RESULT=$$ADDDEMCD(DFN,DEMOFIELD,INDEX,.ONEERRARRAY)
UDBDN   ;
  QUIT RESULT
  ;
GOODDATA(PTINFO)  ;"Does patient have info needed to register as new patient?
  NEW RESULT SET RESULT=1
  NEW IDX FOR IDX="LAST NAME","FIRST NAME","SSNUM","SEX","DOB" DO  QUIT:RESULT=0
  . IF $GET(PTINFO(IDX))="" SET RESULT=0
  IF +$GET(PTINFO("SSNUM"))'>0 SET RESULT=0
  QUIT RESULT
  ;
GETDFN(PTINFO)
  ;"Purpose: Serve as interface to ^TMGGDFN functions (using PTINFO as input)
  ;"Input: PTINFO, Array of PTINFO, as defined in UPDATEDB, and created by PARSLINE
  ;"Result: the IEN in file 2 (i.e. DFN) IF found, otherwise 0 IF not found.
  NEW ENTRY,NAME,DOB,DFN
  SET NAME=$GET(PTINFO("LAST NAME"))_","_$GET(PTINFO("FIRST NAME"))
  SET NAME=$$FRMTNAME^TMGMISC(NAME)
  SET DOB=$GET(PTINFO("DOB"))
  SET ENTRY(.01)=NAME
  SET ENTRY(.03)=DOB
  SET ENTRY(.02)=$GET(PTINFO("SEX"))
  SET ENTRY(.09)=$GET(PTINFO("SSNUM"))
  SET DFN=+$$LOOKUPPAT^TMGGDFN(.ENTRY)  ;"get IEN in file 2 of patient
  ;"do an extended search with increasing intensity.
  IF +DFN=0 SET DFN=$$EXTRLKUP^TMGGDFN(.ENTRY,1)
  IF +DFN=0 SET DFN=$$EXTRLKUP^TMGGDFN(.ENTRY,2)
  IF +DFN=0 SET DFN=$$EXTRLKUP^TMGGDFN(.ENTRY,3)
  QUIT DFN
  ;
XTRCTEX2(G2REF,EXTARR2)
  ;"Purpose: To extract info from 2nd demographics file into an array of SSNUMs.
  ;"Input: G2REF - Name of global array holding 2nd demographics file
  ;"        Note: Format of each line is as follows:
  ;"    scratchNum,AccountNumber,LastName,FirstName,SSNUM ... (other data is redundant)
  ;"    i.e. SSNUM is the 5th piece
  ;"       EXTARR2 -- PASS BY REFERENCE.  An OUT parameter.  See format below
  ;"Output: EXTARR2 filled: Format = EXTARR2(SequelAccountNumber)=SSNUM^LANGCODE^RACECODE^EthnicityCode
  ;
  ;"* Language codes that will be sent:  (Matches entries from Fileman file .85)
  ;"  These codes, sent from Sequel, are set up so that they match IEN's etc
  ;"    in VistA
  ;"  Only ONE code may be provided
  ;"    NOTE 12/17/14 -- these codes have to be compared to values
  ;"     already stored in DB.  When calling GETS^DIQ
  ;"    an EXTERNAL form is returned.  So
  ;"         here we will convert to that form.
  ;"        I.e. 1 --> "EN" (the 2 digit code that is returned)
  ;"      1  means --> ENGLISH
  ;"      2  means --> GERMAN
  ;"      3  means --> SPANISH
  ;"      4  means --> FRENCH
  ;"      5  means --> FINNISH
  ;"      6  means --> ITALIAN
  ;"      7  means --> PORTUGUESE
  ;"     10  means --> ARABIC
  ;"     11  means --> RUSSIAN
  ;"     12  means --> GREEK
  ;"     18  means --> HEBREW
  ;"* Race codes that will be sent. (Matches entries from Fileman file 10)
  ;"  One OR MORE codes may be sent.  E.g. 'BW' means both black and white race
  ;"     A   means --> ASIAN
  ;"     B   means --> BLACK OR AFRICAN AMERICAN
  ;"     D   means --> DECLINED TO ANSWER
  ;"     H   means --> NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER
  ;"     K   means --> AMERICAN INDIAN OR ALASKA NATIVE
  ;"     O   means --> OTHER
  ;"     S   means --> HISPANIC
  ;"     U   means --> UNKNOWN BY PATIENT
  ;"     W   means --> WHITE
  ;"*  Ethnicity codes that will be sent. (Matches entries from 10.2)
  ;"  Only ONE code may be provided
  ;"     D   means --> DECLINED TO ANSWER
  ;"     H   means --> HISPANIC OR LATINO
  ;"     N   means --> NOT HISPANIC OR LATINO
  ;"     U   means --> UNKNOWN BY PATIENT
  ;"Result: None
  ;"Note: 3/2/06 modification:
  ;"  An entry for every SequelAccountNumber will be created.  If SSNUM is invalid, it will
  ;"  be converted to 0, but an entry will still be created, i.e.
  ;"        EXTARR2(SequelAccountNumber)=0
  NEW IDX SET IDX=""
  FOR  SET IDX=$ORDER(@G2REF@(IDX)) QUIT:(IDX="")  DO
  . NEW ONELINE,ACCTNUM,SSNUM,LANGETC
  . SET ONELINE=$GET(@G2REF@(IDX))
  . SET ACCTNUM=$PIECE(ONELINE,",",2)
  . IF ACCTNUM="account_num" QUIT  ;"must be a spreadsheat header line.  Ignore
  . SET SSNUM=$$TRIM^XLFSTR($PIECE(ONELINE,",",5))
  . SET LANGETC=$$TRIM^XLFSTR($PIECE(ONELINE,",",7))
  . IF $LENGTH(LANGETC,"^")'=3 SET LANGETC=""
  . NEW INFOSTR SET INFOSTR=0 ;"default value
  . IF +SSNUM'<999999 DO   ;"force at least 6 digits --> allow 0000 11 1111
  . . IF $LENGTH(SSNUM)'=9 DO
  . . . SET EXTARR2("ERRORS",ACCTNUM)=SSNUM  ;"leaves value="" --> not used
  . . ELSE  DO
  . . . ;"set EXTARR2(ACCTNUM)=SSNUM
  . . . SET INFOSTR=SSNUM
  . NEW LANGIEN SET LANGIEN=+$PIECE(LANGETC,"^",1)
  . IF LANGIEN>0 DO
  . . NEW ELANG SET ELANG=$PIECE($GET(^DI(.85,LANGIEN,0)),"^",2)
  . . SET $PIECE(LANGETC,"^",1)=ELANG
  . IF LANGETC'="" SET INFOSTR=INFOSTR_"^"_LANGETC
  . SET EXTARR2(ACCTNUM)=INFOSTR
  QUIT
  ;  
XTRCTEX3(G3REF,EXTARR3) ;
  ;"Purpose: To extract info from 3rd Sequel export file 
  ;"Input: G3REF - Name of global array holding 3rd Sequel export file
  ;"    EXTARR3 -- PASS BY REFERENCE.  An OUT parameter.  See format below
  ;"Output: EXTARR3 will be filled as follows:  
  ;"  EXTARR3(<SSN>,"SSN")=
  ;"  EXTARR3(<SSN>,"FIRST NAME")=
  ;"  EXTARR3(<SSN>,"LAST NAME")=
  ;"  EXTARR3(<SSN>,"MIDDLE NAME")=
  ;"  EXTARR3(<SSN>,"DOB")=
  ;"  EXTARR3(<SSN>,"SEX")=
  ;"  EXTARR3(<SSN>,"MARITAL")=
  ;"  EXTARR3(<SSN>,"EMAIL")=
  ;"  EXTARR3(<SSN>,"ADDRESS1")=
  ;"  EXTARR3(<SSN>,"ADDRESS2")=
  ;"  EXTARR3(<SSN>,"CITY")=
  ;"  EXTARR3(<SSN>,"STATE")=
  ;"  EXTARR3(<SSN>,"ZIP")=
  ;"  EXTARR3(<SSN>,"ZIP4")=
  ;"  EXTARR3(<SSN>,"PHONE HOME")=
  ;"  EXTARR3(<SSN>,"PHONE WORK")=
  ;"  EXTARR3(<SSN>,"PHONE EXT")=
  ;"  EXTARR3(<SSN>,"PHONE CELL")=
  ;"Result: None
  NEW IDX SET IDX=""
  FOR  SET IDX=$ORDER(@G3REF@(IDX)) QUIT:(IDX="")  DO
  . NEW ONELINE,TEMPARR
  . SET ONELINE=$GET(@G3REF@(IDX))
  . IF $PIECE(ONELINE,",",1)="SSN" QUIT  ;"must be a spreadsheat header line.  Ignore
  . DO PARSLIN3(ONELINE,.TEMPARR) ;
  . NEW SSN SET SSN=$GET(TEMPARR("SSN")) QUIT:SSN=""
  . MERGE EXTARR3(SSN)=TEMPARR
  QUIT
  ; 
GTDEMOCD(DFN,FIELD)  ;"GET DEMO CODES
  ;"Return codes for entries in FILE
  ;"Input: DFN -- IEN IN 2
  ;"       FIELD -- should be 2, or 6
  ;"Results: returns CODES for entries in multiples, stored in FILE 2
  NEW RESULT SET RESULT=""
  NEW NODE,P2FILE
  SET FIELD=$GET(FIELD)
  IF FIELD=2 SET NODE=.02,P2FILE=10
  ELSE  IF FIELD=6 SET NODE=.06,P2FILE=10.2
  ELSE  GOTO GDCDN
  NEW IEN SET IEN=0
  FOR  SET IEN=$ORDER(^DPT(DFN,NODE,IEN)) QUIT:(+IEN'>0)  DO
  . NEW PTR SET PTR=+$GET(^DPT(DFN,NODE,IEN,0))
  . QUIT:PTR'>0
  . NEW CODE SET CODE=$PIECE($GET(^DIC(P2FILE,PTR,0)),"^",2)
  . SET RESULT=RESULT_CODE
GDCDN   QUIT RESULT
  ;
ADDDEMCD(DFN,FIELD,CODE,ERRORS) ;
  ;"Input: DFN -- IEN IN 2
  ;"       FIELD -- should be 2, or 6
  ;"       CODE -- value to add
  ;"       ERRORS -- PASS BY REFERENCE.  An OUT PARAMETER.  Filled with errors, IF any
  ;"Results: 1 if OK, or 0 IF error
  NEW RESULT SET RESULT=1
  NEW TMGFILE,TMGIENS,TMGMSG,TMGFDA,P2FILE
  SET FIELD=$GET(FIELD)
  IF FIELD=2 SET TMGFILE=2.02,P2FILE=10
  ELSE  IF FIELD=6 SET TMGFILE=2.06,P2FILE=10.2
  ELSE  GOTO ADCDN
  NEW PTR SET PTR=+$ORDER(^DIC(P2FILE,"C",CODE,0)) ;"RECORD IN POINTED TO FILE
  IF PTR'>0 DO  GOTO ADCDN
  . SET RESULT=0
  . SET ERRORS("ERROR")="Unable to find Code ["_CODE_"] in Fileman FILE #"_P2FILE
  SET TMGIENS="+1,"_DFN_","
  SET TMGFDA(TMGFILE,TMGIENS,.01)="`"_PTR
  SET TMGFDA(TMGFILE,TMGIENS,.02)="O" ;" OBSERVER
  DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO ADCDN
  . SET RESULT=0
  . MERGE ERRORS=TMGMSG("DIERR")
ADCDN   QUIT RESULT
  ;
DELDEMCD(DFN,FIELD,CODE,ERRORS) ;"DEL DEMO CODE
  ;"Input: DFN -- IEN IN 2
  ;"       FIELD -- should be 2, or 6
  ;"       CODE -- value to delete
  ;"       ERRORS -- PASS BY REFERENCE.  An OUT PARAMETER.  Filled with errors, IF any
  ;"Results: 1 if OK, or 0 IF error
  NEW RESULT SET RESULT=1
  NEW NODE,P2FILE,SUBFILE
  SET FIELD=$GET(FIELD)
  IF FIELD=2 SET NODE=.02,P2FILE=10,SUBFILE=2.02
  ELSE  IF FIELD=6 SET NODE=.06,P2FILE=10.2,SUBFILE=2.06
  ELSE  DO  GOTO DDCDN
  . SET RESULT=0
  . SET ERRORS("ERROR")="Invalid FIELD specified for DelDemocode^TMGSEQL1"
  NEW PTR SET PTR=+$ORDER(^DIC(P2FILE,"C",CODE,0)) ;"RECORD IN POINTED TO FILE
  IF PTR'>0 DO  GOTO DDCDN
  . SET RESULT=0
  . SET ERRORS("ERROR")="Unable to find Code ["_CODE_"] in Fileman FILE #"_P2FILE
  NEW IEN SET IEN=+$ORDER(^DPT(DFN,NODE,"B",PTR,0))
  IF IEN'>0 DO  GOTO DDCDN
  . SET RESULT=0
  . SET ERRORS("ERROR")="Code ["_CODE_"] not found in field #"_FIELD_" for patient "_$PIECE($GET(^DPT(DFN,0)),"^",1)
  NEW TMGFILE,TMGMSG,TMGFDA
  SET TMGFDA(SUBFILE,IEN_","_DFN_",",.01)="@"
  DO FILE^DIE("E","TMGFDA","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO ADCDN
  . SET RESULT=0
  . MERGE ERRORS=TMGMSG("DIERR")
DDCDN   ;
  QUIT RESULT
  ;
