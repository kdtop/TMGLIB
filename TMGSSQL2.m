TMGSSQL2 ;TMG/kst/Interface with SequelSystems SQK queriesPMS ;12/19/24
   ;;1.0;TMG-LIB;**1**;12/19/24
 ;"NOTE: THIS ENTIRE FUNCTION WAS COPIED FROM TMGSEQL1
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
 ;"GTDEMOCD(TMGDFN,FIELD) -- Return codes for entries in FILE
 ;"ADDDEMCD(TMGDFN,FIELD,CODE,ERRORS) ;
 ;"DELDEMCD(TMGDFN,FIELD,CODE,ERRORS)
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"TMGIOUTL
 ;"TMGMISC
 ;"=======================================================================
 ;"=======================================================================
 ;"
RUNNOW(LOGFILE)  ;"RUN NOW
  ;"Purpose: To provide an entry point for running import NOW.  This will delete prior alerts
  ;"Input: none.  Settings stored in FILE 22711 are used
  ;"Output: None.  Progress shown to console.  The database should be updated
  ;"Results: none
  WRITE !!,"Import Sequel Demographics Now...",!
  NEW ERRARRAY,CHANGLOG
  NEW USERID SET USERID=150
  NEW FPATH,FNAME SET FPATH=$$SQLDIRECTORY^TMGSSQL1(),FNAME=$$PTDEMOF^TMGSSQL1()_$$DATAFILETYPE^TMGSSQL1()
  ;
  NEW RESULT SET RESULT=$$IMPORTFILE(FPATH,FNAME,USERID,LOGFILE)
  ;"
  IF $$DELFILE^TMGIOUTL(FPATH_FNAME)=0 DO LOGLINE^TMGSSQL1(LOGFILE,"ERROR DELETING CSV FILE",0,0)
  ;"
  QUIT
  ;

IMPORTFILE(FILEPATH,FILENAME,USERID,LOGFILE) ;
  ;"Purpose: To import data from file specified.
  ;"Input:   FILEPATH: PATH of file to input.
  ;"   FILENAME: The Name of file of file to input.
  ;"  Note: This is written to import a specific file
  ;"    created by SequelMed Systems, filled with
  ;"    patient demographics, in CVS format
  ;"   LOGFILE: Log file path+name
  ;"   USERID : OPTIONAL -- user to receive alerts regarding errors.  Default is current user (DUZ)
  ;"Note: I have learned that SequelMed billing system exports ALL patients in the primary
  ;"      export file, including one that have been marked inactive due to invalid data etc.
  ;"      Thus, while the second file (F2NAME) has limited info, it contains the list of
  ;"      ACTIVE patients.  So if a name is not included in the 2nd file, then its info will
  ;"      be ignored in the 1st file.
  ;"Output: Database is updated with data from file.
  ;"Result: 1 successful completion, -1^Message if error
  NEW RESULT,GREF,G2REF,G3REF     ;"I use these references when processing arrays
  NEW PTDATA
  SET TMGRESULT=$$LCSV2ARR^TMGIOUT4(FILEPATH_FILENAME,"PTDATA")
  NEW TMGCUR SET TMGCUR=""
  NEW TMGABORT SET TMGABORT=0
  ;"
  FOR  SET TMGCUR=$ORDER(PTDATA(TMGCUR)) QUIT:(TMGCUR="")!(TMGABORT=1)  DO
  . NEW ONELINE MERGE ONELINE=PTDATA(TMGCUR)  
  . SET RESULT=$$PROCESPT(.ONELINE,LOGFILE,USERID)
  IF RESULT'>0 GOTO IFDONE
  ;
  ;"record the current time as the time of last import
  NEW TMGFDA,TMGMSG      
  SET TMGFDA(22711,"1,",4)=$$NOW^XLFDT  ;"#4 = LAST IMPORT DATE
  DO FILE^DIE("E","TMGFDA","TMGMSG")  ;" note: ignores TMGMSG or errors.
  IF $DATA(TMGMSG("DIERR")) DO  GOTO IFDONE
  . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)  
  ;
IFDONE  ;
  QUIT RESULT
  ;
PROCESPT(ONELINE,LOGFILE,DUZ) ;
  ;"Purpose: To process one line from patient demographics file.
  ;"Input: 
  ;"  ONELINE-- One line from CVS demographics file.
  ;"  LOGFILE - PATH+FILE for the log file
  ;"  DUZ: The user who will recieve alerts of errors
  ;"Output: Data is put into database, IF it is not there already.
  ;"Result: 1=OK To continue; -1^Message if error, or to abort or bad data
  ;"NEW XFn
  SET ^TMP("ERROR")=DUZ
  NEW PTINFO,ONEERRARRAY
  NEW RESULT SET RESULT=1
  NEW AUTOREG SET AUTOREG=1
  SET RESULT=$$PARSLINE(.ONELINE,.PTINFO)
  ;"Result: 1=OK To continue; 2 skip, but don't store as error;  -1^Message -- abort or bad data;
  IF +RESULT=2 SET RESULT=1 GOTO PPTDN  ;"//kt 7/30/15
  IF +RESULT'=1 DO  GOTO PPTL1
  . SET ONEERRARRAY(0)=$PIECE(RESULT,"^",2)
  IF $GET(PTINFO("FACILITY"))="SAMPLE" GOTO PPTDN
  IF $GET(PTINFO("ACTIVE"))="N" GOTO PPTDN
  NEW TEMP SET TEMP=$$UPDATEDB(.PTINFO,AUTOREG,LOGFILE)
  ;"IF TEMP=0 GOTO PPTDN
  IF ($D(PTINFO("INSURANCE")))&('$D(PTINFO("INACTIVE"))) DO CHKINSUR(.PTINFO,LOGFILE)
PPTL1 ;
  ;"NEW COUNT SET COUNT=+$GET(ERRARRAY)+1
  ;"SET ERRARRAY=COUNT
  ;"SET ERRARRAY(COUNT)=ONELINE
  ;"MERGE ERRARRAY(COUNT,"INFO")=ONEERRARRAY
  ;"DO ALERTERR^TMGSEQL2(ONELINE,.PTINFO,.ONEERRARRAY,DUZ)
  ;
PPTDN  ;
  QUIT RESULT
  ;
PARSLINE(ONELINE,ARRAY) ;
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
  ;"   11- pat_address
  ;"   12- location_name,
  ;"   13- city,
  ;"   14- provider_short_name,
  ;"   15- zipcode,
  ;"   16- class_name,
  ;"   17- pat_dob,
  ;"   18- home_num
  ;"   19- pat_cell_num,
  ;"   20- active,
  ;"   21+ - insurances (format: name|priority|ID|group)
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
  ;"SET ONELINE=$TRANSLATE($GET(ONELINE),"""","'") ;"  convert " to ' to avoid fileman error
  KILL ARRAY
  ;"SET ONELINE=$TRANSLATE(ONELINE,$C(13,10),"") ;"Strip any CRLF
  SET ARRAY("FACILITY")=$GET(ONELINE(2))
  SET ARRAY("LAST NAME")=$$TRIM^XLFSTR($GET(ONELINE(3)))
  SET ARRAY("FIRST NAME")=$$TRIM^XLFSTR($GET(ONELINE(4)))
  SET ARRAY("PMS ACCOUNT NUM")=$GET(ONELINE(5))
  SET ARRAY("ADDRESS1")=$GET(ONELINE(6))
  SET ARRAY("ADDRESS2")=$$TRIM^XLFSTR($GET(ONELINE(11)))
  SET ARRAY("STATE")=$GET(ONELINE(7))
  SET ARRAY("RESP LAST NAME")=$GET(ONELINE(8))
  SET ARRAY("RESP FIRST NAME")=$GET(ONELINE(9))
  SET ARRAY("CITY")=$$TRIM^XLFSTR($GET(ONELINE(13)),"""")
  SET ARRAY("PROVIDER")=$GET(ONELINE(14))
  SET ARRAY("ZIP CODE")=$$TRIM^XLFSTR($GET(ONELINE(15)))
  SET ARRAY("SSNUM")=$GET(ONELINE(20))
  IF $G(ARRAY("SSNUM"))["00000" KILL ARRAY("SSNUM")
  ;"SET ARRAY("ACTIVE")=$EXTRACT($PIECE(ONELINE,",",23),1)
  NEW DOB SET DOB=$GET(ONELINE(17))
  SET ARRAY("DOB")=DOB
  SET ARRAY("PHONE NUM")=$GET(ONELINE(18))
  SET ARRAY("CELL NUM")=$GET(ONELINE(19))
  ;"Note: if SEX not found in line below, then code later below will fix
  NEW SEX SET SEX=$$TRIM^XLFSTR($GET(ONELINE(21)))
  IF SEX="F" SET SEX="FEMALE"
  IF SEX="M" SET SEX="MALE"
  SET ARRAY("SEX")=SEX
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
  ;"
  ;"Ensure proper length of city.
  SET ARRAY("CITY")=$EXTRACT(ARRAY("CITY"),1,15)
  IF $LENGTH(ARRAY("CITY"))=1 SET ARRAY("CITY")=ARRAY("CITY")_" "
  ;
  ;"Ensure proper length of phone
  IF $LENGTH(ARRAY("PHONE NUM"))<7 KILL ARRAY("PHONE NUM")
  ;
  NEW ACCTNUM SET ACCTNUM=$GET(ARRAY("PMS ACCOUNT NUM"))
  ;
  ;"GET INSURANCE INFO
  NEW IDX SET IDX=22
  NEW DONE SET DONE=0
  FOR  SET IDX=$O(ONELINE(IDX)) QUIT:DONE=1  DO
  . NEW INSLINE SET INSLINE=$G(ONELINE(IDX))
  . IF INSLINE'["|" DO  QUIT
  . . SET DONE=1
  . NEW INSNAME,PRIORITY,ID,GROUP
  . SET INSNAME=$P(INSLINE,"|",1)
  . SET PRIORITY=$P(INSLINE,"|",2)
  . SET ID=$P(INSLINE,"|",3)
  . SET GROUP=$P(INSLINE,"|",4)
  . SET ARRAY("INSURANCE",PRIORITY,"NAME")=$$TRIM^XLFSTR(INSNAME)
  . SET ARRAY("INSURANCE",PRIORITY,"ID")=ID
  . SET ARRAY("INSURANCE",PRIORITY,"GROUP")=GROUP
  ;"
  ;"Added 12/16/14
  IF $GET(ARRAY("SEX"))="" KILL ARRAY("SEX")  ;"ADDED 6/11/15
  IF +$GET(ARRAY("ZIP CODE"))=0 SET ARRAY("ZIP CODE")="" 
  ;
  ;IF +RESULT'=-1 DO
  ;. IF $$INVALPTN(ARRAY("FIRST NAME"),ARRAY("LAST NAME"))=1 SET RESULT=2 QUIT
  ;. IF $$INACTVPT(ARRAY("PMS ACCOUNT NUM"),.EXTARR2)=1 SET RESULT=2 QUIT
  ;
PLNDN   ;
  QUIT RESULT
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
  . . NEW TMGDFN SET TMGDFN=+Y
  . . NEW TMGFDA SET TMGFDA(200,TMGDFN_",",22702)=SEQLPROV
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
UPDATEDB(PTINFO,AUTOREG,LOGFILE) ;
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
  NEW LOGLINE SET LOGLINE=$GET(PTINFO("FULL NAME2"))_" - "
  NEW RESULT SET RESULT=1
  NEW ENTRY,NAME,TMGDOB,TMGDFN,TMGARRAY,TMGMSG,NEWINFO,IENS,INDEX,FIELDS
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
  SET TMGDFN=$$GETDFN(.PTINFO)
  IF (TMGDFN<1)&($$GOODDATA(.PTINFO)=0) DO  GOTO UDBDN  ;"Does patient have info needed to register as new patient?
  . ;"NOTE: When patient can not be auto-registered, don't create error or alert, just quit  //kt 7/31/15
  . SET LOGLINE=LOGLINE_"DFN CANNOT BE FOUND AND NOT ENOUGH INFO TO AUTOREGISTER."
  . SET AUTOREG=0
  IF (TMGDFN<1)&($GET(AUTOREG)=1) DO
  . IF $GET(ENTRY(.02))="" DO  ;"autopick gender IF missing
  . . NEW AUTOPICK
  . . SET AUTOPICK=$$GET1^DIQ(22711,"1,","PICK GENDER FROM NAME?","I")
  . . IF AUTOPICK'=1 QUIT
  . . SET ENTRY(.02)=$$GETSEX^TMGSEQL2($GET(PTINFO("FIRST NAME")))
  . ;"OK, can't find, so will add NEW patient.
  . SET TMGDFN=$$ADDNEWPAT^TMGGDFN(.ENTRY)
  . IF +TMGDFN>0 SET CHANGLOG(NAME_" "_TMGDOB,0)="ADDED PATIENT: "_NAME_" "_TMGDOB
  . SET LOGLINE=LOGLINE_"ADDED AS NEW PATIENT."
  IF +TMGDFN<1 DO  GOTO UDBDN  ;"failure
  . SET RESULT=0
  . NEW ERRMSG SET ERRMSG=$PIECE(TMGDFN,"^",2)
  . SET LOGLINE=LOGLINE_ERRMSG
  . IF ERRMSG="" SET ERRMSG=$$NAMEERR^TMGSEQL2(.ONEERRARRAY)  ;"get name IF DIERR encountered. 
  . IF ERRMSG="" SET ERRMSG="PATIENT NOT IN DATABASE:"  ;"if changed, also change in TMGSEQL2.m
  . SET ONEERRARRAY(0)=$PIECE(TMGDFN,"^",2)
  SET IENS=TMGDFN_","
  SET PTINFO("DFN")=TMGDFN
  IF $$ACTIVEPT^TMGPXR03(TMGDFN)'=1 DO  GOTO UDBDN
  . SET PTINFO("INACTIVE")=1
  . SET LOGLINE=LOGLINE_"NOT AN ACTIVE PATIENT. SKIPPING."
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
  . . . SET LOGLINE=LOGLINE_"ABORTING WITH MSG: "_MSG
  . . ELSE  SET NEWINFO(INDEX)=VALUE
  . . SET UPDATENEEDED=1
  ;
  IF ABORT=1 GOTO UDBDN
  IF UPDATENEEDED=0 DO  GOTO UDDB2
  . SET LOGLINE=LOGLINE_"NO CHANGES FOUND. NO UPDATE NEEDED."
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
  . . SET LOGLINE=LOGLINE_"ERROR: "_$G(TMGMSG("DIERR"))
  . ELSE  DO
  . . SET LOGLINE=LOGLINE_"UPDATED RECORD"
  MERGE CHANGLOG($GET(NAME,"?")_" "_$GET(TMGDOB,"?"),1)=NEWINFO
  ;
UDDB2   ;"Synchronize Race & Ethnicity information stored.
  GOTO UDBDN   ;"  <-- SKIP THESE FIELDS  1/2/25
  NEW DEMOFIELD
  FOR DEMOFIELD=2,6 QUIT:(RESULT=0)  DO
  . NEW DBCODES SET DBCODES=$$GTDEMOCD(TMGDFN,DEMOFIELD)
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
  . . SET RESULT=$$DELDEMCD(TMGDFN,DEMOFIELD,INDEX,.ONEERRARRAY)
  . FOR  SET INDEX=$ORDER(CODES2ADD(INDEX)) QUIT:(INDEX="")!(RESULT=0)  DO
  . . SET RESULT=$$ADDDEMCD(TMGDFN,DEMOFIELD,INDEX,.ONEERRARRAY)
UDBDN   ;
  DO LOGLINE^TMGSSQL1(LOGFILE,LOGLINE,0,0) 
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
  NEW ENTRY,NAME,DOB,TMGDFN
  SET NAME=$GET(PTINFO("LAST NAME"))_","_$GET(PTINFO("FIRST NAME"))
  SET NAME=$$FRMTNAME^TMGMISC(NAME)
  SET DOB=$GET(PTINFO("DOB"))
  SET ENTRY(.01)=NAME
  SET ENTRY(.03)=DOB
  SET ENTRY(.02)=$GET(PTINFO("SEX"))
  SET ENTRY(.09)=$GET(PTINFO("SSNUM"))
  SET TMGDFN=+$$LOOKUPPAT^TMGGDFN(.ENTRY)  ;"get IEN in file 2 of patient
  ;"do an extended search with increasing intensity.
  IF +TMGDFN=0 SET TMGDFN=$$EXTRLKUP^TMGGDFN(.ENTRY,1)
  IF +TMGDFN=0 SET TMGDFN=$$EXTRLKUP^TMGGDFN(.ENTRY,2)
  IF +TMGDFN=0 SET TMGDFN=$$EXTRLKUP^TMGGDFN(.ENTRY,3)
  QUIT TMGDFN
  ;
GTDEMOCD(TMGDFN,FIELD)  ;"GET DEMO CODES
  ;"Return codes for entries in FILE
  ;"Input: TMGDFN -- IEN IN 2
  ;"       FIELD -- should be 2, or 6
  ;"Results: returns CODES for entries in multiples, stored in FILE 2
  NEW RESULT SET RESULT=""
  NEW NODE,P2FILE
  SET FIELD=$GET(FIELD)
  IF FIELD=2 SET NODE=.02,P2FILE=10
  ELSE  IF FIELD=6 SET NODE=.06,P2FILE=10.2
  ELSE  GOTO GDCDN
  NEW IEN SET IEN=0
  FOR  SET IEN=$ORDER(^DPT(TMGDFN,NODE,IEN)) QUIT:(+IEN'>0)  DO
  . NEW PTR SET PTR=+$GET(^DPT(TMGDFN,NODE,IEN,0))
  . QUIT:PTR'>0
  . NEW CODE SET CODE=$PIECE($GET(^DIC(P2FILE,PTR,0)),"^",2)
  . SET RESULT=RESULT_CODE
GDCDN   QUIT RESULT
  ;
ADDDEMCD(TMGDFN,FIELD,CODE,ERRORS) ;
  ;"Input: TMGDFN -- IEN IN 2
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
  SET TMGIENS="+1,"_TMGDFN_","
  SET TMGFDA(TMGFILE,TMGIENS,.01)="`"_PTR
  SET TMGFDA(TMGFILE,TMGIENS,.02)="O" ;" OBSERVER
  DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO ADCDN
  . SET RESULT=0
  . MERGE ERRORS=TMGMSG("DIERR")
ADCDN   QUIT RESULT
  ;
DELDEMCD(TMGDFN,FIELD,CODE,ERRORS) ;"DEL DEMO CODE
  ;"Input: TMGDFN -- IEN IN 2
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
  NEW IEN SET IEN=+$ORDER(^DPT(TMGDFN,NODE,"B",PTR,0))
  IF IEN'>0 DO  GOTO DDCDN
  . SET RESULT=0
  . SET ERRORS("ERROR")="Code ["_CODE_"] not found in field #"_FIELD_" for patient "_$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
  NEW TMGFILE,TMGMSG,TMGFDA
  SET TMGFDA(SUBFILE,IEN_","_TMGDFN_",",.01)="@"
  DO FILE^DIE("E","TMGFDA","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO ADCDN
  . SET RESULT=0
  . MERGE ERRORS=TMGMSG("DIERR")
DDCDN   ;
  QUIT RESULT
  ;
;"=============================================================================================================
;"===================================   INSURANCE ROUTINES BELOW   ============================================
;"=============================================================================================================
ENSURINS(TMGDFN,INSARR,LOGFILE)  ;"Ensure insurance name is a valid record, and link to patient.
  ;"Input: TMGDFN -- Patient's DFN
  ;"       INSARR -- Array of data
  ;"           Example INSARR(COB,"NAME")=INSNAME
  ;"                   INSARR(COB,"ID")=ID
  ;"                   INSARR(COB,"GROUP")=GROUP
  ;"Result: 1 if OK, or -1^Message if problem.
  SET TMGDFN=+$G(TMGDFN)
  IF TMGDFN'>0 QUIT
  NEW COB SET COB=0
  FOR  SET COB=$O(INSARR(COB)) QUIT:COB'>0  DO
  . NEW SHORTNAME SET SHORTNAME=$GET(INSARR(COB,"NAME"))
  . NEW PLANID SET PLANID=$GET(INSARR(COB,"ID"))
  . NEW GROUP SET GROUP=$GET(INSARR(COB,"GROUP"))
  . NEW TMGRESULT SET TMGRESULT=1
  . NEW IEN36 SET IEN36=+$ORDER(^DIC(36,"C",SHORTNAME,0))
  . IF IEN36<1 DO
  . . ;"Make new record for insurance company
  . . NEW LONGNAME SET LONGNAME=SHORTNAME_" (EDIT THIS NAME)"
  . . NEW TMGFDA,TMGIEN,TMGMSG
  . . SET TMGFDA(36,"+1,",.01)=LONGNAME
  . . SET TMGFDA(36,"+1,",.111)="(edit this address)" ;"required field
  . . SET TMGFDA(36,"+1,",1)="N"  ;"required field N=Will not reimburse
  . . SET TMGFDA(36,"+1,",2)="N"  ;"required field N=signature not required
  . . DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG") 
  . . IF $DATA(TMGMSG("DIERR")) DO  QUIT
  . . . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  . . SET IEN36=$GET(TMGIEN(1))
  . . ;"now add short name as a synonym
  . . KILL TMGFDA,TMGIEN,TMGMSG
  . . SET TMGFDA(36.03,"+1,"_IEN36_",",.01)=SHORTNAME
  . . DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG") 
  . . IF $DATA(TMGMSG("DIERR")) DO  QUIT
  . . . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)  
  . ;"
  . ;"LINK TO PATIENT
  . ;"ADD INSURANCE SUBRECORD
  . KILL TMGFDA,TMGIEN,TMGMSG
  . SET TMGFDA(2.312,"+1,"_TMGDFN_",",.01)=IEN36
  . SET TMGFDA(2.312,"+1,"_TMGDFN_",",5.01)=PLANID
  . SET TMGFDA(2.312,"+1,"_TMGDFN_",",.2)=COB
  . SET TMGFDA(2.312,"+1,"_TMGDFN_",",2)=GROUP
  . SET TMGFDA(2.312,"+1,"_TMGDFN_",",1.03)=$$NOW^XLFDT
  . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG") 
  . IF $DATA(TMGMSG("DIERR")) DO
  . . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  . . DO LOGLINE^TMGSSQL1(LOGFILE,"**ERROR ADDING "_SHORTNAME_" INS. "_$P(TMGRESULT,"^",2),0,0)
  . ELSE  DO
  . . DO LOGLINE^TMGSSQL1(LOGFILE,"    "_SHORTNAME_" INS ADDED. "_$P(TMGRESULT,"^",2),0,0)
  QUIT TMGRESULT
  ;"
CHKINSUR(PTINFO,LOGFILE)   ;"CHECK INSURANCES AND ADD IF UPDATED
  NEW TMGDFN SET TMGDFN=+$G(PTINFO("DFN"))
  IF TMGDFN'>0 QUIT
  ;"FOR THE SAKE OF SPEED WE ARE GOING TO DO WHAT THE PREVIOUS IMPORT AND JUST DELETE ALL INSURANCES AND SAVE THE SENT ONE,
  ;"     RATHER THAN TRY TO WASTE TIME COMPARING ALL THE LITTLE PIECES
  DO DELINS(TMGDFN)
  NEW SEQINS
  MERGE SEQINS=PTINFO("INSURANCE")
  DO ENSURINS(TMGDFN,.SEQINS,LOGFILE)
  QUIT
  ;"
GETINS(TMGDFN,CURINS)  ;"Get all of the patient's current insurances
  NEW INSIDX SET INSIDX=0
  FOR  SET INSIDX=$O(^DPT(TMGDFN,.312,INSIDX)) QUIT:INSIDX'>0  DO
  . NEW ZN SET ZN=$G(^DPT(TMGDFN,.312,INSIDX,0))
  . NEW TYPE,COB,GROUP,ID
  . SET TYPE=$P(ZN,"^",1)
  . SET COB=$P(ZN,"^",20)
  . SET GROUP=$P(ZN,"^",3)
  . SET ID=$P($G(^DPT(TMGDFN,.312,INSIDX,5)),"^",20)
  . SET CURINS(COB,"NAME")=TYPE
  . SET CURINS(COB,"ID")=ID
  . SET CURINS(COB,"GROUP")=GROUP
  . SET CURINS(COB,"IDX")=IDX
  QUIT
  ;"
DELINS(TMGDFN)  ;" Delete all insurances
  NEW INSIDX SET INSIDX=0
  FOR  SET INSIDX=$ORDER(^DPT(TMGDFN,.312,INSIDX)) QUIT:INSIDX'>0  DO
  . SET TMGFDA(2.312,INSIDX_","_TMGDFN_",",.01)="@"
  . DO FILE^DIE("E","TMGFDA","TMGMSG")
  QUIT    