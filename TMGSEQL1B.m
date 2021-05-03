TMGSEQL1B ;TMG/kst/Interface with a generic PMS ;03/25/06, 2/2/14, 3/24/21
          ;;1.0;TMG-LIB;**1**;01/09/06

 ;"TMG DEMOGRAPHICS IMPORT FUNCTIONS
 ;"Custom import routines for Clinica Adelante
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
 ;"RUNNOW  provide an entry point for running import NOW.  This will delete prior alerts
 ;"AUTOIN  ;"entry point for scheduled task
 ;"QUIETIN

 ;"$$IMPORTFILE(FilePath,FileName,F2Name,ErrArray,ChgLog,PrgCallback,F2Path,DELFILES,UserID)
 ;"$$IMPORTGLOBAL(GREF,G2Ref,ErrArray,ChgLog,PrgCallback,UserID)

 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"$$ProcessPt(OneLine,ErrArray,ChgLog,SSNArray,DUZ)
 ;"$$ParseLine(OneLine,Array,SSNArray)
 ;"UpdateDB(PtInfo,AutoRegister,ErrArray,ChgLog)
 ;"$$InactivePt(PMSAcctNum,SSNArray)
 ;"$$GETDFN(PtInfo)


 ;"=======================================================================
 ;"DEPENDENCIES
 ;"TMGIOUTL
 ;"TMGMISC
 ;"=======================================================================
 ;"=======================================================================



 ;"=======================================================================
 ;"      Below are three custom files that are used by the TMGSEQL* code
 ;"=======================================================================


 ;"File: 22706 TMG DEMOGRAPHICS IMPORT ERRORS                           Branch: 1
 ;"REF  NODE;PIECE     FLD NUM  FIELD NAME
 ;"===============================================================================
 ;"  1  0;1                .01  ACCOUNT NUMBER                           [RNJ9,0]
 ;"  2  4;1                .02  CREATION DATE                                 [D]
 ;"  3  4;2                .03  PATIENT NAME                                  [F]
 ;"  4  0;2                  1  MESSAGE                                       [F]
 ;"     2;0                  2  IMPORT DATA                       <-WP [22706.02]
 ;"  5   -0;1              .01   -IMPORT DATA                                 [W]
 ;"     3;0                  3  DIERR MESSAGE                     <-WP [22706.03]
 ;"  6   -0;1              .01   -DIERR MESSAGE                               [W]
 ;"  7  4;3                  4  ALERT IEN                                 [NJ9,0]
 ;" <> <> <>
 ;"  A.) FILE NAME:------------- TMG DEMOGRAPHICS IMPORT ERRORS
 ;"                                                F.) FILE ACCESS:
 ;"  B.) FILE NUMBER:----------- 22706                  DD______ @
 ;"                                                     Read____ @
 ;"  C.) NUM OF FLDS:----------- 9                      Write___ @
 ;"                                                     Delete__ @
 ;"  D.) DATA GLOBAL:----------- ^TMG(22706,            Laygo___ @
 ;"
 ;"  E.) TOTAL GLOBAL ENTRIES:-- 76                G.) PRINTING STATUS:-- Off
 ;"================================================================================



 ;"File: 22707 TMG NAME SEX                                              Branch: 1
 ;"REF  NODE;PIECE     FLD NUM  FIELD NAME
 ;"===============================================================================
 ;"  1  0;1                .01  FIRST NAME                                   [RF]
 ;"  2  0;2                  1  SEX                                           [S]
 ;"<> <> <>
 ;"  A.) FILE NAME:------------- TMG NAME SEX
 ;"                                                F.) FILE ACCESS:
 ;"  B.) FILE NUMBER:----------- 22707                  DD______ @
 ;"                                                     Read____ @
 ;"  C.) NUM OF FLDS:----------- 2                      Write___ @
 ;"                                                     Delete__ @
 ;"  D.) DATA GLOBAL:----------- ^TMG(22707,            Laygo___ @
 ;"
 ;"  E.) TOTAL GLOBAL ENTRIES:-- 698               G.) PRINTING STATUS:-- Off
 ;"================================================================================



 ;"File: 22711 TMG UPLOAD SETTINGS                                       Branch: 1
 ;"REF  NODE;PIECE     FLD NUM  FIELD NAME
 ;"===============================================================================
 ;"  1  0;1                .01  NAME                                        [RFX]
 ;"  2  0;2                  1  DEBUG SHOW                               [NJ1,0X]
 ;"  3  1;1                1.1  DEBUG OUTPUT FILE                             [F]
 ;"  4  2;1               1.15  DEBUG OUTPUT PATH                             [F]
 ;"  5  1;2                1.2  DEBUG CUMULATIVE                          [NJ1,0]
 ;"  6  3;1                  2  IMPORT DATAFILE NAME                          [F]
 ;"  7  5;1                2.1  IMPORT DATAFILE 2 NAME                        [F]
 ;"  8  4;1                2.5  IMPORT DATAFILE PATH                          [F]
 ;"  9  6;1                  3  ALERT RECIPIENT                   <-Pntr  [P200']
 ;" 10  6;2                  4  LAST IMPORT DATE                              [D]
 ;" 11  6;3                  5  DELETE DATAFILE AFTER IMPORT?                 [S]
 ;" 12  6;4                  6  PICK GENDER FROM NAME?                        [S]
 ;" 13  6;5                  7  IMPORT FREQUENCY (IN HOURS)               [NJ4,0]
 ;" <> <> <>
 ;"  A.) FILE NAME:------------- TMG UPLOAD SETTINGS
 ;"                                                F.) FILE ACCESS:
 ;"  B.) FILE NUMBER:----------- 22711                  DD______ @
 ;"                                                     Read____ @
 ;"  C.) NUM OF FLDS:----------- 12                     Write___ @
 ;"                                                     Delete__ @
 ;"  D.) DATA GLOBAL:----------- ^TMG(22711,            Laygo___ @
 ;"
 ;"  E.) TOTAL GLOBAL ENTRIES:-- 1                 G.) PRINTING STATUS:-- Off
 ;"================================================================================




ASKIMPORT
        ;"Purpose: To ask user for filename and then import data.
        ;"Input: None
        ;"Output: Database is updated with data from file.
        ;"Result: None

        NEW DiscardName
        NEW DefPath SET DefPath="/tmp/"
        NEW DefFNAME SET DefFNAME="demographics.csv"
        ;"new DefF2Name SET DefF2Name="demographics2.csv"
        NEW FPath,FNAME,F2Name
        NEW ErrArray,ChLog
        NEW result

        NEW PrgsFn SET PrgsFn="do PROGBAR^TMGUSRI2(TMGCUR,""Progress"",1,TMGMAX,,TMGSTART)"
        SET PrgsFn=PrgsFn_" read *TMGKEYIN:0 SET:(TMGKEYIN=27) TMGABORT=1"

        SET DiscardName=$$GETFNAME^TMGIOUTL("Please enter file to import.",.DefPath,.DefFNAME,,.FPath,.FNAME)
        IF DiscardName="" GOTO AIDone

        ;"set DiscardName=$$GETFNAME^TMGIOUTL("Please enter 2nd file to import.",.DefPath,.DefF2Name,,.FPath,.F2Name)
        ;"if DiscardName="" GOTO AIDone

        SET result=$$IMPORTFILE(FPath,FNAME,.F2Name,.ErrArray,.ChLog,PrgsFn)

AIDone
    QUIT


RUNNOW
        ;"Purpose: To provide an entry point for running import NOW.  This will delete prior alerts
        ;"Input: none.  Settings stored in File 22711 are used
        ;"Output: None.  Progress shown to console.  The database should be updated
        ;"Results: none

        WRITE !!,"Import PMS Demographics Now...",!

        NEW FNAME,F2Name,FPath
        NEW result
        NEW ErrArray,ChLog
        NEW DELFILES
        NEW UserID

        SET FNAME=$$GET1^DIQ(22711,"1,","IMPORT DATAFILE NAME")
        ;"set F2Name=$$GET1^DIQ(22711,"1,","IMPORT DATAFILE 2 NAME")
        SET FPath=$$GET1^DIQ(22711,"1,","IMPORT DATAFILE PATH")
        SET DELFILES=+$$GET1^DIQ(22711,"1,","DELETE DATAFILE AFTER IMPORT?","I")
        SET UserID=$$GET1^DIQ(22711,"1,","ALERT RECIPIENT","I")

        NEW PrgsFn SET PrgsFn="do PROGBAR^TMGUSRI2(TMGCUR,""Progress"",1,TMGMAX,,TMGSTART)"
        SET PrgsFn=PrgsFn_" read *TMGKEYIN:0 SET:(TMGKEYIN=27) TMGABORT=1"

        SET result=$$IMPORTFILE(FPath,FNAME,.F2Name,,,PrgsFn,,DELFILES,UserID)

        QUIT


AUTOIN
        ;"Purpose: To provide an entry point for a scheduled task.  This will delete prior alerts
        ;"Input: none.  Settings stored in File 22711 are used
        ;"Output: None.  There should be no console output.  The database should be updated
        ;"Results: none

        NEW InitTime SET InitTime=$H

        NEW UserID SET UserID=$$GET1^DIQ(22711,"1,","ALERT RECIPIENT","I")

        DO  ;"clear out 'next run task number'
        . NEW TMGFDA,TMGMSG
        . SET TMGFDA(22711,"1,",8)="@"  ;"#4 = TASK FOR NEXT RUN
        . DO FILE^DIE("E","TMGFDA","TMGMSG")  ;" note: ignores TMGMSG or errors.

        NEW temp SET temp=$$QuietClear^TMGSEQL3(UserID)  ;"clear prior alerts & errors
        DO QUIETIN  ;" DO import

        ;"Here I schedule the next task to run again.
        NEW HrInterval SET HrInterval=$$GET1^DIQ(22711,"1,","IMPORT FREQUENCY (IN HOURS)","I")
        IF +HrInterval>0 do
        . NEW time SET time=$$HADD^XLFDT(InitTime,0,HrInterval,0)
        . NEW task SET task=$$Schedule^TMGSEQL3(time,"AUTOIN^TMGSEQL1","Import of demographic data from PMS billing system.")
        . ;"store 'next run task number'
        . SET TMGFDA(22711,"1,",8)="`"_task  ;"#4 = TASK FOR NEXT RUN
        . DO FILE^DIE("E","TMGFDA","TMGMSG")  ;" note: ignores TMGMSG or errors.

        QUIT


QUIETIN
        ;"Purpose: To import data based on settings, with no user interaction (in or out)
        ;"Input: none.  Settings stored in File 22711 are used
        ;"Output: None.  There should be no console output.  The database should be updated
        ;"Results: none

        NEW FNAME,F2Name,FPath
        NEW result
        NEW ErrArray,ChLog
        NEW DELFILES
        NEW UserID

        SET FNAME=$$GET1^DIQ(22711,"1,","IMPORT DATAFILE NAME")
        ;"set F2Name=$$GET1^DIQ(22711,"1,","IMPORT DATAFILE 2 NAME")
        SET FPath=$$GET1^DIQ(22711,"1,","IMPORT DATAFILE PATH")
        SET DELFILES=+$$GET1^DIQ(22711,"1,","DELETE DATAFILE AFTER IMPORT?","I")
        SET UserID=$$GET1^DIQ(22711,"1,","ALERT RECIPIENT","I")

        SET result=$$IMPORTFILE(FPath,FNAME,.F2Name,,,,,DELFILES,UserID)

        QUIT


IMPORTFILE(FilePath,FileName,F2Name,ErrArray,ChgLog,PrgCallback,F2Path,DELFILES,UserID)
        ;"Purpose: To import data from file specified.
        ;"Input:   FilePath: Path of file to input.
        ;"         FileName: The Name of file of file to input.
        ;"              Note: This is written to import a specific file
        ;"                      created by SequelMed Systems, filled with
        ;"                      patient demographics, in CVS format
        ;"              Note: This file will be DELETED IF DELFILES=1
        ;"         F2Name : the name of the second demographics file in input
        ;"              The reason for 2 files is because Sequel doesn't report the SSN in the
        ;"              primary demographics report.  So a second report must be used, and these
        ;"              two files are MERGEd to provide complete patient demographics.
        ;"              Note: This file will be DELETED IF DELFILES=1
        ;"           *** F2Name Won't be used in this alteration of the code...
        ;"         ErrArray: PASS BY REFERENCE.  Array to receive failed data lines.
        ;"         ChgLog: PASS BY REFERENCE.  An array to receive record of changes made to database
        ;"         PrgCallback: OPTIONAL -- IF supplied, then M code contained in this string
        ;"              will be xecuted periodically, to allow display of a progress bar etc.
        ;"              Note: the following variables with global scope will be declared and
        ;"                      available for use: TMGCUR (current count), TMGMAX (max count),
        ;"                      TMGSTART (the start time
        ;"                      External function can signal a request an abort by setting TMGABORT=1
        ;"         F2Path: OPTIONAL -- path of 2nd demographics file.  Default=FilePath
        ;"           *** F2Path Won't be used in this alteration of the code...
        ;"         DELFILES: OPTIONAL -- IF 1, then source files (FileName and F2Name) are deleted after import
        ;"         UserID : OPTIONAL -- user to receive alerts regarding errors.  Default is current user (DUZ)
        ;"Note: I have learned that SequelMed billing system exports ALL patients in the primary
        ;"      export file, including one that have been marked inactive DO to invalid data etc.
        ;"      Thus, while the second file (F2Name) has limited info, it contains the list of
        ;"      ACTIVE patients.  So IF a name is not included in the 2nd file, then its info will
        ;"      be ignored in the 1st file.
        ;"Output: Database is updated with data from file.
        ;"Result: 1 successful completion, 0=error

        NEW GREF,GREF1
        NEW G2Ref,G2Ref1
        NEW result

        SET F2Path=$GET(F2Path,FilePath)

        SET GREF=$name(^TMP("TMG","PMSIMPORT","DATA",1,$J))   ;"I use this to process array
        SET GREF1=$name(@GREF@(1))                   ;"I use this to load file
        KILL @GREF
        SET result=$$FTG^%ZISH(FilePath,FileName,GREF1,6)  ;"load file into a global
        IF result=0 GOTO IFDONE

        SET G2Ref=$name(^TMP("TMG","PMSIMPORT","DATA",2,$J))   ;"I use this to process array
        SET G2Ref1=$name(@G2Ref@(1))                   ;"I use this to load file
        KILL @G2Ref
        ;"set result=$$FTG^%ZISH(F2Path,F2Name,G2Ref1,6)  ;"load file into a global
        IF result=0 GOTO IFDONE

        SET UserID=$GET(UserID,+$GET(DUZ))

        SET result=$$IMPORTGLOBAL(GREF,G2Ref,.ErrArray,.ChLog,.PrgCallback,UserID)

        ;"Note: @GREF, @G2Ref killed at end of $$IMPORTGLOBAL()

        DO  ;"record the current time as the time of last import
        . DO NOW^%DTC
        . NEW TMGFDA,TMGMSG
        . SET TMGFDA(22711,"1,",4)=%  ;"#4 = LAST IMPORT DATE
        . DO FILE^DIE("E","TMGFDA","TMGMSG")  ;" note: ignores TMGMSG or errors.

        IF $GET(DELFILES)=1 do
        . ;"Notice: After I implemented this, I realized that I have a permissions problem
        . ;"  at my site... the uploaded files belong to the uploaded user, and deletion by
        . ;"  this user is being blocked.  I'll leave in for now...
        . NEW temp
        . SET temp=$$DELFILE^TMGIOUTL(FilePath_FileName)
        . SET temp=$$DELFILE^TMGIOUTL(F2Path_F2Name)

IFDONE
        QUIT result

IMPORTGLOBAL(GREF,G2Ref,ErrArray,ChLog,PrgCallback,UserID)
        ;"Purpose: To import data from global specified.
        ;"Input:   GREF -- the NAME of array holding the data to import (1st file)
        ;"              Format: @GREF@(1)=OneLine
        ;"                      @GREF@(2)=OneLine .. etc.
        ;"              Note: This is written to import a specific file
        ;"                      created by SequelMed Systems, filled with
        ;"                      patient demographics, in CVS format
        ;"              Note: Array will be KILLED at the end of this function.
        ;"         G2Ref -- the NAME of array holding the data to import (2nd file)
        ;"              Note: Array will be KILLED at the end of this function.
        ;"              *** Note: G2Ref won't be used in this alteration of the code.
        ;"         ErrArray: PASS BY REFERENCE.  Array to receive failed data lines.
        ;"         ChgLog: PASS BY REFERENCE.  An array to receive record of changes made to database
        ;"         PrgCallback: OPTIONAL -- IF supplied, then M code contained in this string
        ;"              will be xecuted periodically, to allow display of a progress bar etc.
        ;"              Note: the following variables with global scope will be declared and
        ;"                      available for use: TMGCUR (current count), TMGMAX (max count),
        ;"                      TMGSTART (the start time
        ;"                      External function can signal a request an abort by setting TMGABORT=1
        ;"         UserID : OPTIONAL -- user to receive alerts regarding errors.  Default is current user (DUZ)
        ;"Output: Database is updated with data from file.
        ;"Result: 1 successful completion, 0=error

        NEW TMGINVALID ;"Will be used as a globally-scoped variable in the module
        NEW result SET result=1
        NEW delay SET delay=0
        NEW TMGCUR,TMGMAX,TMGSTART,TMGABORT ;"avail for PrgCallback function
        SET TMGABORT=0
        SET TMGMAX=+$ORDER(@GREF@(""),-1)
        SET TMGSTART=$H  ;"store starting time.
        SET UserID=$GET(UserID,+$GET(DUZ))

        NEW SSNArray
        ;"do XtractSSNum(G2Ref,.SSNArray)

        SET TMGCUR=$ORDER(@GREF@(""))
        IF TMGCUR'="" FOR  DO  QUIT:(TMGCUR="")!(TMGABORT=1)
        . NEW OneLine
        . SET OneLine=$GET(@GREF@(TMGCUR))
        . SET result=$$ProcessPt(OneLine,.ErrArray,.ChgLog,.SSNArray,UserID)
        . SET delay=delay+1
        . IF (delay>30),$GET(PrgCallback)'="" DO  ;"update progress bar every 30 cycles
        . . NEW $ETRAP SET $ETRAP="write ""(Invalid M Code!.  Error Trapped.)"" SET $ETRAP="""",$ecode="""""
        . . xecute PrgCallback  ;"call the specified progress code.
        . . SET delay=0
        . SET TMGCUR=$ORDER(@GREF@(TMGCUR))

        KILL @GREF
        KILL @G2Ref
        QUIT result



ProcessPt(OneLine,ErrArray,ChgLog,SSNArray,DUZ,InputFn)
        ;"Purpose: To process one line from patient demographics file.
        ;"Input: OneLine-- One line from CVS demographics file.
        ;"      Format is as follows, *** all on one line (comma delimited)
                ;"      01- patient_seq_num,
                ;"      02- facility_short_name,
                ;"      03- pat_last_name,
                ;"      04- pat_first_name,
                ;"      05- pat_account_num,
                ;"      06- pat_address,
                ;"      07- state,
                ;"      08- resp_last_name,
                ;"      09- resp_first_name,
                ;"      10- facility_seq_num,
                ;"      11- register_date,
                ;"      12- location_name,
                ;"      13- city,
                ;"      14- provider_short_name,
                ;"      15- zipcode,
                ;"      16- class_name,
                ;"      17- pat_dob,
                ;"      18- ref_prov_short_name,
                ;"      19- pat_tel_num,
                ;"      20- last_visit_days,
                ;"      21- name,
                ;"      22- description
                ;"      ADDENDUM:
                ;"        sometimes SEX will be appended to line.  Format:
                ;"              previous data^MALE or previous data^FEMALE
                ;"        sometimes SSN will be appended to line.  Format:
                ;"              previous data^(sex)^SSNUM
        ;"    ErrArray: PASS BY REFERENCE.  Array to receive failed data lines.
        ;"    ChgLog: PASS BY REFERENCE.  An array to receive record of changes made to database
        ;"    SSNArray: OPTIONAL -- PASS BY REFERENCE.  An array with social security numbers,
        ;"              as created by XtractSSNum()
        ;"              *** Note: This won't be used by this alteration of the code.
        ;"    DUZ: The user who will recieve alerts of errors
        ;"    InputFn:  OPTIONAL-- the name of a function to turn parse on csv line
        ;"              default value is "ParseLine"
        ;"              e.g. "MyFn" or "MyFn^MyRoutine".  Must take same params as ParseLine
        ;"              This will allow this code to be used on a variety of .csv files, with
        ;"              different data-formats--each one with its own parser funtion.
        ;"Output: Data is put into database, IF it is not there already.
        ;"Result: 1=OK To continue; 0=abort or bad data

        NEW XFn
        NEW PtInfo,OneErrArray
        NEW result SET result=1
        NEW AutoRegister SET AutoRegister=1
        SET InputFn=$GET(InputFn,"ParseLine")

        SET XFn="set result=$$"_InputFn_"(.OneLine,.PtInfo,.SSNArray)"
        xecute XFn       ;"old -- SET result=$$ParseLine(.OneLine,.PtInfo,.SSNArray)
        IF result'>0 GOTO PPtDone
        IF $GET(PtInfo("FACILITY"))="SAMPLE" GOTO PPtDone

        IF $$UpdateDB(.PtInfo,AutoRegister,.OneErrArray,.ChgLog)=0 do
        . NEW count SET count=+$GET(ErrArray)+1
        . SET ErrArray=count
        . SET ErrArray(count)=OneLine
        . MERGE ErrArray(count,"INFO")=OneErrArray
        . ;"------
        . DO ALERTERR^TMGSEQL2(OneLine,.PtInfo,.OneErrArray,DUZ)

PPtDone
        QUIT result


ParseLine(OneLine,Array,SSNArray)
        ;"Purpose: To process one line from patient demographics file.
        ;"         Also gets data into an acceptible format.
        ;"         This parser is written to handle the dataformat as put out by the PMS
        ;"         at the Clinica Adelante, with Matthew King, MD and Benjamin Guldborg
        ;"Input: OneLine -- One line from CVS demographics file. (Format as per ProcessPt)
        ;"      Format is as follows, *** all on one line (comma delimited)
        ;"      Note: Only a few of these fields are used... cde could be modified for use later
        ;"              This would be done by putting data field into Array("SOME NAME")=value
        ;"              Then modify UpdateDB to put data into VistA
        ;"      01-PatientProfileId
        ;"      02-PatientId
        ;"      03-Inactive
        ;"      04-Prefix
        ;"      05-First
        ;"      06-Middle
        ;"      07-Last
        ;"      08-Suffix
        ;"      09-Address1
        ;"      10-Address2
        ;"      11-City
        ;"      12-State
        ;"      13-Zip
        ;"      14-Country
        ;"      15-Phone1
        ;"      16-Phone1Type
        ;"      17-Phone2
        ;"      18-Phone2Type
        ;"      19-County
        ;"      20-EMailAddress
        ;"      21-EmpStat
        ;"      22-EmpStatusDate
        ;"      23-EmpOccup
        ;"      24-EduStat
        ;"      25-SchoolName
        ;"      26-SSN
        ;"      27-Birthdate
        ;"      28-DeathDate
        ;"      29-Sex
        ;"      30-Race
        ;"      31-MaritalStat
        ;"      32-GuarantorId
        ;"      33-GarPrefix
        ;"      34-GarFirst
        ;"      35-GarMiddle
        ;"      36-GarLast
        ;"      37-GarSuffix
        ;"      38-GarAddress1
        ;"      39-GarAddress2
        ;"      40-GarCity
        ;"      41-GarState
        ;"      42-GarZip
        ;"      43-GarCountry
        ;"      44-GarPhone1
        ;"      45-GarPhoneType1
        ;"      46-GarPhone2
        ;"      47-GarPhoneType2
        ;"      48-GarEmail
        ;"      49-GarSSN
        ;"      50-GarBirthDate
        ;"      51-GarSex
        ;"      52-GarRealation
        ;"      53-FinancialClass
        ;"      54-MedicalRecordNumber
        ;"      55-PCPDoctor
        ;"      56-PCFacility
        ;"
        ;"      ---- Below is the original data fields from Sequel PMS (FYI)---
        ;"      01- patient_seq_num,
        ;"      02- facility_short_name,
        ;"      03- pat_last_name,
        ;"      04- pat_first_name,
        ;"      05- pat_account_num,
        ;"      06- pat_address,
        ;"      07- state,
        ;"      08- resp_last_name,
        ;"      09- resp_first_name,
        ;"      10- facility_seq_num,
        ;"      11- register_date,
        ;"      12- location_name,
        ;"      13- city,
        ;"      14- provider_short_name,
        ;"      15- zipcode,
        ;"      16- class_name,
        ;"      17- pat_dob,
        ;"      18- ref_prov_short_name,
        ;"      19- pat_tel_num,
        ;"      20- last_visit_days,
        ;"      21- name,
        ;"      22- description
        ;"      ADDENDUM:
        ;"        sometimes SEX will be appended to line.  Format:
        ;"              previous data^MALE or previous data^FEMALE
        ;"        sometimes SSN will be appended to line.  Format:
        ;"              previous data^(sex)^SSNUM

        ;"         NOTE: IF PASSED BY REFERENCE, then line may be altered such that SSN is
        ;"              added as a 3rd piece, using ^ as a delimiter. (2nd piece used elsewhere
        ;"              to store sex.
        ;"              When processing line, IF SSNArray doesn't provide a SSN for patient, then
        ;"              this 3rd piece can provide the SSN
        ;"       Array -- PASS BY REFERENCE. And OUT parameter.  Any prior data killed.
        ;"       Note: uses TMGINVALID (globally scoped var defined in this module)
        ;"       SSNArray: OPTIONAL -- PASS BY REFERENCE.  An array with social security numbers,
        ;"               as created by XtractSSNum()
        ;"Output: Array is filled with Format as follows (note not all data used):
        ;"        Array("FACILITY")
        ;"        Array("LAST NAME")
        ;"        Array("FIRST NAME")
        ;"        Array("MIDDLE NAME") <--- NEW
        ;"        Array("NAME SUFFIX") <--- NEW
        ;"        Array("NAME PREFIX") <--- NEW
        ;"        Array("PMS ACCOUNT NUM")
        ;"        Array("ADDRESS1")
        ;"        Array("ADDRESS2")
        ;"        Array("ADDRESS3")
        ;"        Array("STATE")
        ;"        Array("RESP LAST NAME")
        ;"        Array("RESP FIRST NAME")
        ;"        Array("CITY")
        ;"        Array("PROVIDER")
        ;"        Array("ZIP CODE")
        ;"        Array("DOB")
        ;"        Array("PHONE NUM")
        ;"        Array("SEX")
        ;"        Array("SSNUM")=Social security number
        ;"        Array("FULL NAME")=FIRSTNAME MIDDLENAME LASTNAME SUFFIX(DOB)
        ;"        Array("FULL NAME2")=LASTNAME,FIRSTNAME MIDDLE SUFFIX (DOB)
        ;"        Array("FULL NAME3")=LASTNAME,FIRSTNAME MIDDLE SUFFIX
        ;"Result: 1=OK To continue; 0=abort or bad data; -1 skip, but don't store as error

        NEW temp
        NEW result SET result=1

        SET OneLine=$TRANSLATE($GET(OneLine),"""","'") ;"  convert " to ' to avoid fileman error

        KILL Array
        SET Array("FACILITY")="ADELANTE"  ;"hard code for only 1 site
        SET Array("LAST NAME")=$$Trim^TMGSTUTL($PIECE(OneLine,",",7))
        SET Array("FIRST NAME")=$$Trim^TMGSTUTL($PIECE(OneLine,",",5))
        SET Array("MIDDLE NAME")=$$Trim^TMGSTUTL($PIECE(OneLine,",",6))

        SET Array("NAME SUFFIX")=$$Trim^TMGSTUTL($PIECE(OneLine,",",6))
        ;"I'm not sure what to DO with prefix yet -- not used.
        SET Array("NAME PREFIX")=$$Trim^TMGSTUTL($PIECE(OneLine,",",6))

        SET Array("PMS ACCOUNT NUM")=$PIECE(OneLine,",",2)
        SET Array("ADDRESS1")=$PIECE(OneLine,",",9)
        SET Array("ADDRESS2")=$PIECE(OneLine,",",10)
        IF Array("ADDRESS2")="Same As Above" SET Array("ADDRESS2")=""
        SET Array("STATE")=$PIECE(OneLine,",",12)
        SET Array("RESP LAST NAME")=$PIECE(OneLine,",",36)
        SET Array("RESP FIRST NAME")=$PIECE(OneLine,",",34)
        SET Array("CITY")=$$Trim^TMGSTUTL($PIECE(OneLine,",",11),"""")
        SET Array("PROVIDER")=""
        SET Array("ZIP CODE")=$PIECE(OneLine,",",13)
        NEW DOB SET DOB=$PIECE(OneLine,",",27)
        SET DOB=$$Trim^TMGSTUTL(DOB)
        SET DOB=$PIECE(DOB," ",1)  ;" '03/09/05 00:00' --> '03/09/05'
        SET Array("DOB")=DOB
        SET Array("PHONE NUM")=$PIECE(OneLine,",",15)
        SET Array("SEX")=$PIECE(OneLine,"^",29)

        NEW tMName SET tMName=$GET(Array("MIDDLE NAME"))
        IF tMName'="" SET tMName=" "_tMName  ;"add space only IF middle name provided
        NEW tSuffix SET tSuffix=$GET(Array("NAME SUFFIX"))
        IF tSuffix'="" SET tSuffix=" "_tSuffix  ;"add space only IF suffix provided
        SET Array("FULL NAME")=Array("FIRST NAME")_tMName_" "_Array("LAST NAME")_tSuffix_" ("_Array("DOB")_")"
        SET Array("FULL NAME2")=Array("LAST NAME")_","_Array("FIRST NAME")_tMName_tSuffix_" ("_Array("DOB")_")"
        SET Array("FULL NAME3")=Array("LAST NAME")_","_Array("FIRST NAME")_tMName_tSuffix

        ;"do a lookup on abreviattion for ALL states, convert to external format
        NEW DIC,X,Y
        SET DIC=5 ;"STATE file
        SET DIC(0)="M"
        SET X=Array("STATE")
        DO ^DIC
        SET Array("STATE")=$PIECE(Y,"^",2)

        ;"  VistA address allows for:
        ;"      .111 -- address line 1
        ;"      .112 -- address line 2
        ;"      .113 -- address line 3
        ;"      BUT, each line must be 3-35 characters
        ;"  PMS might not match this
        ;"  SO, I need to divide the line IF not 3-35
        NEW value SET value=$GET(Array("ADDRESS1"))
        IF $LENGTH(value)'<35 do
        . NEW s1,s2
        . DO NiceSplit^TMGSTUTL(value,35,.s1,.s2,3)
        . SET Array("ADDRESS1")=s1
        . IF $GET(Array("ADDRESS2"))'="" SET s2=s2_"; "_$GET(Array("ADDRESS2"))
        . SET Array("ADDRESS2")=s2
        SET value=$GET(Array("ADDRESS2"))
        IF $LENGTH(value)'<35 do
        . DO NiceSplit^TMGSTUTL(value,35,.s1,.s2,3)
        . SET Array("ADDRESS2")=s1
        . IF s2'="" SET Array("ADDRESS3")=$EXTRACT(s2,1,35)

        ;"Ensure proper length of city.
        SET Array("CITY")=$EXTRACT(Array("CITY"),1,15)
        IF $LENGTH(Array("CITY"))=1 SET Array("CITY")=Array("CITY")_" "

        ;"Ensure proper length of phone
        IF $LENGTH(Array("PHONE NUM"))<7 KILL Array("PHONE NUM")

        NEW SSNum SET SSNum=$$Trim^TMGSTUTL($PIECE(OneLine,",",26))
        IF +SSNum'=SSNum SET SSNum=""   ;"remove alpha answers such as 'UNKNOWN'
        IF SSNum=999999999 SET SSNum=0
        IF +SSNum=0 DO   ;"see IF 3rd ^ piece holds SSNum data
        . SET SSNum=$PIECE(OneLine,"^",3) ;"note this won't overwrite valid data from SSNArray()
        IF SSNum>0 do
        . SET Array("SSNUM")=SSNum
        . SET $PIECE(OneLine,"^",3)=SSNum

PLDone
        QUIT result


GETDFN(PtInfo)
        ;"Purpose: Serve as interface to ^TMGGDFN functions (using PtInfo as input)
        ;"Input: PtInfo, Array of PtInfo, as defined in UpdateDB, and created by ParseLine
        ;"Result: the IEN in file 2 (i.e. DFN) IF found, otherwise 0 IF not found.

        NEW Entry,TMGDFN

        SET Entry(.01)=$$FormatName^TMGMISC($GET(Array("FULL NAME3")))
        SET Entry(.03)=$GET(PtInfo("DOB"))
        SET Entry(.02)=$GET(PtInfo("SEX"))
        SET Entry(.09)=$GET(PtInfo("SSNUM"))
        SET TMGDFN=+$$LOOKUPPAT^TMGGDFN(.Entry)  ;"get IEN in file 2 of patient
        ;"do an extended search with increasing intensity.
        IF +TMGDFN=0 SET TMGDFN=$$EXTRLKUP^TMGGDFN(.Entry,1)
        IF +TMGDFN=0 SET TMGDFN=$$EXTRLKUP^TMGGDFN(.Entry,2)
        IF +TMGDFN=0 SET TMGDFN=$$EXTRLKUP^TMGGDFN(.Entry,3)

        QUIT TMGDFN


UpdateDB(PtInfo,AutoRegister,ErrArray,ChgLog)
        ;"Purpose: To put that data from the PtInfo array into the database (if needed)
        ;"Input: PtInfo -- array (PASS BY REFERENCE), with the following items being used:
        ;"              PtInfo("FULL NAME3")          ----> field .01
        ;"              PtInfo("SEX")                 ----> field .02
        ;"              PtInfo("DOB")                 ----> field .03
        ;"              PtInfo("SSNUM")               ----> field .09
        ;"              PtInfo("PMS ACCOUNT NUM")     ----> field 22701 (custom field)
        ;"              PtInfo("ADDRESS")             ----> field .111
        ;"              PtInfo("STATE")               ----> field .115
        ;"              PtInfo("CITY")                ----> field .114
        ;"              PtInfo("ZIP CODE")            ----> field .1112
        ;"              PtInfo("PHONE NUM")           ----> field .131
        ;"              PtInfo("PROVIDER")            ----> field .1041
        ;"      AutoRegister: IF 1, then patient will be automatically added/registered
        ;"      ErrArray -- PASS BY REFERENCE.  And OUT parameter to get back error info.
        ;"      ChgLog: PASS BY REFERENCE.  An array to receive record of changes made to database
        ;"Output: Data is put into database, IF it is not there already.
        ;"Result: 1 successful completion, 0=error

        NEW Entry
        NEW result SET result=1
        NEW Name,TMGDOB,TMGDFN
        NEW TMGARRAY,TMGMSG
        NEW PriorErrorFound
        NEW NewInfo
        NEW IENS
        NEW INDEX
        KILL ErrArray
        NEW TMGDEBUG SET TMGDEBUG=-1 ;"//EXTRA QUIET mode --> shut down TMGDBAPI messages

        NEW Fields
        ;"Store names indexes of import data to compare with
        SET Fields(22701)="PMS ACCOUNT NUM"
        SET Fields(.01)="FULL NAME3"
        SET Fields(.02)="SEX"
        SET Fields(.03)="DOB"
        SET Fields(.09)="SSNUM"
        SET Fields(.111)="ADDRESS1"
        SET Fields(.112)="ADDRESS2"
        SET Fields(.113)="ADDRESS3"
        SET Fields(.115)="STATE"
        SET Fields(.114)="CITY"
        SET Fields(.1112)="ZIP CODE"
        SET Fields(.131)="PHONE NUM"
        SET Fields(.1041)="PROVIDER"

        ;"This will be fields to get from VistA for comparison
        ;"For every number there should be a matched entry above.
        SET Fields="22701;.01;.02;.03;.09;.111;.112;.113;.115;.114;.1112;.131;.1041"

        SET Name=$GET(PtInfo("FULL NAME3"))
        SET Name=$$FormatName^TMGMISC(Name)
        SET TMGDOB=$GET(PtInfo("DOB"))

        SET Entry(.01)=Name
        SET Entry(.03)=TMGDOB
        IF $GET(PtInfo("SEX"))'="" SET Entry(.02)=$GET(PtInfo("SEX"))
        SET Entry(.09)=$GET(PtInfo("SSNUM"))

        SET TMGDFN=$$GETDFN(.PtInfo)

        ;"Add patient to database (register) IF appropriate
        IF (TMGDFN=0)&($GET(AutoRegister)=1) do
        . SET ErrArray=-1  ;"extra quiet mode.
        . IF $GET(Entry(.02))="" DO  ;"autopick gender IF missing
        . . NEW AutoPick
        . . SET AutoPick=$$GET1^DIQ(22711,"1,","PICK GENDER FROM NAME?","I")
        . . IF AutoPick'=1 QUIT
        . . SET Entry(.02)=$$GETSEX^TMGSEQL2($GET(PtInfo("FIRST NAME")))
        . ;"OK, can't find, so will add NEW patient.
        . ;"SET TMGDFN=+$$ADDNEWPAT^TMGGDFN(.Entry,.ErrArray)
        . SET TMGDFN=+$$ADDNEWPAT^TMGGDFN(.Entry)
        . IF TMGDFN'=0 SET ChLog(Name_" "_TMGDOB,0)="ADDED PATIENT: "_Name_" "_TMGDOB
        IF TMGDFN=0 DO  GOTO UDBDone  ;"failure
        . SET result=0
        . SET ErrArray(0)=$$NAMEERR^TMGSEQL2(.ErrArray)  ;"get name IF DIERR encountered.
        . IF ErrArray(0)["DOB" do
        . . ;"WRITE !,"DOB error found for: ",PtInfo("FULL NAME"),!
        . IF ErrArray(0)="" do
        . . SET ErrArray(0)="PATIENT NOT IN DATABASE:"  ;"if changed, also change in TMGSEQL2.m
        SET IENS=TMGDFN_","

        ;"use DFN(IEN in file 2) to get data from database for comparison
        DO GETS^DIQ(2,IENS,Fields,"","TMGARRAY","TMGMSG")

        ;"check for errors.
        IF $DATA(TMGMSG("DIERR"))'=0 DO  GOTO UDBDone
        . SET result=0
        . MERGE ErrArray=TMGMSG("DIERR")
        . ;"do SHOWDIER^TMGDEBU2(.TMGMSG,.PriorErrorFound)
        KILL TMGMSG

        ;"If any data in data base differs from Array, setup NewInfo
        NEW UpdateNeeded SET UpdateNeeded=0
        NEW ABORT SET ABORT=0
        SET INDEX=$ORDER(Fields(""))
        FOR  DO  QUIT:(+INDEX'>0)!(ABORT=1)
        . NEW field SET field=Fields(INDEX)
        . IF $DATA(PtInfo(field)),$GET(TMGARRAY(2,IENS,INDEX))'=$GET(PtInfo(field)) do
        . . NEW value SET value=$GET(PtInfo(field))
        . . IF INDEX=.1112 do
        . . . IF +value'=0 do
        . . . . SET UpdateNeeded=1
        . . . . SET NewInfo(INDEX)=value
        . . ELSE  IF (INDEX=.09)&(+value'=0)&(+TMGARRAY(2,IENS,INDEX)'=0) do
        . . . IF TMGARRAY(2,IENS,INDEX)["P" DO  QUIT
        . . . . SET UpdateNeeded=1
        . . . . SET NewInfo(INDEX)=value
        . . . ;"we have CONFLICTING SOCIAL SECURITY NUMBERS --> PROBLEM...
        . . . SET ErrArray(0)="CONFLICTING SS-NUMBERS: " ;"NOTE! IF error message format is changed, also change in TMGSEQL2
        . . . SET ErrArray(0)=ErrArray(0)_"PMS#="_PtInfo(field)_" vs. VistA#="_TMGARRAY(2,IENS,INDEX)
        . . . SET ABORT=1,result=0
        . . ELSE  IF INDEX=.03 DO  ;"compare internal values of DOBs, not external values
        . . . NEW %DT SET %DT="P"  ;"past dates                                        A
        . . . NEW ddate1,date2
        . . . SET X=value DO ^%DT SET date1=Y  ;"get internal form of date, store in date1
        . . . SET X=$GET(TMGARRAY(2,IENS,INDEX))
        . . . DO ^%DT SET date2=Y  ;"get internal form of date, store in date2
        . . . IF date1'=date2 SET NewInfo(.03)=value  ;"dates DO differ, so update Vist
        . . . SET UpdateNeeded=1
        . . ELSE  do
        . . . SET NewInfo(INDEX)=value
        . . . SET UpdateNeeded=1
        . SET INDEX=$ORDER(Fields(INDEX))

        IF (UpdateNeeded=0)!(ABORT=1) GOTO UDBDone

        ;"Setup FDA array for database update
        NEW TMGFDA
        SET INDEX=$ORDER(NewInfo(""))
        IF INDEX'=""  do
        . FOR  DO  QUIT:(+INDEX'>0)
        . . SET TMGFDA(2,IENS,INDEX)=NewInfo(INDEX)
        . . SET INDEX=$ORDER(NewInfo(INDEX))
        . ;
        . DO FILE^DIE("E","TMGFDA","TMGMSG")
        . IF $DATA(TMGMSG("DIERR"))'=0 DO  ;"GOTO UDBDone
        . . SET result=0
        . . MERGE ErrArray=TMGMSG("DIERR")

        MERGE ChLog($GET(Name,"?")_" "_$GET(TMGDOB,"?"),1)=NewInfo

UDBDone
        QUIT result

