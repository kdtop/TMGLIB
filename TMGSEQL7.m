TMGSEQL7 ;TMG/KST - Parse current schedule status file (From SequelPMS); 11/06/17, 3/24/21
        ;;1.0;TMG-LIB;**1**; 11/6/17
       ;
 ;"TMG SEQUEL PMS FUNCTIONS -- Importing appointments. 
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
 ;"LOAD(FULLPATHNAME,DELFILE) -- Load specified file with schedule information
 ;"HNDLTASK -- entry to scheduled task to load appointment info from SequelMed
 ;"TEST1 -- Ask user for file name to load.
 ; 
 ;"=======================================================================
 ;" Private Functions.
 ;"=======================================================================
 ;"PARSE(REFDATA) -- Parse data from future-appointents output, from SequelPMS
 ;"PARSE1(REFDATA,IDX) -- Parse 1 data element from data, 
 ;"FILE1(TMGDFN,DATA) -- File 1 data entry into TMG SCHEDULE file (22723) 
 ;"EXISTS(TMGDFN,DATA) -- Check if 1 data entry has already been filed into TMG SCHEDULE file (22723) 
 ;"ENSURE1(TMGDFN,DATA) -- Ensure 1 data entry is filed into TMG SCHEDULE file (22723) 
 ;"ENSURALL(DATA) -- Ensure all parsed data has been filed. 
 ;"CHKREMVD(DATA,MINDT,MAXDT,OUT) -- find stored records on disk not present in DATA
 ;"DOCANCEL(OLD,ERR) -- Set status of obsolete records to CANCELLED
 ;"ERR(MSG) -- Set up alert with information about errors
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;" TMGIOUTL,TMGSEQE1,TMGIOUT4
 ;"=======================================================================
 ;
PARSE(REFDATA)  ;"Parse data from future appointents output, from SequelPMS
  ;"Input: REFDATA -- PASS BY NAME.  Reference for ARRAY containing appointment data
  ;"          Format: 
  ;"             @REFDATA@("A",<Field#>)=FieldName
  ;"             @REFDATA@(Record#,<Field#>)=FieldValue
  ;"          Example: 
  ;"            DATA(82)                                                                        
  ;"            }~1 = 11/6/2017 00:00:00
  ;"            }~2 = 11/6/2017 16:15:00
  ;"            }~3 = 15
  ;"            }~4 = FOCUS
  ;"            }~5 = KTOPPEN
  ;"            }~6 = 1082229
  ;"            }~7 = ""
  ;"            }~8 = SCHEDULED
  ;"            }~9 = ""
  ;"            }~10 = ""
  ;"            }~11 = ""
  ;"            }~12 = ********, MA***
  ;"            }~13 = 321360
  ;"            }~14 = FPG
  ;"            }~15 = FPG
  ;"            }~16 = 67650
  ;"            }~17 = ""
  ;"            }~18 = 1
  ;"            }~19 = 67650
  ;"            }~20 = 875165
  ;"            }~21 = 4239231954
  ;"            }~22 = 4232357631
  ;"            }~23 = ""
  ;"            }~24 = 67650
  ;"            }~25 = 16777215
  ;"            }~26 = ""
  ;"            
  ;"            DATA("A")                                                                       
  ;"            }~1 = appointment_date
  ;"            }~2 = from_time
  ;"            }~3 = slot_minutes
  ;"            }~4 = reason_short_name
  ;"            }~5 = provider_resource
  ;"            }~6 = provider_seq_num
  ;"            }~7 = resource_seq_num
  ;"            }~8 = status
  ;"            }~9 = time_in
  ;"            }~10 = time_out
  ;"            }~11 = comments
  ;"            }~12 = pat_name
  ;"            }~13 = pat_account
  ;"            }~14 = practice_name
  ;"            }~15 = location_name
  ;"            }~16 = seq_num
  ;"            }~17 = card_sent_date
  ;"            }~18 = status_seq_num
  ;"            }~19 = parent_seq_num
  ;"            }~20 = patient_seq_num
  ;"            }~21 = tel_num
  ;"            }~22 = work_tel_num
  ;"            }~23 = work_tel_ext
  ;"            }~24 = ultimate_parent_seq_num
  ;"            }~25 = comment_color
  ;"            }~26 = eilbility_response
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
  . DO PARSE1(REFDATA,IDX)
  QUIT
  ;    
PARSE1(REFDATA,IDX) ;
  NEW ERR SET ERR=""
  ;"NEW NAMEIDX SET NAMEIDX=5  ;"1/19/21 ADDED
  ;"IF $G(DATA("A",1))="provider_name" SET NAMEIDX=1     
  NEW NAME SET NAME=$GET(@REFDATA@(IDX,5))
  SET NAME=$$GETPROVNAME(NAME)
  NEW SEQLNUM SET SEQLNUM=+$GET(@REFDATA@(IDX,13))
  NEW TMGDFN SET TMGDFN=-1
  IF SEQLNUM>0 SET TMGDFN=+$ORDER(^DPT("TMGS",SEQLNUM,""))
  IF TMGDFN'>0 SET ERR="EMPT|Unable to match patient to VistA database" GOTO P1DN
  SET DIC=200,DIC(0)="M",X=NAME DO ^DIC
  IF +Y'>0 SET ERR="EMPV|Unable to locate provider IEN" GOTO P1DN
  NEW PROVIEN SET PROVIEN=+Y
  NEW APPTDT SET APPTDT=$$GETFMDT($GET(@REFDATA@(IDX,2)))
  IF APPTDT'>0 SET ERR="EDT|Unable to determine appt time" GOTO P1DN
  SET @REFDATA@("DT",APPTDT,TMGDFN)=""
  SET @REFDATA@("APPT",TMGDFN,APPTDT,"PROVIEN")=PROVIEN
  ;"SET @REFDATA@("APPT",TMGDFN,APPTDT,"DT")=Y
  SET @REFDATA@("APPT",TMGDFN,APPTDT,"REASON")=$GET(@REFDATA@(IDX,4))
  SET @REFDATA@("APPT",TMGDFN,APPTDT,"MIN")=$GET(@REFDATA@(IDX,3))
  SET @REFDATA@("APPT",TMGDFN,APPTDT,"TIMEIN")=$$GETFMDT($GET(@REFDATA@(IDX,9)))
  SET @REFDATA@("APPT",TMGDFN,APPTDT,"TIMEOUT")=$$GETFMDT($GET(@REFDATA@(IDX,10)))
P1DN ;
  IF ERR'="" SET @REFDATA@("ERR",IDX)=ERR
  QUIT
  ;
GETFMDT(DATETIME)
  IF DATETIME="" QUIT ""
  NEW DATE,TIME SET DATE=$PIECE(DATETIME," ",1),TIME=$PIECE(DATETIME," ",2)
  SET DATETIME=DATE_"@"_$PIECE(TIME,":",1,2)
  NEW %DT SET %DT="T" SET X=DATETIME DO ^%DT
  QUIT Y
  ;"
GETPROVNAME(NAME)
  ;"Provider name is not sent, so we have to hardcode the name for now
  NEW RETURNNAME
  IF (NAME="KTOPPEN")!(NAME="NURSE") SET RETURNNAME="TOPPENBERG,KEVIN"
  IF NAME="MTOPPEN" SET RETURNNAME="TOPPENBERG,MARCIA"
  QUIT RETURNNAME
  ;"
FILE1(TMGDFN,DT,DATA)  ;"File 1 data entry into TMG SCHEDULE file (22723) 
  ;"Input: TMGDFN -- IEN in PATIENT file
  ;"       DT -- The DateTime of the appt (FM Format)
  ;"       DATA -- Array with input data.  Relevent part shown below 
  ;"          DATA("APPT",TMGDFN,DT
  ;"            ;"REMOVED --> "DT")=Appt start date-time (FM format)
  ;"            "MIN")=minutes duration of appt   
  ;"            "PROVIEN")=IEN of the provider who the appt is with 
  ;"            "REASON")=Short name for reason for appt.        
  ;"            "TIMEIN")=Patient's check in time
  ;"            "TIMEOUT")=Patient's check out time
  ;"            "COMMENT")=Comment text  <-- optional
  ;"NOTE: Data is stored such that IEN in TMG SCHEDULE = IEN in PATIENT file.
  ;"NOTE2: If DT already exists as a subrecord, it will be deleted and refiled.
  ;"Result: 1^OK, or -1^Error Message
  NEW TMGRESULT SET TMGRESULT="1^OK"
  NEW TMGFDA,TMGIEN,TMGMSG,IENS
  SET TMGDFN=+$GET(TMGDFN)
  IF TMGDFN'>0 SET TMGRESULT="-1^DFN not provided" GOTO F1DN
  SET DT=+$GET(DT) 
  IF DT'>0 SET TMGRESULT="-1^DATE/TIME of appt not found" GOTO F1DN
  NEW REC MERGE REC=DATA("APPT",TMGDFN,DT)
  IF $DATA(^TMG(22723,TMGDFN))>0 GOTO F1B
  ;"Set up main record
  SET IENS="+1,",TMGIEN(1)=TMGDFN
  SET TMGFDA(22723,IENS,.01)=TMGDFN
  DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO F1DN
  . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
F1B ;" Check for existing subrecord for date
  ;" REMOVED --> NEW DT SET DT=+$GET(REC("DT"))
  ;" IF DT'>0 SET TMGRESULT="-1^DATE/TIME of appt not found" GOTO F1DN
  NEW DA SET DA=+$ORDER(^TMG(22723,TMGDFN,1,"B",DT,""))
  IF DA'>0 GOTO F1C
  NEW DIK SET DIK="^TMG(22723,"_TMGDFN_",1,",DA(1)=TMGDFN
  DO ^DIK  ;"kill old entry
F1C ;"Add new subrecord
  SET IENS="+1,"_TMGDFN_"," KILL TMGIEN,TMGFDA
  SET TMGFDA(22723.01,IENS,.01)=DT
  NEW MIN SET MIN=$GET(REC("MIN"))
  IF MIN'="" SET TMGFDA(22723.01,IENS,.02)=MIN
  NEW PROVIEN SET PROVIEN=+$GET(REC("PROVIEN"))
  IF PROVIEN>0 SET TMGFDA(22723.01,IENS,.03)=PROVIEN
  NEW REASON SET REASON=$GET(REC("REASON"))
  IF REASON'="" SET TMGFDA(22723.01,IENS,.04)=REASON
  NEW TIMEIN SET TIMEIN=$GET(REC("TIMEIN"))
  IF TIMEIN'="" SET TMGFDA(22723.01,IENS,.08)=TIMEIN
  NEW TIMEOUT SET TIMEOUT=$GET(REC("TIMEOUT"))
  IF TIMEOUT'="" SET TMGFDA(22723.01,IENS,.09)=TIMEOUT
  SET TMGFDA(22723.01,IENS,.07)="A"  ;"ACTIVE status
  DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO F1DN
  . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
F1DN ;
  QUIT TMGRESULT
  ;
EXISTS(TMGDFN,DT,DATA)  ;"Check if 1 data entry has already been filed into TMG SCHEDULE file (22723) 
  ;"Input: TMGDFN -- IEN in PATIENT file
  ;"       DT -- The DateTime of the appt (FM Format)
  ;"       DATA -- Array with input data.  Relevent part shown below 
  ;"          DATA("APPT",TMGDFN, 
  ;"            ;"--> REMOVED  "DT")=Appt start date-time (FM format)
  ;"            "MIN")=minutes duration of appt   
  ;"            "PROVIEN")=IEN of the provider who the appt is with 
  ;"            "REASON")=Short name for reason for appt.        
  ;"            "COMMENT")=Comment text  <-- optional
  ;"Result: 1 if record exists, or 0 if doesn't exist, or there is a difference in stored values
  NEW TMGRESULT SET TMGRESULT=0  ;"Default = DOESN'T exist
  SET TMGDFN=+$GET(TMGDFN) IF TMGDFN'>0 GOTO EXSDN
  SET DT=+$GET(DT) IF DT'>0 GOTO EXSDN
  NEW REC MERGE REC=DATA("APPT",TMGDFN,DT)
  IF $DATA(^TMG(22723,TMGDFN))'>0 GOTO EXSDN
  ;"NEW DT SET DT=+$GET(REC("DT")) IF DT'>0 GOTO EXSDN
  NEW SUBIEN SET SUBIEN=+$ORDER(^TMG(22723,"DT",DT,TMGDFN,0)) IF SUBIEN'>0 GOTO EXSDN
  NEW ZN SET ZN=$GET(^TMG(22723,TMGDFN,1,SUBIEN,0))
  ;"look for differences between existing stored record and current data
  IF $GET(REC("MIN"))'=$PIECE(ZN,"^",2) GOTO EXSDN
  IF $GET(REC("PROVIEN"))'=$PIECE(ZN,"^",3) GOTO EXSDN
  IF $GET(REC("REASON"))'=$PIECE(ZN,"^",4) GOTO EXSDN
  IF $GET(REC("TIMEIN"))'=$PIECE(ZN,"^",8) GOTO EXSDN
  IF $GET(REC("TIMEOUT"))'=$PIECE(ZN,"^",9) GOTO EXSDN
  IF "A"'=$PIECE(ZN,"^",7) GOTO EXSDN
  ;"Since we got here, no differences found, so all OK
  SET TMGRESULT=1
EXSDN ;
  QUIT TMGRESULT
  ;    
ENSURE1(TMGDFN,DT,DATA)  ;"Ensure 1 data entry is filed into TMG SCHEDULE file (22723) 
  ;"Input: TMGDFN -- IEN in PATIENT file
  ;"       DT -- The DateTime of the appt (FM Format)
  ;"       DATA -- Array with input data.  Relevent part shown below 
  ;"          DATA("APPT",TMGDFN, 
  ;"            ;"removed --> "DT")=Appt start date-time (FM format)
  ;"            "MIN")=minutes duration of appt   
  ;"            "PROVIEN")=IEN of the provider who the appt is with 
  ;"            "REASON")=Short name for reason for appt.        
  ;"            "COMMENT")=Comment text  <-- optional
  ;"NOTE: Data is stored such that IEN in TMG SCHEDULE = IEN in PATIENT file.
  ;"NOTE2: If DT already exists as a subrecord, it will be LEFT AS IS
  ;"Result: 1^OK, or -1^Error Message  
  NEW TMGRESULT SET TMGRESULT="1^OK"
  IF $$EXISTS(.TMGDFN,DT,.DATA)=0 SET TMGRESULT=$$FILE1(TMGDFN,DT,.DATA)
  QUIT TMGRESULT
  ;    
ENSURALL(DATA)  ;"File all parsed data
  ;"Input: PASS BY REFERENCE.  Data array as created by PARSE()
  ;"Output: DATA("APPT","ERR",DFN)=ERROR MESSAGE
  NEW TMGDFN SET TMGDFN=0
  FOR  SET TMGDFN=$ORDER(DATA("APPT",TMGDFN)) QUIT:(+TMGDFN'>0)  DO
  . NEW DT SET DT=0
  . FOR  SET DT=$ORDER(DATA("APPT",TMGDFN,DT)) QUIT:+DT'>0  DO
  . . NEW TMGRESULT SET TMGRESULT=$$ENSURE1(TMGDFN,DT,.DATA)
  . . IF +TMGRESULT=1 QUIT
  . . SET DATA("APPT","ERR",TMGDFN,DT)=$PIECE(TMGRESULT,"^",2)
  QUIT
  ;  
CHKREMVD(DATA,MINDT,MAXDT,OUT) ;
  ;"Purpose: For date range, find stored records on disk not present in DATA
  ;"Input: DATA -- PASS BY REFERENCE.  Array with input data.  Relevent part shown below 
  ;"          ;"removed --> DATA("APPT",DFN,"DT")=Appt start date-time (FM format)
  ;"          DATA("APPT",DFN,FMDT,...)
  ;"          DATA("DT",FMDATE,DFN)=""
  ;"       MINDT --FM DATE, start of date range to test  E.g. 3111101
  ;"       MAXDT -- FM DATE, end of date range to test.  E.g. 3111231.999999
  ;"       OUT -- PASS BY REFERENCE. Format as per Output below. 
  ;"Output: OUT is filled with disk records that are obsolete 
  ;"           OUT(DFN,SUBIEN)=""  <-- show records that should be cancelled/removed etc.
  ;"Result: none
  NEW DT SET DT=+$GET(MINDT),MAXDT=+$GET(MAXDT)
  FOR  SET DT=$ORDER(^TMG(22723,"DT",DT)) QUIT:(DT'>0)!(DT>MAXDT)  DO
  . NEW ADFN SET ADFN=0 FOR  SET ADFN=$ORDER(^TMG(22723,"DT",DT,ADFN)) QUIT:(ADFN'>0)  DO
  . . ;"IF $DATA(DATA("APPT",ADFN,"DT"))>0 QUIT 
  . . IF $DATA(DATA("APPT",ADFN,DT))>0 QUIT 
  . . NEW SUBIEN SET SUBIEN=0
  . . FOR  SET SUBIEN=$ORDER(^TMG(22723,"DT",DT,ADFN,SUBIEN)) QUIT:+SUBIEN'>0  DO
  . . . NEW STATUS SET STATUS=$GET(^TMG(22723,"DT",DT,ADFN,SUBIEN))
  . . . IF STATUS'="A" QUIT
  . . . SET OUT(ADFN,SUBIEN)=STATUS  
  QUIT
  ;
MARKOLD(DT,ERR) ;
  ;"Purpose: Mark any appts older than DT to OLD status
  ;"       ERR -- PASS BY NAME.  AN OUT PARAMETER.  Format
  ;"         @ERR@(DFN,SUBIEN)=FM Error message
  ;"Results: NONE
  ;"Output: Database is changed, and @ERR may be filled with error messages.
  NEW ADT SET ADT=DT
  NEW DONE SET DONE=0
  FOR  SET ADT=$ORDER(^TMG(22723,"DT",ADT),-1) QUIT:(+ADT'>0)!DONE  DO
  . NEW ADFN SET ADFN=0
  . FOR  SET ADFN=$ORDER(^TMG(22723,"DT",ADT,ADFN)) QUIT:(+ADFN'>0)!DONE  DO
  . . NEW SUBIEN SET SUBIEN=0
  . . FOR  SET SUBIEN=$ORDER(^TMG(22723,"DT",ADT,ADFN,SUBIEN)) QUIT:(+SUBIEN'>0)!DONE  DO
  . . . NEW STATUS SET STATUS=$GET(^TMG(22723,"DT",ADT,ADFN,SUBIEN))
  . . . IF STATUS="O" SET DONE=1 QUIT  ;"'O', as in OLD, not "0" (zero) 
  . . . NEW TMGFDA SET TMGFDA(22723.01,SUBIEN_","_ADFN_",",.07)="O"
  . . . NEW TMGMSG DO FILE^DIE("","TMGFDA","TMGMSG")
  . . . IF $DATA(TMGMSG("DIERR")) DO
  . . . . ;"1/24/19. below generated error because TMGDFN is not defined.
  . . . . ;"Added quotes around it because intention was not clear
  . . . . NEW ERRSTR SET ERRSTR=$GET(@ERR@("ERR","TMGDFN",SUBIEN))
  . . . . IF ERRSTR'="" SET ERRSTR=ERRSTER_" // "
  . . . . SET @ERR@("TMGDFN",SUBIEN)=ERRSTR_$$GETERRST^TMGDEBU2(.TMGMSG)
  QUIT
  ;
DOCANCEL(OLD,ERR) ;"Set status of obsolete records to CANCELLED
  ;"Input: OLD -- PASS BY REFERENCE.  Format:
  ;"         OLD(DFN,SUBIEN)=""  <-- show records that should be cancelled/removed etc.
  ;"       ERR -- PASS BY NAME.  AN OUT PARAMETER.  Format
  ;"         @ERR@(DFN,SUBIEN)=FM Error message
  ;"Result: none
  NEW TMGFDA,TMGMSG
  NEW TMGDFN SET TMGDFN=0
  FOR  SET TMGDFN=$ORDER(OLD(TMGDFN)) QUIT:+TMGDFN'>0  DO
  . NEW SUBIEN SET SUBIEN=0 FOR  SET SUBIEN=$ORDER(OLD(TMGDFN,SUBIEN)) QUIT:+SUBIEN'>0  DO
  . . KILL TMGFDA,TMGMSG
  . . SET TMGFDA(22723.01,SUBIEN_","_TMGDFN_",",.07)="C"
  . . DO FILE^DIE("","TMGFDA","TMGMSG")
  . . IF $DATA(TMGMSG("DIERR")) SET @ERR@(TMGDFN,SUBIEN)=$$GETERRST^TMGDEBU2(.TMGMSG)
  QUIT
  ;
ERR(MSG) ;
  ;"Note setting up alert will save all variables on the variable table for
  ;"     use during error handler
  ;"DO SETALRT^TMGSEQE1(MSG,"","HNDLERR^TMGSEQE1")
  QUIT
  ;
LOAD(FULLPATHNAME,DELFILE) ;
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
  IF +TMGRESULT'>0 DO ERR(TMGRESULT_" on "_$$FMTE^XLFDT($$NOW^XLFDT)) GOTO LDDN
  DO PARSE("DATA")
  NEW MINDT SET MINDT=+$ORDER(DATA("DT",0))\1
  NEW MAXDT SET MAXDT=(+$ORDER(DATA("DT",""),-1)\1)+(0.999999)
  NEW OBSOLETE DO CHKREMVD(.DATA,MINDT,MAXDT,.OBSOLETE)  
  DO ENSURALL(.DATA)
  DO DOCANCEL(.OBSOLETE,$NAME(DATA("ERR2"))) ;"Set status of obsolete records to CANCELLED  
  DO MARKOLD($$NOW^XLFDT,$NAME(DATA("ERR2"))) ;"Set status of past records to OLD     
  IF ($DATA(DATA("ERR")))!($DATA(DATA("ERR2")))!($DATA(DATA("APPT","ERR"))) DO
  . DO ERR("Error(s) during schedule import") 
  ELSE  IF $GET(DELFILE)=1 DO
  . ;"DO ERR("NO ERROR: Imported schedule OK... (can remove this in TMGSEQL6.m)") 
  . IF $$DELFILE^TMGIOUTL(FULLPATHNAME)=0 DO ERR("Error deleting file:")  
LDDN ;  
  QUIT  
  ;
HNDLTASK ;"HANDLE TASK
  ;"Purpose: this is entry to scheduled task to load appointment info from SequelMed
  NEW FULLPATHNAME SET FULLPATHNAME="/mnt/WinServer/FutureAppointments.csv"
  DO LOAD(FULLPATHNAME,1)
  QUIT  
  ;"
TEST1 ;
  NEW OPTION SET OPTION("MATCH","FutureAppointments.csv")=""
  NEW FULLPATHNAME SET FULLPATHNAME=$$FBROWSE^TMGIOUT2(.OPTION)
  DO LOAD(FULLPATHNAME,1)
  QUIT  
  ;  
GETAPPTS(OUT,TMGDFN,ASOFDT)  ;"Get list of appointments on or after ASOFDT for patient
  ;"NOTE: Pulls ONLY from file 22723, not other VistA schedule files. 
  ;"Input: OUT -- PASS BY REFERENCE.  An OUTPUT PARAMETER.  See output below.  
  ;"       TMGDFN -- The patient to look up
  ;"       ASOFDT -- OPTIONAL.  If not provided, then NOW is used. 
  ;"Output: OUT is filled as follows ...
  ;"          OUT(TMGDFN,APPTFMDT)=ApptDT^MinDuration^ProviderIEN^ApptReason^Flags^Comment^Status
  ;"Results: Returns number of appts found
  SET ASOFDT=+$GET(ASOFDT) IF ASOFDT'>0 SET ASOFDT=$$NOW^XLFDT
  SET TMGDFN=+$GET(TMGDFN)
  NEW CT SET CT=0
  NEW DTIDX SET DTIDX=ASOFDT-0.000001
  FOR  SET DTIDX=$ORDER(^TMG(22723,TMGDFN,1,"B",DTIDX)) QUIT:(+DTIDX'>0)  DO
  . NEW SUBIEN SET SUBIEN=+$ORDER(^TMG(22723,TMGDFN,1,"B",DTIDX,0)) QUIT:+SUBIEN'>0
  . SET OUT(TMGDFN,DTIDX)=$GET(^TMG(22723,TMGDFN,1,SUBIEN,0)),CT=CT+1
  QUIT CT
  ;
TEST2 ;
  NEW X,Y,DIC SET DIC=2,DIC(0)="MAEQ" DO ^DIC WRITE !
  IF +Y'>0 QUIT
  NEW OUT,CT SET CT=$$GETAPPTS(.OUT,+Y)
  WRITE CT," appts entries found.",!
  IF $DATA(OUT) DO ZWRITE^TMGZWR("OUT")
  QUIT
  ;
LOADONE ;"LOAD ONE DAY, FOR SCHEDULE STATUSES
  ;"Purpose: Load file with current day's schedule information, including
  ;"         check-in and check-out info
  NEW DATA,TMGRESULT
  NEW FULLPATHNAME SET FULLPATHNAME="/mnt/WinServer/ScheduleStatus.csv"
  NEW DELFILE SET DELFILE=0
  NEW X DO NOW^%DTC
  SET TMGRESULT=$$LCSV2ARR^TMGIOUT4(FULLPATHNAME,"DATA")
  IF +TMGRESULT'>0 DO
  . DO DW^%DTC
  . NEW DAYS SET DAYS="MONDAY,TUESDAY,THURSDAY,FRIDAY"
  . IF DAYS'[X SET TMGRESULT="NOT PROPER DAY"
  IF TMGRESULT="NOT PROPER DAY" GOTO LDDN
  IF +TMGRESULT'>0 DO ERR(TMGRESULT_" on "_$$FMTE^XLFDT($$NOW^XLFDT)) GOTO LODN
  DO PARSE("DATA")
  NEW MINDT SET MINDT=+$ORDER(DATA("DT",0))\1
  NEW MAXDT SET MAXDT=(+$ORDER(DATA("DT",""),-1)\1)+(0.999999)
  NEW OBSOLETE DO CHKREMVD(.DATA,MINDT,MAXDT,.OBSOLETE)
  DO ENSURALL(.DATA)
  DO DOCANCEL(.OBSOLETE,$NAME(DATA("ERR2"))) ;"Set status of obsolete records to CANCELLED
  ;" do we really need to do this here??    ;->  1/19/21 DO MARKOLD($$NOW^XLFDT,$NAME(DATA("ERR2"))) ;"Set status of past records to OLD
  IF ($DATA(DATA("ERR")))!($DATA(DATA("ERR2")))!($DATA(DATA("APPT","ERR"))) DO
  . DO ERR("Error(s) during schedule import")
  ELSE  IF $GET(DELFILE)=1 DO
  . ;"DO ERR("NO ERROR: Imported schedule OK... (can remove this in TMGSEQL6.m)")
  . IF $$DELFILE^TMGIOUTL(FULLPATHNAME)=0 DO ERR("Error deleting file:")
LODN ;
  QUIT
  ;"
GETMINS(TIME1,TIME2) ;"SUBTRACT TIME2 FROM TIME1 AND RETURN MINS
  IF $L(TIME1)=1 SET TIME1=TIME1_"000" IF $L(TIME2)=1 SET TIME2=TIME2_"000"
  IF $L(TIME1)=2 SET TIME1=TIME1_"00" IF $L(TIME2)=2 SET TIME2=TIME2_"00"
  IF $L(TIME1)=3 SET TIME1=TIME1_"0" IF $L(TIME2)=3 SET TIME2=TIME2_"0"
  NEW MINUTES
  NEW MINS1,MINS2
  SET MINS1=($E(TIME1,1,2)*60)+$E(TIME1,3,4);"+($E(TIME1,5,6)/100)
  SET MINS2=($E(TIME2,1,2)*60)+$E(TIME2,3,4);"+($E(TIME2,5,6)/100)
  SET MINUTES=MINS2-MINS1
  QUIT MINUTES
  ;"
GETLOAD(TMGRESULT)  ;"RPC ENTRY POINT
  ;"Purpose: Returns the list of patients that our currently 
  ;"         checked into SEQUELMED, and have not yet been checked out
  ;"Output: RESULT(IDX)="String to display(e.g."TES,PAT 53 mins")^Mins here^Hint(e.g."Test,Patient is 45 mins past appt time")
  ;"SET TMGRESULT(1)="11111 50 TEST,PERSON 30"
  ;"SET TMGRESULT(2)="22222 20 ANOTHER,PERSON 10"
  NEW TODAY SET TODAY=$$TODAY^TMGDATE
  NEW NOW SET NOW=$$NOW^TMGDATE
  NEW IDX SET IDX=1
  NEW APPTDT SET APPTDT=TODAY_.000001
  NEW SKIPREASONS SET SKIPREASONS="INJ ONLY^PROTIME"
  NEW INCLPROV SET INCLPROV=$$MULTIPLEPROV
  FOR  SET APPTDT=$O(^TMG(22723,"DT",APPTDT)) QUIT:(APPTDT'>0)!(APPTDT'[TODAY)  DO
  . NEW TMGDFN SET TMGDFN=0
  . FOR  SET TMGDFN=$O(^TMG(22723,"DT",APPTDT,TMGDFN)) QUIT:TMGDFN'>0  DO
  . . NEW APPTIEN SET APPTIEN=$O(^TMG(22723,"DT",APPTDT,TMGDFN,0))
  . . NEW STATUS SET STATUS=$G(^TMG(22723,"DT",APPTDT,TMGDFN,APPTIEN))
  . . IF STATUS="C" QUIT
  . . NEW ZN SET ZN=$G(^TMG(22723,TMGDFN,1,APPTIEN,0))
  . . NEW REASON SET REASON=$P(ZN,"^",4)
  . . IF SKIPREASONS[REASON QUIT
  . . NEW NAME SET NAME=$P($G(^DPT(TMGDFN,0)),"^",1)
  . . NEW PROVIDER SET PROVIDER=$P(ZN,"^",3)
  . . IF (DUZ=168)&(PROVIDER'=DUZ) QUIT
  . . ;"Don't do for Dr. Dee anymore IF (DUZ=83)&(PROVIDER'=DUZ) QUIT
  . . SET PROVIDER=$P($G(^VA(200,PROVIDER,0)),"^",1)
  . . SET PROVIDER=$P(PROVIDER,",",2)
  . . SET PROVIDER=$E(PROVIDER,1,1)
  . . NEW TIMEIN,TIMEOUT
  . . SET TIMEIN=$P(ZN,"^",8)
  . . SET TIMEOUT=$P(ZN,"^",9)
  . . IF (TIMEIN'="")&(TIMEOUT="") DO
  . . . ;"NEW SEQLACCT SET SEQLACCT=$P($G(^DPT(TMGDFN,"TMG")),"^",2)
  . . . ;"create hint
  . . . NEW APPTMINS,HINT,PATINFO
  . . . SET APPTMINS=$$GETMINS($P(APPTDT,".",2),NOW)
  . . . IF APPTMINS>0 DO
  . . . . SET HINT=NAME_" is "_APPTMINS_" minutes past appt time."
  . . . ELSE  DO
  . . . . SET HINT=NAME_" is "_APPTMINS_" minutes until appt time."
  . . . ;"create label
  . . . NEW ABBV SET ABBV=$E($P(NAME,",",1),1,3)_","_$E($P(NAME,",",2),1,3)
  . . . NEW TIMEHERE SET TIMEHERE=$$GETMINS($P(TIMEIN,".",2),NOW)
  . . . NEW LABEL SET LABEL=ABBV_" "_TIMEHERE_" mins"
  . . . IF INCLPROV=1 SET LABEL=LABEL_" ("_PROVIDER_")"
  . . . SET PATINFO=TMGDFN_"~"_NAME
  . . . SET TMGRESULT(IDX)=LABEL_"^"_TIMEHERE_"^"_HINT_" (Click to open)^"_PATINFO
  . . . SET IDX=IDX+1
  QUIT
  ;"
MULTIPLEPROV()  ;"
  NEW RESULT SET RESULT=0
  NEW TODAY SET TODAY=$$TODAY^TMGDATE
  NEW NOW SET NOW=$$NOW^TMGDATE
  NEW APPTDT SET APPTDT=TODAY_.000001
  NEW PREVIOUS SET PREVIOUS=0
  FOR  SET APPTDT=$O(^TMG(22723,"DT",APPTDT)) QUIT:(APPTDT'>0)!(APPTDT'[TODAY)  DO
  . NEW TMGDFN SET TMGDFN=0
  . FOR  SET TMGDFN=$O(^TMG(22723,"DT",APPTDT,TMGDFN)) QUIT:TMGDFN'>0  DO
  . . NEW APPTIEN SET APPTIEN=$O(^TMG(22723,"DT",APPTDT,TMGDFN,0))
  . . NEW STATUS SET STATUS=$G(^TMG(22723,"DT",APPTDT,TMGDFN,APPTIEN))
  . . IF STATUS="C" QUIT
  . . NEW ZN SET ZN=$G(^TMG(22723,TMGDFN,1,APPTIEN,0))
  . . NEW PROVIDER SET PROVIDER=$P(ZN,"^",3)
  . . IF PREVIOUS=0 SET PREVIOUS=PROVIDER
  . . IF PREVIOUS'=PROVIDER SET RESULT=1
  QUIT RESULT
  ;"