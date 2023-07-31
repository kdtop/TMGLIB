TMGSEQL6 ;TMG/KST - Parse future appointments file (From SequelPMS); 11/12/14
        ;;1.0;TMG-LIB;**1**; 11/4/14
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
  ;"            }~1 = PROTIME    <-- or KTOPPEN, or MTOPPEN                                                               
  ;"            }~2 = 11/3/2014 00:00:00                                                        
  ;"            }~3 = XXXX, MINNIE                                                              
  ;"            }~4 = 11/3/2014 13:30:00                                                        
  ;"            }~5 = 30                                                                        
  ;"            }~6 = PROTIME                                                                   
  ;"            }~7 = FPG                                                                       
  ;"            }~8 = FPG                                                                       
  ;"            }~9 = 321520  <Ñ SEQL account#                                                                    
  ;"            }~10 = 1540100                                                                  
  ;"            }~11 = 11/31/1911 00:00:00                                                       
  ;"            }~12 = 4231112222                                  
  ;"            }~13 = ""
  ;"            }~14 = FAMILY PHYSICIANS OF GREENEVIL
  ;"            }~15 = FAMILY PHYSICIANS OF GREENEVILLE
  ;"            }~16 = TOPPENBERG
  ;"            }~17 = KEVIN
  ;"            }~18 = 
  ;"            
  ;"            DATA("A")                                                                       
  ;"            }~1 = provider_name                                                             
  ;"            }~2 = appointment_date                                                          
  ;"            }~3 = pat_name                                                                  
  ;"            }~4 = from_time                                                                 
  ;"            }~5 = slot_minutes                                                              
  ;"            }~6 = reason_short_name                                                         
  ;"            }~7 = practice_name                                                             
  ;"            }~8 = location_name                                                             
  ;"            }~9 = account_num  <Ñ SEQL account#                                                              
  ;"            }~10 = reason_seq_num                                                           
  ;"            }~11 = patient_dob                                                              
  ;"            }~12 = pat_tel
  ;"            }~13 = comments
  ;"            }~14 = practice_desc
  ;"            }~15 = location_desc
  ;"            }~16 = prov_last_name
  ;"            }~17 = prov_first_name
  ;"            }~18 = prov_alias
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
  NEW NAME SET NAME=$GET(@REFDATA@(IDX,16))_","_$GET(@REFDATA@(IDX,17))
  NEW SEQLNUM SET SEQLNUM=+$GET(@REFDATA@(IDX,9))
  NEW TMGDFN SET TMGDFN=-1
  IF SEQLNUM>0 SET TMGDFN=+$ORDER(^DPT("TMGS",SEQLNUM,""))
  IF TMGDFN'>0 DO
  . NEW X,Y,DIC SET DIC=2,DIC(0)="M"
  . NEW PTNAME SET PTNAME=$$TRIM^XLFSTR($TRANSLATE($GET(@REFDATA@(IDX,3))," ",""))
  . IF PTNAME="" QUIT
  . SET X=PTNAME DO ^DIC
  . IF +Y>0 SET TMGDFN=+Y QUIT  ;"Found
  . NEW PATIENT 
  . SET PATIENT("NAME")=PTNAME
  . SET PATIENT("SEQUELNUM")=SEQLNUM
  . NEW DOB SET DOB=$$TRIM^XLFSTR($PIECE($GET(@REFDATA@(IDX,11))," ",1))
  . IF DOB="" QUIT
  . SET PATIENT("DOB")=DOB
  . SET TMGDFN=$$GETDFN^TMGGDFN(.PATIENT,0)
  . IF (SEQLNUM>0)&(TMGDFN>0) DO
  . . ;"HERE I COULD STORE MISSING SEQLNUM INTO PATIENT RECORD...
  IF TMGDFN'>0 SET ERR="EMPT|Unable to match patient to VistA database" GOTO P1DN
  SET DIC=200,DIC(0)="M",X=NAME DO ^DIC
  IF +Y'>0 SET ERR="EMPV|Unable to locate provider IEN" GOTO P1DN
  NEW PROVIEN SET PROVIEN=+Y
  NEW APPTDT SET APPTDT=$GET(@REFDATA@(IDX,4))
  NEW DATE,TIME SET DATE=$PIECE(APPTDT," ",1),TIME=$PIECE(APPTDT," ",2)
  SET APPTDT=DATE_"@"_$PIECE(TIME,":",1,2)
  NEW %DT SET %DT="T" SET X=APPTDT DO ^%DT
  IF Y'>0 SET ERR="EDT|Unable to determine appt time" GOTO P1DN
  NEW APPTDT SET APPTDT=Y
  SET @REFDATA@("DT",APPTDT,TMGDFN)=""
  SET @REFDATA@("APPT",TMGDFN,APPTDT,"PROVIEN")=PROVIEN
  ;"SET @REFDATA@("APPT",TMGDFN,APPTDT,"DT")=Y
  SET @REFDATA@("APPT",TMGDFN,APPTDT,"REASON")=$GET(@REFDATA@(IDX,6))
  SET @REFDATA@("APPT",TMGDFN,APPTDT,"MIN")=$GET(@REFDATA@(IDX,5))
  NEW CMT SET CMT=$GET(@REFDATA@(IDX,13)) 
  IF CMT'="" SET @REFDATA@("APPT",TMGDFN,APPTDT,"COMMENT")=CMT
P1DN ;
  IF ERR'="" SET @REFDATA@("ERR",IDX)=ERR
  QUIT
  ;
FILE1(TMGDFN,DT,DATA)  ;"File 1 data entry into TMG SCHEDULE file (22723) 
  ;"Input: TMGDFN -- IEN in PATIENT file
  ;"       DT -- The DateTime of the appt (FM Format)
  ;"       DATA -- Array with input data.  Relevent part shown below 
  ;"          DATA("APPT",TMGDFN,DT
  ;"            ;"REMOVED --> "DT")=Appt start date-time (FM format)
  ;"            "MIN")=minutes duration of appt   
  ;"            "PROVIEN")=IEN of the provider who the appt is with 
  ;"            "REASON")=Short name for reason for appt.        
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
  ;"originally the entry was deleted.
  ;"IF DA'>0 GOTO F1C
  ;"NEW DIK SET DIK="^TMG(22723,"_TMGDFN_",1,",DA(1)=TMGDFN
  ;"DO ^DIK  ;"kill old entry
  IF DA>0 DO  GOTO F1DN
  . ;"UPDATE THE RECORD HERE  11-13-17
  . NEW IENS SET IENS=DA_","_TMGDFN_","
  . NEW MIN SET MIN=$GET(REC("MIN"))
  . IF MIN'="" SET TMGFDA(22723.01,IENS,.02)=MIN
  . NEW PROVIEN SET PROVIEN=+$GET(REC("PROVIEN"))
  . IF PROVIEN>0 SET TMGFDA(22723.01,IENS,.03)=PROVIEN
  . NEW REASON SET REASON=$GET(REC("REASON"))
  . IF REASON'="" SET TMGFDA(22723.01,IENS,.04)=REASON
  . NEW COMMENT SET COMMENT=$GET(REC("COMMENT"))
  . IF COMMENT'="" SET TMGFDA(22723.01,IENS,.06)=COMMENT
  . SET TMGFDA(22723.01,IENS,.07)="A"  ;"ACTIVE status
  . ;"Removed following line because it was NEWING the TMGFDA
  . ;"  and if the appt had been moved to same date/time
  . ;"  but with a different provider, it wasn't being changed
  . ;"  ELH    3/1/19
  . ;"NEW TMGFDA SET TMGFDA(22723.01,IENS,.07)="O"
  . NEW TMGMSG DO FILE^DIE("","TMGFDA","TMGMSG")
  . IF $DATA(TMGMSG("DIERR")) DO
  . . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
F1C ;"Add new subrecord
  SET IENS="+1,"_TMGDFN_"," KILL TMGIEN,TMGFDA
  SET TMGFDA(22723.01,IENS,.01)=DT
  NEW MIN SET MIN=$GET(REC("MIN"))
  IF MIN'="" SET TMGFDA(22723.01,IENS,.02)=MIN
  NEW PROVIEN SET PROVIEN=+$GET(REC("PROVIEN"))
  IF PROVIEN>0 SET TMGFDA(22723.01,IENS,.03)=PROVIEN
  NEW REASON SET REASON=$GET(REC("REASON"))
  IF REASON'="" SET TMGFDA(22723.01,IENS,.04)=REASON
  NEW COMMENT SET COMMENT=$GET(REC("COMMENT"))
  IF COMMENT'="" SET TMGFDA(22723.01,IENS,.06)=COMMENT
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
  IF $GET(REC("COMMENT"))'=$PIECE(ZN,"^",6) GOTO EXSDN
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
  . . . ;"IF STATUS'="A" QUIT
  . . . IF STATUS="C" QUIT
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
  . . . . NEW ERRSTR SET ERRSTR=$GET(@ERR@("ERR",TMGDFN,SUBIEN))
  . . . . IF ERRSTR'="" SET ERRSTR=ERRSTER_" // "
  . . . . SET @ERR@(TMGDFN,SUBIEN)=ERRSTR_$$GETERRST^TMGDEBU2(.TMGMSG)
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
  DO INFRMALT^TMGXQAL(.ALERTRESULT,150,MSG)
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
  IF +TMGRESULT'>0 DO ERR($P(TMGRESULT,"^",2)_" on "_$$FMTE^XLFDT($$NOW^XLFDT)) GOTO LDDN
  DO PARSE("DATA")
  NEW MINDT SET MINDT=+$ORDER(DATA("DT",0))\1
  NEW MAXDT SET MAXDT=(+$ORDER(DATA("DT",""),-1)\1)+(0.999999)
  NEW OBSOLETE DO CHKREMVD(.DATA,MINDT,MAXDT,.OBSOLETE)  
  DO ENSURALL(.DATA)
  ;" MOVED BELOW  1/19/21  DO DOCANCEL(.OBSOLETE,$NAME(DATA("ERR2"))) ;"Set status of obsolete records to CANCELLED  
  DO MARKOLD($$NOW^XLFDT,$NAME(DATA("ERR2"))) ;"Set status of past records to OLD  
  DO DOCANCEL(.OBSOLETE,$NAME(DATA("ERR2"))) ;"Set status of obsolete records to CANCELLED
  IF ($DATA(DATA("ERR")))!($DATA(DATA("ERR2")))!($DATA(DATA("APPT","ERR"))) DO
  . DO ERR("Error(s) during schedule import") 
  ELSE  IF $GET(DELFILE)=1 DO
  . DO ERR("NO ERROR: Imported schedule OK... (can remove this in TMGSEQL6.m)") 
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
