TMGHL7R2 ;TMG/kst-HL7 RPC code ;8/16/15, 8/22/16
              ;;1.0;TMG-LIB;**1**;8/16/15
 ;
 ;"TMG HL7 ENGINE, RPC INTERFACE
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 8/16/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"
 ;"LSTALRTX(TMGRESULT,PARAMS)  --GET LIST OF AVAILABLE ALERTS  -- DEPRECIATED ??
 ;"LSTALRTS(TMGRESULT,PARAMS) --GET LIST OF AVAILABLE ALERTS
 ;"LSTALRT2(OUT) --GET LIST OF AVAILABLE ALERTS, in different output format
 ;"GET1ALRT(TMGRESULT,PARAMS) --GET 1 ALERT INFO  
 ;"DELALRT(TMGRESULT,PARAMS)  --DELETE ALERT
 ;"GETMSG(OUT,DH,JN) --Extract saved TMGMSG, holding HL7 message
 ;"PROCESS(TMGRESULT,PARAMS) --Process message and return results.   
 ;"SRCHRECS(TMGRESULT,PARAMS) --Search records   
 ;"ADDDN(TMGRESULT,PARAMS) --Add new data name for lab storage.
 ;"ADDLAB60(TMGRESULT,PARAMS) --Add a new lab entry in file 60  
 ;"NEXTWKLD(TMGRESULT,PARAMS) --Return next unused workload code after input  
 ;"ADDWKLD(TMGRESULT,PARAMS) --Add new workload code.   
 ;"GET62F61(TMGRESULT,PARAMS)  --GET IEN62 FROM IEN61
 ;"LK60TODN(TMGRESULT,PARAMS)  --GET IEN60 TO DATANAME
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;"CUSTDN(ENTRY)  --Callback function from SRCHRECS, for when searching for laba dataname
 ;"CUSTWKLD(ENTRY)  --Callback function from SRCHRECS, for when searching for Workload 
 ;" 
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"
 ;"=======================================================================
 ;
LSTALRTX(TMGRESULT,PARAMS)  ;"GET LIST OF AVAILABLE ALERTS  -- DEPRECIATED ??
  ;"NOTE: Later I will decide how best to return a list of available alerts.
  ;"  For now, I will just return everything I have data for.
  ;"NOTE: this shows results for stored data, doesn't check actual alerts
  ;"Input: TMGRESULT -- AN OUT PARAMETER.  PASS BY REFERENCE.  Format:
  ;"          TMGRESULT(0)=1^OK, or -1^message
  ;"          TMGRESULT(#)=1^$H^JOBNUM^FMDT  <-- used to get info from ^TMG("TMP","TMGHL73",JOBNUM,$H) array
  ;"       PARAMS -- not used.
  ;"Result: none
  SET TMGRESULT(0)="1^OK"
  NEW IDX SET IDX=1
  NEW DH SET DH=""
  FOR  SET DH=$ORDER(^TMG("TMP","TMGHL73","$H",DH)) QUIT:DH=""  DO
  . NEW FMDT SET FMDT=$$HTFM^XLFDT(DH)
  . NEW JN SET JN=""
  . FOR  SET JN=$ORDER(^TMG("TMP","TMGHL73","$H",DH,JN)) QUIT:+JN'>0  DO
  . . SET TMGRESULT(IDX)="1^"_DH_"^"_JN_"^"_FMDT,IDX=IDX+1
  QUIT
  ;
LSTALRTS(TMGRESULT,PARAMS) ;"GET LIST OF AVAILABLE ALERTS
  ;"Input: TMGRESULT -- AN OUT PARAMETER.  PASS BY REFERENCE.  Format:
  ;"         TMGRESULT(0)=1^OK, or -1^message
  ;"         TMGRESULT(#)=1^$H^JOBNUM^FMDT  <-- used to get info from ^TMG("TMP","TMGHL73",JOBNUM,$H) array
  ;"       PARAMS -- DUZ.  Optional, if not provided, current DUZ is used
  ;"Result: none
  SET TMGRESULT(0)="1^OK"
  NEW OUTIDX SET OUTIDX=1
  NEW ADUZ SET ADUZ=+$PIECE($GET(PARAMS),"^",1) IF ADUZ'>0 SET ADUZ=DUZ
  NEW ERRLABEL SET ERRLABEL=$$ERRLABEL^TMGHL7E()
  NEW ALERTS DO USER^XQALERT("ALERTS",ADUZ)
  NEW IDX SET IDX="" FOR  SET IDX=$ORDER(ALERTS(IDX)) QUIT:(+IDX'>0)  DO
  . NEW STR SET STR=$GET(ALERTS(IDX)) QUIT:(STR'[ERRLABEL)
  . NEW ID SET ID=$PIECE(STR,"^",2) QUIT:$PIECE(ID,";",1)'="TMG-HL7"
  . NEW FMDT SET FMDT=$PIECE(ID,";",3) 
  . NEW XQI,XQX,XQZ,XQAGETAC SET XQAGETAC=1 ;"<-- flag to not actually process alert
  . DO ACTION^XQALERT(ID)  ;"returns info stored as 'XQADATA' in XQZ
  . NEW JN SET JN=+$PIECE(XQZ,"^",1) QUIT:JN'>0
  . NEW DH SET DH=$PIECE(XQZ,"^",2) QUIT:DH=""
  . ;"NEW FMDT SET FMDT=$$HTFM^XLFDT(DH)
  . SET TMGRESULT(OUTIDX)="1^"_DH_"^"_JN_"^"_FMDT,OUTIDX=OUTIDX+1
  QUIT
  ;
LSTALRT2(OUT) ;"GET LIST OF AVAILABLE ALERTS, in different output format
  ;"Input: OUT  -- AN OUT PARAMETER.  PASS BY REFERENCE.  Format:
  ;"         TMGRESULT(#)=AlertID^$H^JOBNUM  
  ;"Result: none
  NEW OUTIDX SET OUTIDX=1
  NEW ALERTS DO USER^XQALERT("ALERTS",DUZ)
  NEW IDX SET IDX="" FOR  SET IDX=$ORDER(ALERTS(IDX)) QUIT:(+IDX'>0)  DO
  . NEW STR SET STR=$GET(ALERTS(IDX)) 
  . NEW ID SET ID=$PIECE(STR,"^",2) QUIT:$PIECE(ID,";",1)'="TMG-HL7"
  . NEW FMDT SET FMDT=$PIECE(ID,";",3) 
  . NEW XQI,XQX,XQZ,XQAGETAC SET XQAGETAC=1 ;"<-- flag to not actually process alert
  . DO ACTION^XQALERT(ID)  ;"returns info stored as 'XQADATA' in XQZ
  . NEW JN SET JN=+$PIECE(XQZ,"^",1) QUIT:JN'>0
  . NEW DH SET DH=$PIECE(XQZ,"^",2) QUIT:DH=""
  . SET OUT(OUTIDX)=ID_"^"_DH_"^"_JN,OUTIDX=OUTIDX+1
  QUIT
  ;  
GET1ALRT(TMGRESULT,PARAMS) ;"GET 1 ALERT INFO  
  ;"Input: TMGRESULT -- AN OUT PARAMETER.  PASS BY REFERENCE.  Format:
  ;"          TMGRESULT(0)=1^OK, or -1^message
  ;"          TMGRESULT(#)="<varname>=<varvalue>"
  ;"       PARAMS -- $H^JOBNUM
  ;"Result: none
  SET TMGRESULT(0)="1^OK"
  NEW IDX SET IDX=1
  SET PARAMS=$GET(PARAMS)
  NEW DH SET DH=$PIECE(PARAMS,"^",1)
  NEW JN SET JN=$PIECE(PARAMS,"^",2)
  IF (DH'[",")!(+JN'>0) DO  GOTO G1ADN
  . SET TMGRESULT(0)="-1^Invalid input parameters.  Got: ["_PARAMS_"]"
  NEW DATA MERGE DATA=^TMG("TMP","TMGHL73",JN,DH)
  KILL DATA("ERROR VARS")
  IF $DATA(DATA("ERROR")) DO
  . SET TMGRESULT(IDX)="POC-ERROR="_$GET(DATA("ERROR"))
  NEW VARS MERGE VARS=^TMG("TMP","TMGHL73",JN,DH,"ERROR VARS")
  NEW JDX SET JDX=0 FOR  SET JDX=$ORDER(VARS(JDX)) QUIT:+JDX'>0  DO
  . NEW LINE SET LINE=$GET(VARS(JDX)) QUIT:LINE=""
  . NEW WANT SET WANT=0
  . SET WANT=WANT!$$LMATCH^TMGSTUT3(LINE,"TMG")
  . SET WANT=WANT!$$LMATCH^TMGSTUT3(LINE,"HL") 
  . SET WANT=WANT!$$LMATCH^TMGSTUT3(LINE,"IEN77")
  . IF $$LMATCH^TMGSTUT3(LINE,"TMGHL7MSG") SET WANT=0
  . IF WANT=0 QUIT
  . SET TMGRESULT(IDX)=LINE,IDX=IDX+1  
G1ADN ;
  QUIT
  ;
DELALRT(TMGRESULT,PARAMS)  ;"DELETE ALERT
  ;"Input: TMGRESULT -- AN OUT PARAMETER.  PASS BY REFERENCE.  Format:
  ;"          TMGRESULT(0)=1^OK, or -1^message
  ;"       PARAMS -- $H^JOBNUM^FMDT
  ;"Result: none
  SET TMGRESULT(0)="1^OK"
  NEW IDX SET IDX=1
  SET PARAMS=$GET(PARAMS)
  NEW DH SET DH=$PIECE(PARAMS,"^",1)
  NEW JN SET JN=$PIECE(PARAMS,"^",2)
  NEW FMDT SET FMDT=$PIECE(PARAMS,"^",3)
  IF (DH'[",")!(+JN'>0) DO  GOTO G1ADN
  . SET TMGRESULT(0)="-1^Invalid input parameters.  Got: ["_PARAMS_"]"
  NEW DATA MERGE DATA=^TMG("TMP","TMGHL73",JN,DH)
  NEW ALERTID SET ALERTID=$GET(DATA("ALERT ID"))
  IF ALERTID="" DO
  . NEW ALERTS DO LSTALRT2(.ALERTS) 
  . NEW IDX SET IDX="" 
  . FOR  SET IDX=$ORDER(ALERTS(IDX)) QUIT:(+IDX'>0)!(ALERTID'="")  DO
  . . NEW STR SET STR=$GET(ALERTS(IDX)) QUIT:(STR="")
  . . NEW ID SET ID=$PIECE(STR,"^",1) 
  . . IF $PIECE(STR,"^",2)'=DH QUIT
  . . IF $PIECE(STR,"^",3)'=JN QUIT
  . . SET ALERTID=ID
  IF ALERTID'="" DO
  . NEW XQAID SET XQAID=$PIECE(ALERTID,"^",3)
  . NEW XQAKILL SET XQAKILL=0
  . DO DELETE^XQALERT
  . KILL ^TMG("TMP","TMGHL73",JN,DH)  
  ELSE  DO
  . SET TMGRESULT(0)="-1^Unable to find alert to delete"
  QUIT
  ;
GETMSG(OUT,DH,JN) ;"Extract saved TMGMSG, holding HL7 message
  ;"Input: OUT -- AN OUT PARAMETER.  PASS BY REFERENCE.  Format:
  ;"          TMGRESULT(#)=<LINE DATA>
  ;"       DH -- TIME IN $H FORMAT of stored message. 
  ;"       JN -- JOB NUM of stored message
  ;"Result: none
  NEW IDX SET IDX=1
  SET DH=$GET(DH)
  SET JN=$GET(JN)
  IF (DH'[",")!(+JN'>0) DO  GOTO GMDN
  . ;"SET TMGRESULT(0)="-1^Invalid input parameters.  Got: [DH="_DH_" AND JN="_JN_"]"
  NEW DATA MERGE DATA=^TMG("TMP","TMGHL73",JN,DH)
  NEW VARS MERGE VARS=^TMG("TMP","TMGHL73",JN,DH,"ERROR VARS")
  NEW TMGMSG
  NEW JDX SET JDX=0 FOR  SET JDX=$ORDER(VARS(JDX)) QUIT:+JDX'>0  DO
  . NEW LINE SET LINE=$GET(VARS(JDX)) QUIT:LINE=""
  . IF '$$LMATCH^TMGSTUT3(LINE,"TMGMSG(") QUIT
  . SET LINE="SET "_LINE
  . XECUTE LINE
  MERGE OUT=TMGMSG
GMDN ;
  QUIT
  ;
PROCESS(TMGRESULT,PARAMS) ;"Process message and return results.   
  ;"Input: TMGRESULT -- AN OUT PARAMETER.  PASS BY REFERENCE.  Format:
  ;"          TMGRESULT(0)=1^OK, or -1^message
  ;"          TMGRESULT(#)=<message from server code to GUI client>
  ;"       PARAMS -- $H^JOBNUM^Mode
  ;"          $H -- TIME of stored message. 
  ;"          JOBNUM -- # of stored message
  ;"          Mode -- 1 = Parse and transform only, don't file.
  ;"          PARAMS("GUI","MSG",#)=  
  ;"          PARAMS("GUI","MSG",#,"REPLY")=  
  ;"Result: none
  NEW TMGZZ SET TMGZZ=0
  IF TMGZZ=1 DO
  . KILL PARAMS
  . SET PARAMS=$GET(^TMG("TMP","PROCESS^TMGHL7R2","PARAMS"))
  ELSE  DO
  . KILL ^TMG("TMP","PROCESS^TMGHL7R2")
  . MERGE ^TMG("TMP","PROCESS^TMGHL7R2","PARAMS")=PARAMS
  SET TMGRESULT(0)="1^OK"
  SET PARAMS=$GET(PARAMS)
  NEW DH SET DH=$PIECE(PARAMS,"^",1)
  NEW JN SET JN=$PIECE(PARAMS,"^",2)
  NEW MODE SET MODE=$PIECE(PARAMS,"^",3)  
  IF (DH'[",")!(+JN'>0)!(+MODE'=MODE) DO  GOTO PRCSDN
  . SET TMGRESULT(0)="-1^Invalid input parameters.  Got: ["_PARAMS_"]"
  NEW TMGMSG DO GETMSG(.TMGMSG,DH,JN)
  NEW OPTION SET OPTION("GUI")=1
  MERGE OPTION("GUI","MSG")=PARAMS("GUI","MSG")
  IF MODE=1 SET OPTION("GUI","NO FILE")=1
  NEW TMGHL7MSG,TMGENV
  ;"NEW TEMP SET TEMP=$$HL7PARSE^TMGHL72(.TMGHL7MSG,.TMGENV,.TMGMSG,.OPTION)
  NEW TEMP SET TEMP=$$HL7PARSE^TMGHL71(.TMGHL7MSG,.TMGENV,.TMGMSG,.OPTION)
  IF +TEMP<0 SET TMGRESULT(0)=TEMP
  NEW JDX SET JDX=1
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(OPTION("GUI","MSG",IDX)) QUIT:+IDX'>0  DO
  . NEW MSG SET MSG=OPTION("GUI","MSG",IDX) QUIT:MSG=""
  . SET TMGRESULT(JDX)=MSG,JDX=JDX+1
  . NEW VARNAME SET VARNAME=""
  . FOR  SET VARNAME=$ORDER(OPTION("GUI","MSG",IDX,"VARS",VARNAME)) QUIT:VARNAME=""  DO
  . . IF VARNAME="VARNAME" QUIT
  . . NEW PROCESSTEMPARR
  . . NEW @VARNAME MERGE @VARNAME=OPTION("GUI","MSG",IDX,"VARS",VARNAME)
  . . DO ZWR2ARR^TMGZWR(VARNAME,"PROCESSTEMPARR")
  . . NEW KDX SET KDX=0
  . . FOR  SET KDX=$ORDER(PROCESSTEMPARR(KDX)) QUIT:+KDX'>0  DO
  . . . SET MSG="VAR:"_$GET(PROCESSTEMPARR(KDX))
  . . . SET TMGRESULT(JDX)=MSG,JDX=JDX+1
PRCSDN  ;
  QUIT
  ;
SRCHRECS(TMGRESULT,PARAMS) ;"Search records   
  ;"Input: TMGRESULT -- AN OUT PARAMETER.  PASS BY REFERENCE.  Format:
  ;"          TMGRESULT(0)=1^OK, or -1^message
  ;"          TMGRESULT(#)=IEN^FileNum^.01Name
  ;"       PARAMS -- FileNum^SearchTerms^CustomizerFn
  ;"                    SearchTerms <-- a list of terms to search for, separated by spaces
  ;"                    CustomizerFn -- e.g. '$$MYFN^MYRTN
  ;"                       The signature of the function must take 1 parameter: IEN^FileNum^.01Name
  ;"                       and must return value in format of IEN^FileNum^<Display string>
  ;"                       NOTE: must start with '$$' and NOT include parentheses, '()'
  ;"NOTE: search is case insensitive
  ;"Result: none
  KILL TMGRESULT
  SET TMGRESULT(0)="1^OK"
  SET PARAMS=$GET(PARAMS)
  NEW FNUM SET FNUM=$PIECE(PARAMS,"^",1)
  IF (+FNUM'=FNUM) DO  GOTO SRDN
  . SET TMGRESULT(0)="-1^Invalid file number.  Got: '"_FNUM_"'"
  NEW TERMS SET TERMS=$$UP^XLFSTR($$TRIM^XLFSTR($PIECE(PARAMS,"^",2)))
  NEW CUSTFN SET CUSTFN=$PIECE(PARAMS,"^",3,99) IF CUSTFN'="" DO
  . SET CUSTFN=$PIECE(CUSTFN,"(",1)
  . IF $EXTRACT(CUSTFN,1,2)'="$$" SET CUSTFN=""
  IF TERMS="" GOTO SRDN
  NEW REF SET REF=$GET(^DIC(FNUM,0,"GL"))
  IF REF="",(FNUM=63.04) DO
  . SET REF=$NAME(^DD(FNUM))
  . IF $DATA(@REF)=0 SET REF=""
  ELSE  SET REF=$$CREF^DILF(REF)
  IF REF="" DO  GOTO SRDN
  . SET TMGRESULT(0)="-1^Invalid file number.  Got: '"_FNUM_"'. No entry in ^DIC() for this."
  NEW IDX FOR IDX=1:1:$LENGTH(TERMS," ") DO
  . NEW ATERM SET ATERM=$PIECE(TERMS," ",IDX) QUIT:ATERM=""
  . SET TERMS(ATERM)=""
  NEW CT SET CT=1
  NEW NAME SET NAME=""
  FOR  SET NAME=$ORDER(@REF@("B",NAME)) QUIT:NAME=""  DO
  . NEW IEN SET IEN=0
  . NEW FAIL SET FAIL=0
  . NEW UPNAME SET UPNAME=$$UP^XLFSTR(NAME)
  . NEW ATERM SET ATERM=""
  . FOR  SET ATERM=$ORDER(TERMS(ATERM)) QUIT:(ATERM="")!FAIL  DO
  . . SET FAIL=(UPNAME'[ATERM)
  . IF FAIL QUIT
  . FOR  SET IEN=$ORDER(@REF@("B",NAME,IEN)) QUIT:+IEN'>0  DO
  . . NEW STR SET STR=IEN_"^"_FNUM_"^"_NAME
  . . IF CUSTFN'="" DO
  . . . NEW CODE,TEMP SET CODE="SET TEMP="_CUSTFN_"("""_STR_""")"
  . . . NEW $ETRAP SET $ETRAP="SET STR=STR_"" Error calling '""_CODE_""' SET $ETRAP="""",$ECODE="""""
  . . . XECUTE CODE
  . . . SET STR=TEMP
  . . SET TMGRESULT(CT)=STR,CT=CT+1  
SRDN ;  
  QUIT
  ;
ADDDN(TMGRESULT,PARAMS) ;"Add new data name for lab storage.
  ;"Input: TMGRESULT -- AN OUT PARAMETER.  PASS BY REFERENCE.  Format:
  ;"          TMGRESULT(0)=1^OK, or -1^message
  ;"          TMGRESULT(1)=NewlyAddedFieldNumber^Name
  ;"       PARAMS -- name_of_new_data_name
  NEW TMGZZ SET TMGZZ=0
  IF TMGZZ=1 DO
  . KILL PARAMS
  . SET PARAMS=$GET(^TMG("TMP","ADDDN^TMGHL7R2","PARAMS"))
  ELSE  DO
  . KILL ^TMG("TMP","ADDDN^TMGHL7R2")
  . MERGE ^TMG("TMP","ADDDN^TMGHL7R2","PARAMS")=PARAMS
  SET TMGRESULT(0)="1^OK"
  NEW NAME SET NAME=$GET(PARAMS)
  IF NAME="" DO  GOTO ADNDN
  . SET TMGRESULT(0)="-1^No data name given to auto-add." 
  NEW TEMP SET TEMP=$$AUTOADDN^TMGHL70C(NAME)
  IF +TEMP<0 SET TMGRESULT(0)=TEMP GOTO ADNDN
  IF TEMP=0 SET TMGRESULT(0)="-1^Nothing added"
  SET TMGRESULT(1)=TEMP  
ADNDN ;
  QUIT
  ;
CUSTDN(ENTRY)  ;"Callback function from SRCHRECS, for when searching for laba dataname
  ;"Input: ENTRY -- format = IEN^FileNum^.01Name
  ;"Result: returns value in format of IEN^FileNum^<Display string>
  NEW TMGRESULT SET TMGRESULT=ENTRY
  NEW FLD SET FLD="CH;"_$PIECE(ENTRY,"^",1)_";1"
  NEW LABS SET LABS=""
  NEW IEN SET IEN=0
  FOR  SET IEN=$ORDER(^LAB(60,"C",FLD,IEN)) QUIT:(+IEN'>0)  DO
  . NEW ZN SET ZN=$GET(^LAB(60,IEN,0)) QUIT:ZN=""
  . IF LABS'="" SET LABS=LABS_", "
  . SET LABS=LABS_$PIECE(ZN,"^",1)
  IF LABS'="" DO
  . SET TMGRESULT=TMGRESULT_" (storage for lab test"
  . IF LABS["," SET TMGRESULT=TMGRESULT_"s"
  . SET TMGRESULT=TMGRESULT_": "_LABS_")"
  QUIT TMGRESULT
  ;
CUSTWKLD(ENTRY)  ;"Callback function from SRCHRECS, for when searching for Workload 
  ;"Input: ENTRY -- format = IEN^FileNum^.01Name
  ;"Result: returns value in format of IEN^FileNum^<Display string>^WorkLoadCode
  SET ENTRY=$GET(ENTRY)
  NEW ZN SET ZN=$GET(^LAM(+ENTRY,0))
  NEW WKLD SET WKLD=$PIECE(ZN,"^",2)
  SET TMGRESULT=ENTRY_" ("_WKLD_")^"_WKLD  
  QUIT TMGRESULT
  ;  
ADDLAB60(TMGRESULT,PARAMS) ;"Add a new lab entry in file 60  
  ;"Input: TMGRESULT -- AN OUT PARAMETER.  PASS BY REFERENCE.  Format:
  ;"          TMGRESULT(0)=1^OK, or -1^message
  ;"          TMGRESULT(1)=NewlyAddedIEN60^Name
  ;"       PARAMS -- AlertHandle^TestName^PrintName^StorageLocFld63d04
  ;"          AlertHandle = $H;JOBNUM
  ;"          TestName = Test name (up to 30 chars)
  ;"          PrintName = Shortened name (up to 7 chars)
  ;"          StorageLocFld63d04 = Field in subfile 63.04 for storage
  NEW TMGZZ SET TMGZZ=0
  IF TMGZZ=1 DO
  . KILL PARAMS
  . SET PARAMS=$GET(^TMG("TMP","ADDLAB60^TMGHL7R2","PARAMS"))
  ELSE  DO
  . KILL ^TMG("TMP","ADDLAB60^TMGHL7R2")
  . MERGE ^TMG("TMP","ADDLAB60^TMGHL7R2","PARAMS")=PARAMS
  SET TMGRESULT(0)="1^OK"
  ;"SET TMGRESULT(1)="1^TEST!"   ;"<-- TEMP!!!  
  ;"goto AL6DN  ;"<-- TEMP!!!
  SET PARAMS=$GET(PARAMS)
  NEW HANDLE SET HANDLE=$PIECE(PARAMS,"^",1)
  NEW TESTNAME SET TESTNAME=$PIECE(PARAMS,"^",2)
  NEW PRINTNAME SET PRINTNAME=$PIECE(PARAMS,"^",3)
  NEW LOC63D04 SET LOC63D04=$PIECE(PARAMS,"^",4)
  NEW DH SET DH=$PIECE(HANDLE,";",1)
  NEW JN SET JN=$PIECE(HANDLE,";",2)
  IF (DH'[",")!(+JN'>0) DO  GOTO AL6DN
  . SET TMGRESULT(0)="-1^Invalid input parameters.  Got: ["_PARAMS_"]"
  NEW TMGMSG DO GETMSG(.TMGMSG,DH,JN)
  NEW TEMP,TMGENV SET TEMP=$$SETUPENV^TMGHL7U(.TMGMSG,.TMGENV,0)
  IF TEMP<0 DO  GOTO AL6DN
  . SET TMGRESULT(0)=TEMP
  SET TEMP=$$ADD60^TMGHL70C(TESTNAME,PRINTNAME,LOC63D04,.TMGENV)
  IF TEMP<0 SET TMGRESULT(0)=TEMP GOTO AL6DN
  IF TEMP=0 SET TMGRESULT(0)="-1^Nothing added" GOTO AL6DN
  IF TEMP>0 SET TMGRESULT(1)=TEMP
AL6DN  ;
  QUIT
  ;
NEXTWKLD(TMGRESULT,PARAMS) ;"Return next unused workload code after input  
  ;"Input: TMGRESULT -- AN OUT PARAMETER.  PASS BY REFERENCE.  Format:
  ;"          TMGRESULT(0)=1^OK, or -1^message
  ;"          TMGRESULT(1)=NextAvailableWorkLoadCode
  ;"       PARAMS -- ExistingWorkLoadCode
  SET TMGRESULT(0)="1^OK"
  NEW WKLDCODE SET WKLDCODE=$GET(PARAMS)
  NEW INTPART SET INTPART=$PIECE(WKLDCODE,".",1)
  NEW DECPART SET DECPART=$PIECE(WKLDCODE,".",2)
  IF (INTPART="")!(DECPART="") DO  GOTO NWKDN
  . SET TMGRESULT(0)="-1^Invalid work load code passed.  Got: '"_WKLDCODE_"'"
  NEW DONE SET DONE=0
  FOR  DO  QUIT:DONE
  . SET DECPART=DECPART+1
  . SET DECPART=$$RJ^XLFSTR(DECPART,"4","0")
  . SET WKLDCODE=INTPART_"."_DECPART
  . SET DONE=($DATA(^LAM("C",WKLDCODE_" "))=0)
  SET TMGRESULT(1)=WKLDCODE
NWKDN ;
  QUIT
  ;
ADDWKLD(TMGRESULT,PARAMS) ;"Add new workload code.   
  ;"Input: TMGRESULT -- AN OUT PARAMETER.  PASS BY REFERENCE.  Format:
  ;"          TMGRESULT(0)=1^OK, or -1^message
  ;"          TMGRESULT(1)=IEN^WorkloadName^WorkloadCode
  ;"       PARAMS -- WorkloadName^WorkloadCode
  SET TMGRESULT(0)="1^OK"
  SET PARAMS=$GET(PARAMS)
  NEW NAME SET NAME=$PIECE(PARAMS,"^",1)
  NEW CODE SET CODE=$PIECE(PARAMS,"^",2)
  NEW TEMP SET TEMP=$$NEWWKLD^TMGHL70C(NAME,CODE)
  IF TEMP<0 SET TMGRESULT(0)=TEMP
  ELSE  SET TMGRESULT(1)=TEMP
  QUIT
  ;
GET62F61(TMGRESULT,PARAMS)  ;"GET IEN62 FROM IEN61
  ;"Input: TMGRESULT -- AN OUT PARAMETER.  PASS BY REFERENCE.  Format:
  ;"          TMGRESULT(0)=1^OK, or -1^message
  ;"          TMGRESULT(1)=IEN62  or 0 if not found
  ;"       PARAMS -- IEN61
  SET TMGRESULT(0)="1^OK"
  NEW TEMP SET TEMP=$$GET62F61^TMGHL70C(+$GET(PARAMS))
  IF TEMP<0 SET TMGRESULT(0)=TEMP
  ELSE  SET TMGRESULT(1)=TEMP
  QUIT
  ;
LK60TODN(TMGRESULT,PARAMS)  ;"GET IEN60 TO DATANAME
  ;"Input: TMGRESULT -- AN OUT PARAMETER.  PASS BY REFERENCE.  Format:
  ;"          TMGRESULT(0)=1^OK, or -1^message
  ;"       PARAMS -- AlertHandle^IEN60^Loc63D04
  ;"          AlertHandle = $H;JOBNUM
  SET TMGRESULT(0)="1^OK"
  SET PARAMS=$GET(PARAMS)
  NEW HANDLE SET HANDLE=$PIECE(PARAMS,"^",1)
  NEW DH SET DH=$PIECE(HANDLE,";",1)
  NEW JN SET JN=$PIECE(HANDLE,";",2)
  IF (DH'[",")!(+JN'>0) DO  GOTO LK6DN
  . SET TMGRESULT(0)="-1^Invalid input parameters.  Got: ["_PARAMS_"]"
  NEW TMGMSG DO GETMSG(.TMGMSG,DH,JN)
  NEW TEMP,TMGENV SET TEMP=$$SETUPENV^TMGHL7U(.TMGMSG,.TMGENV,0)
  IF TEMP<0 DO  GOTO LK6DN
  . SET TMGRESULT(0)=TEMP
  NEW IEN60 SET IEN60=$PIECE(PARAMS,"^",2)
  IF IEN60'>0 DO  GOTO LK6DN
  . SET TMGRESULT(0)="-1^Invalid IEN60.  Got '"_IEN60_"'"
  NEW FLD63D04 SET FLD63D04=$PIECE(PARAMS,"^",3)
  IF FLD63D04'>0 DO  GOTO LK6DN
  . SET TMGRESULT(0)="-1^Invalid Data name.  Got '"_FLD63D04_"'"
  SET TMGRESULT(0)=$$STOREDN^TMGHL70C(.TMGENV,.IEN60,FLD63D04) ;"STORE DATANAME for IEN60
  IF TMGRESULT(0)="1" SET TMGRESULT(0)="1^OK"  
LK6DN ;
  QUIT
  ;
