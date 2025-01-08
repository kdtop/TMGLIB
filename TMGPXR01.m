TMGPXR01 ;TMG/kst/TMG reminder stuff ;7/26/12, 2/2/14, 3/24/21
         ;;1.0;TMG-LIB;**1**;7/26/12
 ;
 ;"TMG REMINDER FUNCTIONS
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
 ;" API -- Public Functions
 ;"=======================================================================
 ;"DOB(TMGDFN,TEST,DATE,DATA,TEXT) -- Return information about the date of birth for the patient
 ;"ACTIVEPT(TMGDFN,TEST,DATE,DATA,TEXT) -- Return IF patient is an active patient (recent activity)
 ;"DTEVAL(TMGDFN,TEST,DATE,DATA,TEXT) --Return the effective date that the reminder is being run.
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"PARSEDT(DT,YEAR,MONTH,DAY,TIME) --PARSE date to component parts
 ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"
 ;"=======================================================================
DOB(TMGDFN,TEST,DATE,DATA,TEXT) ;
        ;"Purpose: Return information about the date of birth for the patient
        ;"Input: TMGDFN -- the patient IEN
        ;"       TEST -- AN OUT PARAMETER.  The logical value of the test: 1=true, 0=false
        ;"               Also an IN PARAMETER.  Any value for COMPUTED FINDING PARAMETER will be passed in here.
        ;"       DATE -- AN OUT PARAMETER.  Date of finding.
        ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.  See output format below
        ;"       TEXT -- Text to be display in the Clinical Maintenance output.  Optional.
        ;"Results: none
        ;"Output: DATE -- returns patient DOB (FM format).  NOTE: changed 6/12/13.  Used to return NOW date. 
        ;"        DATA filled as follows:
        ;"          DATA = Date of birth (internal Fileman format)
        ;"          DATA("YEAR") = year of birth  (4 digits)
        ;"          DATA("MONTH")= month of birth (2 digits)
        ;"          DATA("DAY")= day of birth (2 digits)
        ;"        TEST -- The logical value of the test: 1=true, 0=false
        NEW DOB SET DOB=$PIECE($GET(^DPT(TMGDFN,0)),"^",3)\1
        NEW YEAR,MONTH,DAY
        DO PARSEDT(DOB,.YEAR,.MONTH,.DAY)
        SET DATA=DOB
        SET DATA("YEAR")=YEAR
        SET DATA("MONTH")=MONTH
        SET DATA("DAY")=DAY
        NEW %,%H,%I,X
        ;"DO NOW^%DTC
        ;"SET DATE=X
        SET DATE=DOB  ;"//kt 6/12/13
        SET TEXT="Patient's date of birth"
        SET TEST=1  ;"all patients have DOB, so always TRUE
        QUIT
        ;
PARSEDT(DT,YEAR,MONTH,DAY,TIME) ;"PARSE date to component parts
        ;"Input: DT -- FMDate
        ;"       YEAR,MONTH,DAY,TIME -- OUT PARAMETERS
        SET YEAR=+$EXTRACT(DT,1,3)
        SET YEAR=YEAR+1700
        SET MONTH=+$EXTRACT(DT,4,5)
        SET MONTH=$$RJ^XLFSTR(MONTH,2,"0")
        SET DAY=+$EXTRACT(DT,6,7)
        SET DAY=$$RJ^XLFSTR(DAY,2,"0")
        SET TIME=+$PIECE(DT,".",2)
        QUIT
        ;
DTEVAL(TMGDFN,TEST,DATE,DATA,TEXT) ;"DATE OF EVALUATION
        ;"Purpose: Return a "NOW" date as a "finding".  This is the effective date 
        ;"          that the reminder is being run.
        ;"Input: See discussion above for details. 
        ;"Output: DATA filled as follows:
        ;"          DATA = Date of birth (internal Fileman format)
        ;"          DATA("YEAR")= year of effective date
        ;"          DATA("MONTH"= month of effective date
        ;"          DATA("DAY")= day (2 digits) of effective date
        ;"          DATA("TIME")=time of effective date
        ;"          DATA("DOY")= day of year (1-365)
        ;"Results: none
        SET DATE=$GET(PXRMDATE)
        IF DATE="" SET DATE=$$NOW^XLFDT
        SET TEST=1  ;"all evaluations have an effective date, so always TRUE
        NEW YEAR,MONTH,DAY,TIME
        DO PARSEDT(DATE,.YEAR,.MONTH,.DAY,.TIME)
        SET DATA=DATE  ;"NOTE: Can't have just subscripts.  Must have DATA= to something.
        SET DATA("YEAR")=YEAR
        SET DATA("MONTH")=MONTH
        SET DATA("DAY")=DAY
        SET DATA("TIME")=TIME
        NEW NEWYEARDT SET NEWYEARDT=(YEAR-1700)_"0101"
        NEW DIFF SET DIFF=+$$FMDIFF^XLFDT(DATE,NEWYEARDT,1)
        SET DATA("DOY")=DIFF+1
        QUIT
        ;        
ACTIVEPT(TMGDFN,TEST,DATE,DATA,TEXT) ;"ACTIVE PATIENT   **NAME ALERT** --There is also an ACTIVEPT^TMGPXR03() for a different purpose.
        ;"NOTE: This version is designed to work with the clinical reminder system.  
        ;"Input: TMGDFN -- the patient IEN
        ;"       TEST -- AN OUT PARAMETER.  The logical value of the test: 1=true, 0=false
        ;"               Also an IN PARAMETER.  Any value for COMPUTED FINDING PARAMETER (Field #26) will be passed in here.
        ;"       DATE -- AN OUT PARAMETER.  Date of finding.
        ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.  See output format below
        ;"       TEXT -- Text to be display in the Clinical Maintenance output.  Optional.
        ;"Results: none
        ;"Output: None here, but see DOB() above for format IF needed. 
        ;"NOTE: A patient will be considered to be active if they have ANY notes
        ;"      TIU DOCUMENT (#8925) in range of DATE-Xyrs to DATE
        ;"      Will SET X to be 3 years
        ;"NOTE: this does NOT give the date of the most recent note.  Just the date
        ;"      of the first note *encountered* in the desired date range.   
        NEW DIFFYR SET DIFFYR=3  ;"<--- CHANGE THIS TO CHANGE TIME BEFORE PATIENT CONSIDERED INACTIVE
        SET DATE=$GET(DATE) IF +DATE'>0 SET DATE=$$NOW^XLFDT
        NEW SDTIME SET SDTIME=$$FMADD^XLFDT(DATE,-365*DIFFYR,0,0,0)
        NEW REVDATE SET REVDATE=9999999-DATE
        NEW REVSDT SET REVSDT=9999999-SDTIME
        NEW DOCIEN SET DOCIEN=0        
        NEW TITLEIEN SET TITLEIEN=0
        NEW FOUND SET FOUND=0
        FOR  SET TITLEIEN=$ORDER(^TIU(8925,"APT",TMGDFN,TITLEIEN)) QUIT:(+TITLEIEN'>0)!(FOUND>0)  DO
        . NEW STATUSIEN SET STATUSIEN=0
        . FOR  SET STATUSIEN=$ORDER(^TIU(8925,"APT",TMGDFN,TITLEIEN,STATUSIEN)) QUIT:(+STATUSIEN'>0)!(FOUND>0)  DO
        . . NEW PREVDT SET PREVDT=$ORDER(^TIU(8925,"APT",TMGDFN,TITLEIEN,STATUSIEN,REVDATE))
        . . IF PREVDT'>REVSDT DO
        . . . SET FOUND=PREVDT
        . . . SET DOCIEN=$ORDER(^TIU(8925,"APT",TMGDFN,TITLEIEN,STATUSIEN,PREVDT,0))
        SET TEST=(FOUND>0)
        IF TEST DO
        . NEW DATE SET DATE=9999999-FOUND
        . SET TEXT="Active patient because found note (`"_DOCIEN_") dated "_$$FMTE^XLFDT(DATE,"2D")_", which is in past "_DIFFYR_" yrs."
        ELSE  DO
        . SET TEXT="NOT an active patient because note found between "
        . SET TEXT=TEXT_$$FMTE^XLFDT(SDTIME,"2D")_" and "_$$FMTE^XLFDT(DATE,"2D")
        QUIT
        ;        
PAINMEDS(TMGDFN,TEST,DATE,DATA,TEXT) ;
        ;"Purpose: Determine if patient is on pain medication
        ;"Input: TMGDFN -- the patient IEN
        ;"       TEST -- AN OUT PARAMETER.  The logical value of the test:
        ;                1=true, 0=false
        ;"               Also an IN PARAMETER.  Any value for COMPUTED FINDING PARAMETER will be passed in here.
        ;"       DATE -- AN OUT PARAMETER.  Date of finding.
        ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
        ;"       TEXT -- Text to be display in the Clinical Maintenance
        ;"Output.  Optional.
        ;"Results: none
        SET TEST=0
        SET DATE=0
        SET WHY=""
        NEW TMGRESULT,MEDARR SET TMGRESULT=$$ONCSDBRX^TMGC0QT4(TMGDFN,$$TODAY^TMGDATE,.MEDARR)
        IF $D(MEDARR) DO
        . SET TEST=1
        . SET DATE=$$TODAY^TMGDATE       
        . SET WHY="REMINDER DUE BECAUSE PATIENT IS ON: "_$C(13,10)
        . NEW MEDNAME SET MEDNAME="" 
        . FOR  SET MEDNAME=$ORDER(MEDARR(MEDNAME)) QUIT:MEDNAME=""  DO
        . . SET WHY=WHY_MEDNAME_$C(13,10)
        QUIT WHY
        ;"Old method below, in case we need to revert  3/26/19
        NEW TMGMEDLIST,TMGMEDARRAY
        NEW DBTAG SET DBTAG="*CSM-DATABASE REVIEW"
        DO MEDLIST^TMGTIUOJ(.TMGMEDLIST,.TMGDFN,.TMGMEDARRAY)
        ;"ELH use old method for now, until Medication file is properly
        ;"loaded with dates    5/22/18
        ;"DO MEDARR^TMGTIUOJ(.TMGMEDLIST,.TMGDFN,.TMGMEDARRAY)  ;"//kt 5/7/18
        IF $DATA(TMGMEDARRAY) DO
        . NEW DBDATE SET DBDATE=$GET(TMGMEDARRAY("KEY-VALUE",DBTAG))
        . SET DBDATE=$$TRIM^XLFSTR(DBDATE)  ;"//KT 5/7/18
        . SET DBDATE=$PIECE(DBDATE," ",1)
        . IF $$UP^XLFSTR(DBDATE)="NO" SET DBDATE=""
        . NEW %DT,X,Y
        . SET %DT=""
        . SET X=DBDATE
        . DO ^%DT
        . IF Y>0 DO
        . . SET TEST=1
        . . SET DATE=Y
PMDN    QUIT
        ;"
PAINICD(TMGDFN,TEST,DATE,DATA,TEXT) ;
        ;"Purpose: Determine if patient needs pain med ICD
        ;" NOTE: THIS DOES NOT DETERMINE IF THE PATIENT IS ACTUALLY
        ;"       ON PAIN MEDS. IT SHOULD BE USED IN CONJUNCTION WITH
        ;"       THE TMG CONTROLLED RX USER CF
        ;"Input: TMGDFN -- the patient IEN
        ;"       TEST -- AN OUT PARAMETER.  The logical value of the test:
        ;                1=true, 0=false
        ;"               Also an IN PARAMETER.  Any value for COMPUTED FINDING PARAMETER will be passed in here.
        ;"       DATE -- AN OUT PARAMETER.  Date of finding.
        ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
        ;"       TEXT -- Text to be display in the Clinical Maintenance
        ;"Output.  Optional.
        ;"Results: none  
        SET TEST=0
        SET DATE=0 
        NEW ICDMSG SET ICDMSG=$$PAINICD^TMGRPT1(TMGDFN)
        IF ICDMSG["NEEDS DATA ENTERED" DO
        . SET TEST=1
        . SET DATE=$$TODAY^TMGDATE
        QUIT
        ;"
PTONASA(TMGDFN,TEST,DATE,DATA,TEXT)  ;"
        ;"Purpose: To determine if the patient is on ASA
        ;"Input: same as above
        SET TEST=0
        SET DATE=0
        NEW TMGMEDLIST,TMGMEDARRAY
        ;"//kt 5/7/18 DO MEDLIST^TMGTIUOJ(.TMGMEDLIST,.TMGDFN,.TMGMEDARRAY)
        DO MEDARR^TMGTIUOJ(.TMGMEDLIST,.TMGDFN,.TMGMEDARRAY)  ;"//kt 5/7/18
        IF $DATA(TMGMEDARRAY) DO
        . NEW IDX SET IDX=0
        . FOR  SET IDX=$ORDER(TMGMEDARRAY(IDX)) QUIT:IDX'>0  DO
        . . IF ($$UP^XLFSTR($GET(TMGMEDARRAY(IDX)))["ASA ")!($$UP^XLFSTR($GET(TMGMEDARRAY(IDX)))["ASPIRIN") DO
        . . . DO NOW^%DTC
        . . . SET TEST=1
        . . . SET DATE=$$TODAY^TMGDATE
        QUIT        
        ;
PTONTEST(TMGDFN,TEST,DATE,DATA,TEXT)  ;"
        ;"Purpose: To determine if the patient is on TESTOSTERONE
        ;"Input: same as above
        SET TEST=0
        SET DATE=0
        NEW TMGMEDLIST,TMGMEDARRAY
        NEW TBLIEN SET TBLIEN=$O(^TMG(22708,"C","TESTOSTERONE",0))
        ;"//kt 5/7/18 DO MEDLIST^TMGTIUOJ(.TMGMEDLIST,.TMGDFN,.TMGMEDARRAY)   
        DO MEDARR^TMGTIUOJ(.TMGMEDLIST,.TMGDFN,.TMGMEDARRAY)  ;"//kt 5/7/18
        IF $DATA(TMGMEDARRAY) DO
        . DO CHECKTBLMEDS(.TMGMEDARRAY,.TBLIEN,.TEST,.DATE)
        QUIT
        ;
CHECKTBLMEDS(TMGMEDARRAY,TBLIEN,TEST,DATE)
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(TMGMEDARRAY(IDX)) QUIT:IDX'>0  DO
        . NEW MEDNAME SET MEDNAME=""
        . FOR  SET MEDNAME=$ORDER(^TMG(22708,38,3,"B",MEDNAME)) QUIT:MEDNAME=""  DO
        . . IF $$UP^XLFSTR($GET(TMGMEDARRAY(IDX)))[$$UP^XLFSTR(MEDNAME) DO
        . . . DO NOW^%DTC
        . . . SET TEST=1
        . . . SET DATE=$$TODAY^TMGDATE
        QUIT
        ;"
COPDINC(TMGDFN,TEST,DATE,DATA,TEXT)  ;"
        ;"Purpose: To determine if the patient should be included in
        ;"         the COPD cohort based on insurance
        SET TEST=0
        SET DATE=0
        NEW INS2INC
        SET INS2INC=",3,5,6,13,17,36,15," ;"IENS OF ACCEPTABLE INS IN #36. USING LEADING AND TRAILING COMMAS FOR EASE OF MATCHING
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(^DPT(TMGDFN,.312,IDX)) QUIT:IDX'>0  DO
        . NEW INSIEN SET INSIEN=","_$GET(^DPT(TMGDFN,.312,IDX,0))_","
        . IF INS2INC[INSIEN DO
        . . DO NOW^%DTC
        . . SET TEST=1
        . . SET DATE=$$TODAY^TMGDATE
        QUIT
        ;"
COPDRESV(TMGDFN,TEST,DATE,DATA,TEXT)  ;"
        ;"Purpose: To determine if the patient has had a COPD dx
        SET TEST=0
        SET DATE=0
        NEW ICDIEN SET ICDIEN=0
        FOR  SET ICDIEN=$ORDER(^AUPNVPOV("C",TMGDFN,ICDIEN)) QUIT:+ICDIEN'>0  DO
        . NEW ZN SET ZN=$GET(^AUPNVPOV(ICDIEN,0)) QUIT:ZN=""
        . NEW VSTIEN SET VSTIEN=+$PIECE(ZN,"^",3) QUIT:VSTIEN'>0
        . NEW VSTZN SET VSTZN=$GET(^AUPNVSIT(VSTIEN,0)) QUIT:VSTZN=""
        . NEW VSTDT SET VSTDT=$PIECE(VSTZN,"^",1)
        . NEW VICDIEN SET VICDIEN=+$PIECE(ZN,"^",1) QUIT:VICDIEN'>0
        . NEW ICDZN SET ICDZN=$GET(^ICD9(VICDIEN,0)) QUIT:ICDZN=""
        . IF ICDZN["J44." DO
        . . IF VSTDT>DATE SET DATE=VSTDT
        ;"SET DATE=3150208
        IF DATE>0 DO
        . ;"IT IS DUE IF NOT USED THIS CALENDAR YEAR
        . DO NOW^%DTC
        . NEW JANUARY SET JANUARY=%I(3)_"0101"
        . IF JANUARY>DATE DO
        . . SET DATE=0
        . ELSE  DO
        . . SET TEST=1
        QUIT
        ;"
ADVCPEDN(TMGDFN,TEST,DATE,DATA,TEXT)  ;"
        ;"Purpose: To determine if the patient has had a CPE billed this
        ;"         calendar year
        SET TEST=0
        SET DATE=0
        NEW LAST SET LAST="NO CPE FOUND BILLED"
        NEW IEN SET IEN=0
        FOR  SET IEN=$ORDER(^AUPNVCPT("C",TMGDFN,IEN)) QUIT:IEN'>0  DO
        . NEW CPTIEN SET CPTIEN=$P($G(^AUPNVCPT(IEN,0)),"^",1)
        . NEW CPT SET CPT=$P($G(^ICPT(CPTIEN,0)),"^",1)
        . IF (CPT["9939")!(CPT["G043") DO    ;"ADDED AWV TO THIS 8/15/24, PER DR. K
        . . SET TEST=1
        . . NEW VISIT SET VISIT=$P($G(^AUPNVCPT(IEN,0)),"^",3)
        . . NEW THISDATE SET THISDATE=$P($G(^AUPNVSIT(VISIT,0)),"^",1)
        . . SET THISDATE=$E(THISDATE,1,3)_"0101.010101"
        . . IF THISDATE>DATE SET DATE=THISDATE
        IF DATE>0 SET LAST=$$EXTDATE^TMGDATE(DATE,1)
        QUIT LAST
        ;"
ADVPT(TMGDFN,TEST,DATE,DATA,TEXT)  ;"
        ;"Purpose: To detmine if the patient is an Advantage patient
        ;"         
        SET TEST=0
        SET DATE=0
        NEW INSARRAY
        SET INSARRAY(+$ORDER(^DIC(36,"B","BC/BS ADVANTAGE",0)))=""
        SET INSARRAY(+$ORDER(^DIC(36,"B","HUMANA GOLD",0)))=""
        SET INSARRAY(+$ORDER(^DIC(36,"B","AARP",0)))=""
        SET INSARRAY(+$ORDER(^DIC(36,"B","AARP / Secure Horizon",0)))=""
        SET INSARRAY(+$ORDER(^DIC(36,"B","UNITED HEALTHCARE - SECURE HORIZONS",0)))=""
        SET INSARRAY(+$ORDER(^DIC(36,"B","UHC-DSNP",0)))=""   ;"THIS MAY NEED TO BE ALTERED FOR TINCY CUTSHAW'S INSURANCE
        ;"IF BCBSAIEN'>0 GOTO BCDN
        NEW INSIDX SET INSIDX=0
        FOR  SET INSIDX=$ORDER(^DPT(TMGDFN,.312,INSIDX)) QUIT:INSIDX'>0  DO
        . NEW THISIEN SET THISIEN=$G(^DPT(TMGDFN,.312,INSIDX,0))
        . IF +$P($G(^DPT(TMGDFN,.312,INSIDX,0)),"^",20)'=1 QUIT
        . SET THISIEN=$P(THISIEN,"^",1)
        . IF $D(INSARRAY(THISIEN)) DO
        . . SET TEST=1
        QUIT
        ;"
SNPPT(TMGDFN,TEST,DATE,DATA,TEXT)  ;"
        ;"Purpose: To detmine if the patient is a SNP Patient
        ;"         
        SET TEST=0
        SET DATE=0
        NEW INSARRAY
        SET INSARRAY(+$ORDER(^DIC(36,"B","BLUECARE+",0)))=""
        SET INSARRAY(+$ORDER(^DIC(36,"B","HUMANAGLD+ (EDIT THIS NAME)",0)))=""
        SET INSARRAY(+$ORDER(^DIC(36,"B","UNITED HEALTHCARE - COMMUNITY",0)))=""
        ;"SET INSARRAY(+$ORDER(^DIC(36,"B","AARP / Secure Horizon",0)))=""
        ;"SET INSARRAY(+$ORDER(^DIC(36,"B","UNITED HEALTHCARE - SECURE HORIZONS",0)))=""
        ;"IF BCBSAIEN'>0 GOTO BCDN
        NEW INSIDX SET INSIDX=0
        FOR  SET INSIDX=$ORDER(^DPT(TMGDFN,.312,INSIDX)) QUIT:INSIDX'>0  DO
        . NEW THISIEN SET THISIEN=$G(^DPT(TMGDFN,.312,INSIDX,0))
        . IF +$P($G(^DPT(TMGDFN,.312,INSIDX,0)),"^",20)'=1 QUIT
        . SET THISIEN=$P(THISIEN,"^",1)
        . IF $D(INSARRAY(THISIEN)) DO
        . . SET TEST=1
        QUIT
        ;"        
MEDCARPT(TMGDFN,TEST,DATE,DATA,TEXT)  ;"
        ;"Purpose: To detmine if the patient is a Medicare patient
        ;"         
        SET TEST=0
        SET DATE=0
        NEW INSARRAY
        SET INSARRAY(+$ORDER(^DIC(36,"B","MEDICARE",0)))=""
        ;"IF BCBSAIEN'>0 GOTO BCDN
        NEW INSIDX SET INSIDX=0
        FOR  SET INSIDX=$ORDER(^DPT(TMGDFN,.312,INSIDX)) QUIT:INSIDX'>0  DO
        . NEW THISIEN SET THISIEN=$G(^DPT(TMGDFN,.312,INSIDX,0))
        . IF +$P($G(^DPT(TMGDFN,.312,INSIDX,0)),"^",20)'=1 QUIT
        . SET THISIEN=$P(THISIEN,"^",1)
        . IF $D(INSARRAY(THISIEN)) DO
        . . SET TEST=1
        QUIT
        ;"        
IGNADVPT(TMGDFN,TEST,DATE,DATA,TEXT)  ;"
        ;"Purpose: To detmine if the patient's CPE is turned off this year
        ;" 
        DO HFTHISYR(TMGDFN,.TEST,.DATE,"TMG BCBS ADV WELLNESS IGNORE 1 YR")
        QUIT
        ;"
IGNMAMMO(TMGDFN,TEST,DATE,DATA,TEXT)  ;"
        ;"Purpose: To detmine if the patient's MAMMO is turned off this year
        ;" 
        DO HFTHISYR(TMGDFN,.TEST,.DATE,"TMG MAMMOGRAM/IMAGING GYN MANAGING")
        IF TEST=0 DO
        . DO HFL14D(TMGDFN,.TEST,.DATE,"TMG MAMMOGRAM/IMAGING ORDERED")
        QUIT
        ;"        
HFTHISYR(TMGDFN,TEST,DATE,HFNAME)  ;"
        ;"Purpose: To detmine if the patient has a HF assigned this year
        ;"      alerts TEST and DATE
        SET TEST=0
        SET DATE=0
        NEW FIRSTDT SET FIRSTDT=$$FIRSTYR^TMGDATE()
        NEW IGNOREIEN SET IGNOREIEN=+$ORDER(^AUTTHF("B",HFNAME,0))
        IF IGNOREIEN'>0 QUIT
        NEW HFIEN SET HFIEN=0    
        FOR  SET HFIEN=$ORDER(^AUPNVHF("C",TMGDFN,HFIEN)) QUIT:(+HFIEN'>0)!(TEST=1)  DO
        . NEW CURHFIEN SET CURHFIEN=$PIECE($GET(^AUPNVHF(HFIEN,0)),"^",1)
        . IF CURHFIEN=IGNOREIEN DO
        . . NEW VISITIEN SET VISITIEN=$PIECE($GET(^AUPNVHF(HFIEN,0)),"^",3)
        . . NEW CURHFDATE
        . . SET CURHFDATE=$P($G(^AUPNVSIT(VISITIEN,0)),"^",1)
        . . IF CURHFDATE>FIRSTDT DO
        . . . SET TEST=1
        . . . SET DATE=CURHFDATE
        QUIT
        ;"        
HFL14D(TMGDFN,TEST,DATE,HFNAME)  ;"
        ;"Purpose: To detmine if the patient has a HF assigned in the last 14 days
        ;"      alerts TEST and DATE
        SET TEST=0
        SET DATE=0
        ;"NEW FIRSTDT SET FIRSTDT=$$FIRSTYR^TMGDATE()
        NEW BDT SET BDT=$$ADDDAYS^TMGDATE("-14")
        NEW IGNOREIEN SET IGNOREIEN=+$ORDER(^AUTTHF("B",HFNAME,0))
        IF IGNOREIEN'>0 QUIT
        NEW HFIEN SET HFIEN=0    
        FOR  SET HFIEN=$ORDER(^AUPNVHF("C",TMGDFN,HFIEN)) QUIT:(+HFIEN'>0)!(TEST=1)  DO
        . NEW CURHFIEN SET CURHFIEN=$PIECE($GET(^AUPNVHF(HFIEN,0)),"^",1)
        . IF CURHFIEN=IGNOREIEN DO
        . . NEW VISITIEN SET VISITIEN=$PIECE($GET(^AUPNVHF(HFIEN,0)),"^",3)
        . . NEW CURHFDATE
        . . SET CURHFDATE=$P($G(^AUPNVSIT(VISITIEN,0)),"^",1)
        . . IF CURHFDATE>BDT DO
        . . . SET TEST=1
        . . . SET DATE=CURHFDATE
        QUIT
        ;"         
A1CISDM(TMGDFN,TEST,DATE,DATA,TEXT)  ;"
        ;"Purpose: To detmine if the patient is a DM range A1C
        ;"         Returns reason why now.
        SET TEST=0
        SET DATE=0
        NEW GLUCOSEHIGH SET GLUCOSEHIGH=200
        NEW A1CHIGH SET A1CHIGH="6.4"
        NEW DAYCOUNT SET DAYCOUNT="-720"
        NEW TMGWHY SET TMGWHY="[WHY]"
        NEW THRESHOLD,RESULTS,TESTRESULT
        DO GETVALS^TMGLRR01(TMGDFN_"^2",97,.RESULTS)             
        DO GETVALS^TMGLRR01(TMGDFN_"^2",175,.RESULTS)
        NEW TESTNAME,THISDATE SET TESTNAME=1
        FOR  SET TESTNAME=$O(RESULTS(TESTNAME)) QUIT:+TESTNAME'>0  DO
        . IF TESTNAME["GLUCOSE" SET THRESHOLD=GLUCOSEHIGH
        . IF TESTNAME["A1C" SET THRESHOLD=A1CHIGH
        . SET THISDATE=$$ADDDAYS^TMGDATE(DAYCOUNT)
        . FOR  SET THISDATE=$O(RESULTS(TESTNAME,THISDATE)) QUIT:THISDATE'>0  DO
        . . SET TESTRESULT=+$G(RESULTS(TESTNAME,THISDATE))
        . . IF TESTRESULT>THRESHOLD DO
        . . . SET TEST=1,DATE=$$TODAY^TMGDATE
        . . . SET TMGWHY=TMGWHY_" "_$P(TESTNAME,"^",2)_" on "_$$EXTDATE^TMGDATE(THISDATE)_" was "_TESTRESULT_"."
A1CDN
        IF TMGWHY="[WHY]" SET TMGWHY="[WHY] THIS CAN BE IGNORED. No A1C>"_A1CHIGH_" or glucose>"_GLUCOSEHIGH_" in last 2 years."
        QUIT TMGWHY
        ;"
GETIUIEN(NAME) ;
       ;"Return the IEN of the given note title
       NEW TMGRESULT SET TMGRESULT=0
       SET TMGRESULT=$ORDER(^TIU(8925.1,"B",NAME,TMGRESULT))
       QUIT TMGRESULT
       ;"
TIUDATES(TMGDFN,TITLENAME,RESULTARR)  ;
       ;"Purpose: Return array of all a patient's complete physical exam
       ;"         note titles
       ;"Input: TMGDFN -- Patient's IEN
       ;"       RESULTARR(DATE,NOTEIEN)="" -- OUTPUT ARRAY 
       NEW IEN8925D1 SET IEN8925D1=$$GETIUIEN(TITLENAME)
       IF IEN8925D1'>0 GOTO GCDN
       NEW DATE SET DATE=0
       NEW TIUIEN,FMDATE
       SET TIUIEN=0
       FOR  SET TIUIEN=$ORDER(^TIU(8925,"C",TMGDFN,TIUIEN)) QUIT:TIUIEN'>0  DO
       . NEW DOCIEN,ZN
       . SET ZN=$GET(^TIU(8925,TIUIEN,0))
       . SET DOCIEN=$PIECE(ZN,"^",1)
       . IF DOCIEN=IEN8925D1 DO
       . . SET FMDATE=$PIECE(ZN,"^",7)
       . . SET RESULTARR(FMDATE,TIUIEN)=""
       GOTO GCDN
       ;"OLD METHOD
       FOR  SET DATE=$ORDER(^TIU(8925,"AE",TMGDFN,DATE)) QUIT:DATE'>0  DO
       . SET TIUIEN=0
       . FOR  SET TIUIEN=$ORDER(^TIU(8925,"AE",TMGDFN,DATE,TIUIEN)) QUIT:TIUIEN'>0  DO
       . . IF TIUIEN=IEN8925D1 DO
       . . . SET FMDATE=9999999-DATE
       . . . SET RESULTARR(FMDATE,$ORDER(^TIU(8925,"AE",TMGDFN,DATE,TIUIEN,0)))=""
GCDN   QUIT
       ;"
LASTCPES(TMGDFN,NGET,BDT,EDT,NFOUND,TEST,DATE,DATA,TEXT) ;
       ;"Purpose: Return dates of patient's complete physical exams -
       ;"         multiple computed finding
       ;"Input: TMGDFN -- the patient IEN
       ;"       NGET -- the number of findings to search for
       ;"       BDT -- the beginning date and time for the finding search
       ;"       EDT -- the ending date and time for the finding search
       ;"       NFOUND -- the number of findings found in the date range 
       ;"                 Should NEVER be larger than NGET, 
       ;"                 and SET to 0 IF no true findings are found.
       ;"       TEST(n) -- AN OUT PARAMETER.  The logical value of the test:
       ;"                1=true, 0=false
       ;"               Also an IN PARAMETER.  Any value for COMPUTED
       ;"                FINDING PARAMETER will be passed in here.
       ;"       DATE(n) -- AN OUT PARAMETER.  Date of finding.
       ;"            (NOTE: There is no need to SET the unsubscripted 
       ;"                   values of TEST and DATE in a multi-occurrence
       ;"                   computed finding.)
       ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
       ;"       TEXT -- Text to be display in the Clinical Maintenance
       ;"Output.  Optional.
       ;"Results: none
       SET NFOUND=0
       NEW NOTEDATE SET NOTEDATE=9999999
       NEW CPEARRAY
       NEW IEN SET IEN=0
       DO TIUDATES(TMGDFN,"COMPLETE PHYSICAL EXAM",.CPEARRAY)
       IF $DATA(CPEARRAY)=0 GOTO CPEDN
       FOR  SET NOTEDATE=$ORDER(CPEARRAY(NOTEDATE),-1) QUIT:(NOTEDATE'>0)!(NOTEDATE<BDT)!(NFOUND>NGET)  DO
       . IF NOTEDATE>EDT QUIT
       . SET IEN=0
       . FOR  SET IEN=$ORDER(CPEARRAY(NOTEDATE,IEN)) QUIT:IEN'>0  DO
       . . IF NGET=NFOUND QUIT
       . . SET NFOUND=NFOUND+1
       . . SET TEST(NFOUND)=1
       . . SET DATE(NFOUND)=NOTEDATE
CPEDN  QUIT
        ;
LASTEYEE(TMGTMGDFN,NGET,BDT,EDT,NFOUND,TEST,DATE,DATA,TEXT) ;
       ;"Purpose: Return dates of patient's last eye exam -
       ;"         multiple computed finding
       ;"Input: TMGDFN -- the patient IEN
       ;"       NGET -- the number of findings to search for
       ;"       BDT -- the beginning date and time for the finding search
       ;"       EDT -- the ending date and time for the finding search
       ;"       NFOUND -- the number of findings found in the date range
       ;"                 Should NEVER be larger than NGET,
       ;"                 and SET to 0 IF no true findings are found.
       ;"       TEST(n) -- AN OUT PARAMETER.  The logical value of the test:
       ;"                1=true, 0=false
       ;"               Also an IN PARAMETER.  Any value for COMPUTED
       ;"                FINDING PARAMETER will be passed in here.
       ;"       DATE(n) -- AN OUT PARAMETER.  Date of finding.
       ;"            (NOTE: There is no need to SET the unsubscripted
       ;"                   values of TEST and DATE in a multi-occurrence
       ;"                   computed finding.)
       ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
       ;"       TEXT -- Text to be display in the Clinical Maintenance
       ;"Output.  Optional.
       ;"Results: none
       SET NFOUND=0
       NEW NOTEDATE SET NOTEDATE=9999999
       NEW EYEEARRAY
       NEW IEN SET IEN=0
       DO TIUDATES(TMGDFN,"OPHTHO / OPTO / EYE CONSULTANT NOTE (IMAGE)",.EYEEARRAY)
       ;"DO TIUDATES(TMGDFN,"OPHTHO",.EYEEARRAY)
       IF $DATA(EYEEARRAY)=0 GOTO EYEEDN
       FOR  SET NOTEDATE=$ORDER(EYEEARRAY(NOTEDATE),-1) QUIT:(NOTEDATE'>0)!(NOTEDATE<BDT)!(NFOUND>NGET)  DO
       . IF NOTEDATE>EDT QUIT
       . SET IEN=0
       . FOR  SET IEN=$ORDER(EYEEARRAY(NOTEDATE,IEN)) QUIT:IEN'>0  DO
       . . IF NGET=NFOUND QUIT
       . . SET NFOUND=NFOUND+1
       . . SET TEST(NFOUND)=1
       . . SET DATE(NFOUND)=NOTEDATE
EYEEDN  QUIT
        ;
REMTEXT(X) ;
       ;"Purpose: remove any characters from a string
       S X=$TR(X,"ABCDEFGHIJKLMNOPQRSTUVWXYZ","")
       S X=$TR(X,"abcdefghijklmnopqustuvwxyz","")
       S X=$$TRIM^XLFSTR(X)
       QUIT X
       ;"
DATPAINK(TMGDFN,PIECE)   ;
       ;"Purpose: Return last date of pain contract,"DUE",or -1^Message
       NEW UDSIEN SET UDSIEN=113
       ;"Check here to see if
       NEW TMGRESULT SET TMGRESULT="-1^NO DATE FOUND"
       NEW CONTRACTTAG SET CONTRACTTAG="*CSM CONTRACT"
       SET TMGDFN=$GET(TMGDFN)
       IF TMGDFN'>0 DO  GOTO DNPC
       . SET TMGRESULT="-1^DFN NOT PROVIDED"
       NEW TMGMEDLIST,TMGMEDARRAY
       ;"//kt 5/7/18 DO MEDLIST^TMGTIUOJ(.TMGMEDLIST,.TMGDFN,.TMGMEDARRAY)
       DO MEDARR^TMGTIUOJ(.TMGMEDLIST,.TMGDFN,.TMGMEDARRAY)  ;"//kt 5/7/18
       IF $DATA(TMGMEDARRAY) DO
       . NEW CONTRACTDATE SET CONTRACTDATE=$$UP^XLFSTR($GET(TMGMEDARRAY("KEY-VALUE",CONTRACTTAG)))
       . ;"IF CONTRACTDATE["ds" 
       . SET CONTRACTDATE=$$TRIM^XLFSTR(CONTRACTDATE)
       . IF CONTRACTDATE[" " SET CONTRACTDATE=$PIECE(CONTRACTDATE," ",1)
       . SET CONTRACTDATE=$$REMTEXT($PIECE(CONTRACTDATE,"DS",PIECE))
       . SET X=CONTRACTDATE
       . DO ^%DT
       . IF Y>0 DO
       . . SET TMGRESULT=Y
       . ELSE  DO
       . . SET TMGRESULT="DUE"
       ELSE  DO
       . SET TMGRESULT="-1^MED LIST NOT FOUND"
DNPC   QUIT TMGRESULT
       ;
PAINDONE(TMGDFN,TEST,DATE,DATA,TEXT) ;
        ;"Purpose: Return information about the last pain contract
        ;"Input: TMGDFN -- the patient IEN
        ;"       TEST -- AN OUT PARAMETER.  The logical value of the test:
        ;"               1=true, 0=false
        ;"               Also an IN PARAMETER.  Any value for COMPUTED
        ;FINDING PARAMETER will be passed in here.
        ;"       DATE -- AN OUT PARAMETER.  Date of finding.
        ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
        ;"       TEXT -- Text to be display in the Clinical Maintenance
        ;"Output.  Optional.
        ;"Results: none
        NEW CONTRACTDATE SET CONTRACTDATE=$$DATPAINK(.TMGDFN,1)
        IF CONTRACTDATE="DUE" DO
        . SET TEST=0
        . SET DATE=0
        ELSE  IF +CONTRACTDATE>0 DO
        . SET TEST=1
        . SET DATE=CONTRACTDATE
        ELSE  DO
        . SET TEST=1
        . SET DATE=0
        QUIT
        ;"
CSDBDONE(TMGDFN,TEST,DATE,DATA,TEXT)  ;
        SET TEST=0,DATE=0
        NEW TMGTABLE,TMGTABLEARR
        ;"SET TMGTABLE=$$GETTABLX^TMGTIUO6(+$G(TMGDFN),"[MEDICATIONS]",.TMGTABLEARR)
        DO MEDARR^TMGTIUOJ(.TMGTABLE,+$G(TMGDFN),.TMGTABLEARR)  ;"//kt 5/8/18
        IF $DATA(TMGTABLEARR) DO
        . NEW CSDBDATA SET CSDBDATA=$$UP^XLFSTR($GET(TMGTABLEARR("KEY-VALUE","*CSM-DATABASE REVIEW")))
        . IF CSDBDATA'="" DO
        . . SET CSDBDATA=$PIECE(CSDBDATA," ",1)
        . . NEW %DT,X,Y
        . . SET %DT=""
        . . SET X=CSDBDATA
        . . DO ^%DT
        . . IF Y>0 DO
        . . . SET TEST=1
        . . . SET DATE=Y
        QUIT
        ;"
DRGSDONE(TMGDFN,TEST,DATE,DATA,TEXT)  ;
        ;"Purpose: Return information about the last drug screen
        ;"Input: TMGDFN -- the patient IEN
        ;"       TEST -- AN OUT PARAMETER.  The logical value of the test:
        ;"               1=true, 0=false
        ;"               Also an IN PARAMETER.  Any value for COMPUTED
        ;FINDING PARAMETER will be passed in here.
        ;"       DATE -- AN OUT PARAMETER.  Date of finding.
        ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
        ;"       TEXT -- Text to be display in the Clinical Maintenance
        ;"Output.  Optional.
        ;"Results: none
        NEW DSDATE SET DSDATE=$$GETHFDT^TMGPXRU1(.TMGDFN,"TMG DRUG SCREEN DONE")
        IF DSDATE'>0 SET DSDATE=+$$DATPAINK(.TMGDFN,2) 
        ;"Check to see if Urine Drug Screen note title exists and compare
        ;"    dates if it does.
        NEW UDSIEN SET UDSIEN=+$ORDER(^TIU(8925.1,"B","URINE DRUG SCREEN",0))
        IF UDSIEN>0 DO
        . NEW TIUIEN SET TIUIEN=0
        . FOR  SET TIUIEN=$ORDER(^TIU(8925,"B",UDSIEN,TIUIEN)) QUIT:TIUIEN'>0  DO
        . . NEW ZN SET ZN=$GET(^TIU(8925,TIUIEN,0))
        . . IF TMGDFN'=$PIECE(ZN,"^",2) QUIT
        . . IF $PIECE(ZN,"^",7)>DSDATE SET DSDATE=$PIECE(ZN,"^",7) 
        IF +DSDATE'>0 DO
        . SET TEST=0
        . SET DATE=0
        ELSE  DO
        . SET TEST=1
        . SET DATE=DSDATE     
        QUIT
        ;"
EKGDONE(TMGDFN,TEST,DATE,DATA,TEXT)  ;do not use yet
        SET TEST=0,DATE=0
        QUIT  ;"DATA IS INCONSISTENT
        NEW TMGTABLE,TMGTABLEARR
        SET TMGTABLE=$$GETTABLX^TMGTIUO6(+$G(TMGDFN),"[STUDIES]",.TMGTABLEARR)
        IF $DATA(TMGTABLEARR) DO
        . NEW EKGDATA SET EKGDATA=$$UP^XLFSTR($GET(TMGTABLEARR("KEY-VALUE","EKG")))
        . IF EKGDATA'="" DO
        . . SET EKGDATA=$PIECE(EKGDATA," ",1)
        . . NEW %DT,X,Y
        . . SET %DT=""
        . . SET X=EKGDATA
        . . DO ^%DT
        . . IF Y>0 DO
        . . . SET TEST=1
        . . . SET DATE=Y
        QUIT
        ;"
EKGNOTES(TMGDFN,HFARRAY)  ;"Get note titles
        NEW TIUIEN SET TIUIEN=0
        NEW EKGIEN SET EKGIEN=2017
        FOR  SET TIUIEN=$O(^TIU(8925,"C",TMGDFN,TIUIEN)) QUIT:TIUIEN'>0  DO
        . NEW ZN SET ZN=$G(^TIU(8925,TIUIEN,0))
        . IF $P(ZN,"^",1)'=EKGIEN QUIT
        . NEW REFDT SET REFDT=$P($P($G(^TIU(8925,TIUIEN,13)),"^",1),".",1)
        . SET HFARRAY(REFDT)="NOTE"
        QUIT
        ;"
LASTEKG2(TMGDFN)  ;"  USED FOR TIU TABLE
        NEW TMGRESULT,HFARRAY SET TMGRESULT="EKG = "
        NEW EKGDATE SET EKGDATE=$$GETHFDT^TMGPXRU1(.TMGDFN,"TMG EKG DONE",.HFARRAY)
        NEW NOTEARR
        DO EKGNOTES(.TMGDFN,.NOTEARR)
        NEW TOTALARR
        NEW DATE SET DATE=0
        FOR  SET DATE=$O(HFARRAY(DATE)) QUIT:DATE'>0  DO
        . SET TOTALARR(DATE,"HF")=$G(HFARRAY(DATE))
        SET DATE=0
        FOR  SET DATE=$O(NOTEARR(DATE)) QUIT:DATE'>0  DO
        . SET TOTALARR(DATE,"NOTE")=$G(NOTEARR(DATE))
        NEW COUNT SET COUNT=0
        SET DATE=9999999
        FOR  SET DATE=$ORDER(TOTALARR(DATE),-1) QUIT:(DATE="")!(COUNT>2)  DO
        . NEW TYPE SET TYPE=""
        . FOR  SET TYPE=$O(TOTALARR(DATE,TYPE)) QUIT:TYPE=""  DO
        . . NEW Y
        . . SET Y=$E(DATE,4,5)_"/"_$E(DATE,6,7)_"/"_($E(DATE,1,3)+1700)
        . . IF TMGRESULT="EKG = " DO
        . . . SET TMGRESULT=TMGRESULT_Y
        . . ELSE  DO
        . . . SET TMGRESULT=TMGRESULT_", "_Y
        . . SET TMGRESULT=TMGRESULT_" ("_TYPE_")"
        . . ;"IF $G(HFARRAY(DATE))="NOTE" DO
        . . ;". SET TMGRESULT=TMGRESULT_" (NOTE)"
        . . ;"ELSE  DO
        . . ;". SET TMGRESULT=TMGRESULT_" (HF)"
        . . SET COUNT=COUNT+1
        ;"IF TMGRESULT'="EKG = " SET TMGRESULT=TMGRESULT_" (HF)"
        NEW EKGSTUDIES SET EKGSTUDIES=$$LASTEKG(TMGDFN)
        IF (EKGSTUDIES'="")&(TMGRESULT="EKG = ") SET TMGRESULT=TMGRESULT_$P(EKGSTUDIES,"EKG:",2)_" (STUDIES TABLE)"
        QUIT TMGRESULT
        ;"
LASTEKG(TMGDFN)    ;"  USED FOR REMINDER DIALOG
        NEW TMGRESULT SET TMGRESULT=""
        NEW TMGTABLE,TMGTABLEARR,OPTION
        SET OPTION("HTML")=0
        SET TMGTABLE=$$GETTABLX^TMGTIUO6(+$G(TMGDFN),"[STUDIES]",.TMGTABLEARR,.OPTION)
        IF $DATA(TMGTABLEARR) DO
        . NEW IDX SET IDX=0
        . FOR  SET IDX=$ORDER(TMGTABLEARR(IDX)) QUIT:IDX'>0  DO
        . . NEW LINE SET LINE=$GET(TMGTABLEARR(IDX))
        . . IF LINE["EKG:" DO
        . . . SET TMGRESULT=LINE
        QUIT TMGRESULT
        ;"
UDSTABLE(TMGDFN,OUTARRAY)  ;"
        ;"Purpose: this function will be called from the UDS item in the Pain Table
        ;"         and return the last 3 entries
        NEW TMGRESULT,HFARRAY SET TMGRESULT="UDS: "
        NEW DSDATE SET DSDATE=$$GETHFDT^TMGPXRU1(.TMGDFN,"TMG DRUG SCREEN DONE",.HFARRAY)
        NEW UDSIEN SET UDSIEN=+$ORDER(^TIU(8925.1,"B","URINE DRUG SCREEN",0))
        IF UDSIEN>0 DO
        . NEW TIUIEN SET TIUIEN=0
        . FOR  SET TIUIEN=$ORDER(^TIU(8925,"B",UDSIEN,TIUIEN)) QUIT:TIUIEN'>0  DO
        . . NEW ZN SET ZN=$GET(^TIU(8925,TIUIEN,0))
        . . IF TMGDFN'=$PIECE(ZN,"^",2) QUIT
        . . SET HFARRAY($P($PIECE(ZN,"^",7),".",1))=""
        NEW COUNT SET COUNT=0
        NEW DATE SET DATE=9999999
        FOR  SET DATE=$ORDER(HFARRAY(DATE),-1) QUIT:(DATE="")!(COUNT>2)  DO
        . NEW Y
        . SET Y=$E(DATE,4,5)_"/"_$E(DATE,6,7)_"/"_($E(DATE,1,3)+1700)
        . IF TMGRESULT="UDS: " DO
        . . SET TMGRESULT=TMGRESULT_Y
        . ELSE  DO
        . . SET TMGRESULT=TMGRESULT_", "_Y
        . SET COUNT=COUNT+1
        QUIT TMGRESULT
        ;"
LASTHGT(TMGDFN,TEST,DATE,DATA,TEXT)   ;
        ;"Purpose: Return date of last height check and 
        ;"         whether it was in the last 2 years
        ;"Input: TMGDFN -- the patient IEN
        ;"       TEST -- AN OUT PARAMETER.  The logical value of the test:
        ;"               1=true, 0=false
        ;"               Also an IN PARAMETER.  Any value for COMPUTED
        ;FINDING PARAMETER will be passed in here.
        ;"       DATE -- AN OUT PARAMETER.  Date of finding.
        ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
        ;"       TEXT -- Text to be display in the Clinical Maintenance
        ;"Output.  Optional.
        ;"Results: none
        SET TEST=0,DATE=0
        NEW HEIGHTIEN
        SET HEIGHTIEN=$ORDER(^GMRD(120.51,"B","HEIGHT",0))
        IF HEIGHTIEN'>0 QUIT
        NEW GMRIEN SET GMRIEN=0
        NEW VITTYPE,VITDATA
        FOR  SET GMRIEN=$ORDER(^GMR(120.5,"C",TMGDFN,GMRIEN)) QUIT:(+GMRIEN'>0)  DO
        . SET VITDATA=$GET(^GMR(120.5,GMRIEN,0))
        . SET VITTYPE=$PIECE(VITDATA,"^",3)
        . IF VITTYPE'=HEIGHTIEN QUIT
        . SET DATE=$PIECE(VITDATA,"^",1)
        . SET TEST=1 
        QUIT
        ;
ONOXYGEN(TMGDFN,TEST,DATE,DATA,TEXT)  ;
        ;"Purpose: Return whether patient is on oxygen
        ;"Input: TMGDFN -- the patient IEN
        ;"       TEST -- AN OUT PARAMETER.  The logical value of the test:
        ;"               1=true, 0=false
        ;"               Also an IN PARAMETER.  Any value for COMPUTED
        ;FINDING PARAMETER will be passed in here.
        ;"       DATE -- AN OUT PARAMETER.  Date of finding.
        ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
        ;"       TEXT -- Text to be display in the Clinical Maintenance
        ;"Output.  Optional.
        ;"Results: none
        SET TEST=0,DATE=0
        ;"Test Med List
        NEW TMGMEDLIST,TMGMEDARRAY
        ;"//kt 5/7/18 DO MEDLIST^TMGTIUOJ(.TMGMEDLIST,.TMGDFN,.TMGMEDARRAY)  
        DO MEDARR^TMGTIUOJ(.TMGMEDLIST,.TMGDFN,.TMGMEDARRAY)     ;"//kt 5/7/18
        IF $DATA(TMGMEDARRAY) DO
        . NEW IDX SET IDX=0
        . NEW ITEM
        . FOR  SET IDX=$ORDER(TMGMEDARRAY(IDX)) QUIT:(+IDX'>0)!(TEST=1)  DO
        . . SET ITEM=$$UP^XLFSTR($GET(TMGMEDARRAY(IDX)))
        . . IF (ITEM["OXYGEN")!(ITEM["O2") DO
        . . . SET TEST=1
        IF TEST=1 GOTO OODN
        ;"Test COPD Table
        NEW TMGTABLE,TMGTABLEARR
        ;"//kt 5/7/18  SET TMGTABLE=$$GETTABLX^TMGTIUO6(+$G(TMGDFN),"[COPD]",.TMGTABLEARR)
        IF $$TABHASRX^TMGC0QT4(TMGDFN,"COPD",.TMGTABLEARR) ;"//kt 5/7/18
        IF $DATA(TMGTABLEARR) DO        
        . NEW O2DATA SET O2DATA=$GET(TMGTABLEARR("KEY-VALUE","HOME O2"))
        . IF O2DATA'="" DO
        . . SET O2DATA=$$UP^XLFSTR($PIECE(O2DATA," ",1))
        . . IF (O2DATA'["NONE")&(O2DATA'["N/A") DO
        . . . SET TEST=1
        IF TEST=1 GOTO OODN
        ;"Test Health Factors
        NEW O2HFIEN SET O2HFIEN=+$ORDER(^AUTTHF("B","TMG OXYGEN USE",0))
        IF O2HFIEN'>0 GOTO OODN
        NEW HFIEN SET HFIEN=0
        FOR  SET HFIEN=$ORDER(^AUPNVHF("C",TMGDFN,HFIEN)) QUIT:(+HFIEN'>0)!(TEST=1)  DO
        . NEW CURHFIEN SET CURHFIEN=$PIECE($GET(^AUPNVHF(HFIEN,0)),"^",1)
        . IF CURHFIEN=O2HFIEN SET TEST=1
OODN    QUIT
        ;"
NEEDTEST(TMGDFN,TEST,DATE,DATA,TEXT)  ;"  DETERMINE IF PATIENT NEEDS A TESTOSTERONE TOPIC
        NEW WHY SET WHY="[WHY] PATIENT DOES NOT NEED TOPIC"
        SET TEST=0,DATE=0
        ;"Test Med List for testosterone
        NEW TMGMEDLIST,TMGMEDARRAY
        DO MEDARR^TMGTIUOJ(.TMGMEDLIST,.TMGDFN,.TMGMEDARRAY)     ;"//kt 5/7/18
        IF $DATA(TMGMEDARRAY) DO
        . NEW IDX SET IDX=0
        . NEW ITEM
        . FOR  SET IDX=$ORDER(TMGMEDARRAY(IDX)) QUIT:(+IDX'>0)!(TEST=1)  DO
        . . SET ITEM=$$UP^XLFSTR($GET(TMGMEDARRAY(IDX)))
        . . IF ITEM["TESTOSTERONE" DO
        . . . SET TEST=1
        . . . SET DATE=$$TODAY^TMGDATE
        . . . SET WHY="[WHY] PATIENT IS ON TESTOSTERONE BUT DOES NOT HAVE A TOPIC"
        IF TEST=0 GOTO NTDN ;"IF NOT ON TESTOSTERONE, QUIT
        ;"Check patient's topics for testosterone... if found set to 0
        NEW IEN22719 SET IEN22719=0
        FOR  SET IEN22719=$ORDER(^TMG(22719,"DFN",TMGDFN,IEN22719)) QUIT:IEN22719'>0  DO
        . NEW TOPICTEXT SET TOPICTEXT=""
        . FOR  SET TOPICTEXT=$ORDER(^TMG(22719,IEN22719,2,"B",TOPICTEXT)) QUIT:TOPICTEXT=""  DO
        . . NEW UPTOPIC SET UPTOPIC=$$UP^XLFSTR(TOPICTEXT)
        . . IF UPTOPIC["TESTOSTERONE" DO
        . . . NEW TOPICDATE SET TOPICDATE=$PIECE($GET(^TMG(22719,IEN22719,0)),"^",2)
        . . . IF TOPICDATE>DATE SET DATE=TOPICDATE
        . . . SET TEST=0,DATE=0  ;"TURN OFF
        . . . SET WHY="[WHY] PATIENT IS ON TESTOSTERONE BUT HAS TOPIC ALREADY"
NTDN
        QUIT WHY
        ;"
APNEAPT(TMGDFN,TEST,DATE,DATA,TEXT)  ;
        ;"Purpose: Return whether patient uses CPAP machine
        ;"Input: TMGDFN -- the patient IEN
        ;"       TEST -- AN OUT PARAMETER.  The logical value of the test:
        ;"               1=true, 0=false
        ;"               Also an IN PARAMETER.  Any value for COMPUTED
        ;FINDING PARAMETER will be passed in here.
        ;"       DATE -- AN OUT PARAMETER.  Date of finding.
        ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
        ;"       TEXT -- Text to be display in the Clinical Maintenance
        ;"Output.  Optional.
        ;"Results: WHY
        NEW WHY SET WHY=""
        SET TEST=0,DATE=0
        NEW IEN22719 SET IEN22719=0
        FOR  SET IEN22719=$ORDER(^TMG(22719,"DFN",TMGDFN,IEN22719)) QUIT:IEN22719'>0  DO
        . NEW TOPICTEXT SET TOPICTEXT=""
        . FOR  SET TOPICTEXT=$ORDER(^TMG(22719,IEN22719,2,"B",TOPICTEXT)) QUIT:TOPICTEXT=""  DO
        . . NEW UPTOPIC SET UPTOPIC=$$UP^XLFSTR(TOPICTEXT)
        . . IF UPTOPIC["APNEA" DO
        . . . NEW TOPICDATE SET TOPICDATE=$PIECE($GET(^TMG(22719,IEN22719,0)),"^",2)
        . . . IF TOPICDATE>DATE SET DATE=TOPICDATE
        . . . SET TEST=1
         . . . SET WHY="[WHY] PATIENT HAS SLEEP APNEA TOPIC. LAST DISCUSSED ON "_$$EXTDATE^TMGDATE(TOPICDATE,1)
CPAPDN  QUIT WHY
        ;"
PULSEOX(TMGDFN,TEST,DATE,DATA,TEXT)  ;
        ;"Purpose: Return whether patient is on oxygen
        ;"Input: TMGDFN -- the patient IEN
        ;"       TEST -- AN OUT PARAMETER.  The logical value of the test:
        ;"               1=true, 0=false
        ;"               Also an IN PARAMETER.  Any value for COMPUTED
        ;FINDING PARAMETER will be passed in here.
        ;"       DATE -- AN OUT PARAMETER.  Date of finding.
        ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
        ;"       TEXT -- Text to be display in the Clinical Maintenance
        ;"Output.  Optional.
        ;"Results: none
        SET TEST=0,DATE=0
        NEW TMGTABLE,TMGTABLEARR
        SET TMGTABLE=$$GETTABLX^TMGTIUO6(+$G(TMGDFN),"[COPD]",.TMGTABLEARR)
        IF $DATA(TMGTABLEARR) DO
        . NEW PFTDATA SET PFTDATA=$$UP^XLFSTR($GET(TMGTABLEARR("KEY-VALUE","PFT TESTING")))
        . IF PFTDATA'="" DO
        . . SET PFTDATA=$PIECE(PFTDATA," ",1)
        . . NEW %DT,X,Y
        . . SET %DT=""
        . . SET X=PFTDATA
        . . DO ^%DT
        . . IF Y>0 DO
        . . . SET TEST=1
        . . . SET DATE=Y
        QUIT
        ;"
PTHASDM(TMGDFN,TEST,DATE,DATA,TEXT)  ;
        ;"Purpose: Return whether patient is diabetic
        ;"         Will search the TMG TIU 
        ;"Input: TMGDFN -- the patient IEN
        ;"       TEST -- AN OUT PARAMETER.  The logical value of the test:
        ;"               1=true, 0=false
        ;"               Also an IN PARAMETER.  Any value for COMPUTED
        ;FINDING PARAMETER will be passed in here.
        ;"       DATE -- AN OUT PARAMETER.  Date of finding.
        ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
        ;"       TEXT -- Text to be display in the Clinical Maintenance
        ;"Output.  WHY
        ;"Results: none
        SET TEST=0,DATE=0
        NEW WHY SET WHY="[WHY] Unknown reason. No note topics found."
        NEW IEN22719 SET IEN22719=0
        FOR  SET IEN22719=$ORDER(^TMG(22719,"DFN",TMGDFN,IEN22719)) QUIT:IEN22719'>0  DO
        . NEW TOPICTEXT SET TOPICTEXT=""
        . FOR  SET TOPICTEXT=$ORDER(^TMG(22719,IEN22719,2,"B",TOPICTEXT)) QUIT:TOPICTEXT=""  DO
        . . NEW UPTOPIC SET UPTOPIC=$$UP^XLFSTR(TOPICTEXT)
        . . IF (UPTOPIC["DM")!(UPTOPIC["DIABETES") DO
        . . . IF UPTOPIC["FH" QUIT  ;"elh added 9/8/16 to keep family history from giving false positive
        . . . IF UPTOPIC["SCREEN" QUIT ;"elh added 9/12/16 to keep screenings from giving false positives
        . . . IF UPTOPIC["PRE" QUIT ;"elh added 9/15/16 to keep pre-diabetes from giving false positives
        . . . IF UPTOPIC["POSSIBLE" QUIT ;"elh added 3/5/21
        . . . NEW TOPICDATE SET TOPICDATE=$PIECE($GET(^TMG(22719,IEN22719,0)),"^",2)
        . . . IF TOPICDATE>DATE SET DATE=TOPICDATE
        . . . SET TEST=1
        . . . SET WHY=$C(13,10)_"[WHY] Topic "_UPTOPIC_" was used last on "_$$EXTDATE^TMGDATE(DATE)
        ;"See if "Not Diabetic" health factor exists for patient with a
        ;"    later date than the last topic
        NEW HFIEN SET HFIEN=+$ORDER(^AUTTHF("B","TMG PATIENT NOT DIABETIC",0))
        IF HFIEN>0 DO
        . NEW HFDATE
        . SET HFDATE=+$ORDER(^AUPNVHF("AA",TMGDFN,HFIEN,0))
        . IF HFDATE>0 DO
        . . SET HFDATE=9999999-HFDATE
        . . IF HFDATE>DATE DO
        . . . SET TEST=0
        . . . SET DATE=0
        . . . SET WHY="[WHY] This reminder was turned off by health factor"
        QUIT WHY
        ;"
PTHSCOPD(TMGDFN,TEST,DATE,DATA,TEXT)  ;
        ;"Purpose: Return whether patient has COPD
        ;"         Will search the TMG TIU
        ;"Input: TMGDFN -- the patient IEN
        ;"       TEST -- AN OUT PARAMETER.  The logical value of the test:
        ;"               1=true, 0=false
        ;"               Also an IN PARAMETER.  Any value for COMPUTED
        ;FINDING PARAMETER will be passed in here.
        ;"       DATE -- AN OUT PARAMETER.  Date of finding.
        ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
        ;"       TEXT -- Text to be display in the Clinical Maintenance
        ;"Output.  Optional.
        ;"Results: none
        SET TEST=0,DATE=0
        NEW IEN22719 SET IEN22719=0
        FOR  SET IEN22719=$ORDER(^TMG(22719,"DFN",TMGDFN,IEN22719)) QUIT:IEN22719'>0  DO
        . NEW TOPICTEXT SET TOPICTEXT=""
        . FOR  SET TOPICTEXT=$ORDER(^TMG(22719,IEN22719,2,"B",TOPICTEXT)) QUIT:TOPICTEXT=""  DO
        . . NEW UPTOPIC SET UPTOPIC=$$UP^XLFSTR(TOPICTEXT)
        . . IF (UPTOPIC["COPD")!(UPTOPIC["EMPHYSEMA") DO
        . . . NEW TOPICDATE SET TOPICDATE=$PIECE($GET(^TMG(22719,IEN22719,0)),"^",2)
        . . . IF TOPICDATE>DATE SET DATE=TOPICDATE
        . . . SET TEST=1
        QUIT
        ;"
PTHASTOP(TMGDFN,TOPICNAME)  ;"DOES THIS PATIENT HAS TOPIC
        NEW TMGRESULT SET TMGRESULT=0
        SET TOPICNAME=$$UP^XLFSTR(TOPICNAME)
        NEW IEN22719 SET IEN22719=9999999
        NEW FOUNDOFFNOTE SET FOUNDOFFNOTE=0  ;"10/28/21 ONLY USE LAST OFFICE NOTE
        FOR  SET IEN22719=$ORDER(^TMG(22719,"DFN",TMGDFN,IEN22719),-1) QUIT:(IEN22719'>0)!(FOUNDOFFNOTE=1)  DO
        . NEW NOTEIEN SET NOTEIEN=$P($G(^TMG(22719,IEN22719,0)),"^",1);"10/28/21
        . IF +NOTEIEN'>0 QUIT
        . NEW NOTETYPE SET NOTETYPE=$P($G(^TIU(8925,NOTEIEN,0)),"^",1);"10/28/21
        . IF +NOTETYPE'>0 QUIT
        . SET OFFICENOTE=$P($G(^TIU(8925.1,NOTETYPE,"TMGH")),"^",1);"10/28/21
        . IF OFFICENOTE="Y" SET FOUNDOFFNOTE=1  ;"10/28/21
        . NEW TOPICTEXT SET TOPICTEXT=""
        . NEW TOPICCNT SET TOPICCNT=0
        . FOR  SET TOPICTEXT=$ORDER(^TMG(22719,IEN22719,2,"B",TOPICTEXT)) QUIT:TOPICTEXT=""  DO
        . . SET TOPICCNT=TOPICCNT+1
        . . NEW UPTOPIC SET UPTOPIC=$$UP^XLFSTR(TOPICTEXT)
        . . IF UPTOPIC[TOPICNAME DO
        . . . ;"SET TOPICDATE=$PIECE($GET(^TMG(22719,IEN22719,0)),"^",2)
        . . . ;"IF TOPICDATE>DATE SET DATE=TOPICDATE
        . . . SET TMGRESULT=1
        . IF TOPICCNT=0 SET FOUNDOFFNOTE=0  ;"DON'T INCLUDE IF LAST ON DIDN'T HAVE TOPICS (COULD BE ROS)
        QUIT TMGRESULT
        ;"
PTASTHMA(TMGDFN,TEST,DATE,DATA,TEXT)  ;
        ;"Purpose: Return whether patient has asthma
        ;"         Will search the TMG TIU
        ;"Input: TMGDFN -- the patient IEN
        ;"       TEST -- AN OUT PARAMETER.  The logical value of the test:
        ;"               1=true, 0=false
        ;"               Also an IN PARAMETER.  Any value for COMPUTED
        ;FINDING PARAMETER will be passed in here.
        ;"       DATE -- AN OUT PARAMETER.  Date of finding.
        ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
        ;"       TEXT -- Text to be display in the Clinical Maintenance
        ;"Output.  Optional.
        ;"Results: none
        SET TEST=0,DATE=0
        NEW IEN22719 SET IEN22719=0
        FOR  SET IEN22719=$ORDER(^TMG(22719,"DFN",TMGDFN,IEN22719)) QUIT:IEN22719'>0  DO
        . NEW TOPICTEXT SET TOPICTEXT=""
        . FOR  SET TOPICTEXT=$ORDER(^TMG(22719,IEN22719,2,"B",TOPICTEXT)) QUIT:TOPICTEXT=""  DO
        . . NEW UPTOPIC SET UPTOPIC=$$UP^XLFSTR(TOPICTEXT)
        . . IF UPTOPIC["ASTHMA" DO
        . . . NEW TOPICDATE SET TOPICDATE=$PIECE($GET(^TMG(22719,IEN22719,0)),"^",2)
        . . . IF TOPICDATE>DATE SET DATE=TOPICDATE
        . . . SET TEST=1
        QUIT
        ;"
PTLIPID(TMGDFN,TEST,DATE,DATA,TEXT)   ;
        ;"Purpose: Return whether patient is hypertensive
        ;"         Will search the TMG TIU
        ;"Input: TMGDFN -- the patient IEN
        ;"       TEST -- AN OUT PARAMETER.  The logical value of the test:
        ;"               1=true, 0=false
        ;"               Also an IN PARAMETER.  Any value for COMPUTED
        ;FINDING PARAMETER will be passed in here.
        ;"       DATE -- AN OUT PARAMETER.  Date of finding.
        ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
        ;"       TEXT -- Text to be display in the Clinical Maintenance
        ;"Output.  Optional.
        ;"Results: none
        SET TEST=0,DATE=0
        NEW IEN22719 SET IEN22719=0
        FOR  SET IEN22719=$ORDER(^TMG(22719,"DFN",TMGDFN,IEN22719)) QUIT:IEN22719'>0  DO
        . NEW TOPICTEXT SET TOPICTEXT=""
        . FOR  SET TOPICTEXT=$ORDER(^TMG(22719,IEN22719,2,"B",TOPICTEXT)) QUIT:TOPICTEXT=""  DO
        . . IF (TOPICTEXT["HTN")!(TOPICTEXT["HYPERTENSION")!(TOPICTEXT["DM") DO
        . . . NEW TOPICDATE SET TOPICDATE=$PIECE($GET(^TMG(22719,IEN22719,0)),"^",2)
        . . . IF TOPICDATE>DATE SET DATE=TOPICDATE
        . . . SET TEST=1        
        QUIT
        ;"
SRCHTOPX(TMGDFN,TEST,DATE,TEXT)  ;
        ;"Purpose: Will search each topix in the TMG TIU DOCUMENT TOPICS
        ;"      NOTE: Currently unused but here in case we need more than DM
        ;"            and HTN CF
        SET TEST=0,DATE=0
        NEW IEN22719 SET IEN22719=0
        FOR  SET IEN22719=$ORDER(^TMG(22719,"DFN",TMGDFN,IEN22719)) QUIT:IEN22719'>0  DO
        . NEW TOPICTEXT SET TOPICTEXT=""
        . FOR  SET TOPICTEXT=$ORDER(^TMG(22719,IEN22719,2,"B",TOPICTEXT)) QUIT:TOPICTEXT=""  DO
        . . IF TOPICTEXT[TEXT DO
        . . . NEW TOPICDATE SET TOPICDATE=$PIECE($GET(^TMG(22719,IEN22719,0)),"^",2)
        . . . IF TOPICDATE>DATE SET DATE=TOPICDATE
        . . . SET TEST=1
        QUIT
        ;"
PACKYR(TMGDFN)
        ;"RETURN ALL THE PACKYEARS FOR THE GIVEN PATIENT FOR TMG TOBACCO
        ;"        PACK YEARS OBJECT
        NEW TMGRESULT,RESULTARR SET TMGRESULT=""
        NEW TOB20IEN,TOB30IEN,TOB40IEN,TOB50IEN
        SET TOB20IEN=779,TOB30IEN=780,TOB40IEN=781,TOB50IEN=782
        NEW HFARRAY,HFDATE,HFIEN
        DO GETHFGRP(.TMGDFN,765,.HFARRAY)
        SET HFDATE=9999999
        ;"GATHER ALL PACKYEARS (20+) IN AN ARRAY
        FOR  SET HFDATE=$ORDER(HFARRAY(HFDATE),-1) QUIT:(HFDATE'>0)  DO
        . SET HFIEN=0
        . NEW TEMP
        . FOR  SET HFIEN=$ORDER(HFARRAY(HFDATE,HFIEN)) QUIT:(HFIEN'>0)  DO
        . . SET TEMP=""
        . . IF HFIEN=TOB20IEN SET TEMP=" * 20-29 PACK YEAR on "_$$EXTDATE^TMGDATE(HFDATE)
        . . IF HFIEN=TOB30IEN SET TEMP=" * 30-39 PACK YEAR on "_$$EXTDATE^TMGDATE(HFDATE)
        . . IF HFIEN=TOB40IEN SET TEMP=" * 40-49 PACK YEAR on "_$$EXTDATE^TMGDATE(HFDATE)
        . . IF HFIEN=TOB50IEN SET TEMP=" * 50+ PACK YEAR on "_$$EXTDATE^TMGDATE(HFDATE)
        . . IF TEMP'="" DO
        . . . SET TMGRESULT=TMGRESULT_$C(13,10)_TEMP
        ;"
        IF TMGRESULT="" SET TMGRESULT="No pack years found > 20"
        SET TMGRESULT="PACK YEARS ENTERED:"_TMGRESULT
        QUIT TMGRESULT
        ;"
LDCTSMKR(TMGDFN,TEST,DATE,DATA,TEXT) ;
        ;"Purpose: Determine IF patient is currently a smoker or has quit in
        ;"         the last 15 years
        ;"Input: TMGDFN -- the patient IEN
        ;"       TEST -- AN OUT PARAMETER.  The logical value of the test:
        ;                1=true, 0=false
        ;"               Also an IN PARAMETER.  Any value for COMPUTED
        ;FINDING PARAMETER will be passed in here.
        ;"       DATE -- AN OUT PARAMETER.  Date of finding.
        ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
        ;"       TEXT -- Text to be display in the Clinical Maintenance
        ;"Output.  Optional.
        ;"Results: none
        SET TEST=0,DATE=0
        NEW TMGSMKRIEN
        NEW TMGQSIEN  ;"was TMGQUITSMOKINGIEN
        SET TMGSMKRIEN=+$ORDER(^AUTTHF("B","TMG TOBACCO EVERYDAY USER",0))
        SET TMGQSIEN=+$ORDER(^AUTTHF("B","TMG TOBACCO FORMER USER",0))
        IF (TMGSMKRIEN'>0)!(TMGQSIEN'>0) GOTO LDDN
        ;"GET DATE 15 YEARS AGO
        NEW X,X1,X2,DATETOTEST
        DO NOW^%DTC
        SET X1=X
        SET X2=-(15*365) ;"15 YEARS
        DO C^%DTC
        SET DTTOTEST=X
        ;"
        NEW HFARRAY,FOUND,HFDATE,HFTYPEIEN,HFIEN
        DO GETHFGRP(.TMGDFN,765,.HFARRAY)
        SET HFDATE=9999999,FOUND=0,HFIEN=0
        FOR  SET HFDATE=$ORDER(HFARRAY(HFDATE),-1) QUIT:(HFDATE'>0)!(FOUND=1)  DO
        . FOR  SET HFIEN=$ORDER(HFARRAY(HFDATE,HFIEN)) QUIT:(HFIEN'>0)  DO
        . . IF HFIEN=TMGSMKRIEN DO
        . . . SET DATE=HFDATE,TEST=1
        . . . SET FOUND=1
        . . IF HFIEN=TMGQSIEN DO
        . . . IF HFDATE>DTTOTEST DO
        . . . . SET DATE=HFDATE,TEST=1
        . . . . SET FOUND=1
LDDN    QUIT
        ;"
CURSMOKR(TMGDFN,TEST) ;
        ;"Purpose: Determine IF patient is currently a smoker or has quit in
        ;"         the last 15 years
        ;"Input: TMGDFN -- the patient IEN
        ;"       TEST -- AN OUT PARAMETER.  The logical value of the test:
        ;                1=true, 0=false
        ;"               Also an IN PARAMETER.  Any value for COMPUTED
        ;FINDING PARAMETER will be passed in here.
        ;"       DATE -- AN OUT PARAMETER.  Date of finding.
        ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
        ;"       TEXT -- Text to be display in the Clinical Maintenance
        ;"Output.  Optional.
        ;"Results: none
        SET TEST=0
        NEW TMGSMKRIEN
        NEW TMGQSIEN  ;"was TMGQUITSMOKINGIEN
        SET TMGSMKRIEN=+$ORDER(^AUTTHF("B","TMG TOBACCO EVERYDAY USER",0))
        SET TMGQSIEN=+$ORDER(^AUTTHF("B","TMG TOBACCO FORMER USER",0))
        IF (TMGSMKRIEN'>0)!(TMGQSIEN'>0) GOTO CURSDN
        NEW HFARRAY,FOUND,HFDATE,HFTYPEIEN,HFIEN
        DO GETHFGRP(.TMGDFN,765,.HFARRAY)
        SET HFDATE=9999999,FOUND=0,HFIEN=0
        FOR  SET HFDATE=$ORDER(HFARRAY(HFDATE),-1) QUIT:(HFDATE'>0)!(FOUND=1)  DO
        . FOR  SET HFIEN=$ORDER(HFARRAY(HFDATE,HFIEN)) QUIT:(HFIEN'>0)  DO
        . . IF HFIEN=TMGSMKRIEN DO
        . . . SET TEST=1
        . . . SET FOUND=1
        . . IF HFIEN=TMGQSIEN DO
        . . . ;"SET DATE=HFDATE,TEST=1
        . . . ;"since we found this, first we can assume patient quit and go no further
        . . . SET FOUND=1
CURSDN    QUIT
        ;"        
GETHFGRP(TMGDFN,HFGROUPIEN,TMGRESULTARR)  ;"Return health factors for a patient by a given hf group
        ;"Purpose:
        ;"Input:
        ;"Output:
        NEW HFIEN,DATE
        SET HFIEN=0
        FOR  SET HFIEN=$ORDER(^AUTTHF("AC",HFGROUPIEN,HFIEN)) QUIT:HFIEN'>0  DO
        . SET DATE=0
        . FOR  SET DATE=$ORDER(^AUPNVHF("AA",TMGDFN,HFIEN,DATE)) QUIT:DATE'>0  DO
        . . SET TMGRESULTARR(9999999-DATE,HFIEN)=""
        QUIT
        ;"
MCOGRSLT(TMGDFN)  ;"
        NEW TMGRESULT SET TMGRESULT="Mini Cog Results = "
        NEW RESULTS,COUNT,MAX,HFTAG
        SET COUNT=0,MAX=3
        ;"SET HFTAG(2318)="ABN",HFTAG(2347)="BRD",HFTAG(2317)="N"
        SET HFTAG(2347)="abnl",HFTAG(2318)="borderline",HFTAG(2317)="NL"
        DO GETHFGRP(TMGDFN,2348,.RESULTS)
        NEW DATE SET DATE=9999999
        FOR  SET DATE=$O(RESULTS(DATE),-1) QUIT:(DATE'>0)!(COUNT>MAX)  DO
        . NEW HFIEN SET HFIEN=$O(RESULTS(DATE,0))
        . IF COUNT=0 DO
        . . SET TMGRESULT=TMGRESULT_$$EXTDATE^TMGDATE(DATE)_" ("_$G(HFTAG(HFIEN))_")"
        . ELSE  DO
        . . SET TMGRESULT=TMGRESULT_", "_$$EXTDATE^TMGDATE(DATE)_" ("_$G(HFTAG(HFIEN))_")"
        . SET COUNT=COUNT+1
        QUIT TMGRESULT
        ;"
MCOGDIAG(TMGDFN)  ;"MINI COG RESULTS FOR REMINDER DIALOG
        NEW TMGRESULT SET TMGRESULT="Mini Cog Results = "
        NEW RESULTS,COUNT,MAX,HFTAG
        SET COUNT=0,MAX=3
        SET HFTAG(2318)="abnl",HFTAG(2347)="borderline",HFTAG(2317)="NL"
        DO GETHFGRP(TMGDFN,2348,.RESULTS)
        NEW MCOGTESTS,DATE SET DATE=0 
        FOR  SET DATE=$ORDER(^AUPNVHF("AA",TMGDFN,2316,DATE)) QUIT:DATE'>0  DO
        . NEW THISIEN SET THISIEN=$O(^AUPNVHF("AA",TMGDFN,2316,DATE,0))
        . NEW COMMENT SET COMMENT=$P($G(^AUPNVHF(THISIEN,811)),"^",1)
        . SET MCOGTESTS(9999999-DATE,2316)=COMMENT
        SET DATE=9999999
        FOR  SET DATE=$O(MCOGTESTS(DATE),-1) QUIT:(DATE'>0)!(COUNT>MAX)  DO
        . NEW HFIEN SET HFIEN=$O(RESULTS(DATE,0))
        . NEW COMMENT SET COMMENT=$G(MCOGTESTS(DATE,2316))
        . IF COUNT=0 DO
        . . SET TMGRESULT=TMGRESULT_$$EXTDATE^TMGDATE(DATE)_"["_COMMENT_"] ("_$G(HFTAG(HFIEN))_")"
        . ELSE  DO
        . . SET TMGRESULT=TMGRESULT_$C(13,10)_$$EXTDATE^TMGDATE(DATE)_"["_COMMENT_"] ("_$G(HFTAG(HFIEN))_")"
        . SET COUNT=COUNT+1
        QUIT TMGRESULT
        ;"        
FLUREM(TMGDFN,TEST,DATE,DATA,TEXT) ;"Return whether flu reminder should be active
        ;"Purpose: Determine whether a flu reminder should be active
        ;"Input: See discussion above for details.
        ;"Output: TEST=1 or 0
        ;"Results: none
        SET DATE=$GET(PXRMDATE)
        IF DATE="" SET DATE=$$NOW^XLFDT
        SET TEST=0
        NEW YEAR,MONTH,DAY,TIME
        DO PARSEDT(DATE,.YEAR,.MONTH,.DAY,.TIME)
        IF MONTH<3 SET TEST=1
        IF MONTH>8 SET TEST=1
        QUIT
        ;"
;"This following code can be deleted - it is scratch code to run an isolated report
REPORT
       NEW %ZIS
       SET %ZIS("A")="Enter Output Device: "
       SET %ZIS("B")="HOME"
       DO ^%ZIS  ;"standard device call
       ;IF POP DO  GOTO AIDn
       ;. DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output. Aborting.")
       use IO
       WRITE "PATIENTS WITH MEDICARE WHO ARE DIABETIC",!
       WRITE "BASED ON PRIMARY INSURANCE AND CPRS NOTE TOPICS",!
       NEW TMGDFN SET TMGDFN=0
       NEW TEST,DATE,DATA,TEXT
       FOR  SET TMGDFN=$ORDER(^DPT(TMGDFN)) QUIT:+TMGDFN'>0  DO
       . DO PTHASDM(TMGDFN,.TEST,.DATE,.DATA,.TEXT)
       . IF TEST=0 QUIT
       . IF $$EXCLUDE^TMGC0QTU(TMGDFN)=1 QUIT
       . WRITE "[ ] ",$PIECE($GET(^DPT(TMGDFN,0)),"^",1),!
       DO ^%ZISC  ;" Close the output device
       DO PRESS2GO^TMGUSRI2
       QUIT
       ;"
HTNMEDS(TMGDFN,TEST,DATE,DATA,TEXT) ;
        ;"Purpose: Determine if patient is on HTN medication
        ;"Input: TMGDFN -- the patient IEN
        ;"       TEST -- AN OUT PARAMETER.  The logical value of the test:
        ;                1=true, 0=false
        ;"               Also an IN PARAMETER.  Any value for COMPUTED
        ; FINDING PARAMETER will be passed in here.
        ;"       DATE -- AN OUT PARAMETER.  Date of finding.
        ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
        ;"       TEXT -- Text to be display in the Clinical Maintenance
        ;"Output.  Optional.
        ;"Results: WHY - returns why HTNMEDS is true 
        SET TEST=0
        SET DATE=0
        NEW WHY SET WHY="Patient is NOT on HTN meds and does NOT have a HTN topic"
        NEW X DO NOW^%DTC
        NEW TMGRESULT,MEDARR SET TMGRESULT=$$ONHTNTX^TMGC0QT4(TMGDFN,X,.MEDARR)
        NEW MED SET MED=""
        IF $D(MEDARR) DO
        . SET WHY=""
        . FOR  SET MED=$O(MEDARR(MED)) QUIT:MED=""  DO
        . . IF WHY'="" SET WHY=WHY_", "
        . . SET WHY=WHY_MED_$C(13,10)
        . SET WHY="PATIENT IS ON THESE HTN MEDS: "_WHY
        IF TMGRESULT=0 DO   ;"If not on meds, check topics
        . IF ($$PTHASTOP^TMGPXR01(TMGDFN,"HTN")=1)!($$PTHASTOP^TMGPXR01(TMGDFN,"HYPERTENSION")=1) DO
        . . SET TMGRESULT=1
        . . SET WHY="PATIENT DOESN'T SEEM TO HAVE HTN MEDS, BUT DOES HAVE A HYPERTENSION TOPIC FOUND"
        IF TMGRESULT=1 DO
        . SET TEST=1
        . SET DATE=$$TODAY^TMGDATE
        QUIT WHY
        ;"    
HTNCTRL(TMGDFN)  ;"Determine 
        NEW TMGRESULT SET TMGRESULT="ERROR DETERMINING CONTROL STATUS"
        NEW GOAL,SGOAL,DGOAL
        NEW BP,SBP,DBP
        ;"
        DO HTNGOAL(TMGDFN,.GOAL)
        SET SGOAL=+$P($P(GOAL,"/",1),"<",2)
        SET DGOAL=+$P(GOAL,"/",2)
        ;"        
        NEW LASTBP SET LASTBP=$$TREND^TMGGMRV1(TMGDFN,"T","BP",1,"")
        SET SBP=+$P(LASTBP,"/",1),DBP=+$P(LASTBP,"/",2)
        ;"
        NEW MSG,SMSG,DMSG SET SMSG="",DMSG=""
        IF SBP>SGOAL SET SMSG="SYS"
        IF DBP>DGOAL SET DMSG="DIA"
        ;"
        IF (SMSG="")&(DMSG="") DO
        . SET MSG="IN GOAL"
        ELSE  DO
        . NEW ABOVETEXT SET ABOVETEXT=""
        . IF SMSG'="" SET ABOVETEXT=SMSG
        . IF DMSG'="" DO
        . . IF SMSG="" SET ABOVETEXT=DMSG
        . . ELSE  SET ABOVETEXT=ABOVETEXT_" AND "_DMSG
        . SET MSG="ABOVE GOAL ("_ABOVETEXT_" PRESSURE HIGH)"
        SET TMGRESULT="BP Control Status: "_MSG
        QUIT TMGRESULT
        ;"
LASTBPHI(TMGDFN,TEST,DATE,DATA,TEXT) ;
        ;"Determine if the last BP was high (Over 150 sys or 90 dia) and
        ;"they do NOT have a HTN topic
        NEW WHY SET WHY="[WHY] "
        SET TEST=0,DATE=0
        IF ($$PTHASTOP(TMGDFN,"HYPERTENSION")=1)!($$PTHASTOP(TMGDFN,"HTN")=1) QUIT WHY
        NEW SBP,DBP
        NEW LASTBP SET LASTBP=$$TREND^TMGGMRV1(TMGDFN,"T","BP",1,"")
        SET SBP=+$P(LASTBP,"/",1),DBP=+$P(LASTBP,"/",2)
        IF (SBP>149)!(DBP>89) DO
        . SET TEST=1,DATE=$$TODAY^TMGDATE
        . SET WHY="[WHY] BP is "_LASTBP_" above threshold (150/90)"
        QUIT WHY
        ;"
HTNGOAL(TMGDFN,GOAL)  ;"Determine HTN Goal based on JNC8
        ;"  TEST IS AS FOLLOWS:
        ;"                      PATIENT
        ;"                         |
        ;"            ---------------------------
        ;"           |                           |
        ;"      No DM or CKD                 DM or CKD
        ;"           |                           |
        ;"   -----------------          ------------------
        ;"  |                 |        |                  |
        ;" >59 yrs old     <60 yrs    DM,no CKD      CKD, DM or no
        ;"  |                 |        |                  |
        ;" <150/90         <140/90   <140/90           <140/90
        ;"   
        NEW TMGRESULT SET TMGRESULT=""
        NEW WHY SET WHY=""
        NEW DM,CKD,DATE
        DO DMMEDS(TMGDFN,.DM,.TEST)
        DO PTHASCKD(TMGDFN,.CKD,.TEST)
        NEW AGE K VADM SET AGE=$$AGE^TIULO(TMGDFN)
        IF (DM=1)!(CKD=1) DO
        . IF DM=1 SET WHY=WHY_"DM"
        . ELSE  SET WHY=WHY_"No DM"
        . IF CKD=1 SET WHY=WHY_", CKD"
        . ELSE  SET WHY=WHY_", No CKD"
        . SET GOAL="<140/90"
        ELSE  DO
        . SET WHY=WHY_"No DM, No CKD"
        . IF AGE<60 DO
        . . SET GOAL="<140/90"
        . ELSE  DO
        . . SET GOAL="<150/90"
        SET WHY=WHY_", Age: "_AGE
        SET TMGRESULT="Guideline BP Goal: "_GOAL_" (JNC8) ("_WHY_")"
        QUIT TMGRESULT
        ;"
LIPIDMED(TMGDFN,TEST,DATE,DATA,TEXT) ;
        ;"Purpose: Determine if patient is on lipid medication
        ;"Input: TMGDFN -- the patient IEN
        ;"       TEST -- AN OUT PARAMETER.  The logical value of the test:
        ;                1=true, 0=false
        ;"               Also an IN PARAMETER.  Any value for COMPUTED
        ;"FINDING PARAMETER will be passed in here.
        ;"       DATE -- AN OUT PARAMETER.  Date of finding.
        ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
        ;"       TEXT -- Text to be display in the Clinical Maintenance
        ;"Output.  Optional.
        ;"Results: none
        SET TEST=0
        SET DATE=0
        ;"//kt NEW X DO NOW^%DTC
        ;"//kt NOTE: Something downstream was killing X, so using different var name
        NEW ADT SET ADT=$$NOW^XLFDT\1
        ;"//kt NEW TMGRESULT SET TMGRESULT=$$ONLIPDTX^TMGC0QT4(TMGDFN,X)
        NEW TMGRESULT SET TMGRESULT=$$ONLIPDTX^TMGC0QT4(TMGDFN,ADT)
        IF TMGRESULT=1 DO
        . SET TEST=1
        . ;"//kt SET DATE=X   <--- was getting undefined error here.  
        . SET DATE=ADT
        QUIT
        ;"
DMMEDS(TMGDFN,TEST,DATE,DATA,TEXT) ;
        ;"Purpose: Determine if patient is on DM medication
        ;"Input: TMGDFN -- the patient IEN
        ;"       TEST -- AN OUT PARAMETER.  The logical value of the test:
        ;                1=true, 0=false
        ;"               Also an IN PARAMETER.  Any value for COMPUTED
        ; FINDING PARAMETER will be passed in here.
        ;"       DATE -- AN OUT PARAMETER.  Date of finding.
        ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
        ;"       TEXT -- Text to be display in the Clinical Maintenance
        ;"Output.  Optional.
        ;"Results: none
        SET TEST=0
        SET DATE=0
        NEW X DO NOW^%DTC
        NEW TMGRESULT SET TMGRESULT=$$ONDMTX^TMGC0QT4(TMGDFN,X)
        IF TMGRESULT=1 DO
        . SET TEST=1
        . SET DATE=$$TODAY^TMGDATE
        QUIT
        ;"
PTOWBMI(TMGDFN,TEST,DATE,DATA,TEXT) ;
        ;"Purpose: Determine if patient's BMI is overweight
        ;"Input: TMGDFN -- the patient IEN
        ;"       TEST -- AN OUT PARAMETER.  The logical value of the test:
        ;                1=true, 0=false
        ;"               Also an IN PARAMETER.  Any value for COMPUTED
        ; FINDING PARAMETER will be passed in here.
        ;"       DATE -- AN OUT PARAMETER.  Date of finding.
        ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
        ;"       TEXT -- Text to be display in the Clinical Maintenance
        ;"Output.  Optional.
        ;"Results: none
        SET TEST=0
        SET DATE=0
        NEW X DO NOW^%DTC
        NEW BMI SET BMI=$$ONEVITAL^TMGTIUOJ(+$GET(TMGDFN),.TIU,"BMI")
        IF BMI>25 DO
        . SET TEST=1
        . SET DATE=$$TODAY^TMGDATE
        QUIT
        ;"
HASPNEUM(TMGDFN,TEST,DATE,DATA,TEXT)  ;"
        ;"PURPOSE: WILL BE TRUE IF THE PATIENT HAS HAD A PNEUMOCOCCAL
        ;"         VACCINE IN THE LAST 12 MONTHS
        SET TEST=0,DATE=0
        NEW IMMLIST SET IMMLIST="^19^96^132^"
        NEW IDX SET IDX=0
        FOR  SET IDX=$O(^AUPNVIMM("C",TMGDFN,IDX)) QUIT:IDX'>0  DO
        . NEW IMMIEN SET IMMIEN="^"_$P($G(^AUPNVIMM(IDX,0)),"^",1)_"^"
        . IF IMMLIST'[IMMIEN QUIT
        . NEW VSTIEN SET VSTIEN=$P($G(^AUPNVIMM(IDX,0)),"^",3)
        . NEW THISDATE SET THISDATE=$PIECE($GET(^AUPNVSIT(VSTIEN,0)),"^",1)
        . NEW X1,X2,X
        . ;"SET X1=THISDATE,X2=$$TODAY^TMGDATE
        . SET X1=$$TODAY^TMGDATE,X2=THISDATE
        . DO ^%DTC
        . IF X<365 DO
        . . SET TEST=1
        . . SET DATE=$$TODAY^TMGDATE
        QUIT
        ;"
TEST  ;"
        NEW I SET I=0
        FOR  SET I=$O(^DPT(I)) QUIT:I'>0  DO
        . NEW TEST,DATE
        . DO PNLST5YR(I,.TEST,.DATE)
        . IF TEST=1 DO
        . . WRITE !,$P($G(^DPT(I,0)),"^",1),!
        QUIT
        ;"
PNLST5YR(TMGDFN,TEST,DATE,DATA,TEXT)  ;"
        ;"PURPOSE: THIS COMPUTED FINDING  
        ;"WILL BE TRUE IF IS OKAY FOR PATIENT TO GET P-20 (BASED ON PREVIOUS
        ;"         PNEUMOCOCCAL VACCINES). THIS TAKES INTO ACCOUNT P-23 AND P-13.
        ;"         IT DOES NOT TAKE INTO ACCOUNT P15 OR P20, AS THEY SHOULD BE LISTED
        ;"         IN THE REMINDER'S RESOLUTION LOGIC
        ;"  THE LOGIC, AS GIVEN FROM IMMUNIZE.ORG IS:
        ;"    IF THE PATIENT HAS HAD P-13 (AT ANY AGE) AND P-23 BEFORE THE AGE OF 65
        ;"      THEY SHOULD WAIT 5 YEARS SINCE WHICHEVER ONE WAS MORE RECENT TO GET THE P-20
        SET TEST=0,DATE=0
        NEW DOB SET DOB=$P($G(^DPT(TMGDFN,0)),"^",3)
        NEW P23IMM,P23DAYS
        SET P23IMM=19,P23DAYS=999999
        NEW P13IMM,P13DAYS
        SET P13IMM=132,P13DAYS=999999
        NEW IMMLIST SET IMMLIST="^19^96^132^"
        NEW IDX SET IDX=0
        ;"
        ;"GO THROUGH PATIENT'S IMMUNIZATIONS, TO GET MOST RECENT P13 AND P23 ADMIN DATES
        FOR  SET IDX=$O(^AUPNVIMM("C",TMGDFN,IDX)) QUIT:IDX'>0  DO
        . NEW IMMIEN SET IMMIEN=$P($G(^AUPNVIMM(IDX,0)),"^",1)
        . IF (IMMIEN'=P23IMM)&(IMMIEN'=P13IMM)&(IMMIEN'=96) QUIT
        . NEW VSTIEN SET VSTIEN=$P($G(^AUPNVIMM(IDX,0)),"^",3)
        . NEW THISDATE SET THISDATE=$PIECE($GET(^AUPNVSIT(VSTIEN,0)),"^",1)
        . NEW X1,X2,X
        . SET X1=$$TODAY^TMGDATE,X2=THISDATE
        . DO ^%DTC
        . IF (IMMIEN=P23IMM)!(IMMIEN=96) DO
        . . NEW AGE SET AGE=$$DAYSDIFF^TMGDATE(DOB,THISDATE)
        . . SET AGE=$P(AGE/365,".",1)
        . . ;"IGNORE IF GIVEN AFTER 65
        . . ;"IF AGE>64 QUIT
        . . IF X<+P23DAYS SET P23DAYS=X
        . ELSE  IF IMMIEN=P13IMM DO
        . . IF X<+P13DAYS SET P13DAYS=X
        IF (P23DAYS=999999)&(P13DAYS=999999) QUIT  ;"NOT FOUND
        ;" NEW CODE HERE 9/17/24
        NEW LASTPNU SET LASTPNU=$S(P23DAYS<P13DAYS:P23DAYS,P13DAYS<P23DAYS:P13DAYS,P13DAYS=P23DAYS:P23DAYS)
        IF LASTPNU<1825 DO
        . SET TEST=1
        . SET DATE=$$TODAY^TMGDATE
        QUIT
        ;"
        
        ;"ORIGINAL CODE BELOW     9/17/24
        IF (P23DAYS>0)&(P13DAYS>0) DO
        . ;"DETERMINE WHICH ONE WAS MORE RECENT
        . ;"WRITE "ISSUE: ",P23DAYS,"-",P13DAYS,! QUIT
        . NEW LASTPNU SET LASTPNU=$S(P23DAYS<P13DAYS:P23DAYS,P13DAYS<P23DAYS:P13DAYS,P13DAYS=P23DAYS:P23DAYS)
        . ;"IF DAY COUNT WAS LESS THAN 5 YEARS (1825 DAYS) THEN REPORT TRUE.
        . ;" THIS WILL BE USED IN THE NOT LOGIC OF THE COHORT, WHICH WILL TRUN THE
        . ;" REMINDER OFF
        . IF LASTPNU<1825 DO
        . . SET TEST=1
        . . SET DATE=$$TODAY^TMGDATE
        QUIT
        ;"        
HASSHGIX(TMGDFN,TEST,DATE,DATA,TEXT)  ;"
        ;"PURPOSE: Check to see if patient has had 2 or more Shingrix
        SET TEST=0,DATE=0
        NEW IDX SET IDX=0
        NEW COUNT SET COUNT=0
        FOR  SET IDX=$O(^AUPNVIMM("C",TMGDFN,IDX)) QUIT:IDX'>0  DO
        . NEW IMMIEN SET IMMIEN=$P($G(^AUPNVIMM(IDX,0)),"^",1)
        . IF IMMIEN'=612007 QUIT
        . SET COUNT=COUNT+1
        IF COUNT>1 SET TEST=1,DATE=$$TODAY^TMGDATE
        QUIT
        ;"
NEEDSP23(TMGDFN,TEST,DATE,DATA,TEXT,WHY)  ;"  
        ;"PURPOSE: WILL BE TRUE IF THE PATIENT IS BETWEEN 19-64 AND DIABETIC
        ;"         OR 65+
        ;"         WHY (OPTIONAL) - BY REFERENCE, RETURNS REASON WHY
        SET TEST=0,DATE=0
        NEW AGE K VADM SET AGE=$$AGE^TIULO(TMGDFN)
        SET WHY=$G(WHY)
        IF AGE>64 DO
        . SET TEST=1
        . SET DATE=$$TODAY^TMGDATE
        . SET WHY="Patient is over 65 ("_AGE_")."
        ELSE  IF AGE>18 DO
        . NEW DMTEST,DMDATE,DMDATA,DMTEXT
        . DO PTHASDM(TMGDFN,.DMTEST,.DMDATE,.DMDATA,.DMTEXT)
        . IF DMTEST=1 DO
        . . SET TEST=1
        . . SET DATE=$$TODAY^TMGDATE
        . . SET WHY="Patient is between 18 and 65 ("_AGE_") and is diabetic"
        . NEW COPDTEST,COPDDATE,COPDDATA,COPDTEXT
        . DO PTHSCOPD(TMGDFN,.COPDTEST,.COPDDATE,.COPDDATA,.COPDTEXT)  ;
        . IF COPDTEST=1 DO
        . . SET TEST=1
        . . SET DATE=$$TODAY^TMGDATE
        . . IF WHY="" DO
        . . . SET WHY="Patient is between 18 and 65 ("_AGE_") and has COPD"
        . . ELSE  DO
        . . . SET WHY=WHY_" and has COPD"
        . NEW SMOKETEST
        . DO CURSMOKR(TMGDFN,.SMOKETEST)  ;
        . IF SMOKETEST=1 DO
        . . SET TEST=1
        . . SET DATE=$$TODAY^TMGDATE
        . . IF WHY="" DO
        . . . SET WHY="Patient is between 18 and 65 ("_AGE_") and is a current smoker"
        . . ELSE  DO
        . . . SET WHY=WHY_" and is a current smoker"
        IF WHY'="" SET WHY=WHY_"."
        QUIT
P23RESVL(TMGDFN,TEST,DATE,DATA,TEXT,WHY)  ;"
        ;"PURPOSE: WILL BE TRUE IF THE PATIENT'S P23 IS RESOLVED.
        ;"         CONSIDERED RESOLVED IF:
        ;"               * IT WAS RECEIVED WHEN PATIENT WAS OVER 65
        ;"               * PATIENT IS UNDER 65 AND HAS RECEIVED ONE
        ;"               * PATIENT IS OVER 65, AND RECEIVED ONE WHEN
        ;"                    UNDER 65, AND IT HAS BEEN LESS THAN 5 YEARS 
        ;"                    SINCE ADMINISTRATION
        SET TEST=0,DATE=0
        SET WHY=$G(WHY)
        NEW AGE K VADM SET AGE=$$AGE^TIULO(TMGDFN)
        NEW ADMINARR
        DO P23ADMIN(TMGDFN,.ADMINARR)
        IF AGE>64.99 DO
        . NEW THISAGE SET THISAGE=0
        . FOR  SET THISAGE=$O(ADMINARR(THISAGE)) QUIT:THISAGE'>0  DO
        . . IF THISAGE>64.99 DO
        . . . SET TEST=1
        . . . SET DATE=$P($G(ADMINARR(THISAGE)),"^",1)
        . . IF THISAGE<65 DO
        . . . NEW YEARSINCE
        . . . SET YEARSINCE=$P($G(ADMINARR(THISAGE)),"^",2)
        . . . IF YEARSINCE<5 DO
        . . . . SET TEST=1
        . . . . SET DATE=$P($G(ADMINARR(THISAGE)),"^",1)
        . . SET WHY=WHY_$C(13,10)_"*At "_THISAGE_" patient received one on "_$$EXTDATE^TMGDATE($P($G(ADMINARR(THISAGE)),"^",1))_"."
        . IF TEST=1 SET WHY="THIS VACCINE REMINDER IS RESOLVED AND NOT DUE."
        ELSE  DO
        . NEW THISAGE SET THISAGE=0
        . FOR  SET THISAGE=$O(ADMINARR(THISAGE)) QUIT:THISAGE'>0  DO
        . . SET TEST=1
        . . SET DATE=$P($G(ADMINARR(THISAGE)),"^",1) 
        . IF TEST=0 SET WHY=WHY_" Patient has not previously had vaccination according to our records."
        QUIT
        ;"
P23ADMIN(TMGDFN,AGEARR)  ;"Purpose: get a listing of ages when P23 was administered
        ;"AGEARR (PASS BY REF):
        ;"          AGEARR(AGE)=DATE ADMINISTERED^AGE WHEN RECEIVED
        NEW IMMLIST SET IMMLIST="^19^96^777003^"
        NEW DOB SET DOB=$P($G(^DPT(TMGDFN,0)),"^",3)
        NEW IDX SET IDX=0
        FOR  SET IDX=$O(^AUPNVIMM("C",TMGDFN,IDX)) QUIT:IDX'>0  DO
        . NEW IMMIEN SET IMMIEN=$P($G(^AUPNVIMM(IDX,0)),"^",1)
        . IF IMMLIST'[IMMIEN QUIT
        . NEW VSTIEN SET VSTIEN=$P($G(^AUPNVIMM(IDX,0)),"^",3)
        . NEW THISDATE SET THISDATE=$PIECE($GET(^AUPNVSIT(VSTIEN,0)),"^",1)
        . ;"GET AGE ADMINISTERED
        . NEW X1,X2,X
        . SET X1=THISDATE,X2=DOB
        . DO ^%DTC
        . NEW AGEATADMIN SET AGEATADMIN=X/365
        . SET AGEATADMIN=$J(AGEATADMIN,"",2)
        . ;"GET TIME SINCE ADMINISTERED
        . SET X1=$$TODAY^TMGDATE,X2=THISDATE
        . DO ^%DTC
        . NEW YRSSINCE SET YRSSINCE=X/365
        . SET YRSSINCE=$J(YRSSINCE,"",2)       
        . SET AGEARR(AGEATADMIN)=THISDATE_"^"_YRSSINCE
        QUIT
        ;"
P23WHY(TMGDFN)  ;"Purpose: To return the reason why P23 is due
        NEW TMGRESULT SET TMGRESULT=""
        NEW TEST,DATE,DATA,TEXT
        DO NEEDSP23(TMGDFN,.TEST,.DATE,.DATA,.TEXT,.TMGRESULT)
        IF TEST=0 GOTO P23DN  DO
        . SET TMGRESULT="NOT DUE"
        DO P23RESVL(TMGDFN,.TEST,.DATE,.DATA,.TEXT,.TMGRESULT)
P23DN
        QUIT TMGRESULT
        ;"
CKDSTAGE(TMGDFN)  ;"Used by TMG CKD STAGE tiu object
        ;"Purpose: to return the CKD stage for the given patient
        IF +$G(TMGDFN)'>0 DO  GOTO CKDDN
        . SET TMGRESULT="NO DFN RECEIVED BY TIU OBJECT"
        NEW TMGRESULT SET TMGRESULT="CKD STAGE = NO CKD STAGE DETERMINED "
        NEW LASTEGFR,RESULTS,DATEARR,DATE,LASTDATE
        DO GETVALS^TMGLRR01(TMGDFN_"^2",5111,.RESULTS)   ;"eGFR_WHITE
        ;"DO GETVALS^TMGLRR01(TMGDFN_"^2",5110,.RESULTS)   ;"eGFR_BLACK
        DO GETVALS^TMGLRR01(TMGDFN_"^2",6028,.RESULTS)   ;"ESTIMATED GFR (OTHER)
        DO GETVALS^TMGLRR01(TMGDFN_"^2",5070,.RESULTS)   ;"GLOMERULAR FILTRATION RATE PANEL
        NEW LABNAME SET LABNAME=""
        FOR  SET LABNAME=$O(RESULTS(LABNAME)) QUIT:LABNAME=""  DO
        . SET DATE=0
        . FOR  SET DATE=$O(RESULTS(LABNAME,DATE)) QUIT:DATE'>0  DO
        . . IF $GET(RESULTS(LABNAME,DATE))="" QUIT
        . . SET DATEARR(DATE)=$GET(RESULTS(LABNAME,DATE))
        SET DATE=9999999,LASTDATE=$O(DATEARR(DATE),-1)
        SET LASTEGFR=+$G(DATEARR(LASTDATE))
        IF LASTEGFR'>0 GOTO CKDDN
        IF LASTEGFR>89 SET TMGRESULT="CKD STAGE = 1. "
        ELSE  IF LASTEGFR>59 SET TMGRESULT="CKD STAGE = 2. "
        ELSE  IF LASTEGFR>44 SET TMGRESULT="CKD STAGE = 3a. "
        ELSE  IF LASTEGFR>29 SET TMGRESULT="CKD STAGE = 3b. "
        ELSE  IF LASTEGFR>14 SET TMGRESULT="CKD STAGE = 4. "
        ELSE  IF LASTEGFR<15 SET TMGRESULT="CKD STAGE = 5. "
        SET TMGRESULT=TMGRESULT_"(LAST EGFR="_LASTEGFR_" ON "_$$EXTDATE^TMGDATE(LASTDATE)_")"
CKDDN   QUIT TMGRESULT
        ;"
CKDTOPIC(TMGDFN,TEST,DATE,DATA,TEXT)  ;"Determines if patient has CKD topic
        SET TEST=0,DATE=0
        IF ($$PTHASTOP(TMGDFN,"CKD")=1)!($$PTHASTOP(TMGDFN,"CHRONIC KIDNEY DISEASE")=1) DO
        . SET TEST=1
        . SET DATE=$$TODAY^TMGDATE
        QUIT
        ;"
ANCCALC1(TMGDFN)  ;"CALCULATE ABSOLUTE NEUTROPHIL COUNT
       NEW TMGRESULT SET TMGRESULT="ANC = Cannot calculate: unknown reason"
       NEW NDT,NVAL,BDT,BVAL,WDT,WVAL
       DO GETLLAB(TMGDFN,5076,.NDT,.NVAL)       
       NEW FAILREASON SET FAILREASON=""
       IF NDT'>0 SET FAILREASON="Neutrophil%"
       SET NDT=$P(NDT,".",1)
       DO GETLLAB(TMGDFN,5926,.BDT,.BVAL)
       ;"IF BDT'>0 DO
       ;". "IF FAILREASON'="" SET FAILREASON=FAILREASON_", "
       ;". "SET FAILREASON=FAILREASON_"Band%"
       SET BDT=$P(BDT,".",1)
       DO GETLLAB(TMGDFN,1,.WDT,.WVAL)
       IF WDT'>0 DO  GOTO ANCDN
       . IF FAILREASON'="" SET FAILREASON=FAILREASON_", "
       . SET FAILREASON=FAILREASON_"WBC"
       IF FAILREASON'="" DO  GOTO ANCDN
       . SET TMGRESULT="ANC = Cannot calculate: No "_FAILREASON_" found"
       SET WDT=$P(WDT,".",1)
       IF NDT'=WDT DO  GOTO ANCDN
       . SET TMGRESULT="ANC = Cannot calculate: Last Neutro%, and WBC not done on same day"
       IF NDT'=BDT SET BVAL=0  ;"IF THE BANDS ARE OLD, SET TO 0
       NEW CALC
       SET CALC=((NVAL+BVAL)*(WVAL*1000))/100
       SET TMGRESULT="ANC (1500-6000) = "_CALC_" ("_$$EXTDATE^TMGDATE(WDT)_")"
ANCDN
       QUIT TMGRESULT
       ;"       
ANCCALC(TMGDFN)  ;"CALCULATE ABSOLUTE NEUTROPHIL COUNT
       NEW TMGRESULT SET TMGRESULT=""
       NEW NDT,NVAL,BDT,BVAL,WDT,WVAL,CALC
       NEW NUM
       FOR NUM=1:1:3  DO
       . DO GETNLAB(TMGDFN,5076,.NDT,.NVAL,NUM)       
       . NEW FAILREASON SET FAILREASON=""
       . ;"IF NDT'>0 SET FAILREASON="Neutrophil%"
       . SET NDT=$P(NDT,".",1)
       . DO GETNLAB(TMGDFN,5926,.BDT,.BVAL,NUM)
       . ;"IF BDT'>0 DO
       . ;". "IF FAILREASON'="" SET FAILREASON=FAILREASON_", "
       . ;". "SET FAILREASON=FAILREASON_"Band%"
       . SET BDT=$P(BDT,".",1)
       . DO GETNLAB(TMGDFN,1,.WDT,.WVAL,NUM)
       . ;"IF WDT'>0 DO  GOTO ANCDN
       . ;". IF FAILREASON'="" SET FAILREASON=FAILREASON_", "
       . ;". SET FAILREASON=FAILREASON_"WBC"
       . ;"IF FAILREASON'="" DO  GOTO ANCDN
       . ;". SET TMGRESULT="ANC = Cannot calculate: No "_FAILREASON_" found"
       . SET WDT=$P(WDT,".",1)
       . ;"IF NDT'=WDT DO  GOTO ANCDN
       . ;". SET TMGRESULT="ANC = Cannot calculate: Last Neutro%, and WBC not done on same day"
       . IF NDT'=BDT SET BVAL=0  ;"IF THE BANDS ARE OLD, SET TO 0
       . IF (NVAL>0)&(WVAL>0) DO
       . . IF TMGRESULT'="" SET TMGRESULT=TMGRESULT_", "
       . . SET CALC=((NVAL+BVAL)*(WVAL*1000))/100
       . . SET CALC=$FN(CALC,",")_" ("_$$EXTDATE^TMGDATE(WDT,1)_")"
       . . SET TMGRESULT=TMGRESULT_CALC
       SET TMGRESULT="ANC (1500-6000) = "_TMGRESULT
ANCDN2
       QUIT TMGRESULT
       ;"       
ALCCALC1(TMGDFN)  ;"CALCULATE ABSOLUTE LYMPHOCYTE COUNT
       NEW TMGRESULT SET TMGRESULT="ALC = Cannot calculate: unknown reason"
       NEW LDT,LVAL,WDT,WVAL
       DO GETLLAB(TMGDFN,5077,.LDT,.LVAL)       
       NEW FAILREASON SET FAILREASON=""
       IF LDT'>0 SET FAILREASON="Lymphocyte%"
       SET LDT=$P(LDT,".",1)
       DO GETLLAB(TMGDFN,1,.WDT,.WVAL)
       IF WDT'>0 DO  GOTO ALCDN
       . IF FAILREASON'="" SET FAILREASON=FAILREASON_", "
       . SET FAILREASON=FAILREASON_"WBC"
       IF FAILREASON'="" DO  GOTO ALCDN
       . SET TMGRESULT="ALC = Cannot calculate: No "_FAILREASON_" found"
       SET WDT=$P(WDT,".",1)
       IF LDT'=WDT DO  GOTO ALCDN
       . SET TMGRESULT="ALC = Cannot calculate: Last Lymph%, and WBC not done on same day"
       NEW CALC
       SET CALC=(WVAL*1000*LVAL)/100
       SET TMGRESULT="ALC  (1000-4000) = "_CALC_" ("_$$EXTDATE^TMGDATE(WDT)_")"
ALCDN
       QUIT TMGRESULT
       ;" 
ALCCALC(TMGDFN)  ;"CALCULATE ABSOLUTE LYMPHOCYTE COUNT
       NEW TMGRESULT SET TMGRESULT=""
       NEW EDT,EVAL,WDT,WVAL,CALC
       NEW NUM
       FOR NUM=1:1:3  DO
       . DO GETNLAB(TMGDFN,5077,.EDT,.EVAL,NUM)       
       . NEW FAILREASON SET FAILREASON=""
       . IF EDT'>0 SET FAILREASON="Eosinophil%"
       . SET EDT=$P(EDT,".",1)
       . DO GETNLAB(TMGDFN,1,.WDT,.WVAL,NUM)
       . ;"IF WDT'>0 DO  GOTO AECDN
       . ;". IF FAILREASON'="" SET FAILREASON=FAILREASON_", "
       . ;". SET FAILREASON=FAILREASON_"WBC"
       . ;"IF FAILREASON'="" DO  GOTO AECDN
       . ;". SET TMGRESULT="AEC = Cannot calculate: No "_FAILREASON_" found"
       . SET WDT=$P(WDT,".",1)
       . ;"IF EDT'=WDT DO  GOTO AECDN
       . ;". SET TMGRESULT="AEC = Cannot calculate: Last Eosinophil%, and WBC not done on same day"
       . ;"NEW CALC
       . IF (WVAL>0)&(EVAL>0) DO
       . . IF TMGRESULT'="" SET TMGRESULT=TMGRESULT_", "
       . . SET CALC=(WVAL*1000*EVAL)/100
       . . SET CALC=$FN(CALC,",")_" ("_$$EXTDATE^TMGDATE(WDT,1)_")"
       . . SET TMGRESULT=TMGRESULT_CALC
       SET TMGRESULT="ALC  (1000-4000) = "_TMGRESULT
ALCDN2
       QUIT TMGRESULT
       ;"
AECCALC1(TMGDFN)  ;"CALCULATE ABSOLUTE EOSINOPHIL COUNT
       NEW TMGRESULT SET TMGRESULT="AEC = Cannot calculate: unknown reason"
       NEW EDT,EVAL,WDT,WVAL
       DO GETLLAB(TMGDFN,5079,.EDT,.EVAL)       
       NEW FAILREASON SET FAILREASON=""
       IF EDT'>0 SET FAILREASON="Eosinophil%"
       SET EDT=$P(EDT,".",1)
       DO GETLLAB(TMGDFN,1,.WDT,.WVAL)
       IF WDT'>0 DO  GOTO AECDN
       . IF FAILREASON'="" SET FAILREASON=FAILREASON_", "
       . SET FAILREASON=FAILREASON_"WBC"
       IF FAILREASON'="" DO  GOTO AECDN
       . SET TMGRESULT="AEC = Cannot calculate: No "_FAILREASON_" found"
       SET WDT=$P(WDT,".",1)
       IF EDT'=WDT DO  GOTO AECDN
       . SET TMGRESULT="AEC = Cannot calculate: Last Eosinophil%, and WBC not done on same day"
       NEW CALC
       SET CALC=(WVAL*1000*EVAL)/100
       SET TMGRESULT="AEC (<500) = "_CALC_" ("_$$EXTDATE^TMGDATE(WDT)_")"
AECDN
       QUIT TMGRESULT
       ;"         
AECCALC(TMGDFN)  ;"CALCULATE ABSOLUTE EOSINOPHIL COUNT
       NEW TMGRESULT SET TMGRESULT=""
       NEW EDT,EVAL,WDT,WVAL,CALC
       NEW NUM
       FOR NUM=1:1:3  DO
       . DO GETNLAB(TMGDFN,5079,.EDT,.EVAL,NUM)       
       . NEW FAILREASON SET FAILREASON=""
       . IF EDT'>0 SET FAILREASON="Eosinophil%"
       . SET EDT=$P(EDT,".",1)
       . DO GETNLAB(TMGDFN,1,.WDT,.WVAL,NUM)
       . ;"IF WDT'>0 DO  GOTO AECDN
       . ;". IF FAILREASON'="" SET FAILREASON=FAILREASON_", "
       . ;". SET FAILREASON=FAILREASON_"WBC"
       . ;"IF FAILREASON'="" DO  GOTO AECDN
       . ;". SET TMGRESULT="AEC = Cannot calculate: No "_FAILREASON_" found"
       . SET WDT=$P(WDT,".",1)
       . ;"IF EDT'=WDT DO  GOTO AECDN
       . ;". SET TMGRESULT="AEC = Cannot calculate: Last Eosinophil%, and WBC not done on same day"
       . ;"NEW CALC
       . IF (WVAL>0)&(EVAL>0) DO
       . . IF TMGRESULT'="" SET TMGRESULT=TMGRESULT_", "
       . . SET CALC=(WVAL*1000*EVAL)/100
       . . SET CALC=$FN(CALC,",")_" ("_$$EXTDATE^TMGDATE(WDT,1)_")"
       . . SET TMGRESULT=TMGRESULT_CALC
       SET TMGRESULT="AEC (<500) = "_TMGRESULT      
AECDN2
       QUIT TMGRESULT
       ;"          
AMCCALC(TMGDFN)  ;"CALCULATE ABSOLUTE MONOCYTE COUNT
       NEW TMGRESULT SET TMGRESULT=""
       NEW MDT,MVAL,WDT,WVAL,CALC
       NEW NUM
       FOR NUM=1:1:3  DO
       . DO GETNLAB(TMGDFN,5078,.MDT,.MVAL,NUM)       
       . NEW FAILREASON SET FAILREASON=""
       . IF MDT'>0 SET FAILREASON="Monocyte%"
       . SET MDT=$P(MDT,".",1)
       . DO GETNLAB(TMGDFN,1,.WDT,.WVAL,NUM)
       . ;"IF WDT'>0 DO  GOTO AECDN
       . ;". IF FAILREASON'="" SET FAILREASON=FAILREASON_", "
       . ;". SET FAILREASON=FAILREASON_"WBC"
       . ;"IF FAILREASON'="" DO  GOTO AECDN
       . ;". SET TMGRESULT="AMC = Cannot calculate: No "_FAILREASON_" found"
       . SET WDT=$P(WDT,".",1)
       . ;"IF EDT'=WDT DO  GOTO AECDN
       . ;". SET TMGRESULT="AMC = Cannot calculate: Last Monocyte%, and WBC not done on same day"
       . ;"NEW CALC
       . IF (WVAL>0)&(MVAL>0) DO
       . . IF TMGRESULT'="" SET TMGRESULT=TMGRESULT_", "
       . . SET CALC=(WVAL*1000*MVAL)/100
       . . SET CALC=$FN(CALC,",")_" ("_$$EXTDATE^TMGDATE(WDT,1)_")"
       . . SET TMGRESULT=TMGRESULT_CALC
       SET TMGRESULT="AMC (<1000) = "_TMGRESULT      
AMCDN2
       QUIT TMGRESULT
       ;"   
BASOCALC(TMGDFN)  ;"CALCULATE ABSOLUTE BASOPHIL COUNT  
       NEW TMGRESULT SET TMGRESULT="BASO (<300) = Cannot calculate: unknown reason"
       NEW BDT,BVAL,WDT,WVAL
       DO GETLLAB(TMGDFN,5080,.BDT,.BVAL)       
       NEW FAILREASON SET FAILREASON=""
       IF BDT'>0 SET FAILREASON="Basophil %"
       SET BDT=$P(BDT,".",1)
       DO GETLLAB(TMGDFN,1,.WDT,.WVAL)
       IF WDT'>0 DO  GOTO BASODN
       . IF FAILREASON'="" SET FAILREASON=FAILREASON_", "
       . SET FAILREASON=FAILREASON_"WBC"
       IF FAILREASON'="" DO  GOTO BASODN
       . SET TMGRESULT="BASO = Cannot calculate: No "_FAILREASON_" found"
       SET WDT=$P(WDT,".",1)
       IF BDT'=WDT DO  GOTO BASODN
       . SET TMGRESULT="BASO (<300) = Cannot calculate: Last Baso%, and WBC not done on same day"
       NEW CALC
       SET CALC=(WVAL*1000*BVAL)/100
       SET TMGRESULT="BASO (<300) = "_CALC_" ("_$$EXTDATE^TMGDATE(WDT)_")"
BASODN
       QUIT TMGRESULT
       ;"
GETLLAB(TMGDFN,LABIEN,DATE,VALUE)  ;"
       ;"GET THE LAST DATE AND VALUE OF PROVIDED LAB
       ;"DATE AND VALUE ARE PASSED BY REF
       NEW RESULTS
       SET DATE=0,VALUE=0
       DO GETVALS^TMGLRR01(TMGDFN_"^2",LABIEN,.RESULTS)   ;
       NEW LABNAME SET LABNAME="1"
       SET LABNAME=$O(RESULTS(LABNAME))
       SET DATE=+$O(RESULTS(LABNAME,9999999),-1)
       IF DATE>0 SET VALUE=$GET(RESULTS(LABNAME,DATE))
       QUIT
       ;"
GETNLAB(TMGDFN,LABIEN,DATE,VALUE,NUMBACK)  ;"
       ;"GET THE N# (FROM LAST) DATE AND VALUE OF PROVIDED LAB
       ;"DATE AND VALUE ARE PASSED BY REF
       NEW RESULTS
       SET DATE=9999999,VALUE=0
       DO GETVALS^TMGLRR01(TMGDFN_"^2",LABIEN,.RESULTS)   ;
       NEW LABNAME SET LABNAME="1"
       SET LABNAME=$O(RESULTS(LABNAME))
       NEW NUM
       FOR NUM=1:1:NUMBACK  DO
       . SET DATE=+$O(RESULTS(LABNAME,DATE),-1)
       IF DATE>0 SET VALUE=$GET(RESULTS(LABNAME,DATE))
       QUIT
       ;"            
LUPUS(TMGDFN)  ;"RETURN IF PATIENT IS AT RISK OF LUPIS BASED ON MEDS
       ;"As of right now, there is only one med to check for. If more are
       ;"    added then this will have to be rewritten.
       NEW TMGRESULT SET TMGRESULT=""
       NEW TMGMEDLIST,TMGMEDARRAY
       DO MEDLIST^TMGTIUOJ(.TMGMEDLIST,.TMGDFN,.TMGMEDARRAY)  
       IF $DATA(TMGMEDARRAY) DO
       . NEW IDX SET IDX=0
       . FOR  SET IDX=$ORDER(TMGMEDARRAY(IDX)) QUIT:IDX'>0  DO
       . . IF ($$UP^XLFSTR($GET(TMGMEDARRAY(IDX)))["HYDRALAZINE") DO
       . . . SET TMGRESULT="Risk of drug induced lupus: hydralazine"
       QUIT TMGRESULT
       ;"      
PTCKD2(TMGDFN,TEST,DATE,DATA,TEXT)  ;" Determines the patient's CKD
        ;"  stage. If over 2, it is set as true and populates the WHY.
        ;"  I'm not sure where the below function is being used but I don't
        ;"  want to repurpose, just in case
        NEW WHY
        SET TEST=0,DATE=0,WHY=""
        NEW CKDSTAGE,CKDNUM,CKDLINE SET CKDLINE=$$CKDSTAGE(TMGDFN)
        SET CKDSTAGE=$P(CKDLINE," = ",2)
        SET CKDNUM=+$G(CKDSTAGE)
        ;"IF (CKDNUM>3)!(CKDSTAGE["3b") DO ;"changed per Dr Dee 11/22/21
        IF CKDNUM>2 DO
        . SET TEST=1
        . SET DATE=$$TODAY^TMGDATE
        . SET WHY=CKDLINE   
        ELSE  DO
        . NEW LCREATV,LEGFRV,LCREATAR,LEGFRDT,CREATARR,EGFRARR
        . DO GETVALS^TMGLRR01(TMGDFN_"^2",5111,.EGFRARR)   ;"eGFR_WHITE
        . DO GETVALS^TMGLRR01(TMGDFN_"^2",6028,.EGFRARR)   ;"ESTIMATED GFR (OTHER)
        . DO GETVALS^TMGLRR01(TMGDFN_"^2",5070,.EGFRARR)   ;"GLOMERULAR FILTRATION RATE PANEL
        . IF '$D(EGFRARR) DO    ;"PATIENT HAS NOT HAD A EGFR DONE, SO WE WILL REVIEW THE LAST CREATININE
        . . NEW LASTCREAT,DATEARR,DATE,LASTDATE
        . . DO GETVALS^TMGLRR01(TMGDFN_"^2",173,.CREATARR)  ;"CREATININE
        . . NEW LABNAME SET LABNAME=""
        . . FOR  SET LABNAME=$O(CREATARR(LABNAME)) QUIT:LABNAME=""  DO
        . . . SET DATE=0
        . . . FOR  SET DATE=$O(CREATARR(LABNAME,DATE)) QUIT:DATE'>0  DO
        . . . . IF $GET(CREATARR(LABNAME,DATE))="" QUIT
        . . . . SET DATEARR(DATE)=$GET(CREATARR(LABNAME,DATE))
        . . SET DATE=9999999,LASTDATE=$O(DATEARR(DATE),-1)
        . . SET LASTCREAT=$G(DATEARR(LASTDATE))
        . . IF LASTCREAT["H" DO
        . . . SET TEST=1
        . . . SET DATE=LASTDATE
        . . . SET WHY="NO EGFR VALUES FOUND. LAST CREATININE WAS "_LASTCREAT_" ON "_$$EXTDATE^TMGDATE(LASTDATE)_"."
        QUIT WHY
        ;"
PTHASCKD(TMGDFN,TEST,DATE,DATA,TEXT) ;
        ;"Purpose: Determine if patient has CKD
        ;"Input: TMGDFN -- the patient IEN
        ;"       TEST -- AN OUT PARAMETER.  The logical value of the test:
        ;                1=true, 0=false
        ;"               Also an IN PARAMETER.  Any value for COMPUTED
        ; FINDING PARAMETER will be passed in here.
        ;"       DATE -- AN OUT PARAMETER.  Date of finding.
        ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
        ;"       TEXT -- Text to be display in the Clinical Maintenance
        ;"Output.  Optional.
        ;"Results: none
        SET TEST=0
        SET DATE=0
        NEW CKD SET CKD=$$CKDSTAGE(+$G(TMGDFN))        
        IF CKD'["NO CKD STAGE" DO
        . SET TEST=1
        . SET DATE=$$TODAY^TMGDATE
        QUIT
        ;"

PATPICS(TMGDFN,TEST,DATE,DATA,TEXT)  ;
        ;"Purpose: Return when the patient's last picture was
        ;"Input: TMGDFN -- the patient IEN
        ;"       TEST -- AN OUT PARAMETER.  The logical value of the test:
        ;"               1=true, 0=false
        ;"               Also an IN PARAMETER.  Any value for COMPUTED
        ;FINDING PARAMETER will be passed in here.
        ;"       DATE -- AN OUT PARAMETER.  Date of finding.
        ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
        ;"       TEXT -- Text to be display in the Clinical Maintenance
        ;"Output.  Optional.
        ;"Results: none
        NEW PICARRAY,PICIDX
        SET (TEST,DATE,PICIDX)=0
        DO PHOTOS^MAGGTIG(.PICARRAY,TMGDFN)        
        FOR  SET PICIDX=$O(PICARRAY(PICIDX)) QUIT:PICIDX'>0  DO
        . NEW THISDATE SET THISDATE=$P($G(PICARRAY(1)),"^",6)
        . IF THISDATE>DATE DO
        . . SET TEST=1
        . . SET DATE=THISDATE
        QUIT
        ;"
P9COHORT(TMGDFN,NGET,BDT,EDT,NFOUND,TEST,DATE,DATA,TEXT) ;
        ;"Purpose: Return true is patient is scheduled for physical
        ;"         or 1 yr check for date provided
        ;"Input: TMGDFN -- the patient IEN
        ;"       NGET -- the number of findings to search for
        ;"       BDT -- the beginning date and time for the finding search
        ;"       EDT -- the ending date and time for the finding search
        ;"       NFOUND -- the number of findings found in the date range
        ;"                 Should NEVER be larger than NGET,
        ;"                 and SET to 0 IF no true findings are found.
        ;"       TEST(n) -- AN OUT PARAMETER.  The logical value of the test:
        ;"                1=true, 0=false
        ;"               Also an IN PARAMETER.  Any value for COMPUTED
        ;"                FINDING PARAMETER will be passed in here.
        ;"       DATE(n) -- AN OUT PARAMETER.  Date of finding.
        ;"            (NOTE: There is no need to SET the unsubscripted
        ;"                   values of TEST and DATE in a multi-occurrence
        ;"                   computed finding.)
        ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
        ;"       TEXT -- Text to be display in the Clinical Maintenance
        ;"SET TEST=0,DATE=0,
        SET NFOUND=0
        NEW AGE K VADM SET AGE=$$AGE^TIULO(TMGDFN)
        IF AGE<18 QUIT
        NEW APPTSTR SET APPTSTR="PHYSICAL^1 YR CHECK^WELL CPE^OFFICE VISIT^MO CHECK^OFF VISIT^AWV^TH-AWV^ANNUAL^MEDANNUAL"
        NEW APPTREASON,THISDATE,APPTDATE
        ;"SET TODAY=$$TODAY^TMGDATE,APPTDATE=TODAY
        SET THISDATE=$P(EDT,".",1),APPTDATE=THISDATE
        FOR  SET APPTDATE=$O(^TMG(22723,"DT",APPTDATE)) QUIT:(APPTDATE'[THISDATE)!(APPTDATE'>0)  DO
        . NEW APPTDFN SET APPTDFN=0
        . FOR  SET APPTDFN=$O(^TMG(22723,"DT",APPTDATE,APPTDFN)) QUIT:APPTDFN'>0  DO
        . . IF APPTDFN'=TMGDFN QUIT
        . . NEW APPTIEN SET APPTIEN=$O(^TMG(22723,"DT",APPTDATE,APPTDFN,0))
        . . NEW STATUS SET STATUS=$G(^TMG(22723,"DT",APPTDATE,APPTDFN,APPTIEN))
        . . IF STATUS="C" QUIT
        . . NEW REASON SET REASON=$P($G(^TMG(22723,APPTDFN,1,APPTIEN,0)),"^",4)
        . . IF REASON["MO CHECK" SET REASON="MO CHECK"
        . . IF APPTSTR[REASON DO
        . . . IF NGET=NFOUND QUIT
        . . . SET NFOUND=NFOUND+1
        . . . SET TEST(NFOUND)=1
        . . . SET DATE(NFOUND)=APPTDATE
        . . . ;"SET TEST=1
        . . . ;"SET DATE=APPTDATE
        QUIT
        ;"
AWVCOHRT(TMGDFN,NGET,BDT,EDT,NFOUND,TEST,DATE,DATA,TEXT) ;
        ;"Purpose: Return true if patient is scheduled for annual wellness visit
        ;"Input: TMGDFN -- the patient IEN
        ;"       TEST -- AN OUT PARAMETER.  The logical value of the test:
        ;"                1=true, 0=false
        ;"               Also an IN PARAMETER.  Any value for COMPUTED
        ;"                FINDING PARAMETER will be passed in here.
        ;"       DATE -- AN OUT PARAMETER.  Date of finding.
        ;"            (NOTE: There is no need to SET the unsubscripted
        ;"                   values of TEST and DATE in a multi-occurrence
        ;"                   computed finding.)
        ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
        ;"       TEXT -- Text to be display in the Clinical Maintenance
        ;"SET TEST=0,DATE=0,
        SET NFOUND=0
        NEW AGE K VADM SET AGE=$$AGE^TIULO(TMGDFN)
        IF AGE<18 QUIT
        NEW APPTSTR SET APPTSTR="AWV^TH-AWV^ANNUAL^MEDANNUAL^WELMEDPE"
        NEW APPTREASON,THISDATE,APPTDATE
        ;"SET TODAY=$$TODAY^TMGDATE,APPTDATE=TODAY,THISDATE=TODAY
        SET THISDATE=$P(EDT,".",1),APPTDATE=THISDATE
        FOR  SET APPTDATE=$O(^TMG(22723,"DT",APPTDATE)) QUIT:(APPTDATE'[THISDATE)!(APPTDATE'>0)  DO
        . NEW APPTDFN SET APPTDFN=0
        . FOR  SET APPTDFN=$O(^TMG(22723,"DT",APPTDATE,APPTDFN)) QUIT:APPTDFN'>0  DO
        . . IF APPTDFN'=TMGDFN QUIT
        . . NEW APPTIEN SET APPTIEN=$O(^TMG(22723,"DT",APPTDATE,APPTDFN,0))
        . . NEW STATUS SET STATUS=$G(^TMG(22723,"DT",APPTDATE,APPTDFN,APPTIEN))
        . . IF STATUS="C" QUIT
        . . NEW REASON SET REASON=$P($G(^TMG(22723,APPTDFN,1,APPTIEN,0)),"^",4)
        . . IF REASON["MO CHECK" SET REASON="MO CHECK"
        . . IF APPTSTR[REASON DO
        . . . IF NGET=NFOUND QUIT
        . . . SET NFOUND=NFOUND+1
        . . . SET TEST(NFOUND)=1
        . . . SET DATE(NFOUND)=APPTDATE
        . . . ;"SET TEST=1
        . . . ;"SET DATE=APPTDATE
        QUIT
        ;" 
WEL2MCRE(TMGDFN,NGET,BDT,EDT,NFOUND,TEST,DATE,DATA,TEXT) ;
        ;"Purpose: Return true if patient is scheduled for Welcome to Medicare
        ;"Input: TMGDFN -- the patient IEN
        ;"       TEST -- AN OUT PARAMETER.  The logical value of the test:
        ;"                1=true, 0=false
        ;"               Also an IN PARAMETER.  Any value for COMPUTED
        ;"                FINDING PARAMETER will be passed in here.
        ;"       DATE -- AN OUT PARAMETER.  Date of finding.
        ;"            (NOTE: There is no need to SET the unsubscripted
        ;"                   values of TEST and DATE in a multi-occurrence
        ;"                   computed finding.)
        ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
        ;"       TEXT -- Text to be display in the Clinical Maintenance
        ;"SET TEST=0,DATE=0,
        SET NFOUND=0
        NEW AGE K VADM SET AGE=$$AGE^TIULO(TMGDFN)
        IF AGE<18 QUIT
        NEW APPTSTR SET APPTSTR="^WELMEDPE^"
        NEW APPTREASON,THISDATE,APPTDATE
        ;"SET TODAY=$$TODAY^TMGDATE,APPTDATE=TODAY,THISDATE=TODAY
        SET THISDATE=$P(EDT,".",1),APPTDATE=THISDATE
        FOR  SET APPTDATE=$O(^TMG(22723,"DT",APPTDATE)) QUIT:(APPTDATE'[THISDATE)!(APPTDATE'>0)  DO
        . NEW APPTDFN SET APPTDFN=0
        . FOR  SET APPTDFN=$O(^TMG(22723,"DT",APPTDATE,APPTDFN)) QUIT:APPTDFN'>0  DO
        . . IF APPTDFN'=TMGDFN QUIT
        . . NEW APPTIEN SET APPTIEN=$O(^TMG(22723,"DT",APPTDATE,APPTDFN,0))
        . . NEW STATUS SET STATUS=$G(^TMG(22723,"DT",APPTDATE,APPTDFN,APPTIEN))
        . . IF STATUS="C" QUIT
        . . NEW REASON SET REASON=$P($G(^TMG(22723,APPTDFN,1,APPTIEN,0)),"^",4)
        . . IF REASON["MO CHECK" SET REASON="MO CHECK"
        . . IF APPTSTR[REASON DO
        . . . IF NGET=NFOUND QUIT
        . . . SET NFOUND=NFOUND+1
        . . . SET TEST(NFOUND)=1
        . . . SET DATE(NFOUND)=APPTDATE
        . . . ;"SET TEST=1
        . . . ;"SET DATE=APPTDATE
        QUIT
        ;"          
AWVORCPE(TMGDFN,NGET,BDT,EDT,NFOUND,TEST,DATE,DATA,TEXT) ;
        ;"Purpose: Return true if patient is scheduled for annual wellness visit or CPE
        ;"Input: TMGDFN -- the patient IEN
        ;"       TEST -- AN OUT PARAMETER.  The logical value of the test:
        ;"                1=true, 0=false
        ;"               Also an IN PARAMETER.  Any value for COMPUTED
        ;"                FINDING PARAMETER will be passed in here.
        ;"       DATE -- AN OUT PARAMETER.  Date of finding.
        ;"            (NOTE: There is no need to SET the unsubscripted
        ;"                   values of TEST and DATE in a multi-occurrence
        ;"                   computed finding.)
        ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
        ;"       TEXT -- Text to be display in the Clinical Maintenance
        ;"SET TEST=0,DATE=0,
        SET NFOUND=0
        NEW AGE K VADM SET AGE=$$AGE^TIULO(TMGDFN)
        IF AGE<18 QUIT
        NEW APPTSTR SET APPTSTR="AWV^TH-AWV^ANNUAL^MEDANNUAL^PHYSICAL^1 YR CHECK^WELL CPE^WELMEDPE^"
        NEW APPTREASON,THISDATE,APPTDATE
        ;"SET TODAY=$$TODAY^TMGDATE,APPTDATE=TODAY,THISDATE=TODAY
        SET THISDATE=$P(EDT,".",1),APPTDATE=THISDATE
        FOR  SET APPTDATE=$O(^TMG(22723,"DT",APPTDATE)) QUIT:(APPTDATE'[THISDATE)!(APPTDATE'>0)  DO
        . NEW APPTDFN SET APPTDFN=0
        . FOR  SET APPTDFN=$O(^TMG(22723,"DT",APPTDATE,APPTDFN)) QUIT:APPTDFN'>0  DO
        . . IF APPTDFN'=TMGDFN QUIT
        . . NEW APPTIEN SET APPTIEN=$O(^TMG(22723,"DT",APPTDATE,APPTDFN,0))
        . . NEW STATUS SET STATUS=$G(^TMG(22723,"DT",APPTDATE,APPTDFN,APPTIEN))
        . . IF STATUS="C" QUIT
        . . NEW REASON SET REASON=$P($G(^TMG(22723,APPTDFN,1,APPTIEN,0)),"^",4)
        . . IF REASON["MO CHECK" SET REASON="MO CHECK"
        . . IF APPTSTR[REASON DO
        . . . IF NGET=NFOUND QUIT
        . . . SET NFOUND=NFOUND+1
        . . . SET TEST(NFOUND)=1
        . . . SET DATE(NFOUND)=APPTDATE
        . . . ;"SET TEST=1
        . . . ;"SET DATE=APPTDATE
        QUIT
        ;"      
CPEVISIT(TMGDFN,TEST,DATE,DATA,TEXT)  ;"
        ;"Purpose: Return true if patient is scheduled for CPE
        ;"Input: TMGDFN -- the patient IEN
        ;"       TEST -- AN OUT PARAMETER.  The logical value of the test:
        ;"                1=true, 0=false
        ;"               Also an IN PARAMETER.  Any value for COMPUTED
        ;"                FINDING PARAMETER will be passed in here.
        ;"       DATE -- AN OUT PARAMETER.  Date of finding.
        ;"            (NOTE: There is no need to SET the unsubscripted
        ;"                   values of TEST and DATE in a multi-occurrence
        ;"                   computed finding.)
        ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
        ;"       TEXT -- Text to be display in the Clinical Maintenance
        ;"SET TEST=0,DATE=0,
        NEW AGE K VADM SET AGE=$$AGE^TIULO(TMGDFN)
        IF AGE<18 QUIT
        NEW APPTSTR SET APPTSTR="^PHYSICAL^1 YR CHECK^WELL CPE^^"
        NEW APPTREASON,THISDATE,APPTDATE,TODAY
        SET TODAY=$$TODAY^TMGDATE,APPTDATE=TODAY,THISDATE=TODAY
        SET THISDATE=$P(EDT,".",1),APPTDATE=THISDATE
        FOR  SET APPTDATE=$O(^TMG(22723,"DT",APPTDATE)) QUIT:(APPTDATE'[THISDATE)!(APPTDATE'>0)  DO
        . NEW APPTDFN SET APPTDFN=0
        . FOR  SET APPTDFN=$O(^TMG(22723,"DT",APPTDATE,APPTDFN)) QUIT:APPTDFN'>0  DO
        . . IF APPTDFN'=TMGDFN QUIT
        . . NEW APPTIEN SET APPTIEN=$O(^TMG(22723,"DT",APPTDATE,APPTDFN,0))
        . . NEW STATUS SET STATUS=$G(^TMG(22723,"DT",APPTDATE,APPTDFN,APPTIEN))
        . . IF STATUS="C" QUIT
        . . NEW REASON SET REASON=$P($G(^TMG(22723,APPTDFN,1,APPTIEN,0)),"^",4)
        . . IF REASON["MO CHECK" SET REASON="MO CHECK"
        . . IF APPTSTR[REASON DO
        . . . SET TEST=1
        . . . SET DATE=APPTDATE
        QUIT
        ;"           
P9RESOLV(TMGDFN,TEST,DATE,DATA,TEXT)  ;
        ;"Purpose: Resolution logic for PHQ-9 reminder
        ;"Input: TMGDFN -- the patient IEN
        ;"       TEST -- AN OUT PARAMETER.  The logical value of the test:
        ;"               1=true, 0=false
        ;"               Also an IN PARAMETER.  Any value for COMPUTED
        ;FINDING PARAMETER will be passed in here.
        ;"       DATE -- AN OUT PARAMETER.  Date of finding.
        ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
        ;"       TEXT -- Text to be display in the Clinical Maintenance
        ;"Output.  Optional.
        ;"Results: none
        SET TEST=0,DATE=0
        ;"HERE WE MAY WANT TO TEST FOR MENTAL HEALTH SCREENING NOTE TITLE
        QUIT
        ;"
MCRESOLV(TMGDFN,TEST,DATE,DATA,TEXT)  ;
        ;"Purpose: Resolution logic for mini cog reminder
        ;"Input: TMGDFN -- the patient IEN
        ;"       TEST -- AN OUT PARAMETER.  The logical value of the test:
        ;"               1=true, 0=false
        ;"               Also an IN PARAMETER.  Any value for COMPUTED
        ;FINDING PARAMETER will be passed in here.
        ;"       DATE -- AN OUT PARAMETER.  Date of finding.
        ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
        ;"       TEXT -- Text to be display in the Clinical Maintenance
        ;"Output.  Optional.
        ;"Results: none
        SET TEST=0,DATE=0
        ;"HERE WE MAY WANT TO TEST FOR MEMORY SCREENING NOTE TITLE
        QUIT
        ;"
NEGHPV(TMGDFN,TEST,DATE,DATA,TEXT,WHY)  ;"
        ;"PURPOSE: WILL BE TRUE IF THE PATIENT'S LAST HPV AND PAP 
        ;"         ARE NEGATIVE. DATE WILL BE THE OLDER OF THE 2
        SET TEST=0,DATE=0
        DO GETVALS^TMGLRR01(TMGDFN_"^2",5182,.RESULTS)   ;Pap test thin prep
        DO GETVALS^TMGLRR01(TMGDFN_"^2",5070,.RESULTS)   ;
        NEW LABNAME SET LABNAME=""
        ;"WAS THIS EVER FINISHED?  FOR  SET LABNAME=$O(RESULTS(LABNAME)) QUIT:LABNAME=""  DO
        
        QUIT
        ;"
MRFNDN(TMGDFN,HFARRAY)  ;"FIND MOST RECENT HF NAME
        ;"This returns name^date
        NEW DATE SET DATE=0
        NEW TMGRESULT SET TMGRESULT=""
        NEW HFNAME SET HFNAME=""
        FOR  SET HFNAME=$O(HFARRAY(HFNAME)) QUIT:HFNAME=""  DO
        . NEW HFIEN SET HFIEN=$G(HFARRAY(HFNAME))
        . NEW TEMPDATE SET TEMPDATE=$$MRDFN^TMGPXR03(TMGDFN,HFIEN)
        . IF TEMPDATE>DATE DO
        . . SET TMGRESULT=HFNAME
        . . SET DATE=TEMPDATE
        IF TMGRESULT'="" SET TMGRESULT=TMGRESULT_"^"_DATE
        QUIT TMGRESULT
        ;"
GETEGDFU(TMGDFN,TEST,DATE,DATA,TEXT)  ;"  
        ;"GERDMEDS(TMGDFN,TEST,DATE,DATA,TEXT) ;
        ;"Purpose: Determine if patient is on GERD medication
        ;"Input: TMGDFN -- the patient IEN
        ;"       TEST -- AN OUT PARAMETER.  The logical value of the test:
        ;"               1=true, 0=false
        ;"               Also an IN PARAMETER.  Any value for COMPUTED
        ;" FINDING PARAMETER will be passed in here.
        ;"       DATE -- AN OUT PARAMETER.  Date of finding.
        ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
        ;"       TEXT -- Text to be display in the Clinical Maintenance
        ;"Output.  Optional.
        ;"Results: WHY - returns why HTNMEDS is true 
        SET TEST=0
        SET DATE=0
        NEW WHY SET WHY="Patient is NOT on GERD meds and does NOT have a GERD topic"
        NEW X DO NOW^%DTC
        NEW TMGRESULT,MEDARR SET TMGRESULT=$$ONGERDTX^TMGC0QT4(TMGDFN,X,.MEDARR)
        NEW MED SET MED=""
        IF $D(MEDARR) DO
        . SET WHY=""
        . FOR  SET MED=$O(MEDARR(MED)) QUIT:MED=""  DO
        . . IF WHY'="" SET WHY=WHY_", "
        . . SET WHY=WHY_MED_$C(13,10)
        . SET WHY="PATIENT IS ON THESE GERD MEDS: "_WHY
        IF TMGRESULT=1 DO
        . SET TEST=1
        . SET DATE=$$TODAY^TMGDATE
        QUIT  ;"DON'T PASS BACK WHY AT THIS TIME
        ;"    
        ;"BELOW IS THE PREVIOUS COHORT. MAY BE USEFUL LATER
        ;"This computed finding will determine if the patient should be in
        ;"the cohort by 
        ;" 1) Has an EGD been done prior
        ;" 2) Is there a FU factor (that isn't "No Followup")
        ;" 3) Is there a frequency for the reminder
        ;" 4) Is the date of the FU Factor AFTER the last EGD
        ;" If all of these are True, then the patient will be in the 
        ;"    cohort and the HFs in the Utility will determine the 
        ;"    reminder frequency
        SET TEST=0,DATE=0  ;"Default to false
        NEW MRD SET MRD=$$MRDFN^TMGPXR03(TMGDFN,2378)
        IF MRD=0 QUIT  ;"If not previously done, don't include
        ;"
        NEW FACTORS DO LOADHFAR^TMGPXR03("TMG FU EGD",.FACTORS)
        NEW NAME,HFDATE SET NAME=$$MRFNDN(TMGDFN,.FACTORS) ;"Name of most recent finding.
        SET HFDATE=$P(NAME,"^",2),NAME=$P(NAME,"^",1)
        IF NAME="" QUIT  ;"If no FU factor, don't include
        IF NAME["NO FOLLOWUP" QUIT  ;"If last one is No FU, don't include
        IF MRD>HFDATE QUIT  ;"If done AFTER the last FU HF, don't include
        ;"        
        NEW YR,MO,DAY DO INTRVLST^TMGPXRF1(NAME,4,.YR,.MO,.DAY) ;"FU interval from str
        NEW FREQ SET FREQ=$SELECT((YR>0):YR_"Y",(MO>0):MO_"M",(DAY>0):DAY_"D",1:"")
        NEW DUETF SET DUETF=0
        IF FREQ["Y" DO
        . SET DUETF=+$G(FREQ)*365
        ELSE  IF FREQ["M" DO
        . SET DUETF=+$G(FREQ)*30
        IF DUETF=0 QUIT  ;"If no due time frame, don't include
        ;"     
        NEW DUEDATE,X,X1,X2
        SET X1=MRD,X2=+$GET(DUETF)
        DO C^%DTC
        SET DUEDATE=X
        IF DUEDATE>0 DO  ;"If 
        . SET TEST=1
        . SET DATE=MRD
        QUIT
        ;"
GETCTFU(TMGDFN,TEST,DATE,DATA,TEXT)  ;"
        ;"This computed finding will determine if the patient should be in
        ;"the cohort by
        ;" 1) Has a CT been done prior
        ;" 2) Is there a FU factor (that isn't "No Followup")
        ;" 3) Is there a frequency for the reminder
        ;" 4) Is the date of the FU Factor AFTER the last CT
        ;" If all of these are True, then the patient will be in the
        ;"    cohort and the HFs in the Utility will determine the
        ;"    reminder frequency
        SET TEST=0,DATE=0  ;"Default to false
        NEW MRD SET MRD=$$MRDFN^TMGPXR03(TMGDFN,2383)
        IF MRD=0 QUIT  ;"If not previously done, don't include
        ;"
        NEW FACTORS DO LOADHFAR^TMGPXR03("TMG FU CT",.FACTORS)
        NEW NAME,HFDATE SET NAME=$$MRFNDN(TMGDFN,.FACTORS) ;"Name of most recent finding.
        SET HFDATE=$P(NAME,"^",2),NAME=$P(NAME,"^",1)
        IF NAME="" QUIT  ;"If no FU factor, don't include
        IF NAME["NO FOLLOWUP" QUIT  ;"If last one is No FU, don't include
        IF MRD>HFDATE QUIT  ;"If done AFTER the last FU HF, don't include
        ;"
        NEW YR,MO,DAY DO INTRVLST^TMGPXRF1(NAME,4,.YR,.MO,.DAY) ;"FU interval from str
        NEW FREQ SET FREQ=$SELECT((YR>0):YR_"Y",(MO>0):MO_"M",(DAY>0):DAY_"D",1:"")
        NEW DUETF SET DUETF=0
        IF FREQ["Y" DO
        . SET DUETF=+$G(FREQ)*365
        ELSE  IF FREQ["M" DO
        . SET DUETF=+$G(FREQ)*30
        IF DUETF=0 QUIT  ;"If no due time frame, don't include
        ;"
        NEW DUEDATE,X,X1,X2
        SET X1=MRD,X2=+$GET(DUETF)
        DO C^%DTC
        SET DUEDATE=X
        IF DUEDATE>0 DO  ;"If
        . SET TEST=1
        . SET DATE=MRD
        QUIT

        ;"
PSATARGT(TMGDFN)  ;
        ;"This TIU Object will return the normal range for the given
        ;"patient based on his age.
        ;"Guidelines came from UpToDate
        ;" <50    0-2.5
        ;" 50-59  0-3.5
        ;" 60-69  0-4.5
        ;" 70-79  0-6.5
        NEW TMGRESULT
        NEW AGE K VADM SET AGE=$$AGE^TIULO(TMGDFN)
        NEW THRESHOLD SET THRESHOLD=0
        IF AGE<50 SET THRESHOLD=2.5
        ELSE  IF AGE<60 SET THRESHOLD=3.5
        ELSE  IF AGE<70 SET THRESHOLD=4.5
        ELSE  SET THRESHOLD=6.5
        SET TMGRESULT="PSA NORMAL RANGE: <"_THRESHOLD_" (based on age of "_AGE_")"
        QUIT TMGRESULT
        ;"
LSTPSAHI(TMGDFN,TEST,DATE,DATA,TEXT,SHORT)  ;"
        ;"This computed finding will return true if the patient's
        ;"last PSA was high
        ;"Guidelines came from UpToDate
        ;" <50    0-2.5
        ;" 50-59  0-3.5
        ;" 60-69  0-4.5
        ;" 70-79  0-6.5
        SET SHORT=+$G(SHORT)
     ;"GET LAST PSA RESULT
        NEW RESULTS,LABDATE,LASTDATE,DATEARR,LASTPSA,LABNAME
        SET (TEST,DATE)=0
        NEW WHY
        SET WHY=""  ;"default to blank
        SET LABNAME=0
        DO GETVALS^TMGLRR01(TMGDFN_"^2",5117,.RESULTS)  ;"PSA
        SET LABNAME=$O(RESULTS(LABNAME))       
        SET LASTDATE=$O(RESULTS(LABNAME,9999999),-1)
        IF LASTDATE'>0 GOTO LPSADN  ;"NOT FOUND
        SET LASTPSA=+$G(RESULTS(LABNAME,LASTDATE))
        ;"WRITE "LAST PSA WAS ",LASTPSA," ON ",LASTDATE,!
     ;"DETERMINE DOB AT TIME OF TEST
        NEW DOB,THATAGE,DAYSDIFF
        SET DOB=$P($G(^DPT(TMGDFN,0)),"^",3)
        SET DAYSDIFF=$$DAYSDIFF^TMGDATE(DOB,LASTDATE)
        SET THATAGE=DAYSDIFF/365
        SET THATAGE=THATAGE\1
        ;"WRITE "DOB ",DOB," DAYS DIFF: ",DAYSDIFF," THAT AGE ",THATAGE,!        
     ;"TEST THRESHOLD FOR RESULT
        NEW THRESHOLD SET THRESHOLD=0
        IF THATAGE<50 SET THRESHOLD=2.5
        ELSE  IF THATAGE<60 SET THRESHOLD=3.5
        ELSE  IF THATAGE<70 SET THRESHOLD=4.5
        ELSE  SET THRESHOLD=6.5
        ;"WRITE "THRESHOLD: ",THRESHOLD,!
        IF LASTPSA>THRESHOLD DO
        . SET TEST=1
        . SET DATE=LASTDATE
        . IF SHORT=1 DO
        . . SET WHY="PSA NOTE = "_LASTPSA_" ("_$P($$EXTDATE^TMGDATE(LASTDATE),"@",1)_"), HIGH (Normal should be  <"_THRESHOLD_")"
        . ELSE  DO
        . . SET WHY="Last PSA was done on "_$P($$EXTDATE^TMGDATE(LASTDATE),"@",1)_" with an ABNORMAL value of "_LASTPSA_". His age at the time was "_THATAGE_". It is considered HIGH if above "_THRESHOLD_" for his age group."
        ELSE  DO
        . IF SHORT=1 DO
        . . SET WHY="PSA NOTE = "_LASTPSA_" ("_$P($$EXTDATE^TMGDATE(LASTDATE),"@",1)_"), NORMAL (Is <"_THRESHOLD_")"
        . ELSE  DO
        . . SET WHY="Last PSA value of "_LASTPSA_" on "_$P($$EXTDATE^TMGDATE(LASTDATE),"@",1)_" is NORMAL because it is below "_THRESHOLD_" for his age group."
LPSADN
        ;"WRITE "TEST: ",TEST," DATE: ",DATE,!
       QUIT WHY
        ;"
INRDOSE(TMGDFN,TOTAL)
        NEW COUNT SET COUNT=1
        SET TOTAL=+$G(TOTAL) 
        IF TOTAL'>0 SET TOTAL=3
        NEW TMGRESULT SET TMGRESULT=""
        NEW DATE SET DATE=9999999
        NEW DONE SET DONE=0
        FOR  SET DATE=$O(^ORAM(103,TMGDFN,3,"B",DATE),-1) QUIT:(DATE'>0)!(DONE=1)!(COUNT>TOTAL)  DO
        . IF DATE'>0 SET DONE=1 QUIT
        . NEW ORAMIEN SET ORAMIEN=+$O(^ORAM(103,TMGDFN,3,"B",DATE,0))
        . IF ORAMIEN'>0 SET DONE=1 QUIT
        . NEW ZN SET ZN=$G(^ORAM(103,TMGDFN,3,ORAMIEN,0))
        . NEW DOSE SET DOSE=$P(ZN,"^",6)
        . IF TMGRESULT'="" SET TMGRESULT=TMGRESULT_", "
        . SET TMGRESULT=TMGRESULT_DOSE_" on "_$$EXTDATE^TMGDATE(DATE,1)
        . SET COUNT=COUNT+1
INRDN
        IF TMGRESULT="" SET TMGRESULT="COULD NOT BE DETERMINED"
        SET TMGRESULT="WEEKLY DOSE = "_TMGRESULT
        QUIT TMGRESULT
        ;"
LSTCTORD(TMGDFN)  ;"
        ;"Return date of last low dose Chest CT Ordered
        NEW TMGRESULT,HFARRAY SET TMGRESULT=""
        NEW CTDATE SET CTDATE=$$GETHFDT^TMGPXRU1(.TMGDFN,"TMG LOW DOSE CT ORDERED",.HFARRAY)
        NEW COUNT SET COUNT=0
        NEW DATE SET DATE=9999999
        FOR  SET DATE=$ORDER(HFARRAY(DATE),-1) QUIT:(DATE="")!(COUNT>1)  DO
        . NEW Y
        . SET Y=$E(DATE,4,5)_"/"_$E(DATE,6,7)_"/"_($E(DATE,1,3)+1700)
        . IF TMGRESULT="" DO
        . . SET TMGRESULT=TMGRESULT_Y
        . SET COUNT=COUNT+1
        IF TMGRESULT="" SET TMGRESULT="No ordered HF found."
        SET TMGRESULT="Date last Low Dose CT was ordered = "_TMGRESULT     
        ;"
        ;"GET RAD PROCEDURE DATES
        ;"Pull data from bone density radiology studies
        NEW RADFN SET RADFN=+$$ENSRADFN^TMGRAU01(.TMGDFN)
        NEW LDTESTDATES SET LDTESTDATES=""
        NEW TESTCOUNT SET TESTCOUNT=0
        NEW CCTDATES SET CCTDATES=""
        IF RADFN>0 DO
        . NEW RADDT SET RADDT=0
        . FOR  SET RADDT=$O(^RADPT(RADFN,"DT",RADDT)) QUIT:(RADDT'>0)!(TESTCOUNT>3)  DO
        . . NEW IEN70D03 SET IEN70D03=0
        . . FOR  SET IEN70D03=$O(^RADPT(RADFN,"DT",RADDT,"P",IEN70D03)) QUIT:IEN70D03'>0  DO
        . . . NEW ZN SET ZN=$GET(^RADPT(RADFN,"DT",RADDT,"P",IEN70D03,0))
        . . . NEW PROCIEN SET PROCIEN=$PIECE(ZN,"^",2)
        . . . NEW PROCNAME SET PROCNAME=$P($G(^RAMIS(71,PROCIEN,0)),"^",1)
        . . . IF (PROCNAME["CT CHEST")!(PROCNAME["CT LOW DOSE") DO
        . . . . SET TESTCOUNT=TESTCOUNT+1
        . . . . NEW FMDATE SET FMDATE=9999999-$P(RADDT,".",1)
        . . . . IF PROCNAME["CT CHEST" DO
        . . . . . IF CCTDATES'="" SET CCTDATES=CCTDATES_", "
        . . . . . SET CCTDATES=CCTDATES_$$EXTDATE^TMGDATE(FMDATE,1)
        . . . . IF PROCNAME["CT LOW DOSE" DO
        . . . . . IF LDTESTDATES'="" SET LDTESTDATES=LDTESTDATES_", "
        . . . . . SET LDTESTDATES=LDTESTDATES_$$EXTDATE^TMGDATE(FMDATE,1)
        IF LDTESTDATES'="" SET TMGRESULT=TMGRESULT_$C(13,10)_"  Low Dose CT Dates: "_LDTESTDATES_" [RAD]"
        IF CCTDATES'="" SET TMGRESULT=TMGRESULT_$C(13,10)_"  Chest CT Dates: "_CCTDATES_" [RAD]"
        ;"
        QUIT TMGRESULT 
        ;"
NEGIFOBT(TMGDFN,TEST,DATE,DATA,TEXT)  ;"
        ;"Returns true if the patient has had a negative iFOBT
        ;"this calendar year
        SET TEST=0,DATE=0
        NEW LABNAME,RESULTS,LASTDATE,LASTIFOBT
        SET LABNAME=0
        DO GETVALS^TMGLRR01(TMGDFN_"^2",5602,.RESULTS)  ;"IFOBT
        SET LABNAME=$O(RESULTS(LABNAME))
        SET LASTDATE=$O(RESULTS(LABNAME,9999999),-1)
        IF LASTDATE'>0 GOTO NIDN  ;"NOT FOUND
        SET LASTIFOBT=$$UP^XLFSTR($G(RESULTS(LABNAME,LASTDATE)))
        NEW FDOY SET FDOY=$$FIRSTYR^TMGDATE
        IF LASTDATE>FDOY DO
        . IF LASTIFOBT["NEG" DO
        . . SET TEST=1
        . . SET DATE=LASTDATE
NIDN
        QUIT
        ;"
WANTSPSA(TMGDFN)  ;"Determine if the patient has already stated that
        ;"they wanted the PSA done at a previous appointment
        NEW TMGRESULT SET TMGRESULT=""
        NEW WANTDT,ORDEREDDT,HFARRAY
        SET WANTDT=$$GETHFDT^TMGPXRU1(.TMGDFN,"TMG PSA PATIENT WANTS SCREENING",.HFARRAY)
        IF WANTDT'>0 GOTO WPDN    
        SET ORDEREDDT=$$GETHFDT^TMGPXRU1(.TMGDFN,"TMG PSA ORDERED",.HFARRAY)
        SET TMGRESULT="PATIENT STATED HE DID WANT PSA SCREENING ON: "_$$EXTDATE^TMGDATE(WANTDT,1)_". "
        IF ORDEREDDT'<WANTDT SET TMGRESULT=TMGRESULT_$C(13,10)_"IT WAS ORDERED ON: "_$$EXTDATE^TMGDATE(ORDEREDDT,1)_". "
WPDN        
        QUIT TMGRESULT
        ;"
LDLISHI(TMGDFN,TEST,DATE,DATA,TEXT)  ;" DETERMINE IF LDL IS HIGH ENOUGH TO WARRANT LIPID TOPIC
        NEW WHY SET WHY=""
        SET (TEST,DATE)=0
        NEW ARRAY,S
        SET S=$$GETTABLX^TMGTIUO6(TMGDFN,"[LIPIDS]",.ARRAY)
        DO PARSTABL^TMGTIUO7(.ARRAY)  ;"Parse out data values
        SET S=+$GET(ARRAY("KEY-VALUE","LDL Cholesterol"))  ;"GET ONLY FIRST VALUE                
        IF S'>0 QUIT
        IF S>160 DO
        . SET TEST=1
        . SET DATE=$$TODAY^TMGDATE
        . SET WHY="PATIENT HAD A LDL VALUE OF "_S_" ON "_$$EXTDATE^TMGDATE(DATE)
        QUIT WHY
        ;"
LIPIDTOP(TMGDFN,TEST,DATE,DATA,TEXT)   ;
        ;"Purpose: Return whether patient needs a lipid topic.
        ;"         It is true if pt doesn't have lipid topic AND has HTN or DM
        ;"              or LDL is > 160
        ;"         Will search the TMG TIU
        ;"Input: TMGDFN -- the patient IEN
        ;"       TEST -- AN OUT PARAMETER.  The logical value of the test:
        ;"               1=true, 0=false
        ;"               Also an IN PARAMETER.  Any value for COMPUTED
        ;FINDING PARAMETER will be passed in here.
        ;"       DATE -- AN OUT PARAMETER.  Date of finding.
        ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
        ;"       TEXT -- Text to be display in the Clinical Maintenance
        ;"Output.  Optional.
        ;"Results: none
        SET TEST=0,DATE=0
        NEW DONE,WHY SET DONE=0
        NEW IEN22719 SET IEN22719=0
        FOR  SET IEN22719=$ORDER(^TMG(22719,"DFN",TMGDFN,IEN22719)) QUIT:(IEN22719'>0)!(DONE=1)  DO
        . NEW TOPICTEXT SET TOPICTEXT=""
        . FOR  SET TOPICTEXT=$ORDER(^TMG(22719,IEN22719,2,"B",TOPICTEXT)) QUIT:(TOPICTEXT="")!(DONE=1)  DO
        . . NEW UPTOPIC SET UPTOPIC=$$UP^XLFSTR(TOPICTEXT)
        . . IF (UPTOPIC["LIPID") DO
        . . . SET (TEST,DATE)=0
        . . . SET DONE=1
        . . . SET WHY="TOPIC ALREADY EXISTS"
        . . ELSE  IF (UPTOPIC["HTN")!(UPTOPIC["HYPERTENSION")!(UPTOPIC["DM") DO
        . . . NEW TOPICDATE SET TOPICDATE=$PIECE($GET(^TMG(22719,IEN22719,0)),"^",2)
        . . . IF TOPICDATE>DATE SET DATE=TOPICDATE
        . . . SET TEST=1
        . . . IF UPTOPIC["DM" SET WHY="PATIENT HAS DIABETES TOPIC"
        . . . ELSE  SET WHY="PATIENT HAS A HYPERTENSION TOPIC"
        IF (DONE=1)!(TEST=1) QUIT WHY  ;"NO FURTHER TESTING NEEDED
        SET WHY=$$LDLISHI(.TMGDFN,.TEST,.DATE,.DATA,.TEXT)        
        QUIT WHY
        ;"        
BMICOHRT(TMGDFN,TEST,DATE,DATA,TEXT)       
        ;"Purpose: Return whether patient's BMI is between 35-39.9
        ;"Input: TMGDFN -- the patient IEN
        ;"       TEST -- AN OUT PARAMETER.  The logical value of the test:
        ;"               1=true, 0=false
        ;"               Also an IN PARAMETER.  Any value for COMPUTED
        ;FINDING PARAMETER will be passed in here.
        ;"       DATE -- AN OUT PARAMETER.  Date of finding.
        ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
        ;"       TEXT -- Text to be display in the Clinical Maintenance
        ;"Output.  Optional.
        ;"Results: none
        SET TEST=0,DATE=0
        NEW BMI SET BMI=$$BMI^TMGTIUOJ(TMGDFN)
        SET BMI=+$GET(BMI)
        IF BMI'>0 QUIT 
        IF (BMI>34.9)&(BMI<40) DO
        . SET TEST=1
        . SET DATE=$$TODAY^TMGDATE
        QUIT
        ;"
B12COHRT(TMGDFN,TEST,DATE,DATA,TEXT)       
        ;"Purpose: Return whether patient should be considered for B-12
        ;"Input: TMGDFN -- the patient IEN
        ;"       TEST -- AN OUT PARAMETER.  The logical value of the test:
        ;"               1=true, 0=false
        ;"               Also an IN PARAMETER.  Any value for COMPUTED
        ;FINDING PARAMETER will be passed in here.
        ;"       DATE -- AN OUT PARAMETER.  Date of finding.
        ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
        ;"       TEXT -- Text to be display in the Clinical Maintenance
        ;"Output.  Optional.
        ;"Results: none
        SET TEST=0,DATE=0
        SET WHY=""
        NEW TMGRESULT,MEDARR SET TMGRESULT=$$ONB12TX^TMGC0QT4(TMGDFN,$$TODAY^TMGDATE,.MEDARR)
        IF $D(MEDARR) DO
        . SET TEST=1
        . SET DATE=$$TODAY^TMGDATE       
        . SET WHY="REMINDER DUE BECAUSE PATIENT IS ON: "_$C(13,10)
        . NEW MEDNAME SET MEDNAME="" 
        . FOR  SET MEDNAME=$ORDER(MEDARR(MEDNAME)) QUIT:MEDNAME=""  DO
        . . SET WHY=WHY_MEDNAME_$C(13,10)
        QUIT WHY
        ;"        
ONMEMMEDS(TMGDFN,TEST,DATE,DATA,TEXT)       
        ;"Purpose: CF if patient is on memory meds 
        ;"Input: TMGDFN -- the patient IEN
        ;"       TEST -- AN OUT PARAMETER.  The logical value of the test:
        ;"               1=true, 0=false
        ;"               Also an IN PARAMETER.  Any value for COMPUTED
        ;"  FINDING PARAMETER will be passed in here.
        ;"       DATE -- AN OUT PARAMETER.  Date of finding.
        ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
        ;"       TEXT -- Text to be display in the Clinical Maintenance
        ;"Output.  Optional.
        ;"Results: none
        SET TEST=0,DATE=0
        ;"Test Med List
        NEW TMGMEDLIST,TMGMEDARRAY
        ;"//kt 5/7/18 DO MEDLIST^TMGTIUOJ(.TMGMEDLIST,.TMGDFN,.TMGMEDARRAY)  
        DO MEDARR^TMGTIUOJ(.TMGMEDLIST,.TMGDFN,.TMGMEDARRAY)     ;"//kt 5/7/18
        IF $DATA(TMGMEDARRAY) DO
        . NEW IDX SET IDX=0
        . NEW ITEM
        . FOR  SET IDX=$ORDER(TMGMEDARRAY(IDX)) QUIT:(+IDX'>0)!(TEST=1)  DO
        . . SET ITEM=$$UP^XLFSTR($GET(TMGMEDARRAY(IDX)))
        . . IF (ITEM["ARICEPT")!(ITEM["NAMENDA") DO
        . . . SET TEST=1
        QUIT
        ;"
PTMEMMEDS(TMGRESULT,TMGDFN)
        ;"RPC Wrapper for ONMEMMEDS
        ;"  CHECKS TO SEE IF PATIENT IS ON AN APPLICABLE MEMORY MED
        ;"  TMGRESULT=1 FOR YES, OR 0 FOR NO
        ;"  Expanding logic: originally this was used to check 
        NEW DATE,TEST
        DO ONMEMMEDS(TMGDFN,.TEST,.DATE)
        IF TEST=1 DO
        . SET TMGRESULT="1^PATIENT HAS MEMORY ISSUES. PLEASE MAKE SURE THEY GET A PRINTOUT OF THIS CONSULT, OR MAIL TO THEM IF THEY AREN'T IN THE OFFICE."
        ELSE  DO
        . NEW MSG SET MSG=$P($G(^DPT(TMGDFN,"TMGMSG")),"^",1)
        . IF MSG'="" DO
        . . SET TMGRESULT="1^"_MSG
        . ELSE  DO
        . . SET TMGRESULT="0^NOT ON MEMORY MEDS"
        QUIT
        ;"
FIB4(TMGDFN)  ;"CALCULATE THE PATIENT'S FIB4 SCORE USING THE FORMULA:
        ;"AGE * AST / (0.001 * PLATELETS * sqr(ALT))
        ;"With results being:
        ;"   <1.45 : Cirrhosis less likely
        ;"   1.45 to 3.25: Indeterminate
        ;"   >3.25 : Cirrhosis more likely
        NEW AGE,AST,PLT,ALT
        NEW ASTDT,PLTDT,ALTDT
        K VADM SET AGE=$$AGE^TIULO(TMGDFN)
        NEW TMGRESULT SET TMGRESULT="VALUE CANNOT BE DETERMINIED. "
        DO GETLLAB(TMGDFN,190,.ASTDT,.AST)
        DO GETLLAB(TMGDFN,9,.PLTDT,.PLT)
        DO GETLLAB(TMGDFN,191,.ALTDT,.ALT)
        NEW REASON SET REASON=""
        IF AST="" SET REASON="NO AST FOUND"
        IF PLT="" DO
        . IF REASON'="" SET REASON=REASON_", "
        . SET REASON=REASON_"NO PLT FOUND"
        IF ALT="" DO
        . IF REASON'="" SET REASON=REASON_", "
        . SET REASON=REASON_"NO ALT FOUND"
        IF REASON'="" SET TMGRESULT=TMGRESULT_"("_REASON_")" QUIT TMGRESULT
        NEW VALUE 
        SET VALUE=(AGE*AST)/(PLT*$$SQRT^XLFMTH(ALT))  ;"CHECK MATH HERE
        SET VALUE=$J(VALUE,"",2)
        IF VALUE<1.45 SET TMGRESULT="FIB-4 = "_VALUE_". Cirrhosis less likely"
        ELSE  IF VALUE<3.26 SET TMGRESULT="FIB-4 = "_VALUE_". Indeterminate"
        ELSE  SET TMGRESULT="FIB-4 = "_VALUE_". Cirrhosis more likely"
        QUIT TMGRESULT
        ;"
BDAPPLIC(TMGDFN,TEST,DATE,DATA,TEXT)   ;
	    ;"Purpose: Determine if patient's bone density not indicated for this year 
        ;"Input: TMGDFN -- the patient IEN
        ;"       TEST -- AN OUT PARAMETER.  The logical value of the test:
        ;"               1=true, 0=false
        ;"               Also an IN PARAMETER.  Any value for COMPUTED
        ;" FINDING PARAMETER will be passed in here.
        ;"       DATE -- AN OUT PARAMETER.  Date of finding.
        ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
        ;"       TEXT -- Text to be display in the Clinical Maintenance
        ;"Output.  Optional.
        ;"Results: WHY - returns why HTNMEDS is true 
        DO HFTHISYR(TMGDFN,.TEST,.DATE,"TMG BONE DENSITY NOT INDICATED THIS YEAR")
        IF TEST=0 DO
        . DO HFL14D(TMGDFN,.TEST,.DATE,"TMG BONE DENSITY ORDERED")
        QUIT
        ;"
UMARDONE(TMGDFN,TEST,DATE,DATA,TEXT) ;"DETERMINE IF UMAR HAS BEEN DONE THIS CALENDAR YEAR
        ;"Purpose: Return a "NOW" date as a "finding".  This is the effective date 
        ;"          that the reminder is being run.
        ;"Input: See discussion above for details. 
        ;"Output: DATA filled as follows:
        ;"          DATA = Date of birth (internal Fileman format)
        ;"          DATA("YEAR")= year of effective date
        ;"          DATA("MONTH"= month of effective date
        ;"          DATA("DAY")= day (2 digits) of effective date
        ;"          DATA("TIME")=time of effective date
        ;"          DATA("DOY")= day of year (1-365)
        ;"Results: none        
        SET (TEST,DATE)=0
        NEW LDT,LVAL
        DO GETLLAB(TMGDFN,5064,.LDT,.LVAL)
        NEW FIRSTDOY SET FIRSTDOY=$$FIRSTYR^TMGDATE
        IF LDT>FIRSTDOY DO
        . SET DATE=LDT
        . SET TEST=1
        QUIT
        ;"
P20CHECK(TMGDFN,TEST,DATE,DATA,TEXT) ;"DETERMINE IF P-20 WAS CHECKED TODAY
        ;"Purpose: Return a "NOW" date as a "finding".  This is the effective date 
        ;"          that the reminder is being run.
        ;"Input: See discussion above for details. 
        ;"Output: DATA filled as follows:
        ;"          DATA = Date of birth (internal Fileman format)
        ;"          DATA("YEAR")= year of effective date
        ;"          DATA("MONTH"= month of effective date
        ;"          DATA("DAY")= day (2 digits) of effective date
        ;"          DATA("TIME")=time of effective date
        ;"          DATA("DOY")= day of year (1-365)
        ;"Results: none              
        SET (TEST,DATE)=0
        NEW DSDATE SET DSDATE=$$GETHFDT^TMGPXRU1(.TMGDFN,"TMG PREVISIT CHECKED FOR P20")
        IF DSDATE=$$TODAY^TMGDATE DO
        . SET TEST=1,DATE=DSDATE
        QUIT
        ;"
SHNGDISC(TMGDFN,TEST,DATE,DATA,TEXT) ;"DETERMINE IF SHINGRIX WAS DISCUSSED IN THE LAST YEAR (TURNS OFF TMG SHINGRIX FOR 1 YEAR IF DISCUSSED)
        ;"Purpose: Return a "NOW" date as a "finding".  This is the effective date 
        ;"          that the reminder is being run.
        ;"Input: See discussion above for details. 
        ;"Output: DATA filled as follows:
        ;"          DATA = Date of birth (internal Fileman format)
        ;"          DATA("YEAR")= year of effective date
        ;"          DATA("MONTH"= month of effective date
        ;"          DATA("DAY")= day (2 digits) of effective date
        ;"          DATA("TIME")=time of effective date
        ;"          DATA("DOY")= day of year (1-365)
        ;"Results: none              
        SET (TEST,DATE)=0
        NEW DSDATE SET DSDATE=$$GETHFDT^TMGPXRU1(.TMGDFN,"TMG SHINGRIX DISCUSSED AND CONSIDERING")
        NEW BDATE SET BDATE=$$ADDDAYS^TMGDATE("-365")
        IF DSDATE>BDATE DO
        . SET TEST=1,DATE=DSDATE
        QUIT
        ;"        