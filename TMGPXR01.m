TMGPXR01 ;TMG/kst/TMG reminder stuff ;7/26/12, 2/2/14
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
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"DOB(DFN,TEST,DATE,DATA,TEXT) -- Return information about the date of birth for the patient
 ;"ACTIVEPT(DFN,TEST,DATE,DATA,TEXT) -- Return IF patient is an active patient (recent activity)
 ;"DTEVAL(DFN,TEST,DATE,DATA,TEXT) --Return the effective date that the reminder is being run.
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
DOB(DFN,TEST,DATE,DATA,TEXT) ;
        ;"Purpose: Return information about the date of birth for the patient
        ;"Input: DFN -- the patient IEN
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
        NEW DOB SET DOB=$PIECE($GET(^DPT(DFN,0)),"^",3)\1
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
DTEVAL(DFN,TEST,DATE,DATA,TEXT) ;"DATE OF EVALUATION
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
ACTIVEPT(DFN,TEST,DATE,DATA,TEXT) ;"ACTIVE PATIENT   **NAME ALERT** --There is also an ACTIVEPT^TMGPXR03() for a different purpose.
        ;"Input: DFN -- the patient IEN
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
        FOR  SET TITLEIEN=$ORDER(^TIU(8925,"APT",DFN,TITLEIEN)) QUIT:(+TITLEIEN'>0)!(FOUND>0)  DO
        . NEW STATUSIEN SET STATUSIEN=0
        . FOR  SET STATUSIEN=$ORDER(^TIU(8925,"APT",DFN,TITLEIEN,STATUSIEN)) QUIT:(+STATUSIEN'>0)!(FOUND>0)  DO
        . . NEW PREVDT SET PREVDT=$ORDER(^TIU(8925,"APT",DFN,TITLEIEN,STATUSIEN,REVDATE))
        . . IF PREVDT'>REVSDT DO
        . . . SET FOUND=PREVDT
        . . . SET DOCIEN=$ORDER(^TIU(8925,"APT",DFN,TITLEIEN,STATUSIEN,PREVDT,0))
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
        ;"Input: DFN -- the patient IEN
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
        NEW TMGMEDLIST,TMGMEDARRAY
        NEW DBTAG SET DBTAG="*CSM-DATABASE REVIEW"
        DO MEDLIST^TMGTIUOJ(.TMGMEDLIST,.TMGDFN,.TMGMEDARRAY)
        IF $DATA(TMGMEDARRAY) DO
        . NEW DBDATE SET DBDATE=$GET(TMGMEDARRAY("KEY-VALUE",DBTAG))
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
PTONASA(TMGDFN,TEST,DATE,DATA,TEXT)  ;"
        ;"Purpose: To determine if the patient is on ASA
        ;"Input: same as above
        SET TEST=0
        SET DATE=0
        NEW TMGMEDLIST,TMGMEDARRAY
        DO MEDLIST^TMGTIUOJ(.TMGMEDLIST,.TMGDFN,.TMGMEDARRAY)
        IF $DATA(TMGMEDARRAY) DO
        . NEW IDX SET IDX=0
        . FOR  SET IDX=$ORDER(TMGMEDARRAY(IDX)) QUIT:IDX'>0  DO
        . . IF $$UP^XLFSTR($GET(TMGMEDARRAY(IDX)))["ASA " DO
        . . . DO NOW^%DTC
        . . . SET TEST=1
        . . . SET DATE=X
        QUIT        
        ;
COPDINC(TMGDFN,TEST,DATE,DATA,TEXT)  ;"
        ;"Purpose: To determine if the patient should be included in
        ;"         the COPD cohort based on insurance
        SET TEST=0
        SET DATE=0
        NEW INS2INC
        SET INS2INC=",3,5,6,13,17,36,15," ;"IENS OF ACCEPTABLE INS IN #36. USING LEADING AND TRAILING COMMAS FOR EASE OF MATCHING
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(^DPT(DFN,.312,IDX)) QUIT:IDX'>0  DO
        . NEW INSIEN SET INSIEN=","_$GET(^DPT(DFN,.312,IDX,0))_","
        . IF INS2INC[INSIEN DO
        . . DO NOW^%DTC
        . . SET TEST=1
        . . SET DATE=X
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
BCWELLDN(TMGDFN,TEST,DATE,DATA,TEXT)  ;"
        ;"Purpose: To determine if the patient has had a G0439 billed this
        ;"         calendar year
        SET TEST=0
        SET DATE=0
        NEW IEN SET IEN=0
        FOR  SET IEN=$ORDER(^AUPNVCPT("C",TMGDFN,IEN)) QUIT:IEN'>0  DO
        . NEW CPTIEN SET CPTIEN=$P($G(^AUPNVCPT(IEN,0)),"^",1)
        . NEW CPT SET CPT=$P($G(^ICPT(CPTIEN,0)),"^",1)
        . IF CPT="G0439" DO
        . . SET TEST=1
        . . NEW VISIT SET VISIT=$P($G(^AUPNVCPT(IEN,0)),"^",3)
        . . NEW THISDATE SET THISDATE=$P($G(^AUPNVSIT(VISIT,0)),"^",1)
        . . SET THISDATE=$E(THISDATE,1,3)_"0101.010101"
        . . IF THISDATE>DATE SET DATE=THISDATE
        QUIT
        ;"
BCADVPT(TMGDFN,TEST,DATE,DATA,TEXT)  ;"
        ;"Purpose: To detmine if the patient is a BCBS Advantage patient
        SET TEST=0
        SET DATE=0
        NEW BCBSAIEN SET BCBSAIEN=+$ORDER(^DIC(36,"B","BC/BS ADVANTAGE",0))
        IF BCBSAIEN'>0 GOTO BCDN
        NEW INSIDX SET INSIDX=0
        FOR  SET INSIDX=$ORDER(^DPT(TMGDFN,.312,INSIDX)) QUIT:INSIDX'>0  DO
        . NEW THISIEN SET THISIEN=$G(^DPT(TMGDFN,.312,INSIDX,0))
        . IF THISIEN=BCBSAIEN DO
        . . SET TEST=1
BCDN
        QUIT
        ;"
GETIUIEN(NAME) ;
       ;"Return the IEN of the given note title
       NEW TMGRESULT SET TMGRESULT=0
       SET TMGRESULT=$ORDER(^TIU(8925.1,"B",NAME,TMGRESULT))
       QUIT TMGRESULT
       ;"
TIUDATES(DFN,TITLENAME,RESULTARR)  ;
       ;"Purpose: Return array of all a patient's complete physical exam
       ;"         note titles
       ;"Input: DFN -- Patient's IEN
       ;"       RESULTARR(DATE,NOTEIEN)="" -- OUTPUT ARRAY 
       NEW IEN8925D1 SET IEN8925D1=$$GETIUIEN(TITLENAME)
       IF IEN8925D1'>0 GOTO GCDN
       NEW DATE SET DATE=0
       NEW TIUIEN,FMDATE
       SET TIUIEN=0
       FOR  SET TIUIEN=$ORDER(^TIU(8925,"C",DFN,TIUIEN)) QUIT:TIUIEN'>0  DO
       . NEW DOCIEN,ZN
       . SET ZN=$GET(^TIU(8925,TIUIEN,0))
       . SET DOCIEN=$PIECE(ZN,"^",1)
       . IF DOCIEN=IEN8925D1 DO
       . . SET FMDATE=$PIECE(ZN,"^",7)
       . . SET RESULTARR(FMDATE,TIUIEN)=""
       GOTO GCDN
       ;"OLD METHOD
       FOR  SET DATE=$ORDER(^TIU(8925,"AE",DFN,DATE)) QUIT:DATE'>0  DO
       . SET TIUIEN=0
       . FOR  SET TIUIEN=$ORDER(^TIU(8925,"AE",DFN,DATE,TIUIEN)) QUIT:TIUIEN'>0  DO
       . . IF TIUIEN=IEN8925D1 DO
       . . . SET FMDATE=9999999-DATE
       . . . SET RESULTARR(FMDATE,$ORDER(^TIU(8925,"AE",DFN,DATE,TIUIEN,0)))=""
GCDN   QUIT
       ;"
LASTCPES(TMGDFN,NGET,BDT,EDT,NFOUND,TEST,DATE,DATA,TEXT) ;
       ;"Purpose: Return dates of patient's complete physical exams -
       ;"         multiple computed finding
       ;"Input: DFN -- the patient IEN
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
LASTEYEE(TMGDFN,NGET,BDT,EDT,NFOUND,TEST,DATE,DATA,TEXT) ;
       ;"Purpose: Return dates of patient's last eye exam -
       ;"         multiple computed finding
       ;"Input: DFN -- the patient IEN
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
       DO MEDLIST^TMGTIUOJ(.TMGMEDLIST,.TMGDFN,.TMGMEDARRAY)
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
PAINDONE(DFN,TEST,DATE,DATA,TEXT) ;
        ;"Purpose: Return information about the last pain contract
        ;"Input: DFN -- the patient IEN
        ;"       TEST -- AN OUT PARAMETER.  The logical value of the test:
        ;"               1=true, 0=false
        ;"               Also an IN PARAMETER.  Any value for COMPUTED
        ;FINDING PARAMETER will be passed in here.
        ;"       DATE -- AN OUT PARAMETER.  Date of finding.
        ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
        ;"       TEXT -- Text to be display in the Clinical Maintenance
        ;"Output.  Optional.
        ;"Results: none
        NEW CONTRACTDATE SET CONTRACTDATE=$$DATPAINK(.DFN,1)
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
CSDBDONE(DFN,TEST,DATE,DATA,TEXT)  ;
        SET TEST=0,DATE=0
        NEW TMGTABLE,TMGTABLEARR
        SET TMGTABLE=$$GETTABLX^TMGTIUOJ(+$G(DFN),"[MEDICATIONS]",.TMGTABLEARR)
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
DRGSDONE(DFN,TEST,DATE,DATA,TEXT)  ;
        ;"Purpose: Return information about the last drug screen
        ;"Input: DFN -- the patient IEN
        ;"       TEST -- AN OUT PARAMETER.  The logical value of the test:
        ;"               1=true, 0=false
        ;"               Also an IN PARAMETER.  Any value for COMPUTED
        ;FINDING PARAMETER will be passed in here.
        ;"       DATE -- AN OUT PARAMETER.  Date of finding.
        ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
        ;"       TEXT -- Text to be display in the Clinical Maintenance
        ;"Output.  Optional.
        ;"Results: none
        NEW DSDATE SET DSDATE=$$GETHFDT^TMGPXRU1(.DFN,"TMG DRUG SCREEN DONE")
        IF DSDATE'>0 SET DSDATE=+$$DATPAINK(.DFN,2) 
        ;"Check to see if Urine Drug Screen note title exists and compare
        ;"    dates if it does.
        NEW UDSIEN SET UDSIEN=+$ORDER(^TIU(8925.1,"B","URINE DRUG SCREEN",0))
        IF UDSIEN>0 DO
        . NEW TIUIEN SET TIUIEN=0
        . FOR  SET TIUIEN=$ORDER(^TIU(8925,"B",UDSIEN,TIUIEN)) QUIT:TIUIEN'>0  DO
        . . NEW ZN SET ZN=$GET(^TIU(8925,TIUIEN,0))
        . . IF DFN'=$PIECE(ZN,"^",2) QUIT
        . . IF $PIECE(ZN,"^",7)>DSDATE SET DSDATE=$PIECE(ZN,"^",7) 
        IF +DSDATE'>0 DO
        . SET TEST=0
        . SET DATE=0
        ELSE  DO
        . SET TEST=1
        . SET DATE=DSDATE     
        QUIT
        ;"
EKGDONE(DFN,TEST,DATE,DATA,TEXT)  ;do not use yet
        SET TEST=0,DATE=0
        QUIT  ;"DATA IS INCONSISTENT
        NEW TMGTABLE,TMGTABLEARR
        SET TMGTABLE=$$GETTABLX^TMGTIUOJ(+$G(DFN),"[STUDIES]",.TMGTABLEARR)
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
LASTEKG(DFN)    ;"
        NEW TMGRESULT SET TMGRESULT=""
        NEW TMGTABLE,TMGTABLEARR
        SET TMGTABLE=$$GETTABLX^TMGTIUOJ(+$G(DFN),"[STUDIES]",.TMGTABLEARR)
        IF $DATA(TMGTABLEARR) DO
        . NEW IDX SET IDX=0
        . FOR  SET IDX=$ORDER(TMGTABLEARR(IDX)) QUIT:IDX'>0  DO
        . . NEW LINE SET LINE=$GET(TMGTABLEARR(IDX))
        . . IF LINE["EKG:" DO
        . . . SET TMGRESULT=LINE
        QUIT TMGRESULT
        ;"
UDSTABLE(DFN)  ;"
        ;"Purpose: this function will be called from the UDS item in the Pain Table
        ;"         and return the last 3 entries
        NEW TMGRESULT,HFARRAY SET TMGRESULT="UDS: "
        NEW DSDATE SET DSDATE=$$GETHFDT^TMGPXRU1(.DFN,"TMG DRUG SCREEN DONE",.HFARRAY)
        NEW UDSIEN SET UDSIEN=+$ORDER(^TIU(8925.1,"B","URINE DRUG SCREEN",0))
        IF UDSIEN>0 DO
        . NEW TIUIEN SET TIUIEN=0
        . FOR  SET TIUIEN=$ORDER(^TIU(8925,"B",UDSIEN,TIUIEN)) QUIT:TIUIEN'>0  DO
        . . NEW ZN SET ZN=$GET(^TIU(8925,TIUIEN,0))
        . . IF DFN'=$PIECE(ZN,"^",2) QUIT
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
LASTHGT(DFN,TEST,DATE,DATA,TEXT)   ;
        ;"Purpose: Return date of last height check and 
        ;"         whether it was in the last 2 years
        ;"Input: DFN -- the patient IEN
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
        FOR  SET GMRIEN=$ORDER(^GMR(120.5,"C",DFN,GMRIEN)) QUIT:(+GMRIEN'>0)  DO
        . SET VITDATA=$GET(^GMR(120.5,GMRIEN,0))
        . SET VITTYPE=$PIECE(VITDATA,"^",3)
        . IF VITTYPE'=HEIGHTIEN QUIT
        . SET DATE=$PIECE(VITDATA,"^",1)
        . SET TEST=1 
        QUIT
        ;
ONOXYGEN(TMGDFN,TEST,DATE,DATA,TEXT)  ;
        ;"Purpose: Return whether patient is on oxygen
        ;"Input: DFN -- the patient IEN
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
        DO MEDLIST^TMGTIUOJ(.TMGMEDLIST,.TMGDFN,.TMGMEDARRAY)
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
        SET TMGTABLE=$$GETTABLX^TMGTIUOJ(+$G(TMGDFN),"[COPD]",.TMGTABLEARR)
        IF $DATA(TMGTABLEARR) DO        
        . NEW O2DATA 
        . SET O2DATA=$GET(TMGTABLEARR("KEY-VALUE","HOME O2"))
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
PULSEOX(TMGDFN,TEST,DATE,DATA,TEXT)  ;
        ;"Purpose: Return whether patient is on oxygen
        ;"Input: DFN -- the patient IEN
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
        SET TMGTABLE=$$GETTABLX^TMGTIUOJ(+$G(TMGDFN),"[COPD]",.TMGTABLEARR)
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
        ;"Input: DFN -- the patient IEN
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
        . . IF (UPTOPIC["DM")!(UPTOPIC["DIABETES") DO
        . . . IF UPTOPIC["FH" QUIT  ;"elh added 9/8/16 to keep family history from giving false positive
        . . . IF UPTOPIC["SCREEN" QUIT ;"elh added 9/12/16 to keep screenings from giving false positives
        . . . IF UPTOPIC["PRE" QUIT ;"elh added 9/15/16 to keep pre-diabetes from giving false positives
        . . . NEW TOPICDATE SET TOPICDATE=$PIECE($GET(^TMG(22719,IEN22719,0)),"^",2)
        . . . IF TOPICDATE>DATE SET DATE=TOPICDATE
        . . . SET TEST=1
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
        QUIT
        ;"
PTHSCOPD(TMGDFN,TEST,DATE,DATA,TEXT)  ;
        ;"Purpose: Return whether patient has COPD
        ;"         Will search the TMG TIU
        ;"Input: DFN -- the patient IEN
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
PTASTHMA(TMGDFN,TEST,DATE,DATA,TEXT)  ;
        ;"Purpose: Return whether patient has asthma
        ;"         Will search the TMG TIU
        ;"Input: DFN -- the patient IEN
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
        ;"Input: DFN -- the patient IEN
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
LDCTSMKR(TMGDFN,TEST,DATE,DATA,TEXT) ;
        ;"Purpose: Determine IF patient is currently a smoker or has quit in
        ;"         the last 15 years
        ;"Input: DFN -- the patient IEN
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
        SET X2=15*365 ;"15 YEARS
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
GETHFGRP(DFN,HFGROUPIEN,TMGRESULTARR)  ;"Return health factors for a patient by a given hf group
        ;"Purpose:
        ;"Input:
        ;"Output:
        NEW HFIEN,DATE
        SET HFIEN=0
        FOR  SET HFIEN=$ORDER(^AUTTHF("AC",HFGROUPIEN,HFIEN)) QUIT:HFIEN'>0  DO
        . SET DATE=0
        . FOR  SET DATE=$ORDER(^AUPNVHF("AA",DFN,HFIEN,DATE)) QUIT:DATE'>0  DO
        . . SET TMGRESULTARR(9999999-DATE,HFIEN)=""
        QUIT
        ;"
FLUREM(DFN,TEST,DATE,DATA,TEXT) ;"Return whether flu reminder should be active
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
       NEW DFN SET DFN=0
       NEW TEST,DATE,DATA,TEXT
       FOR  SET DFN=$ORDER(^DPT(DFN)) QUIT:+DFN'>0  DO
       . DO PTHASDM(DFN,.TEST,.DATE,.DATA,.TEXT)
       . IF TEST=0 QUIT
       . IF $$EXCLUDE^TMGC0QTU(DFN)=1 QUIT
       . WRITE "[ ] ",$PIECE($GET(^DPT(DFN,0)),"^",1),!
       DO ^%ZISC  ;" Close the output device
       DO PRESS2GO^TMGUSRI2
       QUIT
       ;"
HTNMEDS(TMGDFN,TEST,DATE,DATA,TEXT) ;
        ;"Purpose: Determine if patient is on HTN medication
        ;"Input: DFN -- the patient IEN
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
        NEW TMGRESULT SET TMGRESULT=$$ONHTNTX^TMGC0QT4(TMGDFN,X)
        IF TMGRESULT=1 DO
        . SET TEST=1
        . SET DATE=X
        QUIT
        ;"
LIPIDMED(TMGDFN,TEST,DATE,DATA,TEXT) ;
        ;"Purpose: Determine if patient is on lipid medication
        ;"Input: DFN -- the patient IEN
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
        NEW TMGRESULT SET TMGRESULT=$$ONLIPDTX^TMGC0QT4(TMGDFN,X)
        IF TMGRESULT=1 DO
        . SET TEST=1
        . SET DATE=X
        QUIT
        ;"
DMMEDS(TMGDFN,TEST,DATE,DATA,TEXT) ;
        ;"Purpose: Determine if patient is on DM medication
        ;"Input: DFN -- the patient IEN
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
        . SET DATE=X
        QUIT
        ;"












