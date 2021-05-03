TMGIMM1 ;TMG/kst/Immunication interface 12/4/12, 2/2/14, 3/24/21
         ;;1.0;TMG-LIB;**1**;12/4/12
 ;
 ;"TMG C0Q FUNCTIONS
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"NOTE: Report card code.
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;
 ;"V04(TIUIEN) -- Event handler for note completion with immuniztion data, to be sent to registry
 ;"------------ Messages --------------------------------
 ;"GETVXQ(TMGDFN,ERR) -- GET QUERY FOR VACCINATION RECORD
 ;"GETVXU(TMGDFN,IENS,ERR)-- GET UNSOLICITED VACCINATION RECORD UPDATE
 ;"------------ Message Segments ------------------------
 ;"GETMSH() -- GET MESSAGE HEADER
 ;"GETQRD(TMGDFN) -- DEFINES THE QUERY IN HL7 MESSAGE
 ;"GETQRF(SDT,EDT) -- SUBORDINATE TO QRD TO FURTHER REFINE A QUERY
 ;"GETPID(TMGDFN) -- GET PATIENT IDENTIFICATION SEGMENT
 ;"GETNK1(TMGDFN) -- GET NEXT OF KIN INFORMATION SEGMENT
 ;"GETRXA(IENS) -- PHARMACY ADMINISTRATION SEGMENT
 ;"GETRXR(IENS) -- GET PHARMACY ROUTE INFORMATION AND SITE INFO
 ;"GETOBX(IENS) -- Get OBSERVATION AND RESULT SEGMENT (ANY ADVERSE REACTIONS)
 ;"GETCVX5(TEXT) -- TRANSLATE CVX CODE INTO THE PROPER RXA-5 STRING
 ;
 ;"=======================================================================
 ;" API -- Private Functions.
 ;"=======================================================================
 ;"GETIENS(TIUIEN,TMGDFN,IENS) -- Get IENS to careplan for given TIUIEN
 ;"ADD(OUT,VALUE) -- Add value to growing HL7 message
 ;"OUTPUT(TEXT) -- Output HL7 text to file system etc. 
 ;"TEST -- To see test output
 ;"TESTTIU -- Query user for TIU document, and then test.
 ;"
 ;"=======================================================================
 ;
V04(TIUIEN) ;
        ;"Purpose: Event handler for note completion with immuniztion data, 
        ;"         to be sent to registry
        ;"Input: TIUIEN -- IEN in 8925
        ;"Output: Will output file
        ;"Result: 1^Success or -1^Message IF error.
        ;
        NEW TMGRESULT,TMGDFN,IENS,ERR,TEXTARRAY
        SET TMGRESULT=$$GETIENS(TIUIEN,.TMGDFN,.IENS)
        IF +TMGRESULT<0 GOTO V04DN
        SET TMGRESULT=$$GETVXU(TMGDFN,IENS,.TEXTARRAY)
        IF +TMGRESULT<0 GOTO V04DN
        SET TMGRESULT=$$OUTPUT(.TEXTARRAY)
V04DN   QUIT TMGRESULT
        ;
        ;"------------------------------------------------------
        ;"------------ Messages --------------------------------
        ;"------------------------------------------------------
        ;
GETVXQ(TMGDFN,OUT) ;
        ;"Purpose: QUERY FOR VACCINATION RECORD
        ;"Input: TMGDFN -- patient IEN
        ;"       OUT -- PASS BY REFERENCE, AN OUT PARAMETER
        ;"Structure:
        ;" MSH
        ;" QRD
        ;" [QRF]
        ;"Result: 1^Success, or -1^Error Message IF any.
        ;"OUT format:  OUT(1)=<first line>
        ;"             OUT(2)=<2nd line> ...
        NEW RESULT SET RESULT="1^Success"
        SET RESULT=$$ADD(.OUT,$$GETMSH()) IF +RESULT<0  GOTO VXQDN
        SET RESULT=$$ADD(.OUT,$$GETQRD(TMGDFN)) IF +RESULT<0  GOTO VXQDN
        SET RESULT=$$ADD(.OUT,$$GETQRF) IF +RESULT<0  GOTO VXQDN
VXQDN   QUIT RESULT
        ;
GETVXU(TMGDFN,IENS,OUT) ;
        ;"Purpose: UNSOLICITED VACCINATION RECORD UPDATE
        ;"Input: TMGDFN -- patient IEN 
        ;"       IENS -- pointer to subfile 19009.03
        ;"       OUT -- PASS BY REFERENCE, AN OUT PARAMETER
        ;"Structure:
        ;" MSH
        ;" PID
        ;" [{NK1}]
        ;" [{RXA
        ;"   [RXR]
        ;"      [{OBX}]
        ;" }]            
        ;"Result: 1^Success, or -1^Error Message IF any.  
        ;"OUT format:  OUT(1)=<first line>
        ;"             OUT(2)=<2nd line> ...
        NEW RESULT SET RESULT="1^Success"
        SET RESULT=$$ADD(.OUT,$$GETMSH) IF +RESULT<0 GOTO VXQDN
        SET RESULT=$$ADD(.OUT,$$GETPID(TMGDFN)) IF +RESULT<0 GOTO VXQDN
        SET RESULT=$$ADD(.OUT,$$GETNK1(TMGDFN)) IF +RESULT<0 GOTO VXQDN
        SET RESULT=$$ADD(.OUT,$$GETRXA(IENS)) IF +RESULT<0 GOTO VXQDN
        SET RESULT=$$ADD(.OUT,$$GETRXR(IENS)) IF +RESULT<0 GOTO VXQDN
        SET RESULT=$$ADD(.OUT,$$GETOBX(IENS)) IF +RESULT<0 GOTO VXQDN
        QUIT RESULT
        ;
        ;"------------------------------------------------------
        ;"------------ Message Segments ------------------------
        ;"------------------------------------------------------
GETMSH() ;
        ;"Purpose: GET MESSAGE HEADER
        ;"Input:(none) 
        ;"Structure:    Prefix: MSH
        ;"              Seq 1: Delimiter ("|")
        ;"              Seq 2: Encoding Characters ("^~\&")
        ;"              Seq 3: Sending Application ( VistA )
        ;"              Seq 4: Sending Facility ( NPI )
        ;"              Seq 5: Receiving Application ("SIIS")
        ;"              Seq 6: Receiving Facility ("TDH")
        ;"              Seq 7: Date/Time of Message (OPTIONAL)
        ;"              Seq 8: Security (OPTIONAL)
        ;"              Seq 9: Message Type ("VXU^V04")
        ;"              Seq 10: Message control ID ("1000")
        ;"              Seq 11: Processing ID (Either "P" (Production) or "D" (Debugging) or "T" (Testing))
        ;"              Seq 12: Version ID ("2.3.1")
        ;"              Seq 13-20 Are all optional
        ;"Result: returns string for HL7 message segment.
        ;"        or -1^Message for cases of error
        NEW TMGRESULT
        NEW APP SET APP="VistA"  ;"THIS IS PIECE 3 (IS TN EXPECTING A CERTAIN VALUE HERE?)
        NEW NPI SET NPI="1093726085" ;"THIS IS PIECE 4 (CURRENTLY SET TO DR K)
        NEW PROCESSINGID SET PROCESSINGID="T"  ;"PIECE 11
        ;"GET DATE
        NEW DATETIME SET DATETIME=$$HL7DT($$NOW^XLFDT)
        ;"
        SET TMGRESULT="MSH|^~\&|"_APP_"|"_NPI_"|SIIS|TDH|"_DATETIME_"||VXU^V04|1000|"
        SET TMGRESULT=TMGRESULT_PROCESSINGID_"|2.3.1|"
        QUIT TMGRESULT
        ;
GETQRD(TMGDFN) ;
        ;"Purpose: DEFINES THE QUERY IN HL7 MESSAGE
        ;"Input: TMGDFN -- patient IEN 
        ;"Structure:    Prefix: QRD
        ;"              Seq 1: Query Time Date
        ;"              Seq 2: Query Format Code ("D"=Display,"R"=Record,"T"=Tabular)
        ;"              Seq 3: Query Priority ("D"=Deferred,"I"=Immediate)
        ;"              Seq 4: Query ID (Unique ID, assigned by querying application)
        ;"              Seq 5-6 Optional
        ;"              Seq 7: Quantity Limited Request (#^Value) 
        ;"                     Values: "CH"=Characters,"LI"=Lines,"PG"=Pages,"RD"=Records,"ZO"=Locally Defined
        ;"              Seq 8: Who Subject Filter (^LastName%^FirstName%)
        ;"              Seq 9: What Subject Filter ("VXI")
        ;"              Seq 10: What Department Data Code ("SIIS")
        ;"              Seq 11&12: Optional
        ;"Result: returns string for HL7 message segment.
        ;"        or -1^Message for cases of error
        NEW TMGRESULT,DATE,X
        NEW DATE SET DATE=$$HL7DT($$NOW^XLFDT)
        ;"
        NEW PATIENTNAME,FNAME,LNAME
        SET PATIENTNAME=$$GET1^DIQ(2,TMGDFN_",",".01")
        SET FNAME=$PIECE(PATIENTNAME,",",2)
        SET LNAME=$PIECE(PATIENTNAME,",",1)
        SET TMGRESULT="QRD|"_DATE_"|R|I|00001||2000^RD|^"_LNAME_"%^"_FNAME_"%|VXI|SIIS"
        QUIT TMGRESULT
        ;
GETQRF(SDT,EDT) ;
        ;"Purpose: SUBORDINATE TO QRD TO FURTHER REFINE A QUERY
        ;"Input: SDT -- Start Date  (TO BE IMPLEMENTED)
        ;"       EDT -- End Date    (TO BE IMPLEMENTED)
        ;"Structure:    Prefix: QRF
        ;"              Seq 1: Where Subject Filer  ("TNSIIS")
        ;"              Seq 2-9 Optional
        ;"Result: returns string for HL7 message segment.
        ;"        or -1^Message for cases of error
        NEW TMGRESULT
        SET TMGRESULT="QRF|TNSIIS|"
        QUIT TMGRESULT
        ;
GETPID(TMGDFN) ;
        ;"Purpose: GET PATIENT IDENTIFICATION SEGMENT
        ;"Input: TMGDFN -- patient IEN 
        ;"Structure:    Prefix: PID
        ;"              Seq 1: Set ID - PID (Optional)
        ;"              Seq 2: Patient ID (Optional)
        ;"              Seq 3: Patient Identifier List (How should this be formatted for SSN)
        ;"              Seq 4: Alternate Patient ID (Optional)
        ;"              Seq 5: Patient Name (Last^First^MI)
        ;"              Seq 6: Mother's Maiden Name (Optional)
        ;"              Seq 7: Date/Time of Birth
        ;"              Seq 8 - 18 (Optional)
        ;"              Seq 19: SSN - Patient
        ;"              Seq 20-30 (Optional)
        ;"Result: returns string for HL7 message segment.
        ;"        or -1^Message for cases of error
        NEW TMGRESULT
        NEW PATIENTNAME,PATIENTSSN,PATIENTDOB,SEX
        SET TMGDFN=+$GET(TMGDFN)
        IF TMGDFN'>0 SET TMGRESULT="-1^NO DFN PROVIDED" GOTO PIDDN
        ;"
        SET PATIENTNAME=$$GET1^DIQ(2,TMGDFN_",",".01")
        SET PATIENTSSN=$$GET1^DIQ(2,TMGDFN_",",".09")
        SET PATIENTDOB=$$GET1^DIQ(2,TMGDFN_",",".03","I")
        SET SEX=$$GET1^DIQ(2,TMGDFN_",",.02,"I")
        SET PATIENTDOB=$$HL7DT(PATIENTDOB)
        IF PATIENTNAME="" SET TMGRESULT="-1^PATIENT NAME NOT FOUND" GOTO PIDDN
        IF PATIENTSSN["P" SET PATIENTSSN=$PIECE(PATIENTSSN,"P",1) ;"THIS LINE IS FOR TEST DATA ONLY
        IF (PATIENTSSN="")!(PATIENTSSN["P") SET TMGRESULT="-1^PATIENT SSN NOT FOUND/INVALID" GOTO PIDDN
        IF PATIENTDOB="" SET TMGRESULT="-1^PATIENT DOB NOT FOUND" GOTO PIDDN
        ;"
        SET $PIECE(TMGRESULT,"|",1)="PID"
        SET $PIECE(TMGRESULT,"|",3)=PATIENTSSN
        SET $PIECE(TMGRESULT,"|",5)=PATIENTNAME
        SET $PIECE(TMGRESULT,"|",7)=PATIENTDOB
        SET $PIECE(TMGRESULT,"|",8)=SEX
        SET $PIECE(TMGRESULT,"|",18)=TMGDFN
        SET $PIECE(TMGRESULT,"|",19)=PATIENTSSN
        SET TMGRESULT=TMGRESULT
PIDDN   QUIT TMGRESULT
        ;
GETNK1(TMGDFN) ;
        ;"Purpose: GET NEXT OF KIN INFORMATION SEGMENT
        ;"Input: TMGDFN -- patient IEN 
        ;"Result: returns string for HL7 message segment.
        ;"        or -1^Message for cases of error
        QUIT ""  ;<---- temporary.  Remove if/when NK1 implemented.
        NEW TMGRESULT
        SET TMGRESULT="NK1|"
        QUIT TMGRESULT
        ;
GETRXA(IENS) ;
        ;"Purpose: PHARMACY ADMINISTRATION SEGMENT
        ;"Input: IENS -- pointer to subfile 19009.03, expects dataien,cpien
        ;"Structure:    Prefix: RXA
        ;"              Seq 1: Sub-ID Coutner ("0")
        ;"              Seq 2: Admin ID Sub-ID Counter ("999")
        ;"              Seq 3: Date/Time Start of Admin
        ;"              Seq 4: Date/Time End of Admin
        ;"              Seq 5: Administered Code (What is format?)
        ;"              Seq 6: Administered Amount
        ;"              Seq 7: Administered Units ("DOSE")
        ;"              Seq 8: Admin Dosage Form (Optional)
        ;"              Seq 9: Administered Notes ("00" for Given, "01" for historical)
        ;"              Seq 10: Administering Provider (Optional IF 9 is "01", ELSE must be
        ;"                                              NPI^Provider Name^^^^^^^^^^^NP)
        ;"              Seq 11-22: Optional
        ;"Result: returns string for HL7 message segment.
        ;"        or -1^Message for cases of error
        NEW TMGRESULT
        ;"The following are temporary field numbers until we know what the final labels or numbers are
        ;"******Must change IF any change to Careplan template.
        NEW DATEFIELD SET DATEFIELD=2
        NEW VACCINEFIELD SET VACCINEFIELD=6
        ;"
        NEW ADMINDATE,VACCINE,TEMPDATE
        SET ADMINDATE=$$GET1^VEFACPU(19009.31,DATEFIELD_","_IENS,1)
        IF ADMINDATE="" SET TMGRESULT="ADMIN DATE NOT FOUND" GOTO RXADN
        D DT^DILF("T",ADMINDATE,.TEMPDATE)
        SET ADMINDATE=$PIECE(TEMPDATE,".",1)
        SET VACCINE=$$GET1^VEFACPU(19009.31,VACCINEFIELD_","_IENS,1)
        IF VACCINE="" SET TMGRESULT="VACCINE NOT FOUND IN CAREPLAN" GOTO RXADN
        SET VACCINE=$$GETCVX5(VACCINE)
        ;"
        NEW ADMINAMOUNT
        SET ADMINAMOUNT="0100"    ;"NEEDS TO BE ADDED
        ;"
        NEW ADMINBY SET ADMINBY="1093726085^TOPPENBERG,KEVIN S^^^^^^^^^^^NP"
        ;"
        SET $PIECE(TMGRESULT,"|",1)="RXA"
        SET $PIECE(TMGRESULT,"|",2)="0"
        SET $PIECE(TMGRESULT,"|",3)="999"
        SET $PIECE(TMGRESULT,"|",4)=ADMINDATE
        SET $PIECE(TMGRESULT,"|",5)=ADMINDATE
        SET $PIECE(TMGRESULT,"|",6)=VACCINE
        SET $PIECE(TMGRESULT,"|",7)=ADMINAMOUNT
        SET $PIECE(TMGRESULT,"|",8)="DOSE"
        SET $PIECE(TMGRESULT,"|",10)="00"
        SET $PIECE(TMGRESULT,"|",11)=ADMINBY
RXADN   QUIT TMGRESULT
        ;
GETRXR(IENS) ;
        ;"Purpose: GET PHARMACY ROUTE INFORMATION AND SITE INFO
        ;"Input: IENS -- pointer to subfile 19009.03, expects dataien,cpien
        ;"Result: returns string for HL7 message segment.
        ;"        or -1^Message for cases of error
        NEW TMGRESULT,ROUTEFIELD,ROUTE
        SET ROUTEFIELD=4
        SET ROUTE=$$GET1^VEFACPU(19009.31,ROUTEFIELD_","_IENS,1)
        SET ROUTE=$PIECE(ROUTE,"[",2),ROUTE=$PIECE(ROUTE,"]",1)
        SET $PIECE(TMGRESULT,"|",1)="RXR"
        SET $PIECE(TMGRESULT,"|",2)="IM"
        SET $PIECE(TMGRESULT,"|",3)=ROUTE
        QUIT TMGRESULT
        ;
GETOBX(IENS) ;
        ;"Purpose: Get OBSERVATION AND RESULT SEGMENT (ANY ADVERSE REACTIONS)
        ;"Input: IENS -- pointer to subfile 19009.03
        ;"Result: returns string for HL7 message segment.
        ;"        or -1^Message for cases of error
        NEW TMGRESULT,OBFIELD,OBSERVATION
        SET TMGRESULT=""
        SET OBFIELD=1
        SET OBSERVATION=$$GET1^VEFACPU(19009.31,OBFIELD_","_IENS,1)
        SET $PIECE(TMGRESULT,"|",1)="OBX"
        SET $PIECE(TMGRESULT,"|",3)="**"
        SET $PIECE(TMGRESULT,"|",5)=OBSERVATION
        SET $PIECE(TMGRESULT,"|",11)="F"
        QUIT TMGRESULT
        ;
GETCVX5(TEXT)  ;
        ;"PURPOSE: TO TRANSLATE THE CVX CODE INTO THE PROPER RXA-5 STRING
        ;"Input: TEXT -- Expected format: [CVX#####] 
        NEW RESULT
        SET TEXT=$PIECE(TEXT,"[CVX",2),TEXT=$PIECE(TEXT,"]",1)
        IF TEXT=141 SET RESULT="141^INFLUENZA/INJ^CVX"
        ELSE  IF TEXT=08 SET RESULT="08^HEP B^CVX"
        ELSE  IF TEXT=62 SET RESULT="62^HPV,QUAD^CVX"
        ELSE  IF TEXT=33 SET RESULT="33^PNEUMOCOCCAL^CVX"
        ELSE  SET RESULT=TEXT_"^VACCINE NAME NOT DEFINED IN GETCVX5-TMGIMM1^CVX"
        QUIT RESULT
        ;
HL7DT(DT) ;
        ;"Purpose: return an HL7 format date, without seconds
        NEW RESULT SET RESULT=$$FMTHL7^XLFDT(DT)
        SET RESULT=$PIECE(RESULT,"-",1)
        QUIT RESULT
        ;        
ADD(OUT,VALUE) ;
        ;"Purpose: Add value to growing HL7 message
        ;"Input: OUT -- PASS BY REFERENCE, An OUT parameter
        ;"       VALUE -- The value to be added
        ;"Result: 0 IF OK, or -1^Message IF error
        ;"OUT format:  OUT(1)=<first line>
        ;"             OUT(2)=<2nd line> ...
        NEW RESULT SET RESULT=0
        IF +VALUE=-1 SET RESULT=VALUE GOTO ADDDN
        IF VALUE="" GOTO ADDDN
        NEW INDEX SET INDEX=1+$ORDER(OUT(""),-1)
        SET OUT(INDEX)=VALUE
ADDDN   QUIT RESULT   
        ;
OUTPUT(ARRAY) ;
        ;"Purpose: to output HL7 text to file system etc.        
        NEW TMGRESULT SET TMGRESULT="1^SUCCESSFUL"
        NEW FHANDLE SET FHANDLE="OUTFILE"
        NEW FPATH SET FPATH="/tmp/"
        NEW FNAME SET FNAME=$$UNIQUE^%ZISUTL("Immunization_HL7.txt")
        DO OPEN^%ZISH(FHANDLE,FPATH,FNAME,"W")
        IF POP DO  GOTO OPDN
        . SET TMGRESULT="-1^Unable to open file for writing: "_FPATH_FNAME
        USE IO
        NEW INDEX SET INDEX=0
        FOR  SET INDEX=$ORDER(ARRAY(INDEX)) QUIT:(+INDEX'>0)  DO
        . NEW S SET S=$GET(ARRAY(INDEX)) 
        . QUIT:S=""
        . WRITE S,$CHAR(13)
        WRITE $CHAR(10)
        DO CLOSE^%ZISH(FHANDLE)
OPDN    QUIT TMGRESULT
        ;
GETIENS(TIUIEN,TMGDFN,IENS) ;
        ;"Purpose: Get IENS to careplan for given TIUIEN
        ;"Input: TIUIEN -- IEN in 8925
        ;"       TMGDFN  -- PASS BY REFERENCE, AN OUT PARAMETER 
        ;"       IENS -- PASS BY REFERENCE, AN OUT PARAMETER
        ;"Result: 1^Success, or -1^Message IF error
        NEW TMGRESULT,CPIEN,ENTRYIEN
        SET TMGRESULT="1^SUCCESSFUL"        
        SET CPIEN=+$ORDER(^VEFA(19009,"TIU",TIUIEN,0))
        IF CPIEN=0 SET TMGRESULT="-1^CarePlan not found" GOTO GTIDN 
        SET ENTRYIEN=+$ORDER(^VEFA(19009,"TIU",TIUIEN,CPIEN,0))
        IF ENTRYIEN=0 SET TMGRESULT="-1^CarePlan entry not found" GOTO GTIDN
        SET TMGDFN=+$PIECE($GET(^VEFA(19009,CPIEN,0)),"^",1)
        IF TMGDFN=0 SET TMGRESULT="-1^DFN not found" GOTO GTIDN
        SET IENS=ENTRYIEN_","_CPIEN_","
GTIDN   QUIT TMGRESULT        
        ;"------------------------------------------------------
        ;"------------ Debug code ------------------------------
        ;"------------------------------------------------------
TEST ;
        ;"Purpose: To see test output
        NEW ERR
        WRITE "--------VXQ SECTION--------",!
        WRITE $$GETVXQ(74268,.ERR),!,!
        IF $DATA(ERR) WRITE "ERROR FOUND:"_ERR,1
        WRITE "--------MSH SECTION--------",!
        WRITE $$GETMSH,!,!
        WRITE "--------QRD SECTION--------",!
        WRITE $$GETQRD(74268),!,!
        WRITE "--------QRF SECTION--------",!
        WRITE $$GETQRF,!,!
        WRITE "--------PID SECTION--------",!
        WRITE $$GETPID(74268),!,!
        WRITE "--------RXA SECTION--------",!
        WRITE $$GETRXA("2,84"),!,!
        QUIT
        ;
TESTTIU ;
        NEW DIC,X,Y
        SET DIC=8925,DIC(0)="MAEQ"
        DO ^DIC WRITE !
        IF +Y>0 IF $$V04(+Y)
        QUIT
        ;        
