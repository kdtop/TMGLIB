TMGRPC1E ;TMG/kst-RPC Functions ;07/21/10, 2/2/14
         ;;1.0;TMG-LIB;**1**;07/21/10
 ;
 ;"TMG RPC FUNCTIONS especially related to ADT in CPRS
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
 ;" RPC -- Public Functions.
 ;"=======================================================================
 ;"
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"
 ;"=======================================================================
 ;"Dependancies:
 ;"=======================================================================
 ;
 ;"NOTE: MENU OPTION: DG ADMIT PATIENT --> SET DGPMT=1 DO PAT^DGPMV
 ;
 ;"Code below from:
 ;"DGPMV ;ALB/MRL/MIR - PATIENT MOVEMENT DRIVER; 10 MAR 89
 ;";5.3;Registration;**60,200,268**;Aug 13, 1993
 ;
ADT(TMGOUT,TMGINFO) ;
 ;"Purpose: Provide API of interactive ADT code
 ;"Input: TMGOUT -- PASS BY REFERENCE.  Used for output.  (See below)
 ;"       TMGINFO -- PASS BY REFERENCE.  Used for pre-loaded user input.
 ;"           TMGINFO("TYPE") -- for setting DGPMT.  Should be as follows:
 ;"                     API USE       VALUE OF TYPE
 ;"                     ------         --------------
 ;"                     admit                 1
 ;"                     transfer              2
 ;"                     discharge             3
 ;"                     check-in              4
 ;"                     check-out             5
 ;"                     t.s. transfer         6
 ;"           TMGINFO("PATIENT")=IEN (aka DFN) of patient, or Patient Name
 ;"                              in format that Fileman can look up.
 ;"           TMGINFO("KNOWN-DEAD")=1 (OPTIONAL).  Set to 1 IF code should
 ;"                             still process ADT change, though patient is dead.
 ;"           TMGINFO("DATE")=Date of ADT.
 ;"Output: TMGOUT filled with results:
 ;"             TMGOUT(0)=-1^Error message (may include CRLF's to split line)
 ;"Result: none
 ;
START ;
        NEW DFN  ;"Prevent this code from changing CPRS's active patient
        NEW TMGCT SET TMGCT=1
        NEW X,Y
        SET TMGOUT(0)="1^OK"
        SET DGPMT=+$GET(TMGINFO("TYPE"))
        IF DGPMT'>0 GOTO Q
PAT ;
        KILL ORACTION,ORMENU
        DO LO  ;"Ensure environment
        IF +TMGOUT(0)=-1 GOTO Q
PAT1 ;
        SET DFN=$GET(TMGINFO("PATIENT"))
        IF DFN="" DO  GOTO Q
        . SET TMGOUT(0)="-1^PATIENT name or IEN not provided."
        IF +DFN=DFN DO  ;"Look up patient
        . SET Y(0)=$GET(^DPT(DFN,0))
        ELSE  DO  ;"Look up patient
        . NEW DIC SET DIC="^DPT("
        . SET DIC(0)="MZ"   ;"'Z' --> return Y(0)
        . IF (DGPMT'=1)&(DGPMT'=4) DO
        . . SET DIC("S")="I $D(^DGPM($S(DGPMT'=5:""APTT1"",1:""APTT4""),+Y))"
        . SET X=DFN ;"At this point, DFN probably is a string of patient NAME
        . DO ^DIC
        . SET DFN=+Y
        IF +DFN'>0 DO  GOTO Q
        . SET TMGOUT(0)="-1^Unable to locate PATIENT to act on.  Got '"_DFN_"'"
        ;
        IF DGPMT'=5 GOTO OREN   ;"5=Check-out
        SET DGPMN=0  ;"signal IF NEW patient 0=not new
        DO SPCLU     ;"Special (quick) look-up for check-out lodgers
        IF DGER GOTO Q
        ;
OREN    SET DGPMN=""
        SET Y(0)=$GET(^DPT(DFN,0))
        IF $$LODGER(DFN)&(DGPMT=1) DO  GOTO Q   ;"1=Check-in
        . SET TMGOUT(0)="-1^Patient is a lodger...you can not add an admission!"
        . KILL DGPMDER
        ;
MOVE    SET XQORQUIT=1,DGPME=0
        DO UC
        IF (DGPMT=1)!(DGPMT=4) GOTO CHK    ;"Skip IF Admit(1) or Check-In(4)
        IF '$D(^DGPM("APTT"_$S(DGPMT'=5:1,1:4),DFN)) DO   GOTO Q
        . SET TMGOUT(0)="-1^'"_$P(Y(0),"^",1)_"' HAS NEVER BEEN "
        . SET TMGOUT(0)=TMGOUT(0)_$S(DGPMT'=5:"ADMITTED",1:"CHECKED-IN")
        . SET TMGOUT(0)=TMGOUT(0)_" TO THE ADMISSIONS MODULE."
        ;
CHK     I 'DGPME,$D(^DPT(DFN,.35)),+^(.35) S Y=+^(.35) D DIED(.Y)
        D NEWODS  ;"See IF ODS software is on & Ensure period of service is defined
        IF +TMGOUT(0)=-1 GOTO Q
        I $S('DGODSON:0,'$D(^DPT(DFN,.32)):1,'$D(^DIC(21,+$P(^(.32),"^",3),0)):1,1:0) S DGPME=1
        IF DGPME GOTO Q

        ;"DO DGPMV1
        ;"--- Inserting code from ^DGPMV1 code ---------------
        KILL VAIP SET VAIP("D")="L",VAIP("L")=""
        DO INP  ;"Set-up inpt vbls needed (mimic VAIP array) Non-interactive
        DO Q^VADPT3  ;"Cleanup vars.  Non-interactive
        ;"IF '$D(DFN)#2 GOTO Q1   ;Test already performed.  Not needed here.
        SET X=$P($G(^DPT(DFN,0)),"^",14) ;"0;4 is MEANS TEST Subfile/Multiple
        IF X DO  GOTO Q1
        . SET TMGOUT(0)="-1^Means Test needed. Unable to process at this time."
        . ;"DO DOM^DGMTR
        . ;"IF '$G(DGDOM) DO DIS^DGMTU(DFN)
        . ;"KILL DGDOM
        ELSE  DO
        . DO ADDINFO("Means Test not required based on available information")
        DO CS
        ;
NEXT    ;"NOTE: The <MORE> option allows viewing/editing of various fields of
        ;"      admission, and is not appropriate for an API, so it was cut out.
C       SET DGPM2X=0 ;"were DGPMVI variables SET 2 times?
        IF DGPMT=1,+DGPMVI(2)=4,'$D(^DGPM("APTT1",DFN)) DO  GOTO Q1
        . SET TMGOUT(0)="-1^THIS PATIENT IS A LODGER AND HAS NO ADMISSIONS ON FILE.  MUST BE CHECKED OUT PRIOR TO CONTINUING."
        IF DGPMT=4,"^1^2^6^7^"[("^"_+DGPMVI(2)_"^"),'$D(^DGPM("APTT4",DFN)) DO  Q
        . SET TMGOUT(0)="-1^THIS PATIENT IS AN INPATIENT AND HAS NO LODGER MOVEMENTS ON FILE.  MUST BE DISCHARGED  PRIOR TO CONTINUING"
        IF "^1^2^6"[("^"_+DGPMVI(2)_"^")&("^4^5^"[("^"_DGPMT_"^"))!(+DGPMVI(2)=3&(DGPMT=5)) DO
        . DO LODGER1   ;"from LODGER^DGPMV10  Non-interactive
        . SET DGPM2X=1
        IF (+DGPMVI(2)=4)&("^1^2^3^6^"[("^"_DGPMT_"^"))!((+DGPMVI(2)=5)&(DGPMT=3)) DO
        . KILL VAIP
        . SET VAIP("D")="L"
        . DO INP  ;"Set-up inpt vbls needed (mimic VAIP array) Non-interactive
        . SET DGPM2X=1
        LOCK +^DGPM("C",DFN):0  ;"Block 2 ppl from moving same patient at same time
        IF '$T DO  Q
        . SET TMGOUT(0)="-1^** This patient's inpatient or lodger activity is being "
        . SET TMGOUT(0)=TMGOUT(0)_"edited by another employee.  Please try again later. **"
        DO DGPMV2  ;"Patient Movement Processor  FINISH!!!
        LOCK -^DGPM("C",DFN)
        ;"QUIT  ;"Continue with movement entry
Q1      ;"DO TMGCLEAN
        ;"Q
        ;"--- Resumption of DGPMV code ---------------
Q       DO TMGCLEAN
        GOTO TMGQUIT
        ;
        ;"====================================================================
DIED(Y) ;
        IF ($GET(TMGINFO("KNOWN-DEAD"))=1) QUIT
        XECUTE ^DD("DD")  ;"Convert date in Y to external form
        SET TMGOUT(0)="-1^PATIENT EXPIRED '"_Y_"'. Can not process ADT action."
        SET DGPME=1
        QUIT
        ;
UC      ;"Set type of mvt literal
        N T
        S T="ADMISSION^TRANSFER^DISCHARGE^LODGER CHECK-IN^CHECK-OUT LODGER^SPECIALTY TRANSFER^ROOM-BED CHANGE"
        SET DGPMUC=$P(T,"^",DGPMT)
        IF (DGPMT=6),$D(DGPMPC) SET DGPMUC="PROVIDER CHANGE"
        QUIT
        ;
CA      ;"Bypass interactive process and allows editing of past admission
        ;"    mvts
        ;"
        ;"    input: DFN
        ;"           DGPMT  - mvt transaction type
        ;"           DGPMCA - coresp. adm
        ;"
        ;"   output: Y - the mvt entry added/edited
        ;"
        DO UC
        KILL VAIP SET VAIP("E")=DGPMCA
        N DGPMCA
        DO INP  ;"Set-up inpt vbls needed (mimic VAIP array) Non-interactive
        SET DGPMBYP=""
        DO C
        SET Y=DGPMBYP
        KILL DGPMUC,DGPMBYP
        QUIT
        ;
DISPO   ;"Called from admission disposition types
        ;"input  DGPMSVC: SVC OF WARD REQUIRED (FROM DISPOSITION TYPE FILE)
        ;"       DFN:     patient file IFN (this variable is NOT killed on exit)
        ;"output DGPMDER=disposition error?? - FOR FUTURE USE
        ;
        SET DGPMT=1,(DGPML,DGPMMD)=""
        KILL DGPMDER,VAIP
        SET VAIP("D")="L"
        DO UC^DGPMV
        DO INP  ;"Set-up inpt vbls needed (mimic VAIP array) Non-interactive
        DO NOW^%DTC
        IF DGPMVI(1)&('DGPMDCD!(DGPMDCD>%)) DO  GOTO DISPOQ
        . SET TMGOUT(0)="-1^Patient is already an inpatient...editing the admission is not allowed."
        . KILL DGPMDER
        IF $$LODGER(DFN) DO  GOTO DISPOQ
        . SET TMGOUT(0)="-1^Patient is a lodger...you can not add an admission!"
        . KILL DGPMDER
        ;"next block should be involked in future release to error IF wrong service
        ;"I DGPMVI(1)&('DGPMDCD!(DGPMDCD>%)) DO  GOTO DISPOQ
        ;". SET DGPMDER=$S(DGPMSVC="H"&("^NH^D^"'[("^"_DGPMSV_"^")):0,DGPMSVC=DGPMSV:0,1:1)
        ;". SET TMGOUT(0)="-1^Current inpatient, but not to proper service"
        DO NEW^DGPMVODS
        IF $S('DGODSON:0,'$D(^DPT(DFN,.32)):1,'$D(^DIC(21,+$P(^(.32),"^",3),0)):1,1:0) SET DGPME=1
        SET DEF="NOW",DGPM1X=0
        DO SEL  ;^DGPMV2
        IF '$D(DGPMDER) SET DGPMDER=1
        ;
DISPOQ  DO TMGCLEAN
        QUIT
        ;
LODGER(DFN) ;" Determine lodger status
        ;"Input: DFN=patient IEN
        ;"Results: '1' IF currently a lodger, '0' otherwise
        N DGPMDCD,DGPMVI,I,X
        DO LODGER1  ;"Set-up necessary variables IF getting last lodger episode
        QUIT $GET(DGPMVI(2))=4
        ;
        ;"======== End of code from DGPMV ====================================
        ;
        ;"======== Start of code from DGPMV1 =================================
DGPMV1  ;"(code moved above)
        ;
 ;"L       DO ENED^DGRP   ;"??
 ;"        GOTO C         ;"??
        ;"======== End of code from DGPMV1 =================================
        ;
CS      ;"From CS^DGPMV10
        ;"Current Status
        ;"First print primary care team/practitioner/attending
        DO PCMM(DFN,DT) ;"Non-interactive
        SET X=$S('DGPMT:1,DGPMT<4:2,DGPMT>5:2,1:3) ;"DGPMT=0 IF from pt inq (DGRPD)
        IF '$D(^DGPM("C",DFN)) DO  GOTO CSQ
        . DO ADDINFO("Status : PATIENT HAS NO INPATIENT OR LODGER ACTIVITY IN THE COMPUTER")
        . DO CS2
        SET A=$S("^3^5^"[("^"_+DGPMVI(2)_"^"):0,1:+DGPMVI(2))
        DO ADDINFO("Status: "_$S('A:"IN",1:"")_"ACTIVE "_$S("^4^5^"[("^"_+DGPMVI(2)_"^"):"LODGER",1:"INPATIENT"),0)
        IF 'A GOTO CS1
        DO ADDINFO("-",0)
        SET X=+DGPMVI(4)
        IF X=1 DO ADDINFO("on PASS") GOTO CS1
        IF "^2^3^25^26^"[("^"_X_"^") DO ADDINFO("on "_$S("^2^26^"[X:"A",1:"U")_"A") GOTO CS1
        IF "^13^43^44^45^"[("^"_X_"^") DO ADDINFO("ASIH") GOTO CS1
        IF X=6 DO ADDINFO("OTHER FAC") GOTO CS1
        DO ADDINFO("on WARD")
CS1     SET TMGS=""
        IF +DGPMVI(2)=3,$D(^DGPM(+DGPMVI(17),0)) DO
        . SET TMGS="Status : "_$S($D(^DG(405.1,+$P(^(0),"^",4),0)):$P(^(0),"^",1),1:"UNKNOWN")
        IF "^3^4^5^"'[("^"_+DGPMVI(2)_"^"),$D(^DPT(DFN,"DAC")),($P(^("DAC"),"^",1)="S") DO
        . SET TMGS=TMGS_"  (Seriously ill)"
        DO ADDINFO(TMGS)
        IF +DGPMVI(19,1) DO
        . SET TMGOUT(TMGCT)="INFO^Patient chose not to be included in the Facility Directory for this admission",TMGCT=TMGCT+1
        DO ADDINFO($S("^4^5^"'[("^"_+DGPMVI(2)_"^"):"Admitted    ",1:"Checked-in  ")_": "_$P(DGPMVI(13,1),"^",2))
        DO ADDINFO($S("^4^5^"[("^"_+DGPMVI(2)_"^"):"Checked-out",+DGPMVI(2)=3:"Discharged ",1:"Transferred")_"    : "_$S("^1^4^"'[("^"_+DGPMVI(2)_"^"):$P(DGPMVI(3),"^",2),$P(DGPMVI(3),"^",2)'=$P(DGPMVI(13,1),"^",2):$P(DGPMVI(3),"^",2),1:""))
        DO ADDINFO("Ward: "_$E($P(DGPMVI(5),"^",2),1,24))
        DO ADDINFO("Room-Bed: "_$E($P(DGPMVI(6),"^",2),1,21))
        IF "^4^5^"'[("^"_+DGPMVI(2)_"^") DO
        . DO ADDINFO("Provider: "_$E($P(DGPMVI(7),"^",2),1,26))
        . DO ADDINFO("Specialty: "_$E($P(DGPMVI(8),"^",2),1,21))
        DO ADDINFO("Attending: "_$E($P(DGPMVI(18),"^",2),1,26))
        DO CS2
        SET DGPMIFN=DGPMVI(13)
        IF +DGPMVI(2)'=4&(+DGPMVI(2)'=5) DO
        . DO ^DGPMLOS
        . DO ADDINFO("Admission LOS: "_+$P(X,"^",5)_"  Absence days: "_+$P(X,"^",2)_"  Pass Days: "_+$P(X,"^",3)_"  ASIH days: "_+$P(X,"^",4))
        KILL A,C,I,J,X
CSQ     QUIT
 ;
CS2     ;"Get additional information fields for admission screening.
        IF DGPMT'=1 QUIT
        SET DGHOLD=$S($D(^DPT(DFN,0)):^(0),1:"")
        DO ADDINFO("Religion : "_$S($D(^DIC(13,+$P(DGHOLD,U,8),0)):$E($P(^(0),U),1,24),1:""))
        DO ADDINFO("Marital Status : "_$S($D(^DIC(11,+$P(DGHOLD,U,5),0)):$P(^(0),U),1:""))
        SET DGHOLD=$S($D(^DPT(DFN,.36)):$P(^(.36),U),1:"")
        DO ADDINFO("Eligibility : "_$S($D(^DIC(8,+$P(DGHOLD,U),0)):$P(^(0),U),1:""),0)
        SET DGHOLD=$S($D(^DPT(DFN,.361)):^(.361),1:"")
        IF $P(DGHOLD,U)]"" DO
        . DO ADDINFO(" ("_$P($P($P(^DD(2,.3611,0),U,3),$P(DGHOLD,U)_":",2),";")_")")
        ELSE  DO
        . DO ADDINFO(" (NOT VERIFIED)")
        KILL DGHOLD
        QUIT
 ;
LODGER1 ;"Set-up necessary variables IF getting last lodger episode
        ;"Only need 1,2,13,17 - date/time,TT,check-in IFN,check-out IFN
        SET I=$O(^DGPM("ATID4",DFN,0)),I=$O(^(+I,0))
        SET X=$S($D(^DGPM(+I,0)):^(0),1:"")
        IF 'X DO NULL Q
        IF $D(^DGPM(+$P(X,"^",17),0)) DO  QUIT
        . SET (DGPMDCD,DGPMVI(1))=+^(0)
        . SET DGPMVI(2)=5
        . SET DGPMVI(13)=I
        . SET DGPMVI(17)=$P(X,"^",17)
        SET (DGPMDCD,DGPMVI(17))=""
        SET DGPMVI(1)=+X
        SET DGPMVI(2)=4
        SET DGPMVI(13)=I
        Q
        ;
NULL    SET DGPMDCD=""
        FOR I=1,2,13,17 SET DGPMVI(I)=""
        Q
        ;
        ;"==================================================================
INP     ;"From INP^DGPMV10
        ;"Set-up inpt vbls needed (mimic VAIP array)
        ;"NOTE: (INP^DGPMV10 is Called from scheduling, too)
        ;
        NEW DGAP,DGPP,DGTS,DGX,IFN,I,J,NOWI
        DO NOW^%DTC
        SET (VAX("DAT"),NOW)=%
        SET NOWI=9999999.999999-%
        IF '$D(VAIP("E")) DO LAST^VADPT3
        FOR I=1:1:8,13,17 SET DGPMVI(I)=""
        FOR I=13,19 SET DGPMVI(I,1)=""
        SET DGPMVI(1)=$S($D(VAIP("E")):VAIP("E"),1:E) ;"Use ifn of last mvt from VADPT call or one passed from DGPMV
        SET DGX=$G(^DGPM(+DGPMVI(1),0)),DGPMVI(2)=$P(DGX,"^",2),DGPMVI(4)=$P(DGX,"^",18)
        SET Y=+DGX X ^DD("DD") SET DGPMVI(3)=$P(DGX,"^",1)_"^"_Y
        SET DGPMVI(5)=$P(DGX,"^",6)_"^"_$S($D(^DIC(42,+$P(DGX,"^",6),0)):$P(^(0),"^",1),1:"")
        SET DGPMVI(6)=$P(DGX,"^",7)_"^"_$S($D(^DG(405.4,+$P(DGX,"^",7),0)):$P(^(0),"^",1),1:"")
        SET DGPMVI(13)=$P(DGX,"^",14)
        IF "^3^5^"[("^"_DGPMVI(2)_"^") DO GETWD ;"Get from ward IF d/c or check-out
        SET DGX=$G(^DGPM(+DGPMVI(13),0))
        IF DGX]"" DO
        . SET Y=+DGX X ^DD("DD")
        . SET DGPMVI(13,1)=$P(DGX,"^",1)_"^"_Y
        . SET DGPMVI(17)=$P(DGX,"^",17)
        . IF $D(DGPMSVC) SET DGPMSV=$P($G(^DIC(42,+$P(DGX,"^",6),0)),"^",3)
        SET DGPMDCD=$S($D(^DGPM(+DGPMVI(17),0)):$P(^(0),"^",1),1:"")
        SET (DGTS,DGPP,DGAP)="" ;"t.s., primary care physician, attending
        NEW TMGDN SET TMGDN=0
        SET I=NOWI
        FOR  SET I=$O(^DGPM("ATS",DFN,+DGPMVI(13),I)) Q:'I  DO  Q:(DGTS&DGPP&DGAP)
        . SET J=0
        . FOR  SET J=$O(^DGPM("ATS",DFN,+DGPMVI(13),I,J)) Q:'J  DO  Q:(DGTS&DGPP&DGAP)
        . . SET IFN=0
        . . FOR   SET IFN=$O(^DGPM("ATS",DFN,+DGPMVI(13),I,J,IFN)) Q:'IFN  DO  Q:(DGTS&DGPP&DGAP)
        . . . DO TS1
TSQ     SET DGPMVI(7)=DGPP
        SET DGPMVI(8)=DGTS
        SET DGPMVI(18)=DGAP
        SET DGX=$G(^DGPM(+DGPMVI(13),0))
        IF $P(DGX,"^",2)=1 D
        . SET DGX=$G(^DGPM(+DGPMVI(13),"DIR"))
        . SET DGX=$P(DGX,"^",1)
        . IF DGX="" SET DGX=$S('DGPMDCD:1,(DGPMDCD<3030414.999999):"",1:1) Q:DGX=""
        . SET DGPMVI(19,1)=DGX_"^"_$$EXTERNAL^DILFD(405,41,,DGX)
        DO Q^VADPT3  ;"non interactive cleanup of vars
        QUIT
 ;
TS1     ;"Set DGTS, DGPP, and DGAP
        Q:'$D(^DGPM(IFN,0))  SET DGX=^(0)
        IF 'DGPP,$D(^VA(200,+$P(DGX,"^",8),0)) DO
        . SET Y=$P(DGX,"^",8)_"^"_$P(^(0),"^")
        . SET DGPP=Y
        IF 'DGAP,$D(^VA(200,+$P(DGX,"^",19),0)) DO
        . SET Y=$P(DGX,"^",19)_"^"_$P(^(0),"^")
        . SET DGAP=Y
        IF 'DGTS,$D(^DIC(45.7,+$P(DGX,"^",9),0)) DO
        . SET DGTS=$P(DGX,"^",9)_"^"_$P(^(0),"^")
        QUIT
        ;
GETWD ;"Get the from ward IF last mvt is discharge or check-out
        NEW I,J
        IF DGPMVI(2)=5 SET J=DGPMVI(13) DO SETWD Q
        SET I=0
        FOR  SET I=$O(^DGPM("APMV",DFN,DGPMVI(13),I)) Q:'I!+DGPMVI(5)  DO
        . SET J=0
        . FOR  SET J=$O(^DGPM("APMV",DFN,DGPMVI(13),I,J)) Q:'J  DO  QUIT:+DGPMVI(5)
        . . DO SETWD
        QUIT
        ;
SETWD ;"set ward and room-bed variables for discharge/check-out mvts
        SET X=$G(^DGPM(J,0))
        IF $D(^DIC(42,+$P(X,"^",6),0)) DO
        . SET DGPMVI(5)=$P(X,"^",6)_"^"_$P(^(0),"^",1)
        IF $D(^DG(405.4,+$P(X,"^",7),0)) DO
        . SET DGPMVI(6)=$P(X,"^",7)_"^"_$P(^(0),"^",1)
        QUIT
        ;
        ;"=============Code from DGPMV2======================================
DGPMV2 ;"PATIENT MOVEMENT PROCESSOR
        IF '$D(DGPMVI) DO  GOTO Q2
        . SET TMGOUT(0)="-1^INPATIENT ARRAY NOT DEFINED...MODULE ENTERED INCORRECTLY"
        KILL DGPME  ;"Error message
        SET DGPMMD="",DEF="NOW",DGPM1X=0
        DO S
        ;"//kt start added ------------
        NEW TMGDT
        DO
        . NEW X,Y,%DT SET %DT="T"
        . SET X=$GET(TMGINFO("DATE"))
        . DO ^%DT
        . IF +Y'>0 DO  QUIT
        . . SET TMGOUT(0)="-1^Invalid Date/Time provided.  Got '"_X_"'"
        . SET (TMGDT,DGPMDCD)=+Y  ;"Exclude time portion
        IF +TMGOUT(0)=-1 GOTO Q2
        ;"//kt end add ----------------
        IF "^1^4^5^"[("^"_DGPMT_"^") GOTO MV2B
        DO PTF  ;"Non-interactive
        IF $D(DGPME) DO  GOTO Q2
        . SET TMGOUT(0)="-1^"_DGPME
MV2B    IF (DGPMT'=3)&(DGPMT'=5) GOTO MV2C   ;"3=discharge, 5=check-out
        KILL DGPME
        IF DGPMDCD DO  GOTO Q2
        . DO OLD  ;"For previous entries (discharges and check-outs) skip select
        SET DGPML="",DGPM1X=1
        GOTO NEWL
        ;
MV2C    DO NOW^%DTC  ;"puts current date/time into % in FM format (and date only X)
        DO @("S"_DGPMT)   ;"e.g DO S1 or DO S2 etc (functions below)
        SET DGPML=$S($D(^UTILITY("DGPMVN",$J,1)):$P(^(1),"^",2),1:"")
        KILL C,D,I,J,N
        IF $S('DGPMDCD:1,DGPMDCD>%:1,DGPM2X:1,1:0)&$S(DGPMT=1:1,DGPMT=4:1,1:0) DO
        . SET DGPMMD=DGPML
        IF $S('DGPMDCD:0,DGPMT=3:1,DGPMT=5:1,DGPMDCD'>%:1,1:0)&($S(DGPMT=1:0,DGPMT=4:0,1:1)) DO
        . SET DGPMMD=DGPML,DEF=""
        IF $S(DGPMT=2:1,DGPMT=6:1,1:0),DGPMDCD,(DGPMDCD<%) DO
        . SET DEF=""
SEL     IF $DATA(DGPME) DO  GOTO Q2  ;"if no PTF, QUIT all the way out, don't reprompt
        . SET ^TMGOUT(0)="-1^"_DGPME
        KILL DGPME
        IF DGPMMD DO
        . SET Y=DGPMMD X ^DD("DD") SET DEF=Y
NEWL    SET DGX=$S(DGPMT=5:7,DGPMT=6:20,1:0)
        IF DGX DO
        . SET DGONE=1
        . IF $O(^DG(405.1,"AM",DGX,+$O(^DG(405.1,"AM",DGX,0)))) SET DGONE=0
        IF 'DGX SET DGONE=0
        IF DGPML DO  GOTO Q2    ;"If implemented, remove GOTO Q2
        . DO DGPMV20^TMGRPC1F   ;"Display Dates For Selection
        . SET TMGOUT(0)="-1^ADT requires interactive selection not yet implemented in API"
        ;"IF $D(^UTILITY("DGPMVN",$J,7)) W !?22,"Enter '?' to see more choices"
SEL2    SET DGPMN=0
        ;"W !!
        ;"W:'DGPM1X "Select "
        ;"W DGPMUC," DATE:  ",DEF
        ;"W $S(DEF]"":"// ",1:"")
        ;"R X:DTIME
        ;"G Q:'$T!(X["^")
        ;"IF X["?" DO SHOW G SEL2
        SET X=$GET(TMGINFO("DATE"))
        IF X["?" DO  GOTO Q2
        . DO SHOW
        . SET TMGOUT(0)="-1^Code calls for user interaction not yet implemented in API"
        DO UP^DGHELP  ;"Non-interactive UPPERCASE()
        IF $S($E(X,1,3)="NOV":0,$E(X)="N":1,X=""&(DEF="NOW"):1,1:0)=0 GOTO SEL2B
        DO NOW^%DTC ;"puts FM time into %
        SET DGPMN=1,(DGZ,Y)=% X ^DD("DD")
        ;"WRITE "  (",Y,")"
        SET Y=DGZ
        IF (DEF="NOW")!(DGPMT=2)!(DGPMT=6) GOTO CONT
        DO ERR
        GOTO Q2  ;"G SEL
        ;
SEL2B   IF X="",DGPMMD]"" SET Y=DGPMMD G CONT
        IF X?1N.N,$D(^UTILITY("DGPMVN",$J,+X)) SET (Y,DGZ)=$P(^(+X),"^",2) X ^DD("DD") W "  (",Y,")" SET Y=DGZ G CONT
        IF X=+X,(X<10000),'$D(^UTILITY("DGPMVN",$J,+X)) DO ERR GOTO Q2
        SET %DT="SXT"  ;"was SEXT
        SET %DT(0)="-NOW"
        DO ^%DT  ;"Output in Y
        IF $S('Y:1,$D(^UTILITY("DGPMVD",$J,+Y)):0,Y'?7N1".".N:1,1:0) DO ERR GOTO Q2
        IF $D(^UTILITY("DGPMVD",$J,+Y)) GOTO CONT
        SET DGPMN=1
        IF $S(DGPMMD']"":0,DGPMT=2:0,DGPMT=6:0,1:1)!($P(Y,".",2)']"") DO ERR GOTO Q2
CONT    SET DGPMY=+Y  ;"<----- ADT date/time.
        SET DGPMDA=$S($D(^UTILITY("DGPMVD",$J,+Y)):+^(Y),1:"")
        IF DGPMT=1!(DGPMT=4) DO
        . SET DGPMCA=+DGPMDA
        . SET DGPMAN=$S($D(^DGPM(DGPMCA,0)):^(0),1:DGPMY)
        KILL %DT
        DO DGPMV21   ;"now not interactive
        IF DGPMT=1&DGPMN DO SCHDADM^DGPMV22   ;"FINISH!!
        IF DGPMY DO ^DGPMV3   ;"FINISH!!
        IF $D(DGPME) DO  GOTO Q2
        . SET TMGOUT(0)="-1^"_DGPME
        . ;"W:DGPME'="***" !,DGPME
        GOTO Q2
        ;
ERR     SET TMGOUT(0)="-1^NOT A VALID SELECTION...CHOOSE BY DATE/TIME OR NUMBER.  "
        IF DGPMN SET TMGOUT(0)=TMGOUT(0)_"NEW MOVEMENT ENTRIES MUST INCLUDE A DATE AND TIME."
        Q
 ;
SHOW    ;"W !,"CHOOSE FROM" SET %DT="RSE" W ! F I=0:0 SET I=$O(^UTILITY("DGPMVN",$J,I)) Q:'I  DO WR^DGPMV20
        DO ADDINFO("CHOOSE FROM")
        SET %DT="RSE"
        F I=0:0 SET I=$O(^UTILITY("DGPMVN",$J,I)) Q:'I  DO
        . DO WR^TMGRPC1F   ;"was WR^DGPMV20  -- add options into ADDINFO
        ;"W ! DO HELP^%DTC
        KILL I,I1,N,D,C,%DT
        QUIT
 ;
S       SET DGPMAN=$S('DGPMVI(1):0,$D(^DGPM(+DGPMVI(13),0)):^(0),1:0)
        SET DGPMCA=$S(DGPMAN:DGPMVI(13),1:"")
        QUIT
        ;
S1      SET C=0   ;"Setup for Admitions
        FOR I=0:0 SET I=$O(^DGPM("ATID1",DFN,I)) Q:'I  DO
        . SET N=$O(^(I,0))
        . IF $D(^DGPM(+N,0)) DO
        . . SET D=^(0),C=C+1
        . . SET ^UTILITY("DGPMVN",$J,C)=N_"^"_D
        . . SET ^UTILITY("DGPMVD",$J,+D)=N
        QUIT
        ;
S2      SET C=0   ;"Setup for Transfers
        FOR I=0:0 SET I=$O(^DGPM("APMV",DFN,DGPMCA,I)) Q:'I  DO
        . SET N=$O(^(+I,0))
        . IF $D(^DGPM(+N,0)),($P(^(0),"^",2)=2) DO
        . . SET D=^(0),C=C+1
        . . SET ^UTILITY("DGPMVN",$J,C)=N_"^"_D
        . . SET ^UTILITY("DGPMVD",$J,+D)=N
        QUIT
        ;
S4      SET C=0   ;"Setup for Check-ins
        FOR I=0:0 SET I=$O(^DGPM("ATID4",DFN,I)) Q:'I  DO
        . SET N=$O(^(I,0))
        . IF $D(^DGPM(+N,0)) DO
        . . SET D=^(0),C=C+1
        . . SET ^UTILITY("DGPMVN",$J,C)=N_"^"_D
        . . SET ^UTILITY("DGPMVD",$J,+D)=N
        QUIT
        ;
S6      SET C=0   ;"Setup for T.S. Transfer
        FOR I=0:0 SET I=$O(^DGPM("ATS",DFN,DGPMCA,I)) Q:'I  DO
        . SET J=$O(^(+I,0))
        . SET N=$O(^(+J,0))
        . IF $D(^DGPM(+N,0)) DO
        . . SET C=C+1,D=^(0)
        . . SET ^UTILITY("DGPMVN",$J,C)=N_"^"_D
        . . SET ^UTILITY("DGPMVD",$J,+D)=N
        QUIT
        ;
OLD     ;"For previous entries (discharges and check-outs) skip select
        SET DGPMY=+DGPMDCD
        SET DGPMDA=+DGPMVI(17)
        SET DGPMN=0
        KILL %DT
        DO DGPMV21   ;"now not interactive
        IF $D(DGPME) DO  GOTO OLD2
        . DO ADDINFO(DGPME)
        . SET TMGOUT(0)="-1^"_DGPME
        . ;"IF DGPME'="***" WRITE !,DGPME
        IF 'DGPMY DO  GOTO Q2
        . SET TMGOUT(0)="-1^No valid date/time"  ;"//kt added
        DO ^DGPMV3  ;"FINISH!!
        IF $D(DGPME) DO  GOTO OLD2
        . SET TMGOUT(0)="-1^"_DGPME
OLD2    QUIT
        ;
        ;
Q2      KILL %,D,DEF,DGPM1X,DGPMAN,DGPMCA,DGPME,DGPML,DGPMMD,DGPMN,DGONE,DGPMSA
        KILL I,J,I1,N,PTF,X,Y,^UTILITY("DGPMVD",$J),^UTILITY("DGPMVN",$J)
        QUIT   ;"Return from call to DGPMV2
        ;===============================================================
LO      ;"From LO^DGUTL
        IF ('$D(DT))!('$D(U)) DO DT^DICRW  ;"Setup required FM vars
        IF '$D(DTIME) SET DTIME=300
        IF '$D(^DG(43,1,0)) DO  GOTO LOQ
        . SET TMGOUT(0)="-1^ADT parameters not SET up"
        NEW USER SET USER=+$GET(DUZ)
        IF 'USER!('$D(^VA(200,USER,0))) DO  GOTO LOQ
        . SET TMGOUT(0)="-1^Current user not defined.  Please contact administrator."
LOQ     IF +TMGOUT(0)=-1 KILL ^UTILITY("DG",$J)
        QUIT
        ;
        ;===============================================================
SPCLU ;"From SPLCU^DGPMV0
        ;"Special (quick) look-up for check-out lodgers
        S DGER=0
        ;"S DIC="^DPT(",DIC(0)="EQMZ"
        ;"R !,"Check-out PATIENT:  ",X:DTIME
        ;"I '$T!(X["^")!(X="") S DGER=1 Q
        ;"I X["?" D COHELP G SPCLU
        ;"D ^DIC I Y'>0 G SPCLU
        I '$D(^DGPM("APTT4",+DFN)) DO
        . SET TMGOUT(0)="-1^Patient was never a lodger ??"
        . SET DGER=1
        ;"S DFN=+Y
        Q
        ;
        ;===============================================================
NEWODS ;"From NEW^DGPMVODS
        ;"Determine IF ODS software is on and, IF so, make sure period of service is defined
        ;
        D ON^DGYZODS
        S DGODSON=DGODS
        I 'DGODS Q
        I $D(^DPT(DFN,.32)),$D(^DIC(21,+$P(^(.32),"^",3),0)) Q
        SET TMGOUT(0)="-1^Entry of Eligibility Code and Period of Service required. Use traditional console option to complete."
        ;"S DIE="^DPT(",DA=DFN
        ;"S DR=".361;.323;D ^DGYZODS;S:'DGODS Y="""";11500.02;11500.03" D ^DIE
        Q
        ;
        ;===============================================================
PCMM(DFN,ADATE)  ;"From PCMM^SCRPU4   Now Non-Interactive
        ;"Get info for primary care team, primary care provider and team phone for patient DFN on date ADATE
        ;"DFN - patient ien
        ;"ADATE - date to find primary care team, provider and team phone (default = today)
        ;
        I $GET(ADATE)="" S ADATE=DT
        I '$D(DFN) Q
        ;"NOTE: Later, I could turn function below into silent call, but for
        ;"      now, I will just disable gathering this information for feedback
        ;"      to user.   CHANGE LATER...
        ;"      To output information, use this format:
        ;"      SET TMGOUT(TMGCT)="INFO^SomeInformationHere...",TMGCT=TMGCT+1
        ;"D TDATA^SDPPTEM(DFN,"",ADATE,"P")  ;"Team information - gather, format and optionally print.
        Q
        ;
        ;===============================================================
DGPMV21 ;"From ^DGPMV21 --  'PASS/FAIL MOVEMENT DATE'
        IF $S('$D(DGPMY):1,DGPMY?7N:0,DGPMY'?7N1".".N:1,1:0) DO  GOTO Q21
        . SET DGPME="DATE EITHER NOT PASSED OR NOT IN EXPECTED VA FILEMANAGER FORMAT"
        IF $S('$D(DGPMT):1,'DGPMT:1,1:0) DO  GOTO Q21
        . SET DGPME="TRANSACTION TYPE IS NOT DEFINED"
        DO PTF^DGPMV22(DFN,DGPMDA,.DGPME,DGPMCA)  ;"FINISH!!
        IF $G(DGPME)]"" GOTO Q21
        K DGPME
        IF ("^4^5^"[("^"_DGPMT_"^"))!DGPMN GOTO CONT21
        DO PTF
        IF $D(DGPME),DGPME="***" GOTO QB21
CONT21  IF 'DGPMN GOTO QB21
        DO CHK
        IF $D(DGPME) G Q21
        IF DGPM1X GOTO QB21  ;"Don't ask to add a NEW one IF discharge or check-out
ADD     SET Y=DGPMY X ^DD("DD")
ADD1    GOTO QB21  ;"//kt added  Force YES answer to confirmation query below
        ;"W !!,"SURE YOU WANT TO ADD '",Y,"' AS A NEW ",DGPMUC
        ;"W " DATE" S:"^1^4^"'[("^"_DGPMT_"^") %=1 D YN^DICN
        ;"IF %=1 GOTO QB21
        ;"IF '%  DO  GOTO ADD1
        ;". W !?4,"Answer YES IF you wish to add this NEW entry otherwise answer NO!"
        ;"SET DGPME="NOTHING ADDED"
        ;"GOTO Q21
Q21     SET DGPMY=0
QB21    Q  ;"unified exit from DGPMV21
        ;
PTF     ;"From PTF^DGPMV21
        NEW PTF SET PTF=+$P(DGPMAN,"^",16)
        IF $S('PTF:1,'$D(^DGPT(PTF,0)):1,1:0) DO  QUIT
        . FOR I=1:1 SET J=$P($TEXT(NP+I),";;",2) QUIT:J=""  DO
        . . DO ADDINFO(J)
        . SET DGPME="***"
        . DO ADDINFO(DGPME)
        . SET DGPMY=0
        IF $D(^DGP(45.84,PTF)) DO  QUIT
        . DO ADDINFO("***")
        . SET DGPME="PTF record is closed for this admission...cannot edit"
        . SET DGPMY=0
        QUIT
NP      ;
        ;;WARNING:  This  admission has no corresponding  PTF record.
        ;;A  PTF record is  required in order to continue  processing
        ;;this movement activity.   If you have the PTF option called
        ;;"Establish PTF record from Past Admission" on your menu, it
        ;;may be used to  create the PTF  record for this  admission.
        ;;Otherwise appropriate  Medical  Information  Section  (MIS)
        ;;personnel  and/or your supervisor  will need to be notified
        ;;that the PTF record is missing as soon as possible in order
        ;;to continue processing this movement.
        ;
        ;===============================================================
        ;
ADDINFO(S,NL) ;
        ;"Purpose: Add Info to output arran
        ;"Input: S -- the string to add
        ;"       NL -- NEW Line.  Optional. Default=1
        ;"             If 1, then next information added to a NEW line.
        SET NL=$GET(NL,1)
        SET TMGCT=$GET(TMGCT,1)
        IF $GET(TMGOUT(TMGCT))="" SET TMGOUT(TMGCT)="INFO^"
        SET TMGOUT(TMGCT)=TMGOUT(TMGCT)_S
        IF NL SET TMGCT=TMGCT+1
        QUIT  
        ;
TMGCLEAN ;"Cleanup vars
      KILL %,DGER,DGPM5X,DGODS,DGODSON,DGPMUC,DGPME,DGPMN,DGPMT,DGPMPC,DIC,X,Y,^UTILITY("VAIP",$J)
      KILL DGPM2X,DGPMIFN,DGPMDCD,DGPMVI,DGPMY,DIE,DR,I,J,X,X1,Z
      KILL DGODS,DGODSON,DGPMT,DGPMSV,DGPMSVC,DGPMUC,DGPMN,^UTILITY("VAIP",$J)
      DO KVAR^VADPT
      KILL XQORQUIT
      QUIT
      ;
TMGQUIT ;"Unified Exit point
      QUIT

