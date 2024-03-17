TMGLRWU2 ;TMG/kst-Utility filing LAB DATA ; 4/15/21
              ;;1.0;TMG-LIB;**1**;1/4/17
 ;
 ;"TMG LAB ENTRY UTILITIES
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 1/4/17  Kevin S. Toppenberg MD
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
 ;"OR(LRTYPE,LRDFN,LRSS,LRIDT,LRUID,LRXQA,LRTST)  --  Send OR (CPRS) notification of lab reports being available.  
 ;"ALERT(RECIP,TMGDFN,FMDT,LEVEL,NODE)  -- Send Alert.  Wrapper for OR() above
 ;"RPCALERT(OUT,RECIP,TMGDFN,FMDT,LEVEL,NODE)  -- RPC for sending Alert.  Wrapper for ALERT() above
 ;"
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;"TEST1        
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;
OR(LRTYPE,LRDFN,LRSS,LRIDT,LRUID,LRXQA,LRTST,SUPPRESS)    ;" Send OR (CPRS) notification of lab reports being available.  
  ;"NOTE: Copied from OR^LR7ORB3 for mod purposes
  ;" Call with LRTYPE = type OERR notification, IEN 100.9 (currently supports 3, 14, 57)
  ;"               3 = NF_LAB_RESULTS 
  ;"               14 = NF_ABNORMAL_LAB_RESULTS
  ;"               57 = NF_CRITICAL_LAB_RESULTS
  ;"              999 = Will alter to NF_LAB_RESULTS for CPRS   ;"4/24/18
  ;"           LRDFN  = file #63 IEN
  ;"           LRSS   = file #63 subscript -- "CH" OR "MI"
  ;"                    Only supports CH and MI. AP subscript handled by separate API.
  ;"           LRIDT  = inverse d/t of entry in file #63
  ;"           LRUID  = accession's UID
  ;"           LRXQA  = recipient array, e.g. LRXQA(168)=""
  ;"           LRTST  = test IEN60 ^ Name of test being alerted ^ parent test ien60
  ;"           SUPPRESS = Optional. Default is 0. If 1, will suppress if duplicate alert
  ;"Result: 1^Alert Sent, or 0^OK, or -1^Error
  NEW TMGDFN,LRMSG,LRPREFIX,LRX,LRY         
  SET SUPPRESS=+$GET(SUPPRESS)
  NEW LRIENS   ;"a string to pass to EN^ORB3
  NEW LROIFN   ;"OK to be null "". OERR INTERNAL FILE #, an IEN100  
  NEW LROE     ;"OK to be null "". ORDER # (field 9.5) in 69.01
  NEW LRODT    ;"DATE ORDERED field (#3) in 68.02/ACCESSION NUMBER in 68.01/DATE in 68/ACCESSION
  NEW LRSN     ;"SPECIMEN NUMBER (#4) in 68.02/ACCESSION NUMBER in 68.01/DATE in 68/ACCESSION
  SET (LROE,LROIFN)=""
  SET LROIFN=0  ;"//kt -- this affects processing in CPRS. 1 means use IFN order, 0 is a default generic 
  NEW TMGRESULT SET TMGRESULT="1^Alert Sent"
  IF LRSS'?1(1"CH",1"MI") DO  GOTO ORDN
  . SET TMGRESULT="-1^Lab Subscript not supported"
  SET TMGDFN=$PIECE(^LR(LRDFN,0),"^",3)
  SET LRPREFIX=$SELECT(LRTYPE=3:"",LRTYPE=14:"Abnormal ",LRTYPE=57:"Critical ",LRTYPE=999:"Manually entered ",1:"")
  IF LRTYPE=999 SET LRTYPE=3
  ;
  SET LRX=$$CHECKUID^LRWU4(LRUID,LRSS)  ;"RETURNS: 1(accession exists)^area^date^number, i.e. 1^IEN68^IEN68.01^IEN68.02       
  ;"IF LRX<1 QUIT "0^Accession's UID not valid"
  IF LRX<1 SET LRX=""  ;"//kt
  NEW IEN68 SET IEN68=+$PIECE(LRX,"^",2)
  NEW IEN68D01 SET IEN68D01=+$PIECE(LRX,"^",3)  ;"DATE field (a subfile -- 68.01)
  NEW IEN68D02 SET IEN68D02=+$PIECE(LRX,"^",4)  ;"ACCESSION NUMBER subfield (a subfile -- 68.02)  
  SET LRY=$GET(^LRO(68,IEN68,1,IEN68D01,1,IEN68D02,0))
  SET LRODT=+$PIECE(LRY,"^",4)   ;"DATE ORDERED field (#3) in 68.02/ACCESSION NUMBER in 68.01/DATE in 68/ACCESSION
  SET LRSN=+$PIECE(LRY,"^",5)    ;"SPECIMEN NUMBER (#4) in 68.02/ACCESSION NUMBER in 68.01/DATE in 68/ACCESSION
  IF LRODT,LRSN DO
  . ;"Get data from LAB ORDER ENTRY (#69)->SPECIMEN#(69.01)->TEST(69.03)
  . ;"                                 |       |              | 
  . ;"                                 LRDT    LRSN           LRTST (<-this is an IEN60)
  . NEW LR6903 SET LR6903=$ORDER(^LRO(69,LRODT,1,LRSN,2,"B",+LRTST,0))
  . IF 'LR6903,$PIECE(LRTST,"^",3) DO
  . . SET LR6903=$ORDER(^LRO(69,LRODT,1,LRSN,2,"B",+$PIECE(LRTST,"^",3),0))
  . IF LR6903 SET LROIFN=$PIECE($GET(^LRO(69,LRODT,1,LRSN,2,LR6903,0)),"^",7)
  . IF 'LROIFN SET LROIFN=$PIECE($GET(^LRO(69,LRODT,1,LRSN,0)),"^",11)
  . SET LROE=$PIECE($GET(^LRO(69,LRODT,1,LRSN,.1)),"^")
  IF LRODT'>0 SET LRODT=$$RDT2FMDT^TMGLRWU1(LRIDT)  ;"//kt
  ;
  ;"          OK to be null "". OERR INTERNAL FILE #, an IEN100
  ;"          |             OK to be null "". ORDER # (field 9.5) in 69.01
  ;"          |             |        DATE ORDERED field (#3) in 68.02/ACCESSION NUMBER in 68.01/DATE in 68/ACCESSION 
  ;"          |             |        |        SPECIMEN NUMBER (#4) in 68.02/ACCESSION NUMBER in 68.01/DATE in 68/ACCESSION
  ;"          |             |        |        |         File #63 subscript -- "CH" OR "MI"         
  ;"          |             |        |        |         |       Inverse d/t of entry in file #63         
  ;"          |             |        |        |         |       |         
  SET LRIENS=LROIFN_"@OR|"_LROE_";"_LRODT_";"_LRSN_";"_LRSS_";"_LRIDT_"@LRCH"
  ;
  IF LRSS="CH" DO
  . ;"4/24/18 IF LRTYPE=14!(LRTYPE=57) SET LRMSG=LRPREFIX_"lab results:"
  . IF LRPREFIX'="" SET LRMSG=LRPREFIX_"lab results:"
  . ELSE   SET LRMSG="Lab results:"
  IF LRSS="MI" DO
  . IF LRTYPE=14!(LRTYPE=57) SET LRMSG=LRPREFIX_"microbiology results:"
  . ELSE   SET LRMSG="Microbiology results:"
  ;
  SET LRMSG=LRMSG_" - ["_$PIECE(LRTST,"^",2)_"]"
  IF $D(LRXQA(83)) SET SUPPRESS=0
  IF SUPPRESS=1 DO CHKALERT(.LRXQA,TMGDFN,LRMSG) ;"Removes entries from LRXQA if duplicates or unwanted
  DO SPECALRT^TMGHL7U5(TMGDFN)  ;"CHECK TO SEE IF RESULTS NEED TO BE SENT TO SPECIALIST. IF SO SEND ALERT TO EDDIE
  ;
  ;"OERR parameters:
  ;"            ORN: notification id (#100.9 ien). e.g. 3 <-- NF_LAB_RESULTS 
  ;"            |     ORBDFN: patient id (#2 ien)
  ;"            |     |    ORNUM: order number (#100 ien)
  ;"            |     |    |       ORBADUZ: recipient array
  ;"            |     |    |       |     ORBPMSG: message text
  ;"            |     |    |       |     |     ORBPDATA lab result reference
  ;"            |     |    |       |     |     |
  ;"//kt mod 4/15/21
  IF $ORDER(LRXQA(""))>0 DO
  . DO EN^ORB3(LRTYPE,TMGDFN,LROIFN,.LRXQA,LRMSG,LRIENS)
  ELSE  DO
  . SET TMGRESULT="0^OK"
  ;"IF SUPPRESS=0 DO
  ;". DO EN^ORB3(LRTYPE,TMGDFN,LROIFN,.LRXQA,LRMSG,LRIENS)
  ;"ELSE  IF (SUPPRESS=1)&($D(LRXQA)) DO
  ;". DO EN^ORB3(LRTYPE,TMGDFN,LROIFN,.LRXQA,LRMSG,LRIENS)
  ;"ELSE  DO
  ;". SET TMGRESULT="0^ALERTS ALREADY EXISTS"
  ;
  ;"FYI: In comparison, this is what is sent from RAUTL00, a radiology alert
  ;"------------------------------------------------
  ;"                E.g. 22^Imaging Results,Non Critical: BONE DENSITOMETRY <-- TEXT IS STRIPPED OFF
  ;"                |      patient id (#2 ien), e.g. 69928
  ;"                |      |     OK to be null
  ;"                |      |     |        e.g. LRXQA(168)=""
  ;"                |      |     |       |        e.g. Imaging Results,Non Critical: BONE DENSITOMETRY
  ;"                |      |     |       |        |     e.g. 6838787.8955~1
  ;"                |      |     |       |        |     |
  ;"D EN^ORB3(+$G(RANOTE),RADFN,RAOIFN,.RAREQPHY,RAMSG,RAIENS)         
ORDN  ;
  QUIT TMGRESULT
  ;
CHKALERT(RECIPARR,TMGDFN,ALERTMSG)   
  ;"Purpose: Checks existing alerts to see if current one is duplicate.
  ;"         When one is found, the user's entry will be removed from RECIPARR
  ;"Input: RECIPARR - BY REF - array of user IENs
  ;"       TMGDFN - Patient IEN
  ;"       ALERTMSG - Message text of current alert
  SET ALERTMSG=$$TRIM^XLFSTR(ALERTMSG)
  NEW DTTIME SET DTTIME=0
  NEW RECIP SET RECIP=0
  FOR  SET RECIP=$ORDER(RECIPARR(RECIP)) QUIT:RECIP'>0  DO
  . FOR  SET DTTIME=$ORDER(^XTV(8992,RECIP,"XQA",DTTIME)) QUIT:(DTTIME'>0)  DO
  . . NEW ZN SET ZN=$GET(^XTV(8992,RECIP,"XQA",DTTIME,0))
  . . NEW ITEMDFN,ITEMMSG
  . . SET ITEMDFN=$P($P(ZN,"^",2),",",2)
  . . SET ITEMMSG=$$TRIM^XLFSTR($P($P(ZN,"^",3),": ",2,999))
  . . IF (ITEMDFN=TMGDFN)&(ITEMMSG=ALERTMSG) DO
  . . . KILL RECIPARR(RECIP)
  . . ;"ELH -- 11/16/17
  . . ;"  Try to determine if:
  . . ;"    current is abnormal, previous was normal... add (maybe remove normal later)
  . . ;"    current is normal, previous was abnormal... don't add
  . . IF ALERTMSG["Lab" DO
  . . . ;"BELOW WILL JUST GET "results: - [DATE@TIME]" from each and test
  . . . ;"   THIS WILL NOT ADD IF ABNORMAL ALREADY EXISTS FOR THE SAME DATETIME LABS
  . . . NEW CURENDPART SET CURENDPART=$P(ITEMMSG,"ab ",2)
  . . . NEW MSGENDPART SET MSGENDPART=$P(ALERTMSG,"ab ",2)
  . . . IF (ITEMDFN=TMGDFN)&(CURENDPART=MSGENDPART) DO
  . . . . KILL RECIPARR(RECIP)
  . . IF ALERTMSG["Abnormal" DO
  . . . ;"BELOW WILL DETERMINE IF NORMAL ALREADY EXISTS FOR THE SAME DATETIME LABS
  . . . ;"   THIS WILL DELETE THE EXISTING NORMAL ALERT
  . . . ;"note: don't enact this portion yet until top has been tested
  . . . NEW CURENDPART SET CURENDPART=$P(ITEMMSG,"ab ",2)
  . . . NEW MSGENDPART SET MSGENDPART=$P(ALERTMSG,"ab ",2)
  . . . IF (ITEMDFN=TMGDFN)&(CURENDPART=MSGENDPART)&(ITEMMSG'["Abnormal") DO
  . . . . KILL ^XTV(8992,RECIP,"XQA",DTTIME)
  . ;"Just in case an alert was previously created in the same job, but 
  . ;"was tasked off and hasn't been fully completed yet... we will test
  . ;"a temp global and also store in the global
  . IF $D(^TMP("TMG HL7 ALERT",$J,RECIP,TMGDFN,ALERTMSG)) DO
  . . KILL RECIPARR(RECIP)
  . SET ^TMP("TMG HL7 ALERT",$J,RECIP,TMGDFN,ALERTMSG)=""
  QUIT
  ;  
ALERT(RECIP,TMGDFN,FMDT,LEVEL,NODE,SUPPRESS)  ;"Send Alert.  Wrapper for OR() above
  ;"Input: RECIP -- IEN200, or for multiple recipients, use RECIP(IEN200)=""
  ;"       TMGDFN -- IEN in 2
  ;"       FMDT -- The date of the labs, in Fileman format (not IDT or RDT)
  ;"       LEVEL -- Optional.  Default=1.  1->normal 2->abnormal,3->CriticalM 4->Manually Entered
  ;"       NODE -- Optional.  Default is "CH".  Should be "CH" or "MI" only. 
  ;"       SUPPRESS -- Optional. Default is 0. If value is 1, alert will be
  ;"                 checked against existing alerts for user and not sent
  ;"                 if duplicate
  ;"Result:  1^OK, or 0^OK, or -1^Error message
  NEW TMGRESULT SET TMGRESULT="1^OK"
  SET SUPPRESS=+$GET(SUPPRESS)
  SET TMGDFN=$GET(TMGDFN) IF TMGDFN'>0 DO  GOTO ALRTDN
  . SET TMGRESULT="-1^Valid DFN not provided.  Got ["_TMGDFN_"]"
  NEW LRDFN SET LRDFN=+$GET(^DPT(TMGDFN,"LR"))
  IF LRDFN'>0 DO  GOTO ALRTDN
  . SET TMGRESULT="-1^Field# 63 not defined in file #2 (PATIENT) for DFN="_TMGDFN 
  SET FMDT=$GET(FMDT) IF FMDT'>0 DO  GOTO ALRTDN
  . SET TMGRESULT="-1^Valid Lab date-time not provided.  Got ["_FMDT_"]"
  ;"NEW NODE SET NODE=$GET(NODE,"CH")
  SET NODE=$GET(NODE,"CH")
  IF "CH,MI,"'[NODE_"," DO  GOTO ALRTDN
  . SET TMGRESULT="-1^Invalid NODE provided.  Got ["_NODE_"]"
  NEW RDT SET RDT=$$FMDT2RDT^TMGLRWU1(FMDT)
  IF $DATA(^LR(LRDFN,NODE,RDT))=0 DO  GOTO ALRTDN
  . SET TMGRESULT="-1^No lab data to send alert for.  DFN=["_TMGDFN_"], NODE=["_NODE_"], RDT=["_RDT_"]"
  SET LEVEL=$GET(LEVEL,1)
  IF "1,2,3,4,"'[LEVEL_"," DO  GOTO ALRTDN  ;"ADDED 999      4/24/18
  . SET TMGRESULT="-1^Invalid LEVEL.  Expected 1, 2, 3 or 4.  Got ["_LEVEL_"]"
  NEW LRTYPE SET LRTYPE=$SELECT(LEVEL=4:999,LEVEL=3:57,LEVEL=2:14,1:3)  ;"//kt changed level 3 LRTYPE from 57 --> 24 (then changed it back)  
  NEW TEMP IF $GET(RECIP)>0 SET TEMP(RECIP)=""
  NEW ADUZ SET ADUZ="" FOR  SET ADUZ=$ORDER(RECIP(ADUZ)) QUIT:ADUZ'>0  SET TEMP(ADUZ)=""
  KILL RECIP MERGE RECIP=TEMP KILL TEMP   ;"The purpose of this is to disallow RECIP="" //kt 4/15/21
  ;"//kt 4/15/21 -- NEW TEMP SET TEMP=$GET(RECIP) IF TEMP>0 SET RECIP(TEMP)="",RECIP=""
  ;
  NEW FLD SET FLD=1
  NEW IEN60 SET IEN60=0
  ;"Find the first enountered IEN60 for grouping of tests at given DT
  FOR  SET FLD=$ORDER(^LR(LRDFN,NODE,RDT,1)) QUIT:(FLD'>0)!(IEN60>0)  DO
  . NEW N1 SET N1=$GET(^LR(LRDFN,NODE,RDT,FLD))
  . NEW P3 SET P3=$PIECE(N1,"^",3)
  . SET IEN60=$PIECE(P3,"!",7)
  ;"//kt note:  I encountered situation where alert was being called and there was just a 
  ;"   comment, but no actual lab result.  And in that case, an alert had already
  ;"   been sent for the other part of the message (that wasn't just comment).
  ;"   So I am going to remove IEN60=0 as an error state.  Will see if this causes
  ;"   other problems.  
  ;"IF IEN60'>0 DO  GOTO ALRTDN
  ;". SET TMGRESULT="-1^Unable to find lab test (IEN 60).  DFN=["_TMGDFN_"], NODE=["_NODE_"], RDT=["_RDT_"]"
  ;"NOTE: I will be sending an alert for just 1 single test.  This will be used
  ;"      to notify the provider of the availablity for review of the entire panel.
  NEW TESTNAME SET TESTNAME=$PIECE($GET(^LAB(60,IEN60,0)),"^",1)
  IF TESTNAME="" SET TESTNAME="LAB TEST"   ;"//kt 7/5/17, HOWEVER, not even used (see below)
  ;"NOTE: could have more than one parent.  I am just picking the first encountered. 
  NEW PARENTIEN60 SET PARENTIEN60=$ORDER(^LAB(60,"AB",IEN60,0))   
  ;"NEW LRTST SET LRTST=IEN60_"^"_TESTNAME_"^"_PARENTIEN60
  NEW LRTST SET LRTST=IEN60_"^"_$$FMTE^XLFDT(FMDT,"1M")_"^"_PARENTIEN60  ;"USE DATE INSTEAD OF TEST NAME 
  SET TMGRESULT=$$OR(LRTYPE,LRDFN,NODE,RDT,0,.RECIP,LRTST,SUPPRESS)
ALRTDN ;
  QUIT TMGRESULT
  ;
RPCALERT(OUT,RECIP,TMGDFN,FMDT,LEVEL,NODE)  ;"RPC for sending Alert.  Wrapper for ALERT() above
  ;"RPC NAME -- 'TMG CPRS LAB ALERT'
  ;"Input: OUT -- PASSED BY REFERENCE.  AN OUT PARAMETER.  Format: OUT(0)=1^OK, or 0^OK, or -1^Error message
  ;"       RECIP -- IEN200, or for multiple recipients, use RECIP(IEN200)=""
  ;"       TMGDFN -- IEN in 2
  ;"       FMDT -- The date of the labs, in Fileman format (not IDT or RDT)
  ;"       LEVEL -- Optional.  Default=1.  1->normal 2->abnormal, 3->Critical 
  ;"                ;"4/24/18. Added a LEVEL 4 for manually entered values.
  ;"                ;"         This translates to 999 for the sake of a
  ;"                ;"         alert message prefix. It is a valid server
  ;"                ;"         value, but doesn't have a corresponding CPRS value
  ;"                ;"         yet but one could be created. As of now it
  ;"                ;"         just gets translated into a "3"
  ;"       NODE -- Optional.  Default is "CH".  Should be "CH" or "MI" only. 
  ;"Result:  None
  NEW TMGZZDEBUG SET TMGZZDEBUG=0
  IF TMGZZDEBUG=1 DO
  . KILL RECIP MERGE RECIP=^TMP("RPCALERT","RECIP")
  . SET TMGDFN=$GET(^TMP("RPCALERT","DFN"))
  . SET FMDT=$GET(^TMP("RPCALERT","FMDT"))
  . SET LEVEL=$GET(^TMP("RPCALERT","LEVEL"))
  . SET NODE=$GET(^TMP("RPCALERT","NODE"))
  ELSE  DO
  . MERGE ^TMP("RPCALERT","RECIP")=RECIP
  . SET ^TMP("RPCALERT","DFN")=TMGDFN
  . SET ^TMP("RPCALERT","FMDT")=FMDT
  . SET ^TMP("RPCALERT","LE1VEL")=LEVEL
  . SET ^TMP("RPCALERT","NODE")=NODE
  ;"
  NEW TMGRESULT 
  SET TMGRESULT=$$ALERT(.RECIP,.TMGDFN,.FMDT,.LEVEL,.NODE)  ;"Send Alert.  Wrapper for ALERT() above
  SET OUT(0)=TMGRESULT
  QUIT
  ;  
TEST1  ;       
  ;" TMGDFN=9182       
  ;" LRDFN=128               
  ;" 
  ;" 22) ^LR(128,"CH",6839492.899498,0) = 3160506.100502^0^3160507.0051^168^72^^^^^
  ;"                                    = ^^^^69
  ;" 23) ^LR(128,"CH",6839492.899498,1,0) = ^63.041A^6^6
  ;" 24) ^LR(128,"CH",6839492.899498,1,1,0) =
  ;" 25) ^LR(128,"CH",6839492.899498,1,2,0) = Test performed by PathGroup Labs
  ;" 26) ^LR(128,"CH",6839492.899498,1,3,0) = 658 Grassmere Park Suite 101, Nashvil
  ;"                                        = le, TN 37211
  ;" 27) ^LR(128,"CH",6839492.899498,1,4,0) = Mary Mayo, Ph.D., Medical Director
  ;" 28) ^LR(128,"CH",6839492.899498,1,5,0) = Chronic Kidney Disease:  Less than 60
  ;"                                        =  ml/min/1.73 square meters
  ;" 29) ^LR(128,"CH",6839492.899498,1,6,0) = End Stage Renal Disease: Less than 15
  ;"                                        =  ml/min/1.73 square meters
  ;" 30) ^LR(128,"CH",6839492.899498,69024) = >60^^00000.2270!00000.2272!!0000!!!51
  ;"                                        = 10^168^!>60!!!!!mL/min/1.73m2!!!!^316
  ;"                                        = 0507.0201^^^73^^PATHGROUP
  ;" 31) ^LR(128,"CH",6839492.899498,69025) = >60^^00000.2270!00000.2270!!0000!!!51
  ;"                                        = 11^168^!>60!!!!!mL/min/1.73m2!!!!^316
  ;"                                        = 0507.0201^^^73^^PATHGROUP
  ;" 32) ^LR(128,"CH",6839492.899498,"NPC") = 2
  NEW RDT SET RDT="6839492.899498"
  NEW FMDT SET FMDT=$$FMDT2RDT^TMGLRWU1(RDT)
  NEW TEMP SET TEMP=$$ALERT(168,9182,FMDT)
  WRITE TEMP,!
  QUIT
  ;"
CLEANALR()  ;"Remove duplicate lab alerts
  ;"First: grab all alerts and stuff into an array
  NEW ALERTARR  ;"DATA WILL BE ALERTARR(USER,PAT,MESSAGE)=DATETIME
  NEW DTTIME
  NEW RECIP SET RECIP=0
  FOR  SET RECIP=$ORDER(^XTV(8992,RECIP)) QUIT:RECIP'>0  DO 
  . SET DTTIME=0
  . FOR  SET DTTIME=$ORDER(^XTV(8992,RECIP,"XQA",DTTIME)) QUIT:(DTTIME'>0)  DO
  . . NEW ZN SET ZN=$GET(^XTV(8992,RECIP,"XQA",DTTIME,0))
  . . NEW THISINFO,THISMSG
  . . SET THISINFO=$P(ZN,"^",2)
  . . SET THISMSG=$$TRIM^XLFSTR($P($P(ZN,"^",3),": ",2,999))
  . . IF THISMSG["ab result" SET ALERTARR(RECIP,THISINFO,THISMSG)=DTTIME
  ;"
  NEW LASTMSG
  SET RECIP=0
  FOR  SET RECIP=$ORDER(ALERTARR(RECIP)) QUIT:RECIP'>0  DO
  . SET THISINFO=0
  . FOR  SET THISINFO=$ORDER(ALERTARR(RECIP,THISINFO)) QUIT:THISINFO=""  DO
  . . SET THISMSG="",LASTMSG=""
  . . WRITE THISINFO,!
  . . FOR  SET THISMSG=$ORDER(ALERTARR(RECIP,THISINFO,THISMSG)) QUIT:THISMSG=""  DO
  . . . WRITE "-->",THISMSG,!
  . . . SET LASTMSG=THISMSG
  . . . ;SET DTTIME=$G(ALERTARR(RECIP,THISINFO,THISMSG))
  . . . ;KILL ^XTV(8992,RECIP,"XQA",DTTIME)


  QUIT
;  . . IF (ITEMDFN=TMGDFN)&(ITEMMSG=ALERTMSG) KILL RECIPARR(RECIP)
;  . . ;"ELH -- 11/16/17
;  . . ;"  Try to determine if:
;  . . ;"    current is abnormal, previous was normal... add (maybe remove normal later)
;  . . ;"    current is normal, previous was abnormal... don't add
;  . . IF ALERTMSG["Normal" DO
;  . . . ;"BELOW WILL JUST GET "results: - [DATE@TIME]" from each and test
;  . . . ;"   THIS WILL NOT ADD IF ABNORMAL ALREADY EXISTS FOR THE SAME DATETIME LABS
;  . . . NEW CURENDPART SET CURENDPART=$P(ITEMMSG,"ab ",2)
;  . . . NEW MSGENDPART SET MSGENDPART=$P(ALERTMSG,"ab ",2)
;  . . . IF (ITEMDFN=TMGDFN)&(CURENDPART=MSGENDPART) KILL RECIPARR(RECIP)
;  . . IF ALERTMSG["Abnormal" DO
;  . . . ;"BELOW WILL DETERMINE IF NORMAL ALREADY EXISTS FOR THE SAME DATETIME LABS
;  . . . ;"   THIS WILL DELETE THE EXISTING NORMAL ALERT
;  . . . ;"note: don't enact this portion yet until top has been tested
;  . . . NEW CURENDPART SET CURENDPART=$P(ITEMMSG,"ab ",2)
 ;; . . . NEW MSGENDPART SET MSGENDPART=$P(ALERTMSG,"ab ",2)
 ; . . . IF (ITEMDFN=TMGDFN)&(CURENDPART=MSGENDPART) KILL ^XTV(8992,RECIP,"XQA",DTTIME)
 ; . ;"Just in case an alert was previously created in the same job, but
 ; . ;"was tasked off and hasn't been fully completed yet... we will test
 ; . ;"a temp global and also store in the global
 ; . IF $D(^TMP("TMG HL7 ALERT",$J,RECIP,TMGDFN,ALERTMSG)) KILL 