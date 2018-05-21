TMGLRWU1 ;TMG/kst-Utility for entering data to LAB DATA file ;2/2/14, 4/1/18
              ;;1.0;TMG-LIB;**1**;9/13/13
 ;
 ;"TMG LAB ENTRY UTILITY
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
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"CONSOLE ;
 ;"FMDT2RDT(FMDT)  --CONVERT FMDT --> LAB RDT
 ;"RDT2FMDT(RDT)   --CONVERT LAB RDT --> FMDT
 ;"
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;"ASSIGN(STR,LABARR) --Handle assignment inputs
 ;"CLEARARR(LABARR)
 ;"VIEWARR(LABARR)
 ;"STORE(LABARR) --DO ACTUAL LAB STORAGE
 ;"SETLAB(PARTA,VALUE,LABARR) ;
 ;"SETPARTS(IEN60,LABARR)
 ;"GETSTORE(IEN60) ;
 ;"SETPROV(VALUE,LABARR) ;
 ;"SETLOC(VALUE,LABARR) ;
 ;"SETPAT(VALUE,LABARR) ;
 ;"SETDATE(VALUE,LABARR) ;
 ;"GETSPEC(IEN60) ;
 ;"HELP ;
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;
CONSOLE ;
        WRITE "LAB ENTRY CONSOLE",!
        WRITE "------------------",!
        WRITE !
        NEW IN,UPIN,LABARR
L1      READ ">",IN:$GET(DTIME,3600),!
        IF IN="" SET IN="?"
        SET UPIN=$$UP^XLFSTR(IN)
        IF IN="^" GOTO CONDN
        IF IN["=" DO ASSIGN(IN,.LABARR) GOTO L1
        IF IN["?" DO HELP GOTO L1
        IF UPIN="CLEAR" DO CLEARARR(.LABARR) GOTO L1
        IF UPIN="VIEW" DO VIEWARR(.LABARR) GOTO L1
        IF UPIN="STORE" DO STORE(.LABARR) GOTO L1
        WRITE "??",! 
        GOTO L1
CONDN   ;"SEE IF ANYTHING NEEDS TO BE FILED FIRST...        
        QUIT
        ;
ASSIGN(STR,LABARR) ;"Handle assignment inputs
        ;"Input: STR- the user input string.  Should be 'aaa = bbb' format
        ;"       LABARR -- PASS BY REFERENCE.  The composite array to be posted
        NEW PARTA,VALUE
        SET PARTA=$$UP^XLFSTR($$TRIM^XLFSTR($PIECE(STR,"=",1)))
        IF $LENGTH(PARTA," ")>1,$PIECE(PARTA," ")="SET" SET PARTA=$PIECE(PARTA," ",2,99)
        SET VALUE=$$TRIM^XLFSTR($PIECE(STR,"=",2))
        IF PARTA="PROV" DO SETPROV(VALUE,.LABARR) GOTO ASGNDN
        IF PARTA="PAT" DO SETPAT(VALUE,.LABARR) GOTO ASGNDN
        IF PARTA="LOC" DO SETLOC(VALUE,.LABARR) GOTO ASGNDN
        IF PARTA="DATE" DO SETDATE(VALUE,.LABARR) GOTO ASGNDN
        IF PARTA="PATIENT" DO SETPAT(VALUE,.LABARR) GOTO ASGNDN
        ELSE  DO SETLAB(PARTA,VALUE,.LABARR) GOTO ASGNDN        
ASGNDN  QUIT        
        ;
CLEARARR(LABARR) ;
        KILL LABARR
        QUIT
        ;
VIEWARR(LABARR) ;
        IF $DATA(LABARR) DO
        . WRITE "**HERE IS THE CURRENT DATA ENTERED**",!
        . DO ZWRITE^TMGZWR("LABARR")
        . WRITE !
        ELSE  DO
        . WRITE "**NO DATA ENTERED**",!,!
        QUIT
        ;
STORE(LABARR) ;"DO ACTUAL LAB STORAGE
        ;"//FORMAT FOR POSTLAB^TMGRPCL0
        NEW RPCARR
        NEW IDX SET IDX=0
        SET IDX=IDX+1,RPCARR(IDX)="<METADATA>"
        SET IDX=IDX+1,RPCARR(IDX)="DT_TAKEN ="_$GET(LABARR("DATE"))
        ;"SET IDX=IDX+1,RPCARR(IDX)="DT_COMPLETED ="_$GET(LABARR("DATE"))
        SET IDX=IDX+1,RPCARR(IDX)="PROVIDER ="_$GET(LABARR("PROVIDER"))
        SET IDX=IDX+1,RPCARR(IDX)="LOCATION ="_$GET(LABARR("LOCATION"))
        SET IDX=IDX+1,RPCARR(IDX)="PATIENT ="_$GET(LABARR("PATIENT"))
        SET IDX=IDX+1,RPCARR(IDX)="SPECIMEN =72"  ;"FORCE TO BE SERUM
        SET IDX=IDX+1,RPCARR(IDX)="<VALUES>"
        NEW IEN60 SET IEN60=0
        FOR  SET IEN60=$ORDER(LABARR("LABS",IEN60)) QUIT:IEN60'>0  DO
        . SET IDX=IDX+1,RPCARR(IDX)=IEN60_"^"_$GET(LABARR("LABS",IEN60))_"^^0^0"
        SET IDX=IDX+1,RPCARR(IDX)="<COMMENTS>"
        SET IDX=IDX+1,RPCARR(IDX)="Entered manually, typographical/human errors possible"
        NEW RPCRESULT SET RPCRESULT="1^OK"
        ;"DO ZWRITE^TMGZWR("RPCARR")
        ;"DO PRESS2GO^TMGUSRI2
        DO POSTLABS^TMGRPCL0(.RPCRESULT,.RPCARR)
        IF $PIECE(RPCRESULT,"^",1)="-1" DO
        . WRITE "ERROR FILING RESULTS! ",$PIECE(RPCRESULT,"^",2,99),!
        . WRITE "PLEASE CORRECT ANY PROBLEMS AND RUN STORE AGAIN",!
        ELSE  DO
        . WRITE "VALUES SUCCESSFULLY STORED.",!
        . NEW % SET %=1 
        . WRITE "WOULD YOU LIKE TO KEEP THE PATIENT DATA FOR MORE LABS" DO YN^DICN WRITE !
        . IF %=1 DO
        . . KILL LABARR("LABS")
        . . WRITE "HERE IS THE CURRENT INFORMATION.",! 
        . . DO ZWRITE^TMGZWR("LABARR")
        . ELSE  DO
        . . KILL LABARR
        QUIT
        ;
SETLAB(PARTA,VALUE,LABARR) ;
        NEW DIC,X,Y SET DIC=60,DIC(0)="EMQ",X=PARTA
        ;"SET DIC("S")="IF +$$GETSTORE^TMGLRWU1(Y)>0"
        DO ^DIC WRITE !
        IF +Y'>0 DO  GOTO SLBDN
        . WRITE "Unable to find lab '",PARTA,"' in file 60, or this ",!
        . WRITE "lab doesn't have defined storage location. Try again.",!
        ;"NEW STORE SET STORE=$$GETSTORE(+Y)
        IF $DATA(^LAB(60,+Y,2)) DO SETPARTS(+Y,.LABARR) GOTO SLBDN
        SET LABARR("LABS",+Y)=VALUE
SLBDN    QUIT        
        ;
SETPARTS(IEN60,LABARR) ;
        NEW ACTIVE SET ACTIVE=0
        NEW SUBIEN SET SUBIEN=0
        WRITE "** PANEL ENTRY **",!
        NEW IN
        FOR  SET SUBIEN=$ORDER(^LAB(60,IEN60,2,SUBIEN)) QUIT:(SUBIEN'>0)  DO
        . NEW LABIEN SET LABIEN=+$PIECE($GET(^LAB(60,IEN60,2,SUBIEN,0)),"^",1)
        . IF LABIEN'>0 QUIT
        . NEW LABNAME SET LABNAME=$PIECE($GET(^LAB(60,LABIEN,0)),"^",1)
        . NEW STORE SET STORE=$PIECE($GET(^LAB(60,LABIEN,0)),"^",5)
        . IF $PIECE(STORE,";",1)'="CH" SET STORE=""  ;"I only want labs of type CHEM... (i.e. not micro or pathology etc)
        . SET ACTIVE=(STORE'="")
        . IF ACTIVE=0 QUIT
        . WRITE " - VALUE FOR "_LABNAME
        . READ ": ",IN:$GET(DTIME,3600),!
        . IF IN="" QUIT
        . SET LABARR("LABS",+LABIEN)=IN
        QUIT 
        ;
GETSTORE(IEN60) ;
        QUIT $PIECE($GET(^LAB(60,IEN60,0)),"^",5)
        ;
SETPROV(VALUE,LABARR) ;
        NEW DIC,X,Y
        SET DIC=200,DIC(0)="MEQ",X=VALUE
        DO ^DIC WRITE !
        IF +Y'>0 DO  GOTO SPDN
        . WRITE "?? Unable to find '",VALUE,"' in NEW PERSON file.  Try again",!
        SET LABARR("PROVIDER")=+Y
SPDN    QUIT        
        ;
SETLOC(VALUE,LABARR) ;
        NEW DIC,X,Y
        SET DIC=4,DIC(0)="MEQ",X=VALUE
        DO ^DIC WRITE !
        IF +Y'>0 DO  GOTO SPDN
        . WRITE "?? Unable to find '",VALUE,"' in LOCATION file.  Try again",!
        SET LABARR("LOCATION")=+Y
SLDN    QUIT
        ;
SETPAT(VALUE,LABARR) ;
        NEW DIC,X,Y
        SET DIC=2,DIC(0)="MEQ",X=VALUE
        DO ^DIC WRITE !
        IF +Y'>0 DO  GOTO SPTDN
        . WRITE "?? Unable to find '",VALUE,"' in PATIENT file.  Try again",!
        SET LABARR("PATIENT")=+Y
SPTDN    QUIT
        ;
SETDATE(VALUE,LABARR) ;
        IF $DATA(LABARR("LABS"))=0 GOTO SD2
        NEW % SET %=1
        WRITE "File labs entered so far" DO YN^DICN WRITE !
        IF %=1 GOTO SDDN
        DO STORE(.LABARR)
SD2     NEW X,Y,%DT SET %DT="PTE",X=VALUE
        DO ^%DT WRITE !
        IF Y=-1 DO  GOTO SDDN
        . WRITE "Invalid date entry: '",VALUE,"'.  Try again.",!
        SET LABARR("DATE")=Y
SDDN    QUIT
        ;
GETSPEC(IEN60) ;
        NEW IEN61 SET IEN61=0 
        ;"SET IEN61=$$GET1^DIQ(60,IEN60_",",22701,"I")
        IF IEN61'>0 SET IEN61=72  ;"SERUM
        QUIT IEN61
        ;
HELP    ;
        WRITE "HELP.  Below are supported commands",!
        WRITE "------------------------------------",!
        WRITE "PROV = <NAME>   to SET provider",!
        WRITE "LOC = <NAME>  to SET location",!
        WRITE "DATE = <DATE>  to SET date of labs",!
        WRITE "PATIENT = <NAME> to SET patient name.",!
        WRITE "LAB = <VALUE>  to SET a lab value",!
        WRITE "CLEAR to clear all data entered",!
        WRITE "VIEW to view current data entered",!
        WRITE "^ to QUIT",!
        WRITE !
        QUIT
        ;   
 ;"NOTE: SEARCH ON 5/17/17 found  no calls into this, so renaming OR-->ORXX.  Can delete function later if no problems        
ORXX(LRTYPE,LRDFN,LRSS,LRIDT,LRUID,LRXQA,LRTST)    ;" Send OR (CPRS) notification
         ;"NOTE: I think that OR^TMGLRWU2 is the function that is being used more.  ??Duplicate??
         ;"NOTE: Copied from OR^LR7ORB3 for mod purposes
         ;" Call with LRTYPE = type OERR notification (currently supports 3, 14, 57)
         ;"               3 = NF_LAB_RESULTS 
         ;"               14 = NF_ABNORMAL_LAB_RESULTS
         ;"               57 = NF_CRITICAL_LAB_RESULTS
         ;"           LRDFN  = file #63 IEN
         ;"           LRSS   = file #63 subscript -- "CH" OR "MI"
         ;"                    Only supports CH and MI. AP subscript handled by separate API.
         ;"           LRIDT  = inverse d/t of entry in file #63
         ;"           LRUID  = accession's UID
         ;"           LRXQA  = recipient array, e.g. LRXQA(168)=""
         ;"           LRTST  = test IEN60 ^ Name of test being alerted ^ parent test ien60
         ;"Result: 1^Alert Sent, or -1^Error
         NEW DFN,LRMSG,LRPREFIX,LRX,LRY         
         NEW LRIENS   ;"a string to pass to EN^ORB3
         NEW LROIFN   ;"OK to be null "". OERR INTERNAL FILE #, an IEN100  
         NEW LROE     ;"OK to be null "". ORDER # (field 9.5) in 69.01
         NEW LRODT    ;"DATE ORDERED field (#3) in 68.02/ACCESSION NUMBER in 68.01/DATE in 68/ACCESSION
         NEW LRSN     ;"SPECIMEN NUMBER (#4) in 68.02/ACCESSION NUMBER in 68.01/DATE in 68/ACCESSION
         SET (LROE,LROIFN)=""
         NEW TMGRESULT SET TMGRESULT="1^Alert Sent"
         IF LRSS'?1(1"CH",1"MI") DO  GOTO ORDN
         . SET TMGRESULT="-1^Lab Subscript not supported"
         SET DFN=$PIECE(^LR(LRDFN,0),"^",3)
         SET LRPREFIX=$SELECT(LRTYPE=3:"",LRTYPE=14:"Abnormal ",LRTYPE=57:"Critical ",1:"")
         ;
         SET LRX=$$CHECKUID^LRWU4(LRUID,LRSS)  ;"RETURNS: 1(accession exists)^area^date^number, i.e. 1^IEN68^IEN68.01^IEN68.02       
         ;"IF LRX<1 QUIT "0^Accession's UID not valid"
         IF LRX<1 SET LRX=""  ;"//kt
         NEW IEN68 SET IEN68=$PIECE(LRX,"^",2)
         NEW IEN68D01 SET IEN68D01=$PIECE(LRX,"^",3)  ;"DATE field (a subfile -- 68.01)
         NEW IEN68D02 SET IEN68D02=$PIECE(LRX,"^",4)  ;"ACCESSION NUMBER subfield (a subfile -- 68.02)  
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
         . IF LRTYPE=14!(LRTYPE=57) SET LRMSG=LRPREFIX_"lab results:"
         . ELSE   SET LRMSG="Lab results:"
         IF LRSS="MI" DO
         . IF LRTYPE=14!(LRTYPE=57) SET LRMSG=LRPREFIX_"microbiology results:"
         . ELSE   SET LRMSG="Microbiology results:"
         ;
         SET LRMSG=LRMSG_" - ["_$PIECE(LRTST,"^",2)_"]"
         ;
         ;" OERR parameters:
         ;"            ORN: notification id (#100.9 ien). e.g. 3 <-- NF_LAB_RESULTS 
         ;"            |    ORBDFN: patient id (#2 ien)
         ;"            |    |    ORNUM: order number (#100 ien)
         ;"            |    |    |       ORBADUZ: recipient array
         ;"            |    |    |       |     ORBPMSG: message text
         ;"            |    |    |       |     |     ORBPDATA lab result reference
         ;"            |    |    |       |     |     |
         DO EN^ORB3(LRTYPE,DFN,LROIFN,.LRXQA,LRMSG,LRIENS)
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
ORDN     QUIT TMGRESULT
         ;
         ;        
 ;"NOTE:    IF Y=C-X  THEN X=C-Y   e.g. 100-7=93, and 100-93=7         
FMDT2RDT(FMDT)  ;"CONVERT FMDT --> LAB RDT
  ;"QUIT 9999999.999999-FMDT
  QUIT 9999999-FMDT
  ;
RDT2FMDT(RDT)   ;"CONVERT LAB RDT --> FMDT
  ;"QUIT 9999999.999999-RDT
  QUIT 9999999-RDT
  ;