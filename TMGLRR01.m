TMGLRR01 ;TMG/kst-Entry point for reading from LAB DATA file ;12/28/14, 4/1/18
              ;;1.0;TMG-LIB;**1**;8/14/13
 ;
 ;"TMG LAB RESULTS STORAGE API
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
 ;"GFRMTLAB(PTID,LAB,OUT,OPTION) -- GET FORMATED LAB REPORT
 ;"GETPLABS(PTID,LAB,OUT,OPTION) Return a list of values for given lab PANEL, for given patient
 ;"GETVALS(PTID,IEN60,OUT,OPTION) -- Return a list of values for given lab test, for given patient
 ;"
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;"GETLRDFN(DFN) -- CONVERT DFN (IEN IN 2)--> LRRDFN (IEN IN 63)
 ;"GETDFN(LRDFN) -- CONVERT LRRDFN (IEN IN 63) --> DFN (IEN IN 2) 
 ;"GETSTORE(IEN60) ;"Get storage field in subfile 63.04 in 63
 ;"
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;
TESTGVLS ;
        NEW DIC,X,Y
        SET DIC=2,DIC(0)="MAEQ"
        NEW OUT
        DO ^DIC WRITE !
        ;"SET Y=164
        IF +Y'>0 GOTO TGVLDN
        NEW DFN SET DFN=+Y_"^2"
        SET DIC=60
        ;"DO ^DIC WRITE !
        ;"IF +Y'>0 GOTO TGVLDN
        ;"NEW IEN60 SET IEN60=+Y    
        NEW IEN60 READ "ENTER LAB/TAXONOMY: ",IEN60:$GET(DTIME,3600),!    
        NEW OPTION 
        SET OPTION("SHOW DATE")=1
        SET OPTION("MAX CT")=3
        DO GFRMTLAB(DFN,IEN60,.OUT,.OPTION)
        IF $DATA(OUT) DO ZWRITE^TMGZWR("OUT")
TGVLDN  QUIT
        ;
GFRMTLAB(PTID,LAB,OUT,OPTION) ;"GET FORMATED LAB REPORT
        ;"Input: PTID -- IEN2^2, or IEN63^63  i.e. DFN^2 or LRDFN^63
        ;"       LAB -- UniqueLabName, or IEN60 -- for lab panel (or individual lab test)
        ;"       OUT -- PASS BY REFERENCE.  AN OUT PARAMETER.  Prior data NOT killed
        ;"         OUT(LabName)='Value on Date <-- Value on Date <-- ... '  <-- IF SHOW DATE option set. 
        ;"         OUT(LabName)='Value <-- Value <-- ... "
        ;"       OPTION -- PASS BY REFERENCE.  Options, as follows:
        ;"         OPTION("MAX CT") -- OPTIONAL.  DEFAULT = 999.  Max number of values to return
        ;"         OPTION("SDT") -- OPTIONAL.  FM DATE of start of date range to return from
        ;"         OPTION("EDT") -- OPTIONAL.  FM DATE of end of date range to return from
        ;"         OPTION("SHOW DATE")=1  OPTIONAL.  IF present, dates showed in output
        ;"         OPTION("DATES ONLY")=1 OPTIONAL.  If present, then ONLY dates shown.
        ;"         OPTION("SHOW NULL")=1 OPTIONAL.  If present then at least lab name is always returned, even IF no data. 
        ;"         OPTION("HIDE DUPLICATES")=1 OPTIONAL. If present then values within a close time frame will be omitted.
        ;"Result: none
        NEW TEMP
        DO GETPLABS(.PTID,.LAB,.TEMP,.OPTION) ;"GET LABS ARRAY
        NEW SHOWDT SET SHOWDT=$GET(OPTION("SHOW DATE"))
        NEW MAXCT SET MAXCT=+$GET(OPTION("MAX CT"))  ;"Need this for taxonomies
        ;"NEW HIDEDUPS SET HIDEDUPS=+$GET(OPTION("HIDE DUPLICATES"))  ;"3/2/18
        NEW DTONLY SET DTONLY=$GET(OPTION("DATES ONLY"))
        NEW IEN60 SET IEN60=0
        IF $DATA(TEMP("TAXONOMY")) DO  ;"CONVERT TAXONOMY ARRAY IN TO SINGLE PSEUDO TEST
        . NEW NAME SET NAME=$GET(TEMP("TAXONOMY","NAME")) QUIT:NAME=""
        . FOR  SET IEN60=$ORDER(TEMP(IEN60)) QUIT:(+IEN60'>0)  DO
        . . IF $PIECE(IEN60,"^",2)=NAME QUIT
        . . IF $DATA(TEMP("TAXONOMY",+IEN60))=0 QUIT
        . . MERGE TEMP("0.5^"_NAME)=TEMP(IEN60)
        . . KILL TEMP(IEN60)
        KILL TEMP("TAXONOMY")
        SET IEN60=0
        FOR  SET IEN60=$ORDER(TEMP(IEN60)) QUIT:(+IEN60'>0)  DO
        . NEW LABNAME SET LABNAME=$PIECE(IEN60,"^",2) QUIT:LABNAME=""
        . NEW OUTSTR SET OUTSTR=""
        . NEW DT SET DT=""
        . NEW COUNT SET COUNT=1  ;"8/14/18
        . ;"NEW LDATE SET LDATE=0  ;"3/2/18
        . FOR  SET DT=$ORDER(TEMP(IEN60,DT),-1) QUIT:(+DT'>0)!(COUNT>MAXCT)  DO
        . . ;"IF (LDATE'=0)&(HIDEDUPS=1)&($$TIMEDIFF^TMGDATE(DT,LDATE)<5) QUIT  ;"3/2/18
        . . ;"SET LDATE=DT  ;"3/2/18
        . . NEW VAL SET VAL=$GET(TEMP(IEN60,DT)) QUIT:VAL=""
        . . NEW STR
        . . IF DTONLY DO
        . . . SET STR=$$FMTE^XLFDT(DT,"5D")
        . . ELSE  DO
        . . . SET STR=VAL
        . . . IF SHOWDT SET STR=STR_" on "_$$FMTE^XLFDT(DT,"5D")
        . . SET COUNT=COUNT+1
        . . IF OUTSTR'="" SET OUTSTR=OUTSTR_" <-- "
        . . SET OUTSTR=OUTSTR_STR
        . SET OUT(LABNAME)=OUTSTR
        QUIT
        ;
GETPLABS(PTID,LAB,OUT,OPTION) ;"GET PATIENT LAB/LAB-PANEL LABS
        ;"Purpose: Return a list of values for given lab or lab panel, for given patient
        ;"Input: PTID -- IEN2^2, or IEN63^63  i.e. DFN^2 or LRDFN^63
        ;"       LAB -- UniqueLabName, or IEN60 -- for lab panel (or individual lab test)
        ;"       OUT -- PASS BY REFERENCE.  AN OUT PARAMETER.  Prior data NOT killed
        ;"          OUT(0,IEN60,"LAB NAME")=LABNAME
        ;"          OUT(IEN60^LABNAME,"STORAGE")=FLD63D04^DATANAME  <-- only IF "WANT RAW" options specified.
        ;"          OUT(IEN60^LABNAME,FMDATE)=VALUE
        ;"          OUT(IEN60^LABNAME,FMDATE,"ZN")=Entire 0 node for data storage <-- only IF "WANT RAW" options specified. 
        ;"       OPTION -- PASS BY REFERENCE.  Options, as follows:
        ;"         OPTION("MAX CT") -- OPTIONAL.  DEFAULT = 999.  Max number of values to return
        ;"         OPTION("SDT") -- OPTIONAL.  FM DATE of start of date range to return from
        ;"         OPTION("EDT") -- OPTIONAL.  FM DATE of end of date range to return from
        ;"         OPTION("WANT RAW")=1  IF present, then 0-node returned
        ;"         OPTION("SHOW NULL")=1 OPTIONAL.  If present then at least lab name is always returned, even IF no data. 
        ;"Results: none
        NEW IEN60,LABNAME,TEMP,DIC,X,Y,ONEIEN60
        SET LAB=$GET(LAB) 
        IF +LAB=LAB SET IEN60=LAB
        ELSE  DO
        . SET DIC=22717,DIC(0)="MX",X=LAB  ;"First look up in TMG LAB TAXONOMY file 
        . DO ^DIC 
        . IF +Y>0 SET IEN60=0 DO TAX2LST(+Y,.IEN60) QUIT
        . SET DIC=60,DIC(0)="MX",X=LAB 
        . DO ^DIC 
        . SET IEN60=+Y
        IF IEN60<0 GOTO GPLBDN
        DO GETVALS(PTID,.IEN60,.OUT,.OPTION) ;"GET INDIVIDUAL LABS
        NEW IEN60D02 SET IEN60D02=0
        FOR  SET IEN60D02=$ORDER(^LAB(60,+IEN60,2,IEN60D02)) QUIT:(IEN60D02'>0)  DO
        . SET ONEIEN60=+$PIECE($GET(^LAB(60,+IEN60,2,IEN60D02,0)),"^",1) QUIT:(ONEIEN60'>0)
        . DO GETVALS(PTID,ONEIEN60,.OUT,.OPTION) ;"GET INDIVIDUAL LABS
GPLBDN  QUIT
        ;
TAX2LST(IEN22717,LST,TAXNDONE) ;"Fill LST with IEN values from taxonomy list. 
        ;"Input: IEN22717 -- IEN IN TMG LAB TAXONOMY file. 
        ;"       LST -- PASS BY REFERENCE.  An OUT PARAMETER.  Format:
        ;"          LST(IEN60)=""
        ;"          LST(IEN60)=""
        ;"          LST("NAME")=Taxonomy name. 
        ;"       TAXNDONE -- OPTIONAL.  Used during recursive calls to prevent possible endless loops
        SET TAXNDONE(IEN22717)=""
        SET IEN22717=+$GET(IEN22717)
        IF $GET(LST("NAME"))="" SET LST("NAME")=$PIECE($GET(^TMG(22717,IEN22717,0)),"^",1)
        NEW IEN SET IEN=0
        FOR  SET IEN=$ORDER(^TMG(22717,IEN22717,1,IEN)) QUIT:+IEN'>0  DO
        . NEW VPTR SET VPTR=$GET(^TMG(22717,IEN22717,1,IEN,0)) QUIT:VPTR=""
        . NEW PTR SET PTR=+VPTR
        . NEW REF SET REF=$PIECE(VPTR,";",2)
        . IF REF="LAB(60," SET LST(PTR)="" QUIT
        . IF REF="TMG(22717," DO  QUIT
        . . IF $DATA(TAXNDONE(PTR))>0 QUIT  ;"Ignore IF done before, to prevent endless loop.
        . . DO TAX2LST(PTR,.LST,.TAXNDONE)
        QUIT
        ;
GETVALS(PTID,LAB,OUT,OPTION) ;"GET VALUES FOR LAB TEST 60       
        ;"Purpose: Return a list of values for given lab test, for given patient
        ;"Input: PTID -- IEN2^2, or IEN63^63  i.e. DFN^2 or LRDFN^63
        ;"       LAB -- UniqueLabName, or IEN60 (LABORATORY TEST) This is the test to retrieve
        ;"              Also allowed: LAB=0 <-- signal of list (i.e. a taxonomy)  
        ;"                            LAB(AnIEN60)=""  <-- first IEN60 to retrieve
        ;"                            LAB(AnIEN60)=""  <-- second IEN60 to retrieve
        ;"                            LAB("NAME")=<Taxonomy name> 
        ;"       OUT -- PASS BY REFERENCE.  AN OUT PARAMETER.  Prior data NOT killed
        ;"          OUT(0,IEN60,"LAB NAME")=LABNAME
        ;"          OUT(IEN60^LABNAME)=LABNAME  <-- ONLY IF "SHOW NULL" option specified
        ;"          OUT(IEN60^LABNAME,"STORAGE")=FLD63D04^DATANAME  <-- only IF "WANT RAW" options specified.
        ;"          OUT(IEN60^LABNAME,FMDATE)=VALUE
        ;"          OUT(IEN60^LABNAME,FMDATE,"ZN")=Entire 0 node for data storage <-- only IF "WANT RAW" options specified.
        ;"          OUT("TAXONOMY",IEN60)="" <-- IF taxonomy passed in. 
        ;"       OPTION -- PASS BY REFERENCE.  Options, as follows:
        ;"         OPTION("MAX CT") -- OPTIONAL.  DEFAULT = 999.  Max number of values to return
        ;"         OPTION("SDT") -- OPTIONAL.  FM DATE of start of date range to return from
        ;"         OPTION("EDT") -- OPTIONAL.  FM DATE of end of date range to return from
        ;"         OPTION("WANT RAW")=1  IF present, then 0-node and storage location are returned
        ;"         OPTION("SHOW NULL")=1 OPTIONAL.  If present then at least lab name is always returned, even IF no data. 
        ;"Results: None
        ;
        SET PTID=$GET(PTID) IF PTID="" GOTO GVLSDN
        NEW LRDFN SET LRDFN=0
        IF $PIECE(PTID,"^",2)=2 DO
        . SET LRDFN=$$GETLRDFN(+PTID) 
        ELSE  IF $PIECE(PTID,"^",2)=63 DO
        . SET LRDFN=+PTID
        IF LRDFN'>0 GOTO GVLSDN
        SET LAB=$GET(LAB)
        NEW IEN60
        IF LAB=0 DO  GOTO GVLSDN ;"0 is signal that LAB contains a list.
        . MERGE OUT("TAXONOMY")=LAB
        . NEW AIEN60 SET AIEN60=0
        . FOR  SET AIEN60=$ORDER(LAB(AIEN60)) QUIT:(+AIEN60'>0)  DO
        . . DO GETVALS(PTID,AIEN60,.OUT,.OPTION)  ;"Recursive call for each member of list. 
        IF +LAB=LAB SET IEN60=LAB
        ELSE  SET DIC=60,DIC(0)="M",X=LAB DO ^DIC SET IEN60=+Y
        IF IEN60'>0 GOTO GVLSDN
        NEW SDT SET SDT=$GET(OPTION("SDT"),9999999) NEW RSDT SET RSDT=$$FMDT2RDT^TMGLRWU1(SDT)
        NEW EDT SET EDT=$GET(OPTION("EDT"),0) NEW REDT SET REDT=$$FMDT2RDT^TMGLRWU1(EDT)
        NEW WANTRAW SET WANTRAW=$GET(OPTION("WANT RAW"))
        NEW HIDEDUPS SET HIDEDUPS=+$GET(OPTION("HIDE DUPLICATES"))  ;"3/5/18
        SET IEN60=+$GET(IEN60) IF IEN60'>0 GOTO GVLSDN
        NEW LABNAME SET LABNAME=$PIECE($GET(^LAB(60,+IEN60,0)),"^",1)
        SET IEN60=IEN60_"^"_LABNAME
        SET OUT(0,+IEN60,"LAB NAME")=LABNAME
        IF +$GET(OPTION("SHOW NULL")) SET OUT(IEN60)=LABNAME
        NEW FOUND SET FOUND=0
        NEW FLD63D04 SET FLD63D04=$$GETSTORE(+IEN60) IF FLD63D04'>0 GOTO GVLSDN
        NEW CT SET CT=+$GET(OPTION("MAX CT")) IF CT'>0 SET CT=999
        NEW RT SET RT=RSDT IF RT>0 SET RT=RT-0.1
        NEW LDATE SET LDATE=0  ;"3/5/18
        FOR  SET RT=$ORDER(^LR(LRDFN,"CH",RT)) QUIT:(+RT>REDT)!(+RT'>0)!(CT'>0)  DO
        . IF $DATA(^LR(LRDFN,"CH",RT,+FLD63D04))=0 QUIT
        . NEW FMDT SET FMDT=$$RDT2FMDT^TMGLRWU1(RT)
        . IF (LDATE'=0)&(HIDEDUPS=1)&($$TIMEDIFF^TMGDATE(FMDT,LDATE)<5) QUIT ;"3/5/18
        . SET LDATE=FMDT  ;"3/5/18
        . NEW ZNODE SET ZNODE=$GET(^LR(LRDFN,"CH",RT,+FLD63D04))
        . NEW VALUE SET VALUE=$PIECE(ZNODE,"^",1)
        . NEW FLAG SET FLAG=$PIECE(ZNODE,"^",2)
        . IF (FLAG="H")!(FLAG="L")!(FLAG="HH")!(FLAG="LL") SET VALUE=VALUE_FLAG ;"4/6/20  elh added critical flags (HH and LL)
        . SET OUT(IEN60,FMDT)=VALUE
        . SET FOUND=FOUND+1
        . IF WANTRAW SET OUT(IEN60,FMDT,"ZN")=ZNODE
        . SET CT=CT-1
        IF FOUND=0 GOTO GVLSDN
        ;"SET OUT(0,+IEN60,"LAB NAME")=LABNAME
        IF WANTRAW SET OUT(IEN60,"STORAGE")=FLD63D04  
GVLSDN  QUIT
        ;
GETDFN(LRDFN) ;"CONVERT LRRDFN (IEN IN 63) --> DFN (IEN IN 2) 
        ;"Result: IEN in file 2 or 0 IF not found
        ;"NOTE: depends on custom TMG cross reference in file 2. "ATMGLR"
        SET LRDFN=+$GET(LRDFN)
        NEW TMGRESULT SET TMGRESULT=+$ORDER(^DPT("ATMGLR",LRDFN,0))
        QUIT TMGRESULT
        ;
GETLRDFN(DFN) ;"CONVERT DFN (IEN IN 2)--> LRRDFN (IEN IN 63)
        ;"Result: IEN in file 63, or 0 IF not found
        SET DFN=+$GET(DFN)
        NEW TMGRESULT SET TMGRESULT=+$GET(^DPT(DFN,"LR"))
        QUIT TMGRESULT
        ;
GETSTORE(IEN60) ;"Get storage field in subfile 63.04 in 63
        ;"Input: IEN60 -- IEN in LABORATORY TEST file.
        ;"Result: Field_in_63.04^DataName, OR 0 IF not found.
        NEW TMGRESULT      
        SET IEN60=+$GET(IEN60)
        NEW FLD63D04 SET FLD63D04=+$PIECE($GET(^LAB(60,IEN60,.2)),"^",1)
        NEW DATANAME SET DATANAME=$PIECE($GET(^DD(63.04,FLD63D04,0)),"^",1)
        SET TMGRESULT=FLD63D04_"^"_DATANAME
        QUIT TMGRESULT
        ;        
