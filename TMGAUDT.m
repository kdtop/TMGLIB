TMGAUDT ;TMG/kst/Entry points for Audit API ; 10/23/13, 6/3/24
        ;;1.0;TMG-LIB;**1**;10/23/13
        ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"
 ;"=======================================================================
 ;" RPC -- Public Functions.
 ;"=======================================================================
 ;"
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"
 ;"=======================================================================
 ;"=======================================================================
 ;"Dependencies:
 ;"=======================================================================
 ;"=======================================================================
 ;
GTDETAIL(OUT,STR) ;"GET AUDIT ENTRY DETAIL
        ;"Purpose: RPC entry point for geting audit detail
        ;"Input: OUT -- PASS BY REFERENCE, and OUT PARAMETER.  Format:
        ;"              
        ;"       STR -- Descriptor of record to get.  Format: 'File;IENS'
        ;"            Note: file 8994.81 --> logged RPC calls.
        ;"                  All other files --> Fileman audit log. 
        ;"Result: None
        NEW TMGAUDEBUG SET TMGAUDEBUG=0
        IF TMGAUDEBUG=1 DO
        . SET STR=$GET(^TMG("TMP","RPC","GTDETAIL^TMGAUDT"))
        ELSE  DO
        . SET ^TMG("TMP","RPC","GTDETAIL^TMGAUDT")=STR        
        SET OUT(0)="1^OK"
        NEW CT SET CT=0
        SET STR=$GET(STR)
        NEW FILE SET FILE=+$PIECE(STR,";",1)
        IF FILE'>0 DO  GOTO GTDTLDN
        . SET OUT(0)="-1^File number not provided."
        NEW IENS SET IENS=$PIECE(STR,";",2,99)
        IF IENS="" DO  GOTO GTDTLDN
        . SET OUT(0)="-1^IENS not provided."
        NEW TMGARR,TMGMSG
        IF FILE="8994.81" DO
        . DO GETS^DIQ(8994.81,IENS,"*","E","TMGARR","TMGMSG")
        . IF $DATA(TMGMSG) DO  QUIT
        . . SET OUT(0)="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        . NEW FLD SET FLD=0
        . FOR  SET FLD=$ORDER(TMGARR(8994.81,IENS,FLD)) QUIT:+FLD'>0  DO
        . . NEW FLDNAME SET FLDNAME=$PIECE($GET(^DD(8994.81,FLD,0)),"^",1)
        . . SET CT=CT+1,OUT(CT)=FLD_"^"_FLDNAME_"^"_$GET(TMGARR(8994.81,IENS,FLD,"E"))
        ELSE  DO
        . NEW TMGARR
        . SET FILE=$PIECE(IENS,";",1)
        . NEW AUIEN SET AUIEN=$PIECE(IENS,";",2)
        . DO GETAUDIT(.TMGARR,FILE,AUIEN,"","E")
        . NEW FLD SET FLD=0
        . FOR  SET FLD=$ORDER(TMGARR(1.1,IENS,FLD)) QUIT:+FLD'>0  DO
        . . NEW FLDNAME SET FLDNAME=$PIECE($GET(^DD(1.1,FLD,0)),"^",1)
        . . SET CT=CT+1,OUT(CT)=FLD_"^"_FLDNAME_"^"_$GET(TMGARR(1.1,IENS,FLD,"E"))
GTDTLDN QUIT
        ;
GETAUDT1(OUT,USERIEN,SDT,EDT,MODE) ;"PER USER AUDIT
        ;"Purpose: return an audit log for 1 user, for 1 file, for given date range
        ;"Input: OUT -- PASS BY REFERENCE, an OUT parameter.
        ;"       USERIEN -- OPTIONAL.  IEN IN file 200 (NEW PERSON)
        ;"       SDT -- Fileman date, or phrase (e.g. 'T-1'). Start Date. OPTIONAL.  DEFAULT=TODAY@00:00
        ;"       EDT -- Fileman date or phrase.  End Date.   OPTIONAL.  DEFAULT=TODAY@23:59
        ;"       MODE -- OPTIONAL. DEFAULT=1
        ;"            1 -- sort entries by patient name
        ;"            2 -- sort entries by date/time of first interaction
        ;"Results: OUT(0)=1^Success, or -1^<Error Message>
        ;"         OUT(#)=DFN^PatientName -- DateTimeOfFirstInteraction  <-- IF mode 1
        ;"         OUT(#)=DFN^DateTimeOfFirstInteraction -- PatientName  <-- IF mode 2
        NEW TMGRESULT,TEMPARR,SORTARR,NAMESORTARR
        NEW SHOULDGARBLE SET SHOULDGARBLE=$$SHOULDGARBLE^TMGMISC4()        
        SET MODE=+$GET(MODE) IF MODE'>0 SET MODE=1
        DO PREPDATS(.SDT,.EDT) ;"PREP DATES
        SET TMGRESULT=$$GACCES1(.TEMPARR,.USERIEN,.SDT,.EDT) ;"-- GET BRIEF ACCESS LIST FROM FILE 1.1 (AUDIT)
        IF +TMGRESULT<0 GOTO GA1DN
        SET TMGRESULT=$$GRPCUAUL(.TEMPARR,.USERIEN,.SDT,.EDT) ;"GET AUDIT LIST, PER USER, FROM FILE 8994.81 (LOGGABLE RPC)
        IF +TMGRESULT<0 GOTO GA1DN
        NEW IDX SET IDX=0
        NEW FILE SET FILE=0
        FOR  SET FILE=$ORDER(TEMPARR(FILE)) QUIT:(+FILE'>0)  DO
        . NEW DUZ SET DUZ=0
        . FOR  SET DUZ=$ORDER(TEMPARR(FILE,DUZ)) QUIT:(+DUZ'>0)  DO
        . . NEW DT SET DT=0
        . . FOR  SET DT=$ORDER(TEMPARR(FILE,DUZ,DT)) QUIT:(+DT'>0)  DO
        . . . NEW TMGDFN SET TMGDFN=0
        . . . FOR  SET TMGDFN=$ORDER(TEMPARR(FILE,DUZ,DT,TMGDFN)) QUIT:(+TMGDFN'>0)  DO
        . . . . NEW NAME SET NAME=$GET(TEMPARR(FILE,DUZ,DT,TMGDFN))
        . . . . IF SHOULDGARBLE SET NAME=$$GARBLENAME^TMGMISC4(NAME)
        . . . . NEW ADT SET ADT=DT IF ADT'["." SET ADT=ADT_".999999"
        . . . . SET SORTARR(DT\1,ADT,TMGDFN)=NAME
        . . . . SET NAMESORTARR(NAME,DT\1,ADT)=TMGDFN
        IF MODE=1 DO  ;"sort by name
        . NEW NAME SET NAME=""
        . FOR  SET NAME=$ORDER(NAMESORTARR(NAME)) QUIT:(NAME="")  DO
        . . NEW DAY SET DAY=0
        . . FOR  SET DAY=$ORDER(NAMESORTARR(NAME,DAY)) QUIT:+DAY'>0  DO
        . . . NEW DT SET DT=$ORDER(NAMESORTARR(NAME,DAY,0))
        . . . NEW TMGDFN SET TMGDFN=$GET(NAMESORTARR(NAME,DAY,DT))
        . . . NEW DTSTR SET DTSTR=$$LJ^XLFSTR($$FMTE^XLFDT(DT,"7MZ"),16)
        . . . NEW STR SET STR=TMGDFN_"^"_NAME_" -- "_DTSTR
        . . . SET IDX=IDX+1,OUT(IDX)=STR
        ELSE  IF MODE=2 DO  ;"sort by date/time
        . NEW SHOWN
        . NEW DAY SET DAY=0
        . FOR  SET DAY=$ORDER(SORTARR(DAY)) QUIT:+DAY'>0  DO
        . . NEW DT SET DT=0
        . . FOR  SET DT=$ORDER(SORTARR(DAY,DT)) QUIT:+DT'>0  DO
        . . . NEW TMGDFN SET TMGDFN=0
        . . . FOR  SET TMGDFN=$ORDER(SORTARR(DAY,DT,TMGDFN)) QUIT:+TMGDFN'>0  DO
        . . . . IF $DATA(SHOWN(TMGDFN,DAY)) QUIT
        . . . . NEW ADT SET ADT=DT IF ADT[".9999" DO
        . . . . . SET ADT=ADT\1
        . . . . NEW NAME SET NAME=$GET(SORTARR(DAY,DT,TMGDFN))
        . . . . NEW DTSTR SET DTSTR=$$LJ^XLFSTR($$FMTE^XLFDT(ADT,"7MZ"),16)
        . . . . NEW STR SET STR=TMGDFN_"^"_DTSTR_" "_NAME
        . . . . SET IDX=IDX+1,OUT(IDX)=STR
        . . . . SET SHOWN(TMGDFN,DAY)=1
GA1DN   SET OUT(0)=TMGRESULT
        QUIT
        ;
GETAUDT2(OUT,TMGDFN,MODE,SDT,EDT) ;"PER PATIENT AUDIT
        ;"Purpose: return an audit log for 1 user, for 1 file, for given date range
        ;"Input: OUT -- PASS BY REFERENCE, an OUT parameter.
        ;"       TMGDFN -- IEN IN file 2 (PATIENT)
        ;"       MODE -- OPTIONAL. DEFAULT=1
        ;"            1 -- sort entries by USER, RPC
        ;"            2 -- sort entries by USER, DateTime
        ;"            3 -- sort entries by date/time
        ;"       SDT -- Fileman date, or phrase (e.g. 'T-1'). Start Date. OPTIONAL.  DEFAULT=TODAY@00:00
        ;"       EDT -- Fileman date or phrase.  End Date.   OPTIONAL.  DEFAULT=TODAY@23:59
        ;"Results: OUT(0)=1^Success, or -1^<Error Message>
        ;"         OUT(#)=File;IENS^UserName^DateTime^RPC Called  <-- mode 1
        ;"         OUT(#)=File;IENS^UserName^RPCC_Called^DateTime <-- mode 2
        ;"         OUT(#)=File;IENS^DateTime^UserName^RPCC_Called <-- mode 3
        ;"
        KILL OUT
        SET OUT(0)="1^Success"
        IF TMGDFN="" DO  GOTO GAU2DN
        . SET OUT(0)="-1^No patient defined"
        DO PREPDATS(.SDT,.EDT) ;"PREP DATES
        SET MODE=+$GET(MODE) IF +MODE'>0 SET MODE=1       
        NEW TEMPARR,FLDS SET FLDS=".001;.01;1"
        NEW NAMEDATEARR,NAMERPCARR,DATEARR
        NEW TMGRESULT SET TMGRESULT=$$GRPCPAUL(.TEMPARR,TMGDFN,FLDS,"EN",SDT,EDT) ;"GET AUDIT LIST PER PATIENT FROM FILE 8994.81 (AUDIT LOG FOR RPCS)
        IF +TMGRESULT<0 DO  GOTO GAU2DN
        . SET OUT(0)=TMGRESULT
        NEW ADT SET ADT=0
        FOR  SET ADT=$ORDER(TEMPARR(8994.81,TMGDFN,ADT)) QUIT:ADT'>0  DO
        . NEW USER,RPC,DATE,FORMAT
        . SET FORMAT=$ORDER(TEMPARR(8994.81,TMGDFN,ADT,1,"A"))
        . SET USER=$GET(TEMPARR(8994.81,TMGDFN,ADT,1,FORMAT))
        . SET RPC=$GET(TEMPARR(8994.81,TMGDFN,ADT,.01,FORMAT))
        . SET DATE=$GET(TEMPARR(8994.81,TMGDFN,ADT,.001,FORMAT))
        . ;"SET NAMEDATEARR(USER,DATE)=RPC,NAMERPCARR(USER,RPC,DATE)="",DATEARR(DATE)=USER_"^"_RPC
        . SET (NAMEDATEARR(USER,DATE,RPC),NAMERPCARR(USER,RPC,DATE),DATEARR(DATE,USER,RPC))="8994.81;"_ADT
        SET FLDS=".02;.04;4.1"
        KILL TEMPARR
        DO GETADTLS(.TEMPARR,2,TMGDFN,FLDS,"IEN",SDT,EDT)
        NEW AUIEN SET AUIEN=0
        FOR  SET AUIEN=$ORDER(TEMPARR(2,TMGDFN,AUIEN)) QUIT:AUIEN'>0  DO
        . NEW ADUZ,USER,OPTION,DATE,FORMAT,ADT
        . SET ADUZ=$GET(TEMPARR(2,TMGDFN,AUIEN,.04,"I"))
        . SET USER=$GET(TEMPARR(2,TMGDFN,AUIEN,.04,"E"))
        . SET OPTION=$GET(TEMPARR(2,TMGDFN,AUIEN,4.1,"E"))
        . SET ADT=$GET(TEMPARR(2,TMGDFN,AUIEN,.02,"I"))
        . SET DATE=$GET(TEMPARR(2,TMGDFN,AUIEN,.02,"E"))
        . SET (NAMEDATEARR(USER,DATE,OPTION),NAMERPCARR(USER,OPTION,DATE),DATEARR(DATE,USER,OPTION))="1.1;2;"_AUIEN
        NEW IDX SET IDX=0
        NEW USER,RPC,DATE
        SET USER="A",RPC="A",DATE=""
        IF MODE=1 DO  ;"Sort by name,date
        . FOR  SET USER=$ORDER(NAMEDATEARR(USER)) QUIT:USER=""  DO
        . . FOR  SET DATE=$ORDER(NAMEDATEARR(USER,DATE)) QUIT:DATE=""  DO
        . . . FOR  SET RPC=$ORDER(NAMEDATEARR(USER,DATE,RPC)) QUIT:RPC=""  DO
        . . . . SET IDX=IDX+1
        . . . . SET OUT(IDX)=$GET(NAMEDATEARR(USER,DATE,RPC))_"^"_USER_"^"_DATE_"^"_RPC
        ELSE  IF MODE=2 DO  ;" Sort by name,RPC
        . FOR  SET USER=$ORDER(NAMERPCARR(USER)) QUIT:USER=""  DO
        . . FOR  SET RPC=$ORDER(NAMERPCARR(USER,RPC)) QUIT:RPC=""  DO
        . . . FOR  SET DATE=$ORDER(NAMERPCARR(USER,RPC,DATE)) QUIT:DATE=""  DO
        . . . . SET IDX=IDX+1
        . . . . SET OUT(IDX)=$GET(NAMERPCARR(USER,RPC,DATE))_"^"_USER_"^"_RPC_"^"_DATE
        ELSE  IF MODE=3 DO  ;" Sort by date
        . FOR  SET DATE=$ORDER(DATEARR(DATE)) QUIT:DATE=""  DO
        . . FOR  SET USER=$ORDER(DATEARR(DATE,USER)) QUIT:USER=""  DO
        . . . FOR  SET RPC=$ORDER(DATEARR(DATE,USER,RPC)) QUIT:RPC=""  DO
        . . . . SET IDX=IDX+1
        . . . . SET OUT(IDX)=$GET(DATEARR(DATE,USER,RPC))_"^"_DATE_"^"_USER_"^"_RPC
        ;
GAU2DN  QUIT
        ;        
GRPCUAUL(OUT,TMGDUZ,SDT,EDT) ;"GET AUDIT LIST, PER USER, FROM FILE 8994.81 (LOGGABLE RPC)
        ;"Purpose: return list of patients seen during date range, by specified user
        ;"Input: OUT -- PASS BY REFERENCE, AN OUT PARAMETER. Format:
        ;"              OUT(8994.81,DUZ,FMDATE,DFN)="<PATIENT NAME>"
        ;"       TMGDUZ -- OPTIONAL.  User IEN to check
        ;"       SDT -- Fileman date, or phrase (e.g. 'T-1'). Start Date. OPTIONAL.  DEFAULT=TODAY@00:00
        ;"       EDT -- Fileman date or phrase.  End Date.   OPTIONAL.  DEFAULT=TODAY@23:59
        ;"Result: 1^OK, OR -1^ErrorMsg
        NEW TMGRESULT SET TMGRESULT="1^OK"
        SET TMGDUZ=+$GET(TMGDUZ)       
        IF TMGDUZ>0,$DATA(^VA(200,TMGDUZ,0))'>0 DO  GOTO GRUAUDN 
        . SET TMGRESULT="-1^Invalid User parameter. Got: '"_TMGDUZ_"'"
        DO PREPDATS(.SDT,.EDT) ;"PREP DATES
        NEW ADUZ SET ADUZ=0
        FOR  SET ADUZ=$ORDER(^XUSEC(8994,"USR",ADUZ)) QUIT:ADUZ'>0  DO
        . IF TMGDUZ>0,TMGDUZ'=ADUZ QUIT        
        . NEW ADT SET ADT=(SDT-0.00000001)
        . FOR  SET ADT=$ORDER(^XUSEC(8994,"USR",ADUZ,ADT)) QUIT:(+ADT'>0)!(ADT>EDT)  DO
        . . NEW TMGDFN SET TMGDFN=0
        . . FOR  SET TMGDFN=$ORDER(^XUSEC(8994,"USR",ADUZ,ADT,TMGDFN)) QUIT:(+TMGDFN'>0)  DO
        . . . SET OUT(8994.81,ADUZ,ADT,TMGDFN)=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
GRUAUDN QUIT TMGRESULT
        ;                
GRPCPAUL(OUT,TMGDFN,FLDS,FLAGS,SDT,EDT) ;"GET AUDIT LIST, PER PT, FROM FILE 8994.81 (AUDIT LOG FOR RPCS)
        ;"Purpose: return a detailed list of RPC calls for a given patient for date range
        ;"Input: OUT -- PASS BY REFERENCE, AN OUT PARAMETER. Format:
        ;"              OUT(8994.81,DFN,IENS,Fld#)=<value>   <--- IF FLAGS don't contain I or E
        ;"              OUT(8994.81,DFN,IENS,Fld#,"I")=<Internal value>   <--- IF FLAGS contain I
        ;"              OUT(8994.81,DFN,IENS,Fld#,"E")=<External value>   <--- IF FLAGS contain E
        ;"       TMGDFN -- Patient record number in PATIENT file to get RPC audit trail on
        ;"       FLDS -- OPTIONAL.  Default is "*"
        ;"               Allowed format is as per parameter 'FIELD' for GETS^DIQ
        ;"       FLAGS -- (OPTIONAL) String with one or more of the following flags
        ;"                I -- return INTERNAL values
        ;"                E -- return EXTERNAL values
        ;"                N -- DO not return NULL values
        ;"       SDT -- Fileman date, or phrase (e.g. 'T-1'). Start Date. OPTIONAL.  DEFAULT=TODAY@00:00
        ;"       EDT -- Fileman date or phrase.  End Date.   OPTIONAL.  DEFAULT=TODAY@23:59
        ;"Result: 1^OK, OR -1^ErrorMsg
        ;"Output: OUT is filled. See above. 
        SET FILE=+$GET(FILE)
        NEW TMGRESULT SET TMGRESULT="1^OK"
        SET TMGDFN=+$GET(TMGDFN) IF TMGDFN'>0 DO  GOTO GRADTDN
        . SET TMGRESULT="-1^Invalid DFN parameter."
        SET FLDS=$GET(FLDS,"*")   
        DO PREPDATS(.SDT,.EDT) ;"PREP DATES
        NEW ADT SET ADT=(SDT-0.00000001)
        FOR  SET ADT=$ORDER(^XUSEC(8994,"APAT",TMGDFN,ADT)) QUIT:(+ADT'>0)!(ADT>EDT)!(+TMGRESULT=-1)  DO
        . NEW TMGARR,TMGMSG
        . DO GETS^DIQ(8994.81,ADT_",",.FLDS,.FLAGS,"TMGARR","TMGMSG")
        . IF $DATA(TMGMSG) DO  QUIT  
        . . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)                             
        . MERGE OUT(8994.81,TMGDFN)=TMGARR(8994.81)
GRADTDN QUIT TMGRESULT
        ;        
GACCES1(OUT,TMGDUZ,SDT,EDT) ;"-- GET BRIEF ACCESS LIST FROM FILE 1.1 (AUDIT)
        ;"Purpose: return list of patients seen during date range, by specified user
        ;"Input: OUT -- PASS BY REFERENCE, AN OUT PARAMETER. Format:
        ;"              OUT(1.1,DUZ,FMDATE,DFN)="<PATIENT NAME>
        ;"       TMGDUZ -- OPTIONAL.  User IEN to check.  
        ;"       SDT -- Fileman date, or phrase (e.g. 'T-1'). Start Date. OPTIONAL.  DEFAULT=TODAY@00:00
        ;"       EDT -- Fileman date or phrase.  End Date.   OPTIONAL.  DEFAULT=TODAY@23:59
        ;"Result: 1^OK, OR -1^ErrorMsg
        NEW TMGRESULT SET TMGRESULT="1^OK"
        SET TMGDUZ=+$GET(TMGDUZ)
        IF TMGDUZ>0,$DATA(^VA(200,TMGDUZ,0))'>0 DO  GOTO GACS1 
        . SET TMGRESULT="-1^Invalid User parameter. Got: '"_TMGDUZ_"'"
        DO PREPDATS(.SDT,.EDT) ;"PREP DATES
        NEW ADUZ SET ADUZ=0
        FOR  SET ADUZ=$ORDER(^DIA(2,"D",ADUZ)) QUIT:ADUZ'>0  DO
        . IF TMGDUZ>0,TMGDUZ'=ADUZ QUIT
        . NEW DONE SET DONE=0
        . ;"NOTE: I am assuming that DATE/TIME increases with record number.  So IEN's are in chronological order. 
        . NEW IEN SET IEN=$ORDER(^DIA(2,"D",ADUZ,""),-1)  ;"plan reverse order scan
        . FOR  SET IEN=$ORDER(^DIA(2,"D",ADUZ,IEN),-1) QUIT:(+IEN'>0)!DONE  DO
        . . NEW ZN SET ZN=$GET(^DIA(2,IEN,0))
        . . NEW ADT SET ADT=$PIECE(ZN,"^",2)  ;"0;2 = DATE/TIME RECORDED
        . . IF ADT<SDT SET DONE=1  ;"Assuming IEN's are chronological, no need to go futher back in time. 
        . . IF (ADT<SDT)!(ADT>EDT) QUIT
        . . NEW TEMPARR DO GETAUDIT(.TEMPARR,2,IEN,"2.9","IE") 
        . . NEW TMGDFN SET TMGDFN=+$GET(TEMPARR(1.1,IEN,2.9,"E"))
        . . SET OUT(1.1,ADUZ,ADT,TMGDFN)=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
GACS1   QUIT TMGRESULT
        ;
GETADTLS(OUT,FILE,IEN,FLDS,FLAGS,SDT,EDT) ;"GET DETAILED AUDIT LIST FROM FILE 1.1 (AUDIT FILE)
        ;"Input: OUT -- PASS BY REFERENCE, AN OUT PARAMETER. Format:
        ;"              OUT(<FILE>,<IEN>,<AUIEN>,Fld#)=<value>   <--- IF FLAGS don't contain I or E
        ;"              OUT(<FILE>,<IEN>,<AUIEN>,Fld#,"I")=<Internal value>   <--- IF FLAGS contain I
        ;"              OUT(<FILE>,<IEN>,<AUIEN>,Fld#,"E")=<External value>   <--- IF FLAGS contain E
        ;"              NOTE: AUIEN is IEN in file 1.1, e.g. '^DIA(<FILE>,AUIEN,'
        ;"       FILE -- File NUMBER that audited record is in, not file# 1.1,
        ;"       IEN -- record number in FILE to get audit trail on
        ;"       FLDS -- OPTIONAL.  Default is ".01:9999"
        ;"               Allowed format is as per parameter 'DR' for EN^DIQ1
        ;"       FLAGS -- (OPTIONAL) String with one or more of the following flags
        ;"                I -- return INTERNAL values
        ;"                E -- return EXTERNAL values
        ;"                N -- DO not return NULL values
        ;"       SDT -- Fileman date, or phrase (e.g. 'T-1'). Start Date. OPTIONAL.  DEFAULT=TODAY@00:00
        ;"       EDT -- Fileman date or phrase.  End Date.   OPTIONAL.  DEFAULT=TODAY@23:59
        ;"Result: None
        SET FILE=+$GET(FILE)
        DO PREPDATS(.SDT,.EDT) ;"PREP DATES
        NEW AUIEN SET AUIEN=0
        FOR  SET AUIEN=$ORDER(^DIA(FILE,"B",IEN,AUIEN)) QUIT:+AUIEN'>0  DO
        . NEW TMGARR
        . NEW REF SET REF="^DIA("_FILE_","_AUIEN_",0)"
        . NEW ZN,ADT SET ZN=$GET(@REF),ADT=$PIECE(ZN,"^",2)
        . ;"WRITE "SDT=",SDT,"  -- ADT=",ADT,"  -- EDT=",EDT,!
        . IF (ADT<SDT)!(ADT>EDT) QUIT  ;"Skip IF not in date range. 
        . DO GETAUDIT(.TMGARR,FILE,AUIEN,.FLDS,.FLAGS)
        . MERGE OUT(FILE,IEN)=TMGARR(1.1)
        QUIT
        ;
GETAUDIT(TMGAUOUT,FILE,AUIEN,FLDS,FLAGS) ;"GET 1 AUDIT RECORD FROM FILE 1.1 (AUDIT FILE)
        ;"Input: TMGAUOUT -- PASS BY REFERENCE, AN OUT PARAMETER.  Prior array killed. 
        ;"         Format: TMGAUOUT(1.1,AUIEN,Fld#)=<value>   <--- IF FLAGS don't contain I or E
        ;"                 TMGAUOUT(1.1,AUIEN,Fld#,"I")=<Internal value>   <--- IF FLAGS contain I
        ;"                 TMGAUOUT(1.1,AUIEN,Fld#,"E")=<External value>   <--- IF FLAGS contain E
        ;"       FILE -- File NUMBER that audited record is in, not file# 1.1,
        ;"       AUIEN -- Audit record number in AUDIT FILE to get. (NOT IEN in FILE)
        ;"       FLDS -- OPTIONAL.  Default is ".01:99999"
        ;"               Allowed format is as per parameter 'DR' for EN^DIQ1
        ;"       FLAGS -- (OPTIONAL) String with one or more of the following flags
        ;"                I -- return INTERNAL values
        ;"                E -- return EXTERNAL values
        ;"                N -- DO not return NULL values
        ;"Result: None
        NEW TMGARR,DIC,DR,DA,DIQ,DIA KILL TMGAUOUT
        SET FILE=+$GET(FILE),DIA=FILE
        SET DIC="^DIA("_FILE_","
        SET DR=$GET(FLDS) IF DR="" SET DR=".01:99999"
        SET DA=+$GET(AUIEN)   
        SET DIQ="TMGAUOUT",DIQ(0)=$GET(FLAGS)
        DO EN^DIQ1
GADTDN  QUIT
        ;
PREPDATS(SDT,EDT) ;"PREP DATES
        DO PREP1DAT(.SDT)
        IF SDT'>0 SET SDT=$$NOW^XLFDT\1
        DO PREP1DAT(.EDT)
        IF EDT'>0 SET EDT=$$NOW^XLFDT\1
        IF EDT'["." SET EDT=EDT+0.99999999
        QUIT
        ;        
PREP1DAT(DT) ;"PREP 1 DATE
        SET DT=$GET(DT)
        IF +DT'=DT DO
        . NEW %DT SET %DT="T"
        . NEW X SET X=DT
        . NEW Y DO ^%DT SET DT=Y
        QUIT
        ;
TEST    ;
        NEW SDT,EDT,%DT,X,Y
        SET %DT=""
        SET X="T" DO ^%DT SET EDT=Y
        SET X="T-30D" DO ^%DT SET SDT=Y
        NEW TEMPARR
        ;"DO GETADTLS(.TEMPARR,2,9182,".02;.04;.06;4.1","NEI",SDT,EDT)
        DO GETADTLS(.TEMPARR,2,9182,".01:9999","NEI",SDT,EDT)
        IF $DATA(TEMPARR) DO ZWRITE^TMGZWR("TEMPARR")
        QUIT
        ;
TEST2   ;        
        NEW SDT,EDT,%DT,X,Y
        SET %DT=""
        SET X="T" DO ^%DT SET EDT=Y
        SET X="T-30D" DO ^%DT SET SDT=Y
        NEW TMGDFN SET TMGDFN=69928
        NEW TEMPARR,FLDS SET FLDS=".001;.01;1"
        NEW TMGRESULT SET TMGRESULT=$$GRPCPAUL(.TEMPARR,TMGDFN,FLDS,"EN",SDT,EDT) ;"GET AUDIT LIST PER PATIENT FROM FILE 8994.81 (AUDIT LOG FOR RPCS)
        IF +TMGRESULT'>0 WRITE TMGRESULT,!
        IF $DATA(TEMPARR) DO ZWRITE^TMGZWR("TEMPARR")
        QUIT
        ;
TEST3   ;        
        NEW SDT,EDT,%DT,X,Y
        SET %DT=""
        SET X="T" DO ^%DT SET EDT=Y
        SET X="T-5D" DO ^%DT SET SDT=Y
        NEW TEMPARR
        NEW TMGDUZ SET TMGDUZ=168
        NEW TMGRESULT SET TMGRESULT=$$GRPCUAUL(.TEMPARR,TMGDUZ,SDT,EDT) ;"GET AUDIT LIST, PER USER, FROM FILE 8994.81 (LOGGABLE RPC)
        IF +TMGRESULT'>0 WRITE TMGRESULT,!
        IF $DATA(TEMPARR) DO ZWRITE^TMGZWR("TEMPARR")
        QUIT
        ;        
TEST4   ;        
        NEW SDT,EDT,%DT,X,Y
        SET %DT=""
        SET X="T" DO ^%DT SET EDT=Y
        SET X="T-5D" DO ^%DT SET SDT=Y
        NEW TEMPARR
        NEW TMGDUZ SET TMGDUZ=168
        NEW TMGRESULT SET TMGRESULT=$$GACCES1(.TEMPARR,TMGDUZ,SDT,EDT) ;"GET AUDIT LIST, PER USER, FROM FILE 8994.81 (LOGGABLE RPC)
        IF +TMGRESULT'>0 WRITE TMGRESULT,!
        IF $DATA(TEMPARR) DO ZWRITE^TMGZWR("TEMPARR")
        QUIT
        ;        
TEST5   ;        
        NEW SDT,EDT,%DT,X,Y
        SET %DT=""
        SET X="T" DO ^%DT SET EDT=Y
        SET X="T-5D" DO ^%DT SET SDT=Y
        NEW TEMPARR
        NEW TMGDUZ SET TMGDUZ=168
        DO GETAUDT1(.TEMPARR,TMGDUZ,SDT,EDT,2) ;"GET AUDIT LIST, PER USER
        IF $DATA(TEMPARR) DO ZWRITE^TMGZWR("TEMPARR")
        QUIT
        ; 
TEST6(MODE,TMGDFN)   ;
        NEW SDT,EDT,%DT,X,Y
        SET %DT=""
        SET X="T" DO ^%DT SET EDT=Y
        SET X="T-5D" DO ^%DT SET SDT=Y
        NEW TEMPARR
        IF $DATA(MODE)'>0 NEW MODE SET MODE=1
        IF $DATA(TMGDFN)'>0 NEW TMGDFN SET TMGDFN=69928
        DO GETAUDT2(.TEMPARR,TMGDFN,MODE,SDT,EDT) ;"GET AUDIT LIST, PER USER
        IF $DATA(TEMPARR) DO ZWRITE^TMGZWR("TEMPARR")
        QUIT
