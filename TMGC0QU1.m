TMGC0QU1 ;TMG/kst/TMG C0Q Utility code ;5/12/14
         ;;1.0;TMG-LIB;**1**;5/12/14
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
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ; 
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"=======================================================================
 ;"  
 ;"=======================================================================
 ;"FIX!
 ;"This code effected output of records like this:
 ;" kst-LAUGHLIN-2014-EP-BMI_Age_18+_Obese
 ;" instead of:
 ;" kst-LAUGHLIN-MU14-EP-BMI_Age_18+_Obese 
 ;"I don't have time to fix right now, but will need to be fixed next year.  
 ;"  The reporting looks for the MUxx pattern for some reason.
 ;
COPYMS  ;"COPY C0Q MEASUREMENT SET.
        NEW X,Y,DIC
        SET DIC="1130580001.201",DIC(0)="MAEQ"
        WRITE !,"Utility for copying a C0Q measurement set.",!,!
        DO ^DIC WRITE !
        IF +Y'>0 DO  GOTO CPMSDN
        . WRITE "None selected, so aborting.",!
        WRITE "Enter year (YYYY) for new C0Q measurement set: "
        NEW YR READ YR:$GET(DTIME,3600),!
        IF YR="^" GOTO CPMSDN
        IF +YR'>2010 DO  GOTO CPMSDN
        . WRITE "Year should be numeric, and > 2010.  Aborting.",!
        WRITE "Enter initials for provider (e.g. kst) for new set: "
        NEW PROVINIT READ PROVINIT:$GET(DTIME,3600),!
        SET PROVINIT=$$TRIM^XLFSTR(PROVINIT)
        IF PROVINIT="^" GOTO CPMSDN
        IF PROVINIT="" DO  GOTO CPMSDN
        . WRITE "Provider initials not entered.  Aborting.",!
        NEW SITE SET SITE="LAUGHLIN"  ;"<-- hard coded site
        NEW RESULT SET RESULT=$$COPY1MS(+Y,YR,PROVINIT,SITE)
        IF RESULT<0 DO
        . WRITE "ERROR:",!
        . WRITE "  ",$PIECE(RESULT,"^",2,99),!
CPMSDN  QUIT
        ;
COPY1MS(SRCIEN201,YR,PROVINIT,SITE) ;"COPY 1 MEASURE SET
        ;"Input: SRCIEN201 -- source record from 1130580001.201 (C0Q MEASUREMENT SET)
        ;"       YR -- Year for new set
        ;"       PROVINIT -- Initials for provider, e.g. 'kst'
        ;"       SITE -- site.  E.g. 'LAUGHLIN'
        ;"Result: 1 if OK, or -1^Message
        NEW TMGFDA,TMGIEN,TMGMSG
        NEW TMGRESULT SET TMGRESULT=1
        NEW DT,%DT,X,Y,SDT,EDT
        SET %DT=""
        SET X="1/1/"_YR DO ^%DT SET SDT=Y
        IF +SDT'>0 DO  GOTO CP1MSDN
        . SET TMGRESULT="-1^Unable to convert date: "_X        
        SET X="12/31/"_YR DO ^%DT SET EDT=Y
        IF +EDT'>0 DO  GOTO CP1MSDN
        . SET TMGRESULT="-1^Unable to convert date: "_X        
        NEW KEY SET KEY="MU"_$EXTRACT(YR,3,4)        
        NEW NAME SET NAME=PROVINIT_" "_KEY_" Measure set"
        SET TMGFDA(1130580001.201,"+1,",.01)=NAME
        SET TMGFDA(1130580001.201,"+1,",.02)=SDT
        SET TMGFDA(1130580001.201,"+1,",.03)=EDT
        SET TMGFDA(1130580001.201,"+1,",.3)=KEY
        DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO CP1MSDN
        . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        ;"Now copy the contained measures from source record
        NEW NEWIEN201 SET NEWIEN201=+$GET(TMGIEN(1)) 
        IF NEWIEN201'>0 DO  GOTO CP1MSDN
        . SET TMGRESULT="-1^Unable to determine IEN of added record in C0Q MEASUREMENT SET"        
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(^C0Q(201,SRCIEN201,5,IDX)) QUIT:(+IDX'>0)!(+TMGRESULT<0)  DO
        . NEW QMIEN SET QMIEN=+$PIECE($GET(^C0Q(201,SRCIEN201,5,IDX,0)),"^",1) ;"pointer to '101 file
        . NEW TMGRESULT SET TMGRESULT=$$CPY1QM(QMIEN,KEY,PROVINIT,SITE)
        . IF +TMGRESULT'>0 QUIT
        . KILL TMGFDA,TMGIEN,TMGMSG
        . NEW IENS SET IENS="+1,"_NEWIEN201_","
        . SET TMGFDA(1130580001.2011,IENS,.01)=+TMGRESULT
        . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) DO  QUIT
        . . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        IF TMGRESULT'<0 SET TMGRESULT=1       
CP1MSDN QUIT TMGRESULT        
        ;        
CPY1QM(IEN101,KEY,PROVINIT,SITE) ;"Copy 1 C0Q QUALITY MEASURE
        ;"Input: IEN101 -- source record from 1130580001.101 (C0Q QUALITY MEASURE)
        ;"       KEY -- e.g. 'MU14'
        ;"       PROVINIT -- Initials for provider, e.g. 'kst'
        ;"       SITE -- site.  E.g. 'LAUGHLIN'
        ;"Result: IEN in 1130580001.101 (C0Q QUALITY MEASURE) of new record,
        ;"        or 0 if skipping this one.
        ;"        or -1^Error if problem
        NEW TMGOUT,TMGMSG
        NEW TMGRESULT SET TMGRESULT=-1
        NEW FILE SET FILE=1130580001.101
        NEW IENS SET IENS=IEN101_","
        DO GETS^DIQ(FILE,IENS,"**","IE","TMGOUT","TMGMSG")
        ;"NOTE: This code will only copy over the ALTERNATIVE LISTS (fields, 1.1, 2.1) 
        ;"      NOT the other lists 1, 2.  Could implement later. 
        IF $DATA(TMGMSG("DIERR")) DO  GOTO CQMDN
        . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        NEW NAME SET NAME=$$GETVAL(.TMGOUT,FILE,IENS,".01","E",.TMGRESULT) GOTO:(+TMGRESULT'>0) CQMDN
        NEW NEWNAME SET NEWNAME=NAME
        SET $PIECE(NEWNAME," ",1)=PROVINIT
        SET $PIECE(NEWNAME," ",2)=KEY
        WRITE "Copy patient list (^ to abort):",!
        WRITE "  ",NAME," --> ",NEWNAME
        NEW % SET %=1 DO YN^DICN WRITE !
        IF %=-1 SET TMGRESULT="-1^User aborted" GOTO CQMDN
        IF %=2 SET TMGRESULT=0 GOTO CQMDN        
        NEW IEN301,NUMIEN301,DENOMIEN301
        SET IEN301=$$GETVAL(.TMGOUT,FILE,IENS,1.1,"I",.TMGRESULT) GOTO:(+TMGRESULT'>0) CQMDN
        SET NUMIEN301=$$CPY1PL(IEN301,YR,PROVINIT,SITE)
        IF NUMIEN301'>0 DO  GOTO CQMDN
        . SET TMGRESULT=NUMIEN301
        SET IEN301=$$GETVAL(.TMGOUT,FILE,IENS,2.1,"I",.TMGRESULT) GOTO:(+TMGRESULT'>0) CQMDN
        SET DENOMIEN301=$$CPY1PL(IEN301,KEY,PROVINIT,SITE)
        IF DENOMIEN301'>0 DO  GOTO CQMDN
        . SET TMGRESULT=DENOMIEN301
        NEW DISPNAME SET DISPNAME=$$GETVAL(.TMGOUT,FILE,IENS,".7","E",.TMGRESULT) GOTO:(+TMGRESULT'>0) CQMDN
        NEW TMGFDA,TMGIEN KILL TMGMSG
        SET IENS="+1,"
        SET TMGFDA(FILE,IENS,.01)=NEWNAME
        SET TMGFDA(FILE,IENS,.3)=KEY
        SET TMGFDA(FILE,IENS,.7)=DISPNAME
        SET TMGFDA(FILE,IENS,1.1)=NUMIEN301
        SET TMGFDA(FILE,IENS,2.1)=DENOMIEN301
        DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO CP1MSDN
        . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)        
        SET TMGRESULT=+$GET(TMGIEN(1))
        IF TMGRESULT'>0 DO  GOTO CQMDN
        . SET TMGRESULT="-1^Unable to determine IEN of added record in C0Q QUALITY MEASURE"
        ;        
CQMDN   QUIT TMGRESULT  
        ;
GETVAL(ARR,FILE,IENS,FLD,IE,TMGRESULT) ;
        NEW LOCRESULT SET LOCRESULT=$GET(ARR(FILE,IENS,FLD,IE))
        IF LOCRESULT'="" SET TMGRESULT=1
        ELSE  SET TMGRESULT="-1^Unable to value for field "_FLD_" in record "_IENS_" in file "_FILE
        QUIT LOCRESULT
        ;
CPY1PL(IEN301,KEY,PROVINIT,SITE) ;"COPY 1 PATIENT LIST
        ;"Input: IEN301 -- source record from 1130580001.301 (C0Q PATIENT LIST)
        ;"       KEY -- e.g. 'MU14'
        ;"       PROVINIT -- Initials for provider, e.g. 'kst'
        ;"       SITE -- site.  E.g. 'LAUGHLIN'
        ;"Result: IEN in 1130580001.301 (C0Q PATIENT LIST) of new record, or -1^Error if problem
        NEW TMGRESULT SET TMGRESULT=-1
        NEW TMGOUT,TMGMSG
        NEW FILE SET FILE=1130580001.301
        NEW IENS SET IENS=IEN301_","
        NEW NAME SET NAME=$$GET1^DIQ(FILE,IENS,.01)        
        NEW NEWNAME SET NEWNAME=NAME
        SET $PIECE(NEWNAME,"-",1)=PROVINIT        
        SET $PIECE(NEWNAME,"-",2)=SITE
        SET $PIECE(NEWNAME,"-",3)=KEY                
        NEW TMGFDA,TMGIEN KILL TMGMSG
        SET IENS="+1,"
        SET TMGFDA(FILE,IENS,.01)=NEWNAME
        SET TMGFDA(FILE,IENS,.3)=KEY
        SET TMGFDA(FILE,IENS,999)=NEWNAME
        DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO CPPLDN
        . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)        
        SET TMGRESULT=+$GET(TMGIEN(1))
        IF TMGRESULT'>0 DO  GOTO CQMDN
        . SET TMGRESULT="-1^Unable to determine IEN of added record in C0Q PATIENT LIST"
        ;
CPPLDN  QUIT TMGRESULT
        ;
FILEPL(ARR) ;"File C0Q PATIENT LISTS, if it doesn't already exist.
        ;"Input: ARR -- this should be the C0QLIST, as made by CUSTEP^TMGC0Q07
        ;"         Format ARR(IEN200,CategoryName,DFN)=""
        ;"Result: 1, or -1^message if error.
        NEW TMGDEBUG SET TMGDEBUG=0
        IF TMGDEBUG=1 DO
        . MERGE ARR=^TMG("TMP","FILEPL^TMGC0QU1")
        ELSE  DO
        . KILL ^TMG("TMP","FILEPL^TMGC0QU1")
        . MERGE ^TMG("TMP","FILEPL^TMGC0QU1")=ARR
        NEW RESULT SET RESULT=1
        NEW IEN200 SET IEN200=""
        FOR  SET IEN200=$ORDER(ARR(IEN200)) QUIT:(+IEN200'>0)!(+RESULT<0)  DO
        . NEW INIT SET INIT=$PIECE($GET(^VA(200,IEN200,0)),"^",2)
        . IF INIT="" DO  QUIT
        . . SET RESULT="-1^Unable to find initials for provider IEN# "_IEN200
        . NEW CATNAME SET CATNAME=""
        . FOR  SET CATNAME=$ORDER(ARR(IEN200,CATNAME)) QUIT:(CATNAME="")!(+RESULT<0)  DO
        . . NEW RECNAME SET RECNAME=INIT_"-"_CATNAME
        . . SET RESULT=$$ENSUR1PL(RECNAME)
        QUIT (RESULT>0)
        ;
ENSUR1PL(RECNAME) ;"Ensure record exists.
        ;"Results: IEN of rec, if record exists (or was added), or -1^Error Message
        NEW TMGRESULT SET TMGRESULT=1
        NEW FNUM SET FNUM=$$C0QPLFN^C0QMU12()
        DO FIND^DIC(FNUM,,"@;.01","X",RECNAME,"*","B","","","TMGOUT","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO EN1DN
        . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)        
        IF +$GET(TMGOUT("DILIST",0))>0 DO  GOTO EN1DN
        . SET TMGRESULT=$$DELXTRAS($NAME(TMGOUT("DILIST",2)))
        SET TMGRESULT=$$ADD1PL(RECNAME)
EN1DN   QUIT TMGRESULT
        ;
ADD1PL(RECNAME) ;
        NEW TMGRESULT
        NEW FNUM SET FNUM=$$C0QPLFN^C0QMU12()
        NEW MUKEY SET MUKEY=$PIECE(RECNAME,"-",3)
        NEW YR SET YR="20"_$EXTRACT(MUKEY,3,4)
        NEW TMGFDA,TMGIEN,TMGMSG
        SET TMGFDA(FNUM,"+1,",.01)=RECNAME
        SET TMGFDA(FNUM,"+1,",.02)=YR
        SET TMGFDA(FNUM,"+1,",.04)="NOW"
        SET TMGFDA(FNUM,"+1,",.07)="`"_DUZ
        SET TMGFDA(FNUM,"+1,",.3)=MUKEY
        DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO EN1DN
        . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)        
        SET TMGRESULT=+$GET(TMGIEN(1))
        WRITE "CREATED ",RECNAME,": #",TMGRESULT,!
        QUIT TMGRESULT
        ;        
DELXTRAS(PARR) ;"Delete extra (duplicate) records in Patient List file
        ;"Purpose, delete all but the #1 index IEN
        ;"Input: ARR -- PASS BY NAME.  Format: @PARR@(#)=IEN
        NEW FNUM SET FNUM=$$C0QPLFN^C0QMU12()
        NEW IDX SET IDX=""
        FOR  SET IDX=$ORDER(@PARR@(IDX),-1) QUIT:(+IDX'>1)  DO
        . NEW DIK SET DIK="^C0Q(301,"
        . NEW DA SET DA=+$GET(@PARR@(IDX)) QUIT:DA'>0
        . DO ^DIK
        NEW TMGRESULT SET TMGRESULT=+$GET(@PARR@(1))
        QUIT TMGRESULT
        
