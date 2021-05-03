TMGC0Q07 ;TMG/kst/REPORTS from C0Q-based code  10/2/12, 2/2/14, 3/24/21
         ;;1.0;TMG-LIB;**1**;8/17/12
 ;
 ;"TMG C0Q FUNCTIONS
 ;"NOTE: Report card code.
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
 ;"PRINTOUT --Interactive menu to print out report cards
 ;"KTWKRPT -- Do weekly report for K. TOPPENBERG
 ;"KTMNRPT -- Do monthly report for K. TOPPENBERG
 ;"DTWKRPT -- Do weekly report for D. TOPPENBERG
 ;"DTMNRPT -- Do monthly report for D. TOPPENBERG
 ;"WKRPT(PROVIEN) --WEEKLY REPORT (T-7 to NOW)
 ;"MNTHRPT(PROVIEN) --MONTHLY REPORT (T-30 to NOW)
 ;"ALLRPT(PROVIEN) --Full Period REPORT (T-30 to NOW)
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"GETDTS(SDATE,EDATE) --Get custom date range.    
 ;"REPORT(PROVIEN,CUSTSDT,CUSTEDT,RPTDESCR,SHOWFAIL) ; Purpose: generate a report for provider for range, output to current device
 ;"CUSTEP(PERIODKEY,CUSTSDT,CUSTEDT) --Custom entry point for EP computations
 ;"OUTRPT(PROVIEN,TITLE,MEASUREIEN,C0QLIST) -- output report for provider, to the current output device
 ;"PRNTITEM(PROVIEN,MEASUREIEN,ITEMIEN,C0QLIST) -- print 1 given item.
 ;"GETLIST(PROVIEN,LSTNAME,C0QLIST,OUTLIST) -- Return names list
 ;"NEGLIST(LIST,NEGLIST)  -- removes all entries from NEGLIST out of LIST
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"=======================================================================
 ;"=======================================================================
 ;
PRINTOUT ;
        NEW MENU,MENU2,USRPICK,%ZIS,PROVIEN,RPTPICK
        NEW SDATE,EDATE,SDTSTR,EDTSTR
        NEW PERIODKEY SET PERIODKEY=$$GETKEY^TMGC0Q01
        NEW SHOWFAIL SET SHOWFAIL=0
        SET MENU(0)="Pick Provider For Meaningful Use Report"
        SET MENU(1)="Kevin Toppenberg"_$CHAR(9)_"168^Kevin Toppenberg"
        SET MENU(2)="Marcia Dee Toppenberg"_$CHAR(9)_"83^Marcia Dee Toppenberg"
        SET MENU2(1)="Weekly report"_$CHAR(9)_"WEEK"
        SET MENU2(2)="Monthly report"_$CHAR(9)_"MONTH"
        SET MENU2(3)="Period report"_$CHAR(9)_"PERIOD"
        SET MENU2(4)="Custom date range"_$CHAR(9)_"CUSTOM"
        SET MENU2(5)="<placeholder>"
        SET MENU2(6)="Run tests on just ONE (1) patient"_$CHAR(9)_"TEST_ONE" 
POM     WRITE !
        SET USRPICK=$$MENU^TMGUSRI2(.MENU)
        IF USRPICK="^" SET USRPICK=""
        IF USRPICK="" GOTO PODN
        SET PROVIEN=$PIECE(USRPICK,"^",1)
        SET MENU2(0)="Pick Type of Report for "_$PIECE(USRPICK,"^",2)        
POM2    WRITE !
        SET MENU2(5)="Turn display of failure detail "_$SELECT(SHOWFAIL:"OFF",1:"ON")_$CHAR(9)_"TOGGLE_FAIL" 
        SET RPTPICK=$$MENU^TMGUSRI2(.MENU2,"")
        IF RPTPICK="^" GOTO PODN
        IF RPTPICK="" GOTO POM
        IF RPTPICK="TOGGLE_FAIL" SET SHOWFAIL='SHOWFAIL GOTO POM2
        IF RPTPICK="TEST_ONE" DO  GOTO POM2
        . DO TEST1(PROVIEN)
        IF RPTPICK="CUSTOM" DO  GOTO:(+SDATE'>0)!(+EDATE'>0) POM2
        . DO GETDTS(.SDATE,.EDATE) 
        SET %ZIS("A")="Enter Output Device: "
        SET %ZIS("B")="HOME"
        NEW INITIO SET INITIO=IO
        DO ^%ZIS  ;"standard device call
        NEW TOHOME SET TOHOME=(IO=INITIO)
        IF POP GOTO POM
        USE IO
        IF RPTPICK="WEEK" DO
        . DO WKRPT(PROVIEN,SHOWFAIL,PERIODKEY,TOHOME) 
        ELSE  IF RPTPICK="MONTH" DO
        . DO MNTHRPT(PROVIEN,SHOWFAIL,PERIODKEY,TOHOME) 
        ELSE  IF RPTPICK="PERIOD" DO
        . DO ALLRPT(PROVIEN,SHOWFAIL,PERIODKEY,TOHOME)
        ELSE  IF RPTPICK="CUSTOM" DO
        . DO REPORT(PROVIEN,SDATE,EDATE,"Custom date range ",SHOWFAIL,PERIODKEY,TOHOME)        
        DO ^%ZISC  ;" Close the output device        
        GOTO POM2
PODN    QUIT
        ;
GETDTS(SDATE,EDATE) ;"Get custom date range.    
        NEW %DT,X,Y
        SET (SDATE,EDATE)=0
        SET %DT="AE"
        SET %DT("A")="Date range START DATE: "
        DO ^%DT
        SET SDATE=Y QUIT:(Y'>0)
        SET %DT("A")="Date range END DATE: "
        DO ^%DT
        SET EDATE=Y
        QUIT
        ;
KTWKRPT DO WKRPT(168,0,"LAUGHLIN-MU13") QUIT    
KTMNRPT DO MNTHRPT(168,0,"LAUGHLIN-MU13") QUIT  
DTWKRPT DO WKRPT(83,0,"LAUGHLIN-MU13") QUIT     
DTMNRPT DO MNTHRPT(83,0,"LAUGHLIN-MU13") QUIT   
        ;
WKRPT(PROVIEN,SHOWFAIL,PERIODKEY,VERBOSE) ;"WEEKLY REPORT (T-7 to NOW)
        DO REPORT(PROVIEN,"T-7","NOW","Last Week",.SHOWFAIL,.PERIODKEY,.VERBOSE) ;
        QUIT
        ;
MNTHRPT(PROVIEN,SHOWFAIL,PERIODKEY,VERBOSE) ;"MONTHLY REPORT (T-30 to NOW)
        DO REPORT(PROVIEN,"T-30","NOW","Last 30 days",.SHOWFAIL,.PERIODKEY,.VERBOSE) ;
        QUIT
        ;
ALLRPT(PROVIEN,SHOWFAIL,PERIODKEY,VERBOSE) ;"Full Period REPORT (T-30 to NOW)
        NEW TEMPARR DO INIT^TMGC0Q01(PERIODKEY,"TEMPARR","EP")
        NEW SDT SET SDT=$GET(TEMPARR(1,"EPBeginDate"))
        NEW EDT SET EDT=$GET(TEMPARR(1,"EPEndDate"))
        DO REPORT(PROVIEN,SDT,EDT,"Period to date",.SHOWFAIL,.PERIODKEY,.VERBOSE) ;
        QUIT
        ;
REPORT(PROVIEN,CUSTSDT,CUSTEDT,RPTDESCR,SHOWFAIL,PERIODKEY,VERBOSE) ;
        ;"Purpose: generate a report for provider for range, output to current device
        ;"Input: PROVIEN -- IEN of provider to run report for
        ;"       CUSTSDT -- Start date.  Either FM format, or T-7 etc. 
        ;"       CUSTEDT -- End date.  Either FM format, or T-7 etc.
        ;"       RPTDESCR -- E.g. "Weekly", or "Monthly"
        ;"       SHOWFAIL -- OPTIONAL.  If 1 then list of failures 
        ;"       PERIODKEY -- REQUIRED.  E.G. "LAUGHLIN-MU13"
        ;"       VERBOSE -- OPTIONAL.  1 for TRUE, 0 for FALSE.  Should only be 1 IF output to screen (HOME) device
        ;"NOTE: Assumes that desired output SET is defined in file C0Q MEASUREMENT SET
        ;"      (#1130580001.201), and that the desired SET begins with providers
        ;"      initials, as defined in the NEW PERSON file, and that only ONE
        ;"      record begins with these initials.
        ;"Results: NONE
        NEW TMGLIMITPROV SET TMGLIMITPROV(PROVIEN)=1
        ;"NEW PERIODKEY SET PERIODKEY="LAUGHLIN-MU12" 
        SET PERIODKEY=$GET(PERIODKEY)
        IF PERIODKEY="" DO  GOTO RDN
        . WRITE "ERROR: no PERIOD KEY provided.",!
        SET VERBOSE=+$GET(VERBOSE)
        NEW JUSTKEY SET JUSTKEY=$PIECE(PERIODKEY,"-",2)
        IF JUSTKEY="" DO  GOTO RDN
        . WRITE "ERROR: Unable to determine period, e.g. 'MU13', from '",PERIODKEY,"'",!
        NEW C0QLIST
RPT1    IF VERBOSE WRITE "GENERATING REPORT..."
        DO CUSTEP(PERIODKEY,.CUSTSDT,.CUSTEDT,VERBOSE) ;"//fills C0QLIST
        NEW % SET %=1
        WRITE !,"File findings to Fileman records"
        ;"DO YN^DICN WRITE !
        IF %=-1 GOTO RDN
        IF %'=1 GOTO RPT2 
        NEW FILERESULT SET FILERESULT=$$FILEPL^TMGC0QU1(.C0QLIST)
        IF +FILERESULT<0 DO  GOTO RDN
        . WRITE !,"ERROR: ",$PIECE(FILERESULT,"^",2),!
RPT2    WRITE !
        NEW DIC,X,Y
        NEW RESULT SET RESULT=1
        SET X=$$GET1^DIQ(200,PROVIEN,1)
        IF X="" DO  GOTO RDN
        . WRITE "ERROR: Can't find initials in file 200 for provider #"_PROVIEN,!
        SET DIC=$$C0QMFN^C0QMAIN(),DIC(0)="M"
        SET X=X_" "_JUSTKEY
        DO ^DIC
        IF +Y'>0 DO  GOTO ORPDN
        . WRITE "ERROR: Unable to find unique record starting with name: "_X,!
        NEW MEASUREIEN SET MEASUREIEN=+Y        
        ;"DO EN3^C0QMAIN(MEASUREIEN)  ;"Menu option "COPY PATIENT LISTS"
        NEW TITLE SET TITLE="MEANINGFUL USE REPORT CARD: "_RPTDESCR_"  ["
        SET Y=CUSTSDT DO DD^%DT SET TITLE=TITLE_Y_" - "
        SET Y=CUSTEDT DO DD^%DT SET TITLE=TITLE_Y_"]"
        DO OUTRPT(PROVIEN,TITLE,MEASUREIEN,.C0QLIST,.SHOWFAIL)
RDN     QUIT
        ;
CUSTEP(PERIODKEY,CUSTSDT,CUSTEDT,TMGVERBOSE) ;"Custom entry point for EP computations
        ;"NOTE: C0QLIST is modified in L1^TMGC0Q01, in global scope
        IF +CUSTSDT'=CUSTSDT DO
        . NEW X,Y,%DT
        . SET X=CUSTSDT DO ^%DT SET CUSTSDT=Y
        IF +CUSTEDT'=CUSTEDT DO
        . NEW X,Y,%DT
        . SET X=CUSTEDT DO ^%DT SET CUSTEDT=Y
        NEW TMGSKIPC0QFILE SET TMGSKIPC0QFILE=1
        SET TMGVERBOSE=+$GET(TMGVERBOSE)
        GOTO L1^TMGC0Q01  ;"Quit occurs from there, effecting QUIT from this routine
        ;
OUTRPT(PROVIEN,TITLE,MEASUREIEN,C0QLIST,SHOWFAIL) ;
        ;"Purpose: output report for provider, to the current outptu device
        ;"Input: PROVIEN -- IEN of provider to run report for;" 
        ;"       TITLE -- Title for the report
        ;"       MEASUREIEN -- IEN in FILE: C0Q MEASUREMENT (# $$C0QMFN^C0QMAIN)
        ;"       C0QLIST -- PASS BY REFERENCE.  Array with all lists, as created in DOEP^TMGC0Q01
        ;"       SHOWFAIL -- OPTIONAL.  If 1 then list of failures 
        ;"Results: NONE
        WRITE !,"Please deliver this report to: ",$$GET1^DIQ(200,PROVIEN,.01),!
        WRITE TITLE,!
        WRITE "===================================================",!
        NEW SHOWNIEN,ITEMIEN
        NEW ORDERSEQ SET ORDERSEQ=0
        FOR  SET ORDERSEQ=$ORDER(^C0Q(201,MEASUREIEN,5,"ADISP",ORDERSEQ)) QUIT:(+ORDERSEQ'>0)  DO
        . SET ITEMIEN=+$ORDER(^C0Q(201,MEASUREIEN,5,"ADISP",ORDERSEQ,""))
        . QUIT:ITEMIEN'>0
        . SET SHOWNIEN(ITEMIEN)=""
        . DO PRNTITEM(PROVIEN,MEASUREIEN,ITEMIEN,.C0QLIST,.SHOWFAIL)        
        SET ITEMIEN=0
        FOR  SET ITEMIEN=$ORDER(^C0Q(201,MEASUREIEN,5,ITEMIEN)) QUIT:+ITEMIEN'>0  DO
        . QUIT:$DATA(SHOWNIEN(ITEMIEN))  ;"skip IF already shown above. 
        . DO PRNTITEM(PROVIEN,MEASUREIEN,ITEMIEN,.C0QLIST,.SHOWFAIL)        
ORPDN   QUIT   
        ;
PRNTITEM(PROVIEN,MEASUREIEN,ITEMIEN,C0QLIST,SHOWFAIL) ;
        ;"Input: PROVIEN -- IEN of provider to run report for
        ;"       MEASUREIEN -- IEN in FILE: C0Q MEASUREMENT (# $$C0QMFN^C0QMAIN)
        ;"       ITEMIEN -- IEN IF particular MEASURE subfile entry inside C0Q MEASUREMENT file 
        ;"       C0QLIST -- PASS BY REFERENCE.  Array with all lists, as created in DOEP^TMGC0Q01
        ;"       SHOWFAIL -- OPTIONAL.  If 1 then list of failures 
        ;
        NEW LISTARR MERGE LISTARR=^TMG("TMP","TMGC0Q01","C0QLIST")
        NEW QMPTR  ;"POINTER TO FILE: C0Q QUALITY MEASURE  (# $$C0QQFN^C0QMAIN() )
        SET QMPTR=$PIECE($GET(^C0Q(201,MEASUREIEN,5,ITEMIEN,0)),"^",1)
        ;"Print out information about MEASURE name
        NEW FNUM SET FNUM=$$C0QQFN^C0QMAIN()
        NEW NAME SET NAME=$$GET1^DIQ(FNUM,QMPTR,.05)  ;"TITLE
        IF NAME="" SET NAME=$$GET1^DIQ(FNUM,QMPTR,.01) ;"NAME
        WRITE NAME," =================",!
        NEW NUMRLISTNAME,DENOMLISTNAME SET (NUMRLISTNAME,DENOMLISTNAME)=""
        NEW NEGNUMRLISTNAME,NEGDENOMLISTNAME SET (NEGNUMRLISTNAME,NEGDENOMLISTNAME)=""
        SET P=$$GET1^DIQ(FNUM,QMPTR,1,"I") ;"Ptr to Numerator Patient List
        IF P>0 DO  GOTO PI5
        . SET NUMRLISTNAME=$$GET1^DIQ(810.5,P,.01)
        . WRITE " NUM List: ",NUMRLISTNAME,!
        SET P=$$GET1^DIQ(FNUM,QMPTR,1.1,"I") ;"Ptr to Alternative Numerator Patient List
        IF P>0 DO  GOTO PI5
        . SET NUMRLISTNAME=$$GET1^DIQ($$C0QPTLFN^C0QMAIN(),P,.01)
        . WRITE " NUM List: ",NUMRLISTNAME,!
PI5     SET P=$$GET1^DIQ(FNUM,QMPTR,1.5,"I") ;"Ptr to Negative Numerator Patient List
        IF P>0 DO  GOTO PI10
        . SET NEGNUMRLISTNAME=$$GET1^DIQ(810.5,P,.01)
        . WRITE " Negative NUM List: ",NEGNUMRLISTNAME,!
        SET P=$$GET1^DIQ(FNUM,QMPTR,1.51,"I") ;"Ptr to Alternative Negative Numerator Patient List
        IF P>0 DO  GOTO PI10
        . SET NEGNUMRLISTNAME=$$GET1^DIQ($$C0QPTLFN^C0QMAIN(),P,.01)
        . WRITE " Negative NUM List: ",NEGNUMRLISTNAME,!
PI10    SET P=$$GET1^DIQ(FNUM,QMPTR,2,"I") ;"Ptr to Denominator list
        IF P>0 DO  GOTO PI15
        . SET DENOMLISTNAME=$$GET1^DIQ(810.5,P,.01)
        . WRITE " DENOM List: ",DENOMLISTNAME,!
        SET P=$$GET1^DIQ(FNUM,QMPTR,2.1,"I") ;"Ptr to Alternative Denominator list
        IF P>0 DO  GOTO PI15
        . SET DENOMLISTNAME=$$GET1^DIQ($$C0QPTLFN^C0QMAIN(),P,.01)
        . WRITE " DENOM List: ",DENOMLISTNAME,!        
PI15    ;"NEW NUMR SET NUMR=$PIECE($GET(^C0Q(201,MEASUREIEN,5,ITEMIEN,2)),"^",1)
        ;"NEW DENOM SET DENOM=$PIECE($GET(^C0Q(201,MEASUREIEN,5,ITEMIEN,4)),"^",1)
        NEW NUMRARRAY,DENOMARRAY
        NEW NUMR SET NUMR=$$GETLIST(PROVIEN,NUMRLISTNAME,.C0QLIST,.NUMRARRAY)
        NEW FILENUM SET FILENUM=$GET(C0QLIST("FILE",NUMRLISTNAME),2) ;"Default is PATIENT file (2)
        IF NEGNUMRLISTNAME'="" DO
        . NEW NEGNUMRARRAY
        . NEW NEGNUMR SET NEGNUMR=$$GETLIST(PROVIEN,NEGNUMRLISTNAME,.C0QLIST,.NEGNUMRARRAY)
        . SET NUMR=$$NEGLIST(.NUMRARRAY,.NEGNUMRARRAY)
        NEW DENOM SET DENOM=$$GETLIST(PROVIEN,DENOMLISTNAME,.C0QLIST,.DENOMARRAY)
        NEW PCT
        IF DENOM>0 SET PCT=((NUMR/DENOM)*100)\1
        ELSE  SET PCT=0
        ;"Later I could print out patients that failed list....
        WRITE " ",NUMR," / ",DENOM," = ",PCT,"%",!
        IF $GET(SHOWFAIL)'=1 GOTO PIDN
        NEW SOMEFOUND SET SOMEFOUND=0
        WRITE " -- Missing Entries (not in numerator list) -- ",!
        NEW IEN SET IEN=0
        FOR  SET IEN=$ORDER(DENOMARRAY(IEN)) QUIT:(+IEN'>0)  DO
        . IF $DATA(NUMRARRAY(IEN)) QUIT
        . IF FILENUM=8925 DO
        . . WRITE "  #",IEN,": ",$$GET1^DIQ(FILENUM,IEN_",",.01),"; ",$$GET1^DIQ(FILENUM,IEN_",",.02),"; ",$$GET1^DIQ(FILENUM,IEN_",",.07),!
        . ELSE  DO
        . . WRITE "  ",$$GET1^DIQ(FILENUM,IEN_",",.01),!
        . SET SOMEFOUND=1
        IF SOMEFOUND=0 WRITE "  (none)",!
        WRITE !
        ;
PIDN    QUIT
        ;
GETLIST(PROVIEN,LSTNAME,C0QLIST,OUTLIST) ;
        ;"Purpose: Return names list
        ;"Input: PROVIEN -- IEN of provider to run report for 
        ;"       LSTNAME -- Name of list to get
        ;"       C0QLIST -- PASS BY REFERENCE.  Array with all lists, as created in DOEP^TMGC0Q01
        ;"       OUTLIST -- PASS BY REFERENCE. An OUT PARAMETER.
        ;"Output: OUTLIST is filled.
        ;"Results: Count of list, or -1 IF list not found
        NEW RESULT SET RESULT=-1
        SET LSTNAME=$PIECE(LSTNAME,"-",2,99)
        KILL OUTLIST
        MERGE OUTLIST=C0QLIST(PROVIEN,LSTNAME)
        SET RESULT=$$LISTCT^TMGMISC2("OUTLIST")
        QUIT RESULT
        ;
NEGLIST(LIST,NEGLIST)  ;
        ;"Purpose: removes all entries from NEGLIST out of LIST
        ;"Input: LIST -- PASS BY REFERENCE.  WILL BE ALTERED
        ;"       NEGLIST -- PASS BY REFERENCE        
        ;"Returns count of LIST
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(NEGLIST(IDX)) QUIT:IDX=""  DO
        . KILL LIST(IDX)
        NEW RESULT SET RESULT=$$LISTCT^TMGMISC2("LIST")        
        QUIT RESULT
        ;
TEST1(PROVIEN)  ;
        ;"Purpose: to run tests on 1 patient (selected by user)
        NEW PERIODKEY SET PERIODKEY="LAUGHLIN-MU12" 
        NEW ZYR SET ZYR=PERIODKEY_"-"
        NEW SDATE,EDATE,DIC,X,Y,C0QLIST
        SET DIC=2,DIC(0)="MAEQ"
        DO ^DIC WRITE !
        IF +Y'>0 GOTO T1DN
        NEW TMGDFN SET TMGDFN=+Y
        DO GETDTS(.SDATE,.EDATE) WRITE !
        IF (+SDATE'>0)!(+EDATE'>0) GOTO T1DN
        NEW RESULT,WHY
        NEW VISITS  ;"leave empty for now. --> no
        DO GET1VSTS(TMGDFN,SDATE,EDATE,.VISITS)
        DO TESTPT^TMGC0Q02(TMGDFN,PROVIEN,SDATE,EDATE,.C0QLIST,.VISITS,.RESULT,.WHY)  ;"Run various tests on one patient.
        WRITE "RESULT=",RESULT,!
        WRITE "WHY=",WHY,!
        DO PRESS2GO^TMGUSRI2
T1DN    QUIT       
        ;"
GET1VSTS(TMGDFN,SDATE,EDATE,VISITS) ;"Get list of visits for 1 patient, in date range
        NEW DOC SET DOC=""
        FOR  SET DOC=$ORDER(^TIU(8925,"AA",TMGDFN,DOC)) QUIT:DOC=""  DO
        . NEW RDT SET RDT=0
        . FOR  SET RDT=$ORDER(^TIU(8925,"AA",TMGDFN,DOC,RDT)) QUIT:RDT=""  DO
        . . NEW DT SET DT=9999999-RDT
        . . IF (DT<SDATE)!(DT>EDATE) QUIT
        . . SET VISITS(DT)=""
        QUIT
HTNRPT ;
        ;"Purpose:  Printout list of patients with HTN
        NEW TMGDFN SET TMGDFN=0
        NEW HASHTN,SDATE,EDATE
        DO GETDTS^TMGC0Q07(.SDATE,.EDATE)
        NEW NAME,HTNARRAY,DOB
        SET %ZIS("A")="Enter Output Device: "
        SET %ZIS("B")="HOME"
        NEW INITIO SET INITIO=IO
        DO ^%ZIS  ;"standard device call
        NEW TOHOME SET TOHOME=(IO=INITIO)
        IF POP GOTO DNHTN
        USE IO
        FOR  SET TMGDFN=$ORDER(^DPT(TMGDFN)) QUIT:TMGDFN'>0  DO
        . IF $$INLSTHTN^TMGC0Q05(TMGDFN,SDATE,EDATE) DO
        . . SET NAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
        . . SET DOB=$PIECE($GET(^DPT(TMGDFN,0)),"^",3)
        . . SET HTNARRAY(NAME,DOB)=""
        WRITE "MEANINGFUL USE - PATIENT LIST  (HTN)",!,!
        SET NAME=""
        FOR  SET NAME=$ORDER(HTNARRAY(NAME)) QUIT:NAME=""  DO
        . SET DOB=0
        . FOR  SET DOB=$ORDER(HTNARRAY(NAME,DOB)) QUIT:DOB'>0  DO
        . . WRITE NAME," (",$$FMTE^XLFDT(DOB,"2D"),")",!
        DO ^%ZISC  ;" Close the output device
DNHTN   QUIT
