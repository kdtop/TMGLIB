TMGRPT2  ;TMG/kst TMG REPORTS  ;8/16/13, 2/2/14, 10/18/16
         ;;1.0;TMG-LIB;**1**;8/16/13
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
 ;"TMGIMMR(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) Immunization report
 ;"TMGHFACT(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) HEALTH FACTOR REPORT
 ;"TMGCPT(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) CPT report
 ;"BILLRPC(TMGRESULT,BDATE,EDATE,FILTER)  -- RPC entry point. Report of any billable items found 
 ;"GETITEMS(TMGRESULT,GROUP,BDATE,EDATE,FILTER) -- pull any billable items found 
 ;"GETICDS(TMGRESULT,BDATE,EDATE) -- return all the ICDs for patients seen for the date range
 ;"PNTITEMS -- print out BILLRPC report
 ;"RPTPTMIS -- Autoprint entry point for PTMISSED
 ;"ASKPTMIS -- Interactive entry point for PTMISSED 
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"IMMRPT(ROOT,DFN) --a report to be listed in the report tab in CPRS containing all the vaccinations a patient has been administered.
 ;"GETHF(TMGRESULT,DFN,STDATE,ENDDATE)  -- return health factors for a patient for given date range
 ;"PTMISSED -- report patients who have missed appts and have not had office visits in at least 3 months 
 ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;
TMGIMMR(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) ;"immunization report
  ;"Purpose: Entry point, as called from CPRS REPORT system
  ;"Input: ROOT -- Pass by NAME.  This is where output goes
  ;"       DFN -- Patient DFN ; ICN for foriegn sites
  ;"       ID --
  ;"       ALPHA -- Start date (lieu of DTRANGE)
  ;"       OMEGA -- End date (lieu of DTRANGE)
  ;"       DTRANGE -- # days back from today
  ;"       REMOTE --
  ;"       MAX    --
  ;"       ORFHIE --
  ;"Result: None.  Output goes into @ROOT
  DO IMMRPT(.ROOT,DFN)
  ;"DO SETHTML(.ROOT,"TEST STRING")
  ;"SET @ROOT@(1)="<HTML><HEAD><TITLE>IMMUNIZATIONS</TITLE></HEAD><BODY>THIS <B>IS A</B> TEST</BODY></HTML>"
  QUIT
  ;
TMGCPT(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) ;"CPT report
  ;"Purpose: Entry point, as called from CPRS REPORT system
  ;"Input: (see TMGIMMR)
  ;"ZLINK "TMGRPT4"  <--- uncomment to allow changing the report on the fly without starting CPRS
  DO TMGCPT^TMGRPT4(.ROOT,.DFN,.ID,.ALPHA,.OMEGA,.DTRANGE,.REMOTE,.MAX,.ORFHIE) ;"CPT report  
  QUIT
  ;
IMMRPT(ROOT,DFN)   ;
  ;"Purpose: This function returns a report to be listed in the report tab in CPRS
  ;"         containing all the vaccinations a patient has been administered.
  ;"Input:  DFN - Patient
  ;"Output: TMGRESULT(#)=IMM^DOS^NDC^MANUFACTURER^LOT^EXP
  NEW TIUIEN SET TIUIEN=0
  NEW TIULINE,TEXT,TMGRESULT
  NEW IMMIEN SET IMMIEN=1
  NEW GOTADMIN
  NEW TEMPSTR,IMM,NDC,MANUFACTURER,LOT,EXP,DONE,DOS
  FOR  SET TIUIEN=$ORDER(^TIU(8925,"C",DFN,TIUIEN)) QUIT:TIUIEN'>0  DO
  . SET TIULINE=0
  . FOR  SET TIULINE=$ORDER(^TIU(8925,TIUIEN,"TEXT",TIULINE)) QUIT:TIULINE'>0  DO
  . . SET TEXT=$GET(^TIU(8925,TIUIEN,"TEXT",TIULINE,0))
  . . NEW Y SET Y=$PIECE($GET(^TIU(8925,TIUIEN,0)),"^",7)
  . . D DD^%DT
  . . SET DOS=$PIECE(Y,"@",1)
  . . IF TEXT["IMMUNIZATION(S)" DO
  . . . SET DONE=0,TEMPSTR=""
  . . . NEW TEMPIDX SET TEMPIDX=TIULINE-1
  . . . ;"SET TIULINE=TIULINE+1
  . . . ;"SET IMM=$$TRIM^XLFSTR($PIECE($$HTML2TXS^TMGHTM1($GET(^TIU(8925,TIUIEN,"TEXT",TEMPIDX,0))),"-",1))
  . . . SET GOTADMIN=0
  . . . FOR  SET TEMPIDX=$ORDER(^TIU(8925,TIUIEN,"TEXT",TEMPIDX)) QUIT:(DONE)!(TEMPIDX'>0)  DO
  . . . . ;"SET GOTADMIN=0  MOVED 7/28/15
  . . . . SET TEMPSTR=TEMPSTR_$GET(^TIU(8925,TIUIEN,"TEXT",TEMPIDX,0))
  . . . . ;"IF (TEMPSTR["Manufacturer")!(TEMPSTR["NDC")!(TEMPSTR["Lot Number")!(TEMPSTR["Expiration") SET DONE=1,GOTADMIN=1
  . . . . IF TEMPSTR["Expiration" SET DONE=1,GOTADMIN=1
  . . . . IF (TEMPSTR["Ordered")!(TEMPSTR["Refused") SET DONE=1  ;"Not administered here
  . . . SET TEMPSTR=$P($$HTML2TXS^TMGHTM1(TEMPSTR),"DOCUMENTATION:",2)
  . . . SET IMM=$$TRIM^XLFSTR($PIECE(TEMPSTR,":",1))
  . . . SET IMM=$P(IMM,"-",1)
  . . . IF GOTADMIN=1 DO
  . . . . SET TEMPSTR=TEMPSTR_" "_$GET(^TIU(8925,TIUIEN,"TEXT",TEMPIDX,0))
  . . . . SET TEMPSTR=$$HTML2TXS^TMGHTM1(TEMPSTR)
  . . . . SET NDC=$$GETRSLT(TEMPSTR,"NDC: ")
  . . . . SET MANUFACTURER=$$GETRSLT(TEMPSTR,"Manufacturer: ")
  . . . . SET LOT=$$GETRSLT(TEMPSTR,"Lot Number: ")
  . . . . SET EXP=$$GETRSLT(TEMPSTR,"Date: ")
  . . . . ;"Original LineSET TMGRESULT(IMMIEN)=IMM_"^"_DOS_"^"_NDC_"^"_MANUFACTURER_"^"_LOT_"^"_EXP
  . . . . SET TEMPSTR=$$UP^XLFSTR(TEMPSTR)
  . . . . SET TEMPSTR=$PIECE(TEMPSTR,"ADMINISTERED TODAY.",2)
  . . . . SET TEMPSTR=$$ADDLF(TEMPSTR)
  . . . . SET TMGRESULT(IMMIEN)=DOS_"^"_IMM_"^<FONT SIZE=2>"_TEMPSTR_"</FONT>"   ;"NEW LINE
  . . . . SET IMMIEN=IMMIEN+1
  NEW HD SET HD="<TABLE BORDER=3><CAPTION><B>IMMUNIZATION RECORD</B><BR>"
  SET HD=HD_" FAMILY PHYSICIANS OF GREENEVILLE<BR>1410 TUSCULUM BLVD  STE. 2600 <BR>"
  SET HD=HD_" GREENEVILLE, TN 37745</CAPTION><TR><TH>ADMIN DATE</TH>"
  SET HD=HD_"<TH>VACCINATION</TH><TH>DETAILS</TH></TR>" 
  ;"Original -> <TH>DOS</TH><TH>NDC</TH><TH>MANUFACTURER</TH><TH>LOT NUMBER</TH><TH>EXP</TH></TR>"
  DO SETHTML(.ROOT,.TMGRESULT,"IMMUNIZATIONS",HD,3)
  QUIT
  ;
ADDLF(TEMPSTR)
  SET TEMPSTR=$PIECE(TEMPSTR,"NDC",1)_"<BR>NDC"_$PIECE(TEMPSTR,"NDC",2)
  SET TEMPSTR=$PIECE(TEMPSTR,"ROUTE",1)_"<BR>ROUTE"_$PIECE(TEMPSTR,"ROUTE",2)
  SET TEMPSTR=$PIECE(TEMPSTR,"LOT",1)_"<BR>LOT"_$PIECE(TEMPSTR,"LOT",2)
  SET TEMPSTR=$PIECE(TEMPSTR,"EXPIRA",1)_"<BR>EXPIRA"_$PIECE(TEMPSTR,"EXPIRA",2)
  QUIT TEMPSTR
  ;
GETRSLT(STRING,LABEL)  ;
  ;"PURPOSE: SEARCHES THOUGH A STRING FOR A LABEL AND RETURNS RESULTS
  ;"INPUT: STRING:  TEXT TO BE SEARCHED THROUGH
  ;"       LABEL:  LABEL TO FIND
  ;"RESULT: TEXT OR -1
  NEW TEMPSTRING
  NEW TMGRESULT SET TMGRESULT=-1
  SET TEMPSTRING=$$TRIM^XLFSTR($PIECE(STRING,LABEL,2))
  SET TMGRESULT=$PIECE(TEMPSTRING," ",1)
  IF LABEL["Date" DO
  . NEW Y,X
  . SET X=TMGRESULT
  . DO ^%DT
  . IF Y=-1 SET TMGRESULT=$PIECE(TEMPSTRING," ",1,3)
  . ;"SET TMGRESULT=TMGRESULT_"-"_Y
  QUIT TMGRESULT
  ;
  ;"----------------------------------------------------------------------------
  ;
TMGHFACT(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)  ;"HEALTH FACTOR REPORT
  ;"Purpose: Entry point, as called from CPRS REPORT system
  ;"Input: <Same as above>
  ;"Result: None. Output goes into @ROOT
  ;"SET @ROOT@(1)="<HTML><HEAD><TITLE>HEALTH FACTORS</TITLE></HEAD><BODY>THIS IS THE <B>TEST</B></BODY></HTML>"
  NEW HD SET HD="<TABLE BORDER=3><CAPTION><B>HEALTH FACTORS</B><BR>"
  SET HD=HD_" FAMILY PHYSICIANS OF GREENEVILLE<BR>1410 TUSCULUM BLVD  STE. 2600 <BR>"
  SET HD=HD_" GREENEVILLE, TN 37745</CAPTION><TR bgColor=#c4e3ed><TH>DATE</TH><TH>HEALTH FACTOR</TH><TH>COMMENT</TH></TR>"
  NEW DATA 
  DO GETHF(.DATA,DFN,ALPHA,OMEGA)
  DO XTRADATA(.DATA,DFN)
  DO SETHTML(.ROOT,.DATA,"HEALTH FACTORS",HD,3)
  QUIT
  ;"
GETHF(TMGRESULT,DFN,STDATE,ENDDATE)  ;"
  ;"Purpose: To return health factors for a patient for given date range
  ;"Input: DFN - patient DFN
  ;"       STDATE - START DATE
  ;"       ENDDATE - END DATE
  ;"Result: TMGRESULT(#)=DATE^HF^COMMENT
  KILL DATA
  NEW IEN,HFIEN
  SET IEN=0,HFIEN=0
  NEW COUNT SET COUNT=0
  NEW DATE,TEMPARR,EXTDATE
  FOR  SET IEN=$ORDER(^AUPNVHF("C",DFN,IEN)) QUIT:IEN'>0  DO
  . SET HFIEN=$PIECE($GET(^AUPNVHF(IEN,0)),"^",1)
  . IF HFIEN'>0 QUIT
  . NEW DATEIEN SET DATEIEN=$PIECE($GET(^AUPNVHF(IEN,0)),"^",3)
  . SET DATE=$PIECE($GET(^AUPNVSIT(DATEIEN,0)),"^",1)
  . IF DATE="" SET DATE="????"                               
  . NEW HFNAME SET HFNAME=$PIECE($GET(^AUTTHF(HFIEN,0)),"^",1)
  . IF HFNAME="" SET HFNAME="No name found for IEN: "_HFIEN
  . FOR  QUIT:'$DATA(TEMPARR(DATE))  SET DATE=DATE+0.000001
  . NEW HFCOMM SET HFCOMM=$GET(^AUPNVHF(IEN,811))
  . IF HFCOMM="" SET HFCOMM=" "
  . SET TEMPARR(DATE)=HFNAME_"^"_HFCOMM
  SET DATE=9999999
  FOR  SET DATE=$ORDER(TEMPARR(DATE),-1) QUIT:DATE'>0  DO
  . SET COUNT=COUNT+1
  . NEW Y SET Y=DATE
  . D DD^%DT
  . SET EXTDATE=$PIECE(Y,"@",1)
  . SET TMGRESULT(COUNT)=EXTDATE_"^"_$GET(TEMPARR(DATE))
  QUIT
  ;"
XTRADATA(TMGRESULT,DFN)  ;"
  NEW IDX,CFIDX,FINDING
  SET IDX=9000 ;"Initial index
  SET TMGRESULT(IDX)="<HR>^<HR>^<HR>",IDX=IDX+1
  SET TMGRESULT(IDX)="<B>COMPUTED FINDING^<B>FINDING RESULT^"
  SET TMGRESULT(IDX,"HEADING")=1,IDX=IDX+1
  SET CFIDX="103,110,108,112"  ;"IENS FOR FM FILE 811.4
  FOR FINDING=1:1:4  DO
  . NEW NAME,ROUTINE,ENTRY,TEST,ZN,XSTRING
  . SET ZN=$GET(^PXRMD(811.4,$PIECE(CFIDX,",",FINDING),0))
  . SET NAME=$PIECE(ZN,"^",1),ROUTINE=$PIECE(ZN,"^",2),ENTRY=$PIECE(ZN,"^",3)
  . SET XSTRING="DO "_ENTRY_"^"_ROUTINE_"("_DFN_",.TEST)"
  . XECUTE XSTRING
  . IF NAME["TMG" SET NAME=$PIECE(NAME,"TMG",2)
  . IF TEST=1 SET TEST="TRUE"
  . ELSE  SET TEST="FALSE"
  . SET TMGRESULT(IDX)=NAME_"^"_TEST_"^"
  . SET IDX=IDX+1
  QUIT
  ;"
SETHTML(ROOT,RESULTS,TITLE,HEADING,COLNUMS)  ;
  ;"Input: ROOT -- AN OUT PARAMETER 
  ;"          @ROOT@(1)= HEADING
  ;"          @ROOT@(2)=one long string with HTML codes.
  ;"          @ROOT@(3)=END OF TABLE                
  ;"       RESULTS -- INPUT DATA.  Pass by reference.  Format:  
  ;"            RESULT(#)=<COL1>^<COL2)^<COL3>
  ;"       TITLE -- STRING FOR TITLE OF TABLE
  ;"       HEADING -- Column titles, carot deliminated
  ;"             <Title1>^<Title2>^<Title3>
  ;"       COLNUM -- number of colums
  ;"Results -- none
  NEW END SET END=3
  MERGE ^EDDIE("TMGRPT2")=RESULTS
  NEW DATA
  SET @ROOT@(1)="<HTML><HEAD><TITLE>"_TITLE_"</TITLE></HEAD><BODY>"
  SET DATA=HEADING
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(RESULTS(IDX)) QUIT:IDX'>0  DO
  . IF $DATA(RESULTS(IDX,"HEADING")) DO
  . . SET DATA=DATA_"<TR bgcolor=#c4e3ed align=""center"">"
  . ELSE  DO
  . . SET DATA=DATA_"<TR>"
  . NEW PIECE
  . FOR PIECE=1:1:COLNUMS  DO
  . . SET DATA=DATA_"<TD>"_$PIECE($GET(RESULTS(IDX)),"^",PIECE)_"</TD>"
  . ;SET DATA=DATA_"<TD>"_$GET(RESULTS(IDX))_"</TD>"
  . SET DATA=DATA_"</TR>"
  . SET END=END+1
  SET @ROOT@(2)=DATA
  SET @ROOT@(3)="</TABLE></BODY></HTML>"
  QUIT        
  ;"
BILLRPC(TMGRESULT,BDATE,EDATE,FILTER)   ;"
  ;"Purpose: This is the RPC entry point for displaying a report
  ;"         of any billable items found for today's date.
  ;"
  NEW DATA
  SET BDATE=$GET(BDATE)
  SET EDATE=$GET(EDATE)
  SET FILTER=+$GET(FILTER)
  DO GETITEMS(.DATA,"SEQUELMED",BDATE,EDATE,FILTER)
  IF (BDATE="T")!(BDATE="") SET BDATE="TODAY"
  IF (EDATE="T")!(EDATE="") SET EDATE="TODAY"
  DO RTFFMT(.TMGRESULT,.DATA)
  NEW TITLE SET TITLE="" 
  IF FILTER=0 SET TITLE="ALL BILLABLE ITEMS "
  IF FILTER=1 SET TITLE="BILLABLE PROCEDURES "
  IF FILTER=2 SET TITLE="BILLABLE CPT-II CODES "
  IF FILTER=3 SET TITLE="ICD CODES "
  IF BDATE=EDATE SET TMGRESULT(1)="\qc {\fs30 "_TITLE_" FOR {\b "_BDATE_"}} \fs20 \line \line \ql"
  ELSE  SET TMGRESULT(1)="\qc {\fs30 "_TITLE_" FOR {\b "_BDATE_"} TO {\b "_EDATE_"}} \fs20 \line \line \ql"
  ;"kill TMGRESULT
  ;"set TMGRESULT(0)="{\rtf1\ansi\deff0 {\fonttbl {\f0 Times;}{\f1 Wingdings;}} \fs25 {\pard You know, {\f1 grep} is my {\fs60 favorite} Unix command! \par}}"
  QUIT
  ;"
FORMAT(TMGRESULT,DATA,SORT)  ;"
  ;"Purpose: Sort the data in array
  ;"Input: TMGRESULT -- OUT PARAMETER
  ;"       DATA -- ARRAY OF DATA
  ;"       SORT -- HOW TO SORT, UNUSED CURRENTLY
  ;"OUTPUT: TMGRESULT(#)="Line of text"
  NEW NAME SET NAME=""
  NEW LINE SET LINE=0
  FOR  SET NAME=$ORDER(DATA(NAME)) QUIT:NAME=""  DO
  . SET LINE=LINE+1
  . SET TMGRESULT(LINE)="==== "_NAME_" ===="
  . NEW ITEM SET ITEM=""
  . FOR  SET ITEM=$ORDER(DATA(NAME,ITEM)) QUIT:ITEM=""  DO
  . . SET LINE=LINE+1
  . . NEW DATE SET DATE=0
  . . FOR  SET DATE=$ORDER(DATA(NAME,ITEM,DATE)) QUIT:DATE'>0  DO        
  . . . NEW Y SET Y=DATE D DD^%DT     
   . . . SET TMGRESULT(LINE)="   [ ] "_Y_" ->"_ITEM
  . SET LINE=LINE+1
  . SET TMGRESULT(LINE)=""         
  QUIT
RTFFMT(TMGRESULT,DATA,SORT)  ;"
  ;"Same as above, but uses RTF tags
  NEW NAME SET NAME=""
  SET TMGRESULT(0)="{\rtf1\ansi\deff0 {\fonttbl {\f0 Times;}{\f1 Webdings;}} "
  NEW LINE SET LINE=1
  FOR  SET NAME=$ORDER(DATA(NAME)) QUIT:NAME=""  DO
  . SET LINE=LINE+1
  . SET TMGRESULT(LINE)="==== {\b "_NAME_"} ====\line"
  . NEW ITEM SET ITEM=""
  . FOR  SET ITEM=$ORDER(DATA(NAME,ITEM)) QUIT:ITEM=""  DO
  . . SET LINE=LINE+1
  . . NEW DATE SET DATE=0
  . . FOR  SET DATE=$ORDER(DATA(NAME,ITEM,DATE)) QUIT:DATE'>0  DO
  . . . NEW Y SET Y=DATE D DD^%DT
  . . . SET TMGRESULT(LINE)="   {\f1 [_] } "_Y_" -> {\i "_ITEM_" } \line"
  . SET LINE=LINE+1
  . SET TMGRESULT(LINE)="\line"
  SET LINE=LINE+1
  SET TMGRESULT(LINE)=" } "
  QUIT
  ;"
GETITEMS(TMGRESULT,GROUP,BDATE,EDATE,FILTER)  ;"
  ;"Purpose: This will pull any billable items found for the
  ;"         provided group name on the provided date.
  ;"Input: RESULT -- Out parameter
  ;"       GROUP -- Name of group of billable items
  ;"       BDATE -- (OPTIONAL). External date. Default is "T"
  ;"       EDATE -- (OPTIONAL). External date. Default is "T"
  ;"       FILTER -- 0,1,2,3 (0=ALL, 1=CPT, 2=CPT-II, 3=ICDs)
  ;"Output: TMGRESULT(TEXT,DFN,DATE)="" (e.g. TMGRESULT("TdaP (90715)",1234,3031212)=""
  NEW GROUPIEN SET GROUPIEN=+$ORDER(^TMG(22727,"B",GROUP,0))
  IF GROUPIEN'>0 QUIT
  NEW %DT,X,Y
  ;"SET BEGIN DATE
  SET BDATE=$GET(BDATE) 
  IF BDATE="" SET BDATE="T"
  SET X=BDATE DO ^%DT
  SET BDATE=$PIECE(Y,".",1)_".0000"
  ;"SET END DATE
  SET EDATE=$GET(EDATE)
  IF EDATE="" SET EDATE="T"
  SET X=EDATE DO ^%DT
  SET EDATE=$PIECE(Y,".",1)_".9999"        
  ;"Get immunizations        
  NEW BILLIEN
  SET BILLIEN=0
  IF FILTER=3 DO GETICDS(.TMGRESULT,BDATE,EDATE) QUIT
  IF FILTER>1 GOTO C2
  FOR  SET BILLIEN=$ORDER(^TMG(22727,GROUPIEN,1,BILLIEN)) QUIT:BILLIEN'>0  DO       
  . NEW IMMIEN,TEXT
  . SET IMMIEN=$PIECE($GET(^TMG(22727,GROUPIEN,1,BILLIEN,0)),"^",1)
  . SET TEXT=$PIECE($GET(^TMG(22727,GROUPIEN,1,BILLIEN,0)),"^",2)
  . NEW IEN SET IEN=0
  . FOR  SET IEN=$ORDER(^AUPNVIMM("B",IMMIEN,IEN)) QUIT:IEN'>0  DO
  . . NEW ZN SET ZN=$GET(^AUPNVIMM(IEN,0))
  . . NEW VISITIEN SET VISITIEN=$PIECE(ZN,"^",3)
  . . NEW DATE SET DATE=$PIECE($GET(^AUPNVSIT(VISITIEN,0)),"^",1)
  . . IF (DATE>BDATE)&(DATE<EDATE) DO
  . . . NEW DFN SET DFN=$PIECE(ZN,"^",2)
  . . . NEW NAME SET NAME=$$GETNAME(DFN)
  . . . SET TMGRESULT(NAME,TEXT,DATE)=""
  ;"Get Health Factors
  SET BILLIEN=0
  FOR  SET BILLIEN=$ORDER(^TMG(22727,GROUPIEN,2,BILLIEN)) QUIT:BILLIEN'>0  DO
  . NEW HFIEN,TEXT
  . SET HFIEN=$PIECE($GET(^TMG(22727,GROUPIEN,2,BILLIEN,0)),"^",1)
  . SET TEXT=$PIECE($GET(^TMG(22727,GROUPIEN,2,BILLIEN,0)),"^",2)
  . SET IEN=0
  . FOR  SET IEN=$ORDER(^AUPNVHF("B",HFIEN,IEN)) QUIT:IEN'>0  DO
  . . NEW ZN SET ZN=$GET(^AUPNVHF(IEN,0))
  . . NEW VISITIEN SET VISITIEN=$PIECE(ZN,"^",3)
  . . NEW DATE SET DATE=$PIECE($GET(^AUPNVSIT(VISITIEN,0)),"^",1)
  . . IF (DATE>BDATE)&(DATE<EDATE) DO
  . . . NEW DFN SET DFN=$PIECE(ZN,"^",2)
  . . . NEW NAME SET NAME=$$GETNAME(DFN)
  . . . SET TMGRESULT(NAME,TEXT,DATE)=""
  ;"Get TIU Notes
  SET BILLIEN=0
  FOR  SET BILLIEN=$ORDER(^TMG(22727,GROUPIEN,3,BILLIEN)) QUIT:BILLIEN'>0  DO
  . NEW TIUIEN,TEXT
  . SET TIUIEN=$PIECE($GET(^TMG(22727,GROUPIEN,3,BILLIEN,0)),"^",1)
  . SET TEXT=$PIECE($GET(^TMG(22727,GROUPIEN,3,BILLIEN,0)),"^",2)
  . SET IEN=0
  . FOR  SET IEN=$ORDER(^TIU(8925,"B",TIUIEN,IEN)) QUIT:IEN'>0  DO
  . . NEW ZN SET ZN=$GET(^TIU(8925,IEN,0))
  . . NEW DATE SET DATE=$PIECE(ZN,"^",7)
  . . IF (DATE>BDATE)&(DATE<EDATE) DO
  . . . NEW DFN SET DFN=$PIECE(ZN,"^",2)
  . . . NEW NAME SET NAME=$$GETNAME(DFN)
  . . . SET TMGRESULT(NAME,TEXT,DATE)=""
  ;"Look for billing tags
  SET BILLIEN=0
  FOR  SET BILLIEN=$ORDER(^TMG(22727,GROUPIEN,4,BILLIEN)) QUIT:BILLIEN'>0  DO 
  . NEW STR,TEXT
  . SET STR=$$UP^XLFSTR($PIECE($GET(^TMG(22727,GROUPIEN,4,BILLIEN,0)),"^",1))
  . SET TEXT=$PIECE($GET(^TMG(22727,GROUPIEN,4,BILLIEN,0)),"^",2)
  . NEW TIUDATE,TIUIEN,FOUND 
  . SET TIUDATE=$PIECE(BDATE,".",1)
  . FOR  SET TIUDATE=$ORDER(^TIU(8925,"D",TIUDATE)) QUIT:(TIUDATE>EDATE)!(TIUDATE'>0)  DO
  . . SET TIUIEN=0
  . . FOR  SET TIUIEN=$ORDER(^TIU(8925,"D",TIUDATE,TIUIEN)) QUIT:TIUIEN'>0  DO
  . . . SET FOUND=$$SRCHTIU(TIUIEN,STR)
  . . . IF FOUND=1 DO
  . . . . NEW DFN SET DFN=$PIECE($GET(^TIU(8925,TIUIEN,0)),"^",2)
  . . . . NEW NAME SET NAME=$$GETNAME(DFN)
  . . . . SET TMGRESULT(NAME,TEXT,TIUDATE)=""
  ;"Handle Routines
  IF FILTER=1 GOTO GDDN
C2 ;"CPT-II CODES  
  SET BILLIEN=0
  FOR  SET BILLIEN=$ORDER(^TMG(22727,GROUPIEN,5,BILLIEN)) QUIT:BILLIEN'>0  DO
  . NEW ENTRY,ROUTINE,CALL,TEXT,RESULT,OUTARRAY,RESULT
  . SET ENTRY=$P($GET(^TMG(22727,GROUPIEN,5,BILLIEN,0)),"^",1)
  . SET ROUTINE=$P($GET(^TMG(22727,GROUPIEN,5,BILLIEN,0)),"^",2)
  . SET TEXT=$GET(^TMG(22727,GROUPIEN,5,BILLIEN,1))
  . IF (ENTRY="")!(ROUTINE="") QUIT
  . SET CALL="SET RESULT=+$$"_ENTRY_"^"_ROUTINE_"(.OUTARRAY,BDATE,EDATE,TEXT)"
  . XECUTE CALL
  . IF RESULT>0 DO
  . . MERGE TMGRESULT=OUTARRAY
GDDN
  QUIT
  ;"
GETICDS(TMGRESULT,BDATE,EDATE)  ;"
  ;"This report will return all the ICDs for patients seen for the date
  ;"range specified
  NEW IEN,TEXT,ICDIEN,ICDTEXT
  NEW TIUDATE,TIUIEN,FOUND
  SET TIUDATE=$PIECE(BDATE,".",1)
  NEW SDT,EDT
  SET SDT=BDATE-10000
  SET EDT=BDATE
  FOR  SET TIUDATE=$ORDER(^TIU(8925,"D",TIUDATE)) QUIT:(TIUDATE>EDATE)!(TIUDATE'>0)  DO
  . SET TIUIEN=0
  . FOR  SET TIUIEN=$ORDER(^TIU(8925,"D",TIUDATE,TIUIEN)) QUIT:TIUIEN'>0  DO
  . . NEW DFN SET DFN=+$PIECE($GET(^TIU(8925,TIUIEN,0)),"^",2) QUIT:DFN'>0
  . . NEW NAME SET NAME=$$GETNAME(DFN)
  . . NEW ICDIEN SET ICDIEN=0
  . . FOR  SET ICDIEN=$ORDER(^AUPNVPOV("C",DFN,ICDIEN)) QUIT:+ICDIEN'>0  DO
  . . . NEW ZN SET ZN=$GET(^AUPNVPOV(ICDIEN,0)) QUIT:ZN=""
  . . . NEW VSTIEN SET VSTIEN=+$PIECE(ZN,"^",3) QUIT:VSTIEN'>0
  . . . NEW VSTZN SET VSTZN=$GET(^AUPNVSIT(VSTIEN,0)) QUIT:VSTZN=""
  . . . NEW VSTDT SET VSTDT=$PIECE(VSTZN,"^",1)
  . . . IF (VSTDT<SDT)!(VSTDT>EDT) QUIT  ;"Past this line, ICD is within daterange
  . . . ;" BELOW WAS ICDIEN, AND WHILE IT WAS NEWED WAS CONFUSING WITH ABOVE VARIABLE  12/28/16
  . . . NEW VICDIEN SET VICDIEN=+$PIECE(ZN,"^",1) QUIT:VICDIEN'>0
  . . . NEW ICDZN SET ICDZN=$GET(^ICD9(VICDIEN,0)) QUIT:ICDZN=""
  . . . NEW ADT SET ADT=$ORDER(^ICD9(VICDIEN,68,"B",""),-1) QUIT:ADT'>0
  . . . NEW PTR SET PTR=$ORDER(^ICD9(VICDIEN,68,"B",ADT,0)) QUIT:PTR'>0
  . . . NEW DESCR SET DESCR=$GET(^ICD9(VICDIEN,68,PTR,1))
  . . . NEW STR SET STR=$PIECE(ICDZN,"^",1)_" -- "_DESCR
  . . . SET TMGRESULT(NAME,STR,VSTDT)=""
  QUIT
SRCHTIU(IEN,TERM)  ;"SEARCH FOR A TERM
  ;"REWORKED TO ASSEMBLE ENTIRE NOTE THEN SEARCH     6/20/17
  NEW FOUND SET FOUND=0 ;"default to not found
  NEW LINE SET LINE=0
  NEW NOTETEXT SET NOTETEXT=""
  FOR  SET LINE=$ORDER(^TIU(8925,IEN,"TEXT",LINE)) QUIT:+LINE'>0  DO
  . NEW ONELINE SET ONELINE=$$UP^XLFSTR($GET(^TIU(8925,IEN,"TEXT",LINE,0)))
  . SET NOTETEXT=NOTETEXT_ONELINE
  IF NOTETEXT[TERM DO
  . SET FOUND=1
  QUIT FOUND
  ;"
PNTITEMS  ;"
  NEW OUT,BDATE,EDATE
  SET (BDATE,EDATE)=""
  READ "ENTER THE START DATE OR PRESS ENTER FOR TODAY",BDATE:$GET(DTIME,3600),!
  READ "ENTER THE END DATE OR PRESS ENTER FOR TODAY",EDATE:$GET(DTIME,3600),!
  NEW %ZIS
  SET %ZIS("A")="Enter Output Device: "
  SET %ZIS("B")="HOME"
  DO ^%ZIS  ;"standard device call
  IF POP DO  GOTO ABDn
  . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output.   Aborting.")
  USE IO
  DO BILLRPC^TMGRPT2(.OUT,.BDATE,.EDATE)
  WRITE !
  WRITE "************************************************************",!
  WRITE "              ",$GET(OUT(0)),!
  WRITE "************************************************************",!
  WRITE "                                            (From TMGRPT2.m)",!!
  NEW COUNT SET COUNT=0
  WRITE "BILLING ITEMS REPORT",!
  FOR  SET COUNT=$ORDER(OUT(COUNT)) QUIT:COUNT'>0  DO
  . WRITE $GET(OUT(COUNT)),!
  DO ^%ZISC  ;" Close the output device
ABDn  QUIT
  ;"
RPTPTMIS
  ;"Autoprint entry point for PTMISSED
  NEW %ZIS,IOP
  SET IOP="S121-LAUGHLIN-LASER"
  DO ^%ZIS  ;"standard device call
  IF POP DO
  . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output. Aborting.")
  use IO
  DO PTMISSED
  DO ^%ZISC  ;" Close the output device
  QUIT
  ;"
ASKPTMIS
  ;"Interactive entry point for PTMISSED
  NEW %ZIS
  SET %ZIS("A")="Enter Output Device: "
  SET %ZIS("B")="HOME"
  DO ^%ZIS  ;"standard device call
  USE IO
  DO PTMISSED
  DO ^%ZISC  ;" Close the output device  
  QUIT
  ;"
PTMISSED  ;" 
  ;"Purpose: This report prints out a list of patients who have
  ;"         missed appts and have not had office visits in at
  ;"         least 3 months
  NEW PTARRAY,VISITARR,CANCELIEN,NSIEN,DONE
  ;"string of visit note iens OV^FOCUS^ACUTE^PHYSICAL^WELCOME^MEDICARE
  SET VISITARR="1408^1983^1399^1402^7^6"
  ;"string of missed note iens  cancelled^noshow
  SET CANCELIEN=1455
  SET NSIEN=1413
  NEW MESSAGE
  SET MESSAGE(1455)=" cancelled on: "
  SET MESSAGE(1413)=" no showed on: "
  NEW X,Y DO NOW^%DTC NEW NowDate SET NowDate=X
  NEW CUTOFFDT SET CUTOFFDT=NowDate-20000
  ;
  WRITE !
  WRITE "************************************************************",!
  WRITE "   Overdue patients with missed or cancelled appointments",!
  WRITE "                     " SET Y=X DO DD^%DT WRITE Y,!
  WRITE "          Please deliver this report to the nurse",!
  WRITE "************************************************************",!
  WRITE "                                            (From TMGRPT2.m)",!!
  NEW PATIEN SET PATIEN=0
  FOR  SET PATIEN=$ORDER(^TIU(8925,"ZTMGPTDT",PATIEN)) QUIT:PATIEN'>0  DO
  . IF $$ACTIVEPT^TMGPXR03(PATIEN,4)<1 QUIT
  . NEW APPTDT SET APPTDT=NowDate-1
  . SET APPTDT=+$ORDER(^TMG(22723,PATIEN,1,"B",APPTDT))  
  . NEW APPTMSG,NEEDCALL
  . IF APPTDT>0 DO
  . . NEW Y SET Y=APPTDT
  . . DO DD^%DT
  . . SET APPTMSG="APPT SCHEDULED FOR "_Y
  . . SET NEEDCALL=0
  . ELSE  DO
  . . SET APPTMSG="**NO UPCOMING APPOINTMENT SCHEDULED**"
  . . SET NEEDCALL=1
  . SET DONE=0
  . NEW DATEIEN SET DATEIEN=9999999
  . FOR  SET DATEIEN=$ORDER(^TIU(8925,"ZTMGPTDT",PATIEN,DATEIEN),-1) QUIT:(DATEIEN'>0)!(DONE=1)  DO
  . . IF DATEIEN<CUTOFFDT SET DONE=1 QUIT
  . . NEW DOCIEN SET DOCIEN=0
  . . FOR  SET DOCIEN=$ORDER(^TIU(8925,"ZTMGPTDT",PATIEN,DATEIEN,DOCIEN)) QUIT:(DOCIEN'>0)!(DONE=1)  DO
  . . . NEW ZN SET ZN=$GET(^TIU(8925,DOCIEN,0))
  . . . NEW DOCTYPE SET DOCTYPE=$PIECE(ZN,"^",1)
  . . . IF VISITARR[DOCTYPE SET DONE=1 QUIT
  . . . IF (DOCTYPE=CANCELIEN)!(DOCTYPE=NSIEN) DO
  . . . . NEW NAME SET NAME=$PIECE($GET(^DPT(PATIEN,0)),"^",1)
  . . . . NEW Y SET Y=DATEIEN
  . . . . DO DD^%DT
  . . . . SET PTARRAY(NAME,1)=$GET(MESSAGE(DOCTYPE))_$PIECE(Y,"@",1)_" "_APPTMSG
  . . . . IF NEEDCALL=1 DO
  . . . . . NEW PHONENODE SET PHONENODE=$GET(^DPT(PATIEN,.13))
  . . . . . NEW HOMEPHONE SET HOMEPHONE=$PIECE(PHONENODE,"^",1)
  . . . . . NEW CELLPHONE SET CELLPHONE=$PIECE(PHONENODE,"^",4)
  . . . . . NEW PHONEMESSAGE SET PHONEMESSAGE=""
  . . . . . IF HOMEPHONE'="" SET PHONEMESSAGE=HOMEPHONE_"(h) "
  . . . . . IF CELLPHONE'="" SET PHONEMESSAGE=PHONEMESSAGE_CELLPHONE_"(c)"
  . . . . . IF PHONEMESSAGE="" DO
  . . . . . . SET PHONEMESSAGE="NO PHONE NUMBERS FOUND IN SYSTEM."
  . . . . . ELSE  DO
  . . . . . . SET PHONEMESSAGE="CALL PATIENT AT: "_PHONEMESSAGE
  . . . . . SET PTARRAY(NAME,2)="[ ] "_PHONEMESSAGE
  . . . . SET DONE=1
  NEW NAMEIDX,IDX SET NAMEIDX=0
  FOR  SET NAMEIDX=$ORDER(PTARRAY(NAMEIDX)) QUIT:NAMEIDX=""  DO
  . SET IDX=0
  . WRITE NAMEIDX
  . FOR  SET IDX=$ORDER(PTARRAY(NAMEIDX,IDX)) QUIT:IDX'>0  DO
  . . WRITE $GET(PTARRAY(NAMEIDX,IDX)),!
  . WRITE "----",!
  QUIT
  ;"
IHSPAT    ;"Print any patients with missing locations in IHS Patient file
  NEW IDX SET IDX=0
  NEW PTARRAY
  FOR  SET IDX=$ORDER(^AUPNPAT(IDX)) QUIT:IDX'>0  DO
  . IF '$DATA(^AUPNPAT(IDX,41)) DO
  . . IF $$ACTIVEPT^TMGPXR03(IDX,4)<1 QUIT
  . . SET PTARRAY($PIECE($GET(^DPT(IDX,0)),"^",1))=""
  WRITE "****THE FOLLOWING PATIENTS DON'T HAVE LOCATIONS SET IN IHS PATIENT FILE****",!
  NEW NAME SET NAME=""
  FOR  SET NAME=$ORDER(PTARRAY(NAME)) QUIT:NAME=""  DO
  WRITE NAME,!
  QUIT
  ;"
BILLTEST(OUTARRAY,BDATE,EDATE,TEXT)  ;"DEMO TEST FUNCTION
  ;"Params: OutArray: PASS BY REFERENCE.  Format:
  ;"          OutArray(PatientName,TextToReturnWhenFound,DateFound)=""
  ;"              PatientName is the patient's name
  ;"              TextToReturnWhenFound is the text that will be displayed on the report
  ;"              DateFound is the date (in Fileman date format) 
  ;"        BDATE: Beginning search date, sent from CPRS, in fileman format
  ;"        EDATE: Ending search date, sent from CPRS, in fileman format
  ;"        TEXT: This is Text To Return When Found
  ;"             Initially, it is the text that is specified in the TMG BILLABLE ITEMS file
  ;"             it can be used or replaced as needed.  This should specify what 
  ;"             to show on report.  E.g. 'Influenza (90656)' 
  ;"RESULT: integer result is also expected, that represents number of records returned. 
  ;"        If this isn't done, 0 is assumed.    
  NEW TMGRESULT SET TMGRESULT=1
  ;"SET OUTARRAY("TEST,PERSON","NO CPT HERE-TEST FUNCTION",3160919)=""
  ;"SET OUTARRAY("TEST,PERSON",TEXT,3160919)=""
  QUIT TMGRESULT
  ;"
GETNAME(DFN)
  SET DFN=+$GET(DFN)
  QUIT $PIECE($GET(^DPT(DFN,0)),"^",1)_" ("_$PIECE($GET(^DPT(DFN,"TMG")),"^",2)_")"
  ;
ASKBMI
  ;"Interactive entry point for PTMISSED
  NEW %ZIS
  SET %ZIS("A")="Enter Output Device: "
  SET %ZIS("B")="HOME"
  DO ^%ZIS  ;"standard device call
  USE IO
  DO BMIMISS
  DO ^%ZISC  ;" Close the output device
  QUIT
  ;"
BMIMISS  ;" This is a temp report. it can be deleted.
  ;"It simply checks each note to see if a BMI is found. If not,
  ;"it prints out the note title and patient
  NEW DATE SET DATE=3160101
  WRITE "OFFICE NOTES MISSING BMI",!,!
  NEW IEN
  FOR  SET DATE=$ORDER(^TIU(8925,"D",DATE)) QUIT:DATE'>0  DO
  . SET IEN=0
  . FOR  SET IEN=$ORDER(^TIU(8925,"D",DATE,IEN)) QUIT:IEN'>0  DO
  . . ;"TEST BELOW
  . . NEW VITALS DO VITARR^TMGTIUO3(.VITALS,IEN,DATE\1,DATE\1_".9999")  
  . . IF $$SRCHTIU(IEN,"BMI")="0" DO
  . . . NEW ZN SET ZN=$GET(^TIU(8925,IEN,0))
  . . . NEW DFN SET DFN=$PIECE(ZN,"^",2)
  . . . NEW NAME SET NAME=$$GETNAME(DFN)
  . . . NEW TITLEIEN SET TITLEIEN=$PIECE(ZN,"^",1)
  . . . NEW TITLE SET TITLE=$PIECE($GET(^TIU(8925.1,TITLEIEN,0)),"^",1)
  . . . IF TITLE'["VISIT" QUIT
  . . . IF TITLE["ACUTE" QUIT
  . . . IF TITLE["IMAGE" QUIT
  . . . IF TITLE["ADDENDUM" QUIT
  . . . IF TITLE["RESULT" QUIT
  . . . IF TITLE["PHONE" QUIT
  . . . IF TITLE["EXCUSE" QUIT
  . . . IF TITLE["RECORDS" QUIT
  . . . IF TITLE["MEDICATION" QUIT
  . . . IF TITLE["DEATH" QUIT
  . . . IF TITLE["CANCELLED" QUIT
  . . . IF TITLE["IMM" QUIT
  . . . IF TITLE["URINE" QUIT
  . . . IF TITLE["DISCHARGE" QUIT
  . . . IF TITLE["NO SHOW" QUIT
  . . . IF TITLE["COLLECTION" QUIT
  . . . IF TITLE["NURSE" QUIT
  . . . IF TITLE["TICKLER" QUIT
  . . . IF TITLE["NOT KEPT" QUIT
  . . . IF TITLE["LETTER" QUIT
  . . . IF NAME["ZZTEST" QUIT
  . . . NEW Y SET Y=DATE X ^DD("DD")
  . . . ;"WRITE NAME,?30,TITLE,?65,Y,!,"          Notes:",!
  . . . WRITE NAME,?30,TITLE,!,Y," _________________________________________________________",! 
  QUIT
  ;"
NEXTAPPT(TMGRESULT,DFN)  ;"
  SET TMGRESULT="UPCOMING APPTS:"
  NEW DATE,X,Y
  DO NOW^%DTC SET DATE=X
  NEW APPTCOUNT SET APPTCOUNT=0
  FOR  SET DATE=$ORDER(^TMG(22723,DFN,1,"B",DATE)) QUIT:DATE'>0  DO
  . NEW IDX SET IDX=$ORDER(^TMG(22723,DFN,1,"B",DATE,0))
  . IF $P($G(^TMG(22723,DFN,1,IDX,0)),"^",7)'="A" QUIT
  . NEW REASON SET REASON=$P($G(^TMG(22723,DFN,1,IDX,0)),"^",4)
  . SET Y=DATE
  . DO DD^%DT
  . SET TMGRESULT=TMGRESULT_" "_Y_" - "_REASON
  . SET APPTCOUNT=APPTCOUNT+1
  IF APPTCOUNT=0 SET TMGRESULT="NO UPCOMING APPOINTMENT FOUND."
  QUIT
  ;"
MISSED
       ;"Purpose: Provide an NON-interactive entry point for report
       ; device.
       NEW %ZIS,IOP
       SET IOP="S121-LAUGHLIN-LASER"
       DO ^%ZIS  ;"standard device call
       IF POP DO  GOTO DCNDn
       . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output. Aborting.")
       use IO
       DO MISMAMMO
       DO ^%ZISC  ;" Close the output device
DCNDn  QUIT
       ;"
MISMAMMO
       ;"Purpose: Print report for patients with unscheduled mammograms
       NEW RESULT
       NEW X,Y DO NOW^%DTC NEW NowDate SET NowDate=X
       NEW REMARR
       SET REMARR(224)="TMG MAMMOGRAM/BREAST IMAGING"
       DO ALLREMS^TMGPXR03(.RESULT,.REMARR,1)
       IF RESULT(0)=0 GOTO MMDn
       WRITE !
       WRITE "************************************************************",!
       WRITE "                      Overdue mammogram reminders",!
       WRITE "              NOTE: this will display yearly reminders which are",!
       WRITE "                    over 6 months past due, or shorter followups",!
       WRITE "                    which are 3 months past due.",!
       WRITE "                            " SET Y=NowDate DO DD^%DT WRITE Y,!
       WRITE "               Please deliver this report to the NURSE",!
       WRITE "************************************************************",!
       WRITE "                                            (From TMGRPT2.m)",!!
       NEW REMIEN SET REMIEN=0
       NEW RESULTARR 
       FOR  SET REMIEN=$ORDER(RESULT(REMIEN)) QUIT:REMIEN'>0  DO
       . NEW REMDISP SET REMDISP=$PIECE($GET(^PXD(811.9,REMIEN,0)),"^",3)
       . WRITE "============= PATIENTS DUE FOR ",REMDISP," ============",!
       . NEW DATE SET DATE=""
       . FOR  SET DATE=$ORDER(RESULT(REMIEN,DATE)) QUIT:DATE=""  DO
       . . NEW DFN SET DFN=0
       . . FOR  SET DFN=$ORDER(RESULT(REMIEN,DATE,DFN)) QUIT:DFN'>0  DO
       . . . NEW REMSTR SET REMSTR=$G(RESULT(REMIEN,DATE,DFN))
       . . . NEW DUEDATE SET DUEDATE=$P(REMSTR,"^",2)
       . . . NEW NAME SET NAME=$PIECE($GET(^DPT(DFN,0)),"^",1)
       . . . IF $$EXCLUDE($G(RESULT(REMIEN,DATE,DFN)),.RESULTARR,NAME,DUEDATE)=1 QUIT
       . . . ;"SET RESULTARR(NAME,DUEDATE)=""
       SET DATE=""
       FOR  SET DATE=$ORDER(RESULTARR(DATE)) QUIT:DATE=""  DO
       . SET NAME="" 
       . FOR  SET NAME=$ORDER(RESULTARR(DATE,NAME)) QUIT:NAME=""  DO
       . . WRITE "  - ",NAME,?40,DATE,!       
MMDn  
  QUIT
  ;"
EXCLUDE(REMSTR,ARRAY,NAME,DUEDATE)
       NEW RESULT SET RESULT=1  ;"DEFAULT TO EXCLUDE
       NEW FUDAYS,PASTDUEDAYS,YRMAXDAYS,LESSMAXDAYS
       NEW NOW,X DO NOW^%DTC SET NOW=X
       SET YRMAXDAYS=180,LESSMAXDAYS=90
       IF $P(REMSTR,"^",1)["SOON" GOTO EXDN
       IF $P(REMSTR,"^",2)["DUE" DO
       . SET RESULT=0
       . SET ARRAY(DUEDATE,NAME)=""
       ELSE  DO
       . NEW DISPLAYSTR
       . NEW X1,X2 SET X1=$$DTTOFMDT($P(REMSTR,"^",2)),X2=$$DTTOFMDT($P(REMSTR,"^",3))
       . DO ^%DTC SET FUDAYS=X
       . SET X2=$$DTTOFMDT($P(REMSTR,"^",2)),X1=NOW
       . DO ^%DTC SET PASTDUEDAYS=X
       . ;"WRITE REMSTR," -> ",FUDAYS,!
       . IF FUDAYS<360 DO
       . . IF PASTDUEDAYS>LESSMAXDAYS DO
       . . . SET RESULT=0,DISPLAYSTR=FUDAYS_" DAY FU WAS DUE ON "_DUEDATE
       . . . SET ARRAY(DISPLAYSTR,NAME)=""
       . ELSE  DO
       . . IF PASTDUEDAYS>YRMAXDAYS DO
       . . . SET RESULT=0,DISPLAYSTR="YEARLY FU WAS DUE ON "_DUEDATE
       . . . SET ARRAY(DISPLAYSTR,NAME)=""
EXDN   QUIT RESULT
       ;"
DTTOFMDT(EXTDATE)  ;"RETURNS FMDATETIME
       NEW FMDATE,%DT,X
       IF $P(EXTDATE,"/",1)="00" SET $P(EXTDATE,"/",1)="01"
       IF $P(EXTDATE,"/",2)="00" SET $P(EXTDATE,"/",2)="01"
       SET X=$P(EXTDATE,"@",1),%DT="T"
       DO ^%DT
       SET FMDATE=Y       
       QUIT FMDATE
       ;"
TICKLER(ROOT,DFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) ;"Tickler report
  ;"Purpose: Entry point, as called from CPRS REPORT system
  ;"Input: ROOT -- Pass by NAME.  This is where output goes
  ;"       DFN -- Patient DFN ; ICN for foriegn sites
  ;"       ID --
  ;"       ALPHA -- Start date (lieu of DTRANGE)
  ;"       OMEGA -- End date (lieu of DTRANGE)
  ;"       DTRANGE -- # days back from today
  ;"       REMOTE --
  ;"       MAX    --
  ;"       ORFHIE --
  NEW TICKLIEN,ARRAY SET TICKLIEN=0
  NEW HD SET HD="<TABLE BORDER=3><CAPTION><B>PATIENT TICKLERS</B>"
  ;"SET HD=HD_"<BR> FAMILY PHYSICIANS OF GREENEVILLE<BR>1410 TUSCULUM BLVD  STE. 2600 <BR>"
  ;"SET HD=HD_" GREENEVILLE, TN 37745
  SET HD=HD_"</CAPTION><TR bgColor=#c4e3ed><TH>DUE DATE</TH><TH>STATUS</TH><TH>RECIPIENT</TH><TH>NOTE DATE</TH><TH>NOTE TITLE</TH></TR>"
  NEW DATA,TEMPARR,TIU,STATUS,USER,DUE,IDX,TIUDATE,TIUNAME
  FOR  SET TICKLIEN=$O(^TMG(22705.5,"B",DFN,TICKLIEN)) QUIT:TICKLIEN'>0  DO
  . NEW ZN SET ZN=$G(^TMG(22705.5,TICKLIEN,0))
  . SET TIU=$P(ZN,"^",4),STATUS=$P(ZN,"^",3),USER=$P(ZN,"^",5),DUE=$P(ZN,"^",2)
  . IF +$G(TIU)'>0 QUIT
  . SET TIUDATE=$$EXTDATE^TMGDATE($P($G(^TIU(8925,TIU,0)),"^",7))
  . SET TIUNAME=$P($G(^TIU(8925.1,$P($G(^TIU(8925,TIU,0)),"^",1),0)),"^",1)
  . NEW OVERDUE SET OVERDUE=$$OVERDUE(STATUS,DUE)
  . SET TEMPARR(DUE,TIU,OVERDUE)=$$TSTATUS(STATUS)_"^"_$P($G(^VA(200,USER,0)),"^",1)_"^"_TIUDATE_"^"_TIUNAME
  SET DUE=9999999,IDX=1
  FOR  SET DUE=$O(TEMPARR(DUE),-1) QUIT:DUE'>0  DO
  . SET TIU=0
  . FOR  SET TIU=$O(TEMPARR(DUE,TIU)) QUIT:TIU'>0  DO
  . . NEW OVERDUE SET OVERDUE=$O(TEMPARR(DUE,TIU,-1))
  . . IF OVERDUE>0 DO
  . . . SET DATA(IDX)="<b>"_$$EXTDATE^TMGDATE(DUE)_"^<b>"_$G(TEMPARR(DUE,TIU,OVERDUE))_"</b>"
  . . ELSE  DO
  . . . SET DATA(IDX)=$$EXTDATE^TMGDATE(DUE)_"^"_$G(TEMPARR(DUE,TIU,OVERDUE))
  . . SET IDX=IDX+1
  DO SETHTML(.ROOT,.DATA,"PATIENT TICKLER MESSAGES",HD,5)
  QUIT
  ;"
TSTATUS(STATUS)
  NEW RESULT SET RESULT=""
  IF STATUS="C" QUIT "COMPLETED"
  IF STATUS="U" QUIT "UNSIGNED"
  IF STATUS="S" QUIT "PENDING"
  IF STATUS="O" QUIT "DISCARDED"
  QUIT RESULT
  ;"
OVERDUE(STATUS,DUE)
  NEW RESULT SET RESULT=0
  IF (STATUS'="C")&(STATUS'="O") DO
  . IF DUE<$$TODAY^TMGDATE(0) SET RESULT=1
  QUIT RESULT
  ;"