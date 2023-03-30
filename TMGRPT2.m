TMGRPT2  ;TMG/kst TMG REPORTS  ;8/16/13, 2/2/14, 10/18/16, 3/24/21
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
 ;"TMGIMMR(ROOT,TMGDFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) Immunization report
 ;"TMGHFACT(ROOT,TMGDFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) HEALTH FACTOR REPORT
 ;"TMGCPT(ROOT,TMGDFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) CPT report
 ;"BILLRPC(TMGRESULT,BDATE,EDATE,FILTER)  -- RPC entry point. Report of any billable items found 
 ;"GETITEMS(TMGRESULT,GROUP,BDATE,EDATE,FILTER) -- pull any billable items found 
 ;"GETICDS(TMGRESULT,BDATE,EDATE) -- return all the ICDs for patients seen for the date range
 ;"PNTITEMS -- print out BILLRPC report
 ;"RPTPTMIS -- Autoprint entry point for PTMISSED
 ;"ASKPTMIS -- Interactive entry point for PTMISSED 
 ;
 ;"IHSPAT --Print any patients with missing locations in IHS Patient file
 ;"BILLTEST(OUTARRAY,BDATE,EDATE,TEXT) --DEMO TEST FUNCTION
 ;"ASKBMI -- Interactive entry point for BMIMISSED
 ;"TMPLNAPT(TMGDFN) -- TIU TEMPLATE TO RETURN NEXT APPTS
 ;"NEXTAPPT(TMGRESULT,TMGDFN,TODAY) 
 ;"MISMAMMO -- Print report for patients with unscheduled mammograms
 ;"MAMORDRD(TMGDFN) -- Determine if patient has an outstanding mammogram ordered
 ;"TICKLER(ROOT,TMGDFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) --Tickler report
 ;"TOPICS(ROOT,TMGDFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) -- TOPICS report
 ;"CPTIIRPT -- return all patients, over 65 with any metrics they have had for the current month.
 ;"GETCPT(TMGDFN,KEY,VALUE,TMGRESULT,DATE) --  returns the CPT and ICD codes to be documented.
 ;"CHKNOTE(TMGDFN,NOTEIEN,BEGINDATE,DATESTR,CPT,EXCLUDEFOUND) -- find whether the given note exists
 ;"SMOKER(OUTARR)  
 ;"HASNOTES(TMGDFN,BEGINDATE)  -- returns 0 (if none) or date of note the patient had after BEGINDATE
 ;"APPTTIME(BDATE,EDATE) -- calculate the wait times for a given time period.
 ;"APTTIME2(BDATE,EDATE) -- calculate the wait times for a given time period.
 ;"WAITTIME(BDATE,EDATE) -- calculate the wait times for a given time period.
 ;"MISOVCHG(BDATE,EDATE) -- Print appts that don't have corresponding OV charges
 ;" TMGAWV(ROOT,TMGDFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) -- AWV report
 ;"AWVREMS(REMLIST) -- list of reminders to include on AWV report
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"IMMRPT(ROOT,TMGDFN) --a report to be listed in the report tab in CPRS containing all the vaccinations a patient has been administered.
 ;"GETHF(TMGRESULT,TMGDFN,STDATE,ENDDATE)  -- return health factors for a patient for given date range
 ;"PTMISSED -- report patients who have missed appts and have not had office visits in at least 3 months 
 ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;
TMGIMMR(ROOT,TMGDFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) ;"immunization report
  ;"Purpose: Entry point, as called from CPRS REPORT system
  ;"Input: ROOT -- Pass by NAME.  This is where output goes
  ;"       TMGDFN -- Patient DFN ; ICN for foriegn sites
  ;"       ID --
  ;"       ALPHA -- Start date (lieu of DTRANGE)
  ;"       OMEGA -- End date (lieu of DTRANGE)
  ;"       DTRANGE -- # days back from today
  ;"       REMOTE --
  ;"       MAX    --
  ;"       ORFHIE --
  ;"Result: None.  Output goes into @ROOT
  DO IMMRPT(.ROOT,TMGDFN)
  ;"DO SETHTML(.ROOT,"TEST STRING")
  ;"SET @ROOT@(1)="<HTML><HEAD><TITLE>IMMUNIZATIONS</TITLE></HEAD><BODY>THIS <B>IS A</B> TEST</BODY></HTML>"
  QUIT
  ;
TMGCPT(ROOT,TMGDFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) ;"CPT report
  ;"Purpose: Entry point, as called from CPRS REPORT system
  ;"Input: (see TMGIMMR)
  ;"ZLINK "TMGRPT4"  <--- uncomment to allow changing the report on the fly without starting CPRS
  DO TMGCPT^TMGRPT4(.ROOT,.TMGDFN,.ID,.ALPHA,.OMEGA,.DTRANGE,.REMOTE,.MAX,.ORFHIE) ;"CPT report  
  QUIT
  ;
IMMRPT(ROOT,TMGDFN)   ;
  ;"Purpose: This function returns a report to be listed in the report tab in CPRS
  ;"         containing all the vaccinations a patient has been administered.
  ;"Input:  TMGDFN - Patient
  ;"Output: TMGRESULT(#)=IMM^DOS^NDC^MANUFACTURER^LOT^EXP
  NEW TIUIEN SET TIUIEN=0
  NEW TIULINE,TEXT,TMGRESULT
  NEW IMMIEN SET IMMIEN=1
  NEW GOTADMIN
  NEW TEMPSTR,IMM,NDC,MANUFACTURER,LOT,EXP,DONE,DOS
  FOR  SET TIUIEN=$ORDER(^TIU(8925,"C",TMGDFN,TIUIEN)) QUIT:TIUIEN'>0  DO
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
  NEW HD SET HD="<TABLE BORDER=3><CAPTION><B>IMMUNIZATION ADMINISTRATION RECORD</B><BR>"
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
TMGHFACT(ROOT,TMGDFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)  ;"HEALTH FACTOR REPORT
  ;"Purpose: Entry point, as called from CPRS REPORT system
  ;"Input: <Same as above>
  ;"Result: None. Output goes into @ROOT
  ;"SET @ROOT@(1)="<HTML><HEAD><TITLE>HEALTH FACTORS</TITLE></HEAD><BODY>THIS IS THE <B>TEST</B></BODY></HTML>"
  NEW HD SET HD="<TABLE BORDER=3><CAPTION><B>HEALTH FACTORS</B><BR>"
  SET HD=HD_" FAMILY PHYSICIANS OF GREENEVILLE<BR>1410 TUSCULUM BLVD  STE. 2600 <BR>"
  SET HD=HD_" GREENEVILLE, TN 37745</CAPTION><TR bgColor=#c4e3ed><TH>DATE</TH><TH>HEALTH FACTOR</TH><TH>COMMENT</TH></TR>"
  NEW DATA 
  DO GETHF(.DATA,TMGDFN,ALPHA,OMEGA)
  DO XTRADATA(.DATA,TMGDFN)
  DO SETHTML(.ROOT,.DATA,"HEALTH FACTORS",HD,3)
  QUIT
  ;"
GETHF(TMGRESULT,TMGDFN,STDATE,ENDDATE)  ;"
  ;"Purpose: To return health factors for a patient for given date range
  ;"Input: TMGDFN - patient DFN
  ;"       STDATE - START DATE
  ;"       ENDDATE - END DATE
  ;"Result: TMGRESULT(#)=DATE^HF^COMMENT
  KILL DATA
  NEW IEN,HFIEN
  SET IEN=0,HFIEN=0
  NEW COUNT SET COUNT=0
  NEW DATE,TEMPARR,EXTDATE
  FOR  SET IEN=$ORDER(^AUPNVHF("C",TMGDFN,IEN)) QUIT:IEN'>0  DO
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
XTRADATA(TMGRESULT,TMGDFN)  ;"
  NEW IDX,CFIDX,FINDING
  SET IDX=9000 ;"Initial index
  SET TMGRESULT(IDX)="<HR>^<HR>^<HR>",IDX=IDX+1
  SET TMGRESULT(IDX)="<B>COMPUTED FINDING^<B>FINDING RESULT^"
  SET TMGRESULT(IDX,"HEADING")=1,IDX=IDX+1
  SET CFIDX="103,110,108,112,122,149"  ;"IENS FOR FM FILE 811.4
  FOR FINDING=1:1:6  DO
  . NEW NAME,ROUTINE,ENTRY,TEST,ZN,XSTRING
  . SET ZN=$GET(^PXRMD(811.4,$PIECE(CFIDX,",",FINDING),0))
  . SET NAME=$PIECE(ZN,"^",1),ROUTINE=$PIECE(ZN,"^",2),ENTRY=$PIECE(ZN,"^",3)
  . SET XSTRING="DO "_ENTRY_"^"_ROUTINE_"("_TMGDFN_",.TEST)"
  . XECUTE XSTRING
  . IF NAME["TMG" SET NAME=$PIECE(NAME,"TMG",2)
  . IF TEST=1 SET TEST="TRUE"
  . ELSE  SET TEST="FALSE"
  . SET TMGRESULT(IDX)=NAME_"^"_TEST_"^"
  . SET IDX=IDX+1
  NEW LASTCPE DO LASTCPE^TMGTIUOA(.LASTCPE,TMGDFN)
  NEW LASTAWV DO LASTAWV^TMGTIUOA(.LASTAWV,TMGDFN)
  SET TMGRESULT(IDX)="<HR>^<HR>^<HR>",IDX=IDX+1
  SET TMGRESULT(IDX)="<B>BILLING TYPE^<B>DATE LAST BILLED FOR^"
  SET TMGRESULT(IDX,"HEADING")=1,IDX=IDX+1
  SET TMGRESULT(IDX)="LAST BILLED PHYSICAL^"_LASTCPE,IDX=IDX+1
  SET TMGRESULT(IDX)="LAST BILLED AWV^"_LASTAWV,IDX=IDX+1
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
  NEW UPDATED SET UPDATED="LAST UPDATED: "_$$TODAY^TMGDATE(1)
  NEW TIME SET TIME=$$NOW^TMGDATE
  SET UPDATED=UPDATED_" @ "_$E(TIME,1,2)_":"_$E(TIME,3,4)
  IF BDATE=EDATE SET TMGRESULT(1)="\qc {\fs30 "_TITLE_" FOR {\b "_BDATE_"}} \line {\b "_UPDATED_"} \fs20 \line \line \ql"
  ELSE  SET TMGRESULT(1)="\qc {\fs30 "_TITLE_" FOR {\b "_BDATE_"} TO {\b "_EDATE_"}} \line {\b "_UPDATED_"} \fs20 \line \line \ql"
  ;"IF BDATE=EDATE SET TMGRESULT(1)="\qc {\fs30 "_TITLE_" FOR {\b "_BDATE_"}} \fs20 \line \line \ql"
  ;"ELSE  SET TMGRESULT(1)="\qc {\fs30 "_TITLE_" FOR {\b "_BDATE_"} TO {\b "_EDATE_"}} \fs20 \line \line \ql"
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
  ;"Output: TMGRESULT(TEXT,TMGDFN,DATE)="" (e.g. TMGRESULT("TdaP (90715)",1234,3031212)=""
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
  . . . NEW TMGDFN SET TMGDFN=$PIECE(ZN,"^",2)
  . . . NEW NAME SET NAME=$$GETNAME(TMGDFN)
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
  . . . NEW TMGDFN SET TMGDFN=$PIECE(ZN,"^",2)
  . . . NEW NAME SET NAME=$$GETNAME(TMGDFN)
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
  . . . NEW TMGDFN SET TMGDFN=$PIECE(ZN,"^",2)
  . . . NEW NAME SET NAME=$$GETNAME(TMGDFN)
  . . . SET TMGRESULT(NAME,TEXT,DATE)=""
  . . . NEW ACEARB,MEDSTR
  . . . SET ACEARB=$$CHKMEDS(TMGDFN,49,.MEDSTR,"4010F")
  . . . IF ACEARB=1 SET TMGRESULT(NAME,"ACE/ARB "_MEDSTR,DATE)=""
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
  . . . . NEW TMGDFN SET TMGDFN=$PIECE($GET(^TIU(8925,TIUIEN,0)),"^",2)
  . . . . NEW NAME SET NAME=$$GETNAME(TMGDFN)
  . . . . SET TMGRESULT(NAME,TEXT,TIUDATE)=""
  ;"Look for ICDs assigned during the visit
  ;"NEW ICDARRAY DO GETICDS(.ICDARRAY,BDATE,EDATE)
  NEW ICDARRAY,CPTARRAY DO GETCODES(.ICDARRAY,.CPTARRAY,BDATE,EDATE)
  NEW TMPPAT SET TMPPAT=""
  IF $D(ICDARRAY) DO
  . FOR  SET TMPPAT=$O(ICDARRAY(TMPPAT)) QUIT:TMPPAT=""  DO
  . . NEW ICDSTR SET ICDSTR=""
  . . FOR  SET ICDSTR=$O(ICDARRAY(TMPPAT,ICDSTR)) QUIT:ICDSTR=""  DO
  . . . NEW THISDATE SET THISDATE=$O(ICDARRAY(TMPPAT,ICDSTR,""))
  . . . IF (THISDATE>BDATE)&(THISDATE<EDATE) DO  ;"THISDATE[$$TODAY^TMGDATE DO  ;"only get ICDs for TODAY'S VISIT
  . . . . NEW ICDCODE SET ICDCODE=$P(ICDSTR," --",1)
  . . . . SET ICDCODE=$$UPDTICD(ICDCODE)
  . . . . SET TMGRESULT(TMPPAT,"Bill with ICD "_ICDCODE,THISDATE)=""
  ;"Look for CPTs assigned during the visit
  ;"NOTE:; The following pulls CPTs for the claims, however we aren't using this yet so commenting
  ;"       out until we start adding CPTs through CPRS
  ;"SET TMPPAT=""
  ;"IF $D(CPTARRAY) DO
  ;". FOR  SET TMPPAT=$O(CPTARRAY(TMPPAT)) QUIT:TMPPAT=""  DO
  ;". . NEW CPTSTR SET CPTSTR=""
  ;". . FOR  SET CPTSTR=$O(CPTARRAY(TMPPAT,CPTSTR)) QUIT:CPTSTR=""  DO
  ;". . . NEW THISDATE SET THISDATE=$O(CPTARRAY(TMPPAT,CPTSTR,""))
  ;". . . IF (THISDATE>BDATE)&(THISDATE<EDATE) DO  ;"THISDATE[$$TODAY^TMGDATE DO  ;"only get CPTs for TODAY'S VISIT
  ;". . . . NEW CPTCODE SET CPTCODE=$P(CPTSTR," --",1)
  ;". . . . ;"SET CPTCODE=$$UPDTCPT(CPTCODE)
  ;". . . . SET TMGRESULT(TMPPAT,"Bill with CPT "_CPTCODE,THISDATE)=""
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
  ;"This report will return all the ICDs for patients seen for the date range specified
  ;"NOTE: GETCODES below contains this code, and more...
  NEW IEN,TEXT,ICDIEN,ICDTEXT
  NEW TIUDATE,TIUIEN,FOUND
  SET TIUDATE=$PIECE(BDATE,".",1)
  NEW SDT,EDT
  SET SDT=BDATE-10000
  SET EDT=$PIECE(BDATE,".",1)_".999999"  ;"6/1/20 was originally set to BDATE
  FOR  SET TIUDATE=$ORDER(^TIU(8925,"D",TIUDATE)) QUIT:(TIUDATE>EDATE)!(TIUDATE'>0)  DO
  . SET TIUIEN=0
  . FOR  SET TIUIEN=$ORDER(^TIU(8925,"D",TIUDATE,TIUIEN)) QUIT:TIUIEN'>0  DO
  . . NEW TMGDFN SET TMGDFN=+$PIECE($GET(^TIU(8925,TIUIEN,0)),"^",2) QUIT:TMGDFN'>0
  . . NEW NAME SET NAME=$$GETNAME(TMGDFN)
  . . NEW ICDIEN SET ICDIEN=0
  . . FOR  SET ICDIEN=$ORDER(^AUPNVPOV("C",TMGDFN,ICDIEN)) QUIT:+ICDIEN'>0  DO
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
GETCODES(ICDARRAY,CPTARRAY,BDATE,EDATE)  ;"
  ;"This report will return all the ICDs for patients seen for the date range specified
  NEW IEN,TEXT,ICDIEN,ICDTEXT
  NEW TIUDATE,TIUIEN,FOUND
  SET TIUDATE=$PIECE(BDATE,".",1)
  NEW SDT,EDT
  SET SDT=BDATE-10000
  SET EDT=$PIECE(BDATE,".",1)_".999999"  ;"6/1/20 was originally set to BDATE
  FOR  SET TIUDATE=$ORDER(^TIU(8925,"D",TIUDATE)) QUIT:(TIUDATE>EDATE)!(TIUDATE'>0)  DO
  . SET TIUIEN=0
  . FOR  SET TIUIEN=$ORDER(^TIU(8925,"D",TIUDATE,TIUIEN)) QUIT:TIUIEN'>0  DO
  . . NEW TMGDFN SET TMGDFN=+$PIECE($GET(^TIU(8925,TIUIEN,0)),"^",2) QUIT:TMGDFN'>0
  . . NEW NAME SET NAME=$$GETNAME(TMGDFN)
  . . NEW ICDIEN SET ICDIEN=0
  . . FOR  SET ICDIEN=$ORDER(^AUPNVPOV("C",TMGDFN,ICDIEN)) QUIT:+ICDIEN'>0  DO
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
  . . . SET ICDARRAY(NAME,STR,VSTDT)=""
  . . NEW CPTIEN SET CPTIEN=0
  . . FOR  SET CPTIEN=$ORDER(^AUPNVCPT("C",TMGDFN,CPTIEN)) QUIT:+CPTIEN'>0  DO
  . . . NEW ZN SET ZN=$GET(^AUPNVCPT(CPTIEN,0)) QUIT:ZN=""
  . . . NEW VSTIEN SET VSTIEN=+$PIECE(ZN,"^",3) QUIT:VSTIEN'>0
  . . . NEW VSTZN SET VSTZN=$GET(^AUPNVSIT(VSTIEN,0)) QUIT:VSTZN=""
  . . . NEW VSTDT SET VSTDT=$PIECE(VSTZN,"^",1)
  . . . IF (VSTDT<SDT)!(VSTDT>EDT) QUIT  ;"Past this line, CPT is within daterange
  . . . NEW VCPTIEN SET VCPTIEN=+$PIECE(ZN,"^",1) QUIT:VCPTIEN'>0
  . . . NEW CPTZN SET CPTZN=$GET(^ICPT(VCPTIEN,0)) QUIT:CPTZN=""
  . . . NEW ADT SET ADT=$ORDER(^ICPT(VCPTIEN,62,"B",""),-1) QUIT:ADT'>0
  . . . NEW PTR SET PTR=$ORDER(^ICPT(VCPTIEN,62,"B",ADT,0)) QUIT:PTR'>0
  . . . NEW DESCR SET DESCR=$GET(^ICPT(VCPTIEN,62,PTR,1))
  . . . NEW STR SET STR=$PIECE(CPTZN,"^",1)_" -- "_DESCR
  . . . SET CPTARRAY(NAME,STR,VSTDT)=""
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
UPDTICD(ICDCODE)
  ;"THIS FUNCTION IS TO TRANSLATE OLD ICDS TO CURRENT CODES
  IF ICDCODE="M79.1" SET ICDCODE="M79.10"
  QUIT ICDCODE
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
  NEW COUNT SET COUNT=0
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
  . IF $$ACTIVEPT^TMGPXR03(PATIEN,1)<1 QUIT
  . NEW APPTDT SET APPTDT=NowDate-1
  . SET APPTDT=+$ORDER(^TMG(22723,PATIEN,1,"B",APPTDT))  
  . IF APPTDT>0 QUIT        ;"ELH ADDED 4/16/20
  . NEW APPTMSG,NEEDCALL
  . IF APPTDT>0 DO
  . . NEW Y SET Y=APPTDT
  . . DO DD^%DT
  . . SET APPTMSG="APPT SCHEDULED FOR "_Y
  . . SET NEEDCALL=0
  . ELSE  DO
  . . SET APPTMSG="**NO APPOINTMENT SCHEDULED**"
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
  . SET COUNT=COUNT+1
  WRITE "TOTAL PATIENTS: ",COUNT,!
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
  . WRITE NAME,!
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
GETNAME(TMGDFN)
  SET TMGDFN=+$GET(TMGDFN)
  QUIT $PIECE($GET(^DPT(TMGDFN,0)),"^",1)_" ("_$PIECE($GET(^DPT(TMGDFN,"TMG")),"^",2)_")"
  ;
ASKBMI
  ;"Interactive entry point for BMIMISSED
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
  . . . NEW TMGDFN SET TMGDFN=$PIECE(ZN,"^",2)
  . . . NEW NAME SET NAME=$$GETNAME(TMGDFN)
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
TMPLNAPT(TMGDFN)  ;"TIU TEMPLATE TO RETURN NEXT APPTS
  NEW TMGRESULT
  DO NEXTAPPT(.TMGRESULT,TMGDFN,0)
  IF TMGRESULT["UPCOMING APPTS:" SET TMGRESULT="Previously scheduled F/U appts: "_$P(TMGRESULT,"UPCOMING APPTS:",2)
  IF TMGRESULT["NO UPCOMING APPOINTMENT FOUND." SET TMGRESULT="Previously scheduled F/U appts: <NONE FOUND>"
  QUIT TMGRESULT
  ;"
NEXTAPPT(TMGRESULT,TMGDFN,TODAY)  ;"
  ;"ADDING TODAY (1 OR 0) AS A SWITCH FOR 2 DIFFERENT RESULTS (TODAY'S APPT OR FUTURE)
  ;"IF TODAY = 0, SHOW FUTURE. TODAY = 1, SHOW TODAY'S APPOINTMENT OR NOTHING
  SET TODAY=+$G(TODAY) ;"FORCE 0 
  NEW TODAYDATE SET TODAYDATE=$$TODAY^TMGDATE
  IF TODAY=0 DO
  . SET TMGRESULT="UPCOMING APPTS:"
  ELSE  DO
  . SET TMGRESULT=""
  NEW DATE,X,Y
  DO NOW^%DTC SET DATE=X
  NEW APPTCOUNT SET APPTCOUNT=0
  NEW DONE SET DONE=0
  FOR  SET DATE=$ORDER(^TMG(22723,TMGDFN,1,"B",DATE)) QUIT:(DATE'>0)!(DONE=1)  DO
  . NEW IDX SET IDX=$ORDER(^TMG(22723,TMGDFN,1,"B",DATE,0))
  . ;"10/2/20 - ONLY EXCLUDE CANCELLED IF $P($G(^TMG(22723,TMGDFN,1,IDX,0)),"^",7)'="A" QUIT
  . IF $P($G(^TMG(22723,TMGDFN,1,IDX,0)),"^",7)="C" QUIT
  . NEW REASON SET REASON=$P($G(^TMG(22723,TMGDFN,1,IDX,0)),"^",4)
  . NEW WITHDR SET WITHDR=$P($G(^TMG(22723,TMGDFN,1,IDX,0)),"^",3)
  . IF WITHDR=168 SET WITHDR="(KT)"
  . ELSE  IF WITHDR=83 SET WITHDR="(DT)"
  . ELSE  SET WITHDR=""
  . SET Y=DATE
  . DO DD^%DT
  . IF (TODAY=1)&($P(DATE,".",1)=TODAYDATE) DO
  . . IF TMGRESULT="" DO
  . . . SET TMGRESULT="Appt today at: "_$P(Y,"@",2)
  . . . SET DONE=1
  . . ELSE  DO
  . . . SET TMGRESULT=TMGRESULT_","_$P(Y,"@",2)
  . . SET TMGRESULT=TMGRESULT_WITHDR_" - "_REASON
  . ELSE  IF (TODAY=0)&($P(DATE,".",1)'=TODAYDATE) DO
  . . IF APPTCOUNT>0 SET TMGRESULT=TMGRESULT_";"
  . . SET TMGRESULT=TMGRESULT_" "_Y_WITHDR_" - "_REASON
  . . SET APPTCOUNT=APPTCOUNT+1
  IF (TODAY=0)&(APPTCOUNT=0) SET TMGRESULT="NO UPCOMING APPOINTMENT FOUND."
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
       SET REMARR(272)="TMG MAMMOGRAM FOR REPORT"
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
       . . NEW TMGDFN SET TMGDFN=0
       . . FOR  SET TMGDFN=$ORDER(RESULT(REMIEN,DATE,TMGDFN)) QUIT:TMGDFN'>0  DO
       . . . NEW REMSTR SET REMSTR=$G(RESULT(REMIEN,DATE,TMGDFN))
       . . . NEW DUEDATE SET DUEDATE=$P(REMSTR,"^",2)
       . . . K VADM
       . . . NEW AGE SET AGE=+$$AGE^TIULO(TMGDFN)
       . . . IF AGE>75 QUIT
       . . . NEW ACTIVE
       . . . DO RPCCKDUE^TMGTIU10(.ACTIVE,TMGDFN,1)
       . . . IF $P($G(ACTIVE(0)),"^",2)="-1" QUIT
       . . . IF $P($G(ACTIVE(0)),"^",1)="-1" QUIT
       . . . IF $$MAMORDRD(TMGDFN)=1 QUIT
       . . . NEW NAME SET NAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)_" "_$$AGE^TIULO(TMGDFN)
       . . . IF $$EXCLUDE($G(RESULT(REMIEN,DATE,TMGDFN)),.RESULTARR,NAME,DUEDATE)=1 QUIT
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
MAMORDRD(TMGDFN)  ;"Determine if patient has an outstanding mammogram ordered
       NEW TMGRESULT SET TMGRESULT=0
       NEW MammoIEN SET MammoIEN=+$ORDER(^GMR(123.5,"B","MAMMOGRAM",""))
       IF MammoIEN'>0 DO  GOTO MODN
       . WRITE "Can't locate record for MAMMOGRAM report.  Aborting.",!
       NEW ComplIEN SET ComplIEN=+$ORDER(^ORD(100.01,"B","COMPLETE",""))
       IF ComplIEN'>0 DO  GOTO MODN
       . WRITE "Can't find record for COMPLETE status.  Aborting.",!
       NEW DCIEN SET DCIEN=+$ORDER(^ORD(100.01,"B","DISCONTINUED",""))
       ;"
       NEW ORDIEN SET ORDIEN=0
       FOR  SET ORDIEN=$ORDER(^GMR(123,"F",TMGDFN,ORDIEN)) QUIT:ORDIEN'>0  DO
       . NEW ZN SET ZN=$GET(^GMR(123,ORDIEN,0))
       . NEW STATUS SET STATUS=$PIECE(ZN,"^",12)
       . IF STATUS=ComplIEN QUIT
       . IF STATUS=DCIEN QUIT
       . SET TMGRESULT=1      
MODN
       QUIT TMGRESULT

TICKLER(ROOT,TMGDFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) ;"Tickler report
  ;"Purpose: Entry point, as called from CPRS REPORT system
  ;"Input: ROOT -- Pass by NAME.  This is where output goes
  ;"       TMGDFN -- Patient DFN ; ICN for foriegn sites
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
  SET HD=HD_"</CAPTION><TR bgColor=#c4e3ed><TH>DUE DATE</TH><TH>STATUS</TH><TH>RECIPIENT</TH><TH>TICKLER MESSAGE</TH><TH>NOTE DATE</TH><TH>NOTE TITLE</TH></TR>"
  NEW DATA,TEMPARR,TIU,STATUS,USER,DUE,IDX,TIUDATE,TIUNAME
  FOR  SET TICKLIEN=$O(^TMG(22705.5,"B",TMGDFN,TICKLIEN)) QUIT:TICKLIEN'>0  DO
  . NEW ZN SET ZN=$G(^TMG(22705.5,TICKLIEN,0))
  . SET TIU=$P(ZN,"^",4),STATUS=$P(ZN,"^",3),USER=$P(ZN,"^",5),DUE=$P(ZN,"^",2)
  . IF +$G(TIU)'>0 QUIT
  . SET TIUDATE=$P($G(^TIU(8925,TIU,0)),"^",7)
  . SET TIUDATE=$$NOTEHREF^TMGTIUOJ(TMGDFN,$P(TIUDATE,".",1),$$EXTDATE^TMGDATE(TIUDATE))
  . IF TIUDATE["Select^" SET TIUDATE=$TR(TIUDATE,"Select^","Select@")
  . ;"SET TIUDATE=$$EXTDATE^TMGDATE($P($G(^TIU(8925,TIU,0)),"^",7))
  . SET TIUNAME=$P($G(^TIU(8925.1,$P($G(^TIU(8925,TIU,0)),"^",1),0)),"^",1)
  . NEW OVERDUE SET OVERDUE=$$OVERDUE(STATUS,DUE)
  . NEW MESSAGE SET MESSAGE=$$FLMSG^TMGTICK2(TICKLIEN)
  . IF MESSAGE="" SET MESSAGE="----NO MESSAGE FOUND----"
  . SET TEMPARR(DUE,TIU,OVERDUE)=$$TSTATUS(STATUS)_"^"_$P($G(^VA(200,USER,0)),"^",1)_"^"_MESSAGE_"^"_TIUDATE_"^"_TIUNAME
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
  DO SETHTML(.ROOT,.DATA,"PATIENT TICKLER MESSAGES",HD,6)
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
TOPICS(ROOT,TMGDFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) ;"TOPICS report
  ;"Purpose: Entry point, as called from CPRS REPORT system
  ;"Input: ROOT -- Pass by NAME.  This is where output goes
  ;"       TMGDFN -- Patient DFN ; ICN for foriegn sites
  ;"       ID --
  ;"       ALPHA -- Start date (lieu of DTRANGE)
  ;"       OMEGA -- End date (lieu of DTRANGE)
  ;"       DTRANGE -- # days back from today
  ;"       REMOTE --
  ;"       MAX    --
  ;"       ORFHIE --
  KILL @ROOT
  NEW TICKLIEN,ARRAY SET TICKLIEN=0
  NEW HD SET HD="<TABLE BORDER=3><CAPTION><B>PATIENT TOPICS</B>"
  ;"SET HD=HD_"<BR> FAMILY PHYSICIANS OF GREENEVILLE<BR>1410 TUSCULUM BLVD  STE. 2600 <BR>"
  ;"SET HD=HD_" GREENEVILLE, TN 37745
  SET HD=HD_"</CAPTION><TR bgColor=#c4e3ed><TH>TOPIC</TH><TH>LAST DISCUSSED</TH><TH>NOTE TITLE</TH></TR>"  ;"<TH>TOPIC TEXT</TH></TR>"
  NEW DATA,TOPICARR,TIU,TOPIC,IDX,TEMPARR
  DO TOPICS^TMGTIUT3(.TOPICARR,"LIST",TMGDFN,"HPI")
  SET IDX=0
  FOR  SET IDX=$O(TOPICARR(IDX)) QUIT:IDX'>0  DO
  . ;"TOPIC SHOULD BE SORTED BY TOPIC AND THEN ASCENDING BY TIUIEN, SO NO NEED
  . ;"TO COMPARE DATES
  . NEW NODE SET NODE=$G(TOPICARR(IDX))
  . SET TOPIC=$P(NODE,"^",1)
  . NEW TIUTITLE SET TIUTITLE=$P($G(^TIU(8925,$P(NODE,"^",2),0)),"^",1)
  . NEW UPTOPIC SET UPTOPIC=$$UP^XLFSTR(TOPIC)
  . IF $D(TEMPARR(UPTOPIC)) DO  ;"CHECK FOR TOPIC, IN UPPERCASE FORM
  . . NEW OLDDT SET OLDDT=$P($G(TEMPARR(UPTOPIC)),"^",1)
  . . IF $P(NODE,"^",3)>OLDDT DO
  . . . SET TEMPARR(UPTOPIC)=$P(NODE,"^",3)_"^"_$P($G(^TIU(8925.1,TIUTITLE,0)),"^",1)
  . ELSE  DO
  . . SET TEMPARR(UPTOPIC)=$P(NODE,"^",3)_"^"_$P($G(^TIU(8925.1,TIUTITLE,0)),"^",1)
  SET TOPIC="",IDX=1
  FOR  SET TOPIC=$O(TEMPARR(TOPIC)) QUIT:TOPIC=""  DO
  . IF TOPIC["=== HPI ISSUES BELOW " QUIT
  . IF (TOPIC="")!(TOPIC=" ") QUIT
  . SET DATA(IDX)=TOPIC_"^"_$$EXTDATE^TMGDATE($P($G(TEMPARR(TOPIC)),"^",1),1)_"^"_$P($G(TEMPARR(TOPIC)),"^",2),IDX=IDX+1  ;"_"^TEXT TO BE INSERTED HERE LATER"
  . ;"$$EXTDATE^TMGDATE($P(NODE,"^",3),1)
  . ;"DO TOPICS^TMGTIUT3(.TOPICARR,"SUM1",TMGDFN,"HPI")  <-  
  DO SETHTML(.ROOT,.DATA,"PATIENT TOPICS",HD,3)
  QUIT
  ;"  
CPTIIRPT
  ;"This report is designed to return all patients, over 65
  ;"with any metrics they have had for the current month.
  ;"
  ;"The array "TABLEARR" should contain the name of all 
  ;"TMG TIU PXRM tables that are to be used for this report.
  ;"
  ;"This report currently handles lab and HF tables, but not inline at
  ;"this time.
  ;"
  NEW %ZIS,IOP
  SET IOP="S121-LAUGHLIN-LASER"
  DO ^%ZIS  ;"standard device call
  IF POP DO  GOTO NRDn
  . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output. Aborting.")
  USE IO
TEST
  ;"get month name and beginning date
  NEW BEGINDATE SET BEGINDATE=$$TODAY^TMGDATE
  SET BEGINDATE=$E(BEGINDATE,1,5)_"01.000000"
  ;"SET BEGINDATE=3200601.0000
  SET BEGINDATE=$$ADDDAYS^TMGDATE(-7)
  ;"SET BEGINDATE=3200401.0000
  ;"header
  WRITE !
  WRITE "************************************************************",!
  ;"WRITE "              INSURANCE METRICS FOR ",$$MNTHNAME^TMGDATE(BEGINDATE),!
  WRITE "              INSURANCE METRICS FOR LAST 7 DAYS",!
  WRITE "************************************************************",!
  WRITE "                                            (From TMGRPT2.m)",!!
  ;"GET TABLE DATA
  NEW TABLEARR,DATAARR,TMGDFN,X,THISTABLE
  SET TABLEARR("INSURANCE METRICS")=""
  SET TABLEARR("INSURANCE METRICS2")=""
  SET TMGDFN=0
  FOR  SET TMGDFN=$O(^DPT(TMGDFN)) QUIT:TMGDFN'>0  DO
  . ;"
  . ;"IF $$EXCLUDE^TMGC0QTU(TMGDFN) QUIT
  . IF $$HASNOTES(TMGDFN,BEGINDATE)=0 QUIT
  . IF $$ACTIVEPT^TMGPXR03(TMGDFN)'=1 QUIT
  . NEW NAME SET NAME=$P($G(^DPT(TMGDFN,0)),"^",1)
  . NEW DOB SET DOB=$P($G(^DPT(TMGDFN,0)),"^",3)
  . SET NAME=NAME_" ("_$$EXTDATE^TMGDATE(DOB)_") "
  . SET NAME=NAME_$P($G(^DPT(TMGDFN,"TMG")),"^",2)
  . ;"Include DC/Med Recs regardless of age
  . NEW FOUND,DATESTR
  . SET FOUND=$$CHKNOTE(TMGDFN,2031,BEGINDATE,.DATESTR,"1111F",0)
  . IF FOUND=1 DO
  . . ;"CYCLE THROUGH DATES HERE 
  . . NEW IDX SET IDX=0
  . . FOR  SET IDX=$O(DATESTR(IDX)) QUIT:IDX'>0  DO
  . . . NEW ITEMNAME SET ITEMNAME="HOSPITAL DC-MED RECONCILIATION "_IDX
  . . . SET DATAARR(NAME,ITEMNAME)=$G(DATESTR(IDX))
  . ;"Anything above this section will be included regardless of age
  . ;"
  . ;"NEW AGE K VADM SET AGE=$$AGE^TIULO(TMGDFN)
  . ;"IF AGE<65 QUIT
  . IF $$EXCLUDE^TMGC0QTU(TMGDFN) QUIT
  . SET THISTABLE=""
  . FOR  SET THISTABLE=$O(TABLEARR(THISTABLE)) QUIT:THISTABLE=""  DO
  . . NEW OUTTABLE
  . . S X=$$GETTABLX^TMGTIUO6(TMGDFN,THISTABLE,.OUTTABLE)
  . . ;"ZWR OUTTABLE
  . . NEW KEY,VALUE SET KEY=""
  . . FOR  SET KEY=$O(OUTTABLE("KEY-VALUE",KEY)) QUIT:KEY=""  DO
  . . . IF KEY="TOBACCO SCREENING" QUIT   ;"DON'T INCLUDE FOR NOW
  . . . SET VALUE=$G(OUTTABLE("KEY-VALUE",KEY))
  . . . IF VALUE'="" DO
  . . . . NEW RESULT,FMDATE,X,Y
  . . . . IF VALUE[" on " DO
  . . . . . SET RESULT=$P(VALUE," on ",1)
  . . . . . SET X=$P(VALUE," on ",2) DO ^%DT
  . . . . ELSE  DO
  . . . . . SET X=$P(VALUE," ",1)
  . . . . . SET RESULT=$P(VALUE," ",2,999)
  . . . . DO ^%DT
  . . . . SET FMDATE=Y
  . . . . IF FMDATE>BEGINDATE DO  
  . . . . . NEW CPTICD,REPORTED
  . . . . . SET REPORTED=$$GETCPT(TMGDFN,KEY,VALUE,.CPTICD,FMDATE)
  . . . . . ;"TO CUT THESE OUT - SET BELOW TO "
  . . . . . IF REPORTED=0 DO
  . . . . . . SET DATAARR(NAME,KEY)=RESULT_" on "_X_" "_CPTICD
  . ;"
  . ;"CHECK FOR NOTE TITLES <- this needs to be moved to database later.
  . ;"                         hardcoded for now.
  . NEW FOUND,DATESTR
  . SET FOUND=$$CHKNOTE(TMGDFN,63,BEGINDATE,.DATESTR,"3066F",0)
  . IF FOUND=1 DO
  . . NEW IDX SET IDX=0
  . . FOR  SET IDX=$O(DATESTR(IDX)) QUIT:IDX'>0  DO
  . . . NEW TEMPNAME SET TEMPNAME="NEPHROLOGY VISIT "_IDX
  . . . SET DATAARR(NAME,TEMPNAME)=DATESTR
  . ;"
  . NEW ACEARB,MEDSTR
  . SET ACEARB=$$CHKMEDS(TMGDFN,49,.MEDSTR,"4010F")
  . ;"  ELH    1/20/23 - don't include  IF ACEARB=1 SET DATAARR(NAME,"ACE/ARB MEDICATION")=MEDSTR_" (JUST CHECK FOR NOW)"
  . ;"
  . ;"Get AtHome BPs
  ;"KILL DATAARR
  DO HOMEBPS(BEGINDATE,.DATAARR)  
  ;"
  ;"PRINT REPORT BELOW
  DO CLEANUP(.DATAARR)
  NEW NAME SET NAME=""
  NEW COUNT SET COUNT=0
  FOR  SET NAME=$O(DATAARR(NAME)) QUIT:NAME=""  DO
  . SET COUNT=COUNT+1
  . NEW KEY SET KEY=""
  . WRITE "-- ",NAME," --------",!
  . FOR  SET KEY=$O(DATAARR(NAME,KEY)) QUIT:KEY=""  DO 
  . . W KEY,": ",$G(DATAARR(NAME,KEY)),!
  . W !
  WRITE "TOTAL COUNT: ",COUNT,!
NRDn
  DO ^%ZISC  ;" Close the output device
  QUIT
  ;"
TNCARRPT(DATE)  ;"
  ;"GET SCHEDULE
  NEW SCHARRAY
  NEW ONEWEEK SET ONEWEEK=$$ADDDAYS^TMGDATE(7,DATE)
  DO GETSCHED^TMGPXR03(.SCHARRAY,DATE)
  DO GETSCHED^TMGPXR03(.SCHARRAY,ONEWEEK)
  ;"
  ;"LOAD INSURANCES TO CHECK ARRAY
  NEW INSARRAY
  SET INSARRAY(40)="AMERIGROUP"
  SET INSARRAY(7)="AMERICHOICE"
  SET INSARRAY(20)="BLUECARE"
  SET INSARRAY(25)="BLUECARE+"
  SET INSARRAY(14)="UH-COMMUNITY"
  SET INSARRAY(78)="TNCARE SELECT"
  SET INSARRAY(68)="BLUESELECT"
  ;"
  ;"CHECK FOR PATIENTS WITH MATCHING INSURANCES
  NEW TMGDFN SET TMGDFN=0
  NEW PTCHECKARR
  FOR  SET TMGDFN=$O(SCHARRAY(TMGDFN)) QUIT:TMGDFN'>0  DO
  . NEW INSIEN SET INSIEN=0
  . FOR  SET INSIEN=$O(^DPT(TMGDFN,.312,"B",INSIEN)) QUIT:INSIEN'>0  DO
  . . IF $D(INSARRAY(INSIEN)) DO
  . . . NEW DOS SET DOS=$O(SCHARRAY(TMGDFN,""))
  . . . SET DOS=$$EXTDATE^TMGDATE(DOS)
  . . . SET PTCHECKARR(TMGDFN)=$P($G(^DPT(TMGDFN,0)),"^",1)_" ("_DOS_") has "_$P($G(^DIC(36,INSIEN,0)),"^",1)
  IF $D(PTCHECKARR) DO
  . WRITE !
  . WRITE "************************************************************",!
  . WRITE "              PATIENTS WITH INSURANCES TO BE VERIFIED",!
  . WRITE "              APPOINTMENT DATE: ",$$EXTDATE^TMGDATE(DATE)," OR ",$$EXTDATE^TMGDATE(ONEWEEK),!
  . WRITE "************************************************************",!
  . WRITE "                                            (From TMGRPT2.m)",!!
  . SET TMGDFN=0
  . FOR  SET TMGDFN=$O(PTCHECKARR(TMGDFN)) QUIT:TMGDFN'>0  DO
  . . WRITE $G(PTCHECKARR(TMGDFN)),!,!
  . . WRITE "[  ] ___________________________________",!,!
  QUIT
  ;"
CLEANUP(DATAARR)  ;"
  NEW NAME,KEY,VALUE
  SET NAME=""
  FOR  SET NAME=$O(DATAARR(NAME)) QUIT:NAME=""  DO
  . SET KEY=""
  . FOR  SET KEY=$O(DATAARR(NAME,KEY)) QUIT:KEY=""  DO
  . . SET VALUE=$G(DATAARR(NAME,KEY))
  . . IF VALUE["ALREADY REPORTED" DO
  . . . KILL DATAARR(NAME,KEY)
  QUIT
  ;"
HOMEBPS(BDATE,DATAARR)  
  ;"Purpose: get the at home blood pressure readings
  NEW BPNOTEIEN SET BPNOTEIEN=2066  ;"HARD CODED FOR NOTE IEN
  NEW TIUIEN SET TIUIEN=0
  FOR  SET TIUIEN=$O(^TIU(8925,"B",BPNOTEIEN,TIUIEN)) QUIT:TIUIEN'>0  DO
  . NEW ZN SET ZN=$G(^TIU(8925,TIUIEN,0))
  . NEW DATE SET DATE=$P(ZN,"^",7)
  . IF BDATE>DATE QUIT
  . NEW TMGDFN SET TMGDFN=$P(ZN,"^",2)
  . NEW NAME SET NAME=$P($G(^DPT(TMGDFN,0)),"^",1)
  . NEW NOTETEXT SET NOTETEXT=$$NOTETEXT(TIUIEN)
  . NEW BP SET BP=$$TRIM^XLFSTR($P($P(NOTETEXT,"BP",2),";",1))
  . NEW SYS,DIA,SYSSTR,DIASTR SET (SYSSTR,DIASTR)=""
  . SET BP=$$STRIPTAG^TMGHTM1(BP)
  . IF BP[":" SET BP=$TR(BP,":"," ")
  . SET BP=$$TRIM^XLFSTR(BP)
  . SET SYS=+$P(BP,"/",1),DIA=+$P(BP,"/",2)
  . IF SYS>0 DO
  . . IF SYS<130 DO
    . . . SET SYSSTR="3074F"
  . . ELSE  IF SYS<140 DO
  . . . SET SYSSTR="3075F"
  . . ELSE  IF SYS>139 DO
  . . . SET SYSSTR="3077F"
  . . IF SYSSTR'="" DO
  . . . IF $$HASCPT^TMGRPU1(TMGDFN,SYSSTR,$P(DATE,".",1),$P(DATE,".",1)) SET SYSSTR=SYSSTR_" (ALREADY REPORTED)"
  . . . SET DATAARR(NAME,"AT HOME SYSTOLIC BP ON "_$$EXTDATE^TMGDATE(DATE))="CPT: "_SYSSTR
  . ELSE  SET DATAARR(NAME,"COULD NOT FIND SYSTOLIC ON "_$$EXTDATE^TMGDATE(DATE))=""
  . IF DIA>0 DO
  . . IF DIA<80 DO
  . . . SET DIASTR="3078F"
  . . ELSE  IF DIA<90 DO
  . . . SET DIASTR="3079F"
  . . IF DIASTR'="" DO
  . . . IF $$HASCPT^TMGRPU1(TMGDFN,DIASTR,$P(DATE,".",1),$P(DATE,".",1)) SET DIASTR=DIASTR_" (ALREADY REPORTED)"
  . . . SET DATAARR(NAME,"AT HOME DIASTOLIC BP ON "_$$EXTDATE^TMGDATE(DATE))="CPT: "_DIASTR
  . ELSE  SET DATAARR(NAME,"COULD NOT FIND DIASTOLIC ON "_$$EXTDATE^TMGDATE(DATE))=""
  QUIT
  ;"
NOTETEXT(TIUIEN)  ;"RETURNS TEXT OF A NOTE IN A STRING
  ;"THIS FUNCTION EXISTS ELSEWHERE BUT I AM UNSURE WHERE SO
  ;"FASTER TO REWRITE THAN SEARCH
  NEW STRING,IDX SET IDX=0,STRING=""
  FOR  SET IDX=$O(^TIU(8925,TIUIEN,"TEXT",IDX)) QUIT:IDX'>0  DO
  . SET STRING=STRING_$G(^TIU(8925,TIUIEN,"TEXT",IDX,0))
  QUIT STRING
  ;"
GETCPT(TMGDFN,KEY,VALUE,TMGRESULT,DATE) 
  ;"This function returns the CPT and ICD codes
  ;"to be documented. This needs to be added to a file at a later date
  SET TMGRESULT=""
  NEW REPORTED SET REPORTED=0
  ;"SET TMGRESULT="(CPT #####,ICD ZZ.####)"
  NEW TEMPVAL SET TEMPVAL=+VALUE
  NEW CPT,ICD SET CPT="",ICD=""
  ;"LABS
  IF KEY="A1C" DO
  . IF TEMPVAL<7 SET CPT="3044F"
  . ;"IF (TEMPVAL<9.1)&(TEMPVAL>6.9) SET CPT="3045F"
  . IF (TEMPVAL<8)&(TEMPVAL>6.9) SET CPT="3051F"
  . IF (TEMPVAL<9.1)&(TEMPVAL>7.9) SET CPT="3052F"
  . IF TEMPVAL>9 SET CPT="3046F"
  IF KEY="LDL" DO
  . IF TEMPVAL<100 SET CPT="3048F"
  . IF (TEMPVAL>99)&(TEMPVAL<131) SET CPT="3049F"
  . IF TEMPVAL>130 SET CPT="3050F"
  IF KEY="MALB" DO
  . IF VALUE["H" SET CPT="3060F"
  . ELSE  SET CPT="3061F"
  ;"Health Factors
  IF KEY="CP RECORDED" SET CPT="1157F"
  IF KEY="NONUSER" SET CPT="1036F"
  IF KEY="TOBACCO SCREENING" SET CPT="4004F"
  IF KEY="TOBACCO COUNSELING" SET CPT="4000F"
  IF KEY="SMOKER" SET CPT="1034F"
  IF KEY="SMOKELESS" SET CPT="1035F"
  IF KEY="TOBACCO USER" SET CPT="1032F"
  IF KEY="DC MED RECONCILED" SET CPT="1111F"
  IF KEY="DEPRESSION SCREENING" SET CPT="3725F"
  IF KEY="DILATED EYE EXAM" SET CPT="2022F"
  IF KEY="FALL RISK, 0-1" SET CPT="1101F"
  IF KEY="FALL RISK, 2+" SET CPT="1100F"
  IF KEY="IFOBT" SET CPT="3017F"
  ;"
  ;"TEST CPT
  IF CPT="" QUIT REPORTED
  NEW BEGINDATE SET BEGINDATE=$E(DATE,1,3)_"0101"
  IF +$$HASCPT^TMGRPU1(TMGDFN,CPT,BEGINDATE,9999999)=1 SET REPORTED=1
  IF CPT'="" SET TMGRESULT="CPT: "_CPT_" "
  IF ICD'="" SET TMGRESULT=TMGRESULT_"ICD: "_ICD
  IF (CPT="3044F")!(CPT="3045F")!(CPT="3046F")!(CPT="3051F")!(CPT="3052F") DO
  . SET REPORTED=0
  . IF +$$HASCPT^TMGRPU1(TMGDFN,CPT,$P(FMDATE,".",1)_".0000",$P(FMDATE,".",1)_".999999")=1 SET TMGRESULT=TMGRESULT_" (ALREADY REPORTED)",REPORTED=1
  ELSE  DO
  . IF +$$HASCPT^TMGRPU1(TMGDFN,CPT,BEGINDATE,9999999)=1 SET TMGRESULT=TMGRESULT_" (ALREADY REPORTED)"
  QUIT REPORTED
  ;"  
CHKNOTE(TMGDFN,NOTEIEN,BEGINDATE,DATESTR,CPT,EXCLUDEFOUND)
  ;"Purpose: This function goes through a patient's notes and tries to find
  ;"         whether the given note exists
  ;"Input: TMGDFN - Patient's IEN
  ;"       NOTEIEN - The IEN of the note to look for
  ;"       BEGINDATE - The date to start looking 
  ;"       DATESTR - Return value. This will be returned one of 2 ways
  ;"                 depending on the value of EXCLUDEFOUND
  ;"                 If EXCLUDEFOUND=0, then multiple values will be returned
  ;"                    DATESTR(IDX)=" on <Date> CPT: <CPT>"
  ;"                 If EXCLUDEFOUND=1, then a single value will be returned
  ;"                    DATESTR=" on <Date> CPT: <CPT>"
  ;"       CPT - This is the CPT value to assign when a note is found
  ;"       EXCLUDEFOUND - Default is 0. When set to 0, values will be return
  ;"                      even if the CPT code is found to have been billed
  ;"                      before. This is for codes that are billed
  ;"                      everytime a note if found. Multiple values will
  ;"                      also be returned.
  ;"                      When set to 1, it will not return anything if the
  ;"                      CPT code is found. These are for yearly billed
  ;"                      codes that shouldn't be billed every time.
  ;"                 
  NEW DATE,RESULT
  SET DATE=0,RESULT=0
  SET EXCLUDEFOUND=+$G(EXCLUDEFOUND)
  IF (EXCLUDEFOUND=1)&($$HASCPT^TMGRPU1(TMGDFN,CPT,$$FIRSTYR^TMGDATE,9999999)) GOTO CHDN
  NEW IDX SET IDX=1
  FOR  SET DATE=$O(^TIU(8925,"AA",TMGDFN,NOTEIEN,DATE)) QUIT:DATE'>0  DO
  . NEW NEWDATE SET NEWDATE=9999999-DATE
  . IF NEWDATE<$$FIRSTYR^TMGDATE QUIT
  . IF EXCLUDEFOUND=0 DO
  . . SET DATESTR(IDX)=" on "_$$EXTDATE^TMGDATE(NEWDATE)_" CPT: "_CPT
  . . IF $$HASCPT^TMGRPU1(TMGDFN,CPT,NEWDATE,NEWDATE) SET DATESTR(IDX)=$G(DATESTR(IDX))_" (ALREADY REPORTED)"
  . . SET IDX=IDX+1
  . ELSE  DO
  . . SET DATESTR=" on "_$$EXTDATE^TMGDATE(NEWDATE)_" CPT: "_CPT
  . SET RESULT=1
CHDN
  QUIT RESULT
  ;"
CHKMEDS(TMGDFN,LISTIEN,DATESTR,CPT)
  NEW RESULT SET RESULT=0,DATESTR=""
  IF $$HASCPT^TMGRPU1(TMGDFN,CPT,$$FIRSTYR^TMGDATE,9999999) GOTO CMDN
  IF LISTIEN'>0 GOTO CMDN
  NEW MEDLISTSTRING,OUTARR,OPTION
  SET MEDLISTSTRING=$$UP^XLFSTR($$DOTABL^TMGPXR02(TMGDFN,"[MEDICATIONS]","GETITEM^TMGTIUO8",.OUTARR,.OPTION))  ;GET MEDLIST INTO ONE STRING
  NEW MEDNAME SET MEDNAME=""
  NEW FOUND SET FOUND=0
  IF $D(TMGPRIORTABLE) KILL TMGPRIORTABLE
  ;"WRITE TMGDFN,!
  FOR  SET MEDNAME=$O(^TMG(22708,LISTIEN,3,"B",MEDNAME)) QUIT:(MEDNAME="")!(FOUND=1)  DO
  . IF MEDLISTSTRING[$$UP^XLFSTR(MEDNAME) DO
  . . ;"w MEDNAME," was found in ",!
  . . ;"w MEDLISTSTRING,!,!
  . . SET DATESTR=" patient on "_MEDNAME_" CPT: "_CPT
  . . SET RESULT=1
  . . SET FOUND=1
CMDN
  QUIT RESULT
  ;"
SMOKER(OUTARR)  ;
  SET TMGY=""
  NEW FORMER SET FORMER=$G(OUTARR("KEY-VALUE","FORMER TOBACCO USER"))
  SET FORMER=+$$INTDATE^TMGDATE(FORMER)
  NEW CURRENT SET CURRENT=$G(OUTARR("KEY-VALUE","CURRENT TOBACCO USER"))
  SET CURRENT=+$$INTDATE^TMGDATE(CURRENT)
  IF FORMER<1 QUIT
  IF CURRENT<FORMER SET TMGY="NOT A SMOKER"    
  QUIT
  ;"
HASNOTES(TMGDFN,BEGINDATE)  ;"This function returns 0 (if none) or date of note
                         ;"the patient had after BEGINDATE
  NEW TMGRESULT SET TMGRESULT=0
  ;"NEW DATE SET DATE=$P(BEGINDATE,".",1)
  NEW DATE SET DATE=$E(BEGINDATE,1,3)_"0101"
  SET DATE=$O(^TIU(8925,"ZTMGPTDT",TMGDFN,DATE))
  IF DATE>0 DO
  . SET TMGRESULT=1
  QUIT TMGRESULT
  ;"
APPTTIME(BDATE,EDATE)    ;"
  ;"This function will calculate the wait times for a given time period.
  NEW %ZIS,IOP
  SET IOP="S121-LAUGHLIN-LASER"
  DO ^%ZIS  ;"standard device call
  IF POP DO  GOTO ATDn
  . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output. Aborting.")
  USE IO
  WRITE !
  WRITE "************************************************************",!
  WRITE "               WAIT TIMES FOR APPOINTMENTS FROM ",!
  WRITE "               ",$$EXTDATE^TMGDATE(BDATE)," TO ",$$EXTDATE^TMGDATE(EDATE),!
  WRITE "************************************************************",!
  WRITE "                                            (From TMGRPT2.m)",!!
  NEW DATE SET DATE=$P(BDATE,".",1)
  NEW DAYNUM,DAYTOT,RPTNUM,RPTTOT  ;"Used to report totals
  SET (DAYNUM,DAYTOT,RPTNUM,RPTTOT)=0
  SET EDATE=$P(EDATE,".",1)_".9999"
  NEW THISDAY SET THISDAY=0
  WRITE "PATIENT NAME",?25,"TIME TO TRIAGE",?45,"TIME IN ROOM",?65,"TOTAL TIME",!
  FOR  SET DATE=$O(^TMG(22723,"DT",DATE)) QUIT:(DATE>EDATE)!(DATE'>0)  DO
  . NEW TMGDFN SET TMGDFN=0
  . IF THISDAY'=$P(DATE,".",1) DO
  . . SET THISDAY=$P(DATE,".",1)
  . . IF DAYNUM>0 DO
  . . . WRITE "  *AVERAGE WAIT FOR THIS DAY: ",$J(DAYTOT/DAYNUM,9,2)," MINUTES",!,!
  . . WRITE "-------- DATE: ",$$EXTDATE^TMGDATE(THISDAY)," --------",!
  . . SET DAYNUM=0,DAYTOT=0
  . FOR  SET TMGDFN=$O(^TMG(22723,"DT",DATE,TMGDFN)) QUIT:TMGDFN'>0  DO
  . . NEW SUBIEN SET SUBIEN=0
  . . FOR  SET SUBIEN=$O(^TMG(22723,"DT",DATE,TMGDFN,SUBIEN)) QUIT:SUBIEN'>0  DO
  . . . NEW STATUS SET STATUS=$G(^TMG(22723,"DT",DATE,TMGDFN,SUBIEN))
  . . . IF STATUS="C" QUIT
  . . . NEW ZN,CHKIN,CHKOUT,DIFF
  . . . SET ZN=$G(^TMG(22723,TMGDFN,1,SUBIEN,0))
  . . . SET CHKIN=+$P(ZN,"^",8),CHKOUT=+$P(ZN,"^",9)
  . . . IF (CHKIN'>0)!(CHKOUT'>0) QUIT
  . . . ;"
  . . . ;"This section will review the note created for the visit
  . . . NEW NOTEDT,NOTEDIFF
  . . . SET NOTEDT=$O(^TIU(8925,"ZTMGPTDT",TMGDFN,CHKIN))
  . . . IF NOTEDT[$P(CHKIN,".",1) DO
  . . . . SET NOTEDT=$J(NOTEDT,7,4)
  . . . . SET CHKIN=$J(CHKIN,7,4)
  . . . . SET CHKOUT=$J(CHKOUT,7,4)
  . . . . ;"WRITE CHKIN," - ",NOTEDT," - ",CHKOUT," = "
  . . . . SET NOTEDIFF=$$TIMEDIFF^TMGDATE(NOTEDT,CHKIN)
  . . . . ;"WRITE NOTEDIFF,!
  . . . ELSE  DO
  . . . . SET NOTEDIFF="??"
  . . . ;"
  . . . SET DIFF=$$TIMEDIFF^TMGDATE(CHKOUT,CHKIN)
  . . . NEW NOTE2CHKOUT 
  . . . IF NOTEDIFF="??" DO
  . . . . SET NOTE2CHKOUT="??"
  . . . ELSE  DO
  . . . . SET NOTE2CHKOUT=DIFF-NOTEDIFF
  . . . WRITE $P($G(^DPT(TMGDFN,0)),"^",1),?25,NOTEDIFF," MINS",?45,NOTE2CHKOUT," MINS",?65,DIFF," MINS",!
  . . . SET DAYNUM=DAYNUM+1,DAYTOT=DAYTOT+DIFF,RPTNUM=RPTNUM+1,RPTTOT=RPTTOT+DIFF
  IF DAYNUM>0 DO
  . WRITE "  *AVERAGE WAIT FOR THIS DAY: ",$J(DAYTOT/DAYNUM,9,2)," MINUTES",!,!
  WRITE "  ****AVERAGE WAIT PER PATIENT: ",$J(RPTTOT/RPTNUM,9,2)," MINUTES",!
ATDn
  DO ^%ZISC  ;" Close the output device
  QUIT
  ;"
APTTIME2(BDATE,EDATE)    ;"
  ;"This function will calculate the wait times for a given time period.
  NEW %ZIS,IOP
  SET IOP="S121-LAUGHLIN-LASER"
  DO ^%ZIS  ;"standard device call
  IF POP DO  GOTO AT2Dn
  . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output. Aborting.")
  USE IO
  WRITE !
  WRITE "************************************************************",!
  WRITE "               VISIT TIMES FOR APPOINTMENTS FROM ",!
  WRITE "               ",$$EXTDATE^TMGDATE(BDATE)," TO ",$$EXTDATE^TMGDATE(EDATE),!
  WRITE "************************************************************",!
  WRITE "                                            (From TMGRPT2.m)",!!
  NEW DATE SET DATE=$P(BDATE,".",1)
  NEW DAYNUM,DAYTOT,RPTNUM,RPTTOT  ;"Used to report totals
  SET (DAYNUM,DAYTOT,RPTNUM,RPTTOT)=0
  SET EDATE=$P(EDATE,".",1)_".9999"
  NEW THISDAY SET THISDAY=0
  WRITE "PATIENT NAME",!
  WRITE ?2,"CHECK-IN",?15,"CHECK-OUT",?26,"FIRST TIMER",?39,"LAST TIMER",?54,"NOTE START",?67,"NOTE SIGN",!
  FOR  SET DATE=$O(^TMG(22723,"DT",DATE)) QUIT:(DATE>EDATE)!(DATE'>0)  DO
  . NEW TMGDFN SET TMGDFN=0
  . IF THISDAY'=$P(DATE,".",1) DO
  . . SET THISDAY=$P(DATE,".",1)
  . . IF DAYNUM>0 DO
  . . . WRITE "  *AVERAGE WAIT FOR THIS DAY: ",$J(DAYTOT/DAYNUM,9,2)," MINUTES",!,!
  . . WRITE "-------- DATE: ",$$EXTDATE^TMGDATE(THISDAY)," --------",!
  . . SET DAYNUM=0,DAYTOT=0
  . FOR  SET TMGDFN=$O(^TMG(22723,"DT",DATE,TMGDFN)) QUIT:TMGDFN'>0  DO
  . . NEW SUBIEN SET SUBIEN=0
  . . FOR  SET SUBIEN=$O(^TMG(22723,"DT",DATE,TMGDFN,SUBIEN)) QUIT:SUBIEN'>0  DO
  . . . NEW STATUS SET STATUS=$G(^TMG(22723,"DT",DATE,TMGDFN,SUBIEN))
  . . . IF STATUS="C" QUIT
  . . . NEW ZN,CHKIN,CHKOUT,DIFF
  . . . SET ZN=$G(^TMG(22723,TMGDFN,1,SUBIEN,0))
  . . . SET CHKIN=+$P(ZN,"^",8),CHKOUT=+$P(ZN,"^",9)
  . . . IF (CHKIN'>0)!(CHKOUT'>0) QUIT
  . . . ;"
  . . . ;"This section will review the note created for the visit
  . . . NEW NOTEDT,NOTEDIFF,NOTESIGN
  . . . SET NOTEDT=$O(^TIU(8925,"ZTMGPTDT",TMGDFN,CHKIN))
  . . . NEW NOTEIEN 
  . . . IF NOTEDT'="" SET NOTEIEN=$O(^TIU(8925,"ZTMGPTDT",TMGDFN,NOTEDT,0))
  . . . IF NOTEDT[$P(CHKIN,".",1) DO
  . . . . SET NOTEDT=$J(NOTEDT,7,4)
  . . . . SET CHKIN=$J(CHKIN,7,4)
  . . . . SET CHKOUT=$J(CHKOUT,7,4)
  . . . . ;"WRITE CHKIN," - ",NOTEDT," - ",CHKOUT," = "
  . . . . SET NOTEDIFF=$$TIMEDIFF^TMGDATE(NOTEDT,CHKIN)
  . . . . SET NOTESIGN=$P($G(^TIU(8925,NOTEIEN,15)),"^",1)
  . . . . ;"WRITE NOTEDIFF,!
  . . . ELSE  DO
  . . . . SET NOTEDIFF="??"
  . . . . SET NOTEDT="NOT ON DOS"
  . . . . SET NOTESIGN="NOT ON DOS"
  . . . ;"
  . . . ;"This section will get timer values
  . . . NEW BTIME,ETIME
  . . . NEW TIMEIDX,THISTIME
  . . . SET (TIMEIDX,ETIME)=0,BTIME=9999999
  . . . FOR  SET TIMEIDX=$O(^TMG(22747,"C",TMGDFN,TIMEIDX)) QUIT:TIMEIDX'>0  DO
  . . . . SET THISTIME=$P($G(^TMG(22747,TIMEIDX,0)),"^",4)
  . . . . IF $P(THISTIME,".",1)'=THISDAY QUIT
  . . . . ;"SET THISTIME=$P(THISTIME,".",2)
  . . . . IF THISTIME<BTIME SET BTIME=THISTIME
  . . . . IF THISTIME>ETIME SET ETIME=THISTIME
  . . . SET BTIME=$$EXTDATE^TMGDATE(BTIME)
  . . . SET BTIME=$P(BTIME,"@",2)
  . . . SET ETIME=$$EXTDATE^TMGDATE(ETIME)
  . . . SET ETIME=$P(ETIME,"@",2)
  . . . ;"
  . . . SET DIFF=$$TIMEDIFF^TMGDATE(CHKOUT,CHKIN)
  . . . NEW NOTE2CHKOUT 
  . . . IF NOTEDIFF="??" DO
  . . . . SET NOTE2CHKOUT="??"
  . . . ELSE  DO
  . . . . SET NOTE2CHKOUT=DIFF-NOTEDIFF
  . . . ;"WRITE $P($G(^DPT(TMGDFN,0)),"^",1),?25,NOTEDIFF," MINS",?45,NOTE2CHKOUT," MINS",?65,DIFF," MINS",!
  . . . WRITE $P($G(^DPT(TMGDFN,0)),"^",1),!
  . . . SET CHKIN=$$EXTDATE^TMGDATE(CHKIN),CHKOUT=$$EXTDATE^TMGDATE(CHKOUT),NOTEDT=$$EXTDATE^TMGDATE(NOTEDT)
  . . . SET NOTESIGN=$$EXTDATE^TMGDATE(NOTESIGN)
  . . . SET CHKIN=$P(CHKIN,"@",2),CHKOUT=$P(CHKOUT,"@",2),NOTEDT=$P(NOTEDT,"@",2),NOTESIGN=$P(NOTESIGN,"@",2)
  . . . WRITE ?2,CHKIN,?15,CHKOUT,?26,BTIME,?39,ETIME,?54,NOTEDT,?67,NOTESIGN,!
  . . . SET DAYNUM=DAYNUM+1,DAYTOT=DAYTOT+DIFF,RPTNUM=RPTNUM+1,RPTTOT=RPTTOT+DIFF
  IF DAYNUM>0 DO
  . WRITE "  *AVERAGE WAIT FOR THIS DAY: ",$J(DAYTOT/DAYNUM,9,2)," MINUTES",!,!
  WRITE "  ****AVERAGE WAIT PER PATIENT: ",$J(RPTTOT/RPTNUM,9,2)," MINUTES",!
AT2Dn
  DO ^%ZISC  ;" Close the output device
  QUIT
  ;"  
WAITTIME(BDATE,EDATE)    ;"
  ;"This function will calculate the wait times for a given time period.
  ;NEW %ZIS,IOP
  ;SET IOP="S121-LAUGHLIN-LASER"
  ;DO ^%ZIS  ;"standard device call
  ;IF POP DO  GOTO ATDn
  ;. DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output. Aborting.")
  ;USE IO
  WRITE !
  WRITE "************************************************************",!
  WRITE "               WAIT TIMES (FOR CHECKIN TO SEEING PHYSICIAN",!
  WRITE "               FOR APPOINTMENTS FROM ",!
  WRITE "               ",$$EXTDATE^TMGDATE(BDATE)," TO ",$$EXTDATE^TMGDATE(EDATE),!
  WRITE "************************************************************",!
  WRITE "                                            (From TMGRPT2.m)",!!
  NEW DATE SET DATE=$P(BDATE,".",1)
  NEW DAYNUM,DAYTOT,RPTNUM,RPTTOT  ;"Used to report totals
  SET (DAYNUM,DAYTOT,RPTNUM,RPTTOT)=0
  SET EDATE=$P(EDATE,".",1)_".9999"
  NEW THISDAY SET THISDAY=0
  WRITE "PATIENT NAME",?25,"CHECK-IN TIME",?45,"PHYSICIAN START TIME",?65,"WAIT TIME",!
  FOR  SET DATE=$O(^TMG(22723,"DT",DATE)) QUIT:(DATE>EDATE)!(DATE'>0)  DO
  . NEW TMGDFN SET TMGDFN=0
  . IF THISDAY'=$P(DATE,".",1) DO
  . . SET THISDAY=$P(DATE,".",1)
  . . IF DAYNUM>0 DO
  . . . WRITE "  *AVERAGE WAIT FOR THIS DAY: ",$J(DAYTOT/DAYNUM,9,2)," MINUTES",!,!
  . . WRITE "-------- DATE: ",$$EXTDATE^TMGDATE(THISDAY)," --------",!
  . . SET DAYNUM=0,DAYTOT=0
  . FOR  SET TMGDFN=$O(^TMG(22723,"DT",DATE,TMGDFN)) QUIT:TMGDFN'>0  DO
  . . NEW SUBIEN SET SUBIEN=0
  . . FOR  SET SUBIEN=$O(^TMG(22723,"DT",DATE,TMGDFN,SUBIEN)) QUIT:SUBIEN'>0  DO
  . . . NEW STATUS SET STATUS=$G(^TMG(22723,"DT",DATE,TMGDFN,SUBIEN))
  . . . IF STATUS="C" QUIT
  . . . NEW ZN,CHKIN,DOCIN,DIFF
  . . . SET ZN=$G(^TMG(22723,TMGDFN,1,SUBIEN,0))
  . . . SET CHKIN=+$P(ZN,"^",8),DOCIN=$$STRTTIME(TMGDFN,THISDAY)
  . . . IF DATE>CHKIN SET CHKIN=DATE
  . . . IF (CHKIN'>0)!(DOCIN'>0) QUIT
  . . . ;"
  . . . SET DIFF=$$TIMEDIFF^TMGDATE(DOCIN,CHKIN)
  . . . IF DIFF>180 QUIT  ;"THIS COULD SIGNIFY A PROBLEM
  . . . WRITE $P($G(^DPT(TMGDFN,0)),"^",1),?25,CHKIN,?45,DOCIN,?65,DIFF," MINS",!
  . . . SET DAYNUM=DAYNUM+1,DAYTOT=DAYTOT+DIFF,RPTNUM=RPTNUM+1,RPTTOT=RPTTOT+DIFF
  IF DAYNUM>0 DO
  . WRITE "  *AVERAGE WAIT FOR THIS DAY: ",$J(DAYTOT/DAYNUM,9,2)," MINUTES",!,!
  WRITE "  ****AVERAGE WAIT PER PATIENT: ",$J(RPTTOT/RPTNUM,9,2)," MINUTES",!
;"ATDn
  ;"DO ^%ZISC  ;" Close the output device
  QUIT
  ;"
STRTTIME(TMGDFN,THISDATE)  ;"RETURN THE START TIME FOR THE DAY
  NEW BTIME,ETIME
  NEW TIMEIDX,THISTIME
  SET (TIMEIDX,ETIME)=0,BTIME=9999999
  FOR  SET TIMEIDX=$O(^TMG(22747,"C",TMGDFN,TIMEIDX)) QUIT:TIMEIDX'>0  DO
  . SET THISTIME=$P($G(^TMG(22747,TIMEIDX,0)),"^",4)
  . IF $P(THISTIME,".",1)'=THISDAY QUIT
  . ;"SET THISTIME=$P(THISTIME,".",2)
  . IF THISTIME<BTIME SET BTIME=THISTIME
  . IF THISTIME>ETIME SET ETIME=THISTIME
  ;SET BTIME=$$EXTDATE^TMGDATE(BTIME)
  ;SET BTIME=$P(BTIME,"@",2)
  ;SET ETIME=$$EXTDATE^TMGDATE(ETIME)
  ;SET ETIME=$P(ETIME,"@",2)
  IF BTIME=9999999 SET BTIME=0
  QUIT BTIME
  ;"
MISOVCHG(BDATE,EDATE)  ;"Print appts that don't have corresponding OV charges
  NEW DATE SET DATE=$P(BDATE,".",1)
  SET EDATE=$P(EDATE,".",1)_".999999"
  FOR  SET DATE=$O(^TMG(22723,"DT",DATE)) QUIT:(DATE>EDATE)!(DATE'>0)  DO
  . NEW TMGDFN SET TMGDFN=0
  . FOR  SET TMGDFN=$O(^TMG(22723,"DT",DATE,TMGDFN)) QUIT:TMGDFN'>0  DO
  . . NEW SUBIEN SET SUBIEN=0
  . . FOR  SET SUBIEN=$O(^TMG(22723,"DT",DATE,TMGDFN,SUBIEN)) QUIT:SUBIEN'>0  DO
  . . . NEW STATUS SET STATUS=$G(^TMG(22723,"DT",DATE,TMGDFN,SUBIEN))
  . . . IF STATUS="C" QUIT
  . . . NEW THISDATE SET THISDATE=$P(DATE,".",1)
  . . . IF $$HASCPT2^TMGRPU1(TMGDFN,99,THISDATE_".000000",THISDATE_".999999")=0 DO
  . . . . NEW NOTEDATE SET NOTEDATE=THISDATE
  . . . . FOR  SET NOTEDATE=$O(^TIU(8925,"ZTMGPTDT",TMGDFN,NOTEDATE)) QUIT:(NOTEDATE=0)!(NOTEDATE'[THISDATE)  DO
  . . . . . NEW NOTEIEN SET NOTEIEN=$O(^TIU(8925,"ZTMGPTDT",TMGDFN,NOTEDATE,0))
  . . . . . NEW NOTETITLE SET NOTETITLE=$P($G(^TIU(8925,NOTEIEN,0)),"^",1)
  . . . . . NEW OVTITLE SET OVTITLE=$P($G(^TIU(8925.1,NOTETITLE,"TMGH")),"^",1)
  . . . . . ;"WRITE OVTITLE,!
  . . . . . IF OVTITLE'="Y" QUIT
  . . . . . ;"WRITE TMGDFN," - ",NOTEDATE,"  -  ",NOTETITLE,!
  . . . . . WRITE $P($G(^DPT(TMGDFN,0)),"^",1)_" MISSING OV FOR "_$$EXTDATE^TMGDATE(DATE),!  
  QUIT
  ;"
TMGAWV(ROOT,TMGDFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) ;"AWV report
  ;"Purpose: Entry point, as called from CPRS REPORT system
  ;"Input: ROOT -- Pass by NAME.  This is where output goes
  ;"       TMGDFN -- Patient TMGDFN ; ICN for foriegn sites
  ;"       ID --
  ;"       ALPHA -- Start date (lieu of DTRANGE)
  ;"       OMEGA -- End date (lieu of DTRANGE)
  ;"       DTRANGE -- # days back from today
  ;"       REMOTE --
  ;"       MAX    --
  ;"       ORFHIE --
  ;"Result: None.  Output goes into @ROOT
  NEW TMGRESULT
  NEW HD SET HD="<TABLE BORDER=3><CAPTION><B>ANNUAL WELLNESS REPORT</B><BR>"
  SET HD=HD_" FAMILY PHYSICIANS OF GREENEVILLE<BR>1410 TUSCULUM BLVD  STE. 2600 <BR>"
  SET HD=HD_" GREENEVILLE, TN 37745</CAPTION><TR><TH>ITEM</TH>"
  SET HD=HD_"<TH>STATUS</TH><TH>LAST DONE</TH><TH>DUE DATE</TH></TR>"
  ;"SET TMGRESULT(1)="PAP SMEAR^1/1/18^1/1/19"
  ;"SET TMGRESULT(2)="MAMMOGRAM^2/2/20^2/2/21"  
  NEW REMIEN,REMLIST SET REMIEN=0
  DO AWVREMS(.REMLIST)
  FOR  SET REMIEN=$O(REMLIST(REMIEN)) QUIT:REMIEN'>0  DO
  . NEW REMRESULT
  . SET REMRESULT=$$DOREM^TMGPXR03(TMGDFN,REMIEN,5,$$TODAY^TMGDATE,1)
  . NEW STATUS,DUE,DONE
  . SET STATUS=$P(REMRESULT,"^",1),DONE=$P(REMRESULT,"^",3),DUE=$P(REMRESULT,"^",2)
  . IF DONE="" SET DONE="NO RECORD OF BEING DONE"
  . IF DUE="" SET DUE="NO DUE DATE CALCULATED"
  . SET TMGRESULT(REMIEN)=$G(REMLIST(REMIEN))_"^"_STATUS_"^"_DONE_"^"_DUE
  DO SETHTML(.ROOT,.TMGRESULT,"ANNUAL WELLNESS REPORT",HD,4)
  QUIT
  ;
AWVREMS(REMLIST)  ;"list of reminders to include on AWV report
  SET REMLIST(225)="ABDOMINAL AORTIC ANEURYSM (AAA)^Once"
  ;"SET REMLIST(256)="LIPID PANEL^Annually"
  SET REMLIST(294)="LIPID PANEL^Every 5 years"
  SET REMLIST(228)="BONE DENSITY^Every 2 years"
  SET REMLIST(281)="PAP SMEAR^Every 5 years"
  ;"SET REMLIST(277)="DEPRESSION SCREENING^Annually"
  SET REMLIST(299)="DEPRESSION SCREENING^Annually"
  SET REMLIST(290)="DIABETES SCREENING^Annually"
  SET REMLIST(33)="FLU VACCINATION^Annually"
  ;"SET REMLIST(244)="LOW DOSE CT SCREENING^Annually"
  SET REMLIST(298)="LOW DOSE CT SCREENING^Annually"
  SET REMLIST(224)="MAMMOGRAM/BREAST IMAGING^Annually"  
  ;"SET REMLIST(241)="PREVNAR-13^Once, over the age of 65"
  ;"SET REMLIST(257)="PNEUMOCOCCAL-23^Once, over the age of 65"
  SET REMLIST(257)="PNEUMOCOCCAL-23^Once, over the age of 65"
  SET REMLIST(224)="MAMMOGRAM/BREAST IMAGING^Annually"
  ;"Turn off until completed -> SET REMLIST(293)="TOBACCO CESSATION^Annually"
  SET REMLIST(231)="ADVANCE CARE PLANNING^Every 2 years"  
  SET REMLIST(220)="FECAL OCCULT BLOOD TEST^Annually or every 3 years if cologuard"
  SET REMLIST(223)="COLONOSCOPY^Every 10 years, if not otherwise specified"
  SET REMLIST(302)="SHINGRIX (SHINGLES VACCINE)^Once in a 2 dose series"
  ;"STOOL DNA SCREENING TEST
  ;"SET REMLIST(242)="DIABETIC EYE EXAM^Annually"
  ;"SET REMLIST(243)="DIABETIC FOOT EXAM^Annually"
  ;"SET REMLIST(267)="STD SCREENING^Once"
  ;"SET REMLIST(267)="GLAUCOMA SCREENING^Annually"
  ;"SET REMLIST(254)="HEMOGLOBIN A1C^Every 6 months"
  ;"SET REMLIST(127)="HEPATITIS C^Once"
  ;"SET REMLIST(211)="HIV SCREENING^Once" 
  ;"SET REMLIST(263)="PSA SCREENING^Every 3 months" 
  ;"SET REMLIST(229)="HEPATITIS B RISK ASSESSMENT^Once"
  ;"SET REMLIST(214)="TDAP VACCINATION^Once"
  ;" Don't use for now -> SET REMLIST(261)="CARDIOVASCULAR DISEASE SCREENING"
  QUIT
  ;"
PTONSTATIN  ;"REPORT THAT CAN BE KILLED LATER THAT JUST PULLS ALL PATIENTS WHO ARE ON STATINS
  NEW %ZIS,IOP
  SET IOP="S121-LAUGHLIN-LASER"
  DO ^%ZIS  ;"standard device call
  IF POP DO  GOTO ATDn
  . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output. Aborting.")
  USE IO
  NEW TMGDFN SET TMGDFN=0
  NEW PATARR
  FOR  SET TMGDFN=$O(^DPT(TMGDFN)) QUIT:TMGDFN'>0  DO
  . IF $$ACTIVEPT^TMGPXR03(TMGDFN,3)<1 QUIT
  . NEW TMGMEDARR,TMGMEDLIST
  . DO MEDARR^TMGTIUOJ(.TMGMEDLIST,.TMGDFN,.TMGMEDARR)
  . NEW MEDIDX SET MEDIDX=0
  . FOR  SET MEDIDX=$O(TMGMEDARR(MEDIDX)) QUIT:MEDIDX'>0  DO
  . . NEW MEDNAME SET MEDNAME=$G(TMGMEDARR(MEDIDX))
  . . ;"WRITE MEDNAME,!
  . . IF ($$UP^XLFSTR(MEDNAME)[$$UP^XLFSTR("simvastatin"))!($$UP^XLFSTR(MEDNAME)[$$UP^XLFSTR("lovastatin")) DO
  . . . SET PATARR($PIECE($G(^DPT(TMGDFN,0)),"^",1)_" is on "_MEDNAME)=""
  WRITE !
  WRITE "************************************************************",!
  WRITE "               PATIENTS WHO ARE ON EITHER ",!
  WRITE "               SIMVASTATIN OR LOVASTATIN",!
  WRITE "************************************************************",!
  WRITE "                                            (From TMGRPT2.m)",!!
  NEW OUTLINE SET OUTLINE=""
  FOR  SET OUTLINE=$O(PATARR(OUTLINE)) QUIT:OUTLINE=""  DO
  . WRITE OUTLINE,!
  DO ^%ZISC  ;" Close the output device
  QUIT
  ;"
PAXNOTE(TMGDFN,TEMPOUTARR)  ;"
  NEW TMGRESULT SET TMGRESULT=""
  NEW TMGMEDARR,TMGMEDLIST
  DO MEDARR^TMGTIUOJ(.TMGMEDLIST,.TMGDFN,.TMGMEDARR)
  NEW MEDIDX SET MEDIDX=0
  FOR  SET MEDIDX=$O(TMGMEDARR(MEDIDX)) QUIT:MEDIDX'>0  DO
  . NEW MEDNAME SET MEDNAME=$G(TMGMEDARR(MEDIDX))
  . IF ($$UP^XLFSTR(MEDNAME)[$$UP^XLFSTR("simvastatin"))!($$UP^XLFSTR(MEDNAME)[$$UP^XLFSTR("lovastatin")) DO
  . . SET TMGRESULT="NOTE: Patient is on "_MEDNAME_". This should be stopped at least 12 hours prior to initiation of PAXLOVID"  
  QUIT TMGRESULT
  ;"
INSCARDS   ;"GET ALL INSURANCE CARDS
  NEW SCHARRAY
  DO GETSCHED^TMGPXR03(.SCHARRAY,$$TODAY^TMGDATE)
  NEW TMGDFN SET TMGDFN=0
  NEW OUTARR,OKAYARRAY
  NEW ARR65,ARR18
  NEW BEGDATE SET BEGDATE=$$FIRSTYR^TMGDATE
  FOR  SET TMGDFN=$O(SCHARRAY(TMGDFN)) QUIT:TMGDFN'>0  DO
  . NEW APPTTIME SET APPTTIME=$O(SCHARRAY(TMGDFN,0))
  . NEW LASTINSCARD SET LASTINSCARD=0
  . NEW TIUIEN SET TIUIEN=0
  . FOR  SET TIUIEN=$O(^TIU(8925,"C",TMGDFN,TIUIEN)) QUIT:TIUIEN'>0  DO
  . . NEW DOCTYPE SET DOCTYPE=$P($G(^TIU(8925,TIUIEN,0)),"^",1)
  . . IF DOCTYPE'=2002 QUIT
  . . NEW DOCDATE SET DOCDATE=$P($G(^TIU(8925,TIUIEN,13)),"^",1)
  . . IF DOCDATE>LASTINSCARD SET LASTINSCARD=DOCDATE
  . SET APPTTIME=$$EXTDATE^TMGDATE(APPTTIME)
  . SET APPTTIME=$P(APPTTIME,"@",2)
  . IF LASTINSCARD>BEGDATE SET OKAYARRAY(APPTTIME,TMGDFN)=$P($G(^DPT(TMGDFN,0)),"^",1)_"-"_$$EXTDATE^TMGDATE(LASTINSCARD,1) QUIT
  . IF LASTINSCARD=0 DO
  . . SET LASTINSCARD="NO INSURANCE CARD FOUND IN CPRS"
  . ELSE  DO
  . . SET LASTINSCARD=$$EXTDATE^TMGDATE(LASTINSCARD,1)
  . SET OUTARR(APPTTIME,TMGDFN)=$P($G(^DPT(TMGDFN,0)),"^",1)_"^"_LASTINSCARD
  . NEW AGE K VADM SET AGE=+$$AGE^TIULO(TMGDFN)
  . IF AGE=65 SET ARR65(TMGDFN)=""
  . IF AGE=18 SET ARR18(TMGDFN)=""
  IF '$D(OUTARR) QUIT
  NEW %ZIS,IOP
  SET IOP="S121-LAUGHLIN-LASER"
  DO ^%ZIS  ;"standard device call
  IF POP DO  GOTO ATDn
  . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output. Aborting.")
  USE IO
  WRITE !
  WRITE "************************************************************",!
  WRITE "               PATIENTS WHO NEED UPDATED ",!
  WRITE "               INSURANCE CARDS IN CPRS",!
  WRITE "************************************************************",!
  WRITE "                                            (From TMGRPT2.m)",!!
  WRITE ?5,"TIME",?15,"PATIENT",?40,"LAST SCAN",!
  NEW TIME SET TIME=0
  FOR  SET TIME=$O(OUTARR(TIME)) QUIT:TIME'>0  DO
  . SET TMGDFN=0
  . FOR  SET TMGDFN=$O(OUTARR(TIME,TMGDFN)) QUIT:TMGDFN'>0  DO
  . . NEW PT,LASTSCAN
  . . SET PT=$P($G(OUTARR(TIME,TMGDFN)),"^",1)
  . . SET LASTSCAN=$P($G(OUTARR(TIME,TMGDFN)),"^",2)
  . . WRITE "[  ]",?5,TIME,?15,PT,?40,LASTSCAN,!,!
  WRITE "************************************************************",!
  WRITE "               PATIENTS WHO HAVE UPDATED CARDS ",!
  WRITE "************************************************************",!
  SET TIME=0
  FOR  SET TIME=$O(OKAYARRAY(TIME)) QUIT:TIME'>0  DO
  . SET TMGDFN=0
  . FOR  SET TMGDFN=$O(OKAYARRAY(TIME,TMGDFN)) QUIT:TMGDFN'>0  DO
  . . NEW PT,LASTSCAN
  . . SET PT=$P($G(OKAYARRAY(TIME,TMGDFN)),"^",1)
  . . SET LASTSCAN=$P($G(OKAYARRAY(TIME,TMGDFN)),"^",2)
  . . WRITE ?5,TIME,?15,PT,?40,LASTSCAN,!
  ;" 
  ;"65 YR OLDS ON THE SCHEDULE TODAY FOR CPE
  IF $D(ARR65) DO
  . WRITE "************************************************************",!
  . WRITE "               PATIENTS WHO ARE 65 AND SCHEDULED FOR CPE",!
  . WRITE "                        (DO THEY HAVE MEDICARE?)",!
  . WRITE "************************************************************",!
  . NEW TMGDFN SET TMGDFN=0
  . FOR  SET TMGDFN=$O(ARR65(TMGDFN)) QUIT:TMGDFN'>0  DO
  . . WRITE "[  ]",?5,$P($G(^DPT(TMGDFN,0)),"^",1),!
  ;"
  ;"18 YR OLDS ON THE SCHEDULE TODAY  
  IF $D(ARR18) DO
  . WRITE "************************************************************",!
  . WRITE "               PATIENTS WHO ARE 18 AND MAY NEED",! 
  . WRITE "                    UPDATED HIPAA FORMS",!
  . WRITE "************************************************************",!
  . NEW TMGDFN SET TMGDFN=0
  . FOR  SET TMGDFN=$O(ARR18(TMGDFN)) QUIT:TMGDFN'>0  DO
  . . WRITE "[  ]",?5,$P($G(^DPT(TMGDFN,0)),"^",1),!
  DO ^%ZISC  ;" Close the output device
  QUIT
  ;"
  