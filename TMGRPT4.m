TMGRPT4  ;TMG/kst TMG REPORTS  ;10/7/16, 3/24/21
         ;;1.0;TMG-LIB;**1**;10/7/16
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 10/7/16  Kevin S. Toppenberg MD
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
 ;"TMGCPT(ROOT,TMGDFN,ID,SDT,EDT,DTRANGE,REMOTE,MAX,ORFHIE) -- CPT report Entry point, as called from CPRS REPORT system
 ;"GETCPT(OUT,TMGDFN,SDT,EDT) --gather CPT'S for patient into array
 ;"GETICD(OUT,TMGDFN,SDT,EDT) --Gather ICD's for patient into array  
 ;"FLURPT(SDT,EDT,VERBOSE) -- Show number of flu shots administered by month
 ;"PETAGETR() -- Physical exam tag entry point  Entry point for report option
 ;"PETAGEST(BDATE,EDATE,SUPPRESS) -- Physical Exam tag exists.  look through TIU notes to see if Dr. Kevin's physical exam disclaimer has not been removed properly.
 ;"PHNVISIT(BDT,EDT)  -- DISPLAY TELEPHONE VISITS WHERE E&M CODES WERE BILLED 7 DAYS PRIOR OR 24 HOURS AFTER
 ;"NONTEAPT -- see if there are any appts scheduled that doesn't have any notes. Checks for the last 7 days
 ;"NOADDL -- see if over the course of the last 7 days there are any particular note titles that doesn't have addl signers but should
 ;"UNSIGNTIU  -- UNSIGNED OFFICE NOTES"
 ;"NOFUAPPT  --PRINT A LIST OF PATIENT SEEN LAST WEEK, WHO DON'T HAVE APPTS SCHEDULED
 ;"WRONGTIUDATE
 ;" 
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;
ADD(ROOT,STR,BR) ;
  SET BR=$GET(BR,1)
  NEW IDX SET IDX=+$ORDER(@ROOT@(""),-1)+1
  NEW BRSTR SET BRSTR=$SELECT($GET(BR)>0:"<BR>",1:"") 
  SET @ROOT@(IDX)=STR_BRSTR
  QUIT
  ;  
TMGCPT(ROOT,TMGDFN,ID,SDT,EDT,DTRANGE,REMOTE,MAX,ORFHIE) ;"CPT report  
  ;"Purpose: Entry point, as called from CPRS REPORT system
  ;"Called from TMGCPT^TMGRPT2, so this routine can be ZLINKED on the fly. 
  ;"Input: ROOT -- Pass by NAME.  This is where output goes
  ;"       TMGDFN -- Patient DFN ; ICN for foriegn sites
  ;"       ID --
  ;"       SDT -- Start date (lieu of DTRANGE)
  ;"       EDT -- End date (lieu of DTRANGE)
  ;"       DTRANGE -- # days back from today
  ;"       REMOTE --
  ;"       MAX    --
  ;"       ORFHIE --
  ;"Result: None.  Output goes into @ROOT
  SET SDT=+$GET(SDT)
  SET EDT=+$GET(EDT) IF EDT'>0 SET EDT="9999999"
  DO ADD(ROOT,"<HTML><HEAD>",0)
  DO ADD(ROOT,"<TITLE>CPT CODES</TITLE>",0)
  DO ADD(ROOT,"</HEAD><BODY>",0)
  ;DO ADD(ROOT,"THIS <B>IS A</B> TEST3")
  ;DO ADD(ROOT,"DFN="_TMGDFN)
  ;DO ADD(ROOT,"ID="_$GET(ID))
  ;DO ADD(ROOT,"SDT="_$GET(SDT))
  ;DO ADD(ROOT,"EDT="_$GET(EDT))
  ;DO ADD(ROOT,"DTRANGE"_$GET(DTRANGE))
  ;DO ADD(ROOT,"REMOTE="_$GET(REMOTE))
  ;DO ADD(ROOT,"MAX="_$GET(MAX))
  ;DO ADD(ROOT,"ORFHIE="_$GET(ORFHIE))
  NEW STR SET STR="<U>CPT's And ICD's FOR DATES: "
  SET STR=STR_"<B>"_$$FMTE^XLFDT(SDT\1)_"</B> - "  
  IF EDT=9999999 SET STR=STR_"<B>(NOW)</B>"
  ELSE  SET STR=STR_"<B>"_$$FMTE^XLFDT(EDT\1)_"</B>"
  SET STR=STR_"</U>"
  DO ADD(ROOT,STR)
  NEW TEMP 
  DO GETCPT(.TEMP,.TMGDFN,.SDT,.EDT)
  DO GETICD(.TEMP,.TMGDFN,.SDT,.EDT)
  NEW FOUND SET FOUND=0
  NEW DT SET DT=""
  FOR  SET DT=$ORDER(TEMP("DT",DT),-1) QUIT:+DT'>0  DO
  . DO ADD(ROOT,"<B><U>"_$$FMTE^XLFDT(DT\1)_"</B></U>")
  . NEW ICD SET ICD=""
  . FOR  SET ICD=$ORDER(TEMP("DT",DT,"ICD",ICD)) QUIT:ICD=""  DO
  . . NEW STR SET STR="<B>"_$PIECE(ICD,"^",1)_"</B> -- "_$PIECE(ICD,"^",2)
  . . DO ADD(ROOT,"&nbsp;&nbsp;ICD: "_STR)
  . NEW CPT SET CPT=""
  . FOR  SET CPT=$ORDER(TEMP("DT",DT,"CPT",CPT)) QUIT:CPT=""  DO
  . . NEW STR SET STR="<B>"_$PIECE(CPT,"^",1)_"</B> -- "_$PIECE(CPT,"^",2)
  . . DO ADD(ROOT,"&nbsp;&nbsp;CPT: "_STR)
  . SET FOUND=1
  IF FOUND=0 DO
  . DO ADD(ROOT,"(None found in date range -- perhaps try a different date range?)")
  DO ADD(ROOT,"<P>")  
  DO ADD(ROOT,"</BODY></HTML>")
  QUIT
  ;
GETCPT(OUT,TMGDFN,SDT,EDT) ;"gather CPT'S for patient into array
  NEW IEN SET IEN=0
  FOR  SET IEN=$ORDER(^AUPNVCPT("C",TMGDFN,IEN)) QUIT:+IEN'>0  DO
  . NEW ZN SET ZN=$GET(^AUPNVCPT(IEN,0)) QUIT:ZN=""
  . NEW VSTIEN SET VSTIEN=+$PIECE(ZN,"^",3) QUIT:VSTIEN'>0
  . NEW VSTZN SET VSTZN=$GET(^AUPNVSIT(VSTIEN,0)) QUIT:VSTZN=""
  . NEW VSTDT SET VSTDT=$PIECE(VSTZN,"^",1)
  . IF (VSTDT<SDT)!(VSTDT>EDT) QUIT  ;"Past this line, CPT is within date range
  . NEW CPTIEN SET CPTIEN=+$PIECE(ZN,"^",1) QUIT:CPTIEN'>0
  . NEW CPTZN SET CPTZN=$GET(^ICPT(CPTIEN,0)) QUIT:CPTZN="" 
  . SET OUT("CPT",VSTDT,CPTZN)=""
  . SET OUT("DT",VSTDT\1,"CPT",CPTZN)=""
  QUIT
  ;
GETICD(OUT,TMGDFN,SDT,EDT)  ;"Gather ICD's for patient into array  
  NEW IEN SET IEN=0
  FOR  SET IEN=$ORDER(^AUPNVPOV("C",TMGDFN,IEN)) QUIT:+IEN'>0  DO
  . NEW ZN SET ZN=$GET(^AUPNVPOV(IEN,0)) QUIT:ZN=""
  . NEW VSTIEN SET VSTIEN=+$PIECE(ZN,"^",3) QUIT:VSTIEN'>0
  . NEW VSTZN SET VSTZN=$GET(^AUPNVSIT(VSTIEN,0)) QUIT:VSTZN=""
  . NEW VSTDT SET VSTDT=$PIECE(VSTZN,"^",1)
  . IF (VSTDT<SDT)!(VSTDT>EDT) QUIT  ;"Past this line, ICD is within date range
  . NEW ICDIEN SET ICDIEN=+$PIECE(ZN,"^",1) QUIT:ICDIEN'>0
  . NEW ICDZN SET ICDZN=$GET(^ICD9(ICDIEN,0)) QUIT:ICDZN=""
  . NEW ADT SET ADT=$ORDER(^ICD9(ICDIEN,68,"B",""),-1) QUIT:ADT'>0
  . NEW PTR SET PTR=$ORDER(^ICD9(ICDIEN,68,"B",ADT,0)) QUIT:PTR'>0
  . NEW DESCR SET DESCR=$GET(^ICD9(ICDIEN,68,PTR,1))
  . NEW STR SET STR=$PIECE(ICDZN,"^",1)_"^"_DESCR
  . SET OUT("ICD",VSTDT,STR)=""
  . SET OUT("DT",VSTDT\1,"ICD",STR)=""
  QUIT
  ;
FLURPT(SDT,EDT,VERBOSE) ; 
  ;"Purpose: Show number of flu shots administered by month
  NEW ARR DO ALLCPT^TMGRPU1(.ARR,.SDT,.EDT)
  SET VERBOSE=+$GET(VERBOSE)
  NEW CPTARR
  NEW CPT SET CPT=""
  FOR  SET CPT=$ORDER(ARR("CPT",CPT)) QUIT:CPT=""  DO
  . NEW CPTNAME SET CPTNAME=$$UP^XLFSTR($GET(ARR("CPT",CPT)))
  . IF CPTNAME'["FLU" QUIT 
  . IF CPTNAME["ASSAY" QUIT
  . IF CPTNAME["ADMIN" QUIT
  . SET CPTARR(CPT)=CPTNAME
  . IF VERBOSE ZWR CPTARR
  NEW BYDATE
  SET CPT="" FOR  SET CPT=$ORDER(CPTARR(CPT)) QUIT:CPT=""  DO
  . NEW CPTNAME SET CPTNAME=$GET(CPTARR(CPT))
  . NEW ADT SET ADT=0
  . FOR  SET ADT=$ORDER(ARR("CPT",CPT,ADT)) QUIT:+ADT'>0  DO
  . . NEW YR SET YR=$EXTRACT(ADT,2,3)
  . . NEW MO SET MO=$EXTRACT(ADT,4,5)_" "
  . . NEW TMGDFN SET TMGDFN=0
  . . FOR  SET TMGDFN=$ORDER(ARR("CPT",CPT,ADT,TMGDFN)) QUIT:+TMGDFN'>0  DO
  . . . NEW MOCT SET MOCT=+$GET(BYDATE(YR,MO))
  . . . NEW YRCT SET YRCT=+$GET(BYDATE(YR))
  . . . NEW TOTAL SET TOTAL=+$GET(BYDATE("TOTAL"))
  . . . SET BYDATE(YR)=YRCT+1
  . . . SET BYDATE(YR,MO)=MOCT+1
  . . . SET BYDATE("TOTAL")=TOTAL+1
  . . . NEW PTNAME SET PTNAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
  . . . IF VERBOSE SET BYDATE(YR,MO,ADT,TMGDFN)=CPTNAME_"^"_PTNAME
  IF $DATA(BYDATE) ZWRITE BYDATE
  QUIT
  ;
FLURPT2 ;
  WRITE !,"Below is a listing of the number of flu shots given from 6/1/15-5/1/16",! 
  DO FLURPT(3150601,3160501)
  WRITE !,"Below is a listing of the number of flu shots given from 6/1/15-5/1/16",! 
  DO FLURPT(3160601,3170501)
  WRITE "DONE",!
  QUIT
  ;"
PRINTICD  ;"THIS FUNCTION IS A SCRATCH REPORT AND CAN BE DELETED
  NEW IEN,OUT SET IEN=0
  FOR  SET IEN=$ORDER(^AUPNVPOV("B",IEN)) QUIT:+IEN'>0  DO
  . NEW ZN SET ZN=$GET(^AUPNVPOV(IEN,0)) QUIT:ZN=""
  . NEW VSTIEN SET VSTIEN=+$PIECE(ZN,"^",3) QUIT:VSTIEN'>0
  . NEW VSTZN SET VSTZN=$GET(^AUPNVSIT(VSTIEN,0)) QUIT:VSTZN=""
  . NEW VSTDT SET VSTDT=$PIECE(VSTZN,"^",1)
  . IF (VSTDT<3150931) QUIT  ;"Past this line, ICD is within date
  . NEW ICDIEN SET ICDIEN=+$PIECE(ZN,"^",1) QUIT:ICDIEN'>0
  . NEW ICDZN SET ICDZN=$GET(^ICD9(ICDIEN,0)) QUIT:ICDZN=""
  . NEW ADT SET ADT=$ORDER(^ICD9(ICDIEN,68,"B",""),-1) QUIT:ADT'>0
  . NEW PTR SET PTR=$ORDER(^ICD9(ICDIEN,68,"B",ADT,0)) QUIT:PTR'>0
  . NEW DESCR SET DESCR=$GET(^ICD9(ICDIEN,68,PTR,1))
  . NEW STR SET STR=$PIECE(ICDZN,"^",1)_"^"_DESCR
  . SET OUT(STR)=""
  NEW %ZIS
  SET %ZIS("A")="Enter Output Device: "
  SET IOP="S121-LAUGHLIN-LASER"
  DO ^%ZIS  ;"standard device call
  USE IO
  NEW DATA SET DATA=""
  WRITE "ICD",?10,"DESCRIPTION",!
  FOR  SET DATA=$ORDER(OUT(DATA)) QUIT:DATA=""  DO
  . WRITE $PIECE(DATA,"^",1),?10,$E($PIECE(DATA,"^",2),1,65),!
  DO ^%ZISC  ;" Close the output device
  DO PRESS2GO^TMGUSRI2
  QUIT
  ;"
PETAGETR()  ;"Physical exam tag entry point
  ;"Purpose: Entry point for report option
  ;"         This will set up the 7 day date range and
  ;"         set the output printer
  ;"NOTE: Rewriting this to run daily. As so, it will only print notes for today
  NEW BDATE,EDATE,X,X1,X2
  DO NOW^%DTC
  ;"SET EDATE=X
  ;"SET X1=EDATE
  ;"SET X2="-7"
  ;"DO C^%DTC
  ;"Not doing weekly now, only one day
  SET BDATE=X
  SET EDATE=X+".999999"
  ;"SET BDATE=3140101
  ;"SET EDATE=3190101
  NEW %ZIS
  SET %ZIS("A")="Enter Output Device: "
  ;"SET %ZIS("B")="HOME"
  SET IOP="S121-LAUGHLIN-LASER"
  DO ^%ZIS  ;"standard device call
  IF POP DO  GOTO PETDN
  . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output.Aborting.")
  use IO
  NEW SUPPRESS 
  SET SUPPRESS=1  ;"CHANGE TO 1 TO STOP PRINTING, WHEN NO RESULTS ARE FOUND
  DO PETAGEST(BDATE,EDATE,SUPPRESS)
PETDN
  DO ^%ZISC  ;" Close the output device
  ;"DO PRESS2GO^TMGUSRI2
  QUIT
  ;"
PETAGEST(BDATE,EDATE,SUPPRESS)   ;"Physical Exam tag exists
  ;"Purpose: This report is designed to look through the last
  ;"         # days worth of TIU notes to see if Dr. Kevin's 
  ;"         physical exam disclaimer has not been removed properly.
  ;"         If the tag is found, the patient's name, date of note, and
  ;"         note title
  ;"Input: BDATE & EDATE for beginning and end dates (in fileman format)
  ;"       SUPPRESS - OPTIONAL. 1=DON'T PRINT IF NO RESULTS 
  NEW IENLIST,TOTALCOUNT
  NEW PHRASE SET PHRASE="NOTE: BELOW IS UNEDITED EXAM TEMPLATE."
  NEW TIUDATE SET TIUDATE=$PIECE(BDATE,".",1),TOTALCOUNT=0
  FOR  SET TIUDATE=$ORDER(^TIU(8925,"D",TIUDATE)) QUIT:(TIUDATE>EDATE)!(TIUDATE'>0)  DO
  . NEW TIUIEN
  . SET TIUIEN=0
  . FOR  SET TIUIEN=$ORDER(^TIU(8925,"D",TIUDATE,TIUIEN)) QUIT:TIUIEN'>0  DO
  . . NEW TIULINE,TIUTEXT SET TIULINE=0,TIUTEXT=""
  . . FOR  SET TIULINE=$ORDER(^TIU(8925,TIUIEN,"TEXT",TIULINE)) QUIT:TIULINE'>0  DO
  . . . SET TIUTEXT=TIUTEXT_$GET(^TIU(8925,TIUIEN,"TEXT",TIULINE,0))
  . . IF TIUTEXT[PHRASE DO
  . . . SET IENLIST(TIUIEN)=""
  . . . SET TOTALCOUNT=TOTALCOUNT+1
  SET SUPPRESS=+$GET(SUPPRESS)
  IF (SUPPRESS=1)&(TOTALCOUNT'>0) GOTO PEDN   ;"If no results, don't print anything
  DO NOW^%DTC
  WRITE !
  WRITE "****************************************************************",!
  WRITE "         PATIENTS WITH INCOMPLETE PHYSICAL EXAMINATIONS",!
  WRITE "             DATE RANGE: ",$$EXTDATE(BDATE)," TO ",$$EXTDATE(EDATE),!
  WRITE "                  RUN DATE: ",$$EXTDATE(X),!  ;" SET Y=X DO DD^%DT WRITE Y,!
  WRITE "",!
  WRITE "                  PLEASE DELIVER TO DR. KEVIN",!
  WRITE "****************************************************************",!
  WRITE "                                        (From TMGRPT4.m)",!,!
  WRITE " ",!
  WRITE TOTALCOUNT," NOTES FOUND",!,!
  WRITE "PATIENT",?25,"DATE",?40,"TITLE",!
  WRITE "-------",?25,"----",?40,"-----",!
  NEW TIUIEN SET TIUIEN=0
  FOR  SET TIUIEN=$ORDER(IENLIST(TIUIEN)) QUIT:TIUIEN'>0  DO
  . NEW ZN SET ZN=$GET(^TIU(8925,TIUIEN,0))
  . NEW PATIENT,DATE,TITLE
  . SET PATIENT=$P(ZN,"^",2)
  . SET DATE=$P(ZN,"^",7)
  . SET TITLE=$P(ZN,"^",1)
  . SET PATIENT=$P($G(^DPT(PATIENT,0)),"^",1)
  . SET TITLE=$P($G(^TIU(8925.1,TITLE,0)),"^",1)
  . WRITE PATIENT,?25,$P($$EXTDATE(DATE),"@",1),?40,TITLE,!
  IF (SUPPRESS=1)&(TOTALCOUNT'>0) DO
  . WRITE !,!
  . WRITE "NOTE: Printing with 0 results can be turned off by setting"
  . WRITE " the SUPPRESS variable to 1 in PETAGETR^TMGRPT4",!
PEDN
  QUIT
  ;"
EXTDATE(FMDATE)  ;"WILL PROBABLY MOVE THIS UTILITY TO COMMON AREA
  NEW Y
  SET Y=FMDATE
  D DD^%DT
  QUIT Y
  ;"
PHNVISIT(BDT,EDT)  ;"
  WRITE "============= BILLING REPORT ================",!
  WRITE "DISPLAYING TELEPHONE VISITS WHERE E&M CODES",!
  WRITE "WERE BILLED 7 DAYS PRIOR OR 24 HOURS AFTER",!
  WRITE "=============================================",!,!
  NEW CPTARR,CPT,VCPTIEN,OUTPUTARR
  SET CPTARR(99441)="",CPTARR(99442)="",CPTARR(99443)="",CPT=0
  FOR  SET CPT=$O(CPTARR(CPT)) QUIT:CPT'>0  DO
  . SET VCPTIEN=0
  . FOR  SET VCPTIEN=$O(^AUPNVCPT("B",CPT,VCPTIEN)) QUIT:VCPTIEN'>0  DO
  . . NEW ZN SET ZN=$GET(^AUPNVCPT(VCPTIEN,0)) QUIT:ZN=""
  . . NEW TMGDFN SET TMGDFN=+$P(ZN,"^",2) QUIT:TMGDFN'>0
  . . NEW VSTIEN SET VSTIEN=+$PIECE(ZN,"^",3) QUIT:VSTIEN'>0
  . . NEW VSTZN SET VSTZN=$GET(^AUPNVSIT(VSTIEN,0)) QUIT:VSTZN=""
  . . NEW VSTDT SET VSTDT=$PIECE(VSTZN,"^",1)
  . . IF (VSTDT<BDT)!(VSTDT>EDT) QUIT  ;"Past this line, CPT is within date range
  . . NEW STARTDT,ENDDT   ;"THESE WILL BE THE DATE RANGE (7 DAYS PRIOR, OR 24 HOURS AFTER) TO LOOK FOR E&M VISITS
  . . SET STARTDT=$$ADDDAYS^TMGDATE("-7",VSTDT)
  . . SET ENDDT=$$ADDDAYS^TMGDATE(1,VSTDT)
  . . NEW CPTARR DO GETCPT(.CPTARR,TMGDFN,STARTDT,ENDDT)
  . . ;"CPTARR("CPT",3200714.1,"99442
  . . NEW DATE SET DATE=0
  . . FOR  SET DATE=$O(CPTARR("CPT",DATE)) QUIT:DATE'>0  DO
  . . . NEW CPTSTR SET CPTSTR=""
  . . . FOR  SET CPTSTR=$O(CPTARR("CPT",DATE,CPTSTR)) QUIT:CPTSTR=""  DO
  . . . . NEW THISCPT SET THISCPT=$P(CPTSTR,"^",1)
  . . . . IF $$EXTDATE^TMGDATE(DATE,1)=$$EXTDATE^TMGDATE(VSTDT,1) QUIT
  . . . . IF THISCPT["9921" DO
  . . . . . SET OUTPUTARR($P($G(^DPT(TMGDFN,0)),"^",1)," PHONE VISIT: "_CPT_" ON "_$$EXTDATE^TMGDATE(VSTDT,1)_" E&M VISIT: "_THISCPT_" ON "_$$EXTDATE^TMGDATE(DATE,1))=""
  . . . . . ;"WRITE "PATIENT: ",$P($G(^DPT(TMGDFN,0)),"^",1),!," PHONE VISIT: ",THISCPT," ON ",$$EXTDATE^TMGDATE(VSTDT,1),!," E&M VISIT: ",CPT," ON ",$$EXTDATE^TMGDATE(THISDATE,1),!,!
  IF '$D(OUTPUTARR) QUIT
  NEW PATNAME SET PATNAME=""
  FOR  SET PATNAME=$O(OUTPUTARR(PATNAME)) QUIT:PATNAME=""  DO
  . WRITE "PATIENT: ",PATNAME,!
  . NEW DATASTR SET DATASTR=""
  . FOR  SET DATASTR=$O(OUTPUTARR(PATNAME,DATASTR)) QUIT:DATASTR=""  DO
  . . WRITE DATASTR,!
  . WRITE !
  ;"
NONTEAPT    ;"This report checks to see if there are any appts scheduled
  ;" that doesn't have any notes. Checks for the last 7 days
  NEW BDATE,EDATE 
  SET BDATE=$$ADDDAYS^TMGDATE(-8)
  SET EDATE=$$ADDDAYS^TMGDATE(-1)
  NEW %ZIS
  SET %ZIS("A")="Enter Output Device: "
  SET IOP="S121-LAUGHLIN-LASER"
  DO ^%ZIS  ;"standard device call
  USE IO  
  WRITE !
  WRITE "****************************************************************",!
  WRITE "         PATIENTS WITH APPOINTMENTS SCHEDULED BUT NO NOTES",!
  WRITE "         (SHOULD THEY HAVE NO SHOW OR CANCELLED NOTES?)",!
  WRITE "             DATE RANGE: ",$$EXTDATE(BDATE)," TO ",$$EXTDATE(EDATE),!
  WRITE "                  RUN DATE: ",$$TODAY^TMGDATE(1,1),!
  WRITE "",!
  WRITE "                  PLEASE DELIVER TO BRENDA",!
  WRITE "****************************************************************",!
  WRITE "                                        (From TMGRPT4.m)",!,!
  WRITE " ",!  
  NEW APPTDATE SET APPTDATE=BDATE
  FOR  SET APPTDATE=$O(^TMG(22723,"DT",APPTDATE)) QUIT:(APPTDATE>EDATE)!(APPTDATE'>0)  DO
  . NEW TMGDFN SET TMGDFN=0
  . FOR  SET TMGDFN=$O(^TMG(22723,"DT",APPTDATE,TMGDFN)) QUIT:TMGDFN'>0  DO
  . . NEW APPTIEN SET APPTIEN=0
  . . FOR  SET APPTIEN=$O(^TMG(22723,"DT",APPTDATE,TMGDFN,APPTIEN)) QUIT:APPTIEN'>0  DO
  . . . NEW STATUS SET STATUS=$G(^TMG(22723,"DT",APPTDATE,TMGDFN,APPTIEN))
  . . . ;"IF STATUS="C" QUIT
  . . . NEW TIMEIN SET TIMEIN=+$P($G(^TMG(22723,TMGDFN,1,APPTIEN,0)),"^",8)
  . . . IF TIMEIN>0 QUIT
  . . . NEW TIUDATE SET TIUDATE=BDATE
  . . . NEW FOUND SET FOUND=0
  . . . FOR  SET TIUDATE=$O(^TIU(8925,"ZTMGPTDT",TMGDFN,TIUDATE)) QUIT:TIUDATE'>0  DO
  . . . . IF TIUDATE[$P(APPTDATE,".",1) SET FOUND=1
  . . . IF FOUND=1 QUIT
  . . . NEW NAME SET NAME=$P($G(^DPT(TMGDFN,0)),"^",1)
  . . . WRITE NAME," HAD APPT ON: ",$$EXTDATE^TMGDATE(APPTDATE)," - STATUS IS: ",STATUS,!
  . . . NEW FUTUREAPPT SET FUTUREAPPT=$$TODAY^TMGDATE_".9999"
  . . . SET FUTUREAPPT=$O(^TMG(22723,TMGDFN,1,"B",FUTUREAPPT))
  . . . IF FUTUREAPPT>0 SET FUTUREAPPT="    PATIENT SCHEDULED ON :"_$$EXTDATE^TMGDATE(FUTUREAPPT,1)
  . . . ELSE  SET FUTUREAPPT="    !!!! NO UPCOMING APPT !!!!"
  . . . WRITE FUTUREAPPT," [  ] OLD APPT, [  ] MADE NOTE",!,!
  DO ^%ZISC  ;" Close the output device
  QUIT
  ;"
NOADDL  ;"This report checks to see if over the course of the last
  ;" 7 days there are any particular note titles that doesn't have 
  ;" addl signers but should
  NEW BDATE,EDATE 
  SET BDATE=$$ADDDAYS^TMGDATE(-8)
  SET EDATE=$$ADDDAYS^TMGDATE(-1)
  NEW ADDLARRAY
  NEW %ZIS
  SET %ZIS("A")="Enter Output Device: "
  SET IOP="S121-LAUGHLIN-LASER"
  DO ^%ZIS  ;"standard device call
  USE IO  
  WRITE !
  WRITE "****************************************************************",!
  WRITE "         RAD, MAM, AND D/C SUMMARIES WITH NO ADDL SIGNERS",!
  WRITE "             DATE RANGE: ",$$EXTDATE(BDATE)," TO ",$$EXTDATE(EDATE),!
  WRITE "                  RUN DATE: ",$$TODAY^TMGDATE(1,1),!
  WRITE "",!
  WRITE "                  PLEASE DELIVER TO SABRINA",!
  WRITE "****************************************************************",!
  WRITE "                                        (From TMGRPT4.m)",!,!
  WRITE " ",!
  NEW TIUDATE SET TIUDATE=BDATE
  FOR  SET TIUDATE=$O(^TIU(8925,"D",TIUDATE)) QUIT:TIUDATE'>0  DO
  . NEW TIUIEN SET TIUIEN=0
  . FOR  SET TIUIEN=$O(^TIU(8925,"D",TIUDATE,TIUIEN)) QUIT:TIUIEN'>0  DO
  . . NEW DOCIEN SET DOCIEN=$P($G(^TIU(8925,TIUIEN,0)),"^",1)
  . . ;"ONLY CHECK MAM REPORT, RAD REPORT, AND HOSPITAL D/C
  . . IF (DOCIEN'=26)&(DOCIEN'=1428)&(DOCIEN'=1470)&(DOCIEN'=1471)&(DOCIEN'=81) QUIT
  . . NEW AUTHOR SET AUTHOR=$P($G(^TIU(8925,TIUIEN,12)),"^",2)
  . . IF (((DOCIEN=81)&(AUTHOR'=83))&((DOCIEN=81)&(AUTHOR'=168))) QUIT
  . . ;"IS SABRINA AN ADD'L SIGNER?
  . . NEW ADDLIEN SET ADDLIEN=0
  . . NEW ADDLFOUND SET ADDLFOUND=0
  . . FOR  SET ADDLIEN=$O(^TIU(8925.7,"B",TIUIEN,ADDLIEN)) QUIT:ADDLIEN'>0  DO
  . . . IF $P($G(^TIU(8925.7,ADDLIEN,0)),"^",3)=259 SET ADDLFOUND=1
  . . IF ADDLFOUND=0 DO
  . . . NEW TMGDFN SET TMGDFN=$P($G(^TIU(8925,TIUIEN,0)),"^",2)
  . . . SET ADDLARRAY(DOCIEN,TIUIEN)="  "_$P($G(^DPT(TMGDFN,0)),"^",1)_" ON "_$$EXTDATE^TMGDATE(TIUDATE,1)
  NEW DOCIEN SET DOCIEN=0
  FOR  SET DOCIEN=$O(ADDLARRAY(DOCIEN)) QUIT:DOCIEN'>0  DO
  . WRITE "==== ",$P($G(^TIU(8925.1,DOCIEN,0)),"^",1)," ====",!
  . NEW TIUIEN SET TIUIEN=0
  . FOR  SET TIUIEN=$O(ADDLARRAY(DOCIEN,TIUIEN)) QUIT:TIUIEN'>0  DO
  . . WRITE $G(ADDLARRAY(DOCIEN,TIUIEN)),!,!
  . WRITE !
  DO ^%ZISC  ;" Close the output device
  QUIT
  ;"
UNSIGNTIU  ;"
  NEW %ZIS
  SET %ZIS("A")="Enter Output Device: "
  SET IOP="S121-LAUGHLIN-LASER"
  DO ^%ZIS  ;"standard device call
  USE IO  
  WRITE !
  WRITE "****************************************************************",!
  WRITE "                  UNSIGNED OFFICE NOTES",!
  WRITE "                  RUN DATE: ",$$TODAY^TMGDATE(1,1),!
  WRITE "",!
  WRITE "                  PLEASE DELIVER TO EDDIE",!
  WRITE "****************************************************************",!
  WRITE "                                        (From TMGRPT4.m)",!,!
  WRITE " ",!
  NEW TMGDUZ SET TMGDUZ=0
  FOR  SET TMGDUZ=$O(^XTV(8992,TMGDUZ)) QUIT:TMGDUZ'>0  DO
  . NEW DISPLAY SET DISPLAY=0
  . NEW XTVDT SET XTVDT=0
  . FOR  SET XTVDT=$O(^XTV(8992,TMGDUZ,"XQA",XTVDT)) QUIT:XTVDT=""  DO
  . . NEW XTVOBJ SET XTVOBJ=$P($G(^XTV(8992,TMGDUZ,"XQA",XTVDT,0)),"^",2)
  . . IF XTVOBJ'["TIU" QUIT
  . . SET XTVOBJ=$P($P(XTVOBJ,"TIU",2),";",1)
  . . NEW TIUTYPE,TIUHLIGHT,TMGDFN
  . . SET TIUTYPE=+$P($G(^TIU(8925,XTVOBJ,0)),"^",1)
  . . IF TIUTYPE'>0 QUIT
  . . SET TMGDFN=$P($G(^TIU(8925,XTVOBJ,0)),"^",2)
  . . SET TIUHLIGHT=$P($G(^TIU(8925.1,TIUTYPE,"TMGH")),"^",1)
  . . IF TIUHLIGHT="" QUIT
  . . IF DISPLAY=0 WRITE " **** ",$P($G(^VA(200,TMGDUZ,0)),"^",1)," **** ",! SET DISPLAY=1
  . . WRITE "  - ",$P($G(^DPT(TMGDFN,0)),"^",1),", ",$$EXTDATE^TMGDATE($P($G(^TIU(8925,XTVOBJ,13)),"^",1),1)," -> ",$P($G(^TIU(8925.1,TIUTYPE,0)),"^",1),!
  . IF DISPLAY=1 WRITE !
  DO ^%ZISC  ;" Close the output device
  QUIT
  ;"
NOFUAPPT  ;"PRINT A LIST OF PATIENT SEEN LAST WEEK, WHO DON'T HAVE APPTS SCHEDULED
  NEW APPTARRAY,DATE
  SET DATE=$$ADDDAYS^TMGDATE(-8)
  FOR  SET DATE=$O(^TIU(8925,"D",DATE)) QUIT:DATE=""  DO
  . NEW TIUIEN SET TIUIEN=0
  . FOR  SET TIUIEN=$O(^TIU(8925,"D",DATE,TIUIEN)) QUIT:TIUIEN=""  DO
  . . NEW TITLEIEN SET TITLEIEN=$P($G(^TIU(8925,TIUIEN,0)),"^",1)
  . . NEW TMGDFN SET TMGDFN=+$P($G(^TIU(8925,TIUIEN,0)),"^",2)
  . . IF TMGDFN'>0 QUIT
  . . NEW PHONE SET PHONE=$P($G(^DPT(TMGDFN,.13)),"^",1)
  . . NEW CELL SET CELL=$P($G(^DPT(TMGDFN,.13)),"^",4)
  . . NEW PHONELINE SET PHONELINE=""
  . . IF PHONE'="" SET PHONELINE="(H) "_PHONE
  . . IF CELL'="" DO
  . . . IF PHONELINE'="" SET PHONELINE=PHONELINE_","
  . . . SET PHONELINE=PHONELINE_"(C)"_CELL
  . . IF TITLEIEN=1413 SET APPTARRAY(TMGDFN)=$P($G(^DPT(TMGDFN,0)),"^",1)_" NO SHOWED ON "_$$EXTDATE^TMGDATE(DATE,1)_" "_PHONELINE
  . . NEW OVNOTE SET OVNOTE=$P($G(^TIU(8925.1,TITLEIEN,"TMGH")),"^",1)
  . . IF OVNOTE="Y" SET APPTARRAY(TMGDFN)=$P($G(^DPT(TMGDFN,0)),"^",1)_" was seen on "_$$EXTDATE^TMGDATE(DATE,1)_" "_PHONELINE
  ;"CHECK FOR FUTURE APPTS
  SET DATE=$$TODAY^TMGDATE
  FOR  SET DATE=$O(^TMG(22723,"DT",DATE)) QUIT:DATE=""  DO
  . NEW TMGDFN SET TMGDFN=0
  . FOR  SET TMGDFN=$O(^TMG(22723,"DT",DATE,TMGDFN)) QUIT:TMGDFN'>0  DO
  . . NEW SUBIEN SET SUBIEN=0
  . . FOR  SET SUBIEN=$O(^TMG(22723,"DT",DATE,TMGDFN,SUBIEN)) QUIT:SUBIEN'>0  DO
  . . . NEW STATUS SET STATUS=$G(^TMG(22723,"DT",DATE,TMGDFN,SUBIEN))
  . . . IF (STATUS="A")&($D(APPTARRAY(TMGDFN))) DO
  . . . . ;"SET APPTARRAY(TMGDFN)="HAS APPT ON "_DATE
  . . . . KILL APPTARRAY(TMGDFN)
  IF '$D(APPTARRAY) QUIT
  NEW REPORTARRAY
  SET TMGDFN=0
  FOR  SET TMGDFN=$O(APPTARRAY(TMGDFN)) QUIT:TMGDFN'>0  DO
  . SET REPORTARRAY($G(APPTARRAY(TMGDFN)))=""
  NEW %ZIS
  SET %ZIS("A")="Enter Output Device: "
  SET IOP="S121-LAUGHLIN-LASER"
  DO ^%ZIS  ;"standard device call
  USE IO  
  WRITE !
  WRITE "****************************************************************",!
  WRITE "                  LAST WEEK'S PATIENTS WHO DON'T HAVE",!
  WRITE "                  SCHEDULED APPOINTMENTS",!
  WRITE "                  RUN DATE: ",$$TODAY^TMGDATE(1,1),!
  WRITE "",!
  WRITE "                  PLEASE DELIVER TO TAMMY",!
  WRITE "****************************************************************",!
  WRITE "                                        (From TMGRPT4.m)",!,!
  WRITE " ",!
  NEW LINE SET LINE=""
  FOR  SET LINE=$O(REPORTARRAY(LINE)) QUIT:LINE=""  DO
  . WRITE LINE,!,!,!
  DO ^%ZISC  ;" Close the output device
NOFUDONE  
  QUIT
  ;"
WRONGTIUDATE
  NEW REFDATE SET REFDATE=3220718
  FOR  SET REFDATE=$O(^TIU(8925,"D",REFDATE)) QUIT:REFDATE=""  DO
  . NEW TIUIEN SET TIUIEN=0
  . FOR  SET TIUIEN=$O(^TIU(8925,"D",REFDATE,TIUIEN)) QUIT:TIUIEN'>0  DO
  . . NEW AUTHOR SET AUTHOR=$P($G(^TIU(8925,TIUIEN,12)),"^",2)
  . . IF AUTHOR'=83 QUIT
  . . NEW ENTRYDT SET ENTRYDT=$P($P($G(^TIU(8925,TIUIEN,12)),"^",1),".",1)
  . . IF ENTRYDT=$P(REFDATE,".",1) QUIT
  . . NEW NOTETYPE SET NOTETYPE=$P($G(^TIU(8925,TIUIEN,0)),"^",1)
  . . NEW TMGDFN SET TMGDFN=$P($G(^TIU(8925,TIUIEN,0)),"^",2)
  . . NEW NOTETEXT,NOTEIDX SET NOTEIDX=0,NOTETEXT=""
  . . FOR  SET NOTEIDX=$O(^TIU(8925,TIUIEN,"TEXT",NOTEIDX)) QUIT:NOTEIDX'>0  DO
  . . . SET NOTETEXT=NOTETEXT_$G(^TIU(8925,TIUIEN,"TEXT",NOTEIDX,0))
  . . IF NOTETEXT'["FOLLOWUP" QUIT
  . . WRITE $P($G(^DPT(TMGDFN,0)),"^",1),",",TIUIEN,",",REFDATE,",",ENTRYDT,",",$P($G(^TIU(8925.1,NOTETYPE,0)),"^",1),!
  QUIT
