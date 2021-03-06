TMGRPCL1 ;TMG/kst/Lab RPC Call Routines ; 3/21/15
         ;;1.0;TMG-LIB;**1**;3/21/15
 ;
 ;"TMG LAB RPC ROUTINES
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
 ;" Private Functions.
 ;"=======================================================================
 ;"
 ;"=======================================================================
 ;"Dependancies:
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;
TEST ;
  NEW DIR,DIC,X,Y,DFN,SDT,EDT,ARR
  SET DIC=2,DIC(0)="MAEQ"
  DO ^DIC WRITE !
  IF Y'>0 QUIT
  SET DFN=+Y
  SET DIR(0)="D"
  SET DIR("A")="Enter starting date"
  DO ^DIR IF Y'>0 QUIT
  SET SDT=+Y
  SET DIR("A")="Enter ending date"
  DO ^DIR WRITE !
  IF Y'>0 QUIT
  SET EDT=+Y\1+0.999999
  DO GETLABS(.ARR,DFN,SDT,EDT)
  IF $DATA(ARR) ZWR ARR
  ELSE  WRITE !,"None found.",!
  QUIT
  ;   
GETLDATS(OUT,DFN,FMDT) ;"
  ;"Purpose: Get an array of all dates a patient has lab results for
  ;"OUT(IDX)=DATE 
  SET FMDT=+$G(FMDT)   ;"RETURN FM DATE 
  NEW LABS,LDATE,CURDATE,IDX
  SET LDATE=0,CURDATE=9999999,IDX=0
  NEW OPTION SET OPTION("ADD SUMMARY")=1
  DO GETLABS^TMGLRR02(.LABS,.DFN,.SDT,.EDT,.OPTION)
  FOR  SET CURDATE=$O(LABS("DT",CURDATE),-1) QUIT:CURDATE'>0  DO
  . NEW THISDATE SET THISDATE=$P(CURDATE,".",1)
  . IF THISDATE=CURDATE QUIT
  . SET CURDATE=THISDATE
  . IF FMDT=1 DO
  . . SET OUT(IDX)=CURDATE_"^"_$$TSTNAMES(.LABS,CURDATE) 
  . ELSE  DO
  . . SET OUT(IDX)=$$EXTDATE^TMGDATE(CURDATE)_"-"_$$TSTNAMES(.LABS,CURDATE)
  . SET IDX=IDX+1
  QUIT
  ;"
TSTNAMES(LABARR,CURDATE)
  NEW TMGRESULT SET TMGRESULT=""
  NEW IDX SET IDX=0
  NEW TEMPDATE SET TEMPDATE=CURDATE-.000001
  FOR  SET TEMPDATE=$O(LABARR("DT",TEMPDATE)) QUIT:(TEMPDATE'>0)!(TEMPDATE'[CURDATE)  DO
  . FOR  SET IDX=$O(LABARR("DT",TEMPDATE,IDX)) QUIT:IDX'>0  DO
  . . NEW TESTNAME SET TESTNAME=$P($G(LABARR("DT",TEMPDATE,IDX)),"^",1)
  . . IF TMGRESULT'="" SET TMGRESULT=TMGRESULT_","
  . . SET TMGRESULT=TMGRESULT_TESTNAME
  QUIT TMGRESULT
  ;"
FRMTDTS(INPUT,OUTPUT)
  NEW IDX SET IDX=0
  FOR  SET IDX=$O(INPUT(IDX)) QUIT:IDX'>0  DO
  . IF $G(INPUT(IDX))'["-" DO
  . . SET OUTPUT(IDX)=""   ;"DOESN'T NEED TO BE CONVERTED
  . ELSE  DO
  . . SET OUTPUT($$INTDATE^TMGDATE($P($G(INPUT(IDX)),"-",1)))=""
  QUIT
  ;"
GETREPRT(OUT,DFN,ARRAY) ;"
  ;"Purpose: take an array of dates (ARRAY) and format the data in HTML for
  ;"         printing or viewing
  NEW RESULTS,LABS
  NEW DATEARR
  DO FRMTDTS(.ARRAY,.DATEARR)
  DO GETLABS^TMGLRR02(.LABS,.DFN,.SDT,.EDT,.OPTION)
  NEW IDX SET IDX=2
  
  SET OUT(0)="<!DOCTYPE html>"
  SET OUT(1)="<html><head><title>Page Title</title></head><body><font size=""2"">"
  ;"SET OUT(2)="<table BORDER=1>"
  NEW DT SET DT=99999999
  NEW STR
  FOR  SET DT=$ORDER(LABS("DT",DT),-1) QUIT:(DT="")  DO
  . NEW DAY SET DAY=$P(DT,".",1)
  . IF '$D(DATEARR(DAY)) QUIT
  . ;"SET OUT(IDX)="<TABLE BORDER=2 WIDTH=""600"">",IDX=IDX+1
  . ;"SET OUT(IDX)="<CAPTION><B>"_$$EXTDATE^TMGDATE(DT)_"</B></CAPTION>",IDX=IDX+1
  . ;"SET OUT(IDX)=$$HEADER(),IDX=IDX+1
  . NEW SETHEAD SET SETHEAD=0
  . NEW NODE SET NODE=""
  . NEW COMMENT SET COMMENT=""
  . NEW COUNT SET COUNT=0
  . FOR  SET NODE=$ORDER(LABS("DT",DT,NODE)) QUIT:(NODE="")  DO
  . . IF +NODE=NODE DO
  . . . IF SETHEAD=0 DO
  . . . . IF IDX>2 DO
  . . . . . SET OUT(IDX)="<p style=""page-break-before: always"">",IDX=IDX+1
  . . . . DO CAPTION(.OUT,.IDX,DT,DFN)
  . . . . SET SETHEAD=1
  . . . ;"SET STR="LAB^"_DT_"^"_NODE_"^"_$GET(LABS("DT",DT,NODE))
  . . . NEW ROWHEAD 
  . . . IF $P($GET(LABS("DT",DT,NODE)),"^",4)'="" DO
  . . . . SET ROWHEAD="<TR bgcolor=""#FF0000"">"
  . . . ELSE  DO
  . . . . SET ROWHEAD="<TR>"
  . . . SET STR=ROWHEAD_$$U2CELL($GET(LABS("DT",DT,NODE)))_"</TR>"
  . . . SET COUNT=COUNT+1
  . . . SET OUT(IDX)=STR,IDX=IDX+1
  . . ELSE  IF NODE="COMMENT" DO
  . . . NEW JDX SET JDX=0
  . . . FOR  SET JDX=$ORDER(LABS("DT",DT,"COMMENT",JDX)) QUIT:+JDX'>0  DO
  . . . . ;"SET STR="LAB^"_DT_"^COMMENT^"_JDX_"^"_$GET(LABS("DT",DT,"COMMENT",JDX))
  . . . . IF COMMENT'="" SET COMMENT=COMMENT_"<BR>"
  . . . . SET COMMENT=COMMENT_$GET(LABS("DT",DT,"COMMENT",JDX))
  . . . . ;"SET OUT(IDX)=STR,IDX=IDX+1     
  . IF (COMMENT'="")&(COUNT>0) DO
  . . SET OUT(IDX)="<tr><td colspan=""6"">"_COMMENT_"</td></tr>",IDX=IDX+1
  . ;"SET OUT(3)=$G(ARRAY(1))
  . SET OUT(IDX)="</td></tr></table><BR>",IDX=IDX+1
  SET OUT(IDX)="</font></body></html>" 
  QUIT
  ;"
CAPTION(OUT,IDX,LABDATE,DFN)
  NEW NAME,DOB,AGE,DT
  SET LABDATE=$P(LABDATE,".",1)
  SET NAME=$P($G(^DPT(DFN,0)),"^",1)
  SET DOB=$$EXTDATE^TMGDATE($P($G(^DPT(DFN,0)),"^",3))
  K VADM SET AGE=$$AGE^TIULO(DFN)
  SET OUT(IDX)="<DIV align left>",IDX=IDX+1
  SET OUT(IDX)="<TABLE width=""50%"" border=""0"" cellspacing=""0""",IDX=IDX+1
  SET OUT(IDX)="cellpadding=""1"" style=""background-color:gray"">",IDX=IDX+1
  SET OUT(IDX)="<TR valign=""bottom"" align=""left"">",IDX=IDX+1
  SET OUT(IDX)="<TD nowrap><B>Patient: "_NAME_"</B></TD>",IDX=IDX+1
  SET OUT(IDX)="<TD nowrap><B>DOB: "_DOB_"</B></TD>",IDX=IDX+1
  SET OUT(IDX)="<TD nowrap><B>Age: "_AGE,IDX=IDX+1
  SET OUT(IDX)="</B></TD>",IDX=IDX+1
  SET OUT(IDX)="</TR></TABLE></DIV><HR>",IDX=IDX+1
  SET OUT(IDX)="<TABLE BORDER=1 WIDTH=""600"">",IDX=IDX+1
  SET OUT(IDX)="<CAPTION><B>"_$$EXTDATE^TMGDATE(LABDATE)_"</B></CAPTION>",IDX=IDX+1
  SET OUT(IDX)=$$HEADER(),IDX=IDX+1
  QUIT
  ;"
HEADER()
  QUIT "<TH width=""50%"">LAB NAME</TH><TH width=""10%"">RESULT</TH><TH width=""10%"">UNITS</TH><TH width=""10%"">FLAG</TH><TH width=""10%"">REF LOW</TH><TH width=""10%"">REF HIGH</TH>"
  ;"
U2CELL(LINE) ;"CONVERT STRING WITH CAROT TO HTML TABLE CELL
  NEW DONE SET DONE=0
  SET LINE="<TD>"_$$REPLSTR^TMGSTUT3(LINE,"^","</TD><TD>")_"</TD>"
  SET LINE=$$REPLSTR^TMGSTUT3(LINE,"<TD></TD>","<TD>&nbsp;</TD>")  
  QUIT LINE
  ;"
GETLABS(OUT,DFN,SDT,EDT,NCM,NTNX,NTFX,NNMX,NDT,NPNL) ;
  ;"Purpose: return formatted array containing patient's labs for specified date range. 
  ;"Input: OUT -- PASS BY REFERENCE.  AN OUT PARAMETER.  Format:
  ;"          OUT(0)="1^Success"
  ;"          OUT(#)="LAB^<FMDT>^<IEN60>^TESTNAME^VAL^UNITS^FLAG^REFLO^REFHI
  ;"          OUT(#)="LAB^<FMDT>^COMMENT^<Line#>^<text of comment>"
  ;"          OUT(#)="PANELS^<IEN60>^NAME^<PNLNAME>"
  ;"          OUT(#)="PANELS^<IEN60>^<FMDT>"  <-- FMDT will be unique to 1 lab group / panel
  ;"          OUT(#)="PANELS^<IEN60>^CONTAINS^<DISP SEQ#>^<HELD IEN60>^<Held IEN60-Name>"
  ;          old-> OUT(#)="TEST^IEN60^<IEN60>^<FMDT>"  -- Index of test by date. 
  ;          old-> OUT(#)="TEST^NAME^<TEST_NAME>^IEN60^<IEN60>
  ;          old-> OUT(#)="TEST^NAME^<TEST_NAME>^<FMDT>"
  ;          old-> OUT(#)="NAMES^<TEST_FLD#>^<test name>"
  ;"          NOTE: the order shown above is the order result will return in.
  ;"       1 DFN -- Patient IEN
  ;"       2 SDT -- Optional.  Start date FMDT format.  Default is 0 (earliest)
  ;"       3 EDT -- Optional.  End date FMDT format.  Default is 9999999 (last possible)
  ;"       4 NCM  -- Optional.  if 1 then don't return comment
  ;"       5 NTNX -- Optional.  if 1 then don't return TEST NAMES index node
  ;"       6 NTFX -- Optional.  if 1 then don't return TEST FLD index node
  ;"       7 NNMX -- Optional.  if 1 then don't return NAMES index
  ;"       8 NDT -- Optional.  if 1 then don't return DT index
  ;"       9 NPNL  -- Optional.  if 1 then don't return PANELS index
  ;"Result: none
  NEW LABS,STR
  NEW OPTION
  IF $GET(NCM)=1 SET OPTION("NO COMMENTS")=1
  IF $GET(NTNX)=1 SET OPTION("NO TEST NAME INDEX")=1  ;"obsolete
  IF $GET(NTFX)=1 SET OPTION("NO TEST FLD INDEX")=1   ;"obsolete
  IF $GET(NNMX)=1 SET OPTION("NO NAMES INDEX")=1      ;"obsolete
  IF $GET(NDT)=1 SET OPTION("NO DATES")=1
  IF $GET(NPNL)=1 SET OPTION("NO PANELS")=1
  DO GETLABS^TMGLRR02(.LABS,.DFN,.SDT,.EDT,.OPTION)
  NEW IDX SET IDX=1
  NEW DT SET DT=0
  FOR  SET DT=$ORDER(LABS("DT",DT)) QUIT:(DT="")  DO
  . NEW NODE SET NODE=""
  . FOR  SET NODE=$ORDER(LABS("DT",DT,NODE)) QUIT:(NODE="")  DO
  . . IF +NODE=NODE DO
  . . . SET STR="LAB^"_DT_"^"_NODE_"^"_$GET(LABS("DT",DT,NODE))
  . . . SET OUT(IDX)=STR,IDX=IDX+1
  . . ELSE  IF NODE="COMMENT" DO
  . . . NEW JDX SET JDX=0
  . . . FOR  SET JDX=$ORDER(LABS("DT",DT,"COMMENT",JDX)) QUIT:+JDX'>0  DO
  . . . . SET STR="LAB^"_DT_"^COMMENT^"_JDX_"^"_$GET(LABS("DT",DT,"COMMENT",JDX))
  . . . . SET OUT(IDX)=STR,IDX=IDX+1
  NEW IEN60 SET IEN60=0
  FOR  SET IEN60=$ORDER(LABS("PANELS",IEN60)) QUIT:+IEN60'>0  DO
  . SET STR="PANELS^"_IEN60_"^NAME^"_$GET(LABS("PANELS",IEN60))
  . SET OUT(IDX)=STR,IDX=IDX+1
  . SET DT=0 FOR  SET DT=$ORDER(LABS("PANELS",IEN60,DT)) QUIT:+DT'>0  DO
  . . SET STR="PANELS^"_IEN60_"^"_DT
  . . SET OUT(IDX)=STR,IDX=IDX+1
  . NEW SEQ SET SEQ=0
  . FOR  SET SEQ=$ORDER(^LAB(60,IEN60,2,SEQ)) QUIT:+SEQ'>0  DO
  . . NEW HELDIEN60 SET HELDIEN60=$PIECE($GET(^LAB(60,IEN60,2,SEQ,0)),"^",1)
  . . NEW HELDNAME SET HELDNAME=$PIECE($GET(^LAB(60,HELDIEN60,0)),"^",1)
  . . SET STR="PANELS^"_IEN60_"^CONTAINS^"_SEQ_"^"_HELDIEN60_"^"_HELDNAME
  . . SET OUT(IDX)=STR,IDX=IDX+1
  ;"NEW NODE SET NODE=""
  ;"FOR  SET NODE=$ORDER(LABS("TEST",NODE)) QUIT:NODE=""  DO
  ;". IF NODE="IEN60" DO
  ;". . NEW IEN60 SET IEN60=""
  ;". . FOR  SET IEN60=$ORDER(LABS("TEST","IEN60",IEN60)) QUIT:+IEN60'>0  DO
  ;". . . NEW DT SET DT=0
  ;". . . FOR  SET DT=$ORDER(LABS("TEST","IEN60",IEN60,DT)) QUIT:+DT'>0  DO
  ;". . . . SET STR="TEST^IEN60^"_IEN60"^"_DT
  ;". . . . SET OUT(IDX)=STR,IDX=IDX+1
  ;". IF NODE="FLD" DO  ;"<-- OLD
  ;". . NEW FLDNUM SET FLDNUM=""
  ;". . FOR  SET FLDNUM=$ORDER(LABS("TEST","FLD",FLDNUM)) QUIT:+FLDNUM'>0  DO
  ;". . . NEW DT SET DT=0
  ;". . . FOR  SET DT=$ORDER(LABS("TEST","FLD",FLDNUM,DT)) QUIT:+DT'>0  DO
  ;". . . . SET STR="TEST^FLD^"_FLDNUM_"^"_DT
  ;". . . . SET OUT(IDX)=STR,IDX=IDX+1
  ;". ELSE  IF NODE="NAME" DO
  ;". . NEW LABNAME SET LABNAME=""
  ;". . FOR  SET LABNAME=$ORDER(LABS("TEST","NAME",LABNAME)) QUIT:LABNAME=""  DO
  ;". . . NEW IEN60 SET IEN60=$GET(LABS("TEST","NAME",LABNAME))
  ;". . . SET STR="TEST^NAME^"_LABNAME_"^IEN60^"_IEN60
  ;". . . SET OUT(IDX)=STR,IDX=IDX+1
  ;". . . NEW DT SET DT=0
  ;". . . FOR  SET DT=$ORDER(LABS("TEST","NAME",LABNAME,DT)) QUIT:+DT'>0  DO
  ;". . . . SET STR="TEST^NAME^"_LABNAME_"^"_DT
  ;". . . . SET OUT(IDX)=STR,IDX=IDX+1
  ;"NEW FLDNUM SET FLDNUM=0
  ;"FOR  SET FLDNUM=$ORDER(LABS("NAMES",FLDNUM)) QUIT:FLDNUM=""  DO  
  ;". SET STR="NAMES^"_FLDNUM_"^"_$GET(LABS("NAMES",FLDNUM))
  QUIT
  ;"
CHKVIEW(SDT,EDT)  ;"Check all labs for a date range to make sure they have
                  ;"been viewed by the ordering provider
  NEW LRDFN SET LRDFN=0
  NEW RSDT SET RSDT=$$FMDT2RDT^TMGLRWU1(EDT)
  NEW REDT SET REDT=$$FMDT2RDT^TMGLRWU1(SDT)
  NEW ERRARRAY
  FOR  SET LRDFN=$O(^LR(LRDFN)) QUIT:LRDFN'>0  DO
  . NEW RDT SET RDT=RSDT-0.000001 IF RDT<0 SET RDT=0
  . FOR  SET RDT=$ORDER(^LR(LRDFN,"CH",RDT)) QUIT:(+RDT'>0)!(RDT>REDT)  DO
  . . ;"WRITE LRDFN,"-",RDT,!
  . . NEW DFN SET DFN=$O(^DPT("ATMGLR",LRDFN,0))
  . . ;"WRITE "    PATIENT:",$P($G(^DPT(DFN,0)),"^",1),"-",DFN,!
  . . NEW DATE SET DATE=$$RDT2FMDT^TMGLRWU1(RDT)
  . . ;"WRITE "    DATE:",DATE,!
  . . NEW IDX SET IDX=+$O(^TMG(22732,"B",DFN,0))
  . . IF IDX'>0 WRITE "NOT FOUND",! QUIT
  . . NEW TMGIDX SET TMGIDX=+$O(^TMG(22732,IDX,1,"B",DATE,0))
  . . IF TMGIDX'>0 DO  QUIT
  . . . ;"SET ERRARRAY(DFN,DATE)=""
  . . . SET ERRARRAY($P($G(^DPT(DFN,0)),"^",1),DATE)=""
  . . . ;"WRITE "PATIENT:",$P($G(^DPT(DFN,0)),"^",1),"-",DFN,!
  . . . ;"WRITE "    DATE:",DATE,!
  . . . ;"WRITE "!!!!!! NOT REVIEWED  !!!!!!!",!
  . . NEW TMGDUZ SET TMGDUZ=0
  . . NEW REVIEWED SET REVIEWED=0
  . . FOR  SET TMGDUZ=$O(^TMG(22732,IDX,1,TMGIDX,1,"B",TMGDUZ)) QUIT:TMGDUZ'>0  DO
  . . . IF TMGDUZ=83 DO
  . . . . ;"WRITE "   ---->>>>REVIEWED BY DR DEE",!
  . . . . SET REVIEWED=1
  . . . IF TMGDUZ=168 DO
  . . . . ;"WRITE "   ---->>>>REVIEWED BY DR KEVIN",!
  . . . . SET REVIEWED=1
  . . IF REVIEWED=0 DO
  . . . ;"SET ERRARRAY(DFN,DATE)=""
  . . . SET ERRARRAY($P($G(^DPT(DFN,0)),"^",1),DATE)=""
  . . . ;"WRITE "PATIENT:",$P($G(^DPT(DFN,0)),"^",1),"-",DFN,!
  . . . ;"WRITE "    DATE:",DATE,!
  . . . ;"WRITE "!!!!!! NOT REVIEWED  !!!!!!!",!
  . . ;NEW PROVIDER SET PROVIDER=$P($G(^LR(LRDFN,"CH",RDT,0)),"^",4)
  . . ;SET PROVIDER=$P($G(^VA(200,PROVIDER,0)),"^",1)
  . . ;WRITE "      ->",PROVIDER,!
  NEW %ZIS
  SET %ZIS("A")="Enter Output Device: "
  SET %ZIS("B")="HOME"
  DO ^%ZIS  ;"standard device call
  IF POP DO  QUIT
  . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output.      Aborting.")
  USE IO
  IF $D(ERRARRAY) DO
  . WRITE "UNREVIEWED LABS FOR DATES:  ",$$EXTDATE^TMGDATE(SDT),"-",$$EXTDATE^TMGDATE(EDT),!
  . WRITE "----------------------------------------------------",!,!
  . NEW COUNT SET COUNT=0
  . NEW DFN SET DFN=0
  . NEW NAME SET NAME=""
  . ;"FOR  SET DFN=$O(ERRARRAY(DFN)) QUIT:DFN'>0  DO
  . FOR  SET NAME=$O(ERRARRAY(NAME)) QUIT:NAME=""  DO
  . . SET COUNT=COUNT+1
  . . WRITE "PATIENT:",NAME,!
  . . NEW DATE SET DATE=0
  . . WRITE "DATE: "
  . . NEW LASTDATE SET LASTDATE=0
  . . FOR  SET DATE=$O(ERRARRAY(NAME,DATE)) QUIT:(DATE'>0)!(DATE="")  DO
  . . . NEW TEMPDATE SET TEMPDATE=$P($$EXTDATE^TMGDATE(DATE,1),"@",1)
  . . . IF TEMPDATE=LASTDATE QUIT
  . . . IF LASTDATE'=0 WRITE ", "
  . . . WRITE TEMPDATE," ORDERED BY ",$G(ERRARRAY(NAME,DATE))
  . . . SET LASTDATE=TEMPDATE
  . . WRITE !,!
  . WRITE COUNT," PATIENTS WITH UNREVIEWED LAB RESULTS",!
  DO ^%ZISC  ;" Close the output device
  DO PRESS2GO^TMGUSRI2
  QUIT
  ;"
HASPDF(OUT,DFN,SDT,EDT)  ;"RPC FOR HAS LAB PDF for given date range?
  DO RPCHASPDF^TMGLRPD1(.OUT,.DFN,.SDT,.EDT)
  QUIT
  ;