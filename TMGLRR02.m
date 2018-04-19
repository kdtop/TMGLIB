TMGLRR02 ;TMG/kst-Reading from LAB DATA file ;2/11/15, 4/1/18
              ;;1.0;TMG-LIB;**1**;2/11/15
 ;
 ;"TMG LAB READING API
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
 ;"GETLABS(OUT,DFN,SDT,EDT,OPTION) --return formatted array containing patient's labs for specified date range. 
 ;"
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;"TEST                
 ;"CONTPNL(OUT,IEN60) -- for a given IEN 60, return all parent lab tests (i.e. 'Panels') that it is a member of
 ;"PARSXTRA(REF,DATA,DIV1,DIV2) -- Parse lab dataline back into XTRA array
 ;"
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;
TEST  ;              
  NEW DIC,X,Y SET DIC=2,DIC(0)="MAEQ" 
  DO ^DIC WRITE !
  IF +Y'>0 QUIT
  NEW LABS
  DO GETLABS(.LABS,+Y)
  DO BROWSENODES^TMGMISC("LABS",1,0,1)
  QUIT
  ;
GETLABS(OUT,DFN,SDT,EDT,OPTION) ;
  ;"Purpose: return formatted array containing patient's labs for specified date range. 
  ;"Input: OUT -- PASS BY REFERENCE.  AN OUT PARAMETER.  Format:
  ;           OLD--> OUT("DT",FMDT,TEST_FLD#)=<data line>
  ;"          OUT("DT",FMDT,IEN60)=<data line>
  ;"              <data line> =  TESTNAME^VAL^UNITS^FLAG^REFLO^REFHI
  ;"          OUT("DT",FMDT,"COMMENT",Line#)=<text of comment>
  ;"          OUT("PANELS",IEN60)=PNLNAME
  ;"          OUT("PANELS",IEN60,FMDT)=""
  ;           OLD-> OUT("TEST","FLD",TEST_FLD#,FMDT)=""  -- Index of test by date. 
  ;"          OUT("TEST","IEN60",IEN60,FMDT)=""  -- Index of test by date. 
  ;           OLD-> OUT("TEST","NAME",<test name>)=TEST_FLD#
  ;"          OUT("TEST","NAME",<test name>)=IEN60
  ;"          OUT("TEST","NAME",<test name>,FMDT)=""
  ;           OLD-> OUT("NAMES",TEST_FLD#)=<test name>
  ;"          OUT("NAMES",IEN60)=<test name>
  ;"       DFN -- Patient IEN
  ;"       SDT -- Start date FMDT format.  Optional.  Default is 0 (earliest)
  ;"       EDT -- End date FMDT format.  Optional.  Default is 9999999 (last possible)
  ;"       OPTION -- optional
  ;"          OPTION("NO COMMENTS")=1  -- don't return comments
  ;"          OPTION("NO TEST NAME INDEX")=1  -- don't return TEST NAMES node
  ;"          OPTION("NO TEST FLD INDEX")=1  -- don't return TEST FLD node
  ;"          OPTION("NO NAMES INDEX")=1  -- don't return NAMES node
  ;"          OPTION("NO DATES")=1  -- don't return DT node
  ;"          OPTION("NO PANELS")=1  -- don't return PANELS node
  SET DFN=+$GET(DFN)
  NEW LRDFN SET LRDFN=+$GET(^DPT(DFN,"LR"))
  NEW SKIPCOMMENT SET SKIPCOMMENT=+$GET(OPTION("NO COMMENTS"))
  NEW SKIPTSTNAMEIDX SET SKIPTSTNAMEIDX=+$GET(OPTION("NO TEST NAME INDEX"))
  NEW SKIPTSTFLDIDX SET SKIPTSTFLDIDX=+$GET(OPTION("NO TEST FLD INDEX"))
  NEW SKIPNAMESIDX SET SKIPNAMESIDX=+$GET(OPTION("NO NAMES INDEX"))
  NEW SKIPDATES SET SKIPDATES=+$GET(OPTION("NO DATES"))
  NEW SKIPPANELS SET SKIPPANELS=+$GET(OPTION("NO PANELS"))
  NEW PANELS,PANELCONTAINS,PANELINSTANCE
  SET SDT=+$GET(SDT)
  SET EDT=+$GET(EDT) IF EDT'>0 SET EDT=9999999
  NEW RSDT SET RSDT=$$FMDT2RDT^TMGLRWU1(EDT)
  NEW REDT SET REDT=$$FMDT2RDT^TMGLRWU1(SDT)
  NEW RDT SET RDT=RSDT-0.000001 IF RDT<0 SET RDT=0
  FOR  SET RDT=$ORDER(^LR(LRDFN,"CH",RDT)) QUIT:(+RDT'>0)!(RDT>REDT)  DO
  . NEW ADT SET ADT=$$RDT2FMDT^TMGLRWU1(RDT)
  . NEW TESTFLD SET TESTFLD=1  ;"TEST 1 = COMMENT
  . NEW CLINE SET CLINE=0
  . IF 'SKIPCOMMENT FOR  SET CLINE=$ORDER(^LR(LRDFN,"CH",RDT,TESTFLD,CLINE)) QUIT:+CLINE'>0  DO
  . . NEW LINE SET LINE=$GET(^LR(LRDFN,"CH",RDT,TESTFLD,CLINE,0))
  . . SET OUT("DT",ADT,"COMMENT",CLINE)=LINE
  . FOR  SET TESTFLD=$ORDER(^LR(LRDFN,"CH",RDT,TESTFLD)) QUIT:+TESTFLD'>0  DO
  . . NEW TESTNAME SET TESTNAME=$PIECE($GET(^DD(63.04,TESTFLD,0)),"^",1)
  . . NEW DATA SET DATA=$GET(^LR(LRDFN,"CH",RDT,TESTFLD))
  . . NEW ARR DO PARSXTRA("ARR",DATA)
  . . NEW IEN60 SET IEN60=+$GET(ARR(3,7))
  . . NEW LABNAME SET LABNAME=$PIECE($GET(^LAB(60,IEN60,0)),"^",1)
  . . NEW PANELARR DO CONTPNL(.PANELARR,IEN60)
  . . IF 'SKIPPANELS DO
  . . . ;"Format PANELS(TESTFLD,PNLIEN60)=<panel_name>
  . . . ;"Format PANELCONTAINS(PNLIEN60,HELDIEN60)=HELD_LAB_NAME^COUNT_ENCOUNTERED
  . . . MERGE PANELS(TESTFLD)=PANELARR  
  . . . NEW PNLIEN60 SET PNLIEN60=0
  . . . FOR  SET PNLIEN60=$ORDER(PANELARR(PNLIEN60)) QUIT:+PNLIEN60'>0  DO
  . . . . IF $DATA(PANELCONTAINS(PNLIEN60))'>0 DO
  . . . . . NEW PNLNAME SET PNLNAME=$PIECE($GET(^LAB(60,PNLIEN60,0)),"^",1)
  . . . . . SET PANELCONTAINS(PNLIEN60)=PNLNAME
  . . . . . NEW SUBIEN SET SUBIEN=0
  . . . . . FOR  SET SUBIEN=$ORDER(^LAB(60,PNLIEN60,2,SUBIEN)) QUIT:+SUBIEN'>0  DO
  . . . . . . NEW HELDIEN SET HELDIEN=$PIECE($GET(^LAB(60,PNLIEN60,2,SUBIEN,0)),"^",1)
  . . . . . . NEW HELDNAME SET HELDNAME=$PIECE($GET(^LAB(60,HELDIEN,0)),"^",1)
  . . . . . . SET PANELCONTAINS(PNLIEN60,HELDIEN)=HELDNAME_"^0"
  . . . . NEW CT SET CT=$PIECE($GET(PANELCONTAINS(PNLIEN60,IEN60)),"^",2)+1
  . . . . SET $PIECE(PANELCONTAINS(PNLIEN60,IEN60),"^",2)=CT
  . . . . SET PANELINSTANCE(PNLIEN60,ADT)=""
  . . NEW VAL SET VAL=$GET(ARR(1))
  . . NEW FLAG SET FLAG=$GET(ARR(2))
  . . NEW REFLO SET REFLO=$GET(ARR(5,2))
  . . NEW REFHI SET REFHI=$GET(ARR(5,3))
  . . NEW UNITS SET UNITS=$GET(ARR(5,7))
  . . NEW DATASTR SET DATASTR=TESTNAME_"^"_VAL_"^"_UNITS_"^"_FLAG_"^"_REFLO_"^"_REFHI
  . . IF 'SKIPDATES SET OUT("DT",ADT,IEN60)=DATASTR
  . . ;"IF 'SKIPDATES SET OUT("DT",ADT,TESTFLD)=DATASTR
  . . IF 'SKIPTSTFLDIDX DO
  . . . ;"SET OUT("TEST","FLD",TESTFLD,ADT)=""
  . . . SET OUT("TEST","IEN60",IEN60,ADT)=""
  . . IF 'SKIPTSTNAMEIDX DO
  . . . ;"SET OUT("TEST","NAME",TESTNAME,ADT)=""
  . . . SET OUT("TEST","NAME",LABNAME,ADT)=""
  . . . ;"SET OUT("TEST","NAME",TESTNAME)=TESTFLD
  . . . SET OUT("TEST","NAME",LABNAME)=IEN60
  . . IF 'SKIPNAMESIDX SET OUT("NAMES",TESTFLD)=TESTNAME
  IF SKIPPANELS GOTO GLDN
  ;"Analysis of panels
  NEW KILLEDPANELS
  NEW PNLIEN60 SET PNLIEN60=0
  FOR  SET PNLIEN60=$ORDER(PANELCONTAINS(PNLIEN60)) QUIT:+PNLIEN60'>0  DO
  . NEW TOTAL,FOUND,HELDIEN60 SET (TOTAL,FOUND,HELDIEN60)=0
  . FOR  SET HELDIEN60=$ORDER(PANELCONTAINS(PNLIEN60,HELDIEN60)) QUIT:+HELDIEN60'>0  DO
  . . SET TOTAL=TOTAL+1
  . . NEW CT SET CT=$PIECE($GET(PANELCONTAINS(PNLIEN60,HELDIEN60)),"^",2)
  . . IF CT>0 SET FOUND=FOUND+1
  . IF FOUND/TOTAL<0.5 DO
  . . KILL PANELCONTAINS(PNLIEN60)  ;"If < 50% of panel members present, don't report panel
  . . SET KILLEDPANELS(PNLIEN60)=""
  NEW TESTFLD SET TESTFLD=0
  FOR  SET TESTFLD=$ORDER(PANELS(TESTFLD)) QUIT:+TESTFLD'>0  DO
  . NEW PNLIEN60 SET PNLIEN60=0
  . FOR  SET PNLIEN60=$ORDER(PANELS(TESTFLD,PNLIEN60)) QUIT:+PNLIEN60'>0  DO
  . . IF $DATA(KILLEDPANELS(PNLIEN60)) QUIT
  . . SET OUT("PANELS",PNLIEN60)=""
  NEW PNLIEN60 SET PNLIEN60=0
  FOR  SET PNLIEN60=$ORDER(OUT("PANELS",PNLIEN60)) QUIT:+PNLIEN60'>0  DO
  . NEW PNLNAME SET PNLNAME=$PIECE($GET(^LAB(60,PNLIEN60,0)),"^",1)
  . SET OUT("PANELS",PNLIEN60)=PNLNAME
  . MERGE OUT("PANELS",PNLIEN60)=PANELINSTANCE(PNLIEN60)
GLDN ;  
  QUIT
  ;
CONTPNL(OUT,IEN60) ;"CONTAINING PANELS
  ;"Purpose: for a given IEN 60, return all parent lab tests (i.e. 'Panels') that it is a member of
  ;"Input: OUT -- PASS BY REFERENCE, AN OUT PARAMETER.  Format:
  ;"          OUT(PanelIEN60)=<panel/test name>
  ;"       IEN60 -- IEN of lab test
  NEW IEN SET IEN=0
  FOR  SET IEN=$ORDER(^LAB(60,"AB",IEN60,IEN)) QUIT:+IEN'>0  DO
  . NEW NAME SET NAME=$PIECE($GET(^LAB(60,IEN,0)),"^",1)
  . SET OUT(IEN)=NAME
  QUIT
  ;
PARSXTRA(REF,DATA,DIV1,DIV2) ;"Parse lab dataline back into XTRA array
  ;"Input: REF -- PASS BY NAME.  OUTPUT XTRA array
  ;"       DATA -- The data line to be parsed
  ;"       DIV1 -- OPTIONAL, divider marker. DEFAULT="^"
  ;"       DIV2 -- OPTIONAL, divider marker. DEFAULT="!"
  ;"NOTE: a reverse of this function can be achieved via COMPLXTR^TMGLRW01
  ;"      See PREPOBX^TMGLRW01 for interpretation of array elements.  
  SET DIV1=$GET(DIV1,"^")
  SET DIV2=$GET(DIV2,"!")
  NEW IDX1 FOR IDX1=1:1:$LENGTH(DATA,DIV1) DO
  . NEW PART SET PART=$PIECE(DATA,DIV1,IDX1)
  . IF PART'[DIV2 SET @REF@(IDX1)=PART QUIT
  . SET @REF@(IDX1)=DIV2
  . NEW IDX2 FOR IDX2=1:1:$LENGTH(PART,DIV2) DO
  . . SET @REF@(IDX1,IDX2)=$PIECE(PART,DIV2,IDX2)
  QUIT
  ;
  
