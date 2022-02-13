TMGSUSMC ;TMG/KST - Handle importing and reporting suspect medical condition; 2/4/22
        ;;1.0;TMG-LIB;**1**; 2/4/22
       ;
 ;"TMG SUSPECT MEDICAL CONDITION FUNCTIONS
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
 ;"
 ;
 ;"=======================================================================
 ;" Private Functions.
 ;"=======================================================================
 ;"
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;" TMGIOUTL,TMGSEQE1,TMGIOUT4
 ;"=======================================================================
 ;
IMPORT
  ;"IMPORT PAYMENT DATA FROM AARP AND ENTER INTO 22749
  ;"NOTE: The original csv file I pulled Practice Assist had too much information so I trimmed it to the below fields
  ;"Patient First Name,Patient Last Name,Patient DOB,Gender,Condition,Suspect Detail,Suspect Condition Date Added,Disposition
  ;"PARSE CSV
  NEW ARRAY,OPTION,OUTARR
  NEW IDX SET IDX=1
  NEW INSARRAY
  NEW PLAN,PAID,CPT,VISIT
  NEW YEAR SET YEAR=2022  ;"HARD CODING FOR NOW. CAN PROMPT USER IF NEED BE
  NEW DATE SET DATE=$$TODAY^TMGDATE
  DO HFS2ARR^TMGIOUT3("/mnt/WinServer","SuspectMedConditions.csv","ARRAY",.OPTION)
  FOR  SET IDX=$O(ARRAY(IDX)) QUIT:IDX'>0  DO
  . NEW LINE SET LINE=$G(ARRAY(IDX))
  . NEW FNAME,LNAME,DOB,SEX
  . SET FNAME=$P(LINE,",",1),LNAME=$P(LINE,",",2)
  . SET DOB=$$INTDATE^TMGDATE($P(LINE,",",3)),SEX=$P(LINE,",",4)
  . NEW TMGDFN,INFO
  . SET INFO("NAME")=LNAME_","_FNAME
  . SET INFO("DOB")=DOB
  . SET INFO("SEX")=SEX
  . SET TMGDFN=$$GETDFN^TMGGDFN(.INFO,0) 
  . IF TMGDFN'>0 QUIT  ;"ERROR HANDLER?
  . NEW CONDITION,ICD,DETAIL,DATEADDED,DISPOSITION,ICDLINE
  . SET ICDLINE=$P(LINE,",",6),CONDITION=$P(LINE,",",5)
  . SET ICD=$$TRIM^XLFSTR($P($P(ICDLINE,"ICD10:",2),":",1)),DETAIL=$P($P(ICDLINE,"ICD10:",2),":",2)
  . SET DATEADDED=$P(LINE,",",7),DISPOSITION=$P(LINE,",",8)
  . NEW TMGFDA,TMGIEN,TMGMSG,TMGIENS,PATIDX
  . ;"Does patient already have entry in 22749?
  . IF $D(^TMG(22749,"B",TMGDFN)) DO
  . . SET PATIDX=+$O(^TMG(22749,"B",TMGDFN,0))
  . . IF PATIDX'>0  WRITE "ERROR PULLING PATIENT IEN FROM B INDEX",! QUIT
  . ELSE  DO
  . . SET TMGFDA(22749,"+1,",.01)=TMGDFN
  . . DO UPDATE^DIE("","TMGFDA","TMGIENS","TMGMSG")
  . . IF $D(TMGMSG("DIERR")) WRITE "ERROR ADDING PATIENT TO 22749",! QUIT
  . . SET PATIDX=+$G(TMGIENS(1))
  . . KILL TMGFDA,TMGIENS,TMGMSG
  . ;"Does the data already exist for this year?
  . NEW ADD SET ADD=1
  . NEW DATAIDX SET DATAIDX=0
  . FOR  SET DATAIDX=$O(^TMG(22749,PATIDX,1,DATAIDX)) QUIT:DATAIDX'>0  DO
  . . NEW THISYEAR SET THISYEAR=$P($G(^TMG(22749,PATIDX,1,DATAIDX,0)),"^",1)
  . . IF THISYEAR'=YEAR QUIT
  . . NEW THISICD SET THISICD=$P($G(^TMG(22749,PATIDX,1,DATAIDX,0)),"^",2)
  . . IF THISICD=ICD SET ADD=0
  . IF ADD=0 WRITE "!! ICD ",ICD," ALREADY EXISTS FOR ",FNAME," ",LNAME,! QUIT
  . SET TMGIENS="+1,"_PATIDX_","
  . SET TMGFDA(22749.01,TMGIENS,.01)=YEAR
  . SET TMGFDA(22749.01,TMGIENS,1)=ICD
  . SET TMGFDA(22749.01,TMGIENS,1.1)=CONDITION
  . SET TMGFDA(22749.01,TMGIENS,1.2)=DETAIL
  . SET TMGFDA(22749.01,TMGIENS,1.3)=$$INTDATE^TMGDATE(DATEADDED)
  . SET TMGFDA(22749.01,TMGIENS,1.4)=DISPOSITION
  . DO UPDATE^DIE("","TMGFDA","TMGIENS","TMGMSG")
  . IF $D(TMGMSG("DIERR")) WRITE "ERROR ADDING "_ICD_" TO PATIENT ",FNAME," ",LNAME,! QUIT
  . KILL TMGFDA,TMGIENS,TMGMSG
  QUIT
  ;"
DAILYREPORT 
   ;"This routine will take today's schedule and determine if any of the patients on it
   ;"   have open suspect medical conditions and then print the report
   NEW %ZIS
   SET %ZIS("A")="Enter Output Device: "
   SET IOP="S121-LAUGHLIN-LASER"
   DO ^%ZIS  ;"standard device call
   IF POP DO  GOTO DRDN
   . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output. Aborting.")
   use IO
   ;"
   NEW APPTARRAY,HEADER,LINES
   SET HEADER=0
   NEW SDT,EDT
   SET SDT=$$TODAY^TMGDATE+0.00001
   SET EDT=$$TODAY^TMGDATE+0.999999
   NEW THISYEAR SET THISYEAR=$E(SDT,1,3)+1700
   DO APPT4DT^TMGSMS05(SDT,EDT,.APPTARRAY,1)
   NEW DATE,TMGDFN SET DATE=0
   FOR  SET DATE=$O(APPTARRAY("DT",DATE)) QUIT:DATE'>0  DO
   . SET TMGDFN=0
   . FOR  SET TMGDFN=$O(APPTARRAY("DT",DATE,TMGDFN)) QUIT:TMGDFN'>0  DO
   . . NEW ARRAY DO GET1PAT(.ARRAY,TMGDFN,THISYEAR)
   . . IF $D(ARRAY) DO
   . . . IF HEADER=0 DO
   . . . . WRITE !
   . . . . WRITE "****************************************************************",!
   . . . . WRITE "          PATIENTS SCHEDULED TODAY WITH OPEN SUSPECT MEDICAL CONDITIONS",!
   . . . . WRITE "                            " WRITE $$TODAY^TMGDATE(1),!
   . . . . WRITE "               Please deliver this report to Eddie",!
   . . . . WRITE "****************************************************************",!
   . . . . WRITE "                                            (From: TMGSUSMC.m)",!,!
   . . . . SET HEADER=1
   . . . WRITE $P($G(^DPT(TMGDFN,0)),"^",1)," (APPT TIME: ",$P($$EXTDATE^TMGDATE(DATE),"@",2),")",!
   . . . NEW IDX SET IDX=0
   . . . NEW COUNT SET COUNT=0
   . . . NEW CONDS SET CONDS=""
   . . . FOR  SET IDX=$O(ARRAY(IDX)) QUIT:IDX'>0  DO
   . . . . SET COUNT=COUNT+1
   . . . . IF CONDS'="" SET CONDS=CONDS_","
   . . . . SET CONDS=CONDS_$P($G(ARRAY(IDX)),"^",2)
   . . . IF COUNT=1 DO
   . . . . WRITE "     -> HAS ",COUNT," CONDITION: ",CONDS,!,!
   . . . ELSE  DO
   . . . . WRITE "     -> HAS ",COUNT," CONDITIONS INCLUDING: ",CONDS,!,!   
DRDN   
   DO ^%ZISC  ;" Close the output device
   QUIT
   ;"
PATSUSMC(TMGDFN)
   ;"This routine is designed to display a single patient's medical conditions for use
   ;"   in a TIU Template
   NEW TMGRESULT SET TMGRESULT=""
   NEW PATIDX SET PATIDX=+$O(^TMG(22749,"B",TMGDFN,0))
   IF PATIDX'>0 GOTO PSMCDN
   NEW X DO NOW^%DTC
   NEW THISYEAR SET THISYEAR=$E(%,1,3)+1700
   NEW IDX SET IDX=0
   NEW HEADING SET HEADING=0
   NEW ARRAY
   DO GET1PAT(.ARRAY,TMGDFN,THISYEAR)
   IF '$D(ARRAY) GOTO PSMCDN
   FOR  SET IDX=$O(ARRAY(IDX)) QUIT:IDX'>0  DO
   . IF HEADING=0 DO
   . . SET TMGRESULT="<table><caption><b>OPEN SUSPECT MEDICAL CONDITIONS</b></caption>"
   . . SET HEADING=1
   . NEW ICD,DESC,ZN,CONDITION
   . SET ZN=$G(ARRAY(IDX))
   . SET ICD=$P(ZN,"^",2),DESC=$P(ZN,"^",4)
   . SET CONDITION=$P(ZN,"^",3)
   . IF ICD="" SET ICD="*NOT PROVIDED*"
   . IF DESC="" SET DESC="*NOT PROVIDED*"
   . IF CONDITION SET CONDITION="*NOT PROVIDED*"
   . ;"SET TMGRESULT=TMGRESULT_"<b>ICD10:</b> "_ICD_" <b>DESC:</b> "_DESC_" <b>CONDITION:</b> "_CONDITION_"<BR>"
   . SET TMGRESULT=TMGRESULT_"<tr><td><b>ICD10:</b>"_ICD_"</td><td> <b>DESC:</b> "_DESC_"</td><td> <b>CONDITION:</b> "_CONDITION_"</td></tr>"
   NEW FOOTER SET FOOTER="<tfoot align=""center""><tr><th colspan=""3""><b>Resolve this via paper form</b></th></tr></tfoot>"
   IF TMGRESULT'="" SET TMGRESULT="{HTML:<FONT style=""BACKGROUND-COLOR:#ff0000"">}"_TMGRESULT_FOOTER_"</table>{HTML:</FONT>}"
PSMCDN
   QUIT TMGRESULT
   ;"
GET1PAT(RESULTARR,TMGDFN,YEAR)  ;"  RETURN ARRAY WITH OPEN SUSPECT MEDICAL CONDITIONS
   NEW PATIDX SET PATIDX=+$O(^TMG(22749,"B",TMGDFN,0))
   NEW IDX SET IDX=0
   FOR  SET IDX=$O(^TMG(22749,PATIDX,1,"B",YEAR,IDX)) QUIT:IDX'>0  DO
   . NEW ZN,ONEN
   . SET ZN=$G(^TMG(22749,PATIDX,1,IDX,0))
   . SET ONEN=$G(^TMG(22749,PATIDX,1,IDX,1))
   . ;"NEW STATUS SET STATUS=$$UP^XLFSTR($P(ONEN,"_",1))
   . ;"IF STATUS'["NOT ASSESSED" QUIT
   . NEW STATUS SET STATUS=$P(ONEN,"^",2)
   . IF STATUS'="" QUIT  ;"IF THERE IS A STATUS, DO NOT INCLUDE
   . SET RESULTARR(IDX)=ZN
   QUIT