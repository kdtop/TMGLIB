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
PICKIMPT
 NEW PATH,FILE
 ;"GET FILE
 DO IMPORT(PATH,FILE)
 QUIT
 ;"
IMPORT(PATH,FILE)
  ;"IMPORT PAYMENT DATA FROM AARP AND ENTER INTO 22749
  ;"NOTE: The original csv file I pulled Practice Assist had too much information so I trimmed it to the below fields
  ;"Patient First Name,Patient Last Name,Patient DOB,Gender,Condition,Suspect Detail,Suspect Condition Date Added,Disposition
  ;"PARSE CSV
  ;"SET PATH=$G(PATH),FILE=$G(FILE)
  ;"IF (PATH="")!(FILE="") WRITE "NO FILE SELECTED. QUITTING" QUIT
  NEW ARRAY,OPTION,OUTARR
  NEW IDX SET IDX=1
  NEW INSARRAY
  NEW PLAN,PAID,CPT,VISIT
  NEW YEAR SET YEAR=2025  ;"HARD CODING FOR NOW. CAN PROMPT USER IF NEED BE
  NEW DATE SET DATE=$$TODAY^TMGDATE
  DO HFS2ARR^TMGIOUT3("/mnt/WinServer","SuspectMedConditions.csv","ARRAY",.OPTION)
  FOR  SET IDX=$O(ARRAY(IDX)) QUIT:IDX'>0  DO
  . NEW LINE SET LINE=$G(ARRAY(IDX))
  . ;"SET LINE=$$REPLSTR^TMGSTUT3(LINE,"""","^")
  . NEW FNAME,LNAME,DOB,SEX
  . ;"SET FNAME=$P(LINE,"""",2),LNAME=$P(LINE,"""",4)
  . SET FNAME=$P(LINE,",",2),LNAME=$P(LINE,",",1)
  . SET LNAME=$P(LNAME," ",1)
  . ;"SET DOB=$$INTDATE^TMGDATE($P(LINE,"""",6)),SEX=$P(LINE,"""",8)
  . SET DOB=$$INTDATE^TMGDATE($P(LINE,",",3));",SEX=$P(LINE,",",4)
  . NEW TMGDFN,INFO
  . SET INFO("NAME")=$$TRIM^XLFSTR(LNAME)_","_$$TRIM^XLFSTR(FNAME)
  . SET INFO("DOB")=DOB
  . ;"The Optum export is not sending Sex. Since that is required, I am going to try it both ways
  . SET INFO("SEX")="MALE"
  . SET TMGDFN=$$GETDFN^TMGGDFN(.INFO,0)
  . IF TMGDFN'>0 DO   ;"try it with Female
  . . SET INFO("SEX")="FEMALE"
  . . SET TMGDFN=$$GETDFN^TMGGDFN(.INFO,0)
  . IF TMGDFN'>0 DO  QUIT  ;"ERROR HANDLER?
  . . WRITE "COULD NOT FIND ",LNAME,",",FNAME," IN THE SYSTEM",!
  . NEW CONDITION,ICD,DETAIL,DATEADDED,DISPOSITION,ICDLINE
  . SET DETAIL=$P(LINE,",",7),CONDITION=$P(LINE,",",4)    ;"DETAIL WAS ICDLINE
  . FOR  QUIT:DETAIL'["@@"  DO
  . . SET DETAIL=$PIECE(DETAIL,"@@",1)_","_$PIECE(DETAIL,"@@",2,999)
  . FOR  QUIT:CONDITION'["@@"  DO
  . . SET CONDITION=$PIECE(CONDITION,"@@",1)_","_$PIECE(CONDITION,"@@",2,999)
  . ;"SET ICD=$$TRIM^XLFSTR($P($P(ICDLINE,"ICD10:",2),":",1)),DETAIL=$P($P(ICDLINE,"ICD10:",2),":",2)
  . SET ICD=$P(LINE,",",6)
  . SET DATEADDED=$$TODAY^TMGDATE,DISPOSITION="Not Accessed"
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
DAILYREPORT(SDT,DISPLAY) 
   ;"This routine will take today's schedule and determine if any of the patients on it
   ;"   have open suspect medical conditions and then print the report
   SET DISPLAY=+$G(DISPLAY)
   IF DISPLAY=1 GOTO DR1
   NEW %ZIS
   SET %ZIS("A")="Enter Output Device: "
   SET IOP="S121-LAUGHLIN-LASER"
   DO ^%ZIS  ;"standard device call
   IF POP DO  GOTO DRDN
   . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output. Aborting.")
   use IO
   ;"
DR1   
   NEW APPTARRAY,HEADER,LINES
   SET HEADER=0
   NEW EDT
   SET SDT=$G(SDT)
   NEW PASTONES SET PASTONES=1
   IF SDT="" DO
   . SET SDT=$$TODAY^TMGDATE+0.00001
   . SET PASTONES=0
   SET EDT=$$TODAY^TMGDATE+0.999999
   ;"SET SDT="3250422.00001"
   ;"SET EDT="3250426.99999"
   NEW THISYEAR SET THISYEAR=$E(SDT,1,3)+1700
   DO APPT4DT^TMGSMS05(SDT,EDT,.APPTARRAY,1)
   NEW DATE,TMGDFN SET DATE=0
   NEW OTHERARR
   FOR  SET DATE=$O(APPTARRAY("DT",DATE)) QUIT:DATE'>0  DO
   . IF PASTONES=1 WRITE " ===== PATIENTS FOR ",$P($$EXTDATE^TMGDATE(DATE),"@",1)," =====",!
   . SET TMGDFN=0
   . FOR  SET TMGDFN=$O(APPTARRAY("DT",DATE,TMGDFN)) QUIT:TMGDFN'>0  DO
   . . NEW REASON SET REASON=$G(APPTARRAY(DATE,TMGDFN,"REASON"))
   . . NEW ARRAY DO GET1PAT(.ARRAY,TMGDFN,THISYEAR)
   . . NEW EXARRAY DO GET1PAT(.EXARRAY,TMGDFN,THISYEAR,1)
   . . IF ($D(ARRAY))!($D(EXARRAY)) DO
   . . . IF HEADER=0 DO
   . . . . WRITE !
   . . . . WRITE "****************************************************************",!
   . . . . WRITE "          PATIENTS SCHEDULED TODAY WITH OPEN SUSPECT MEDICAL CONDITIONS",!
   . . . . IF PASTONES=0 DO
   . . . . . WRITE "                            " WRITE $$TODAY^TMGDATE(1),!
   . . . . ELSE  DO
   . . . . . WRITE "                  " WRITE $$EXTDATE^TMGDATE(SDT)," TO ",$$EXTDATE^TMGDATE(EDT),!
   . . . . WRITE "               Please deliver this report to Eddie",!
   . . . . WRITE "****************************************************************",!
   . . . . WRITE "                                            (From: TMGSUSMC.m)",!,!
   . . . . SET HEADER=1
   . . . IF '$D(ARRAY) DO
   . . . . NEW LINE SET LINE=$P($G(^DPT(TMGDFN,0)),"^",1)_" (APPT TIME: "_$P($$EXTDATE^TMGDATE(DATE),"@",2)_" FOR: "_REASON_")"
   . . . . NEW IDX SET IDX=0
   . . . . NEW COUNT SET COUNT=0
   . . . . NEW CONDS SET CONDS=""
   . . . . FOR  SET IDX=$O(EXARRAY(IDX)) QUIT:IDX'>0  DO
   . . . . . SET COUNT=COUNT+1
   . . . . . IF CONDS'="" SET CONDS=CONDS_","
   . . . . . SET CONDS=CONDS_$P($G(EXARRAY(IDX)),"^",2)
   . . . . SET OTHERARR(LINE)=CONDS
   . . . ELSE  DO
   . . . . IF PASTONES=0 DO
   . . . . . WRITE $P($G(^DPT(TMGDFN,0)),"^",1)," (APPT TIME: ",$P($$EXTDATE^TMGDATE(DATE),"@",2)," FOR: "_REASON_")",!
   . . . . ELSE  DO
   . . . . . WRITE $P($G(^DPT(TMGDFN,0)),"^",1)," (APPT DATETIME: ",$$EXTDATE^TMGDATE(DATE),"@",$P($$EXTDATE^TMGDATE(DATE),"@",2)," FOR: "_REASON_")",!
   . . . . NEW IDX SET IDX=0
   . . . . NEW COUNT SET COUNT=0
   . . . . NEW CONDS SET CONDS=""
   . . . . FOR  SET IDX=$O(ARRAY(IDX)) QUIT:IDX'>0  DO
   . . . . . SET COUNT=COUNT+1
   . . . . . IF CONDS'="" SET CONDS=CONDS_","
   . . . . . SET CONDS=CONDS_$P($G(ARRAY(IDX)),"^",2)
   . . . . IF COUNT=1 DO
   . . . . . WRITE "     -> HAS ",COUNT," CONDITION: ",CONDS,!,!
   . . . . ELSE  DO
   . . . . . WRITE "     -> HAS ",COUNT," CONDITIONS INCLUDING: ",CONDS,!,!
   IF $D(OTHERARR) DO
   . WRITE !,"================",!,"CHECK THE PATIENTS BELOW FOR NEWLY ADDED CONDITIONS",!,"================",!
   . NEW LINE SET LINE=""
   . FOR  SET LINE=$O(OTHERARR(LINE)) QUIT:LINE=""  DO
   . . WRITE LINE,!
   . . WRITE "       SATISFIED CODES: ",$G(OTHERARR(LINE)),!
DRDN 
   IF DISPLAY'=1 DO
   . DO ^%ZISC  ;" Close the output device
   QUIT
   ;"
VSTMSGS(TMGDFN)
   ;"This routine is designed to display a single patient's messages today, including
   ;"   open suspect medical conditions as well as missing vitals for today's visit
   ;" TIU OBJECT ->  "TMG TODAYS PATIENT MSGS"
   NEW TMGRESULT SET TMGRESULT=""
   NEW SUSMC SET SUSMC=$$PATSUSMC(TMGDFN)
   NEW MISSINGVITALS SET MISSINGVITALS=$$MISSVITS(TMGDFN)
   IF SUSMC'="" SET TMGRESULT=TMGRESULT_SUSMC
   IF MISSINGVITALS'="" DO
   . IF TMGRESULT'="" SET TMGRESULT=TMGRESULT_"{HTML:<BR>}"
   . SET TMGRESULT=TMGRESULT_MISSINGVITALS
   QUIT TMGRESULT
   ;"  
MISSVITS(TMGDFN)
   NEW TMGRESULT SET TMGRESULT=""
   NEW DATE SET DATE=$$TODAY^TMGDATE
   NEW RSDT SET RSDT=9999999-DATE
   NEW REDT SET REDT=9999999-DATE
   NEW VITALS DO VITARR^TMGTIUO3(.VITALS,TMGDFN,DATE\1,DATE\1_".9999")
   IF '$D(VITALS(TMGDFN,"V")) DO
   . SET TMGRESULT="NO VITALS ENTERED TODAY!"
   . SET TMGRESULT="{HTML:<FONT style=""BACKGROUND-COLOR:#ff0000"">}"_TMGRESULT_"{HTML:</FONT>}"
   QUIT TMGRESULT
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
   . . SET TMGRESULT="<table class=""inline-style""><caption><b>OPEN SUSPECT MEDICAL CONDITIONS</b></caption>"
   . . SET HEADING=1
   . NEW ICD,DESC,ZN,CONDITION
   . SET ZN=$G(ARRAY(IDX))
   . SET ICD=$P(ZN,"^",2),DESC=$P(ZN,"^",4)
   . SET CONDITION=$P(ZN,"^",3)
   . IF ICD="" SET ICD="*NOT PROVIDED*"
   . IF DESC="" SET DESC="*NOT PROVIDED*"
   . IF CONDITION SET CONDITION="*NOT PROVIDED*"
   . ;"SET TMGRESULT=TMGRESULT_"<b>ICD10:</b> "_ICD_" <b>DESC:</b> "_DESC_" <b>CONDITION:</b> "_CONDITION_"<br>"
   . SET TMGRESULT=TMGRESULT_"<tr><td><b>ICD10:</b>"_ICD_"</td><td> <b>DESC:</b> "_DESC_"</td><td> <b>CONDITION:</b> "_CONDITION_"</td></tr>"
   NEW FOOTMSG
   IF $$UP^XLFSTR(TMGRESULT)["DR. DEE" DO
   . SET FOOTMSG="No paper form. Dr. Dee has reviewed these."
   ELSE  DO  
   . SET FOOTMSG="Resolve this via paper form."
   NEW FOOTER SET FOOTER="<tfoot align=""center""><tr><th colspan=""3""><b>"_FOOTMSG_"</b></th></tr></tfoot>"
   ;"IF TMGRESULT'="" SET TMGRESULT="{HTML:<FONT style=""BACKGROUND-COLOR:#ff0000"">}"_TMGRESULT_FOOTER_"</table>{HTML:</FONT>}"
   IF TMGRESULT'="" SET TMGRESULT=$$TBLHEAD()_TMGRESULT_FOOTER_"</table>{HTML:</div>}" 
PSMCDN
   QUIT TMGRESULT
   ;"
TBLHEAD()
   NEW HEAD
   SET HEAD="<div><style>table.inline-style {"
   SET HEAD=HEAD_"width: 700px;"
   SET HEAD=HEAD_"border-collapse: collapse; }"
   SET HEAD=HEAD_" table.inline-style th,"
   SET HEAD=HEAD_" table.inline-style td {"
   SET HEAD=HEAD_"   border: 0.5px solid gray;"
   SET HEAD=HEAD_"   background-color:#ff0000; "
   SET HEAD=HEAD_"   padding: 6px;"
   SET HEAD=HEAD_" }"
   SET HEAD=HEAD_"  </style>"
   QUIT HEAD
   ;"
GET1PAT(RESULTARR,TMGDFN,YEAR,RETURNALL)  ;"  RETURN ARRAY WITH OPEN SUSPECT MEDICAL CONDITIONS (OR ALL IF RETURNALL=1)
   SET RETURNALL=+$G(RETURNALL)
   NEW PATIDX SET PATIDX=+$O(^TMG(22749,"B",TMGDFN,0))
   NEW IDX SET IDX=0
   NEW YEARARR
   SET YEARARR(YEAR)=""
   SET YEARARR("9999")=""
   IF RETURNALL=1 DO
   . SET YEAR=0
   ELSE  DO
   . SET YEAR=YEAR-1
   FOR  SET YEAR=$O(^TMG(22749,PATIDX,1,"B",YEAR)) QUIT:YEAR'>0  DO
   . FOR  SET IDX=$O(^TMG(22749,PATIDX,1,"B",YEAR,IDX)) QUIT:IDX'>0  DO
   . . NEW ZN,ONEN
   . . SET ZN=$G(^TMG(22749,PATIDX,1,IDX,0))
   . . SET ONEN=$G(^TMG(22749,PATIDX,1,IDX,1))
   . . ;"NEW STATUS SET STATUS=$$UP^XLFSTR($P(ONEN,"_",1))
   . . ;"IF STATUS'["NOT ASSESSED" QUIT
   . . NEW STATUS SET STATUS=$P(ONEN,"^",2)
   . . IF (RETURNALL=0)&(STATUS'="") QUIT  ;"IF THERE IS A STATUS, DO NOT INCLUDE
   . . IF (YEAR="9999")&($$HASICD^TMGRPU1(TMGDFN,$P(ZN,"^",2),$$FIRSTYR^TMGDATE)=1) QUIT 
   . . IF YEAR="9999" DO  
   . . . SET $P(ZN,"^",4)=$P(ZN,"^",4)_" (Yearly)"
   . . SET RESULTARR(IDX)=ZN
   . . SET RESULTARR(IDX,"DATA")="^TMG(22749,"_PATIDX_",1,"_IDX_",1)"
   QUIT
   ;"
UPDATEMC    ;"ENTRY POINT TO UPDATE MEDICAL CONDITIONS FOR A GIVEN DATE
   WRITE "WELCOME TO THE SUSPECT MEDICAL CONDITION EDITOR",!
   NEW MENU,MENUCT,USRPICK,ADT,FLD
M1   
   KILL MENU SET MENUCT=0
   SET MENU(0)="What would you like to do?"
   SET MENU(1)="Import new Suspect Medication Condition CSV"_$CHAR(9)_"Upload"
   SET MENU(2)="View only open Suspect Medication Conditions for a given date"_$CHAR(9)_"ViewOpen"
   SET MENU(3)="View all patients with Suspect Medication Condition for a given date"_$CHAR(9)_"ViewAll"
   SET MENU(4)="View one patient"_$CHAR(9)_"ViewOne"
   SET MENU(5)="View available reports"_$CHAR(9)_"Reports"
   SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
   IF USRPICK="Upload" DO PICKIMPT
   IF USRPICK="ViewOpen" DO DISPLAYMC(0)
   IF USRPICK="ViewAll" DO DISPLAYMC(1)
   IF USRPICK="ViewOne" DO DISPLAY1
   IF USRPICK="Reports" DO MCREPORTS
   IF USRPICK="^" GOTO UMCDN
   IF USRPICK=0 SET USRPICK=""
   GOTO M1
UMCDN   
   QUIT
   ;"
DISPLAYMC(RETURNALL)   
   NEW %DT,X,Y SET %DT="AEP"
   SET %DT("A")="Enter appointment date: "
   DO ^%DT WRITE !
   IF Y'>0 DO  GOTO DMCDN
   . WRITE "No date selected.  Aborting.",!
   NEW SDT,EDT,APPTARRAY,MCARRAY
   SET SDT=Y+0.00001
   SET EDT=Y+0.999999
   NEW THISYEAR SET THISYEAR=$E(SDT,1,3)+1700
   DO APPT4DT^TMGSMS05(SDT,EDT,.APPTARRAY,1)
   NEW DATE,TMGDFN SET DATE=0
   FOR  SET DATE=$O(APPTARRAY("DT",DATE)) QUIT:DATE'>0  DO
   . SET TMGDFN=0
   . FOR  SET TMGDFN=$O(APPTARRAY("DT",DATE,TMGDFN)) QUIT:TMGDFN'>0  DO
   . . NEW REASON SET REASON=$G(APPTARRAY(DATE,TMGDFN,"REASON"))
   . . NEW ARRAY DO GET1PAT(.ARRAY,TMGDFN,THISYEAR,+$G(RETURNALL))
   . . IF $D(ARRAY) MERGE MCARRAY(TMGDFN)=ARRAY
   IF '$D(MCARRAY) WRITE "NO PATIENTS FOUND ON THIS DATE OF SERVICE" GOTO DMCDN
   NEW MENU,MENUCT,USRPICK
M2   
   KILL MENU SET MENUCT=0,TMGDFN=0
   SET MENU(0)="Who would you like to edit?"
   FOR  SET TMGDFN=$O(MCARRAY(TMGDFN)) QUIT:TMGDFN'>0  DO
   . SET MENUCT=MENUCT+1,MENU(MENUCT)=$P($G(^DPT(TMGDFN,0)),"^",1)_$CHAR(9)_TMGDFN
   SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
   IF USRPICK="^" GOTO DMCDN
   IF $D(MCARRAY(USRPICK)) DO DISPLAYONE(.MCARRAY,USRPICK,RETURNALL) 
   GOTO M2
DMCDN   
   QUIT
   ;"
DISPLAY1
   NEW RETURNALL SET RETURNALL=0
   NEW X,Y,DIC,ANSWER
   SET DIC=2,DIC(0)="MAEQ"
   D ^DIC
   SET Y=+$G(Y)
   IF Y<1 QUIT
   NEW ARRAY DO GET1PAT(.ARRAY,+Y,2023,+$G(RETURNALL))
   NEW SMCARRAY MERGE SMCARRAY(Y)=ARRAY
   DO DISPLAYONE(.SMCARRAY,+Y,RETURNALL)
   QUIT
   ;"
DISPLAYONE(MCARRAY,TMGDFN,RETURNALL)  ;"
   SET RETURNALL=+$G(RETURNALL)
   WRITE "DISPLAYING DATA FOR ",$P($G(^DPT(TMGDFN,0)),"^",1),!
   NEW MENU,MENUCT,USRPICK,MCIDX
M3   
   KILL MENU SET MENUCT=0,MCIDX=0
   IF RETURNALL=0 DO
   . SET MENU(0)="Select a suspect medical condition to edit (Only displaying open conditions)"
   ELSE  DO
   . SET MENU(0)="Select a suspect medical condition to edit"
   FOR  SET MCIDX=$O(MCARRAY(TMGDFN,MCIDX)) QUIT:MCIDX'>0  DO
   . NEW CODE SET CODE=$P($G(MCARRAY(TMGDFN,MCIDX)),"^",2)
   . IF CODE="" SET CODE="<NOT PROVIDED>"
   . NEW TEXT SET TEXT=$P($G(MCARRAY(TMGDFN,MCIDX)),"^",3)
   . NEW ONEN SET ONEN=$G(MCARRAY(TMGDFN,MCIDX,"DATA"))
   . IF $D(@ONEN) DO
   . . SET ONEN=@ONEN
   . ELSE  DO
   . . SET ONEN="^^^^"
   . NEW STATUS SET STATUS=$P(ONEN,"^",2)
   . IF (RETURNALL=0)&(STATUS'="") QUIT  ;"DON'T SHOW COMPLETED ONES
   . IF STATUS="D" SET STATUS=" (SET TO: DIAGNOSED)"
   . IF STATUS="U" SET STATUS=" (SET TO: UNABLE TO DIAGNOSE)"
   . SET TEXT=TEXT_STATUS
   . SET MENUCT=MENUCT+1,MENU(MENUCT)=CODE_"-"_TEXT_$CHAR(9)_MCIDX
   SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
   IF USRPICK="^" GOTO DMCDN
   IF '$D(MCARRAY(TMGDFN,USRPICK)) GOTO M3 
   NEW SUBMENU,SUBUSRPICK 
   NEW CODE SET CODE=$P($G(MCARRAY(TMGDFN,USRPICK)),"^",3)
   SET SUBMENU(0)="What would you like to do to the "_CODE_"?"
   SET SUBMENU(1)="Set this condition to Accessed and Diagnosed"_$CHAR(9)_"D"
   SET SUBMENU(2)="Set this condition to Unable to Diagnose"_$CHAR(9)_"U"
   SET SUBMENU(3)="Set this to unaccessed"_$CHAR(9)_"@"
   SET SUBUSRPICK=$$MENU^TMGUSRI2(.SUBMENU,"^")
   IF SUBUSRPICK="^" GOTO M3
   IF (SUBUSRPICK="D")!(SUBUSRPICK="U")!(SUBUSRPICK="@") DO
   . NEW TMGFDA,TMGMSG
   . NEW PATIDX SET PATIDX=$O(^TMG(22749,"B",TMGDFN,0))
   . SET TMGFDA(22749.01,USRPICK_","_PATIDX_",",1.6)=SUBUSRPICK
   . SET TMGFDA(22749.01,USRPICK_","_PATIDX_",",1.7)="SET ON "_$$TODAY^TMGDATE(1)_" BY "_$P($G(^VA(200,DUZ,0)),"^",1)
   . DO FILE^DIE("K","TMGFDA","TMGMSG")
   . IF $D(TMGMSG) DO
   . . ZWR TMGMSG
   . ELSE  DO
   . . WRITE "Entry successfully set",!
   GOTO M3
   QUIT
   ;"
MCREPORTS  
   NEW MENU,MENUCT,USRPICK
M4
   KILL MENU SET MENUCT=0,MCIDX=0
   SET MENU(0)="Select a report"
   SET MENU(1)="Print today's open Suspect Medical Conditions"_$CHAR(9)_"1"
   SET MENU(2)="Display today's open Suspect Medical Conditions"_$CHAR(9)_"2"
   SET MENU(3)="Print this year's open Suspect Medical Conditions"_$CHAR(9)_"3"
   SET MENU(4)="Display this year's open Suspect Medical Conditions"_$CHAR(9)_"4"
   SET MENU(5)="MORE REPORTS COMING SOON"_$CHAR(9)_"5"
   SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
   IF USRPICK="^" QUIT
   IF USRPICK=1 DO DAILYREPORT("",0)
   IF USRPICK=2 DO DAILYREPORT("",1)
   IF USRPICK=3 DO DAILYREPORT($$FIRSTYR^TMGDATE(),0)
   IF USRPICK=4 DO DAILYREPORT($$FIRSTYR^TMGDATE(),1)
   IF USRPICK=5 WRITE "WHY DID YOU PICK ME?? I CLEARLY SAID MORE ARE COMING SOON MEANING THEY AREN'T READY YET!!",!,!
   GOTO M4
   QUIT
   ;"
DRAUTO  ;"CALLED FROM OPTION
   DO DAILYREPORT("",0)
   QUIT
   ;"
CLEANUP
   NEW PATIDX SET PATIDX=0
   FOR  SET PATIDX=$O(^TMG(22749,PATIDX)) QUIT:PATIDX'>0  DO
   . NEW DATAIDX SET DATAIDX=0
   . FOR  SET DATAIDX=$O(^TMG(22749,PATIDX,1,DATAIDX)) QUIT:DATAIDX'>0  DO
   . . NEW THISYEAR SET THISYEAR=$P($G(^TMG(22749,PATIDX,1,DATAIDX,0)),"^",1)
   . . IF THISYEAR'=2023 QUIT
   . . NEW THISICD SET THISICD=$P($G(^TMG(22749,PATIDX,1,DATAIDX,0)),"^",2)
   . . IF THISICD="" WRITE "FOUND ONE FOR ",PATIDX,!
   . . SET TMGFDA(22749.01,DATAIDX_","_PATIDX_",",.01)="@"
   . . DO FILE^DIE("K","TMGFDA","TMGMSG")
   . . IF $D(TMGMSG) DO
   . . . ZWR TMGMSG
   . . ELSE  DO
   . . . WRITE "Entry successfully deleted",!
   QUIT
   ;"
SUSMCENC(OUT,IDX,TMGDFN)  ;
   ;"  ADD SUSPECT MEDICAL CONDITIONS TO ENCOUNTER ARRAY  (FROM TMGTIUT3)
   ;"//kt moved to tmgtiut3 --> NEW SDT
   ;"//kt moved to tmgtiut3 --> SET SDT=$$TODAY^TMGDATE
   ;"//kt moved to tmgtiut3 --> NEW THISYEAR SET THISYEAR=$E(SDT,1,3)+1700
   ;"//kt moved to tmgtiut3 --> NEW ARRAY DO GET1PAT(.ARRAY,TMGDFN,THISYEAR)
   ;"//kt moved to tmgtiut3 --> IF $D(ARRAY) DO
   ;"//kt moved to tmgtiut3 --> . SET OUT(IDX)="5^HEADER^Suspect Medical Conditions",IDX=IDX+1
   ;"//kt moved to tmgtiut3 --> . NEW MCIDX SET MCIDX=9999
   ;"//kt moved to tmgtiut3 --> . FOR  SET MCIDX=$O(ARRAY(MCIDX),-1) QUIT:MCIDX'>0  DO
   ;"//kt moved to tmgtiut3 --> . . NEW ICD,DESC,ZN,CONDITION
   ;"//kt moved to tmgtiut3 --> . . SET ZN=$G(ARRAY(MCIDX))
   ;"//kt moved to tmgtiut3 --> . . SET ICD=$P(ZN,"^",2),DESC=$P(ZN,"^",4)
   ;"//kt moved to tmgtiut3 --> . . SET ICD=$$FIXICD(ICD)
   ;"//kt moved to tmgtiut3 --> . . SET CONDITION=$P(ZN,"^",3)
   ;"//kt moved to tmgtiut3 --> . . SET OUT(IDX)="5^ENTRY^"_ICD_"^"_DESC_"^"_CONDITION_"^10D",IDX=IDX+1
   QUIT
   ;"
FIXICD(CODE)  ;" Test given code against file 80 (which has the dots
   ;"NOTE: ^ICD9("ABA",30,<ICD 10 CODE> is the index to use, I think 
   IF CODE="" QUIT "NO CODE SAVED"
   NEW OUTCODE,I,FOUNDCODE
   IF $D(^ICD9("AB",CODE)) QUIT CODE
   IF $D(^ICD9("AB",CODE_" ")) QUIT CODE_" "
   SET FOUNDCODE=""
   FOR I=1:1:$L(CODE) D
   . S OUTCODE=$E(CODE,1,I)_"."_$E(CODE,I+1,$L(CODE))_" "
   . ;"W OUTCODE,!
   . IF $D(^ICD9("AB",OUTCODE)) SET FOUNDCODE=OUTCODE
   IF FOUNDCODE="" SET FOUNDCODE="CODE "_CODE_" NOT FOUND IN ICD FILE"
   QUIT FOUNDCODE
   ;"
FIXICD10(CODE)  ;" Test given code against file 80 -- which has the dots
   ;"NOTE: //kt copied from FIXICD above, and changed index to ABA
   ;"INPUT: CODE - ICD10 code.  
   ;"RESULT: Returns code as found in database (likely with trailing space)
   ;"        or -1^message
   IF CODE="" QUIT "NO CODE SAVED"
   NEW OUTCODE,I,FOUNDCODE
   IF $D(^ICD9("ABA",30,CODE)) QUIT CODE
   IF $D(^ICD9("ABA",30,CODE_" ")) QUIT CODE_" "
   SET FOUNDCODE=""
   FOR I=1:1:$LENGTH(CODE) DO
   . SET OUTCODE=$EXTRACT(CODE,1,I)_"."_$EXTRACT(CODE,I+1,$LENGTH(CODE))_" "
   . IF $D(^ICD9("ABA",30,OUTCODE)) SET FOUNDCODE=OUTCODE
   IF FOUNDCODE="" SET FOUNDCODE="-1^CODE "_CODE_" NOT FOUND IN ICD FILE"
   QUIT FOUNDCODE
   ;"
ADD1COND(TMGRESULT,TMGDFN,ICD,YEAR,REVIEWED)  ;"RPC: TMG CPRS ADD SUSPECT CONDITION
  SET TMGRESULT=""
  SET REVIEWED=+$G(REVIEWED)
  NEW REVIEWEDMSG SET REVIEWEDMSG=""
  IF REVIEWED=1 SET REVIEWEDMSG=" (PHYSICIAN REVIEWED AND READY TO CODE)"
  ;"Does patient already have entry in 22749?
  NEW PATIDX,TMGFDA,TMGIENS,TMGMSG
  IF $D(^TMG(22749,"B",TMGDFN)) DO
  . SET PATIDX=+$O(^TMG(22749,"B",TMGDFN,0))
  . IF PATIDX'>0  SET TMGRESULT="-1^ERROR PULLING PATIENT FROM B INDEX" QUIT
  ELSE  DO
  . SET TMGFDA(22749,"+1,",.01)=TMGDFN
  . DO UPDATE^DIE("","TMGFDA","TMGIENS","TMGMSG")
  . IF $D(TMGMSG("DIERR")) SET TMGRESULT="-1^ERROR ADDING PATIENT TO 22749" QUIT
  . SET PATIDX=+$G(TMGIENS(1))
  . KILL TMGFDA,TMGIENS,TMGMSG
  IF TMGRESULT["-1" QUIT  ;"ERROR WITH PATIENT 22749 
  ;"Does the data already exist for this year?
  NEW ADD SET ADD=1
  NEW DATAIDX SET DATAIDX=0
  SET ICD=$$FIXICD(ICD)
  IF ICD["NOT FOUND IN ICD FILE" SET TMGRESULT="-1^"_ICD QUIT
  FOR  SET DATAIDX=$O(^TMG(22749,PATIDX,1,DATAIDX)) QUIT:DATAIDX'>0  DO
  . NEW THISYEAR SET THISYEAR=$P($G(^TMG(22749,PATIDX,1,DATAIDX,0)),"^",1)
  . IF THISYEAR'=YEAR QUIT
  . NEW THISICD SET THISICD=$P($G(^TMG(22749,PATIDX,1,DATAIDX,0)),"^",2)
  . IF THISICD=ICD SET ADD=0
  IF ADD=0 SET TMGRESULT="-1^ICD "_ICD_" ALREADY EXISTS FOR YEAR "_YEAR QUIT
  NEW CONDITION,DETAIL,DISPOSITION SET (CONDITION,DETAIL,DISPOSITION)=""
  NEW ICDIEN SET ICDIEN=+$O(^ICD9("AB",ICD,0))
  IF ICDIEN>0 DO
  . SET DETAIL=$G(^ICD9(ICDIEN,68,1,1))
  . NEW WORDS,WRDIDX SET WORDS="",WRDIDX=0
  . FOR  SET WRDIDX=$O(^ICD9(ICDIEN,68,1,2,WRDIDX)) QUIT:WRDIDX'>0  DO
  . . NEW WORD SET WORD=$G(^ICD9(ICDIEN,68,1,2,WRDIDX,0))
  . . IF WORDS'="" SET WORDS=WORDS_","
  . . SET WORDS=WORDS_WORD
  . SET CONDITION=WORDS
  ELSE  DO
  . SET (CONDITION,DETAIL)="UNKNOWN"
  SET TMGIENS="+1,"_PATIDX_","
  SET TMGFDA(22749.01,TMGIENS,.01)=YEAR
  SET TMGFDA(22749.01,TMGIENS,1)=ICD
  SET TMGFDA(22749.01,TMGIENS,1.1)=CONDITION_REVIEWEDMSG
  SET TMGFDA(22749.01,TMGIENS,1.2)=DETAIL_REVIEWEDMSG
  SET TMGFDA(22749.01,TMGIENS,1.3)=$$TODAY^TMGDATE()
  SET TMGFDA(22749.01,TMGIENS,1.4)=DISPOSITION
  DO UPDATE^DIE("","TMGFDA","TMGIENS","TMGMSG")
  IF $D(TMGMSG("DIERR")) DO
  . SET TMGRESULT="-1^ERROR ADDING "_ICD_"("_$$GETERRST^TMGDEBU2(.TMGMSG)_")"
  ELSE  DO
  . SET TMGRESULT="1^ADDED "_ICD_": "_DETAIL_REVIEWEDMSG
  QUIT
  ;"