TMGSMS05 ;TMG/kst/OS SMS Message -- Reporting ;1/28/15
         ;;1.0;TMG-LIB;**1**;1/28/15
 ;
 ;"TMG FUNCTIONS
 ;"I.e. functions that related to reporting on  SMS messages
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
 ;" API -- Private Functions.
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"Dependencies:
 ;"   TMGSMS*, TMGUSRI2, XLFDT
 ;"=======================================================================
 ;
MENU  ;
  NEW MENU,IDX SET IDX=1
  SET MENU(0)="Pick SMS Report / Utility Option"
  SET MENU(IDX)="Patient report"_$C(9)_"PATIENT",IDX=IDX+1
  SET MENU(IDX)="Phone number report"_$C(9)_"PHONE",IDX=IDX+1
  SET MENU(IDX)="Dates SMS sent report"_$C(9)_"DATES",IDX=IDX+1
  SET MENU(IDX)="Show LOG file for SMS MESSAGES SENT"_$C(9)_"LOG",IDX=IDX+1
  SET MENU(IDX)="Appt view"_$C(9)_"APPT",IDX=IDX+1
  SET MENU(IDX)="Opt-out patient phone number"_$C(9)_"OPT_OUT",IDX=IDX+1
  SET MENU(IDX)="Opt-out patient by date seen"_$C(9)_"OPT_OUT2",IDX=IDX+1
  SET MENU(IDX)="Send Test SMS message now"_$C(9)_"TEST SMS",IDX=IDX+1
  NEW USRSLCT
LOOP ;  
  WRITE !
  SET USRSLCT=$$MENU^TMGUSRI2(.MENU)
  IF USRSLCT="PATIENT" DO ASKPTRPT GOTO LOOP   
  IF USRSLCT="PHONE" DO ASKPNRPT GOTO LOOP
  IF USRSLCT="DATES" DO ASKDTRPT GOTO LOOP
  IF USRSLCT="LOG" DO SHOWLOG GOTO LOOP
  IF USRSLCT="APPT" DO ASKAPPT GOTO LOOP
  IF USRSLCT="OPT_OUT2" DO OPTOUT^TMGSMS02(1) GOTO LOOP
  IF USRSLCT="OPT_OUT" DO ASKOPTOT^TMGSMS02 GOTO LOOP
  IF USRSLCT="TEST SMS" DO TESTSMS GOTO LOOP  
  QUIT
  ;  
ASKPTRPT ;"ASK PATIENT REPORT 
  WRITE !,"----------------------------------",!
  WRITE "SMS Report for one patient.",!
  WRITE "----------------------------------",!
  NEW ERR,X,Y,DIC 
  SET DIC=2,DIC(0)="MAEQ" DO ^DIC WRITE !
  IF +Y>0 DO RPTDFN(+Y,.ERR)
  DO SHOWIFER(.ERR) ;
  QUIT
  ;
ASKPNRPT  ;"ASK PHONE REPORT
  NEW PHONE,ERR
  WRITE !,"----------------------------------",!
  WRITE "SMS Report for one phone number.",!
  WRITE "----------------------------------",!
  READ "Enter phone number: ",PHONE:$GET(DTIME,3600)
  WRITE !
  DO RPTPHONE(PHONE,.ERR)
  DO SHOWIFER(.ERR) ;
  QUIT
  ;
ASKDTRPT  ;"ASK DATES REPORT  
  NEW %DT,X,SDT,EDT SET %DT="APTE"
  WRITE !,"----------------------------------",!
  WRITE "SMS Report for date range.",!
  WRITE "----------------------------------",!
  SET %DT("A")="Enter starting date: "
  DO ^%DT 
  IF Y'>0 WRITE ! GOTO ADRDN
  SET SDT=Y
  SET %DT("A")="Enter ending date (optional): "
  DO ^%DT WRITE !
  SET EDT=Y
  NEW VERBOSELVL SET VERBOSELVL=2
  NEW % SET %=2
  WRITE "Show message sent" DO YN^DICN
  IF %=-1 GOTO ADRDN
  IF %=2 SET VERBOSELVL=1  ;"1 vs 2
  NEW INFO
  DO GET4DATE(SDT,EDT,.INFO) 
  DO SHWDTARR(.INFO,VERBOSELVL) ;"SHOW ARRAY FOR DATE RANGES
ADRDN ;
  QUIT
  ;
ASKAPPT ;"ASK APPTS DATE REPORT
  NEW %DT,X,SDT,EDT SET %DT="APTE"
  WRITE !,"----------------------------------",!
  WRITE "Appointment report for date range.",!
  WRITE "----------------------------------",!
  SET %DT("A")="Enter starting date: "
  DO ^%DT 
  IF Y'>0 WRITE ! GOTO ADRDN
  SET SDT=Y
  SET %DT("A")="Enter ending date (optional): "
  DO ^%DT WRITE !
  SET EDT=Y
  NEW INFO
  DO APPT4DT(SDT,EDT,.INFO)
AAPLP1 ;  
  NEW MENU
  SET MENU(0)="Pick Option for Showing Appts "
  SET MENU(1)="Show by Appt date/time"_$C(9)_"DT"
  SET MENU(2)="Show by Patient name"_$C(9)_"NAME"
  NEW USRSLCT,MODE SET MODE=0
  WRITE !
  SET USRSLCT=$$MENU^TMGUSRI2(.MENU)
  IF USRSLCT="DT" SET MODE=1    
  IF USRSLCT="NAME" SET MODE=2
  IF MODE=0 GOTO AAPLP2
  DO SHWAPARR(.INFO,MODE) ;"SHOW APPT ARRAY
  GOTO AAPLP1
AAPLP2 ;  
  QUIT
  ;
RPTDFN(DFN,ERR) ;"Output report for patient
  ;"Input: DFN -- The patient to query.
  ;"       ERR -- PASS BY REFERENCE.  Format: ERR(#)=Message
  NEW ERR2
  SET DFN=+$GET(DFN)
  NEW PHONEARR,ONEPHONE  
  DO GETPHONS^TMGSMS01(DFN,.PHONEARR,.ERR2)
  IF $DATA(ERR2) DO  GOTO RDFNDN
  . DO ADDMSG(.ERR,$GET(ERR2(DFN)))
  . DO SHOWIFER(.ERR)  
  NEW PTNAME SET PTNAME=$PIECE($GET(^DPT(DFN,0)),"^",1)
RPDLP1 ;  
  NEW MENU,IDX SET IDX=1
  SET MENU(0)="Pick Option for Displaying Patient Info"
  SET MENU(0,1)="Patient name: "_PTNAME
  SET MENU(IDX)="Show Phone numbers for patient"_$C(9)_"PHONE",IDX=IDX+1
  SET MENU(IDX)="Show Excluded Phone numbers for patient"_$C(9)_"EXCLUDED",IDX=IDX+1
  SET MENU(IDX)="Opt-out phone number"_$C(9)_"OPT_OUT",IDX=IDX+1
  SET MENU(IDX)="Dump patient record"_$C(9)_"PATIENT",IDX=IDX+1
  SET MENU(IDX)="Show Appts for patient"_$C(9)_"APPT",IDX=IDX+1
  SET MENU(IDX)="Show SMS messages for patient"_$C(9)_"SMS",IDX=IDX+1
  SET ONEPHONE=""
  FOR  SET ONEPHONE=$ORDER(PHONEARR(DFN,ONEPHONE)) QUIT:+ONEPHONE'>0  DO
  . SET MENU(IDX)="View thread for phone num: "_ONEPHONE_$C(9)_"NUM^"_ONEPHONE,IDX=IDX+1
  NEW USRSLCT
  WRITE !
  SET USRSLCT=$$MENU^TMGUSRI2(.MENU)
  IF USRSLCT["NUM^" DO  GOTO RPDLP1
  . NEW ONENUM SET ONENUM=$PIECE(USRSLCT,"^",2)
  . NEW ERR
  . DO RPTPHONE(ONENUM,.ERR)
  . DO SHOWIFER(.ERR)
  . DO PRESS2GO^TMGUSRI2 WRITE !  
  IF USRSLCT="PHONE" DO  GOTO RPDLP1
  . NEW FOUND SET FOUND=0
  . NEW PHONENAME SET PHONENAME=""
  . FOR  SET PHONENAME=$ORDER(PHONEARR(DFN,"NAME",PHONENAME)) QUIT:PHONENAME=""  DO
  . . NEW ONENUM SET ONENUM=$GET(PHONEARR(DFN,"NAME",PHONENAME))
  . . WRITE "  ",PHONENAME,": ",$$FMTPHONE^TMGSMS02(ONENUM),!
  . . SET FOUND=1
  . IF FOUND=0 WRITE "  (No phone numbers on file for patient.)",!
  . DO PRESS2GO^TMGUSRI2 WRITE !  
  IF USRSLCT="EXCLUDED" DO  GOTO RPDLP1
  . WRITE "Excluded phone numbers:",!
  . SET FOUND=0
  . NEW SUBIEN SET SUBIEN=0
  . FOR  SET SUBIEN=$ORDER(^DPT(DFN,"TMGSMS",SUBIEN)) QUIT:+SUBIEN'>0  DO
  . . DO DUMPREC^TMGDEBU3(2.022705,SUBIEN_","_DFN_",")
  . . SET FOUND=1
  . IF FOUND=0 WRITE "  (none)",!
  . DO PRESS2GO^TMGUSRI2 WRITE !  
  IF USRSLCT="OPT_OUT" DO  GOTO RPDLP1
  . DO OPTOTDFN^TMGSMS02(DFN)
  . DO PRESS2GO^TMGUSRI2 WRITE !  
  IF USRSLCT="PATIENT" DO  GOTO RPDLP1
  . DO DUMPREC^TMGDEBU3(2,DFN_",")
  . DO PRESS2GO^TMGUSRI2 WRITE !  
  IF USRSLCT="APPT" DO  GOTO RPDLP1
  . DO DUMPREC^TMGDEBU3(22723,DFN_",")
  . DO PRESS2GO^TMGUSRI2 WRITE !  
  IF USRSLCT="SMS" DO  GOTO RPDLP1
  . WRITE !,"Information from TMG SMS MESSAGES file:",!
  . NEW FOUND,ABORT SET (FOUND,ABORT)=0 
  . NEW IEN SET IEN=0
  . FOR  SET IEN=$ORDER(^TMG(22724,"DFN",DFN,IEN)) QUIT:(+IEN'>0)!(ABORT)  DO
  . . NEW ONENUM SET ONENUM=$PIECE($GET(^TMG(22724,IEN,0)),"^",1)
  . . SET FOUND=1
  . . WRITE "  Message sent to phone num: ",ONENUM,".  "
  . . NEW % SET %=2 WRITE "View" DO YN^DICN WRITE !
  . . IF %=-1 SET ABORT=1 QUIT
  . . IF %'=1 QUIT
  . . NEW ERR
  . . DO RPTPHONE(ONENUM,.ERR)
  . . DO SHOWIFER(.ERR)
  . IF FOUND=0 WRITE "  None found",!
  . DO PRESS2GO^TMGUSRI2 WRITE !  
RDFNDN ;  
  QUIT
  ;  
RPTPHONE(PHONE,ERR) ;"Output report for one phone number
  ;"Input: PHONE -- Phone number to query.  Should be 11 digits.
  ;"       ERR -- PASS BY REFERENCE.  Format: ERR(#)=Message
  NEW GOODPHONE SET GOODPHONE=$$MKVALPHN^TMGSMS01($GET(PHONE))
  IF GOODPHONE="" DO  GOTO RPDN
  . DO ADDMSG(.ERR,"Bad phone number.  Got '"_$GET(PHONE)_"'")
  NEW THREAD DO GETTHRED^TMGSMS03(.THREAD,GOODPHONE)
  IF $DATA(THREAD("FROM")) DO
  . DO SHWTHRED^TMGSMS03(.THREAD)
  ELSE  DO
  . WRITE "Phone number: ",GOODPHONE," --> no SMS messages",!
RPDN ;
  QUIT
  ;
GET4DATE(SDT,EDT,OUT) ;"GET MESSAGE INFORMATION FOR DATE RANGE
  ;"Purpose: Get an array with information about messages sent during date ranges. 
  ;"Input: SDT -- FM date for starting date-time
  ;"       EDT -- FM Date for ending date-time
  ;"       OUT -- PASS BY REFERENCE. Format:
  ;"          OUT("TOTAL")=total number of messages sent
  ;"          OUT("SDT")=SDT^External start date
  ;"          OUT("EDT")=EDT^External ending date
  ;"          OUT(PHONENUM,"DFN")=DFN^Patient name (or "" if unknown)
  ;"          OUT(PHONENUM,DT)=message
  ;"          OUT("B",<patient name>,PHONENUM)=""
  ;"Results: none
  SET SDT=+$GET(SDT)
  SET EDT=$GET(EDT) IF +EDT'>0 SET EDT=SDT
  SET EDT=EDT\1_".999999"
  SET OUT("SDT")=SDT_"^"_$$FMTE^XLFDT(SDT,"D")
  SET OUT("EDT")=EDT_"^"_$$FMTE^XLFDT(EDT,"D")
  NEW ADT SET ADT=SDT-0.000001
  FOR  SET ADT=$ORDER(^TMG(22724,"D",ADT)) QUIT:(ADT>EDT)!(+ADT'>0)  DO
  . NEW IEN SET IEN=0
  . FOR  SET IEN=$ORDER(^TMG(22724,"D",ADT,IEN)) QUIT:+IEN'>0  DO
  . . NEW PHONE SET PHONE=$PIECE($GET(^TMG(22724,IEN,0)),"^",1)
  . . IF PHONE="" QUIT
  . . NEW SUBIEN SET SUBIEN=0
  . . FOR  SET SUBIEN=$ORDER(^TMG(22724,"D",ADT,IEN,SUBIEN)) QUIT:+SUBIEN'>0  DO  
  . . . NEW ZN SET ZN=$GET(^TMG(22724,IEN,1,SUBIEN,0))
  . . . NEW ONEDT SET ONEDT=$PIECE(ZN,"^",1)
  . . . NEW ADFN SET ADFN=+$PIECE(ZN,"^",4)
  . . . NEW NAME SET NAME=$PIECE($GET(^DPT(ADFN,0)),"^",1)
  . . . IF NAME="" SET NAME="??"
  . . . NEW MSG SET MSG=$PIECE($GET(^TMG(22724,IEN,1,SUBIEN,1)),"^",1)
  . . . SET OUT(PHONE,"DFN")=ADFN_"^"_NAME
  . . . SET OUT(PHONE,ONEDT)=MSG
  . . . SET OUT("TOTAL")=+$GET(OUT("TOTAL"))+1
  . . . SET OUT("B",NAME,PHONE)=""
  . . NEW TEMP SET TEMP=1
  . NEW TEMP2 SET TEMP2=1
  QUIT
  ;
SHWDTARR(INFO,VERBOSELVL) ;"SHOW ARRAY FOR DATE RANGES
  ;"Input: INFO
  ;"       INFO -- PASS BY REFERENCE. Format:
  ;"          INFO("TOTAL")=total number of messages sent
  ;"          INFO(PHONENUM,"DFN")=DFN^Patient name (or "" if unknown)
  ;"          INFO(PHONENUM,DT)=message
  ;"          INFO("B",<patient name>,PHONENUM)=""
  ;"       VERBOSELVL.  Default=2.  Level of verbosity.  1=TERSE
  SET VERBOSELVL=+$GET(VERBOSELVL) IF VERBOSELVL'>0 SET VERBOSELVL=2
  IF VERBOSELVL'=2 GOTO SHWA2 
  NEW %ZIS,POP
  SET %ZIS("A")="Enter Output Device: "
  SET %ZIS("B")="HOME"
  DO ^%ZIS  ;"standard device call
  IF POP QUIT
  USE IO
  ;"Do the output
SHWA2 ;  
  WRITE !,"-----------------------------------------------",!
  WRITE "Start date: ",$PIECE($GET(INFO("SDT")),"^",2),!
  WRITE "End date: ",$PIECE($GET(INFO("EDT")),"^",2),!
  WRITE +$GET(INFO("TOTAL"))," SMS messages sent during date range",!
  WRITE "-----------------------------------------------",!
  NEW NAME SET NAME=""
  FOR  SET NAME=$ORDER(INFO("B",NAME)) QUIT:NAME=""  DO
  . WRITE "SMS To: ",NAME,!
  . NEW PHONE SET PHONE=""
  . FOR  SET PHONE=$ORDER(INFO("B",NAME,PHONE)) QUIT:PHONE=""  DO
  . . NEW DT SET DT=0
  . . FOR  SET DT=$ORDER(INFO(PHONE,DT)) QUIT:+DT'>0  DO
  . . . NEW MSG SET MSG=$GET(INFO(PHONE,DT))
  . . . IF VERBOSELVL=1 DO
  . . . . WRITE "  Phone#: ",PHONE," on ",$$FMTE^XLFDT(DT),!
  . . . ELSE  DO
  . . . . WRITE "  Sent: ",$$FMTE^XLFDT(DT),!
  . . . . WRITE "  Phone#: ",PHONE,!
  . . . IF VERBOSELVL=1 QUIT
  . . . WRITE "  Msg: "
  . . . NEW DISPW SET DISPW=70
  . . . FOR  QUIT:$LENGTH(MSG)'>0  DO
  . . . . WRITE $EXTRACT(MSG,1,DISPW),!
  . . . . SET MSG=$EXTRACT(MSG,DISPW+1,999)
  . . . . IF MSG'="" WRITE "       "
  ;" Close the output device
  IF VERBOSELVL=2 DO ^%ZISC  
  QUIT
  ;  
APPT4DT(SDT,EDT,OUT,ACTIVE) ;"GET APPT INFORMATION FOR DATE RANGE
  ;"Purpose: Get an array with information about messages sent during date ranges. 
  ;"Input: SDT -- FM date for starting date-time (REQUIRED)
  ;"       EDT -- FM Date for ending date-time (OPTIONAL). Default is same day as SDT
  ;"       OUT -- PASS BY REFERENCE. Format:
  ;"          OUT("TOTAL")=total number of appts found
  ;"          OUT("SDT")=SDT^External start date
  ;"          OUT("EDT")=EDT^External ending date
  ;"          OUT("B",<patient name>,PHONENUM)=""
  ;"          OUT(ONEDT,ADFN,"NAME")=NAME
  ;"          OUT(ONEDT,ADFN,"MINS")=MINS
  ;"          OUT(ONEDT,ADFN,"PROV")=PROV
  ;"          OUT(ONEDT,ADFN,"REASON")=REASON
  ;"          OUT(ONEDT,ADFN,"FLAGS")=FLAGS
  ;"          OUT(ONEDT,ADFN,"COMMENT")=COMMENT
  ;"          OUT(ONEDT,ADFN,"IEN")=IEN
  ;"          OUT(ONEDT,ADFN,"SUBIEN")=SUBIEN
  ;"          OUT("TOTAL")=+$GET(OUT("TOTAL"))+1
  ;"          OUT("B",NAME,ONEDT)=ADFN
  ;"          OUT("DT",ONEDT,ADFN)=NAME
  ;"      ACTIVE -- BOOLEAN (OPTIONAL). IF 1, ONLY RETURN ACTIVE APPOINTMENTS
  ;"Results: none
  SET SDT=+$GET(SDT)
  SET EDT=$GET(EDT) IF +EDT'>0 SET EDT=SDT
  SET EDT=EDT\1_".999999"
  SET OUT("SDT")=SDT_"^"_$$FMTE^XLFDT(SDT,"D")
  SET OUT("EDT")=EDT_"^"_$$FMTE^XLFDT(EDT,"D")
  SET ACTIVE=+$G(ACTIVE)
  NEW ADT SET ADT=SDT-0.000001
  FOR  SET ADT=$ORDER(^TMG(22723,"DT",ADT)) QUIT:(ADT>EDT)!(+ADT'>0)  DO
  . NEW IEN SET IEN=0
  . FOR  SET IEN=$ORDER(^TMG(22723,"DT",ADT,IEN)) QUIT:+IEN'>0  DO
  . . NEW ADFN SET ADFN=$PIECE($GET(^TMG(22723,IEN,0)),"^",1)
  . . NEW NAME SET NAME=$PIECE($GET(^DPT(ADFN,0)),"^",1)
  . . NEW SUBIEN SET SUBIEN=0
  . . FOR  SET SUBIEN=$ORDER(^TMG(22723,"DT",ADT,IEN,SUBIEN)) QUIT:+SUBIEN'>0  DO  
  . . . NEW ZN SET ZN=$GET(^TMG(22723,IEN,1,SUBIEN,0))
  . . . NEW ONEDT SET ONEDT=$PIECE(ZN,"^",1)
  . . . NEW MINS SET MINS=$PIECE(ZN,"^",2)
  . . . NEW PROV SET PROV=$PIECE(ZN,"^",3)
  . . . NEW REASON SET REASON=$PIECE(ZN,"^",4)
  . . . NEW FLAGS SET FLAGS=$PIECE(ZN,"^",5)
  . . . NEW COMMENT SET COMMENT=$PIECE(ZN,"^",6)
  . . . NEW STATUS SET STATUS=$PIECE(ZN,"^",7)
  . . . IF (ACTIVE=1)&(STATUS'="A") QUIT
  . . . IF (ACTIVE=1)&(REASON="INJ ONLY") QUIT
  . . . IF (ACTIVE=1)&(REASON["PROTIME") QUIT
  . . . IF (ACTIVE=1)&(REASON["SUTRE") QUIT
  . . . SET OUT(ONEDT,ADFN,"NAME")=NAME
  . . . SET OUT(ONEDT,ADFN,"MINS")=MINS
  . . . SET OUT(ONEDT,ADFN,"PROV")=PROV
  . . . SET OUT(ONEDT,ADFN,"REASON")=REASON
  . . . SET OUT(ONEDT,ADFN,"FLAGS")=FLAGS
  . . . SET OUT(ONEDT,ADFN,"COMMENT")=COMMENT
  . . . SET OUT(ONEDT,ADFN,"IEN")=IEN
  . . . SET OUT(ONEDT,ADFN,"SUBIEN")=SUBIEN
  . . . SET OUT("TOTAL")=+$GET(OUT("TOTAL"))+1
  . . . SET OUT("B",NAME,ONEDT)=ADFN
  . . . SET OUT("DT",ONEDT,ADFN)=NAME
  QUIT
  ;
SHWAPARR(INFO,MODE) ;"SHOW APPT ARRAY
  ;"Input: INFO
  ;"       INFO -- PASS BY REFERENCE. Format:
  ;"          INFO("TOTAL")=total number of appts found
  ;"          INFO("SDT")=SDT^External start date
  ;"          INFO("EDT")=EDT^External ending date
  ;"          INFO("B",<patient name>,PHONENUM)=""
  ;"          INFO(ONEDT,ADFN,"NAME")=NAME
  ;"          INFO(ONEDT,ADFN,"MINS")=MINS
  ;"          INFO(ONEDT,ADFN,"PROV")=PROV
  ;"          INFO(ONEDT,ADFN,"REASON")=REASON
  ;"          INFO(ONEDT,ADFN,"FLAGS")=FLAGS
  ;"          INFO(ONEDT,ADFN,"COMMENT")=COMMENT
  ;"          INFO(ONEDT,ADFN,"IEN")=IEN
  ;"          INFO(ONEDT,ADFN,"SUBIEN")=SUBIEN
  ;"          INFO("TOTAL")=+$GET(OUT("TOTAL"))+1
  ;"          INFO("B",NAME,ONEDT)=ADFN
  ;"          INFO("DT",ONEDT,ADFN)=NAME
  ;"       MODE.  1=by time, 2= by patient name.  DEFAULT=1
  WRITE !,"-----------------------------------------------",!
  WRITE "Start date: ",$PIECE($GET(INFO("SDT")),"^",2),!
  WRITE "End date: ",$PIECE($GET(INFO("EDT")),"^",2),!
  WRITE +$GET(INFO("TOTAL"))," appts found during date range",!
  WRITE "-----------------------------------------------",!
  SET MODE=+$GET(MODE) IF MODE'>0 SET MODE=1
  NEW SELARR
  IF MODE=1 DO
  . NEW ONEDT SET ONEDT=""
  . FOR  SET ONEDT=$ORDER(INFO("DT",ONEDT)) QUIT:ONEDT=""  DO
  . . NEW ADFN SET ADFN=""
  . . FOR  SET ADFN=$ORDER(INFO("DT",ONEDT,ADFN)) QUIT:ADFN=""  DO
  . . . NEW NAME SET NAME=$GET(INFO("DT",ONEDT,ADFN))
  . . . SET SELARR($$FMTE^XLFDT(ONEDT)_" -- "_NAME,ONEDT_"^"_ADFN)=""  
  IF MODE=2 DO    
  . NEW NAME SET NAME=""
  . FOR  SET NAME=$ORDER(INFO("B",NAME)) QUIT:NAME=""  DO
  . . NEW ONEDT SET ONEDT=""
  . . FOR  SET ONEDT=$ORDER(INFO("B",NAME,ONEDT)) QUIT:ONEDT=""  DO
  . . . NEW ADFN SET ADFN=$GET(INFO("B",NAME,ONEDT))
  . . . SET SELARR(NAME_" -- "_$$FMTE^XLFDT(ONEDT),ONEDT_"^"_ADFN)=""
  NEW OUT
  DO SELECTR2^TMGUSRI3("SELARR","OUT","Select Appt to View ([ESC][ESC] when done.)")
  WRITE !,"Details of appointments selected",!
  WRITE "====================================",!
  NEW DISPW SET DISPW=""
  FOR  SET DISPW=$ORDER(OUT(DISPW)) QUIT:DISPW=""  DO
  . NEW VALUE SET VALUE=""
  . FOR  SET VALUE=$ORDER(OUT(DISPW,VALUE)) QUIT:VALUE=""  DO
  . . NEW ADT SET ADT=$PIECE(VALUE,"^",1)
  . . NEW ADFN SET ADFN=$PIECE(VALUE,"^",2)
  . . NEW REC MERGE REC=INFO(ADT,ADFN)
  . . NEW IEN SET IEN=+$GET(REC("IEN")) QUIT:IEN'>0
  . . NEW SUBIEN SET SUBIEN=+$GET(REC("SUBIEN")) QUIT:SUBIEN'>0
  . . WRITE DISPW,!
  . . DO DUMPREC^TMGDEBU3(22723.01,SUBIEN_","_IEN_",")
  . . DO PRESS2GO^TMGUSRI2 WRITE !
  . . NEW ERR
  . . DO RPTDFN(ADFN,.ERR) ;"Output report for patient
  . . DO PRESS2GO^TMGUSRI2 WRITE !
  . . DO SHOWIFER(.ERR) ;
  QUIT
  ;
SHOWLOG() ;
  NEW % SET %=2
  WRITE !,"-----------------------------------------------",!
  WRITE "  Show LOG entries in TMG LOG file.",!
  WRITE "-----------------------------------------------",!
  NEW IEN SET IEN=+$ORDER(^TMG(22725,"B","SMS MESSAGES SENT",0))
  IF IEN'>0 DO  GOTO SLL2
  . WRITE "Unable to find entry in file 22725 for 'SMS MESSAGES SENT'",!
  WRITE "Show *ALL* SMS log entries" DO YN^DICN WRITE !
  IF %=-1 QUIT
  IF %=1 DO
  . NEW %ZIS,POP
  . SET %ZIS("A")="Enter Output Device: "
  . SET %ZIS("B")="HOME"
  . DO ^%ZIS  ;"standard device call
  . IF POP QUIT
  . USE IO
  . ;"Do the output
  . DO DUMPREC^TMGDEBU3(22725,IEN,0)
  . DO ^%ZISC
  ELSE  DO
  . NEW IENS SET IENS=","_IEN_","
  . SET IENS=$$ASKIENS^TMGDBAP3(22725.01,IENS)
  . DO DUMPREC^TMGDEBU3(22725.01,IENS,0)
SLL2 ;  
  DO PRESS2GO^TMGUSRI2 WRITE !
  QUIT
  ;
TESTSMS ;
  WRITE !,"-----------------------------------------------",!
  WRITE "  Send a TEST SMS message.",!
  WRITE "-----------------------------------------------",!
  NEW PHONE
  READ "Phone number to send test SMS to: ",PHONE:$GET(DTIME,3600)
  WRITE !
  SET PHONE=$TRANSLATE(PHONE,"(- ","") 
  SET PHONE=+$GET(PHONE) 
  IF PHONE'>0 WRITE "??" GOTO TSMSDN
  IF $LENGTH(PHONE)'=11 DO
  . SET PHONE=$$MKVALPHN^TMGSMS01(PHONE)
  . WRITE " using --> ",PHONE,!
  NEW DIC,X,Y SET DIC=2,DIC(0)="MAEQ"
  SET DIC("A")="Patient of record for test SMS (optional): "
  DO ^DIC WRITE !
  NEW DFN SET DFN=+Y IF Y'>0 SET Y=0
  DO SNDTSMS^TMGSMS01(DFN,PHONE)
  WRITE "  ... SENT",! 
TSMSDN ;
  DO PRESS2GO^TMGUSRI2 WRITE !
  QUIT

SHOWIFER(ERR) ;
  IF $DATA(ERR) DO
  . WRITE "Error encountered:",!
  . DO SHOWMSG(.ERR)
  QUIT
  ;
ADDMSG(ARR,MSG) ;"Add message to array
  NEW CT SET CT=$ORDER(ARR(""),-1)+1
  SET ARR(CT)=$GET(MSG)
  QUIT
  ;
SHOWMSG(ARR) ;"SHOW MESSAGE ARRAY
  NEW IDX SET IDX=""
  FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:+IDX'>0  DO
  . WRITE $GET(ARR(IDX)),!
  WRITE !
  QUIT
  ;
