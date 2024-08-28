TMGSMSH1 ;TMG/kst/OS HOURLY SMS Message ;1/18/15, 3/24/21
         ;;1.0;TMG-LIB;**1**;12/9/14
 ;
 ;"TMG FUNCTIONS
 ;"I.e. functions that related to sending SMS messages
 ;
 ;"NOTE:  This was copied and modified from TMGSMS01 -- set up for DayLead,
 ;"       This purpose of this file is Hourslead
 ;"     Could later work to merge these two.  
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
 ;"SENDSMS(HOURLEAD) -- Send out SMS messages for upcoming appts, vit FTP method 
 ;"SENDSMS1(HOURLEAD) -- Send out SMS messages for upcoming appts, via HTTP method
 ;"SMSGTALL() -- Get all incoming SMS messages 
 ;"SMSGET(DELFILE) -- Get all incoming SMS messages, optionally deleting source file.
 ;"GETFSTAT() -- Process FINAL status report.
 ;
 ;"=======================================================================
 ;" API -- Private Functions.
 ;"=======================================================================
 ;"SETUPSMS(OUT,HOURLEAD,ERRARRAY,NONE) -- set up for sending out SMS messages
 ;"GETDTPTL(OUT,TMGDT,EXCLUDE) -- get a list patients that have upcoming appts on TMGDT
 ;"GETPTLST(OUT,HOURLEAD,EXCLUDE) -- get a list patients that have upcoming appts
 ;
 ;"COMPMSG(ARR,ERRARRAY,HOURLEAD) -- create an array with messages to be sent by SMS
 ;"GETMSG(TMGDFN,PROVIEN,TMGDT,REASON,HOURLEAD) -- Create outogoing message.
 ;"SUBSTX(MSG,ARR) -- Substitute values into text string
 ;"SCHSTCHK -- DO SCHEDULE TASK FOR STATUS CHECK
 ;
 ;"=======================================================================
 ;"Dependencies
 ;"=======================================================================
 ;"NOTES: 
 ;"  TMGKERN5 -- interfaces with actual sending and receiving of messages
 ;"  DIE
 ;"  TMGDEBU2 -- for fileman error messages.
 ;"  FM file# 22724 (TMG SMS MESSAGES) stores sent and received messages
 ;" 
 ;"=======================================================================
 ;
GETDTPTL(OUT,TMGDT,MODE,EXCLUDE) ;
  ;"Purpose: to get a list patients that have upcoming appts on TMGDT
  ;"Input:   OUT -- PASS BY REFERENCE.  Format as follows:
  ;"             OUT(TMGDFN)=SubIEN (file 22723.01)
  ;"             Prior data killed.  If no appts, then array returned empty.
  ;"         TMGDT -- The FM date to obtain for.
  ;"         MODE - "A": return only active appts, "*": return any appt
  ;"         EXCLUDE -- PASS BY REFERENCE.  Format:
  ;"            EXCLUDE(<node in subfile>,<piece in node>,<exclude value>)="" <-- do include if match found.
  ;"Results: none
  KILL OUT
  SET MODE=$GET(MODE,"*")
  NEW ENDDT SET ENDDT=$$FMADD^XLFDT(TMGDT,,1)
  SET TMGDT=ENDDT-0.01
  SET TMGDT=TMGDT-0.000001
  ;"SET ^TMG("HOURLY SMS",$$NOW^XLFDT,"TIMES")=TMGDT_" TO "_ENDDT
  FOR  SET TMGDT=$ORDER(^TMG(22723,"DT",TMGDT)) QUIT:(TMGDT'<ENDDT)  DO
  . NEW IEN SET IEN=0
  . FOR  SET IEN=$ORDER(^TMG(22723,"DT",TMGDT,IEN)) QUIT:+IEN'>0  DO
  . . NEW SUBIEN SET SUBIEN=0
  . . FOR  SET SUBIEN=$ORDER(^TMG(22723,"DT",TMGDT,IEN,SUBIEN)) QUIT:+SUBIEN'>0  DO  
  . . . IF MODE="A",$GET(^TMG(22723,"DT",TMGDT,IEN,SUBIEN))'="A" QUIT
  . . . NEW TMGDFN SET TMGDFN=$PIECE($GET(^TMG(22723,IEN,0)),"^",1)
  . . . IF TMGDFN'>0 QUIT
  . . . NEW SKIP SET SKIP=0
  . . . NEW NODE SET NODE=""
  . . . FOR  SET NODE=$ORDER(EXCLUDE(NODE)) QUIT:(NODE="")!SKIP  DO
  . . . . NEW PCE SET PCE=""
  . . . . FOR  SET PCE=$ORDER(EXCLUDE(NODE,PCE)) QUIT:(+PCE'>0)!SKIP  DO
  . . . . . NEW APPTVAL SET APPTVAL=$PIECE($GET(^TMG(22723,IEN,1,SUBIEN,NODE)),"^",PCE)
  . . . . . NEW VALUE SET VALUE=""
  . . . . . FOR  SET VALUE=$ORDER(EXCLUDE(NODE,PCE,VALUE)) QUIT:(VALUE="")!SKIP  DO
  . . . . . . IF VALUE=APPTVAL SET SKIP=1
  . . . IF SKIP QUIT
  . . . SET OUT(TMGDFN)=SUBIEN
  QUIT
  ; 
GETPTLST(OUT,AHOURLEAD,MODE,EXCLUDE,TEST) ;
  ;"Purpose: to get a list patients that have upcoming appts
  ;"Input:   OUT -- PASS BY REFERENCE.  Format as follows:
  ;"             OUT(TMGDFN)=SubIEN (file 22723.01)
  ;"             Prior data killed.  If no appts, then array returned empty.
  ;"         AHOURLEAD --  The number of hours from NOW to get list from.  E.g if
  ;"                value = 1, then should return list of patients with an 
  ;"                appt 1 hour from now.
  ;"         MODE - "A": return only active appts, "*": return any appt
  ;"         EXCLUDE -- PASS BY REFERENCE.  See GETDTPTL() for format
  ;"         TEST -- USED TO TEST, VERBOSE
  ;"Results: none
  KILL OUT
  NEW NOW SET NOW=$$NOW^XLFDT
  IF TEST=1 SET NOW=3180907.08
  NEW TARGETDT SET TARGETDT=$J($$FMADD^XLFDT(NOW,,AHOURLEAD),0,2)
  DO GETDTPTL(.OUT,TARGETDT,.MODE,.EXCLUDE) 
  QUIT
  ;
COMPMSG(ARR,ERRARRAY,HOURLEAD) ;"COMPILE MESSAGES
  ;"Purpose: create an array with messages to be sent by SMS
  ;"Input: ARR -- PASS BY REFERENCE.  An IN and OUT parameter.  Format:
  ;"          for input, same as output of GETPTLIST: 
  ;"             ARR(TMGDFN)=SubIEN (file 22723.01) (this is also a FM date/time)
  ;"          for output, data added to ARR.  Format:
  ;"              ARR("MSG",DFN,PhoneNumber)=ApptDT^<message>
  ;"              ARR("NONE",DFN)=<message>  <-- Message that could not be sent
  ;"          (Note: a husband and wife could have same phone number
  ;"           and both have appt on same hour)
  ;"       ERRARRAY -- PASS BY REFERENCE.  An OUT PARAMETER.
  ;"         Format: ERRARRAY(DFN)=<MESSAGE>
  ;"       HOURLEAD -- The number of hours from NOW to get list from.  E.g if
  ;"                 value = 7, then should return list of patients with an 
  ;"                appt exactly 1 week from now.  All results returned
  ;"               be from the same hour, i.e. it doesn't return a range.
  ;"Result: none
  NEW TMGDFN SET TMGDFN=0
  FOR  SET TMGDFN=$ORDER(ARR(TMGDFN)) QUIT:(+TMGDFN'>0)  DO
  . NEW SUBIEN SET SUBIEN=$GET(ARR(TMGDFN)) ;"IEN in subfile 22723.01
  . NEW ZN SET ZN=$GET(^TMG(22723,TMGDFN,1,SUBIEN,0))
  . NEW TMGDT SET TMGDT=$PIECE(ZN,"^",1)
  . NEW PROVIEN SET PROVIEN=+$PIECE(ZN,"^",3)
  . NEW REASON SET REASON=$PIECE(ZN,"^",4)
  . NEW PHONEARR DO GETPHONS^TMGSMS01(TMGDFN,.PHONEARR,.ERRARRAY)
  . NEW MSG SET MSG=$$GETMSG(TMGDFN,PROVIEN,TMGDT,REASON,HOURLEAD)  
  . NEW PHONENUM SET PHONENUM=""
  . NEW HASNUM SET HASNUM=0
  . FOR  SET PHONENUM=$ORDER(PHONEARR(TMGDFN,PHONENUM)) QUIT:+PHONENUM'>0  DO
  . . SET ARR("MSG",TMGDFN,PHONENUM)=TMGDT_"^"_MSG
  . . SET HASNUM=1
  . IF HASNUM=0 SET ARR("NONE",TMGDFN)=MSG
  QUIT
  ;  
GETMSG(TMGDFN,PROVIEN,TMGDT,REASON,HOURLEAD) ;"Create out-going message.
  ;"Result: a message like this:
  ;"   [FIRST NAME] has appt with [DOCTOR NAME] on <DATE> for <purpose>
  ;"   Or "" if there was a problem.   
  NEW RESULT SET RESULT=""
  SET PROVIEN=+$GET(PROVIEN) GOTO:PROVIEN'>0 GMDN
  NEW PVFNAME SET PVFNAME=$$PVFNAME^TMGGDFNU(PROVIEN)
  NEW PVLNAME SET PVLNAME=$$PVLNAME^TMGGDFNU(PROVIEN)
  SET HOURLEAD=+$GET(HOURLEAD)
  ;"TO DO -- Check what happens with nursing visits, or INR visits.
  NEW PROVNAME SET PROVNAME="DR "_$EXTRACT(PVFNAME,1)_" "_PVLNAME
  NEW ARR SET ARR("PROVNAME")=PROVNAME 
  SET ARR("FNAME")=$$FNAME^TMGTIUO3(TMGDFN)
  IF ARR("FNAME")="" GOTO GMDN  ;"ABORT
  SET ARR("DATE")=$$GETDTSTR^TMGSMS01(TMGDT)
  SET ARR("REASON")=$$FIXREASN^TMGSMS01(REASON)
  SET ARR("TIME")=$$GETTIME(TMGDT)
  ;"NEW MSG SET MSG="|FNAME| has an appt with |PROVNAME| on |DATE|, for |REASON|. "
  NEW MSG SET MSG=""
  IF REASON="PROTIME" DO
  . SET MSG=$$GTMSGTXT^TMGSMS01("NURSE-HOUR","|FNAME| has an appt with nurse for Rx check at |TIME|.")
  ELSE  IF REASON["TH-" DO
  . SET MSG=$$GTMSGTXT^TMGSMS01("TELEMEDICINE-HOUR","|FNAME| has a telemedicine appointment TODAY at |TIME|. We will contact you when it is time to login. The virtual waiting room is at https://doxy.me/familyphysiciansofgreeneville")
  ELSE  IF REASON["TELEPHONE" DO
  . SET MSG=$$GTMSGTXT^TMGSMS01("TELEPHONE-HOUR","|FNAME| has a telephone appointment TODAY at |TIME|. We will contact you when we are ready to begin your visit.")
  ELSE  DO
  . SET MSG=$$GTMSGTXT^TMGSMS01("APPOINTMENT-HOUR","|FNAME| has an appt with |PROVNAME| TODAY at |TIME|.")
  . SET MSG=MSG_" "_$$GTMSGTXT^TMGSMS01("MEDS","Please bring ALL your medications with you.")
  SET MSG=MSG_" "_$$GTMSGTXT^TMGSMS01("QUESTIONS","QUESTIONS? Call Family Physicians of Greeneville (423-787-7000).")
  SET RESULT=$$SUBSTX^TMGSMS01(MSG,.ARR)
GMDN  
  QUIT RESULT
   ;
GETTIME(TMGDT)
  ;NEW RESULT SET RESULT=$$EXTDATE^TMGDATE(TMGDT)
  ;SET RESULT=$P(RESULT,"@",2)
  ;NEW HOUR,MIN SET HOUR=$P(RESULT,":",1,MIN=$P(RESULT,":",2)
  ;NEW SUFF SET SUFF="AM"
  ;IF HOUR>12
  NEW RESULT SET RESULT=$$FMTE^XLFDT(TMGDT,"1P")
  SET RESULT=$P(RESULT," ",4)_" "_$P(RESULT," ",5)
  QUIT RESULT
  ;"
SETUPSM0(OUT,AHOURLEAD,EXCLBYDL,TEST) ;
  ;"Purpose: To set up for sending out SMS messages
  ;"Input: OUT -- PASS BY REFERENCE -- an OUT PARAMETER.  Format:
  ;"         OUT("MSG",DFN,PhoneNumber)=ApptDT^<message>
  ;"         OUT("NONE",DFN)=<message>  <-- Message that could not be sent
  ;"         OUT(DFN)=fmdate
  ;"         (Prior contents of OUT killed)
  ;"       AHOURLEAD -- The number of hours from NOW to get list from.  E.g if
  ;"                 value = 1, then should return list of patients with an 
  ;"                 appt 1 hour (e.g. if ran at 8, returns 9:00, 9:15, and 
  ;"                 9:45) from now.  All results returned
  ;"               be from the same hour, i.e. it doesn't return a range.
  ;"       EXCLBYDL -- PASS BY REFERENCE.  Format: 
  ;"            EXCLBYDL(AHOURLEAD,[EXCLUDE ARRAY] ..See GETDTPTL() for format
  ;"       TEST - USED TO TEST, VERBOSE
  ;"Output: Alerts may be made in case of errors.
  ;"Result: None
  NEW ERRARRAY
  SET TEST=+$G(TEST)
  NEW EXCLUDE MERGE EXCLUDE=EXCLBYDL(AHOURLEAD)
  DO GETPTLST(.OUT,AHOURLEAD,"A",.EXCLUDE,TEST)  ;"Get list of patients to message 
  DO COMPMSG(.OUT,.ERRARRAY,AHOURLEAD) ;"compile outgoing messages
  ;"OUT FORMAT:
  ;" OUT("MSG",DFN,PhoneNumber)=ApptDT^<message>
  ;" OUT("NONE",DFN)=<message>  <-- Message that could not be sent  
  NEW TMGDFN SET TMGDFN=""
  FOR  SET TMGDFN=$ORDER(ERRARRAY(TMGDFN)) QUIT:TMGDFN'>0  DO
  . NEW ERRMSG SET ERRMSG=$GET(ERRARRAY(TMGDFN)) QUIT:ERRMSG=""
  . NEW PTNAME SET PTNAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1) QUIT:PTNAME=""
  . SET ERRMSG="Patient='"_PTNAME_"' (`"_TMGDFN_"), "_ERRMSG
  . DO ALERTERR^TMGKERN5(ERRMSG)
  ;"KILL OUT MERGE OUT=TEMP("MSG")  
  QUIT
  ;
ADDLINE(ARR,LINE) ;
  NEW IDX SET IDX=$GET(ARR)+1
  SET ARR(IDX)=LINE
  SET ARR=IDX
  QUIT                
  ;
SUEXCLUD(EXCLBYDL) ;"SET UP EXCLUSION
  ;"Purpose: to be able to specify fields (by node,piece) to check, and exclusion value
  ;"Input:  EXCLBYDL -- PASS BY REFERENCE.  Format: 
  ;"            EXCLBYDL(AHOURLEAD,[EXCLUDE ARRAY] ..See GETDTPTL() for format
  SET EXCLBYDL(7,0,4,"PROTIME")=""  ;"Don't send reminder for PROTIME 1 week in advance
  QUIT
  ;
SETUPSMS(ARR,STOREOUT,HOURLEAD,NONE,EXCLBYDL,TEST) ;
  ;"Purpose: To set up for sending out SMS messages
  ;"Input: ARR -- PASS BY REFERENCE -- an OUT PARAMETER.  Format:
  ;"         ARR(#)=output line for FTP file, to be uploaded to FTP server.
  ;"       STOREOUT -- PASS BY REFERENCE -- an OUT PARAMETER.  Format:
  ;"         STOREOUT(phone number)=DFN^ApptDT^Message
  ;"       HOURLEAD -- The number of hours from NOW to get list from.  E.g if
  ;"                 value = 7, then should return list of patients with an 
  ;"                appt exactly 1 week from now.  All results returned
  ;"               be from the same hour, i.e. it doesn't return a range.
  ;"               Format:  Number,Number,Number.....
  ;"       NONE -- PASS BY REFERENCE -- AN OUT PARAMETER.  List of patients 
  ;"           for whom an SMS message could not be sent out.  Format:
  ;"           NONE(DFN)=MSG <-- that could not be sent
  ;"       EXCLBYDL -- PASS BY REFERENCE.  Format: 
  ;"            EXCLBYDL(AHOURLEAD,[EXCLUDE ARRAY] ..See GETDTPTL() for format
  ;"       TEST -- USED TO TEST, VERBOSE
  ;"Output: Alerts may be made in case of errors.
  ;"Result: None
  NEW OUT,RESULT
  SET TEST=+$G(TEST)
  NEW EXCLBYDL DO SUEXCLUD(.EXCLBYDL)
  NEW IDX FOR IDX=1:1:$LENGTH(HOURLEAD,",") DO
  . NEW AHOURLEAD SET AHOURLEAD=$PIECE(HOURLEAD,",",IDX) QUIT:AHOURLEAD=""
  . NEW TEMP DO SETUPSM0(.TEMP,.AHOURLEAD,.EXCLBYDL,TEST) ;
  . MERGE OUT=TEMP
  SET RESULT=$$SUHEADER^TMGSMS01(.ARR)
  IF +RESULT'>0 GOTO SUSMDN  ;"SUHEADER makes it's own alert
  NEW TMGDFN SET TMGDFN=""
  FOR  SET TMGDFN=$ORDER(OUT("MSG",TMGDFN)) QUIT:(+TMGDFN'>0)  DO
  . NEW PHONE SET PHONE=""
  . FOR  SET PHONE=$ORDER(OUT("MSG",TMGDFN,PHONE)) QUIT:(PHONE="")  DO
  . . NEW LINE SET LINE=$GET(OUT("MSG",TMGDFN,PHONE)) QUIT:LINE=""
  . . NEW MSG SET MSG=$PIECE(LINE,"^",2) QUIT:MSG=""
  . . NEW DT SET DT=+$PIECE(LINE,"^",1) 
  . . DO ADDLINE(.ARR,"csv:"_PHONE_"|"_MSG)
  . . SET STOREOUT(PHONE)=TMGDFN_"^"_DT_"^"_MSG
  MERGE NONE=OUT("NONE")
SUSMDN  
  QUIT
  ;
SCHSTCHK ;" DO SCHEDULE TASK FOR STATUS CHECK
  NEW ZTRTN,ZTDESC,ZTDTH,ZTIO,%
  SET ZTRTN="GETSMSID^TMGKERN5(1)"
  SET ZTDESC="Get status of SMS messages"
  SET ZTIO=""
  DO NOW^%DTC
  SET ZTDTH=$$FMADD^XLFDT(%,0,0,10,0)
  DO ^%ZTLOAD
  QUIT
  ;
  ;"=============================================================
  ;"MAIN API
  ;"=============================================================
  ;
TSKSEND ;" <--- ENTRY POINT FOR TASKMAN FOR OPTION: 'TMG SMS SEND HOURLY MESSAGES'
  ;"Entry point for taskman, with task run daily
  ;"In general, send notification for 1 hour from now.
  ;"NOTE: this is currently scheduled to run every hour
  DO MSGLOG^TMGSMS02("RUNNING TSKSEND^TMGSMS01")
  NEW HOUR SET HOUR=+$E($$NOW^TMGDATE,1,2)
  IF HOUR<6 QUIT
  DO SENDSMS("1",0)
  QUIT
  ;"
TSKTEST
  NEW HOUR SET HOUR=0
  FOR  SET HOUR=HOUR+1 QUIT:HOUR>9  DO
  . NEW EXCLUDE MERGE EXCLUDE=EXCLBYDL(HOUR)
  . NEW OUT,ERRARRAY
  . DO GETPTLST(.OUT,HOUR,"A",.EXCLUDE,1)  ;"Get list of patients to message
  . DO COMPMSG(.OUT,.ERRARRAY,HOUR) ;"compile outgoing messages
  . WRITE "========= FOR ",HOUR," LEAD TIME==========",!
  . IF $D(OUT("MSG")) DO
  . . ZWR OUT 
  . KILL OUT
  QUIT
  ;"
SENDSMS(HOURLEAD,TEST) ;"Send out SMS messages for upcoming appts, via FTP method
  ;"INPUT: HOURLEAD -- number of hours in advance to get appts for.  Format:
  ;"            Number,Number,Number.....
  DO MSGLOG^TMGSMS02("RUNNING SENDSMS^TMGSMS01. HourLead="_$GET(HOURLEAD))
  SET TEST=+$G(TEST)
  NEW ARR,STORE,NONE
  DO SETUPSMS(.ARR,.STORE,.HOURLEAD,.NONE,.TEST)
  NEW LIVE SET LIVE=1  ;"SET TO 1 TO TURN SYSTEM ON.
  NEW COUNT,IDX SET (IDX,COUNT)=0
  FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:+IDX'>0  SET COUNT=COUNT+1  
  IF LIVE'=1 QUIT
  IF TEST=1 DO SAVEARR(.ARR)
  IF TEST=1 QUIT
  DO SMSSEND^TMGKERN5(.ARR,.STORE,LIVE)
  DO SCHSTCHK ;"Effects call to GETSMSID^TMGKERN5 after 10 minutes
  ;"Will remove below later...
  DO ALRTMSG^TMGKERN5("FYI: Sent "_COUNT_" SMS messages via FTP upload on "_$$FMTE^XLFDT($$NOW^XLFDT))
  ;"TO DO -- Handle the people that were NOT sent SMS messages, In NONE array
  QUIT
  ;  
SAVEARR(ARR)
  DO NOW^%DTC
  ;"MERGE ^TMG("HOURLY SMS",%)=ARR
  QUIT
  ;"
SENDSMS1(HOURLEAD) ;"Send out SMS messages for upcoming appts, via HTTP method
  ;"NOTE: This a suitable entry point for a TASKMAN task. 
  ;"NOTICE: This uses SMSSEND1^TMGKERN5, which sends out SMS messages
  ;"       one by one, via HTTP call achieved in script sms_send_msg.
  ;"       This method doesn't allow tracking of success of message, so
  ;"       will change to alternative method (FTP upload).  See SENDSMS()
  DO MSGLOG^TMGSMS02("RUNNING SENDSMS1^TMGSMS01")
  NEW OUT
  NEW EXCLBYDL DO SUEXCLUD(.EXCLBYDL)
  DO SETUPSM0(.OUT,.HOURLEAD,.EXCLBYDL) ;
  NEW LIVE SET LIVE=1  ;"SET TO 1 TO TURN SYSTEM ON.
  NEW TMGDFN SET TMGDFN=""
  FOR  SET TMGDFN=$ORDER(OUT("MSG",TMGDFN)) QUIT:(+TMGDFN'>0)  DO
  . NEW PHONE SET PHONE=""
  . FOR  SET PHONE=$ORDER(OUT("MSG",TMGDFN,PHONE)) QUIT:(PHONE="")  DO
  . . NEW MSG SET MSG=$GET(OUT("MSG",TMGDFN,PHONE)) QUIT:MSG=""
  . . IF LIVE=1 DO 
  . . . DO SMSSEND1^TMGKERN5(PHONE,MSG,TMGDFN) ;
  . . ELSE  WRITE PHONE," -- ",$LENGTH(MSG)," -- ",MSG,!
  NEW NONE MERGE NONE=OUT("NONE")
  ;"TO DO -- Handle the people that were NOT sent SMS messages, In NONE array
  QUIT
  ;
SMSGTALL()  ;"SMS GET ALL <-- suitable entry point for TASKMAN task
  ;"DO MSGLOG^TMGSMS02("RUNNING SMSGTALL^TMGSMS01")
  DO SMSGTALL^TMGKERN5()
  QUIT 
  ;
SMSGET(DELFILE) ;" Call shell script (custom) sms_download_msg, and get replies
  DO MSGLOG^TMGSMS02("RUNNING SENDGET^TMGSMS01")
  DO SMSGET^TMGKERN5(.DELFILE)
  QUIT
  ;
GETFSTAT() ;"Process FINAL status report. <--- SUITABLE ENTRY POINT FOR TASKMAN
  ;"NOTE: this is currently scheduled to run weekly 
  DO MSGLOG^TMGSMS02("RUNNING GETFSTAT^TMGSMS01")
  DO GETFSTAT^TMGSMS04()
  QUIT
  ;  
SMSCURL(TONUM,TOMSG)
   NEW INFO IF $$GETCRDNTL^TMGSMS01(.INFO,"TMG")  ;"//kt 5/23/24  ignore results, which could hold error message. 
   ;"NEW APIKEY SET APIKEY=$P($G(^TMG(22724.1,1,2)),"^",1)  ;"GET KEY FROM DATABASE FIELD
   ;"NEW APINUMBER SET APINUMBER=$P($G(^TMG(22724.1,1,2)),"^",2)  ;"GET NUMBER FROM DATABASE FIELD
   NEW APIKEY SET APIKEY=$GET(INFO("API KEY"))   
   NEW APINUMBER SET APINUMBER=$GET(INFO("API NUMBER"))
   NEW CURL
   SET CURL="curl -i \ "
   SET CURL=CURL_"-X POST \ "
   SET CURL=CURL_"-H ""Content-Type: application/json"" \"
   SET CURL=CURL_"-H ""Accept: application/json"" \"
   SET CURL=CURL_"-H ""Authorization: "_APIKEY_""" \"
   SET CURL=CURL_"-d '{""messages"": [{ ""channel"": ""whatsapp"", ""to"": ""14234260236""," 
   SET CURL=CURL_"""from"": """_APINUMBER_""", ""content"": """_TOMSG_""" }, {" 
   SET CURL=CURL_"""channel"": ""sms"", ""to"": """_TONUM_""", ""from"": """_APINUMBER_""", ""content"": """_TOMSG_""" }]}' \"
   SET CURL=CURL_"-s https://platform.clickatell.com/v1/message"
   W CURL,!
   new ok,C0CRSLT,C0CMIME
   s C0CMIME=""
   S ok=$$httpPOST^%zewdGTM("https://platform.clickatell.com/v1/message",.CURL,C0CMIME,.C0CRSLT,.HEADER,"",.gpl5,.C0CRHDR)
   QUIT