TMGSMS01 ;TMG/kst/OS SMS Message ;1/18/15, 2/24/21
         ;;1.0;TMG-LIB;**1**;12/9/14
 ;
 ;"TMG FUNCTIONS
 ;"I.e. functions that related to sending SMS messages
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
 ;"SENDSMS(DAYLEAD) -- Send out SMS messages for upcoming appts, vit FTP method 
 ;"SENDSMS1(DAYLEAD) -- Send out SMS messages for upcoming appts, via HTTP method
 ;"SNDTSMS(TMGDFN,PHONE) -- Send out 1 TEST SMS message, via FTP method
 ;"SMSGTALL() -- Get all incoming SMS messages 
 ;"SMSGET(DELFILE) -- Get all incoming SMS messages, optionally deleting source file.
 ;"GETFSTAT() -- Process FINAL status report.
 ;
 ;"GETPHONS(TMGDFN,OUT,ERRARRAY) -- for a given patient, return list of phone numbers
 ; 
 ;"=======================================================================
 ;" API -- Private Functions.
 ;"=======================================================================
 ;"SETUPSMS(OUT,DAYLEAD,ERRARRAY,NONE) -- set up for sending out SMS messages
 ;"SUDNFSMS(TMGDFN,PHONE,ARR,STOREOUT) --SET UP A TEST MESSAGE
 ;"SUHEADER(ARR) -- setup SMS header
 ;"GETDTPTL(OUT,TMGDT,EXCLUDE) -- get a list patients that have upcoming appts on TMGDT
 ;"GETPTLST(OUT,DAYLEAD,EXCLUDE) -- get a list patients that have upcoming appts
 ;
 ;"MKVALPHN(PHONENUM) -- return valid phone number, or "" if can't make valid
 ;"COMPMSG(ARR,ERRARRAY,DAYLEAD) -- create an array with messages to be sent by SMS
 ;"GETMSG(TMGDFN,PROVIEN,TMGDT,REASON,DAYLEAD) -- Create outogoing message.
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
  NEW ENDDT SET ENDDT=TMGDT+1
  SET TMGDT=TMGDT-0.000001
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
GETPTLST(OUT,ADAYLEAD,MODE,EXCLUDE) ;
  ;"Purpose: to get a list patients that have upcoming appts
  ;"Input:   OUT -- PASS BY REFERENCE.  Format as follows:
  ;"             OUT(TMGDFN)=SubIEN (file 22723.01)
  ;"             Prior data killed.  If no appts, then array returned empty.
  ;"         ADAYLEAD --  The number of days from NOW to get list from.  E.g if
  ;"                value = 7, then should return list of patients with an 
  ;"                appt 1 week from now.
  ;"         MODE - "A": return only active appts, "*": return any appt
  ;"         EXCLUDE -- PASS BY REFERENCE.  See GETDTPTL() for format
  ;"Results: none
  KILL OUT
  NEW NOW SET NOW=$$NOW^XLFDT
  NEW TARGETDT SET TARGETDT=$$FMADD^XLFDT(NOW\1,ADAYLEAD)
  DO GETDTPTL(.OUT,TARGETDT,.MODE,.EXCLUDE) 
  QUIT
  ;
GETPHONS(TMGDFN,OUT,ERRARRAY) ;"
  ;"Purpose: for a given patient, return list of phone numbers
  ;"Input: TMGDFN -- patient IEN
  ;"       OUT -- PASS BY REFERENCE.  An OUT PARAMETER.  Format:
  ;"          OUT(TMGDFN,<PHONE NUMBER>)=""
  ;"       ERRARRAY -- PASS BY REFERENCE.  An OUT PARAMETER.
  ;"         Format: ERRARRAY(TMGDFN)=<MESSAGE>
  ;"NOTE: All phone numbers MUST BE 11 digits, e.g. 14235551111
  ;"      ALSO, field 22705 (TMG SMS EXCLUSION) should be checked and
  ;"      no number should be returned if found there.
  NEW IENS SET IENS=TMGDFN_","
  NEW FIELDS
  SET FIELDS(.131)=""
  SET FIELDS(.132)=""
  SET FIELDS(.134)=""
  SET FIELDS(.2919)=""
  SET FIELDS(.2929)=""
  NEW IDX,TEMP SET IDX="",TEMP=""
  FOR  SET IDX=$ORDER(FIELDS(IDX)) QUIT:IDX=""  DO
  . SET:TEMP'="" TEMP=TEMP_";" SET TEMP=TEMP_IDX
  SET FIELDS=TEMP
  NEW TMGARRAY,TMGMSG
  ;"use DFN(IEN in file 2) to get data into database
  DO GETS^DIQ(2,IENS,FIELDS,"","TMGARRAY","TMGMSG") ;"get EXTERNAL values
  IF $DATA(TMGMSG("DIERR"))'=0 DO  GOTO GPDN   ;"check for errors.
  . ;"MERGE ERRARRAY=TMGMSG("DIERR")
  . SET ERRARRAY(TMGDFN)=$$GETERRST^TMGDEBU2(.TMGMSG)
  SET IDX="" FOR  SET IDX=$ORDER(FIELDS(IDX)) QUIT:(+IDX'>0)  DO
  . NEW FIELD SET FIELD=FIELDS(IDX)
  . NEW VAL SET VAL=$GET(TMGARRAY(2,IENS,IDX)) QUIT:VAL=""
  . NEW PHONE SET PHONE=$$MKVALPHN(VAL) QUIT:PHONE=""
  . IF $$ISEXLPHN(TMGDFN,PHONE) QUIT
  . NEW FLDNAME SET FLDNAME=$PIECE($GET(^DD(2,IDX,0)),"^",1)
  . SET OUT(TMGDFN,PHONE)=""
  . SET OUT(TMGDFN,"NAME",FLDNAME)=PHONE
GPDN ;  
  QUIT
  ;
MKVALPHN(PHONENUM) ;"MAKE VALID PHONE NUMBER
  ;"Return: valid phone number, or "" if can't make valid
  ;"MAKE SURE IS 11 DIGITS
  NEW RESULT SET RESULT=$TRANSLATE($GET(PHONENUM)," (-)","")
  IF $EXTRACT(RESULT,1,3)="000" DO
  . SET RESULT=$EXTRACT(RESULT,4,99)
  NEW LEN SET LEN=$LENGTH(RESULT)
  IF LEN=7 DO
  . SET RESULT="423"_RESULT,LEN=10  ;"NOTICE!!! hard-coded area code 423 assumption!
  IF LEN=10 DO
  . SET RESULT="1"_RESULT,LEN=11
  IF LEN'=11 SET RESULT=""
  QUIT RESULT      
  ;
ISEXLPHN(TMGDFN,PHONENUM) ;"IS EXCLUDED PHONE NUMBER?
  ;"Result: 1 if phone number has been entered 
  NEW RESULT 
  SET RESULT=($DATA(^DPT(TMGDFN,"TMGSMS","B",PHONENUM))>0)
  QUIT RESULT
  ;
COMPMSG(ARR,ERRARRAY,DAYLEAD) ;"COMPILE MESSAGES
  ;"Purpose: create an array with messages to be sent by SMS
  ;"Input: ARR -- PASS BY REFERENCE.  An IN and OUT parameter.  Format:
  ;"          for input, same as output of GETPTLIST: 
  ;"             ARR(DFN)=SubIEN (file 22723.01) (this is also a FM date/time)
  ;"          for output, data added to ARR.  Format:
  ;"              ARR("MSG",DFN,PhoneNumber)=ApptDT^<message>
  ;"              ARR("NONE",DFN)=<message>  <-- Message that could not be sent
  ;"          (Note: a husband and wife could have same phone number
  ;"           and both have appt on same day)
  ;"       ERRARRAY -- PASS BY REFERENCE.  An OUT PARAMETER.
  ;"         Format: ERRARRAY(DFN)=<MESSAGE>
  ;"       DAYLEAD -- The number of days from NOW to get list from.  E.g if
  ;"                 value = 7, then should return list of patients with an 
  ;"                appt exactly 1 week from now.  All results returned
  ;"               be from the same day, i.e. it doesn't return a range.
  ;"Result: none
  NEW TMGDFN SET TMGDFN=0
  FOR  SET TMGDFN=$ORDER(ARR(TMGDFN)) QUIT:(+TMGDFN'>0)  DO
  . NEW SUBIEN SET SUBIEN=$GET(ARR(TMGDFN)) ;"IEN in subfile 22723.01
  . NEW ZN SET ZN=$GET(^TMG(22723,TMGDFN,1,SUBIEN,0))
  . NEW TMGDT SET TMGDT=$PIECE(ZN,"^",1)
  . NEW PROVIEN SET PROVIEN=+$PIECE(ZN,"^",3)
  . NEW REASON SET REASON=$PIECE(ZN,"^",4)
  . NEW PHONEARR DO GETPHONS(TMGDFN,.PHONEARR,.ERRARRAY)
  . NEW MSG SET MSG=$$GETMSG(TMGDFN,PROVIEN,TMGDT,REASON,DAYLEAD)  
  . NEW PHONENUM SET PHONENUM=""
  . NEW HASNUM SET HASNUM=0
  . FOR  SET PHONENUM=$ORDER(PHONEARR(TMGDFN,PHONENUM)) QUIT:+PHONENUM'>0  DO
  . . SET ARR("MSG",TMGDFN,PHONENUM)=TMGDT_"^"_MSG
  . . SET HASNUM=1
  . IF HASNUM=0 SET ARR("NONE",TMGDFN)=MSG
  QUIT
  ;  
GETMSG(TMGDFN,PROVIEN,TMGDT,REASON,DAYLEAD) ;"Create out-going message.
  ;"Result: a message like this:
  ;"   [FIRST NAME] has appt with [DOCTOR NAME] on <DATE> for <purpose>
  ;"   Or "" if there was a problem.   
  NEW RESULT SET RESULT=""
  SET PROVIEN=+$GET(PROVIEN) GOTO:PROVIEN'>0 GMDN
  NEW PVFNAME SET PVFNAME=$$PVFNAME^TMGGDFNU(PROVIEN)
  NEW PVLNAME SET PVLNAME=$$PVLNAME^TMGGDFNU(PROVIEN)
  SET DAYLEAD=+$GET(DAYLEAD)
  ;"TO DO -- Check what happens with nursing visits, or INR visits.
  NEW PROVNAME SET PROVNAME="DR "_$EXTRACT(PVFNAME,1)_" "_PVLNAME
  NEW ARR SET ARR("PROVNAME")=PROVNAME 
  SET ARR("FNAME")=$$FNAME^TMGTIUO3(TMGDFN)
  IF ARR("FNAME")="" GOTO GMDN  ;"ABORT
  SET ARR("DATE")=$$GETDTSTR(TMGDT)
  SET ARR("REASON")=$$FIXREASN(REASON)
  ;"NEW MSG SET MSG="|FNAME| has an appt with |PROVNAME| on |DATE|, for |REASON|. "
  NEW MSG SET MSG=""
  IF REASON="PROTIME" DO
  . SET MSG=$$GTMSGTXT("NURSE","|FNAME| has an appt with nurse for Rx check on |DATE|.")
  ELSE  IF REASON["TH-" DO
  . SET MSG=$$GTMSGTXT("TELEMEDICINE","|FNAME| has a telemedicine appointment on |DATE|. We will contact you when it is time to login.")
  . ;"SET MSG=MSG_" The virtual waiting room is at https://doxy.me/familyphysiciansofgreeneville"
  ELSE  IF REASON["TELEPHONE" DO
  . SET MSG=$$GTMSGTXT("TELEPHONE","|FNAME| has a telephone appointment on |DATE|. We will contact you when we are ready to begin your visit.")
  ELSE  DO
  . SET MSG=$$GTMSGTXT("APPOINTMENT","|FNAME| has an appt with |PROVNAME| on |DATE|.")
  . IF (DAYLEAD>1) DO 
  . . SET MSG=MSG_" "_$$GTMSGTXT("CONFIRM","Reply ""OK"" to confirm. Please don't forget to get labs drawn, if ordered for this appt.")
  . ELSE  IF (TMGDT[".0815") DO
  . . SET MSG=MSG_" "_$$GTMSGTXT("MEDS","Please bring ALL your medications with you.")
  . ELSE  DO
  . . SET MSG=MSG_" "_$$GTMSGTXT("ARRIVE_EARLY","Please arrive 10 minutes prior to your appointment to complete paperwork. Please bring ALL your medications with you.")
  SET MSG=MSG_" "_$$GTMSGTXT("QUESTIONS","QUESTIONS? Call Family Physicians of Greeneville (423-787-7000).")
  SET RESULT=$$SUBSTX(MSG,.ARR)
GMDN  
  QUIT RESULT
   ;
GTMSGTXT(NAME,DEFAULT) 
  ;"CHECK FILE 22743 TO GET THE TEXT OF THE MESSAGE  
  ;"THE DEFAULT IS IN PLACE TO CATCH A TIME WHEN THE NEW METHOD FAILS
  ;"IF THAT OCCURS AN ALERT WILL BE SENT TO EDDIE. 
  NEW TEXT,IEN
  SET TEXT=""
  SET IEN=+$O(^TMG(22743,"B",NAME,0))  
  IF IEN'>0 DO
  . SET TEXT=DEFAULT
  . NEW ALERTRESULT
  . DO INFRMALT^TMGXQAL(.ALERTRESULT,150,"GTMSGTXT^TMGSMS01 COULD NOT FIND "_NAME)
  ELSE  DO
  . SET TEXT=$G(^TMG(22743,IEN,1))
  QUIT TEXT
  ;"
GETDTSTR(TMGDT) ;"Get friendly date string
  NEW RESULT SET RESULT=$$DOW^XLFDT(TMGDT)_", "_$$FMTE^XLFDT(TMGDT,"1P")
  NEW DELTA SET DELTA=$$FMDIFF^XLFDT(TMGDT,$$NOW^XLFDT)
  IF DELTA=0  SET RESULT=RESULT_" (TODAY)"
  IF DELTA=1  SET RESULT=RESULT_" (TOMORROW)"
  ;"SET RESULT=RESULT_","
  QUIT RESULT
  ;
SUBSTX(MSG,ARR) ;"Substitute array values into text string with |<KEY>|'s
  FOR  QUIT:MSG'["|"  DO
  . NEW PA,PB,PC
  . SET PA=$PIECE(MSG,"|",1),PB=$PIECE(MSG,"|",2,999)
  . SET PC=$PIECE(PB,"|",2,999),PB=$PIECE(PB,"|",1)
  . NEW SUBST SET SUBST=$GET(ARR(PB),"??")
  . SET MSG=PA_SUBST_PC
  QUIT MSG
  ;  
FIXREASN(REASON) ;"Convert SEQL PMS visit types into something patient readable
  IF REASON="SICK" DO
  . SET REASON="a sick visit"
  ;"finish...
  ;"NOTE: for now, we are not going to be sending reason with SMS message,
  ;"  so this is not needed for now.
  QUIT REASON
  ;
SETUPSM0(OUT,ADAYLEAD,EXCLBYDL) ;
  ;"Purpose: To set up for sending out SMS messages
  ;"Input: OUT -- PASS BY REFERENCE -- an OUT PARAMETER.  Format:
  ;"         OUT("MSG",DFN,PhoneNumber)=ApptDT^<message>
  ;"         OUT("NONE",DFN)=<message>  <-- Message that could not be sent
  ;"         OUT(DFN)=fmdate
  ;"         (Prior contents of OUT killed)
  ;"       ADAYLEAD -- The number of days from NOW to get list from.  E.g if
  ;"                 value = 7, then should return list of patients with an 
  ;"                appt exactly 1 week from now.  All results returned
  ;"               be from the same day, i.e. it doesn't return a range.
  ;"       EXCLBYDL -- PASS BY REFERENCE.  Format: 
  ;"            EXCLBYDL(ADAYLEAD,[EXCLUDE ARRAY] ..See GETDTPTL() for format
  ;"Output: Alerts may be made in case of errors.
  ;"Result: None
  NEW ERRARRAY
  NEW EXCLUDE MERGE EXCLUDE=EXCLBYDL(ADAYLEAD)
  DO GETPTLST(.OUT,ADAYLEAD,"A",.EXCLUDE)  ;"Get list of patients to message 
  DO COMPMSG(.OUT,.ERRARRAY,ADAYLEAD) ;"compile outgoing messages
  ;"OUT FORMAT:
  ;" OUT("MSG",DFN,PhoneNumber)=ApptDT^<message>
  ;" OUT("NONE",DFN)=<message>  <-- Message that could not be sent  
  NEW TMGDFN SET TMGDFN=""
  FOR  SET TMGDFN=$ORDER(ERRARRAY(TMGDFN)) QUIT:TMGDFN'>0  DO
  . NEW ERRMSG SET ERRMSG=$GET(ERRARRAY(TMGDFN)) QUIT:ERRMSG=""
  . NEW PTNAME SET PTNAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1) QUIT:PTNAME=""
  . SET ERRMSG="Patient='"_PTNAME_"' (`"_DFN_"), "_ERRMSG
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
GETCRDNTL(OUT,NAME) ;"GET CREDENTIALS
  ;"Purpose: get SMS credentials from file 22724.1 (TMG SMS CREDENTIALS)
  ;"Input: OUT -- PASS BY REFERENCE.  AN OUT PARAMETER.  Format:
  ;"         OUT("APIID")=api_id value
  ;"         OUT("NUMBER")=from value
  ;"         OUT("PW")=password value
  ;"         OUT("USER")=user value
  ;"       NAME -- the value of the .01 field record to use.  
  ;"Result: 1 if OK, or -1^Message if error. 
  KILL OUT                   
  SET NAME=$GET(NAME,"??")
  NEW RESULT SET RESULT=1
  NEW IEN SET IEN=+$ORDER(^TMG(22724.1,"B",NAME,0))
  NEW ZN SET ZN=$GET(^TMG(22724.1,IEN,0))
  SET OUT("APIID")=$PIECE(ZN,"^",2)
  SET OUT("PW")=$PIECE(ZN,"^",3)
  SET OUT("NUMBER")=$PIECE(ZN,"^",4)
  SET OUT("USER")=$PIECE(ZN,"^",5)
  IF ZN="" DO  GOTO GCRDN
  . SET RESULT="-1^Record not found in 22724.1 for "_NAME 
  IF OUT("APIID")="" DO  GOTO GCRDN
  . SET RESULT="-1^API ID not found in 22724.1 for IEN="_IEN 
  IF OUT("PW")="" DO  GOTO GCRDN
  . SET RESULT="-1^Password not found in 22724.1 for IEN="_IEN 
  IF OUT("NUMBER")="" DO  GOTO GCRDN
  . SET RESULT="-1^Long Number not found in 22724.1 for IEN="_IEN 
  IF OUT("USER")="" DO  GOTO GCRDN
  . SET RESULT="-1^User name not found in 22724.1 for IEN="_IEN 
GCRDN ;  
  QUIT RESULT
  ;
SUHEADER(ARR) ;
  NEW PWINFO,RESULT
  SET RESULT=$$GETCRDNTL(.PWINFO,"TMG") ;"<--- hard coded for FP of Greeneville ("TMG").  Change if needed
  IF +RESULT'>0 DO  GOTO SUHDN
  . DO ALERTERR^TMGKERN5($PIECE(RESULT,"^",2))  
  KILL ARR
  DO ADDLINE(.ARR,"user:"_PWINFO("USER"))
  DO ADDLINE(.ARR,"password:"_PWINFO("PW"))
  DO ADDLINE(.ARR,"api_id:"_PWINFO("APIID"))
  DO ADDLINE(.ARR,"from:"_PWINFO("NUMBER"))
  DO ADDLINE(.ARR,"mo:1")
  DO ADDLINE(.ARR,"text:#field1#")
  DO ADDLINE(.ARR,"Delimiter:|")
SUHDN ;
  QUIT RESULT
  ;
SUEXCLUD(EXCLBYDL) ;"SET UP EXCLUSION
  ;"Purpose: to be able to specify fields (by node,piece) to check, and exclusion value
  ;"Input:  EXCLBYDL -- PASS BY REFERENCE.  Format: 
  ;"            EXCLBYDL(ADAYLEAD,[EXCLUDE ARRAY] ..See GETDTPTL() for format
  SET EXCLBYDL(7,0,4,"PROTIME")=""  ;"Don't send reminder for PROTIME 1 week in advance
  QUIT
  ;
SETUPSMS(ARR,STOREOUT,DAYLEAD,NONE,EXCLBYDL) ;
  ;"Purpose: To set up for sending out SMS messages
  ;"Input: ARR -- PASS BY REFERENCE -- an OUT PARAMETER.  Format:
  ;"         ARR(#)=output line for FTP file, to be uploaded to FTP server.
  ;"       STOREOUT -- PASS BY REFERENCE -- an OUT PARAMETER.  Format:
  ;"         STOREOUT(phone number)=DFN^ApptDT^Message
  ;"       DAYLEAD -- The number of days from NOW to get list from.  E.g if
  ;"                 value = 7, then should return list of patients with an 
  ;"                appt exactly 1 week from now.  All results returned
  ;"               be from the same day, i.e. it doesn't return a range.
  ;"               Format:  Number,Number,Number.....
  ;"       NONE -- PASS BY REFERENCE -- AN OUT PARAMETER.  List of patients 
  ;"           for whom an SMS message could not be sent out.  Format:
  ;"           NONE(DFN)=MSG <-- that could not be sent
  ;"       EXCLBYDL -- PASS BY REFERENCE.  Format: 
  ;"            EXCLBYDL(ADAYLEAD,[EXCLUDE ARRAY] ..See GETDTPTL() for format
  ;"Output: Alerts may be made in case of errors.
  ;"Result: None
  NEW OUT,RESULT
  NEW EXCLBYDL DO SUEXCLUD(.EXCLBYDL)
  NEW IDX FOR IDX=1:1:$LENGTH(DAYLEAD,",") DO
  . NEW ADAYLEAD SET ADAYLEAD=$PIECE(DAYLEAD,",",IDX) QUIT:ADAYLEAD=""
  . NEW TEMP DO SETUPSM0(.TEMP,.ADAYLEAD,.EXCLBYDL) ;
  . MERGE OUT=TEMP
  SET RESULT=$$SUHEADER(.ARR)
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
SUDNFSMS(TMGDFN,PHONE,ARR,STOREOUT) ;"SET UP A TEST MESSAGE
  ;"Purpose: To set up for sending out 1 *TEST* SMS messages
  ;"Input: TMGDFN -- PATIENT IEN.  Can be 0 if patient unknown.
  ;"       PHONE -- 11 digit number to send message to. 
  ;"       ARR -- PASS BY REFERENCE -- an OUT PARAMETER.  Format:
  ;"         ARR(#)=output line for FTP file, to be uploaded to FTP server.
  ;"       STOREOUT -- PASS BY REFERENCE -- an OUT PARAMETER.  Format:
  ;"         STOREOUT(phone number)=DFN^Message
  ;"Output: Alerts may be made in case of errors.
  ;"Result: None
  SET TMGDFN=+$GET(TMGDFN)
  SET PHONE=+$GET(PHONE)
  NEW MSG SET MSG="Test message from Family Physicians of Greeneville."
  SET MSG=MSG_" Please arrive 10 minutes prior to your appointment to complete paperwork. Please bring ALL your medications with you. "
  SET MSG=MSG_" "_$$GTMSGTXT("QUESTIONS","QUESTIONS? Call Family Physicians of Greeneville (423-787-7000).")
  ;"SET MSG=MSG_" Questions?  Call (423) "
  NEW RESULT SET RESULT=$$SUHEADER(.ARR)
  IF +RESULT'>0 GOTO SUTSMDN  ;"SUHEADER makes it's own alert
  DO ADDLINE(.ARR,"csv:"_PHONE_"|"_MSG)
  SET STOREOUT(PHONE)=TMGDFN_"^"_MSG
SUTSMDN  
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
TSKSEND ;" <--- ENTRY POINT FOR TASKMAN FOR OPTION: 'TMG SMS SEND MESSAGES'
  ;"Entry point for taskman, with task run daily
  ;"In general, send notification for 1 day and 1 week from now.
  ;"NOTE: this is currently scheduled to run every morning at 8 AM 
  DO MSGLOG^TMGSMS02("RUNNING TSKSEND^TMGSMS01")
  DO SENDSMS("1,7")
  QUIT
  ;"
SNDTSMS(TMGDFN,PHONE) ;"Send out 1 TEST SMS message, via FTP method
  ;"INPUT: TMGDFN -- PATIENT IEN.  Can be 0 if patient unknown.
  ;"       PHONE -- 11 digit number to send message to.
  NEW ARR,STORE
  DO SUDNFSMS(.TMGDFN,.PHONE,.ARR,.STORE)  ;"SET UP A TEST MESSAGE
  DO SMSSEND^TMGKERN5(.ARR,.STORE,1)
  DO SCHSTCHK  ;"Effects call to GETSMSID^TMGKERN5 after 10 minutes
  DO ALRTMSG^TMGKERN5("FYI: Sent 1 *TEST* SMS messages via FTP upload")
  QUIT
  ;  
SENDSMS(DAYLEAD) ;"Send out SMS messages for upcoming appts, via FTP method
  ;"INPUT: DAYLEAD -- number of days in advance to get appts for.  Format:
  ;"            Number,Number,Number.....
  DO MSGLOG^TMGSMS02("RUNNING SENDSMS^TMGSMS01. DayLead="_$GET(DAYLEAD))
  NEW ARR,STORE,NONE
  DO SETUPSMS(.ARR,.STORE,.DAYLEAD,.NONE)
  NEW LIVE SET LIVE=1  ;"SET TO 1 TO TURN SYSTEM ON.
  NEW COUNT,IDX SET (IDX,COUNT)=0
  FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:+IDX'>0  SET COUNT=COUNT+1  
  IF LIVE'=1 QUIT
  DO SMSSEND^TMGKERN5(.ARR,.STORE,LIVE)
  DO SCHSTCHK ;"Effects call to GETSMSID^TMGKERN5 after 10 minutes
  ;"Will remove below later...
  DO ALRTMSG^TMGKERN5("FYI: Sent "_COUNT_" SMS messages via FTP upload on "_$$FMTE^XLFDT($$NOW^XLFDT))
  ;"TO DO -- Handle the people that were NOT sent SMS messages, In NONE array
  QUIT
  ;  
SENDSMS1(DAYLEAD) ;"Send out SMS messages for upcoming appts, via HTTP method
  ;"NOTE: This a suitable entry point for a TASKMAN task. 
  ;"NOTICE: This uses SMSSEND1^TMGKERN5, which sends out SMS messages
  ;"       one by one, via HTTP call achieved in script sms_send_msg.
  ;"       This method doesn't allow tracking of success of message, so
  ;"       will change to alternative method (FTP upload).  See SENDSMS()
  DO MSGLOG^TMGSMS02("RUNNING SENDSMS1^TMGSMS01")
  NEW OUT
  NEW EXCLBYDL DO SUEXCLUD(.EXCLBYDL)
  DO SETUPSM0(.OUT,.DAYLEAD,.EXCLBYDL) ;
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
FNAME(TMGDFN)
  NEW NAME
  SET NAME=$P($G(^DPT(TMGDFN,0)),"^",1)
  SET NAME=$P(NAME,",",2)
  SET NAME=$P(NAME," ",1)
  QUIT NAME
  ;"
SENDLAB1(DAYLEAD) ;"Send out SMS messages for upcoming appts, via HTTP method
  ;"NOTE: This a suitable entry point for a TASKMAN task. 
  ;"NOTICE: This uses SMSSEND1^TMGKERN5, which sends out SMS messages
  ;"       one by one, via HTTP call achieved in script sms_send_msg.
  ;"       This method doesn't allow tracking of success of message, so
  ;"       will change to alternative method (FTP upload).  See SENDSMS()
  DO MSGLOG^TMGSMS02("RUNNING SENDSMS1^TMGSMS01")
  NEW OUT,ARR,STORE
  NEW RESULT SET RESULT=$$SUHEADER^TMGSMS01(.ARR)
  NEW EXCLBYDL DO SUEXCLUD(.EXCLBYDL)
  DO SETUPSM0(.OUT,.DAYLEAD,.EXCLBYDL) ;
  NEW LIVE SET LIVE=1  ;"SET TO 1 TO TURN SYSTEM ON.
  NEW TMGDFN SET TMGDFN=""
  FOR  SET TMGDFN=$ORDER(OUT("MSG",TMGDFN)) QUIT:(+TMGDFN'>0)  DO
  . NEW PHONE SET PHONE=""
  . FOR  SET PHONE=$ORDER(OUT("MSG",TMGDFN,PHONE)) QUIT:(PHONE="")  DO
  . . NEW MSG SET MSG=$GET(OUT("MSG",TMGDFN,PHONE)) QUIT:MSG=""
  . . SET MSG="Good morning "_$$FNAME(TMGDFN)_". Dr. Toppenberg's office and our lab will be closed today due to current road conditions. We will reopen on Monday Jan 22,2024. Thank you and have a great day."
  . . IF LIVE=1 DO 
  . . . DO ADDLINE^TMGSMS01(.ARR,"csv:"_PHONE_"|"_MSG)
  . . . ;"DO SMSSEND1^TMGKERN5(PHONE,MSG,TMGDFN) ;
  . . ELSE  WRITE PHONE," -- ",$LENGTH(MSG)," -- ",MSG,!
  ;"NEW NONE MERGE NONE=OUT("NONE")
  DO ADDLINE^TMGSMS01(.ARR,"csv:14234260236|"_"TEST2")
  ZWR ARR
  DO SMSSEND^TMGKERN5(.ARR,.STORE,1)
  QUIT
  ;"
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
 ;"
 ;"===========================================================================================
 ;"The procedures below are single use only. Kept here to repurpose in the
 ;"future if need be but can be deleted before pushing up to github
 ;" 
SENDFLUMSG
  NEW MESSAGE 
  SET MESSAGE="Family Physicians of Greeneville is now offering Flu Vaccinations. We are accepting walk-ins for flu vaccines every Mon,Tues,Thurs,and Friday from 9A-11:30A and 1P-4P."
  SET MESSAGE=MESSAGE_" You can also get one during your in-office appointment. If you have any questions, please call us at 423-787-7000."
  DO ONEMSG(MESSAGE)
  QUIT
  ;" 
SENDWEBSITEMSG
  NEW MESSAGE 
  SET MESSAGE="Please visit the Family Physicians website for updated Coronavirus information at www.familyphysiciansofgreeneville.com/covid-19"
  DO ONEMSG(MESSAGE)
  QUIT
  ;"
SENDTELEMEDMSG
  NEW MESSAGE
  SET MESSAGE="Family Physicians of Greeneville is currently seeing all patients via Telemedicine or telephone visits. Call 423-787-7000 or visit https://www.familyphysiciansofgreeneville.com/telemedicine for more details."
  DO ONEMSG(MESSAGE)
  QUIT
  ;"
SENDWAITINGROOM
  NEW MESSAGE SET MESSAGE="ATTENTION: Due to the COVID-19 emergency, Dr. Toppenberg's office will remain open but the waiting room will be closed."
  SET MESSAGE=MESSAGE_" When you arrive in the parking lot,"
  SET MESSAGE=MESSAGE_" please call 423-787-7000 for further instructions. Appointments for bloodwork are available, if desired."
  NEW ARR,STORE
  NEW RESULT SET RESULT=$$SUHEADER^TMGSMS01(.ARR)
  ;"SCHEDULE, very ugly function for now. revise this later
  NEW PATLIST
  NEW DATELIST,DATE,TEMPPATLIST,EXLIST
  SET DATELIST(3200324)="",DATELIST(3200326)="",DATELIST(3200327)="",DATELIST(3200330)="" 
  SET DATE=0
  FOR  SET DATE=$O(DATELIST(DATE)) QUIT:DATE'>0  DO
  . DO GETDTPTL(.TEMPPATLIST,DATE,"A",.EXLIST) ;
  . MERGE PATLIST=TEMPPATLIST
  ;"GET ACTIVE PHONE NUMBERS FOR THOSE PATIENTS
  NEW PHONELIST,TMGDFN,ERRARRAY,COUNT
  SET TMGDFN=0
  FOR  SET TMGDFN=$O(PATLIST(TMGDFN)) QUIT:TMGDFN'>0  DO
  . DO GETPHONS(TMGDFN,.PHONELIST,.ERRARRAY) ;"
  ;"CYCLE THROUGH THE NUMBERS AND BUILD THE SEND ARRAY
  SET TMGDFN=0,COUNT=0
  FOR  SET TMGDFN=$O(PHONELIST(TMGDFN)) QUIT:TMGDFN'>0  DO
  . NEW PHONENUM SET PHONENUM=0
  . FOR  SET PHONENUM=$O(PHONELIST(TMGDFN,PHONENUM)) QUIT:+PHONENUM'>0  DO
  . . DO ADDLINE^TMGSMS01(.ARR,"csv:"_PHONENUM_"|"_MESSAGE)
  ;"DO ADDLINE^TMGSMS01(.ARR,"csv:14235258434|"_MESSAGE)
  ;"DO ADDLINE^TMGSMS01(.ARR,"csv:14233295443|"_MESSAGE)
  ;"DO ADDLINE^TMGSMS01(.ARR,"csv:14233295446|"_MESSAGE)
  ZWR ARR
  ;"WRITE "NOT SENDING AT THIS TIME",!
  DO SMSSEND^TMGKERN5(.ARR,.STORE,1)
  QUIT
  ;"
SENDTELM(NUMBER)
  ;"This function sends the virtual waiting room link to 
  NEW MESSAGE
  SET MESSAGE="The virtual waiting room is at https://doxy.me/familyphysiciansofgreeneville"
  NEW ARR,STORE
  NEW RESULT SET RESULT=$$SUHEADER^TMGSMS01(.ARR)
  ;"GET ACTIVE PT LIST
  NEW PHONELIST,TMGDFN,ERRARRAY,COUNT
  DO ADDLINE^TMGSMS01(.ARR,"csv:"_NUMBER_"|"_MESSAGE)
  ZWR ARR
  ;"WRITE "NOT SENDING AT THIS TIME",!
  DO SMSSEND^TMGKERN5(.ARR,.STORE,1)
  QUIT
  ;"
ONEMSG(MESSAGE)  ;"
  ;"This takes a message and texts it to all patients seen in the last 12
  ;"      months who are active
  NEW ARR,STORE
  NEW RESULT SET RESULT=$$SUHEADER^TMGSMS01(.ARR)
  ;"GET ACTIVE PT LIST  
  NEW PATLIST 
  DO GETACTPTS^TMGPXR03(.PATLIST,1)
  ;"GET ACTIVE PHONE NUMBERS FOR THOSE PATIENTS
  NEW PHONELIST,TMGDFN,ERRARRAY,COUNT
  SET TMGDFN=0
  FOR  SET TMGDFN=$O(PATLIST(TMGDFN)) QUIT:TMGDFN'>0  DO
  . DO GETPHONS(TMGDFN,.PHONELIST,.ERRARRAY) ;"
  ;"CYCLE THROUGH THE NUMBERS AND BUILD THE SEND ARRAY
  SET TMGDFN=0,COUNT=0
  FOR  SET TMGDFN=$O(PHONELIST(TMGDFN)) QUIT:TMGDFN'>0  DO
  . NEW PHONENUM SET PHONENUM=0
  . FOR  SET PHONENUM=$O(PHONELIST(TMGDFN,PHONENUM)) QUIT:+PHONENUM'>0  DO
  . . DO ADDLINE^TMGSMS01(.ARR,"csv:"_PHONENUM_"|"_MESSAGE)
  . . SET COUNT=COUNT+1
  ;"DO ADDLINE^TMGSMS01(.ARR,"csv:14235258434|"_MESSAGE)
  ;"DO ADDLINE^TMGSMS01(.ARR,"csv:14233295443|"_MESSAGE)
  ;"DO ADDLINE^TMGSMS01(.ARR,"csv:14234260236|"_MESSAGE)
  ZWR ARR
  WRITE COUNT," TOTAL MESSAGES",!
  ;"WRITE "NOT SENDING AT THIS TIME",!
  DO SMSSEND^TMGKERN5(.ARR,.STORE,1) 
  QUIT
  ;"
SENDBCBS
  NEW MESSAGE SET MESSAGE="FROM DR. KEVIN TOPPENBERG. It has come to our attention that BCBS is mailing out letters saying that I am no longer in network."
  SET MESSAGE=MESSAGE_" This is not accurate. I left a hospitalist group, but am still in network with your BCBS plan."
  SET MESSAGE=MESSAGE_" Please call 423-787-7000 if you have any further questions."
  NEW INSTOSEND
  SET INSTOSEND(15)="" ;"BCBS HOST
  SET INSTOSEND(28)="" ;"BCBS WALMART
  NEW ARR,STORE
  NEW RESULT SET RESULT=$$SUHEADER^TMGSMS01(.ARR)
  ;"SCHEDULE, very ugly function for now. revise this later
  NEW PATLIST
  ;"GET ACTIVE PHONE NUMBERS FOR THOSE PATIENTS
  NEW PHONELIST,TMGDFN,ERRARRAY,COUNT,TESTLIST
  SET TMGDFN=0
  NEW COUNT SET COUNT=1
  FOR  SET TMGDFN=$O(^DPT(TMGDFN)) QUIT:TMGDFN'>0  DO
  . NEW INSIDX SET INSIDX=0
  . FOR  SET INSIDX=$O(^DPT(TMGDFN,.312,INSIDX)) QUIT:INSIDX'>0  DO
  . . IF $$ACTIVEPT^TMGPXR03(TMGDFN)'=1 QUIT  ;"ONLY CHECK ACTIVE PATIENTS
  . . NEW INSTYPE,INSCOB
  . . SET INSTYPE=$P($G(^DPT(TMGDFN,.312,INSIDX,0)),"^",1)
  . . SET INSCOB=$P($G(^DPT(TMGDFN,.312,INSIDX,0)),"^",20)
  . . IF INSCOB'=1 QUIT   ;"ONLY USE PRIMARY
  . . IF '$D(INSTOSEND(INSTYPE)) QUIT  ;"ONLY USE THE INS WE SPECIFY ABOVE
  . . SET PATLIST(TMGDFN)=""
  . . SET TESTLIST($P($G(^DPT(TMGDFN,0)),"^",1))=""
  . . SET COUNT=COUNT+1
  ZWR TESTLIST
  W COUNT,!
  SET TMGDFN=0
  FOR  SET TMGDFN=$O(PATLIST(TMGDFN)) QUIT:TMGDFN'>0  DO
  . DO GETPHONS(TMGDFN,.PHONELIST,.ERRARRAY) ;"
  ;"CYCLE THROUGH THE NUMBERS AND BUILD THE SEND ARRAY
  SET TMGDFN=0,COUNT=0
  FOR  SET TMGDFN=$O(PHONELIST(TMGDFN)) QUIT:TMGDFN'>0  DO
  . NEW PHONENUM SET PHONENUM=0
  . FOR  SET PHONENUM=$O(PHONELIST(TMGDFN,PHONENUM)) QUIT:+PHONENUM'>0  DO
  . . DO ADDLINE^TMGSMS01(.ARR,"csv:"_PHONENUM_"|"_MESSAGE)
  DO ADDLINE^TMGSMS01(.ARR,"csv:14234260236|"_MESSAGE)
  ;"DO ADDLINE^TMGSMS01(.ARR,"csv:14233295443|"_MESSAGE)
  ;"DO ADDLINE^TMGSMS01(.ARR,"csv:14233295446|"_MESSAGE)
  ;"ZWR ARR
  WRITE "NOT SENDING AT THIS TIME",!
  DO SMSSEND^TMGKERN5(.ARR,.STORE,1)
  QUIT
  ;"
