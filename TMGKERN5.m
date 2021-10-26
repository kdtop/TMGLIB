TMGKERN5 ;TMG/kst/OS Specific functions -- SMS Message ;12/8/14, 3/24/21
         ;;1.0;TMG-LIB;**1**;12/5/14
 ;
 ;"TMG KERNEL FUNCTIONS
 ;"I.e. functions that are OS specific.
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
 ;"SMSSEND(ARRAY,STORE,LIVE) -- SEND SMS MESSAGE via FTP batch
 ;"SMSSEND1(NUMBER,MSG,TMGDFN) -- SEND SMS MESSAGE via HTTP request
 ;"SMSGTALL() -- Get all SMS messages
 ;"GETSMSID(DELFILE) -- GET SMS MESSAGEID OF MESSAGES BY FTP BATCH
 ;
 ;"=======================================================================
 ;" Private Functions.
 ;"=======================================================================
 ;"COMFILE() -- RETURN COM FILE NAME
 ;"COMFILEP() -- RETURN COM FILE NAME WITH PATH
 ;"SMSGET(DELFILE,OUTREF) -- call shell script (custom) sms_download_msg, and get replies
 ;"PARS1SMS(OUTREF,ID,DATE,TIME,MOBILENUM,TEXT) -- Store one message in the OUTREF
 ;"NSURSSMS(ARR,ID,STATUS) -- ENSURE STORAGE OF SMS MESSAGE
 ;"MSGEXIST(REF,ID) -- Determine if record already exists
 ;"ALERTERR(ERRTEXT,AMSG) -- Set up alerts for error handling of SMS problems.
 ;"ALRTMSG(TEXT,AMSG,IENS) -- Set up alerts for message handling of SMS issues.
 ;"SENDALERT(RECIPIENT,HEADING,MSG,HANDLER,IENS) -- SEND ALERT  
 ;
 ;"=======================================================================
 ;"Dependancies  TMGIOUTL, TMGDEBU2, TMGIOUT3, TMGIOUTL
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;
FILEDATE() ;"RETURN FM DT WITHOUT "." FOR USE WITH THE FILENAME
  NEW X,% DO NOW^%DTC
  SET %=$TR(%,".","")
  QUIT %
  ;
COMFILE() ;"RETURN COM FILE NAME
  QUIT "sms_batch_msgs_"_$$FILEDATE_".txt"
  ;
COMFILEP() ;"RETURN COM FILE NAME WITH PATH
  QUIT "/tmp/"_$$COMFILE
  ;
SMSSEND(ARRAY,STORE,LIVE) ;"SEND SMS MESSAGES via FTP BATCH
  ;"Purpose: to send out SMS message via shell command
  ;"Input:  ARRAY -- PASS BY REFERENCE.  Format:
  ;"          ARRAY(1)=first line of text file to send to FTP
  ;"          ARRAY(2)=second line of text file to send to FTP
  ;"          ...
  ;"          ARRAY(#)=second line of text file to send to FTP
  ;"        STORE -- PASS BY REFERENCE.  Format:
  ;"          STORE(PHONENUMBER)=DFN^ApptDT^Message
  ;"        LIVE -- Optional.  Default is 0.  Set to 1 for actual sending.
  ;"Result: NONE
  NEW RESULT SET RESULT="1^OK"
  SET LIVE=+$GET(LIVE)
  NEW FNAME SET FNAME=$$COMFILEP()
  SET RESULT=$$AR2HFSFP^TMGIOUT3("ARRAY",FNAME) ;"Array to HFS. 
  ;"Result of 1 is OK, 0 is error
  IF RESULT=0 DO  GOTO SMSSDN
  . SET RESULT="-1^Unable to store message file to HFS file '"_FNAME_"'"  
  NEW P SET P="temp"
  ;"NEW HOOKCMD SET HOOKCMD="sms_upload_msg -f="_FNAME   7/20/21
  NEW HOOKCMD SET HOOKCMD="python /opt/worldvista/EHR/bin/sms_upload_msg.py -f="_FNAME
  IF LIVE=0 DO
  . SET HOOKCMD=HOOKCMD_" --test"
  OPEN P:(COMMAND=HOOKCMD:readonly)::"pipe"
  USE P
  NEW LINEIN,ID,%
  NEW DELFILE SET DELFILE=1
  FOR  DO  QUIT:($ZEOF)
  . READ LINEIN
  . IF LINEIN["ERR" DO
  . . DO ALERTERR(LINEIN_" SENDING SMS MESSAGE BATCH - EDDIE TO TEST")
  . . ;"saving off some info to help debug later
  . . SET DELFILE=0
  . . MERGE ^TMG("SMSSEND","ARRAY")=ARRAY
  . . MERGE ^TMG("SMSSEND","STORE")=STORE
  . . SET ^TMG("SMSSEND","FNAME")=FNAME
  CLOSE P
  USE $P
  IF DELFILE=1 DO
  . SET RESULT=$$DELFILE^TMGIOUTL(FNAME)
  ELSE  DO
  . SET RESULT=1
  ;"Result of 1 is OK, 0 is error
  IF RESULT=0 DO  GOTO SMSSDN
  . SET RESULT="-1^Unable to delete message file on HFS file '"_FNAME_"'"  
  ;"=============================================
  IF LIVE=0 GOTO SMSSDN 
  NEW LOGARR
  NEW PHONE SET PHONE=""
  FOR  SET PHONE=$ORDER(STORE(PHONE)) QUIT:PHONE=""  DO
  . NEW TEMP
  . NEW LINE SET LINE=$GET(STORE(PHONE)) QUIT:LINE=""
  . NEW ADFN SET ADFN=$PIECE(LINE,"^",1)
  . NEW APPTDT SET APPTDT=$PIECE(LINE,"^",2)  ;"appt date time
  . NEW MSG SET MSG=$PIECE(LINE,"^",3)
  . SET TEMP(0,"DT")=$$NOW^XLFDT  ;"time of message
  . SET TEMP(0,"NUMBER")=PHONE
  . SET TEMP(0,"MSG")=MSG
  . DO NSURSSMS(.TEMP,0,"P",ADFN) ;"P=PENDING STATUS.  Will get final status later...
  . NEW NAME SET NAME=$PIECE($GET(^DPT(ADFN,0)),"^",1)
  . NEW IDX SET IDX=+$ORDER(LOGARR(""),-1)+1
  . SET LOGARR(IDX)="Phone: "_PHONE_"; Appt: "_$$FMTE^XLFDT(APPTDT,"5ZFMP")_"; To: "_NAME   
  SET RESULT=$$ADDLOG^TMGLOG01("SMS MESSAGES SENT","SMS messages sent",.LOGARR)
SMSSDN ;
  IF +RESULT'>0 DO
  . DO ALERTERR^TMGKERN5($PIECE(RESULT,"^",2))  
  QUIT
  ;
SMSSEND1(NUMBER,MSG,TMGDFN) ;"SEND SMS MESSAGE VIA HTTP REQUEST
  ;"Purpose: to send out SMS message via shell command
  ;"Input: NUMBER -- the phone number.  Must be FULL number, e.g. 14234445555
  ;"       MSG -- the message to send.  Must be < 160 characters.
  ;"       TMGDFN -- OPTIONAL.  If provided, used to track who message was sent to
  ;"NOTICE: This sends out SMS messages one by one, via HTTP call achieved in 
  ;"       script 'sms_send_msg'. This method doesn't allow tracking of success of 
  ;"       message, so will change to alternative method (FTP upload).  
  ;"       See SMSSEND
  ;"NOTICE: This function is not currently being used.  It is older.  It doesn't
  ;"     do many things that SMSSEND does. If it is to be used, then it should be
  ;"     brought up to specs of SMSSEND
  ;"Result: NONE
  IF $LENGTH(NUMBER)'=11 GOTO SMSSDN0  ;"FILE ERROR OR NOT?
  IF $EXTRACT(NUMBER)'="1" GOTO SMSSDN0  ;"FILE ERROR OR NOT?
  NEW P SET P="temp"
  NEW HOOKCMD
  ;"SET HOOKCMD="sms_send_msg -n="_NUMBER_" -m='"_MSG_"'"   7/20/21
  SET HOOKCMD="python /opt/worldvista/EHR/bin/sms_send_msg.py -n="_NUMBER_" -m='"_MSG_"'"   
  OPEN P:(COMMAND=HOOKCMD:readonly)::"pipe"
  USE P
  NEW LINEIN,ID,%,ERROR
  SET ERROR=0
  FOR  DO  QUIT:($ZEOF)
  . READ LINEIN
  . IF LINEIN["ERR" DO
  . . SET ERROR=1  
  . . DO ALERTERR(LINEIN_" SENDING SMS MESSAGE TO "_NUMBER_" ORIGINAL MESSAGE: "_MSG)
  . IF LINEIN["ID:" DO
  . . SET ID=$$TRIM^XLFSTR($PIECE(LINEIN,"ID: ",2))
  . . SET ID=$PIECE(ID,"']",1)
  CLOSE P
  USE $P
  IF ERROR=1 GOTO SMSSDN0
  NEW STORE
  SET STORE(ID,"DT")=$$NOW^XLFDT
  SET STORE(ID,"NUMBER")=NUMBER
  SET STORE(ID,"MSG")=MSG
  DO NSURSSMS(.STORE,ID,"S",.TMGDFN)
SMSSDN0 ;
  QUIT
  ;
SMSGTALL()  ;"SMS GET ALL 
  ;"Purpose: Get all SMS messages, 
  NEW DELFILE,OUT,%,HOUR
  DO NOW^%DTC
  SET HOUR=+$EXTRACT($PIECE(%,".",2),1,2)
  SET DELFILE=(HOUR>21) ;"IF TIME AFTER 10 pm DEL FILE
  DO SMSGET(DELFILE) ;
  QUIT 
  ;
SMSGET(DELFILE) ;
  ;"Purpose: to call shell script (custom) sms_download_msg, and get replies
  ;"Input: DELFILE --  OPTIONAL.  If 1, message file will be deleted from
  ;"        SMS service provider.  Note: because the server could be adding
  ;"        messages at any time, it seems most prudent only to do this
  ;"        in the middle of the night.
  NEW ID,DATE,TIME,MOBILENUM,TEXT SET (ID,DATE,TIME,MOBILENUM,TEXT)="?"
  NEW P SET P="temp"
  ;"NEW HOOKCMD SET HOOKCMD="sms_download_msg"  7/20/21
  NEW HOOKCMD SET HOOKCMD="python /opt/worldvista/EHR/bin/sms_download_msg.py"
  IF +$GET(DELFILE)=1 SET HOOKCMD=HOOKCMD_" -d"
  OPEN P:(COMMAND=HOOKCMD:readonly)::"pipe"
  USE P
  NEW LINEIN
  FOR  DO  QUIT:($ZEOF)
  . READ LINEIN
  . IF LINEIN["BRK" DO  QUIT
  . . NEW TEMPID SET TEMPID=ID
  . . NEW STORE
  . . DO STORGLOB(.ID,.DATE,.MOBILENUM,.TEXT)
  . . DO PARS1SMS(.STORE,.ID,.DATE,.TIME,.MOBILENUM,.TEXT)
  . . DO NSURSSMS(.STORE,TEMPID,"R")
  . IF LINEIN["ID:" SET ID=$$TRIM^XLFSTR($PIECE(LINEIN,"ID:",2)) QUIT
  . IF LINEIN["Date:" SET DATE=$$TRIM^XLFSTR($PIECE(LINEIN,"Date:",2)) QUIT
  . IF LINEIN["Time:" SET TIME=$$TRIM^XLFSTR($PIECE(LINEIN,"Time:",2)) QUIT
  . IF LINEIN["MobileNum:" SET MOBILENUM=$$TRIM^XLFSTR($PIECE(LINEIN,"MobileNum:",2)) QUIT
  . IF LINEIN["Text:" SET TEXT=$$TRIM^XLFSTR($PIECE(LINEIN,"Text:",2)) QUIT
  . QUIT
  CLOSE P
  USE $P
  QUIT
  ;
GETSMSID(DELFILE) ;"GET SMS MESSAGE ID OF MESSAGES BY FTP BATCH
  ;"Purpose: to call shell script (custom) sms_batch_status_msg, and get replies
  ;"Input: DELFILE --  OPTIONAL.  If 1, message file will be deleted from
  ;"        SMS service provider.  Note: because the server could be adding
  ;"        messages at any time, this should be done after giving the server
  ;"        10 minutes or so to process the messages.
  ;"NOTE: this doesn't give the *final* status SMS message, i.e. if it could be
  ;"   delivered or not.  It just returns if the message was processed by
  ;"   the SMS gateway, and gets the message ID of the sent message. 
  ;"NOTE: this is run by a task, scheduled in SCHSTCHK^TMGSMS01, which is
  ;"      called from SENDSMS^TMGSMS01 and SNDTSMS^TMGSMS01.  The goal is to
  ;"      wait 10 minutes before running this function.
  NEW ID,MOBILENUM SET (ID,MOBILNUM)=""
  NEW P SET P="temp"
  ;"NEW HOOKCMD SET HOOKCMD="sms_batch_id -f='"_$$COMFILE()_"'"  7/20/21
  NEW HOOKCMD SET HOOKCMD="python /opt/worldvista/EHR/bin/sms_batch_id.py -f='"_$$COMFILE()_"'"
  IF +$GET(DELFILE)=1 SET HOOKCMD=HOOKCMD_" -d"
  OPEN P:(COMMAND=HOOKCMD:readonly)::"pipe"
  USE P
  NEW LINEIN
  FOR  DO  QUIT:($ZEOF)
  . READ LINEIN
  . IF LINEIN["BRK" DO  QUIT
  . . NEW STORE
  . . IF ID'="" DO
  . . . SET STORE(ID,"NUMBER")=MOBILENUM
  . . . SET STORE(ID,"DT")=$$NOW^XLFDT
  . . . DO NSURSSMS(.STORE,ID,"CS")
  . . SET (ID,MOBILNUM)=""
  . IF LINEIN["ID:" SET ID=$$TRIM^XLFSTR($PIECE(LINEIN,"ID:",2)) QUIT
  . IF LINEIN["To:" SET MOBILENUM=$$TRIM^XLFSTR($PIECE(LINEIN,"To:",2)) QUIT
  . QUIT
  CLOSE P
  USE $P
  QUIT
  ;  
PARS1SMS(OUT,ID,DATE,TIME,MOBILENUM,TEXT)  ;
  ;"Purpose: Store one message in the OUTREF
  ;"Input: OUT -- PASS BY REFERENCE.  AN OUT PARAMETER.  Format:
  ;"         OUT(<ID>,"DT")=FMDateTime
  ;"         OUT(<ID>,"NUMBER")=telephone number
  ;"         OUT(<ID>,"MSG")=Message
  ;"       ID,DATE,TIME,MOBILENUM,TEXT - all passed by ref, and will be cleared.
  SET ID=$GET(ID)
  IF ID="" SET ID="INVALID"
  NEW X,Y,%DT SET X=DATE,%DT="" D ^%DT
  NEW DATETIME SET DATETIME=""
  IF +Y>-1 DO
  . SET TIME=$PIECE(TIME,":",1)_$PIECE(TIME,":",2)_$PIECE(TIME,":",3)
  . SET DATETIME=Y_"."_TIME
  . ;"Subtract 7 hrs because SMS is stamped from server in time zone ahead of Eastern Time zone
  . SET DATETIME=$$FMADD^XLFDT(DATETIME,0,-7) ;"0 DAYS, 7 HRS subtracted
  SET OUT(ID,"DT")=DATETIME
  SET OUT(ID,"NUMBER")=MOBILENUM
  SET OUT(ID,"MSG")=TEXT
  SET ID="",DATE="",TIME="",MOBILENUM="",TEXT=""
  QUIT
  ;
NSURSSMS(ARR,ID,STATUS,TMGDFN) ;"ENSURE STORAGE OF SMS MESSAGE
  ;"Purpose: To ensure that the passes SMS messages have been saved.
  ;"NOTE: There is a high likelihood that the same SMS messages will be 
  ;"      downloaded from the server multiple times, so it should be stored
  ;"      only ONCE
  ;"Input: ARR -- PASS BY REFERENCE.  Format
  ;"         ARR(<ID>,"DT")=FMDateTime
  ;"         ARR(<ID>,"NUMBER")=telephone number
  ;"         ARR(<ID>,"MSG")=Message
  ;"       ID -- the ID of the message to store
  ;"       STATUS -- should be "S" for sent, "R" for recieved, OR "P" for pending, or "CS" for confirm sent
  ;"       TMGDFN -- OPTIONAL.  If provided, used to track who message was sent to
  ;"Result: none
  NEW TMGFDA,TMGMSG,TMGIEN
  NEW MSG SET MSG=$GET(ARR(ID,"MSG"))
  SET ID=$GET(ID) IF ID="" DO  GOTO NSSDN
  . DO ALERTERR("No Msg ID value provided to NSURSSMS^TMGKERN5")
  IF ID'="",$$MSGEXIST(.ARR,ID) GOTO NSSDN
  NEW PHONENUM SET PHONENUM=$GET(ARR(ID,"NUMBER"))
  IF PHONENUM="" DO  GOTO NSSDN
  . DO ALERTERR("No Phone number provided to NSURSSMS^TMGKERN5. ID="_ID)
  SET TMGDFN=+$GET(TMGDFN)
  NEW IEN SET IEN=+$ORDER(^TMG(22724,"B",PHONENUM,0))
  IF IEN>0 GOTO NSS2
  ;"ADD RECORDS FOR PHONE NUMER
  SET TMGFDA(22724,"+1,",.01)=PHONENUM
  DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO NSSDN
  . NEW MSG SET MSG=$$GETERRST^TMGDEBU2(.TMGMSG)
  . DO ALERTERR("Problem storing into file 22724: "_MSG)
  SET IEN=+$GET(TMGIEN(1)) IF IEN'>0 DO  GOTO NSSDN
  . DO ALERTERR("Unable to recover IEN of record for phone number ["_PHONENUM_"] in NSURSSMS^TMGKERN5. ID="_ID)
NSS2  ;
  IF STATUS'="CS" GOTO NSS3
  ;" See if there is already a message with PENDING status
  ;"  If so, then use the current information to fill in the missing ID
  SET STATUS="S"
  NEW FOUND SET FOUND=0
  NEW DATE SET DATE=""
  FOR  SET DATE=$ORDER(^TMG(22724,IEN,1,"B",DATE),-1) QUIT:(+DATE'>0)!FOUND  DO
  . NEW SUBIEN SET SUBIEN=""
  . FOR  SET SUBIEN=$ORDER(^TMG(22724,IEN,1,"B",DATE,SUBIEN)) QUIT:(+SUBIEN'>0)!FOUND  DO
  . . NEW CURSTATUS SET CURSTATUS=$PIECE($GET(^TMG(22724,IEN,1,SUBIEN,0)),"^",3)
  . . IF CURSTATUS'="P" QUIT
  . . SET FOUND=1
  . . ;"SET $PIECE(^TMG(22724,IEN,1,SUBIEN,0),"^",3)="S" 
  . . KILL TMGFDA,TMGIEN,TMGMSG
  . . NEW IENS SET IENS=SUBIEN_","_IEN_","
  . . SET TMGFDA(22724.01,IENS,.03)=STATUS 
  . . SET TMGFDA(22724.01,IENS,.02)=ID
  . . DO FILE^DIE("","TMGFDA","TMGMSG")
  . . IF $DATA(TMGMSG("DIERR"))=0 QUIT
  . . NEW ERRMSG SET ERRMSG=$$GETERRST^TMGDEBU2(.TMGMSG)
  . . DO ALERTERR(ERRMSG,"Error storing SMS message.") ;  
  IF FOUND GOTO NSSDN
  ;"If not found, then create a record below, though it won't contain a MSG.
NSS3 ;
  ;"ADD SUBRECORD FOR ID
  KILL TMGFDA,TMGIEN,TMGMSG
  NEW IENS SET IENS="+1,"_IEN_","
  NEW DATE SET DATE=$GET(ARR(ID,"DT")) IF DATE="" DO  GOTO NSSDN
  . DO ALERTERR("Unable to determine Date/Time in NSURSSMS^TMGKERN5. ID="_ID)
  SET TMGFDA(22724.01,IENS,.01)=+DATE  ;"time of message
  SET TMGFDA(22724.01,IENS,.02)=ID
  SET TMGFDA(22724.01,IENS,.03)=STATUS 
  IF TMGDFN>0 SET TMGFDA(22724.01,IENS,.04)=TMGDFN 
  IF MSG'="" SET TMGFDA(22724.01,IENS,1)=MSG
  DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  SET IENS=+$GET(TMGIEN(1))_","_IEN
  IF $DATA(TMGMSG("DIERR")) DO  GOTO NSSDN
  . NEW ERRMSG SET ERRMSG=$$GETERRST^TMGDEBU2(.TMGMSG)
  . DO ALERTERR(ERRMSG,"Error storing SMS message.") ;
  ELSE  IF STATUS="R" DO
  . DO ALRTMSG("Message Received and Saved","",IENS)
NSSDN ;    
  QUIT
  ;  
MSGEXIST(REF,ID) ;"SMS MESSAGE EXISTS
  ;"Purpose: Determine if record already exists
  ;"Input: REF -- PASS BY NAME.  Format
  ;"         ARR(<ID>,"DT")=FMDateTime
  ;"         ARR(<ID>,"NUMBER")=telephone number
  ;"         ARR(<ID>,"MSG")=Message
  ;"Result: 1 if exists, 0 if doesn't exist.
  ;"Use C index and ID to see if message exists.
  SET ID=$GET(ID,"??")
  NEW RESULT SET RESULT=($DATA(^TMG(22724,"C",ID))>0)
  IF ID=0 SET RESULT=0
  QUIT RESULT
  ;
ALERTERR(ERRTEXT,AMSG,IENS) ;
  ;"Purpose: Set up alerts for error handling of SMS problems.
  ;"Input: ERRTEXT -- Text of error.
  ;"       AMSG -- Additional message, IF any.
  ;"       IENS -- IENS for file 22724.01 (OPTIONAL) 
  ;"Results: NONE:
  ;"Output: An alert is created. 
  ;"Restore original message
  SET ERRTEXT=$GET(ERRTEXT)
  IF (ERRTEXT["^")&(+ERRTEXT>0) SET ERRTEXT=$PIECE(ERRTEXT,"^",2,99)
  SET AMSG=$GET(AMSG)
  NEW TMGERROR SET TMGERROR="[SMS ERR]: "_ERRTEXT
  IF AMSG'="" SET TMGERROR=TMGERROR_"; SMS MESSAGE]: "_AMSG
  ;"MAKE AN ALERT WITH ERROR MESSAGE.
  ;"LATER I should put NEW PERSON IEN to receive alerts in the TMG SMS CREDENTIALS file or elsewhere
  NEW USRIEN SET USRIEN=168  ;"Kevin Toppenberg
  SET USRIEN=150  ;"Setting to Eddie for now
  DO SENDALERT(USRIEN,"Error during sending/receiving SMS message.",TMGERROR)
  QUIT
  ;
ALRTMSG(TEXT,AMSG,IENS) ;
  ;"Purpose: Set up alerts for message handling of SMS issues.
  ;"Input: TEXT -- Text of error.
  ;"       AMSG -- Additional message, IF any.
  ;"       IENS -- IENS for file 22724.01 (OPTIONAL)
  ;"Results: NONE:
  ;"Output: An alert is created. 
  ;"Restore original message
  SET TEXT=$GET(TEXT)
  IF (TEXT["^")&(+TEXT>0) SET TEXT=$PIECE(TEXT,"^",2,99)
  SET AMSG=$GET(AMSG)
  IF TEXT="" SET TEXT="(none)"
  NEW TMGMSG SET TMGMSG="[SMS MSG]: "_TEXT
  IF AMSG'="" SET TMGMSG=TMGMSG_"; SMS MESSAGE]: "_AMSG
  ;"MAKE AN ALERT WITH MESSAGE.
  ;"NEW USRIEN SET USRIEN=150  ;"Eddie Hagood
  ;"LATER I should put NEW PERSON IEN to receive alerts in the TMG SMS CREDENTIALS file or elsewhere
  NEW USRIEN SET USRIEN=168  ;"Kevin Toppenberg
  NEW NOTIFY SET NOTIFY=0
  IF TEXT["FYI: Sent" DO
  . IF NOTIFY DO SENDALERT(USRIEN,TEXT,TMGMSG,"MENU^TMGSMS05",.IENS)
  ELSE  DO
  . IF NOTIFY DO SENDALERT(USRIEN,"SMS message received.",TMGMSG,"HNDLMSG^TMGSMS03",.IENS)
  QUIT
  ;
SENDALERT(RECIPIENT,HEADING,MSG,HANDLER,IENS) ;"SEND ALERT
  ;"Input -- RECIPIENT -- IEN in NEW PERSON file
  ;"         HEADING -- The header message (show in CPRS)
  ;"         MSG -- The text to send
  ;"         HANDLER -- OPTIONAL.  Default is "HNDLERR^TMGSMS03"
  ;"         IENS -- IENS for file 22724.01 (OPTIONAL) 
  NEW NOW SET NOW=+$TRANSLATE($H,",",".")
  FOR  QUIT:($DATA(^TMG("TMP","TMGKERN5",$J,NOW))=0)  SET NOW=NOW+0.00000001
  SET HANDLER=$GET(HANDLER,"HNDLERR^TMGSMS03")
  SET MSG=$GET(MSG) IF MSG="" GOTO SADN
  SET RECIPIENT=+$GET(RECIPIENT) IF (RECIPIENT'>0) GOTO SADN
  SET IENS=$GET(IENS)
  NEW XQA,XQAARCH,XQADATA,XQAFLG,XQAGUID,XQAID,XQAOPT,XQAROU,XQASUPV,XQASURO,XQATEXT
  SET XQA(RECIPIENT)=""   
  SET XQADATA=$J_"^"_NOW_"^"_IENS
  SET XQAID="TMG"
  SET XQAROU=HANDLER
  SET XQAMSG=HEADING
  SET ^TMG("TMP","TMGKERN5",$J,NOW,"MSG")=MSG
  NEW TEMP SET TEMP=$$SETUP1^XQALERT  
SADN ;  
  QUIT  
  ;
STORGLOB(ID,DATE,MOBILENUM,TEXT)  ;"STORE RESPONSES IN A GLOBAL TO BE RETRIEVED LATER
  ;"Input -- ID, DATE, MOBILENUM, TEXT
  IF TEXT="" SET TEXT=" "
  IF TEXT'["FYI: Sent" SET ^TMG("TMP","SMS",DATE,ID,MOBILENUM,TEXT)=""
  QUIT
  ;"
SMSRPT    ;" Print report for received SMS messages
  ;"Input -- none
  ;"Output -- Report printed to s121
  ;"Result -- none  
  ;"IF '$DATA(^TMG("TMP","SMS")) QUIT
  NEW %ZIS,IOP
  SET IOP="S121-LAUGHLIN-LASER"
  DO ^%ZIS  ;"standard device call
  IF POP DO  QUIT
  . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output. Aborting.")
  use IO
  NEW X,Y DO NOW^%DTC
  WRITE !
  WRITE "************************************************************",!
  WRITE "                  Received Text Messages",!
  WRITE "                     " SET Y=X DO DD^%DT WRITE Y,!
  WRITE "************************************************************",!
  WRITE "                                            (From TMGKERN5.m)",!!
  NEW PTNAME,TMGDFN,IEN,SUBIEN,SENTDATE
  NEW DATE SET DATE=0
  FOR  SET DATE=$ORDER(^TMG("TMP","SMS",DATE)) QUIT:DATE'>0  DO
  . NEW ID SET ID=0
  . FOR  SET ID=$ORDER(^TMG("TMP","SMS",DATE,ID)) QUIT:ID'>0  DO
  . . NEW PHONE SET PHONE=0
  . . FOR  SET PHONE=$ORDER(^TMG("TMP","SMS",DATE,ID,PHONE)) QUIT:PHONE'>0  DO
  . . . SET IEN=$ORDER(^TMG(22724,"C",ID,0))
  . . . SET SUBIEN=+$ORDER(^TMG(22724,"C",ID,IEN,0))
  . . . SET SENTDATE=$PIECE($GET(^TMG(22724,IEN,1,SUBIEN,0)),"^",1)
  . . . SET SUBIEN=SUBIEN-1
  . . . SET TMGDFN=+$PIECE($GET(^TMG(22724,IEN,1,SUBIEN,0)),"^",4)
  . . . IF TMGDFN>0 DO
  . . . . SET PTNAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)_" ("_PHONE_")"
  . . . ELSE  DO
  . . . . SET PTNAME="** NAME NOT FOUND **  MOBILE NUMBER = "_PHONE
  . . . NEW MESSAGE SET MESSAGE=$ORDER(^TMG("TMP","SMS",DATE,ID,PHONE,""))
  . . . WRITE PTNAME,!," -> ",MESSAGE,!
  . . . WRITE "    On " SET Y=SENTDATE DO DD^%DT WRITE Y,!,!
  DO ^%ZISC  ;" Close the output device  
  KILL ^TMG("TMP","SMS")
  QUIT
  ;"
SMSRPC(TMGRESULT,PHONE,MSG) ; RPC ENTRY POINT FOR TMG SMS SEND LAB MSG
  SET PHONE=$TRANSLATE(PHONE,"(- ","")
  SET PHONE=+$GET(PHONE)
  IF PHONE'>0 GOTO RPCDN
  IF $LENGTH(PHONE)'=11 DO
  . SET PHONE=$$MKVALPHN^TMGSMS01(PHONE)
  NEW ARR,STORE
  NEW RESULT SET RESULT=$$SUHEADER^TMGSMS01(.ARR)
  IF +RESULT'>0 GOTO RPCDN  ;"SUHEADER makes it's own alert
  DO ADDLINE^TMGSMS01(.ARR,"csv:"_PHONE_"|"_MSG)
  SET STORE(PHONE)=0_"^"_MSG
  ;"Job this off for the sake of time
  NEW ZTIO,ZTRTN,ZTDTH,ZTSAVE,ZTDESC,ZTSYNC,ZTSK
  SET ZTRTN="JOBSEND^TMGKERN5"
  SET ZTIO=""
  DO NOW^%DTC
  SET ZTDTH=%
  SET ZTSAVE("*")=""
  DO ^%ZTLOAD IF '$DATA(ZTSK) DO SMSSEND(.ARR,.STORE,1)
RPCDN ;
  QUIT
  ;"
JOBSEND()
  DO SMSSEND^TMGKERN5(.ARR,.STORE,1)
  QUIT
  