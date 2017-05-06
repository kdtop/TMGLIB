TMGSMS04 ;TMG/kst/SMS Messages;1/4/15
         ;;1.0;TMG-LIB;**1**;1/4/15
 ;
 ;"TMG FUNCTIONS
 ;"I.e. functions that related to processing SMS messages
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
 ;"GETFSTAT() -- Process FINAL status report. <--- SUITABLE ENTRY POINT FOR TASKMAN
 ; 
 ;"=======================================================================
 ;" API -- Private Functions.
 ;"=======================================================================
 ;"GETFPATH(NAME) -- GET FILE PATH FROM CREDENTIALS FILE 
 ;"PROCESS1(ARR) -- PROCESS ONE STATUS ARRAY
 ;
 ;"=======================================================================
 ;"Dependencies
 ;"=======================================================================
 ;"NOTES: 
 ;"  TMGKERN5 -- interfaces with actual sending and receiving of messages
 ;"  TMGIOUTL
 ;"  %ZISH
 ;"  DIE
 ;"  TMGDEBU2 -- for fileman error messages.
 ;"  FM file# 22724 (TMG SMS MESSAGES) stores sent and received messages
 ;" 
 ;"=======================================================================
 ;
GETFPATH(NAME) ;"GET FILE PATH FROM CREDENTIALS FILE 
  ;"Purpose: get SMS credentials from file 22724.1 (TMG SMS CREDENTIALS)
  ;"Input: NAME -- the value of the .01 field record to use.  
  ;"Result: 1^FPathName if OK, or -1^Message if error. 
  SET NAME=$GET(NAME,"??")
  NEW RESULT SET RESULT="-1^Default"
  NEW IEN SET IEN=+$ORDER(^TMG(22724.1,"B",NAME,0))
  IF $DATA(^TMG(22724.1,IEN,0))'>0 DO  GOTO GFPDN
  . SET RESULT="-1^Record not found in 22724.1 for "_NAME 
  NEW FNAME SET FNAME=$PIECE($GET(^TMG(22724.1,IEN,1)),"^",1)
  IF FNAME="" DO  GOTO GFPDN
  . SET RESULT="-1^Filename not found in field 1 in record in 22724.1 for IEN="_IEN
  ELSE  SET RESULT="1^"_FNAME
GFPDN ;  
  QUIT RESULT
  ;  
GETFSTAT() ;"Process FINAL status report. <--- SUITABLE ENTRY POINT FOR TASKMAN
  ;"This function is to process the CSV file from Clickatell that provides the
  ;"  ultimate status of the SMS messages sent, over a period of time.  It will
  ;"  give information such as 'Delivered to Gateway', 'Received by Recipient',
  ;"  'Routing Error', etc.
  ;"This report file can only be obtained from the Clickatell website.  So I will
  ;"  use an AutoHotKey script to automatically download the file periodically,
  ;"  save it in a common location, and then parse and process it here.
  DO MSGLOG^TMGSMS02("RUNNING GETFSTAT^TMGSMS04")
  NEW TMGRESULT
  SET TMGRESULT=$$GETFPATH("TMG") ;"<--- hard coded for FP of Greeneville.  Change if needed
  NEW RESULTP2 SET RESULTP2=$PIECE(TMGRESULT,"^",2)
  IF +TMGRESULT'>0 DO  GOTO GFLSDN
  . DO ALERTERR^TMGKERN5(RESULTP2)  
  NEW FPATH SET FPATH=$$MKTRALDV^TMGIOUTL(RESULTP2,"/")
  NEW FILESARR
  NEW SRCHNAME SET SRCHNAME="*Report_*"
  NEW SRCHSPEC SET SRCHSPEC(SRCHNAME)=""
  SET TMGRESULT=$$LIST^%ZISH(FPATH,"SRCHSPEC","FILESARR")
  IF +TMGRESULT'=1 DO  GOTO GFLSDN
  . NEW Y SET Y=$$NOW^XLFDT DO DD^%DT
  . NEW MSG SET MSG=Y_" Problems searching HFS directory ('"_FPATH_"') for 'Report_*' in GETFSTAT^TMGSMS04."
  . DO ALERTERR^TMGKERN5(MSG)
  NEW FILEFOUND SET FILEFOUND=0 
  NEW FNAME SET FNAME=""
  FOR  SET FNAME=$ORDER(FILESARR(FNAME)) QUIT:FNAME=""  DO
  . NEW L SET L=$LENGTH(FNAME)-3
  . NEW EXTN SET EXTN=$$LOW^XLFSTR($EXTRACT(FNAME,L+1,L+3))
  . IF EXTN'="csv" QUIT
  . SET FILEFOUND=1
  . NEW FPNAME SET FPNAME=FPATH_FNAME
  . NEW DATA SET TMGRESULT=$$LCSV2ARR^TMGIOUT4(FPNAME,"DATA")
  . IF +TMGRESULT'>0 DO  QUIT
  . . DO ALERTERR^TMGKERN5($PIECE(TMGRESULT,"^",2))    
  . NEW IDX SET IDX=""
  . NEW HADERR SET HADERR=0
  . FOR  SET IDX=$ORDER(DATA(IDX)) QUIT:+IDX'>0  DO
  . . NEW TEMP MERGE TEMP=DATA(IDX)
  . . SET TMGRESULT=$$PROCESS1(.TEMP)
  . . IF +TMGRESULT'>0 DO  QUIT
  . . . SET HADDERR=1
  . . . NEW MSG SET MSG="Processing file: '"_FPNAME_"'. "_$PIECE(TMGRESULT,"^",2)
  . . . DO ALERTERR^TMGKERN5(MSG)
  . IF HADERR=0 DO
  . . SET TMGRESULT=$$DELFILE^TMGIOUTL(FPNAME)  ;"1=SUCCESS, 0=FAILURE
  . . IF TMGRESULT'=0 QUIT
  . . NEW MSG SET MSG="Problems deleting HFS file '"_FPNAME_"'" 
  . . DO ALERTERR^TMGKERN5(MSG)
  IF FILEFOUND=0 DO
  . NEW MSG SET MSG="Unable to file "_SRCHNAME_".csv at location: "_FPATH
  . DO ALERTERR^TMGKERN5(MSG)
GFLSDN ;
  QUIT
  ;
PROCESS1(ARR) ;"PROCESS ONE STATUS ARRAY
  ;"Input: ARR -- PASS BY REFERENCE.  Format
  ;"         ARR(1)= Origination Time
  ;"         ARR(2)= Delivery Date
  ;"         ARR(3)= Last Change
  ;"         ARR(4)= Destination Number
  ;"         ARR(5)= API Sent From
  ;"         ARR(6)= Message ID
  ;"         ARR(7)= Cost
  ;"         ARR(8)= Status Text
  ;"         ARR(9)= Message Data
  ;"         ARR(10) = TelcoName
  ;"         ARR(11) = CountryName
  ;"         ARR(12) = Source Address
  ;"         ARR(13) = ClientMsgId
  ;"Results: 1 if OK, or -1^message if error
  NEW TMGRESULT SET TMGRESULT=1
  NEW MSGID SET MSGID=$TRANSLATE($GET(ARR(6)),"""","") IF MSGID="" GOTO P1DN  ;"Don't trigger error
  NEW STATUS SET STATUS=$TRANSLATE($GET(ARR(8)),"""","") IF STATUS="" GOTO P1DN ;"Don't trigger error  
  NEW IEN SET IEN=+$ORDER(^TMG(22724,"C",MSGID,0)) IF IEN'>0 GOTO P1DN ;"Don't trigger error
  NEW SUBIEN SET SUBIEN=+$ORDER(^TMG(22724,"C",MSGID,IEN,0)) IF SUBIEN'>0 GOTO P1DN ;"Don't trigger error
  NEW INTSTATUS
  IF STATUS="Delivered to gateway" SET INTSTATUS="D"
  ELSE  IF STATUS="Delivered to gateway" SET INTSTATUS="D"
  ELSE  IF STATUS="Received by recipient" SET INTSTATUS="R"
  ELSE  IF STATUS="Routing error" SET INTSTATUS="E"
  ELSE  SET INTSTATUS="O"
  NEW ZN SET ZN=$GET(^TMG(22724,IEN,1,SUBIEN,0))
  IF $PIECE(ZN,"^",5)'=INTSTATUS DO 
  . NEW IENS SET IENS=SUBIEN_","_IEN_","
  . NEW TMGFDA,TMGMSG
  . SET TMGFDA(22724.01,IENS,.05)=INTSTATUS
  . DO FILE^DIE("","TMGFDA","TMGMSG")
  . IF $DATA(TMGMSG("DIERR")) DO  
  . . NEW ERRMSG,MSG SET ERRMSG=$$GETERRST^TMGDEBU2(.TMGMSG)
  . . SET MSG="Error setting SMS message final status to "_STATUS_" ("_INTSTATUS_")"
  . . DO ALERTERR^TMGKERN5(ERRMSG,MSG)
  IF INTSTATUS="E" DO
  . NEW PHONENUM SET PHONENUM=$TRANSLATE($GET(ARR(4)),"""","") QUIT:PHONENUM=""
  . NEW PATARR DO GETNUSRS^TMGSMS03(.PATARR,PHONENUM)
  . NEW ADFN SET ADFN=0
  . FOR  SET ADFN=+$ORDER(PATARR("PHONE NUM USERS",ADFN)) QUIT:(+ADFN'>0)!(TMGRESULT'=1)  DO
  . . NEW ERRARR
  . . DO EXCLPHON^TMGSMS02(ADFN,PHONENUM,.ERRARR)
  . . IF $DATA(ERRARR)=0 QUIT  ;"Success
  . . NEW MSG SET MSG=$GET(ERRARR(ADFN)) QUIT:MSG=""
  . . IF MSG["ALREADY EXCLUDED" QUIT  ;"not really an error
  . . SET TMGRESULT="-1^"_MSG
P1DN ;  
  QUIT TMGRESULT
  ;
