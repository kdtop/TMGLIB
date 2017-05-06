TMGSMS03 ;TMG/kst/OS SMS Message -- Alert Handlers;12/23/14
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
 ;"HNDLERR() -- Handle SMS error
 ;"HNDLMSG() -- Handle SMS message
 ;
 ;"=======================================================================
 ;" API -- Private Functions.
 ;"=======================================================================
 ;"SHWSMSA(INFO) --Show parts of array -- as created by GETSMSA
 ;"GETSMSA(OUT,IENS) --  Recover SMS message information, parsed into an array
 ;"GETNUSRS(OUT,PHONENUM) -- GET ALL PATIENTS USING PHONE NUMBER
 ;"GETINFO(OUT,DATASTR) -- Recover message information, parsed into an array
 ;"GETTHRED(THREAD,PHONENUM,SDT,EDT) -- Get thread of conversation for phone number for date rage
 ;"SHWTHRED(THREAD) -- SHOW THREAD
 ;
 ;"=======================================================================
 ;"Dependencies
 ;"=======================================================================
 ;"NOTES: 
 ;"  TMGKERN5 -- interfaces with actual sending and receiving of messages
 ;"  TMGDEBU2 -- for fileman error messages.
 ;"  FM file# 22724 (TMG SMS MESSAGES) stores sent and received messages
 ;" 
 ;"=======================================================================
 ;
SHWSMSA(INFO) ;"Show parts of array -- as created by GETSMSA
  NEW LINE SET LINE=""
  IF $GET(INFO("S/R"))="R" SET LINE=LINE_"Received "
  ELSE  IF $GET(INFO("S/R"))="S" SET LINE=LINE_"Sent "
  NEW TMGDT SET TMGDT=$GET(INFO("FMDT")) IF TMGDT'="" DO
  . SET LINE=LINE_$$DOW^XLFDT(TMGDT)_", "
  . SET LINE=LINE_$$FMTE^XLFDT(TMGDT,"2P")_" "
  WRITE LINE,! SET LINE=""
  WRITE "] ",$GET(INFO("SMS MSG")),!,!  
  QUIT
  ;
GETSMSA(OUT,IENS) ;
  ;"Purpose: Recover SMS message information, parsed into an array
  ;"Input: OUT -- PASS BY REFERENCE.  An OUT PARAMETER.  Format:
  ;"            OUT("IENS")=IENS
  ;"            OUT("PHONE")=PHONE number
  ;"            OUT("PHONE IEN")=IEN in 22724
  ;"            OUT("FMDT")=FM date/time of SMS message
  ;"            OUT("ID")=SMS message ID
  ;"            OUT("S/R")=S for sent, R for received
  ;"            OUT("DFN")=DFN of patient for message, if known
  ;"            OUT("SMS MSG")=SMS message
  ;"        IENS -- IENS FOR FILE 22724.01 (OPTIONAL, but returns ~empty if not provided)
  ;"Result: none  
  SET OUT("IENS")=IENS
  NEW IEN SET IEN=+$PIECE(IENS,",",2)
  SET OUT("PHONE IEN")=IEN
  IF IEN>0 DO
  . NEW PHONE SET PHONE=$PIECE($GET(^TMG(22724,IEN,0)),"^",1)
  . SET OUT("PHONE")=PHONE
  . NEW SUBIEN SET SUBIEN=+$PIECE(IENS,",",1)
  . NEW ADFN SET ADFN=0
  . IF SUBIEN>0 DO
  . . NEW ZN SET ZN=$GET(^TMG(22724,IEN,1,SUBIEN,0))
  . . IF ZN'="" DO
  . . . SET OUT("FMDT")=$PIECE(ZN,"^",1)
  . . . SET OUT("ID")=$PIECE(ZN,"^",2)
  . . . SET OUT("S/R")=$PIECE(ZN,"^",3)
  . . . SET ADFN=$PIECE(ZN,"^",4),OUT("DFN")=ADFN
  . . SET ZN=$GET(^TMG(22724,IEN,1,SUBIEN,1))
  . . IF ZN'="" DO
  . . . SET OUT("SMS MSG")=$PIECE(ZN,"^",1)
  QUIT
  ;
GETNUSRS(OUT,PHONENUM) ;"GET ALL PATIENTS USING PHONE NUMBER
  ;"INPUT: OUT -- PASS BY REFERENCE.  AN OUT PARAMETER.  format:
  ;"            OUT("PHONE NUM USERS",DFN)=""  -- all patients linked to phone number 
  ;"       PHONENUMBER -- THE 11 digit phone number for thread
  ;"Result: none
  SET PHONENUM=+$GET(PHONENUM)
  SET OUT("PHONE NUM USERS")=""
  NEW IEN SET IEN=$ORDER(^TMG(22724,"B",PHONENUM,0)) IF IEN'>0 GOTO GNURDN
  NEW SUBIEN SET SUBIEN=0 
  FOR  SET SUBIEN=$ORDER(^TMG(22724,IEN,1,SUBIEN)) QUIT:+SUBIEN'>0  DO
  . NEW ADFN SET ADFN=+$PIECE($GET(^TMG(22724,IEN,1,SUBIEN,0)),"^",4)
  . IF ADFN>0 SET OUT("PHONE NUM USERS",ADFN)=""  
  NEW NAMES SET NAMES=""
  NEW DFN SET DFN=0
  FOR  SET DFN=$ORDER(THREAD("PHONE NUM USERS",DFN)) QUIT:+DFN'>0  DO
  . NEW ONENAME SET ONENAME=$PIECE($GET(^DPT(DFN,0)),"^",1)
  . SET NAMES=NAMES_"<"_ONENAME_"> "
  SET NAMES=NAMES_"("_PHONENUM_")"
  SET OUT("FROM")=NAMES 
GNURDN ;
  QUIT
  ;
GETINFO(OUT,DATASTR) ;
  ;"Purpose: Recover message information, parsed into an array
  ;"Input: OUT -- PASS BY REFERENCE.  An OUT PARAMETER.  Format:
  ;"            OUT("JOB")=$J of alert creator
  ;"            OUT("TIME")=$H of alert creation.
  ;"            OUT("IENS")=IENS
  ;"            OUT("ALERT MSG")=alert message
  ;"            OUT("PHONE")=PHONE number
  ;"            OUT("PHONE IEN")=IEN in 22724
  ;"            OUT("FMDT")=FM date/time of SMS message
  ;"            OUT("ID")=SMS message ID
  ;"            OUT("S/R")=S for sent, R for received
  ;"            OUT("DFN")=DFN of patient for message, if known
  ;"            OUT("SMS MSG")=SMS message
  ;"            OUT("PHONE NUM USERS",DFN)=""  -- all patients linked to phone number 
  ;"            OUT("FROM")=<Name> <Name> <Name> (PhoneNumber) 
  ;"       DATASTR -- $JOB^FMDT^IENS in 22724.01 (optional)
  ;"Result: none  
  KILL OUT
  SET DATASTR=$GET(DATASTR)
  NEW JOBN SET JOBN=+$PIECE(DATASTR,"^",1) SET OUT("JOB")=JOBN
  NEW TIME SET TIME=$PIECE(DATASTR,"^",2) SET OUT("TIME")=TIME
  NEW IENS SET IENS=$PIECE(DATASTR,"^",3) SET OUT("IENS")=IENS
  SET OUT("ALERT MSG")=$GET(^TMG("TMP","TMGKERN5",JOBN,TIME,"MSG"))
  DO GETSMSA(.OUT,IENS) ;
  NEW PHONENUM SET PHONENUM=$GET(OUT("PHONE"))
  DO GETNUSRS(.OUT,PHONENUM)
GIDN ;  
  QUIT
  ;
GETTHRED(THREAD,PHONENUM,SDT,EDT) ;
  ;"Purpose: Get thread of conversation for phone number for date rage
  ;"INPUT: THREAD -- PASS BY REFERENCE.  AN OUT PARAMETER.  format:
  ;"         THREAD(#)=Array for 1 message.  See format from GETINFO
  ;"         THREAD("PHONE NUM USERS",DFN)=""
  ;"       PHONENUMBER -- THE 11 digit phone number for thread
  ;"       SDT -- STARTING FM date/time to retrieve.  OPTIONAL.  Default=0
  ;"       EDT -- ENDING FM date/time to retrieve.  OPTIONAL.  Default=9999999
  ;"Result: none
  SET PHONENUM=$GET(PHONENUM,"?") 
  SET SDT=+$GET(SDT)
  SET EDT=+$GET(EDT) IF EDT'>0 SET EDT=9999999  
  NEW DT SET DT=SDT IF DT>0 SET DT=DT-0.000001
  NEW IDX SET IDX=1
  NEW IEN SET IEN=+$ORDER(^TMG(22724,"B",PHONENUM,0))
  FOR  SET DT=$ORDER(^TMG(22724,IEN,1,"B",DT)) QUIT:(+DT'>0)!(DT>EDT)  DO
  . NEW SUBIEN SET SUBIEN=0
  . FOR  SET SUBIEN=$ORDER(^TMG(22724,IEN,1,"B",DT,SUBIEN)) QUIT:+SUBIEN'>0  DO
  . . NEW IENS SET IENS=SUBIEN_","_IEN_","
  . . NEW INFO DO GETSMSA(.INFO,IENS)
  . . MERGE THREAD(IDX)=INFO 
  . . SET THREAD("B",DT_" ",IDX)=""
  . . SET IDX=IDX+1
  DO GETNUSRS(.THREAD,PHONENUM)  
  QUIT
  ;
SHWTHRED(THREAD) ;"SHOW THREAD
  ;"Input: THREAD -- PASS BY REFERENCE.  Array as made by GETTHRED
  ;"NEW NAMES SET NAMES=""   
  ;"NEW DFN SET DFN=0
  ;"FOR  SET DFN=$ORDER(THREAD("PHONE NUM USERS",DFN)) QUIT:+DFN'>0  DO
  ;". NEW ONENAME SET ONENAME=$PIECE($GET(^DPT(DFN,0)),"^",1)
  ;". SET NAMES=NAMES_"<"_ONENAME_"> "
  ;"WRITE NAMES,!
  WRITE !
  WRITE "***************************",!
  WRITE "*   SMS Message Thread    *",!
  WRITE "* (Most recent at bottom) *",!
  WRITE "***************************",!
  WRITE "TO/FROM: ",$GET(THREAD("FROM")),!
  WRITE "-------------------",!
  NEW ADT SET ADT=0
  FOR  SET ADT=$ORDER(THREAD("B",ADT)) QUIT:ADT=""  DO
  . NEW IDX SET IDX=0
  . FOR  SET IDX=$ORDER(THREAD("B",ADT,IDX)) QUIT:+IDX'>0  DO
  . . NEW INFO MERGE INFO=THREAD(IDX)
  . . DO SHWSMSA(.INFO) 
  QUIT
  ;
HNDLERR()  ;"
  ;"Purpose: Handle SMS error
  ;"Input: Globally scoped variable: XQADATA will hold $J^$H^IENSfrom22724.01
  ;"       ^TMG("TMP","TMGKERN5",$J,$H,"MSG") holds variable table at message
  NEW DATASTR SET DATASTR=$GET(XQADATA)
  NEW INFO DO GETINFO(.INFO,DATASTR) 
  WRITE !,!,"IMPLEMENT HANDLER IN HNDLERR^TMGSMS03",!,"  ->",$GET(INFO("ALERT MSG")),! 
  DO PRESS2GO^TMGUSRI2
  ;"to do -- clean up ^TMG("TMP","TMGKERN5",$J,$H,"MSG")
  QUIT
  ;
HNDLMSG() ;
  ;"Purpose: Handle SMS message
  ;"Input: Globally scoped variable: XQADATA will hold $J^$H^IENSfrom22724.01
  ;"       ^TMG("TMP","TMGKERN5",$J,$H,"MSG") holds variable table at message
  NEW DATASTR SET DATASTR=$GET(XQADATA)
  NEW INFO DO GETINFO(.INFO,DATASTR)
  WRITE !,!,!,$GET(INFO("ALERT MSG")),!
  NEW PHONENUM SET PHONENUM=$GET(INFO("PHONE"),"?")
  IF (PHONENUM'="?")&(PHONENUM'=0) DO 
  . NEW THREAD DO GETTHRED(.THREAD,PHONENUM) 
  . DO SHWTHRED(.THREAD)
  DO PRESS2GO^TMGUSRI2 WRITE !
  NEW % SET %=1
  WRITE "Delete *alert* data (not SMS message itself)" DO YN^DICN WRITE !
  IF %=1 DO
  . KILL ^TMG("TMP","TMGKERN5",$J,$H,"MSG")  ;"<-- CHECK, IS $J,$H RIGHT HERE??
  QUIT
  ;
