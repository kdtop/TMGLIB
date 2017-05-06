TMGLOG01 ;TMG/kst/Message Log Storage ;1/30/15
         ;;1.0;TMG-LIB;**1**;1/30/15
 ;
 ;"TMG FUNCTIONS
 ;"I.e. functions that related to creating log file entry
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
 ;"ADDLOG(LOGNAME,LINE,WPARR,DT,USR) -- Add log to TMG LOGS file (22725)
 ; 
 ;"=======================================================================
 ;" API -- Private Functions.
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"Dependencies:
 ;"   TMGSMS*, TMGUSRI2, XLFDT, DIC, DTC
 ;"=======================================================================
 ;
ADDLOG(LOGNAME,LINE,WPARR,DT,USR)  ;"Add log to TMG LOGS file (22725)
  ;"Input: LOGNAME -- This is a broad topic to group all similar entries
  ;"                  E.g. 'SMS MESSAGES'
  ;"       LINE -- This can be used for single line log entries, or for
  ;"               further description when giving WP data
  ;"       WPARR -- PASS BY REFERENCE -- An array to store in WP field.  Format
  ;"               WPARR(#)=<text>
  ;"       DT -- OPTIONAL.  Default is NOW.  Should be FM format DT of the log entry
  ;"       USR -- OPTIONAL.  Default is DUZ.  IEN of user adding log entry.
  ;"Result: 1^OK, or -1^Message if error.        
  ;"Find LOGNAME.  If not found, then create new record
  NEW TMGDEBUG SET TMGDEBUG=0
  IF TMGDEBUG=1 DO
  . SET LOGNAME=$GET(^TMG("TMGLOG","LOGNAME"))
  . SET LINE=$GET(^TMG("TMGLOG","LINE"))
  . MERGE WPARR=^TMG("TMGLOG","WPARR")
  . SET DT=$GET(^TMG("TMGLOG","DT"))
  . SET USR=$GET(^TMG("TMGLOG","USR"))
  ELSE  DO
  . SET ^TMG("TMGLOG","LOGNAME")=$GET(LOGNAME)
  . SET ^TMG("TMGLOG","LINE")=$GET(LINE)
  . MERGE ^TMG("TMGLOG","WPARR")=WPARR
  . SET ^TMG("TMGLOG","DT")=$GET(DT)
  . SET ^TMG("TMGLOG","USR")=$GET(USR)
  NEW TMGFDA,TMGMSG,IENS,IEN
  NEW TMGRESULT SET TMGRESULT="1^OK" 
  SET LOGNAME=$GET(LOGNAME)
  IF LOGNAME="" DO  GOTO ALDN
  . SET TMGRESULT="-1^NO LOGNAME PROVIDED"
  SET IEN=+$ORDER(^TMG(22725,"B",LOGNAME,0))
  IF IEN'>0 DO
  . SET TMGFDA(22725,"+1,",.01)=LOGNAME
  . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  . IF $DATA(TMGMSG("DIERR")) DO
  . . SET TMGRESULT="-1^RECORD CANNOT BE ADDED: "_$$GETERRST^TMGDEBU2(.TMGMSG)
  . ELSE  SET IEN=TMGIEN(1)
  IF TMGRESULT["-1" GOTO ALDN
  ;"Add new record for DT
  KILL TMGFDA,TMGIEN,TMGMSG  
  SET IENS="+1,"_IEN_","  
  ;"DT CHECK
  SET DT=+$GET(DT) IF DT'>0 DO
  . NEW % DO NOW^%DTC
  . SET DT=%
  KILL TMGFDA
  SET TMGFDA(22725.01,IENS,.01)=DT
  ;"USER CHECK
  SET USR=+$GET(USR) IF USR'>0 SET USR=DUZ
  SET TMGFDA(22725.01,IENS,.02)=USR
  ;"LINE CHECK
  SET LINE=$GET(LINE)
  IF LINE'="" SET TMGFDA(22725.01,IENS,1)=LINE 
  ;"
  DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO ALDN
  . SET TMGRESULT="-1^RECORD CANNOT BE ADDED: "_$$GETERRST^TMGDEBU2(.TMGMSG)
  SET IENS=TMGIEN(1)_","_IEN_","
  ;"CHECK WPARR
  IF $DATA(WPARR) DO
  . KILL TMGMSG
  . DO WP^DIE(22725.01,IENS,10,"KA","WPARR","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO ALDN
  . SET TMGRESULT="-1^RECORD CANNOT BE ADDED: "_$$GETERRST^TMGDEBU2(.TMGMSG)  
ALDN  ;
  QUIT TMGRESULT
  ;
