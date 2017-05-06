TMGMSG01 ;TMG/kst/OS Network Messager ;4/21/15
         ;;1.0;TMG-LIB;**1**;4/21/15
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
SENDMSG(PATH,TO,FROM,MESSAGE)  ;"
   NEW DEV,TMGRESULT
   SET PATH=$GET(PATH)
   IF PATH="" SET PATH="/mnt/WinPublic/NetworkMessenger/"
   SET TMGRESULT="1^SUCCESS"
   SET DEV=PATH_TO_"-"_FROM_".mgr"
   OPEN DEV::2
   IF '$TEST SET TMGRESULT="-1^Failure to open "_DEV_" for writing" 
   USE DEV
   WRITE MESSAGE,!
   CLOSE DEV
   QUIT TMGRESULT
   ;"
SEND
   NEW TO,FROM,MESSAGE
   READ "WHO IS THIS MESSAGE FOR? ",TO:$GET(DTIME,3600),!
   READ "WHO IS THIS MESSAGE FROM? ",FROM:$GET(DTIME,3600),!
   READ "WHAT WOULD YOU LIKE THE MESSAGE TO SAY? ",MESSAGE:$GET(DTIME,3600),!
   W $$SENDMSG("",.TO,.FROM,.MESSAGE)
   QUIT 
   ;"

   
