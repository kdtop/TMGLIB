TMGSMS02 ;TMG/kst/OS SMS Message -- utilities ;12/23/14, 3/24/21
         ;;1.0;TMG-LIB;**1**;12/23/14
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
 ;"VIEWPHON -- For a given patient, display phone numbers
 ;"OPTOUT(MODE) -- interact with user to opt-out (exclude) a phone number
 ;"ASKOPTOT  ;"ASK OPT OUT
 ;"OPTOTDFN(TMGDFN) ;"OPT-OUT one patient (TMGDFN)
 ; 
 ;"=======================================================================
 ;" API -- Private Functions.
 ;"=======================================================================
 ;"EXCLPHON(TMGDFN,PHONENUM,ERRARRAY) -- add a phone number to the SMS exclusion list
 ;"ISEXCLED(TMGDFN,PHONENUM) --IS NUMBER EXCLUDED ALREADY?
 ;"FMTPHONE(NUM) -- Format an 11 digit phone number with characters
 ;
 ;"=======================================================================
 ;"Dependencies
 ;"=======================================================================
 ;"NOTES: 
 ;"  TMGSMS*
 ;"  TMGKERN5 -- interfaces with actual sending and receiving of messages
 ;"  TMGDEBU2 -- for fileman error messages.
 ;"  FM file# 22724 (TMG SMS MESSAGES) stores sent and received messages
 ;" 
 ;"=======================================================================
FMTPHONE(NUM) ;"Format an 11 digit phone number with characters
  NEW RESULT SET RESULT="("_$E(NUM,2,4)_") "_$E(NUM,5,7)_"-"_$E(NUM,8,11) 
  QUIT RESULT
  ;
VIEWPHON ;
  ;"Purpose: For a given patient, display
  NEW X,Y,DIC SET DIC(0)="MAEQ",DIC=2
  DO ^DIC WRITE !
  NEW TMGDFN SET TMGDFN=+Y
  IF TMGDFN'>0 WRITE "ABORTING." QUIT
  NEW PHONENUMS,ERRARRAY
  DO GETPHONS^TMGSMS01(.TMGDFN,.PHONENUMS,.ERRARRAY)
  IF $DATA(ERRARRAY) WRITE "ERRORS:",! DO ZWRITE^TMGZWR("ERRARRAY")
  IF $DATA(PHONENUMS) WRITE "PHONE NUMBERS:",!
  NEW ONENUM SET ONENUM=""
  FOR  SET ONENUM=$ORDER(PHONENUMS(TMGDFN,ONENUM)) QUIT:+ONENUM'>0  DO
  . NEW EXCLUDED SET EXCLUDED=""
  . IF $$ISEXCLED(TMGDFN,ONENUM) SET EXCLUDED=" (EXCLUDED)"
  . WRITE "  ",ONENUM," <--> ",$$FMTPHONE(ONENUM),EXCLUDED,!
  DO PRESS2GO^TMGUSRI2
  QUIT
  ;
ISEXCLED(TMGDFN,PHONENUM) ;"IS EXCLUDED ALREADY?
  SET TMGDFN=+$GET(TMGDFN)
  SET PHONENUM=$GET(PHONENUM) IF PHONENUM="" SET PHONENUM=0
  NEW RESULT SET RESULT=($DATA(^DPT(TMGDFN,"TMGSMS","B",PHONENUM))>0)
  QUIT RESULT
  ;
EXCLPHON(TMGDFN,PHONENUM,ERRARRAY) ;
  ;"Purpose: To add a phone number to the SMS exclusion list
  ;"Input:TMGDFN - Patient's DFN
  ;"      PHONENUM - Number to exclude, should be 10 digits and unformatted
  ;"      ERRARRAY - PASS BY REFERENCE.  Returns errors. Format: 
  ;"          ERRARRAY(DFN)=message
  ;"Result: none
  KILL ERRARRAY
  IF $$ISEXCLED(TMGDFN,PHONENUM) DO  GOTO EPDN
  . SET ERRARRAY(TMGDFN)="NUMBER "_PHONENUM_" ALREADY EXCLUDED" 
  NEW TMGFDA,TMGIENS,TMGMSG,%
  DO NOW^%DTC
  SET TMGIENS="+1,"_TMGDFN_","
  SET TMGFDA(2.022705,TMGIENS,.01)=PHONENUM
  SET TMGFDA(2.022705,TMGIENS,1)="`"_DUZ
  SET TMGFDA(2.022705,TMGIENS,2)=%
  DO UPDATE^DIE("E","TMGFDA","TMGIENS","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO
  . SET ERRARRAY(TMGDFN)=$$GETERRST^TMGDEBU2(.TMGMSG)
EPDN  ;
  QUIT
  ;  
OPTOUT(MODE) ;
  ;"Purpose: for a given date, allow user to select phone numbers to exclude
  ;"Input: MODE:  if 1 then user is shown Patient Name and phone number
  ;"              if 2 then user is shown Message ID, Phone Number, Patient Name
  ;"              Default value is 2
  ;"NOTE: Phone numbers that are already opted-out (excluded) are NOT shown.
  ;"Result: none
  SET MODE=+$GET(MODE) IF MODE'>0 SET MODE=2
  NEW %DT,X,Y SET %DT="AEP" 
  SET %DT("A")="Enter DATE patient seen: "
  DO ^%DT WRITE Y 
  IF Y'>0 GOTO OODN  
  NEW ERRARRAY,SELLIST
  IF MODE=1 DO
  . NEW TMGDT SET TMGDT=Y
  . NEW OUT DO GETDTPTL^TMGSMS01(.OUT,TMGDT,"*") ;"returns OUT(DFN)=SubIEN (file 22723.01)
  . NEW TMGDFN SET TMGDFN=0
  . FOR  SET TMGDFN=$ORDER(OUT(TMGDFN)) QUIT:+TMGDFN'>0  DO
  . . NEW PHONENUMS
  . . NEW PTNAME SET PTNAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
  . . DO GETPHONS^TMGSMS01(TMGDFN,.PHONENUMS,.ERRARRAY) ;"returns -- PHONENUMS(DFN,<PHONE NUMBER>)="",  ERRARRAY(DFN)=<MESSAGE>
  . . NEW ONENUM SET ONENUM=""
  . . FOR  SET ONENUM=$ORDER(PHONENUMS(TMGDFN,ONENUM)) QUIT:+ONENUM'>0  DO
  . . . NEW EXOPEN,EXCLOSE
  . . . IF $$ISEXCLED(TMGDFN,ONENUM) SET EXOPEN="(",EXCLOSE=")"
  . . . ELSE  SET EXOPEN=" ",EXCLOSE=" "
  . . . NEW LINE SET LINE=PTNAME_" --"_EXOPEN_ONENUM_EXCLOSE_"--"_EXOPEN_$$FMTPHONE(ONENUM)_EXCLOSE
  . . . SET SELLIST(LINE)=TMGDFN_"^"_ONENUM
  IF MODE=2 DO  
  . NEW ENDDATE SET ENDDATE=Y+1
  . NEW DATE SET DATE=Y-0.000001
  . FOR  SET DATE=$ORDER(^TMG(22724,"D",DATE)) QUIT:(+DATE'>0)!(DATE'<ENDDATE)  DO
  . . SET MSGIEN=0
  . . FOR  SET MSGIEN=$ORDER(^TMG(22724,"D",DATE,MSGIEN)) QUIT:+MSGIEN'>0  DO
  . . . NEW TMGDFN SET TMGDFN=+$PIECE($GET(^TMG(22724,MSGIEN,1,1,0)),"^",4)
  . . . NEW ONENUM SET ONENUM=$PIECE($GET(^TMG(22724,MSGIEN,0)),"^",1)
  . . . NEW EXOPEN,EXCLOSE
  . . . IF $$ISEXCLED(TMGDFN,ONENUM) SET EXOPEN="(",EXCLOSE=")"
  . . . ELSE  SET EXOPEN=" ",EXCLOSE=" "
  . . . NEW PTNAME SET PTNAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
  . . . NEW MSGID SET MSGID=$PIECE($GET(^TMG(22724,MSGIEN,1,1,0)),"^",2)
  . . . NEW LINE SET LINE=MSGID_" --"_EXOPEN_ONENUM_EXCLOSE_"-- "_PTNAME
  . . . SET SELLIST(LINE)=TMGDFN_"^"_ONENUM  
  IF $DATA(ERRARRAY) WRITE "ERRORS:",! DO ZWRITE^TMGZWR("ERRARRAY") GOTO OODN
  NEW BLOCKLST
  IF $DATA(SELLIST) DO
  . DO SELECTOR^TMGUSRI3("SELLIST","BLOCKLST","Select numbers to exclude. <ESC><ESC> when done.")
  NEW ABORT SET ABORT=0
  NEW LINE SET LINE=""
  FOR  SET LINE=$ORDER(BLOCKLST(LINE)) QUIT:(LINE="")!(ABORT)  DO
  . NEW % SET %=2
  . WRITE "BLOCK ",LINE DO YN^DICN WRITE !
  . SET ABORT=(%=-1) QUIT:(%'=1)
  . SET TMGDFN=$PIECE(BLOCKLST(LINE),"^",1) IF +TMGDFN'>0 WRITE "??",! QUIT
  . NEW PHONENUM SET PHONENUM=$PIECE(BLOCKLST(LINE),"^",2) IF PHONENUM="" WRITE "??" QUIT
  . DO EXCLPHON(TMGDFN,PHONENUM,.ERRARRAY) ;
  IF $DATA(ERRARRAY) WRITE "ERRORS:",! DO ZWRITE^TMGZWR("ERRARRAY") GOTO OODN
OODN ;
  QUIT
  ;
ASKOPTOT  ;"ASK OPT OUT
  NEW X,Y,DIC SET DIC=2,DIC(0)="MAEQ"
  DO ^DIC WRITE !
  IF +Y>0 DO OPTOTDFN(+Y)
  QUIT
  ;
OPTOTDFN(TMGDFN) ;"OPT-OUT one patient (DFN)
  ;"Input: TMGDFN -- PATIENT IEN to exclude
  ;"NOTE: Phone numbers that are already opted-out (excluded) are NOT shown.
  ;"Result: none
  NEW SELLIST,ERRARRAY
  NEW PHONENUMS
  NEW PTNAME SET PTNAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
  DO GETPHONS^TMGSMS01(TMGDFN,.PHONENUMS,.ERRARRAY) ;"returns -- PHONENUMS(DFN,<PHONE NUMBER>)="",  ERRARRAY(DFN)=<MESSAGE>
  NEW ONENUM SET ONENUM=""
  FOR  SET ONENUM=$ORDER(PHONENUMS(TMGDFN,ONENUM)) QUIT:+ONENUM'>0  DO
  . NEW EXOPEN,EXCLOSE
  . IF $$ISEXCLED(TMGDFN,ONENUM) SET EXOPEN="(",EXCLOSE=")"
  . ELSE  SET EXOPEN=" ",EXCLOSE=" "
  . NEW LINE SET LINE=PTNAME_" --"_EXOPEN_ONENUM_EXCLOSE_"--"_EXOPEN_$$FMTPHONE(ONENUM)_EXCLOSE
  . SET SELLIST(LINE)=TMGDFN_"^"_ONENUM
  NEW BLOCKLST
  IF $DATA(SELLIST) DO
  . DO SELECTOR^TMGUSRI3("SELLIST","BLOCKLST","Select numbers to exclude. <ESC><ESC> when done.")
  NEW ABORT SET ABORT=0
  NEW LINE SET LINE=""
  FOR  SET LINE=$ORDER(BLOCKLST(LINE)) QUIT:(LINE="")!(ABORT)  DO
  . NEW % SET %=2
  . WRITE "BLOCK ",LINE DO YN^DICN WRITE !
  . SET ABORT=(%=-1) QUIT:(%'=1)
  . SET TMGDFN=$PIECE(BLOCKLST(LINE),"^",1) IF +TMGDFN'>0 WRITE "??",! QUIT
  . NEW PHONENUM SET PHONENUM=$PIECE(BLOCKLST(LINE),"^",2) IF PHONENUM="" WRITE "??" QUIT
  . DO EXCLPHON(TMGDFN,PHONENUM,.ERRARRAY) ;
  IF $DATA(ERRARRAY) WRITE "ERRORS:",! DO ZWRITE^TMGZWR("ERRARRAY") GOTO OODN
  QUIT
  ;
MSGLOG(MSG,ARR) ;"ADD MESSAGE TO SMS LOG
  NEW RESULT SET RESULT=$$ADDLOG^TMGLOG01("SMS MESSAGES ADMIN LOG",.MSG,.ARR)
  ;" NEW NOW SET NOW=$$NOW^XLFDT
  ;" NEW REF SET REF=$NAME(^TMG("TMP","SMS INFO"))
  ;" FOR  QUIT:$DATA(@REF@(NOW))=0  SET NOW=NOW+0.00000001
  ;" SET @REF@(NOW)=MSG
  QUIT
  ;
