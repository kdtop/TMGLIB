TMGSEQE1 ;TMG/KST - Error Alert handling (From SequelPMS); 11/10/14
        ;;1.0;TMG-LIB;**1**; 11/10/14
       ;
 ;"TMG KERNEL FUNCTIONS -- i.e. functions that are OS specific.
 ;"(c) Kevin Toppenberg MD
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
 ;"Dependancies
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;
SETALRT(ERRTEXT,AMSG,ERRHANDLER) ;
  ;"Purpose: Set up alerts for error handling of POC filer problems.
  ;"NOTE: called from CREATE^LA7LOG
  ;"Input: ERRTEXT -- Text of error.
  ;"       AMSG -- Additional message, If any.
  ;"       ERRHANDLER -- E.G. "HANDLERR^TMGSEQE1" -- the error handler to use
  ;"  ALSO -- the entire variable table will be stored, so the error
  ;"          handler will be able to make use of any stored variable
  ;"Results: NONE:
  ;"Output: An alert is created. 
  ;"Restore original message
  NEW NOW SET NOW=$H
  SET ERRTEXT=$GET(ERRTEXT)
  IF (ERRTEXT["^")&(+ERRTEXT>0) SET ERRTEXT=$PIECE(ERRTEXT,"^",2,99)
  SET AMSG=$GET(AMSG)
  NEW TMGERROR SET TMGERROR="[SEQLMED-PROCESS ERR]: "_ERRTEXT
  IF AMSG'="" SET TMGERROR=TMGERROR_"; MESSAGE]: "_AMSG
  NEW VTABLE ZSHOW "V":VTABLE        
  MERGE ^TMG("TMP","TMGSEQL",$J,NOW,"ERROR VARS")=VTABLE("V")
  KILL VTABLE
  ;"MAKE AN ALERT WITH ERROR MESSAGE.
  NEW XQA,XQAARCH,XQADATA,XQAFLG,XQAGUID,XQAID,XQAOPT,XQAROU,XQASUPV,XQASURO,XQATEXT
  SET XQA(150)=""   ;"//to Eddie Hagood
  SET XQADATA=$J_"^"_NOW
  SET XQAID="TMG"
  SET XQAROU=$GET(ERRHANDLER)
  SET XQAMSG="Error during SequelMed process."
  SET ^TMG("TMP","TMGSEQL",$J,NOW,"ERROR")=TMGERROR
  NEW TEMP SET TEMP=$$SETUP1^XQALERT
  IF +TEMP'=1 DO  
  . ;"Could do something else with error of failed error-creation here, I guess.  
  QUIT
  ;          
HNDLERR ;
  ;"Purpose -- Handler for alert created during POC filing system.
  ;"Input: Globally scoped variable: XQADATA will hold $J^$H
  ;"       ^TMG("TMP","TMGSEQL",$J,$H,"ERROR VARS") holds variable table at time of error.
  ;"       ^TMG("TMP","TMGSEQL",$J,$H,"ERROR")=Error message.
  ;"FINISH....
  NEW JB,DH SET JB=$PIECE(XQADATA,"^",1) SET DH=$PIECE(XQADATA,"^",2)
  ;"NEW VARS MERGE VARS=^TMG("TMP","TMGSEQL",JB,DH,"ERROR VARS")
  NEW REF SET REF=$NAME(^TMG("TMP","TMGSEQL",JB,DH,"ERROR VARS"))
  NEW ERRMSG SET ERRMSG=$GET(^TMG("TMP","TMGSEQL",JB,DH,"ERROR"))  
  WRITE !,"ERROR MESSAGE:",!
  WRITE ERRMSG,!
  WRITE "-----------------------------------------------------",!
  NEW DATA,FULLPATHNAME
  ;"Recover relevent info from stored variables.
  NEW ZZTMGTEMPI SET ZZTMGTEMPI=""
  FOR  SET ZZTMGTEMPI=$ORDER(@REF@(ZZTMGTEMPI)) QUIT:(+ZZTMGTEMPI'>0)  DO
  . NEW ZZTMGTEMPS SET ZZTMGTEMPS=$GET(@REF@(ZZTMGTEMPI)) QUIT:ZZTMGTEMPS=""
  . IF ($EXTRACT(ZZTMGTEMPS,1,5)="DATA(")!(ZZTMGTEMPS["FULLPATHNAME=") DO
  . . SET ZZTMGTEMPS="SET "_ZZTMGTEMPS
  . . XECUTE ZZTMGTEMPS
  NEW ERRFOUND SET ERRFOUND=0
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(DATA("ERR",IDX)) QUIT:+IDX'>0  DO
  . NEW ERR SET ERR=$GET(DATA("ERR",IDX))
  . NEW ERRCODE SET ERRCODE=$PIECE(ERR,"|",1),ERR=$PIECE(ERR,"|",2,99)
  . WRITE ERR,!
  . IF $DATA(DATA(IDX)) DO ZWRITE^TMGZWR($NAME(DATA(IDX))) WRITE "-------------",!
  . SET ERRFOUND=1
  . ;"FINISH HANDLING HERE...
  IF ERRFOUND,$DATA(DATA("A")) DO ZWRITE^TMGZWR($NAME(DATA("A"))) WRITE "-------------",!
  NEW TMGDFN SET TMGDFN=0
  FOR  SET TMGDFN=$ORDER(DATA("APPT","ERR",TMGDFN)) QUIT:(+TMGDFN'>0)  DO
  . NEW ERR SET ERR=$GET(DATA("APPT","ERR",TMGDFN)) QUIT:ERR=""
  . WRITE "Fileman error while filing data:",!
  . IF $DATA(DATA("APPT",TMGDFN)) DO ZWRITE^TMGZWR($NAME(DATA("APPT",TMGDFN))) WRITE !
  . WRITE "MESSAGE: ",ERR,!
  . WRITE "-------------",!
  SET TMGDFN=0
  FOR  SET TMGDFN=$ORDER(DATA("ERR2",TMGDFN)) QUIT:(+TMGDFN'>0)  DO
  . NEW SUBIEN SET SUBIEN=0
  . FOR  SET SUBIEN=$ORDER(DATA("ERR2",TMGDFN,SUBIEN)) QUIT:(+SUBIEN'>0)  DO
  . . NEW ERR SET ERR=$GET(DATA("ERR2",TMGDFN,SUBIEN)) QUIT:ERR=""
  . . WRITE "Fileman error while cancelling appt:",!
  . . WRITE "IEN in file 22723 = ",TMGDFN,!
  . . WRITE "SUB-IEN in file 22723.01 = ",SUBIEN,!
  . . WRITE "MESSAGE: ",ERR,!
  . . WRITE "-------------",!
  WRITE !,"Done with alert handler.",!
  WRITE "Errors must be fixed at the source.  Can't be fixed here...",!
  NEW % SET %=1
  WRITE "Delete stored 'DATA' (can not be undone, voids RENEW of alert)"
  DO YN^DICN
  IF %=1 DO
  . KILL ^TMG("TMP","TMGSEQL",JB,DH)
  QUIT
