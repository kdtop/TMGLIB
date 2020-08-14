TMGRPC6A ;TMG/kst/Support Functions for tmg-messenger ;09/17/09, 2/2/14
         ;;1.0;TMG-LIB;**1**;09/17/09
 ;
 ;"TMG RPC FUNCTIONS for TMG-Messenger program
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
 ;" RPC -- Public Functions.
 ;"=======================================================================
 ;"GETEMULT(TMGOUT,TMGPARAMS) ;"GET USERS OF EMAIL ADDRESS
 ;"GETUEMA(TMGOUT,TMGPARAMS) ;"GET MULT USERS EMAIL ADDRESSES
 ;"SETUEMA(TMGOUT,TMGPARAMS) ;"SET ONE USER'S EMAIL ADDRESS
 ;"KILLUEMA(TMGOUT,TMGPARAMS) ;"REMOVE ONE EMAIL ADDRESS FROM A USER
 ;"SETUSEM(TMGOUT,TMGPARAMS) ;"SET MULTIPLE USERS EMAIL ADDRESSES
 ;"GFLSUBST(TMGOUT,TMGPARAMS) ;"GET FILE SUBSET
 ;"GETIEN8925(TMGOUT,TMGUID) ;"GET IEN 8925 FOR IMAP UID
 ;"SETUID(TMGOUT,TMGPARAMS) ;"SET IMAP UID FOR IEN 8925
 ;"GETEMDOC(TMGOUT,TMGPARAMS) ;"GET IEN OF 'EMAIL' DOC IN 8925.1
 ;"
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;"Dependencies:
 ;" TMGDEBUG
 ;"=======================================================================
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;
GETEMULT(TMGOUT,TMGPARAMS) ;"GET USERS OF EMAIL ADDRESS
        ;"Purpose: to fill list with users with matching email address
        ;"Input: TMGOUT -- an OUT PARAMETER, PASS BY REFERENCE.
        ;"       TMGPARAMS -- email address, e.g. 'Someuser@gmail.com'
        ;"Output: TMGOUT is filled as follows:
        ;"          TMGOUT(0)="#Found^Success" or "-1^Message"
        ;"          e.g. 1^Success --> 1 match found
        ;"               2^Success --> 2 matches found
        ;"               0^Success --> no errors, but no matches found.
        ;"          TMGOUT(1)=Name^DOB^IEN2
        ;"          TMGOUT(2)=Name^DOB^IEN2
        ;"
        ;"Results: none
 ;
        MERGE ^TMG("TMP","RPC","GETEMULT","TMGPARAMS")=TMGPARAMS
        NEW TMGINDEX SET TMGINDEX=1
        NEW TMGEMAIL SET TMGEMAIL=$$LOW^XLFSTR($EXTRACT($GET(TMGPARAMS),1,128))
        IF TMGEMAIL="" DO  QUIT
        . SET TMGOUT(0)="-1^No email address passed for lookup"
        NEW TMGIEN SET TMGIEN=""
        KILL TMGOUT
        FOR  SET TMGIEN=$ORDER(^DPT("ATMGEMAIL",TMGEMAIL,TMGIEN)) QUIT:(+TMGIEN'>0)  DO
        . NEW TMGNAME SET TMGNAME=$PIECE($GET(^DPT(TMGIEN,0)),"^",1)
        . NEW TMGDOB SET TMGDOB=$PIECE($GET(^DPT(TMGIEN,0)),"^",3)
        . SET TMGDOB=$$FMTE^XLFDT(TMGDOB,"5D") ;"MM/DD/YYY format
        . SET TMGOUT(TMGINDEX)=TMGNAME_"^"_TMGDOB_"^"_TMGIEN
        . SET TMGINDEX=TMGINDEX+1
        FOR  SET TMGIEN=$ORDER(^DPT("ATMGALTEMAIL",TMGEMAIL,TMGIEN)) QUIT:(+TMGIEN'>0)  DO
        . NEW TMGNAME SET TMGNAME=$PIECE($GET(^DPT(TMGIEN,0)),"^",1)
        . NEW TMGDOB SET TMGDOB=$PIECE($GET(^DPT(TMGIEN,0)),"^",3)
        . SET TMGDOB=$$FMTE^XLFDT(TMGDOB,"5D") ;"MM/DD/YYY format
        . SET TMGOUT(TMGINDEX)=TMGNAME_"^"_TMGDOB_"^"_TMGIEN
        . SET TMGINDEX=TMGINDEX+1
 ;
        SET TMGOUT(0)=(TMGINDEX-1)_"^Success"
 ;
        QUIT
 ;
GETUEMA(TMGOUT,TMGPARAMS) ;"GET MULT USERS EMAIL ADDRESSES
        ;"Purpose: to fill list of email address for requested users.
        ;"Input: TMGOUT -- an OUT PARAMETER, PASS BY REFERENCE.
        ;"       TMGPARAMS -- list of DFN's e.g. 1234;2345;234
        ;"Output: TMGOUT is filled as follows:
        ;"          TMGOUT(0)="#Found^Success" or "-1^Message"
        ;"          e.g. 1^Success --> 1 match found
        ;"               2^Success --> 2 matches found
        ;"               0^Success --> no errors, but no matches found.
        ;"          TMGOUT(1)=DFN1^EmailAddress
        ;"          TMGOUT(2)=DFN1^;ALT;AltEmail1
        ;"          TMGOUT(3)=DFN1^;ALT;AltEmail2
        ;"          TMGOUT(4)=DFN2^EmailAddress
        ;"          NOTE: So IF a user has 1 primary and 2 secondary
        ;"              email addresses, then there will be 3 entries
        ;"              starting with the same DFN
        ;"
        ;"Results: none
 ;
        MERGE ^TMG("TMP","RPC","GETUEMA","TMGPARAMS")=TMGPARAMS
        NEW TMGINDEX SET TMGINDEX=1
        NEW TMGI
        NEW TMGEMAIL
        NEW TMGIEN SET TMGIEN=""
        SET TMGPARAMS=$GET(TMGPARAMS)
        KILL TMGOUT
        FOR TMGI=1:1:$LENGTH(TMGPARAMS,";") DO
        . SET TMGIEN=+$PIECE(TMGPARAMS,";",TMGI)
        . QUIT:(TMGIEN'>0)
        . SET TMGEMAIL=$PIECE($GET(^DPT(TMGIEN,.13)),"^",3)
        . SET TMGOUT(TMGINDEX)=TMGIEN_"^"_TMGEMAIL
        . SET TMGINDEX=TMGINDEX+1
        . NEW TMGIEN2 SET TMGIEN2=0
        . FOR  SET TMGIEN2=$ORDER(^DPT(TMGIEN,"TMGALTEMAIL",TMGIEN2)) QUIT:(+TMGIEN2'>0)  DO
        . . NEW TMGALTEMAIL SET TMGALTEMAIL=""
        . . SET TMGALTEMAIL=$PIECE($GET(^DPT(TMGIEN,"TMGALTEMAIL",TMGIEN2,0)),"^",1)
        . . SET TMGOUT(TMGINDEX)=TMGIEN_"^;ALT;"_TMGALTEMAIL
        . . SET TMGINDEX=TMGINDEX+1
 ;
        SET TMGOUT(0)=(TMGINDEX-1)_"^Success"
 ;
        QUIT
 ;
SETUEMA(TMGOUT,TMGPARAMS) ;"SET ONE USER EMAIL ADDRESS
        ;"Purpose: to store a NEW email address for 1 user.
        ;"Input: TMGOUT -- an OUT PARAMETER, PASS BY REFERENCE.
        ;"       TMGPARAMS -- DFN^NewEMailAddress^AltEMail1^AltEMail2^...
        ;"              e.g. 1234^MyEMail@server.com
        ;"              NOTE: NewEMailAddress is optional
        ;"Output: TMGOUT is filled as follows:
        ;"          TMGOUT(0)="1^Success" or "-1^Message"
        ;"
        ;"Results: none
 ;
        MERGE ^TMG("TMP","RPC","SETUEMA","TMGPARAMS")=TMGPARAMS
        KILL TMGOUT
        SET TMGOUT(0)="1^Success"
        SET TMGPARAMS=$GET(TMGPARAMS)
        NEW TMGDFN SET TMGDFN=$PIECE(TMGPARAMS,"^",1)
        IF +TMGDFN'>0 SET TMGOUT(0)="-1^Patient IEN not provided (Got '"_TMGDFN_"')" QUIT
        NEW TMGEMAIL SET TMGEMAIL=$PIECE(TMGPARAMS,"^",2)
        NEW TMGFDA,TMGMSG
        IF TMGEMAIL'="" do
        . SET TMGFDA(2,TMGDFN_",",".133")=TMGEMAIL
        . DO FILE^DIE("EK","TMGFDA","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) DO
        . . SET TMGOUT(0)="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        NEW TMGI,TMGALTEMAIL
        FOR TMGI=3:1:$LENGTH(TMGPARAMS,"^") DO  QUIT:(TMGALTEMAIL="")
        . SET TMGALTEMAIL=$PIECE(TMGPARAMS,"^",TMGI)
        . IF TMGALTEMAIL="" QUIT
        . IF +$ORDER(^DPT(TMGDFN,"ATMGALTEMAIL",$$LOW^XLFSTR(TMGALTEMAIL),""))>0 QUIT
        . KILL TMGFDA NEW TMGIEN
        . SET TMGFDA(2.022703,"+1,"_TMGDFN_",",.01)=TMGALTEMAIL
        . DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) DO
        . . SET TMGOUT(0)="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)  ;"Only will store LAST error.
 ;
        QUIT
 ;
KILLUEMA(TMGOUT,TMGPARAMS) ;"REMOVE ONE EMAIL ADDRESS FROM A USER
        ;"Purpose: Remove an email address, either primary or alternative
        ;"Input: TMGOUT -- an OUT PARAMETER, PASS BY REFERENCE.
        ;"       TMGPARAMS -- DFN^BAD-EMailAddress^Bademail2^bademail3^...
        ;"              e.g. 1234^MyBadEMail@server.com
        ;"Output: TMGOUT is filled as follows:
        ;"          TMGOUT(0)="1^Success" or "-1^Message"
        ;"
        ;"Results: none
        ;
        MERGE ^TMG("TMP","RPC","KILLUEMA","TMGPARAMS")=TMGPARAMS
        SET TMGOUT(0)="1^Success"
        SET TMGPARAMS=$GET(TMGPARAMS)
        NEW TMGDFN SET TMGDFN=$PIECE(TMGPARAMS,"^",1)
        IF +TMGDFN'>0 SET TMGOUT(0)="-1^Patient IEN not provided (Got '"_TMGDFN_"')" QUIT
        NEW TMGI,TMGEMAIL
        FOR TMGI=2:1:$LENGTH(TMGPARAMS,"^") DO  QUIT:(TMGEMAIL="")!(+TMGOUT(0)=-1)
        . NEW TMGFOUND SET TMGFOUND=0
        . SET TMGEMAIL=$$LOW^XLFSTR($PIECE(TMGPARAMS,"^",TMGI))
        . IF TMGEMAIL="" QUIT
        . NEW TMGIEN SET TMGIEN=""
        . FOR  SET TMGIEN=$ORDER(^DPT("ATMGEMAIL",TMGEMAIL,TMGIEN)) QUIT:(+TMGIEN'>0)!(+TMGOUT(0)=-1)  DO
        . . IF TMGIEN'=TMGDFN QUIT
        . . SET TMGFOUND=1
        . . NEW TMGFDA,TMGMSG
        . . SET TMGFDA(2,TMGDFN_",",".133")="@"
        . . DO FILE^DIE("EK","TMGFDA","TMGMSG")
        . . IF $DATA(TMGMSG("DIERR")) DO
        . . . SET TMGOUT(0)="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        . SET TMGIEN=""
        . FOR  SET TMGIEN=$ORDER(^DPT(TMGDFN,"TMGALTEMAIL","B",TMGEMAIL,TMGIEN)) QUIT:(+TMGIEN'>0)!(+TMGOUT(0)=-1)  DO
        . . SET TMGFOUND=1
        . . NEW TMGFDA,TMGMSG
        . . SET TMGFDA(2.022703,TMGIEN_","_TMGDFN_",",".01")="@"
        . . DO FILE^DIE("EK","TMGFDA","TMGMSG")
        . . IF $DATA(TMGMSG("DIERR")) DO
        . . . SET TMGOUT(0)="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        . IF TMGFOUND=0 DO  ;"only will retain LAST message...
        . . SET TMGOUT(0)="-1^Couldn't locate email to delete: "+TMGEMAIL
        ;
        QUIT
 ;
ALTEREMA(TMGOUT,TMGPARAMS) ;"ALTER USER EMAIL ADDRESS
        ;"Purpose: Change the value for an email address
        ;"Input: TMGOUT -- an OUT PARAMETER, PASS BY REFERENCE.
        ;"       TMGPARAMS -- DFN^OldEMailAddress^NewEmailAddress
        ;"              e.g. 1234^OldEMail@server.com^NewEmail@server2.com
        ;"  NOTE: If old value is the value for primary email address, then that will be changed
        ;"        Otherwise, alternative emails will be searched.  Search is case insensitive
        ;"Output: TMGOUT is filled as follows:
        ;"          TMGOUT(0)="1^Success" or "-1^Message"
        ;"
        ;"Results: none
        ;
        MERGE ^TMG("TMP","RPC","ALTEREMA","TMGPARAMS")=TMGPARAMS
        SET TMGOUT(0)="1^Success"
        NEW TMGFOUND SET TMGFOUND=0
        SET TMGPARAMS=$GET(TMGPARAMS)
        NEW TMGDFN SET TMGDFN=$PIECE(TMGPARAMS,"^",1)
        IF +TMGDFN'>0 SET TMGOUT(0)="-1^Patient IEN not provided (Got '"_TMGDFN_"')" QUIT
        NEW TMGI,TMGOLDEMAIL,TMGNEWEMAIL
        SET TMGOLDEMAIL=$$LOW^XLFSTR($PIECE(TMGPARAMS,"^",2))
        IF TMGOLDEMAIL="" DO  QUIT
        . SET TMGOUT(0)="-1^Old email value not provided."
        SET TMGNEWEMAIL=$PIECE(TMGPARAMS,"^",3)
        IF TMGNEWEMAIL="" DO  QUIT
        . SET TMGOUT(0)="-1^New email value not provided."
        NEW TMGIEN SET TMGIEN=""
        FOR  SET TMGIEN=$ORDER(^DPT("ATMGEMAIL",TMGOLDEMAIL,TMGIEN)) QUIT:(+TMGIEN'>0)  DO
        . IF TMGIEN'=TMGDFN QUIT
        . SET TMGFOUND=1
        . NEW TMGFDA,TMGMSG
        . SET TMGFDA(2,TMGDFN_",",".133")=TMGNEWEMAIL
        . DO FILE^DIE("EK","TMGFDA","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) DO
        . . SET TMGOUT(0)="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        IF TMGFOUND=1 QUIT
        SET TMGIEN=""
        FOR  SET TMGIEN=$ORDER(^DPT(TMGDFN,"TMGALTEMAIL","B",TMGOLDEMAIL,TMGIEN)) QUIT:(+TMGIEN'>0)  DO
        . NEW TMGFDA,TMGMSG
        . SET TMGFDA(2.022703,TMGIEN_","_TMGDFN_",",".01")=TMGNEWEMAIL
        . DO FILE^DIE("EK","TMGFDA","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) DO
        . . SET TMGOUT(0)="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        ;
        QUIT
 ;
SETUSEM(TMGOUT,TMGPARAMS) ;"SET MULTIPLE USERS EMAIL ADDRESSES
        ;"Purpose: to store a NEW email address for more than 1 user.
        ;"Input: TMGOUT -- an OUT PARAMETER, PASS BY REFERENCE.
        ;"       TMGPARAMS -- DFN=NewEMailAddress;DFN=NewEMailAddress;DFN=NewEMailAddress....
        ;"Output: TMGOUT is filled as follows:
        ;"          TMGOUT(0)="1^Success" or "-1^Message"
        ;"
        ;"Results: none
        ;"Note: If error encountered, then no further attempts to file others in list tried.
 ;
        MERGE ^TMG("TMP","RPC","SETUSEM","TMGPARAMS")=TMGPARAMS
        KILL TMGOUT
        SET TMGOUT(0)="1^Success"
        SET TMGPARAMS=$GET(TMGPARAMS)
        NEW TMGFDA,TMGMSG,TMGI,TMGERR
        SET TMGERR=0
        FOR TMGI=1:1:$LENGTH(TMGPARAMS,";") DO  QUIT:TMGERR
        . KILL TMGFDA,TMGMSG
        . NEW TMG1PARAM SET TMG1PARAM=$PIECE(TMGPARAMS,";",TMGI)
        . NEW TMGIEN SET TMGIEN=$PIECE(TMG1PARAM,"=",1)
        . IF +TMGIEN'>0 SET TMGOUT(0)="-1^Patient IEN not provided (Got '"_TMGIEN_"')" SET TMGERR=1 QUIT
        . NEW TMGEMAIL SET TMGEMAIL=$PIECE(TMG1PARAM,"=",2)
        . IF TMGEMAIL="" SET TMGOUT(0)="-1^Email address not provided" SET TMGERR=1 QUIT
        . SET TMGFDA(2,TMGIEN_",",".133")=TMGEMAIL
        . DO FILE^DIE("EK","TMGFDA","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) DO
        . . SET TMGOUT(0)="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        . . SET TMGERR=1
 ;
        QUIT
 ;
GFLSUBST(TMGOUT,TMGPARAMS) ;"GET FILE SUBSET
        ;"Purpose: to return a subset of entries a file's .01 names
        ;"Input: TMGOUT -- an OUT PARAMETER, PASS BY REFERENCE.
        ;"       TMGPARAMS -- FileNum^StartFrom^Direction^maxCount
        ;"              TMGFNUM - filename file to traverse
        ;"              StartFrom -- text to $ORDER() from  -- OPTIONAL
        ;"              Direction -- $ORDER(xx,Direction) direction (should be 1 or -1) -- OPTIONAL
        ;"              maxCt -- OPTIONAL -- the max number of entries to return.
        ;"Output: TMGOUT is filled as follows:
        ;"          TMGOUT(0)="1^Success" or "-1^Message"
        ;"          TMGOUT(1)=IEN^Value
        ;"          TMGOUT(2)=IEN^Value
        ;"          ...
        ;"Results: none
        ;"NOTE: does NOT work with sub files.
        ;"      Also, originally copied from TMGRPC3B to remove dependancies to that file
 ;
        NEW TMGFILE SET TMGFILE=+$PIECE(TMGPARAMS,"^",1)
        IF TMGFILE'>0 DO  GOTO GFSDONE
        . SET TMGOUT(0)="-1^No file number supplied"
        NEW TMGFROM SET TMGFROM=$PIECE(TMGPARAMS,"^",2)
        NEW TMGDIR SET TMGDIR=$PIECE(TMGPARAMS,"^",3)
        IF TMGDIR'=-1 SET TMGDIR=1
        NEW TMGMAXCT SET TMGMAXCT=+$PIECE(TMGPARAMS,"^",4)
        IF TMGMAXCT=0 SET TMGMAXCT=44
 ;
        NEW TMGI SET TMGI=0
        ;"NEW TMGLAST SET TMGLAST=""
        ;"NEW prev SET prev=""
        NEW TMGREF SET TMGREF=$GET(^DIC(TMGFILE,0,"GL"))
        SET TMGREF=$$CREF^DILF(TMGREF)  ;"convert open --> closed reference
        IF TMGREF="" DO  GOTO GFSDONE
        . SET TMGOUT(0)="-1^Unable to obtain global ref for file #"_TMGFILE
 ;
        FOR  SET TMGFROM=$ORDER(@TMGREF@("B",TMGFROM),TMGDIR) QUIT:(TMGFROM="")!(TMGI=TMGMAXCT)  DO
        . NEW TMGIEN SET TMGIEN=""
        . FOR  SET TMGIEN=$ORDER(@TMGREF@("B",TMGFROM,TMGIEN),TMGDIR) QUIT:(+TMGIEN'>0)  DO
        . . SET TMGI=TMGI+1
        . . SET TMGOUT(TMGI)=TMGIEN_"^"_$$GET1^DIQ(TMGFILE,TMGIEN_",",.01)
        . . ;"SET TMGOUT(TMGI)=$$GET1^DIQ(TMGFILE,IEN_",",.01)
 ;
        SET TMGOUT(0)="1^Success"
GFSDONE ;
        QUIT
 ;
GETIEN8925(TMGOUT,TMGUID) ;"GET IEN 8925 FOR IMAP UID
      ;"Purpose: To retrieve the IEN from file 8925 that is linked to UID (if any)
      ;"Input:  TMGOUT -- An OUT PARAMETER.  PASS BY REFERENCE.
      ;"        TMGPARAMS -- UID
      ;"Output: TMGOUT is filled as follows:
      ;"          TMGOUT(0)="-1^Message" or TMG(0)=IEN or TMG(0)=0 IF not found.
      ;"Results: none
      NEW TMGIEN
      IF $GET(TMGUID)="" DO  QUIT
      . SET TMGOUT(0)="-1^No UID passed"
      SET TMGIEN=+$ORDER(^TIU(8925,"TMGUID",TMGUID,""))
      SET TMGOUT(0)=TMGIEN
      ;
      QUIT
 ;
SETUID(TMGOUT,TMGPARAMS) ;"SET IMAP UID FOR IEN 8925
      ;"Purpose: To store an IMAP UID (identifier) for a given TIU Document
      ;"Input:  TMGOUT -- An OUT PARAMETER.  PASS BY REFERENCE.
      ;"        TMGPARAMS -- IEN8925^UID
      ;"                IEN8925 -- The IEN in file 8925 to be altered.
      ;"                UID -- The UID to be stored in the above document.
      ;"Output:  TMGOUT(0) = 1^Success, or -1^Error Message
      ;"Results: none
      SET TMGOUT(0)="1^Success" ;"Default to success
      NEW TMGIEN,TMGUID,TMGFDA,TMGMSG
      SET TMGIEN=$PIECE(TMGPARAMS,"^",1)
      IF +TMGIEN'>0 DO  QUIT
      . SET TMGOUT(0)="-1^Bad IEN passed: "_TMGIEN
      SET TMGUID=$PIECE(TMGPARAMS,"^",2)
      IF TMGUID="" DO  QUIT
      . SET TMGOUT(0)="-1^No UID passed."
      SET TMGFDA(8925,TMGIEN_",",22710)=TMGUID
      DO FILE^DIE("EK","TMGFDA","TMGMSG")
      IF $DATA(TMGMSG("DIERR")) DO
      . SET TMGOUT(0)="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
      ;
      QUIT
 ;
GETEMDOC(TMGOUT,TMGPARAMS) ;"
      ;"Purpose: GET IEN OF 'EMAIL' DOC IN 8925.1
      ;"Input:  TMGOUT -- An OUT PARAMETER.  PASS BY REFERENCE.
      ;"        TMGPARAMS -- Not used, so data ignored.
      ;"Output: TMGOUT is filled as follows:
      ;"          TMGOUT(0)="-1^Message" or TMG(0)=IEN^Name
      ;"Results: none
      ;"
      NEW X,Y,DIC
      SET DIC=8925.1
      SET DIC(0)="M"
      SET X="EMAIL"
      DO ^DIC
      IF +Y>0 DO
      . SET TMGOUT(0)=Y
      ELSE  DO
      . SET TMGOUT(0)="-1^Unique title EMAIL not found"
      ;
      QUIT
 ;
GETCONSNT(TMGOUT,TMGPARAMS) ;
      ;"Purpose: Get status of HIPPA consent documented in patient chart
      ;"Input:   TMGOUT -- An OUT PARAMETER.  PASS BY REFERENCE.
      ;"         TMGPARAMS -- PatientIEN^
      ;"Results:  TMGOUT(0) = 1^Codes,  or -1^Error Message
      ;"          Codes are E  -  Email consented;
      ;"                    EC -  email & cell msg consented;
      ;"                    C  -  just cell msg consented.
      ;"                    N  -  NOT CONSENTED, or no code found
      ;"Results : none
      ;"Note: This field is a multiple, and allows status to change over time
      ;"      This routine will return the status for NOW.
      NEW TMGDFN SET TMGDFN=$PIECE(TMGPARAMS,"^",1)
      IF +TMGDFN'>0 DO  QUIT
      . SET TMGOUT(0)="-1^Valid Patient IEN Not Found.  Got: "_TMGDFN
      SET TMGDFN=+TMGDFN
      NEW TMGDT,%,X
      DO NOW^%DTC SET TMGDT=%  ;"Get NOW into TMGDT
      NEW TMGLASTDT
      SET TMGLASTDT=$ORDER(^DPT(TMGDFN,"TMGHIPPA","TMGDATE",TMGDT),-1)
      IF TMGLASTDT="" DO  QUIT
      . SET TMGOUT(0)="-1^NO Current Status for Current Date-Time"
      NEW TMGIEN
      SET TMGIEN=$ORDER(^DPT(TMGDFN,"TMGHIPPA","TMGDATE",TMGLASTDT,""))
      NEW TMGSTATUS
      SET TMGSTATUS=$PIECE($GET(^DPT(TMGDFN,"TMGHIPPA",TMGIEN,0)),"^",1)
      IF TMGSTATUS="" SET TMGSTATUS="N"
      SET TMGOUT(0)="1^"_TMGSTATUS
      QUIT
 ;
SETCONSNT(TMGOUT,TMGPARAMS) ;
      ;"PURPOSE: Set status of HIPPA consent documented in patient chart
      ;"Input   TMGOUT -- An OUT PARAMETER.  PASS BY REFERENCE.
      ;"        TMGPARAMS -- PatientIEN^StatusCodes
      ;"            Codes should be E - Email consented;
      ;"                           EC - email & cell msg consented;
      ;"                            C - just cell msg consented.
      ;"                            N - NOT CONSENTED
      ;"            (Note, codes ARE case sensitive)
      ;"Results:  TMGOUT(0) = 1^Success, or -1^Error Message
      NEW TMGDFN SET TMGDFN=$PIECE(TMGPARAMS,"^",1)
      IF +TMGDFN'>0 DO  QUIT
      . SET TMGOUT(0)="-1^Valid Patient IEN Not Found.  Got: "_TMGDFN
      SET TMGDFN=+TMGDFN
      NEW TMGCODE SET TMGCODE=$PIECE(TMGPARAMS,"^",2)
      NEW TMGVCODES SET TMGVCODES=$PIECE($GET(^DD(2.22704,.01,0)),"^",3)
      NEW TMGOK,TMGI SET TMGOK=0
      FOR TMGI=1:1:$LENGTH(TMGVCODES,";") DO  QUIT:(TMGOK=1)
      . NEW ONECODE SET ONECODE=$PIECE(TMGVCODES,";",TMGI)
      . IF $PIECE(ONECODE,":",1)=TMGCODE SET TMGOK=1 QUIT
      . ;"IF $PIECE(ONECODE,":",2)=TMGCODE SET TMGOK=1 QUIT
      IF TMGOK'=1 DO  QUIT
      . SET TMGOUT(0)="-1^Invalid code.  Got: "_TMGCODE
      NEW TMGDT,%,X
      DO NOW^%DTC SET TMGDT=%  ;"Get NOW into TMGDT
      NEW TMGFDA,TMGMSG,TMGIEN,TMGIENS
      SET TMGIENS="+1,"_TMGDFN_","
      SET TMGFDA(2.22704,TMGIENS,.01)=TMGCODE
      SET TMGFDA(2.22704,TMGIENS,.02)=TMGDT
      SET TMGFDA(2.22704,TMGIENS,.03)=DUZ
      DO UPDATE^DIE("S","TMGFDA","TMGIEN","TMGMSG")
      IF $DATA(TMGMSG("DIERR")) DO
      . SET TMGOUT(0)="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
      ELSE  DO
      . SET TMGOUT(0)="1^Success"
      ;
      QUIT
 ;
