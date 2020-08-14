TMGRSTU3 ;TMG/kst/Web RPC Utilities; 2/14/15
       ;;1.0;TMG-LIB;**1**;2/12/15
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
 ;"GETSHARD(GRESULT,CREDNTID) -- recover pre-stored credentials from another user sharing context
 ;"SETSHARD() -- store current user's credentials for later sharing of context
 ; 
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"GNODENAM() QUIT "TMG_WEB_SHARED_CREDENTIALS"
 ;"MAKCRDNT()  ;"MAKE NEW CREDENTIALS ID
 ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"=======================================================================
 ;"Uses:  
 ;"=======================================================================
 ;
 ;"NOTES:  
 ;"   The process of sharing credentials will be as follows:
 ;"   1. User will log into system using normal RPC authentication
 ;"   2. User will call RPC 'TMG XUS SET SHARED SIGNON', which will
 ;"      store users current credentials into temp storage location.
 ;"      Location will be indicated by a CREDENTIALS_ID string.  This
 ;"      CREDENTIALS_ID will be returned to user.
 ;"   3. User can then use CREDENTIALS_ID in a WEB RPC call, calling 
 ;"      RPC: 'TMG XUS GET SHARED SIGNON', which will recover the stored
 ;"      credentials.
 ;"   4. Store credentials will be short-lived, so that prior unused stores
 ;"      won't accumulate.  The process of storing and retrieving will
 ;"      both search for and delete expired credentials.
 ;
 ;"--The credentials ide will be stored as follows:
 ;"  ^XTMP("TMG_WEB_SHARED_CREDENTIALS",0)=PurgeDate^CreateData^Description with creater DUZ
 ;"  ^XTMP("TMG_WEB_SHARED_CREDENTIALS",IEN,0)=ID^LastAccess$H
 ;"  ^XTMP("TMG_WEB_SHARED_CREDENTIALS",IEN,"VARS",#)="Z(""sub1"",""sub2"")=1" <-- sample 
 ;"  ^XTMP("TMG_WEB_SHARED_CREDENTIALS","B",ID)=IEN
 ;"  ^XTMP("TMG_WEB_SHARED_CREDENTIALS","C",LastAccess$H,IEN)=""
 ; 
GNODENAM() QUIT "TMG_WEB_SHARED_CREDENTIALS"
  ;
GETSHARD(OUT,CREDNTID)  ;"Entry point for RPC TMG XUS GET SHARED SIGNON
  ;"Purpose: To recover pre-stored credentials from another user sharing context
  ;"NOTE!!:  This will KILL THE CURRENT ENVIORMENT (ALL VARIABLES ON THE SYSTEM
  ;"         VARIABLE TABLE, AND REPLACE IT WITH STORED ENVIRONEMENT.
  ;"        If an environment matching CREDNTID is not found, then local 
  ;"        environment is NOT killed. 
  ;"NOTE: this may copy IO array.  E.g. IO(0)="/dev/pts/2".  This IO channel
  ;"   will likely not be valid for this new environment, and user may need
  ;"   to kill IO and reinitialize.
  ;"Input: OUT -- PASS BY REFERENCE.  AN OUT PARAMETER.
  ;"          OUT(1)="1^OK"  or "-1^<error message>"  
  ;"       CREDNTID: String used to identify shared credentials
  ;"Results: None, but see OUT above. 
  NEW ZZZTMGRESULT SET ZZZTMGRESULT="1^OK"
  NEW ZZZHASHID SET ZZZHASHID=$$EN^XUSHSH(CREDNTID)
  DO RESTORSN^TMGRSTU2(ZZZHASHID,1,.ZZZTMGRESULT,"ZZZTMGRESULT,ZZZHASHID",$$GNODENAM())
  IF +ZZZTMGRESULT>0 DO
  . NEW NODENAME SET NODENAME=$$GNODENAM()
  . NEW IEN SET IEN=$$GETSNIEN^TMGRSTU2(ZZZHASHID,NODENAME)
  . IF IEN'>0 QUIT
  . DO KILLONE^TMGRSTU2(IEN,NODENAME)  ;"Delete session after single use
  SET OUT(1)=ZZZTMGRESULT
  QUIT
  ;
SETSHARD(OUT)  ;"Entry point for RPC TMG XUS SET SHARED SIGNON
  ;"Purpose: To store current mumps session variable table, including user's 
  ;"         credentials for later sharing of context
  ;"Input: OUT -- PASS BY REFERENCE.  AN OUT PARAMETER.  Format:
  ;"  OUT(1) = CREDENTIALS ID 
  ;"Results: None, but see OUT above. 
  NEW ZZZID SET ZZZID=$$MAKCRDNT()
  NEW ZZZHASHID SET ZZZHASHID=$$EN^XUSHSH(ZZZID)
  DO SAVESN^TMGRSTU2(ZZZHASHID,"ZZZID,ZZZHASHID",$$GNODENAM())
  SET OUT(1)=ZZZID  
  QUIT
  ;  
MAKCRDNT()  ;"MAKE NEW CREDENTIALS ID
  NEW CRDNTID SET CRDNTID=$$MAKESN^TMGRSTU2($$GNODENAM())
  QUIT CRDNTID
  ;