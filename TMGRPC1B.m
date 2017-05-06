TMGRPC1B ;TMG/kst-RPC Functions ; 6/28/15, 6/15/16
         ;;1.0;TMG-LIB;**1**;3/28/10
 ;
 ;"TMG RPC FUNCTIONS  related to CPRS
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
 ;"MEGA   ;Install multiple Post-KIDS patch routines
 ;"ENSUREALL -- Ensure all needed TMG RPC entries have been added
 ;"CHKMSNG --MENU TO CHECK PATCH FOR MISSING PARTS
 ;"SHOWMSNG -- Show which RPC's, are missing from list here in L1
 ;"SHWMSNG2 -- Show missing RPC'S in a BUILD 
 ;"SHWMSNGR -- Show missing RPC'S from REMOTE PROCEDURE file in a BUILD
 ;"SHWMSNGF -- Show missing ROUTINES (source files) in a BUILD 
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"ENSURE1(RPCNAME) -- ensure 1 RPC is in OPTION record OR CPRS GUI CHART
 ;"GETLIST(OUT)  -- Read list at L1 into an array 
 ;"ISPRESNT(RPC) -- return if named RPC is present in REMOTE PROCEDURE file
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;"Dependencies:
 ;" DIC
 ;"=======================================================================
 ;"=======================================================================
 ;
MEGA   ;Install multiple Post-KIDS patch routines
       ;
       WRITE "***************************************************",!
       WRITE "****         TMG POST-INSTALL ROUTINES         ****",!
       WRITE "***************************************************",!
       DO PINST1^TMGRPC1D
       DO INSTWEDG^TMGHRPC2
       DO INSTALL^TMGRPCSR
       DO FIXDD2^TMGFIX2
       DO ENSUREAL^TMGRPC1B
       DO ADDRPT^TMGGRC2
       DO POSTINST^TMGKIDS  ;"//kt I think this is duplicate and less complete than ENSUREAL()
       DO FIX8927FD08^TMGFIX2 
       WRITE "****                  FINISHED                 ****",!
       QUIT
       ;
L1      ;"LIST OF CPRS FOR TMG-CPRS
        ;;TMG ADD PATIENT
        ;;TMG AUTOSIGN TIU DOCUMENT
        ;;TMG BARCODE DECODE
        ;;TMG BARCODE ENCODE
        ;;TMG CHANNEL
        ;;TMG CPRS GET URL LIST
        ;;TMG DOWNLOAD FILE
        ;;TMG DOWNLOAD FILE DROPBOX
        ;;TMG GET BLANK TIU DOCUMENT
        ;;TMG GET DFN
        ;;TMG GET NOTE ADDENDUMS
        ;;TMG GET IMAGE LONG DESCRIPTION
        ;;TMG GET PATIENT DEMOGRAPHICS
        ;;TMG SET PATIENT DEMOGRAPHICS
        ;;TMG INIFILE GET
        ;;TMG INIFILE SET
        ;;TMG MSGLINK CHANNEL
        ;;TMG SEARCH CHANNEL
        ;;TMG SET PATIENT DEMOGRAPHICS
        ;;TMG UPLOAD FILE
        ;;TMG UPLOAD FILE DROPBOX
        ;;TMG IMAGE DELETE
        ;;MAGGADDIMAGE
        ;;MAG3 TIU IMAGE
        ;;MAG3 CPRS TIU NOTE
        ;;MAGG PAT PHOTOS
        ;;TMG USER PWD GET
        ;;TMG CPRS CREATE COMPONENT REC
        ;;TMG MENU GET BY NAME
        ;;TMG MENU GET ROOT
        ;;TMG CONSOLE INITIATE
        ;;TMG CONSOLE SERVER POST
        ;;TMG CONSOLE SERVER QUERY
        ;;TMG CONSOLE SHUT DOWN
        ;;TMG CONSOLE DISALLOW OPTION
        ;;TMG KEENE GET ACCOUNT NUMBERS
        ;;TMG KEENE ICN CHECKSUM
        ;;TMG KEENE PROVIDERS
        ;;TMG KEENE SET ACCOUNT NUMBER
        ;;TMG KEENE SET ADMISSION NUMBER
        ;;DGWPT SELECT
        ;;DGWPT1 PRCARE
        ;;TMG MULTI IMAGE ADD
        ;;TMG MULTI IMAGE GET
        ;;TMG MULTI IMAGE INFO
        ;;TMG MULTI IMAGE BY NAME
        ;;TMG CPRS ACCESS LOG RETRIEVE
        ;;TMG CPRS ACCESS LOG STORE
        ;;TMG CPRS GET AUDIT DETAIL
        ;;TMG CPRS GET AUDIT PER USER
        ;;TMG CPRS GET AUDIT PER PATIENT
        ;;TMG CPRS GET DD HELP
        ;;TMG CPRS GET INSTIT LIST
        ;;TMG CPRS GET LAB COMPONENTS
        ;;TMG CPRS GET LAB LIST
        ;;TMG CPRS GET SPEC LIST
        ;;TMG CPRS GET URL LIST
        ;;TMG CPRS LAB DEF SPECIMEN GET
        ;;TMG CPRS LAB DEF SPECIMEN SET
        ;;TMG CPRS LAB GET RESULTS
        ;;TMG CPRS LAB ALERT        
        ;;TMG CPRS POST LAB VALUES  
        ;;TMG CPRS PROCESS NOTE 
        ;;TMG CPRS MACRO RESOLVE
        ;;TMG CPRS DISPLAY COS DIALOG
        ;;TMG CPRS GET CUSTOM PS CODE
        ;;TMG CPRS STORE ESIG IMAGE
        ;;TMG CPRS SEARCH TEMPLATE
        ;;TMG CPRS GET PT DUE STATUS
        ;;TMG CPRS MACRO RESOLVE
        ;;TMG CPRS PROBLEM LINK
        ;;TMG CPRS PROBLEM TOPICS
        ;;TMG CPRS TOPIC PROB SUGGEST
        ;;TMG CPRS CHANGE ES
        ;;TMG DB CONTROL VALUES
        ;;VEFA CAREPLAN ADD UPDATE
        ;;VEFA CAREPLAN DELETE
        ;;VEFA CAREPLAN GET 1 ITEM
        ;;VEFA CAREPLAN GET CAREPLANS
        ;;VEFA CAREPLAN GET ITEMS
        ;;VEFA CAREPLAN GET LINKED DX
        ;;VEFA CAREPLAN GET OR CREATE
        ;;VEFA CAREPLAN GET PROB ICD
        ;;VEFA CAREPLAN GET PROB INFO
        ;;VEFA CAREPLAN GET RESULTS
        ;;VEFA CAREPLAN GETITEMS
        ;;VEFA CAREPLAN SET ACTIVE STAT
        ;;VEFA CAREPLAN SET CAREPLAN
        ;;VEFA CAREPLAN SET LINKED DX
        ;;VEFA CAREPLAN TP MATCH PL
        ;;VEFA GET LIST OF PATIENT LISTS
        ;;VEFA GET LIST TYPES
        ;;VEFA GET PATIENT LIST
        ;;VEFA GET PROB INFO
        ;;VEFA GET XP STRING
        ;;VEFA LETTERS GET
        ;;VEFA TEMPLATE MATCH PROBLIST
        ;;<END>
        ;
GETLIST(OUT)  ;"Read list at L1 into an array
        NEW TMGI
        NEW DONE SET DONE=0
        FOR TMGI=1:1 DO  QUIT:DONE
        . NEW RPC SET RPC=$PIECE($TEXT(L1+TMGI^TMGRPC1B),";;",2)
        . SET RPC=$$TRIM^XLFSTR(RPC)
        . IF RPC="<END>" SET DONE=1 QUIT
        . SET OUT(RPC)=""
        QUIT
        ;       
ENSUREAL ;
        ;"Ensure all needed TMG RPC entries have been added
        NEW TMGI
        NEW BOOLKEENE SET BOOLKEENE=0  ;SET TO 1 TO INSTALL KEENE RPCs
        NEW DIC,X,Y,DA,CPRSIEN,RPC
        SET DIC="^DIC(19,",DIC(0)="M"
        SET X="OR CPRS GUI CHART"
        DO ^DIC
        IF +Y'>0 DO  QUIT
        . WRITE "ERROR.  Unable to find [OR CPRS GUI CHART] in file OPTION (#19)",!
        . NEW TEMP READ "Press [ENTER] to continue...",TEMP:($GET(DTIME,3600))
        . WRITE !
        SET CPRSIEN=+Y
        ;
        NEW TMGOUT,TMGMSG
        DO LIST^DIC(19.05,","_CPRSIEN_",",,,"*",,,,,,"TMGOUT","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO ESAD
        . DO SHOWDIER^TMGDEBU2(.TMGMSG)
        NEW FOUND
        SET TMGI=0
        FOR  SET TMGI=$ORDER(TMGOUT("DILIST",1,TMGI)) QUIT:(+TMGI'>0)  DO
        . SET RPC=$GET(TMGOUT("DILIST",1,TMGI)) QUIT:RPC=""
        . SET FOUND(RPC)=+$GET(TMGOUT("DILIST",2,TMGI))
        ;
        NEW RPCLIST
        DO GETLIST(.RPCLIST)  ;"Read list at L1 into an array
        NEW MSNGRPC
        NEW DONE SET DONE=0
        NEW RPC SET RPC=""
        FOR  SET RPC=$ORDER(RPCLIST(RPC)) QUIT:RPC=""  DO
        . IF (RPC["KEENE")&(BOOLKEENE=0) QUIT
        . IF $DATA(FOUND(RPC)) QUIT
        . IF $$ISPRESNT(RPC)=0 SET MSNGRPC(RPC)="" QUIT
        . DO ENSURE1(RPC)
        IF $DATA(MSNGRPC) DO
        . WRITE "ERROR: The following RPC(s) are missing from the REMOTE PROCEDURE",!
        . WRITE "       file, and WERE NOT INSTALLED.  Perhaps there is a missing ",!
        . WRITE "       patch?  Please contact your IT support technician.",!
        . NEW RPC SET RPC=""
        . FOR  SET RPC=$ORDER(MSNGRPC(RPC)) QUIT:(RPC="")  DO
        . . WRITE "  --> ",RPC,!
        . WRITE !
        . HANG 2
ESAD    QUIT
        ;
ISPRESNT(RPC) ;
        ;"Purpose: to return if named RPC is present in REMOTE PROCEDURE file
        ;"Input:  RPC -- NAME of remote procedure to call to check.
        ;"Result: 1 IF RPC is installed, 0 IF missing
        NEW DIC,X,Y
        NEW RESULT SET RESULT=0 ;"default to failure
        SET DIC=8994 ;"REMOTE PROCEDURE file
        SET DIC(0)="MXV" ;"mult index, exact match, verify entry is OK
        SET X=$GET(RPC) IF X="" GOTO IPDN
        DO ^DIC
        SET RESULT=(+Y>0)
IPDN    QUIT RESULT
        ;
        ;
ENSURE1(RPCNAME) ;
        ;"Purpose: to ensure 1 RPC is in OPTION record OR CPRS GUI CHART
        ;"         (add IF needed)
        NEW DIC,X,Y,DA
        SET DIC="^DIC(19,",DIC(0)="M"
        SET X="OR CPRS GUI CHART"
        DO ^DIC
        IF +Y'>0 DO  QUIT
        . WRITE "ERROR.  Unable to find [OR CPRS GUI CHART] in file OPTION (#19)",!
        . NEW TEMP READ "Press [ENTER] to continue...",TEMP:($GET(DTIME,3600))
        . WRITE !
        SET DA(1)=+Y
        SET DIC=DIC_DA(1)_",""RPC"","
        SET DIC(0)="ML" ;"LAYGO --> add entry IF not found
        SET X=RPCNAME
        ;"NOTE: 8/17/10.. This doesn't seem to be working.  I don't know why
        ;"Needs to be rewritten in a two step fashion, search then post IF absent.
        ;"NOTE: I seems maybe they are being added after all, but Y is not returned ??
        DO ^DIC
        IF +Y'>0 DO
        . WRITE "ERROR.  Unable to add or find "_RPCNAME_" for subfile RPC in record",!
        . WRITE "OR CPRS GUI CHART in file OPTION (#19)",!
        . NEW TEMP READ "Press [ENTER] to continue...",TEMP:($GET(DTIME,3600))
        . WRITE !
        QUIT
        ;
        ;"==============================================
CHKMSNG ;"MENU TO CHECK PATCH FOR MISSING PARTS
        NEW MENU
        SET MENU(0)="Select Option"
        SET MENU(1)="Show missing RPC's, comparing L1 lst to REMOTE PROCEDURE file"_$C(9)_"SHOWMSNG"
        SET MENU(2)="Show missing RPC's, comparing L1 list to BUILD"_$C(9)_"SHWMSNG2"
        SET MENU(3)="Show misssing RPC's, comparing BUILD to REMOTE PROCEDURE file"_$C(9)_"SHWMSNGR"  
        SET MENU(4)="Show missing ROUTINES, comparing BUILD to ROUTINES file"_$C(9)_"SHWMSNGF"
        NEW USRINPUT,CMD
CHKL    SET USRINPUT=$$MENU^TMGUSRI2(.MENU)
        IF (USRINPUT="")!(USRINPUT="^") QUIT
        SET CMD="DO "_USRINPUT_"^TMGRPC1B"
        XECUTE CMD
        GOTO CHKL
        ;
SHOWMSNG ;"SHOW MISSING
        ;"Show which RPC's, are missing from list L1, compared to RPC 
        ;"multiple in OR CPRS GUI CHART option
        NEW ARR,TMGI,RPC       
        NEW RPCLIST
        DO GETLIST(.RPCLIST)  ;"Read list at L1 into an array

        NEW DONE SET DONE=0
        SET RPC="" FOR  SET RPC=$ORDER(RPCLIST(RPC)) QUIT:RPC=""  DO
        . SET ARR(RPC)=1
        NEW NAMESPACE
        SET DONE=0
        FOR  DO  QUIT:DONE
        . WRITE "Enter a namespace (e.g. 'TMG') to scan for (^ to abort): "
        . NEW NS READ NS:$GET(DTIME,3600) WRITE !
        . IF NS="" SET NS="^" 
        . IF NS="^" SET DONE=1 QUIT
        . SET NAMESPACE(NS)=""
        NEW DIC,X,Y
        SET DIC=19,DIC(0)="M"
        SET X="OR CPRS GUI CHART"
        DO ^DIC
        IF +Y'>0 DO  GOTO SWMDN
        . WRITE "CAN'T FIND OR CPRS GUI CHART!!!",!
        WRITE !,"Scanning through all entries in REMOTE PROCEDURE file",!
        WRITE "  for matches in given namespace(s), to ensure all entries",!
        WRITE "    are present in list.",!
        WRITE "-------------------------------------",!
        NEW RPCIEN SET RPCIEN=0
        FOR  SET RPCIEN=$ORDER(^DIC(19,+Y,"RPC",RPCIEN)) QUIT:(+RPCIEN'>0)  DO
        . NEW RPCPTR SET RPCPTR=+$GET(^DIC(19,+Y,"RPC",RPCIEN,0)) QUIT:RPCPTR'>0
        . NEW RPCNAME SET RPCNAME=$PIECE($GET(^XWB(8994,RPCPTR,0)),"^",1)
        . NEW NS SET NS=""
        . FOR  SET NS=$ORDER(NAMESPACE(NS)) QUIT:NS=""  DO
        . . IF $EXTRACT(RPCNAME,1,$LENGTH(NS))'=NS QUIT
        . . IF $GET(ARR(RPCNAME))=1 DO  QUIT  ;"RPC ALREADY PRESENT
        . . . WRITE RPCNAME," --> Already present",!
        . . WRITE "MISSING: ",RPCNAME,", should add to list at L1^TMGRPC1B",!
SWMDN   QUIT                
  ;
SHWMSNG2 ;"Show missing RPC'S in a BUILD 
        ;"Show which RPC's, are missing from list L1, compared to RPC in BUILD 
        NEW DIC,X,Y SET DIC(0)="MAEQ",DIC=9.6  ;"9.6 = BUILD file
        DO ^DIC WRITE !
        IF +Y'>0 QUIT
        NEW BLDIEN SET BLDIEN=+Y
        ;
        NEW RPCLIST,RPC 
        DO GETLIST(.RPCLIST)
        NEW RPCMISSING
        NEW FOUNDCT SET FOUNDCT=0
        NEW RPC SET RPC="" 
        FOR  SET RPC=$ORDER(RPCLIST(RPC)) QUIT:RPC=""  DO
        . NEW TMPIEN SET TMPIEN=$ORDER(^XPD(9.6,BLDIEN,"KRN",8994,"NM","B",RPC,0))
        . IF TMPIEN>0 SET FOUNDCT=FOUNDCT+1 QUIT
        . SET RPCMISSING(RPC)=""
        WRITE FOUNDCT," RPC's in L1 list are already in BUILD",!
        IF $DATA(RPCMISSING) DO
        . WRITE "And here are the RPC's missing from BUILD",!
        . DO ZWRITE^TMGZWR("RPCMISSING")
        ELSE  DO
        . WRITE "And there were NO missing RPC's in BUILD",!
        QUIT
        ;
SHWMSNGR ;"Show missing RPC'S from REMOTE PROCEDURE file in a BUILD
        ;"Looks at available RPC's and sees if any are missing from BUILD.
        NEW DIC,X,Y SET DIC(0)="MAEQ",DIC=9.6  ;"9.6 = BUILD file
        DO ^DIC WRITE !
        IF +Y'>0 QUIT
        NEW BLDIEN SET BLDIEN=+Y
        ;
        NEW ARR,TMGI,RPC,MISSING        
        NEW NAMESPACE
        SET DONE=0
        WRITE "Enter a namespace (e.g. 'TMG') to scan for (ENTER when done, or ^ to abort): "
        NEW NS READ NS:$GET(DTIME,3600) WRITE !
        IF NS="" SET NS="^" 
        IF NS="^" QUIT
        NEW RPCNAME SET RPCNAME=NS 
        FOR  SET RPCNAME=$ORDER(^XWB(8994,"B",RPCNAME)) QUIT:(RPCNAME="")!($EXTRACT(RPCNAME,1,$LENGTH(NS))'=NS)  DO
        . WRITE RPCNAME," (in REMOTE PROCEDURE file) --> " 
        . NEW TMPIEN SET TMPIEN=$ORDER(^XPD(9.6,BLDIEN,"KRN",8994,"NM","B",RPCNAME,0))
        . IF TMPIEN>0 DO  QUIT  ;"RPC ALREADY PRESENT
        . . WRITE " OK, present in build",!
        . WRITE "MISSING: ",RPCNAME,", should REMOTE PROCEDURES in build.",!
        . SET MISSING(RPCNAME)=""
        WRITE !
        IF $DATA(MISSING) DO
        . WRITE "SUMMARY OF MISSING RPC'S, that should be added to BUILD'",!
        . DO ZWRITE^TMGZWR("MISSING")
        QUIT
        ;
SHWMSNGF ;"Show missing ROUTINES (source files) in a BUILD
        ;"Looks at available ROUTINEs and sees if any are missing from BUILD.
        NEW DIC,X,Y SET DIC(0)="MAEQ",DIC=9.6  ;"9.6 = BUILD file
        DO ^DIC WRITE !
        IF +Y'>0 QUIT
        NEW BLDIEN SET BLDIEN=+Y
        ;
        NEW ARR,TMGI,RPC,MISSING        
        NEW NAMESPACE
        SET DONE=0
        WRITE "Enter a namespace (e.g. 'TMG') to scan for (ENTER when done, or ^ to abort): "
        NEW NS READ NS:$GET(DTIME,3600) WRITE !
        IF NS="" SET NS="^" 
        IF NS="^" QUIT
        NEW FNAME SET FNAME=NS 
        FOR  SET FNAME=$ORDER(^DIC(9.8,"B",FNAME)) QUIT:(FNAME="")!($EXTRACT(FNAME,1,$LENGTH(NS))'=NS)  DO
        . WRITE FNAME," (in ROUTINE file) --> " 
        . NEW TMPIEN SET TMPIEN=$ORDER(^XPD(9.6,BLDIEN,"KRN",9.8,"NM","B",FNAME,0))
        . IF TMPIEN>0 DO  QUIT  ;"FILE ALREADY PRESENT
        . . WRITE " OK, present in build",!
        . WRITE "MISSING: ",FNAME,", should be added to ROUTINES in build.",!
        . SET MISSING(FNAME)=""
        WRITE !
        IF $DATA(MISSING) DO
        . WRITE "SUMMARY OF MISSING ROUTINES'S, that should be added to BUILD'",!
        . DO ZWRITE^TMGZWR("MISSING")
        QUIT
        ;        