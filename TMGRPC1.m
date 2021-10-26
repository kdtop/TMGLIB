TMGRPC1 ;TMG/kst-RPC Functions ; 4/19/15, 5/23/14, 3/24/21
         ;;1.0;TMG-LIB;**1**;08/18/09
 ;
 ;"TMG RPC FUNCTIONS related to CPRS
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
 ;"DOWNLOAD(GREF,FPATH,FNAME,LOCIEN)        ; Depreciated MOVED to TMGRPC1C
 ;"UPLOAD(RESULT,FPATH,FNAME,LOCIEN,ARRAY)  ; Depreciated MOVED to TMGRPC1C
 ;"DOWNDROP(RESULT,FPATH,FNAME,LOCIEN)      ; Depreciated MOVED to TMGRPC1C
 ;"UPLDDROP(RESULT,FPATH,FNAME,LOCIEN)      ; Depreciated MOVED to TMGRPC1C
 ;"GETLONG(GREF,IMAGEIEN)
 ;"GETDFN(RESULT,RECNUM,RECFIELD,LNAME,FNAME,MNAME,DOB,SEX,SSNUM)
 ;"BLANKTIU(RESULT,TMGDFN,PERSON,LOC,DOS,TITLE)
 ;"AUTOSIGN(RESULT,DOCIEN)
 ;"FNINFO(RESULT,TMGDFN) -- GET PATIENT DEMOGRAPHICS
 ;"PTADD(RESULT,INFO)  -- ADD PATIENT
 ;"STPTINFO(RESULT,TMGDFN,INFO) -- SET PATIENT DEMOGRAPHICS
 ;"GETURLS(RESULT) -- TMG CPRS GET URL LIST
 ;"SIGIMAGE(TMGRESULT,DUZ,IMAGEIEN)  -- Store image as user's signature
 ;"GETESIG(TMGDFN)  ; --To return the provided user's last e-signature
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"ENCODE(GREF,incSubscr,encodeFn)    ; Depreciated MOVED to TMGRPC1C
 ;"DECODE(GREF,incSubscr,decodeFn)    ; Depreciated MOVED to TMGRPC1C 
 ;"$$HEXCODER(INPUT)    ;encode the input string.  Currently using simple hex encoding/ <-- moved to TMGRPC1C
 ;"$$B64CODER(INPUT)    ;encode the input string via UUENCODE (actually Base64) <-- moved to TMGRPC1C
 ;"$$B64DECODER(INPUT)  ;encode the input string via UUDECODE (actually Base64) <-- moved to TMGRPC1C
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;"Dependencies:
 ;"TMGBINF
 ;"TMGSTUTL
 ;"RGUTUU
 ;"=======================================================================
 ;"=======================================================================
 ;
DOWNLOAD(GREF,FPATH,FNAME,LOCIEN) ;"DEPRECIATED
        DO DOWNLOAD^TMGRPC1C(.GREF,.FPATH,.FNAME,.LOCIEN) ;
        QUIT
        ;
UPLOAD(RESULT,FPATH,FNAME,LOCIEN,ARRAY) ;"DEPRECIATED
        SET RESULT="-1^DEFAULT OF ERROR"
        DO UPLOAD^TMGRPC1C(.RESULT,.FPATH,.FNAME,.LOCIEN,.ARRAY) 
        QUIT
        ;
DOWNDROP(RESULT,FPATH,FNAME,LOCIEN)  ;"DEPRECIATED
        DO DOWNDROP^TMGRPC1C(.RESULT,.FPATH,.FNAME,.LOCIEN)  
        QUIT
        ;
UPLDDROP(RESULT,FPATH,FNAME,LOCIEN)  ;"DEPRECIATED
        DO UPLDDROP^TMGRPC1C(.RESULT,.FPATH,.FNAME,.LOCIEN) 
        QUIT
        ;
ENCODE(GREF,INCSUBSCR,ENCODEFN)   ;"DEPRECIATED
        DO ENCODE^TMGRPC1C(.GREF,.INCSUBSCR,.ENCODEFN)  
        QUIT
        ;
DECODE(GREF,INCSUBSCR,DECODEFN)   ;"DEPRECIATED
        DO DECODE^TMGRPC1C(.GREF,.INCSUBSCR,.DECODEFN)
        QUIT
        ;
GETLONG(GREF,IMAGEIEN) ;
        ;"SCOPE: Public
        ;"Purpose: To provide an entry point for a RPC call from a client.
        ;"              Will return results of field 11 (LONG DESCRIPTION) from file IMAGE(2005)
        ;"Input: GREF --        OUT PARAM -- the array to pass the result back in (PASSED BY REFERENCE)
        ;"         IMAGEIEN--  The IEN (record number) from file 2005 (IMAGE)
        ;"Output: results are passed out in @GREF
        ;"              @GREF@(0) = WP header LINECT: format is:  ^^MaxLine^MaxLine^TimeStamp(FM Date/Time Format)
        ;"              @GREF@(1) = WP line 1
        ;"              @GREF@(2) = WP line 2
        ;"              @GREF@(3) = WP line 3
        ;"              @GREF@(4) = WP line 4   ... etc.
        ;
        SET GREF="^TMP(""GETLONG^TMGRPC1"","_$J_")"
        ;
        KILL @GREF
        ;
        NEW i,s,MaxLines,header
        SET header=""
        SET IMAGEIEN=+$GET(IMAGEIEN)
        IF IMAGEIEN>0 DO
        . SET header=$GET(^MAG(2005,IMAGEIEN,3,0))   ;"NOTE: Field 11 held in node 3;0
        SET @GREF@(0)=header
        SET MaxLines=+$PIECE(header,"^",3)
        FOR i=1:1:MaxLines  DO
        . SET @GREF@(i)=$GET(^MAG(2005,IMAGEIEN,3,i,0))
        ;
        QUIT
        ;
        ;
GETDFN(RESULT,RECNUM,PMS,FNAME,LNAME,MNAME,DOB,SEX,SSNUM,AUTOADD) ;
        ;"Purpose: This is a RPC entry point for looking up a patient.
        ;"Input:
        ;"  RESULT  -- an OUT PARAMETER
        ;"  RECNUM  -- Record number from a PMS
        ;"  PMS     -- Which PMS RECNUM refers to (1=Medic,2=Sequel,3=Paradigm)
        ;"  FNAME   -- First Name
        ;"  LNAME   -- Last name
        ;"  MNAME   -- Middle Name or initial
        ;"  DOB     -- Date of birth in EXTERNAL format
        ;"  SEX     -- Patient sex: M or F
        ;"  SSNUM   -- Social security number (digits only)
        ;"  AUTOADD -- Automatically register patient IF needed (if value=1)
        ;"Output: Patient may be added to database IF AUTOADD=1
        ;"Results: Returns DFN (i.e. IEN in PATIENT file) or -1 IF not found or error
        ;
        NEW PATIENT
        SET RESULT=-1  ;"default to not found
        ;
        IF $GET(LNAME)'="" DO
        . SET PATIENT("NAME")=$GET(LNAME)
        . IF $GET(FNAME)'="" SET PATIENT("NAME")=PATIENT("NAME")_","_FNAME
        . IF $GET(MNAME)'="" SET PATIENT("NAME")=PATIENT("NAME")_" "_MNAME
        SET PATIENT("DOB")=$GET(DOB)
        SET PATIENT("SEX")=$GET(SEX)
        SET PATIENT("SSNUM")=$GET(SSNUM)
        ;
        IF $GET(PMS)=1 SET PATIENT("PATIENTNUM")=$GET(RECNUM) ;" <-- Medic account number
        IF $GET(PMS)=2 SET PATIENT("SEQUELNUM")=$GET(RECNUM)  ;" <-- Sequel or other account number
        IF $GET(PMS)=3 SET PATIENT("PARADIGMNUM")=$GET(RECNUM)  ;" <-- Paradigm or other account number
        ;
        ;
        SET RESULT=$$GETDFN^TMGGDFN(.PATIENT)
        ;
        QUIT
        ;
        ;
BLANKTIU(RESULT,TMGDFN,PERSON,LOC,DOS,TITLE) ;
        ;"Purpose: To create a new, blank TIU note and return it's IEN
        ;"Input: TMGDFN  -- IEN in PATIENT file of patient
        ;"       PERSON -- Provider NAME
        ;"       LOC -- Location for NEW document
        ;"       DOS -- Date of Service
        ;"       TITLE -- Title of NEW document
        ;"Results: IEN in file 8925 is returned in RESULT,
        ;"     or -1^ErrMsg1;ErrMsg2...  IF failure
        ;"Note: This functionality probably duplicates that of RPC call:
        ;"        TIU CREATE NOTE  -- found after writing this...
        ;
        NEW DOCUMENT,Flag
        ;
        ;"KILL ^TMG("TMP","BLANKTIU")
        ;"SET ^TMG("TMP","BLANKTIU","DFN")=$G(TMGDFN)
        ;"SET ^TMG("TMP","BLANKTIU","PERSON")=$G(PERSON)
        ;"SET ^TMG("TMP","BLANKTIU","LOC")=$G(LOC)
        ;"SET ^TMG("TMP","BLANKTIU","DOS")=$G(DOS)
        ;"SET ^TMG("TMP","BLANKTIU","TITLE")=$G(TITLE)
        ;
        SET DOCUMENT("DFN")=TMGDFN
        SET DOCUMENT("PROVIDER IEN")=$$GetProvIEN^TMGPUTN0(PERSON)
        IF +LOC=LOC s LOC="`"_LOC
        SET DOCUMENT("LOCATION")=$GET(LOC)
        SET DOCUMENT("DATE")=$GET(DOS)
        SET DOCUMENT("TITLE")=$GET(TITLE)
        SET DOCUMENT("TRANSCRIPTIONIST")=""
        SET DOCUMENT("CHARACTER COUNT - TRANSCRIPTIONIST'S")=0
        ;
        SET RESULT=$$PrepDoc^TMGPUTN0(.DOCUMENT)
        IF +RESULT>0 DO  ;"change capture method from Upload (default) to RPC
        . NEW TMGFDA,TMGMSG
        . SET TMGFDA(8925,RESULT_",",1303)="R"  ;"1303 = capture method. "R" = RPC
        . ;"//kt 3/12/15 -- MERGE ^TMG("TMP","BLANKTIU","TMGFDA")=TMGFDA
        . DO FILE^DIE("E","TMGFDA","TMGMSG")  ;"ignore any errors.
        ELSE  DO
        . NEW i,TMGERRMSG SET TMGERRMSG=""
        . FOR i=1:1:+$GET(DOCUMENT("ERROR","NUM")) DO
        . . SET TMGERRMSG=TMGERRMSG_$GET(DOCUMENT("ERROR",i))_" ||"
        . IF $DATA(DOCUMENT("ERROR","FM INFO"))>0 DO
        . . NEW ref SET ref="DOCUMENT(""ERROR"",""FM INFO"")"
        . . SET TMGERRMSG=TMGERRMSG_"FILEMAN SAYS:"
        . . FOR  SET ref=$query(@ref) QUIT:(ref="")!(ref'["FM INFO")  DO
        . . . IF TMGERRMSG'="" SET TMGERRMSG=TMGERRMSG_" ||"
        . . . SET TMGERRMSG=TMGERRMSG_$PIECE(ref,"DIERR",2)_"="_$GET(@ref)
        . IF TMGERRMSG="" SET TMGERRMSG="Unknown error"
        . SET TMGERRMSG=$TRANSLATE(TMGERRMSG,"^","@")
        . SET $PIECE(RESULT,"^",2)=TMGERRMSG
        ;
        ;"temp
        ;"MERGE ^TMG("TMP","BLANKTIU","RESULT")=RESULT
        ;"MERGE ^TMG("TMP","BLANKTIU","DOCUMENT")=DOCUMENT
        ;
        QUIT
        ;
        ;
AUTOSIGN(RESULT,DOCIEN) ;
        ;"Purpose: To automatically sign TIU note (8925).
        ;"Input: DOCIEN -- the IEN in 8925 of the file to be automatically signed.
        ;"Note: This function will not succeed unless field 1303 holds "R"
        ;"      and an Author found for note
        ;"Results: Results passed back in RESULT(0) ARRAY
        ;"              -1 = failure. 1= success
        ;"         Any error message is passed back in RESULT("DIERR")
        ;"Note: This differs from RPC CALL: TIU SIGN RECORD in that a signiture
        ;"      code is NOT required
        ;
        NEW TMGFDA,TMGMSG
        NEW AuthorIEN,AuthorName
        NEW CaptureMethod
        ;
        SET DOCIEN=+$GET(DOCIEN)
        SET RESULT=-1  ;"default to failure
        NEW TITLEIEN   ;",EXCLUDE
        SET TITLEIEN=+$PIECE($GET(^TIU(8925,DOCIEN,0)),"^",1)
        SET AuthorIEN=$PIECE($GET(^TIU(8925,DOCIEN,12)),"^",2)
        IF AuthorIEN'>0 DO  GOTO ASDone
        . SET RESULT("DIERR")="Unable to find author of document."
        ;"The following line will test to see if the individual user
        ;"has this set to autosignature. If so, it will continue on, but
        ;"by default this will only create an alert that a document exists
        ;"to sign
        ;"Original code below...
        ;"IF $DATA(^TIU(8925.1,TITLEIEN,"TMG1","B",AuthorIEN)) SET EXCLUDE="Y"        
        ;"ELSE  SET EXCLUDE=$PIECE($GET(^TIU(8925.1,TITLEIEN,"TMG")),"^",1)
        ;"IF EXCLUDE="Y" DO  GOTO ASDone
        ;". SET RESULT(0)=1
        ;". DO SEND^TIUALRT(DOCIEN)
        ;
        ;"New code     9/3/15
        IF '$DATA(^TIU(8925.1,TITLEIEN,"TMG1","B",AuthorIEN)) DO  GOTO ASDone
        . SET RESULT(0)=1
        . DO SEND^TIUALRT(DOCIEN)
        ;"
        SET CaptureMethod=$PIECE($GET(^TIU(8925,DOCIEN,13)),"^",3)
        IF CaptureMethod'="R" DO  GOTO ASDone
        . SET RESULT("DIERR")="Unable to auto-sign.  Upload-Method was not 'R'."
        SET AuthorName=$PIECE($GET(^VA(200,AuthorIEN,0)),"^",1)
        NEW SIGNATURE SET SIGNATURE=$PIECE($GET(^VA(200,AuthorIEN,20)),"^",2) ;"elh added for signature reasons
        IF SIGNATURE="" SET SIGNATURE=AuthorName  ;"elh added for signature 10/1/15
        ;
        SET TMGFDA(8925,DOCIEN_",",.05)="COMPLETED"      ;"field .05 = STATUS
        SET TMGFDA(8925,DOCIEN_",",1501)="NOW"           ;"field 1501 = Signed date
        SET TMGFDA(8925,DOCIEN_",",1502)="`"_AuthorIEN   ;"field 1502 = signed by
        SET TMGFDA(8925,DOCIEN_",",1503)=SIGNATURE    ;"formerly AuthorName  ;"field 1503 = Signature block name
        SET TMGFDA(8925,DOCIEN_",",1504)="[Scanned image auto-signed]" ;"field 1504 = Signature block title
        SET TMGFDA(8925,DOCIEN_",",1505)="C"  ;C=Chart   ;"field 1505 = Signature mode
        DO FILE^DIE("E","TMGFDA","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO ASDone
        . MERGE RESULT("DIERR")=TMGMSG("DIERR")
        ;
        SET RESULT(0)=1  ;"SET success IF we got this far.
ASDone  ;
        QUIT
        ;
        ;
DFNINFO(RESULT,TMGDFN) ;
        ;"Purpose: To return array with demographcs details about patient
        ;"Input: RESULT (this is the output array)
        ;"       TMGDFN : The record number in file #2 of the patient to inquire about.
        ;"Results: Results passed back in RESULT array.  Format as follows:
        ;"              The results are in format: KeyName=Value,
        ;"              There is no SET order these will appear.
        ;"              Here are the KeyName names that will be provided.
        ;"              If the record has no value, then value will be empty
        ;"      IEN=record#
        ;"      COMBINED_NAME=
        ;"      LNAME=
        ;"      FNAME=
        ;"      MNAME=
        ;"      PREFIX=
        ;"      SUFFIX=
        ;"      DEGREE
        ;"      DOB=
        ;"      SEX=
        ;"      SS_NUM=
        ;"      ADDRESS_LINE_1=
        ;"      ADDRESS_LINE_2=
        ;"      ADDRESS_LINE_3=
        ;"      CITY=
        ;"      STATE=
        ;"      ZIP4=
        ;"      BAD_ADDRESS=
        ;"      TEMP_ADDRESS_LINE_1=
        ;"      TEMP_ADDRESS_LINE_2=
        ;"      TEMP_ADDRESS_LINE_3=
        ;"      TEMP_CITY=
        ;"      TEMP_STATE=
        ;"      TEMP_ZIP4=
        ;"      TEMP_STARTING_DATE=
        ;"      TEMP_ENDING_DATE=
        ;"      TEMP_ADDRESS_ACTIVE=
        ;"      CONF_ADDRESS_LINE_1=
        ;"      CONF_ADDRESS_LINE_2=
        ;"      CONF_ADDRESS_LINE_3=
        ;"      CONF_CITY=
        ;"      CONF_STATE=
        ;"      CONF_ZIP4=
        ;"      CONF_STARTING_DATE=
        ;"      CONF_ENDING_DATE=
        ;"      CONF_ADDRESS_ACTIVE=
        ;"      PHONE_RESIDENCE=
        ;"      PHONE_WORK=
        ;"      PHONE_CELL=
        ;"      PHONE_TEMP=
        ;
        ;"Note, for the following, there may be multiple entries.  # is record number
        ;"      ALIAS # NAME
        ;"      ALIAS # SSN
        ;
        NEW TMGFDA,TMGMSG,IENS
        SET IENS=""
        NEW ptrParts SET ptrParts=0
        SET TMGDFN=+$GET(TMGDFN)
        IF TMGDFN>0 DO
        . SET ptrParts=+$PIECE($GET(^DPT(TMGDFN,"NAME")),"^",1) ;"ptr to file #20, NAME COMPONENTS
        . SET IENS=TMGDFN_","
        . DO GETS^DIQ(2,IENS,"**","N","TMGFDA","TMGMSG")
        ;
        NEW LINECT SET LINECT=0
        SET RESULT(LINECT)="IEN="_TMGDFN SET LINECT=LINECT+1
        SET RESULT(LINECT)="COMBINED_NAME="_$GET(TMGFDA(2,IENS,.01)) SET LINECT=LINECT+1
        NEW s SET s=""
        IF ptrParts>0 SET s=$GET(^VA(20,ptrParts,1))
        SET RESULT(LINECT)="LNAME="_$PIECE(s,"^",1) SET LINECT=LINECT+1
        SET RESULT(LINECT)="FNAME="_$PIECE(s,"^",2) SET LINECT=LINECT+1
        SET RESULT(LINECT)="MNAME="_$PIECE(s,"^",3) SET LINECT=LINECT+1
        SET RESULT(LINECT)="PREFIX="_$PIECE(s,"^",4) SET LINECT=LINECT+1
        SET RESULT(LINECT)="SUFFIX="_$PIECE(s,"^",5) SET LINECT=LINECT+1
        SET RESULT(LINECT)="DEGREE="_$PIECE(s,"^",5) SET LINECT=LINECT+1
        SET RESULT(LINECT)="DOB="_$GET(TMGFDA(2,IENS,.03)) SET LINECT=LINECT+1
        SET RESULT(LINECT)="SEX="_$GET(TMGFDA(2,IENS,.02)) SET LINECT=LINECT+1
        SET RESULT(LINECT)="SS_NUM="_$GET(TMGFDA(2,IENS,.09)) SET LINECT=LINECT+1
        SET RESULT(LINECT)="EMAIL="_$GET(TMGFDA(2,IENS,.133)) SET LINECT=LINECT+1
        SET RESULT(LINECT)="ADDRESS_LINE_1="_$GET(TMGFDA(2,IENS,.111)) SET LINECT=LINECT+1
        SET RESULT(LINECT)="ADDRESS_LINE_2="_$GET(TMGFDA(2,IENS,.112)) SET LINECT=LINECT+1
        SET RESULT(LINECT)="ADDRESS_LINE_3="_$GET(TMGFDA(2,IENS,.113)) SET LINECT=LINECT+1
        SET RESULT(LINECT)="CITY="_$GET(TMGFDA(2,IENS,.114)) SET LINECT=LINECT+1
        SET RESULT(LINECT)="STATE="_$GET(TMGFDA(2,IENS,.115)) SET LINECT=LINECT+1
        IF $GET(TMGFDA(2,IENS,.1112))'="" DO   ;"was .1122   ?
        . SET RESULT(LINECT)="ZIP4="_$GET(TMGFDA(2,IENS,.1112)) SET LINECT=LINECT+1
        ELSE  IF $GET(TMGFDA(2,IENS,.116))'="" DO   ;"was   .1116  ?
        . SET RESULT(LINECT)="ZIP4="_$GET(TMGFDA(2,IENS,.116)) SET LINECT=LINECT+1
        SET RESULT(LINECT)="BAD_ADDRESS="_$GET(TMGFDA(2,IENS,.121)) SET LINECT=LINECT+1
        SET RESULT(LINECT)="TEMP_ADDRESS_LINE_1="_$GET(TMGFDA(2,IENS,.1211)) SET LINECT=LINECT+1
        SET RESULT(LINECT)="TEMP_ADDRESS_LINE_2="_$GET(TMGFDA(2,IENS,.1212)) SET LINECT=LINECT+1
        SET RESULT(LINECT)="TEMP_ADDRESS_LINE_3="_$GET(TMGFDA(2,IENS,.1213)) SET LINECT=LINECT+1
        SET RESULT(LINECT)="TEMP_CITY="_$GET(TMGFDA(2,IENS,.1214)) SET LINECT=LINECT+1
        SET RESULT(LINECT)="TEMP_STATE="_$GET(TMGFDA(2,IENS,.1215)) SET LINECT=LINECT+1
        SET RESULT(LINECT)="TEMP_ZIP4="_$GET(TMGFDA(2,IENS,.1216)) SET LINECT=LINECT+1
        SET RESULT(LINECT)="TEMP_STARTING_DATE="_$GET(TMGFDA(2,IENS,.1217)) SET LINECT=LINECT+1
        SET RESULT(LINECT)="TEMP_ENDING_DATE="_$GET(TMGFDA(2,IENS,.1218)) SET LINECT=LINECT+1
        SET RESULT(LINECT)="TEMP_ADDRESS_ACTIVE="_$GET(TMGFDA(2,IENS,.12105)) SET LINECT=LINECT+1
        SET RESULT(LINECT)="CONF_ADDRESS_LINE_1="_$GET(TMGFDA(2,IENS,.1411)) SET LINECT=LINECT+1
        SET RESULT(LINECT)="CONF_ADDRESS_LINE_1="_$GET(TMGFDA(2,IENS,.1412)) SET LINECT=LINECT+1
        SET RESULT(LINECT)="CONF_ADDRESS_LINE_1="_$GET(TMGFDA(2,IENS,.1413)) SET LINECT=LINECT+1
        SET RESULT(LINECT)="CONF_CITY="_$GET(TMGFDA(2,IENS,.1414)) SET LINECT=LINECT+1
        SET RESULT(LINECT)="CONF_STATE="_$GET(TMGFDA(2,IENS,.1415)) SET LINECT=LINECT+1
        SET RESULT(LINECT)="CONF_ZIP4="_$GET(TMGFDA(2,IENS,.1416)) SET LINECT=LINECT+1
        SET RESULT(LINECT)="CONF_STARTING_DATE="_$GET(TMGFDA(2,IENS,.1417)) SET LINECT=LINECT+1
        SET RESULT(LINECT)="CONF_ENDING_DATE="_$GET(TMGFDA(2,IENS,.1418)) SET LINECT=LINECT+1
        SET RESULT(LINECT)="CONF_ADDRESS_ACTIVE="_$GET(TMGFDA(2,IENS,.14105)) SET LINECT=LINECT+1
        SET RESULT(LINECT)="PHONE_RESIDENCE="_$GET(TMGFDA(2,IENS,.131)) SET LINECT=LINECT+1
        SET RESULT(LINECT)="PHONE_WORK="_$GET(TMGFDA(2,IENS,.132)) SET LINECT=LINECT+1
        SET RESULT(LINECT)="PHONE_CELL="_$GET(TMGFDA(2,IENS,.134)) SET LINECT=LINECT+1
        SET RESULT(LINECT)="PHONE_TEMP="_$GET(TMGFDA(2,IENS,.1219)) SET LINECT=LINECT+1
        ;
        ;"the GETS doesn't return ALIAS entries, so will DO manually:
        NEW Itr,IEN
        SET IEN=$$ItrInit^TMGITR(2.01,.Itr,TMGDFN_",")
        IF IEN'="" FOR  DO  QUIT:(+$$ItrNext^TMGITR(.Itr,.IEN)'>0)
        . NEW s SET s=$GET(^DPT(TMGDFN,.01,IEN,0))
        . IF s="" QUIT
        . SET RESULT(LINECT)="ALIAS "_IEN_" NAME="_$PIECE(s,"^",1) SET LINECT=LINECT+1
        . SET RESULT(LINECT)="ALIAS "_IEN_" SSN="_$PIECE(s,"^",2) SET LINECT=LINECT+1
        . ;"maybe later DO something with NAME COMPONENTS in Alias.
        ;
        QUIT
        ;
        ;
STPTINFO(RESULT,TMGDFN,INFO)  ;" SET PATIENT INFO
        ;"Purpose: To SET demographcs details about patient
        ;"Input: RESULT (this is the output array)
        ;"       TMGDFN : The record number in file #2 of the patient to inquire about.
        ;"       INFO: Format as follows:
        ;"              The results are in format: INFO("KeyName")=Value,
        ;"              There is no SET order these will appear.
        ;"              Here are the KeyName names that will be provided.
        ;"              If the record has no value, then value will be empty
        ;"              If a record should be deleted, its value will be @
        ;"      INFO("COMBINED_NAME")=
        ;"      INFO("PREFIX")=
        ;"      INFO("SUFFIX")=
        ;"      INFO("DEGREE")=
        ;"      INFO("DOB")=
        ;"      INFO("SEX")=
        ;"      INFO("SS_NUM")=
        ;"      INFO("ADDRESS_LINE_1")=
        ;"      INFO("ADDRESS_LINE_2")=
        ;"      INFO("ADDRESS_LINE_3")=
        ;"      INFO("CITY")=
        ;"      INFO("STATE")=
        ;"      INFO("ZIP4")=
        ;"      INFO("BAD_ADDRESS")=
        ;"      INFO("TEMP_ADDRESS_LINE_1")=
        ;"      INFO("TEMP_ADDRESS_LINE_2")=
        ;"      INFO("TEMP_ADDRESS_LINE_3")=
        ;"      INFO("TEMP_CITY")=
        ;"      INFO("TEMP_STATE")=
        ;"      INFO("TEMP_ZIP4")=
        ;"      INFO("TEMP_STARTING_DATE")=
        ;"      INFO("TEMP_ENDING_DATE")=
        ;"      INFO("TEMP_ADDRESS_ACTIVE")=
        ;"      INFO("CONF_ADDRESS_LINE_1")=
        ;"      INFO("CONF_ADDRESS_LINE_2")=
        ;"      INFO("CONF_ADDRESS_LINE_3")=
        ;"      INFO("CONF_CITY")=
        ;"      INFO("CONF_STATE")=
        ;"      INFO("CONF_ZIP4")=
        ;"      INFO("CONF_STARTING_DATE")=
        ;"      INFO("CONF_ENDING_DATE")=
        ;"      INFO("CONF_ADDRESS_ACTIVE")=
        ;"      INFO("PHONE_RESIDENCE")=
        ;"      INFO("PHONE_WORK")=
        ;"      INFO("PHONE_CELL")=
        ;"      INFO("PHONE_TEMP")=
        ;"Note, for the following, there may be multiple entries.  # is record number
        ;"  If a record should be added, it will be marked +1, +2 etc.
        ;"      INFO("ALIAS # NAME")=
        ;"      INFO("ALIAS # SSN")=
        ;"
        ;"Results: Results passed back in RESULT string:
        ;"          1              = success
        ;"          -1^Message     = failure
        ;
        SET RESULT=1  ;"default to success
        ;
        ;"KILL ^TMG("TMP","RPC")
        ;"MERGE ^TMG("TMP","RPC")=INFO   ;"temp... remove later
        ;
        NEW TMGFDA,TMGMSG,IENS
        SET IENS=TMGDFN_","
        NEW TMGKEY SET TMGKEY=""
        FOR  SET TMGKEY=$ORDER(INFO(TMGKEY)) QUIT:(TMGKEY="")  DO
        . IF TMGKEY="COMBINED_NAME" SET TMGFDA(2,IENS,.01)=INFO("COMBINED_NAME")
        . ELSE  IF +TMGKEY=TMGKEY SET TMGFDA(2,IENS,TMGKEY)=INFO(TMGKEY)
        . ELSE  IF TMGKEY="DOB" SET TMGFDA(2,IENS,.03)=INFO("DOB")
        . ELSE  IF TMGKEY="SEX" SET TMGFDA(2,IENS,.02)=INFO("SEX")
        . ELSE  IF TMGKEY="SS_NUM" SET TMGFDA(2,IENS,.09)=INFO("SS_NUM")
        . ELSE  IF TMGKEY="ADDRESS_LINE_1" SET TMGFDA(2,IENS,.111)=INFO("ADDRESS_LINE_1")
        . ELSE  IF TMGKEY="ADDRESS_LINE_2" SET TMGFDA(2,IENS,.112)=INFO("ADDRESS_LINE_2")
        . ELSE  IF TMGKEY="ADDRESS_LINE_3" SET TMGFDA(2,IENS,.113)=INFO("ADDRESS_LINE_3")
        . ELSE  IF TMGKEY="CITY" SET TMGFDA(2,IENS,.114)=INFO("CITY")
        . ELSE  IF TMGKEY="STATE" SET TMGFDA(2,IENS,.115)=INFO("STATE")
        . ELSE  IF TMGKEY="ZIP4" SET TMGFDA(2,IENS,.1112)=INFO("ZIP4")
        . ELSE  IF TMGKEY="BAD_ADDRESS" SET TMGFDA(2,IENS,.121)=INFO("BAD_ADDRESS")
        . ELSE  IF TMGKEY="TEMP_ADDRESS_LINE_1" SET TMGFDA(2,IENS,.1211)=INFO("TEMP_ADDRESS_LINE_1")
        . ELSE  IF TMGKEY="TEMP_ADDRESS_LINE_2" SET TMGFDA(2,IENS,.1212)=INFO("TEMP_ADDRESS_LINE_2")
        . ELSE  IF TMGKEY="TEMP_ADDRESS_LINE_3" SET TMGFDA(2,IENS,.1213)=INFO("TEMP_ADDRESS_LINE_3")
        . ELSE  IF TMGKEY="TEMP_CITY" SET TMGFDA(2,IENS,.1214)=INFO("TEMP_CITY")
        . ELSE  IF TMGKEY="TEMP_STATE" SET TMGFDA(2,IENS,.1215)=INFO("TEMP_STATE")
        . ELSE  IF TMGKEY="TEMP_ZIP4" SET TMGFDA(2,IENS,.12112)=INFO("TEMP_ZIP4")
        . ELSE  IF TMGKEY="TEMP_STARTING_DATE" SET TMGFDA(2,IENS,.1217)=INFO("TEMP_STARTING_DATE")
        . ELSE  IF TMGKEY="TEMP_ENDING_DATE" SET TMGFDA(2,IENS,.1218)=INFO("TEMP_ENDING_DATE")
        . ELSE  IF TMGKEY="TEMP_ADDRESS_ACTIVE" SET TMGFDA(2,IENS,.12105)=INFO("TEMP_ADDRESS_ACTIVE")
        . ELSE  IF TMGKEY="CONF_ADDRESS_LINE_1" SET TMGFDA(2,IENS,.1411)=INFO("CONF_ADDRESS_LINE_1")
        . ELSE  IF TMGKEY="CONF_ADDRESS_LINE_2" SET TMGFDA(2,IENS,.1412)=INFO("CONF_ADDRESS_LINE_2")
        . ELSE  IF TMGKEY="CONF_ADDRESS_LINE_3" SET TMGFDA(2,IENS,.1413)=INFO("CONF_ADDRESS_LINE_3")
        . ELSE  IF TMGKEY="CONF_CITY" SET TMGFDA(2,IENS,.1414)=INFO("CONF_CITY")
        . ELSE  IF TMGKEY="CONF_STATE" SET TMGFDA(2,IENS,.1415)=INFO("CONF_STATE")
        . ELSE  IF TMGKEY="CONF_ZIP" SET TMGFDA(2,IENS,.1416)=INFO("CONF_ZIP")
        . ELSE  IF TMGKEY="CONF_STARTING_DATE" SET TMGFDA(2,IENS,.1417)=INFO("CONF_STARTING_DATE")
        . ELSE  IF TMGKEY="CONF_ENDING_DATE" SET TMGFDA(2,IENS,.1418)=INFO("CONF_ENDING_DATE")
        . ELSE  IF TMGKEY="CONF_ADDRESS_ACTIVE" SET TMGFDA(2,IENS,.14105)=INFO("CONF_ADDRESS_ACTIVE")
        . ELSE  IF TMGKEY="PHONE_RESIDENCE" SET TMGFDA(2,IENS,.131)=INFO("PHONE_RESIDENCE")
        . ELSE  IF TMGKEY="PHONE_WORK" SET TMGFDA(2,IENS,.132)=INFO("PHONE_WORK")
        . ELSE  IF TMGKEY="PHONE_CELL" SET TMGFDA(2,IENS,.134)=INFO("PHONE_CELL")
        . ELSE  IF TMGKEY="PHONE_TEMP" SET TMGFDA(2,IENS,.1219)=INFO("PHONE_TEMP")
        . ELSE  IF TMGKEY="EMAIL" SET TMGFDA(2,IENS,.133)=INFO("EMAIL")
        ;
        IF $DATA(TMGFDA) DO
        . DO FILE^DIE("EKST","TMGFDA","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) DO
        . . SET RESULT="-1^Filing Error Occured: "_$GET(TMGMSG("DIERR",1,"TEXT",1))
        . . ;"MERGE ^TMG("TMP","RPC","DIERR")=TMGMSG("DIERR")
        . . ;"MERGE ^TMG("TMP","RPC","FDA")=TMGFDA
        ;
        ;"now file Alias info separately
        IF RESULT=1 DO
        . NEW tempArray,index,TMGKEY2
        . NEW TMGKEY SET TMGKEY=""
        . FOR  SET TMGKEY=$ORDER(INFO(TMGKEY)) QUIT:(TMGKEY="")  DO
        . . IF TMGKEY["ALIAS" DO
        . . . SET index=$PIECE(TMGKEY," ",2) QUIT:(index="")
        . . . SET TMGKEY2=$PIECE(TMGKEY," ",3)
        . . . SET tempArray(index,TMGKEY2)=INFO(TMGKEY)
        . SET index=0 FOR  SET index=$ORDER(tempArray(index)) QUIT:(index="")!(RESULT'=1)  DO
        . . NEW TMGFDA,TMGMSG,TMGIEN,newRec
        . . SET newRec=0
        . . SET TMGKEY="" FOR  SET TMGKEY=$ORDER(tempArray(index,TMGKEY)) QUIT:(TMGKEY="")!(RESULT'=1)  DO
        . . . IF TMGKEY="NAME" SET TMGFDA(2.01,index_","_TMGDFN_",",.01)=$GET(tempArray(index,"NAME"))
        . . . IF TMGKEY="SSN" SET TMGFDA(2.01,index_","_TMGDFN_",",1)=$GET(tempArray(index,"SSN"))
        . . . IF index["+" SET newRec=1
        . . IF $DATA(TMGFDA) DO
        . . . IF newRec=0 DO FILE^DIE("EKST","TMGFDA","TMGMSG")
        . . . ELSE  DO UPDATE^DIE("ES","TMGFDA","TMGIEN","TMGMSG")
        . . IF $DATA(TMGMSG("DIERR")) DO
        . . . SET RESULT="-1^Filing Error Occured: "_$GET(TMGMSG("DIERR",1,"TEXT",1))
        . . . ;"MERGE ^TMG("TMP","RPC","DIERR")=TMGMSG("DIERR")
        . . . ;"MERGE ^TMG("TMP","RPC","FDA")=TMGFDA
        ;
        QUIT
        ;
        ;
PTADD(RESULT,INFO)  ;" ADD PATIENT  RPC Entry point
        ;"Purpose: To add a patient
        ;"Input: RESULT (this is the output array)
        ;"
        ;"       INFO: Format as follows:
        ;"              The results are in format: INFO("KeyName")=Value,
        ;"              There is no SET order these will appear.
        ;"              Here are the KeyName names that will be provided.
        ;"              If the record has no value, then value will be empty
        ;"              If a record should be deleted, its value will be @
        ;"      INFO("COMBINED_NAME")=
        ;"      INFO("DOB")=
        ;"      INFO("SEX")=
        ;"      INFO("SS_NUM")=
        ;"      INFO("Veteran")=
        ;"      INFO("PtType")=
        ;"      INFO("ADDRESS1")=
        ;"      INFO("CITY")=
        ;"      INFO("STATE")=
        ;"      INFO("ADDRESS2")=
        ;"Results: Results passed back in RESULT string:
        ;"          DFN           = success
        ;"          -1^Message    = failure
        ;"          0^DFN        = already exists
        ;"----------------------------------------------------------
        NEW TMGDEBUG SET TMGDEBUG=0
        IF TMGDEBUG=1 DO
        . KILL INFO
        . MERGE INFO=^TMG("TMP","RPC","PTADD^TMGRPC1","INFO")
        KILL ^TMG("TMP","RPC","PTADD^TMGRPC1","INFO")
        MERGE ^TMG("TMP","RPC","PTADD^TMGRPC1","INFO")=INFO
        SET RESULT="-1^Unknown error"  ;"default to failure
        NEW TMGFDA,TMGMSG,PATIENT,TMGDFN
        NEW TMGKEY SET TMGKEY=""
        FOR  SET TMGKEY=$ORDER(INFO(TMGKEY)) QUIT:(TMGKEY="")  DO
        . IF TMGKEY="COMBINED_NAME" SET PATIENT("NAME")=INFO("COMBINED_NAME")
        . ELSE  IF TMGKEY="DOB" SET PATIENT("DOB")=INFO("DOB")
        . ELSE  IF TMGKEY="SEX" SET PATIENT("SEX")=INFO("SEX")
        . ELSE  IF TMGKEY="SS_NUM" SET PATIENT("SSNUM")=INFO("SS_NUM")
        . ELSE  IF TMGKEY="Veteran" SET PATIENT("VETERAN")=INFO("Veteran")
        . ELSE  IF TMGKEY="PtType" SET PATIENT("PT_TYPE")=INFO("PtType")
        . ELSE  IF TMGKEY="ADDRESS1" SET PATIENT("ADDRESS1")=INFO("ADDRESS1")
        . ELSE  IF TMGKEY="ADDRESS2" SET PATIENT("ADDRESS2")=INFO("ADDRESS2")
        . ELSE  IF TMGKEY="CITY" SET PATIENT("CITY")=INFO("CITY")
        . ELSE  IF TMGKEY="STATE" SET PATIENT("STATE")=INFO("STATE")
        . ELSE  IF TMGKEY="ZIP" SET PATIENT("ZIP")=INFO("ZIP")
        . ELSE  IF TMGKEY="SEQUELNUM" SET PATIENT("SEQUELNUM")=INFO("SEQUELNUM")
        . ELSE  IF TMGKEY="PHONE" SET PATIENT("PHONE")=INFO("PHONE")
        . ELSE  IF TMGKEY="CELL" SET PATIENT("CELL")=INFO("CELL")
        SET TMGDFN=$$GETDFN^TMGGDFN(.PATIENT,1)  ;"1=will autoregister if needed
        IF TMGDFN>0 DO
        . IF $PIECE(TMGDFN,"^",2)=1 SET RESULT=+TMGDFN  ;"Added new
        . ELSE  SET RESULT="0^"_+TMGDFN  ;"i.e. already existed
        ELSE  IF TMGDFN<0 DO
        . SET RESULT=TMGDFN  ;"Should be in -1^Message format
        ;
        ;"IF TMGDFN=-1 DO
        ;". NEW Entry,RESULT,TMGERRMSG
        ;". DO PAT2ENTRY^TMGGDFN(.PATIENT,.Entry)
        ;". SET TMGDFN=$$ADDNEWPAT^TMGGDFN(.Entry,.TMGERRMSG)
        ;". ;"SET TMGDFN=$$GETDFN^TMGGDFN(.PATIENT)
        ;". IF TMGDFN>0 DO
        ;". . SET RESULT=TMGDFN
        ;". ELSE  DO
        ;". . SET RESULT="-1^ERROR ADDING"
        ;". . IF $GET(TMGERRMSG)'="" SET RESULT="-1^ERROR ADDING: "_$$GETERRST^TMGDEBU2(.TMGERRMSG)
        ;". . ELSE  SET RESULT="-1^"_$PIECE(TMGDFN,"^",2)
        ;"ELSE  DO
        ;". SET RESULT="0^"_TMGDFN
        ;
        QUIT
        ;
        ;
GETBARCD(GREF,MESSAGE,OPTION)
        ;"SCOPE: Public
        ;"RPC that calls this: TMG BARCODE ENCODE
        ;"Purpose: To provide an entry point for a RPC call from a client.
        ;"         A 2D DataMatrix Bar Code will be create and passed to client.
        ;"         It will not be stored on server
        ;"Input: GREF --   OUT PARAM -- the array to pass the result back in (PASSED BY REFERENCE)
        ;"       MESSAGE-- The text to use to create the barcode
        ;"       OPTION -- Array that may hold optional settings, as follows:
        ;"            OPTION("IMAGE TYPE")="jpg"  <-- IF not specified, then default is "png"
        ;"Output: results are passed out in @GREF
        ;"              @GREF@(0)=success;    1=success, 0=failure
        ;"              @GREF@(1..xxx) = actual data

        ;"NOTE: dmtxread must be installed on linux host.
        ;"      I found source code here:
        ;"      http://sourceforge.net/projects/libdmtx/
        ;"      After installing (./configure --> make --> make install), I
        ;"        copied dmtxread and dmtxwrite, which were found in the
        ;"        (installdir)/util/dmtxread/.libs and (installdir)/util/dmtxwrite/.libs
        ;"        folders, into a folder on the system path.  I chose /usr/bin/
        ;"      Also, to achieve compile of above, I had to install required libs.
        ;"      See notes included with dmtx source code.
        ;
        NEW FileSpec
        NEW file
        NEW FNAME,FPath
        ;
        SET GREF="^TMP(""GETBARCD^TMGRPC1"","_$J_")"
        KILL @GREF
        SET @GREF@(0)=""  ;"default to failure
        SET MESSAGE=$GET(MESSAGE)
        IF MESSAGE="" GOTO GBCDone
        ;
        ;"Create the barcode and get file name and path
        SET file=$$MAKEBC^TMGBARC(MESSAGE,.OPTION)
        DO SPLITFPN^TMGIOUTL(file,.FPath,.FNAME,"/")
        ;
        ;"Load binary image into global array
        NEW TEMP SET TEMP=$$BFTG^TMGBINF(.FPath,.FNAME,$name(@GREF@(1)),3)
        IF +TEMP=0 DO
        . ;"decide what to DO with error message
        SET TEMP=+TEMP  ;"for now, will discard until I can see IF the calling software can handle more than plain "0" or "1" 
        SET @GREF@(0)=TEMP
        ;
        ;"convert binary data to ascii encoded data
        DO ENCODE($name(@GREF@(1)),3)
        ;"MERGE ^TMG("TMP","RPC","GETBARCODE")=@GREF
        ;
        ;"delete temp image file
        DO SPLITFPN^TMGIOUTL(file,.FPath,.FNAME,"/")
        SET FileSpec(FNAME)=""
        NEW temp SET temp=$$DEL^%ZISH(FPath,"FileSpec")
        ;
GBCDone ;
        QUIT
        ;
        ;
DECODEBC(RESULT,ARRAY,IMGTYPE) ;
        ;"SCOPE: Public
        ;"RPC that calls this: TMG BARCODE DECODE
        ;"Purpose: To provide an entry point for a RPC call from a client.  The client
        ;"         will upload an image file (.png format only) of a barcode (Datamatrix
        ;"         format) for decoding.  Decoded message is passed back.
        ;"Input:  RESULT -- an OUT PARAMETER.  See output below.
        ;"        ARRAY --   the array that will hold the image file, in BASE64 ascii encoding
        ;"        IMGTYPE -- Image type, e.g. "jpg" (Note: don't include any '.')
        ;"Output: results are passed out in RESULT:  1^Decoded Message   or 0^FailureMessage
        ;
        ;"NOTE: dmtxread must be installed on linux host.
        ;"      I found source code here:
        ;"      http://sourceforge.net/projects/libdmtx/
        ;"      After installing (./configure --> make --> make install), I
        ;"        copied dmtxread and dmtxwrite, which were found in the
        ;"        (installdir)/util/dmtxread/.libs and (installdir)/util/dmtxwrite/.libs
        ;"        folders, into a folder on the system path.  I chose /usr/bin/
        ;"      Also, to achieve compile of above, I had to install required libs.
        ;"      See notes included with dmtx source code.
        ;"NOTE: IF image types other than .png will be uploaded, then the linux host
        ;"     must have ImageMagick utility 'convert' installed for conversion
        ;"     between image types.
        ;
        KILL ^TMG("TMP","BARCODE")
        NEW RESULTMSG
        IF $DATA(ARRAY)=0 SET RESULTMSG="0^No image data received to decode" GOTO DBCDone
        ;
        NEW imageType SET imageType=$$LOW^XLFSTR($GET(IMGTYPE))
        IF imageType=""  SET RESULTMSG="0^Image type not specified" GOTO DBCDone
        ;
        NEW imageFNAME SET imageFNAME="/tmp/barcode."_imageType
        SET imageFNAME=$$UNIQUE^%ZISUTL(imageFNAME)
        NEW FNAME,FPath,FileSpec
        DO SPLITFPN^TMGIOUTL(imageFNAME,.FPath,.FNAME,"/")
        SET FileSpec(FNAME)=""
        ;
        ;"Remove BASE64 ascii encoding
        DO DECODE("ARRAY(0)",1)
        ;
        ;"Save to host file system
        NEW TEMP SET TEMP=$$GTBF^TMGBINF("ARRAY(0)",1,FPath,FNAME)
        IF +TEMP=0 DO  GOTO DBCDone
        . SET RESULTMSG="0^Error while saving file to HFS. "_$PIECE(TEMP,"^",2,99)
        ;
        ;"convert image file to .png format, IF needed
        IF imageType'="png" DO
        . SET imageFNAME=$$Convert^TMGKERNL(imageFNAME,"png")
        . IF imageFNAME="" DO  QUIT
        . . SET RESULTMSG="0^Error while converting image from ."_imageType_" to .png format."
        . DO SPLITFPN^TMGIOUTL(imageFNAME,.FPath,.FNAME,"/")
        . SET FileSpec(FNAME)=""
        IF imageFNAME="" GOTO DBCDone
        ;
        ;"Decode the barcode.png image
        NEW RESULT SET RESULT=$$READBC^TMGBARC(imageFNAME)
        IF RESULT'="" SET RESULTMSG="1^"_RESULT
        ELSE  SET RESULTMSG="0^Unable to Decode Image"
        ;
DBCDone ;
        SET RESULT=RESULTMSG
        QUIT
        ;
 ;"--------------------
GETURLS(RESULT,TMGDFN) ;
        ;"SCOPE: Public
        ;"RPC that calls this: TMG CPRS GET URL LIST
        ;"Purpose: To provide an entry point for a RPC call from a client.  The client
        ;"         will request URLs to display in custom tabs inside CPRS, in an
        ;"         imbedded web browser
        ;"Input:  RESULT -- an OUT PARAMETER.  See output below.
        ;"        TMGDFN -- IEN in PATIENT file.  NOTE: this RPC is also called when
        ;"               patient is CLEARED (i.e. set to NONE).  In that case
        ;"               TMGDFN=""
        ;"Output: results are passed out in RESULT:
        ;"         RESULT(0)="1^Success"   or "0^SomeFailureMessage"
        ;"         RESULT(<TAB#>)="Name1^URL#1"  ; shows URL#1 in tab #1, named 'Name1'
        ;"         RESULT(<TAB#>)="Name2^URL#2"  ; etc.
        ;"         RESULT(<TAB#>)="Name3^URL#3"
        ;"
        ;"        E.g. RESULT(0)="1^Success"
        ;"             RESULT(1)="cnn^www.cnn.com"
        ;"             RESULT(2)="INFO^192.168.0.1/home.html"
        ;"
        ;"       In TMG-CPRS 2015.04.07 and higher, using the tags "{{ws}}"
        ;"          and "{{wp}}" will cause CPRS to resolve them with
        ;"          either shortcut parameters for ws and wp or s and p.
        ;"          ws = web server ip address, as specified on commandline parameters
        ;"          wp = web server port, as specified on commandline parameters
        ;"          ws = server ip address, as specified on commandline parameters
        ;"          wp = server port, as specified on commandline parameters
        ;"       The number of allowed tabs is determined by code in CPRS
        ;"          Reference to tab numbers > specified in CPRS will be ignored by CPRS
        ;"       If a web tab is NOT specified, then the page previously
        ;"          displayed will be left in place.  **It will not be cleared.**
        ;"       To clear a given page, a url of "about:blank" will cause a
        ;"          blank page to be displayed.  e.g.
        ;"            RESULT(3)="^about:blank"
        ;"       To HIDE a tab on CPRS use this:
        ;"            RESULT(3)="^<!HIDE!>"   ;triggers tab #3 to be hidden
        ;"       To have the browser remain UNCHANGED use this:
        ;"            RESULT(3)="^<!NOCHANGE!>"   ;triggers tab #3 to remain unchanged.
        ;"            Note: the rationale for this is that the web tab may have info
        ;"              that should not be refreshed when the patient info is refreshed
        ;"              i.e. the user may have navigated somewhere, and wouldn't want
        ;"              to loose their location.
        ;"            Note: The other way to do this, as above, is to simply have NO
        ;"              entry for a given tab.  I.e. don't have any value for RESULT(3)
        ;"Notice to others:  Below is where code should be added to return
        ;"   proper URL's to CPRS.  This will be called whenever a NEW patient
        ;"   is opened, or a Refresh Information is requested.
        ;"   FYI, 'DFN', IF passed in can be used to pass back URLS specific for
        ;"      a given patient.  However, this RPC is also called when
        ;"      patient is CLEARED (i.e. set to NONE).  In that case DFN=""        
        ;
        SET TMGDFN=+$GET(TMGDFN)
        SET RESULT(0)="1^Success"
        SET ^TMP("TMG","GETURLS")=$GET(TMGDFN)
        NEW IDX SET IDX=1
        ;"SET RESULT(IDX)="Google^http://www.google.com",IDX=IDX+1
        NEW ADDNUM SET ADDNUM=0
        NEW REF SET REF=$NAME(^TMG("TMP","TMGRPC1","DYNURL"))
        FOR  SET ADDNUM=$ORDER(@REF@(ADDNUM)) QUIT:+ADDNUM'>0  DO
        . IF $DATA(@REF@(ADDNUM)) DO
        . . SET RESULT(IDX)=@REF@(ADDNUM),IDX=IDX+1

        ;"SET RESULT(2)="Lab Test^http://{{ws}}:{{wp}}/filesystem/lab/index.html?DFN=9182"
        SET RESULT(IDX)=$$MAKEURL^TMGRST03(TMGDFN),IDX=IDX+1
        ;"SET RESULT(IDX)="yahoo^http://www.yahoo.com",IDX=IDX+1
        ;"SET RESULT(IDX)="Google^http://www.google.com",IDX=IDX+1
        ;"BELOW 1 LINE IS TEMP... REMOVE LATER
        ;"SET RESULT(IDX)="Allscripts^https://eprescribe.allscripts.com/Login.aspx?ReturnUrl=%2f",IDX=IDX+1
        ;"Specify all web tabs not specified above to be hidden/removed in CPRS
        FOR  QUIT:IDX>4  DO  
        . SET RESULT(IDX)="^<!HIDE!>",IDX=IDX+1
        QUIT
        ;
SIGIMAGE(TMGRESULT,DUZ,IMAGEIEN)  ;
        ;"Purpose: Store image as user's signature
        ;"Input: TMGRESULT - Result variable
        ;"       TMGDFN - User to store image to
        ;"       IMAGEIEN - pointer to image in 2005
        NEW TMGFDA,TMGMSG,TMGIEN,RECIEN,TMGIENS
        SET TMGRESULT="1^SUCCESSFUL"
        IF '$DATA(^MAG(2005,IMAGEIEN)) DO  GOTO SIDN
        . SET TMGRESULT="-1^NOT VALID IMAGE"
        SET RECIEN=$ORDER(^TMG(22701,"B",DUZ,0))        
        IF RECIEN'>0 DO
        . SET TMGFDA(22701,"+1,",.01)=DUZ
        . DO UPDATE^DIE("","TMGFDA","RECIEN","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) DO  GOTO SIDN
        . . SET TMGRESULT="-1^Filing Error Occured:"_$GET(TMGMSG("DIERR",1,"TEXT",1))
        . KILL TMGFDA
        SET TMGIENS="+1,"_RECIEN_","
        NEW X
        DO NOW^%DTC
        SET TMGFDA(22701.01,TMGIENS,.01)=IMAGEIEN
        SET TMGFDA(22701.01,TMGIENS,1)=X
        DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO SIDN
        . SET TMGRESULT="-1^Filing Error Occured:"+$GET(TMGMSG("DIERR",1,"TEXT",1))
        KILL TMGFDA
        SET TMGFDA(2005,IMAGEIEN_",",.01)="Signature For "_DUZ
        SET TMGFDA(2005,IMAGEIEN_",",5)="@"
        DO FILE^DIE("E","TMGFDA","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO SIDN
        . SET TMGRESULT="-1^Filing Error Occured:"+$GET(TMGMSG("DIERR",1,"TEXT",1))
SIDN    QUIT
        ;"
GETESIG(TMGDFN)  ;
        ;"Purpose: To return the provided user's last e-signature
        ;"Input: TMGDFN - User's IEN
        ;"Result: Path of image to be appended to an HTML TIU Note
        NEW IMAGEIEN,TMGRESULT,IMAGEARR,MAGIEN,MAGDATE,FILENAME
        SET IMAGEIEN=0,TMGRESULT=""
        NEW ESIGIEN SET ESIGIEN=$ORDER(^TMG(22701,"B",TMGDFN,0))
        IF ESIGIEN'>0 GOTO GESDN
        FOR  SET IMAGEIEN=$ORDER(^TMG(22701,ESIGIEN,1,IMAGEIEN)) QUIT:IMAGEIEN'>0  DO
        . SET MAGIEN=$PIECE($GET(^TMG(22701,ESIGIEN,1,IMAGEIEN,0)),"^",1)
        . SET MAGDATE=$PIECE($GET(^TMG(22701,ESIGIEN,1,IMAGEIEN,0)),"^",2)
        . SET IMAGEARR(MAGDATE)=MAGIEN
        SET MAGDATE=$ORDER(IMAGEARR(9999999),-1)
        IF MAGDATE'>0 GOTO GESDN
        SET MAGIEN=+$GET(IMAGEARR(MAGDATE))
        SET FILENAME=$PIECE($GET(^MAG(2005,MAGIEN,0)),"^",2)
        IF FILENAME="" GOTO GESDN
        ;"SET TMGRESULT="<IMG src=""$CPRSDIR$\Cache\"_FILENAME_""" alt=""Signature will appear when note is signed"">"
        SET TMGRESULT="<IMG src=""$CPRSDIR$\Cache\"_FILENAME_""">"
GESDN   QUIT TMGRESULT
        ;"