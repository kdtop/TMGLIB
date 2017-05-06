TMGHRPC3        ;TMG/elh/Support Functions for TMG_CPRS ;11/17/10; 11/17/10, 2/2/14
                ;;1.0;TMG-LIB;**1**;10/17/10;Build 3
        ;
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
        ;"The following functions are used solely for Intracare Hospital.
        ;
        ;"=======================================================================
        ;" RPC -- Public Functions.
        ;"=======================================================================
        ;" GETKEENE(OUT,DFN)
        ;" SETPERKN(OUT,DFN,ACCOUNT)
        ;" SETADMKN(OUT,DFN,ACCOUNT,DATE)
        ;" EXISTFS(OUT)
        ;"=======================================================================
        ;"PRIVATE API FUNCTIONS
        ;"=======================================================================
        ;" ;
        ;"=======================================================================
        ;"Dependencies:
        ;" ;
        ;"=======================================================================
        ;
GETKEENE(OUT,DFN)     ;
        ;"
        ;"Purpose: To return the Intracare Keene Account Numbers, based on DFN
        ;"Input: DFN - Patient's DFN number
        ;"Output: OUT - KEENE PERSONAL ACCT #^KEENE ADMISSION ACCT #^DATE OF ADMISSION^ICN Checksum
        ;"Results: none
        NEW IEN,ADMDATA,Y
        SET OUT=0
        SET OUT=$GET(^DPT(DFN,"TMGZ"))  ;"GET PERSONAL ACCT NUMBER
        SET IEN=$ORDER(^DPT(DFN,"TMGZ2","TMGKDT","Z"),-1) ;"GET LAST ADMISSION ENTRY DATE
        IF IEN'="" DO
        . SET IEN=$ORDER(^DPT(DFN,"TMGZ2","TMGKDT",IEN,0)) ;" GET IEN
        . SET ADMDATA=$GET(^DPT(DFN,"TMGZ2",IEN,0))  ;"GET DATA
        . SET Y=$PIECE(ADMDATA,"^",2)
        . ;"X ^DD("DD")
        . SET OUT=OUT_"^"_$PIECE(ADMDATA,"^",1)_"^"_Y
        QUIT
       ;"
GETICNCK(OUT,DFN)   ;
        ;"
        ;"Purpose: To return the ICN ChecksuM, based on DFN
        ;"Input: DFN - Patient's DFN number
        ;"Output: OUT - ICN Checksum or 0
        ;"Results: none
        NEW ICNCKSUM
        SET OUT=+$PIECE($GET(^DPT(DFN,"MPI")),"^",2)
        QUIT
SETPERKN(OUT,DFN,ACCOUNT)    ;
        ;"
        ;"Purpose: To SET the Intracare Personal Static Keene Number
        ;"Input: DFN - Patient's DFN
        ;"       ACCOUNT - Keene account number
        ;"               - The account number is to be static, but the server assumes the client has
        ;"               - verified the change with the user.
        ;"Output: N/A
        ;"Results: OUT - 1^Successful or -1^Error Message
        SET OUT="1^SUCCESSFUL"
        NEW TMGFDA,TMGMSG
        SET TMGFDA(2,DFN_",",19005.1)=ACCOUNT
        DO FILE^DIE("","TMGFDA","TMGMSG")
        QUIT
        ;"
SETADMKN(OUT,DFN,ACCOUNT,DATE)    ;
        ;"
        ;"Purpose: To SET the Intracare Admission Keene Number
        ;"Input: DFN - Patient's DFN
        ;"       ACCOUNT - Keene account number
        ;"       DATE - The Date of the Admission
        ;"Output: N/A
        ;"Results: OUT - 1^Successful or -1^Error Message
        SET OUT="1^SUCCESSFUL"
        NEW TMGFDA,TMGMSG,TMGIEN
        SET TMGFDA(2.190052,"+1,"_DFN_",",.01)=ACCOUNT
        SET TMGFDA(2.190052,"+1,"_DFN_",",1)=DATE
        DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  QUIT
        . SET OUT="-1^"_$$FMERRSTR(.TMGMSG)
        QUIT
        ;
FMERRSTR(ERRARRAY)
        QUIT $$GETERRST^TMGDEBU2(.ERRARRAY)
        ;
EXISTFS(TMGRESULT)    ;"
        ;"Purpose: To determine if the TIU Template "FaceSheet" exists
        ;"Output: TMGRESULT > 0 is yes, 0 if no
        SET TMGRESULT=+$ORDER(^TIU(8927,"B","FACESHEET",0))
        QUIT  ;
        ;"
HTMLFACE(TMGRESULT,PATIENTDFN) ;"
        SET TMGRESULT(0)="<!DOCTYPE HTML PUBLIC ""-//WC3//DTD HTML 3.2//EN"">"
        SET TMGRESULT(1)="TEST<br>LINE 2"
        QUIT  
        ;"

