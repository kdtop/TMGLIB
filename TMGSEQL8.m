TMGSEQL8 ;TMG/KST - Handle insurance importing (From SequelPMS);11/06/17, 3/24/21
        ;;1.0;TMG-LIB;**1**; 11/6/17
       ;
 ;"TMG SEQUEL PMS FUNCTIONS -- Importing insurances
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
 ;"
 ;
 ;"=======================================================================
 ;" Private Functions.
 ;"=======================================================================
 ;"
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;" TMGIOUTL,TMGSEQE1,TMGIOUT4
 ;"=======================================================================
 ;
ENSURINS(SHORTNAME,PLANID,COB,TMGDFN)  ;"Ensure insurance name is a valid record, and link to patient.
  ;"Input: SHORTNAME - Name of insurance
  ;"       PLANID - Patient's ID
  ;"       COB - Coordination of benefits
  ;"       TMGDFN -- Patient IEN
  ;"Result: 1 if OK, or -1^Message if problem.
  ;"NEW SHORTNAME SET SHORTNAME=$GET(CSV(1))
  ;"NEW PLANID SET PLANID=$GET(CSV(20))
  NEW TMGRESULT SET TMGRESULT=1
  ;"NEW TMGDFN SET TMGDFN=+$GET(OUT("DFN"))
  ;"IF TMGDFN'>0 SET TMGDFN=+$GET(INDFN)
  IF TMGDFN'>0 DO  GOTO ENSDN
  . SET TMGRESULT="-1^Patient DFN not provided to ENSURINS^TMGSEQL5"
  NEW IEN36 SET IEN36=+$ORDER(^DIC(36,"C",SHORTNAME,0))
  IF IEN36>0 GOTO ENIS1
  ;"Make new record for insurance company
  NEW LONGNAME SET LONGNAME=SHORTNAME_" (EDIT THIS NAME)"
  NEW TMGFDA,TMGIEN,TMGMSG
  SET TMGFDA(36,"+1,",.01)=LONGNAME
  SET TMGFDA(36,"+1,",.111)="(edit this address)" ;"required field
  SET TMGFDA(36,"+1,",1)="N"  ;"required field N=Will not reimburse
  SET TMGFDA(36,"+1,",2)="N"  ;"required field N=signature not required
  DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO ENSDN
  . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  SET IEN36=$GET(TMGIEN(1))
  ;"now add short name as a synonym
  KILL TMGFDA,TMGIEN,TMGMSG
  SET TMGFDA(36.03,"+1,"_IEN36_",",.01)=SHORTNAME
  DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO ENSDN
  . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
ENIS1  ;"ENSURE PATIENT IS LINKED TO INSURANCE COMPANY
  NEW SUBIEN SET SUBIEN=+$ORDER(^DPT(TMGDFN,.312,"B",IEN36,0))
  NEW SUBRECPLANID SET SUBRECPLANID=""
  IF SUBIEN>0 GOTO ENIS2
  ;"ADD INSURANCE SUBRECORD
  KILL TMGFDA,TMGIEN,TMGMSG
  SET TMGFDA(2.312,"+1,"_TMGDFN_",",.01)=IEN36
  DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO ENSDN
  . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  SET SUBIEN=$GET(TMGIEN(1))
  IF SUBIEN'>0 DO  GOTO ENSDN
  . SET TMGRESULT="-1^Unable to find subrec IEN in ENSURINS^TMGSEQL5"
ENIS2  ;"ENSURE SUBFILE RECORD HAS MATCHING PLAN ID
  ;"NOTE: FIELD 1,'SUBSCRIBER ID' is marked for deletion as of 2015
  ;"  So I am going to use 5.01 'PATIENT ID' to store Sequel plan ID
  SET SUBRECPLANID=$PIECE($GET(^DPT(TMGDFN,.312,SUBIEN,5)),"^",1)
  IF SUBRECPLANID=PLANID GOTO ENIS3
  ;"Here I will just put the plan ID in that Sequel provides.
  ;"But if the changes in the future, should a new subrecord be
  ;"created instead of overwriting?
  KILL TMGFDA,TMGMSG
  SET TMGFDA(2.312,SUBIEN_","_TMGDFN_",",5.01)=PLANID
  DO FILE^DIE("E","TMGFDA","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO ENSDN
  . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
ENIS3 ;
  ;"Update the last verified date
  KILL TMGFDA,TMGMSG
  SET COB=+$G(COB)
  IF COB'>0 SET COB=1
  SET TMGFDA(2.312,SUBIEN_","_TMGDFN_",",1.03)=$$NOW^XLFDT
  SET TMGFDA(2.312,SUBIEN_","_TMGDFN_",",.2)=COB
  DO FILE^DIE("E","TMGFDA","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO ENSDN
  . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  ;
  ;"Do anything else here needed for insurance sync'ing.
ENSDN
  QUIT TMGRESULT
  ;
DELINS(TMGDFN)  ;" Delete all insurances
  NEW INSIDX SET INSIDX=0
  FOR  SET INSIDX=$ORDER(^DPT(TMGDFN,.312,INSIDX)) QUIT:INSIDX'>0  DO
  . SET TMGFDA(2.312,INSIDX_","_TMGDFN_",",.01)="@"
  . DO FILE^DIE("E","TMGFDA","TMGMSG")
  QUIT
  ;"