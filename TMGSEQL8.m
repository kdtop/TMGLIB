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
IMPPAYAVG  ;
  ;"IMPORT PAYMENT DATA FROM SEQUEL AND STORE AVERAGE PAYMENTS INTO VISTA
  ;"plan_short_name,visit_date_from,proc_cpt_code,practice_name,location_name,provider_name,total_amount_charged,total_amount_paid,total_adjustment,icd_9_code,modifier,visit_seq_num,charge_seq_num,patient_seq_num,parent_seq_num,ref_provider,mhrvs
  ;"PARSE CSV FROM SEQUEL
  NEW ARRAY,OPTION,OUTARR
  NEW IDX SET IDX=1
  NEW INSARRAY
  NEW PLAN,PAID,CPT,VISIT
  NEW DATE SET DATE=$$TODAY^TMGDATE
  DO HFS2ARR^TMGIOUT3("/mnt/WinServer","TEST2.csv","ARRAY",.OPTION)
  FOR  SET IDX=$O(ARRAY(IDX)) QUIT:IDX'>0  DO
  . NEW LINE SET LINE=$G(ARRAY(IDX))
  . SET PLAN=$P(LINE,",",1),PAID=$P(LINE,",",8),CPT=$P(LINE,",",3),VISIT=$P(LINE,",",12)
  . IF PAID["." SET PAID=+$P(PAID,".",1)
  . IF PAID'>0 QUIT
  . SET INSARRAY(PLAN,CPT,VISIT)=PAID
  SET PLAN=""
  ;"CALCULATE AVERAGES AND STORE
  NEW TMGFDA,TMGIEN,TMGMSG,TMGIENS
  FOR  SET PLAN=$O(INSARRAY(PLAN)) QUIT:PLAN=""  DO
  . WRITE "GOING THROUGH PLAN ",PLAN,!
  . NEW PLANIEN SET PLANIEN=+$O(^DIC(36,"C",PLAN,0))
  . ;"IF PLANIEN'>0 SET PLANIEN=+$O(^DIC(36,"B",PLAN_" (EDIT THIS NAME)",0))
  . IF PLANIEN'>0 WRITE "CANNOT FIND ",PLAN,! QUIT
  . ;"SAVE TODAY'S DATE IN PLAN
  . K TMGIENS,TMGFDA,TMGMSG
  . SET TMGIENS="+1,"_PLANIEN_","
  . SET TMGFDA(36.01,TMGIENS,.01)=DATE
  . DO UPDATE^DIE("","TMGFDA","TMGIENS","TMGMSG")
  . IF $D(TMGMSG("DIERR")) QUIT
  . NEW DATEIEN SET DATEIEN=+$G(TMGIENS(1))
  . ;"IF DATEIEN'>0 WRITE "NO DATE CAN BE STORED",! QUIT
  . SET CPT=0
  . FOR  SET CPT=$O(INSARRAY(PLAN,CPT)) QUIT:CPT'>0  DO
  . . WRITE "    ->",CPT
  . . SET VISIT=0
  . . NEW TOTCPTS,TOTPAID,HIGH,LOW
  . . SET (TOTCPTS,TOTPAID,HIGH)=0,LOW=999
  . . FOR  SET VISIT=$O(INSARRAY(PLAN,CPT,VISIT)) QUIT:VISIT'>0  DO
  . . . SET PAID=$G(INSARRAY(PLAN,CPT,VISIT))
  . . . IF PAID>HIGH SET HIGH=PAID
  . . . IF PAID<LOW SET LOW=PAID
  . . . SET TOTCPTS=TOTCPTS+1
  . . . SET TOTPAID=TOTPAID+PAID
  . . NEW AVERAGE SET AVERAGE=TOTPAID/TOTCPTS
  . . SET AVERAGE=$J(AVERAGE,0,2)
  . . WRITE " ",AVERAGE,!
  . . ;"SET OUTARR(PLAN,CPT)=AVERAGE
  . . WRITE PLAN,"-",CPT,"-",AVERAGE,"-",HIGH,"-",LOW,!
  . . K TMGIENS,TMGFDA,TMGMSG
  . . SET TMGIENS="+1,"_DATEIEN_","_PLANIEN_","
  . . SET TMGFDA(36.11,TMGIENS,.01)=CPT
  . . SET TMGFDA(36.11,TMGIENS,1)=AVERAGE
  . . SET TMGFDA(36.11,TMGIENS,1.1)=HIGH
  . . SET TMGFDA(36.11,TMGIENS,1.2)=LOW
  . . DO UPDATE^DIE("","TMGFDA","TMGIENS","TMGMSG")
  . . IF $DATA(TMGMSG("DIERR")) DO
  . . . WRITE $$GETERSTR^TMGRPC3G(.TMGMSG),!
  QUIT
  ;"
GETINSAV(TMGRESULT,TMGDFN)  ;"
  ;"THIS RPC WILL TAKE THE PATIENT DFN AND 
  ;"    DETERMINE THE AVERAGE INSURACE PAYMENTS THEN 
  ;"    RETURN AN ARRAY OF THE AVERAGE REIMBURSMENTS
  ;"    FOR EACH CPT
  ;"FOR NOW THE CHARGE WILL BE SET HERE
  NEW CHARGEARR
  SET CHARGEARR(99213)=100
  SET CHARGEARR(99214)=150
  SET CHARGEARR(99215)=210
  SET CHARGEARR(99441)=75
  SET CHARGEARR(99442)=112
  SET CHARGEARR(99443)=171
  SET CHARGEARR(99393)=131
  SET CHARGEARR(99394)=135
  SET CHARGEARR(99395)=159
  SET CHARGEARR(99396)=177
  SET CHARGEARR(99397)=196
  NEW INSIDX,INSIEN 
  SET INSIDX=0,INSIEN=0
  FOR  SET INSIDX=$O(^DPT(TMGDFN,.312,INSIDX)) QUIT:(INSIDX'>0)!(INSIEN'=0)  DO
  . NEW ZN SET ZN=$G(^DPT(TMGDFN,.312,INSIDX,0))
  . IF $P(ZN,"^",20)'=1 QUIT
  . SET INSIEN=$P(ZN,"^",1)
  IF INSIEN'>0 QUIT ;"PATIENT DOESN'T HAVE PRIMARY INSURANCE
  NEW DATESTORED 
  SET DATESTORED=$O(^DIC(36,INSIEN,1,"B",9999999),-1) ;"GET LATEST
  IF DATESTORED="" QUIT  ;"NO DATE STORED FOUND
  NEW DATEIEN SET DATEIEN=$O(^DIC(36,INSIEN,1,"B",DATESTORED,0))
  NEW CPTIEN SET CPTIEN=0
  FOR  SET CPTIEN=$O(^DIC(36,INSIEN,1,DATEIEN,1,CPTIEN)) QUIT:CPTIEN'>0  DO
  . NEW ZN SET ZN=$G(^DIC(36,INSIEN,1,DATEIEN,1,CPTIEN,0))
  . NEW CPT SET CPT=$P(ZN,"^",1)
  . NEW AVG SET AVG=$P(ZN,"^",2)
  . NEW HIGH SET HIGH=$P(ZN,"^",3)
  . NEW LOW SET LOW=$P(ZN,"^",4)
  . SET TMGRESULT(CPTIEN)=CPT_"^"_+$G(CHARGEARR(CPT))_"^"_AVG_"^"_HIGH_"^"_LOW
  QUIT
  ;"