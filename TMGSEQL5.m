TMGSEQL5 ;TMG/kst/Interface with SequelSystems PMS for ICD codes ; 9/20/16, 9/24/21
        ;;1.0;TMG-LIB;**1**; 9/20/16
       ;
 ;"TMG SEQUEL FUNCTIONS
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 9/20/16  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;" IMPCPTS
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;
IMPCPTS() ;"IMPORT SEQUEL CPT EXPORT FILE CPT_By_Patient.csv
  ;"NOTE: the fields of the CSV file are as follows. 
  ;"      [1]=plan_short_name
  ;"      [2]=visit_date_from                                                                         
  ;"      [3]=proc_cpt_code
  ;"      [4]=practice_name
  ;"      [5]=location_name
  ;"      [6]=provider_name
  ;"      [7]=total_amount_charged
  ;"      [8]=total_amount_paid
  ;"      [9]=total_adjustment
  ;"      [10]=icd_9_code
  ;"      [11]=modifier       
  ;"      [12]=visit_seq_num
  ;"      [13]=charge_seq_num
  ;"      [14]=patient_seq_num
  ;"      [15]=parent_seq_num
  ;"      [16]=account_num
  ;"      [17]=last_name
  ;"      [18]=first_name
  ;"      [19]=plan_priority_type
  ;"      [20]=plan_id
  ;"      [21]=ref_provider
  ;"      [22]=asst_provider
  ;"      [23]=entry_date
  ;"      [24]=panel_group
  ;"      [25]=num_of_units
  ;"NOTE: There are not alerts created for problems because the PCE API approach
  ;"      we use here doesn't return errors.  It just jobs off the save attempt
  ;"      We could use a lower-level API to get back the potential errors if
  ;"      we want to refactor code.
  NEW TMGARRAY
  DO LCSV2ARR^TMGIOUT4("/mnt/WinServer/CPT_By_Patient.csv","TMGARRAY")
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(TMGARRAY(IDX)) QUIT:+IDX'>0  DO
  . NEW ENTRY MERGE ENTRY=TMGARRAY(IDX)
  . NEW TMGRESULT SET TMGRESULT=$$PROCESS1(.ENTRY)
  . IF +TMGRESULT>-1 QUIT
  . ;"SEND ERROR ALERT HERE...   
  . WRITE TMGRESULT,!  ;"TEMP!!  
  QUIT
  ;     
PROCESS1(ONELINE) ;"Process one line from the CSV file.  
  ;"list	
  ;"	(1)=HDR^0^^6;3161005.142013;A
  ;"	(2)=VST^DT^3161005.142013
  ;"	(3)=VST^PT^74592
  ;"	(4)=VST^HL^6
  ;"	(5)=VST^VC^A
  ;"	(6)=PRV^168^^^Toppenberg,Kevin S^1
  ;"	(7)=POV+^I48.2^Problem List Items^Atrial fibrillation (SCT	49436004)^1^^0^^^1
  ;"	(8)=COM^1^@
  ;"	(9)=CPT+^99213^^Office/Outpatient visit for established Patient requires 2 of 3:  Expanded Problem Focused History/Exam, Low Complexity Medical Decision making. Presenting problem(s) are Low to moderate Severity. 15 min spent face-to-face^1^168^^^0^2^
  ;"	(10)=COM^2^@
  ;"note ien	0
  ;"orloc 	6 <-- for TMG use only
  ;"Result: 1^Success  or -1^Error Message
  NEW PCELIST,NOTEIEN,ORLOC,TMGRESULT,IDX,VC,INPATIENT,VSTSTR,PROV
  SET TMGRESULT="1^SUCCESSFUL"
  SET IDX=1
  SET NOTEIEN=0,ORLOC=6,VC="A",INPATIENT=0  ;"These are for TMG use only
  ;"
  ;" Get info to complete PCELIST                                            
  NEW TMGDFN,DT
  SET TMGDFN=$$FINDDFN(.ONELINE)        
  IF +TMGDFN<1 DO  GOTO PRODN
  . SET TMGRESULT=TMGDFN
  NEW TEMP,DISCARD
  SET TEMP=$$ENSURINS(.ONELINE,.DISCARD,TMGDFN)  ;"Ensure insurance name is a valid record, and linked to patient.
  IF +TEMP<0 SET TMGRESULT=TEMP GOTO PRODN     
  SET DT=$$SEQ2FMDT($GET(ONELINE(2)))
  IF +DT<0 DO  GOTO PRODN
  . SET TMGRESULT=PRODN
  IF DT'["." SET DT=DT_".1000"
  ;"
  SET PROV=$$GETPROV(.ONELINE)
  ;"Format header
  SET VSTSTR=ORLOC_";"_DT_";"_VC
  SET PCELIST(1)="HDR^"_INPATIENT_"^^"_VSTSTR
  SET PCELIST(2)="VST^DT^"_DT
  SET PCELIST(3)="VST^PT^"_TMGDFN
  SET PCELIST(4)="VST^HL^"_ORLOC
  SET PCELIST(5)="VST^VC^"_VC
  SET PCELIST(6)="PRV^"_PROV
  SET IDX=7
  ;"
  ;"ADD ICDs
  NEW ENTRY SET ENTRY=1
  DO ADDICDS(.PCELIST,.IDX,.ONELINE,.ENTRY)
  ;"
  ;"ADD CPTs
  DO ADDCPTS(.PCELIST,.IDX,.ONELINE,.ENTRY)
  ;"
  ;"FILE PCELIST
  ;"note: CONSIDER if this is the correct API or not.  Perhaps won't cause
  ;taskman problems.  DATA2PCE^PXAI() or similar
  DO SAVE^ORWPCE(.TMGRESULT,.PCELIST,.NOTEIEN,.ORLOC)
PRODN ;
  QUIT TMGRESULT
  ;"
FINDDFN(ONELINE)  ;"Try and find patient's DFN
  ;"Input: ONELINE -- PASS BY REFERENCE.  See IMPCPTS() for CSV format
  ;"Result: -1^error or DFN
  NEW TMGRESULT SET TMGRESULT="-1^Unknown error"
  NEW SEQLNUM SET SEQLNUM=+$GET(ONELINE(16))
  NEW LNAME SET LNAME=$GET(ONELINE(17))
  NEW FNAME SET FNAME=$GET(ONELINE(18))
  NEW TMGDFN SET TMGDFN=0
  SET TMGDFN=$ORDER(^DPT("TMGS",SEQLNUM,0))
  IF TMGDFN>0 DO  GOTO ADFNDN
  . ;"NEW NAME SET NAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
  . SET TMGRESULT=TMGDFN
  ;"Unable to identify by patient SEQL number
  ;"NOTE: if needed, I could try to look up by name.  But without DOB,
  ;"      probably not specific enough.
  SET TMGRESULT="-1^Unable to identify unique patient. SEQL#="_SEQLNUM
ADFNDN ;
  QUIT TMGRESULT
  ;
SEQ2FMDT(SEQLDT) ;"CONVERT SEQUEL DATE INTO FMDT
  ;"Input: SEQLDT -- e.g. format: 8/11/2016 00:00:00'
  ;"Results: FMDT version of above.
  NEW TMGRESULT SET TMGRESULT=1
  NEW %DT,X,Y SET %DT="PT"
  SET X=$PIECE(SEQLDT," ",1)
  DO ^%DT
  SET TMGRESULT=Y IF TMGRESULT<0 GOTO SQ2FDN
  NEW TIME SET TIME=$PIECE(SEQLDT," ",2) IF TIME="" GOTO SQ2FDN
  SET TIME=$TRANSLATE(TIME,":","") IF +TIME=0 GOTO SQ2FDN
  SET TMGRESULT=TMGRESULT_"."_TIME
  SET X=TMGRESULT DO ^%DT SET TMGRESULT=Y  ;"Run through again to ensure time addition was OK
SQ2FDN ;
  IF TMGRESULT<0 DO  GOTO SQ2FDN
  . SET TMGRESULT="-1^Unable to convert date ["_SEQLDT_"] to Fileman format."
  QUIT TMGRESULT
  ;
GETPROV(ONELINE)  ;"Find provider and format for PCELIST
  ;"Input: ONELINE as above
  ;"Result: provider formatted for header... THIS IS TMG SPECIFIC INFORMATION 
  NEW TMGRESULT SET TMGRESULT="168^^^Toppenberg,Kevin S^1"
  NEW PROVIDER SET PROVIDER=$GET(ONELINE(6))
  IF PROVIDER="MTOPPEN" SET TMGRESULT="83^^^Toppenberg,Marcia Dee^1"
  QUIT TMGRESULT
  ;"
ADDICDS(PCELIST,IDX,ONELINE,ENTRY)  ;"ADD ICDS TO PCELIST
  ;"    (#)=POV+^I48.2^^^Atrial fibrillation (SCT  ;49436004)^1^^0^^^1
  ;"    (#)=COM^1^@
  NEW ICDLIST SET ICDLIST=$GET(ONELINE(10))
  IF ICDLIST'[" " SET ICDLIST=ICDLIST_" "
  FOR  QUIT:(ICDLIST="")!(ICDLIST=" ")  DO
  . NEW ICD SET ICD=$PIECE(ICDLIST," ",1),ICDLIST=$PIECE(ICDLIST," ",2,99)
  . IF ICD'["." SET ICD=ICD_"."
  . NEW ICDIEN,ICDTEXT
  . SET ICDIEN=+$ORDER(^ICD9("BA",ICD_" ",0))
  . IF ICDIEN'>0 QUIT
  . SET ICDTEXT=$GET(^ICD9(ICDIEN,68,1,1))
  . SET PCELIST(IDX)="POV+^"_ICD_"^^"_ICDTEXT_"^0^^0^^^"_ENTRY
  . SET IDX=IDX+1
  . SET PCELIST(IDX)="COM^"_ENTRY_"^@"
  . SET IDX=IDX+1
  . SET ENTRY=ENTRY+1
  QUIT
  ;"
ADDCPTS(PCELIST,IDX,ONELINE,ENTRY)  ;"ADD CPTS TO PCELIST
  ;"    (9)=CPT+^99213^^Office/Outpatient visit for established Patient requires 2...
  ;"    (10)=COM^2^@  
  NEW CPT SET CPT=$GET(ONELINE(3))
  NEW CPTIEN,CPTTEXT
  SET CPTIEN=+$ORDER(^ICPT("B",CPT,0))
  IF CPTIEN'>0 QUIT
  NEW CPTTEXTIDX SET CPTTEXTIDX=0
  SET CPTTEXT=""
  FOR  SET CPTTEXTIDX=$ORDER(^ICPT(CPTIEN,62,1,1,CPTTEXTIDX)) QUIT:CPTTEXTIDX'>0  DO
  . SET CPTTEXT=CPTTEXT_$GET(^ICPT(CPTIEN,62,1,1,CPTTEXTIDX,0))
  SET PCELIST(IDX)="CPT+^"_CPT_"^^"_CPTTEXT_"^1^168^^^0^"_ENTRY_"^"
  SET IDX=IDX+1
  SET PCELIST(IDX)="COM^"_ENTRY_"^@"
  SET IDX=IDX+1
  QUIT
  ;"
SAVE(OK,PCELIST,NOTEIEN,ORLOC)  ; save PCE information
  ;"COPIED FROM ORWPCE SO IT ISN'T TASKED OFF
  N VSTR,GMPLUSER
  N ZTIO,ZTRTN,ZTDTH,ZTSAVE,ZTDESC,ZTSYNC,ZTSK
  S VSTR=$P(PCELIST(1),U,4) K ^TMP("ORWPCE",$J,VSTR)
  M ^TMP("ORWPCE",$J,VSTR)=PCELIST
  S GMPLUSER=$$CLINUSER^ORQQPL1(DUZ),NOTEIEN=+$G(NOTEIEN)
  S ZTIO="ORW/PXAPI RESOURCE",ZTRTN="DQSAVE^ORWPCE1",ZTDTH=$H
  S ZTSAVE("PCELIST(")="",ZTDESC="Data from CPRS to PCE"
  S ZTSAVE("GMPLUSER")="",ZTSAVE("NOTEIEN")="",ZTSAVE("DUZ")=""
  I VSTR'["E" S ZTSYNC="ORW"_VSTR
  S ZTSAVE("ORLOC")=""
  DO DQSAVE^ORWPCE1 QUIT
  Q
  ;"
ENSURINS(CSV,OUT,INDFN)  ;"Ensure insurance name is a valid record, and link to patient.
  ;"Input: CSV -- PASS BY REFERENCE.  See IMPCPTS() for CSV format
  ;"       OUT -- PASS BY REFERENCE.  AN IN AND OUT PARAMETER.
  ;"           Expected to have OUT("DFN")=DFN^PatientName  <-- only DFN is needed
  ;"       INDFN -- OPTIONAL.  If OUT("DFN") has value, this is ignored
  ;"Result: 1 if OK, or -1^Message if problem.
  NEW SHORTNAME SET SHORTNAME=$GET(CSV(1))
  NEW PLANID SET PLANID=$GET(CSV(20))
  NEW TMGRESULT SET TMGRESULT=1
  NEW TMGDFN SET TMGDFN=+$GET(OUT("DFN"))
  IF TMGDFN'>0 SET TMGDFN=+$GET(INDFN)
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
  SET TMGFDA(2.312,SUBIEN_","_TMGDFN_",",1.03)=$$NOW^XLFDT
  DO FILE^DIE("E","TMGFDA","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO ENSDN
  . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)  
  ;
  ;"Do anything else here needed for insurance sync'ing.
ENSDN  
  QUIT TMGRESULT
  ;
  ;"=====DISREGARD ALL CODE BELOW======
FIXINSR  ;"
  NEW IDX SET IDX=0
  WRITE !,"EDIT NEEDED INSURANCE NAMES",!
  FOR  SET IDX=$ORDER(^DIC(36,IDX)) QUIT:IDX'>0  DO
  . NEW NAME SET NAME=$PIECE($GET(^DIC(36,IDX,0)),"^",1)
  . IF NAME["EDIT THIS" DO
  . . WRITE "What should ",NAME," be called? "
  . . NEW NEWNAME SET NEWNAME=""
  . . READ NEWNAME
  . . IF NEWNAME'="" DO
  . . . SET $PIECE(^DIC(36,IDX,0),"^",1)=NEWNAME
  . . WRITE !
  QUIT
  ;
  ;"... CODE FRAGMENT...  
  NEW DATA,TMGRESULT
  SET TMGRESULT=$$ADDDFN(.CSV,.DATA)    IF +TMGRESULT<0 GOTO P1DN
  SET TMGRESULT=$$ADDLOC(.CSV,.DATA)     IF +TMGRESULT<0 GOTO P1DN  
  SET TMGRESULT=$$ENSRVIST(.CSV,.DATA)  IF +TMGRESULT<0 GOTO P1DN
  SET TMGRESULT=$$ENSURINS(.CSV,.DATA)  IF +TMGRESULT<0 GOTO P1DN
  SET TMGRESULT=$$ADDPROV(.CSV,.DATA)    IF +TMGRESULT<0 GOTO P1DN
  SET TMGRESULT=$$ADDCPT(.CSV,.DATA)     IF +TMGRESULT<0 GOTO P1DN
  SET TMGRESULT=$$ADDICD(.CSV,.DATA)       IF +TMGRESULT<0 GOTO P1DN
  SET TMGRESULT=$$FILEDATA(.DATA)        IF +TMGRESULT<0 GOTO P1DN
P1DN ;  
  QUIT TMGRESULT
  ;
OLDSEQ2FMDT(SEQLDT) ;"CONVERT SEQUEL DATE INTO FMDT
  ;"Input: SEQLDT -- e.g. format: 8/11/2016 00:00:00'
  ;"Results: FMDT version of above.
  NEW TMGRESULT SET TMGRESULT=1
  NEW %DT,X,Y SET %DT="PT"
  SET X=$PIECE(SEQLDT," ",1)
  DO ^%DT
  SET TMGRESULT=Y IF TMGRESULT<0 GOTO SQ2FDN
  NEW TIME SET TIME=$PIECE(SEQLDT," ",2) IF TIME="" GOTO SQ2FDN
  SET TIME=$TRANSLATE(TIME,":","") IF +TIME=0 GOTO SQ2FDN
  SET TMGRESULT=TMGRESULT_"."_TIME
  SET X=TMGRESULT DO ^%DT SET TMGRESULT=Y  ;"Run through again to ensure time addition was OK
OLDSQ2FDN ;  
  IF TMGRESULT<0 DO  GOTO SQ2FDN
  . SET TMGRESULT="-1^Unable to convert date ["_SEQLDT_"] to Fileman format."
  QUIT TMGRESULT  
  ;
ENSRVIST(CSV,OUT) ;"Process date time of CPT event.
  ;"Purpose: This will take date-time of CPT event and get existing VISIT record pointer
  ;"          If VISIT doesn't exist, then will create.
  ;"Input: CSV -- PASS BY REFERENCE.  See IMPCPTS() for CSV format
  ;"       OUT -- PASS BY REFERENCE.  AN IN AND OUT PARAMETER.
  ;"           Expected to have OUT("DFN")=DFN^PatientName
  ;"           Expected to have OUT("LOC")=IEN44^LocationName
  ;"           Will add OUT("VISIT")=VisitIEN^DT^RDT
  ;"Result: 1 if OK, or -1^Message if error.  
  NEW TMGRESULT SET TMGRESULT=1
  NEW TMGDFN SET TMGDFN=+$GET(OUT("DFN")) 
  IF TMGDFN'>0 DO  GOTO EVTDN
  . SET TMGRESULT="-1^Patient DFN not provided to ENSRVIST^TMGSEQL5"
  NEW DT SET DT=$$SEQ2FMDT($GET(CSV(2)))
  IF +DT<0 SET TMGRESULT=DT GOTO EVTDN
  NEW RDT SET RDT=9999999-(DT\1)
  IF DT["." SET RDT=RDT_"."_$PIECE(DT,".",2)
  ;"USING LAST VISIT FOUND ON DATE (FIRST RDT)
  NEW ADT SET ADT=$ORDER(^AUPNVSIT("AA",TMGDFN,RDT-0.000001))
  NEW IEN SET IEN=0
  IF ADT\1=RDT\1 DO   
  . SET IEN=$ORDER(^AUPNVSIT("AA",TMGDFN,ADT,0))
  . SET RDT=ADT
  . SET DT=9999999-(RDT\1)_"."_$PIECE(RDT,".",2)
  IF IEN>0 GOTO EVTDN
  ;"MAKE NEW VISIT RECORD....
  NEW IEN44 SET IEN44=+$GET(OUT("LOC"))
  NEW TMGFDA,TMGIEN,TMGMSG
  SET TMGFDA(9000010,"+1,",.01)=DT
  SET TMGFDA(9000010,"+1,",.02)=$$NOW^XLFDT
  SET TMGFDA(9000010,"+1,",.03)="E"  ;"E=EHR
  SET TMGFDA(9000010,"+1,",.05)=TMGDFN
  SET TMGFDA(9000010,"+1,",.06)=IEN44  ;"Pointer to 9999999.06  <--> 4
  SET TMGFDA(9000010,"+1,",.07)="E"  ;"E=Event (Historical)
  SET TMGFDA(9000010,"+1,",.22)=IEN44
  IF +$GET(DUZ)>0 SET TMGFDA(9000010,"+1,",.23)=+DUZ
  DO UPDATE^DIE("S","TMGFDA","TMGIEN","TMGMSG")  ;"filing data in INTERNAL format
  IF $DATA(TMGMSG("DIERR")) DO  GOTO EVTDN
  . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  . SET IEN=-1
  SET IEN=$GET(TMGIEN(1))
  IF IEN'>0 DO  GOTO EVTDN
  . SET TMGRESULT="-1^Unable to locate IEN of created visit record"
EVTDN ;  
  SET OUT("VISIT")=IEN_"^"_DT_"^"_RDT
  QUIT TMGRESULT
  ;
ADDDFN(CSV,OUT)  ;"Use entry information to add a "DFN" node to OUT array  
  ;"Input: CSV -- PASS BY REFERENCE.  See IMPCPTS() for CSV format
  ;"       OUT -- PASS BY REFERENCE.  AN IN AND OUT PARAMETER.
  ;"           Will add OUT("DFN")=DFN^PatientName
  ;"Result: 1 if OK, or -1^Message if problem. 
  NEW TMGRESULT SET TMGRESULT=1
  NEW SEQLNUM SET SEQLNUM=+$GET(CSV(16))
  NEW LNAME SET LNAME=$GET(CSV(17))
  NEW FNAME SET FNAME=$GET(CSV(18))
  NEW TMGDFN SET TMGDFN=0
  SET TMGDFN=$ORDER(^DPT("TMGS",SEQLNUM,0))
  IF TMGDFN>0 DO  GOTO ADFNDN
  . NEW NAME SET NAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
  . SET OUT("DFN")=TMGDFN_"^"_NAME 
  ;"Unable to identify by patient SEQL number
  ;"NOTE: if needed, I could try to look up by name.  But without DOB, probably not specific enough.  
  SET TMGRESULT="-1^Unable to identify unique patient. SEQL#="_SEQLNUM
  SET TMGRESULT=TMGRESULT_", LNAME="_LNAME_", FNAME="_FNAME
OLDADFNDN ;  
  QUIT TMGRESULT           
  ;               
PROVIEN(PROV)  ;"CONVERT PROVIDER SEQUEL NAME INTO IEN IN NEW PERSON FILE
  ;"Input: PROV -- e.g. KTOPPEN
  ;"Result: IEN200^Name, or  or -1 if not found.  
  NEW TMGRESULT SET TMGRESULT=-1
  NEW IEN200 SET IEN200=+$ORDER(^VA(200,"TMG",PROV,0))
  IF IEN200'>0 GOTO PVDN 
  NEW PROVNAME SET PROVNAME=$PIECE($GET(^VA(200,IEN200,0)),"^",1)
  SET TMGRESULT=IEN200_"^"_PROVNAME
PVDN ;  
  QUIT TMGRESULT
  ;
ADDPROV(CSV,OUT) ;"ADD INFORMATION ABOUT PROVIDER...
  ;"Input: CSV -- PASS BY REFERENCE.  See IMPCPTS() for CSV format
  ;"       OUT -- PASS BY REFERENCE.  AN IN AND OUT PARAMETER.
  ;"           Will add OUT("PROV")=IEN200^ProviderName
  ;"Result: 1 if OK, or -1^Message if problem.
  NEW TMGRESULT SET TMGRESULT=1
  NEW PROV SET PROV=$$PROVIEN($GET(CSV(6))) 
  IF PROV>0 SET OUT("PROV")=PROV
  ELSE  SET TMGRESULT=PROV
  QUIT TMGRESULT
  ;
CPT(CPTNAME) ;"CONVERT CPT NAME INTO POINTER TO FILE 81
  ;"Input: CPTNAME -- e.g. '99215' or 'Q2037'
  ;"Result: POINTER to file 81, or -1^Message if error.
  NEW TMGRESULT SET TMGRESULT=0
  SET TMGRESULT=+$ORDER(^ICPT("B",CPTNAME,0))
  IF TMGRESULT'>0 DO
  . SET TMGRESULT="-1^Unable to find CPT ["_CPTNAME_"] in file 81"
  ELSE  SET TMGRESULT=TMGRESULT_"^"_CPTNAME
  QUIT TMGRESULT
  ;
ADDCPT(CSV,OUT) ;"ADD CPT CODES TO OUT ARRAY
  ;"Input: CSV -- PASS BY REFERENCE.  See IMPCPTS() for CSV format
  ;"       OUT -- PASS BY REFERENCE.  AN IN AND OUT PARAMETER.
  ;"           Will add OUT("CPT")=CPTIEN^CPTNAME
  ;"Result: 1 if OK, or -1^Message if problem.
  NEW TMGRESULT SET TMGRESULT=1
  NEW CPT SET CPT=$$CPT($GET(CSV(3)))
  IF CPT>0 SET OUT("CPT")=CPT
  ELSE  SET TMGRESULT=CPT
  QUIT TMGRESULT
  ;
ICDS(ENTRY,OUT) ;"PARSE LIST OF ICD'S INTO ARRY
  ;"Input: ENTRY -- e.g. 'M89.9 M15.0 H91.10 R03.0'
  ;"       OUT -- PASS BY REFERENCE.  Output format:
  ;"          Will add OUT("ICD",#)=<pointer for ICD code into file 80>^ICD string
  ;"Result: 1 if OK, or -1^Message if problem.
  NEW TMGRESULT SET TMGRESULT=1
  NEW CT SET CT=1
  FOR  QUIT:ENTRY=""  DO  QUIT:TMGRESULT<0
  . NEW AICD SET AICD=$PIECE(ENTRY," ",1),ENTRY=$PIECE(ENTRY," ",2,99)
  . IF AICD="" QUIT
  . IF AICD'["." SET AICD=AICD_"."
  . NEW IEN80 SET IEN80=$ORDER(^ICD9("BA",AICD_" ",0))
  . IF IEN80'>0 DO  QUIT
  . . SET TMGRESULT="-1^Unable to find ["_AICD_"] if file 80"
  . SET OUT("ICD",CT)=IEN80_"^"_AICD,CT=CT+1
  QUIT TMGRESULT
  ;
ADDICD(CSV,OUT) ;"ADD ICD INFO INTO ARRAY  
  NEW ICDCODES SET ICDCODES=$GET(CSV(10))
  NEW TMGRESULT SET TMGRESULT=$$ICDS(ICDCODES,.OUT)
  QUIT TMGRESULT
  ;
ADDLOC(CSV,OUT)  ;"ADD VISIT LOCATION INFORMATION
  ;"Input: CSV -- PASS BY REFERENCE.  See IMPCPTS() for CSV format
  ;"       OUT -- PASS BY REFERENCE.  AN IN AND OUT PARAMETER.
  ;"           Will add OUT("LOC")=IEN44^LocationName
  ;"NOTE: This depends on custome field 22700 "TMG PMS NAME" which must match
  ;"        name passed in CSV(5)
  ;"Result: 1 if OK, or -1^Message if problem.
  NEW TMGRESULT SET TMGRESULT=1
  NEW LOCNAME SET LOCNAME=$GET(CSV(5))
  NEW IEN44 SET IEN44=+$ORDER(^SC("TMGPMS",LOCNAME,0))
  IF IEN44>0 DO                   
  . NEW NAME SET NAME=$PIECE($GET(^SC(IEN44,0)),"^",1)
  . SET OUT("LOC")=IEN44_"^"_NAME
  ELSE  DO
  . SET TMGRESULT="-1^Unable to find HOSPITAL LOCATION (file #44) to match ["_LOCNAME_"]"
  QUIT TMGRESULT
  ;
FILEDATA(DATA)  ;"ENSURE DATA FROM DATA IS FILED INTO DATABASE
  ;"Input: DATA. PASS BY REFERENCE.  Format:
  ;"          DATA("CPT")=IEN^NAME
  ;"          DATA("DFN")=IEN^NAME
  ;"          DATA("LOC")=IEN44^LocationName
  ;"          DATA("ICD",#)=IEN80^NAME
  ;"          DATA("PROV")=IEN200^ProviderName
  ;"          DATA("VISIT")=VisitIEN
  ;"Result: 1 if OK, or -1^Message if problem.
  NEW TMGRESULT SET TMGRESULT=1
  NEW TMGDFN SET TMGDFN=+$GET(DATA("DFN"))
  IF TMGDFN'>0 DO  GOTO FDDN
  . SET TMGRESULT="-1^Patient DFN not provided to FILEDATA^TMGSEQL5"
  NEW CPTIEN SET CPTIEN=+$GET(DATA("CPT"))  
  IF CPTIEN'>0 DO  GOTO FDDN
  . SET TMGRESULT="-1^CPT IEN not provided to FILEDATA^TMGSEQL5"
  NEW VIEN SET VIEN=+$GET(DATA("VISIT"))
  IF VIEN'>0 DO  GOTO FDDN
  . SET TMGRESULT="-1^Visit IEN not provided to FILEDATA^TMGSEQL5"
  NEW RDT SET RDT=$PIECE($GET(DATA("VISIT")),"^",3)
  IF RDT'>0 DO  GOTO FDDN
  . SET TMGRESULT="-1^RDT not provided to FILEDATA^TMGSEQL5"
  NEW PROV SET PROV=+$GET(DATA("PROV"))
  IF PROV'>0 DO  GOTO FDDN
  . SET TMGRESULT="-1^Provider IEN not provided to FILEDATA^TMGSEQL5"
  ;"AA XREF IS: <"AA",IEN2,IEN81,RDT,DA> 
  IF $DATA(^AUPNVCPT("AA",TMGDFN))=0 GOTO ADD
  IF $DATA(^AUPNVCPT("AA",TMGDFN,CPTIEN))=0 GOTO ADD
  IF $DATA(^AUPNVCPT("AA",TMGDFN,CPTIEN,RDT\1))=0 GOTO ADD
  NEW IEN SET IEN=$ORDER(^AUPNVCPT("AA",TMGDFN,CPTIEN,RDT\1,0))
  IF IEN>0 GOTO FD2
ADD ;  
  ;"ADD NEW RECORD IN V CPT FILE.
  NEW IDX,DXFLDS SET DXFLDS(1)=.05
  FOR IDX=2:1:8 SET DXFLDS(IDX)=+("."_$$RJ^XLFSTR(IDX+7,2,"0"))  
  NEW TMGFDA,TMGIEN,TMGMSG
  SET TMGFDA(9000010.18,"+1,",.01)=CPTIEN
  SET TMGFDA(9000010.18,"+1,",.02)=TMGDFN  ;" 9000001 <--> PATIENT FILE
  SET TMGFDA(9000010.18,"+1,",.03)=VIEN
  FOR IDX=1:1:8 QUIT:$DATA(DATA("ICD",IDX))=0  DO
  . NEW FLD SET FLD=DXFLDS(IDX)
  . NEW VALUE SET VALUE=+$GET(DATA("ICD",IDX))
  . SET TMGFDA(9000010.18,"+1,",FLD)=VALUE
  SET TMGFDA(9000010.18,"+1,",1204)=PROV
  ;"Below is hard coded for TMG SEQUEL PMS INTERFACE (`37)
  ;"This field is not really needed, and it can be changed if needed.
  SET TMGFDA(9000010.18,"+1,",81203)=37
  DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG") ;"USING INTERNAL FORMAT 
  IF $DATA(TMGMSG("DIERR")) DO  GOTO FDDN
  . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)  
  SET IEN=$GET(TMGIEN(1))
FD2 ;
  ;"Do anything additional here. 
FDDN ;  
  QUIT TMGRESULT
  