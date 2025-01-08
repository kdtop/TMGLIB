TMGSSQL3 ;TMG/kst/Interface with SequelSystems SQL PMS for ICD codes ; 12/27/24
        ;;1.0;TMG-LIB;**1**; 12/27/24
       ;
       ;"NOTE: Function was copied from TMGSEQL5.m
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
IMPCPTS(LOGFILE) ;"IMPORT SEQUEL CPT EXPORT FILE CPT_By_Patient.csv
  ;"NOTE: the fields of the CSV file are as follows. 
  ;"      [1]=pd.PATIENT_SEQ_NUM
  ;"      [2]=pd.PATIENT_PRACTICE
  ;"      [3]=pd.LAST_NAME
  ;"      [4]=pd.FIRST_NAME
  ;"      [5]=pd.ACCOUNT_NUM
  ;"      [6]=pd.DOB
  ;"      [7]=c.VISIT_DATE_FROM
  ;"      [8]=c.CPT_CODE
  ;"      [9]=c.ICD_9_CODE1
  ;"      [10]=c.ICD_9_CODE2
  ;"      [11]=c.ICD_9_CODE3
  ;"      [12]=c.ICD_9_CODE4
  ;"      [13]=c.ENTERED_BY
  ;"NOTE: There are no alerts created for problems because the PCE API approach
  ;"      we use here doesn't return errors.  It just jobs off the save attempt
  ;"      We could use a lower-level API to get back the potential errors if
  ;"      we want to refactor code.
  NEW SAVEFG SET SAVEFG=0  ;"SET TO 1 FOR FOREGROUND, 0 FOR TASKING OFF
  DO LOGIMPM("BEGINNING ENCOUNTER IMPORT")
  NEW TMGARRAY
  NEW FULLPATHNAME SET FULLPATHNAME=$$SQLDIRECTORY^TMGSSQL1()_$$ENCOUNTF^TMGSSQL1()_$$DATAFILETYPE^TMGSSQL1()
  DO LCSV2ARR^TMGIOUT4(FULLPATHNAME,"TMGARRAY")
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(TMGARRAY(IDX)) QUIT:+IDX'>0  DO
  . NEW ENTRY MERGE ENTRY=TMGARRAY(IDX)
  . DO LOGIMPM("PROCESSING LINE #"_IDX)
  . NEW TMGRESULT SET TMGRESULT=$$PROCESS1(.ENTRY,SAVEFG)
  . DO LOGLINE^TMGSSQL1(LOGFILE,$P(TMGRESULT,"^",2),0,0)
  . IF +TMGRESULT>-1 QUIT
  . ;"SEND ERROR ALERT HERE...   
  . WRITE TMGRESULT,!  ;"TEMP!!
  IF $$DELFILE^TMGIOUTL(FULLPATHNAME)=0 DO LOGLINE^TMGSSQL1(LOGFILE,"ERROR DELETING CSV FILE",0,0)
  DO LOGIMPM("COMPLETED ENCOUNTER IMPORT")
  QUIT
  ;"
PROCESS1(ONELINE,SAVEFG) ;"Process one line from the CSV file.
  ;"Input: ONELINE
  ;"       SAVEFG -- if 1, then saves in foreground, otherwise tasks off.  
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
  ;"SET TEMP=$$ENSURINS(.ONELINE,.DISCARD,TMGDFN)  ;"Ensure insurance name is a valid record, and linked to patient.
  ;"IF +TEMP<0 SET TMGRESULT=TEMP GOTO PRODN     
  SET DT=$$SEQ2FMDT($GET(ONELINE(7)))
  IF +DT<0 DO  GOTO PRODN
  . SET TMGRESULT=PRODN
  ;"//KT: Here I should see if a visit already exists on given date, and change date to that...
  ;"Find first DT in index for given day, that contains a time.  
  NEW TMP,ADT,NEWDT SET ADT=(DT\1)-0.00000001,NEWDT=0
  FOR  SET ADT=$ORDER(^AUPNVSIT("ATMGPTDT",TMGDFN,ADT)) QUIT:(ADT'>0)!((ADT\1)'=(DT\1))!(NEWDT>0)  DO
  . IF ADT'["." QUIT
  . SET NEWDT=ADT
  IF NEWDT>0 SET DT=NEWDT
  ;"
  IF DT'["." SET DT=DT_".1000"
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
  IF $$PRIORFILE(.PCELIST) DO  GOTO PRODN
  . SET TMGRESULT="-1^"_$GET(ONELINE(3))_","_$GET(ONELINE(4))_" "_VSTSTR_" ALREADY EXISTS"
  ;
  NEW TMGPCESRC SET TMGPCESRC="TMG SEQUEL PMS INTERFACE"  
  ;"FILE PCELIST
  ;"NOTE: DATA2PCE^PXAI() would be a better API, but we would have to rewrite this entire code.  SAVE^ORWPCE ultimately calls DATA2PCE
  ;"NOTE: 8/5/22.  We tried having saves occur in foreground task (not tasking off), but this
  ;"      causes hangs as each element was trying to get database lock, and everything was getting very slow.
  ;"      When tasking off, there are thousands of tasks generated, but Taskman is typically
  ;"      able to process them all in 20 minutes or so.  SO, leave code to allow tasking off.  
  DO SAVE(.TMGRESULT,.PCELIST,.NOTEIEN,.ORLOC,.SAVEFG)   ;"Call the save below for now  2/2/23
  SET TMGRESULT="1^SAVED "_VSTSTR_" FOR "_$GET(ONELINE(3))_","_$GET(ONELINE(4))
PRODN ;
  QUIT TMGRESULT
  ;"
SAVE(OK,PCELIST,NOTEIEN,ORLOC,SAVEFG)	;" save PCE information -- COPIED FROM ORWPCE.m, so that we can
    ;" make tweaks as needed for now. Once our saves are solid we can add back
	N VSTR,GMPLUSER
	N ZTIO,ZTRTN,ZTDTH,ZTSAVE,ZTDESC,ZTSYNC,ZTSK
	S VSTR=$P(PCELIST(1),U,4) 
	K ^TMP("ORWPCE",$J,VSTR)
	M ^TMP("ORWPCE",$J,VSTR)=PCELIST
	S GMPLUSER=$$CLINUSER^ORQQPL1(DUZ)
	S NOTEIEN=+$G(NOTEIEN)
	S ZTIO="ORW/PXAPI RESOURCE"
	S ZTRTN="DQSAVE^ORWPCE1"
	S ZTDTH=$H
	S ZTDESC="Data from CPRS to PCE"
	S ZTSAVE("PCELIST(")=""
	S ZTSAVE("GMPLUSER")=""
	S ZTSAVE("NOTEIEN")=""
	S ZTSAVE("DUZ")=""
	I VSTR'["E" S ZTSYNC="ORW"_VSTR
	S ZTSAVE("ORLOC")=""
    S ZTSAVE("TMGPCESRC")="TMG SEQUEL PMS INTERFACE"  ;"//kt 4/14/23.  Used in SAVE^ORWPCE1  
	IF $GET(SAVEFG)=1 DO DQSAVE^ORWPCE1 QUIT   ;"//kt
	;"DO DQSAVE^ORWPCE1 QUIT  ;//elh 1/17/23 - added to force immediate save, so the Health Factors are there when calling TMG GET ORDERED LABS
	D ^%ZTLOAD I '$D(ZTSK) D DQSAVE^ORWPCE1
	Q
	;"
PRIORFILE(PCELIST)  ;"Determine if CPT data here already in system (prior filing)
  NEW TMGRESULT SET TMGRESULT=0
  NEW TMGDFN SET TMGDFN=$P($G(PCELIST(3)),"^",3)
  NEW DATE SET DATE=$P($G(PCELIST(2)),"^",3)\1
  NEW CPT,IDX,DONE SET IDX=0,DONE=0,CPT=""
  FOR  SET IDX=$O(PCELIST(IDX)) QUIT:(IDX'>0)!(DONE=1)  DO
  . NEW LINE SET LINE=$G(PCELIST(IDX))
  . IF $P(LINE,"^",1)'="CPT+" QUIT
  . SET CPT=$P(LINE,"^",2),DONE=1
  IF CPT="" QUIT
  NEW CPTIDX SET CPTIDX=0
  FOR  SET CPTIDX=$O(^AUPNVCPT("C",TMGDFN,CPTIDX)) QUIT:(CPTIDX'>0)!(TMGRESULT=1)  DO
  . NEW THISCPT,THISVISIT
  . SET THISCPT=$P($G(^AUPNVCPT(CPTIDX,0)),"^",1)
  . SET THISCPT=$P($G(^ICPT(THISCPT,0)),"^",1)
  . SET THISVISIT=$P($G(^AUPNVCPT(CPTIDX,0)),"^",3)
  . SET THISVISIT=$P($G(^AUPNVSIT(THISVISIT,0)),"^",1)\1
  . IF (THISCPT=CPT)&(THISVISIT=DATE) DO
  . . SET TMGRESULT=1
  . . WRITE "   !!SKIPPING!!  ",!
  QUIT TMGRESULT
  ;
FINDDFN(ONELINE)  ;"Try and find patient's DFN
  ;"Input: ONELINE -- PASS BY REFERENCE.  See IMPCPTS() for CSV format
  ;"Result: -1^error or DFN
  NEW TMGRESULT SET TMGRESULT="-1^Unknown error"
  NEW SEQLNUM SET SEQLNUM=+$GET(ONELINE(5))
  NEW LNAME SET LNAME=$GET(ONELINE(3))
  NEW FNAME SET FNAME=$GET(ONELINE(4))
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
  NEW TMGRESULT SET TMGRESULT=-1 
  SET SEQLDT=$$TRIM^XLFSTR(SEQLDT)
  NEW MM,DD,YYYY SET MM=+$P(SEQLDT,"/",1),DD=+$P(SEQLDT,"/",2),YYYY=$P(SEQLDT,"/",3)
  IF $L(MM)=1 SET MM="0"_MM
  IF $L(DD)=1 SET DD="0"_DD
  SET YYYY=YYYY-1700
  SET TMGRESULT=YYYY_MM_DD
  QUIT TMGRESULT
  ;"old code below
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
  NEW PROVIDER SET PROVIDER=$GET(ONELINE(13))
  IF PROVIDER="MTOPPEN" SET TMGRESULT="83^^^Toppenberg,Marcia Dee^1"
  QUIT TMGRESULT
  ;"
ADDICDS(PCELIST,IDX,ONELINE,ENTRY)  ;"ADD ICDS TO PCELIST
  ;"    (#)=POV+^I48.2^^^Atrial fibrillation (SCT  ;49436004)^1^^0^^^1
  ;"    (#)=COM^1^@
  NEW ICDLIST SET ICDLIST=$GET(ONELINE(9))_" "_$GET(ONELINE(10))_" "_$GET(ONELINE(11))_" "_$GET(ONELINE(12))
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
  NEW CPT SET CPT=$GET(ONELINE(8))
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
LOGIMPM(MESSAGE)
  NEW X DO NOW^%DTC
  w MESSAGE,!
  SET ^TMG("TMG ENCOUNTER IMPORT",%,MESSAGE)=""
  QUIT
  ;"