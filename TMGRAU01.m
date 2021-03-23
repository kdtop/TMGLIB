TMGRAU01 ;TMG/kst/Radiology report utilities; 11/1/16, 4/25/19
         ;;1.0;TMG-LIB;**1**;11/1/16
 ;
 ;"TMG RADIOLOGY REPORT FILING UTILITIES. 
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 11/1/2016  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"REGEXAM(DFN,DATA)  -Register exam in RAD/NUC MED PATIENT file, #70, REGISTERED EXAMS subfile
 ;"ENSRADFN(DFN) --Ensure patient registered in RAD/NUC MED PATIENT file, #70
 ;"STOREXAM(DATA) --Store exam report in file RAD/NUC MED REPORTS file, #74
 ;"GETPROC(CPT) --RETURN IEN IN 71 CORRESPONDING TO CPT
 ;"SNDALRTS(DATA) --Send one or more alerts, base on DATA array
 ;"SENDALRT(RADFN,IDT,IEN70D03) --Fire off OE/RR notifications, version 3.0+
 ;"SNDALRT2(RADFN,IDT,IEN70D03,RECIPS,OPTION)  ;
 ;"RPCALERT(OUT,RECIP,DFN,IDT,CASENUM,LEVEL,ADDEND)  -entry point for RPC: TMG CPRS IMAGING ALERT
 ;"GETRADFN(DFN)  
 ;"ASKDELRAD 
 ;"DELRAD(IENS)  
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;
REGEXAM(DFN,DATA)  ;"Register exam in RAD/NUC MED PATIENT file, #70, REGISTERED EXAMS subfile
  ;"INPUT: DFN -- IEN in PATIENT file #2
  ;"       DATA -- PASS BY REFERENCE.  FORMAT:
  ;"          DATA("DT")=FM format of date-time of study
  ;"          DATA("RDT")=Inverse of DT <-- an OUT PARAMETER
  ;"          DATA("RADFN")=RADFN <-- an OUT PARAMETER.  
  ;"          DATA("DIV")=Name or IEN of HOSPITAL DIVISION (#79) of imaging study -- required
  ;"          DATA("LOC")=Name or IEN of IMAGING LOCATION (#79.1) of imaging study -- required
  ;"          DATA("TYPE")=IEN of IMAGING TYPE (#79.2) of type of study -- optional
  ;"          DATA("IEN70.02")=IEN of registered exams in 70.02.  AN OUT PARAMETER.
  ;"          DATA(#)=""  # is the study number.  If single report, then just 1, if series, then multiple.
  ;"          DATA(#,"IEN70.03")=IEN of registered exam in 70.03.  AN OUT PARAMETER.
  ;"          DATA(#,"PROC")=IEN of RAD/NUC MED PROCEDURES (#71) of study -- optional
  ;"          DATA(#,"RADPROV")=Name or IEN in NEW PERSON file of radiologist -- optional
  ;"          DATA(#,"PROV")=Name or IEN in NEW PERSON file of ordering provider -- optional
  ;"          DATA(#,"CASE#")=an OUT parameter.  Filled with system-constructed Case number
  ;"          DATA(#,"RPT",##)=<LINES OF REPORT TEXT>  <--- NOTE: not used in this routine
  ;"Result: 1 if OK, or -1^Message if problem. 
  NEW TMGRESULT SET TMGRESULT=1
  NEW TMGRAQUIET SET TMGRAQUIET=1  ;"Use to shut down some console code in the RA* namespace (TMG modified)
  NEW TMGFDA,TMGIEN,TMGMSG,DIC,X,Y
  NEW RADFN SET RADFN=$$ENSRADFN(.DFN)  ;"Ensure patient registered in RAD/NUC MED PATIENT file, #70
  IF RADFN'>0 SET TMGRESULT=RADFN GOTO REGDN
  SET DATA("RADFN")=RADFN
  NEW DIVIEN SET DIVIEN=$GET(DATA("DIV"))
  IF DIVIEN'>0 SET DIC=79,DIC(0)="M",X=DIVIEN DO ^DIC SET DIVIEN=+Y
  IF DIVIEN'>0 DO  GOTO REGDN
  . SET TMGRESULT="-1^No DATA(""DIV"") provided, got ["_$GET(DATA("DIV"))_"]"
  NEW LOCIEN SET LOCIEN=$GET(DATA("LOC")) 
  IF LOCIEN'>0 SET DIC=79.1,DIC(0)="M",X=LOCIEN DO ^DIC SET LOCIEN=+Y
  IF LOCIEN'>0 DO  GOTO REGDN
  . SET TMGRESULT="-1^No DATA(""LOC"") provided, got ["_$GET(DATA("LOC"))_"]"
  NEW DT,%DT SET DT=+$GET(DATA("DT")),%DT="T",X=DT
  DO ^%DT SET DT=Y
  IF DT'>0 DO  GOTO REGDN
  . SET TMGRESULT="-1^No/invalid DATA(""DT"") provided, got ;"_$GET(DATA("DT"))_"]"
  NEW RDT SET RDT=9999999.9999-DT
  SET DATA("RDT")=RDT
  NEW TYPE SET TYPE=+$GET(DATA("TYPE"))   
REG1 ;"Ensure subrec 70.02 (REGISTERED EXAMS) is set up
  NEW SUBIEN SET SUBIEN=$ORDER(^RADPT("AR",DT,RADFN,0))
  IF SUBIEN>0 GOTO REG2
  NEW RAMDIV SET RAMDIV=DIVIEN  ;"used in input transform for field .01 in subfile 70.02
  NEW IENS SET IENS="+1,"_RADFN_","
  SET TMGFDA(70.02,IENS,.01)=DT
  IF TYPE>0 SET TMGFDA(70.02,IENS,2)=TYPE
  SET TMGFDA(70.02,IENS,3)=DIVIEN
  SET TMGFDA(70.02,IENS,4)=LOCIEN
  SET TMGIEN(1)=RDT
  DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO REGDN
  . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  SET SUBIEN=+$GET(TMGIEN(1)) IF SUBIEN'>0 DO  GOTO REGDN
  . SET TMGRESULT="-1^Unable to determine IEN in 70.02 of newly added record"
REG2 ;"
  SET DATA("IEN70.02")=SUBIEN
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(DATA(IDX)) QUIT:(IDX'>0)!(+TMGRESULT<0)  DO
  . KILL TMGFDA,TMGIEN,TMGMSG
  . NEW PROCIEN SET PROCIEN=+$GET(DATA(IDX,"PROC"))
  . NEW PROV SET PROV=+$GET(DATA(IDX,"PROV"))
  . NEW RADPROV SET RADPROV=+$GET(DATA(IDX,"RADPROV"))
  . NEW CASENUM SET CASENUM=0
  . NEW SUBSUBIEN SET SUBSUBIEN=0
  . NEW IEN70D03 SET IEN70D03=0
  . NEW FOUND SET FOUND=0
  . ;"See if study has already been registered (i.e. reprocessing HL7 message)
  . FOR  SET IEN70D03=$ORDER(^RADPT(RADFN,"DT",RDT,"P",IEN70D03)) QUIT:(IEN70D03'>0)!(FOUND>0)  DO
  . . NEW ZN SET ZN=$GET(^RADPT(RADFN,"DT",RDT,"P",IEN70D03,0))
  . . SET FOUND=($PIECE(ZN,"^",2)=PROCIEN) QUIT:'FOUND
  . . IF RADPROV>0 SET FOUND=($PIECE(ZN,"^",15)=RADPROV) QUIT:'FOUND
  . . IF PROV>0 SET FOUND=($PIECE(ZN,"^",14)=PROV) QUIT:'FOUND
  . . SET CASENUM=$PIECE(ZN,"^",1)
  . . SET SUBSUBIEN=IEN70D03
  . IF 'FOUND DO  QUIT:(+TMGRESULT<0)
  . . NEW IENS SET IENS="+1,"_SUBIEN_","_RADFN_","
  . . SET TMGFDA(70.03,IENS,.01)="N"  ;"CASE NUMBER.  Input XFrm should change this.  Must be 'N' to be non-interactive
  . . IF PROCIEN>0 SET TMGFDA(70.03,IENS,2)="`"_PROCIEN  ;"2=PROCEDURE
  . . SET TMGFDA(70.03,IENS,4)="SHARING"
  . . IF PROV>0 SET TMGFDA(70.03,IENS,14)="`"_PROV
  . . IF RADPROV>0 SET TMGFDA(70.03,IENS,15)="`"_RADPROV
  . . DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")  ;"<--- need "E" so input XFrm sets case number.  
  . . IF $DATA(TMGMSG("DIERR")) DO  QUIT
  . . . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  . . SET SUBSUBIEN=+$GET(TMGIEN(1)) IF SUBSUBIEN'>0 DO  QUIT
  . . . SET TMGRESULT="-1^Unable to get sub-IEN in file 70.03 in REG2^TMGRAU01"
  . . SET IENS=SUBSUBIEN_","_SUBIEN_","_RADFN_","
  . . SET CASENUM=$$GET1^DIQ(70.03,IENS,.01)
  . SET DATA(IDX,"CASE#")=CASENUM
  . SET DATA(IDX,"IEN70.03")=SUBSUBIEN
  ;"NOTE: TRACE THROUGH EN1^RAO7PC1 to see how reports are supposed to be filed..
REGDN ;  
  QUIT TMGRESULT
  ;
ENSRADFN(DFN)  ;"Ensure patient registered in RAD/NUC MED PATIENT file, #70
  ;"Input: DFN -- pointer in PATIENT file, #2
  ;"Result: RADFN or -1^message if error
  NEW TMGRESULT 
  SET DFN=+$GET(DFN)
  IF DFN'>0 DO  GOTO ENSDN
  . SET TMGRESULT="-1^No DFN provided, got ;"_$GET(DFN)_"]"
  SET TMGRESULT=DFN
  IF $DATA(^RADPT(DFN))>0 GOTO ENSDN  ;"ALREADY REGISTERED
  NEW TMGFDA,TMGMSG,TMGIEN
  SET TMGFDA(70,"+1,",.01)=DFN
  SET TMGFDA(70,"+1,",.04)="O"  ;"O=OUTPATIENT
  IF $GET(DUZ)>0 SET TMGFDA(70,"+1,",.06)=DUZ
  SET TMGIEN(1)=DFN
  DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO ENSDN
  . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
ENSDN ;  
  QUIT TMGRESULT
  ;
STOREXAM(DFN,DATA)  ;"Store exam report in file RAD/NUC MED REPORTS file, #74
  ;"INPUT: DFN - PATIENT IEN
  ;"       DATA -- PASS BY REFERENCE.  FORMAT:
  ;"          DATA("DT")=FM format of date-time of study
  ;"          DATA("IEN70.02")=IEN of registered exams in 70.02. 
  ;"                          NOTE: this is automatically added to data array by REGEXAM()
  ;"          DATA("RADFN")=RADFN   
  ;"          DATA("DIV")=Name or IEN of HOSPITAL DIVISION (#79) of imaging study <--- NOTE: not used in this routine
  ;"          DATA("LOC")=Name or IEN of IMAGING LOCATION (#79.1) of imaging study  <--- NOTE: not used in this routine
  ;"          DATA("TYPE")=IEN of IMAGING TYPE (#79.2) of type of study <--- NOTE: not used in this routine
  ;"          DATA(#)  # is the study number.  If single report, then just 1, if series, then multiple.
  ;"          DATA(#,"CASE#")=The case number that the system assigned to study during registration
  ;"                          NOTE: this is automatically added to data array by REGEXAM()
  ;"          DATA(#,"PROC")=IEN of RAD/NUC MED PROCEDURES (#71) of study -- optional
  ;"          DATA(#,"PROV")=Name or IEN in NEW PERSON file of ordering provider -- optional
  ;"          DATA(#,"RADPROV")=Name or IEN in NEW PERSON file of radiologist -- optional
  ;"               NOTE: the provider must hold the key RA VERIFY, and also have a value of
  ;"               [staff] or [resident] in field RAD/NUC MED CLASSIFICATION (#72) in the NEW PERSON file
  ;"          DATA(#,"DT REPORTED")=Date reported, put into field #8
  ;"          DATA(#,"RPT",##)=<LINES OF REPORT TEXT>  
  ;"          DATA(#,"IMP",##)=<LINES OF IMPRESSION TEXT>  
  ;"          DATA(#,"HX",##)=<LINES OF ADDITIONAL CLINICAL HISTORY TEXT>  
  ;"          DATA(#,"IEN70.03")=IEN of registered exam in 70.03. 
  ;"          DATA(#,"STATUS")="2^COMPLETE" or other allowed status, see file #72
  ;"Result: 1 if OK, or -1^Message(s) if problem. 
  NEW TMGRESULT SET TMGRESULT=1
  NEW TMGRAQUIET SET TMGRAQUIET=1  ;"Use to shut down some console code in the RA* namespace
  NEW RADFN SET RADFN=DATA("RADFN")
  NEW TMGFDA,TMGIEN,TMGMSG
  NEW IEN70D02 SET IEN70D02=DATA("IEN70.02")
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(DATA(IDX)) QUIT:+IDX'>0  DO
  . NEW RADPROV SET RADPROV=$GET(DATA(IDX,"RADPROV"))
  . NEW X,Y,DIC SET DIC=200,DIC(0)="M"
  . IF RADPROV'=+RADPROV SET X=RADPROV DO ^DIC SET RADPROV=+Y
  . NEW PROV SET PROV=$GET(DATA(IDX,"PROV")) 
  . IF PROV'=+PROV SET X=PROV DO ^DIC SET PROV=+Y
  . NEW RPTDT SET RPTDT=+$GET(DATA(IDX,"DT REPORTED"))
  . NEW DTS SET DTS=$TRANSLATE($$FMTE^XLFDT(DATA("DT")\1,"2Z"),"/","")
  . SET DTS=DTS_"-"_DATA(IDX,"CASE#")
  . NEW IEN74 SET IEN74=$ORDER(^RARPT("B",DTS,0))
  . ;"FIX!  It looks like IEN74 can potentially point to an existing report
  . ;"      that may be the same date but a different study
  . IF IEN74'>0 DO  QUIT:$DATA(DATA(IDX,"ERR"))
  . . KILL TMGFDA,TMGIEN,TMGMSG
  . . SET TMGFDA(74,"+1,",.01)=DTS
  . . SET TMGFDA(74,"+1,",2)=DFN   ;"`"_DFN
  . . SET TMGFDA(74,"+1,",3)=DATA("DT")  ;"EXAM DATE/TIME
  . . SET TMGFDA(74,"+1,",4)=DATA(IDX,"CASE#")
  . . SET TMGFDA(74,"+1,",5)="EF"  ;"ELECTRONICALLY FILED"  <--REPORT STATUS
  . . SET TMGFDA(74,"+1,",6)=$$NOW^XLFDT   ;"DATE REPORT ENTERED
  . . IF RPTDT>0 SET TMGFDA(74,"+1,",7)=RPTDT   ;"VERIFIED DATE
  . . IF RADPROV>0 SET TMGFDA(74,"+1,",9)=RADPROV   ;"`"_RADPROV  ;"VERIFYING PHYSICIAN
  . . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  . . IF $DATA(TMGMSG("DIERR")) DO  QUIT
  . . . SET DATA(IDX,"ERR")=$$GETERRST^TMGDEBU2(.TMGMSG)
  . . . SET DATA("ERR",IDX)=1
  . . SET IEN74=+$GET(TMGIEN(1)) IF IEN74'>0 DO  QUIT
  . . . SET DATA(IDX,"ERR")="Unable to get sub-IEN in file 70.03 in STOREEXAM^TMGRAU01"
  . . . SET DATA("ERR",IDX)=1
  . NEW PART FOR PART="RPT^200","IMP^300","HX^400" DO  QUIT:$DATA(DATA(IDX,"ERR"))
  . . ;"200=REPORT TEXT,  300=IMPRESSION TEXT, 400=ADDITIONAL CLINICAL HISTORY
  . . NEW NODE,FLD,TMGMSG SET NODE=$PIECE(PART,"^",1),FLD=$PIECE(PART,"^",2)
  . . IF $DATA(DATA(IDX,NODE))=0 QUIT
  . . NEW IDX2 SET IDX2=0
  . . FOR  SET IDX2=$ORDER(DATA(IDX,NODE,IDX2)) QUIT:IDX2'>0  DO
  . . . SET DATA(IDX,NODE,IDX2)=" "_$GET(DATA(IDX,NODE,IDX2))  ;"adding leading space causes system to not wrap text
  . . DO WP^DIE(74,IEN74_",",FLD,"K",$NAME(DATA(IDX,NODE)),"TMGMSG")  
  . . IF $DATA(TMGMSG("DIERR")) DO  QUIT
  . . . SET DATA(IDX,"ERR")=$$GETERRST^TMGDEBU2(.TMGMSG)
  . . . SET DATA("ERR",IDX)=1
  . KILL TMGFDA,TMGMSG
  . SET TMGFDA(74,IEN74_",",8)=$$FMADD^XLFDT(RPTDT,0,0,0,1) ;"REPORTED DATE
  . DO FILE^DIE("","TMGFDA","TMGMSG")
  . IF $DATA(TMGMSG("DIERR")) DO  QUIT
  . . SET DATA(IDX,"ERR")=$$GETERRST^TMGDEBU2(.TMGMSG)
  . . SET DATA("ERR",IDX)=1
  . ;"Now link report back to registered exam
  . NEW IEN70D03 SET IEN70D03=DATA(IDX,"IEN70.03")
  . KILL TMGFDA,TMGMSG 
  . NEW IENS SET IENS=IEN70D03_","_IEN70D02_","_RADFN_","
  . ;"------------------------------
  . NEW ZN SET ZN=$GET(^RADPT(RADFN,"DT",IEN70D02,"P",IEN70D03,0))
  . NEW STATUS SET STATUS=+$GET(DATA(IDX,"STATUS"))
  . IF STATUS'="" DO
  . . SET TMGFDA(70.03,IENS,3)=STATUS  ;"EXAM STATUS
  . IF PROV>0 SET TMGFDA(70.03,IENS,14)=PROV   ;"`"_PROV  ;"REQUESTING PHYSICIAN
  . IF RADPROV>0 SET TMGFDA(70.03,IENS,15)=RADPROV  ;"`"_RADPROV  ;"PRIMARY INTERPRETING STAFF
  . NEW CURIEN74 SET CURIEN74=$PIECE(ZN,"^",17)
  . IF CURIEN74="" SET TMGFDA(70.03,IENS,17)=IEN74  ;"`"_IEN74  ;"REPORT TEXT
  . ELSE  IF CURIEN74'=IEN74 DO  QUIT
  . . SET DATA(IDX,"ERR")="FILE 70.03, IENS="_IENS_" has current value of: "_CURIEN74_".  Should be: "_IEN74_", but field can not be edited.  Please fix."
  . . SET DATA("ERR",IDX)=1 
  . ;"------------------------------
  . ;"IF STATUS'="" DO KILLDD3
  . ;"DO FILE^DIE("EK","TMGFDA","TMGMSG")
  . DO FILE^DIE("K","TMGFDA","TMGMSG")
  . ;"IF STATUS'="" DO RESTRDD3
  . IF $DATA(TMGMSG("DIERR")) DO  QUIT
  . . SET DATA(IDX,"ERR")=$$GETERRST^TMGDEBU2(.TMGMSG)
  . . SET DATA("ERR",IDX)=1
  IF $DATA(DATA("ERR"))>0 DO
  . SET TMGRESULT="-1^"
  . SET IDX=0
  . FOR  SET IDX=$ORDER(DATA("ERR",IDX)) QUIT:IDX'>0  DO
  . . NEW MSG SET MSG=DATA(IDX,"ERR")
  . . SET TMGRESULT=TMGRESULT_IDX_":"_MSG_"; "
STRDN ;  
  QUIT TMGRESULT
  ;
GETPROC(CPT) ;"RETURN IEN IN 71 CORRESPONDING TO CPT
  NEW RESULT SET RESULT=+$ORDER(^RAMIS(71,"D",CPT,0))
  QUIT RESULT
  ;
SNDALRTS(DATA)  ;"Send one or more alerts, base on DATA array
  ;"Input: DATA -- PASS BY REFERENCE.  Format as output from REGEXAM(), *and* STOREXAM() above.
  ;"Result: 1^OK, or -1^Error message
  NEW TMGRESULT SET TMGRESULT="1^OK"
  NEW RDT SET RDT=+$GET(DATA("RDT"))
  IF RDT'>0 DO  GOTO SALTSDN
  . SET TMGRESULT="-1^Invalid RDT provided.  Got ["_$GET(DATA("RDT"))_"]"
  NEW RADFN SET RADFN=+$GET(DATA("RADFN"))
  IF RADFN'>0 DO  GOTO SALTSDN
  . SET TMGRESULT="-1^Invalid RADFN provided.  Got ["_$GET(DATA("RADFN"))_"]"
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(DATA(IDX)) QUIT:(IDX'>0)!(+TMGRESULT'>0)  DO
  . NEW IEN70D03 SET IEN70D03=+$GET(DATA(IDX,"IEN70.03"))
  . IF IEN70D03'>0 DO  QUIT
  . . SET TMGRESULT="-1^Invalid IEN 70.03 provided.  Got ["_$GET(DATA(IDX,"IEN70.03"))_"] Index#"_IDX
  . SET TMGRESULT=$$SENDALRT(RADFN,RDT,IEN70D03)      
SALTSDN ;
  QUIT TMGRESULT
  ;
SENDALRT(RADFN,IDT,IEN70D03)  ;" Fire off OE/RR notifications, version 3.0+
  ;" Input: RADFN:    Patient DFN (IEN IN RAD/NUC MED PATIENT)
  ;"        IDT:      Exam timestamp (inverse)  (IEN IN REGISTERED EXAMS 70.02)   
  ;"        IEN70D03: Exam IEN70.03 (EXMINATION SUBFILE)
  SET TMGRESULT="1^OK"
  NEW ZN SET ZN=$GET(^RADPT(RADFN,"DT",IDT,"P",IEN70D03,0))
  NEW RARPT SET RARPT=+$PIECE(ZN,"^",17)  ;"This is an IEN74 <-- REQUIRED in OE3^RAUTL00
  IF RARPT'>0 DO  GOTO SADN
  . SET TMGRESULT="-1^No linked report in field #17 of file 70.03.  IENS="_IEN70D03_","_IDT_","_RADFN_","
  NEW RAAB  ;"<-- used in OE3^RAUTL00
  ;"2/16/21 Note: the below alert goes to the provider of the radiology report (piece 14 in ZN).
  ;"      since this may be a provider from another facility, the below code will replace
  ;"      the provider with the patient's PCP (or failing that default to 168 (Kevin Toppenberg)
  ;"
  NEW PCP DO GETPROV^TMGPROV1(.PCP,RADFN,168)
  IF +PCP'>0 SET PCP=168   ;"SHOULDN'T BE NEEDED, BUT JUST IN CASE
  SET $P(ZN,"^",14)=PCP  ;"END 2/16/21 CHANGE
  DO OE3^RAUTL00(RADFN,IDT,IEN70D03,ZN)  ;"doesn't return any result state
SADN ;  
  QUIT TMGRESULT
  ;
SNDALRT2(RADFN,IDT,IEN70D03,RECIPS,OPTION)  ;
  ;" Input: RADFN:    Patient DFN (IEN IN RAD/NUC MED PATIENT)
  ;"        IDT:      Exam timestamp (inverse)  (IEN IN REGISTERED EXAMS 70.02)   
  ;"        IEN70D03: Exam IEN70.03 (EXMINATION SUBFILE)
  ;"        RECIPS:   PASS BY REFERENCE.  List of recipients of alert.  Format:
  ;"              RECIPS(IEN200)=""
  ;"        OPTION:   OPTIONAL.  PASS BY REFERENCE.  FORMAT:
  ;"            OPTION("LEVEL") = 1 (DEFAULT) --> NON-CRITICAL
  ;"                            = 2 --> Abnormal, needs attention
  ;"            OPTION("ADDENDUM") = 0 (DEFAULT) --> not addended
  ;"                               = 1 --> addended.
  SET TMGRESULT="1^OK"
  NEW ZN SET ZN=$GET(^RADPT(RADFN,"DT",IDT,"P",IEN70D03,0))
  NEW RARPT SET RARPT=+$PIECE(ZN,"^",17)  ;"This is an IEN74 <-- REQUIRED in OE3^RAUTL00
  IF RARPT'>0 DO  GOTO SA2DN
  . SET TMGRESULT="-1^No linked report in field #17 of file 70.03.  IENS="_IEN70D03_","_IDT_","_RADFN_","
  NEW LEVEL SET LEVEL=+$GET(OPTION("LEVEL")) IF LEVEL'=2 SET LEVEL=1
  NEW ADDEND SET ADDEND=+$GET(OPTION("ADDENDUM")) IF ADDEND'=1 SET ADDEND=0
  ;"code below copied and modified from OE3^RAUTL00
  NEW RA751,RAIENS,RAMSG,RANOTE,RAOIFN,RAREQPHY,X1
  SET X1=$SELECT($DATA(^RAMIS(71,+$PIECE(X,"^",2),0)):$PIECE(^(0),"^"),1:"")
  SET RA751=$GET(^RAO(75.1,+$PIECE(X,"^",11),0))
  SET RAIENS=IDT_"~"_IEN70D03
  IF LEVEL=2 DO  ;" abnormal report
  . IF ADDEND=0 SET RANOTE="25^Abnl Imaging Reslt, Needs Attn: "_$EXTRACT(X1,1,25)
  . IF ADDEND=1 SET RANOTE="53^Amended/Abnormal Imaging Results: "_$EXTRACT(X1,1,20)
  ELSE  DO   ;" no abnormal report
  . IF ADDEND=0 SET RANOTE="22^Imaging Results,Non Critical: "_$EXTRACT(X1,1,30)
  . IF ADDEND=1 SET RANOTE="53^Amended Imaging Results: "_$EXTRACT(X1,1,25)
  SET RAMSG=$PIECE($GET(RANOTE),"^",2)
  SET RAOIFN=$PIECE(RA751,"^",7)
  ;"SET RAREQPHY(+$PIECE(X,"^",14))=""
  MERGE RAREQPHY=RECIPS
  DO EN^ORB3(+$GET(RANOTE),RADFN,RAOIFN,.RAREQPHY,RAMSG,RAIENS)
SA2DN ;  
  QUIT TMGRESULT
  ;
RPCALERT(OUT,RECIP,DFN,IDT,CASENUM,LEVEL,ADDEND)  ;"entry point for RPC: TMG CPRS IMAGING ALERT
  SET TMGRESULT="1^OK"
  NEW TMGZZDEBUG SET TMGZZDEBUG=0
  IF TMGZZDEBUG=1 DO
  . KILL RECIP MERGE RECIP=^TMP("RPCALERT","RECIP")
  . SET DFN=$GET(^TMP("RPCALERT","DFN"))
  . SET IDT=$GET(^TMP("RPCALERT","IDT"))
  . SET CASENUM=$GET(^TMP("RPCALERT","CASENUM"))
  . SET LEVEL=$GET(^TMP("RPCALERT","LEVEL"))
  . SET ADDEND=$GET(^TMP("RPCALERT","ADDEND"))
  ELSE  DO
  . MERGE ^TMP("RPCALERT","RECIP")=RECIP
  . SET ^TMP("RPCALERT","DFN")=DFN
  . SET ^TMP("RPCALERT","IDT")=IDT
  . SET ^TMP("RPCALERT","CASENUM")=CASENUM
  . SET ^TMP("RPCALERT","LEVEL")=LEVEL
  . SET ^TMP("RPCALERT","ADDEND")=ADDEND
  ;"
  NEW RADFN SET RADFN=$$GETRADFN(.DFN)
  IF RADFN'>0 DO  GOTO RPADN
  . SET TMGRESULT="-1^Unable to find RADFN from patient DFN ["_$GET(DFN)_"] in RPCALERT^TMGRAU01"
  NEW IEN70D03 SET IEN70D03=$ORDER(^RADPT(RADFN,"DT",IDT,"P","B",+$GET(CASENUM),0))
  IF IEN70D03'>0 DO  GOTO RPADN
  . SET TMGRESULT="-1^Unable to to find study from case# ["_$GET(CASENUM)_"] in RPCALERT^TMGRAU01"
  NEW OPTION SET OPTION("LEVEL")=$GET(LEVEL),OPTION("ADDENDUM")=$GET(ADDEND)
  SET TMGRESULT=$$SNDALRT2(RADFN,IDT,IEN70D03,.RECIP,.OPTION)
RPADN ;
  SET OUT(0)=TMGRESULT
  QUIT TMGRESULT
  ;
GETRADFN(DFN)  ;"
  ;"Result: RADFN, or -1^Message
  ;"//use "B" index of ^RADPT  (file 70)
  ;"//e.g. 36378 --> RADFN = 36378
  NEW TMGRESULT SET TMGRESULT=+$ORDER(^RADPT("B",+$GET(DFN),0))
  QUIT TMGRESULT
  ;
ASKDELRAD ; "SEE ALSO ASKDELRAD^TMGLRWU3 (duplicate function)
  NEW IENS SET IENS=$$ASKIENS^TMGDBAP3(70.03)
  IF IENS'>0 QUIT
  NEW TMGRESULT SET TMGRESULT=$$DELRAD(IENS)
  QUIT
  ;
DELRAD(IENS)  ;"Delete a radiology study.  
  ;"PURPOSE: Remove from REGISTERED EXAMS in file 70, and also remove linked report file 74
  ;"Input: IENS -- an IENS for 70.03 (inside, 70.02, inside 70)
  ;"Result: 1^OK, or -1^error message
  NEW TMGRESULT SET TMGRESULT="1^OK"
  NEW IEN,IDX FOR IDX=1:1 QUIT:IDX>$LENGTH(IENS,",")  SET IEN(IDX)=$PIECE(IENS,",",IDX)
  IF $GET(IEN(3))'>0 DO  GOTO DLRDDN
  . SET TMGRESULT="-1^Invalid IENS. Got ["_IENS_"]"
  ;"step 1.  Get value from field 17 in 70.03.  This is a pointer to file 74
  NEW IEN74 SET IEN74=$$GET1^DIQ(70.03,IENS,17,"I")
  IF IEN74'>0 DO  GOTO DLRDDN
  . SET TMGRESULT="-1^Unable to get linked report in file 74.  Aborting."
  ;"Step 2.  Kill the sub-sub-record in 70.03, leaving rest of file alone.
  NEW DIK SET DIK=$$OREF^DILF($NAME(^RADPT(IEN(3),"DT",IEN(2),"P")))
  NEW DA SET DA=IEN(1),DA(1)=IEN(2),DA(2)=IEN(3)
  DO ^DIK  ;"kill sub-sub-record in 70.03
  ;"step 3.  Delete record in 74
  KILL DA SET DIK="^RARPT(",DA=IEN74
  DO ^DIK  ;"kill the record in 74
DLRDDN ;  
  QUIT TMGRESULT
  ;  
 ;"===================================================================================================== 
TEST  ;
  ;"CREATE A TEST ARRAY TO TRY FILING OF REPORTS
  NEW DATA
  NEW DT SET DT=$$NOW^XLFDT
  SET DATA("DIV")="LAUGHLIN"
  SET DATA("LOC")="LAUGHLIN"
  SET DATA("DT")=DT
  NEW PROCIEN SET PROCIEN=$$GETPROC(74000)  ;"74000 IS ABDOMEN 1 VIEW
  IF PROCIEN>0 DO
  . SET DATA(1,"PROC")=PROCIEN
  . NEW ZN SET ZN=$GET(^RAMIS(71,PROCIEN,0))
  . NEW TYPEIEN SET TYPEIEN=$PIECE(ZN,"^",12)
  . IF TYPEIEN>0 SET DATA("TYPE")=TYPEIEN
  SET DATA(1,"DT REPORTED")=DT
  SET DATA(1,"PROV")="TOPPENBERG,KEVIN"  
  SET DATA(1,"RADPROV")="DOCTOR,TWELVE"  
  SET DATA(1,"RPT",1)="Sample Rad Report  "  
  SET DATA(1,"RPT",3)="Free text exam: Spiffy XRay Exam  "  
  SET DATA(1,"RPT",4)="Comments:  "  
  SET DATA(1,"RPT",5)=" Normal appearing SAMPLE abdominal radiograph."  
  SET DATA(1,"RPT",6)=" Consider repeat study in 12 minutes "
  SET DATA(1,"IMP",1)="--patient is alive."  
  SET DATA(1,"IMP",2)="--patient has a heart"  
  SET DATA(1,"HX",1)="Patient is very sick"  
  SET DATA(1,"HX",2)="Recent back pain."  
  SET DATA(1,"STATUS")="2^COMPLETE"  
  NEW DFN SET DFN=75282  ;"zztest,strange
  NEW TMGRESULT SET TMGRESULT=$$REGEXAM(DFN,.DATA)
  IF TMGRESULT<0 WRITE !,TMGRESULT,! GOTO TESTDN
  SET TMGRESULT=$$STOREXAM(.DATA)
TESTDN ;  
  QUIT
  ;
TEST2 ;Test RPC getting back results of a LIST OF TESTS.
  NEW ROOT,TMGOUT,DFN SET DFN=75282  ;"zztest,strange
  DO EXAMS^ORWRA(.ROOT,DFN)
  MERGE TMGOUT=@ROOT
  KILL @ROOT
  IF $DATA(TMGOUT) ZWR TMGOUT(*)
  NEW STR SET STR=TMGOUT(1)
  NEW RPTID SET RPTID="18:IMAGING (LOCAL ONLY)~"
  NEW HSTYPE SET HSTYPE=""
  NEW DTRANGE SET DTRANGE=""
  NEW EXAMID SET EXAMID=$PIECE(STR,"^",1)_"#"_$PIECE(STR,"^",4)
  NEW ALPHA SET ALPHA=0
  NEW OMEGA SET OMEGA=0
  DO RPT^ORWRP(.ROOT,DFN,RPTID,HSTYPE,DTRANGE,EXAMID,ALPHA,OMEGA)
  KILL TMGOUT
  MERGE TMGOUT=@ROOT
  KILL @ROOT
  IF $DATA(TMGOUT) ZWR TMGOUT(*)
  QUIT
  ;
TEST3  ;"TEST SENDING ALERT FOR A GIVEN STUDY
  ;"Sample data
  ;" 90) ^RADPT(69928,0) = 69928^^^O^^168
  ;" 91) ^RADPT(69928,"DT",0) = ^70.02DA^6838787.8993^2
  ;" 92) ^RADPT(69928,"DT",6838787.8955,0) = 3161212.1044^1^71^4
  ;" 93) ^RADPT(69928,"DT",6838787.8955,"P",0) = ^70.03IA^1^1
  ;" 94) ^RADPT(69928,"DT",6838787.8955,"P",1,0) = 15^536^2^S^^^^^^^^^^83^241^^5
  ;" 95) ^RADPT(69928,"DT",6838787.8955,"P","B",15,1) =
  ;" 96) ^RADPT(69928,"DT",6838787.8993,0) = 3161212.1006^1^71^4
  ;" 97) ^RADPT(69928,"DT",6838787.8993,"P",0) = ^70.03IA^1^1
  ;" 98) ^RADPT(69928,"DT",6838787.8993,"P",1,0) = 16^537^2^S^^^^^^^^^^168^241^^6
  ;" 99) ^RADPT(69928,"DT",6838787.8993,"P","B",16,1) =
  ;"100) ^RADPT(69928,"DT","AP",536,6838787.8955,1) =
  ;"101) ^RADPT(69928,"DT","AP",537,6838787.8993,1) =
  ;"102) ^RADPT(69928,"DT","B",3161212.1006,6838787.8993) =
  ;"103) ^RADPT(69928,"DT","B",3161212.1044,6838787.8955) =  
  ;"
  ;"FILE 74 DATA                                 
  ;"1) ^RARPT(6,0) = 121216-16^69928^3161212.1006^16^EF^3161214.142151^3161212.10
  ;"               = 06^3161212.100601
  ;"2) ^RARPT(6,"R",0) = ^^23^23^3161214
  ;"3) ^RARPT(6,"R",1,0) =  HISTORY
  ;"4) ^RARPT(6,"R",2,0) =  Screening mammography with tomosynthesis.
  ;"5) ^RARPT(6,"R",3,0) =  COMPARISON
  ;"...                                                                 
  ;"Internal entry number for an exam; ^RADPT(RADFN,"DT",RADTI,"P",RACNI,0)
  NEW RADFN SET RADFN=69928
  NEW RADTI SET RADTI=6838787.8955
  NEW RACNI SET RACNI=1
  DO SENDALRT(RADFN,RADTI,RACNI)
  QUIT
  ;
 ;"-- Below depreciated.  Killing the input transform causes weird Fileman crash because Y is not defined... 
 ;"KILLDD3 ;" Remove input transform on field #3 in 70.03
 ;"  NEW XFRM SET XFRM=$PIECE($GET(^DD(70.03,3,0)),"^",5,999)
 ;"  IF XFRM=$$NEWDD3() QUIT
 ;"  SET ^DD(70.03,3,0,"TMGDDSAV")=XFRM
 ;"  SET $PIECE(^DD(70.03,3,0),"^",5,999)=$$NEWDD3()
 ;"  QUIT
 ;"  ;
 ;"NEWDD3() ;
 ;"  QUIT "S Y=X Q"
 ;"  ;
 ;"RESTRDD3 ;" Restore input transform on field #3 in 70.03, as saved by KILLDD3
 ;"  NEW XFRM SET XFRM=$GET(^DD(70.03,3,0,"TMGDDSAV"))
 ;"  IF (XFRM="")!(XFRM=$$NEWDD3()) QUIT  
 ;"  SET $PIECE(^DD(70.03,3,0),"^",5,999)=XFRM
 ;"  KILL ^DD(70.03,3,0,"TMGDDSAV")
 ;"  QUIT
 ;"  ;
  