TMGHL7U3 ;TMG/kst-HL7 utility functions ; 12/11/17
              ;;1.0;TMG-LIB;**1**;12/11/17
 ;
 ;"TMG HL7 UTILITY FUNCTIONS 
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 12/11/17  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;                                           
 ;"=======================================================================
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"HL72FMDT(DATETIME) --Convert HL7 date time format into Fileman/Timson date.
 ;"FMDT2HL7(FMDT) -- Convert Fileman/Timson date into HL7 date time    
 ;"GETNAME(TMGHL7MSG) ;"GET PATIENT NAME FROM TMGHL7MSG ARRAY
 ;"GETDOB(TMGHL7MSG) ;"GET PATIENT DOB FROM TMGHL7MSG ARRAY
 ;"GETNMDOB(TMGHL7MSG) ;"GET PATIENT NAME AND DOB FROM TMGHL7MSG ARRAY
 ;              
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"=======================================================================
 ;
 ;"FYI -- $$FMDT2RDT and $$RDT2FMDT are in TMGLRWU1
 ; 
HL72FMDT(HL7DT) ;
  ;"Purpose: Convert HL7 date time format into Fileman/Timson date.
  ;"Input: HL7DT -- Expected format YYYYMMDD[HH[MM[SS]]]
  NEW TMGRESULT SET TMGRESULT=""
  SET HL7DT=$GET(HL7DT)
  IF HL7DT="" GOTO H2FMDN
  NEW YR SET YR=$EXTRACT(HL7DT,1,4)
  SET YR=YR-1700                                    
  NEW MNTH SET MNTH=$EXTRACT(HL7DT,5,6)
  NEW DAY SET DAY=$EXTRACT(HL7DT,7,8)
  NEW TIME,HR,MIN,SEC SET (HR,MIN,SEC)=""
  SET HR=$EXTRACT(HL7DT,9,10)
  SET MIN=$EXTRACT(HL7DT,11,12)
  SET SEC=$EXTRACT(HL7DT,13,14)
  IF $$SUPPTIME^TMGHL7U2()=1 DO  ;"ELH 2/10/15, //kt mod 12/11/16
  . IF HR'="" SET HR=00
  . IF MIN'="" SET MIN=00  
  . IF SEC'="" SET SEC=00
  SET TIME=HR_MIN_SEC IF TIME'="" SET TIME="."_TIME
  SET TMGRESULT=YR_MNTH_DAY_TIME
H2FMDN ;
  QUIT TMGRESULT
  ;
FMDT2HL7(FMDT)    ;
  ;"Purpose: Convert Fileman/Timson date into HL7 date time 
  ;"Input: FMDT -- Fileman DT  Format: YYYMMDD.HHMMSS
  ;"Output: HL7 DATETIME -- format YYYYMMDD[HH[MM[SS]]]
  NEW TMGRESULT SET TMGRESULT=""
  SET FMDT=$GET(FMDT)
  IF FMDT="" GOTO FM2HDN
  NEW YR SET YR=$EXTRACT(FMDT,1,3)+1700
  NEW MNTH SET MNTH=$EXTRACT(FMDT,4,5)
  NEW DAY SET DAY=$EXTRACT(FMDT,6,7)
  NEW HR,MIN,SEC SET (MIN,SEC)=""
  SET HR=$EXTRACT(FMDT,9,10)
  IF HR'="" DO
  . SET HR=$$RJ^XLFSTR(HR,2,0)
  . SET MIN=$EXTRACT(FMDT,11,12)
  . IF MIN'="" DO
  . . SET MIN=$$RJ^XLFSTR(MIN,2,0)
  . . SET SEC=$EXTRACT(FMDT,13,14)
  . . IF SEC'="" SET SEC=$$RJ^XLFSTR(SEC,2,0)
  SET TMGRESULT=YR_MNTH_DAY_HR_MIN_SEC
FM2HDN  ;
  QUIT TMGRESULT
  ;
GETNAME(TMGHL7MSG) ;"GET PATIENT NAME FROM TMGHL7MSG ARRAY
  NEW PIDIDX SET PIDIDX=+$ORDER(TMGHL7MSG("B","PID",0))
  NEW LNAME SET LNAME=$GET(TMGHL7MSG(PIDIDX,5,1))
  NEW FNAME SET FNAME=$GET(TMGHL7MSG(PIDIDX,5,2))
  NEW MNAME SET MNAME=$GET(TMGHL7MSG(PIDIDX,5,3))
  NEW NAME SET NAME=LNAME
  IF FNAME'="" SET NAME=NAME_","_FNAME 
  IF MNAME'="" SET NAME=NAME_" "_MNAME
  QUIT NAME
  ;
GETDOB(TMGHL7MSG) ;"GET PATIENT DOB FROM TMGHL7MSG ARRAY
  NEW PIDIDX SET PIDIDX=+$ORDER(TMGHL7MSG("B","PID",0))
  NEW DOB SET DOB=$GET(TMGHL7MSG(PIDIDX,7))
  SET DOB=$$HL72FMDT^TMGHL7U3(DOB)
  SET DOB=$$FMTE^XLFDT(DOB,"5DZ") ;"//kt added Z 5/2/19
  QUIT DOB
  ;
GETNMDOB(TMGHL7MSG) ;"GET PATIENT NAME AND DOB FROM TMGHL7MSG ARRAY
  NEW RESULT SET RESULT=$$GETNAME(.TMGHL7MSG)
  NEW DOB SET DOB=$$GETDOB(.TMGHL7MSG)
  IF DOB'="" SET RESULT=RESULT_" ("_$$GETDOB(.TMGHL7MSG)_")"
  QUIT RESULT
  ;  
SAVEALIAS(TMGDFN,NAME,DOB,SEX,SSN) ;"Create mapping between erroneous HL7 patient info and VistA patient
  ;" Input: TMGDFN -- Patient DFN
  ;"        NAME   -- Name as provided in HL7 message
  ;"        DOB    -- DOB as provided in HL7 message, converted to FMDT format
  ;"        SEX    -- Sex as provided in HL7 message
  ;"        SSN    -- SSN as provided in HL7 message, hyphens optional
  NEW TMGRESULT SET TMGRESULT="1^OK"
  SET TMGDFN=+$GET(TMGDFN)
  SET NAME=$GET(NAME) IF NAME="" DO  GOTO SADN
  . SET TMGRESULT="-1^Name not provided."
  NEW COUNT SET COUNT=0
  SET DOB=$GET(DOB) IF DOB'="" SET COUNT=COUNT+1
  SET SEX=$GET(SEX) IF SEX'="" SET COUNT=COUNT+1
  SET SSN=$TRANSLATE($GET(SSN),"-","") IF SSN'="" SET COUNT=COUNT+1
  IF COUNT<2 DO  GOTO SADN
  . SET TMGRESULT="-1^Did not get two elements of DOB, sex, SSN."
  NEW PRIORMATCH SET PRIORMATCH=$$ALIAS2DFN(NAME,DOB,SEX,SSN)
  IF PRIORMATCH>0 GOTO SADN
  NEW TMGFDA,TMGMSG
  IF $DATA(^TMG(22720.7,TMGDFN))>0 GOTO SA2
  NEW TMGIEN SET TMGIEN(1)=TMGDFN
  SET TMGFDA(22720.7,"+1,",.01)=TMGDFN
  DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO SADN
  . SET TMGRESULT="-1^"_$$GETERSTR^TMGDEBU2(.TMGMSG)
  KILL TMGIEN
SA2 ;  
  SET TMGFDA(22720.71,"+1,"_TMGDFN_",",.01)=NAME
  IF DOB'="" SET TMGFDA(22720.71,"+1,"_TMGDFN_",",.02)=DOB
  IF SEX'="" SET TMGFDA(22720.71,"+1,"_TMGDFN_",",.03)=SEX
  IF SSN'="" SET TMGFDA(22720.71,"+1,"_TMGDFN_",",.04)=SSN
  DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO SADN
  . SET TMGRESULT="-1^"_$$GETERSTR^TMGDEBU2(.TMGMSG)
SADN ;
  QUIT TMGRESULT
  ;
KILLALIAS(INFO)  ;"Remove alias info, as found by ALIAS2DFN
  ;"INPUT:  INFO -- array as created by ALIAS2DFN:
  ;"          INFO("INPUT","NAME")=NAME
  ;"          INFO("INPUT","DOB")=DOB
  ;"          INFO("INPUT","SEX")=SEX                    
  ;"          INFO("INPUT","SSN")=SSN
  ;"          INFO("MATCH","DFN")=TMGDFN
  ;"          INFO("MATCH","IEN22720.7")=TMGDFN
  ;"          INFO("MATCH","IENS")=SUBIEN_","_TMGDFN_","   -- IENS in file 22720.7
  ;"RESULTS:  1^OK, or -1^Error message
  NEW IEN SET IEN=$GET(INFO("MATCH","IEN22720.7"))
  NEW IENS SET IENS=$GET(INFO("MATCH","IENS"))
  NEW TMGFDA SET TMGFDA(22720.71,IENS,.01)="@"
  NEW TMGMSG
  NEW TMGRESULT SET TMGRESULT="1^OK"
  DO FILE^DIE("E","TMGFDA","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO KADN
  . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)  
KADN ;  
  QUIT TMGRESULT
  ;
ALIAS2DFN(NAME,DOB,SEX,SSN,OUT) ;" Get VistA DFN from erroneous HL7 alias
  ;" INPUT: NAME   -- Name as provided in HL7 message
  ;"        DOB    -- DOB as provided in HL7 message, converted to FMDT format
  ;"        SEX    -- Sex as provided in HL7 message
  ;"        SSN    -- SSN as provided in HL7 message, hyphens optional
  ;"        OUT    -- PASS BY REFERENCE, an OUT PARAMETER.  Optional.  
  ;"RESULT: Returns DFN of patient with HL7 alias matching provided HL7 info.
  ;"        If match found, then OUT is filled as follows:
  ;"          OUT("INPUT","NAME")=NAME
  ;"          OUT("INPUT","DOB")=DOB
  ;"          OUT("INPUT","SEX")=SEX                    
  ;"          OUT("INPUT","SSN")=SSN
  ;"          OUT("MATCH","DFN")=TMGDFN
  ;"          OUT("MATCH","IEN22720.7")=TMGDFN
  ;"          OUT("MATCH","IENS")=SUBIEN_","_TMGDFN_","   -- IENS in file 22720.7
  ;"NOTE: There must be an exact match for DFN to be returned
  NEW TMGRESULT SET TMGRESULT=0
  SET SSN=$TRANSLATE($GET(SSN),"-","")
  NEW TMGDFN SET TMGDFN=0
  FOR  SET TMGDFN=$ORDER(^TMG(22720.7,"TMGNAME",NAME,TMGDFN)) QUIT:(TMGDFN'>0)!(TMGRESULT>0)  DO
  . NEW SUBIEN SET SUBIEN=0
  . FOR  SET SUBIEN=$ORDER(^TMG(22720.7,"TMGNAME",NAME,TMGDFN,SUBIEN)) QUIT:(SUBIEN'>0)!(TMGRESULT>0)  DO
  . . NEW ZN SET ZN=$GET(^TMG(22720.7,TMGDFN,1,SUBIEN,0))
  . . NEW DBDOB SET DBDOB=$PIECE(ZN,"^",2)
  . . NEW DBSEX SET DBSEX=$PIECE(ZN,"^",3)
  . . NEW DBSSN SET DBSSN=$PIECE(ZN,"^",4)
  . . IF (DBDOB'=DOB)!(DBSEX'=SEX)!(DBSSN'=SSN) QUIT
  . . SET TMGRESULT=TMGDFN
  . . SET OUT("INPUT","NAME")=NAME
  . . SET OUT("INPUT","DOB")=DOB
  . . SET OUT("INPUT","SEX")=SEX
  . . SET OUT("INPUT","SSN")=SSN
  . . SET OUT("MATCH","DFN")=TMGDFN
  . . SET OUT("MATCH","IEN22720.7")=TMGDFN
  . . SET OUT("MATCH","IENS")=SUBIEN_","_TMGDFN_","
  QUIT TMGRESULT
  ;
WRITEALIAS(INFO) ;"Write out alias match info, as found by ALIAS2DFN. 
  ;"INPUT:  INFO -- array as created by ALIAS2DFN:
  ;"          INFO("INPUT","NAME")=NAME
  ;"          INFO("INPUT","DOB")=DOB
  ;"          INFO("INPUT","SEX")=SEX                    
  ;"          INFO("INPUT","SSN")=SSN
  ;"          INFO("MATCH","DFN")=TMGDFN
  ;"          INFO("MATCH","IEN22720.7")=TMGDFN
  ;"          INFO("MATCH","IENS")=SUBIEN_","_TMGDFN_","   -- IENS in file 22720.7
  WRITE "ALIAS INPUT:",!
  WRITE "  ",INFO("INPUT","NAME")," DOB=",$$FMTE^XLFDT(INFO("INPUT","DOB"))," SEX=",INFO("INPUT","SEX"),!
  NEW DFN SET DFN=$GET(INFO("MATCH","DFN"))
  IF DFN'>0 WRITE "(NO MATCH FOUND)",! QUIT
  WRITE "MATCHED TO VISTA PATIENT:",!
  WRITE " --> ",$$DFN2STR^TMGGDFN(DFN),!  
  QUIT