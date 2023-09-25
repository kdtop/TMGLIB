TMGC0QT3 ;TMG/kst/TMG CPT2 tests ;10/17/17, 3/24/21
         ;;1.0;TMG-LIB;**1**;10/17/17
  ;
  ;"Test for generating CPT-II codes based on office notes.
  ;"These will be called by report for relevant billable items
  ;
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;"Copyright (c) 10/12/2016  Kevin S. Toppenberg MD
  ;"
  ;"This file is part of the TMG LIBRARY, and may only be used in accordence
  ;" to license terms outlined in separate file TMGLICNS.m, which should 
  ;" always be distributed with this file.
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;
  ;"=======================================================================
  ;" API -- Public Functions.
  ;"=======================================================================
  ;"BMI(OUTARRAY,BDATE,EDATE,TEXT)  -- Function for BMI CPT codes. 
  ;"HTN(OUTARRAY,SDT,EDT,TEXT) -- Function for HTN CPT codes. 
  ; 
  ;"=======================================================================
  ;"PRIVATE API FUNCTIONS
  ;"=======================================================================
  ;"EXCLUDE(TMGDFN) -- whether patient should be excluded based on insurance
  ;"BPGRP(TMGDFN,ADT,VITARR) --BP GROUP FOR GIVEN DATE
  ;"BESTGRP(TMGDFN,YSDT,YEDT) -- Best group already reported in date range.
  ;
  ;"=======================================================================
  ;
BMI(OUTARRAY,BDATE,EDATE,TEXT)  ;"Function for BMI CPT codes. 
  ;"Params: OutArray: PASS BY REFERENCE.  Format:
  ;"          OutArray(PatientName,TextToReturnWhenFound,DateFound)=""
  ;"              PatientName is the patient's name
  ;"              TextToReturnWhenFound is the text that will be displayedon the report
  ;"              DateFound is the date (in Fileman date format)
  ;"        BDATE: Beginning search date, sent from CPRS, in fileman format
  ;"        EDATE: Ending search date, sent from CPRS, in fileman format
  ;"        TEXT: This is Text To Return When Found. PASSED BY REFERENCE
  ;"             Initially, it is the text that is specified in the TMG BILLABLE ITEMS file
  ;"             it can be used or replaced as needed.  This should specify what
  ;"             to show on report.  E.g. 'Influenza (90656)'
  ;"RESULT: integer result is also expected, that represents number of records returned.
  ;"        If this isn't done,0 is assumed.
  QUIT $$STRSRCH(.OUTARRAY,BDATE,EDATE,TEXT,"BMI ","3008F")
  ;"
MEDLIST(OUTARRAY,BDATE,EDATE,TEXT)  ;"Function for med list documented CPT codes.
  ;"Params: OutArray: PASS BY REFERENCE.  Format:
  ;"          OutArray(PatientName,TextToReturnWhenFound,DateFound)=""
  ;"              PatientName is the patient's name
  ;"              TextToReturnWhenFound is the text that will be displayedon
  ;the report
  ;"              DateFound is the date (in Fileman date format)
  ;"        BDATE: Beginning search date, sent from CPRS, in fileman format
  ;"        EDATE: Ending search date, sent from CPRS, in fileman format
  ;"        TEXT: This is Text To Return When Found. PASSED BY REFERENCE
  ;"             Initially, it is the text that is specified in the TMG
  ;BILLABLE ITEMS file
  ;"             it can be used or replaced as needed.  This should specify
  ;what
  ;"             to show on report.  E.g. 'Influenza (90656)'
  ;"RESULT: integer result is also expected, that represents number of
  ;records returned.
  ;"        If this isn't done,0 is assumed.
  QUIT $$STRSRCH(.OUTARRAY,BDATE,EDATE,TEXT,"[FINAL MEDICATIONS]","1159F")
  ;"
TOBACCO(OUTARRAY,BDATE,EDATE,TEXT)  ;"Function for tobacco status updated
  ;"PARAMS: AS ABOVE
  QUIT $$STRSRCH(.OUTARRAY,BDATE,EDATE,TEXT,"TOBACCO USE HISTORY UPDATE--NEEDS RESEARCH","4004F")
  ;"
STRSRCH(OUTARRAY,BDATE,EDATE,TEXT,STRING,CPT)  ;"
  ;"Params: OutArray: PASS BY REFERENCE.  Format:
  ;"          OutArray(PatientName,TextToReturnWhenFound,DateFound)=""
  ;"              PatientName is the patient's name
  ;"              TextToReturnWhenFound is the text that will be displayed on the report
  ;"              DateFound is the date (in Fileman date format)
  ;"        BDATE: Beginning search date, sent from CPRS, in fileman format
  ;"        EDATE: Ending search date, sent from CPRS, in fileman format
  ;"        TEXT: This is Text To Return When Found. PASSED BY REFERENCE
  ;"             Initially, it is the text that is specified in the TMG BILLABLE ITEMS file
  ;"             it can be used or replaced as needed.  This should specify what
  ;"             to show on report.  E.g. 'Influenza (90656)'
  ;"RESULT: integer result is also expected, that represents number of records returned.
  ;"        If this isn't done,0 is assumed.
  NEW TMGRESULT SET TMGRESULT=0
  NEW SDT SET SDT=$EXTRACT(BDATE,1,3)_"0101"  ;"BEGINNING OF PASSED-IN YEAR
  NEW EDT SET EDT=$EXTRACT(BDATE,1,3)_"1231"  ;"END OF PASSED-IN YEAR
  SET EDATE=+$GET(EDATE) IF EDATE=0 SET EDATE=9999999
  SET EDATE=EDATE\1_".999999"
  NEW TIUDT,TIUIEN
  SET TIUDT=$PIECE(BDATE,".",1)-0.0000001
  FOR  SET TIUDT=$ORDER(^TIU(8925,"D",TIUDT)) QUIT:(TIUDT>EDATE)!(TIUDT'>0)  DO
  . SET TIUIEN=0
  . FOR  SET TIUIEN=$ORDER(^TIU(8925,"D",TIUDT,TIUIEN)) QUIT:TIUIEN'>0  DO
  . . SET FOUND=$$SRCHTIU^TMGRPT2(TIUIEN,STRING)
  . . IF FOUND=1 DO
  . . . NEW TMGDFN SET TMGDFN=$PIECE($GET(^TIU(8925,TIUIEN,0)),"^",2)
  . . . NEW NAME SET NAME=$$GETNAME^TMGRPT2(TMGDFN)
  . . . IF $$HASCPT^TMGRPU1(TMGDFN,CPT,SDT,EDT) QUIT
  . . . IF $$EXCLUDE^TMGC0QTU(TMGDFN) QUIT
  . . . SET OUTARRAY(NAME,TEXT,TIUDT)=""
  . . . SET TMGRESULT=TMGRESULT+1
  QUIT TMGRESULT
  ;"
BMIVALUE(OUTARRAY,BDATE,EDATE,TEXT)  ;"Function for BMI Value codes
  NEW BMITEXT SET BMITEXT="BMI "  ;"<- This is the text to find in the note. Should be defined server side at a later date
  NEW TMGRESULT SET TMGRESULT=0
  NEW SDT SET SDT=$EXTRACT(BDATE,1,3)_"0101"  ;"BEGINNING OF PASSED-IN YEAR
  NEW EDT SET EDT=$EXTRACT(BDATE,1,3)_"1231"  ;"END OF PASSED-IN YEAR
  SET EDATE=+$GET(EDATE) IF EDATE=0 SET EDATE=9999999
  SET EDATE=EDATE\1_".999999"
  NEW TIUDT,TIUIEN
  SET TIUDT=$PIECE(BDATE,".",1)-0.0000001
  FOR  SET TIUDT=$ORDER(^TIU(8925,"D",TIUDT)) QUIT:(TIUDT>EDATE)!(TIUDT'>0)  DO
  . SET TIUIEN=0
  . FOR  SET TIUIEN=$ORDER(^TIU(8925,"D",TIUDT,TIUIEN)) QUIT:TIUIEN'>0  DO
  . . SET FOUND=$$SRCHTIU^TMGRPT2(TIUIEN,BMITEXT)
  . . IF FOUND=1 DO
  . . . NEW TMGDFN SET TMGDFN=$PIECE($GET(^TIU(8925,TIUIEN,0)),"^",2)
  . . . NEW NAME SET NAME=$$GETNAME^TMGRPT2(TMGDFN)
  . . . NEW BMI SET BMI=$$BMI^TMGTIUOJ(TMGDFN)
  . . . SET BMI=+$GET(BMI)
  . . . IF BMI'>0 QUIT
  . . . KILL VADM
  . . . NEW AGE SET AGE=$P($$AGE^TIULO(TMGDFN),"yr",1)
  . . . NEW MORBID,MORBIDTEXT  ;"ELH ADDED 3/15/21
  . . . SET MORBID=0
  . . . IF BMI>39.9 DO
  . . . . SET MORBID=2
  . . . ELSE  IF BMI>34.9 DO    ;"11/4/22
  . . . . NEW PTHTN,PTDM,PTCOPD,TEMPDATE
  . . . . DO HTNMEDS^TMGPXR01(TMGDFN,.PTHTN,.TEMPDATE)
  . . . . DO PTHASDM^TMGPXR01(TMGDFN,.PTDM,.TEMPDATE)
  . . . . DO PTHSCOPD^TMGPXR01(TMGDFN,.PTCOPD,.TEMPDATE)
  . . . . SET MORBIDTEXT=""
  . . . . IF PTHTN=1 SET MORBIDTEXT="HTN"
  . . . . IF PTDM=1 DO
  . . . . . IF MORBIDTEXT'="" SET MORBIDTEXT=MORBIDTEXT_","
  . . . . . SET MORBIDTEXT=MORBIDTEXT_"DM"
  . . . . IF PTCOPD=1 DO
  . . . . . IF MORBIDTEXT'="" SET MORBIDTEXT=MORBIDTEXT_","
  . . . . . SET MORBIDTEXT=MORBIDTEXT_"COPD"
  . . . . IF MORBIDTEXT'="" SET MORBID=1
  . . . ELSE  DO
  . . . . SET MORBID=0
  . . . IF AGE<21 DO
  . . . . NEW SEX SET SEX=$P($G(^DPT(TMGDFN,0)),"^",2)
  . . . . SET BMI=$$BMIPTILE(BMI,AGE,SEX)
  . . . ELSE  DO
  . . . . SET BMI=$$BMICODE(BMI)
  . . . NEW BMIICD SET BMIICD=$P($P(BMI,"(",2),")",1)_" "
  . . . ;"Always enter BMI evne if already entered this year -> 8/22/23 IF $$HASICD^TMGRPU1(TMGDFN,BMIICD,SDT,EDT) QUIT
  . . . IF $$EXCLUDE^TMGC0QTU(TMGDFN) QUIT
  . . . SET OUTARRAY(NAME,BMI,TIUDT)=""
  . . . IF MORBID=2 SET OUTARRAY(NAME,"BMI is over 40 (E66.01)",TIUDT)=""  ;"ELH ADDED 3/15/21
  . . . IF MORBID=1 SET OUTARRAY(NAME,"BMI is over 34.9 (E66.01) and patient has "_MORBIDTEXT,TIUDT)=""  ;"ELH ADDED 11/4/22
  . . . SET TMGRESULT=TMGRESULT+1
  QUIT TMGRESULT
  ;"
BMICODE(BMIVALUE)  ;"DETERMINE PROPER BMI CODE
  NEW BMICODE SET BMICODE="BMI CODE NOT FOUND. PLEASE REPORT TO IT."  ;"ERROR STATE
  NEW BMIROUNDED
  IF BMIVALUE<20 DO
  . SET BMICODE="BMI is "_BMIVALUE_" (3008F w/ Z68.1)"
  ELSE  IF BMIVALUE<40 DO
  . SET BMIROUNDED=$P(BMIVALUE,".",1)
  . SET BMICODE="BMI is "_BMIVALUE_" (3008F w/ Z68."_BMIROUNDED_")"
  ELSE  DO
  . IF BMIVALUE<45 DO
  . . SET BMICODE="BMI is "_BMIVALUE_" (3008F w/ Z68.41)"
  . ELSE  IF BMIVALUE<50 DO
  . . SET BMICODE="BMI is "_BMIVALUE_" (3008F w/ Z68.42)"
  . ELSE  IF BMIVALUE<60 DO
  . . SET BMICODE="BMI is "_BMIVALUE_" (3008F w/ Z68.43)"
  . ELSE  IF BMIVALUE<70 DO
  . . SET BMICODE="BMI is "_BMIVALUE_" (3008F w/ Z68.44)"
  . ELSE  DO
  . . SET BMICODE="BMI is "_BMIVALUE_" (3008F w/ Z68.45)"
  QUIT BMICODE
  ;"
BMIPTILE(BMIVALUE,AGE,SEX)  ;"DETERMINE PROPER BMI PERCENTILE FOR THE CODE
  NEW BMICODE SET BMICODE="BMI CODE NOT FOUND. PLEASE REPORT TO IT."  ;"ERROR STATE
  NEW PTILE SET PTILE=+$$BMIPCTL^TMGGRC1(AGE,SEX,BMIVALUE)
  IF PTILE<6 DO
  . SET BMICODE="BMI is "_BMIVALUE_" - "_PTILE_" %tile (Z68.51)"
  ELSE  IF PTILE<86 DO
  . SET BMICODE="BMI is "_BMIVALUE_" - "_PTILE_" %tile (Z68.52)"
  ELSE  IF PTILE<96 DO
  . SET BMICODE="BMI is "_BMIVALUE_" - "_PTILE_" %tile (Z68.53)"
  ELSE  DO
  . SET BMICODE="BMI is "_BMIVALUE_" - "_PTILE_" %tile (Z68.54)"
  QUIT BMICODE
  ;"
HTN(OUTARRAY,SDT,EDT,TEXT)  ;"Function for HTN CPT codes. 
  ;"Purpose: prepare array for Billable Items, which is typically done on day of service.  
  ;"Params: OutArray: PASS BY REFERENCE.  Format:
  ;"          OutArray(PatientName,TextToReturnWhenFound,DateFound)=""
  ;"              PatientName is the patient's name
  ;"              TextToReturnWhenFound is the text that will be displayedon the report
  ;"              DateFound is the date (in Fileman date format)
  ;"        SDT: Beginning search date, sent from CPRS, in fileman format
  ;"        EDT: Ending search date, sent from CPRS, in fileman format
  ;"        TEXT: This is Text To Return When Found. PASSED BY REFERENCE
  ;"             Initially, it is the text that is specified in the TMG BILLABLE ITEMS file
  ;"             it can be used or replaced as needed.  This should specify what
  ;"             to show on report.  E.g. 'Influenza (90656)'
  ;"RESULT: integer result is also expected, that represents number of records returned.
  ;"        If this isn't done,0 is assumed.
  ;"NOTE:  //Below are possible findings, with each block marked with group #
  ;"       +---------------+---------------+---------------+
  ;"       | SYS < 130     | SYS 130-139   | SYS >= 140    |
  ;"       +1--------------+2--------------+4--------------+
  ;" DIAS  | 3074F         | 3075F         | XXXXX         |
  ;" <80   | 3078F         | 3078F         | 3078F         |
  ;"       +2--------------+3--------------+4--------------+
  ;" DIAS  | 3074F         | 3075F         | XXXXX         |
  ;" 80-89 | 3079F         | 3079F         | 3079F         |
  ;"       +4--------------+4--------------+5--------------+
  ;" DIAS  | 3074F         | 3075F         | XXXXX         |
  ;" >=90  | XXXXX         | XXXXX         | XXXXX         |
  ;"       +---------------+---------------+---------------+
  ;  
  NEW TMGRESULT SET TMGRESULT=0
  NEW YSDT SET YSDT=$EXTRACT(SDT,1,3)_"0101"  ;"BEGINNING OF PASSED-IN YEAR
  NEW YEDT SET YEDT=$EXTRACT(SDT,1,3)_"1231"  ;"END OF PASSED-IN YEAR
  SET EDT=+$GET(EDT) IF EDT=0 SET EDT=9999999
  SET EDT=EDT\1_".999999"
  NEW VITALS,TEMP,PTSEEN,EXCLUDE,BESTGRPARR
  NEW TIUDT,TIUIEN,TMGDFN
  SET TIUDT=$PIECE(SDT,".",1)-0.0000001
  FOR  SET TIUDT=$ORDER(^TIU(8925,"D",TIUDT)) QUIT:(TIUDT>EDT)!(TIUDT'>0)  DO
  . SET TIUIEN=0
  . FOR  SET TIUIEN=$ORDER(^TIU(8925,"D",TIUDT,TIUIEN)) QUIT:TIUIEN'>0  DO
  . . SET TMGDFN=+$PIECE($GET(^TIU(8925,TIUIEN,0)),"^",2) QUIT:TMGDFN'>0
  . . IF $DATA(EXCLUDE(TMGDFN)) QUIT  
  . . IF $$EXCLUDE^TMGC0QTU(TMGDFN) SET EXCLUDE(TMGDFN)="" QUIT
  . . SET PTSEEN(TMGDFN,TIUDT)=""
  ;"NOTE: I could break out below to a separate function if need to pass in a list 
  ;"      of patients from some other source...
  SET TMGDFN=0
  FOR  SET TMGDFN=$ORDER(PTSEEN(TMGDFN)) QUIT:+TMGDFN'>0  DO
  . NEW ISDM2 ;"SET ISDM2=$$.... FINISH THIS....
  . NEW AGE SET AGE=$$GET1^DIQ(2,TMGDFN,".033")
  . NEW ADT SET ADT=0
  . FOR  SET ADT=$ORDER(PTSEEN(TMGDFN,ADT)) QUIT:+ADT'>0  DO
  . . ;"10/25/18 - Insurance reps have advised us to report BP values every
  . . ;"time. Because of this, we will no longer worry about "best reportedgroups"
  . . ;"and just send the values in every time     ELH
  . . ;"
  . . ;"ORIGINAL BELOW
  . . ;"IF $DATA(VITALS(TMGDFN))=0 DO VITARR^TMGTIUO3(.VITALS,TMGDFN,.YSDT,.YEDT)
  . . ;"IF $DATA(VITALS(TMGDFN))=0 QUIT
  . . ;"NEW CURGROUP SET CURGROUP=$$BPGRP(TMGDFN,ADT,.VITALS) ;"GET CURRENT BP GROUP, LOWER IS BETTER
  . . ;"NEW BESTGROUP
  . . ;"IF $DATA(BESTGRPARR(TMGDFN)) SET BESTGROUP=$GET(BESTGRPARR(TMGDFN))
  . . ;"ELSE  DO
  . . ;". SET BESTGROUP=$$BESTGRP(TMGDFN,YSDT,YEDT) ;"Best group already reported this year
  . . ;". SET BESTGRPARR(TMGDFN)=BESTGROUP
  . . ;"IF +CURGROUP'<BESTGROUP QUIT  
  . . ;"
  . . ;"UPDATED
  . . NEW SDT,EDT SET SDT=$P(ADT,".",1)_".0001",EDT=$P(ADT,".",1)_".9999"
  . . IF $DATA(VITALS(TMGDFN))=0 DO VITARR^TMGTIUO3(.VITALS,TMGDFN,.SDT,.EDT)
  . . IF $DATA(VITALS(TMGDFN))=0 QUIT
  . . NEW CURGROUP SET CURGROUP=$$BPGRP(TMGDFN,ADT,.VITALS) ;"GET CURRENT BPGROUP, LOWER IS BETTER
  . . ;"  
  . . NEW CODE1 SET CODE1=$PIECE(CURGROUP,"^",2)
  . . NEW CODE2 SET CODE2=$PIECE(CURGROUP,"^",3)
  . . IF CODE1="",CODE2="" QUIT
  . . NEW TEMPSTR SET TEMPSTR=CODE1 
  . . IF TEMPSTR'="",CODE2'="" SET TEMPSTR=TEMPSTR_","
  . . NEW ATEXT SET ATEXT=TEXT_TEMPSTR_CODE2
  . . SET TEMP(TMGDFN,+CURGROUP)=ADT_"^"_ATEXT
  . . SET TEMP(TMGDFN,"DT",ADT)=+CURGROUP_"^"_ATEXT
  . . ;"SET OUTARRAY(NAME,ATEXT,TIUDT)=""
  NEW TMGDFN SET TMGDFN=0
  FOR  SET TMGDFN=$ORDER(TEMP(TMGDFN)) QUIT:+TMGDFN'>0  DO
  . NEW NAME SET NAME=$$GETNAME^TMGRPT2(TMGDFN)
  . ;"NEW GRP SET GRP=$ORDER(TEMP(TMGDFN,""))
  . ;"NEW STR SET STR=$GET(TEMP(TMGDFN,GRP))
  . ;"SET TIUDT=$PIECE(STR,"^",1)
  . NEW ADT SET ADT=$ORDER(TEMP(TMGDFN,"DT",""),-1) ;"codes must be *most recent*, not best, entry
  . NEW STR SET STR=$GET(TEMP(TMGDFN,"DT",ADT))
  . NEW ATEXT SET ATEXT=$PIECE(STR,"^",2,99)
  . SET OUTARRAY(NAME,ATEXT,ADT)=""
  . SET TMGRESULT=TMGRESULT+1
  QUIT TMGRESULT
  ;"
BPGRP(TMGDFN,ADT,VITARR) ;"GET BP GROUP FOR GIVEN DATE
  ;"INPUT: TMGDFN -- PATIENT
  ;"       ADT -- date of referece
  ;"       VITARR -- PASS BY REFERENCE.  AS CREATED BY VITARR^TMGTIUO3()
  ;"RESULT: number 1-5^CPT2 #1^CPT2 #2.  For group definitions, see HTN() above
  ;"NOTE: the *last* BP reading found in vitals array, *for given date*, will be used.
  NEW TMGRESULT SET TMGRESULT=5
  NEW CPT2A,CPT2B SET (CPT2A,CPT2B)=""
  NEW DT SET DT=$ORDER(VITARR(TMGDFN,"V",1,ADT\1+1),-1) IF DT'>0 GOTO GCBDN  
  NEW SYS SET SYS=+$GET(VITARR(TMGDFN,"V",1.1,DT)) IF SYS'>0 GOTO GCBDN
  NEW DIA SET DIA=+$GET(VITARR(TMGDFN,"V",1.2,DT)) IF DIA'>0 GOTO GCBDN
  NEW CPT2A,CPT2B SET (CPT2A,CPT2B)=""
  IF SYS<130 DO
  . SET CPT2A="3074F"
  . IF DIA<80 SET TMGRESULT=1,CPT2B="3078F" QUIT
  . IF DIA<90 SET TMGRESULT=2,CPT2B="3079F" QUIT
  . SET TMGRESULT=4
  ELSE  IF SYS<140 DO
  . SET CPT2A="3075F"
  . IF DIA<80 SET TMGRESULT=2,CPT2B="3078F" QUIT
  . IF DIA<90 SET TMGRESULT=3,CPT2B="3079F" QUIT
  . SET TMGRESULT=4
  ELSE  DO
  . IF DIA<80 SET TMGRESULT=4,CPT2B="3078F" QUIT
  . IF DIA<90 SET TMGRESULT=4,CPT2B="3079F" QUIT
  . SET TMGRESULT=5  
GCBDN ;
  QUIT TMGRESULT_"^"_CPT2A_"^"_CPT2B
  ;
BESTGRP(TMGDFN,YSDT,YEDT) ;"Best group already reported in date range.
  ;"INPUT: TMGDFN -- PATIENT
  ;"       YSDT -- should be date of beginning of calendar year under consideration
  ;"       ESDT -- should be date of end of calendar year under consideration
  NEW CPTARR DO ALDFNCPT^TMGRPU1(.CPTARR,TMGDFN,.YSDT,.YEDT)
  NEW ARR
  NEW DT SET DT=0
  FOR  SET DT=$ORDER(CPTARR("DT",DT)) QUIT:+DT'>0  DO
  . NEW CPT,CPTSTR SET CPT="",CPTSTR=""
  . FOR  SET CPT=$ORDER(CPTARR("DT",DT,TMGDFN,CPT)) QUIT:CPT=""  SET CPTSTR=CPTSTR_CPT_"^"
  . NEW TEMP SET TEMP=5
  . IF CPTSTR["3074F" DO
  . . IF CPTSTR["3078F" SET TEMP=1 QUIT
  . . IF CPTSTR["3079F" SET TEMP=2 QUIT
  . . SET TEMP=4
  . ELSE  IF CPTSTR["3075F" DO
  . . IF CPTSTR["3078F" SET TEMP=2 QUIT
  . . IF CPTSTR["3079F" SET TEMP=3 QUIT
  . . SET TEMP=4
  . ELSE  DO
  . . IF CPTSTR["3078F" SET TEMP=4 QUIT
  . . IF CPTSTR["3079F" SET TEMP=4 QUIT
  . . SET TEMP=5
  . SET ARR("DT",DT)=TEMP
  . SET ARR(TEMP)=DT
  NEW TMGRESULT SET TMGRESULT=+$ORDER(ARR(0)) IF TMGRESULT=0 SET TMGRESULT=5
  QUIT TMGRESULT
  ;
