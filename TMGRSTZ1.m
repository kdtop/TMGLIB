TMGRSTZ1  ;TMG/kst/REST web service; 12/19/21
  ;;1.0;TMG-LIB;**1**;12/19/21
  ;
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;"Copyright (c) 12/19/2021  Kevin S. Toppenberg MD
  ;"
  ;"This file is part of the TMG LIBRARY, and may only be used in accordence
  ;" to license terms outlined in separate file TMGLICNS.m, which should
  ;" always be distributed with this file.
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;
  ;"=======================================================================
  ;" API -- Public Functions.
  ;"=======================================================================
  ;
  ;"=======================================================================
  ;"PRIVATE API FUNCTIONS
  ;"=======================================================================
  ;
  ;"=======================================================================
  ;"DEPENDENCIES
  ;"=======================================================================
  ;"Uses:  TMGRST*
  ;"=======================================================================
  ;
CCDAPTQ(RESULT,ARGS)  ; interface for ccda/patient GET.
  ;"Input: RESULT -- PASSED BY REFRENCE. AN OUT PARAMETER.  Format:
  ;"         RESULT("mime")=mime type
  ;"         @RESULT(#)=<line of result>
  ;"       ARGS -- URL args.  E.g.
  ;"       ARGS("name") = jones,bill
  ;"       ARGS("dob") = 1947-05-19  (yyyy-mm-dd)
  ;"       ARGS("gender") = m   <-- OPTIONAL
  ;"       ARGS("ssn") = 12345689  <---OPTIONAL
  ;"NOTE: may use HTTPREQ in global scope.
  ;
  DO DEBUGLOG
  SET RESULT=$NAME(^TMP("CCDA",$J))
  SET RESULT("mime")="text/plain; charset=utf-8"
  NEW PATIENT
  NEW NAME SET NAME=$$UP^XLFSTR($GET(ARGS("name"))) IF NAME="" GOTO PTQNONE
  NEW DOB SET DOB=$GET(ARGS("dob")) IF DOB="" GOTO PTQNONE
  SET PATIENT("NAME")=NAME
  SET PATIENT("DOB")=$$NORMDATE(DOB)
  NEW SEX SET SEX=$GET(ARGS("gender")) IF SEX'="" SET PATIENT("SEX")=SEX
  NEW SSN SET SSN=$GET(ARGS("ssn")) IF SSN'="" SET PATIENT("SSNUM")=SSN
  NEW ADFN SET ADFN=$$GETDFN^TMGGDFN(.PATIENT,0)
  IF ADFN'>0 GOTO PTQNONE
  SET @RESULT@(1)=+ADFN
  GOTO PTQDN
PTQNONE ;
  SET RESULT("mime")="application/json; charset=utf-8"
  DO SETERROR^VPRJRUT(404)  ;"not found
  SET @RESULT@(1)=""
PTQDN ;
  DO DEBUGLOG2
  QUIT
  ;
CCDAQ(RESULT,ARGS)  ; interface for ccdaAPI/query GET.  Will work with C0CD* code.
  ;"Input: RESULT -- PASSED BY REFRENCE. AN OUT PARAMETER.  Format:
  ;"         RESULT("mime")=mime type
  ;"         @RESULT(#)=<line of result>
  ;"       ARGS -- Elements from URL query.  E.g.
  ;"         ARGS("patientId") = 13222    <--- THIS IS REQUIRED.
  ;"         ARGS("data") = item1,item2,item3... A comma-delim list of items to include
  ;"            Allowed items are:  allergies,documents,immunizations,
  ;"                                encounters,procedures,social,medications,
  ;"                                problems,labs,vitals,evaluations
  ;"         ARGS("extract) = item (JUST 1 ITEM)
  ;"                                This means that after XML is found, but before returned to user,
  ;"                                the following items can be specifically extracted for return.
  ;"                                NOTE: for an item to be extracted, it must be included in initial search.
  ;"                                  E.g. to extract Smoking_Status, would have to have social in "data"
  ;"                                  so, if not already present, it will be forced (added to data node)
  ;"            Allowed items are:  Name, Sex, DOB, Race, Ethnicity, Preferred_Language, Smoking_Status,
  ;"                                Problems, Medications, Medication_Allergies, Lab_Tests, Lab_Result,
  ;"                                Vital_Signs, Procedures, Immunizations, Unique_Device_Identifier,
  ;"                                Assessment, Plan_of_Treatment, Goals, Health_Concerns
  ;"         ARGS("date") = may,2015:august,2015
  ;"NOTE: may use HTTPREQ in global scope.
  ;
  DO DEBUGLOG
  SET RESULT=$NAME(^TMP("CCDA",$J,"xml"))
  SET RESULT("mime")="text/xml; charset=utf-8"
  ;
  NEW DATE SET DATE=$GET(ARGS("date"))
  IF DATE'="" DO
  . NEW D1 SET D1=$$NORMDATE($PIECE(DATE,":",1))
  . NEW D2 SET D2=$$NORMDATE($PIECE(DATE,":",2))
  . SET $PIECE(DATE,":",1)=D1
  . IF D2'="" SET $PIECE(DATE,":",2)=D2
  . SET ARGS("date")=DATE

  NEW INCL SET INCL=$$LOW^XLFSTR($GET(ARGS("data")))
  NEW XMLMAP
  NEW EXTRACT SET EXTRACT=$$LOW^XLFSTR($GET(ARGS("extract")))
  IF EXTRACT'="" DO  ;"If extract planned, ensure that relevant section will included in XML
  . DO XMLMAP(.XMLMAP)
  . IF $DATA(XMLMAP(EXTRACT))'>0 QUIT
  . NEW SEC SET SEC=$GET(XMLMAP(EXTRACT,"SEC")) QUIT:SEC=""
  . ;"Here is a section that must be in data, if data is specified.
  . IF INCL="" QUIT  ;"since INCL is empty, then ALL are returned, so no worries
  . IF INCL[SEC QUIT  ;"already included, no problems
  . IF $EXTRACT(INCL,$LENGTH(INCL))'="," SET INCL=INCL_","
  . SET INCL=INCL_SEC
  ;
  IF INCL'="" do
  . NEW ARR DO SECMAP(.ARR)
  . NEW IDX FOR IDX=1:1:$LENGTH(INCL,",") DO
  . . NEW LONG SET LONG=$PIECE(INCL,",",IDX)  ;"this item wanted
  . . NEW SHORT SET SHORT=$GET(ARR("B",LONG)) QUIT:SHORT=""
  . . KILL ARR("A",SHORT),ARR("B",LONG)
  . ;"exclude everything left in ARR
  . NEW EXCLUDE SET EXCLUDE=""
  . NEW SHORT SET SHORT=""
  . FOR  SET SHORT=$ORDER(ARR("A",SHORT)) QUIT:SHORT=""  DO
  . . IF EXCLUDE'="" SET EXCLUDE=EXCLUDE_","
  . . SET EXCLUDE=EXCLUDE_SHORT
  . IF EXCLUDE'="" SET ARGS("exclude")=EXCLUDE
  . KILL ARGS("data")
  ;
  MERGE ARGS("select")=ARGS("date") KILL ARGS("date")
  ;
  DO wsCCDA^C0CDA(.RESULT,.ARGS)
  ;
  IF EXTRACT'="" DO
  . NEW PATH SET PATH=$GET(XMLMAP(EXTRACT,"PATH")) ;" DON'T QUIT OUT. LET GETXMLNODE HANDLE...  QUIT:PATH=""
  . NEW GREP SET GREP=$GET(XMLMAP(EXTRACT,"GREP"))
  . NEW TEMP,TMGXML
  . MERGE TMGXML=@RESULT
  . DO GETXMLNODE^TMGRSTZ2(.TEMP,.TMGXML,PATH,GREP)  ;"RESULT here holds XML array
  . KILL @RESULT
  . MERGE @RESULT=TEMP
  . SET RESULT("mime")="text/xml; charset=utf-8"
  ;
  DO DEBUGLOG2
  ;
  QUIT
  ;
SECMAP(ARR) ;
  ;;ALGY,allergies
  ;;DOCS,documents
  ;;IMMU,immunizations
  ;;ENC,encounters
  ;;PROC,procedures
  ;;SOC,social
  ;;MEDS,medications
  ;;PROBLEMS,problems
  ;;LABS,labs
  ;;VITALS,vitals
  ;;FUNC,evaluations
  ;;
  NEW DONE SET DONE=0
  NEW IDX SET IDX=0
  FOR  SET IDX=IDX+1 DO  QUIT:DONE
  . NEW LINE SET LINE=$TEXT(SECMAP+IDX^TMGRSTZ1)
  . NEW ENTRY SET ENTRY=$PIECE(LINE,";;",2)
  . IF (LINE'[";;")!(ENTRY="") SET DONE=1 QUIT
  . NEW SHORT SET SHORT=$PIECE(ENTRY,",",1)
  . NEW LONG SET LONG=$PIECE(ENTRY,",",2)
  . SET ARR("A",SHORT)=LONG
  . SET ARR("B",LONG)=SHORT
  QUIT
  ;
XMLMAP(ARR) ;  ;"//FORMAT: query name^XMLPath^requiredSection^grep value  <--- NOTE: required sections below should be tests to see if correct
  ;;Name^//x:patient/x:name^HEADER
  ;;Sex^//x:patient/x:administrativeGenderCode^HEADER
  ;;DOB^//x:patient/x:birthTime^HEADER
  ;;Race^//x:patient/x:raceCode^HEADER
  ;;Ethnicity^//x:patient/x:ethnicGroupCode^HEADER
  ;;Preferred_Language^//x:languageCode^HEADER
  ;;Smoking_Status^//x:title[text()='Social History']/..^social^tobacco
  ;;Problems^//x:title[text()='Problems']/..^problems
  ;;Medications^//x:title[text()='Medications']/..^medications
  ;;Medication_Allergies^//x:title[text()='Allergies']/..^allergies
  ;;Lab_Orders^//x:title[text()=' LAB ORDERS']/..^labs
  ;;Lab_Results^//x:title[text()='Lab Results']/..^labs
  ;;Vital_Signs^//x:title[text()='Vital Signs']/..^vitals
  ;;Procedures^//x:title[text()='Procedure']/..^procedures
  ;;Immunizations^//x:title[text()='Immunization']/..^immunizations
  ;;Unique_Device_Identifier^//x:title[text()='Procedure']/..^procedures^cardiac
  ;;Assessment^//x:title[text()=' ASSESSMENT']/..^documents
  ;;Plan_of_Treatment^//x:title[text()=' PLAN OF TREATMENT']/..^documents
  ;;Goals^//x:title[text()=' GOALS']/..^evaluations
  ;;Health_Concerns^//x:title[text()=' HEALTH CONCERNS']/..^evaluations
  ;;Care_Team^//x:title[text()='CARE TEAM MEMBERS']/..
  ;;
  NEW DONE SET DONE=0
  NEW IDX SET IDX=0
  FOR  SET IDX=IDX+1 DO  QUIT:DONE
  . NEW LINE SET LINE=$TEXT(XMLMAP+IDX^TMGRSTZ1)
  . NEW ENTRY SET ENTRY=$PIECE(LINE,";;",2)
  . IF (LINE'[";;")!(ENTRY="") SET DONE=1 QUIT
  . NEW ITEM SET ITEM=$$LOW^XLFSTR($PIECE(ENTRY,"^",1))
  . NEW PATH SET PATH=$PIECE(ENTRY,"^",2)
  . NEW SEC SET SEC=$PIECE(ENTRY,"^",3)
  . NEW GREP SET GREP=$PIECE(ENTRY,"^",4)
  . SET ARR(ITEM,"PATH")=PATH
  . SET ARR(ITEM,"SEC")=SEC
  . SET ARR(ITEM,"GREP")=GREP
  QUIT
  ;
NORMDATE(D) ;"Normalize date (a little)
  IF $GET(D)["-" DO
  . NEW YEAR SET YEAR=$PIECE(D,"-",1)
  . NEW MONTH SET MONTH=$PIECE(D,"-",2)
  . NEW DAY SET DAY=$PIECE(D,"-",3)
  . SET D=MONTH_"/"_DAY_"/"_YEAR
  QUIT D
  ;
DEBUGLOG ;
  NEW TMGZZ SET TMGZZ=0
  IF TMGZZ=1 DO
  . KILL ARGS,HTTPREQ
  . MERGE ARGS=^TMG("TMP","TMGRSTZ1","ARGS")
  . MERGE HTTPREQ=^TMG("TMP","TMGRSTZ1","HTTPREQ")
  ELSE  DO
  . KILL ^TMG("TMP","TMGRSTZ1")
  . MERGE ^TMG("TMP","TMGRSTZ1","ARGS")=ARGS
  . MERGE ^TMG("TMP","TMGRSTZ1","HTTPREQ")=HTTPREQ
  . ZSHOW "S":^TMG("TMP","TMGRSTZ1","STACK")
  QUIT
  ;
DEBUGLOG2 ;
  IF $GET(TMGZZ)'=1 DO
  . MERGE ^TMG("TMP","TMGRST02","RESULT")=RESULT
  . MERGE ^TMG("TMP","TMGRST02","@RESULT")=@RESULT
  QUIT

