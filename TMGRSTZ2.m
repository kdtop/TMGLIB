TMGRSTZ2  ;TMG/kst/REST web service; 12/19/21
  ;;1.0;TMG-LIB;**1**;12/19/21
  ;
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
GETXMLNODE(RETURN,XML,XMLPATH,SUFFIX)
  ;"Purpose: Returns a node from an XML array
  ;"Input: RETURN - Output, string containing requested XML node
  ;"       XML - Source XML array to pull node from
  ;"       XMLPath - The path for the document to pull from
  ;"       Suffix - Any command to insert at the end of the command (assuming a grep command)
  ;"Result: None
  ;"Get temp unique filename to store the XML to
  IF XMLPATH="" SET RETURN(1)="No path found. Invalid extract sent." QUIT
  NEW FPATH,FNAME
  SET FPATH="/tmp"
  SET FNAME="TEMP-"_$J_".xml"
  SET FNAME=$$UNIQUEFN^TMGTIUF2(FPATH,FNAME)  ;"Get unique filename
  ;"
  DO ARR2HFS^TMGIOUT3("XML",FPATH,FNAME)  ;"Save array to disk
  ;"
  ;"Run command
  IF SUFFIX'="" SET SUFFIX="| grep "_SUFFIX
  SET XMLPATH=""""_XMLPATH_""""
  NEW COMMAND SET COMMAND="echo $(xmlstarlet sel -N x=""urn:hl7-org:v3"" -t -c "_XMLPATH_" "_FPATH_"/"_FNAME_" "_SUFFIX_")"
  ;"Run command and store resulting array inside RETURN
  NEW RESULT SET RESULT=$$LINUXCMD(COMMAND,.RETURN)
  IF RETURN(1)="" SET RETURN(1)="No value found at: "_XMLPATH
  ;"
  ;"Clean up created file
  SET RESULT=$$LINUXCMD^TMGKERNL("rm "_FPATH_"/"_FNAME)
  QUIT
  ;"
TESTOUT
  NEW XML
  DO HFS2ARR^TMGIOUT3("/tmp","Test.xml","XML")
  IF '$D(XML) QUIT
  ;"
  NEW OUTXML
  NEW PATH,SUFFIX
  K DIR
  S DIR(0)="FO"
  S DIR("B")="//x:patient/x:name"
  S DUZ("L")="Enter path to return"
  D ^DIR
  S PATH=Y
  ;"
  K DIR
  S DIR(0)="FO"
  S DIR("B")=""
  S DUZ("L")="Enter suffix"
  D ^DIR
  S SUFFIX=Y
  ;"
  DO GETXMLNODE^TMGRSTZ2(.OUTXML,.XML,PATH,SUFFIX)
  ZWR OUTXML
  QUIT
  ;"
TESTOUT2
  NEW DFNARR,TITLEARR
  SET DFNARR(13195)=""
  SET DFNARR(13193)=""
  SET DFNARR(13194)=""
  set DFNARR(13196)=""
  ;SET EXTRACTARR("Name")=""
  ;SET EXTRACTARR("Sex")=""
  ;SET EXTRACTARR("DOB")=""
  ;SET EXTRACTARR("Race")=""
  ;SET EXTRACTARR("Ethnicity")=""
  ;SET EXTRACTARR("Preferred_Language")=""
  ;SET EXTRACTARR("Smoking_Status")=""
  ;SET EXTRACTARR("Problems")=""
  ;SET EXTRACTARR("Medications")=""
  ;SET EXTRACTARR("Medication_Allergies")=""
  ;SET EXTRACTARR("Lab_Tests")=""
  ;SET EXTRACTARR("Lab_Result")=""
  ;SET EXTRACTARR("GARBAGE")=""
  ;SET EXTRACTARR("Vital_Signs")=""
  ;SET EXTRACTARR("Procedures")=""
  ;SET EXTRACTARR("Immunizations")=""
  ;SET EXTRACTARR("Unique_Device_Identifier")=""
  ;SET EXTRACTARR("Assessment")=""
  ;SET EXTRACTARR("Plan_of_Treatment")=""
  SET EXTRACTARR("Goals")=""
  ;SET EXTRACTARR("Health_Concerns")=""
  ;SET EXTRACTARR("Care_Team")=""
  ;SET DATAARR("allergies")=""
  ;SET DATAARR("documents")=""
  ;SET DATAARR("immunizations")=""
  ;SET DATAARR("encounters")=""
  ;SET DATAARR("procedures")=""
  ;SET DATAARR("social")=""
  ;SET DATAARR("medications")=""
  ;SET DATAARR("problems")=""
  ;SET DATAARR("labs")=""
  ;SET DATAARR("vitals")=""
  ;SET DATAARR("evaluations")=""
  ;"
  NEW DFN SET DFN=0
  FOR  SET DFN=$O(DFNARR(DFN)) QUIT:DFN'>0  DO
  . WRITE "========= TESTING PATIENT: ",$P($G(^DPT(DFN,0)),"^",1)_"=======",!
  . NEW EXTRACT SET EXTRACT=""
  . FOR  SET EXTRACT=$O(EXTRACTARR(EXTRACT)) QUIT:EXTRACT=""  DO
  . . WRITE "       ===== Result for extract: ",EXTRACT," ====",!
  . . NEW ARGS
  . . SET ARGS("patientId")=DFN
  . . SET ARGS("extract")=EXTRACT
  . . NEW RESULT
  . . DO CCDAQ^TMGRSTZ1(.RESULT,.ARGS)
  . . ZWR @RESULT@(*)
  . . K @RESULT
  . NEW DATASECT SET DATASECT=""
  . FOR  SET DATASECT=$O(DATAARR(DATASECT)) QUIT:DATASECT=""  DO
  . . WRITE "       ===== Result for data section: ",DATASECT," ====",!
  . . NEW ARGS
  . . SET ARGS("patientId")=DFN
  . . SET ARGS("data")=DATASECT
  . . NEW RESULT
  . . DO CCDAQ^TMGRSTZ1(.RESULT,.ARGS)
  . . ZWR @RESULT@(*)
  . . K @RESULT
  . WRITE !,!
  QUIT
  ;"
LINUXCMD(CMD,OUT)  ;"Execute command on linux system, and return output
  ;"Input: CMD -- generic linux command to execute.
  ;"Result: "1^OK" or "-1^Error Message"
  NEW P SET P="TEMP" OPEN P:(COMMAND=CMD:readonly)::"pipe" USE P
  NEW X,IDX SET IDX=1
  FOR   QUIT:$ZEOF  DO
  . READ X#1024:5 IF X=""&$ZEOF QUIT
  . SET OUT(IDX)=X,IDX=IDX+1
  CLOSE P USE $P
  NEW TMGRESULT SET TMGRESULT="1^OK"
  ;"RESULT: 0 if no error; >0 if error
  NEW TEMP SET TEMP=$ZSYSTEM&255  ;"get result of execution. (low byte only)
  IF TEMP>0 SET TMGRESULT="-1^Linux error code returned: "_TEMP
  QUIT TMGRESULT
  ;
  ;"CCDAQ(RESULT,ARGS)  ; interface for ccdaAPI/query GET.  Will work with C0CD* code.
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

