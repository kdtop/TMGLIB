TMGHL70B ;TMG/kst-HL7 transformation utility functions ;11/18/15, 12/13/15
              ;;1.0;TMG-LIB;**1**;03/28/11
 ;
 ;"TMG HL7 TRANSFORMATION FUNCTIONS UTILITY
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"Especially mapping of test codes into array of linked files, and
 ;"   fixing problems that are found with mapping.  
 ;"=======================================================================
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"GETMAP(TMGENV,TEST,MODE,AUTOFIX,OUT) -- Map HL7 test onto VistA data, optionally interacting with user to add missing 
 ; 
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;"FIX60(TMGENV,TEST,INFO,IEN60,MODE) ;60=LABORATORY TEST
 ;"TRIMNAME(TESTNAME,INDENTN) ;Trim test name to 30 characters
 ;"ADD60SYN(IEN60,SYNONYM) ;
 ;"FIX61(TMGENV,NLT,INFO,IEN61) ;61=TOPOGRAPHY FIELD (used as specimen)
 ;"FIX64(TMGENV,NLT,INFO,IEN64) ;64=WKLD CODE
 ;"FIX64061(TMGENV,NLT,INFO,IEN64D061) ;64.061= LAB ELECTRONIC CODES (but used for SPECIMEN type)
 ;"FIX62D41(TMGENV,NLT,INFO,IEN62D41) ;62.41=CHEM TESTS in AUTO INSTRUMENT FILE (holds resultable tests)
 ;"FIXSTORE(TMGENV,IEN62D41,INFO,STORAGE) ;
 ;"FIX68D24(TMGENV,NLT,INFO,IEN68D24) ; 68.24 is TEST field in PROFILE field (field 68.23) in LOAD/WORK LIST file (68.2)
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;" TMGDEBUG, TMGUSRI4, TMGHL7*
 ;"=======================================================================
 ;
GETMAP(TMGENV,TEST,MODE,OUT) ;
  ;"Purpose: Map HL7 test onto VistA data, optionally interacting with user to add missing 
  ;"Input: TMGENV -- PASS BY REFERENCE.  Lab environment
  ;"           TMGENV("PREFIX") -- e.g. "LMH"
  ;"           TMGENV("IEN 68.2") -- IEN in LOAD/WORK LIST (holds orderable items)
  ;"           TMGENV("IEN 62.4") -- IEN in AUTO INSTRUMENT (holds resultable items)
  ;"           TMGENV("IEN PROFILE IN 68.2")=1  ;This is really IEN68D23, IN IEN68D2
  ;"           TMGENV("INTERACTIVE MODE") IF 1, then user is prompted to fix problems, otherwise problem just reported.         
  ;"       TEST -- <TestCode>^<Test Text>^<Coding System>^<Alternative Identifier>^<Alternative Text>^<Alternative coding system>
  ;"            -- OR NLT code
  ;"       MODE -- Flags: Contains 'R' if resultable, 'O' if orderable
  ;"       OUT -- PASS BY REFERENCE. An OUT PARAMETER.
  ;"Output: OUT("NLT")=<NLT CODE>
  ;"        OUT("SPECIMEN (61)")=SpecimenIEN61
  ;"        OUT("SPECIMEN (64.061)")=SpecimenIEN64.061, or "" IF not provided
  ;"        OUT("IEN64")=IEN64^NLT^NAME    ;64 is WKLD CODE file
  ;"        OUT("IEN60")=IEN60^TestName^Printname    ;60 = LABORATORY TEST file
  ;"        ----- Below IF MODE['R' ------ RESULTABLE INFO
  ;"        OUT("AI TEST")=IENS^TESTNAME   ;AI = AUTO INSTRUMENT (62.4)  
  ;"        OUT("STORAGE")=STORAGE Code^FIELD^FieldName        
  ;"        ----- Below IF MODE['O' ------ ORDERABLE INFO
  ;"        OUT("LWL TEST")=IENS (or '??' IF not found) ;IENS is for file 68.24 (in 68.23 in 68.2)  LWL = LOAD/WORKLOAD
  ;"Result: 1 if OK, or -1^ErrorMessage
  ;
  ;"NOTE: Test needs to be checked for TMG DEFAULT SPECIMEN SOURCE
  ;"  Add IF not present, and then use this value for all result codes...
  ;
  NEW DEBUG SET DEBUG=0
  IF DEBUG=1 DO
  . SET TMGENV("PREFIX")="PG"
  . SET TMGENV("IEN 68.2")=2
  . SET TMGENV("IEN 62.4")=204
  . SET TMGENV("IEN PROFILE IN 68.2")=1
  . SET TEST="5802-4^Urine Nitrites^LN"
  . SET TEST="82195.0000"
  . SET REQRESULT="R"
  . SET AUTOFIX=0
  ;
  NEW IEN60,IEN61,IEN62,IEN62D41,IEN64,IEN64D061,IEN68D2,IEN68D24
  NEW NLT,STORAGE,TEMPOUT
  ;
  NEW AUTOFIX SET AUTOFIX=+$GET(TMGENV("INTERACTIVE MODE"))
  SET MODE=$GET(MODE)
  NEW TMGU MERGE TMGU=TMGENV("TMGU")
  NEW TMGRESULT SET TMGRESULT="-1^No mapped test selected"
  NEW TMGLABPREFIX SET TMGLABPREFIX=TMGENV("PREFIX")
  NEW TESTID SET TESTID=$PIECE(TEST,TMGU(2),1)
  NEW TESTNAME SET TESTNAME=$PIECE(TEST,TMGU(2),2)
  NEW ALTTESTID SET ALTTESTID=$PIECE(TEST,TMGU(2),4)
  NEW ALTTESTNAME SET ALTTESTNAME=$PIECE(TEST,TMGU(2),5)
  IF TESTID="" DO
  . SET TESTID=ALTTESTID
  . SET $PIECE(TEST,TMGU(2),1)=TESTID
  . SET TESTNAME=ALTTESTNAME
  . SET $PIECE(TEST,TMGU(2),2)=TESTNAME
  ;"NEW LONGTESTNAME SET LONGTESTNAME=""
  NEW SYNONYM SET SYNONYM=TMGLABPREFIX_"-"_TESTID
  NEW TESTISNLT SET TESTISNLT=(TESTID?5N1"."4N)
  ;"
GM0 ;
  KILL TEMPOUT
  SET TEMPOUT("TESTID")=TESTID
  SET TEMPOUT("TESTNAME")=TESTNAME
  IF TESTISNLT=1 DO
  . NEW TMPOUT2
  . SET TMGRESULT=$$LMAPAPI2^TMGHL7U(.TMGENV,.TEST,.TMPOUT2)
  . ;"SET TMGRESULT=$$LMAPAPI2^TMGHL7U(.TMGENV,.TESTID,.TMPOUT2)
  . SET NLT=TESTID
  . MERGE TEMPOUT(NLT)=TMPOUT2(NLT) KILL TMPOUT2
  . SET IEN60=$GET(TEMPOUT(NLT,"IEN 60"))
  . SET TMGRESULT=$$IEN60API^TMGHL7U(.TMGENV,.IEN60,.TMPOUT2)
  . MERGE TEMPOUT(NLT)=TMPOUT2
  ELSE  DO
  . NEW TMPOUT2
  . SET TMGRESULT=$$LMAPAPI^TMGHL7U(.TMGENV,.TEST,.TMPOUT2)
  . ;"SET TMGRESULT=$$LMAPAPI^TMGHL7U(.TMGENV,.TESTID,.TMPOUT2)
  . SET NLT=$PIECE($GET(TMPOUT2(TESTID,"NLT CODE")),"^",2)
  . IF NLT="" SET NLT="??NLT"
  . MERGE TEMPOUT(NLT)=TMPOUT2(TESTID) KILL TMPOUT2
  . SET TMGRESULT=$$LMAPAPI2^TMGHL7U(.TMGENV,.NLT,.TMPOUT2)
  . MERGE TEMPOUT=TMPOUT2
  SET TMGRESULT=1  ;"Ignore errors, and detect ourselves below
  ;
GM1 ;
  SET OUT("NLT")=NLT
  SET TEMPOUT("NLT")=NLT
  ;
GM2  ;
  SET IEN60=$GET(TEMPOUT(NLT,"IEN60"))
  IF (IEN60'>0)&(AUTOFIX>0) DO
  . SET TMGRESULT=$$FIX60(.TMGENV,.TEST,.TEMPOUT,.IEN60,MODE)
  . IF TMGRESULT<0 QUIT
  . SET TEMPOUT("TESTNAME")=$PIECE(TEST,"^",2)  ;"Sometimes changed by $$FIX60()
  . NEW TMPOUT2
  . IF $$IEN60API^TMGHL7U(.TMGENV,.IEN60,.TMPOUT2)  ;"Ignore result. It will have errors becuase test not completely SET up
  . MERGE TEMPOUT(NLT)=TMPOUT2
  . SET IEN60=$GET(TMPOUT2("IEN60"),IEN60)
  IF (TMGRESULT<0)!(IEN60'>0) GOTO GMDN
  SET TEMPOUT("IEN60")=IEN60
  SET OUT("IEN60")=IEN60
  ;"
GM3 ;
  SET IEN64=$GET(TEMPOUT(NLT,"VA CODE"))
  IF (IEN64'>0)&(AUTOFIX>0) DO
  . SET TMGRESULT=$$FIX64(.TMGENV,.NLT,.TEMPOUT,.IEN64)
  . ;"SET OUT("NLT")=NLT
  . ;"SET TEMPOUT("NLT")=NLT
  IF (NLT["??") DO
  . NEW NEWNLT SET NEWNLT=$PIECE(IEN64,"^",2) QUIT:(+NEWNLT'>0)
  . MERGE TEMPOUT(NEWNLT)=TEMPOUT(NLT) KILL TEMPOUT(NLT)
  . SET NLT=NEWNLT 
  SET OUT("NLT")=NLT
  SET TEMPOUT("NLT")=NLT
  IF (TMGRESULT<0)!(IEN64'>0) GOTO GMDN
  SET TEMPOUT("IEN64")=IEN64
  SET OUT("IEN64")=IEN64
  ;
GM4  ;
  SET IEN61=$PIECE($GET(TEMPOUT(NLT,"SPECIMEN (61)")),"^",1)
  IF (+IEN61'>0)&(AUTOFIX>0) DO
  . SET TMGRESULT=$$FIX61(.TMGENV,.NLT,.TEMPOUT,.IEN61)
  IF TMGRESULT<0 GOTO GMDN
  SET TEMPOUT("SPECIMEN (61)")=IEN61
  SET OUT("SPECIMEN (61)")=IEN61
  ;
  SET IEN64D061=$PIECE($GET(TEMPOUT(NLT,"SPECIMEN (64.061)")),"^",1)
  IF (+IEN64D061'>0)&(AUTOFIX>0) DO
  . SET TMGRESULT=$$FIX64061(.TMGENV,.NLT,.TEMPOUT,.IEN64D061)
  IF TMGRESULT<0 GOTO GMDN
  SET TEMPOUT("SPECIMEN (64.061)")=IEN64D061
  SET OUT("SPECIMEN (64.061)")=IEN64D061
  ;"
  IF MODE["R" DO  GOTO:(TMGRESULT'>0) GMDN
  . SET IEN62D41=$GET(TEMPOUT(NLT,"AUTO INSTRUMENT TEST"))
  . IF (IEN62D41="")!(IEN62D41="??") DO
  . . SET TMGRESULT=$$FIX62D41(.TMGENV,NLT,.TEMPOUT,.IEN62D41) 
  . IF TMGRESULT'>0 QUIT
  . SET TEMPOUT("AI TEST")=IEN62D41
  . SET OUT("AI TEST")=IEN62D41
  . SET STORAGE=$GET(TEMPOUT(NLT,"STORAGE"))
  . NEW STOREFLD SET STOREFLD=+$PIECE(STORAGE,"(",2)
  . IF (STORAGE="")!(STORAGE="??")!(STOREFLD'>0) DO
  . . IF NLT'["??" DO
  . . . NEW TMPOUT2
  . . . SET TMGRESULT=$$LMAPAPI2^TMGHL7U(.TMGENV,NLT,.TMPOUT2)
  . . . MERGE TEMPOUT=TMPOUT2
  . . . SET STORAGE=$GET(TEMPOUT(NLT,"STORAGE"))
  . . . SET STOREFLD=+$PIECE(STORAGE,"(",2)
  . . IF (STORAGE="")!(STORAGE="??")!(STOREFLD'>0) DO
  . . . SET TMGRESULT=$$FIXSTORE(.TMGENV,IEN62D41,.TEMPOUT,.STORAGE) 
  . IF TMGRESULT'>0 QUIT
  . SET TEMPOUT("STORAGE")=STORAGE
  . SET OUT("STORAGE")=STORAGE
  . NEW VALREPL SET TMGRESULT=$$GRSLTMAP^TMGHL7U(.TMGENV,IEN64,.VALREPL)
  . IF TMGRESULT'>0 QUIT
  . MERGE OUT("REPLACEMENT MAP")=VALREPL
  IF MODE["O" DO  GOTO:(TMGRESULT'>0) GMDN
  . SET IEN68D24=$GET(TEMPOUT(NLT,"ORDERABLES","MAP 60->68.24"))
  . IF +IEN68D24'>0 DO
  . . SET TMGRESULT=$$FIX68D24(.TMGENV,NLT,.TEMPOUT,.IEN68D24) 
  . IF TMGRESULT'>0 QUIT
  . SET OUT("LWL TEST")=IEN68D24
  ;
GMDN  ; 
  QUIT TMGRESULT        
  ; 
 ;"-------------------------------------------------------------------------        
 ;"-------------------------------------------------------------------------
MSGREPLY(TMGENV,MSG)  ;"Return (and delete) reply for message, or "" if message not found.
  ;"Input : TMGENV -- Pass by reference.  Format: (also include more than shown here)
  ;"          TMGENV("GUI")=1  <-- signal that GUI is driving this code
  ;"          TMGENV("GUI","MSG",#)=<message from server code to GUI> <-- OUT PARAMETER
  ;"          TMGENV("GUI","MSG",#,"REPLY")=<reply from GUI to server code>   <-- IN PARAMETER
  ;"        MSG -- the particular message
  SET MSG=$GET(MSG)
  NEW TMGRESULT SET TMGRESULT=""
  NEW IDX SET IDX=""
  FOR  SET IDX=$ORDER(TMGENV("GUI","MSG",IDX)) QUIT:(+IDX'=IDX)!(TMGRESULT'="")  DO
  . IF $GET(TMGENV("GUI","MSG",IDX))'=MSG QUIT
  . SET TMGRESULT=$GET(TMGENV("GUI","MSG",IDX,"REPLY"))
  . KILL TMGENV("GUI","MSG",IDX)
  QUIT TMGRESULT
  ;
ADDMSG(TMGENV,MSG,VARS) ;"Add an outgoing message
  ;"Input : TMGENV -- Pass by reference.  Format: (also include more than shown here)
  ;"          TMGENV("GUI")=1  <-- signal that GUI is driving this code
  ;"          TMGENV("GUI","MSG",#)=<message from server code to GUI> <-- OUT PARAMETER
  ;"          TMGENV("GUI","MSG",#,"REPLY")=<reply from GUI to server code>   <-- IN PARAMETER
  ;"        MSG -- the particular message to send
  ;"        VARS -- OPTIONAL.  Format:
  ;"           VARS(<VARNAME>)=<merge of variable or array>
  NEW IDX SET IDX=$ORDER(TMGENV("GUI","MSG",""),-1)+1
  SET TMGENV("GUI","MSG",IDX)=MSG
  MERGE TMGENV("GUI","MSG",IDX,"VARS")=VARS
  QUIT
  ;
EXEC(TMGENV,CODE,VARS)  ;"Either execute code, or pass as request message to GUI, or get results from GUI action
  NEW TMGRESULT SET TMGRESULT=""
  IF +$GET(TMGENV("GUI")) DO  
  . NEW REPLY SET REPLY=$$MSGREPLY(.TMGENV,CODE)
  . IF REPLY="" DO  QUIT
  . . SET TMGRESULT="-1^GUI ACTION NEEDED"
  . . DO ADDMSG(.TMGENV,CODE,.VARS) 
  . SET TMGRESULT=REPLY
  ELSE  DO
  . SET CODE="SET TMGRESULT="_CODE
  . XECUTE CODE
  QUIT TMGRESULT
  ;
FIX60(TMGENV,TEST,INFO,IEN60,MODE) ;"60=LABORATORY TEST
  ;"Input: TMGENV -- environment.  See definition elsewhere, may include: 
  ;"          TMGENV("GUI")=1  <-- signal that GUI is driving this code
  ;"          TMGENV("GUI","MSG",#)=<message from server code to GUI> <-- OUT PARAMETER
  ;"          TMGENV("GUI","MSG",#,"REPLY")=<reply from GUI to server code>   <-- IN PARAMETER
  ;"       TEST -- PASS BY REFERENCE.  <TestCode>^<Test Text>^<Coding System>^<Alternative Identifier>^<Alternative Text>^<Alternative coding system>
  ;"       INFO -- array of  mapping information gathered so far
  ;"       IEN60 -- PASS BY REFERENCE.  AN OUT PARAMETER 
  ;"       MODE -- [OPTIONAL] 'O' for order-type tests (OBR) or 'R' resulting-type tests (OBX) 
  ;"Output: IEN60 is modified.  If TEST name is too long, it will be shortened.
  ;"Result: 1 if OK, or -1^Message if problem.        
  ;"MAP IEN 60 HERE OR RESULT ERROR STATE 
  NEW TMGRESULT SET TMGRESULT=1
  NEW CODE
  SET IEN60=""
  NEW TMGU MERGE TMGU=TMGENV("TMGU")
  NEW INDENTN SET INDENTN=+$GET(TMGENV("INDENTN"))
  SET TEST=$GET(TEST) 
  IF TEST="" DO  GOTO F60DN
  . SET TMGRESULT="In FIX60.TMGHL70B.  No HL7 test name provided to map."
  NEW TESTCODE SET TESTCODE=$PIECE(TEST,TMGU(2),1)
  NEW TESTNAME SET TESTNAME=$PIECE(TEST,TMGU(2),2)
  NEW ALTNAME SET ALTNAME=$PIECE(TEST,TMGU(2),5)
  NEW LONGNAME SET LONGNAME=""
  IF $LENGTH(TESTNAME)>30 DO  GOTO:(+TESTNAME<0) F60DN
  . NEW TEMP SET TEMP=TESTNAME 
  . IF $DATA(INFO("SHORTNAME")) SET INFO("SHORTNAME")=TESTNAME QUIT
  . SET CODE="$$TRIMNAME^TMGHL70B("""_TESTNAME_""","""_INDENTN_""")"
  . SET TESTNAME=$$EXEC(.TMGENV,CODE)
  . ;"SET TESTNAME=$$TRIMNAME(TESTNAME,INDENTN)
  . IF +TESTNAME<0 SET TMGRESULT=TESTNAME QUIT
  . SET INFO("SHORTNAME")=TESTNAME
  . SET $PIECE(TEST,"^",2)=TESTNAME
  . SET LONGNAME=TEMP
  NEW PREFIX SET PREFIX=TMGENV("PREFIX")
  NEW ADDNAME SET ADDNAME=TESTNAME
  IF (ALTNAME'="")&(ALTNAME'=ADDNAME) SET ADDNAME=ADDNAME_"^"_ALTNAME
  SET CODE="$$PCKADD60^TMGHL70C("""_ADDNAME_""",""DONE Adding LAB TEST"","""_INDENTN_""","""_$GET(MODE)_""")"
  SET IEN60=$$EXEC(.TMGENV,CODE)
  ;"SET IEN60=$$PCKADD60^TMGHL70C(TESTNAME,"DONE Adding LAB TEST",INDENTN,.MODE)
  IF IEN60<0 SET TMGRESULT=IEN60 GOTO F60DN 
  IF +IEN60=0 DO  GOTO F60DN
  . SET TMGRESULT="-1^No LAB TEST selected."
  NEW SYNONYM SET SYNONYM=PREFIX_"-"_TESTCODE
  SET TMGRESULT=$$ADD60SYN(IEN60,SYNONYM) 
  IF TMGRESULT'>0 GOTO F60DN
  SET SYNONYM=PREFIX_"-"_TESTNAME
  SET TMGRESULT=$$ADD60SYN(IEN60,SYNONYM) 
  IF TMGRESULT'>0 GOTO F60DN
  IF LONGNAME'="" DO
  . SET SYNONYM=PREFIX_"-"_LONGNAME
  . SET TMGRESULT=$$ADD60SYN(IEN60,SYNONYM) 
F60DN  ;
  QUIT TMGRESULT
  ;
TRIMNAME(TESTNAME,INDENTN) ;"Trim test name to 30 characters
  ;"Input: TESTNAME -- name to shorten
  ;"Result: New shortened test name, or -1^Message
  NEW INDENTSTR SET INDENTSTR=""
  SET INDENTN=+$GET(INDENTN)
  IF INDENTN>0 SET INDENTSTR=$JUSTIFY(" ",INDENTN)
  WRITE INDENTSTR,"'",TESTNAME,"' is too long.",!
  WRITE INDENTSTR,"Please shorten this name to 30 characters or less.",!,!
  NEW TMGRESULT SET TMGRESULT="-1^User aborted"
  NEW NEWNAME,%
TNM1  ;
  SET NEWNAME=$$READLEN^TMGUSRI4("Enter shorter test name (^ to abort): ",30,.INDENTN)
  IF NEWNAME["^" GOTO TRIMNDN
  IF NEWNAME="" DO  GOTO TNM1:(%=1),TRIMNDN
  . SET %=1
  . WRITE INDENTSTR,"Nothing entered.  Try again" DO YN^DICN WRITE !
  WRITE INDENTSTR,"Use name: '",NEWNAME,"'",!
  WRITE INDENTSTR,"instead of '",TESTNAME,"'",!
  SET %=1 WRITE INDENTSTR,"Is NEW name OK" DO YN^DICN WRITE !
  IF %=1 SET TMGRESULT=NEWNAME
  IF %'=2 GOTO TRIMNDN
  SET %=1 WRITE INDENTSTR,"Try entering a shorter name again" DO YN^DICN WRITE !
  IF %=1 GOTO TNM1        
TRIMNDN  ;
  QUIT TMGRESULT
  ;
ADD60SYN(IEN60,SYNONYM) ;
  ;"Purpose: add synonym to 60.
  ;"Result: 1 if OK, or -1^Message IF error. 
  NEW TMGFDA,TMGIEN,TMGMSG
  NEW TMGRESULT SET TMGRESULT=1
  SET TMGFDA(60.1,"+1,"_+IEN60_",",.01)=SYNONYM
  DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO F60DN
  . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  QUIT TMGRESULT
  ;        
FIX61(TMGENV,NLT,INFO,IEN61) ;"61=TOPOGRAPHY FIELD (used as specimen)
  ;"Input: TMGENV -- environment.  See definition elsewhere
  ;"       NLT -- code of test
  ;"       INFO -- array of  mapping information gathered so far
  ;"           INFO("IEN60") must exist
  ;"           INFO("TESTNAME") must exist
  ;"       IEN61 -- PASS BY REFERENCE.  AN OUT PARAMETER
  ;"Output: IEN61 is modified.  Expected format IEN61^Name
  ;"Result: 1 if OK, or -1^Message IF problem.        
  NEW TMGRESULT SET TMGRESULT=1
  NEW TESTNAME SET TESTNAME=INFO("TESTNAME")
  NEW INDENTN SET INDENTN=+$GET(TMGENV("INDENTN"))  
  NEW CODE SET CODE="$$ASKSPEC^TMGHL70C("""_TESTNAME_""","""_INDENTN_""")"
  NEW TEMP SET TEMP=$$EXEC(.TMGENV,CODE)  
  ;"NEW TEMP SET TEMP=$$ASKSPEC^TMGHL70C(TESTNAME,INDENTN)
  IF TEMP<0 SET TMGRESULT=TEMP GOTO F61DN
  SET IEN61=+TEMP
  NEW IEN60 SET IEN60=+INFO("IEN60") IF IEN60'>0 GOTO F61DN 
  NEW TMGFDA,TMGIEN,TMGMSG
  SET TMGFDA(60,+IEN60_",",22701)=+IEN61
  DO FILE^DIE("","TMGFDA","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO F61DN
  . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  SET INFO("IEN61")=IEN61
  NEW IEN62 SET IEN62=+$PIECE(TEMP,"^",2)
  IF IEN62>0 SET INFO("IEN62")=IEN62
F61DN  ;
  QUIT TMGRESULT
  ;"
FIX64(TMGENV,NLT,INFO,IEN64) ;"64=WKLD CODE
  ;"Input: TMGENV -- environment.  See definition elsewhere
  ;"       NLT -- code of test
  ;"       INFO -- array of  mapping information gathered so far
  ;"          INFO("IEN60") -- must exist. 
  ;"          INFO("TESTNAME") -- must exist
  ;"       IEN64 -- PASS BY REFERENCE.  AN OUT PARAMETER
  ;"Output: IEN64 is modified.  Format: IEN64^NLT^NAME
  ;"        INFO array is fixed to reflect NEW changes
  ;"Result: 1 if OK, or -1^Message IF problem.        
  NEW TMGRESULT SET TMGRESULT=1
  NEW CODE
  NEW IEN60 SET IEN60=INFO("IEN60")
  NEW INDENTN SET INDENTN=+$GET(TMGENV("INDENTN"))
  NEW TESTNAME SET TESTNAME=INFO("TESTNAME")
  IF $LENGTH(TESTNAME)>30 DO  GOTO:(+TESTNAME<0) F64DN
  . IF $DATA(INFO("SHORTNAME")) SET INFO("SHORTNAME")=TESTNAME QUIT  
  . SET CODE="$$TRIMNAME^TMGHL70B("""_TESTNAME_""","""_INDENTN_""")"
  . SET TESTNAME=$$EXEC(.TMGENV,CODE)
  . ;"SET TESTNAME=$$TRIMNAME(TESTNAME,INDENTN)
  . IF +TESTNAME<0 SET TMGRESULT=TESTNAME QUIT
  . SET INFO("SHORTNAME")=TESTNAME
  NEW EXCLUDELIST
  ;"Copy xref array to EXCLUDE LIST
  NEW IEN62D4 SET IEN62D4=$GET(TMGENV("IEN 62.4"))
  MERGE EXCLUDELIST=^LAB(62.4,IEN62D4,3,"AC")
  SET EXCLUDELIST=$NAME(^LAB(62.4,IEN62D4,3))
  SET CODE="$$ASKWKLD^TMGHL70C("""_TESTNAME_""",.EXCLUDELIST,"""_INDENTN_""")"
  NEW VARS MERGE VARS("EXCLUDELIST")=EXCLUDELIST
  SET IEN64=$$EXEC(.TMGENV,CODE,.VARS)
  ;"SET IEN64=$$ASKWKLD^TMGHL70C(TESTNAME,.EXCLUDELIST,INDENTN)
  IF IEN64'>0 DO  GOTO F64DN
  . IF IEN64["-1^GUI" SET TMGRESULT=IEN64 QUIT
  . SET TMGRESULT="-1^WKLD CODE not SET up for '"_TESTNAME_"'"
  NEW TMGFDA,TMGMSG
  SET TMGFDA(60,+IEN60_",",64)=+IEN64
  SET TMGFDA(60,+IEN60_",",64.1)=+IEN64
  DO FILE^DIE("","TMGFDA","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO F64DN
  . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  SET NLT=$PIECE(IEN64,"^",3)
  MERGE INFO(NLT)=INFO("??NLT")
  KILL INFO("??NLT")
F64DN  ;
  QUIT TMGRESULT
  ;"
FIX64061(TMGENV,NLT,INFO,IEN64D061) ;"64.061= LAB ELECTRONIC CODES (but used for SPECIMEN type)
  ;"Input: TMGENV -- environment.  See definition elsewhere
  ;"       NLT -- code of test
  ;"       INFO -- array of  mapping information gathered so far
  ;"           INFO("IEN60") must exist
  ;"           INFO("TESTNAME") must exist
  ;"       IEN64D061 -- PASS BY REFERENCE.  AN OUT PARAMETER
  ;"Output: IEN64D061 is modified
  ;"Result: 1 if OK, or -1^Message IF problem.        
  NEW TMGRESULT SET TMGRESULT=1
  ;"//kt original --> NEW TESTNAME SET TESTNAME=INFO("TESTNAME")  
  NEW TESTNAME SET TESTNAME=$PIECE($GET(INFO("IEN60")),"^",2)  ;"//kt 11/18/15
  NEW INDENTN SET INDENTN=+$GET(TMGENV("INDENTN"))
  NEW CODE SET CODE="$$ASKSPEC2^TMGHL70C("""_TESTNAME_""",.INFO,"""_INDENTN_""")"
  NEW VARS MERGE VARS("INFO")=INFO
  NEW TEMP SET TEMP=$$EXEC(.TMGENV,CODE,.VARS)
  ;"NEW TEMP SET TEMP=$$ASKSPEC2^TMGHL70C(TESTNAME,.INFO,INDENTN)
  IF TEMP'>0 SET TMGRESULT=TEMP GOTO F61DN
  SET IEN64D061=+TEMP
  NEW IEN60 SET IEN60=+INFO("IEN60") IF IEN60<0 GOTO F61XDN 
  NEW TMGFDA,TMGMSG
  SET TMGFDA(60,+IEN60_",",22700)=+IEN64D061
  DO FILE^DIE("","TMGFDA","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO F61XDN
  . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
F61XDN  ;
  QUIT TMGRESULT
  ;"
FIX62D41(TMGENV,NLT,INFO,IEN62D41) ;"62.41=CHEM TESTS in AUTO INSTRUMENT FILE (holds resultable tests)
  ;"Input: TMGENV -- environment.  See definition elsewhere
  ;"       NLT -- code of test
  ;"       INFO -- array of  mapping information gathered so far
  ;"       IEN62D41 -- PASS BY REFERENCE.  AN OUT PARAMETER
  ;"Output: IEN62D41 is modified.  Expected format: IENS^Name
  ;"Result: 1 if OK, or -1^Message IF problem.        
  NEW TMGRESULT SET TMGRESULT=1
  NEW TESTNAME SET TESTNAME=$PIECE($GET(INFO(NLT,"IEN60")),"^",2)
  IF TESTNAME="" DO  GOTO F62D41Q
  . SET TMGRESULT="-1^in FIX62D41.TMGHL70B.  Can't determine TESTNAME."
  NEW IEN60 SET IEN60=+$GET(INFO(NLT,"IEN60"))
  IF IEN60'>0 DO  GOTO F62D41Q
  . SET TMGRESULT="-1^in FIX62D41.TMGHL70B.  Can't determine IEN 60."
  NEW IEN61 SET IEN61=+$GET(INFO(NLT,"SPECIMEN (61)"))
  IF IEN61'>0 SET IEN61=+$GET(INFO("SPECIMEN (61)"))
  IF IEN61'>0 DO  GOTO F62D41Q
  . SET TMGRESULT="-1^in FIX62D41.TMGHL70B.  Can't determine IEN 61."
  SET TMGRESULT=$$ADD2AI^TMGHL70C(.TMGENV,TESTNAME,NLT,IEN60,IEN61) ;"not interactive
  IF +TMGRESULT>0 SET IEN62D41=$PIECE(TMGRESULT,"^",2),TMGRESULT=1
F62D41Q  ;
  QUIT TMGRESULT
  ;
FIXSTORE(TMGENV,IEN62D41,INFO,STORAGE) ;
  ;"Input: TMGENV -- environment.  See definition elsewhere
  ;"       IEN62D41 -- IEN that needs to be fixed. 
  ;"       INFO -- array of  mapping information gathered so far
  ;"       STORAGE -- PASS BY REFERENCE.  AN OUT PARAMETER
  ;"Output: STORAGE is modified.  Expected format: e.g. 'TV(795,1)^795^NITRITE, URINE'
  ;"Result: 1 if OK, or -1^Message if problem.        
  NEW TMGRESULT SET TMGRESULT=1
  NEW CODE
  NEW INDENTN SET INDENTN=+$GET(TMGENV("INDENTN"))
  NEW IEN60 SET IEN60=INFO("IEN60")
  NEW TESTNAME SET TESTNAME=INFO("TESTNAME")
  IF TESTNAME="" SET TESTNAME=$PIECE(INFO("AI TEST"),"^",2)
  IF $LENGTH(TESTNAME)>30 DO  GOTO:(+TESTNAME<0) FSDN
  . IF $GET(TMGENV("INTERACTIVE MODE"))'=1 DO  ;"//kt 2/25/2020
  . . SET TESTNAME="-1^Name too long: ["_TESTNAME_"]"
  . ELSE  DO
  . . SET CODE="$$TRIMNAME^TMGHL70B("""_TESTNAME_""","""_INDENTN_""")"
  . . SET TESTNAME=$$EXEC(.TMGENV,CODE)
  . IF +TESTNAME<0 SET TMGRESULT=TESTNAME QUIT
  . SET INFO("TESTNAME")=TESTNAME  
  NEW CODE SET CODE="$$LINKDN^TMGHL70C(.TMGENV,"""_TESTNAME_""","""_IEN60_""","""_INDENTN_""")"
  NEW VARS MERGE VARS("TMGENV")=TMGENV
  SET TMGRESULT=$$EXEC(.TMGENV,CODE,.VARS)
  ;"SET TMGRESULT=$$LINKDN^TMGHL70C(.TMGENV,TESTNAME,IEN60,INDENTN)
  IF TMGRESULT<0 GOTO FSDN
  NEW NLT SET NLT=INFO("NLT")
  NEW TEMP SET TMGRESULT=$$LMAPAPI2^TMGHL7U(.TMGENV,NLT,.TEMP) ;"not interactive
  IF TMGRESULT<0 GOTO FSDN
  SET STORAGE=$GET(TEMP(NLT,"STORAGE"))
  IF STORAGE="" SET TMGRESULT="-1^Couldn't determine storage for '"_TESTNAME_"'"
FSDN  ;
  QUIT TMGRESULT
  ;
FIX68D24(TMGENV,NLT,INFO,IEN68D24) ;" 68.24 is TEST field in PROFILE field (field 68.23) in LOAD/WORK LIST file (68.2)
  ;"Input: TMGENV -- environment.  See definition elsewhere
  ;"         TMGENV("IEN 62.4") must exist
  ;"         TMGENV("IEN 68.2") must exist
  ;"         TMGENV("IEN PROFILE IN 68.2") must exist
  ;"       NLT -- code of test
  ;"       INFO -- array of  mapping information gathered so far
  ;"           INFO("IEN60") must exist
  ;"           INFO("SPECIMEN (61)") must exist
  ;"           INFO("IEN62") used IF present
  ;"       IEN68D24 -- PASS BY REFERENCE.  AN OUT PARAMETER
  ;"Output: IEN68D24 is modified.  Expected format: ...
  ;"Result: 1 if OK, or -1^Message IF problem.        
  NEW TMGRESULT SET TMGRESULT=1
  NEW IEN60 SET IEN60=INFO("IEN60")
  NEW IEN61 SET IEN61=INFO("SPECIMEN (61)")
  NEW IEN68D2 SET IEN68D2=TMGENV("IEN 68.2")
  NEW IEN68D23 SET IEN68D23=TMGENV("IEN PROFILE IN 68.2")
  NEW IEN62D4 SET IEN62D4=TMGENV("IEN 62.4")
  NEW IEN62 SET IEN62=+$GET(INFO("IEN62"))
  NEW TMGFDA,TMGIEN,TMGMSG
  NEW IENS SET IENS="+1,"_+IEN68D23_","_IEN68D2_","
  SET TMGFDA(68.24,IENS,.01)="`"_+IEN60   ;"TEST
  SET TMGFDA(68.24,IENS,1)="`"_+IEN61     ;"SPECIMEN
  SET TMGFDA(68.24,IENS,2)="NO"          ;"BUILD NAME ONLY
  SET TMGFDA(68.24,IENS,3)="GENERIC"     ;"POC WKLD METHOD
  IF IEN62>0 SET TMGFDA(68.24,IENS,4)="`"_+IEN62  ;"POC COLLECTION SAMPLE
  DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO F68DXDN
  . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  SET IEN68D24=TMGIEN(1)
F68DXDN  ;
  QUIT TMGRESULT
  ;
