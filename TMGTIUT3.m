TMGTIUT3 ;TMG/kst-TIU-related code ; 5/20/15, 4/11/17, 3/15/23, 7/28/24
   ;;1.0;TMG-LIB;**1**;5/20/15
  ;
  ;"Various TIU / text related code modules
  ;"Especially with TOPICS in TIU notes
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
  ;" RPC -- Public Functions.
  ;"=======================================================================
  ;"TOPICS(OUT,CMD,ADFN,SECTION,TOPICLST,SDT,EDT)-- High level entry point for functions below
  ;"TOPRBLNK(OUT,IN)  --TOPIC-PROBLEM LINK entry point.  RPC: TMG CPRS TOPIC LINKS
  ;"TOPICLST(OUT,ADFN,STARTING,DIR,MAX) -- List all topics in file 22719.51 (a subfile), or subset thereof. RPC: TMG CPRS TOPIC SUBSET
  ;"LKSUGEST(OUT,TOPIC)  -- Suggest info for problems to create for given topic. RPC ENTRY POINT FOR: TMG CPRS TOPIC CODE SUGGEST
  ;"ORDERDXS(OUT,ADFN) -- Get list of possible diagnoses for user to pick from, to be used in lab order dialog  
  ;"DXLIST(OUT,ADFN,CMD,SDT) -- Get list of possible diagnoses for user to pick from, during patient encounter.  RPC NAME:  TMG CPRS ENCOUNTER GET DX LIST
  ;"SUMNOTE(IEN8925,ARRAY) -- Get back summary data from prior parsing and storage. 
  ;"THREADS(OUT,ADFN,IEN8925,ARRAY) -- Get THREADS info for note
  ;"UNUSEDTOPICS(OUT,ARRAY,THREADINFO) -- Get list of topics that do NOT have corresponding thread text  
  ;"PROBLST(OUT,ADFN,SDT)  -- Get listing of patient's defined problem list
  ;"ICDLIST(OUT,ADFN,SDT) -- Get listing of ICD's used for patient in past  
  ;"PROCLIST(OUT,ADFN,CMD,SDT) -- Get list of possible procedures for user to pick from, during patient encounter. RPC NAME:  TMG CPRS ENCOUNTER GET CPT LST
  ;"VISITLIST(OUT,ADFN,CMD,SDT) -- Get list of possible procedures for user to pick from, during patient encounter.  RPC NAME:  TMG CPRS ENCOUNTER GET VST LST
  ;"SUBRECID(OREF,IEN,FILENUM) -- Callback function from LIST^DIC, optionally used
  ;"ENCEDIT(TMGRESULT,INPUT)  --RPC: TMG CPRS ENCOUNTER EDIT
  ;  
  ;"=======================================================================
  ;"PRIVATE API FUNCTIONS
  ;"=======================================================================
  ;"TESTLST ;
  ;"GETFTOP(OUT,ADFN,SECTION,SDT,EDT,.OPTION) -- GET FORMATTED TOPICS LIST for a patient
  ;"GETTOPL(OUT,ADFN,SECTION,SDT,EDT,FILTER) --GET TOPICS LIST for a patient
  ;"GETFULL(OUT,ADFN,SECTION,TOPIC,SDT,EDT) --Get cumulative, full text for problem
  ;"HNDLSET(LINE) -- Handle SET command from TOPRBLNK
  ;"HNDLGET(LINE) -- Handle GET command from TOPRBLNK
  ;"HNDLKILL(LINE)-- Handle KILL command from TOPRBLNK
  ;"TESTSGST -- test suggested toppic LKSUGEST
  ;"ADDENTRY(OUT,IDX,IEN,USEDARR,ALLARRAY) --utility function for common ICD's above
  ;"ADDSUBENTRY(OUT,IDX,IEN,SUBIEN,ALLARRAY) --utility function for common ICD's above  
  ;"ADDENTRYCPT(OUT,NODENUM,FNUM,IDX,IEN,USEDARR,ALLARRAY) --utility function for common CPT's above
  ;"ADDSUBENTRYCPT(OUT,NODENUM,FNUM,IDX,IEN,SUBIEN,ALLARRAY) --utility function for common CPT's above  
  ;"CPTLIST(OUT,ADFN,SDT) --Get listing of CPT's used for patient in past. Utility function for PROCLIST above. 
  ;"TESTCPT 
  ;"TESTVST 
  ;"ADDITEM(TMGCAT,TMGIEN,ARR,ERRORMSG)  -- add an item to the encounter form
  ;"DELITEM(TMGCAT,TMGIEN,ARR,ERRORMSG)  -- delete an item to the encounter form  
  ;"EDITITEM(TMGCAT,TMGIEN,ARR,ERRORMSG) -- EDIT an item on the encounter form
  ;"ADDERROR(ERRORMSG,ERROR)
  ;"TESTEDITENC 
  ;
  ;"=======================================================================
  ;"DEPENDENCIES: 
  ;"=======================================================================
  ;
TESTLST ;
  NEW DIC,X,Y SET DIC=2,DIC(0)="MAEQ"
  DO ^DIC WRITE !
  IF +Y'>0 QUIT
  NEW OUT DO GETFTOP(.OUT,+Y,"HPI")
  IF $DATA(OUT) DO ZWRITE^TMGZWR("OUT")
  QUIT
  ;
TOPICS(OUT,CMD,ADFN,SECTION,TOPICLST,SDT,EDT,OPTION) ;"High level entry point for functions below
  ;"INPUT: OUT -- AN OUT PARAMETER.  PASS BY REFERENCE.  Format depends on CMD
  ;"        If CMD="LIST", then 
  ;"          OUT(0)="1^OK" or "-1^Error message"
  ;"          OUT(#)=<TOPIC NAME>^<IEN8925>^<FM DT>
  ;"        If CMD="SUM1", then 
  ;"          OUT(0)="1^OK" or "-1^Error message"
  ;"          OUT(#)=FMDT^line#^line of text
  ;"       CMD -- The mode of the function.  Should be: 'LIST', or 'SUM1'
  ;"       ADFN -- PATIENT IEN NUMBER 
  ;"       SECTION -- 'HPI', or 'A&P'  OPTIONAL.  Default is 'HPI'
  ;"       TOPICLST -- +/-OPTIONAL. if CMD="SUM1", then this should be name of desired topic for summary.
  ;"            To provide multiple topic names, separate by "," character.  E.g. 
  ;"             "HTN,HYPERTENSION.,HYPERTENSION"
  ;"       SDT -- FM date for starting date range.  OPTIONAL.  Default = 0;
  ;"       EDT -- FM date for ENDING date range.  OPTIONAL.  Default = 9999999;
  ;"       OPTION -- OPTIONAL.  
  ;"         OPTION("LAST")=5  <-- example.  Means return the 5 most recent matches
  ;"         OPTION("FIRST")=3 <-- example.  Means return the 3 first/oldest matches
  ;"         If not provided, then ALL matches are returned.  
  ;"Result: none
  NEW ZZDEBUG SET ZZDEBUG=0
  IF ZZDEBUG=1 DO
  . SET CMD=$GET(^TMP("TMG","TOPICS","CMD"))
  . SET ADFN=$GET(^TMP("TMG","TOPICS","ADFN"))               
  . SET SECTION=$GET(^TMP("TMG","TOPICS","SECTION"))
  . SET TOPICLST=$GET(^TMP("TMG","TOPICS","TOPICLST"))
  . SET SDT=$GET(^TMP("TMG","TOPICS","SDT"))
  . SET EDT=$GET(^TMP("TMG","TOPICS","EDT"))
  ELSE  DO
  . KILL ^TMP("TMG","TOPICS")
  . SET ^TMP("TMG","TOPICS","CMD")=CMD
  . SET ^TMP("TMG","TOPICS","ADFN")=ADFN
  . SET ^TMP("TMG","TOPICS","SECTION")=$GET(SECTION)
  . SET ^TMP("TMG","TOPICS","TOPICLST")=$GET(TOPICLST)
  . SET ^TMP("TMG","TOPICS","SDT")=$GET(SDT)
  . SET ^TMP("TMG","TOPICS","EDT")=$GET(EDT)
  SET SECTION=$GET(SECTION,"HPI")
  SET CMD=$GET(CMD)
  SET SDT=$GET(SDT) IF (SDT="")!(SDT="-1") SET SDT=0
  SET EDT=$GET(EDT) IF EDT="" SET EDT=9999999
  IF CMD="LIST" DO
  . DO GETFTOP(.OUT,.ADFN,.SECTION,SDT,EDT,.OPTION)  ;"GET FORMATTED TOPICS LIST for a patient  
  ELSE  IF CMD="SUM1" DO
  . DO GETFULL(.OUT,.ADFN,.SECTION,.TOPICLST,SDT,EDT,.OPTION) ;"Get cumulative, full text for problem
  ELSE  DO
  . SET OUT(0)="-1^Invalid CMD (command) parameter provided."
  QUIT
  ;
GETFTOP(OUT,ADFN,SECTION,SDT,EDT,OPTION)  ;"GET FORMATTED TOPICS LIST for a patient
  ;"INPUT: OUT -- AN OUT PARAMETER.  PASS BY REFERENCE.  Format:
  ;"        OUT(0)="1^OK" or "-1^Error message"
  ;"        OUT(#)=<TOPIC NAME>^<IEN8925>^<FM DT>
  ;"       ADFN -- PATIENT IEN NUMBER
  ;"       SECTION -- 'HPI', or 'A&P'
  ;"       SDT -- FM date for starting date range.  OPTIONAL.  Default = 0;
  ;"       EDT -- FM date for ENDING date range.  OPTIONAL.  Default = 9999999;
  ;"       OPTION -- OPTIONAL.  
  ;"         OPTION("LAST")=5  <-- example.  Means return the 5 most recent matches
  ;"         OPTION("FIRST")=3 <-- example.  Means return the 3 first/oldest matches
  ;"         If not provided, then ALL matches are returned.  
  ;"Result: none
  NEW TEMP 
  SET SDT=$GET(SDT) IF SDT="" SET SDT=0
  SET EDT=$GET(EDT) IF EDT="" SET EDT=9999999
  NEW FIRST SET FIRST=+$GET(OPTION("FIRST"))
  NEW LAST SET LAST=+$GET(OPTION("LAST"))
  SET TEMP(0)=$$GETTOPL(.OUT,.ADFN,.SECTION,SDT,EDT)
  KILL OUT(ADFN,"C")
  NEW TOPIC SET TOPIC=""
  ;"FOR  SET TOPIC=$ORDER(OUT(ADFN,"B",TOPIC)) QUIT:TOPIC=""  DO
  ;". NEW IEN8925 SET IEN8925=$ORDER(OUT(ADFN,"B",TOPIC,SECTION,""),-1)
  ;". FOR  SET IEN8925=$ORDER(OUT(ADFN,"B",TOPIC,SECTION,IEN8925),-1) QUIT:+IEN8925'>0  DO
  ;". . KILL OUT(ADFN,"B",TOPIC,SECTION,IEN8925)
  NEW TDTIDX
  NEW IDX SET IDX=1  
  SET TOPIC=""
  FOR  SET TOPIC=$ORDER(OUT(ADFN,"B",TOPIC)) QUIT:TOPIC=""  DO
  . NEW IEN8925 SET IEN8925=0
  . FOR  SET IEN8925=$ORDER(OUT(ADFN,"B",TOPIC,SECTION,IEN8925)) QUIT:+IEN8925'>0  DO
  . . NEW DT SET DT=$GET(OUT(ADFN,"B",TOPIC,SECTION,IEN8925))
  . . SET TDTIDX(TOPIC,DT,IDX)=""
  . . SET TEMP(IDX)=TOPIC_"^"_IEN8925_"^"_DT SET IDX=IDX+1
  KILL OUT
  IF (FIRST>0)!(LAST>0) DO
  . NEW DIR,CT
  . NEW IDX2 SET IDX2=1
  . SET TOPIC=""
  . FOR  SET TOPIC=$ORDER(TDTIDX(TOPIC)) QUIT:TOPIC=""  DO
  . . NEW ADT
  . . IF FIRST>0 SET CT=FIRST,DIR=1,ADT=0
  . . ELSE  IF LAST>0 SET CT=LAST,DIR=-1,ADT=""
  . . FOR  SET ADT=$ORDER(TDTIDX(TOPIC,ADT),DIR) QUIT:(ADT'>0)!(CT'>0)  DO
  . . . NEW JDX SET JDX=0
  . . . FOR  SET JDX=$ORDER(TDTIDX(TOPIC,ADT,JDX)) QUIT:(JDX'>0)!(CT'>0)  DO
  . . . . NEW ENTRY SET ENTRY=$GET(TEMP(JDX))
  . . . . SET OUT(IDX2)=ENTRY,IDX2=IDX2+1,CT=CT-1
  ELSE  DO
  . MERGE OUT=TEMP
GFTLDN ;  
  QUIT  
  ;
GETTOPL(OUT,ADFN,SECTION,SDT,EDT,FILTER)  ;"GET TOPICS LIST for a patient
  ;"INPUT: OUT -- AN OUT PARAMETER.  PASS BY REFERENCE.  Format:
  ;"         OUT(ADFN,"B",<TOPIC NAME>,SECTION,IEN8925)=<FM DATE OF NOTE>
  ;"         OUT(ADFN,"C",<FM DATE OF NOTE>,<TOPIC NAME>,SECTION,IEN8925)=""
  ;"       ADFN -- PATIENT IEN NUMBER
  ;"       SECTION -- 'HPI', or 'A&P'
  ;"       SDT -- FM date for starting date range.  OPTIONAL.  Default = 0;
  ;"       EDT -- FM date for ENDING date range.  OPTIONAL.  Default = 9999999;
  ;"       FILTER -- OPTIONAL.  Pass by reference.  List of TOPICS that are allowed
  ;"           Format:  FILTER(<Allowed Topic Name>)=""
  ;"           If some data is passed in FILTER, then ONLY those names given are 
  ;"               included in output.  Otherwise all entries are output. 
  ;"Result: "1^OK", or "-1^Error message"
  NEW RESULT SET RESULT="1^OK"
  SET ADFN=+$GET(ADFN) IF ADFN'>0 DO  GOTO GTLDN
  . SET RESULT="-1^Patient record number (DFN) not provided."
  SET SECTION=$GET(SECTION)
  IF (SECTION'="HPI")&(SECTION'="A&P") DO  GOTO GFTLDN
  . SET RESULT="-1^Section name of 'HPI' or 'A&P' not provided."
  SET SDT=$GET(SDT) IF SDT="" SET SDT=0
  SET EDT=$GET(EDT) IF EDT="" SET EDT=9999999
  NEW NODE SET NODE=$SELECT(SECTION="HPI":2,SECTION="A&P":3,1:0) IF NODE'>0 GOTO GTLDN
  NEW IEN SET IEN=0
  FOR  SET IEN=$ORDER(^TMG(22719,"DFN",ADFN,IEN)) QUIT:+IEN'>0  DO
  . NEW IEN8925 SET IEN8925=+$PIECE($GET(^TMG(22719,IEN,0)),"^",1)
  . NEW NOTEDT SET NOTEDT=$PIECE($GET(^TIU(8925,IEN8925,0)),"^",7)
  . IF (NOTEDT<SDT)!(NOTEDT>EDT) QUIT
  . NEW SUBIEN SET SUBIEN=0 
  . FOR  SET SUBIEN=$ORDER(^TMG(22719,IEN,NODE,SUBIEN)) QUIT:+SUBIEN'>0  DO
  . . NEW TOPIC SET TOPIC=$PIECE($GET(^TMG(22719,IEN,NODE,SUBIEN,0)),"^",1) 
  . . IF TOPIC="" QUIT
  . . IF $DATA(FILTER),'$DATA(FILTER(TOPIC)) QUIT
  . . SET OUT(ADFN,"B",TOPIC,SECTION,IEN8925)=NOTEDT  
  . . SET OUT(ADFN,"C",NOTEDT,TOPIC,SECTION,IEN8925)=""
GTLDN ;
  QUIT RESULT
  ;
GETFULL(OUT,ADFN,SECTION,TOPIC,SDT,EDT,OPTION) ;"Get cumulative, full text for problem
  ;"Input:  OUT -- AN OUT PARAMETER.  PASS BY REFERENCE.  Format:
  ;"         OUT(0)="1^OK" or "-1^Error message"
  ;"         OUT(#)=FMDT^line#^line of text
  ;"       ADFN -- patient IEN
  ;"       SECTION -- Optional.  'HPI', or 'A&P'   Default is HPI
  ;"       TOPIC -- the topic, or paragraph title name.  Should exactly match
  ;"              that returned in output from GETTOPL() or GETFTOP()
  ;"              May contain multiple names, separated by commas, e.g.:
  ;"                 "HTN,HYPERTENSION.,HYPERTENSION"
  ;"       SDT -- FM date for starting date range.  OPTIONAL.  Default = 0;
  ;"       EDT -- FM date for ENDING date range.  OPTIONAL.  Default = 9999999;
  ;"       OPTION -- OPTIONAL.  
  ;"           OPTION("LAST")=5  <-- example.  Means return the 5 most recent matches
  ;"           OPTION("FIRST")=3 <-- example.  Means return the 3 first/oldest matches
  ;"           If not provided, then ALL matches are returned.  
  ;"Result: none.
  ;
  ;"NOTE: I once had RPC take 50 seconds(!) to return.  I think this was choke point
  ;"      Later I will see if I can rewrite with data from file 22719.2  //kt 3/5/23
  ;
  SET OUT(0)="1^OK"
  SET TOPIC=$GET(TOPIC)                   
  IF TOPIC="" DO  GOTO GFDN
  . SET OUT(0)="-1^Topic name not provided"
  SET SDT=$GET(SDT) IF SDT="" SET SDT=0
  SET EDT=$GET(EDT) IF EDT="" SET EDT=9999999
  SET SECTION=$GET(SECTION,"HPI")
  NEW DATA,IDX,TOPICARR
  FOR IDX=1:1:$LENGTH(TOPIC,",") DO
  . NEW ATOPIC SET ATOPIC=$PIECE(TOPIC,",",IDX) QUIT:ATOPIC=""
  . SET TOPICARR($$TRIM^XLFSTR(ATOPIC))=""  
  SET IDX=1
  SET OUT(0)=$$GETTOPL(.DATA,.ADFN,.SECTION,SDT,EDT,.TOPICARR)  ;"GET FORMATTED TOPICS LIST for a patient
  IF +OUT(0)'=1 GOTO GFDN
  NEW DIR SET DIR=1
  NEW LIMITCT SET LIMITCT=9999
  IF $DATA(OPTION("LAST")) SET DIR=-1,LIMITCT=+$GET(OPTION("LAST"))
  ELSE  IF +$GET(OPTION("FIRST")) SET LIMITCT=+$GET(OPTION("FIRST"))
  IF LIMITCT'>0 SET LIMITCT=1  
  NEW ATOPIC SET ATOPIC=""
  FOR  SET ATOPIC=$ORDER(TOPICARR(ATOPIC)) QUIT:ATOPIC=""  DO
  . NEW DATA2 MERGE DATA2=DATA(ADFN,"B",ATOPIC,SECTION)  ;"-->DATA2(IEN8925)=FMDT
  . NEW IEN8925 SET IEN8925=""
  . FOR  SET IEN8925=$ORDER(DATA2(IEN8925),-1) QUIT:(+IEN8925'>0)!(LIMITCT<1)  DO
  . . SET LIMITCT=LIMITCT-1
  . . NEW FMDT SET FMDT=$GET(DATA2(IEN8925))
  . . NEW PARSEDNOTE
  . . DO SUMNOTE^TMGTIUP1(.IEN8925,.PARSEDNOTE) 
  . . NEW LINENUM SET LINENUM=""
  . . FOR  SET LINENUM=$ORDER(PARSEDNOTE(IEN8925,"FULL",SECTION,ATOPIC,LINENUM)) QUIT:+LINENUM'>0  DO
  . . . NEW STR SET STR=$GET(PARSEDNOTE(IEN8925,"FULL",SECTION,ATOPIC,LINENUM))
  . . . SET OUT(IDX)=FMDT_"^"_LINENUM_"^"_STR,IDX=IDX+1
GFDN ;  
  QUIT
  ;
GETFULL4IEN(OUT,IEN8925,TOPIC,SECTION) ;"Get cumulative, full text for problem from given note.  
  ;"Input:  OUT -- AN OUT PARAMETER.  PASS BY REFERENCE.  Format:
  ;"         OUT(0)="1^OK" or "-1^Error message"
  ;"         OUT(#)=FMDT^line#^line of text
  ;"       TOPIC -- the topic, or paragraph title name.  Should exactly match
  ;"              that returned in output from GETTOPL() or GETFTOP()
  ;"       SECTION -- Optional.  'HPI', or 'A&P'   Default is HPI
  ;"Result: none.
  ;
  SET OUT(0)="1^OK"
  SET TOPIC=$GET(TOPIC)                   
  IF TOPIC="" SET OUT(0)="-1^Topic name not provided" GOTO GF4DN
  IF +$GET(IEN8925)'>0 SET OUT(0)="-1^IEN 8925 not provided.  Got ["_$GET(IEN8925)_"]" GOTO GF4DN  
  SET SECTION=$GET(SECTION,"HPI")
  NEW FMDT SET FMDT=$PIECE($GET(^TIU(8925,IEN8925,0)),"^",7) ;"0;7  -- .07  EPISODE BEGIN DATE/TIME      
  NEW IDX SET IDX=1
  NEW PARSEDNOTE DO SUMNOTE^TMGTIUP1(.IEN8925,.PARSEDNOTE) 
  NEW LINENUM SET LINENUM=""
  FOR  SET LINENUM=$ORDER(PARSEDNOTE(IEN8925,"FULL",SECTION,TOPIC,LINENUM)) QUIT:+LINENUM'>0  DO
  . NEW STR SET STR=$GET(PARSEDNOTE(IEN8925,"FULL",SECTION,TOPIC,LINENUM))
  . SET OUT(IDX)=FMDT_"^"_LINENUM_"^"_STR,IDX=IDX+1
GF4DN ;  
  QUIT
  ;"==========================================================
  ;
GETLTOPICS(OUT,ADFN,TERM,SDT,EDT) ;"RPC ENTRY POINT 'TMG CPRS PATIENT TOPIC SEARCH'
  ;"Purpose: Return a list of the patient's topics
  ;"Input: ADFN Patient's DFN
  ;"       TERM(Optional) - String to search by. If blank all results will be returned
  ;"       SDT(Optional) - Starting date to search by
  ;"       EDT(Optional) - Ending date for the search
  ;"Results: OUT
  NEW OPTION SET OPTION("LAST")=1
  NEW TOPICARR
  DO TOPICS(.TOPICARR,"LIST",ADFN,,,.SDT,.EDT,.OPTION)
  ;"FORMAT OUT LIST AS NEEDED FOR RPC RESULTS.  
  NEW IDX SET IDX=0
  NEW OUTIDX SET OUTIDX=0
  FOR  SET IDX=$O(TOPICARR(IDX)) QUIT:IDX'>0  DO
  . NEW TOPIC SET TOPIC=$P($G(TOPICARR(IDX)),"^",1)
  . IF TOPIC["HPI ISSUES" QUIT
  . IF TOPIC["E-Scribe" QUIT
  . IF TOPIC["Items arranged in Following Order" QUIT
  . IF TOPIC["Patient agrees toTELEMEDICINEvisit" QUIT
  . IF TOPIC["Patient agrees to TELEMEDICINE visit" QUIT
  . SET TOPIC=$$UP^XLFSTR(TOPIC),TERM=$$UP^XLFSTR(TERM)
  . IF (TERM="")!(TOPIC[TERM) DO
  . . SET OUT(OUTIDX)=$G(TOPICARR(IDX)),OUTIDX=OUTIDX+1
  QUIT
  ;
GETFULRPC(OUT,IEN8925,TOPIC,TMGDUZ)  ;"RPC ENTRY POINT 'TMG CPRS GET ONE PATIENT TOPIC'
  ;"Input: OUT -- AN OUT PARAMETER.  PASS BY REFERENCE.  Format:
  ;"         OUT(0)="1^OK" or "-1^Error message"
  ;"         OUT(#)=line of text
  ;"       IEN8925 -- IEN of TIU document
  ;"       TOPIC -- the topic, or paragraph title name.  Should exactly match
  ;"              that returned in output from GETTOPL() or GETFTOP()
  NEW TEMP
  NEW PREFIX,SUFFIX
  IF TMGDUZ=168 DO
  . SET PREFIX="<ul><li>",SUFFIX="</li></ul>"
  ELSE  IF TMGDUZ=83 DO
  . SET PREFIX="   *",SUFFIX=""
  ELSE  DO
  . SET PREFIX="",SUFFIX=""
  DO GETFULL4IEN(.TEMP,.IEN8925,.TOPIC)
  IF +$GET(TEMP(0))'>0 DO  GOTO GFRDN
  . MERGE OUT=TEMP
  NEW ADFN SET ADFN=$PIECE($GET(^TIU(8925,IEN8925,0)),"^",2) ;"0;2  .02  PATIENT        
  NEW TXT MERGE TXT("TEXT")=TEMP SET TXT("DFN")=ADFN
  NEW LTXTIDX SET LTXTIDX=$O(TXT("TEXT",99999),-1)
  SET TXT("TEXT",LTXTIDX)=$G(TXT("TEXT",LTXTIDX))_"<P>"  ;" Added in case a table is at the end of the topic text
  NEW OPTION SET OPTION("HTML")=1
  SET OPTION("PREFIX_BREAK")=1
  KILL TEMP
  
  DO FRSHTABL^TMGTIUP3(.TEMP,.TXT,.OPTION)  
  ;"SET OUT(1)="<b><u>"_TOPIC_":</b></U>"_$P($G(OUT(1)),"^",3)
  ;"FORMAT OUT LIST AS NEEDED FOR RPC RESULTS.
  NEW IDX,LASTIDX SET IDX=0,LASTIDX=0
  FOR  SET IDX=$O(TEMP(IDX)) QUIT:IDX'>0  DO
  . SET OUT(IDX)=$G(TEMP(IDX))
  . SET LASTIDX=IDX
  . IF IDX=1 SET OUT(IDX)=PREFIX_"<u>"_TOPIC_"</u>: <i>"_$P($G(OUT(IDX)),"^",3)_"</i>"
  SET OUT(LASTIDX)=$G(OUT(LASTIDX))_SUFFIX
  ;
GFRDN ;
  QUIT
  ;
  ;"==========================================================
  ;
TOPRBLNK(OUT,IN)  ;"TOPIC-PROBLEM LINK entry point
  ;"RPC ENTRY POINT FOR: TMG CPRS TOPIC LINKS    ... was TMG CPRS TOPIC PROBLEM LINK
  ;"Input: OUT -- PASS BY REFERENCE, AN OUT PARAMETER.  Format as per Output below
  ;"       IN --  PASS BY REFERENCE, Format:
  ;"          NOTE: one or many lines can be passed.  Each is a separate command,
  ;"              and are executed in the number order #
  ;"          IN(#)='<CMD>^<Info>...'
  ;"            <CMD> can be:
  ;"               'SET', in which case <Info> should be as follows:
  ;"                   'SET^<DFN>^<TopicName>^<ProblemIEN>^<ICD_INFO*>^<SCTIEN>'  <-- can have any combo of IENS's    
  ;"                   'SET^1234^A-fib^5678'
  ;"                   NOTE: ProblemIEN is IEN in 9000011, SCT is snowmed code in ____ (to do!)
  ;"                   NOTE: *For ICD_INFO: Should be:
  ;"                        '<ICD_IEN>;<ICD_CODE>;<ICD_CODING_SYS>;<AS-OF FMDT>' 
  ;"                        ICD_IEN is IEN in 80.   If valid IEN provided, then rest is optional and ignored
  ;"                        ICD_CODE e.g. I10 for Essential HTN
  ;"                        ICD_CODING_SYS.  e.g. 10D for ICD 10
  ;"                        AS-OF FMDT: FMDT for date lookup.  Default is NOW 
  ;"               'GET', in which case <Info> should be as follows:
  ;"                   'GET^<DFN>^PROB=<ProblemIEN>'
  ;"                or 'GET^<DFN>^TOPIC=<TopicName>'
  ;"                or 'GET^<DFN>^ICD=<ICDIEN>'
  ;"                or 'GET^<DFN>^SCT=<SCTIEN>'  <-- to do! 
  ;"                or 'GET^<DFN>^ALL'
  ;"               'KILL', in which case <Info> should be as follows:
  ;"                   'KILL^<DFN>^<TopicName>'
  ;"Results: none
  ;"Output: Out(0)=1^OK   <-- only if all filings were OK, otherwise -1^Problem
  ;"        Out(#)=1^OK  or -1^ErrorMessage  <-- if line command was a SET or KILL command
  ;"        Out(#)=<result value>   <-- if line command was a GET command.
  ;"         e.g.   1^OK^DFN^PROB=<ProblemIEN>^TOPIC=<Name>,<Name>,<Name>,....   <-- if input was PROB=<IEN>
  ;"          or    1^OK^DFN^ICD=<ICDIEN>^^TOPIC=<Name>,<Name>,<Name>,....       <-- if input was ICD=<IEN>
  ;"          or    1^OK^DFN^TOPIC=<TopicName>^PROB=<IEN>                        <-- if input was TOPIC=<Name>
  ;"          or    1^OK^DFN^TOPIC=<TopicName>^PROB=<IEN>^ICD=<ICDIEN>^SCT=<SCTIEN> <-- if input was ALL  (SCT IS STILL TO DO...)
  ;"          or   -1^Error Message
  ;"        The # will match the # from the IN array
  NEW TMGZZDB SET TMGZZDB=0
  IF TMGZZDB=0 DO
  . KILL ^TMG("TMP","RPC","TOPRBLNK^TMGTIUT3")
  . MERGE ^TMG("TMP","RPC","TOPRBLNK^TMGTIUT3","IN")=IN
  ELSE  DO
  . KILL IN
  . MERGE IN=^TMG("TMP","RPC","TOPRBLNK^TMGTIUT3","IN")
  KILL OUT NEW RESULT SET RESULT="1^OK"  ;"default to success 
  NEW IDX SET IDX=""
  FOR  SET IDX=$ORDER(IN(IDX)) QUIT:+IDX'>0  DO
  . NEW LINE SET LINE=$GET(IN(IDX))
  . NEW CMD SET CMD=$PIECE(LINE,"^",1)
  . NEW ONERESULT SET ONERESULT="1^OK"
  . IF CMD="SET" DO
  . . ;"SET ONERESULT=$$HNDLSET(LINE)
  . . SET ONERESULT=$$HNDLADDL(LINE)
  . ELSE  IF CMD="GET" DO
  . . SET ONERESULT=$$HNDLGET(LINE)
  . ELSE  IF CMD="KILL" DO
  . . SET ONERESULT=$$HNDLKILL(LINE)
  . ELSE  IF CMD="ADDL" DO
  . . SET ONERESULT=$$HNDLADDL(LINE)
  . ELSE  DO
  . . SET ONERESULT="-1^Invalid command ["_CMD_"] received."
  . IF +ONERESULT'=1 SET RESULT="-1^Problem"
  . SET OUT(IDX)=$P(ONERESULT,"^",2)
  SET OUT(0)=RESULT
  QUIT
  ;
HNDLSET(LINE)  ;"Handle SET command - Can we delete this?
  ;"Input: LINE -- 'SET^<DFN>^<TopicName>^<ProblemIEN>^<ICD_INFO*>^<SCTIEN>'  <-- can have any combo of IENS's / info    
  ;"               'SET^1234^A-fib^5678'
  ;"               NOTE: ProblemIEN is IEN in 9000011, SCT is snowmed code in ____ (to do!)
  ;"               NOTE: *For ICD_INFO: Should be:
  ;"                    '<ICD_IEN>;<ICD_CODE>;<ICD_CODING_SYS>;<AS-OF FMDT>' 
  ;"                    ICD_IEN is IEN in 80.   If valid IEN provided, then rest is optional and ignored
  ;"                    ICD_CODE e.g. I10 for Essential HTN
  ;"                    ICD_CODING_SYS.  e.g. 10D for ICD 10
  ;"                    AS-OF FMDT: FMDT for date lookup.  Default is NOW 
  ;"Result: '1^OK' or '-1^Error message'
  NEW RESULT SET RESULT="1^OK"
  NEW TMGFDA,TMGIEN,TMGMSG
  SET LINE=$GET(LINE)
  NEW ADFN SET ADFN=+$PIECE(LINE,"^",2) IF ADFN'>0 DO  GOTO HNDSDN
  . SET RESULT="-1^Numeric DFN not provided in piece #2 of '"_LINE_"'"
  IF '$DATA(^DPT(ADFN)) DO  GOTO HNDSDN
  . SET RESULT="-1^DFN provided in piece #2 doesn't exist in PATIENT file (#2).  Input: '"_LINE_"'"
  NEW TOPIEN SET TOPIEN=+$ORDER(^TMG(22719.5,"B",ADFN,0))
  IF TOPIEN>0 GOTO HNDS2
  SET TMGFDA(22719.5,"+1,",.01)=ADFN
  SET TMGIEN(1)=ADFN   ;"<--- RECORD NUMBER SHOULD MATCH PATIENT IEN (DFN)
  DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG) DO  GOTO HNDSDN
  . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  SET TOPIEN=$GET(TMGIEN(1))
  IF TOPIEN'>0 DO  GOTO HNDSDN
  . SET RESULT="-1^Unable to determine IEN of added record in file 22719.5"  
HNDS2 ;"Add subrecord or modify existing subrecord. 
  NEW TOPIC SET TOPIC=$PIECE(LINE,"^",3)
  IF TOPIC="" DO  GOTO HNDSDN
  . SET RESULT="-1^Topic name not provide in piece #3.  Input: '"_LINE_"'"
  NEW PROBIEN SET PROBIEN=$PIECE(LINE,"^",4)
  NEW ICDINFO SET ICDINFO=$PIECE(LINE,"^",5)
  NEW ICDIEN SET ICDIEN=0
  IF ICDINFO'="" DO  GOTO:(+RESULT=-1) HNDSDN
  . SET ICDIEN=+$PIECE(ICDINFO,";",1)
  . IF (+ICDIEN>0),$DATA(^ICD9(ICDIEN))'>0 DO  QUIT
  . . SET RESULT="-1^ICD IEN provided in piece #5 doesn't exist in ICD DIAGNOSIS file (#80).  Input: '"_LINE_"'"
  . NEW ICDCODE SET ICDCODE=$PIECE(ICDINFO,";",2) 
  . IF ICDCODE="@" SET ICDIEN="@" QUIT
  . NEW CSYS SET CSYS=$PIECE(ICDINFO,";",3)
  . NEW CDT SET CDT=$PIECE(ICDINFO,";",4)
  . SET ICDIEN=+$$ICDDATA(CSYS,ICDCODE,CDT,"E")
  . IF ICDIEN'>0 DO  QUIT
  . . SET RESULT="-1^Invalid ICD info provided in piece#4: ["_ICDINFO_"]"
  NEW SCTIEN SET SCTIEN=+$PIECE(LINE,"^",6)  ;"<-- needs to be added to fileman file still!!
  IF (+PROBIEN'>0)&((ICDIEN'>0)&(ICDIEN'="@"))&(SCTIEN'>0) DO  GOTO HNDSDN
  . SET RESULT="-1^Numeric PROBLEM or ICD or Snowmed IEN not provided in piece #4 or #5 or #6 of '"_LINE_"'"
  IF (+PROBIEN>0),'$DATA(^AUPNPROB(PROBIEN)) DO  GOTO HNDSDN
  . SET RESULT="-1^PROBLEM IEN provided in piece #4 doesn't exist in PROBLEM file (#9000011).  Input: '"_LINE_"'"
  ;"IF (+SCTIEN>0),'$DATA(^?????(SCTIEN)) DO  GOTO HNDSDN
  ;". SET RESULT="-1^SNOWMED IEN provided in piece #6 doesn't exist in ?????????????.  Input: '"_LINE_"'"
  IF +PROBIEN>0 DO  GOTO:+RESULT<0 HNDSDN  
  . NEW P2DFN SET P2DFN=+$PIECE($GET(^AUPNPROB(+PROBIEN,0)),"^",2)
  . IF P2DFN'=ADFN DO  
  . . SET RESULT="-1^PROBLEM IEN points to a record for a PATIENT `"_P2DFN_", which is different from that provided in piece #2.  Input: '"_LINE_"'"  
  NEW SUBIEN SET SUBIEN=+$ORDER(^TMG(22719.5,TOPIEN,1,"B",$EXTRACT(TOPIC,1,30),0))
  IF SUBIEN'>0 DO
  . ;"Add new subrecord
  . KILL TMGFDA,TMGIEN,TMGMSG
  . NEW IENS SET IENS="+1,"_TOPIEN_","
  . SET TMGFDA(22719.51,IENS,.01)=TOPIC
  . IF PROBIEN>0 SET TMGFDA(22719.51,IENS,.02)=+PROBIEN 
  . ;"IF ICDIEN>0 SET TMGFDA(22719.51,IENS,.03)=+ICDIEN
  . ;"IF SCTIEN>0 SET TMGFDA(22719.51,IENS,.04)=+SCTIEN   <-- TO DO!!
  . IF $D(TMGFDA) DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  . IF $DATA(TMGMSG) DO  QUIT 
  . . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  . SET SUBIEN=+$G(TMGIEN(1))
  ;" KEEP RUNNING FOR SUBITEM NOW. WAS -> GOTO HNDSDN
  ;"Add new sub-subrecord
  IF +SUBIEN'>0 DO  QUIT
  . SET RESULT="-1^ERROR WITH FINDING SUBIEN"
  KILL TMGFDA,TMGIEN,TMGMSG
  NEW IENS SET IENS="+1,"_SUBIEN_","_TOPIEN_","
  SET TMGFDA(22719.511,IENS,.01)=+ICDIEN
  ;"IF PROBIEN>0 SET TMGFDA(22719.51,IENS,.02)=+PROBIEN 
  ;"IF ICDIEN>0 SET TMGFDA(22719.51,IENS,.03)=+ICDIEN
  ;"IF SCTIEN>0 SET TMGFDA(22719.51,IENS,.04)=+SCTIEN   <-- TO DO!!
  IF $D(TMGFDA) DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG) DO  GOTO HNDADN
  . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
HNDS3 ;"Overwrite .02 and/or .03 field of existing record.  
  NEW IENS SET IENS=SUBIEN_","_TOPIEN_","
  IF PROBIEN>0 SET TMGFDA(22719.51,IENS,.02)="`"_+PROBIEN 
  ELSE  IF PROBIEN="@" SET TMGFDA(22719.51,IENS,.02)=PROBIEN 
  ;"IF ICDIEN>0 SET TMGFDA(22719.51,IENS,.03)="`"_+ICDIEN
  ;"ELSE  IF ICDIEN="@" SET TMGFDA(22719.51,IENS,.03)=ICDIEN
  ;"IF SCTIEN>0 SET TMGFDA(22719.51,IENS,.04)="`"_+SCTIEN
  ;"ELSE  IF ICDIEN="@" SET TMGFDA(22719.51,IENS,.04)=SCTIEN
  IF $D(TMGFDA) DO FILE^DIE("E","TMGFDA","TMGMSG")
  IF $DATA(TMGMSG) DO  GOTO HNDSDN
  . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  ;"GOTO HNDSDN
HNDSDN ;  
  QUIT RESULT
  ;
HNDLADDL(LINE)    ;"Handle SET command
  ;"Input: LINE -- 'ADDL^<DFN>^<TopicName>^<ProblemIEN>^<ICD_INFO*>^<SCTIEN>'  <-- can have any combo of IENS's / info    
  ;"               'ADDL^1234^A-fib^5678'
  ;"               NOTE: ProblemIEN is IEN in 9000011, SCT is snowmed code in ____ (to do!)
  ;"               NOTE: *For ICD_INFO: Should be:
  ;"                    '<ICD_IEN>;<ICD_CODE>;<ICD_CODING_SYS>;<AS-OF FMDT>' 
  ;"                    ICD_IEN is IEN in 80.   If valid IEN provided, then rest is optional and ignored
  ;"                    ICD_CODE e.g. I10 for Essential HTN
  ;"                    ICD_CODING_SYS.  e.g. 10D for ICD 10
  ;"                    AS-OF FMDT: FMDT for date lookup.  Default is NOW 
  ;"Result: '1^OK' or '-1^Error message'  
    NEW RESULT SET RESULT="1^OK"
  NEW TMGFDA,TMGIEN,TMGMSG
  SET LINE=$GET(LINE)
  NEW ADFN SET ADFN=+$PIECE(LINE,"^",2) IF ADFN'>0 DO  GOTO HNDADN
  . SET RESULT="-1^Numeric DFN not provided in piece #2 of '"_LINE_"'"
  IF '$DATA(^DPT(ADFN)) DO  GOTO HNDADN
  . SET RESULT="-1^DFN provided in piece #2 doesn't exist in PATIENT file (#2).  Input: '"_LINE_"'"
  NEW TOPIEN SET TOPIEN=+$ORDER(^TMG(22719.5,"B",ADFN,0))
  IF TOPIEN>0 GOTO HNDA2
  SET TMGFDA(22719.5,"+1,",.01)=ADFN
  SET TMGIEN(1)=ADFN   ;"<--- RECORD NUMBER SHOULD MATCH PATIENT IEN (DFN)
  DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG) DO  GOTO HNDADN
  . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  SET TOPIEN=$GET(TMGIEN(1))
  IF TOPIEN'>0 DO  GOTO HNDADN
  . SET RESULT="-1^Unable to determine IEN of added record in file 22719.5"  
HNDA2 ;"Add subrecord or modify existing subrecord. 
  NEW TOPIC SET TOPIC=$PIECE(LINE,"^",3)
  IF TOPIC="" DO  GOTO HNDADN
  . SET RESULT="-1^Topic name not provide in piece #3.  Input: '"_LINE_"'"
  NEW PROBIEN SET PROBIEN=$PIECE(LINE,"^",4)
  NEW ICDINFO SET ICDINFO=$PIECE(LINE,"^",5)
  NEW ICDIEN SET ICDIEN=0
  IF ICDINFO'="" DO  GOTO:(+RESULT=-1) HNDADN
  . SET ICDIEN=+$PIECE(ICDINFO,";",1)
  . IF (+ICDIEN>0),$DATA(^ICD9(ICDIEN))'>0 DO  QUIT
  . . SET RESULT="-1^ICD IEN provided in piece #5 doesn't exist in ICD DIAGNOSIS file (#80).  Input: '"_LINE_"'"
  . NEW ICDCODE SET ICDCODE=$PIECE(ICDINFO,";",2) 
  . IF ICDCODE="@" SET ICDIEN="@" QUIT
  . NEW CSYS SET CSYS=$PIECE(ICDINFO,";",3)
  . NEW CDT SET CDT=$PIECE(ICDINFO,";",4)
  . SET ICDIEN=+$$ICDDATA(CSYS,ICDCODE,CDT,"E")
  . IF ICDIEN'>0 DO  QUIT
  . . SET RESULT="-1^Invalid ICD info provided in piece#4: ["_ICDINFO_"]"
  NEW SCTIEN SET SCTIEN=+$PIECE(LINE,"^",6)  ;"<-- needs to be added to fileman file still!!
  IF (+PROBIEN'>0)&((ICDIEN'>0)&(ICDIEN'="@"))&(SCTIEN'>0) DO  GOTO HNDADN
  . SET RESULT="-1^Numeric PROBLEM or ICD or Snowmed IEN not provided in piece #4 or #5 or #6 of '"_LINE_"'"
  IF (+PROBIEN>0),'$DATA(^AUPNPROB(PROBIEN)) DO  GOTO HNDADN
  . SET RESULT="-1^PROBLEM IEN provided in piece #4 doesn't exist in PROBLEM file (#9000011).  Input: '"_LINE_"'"
  ;"IF (+SCTIEN>0),'$DATA(^?????(SCTIEN)) DO  GOTO HNDADN
  ;". SET RESULT="-1^SNOWMED IEN provided in piece #6 doesn't exist in ?????????????.  Input: '"_LINE_"'"
  IF +PROBIEN>0 DO  GOTO:+RESULT<0 HNDADN  
  . NEW P2DFN SET P2DFN=+$PIECE($GET(^AUPNPROB(+PROBIEN,0)),"^",2)
  . IF P2DFN'=ADFN DO  
  . . SET RESULT="-1^PROBLEM IEN points to a record for a PATIENT `"_P2DFN_", which is different from that provided in piece #2.  Input: '"_LINE_"'"  
  NEW SUBIEN SET SUBIEN=+$ORDER(^TMG(22719.5,TOPIEN,1,"B",$EXTRACT(TOPIC,1,30),0))
  IF SUBIEN'>0 DO
  . ;"Add new subrecord
  . KILL TMGFDA,TMGIEN,TMGMSG
  . NEW IENS SET IENS="+1,"_TOPIEN_","
  . SET TMGFDA(22719.51,IENS,.01)=TOPIC
  . IF PROBIEN>0 SET TMGFDA(22719.51,IENS,.02)=+PROBIEN 
  . ;"IF ICDIEN>0 SET TMGFDA(22719.51,IENS,.03)=+ICDIEN
  . ;"IF SCTIEN>0 SET TMGFDA(22719.51,IENS,.04)=+SCTIEN   <-- TO DO!!
  . IF $D(TMGFDA) DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  . IF $DATA(TMGMSG) DO  QUIT 
  . . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  . SET SUBIEN=+$G(TMGIEN(1))
  IF SUBIEN'>0 DO  GOTO HNDADN
  . SET RESULT="-1^CANNOT FIND SUBRECORD"
  ;"Does this problem exist?
  NEW TEMPIDX SET TEMPIDX=0
  NEW SUBEXISTS SET SUBEXISTS=0
  FOR  SET TEMPIDX=$O(^TMG(22719.5,ADFN,1,SUBIEN,1,TEMPIDX)) QUIT:TEMPIDX'>0  DO
  . NEW SUBICD SET SUBICD=$G(^TMG(22719.5,ADFN,1,SUBIEN,1,TEMPIDX,0))
  . IF SUBICD=ICDIEN SET SUBEXISTS=1
  IF SUBEXISTS=1 DO  GOTO HNDADN
  . SET RESULT="-1^THIS ICD IS ALREADY LINKED"  	  
  ;"
  ;"Add new sub-subrecord
  KILL TMGFDA,TMGIEN,TMGMSG
  NEW IENS SET IENS="+1,"_SUBIEN_","_TOPIEN_","
  SET TMGFDA(22719.511,IENS,.01)=+ICDIEN
  ;"IF PROBIEN>0 SET TMGFDA(22719.51,IENS,.02)=+PROBIEN 
  ;"IF ICDIEN>0 SET TMGFDA(22719.51,IENS,.03)=+ICDIEN
  ;"IF SCTIEN>0 SET TMGFDA(22719.51,IENS,.04)=+SCTIEN   <-- TO DO!!
  DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG) DO  GOTO HNDADN
  . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  ;GOTO HNDADN
HNDA3 ;"Overwrite .02 and/or .03 field of existing record.  
  ;"NEW IENS SET IENS=SUBIEN_","_TOPIEN_","
  ;"IF PROBIEN>0 SET TMGFDA(22719.51,IENS,.02)="`"_+PROBIEN 
  ;"ELSE  IF PROBIEN="@" SET TMGFDA(22719.51,IENS,.02)=PROBIEN 
  ;"IF ICDIEN>0 SET TMGFDA(22719.51,IENS,.03)="`"_+ICDIEN
  ;"ELSE  IF ICDIEN="@" SET TMGFDA(22719.51,IENS,.03)=ICDIEN
  ;";"IF SCTIEN>0 SET TMGFDA(22719.51,IENS,.04)="`"_+SCTIEN
  ;";"ELSE  IF ICDIEN="@" SET TMGFDA(22719.51,IENS,.04)=SCTIEN
  ;"DO FILE^DIE("E","TMGFDA","TMGMSG")
  ;"IF $DATA(TMGMSG) DO  GOTO HNDADN
  ;". SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  ;"GOTO HNDADN
HNDADN ;  
  QUIT RESULT
  ;"
HNDLGET(LINE)  ;"Handle GET command.
  ;"Input: LINE.  Format
  ;"          'GET^<DFN>^PROB=<ProblemIEN>'
  ;"       or 'GET^<DFN>^ICD=<ICDIEN>'
  ;"       or 'GET^<DFN>^SCT=<SCTIEN>'
  ;"       or 'GET^<DFN>^TOPIC=<TopicName>'
  ;"Result: 1^OK^DFN^PROB=<ProblemIEN>^TOPIC=<Name>,<Name>,<Name>,....   <-- if input was PROB=<IEN>
  ;"  or    1^OK^DFN^ICD=<ICDIEN>^^TOPIC=<Name>,<Name>,<Name>,....       <-- if input was ICD=<IEN>
  ;"  or    1^OK^DFN^SCT=<SCTIEN>^^TOPIC=<Name>,<Name>,<Name>,....       <-- if input was SCT=<IEN>
  ;"  or    1^OK^DFN^TOPIC=<TopicName>^PROB=<IEN>^ICD=<IEN>              <-- if input was TOPIC=<Name> OR ALL
  ;"  or   -1^Error Message
  NEW RESULT SET RESULT="1^OK"
  NEW TMGFDA,TMGIEN,TMGMSG
  SET LINE=$GET(LINE)
  NEW ADFN SET ADFN=+$PIECE(LINE,"^",2) IF ADFN'>0 DO  GOTO HNDGDN
  . SET RESULT="-1^Numeric DFN not provided in piece #2 of '"_LINE_"'"
  IF '$DATA(^DPT(ADFN)) DO  GOTO HNDGDN
  . SET RESULT="-1^DFN provided in piece #2 doesn't exist in PATIENT file (#2).  Input: '"_LINE_"'"
  SET RESULT=RESULT_"^"_ADFN
  NEW TOPIEN SET TOPIEN=+$ORDER(^TMG(22719.5,"B",ADFN,0))  
  IF TOPIEN'>0 DO  GOTO HNDGDN
  . SET RESULT="-1^Record in file 22719.5 doesn't exist for DFN of "_ADFN_".  Input: '"_LINE_"'"
  NEW MODE SET MODE=$PIECE(LINE,"^",3)
  SET RESULT=RESULT_"^"_MODE
  NEW VALUE SET VALUE=$PIECE(MODE,"=",2)
  NEW MODECMD SET MODECMD=$PIECE(MODE,"=",1)
  NEW SUBIEN SET SUBIEN=0
  IF ((MODECMD="PROB")!(MODECMD="ICD")!(MODECMD="SCT"))&(+VALUE>0) DO
  . NEW XREF SET XREF=$SELECT(MODE="ICD":"DX",1=1:"AC")  ;"<--- TO DO!!  ADD XREF FOR SCT
  . SET RESULT=RESULT_"^TOPIC="
  . FOR  SET SUBIEN=$ORDER(^TMG(22719.5,TOPIEN,1,XREF,+VALUE,SUBIEN)) QUIT:+SUBIEN'>0  DO
  . . NEW ATOPIC SET ATOPIC=$PIECE($GET(^TMG(22719.5,TOPIEN,1,SUBIEN,0)),"^",1)
  . . IF $EXTRACT(RESULT,$LENGTH(RESULT))'="=" SET RESULT=RESULT_","
  . . SET RESULT=RESULT_ATOPIC
  ELSE  IF ((MODECMD="TOPIC")!(MODECMD="ALL"))&(VALUE'="") DO
  . SET SUBIEN=$ORDER(^TMG(22719.5,TOPIEN,1,"B",$EXTRACT(VALUE,1,30),0))
  . IF SUBIEN'>0 DO  QUIT
  . . SET RESULT="-1^Topic '"_VALUE_"' not found in record #"_TOPIEN_".    Input: '"_LINE_"'"
  . NEW ZN SET ZN=$GET(^TMG(22719.5,TOPIEN,1,SUBIEN,0))
  . SET RESULT=RESULT_"^PROB="_$PIECE(ZN,"^",2)
  . SET RESULT=RESULT_"^ICD="_$PIECE(ZN,"^",3)
  . ;"SET RESULT=RESULT_"^SCT="_$PIECE(ZN,"^",4)  <-- TO DO!!
  ;"ELSE  IF (MODECMD="ALL") DO
  ;". SET RESULT=RESULT_"^ALL:"
  ;". FOR  SET SUBIEN=$ORDER(^TMG(22719.5,TOPIEN,1,SUBIEN)) QUIT:+SUBIEN'>0  DO
  ;". . NEW ZN SET ZN=$GET(^TMG(22719.5,TOPIEN,1,SUBIEN,0))
  ;". . IF $EXTRACT(RESULT,$LENGTH(RESULT))'=":" SET RESULT=RESULT_","
  ;". . SET RESULT=RESULT_$PIECE(ZN,"^",1)_"="_$PIECE(ZN,"^",2)
  ELSE  DO
  . SET RESULT="-1^Invalid mode in piece #3.  Expected 'PROB=<IEN>' or 'ICD=<IEN>' or 'TOPIC=<Name>' or 'ALL'.  Input: '"_LINE_"'"
HNDGDN  
  QUIT RESULT
  ;
HNDLKILL(LINE)  ;"Handle KILL command.
  ;"Input: LINE.  Format
  ;"         'KILL^<DFN>^<TopicName>'
  ;"Result: 1^OK  or -1^Error Message
  NEW RESULT SET RESULT="1^OK"
  SET LINE=$GET(LINE)
  NEW ADFN SET ADFN=+$PIECE(LINE,"^",2) IF ADFN'>0 DO  GOTO HNDKDN
  . SET RESULT="-1^Numeric DFN not provided in piece #2 of '"_LINE_"'"
  IF '$DATA(^DPT(ADFN)) DO  GOTO HNDKDN
  . SET RESULT="-1^DFN provided in piece #2 doesn't exist in PATIENT file (#2).  Input: '"_LINE_"'"
  NEW TOPIEN SET TOPIEN=+$ORDER(^TMG(22719.5,"B",ADFN,0))  
  IF TOPIEN'>0 DO  GOTO HNDKDN
  . SET RESULT="-1^Record in file 22719.5 doesn't exist for DFN of "_ADFN_".  Input: '"_LINE_"'"
  NEW TOPIC SET TOPIC=$PIECE(LINE,"^",3)
  IF TOPIC["TOPIC=" SET TOPIC=$PIECE(TOPIC,"TOPIC=",2)
  IF TOPIC="" DO  GOTO HNDKDN
  . SET RESULT="-1^Topic name not provide in piece #3.  Input: '"_LINE_"'"
  NEW SUBIEN SET SUBIEN=$ORDER(^TMG(22719.5,TOPIEN,1,"B",$EXTRACT(TOPIC,1,30),0))
  IF SUBIEN'>0 GOTO HNDKDN  ;"Don't raise error if record doesn't exist to kill 
  NEW TEMPIDX SET TEMPIDX=0
  NEW ICDIDX SET ICDIDX=0
  NEW ICDINFO SET ICDINFO=$PIECE(LINE,"^",5)
  NEW ICDCODE SET ICDCODE=$P(ICDINFO,";",2)
  IF ICDCODE["#" SET ICDCODE=$P(ICDCODE,"#",2)
  NEW ICDIEN SET ICDIEN=$O(^ICD9("ABA",30,ICDCODE_" ",0))
  FOR  SET TEMPIDX=$O(^TMG(22719.5,ADFN,1,SUBIEN,1,TEMPIDX)) QUIT:(TEMPIDX'>0)!(ICDIDX>0)  DO
  . NEW SUBICD SET SUBICD=$G(^TMG(22719.5,ADFN,1,SUBIEN,1,TEMPIDX,0))
  . IF SUBICD=ICDIEN SET ICDIDX=TEMPIDX
  IF ICDIDX'>0 DO  GOTO HNDKDN
  . SET RESULT="-1^CANNOT LOCATE ICD IN TOPIC"
  NEW IENS SET IENS=ICDIDX_","_SUBIEN_","_TOPIEN_","
  SET TMGFDA(22719.511,IENS,.01)="@"
  DO FILE^DIE("E","TMGFDA","TMGMSG")
  IF $DATA(TMGMSG) DO  GOTO HNDKDN
  . SET RESULT="-1^"+$$GETERRST^TMGDEBU2(.TMGMSG)
HNDKDN ;  
  QUIT RESULT
  ;
TOPICLST(OUT,ADFN,STARTING,DIR,MAX)  ;"List all topics in file 22719.51 (a subfile)
  ;"RPC ENTRY POINT FOR: TMG CPRS TOPIC SUBSET
  ;"Input: OUT -- PASS BY REFERENCE, an OUT PARAMETER.
  ;"       ADFN -- Patient IEN to retrieve results for
  ;"       STARTING -- Optional.  If provided, then list returns starts right AFTER this via $ORDER
  ;"       DIR -- Optional.  Default=1.  If -1, then direction is reversed
  ;"       MAX -- Optional.  Default=42.  Max number of results to return
  ;"Results: none
  ;"Output: OUT is filled as follows.
  ;"        OUT(0)="1^OK" or "-1^Error Message"
  ;"        OUT(#)=<IENS>^Name^<ProbIEN>^<ICDIEN>
  KILL OUT
  NEW RESULT SET RESULT="1^OK"
  SET ADFN=$GET(ADFN) IF ADFN'>0 DO  GOTO TPLSTDN
  . SET RESULT="-1^Patient DFN not provided"
  IF '$DATA(^DPT(ADFN)) DO  GOTO TPLSTDN
  . SET RESULT="-1^DFN provided doesn't exist in PATIENT file (#2)"
  NEW TOPIEN SET TOPIEN=+$ORDER(^TMG(22719.5,"B",ADFN,0))  
  IF TOPIEN'>0 DO  GOTO TPLSTDN
  . SET RESULT="-1^Record in file 22719.5 doesn't exist for DFN of "_ADFN
  SET DIR=+$GET(DIR) IF (DIR'=1)&(DIR'=-1) SET DIR=1
  SET MAX=+$GET(MAX) IF MAX'>0 SET MAX=42
  NEW CT SET CT=1
  NEW TOPIC SET TOPIC=$$UP^XLFSTR($GET(STARTING))
  FOR  SET TOPIC=$ORDER(^TMG(22719.5,TOPIEN,1,"B",TOPIC),DIR) QUIT:(TOPIC="")!(CT>MAX)  DO
  . NEW SUBIEN SET SUBIEN=+$ORDER(^TMG(22719.5,TOPIEN,1,"B",TOPIC,0)) QUIT:+SUBIEN'>0
  . NEW ZN SET ZN=$GET(^TMG(22719.5,TOPIEN,1,SUBIEN,0))
  . SET OUT(CT)=SUBIEN_","_TOPIEN_",^"_$PIECE(ZN,"^",1,3),CT=CT+1  
TPLSTDN ;
  SET OUT(0)=RESULT
  QUIT
  ;
LKSUGEST(OUT,TOPIC)  ;"Suggest info for problems to create, or ICD's, for given topic  
  ;"RPC ENTRY POINT FOR: TMG CPRS TOPIC CODE SUGGEST
  ;"Input: OUT -- PASS BY REFERENCE, an OUT PARAMETER.
  ;"       TOPIC -- Topic name to suggest from
  ;"Results: none
  ;"Output: OUT is filled as follows.
  ;"        OUT(0)="1^OK" or "-1^Error Message"
  ;"        OUT(#)=TopicName^<"ICD" OR "10D">^<ICD CODE>^<ICD NAME>^<ProblemIEN>^<SCT CODE>^<SCT NAME>   <-- multiple entries, for each suggestion.  
  ;"        if no entries are found, then none returned.  but OUT(0) will still be "1^OK"
  ;"Code below taken (and modified heavily) from LIST^ORQQPL3
  SET TOPIC=$GET(TOPIC)
  IF TOPIC="" DO  QUIT
  . SET OUT(0)="-1^No topic name supplied"
  SET OUT(0)="1^OK"
  NEW TEMP
  SET TOPIC=$$UP^XLFSTR(TOPIC)
  NEW ATOPIC SET ATOPIC=""
  FOR  SET ATOPIC=$ORDER(^TMG(22719.5,"TOPIC",ATOPIC)) QUIT:ATOPIC=""  DO
  . IF ATOPIC'[TOPIC QUIT
  . NEW IEN SET IEN=0
  . FOR  SET IEN=$ORDER(^TMG(22719.5,"TOPIC",ATOPIC,IEN)) QUIT:+IEN'>0  DO
  . . NEW SUBIEN SET SUBIEN=0
  . . FOR  SET SUBIEN=$ORDER(^TMG(22719.5,"TOPIC",ATOPIC,IEN,SUBIEN)) QUIT:+SUBIEN'>0  DO
  . . . NEW ZN SET ZN=$GET(^TMG(22719.5,IEN,1,SUBIEN,0))
  . . . NEW PROBIEN SET PROBIEN=+$PIECE(ZN,"^",2)
  . . . IF PROBIEN>0 DO SUGST4PROB(.TEMP,ATOPIC,PROBIEN)
  . . . ;"NEW ICDIEN SET ICDIEN=$PIECE(ZN,"^",3)
  . . . NEW ICDIDX SET ICDIDX=0
  . . . FOR  SET ICDIDX=$O(^TMG(22719.5,IEN,1,SUBIEN,1,ICDIDX)) QUIT:ICDIDX'>0  DO
  . . . . NEW ICDIEN SET ICDIEN=+$G(^TMG(22719.5,IEN,1,SUBIEN,1,ICDIDX,0))
  . . . . DO SUGST4ICD(.TEMP,ATOPIC,ICDIEN)
  NEW CNT SET CNT=0
  NEW TMGLN SET TMGLN=""
  FOR  SET TMGLN=$ORDER(TEMP(TMGLN)) QUIT:TMGLN=""  DO
  . SET CNT=CNT+1,OUT(CNT)=TMGLN
  QUIT
  ;
SUGST4ICD(TEMP,TOPICNAME,ICDIEN) ;"Utility function for LKSUGEST above
  NEW CSINFO,ICDINFO
  SET CSINFO=$$ICDCODESYS(ICDIEN,.ICDINFO) ;"Result: -1^Error message, or  <CodeSys ShortName>^<CodeSys LongName>^<CodeSysIEN>
  IF +ICDINFO=-1 QUIT
  NEW CODESYS SET CODESYS=$PIECE(CSINFO,"^",1)
  NEW ICDCODE SET ICDCODE=$PIECE(ICDINFO,"^",2)
  NEW ICDDESCR SET ICDDESCR=$PIECE(ICDINFO,"^",4)
  SET TMGLN=TOPICNAME_U_CODESYS_U_ICDCODE_U_ICDDESCR
  SET TEMP(TMGLN)=""
  QUIT
  ;
SUGST4PROB(TEMP,TOPICNAME,PROBIEN) ;"Utility function for LKSUGEST above
  ;"Input: OUT: output, see format above
  ;"       PROBIEN is pointer to file 9000011
  NEW IMPLDT SET IMPLDT=$$IMPDATE^LEXU("10D")
  NEW NOWDT SET NOWDT=$$NOW^XLFDT
  NEW N0,GMPL1,GMPL800,GMPL802,I,SCTCODE,ST,ICDCODE,DTREC,ICDDESCR,ORDTINT,CODESYS
  NEW ORTOTAL,LIN,INACT
  SET (ICDDESCR,INACT)=""
  NEW TMGLN SET TMGLN=""
  SET N0=$GET(^AUPNPROB(PROBIEN,0)),GMPL1=$GET(^AUPNPROB(PROBIEN,1))
  NEW ICDIEN SET ICDIEN=+N0
  SET GMPL800=$GET(^AUPNPROB(PROBIEN,800)),GMPL802=$GET(^AUPNPROB(PROBIEN,802))
  SET SCTCODE=$PIECE(GMPL800,U,1)
  NEW SCTNAME SET SCTNAME=""
  IF SCTCODE'="" DO
  . NEW IEN757D02 SET IEN757D02=$ORDER(^LEX(757.02,"APCODE",SCTCODE_" ",0)) QUIT:IEN757D02'>0
  . NEW IEN757D01 SET IEN757D01=+$GET(^LEX(757.02,IEN757D02,0)) QUIT:IEN757D01'>0
  . SET SCTNAME=$$UP^XLFSTR($PIECE($GET(^LEX(757.01,IEN757D01,0)),"^",1))
  SET ST=$PIECE(ICDIEN,U,12) ;" .12 STATUS: A=ACTIVE I=INACTIVE
  IF ST'="A" QUIT  ;"//kt added
  NEW LEX
  SET ORDTINT=$SELECT(+$PIECE(GMPL802,U,1):$PIECE(GMPL802,U,1),1:$PIECE(ICDIEN,U,8))
  SET CODESYS=$SELECT($PIECE(GMPL802,U,2)]"":$PIECE(GMPL802,U,2),1:$$SAB^ICDEX($$CSI^ICDEX(80,ICDIEN),ORDTINT))
  IF CODESYS="" SET CODESYS="ICD"  ;"//kt added
  SET ICDCODE=$PIECE($$ICDDATA(CODESYS,+ICDIEN,ORDTINT,"I"),U,2)
  IF (NOWDT<IMPLDT),(+$$STATCHK^ICDXCODE($$CSI^ICDEX(80,+ICDIEN),ICDCODE,NOWDT)'=1) SET INACT="#"
  IF +$GET(SCTCODE),(+$$STATCHK^LEXSRC2(SCTCODE,NOWDT,.LEX)'=1) SET INACT="$"
  IF INACT'="" QUIT  ;"//kt added
  IF $DATA(^AUPNPROB(PROBIEN,803)) DO
  . NEW I SET I=0
  . FOR   SET I=$ORDER(^AUPNPROB(PROBIEN,803,I)) QUIT:+I'>0   SET $PIECE(ICDCODE,"/",(I+1))=$PIECE($GET(^AUPNPROB(PROBIEN,803,I,0)),U)
  IF +ICDCODE'="" SET ICDDESCR=$$ICDDESC^GMPLUTL2(ICDCODE,NOWDT,CODESYS)
  SET DTREC=$PIECE(GMPL1,U,9)  ;"DATE RECORDED
  NEW PROBTXT SET PROBTXT=$$PROBTEXT^GMPLX(PROBIEN)
  ;"SET LIN=PROBIEN_U_PROBTXT_U_ICDCODE_U_DTREC_U_INACT_U_ICDDESCR_U_CODESYS
  SET TMGLN=TOPICNAME_U_CODESYS_U_ICDCODE_U_ICDDESCR_U_PROBIEM_U_SCTCODE_U_SCTNAME
  SET TEMP(TMGLN)=""
  QUIT
  ;
ICDDATA(CSYS,CODE,DATE,FRMT,LOC) ;"API Wrapper for ICDDATA^ICDXCODE(), with custom status checking
  ;"Input:  CSYS -- code system.  OPTIONAL.  If not provided, then first matching code system will be used. 
  ;"        CODE -- Code/IEN/variable pointer  <-- UPDATE: I don't think IEN works....
  ;"        DATE -- Code-Set Date (default = TODAY)
  ;"        FRMT -- "E" external (default)    
  ;"                "I" internal (IEN)
  ;"        LOC    Local codes    1 = Yes
  ;"                              0 = No (default)  
  ;"Output:
  ;" 
  ;"  If CODE was a diagnosis, returns an 22 piece string delimited by "^"
  ;"  
  ;"    1  IEN of code in file 80
  ;"    2  ICD-9 Dx Code                (#.01)     
  ;"       NOTE: If status is INACTIVE, then code is prefixed with "#"
  ;"    3  Identifier                   (#1.2)
  ;"    4  Versioned Dx                 (67 multiple)  <-- i.e. name
  ;"       NOTE: If status is INACTIVE, then name  is prefixed with "<INACTIVE>"
  ;"    5  Unacceptable as Principal Dx (#1.3)
  ;"    6  Major Dx Cat                 (72 multiple)
  ;"    7  MDC13                        (#1.4)
  ;"    8  Compl/Comorb                 (103 multiple)
  ;"    9  ICD Expanded                 (#1.7)
  ;"    10 Status                       (66 multiple)
  ;"    11 Sex                          (10 multiple)
  ;"    12 Inactive Date                (66 multiple)
  ;"    13 MDC24                        (#1.5)
  ;"    14 MDC25                        (#1.6)
  ;"    15 Age Low                      (11 multiple)
  ;"    16 Age High                     (12 multiple)
  ;"    17 Activation Date              (66 multiple)
  ;"    18 Message                      
  ;"    19 Complication/Comorbidity     (103 multiple)
  ;"    20 Coding System                (#1.1)
  ;"    21 Primary CC Flag              (103 multiple)
  ;"    22 PDX Exclusion Code           (#1.11)          
  ;"
  ;"   If CODE was a procedure, returns A 14 piece string delimited by "^"
  ;"   
  ;"    1  IEN of code in file 80.1
  ;"    2  ICD procedure code           (#.01)
  ;"    3  Identifier                   (#1.2)
  ;"    4  MDC24                        (#1.5)
  ;"    5  Versioned Oper/Proc          (67 multiple)
  ;"    6  <null>
  ;"    7  <null>
  ;"    8  <null>
  ;"    9  ICD Expanded                 (#1.7)
  ;"    10 Status                       (66 multiple)
  ;"    11 Use with Sex                 (10 multiple)
  ;"    12 Inactive Date                (66 multiple)
  ;"    13 Activation Date              (66 multiple)
  ;"    14 Message
  ;"    15 Coding System                (#1.1)
  ;"     
  ;"    or
  ;"
  ;"    -1^Error Description  
  NEW RESULT SET RESULT=$$ICDDATA^ICDXCODE(.CSYS,.CODE,.DATE,.FRMT,.LOC)
  IF $GET(CSYS)="",+RESULT=-1 DO
  . NEW TESTCS SET TESTCS=""
  . FOR  SET TESTCS=$ORDER(^ICDS("C",TESTCS)) QUIT:(TESTCS="")!(+RESULT'=-1)  DO
  . . SET RESULT=$$ICDDATA^ICDXCODE(TESTCS,.CODE,.DATE,.FRMT,.LOC)  ;"Try each code system.     
  NEW STATUS SET STATUS=$PIECE(RESULT,"^",10)
  IF +STATUS'=1 DO   ;"1 MEANS ACTIVE, 0 MEANS INACTIVE
  . NEW ICDCODE SET ICDCODE=$PIECE(RESULT,"^",2) QUIT:ICDCODE=""  ;"e.g. M54.5
  . SET $PIECE(RESULT,"^",2)="#"_ICDCODE
  . NEW DESCR SET DESCR=$PIECE(RESULT,"^",4) QUIT:DESCR=""  ;"e.g. Low back pain
  . SET $PIECE(RESULT,"^",4)="<INACTIVE>"_DESCR
  QUIT RESULT
  ;
TESTSGST ;
  NEW TOPIC SET TOPIC=""
  FOR  SET TOPIC=$ORDER(^TMG(22719,"HPIALL",TOPIC)) QUIT:TOPIC=""  DO
  . NEW OUT DO LKSUGEST(.OUT,TOPIC)
  . IF $GET(OUT(0))'="1^OK" DO  QUIT
  . . WRITE "ERROR: ",$PIECE(OUT(0),"^",2),! 
  . KILL OUT(0)
  . IF $DATA(OUT)=0 QUIT
  . WRITE "==============",!,TOPIC,!,"==============",!
  . ZWR OUT
  QUIT
  ;
TESTSGST2 ;
  NEW TOPIC 
  READ "Enter Topic name to test (^ to abort): ",TOPIC WRITE !
  IF TOPIC["^" QUIT
  NEW OUT DO LKSUGEST(.OUT,TOPIC)
  IF $GET(OUT(0))'="1^OK" DO  QUIT
  . WRITE "ERROR: ",$PIECE(OUT(0),"^",2),! 
  KILL OUT(0)
  IF $DATA(OUT)=0 QUIT
  WRITE "==============",!,TOPIC,!,"==============",!
  ZWR OUT
  QUIT
  ;"==========================================================
  ;
ORDERDXS(OUT,ADFN,SDT) ;"Get list of possible diagnoses for user to pick from, to be used in lab order dialog
  ;"RPC NAME:  TMG CPRS LAB ORDER GET DX LIST
  ;"Input:  OUT  -- output.  Format depends on command
  ;"        ADFN -- patient DFN
  ;"        SDT -- optional
  ;"OUTPUT:  OUT -- 
  ;"           OUT(0)="1^OK", OR "-1^<ErrorMessage>"
  ;"           OUT(#)="2^<PROBLEM INFORMATION>  <--- format as created by LIST^ORQQPL3
  ;"               FORMAT of PROBLEM INFORMATION by pieces.  
  ;"                  1    2       3         4   5       6          7   8      9       10    11      12     13      14       15             16           17              18                19                 20                    
	;"                 ifn^status^description^ICD^onset^last modified^SC^SpExp^Condition^Loc^loc.type^prov^service^priority^has comment^date recorded^SC condition(s)^inactive flag^ICD long description^ICD coding system
	;"                   NOTE: ifn = Pointer to Problem #9000011
  ;"               NOTES:
  ;"                 PIECE#1 = 3 means node is a listing of PROBLEM
  ;"                 If problem already listed in a 1 or 2 node, then it will NOT be listed here
  ;"           OUT(#)="3^<PRIOR ICD>^<ICD LONG NAME>^<FMDT LAST USED>^<ICDCODESYS>" 
  ;"               NOTES:
  ;"                 PIECE#1 = 4 means that information is for an ICD code that was previously set for a patient visit.
  ;"                 If ICD has already been included in a piece#1=1 node, then it will NOT be listed here.  
  ;"RESULT: none. But OUT(0) = 1^OK, or -1^ErrorMessage
  ;
  NEW TMGZZDB SET TMGZZDB=0
  IF TMGZZDB=1 DO
  . SET ADFN=$GET(^TMG("TMP","ORDERDXS^TMGTIIUT3","ADFN"))
  . SET CMD=$GET(^TMG("TMP","ORDERDXS^TMGTIIUT3","CMD"))
  . SET SDT=$GET(^TMG("TMP","ORDERDXS^TMGTIIUT3","SDT"))
  ELSE  DO
  . KILL ^TMG("TMP","ORDERDXS^TMGTIIUT3")
  . SET ^TMG("TMP","ORDERDXS^TMGTIIUT3","ADFN")=$GET(ADFN)
  . SET ^TMG("TMP","ORDERDXS^TMGTIIUT3","CMD")=$GET(CMD)
  . SET ^TMG("TMP","ORDERDXS^TMGTIIUT3","SDT")=$GET(SDT)
  ;    
  SET OUT(0)="1^OK"  ;"default
  SET ADFN=+$GET(ADFN)
  SET SDT=+$GET(SDT)
  NEW IDX SET IDX=1
  NEW JDX SET JDX=0
  NEW PROBLIST DO PROBLST(.PROBLIST,ADFN,SDT)
  NEW ICDLIST DO ICDLIST(.ICDLIST,ADFN,3160101)  ;"SET THE START DATE TO 1/1/2016 TO LIMIT ICD-9 CODES
  ;"------- listing of PROBLEMs ----------------------------  
  SET JDX=0
  FOR  SET JDX=$ORDER(PROBLIST(JDX)) QUIT:JDX'>0  DO
  . NEW LINE SET LINE=$GET(PROBLIST(JDX)) QUIT:JDX=""
  . NEW IEN SET IEN=+LINE QUIT:IEN'>0
  . SET OUT(IDX)="2^"_LINE,IDX=IDX+1
  ;"------- Listing of prior ICD codes -----------------------  
  SET JDX=0
  FOR  SET JDX=$ORDER(ICDLIST(JDX)) QUIT:JDX'>0  DO
  . NEW LINE SET LINE=$GET(ICDLIST(JDX)) QUIT:JDX=""
  . SET OUT(IDX)="3^"_LINE,IDX=IDX+1
  ;"-------------------------------------------------------------  
  QUIT
  ;
DXLIST(OUT,ADFN,CMD,SDT)  ;"Get list of possible diagnoses for user to pick from, during patient encounter
  ;"RPC NAME:  TMG CPRS ENCOUNTER GET DX LIST
  ;"Input:  OUT  -- output.  Format depends on command
  ;"        ADFN
  ;"        CMD -- (can extend in future if needed)
  ;"            CMD = "LIST FOR NOTE^<IEN8925>"
  ;"        SDT -- OPTIONAL.  Starting DT.  Default = 0.  Data prior to SDT can be ignored
  ;"OUTPUT:  OUT -- if CMD='LIST FOR NOTE'
  ;"           OUT(0)="1^OK", OR "-1^<ErrorMessage>"
  ;"           OUT(#)="1^<TOPIC NAME>^<THREAD TEXT>^<LINKED PROBLEM IEN>^<LINKED ICD>^<LINKED ICD LONG NAME>^<LINKED SNOWMED NAME>^<ICD_CODE_SYS>"
  ;"               NOTES:
  ;"                 PIECE#1 = 1 means discussed this visit in note
  ;"                 Topic name is title of section, e.g. 'Back pain'.
  ;"                 Thread text is text that was ADDED to topic paragraph in TIU NOTE
  ;"                 Linked values only returned if Topic name previously linked to a PROBLEM
  ;"                 If Linked ICD's ect not present, then values will be filled with Topic name, and ICDSYS will be 'TMGTOPIC'
  ;"           OUT(#)="2^<TOPIC NAME>^<SUMMARY TEXT>^<LINKED PROBLEM IEN>^<LINKED ICD>^<LINKED ICD LONG NAME>^<LINKED SNOWMED NAME>^<ICD_CODE_SYS>"  
  ;"               NOTES:
  ;"                 PIECE#1 = 2 means NOT discussed this visit, so no THREAD TEXT returned
  ;"                 SummaryText is a fragment from the BEGINNING of the topic paragraph.
  ;"                 Linked values only returned if Topic name previously linked to a PROBLEM
  ;"                 If Linked ICD's ect not present, then values will be filled with Topic name, and ICDSYS will be 'TMGTOPIC'
  ;"           OUT(#)="3^<PROBLEM INFORMATION>  <--- format as created by LIST^ORQQPL3
  ;"               FORMAT of PROBLEM INFORMATION by pieces.  
  ;"                  1    2       3         4   5       6          7   8      9       10    11      12     13      14       15             16           17              18                19                 20                    
	;"                 ifn^status^description^ICD^onset^last modified^SC^SpExp^Condition^Loc^loc.type^prov^service^priority^has comment^date recorded^SC condition(s)^inactive flag^ICD long description^ICD coding system
	;"                   NOTE: ifn = Pointer to Problem #9000011
  ;"               NOTES:
  ;"                 PIECE#1 = 3 means node is a listing of PROBLEM
  ;"                 If problem already listed in a 1 or 2 node, then it will NOT be listed here
  ;"           OUT(#)="4^<PRIOR ICD>^<ICD LONG NAME>^<FMDT LAST USED>^<ICDCODESYS>" 
  ;"               NOTES:
  ;"                 PIECE#1 = 4 means that information is for an ICD code that was previously set for a patient visit.
  ;"                 If ICD has already been included in a piece#1=1 node, then it will NOT be listed here.  
  ;"           OUT(#)="5^HEADER^<Section Name>^<IEN22753>" 
  ;"        or OUT(#)="5^ENTRY^<ICD CODE>^<DISPLAY NAME>^<ICD LONG NAME>^<ICDCODESYS>" 
  ;"               NOTES:
  ;"                 PIECE#1 = 5 means that information is for a listing of common ICD code from a defined encounter form. 
  ;"                 PIECE#2 is either 'HEADER' or 'ENTRY'
  ;"                        Examples of HEADER nodes would be 'Cardiovascular', or 'Musculoskeletal'.  This is title of section for grouping of codes
  ;"                        ENTRY nodes will be the actual returned data. 
  ;"RESULT: none. But OUT(0) = 1^OK, or -1^ErrorMessage
  ;
  NEW TMGZZDB SET TMGZZDB=0
  IF TMGZZDB=1 DO
  . SET ADFN=$GET(^TMG("TMP","DXLIST^TMGTIIUT3","ADFN"))
  . SET CMD=$GET(^TMG("TMP","DXLIST^TMGTIIUT3","CMD"))
  . SET SDT=$GET(^TMG("TMP","DXLIST^TMGTIIUT3","SDT"))
  ELSE  DO
  . KILL ^TMG("TMP","DXLIST^TMGTIIUT3")
  . SET ^TMG("TMP","DXLIST^TMGTIIUT3","ADFN")=$GET(ADFN)
  . SET ^TMG("TMP","DXLIST^TMGTIIUT3","CMD")=$GET(CMD)
  . SET ^TMG("TMP","DXLIST^TMGTIIUT3","SDT")=$GET(SDT)
  ;    
  SET OUT(0)="1^OK"  ;"default
  SET ADFN=+$GET(ADFN)
  SET SDT=+$GET(SDT)
  NEW IDX SET IDX=1
  NEW CMD1 SET CMD1=$PIECE(CMD,"^",1)
  IF CMD1="LIST FOR NOTE" DO
  . NEW IEN8925 SET IEN8925=+$PIECE(CMD,"^",2)
  . NEW ARRAY DO SUMNOTE(IEN8925,.ARRAY)
  . KILL ARRAY(IEN8925,"FULL"),ARRAY(IEN8925,"SEQ#")
  . NEW PROBLIST DO PROBLST(.PROBLIST,ADFN,SDT)
  . NEW THREADINFO DO THREADS(.THREADINFO,ADFN,IEN8925,.ARRAY)
  . NEW TOPICS DO UNUSEDTOPICS(.TOPICS,.ARRAY,.THREADINFO)
  . NEW ICDLIST DO ICDLIST(.ICDLIST,ADFN,3160101)  ;"SET THE START DATE TO 1/1/2016 TO LIMIT ICD-9 CODES
  . ;"-------Items discussed this visit in note ----------------
  . NEW JDX SET JDX=0
  . FOR  SET JDX=$ORDER(THREADINFO(JDX)) QUIT:JDX'>0  DO
  . . NEW LINE SET LINE=$GET(THREADINFO(JDX)) QUIT:JDX=""
  . . NEW TOPIC SET TOPIC=$PIECE(LINE,"^",1)
  . . NEW THREADTXT SET THREADTXT=$PIECE(LINE,"^",2)
  . . NEW PROBIEN SET PROBIEN=$PIECE(LINE,"^",3)
  . . NEW LINKIDX SET LINKIDX=$GET(PROBLST("PROBLEM",PROBIEN))
  . . NEW PROBINFO SET PROBINFO=$GET(PROBLST(LINKIDX))
  . . NEW ICD,ICDNAME,ICDCODESYS,HCCCOLOR SET (ICD,ICDDAME,ICDCODESYS,HCCCOLOR)=""
  . . SET ICD=$PIECE(PROBINFO,"^",4) IF ICD'="" DO
  . . . SET ICDNAME=$PIECE(PROBINFO,"^",19)
  . . . SET ICDCODESYS=$PIECE(PROBINFO,"^",20)
  . . SET ICDINFO=$PIECE(LINE,"^",4) IF ICDINFO'=""  DO   ;"Only if linked ICD in 22719.5 
  . . . ;"ICDINFO format: '<ICD_IEN>;<ICD_CODE>;<ICD_TEXT>;<ICD_CODING_SYS>;<HCC COLOR>'
  . . . SET ICD=$PIECE(ICDINFO,";",2)
  . . . SET ICDNAME=$PIECE(ICDINFO,";",3)
  . . . SET ICDCODESYS=$PIECE(ICDINFO,";",4)
  . . . SET HCCCOLOR=$PIECE(ICDINFO,";",5)
  . . IF ICD="",ICDCODESYS="" SET (ICE,ICDNAME)=TOPIC,ICDCODESYS="TMGTOPIC"
  . . NEW SNOMED SET SNOMED=$PIECE(PROBINFO,"^",3)
  . . NEW RESULT SET RESULT=TOPIC_"^"_THREADTXT_"^"_PROBIEN_"^"_ICD_"^"_ICDNAME_"^"_SNOMED_"^"_ICDCODESYS_"^"_HCCCOLOR
  . . ;" DO NOT INCLUDE THE INITIAL .03 FIELD. IT WILL NOW BE THE 1 FIELD NOW UNLESS THERE AREN'T ANY SUBS -> SET OUT(IDX)="1^"_RESULT,IDX=IDX+1
  . . NEW HASSUBS SET HASSUBS=0
  . . ;"THIS SECTION WILL CHECK FOR ADDL ICD CODES    3/19/24
  . . NEW KDX SET KDX=0
  . . FOR  SET KDX=$ORDER(THREADINFO(JDX,KDX)) QUIT:KDX'>0  DO
  . . . SET ICDINFO=$GET(THREADINFO(JDX,KDX)) IF ICDINFO'=""  DO   ;"Only if linked ICD in 22719.5 
  . . . . ;"ICDINFO format: '<ICD_IEN>;<ICD_CODE>;<ICD_TEXT>;<ICD_CODING_SYS>;<HCC COLOR>'
  . . . . SET ICD=$PIECE(ICDINFO,";",2)
  . . . . SET ICDNAME=$PIECE(ICDINFO,";",3)
  . . . . SET ICDCODESYS=$PIECE(ICDINFO,";",4)
  . . . . SET HCCCOLOR=$PIECE(ICDINFO,";",5)
  . . . . NEW SUBRESULT SET SUBRESULT=TOPIC_"^"_THREADTXT_"^"_PROBIEN_"^"_ICD_"^"_ICDNAME_"^"_SNOMED_"^"_ICDCODESYS_"^"_HCCCOLOR
  . . . . SET HASSUBS=1
  . . . . SET OUT(IDX)="1^"_SUBRESULT,IDX=IDX+1
  . . IF HASSUBS=0 SET OUT(IDX)="1^"_RESULT,IDX=IDX+1
  . ;"------- NOT discussed this visit ----------------------------
  . SET JDX=0
  . FOR  SET JDX=$ORDER(TOPICS(JDX)) QUIT:JDX'>0  DO
  . . NEW LINE SET LINE=$GET(TOPICS(JDX)) QUIT:JDX=""
  . . NEW TOPIC SET TOPIC=$PIECE(LINE,"^",1)
  . . IF $DATA(THREADINFO("TOPIC",TOPIC))>0 QUIT
  . . NEW THREADTXT SET THREADTXT=$PIECE(LINE,"^",2)
  . . NEW PROBIEN SET PROBIEN=$PIECE(LINE,"^",3)
  . . NEW LINKIDX SET LINKIDX=$GET(PROBLST("PROBLEM",PROBIEN))
  . . NEW PROBINFO SET PROBINFO=$GET(PROBLST(LINKIDX))
  . . NEW ICD SET ICD=$PIECE(PROBINFO,"^",4)
  . . NEW ICDDAME SET ICDNAME=$PIECE(PROBINFO,"^",19)
  . . NEW ICDCODESYS SET ICDCODESYS=$PIECE(PROBINFO,"^",20)
  . . IF ICD="",ICDCODESYS="" SET (ICE,ICDNAME)=TOPIC,ICDCODESYS="TMGTOPIC"
  . . NEW SNOMED SET SNOMED=$PIECE(PROBINFO,"^",3)
  . . NEW RESULT SET RESULT=TOPIC_"^"_THREADTXT_"^"_PROBIEN_"^"_ICD_"^"_ICDNAME_"^"_SNOMED_"^"_ICDCODESYS
  . . ;" DO NOT INCLUDE THE INITIAL .03 FIELD. IT WILL NOW BE THE 1 FIELD NOW -> SET OUT(IDX)="2^"_LINE,IDX=IDX+1
  . . NEW HASSUBS SET HASSUBS=0
  . . ;"THIS SECTION WILL CHECK FOR ADDL ICD CODES    4/11/24
  . . NEW KDX SET KDX=0
  . . FOR  SET KDX=$ORDER(TOPICS(JDX,KDX)) QUIT:KDX'>0  DO
  . . . SET ICDINFO=$GET(TOPICS(JDX,KDX)) IF ICDINFO'=""  DO   ;"Only if linked ICD in 22719.5 
  . . . . ;"ICDINFO format: '<ICD_IEN>;<ICD_CODE>;<ICD_TEXT>;<ICD_CODING_SYS>;<HCC COLOR>'
  . . . . SET ICD=$PIECE(ICDINFO,";",2)
  . . . . SET ICDNAME=$PIECE(ICDINFO,";",3)
  . . . . SET ICDCODESYS=$PIECE(ICDINFO,";",4)
  . . . . SET HCCCOLOR=$PIECE(ICDINFO,";",5)
  . . . . NEW SUBRESULT SET SUBRESULT=TOPIC_"^"_THREADTXT_"^"_PROBIEN_"^"_ICD_"^"_ICDNAME_"^"_SNOMED_"^"_ICDCODESYS_"^"_HCCCOLOR
  . . . . SET HASSUBS=1
  . . . . SET OUT(IDX)="2^"_SUBRESULT,IDX=IDX+1
  . . IF HASSUBS=0 SET OUT(IDX)="2^"_RESULT,IDX=IDX+1
  . ;"------- listing of PROBLEMs ----------------------------  
  . SET JDX=0
  . FOR  SET JDX=$ORDER(PROBLIST(JDX)) QUIT:JDX'>0  DO
  . . NEW LINE SET LINE=$GET(PROBLIST(JDX)) QUIT:JDX=""
  . . NEW IEN SET IEN=+LINE QUIT:IEN'>0
  . . IF $DATA(THREADINFO(IEN)) QUIT
  . . IF $DATA(TOPICS(IEN)) QUIT
  . . SET OUT(IDX)="3^"_LINE,IDX=IDX+1
  . ;"------- Listing of prior ICD codes -----------------------  
  . SET JDX=0
  . FOR  SET JDX=$ORDER(ICDLIST(JDX)) QUIT:JDX'>0  DO
  . . NEW LINE SET LINE=$GET(ICDLIST(JDX)) QUIT:JDX=""
  . . SET OUT(IDX)="4^"_LINE,IDX=IDX+1
  . ;"--------Suspect medical conditions -----
  . ;"DO SUSMCENC^TMGSUSMC(.OUT,.IDX,ADFN)                  
  . NEW SDT SET SDT=$$TODAY^TMGDATE
  . NEW THISYEAR SET THISYEAR=$EXTRACT(SDT,1,3)+1700
  . NEW SUSARR DO GET1PAT^TMGSUSMC(.SUSARR,ADFN,THISYEAR)
  . IF $DATA(SUSARR) DO
  . . SET OUT(IDX)="5^HEADER^--> SUSPECT MEDICAL CONDITIONS <--",IDX=IDX+1
  . . NEW MCIDX SET MCIDX=9999
  . . FOR  SET MCIDX=$ORDER(SUSARR(MCIDX),-1) QUIT:MCIDX'>0  DO
  . . . NEW ICD,DESC,ZN,CONDITION
  . . . SET ZN=$GET(SUSARR(MCIDX))
  . . . SET ICD=$PIECE(ZN,"^",2),DESC=$PIECE(ZN,"^",4)
  . . . SET ICD=$$FIXICD^TMGSUSMC(ICD)
  . . . SET CONDITION=$PIECE(ZN,"^",3)
  . . . ;"NOTE: Later, should use ICDCODESYS(ICD) Result: -1^Error message, or  <CodeSys ShortName>^<CodeSys LongName>^<CodeSysIEN>  
  . . . SET OUT(IDX)="5^ENTRY^"_ICD_"^"_DESC_"^"_CONDITION_"^10D",IDX=IDX+1
  . ;"--------common ICD code from file -----
  . NEW ALLARRAY,USEDARR
  . SET JDX=0 FOR  SET JDX=$ORDER(^TMG(22753,"ASEQ",JDX)) QUIT:JDX'>0  DO
  . . NEW IEN SET IEN=0
  . . FOR  SET IEN=$ORDER(^TMG(22753,"ASEQ",JDX,IEN)) QUIT:IEN'>0  DO
  . . . DO ADDENTRY(.OUT,.IDX,IEN,.USEDARR,.ALLARRAY) 
  . ;"Now get subentries for which SEQ was not specified
  . NEW IEN SET IEN=0
  . FOR  SET IEN=$ORDER(^TMG(22753,IEN)) QUIT:IEN'>0  DO
  . . IF $DATA(USEDARR(IEN)) QUIT
  . . DO ADDENTRY(.OUT,.IDX,IEN,.USEDARR,.ALLARRAY) 
  . SET OUT(IDX)="5^HEADER^All Encounter Dx's",IDX=IDX+1
  . NEW DISPNAME SET DISPNAME=""
  . FOR  SET DISPNAME=$ORDER(ALLARRAY(DISPNAME)) QUIT:DISPNAME=""  DO
  . . SET OUT(IDX)="5^ENTRY^"_$GET(ALLARRAY(DISPNAME)),IDX=IDX+1  
  . ;"-------------------------------------------------------------  
  QUIT
  ;
ADDENTRY(OUT,IDX,IEN,USEDARR,ALLARRAY) ;"utility function for common ICD's above
  IF $DATA(USEDARR(IEN)) QUIT
  SET USEDARR(IEN)=""
  NEW SECTNAME SET SECTNAME=$PIECE($GET(^TMG(22753,IEN,0)),"^",1) QUIT:SECTNAME=""
  SET OUT(IDX)="5^HEADER^"_SECTNAME_"^"_IEN,IDX=IDX+1
  NEW KDX SET KDX=0
  FOR  SET KDX=$ORDER(^TMG(22753,IEN,1,"ASEQ",KDX)) QUIT:KDX'>0  DO
  . NEW SUBIEN SET SUBIEN=0
  . FOR  SET SUBIEN=$ORDER(^TMG(22753,IEN,1,"ASEQ",KDX,SUBIEN)) QUIT:SUBIEN'>0  DO
  . . SET USEDARR(IEN,"SUB",SUBIEN)=""
  . . DO ADDSUBENTRY(.OUT,.IDX,IEN,SUBIEN,.ALLARRAY)
  ;"Now get subentries for which SEQ was not specified
  NEW SUBIEN SET SUBIEN=0
  NEW TEMPARR,DISPNAME
  FOR  SET SUBIEN=$ORDER(^TMG(22753,IEN,1,SUBIEN)) QUIT:SUBIEN'>0  DO
  . IF $DATA(USEDARR(IEN,"SUB",SUBIEN)) QUIT  ;"already used
  . NEW ZN SET ZN=$GET(^TMG(22753,IEN,1,SUBIEN,0))  ;"9/12/24
  . SET DISPNAME=$PIECE(ZN,"^",3)   ;"9/12/24
  . IF DISPNAME="" SET DISPNAME="NOTSPEC-"_IEN_"-"_SUBIEN
  . SET TEMPARR(DISPNAME)=SUBIEN   ;"9/12/24
  SET DISPNAME=""   ;"9/12/24
  ;"9/12/24 Loop through TEMPARR to ensure NON-SEQ entries are alphabetical
  FOR  SET DISPNAME=$O(TEMPARR(DISPNAME)) QUIT:DISPNAME=""  DO  
  . SET SUBIEN=$G(TEMPARR(DISPNAME))
  . SET USEDARR(IEN,"SUB",SUBIEN)=""
  . DO ADDSUBENTRY(.OUT,.IDX,IEN,SUBIEN,.ALLARRAY)
  QUIT
  ;
ADDSUBENTRY(OUT,IDX,IEN,SUBIEN,ALLARRAY) ;"utility function for common ICD's above  
  NEW ZN SET ZN=$GET(^TMG(22753,IEN,1,SUBIEN,0)) QUIT:ZN=""
  NEW IEN80 SET IEN80=+$PIECE(ZN,"^",1) QUIT:IEN80'>0
  NEW ICDCODESYS SET ICDCODESYS="10D"   ;"HARD CODE Initially
  NEW ICDINFO SET ICDINFO=$$ICDDATA(ICDCODESYS,IEN80)
  ;"NEW ICDCODE SET ICDCODE=$PIECE($GET(^ICD9(IEN80,0)),"^",1)
  NEW ICDCODE SET ICDCODE=$PIECE(ICDINFO,"^",2)
  ;"NEW ICDNAME SET ICDNAME=$$VSTD^ICDEX(IEN80)
  NEW ICDNAME SET ICDNAME=$PIECE(ICDINFO,"^",4)
  NEW DISPNAME SET DISPNAME=$PIECE(ZN,"^",3)
  NEW DISPMODE SET DISPMODE=$PIECE(ZN,"^",4)
  IF DISPMODE="D" SET ICDNAME=DISPNAME,DISPNAME=""   ;"D=DISPLAY NAME ONLY
  IF DISPMODE="C" SET DISPNAME="" ;"C=CPT NAME ONLY
  IF $PIECE(ICDINFO,"^",10)'=1 DO  ;"1=ACTIVE, 0=INACTIVE
  . IF DISPNAME="" QUIT
  . SET DISPNAME="<INACTIVE>"_DISPNAME
  ;"NEW TEMP SET TEMP=$$ICDCODESYS(ICDCODE) ;"Result: -1^Error message, or  <CodeSys ShortName>^<CodeSys LongName>^<CodeSysIEN>
  NEW TEMP SET TEMP=$$ICDCSYS(ICDINFO) ;"Result: -1^Error message, or  <CodeSys ShortName>^<CodeSys LongName>^<CodeSysIEN>
  NEW COLOR SET COLOR=$$ICDCOLOR^TMGCMS1(ICDCODE)
  IF +TEMP'=-1 SET ICDCODESYS=$PIECE(TEMP,"^",1)
  SET OUT(IDX)="5^ENTRY^"_ICDCODE_"^"_DISPNAME_"^"_ICDNAME_"^"_ICDCODESYS_"^"_COLOR,IDX=IDX+1
  NEW TEMPNAME SET TEMPNAME=DISPNAME
  IF TEMPNAME'="" SET TEMPNAME=DISPNAME_"("_ICDNAME_")" 
  ELSE  SET TEMPNAME=ICDNAME
  SET ALLARRAY(TEMPNAME)=ICDCODE_"^"_DISPNAME_"^"_ICDNAME_"^"_ICDCODESYS_"^"_COLOR
  QUIT
  ;
ICDCODESYS(ICD,ICDINFO) ;"Get ICD coding system for ICD code (or IEN)
  ;"Input: ICD -- either name or IEN of ICD code
  ;"       INFO -- OPTIONAL.  OUT PARAMETER.  Full info as returned by $$ICDDATA^ICDXCODE 
  ;"Result: -1^NOT FOUND, or  <CodeSys ShortName>^<CodeSys LongName>^<CodeSysIEN>
  NEW RESULT SET RESULT="-1^NOT FOUND"  ;"default
  SET ICDINFO=$$ICDDATA("",ICD)
  ;"IF +ICDINFO=-1 DO
  ;". NEW TESTCS SET TESTCS=""
  ;". FOR  SET TESTCS=$ORDER(^ICDS("C",TESTCS)) QUIT:(TESTCS="")!(+ICDINFO'=-1)  DO
  ;". . SET ICDINFO=$$ICDDATA(TESTCS,ICD)  ;"try each code system
  IF +ICDINFO>0 SET RESULT=$$ICDCSYS(ICDINFO)
  QUIT RESULT
  ;  
ICDCSYS(ICDINFO) ;"Get ICD coding system from ICDINFO data string
  NEW CSIEN SET CSIEN=+$PIECE(ICDINFO,"^",20)  ;"this is pointer to file 80.4
  QUIT $$ICDCSYS2(CSIEN)
  ;
ICDCSYS2(CSIEN) ;"Get ICD coding system for CS IEN in  file 80.4
  NEW RESULT SET RESULT="-1^NOT FOUND"  ;"default
  IF CSIEN>0 DO
  . NEW ZN SET ZN=$GET(^ICDS(CSIEN,0))
  . SET RESULT=$PIECE(ZN,"^",2)_"^"_$PIECE(ZN,"^",1)_"^"_CSIEN
  QUIT RESULT
  ;
SUMNOTE(IEN8925,ARRAY) ;"Get back summary data from prior parsing and storage. 
  ;"Input: IEN8925 -- note IEN
  ;"       ARRAY -- PASS BY REFERENCE, AN OUT PARAMETER.  Format: 
  ;"            ARRAY(<IEN8925>,"THREAD",#)=<TOPIC NAME>^<THREAD TEXT>
  ;"            ARRAY(<IEN8925>,"HPI",#)=<TOPIC NAME>^<SUMMARY TEXT>    
  SET IEN8925=+$GET(IEN8925)
  NEW ADFN SET ADFN=0
  FOR  SET ADFN=$ORDER(^TMG(22719.2,"ATIU",IEN8925,ADFN)) QUIT:ADFN'>0  DO
  . NEW TOPICREC SET TOPICREC=0
  . FOR  SET TOPICREC=$ORDER(^TMG(22719.2,"ATIU",+IEN8925,ADFN,TOPICREC)) QUIT:TOPICREC'>0  DO
  . . NEW TOPICNAME SET TOPICNAME=$PIECE($GET(^TMG(22719.2,ADFN,1,TOPICREC,0)),"^",1)
  . . NEW DTREC SET DTREC=0
  . . FOR  SET DTREC=$ORDER(^TMG(22719.2,"ATIU",+IEN8925,ADFN,TOPICREC,DTREC)) QUIT:DTREC'>0  DO
  . . . NEW LINE SET LINE=""
  . . . NEW IDX SET IDX=0
  . . . FOR  SET IDX=$ORDER(^TMG(22719.2,ADFN,1,TOPICREC,1,DTREC,1,IDX)) QUIT:IDX'>0  DO
  . . . . SET LINE=LINE_$GET(^TMG(22719.2,ADFN,1,TOPICREC,1,DTREC,1,IDX,0))
  . . . SET ARRAY(IEN8925,"THREAD",DTREC)=TOPICNAME_"^"_LINE
  NEW TOPICREC SET TOPICREC=0
  FOR  SET TOPICREC=$ORDER(^TMG(22719,IEN8925,2,TOPICREC)) QUIT:TOPICREC'>0  DO
  . NEW ZN SET ZN=$GET(^TMG(22719,IEN8925,2,TOPICREC,0))
  . SET ARRAY(IEN8925,"HPI",TOPICREC)=ZN
  IF $DATA(ARRAY)=0 DO SUMNOTE^TMGTIUP1(IEN8925,.ARRAY)
  QUIT
  ;
THREADS(OUT,ADFN,IEN8925,ARRAY)  ;"Get THREADS info for note
  ;"INPUT:  OUT -- PASS BY REFERENCE, AN OUT PARAMETER.  Format:
  ;"           OUT(#)='<TOPIC NAME>^<THREAD TEXT>^<LINKED PROBLEM IEN>^<ICD_INFO>'
  ;"           OUT("TOPIC",<TOPIC NAME>)=#
  ;"           OUT("PROBLEM",<PROBLEM IEN>)=#
  ;"           NOTE: *For ICD_INFO, format is:
  ;"                '<ICD_IEN>;<ICD_CODE>;<ICD_TEXT>;<ICD_CODING_SYS>;<HCC COLOR>' 
  ;"                ICD_IEN is IEN in #80.  
  ;"                ICD_CODE e.g. I10 for Essential HTN
  ;"                ICD_CODING_SYS.  e.g. 10D for ICD 10
  ;"                HCC COLOR -- color to show id ICD is HCC code, or if descendant child code would be HCC ICD
	;"        ADFN -- patient IEN
  ;"        IEN8925 -- NOTE IEN
  ;"        ARRAY -- note info as could be created by SUMNOTE() or SUMNOTE^TMGTIUP1
  ;"            ARRAY(<IEN8925>,"THREAD",#)=<TOPIC NAME>^<THREAD TEXT>
  NEW IDX SET IDX=1  
  NEW JDX SET JDX=0  
  FOR  SET JDX=$ORDER(ARRAY(IEN8925,"THREAD",JDX)) QUIT:JDX'>0  DO
  . NEW LINE SET LINE=$GET(ARRAY(IEN8925,"THREAD",JDX)) QUIT:LINE=""
  . NEW TOPIC SET TOPIC=$$UP^XLFSTR($PIECE(LINE,"^",1))
  . NEW THREADTXT SET THREADTXT=$PIECE(LINE,"^",2,9999)
  . SET THREADTXT=$$REPLSTR^TMGSTUT3(THREADTXT,"^","-[/\]-")  ;"Ensure threadtxt doesn't contain any "^"s
  . NEW PTIEN SET PTIEN=+$ORDER(^TMG(22719.5,"B",ADFN,0))
  . NEW LINKIEN SET LINKIEN=+$ORDER(^TMG(22719.5,PTIEN,1,"B",$EXTRACT(TOPIC,1,30),0))
  . NEW ZN SET ZN=$GET(^TMG(22719.5,PTIEN,1,LINKIEN,0))
  . NEW PROBIEN SET PROBIEN=+$PIECE(ZN,"^",2)
  . NEW ICDIEN SET ICDIEN=+$PIECE(ZN,"^",3)
  . NEW ICDSTR SET ICDSTR="" IF ICDIEN>0 DO
  . . NEW N0 SET N0=$GET(^ICD9(ICDIEN,0))
  . . NEW N1 SET N1=$GET(^ICD9(ICDIEN,1))
  . . NEW CS SET CS=""
  . . NEW CODESYSIEN SET CODESYSIEN=$PIECE(N1,"^",1)  ;" 1;1  --> 1.1  CODING SYSTEM <-Pntr  [*P80.4']
  . . IF CODESYSIEN>0 DO  
  . . . NEW ZN SET ZN=$GET(^ICDS(CODESYSIEN,0))
  . . . NEW CSABRV SET CSABRV=$PIECE(ZN,"^",2) 
  . . . NEW CSYS SET CSYS=$PIECE(ZN,"^",1) 
  . . . SET CS=CSABRV
  . . NEW TEMP SET TEMP=$$ICDDATA(CS,ICDIEN)  ;"e.g. 504605^G47.30^^Sleep apnea, unspecified^^3^^0^^1^^^^^^^3151001^^0^30^0^
  . . NEW ICD SET ICD=$PIECE(TEMP,"^",2)  ;"e.g. G47.30
  . . NEW COLOR SET COLOR=$$ICDCOLOR^TMGCMS1(ICD)
  . . SET ICDSTR=ICDIEN_";"_ICD_";"_$PIECE(TEMP,"^",4)_";"_CS_";"_COLOR  ;"//4/11/24 //kt added COLOR
  . NEW RESULT SET RESULT=TOPIC_"^"_THREADTXT_"^"_PROBIEN_"^"_ICDSTR
  . SET OUT(IDX)=RESULT
  . SET OUT("TOPIC",TOPIC)=IDX
  . SET OUT("PROBLEM",+PROBIEN)=IDX
  . ;"Now check for extra codes  3/19/24
  . NEW KDX SET KDX=0
  . FOR  SET KDX=$ORDER(^TMG(22719.5,PTIEN,1,LINKIEN,1,KDX)) QUIT:KDX'>0  DO
  . . NEW SUBICDIEN SET SUBICDIEN=+$G(^TMG(22719.5,PTIEN,1,LINKIEN,1,KDX,0))
  . . NEW SUBICDSTR SET SUBICDSTR="" IF SUBICDIEN>0 DO
  . . . NEW N0 SET N0=$GET(^ICD9(SUBICDIEN,0))
  . . . NEW N1 SET N1=$GET(^ICD9(SUBICDIEN,1))
  . . . NEW CS SET CS=""
  . . . NEW CODESYSIEN SET CODESYSIEN=$PIECE(N1,"^",1)  ;" 1;1  --> 1.1  CODING SYSTEM <-Pntr  [*P80.4']
  . . . IF CODESYSIEN>0 DO  
  . . . . NEW ZN SET ZN=$GET(^ICDS(CODESYSIEN,0))
  . . . . NEW CSABRV SET CSABRV=$PIECE(ZN,"^",2) 
  . . . . NEW CSYS SET CSYS=$PIECE(ZN,"^",1) 
  . . . . SET CS=CSABRV
  . . . NEW TEMP SET TEMP=$$ICDDATA(CS,SUBICDIEN)  ;"e.g. 504605^G47.30^^Sleep apnea, unspecified^^3^^0^^1^^^^^^^3151001^^0^30^0^
  . . . NEW COLOR SET COLOR=$$ICDCOLOR^TMGCMS1($PIECE(TEMP,"^",2))
  . . . SET SUBICDSTR=SUBICDIEN_";"_$PIECE(TEMP,"^",2)_";"_$PIECE(TEMP,"^",4)_";"_CS_";"_COLOR
  . . . SET OUT(IDX,KDX)=SUBICDSTR
  . SET IDX=IDX+1
  QUIT
  ;
UNUSEDTOPICS(OUT,ARRAY,THREADINFO)  ;"Get list of topics that do NOT have corresponding thread text  
  ;"INPUT:  OUT -- PASS BY REFERENCE, AN OUT PARAMETER.  Format:
  ;"           OUT(#)="<TOPIC NAME>^<SUMMARY TEXT>^<LINKED PROBLEM IEN>
  ;"           OUT("TOPIC",<TOPIC NAME>)=#
  ;"           OUT("PROBLEM",<PROBLEM IEN>)=#
  ;"           OUT("ICD",<ICD COD>)=#
  ;"        ARRAY -- note info as could be created by SUMNOTE() or SUMNOTE^TMGTIUP1
  ;"            ARRAY(<IEN8925>,"HPI",#)=<TOPIC NAME>^<SUMMARY TEXT>
  ;"        THREADINFO -- ARRAY as created by THREADS()
  NEW IDX SET IDX=1
  NEW TOPIC SET TOPIC=""
  NEW JDX SET JDX=0
  FOR  SET JDX=$ORDER(ARRAY(IEN8925,"HPI",JDX)) QUIT:(JDX'>0)  DO
  . NEW LINE SET LINE=$GET(ARRAY(IEN8925,"HPI",JDX)) QUIT:LINE=""
  . NEW TOPIC SET TOPIC=$$UP^XLFSTR($PIECE(LINE,"^",1)) QUIT:TOPIC=""
  . IF $DATA(THREADINFO("TOPIC",TOPIC)) QUIT
  . NEW SUMMTXT SET SUMMTXT=$PIECE(LINE,"^",2,9999)
  . SET SUMMTXT=$$REPLSTR^TMGSTUT3(SUMMTXT,"^","-[/\]-")  ;"Ensure text doesn't contain any "^"s
  . NEW PTIEN SET PTIEN=+$ORDER(^TMG(22719.5,"B",ADFN,0))
  . NEW LINKIEN SET LINKIEN=+$ORDER(^TMG(22719.5,PTIEN,1,"B",TOPIC,0))
  . NEW ZN SET ZN=$GET(^TMG(22719.5,PTIEN,1,LINKIEN,0))
  . NEW PROBIEN SET PROBIEN=+$PIECE(ZN,"^",2)
  . NEW RESULT SET RESULT=TOPIC_"^"_SUMMTXT_"^"_PROBIEN
  . SET OUT(IDX)=RESULT
  . ;"Now check for extra codes  3/19/24
  . NEW KDX SET KDX=0
  . FOR  SET KDX=$ORDER(^TMG(22719.5,PTIEN,1,LINKIEN,1,KDX)) QUIT:KDX'>0  DO
  . . NEW SUBICDIEN SET SUBICDIEN=+$G(^TMG(22719.5,PTIEN,1,LINKIEN,1,KDX,0))
  . . NEW SUBICDSTR SET SUBICDSTR="" IF SUBICDIEN>0 DO
  . . . NEW N0 SET N0=$GET(^ICD9(SUBICDIEN,0))
  . . . NEW N1 SET N1=$GET(^ICD9(SUBICDIEN,1))
  . . . NEW CS SET CS=""
  . . . NEW CODESYSIEN SET CODESYSIEN=$PIECE(N1,"^",1)  ;" 1;1  --> 1.1  CODING SYSTEM <-Pntr  [*P80.4']
  . . . IF CODESYSIEN>0 DO  
  . . . . NEW ZN SET ZN=$GET(^ICDS(CODESYSIEN,0))
  . . . . NEW CSABRV SET CSABRV=$PIECE(ZN,"^",2) 
  . . . . NEW CSYS SET CSYS=$PIECE(ZN,"^",1) 
  . . . . SET CS=CSABRV
  . . . NEW TEMP SET TEMP=$$ICDDATA(CS,SUBICDIEN)  ;"e.g. 504605^G47.30^^Sleep apnea, unspecified^^3^^0^^1^^^^^^^3151001^^0^30^0^
  . . . NEW COLOR SET COLOR=$$ICDCOLOR^TMGCMS1($PIECE(TEMP,"^",2))
  . . . SET SUBICDSTR=SUBICDIEN_";"_$PIECE(TEMP,"^",2)_";"_$PIECE(TEMP,"^",4)_";"_CS_";"_COLOR
  . . . SET OUT(IDX,KDX)=SUBICDSTR
  . SET IDX=IDX+1
  QUIT
  ;
PROBLST(OUT,ADFN,SDT)  ;"Get listing of patient's defined problem list
  ;"Input: OUT -- PASS BY REFERENCE, AN OUT PARAMETER.  Format:      
  ;"           OUT(0) = COUNT
  ;"           OUT(#)="<PROBLEM INFORMATION>  <--- format as created by LIST^ORQQPL3
  ;"               FORMAT of PROBLEM INFORMATION by pieces.  
  ;"                  1    2       3         4   5       6          7   8      9       10    11      12     13      14       15             16           17              18                19                 20                    
	;"                 ifn^status^description^ICD^onset^last modified^SC^SpExp^Condition^Loc^loc.type^prov^service^priority^has comment^date recorded^SC condition(s)^inactive flag^ICD long description^ICD coding system
	;"                   NOTE: ifn = Pointer to Problem #9000011
	;"       ADFN -- patient IEN
	;"       SDT -- Starting date for returned problems.
	;"Result: none
  SET SDT=+$GET(SDT)
  NEW STATUS SET STATUS="A"  ;"A=ACTIVE
  DO LIST^ORQQPL3(.OUT,ADFN,STATUS,SDT)
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(OUT(IDX)) QUIT:IDX'>0  DO
  . NEW LINE SET LINE=$GET(OUT(IDX)) QUIT:LINE=""
  . NEW IEN SET IEN=+LINE QUIT:IEN'>0
  . NEW COLOR SET COLOR=$$ICDCOLOR^TMGCMS1($PIECE(LINE,"^",4))
  . SET OUT(IDX)=$G(OUT(IDX))_"^"_COLOR
  . SET OUT("PROBLEM",IEN)=IDX
  QUIT
  ;
ICDLIST(OUT,ADFN,SDT) ;"Get listing of ICD's used for patient in past
  ;"Input: OUT -- PASS BY REFERENCE, AN OUT PARAMETER.  Format:
  ;"        OUT(#)=<ICD CODE>^<ICD NAME>^<LAST USED FMDT>^<ICDCODESYS>
	;"       ADFN -- patient IEN
	;"       SDT -- Starting date for returned problems.
	;"Result: none
  NEW TEMP,XREF
  NEW EDT SET EDT=9999999
  NEW OPTION SET OPTION("ICDSYS")=1,OPTION("ACTIVECHK")=1
  DO GETICD^TMGRPT4(.TEMP,ADFN,.SDT,.EDT,.OPTION)  ;"Gather ICD's for patient into array
  NEW IDX SET IDX=1
  NEW SORTARR  ;"<-- Used to remove duplicate ICD entries and alpha sort by NAME
  NEW ADT SET ADT=0  
  FOR  SET ADT=$ORDER(TEMP("ICD",ADT)) QUIT:ADT'>0  DO
  . NEW LINE SET LINE=""
  . FOR  SET LINE=$ORDER(TEMP("ICD",ADT,LINE)) QUIT:LINE=""  DO
  . . NEW ICD SET ICD=$PIECE(LINE,"^",1)
  . . NEW ICDNAME SET ICDNAME=$PIECE(LINE,"^",2)
  . . NEW CODESYS SET CODESYS=$PIECE(LINE,"^",3)
  . . SET SORTARR(ICDNAME,ADT)=ICD_"^"_CODESYS
  NEW ICDNAME SET ICDNAME=""
  FOR  SET ICDNAME=$ORDER(SORTARR(ICDNAME)) QUIT:ICDNAME=""  DO
  . NEW LDT SET LDT=$ORDER(SORTARR(ICDNAME,""),-1)  ;"get just the last entry
  . NEW ICD SET ICD=$GET(SORTARR(ICDNAME,LDT))
  . NEW CODESYS SET CODESYS=$PIECE(ICD,"^",2)
  . SET ICD=$PIECE(ICD,"^",1)
  . NEW COLOR SET COLOR=$$ICDCOLOR^TMGCMS1(ICD)
  . SET OUT(IDX)=ICD_"^"_ICDNAME_"^"_LDT_"^"_CODESYS_"^"_COLOR
  . SET IDX=IDX+1
  ;" FOR  SET ADT=$ORDER(TEMP("ICD",ADT)) QUIT:ADT'>0  DO
  ;" . NEW LINE SET LINE=""
  ;" . FOR  SET LINE=$ORDER(TEMP("ICD",ADT,LINE)) QUIT:LINE=""  DO
  ;" . . NEW ICD SET ICD=$PIECE(LINE,"^",1)
  ;" . . NEW ICDNAME SET ICDNAME=$PIECE(LINE,"^",2)
  ;" . . SET XREF(ICD,ADT)=IDX
  ;" . . SET OUT(IDX)=ICD_"^"_ICDNAME,IDX=IDX+1
  ;" ;"Now determine last used date.
  ;" NEW ICD SET ICD=""
  ;" FOR  SET ICD=$ORDER(XREF(ICD)) QUIT:ICD=""  DO
  ;" . NEW LASTDT SET LASTDT=+$ORDER(XREF(ICD,""),-1) QUIT:LASTDT=0 
  ;" . SET IDX=+$GET(XREF(ICD,LASTDT))
  ;" . SET $PIECE(OUT(IDX),"^",3)=LASTDT
  QUIT
  ;
TESTDX ;
  NEW IEN8925 SET IEN8925=743345
  NEW ADFN SET ADFN=75985
  NEW CMD SET CMD="LIST FOR NOTE^"_IEN8925
  NEW SDT SET SDT=""
  NEW OUT DO DXLIST(.OUT,ADFN,CMD,SDT)
  ZWR OUT
  QUIT
  ;
  ;"==========================================================
  ;
PROCLIST(OUT,ADFN,CMD,SDT)  ;"Get list of possible procedures for user to pick from, during patient encounter
  ;"RPC NAME:  TMG CPRS ENCOUNTER GET CPT LST
  ;"Input:  OUT  -- output.  Format depends on command
  ;"        ADFN
  ;"        CMD -- (can extend in future if needed)
  ;"            CMD = "LIST FOR USER^<USER_IEN/DUZ>"   
  ;"        SDT -- OPTIONAL.  Starting DT.  Default = 0.  Data prior to SDT can be ignored
  ;"OUTPUT:  OUT -- if CMD='LIST FOR USER'
  ;"           OUT(0)="1^OK", OR "-1^<ErrorMessage>"
  ;"           OUT(#)="1^<PRIOR CPT>^<CPT LONG NAME>^<FMDT LAST USED>" 
  ;"               NOTES:
  ;"                 PIECE#1 = 1 means that information is for an ICD code that was previously set for a patient visit.
  ;"           OUT(#)="2^HEADER^<Section Name>^<IEN22754>" 
  ;"        or OUT(#)="2^ENTRY^<CPT CODE>^<DISPLAY NAME>^<CPT LONG NAME>" 
  ;"               NOTES:
  ;"                 PIECE#1 = 2 means that information is for a listing of common CPT code from a defined encounter form. 
  ;"                 PIECE#2 is either 'HEADER' or 'ENTRY'
  ;"                        Examples of HEADER nodes would be 'Injections', or 'Skin Bx'.  This is title of section for grouping of codes
  ;"                        ENTRY nodes will be the actual returned data. 
  ;"RESULT: none. But OUT(0) = 1^OK, or -1^ErrorMessage
  ;
  NEW TMGZZDB SET TMGZZDB=0
  IF TMGZZDB=1 DO
  . SET ADFN=$GET(^TMG("TMP","PROCLIST^TMGTIIUT3","ADFN"))
  . SET CMD=$GET(^TMG("TMP","PROCLIST^TMGTIIUT3","CMD"))
  . SET SDT=$GET(^TMG("TMP","PROCLIST^TMGTIIUT3","SDT"))
  ELSE  DO
  . KILL ^TMG("TMP","PROCLIST^TMGTIIUT3")
  . SET ^TMG("TMP","PROCLIST^TMGTIIUT3","ADFN")=$GET(ADFN)
  . SET ^TMG("TMP","PROCLIST^TMGTIIUT3","CMD")=$GET(CMD)
  . SET ^TMG("TMP","PROCLIST^TMGTIIUT3","SDT")=$GET(SDT)
  ;    
  SET OUT(0)="1^OK"  ;"default
  SET ADFN=+$GET(ADFN)
  SET SDT=+$GET(SDT)      ;"may use later
  NEW IDX SET IDX=1
  NEW CMD1 SET CMD1=$PIECE(CMD,"^",1)
  NEW ADUZ SET ADUZ=$PIECE(CMD,"^",2)  ;"may use later
  IF CMD1="LIST FOR USER" DO
  . NEW CPTLIST DO CPTLIST(.CPTLIST,ADFN,SDT)
  . ;"------- Listing of prior CPT codes -----------------------  
  . SET JDX=0
  . FOR  SET JDX=$ORDER(CPTLIST(JDX)) QUIT:JDX'>0  DO
  . . NEW LINE SET LINE=$GET(CPTLIST(JDX)) QUIT:JDX=""
  . . SET OUT(IDX)="1^"_LINE,IDX=IDX+1  
  . ;"--------common CPT code from file -----
  . NEW ALLARRAY,USEDARR
  . SET JDX=0 FOR  SET JDX=$ORDER(^TMG(22754,"ASEQ",JDX)) QUIT:JDX'>0  DO
  . . NEW IEN SET IEN=0
  . . FOR  SET IEN=$ORDER(^TMG(22754,"ASEQ",JDX,IEN)) QUIT:IEN'>0  DO
  . . . DO ADDENTRYCPT(.OUT,2,22754,.IDX,IEN,.USEDARR,.ALLARRAY) 
  . ;"Now get subentries for which SEQ was not specified
  . NEW IEN SET IEN=0
  . FOR  SET IEN=$ORDER(^TMG(22754,IEN)) QUIT:IEN'>0  DO
  . . IF $DATA(USEDARR(IEN)) QUIT
  . . DO ADDENTRYCPT(.OUT,2,22754,.IDX,IEN,.USEDARR,.ALLARRAY) 
  . SET OUT(IDX)="2^HEADER^All Encounter CPT's",IDX=IDX+1
  . NEW DISPNAME SET DISPNAME=""
  . FOR  SET DISPNAME=$ORDER(ALLARRAY(DISPNAME)) QUIT:DISPNAME=""  DO
  . . SET OUT(IDX)="2^ENTRY^"_$GET(ALLARRAY(DISPNAME)),IDX=IDX+1  
  . ;"-------------------------------------------------------------  
  QUIT
  ;
ADDENTRYCPT(OUT,NODENUM,FNUM,IDX,IEN,USEDARR,ALLARRAY) ;"utility function for common CPT's above
  IF $DATA(USEDARR(IEN)) QUIT
  SET USEDARR(IEN)=""
  NEW SECTNAME SET SECTNAME=$PIECE($GET(^TMG(FNUM,IEN,0)),"^",1) QUIT:SECTNAME=""
  SET OUT(IDX)=NODENUM_"^HEADER^"_SECTNAME_"^"_IEN,IDX=IDX+1
  NEW KDX SET KDX=0
  FOR  SET KDX=$ORDER(^TMG(FNUM,IEN,1,"ASEQ",KDX)) QUIT:KDX'>0  DO
  . NEW SUBIEN SET SUBIEN=0
  . FOR  SET SUBIEN=$ORDER(^TMG(FNUM,IEN,1,"ASEQ",KDX,SUBIEN)) QUIT:SUBIEN'>0  DO
  . . SET USEDARR(IEN,"SUB",SUBIEN)=""
  . . DO ADDSUBENTRYCPT(.OUT,NODENUM,FNUM,.IDX,IEN,SUBIEN,.ALLARRAY)
  ;"Now get subentries for which SEQ was not specified
  NEW SUBIEN SET SUBIEN=0
  FOR  SET SUBIEN=$ORDER(^TMG(FNUM,IEN,1,SUBIEN)) QUIT:SUBIEN'>0  DO
  . IF $DATA(USEDARR(IEN,"SUB",SUBIEN)) QUIT  ;"already used
  . SET USEDARR(IEN,"SUB",SUBIEN)=""
  . DO ADDSUBENTRYCPT(.OUT,NODENUM,FNUM,.IDX,IEN,SUBIEN,.ALLARRAY)
  QUIT
  ;
ADDSUBENTRYCPT(OUT,NODENUM,FNUM,IDX,IEN,SUBIEN,ALLARRAY) ;"utility function for common CPT's above  
  NEW ZN SET ZN=$GET(^TMG(FNUM,IEN,1,SUBIEN,0)) QUIT:ZN=""
  NEW IEN81 SET IEN81=+$PIECE(ZN,"^",1) QUIT:IEN81'>0
  NEW Z2 SET Z2=$GET(^ICPT(IEN81,0))
  NEW CPTCODE SET CPTCODE=$PIECE(Z2,"^",1)
  NEW CPTNAME SET CPTNAME=$PIECE(Z2,"^",2)
  NEW DISPNAME SET DISPNAME=$PIECE(ZN,"^",3)
  NEW DISPMODE SET DISPMODE=$PIECE(ZN,"^",4)
  IF DISPMODE="D" SET CPTNAME=""   ;"D=DISPLAY NAME ONLY
  IF DISPMODE="C" SET DISPNAME="" ;"C=CPT NAME ONLY
  SET OUT(IDX)=NODENUM_"^ENTRY^"_CPTCODE_"^"_DISPNAME_"^"_CPTNAME,IDX=IDX+1
  NEW TEMPNAME SET TEMPNAME=DISPNAME
  IF TEMPNAME'="" SET TEMPNAME=DISPNAME_"("_CPTNAME_")" 
  ELSE  SET TEMPNAME=CPTNAME
  SET ALLARRAY(TEMPNAME)=CPTCODE_"^"_DISPNAME_"^"_CPTNAME
  QUIT
  ;   
  ;"==========================================================
  ;
CPTLIST(OUT,ADFN,SDT) ;"Get listing of CPT's used for patient in past. Utility function for PROCLIST above. 
  ;"Input: OUT -- PASS BY REFERENCE, AN OUT PARAMETER.  Format:
  ;"        OUT(#)=<CPT CODE>^<CPT NAME>^<LAST USED FMDT>
	;"       ADFN -- patient IEN
	;"       SDT -- Starting date for returned problems.
	;"Result: none
  NEW TEMP,XREF
  NEW EDT SET EDT=9999999
  DO GETCPT^TMGRPT4(.TEMP,ADFN,.SDT,.EDT)  ;"Gather CPT's for patient into array
  NEW IDX SET IDX=1
  NEW SORTARR  ;"<-- Used to remove duplicate CPT entries and alpha sort by NAME
  NEW ADT SET ADT=0  
  FOR  SET ADT=$ORDER(TEMP("CPT",ADT)) QUIT:ADT'>0  DO
  . NEW LINE SET LINE=""
  . FOR  SET LINE=$ORDER(TEMP("CPT",ADT,LINE)) QUIT:LINE=""  DO
  . . NEW CPT SET CPT=$PIECE(LINE,"^",1)
  . . NEW CPTNAME SET CPTNAME=$PIECE(LINE,"^",2)
  . . SET SORTARR(CPTNAME,ADT)=CPT
  NEW CPTNAME SET CPTNAME=""
  FOR  SET CPTNAME=$ORDER(SORTARR(CPTNAME)) QUIT:CPTNAME=""  DO
  . NEW LDT SET LDT=$ORDER(SORTARR(CPTNAME,""),-1)  ;"get just the last entry
  . NEW CPT SET CPT=$GET(SORTARR(CPTNAME,LDT))
  . SET CPT=$PIECE(CPT,"^",1)
  . SET OUT(IDX)=CPT_"^"_CPTNAME_"^"_LDT
  . SET IDX=IDX+1
  QUIT
  ;
TESTCPT ;
  NEW ADFN SET ADFN=75985
  NEW CMD SET CMD="LIST FOR USER^168"
  NEW SDT SET SDT=""
  NEW OUT DO PROCLIST(.OUT,ADFN,CMD,SDT)
  ZWR OUT
  QUIT
  ;
  ;"==========================================================
  ;
VISITLIST(OUT,ADFN,CMD,SDT)  ;"Get list of possible procedures for user to pick from, during patient encounter
  ;"RPC NAME:  TMG CPRS ENCOUNTER GET VST LST
  ;"Input:  OUT  -- output.  Format depends on command
  ;"        ADFN
  ;"        CMD -- (can extend in future if needed)
  ;"            CMD = "LIST FOR USER,LOC^<USER_IEN/DUZ>^<LOCIEN>"   
  ;"        SDT -- OPTIONAL.  Starting DT.  Default = 0.  Data prior to SDT can be ignored
  ;"OUTPUT:  OUT -- if CMD='LIST FOR USER'
  ;"           OUT(0)="1^OK", OR "-1^<ErrorMessage>"
  ;"           OUT(#)="1^HEADER^<Section Name>^<IEN22755>" 
  ;"        or OUT(#)="1^ENTRY^<CPT CODE>^<DISPLAY NAME>^<CPT LONG NAME>" 
  ;"               NOTES:
  ;"                 PIECE#1 =12 means that information is for a listing of common VISIT CPT code from a defined encounter form. 
  ;"                 PIECE#2 is either 'HEADER' or 'ENTRY'
  ;"                        Examples of HEADER nodes would be 'Injections', or 'Skin Bx'.  This is title of section for grouping of codes
  ;"                        ENTRY nodes will be the actual returned data. 
  ;"RESULT: none. But OUT(0) = 1^OK, or -1^ErrorMessage
  ;
  NEW TMGZZDB SET TMGZZDB=0
  IF TMGZZDB=1 DO
  . SET ADFN=$GET(^TMG("TMP","VISITLIST^TMGTIIUT3","ADFN"))
  . SET CMD=$GET(^TMG("TMP","VISITLIST^TMGTIIUT3","CMD"))
  . SET SDT=$GET(^TMG("TMP","VISITLIST^TMGTIIUT3","SDT"))
  ELSE  DO
  . KILL ^TMG("TMP","VISITLIST^TMGTIIUT3")
  . SET ^TMG("TMP","VISITLIST^TMGTIIUT3","ADFN")=$GET(ADFN)
  . SET ^TMG("TMP","VISITLIST^TMGTIIUT3","CMD")=$GET(CMD)
  . SET ^TMG("TMP","VISITLIST^TMGTIIUT3","SDT")=$GET(SDT)
  ;    
  SET OUT(0)="1^OK"  ;"default
  SET ADFN=+$GET(ADFN)      ;"may use later
  SET SDT=+$GET(SDT)        ;"may use later
  NEW IDX SET IDX=1
  NEW CMD1 SET CMD1=$PIECE(CMD,"^",1)
  NEW ADUZ SET ADUZ=$PIECE(CMD,"^",2)  ;"may use later
  NEW LOC SET LOC=$PIECE(CMD,"^",1)    ;"may use later
  IF CMD1="LIST FOR USER,LOC" DO
  . ;"--------common VISIT CPT code from a file  -----
  . NEW ALLARRAY,USEDARR
  . SET JDX=0 FOR  SET JDX=$ORDER(^TMG(22755,"ASEQ",JDX)) QUIT:JDX'>0  DO
  . . NEW IEN SET IEN=0
  . . FOR  SET IEN=$ORDER(^TMG(22755,"ASEQ",JDX,IEN)) QUIT:IEN'>0  DO
  . . . DO ADDENTRYCPT(.OUT,1,22755,.IDX,IEN,.USEDARR,.ALLARRAY) 
  . ;"Now get subentries for which SEQ was not specified
  . NEW IEN SET IEN=0
  . FOR  SET IEN=$ORDER(^TMG(22755,IEN)) QUIT:IEN'>0  DO
  . . IF $DATA(USEDARR(IEN)) QUIT
  . . DO ADDENTRYCPT(.OUT,1,22755,.IDX,IEN,.USEDARR,.ALLARRAY) 
  . SET OUT(IDX)="1^HEADER^All Visit Codes",IDX=IDX+1
  . NEW DISPNAME SET DISPNAME=""
  . FOR  SET DISPNAME=$ORDER(ALLARRAY(DISPNAME)) QUIT:DISPNAME=""  DO
  . . SET OUT(IDX)="1^ENTRY^"_$GET(ALLARRAY(DISPNAME)),IDX=IDX+1  
  . ;"-------------------------------------------------------------  
  QUIT
  ;
TESTVST ;
  NEW ADFN SET ADFN=75985
  NEW CMD SET CMD="LIST FOR USER,LOC^168^0"
  NEW SDT SET SDT=""
  NEW OUT DO VISITLIST(.OUT,ADFN,CMD,SDT)
  ZWR OUT
  QUIT
  ;
SUBRECID(OREF,IEN,FILENUM) ;"Callback function from LIST^DIC, optionally used
  ;"This code is designed to be called via IDENTIFIER parameter in LIST^DIC, via
  ;"     "DO SUBRECID^TMGTIUT3(DIC,Y,+$GET(DIFILE))"
  ;"     See here: https://hardhats.org/fileman/pm/db_dic_l.htm
  ;"     NOTE: DIFILE is not listed as variable that can be depended on.  But 
  ;"           runtime inspection shows that this has been setup by Fileman. 
  ;"     Code should not do any output EXCEPT via EN^DDIOL, e.g. D EN^DDIOL("KILROY WAS HERE!") 
  ;"INPUT: OREF -- this is the open format of file being transversed, e.g. '^TMG(22753,5,1,'
  ;"       Y -- this is IEN of the item being considered, e.g. '6'
  ;"       FILENUM -- The Fileman number of the file being transversed.  
  IF FILENUM'=22753.01 QUIT  ;"can make more general later.  
  NEW CREF SET CREF=OREF_Y_")"
  NEW ZN SET ZN=$GET(@CREF@(0))
  NEW IEN80 SET IEN80=+ZN QUIT:(IEN80'>0)
  NEW OUT SET OUT=$$CODEC^ICDEX(80,IEN80)
  NEW LONGTEXT SET LONGTEXT=$$VLT^ICDEX(80,IEN80)
  IF LONGTEXT'="" SET OUT=OUT_" -- "_LONGTEXT
  DO EN^DDIOL(OUT)
  QUIT
  ;  
ENCEDIT(TMGRESULT,INPUT)  ;"RPC: TMG CPRS ENCOUNTER EDIT
  ;"Purpose: This RPC is designed to allow editing of the encounter from CPRS
  ;"Input: TMGRESULT - OUT parameter 
  ;"      INPUT - Array of data sent from CPRS
  ;"      INPUT(#)=CMD^CATEGORY^IENINFO^DATA(FLD=VALUE)^DATA^DATA...
  ;"           CMD: DEL, ADD, or EDIT
  ;"           Category: ENTRY or SECTION
  ;"           IEN: IEN22753 value, OR '.01=<value>'  
  ;"           DATA: Will be FLD=VALUE
  ;"           Examples of expected input  
  ;"             INPUT(#)="ADD^SECTION^0^.01=Misc^.02=12"                        ; <-- ADD new section named Misc with sequence of 12.  NOTE: don't include any child entries here.  
  ;"             INPUT(#)="EDIT^SECTION^.01=Misc^.01=Misc-Stuff"                 ; <-- rename .01 field  
  ;"             INPUT(#)="ADD^ENTRY^.01=Misc-Stuff^.01=`572555^.03=Pes Planus"  ; <-- add 'Pes Planus', for ICD (IEN80) 667788, into section `4567
  ;"             INPUT(#)="EDIT^ENTRY^.01=Misc-Stuff^.01=`572555^.03=Planus,Pes" ; <-- edit ICD (IEN80) 667788, in section `4567, to have new .03 value
  ;"             INPUT(#)="DEL^ENTRY^.01=Misc-Stuff^.01=`572555"                 ; <-- delete entry for ICD (IEN80) 667788, in section `4567
  ;"             INPUT(#)="DEL^SECTION^.01=Misc-Stuff"                           ; <-- DELETE entry NOTE: this will kill all contained child entries!
  ;"RESULT: (ARRAY TYPE)
  ;"        RESULT(#)=<success or failure of line of input.>, i.e. 1^SUCCESS, or -1^ErrorMessage
  ;
  NEW TMGDEBUG SET TMGDEBUG=0 ;"Set to 1 at runtime to use stored input
  IF TMGDEBUG=0 DO
  . KILL ^TMG("TMP","ENCEDIT")
  . MERGE ^TMG("TMP","ENCEDIT","INPUT")=INPUT
  ELSE  DO
  . KILL INPUT 
  . MERGE INPUT=^TMG("TMP","ENCEDIT","INPUT")
  ;
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(INPUT(IDX)) QUIT:IDX'>0  DO
  . NEW ERRORMSG SET ERRORMSG=""
  . NEW LINE SET LINE=INPUT(IDX)
  . NEW ARR DO SPLIT2AR^TMGSTUT2(LINE,"^",.ARR)
  . NEW TMGCMD SET TMGCMD=$$UP^XLFSTR($GET(ARR(1))) KILL ARR(1)
  . IF (TMGCMD'="ADD")&(TMGCMD'="DEL")&(TMGCMD'="EDIT") DO  QUIT
  . . DO ADDERROR(.ERRORMSG,"INVALID COMMAND ON ARRAY #"_IDX_". Got ["_TMGCMD_"]")
  . . SET TMGRESULT(IDX)="-1^"_ERRORMSG
  . NEW TMGCAT SET TMGCAT=$$UP^XLFSTR($GET(ARR(2))) KILL ARR(2)
  . IF (TMGCAT'="ENTRY")&(TMGCAT'="SECTION") DO  QUIT
  . . DO ADDERROR(.ERRORMSG,"INVALID CATEGORY ON ARRAY #"_IDX_". Got ["_TMGCAT_"]")  
  . . SET TMGRESULT(IDX)="-1^"_ERRORMSG
  . NEW IENINFO SET IENINFO=$GET(ARR(3)) KILL ARR(3)
  . NEW ARR2 DO FMTARR(.ARR,.ARR2)
  . NEW DIC,X,Y 
  . NEW TMGIEN SET TMGIEN=0
  . IF +IENINFO=IENINFO SET TMGIEN=+IENINFO
  . ELSE  IF IENINFO[".01=" DO
  . . SET TMGIEN=+$$SECTIONIEN(.IENINFO) ;Get IEN of passed section
  . IF TMGIEN'>0,TMGCMD'="ADD",TMGCAT'="SECTION" DO  QUIT
  . . DO ADDERROR(.ERRORMSG,"Unable to find SECTION.  Got ["_IENINFO_"]")
  . . SET TMGRESULT(IDX)="-1^"_ERRORMSG
  . SET TMGIEN(22753)=+TMGIEN    
  . IF TMGCAT="ENTRY" DO
  . . SET TMGIEN(22753.01)=+$$SECTSUBIEN(+TMGIEN,$GET(ARR2(.01))) 
  . . IF TMGIEN(22753.01)'>0,TMGCMD="ADD" SET TMGIEN(22753.01)="+1," QUIT
  . . ;"IF TMGCMD="ADD" SET TMGIEN(22753.01)="+1," QUIT
  . . ;"SET DIC=$$OREF^DILF($NAME(^TMG(22753,+TMGIEN,1))),DIC(0)="",X=$GET(ARR2(.01))
  . . ;"IF $EXTRACT(X,1)="`" SET X=$EXTRACT(X,2,$LENGTH(X)),DIC(0)="U"
  . . ;"DO ^DIC
  . . ;"IF +Y'>0 DO  QUIT
  . . ;". DO ADDERROR(.ERRORMSG,"Unable to find subfile record.  Got [.01="_$GET(ARR2(.01))_"]")
  . . ;"SET TMGIEN(22753.01)=+Y
  . IF TMGCMD="ADD" DO
  . . DO ADDITEM(TMGCAT,.TMGIEN,.ARR2,.ERRORMSG)
  . ELSE  IF TMGCMD="DEL" DO
  . . DO DELITEM(TMGCAT,.TMGIEN,.ARR2,.ERRORMSG)
  . ELSE  IF TMGCMD="EDIT" DO
  . . DO EDITITEM(TMGCAT,.TMGIEN,.ARR2,.ERRORMSG)
  . IF ERRORMSG'="" SET TMGRESULT(IDX)="-1^"_ERRORMSG
  . ELSE  SET TMGRESULT(IDX)="1^SUCCESS"
  QUIT
  ;
FMTARR(IN,OUT)  ;"Reformat array
  NEW IDX SET IDX=""
  FOR  SET IDX=$ORDER(IN(IDX)) QUIT:IDX'>0  DO
  . NEW LINE SET LINE=$GET(IN(IDX))
  . NEW FLD SET FLD=+LINE
  . IF FLD'>0 QUIT
  . NEW VALUE SET VALUE=$PIECE(LINE,"=",2)
  . SET OUT(FLD)=VALUE
  QUIT
  ;
ADDITEM(TMGCAT,TMGIEN,ARR,ERRORMSG)  ;" add an item to the encounter form
  ;"Input:  TMGCAT -- should be ENTRY (for a particular entry) or SECTION (for group entry)
  ;"        IENINFO --PASS BY REFERENCE
  ;"                    TMGIEN(22753)=<IEN IN 22753>  -- if found
  ;"                    TMGIEN(22753.01)=<IEN IN 22753.01>  <--- NOT USED
  ;"        ARR -- PASS BY REFERENCE. Should be in format of ARR(<FLDNUMBER>)=<VALUE>  
  ;"        ERRORMSG -- PASS BY REFERENCE, an OUT parameter.  Text added for error message.  DONT add -1^ here.
  ;"Examples of expected input
  ;"  INPUT(#)="ADD^SECTION^0^.01=Misc^.02=12"              <-- ADD new section named Misc with sequence of 12.  NOTE: don't include any child entries here.  
  ;"  INPUT(#)="ADD^ENTRY^4567^.01='667788^.03=Pes Planus"   <-- add 'Pes Planus', for ICD (IEN80) 667788, into section `4567
  NEW TMGFDA,TMGMSG
  NEW FLD01 SET FLD01=$GET(ARR(.01))
  IF FLD01="" DO  QUIT
  . DO ADDERROR(.ERRORMSG,"Value for .01 not provided.  Unable to add unnamed record.")
  IF TMGCAT="ENTRY" DO
  . SET TMGIEN=+$GET(TMGIEN(22753)) IF TMGIEN'>0 DO  QUIT
  . . DO ADDERROR(.ERRORMSG,"Unable to find section to add to.  Got ["_TMGINFO_"]")
  . NEW SUBIEN SET SUBIEN=$GET(TMGIEN(22753.01))
  . IF SUBIEN'="+1," DO  QUIT
  . . DO ADDERROR(.ERRORMSG,"Section ["_FLD01_"] already exists: "_SUBIEN_","_TMGIEN_",")
  . ;"EDDIE TEST SET ARR(.03)="ADDED"
  . MERGE TMGFDA(22753.01,"+1,"_TMGIEN_",")=ARR
  ELSE  IF TMGCAT="SECTION" DO
  . NEW TEMPIEN SET TEMPIEN=$$SECTIONIEN(".01="_FLD01)
  . IF TEMPIEN>0 DO  QUIT
  . . DO ADDERROR(.ERRORMSG,"Section ["_FLD01_"] already exists")
  . MERGE TMGFDA(22753,"+1,")=ARR
  IF $DATA(TMGFDA) DO
  . KILL TMGIEN DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
  . IF $DATA(TMGMSG("DIERR")) DO
  . . NEW ERR SET ERR=$$GETERRST^TMGDEBU2(.TMGMSG) DO ADDERROR(.ERRORMSG,ERR)
AIDN ;
  QUIT
  ;
DELITEM(TMGCAT,TMGIEN,ARR,ERRORMSG)  ;"delete an item to the encounter form  
  ;"Input:  TMGCAT -- should be ENTRY (for a particular entry) or SECTION (for group entry)
  ;"        IENINFO --PASS BY REFERENCE
  ;"                    TMGIEN(22753)=<IEN IN 22753>  -- if found
  ;"                    TMGIEN(22753.01)=<IEN IN 22753.01>  -- if relevant
  ;"        ARR -- PASS BY REFERENCE. Should be in format of ARR(<FLDNUMBER>)=<VALUE>  
  ;"               NOTE: field .01 will be used to locate subfile record, other values filed as provided.    
  ;"        ERRORMSG -- PASS BY REFERENCE, an OUT parameter.  Text added for error message.  DONT add -1^ here.
  ;"Examples of expected input
  ;"  INPUT(#)="DEL^SECTION^4567"                           <-- DELETE entry `4567.  NOTE: this will kill all contained child entries!
  ;"  INPUT(#)="DEL^ENTRY^4567^.01='667788"                  <-- delete entry for ICD (IEN80) 667788, in section `4567
  NEW TMGFDA,TMGMSG
  SET TMGIEN=+$GET(TMGIEN(22753))
  IF TMGIEN'>0 DO  QUIT
  . DO ADDERROR(.ERRORMSG,"Unable to find SECTION IEN.")
  IF TMGCAT="ENTRY" DO
  . NEW SUBIEN SET SUBIEN=$GET(TMGIEN(22753.01))
  . IF SUBIEN'>0 DO  QUIT
  . . DO ADDERROR(.ERRORMSG,"Unable to find SUB-IEN. Got ["_SUBIEN_"]")
  . SET TMGFDA(22753.01,SUBIEN_","_TMGIEN_",",.01)="@"  
  ELSE  IF TMGCAT="SECTION" DO
  . SET TMGFDA(22753,TMGIEN_",",.01)="@"
  IF $DATA(TMGFDA)'>0 DO  QUIT
  . DO ADDERROR(.ERRORMSG,"Unable to delete item for unknow reason")
  DO FILE^DIE("E","TMGFDA","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO
  . NEW ERR SET ERR=$$GETERRST^TMGDEBU2(.TMGMSG) DO ADDERROR(.ERRORMSG,ERR)
  QUIT
  ;
EDITITEM(TMGCAT,TMGIEN,ARR,ERRORMSG)  ;"EDIT an item on the encounter form
  ;"Input:  TMGCAT -- should be ENTRY (for a particular entry) or SECTION (for group entry)
  ;"        IENINFO --PASS BY REFERENCE
  ;"                    TMGIEN(22753)=<IEN IN 22753>  -- if found
  ;"                    TMGIEN(22753.01)=<IEN IN 22753.01>  -- if relevant
  ;"        ARR -- PASS BY REFERENCE. Should be in format of ARR(<FLDNUMBER>)=<VALUE>  
  ;"               NOTE: field .01 will be used to locate subfile record, other values filed as provided.    
  ;"        ERRORMSG -- PASS BY REFERENCE, an OUT parameter.  Text added for error message.  DONT add -1^ here. 
  ;"Examples of expected input
  ;"  INPUT(#)="EDIT^SECTION^4567^.01=Ortho Stuff"          <-- rename by changing .01 field  
  ;"  INPUT(#)="EDIT^SECTION^.01=Ortho Stuff^.01=Derm Stuff"<-- rename Ortho Stuff to Derm Stuff  
  ;"  INPUT(#)="EDIT^ENTRY^4567^.01='667788^.03=Planus,Pes"  <-- edit ICD (IEN80) 667788, in section `4567, to have new .03 value
  NEW TMGFDA,TMGMSG
  SET TMGIEN=+$GET(TMGIEN(22753))
  IF TMGIEN'>0 DO  QUIT
  . DO ADDERROR(.ERRORMSG,"Unable to find SECTION IEN.")
  IF TMGCAT="ENTRY" DO
  . NEW SUBIEN SET SUBIEN=$GET(TMGIEN(22753.01))
  . IF SUBIEN'>0 DO  QUIT
  . . DO ADDERROR(.ERRORMSG,"Unable to find SUB-IEN. Got ["_SUBIEN_"]")
  . MERGE TMGFDA(22753.01,SUBIEN_","_TMGIEN_",")=ARR  
  ELSE  IF TMGCAT="SECTION" DO
  . MERGE TMGFDA(22753,TMGIEN_",")=ARR
  IF $DATA(TMGFDA)'>0 DO  QUIT
  . DO ADDERROR(.ERRORMSG,"Unable to delete item for unknow reason")
  DO FILE^DIE("E","TMGFDA","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO
  . NEW ERR SET ERR=$$GETERRST^TMGDEBU2(.TMGMSG) DO ADDERROR(.ERRORMSG,ERR)
  QUIT
  ;
SECTIONIEN(IENINFO) ;Get IEN of passed section
  ;"        IENINFO --PASS BY REFERENCE
  ;"                    TMGIEN(22753)=<IEN IN 22753>  -- if found
  ;"                    TMGIEN(22753.01)=<IEN IN 22753.01>  -- if relevant
  NEW X,Y,DIC
  SET X=$PIECE(IENINFO,"=",2)
  IF $EXTRACT(X,1)="`" SET X=$PIECE(X,"`",2)  ;"STRIP TIC
  SET DIC=22753,DIC(0)="MNX" DO ^DIC
  ;"IF Y>0 SET TMGIEN=+Y
  QUIT +Y
  ;
SECTSUBIEN(IEN22753,FLD01,ERRORMSG)   ;
  NEW DIC,X,Y
  SET DIC=$$OREF^DILF($NAME(^TMG(22753,IEN22753,1))),DIC(0)="",X=FLD01
  IF $EXTRACT(X,1)="`" SET X=$EXTRACT(X,2,$LENGTH(X)),DIC(0)="U"
  DO ^DIC
  IF +Y'>0 DO  QUIT
  . DO ADDERROR(.ERRORMSG,"Unable to find subfile record.  Got [.01="_$GET(ARR2(.01))_"]")
  QUIT Y
  ;
ADDERROR(ERRORMSG,ERROR)  ;
  IF $get(ERRORMSG)'="" SET ERRORMSG=ERRORMSG_","
  SET ERRORMSG=$GET(ERRORMSG)_ERROR
  QUIT
  ;
TESTEDITENC ;
  NEW ARR
  SET ARR(1)="ADD^SECTION^0^.01=Misc^.02=12"                          ;" <-- ADD new section named Misc with sequence of 12.  NOTE: don't include any child entries here.  
  SET ARR(1.5)="ADD^SECTION^0^.01=Misc^.02=12"                        ;" <-- ADD new section named Misc with sequence of 12.  NOTE: don't include any child entries here.  
  SET ARR(2)="EDIT^SECTION^.01=Misc^.01=Misc-Stuff"                   ;" <-- rename .01 field  
  SET ARR(3)="ADD^ENTRY^.01=Misc-Stuff^.01=`572555^.03=Pes Planus"    ;" <-- add 'Pes Planus', for ICD (IEN80) 667788, into section `4567
  SET ARR(3.5)="ADD^ENTRY^.01=Misc-Stuff^.01=`572555^.03=Pes Planus"  ;" <-- add 'Pes Planus', for ICD (IEN80) 667788, into section `4567
  SET ARR(4)="EDIT^ENTRY^.01=Misc-Stuff^.01=`572555^.03=Planus,Pes"   ;" <-- edit ICD (IEN80) 667788, in section `4567, to have new .03 value
  SET ARR(5)="DEL^ENTRY^.01=Misc-Stuff^.01=`572555"                   ;" <-- delete entry for ICD (IEN80) 667788, in section `4567
  SET ARR(6)="DEL^SECTION^.01=Misc-Stuff"                             ;" <-- DELETE entry NOTE: this will kill all contained child entries!
  NEW TMGRESULT
  DO ENCEDIT(.TMGRESULT,.ARR)  ;"RPC: TMG CPRS ENCOUNTER EDIT
  QUIT
  ;"
LABTEST(TMGRESULT,TMGDFN,TESTS,ICDS)  ;"RPC TMG LAB SAVE PRETEST
  NEW TMGDEBUG SET TMGDEBUG=0
  IF TMGDEBUG=1 DO
  . SET TMGDFN=$G(^TMG("LABTEST","TMGDFN"))
  . SET TESTS=$G(^TMG("LABTEST","TESTS"))
  . SET ICDS=$G(^TMG("LABTEST","ICDS"))
  ELSE  DO
  . SET ^TMG("LABTEST","TMGDFN")=TMGDFN
  . SET ^TMG("LABTEST","TESTS")=TESTS
  . SET ^TMG("LABTEST","ICDS")=ICDS
  SET TMGRESULT="1^SUCCESS"
  NEW HEADING SET HEADING="YOUR LAB ORDER HAS THE FOLLOWING WARNINGS"
  NEW PROMPT SET PROMPT="Would you like to edit this lab order?"
  NEW LF SET LF="@@BR@@"
  NEW TODAY SET TODAY=$$TODAY^TMGDATE
  NEW AGE
  K VADM SET AGE=$$AGE^TIULO(TMGDFN)
  NEW SEX SET SEX=$P($G(^DPT(TMGDFN,0)),"^",2)
  NEW MESSAGE SET MESSAGE=""   ;"SET MESSAGE="YOU NEED MORE DIAGNOSES CODES (TEST)"
  IF $$UP^XLFSTR(TESTS)["HGBA1C" DO
  . NEW LASTA1CDT,LASTA1C DO GETLLAB^TMGPXR01(TMGDFN,97,.LASTA1CDT,.LASTA1C)
  . SET LASTA1CDT=$P(LASTA1CDT,".",1)
  . NEW DAYSDIFF SET DAYSDIFF=$$DAYSDIFF^TMGDATE(TODAY,LASTA1CDT)
  . IF DAYSDIFF="" SET DAYSDIFF=999999
  . IF (ICDS["R73.09")!(ICDS["E11.9")!(ICDS["E11.65") QUIT
  . IF DAYSDIFF<1095 DO
  . . IF MESSAGE'="" SET MESSAGE=MESSAGE_LF
  . . SET MESSAGE=MESSAGE_"LAST A1C WAS DONE ON: "_$$EXTDATE^TMGDATE(LASTA1CDT)_". A SCREENING CODE MAY NOT COVER THIS ONE. (ONLY 1 EVERY 3 YRS IS ALLOWED)"
  IF ($$UP^XLFSTR(ICDS)["BPH")!($$UP^XLFSTR(ICDS)["PROSTATE")!($$UP^XLFSTR(ICDS)["N40.0") DO
  . IF SEX="F" DO 
  . . IF MESSAGE'="" SET MESSAGE=MESSAGE_LF
  . . SET MESSAGE=MESSAGE_"A DIAGNOSIS CONCERNING PROSTATE WAS SELECTED FOR A FEMALE PATIENT."
  IF MESSAGE'="" DO
  . SET TMGRESULT="-1^"_HEADING_LF_LF_MESSAGE_LF_LF_PROMPT
  QUIT;
  ;"
ENCTEST(TMGRESULT,TMGDFN,CPTS,ICDS,HTML) ;"RPC TMG ENC SAVE PRETEST
  SET TMGRESULT="1^SUCCESS"
  SET HTML=+$G(HTML)
  NEW HEADING SET HEADING="YOUR ENCOUNTER INFORMATION HAS THE FOLLOWING WARNINGS"
  NEW PROMPT SET PROMPT="Would you like to edit this encounter?"
  NEW LF 
  IF HTML="1" DO
  . SET LF="<P>"
  ELSE  DO
  . SET LF="@@BR@@"
  NEW TMGTEST SET TMGTEST=0
  IF TMGTEST=1 DO
  . SET TMGDFN=$GET(^TMG("ENCTEST","DFN"))
  . SET CPTS=$GET(^TMG("ENCTEST","CPTS"))
  . SET ICDS=$GET(^TMG("ENCTEST","ICDS"))
  ELSE  DO
  . SET ^TMG("ENCTEST","DFN")=$GET(TMGDFN)
  . SET ^TMG("ENCTEST","CPTS")=$GET(CPTS)
  . SET ^TMG("ENCTEST","ICDS")=$GET(ICDS)
  NEW CPTARR,ICDARR,MESSAGE
  DO STR2ARR(.CPTARR,CPTS,"^")
  DO STR2ARR(.ICDARR,ICDS,"^")
  ;"
  ;"NOW BEGIN TESTING
  SET MESSAGE=""
  DO INSCPE(.CPTARR,TMGDFN,.MESSAGE,LF)
  DO CPEAGE(.CPTARR,TMGDFN,.MESSAGE,LF)
  DO ADDON(.CPTARR,TMGDFN,.MESSAGE,LF)
  DO CHGAWV(.CPTARR,TMGDFN,.MESSAGE,LF)
  DO G2CODE(.CPTARR,TMGDFN,.MESSAGE,LF)
  DO ICDCOUNT(.ICDARR,TMGDFN,.MESSAGE,LF)
  ;"   
  IF MESSAGE'="" DO
  . IF HTML="1" DO
  . . SET TMGRESULT="-1^"_"<TABLE border=""1""><TR bgcolor=""#ff3333""><TD>"_HEADING_"</TD></TR><TR><TD>"_MESSAGE_"</TD></TR></TABLE>"
  . ELSE  DO
  . . SET TMGRESULT="-1^"_HEADING_LF_LF_MESSAGE_LF_LF_PROMPT
  QUIT
  ;"
ICDCOUNT(ICDARR,TMGDFN,MESSAGE,LF)  ;"DETERMINE IF ICD COUNT IS OVER 12
  NEW COUNT SET COUNT=0  
  NEW TEXT SET TEXT=""
  NEW ICD SET ICD=""
  FOR  SET ICD=$O(ICDARR(ICD)) QUIT:ICD=""  DO
  . SET COUNT=COUNT+1
  SET TEXT="CONTAINS: "_COUNT_" DXs"
  DO HFICDCOUNT(.TMGDFN,.COUNT,.TEXT)
  IF COUNT>11 DO
  . NEW EXCESS SET EXCESS=COUNT-11
  . DO ADDMSG(.MESSAGE,LF,"ENCOUNTER MAY EXCEED 12 DIAGNOSES: "_TEXT)
  . DO ADDMSG(.MESSAGE,LF,"  *Try reducing by: "_EXCESS_" codes.")
  QUIT
  ;"
HFICDCOUNT(TMGDFN,COUNT,TEXT)   ;"Add HFs from todays encounter
  NEW IEN SET IEN=0
  NEW TODAY SET TODAY=$$TODAY^TMGDATE
  NEW IMMARR 
  SET IMMARR(2746)=""   ;"FLU ORDERED
  SET IMMARR(2755)=""   ;"P-20 ORDERED
  NEW IMMORDERED SET IMMORDERED=0
  FOR  SET IEN=$ORDER(^AUPNVHF("C",TMGDFN,IEN)) QUIT:IEN'>0  DO
  . NEW ZN SET ZN=$G(^AUPNVHF(IEN,0))
  . NEW ENCIEN SET ENCIEN=$P(ZN,"^",3)  ;"get data
  . NEW VISITDATE SET VISITDATE=$P($G(^AUPNVSIT(ENCIEN,0)),"^",1) ;"get data
  . IF $P(VISITDATE,".",1)'=TODAY QUIT
  . NEW HFIEN SET HFIEN=$P(ZN,"^",1)
  . IF $D(IMMARR(HFIEN)) SET IMMORDERED=1
  . IF HFIEN=2310 DO
  . . SET COUNT=COUNT+1  ;"PHQ-9 DONE TODAY
  . . SET TEXT=TEXT_", PHQ-9"
  IF IMMORDERED=1 DO
  . SET COUNT=COUNT+1
  . SET TEXT=TEXT_", IMMUNIZATIONS"
  ;"
  ;"CHECK FOR CKD
  NEW CKD
  ;"SET TMGTABLE=$$GETTABLX^TMGTIUO6(+$G(TMGDFN),"[CKD/RENAL]",.TMGTABLEARR)
  SET CKD=$$CKDSTAGE^TMGPXR01(+$G(TMGDFN))
  SET CKD=$$TRIM^XLFSTR($P(CKD,"CKD STAGE = ",2))
  SET CKD=+$G(CKD)
  IF CKD>1 DO
  . SET COUNT=COUNT+1
  . SET TEXT=TEXT_", CKD"
  ;"CHECK FOR BMI
  NEW TMPARR,TMPTEXT,BDATE,EDATE
  SET BDATE=$$TODAY^TMGDATE
  SET EDATE=$$TODAY^TMGDATE
  NEW BMICODE
  SET BMICODE=$$BMIVALUE^TMGC0QT3(.TMPARR,BDATE,EDATE,.TMPTEXT)
  NEW BMINAME SET BMINAME=""
  NEW DONE SET DONE=0
  NEW SEQNUM SET SEQNUM=$P($G(^DPT(TMGDFN,"TMG")),"^",2)
  FOR  SET BMINAME=$O(TMPARR(BMINAME)) QUIT:(BMINAME="")!(DONE=1)  DO
  . IF BMINAME[SEQNUM DO
  . . SET DONE=1
  . . SET COUNT=COUNT+1
  . . SET TEXT=TEXT_", BMI"
  QUIT
  ;"
STR2ARR(ARRAY,STR,DELIM)  ;
  ;"PARSE A LINE INTO AN ARRAY, USING GIVEN DELIM
  NEW CODE,PIECE SET PIECE=1
  NEW DN SET DN=""
  FOR  SET CODE=$PIECE(STR,DELIM,PIECE) QUIT:CODE=""  DO
  . SET PIECE=PIECE+1
  . SET ARRAY(CODE)=""
  QUIT
  ;"
CHGAWV(CPTARR,TMGDFN,MESSAGE,LF)  ;
  NEW ADLASKED SET ADLASKED=0
  NEW ADLHFS
  SET ADLHFS(2519)="TMG AWV ADL DID NEED HELP"
  SET ADLHFS(2524)="TMG AWV ADL DID NOT NEED HELP"
  SET ADLHFS(2525)="TMG AWV INS-ADL DID NEED HELP"
  SET ADLHFS(2526)="TMG AWV INS-ADL DID NOT NEED HELP"
  NEW HFIEN SET HFIEN=0
  FOR  SET HFIEN=$ORDER(ADLHFS(HFIEN)) QUIT:HFIEN'>0  DO
  . NEW DATE SET DATE=0
  . FOR  SET DATE=$ORDER(^AUPNVHF("AA",TMGDFN,HFIEN,DATE)) QUIT:(DATE'>0)!(ADLASKED=1)  DO
  . . NEW THISDATE SET THISDATE=9999999-DATE
  . . IF THISDATE=$$TODAY^TMGDATE SET ADLASKED=1 
  IF ADLASKED=0 QUIT
  NEW AWVCHG SET AWVCHG=0
  NEW CPT SET CPT=""
  FOR  SET CPT=$O(CPTARR(CPT)) QUIT:CPT=""  DO
  . IF CPT["G043" SET AWVCHG=1
  . IF CPT["G0402" SET AWVCHG=1
  IF AWVCHG=0 DO
  . DO ADDMSG(.MESSAGE,LF,"ADL questions were asked but AWV not charged.")
  QUIT
  ;"
ADDON(CPTARR,TMGDFN,MESSAGE,LF)  ;
  NEW INS SET INS="^"_$$GETPINS(TMGDFN)_"^"
  NEW TESTINS SET TESTINS="^AARP / Secure Horizon^BC/BS ADVANTAGE^HUMANA GOLD^MEDICARE^"
  ;"if need be we can remove an insurance from above, and uncomment below
  ;"IF TESTINS'[INS QUIT
  ;"NEW TESTINS SET TESTINS="^MEDICARE^"  ;"only medicare for now
  NEW GCODE SET GCODE=0
  NEW EMCODE SET EMCODE=0
  NEW CPT SET CPT=""
  FOR  SET CPT=$O(CPTARR(CPT)) QUIT:CPT=""  DO
  . IF CPT="G2211" SET GCODE=1
  . IF CPT["99214" SET EMCODE=CPT
  . IF CPT["99215" SET EMCODE=CPT
  IF (GCODE=0)&(EMCODE>0) DO
  . DO ADDMSG(.MESSAGE,LF,"You coded "_EMCODE_" but did not code G2211")
  QUIT
  ;"
G2CODE(CPTARR,TMGDFN,MESSAGE,LF)
  GOTO G2DN  ;"REMOVE WHEN READY TO USE
  NEW INS SET INS="^"_$$GETPINS(TMGDFN)_"^"
  NEW TESTINS SET TESTINS=""   ;" here we can add insurance to setermine"^AETNA^"
  NEW GCODE SET GCODE=0
  NEW CPT SET CPT=""
  FOR  SET CPT=$O(CPTARR(CPT)) QUIT:CPT=""  DO
  . IF CPT="G2211" SET GCODE=1
  IF (GCODE=1)&(TESTINS[INS) DO
  . DO ADDMSG(.MESSAGE,LF,"You coded G2211 but patient's "_$P(INS,"^",2)_" does not pay for it.")
G2DN
  QUIT
  ;"
INSCPE(CPTARR,TMGDFN,MESSAGE,LF)
  NEW CPEBILLED SET CPEBILLED=0
  NEW CODE SET CODE=""
  NEW CPT SET CPT=0
  FOR  SET CPT=$O(CPTARR(CPT)) QUIT:CPT'>0  DO
  . IF (CPT>99393)&(CPT<99398) SET CPEBILLED=1,CODE="("_CPT_")"
  . IF (CPT>99343)&(CPT<99388) SET CPEBILLED=1
  IF CPEBILLED'=1 QUIT
  NEW INS SET INS=$$GETPINS(TMGDFN)
  IF INS="MEDICARE" DO ADDMSG(.MESSAGE,LF,"CPE "_CODE_" cannot be billed to Medicare.")
  IF INS="UMR" DO ADDMSG(.MESSAGE,LF,"CPE "_CODE_" may not be paid by UMR.")
  NEW LASTCPE SET LASTCPE=$$LASTCPE(TMGDFN)
  IF LASTCPE>$$FIRSTYR^TMGDATE DO
  . DO ADDMSG(.MESSAGE,LF,"CPE "_CODE_" was selected but it appears a CPE was done on "_$$EXTDATE^TMGDATE(LASTCPE,1))
  QUIT
  ;"
LASTCPE(TMGDFN)
 ;"Return the date of the last CPE that was billed for the patient
 ;"also return "(MEDICARE)" at the end if they have Medicare
 NEW TMGRESULT SET TMGRESULT="-1"
 NEW CPTARRAY,IEN,VISITIEN,CPTIEN,VISITDATE
 SET IEN=0
 FOR  SET IEN=$O(^AUPNVCPT("C",TMGDFN,IEN)) QUIT:IEN'>0  DO 
 . SET CPTIEN=$PIECE($GET(^AUPNVCPT(IEN,0)),"^",1)
 . IF (CPTIEN'["9939")&(CPTIEN'["9938") QUIT
 . SET VISITIEN=$PIECE($GET(^AUPNVCPT(IEN,0)),"^",3)
 . SET VISITDATE=$PIECE($GET(^AUPNVSIT(VISITIEN,0)),"^",1)
 . SET VISITDATE=$PIECE(VISITDATE,".",1)
 . IF VISITDATE=$$TODAY^TMGDATE QUIT
 . SET CPTARRAY(VISITDATE)=CPTIEN
 IF $D(CPTARRAY) DO
 . SET TMGRESULT=$O(CPTARRAY(9999999),-1) 
 . ;"SET TMGRESULT=$$EXTDATE^TMGDATE(TMGRESULT,1)
 QUIT TMGRESULT
 ;"
CPEAGE(CPTARR,TMGDFN,MESSAGE,LF)
  NEW AGE K VADM SET AGE=+$$AGE^TIULO(TMGDFN)
  NEW CPT SET CPT=0
  FOR  SET CPT=$O(CPTARR(CPT)) QUIT:CPT'>0  DO
  . IF (CPT=99384)!(CPT=99394) DO
  . . IF (AGE<12)!(AGE>17) DO
  . . . NEW LINE SET LINE=CPT_" was selected but patient's age ("_AGE_") is not in range of 12-17 for that code"
  . . . DO ADDMSG(.MESSAGE,LF,LINE)
  . IF (CPT=99385)!(CPT=99395) DO
  . . IF (AGE<18)!(AGE>39) DO
  . . . NEW LINE SET LINE=CPT_" was selected but patient's age ("_AGE_") is not in range of 18-39 for that code"
  . . . DO ADDMSG(.MESSAGE,LF,LINE)
  . IF (CPT=99386)!(CPT=99396) DO
  . . IF (AGE<40)!(AGE>64) DO
  . . . NEW LINE SET LINE=CPT_" was selected but patient's age ("_AGE_") is not in range of 40-64 for that code"
  . . . DO ADDMSG(.MESSAGE,LF,LINE)
  . IF (CPT=99387)!(CPT=99397) DO
  . . IF AGE<65 DO
  . . . NEW LINE SET LINE=CPT_" was selected but patient's age ("_AGE_") is not in range of >64 for that code"
  . . . DO ADDMSG(.MESSAGE,LF,LINE)  
  QUIT
  ;"
ADDMSG(MESSAGE,LF,LINE)
  IF MESSAGE'="" SET MESSAGE=MESSAGE_LF_LF
  SET MESSAGE=MESSAGE_"* "_LINE
  QUIT
  ;"
GETPINS(TMGDFN)  ;"GET PATIENT'S PRIMARY INSURANCE
  NEW TMGRESULT SET TMGRESULT=""
  NEW INSIDX SET INSIDX=0
  NEW INSIEN
  FOR  SET INSIDX=$ORDER(^DPT(TMGDFN,.312,INSIDX)) QUIT:INSIDX'>0  DO
  . SET INSIEN=$PIECE($GET(^DPT(TMGDFN,.312,INSIDX,0)),"^",1)
  . NEW INSNAME SET INSNAME=$PIECE($GET(^DIC(36,INSIEN,0)),"^",1)
  . NEW COB SET COB=+$PIECE($GET(^DPT(TMGDFN,.312,INSIDX,0)),"^",20)
  . IF COB'>0 QUIT
  . IF INSNAME["(" DO
  . . SET INSNAME=$PIECE(INSNAME,"(",1)
  . SET INSNAME=$$TRIM^XLFSTR(INSNAME)
  . SET TMGRESULT=INSNAME
  QUIT TMGRESULT
  