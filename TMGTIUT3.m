TMGTIUT3 ;TMG/kst-TIU-related code ; 5/20/15, 4/11/17
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
  ;"TOPRBLNK(OUT,IN)  --TOPIC-PROBLEM LINK entry point
  ;"TOPICLST(OUT,ADFN,STARTING,DIR,MAX) -- List all topics in file 22719.51 (a subfile), or subset thereof. 
  ;"=======================================================================
  ;"PRIVATE API FUNCTIONS
  ;"=======================================================================
  ;"TESTLST ;
  ;"GETFTOP(OUT,ADFN,SECTION,SDT,EDT)  ;"GET FORMATTED TOPICS LIST for a patient
  ;"GETTOPL(OUT,ADFN,SECTION,SDT,EDT,FILTER)  ;"GET TOPICS LIST for a patient
  ;"GETFULL(OUT,ADFN,SECTION,TOPIC,SDT,EDT) ;"Get cumulative, full text for problem
  ;"HNDLSET(LINE) -- Handle SET command from TOPRBLNK
  ;"HNDLGET(LINE) -- Handle GET command from TOPRBLNK
  ;"HNDLKILL(LINE)-- Handle KILL command from TOPRBLNK
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
  ;"       OPTION -- OPTIONAL.  Used when CMD="SUM1"
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
  . SET ^TMP("TMG","TOPICS","SECTION")=SECTION
  . SET ^TMP("TMG","TOPICS","TOPICLST")=$GET(TOPICLST)
  . SET ^TMP("TMG","TOPICS","SDT")=$GET(SDT)
  . SET ^TMP("TMG","TOPICS","EDT")=$GET(EDT)
  SET SECTION=$GET(SECTION,"HPI")
  SET CMD=$GET(CMD)
  SET SDT=$GET(SDT) IF (SDT="")!(SDT="-1") SET SDT=0
  SET EDT=$GET(EDT) IF EDT="" SET EDT=9999999
  IF CMD="LIST" DO
  . DO GETFTOP(.OUT,.ADFN,.SECTION,SDT,EDT)  ;"GET FORMATTED TOPICS LIST for a patient  
  ELSE  IF CMD="SUM1" DO
  . DO GETFULL(.OUT,.ADFN,.SECTION,.TOPICLST,SDT,EDT,.OPTION) ;"Get cumulative, full text for problem
  ELSE  DO
  . SET OUT(0)="-1^Invalid CMD (command) parameter provided."
  QUIT
  ;
GETFTOP(OUT,ADFN,SECTION,SDT,EDT)  ;"GET FORMATTED TOPICS LIST for a patient
  ;"INPUT: OUT -- AN OUT PARAMETER.  PASS BY REFERENCE.  Format:
  ;"        OUT(0)="1^OK" or "-1^Error message"
  ;"        OUT(#)=<TOPIC NAME>^<IEN8925>^<FM DT>
  ;"       ADFN -- PATIENT IEN NUMBER
  ;"       SECTION -- 'HPI', or 'A&P'
  ;"       SDT -- FM date for starting date range.  OPTIONAL.  Default = 0;
  ;"       EDT -- FM date for ENDING date range.  OPTIONAL.  Default = 9999999;
  ;"Result: none
  NEW TEMP 
  SET SDT=$GET(SDT) IF SDT="" SET SDT=0
  SET EDT=$GET(EDT) IF EDT="" SET EDT=9999999
  SET TEMP(0)=$$GETTOPL(.OUT,.ADFN,.SECTION,SDT,EDT)
  KILL OUT(ADFN,"C")
  NEW TOPIC SET TOPIC=""
  ;"FOR  SET TOPIC=$ORDER(OUT(ADFN,"B",TOPIC)) QUIT:TOPIC=""  DO
  ;". NEW IEN8925 SET IEN8925=$ORDER(OUT(ADFN,"B",TOPIC,SECTION,""),-1)
  ;". FOR  SET IEN8925=$ORDER(OUT(ADFN,"B",TOPIC,SECTION,IEN8925),-1) QUIT:+IEN8925'>0  DO
  ;". . KILL OUT(ADFN,"B",TOPIC,SECTION,IEN8925)
  NEW IDX SET IDX=1  
  SET TOPIC=""
  FOR  SET TOPIC=$ORDER(OUT(ADFN,"B",TOPIC)) QUIT:TOPIC=""  DO
  . NEW IEN8925 SET IEN8925=0
  . FOR  SET IEN8925=$ORDER(OUT(ADFN,"B",TOPIC,SECTION,IEN8925)) QUIT:+IEN8925'>0  DO
  . . NEW DT SET DT=$GET(OUT(ADFN,"B",TOPIC,SECTION,IEN8925))
  . . SET TEMP(IDX)=TOPIC_"^"_IEN8925_"^"_DT SET IDX=IDX+1
  KILL OUT MERGE OUT=TEMP
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
  ;"       IEN8925 = The source document holding topic  
  ;"       SECTION -- 'HPI', or 'A&P'
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
  SET OUT(0)="1^OK"
  SET TOPIC=$GET(TOPIC)                   
  IF TOPIC="" DO  GOTO GFDN
  . SET OUT(0)="-1^Topic name not provided"
  SET SDT=$GET(SDT) IF SDT="" SET SDT=0
  SET EDT=$GET(EDT) IF EDT="" SET EDT=9999999
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
  ;"==========================================================
  ;
TOPRBLNK(OUT,IN)  ;"TOPIC-PROBLEM LINK entry point
  ;"RPC ENTRY POINT FOR: TMG CPRS TOPIC PROBLEM LINK
  ;"Input: OUT -- PASS BY REFERENCE, AN OUT PARAMETER.  Format as per Output below
  ;"       IN --  PASS BY REFERENCE, Format:
  ;"          NOTE: one or many lines can be passed.  Each is a separate command,
  ;"              and are executed in the number order #
  ;"          IN(#)='<CMD>^<Info>...'
  ;"            <CMD> can be:
  ;"               'SET', in which case <Info> should be as follows:
  ;"                   'SET^<DFN>^<TopicName>^<ProblemIEN>'
  ;"                   'SET^1234^A-fib^5678'
  ;"               'GET', in which case <Info> should be as follows:
  ;"                   'GET^<DFN>^PROB=<ProblemIEN>'
  ;"                or 'GET^<DFN>^TOPIC=<TopicName>'
  ;"                or 'GET^<DFN>^ALL'
  ;"               'KILL', in which case <Info> should be as follows:
  ;"                   'KILL^<DFN>^<TopicName>'
  ;"Results: none
  ;"Output: Out(0)=1^OK   <-- only if all filings were OK, otherwise -1^Problem
  ;"        Out(#)=1^OK  or -1^ErrorMessage  <-- if line command was a SET or KILL command
  ;"        Out(#)=<result value>   <-- if line command was a GET command.
  ;"         e.g.   1^OK^DFN^PROB=<ProblemIEN>^TOPIC=<Name>,<Name>,<Name>,....   <-- if input was PROB=<IEN>
  ;"          or    1^OK^DFN^TOPIC=<TopicName>^PROB=<IEN>                        <-- if input was TOPIC=<Name>
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
  . . SET ONERESULT=$$HNDLSET(LINE)
  . ELSE  IF CMD="GET" DO
  . . SET ONERESULT=$$HNDLGET(LINE)
  . ELSE  IF CMD="KILL" DO
  . . SET ONERESULT=$$HNDLKILL(LINE)
  . ELSE  DO
  . . SET ONERESULT="-1^Invalid command ["_CMD_"] received."
  . SET OUT(IDX)=ONERESULT
  . IF +ONERESULT'=1 SET RESULT="-1^Problem"
  SET OUT(0)=RESULT
  QUIT
  ;
HNDLSET(LINE)  ;"Handle SET command
  ;"Input: LINE -- 'SET^<DFN>^<TopicName>^<ProblemIEN>'
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
  DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG) DO  GOTO HNDSDN
  . SET RESULT="-1^"+$$GETERRST^TMGDEBU2(.TMGMSG)
  SET TOPIEN=$GET(TMGIEN(1))
  IF TOPIEN'>0 DO  GOTO HNDSDN
  . SET RESULT="-1^Unable to determine IEN of added record in file 22719.5"  
HNDS2 ;"Add subrecord or modify existing subrecord. 
  NEW TOPIC SET TOPIC=$PIECE(LINE,"^",3)
  IF TOPIC="" DO  GOTO HNDSDN
  . SET RESULT="-1^Topic name not provide in piece #3.  Input: '"_LINE_"'"
  NEW PROBIEN SET PROBIEN=+$PIECE(LINE,"^",4)
  IF PROBIEN'>0 DO  GOTO HNDSDN
  . SET RESULT="-1^Numeric PROBLEM IEN not provided in piece #4 of '"_LINE_"'"
  IF '$DATA(^AUPNPROB(PROBIEN)) DO  GOTO HNDSDN
  . SET RESULT="-1^PROBLEM IEN provided in piece #4 doesn't exist in PROBLEM file (#9000011).  Input: '"_LINE_"'"
  NEW P2DFN SET P2DFN=+$PIECE($GET(^AUPNPROB(PROBIEN,0)),"^",2)
  IF P2DFN'=ADFN DO  GOTO HNDSDN
  . SET RESULT="-1^PROBLEM IEN points to a record for a PATIENT `"_P2DFN_", which is different from that provided in piece #2.  Input: '"_LINE_"'"  
  NEW SUBIEN SET SUBIEN=+$ORDER(^TMG(22719.5,TOPIEN,1,"B",TOPIC,0))
  IF SUBIEN>0 GOTO HNDS3  ;"sub record already exists.
  ;"Add new subrecord
  KILL TMGFDA,TMGIEN,TMGMSG
  NEW IENS SET IENS="+1,"_TOPIEN_","
  SET TMGFDA(22719.51,IENS,.01)=TOPIC
  SET TMGFDA(22719.51,IENS,.02)=PROBIEN
  DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG) DO  GOTO HNDSDN
  . SET RESULT="-1^"+$$GETERRST^TMGDEBU2(.TMGMSG)
  GOTO HNDSDN
HNDS3 ;"Overwrite .02 field of existing record.  
  NEW IENS SET IENS=SUBIEN_","_TOPIEN_","
  SET TMGFDA(22719.51,IENS,.02)=PROBIEN
  DO FILE^DIE("","TMGFDA","TMGMSG")
  IF $DATA(TMGMSG) DO  GOTO HNDSDN
  . SET RESULT="-1^"+$$GETERRST^TMGDEBU2(.TMGMSG)
  GOTO HNDSDN
HNDSDN ;  
  QUIT RESULT
  ;
HNDLGET(LINE)  ;"Handle GET command.
  ;"Input: LINE.  Format
  ;"          'GET^<DFN>^PROB=<ProblemIEN>'
  ;"       or 'GET^<DFN>^TOPIC=<TopicName>'
  ;"Result: 1^OK^DFN^PROB=<ProblemIEN>^TOPIC=<Name>,<Name>,<Name>,....   <-- if input was PROB=<IEN>
  ;"  or    1^OK^DFN^TOPIC=<TopicName>^PROB=<IEN>                        <-- if input was TOPIC=<Name>
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
  IF (MODECMD="PROB")&(+VALUE>0) DO
  . SET RESULT=RESULT_"^TOPIC="
  . FOR  SET SUBIEN=$ORDER(^TMG(22719.5,TOPIEN,1,"AC",+VALUE,SUBIEN)) QUIT:+SUBIEN'>0  DO
  . . NEW ATOPIC SET ATOPIC=$PIECE($GET(^TMG(22719.5,TOPIEN,1,SUBIEN,0)),"^",1)
  . . IF $EXTRACT(RESULT,$LENGTH(RESULT))'="=" SET RESULT=RESULT_","
  . . SET RESULT=RESULT_ATOPIC
  ELSE  IF (MODECMD="TOPIC")&(VALUE'="") DO
  . SET RESULT=RESULT_"^PROB="
  . SET SUBIEN=$ORDER(^TMG(22719.5,TOPIEN,1,"B",VALUE,0))
  . IF SUBIEN'>0 DO  QUIT
  . . SET RESULT="-1^Topic '"_VALUE_"' not found in record #"_TOPIEN_".    Input: '"_LINE_"'"
  . NEW ANIEN SET ANIEN=$PIECE($GET(^TMG(22719.5,TOPIEN,1,SUBIEN,0)),"^",2)
  . SET RESULT=RESULT_ANIEN
  ELSE  IF (MODECMD="ALL") DO
  . SET RESULT=RESULT_"^ALL:"
  . FOR  SET SUBIEN=$ORDER(^TMG(22719.5,TOPIEN,1,SUBIEN)) QUIT:+SUBIEN'>0  DO
  . . NEW ZN SET ZN=$GET(^TMG(22719.5,TOPIEN,1,SUBIEN,0))
  . . IF $EXTRACT(RESULT,$LENGTH(RESULT))'=":" SET RESULT=RESULT_","
  . . SET RESULT=RESULT_$PIECE(ZN,"^",1)_"="_$PIECE(ZN,"^",2)
  ELSE  DO
  . SET RESULT="-1^Invalid mode in piece #3.  Expected 'PROB=<IEN>' or 'TOPIC=<Name>' or 'ALL'.  Input: '"_LINE_"'"
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
  NEW SUBIEN SET SUBIEN=$ORDER(^TMG(22719.5,TOPIEN,1,"B",TOPIC,0))
  IF SUBIEN'>0 GOTO HNDKDN  ;"Don't raise error if record doesn't exist to kill 
  NEW IENS SET IENS=SUBIEN_","_TOPIEN_","
  SET TMGFDA(22719.51,IENS,.01)="@"
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
  ;"        OUT(#)=<IENS>^Name^<ProbIEN>
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
  . SET OUT(CT)=SUBIEN_","_TOPIEN_",^"_$PIECE(ZN,"^",1)_"^"_$PIECE(ZN,"^",2),CT=CT+1  
TPLSTDN ;
  SET OUT(0)=RESULT
  QUIT
  ;
LKSUGEST(OUT,TOPIC)  ;"Suggest info for problems to create for given topic  
  ;"RPC ENTRY POINT FOR: TMG CPRS TOPIC PROB SUGGEST
  ;"Input: OUT -- PASS BY REFERENCE, an OUT PARAMETER.
  ;"       TOPIC -- Topic name to suggest from
  ;"Results: none
  ;"Output: OUT is filled as follows.
  ;"        OUT(0)="1^OK" or "-1^Error Message"
  ;"        OUT(#)=<"ICD" OR "10D">^<ICD CODE>^<ICD NAME>^"SCT"^<SCT CODE>^<SCT NAME>   <-- multiple entries, for each suggestion.  
  ;"        if no entries are found, then none returned.  but OUT(0) will still be "1^OK"
  ;"Code below taken (and modified heavily) from LIST^ORQQPL3
  SET TOPIC=$GET(TOPIC)
  IF TOPIC="" DO  QUIT
  . SET OUT(0)="-1^No topic name supplied"
  SET OUT(0)="1^OK"
  NEW CNT SET CNT=0
  NEW IMPLDT SET IMPLDT=$$IMPDATE^LEXU("10D")
  NEW ORIDT SET ORIDT=$$NOW^XLFDT
  NEW TMGLN SET TMGLN=""
  NEW TEMP
  NEW IEN SET IEN=0
  FOR  SET IEN=$ORDER(^TMG(22719.5,"TOPIC",TOPIC,IEN)) QUIT:+IEN'>0  DO
  . NEW SUBIEN SET SUBIEN=0
  . FOR  SET SUBIEN=$ORDER(^TMG(22719.5,"TOPIC",TOPIC,IEN,SUBIEN)) QUIT:+SUBIEN'>0  DO
  . . NEW PROBIEN SET PROBIEN=+$PIECE($GET(^TMG(22719.5,IEN,1,SUBIEN,0)),"^",2)
  . . QUIT:PROBIEN'>0
  . . ;"NOTE: PROBIEN is pointer to file 9000011
  . . NEW GMPL0,GMPL1,GMPL800,GMPL802,I,SCT,ST,ICD,DTREC,ICDD,ORDTINT,ORPLCSYS
  . . NEW ORTOTAL,LIN,INACT
  . . SET (ICDD,INACT)=""
  . . SET GMPL0=$GET(^AUPNPROB(PROBIEN,0)),GMPL1=$GET(^AUPNPROB(PROBIEN,1))
  . . SET GMPL800=$GET(^AUPNPROB(PROBIEN,800)),GMPL802=$GET(^AUPNPROB(PROBIEN,802))
  . . SET SCT=$PIECE(GMPL800,U)
  . . NEW SCTNAME SET SCTNAME=""
  . . IF SCT'="" DO
  . . . NEW IEN757D02 SET IEN757D02=$ORDER(^LEX(757.02,"APCODE",SCT_" ",0)) QUIT:IEN757D02'>0
  . . . NEW IEN757D01 SET IEN757D01=+$GET(^LEX(757.02,IEN757D02,0)) QUIT:IEN757D01'>0
  . . . SET SCTNAME=$$UP^XLFSTR($PIECE($GET(^LEX(757.01,IEN757D01,0)),"^",1))
  . . SET ST=$PIECE(GMPL0,U,12) ;" .12 STATUS: A=ACTIVE I=INACTIVE
  . . IF ST'="A" QUIT  ;"//kt added
  . . NEW LEX
  . . SET ORDTINT=$SELECT(+$PIECE(GMPL802,U,1):$PIECE(GMPL802,U,1),1:$PIECE(GMPL0,U,8))
  . . NEW ICDIEN SET ICDIEN=+GMPL0
  . . SET ORPLCSYS=$SELECT($PIECE(GMPL802,U,2)]"":$PIECE(GMPL802,U,2),1:$$SAB^ICDEX($$CSI^ICDEX(80,ICDIEN),ORDTINT))
  . . IF ORPLCSYS="" SET ORPLCSYS="ICD"  ;"//kt added
  . . SET ICD=$PIECE($$ICDDATA^ICDXCODE(ORPLCSYS,+GMPL0,ORDTINT,"I"),U,2)
  . . IF (ORIDT<IMPLDT),(+$$STATCHK^ICDXCODE($$CSI^ICDEX(80,+GMPL0),ICD,ORIDT)'=1) SET INACT="#"
  . . IF +$GET(SCT),(+$$STATCHK^LEXSRC2(SCT,ORIDT,.LEX)'=1) SET INACT="$"
  . . IF INACT'="" QUIT  ;"//kt added
  . . IF $DATA(^AUPNPROB(PROBIEN,803)) DO
  . . . NEW I SET I=0
  . . . FOR   SET I=$ORDER(^AUPNPROB(PROBIEN,803,I)) QUIT:+I'>0   SET $PIECE(ICD,"/",(I+1))=$PIECE($GET(^AUPNPROB(PROBIEN,803,I,0)),U)
  . . IF +ICD'="" SET ICDD=$$ICDDESC^GMPLUTL2(ICD,ORIDT,ORPLCSYS)
  . . SET DTREC=$PIECE(GMPL1,U,9)  ;"DATE RECORDED
  . . NEW PROBTXT SET PROBTXT=$$PROBTEXT^GMPLX(PROBIEN)
  . . ;"SET LIN=PROBIEN_U_PROBTXT_U_ICD_U_DTREC_U_INACT_U_ICDD_U_ORPLCSYS
  . . SET TMGLN=ORPLCSYS_U_ICD_U_ICDD_U_"SCT"_U_SCT_U_SCTNAME
  . . ;"SET CNT=CNT+1
  . . ;"SET OUT(CNT)=TMGLN
  . . SET TEMP(TMGLN)=""
  . ;"SET GMPL(0)=CNT
  SET TMGLN=""
  FOR  SET TMGLN=$ORDER(TEMP(TMGLN)) QUIT:TMGLN=""  DO
  . SET CNT=CNT+1,OUT(CNT)=TMGLN
  QUIT
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
