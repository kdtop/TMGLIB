TMGTOPIC ;KT - Topic Routines; 9/1/26
	;;1.0;CODE MANAGING TOPICS & THREADS;9/1/26
 ;
 ;"Functions for dealing with topic threads
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
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"TOPICRPT(ROOT,TMGDFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)  -- TOPIC REPORT, Entry point, as called from CPRS REPORT system
 ;
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;" 
 ;"T1  -- TEST1
 ;"TOPIC2DATA(TMGDFN,DATA)  --Prepare working data array of topics
 ;"PREPINFO(DATA,INFO) -- Take patient data and prepare INFO for insertion into template (designed to work with TOPICRPT^TMGHTMS1 as template)
 ;"TEST
 ;"GETCODE(OUTREF,TAG,ROUTINE) ;
 ;"TESTFIX -- test Code for fixing older threads which are full of repeats from prior entries 
 ;"FIXALL -- Code for fixing older threads which are full of repeats from prior entries
 ;"FIX22719D2(ADFN) ;
 ;"SAVECHANGES(ARR,ADFN,ERROUT)  ;
 ;" 
 ;"=======================================================================
 ;  
 ;"NOTE:  A 'Topic' is a name such as 'Back Pain', or 'HTN'.
 ;"       A 'Thread' is the series of comments written about a particular Topic, written at subsequent date-times  
 ;
 ;"=======================================================================
 ;
TOPICCMD(TMGRESULT,TMGDFN,PARAMS,SDT,EDT,DATA)  ;" RPC: TMG RPC THREAD/TOPIC CMD
  ;"Purpose: This routine will run commands as supplied in PARAMS
  ;"Input: TMGDFN -- PATIENT IEN
  ;"       PARAMS - Command to run. Format: CMD:<data>^<data>^<data>...
  ;"       SDT - Start Date FMDT -- OPTIONAL.  Default=0
  ;"       EDT - End Date FMDT -- OPTIONAL.  Default=9999999
  ;"       DATA -- OPTIONAL.  Used by various commands, see formatting below
  ;"                 DATA(#)=<TEXT LINE>
  ;"       
  ;"Result: TMGRESULT: Array.  TMGRESULT(0)=1^OK,  OR -1^ERROR MESSAGE.
  ;"         TMGRESULT(#)=<RESULTS>  See details per command.
  ;"COMMANDS: 
  ;"  CMD="LIST":
  ;"     PARAMS: (NOT USED)
  ;"     SDT,EDT:  Filters data to only if has info in date range (or default values) 
  ;"     RESULT  TMGRESULT(#)=<topicSubIEN22719.21>^<LastUsed_FMDT>^<topic name>^<HIDDEN>^<USER DATA> <-- zero node here, so any additional 0 node fields will show up too. 
  ;"  CMD="GET1":  
  ;"     PARAMS: SUBIEN22719.21
  ;"     SDT,EDT:  Filters data in topic thread to only those in in date range (or default values) 
  ;"     RESULT: TMGRSULT(1)="1^OK" OR "-1^ErrorMessage"
  ;"             TMGRESULT(#)=0^<TOPIC NAME>^<HIDDEN>^<USER DATA>  
  ;"             TMGRESULT(#)=0.5^<LinkedTableIEN>;<LinkedTableName>^... <-- as many as needed.  Entire line can also be repeated if too many for 1 line.    
  ;"             TMGRESULT(#)=1^<FMDT>^<IEN8925>^<HIDDEN>^<USER DATA>    <-- '1' indicates start of document text
  ;"             TMGRESULT(#)=2^<line of text>    <-- '2' (may be multiple) gives lines of text until next '1' node
  ;"  CMD="RENAME":  ; //kt //codex 9/24/26
  ;"     PARAMS PIECE#1: <IEN22719.21>  <-- topic IEN  ; //kt //codex 9/24/26
  ;"     PARAMS PIECE#2: <new topic name>  <-- up to 180 chars; no ^ allowed.  ; //kt //codex 9/24/26
  ;"  CMD="ADD TOPIC":
  ;"     PARAMS PIECE#1: <topic name>  <-- up to 180 chars; must be case-insensitively unique; no ^ allowed.
  ;"     RESULT: TMGRESULT(1)="1^<new IEN22719.21>" OR "-1^<ErrorMessage>"
  ;"  CMD="DEL TOPIC":
  ;"     PARAMS PIECE#1: <IEN22719.21>  <-- topic IEN
 ;"  CMD="DEL 1 TOPIC ENTRY":
 ;"     PARAMS PIECE#1: <IEN22719.21>  <-- topic IEN
 ;"     PARAMS PIECE#2: <FMDT>  <-- deletes the first matching DATETIME subentry, by sub-IEN
 ;"  CMD="ADD 1 TOPIC ENTRY":
 ;"     PARAMS PIECE#1: <IEN22719.21>  <-- topic IEN
 ;"     PARAMS PIECE#2: <FMDT>  <-- must not already exist in this topic
 ;"     DATA(#)=<TEXT LINE>  <-- initial entry text; may be empty
 ;"  CMD="SET 1 TOPIC ENTRY TEXT":
  ;"     PARAMS PIECE#1: <IEN22719.21>  <-- topic IEN
  ;"     PARAMS PIECE#2: <FMDT>  <-- targets the first matching DATETIME subentry, by sub-IEN
  ;"     DATA(#)=<TEXT LINE>  <-- replaces the entry text; an empty DATA array clears the text
  ;"  CMD="SET TOPIC HIDDEN":
  ;"     PARAMS PIECE#1: <IEN22719.21>
  ;"     PARAMS PIECE#2: 'Y' or ''  <-- NOTE: not 'N' or 'NO', just ''
  ;"  CMD="SET TOPIC USER DATA":
  ;"     PARAMS PIECE#1: <IEN22719.21>
  ;"     PARAMS PIECE#2: <VALUE FOR USER DATA>  <-- up to 64 chars.  
  ;"  CMD="SET TOPIC LINKED TABLES":
  ;"     PARAMS PIECE#1: <IEN22719.21>
  ;"     PARAMS PIECE#2: <IEN22708>^@<IEN22708>^<IEN22708>^...  <--- If entry is @<IEN22708>, then this means to DELETE that IEN  
  ;"  CMD="SET TOPIC MULTI":  
  ;"     PARAMS -- not used.  Instead multiple entries can be found in DATA array
  ;"     DATA -- Format:
  ;"         DATA(#)="IEN=<IEN22719.21>"  <-- topic IEN until another is specified
  ;"         DATA(#)="FMDT=<FMDT>"  <-- FMDT until another is specified (or new IEN entry encountered)
  ;"                 NOTE: If this is provided, then data targets fields in file 22719.211 (DATETIME sub-subfile).  
  ;"                       If not provided then data targets fields in file 22719.21 (TOPIC subfile).
  ;"         DATA(#)="HIDDEN=<VALUE>"  VALUE should be 'YES' or 'Y' or ''  <-- OPTIONAL
  ;"         DATA(#)="USER DATA=<VALUE>"  VALUE should be up to 64 chars.  <-- OPTIONAL  
  ;"  CMD="SET TOPIC FMDT ENTRY HIDDEN":
  ;"     PARAMS PIECE#1: <IEN22719.21>  <-- topic IEN
  ;"     PARAMS PIECE#2: <FMDT>
  ;"     PARAMS PIECE#3: 'Y' or ''  <-- NOTE: not 'N' or 'NO', just ''
  ;"  CMD="SET TOPIC FMDT ENTRY USER DATA":
  ;"     PARAMS PIECE#1: <IEN22719.21>  <-- topic IEN
  ;"     PARAMS PIECE#2: <FMDT>
  ;"     PARAMS PIECE#3: <VALUE FOR USER DATA>  <-- up to 64 chars.
  ;"  CMD="MERGE TOPICS":
  ;"     PARAMS PIECE#1: Destination record <IEN22719.21>  <-- Destination topic IEN
  ;"     PARAMS PIECE#2: Source record <IEN22719.21)  <-- Source topic IEN 
  ;"     PARAMS PIECE#3...: Source record <IEN22719.21)  <-- Source topic IEN 
  ;"     ... repeat as many as needed.  
  ;"  CMD="LIST TABLES":
  ;"     RESULT: TMGRESULT(1)="1^OK"
  ;"             TMGRESULT(#)=<table name>^<IEN22708>, starting at node 2
  ;"  CMD="GET TABLES AS DATA":
  ;"     PARAMS PIECE#1: <Table Name>
  ;"     PARAMS PIECE#2: <Table Name> 
  ;"     ... repeat as many as needed.  
  ;"     RESULT: TMGRSULT(1)="1^OK" OR "-1^ErrorMessage"
  ;"             TMGRESULT(#)=1^<TABLE NAME>     <-- '1' indicates start of a table grouping.  
  ;"             TMGRESULT(#)=2^<line of text>   <-- '2' (may be multiple) gives lines of text from table until next '1' node
  ;
  NEW TMGDEBUG SET TMGDEBUG=0
  NEW REF SET REF=$NAME(^TMG("TMP","TMGTOPICCMD"))
  IF TMGDEBUG=1 DO
  . SET TMGDFN=$GET(@REF@("TMGDFN"))
  . SET PARAMS=$GET(@REF@("PARAMS"))
  . SET SDT=$GET(@REF@("SDT"))
  . SET EDT=@REF@("EDT")
  . KILL DATA MERGE DATA=@REF@("DATA")
  ELSE  DO
  . KILL @REF
  . SET @REF@("TMGDFN")=$GET(TMGDFN)
  . SET @REF@("PARAMS")=$GET(PARAMS)
  . SET @REF@("SDT")=$GET(SDT)
  . SET @REF@("EDT")=$GET(EDT)
  . MERGE @REF@("DATA")=DATA
  SET SDT=+$GET(SDT)
  SET EDT=$GET(EDT) IF EDT'>0 SET EDT=9999999
  SET TMGRESULT(1)="1^OK"
  SET TMGDFN=+$GET(TMGDFN) IF TMGDFN'>0 DO  GOTO TPCDN
  . SET TMGRESULT(1)="-1^Numeric patient ID (DFN) not provided"
  SET PARAMS=$GET(PARAMS)
  NEW CMD SET CMD=$PIECE(PARAMS,":",1),PARAMS=$PIECE(PARAMS,":",2,999)
  IF CMD="LIST" DO  GOTO TPCDN
  . DO TOPICLST(.TMGRESULT,TMGDFN,SDT,EDT)  ;"Get lists of all topics for patient.
  IF CMD="GET1" DO  GOTO TPCDN
  . DO GET1(.TMGRESULT,TMGDFN,PARAMS,SDT,EDT) ;"Get text of 1 topic for patient, filtered by SDT, EDT (if provided)
  IF CMD="LIST TABLES" DO  GOTO TPCDN
  . DO LISTTABLES(.TMGRESULT)
  IF CMD="RENAME" DO  GOTO TPCDN  
  . DO RENAMETOPIC(.TMGRESULT,TMGDFN,PARAMS)  ;"Rename topic
  IF CMD="ADD TOPIC" DO  GOTO TPCDN
  . DO ADDTOPIC(.TMGRESULT,TMGDFN,PARAMS)  ;"Create topic
  IF CMD="DEL TOPIC" DO  GOTO TPCDN
  . DO DELTOPIC(.TMGRESULT,TMGDFN,PARAMS)  ;"Delete 1 topic
  IF CMD="DEL 1 TOPIC ENTRY" DO  GOTO TPCDN
  . DO DEL1TOPICDT(.TMGRESULT,TMGDFN,PARAMS)  ;"Delete 1 topic FMDT entry. 
  IF CMD="ADD 1 TOPIC ENTRY" DO  GOTO TPCDN
  . DO ADD1TOPICENTRY(.TMGRESULT,TMGDFN,PARAMS,.DATA)
  IF CMD="SET 1 TOPIC ENTRY TEXT" DO  GOTO TPCDN
  . DO SET1TOPICENTRYTEXT(.TMGRESULT,TMGDFN,PARAMS,.DATA)
  IF $PIECE(CMD," ",1,2)="SET TOPIC" DO  GOTO TPCDN
  . DO SETTOPIC(.TMGRESULT,TMGDFN,CMD,PARAMS,.DATA) ;" Handle SET TOPIC [MULTI / HIDDEN / USER DATA / FMDT ENTRY]  
  IF CMD="MERGE TOPICS" DO  GOTO TPCDN
  . DO MERGETOPICS(.TMGRESULT,TMGDFN,PARAMS)
  IF CMD="GET TABLES AS DATA" DO  GOTO TPCDN
  . DO GETTABLES(.TMGRESULT,TMGDFN,PARAMS) ;"Retrieve 1 or more tables   
  ;
TPCDN;  
  QUIT
  ;"    
 ;"=======================================================================
FMSET(OUT,FILE,IENS,FLD,VALUE)  ;"Do a write to database
  NEW TMGFDA SET TMGFDA(+$GET(FILE),$GET(IENS),+$GET(FLD))=$GET(VALUE)
  DO POSTFDA(.OUT,.TMGFDA)
  QUIT
  ;
POSTFDA(OUT,TMGFDA)  ;"Post via Fileman 
  NEW TMGMSG DO FILE^DIE("EK","TMGFDA","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO
  . NEW ERR SET ERR=$$GETERRST^TMGDEBU2(.TMGMSG)
  . IF +$GET(OUT(1))=-1 SET OUT(1)=OUT(1)_" AND "_ERR 
  . ELSE  SET OUT(1)="-1^"_ERR  
  KILL TMGFDA
  QUIT
  ;
GETTABLES(OUT,TMGDFN,PARAMS) ;"Retrieve 1 or more tables
  ;"NOTE: Table are different than topics, but many topics have a commonly-used table
  ;"      So will put code here for retrieving that here.  
  ;" PARAMS:  <TABLE_NAME>^<TABLE_NAME>^.....
  ;" RESULT: TMGRSULT(1)="1^OK" OR "-1^ErrorMessage"
  ;"         TMGRESULT(#)=1^<TABLE NAME>     <-- '1' indicates start of a table grouping.  
  ;"         TMGRESULT(#)=2^<line of text>   <-- '2' (may be multiple) gives lines of text from table until next '1' node
  NEW IDX SET IDX=1  ;"<-- 1 is already used OUT(1)='1^OK'
  NEW JDX FOR JDX=1:1:$LENGTH(PARAMS,"^") DO
  . NEW ATABLE SET ATABLE=$PIECE(PARAMS,"^",JDX) QUIT:ATABLE=""
  . NEW ARR,DROP SET DROP=$$GETTABLX^TMGTIUOJ(TMGDFN,ATABLE,.ARR)
  . SET IDX=IDX+1,OUT(IDX)="1^"_ATABLE
  . NEW KDX SET KDX=0
  . FOR  SET KDX=$ORDER(ARR(KDX)) QUIT:KDX'>0  DO
  . . SET IDX=IDX+1,OUT(IDX)="2^"_$GET(ARR(KDX))
  QUIT
  ;
LISTTABLES(OUT)  ;"Return all tables defined in file 22708
  NEW IDX,IEN,NAME
  SET OUT(1)="1^OK",IDX=1,NAME=""
  FOR  SET NAME=$ORDER(^TMG(22708,"B",NAME)) QUIT:NAME=""  DO
  . SET IEN=0
  . FOR  SET IEN=$ORDER(^TMG(22708,"B",NAME,IEN)) QUIT:IEN'>0  DO
  . . SET IDX=IDX+1,OUT(IDX)=NAME_"^"_IEN
  QUIT
  ;  
SETTOPIC(OUT,TMGDFN,CMD,PARAMS,DATA) ;" Handle SET TOPIC [MULTI / HIDDEN / USER DATA / FMDT ENTRY]
  NEW SUBIEN SET SUBIEN=+$PIECE(PARAMS,"^",1)
  NEW VALUE SET VALUE=$PIECE(PARAMS,"^",2)
  NEW IENS SET IENS=""
  IF $PIECE(CMD," ",3)="MULTI" DO  GOTO SETTPDN
  . DO SETMULTI(.OUT,TMGDFN,.DATA)
  IF $PIECE(CMD," ",3)="HIDDEN" DO  GOTO SETTPDN
  . SET IENS=SUBIEN_","_TMGDFN_","
  . DO FMSET(.OUT,22719.21,IENS,.02,VALUE)
  IF $PIECE(CMD," ",3,4)="USER DATA" DO  GOTO SETTPDN
  . SET IENS=SUBIEN_","_TMGDFN_","
  . DO FMSET(.OUT,22719.21,IENS,.03,VALUE)
  IF $PIECE(CMD," ",3,4)="LINKED TABLES" DO  GOTO SETTPDN
  . DO SETTOPICLINKEDTABLES(.OUT,TMGDFN,PARAMS)
  IF $PIECE(CMD," ",3,4)="FMDT ENTRY" DO  GOTO SETTPDN
  . NEW FMDT SET FMDT=$PIECE(PARAMS,"^",2)
  . NEW SSIEN SET SSIEN=$ORDER(^TMG(22719.2,TMGDFN,1,SUBIEN,1,"B",FMDT,0))
  . IF SSIEN'>0 DO  QUIT
  . . SET OUT(1)="-1^Unable to find entry in IEN22719.2="_TMGDFN_", IEN22719.21="_SUBIEN_" for FMDT of ["_FMDT_"]"
  . SET VALUE=$PIECE(PARAMS,"^",3)
  . SET IENS=SSIEN_","_SUBIEN_","_TMGDFN_","
  . IF $PIECE(CMD," ",5)="HIDDEN" DO  QUIT
  . . DO FMSET(.OUT,22719.211,IENS,.03,VALUE)
  . IF $PIECE(CMD," ",5,6)="USER DATA" DO  QUIT
  . . DO FMSET(.OUT,22719.211,IENS,.04,VALUE)
SETTPDN  ;  
  QUIT 
  ;
SETTOPICLINKEDTABLES(OUT,TMGDFN,PARAMS) ;"Add or remove a topic's associated PXRM tables
  NEW SUBIEN SET SUBIEN=+$PIECE(PARAMS,"^",1)
  IF SUBIEN'>0 DO  GOTO STLTDN
  . SET OUT(1)="-1^IEN for 22719.21 not provided as first piece of PARAMS"
  IF '$DATA(^TMG(22719.2,TMGDFN,1,SUBIEN,0)) DO  GOTO STLTDN
  . SET OUT(1)="-1^Topic IEN "_SUBIEN_" was not found for patient "_TMGDFN
  NEW TABLES SET TABLES=$PIECE(PARAMS,"^",2,999)
  IF TABLES="" GOTO STLTDN
  NEW IDX,COUNT,ENTRY,DELETE,TABLEIEN
  SET COUNT=$LENGTH(TABLES,"^")
  FOR IDX=1:1:COUNT DO  QUIT:+OUT(1)'=1
  . SET ENTRY=$PIECE(TABLES,"^",IDX)
  . IF ENTRY="" DO  QUIT
  . . SET OUT(1)="-1^Table IEN was not provided in parameter piece "_(IDX+1)
  . SET DELETE=($EXTRACT(ENTRY,1)="@")
  . SET TABLEIEN=+$SELECT(DELETE:$EXTRACT(ENTRY,2,999),1:ENTRY)
  . IF TABLEIEN'>0 DO  QUIT
  . . SET OUT(1)="-1^A valid file 22708 IEN is required in parameter piece "_(IDX+1)
  . IF 'DELETE,'$DATA(^TMG(22708,TABLEIEN,0)) DO  QUIT
  . . SET OUT(1)="-1^Table IEN "_TABLEIEN_" was not found in file 22708"
  IF +OUT(1)'=1 GOTO STLTDN
  FOR IDX=1:1:COUNT DO  QUIT:+OUT(1)'=1
  . SET ENTRY=$PIECE(TABLES,"^",IDX)
  . SET DELETE=($EXTRACT(ENTRY,1)="@")
  . SET TABLEIEN=+$SELECT(DELETE:$EXTRACT(ENTRY,2,999),1:ENTRY)
  . IF DELETE DO  QUIT
  . . NEW TABLESUBIEN SET TABLESUBIEN=0
  . . FOR  SET TABLESUBIEN=$ORDER(^TMG(22719.2,TMGDFN,1,SUBIEN,2,"B",TABLEIEN,TABLESUBIEN)) QUIT:(TABLESUBIEN'>0)!(+OUT(1)'=1)  DO
  . . . NEW IENS SET IENS=TABLESUBIEN_","_SUBIEN_","_TMGDFN_","
  . . . DO FMSET(.OUT,22719.212,IENS,.01,"@")
  . NEW TABLESUBIEN SET TABLESUBIEN=+$ORDER(^TMG(22719.2,TMGDFN,1,SUBIEN,2,"B",TABLEIEN,0))
  . IF TABLESUBIEN>0 QUIT
  . NEW TMGFDA,TMGIEN,TMGMSG,IENS
  . SET IENS="+1,"_SUBIEN_","_TMGDFN_","
  . SET TMGFDA(22719.212,IENS,.01)=TABLEIEN
  . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  . IF $DATA(TMGMSG("DIERR")) DO
  . . SET OUT(1)="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
STLTDN  ;
  QUIT
  ;
RENAMETOPIC(OUT,TMGDFN,PARAMS)  ;"Rename topic
  NEW SUBIEN SET SUBIEN=+$PIECE(PARAMS,"^",1)  
  IF SUBIEN'>0 DO  GOTO RTDN
  . SET OUT(1)="-1^IEN for 22719.21 not provided as first piece of PARAMS"  
  NEW TOPICNAME SET TOPICNAME=$PIECE(PARAMS,"^",2,999)  
  IF (TOPICNAME="")!(TOPICNAME["^") DO  GOTO RTDN  
  . SET OUT(1)="-1^A non-empty topic name without ^ is required"  
  NEW IENS SET IENS=SUBIEN_","_TMGDFN_","  
  DO FMSET(.OUT,22719.21,IENS,.01,TOPICNAME)  
RTDN  ;  
  QUIT  
  ;
ADDTOPIC(OUT,TMGDFN,PARAMS)  ;"Create one topic with a case-insensitively unique name
  NEW TOPICNAME SET TOPICNAME=$$TRIM^XLFSTR($PIECE(PARAMS,"^",1,999))
  IF (TOPICNAME="")!(TOPICNAME["^") DO  GOTO ATDN
  . SET OUT(1)="-1^A non-empty topic name without ^ is required"
  IF $LENGTH(TOPICNAME)>180 DO  GOTO ATDN
  . SET OUT(1)="-1^Topic name is limited to 180 characters"
  NEW DUPLICATE,EXISTING SET DUPLICATE=0,EXISTING=0
  FOR  SET EXISTING=$ORDER(^TMG(22719.2,TMGDFN,1,EXISTING)) QUIT:EXISTING'>0  DO  QUIT:DUPLICATE
  . NEW NAME SET NAME=$PIECE($GET(^TMG(22719.2,TMGDFN,1,EXISTING,0)),"^",1)
  . IF $$UP^XLFSTR(NAME)=$$UP^XLFSTR(TOPICNAME) SET DUPLICATE=1
  IF DUPLICATE DO  GOTO ATDN
  . SET OUT(1)="-1^A topic with this name already exists"
  NEW TMGFDA,TMGIEN,TMGMSG,IENS,SUBIEN
  SET IENS="+1,"_TMGDFN_","
  SET TMGFDA(22719.21,IENS,.01)=TOPICNAME
  DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO ATDN
  . SET OUT(1)="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  SET SUBIEN=+$GET(TMGIEN(1))
  IF SUBIEN'>0 DO  GOTO ATDN
  . SET OUT(1)="-1^Unable to determine IEN of newly created topic"
  SET OUT(1)="1^"_SUBIEN
ATDN  ;
  QUIT
  ;
DELTOPIC(OUT,TMGDFN,PARAMS)  ;"Delete 1 topic
  NEW SUBIEN SET SUBIEN=+$PIECE(PARAMS,"^",1)
  IF SUBIEN'>0 DO  GOTO D1TDN
  . SET OUT(1)="-1^IEN for 22719.21 not provided as first piece of PARAMS"
  IF '$DATA(^TMG(22719.2,TMGDFN,1,SUBIEN,0)) DO  GOTO D1TDN
  . SET OUT(1)="-1^Topic IEN "_SUBIEN_" was not found for patient "_TMGDFN
  NEW IENS SET IENS=SUBIEN_","_TMGDFN_","
  DO FMSET(.OUT,22719.21,IENS,.01,"@")
D1TDN  ;  
  QUIT
  ;
DEL1TOPICDT(OUT,TMGDFN,PARAMS)  ;"Delete 1 topic FMDT entry.  
  NEW SUBIEN SET SUBIEN=+$PIECE(PARAMS,"^",1)
  NEW FMDT SET FMDT=$PIECE(PARAMS,"^",2)
  IF SUBIEN'>0 DO  GOTO D1TDTDN
  . SET OUT(1)="-1^IEN for 22719.21 not provided as first piece of PARAMS"
  IF FMDT="" DO  GOTO D1TDTDN
  . SET OUT(1)="-1^FMDT not provided as second piece of PARAMS"
  IF '$DATA(^TMG(22719.2,TMGDFN,1,SUBIEN,0)) DO  GOTO D1TDTDN
  . SET OUT(1)="-1^Topic IEN "_SUBIEN_" was not found for patient "_TMGDFN
  NEW SSIEN SET SSIEN=$ORDER(^TMG(22719.2,TMGDFN,1,SUBIEN,1,"B",FMDT,0))
  IF SSIEN'>0 DO  GOTO D1TDTDN
  . SET OUT(1)="-1^Unable to find entry in topic IEN "_SUBIEN_" for FMDT "_FMDT
  NEW IENS SET IENS=SSIEN_","_SUBIEN_","_TMGDFN_","
  DO FMSET(.OUT,22719.211,IENS,.01,"@")
D1TDTDN  ;  
  QUIT
  ;
ADD1TOPICENTRY(OUT,TMGDFN,PARAMS,DATA) ;"Create one topic DATETIME entry and optionally file its WP text
  NEW SUBIEN SET SUBIEN=+$PIECE(PARAMS,"^",1)
  NEW FMDT SET FMDT=+$PIECE(PARAMS,"^",2)
  IF SUBIEN'>0 DO  GOTO A1TEDN
  . SET OUT(1)="-1^IEN for 22719.21 not provided as first piece of PARAMS"
  IF FMDT'>0 DO  GOTO A1TEDN
  . SET OUT(1)="-1^A valid FMDT is required as the second piece of PARAMS"
  IF '$DATA(^TMG(22719.2,TMGDFN,1,SUBIEN,0)) DO  GOTO A1TEDN
  . SET OUT(1)="-1^Topic IEN "_SUBIEN_" was not found for patient "_TMGDFN
  IF $ORDER(^TMG(22719.2,TMGDFN,1,SUBIEN,1,"B",FMDT,0))>0 DO  GOTO A1TEDN
  . SET OUT(1)="-1^This topic already has an entry at the requested FMDT"
  NEW TMGFDA,TMGIEN,TMGMSG,SSIEN,IENS
  SET IENS="+1,"_SUBIEN_","_TMGDFN_","
  SET TMGFDA(22719.211,IENS,.01)=FMDT
  DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO A1TEDN
  . SET OUT(1)="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  SET SSIEN=+$GET(TMGIEN(1))
  IF SSIEN'>0 DO  GOTO A1TEDN
  . SET OUT(1)="-1^Unable to determine IEN of newly created DATETIME entry"
  SET IENS=SSIEN_","_SUBIEN_","_TMGDFN_","
  IF $DATA(DATA) DO
  . KILL TMGMSG
  . DO WP^DIE(22719.211,IENS,1,"K","DATA","TMGMSG")
  . IF $DATA(TMGMSG("DIERR")) DO
  . . NEW ERR SET ERR=$$GETERRST^TMGDEBU2(.TMGMSG)
  . . KILL TMGFDA,TMGMSG
  . . SET TMGFDA(22719.211,IENS,.01)="@"
  . . DO FILE^DIE("","TMGFDA","TMGMSG")
  . . SET OUT(1)="-1^"_ERR
A1TEDN  ;
  QUIT
  ;
SET1TOPICENTRYTEXT(OUT,TMGDFN,PARAMS,DATA) ;"Replace one topic DATETIME entry's WP text from the RPC DATA list
  NEW SUBIEN SET SUBIEN=+$PIECE(PARAMS,"^",1)
  IF SUBIEN'>0 DO  GOTO S1TETDN
  . SET OUT(1)="-1^IEN for 22719.21 not provided as first piece of PARAMS"
  NEW FMDT SET FMDT=$PIECE(PARAMS,"^",2)
  IF FMDT="" DO  GOTO S1TETDN
  . SET OUT(1)="-1^FMDT not provided as second piece of PARAMS"
  IF '$DATA(^TMG(22719.2,TMGDFN,1,SUBIEN,0)) DO  GOTO S1TETDN
  . SET OUT(1)="-1^Topic IEN "_SUBIEN_" was not found for patient "_TMGDFN
  NEW SSIEN SET SSIEN=$ORDER(^TMG(22719.2,TMGDFN,1,SUBIEN,1,"B",FMDT,0))
  IF SSIEN'>0 DO  GOTO S1TETDN
  . SET OUT(1)="-1^Unable to find entry in topic IEN "_SUBIEN_" for FMDT "_FMDT
  NEW IENS SET IENS=SSIEN_","_SUBIEN_","_TMGDFN_","
  NEW TMGMSG
  IF $DATA(DATA) DO
  . DO WP^DIE(22719.211,IENS,1,"K","DATA","TMGMSG")
  ELSE  DO
  . DO WP^DIE(22719.211,IENS,1,"K","@","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO
  . SET OUT(1)="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
S1TETDN
  QUIT
  ;
MERGETOPICS(OUT,TMGDFN,PARAMS) ;"Merge 2 or more topics into 1 final topic record
  ;"INPUT:
  ;"Input: OUT -- PASS BY REFERENCE.  Used to send back results.  
  ;"       TMGDFN -- PATIENT IEN  <-- this is also IEN in 22719.2
  ;"       PARAMS -- Format: DestIEN^SrcIEN1^SrcIEN2^SrcIEN3^SrcIEN4...   <-- these are IENs in 22719.21
  NEW DEST SET DEST=$PIECE(PARAMS,"^",1)
  NEW SRC SET SRC=$PIECE(PARAMS,"^",2,$LENGTH(PARAMS,"^"))
  NEW IDX FOR IDX=1:1:$LENGTH(SRC,"^") DO
  . NEW ASRC SET ASRC=$PIECE(SRC,"^",IDX) QUIT:ASRC=""
  . DO MERGE2TOPICS(.OUT,TMGDFN,DEST,ASRC)
  QUIT
  ;
MERGE2TOPICS(OUT,TMGDFN,DESTSUBIEN,SRCSUBIEN)  ;"Merge 2 topic records into 1 final record (e.g. 'Bak pain' and 'Back pain')
  NEW SRCIENS SET SRCIENS=SRCSUBIEN_","_TMGDFN_","  
  NEW DESTIENS SET DESTIENS=DESTSUBIEN_","_TMGDFN_","  
  NEW TMGTOPICDATA,TMGENTRYDATA,TMGMSG,TMGFDA,TMGIEN  
  IF $GET(OUT(1))="" SET OUT(1)="1^OK"  
  IF +OUT(1)'=1 GOTO M2TDN  
  IF (TMGDFN'>0)!(DESTSUBIEN'>0)!(SRCSUBIEN'>0) DO  GOTO M2TDN  
  . SET OUT(1)="-1^Valid patient, destination topic, and source topic IENs are required"  
  IF DESTSUBIEN=SRCSUBIEN DO  GOTO M2TDN  
  . SET OUT(1)="-1^The source and destination topics must differ"  
  ;"Read source fields and all of its DATETIME multiple through FileMan.  
  DO GETS^DIQ(22719.21,SRCIENS,".02;.03;1*","I","TMGTOPICDATA","TMGMSG")  
  IF $DATA(TMGMSG("DIERR")) DO  GOTO M2TDN  
  . SET OUT(1)="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)  
  ;"Read destination User Data through FileMan before combining it.  
  KILL TMGMSG  
  DO GETS^DIQ(22719.21,DESTIENS,".03","I","TMGENTRYDATA","TMGMSG")  
  IF $DATA(TMGMSG("DIERR")) DO  GOTO M2TDN  
  . SET OUT(1)="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)  
  NEW SRCDATA SET SRCDATA=$GET(TMGTOPICDATA(22719.21,SRCIENS,.03,"I"))  
  NEW DESTDATA SET DESTDATA=$GET(TMGENTRYDATA(22719.21,DESTIENS,.03,"I"))  
  NEW SRCWORDS,DESTWORDS,COMBOARR,WORD,WORDIDX,COMBODATA  
  DO SPLIT2AR^TMGSTUT2(SRCDATA,"{!AN}",.SRCWORDS)  
  DO SPLIT2AR^TMGSTUT2(DESTDATA,"{!AN}",.DESTWORDS)  
  SET WORDIDX=0 FOR  SET WORDIDX=$ORDER(SRCWORDS(WORDIDX)) QUIT:WORDIDX'>0  DO  
  . SET WORD=$GET(SRCWORDS(WORDIDX)) QUIT:WORD=""  
  . SET COMBOARR(WORD)=""  
  SET WORDIDX=0 FOR  SET WORDIDX=$ORDER(DESTWORDS(WORDIDX)) QUIT:WORDIDX'>0  DO  
  . SET WORD=$GET(DESTWORDS(WORDIDX)) QUIT:WORD=""  
  . SET COMBOARR(WORD)=""  
  SET COMBODATA="",WORD=""  
  FOR  SET WORD=$ORDER(COMBOARR(WORD)) QUIT:WORD=""  DO  
  . SET COMBODATA=COMBODATA_WORD_" "  
  SET COMBODATA=$$TRIM^XLFSTR(COMBODATA)  
  ;"Create one destination DATETIME entry for every source entry, even with a duplicate FMDT.  
  NEW SRCENTRYIENS SET SRCENTRYIENS=""  
  FOR  SET SRCENTRYIENS=$ORDER(TMGTOPICDATA(22719.211,SRCENTRYIENS)) QUIT:(SRCENTRYIENS="")!(+OUT(1)'=1)  DO  
  . NEW SRCFMDT,SRCNOTEIEN,SRCHIDDEN,SRCENTRYDATA,NEWENTRYIENS,DESTENTRYIEN,DESTENTRYIENS  
  . SET SRCFMDT=$GET(TMGTOPICDATA(22719.211,SRCENTRYIENS,.01,"I"))  
  . SET SRCNOTEIEN=$GET(TMGTOPICDATA(22719.211,SRCENTRYIENS,.02,"I"))  
  . SET SRCHIDDEN=$GET(TMGTOPICDATA(22719.211,SRCENTRYIENS,.03,"I"))  
  . SET SRCENTRYDATA=$GET(TMGTOPICDATA(22719.211,SRCENTRYIENS,.04,"I"))  
  . IF SRCFMDT'>0 SET OUT(1)="-1^Source DATETIME entry has no valid entry datetime" QUIT  
  . KILL TMGFDA,TMGIEN,TMGMSG  
  . SET NEWENTRYIENS="+1,"_DESTSUBIEN_","_TMGDFN_","  
  . SET TMGFDA(22719.211,NEWENTRYIENS,.01)=SRCFMDT  
  . IF SRCNOTEIEN'="" SET TMGFDA(22719.211,NEWENTRYIENS,.02)=SRCNOTEIEN  
  . IF SRCHIDDEN'="" SET TMGFDA(22719.211,NEWENTRYIENS,.03)=SRCHIDDEN  
  . IF SRCENTRYDATA'="" SET TMGFDA(22719.211,NEWENTRYIENS,.04)=SRCENTRYDATA  
  . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")  
  . IF $DATA(TMGMSG("DIERR")) DO  QUIT  
  . . SET OUT(1)="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)  
  . SET DESTENTRYIEN=+$GET(TMGIEN(1))  
  . IF DESTENTRYIEN'>0 SET OUT(1)="-1^Unable to determine IEN of newly created DATETIME entry" QUIT  
  . SET DESTENTRYIENS=DESTENTRYIEN_","_DESTSUBIEN_","_TMGDFN_","  
  . KILL TMGENTRYDATA,TMGMSG  
  . DO GETS^DIQ(22719.211,SRCENTRYIENS,1,"","TMGENTRYDATA","TMGMSG")  
  . IF $DATA(TMGMSG("DIERR")) DO  QUIT  
  . . NEW ERR SET ERR=$$GETERRST^TMGDEBU2(.TMGMSG)  
  . . KILL TMGFDA,TMGMSG  
  . . SET TMGFDA(22719.211,DESTENTRYIENS,.01)="@"  
  . . DO FILE^DIE("","TMGFDA","TMGMSG")  
  . . SET OUT(1)="-1^"_ERR  
  . NEW TMGWP,LINEIDX  
  . SET LINEIDX=0 FOR  SET LINEIDX=$ORDER(TMGENTRYDATA(22719.211,SRCENTRYIENS,1,LINEIDX)) QUIT:LINEIDX'>0  DO  
  . . SET TMGWP(LINEIDX)=$GET(TMGENTRYDATA(22719.211,SRCENTRYIENS,1,LINEIDX))  
  . IF $DATA(TMGWP) DO  
  . . KILL TMGMSG  
  . . DO WP^DIE(22719.211,DESTENTRYIENS,1,"","TMGWP","TMGMSG")  
  . . IF $DATA(TMGMSG("DIERR")) DO  
  . . . NEW ERR SET ERR=$$GETERRST^TMGDEBU2(.TMGMSG)  
  . . . KILL TMGFDA,TMGMSG  
  . . . SET TMGFDA(22719.211,DESTENTRYIENS,.01)="@"  
  . . . DO FILE^DIE("","TMGFDA","TMGMSG")  
  . . . SET OUT(1)="-1^"_ERR  
  IF +OUT(1)'=1 GOTO M2TDN  
  ;"Store merged topic User Data and force the merged destination record visible.  
  KILL TMGFDA,TMGMSG  
  SET TMGFDA(22719.21,DESTIENS,.03)=COMBODATA  
  SET TMGFDA(22719.21,DESTIENS,.02)=""  
  DO FILE^DIE("","TMGFDA","TMGMSG")  
  IF $DATA(TMGMSG("DIERR")) DO  GOTO M2TDN  
  . SET OUT(1)="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)  
  ;"Only remove the source topic after every destination field and WP entry was filed.  
  KILL TMGFDA,TMGMSG  
  SET TMGFDA(22719.21,SRCIENS,.01)="@"  
  DO FILE^DIE("","TMGFDA","TMGMSG")  
  IF $DATA(TMGMSG("DIERR")) DO  
  . SET OUT(1)="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)  
M2TDN 
  QUIT  
  ;
SETMULTI(OUT,TMGDFN,DATA) ;"Handle SET TOPIC MULTI command  
  ;"Input: OUT -- PASS BY REFERENCE.  Used to send back results.  
  ;"       TMGDFN -- PATIENT IEN  <-- this is also IEN in 22719.2
  ;"       DATA -- Format:
  ;"         DATA(#)="IEN=<IEN22719.21>"  <-- topic IEN until another is specified
  ;"         DATA(#)="FMDT=<FMDT>"  <-- FMDT until another is specified (or new IEN entry encountered)
  ;"                 NOTE: If this is provided, then data targets fields in file 22719.211 (DATETIME sub-subfile).  
  ;"                       If not provided then data targets fields in file 22719.21 (TOPIC subfile).
  ;"         DATA(#)="HIDDEN=<VALUE>"  VALUE should be 'YES' or 'Y' or ''  <-- OPTIONAL
  ;"         DATA(#)="USER DATA=<VALUE>"  VALUE should be up to 64 chars.  <-- OPTIONAL  
  NEW SUBIEN,SUBSUBIEN SET (SUBIEN,SUBSUBIEN)=""
  NEW IENS SET IENS=""
  NEW FILE SET FILE=""
  NEW TMGFDA
  NEW IDX SET IDX=""
  FOR  SET IDX=$ORDER(DATA(IDX)) QUIT:IDX'>0  DO
  . NEW LINE SET LINE=$GET(DATA(IDX)) QUIT:LINE=""
  . IF LINE["IEN=" DO  QUIT
  . . IF $DATA(TMGFDA) DO  ;"First post any data from prior cycle.  
  . . . DO POSTFDA(.OUT,.TMGFDA)  ;"This will clear TMGFDA upon return    
  . . SET SUBIEN=+$PIECE(LINE,"IEN=",2)
  . . KILL SUBSUBIEN,TMGFDA
  . . IF SUBIEN'>0 QUIT
  . . SET FILE=22719.21
  . . SET IENS=SUBIEN_","_TMGDFN_","
  . IF LINE["FMDT=" DO  QUIT
  . . NEW FMDT SET FMDT=+$PIECE(LINE,"FMDT=",2)
  . . SET SUBSUBIEN=+$ORDER(^TMG(22719.2,TMGDFN,1,SUBIEN,1,"B",FMDT,""))
  . . SET FILE=22719.211
  . . SET IENS=SUBSUBIEN_","_SUBIEN_","_TMGDFN_","
  . IF LINE["HIDDEN=" DO  QUIT
  . . NEW VALUE SET VALUE=$PIECE(LINE,"HIDDEN=",2)
  . . NEW FLD SET FLD=0
  . . IF FILE=22719.21 SET FLD=.02
  . . ELSE  IF FILE=22719.211 SET FLD=.03
  . . SET TMGFDA(FILE,IENS,FLD)=VALUE
  . IF LINE["USER DATA=" DO  QUIT
  . . NEW VALUE SET VALUE=$PIECE(LINE,"USER DATA=",2)
  . . NEW FLD SET FLD=0
  . . IF FILE=22719.21 SET FLD=.03
  . . ELSE  IF FILE=22719.211 SET FLD=.04
  . . IF FLD>0 SET TMGFDA(FILE,IENS,FLD)=VALUE
  IF $DATA(TMGFDA) DO  ;"Post any data from last cycle.  
  . DO POSTFDA(.OUT,.TMGFDA)  ;"This will clear TMGFDA upon return   
  QUIT
  ;
GET1(OUT,TMGDFN,PARAMS,SDT,EDT) ;"Get text of 1 topic for patient, filtered by SDT, EDT (if provided)
  ;"Input: OUT -- PASS BY REFERENCE
  ;"       TMGDFN -- PATIENT IEN
  ;"       PARAMS -- IEN22719.2^...
  ;"       SDT -- START FMDT, OPTIONAL
  ;"       EDT -- END FMDT, OPTIONAL
  ;"RESULT: OUT(1)="1^OK" OR "-1^ErrorMessage" 
  ;"        OUT(#)=0^TOPIC NAME^<HIDDEN>^<USER DATA>
  ;"        OUT(#)=0.5^<LinkedTableIEN>;<LinkedTableName>^... <-- as many as needed.  Entire line can also be repeated if too many for 1 line.    
  ;"        OUT(#)=1^<FMDT>^<IEN8925>^<HIDDEN>^<USER DATA>    <-- '1' indicates start of document text
  ;"        OUT(#)=2^<line of text>               <-- '2' (may be multiple) gives lines of text until next '1' node
  ;
  SET TMGDFN=+$GET(TMGDFN)
  IF TMGDFN'>0 DO  GOTO G1DN
  . SET OUT(0)="-1^Numeric patient IEN not provided.  Got ["_$GET(TMGDFN)_"]"  
  NEW SUBIEN SET SUBIEN=+$PIECE(PARAMS,"^",1)  ;"IEN in 22719.21 (inside 22719.2)
  IF SUBIEN'>0 DO  GOTO G1DN
  . SET TMGRESULT(1)="-1^IEN for 22719.21 not provided as first piece of PARAMS"  
  SET OUT(1)="1^OK"
  SET SDT=+$GET(SDT)
  SET EDT=$GET(EDT) IF EDT'>0 SET EDT=9999999  
  NEW IDX SET IDX=1  ;"<-- 1 is already used for '1^OK', setting '1' here will effect next value used to be '2'  
  NEW ZN SET ZN=$GET(^TMG(22719.2,TMGDFN,1,SUBIEN,0))
  NEW NAME SET NAME=$PIECE(ZN,"^",1)
  SET IDX=IDX+1,OUT(IDX)="0^"_ZN  ;"<-- <TOPIC NAME>^<HIDDEN>^<USER DATA>^...
  NEW TABLESUBIEN SET TABLESUBIEN=0
  FOR  SET TABLESUBIEN=$ORDER(^TMG(22719.2,TMGDFN,1,SUBIEN,2,TABLESUBIEN)) QUIT:TABLESUBIEN'>0  DO
  . NEW TABLEIEN SET TABLEIEN=+$PIECE($GET(^TMG(22719.2,TMGDFN,1,SUBIEN,2,TABLESUBIEN,0)),"^",1) QUIT:TABLEIEN'>0
  . NEW TABLENAME SET TABLENAME=$PIECE($GET(^TMG(22708,TABLEIEN,0)),"^",1)
  . SET IDX=IDX+1,OUT(IDX)="0.5^"_TABLEIEN_";"_TABLENAME
  NEW FMDT SET FMDT=0  
  FOR  SET FMDT=$ORDER(^TMG(22719.2,TMGDFN,1,SUBIEN,1,"B",FMDT)) QUIT:FMDT'>0  DO  
  . NEW SSIDX SET SSIDX=0  
  . FOR  SET SSIDX=$ORDER(^TMG(22719.2,TMGDFN,1,SUBIEN,1,"B",FMDT,SSIDX)) QUIT:SSIDX'>0  DO  
  . . NEW ZN SET ZN=$GET(^TMG(22719.2,TMGDFN,1,SUBIEN,1,SSIDX,0))  
  . . IF (FMDT<SDT)!(FMDT>EDT) QUIT  
  . . SET IDX=IDX+1,OUT(IDX)="1^"_ZN  
  . . NEW TXI SET TXI=0  
  . . FOR  SET TXI=$ORDER(^TMG(22719.2,TMGDFN,1,SUBIEN,1,SSIDX,1,TXI)) QUIT:TXI'>0  DO  
  . . . NEW LINE SET LINE=$GET(^TMG(22719.2,TMGDFN,1,SUBIEN,1,SSIDX,1,TXI,0))  
  . . . DO ADDGET1LINE(.OUT,.IDX,LINE)
G1DN ;  
  QUIT
  ;
ADDGET1LINE(OUT,IDX,LINE) ;"Append one GET1 WP node as clean RPC result lines
  ;"Normalize CR/LF variants so no OUT() item contains an embedded line break.
  NEW LINEIDX
  SET LINE=$TRANSLATE(LINE,$CHAR(13,10),$CHAR(10))
  SET LINE=$TRANSLATE(LINE,$CHAR(10,13),$CHAR(10))
  SET LINE=$TRANSLATE(LINE,$CHAR(13),$CHAR(10))
  IF LINE="" SET IDX=IDX+1,OUT(IDX)="2^" QUIT
  FOR LINEIDX=1:1:$LENGTH(LINE,$CHAR(10)) DO
  . NEW PART SET PART=$PIECE(LINE,$CHAR(10),LINEIDX)
  . SET IDX=IDX+1,OUT(IDX)="2^"_PART
  QUIT
  ;
TOPICLST(OUT,TMGDFN,SDT,EDT)  ;"Get lists of all topics for patient.
  ;"Input: TMGDFN -- PATIENT IEN
  ;"       SDT - Start Date FMDT -- OPTIONAL.  Default=0
  ;"       EDT - End Date FMDT -- OPTIONAL.  Default=9999999
  ;"Results: OUT(#)=<topicSubIEN>^<LastUsed_FMDT>^<topic name>^<HIDDEN>  <-- 0 NODE STORED HERE (starting at piece 3), including any future 0-node fields
  SET TMGDFN=+$GET(TMGDFN)
  IF TMGDFN'>0 DO  GOTO TLDN
  . SET OUT(1)="-1^Numeric patient IEN not provided.  Got ["_$GET(TMGDFN)_"]"
  SET OUT(1)="1^OK"
  SET SDT=+$GET(SDT)
  SET EDT=$GET(EDT) IF EDT'>0 SET EDT=9999999
  NEW IDX SET IDX=1  ;"<-- 1 is already used for '1^OK', setting '1' here will effect next value used to be '2'
  NEW ANAME SET ANAME=""
  FOR  SET ANAME=$ORDER(^TMG(22719.2,TMGDFN,1,"B",ANAME)) QUIT:ANAME=""  DO
  . NEW ANIEN SET ANIEN=0
  . FOR  SET ANIEN=$ORDER(^TMG(22719.2,TMGDFN,1,"B",ANAME,ANIEN)) QUIT:ANIEN'>0  DO
  . . NEW LASTUSED SET LASTUSED=+$ORDER(^TMG(22719.2,TMGDFN,1,ANIEN,1,"B",""),-1)
  . . NEW ZN SET ZN=$GET(^TMG(22719.2,TMGDFN,1,ANIEN,0)) QUIT:ZN=""
  . . NEW ANSDT,ANEDT DO TPDTRANGE(TMGDFN,ANIEN,.ANSDT,.ANEDT)  ;"Get range of thread entries to specified topic thread
  . . IF (ANEDT<SDT)!(ANSDT>EDT) QUIT  ;"Skip if no thread entries in specified date range.  
  . . SET IDX=IDX+1,OUT(IDX)=ANIEN_"^"_LASTUSED_"^"_ZN  ;"<TOPIC NAME>^<LASTUSED_FMDT>^<HIDDEN>^<USER DATA>... (other fields in 0 node)
TLDN QUIT
  ;
TPDTRANGE(TMGDFN,TOPICIEN,OUTSDT,OUTEDT)  ;"Get range of thread entries to specified topic thread
  SET OUTSDT=$ORDER(^TMG(22719.2,TMGDFN,1,TOPICIEN,1,"B",0))
  SET OUTEDT=$ORDER(^TMG(22719.2,TMGDFN,1,TOPICIEN,1,"B",""),-1)  
  QUIT
 ;"=======================================================================
 ;
TOPICRPT(ROOT,TMGDFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)  ;"TOPIC REPORT
  ;"Purpose: Entry point, as called from CPRS REPORT system
  ;"Input: ROOT -- Pass by NAME.  This is where output goes
  ;"       TMGDFN -- Patient DFN ; ICN for foriegn sites
  ;"       ID --
  ;"       ALPHA -- Start date (in lieu of DTRANGE)
  ;"       OMEGA -- End date (in lieu of DTRANGE)
  ;"       DTRANGE -- # days back from today
  ;"       REMOTE --
  ;"       MAX    --
  ;"       ORFHIE -D- 
  goto T1
  SET @ROOT@(1)="<HTML><HEAD><TITLE>TOPIC THREAD REPORT</TITLE></HEAD><BODY>"
  NEW TOPIC SET TOPIC=""
  NEW TOPICARR
  FOR  SET TOPIC=$ORDER(^TMG(22719.2,TMGDFN,1,"B",TOPIC)) QUIT:TOPIC=""  DO
  . NEW TOPICIDX SET TOPICIDX=0
  . FOR  SET TOPICIDX=$ORDER(^TMG(22719.2,TMGDFN,1,"B",TOPIC,TOPICIDX)) QUIT:TOPICIDX'>0  DO
  . . SET TOPICARR($$UP^XLFSTR(TOPIC),TOPICIDX)=""
  ;"
  SET TOPIC=""
  NEW ROOTIDX SET ROOTIDX=1
  SET @ROOT@($I(ROOTIDX))="<TABLE BORDER=3><TR><TH>TOPIC</TH><TH>THREADS</TH></TR>"
  FOR  SET TOPIC=$ORDER(TOPICARR(TOPIC)) QUIT:TOPIC=""  DO
  . SET @ROOT@($I(ROOTIDX))="<TR><TD>"_TOPIC_"</TD><TD><ul>"
  . NEW TOPIDX SET TOPIDX=0
  . FOR  SET TOPIDX=$ORDER(TOPICARR(TOPIC,TOPIDX)) QUIT:TOPIDX'>0  DO
  . . ;"SET @ROOT@($I(ROOTIDX))="threads go here"
  . . NEW THREADIDX SET THREADIDX=0
  . . FOR  SET THREADIDX=$ORDER(^TMG(22719.2,TMGDFN,1,TOPIDX,1,THREADIDX)) QUIT:THREADIDX'>0  DO
  . . . SET @ROOT@($I(ROOTIDX))="<li>"
  . . . SET @ROOT@($I(ROOTIDX))="<B>"_$$EXTDATE^TMGDATE($GET(^TMG(22719.2,TMGDFN,1,TOPIDX,1,THREADIDX,0)),1)_"</B>"
  . . . NEW LINEIDX SET LINEIDX=0                      
  . . . FOR  SET LINEIDX=$ORDER(^TMG(22719.2,TMGDFN,1,TOPIDX,1,THREADIDX,1,LINEIDX)) QUIT:LINEIDX'>0  DO
  . . . . SET @ROOT@($I(ROOTIDX))=$GET(^TMG(22719.2,TMGDFN,1,TOPIDX,1,THREADIDX,1,LINEIDX,0))
  . . . SET @ROOT@($I(ROOTIDX))="<hr></li>"
  . SET @ROOT@($I(ROOTIDX))="</ul></td></tr>"
  QUIT
  ;
  ;
T1  ;"TEST1
  NEW ADFN SET ADFN=TMGDFN
  NEW DATA DO TOPIC2DATA(ADFN,.DATA)
  NEW INFO DO PREPINFO(.DATA,.INFO)
  NEW HTMDOC DO HTMT2DOC^TMGHTM3("HTMDOC","TOPICRPT","TMGHTMS1",.INFO)
  ;"ZWR HTMDOC
  MERGE @ROOT=HTMDOC
  QUIT
  ;
TOPIC2DATA(TMGDFN,DATA)  ;"Prepare working data array of topics
  ;"INPUT: TMGDFN -- patient IEN
  ;"        DATA -- PASS BY REFERENCE.  SEE OUTPUT
  ;"OUTPUT: DATA filled as follows:
  ;"          DATA("TOPIC",<TOPIC_NAME>,<FMDT>,#)=<line of text>
  ;
  NEW TOPIC SET TOPIC=""
  FOR  SET TOPIC=$ORDER(^TMG(22719.2,TMGDFN,1,"B",TOPIC)) QUIT:TOPIC=""  DO
  . NEW TOPICIEN SET TOPICIEN=0
  . FOR  SET TOPICIEN=$ORDER(^TMG(22719.2,TMGDFN,1,"B",TOPIC,TOPICIEN)) QUIT:TOPICIEN'>0  DO
  . . NEW FULLTOPICNAME SET FULLTOPICNAME=$PIECE($GET(^TMG(22719.2,TMGDFN,1,TOPICIEN,0)),"^",1)
  . . NEW ADT SET ADT=0
  . . FOR  SET ADT=$ORDER(^TMG(22719.2,TMGDFN,1,TOPICIEN,1,"B",ADT)) QUIT:ADT'>0  DO
  . . . NEW DTSUBIEN SET DTSUBIEN=0
  . . . FOR  SET DTSUBIEN=$ORDER(^TMG(22719.2,TMGDFN,1,TOPICIEN,1,"B",ADT,DTSUBIEN)) QUIT:DTSUBIEN'>0  DO
  . . . . NEW IDX,ODX SET (IDX,ODX)=0
  . . . . FOR  SET IDX=$ORDER(^TMG(22719.2,TMGDFN,1,TOPICIEN,1,DTSUBIEN,1,IDX)) QUIT:IDX'>0  DO
  . . . . . NEW LINE SET LINE=$GET(^TMG(22719.2,TMGDFN,1,TOPICIEN,1,DTSUBIEN,1,IDX,0))
  . . . . . SET DATA("TOPIC",FULLTOPICNAME,ADT,$INCR(ODX))=LINE
  QUIT
  ;
PREPINFO(DATA,INFO) ;"Take patient data and prepare INFO for insertion into template
  ;"NOTE: This is designed to work with TOPICRPT^TMGHTMS1 as template, and
  ;"      will use HTM2DOC^TMGHTM3 to merge the two. 
  ;"INPUT: DATA -- PASS BY REFERENCE.  Format:
  ;"                  DATA("TOPIC",<TOPIC_NAME>,<FMDT>,#)=<line of text>
  ;"       INFO -- PASS BY REFERENCE.  Format:
  ;"                  INFO("DATA",<block_name>,#)=line of HTML-valid text to put into template
  ;"Result: None.
  NEW ODX SET ODX=0
  ;
  ;"First, create Table of Contents (TOC) block
  SET INFO("DATA","TOC",$INCR(ODX))="<h3>Topics</h3>"
  SET INFO("DATA","TOC",$INCR(ODX))="<ul>"
  NEW ATOPIC SET ATOPIC=""
  NEW TOCIDX SET TOCIDX=0
  FOR  SET ATOPIC=$ORDER(DATA("TOPIC",ATOPIC)) QUIT:ATOPIC=""  DO
  . SET TOCIDX=TOCIDX+1
  . SET INFO("DATA","TOC",$INCR(ODX))="  <li><a href=""#"_TOCIDX_""" onclick=""navigateTo(event, '"_TOCIDX_"')"">"_ATOPIC_"</a></li>"
  SET INFO("DATA","TOC",$INCR(ODX))="</ul>"
  ;
  ;"Next, create TABLE block
  SET INFO("DATA","TABLE",$INCR(ODX))="<table>"
  SET ATOPIC="",TOCIDX=0
  FOR  SET ATOPIC=$ORDER(DATA("TOPIC",ATOPIC)) QUIT:ATOPIC=""  DO
  . SET TOCIDX=TOCIDX+1
  . SET INFO("DATA","TABLE",$INCR(ODX))="  <tr>"
  . SET INFO("DATA","TABLE",$INCR(ODX))="    <th id="""_TOCIDX_""">"_ATOPIC_"</th>"
  . SET INFO("DATA","TABLE",$INCR(ODX))="  </tr>"
  . SET INFO("DATA","TABLE",$INCR(ODX))="  <tr>"
  . SET INFO("DATA","TABLE",$INCR(ODX))="    <td>"
  . NEW NUMDTS SET NUMDTS=$$LISTCT^TMGMISC2($NAME(DATA("TOPIC",ATOPIC)))
  . NEW BULLETS SET BULLETS=(NUMDTS>1)
  . NEW TEXT SET TEXT=""
  . SET INFO("DATA","TABLE",$INCR(ODX))="    <ul>"
  . NEW ADT SET ADT=0
  . FOR  SET ADT=$ORDER(DATA("TOPIC",ATOPIC,ADT)) QUIT:ADT'>0  DO
  . . IF BULLETS SET TEXT=TEXT_"      <li>"
  . . NEW EDT SET EDT=$$FMTE^XLFDT(ADT,"2D")
  . . SET TEXT=TEXT_"<B>"_EDT_":</B> "
  . . NEW IDX SET IDX=0
  . . FOR  SET IDX=$ORDER(DATA("TOPIC",ATOPIC,ADT,IDX)) QUIT:IDX'>0  DO
  . . . NEW LINE SET LINE=$GET(DATA("TOPIC",ATOPIC,ADT,IDX))
  . . . SET TEXT=TEXT_LINE
  . . . SET INFO("DATA","TABLE",$INCR(ODX))=TEXT,TEXT=""
  . SET INFO("DATA","TABLE",$INCR(ODX))="      </ul>"
  . SET INFO("DATA","TABLE",$INCR(ODX))="    </td>"
  . SET INFO("DATA","TABLE",$INCR(ODX))="  </tr>"
  ;
  SET INFO("DATA","TABLE",$INCR(ODX))="</table>"
  QUIT
  ;
TEST
  ZLINK "TMGTEST"
  DO GETCODE(ROOT,"HTMLTEST1","TMGTEST")
  QUIT
  ;
GETCODE(OUTREF,TAG,ROUTINE) ;
  NEW OFFSET
  NEW IDX SET IDX=1
  NEW DONE SET DONE=0
  FOR OFFSET=1:1 DO  QUIT:DONE
  . NEW LINE SET LINE=$TEXT(@TAG+OFFSET^@ROUTINE)
  . SET LINE=$PIECE(LINE,";;",2)
  . IF LINE["DONE_WITH_HTML" SET DONE=1 QUIT
  . SET @OUTREF@($I(IDX))=LINE
  QUIT
  ;  
  ;"========================================================================
  ;"Code for fixing older threads which are full of repeats from prior entries
  ;"========================================================================
TESTFIX ; 
  ;"NEW ADFN SET ADFN=75072
  ;"NEW ADFN SET ADFN=36735  
  ;"NEW ADFN SET ADFN=75071
  NEW ADFN SET ADFN=27475
  KILL ^TMG("TMP","FIX22719.2",ADFN)  ;"Remove record of prior run. 
  DO FIX22719D2(ADFN)
  QUIT
  ;
FIXALL ; 
  NEW STIME SET STIME=$H
  NEW PATCT SET PATCT=0
  NEW ADFN SET ADFN=0
  NEW MAXDFN SET MAXDFN=$ORDER(^DPT("@"),-1)
  FOR  SET ADFN=$ORDER(^DPT(ADFN)) QUIT:ADFN'>0  DO
  . ;"SET PATCT=PATCT+1
  . ;"IF (PATCT#10=1)  DO
  . NEW PATNAME SET PATNAME=$$LJ^XLFSTR($PIECE($GET(^DPT(ADFN,0)),"^",1),22)
  . DO PROGBAR^TMGUSRI2(ADFN,"Checking "_PATNAME,1,MAXDFN,80,STIME)
  . DO FIX22719D2(ADFN) 
  QUIT  
  ;
FIX22719D2(ADFN) ;
  IF $DATA(^TMG("TMP","FIX22719.2",ADFN))>0 GOTO FXDN
  NEW DATA DO TOPIC2DATA^TMGTOPIC(ADFN,.DATA)
  NEW COMPOSITE
  NEW ATOPIC SET ATOPIC=""
  FOR  SET ATOPIC=$ORDER(DATA("TOPIC",ATOPIC)) QUIT:ATOPIC=""  DO
  . NEW TOPICS
  . NEW ADT SET ADT=0
  . FOR  SET ADT=$ORDER(DATA("TOPIC",ATOPIC,ADT)) QUIT:ADT'>0  DO
  . . NEW CURARR MERGE CURARR=DATA("TOPIC",ATOPIC,ADT)
  . . NEW CURSTR SET CURSTR=$$ARR2STR^TMGSTUT2(.CURARR," ")
  . . SET TOPICS(ADT)=CURSTR
  . NEW SAVEDTOPICS MERGE SAVEDTOPICS=TOPICS
  . DO CLEANTOPICS(.TOPICS)
  . NEW ADT SET ADT=0
  . FOR  SET ADT=$ORDER(TOPICS(ADT)) QUIT:ADT'>0  DO
  . . ;" IF $GET(TOPICS(ADT))'=$GET(SAVEDTOPICS(ADT)) DO
  . . ;" . WRITE !,"OLD: [",SAVEDTOPICS(ADT),"]",!
  . . ;" . WRITE "NEW: [",TOPICS(ADT),"]",!
  . . IF $GET(TOPICS(ADT))=$GET(SAVEDTOPICS(ADT)) DO
  . . . KILL TOPICS(ADT)  ;"kill every entry that was not changed.      
  . MERGE COMPOSITE(ATOPIC)=TOPICS
  DO SAVECHANGES(.COMPOSITE,ADFN,.ERRARR)
  IF $DATA(ERRARR)=0 DO
  . SET ^TMG("TMP","FIX22719.2",ADFN)=$$NOW^XLFDT
  ELSE  DO
  . WRITE ! ZWR ERRARR
FXDN ;  
  QUIT
  ;
SAVECHANGES(ARR,ADFN,ERROUT)  ;
  ;"NOTE: FILE1B^TMGTIUT5 is where data was initially filed as part of post-sig code.
  ;"INPUT: ARR -- PASS BY REFERENCE.  Array as created by FIX22719D2(ADFN).  Format:
  ;"            ARR(FMDT)=<text for filing>  or "" if entry needs to be deleted
  ;"       ADFN -- PATIENT IEN
  ;"       ERROUT -- PASS BY REFERENCE.  Format:
  ;"            ERROUT(FMDT)=<ERROR MESSAGE>
  NEW IEN SET IEN=+$GET(ADFN)  ;"records are DINUM'd with patient IEN, so IEN22719D2=DFN
  NEW ATOPIC SET ATOPIC=""
  FOR  SET ATOPIC=$ORDER(ARR(ATOPIC)) QUIT:ATOPIC=""  DO
  . NEW SUBIEN SET SUBIEN=0
  . FOR  SET SUBIEN=$ORDER(^TMG(22719.2,IEN,1,"B",$EXTRACT(ATOPIC,1,30),SUBIEN)) QUIT:SUBIEN'>0  DO
  . . NEW THISTOPIC SET THISTOPIC=$PIECE($GET(^TMG(22719.2,IEN,1,SUBIEN,0)),"^",1)
  . . IF THISTOPIC'=ATOPIC QUIT  
  . . NEW ADT SET ADT=0
  . . FOR  SET ADT=$ORDER(ARR(ATOPIC,ADT)) QUIT:ADT'>0  DO
  . . . NEW SUBSUBIEN SET SUBSUBIEN=$ORDER(^TMG(22719.2,IEN,1,SUBIEN,1,"B",ADT,0)) QUIT:SUBSUBIEN'>0
  . . . NEW TMGIENS SET TMGIENS=SUBSUBIEN_","_SUBIEN_","_IEN_","
  . . . NEW TMGFDA,TMGMSG,TMGIEN
  . . . NEW LINE SET LINE=$GET(ARR(ATOPIC,ADT))
  . . . IF LINE="" DO  
  . . . . SET TMGFDA(22719.211,TMGIENS,.01)="@"  ;"delete empty record
  . . . . DO FILE^DIE("E","TMGFDA","TMGMSG")
  . . . ELSE  DO
  . . . . NEW TMGWP DO STR2WP^TMGSTUT2(LINE,"TMGWP",75) IF $DATA(TMGWP)=0 QUIT
  . . . . DO WP^DIE(22719.211,TMGIENS,1,"","TMGWP","TMGMSG")
  . . . IF $DATA(TMGMSG("DIERR")) DO  QUIT
  . . . . SET ERROUT(ADT)=$$GETERRST^TMGDEBU2(.TMGMSG)
  QUIT
  ;  
CLEANTOPICS(TOPICS)  ;
  NEW DIVS SET DIVS=" ,;:.!?-"
  NEW ADTLATER SET ADTLATER=""
  FOR  SET ADTLATER=$ORDER(TOPICS(ADTLATER),-1) QUIT:ADTLATER'>0  DO                                
  . NEW CURARR,CURTEXT SET CURTEXT=$GET(TOPICS(ADTLATER)) QUIT:CURTEXT=""
  . NEW ADTEARLIER SET ADTEARLIER=ADTLATER
  . NEW PRIORCT,MAXPRIOR SET PRIORCT=0,MAXPRIOR=999  ;"<-- If MAXPRIOR=1, then only 1 prior note (the one JUST PRIOR) will be tested  
  . FOR  SET ADTEARLIER=$ORDER(TOPICS(ADTEARLIER),-1) QUIT:(ADTEARLIER'>0)!(PRIORCT>=MAXPRIOR)  DO
  . . SET PRIORCT=PRIORCT+1
  . . NEW PRIORARR,PRIORTEXT SET PRIORTEXT=$GET(TOPICS(ADTEARLIER)) QUIT:PRIORTEXT=""
  . . NEW MATCHES DO SUBSTRMATCH^TMGSTUT3(PRIORTEXT,CURTEXT,.MATCHES,.PRIORARR,.CURARR)
  . . NEW HASMATCH FOR  DO  QUIT:HASMATCH=0
  . . . SET HASMATCH=0
  . . . NEW ALEN SET ALEN=+$ORDER(MATCHES("LENIDX",""),-1) QUIT:ALEN'>0  
  . . . NEW MATCHIDX SET MATCHIDX=$ORDER(MATCHES("LENIDX",ALEN,0)) QUIT:MATCHIDX'>0
  . . . NEW AMATCH SET AMATCH=$GET(MATCHES(MATCHIDX)) QUIT:AMATCH=""
  . . . NEW MATCHLEN SET MATCHLEN=$PIECE(AMATCH,"^",3) QUIT:(MATCHLEN<=2)
  . . . SET HASMATCH=1 
  . . . NEW STARTPOS SET STARTPOS=+AMATCH
  . . . NEW LEN SET LEN=+$PIECE(AMATCH,"^",2)
  . . . NEW ENDPOS SET ENDPOS=+$PIECE(AMATCH,"^",4)
  . . . NEW PARTA SET PARTA=""
  . . . IF STARTPOS>0 SET PARTA=$EXTRACT(CURTEXT,1,STARTPOS-1)
  . . . NEW PARTB SET PARTB=$EXTRACT(CURTEXT,STARTPOS,ENDPOS)
  . . . NEW PARTC SET PARTC=""
  . . . IF LEN>0 SET PARTC=$EXTRACT(CURTEXT,ENDPOS+1,$LENGTH(CURTEXT))
  . . . SET CURTEXT=$SELECT(PARTA'="":PARTA_"...",1:"")_PARTC
  . . . NEW SCANPOS,SCANDONE SET SCANPOS=1,SCANDONE=0
  . . . FOR SCANPOS=1:1:$LENGTH(CURTEXT) DO  QUIT:SCANDONE
  . . . . NEW CH SET CH=$EXTRACT(CURTEXT,SCANPOS)
  . . . . SET SCANDONE=(((DIVS[CH)=0)&(CH'=""))
  . . . IF SCANPOS>1 DO
  . . . . IF SCANDONE=0 SET CURTEXT="" QUIT   ;"never found a non-div char.  
  . . . . SET CURTEXT=$EXTRACT(CURTEXT,SCANPOS,$LENGTH(CURTEXT))
  . . . SET TOPICS(ADTLATER)=CURTEXT
  . . . KILL CURARR,MATCHES 
  . . . DO SUBSTRMATCH^TMGSTUT3(PRIORTEXT,CURTEXT,.MATCHES,.PRIORARR,.CURARR)  
  NEW JUSTDATES,ADT SET ADT=0
  FOR  SET ADT=$ORDER(TOPICS(ADT)) QUIT:ADT'>0  SET JUSTDATES(ADT\1)=""
  SET ADT=0 FOR  SET ADT=$ORDER(TOPICS(ADT)) QUIT:ADT'>0  DO
  . NEW CURARR,CURTEXT SET CURTEXT=$GET(TOPICS(ADT))
  . IF CURTEXT="" QUIT
  . NEW DONE SET DONE=0
  . FOR  DO  QUIT:DONE
  . . NEW CH SET CH=$EXTRACT(CURTEXT,1) 
  . . IF $ASCII(CH)=-1 SET DONE=1 QUIT
  . . IF DIVS'[CH SET DONE=1 QUIT
  . . SET CURTEXT=$EXTRACT(CURTEXT,2,$LENGTH(CURTEXT))
  . SET CURTEXT=$$STRIPTAG^TMGHTM1(CURTEXT)
  . IF $$REPLSTR^TMGSTUT3(.CURTEXT,"<<-AND->>","...")  ;"ignore result
  . IF $$REPLSTR^TMGSTUT3(.CURTEXT,"... ...","...")  ;"ignore result
  . IF $$REPLSTR^TMGSTUT3(.CURTEXT,".....","...")  ;"ignore result
  . IF $$REPLSTR^TMGSTUT3(.CURTEXT,"....","...")  ;"ignore result
  . IF $$REPLSTR^TMGSTUT3(.CURTEXT,"...  ","... ")  ;"ignore result
  . NEW POSINFO DO SCANDATES^TMGSTUT3(CURTEXT,.POSINFO)
  . NEW IDX SET IDX=$ORDER(POSINFO(""),-1)
  . NEW CHECKEND SET CHECKEND=1
  . NEW DONE SET DONE=0
  . IF IDX>0 FOR  DO  QUIT:DONE  DO   ;"Work from end of string to the beginning, so earlier POS's do change
  . . SET DONE=1  ;"default, will change below if OK
  . . NEW CURLEN SET CURLEN=$LENGTH(CURTEXT)
  . . NEW LATERENTRY SET LATERENTRY=POSINFO(IDX) QUIT:LATERENTRY=""
  . . NEW LATERSTART SET LATERSTART=$PIECE(LATERENTRY,"^",1)
  . . NEW LATEREND SET LATEREND=$PIECE(LATERENTRY,"^",2)
  . . IF CHECKEND DO  ;"Check if any meaningful text occurs after LAST date
  . . . SET CHECKEND=0
  . . . NEW LATERDT SET LATERDT=$PIECE(LATERENTRY,"^",3)
  . . . SET LATERDT=$$INTDATE^TMGDATE(LATERDT)
  . . . IF $DATA(JUSTDATES(LATERDT))=0 QUIT  ;"Only cut out dates that reporesent VISIT DATES
  . . . NEW POST SET POST=$EXTRACT(CURTEXT,LATEREND+1,CURLEN)
  . . . SET POST=$TRANSLATE(POST,". ","")
  . . . IF POST'="" QUIT
  . . . SET CURTEXT=$EXTRACT(CURTEXT,1,LATERSTART-1)
  . . NEW PRIORIDX SET PRIORIDX=$ORDER(POSINFO(IDX),-1) QUIT:PRIORIDX=""  ;"If only 1 entry, will drop out. 
  . . NEW PRIORENTRY SET PRIORENTRY=POSINFO(PRIORIDX) QUIT:PRIORENTRY=""
  . . NEW PRIOREND SET PRIOREND=$PIECE(PRIORENTRY,"^",2)
  . . NEW PRIORSTART SET PRIORSTART=$PIECE(PRIORENTRY,"^",1)
  . . ;"Get text BETWEEN dates.  
  . . NEW BETWEEN SET BETWEEN=$EXTRACT(CURTEXT,PRIOREND+1,LATERSTART-1)
  . . SET BETWEEN=$TRANSLATE(BETWEEN,". ","")
  . . IF BETWEEN'="" DO  QUIT
  . . . SET IDX=PRIORIDX,DONE=0
  . . NEW PARTA SET PARTA=$EXTRACT(CURTEXT,1,PRIORSTART-1)
  . . NEW PARTB SET PARTB=$EXTRACT(CURTEXT,LATERSTART,$LENGTH(CURTEXT))
  . . KILL POSINFO(PRIORIDX)
  . . SET CURTEXT=PARTA_" "_PARTB
  . . NEW TRIMLEN SET TRIMLEN=CURLEN-$LENGTH(CURTEXT)
  . . SET LATERSTART=LATERSTART-TRIMLEN,LATEREND=LATEREND-TRIMLEN
  . . SET $PIECE(LATERENTRY,"^",1,2)=LATERSTART_"^"_LATEREND
  . . SET POSINFO(IDX)=LATERENTRY
  . . SET DONE=0  ;"loop again
  . IF $$REPLSTR^TMGSTUT3(.CURTEXT,"  "," ")  ;"ignore result
  . SET TOPICS(ADT)=$$TRIM^XLFSTR(CURTEXT)
  QUIT
  ;
  ;"========================================================================
  ;"END OF Code for fixing older threads, which will all repeats of full paragraph
  ;"========================================================================
  ;  
  ;"========================================================================
  ;"Fixing duplicate thread entries
  ;" -- This was when topic names > 30 chars were getting filed repeatedly
  ;"    because not immediately found looking only in "B" index.  (fixed now I hope). 
  ;"========================================================================
  ;
FIXDUPTHREADALLPTS() ;
  NEW TMGDFN SET TMGDFN=0
  FOR  SET TMGDFN=$ORDER(^DPT(TMGDFN)) QUIT:TMGDFN'>0  DO
  . NEW PTNAME SET PTNAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
  . WRITE PTNAME,!
  . DO FIXDUPTHREAD1PT(TMGDFN)
  QUIT
  ;
FIXDUPTHREAD1PT(TMGDFN) ;"Fix all duplicate threads for 1 patient.  
  ;"Input: TMGDFN -- PATIENT IEN
  ;"Results: none
  ;
  NEW OUT DO TOPICLST(.OUT,TMGDFN)  ;"Get lists of all topics for patient.
  NEW ARR
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(OUT(IDX)) QUIT:IDX'>0  DO
  . NEW STR SET STR=$GET(OUT(IDX)) QUIT:STR=""
  . NEW SUBIEN SET SUBIEN=$PIECE(STR,"^",1) QUIT:SUBIEN'>0
  . NEW TOPIC SET TOPIC=$PIECE(STR,"^",2) QUIT:TOPIC=""
  . SET ARR(TOPIC)=+$GET(ARR(TOPIC))+1
  . SET ARR(TOPIC,SUBIEN)=""
  NEW ATOPIC SET ATOPIC=""
  FOR  SET ATOPIC=$ORDER(ARR(ATOPIC)) QUIT:ATOPIC=""  DO
  . NEW CT SET CT=+$GET(ARR(ATOPIC)) QUIT:CT<2
  . NEW FIRSTIEN SET FIRSTIEN=0
  . NEW SUBIEN SET SUBIEN=0
  . FOR  SET SUBIEN=$ORDER(ARR(ATOPIC,SUBIEN)) QUIT:SUBIEN'>0  DO
  . . IF FIRSTIEN=0 SET FIRSTIEN=SUBIEN QUIT
  . . NEW ARESULT SET ARESULT=$$MERGEDUPTHREAD(TMGDFN,FIRSTIEN,SUBIEN)
  . . IF ARESULT'>0 WRITE !,$PIECE(ARESULT,"^",2),!
  QUIT
  ;
MERGEDUPTHREAD(TMGDFN,DESTIEN,SRCIEN)  ;"Fix 1 duplicate thread pair for 1 patient's topic. 
  ;"Input: TMGDFN -- PATIENT IEN
  ;"       DESTIEN -- IEN in 22719.21 (TOPIC field subfile) which will RECEIVE the other record
  ;"       SRCIEN -- IEN in 22719.21 (TOPIC field subfile) which will SUPPLY a record to put into other.    
  ;"RESULT: 1^OK, or -1^ERROR if any
  ;"NOTES: -- Each TOPIC field subfile may have 1 or more DATETIME subentries. These are each processed in turn.
  ;"       -- Source records will be deleted when done unless there was error.  
  ;"Get source records
  NEW TMGDATA,TMGMSG,FIRSTREC       
  NEW TMGRESULT SET TMGRESULT="1^OK"
  NEW TRIMTOPIC SET TRIMTOPIC=$EXTRACT($GET(^TMG(22719.2,TMGDFN,1,SRCIEN,0)),1,30)
  NEW SRCSUBIEN SET SRCSUBIEN=0
  FOR  SET SRCSUBIEN=$ORDER(^TMG(22719.2,TMGDFN,1,SRCIEN,1,SRCSUBIEN)) QUIT:(SRCSUBIEN'>0)!(+TMGRESULT'>0)  DO
  . SET TMGRESULT=$$MERGE1DTT(TMGDFN,DESTIEN,SRCIEN,SRCSUBIEN)  
  . SET FIRSTREC=+$ORDER(^TMG(22719.2,TMGDFN,1,SRCIEN,1,0))
  . IF FIRSTREC'>0 DO
  . . IF $DATA(^TMG(22719.2,TMGDFN,1,SRCIEN,0))'>0 QUIT
  . . KILL ^TMG(22719.2,TMGDFN,1,SRCIEN)
  . . KILL ^TMG(22719.2,TMGDFN,1,"B",TRIMTOPIC,SRCIEN)
  SET FIRSTREC=+$ORDER(^TMG(22719.2,TMGDFN,1,SRCIEN,1,0))
  IF FIRSTREC'>0 DO
  . KILL ^TMG(22719.2,TMGDFN,1,SRCIEN) 
  . KILL ^TMG(22719.2,TMGDFN,1,"B",TRIMTOPIC,SRCIEN)
  . NEW REF SET REF=$NAME(^TMG(22719.2,TMGDFN,1,0))
  . NEW RECCT SET RECCT=+$PIECE($GET(@REF),"^",4)-1 IF RECCT<0 SET RECCT=0
  . SET $PIECE(@REF,"^",4)=RECCT
  QUIT TMGRESULT
  ;
MERGE1DTT(TMGDFN,DESTIEN,SRCIEN,SRCSUBIEN) ;"Merge 1 duplicate FMDT entry for thread pair for 1 patient's topic. 
  ;"Input: TMGDFN -- PATIENT IEN
  ;"       DESTIEN -- IEN in 22719.21 (TOPIC field subfile) which will RECEIVE the other record
  ;"       SRCIEN -- IEN in 22719.21 (TOPIC field subfile) which will SUPPLY a record to put into other.    
  ;"       SRCsubIEN -- subIEN in 22719.211 (DATETIME field subfile) which will SUPPLY a record to put into other.    
  ;"RESULT: 1^OK, or -1^ERROR if any
  ;"NOTES: -- Source records will be deleted when done unless there was error.
  ;"       -- If DEST record already contains an FMDT subentry for same FMDT as src, then copy will NOT be done, but
  ;"          SRC subentry will be deleted.  
  ;
  NEW TMGRESULT SET TMGRESULT="1^OK"
  IF $DATA(^TMG(22719.2,TMGDFN,1,SRCIEN,1,SRCSUBIEN))'>0 DO  GOTO M1DTTDN
  . SET TMGRESULT="-1^No data in 22719.211, iens="_SRCSUBIEN_","_SRCIEN_","_TMGDFN_","
  NEW ZN SET ZN=$GET(^TMG(22719.2,TMGDFN,1,SRCIEN,1,SRCSUBIEN,0))
  NEW COPYFMDT SET COPYFMDT=$PIECE(ZN,"^",1) IF COPYFMDT'>0 DO  GOTO M1DTTDN
  . SET TMGRESULT="-1^Unable to get FMDT of src records"
  NEW DEST22719D21 SET DEST22719D21=+$ORDER(^TMG(22719.2,TMGDFN,1,DESTIEN,1,"B",COPYFMDT,0))
  NEW DUPLICATE SET DUPLICATE=(DEST22719D21>0)
  IF 'DUPLICATE SET DEST22719D21=+$ORDER(^TMG(22719.2,TMGDFN,1,DESTIEN,1,"@"),-1)+1
  IF DEST22719D21'>0 DO  GOTO M1DTTDN
  . SET TMGRESULT="-1^Unable to determine target IEN in 22719.2"
  IF 'DUPLICATE DO   ;"Do actual copy
  . MERGE ^TMG(22719.2,TMGDFN,1,DESTIEN,1,DEST22719D21)=^TMG(22719.2,TMGDFN,1,SRCIEN,1,SRCSUBIEN)
  . ;"create DEST B index entry
  . SET ^TMG(22719.2,TMGDFN,1,DESTIEN,1,"B",COPYFMDT,DEST22719D21)=""
  . ;"Update record count and last IEN
  . SET REF=$NAME(^TMG(22719.2,TMGDFN,1,DESTIEN,1,0))
  . NEW RECCT SET RECCT=+$PIECE($GET(@REF),"^",4)
  . SET $PIECE(@REF,"^",4)=RECCT+1
  . SET $PIECE(@REF,"^",3)=DEST22719D21
  DO  ;"Delete source subrecord
  . KILL ^TMG(22719.2,TMGDFN,1,SRCIEN,1,SRCSUBIEN)
  . KILL ^TMG(22719.2,TMGDFN,1,SRCIEN,1,"B",COPYFMDT)
  . NEW REF SET REF=$NAME(^TMG(22719.2,TMGDFN,1,SRCIEN,1,0))
  . NEW RECCT SET RECCT=+$PIECE($GET(@REF),"^",4)-1 IF RECCT<0 SET RECCT=0
  . SET $PIECE(@REF,"^",4)=RECCT
  . IF RECCT=0 DO
  . . NEW FIRSTREC SET FIRSTREC=+$ORDER(^TMG(22719.2,TMGDFN,1,SRCIEN,1,0))
  . . IF FIRSTREC>0 QUIT
  . . KILL ^TMG(22719.2,TMGDFN,1,SRCIEN,1)
  ;
M1DTTDN QUIT TMGRESULT  
  ;
  ;"========================================================================
  ;"END OF Fixing duplicate thread entries
  ;"========================================================================

  ;"========================================================================
  ;" CODE FOR REBUILDING TOPICS DATA
  ;"========================================================================
  ;
REBUILD1PT(TMGDFN,OPTION) ;"Rebuild all stored topic data for one patient  ;"//kt 9/30/26
  ;"Input: TMGDFN -- IEN in PATIENT file (#2)
  ;"       OPTION -- OPTION("SILENT") -- OPTIONAL.  If 1, suppress per-patient status output.
  ;"Results: none
  SET TMGDFN=+$GET(TMGDFN)  ;"//kt 9/30/26
  SET SILENT=+$GET(OPTION("SILENT"))  ;"//kt 9/30/26
  IF '$DATA(^DPT(TMGDFN,0)) DO  GOTO R1PDN  ;"//kt 9/30/26
  . WRITE !,"Invalid patient IEN: ",TMGDFN,!
  SET OPTION("REMOVE OLD")=1  ;"//kt 9/30/26 Reconcile threads no longer produced by the corrected parser
  NEW PTNAME SET PTNAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)  ;"//kt 9/30/26
  ;"IF 'SILENT WRITE !,"Rebuilding topic data for ",PTNAME," (",TMGDFN,")",!  ;"//kt 9/30/26
  NEW MAXTIUDT SET MAXTIUDT=$ORDER(^TIU(8925,"ZTMGPTDT",TMGDFN,""),-1)  ;"//kt 10/1/26 Get the most recent event-date key from the patient TIU cross reference
  NEW MAXTIUIEN SET MAXTIUIEN=0  ;"//kt 10/1/26
  IF MAXTIUDT>0 SET MAXTIUIEN=$ORDER(^TIU(8925,"ZTMGPTDT",TMGDFN,MAXTIUDT,""),-1)  ;"//kt 10/1/26 Use the final TIU IEN as the progress-bar maximum
  NEW TIUDT SET TIUDT=0  ;"//kt 9/30/26 Process notes in event-date order so prior-thread checks are valid
  NEW NOTECOUNT SET NOTECOUNT=0  ;"//kt 10/1/26
  NEW NOTESTART SET NOTESTART=$H  ;"//kt 10/1/26
  FOR  SET TIUDT=$ORDER(^TIU(8925,"ZTMGPTDT",TMGDFN,TIUDT)) QUIT:TIUDT'>0  DO  ;"//kt 9/30/26
  . NEW TIUIEN SET TIUIEN=0  ;"//kt 9/30/26
  . FOR  SET TIUIEN=$ORDER(^TIU(8925,"ZTMGPTDT",TMGDFN,TIUDT,TIUIEN)) QUIT:TIUIEN'>0  DO  ;"//kt 9/30/26
  . . SET NOTECOUNT=NOTECOUNT+1  ;"//kt 10/1/26
  . . IF ('SILENT)&(NOTECOUNT#10=0) DO
  . . . DO PROGBAR^TMGUSRI2(TIUIEN,"Note IEN# "_TIUIEN,1,MAXTIUIEN,60,NOTESTART)  ;"//kt 10/1/26
  . . NEW QUIET  ;"//kt 9/30/26
  . . DO SUMM1^TMGTIUT6(TIUIEN,.OPTION)  ;"//kt 9/30/26 Reparse and rewrite this note's topic/thread data
  . . IF $DATA(QUIET(TIUIEN)) WRITE !,"  TIU IEN ",TIUIEN,": ",QUIET(TIUIEN),!  ;"//kt 9/30/26
  ;"IF 'SILENT WRITE "Topic data rebuild complete.",!  ;"//kt 9/30/26
  IF 'SILENT WRITE "                                                                 ",!
  DO CUU^TMGTERM(1)
R1PDN ;
  QUIT
  ;
TEST1REBUILD ;"Select and rebuild topic data for one patient  ;"//kt 9/30/26
  NEW DIC,X,Y  ;"//kt 9/30/26
  SET DIC=2,DIC(0)="AEMQ"  ;"//kt 9/30/26 Standard FileMan patient lookup
  DO ^DIC WRITE !  ;"//kt 9/30/26
  QUIT:+Y'>0  ;"//kt 9/30/26
  NEW OPTION SET OPTION("ONLY OV")=0
  DO REBUILD1PT(+Y,.OPTION)  ;"//kt 9/30/26
  QUIT
  ;
REBUILDALL ;"Rebuild stored topic data for every patient  ;"//kt 9/30/26
  NEW STARTTIME SET STARTTIME=$H  
  NEW MAXDFN SET MAXDFN=+$ORDER(^DPT("@"),-1)  ;"//kt 10/1/26 ^DPT root reverse order returns a string xref, not the last patient IEN
  NEW OPTION  ;"//kt 10/1/26 Do not set SILENT; show note-level activity for the active patient
  SET OPTION("ONLY OV")=0
  NEW TMGDFN,PATCT,MAXPTCT SET TMGDFN=0,PATCT=0,MAXPTCT=0  ;"//kt 10/1/26
  FOR  SET TMGDFN=$ORDER(^DPT(TMGDFN)) QUIT:TMGDFN'>0  DO
  . IF $DATA(^TMG("TMP","REBUILDALL^TMGTOPIC",TMGDFN)) QUIT
  . SET MAXPTCT=MAXPTCT+1
  SET TMGDFN=0
  FOR  SET TMGDFN=$ORDER(^DPT(TMGDFN)) QUIT:TMGDFN'>0  DO  ;"//kt 10/1/26
  . NEW PTNAME SET PTNAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)  ;"//kt 10/1/26
  . IF $DATA(^TMG("TMP","REBUILDALL^TMGTOPIC",TMGDFN)) DO  QUIT  ;"//kt 10/1/26 Resume: do not rebuild patients already completed
  . . WRITE "Already processed "_PTNAME,!
  . SET PATCT=PATCT+1  
  . DO PROGBAR^TMGUSRI2(PATCT,"Rebuilding "_PTNAME,1,MAXPTCT,60,STARTTIME)  ;"//kt 10/1/26 Update for every patient
  . WRITE !
  . DO REBUILD1PT(TMGDFN,.OPTION)  ;"//kt 10/1/26 OPTION has no SILENT flag, so note progress is shown
  . SET ^TMG("TMP","REBUILDALL^TMGTOPIC",TMGDFN)=""
  DO PROGBAR^TMGUSRI2(MAXDFN,"Topic data rebuild complete.",MAXDFN,MAXDFN,60,STARTTIME)  ;"//kt 10/1/26
  QUIT
  ;
  ;"========================================================================
  ;" END OF CODE FOR REBUILDING TOPICS DATA
  ;"========================================================================
