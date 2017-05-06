TMGHL7R1 ;TMG/kst-HL7 RPC code ;8/16/15
              ;;1.0;TMG-LIB;**1**;8/16/15
 ;
 ;"TMG HL7 ENGINE, RPC INTERFACE
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 8/16/2015  Kevin S. Toppenberg MD
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
 ;"
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;" 
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"
 ;"=======================================================================
 ;
CHANNEL(TMGRESULT,INPUT) ;
  ;"Purpose: This will be a general purpose channel RPC for HL7 lab processing
  ;"         Also used by FM Desktop program,
  ;"Input: TMGRESULT -- this is an OUT parameter, and it is always passed by reference
  ;"       INPUT -- this will be array of data sent from the GUI client.  Defined below:
  ;"        <Stuff will go here>
  ;"        INPUT("REQUEST")="cmd^params"  Valid values for "cmd" are:
  ;"          "LIST AVAIL HL7 ALERTS"
  ;"            params: <not used> 
  ;"          "GET ONE HL7 ALERT INFO"
  ;"            params: $H^JOBNUM (as listed from result to call to LIST AVAIL HL7 ALERTS 
  ;"          "PROCESS"
  ;"            params: $H^JOBNUM^Mode
  ;"              Mode -- 1 = Parse and transform only, don't file.  
  ;"          "SEARCH RECS"
  ;"            params: FileNum^SearchTerms^CustomizerFn
  ;"                SearchTerms <-- a list of terms to search for, separated by spaces
  ;"          "AUTOADD DATANAME"
  ;"            params: name_of_new_data_name
  ;"          "ADD LAB 60"
  ;"            params: AlertHandle^TestName^PrintName^StorageLocFld63d04
  ;"                AlertHandle = $H;JOBNUM
  ;"                TestName = Test name (up to 30 chars)
  ;"                PrintName = Shortened name (up to 7 chars)
  ;"                StorageLocFld63d04 = Field in subfile 63.04 for storage
  ;"          "NEXT AVAIL WKLD"
  ;"            params: ExistingWorkLoadCode
  ;"          "ADD WKLD"
  ;"            params: Name^Code
  ;"          "GET IEN62 FROM IEN61"  
  ;"            params: IEN61
  ;"          "LINK IEN60 TO DATANAME"
  ;"            params: AlertHandle^IEN60^Loc63D04
  ;"                AlertHandle = $H;JOBNUM
  ;"          "RESOLVE"  -- Resolve alert.  Do this only after PROCESS returns "OK"
  ;"            params: $H^JOBNUM^FMDT
  ;" 
  ;"Output: results of this function should be put into TMGRESULTS array.
  ;"        For cmd:
  ;"          "LIST AVAIL HL7 ALERTS"
  ;"            TMGRESULT(0)=1^OK, or -1^message
  ;"            TMGRESULT(#)=1^$H^JOBNUM^FMDT  <-- Can be used to get info from ^TMG("TMP","TMGHL73",JOBNUM,$H) array
  ;"          "GET ALERT INFO"
  ;"            TMGRESULT(0)=1^OK, or -1^message
  ;"            TMGRESULT(#)="<varname>=<varvalue>"
  ;"          "PROCESS"
  ;"            TMGRESULT(0)=1^OK, or -1^message
  ;"            TMGRESULT(#)=<message from server code to GUI client>
  ;"          "SEARCH RECS"
  ;"            TMGRESULT(0)=1^OK, or -1^message
  ;"            TMGRESULT(#)=IEN^FileNum^.01Name
  ;"          "AUTOADD DATANAME"
  ;"            TMGRESULT(0)=1^OK, or -1^message
  ;"            TMGRESULT(1)=NewlyAddedFieldNumber^Name
  ;"          "ADD LAB 60"  
  ;"            TMGRESULT(0)=1^OK, or -1^message
  ;"            TMGRESULT(1)=NewlyAddedIEN60^Name
  ;"          "NEXT AVAIL WKLD"
  ;"            TMGRESULT(0)=1^OK, or -1^message
  ;"            TMGRESULT(1)=NextAvailableWorkLoadCode
  ;"          "ADD WKLD"
  ;"            TMGRESULT(0)=1^OK, or -1^message
  ;"            TMGRESULT(1)=IEN^WorkloadName^WorkloadCode
  ;"          "GET IEN62 FROM IEN61"  
  ;"            TMGRESULT(0)=1^OK, or -1^message
  ;"            TMGRESULT(1)=IEN61
  ;"          "LINK IEN60 TO DATANAME"
  ;"            TMGRESULT(0)=1^OK, or -1^message
  ;"          "RESOLVE"  
  ;"            TMGRESULT(0)=1^OK, or -1^message
  ;"Result: none
  ;
  NEW TMGZZ SET TMGZZ=0 ;"Set to 1 at runtime to use stored input
  IF TMGZZ=0 DO
  . KILL ^TMG("TMP","TMGHL7R1")
  . MERGE ^TMG("TMP","TMGHL7R1","INPUT")=INPUT
  . SET ^TMG("TMP","TMGHL7R1","JOB")=$J
  ELSE  DO
  . KILL INPUT MERGE INPUT=^TMG("TMP","TMGHL7R1","INPUT")
  KILL TMGRESULT
  ;"set LIVEDEBUG to 1 at runtime to trigger live debug listener, BEFORE GUI calls RPC
  NEW LIVEDEBUG SET LIVEDEBUG=0
  ;"Below is code for console process for live debugging GUI RPC's   
  IF LIVEDEBUG=1 DO  GOTO CH2
  . KILL ^TMG("TMP","TMGHL7R1","LIVE DEBUG")
  . SET ^TMG("TMP","TMGHL7R1","LIVE DEBUG")=1
  . FOR  DO  QUIT:$DATA(INPUT)
  . . KILL INPUT MERGE INPUT=^TMG("TMP","TMGHL7R1","LIVE DEBUG","INPUT")
  . . IF $DATA(INPUT) QUIT
  . . HANG 1  ;"Wait for GUI to call CHANNEL and save INPUT info.
  ;"Below is code for process serving GUI to allow passing of debugging to
  ;"  a live console process, and getting results from there.
  IF $GET(^TMG("TMP","TMGHL7R1","LIVE DEBUG"))=1 DO  GOTO CHDN  
  . KILL ^TMG("TMP","TMGHL7R1","LIVE DEBUG","RESULT"),TMGRESULT
  . MERGE ^TMG("TMP","TMGHL7R1","LIVE DEBUG","INPUT")=INPUT
  . FOR  DO  QUIT:$GET(^TMG("TMP","TMGHL7R1","LIVE DEBUG","RESULT READY"))=1
  . . HANG 1  ;"Wait for results from other process 
  . MERGE TMGRESULT=^TMG("TMP","TMGHL7R1","LIVE DEBUG","RESULT")
  . KILL ^TMG("TMP","TMGHL7R1","LIVE DEBUG")
CH2 ;  
  NEW TMGCOMMAND,TMGPARAMS
  SET TMGCOMMAND=$$TRIM^XLFSTR($$UP^XLFSTR($PIECE($GET(INPUT("REQUEST")),"^",1)))
  SET TMGPARAMS=$$UP^XLFSTR($PIECE($GET(INPUT("REQUEST")),"^",2,199))
  NEW TMGORGPARAMS SET TMGORGPARAMS=$PIECE($GET(INPUT("REQUEST")),"^",2,199)
  SET TMGRESULT(0)="-1^No command requested. Got: '"_$GET(INPUT("REQUEST"))_"'"  ;"default to error state.
  IF TMGCOMMAND="LIST AVAIL HL7 ALERTS" DO  GOTO CHDN
  . DO LSTALRTS^TMGHL7R2(.TMGRESULT,TMGPARAMS)
  ELSE  IF TMGCOMMAND="GET ONE HL7 ALERT INFO" DO  GOTO CHDN
  . DO GET1ALRT^TMGHL7R2(.TMGRESULT,TMGPARAMS)  
  ELSE  IF TMGCOMMAND="SEARCH RECS" DO  GOTO CHDN
  . DO SRCHRECS^TMGHL7R2(.TMGRESULT,TMGPARAMS)  
  ELSE  IF TMGCOMMAND="PROCESS" DO  GOTO CHDN
  . NEW NODE SET NODE="" FOR  SET NODE=$ORDER(INPUT(NODE)) QUIT:NODE=""  DO
  . . IF NODE'["GUI;MSG;" QUIT
  . . NEW IDX SET IDX=+$PIECE(NODE,";",3)
  . . NEW ISREPLY SET ISREPLY=($PIECE(NODE,";",4)="REPLY")
  . . NEW STR SET STR=$GET(INPUT(NODE))
  . . IF ISREPLY SET TMGPARAMS("GUI","MSG",IDX,"REPLY")=STR
  . . ELSE  SET TMGPARAMS("GUI","MSG",IDX)=STR
  . DO PROCESS^TMGHL7R2(.TMGRESULT,.TMGPARAMS)
  ELSE  IF TMGCOMMAND="RESOLVE" DO  GOTO CHDN
  . NEW FMDT SET FMDT=$PIECE(TMGPARAMS,"^",3)
  . NEW SKIPFILE SET SKIPFILE=0
  . SET $PIECE(TMGPARAMS,"^",3)=SKIPFILE  
  . DO PROCESS^TMGHL7R2(.TMGRESULT,.TMGPARAMS)
  . IF $GET(TMGRESULT(0))'="1^OK" QUIT
  . SET $PIECE(TMGPARAMS,"^",3)=FMDT
  . DO DELALRT^TMGHL7R2(.TMGRESULT,.TMGPARAMS)
  ELSE  IF TMGCOMMAND="AUTOADD DATANAME" DO  GOTO CHDN
  . DO ADDDN^TMGHL7R2(.TMGRESULT,TMGPARAMS)
  ELSE  IF TMGCOMMAND="ADD LAB 60" DO  GOTO CHDN
  . DO ADDLAB60^TMGHL7R2(.TMGRESULT,TMGPARAMS)
  ELSE  IF TMGCOMMAND="NEXT AVAIL WKLD" DO  GOTO CHDN
  . DO NEXTWKLD^TMGHL7R2(.TMGRESULT,TMGPARAMS)
  ELSE  IF TMGCOMMAND="ADD WKLD" DO  GOTO CHDN
  . DO ADDWKLD^TMGHL7R2(.TMGRESULT,TMGPARAMS)    
  ELSE  IF TMGCOMMAND="GET IEN62 FROM IEN61" DO  GOTO CHDN
  . DO GET62F61^TMGHL7R2(.TMGRESULT,TMGPARAMS)   
  ELSE  IF TMGCOMMAND="LINK IEN60 TO DATANAME" DO  GOTO CHDN
  . DO LK60TODN^TMGHL7R2(.TMGRESULT,TMGPARAMS)   
  ;  
CHDN ;
  MERGE ^TMG("TMP","TMGHL7R1","RESULT")=TMGRESULT
  IF LIVEDEBUG DO
  . MERGE ^TMG("TMP","TMGHL7R1","LIVE DEBUG","RESULT")=TMGRESULT
  . SET ^TMG("TMP","TMGHL7R1","LIVE DEBUG","RESULT READY")=1
  . NEW TURNOFF SET TURNOFF=1  ;"can change at runtime to leave mode on.
  . IF TURNOFF=1 SET ^TMG("TMP","TMGHL7R1","LIVE DEBUG")=0
  QUIT
  ;
