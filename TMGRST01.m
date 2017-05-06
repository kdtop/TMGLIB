TMGRST01 ;TMG/kst/REST web service; 4/21/14
       ;;1.0;TMG-LIB;**1**;4/21/14
       ;
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
 ;"===NOTES =============================
 ;"old->Turn on web listener via DO JOB^VPRJREQ(PORTNUMBER)
 ;"old->  (Client side code has been using port 9080)
 ;"old->Turn off web listener via DO STOP^VRPJREQ
 ;"Data that connects particular REST service is 
 ;"  via FILEMAN FILE [WEB SERVICE URL HANDLER]
 ;"Client side code has been using port 9080
 ;"ADDENDUM 6/30/14, now connection is via xinetd on port 9080
 ;"
 ;"REST services that are:
 ;"  GET must have entry point as follows
 ;"    <Atag>(RESULT,ARG) ;i.e. takes 2 parameters.  Names could vary
 ;"      RESULT is an OUT parameter.
 ;"  can be a global ref.  e.g. "^TMP($J,123)"
 ;"  can be scalar (simple) value
 ;"  can be a local array e.g. RESULT(#)=<VALUE> (single dimension)
 ;"      ARG -- an array with calling parameters. 
 ;"  POST
 ;"    <Atag>(ARGS,BODY,RESULT) ;i.e. takes 3 parameters.  Names could vary
 ;
 ;"To call RPC, use RESTclient in firefox
 ;"URL = http://localhost:9080/rpc/ORWU%20NEWPERS
 ;"BODY = ["S", "-1"]
 ;"Result come back in Respons body {raw}
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"GET routines
 ;"=======================================================================
 ;
ENQ(RESULT,ARG) ;"A simple ENQ->ACK call.
  ;"Input: RESULT -- PASS BY REFERNCE.  AN OUT PARAMETER
  ;"       ARG -- ignored. 
  SET RESULT="ack"
  QUIT
  ;
HELLOWD(RESULT,ARG) ;
  ;"Purpose: web serice entry point.  
  ;"Input: RESULT -- PASS BY REFERNCE.  AN OUT PARAMETER
  ;"S RESULT("mime")="application/json"
  ;"SET RESULT(1)="HELLO WORLD!"
  ;"SET RESULT(2)="And here is more info"
  ;"SET RESULT(3)="last one..."
  DO DEBUG
  KILL ^TMP($J)
  SET ^TMP($J,"RESULT",1)="HELLO WORLD, Kevin!"
  SET ^TMP($J,"RESULT",2)="And here is more info"
  SET ^TMP($J,"RESULT",3)="last one..."
  SET RESULT("mime")="application/json"
  SET RESULT=$$REF2CSTR^TMGRSTU1($NAME(^TMP($J,"RESULT")))
  QUIT
  ;
 ;"=======================================================================
 ;"POST routines
 ;"=======================================================================
 ;
SESSION(ARG,BODY,RESULT) ;"Request or release SessionID
  ;"Input: ARG -- web query args (I think)
  ;"       BODY -- This is the text passed by the request
  ;"         BODY(1) should be the command:
  ;"           'NEW', or
  ;"           'RELEASE^<sessionID>'
  ;"       RESULT -- PASS BY REFERNCE.  AN OUT PARAMETER
  ;"Output: RESULT is filled as follows:
  ;"  If request was NEW, then
  ;"    RESULT='OK^1K4KK2AQ3K5W4K6KJLK9'  <-- example session ID
  ;"          Note: Session ID key will never include '^' character
  ;"  If request was 'RELEASE', then
  ;"    RESULT='OK', OR 'ERROR^<MESSAGE>'
  ;"Result: 'OK' regardless
  DO DEBUG
  DO DBGMSG("Starting Session handler")
  NEW MODE SET MODE=$PIECE($GET(BODY(1)),"^",1)
  IF MODE="NEW" DO
  . SET RESULT="OK^"_$$MAKESN^TMGRSTU2()
  . SET ^TMG("TMP","WEB","DEBUG",1,"RESULT")=RESULT
  ELSE  IF MODE="RELEASE" DO
  . NEW SESSIONID SET SESSIONID=$PIECE(BODY(1),"^",2)
  . IF SESSIONID'="" DO
  . . NEW HASHID SET HASHID=$$EN^XUSHSH(SESSIONID)
  . . SET RESULT=$$FREESN^TMGRSTU2(HASHID)
  . . DO DBGMSG("RESULT="_RESULT)
  . . IF RESULT=1 SET RESULT="OK"
  . . ELSE  SET RESULT="ERROR^"_$PIECE(RESULT,"^",2)
  . . DO DBGMSG("RESULT="_RESULT)
  . ELSE  DO
  . . SET RESULT="ERROR^No session ID provided" 
  ELSE  DO
  . SET RESULT="ERROR^No session command found"
  DO DBGMSG("RESULT="_RESULT)
  QUIT "OK"
  ;
RPC(ARGS,BODY,TMGOUTRESULT) ;
  ;"Purpose: This is the main entry point for the RPC broker, built on 
  ;"         Sam Habiel's REST server.  Thanks to Sam for his help.
  ;"Input: ARGS -- web query args
  ;"         ARGS("id")=session ID
  ;"       BODY -- This is the text passed by the request
  ;"         BODY(1) should be the coded string of the variable input
  ;"             After string is decoded, here is expected input format:
  ;"             PARAM("RPC NAME")=Name of RPC to call
  ;"             PARAM("RPC CONTEXT")=Name of RPC CONTEXT to use
  ;"             PARAM(1)=<text of 1st parameter>
  ;"             PARAM(2)=<text of 2nd parameter>
  ;"             PARAM(<name>)=value 
  ;"       TMGOUTRESULT -- PASSED BY REFERENCE, an OUT PARAMETER. See below
  ;"          NOTE: TMGOUTRESULT can be an array with 1 index node.
  ;"Result: 'EXITING RPC' regardless of error state
  ;"Output: Variable TMGOUTRESULT is filled as follows:
  ;"        SET TMGOUTRESULT=encoded string with all results from RPC call  <-- if OK
  ;"           encoded string will include this variable. 
  ;"             TMGOUTRESULT("__RPC_Result_Type__")=<result type>, e.g. "SINGLE VALUE"
  ;"        SET TMGOUTRESULT("mime")="application/json"
  ;"
  ;"--to do -- Needs to hook into LOGRPC^XWBVW(RPCNAME,DFN)
  ;"        or try to hook in further upstream.
  DO DEBUG
  DO DBGMSGCL() ;" CLEAR DEBUG MESSAGES
  DO DBGMSG("Starting RPC handler")
  NEW ZZTMGTEMPVARS,HTTPREQ,HTTPRSP,TMGRPCRESULT ;"prevent some of these vars from being on local variable table.  Was needed... 
  NEW TMGENVSAVE SET TMGENVSAVE=$NAME(^XTMP("TMG ENV TEMP",$J)) ;"//can't use ^TMP, at least one RPC cleared that.
  IF $GET(^XTMP("TMG ENV TEMP",0))="" SET ^XTMP("TMG ENV TEMP",0)="9991231^3140502"
  DO VARS2REF^TMGRSTU2(TMGENVSAVE) ;"Save current variable table to ^TMP
  DO DBGMSG("Prior environment saved to "_TMGENVSAVE)
  ;"NEW TMGRPCRESULT
  NEW SESSIONID SET SESSIONID=$GET(ARGS("id"))
  IF SESSIONID="" SET TMGRPCRESULT(0)="-1^Session ID not provided" GOTO RPCDN  ;"Error state
  DO DBGMSG("Session ID="_SESSIONID)
  NEW HASHID SET HASHID=$$EN^XUSHSH(SESSIONID)
  DO DBGMSG("HASHID="_HASHID)
  NEW CSTR SET CSTR=$GET(BODY(1))
  DO DBGMSG("CSTR="_CSTR)
  ;"Declare variables not to be killed during swap:
  NEW KEEPLIST SET KEEPLIST="SESSIONID,HASHID,CSTR,TMGENVSAVE"    
  DO DBGMSG("KEEPLIST="_KEEPLIST)
  ;"Restore session variables to system variable table, killing prior vars (except those in keep list).
  DO DBGMSG("Loading session environment")  
  NEW TMGRPCRESULT
  DO RESTORSN^TMGRSTU2(HASHID,1,.TMGRPCRESULT,KEEPLIST) ;"THE BIG VARIABLE TABLE SWAP!
  IF +TMGRPCRESULT'>0 SET TMGRPCRESULT(0)=TMGRPCRESULT SET TMGRPCRESULT="" GOTO RPCDN  ;"Error state
  NEW NOSAVE SET NOSAVE=KEEPLIST_",NOSAVE,TMGRPCRESULT,KEEPLIST,REF," 
  ;"load input parameters into TMGPARAMS 
  NEW TMGPARAMS SET NOSAVE=NOSAVE_"TMGPARAMS,"
  DO CSTR2REF^TMGRSTU1(CSTR,"TMGPARAMS")
  DO DBGMSG("TMGPARAMS loaded")  
  NEW RPCNAME SET NOSAVE=NOSAVE_"RPCNAME," 
  SET RPCNAME=$GET(TMGPARAMS("RPC NAME")) KILL TMGPARAMS("RPC NAME")
  IF RPCNAME="" SET TMGRPCRESULT(0)="-1^RPC Name not provided" GOTO RPCDN  ;"Error state
  DO DBGMSG("RPC Name="_RPCNAME)  
  NEW RPCCONTEXT SET NOSAVE=NOSAVE_"RPCCONTEXT," 
  SET RPCCONTEXT=$GET(TMGPARAMS("RPC CONTEXT")) KILL TMGPARAMS("RPC CONTEXT")
  DO DBGMSG("RPC CONTEXT="_RPCCONTEXT)  
  ;  
  ;"QUIT $$RPC^%W0(.ARGS,.BODY,.TMGOUTRESULT) ;<-- NOTE: Sam's prior code
  ;"Finally execute the actual RPC call
  NEW TMP SET NOSAVE=NOSAVE_"TMP," 
  DO DBGMSG("Calling RPC")  
  SET TMP=$$RPC^TMGRSTR1(.TMGRPCRESULT,RPCNAME,RPCCONTEXT,"TMGPARAMS") 
  DO DBGMSG("Back from Calling RPC.  TMP="_TMP)  
  IF +TMP'>0 SET TMGRPCRESULT(0)=TMP GOTO RPCDN
  ;
  DO DBGMSG("Saving session environment")  
  DO SAVESN^TMGRSTU2(HASHID,NOSAVE)  ;"Save session environment, as it might have changed in RPC call
  ;"And we're done! 
RPCDN ;
  DO REF2VARS^TMGRSTU2(TMGENVSAVE,1,"TMGRPCRESULT,TMGENVSAVE") ;"Restore initial variable table, keeping TMGRPCRESULT var
  SET TMGOUTRESULT("mime")="application/json"
  ;"SET TMGOUTRESULT=$$REF2CSTR^TMGRSTU1("TMGRPCRESULT")  ;"NOTE: sometimes TMGOUTRESULT is VERY LARGE!  Can't be saved to disk without breaking up.
  DO REF2CARR^TMGRSTU1("TMGRPCRESULT",.TMGOUTRESULT) 
  ;"DO DBGMSG("Encoded RPC Results="_TMGOUTRESULT)  
  DO DBGMSG("Encoded RPC Results:")  
  DO DBGARMSG(.TMGOUTRESULT)  
  ;"DO DBGMSG("Restoring saved environment")  
  ;"DO REF2VARS^TMGRSTU2(TMGENVSAVE,1,"TMGOUTRESULT,TMGENVSAVE") ;"Restore initial variable table, keeping TMGOUTRESULT var
  KILL @TMGENVSAVE  ;"delete environment save from ^TMP
  DO DBGMSG("Exiting RPC^TMGRST01")  
  QUIT "EXITING RPC"
  ;
 ;"=======================================================================
 ;"Utility
 ;"=======================================================================
 ;
DEBUG ;
  NEW TMGZZ SET TMGZZ=0
  IF TMGZZ=0 DO
  . KILL ^TMG("TMP","WEB","DEBUG")  ;"only keep record for most recent RPC calls
  . NEW TMGSTACK ZSHOW "S":TMGSTACK
  . MERGE ^TMG("TMP","WEB","DEBUG","STACK")=TMGSTACK
  . MERGE ^TMG("TMP","WEB","DEBUG","ARGS")=ARGS
  . MERGE ^TMG("TMP","WEB","DEBUG","BODY")=BODY
  . MERGE ^TMG("TMP","WEB","DEBUG","RESULT")=RESULT
  . SET ^TMG("TMP","WEB","DEBUG")=$H
  . ;"ZSHOW "*":^TMG("TMP","WEB","DEBUG","SYMBOLS")
  ELSE  DO
  . KILL ARGS,BODY,RESULT
  . MERGE ARGS=^TMG("TMP","WEB","DEBUG","ARGS")
  . MERGE BODY=^TMG("TMP","WEB","DEBUG","BODY")
  . MERGE RESULT=^TMG("TMP","WEB","DEBUG","RESULT")  
  QUIT
  ;
DBGMSG(MESSAGE) ;
  ;"QUIT  ;<-- REMOVE COMMENT TO TURN OFF COMMENT MESSAGES
  NEW IDX SET IDX=+$ORDER(^TMG("TMP","WEB","DEBUG","MESSAGE",""),-1)+1
  IF $LENGTH(MESSAGE)>1000 DO
  . SET MESSAGE=$EXTRACT(MESSAGE,1,1000)_"...<trimmed>"
  SET ^TMG("TMP","WEB","DEBUG","MESSAGE",IDX)=MESSAGE
  QUIT
  ;
DBGARMSG(ARRAY) ;"OUTPUT ARRAY TO DEBUG MESSAGES
  NEW TMGZZTEMP
  NEW TMGIDX SET TMGIDX=1
  DO ZWR2ARR^TMGZWR("ARRAY","TMGZZTEMP")
  NEW IDX SET IDX=""
  FOR  SET IDX=$ORDER(TMGZZTEMP(IDX)) QUIT:(IDX="")  DO
  . NEW LINE SET LINE=$GET(TMGZZTEMP(IDX))
  . DO DBGMSG(LINE)
  QUIT
  ;
DBGMSGCL() ;" CLEAR DEBUG MESSAGES
  KILL ^TMG("TMP","WEB","DEBUG","MESSAGE")  
  QUIT
  ;
