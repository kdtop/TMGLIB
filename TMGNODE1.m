TMGNODE1 ;TMG/kst/HTML Code for working with node.js;5/25/25
	;;1.0;TMG-LIB;**1**;05/25/25
	;
	;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
	;"Copyright (c) 5/25/25  Kevin S. Toppenberg MD
	;"
	;"This file is part of the TMG LIBRARY, and may only be used in accordence
	;" to license terms outlined in separate file TMGLICNS.m, which should
	;" always be distributed with this file.;
	;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
	;
NODETEST(A,B) ;
	NEW RESULT SET RESULT="HELLO! NODETEST^TMGNODE() got params: A="_$GET(A)_", and B="_$GET(B)
	QUIT RESULT
	;
	;"================================================================================"
	;"================================================================================"
	;
NODEAPI(CALLTAG,CALLRTN,JSONARGS) ;" Universal Node.js RPC Dispatcher
	;"
	;" This routine acts as an intermediary for Node.js RPC calls.;
	;" It decodes the single JSON argument containing all arguments,
	;" dynamically calls the target Mumps function (all args by reference),
	;" and re-encodes the final state of arguments back to Node.js.;
	;"
	;" Parameters:
	;"   CALLTAG:  The tag name of the Mumps function to call (e.g., "MYFN")
	;"   CALLRTN:  The routine name where the Mumps function is defined (e.g., "MYROUTINE")
	;"   JSONARGS: A single string from Node.js, which is a JSON string
	;"             representing an array of argument objects ({value, type}).;
	;"
	;"NOTE!!: This is not the same as the VistA RPC system.  Also, there is no
	;"        built-in security.  So the caller (node.js) is considered
	;"        (and must be) trusted!
	;
	NEW REF SET REF=$NAME(^TMG("TMP","NODEAPI^TMGNODE1"))
	NEW ZZDEBUG SET ZZDEBUG=0
	IF $GET(ZZSESSIONFLAG)=1 SET ZZDEBUG=1
	IF ZZDEBUG=0 DO
	. KILL @REF
	. SET @REF@("CALLTAG")=CALLTAG
	. SET @REF@("CALLRTN")=CALLRTN
	. SET @REF@("JSONARGS")=JSONARGS
	ELSE  DO
	. SET CALLTAG=@REF@("CALLTAG")
	. SET CALLRTN=@REF@("CALLRTN")
	. SET JSONARGS=@REF@("JSONARGS")
	. ;
	;
	NEW TMGRESULT SET TMGRESULT="{}"
	;
	NEW CALLFN,CODE,ARGSARR,TMGERR
	NEW %TMGAPICALLRESULT ;" To store the scalar result of the called function
	NEW $ETRAP,$ESTACK SET $ETRAP="GOTO CATCHERR^TMGNODE1"
	;"
	;" 1. Construct the full Mumps function reference
	SET CALLFN=CALLTAG_"^"_CALLRTN
	;"
	;" 2. Decode the incoming JSONARGS string into a Mumps array (ARGSARR)
	;"    This array holds metadata for each argument (value, type).;
	;"
	;"NOTICE: If a parameter is to be an OUT parameter (i.e. it was passed empty)
	;"        Then the RPC should first KILL the variable. This is because the JSON
	;"        parser puts in a value of "[]" for empty arrays, and if this gets passed
	;"        back to node.js, it breaks proper handling.;
	NEW TMPERR
	DO JSON2ARR^TMGJSON(JSONARGS,"ARGSARR",,.TMPERR)  ;"Parse JSON string into MUMPS array, storing in ARGSARR
	IF $DATA(TMPERR) DO
	. SET TMGERR("MESSAGE")="Failed to decode initial JSON arguments: "_$$ARR2STR^TMGSTUT2(.TMGERR," // ")
	. SET TMGERR("CODE")="JSON_DECODE_ERROR"
	IF $DATA(TMGERR) DO  GOTO NODEDN
	. NEW ERROBJ SET ERROBJ("errorCode")=$GET(TMGERR("CODE"))
	. SET ERROBJ("errorMessage")=$GET(TMGERR("MESSAGE"))
	. ;" Encode this error object to JSON
	. SET TMGRESULT=$$ARR2JSON^TMGJSON("ERROBJ")
	;"
	MERGE @REF@("JSONARGS-DECODED")=ARGSARR
	;
	;" 3. Prepare arguments for the dynamic call
	;"    Iterate through the top-level subscripts of ARGSARR (representing each original arg)
	;"    We need to unpack each argument's 'value' based on its 'type' and prepare it for the call.;
	SET CODE="SET %TMGAPICALLRESULT=$$"_CALLFN_"("
	NEW PARAMSTR SET PARAMSTR=""
	NEW IDX SET IDX=""
	FOR  SET IDX=$ORDER(ARGSARR(IDX)) QUIT:IDX=""  DO
	. NEW AVALUE,ATYPE,AVARNAME
	. ;" Get the metadata for the current argument
	. SET AVALUE=$GET(ARGSARR(IDX,"value"))
	. SET ATYPE=$GET(ARGSARR(IDX,"type"))
	. SET AVARNAME="%TMGVAR"_IDX  ;"<-- this variable will be put into variable table.  Will kill later below ;
	. IF (ATYPE="json_object")!(ATYPE="json_array") DO
	. . MERGE @AVARNAME=ARGSARR(IDX,"value")
	. ;"IF (ATYPE="json_object")!(ATYPE="json_array") DO
	. ;". ;" For JSON types, decode the stringified value into a separate Mumps local array
	. ;". ;" The called function will receive a reference to this new Mumps array.;
	. ;". NEW DECODED,TMPERR
	. ;". NEW %TMPSTR
	. ;". SET %TMPSTR(1)=AVALUE
	. ;". DO JSON2ARR^TMGJSON(AVALUE,"DECODED",,.TMPERR)
	. ;". IF $DATA(TMPERR) DO
	. ;". . ;" Failed to decode nested JSON, pass as original string.;
	. ;". . SET @AVARNAME=""""_AVALUE_""""
	. ;". ELSE  DO
	. ;". . ;" Successfully decoded, pass by reference.;
	. ;". . ;" Store the name of the new Mumps array for later re-encoding.;
	. ;". . MERGE @AVARNAME=DECODED
	. ELSE  DO
	. . ;" For scalar types (string, number), pass by reference directly
	. . ;" The actual Mumps variable will be created/used within the XECUTE context.;
	. . ;" We'll create a local variable name based on its index.;
	. . SET @AVARNAME=AVALUE
	. IF PARAMSTR'="" SET PARAMSTR=PARAMSTR_","
	. SET PARAMSTR=PARAMSTR_"."_AVARNAME
	;" Close the command string
	SET CODE=CODE_PARAMSTR_")"
	;"
	;" 4. Execute the constructed Mumps function call
	XECUTE CODE
	;"
	;" 5. Collect final argument values and format the result as JSON
	NEW TMGOUTPUTARR ; <--this will hold the array results
	SET IDX=""
	FOR  SET IDX=$ORDER(ARGSARR(IDX)) QUIT:IDX=""  DO
	. NEW AVALUE,ATYPE,AVARNAME
	. SET AVARNAME="%TMGVAR"_IDX
	. SET ATYPE=$GET(ARGSARR(IDX,"type"))
	. MERGE AVALUE=@AVARNAME
	. KILL @AVARNAME ; Clean up temp variables
	. IF $GET(AVALUE)["%%empty_",($DATA(AVALUE)\10=1) DO    ;"not empty any more
	. . NEW TEMP DO MERGESN^TMGMISC(.AVALUE,.TEMP) KILL AVALUE
	. . DO MERGESN^TMGMISC(.TEMP,.AVALUE)    ;"Kill off value of AVALUE, but keep its subnodes
	. IF $DATA(AVALUE)=0 DO
	. . IF ATYPE="json_object" SET AVALUE="%%empty_obj%%"
	. . IF ATYPE="json_array" SET AVALUE="%%empty_array%%"
	. MERGE TMGOUTPUTARR(IDX)=AVALUE ; <-- Changed to use numeric subscript for array
	;"
	;" Create a final result object to return to Node.js
	NEW RETURNOBJ
	MERGE RETURNOBJ("args")=TMGOUTPUTARR ; Put the arguments into an 'args' key
	SET RETURNOBJ("return")=$GET(%TMGAPICALLRESULT) ; Add the scalar return value
	;"
	;" Encode the entire final result object into a JSON string to return to Node.js
	SET TMGRESULT=$$ARR2JSON^TMGJSON("RETURNOBJ") ; Encode the new structure
NODEDN ;
	QUIT TMGRESULT ;" Return the final JSON string
	;"
	;"
CATCHERR ;" Generic error trap handler for errors during XECUTE or called function
	NEW TMGERRMESSAGE SET TMGERRMESSAGE=$ZERROR
	SET $ETRAP="" ;" Disable error trap to prevent infinite loops
	;" Return a structured error string that Node.js can parse
	NEW ERROBJ
	SET ERROBJ("errorCode")="RUNTIME_ERROR"
	SET ERROBJ("errorMessage")="Mumps Runtime Error: "_TMGERRMESSAGE
	ZSHOW "S":ERROBJ("errorStack") ;" Capture stack trace for debugging
	SET TMGRESULT=$$ARR2JSON^TMGJSON("ERROBJ")
	QUIT TMGRESULT
	;
	;"================================================================================"
	;" Utility functions.;
	;"================================================================================"
	;
GETSESSION(DFN)  ;"Return session ID for DFN patient, making new if not already present
	;"Input: DFN -- patient internal entry number
	;"Result: 1^SessionID  -- if session already existed,  2^SessionID -- if new session was created, default -1
	NEW TMP SET TMP=$$HASSESSION(DFN)
	NEW RESULT SET RESULT=-1
	IF TMP>0 DO
	. SET RESULT="1^"_TMP
	ELSE  DO
	. SET RESULT="2^"_$$NEWSESSION(DFN)
	QUIT RESULT
	;
PREFIX()  ;
	QUIT "TMG_NODE_SESSION-"
	;
SESSIONNM(DFN)  ;"This will return the name of the session (so it is managed in one place)
	QUIT $$PREFIX_DFN
	;"
SESSIONTM()  ;"This will return the session timeout (so logic can be updated easily)
	QUIT $$FMDTNOW^TMGDATE+.01  ;"1 HOUR
	;"
GETRNDID()  ;  ;"Return a 10 digit number, not including 0 value (0 OK as a digit)
	NEW CODE SET CODE=""
	FOR I=1:1:10 DO
	. SET CODE=CODE_$RANDOM(10)
	IF +CODE=0 SET CODE=$$GETRNDID()
	QUIT CODE
	;
ID2DATAREF(SESSIONID) ;"Return reference to data for storage.;
	NEW REF SET REF=""
	NEW TMGDFN SET TMGDFN=$$SESSION2DFN^TMGNODE1(SESSIONID)
	IF TMGDFN>0 DO
	. SET REF=$NAME(^XTMP($$SESSIONNM(TMGDFN),"DATA"))
	QUIT REF
	;
NEWSESSION(DFN)  ;"Create a new session ID
	NEW ID SET ID=$$GETRNDID()
	;"SAVE TO ^XTMP("TMG_NODE_SESSION"_DFN,0), with timeout period being NOW + TIMEOUT
	;"See rules here: https://www.va.gov/vdl/documents/Infrastructure/Kernel/xtmp_rules.pdf
	SET ^XTMP($$SESSIONNM(DFN),0)=$$SESSIONTM_"^"_$$FMDTNOW^TMGDATE_"^"_ID
	QUIT ID
	;
HASSESSION(DFN)  ;"Return prexisting session ID (if any) or 0 if not present
	;"NOTE: Every session will have a timeout period, and only valid if not timed out
	NEW RESULT SET RESULT=0
	NEW SESSIONLBL SET SESSIONLBL=$$SESSIONNM(DFN)
	IF $DATA(^XTMP(SESSIONLBL,0)) DO
	. NEW ZN SET ZN=$GET(^XTMP(SESSIONLBL,0))
	. NEW PURGEDT SET PURGEDT=$PIECE(ZN,"^",1)
	. IF PURGEDT>$$FMDTNOW^TMGDATE DO  ;"WITHIN PROPER TIMEFRAME
	. . SET RESULT=$PIECE(ZN,"^",3)  ;"PIECE 3 SHOULD BE SESSION ID
	. . SET $PIECE(^XTMP(SESSIONLBL,0),"^",1)=$$SESSIONTM  ;"RESET TIMER FOR 1 HOUR
	. ELSE  DO
	. . KILL ^XTMP(SESSIONLBL,0)  ;"SESSION IS INVALID. KILL OFF SESSION
	QUIT RESULT
	;
SESSION2DFN(SESSIONID)  ;"Get DFN that matches sessionID
	SET SESSIONID=$GET(SESSIONID)
	NEW RESULT SET RESULT=0
	NEW PRE SET PRE=$$PREFIX()
	NEW DONE SET DONE=0
	NEW IDX SET IDX=PRE_"0"
	FOR  SET IDX=$ORDER(^XTMP(IDX)) QUIT:(IDX'[PRE)!DONE  DO
	. NEW ASESSION SET ASESSION=$PIECE($GET(^XTMP(IDX,0)),"^",3)
	. IF (ASESSION'>0)!(ASESSION'=SESSIONID) QUIT
	. SET RESULT=$PIECE(IDX,PRE,2)
	. IF RESULT>0 SET DONE=1
	QUIT RESULT
	;  ;
	;"================================================================================"
	;"================================================================================"
	;
	;