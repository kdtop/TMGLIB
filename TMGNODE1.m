TMGNODE1 ;TMG/kst/HTML Code for working with node.js;5/25/25
          ;;1.0;TMG-LIB;**1**;05/25/25
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 5/25/25  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
NODEAPI(CALLTAG,CALLRTN,JSONARGS) ;" Universal Node.js RPC Dispatcher (Revised)
  ;"
  ;" This routine acts as an intermediary for Node.js RPC calls.
  ;" It decodes the single JSON argument containing all arguments,
  ;" dynamically calls the target Mumps function (all args by reference),
  ;" and re-encodes the final state of arguments back to Node.js.
  ;"
  ;" Parameters:
  ;"   CALLTAG:  The tag name of the Mumps function to call (e.g., "MYFN")
  ;"   CALLRTN:  The routine name where the Mumps function is defined (e.g., "MYROUTINE")
  ;"   JSONARGS: A single string from Node.js, which is a JSON string
  ;"             representing an array of argument objects ({value, type}).
  ;"            
  ;"
  NEW REF SET REF=$NAME(^TMG("TMP","NODEAPI^TMGNODE1"))
  NEW ZZDEBUG SET ZZDEBUG=0
  IF ZZDEBUG=0 DO
  . KILL @REF
  . SET @REF@("CALLTAG")=CALLTAG
  . SET @REF@("CALLRTN")=CALLRTN
  . SET @REF@("JSONARGS")=JSONARGS
  ELSE  DO
  . SET CALLTAG=@REF@("CALLTAG")
  . SET CALLRTN=@REF@("CALLRTN")
  . SET JSONARGS=@REF@("JSONARGS")
  .   
  ;
  NEW TMGRESULT SET TMGRESULT=""
  NEW CALLFN,CODE,ARGSARR,TMGERR
  NEW %TMGAPICALLRESULT ;" To store the scalar result of the called function
  NEW $ETRAP,$ESTACK SET $ETRAP="GOTO CATCHERR^TMGNODE1"
  ;"
  ;" 1. Construct the full Mumps function reference
  SET CALLFN=CALLTAG_"^"_CALLRTN
  ;"
  ;" 2. Decode the incoming JSONARGS string into a Mumps array (ARGSARR)
  ;"    This array holds metadata for each argument (value, type).
  DO
  . KILL TMPERR
  . NEW TMGSTR SET TMGSTR(1)=JSONARGS  ;"<--- serialized version of json arguments
  . DO DECODE^XLFJSON("TMGSTR","ARGSARR","TMPERR")
  . IF $DATA(TMPERR) DO
  . . SET TMGERR("MESSAGE")="Failed to decode initial JSON arguments: "_$GET(TMGERR("text"))
  . . SET TMGERR("CODE")="JSON_DECODE_ERROR"
  IF $DATA(TMGERR) DO  GOTO NODEDN
  . NEW ERROBJ
  . SET ERROBJ("errorCode")=$GET(TMGERR("CODE"))
  . SET ERROBJ("errorMessage")=$GET(TMGERR("MESSAGE"))
  . ;" Encode this error object to JSON
  . NEW ENCODED,TMPERR
  . DO ENCODE^XLFJSON("ERROBJ","ENCODED","TMPERR")
  . IF $DATA(TMPERR) SET TMGRESULT="FATAL ERROR: Could not encode error message!"
  . ELSE  SET TMGRESULT=$GET(ENCODED(1)) ;" Return JSON error string      
  ;"
  MERGE @REF@("JSONARGS-DECODED")=ARGSARR
  ;
  ;" 3. Prepare arguments for the dynamic call
  ;"    Iterate through the top-level subscripts of ARGSARR (representing each original arg)
  ;"    We need to unpack each argument's 'value' based on its 'type' and prepare it for the call.
  SET CODE="SET %TMGAPICALLRESULT=$$"_CALLFN_"("
  NEW PARAMSTR SET PARAMSTR=""
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(ARGSARR(IDX)) QUIT:IDX=""  DO
  . NEW AVALUE,ATYPE,AVARNAME
  . ;" Get the metadata for the current argument
  . SET AVALUE=$GET(ARGSARR(IDX,"value"))
  . SET ATYPE=$GET(ARGSARR(IDX,"type"))
  . SET AVARNAME="%TMGVAR"_IDX  ;"<-- this variable will be put into variable table.  Must kill later. 
  . IF (ATYPE="json_object")!(ATYPE="json_array") DO
  . . ;" For JSON types, decode the stringified value into a separate Mumps local array
  . . ;" The called function will receive a reference to this new Mumps array.
  . . NEW DECODED,TMPERR
  . . NEW %TMPSTR
  . . SET %TMPSTR(1)=AVALUE
  . . DO DECODE^XLFJSON("%TMPSTR","DECODED","TMPERR")
  . . IF $DATA(TMPERR) DO
  . . . ;" Failed to decode nested JSON, pass as original string.
  . . . SET @AVARNAME=""""_AVALUE_""""
  . . ELSE  DO
  . . . ;" Successfully decoded, pass by reference.
  . . . ;" Store the name of the new Mumps array for later re-encoding.
  . . . MERGE @AVARNAME=DECODED
  . ELSE  DO
  . . ;" For scalar types (string, number), pass by reference directly
  . . ;" The actual Mumps variable will be created/used within the XECUTE context.
  . . ;" We'll create a local variable name based on its index.
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
  NEW TMGOUTPUT
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(ARGSARR(IDX)) QUIT:IDX=""  DO
  . NEW AVALUE,ATYPE,AVARNAME
  . SET AVARNAME="%TMGVAR"_IDX 
  . SET ATYPE=$GET(ARGSARR(IDX,"type"))
  . MERGE AVALUE=@AVARNAME 
  . KILL @AVARNAME
  . IF $DATA(AVALUE)>1 DO  ;"i.e. has subnodes
  . . ;" Re-encode the Mumps array back to JSON string
  . . KILL TMPENCODE,TMPERR
  . . ;" Copy the actual Mumps array's content into a temporary variable
  . . ;" This ensures we get the latest state after the called RPC.
  . . DO ENCODE^XLFJSON("AVALUE","TMPENCODE","TMPERR")
  . . IF '$DATA(TMPERR) DO
  . . . SET AVALUE=$GET(TMPENCODE(1))  ;"stringified version of array
  . . ELSE  DO
  . . . ;" Failed to re-encode, return an error string
  . . . SET AVALUE="ERROR_RE-ENCODING_JSON: "_$GET(TMPERR("text"))
  . ;" Store the final value in the result array, keyed by arg position (e.g., arg1, arg2 ... )
  . SET TMGOUTPUT("arg"_IDX)=AVALUE
  ;"
  ;" Add the scalar return value of the called function to the result, if any
  SET TMGOUTPUT("return")=$GET(%TMGAPICALLRESULT)
  ;"
  ;" Encode the entire final result object into a JSON string to return to Node.js
  DO 
  . NEW TMPENCODE,TMPERR
  . DO ENCODE^XLFJSON("TMGOUTPUT","TMPENCODE","TMPERR")
  . IF $DATA(TMPERR) DO
  . . ;" Handle error if final result cannot be encoded to JSON
  . . SET TMGRESULT="ERROR: Failed to encode final result to JSON: "_$GET(TMPERR("text"))
  . ELSE  SET TMGRESULT=TMPENCODE(1)
  ;"
NODEDN ;  
  QUIT TMGRESULT ;" Return the final JSON string
  ;"
CATCHERR ;" Generic error trap handler for errors during XECUTE or called function
  NEW TMGERRMESSAGE SET TMGERRMESSAGE=$ZERROR
  SET $ETRAP="" ;" Disable error trap to prevent infinite loops
  ;" Return a structured error string that Node.js can parse
  NEW ERROBJ
  SET ERROBJ("errorCode")="RUNTIME_ERROR"
  SET ERROBJ("errorMessage")="Mumps Runtime Error: "_TMGERRMESSAGE
  ZSHOW "S":ERROBJ("errorStack") ;" Capture stack trace for debugging
  NEW ENCODED,TMPERR
  DO ENCODE^XLFJSON("ERROBJ","ENCODED","TMPERR")
  IF $DATA(TMPERR) QUIT "FATAL ERROR: Could not encode runtime error!"
  QUIT $GET(ENCODED(1))
  ;
FIX4JSON(ARRREF) ;
  ;
  NEW ORIGLAST SET ORIGLAST=$QSUBSCRIPT(NAME,$QLENGTH(NAME))
  NEW ORIGQL SET ORIGQL=$QLENGTH(NAME)
  IF $DATA(@NAME)#2 DO
  . 
  FOR  SET NAME=$QUERY(@NAME) Q:(NAME="")!($QSUBSCRIPT(NAME,ORIGQL)'=ORIGLAST)  DO
  . NEW DV SET DV=$DATA(@NAME)
  . ;"1  = node has value, but no subnode(s)
  . ;"11 = node has value AND subnode(s)
  . ;"10 = node has no value, but HAS subnode(s)
  . ;"SET @OUTREF@(IDX)=NAME_"="_$$FORMAT(@NAME)
  . SET IDX=IDX+1
  QUIT   
  
ARR2JSON(REF,GETSIBLINGS,TOPVAL,PRIOR) ;"Stringify array into JSON format (serialized into long string)
  ;"PURPOSE: REPLACE functionality of ENCODE^XLFJSON.
  ;"INPUT:  REF -- NAME of array to process
  ;"        GETSIBLINGS.  Optional.  Default is 0.  If 1 then siblings of REF are added
  ;"        TOPVAL -- Optional.  If present, inserted as a value of the top level node in REF
  ;"        PRIOR -- PASS BY REFERENCE.  Used when calling self recursively, to 
  ;"                 avoid endless loops as can happen with skipped nodes in array.  
  ;"EXAMPLE showing issues with ENCODE^XLFJSON.
  ;"  ZWR Y
  ;"  Y(0)=0
  ;"  Y(1)=1
  ;"  Y(1,2,3)=0
  ;"  Y(2)=2
  ;"  
  ;"  DO ENCODE^XLFJSON("Y","ZZOUT") ZWR ZZOUT
  ;"  ZZOUT(1)="{""0"":0,""1"":1,""2"":2}"  <-- missing data, see below
  ;"  
  ;"  DO DECODE^XLFJSON("ZZOUT","TMP")
  ;"  
  ;"  yottadb>ZWR TMP
  ;"  TMP("""0")=0    <--- notice added "
  ;"  TMP("""1")=1    <--- notice that node 1,2,3=0 is missing.  
  ;"  TMP("""2")=2
  ;
  ;"Expected json
  ;"{'0':0,
  ;" '1':{
  ;"      '%%node_value%%':1,
  ;"      '2':{
  ;"           '3':0
  ;"           },
  ;"     },
  ;" '2':2
  ;"}
  NEW TOPLEVEL SET TOPLEVEL=(REF'["(")
  NEW RESULT SET RESULT=$SELECT(TOPLEVEL:"",1:"{")
  SET GETSIBLINGS=$GET(GETSIBLINGS,0)
  SET TOPVAL=$GET(TOPVAL)
  NEW ORIGREF SET ORIGREF=REF
  NEW ORIGLAST SET ORIGLAST=$QSUBSCRIPT(REF,$QLENGTH(REF))
  NEW ORIGQL SET ORIGQL=$QLENGTH(REF)
  NEW QT SET QT="'"
  NEW DONE SET DONE=0
  FOR  DO  QUIT:DONE 
  . SET PRIOR(REF)=1
  . IF TOPVAL'="" DO
  . . SET RESULT=RESULT_QT_"%%node_value%%"_QT_":"_TOPVAL_","
  . . SET TOPVAL=""
  . NEW LN SET LN=$QLENGTH(REF)
  . NEW CURNODE SET CURNODE=$QSUBSCRIPT(REF,LN)
  . IF TOPLEVEL=0 DO   ;"Exclude top level
  . . SET RESULT=RESULT_QT_CURNODE_QT_":"
  . NEW DV SET DV=$DATA(@REF)
  . ;"1  = node has value, but no subnode(s)
  . ;"11 = node has value AND subnode(s)
  . ;"10 = node has no value, but HAS subnode(s)
  . IF DV=0 DO      ;"1  = node has no value and no subnodes
  . . SET RESULT=RESULT_"'',"
  . IF DV=1 DO      ;"1  = node has value, but no subnode(s)
  . . SET RESULT=RESULT_$GET(@REF)_","
  . ELSE  IF (DV=10)!(DV=11) DO   ;"subnode(s) are found descending from this node
  . . IF DV=11 DO   ;"11 = node has value AND subnode(s)
  . . . SET TOPVAL=$GET(@REF)
  . . NEW NEXTREF SET NEXTREF=$$NEXTREF(REF,.PRIOR)
  . . SET REF=NEXTREF
  . . SET RESULT=RESULT_$$ARR2JSON(REF,1,.TOPVAL,.PRIOR)_","
  . NEW QL SET QL=$QLENGTH(REF)
  . NEW NEXTREF SET NEXTREF=$$NEXTREF(REF,.PRIOR)
  . NEW N1 SET N1=$$NAME2QL(NEXTREF,ORIGQL-1)
  . NEW N2 SET N2=$$NAME2QL(REF,ORIGQL-1)
  . IF N1'=N2 SET DONE=1 QUIT
  . NEW NEWQL SET NEWQL=$QLENGTH(NEXTREF)
  . IF NEWQL-LN>1 SET NEXTREF=$$NAME2QL(REF,LN+1)  
  . IF (NEXTREF="") SET DONE=1 QUIT
  . IF (GETSIBLINGS=0),$QSUBSCRIPT(NEXT,ORIGQL)'=ORIGLAST do
  . . SET DONE=1
  . SET REF=NEXTREF
  IF $EXTRACT(RESULT,$LENGTH(RESULT))="," DO
  . SET RESULT=$EXTRACT(RESULT,1,$LENGTH(RESULT)-1)  ;"remove trailing ','
  IF TOPLEVEL=0 SET RESULT=RESULT_"}"  
  ;
  QUIT RESULT
  ;
NEXTREF(REF,PRIOR) ;"
  NEW DONE SET DONE=0
  NEW RESULT SET RESULT=""
  NEW NL SET NL=$QLENGTH(REF)
  FOR  DO  QUIT:DONE
  . SET REF=$QUERY(@REF)
  . IF $DATA(PRIOR(REF)) QUIT  ;"SKIP OVER
  . SET DONE=1
  IF REF="" GOTO NRDN
  NEW NEWQL SET NEWQL=$QLENGTH(REF)
  ;"if e.g. colation jumps from ZZ(1) to ZZ(1,2,3,4,5) because intermediate nodes have no value, 
  ;"  then make result to be ZZ(1,2), which is next needed node for making json  
  IF NEWQL-LN>1 SET REF=$$NAME2QL(REF,LN+1)
  ;
NRDN ;  
  QUIT REF
  ;
NAME2QL(REF,QL)  ;"trim @REF to have no more than QL subscripts
  IF REF="" QUIT ""
  NEW RESULT SET RESULT=$QSUBSCRIPT(REF,0)_"("
  NEW IDX FOR IDX=1:1:QL SET RESULT=RESULT_$QSUBSCRIPT(REF,IDX)_$SELECT(IDX'=QL:",",1:"")
  SET RESULT=RESULT_")"
  QUIT RESULT
  ;