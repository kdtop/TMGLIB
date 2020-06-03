TMGRSTR1 ;TMG/kst/RPC web service Utilitis; 4/26/14
       ;;1.0;TMG-LIB;**1**;4/26/14
       ;
 ;"The following code is heavily modified from file nstRPCWrapper.
 ;"----------------------------------
 ;" Author: Nikolay Topalov
 ;" Copyright 2014 Nikolay Topalov
 ;" Licensed under the Apache License, Version 2.0 (the "License");
 ;" you may not use this file except in compliance with the License.
 ;" You may obtain a copy of the License at
 ;" http://www.apache.org/licenses/LICENSE-2.0
 ;" Unless required by applicable law or agreed to in writing, software
 ;" distributed under the License is distributed on an "AS IS" BASIS,
 ;" WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 ;" See the License for the specific language governing permissions and
 ;" limitations under the License.
 ;"----------------------------------
 ;" Modifications (c) 4/26/14 Kevin Toppenberg MD
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
 ;"RPC(TMGRPCRESULT,RPCNAME,RPCCONTEXT,PARAMSREF,OPTION) ;-Execute RPC
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"PARMREQ(IEN,SEQIEN) ;Is input RPC parameter is required?
 ;"BLDARGS(OUT,IEN8994,REF) ;Build RPC argument list
 ;"FRMTRSLT(CODE,MESSAGE) ;Return formatted result
 ;"ERROR(CODE,MESSAGE) ;Return an error message
 ;"SUCCESS(CODE,MESSAGE) ;Return a success message
 ;"CHKPRMIT(RPCNAME,IEN200,RPCCONTEXT) ;Is RPC permited to run in specified context?
 ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"=======================================================================
 ;"Uses:  TMGZWR, XUSRB, XWBLIB, XQCS, DILFD
 ;"=======================================================================
 ;
RPC(TMGRPCRESULT,RPCNAME,RPCCONTEXT,PARAMSREF,OPTION) ;"Execute RPC
  ;"Purpose:  Execute an RPC based on paramaters provided in PARAMSREF
  ;"NOTE: Thanks fot Nikolay Topalov for his initial work with an RPC broker for EWD
  ;"      which was my starting point.  However, the code below has been changed substantially.
  ;"Input: TMGRPCRESULT -- PASS BY REFERENCE.  An OUT PARAMETER.  Returns results. 
  ;"       RPCNAME -- Name of RPC to execute (case sensitive).  I.e. NAME (#8994, .01)
  ;"       RPCCONTEXT -- Name of context to run in.  If not provided, default is 'XUS SIGNON'  
  ;"       PARAMSREF -- PASS BY NAME.  Name of array holding input values.  Expected format:
  ;"          @PARAMSREF@(1)=value1
  ;"          @PARAMSREF@(1,j)=value2
  ;"          @PARAMSREF@(1,j,k,...)=value3  ... etc
  ;"          @PARAMSREF@(2)=value4
  ;"          @PARAMSREF@(2,j)=value5
  ;"          @PARAMSREF@(2,j,k,...)=value6 ... etc.
  ;"          1st index should be numeric, and specifies parameter order.  
  ;"          Other indexes can be any valid node: numeric or string.
  ;"          Any number of indices allowed (must have at least the first one, however)
  ;"       RPCCONTEXT -- Name of context to run RPC in. 
  ;"       OPTION -- PASS BY REFERENCE.  OPTIONAL Array holding input values.  Expected format, if any:
  ;"          OPTION("Version")   VERSION (#8994, .09) -- OPTIONAL
  ;"          OPTION("Use") = "L" or "R" -- OPTIONAL
  ;"NOTE: It is expected that the environment has been setup PRIOR to this call
  ;"      I.e. DUZ should be defined, if expected by the RPC, etc. 
  ;"Output: The RPC output is in OUT parameter TMGRPCRESULT 
  ;"        Also TMGRPCRESULT("__RPC_Result_Type__") is stored with result type, e.g. "SINGLE VALUE"
  ;"Result: Code^Message, (1 <--Success, or -n^error message)
  ;
  NEW TMGZZ SET TMGZZ=0  ;"Debug tool
  IF TMGZZ=1 DO
  . SET PARAMSREF="AREF"
  . KILL RPCNAME,RPCCONTEXT,@PARAMSREF,OPTION
  . MERGE RPCNAME=^TMG("TMP","WEB","RPC","DEBUG","RPCNAME")
  . MERGE RPCCONTEXT=^TMG("TMP","WEB","RPC","DEBUG","RPCCONTEXT")
  . MERGE @PARAMSREF=^TMG("TMP","WEB","RPC","DEBUG","@PARAMSREF")
  . MERGE OPTION=^TMG("TMP","WEB","RPC","DEBUG","OPTION")
  . MERGE TMGPARAMS=^TMG("TMP","WEB","RPC","DEBUG","TMGPARAMS")
  ELSE  DO
  . KILL ^TMG("TMP","WEB","RPC","DEBUG")
  . MERGE ^TMG("TMP","WEB","RPC","DEBUG","RPCNAME")=RPCNAME
  . MERGE ^TMG("TMP","WEB","RPC","DEBUG","RPCCONTEXT")=RPCCONTEXT
  . MERGE ^TMG("TMP","WEB","RPC","DEBUG","@PARAMSREF")=@PARAMSREF
  . MERGE ^TMG("TMP","WEB","RPC","DEBUG","OPTION")=OPTION
  . MERGE ^TMG("TMP","WEB","RPC","DEBUG","TMGPARAMS")=TMGPARAMS
  ;
  SET RPCNAME=$GET(RPCNAME) 
  QUIT:RPCNAME="" $$ERROR(-1,"RPC name is missing")
  ;
  NEW RPCIEN SET RPCIEN=$ORDER(^XWB(8994,"B",RPCNAME,""))
  QUIT:'RPCIEN $$ERROR(-2,"Undefined RPC ["_RPCNAME_"]")
  ;
  SET RPCCONTEXT=$GET(RPCCONTEXT)
  IF RPCCONTEXT="" SET RPCCONTEXT="XUS SIGNON"  ;"default to a signon context
  ;  
  NEW XWBAPVER SET XWBAPVER=$GET(OPTION("Version")) ;"OPTIONAL
  NEW RPCUSE SET RPCUSE=$GET(OPTION("Use")) ;"OPTIONAL
  ;
  NEW OUT DO CKRPC^XWBLIB(.OUT,RPCNAME,RPCUSE,XWBAPVER) ;"Is the RPC available?
  QUIT:'OUT $$ERROR(-3,"RPC ["_RPCNAME_"] cannot be run at this time.") 
  ;
  NEW TMGZN SET TMGZN=$GET(^XWB(8994,RPCIEN,0)) ;"e.g., XWB EGCHO STRING^ECHO1^XWBZ1^1^R
  NEW RTNTAG SET RTNTAG=$P(TMGZN,"^",2)
  NEW RTNNAME SET RTNNAME=$P(TMGZN,"^",3)
  QUIT:(RTNNAME="") $$ERROR(-4,"Undefined routine name for RPC ["_RPCNAME_"]")
  ;
  NEW TMP SET TMP=$$CHKPRMIT(RPCNAME,.DUZ,RPCCONTEXT)
  QUIT:TMP'="" $$ERROR(-4,"RPC ["_RPCNAME_"] is not allowed to be run: "_TMP)
  ;
  NEW ARGS SET TMP=$$BLDARGS(.ARGS,RPCIEN,PARAMSREF)  ;" Build RPC arguments list - ARGS
  QUIT:TMP<0 $$ERROR($P(TMP,"^"),$P(TMP,"^",2)) ;" Error building arguments list
  ;
  ;"Now, prepare the arguments for the final call
  ;"Now outside of the $$BLDARGS so we can now NEW the individual parameters
  NEW IDX SET IDX=""
  FOR  SET IDX=$ORDER(ARGS(IDX)) QUIT:IDX=""  NEW @("PARAM"_IDX) XECUTE ARGS(IDX)  
  ;
  KILL TMGRPCRESULT
  NEW CODE SET CODE="DO "_RTNTAG_"^"_RTNNAME_"(.TMGRPCRESULT"_ARGS_")"
  XECUTE CODE  ;" execute the RPC routine
  SET TMGRPCRESULT=$GET(TMGRPCRESULT)
  ;
  ;"1=SINGLE VALUE; 2=ARRAY; 3=WORD PROCESSING; 4=GLOBAL ARRAY; 5=GLOBAL INSTANCE
  NEW RPCRESULTTYPE SET RPCRESULTTYPE=$P(TMGZN,"^",4)
  SET TMGRPCRESULT("__RPC_Result_Type__")=$$EXTERNAL^DILFD(8994,.04,,RPCRESULTTYPE)
  IF TMGRPCRESULT("__RPC_Result_Type__")="GLOBAL ARRAY" DO
  . IF $EXTRACT(TMGRPCRESULT,1)="^" DO
  . . NEW ZZTMP MERGE ZZTMP=@TMGRPCRESULT
  . . IF $PIECE(TMGRPCRESULT,"(",1)="^TMP" KILL @TMGRPCRESULT  ;"What are proper rules for deleting passed array??
  . . KILL TMGRPCRESULT MERGE TMGRPCRESULT=ZZTMP
  . ELSE  DO
  . . SET RPCRESLT="-1^Unable to get result from 'global array' ("_TMGRPCRESULT_")"
  QUIT $$SUCCESS()
  ;
  ;
PARMREQ(IEN,SEQIEN) ;" Is input RPC parameter is required?
  ;" IEN - RPC IEN in file #8994
  ;" SEQIEN - Input parameter IEN in multiple file #8994.02
  QUIT ($P(^XWB(8994,IEN,2,SEQIEN,0),"^",4)=1)
  ;
BLDARGS(OUT,IEN8994,REF) ;"Build RPC argument list
  ;"Input: OUT -- PASS BY REFERENCE.  AN OUT PARAMETER.  FORMAT:
  ;"         OUT = string of parameters.
  ;"         OUT(#) = mumps code to SET parameters
  ;"       IEN8994 -- IEN OF RPC 
  ;"       REF -- PASS BY NAME.  Name of array containing input parameters.  See RPC() documentation
  ;"Result: 1 <--Success, or -n^error message
  NEW IDX SET IDX="" ;"// was 0
  NEW COUNT SET COUNT=0
  KILL OUT SET OUT=""
  ;"NOTE: Code block below writen with understanding that REMOTE PROCEDURE file is
  ;"      kept up to date.  But actually, it is not (e.g. XUS SIGNON SETUP)..  So 
  ;"      will simply take the parameters that are passed, and use them to call RPC.  
  ;"      And if error occurs, then the developer will either need to change the 
  ;"      parameters passed in calling, or modify the RPC entry point itself.
  ;"
  ;"NEW TIEN,X
  ;"NEW JDX SET JDX=""
  ;"NEW ERR SET ERR=0
  ;"NEW SEQXREF SET SEQXREF=$DATA(^XWB(8994,IEN8994,2,"PARAMSEQ"))  ;"Is the cross-reference defined?
  ;"NEW PARAMREF SET PARAMREF=$SELECT(SEQXREF:"^XWB(8994,IEN8994,2,""PARAMSEQ"")",1:"^XWB(8994,IEN8994,2)")
  ;
  ;"FOR  SET IDX=$ORDER(@PARAMREF@(IDX)) QUIT:('IDX)!(ERR)  DO
  ;". SET JDX=$ORDER(@REF@(JDX))
  ;". SET TIEN=$SELECT(SEQXREF:$ORDER(@PARAMREF@(IDX,"")),1:IDX)  ; get the IEN of the input parameter
  ;". NEW ISREQ SET ISREQ=$$PARMREQ(IEN8994,TIEN)
  ;". IF ISREQ,'$DATA(@REF@(JDX,"Value")) SET ERR="-5^Required input paramater is missing." QUIT
  ;". IF '$DATA(@REF@(JDX,"Value")) SET OUT=OUT_"," QUIT
  ;". IF $DATA(@REF@(JDX,"Value"))=1 DO  QUIT
  ;". . SET OUT=OUT_"PARAM"_IDX_","   ; add the argument
  ;". . IF $$UP^XLFSTR($GET(@REF@(JDX,"Type")))="REFERENCE" DO
  ;". . . SET COUNT=COUNT+1,OUT(IDX,COUNT)="SET PARAM"_IDX_"=@@REF@("_JDX_",""Value"")"  ; set it
  ;". . . QUIT 
  ;". . ELSE  SET COUNT=COUNT+1,OUT(IDX,COUNT)="SET PARAM"_IDX_"=@REF@("_JDX_",""Value"")"  ;" set it as action for later
  ;". . QUIT
  ;". ;" list/array
  ;". SET OUT=OUT_".PARAM"_IDX_","
  ;". SET COUNT=COUNT+1
  ;". SET OUT(IDX,COUNT)="MERGE PARAM"_IDX_"=@REF@("_JDX_",""Value"")"  ;"Merge it
  ;". QUIT
  ;
  FOR  SET IDX=$ORDER(@REF@(IDX)) QUIT:(IDX="")  DO
  . SET OUT=OUT_",.PARAM"_IDX
  . SET OUT(IDX)="MERGE PARAM"_IDX_"="_REF_"("_IDX_")"  ;"Merge it later
  ;
  ;"QUIT:ERR ERR
  ;"SET OUT=$EXTRACT(OUT,1,$L(OUT)-1)
  QUIT 1
  ;
FRMTRSLT(CODE,MESSAGE) ;"Return formatted result
  QUIT CODE_"^"_MESSAGE
  ;"QUIT "{""success"": "_CODE_", ""MESSAGE"": """_$SELECT($TR(MESSAGE," ","")="":"",1:MESSAGE)_"""}"
  ;
ERROR(CODE,MESSAGE) ;"Return an error message
  QUIT $$FRMTRSLT(0,CODE_" "_MESSAGE)
  ;
SUCCESS(CODE,MESSAGE) ;"Return a success message
  NEW MSG2 SET MSG2=$GET(CODE)
  IF MSG2="" SET MSG2="OK"
  IF $GET(MESSAGE)'="" SET MSG2=MSG2_" "_MESSAGE
  QUIT $$FRMTRSLT(1,MSG2)
  ;"QUIT $$FRMTRSLT(1,$GET(CODE)_" "_$GET(MESSAGE))
  ;
CHKPRMIT(RPCNAME,IEN200,RPCCONTEXT) ;
  ;"Purpose: Checks to see if remote procedure is permited to run in specified context?
  ;"Input:  RPCNAME - Name of Remote procedure to check
  ;"        IEN200  - User IEN
  ;"        RPCCONTEXT - RPC Context name
  ;"Result: "" or 1 if RPC OK to run, or ErrorMessage if problem.
  ;
  ;"In the beginning, when no DUZ is defined and no context exist,
  ;"setup default signon context
  SET:'$GET(IEN200) IEN200=0,RPCCONTEXT="XUS SIGNON"   ;set up default context
  ;
  QUIT:$$KCHK^XUSRB("XUPROGMODE",IEN200) ""  ;" User has programmer key
  NEW X,XQMES
  NEW RESULT SET RESULT="" ;"Return XWBSEC="" if OK to run RPC
  ;
  ;"These RPC's are allowed in any context, so we can just quit
  SET X="^"
  SET X=X_"XWB IM HERE^"
  SET X=X_"XWB CREATE CONTEXT^"
  SET X=X_"XWB RPC LIST^"
  SET X=X_"XWB IS RPC AVAILABLE^"
  SET X=X_"XUS GET USER INFO^"
  SET X=X_"XUS GET TOKEN^"
  SET X=X_"XUS SET VISITOR^"
  SET X=X_"XUS KAAJEE GET USER INFO^"
  SET X=X_"XUS KAAJEE LOGOUT^"  ;"KAAJEE --> VistALink RPC's that are always allowed.
  SET X=X_"TMG XUS GET SHARED SIGNON^"
  IF X[("^"_RPCNAME_"^") QUIT RESULT
  ;
  ;"If in Signon context, only allow XUS and XWB RPC's
  IF $GET(RPCCONTEXT)="XUS SIGNON","^XUS^XWB^"'[("^"_$E(RPCNAME,1,3)_"^") QUIT "Application context has not been created!"
  ;"XQCS allows all users access to the XUS SIGNON context.
  ;"Also to any context in the XUCOMMAND menu.
  ;
  IF $GET(RPCCONTEXT)="" QUIT "Application context has not been created!"
  ;
  SET X=$$CHK^XQCS(IEN200,RPCCONTEXT,RPCNAME)         ;"Do the check
  SET:'X RESULT=X
  QUIT RESULT
  ;
