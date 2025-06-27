TMGJSON ;TMG/kst/JSON utilities ;5/27/25
	;;1.0;TMG-LIB;**1,17**;5/27/25
	;
	;
	;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
	;"Copyright (c) 5/27/25  Kevin S. Toppenberg MD
	;"
	;"This file is part of the TMG LIBRARY, and may only be used in accordence
	;" to license terms outlined in separate file TMGLICNS.m, which should
	;" always be distributed with this file.;
	;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
	;
	;"=======================================================================
	;" API -- Public Functions.;
	;"=======================================================================
	;"ARR2JSON(REF,[GETSIBLINGS],[TOPVAL],[PRIOR]) -- Stringify MUMPS array into JSON format (serialized into long string)
	;"JSON2ARR(JSONSTR,OUTREF,ENCAPS)  -- Parse JSON string into MUMPS array, storing in @OUTREF
	;"=======================================================================
	;" Private Functions.;
	;"=======================================================================
	;"MAP2ARR(OUTREF,MAPREF,MODE,EXPECTED,ERR,TOKENRESIDUAL)
	;"J2AERR(ERR,MSG) ;
	;"SSNUMERIC(REF,SSNUM)  -- IS SUBSCRIPT #, GIVEN BY SSNUM, PURELY NUMERIC, AND POSITIVE INTEGER VALUED?
	;"NEXTREF(REF,PRIOR) -- UTILITY FUNCTION FOR ARR2JSON
	;"NAME2QL(REF,QL)  -- trim @REF to have no more than QL subscripts.  --UTILITY FUNCTION FOR ARR2JSON
	;"=======================================================================
	;" Test Functions
	;"=======================================================================
	;"TESTA2J -- TEST ARRAY TO JSON
	;"TESTJ2A  -- TEST JSON TO ARRAY
	;"TESTJ2A2  -- TEST JSON TO ARRAY2
	;"=======================================================================
	;"=======================================================================
	;
ARR2JSON(REF,GETSIBLINGS,TOPVAL,PRIOR) ;"Stringify MUMPS array into JSON format (serialized into long string)
	;"PURPOSE: REPLACE functionality of ENCODE^XLFJSON.;
	;"INPUT:  REF -- NAME of MUMPS array to process
	;"        GETSIBLINGS.  Optional.  Default is 0.  If 1 then siblings of REF are added
	;"        TOPVAL -- Optional.  If present, inserted as a value of the top level node in REF
	;"        PRIOR -- PASS BY REFERENCE.  Used when calling self recursively, to
	;"                 avoid endless loops as can happen with skipped nodes in array.;
	;"EXAMPLE showing issues with ENCODE^XLFJSON.;
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
	;"  TMP("""1")=1    <--- notice that node 1,2,3=0 is missing.;
	;"  TMP("""2")=2
	;
	;"Expected json
	;"   [0,{"%%node_value%%":1,"2":[null,null,null,0]},2]
	;
	;"which can be formatted as below:
	;"[0,
	;" {"%%node_value%%":1,    <--- this is now I support a mumps node having both values AND children (not supported by JSON)
	;"  "2":[null,   <-- 0th node
	;"       null,
	;"       null,
	;"       0       <-- 3rd node
	;"      ]
	;" },
	;" 2]
	;"
	;"NOTICE that if a subscript is only positive integers, it will be treated as an array.;
	;"       And in this case, every index has to be accounted for, so null will be inserted for undefined nodes
	;"NOTE: TO DO, check for escape characters etc. ;
	;
	NEW TOPLEVEL SET TOPLEVEL=(REF'["(")
	SET GETSIBLINGS=$GET(GETSIBLINGS,0)
	SET TOPVAL=$GET(TOPVAL)
	NEW ORIGREF SET ORIGREF=REF
	NEW ORIGLAST SET ORIGLAST=$QSUBSCRIPT(REF,$QLENGTH(REF))
	NEW ORIGQL SET ORIGQL=$QLENGTH(REF)
	NEW ISNUMERIC SET ISNUMERIC=0
	IF TOPLEVEL=0 DO
	. ;"IF TOPVAL'="",(+TOPVAL'=TOPVAL)!(TOPVAL\1'=TOPVAL) QUIT  ;"leave as non-numeric
	. IF TOPVAL'="" QUIT  ;"leave as non-numeric
	. SET ISNUMERIC=$$SSNUMERIC(REF,ORIGQL)  ;"IS SUBSCRIPT #, GIVEN BY SSNUM, PURELY NUMERIC, AND POSITIVE INTEGER VALUED?
	NEW RESULT SET RESULT=$SELECT(TOPLEVEL:"",ISNUMERIC:"[",1:"{")
	NEW NUMIDX SET NUMIDX=0
	NEW QT SET QT=""""
	NEW DONE SET DONE=0
	FOR  DO  QUIT:DONE
	. NEW LN SET LN=$QLENGTH(REF)
	. NEW CURNODE SET CURNODE=$QSUBSCRIPT(REF,LN)
	. IF ISNUMERIC FOR  QUIT:(NUMIDX'<CURNODE)  DO
	. . SET RESULT=RESULT_"null,"
	. . SET NUMIDX=NUMIDX+1
	. SET PRIOR(REF)=1
	. IF TOPVAL'="" DO
	. . NEW AVAL SET AVAL=TOPVAL
	. . IF +AVAL'=AVAL SET AVAL=$$QTSTR(AVAL)
	. . SET RESULT=RESULT_$$QTSTR("%%node_value%%")_":"_AVAL_","
	. . SET TOPVAL=""
	. IF TOPLEVEL=0 DO   ;"Exclude top level
	. . IF ISNUMERIC QUIT  ;"If numeric, then we have array, not key:value pair. So don't show 'key'
	. . SET RESULT=RESULT_$$QTSTR(CURNODE)_":"  ;"CURNODE is the 'key'
	. NEW DV SET DV=$DATA(@REF)
	. ;"1  = node has value, but no subnode(s)
	. ;"11 = node has value AND subnode(s)
	. ;"10 = node has no value, but HAS subnode(s)
	. IF DV=0 DO      ;"1  = node has no value and no subnodes
	. . SET RESULT=RESULT_"null,"
	. IF DV=1 DO      ;"1  = node has value, but no subnode(s)
	. . IF ISNUMERIC DO
	. . . NEW AVAL SET AVAL=$GET(@REF)
	. . . IF +AVAL'=AVAL SET AVAL=$$QTSTR(AVAL)
	. . . SET RESULT=RESULT_AVAL_","
	. . . SET NUMIDX=NUMIDX+1
	. . ELSE  DO
	. . . NEW AVAL SET AVAL=$GET(@REF)
	. . . IF +AVAL'=AVAL SET AVAL=$$QTSTR(AVAL)
	. . . SET RESULT=RESULT_AVAL_","
	. ELSE  IF (DV=10)!(DV=11) DO   ;"subnode(s) are found descending from this node
	. . IF DV=11 DO   ;"11 = node has value AND subnode(s)
	. . . SET TOPVAL=$GET(@REF)
	. . NEW SUBREF SET SUBREF=$$NEXTREF(REF,.PRIOR) QUIT:SUBREF=""
	. . SET RESULT=RESULT_$$ARR2JSON(SUBREF,1,.TOPVAL,.PRIOR)_","
	. . IF ISNUMERIC SET NUMIDX=NUMIDX+1
	. NEW QL SET QL=$QLENGTH(REF)
	. NEW NEXTREF SET NEXTREF=$$NEXTREF(REF,.PRIOR)
	. ;"NEW N1 SET N1=$$NAME2QL(NEXTREF,ORIGQL-1)
	. ;"NEW N2 SET N2=$$NAME2QL(REF,ORIGQL-1)
	. ;"IF N1'=N2 SET DONE=1 QUIT
	. ;"NEW NEWQL SET NEWQL=$QLENGTH(NEXTREF)
	. ;"IF NEWQL-LN>1 SET NEXTREF=$$NAME2QL(REF,LN+1)
	. IF (NEXTREF="") SET DONE=1 QUIT
	. IF (GETSIBLINGS=0),$QSUBSCRIPT(NEXTREF,ORIGQL)'=ORIGLAST do
	. . SET DONE=1
	. SET REF=NEXTREF
	IF $EXTRACT(RESULT,$LENGTH(RESULT))="," DO
	. SET RESULT=$EXTRACT(RESULT,1,$LENGTH(RESULT)-1)  ;"remove trailing ','
	SET RESULT=RESULT_$SELECT(TOPLEVEL:"",ISNUMERIC:"]",1:"}")
	;
	QUIT RESULT
	;
QTSTR(S)  ;"Return S surrounded by quotes (and protecting any quotes already in S)
	NEW QT SET QT=""""
	IF S[QT SET S=$$QTPROTCT^TMGSTUT3(S)
	NEW RESULT SET RESULT=QT_S_QT
	QUIT RESULT
	;
GETNEXTTOKEN(MAPREF,IDX,TOKEN,SUBSEQUENT,DONE,RESIDUAL,PEEK) ;"Get next token from MAPREF
	;"INPUT: MAPREF -- map as created by MAPMATCH3^TMGSTUT3()
	;"       IDX -- PASS BY REFERENCE.  This is current index that we are pulling from
	;"       TOKEN -- PASS BY REFERENCE.  AN OUT PARAMETER.  This is the RESULT of the function.;
	;"       SUBSEQUENT --PASS BY REFERENCE.  AN OUT PARAMETER -- This is the token that will follow current token (sometimes needed in parsing)
	;"       DONE -- PASS BY REFERENCE.  Set to 1 when at end of MAPREF
	;"       RESIDUAL -- PASS BY REFERENCE.  Used to hold part of token not yet released
	;"       PEEK -- 1 if looking at subsequent element
	NEW RESULT SET RESULT=""
	NEW PART SET PART=""
	NEW PARTID SET PARTID=""
	NEW QT SET QT=""""
	SET PEEK=+$GET(PEEK)
	KILL TOKEN,SUBSEQUENT
	SET RESIDUAL=$GET(RESIDUAL)
	;
	;"-- Get next element from input 'stream' (stored in @MAPREF)
	IF RESIDUAL'="" DO
	. SET TOKEN=RESIDUAL
	. SET TOKEN("ISSTR")=0 ;"NOTE: Residual will never be a STRING, as those are never split up and put back into residual
	. SET RESIDUAL=""
	ELSE  DO
	. SET TOKEN=$GET(@MAPREF@(IDX))
	. SET TOKEN("ISSTR")=0
	. IF TOKEN="""" DO
	. . SET TOKEN=$GET(@MAPREF@(IDX,"STR"))
	. . IF $GET(@MAPREF@(IDX+1))="""" SET IDX=IDX+1
	. . SET TOKEN("ISSTR")=1  ;"<--- strings are always JUST strings, nothing comes after string. ;
	. SET IDX=$ORDER(@MAPREF@(IDX))
	;
	;"---Trim and parse token such that only next element returned.  Store remainder in RESIDUAL
	IF TOKEN("ISSTR")=0 SET TOKEN=$$TRIM^XLFSTR(TOKEN,"L")  ;"trim any leading whitespace.;
	NEW LEN SET LEN=$LENGTH(TOKEN)
	IF (TOKEN("ISSTR")=1)!(LEN=1) GOTO GNT2
	;"-- check for single character tokens.;
	NEW CH SET CH=$EXTRACT(TOKEN,1)
	IF "{[:,]}"[CH DO  GOTO GNT2
	. SET RESIDUAL=$$TRIM^XLFSTR($EXTRACT(TOKEN,2,$LENGTH(TOKEN)))
	. SET TOKEN=$EXTRACT(TOKEN,1)
	;"-- look for comma-separated elements.  At this point we are NOT dealing with strings which could contain a comma
	IF TOKEN["," DO
	. SET RESIDUAL=$$TRIM^XLFSTR(","_$PIECE(TOKEN,",",2,999))  ;"leave comma as first char of residual
	. SET TOKEN=$PIECE(TOKEN,",",1)
	NEW UPTOKEN SET UPTOKEN=$$UP^XLFSTR(TOKEN)
	IF (UPTOKEN="TRUE")!(UPTOKEN="FALSE") DO   ;"Handle JSON booleans.;
	. SET TOKEN="%%bool%%"_TOKEN
	. SET TOKEN("ISSTR")=1
	;"-- can add more checks below if needed later
	;" ...;
GNT2
	;"SET TOKEN("HASCOMMA")=(TOKEN[",")&(TOKEN("ISSTR")=0)
	;"SET TOKEN("HASCOLON")=(TOKEN[":")&(TOKEN("ISSTR")=0)
	;"SET TOKEN("LEN")=$LENGTH(TOKEN)
	IF TOKEN("ISSTR") DO
	. IF $$ISNUM^TMGSTUT3(TOKEN)=0 QUIT
	. IF +TOKEN'=TOKEN QUIT  ;""51837880897980798046658450" exceeds mumps num length, so keep as string.;
	. SET TOKEN("NUM")=+TOKEN   ;"will store numeric equivalence of numeric string, if applicable.;
	;
	SET DONE=(IDX'>0)&(RESIDUAL="")
	;
	;"-- Now get subsequent token.  Sometimes needed when interpreting TOKEN
	IF PEEK=0,DONE=0 DO
	. NEW TEMPRESIDUAL SET TEMRESIDUAL=$GET(RESIDUAL)
	. NEW TEMPIDX SET TEMPIDX=IDX
	. DO GETNEXTTOKEN(MAPREF,TEMPIDX,.SUBSEQUENT,,,TEMRESIDUAL,1) ;"Get next token from MAPREF
	;
	QUIT
	;
MAP2ARR(OUTREF,MAPREF,MODE,EXPECTED,ERR,TOKENRESIDUAL) ;
	;"INPUT:  OUTREF -- PASS BY NAME.  Data will be appended.;
	;"                E.g. if OUTREF="MVAR(1,2)", then json data will be put into MYVAR(1,2,<here> ...;
	;"        MAP -- PASS BY NAME.  MAP AS CREATED BY MAPMATCH3^TMGSTUT3
	;"        MODE -- "" OR "ARRAY" OR "OBJ"
	;"        EXPECTED -- STRING CONTAINING EXPECTED VALUES
	;"        ERR -- PASS BY REFERENCE.  AN OUT ERROR ARRAY.  Format:
	;"             ERR(#)=<error message>
	;"        TOKENRESIDUAL -- OPTIONAL.  Used when calling self residually, for managing input token stream
	;"Result: 1^OK, OR -1^ERROR MESSAGE
	;"------------
	;"Example map:
	;"  MAP
	;"  }~1 = [
	;"  | }~1 = 0,
	;"  | }~2 = {
	;"  | | }~1 = "
	;"  | | | }~1 = %%node_value%%
	;"  | | | }~STR = %%node_value%%
	;"  | | }~2 = "
	;"  | | }~3 = :1,
	;"  | | }~4 = "
	;"  | | | }~1 = 2
	;"  | | | }~STR = 2
	;"  | | }~5 = "
	;"  | | }~6 = :
	;"  | | }~7 = [
	;"  | | | }~1 = null,null,null,0
	;"  | | | }~STR = null,null,null,0
	;"  | | }~8 = ]
	;"  | | }~STR = "%%node_value%%":1,"2":[null,null,null,0]
	;"  | }~3 = }
	;"  | }~4 = ,2
	;"  | }~STR = 0,{"%%node_value%%":1,"2":[null,null,null,0]},2
	;"  }~2 = ]
	;"  }~STR = [0,{"%%node_value%%":1,"2":[null,null,null,0]},2]
	;"------------
	SET MODE=$GET(MODE)
	KILL PARENTVAL
	SET EXPECTED=$GET(EXPECTED,"[{")
	NEW KEY SET KEY=""
	NEW QT SET QT=""""
	NEW ARRIDX SET ARRIDX=0
	NEW DEPTH SET DEPTH=0
	NEW DONE SET DONE=0
	NEW IDX SET IDX=$ORDER(@MAPREF@(0))     ;"Get initial value. ;
	FOR  DO  QUIT:($DATA(ERR)>0)!DONE     ;"<--- note IDX not changed here.  It is change in GETNEXTTOKEN
	. NEW CURIDX SET CURIDX=IDX ;"GETNEXTTOKEN will advance IDX to the next done.  But we need to reference current node below
	. NEW TOKEN,SUBSEQUENT DO GETNEXTTOKEN(MAPREF,.IDX,.TOKEN,.SUBSEQUENT,.DONE,.TOKENRESIDUAL)
	. SET TOKEN("ID")=""  ;"DEFAULT
	. ;"--- Do some preliminary TOKEN parsing ---
	. IF EXPECTED[TOKEN,TOKEN'="" DO  GOTO M2
	. . SET TOKEN("ID")=TOKEN
	. NEW HANDLED SET HANDLED=0
	. IF EXPECTED["&" DO  QUIT:$DATA(ERR)   ;"& means key:value pattern expected or allowed
	. . ;"Just because '&' is accepted doesn't mean TOKEN is going to follow that pattern
	. . ;"If valid KEY:VALUE pair, then SUBSEQUENT should be ':'
	. . IF SUBSEQUENT'=":" QUIT  ;"not KEY:VALUE
	. . SET TOKEN("ID")="KEY"
	. . SET HANDLED=1
	. IF EXPECTED["$",HANDLED=0 DO       ;"$ means a value (any element)
	. . SET TOKEN("ID")="VAL",EXPECTED=",]}"
	. . SET HANDLED=1
M2 . ;"--- Now that TOKEN is set up, use below ---
	. IF (TOKEN="}")!(TOKEN="]") DO  QUIT
	. . IF DEPTH=0 SET DONE=1 QUIT  ;"End of grouping
	. . IF DEPTH>0 SET DEPTH=DEPTH-1
	. IF TOKEN=":" DO  QUIT
	. . SET EXPECTED="[{$"    ;"IN KEY:VALUE, value can be [] or {} or a value
	. IF TOKEN="," DO  QUIT
	. . SET EXPECTED=$SELECT(MODE="ARRAY":"[{$]",MODE="OBJ":"&}",1:"?")
	. . ;" IF MODE="ARRAY" DO  QUIT
	. . ;" . SET EXPECTED="[{$]"
	. . ;" IF MODE="OBJ" DO  QUIT
	. . ;" . SET EXPECTED="&}"
	. IF EXPECTED["&",TOKEN("ID")="KEY" DO  QUIT  ;"in this case TOKEN will hold key value
	. . IF MODE="ARRAY" DO J2AERR(.ERR,"Got a KEY value ["_KEY_"], but expected a VALUE because in ARRAY mode") QUIT
	. . KILL KEY MERGE KEY=TOKEN KILL TOKEN
	. . SET EXPECTED=":"
	. IF TOKEN("ID")="VAL" DO  QUIT
	. . IF MODE="ARRAY" DO
	. . . IF KEY'="" DO J2AERR(.ERR,"Got a KEY:VALUE, but expected just a VALUE because in ARRAY mode") QUIT
	. . . SET EXPECTED=",]"
	. . . NEW ISNULL SET ISNULL=((TOKEN="null")&(TOKEN("ISSTR")=0))
	. . . IF (ISNULL)=0 DO
	. . . . SET @OUTREF@(ARRIDX)=TOKEN
	. . . SET ARRIDX=ARRIDX+1
	. . ELSE  IF MODE="OBJ" DO
	. . . IF KEY="" DO J2AERR(.ERR,"Expected KEY:VALUE, but KEY is null while VALUE is ["_VAL_"]") QUIT
	. . . IF KEY="%%node_value%%" DO
	. . . . SET @OUTREF=TOKEN
	. . . ELSE  DO
	. . . . NEW ANODE SET ANODE=$SELECT($DATA(KEY("NUM"))>0:+KEY("NUM"),1:KEY)  ;"If numeric string, use number.;
	. . . . SET @OUTREF@(ANODE)=TOKEN
	. . . KILL KEY
	. . . SET EXPECTED=",}"
	. IF (TOKEN="{")!(TOKEN="[") DO  QUIT   ;"starting {} or [], so call self recursively. ;
	. . NEW SUBMAP SET SUBMAP=$NAME(@MAPREF@(CURIDX))
	. . NEW SUBMODE SET SUBMODE=$SELECT(TOKEN="[":"ARRAY",TOKEN="{":"OBJ")
	. . NEW SUBREF SET SUBREF=OUTREF
	. . IF MODE="ARRAY" DO
	. . . SET SUBREF=$NAME(@SUBREF@(ARRIDX))
	. . ELSE  IF MODE="OBJ" DO
	. . . NEW ANODE SET ANODE=$SELECT(KEY="":"NODE",$DATA(KEY("NUM"))>0:+KEY("NUM"),1:KEY)  ;"If numeric string, use number.;
	. . . SET SUBREF=$NAME(@OUTREF@(ANODE))
	. . NEW SUBEXPECT SET SUBEXPECT="[{"_$SELECT(SUBMODE="ARRAY":"$",SUBMODE="OBJ":"&")  ;"$ means value, & means KEY:VALUE
	. . SET DEPTH=DEPTH+1
	. . DO MAP2ARR(SUBREF,SUBMAP,SUBMODE,SUBEXPECT,.ERR,.TOKENRESIDUAL)
	. . IF MODE="ARRAY" SET ARRIDX=ARRIDX+1
	. . SET EXPECTED=","_$SELECT(TOKEN("ID")="[":"]",TOKEN("ID")="{":"}",1:"?")
	. DO J2AERR(.ERR,"Unexpected parse error.  Got ["_TOKEN_"]") ;
	QUIT
	;
J2AERR(ERR,MSG) ;
	NEW IDX SET IDX=$ORDER(ERR(""),-1)+1
	SET ERR(IDX)=MSG
	QUIT
	;
SSNUMERIC(REF,SSNUM)  ;"IS SUBSCRIPT #, GIVEN BY SSNUM, PURELY NUMERIC, AND POSITIVE INTEGER VALUED, AND CONSISTENT WITH ARRAY INDEX?
	NEW RESULT SET RESULT=1  ;"default to true.;
	SET REF=$$CREF^DILF(REF)
	NEW CT SET CT=0
	NEW ORIGPRIOR SET ORIGPRIOR=$QSUBSCRIPT(REF,SSNUM-1)
	NEW FIRSTSS SET FIRSTSS=""
	NEW DONE SET DONE=0
	FOR  DO  QUIT:DONE!(RESULT=0)
	. NEW NEWPRIOR SET NEWPRIOR=$QSUBSCRIPT(REF,SSNUM-1)
	. IF NEWPRIOR'=ORIGPRIOR SET DONE=1 QUIT
	. NEW ASUBSCR SET ASUBSCR=$QSUBSCRIPT(REF,SSNUM)
	. IF (ASUBSCR'=""),(+ASUBSCR'=ASUBSCR)!(ASUBSCR\1'=ASUBSCR)!(ASUBSCR<0) SET RESULT=0 QUIT
	. IF FIRSTSS="" SET FIRSTSS=ASUBSCR
	. SET CT=CT+1
	. SET REF=$QUERY(@REF)
	. IF REF="" SET DONE=1 QUIT
	IF FIRSTSS>5 SET RESULT=0      ;"e.g. if we have a first node with value of 75, don't make array with null entries for 1..74
	;"IF CT<3 SET RESULT=0           ;"e.g. if we only have 1 or 2 nodes, treat as KEY:VALUE, not arrays
	QUIT RESULT
	;
NEXTREF(REF,PRIOR) ;"UTILITY FUNCTION FOR ARR2JSON
	NEW DONE SET DONE=0
	NEW RESULT SET RESULT=""
	NEW ORIGQLEN SET ORIGQLEN=$QLENGTH(REF)
	NEW ORIGLASTN SET ORIGLASTN=$QSUBSCRIPT(REF,ORIGQLEN)
	NEW NEXTREF SET NEXTREF=REF ;"starting point
	FOR  DO  QUIT:DONE
	. SET NEXTREF=$QUERY(@NEXTREF)
	. IF $DATA(PRIOR(NEXTREF)) QUIT  ;"SKIP OVER
	. SET DONE=1
	IF NEXTREF="" GOTO NRDN
	NEW NEWQLEN SET NEWQLEN=$QLENGTH(REF)
	NEW NODECHANGED SET NODECHANGED=($QSUBSCRIPT(NEXTREF,ORIGQLEN)'=ORIGLASTN) ;"Gone from ZZ(1 --> ZZ(2, ?  or ZZ(1 --> ZZ(1,1 ?
	;"if e.g. colation jumps from ZZ(1) to ZZ(1,2,3,4,5) because intermediate nodes have no value,
	;"  then make result to be ZZ(1,2), which is next needed node for making json
	;"Notice in this case, both start with "ZZ(1"
	;"IF NODECHANGED=0,NEWQLEN-ORIGQLEN>1 DO
	IF NODECHANGED=0 DO
	. SET NEXTREF=$$NAME2QL(NEXTREF,ORIGQLEN+1)
	;
	;"If e.g. colation jumps from ZZ(1.5) TO ZZ(2,0) because ZZ(2) doesn't have value, then we
	;"  need to make the result to be ZZ(2), so that 2 can be used to make sub [] array or {} object
	IF NODECHANGED DO
	. SET NEXTREF=$$NAME2QL(NEXTREF,ORIGQLEN)
	;"
	NEW REFSTART1 SET REFSTART1=$$NAME2QL(NEXTREF,ORIGQLEN-1)
	NEW REFSTART2 SET REFSTART2=$$NAME2QL(REF,ORIGQLEN-1)
	IF REFSTART1'=REFSTART2 SET NEXTREF="" GOTO NRDN
	;
NRDN ;
	QUIT NEXTREF
	;
NAME2QL(REF,QL)  ;"trim @REF to have no more than QL subscripts.  --UTILITY FUNCTION FOR ARR2JSON
	IF REF="" QUIT ""
	NEW RESULT SET RESULT=$QSUBSCRIPT(REF,0)_"("
	NEW IDX FOR IDX=1:1:QL DO
	. NEW ANODE SET ANODE=$QSUBSCRIPT(REF,IDX)
	. IF +ANODE'=ANODE SET ANODE=""""_ANODE_""""
	. SET RESULT=RESULT_ANODE_$SELECT(IDX'=QL:",",1:"")
	SET RESULT=RESULT_")"
	QUIT RESULT
	;
TESTA2J ;"TEST ARRAY TO JSON
	NEW ZZ
	SET ZZ(0)=0
	SET ZZ(1)=1
	SET ZZ(1,2,3)=0
	SET ZZ(2)=2
	NEW STR SET STR=$$ARR2JSON("ZZ")
	WRITE STR,!
	QUIT
	;
JSON2ARR(JSONSTR,OUTREF,ENCAPS,ERR)  ;
	;"INPUT:  JSONSTR -- the string that represents a stringified version of json. ;
	;"        OUTREF -- PASS BY NAME.  Data will be appended.;
	;"                E.g. if OUTREF="MVAR(1,2)", then json data will be put into MYVAR(1,2,<here> ...;
	;"        ENCAPS -- OPTIONAL.  Array with encapsulators.  Will be filled automatically if not provided
	;"        ERR -- OPTIONAL.  PASS BY REFERENCE.  AN OUT ERROR ARRAY.  Format:
	;"             ERR(#)=<error message>
	IF $DATA(ENCAPS)=0 DO GETMATCHENCAP^TMGSTUT3(.ENCAPS,"{""'[")
	SET OUTREFF=$$CREF^DILF(OUTREF)  ;"ensure closed format
	NEW MAP DO MAPMATCH3^TMGSTUT3(JSONSTR,"MAP",.ENCAPS)  ;"convert linear string into semi-parsed array describing string
	DO MAP2ARR(OUTREF,"MAP",,,.ERR)
	QUIT
	;
TESTJ2A  ;"TEST JSON TO ARRAY
	;"NEW STR SET STR="[0,{""%%node_value%%"":1,""apple"":[null,null,null,""cat""]},2]"
	NEW STR SET STR="{""notifications"": {""email"": true,""sms"": false,""push"": true}}"
	NEW ZZ
	DO JSON2ARR(STR,"ZZ")
	IF $DATA(ZZ) ZWR ZZ
	QUIT
	;
TESTJ2A2  ;"TEST JSON TO ARRAY2
	NEW REF SET REF=$NAME(^VA(200,168))
	;"NEW REF SET REF=$NAME(^TIU(8925,5000))
	;"NEW REF SET REF=$NAME(^TMG("TMP","JSON"))
	NEW TMP,ORIGTMP
	MERGE TMP=@REF SET REF="TMP"
	MERGE ORIGTMP=TMP
	NEW ARR0 DO ZWR2ARR^TMGZWR("TMP","ARR0")
	NEW STR SET STR=$$ARR2JSON(REF);
	WRITE "JSON STRING",!
	WRITE STR,!,!
	KILL TMP
	DO JSON2ARR(STR,"TMP")
	NEW ARR1 DO ZWR2ARR^TMGZWR("TMP","ARR1")
	NEW ERR SET ERR=0
	NEW IDX SET IDX=0
	FOR  SET IDX=$ORDER(ARR0(IDX)) QUIT:IDX'>0  DO
	. NEW LINE0 SET LINE0=ARR0(IDX)
	. NEW LINE1 SET LINE1=$GET(ARR1(IDX))
	. IF LINE0=LINE1 QUIT
	. WRITE "ERROR:",!
	. WRITE "ORIG:",LINE0,!
	. WRITE " NEW:",LINE1,!
	. SET ERR=1
	IF ERR DO
	. WRITE "JSON STRING",!
	. WRITE STR,!,!
	. WRITE "ORIGINAL ARRAY",! ZWR ORIGTMP
	. WRITE !,"RECONSTRUCTED ARRAY",! ZWR TMP
	. WRITE !
	ELSE  DO
	. WRITE !,"SUCCESS!!",!
	;
	;
	QUIT