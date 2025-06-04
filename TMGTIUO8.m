TMGTIUO8 ;TMG/kst-Text objects for use in CPRS ; 2/2/14 1/15/17, 5/17/18, 3/24/21
         ;;1.0;TMG-LIB;**1,17**;8/21/13
 ;  
 ;"Kevin Toppenberg MD
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
 ;"PUBLIC FUNCTIONS
 ;"=======================================================================
 ;"LABCMTBL(TMGDFN,LABEL,OUTARR) ;"LAB & COMMENT TABLE 
 ;"
 ;"=======================================================================
 ;"PRIVATE FUNCTIONS
 ;"=======================================================================
 ;"
 ;"=======================================================================
 ;"Dependancies : TMGPXR02, TIUSRVD
 ;"=======================================================================
 ;
LABCMTBL(TMGDFN,LABEL,OUTARR,OPTION) ;"LAB & COMMENT TABLE 
        ;"Input: TMGDFN -- IEN in PATIENT file
        ;"       LABEL -- This is the .01 field of the table (File# 22708) to create
        ;"       OUTARR -- OPTIONAL.  PASS BY REFERENCE to get back array of data.
        ;"                 NOTE: doesn't include lines containing 'OLD TABLE (EDIT)-->' 
        ;"       OPTION -- OPTIONAL.
        ;"              OPTION("DT")=FMDT <-- if present, then table is to be returns AS OF given date
        ;"              OPTION("DIRECT HTML INSERTION")=1  <-- Output should be ready to insert directly into HTML DOM
        ;"Result: returns string with embedded line-feeds to create text table.
        NEW TMGPRIORTABLE,TMGTABLEDEFS
        DO INITPFIL^TMGMISC2("GETITEM^TMGTIUO8") ;"delete old timing data
        NEW TMGRESULT SET TMGRESULT=$$RMDGTABL^TMGPXR02(TMGDFN,LABEL,"GETITEM^TMGTIUO8",.OUTARR,.OPTION)
        ;" NEW TMPO SET TMPO="OLD TABLE (EDIT)-->"  ;"also in GETPRIOR^TMGTIUO8
        ;" IF TMGRESULT'[TMPO GOTO LBCMTDN 
        ;" ;"Now remove empty or redundant elements from OLD TABLE 
        ;" NEW CHANGED SET CHANGED=0
        ;" NEW TEMPARR DO SPLIT2AR^TMGSTUT2(TMGRESULT,$CHAR(13,10),.TEMPARR)
        ;" NEW IDX
        ;" ;"Set up a temp xref
        ;" SET IDX="" FOR  SET IDX=$ORDER(TEMPARR(IDX)) QUIT:IDX'>0  DO
        ;" . NEW STR SET STR=$GET(TEMPARR(IDX)) QUIT:STR=""
        ;" . SET TEMPARR("B",STR,IDX)=""
        ;" ;"Delete redundant lines
        ;" SET IDX="" FOR  SET IDX=$ORDER(TEMPARR(IDX)) QUIT:IDX'>0  DO
        ;" . NEW STR SET STR=$GET(TEMPARR(IDX))
        ;" . IF STR'[TMPO QUIT
        ;" . SET STR=$$TRIM^XLFSTR($PIECE(STR,TMPO,2))
        ;" . IF (STR="")!(+$ORDER(TEMPARR("B",STR,0))>0) DO
        ;" . . KILL TEMPARR(IDX)  ;"only kills redundant lines beginning with 'OLD TABLE (EDIT) -->'
        ;" . . SET CHANGED=1
        ;" ;"Reassemble final result if changes made. 
        ;" IF CHANGED=1 DO
        ;" . SET TMGRESULT=""
        ;" . SET IDX="" FOR  SET IDX=$ORDER(TEMPARR(IDX)) QUIT:IDX'>0  DO
        ;" . . NEW STR SET STR=$GET(TEMPARR(IDX))
        ;" . . SET TMGRESULT=TMGRESULT_STR_$CHAR(13,10)
LBCMTDN QUIT TMGRESULT
        ;
INLNTABL(TMGDFN,LABEL,OUTARR,OPTION) ;"INLINE TABLE
        NEW TMGPRIORTABLE,TMGTABLEDEFS
        SET OPTION("NO HEADER LINE")=1
        SET OPTION("NO LF")=1
        ;"IF '$D(OPTION("HTML")) 
        SET OPTION("HTML")=0  ;"FORCE HTML TO 0, IF NOT ALREADY SET 5/31/18
        NEW TMGRESULT SET TMGRESULT=$$DOTABL^TMGPXR02(TMGDFN,LABEL,"GETITEM^TMGTIUO8",.OUTARR,.OPTION)
        QUIT TMGRESULT
        ;
TESTWARN(WARNTEXT,TMGDFN)  ;"
        NEW TMGRESULT SET TMGRESULT=0
        IF WARNTEXT="" GOTO TWDN
        IF (WARNTEXT="Y")!(WARNTEXT="1") DO  GOTO TWDN
        . SET TMGRESULT=1
        IF WARNTEXT["{CODE" DO
        . NEW XCUTE SET XCUTE="SET TMGRESULT=$$"
        . SET WARNTEXT=$$TRIM^XLFSTR($P(WARNTEXT,"{CODE:",2))
        . SET XCUTE=$P(WARNTEXT,",",1)_"^"_$P(WARNTEXT,",",2)_"(TMGDFN)"
        . X XCUTE
TWDN
        QUIT TMGRESULT
        ;"
GETITEM(TMGDFN,IEN,SUBIEN,MAXLEN,OUTARR,OPTION) ;"Get a table item.
        ;"Input: TMGDFN -- IEN in PATIENT file
        ;"       IEN -- IEN IN 22708
        ;"       SUBIEN -- IEN IN 22708.01 (ITEM multiple)
        ;"       MAXLEN -- Number indicating max allowed length of line. 
        ;"       OUTARR -- OPTIONAL.  PASS BY REFERENCE to get back array of data.
        ;"                   OUTARR("KEY-VALUE",<LABEL>)=<VALUE>
        ;"                   OUTARR("KEY-VALUE",<LABEL>,"LINE")=<full text of line>
        ;"                   OUTARR(#)=<Line text>     <-- for entries that are not KEY-VALUE format
        ;"       OPTION -- OPTIONAL.  PASS BY REFERENCE.  Format:
        ;"                   OPTION("DT")=FMDT <-- will return table AS OF specified FM date-time
        ;"                   OPTION("HTML")=1 if text is in HTML format. 
        ;"                   OPTION("DIRECT HTML INSERTION")=1  <-- Output should be ready to insert directly into HTML DOM
        ;"                   OPTION("MESSAGE: MEDS COLORED")=1  <-- an OUTPUT message 
        ;"                   OPTION("WARN")=1 (if designed to warn on blank entry)
        ;"NOTE:  TMGPRIORTABLE used in global scope.  Var may be modified,
        ;"         but no error generated if not defined before calling here.
        ;"       TMGTABLEDEFS used in global scope.  Var may be modified,
        ;"         but no error generated if not defined before calling here.
        ;"           TMGTABLEDEFS(SUBIEN)=CMD^PARAM^PARAM^PARAM...
        ;"Result : returns one line, that can be added to table.
        ;"        Format is   'Label : Value'
        ;"NOTE: If multiple lines need to be returned, then
        ;"      lines can be separated by CR (#13)
        ;"NOTE2: Items from this function will be HTML symbol encoded if OPTION("HTML")=1
        NEW TMGRESULT SET TMGRESULT=""
        NEW DESCR SET DESCR=""
        IF $DATA(TMGTABLEDEFS)=0 DO GETDEFS(IEN,.TMGTABLEDEFS)                 
        NEW TABLENAME SET TABLENAME=$PIECE($GET(^TMG(22708,IEN,0)),"^",1)
        NEW ZN SET ZN=$GET(^TMG(22708,IEN,1,SUBIEN,0))
        NEW DISPCODE SET DISPCODE=$GET(^TMG(22708,IEN,1,SUBIEN,6))
        NEW WARN SET WARN=$PIECE($GET(^TMG(22708,TABLEIEN,1,SUBIEN,5)),"^",1)
        IF $$TESTWARN(WARN,TMGDFN)="1" SET OPTION("WARN")=1
        ELSE  SET OPTION("WARN")=0
        NEW CODE SET CODE=$GET(^TMG(22708,IEN,1,SUBIEN,4))  ;"OPTIONAL TRIGGER HOOK
        IF CODE'="" SET OPTION("CODE")=CODE
        ELSE  SET OPTION("CODE")=""
        NEW HTML SET HTML=+$GET(OPTION("HTML"))
        NEW DIRHTMLINSERT SET DIRHTMLINSERT=+$GET(OPTION("DIRECT HTML INSERTION"))
        NEW NAME SET NAME=$PIECE(ZN,"^",1)
        NEW LIMITNUM SET LIMITNUM=+$PIECE(ZN,"^",4) 
        IF LIMITNUM=0 SET LIMITNUM=3  ;"Default of 3 labs returned. 
        NEW SHOWNULL SET SHOWNULL=($PIECE(ZN,"^",5)="Y") ;"boolean
        ;" Test gender
        NEW GENDER SET GENDER=$PIECE(ZN,"^",6)  ;"  0;6=GENDER SPECIFIC
        IF GENDER'="" IF $PIECE($GET(^DPT(TMGDFN,0)),"^",2)'=GENDER GOTO GIDN  ;"Skip IF wrong gender.
        ;"Test age
        NEW MINAGE,MAXAGE,PTAGE 
        SET MINAGE=$PIECE(ZN,"^",8)
        SET MAXAGE=$PIECE(ZN,"^",9)
        SET PTAGE=$$AGE^TIULO(TMGDFN)
        IF MINAGE'="" IF MINAGE>PTAGE GOTO GIDN  ;"Skip if too young
        IF MAXAGE'="" IF MAXAGE<PTAGE GOTO GIDN  ;"Skip if too old
        ;"Test arbitrary code
        NEW Y SET Y=1
        ;IF DISPCODE'=""DO  IF Y'=1 GOTO GIDN  ;"Skip if code fails
        ;. NEW $ETRAP SET $ETRAP="S Y=0"
        ;. XECUTE DISPCODE
        ;"Test linked reminder
        NEW REMIEN SET REMIEN=$PIECE(ZN,"^",10)
        NEW REMDUE SET REMDUE=1
        ;IF REMIEN'="" DO
        ;. NEW X,REMDATA
        ;. DO NEW^%DTC
        ;. SET REMDATA=$$TEST1REM^TMGRPC3G("TMGRESULT",REMIEN,TMGDFN,X,"N")
        NEW DISPTEXT SET DISPTEXT=$PIECE(ZN,"^",7)
        NEW PARSED SET PARSED=$GET(TMGTABLEDEFS(SUBIEN))
        NEW ISAHF SET ISAHF=0
        IF PARSED="" DO  ;"See IF this entry makes use of the health factor fields
        . IF $DATA(^TMG(22708,IEN,1,SUBIEN,1))>0 SET ISAHF=1 QUIT
        . IF $DATA(^TMG(22708,IEN,1,SUBIEN,2))>0 SET ISAHF=1 QUIT
        . IF $DATA(^TMG(22708,IEN,1,SUBIEN,3))>0 SET ISAHF=1 QUIT
        NEW CMD SET CMD=$$UP^XLFSTR($PIECE(PARSED,"^",1))
        ;"//kt start debug  timer stuff ------------
        NEW DESCRHDR SET DESCRHDR="TABLE#"_IEN_" "
        IF CMD="" SET DESCR=DESCRHDR_"SUBIEN="_SUBIEN
        ELSE  IF CMD="CODE" SET DESCR=DESCRHDR_"CODE:$$"_$PIECE(PARSED,"^",2)_"^"_$PIECE(PARSED,"^",3)_"("_TMGDFN_")"
        ELSE  SET DESCR=DESCRHDR_CMD
        DO TIMEPFIL^TMGMISC2("GETITEM^TMGTIUO8","ITEM^"_DESCR,1)  ;"RECORD START TIME
        ;"//kt end debug  timer stuff ------------
        NEW VALSTR SET VALSTR=""
        NEW TEMPOUTARR
        IF CMD="LAB" DO
        . SET VALSTR=$$GETLABS(TMGDFN,NAME,PARSED,SHOWNULL,LIMITNUM,.TEMPOUTARR,.OPTION) 
        ELSE  IF CMD="LAB DATES" DO
        . SET VALSTR=$$GETLABDT(TMGDFN,NAME,PARSED,SHOWNULL,LIMITNUM,.TEMPOUTARR,.OPTION)
        ELSE  IF CMD="MEDS" DO
        . SET VALSTR=$$GETMEDS(TMGDFN,NAME,IEN,PARSED,SHOWNULL,.TMGTABLEDEFS,.TEMPOUTARR,.OPTION)                 
        ELSE  IF CMD="PRIOR" DO
        . SET VALSTR=$$GETPRIOR(TMGDFN,NAME,IEN,TABLENAME,PARSED,SHOWNULL,.TMGPRIORTABLE,.TMGTABLEDEFS,.TEMPOUTARR,.OPTION)         
        ELSE  IF CMD="HIDE" GOTO GIDN  ;"Does nothing here.  Effects in {PRIOR: !MISC!}
        ELSE  IF CMD="CODE" DO
        . NEW XCODE SET XCODE="SET VALSTR=$$"_$PIECE(PARSED,"^",2)_"^"_$PIECE(PARSED,"^",3)_"("_TMGDFN_",.TEMPOUTARR)"
        . ;"NEW XCODE SET XCODE="SET VALSTR=$$"_$PIECE(PARSED,"^",2)_"^"_$PIECE(PARSED,"^",3)_"("_TMGDFN_")"
        . XECUTE XCODE
        ELSE  IF CMD="",ISAHF=1 DO
        . SET VALSTR=$$GETITEM^TMGPXR02(TMGDFN,IEN,SUBIEN,MAXLEN,.TEMPOUTARR,.OPTION)
        ELSE  DO
        . SET VALSTR=$$BOIL^TIUSRVD(DISPTEXT)
        . NEW I2 SET I2=+$ORDER(TEMPOUTARR("@"),-1)+1
        . SET OUTARR(I2)=VALSTR        
        ;"ELH ADDED BELOW TO UTILIZE THE VALUE MODIFYING CODE 1/6/22
        NEW CODE SET CODE=$GET(^TMG(22708,IEN,1,SUBIEN,4))
        IF (CODE'="")&(VALSTR'="") DO     
        . NEW TMGX,TMGY
        . SET TMGX=VALSTR
        . XECUTE CODE
        . SET TMGY=$GET(TMGY)
        . IF TMGY'="" SET VALSTR=TMGY
        ;"
        SET TMGRESULT=VALSTR
        ;"//-- Start changes 5/23/18
        ;"Below takes care of STRING part of output
        IF (HTML!DIRHTMLINSERT),(TMGRESULT'="") DO 
        . NEW TEMPARR DO SPLIT2AR^TMGSTUT2(TMGRESULT,$CHAR(13),.TEMPARR)
        . NEW IDX SET IDX=0
        . FOR  SET IDX=$ORDER(TEMPARR(IDX)) QUIT:IDX'>0  DO
        . . NEW LINE SET LINE=$GET(TEMPARR(IDX)) QUIT:LINE=""
        . . IF CODE="" SET LINE=$$SYMENC^MXMLUTL(LINE)  ;"DON'T DO FOR VALUE ALTERED ITEMS
        . . IF TABLENAME["MEDICATION" DO
        . . . NEW L2 SET L2=$$CHKMED^TMGTIUOT(LINE,PTAGE,.OPTION)
        . . . IF LINE'=L2 SET OPTION("MESSAGE: MEDS COLORED")=1
        . . . SET LINE=L2
        . . IF LINE[$$WARNTEXT^TMGTIUOT() DO
        . . . NEW WARNTEXT SET WARNTEXT=$$WARNTEXT^TMGTIUOT()
        . . . SET LINE=$P(LINE,WARNTEXT,1)_$$WRAPTEXT^TMGTIUOT(WARNTEXT,$$AUTOCOLOR^TMGTIUOT(),.OPTION)_$P(LINE,WARNTEXT,2)
        . . SET TEMPARR(IDX)=LINE
        . SET TMGRESULT=$$ARR2STR^TMGSTUT2(.TEMPARR,"<br>"_$CHAR(13))  ;"//kt <--- should this CR be removed??
        ;"Below takes care of ARRAY part of output
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(TEMPOUTARR(IDX)) QUIT:IDX'>0  DO
        . NEW JDX SET JDX=$ORDER(OUTARR("@"),-1)+1
        . NEW LINE SET LINE=$GET(TEMPOUTARR(IDX))
        . IF (HTML!DIRHTMLINSERT) DO
        . . SET LINE=$$SYMENC^MXMLUTL(LINE) 
        . IF LINE[$$WARNTEXT^TMGTIUOT() DO
        . . NEW WARNTEXT SET WARNTEXT=$$WARNTEXT^TMGTIUOT()
        . . SET LINE=$P(LINE,WARNTEXT,1)_$$WRAPTEXT^TMGTIUOT(WARNTEXT,$$AUTOCOLOR^TMGTIUOT(),.OPTION)_$P(LINE,WARNTEXT,2)
        . SET OUTARR(JDX)=LINE
        NEW KEY SET KEY=""
        FOR  SET KEY=$ORDER(TEMPOUTARR("KEY-VALUE",KEY)) QUIT:KEY=""  DO
        . NEW ENCODEDKEY SET ENCODEDKEY=$$SYMENC^MXMLUTL(KEY)
        . NEW VALUE SET VALUE=$GET(TEMPOUTARR("KEY-VALUE",KEY))
        . IF (HTML!DIRHTMLINSERT) SET VALUE=$$SYMENC^MXMLUTL(VALUE) 
        . NEW LINE SET LINE=$GET(TEMPOUTARR("KEY-VALUE",KEY,"LINE"))
        . IF (HTML!DIRHTMLINSERT) SET LINE=$$SYMENC^MXMLUTL(LINE)
        . SET OUTARR("KEY-VALUE",ENCODEDKEY)=VALUE
        . SET OUTARR("KEY-VALUE",ENCODEDKEY,"LINE")=LINE
        ;"//-- End changes 5/23/18
GIDN    ;
        DO TIMEPFIL^TMGMISC2("GETITEM^TMGTIUO8","ITEM^"_DESCR,0)  ;"RECORD END TIME
        QUIT TMGRESULT
        ;
TEST(TMGDFN)  ;" Used to test the "CODE" command from up above
        QUIT "HELLO WORLD"
        ;"
GETDEFS(IEN,OUT) ;"Get list of defined        
        ;"Input: IEN -- IEN IN 22708
        ;"       OUT -- PASS BY REFERENCE, AN OUT PARAMETER.  Format:
        ;"         OUT(SUBIEN)=CMD^PARAM^PARAM^PARAM...
        ;"         OUT(SUBIEN,.01)=Line_Label_Name
        ;"Results: none.
        SET IEN=+$GET(IEN)
        NEW PARAMSTR
        NEW SUBIEN SET SUBIEN=0
        FOR  SET SUBIEN=$ORDER(^TMG(22708,IEN,1,SUBIEN)) QUIT:(+SUBIEN'>0)  DO
        . NEW ZN SET ZN=$GET(^TMG(22708,IEN,1,SUBIEN,0))
        . NEW DISPTEXT SET DISPTEXT=$$TRIM^XLFSTR($PIECE(ZN,"^",7))        
        . SET OUT(SUBIEN)=$$PARSEPRM(DISPTEXT) ;"PARSE TEXT PARAMETERS
        . NEW LABELNAME SET LABELNAME=$PIECE(ZN,"^",1)
        . SET OUT(SUBIEN,.01)=LABELNAME
        QUIT
        ;        
PARSEPRM(STR) ;"PARSE TEXT PARAMETERS
        ;"Input: STR -- Format is '{Command: param, param, param ... }
        ;"Output: Command^Param1^Param2^Param3...
        NEW TMGRESULT SET TMGRESULT=""
        IF $EXTRACT(STR,1)'="{" GOTO PPMDN
        SET STR=$PIECE($PIECE($GET(STR),"{",2,999),"}",1)  ;"strip { } 's
        NEW TAG SET TAG=$$TRIM^XLFSTR($PIECE(STR,":",1))
        SET TMGRESULT=TAG
        NEW TEMP SET TEMP=$$TRIM^XLFSTR($PIECE(STR,":",2,999))
        NEW IDX SET IDX=0
        FOR IDX=1:1:$LENGTH(TEMP,",") DO
        . NEW PARAM SET PARAM=$$TRIM^XLFSTR($PIECE(TEMP,",",IDX))
        . IF PARAM="" QUIT
        . SET TMGRESULT=TMGRESULT_"^"_PARAM
PPMDN   QUIT TMGRESULT
        ;
RXMATCH(IEN,MEDLINE,ONLYRELATED) ;"CHECK MEDICATION FOR MATCH
        ;"Input: IEN= IEN in 22708
        ;"        MEDLINE -- one line from medication table (includes directions etc)
        NEW RESULT SET RESULT=0
        SET ONLYRELATED=+$G(ONLYRELATED)  ;"3/17/20
        SET MEDLINE=$$UP^XLFSTR($GET(MEDLINE))
        NEW SUBIEN SET SUBIEN=0
        FOR  SET SUBIEN=$ORDER(^TMG(22708,IEN,3,SUBIEN)) QUIT:(+SUBIEN'>0)!(RESULT>0)  DO
        . NEW AMED SET AMED=$$UP^XLFSTR(^TMG(22708,IEN,3,SUBIEN,0))
        . SET RESULT=($$UP^XLFSTR(MEDLINE)[AMED)  ;"added $$UP here. Some MEDLINES were camelcase  3/26/19
        ;"CHECK ASSOCIATED MEDS NOW  3/17/20
        IF ONLYRELATED=1 GOTO RXDN
        IF RESULT>0 GOTO RXDN
        NEW SUBIEN SET SUBIEN=0
        FOR  SET SUBIEN=$ORDER(^TMG(22708,IEN,6,SUBIEN)) QUIT:(+SUBIEN'>0)!(RESULT>0)  DO
        . NEW AMED SET AMED=$$UP^XLFSTR(^TMG(22708,IEN,6,SUBIEN,0))
        . SET RESULT=($$UP^XLFSTR(MEDLINE)[AMED)  ;"added $$UP here. Some MEDLINES were camelcase 
RXDN
        QUIT RESULT
        ;
GETLABS(TMGDFN,LABELNAME,PARAMS,SHOWNULL,LIMITNUM,OUTARR,OPTION)  ;"GET LABS
        ;"Input: TMGDFN -- PATIENT IEN
        ;"       LABELNAME : print name to display
        ;"       PARAMS : '<CMD>^Lab_name^Show_Dates
        ;"             Show_Dates : 1 means to show date, anything ELSE means don't show
        ;"       SHOWNULL: 1 to show label, even IF not values found. 
        ;"       LIMITNUM: Number of lab values to return.
        ;"       OUTARR -- OPTIONAL.  PASS BY REFERENCE to get back array of data.
        ;"                   OUTARR("KEY-VALUE",<LABEL>)=<VALUE>
        ;"                   OUTARR("KEY-VALUE",<LABEL>,"LINE")=<full text of line>
        ;"                   OUTARR(#)=<Line text>     <-- for entries that are not KEY-VALUE format
        ;"        OPTION -- OPTIONAL.  PASS BY REFERENCE.  Format:
        ;"                   OPTION("DT")=FMDT <-- will return info AS OF specified FM date-time
        ;"                   OPTION("CODE")=<CODE>   OPTIONAL, TO BE IMPLEMENTED (SEE GETPRIOR)
        ;"Result: returns string to show, or "" IF problem.
        ;"NOTE: Uses OPTION variable in global scope
        ;"NOTE: Specified lab should NOT be a panel.  If so, then only ONE result
        ;"      from panel will be returned. (could be changed later IF wanted).
        NEW TMGRESULT SET TMGRESULT=""
        NEW OPTION,PTID,LABSARR
        SET SHOWNULL=+$GET(SHOWNULL)
        SET PTID=TMGDFN_"^2"
        NEW LAB SET LAB=$PIECE(PARAMS,"^",2)
        NEW SHOWDT SET SHOWDT=$PIECE(PARAMS,"^",3)
        NEW EDT SET EDT=+$GET(OPTION("DT")) IF EDT>0 SET OPTION("EDT")=EDT
        SET OPTION("MAX CT")=LIMITNUM
        SET OPTION("SHOW DATE")=(SHOWDT=1)
        SET OPTION("SHOW NULL")=SHOWNULL
        SET OPTION("HIDE DUPLICATES")=1     ;3/2/18
        DO GFRMTLAB^TMGLRR01(PTID,LAB,.LABSARR,.OPTION)
        ;"NOTE: the line below causes problems if lab is actually a panel.  E.g. ask for CBC, and get back Hgb value
        ;"E.g. Asked for HgbA1c, but 'Other Glucose' was component test.  That glucose was returned, not HgbA1c
        ;"Fix later...
        NEW ONELAB SET ONELAB=$ORDER(LABSARR(""))  ;"get only first returned in array
        NEW STRVAL SET STRVAL=$GET(LABSARR(ONELAB))        
        ;"IF ONELAB'="",(SHOWNULL=1)!(STRVAL'="") SET TMGRESULT=LABELNAME_" = "_STRVAL
        IF (SHOWNULL=1)!(STRVAL'="") DO
        . SET TMGRESULT=LABELNAME_" = "_STRVAL
        . ;"SET OUTARR("KEY-VALUE",NAME)=STRVAL
        . ;"SET OUTARR("KEY-VALUE",NAME,"LINE")=TMGRESULT
        . SET OUTARR("KEY-VALUE",LABELNAME)=STRVAL
        . SET OUTARR("KEY-VALUE",LABELNAME,"LINE")=TMGRESULT
        . NEW ALINE SET ALINE=LABELNAME_" = "_STRVAL  ;"//kt 10/15
        . NEW LN SET LN=+$ORDER(OUTARR("@"),-1)+1      ;"//kt 10/15
        . SET OUTARR(LN)=ALINE                        ;"//kt 10/15
        QUIT TMGRESULT
        ;
GETLABDT(TMGDFN,LABELNAME,PARAMS,SHOWNULL,LIMITNUM,OUTARR,OPTION)  ;"GET LAB DATES
        ;"Input: TMGDFN -- PATIENT IEN
        ;"       LABELNAME : print name to display
        ;"       PARAMS : '<CMD>^Lab_name
        ;"       SHOWNULL: 1 to show label, even IF not values found. 
        ;"       LIMITNUM: Number of lab values to return.
        ;"       OUTARR -- OPTIONAL.  PASS BY REFERENCE to get back array of data.
        ;"                   OUTARR("KEY-VALUE",<LABEL>)=<VALUE>
        ;"                   OUTARR("KEY-VALUE",<LABEL>,"LINE")=<full text of line>
        ;"                   OUTARR(#)=<Line text>     <-- for entries that are not KEY-VALUE format
        ;"        OPTION -- OPTIONAL.  PASS BY REFERENCE.  Format:
        ;"                   OPTION("DT")=FMDT <-- will return info AS OF specified FM date-time
        ;"                   OPTION("CODE")=<CODE>   OPTIONAL, TO BE IMPLEMENTED (SEE GETPRIOR)
        ;"Result: returns string to show, or "" IF problem.
        ;"NOTE: Specified lab should NOT be a panel.  If so, then only ONE result
        ;"      from panel will be returned. (could be changed later IF wanted).
        NEW TMGRESULT SET TMGRESULT=""
        NEW OPTION,PTID,LABSARR
        SET DATESONLY=+$GET(DATESONLY)
        SET SHOWNULL=+$GET(SHOWNULL)
        SET PTID=TMGDFN_"^2"
        NEW LAB SET LAB=$PIECE(PARAMS,"^",2)
        NEW EDT SET EDT=+$GET(OPTION("DT")) IF EDT>0 SET OPTION("EDT")=EDT
        SET OPTION("MAX CT")=LIMITNUM
        SET OPTION("DATES ONLY")=1
        SET OPTION("HIDE DUPLICATES")=1  ;"3/2/18
        DO GFRMTLAB^TMGLRR01(PTID,LAB,.LABSARR,.OPTION)
        NEW ONELAB SET ONELAB=$ORDER(LABSARR(""))  ;"get only first returned in array
        NEW STRVAL SET STRVAL=$GET(LABSARR(ONELAB))        
        IF (SHOWNULL=1)!(STRVAL'="") DO
        . NEW ALINE SET ALINE=LABELNAME_" : "_STRVAL  ;"//kt 10/15
        . SET TMGRESULT=ALINE
        . ;"SET OUTARR("KEY-VALUE",NAME)=STRVAL
        . ;"SET OUTARR("KEY-VALUE",NAME,"LINE")=TMGRESULT
        . SET OUTARR("KEY-VALUE",LABELNAME)=STRVAL
        . SET OUTARR("KEY-VALUE",LABELNAME,"LINE")=TMGRESULT
        . NEW LN SET LN=+$ORDER(OUTARR("@"),-1)+1      ;"//kt 10/15
        . SET OUTARR(LN)=ALINE                        ;"//kt 10/15
        QUIT TMGRESULT
        ;
GETMEDS(TMGDFN,LABEL,IEN,PARAMS,SHOWNULL,TABLEDEFS,OUTARR,OPTION) ;
        ;"Purpose: GET PRIOR VALUES FROM PRIOR TABLE.
        ;"Input: TMGDFN -- PATIENT IEN
        ;"       LABEL -- Root key name, e.g. 'Medication' 
        ;"       IEN -- IEN IN 22708
        ;"       PARAMS : (currently unused) 
        ;"       SHOWNULL: 1 to show label, even if no values found. 
        ;"       TABLEDEFS -- PASS BY REFERENCE.  AN IN AND OUT parameter. Format:  
        ;"           TABLEDEFS(SUBIEN)=CMD^PARAM^PARAM^PARAM...
        ;"       OUTARR -- OPTIONAL.  PASS BY REFERENCE to get back array of data.
        ;"                   OUTARR("KEY-VALUE",<LABEL>)=<VALUE>
        ;"                   OUTARR("KEY-VALUE",<LABEL>,"LINE")=<full text of line>
        ;"                   OUTARR(#)=<Line text>     <-- for entries that are not KEY-VALUE format
        ;"        OPTION -- OPTIONAL.  PASS BY REFERENCE.  Format:
        ;"                   OPTION("DT")=FMDT <-- will return info AS OF specified FM date-time
        ;"                   OPTION("CODE")=<CODE>   OPTIONAL, TO BE IMPLEMENTED (SEE GETPRIOR)
        ;"Result: returns string to show, or "" if problem.
        ;"NOTE: If multiple lines need to be returned, then
        ;"      lines can be separated by CR (#13)
        ;"Result : returns one line, that can be added to table.
        ;"        Format is   'Label : Value'
        NEW TMGRESULT SET TMGRESULT=""
        NEW MEDS DO GTLKMDARR(.MEDS,TMGDFN,IEN,.OPTION)
        NEW CT SET CT=1
        NEW AMED SET AMED="" 
        FOR  DO  QUIT:(AMED="")
        . SET AMED=$ORDER(MEDS(AMED))
        . IF AMED="",$GET(SHOWNULL)'=1 QUIT
        . NEW ALABEL SET ALABEL=LABEL_"-"_CT,CT=CT+1
        . NEW ALINE SET ALINE=ALABEL_" = "_AMED
        . SET OUTARR("KEY-VALUE",ALABEL)=AMED
        . SET OUTARR("KEY-VALUE",ALABEL,"LINE")=ALINE
        . IF TMGRESULT'="" SET TMGRESULT=TMGRESULT_$CHAR(13)
        . SET TMGRESULT=TMGRESULT_ALINE
        . NEW LN SET LN=+$ORDER(OUTARR("@"),-1)+1   
        . SET OUTARR(LN)=ALINE                      
        QUIT TMGRESULT
        ;
GTMEDLST(RESULT,TMGDFN,ARRAY,DT,OPTION)  ;"Purpose: RPC (TMG GET MED LIST) to return a patient's med list
        ;"Input RESULT -- a string comprising medlist
        ;"      TMGDFN
        ;"      ARRAY is optional.  Supply to get back array of table. Not used by RPC call.
        ;"      DT -- OPTIONAL.  If supplied, then get med list AS OF the specified FM DT
        ;"Result: none, but output in RESULT variable.  
        IF $DATA(DT)#10 SET OPTION("DT")=DT
        SET RESULT=$$GETTABLX^TMGTIUO6(TMGDFN,"MEDICATIONS",.ARRAY,.OPTION)
        SET RESULT=$$REMHTML^TMGHTM1(RESULT)   ;"ELH ADDED NEW FUNCTION 3/15/18 TO REMOVE {HTML: TAGS
        SET RESULT=$$HTML2TXS^TMGHTM1(RESULT)
        ;"not using below at the moment.
        ;"SET RESULT=$$RPLCMEDS^TMGTIUOT(RESULT)  ;"ELH ADDED AS A WEDGE TO REPLACE MED NAMES AS NEEDED 3/22/18
        QUIT
        ;               
GTLKMDARR(OUT,TMGDFN,IEN,OPTION)  ;"GET LINKED MEDS ARRAY
        ;"Purpose: returns array with all medication line items found in
        ;"         MEDICATIONS table that are also noted in RELATED MEDICATIONS field,
        ;"         or linked by drug CLASS(es) from table
        ;"Input:  OUT.  PASS BY REFERENCE.  AN OUT PARAMETER.  Format:
        ;"            OUT(<medication line entry)=""
        ;"        TMGDFN -- patient IEN
        ;"        IEN -- IEN IN 22708
        ;"        OPTION -- OPTIONAL.  PASS BY REFERENCE.  Format:
        ;"            OPTION("DT")=FMDT <-- will return info AS OF specified FM date-time
        ;"            OPTION("ALL NOTES")=0 OR 1 <-- to use completed notes only or all
        ;"            OPTION("USEOLDMETHOD")=1 if old method wanted.  
        ;"            OPTION("ONLYRELATED")=1 if only related and not associated
        ;"Result: none. 
        NEW USEOLDMETHOD SET USEOLDMETHOD=+$GET(OPTION("USEOLDMETHOD"))  ;"//kt 5/6/18
        NEW STORENAME SET STORENAME="MEDICATIONS LINKED-TO-TABLE-"_IEN_" FOR DFN:"_TMGDFN
        NEW REF SET REF=$$GETMPREF^TMGMISC2(STORENAME)        
        NEW PRIORTIME SET PRIORTIME=+$GET(@REF@("D","HTIME"))
        NEW DELTASEC SET DELTASEC=$$HDIFF^XLFDT($H,PRIORTIME,2)  ;"returns seconds 
        NEW ONLYRELATED SET ONLYRELATED=+$GET(OPTION("ONLYRELATED"))   ;"3/17/20
        IF DELTASEC<120 DO
        . MERGE OUT=@REF@("D","TABLE")
        ELSE  DO           
        . NEW MEDTABL  
        . ;"//kt 5/11/18 NEW TEMPOPT MERGE TEMPOPT("DT")=OPTION("DT")
        . NEW TEMPOPT MERGE TEMPOPT=OPTION  ;"//kt 5/11/18
        . SET TEMPOPT("ALL NOTES")=1  ;"elh added this to force med list to include non-signed notes 4/10/18
        . DO TIMEPFIL^TMGMISC2("GETITEM^TMGTIUO8","GTLKMDARR^TMGTIUO8 CALLING GETTABLX",1) ;"RECORD START TIME
        . IF USEOLDMETHOD DO
        . . NEW TEMP SET TEMP=$$GETTABLX^TMGTIUO6(TMGDFN,"MEDICATIONS",.MEDTABL,.TEMPOPT)  ;"//kt 5/6/18
        . ELSE  DO
        . . DO PRIORRXT(TMGDFN,48,.MEDTABL,1,.TEMPOPT)    ;"//kt 5/6/18                                          ;"//kt 5/6/18                
        . DO TIMEPFIL^TMGMISC2("GETITEM^TMGTIUO8","GTLKMDARR^TMGTIUO8 CALLING GETTABLX",0) ;"RECORD STOP TIME
        . ;"------------------------------------------------------
        . ;"First, add linked medicines based on linked drug *classes* 
        . NEW CLASSES,SUBIEN SET SUBIEN=0
        . FOR  SET SUBIEN=$ORDER(^TMG(22708,IEN,5,SUBIEN)) QUIT:SUBIEN'>0  DO
        . . NEW IEN50D605 SET IEN50D605=$PIECE($GET(^TMG(22708,IEN,5,SUBIEN,0)),"^",1)
        . . SET CLASSES(IEN50D605)=""
        . DO TIMEPFIL^TMGMISC2("GETITEM^TMGTIUO8","GTLKMDARR^TMGTIUO8 CALLING GETCLARX",1) ;"RECORD START TIME
        . NEW CLASSMEDS DO GETCLARX^TMGRX006(.CLASSMEDS,.CLASSES,TMGDFN,.OPTION)  ;"CLASSMEDS("LINE",<original Rx table line>,IEN50.605)=""        
        . DO TIMEPFIL^TMGMISC2("GETITEM^TMGTIUO8","GTLKMDARR^TMGTIUO8 CALLING GETCLARX",0) ;"RECORD STOP TIME
        . NEW ALINE SET ALINE=""
        . FOR  SET ALINE=$ORDER(CLASSMEDS("LINE",ALINE)) QUIT:ALINE=""  DO
        . . NEW TEMP SET TEMP=$$REMTAGS(ALINE)
        . . IF $$EXCUDMED(IEN,TEMP)=1 QUIT
        . . SET OUT(TEMP)=""
        . ;"Next, add meds that match with Associated Meds tied to table.  
        . NEW IDX SET IDX=0
        . FOR  SET IDX=$ORDER(MEDTABL(IDX)) QUIT:+IDX'>0  DO
        . . NEW ALINE SET ALINE=$GET(MEDTABL(IDX)) QUIT:ALINE=""
        . . NEW MATCH SET MATCH=$$RXMATCH(IEN,ALINE,ONLYRELATED) QUIT:'MATCH
        . . SET ALINE=$$REMTAGS(ALINE)
        . . IF $$EXCUDMED(IEN,ALINE)=1 QUIT
        . . SET OUT(ALINE)=""        
        . NEW ARR,H SET H=$H MERGE ARR("HTIME")=H 
        . MERGE ARR("TABLE")=OUT              
        . DO TMPSAVE^TMGMISC2(.ARR,STORENAME,"5M") ;"Save for at least 5 minutes        
        QUIT
        ;       
REMTAGS(ALINE)  ;" REMOVE SPECIAL TAGS FROM MEDICATIONS
        IF ALINE["[" DO
        . SET ALINE=$P(ALINE,"[HOSP",1)
        . SET ALINE=$P(ALINE,"[Auto",1)
        QUIT ALINE
        ;"
EXCUDMED(IEN,ALINE)  ;"
        ;"CHECK TO SEE IF MED IS SET FOR EXCLUSION
        NEW TMGEXCLUDE SET TMGEXCLUDE=0
        NEW EXIEN SET EXIEN=0
        FOR  SET EXIEN=$O(^TMG(22708,IEN,7,EXIEN)) QUIT:(EXIEN'>0)!(TMGEXCLUDE=1)  DO
        . NEW EXMED SET EXMED=$$UP^XLFSTR($P($G(^TMG(22708,IEN,7,EXIEN,0)),"^",1))
        . IF $$UP^XLFSTR(ALINE)[EXMED SET TMGEXCLUDE=1        
        QUIT TMGEXCLUDE
        ;"
GETPRIOR(TMGDFN,NEWLABEL,IEN,TABLENAME,PARAMS,SHOWNULL,PRIORTABLE,TABLEDEFS,OUTARR,OPTION) ;
        ;"Purpose: GET PRIOR VALUES FROM PRIOR TABLE.
        ;"Input: TMGDFN -- PATIENT IEN
        ;"       NEWLABEL -- 
        ;"       IEN -- IEN IN 22708
        ;"       TABLENAME -- Name of prior table to find.
        ;"       PARAMS : <CMD>, or 
        ;"                <CMD>^Prior_Label_name, or 
        ;"                <CMD>^<TABLE_NAME>|Prior_Label_name, or 
        ;"                <CMD>^!MISC! 
        ;"              If "!MISC!" passed, then return all lines not otherwise defined.
        ;"              If Prior_Label_Name is not passed then NEWLABEL is used.
        ;"       SHOWNULL: 1 to show label, even IF not values found. 
        ;"       PRIORTABLE -- PASS BY REFERENCE.  AN IN AND OUT parameter.  Array of prior table.    
        ;"       TABLEDEFS -- PASS BY REFERENCE.  AN IN AND OUT parameter. Format:  
        ;"           TABLEDEFS(SUBIEN)=CMD^PARAM^PARAM^PARAM...
        ;"       OUTARR -- OPTIONAL.  PASS BY REFERENCE to get back array of data.
        ;"                   OUTARR("KEY-VALUE",<LABEL>)=<VALUE>
        ;"                   OUTARR("KEY-VALUE",<LABEL>,"LINE")=<full text of line>
        ;"                   OUTARR(#)=<Line text>     <-- for entries that are not KEY-VALUE format
        ;"       OPTION -- OPTIONAL.  PASS BY REFERENCE.  Format:
        ;"                   OPTION("DT")=FMDT <-- will return info AS OF specified FM date-time
        ;"                   OPTION("ALL NOTES")=0 OR 1 <-- to search all notes or just completed ones
        ;"                   OPTION("CODE")=<CODE>   OPTIONAL, TO BE IMPLEMENTED (SEE GETPRIOR)
        ;"Result: returns string to show, or "" IF problem.
        ;"NOTE: IF multiple lines need to be returned, then
        ;"      lines can be separated by CR (#13)
        SET CODE=$GET(OPTION("CODE"))
        NEW STRVAL SET STRVAL=""
        NEW TMGRESULT SET TMGRESULT=""
        NEW ERROR SET ERROR=0
        NEW WARN SET WARN=+$G(OPTION("WARN"))
        NEW LINKSYMBOL SET LINKSYMBOL=":"
        SET SHOWNULL=+$GET(SHOWNULL)
        NEW PRIORLABEL SET PRIORLABEL=$PIECE(PARAMS,"^",2)
        IF PRIORLABEL["|" DO
        . SET TABLENAME=$PIECE(PRIORLABEL,"|",1)
        . SET PRIORLABEL=$PIECE(PRIORLABEL,"|",2)
        . SET LINKSYMBOL="="
        IF PRIORLABEL="" SET PRIORLABEL=NEWLABEL 
        IF $DATA(PRIORTABLE(TABLENAME))=0 DO
        . NEW TEMPARR
        . IF TABLENAME["MEDICATIONS" DO
        . . DO PRIORRXT(TMGDFN,48,.TEMPARR,1,.OPTION)  ;"48 months; 1 = only last table
        . ELSE  DO
        . . DO GETSPECL^TMGTIUO4(TMGDFN,TABLENAME,"BLANK_LINE",48,.TEMPARR,1)  ;"48 months; 1 = only last table ELH added OPTION parameter  4/10/18
        . NEW KEY SET KEY=""
        . FOR  SET KEY=$ORDER(TEMPARR("KEY-VALUE",KEY)) QUIT:(KEY="")  DO
        . . IF (KEY?1"MEDICATION-"1.N) KILL TEMPARR("KEY-VALUE",KEY)
        . . IF (KEY?1"MEDS-"1.N) KILL TEMPARR("KEY-VALUE",KEY)  ;"Added 3/2/20
        . . IF KEY["***" KILL TEMPARR("KEY-VALUE",KEY)
        . MERGE PRIORTABLE(TABLENAME)=TEMPARR
        . KILL PRIORTABLE(TABLENAME,"KEY-VALUE","SOURCE-DATE")
        . IF $DATA(PRIORTABLE(TABLENAME))=0 SET PRIORTABLE(TABLENAME,"EMPTY")=1
        IF PRIORLABEL="!MISC!" DO
        . ;"First, remove all from prior table that are already defined in this table. 
        . NEW SUBIEN SET SUBIEN=0 
        . FOR  SET SUBIEN=$ORDER(TABLEDEFS(SUBIEN)) QUIT:(SUBIEN'>0)!(ERROR>0)  DO
        . . NEW ONEPARAM SET ONEPARAM=$GET(TABLEDEFS(SUBIEN))
        . . NEW LINENAME SET LINENAME=$GET(TABLEDEFS(SUBIEN,.01))
        . . NEW CMD SET CMD=$$UP^XLFSTR($PIECE(ONEPARAM,"^",1)) 
        . . NEW LABEL SET LABEL=""
        . . IF CMD="",LINENAME'="" SET CMD="HIDE"
        . . QUIT:CMD=""
        . . IF CMD="PRIOR" DO  
        . . . SET LABEL=$PIECE(ONEPARAM,"^",2)
        . . . IF LABEL["|" SET LABEL=$PIECE(LABEL,"|",2)
        . . . IF LABEL="" SET LABEL=$GET(TABLEDEFS(SUBIEN,.01))
        . . ELSE  IF "LAB DATES,HIDE,CODE"[CMD DO  
        . . . SET LABEL=LINENAME        
        . . IF LABEL'="" DO
        . . . NEW TMPO SET TMPO="OLD TABLE (EDIT)-->"    ;"also in LABCMTBL^TMGTIUO8
        . . . NEW TEMPREF SET TEMPREF=$NAME(PRIORTABLE(TABLENAME,"KEY-VALUE"))
        . . . IF LABEL[TMPO QUIT  ;"//leave in old info.  Only happens first time this NEW table is used
        . . . NEW UPLABEL SET UPLABEL=$$UP^XLFSTR(LABEL)
        . . . KILL PRIORTABLE(TABLENAME,"KEY-VALUE",$$UP^XLFSTR(LABEL))
        . . . ;"ELH note: 6/25/20. The above line kills existing entries, however it is using the Label as provided in the DISPLAY TEXT ITEMS. 
        . . . ;"   This works if the LINENAME matches the LABEL, but if it doesn't you can get duplicate values. For example,
        . . . ;"   PAIN MANAGEMENT table with item "Date of Last Pain Contract", pulls "{PRIOR:MEDICATIONS|*CSM Contract}". So when the
        . . . ;"   above kills the entry... it kills *CSM CONTRACT, not Date of Last Pain Contract. This leaves the Date of Last Pain Contract
        . . . ;"   in PRIORTABLE and then duplicates it moving forward. I am adding the line below to alleviate this. I wonder if the above
        . . . ;"   KILL even needs to be there. Will leave for now but the below KILL may work by itself
        . . . KILL PRIORTABLE(TABLENAME,"KEY-VALUE",$$UP^XLFSTR(LINENAME))
        . IF ERROR>0 SET RESULT=RESULT_$CHAR(13,10)_"ERROR: "_$PIECE(ERROR,"^",2) QUIT
        . ;"Next collect remaining entries, in KEY-VALUE form
        . NEW PRIORFOUNDLABEL SET PRIORFOUNDLABEL=""
        . FOR  SET PRIORFOUNDLABEL=$ORDER(PRIORTABLE(TABLENAME,"KEY-VALUE",PRIORFOUNDLABEL)) QUIT:PRIORFOUNDLABEL=""  DO
        . . NEW ALINE SET ALINE=$GET(PRIORTABLE(TABLENAME,"KEY-VALUE",PRIORFOUNDLABEL,"LINE")) QUIT:ALINE=""
        . . MERGE OUTARR(TABLENAME,"KEYVALUE")=PRIORTABLE(TABLENAME,"KEYVALUE",PRIORFOUNDLABEL)
        . . IF TMGRESULT'="" SET TMGRESULT=TMGRESULT_$CHAR(13)
        . . SET TMGRESULT=TMGRESULT_ALINE
        . . NEW LN SET LN=+$ORDER(OUTARR("@"),-1)+1  
        . . SET OUTARR(LN)=ALINE                      
        . ;"Next collect remaining entries, NOT in KEY-VALUE form
        . NEW IDX SET IDX=0
        . FOR  SET IDX=$ORDER(PRIORTABLE(TABLENAME,IDX)) QUIT:+IDX'>0  DO
        . . NEW ALINE SET ALINE=$GET(PRIORTABLE(TABLENAME,IDX)) QUIT:ALINE=""
        . . NEW I2 SET I2=+$ORDER(OUTARR("@"),-1)+1
        . . SET OUTARR(I2)=ALINE
        . . IF TMGRESULT'="" SET TMGRESULT=TMGRESULT_$CHAR(13)
        . . ;"MOVED TO TMGTIUO6  SET ALINE=$$WRAPMISC(ALINE)   ;"4/11/22
        . . SET TMGRESULT=TMGRESULT_ALINE
        . ;"NOTE: 6/23/20... PRIORTABLE ARRAY IS BEING ALTERED INSIDE THIS
        . ;"IF AND ULTIMATELY ENDED UP BEING A PARTAL TABLE. KILLING OFF SO IT IS
        . ;"REFRESHED NEXT ITEM
        . KILL PRIORTABLE(TABLENAME)
        ELSE  DO
        . SET PRIORLABEL=$$UP^XLFSTR(PRIORLABEL)
        . ;"ELH ADDING THE BELOW LINE TO ALTER PRIORLABEL
        . IF PRIORLABEL["@+@" DO ALTERLBL(TABLENAME,.PRIORTABLE,.PRIORLABEL)
        . SET STRVAL=$GET(PRIORTABLE(TABLENAME,"KEY-VALUE",PRIORLABEL))
        . IF STRVAL="",SHOWNULL'=1 QUIT
        . ELSE  DO
        . . IF TMGRESULT'="" SET TMGRESULT=TMGRESULT_$CHAR(13)
        . . IF CODE'="" DO     ;"ELH ADDED TO EXECUTE VALUE MODIFYING CODE
        . . . NEW TMGX,TMGY
        . . . ;"//kt 3/24/21  NEW TMGDFN
        . . . SET TMGX=STRVAL
        . . . ;"//kt 3/24/21  SET TMGDFN=DFN
        . . . XECUTE CODE
        . . . SET TMGY=$GET(TMGY)
        . . . IF TMGY'="" SET STRVAL=TMGY
        . . IF (WARN=1)&(STRVAL="") SET STRVAL=$$WARNTEXT^TMGTIUOT()
        . . NEW ALINE SET ALINE=NEWLABEL_" "_LINKSYMBOL_" "_STRVAL
        . . SET OUTARR("KEY-VALUE",NEWLABEL)=STRVAL        ;"//kt 10/15
        . . SET OUTARR("KEY-VALUE",NEWLABEL,"LINE")=ALINE  ;"//kt 10/15
        . . NEW LN SET LN=+$ORDER(OUTARR("@"),-1)+1        ;"//kt 10/15
        . . SET OUTARR(LN)=ALINE                           ;"//kt 10/15        
        . . SET TMGRESULT=TMGRESULT_ALINE
        . IF STRVAL'=$GET(PRIORTABLE(TABLENAME,"KEY-VALUE",PRIORLABEL)) DO
        . . SET OUTARR("KEY-VALUE",PRIORLABEL)=STRVAL
        . . SET OUTARR("KEY-VALUE",PRIORLABEL,"LINE")=TMGRESULT
        . ELSE  DO
        . . MERGE OUTARR("KEY-VALUE",PRIORLABEL)=PRIORTABLE(TABLENAME,"KEY-VALUE",PRIORLABEL)
GPRDN   QUIT TMGRESULT
        ;
WRAPMISC(LINE)  ;"THIS ROUTINE WILL TAKE A LINE FORWARDED AS A MISC LINE OF THE TABLE AND DETERMINE IF  
                ;" ACTION IS REQUIRED ON IT. IF IT IS, IT WILL BE HIGHLIGHTED IN RED AND A TAG WILL
                ;" BE ADDED SO THE NOTE CANNOT BE SIGNED WITHOUT IT BEING ADDRESSED
                ;"MOVED TO TMGTIUO6. LEAVING FOR NOW FOR REFERENCE
        NEW TAGS SET TAGS="{REQ}^{REQUIRED}^{HI}^{HIGH}"
        NEW I,REQUIRED
        SET REQUIRED=0
        FOR I=1:1:4  DO
        . NEW TAG SET TAG=$P(TAGS,"^",I)
        . IF $$UP^XLFSTR(LINE)[TAG SET REQUIRED=1
        IF REQUIRED=1 DO
        . SET LINE=$$HTML2TXS^TMGHTM1(LINE)
        . SET LINE=LINE_" [^^!REQUIRED FOLLOWUP ITEM!^^]"
        . NEW OPTION ;"SET OPTION("DIRECT HTML INSERTION")=1
        . SET LINE=$$WRAPTEXT^TMGTIUOT(LINE,"#ff0000",.OPTION)
        . SET HTML=0  ;"HTML ON SYSTEM TABLE SHOULD NOW BE OFF
        QUIT LINE
        ;"
ALTERLBL(TABLENAME,PRIORTABLE,PRIORLABEL)
        ;"THIS FUNCTION ALTERS PRIORLABEL THAT HAS A PREFIX ONLY (DENOTED BY A +)
        ;"SO, FOR EXAMPLE, IF PRIORLABEL IS "UROLOG+" THIS FUNCTION
        ;"WILL SEARCH PRIORTABLE FOR AN ENTRY THAT STARTS WITH "UROLOG" 
        ;"AND WOULD BE SET TO UROLOGIST OR UROLOGY, DEPENDING ON WHAT IS FOUND IN THE 
        ;"KEY-VALUEs. ONE ISSUE WOULD BE IF MULTIPLE ENTRIES MATCHED. THIS WOULD
        ;"JUST RETURN THE LAST ONE FOUND, ALPHABETICALLY
        SET PRIORLABEL=$P(PRIORLABEL,"@+@",1)
        NEW LABEL SET LABEL=""
        NEW FOUND SET FOUND=0
        FOR  SET LABEL=$O(PRIORTABLE(TABLENAME,"KEY-VALUE",LABEL)) QUIT:(LABEL="")!(FOUND=1)  DO
        . IF LABEL[PRIORLABEL DO
        . . SET PRIORLABEL=LABEL
        . . SET FOUND=1
        QUIT
        ;"
PRIORRXT(TMGDFN,MONTHS,OUT,MODE,OPTION)  ;"GET PRIOR MEDICATIONS (RX) TABLE
        ;"INPUT: TMGDFN -- patient IEN
        ;"       MONTHS -- number of months to search back
        ;"       OUT -- PASS BY REFERENCE, AND OUT PARAMETER.  Format:
        ;"           OUT(0)=<line count>
        ;"           OUT(#)=<one line from medication table>   <--- Doesn't include [MEDICATIONS] etc table name      
        ;"           OUT("KEY-VALUE",<KEY>)=<VALUE>  <-- KEY and VALUE are separated by "=", <KEY> is UPPERCASE
        ;"           OUT("KEY-VALUE",<KEY>,"LINE")=<original line from medication table)
        ;"           OUT("KEY-VALUE","SOURCE-DATE")=FMDT
        ;"       MODE -- 1 = only last table
        ;"       OPTION -- OPTIONAL.  PASS BY REFERENCE.  Format:
        ;"                   OPTION("DT")=FMDT <-- will return info AS OF specified FM date-time
        ;"                   OPTION("USEOLDMETHOD")) <--- change to alter flow default: NEW VS OLD.
        ;"//kt 5/6/18 -- Modifying to leave medication table as global in variable table for speed.
        ;"              This function was taking about 1 sec to run otherwise
        ;"              Leaves TMGGSMEDLIST on variable stack
        NEW USEOLDMETHOD SET USEOLDMETHOD=+$GET(OPTION("USEOLDMETHOD"))  ;"<--- change to alter flow default: NEW VS OLD.
        SET MODE=+$GET(MODE)
        NEW NOW SET NOW=$$NOW^XLFDT
        IF $DATA(TMGGSMEDLIST) DO           
        . NEW ADFN SET ADFN=0 FOR  SET ADFN=$ORDER(TMGGSMEDLIST(ADFN)) QUIT:ADFN'>0  DO
        . . IF ADFN'=TMGDFN KILL TMGGSMEDLIST(ADFN)  ;"kill off any variables from other patients
        NEW MEDTABL MERGE MEDTABL=TMGGSMEDLIST(TMGDFN)  ;"GLOBAL VARIABLE SCOPE MED LIST
        IF $DATA(MEDTABL) DO
        . NEW PRIORTIME SET PRIORTIME=+$GET(MEDTABL("RETRIEVAL-DATE")) KILL MEDTABL("RETRIEVAL-DATE")        
        . NEW DELTASEC SET DELTASEC=$$FMDIFF^XLFDT(NOW,PRIORTIME,2) ;"returns seconds
        . IF DELTASEC>120 KILL MEDTABL
        IF $DATA(MEDTABL) DO  GOTO PRRXTDN 
        . MERGE OUT=MEDTABL
        ;"-------------------------------------------------        
        NEW TEMP,DT SET DT=0         
        IF (USEOLDMETHOD=1)!(MODE'=1) DO  GOTO PRRXTSV
        . DO PRIORXT0(.TMGDFN,.MONTHS,.OUT,.MODE,.OPTION)
        ;"--------------------------------------------------
        IF 'USEOLDMETHOD,$GET(OPTION("DT"))>0 DO
        . IF OPTION("DT")\1=NOW\1 QUIT
        . DO GETRXTBL^TMGRX007(.TEMP,.DT,TMGDFN)                 ;"GET PRIOR MEDICATIONS (RX) TABLE from file 22733.2
        . IF DT>0,OPTION("DT")<DT SET USEOLDMETHOD=1
        ;"--------------------------------------------------
        IF $DATA(TEMP)=0 DO GETRXTBL^TMGRX007(.OUT,.DT,TMGDFN)  ;"GET PRIOR MEDICATIONS (RX) TABLE from file 22733.2
        ELSE  MERGE OUT=TEMP    ;"IF TEMP WAS POPULATED, IT NEVER WROTE BACK TO OUT AND MED TABLES WERE BEING LOST 3/26/19
        ;"NEW CT SET CT=0      
        ;"SET OUT("KEY-VALUE","SOURCE-DATE")=DT
        ;"IF $GET(OPTION("HEADER"))'="" SET CT=CT+1,OUT(CT)=OPTION("HEADER")
        ;"NEW IDX SET IDX=0
        ;"FOR  SET IDX=$ORDER(TEMP(IDX)) QUIT:IDX'>0  DO
        ;". NEW LINE SET LINE=$$TRIM^XLFSTR($GET(TEMP(IDX))) QUIT:LINE=""
        ;". IF LINE["=" DO
        ;". . NEW KEY SET KEY=$$UP^XLFSTR($$TRIM^XLFSTR($PIECE(LINE,"=",1)))
        ;". . NEW VALUE SET VALUE=$$TRIM^XLFSTR($PIECE(LINE,"=",2))
        ;". . SET OUT("KEY-VALUE",KEY)=VALUE
        ;". . SET OUT("KEY-VALUE",KEY,"LINE")=LINE
        ;". ELSE  DO  
        ;". . IF $EXTRACT(LINE,1)="*" QUIT
        ;". . SET CT=CT+1,OUT(CT)=LINE,OUT(0)=CT
PRRXTSV KILL TMGGSMEDLIST MERGE TMGGSMEDLIST(TMGDFN)=OUT
        SET TMGGSMEDLIST(TMGDFN,"RETRIEVAL-DATE")=NOW
PRRXTDN QUIT
        ;
PRIORXT0(TMGDFN,MONTHS,OUT,MODE,OPTION)  ;"GET PRIOR MEDICATIONS (RX) TABLE
        ;"INPUT: TMGDFN -- patient IEN
        ;"       MONTHS -- number of months to search back
        ;"       OUT -- PASS BY REFERENCE.  The output of this function
        ;"       MODE -- 1 = only last table
        ;"       OPTION -- OPTIONAL.  PASS BY REFERENCE.  Format:
        ;"                   OPTION("DT")=FMDT <-- will return info AS OF specified FM date-time
        ;"Results: none.  See OUT
        NEW STORENAME SET STORENAME="MEDICATIONS TABLE-"_TMGDFN
        NEW REF SET REF=$$GETMPREF^TMGMISC2(STORENAME)                
        NEW RXDT,FRXDT,ARRAY1,ARRAY2 KILL OUT
        NEW H SET H=+$GET(@REF@("D","HTIME"))        
        IF $$HDIFF^XLFDT($H,H,2)<120 DO  GOTO PRXTDN
        . MERGE OUT=@REF@("D")  ;"Used cached copy, if made in last 2 minutes.
        ;"Get both tables, and take the most recent one.
        ;"NOTE: Search for [FINAL MEDICATIONS] table in notes with COMPLETED status, but
        ;"   search for [MEDICATIONS] in ALL notes.  This will allow user to get
        ;"   information from MEDICATIONS table from a current note in progress.
        NEW TEMPOPT MERGE TEMPOPT("DT")=OPTION("DT")
        DO GETSPECL^TMGTIUO4(TMGDFN,"[FINAL MEDICATIONS]","BLANK_LINE",MONTHS,.ARRAY1,MODE,,.TEMPOPT)
        SET FRXDT=+$GET(ARRAY1("KEY-VALUE","SOURCE-DATE"))
        SET TEMPOPT("ALL NOTES")=1  ;"Include unsigned notes.
        DO GETSPECL^TMGTIUO4(TMGDFN,"[MEDICATIONS]","BLANK_LINE",MONTHS,.ARRAY2,MODE,,.TEMPOPT) 
        SET RXDT=$GET(ARRAY2("KEY-VALUE","SOURCE-DATE"))
        IF RXDT>FRXDT DO  ;"If MEDICATIONS table has been reconcilled since FINAL MEDICATIONS table has, then use it. 
        . MERGE OUT=ARRAY2
        ELSE  DO
        . MERGE OUT=ARRAY1
        DO EMEDSARR^TMGTIUO5(TMGDFN,.OUT)
        NEW IDX SET IDX=0 FOR  SET IDX=$ORDER(OUT(IDX)) QUIT:(+IDX'>0)  DO
        . SET OUT(IDX)=$$FIXABREV^TMGTIUO6($GET(OUT(IDX)))
        ;"KILL @REF MERGE @REF@("T")=OUT
        DO TMPSAVE^TMGMISC2(.OUT,STORENAME,"5M")  ;"save for 5 minutes
PRXTDN  ;        
        QUIT
        ;