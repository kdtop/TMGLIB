TMGTIUO8 ;TMG/kst-Text objects for use in CPRS ; 2/2/14 1/15/17
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
 ;"LABCMTBL(DFN,LABEL,OUTARR) ;"LAB & COMMENT TABLE 
 ;"
 ;"=======================================================================
 ;"PRIVATE FUNCTIONS
 ;"=======================================================================
 ;"
 ;"=======================================================================
 ;"Dependancies : TMGPXR02, TIUSRVD
 ;"=======================================================================
 ;
LABCMTBL(DFN,LABEL,OUTARR,OPTION) ;"LAB & COMMENT TABLE 
        ;"Input: DFN -- IEN in PATIENT file
        ;"       LABEL -- This is the .01 field of the table (File# 22708) to create
        ;"       OUTARR -- OPTIONAL.  PASS BY REFERENCE to get back array of data.
        ;"                 NOTE: doesn't include lines containing 'OLD TABLE (EDIT)-->' 
        ;"       OPTION -- OPTIONAL.
        ;"              OPTION("DT")=FMDT <-- if present, then table is to be returns AS OF given date
        ;"Result: returns string with embedded line-feeds to create text table.
        NEW TMGPRIORTABLE,TMGTABLEDEFS
        NEW TMGRESULT SET TMGRESULT=$$RMDGTABL^TMGPXR02(DFN,LABEL,"GETITEM^TMGTIUO8",.OUTARR,.OPTION)
        NEW TMPO SET TMPO="OLD TABLE (EDIT)-->"  ;"also in GETPRIOR^TMGTIUO8
        IF TMGRESULT'[TMPO GOTO LBCMTDN 
        ;"Now remove empty or redundant elements from OLD TABLE 
        NEW CHANGED SET CHANGED=0
        NEW TEMPARR DO SPLIT2AR^TMGSTUT2(TMGRESULT,$CHAR(13,10),.TEMPARR)
        NEW IDX
        ;"Set up a temp xref
        SET IDX="" FOR  SET IDX=$ORDER(TEMPARR(IDX)) QUIT:IDX'>0  DO
        . NEW STR SET STR=$GET(TEMPARR(IDX)) QUIT:STR=""
        . SET TEMPARR("B",STR,IDX)=""
        ;"Delete redundant lines
        SET IDX="" FOR  SET IDX=$ORDER(TEMPARR(IDX)) QUIT:IDX'>0  DO
        . NEW STR SET STR=$GET(TEMPARR(IDX))
        . IF STR'[TMPO QUIT
        . SET STR=$$TRIM^XLFSTR($PIECE(STR,TMPO,2))
        . IF (STR="")!(+$ORDER(TEMPARR("B",STR,0))>0) DO
        . . KILL TEMPARR(IDX)  ;"only kills redundant lines beginning with 'OLD TABLE (EDIT) -->'
        . . SET CHANGED=1
        ;"Reassemble final result if changes made. 
        IF CHANGED=1 DO
        . SET TMGRESULT=""
        . SET IDX="" FOR  SET IDX=$ORDER(TEMPARR(IDX)) QUIT:IDX'>0  DO
        . . NEW STR SET STR=$GET(TEMPARR(IDX))
        . . SET TMGRESULT=TMGRESULT_STR_$CHAR(13,10)
LBCMTDN QUIT TMGRESULT
        ;
INLNTABL(DFN,LABEL,OUTARR,OPTION) ;"INLINE TABLE
        NEW TMGPRIORTABLE,TMGTABLEDEFS
        SET OPTION("NO HEADER LINE")=1
        SET OPTION("NO LF")=1
        NEW TMGRESULT SET TMGRESULT=$$DOTABL^TMGPXR02(DFN,LABEL,"GETITEM^TMGTIUO8",.OUTARR,.OPTION)
        QUIT TMGRESULT
        ;
GETITEM(DFN,IEN,SUBIEN,MAXLEN,OUTARR,OPTION) ;"Get a table item.
        ;"Input: DFN -- IEN in PATIENT file
        ;"       IEN -- IEN IN 22708
        ;"       SUBIEN -- IEN IN 22708.01 (ITEM multiple)
        ;"       MAXLEN -- Number indicating max allowed length of line. 
        ;"       OUTARR -- OPTIONAL.  PASS BY REFERENCE to get back array of data.
        ;"                   OUTARR("KEY-VALUE",<LABEL>)=<VALUE>
        ;"                   OUTARR("KEY-VALUE",<LABEL>,"LINE")=<full text of line>
        ;"                   OUTARR(#)=<Line text>     <-- for entries that are not KEY-VALUE format
        ;"       OPTION -- OPTIONAL.  PASS BY REFERENCE.  Format:
        ;"                   OPTION("DT")=FMDT <-- will return table AS OF specified FM date-time
        ;"NOTE:  TMGPRIORTABLE used in global scope.  Var may be modified,
        ;"         but no error generated if not defined before calling here.
        ;"       TMGTABLEDEFS used in global scope.  Var may be modified,
        ;"         but no error generated if not defined before calling here.
        ;"           TMGTABLEDEFS(SUBIEN)=CMD^PARAM^PARAM^PARAM...
        ;"Result : returns one line, that can be added to table.
        ;"        Format is   'Label : Value'
        ;"NOTE: If multiple lines need to be returned, then
        ;"      lines can be separated by CR (#13)
        NEW TMGRESULT SET TMGRESULT=""
        IF $DATA(TMGTABLEDEFS)=0 DO GETDEFS(IEN,.TMGTABLEDEFS)                 
        NEW TABLENAME SET TABLENAME=$PIECE($GET(^TMG(22708,IEN,0)),"^",1)
        NEW ZN SET ZN=$GET(^TMG(22708,IEN,1,SUBIEN,0))
        NEW DISPCODE SET DISPCODE=$GET(^TMG(22708,IEN,1,SUBIEN,6))
        NEW CODE SET CODE=$GET(^TMG(22708,IEN,1,SUBIEN,4))  ;"OPTIONAL TRIGGER HOOK
        IF CODE'="" SET OPTION("CODE")=CODE
        NEW NAME SET NAME=$PIECE(ZN,"^",1)
        NEW LIMITNUM SET LIMITNUM=+$PIECE(ZN,"^",4) 
        IF LIMITNUM=0 SET LIMITNUM=3  ;"Default of 3 labs returned. 
        NEW SHOWNULL SET SHOWNULL=($PIECE(ZN,"^",5)="Y") ;"boolean
        ;" Test gender
        NEW GENDER SET GENDER=$PIECE(ZN,"^",6)  ;"  0;6=GENDER SPECIFIC
        IF GENDER'="" IF $PIECE($GET(^DPT(DFN,0)),"^",2)'=GENDER GOTO GIDN  ;"Skip IF wrong gender.
        ;"Test age
        NEW MINAGE,MAXAGE,PTAGE 
        SET MINAGE=$PIECE(ZN,"^",8)
        SET MAXAGE=$PIECE(ZN,"^",9)
        SET PTAGE=$$AGE^TIULO(DFN)
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
        ;. SET REMDATA=$$TEST1REM^TMGRPC3G("TMGRESULT",REMIEN,DFN,X,"N")
        NEW DISPTEXT SET DISPTEXT=$PIECE(ZN,"^",7)
        NEW PARSED SET PARSED=$GET(TMGTABLEDEFS(SUBIEN))
        NEW ISAHF SET ISAHF=0
        IF PARSED="" DO  ;"See IF this entry makes use of the health factor fields
        . IF $DATA(^TMG(22708,IEN,1,SUBIEN,1))>0 SET ISAHF=1 QUIT
        . IF $DATA(^TMG(22708,IEN,1,SUBIEN,2))>0 SET ISAHF=1 QUIT
        . IF $DATA(^TMG(22708,IEN,1,SUBIEN,3))>0 SET ISAHF=1 QUIT
        NEW CMD SET CMD=$$UP^XLFSTR($PIECE(PARSED,"^",1))
        NEW VALSTR SET VALSTR=""
        IF CMD="LAB" DO
        . SET VALSTR=$$GETLABS(DFN,NAME,PARSED,SHOWNULL,LIMITNUM,.OUTARR,.OPTION) 
        ELSE  IF CMD="LAB DATES" DO
        . SET VALSTR=$$GETLABDT(DFN,NAME,PARSED,SHOWNULL,LIMITNUM,.OUTARR,.OPTION)
        ELSE  IF CMD="MEDS" DO
        . SET VALSTR=$$GETMEDS(DFN,NAME,IEN,PARSED,SHOWNULL,.TMGTABLEDEFS,.OUTARR,.OPTION)                 
        ELSE  IF CMD="PRIOR" DO
        . SET VALSTR=$$GETPRIOR(DFN,NAME,IEN,TABLENAME,PARSED,SHOWNULL,.TMGPRIORTABLE,.TMGTABLEDEFS,.OUTARR,.OPTION)         
        ELSE  IF CMD="HIDE" GOTO GIDN  ;"Does nothing here.  Effects in {PRIOR: !MISC!}
        ELSE  IF CMD="CODE" DO
        . NEW XCODE SET XCODE="SET VALSTR=$$"_$PIECE(PARSED,"^",2)_"^"_$PIECE(PARSED,"^",3)_"("_DFN_")"
        . XECUTE XCODE
        ELSE  IF CMD="",ISAHF=1 DO
        . SET VALSTR=$$GETITEM^TMGPXR02(DFN,IEN,SUBIEN,MAXLEN,.OUTARR,.OPTION)
        ELSE  DO
        . SET VALSTR=$$BOIL^TIUSRVD(DISPTEXT)
        . NEW I2 SET I2=+$ORDER(OUTARR("@"),-1)+1
        . SET OUTARR(I2)=VALSTR        
        SET TMGRESULT=VALSTR
GIDN    QUIT TMGRESULT
        ;
TEST(DFN)  ;" Used to test the "CODE" command from up above
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
RXMATCH(IEN,MEDLINE) ;"CHECK MEDICATION FOR MATCH
        ;"Input: IEN= IEN in 22708
        ;"        MEDLINE -- one line from medication table (includes directions etc)
        NEW RESULT SET RESULT=0
        SET MEDLINE=$$UP^XLFSTR($GET(MEDLINE))
        NEW SUBIEN SET SUBIEN=0
        FOR  SET SUBIEN=$ORDER(^TMG(22708,IEN,3,SUBIEN)) QUIT:(+SUBIEN'>0)!(RESULT>0)  DO
        . NEW AMED SET AMED=$$UP^XLFSTR(^TMG(22708,IEN,3,SUBIEN,0))
        . SET RESULT=(MEDLINE[AMED)
        QUIT RESULT
        ;
GETLABS(DFN,LABELNAME,PARAMS,SHOWNULL,LIMITNUM,OUTARR,OPTION)  ;"GET LABS
        ;"Input: DFN -- PATIENT IEN
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
        SET PTID=DFN_"^2"
        NEW LAB SET LAB=$PIECE(PARAMS,"^",2)
        NEW SHOWDT SET SHOWDT=$PIECE(PARAMS,"^",3)
        NEW EDT SET EDT=+$GET(OPTION("DT")) IF EDT>0 SET OPTION("EDT")=EDT
        SET OPTION("MAX CT")=LIMITNUM
        SET OPTION("SHOW DATE")=(SHOWDT=1)
        SET OPTION("SHOW NULL")=SHOWNULL
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
GETLABDT(DFN,LABELNAME,PARAMS,SHOWNULL,LIMITNUM,OUTARR,OPTION)  ;"GET LAB DATES
        ;"Input: DFN -- PATIENT IEN
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
        SET PTID=DFN_"^2"
        NEW LAB SET LAB=$PIECE(PARAMS,"^",2)
        NEW EDT SET EDT=+$GET(OPTION("DT")) IF EDT>0 SET OPTION("EDT")=EDT
        SET OPTION("MAX CT")=LIMITNUM
        SET OPTION("DATES ONLY")=1
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
GETMEDS(DFN,LABEL,IEN,PARAMS,SHOWNULL,TABLEDEFS,OUTARR,OPTION) ;
        ;"Purpose: GET PRIOR VALUES FROM PRIOR TABLE.
        ;"Input: DFN -- PATIENT IEN
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
        NEW MEDS DO GTLKMDARR(.MEDS,DFN,IEN,.OPTION)
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
        . NEW LN SET LN=+$ORDER(OUTARR("@"),-1)+1   ;"//kt 10/15
        . SET OUTARR(LN)=ALINE                     ;"//kt 10/15
        QUIT TMGRESULT
        ;
GTLKMDARR(OUT,DFN,IEN,OPTION)  ;"GET LINKED MEDS ARRAY
        ;"Purpose: returns array with all medication line items found in
        ;"         MEDICATIONS table that are also noted in RELATED MEDICATIONS field
        ;"Input:  OUT.  PASS BY REFERENCE.  AN OUT PARAMETER.  Format:
        ;"            OUT(<medication line entry)=""
        ;"        DFN -- patient IEN
        ;"        IEN -- IEN IN 22708
        ;"        OPTION -- OPTIONAL.  PASS BY REFERENCE.  Format:
        ;"            OPTION("DT")=FMDT <-- will return info AS OF specified FM date-time
        ;"Result: none. 
        NEW MEDTABL
        ;"NEW REF SET REF=$NAME(^TMP("TMGTIU08 MEDS",$J))
        NEW STORENAME SET STORENAME="MEDICATIONS TABLE-"_DFN
        NEW REF SET REF=$$GETMPREF^TMGMISC2(STORENAME)        
        NEW PRIORTIME SET PRIORTIME=+$GET(@REF@("D","HTIME"))
        NEW DELTASEC SET DELTASEC=$$HDIFF^XLFDT($H,PRIORTIME,2)  ;"returns seconds
        IF DELTASEC<120 DO
        . MERGE MEDTABL=@REF@("D","TABLE")
        ELSE  DO     
        . NEW TEMPOPT MERGE TEMPOPT("DT")=OPTION("DT")
        . ;"//kt 1/15/17 original --> NEW TEMP SET TEMP=$$GETTABLX^TMGTIUO6(DFN,"MEDICATIONS TABLE",.MEDTABL,.TEMPOPT)
        . NEW TEMP SET TEMP=$$GETTABLX^TMGTIUO6(DFN,"MEDICATIONS",.MEDTABL,.TEMPOPT)
        . NEW ARR,H SET H=$H MERGE ARR("HTIME")=H 
        . MERGE ARR("TABLE")=MEDTABL
        . DO TMPSAVE^TMGMISC2(.ARR,STORENAME,"5M") 
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(MEDTABL(IDX)) QUIT:+IDX'>0  DO
        . NEW ALINE SET ALINE=$GET(MEDTABL(IDX)) QUIT:ALINE=""
        . NEW MATCH SET MATCH=$$RXMATCH(IEN,ALINE) QUIT:'MATCH
        . SET OUT(ALINE)=""
        . ;"//kt 11/25/15 NEW LN SET LN=+$ORDER(OUTARR("@"),-1)+1  ;"//kt 10/15
        . ;"//kt 11/25/15 SET OUTARR(LN)=ALINE                    ;"//kt 10/15
        QUIT
        ;
GETPRIOR(DFN,NEWLABEL,IEN,TABLENAME,PARAMS,SHOWNULL,PRIORTABLE,TABLEDEFS,OUTARR,OPTION) ;
        ;"Purpose: GET PRIOR VALUES FROM PRIOR TABLE.
        ;"Input: DFN -- PATIENT IEN
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
        ;"                   OPTION("CODE")=<CODE>   OPTIONAL, TO BE IMPLEMENTED (SEE GETPRIOR)
        ;"Result: returns string to show, or "" IF problem.
        ;"NOTE: IF multiple lines need to be returned, then
        ;"      lines can be separated by CR (#13)
        SET CODE=$GET(OPTION("CODE"))
        NEW STRVAL SET STRVAL=""
        NEW TMGRESULT SET TMGRESULT=""
        NEW ERROR SET ERROR=0
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
        . . DO PRIORRXT(DFN,48,.TEMPARR,1,.OPTION)  ;"48 months; 1 = only last table
        . ELSE  DO
        . . DO GETSPECL^TMGTIUO4(DFN,TABLENAME,"BLANK_LINE",48,.TEMPARR,1)  ;"48 months; 1 = only last table
        . NEW KEY SET KEY=""
        . FOR  SET KEY=$ORDER(TEMPARR("KEY-VALUE",KEY)) QUIT:(KEY="")  DO
        . . IF (KEY?1"MEDICATION-"1.N) KILL TEMPARR("KEY-VALUE",KEY)
        . MERGE PRIORTABLE(TABLENAME)=TEMPARR
        . KILL PRIORTABLE(TABLENAME,"KEY-VALUE","SOURCE-DATE")
        . IF $DATA(PRIORTABLE(TABLENAME))=0 SET PRIORTABLE(TABLENAME,"EMPTY")=1
        NEW PRIORTABLEUSE SET PRIORTABLEUSE=$$PRIORUSE(DFN,IEN)
        SET ^TMG("EDDIE","GETPRIOR","PRIORTABLEUSE")=PRIORTABLEUSE
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
        . . . ;"ELH 8/18/16 COMMENTED THE NEXT THREE LINES TO KEEP OLD TABLE data from being put into new tables  
        . . . ;"IF ('PRIORTABLEUSE)&(LABEL'="!MISC!") DO
        . . . ;". MERGE @TEMPREF@(TMPO_UPLABEL)=@TEMPREF@(UPLABEL)
        . . . ;". SET @TEMPREF@(TMPO_UPLABEL,"LINE")=TMPO_$GET(@TEMPREF@(TMPO_UPLABEL,"LINE"))
        . . . . ;"
        . . . . ;"ELH 4/12/16 DON'T STORE PRIOR USE ANY LONGER IN 2 LINES BELOW
        . . . . ;"   NEW TEMP SET TEMP=$$SAVPRIOR(DFN,IEN) ;"ENSURE PRIOR USE OF TABLE HAS BEEN STORED.
        . . . . ;"   IF TEMP'>0 SET ERROR="1^"_$PIECE(TEMP,"^",2) 
        . . . KILL PRIORTABLE(TABLENAME,"KEY-VALUE",$$UP^XLFSTR(LABEL))
        . IF ERROR>0 SET RESULT=RESULT_$CHAR(13,10)_"ERROR: "_$PIECE(ERROR,"^",2) QUIT
        . ;"Next collect remaining entries, in KEY-VALUE form
        . NEW PRIORFOUNDLABEL SET PRIORFOUNDLABEL=""
        . FOR  SET PRIORFOUNDLABEL=$ORDER(PRIORTABLE(TABLENAME,"KEY-VALUE",PRIORFOUNDLABEL)) QUIT:PRIORFOUNDLABEL=""  DO
        . . NEW ALINE SET ALINE=$GET(PRIORTABLE(TABLENAME,"KEY-VALUE",PRIORFOUNDLABEL,"LINE")) QUIT:ALINE=""
        . . MERGE OUTARR(TABLENAME,"KEYVALUE")=PRIORTABLE(TABLENAME,"KEYVALUE",PRIORFOUNDLABEL)
        . . IF TMGRESULT'="" SET TMGRESULT=TMGRESULT_$CHAR(13)
        . . SET TMGRESULT=TMGRESULT_ALINE
        . . NEW LN SET LN=+$ORDER(OUTARR("@"),-1)+1      ;"//kt 10/15
        . . SET OUTARR(LN)=ALINE                        ;"//kt 10/15        
        . ;"Next collect remaining entries, NOT in KEY-VALUE form
        . NEW IDX SET IDX=0
        . FOR  SET IDX=$ORDER(PRIORTABLE(TABLENAME,IDX)) QUIT:+IDX'>0  DO
        . . NEW ALINE SET ALINE=$GET(PRIORTABLE(TABLENAME,IDX)) QUIT:ALINE=""
        . . NEW I2 SET I2=+$ORDER(OUTARR("@"),-1)+1
        . . SET OUTARR(I2)=ALINE
        . . IF TMGRESULT'="" SET TMGRESULT=TMGRESULT_$CHAR(13)
        . . SET TMGRESULT=TMGRESULT_ALINE
        . . ;"//kt removed 8/23/17 -- d/t causes duplicate entries
        . . ;"//kt removed 8/23/17 NEW LN SET LN=+$ORDER(OUTARR("@"),-1)+1      ;"//kt 10/15
        . . ;"//kt removed 8/23/17  SET OUTARR(LN)=ALINE                        ;"//kt 10/15        
        ELSE  DO
        . SET PRIORLABEL=$$UP^XLFSTR(PRIORLABEL)
        . SET STRVAL=$GET(PRIORTABLE(TABLENAME,"KEY-VALUE",PRIORLABEL))
        . IF STRVAL="",SHOWNULL'=1 QUIT
        . ELSE  DO
        . . IF TMGRESULT'="" SET TMGRESULT=TMGRESULT_$CHAR(13)
        . . IF CODE'="" DO     ;"ELH ADDED TO EXECUTE VALUE MODIFYING CODE
        . . . NEW TMGX,TMGY
        . . . SET TMGX=STRVAL
        . . . XECUTE CODE
        . . . SET TMGY=$GET(TMGY)
        . . . IF TMGY'="" SET STRVAL=TMGY
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
PRIORRXT(DFN,MONTHS,OUT,MODE,OPTION)  ;"GET PRIOR MEDICATIONS (RX) TABLE
        ;"INPUT: DFN -- patient IEN
        ;"       MONTHS -- number of months to search back
        ;"       OUT -- PASS BY REFERENCE.  The output of this function
        ;"       MODE -- 1 = only last table
        ;"       OPTION -- OPTIONAL.  PASS BY REFERENCE.  Format:
        ;"                   OPTION("DT")=FMDT <-- will return info AS OF specified FM date-time
        ;"Results: none.  See OUT
        ;"Modified from GMEDTABL^TMGTIUO6        
        ;
        ;"NEW REF SET REF=$NAME(^TMP("PRIORRXT^TMGTIUO8",$J,DFN))
        NEW STORENAME SET STORENAME="MEDICATIONS TABLE-"_DFN
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
        DO GETSPECL^TMGTIUO4(DFN,"[FINAL MEDICATIONS]","BLANK_LINE",MONTHS,.ARRAY1,MODE,,.TEMPOPT)
        SET FRXDT=+$GET(ARRAY1("KEY-VALUE","SOURCE-DATE"))
        SET TEMPOPT("ALL NOTES")=1  ;"Include unsigned notes.
        DO GETSPECL^TMGTIUO4(DFN,"[MEDICATIONS]","BLANK_LINE",MONTHS,.ARRAY2,MODE,,.TEMPOPT) 
        SET RXDT=$GET(ARRAY2("KEY-VALUE","SOURCE-DATE"))
        IF RXDT>FRXDT DO  ;"If MEDICATIONS table has been reconcilled since FINAL MEDICATIONS table has, then use it. 
        . MERGE OUT=ARRAY2
        ELSE  DO
        . MERGE OUT=ARRAY1
        NEW IDX SET IDX=0 FOR  SET IDX=$ORDER(OUT(IDX)) QUIT:(+IDX'>0)  DO
        . SET OUT(IDX)=$$FIXABREV^TMGTIUO6($GET(OUT(IDX)))
        ;"KILL @REF MERGE @REF@("T")=OUT
        DO TMPSAVE^TMGMISC2(.OUT,STORENAME,"5M")  ;"save for 5 minutes
PRXTDN  ;        
        QUIT
        ;
PRIORUSE(DFN,IEN22708) ;"HAS TABLE BEEN USED BY PATIENT BEFORE?    
        NEW TMGRESULT
        IF $PIECE($GET(^TMG(22708,+$GET(IEN22708),0)),"^",7)="Y" SET TMGRESULT=1 GOTO PUDN
        SET TMGRESULT=($ORDER(^TMG(22708,+$GET(IEN22708),2,"B",+$GET(DFN),0))>0)
PUDN    QUIT TMGRESULT
        ;
SAVPRIOR(DFN,IEN22708) ;"ENSURE PRIOR USE OF TABLE HAS BEEN STORED.
        ;"Input -- DFN -- IEN IN PATIENT FILE. 
        ;"         IEN22708 -- IEN IN 22708 (TMG TIU PXRM TABLE)
        ;"Result -- 1^OK, or -1^Message
        NEW TMGRESULT SET TMGRESULT="1^OK"
        IF $$PRIORUSE(.DFN,.IEN22708) GOTO SPRDN
        NEW TMGFDA,TMGIEN,TMGMSG
        SET IEN22708=+$GET(IEN22708) 
        IF IEN22708'>0 DO  GOTO SPRDN
        . SET TMGRESULT="-1^No value for TABLE IEN (22708) given."
        SET DFN=+$GET(DFN)
        IF DFN'>0 DO  GOTO SPRDN
        . SET TMGRESULT="-1^No value for patient # (DFN) given."
        SET TMGFDA(22708.04,"+1,"_IEN22708_",",.01)=DFN
        SET TMGFDA(22708.04,"+1,"_IEN22708_",",1)=$$NOW^XLFDT
        DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO SPRDN  
        . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
SPRDN   QUIT TMGRESULT                
        ;