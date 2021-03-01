TMGPXR02 ;TMG/kst/TMG Reminder Text Table stuff ;3/24/14, 10/10,17
         ;;1.0;TMG-LIB;**1**;3/24/13
 ;
 ;"TMG REMINDER FUNCTIONS
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
 ;"RMDGTABL(DFN,LABEL) -- Get Reminder dialog type TIU Text Table
 ;"DOTABL(DFN,LABEL,GICODE,OUTARR,OPTION) -- Get Table, with options
 ;"GETTABLN(DFN,TABLENAME,ITEMNAME) -- GET ONE REMINDER TABLE LINE ENTRY
 ;
 ;"=======================================================================
 ;"PRIVATE FUNCTIONS
 ;"=======================================================================
 ;"GETITEM(DFN,IEN,SUBIEN,SPACES,RESULT) -- Get a table item.(one line entry)
 ;
 ;"=======================================================================
 ;"Dependancies :
 ;"=======================================================================
 ;
RMDGTABL(DFN,LABEL,GICODE,OUTARR,OPTION) ;"Get Reminder dialog type TIU Text Table
        ;"Input: DFN -- IEN in PATIENT file
        ;"       LABEL -- This is the .01 field of the table (File# 22708) to create
        ;"       GICODE -- 'GET ITEM CODE'. OPTIONALS.  Default is "GETITEM"
        ;"                 This is mumps code to get an individual item.
        ;"                 Must take following parametsrs FN(DFN,TABLEIEN,SUBIEN,MAXLEN,.OUTARR))
        ;"       OUTARR -- OPTIONAL.  PASS BY REFERENCE to get back array of data.  Format
        ;"                   OUTARR("KEY-VALUE",<LABEL>)=<VALUE>
        ;"                   OUTARR("KEY-VALUE",<LABEL>,"LINE")=<full text of line, with leading spaces>
        ;"                   OUTARR(#)=<Line text>     <-- for entries that are not KEY-VALUE format
        ;"       OPTION("DT")=FMDT <-- if present, then table is to be returns AS OF given date
        ;"Result: returns string with embedded line-feeds to create text table.
        ;"//NOTE: This table is different from a reminder dialog in CPRS.  I can't recall now
        ;"        the thinking behind why this was called a reminder dialog type table. 10/2017
        QUIT $$DOTABL(.DFN,.LABEL,.GICODE,.OUTARR,.OPTION)  ;"could send OPTION later if needed.
        ; 
DOTABL(DFN,LABEL,GICODE,OUTARR,OPTION) ;"Get Table, with options
        ;"Input: DFN -- IEN in PATIENT file
        ;"       LABEL -- This is the .01 field of the table (File# 22708) to create
        ;"       GICODE -- 'GET ITEM CODE'. OPTIONALS.  Default is "GETITEM"
        ;"                 This is mumps code to get an individual item.
        ;"                 Must take following parametsrs FN(DFN,TABLEIEN,SUBIEN,MAXLEN,.OUTARR))
        ;"       OUTARR -- OPTIONAL.  PASS BY REFERENCE to get back array of data.  Format
        ;"                   OUTARR("KEY-VALUE",<LABEL>)=<VALUE>
        ;"                   OUTARR("KEY-VALUE",<LABEL>,"LINE")=<full text of line, with leading spaces>
        ;"                   OUTARR(#)=<Line text>     <-- for entries that are not KEY-VALUE format
        ;"       OPTION -- OPTIONAL.  PASS BY REFERENCE.  Format:
        ;"                   OPTION("NO HEADER LINE")=1
        ;"                   OPTION("NO LF")=1
        ;"                   OPTION("DT")=FMDT <-- will return table AS OF specified FM date-time
        ;"Result: returns string with embedded line-feeds to create text table.
        NEW LINEDATA
        NEW DEBUG SET DEBUG=0 ;"Change at runtime IF needed.
        IF DEBUG=1 DO
        . SET DFN=$GET(^TMG("TMP","REM DLG TABLE","DFN"))
        . SET GICODE=$GET(^TMG("TMP","REM DLG TABLE","GICODE"))
        . SET LABEL=$GET(^TMG("TMP","REM DLG TABLE","LABEL"))
        . KILL OPTION MERGE OPTION=^TMG("TMP","REM DLG TABLE","OPTION")        
        ELSE  DO
        . KILL ^TMG("TMP","REM DLG TABLE")
        . SET ^TMG("TMP","REM DLG TABLE","DFN")=$GET(DFN)
        . SET ^TMG("TMP","REM DLG TABLE","GICODE")=$GET(GICODE)
        . SET ^TMG("TMP","REM DLG TABLE","LABEL")=$GET(LABEL)
        . MERGE ^TMG("TMP","REM DLG TABLE","OPTION")=OPTION
        NEW RESULT SET RESULT=""
        NEW AGE K VADM SET AGE=$$AGE^TIULO(DFN)
        SET GICODE=$GET(GICODE,"GETITEM")
        SET LABEL=$GET(LABEL,"<UNKNOWN>")
        NEW HEADERLN SET HEADERLN=$GET(OPTION("NO HEADER LINE"))'=1
        NEW HTML SET HTML=+$GET(OPTION("HTML"))
        NEW ADDLF SET ADDLF=$GET(OPTION("NO LF"))'=1
        IF LABEL["[" SET LABEL=$PIECE(LABEL,"[",2),LABEL=$PIECE(LABEL,"]",1)
        NEW TABLEIEN SET TABLEIEN=$ORDER(^TMG(22708,"B",LABEL,0))
        NEW SPACES SET SPACES=""
        IF +TABLEIEN>0 DO
        . NEW NUMINDENT SET NUMINDENT=$PIECE($GET(^TMG(22708,TABLEIEN,0)),"^",3)
        . SET SPACES=$EXTRACT("                 ",1,NUMINDENT)  ;"max is 16 spaces
        NEW PRNAME SET PRNAME=$PIECE($GET(^TMG(22708,TABLEIEN,0)),"^",2)  ;"PRINT NAME
        IF PRNAME'="" SET LABEL=PRNAME
        SET RESULT=SPACES
        NEW AHEADER SET AHEADER="["_LABEL_"] "
        IF HEADERLN SET AHEADER="-- "_AHEADER_"---------"
        NEW LN SET LN=+$ORDER(OUTARR("@"),-1)+1
        SET OUTARR(LN)=AHEADER
        SET RESULT=AHEADER
        IF ADDLF DO
        . IF HTML SET RESULT=RESULT_"<BR>"
        . SET RESULT=RESULT_$CHAR(13)_$CHAR(10)
        NEW MAXLEN SET MAXLEN=+$PIECE($GET(^TMG(22708,TABLEIEN,0)),"^",4)
        IF MAXLEN'>0 SET MAXLEN=9999
        NEW ORDER,IDX SET IDX=0
        ;"Add process all entries with a sequence number to order array
        NEW SEQ SET SEQ=""
        FOR  SET SEQ=$ORDER(^TMG(22708,TABLEIEN,1,"ASEQ",SEQ)) QUIT:(+SEQ'>0)  DO
        . NEW SUBIEN SET SUBIEN=+$ORDER(^TMG(22708,TABLEIEN,1,"ASEQ",SEQ,0)) 
        . IF $$DISABLED(TABLEIEN,SUBIEN) QUIT
        . IF SUBIEN>0 SET IDX=IDX+1,ORDER(IDX)=SUBIEN  ;changed from '> elh 5/29/13
        ;"Next add all remaining entries to order array
        NEW NAME SET NAME=""
        FOR  SET NAME=$ORDER(^TMG(22708,TABLEIEN,1,"B",NAME)) QUIT:(NAME="")  DO
        . NEW SUBIEN SET SUBIEN=+$ORDER(^TMG(22708,TABLEIEN,1,"B",NAME,0)) QUIT:(SUBIEN'>0)
        . SET SEQ=$PIECE($GET(^TMG(22708,TABLEIEN,1,SUBIEN,0)),"^",2) QUIT:(SEQ'="")  ;"Ignore if has SEQ value
        . IF $$DISABLED(TABLEIEN,SUBIEN) QUIT
        . SET IDX=IDX+1,ORDER(IDX)=SUBIEN
        ;"Now get text for each line, in sequence specified by order array
        NEW KILLARR DO LOADKILL^TMGTIUO6(.KILLARR,LABEL)  ;"Get list of items to REMOVE from table. 
        SET IDX=0 FOR  SET IDX=$ORDER(ORDER(IDX)) QUIT:IDX=""  DO
        . NEW SUBIEN SET SUBIEN=ORDER(IDX)
        . NEW TEMP,STR SET TEMP="S STR=$$"_GICODE_"(DFN,TABLEIEN,SUBIEN,MAXLEN,.OUTARR,.OPTION)"
        . ;"NOTE: It is very complicated to figure out exactly when and where to ensure that 
        . ;"      items destined to be show in an HTML page are properly encoded.
        . ;"      So I am going to arbitrarily draw the line here.  
        . ;"      IF HTML=1, then everything that comes back from XECUTE should ALREADY be 
        . ;"      HTML symbol encoded, AND have a trailing <BR>
        . ;"      This will likely break other things.  But at this point, I have a broken system,
        . ;"      and am doing this to get it back going again.
        . XECUTE TEMP  ;"E.g. $$GETITEM(DFN,TABLEIEN,SUBIEN,MAXLEN,.OUTARR)
        . FOR  QUIT:(STR="")  DO
        . . SET LINEDATA=$PIECE(STR,$CHAR(13),1)
        . . SET STR=$PIECE(STR,$CHAR(13),2,999)
        . . SET LINEDATA=$$TRIM^XLFSTR(LINEDATA) QUIT:LINEDATA=""
        . . ;"IF LABEL["MEDICATION" SET LINEDATA=$$CHKMED^TMGTIUOT(LINEDATA,AGE,.OPTION)
        . . IF +$G(OPTION("NO HEADER LINE"))=1 DO
        . . . SET RESULT=RESULT_" "
        . . ELSE  DO
        . . . SET RESULT=RESULT_SPACES
        . . ;"IF HTML SET LINEDATA=$$TXS2HTML^TMGHTM1(LINEDATA)_"<BR>"
        . . ;"If the line item is set to be removed, don't add  8/7/18  ELH
        . . IF $DATA(KILLARR),$$TOKILL^TMGTIUO6(LINEDATA,LABEL,.KILLARR)=1 QUIT  
        . . IF (LINEDATA["00/")&(LINEDATA'["<-")&(LINEDATA'["&lt;-") SET LINEDATA=$$REPLSTR^TMGSTUT3(LINEDATA,"00/","")  ;"remove empty months or days. 4/2/19
        . . ;"   5/28/19 added and above because we only want this for dates and not for data values
        . . IF LINEDATA["{E-Scribe}" QUIT    ;"don't include in tables 4/2/19
        . . SET RESULT=RESULT_LINEDATA        
        . . IF ADDLF SET RESULT=RESULT_$CHAR(13)_$CHAR(10)
        NEW TERMINALSTR SET TERMINALSTR=$PIECE($GET(^TMG(22708,TABLEIEN,4)),"^",1)
        SET RESULT=RESULT_TERMINALSTR
        QUIT RESULT
        ;
DISABLED(TABLEIEN,SUBIEN)  ;"Is line item disabled?
        NEW ZN SET ZN=$GET(^TMG(22708,TABLEIEN,1,SUBIEN,0))
        QUIT ($PIECE(ZN,"^",11)="Y")
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
        ;"Result : returns one line, that can be added to table.
        ;"        Format is   'Label : Value'
        ;"NOTE: In the future, if multiple lines need to be returned, then
        ;"      lines can be separated by CR (#13)
        NEW ZZDEBUG SET ZZDEBUG=0 ;"Change at runtime IF needed.
        IF ZZDEBUG=1 DO
        . SET DFN=$GET(^TMG("TMP","REM DLG TABLE GET ITEM","DFN"))
        . SET IEN=$GET(^TMG("TMP","REM DLG TABLE GET ITEM","IEN"))
        . SET SUBIEN=$GET(^TMG("TMP","REM DLG TABLE GET ITEM","SUBIEN"))
        . SET MAXLEN=$GET(^TMG("TMP","REM DLG TABLE GET ITEM","MAXLEN"))
        . KILL OUTARR
        . KILL OPTION MERGE OPTION=^TMG("TMP","REM DLG TABLE GET ITEM","OPTION")        
        ELSE  DO
        . KILL ^TMG("TMP","REM DLG TABLE GET ITEM")
        . SET ^TMG("TMP","REM DLG TABLE GET ITEM","DFN")=$GET(DFN)
        . SET ^TMG("TMP","REM DLG TABLE GET ITEM","IEN")=$GET(IEN)
        . SET ^TMG("TMP","REM DLG TABLE GET ITEM","SUBIEN")=$GET(SUBIEN)
        . SET ^TMG("TMP","REM DLG TABLE GET ITEM","MAXLEN")=$GET(MAXLEN)
        . MERGE ^TMG("TMP","REM DLG TABLE GET ITEM","OPTION")=OPTION
        NEW RESULT SET RESULT=""
        NEW ZN SET ZN=$GET(^TMG(22708,IEN,1,SUBIEN,0))
        NEW NAME SET NAME=$PIECE(ZN,"^",1)
        NEW LIMITNUM SET LIMITNUM=+$PIECE(ZN,"^",4) IF LIMITNUM=0 SET LIMITNUM=1
        NEW SHOWNUL SET SHOWNUL=($PIECE(ZN,"^",5)="Y") ;"boolean
        NEW GENDER SET GENDER=$PIECE(ZN,"^",6)  ;"  0;6=GENDER SPECIFIC
        IF GENDER'="" IF $PIECE($GET(^DPT(DFN,0)),"^",2)'=GENDER GOTO GIDN  ;"Skip IF wrong gender. 
        NEW FOUNDARRAY,REFUSEDARRAY,FOLLOWUPARRAY,NETARRAY
        NEW ITEMIEN SET ITEMIEN=0  ;"--- Gather array of dates of findings        
        FOR  SET ITEMIEN=+$ORDER(^TMG(22708,IEN,1,SUBIEN,1,ITEMIEN)) QUIT:(ITEMIEN'>0)  DO
        . SET ZN=$GET(^TMG(22708,IEN,1,SUBIEN,1,ITEMIEN,0))
        . NEW VPTR SET VPTR=$PIECE(ZN,"^",1)
        . DO GETVFIND(DFN,VPTR,LIMITNUM,.FOUNDARRAY)
        SET ITEMIEN=0  ;"--- Gather array of dates of refusal items.        
        FOR  SET ITEMIEN=+$ORDER(^TMG(22708,IEN,1,SUBIEN,2,ITEMIEN)) QUIT:(ITEMIEN'>0)  DO
        . SET ZN=$GET(^TMG(22708,IEN,1,SUBIEN,2,ITEMIEN,0))
        . NEW VPTR SET VPTR=$PIECE(ZN,"^",1)
        . DO GETVFIND(DFN,VPTR,LIMITNUM,.REFUSEDARRAY)
        SET ITEMIEN=0  ;"--- Gather array of dates of followup items.        
        FOR  SET ITEMIEN=+$ORDER(^TMG(22708,IEN,1,SUBIEN,3,ITEMIEN)) QUIT:(ITEMIEN'>0)  DO
        . SET ZN=$GET(^TMG(22708,IEN,1,SUBIEN,3,ITEMIEN,0))
        . NEW VPTR SET VPTR=$PIECE(ZN,"^",1)_";AUTTHF("
        . NEW TRIMPREFX SET TRIMPREFX=$PIECE(ZN,"^",2)
        . NEW TRIMLEN SET TRIMLEN=$LENGTH(TRIMPREFX)
        . NEW TEMPARR DO GETVFIND(DFN,VPTR,LIMITNUM,.TEMPARR)
        . NEW IDX SET IDX=0 FOR  SET IDX=$ORDER(TEMPARR(IDX)) QUIT:IDX=""  DO
        . . NEW NAME SET NAME=$PIECE($GET(TEMPARR(IDX)),"^",1) QUIT:NAME=""
        . . IF $EXTRACT(NAME,1,TRIMLEN)=TRIMPREFX DO
        . . . SET NAME=$EXTRACT(NAME,TRIMLEN+1,$LENGTH(NAME))
        . . . SET NAME=$$TRIM^XLFSTR(NAME)
        . . SET $PIECE(TEMPARR(IDX),"^",2)=NAME
        . MERGE FOLLOWUPARRAY=TEMPARR        
        NEW IDT
        SET IDT="" FOR  SET IDT=$ORDER(FOUNDARRAY(IDT)) QUIT:(+IDT'>0)  DO
        . SET NETARRAY(" "_IDT)=$GET(FOUNDARRAY(IDT))
        SET IDT="" FOR  SET IDT=$ORDER(REFUSEDARRAY(IDT)) QUIT:(+IDT'>0)  DO
        . SET NETARRAY(" "_IDT_"R")=$GET(REFUSEDARRAY(IDT))
        NEW VALSTR SET VALSTR=""
        SET IDT="" FOR  SET IDT=$ORDER(NETARRAY(IDT)) QUIT:(IDT="")!(LIMITNUM'>0)  DO
        . NEW REFUSED SET REFUSED=(IDT["R")
        . NEW TRIDT SET TRIDT=+$$TRIM^XLFSTR(IDT)
        . NEW DT SET DT=9999999-TRIDT
        . IF VALSTR'="" SET VALSTR=VALSTR_"; "
        . IF REFUSED SET VALSTR=VALSTR_"refused "
        . NEW DATESTR SET DATESTR=$$FMTE^XLFDT(DT,"5D")  ;"MM/DD/YYYY
        . IF DATESTR["/0/" SET DATESTR=$$REPLSTR^TMGSTUT3(DATESTR,"/0/","/")  ;"remove day IF 0
        . IF $EXTRACT(DATESTR,1,2)="0/" SET DATESTR=$EXTRACT(DATESTR,3,$LENGTH(DATESTR))  ;"remove month IF 0        
        . SET VALSTR=VALSTR_DATESTR
        . NEW COMMENT SET COMMENT=$PIECE($GET(NETARRAY(IDT)),"^",2)
        . IF COMMENT'="" SET VALSTR=VALSTR_" """_COMMENT_""""
        . ;"
        . ;"The following is test code for the followup date. The need for
        . ;"   this is sometimes the FU HF is put in after the colonoscopy
        . ;"   and not on the same date. This code will add the FU if the HF
        . ;"   has a later date than the colonoscopy itself. Original code is below that
        . NEW FUDATE SET FUDATE=$O(FOLLOWUPARRAY(TRIDT+1),-1)
        . IF FUDATE>0 DO
        . . NEW FU SET FU=$PIECE($GET(FOLLOWUPARRAY(FUDATE)),"^",2)
        . . IF FU="" QUIT
        . . SET VALSTR=VALSTR_" "_FU
        . ;"ORIGINAL CODE BELOW
        . ;"IF $DATA(FOLLOWUPARRAY(TRIDT)) DO
        . ;". NEW FU SET FU=$PIECE($GET(FOLLOWUPARRAY(TRIDT)),"^",2)
        . ;". IF FU="" QUIT
        . ;". SET VALSTR=VALSTR_" "_FU
        . ;"
        . IF $LENGTH(VALSTR)>MAXLEN DO
        . . SET LIMITNUM=0
        . . SET VALSTR=$EXTRACT(VALSTR,1,MAXLEN-3)_"..."
        . SET LIMITNUM=LIMITNUM-1
        NEW CODE SET CODE=$GET(^TMG(22708,IEN,1,SUBIEN,4))
        IF (CODE'="")&(VALSTR'="") DO     ;"ELH ADDED TO EXECUTE VALUE MODIFYING CODE 3/23/18
        . NEW TMGX,TMGY
        . SET TMGX=VALSTR
        . XECUTE CODE
        . SET TMGY=$GET(TMGY)
        . IF TMGY'="" SET VALSTR=TMGY
        . IF NAME["BLAKE" W TMGX,"-",TMGY,"-",VALSTR,!
        IF (VALSTR'="")!(SHOWNUL) DO
        . SET RESULT=NAME_" = "_VALSTR
        . SET OUTARR("KEY-VALUE",NAME)=VALSTR
        . SET OUTARR("KEY-VALUE",NAME,"LINE")=RESULT
GIDN    QUIT RESULT
        ;
GETVFIND(DFN,VPTR,LIMITNUM,OUT) ;"Gather findings
        ;"Purpose: For patient, gather dates when patient has finding matching VPTR
        ;"Input: DFN -- IEN in PATIENT file
        ;"       VPTR -- Text of variable ptr, e.g. '715;AUTTHF('
        ;"       LIMITNUM -- The max number of entries to return. 
        ;"       OUT -- PASS BY REFERENCE, an OUT PARAMETER.
        ;"          Format:  OUT(Inverted_FMDate)=FactorName^<Comment>
        NEW P2FILE SET P2FILE=$PIECE(VPTR,";",2)
        NEW P2IEN SET P2IEN=+VPTR
        NEW P2FILENUM SET P2FILENUM=0
        NEW REF SET REF=""
        IF P2FILE="AUTTEDT(" SET REF="ED",P2FILENUM=9999999.09 ;"Education Topic
        ELSE  IF P2FILE="AUTTIMM(" SET REF="IMM",P2FILENUM=9999999.14 ;"Immunization
        ELSE  IF P2FILE="AUTTSK(" SET REF="SK",P2FILENUM=9999999.28 ;"Skin TesT
        ELSE  IF P2FILE="AUTTEXAM(" SET REF="XAM",P2FILENUM=9999999.15 ;"Exam
        ELSE  IF P2FILE="AUTTHF(" SET REF="HF",P2FILENUM=9999999.64 ;"Health Factor
        ELSE  IF P2FILE="ICPT(" SET REF="CPT",P2FILENUM=9000010.18 ;"CPT Procedure
        ELSE  IF P2FILE="ICD9(" SET REF="POV",P2FILENUM=9000010.07 ;"POV Diagnosis
        IF REF="" GOTO GVFDN
        NEW REF0 SET REF0="^AUPNV"_REF
        SET REF=REF0_"(""AA"","_DFN_","_P2IEN_")"  ;"all the V * have the same AA reference.
        NEW IDT SET IDT=0
        FOR  SET IDT=$ORDER(@REF@(IDT)) QUIT:(+IDT'>0)!(LIMITNUM'>0)  DO
        . NEW SUBIEN SET SUBIEN=$ORDER(@REF@(IDT,0))
        . NEW REF2 SET REF2=REF0_"("_SUBIEN_")"
        . NEW COMMENT SET COMMENT=$PIECE($GET(@REF2@(811)),"^",1)
        . SET P2IEN=$PIECE($GET(@REF2@(0)),"^",1)
        . IF REF["AUPNVIMM" DO    ;"CHECK FOR CONTRAINDICATED
        . . NEW CONTRA SET CONTRA=+$P($G(^AUPNVIMM(SUBIEN,0)),"^",7)
        . . IF CONTRA=1 SET COMMENT=COMMENT_"(NOT GIVEN, CONTRAINDICATED)"
        . NEW NAME SET NAME=$$GET1^DIQ(P2FILENUM,P2IEN_",",.01)
        . SET OUT(IDT)=NAME_"^"_COMMENT
        . SET LIMITNUM=LIMITNUM-1        
GVFDN   QUIT
        ;
GETTABLN(DFN,TABLENAME,ITEMNAME) ;"GET 1 TABLE LINE ENTRY
        ;"Input: DFN -- IEN in PATIENT file
        ;"       TABLENAME -- NAME of table to pull from (TMG TIU PXRM TABLE, #22708)
        ;"       ITEMNAME -- ITEM name in table
        ;"Results: Returns text of line, or "" IF none, or problem.
        NEW TMGRESULT SET TMGRESULT=""
        SET TABLENAME=$GET(TABLENAME) IF TABLENAME="" GOTO G1TLDN
        NEW IEN SET IEN=+$ORDER(^TMG(22708,"B",TABLENAME,0))
        IF IEN'>0 GOTO G1TLDN
        NEW MAXLEN SET MAXLEN=+$PIECE($GET(^TMG(22708,IEN,0)),"^",4)
        IF MAXLEN'>0 SET MAXLEN=9999
        NEW SUBIEN SET SUBIEN=+$ORDER(^TMG(22708,IEN,1,"B",ITEMNAME,0))
        IF SUBIEN'>0 GOTO G1TLDN
        SET DFN=+$GET(DFN) IF DFN'>0 DO G1TLDN
        SET TMGRESULT=$$GETITEM(DFN,IEN,SUBIEN,MAXLEN) ;"Get a table item.                
G1TLDN  QUIT TMGRESULT                

