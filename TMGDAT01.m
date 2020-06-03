TMGDAT01 ;TMG/kst-Data storing functions;05/19/16
         ;;1.0;TMG-LIB;**1,17**;05/19/16
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
DBVALS(TMGRESULT,DATA)  ;
  ;"Purpose:  Channel for reading/writing data to file 22729 (TMG PATIENT DISCRETE DATA)
  ;"RESULT[0] = 1^OK  or -1^Error message
  ;"Format of DATA INCOMING...
  ;" DATA[0] = 'INFO^<DFN>^<IEN8925>^<DT>
  ;"            IEN8925 is only used when SET'ing a DB value
  ;"            DT is string in FilemanDT format
  ;" when reading from server
  ;"    DATA[#] = 'GET^<TEMPLATE FIELD IEN>^<any optional tag value>',  or
  ;"    DATA[#] = 'GET^ALL^<any optional tag value>
  ;" or when writing to database
  ;"    DATA[#] = 'SET^<TEMPLATE FIELD IEN>^<value of db control>'
  ;"        NOTE: any #13#0 (CRLF) in <value> will be replaced as {{LF}} prior to storage
  ;" or when deleting
  ;"    DATA[#] = 'KILL   <-- kill all values with DFN, ien8925, and DT
  ;"-------------
  ;"Format sending back to client:  
  ;"     TMGRESULT[0] = 1^OK  or -1^Error message
  ;"after reading
  ;"     TMGRESULT[#]'VALUE^<Template FIELD IEN>^<value of db control>^<any tag value>^<DT of value>^FieldName^<TypeSymbol>
  ;"        NOTE: any #13#0 (CRLF) in <value> will be replaced as {{LF}}
  ;"or after writing
  ;"     TMGRESULT[#]'STATUS^<Template FIELD IEN>^<server message, if any>^<any tag value>
  ;" or after deleting
  ;"     TMGRESULT[#]'STATUS^<Template FIELD IEN>^<server message, if any>^<any tag value>
  ;
  NEW TMGDEBUG SET TMGDEBUG=0
  IF TMGDEBUG=1 DO
  . KILL DATA 
  . MERGE DATA=^TMG("TMP","DBVALS^TMGDAT01","DATA")
  ELSE  DO
  . KILL ^TMG("TMP","DBVALS^TMGDAT01")
  . MERGE ^TMG("TMP","DBVALS^TMGDAT01","DATA")=DATA
  ;
  SET TMGRESULT(0)="1^OK"
  NEW INFO SET INFO=$GET(DATA(0))
  NEW ADFN SET ADFN="" 
  NEW IEN8925 SET IEN8925=0
  NEW ADT SET ADT=0
  IF $PIECE(INFO,"^",1)="INFO" DO
  . SET ADFN=$PIECE(INFO,"^",2)
  . SET IEN8925=+$PIECE(INFO,"^",3)
  . SET ADT=+$PIECE(INFO,"^",4)
  ELSE  DO  GOTO DBVDN
  . SET TMGRESULT(0)="-1^Unexpected DATA(0), piece 1.  Should be 'INFO'"
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(DATA(IDX)) QUIT:IDX=""  DO
  . NEW LINE SET LINE=$GET(DATA(IDX)) QUIT:LINE=""
  . NEW CMD SET CMD=$PIECE(LINE,"^",1)
  . NEW FNRESULT SET FNRESULT="1^OK"
  . IF CMD="GET" SET FNRESULT=$$GETVAL(.TMGRESULT,LINE,ADFN,ADT)
  . IF CMD="SET" SET FNRESULT=$$SETVAL(.TMGRESULT,LINE,ADFN,ADT,IEN8925)
  . IF CMD="KILL" SET FNRESULT=$$KILLVALS(.TMGRESULT,ADFN,ADT,IEN8925)
  . IF +FNRESULT'=1 DO
  . . IF +TMGRESULT(0)=1 SET TMGRESULT(0)=FNRESULT QUIT
  . . NEW PRIOR SET PRIOR=$PIECE(TMGRESULT(0),"^",2)
  . . SET TMGRESULT(0)="-1^"_PRIOR_", ALSO "_$PIECE(FNRESULT,"^",2)  ;"Append err message
DBVDN ;  
  QUIT
  ;
GETVAL(OUT,LINE,ADFN,ADT) ;
  ;"Purpose: recovery stored values for template FIELD IEN's (if any)
  ;"Input:  OUT -- PASS BY REFERENCE, AN OUT PARAMETER.  
  ;"        LINE -- the request line.  Expected format:
  ;"            'GET^<TEMPLATE FIELD IEN-8927.1>^<any optional tag value>'   File #8927.1 is TIU TEMPLATE FIELD
  ;"         or 'GET^ALL^<any optional tag value>'
  ;"        ADFN -- IEN in PATIENT file.
  ;"        ADT -- the reference data, and returned value should be last value PRIOR to this date.  
  ;"Output Format put into OUT:     
  ;"        OUT(#)='VALUE^<Template Field IEN>^<value of db control>^<any tag value>^<DT of value>^<FldName>^<TypeSymbol>
  ;"        If, for some reason, multiple values have been stored for given template field,
  ;"        and with same reference date, then multiple lines will be added with same 
  ;"        Template Field IEN, each with their separate values 
  ;"
  ;"Result "1^OK", or "-1^Error mesage"  
  NEW RESULT SET RESULT="1^OK"
  NEW IDX SET IDX=$ORDER(OUT(""),-1)+1
  NEW ADDED SET ADDED=0
  SET LINE=$GET(LINE)
  NEW TAGVAL SET TAGVAL=$PIECE(LINE,"^",3)
  SET ADFN=+$GET(ADFN) IF ADFN'>0 DO  GOTO GVDN
  . SET TMGRESULT="-1^Expected valid patient IEN.  Got '"_$GET(ADFN)_"'."
  NEW VALUE SET VALUE=""  ;"default
  SET ADT=$GET(ADT) IF ADT=0 SET ADT="999999999"  
  SET ADT=+$ORDER(^TMG(22730,ADFN,"DT",ADT+0.0000001),-1)
  NEW IEN8927D1 SET IEN8927D1=$PIECE(LINE,"^",2)
  IF $$UP^XLFSTR(IEN8927D1)'="ALL" DO  GOTO GVDN
  . SET RESULT=$$GET1(.OUT,ADFN,ADT,TAGVAL,IEN8927D1)
  ;"GET ALL VALUES
  SET IEN8927D1=0
  FOR  SET IEN8927D1=$ORDER(^TMG(22730,ADFN,"DT",ADT,"FLD",IEN8927D1)) QUIT:(IEN8927D1'>0)!(+RESULT=-1)  DO
  . SET RESULT=$$GET1(.OUT,ADFN,ADT,TAGVAL,IEN8927D1)
GVDN ;  
  QUIT RESULT
  ; 
GET1(OUT,ADFN,ADT,TAGVAL,IEN8927D1)  ;
  NEW RESULT SET RESULT="1^OK"
  NEW VALUE SET VALUE=""  ;"default
  NEW ADDED SET ADDED=0
  NEW IDX SET IDX=$ORDER(OUT(""),-1)+1
  NEW ZN SET ZN=$GET(^TIU(8927.1,IEN8927D1,0))
  NEW TYPE SET TYPE=$PIECE(ZN,"^",2)
  NEW FLDNAME SET FLDNAME=$PIECE(ZN,"^",1)
  IF ADT'>0 GOTO G1VDN  ;"Leave value at default value
  IF $DATA(^TMG(22730,ADFN,"DT",ADT,"FLD",IEN8927D1))=0 GOTO G1VDN
  SET ZN=$GET(^TMG(22730,ADFN,"DT",ADT,"FLD",IEN8927D1,0))
  IF TYPE="D" SET VALUE=$PIECE(ZN,"^",3)
  ELSE  IF TYPE="N" SET VALUE=$PIECE(ZN,"^",4) 
  ELSE  DO
  . SET VALUE=$GET(^TMG(22730,ADFN,"DT",ADT,"FLD",IEN8927D1,"TXT"))  ;"Only 1 field on this node.  
  . IF ("ETW"'[TYPE)!(VALUE'="") QUIT
  . NEW TMGARR,TMGREF,TMGMSG
  . NEW IENS SET IENS=IEN8927D1_","_ADT_","_ADFN_","
  . SET TMGREF=$$GET1^DIQ(22730.011,IENS,2,"","TMGARR","TMGMSG")
  . IF $DATA(TMGMSG("DIERR")) DO  QUIT
  . . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  . NEW IDX SET IDX=0,VALUE=""
  . FOR  SET IDX=$ORDER(TMGARR(IDX)) QUIT:IDX=""  DO
  . . IF VALUE'="" SET VALUE=VALUE_"{{LF}}"
  . . SET VALUE=VALUE_$GET(TMGARR(IDX))
G1VDN  ;
  SET OUT(IDX)="VALUE^"_IEN8927D1_"^"_VALUE_"^"_TAGVAL_"^"_ADT_"^"_FLDNAME_"^"_TYPE
  QUIT RESULT
  ;
SETVAL(OUT,LINE,ADFN,ADT,IEN8925) ;
  ;"Expected format of LINE: 'SET^<TEMPLATE FIELD IEN>^<value of db control>'
  ;"   <value of db control> should not contain any '^' characters.  Converted if found.
  ;"Format put into OUT:     'STATUS^<Template FIELD IEN>^<server message, if any>^<any tag value>
  ;"Result "1^OK", or "-1^Error, encountered"  Also, individual result is put into OUT array
  NEW IDX SET IDX=$ORDER(OUT(""),-1)+1
  SET LINE=$GET(LINE)
  NEW IEN8927D1 SET IEN8927D1=$PIECE(LINE,"^",2)
  NEW VALUE SET VALUE=$PIECE(LINE,"^",3,99)
  IF VALUE["^" SET VALUE=$$REPLSTR^TMGSTU3(VALUE,"^","%%/\%%")    
  IF VALUE[$CHAR(13,10) SET VALUE=$$REPLSTR^TMGSTUT3(VALUE,$CHAR(13,10),"{{LF}}")    
  NEW STATUS SET STATUS="OK"   ;"default
  NEW IENS SET IENS=$$GETIENS(.ADFN,.IEN8927D1,.ADT,.IEN8925)  ;"IENS IN 22730.011 
  IF +IENS'>0 SET STATUS=$PIECE(IENS,"^",2) GOTO SVDN  
  NEW TYPE SET TYPE=$PIECE($GET(^TIU(8927.1,IEN8927D1,0)),"^",2)
  NEW TMGFDA,TMGMSG,TMGIDE
  NEW ISWP SET ISWP=0
  IF "ETW"[TYPE DO
  . IF $LENGTH(VALUE)<250 SET TYPE="Z" QUIT  ;"force storage of short text in TEXT field
  . SET ISWP=1
  IF TYPE="D" SET TMGFDA(22730.011,IENS,.03)=VALUE
  IF TYPE="N" SET TMGFDA(22730.011,IENS,.04)=VALUE
  ELSE  IF ISWP=0 SET TMGFDA(22730.011,IENS,1)=VALUE
  DO FILE^DIE("","TMGFDA","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO SVDN
  . SET STATUS=$$GETERRST^TMGDEBU2(.TMGMSG)
  IF ISWP'=1 GOTO SVDN
  NEW SUBIEN SET SUBIEN=+$GET(TMGIEN(1))
  IF SUBIEN'>0 DO  GOTO SVDN
  . SET STATUS="Unable to located IEN of added record in file #22729.01"
  NEW TMGWPARR DO SPLIT2AR^TMGSTUT2(VALUE,"{{LF}}",.TMGWPARR)
  KILL TMGMSG DO WP^DIE(22730.011,IENS,2,"K","TMGWPARR",TMGMSG)
  IF $DATA(TMGMSG("DIERR")) DO  GOTO SVDN
  . SET STATUS=$$GETERRST^TMGDEBU2(.TMGMSG)
SVDN ;
  NEW RESULT SET RESULT="1^OK"
  IF STATUS'="OK" SET RESULT="-1^Error encountered"
  SET OUT(IDX)="STATUS^"_IEN8927D1_"^"_STATUS 
  QUIT RESULT
  ;  
KILLVALS(OUT,ADFN,ADT,IEN8925)  ;
  ;"Delete all entries matching DFN, DT, and IEN8925 input values.
  NEW IDX SET IDX=$ORDER(OUT(""),-1)+1
  NEW TMGRESULT SET TMGRESULT="1^NOT FOUND"  
  SET ADFN=+$GET(ADFN) IF ADFN'>0 DO  GOTO KVDN
  . SET TMGRESULT="-1^Expected valid patient IEN.  Got '"_$GET(ADFN)_"'."
  SET ADT=+$GET(ADT) IF ADT'>0 DO  GOTO KVDN
  . SET TMGRESULT="-1^Expected FM format date/time of event.  Got '"_$GET(ADT)_"'."
  SET IEN8925=+$GET(IEN8925) IF IEN8925'>0 DO  GOTO KVDN
  . SET TMGRESULT="-1^Expected valid IEN in TIU DOCUMENT file.  Got '"_$GET(IEN8925)_"'."
  NEW SUBIEN SET SUBIEN=0
  FOR  SET SUBIEN=$ORDER(^TMG(22730,"TIU",IEN8925,ADFN,SUBIEN)) QUIT:+SUBIEN'>0  DO
  . IF SUBIEN'=ADT QUIT
  . NEW DIK,DA SET DA=ADT,DA(1)=ADFN
  . SET DIK="^TMG(22730,"_DA(1)_",""DT"","
  . DO ^DIK  ;"KILL THE RECORD
  . SET TMGRESULT="1^OK"
KVDN  ;  
  SET OUT(IDX)="STATUS^"_IEN8925_"^"_$PIECE(TMGRESULT,"^",2,99) 
  QUIT TMGRESULT
  ;
GETIENS(ADFN,IEN8927D1,ADT,IEN8925,MAKE)  ;"GET RECORD IN SUBFILE OF [TMG PATIENT DISCRETE DATA] (#22730.011)
  ;"Purpose: Get a record for use, possibly creating if it doesn't already exist
  ;"Input: ADFN -- PATIENT file IEN
  ;"       IEN8927D1 -- IEN in file 8927.1 (TIU TEMPLATE FIELD)
  ;"       ADT -- FM format date-time of event.
  ;"       IEN8925 -- OPTIONAL.  IEN of linked TIU document (if any)
  ;"       MAKE -- Optional.  Default is 1.  If 0, then not made if not found.
  ;"               PASS BY REFERENCE to get information back:
  ;"               MAKE(1)="NEW" if record newly recreated.
  ;"               MAKE(1)="OLD" if record already existed recreated.
  ;"Result: IENS for 22730.011, or -1^Message if error, or 0 if not found and AUTOMAKE=0
  NEW TMGRESULT SET TMGRESULT=-2
  SET MAKE=+$GET(MAKE,1)
  SET ADFN=+$GET(ADFN) IF ADFN'>0 DO  GOTO GRDN
  . SET TMGRESULT="-1^Expected valid patient IEN.  Got '"_$GET(ADFN)_"'."
  IF $DATA(^TMG(22730,ADFN))=0 DO  GOTO:(+TMGRESULT<0) GRDN
  . IF MAKE'=1 QUIT
  . NEW TMGFDA,TMGMSG,TMGIEN
  . SET TMGIEN(1)=ADFN
  . SET TMGFDA(22730,"+1,",.01)=ADFN
  . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  . IF $DATA(TMGMSG("DIERR")) DO  QUIT
  . . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  . SET TMGRESULT=0  ;"to be changed below if all OK. 
  SET IEN8925=+$GET(IEN8925)
  SET ADT=+$GET(ADT) IF ADT'>0 DO  GOTO GRDN
  . SET TMGRESULT="-1^Expected FM format date/time of event.  Got '"_$GET(ADT)_"'."
  IF $DATA(^TMG(22730,ADFN,"DT",ADT))=0 DO  GOTO:(+TMGRESULT<0) GRDN
  . IF MAKE'=1 QUIT
  . NEW TMGFDA,TMGMSG,TMGIEN,TMGIENS
  . SET TMGIEN(1)=ADT
  . SET TMGIENS="+1,"_ADFN_","
  . SET TMGFDA(22730.01,TMGIENS,.01)=ADT
  . IF IEN8925>0 SET TMGFDA(22730.01,TMGIENS,.02)=IEN8925
  . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  . IF $DATA(TMGMSG("DIERR")) DO  QUIT
  . . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  . SET TMGRESULT=0  ;"to be changed below if all OK. 
  SET IEN8927D1=+$GET(IEN8927D1) IF $DATA(^TIU(8927.1,IEN8927D1,0))=0 DO  GOTO GRDN
  . SET TMGRESULT="-1^Expected valid IEN in TIU TEMPLATE FIELD file.  Got '"_$GET(IEN8927D1)_"'."
  IF $DATA(^TMG(22730,ADFN,"DT",ADT,"FLD",IEN8927D1))=0 DO  GOTO:(+TMGRESULT<0) GRDN
  . IF MAKE'=1 QUIT
  . NEW TMGFDA,TMGMSG,TMGIEN,TMGIENS
  . SET TMGIEN(1)=IEN8927D1
  . SET TMGIENS="+1,"_ADT_","_ADFN_","  
  . SET TMGFDA(22730.011,TMGIENS,.01)=IEN8927D1    ;"//ADT
  . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  . IF $DATA(TMGMSG("DIERR")) DO  QUIT
  . . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  . SET MAKE(1)="NEW"  
  . SET TMGRESULT=0  ;"to be changed below if all OK. 
  ELSE  SET MAKE(1)="OLD"
  SET TMGRESULT=IEN8927D1_","_ADT_","_ADFN_","  ;"Note: each record and subrecord is DINUM'd (.01 value = IEN)
GRDN ;
  QUIT TMGRESULT
  ;  
