TMGSIPH0 ;TMG/kst/SIPHON PROGRAM, FOR TRANSFERRING VISTA INSTANCES ;11/27/09, 2/2/14, 3/19/21
         ;;1.0;TMG-LIB;**1**;11/27/09
 ;
 ;"TMG SIPHON PROGRAM, FOR TRANSFERRING VISTA INSTANCE
 ;"----===== SERVER-SIDE CODE ====------
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
 ;"HANDLMSG(MESSAGE) -- A message handler for communication between VistA instances.
 ;
 ;"=======================================================================
 ;" API -- Private Functions.
 ;"=======================================================================
 ;"HANDLGET(REF) --A handler for GET command between VistA instances.  Get a ^global node
 ;"HANDLGDD(FILENUM) -- Return Data Dictionary information about specified file.
 ;"GETSUBDD(SUBFILENUM) -- Return DD information about subfiles (and sub-subfiles)
 ;"HANDLORD(REF) --A handler for ORDREF command between VistA instances. Will get ^Global node that is $ORDER'd after REF
 ;"HANDLNRS(FILENUM) -- Return the highest record number in given file.
 ;"HANDGRXR(PARAMS) -- Return one record, and associated cross-reference entries
 ;"SENDFLDS(FILENUM,IEN) -- send any .01 fields VALUES of any pointers OUT
 ;"HANDLDIC(PARAMS) -- Do a ^DIC lookup in file for value.
 ;"
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"DILF, XLFSTR, TMGSIPHU, TMGKERN2, TMGFMUT2
 ;"=======================================================================
 ;
 ;"=============================================================
 ;" Below will be core of server-side request handler.
 ;"=============================================================
 
 ;"=======================================================================
 ;"=======================================================================
 ;"!!Notice!!: The XML export engine, which this code uses, has been revised
 ;"  as of 3/19/21.  The engine calls back to writer functions, and the
 ;"  signatures may have changed slightly.  If/when this code is used
 ;"  in the future, this will have to be debugged...  //kt  3/19/21
 ;"=======================================================================
 ;"=======================================================================
 
 
 
HANDLMSG(MESSAGE) ;
        ;"Purpose: A message handler for communication between VistA instances.
        ;"Input MESSAGE -- This is the message send from the client, who will be asking for
        ;"                 information and records etc from this instance.
        ;" Format:  'Command|parameters'
        ;" -----------------------
        ;" GET|REF                 -- Get a ^global node
        ;" GET DD|FILENUM          -- return Data Dictionary information about specified file.
        ;" ORDREF|REF              -- Get ^Global node that is $ORDER'd after REF
        ;" NUMRECS|FILENUM         -- Return the highest record number in given file
        ;" PT XREF|FILENUM         -- Prepair PT XREF for all records pointing INTO specified file.
        ;" WIPE PT XREF|           -- Delete the last run of PT XREF, so it can be refreshened.
        ;" PREP XREFS|FILENUM^[1]  -- Make a xref of cross-references (a backward xref)
        ;" GET REF & FILE XREF|REF^FILENUM^IENS -- Return one reference, and associated FILENUM cross-reference entries
        ;" GET RECORD & XREF|FILENUM^IEN -- Return one record, and associated cross-reference entries
        ;" GET PTRS IN|FILENUM^IEN -- Get a listing of all pointers INTO requested record
        ;" DO DIC|FILENUM^VALUE    -- Do a ^DIC lookup in file for value.
        ;" GET XREF AGE            -- Get age of server-side PT xrefs etc, in HOURS
        ;" GET .01 FLD|FILENUM^IEN -- Return INTERNAL format of .01 field.  Doesn't support subfiles.
        ;" DUMP REC|FILENUM^IENS^SHOWEMPTY -- Display dump of server record.
        ;" GET IEN LIST|FILENUM    -- Get a listing of all records (IEN's) in specified file.
        ;" GET IEN HDR|FILENUM     -- Get Last IEN,HighestIEN from file header.
        ;" -----------------------
        ;"Results: None
        ;
        NEW CMD SET CMD=$$UP^XLFSTR($PIECE(MESSAGE,"|",1))
        SET CMD=$$TRIM^XLFSTR(CMD)
        NEW PARAMS SET PARAMS=$$TRIM^XLFSTR($PIECE(MESSAGE,"|",2,99))
        DO DEBUGMSG^TMGKERN2("In HANDLMSG. CMD="_CMD_" & PARAMS="_PARAMS)
        DO
        . NEW $ETRAP SET $ETRAP="write ""#ERROR TRAPPED#  "",$ZSTATUS,! SET $ETRAP="""",$ecode="""""
        . IF CMD="GET" DO HANDLGET(PARAMS) QUIT
        . IF CMD="GET DD" DO HANDLGDD(PARAMS) QUIT
        . IF CMD="ORDREF" DO HANDLORD(PARAMS) QUIT
        . IF CMD="NUMRECS" DO HANDLNRS(PARAMS) QUIT
        . IF CMD="PT XREF" DO HNDLPTIX^TMGSIPH2(PARAMS) QUIT
        . IF CMD="WIPE PT XREF" DO KILLPTIX^TMGFMUT2 QUIT
        . IF CMD="GET PTRS IN" DO GETPTIN^TMGSIPH2(PARAMS) QUIT
        . IF CMD="PREP XREFS" DO BAKXREF^TMGSIPH2(PARAMS) QUIT
        . IF CMD="GET RECORD & XREF" DO HANDGRXR(PARAMS) QUIT
        . IF CMD="GET REF & FILE XREF" DO HANDGRFX(PARAMS) QUIT
        . IF CMD="DO DIC" DO HANDLDIC(PARAMS) QUIT
        . IF CMD="GET XREF AGE" DO GETXRAGE^TMGSIPH2 QUIT
        . IF CMD="GET .01 FLD" DO GET01FLD^TMGSIPH2(PARAMS) QUIT
        . IF CMD="DUMP REC" DO DUMPREC(PARAMS) QUIT
        . IF CMD="GET IEN LIST" DO HANDIENL^TMGSIPH2(PARAMS) QUIT
        . IF CMD="GET IEN HDR" DO HANDLIENHDR^TMGSIPH2(PARAMS) QUIT
        . ELSE  DO
        . . DO SEND^TMGKERN2("Got: ["_MESSAGE_"].  Server is $JOB="_$JOB)
        QUIT
 ;"=============================================================
 ;"=============================================================
 ;
HANDLGET(REF) ;
        ;"Purpose: A handler for GET command between VistA instances.  Get a ^global node
        ;"Input --REF -- reference to a global.  May be in Open or Closed format
        ;"Results: none
        ;"Output: Will WRITE output to current device (should be socket to other instance)
        ;
        NEW OREF SET OREF=$$OREF^DILF(REF)
        NEW LEN SET LEN=$LENGTH(OREF)
        SET REF=$$CREF^DILF(REF)
        NEW DONE SET DONE=0
        FOR  DO  QUIT:(DONE>0)
        . IF $DATA(@REF)#10 DO
        . . DO SEND^TMGKERN2(REF_"=")
        . . DO SEND^TMGKERN2("="_$GET(@REF))
        . SET REF=$QUERY(@REF)
        . IF (REF="")!($QSUBSCRIPT(REF,1)="") SET DONE=1 QUIT
        . IF $EXTRACT(REF,1,LEN)'=OREF SET DONE=1 QUIT
        QUIT
 ;
 ;
HANDLGDD(FILENUM) ; "Handle Get DD
        ;"Purpose: to return Data Dictionary information about specified file.
        SET FILENUM=+$GET(FILENUM)
        NEW REF SET REF=$NAME(^DD(FILENUM))
        DO HANDLGET(REF)
        SET REF=$NAME(^DIC(FILENUM))
        DO HANDLGET(REF)
        ;"Get nodes from INDEX file
        NEW IDX SET IDX=""
        FOR  SET IDX=$ORDER(^DD("IX","B",FILENUM,IDX)) QUIT:(IDX="")  DO
        . SET REF=$NAME(^DD("IX",IDX))
        . DO HANDLGET(REF)
        NEW FLD SET FLD=0
        FOR  SET FLD=$ORDER(^DD(FILENUM,FLD)) QUIT:(+FLD'>0)  DO
        . NEW PT SET PT=+$PIECE($GET(^DD(FILENUM,FLD,0)),"^",2)
        . QUIT:(PT'>0)
        . IF $DATA(^DD(PT,0,"UP")) DO GETSUBDD(PT)
        QUIT
 ;
 ;
GETSUBDD(SUBFILENUM)
        ;"Purpose: Return DD information about subfiles (and sub-subfiles)
        NEW REF SET REF=$NAME(^DD(SUBFILENUM))
        DO HANDLGET(REF)
        NEW PT SET PT=+$PIECE($GET(^DD(SUBFILENUM,0)),"^",2)
        QUIT:(PT'>0)
        IF $DATA(^DD(PT,0,"UP")) DO GETSUBDD(PT)
        QUIT
 ;
 ;
HANDLORD(REF) ;
        ;"Purpose: A handler for ORDREF command between VistA instances.
        ;"         Will get ^Global node that is $ORDER'd after REF
        ;"              e.g.  ^TIU(8925,"")  --> returns node ^TIU(8925,0,
        ;"                    ^TIU(8925,     --> returns node ^TIU(8925.1,
        ;"Input --REF -- reference to a global.  May be in Open or Closed format
        ;"Results: none
        ;"Output: Will WRITE output to current device (should be socket to other VistA instance)
        ;"
        NEW CREF SET CREF=$$CREF^DILF(REF)
        SET REF=$$ORDREF^TMGSIPHU(CREF)
        IF REF'="" DO HANDLGET(REF)
        QUIT
 ;
 ;
HANDLNRS(FILENUM) ;
        ;"Purpose: Return the highest record number in given file.
        ;"Input: FILENUM -- The fileman number of the file to return info for.
        ;"Results: None
        DO SEND^TMGKERN2($$GETNUMREC^TMGSIPHU(FILENUM))
        QUIT
 ;
 ;
HANDGRFX(PARAMS) ;" Handler for GET REF & FILE XREF|REF^FILENUM^IENS
        ;"Purpose: Return one reference, and associated FILENUM cross-reference entries
        ;"         Note: It is anticipated that this will be used to get subfile entries.
        ;"Input: PARAMS :  REF^FILENUM^IENS
        ;"              REF -- should be in OPEN format (ending in a ',')
        ;"              FILENUM -- the subfile number.
        ;"              IENS -- A standard IENS string
        ;"Output: Will WRITE output to current device (should be socket).  Format
        ;"        <Ref>=
        ;"        =<Value>
        ;"        <Ref>=
        ;"        =<Value>
        ;"       ...
        ;"       %PTRSOUT%^PointedToFile^IEN^FIELD_VALUE
        ;"       %PTRSOUT%^PointedToFile^IEN^FIELD_VALUE
        ;"       ...
        ;"Result: none
        ;"NOTE: This function will assume that an xref of all the cross-references has
        ;"      already been SET up by calling BAKXREF^TMGSIPH1(FILENUM).  This can be
        ;"      triggered on the client side by calling QUERY="PREP XREFS|<filenumber>"
        SET PARAMS=$GET(PARAMS)
        NEW GREF SET GREF="^"_$PIECE(PARAMS,"^",2)  ;"Ref itself has a ^ in it.
        NEW FILENUM SET FILENUM=$PIECE(PARAMS,"^",3)
        NEW IENS SET IENS=$PIECE(PARAMS,"^",4)
        DO HANDLGET(GREF) ;
        ;"Now send XRef entries for IEN.
        DO BAKXREF^TMGSIPH2(FILENUM_"^1") ;"organize XRefs IF needed, keeping current orangization array
        NEW REF SET REF=""
        FOR  SET REF=$ORDER(^TMG("PTXREF","XREFS",FILENUM,IENS,REF)) QUIT:(REF="")  DO
        . DO SEND^TMGKERN2(REF_"=")
        . DO SEND^TMGKERN2("="_$GET(^TMG("PTXREF","XREFS",FILENUM,IENS,REF)))
        DO SENDFLDS(FILENUM,IENS) ;"Send values of .01 fields for all pointers OUT from record
        QUIT
 ;
 ;
HANDGRXR(PARAMS) ;
        ;"Purpose: Return one record, and associated cross-reference entries
        ;"Input: PARAMS :  Filenumber^IEN
        ;"Output: Will WRITE output to current device (should be socket).  Format
        ;"        <Ref>=
        ;"        =<Value>
        ;"        <Ref>=
        ;"        =<Value>
        ;"       ...
        ;"       %PTRSOUT%^PointedToFile^IEN^FIELD_VALUE
        ;"       %PTRSOUT%^PointedToFile^IEN^FIELD_VALUE
        ;"       ...
        ;"Result: none
        ;"NOTE: This function will assume that an xref of all the cross-references has
        ;"      already been SET up by calling BAKXREF^TMGSIPH1(FILENUM).  This can be
        ;"      triggered on the client side by calling QUERY="PREP XREFS|<filenumber>"
        ;
        NEW FILENUM,IEN
        SET PARAMS=$GET(PARAMS)
        SET FILENUM=+PARAMS
        SET IEN=$PIECE(PARAMS,"^",2)
        IF (FILENUM'>0)!(IEN'>0) QUIT
        NEW GREF SET GREF=$GET(^DIC(FILENUM,0,"GL"))
        IF GREF="" QUIT
        DO HANDLGET(GREF_IEN_",") ;
        ;"Now send XRef entries for IEN.
        NEW REF SET REF=""
        DO BAKXREF^TMGSIPH2(FILENUM_"^1") ;"organize XRefs IF needed, keeping current orangization array
        FOR  SET REF=$ORDER(^TMG("PTXREF","XREFS",FILENUM,IEN,REF)) QUIT:(REF="")  DO
        . DO SEND^TMGKERN2(REF_"=")
        . DO SEND^TMGKERN2("="_$GET(^TMG("PTXREF","XREFS",FILENUM,IEN,REF)))
        DO SENDFLDS(FILENUM,IEN) ;"Send values of .01 fields for all pointers OUT from record
HGXDN   QUIT
 ;
 ;
SENDFLDS(FILENUM,IEN) ;
        ;"Purpose to send any .01 fields VALUES of any pointers OUT
        ;"Input: FILENUM -- the file containing the record to be scanned
        ;"       IEN -- The record number being scanned.
        ;"Results: none
        ;"Output: Values will be sent to client via SEND^TMGKERN2.  Format as follows:
        ;"           %PTRSOUT%^PointedToFile^IEN^FIELD_VALUE
        ;"           %PTRSOUT%^PointedToFile^IEN^FIELD_VALUE
        NEW TALLY
        KILL ^TMG("TMGSIPH","NEEDED RECORDS","PTOUT",FILENUM)
        IF $$REAL1PTOUT^TMGSIPH1(FILENUM,IEN,.TALLY)=1 DO
        . NEW REF SET REF=""
        . FOR  SET REF=$ORDER(^TMG("TMGSIPH","DD",FILENUM,"PTR OUT",REF)) QUIT:(REF="")  DO
        . . NEW INFO SET INFO=""
        . . FOR  SET INFO=$ORDER(^TMG("TMGSIPH","DD",FILENUM,"PTR OUT",REF,INFO)) QUIT:(INFO="")  DO
        . . . NEW PCE SET PCE=+INFO
        . . . NEW P2FILE SET P2FILE=$PIECE(INFO,"^",2)
        . . . NEW P2REF SET P2REF=$PIECE(INFO,"^",3)
        . . . NEW IENDEPTH SET IENDEPTH=$PIECE(INFO,"^",4)
        . . . NEW ISVIRT SET ISVIRT=($PIECE(INFO,"^",5)="V")
        . . . NEW TEMP SET TEMP=IEN KILL IEN SET IEN=TEMP  ;"kill subnodes
        . . . NEW OKCOMBO
        . . . FOR  DO  QUIT:(OKCOMBO=0)
        . . . . SET OKCOMBO=$$IENCOMBO^TMGFMUT2(REF,IENDEPTH,.IEN) ;"Sets up IEN(n).. needed for @REF
        . . . . QUIT:(OKCOMBO=0)
        . . . . NEW PT SET PT=$PIECE($GET(@REF),"^",PCE)
        . . . . IF ISVIRT,$PIECE(PT,";",2)'=P2REF QUIT  ;"Loop to handle PTR with different INFO entry (V-Ptrs stored as IEN;OREF)
        . . . . SET PT=+PT QUIT:(PT'>0)
        . . . . NEW VALUE SET VALUE=$$FLD01^TMGSIPH2(P2FILE_"^"_PT) ;
        . . . . DO SEND^TMGKERN2("%PTRSOUT%^"_P2FILE_"^"_PT_"^"_VALUE)
        . . . KILL IEN("DONE"),IEN("INIT")
        ;"KILL ^TMG("TMGSIPH","UNRESOLVED",FILENUM)
        KILL ^TMG("TMGSIPH","NEEDED RECORDS","PTOUT",FILENUM)
        KILL ^TMG("TMGSIPH","DD",FILENUM)
        QUIT
 ;
 ;
HANDLDIC(PARAMS) ;
        ;"Purpose: Do a ^DIC lookup in file for value.
        ;"Input: Params:  this is FILENUM^LOOKUPVALUE
        ;"Result: Will send back value of Y to client
        SET PARAMS=$GET(PARAMS)
        NEW DIC SET DIC=+$PIECE(PARAMS,"^",1)
        NEW Y,X SET X=$PIECE(PARAMS,"^",2)
        SET DIC(0)="M"
        DO ^DIC
        DO SEND^TMGKERN2(Y)
        QUIT
 ;
 ;
DUMPREC(PARAMS) ;
        ;"Purpose: To DO a record dump of a server-side record, sending output back to client
        ;"Input: Params -- FILENUM^IENS^SHOWEMPTY
        NEW FILENUM,IENS,SHOWEMPTY
        SET PARAMS=$GET(PARAMS)
        SET FILENUM=+PARAMS
        SET IENS=$PIECE(PARAMS,"^",2)
        IF (FILENUM'>0)!(IENS'>0) QUIT
        SET SHOWEMPTY=+$PIECE(PARAMS,"^",3)
        NEW OPTION
        SET OPTION("WRITE REC LABEL FN")="WRLABEL^TMGSIPH0"
        SET OPTION("WRITE FLD LABEL FN")="WFLABEL^TMGSIPH0"
        SET OPTION("WRITE LINE FN")="WLINE^TMGSIPH0"
        SET OPTION("WRITE WP LINE")="WWPLINE^TMGSIPH0"
        NEW TMGDUMPS ;"Will be used with global scope
        DO DUMPREC^TMGDEBU3(FILENUM,IENS,SHOWEMPTY,,.OPTION)
        QUIT
 ;
 ;
WRLABEL(INDENTS,IEN,ENDER)
        ;"Purpose: To actually WRITE out labels for record starting and ending.
        ;"Input: IEN -- the IEN (record number) of the record
        ;"       ENDER -- OPTIONAL IF 1, then ends field.
        ;"Note: also uses globally scoped variable TMGDUMPS
        ;"Results: none.
        ;"Note: Used by DumpRec2 above, with callback from TMGXMLE2
        SET TMGDUMPS=$GET(TMGDUMPS)
        IF +$GET(ENDER)>0 DO
        . IF TMGDUMPS="" SET TMGDUMPS="."
        ELSE  SET TMGDUMPS=TMGDUMPS_"     Multiple Entry #"_IEN
        DO SEND^TMGKERN2(TMGDUMPS)
        SET TMGDUMPS=""
        QUIT
 ;
WFLABEL(INDENTS,LABEL,FIELD,TYPE,ENDER)
        ;"Purpose: This is the code that actually does writing of labels etc for output
        ;"      This is a CUSTOM CALL BACK function called by WRIT1FLD^TMGXMLE2
        ;"Input: LABEL -- OPTIONAL -- Name of label, to WRITE after  'label='
        ;"       FIELD -- OPTIONAL -- Name of field, to WRITE after  'id='
        ;"       TYPE -- OPTIONAL -- TYPEof field, to WRITE after  'type='
        ;"       ENDER -- OPTIONAL IF 1, then ends field.
        ;"Note: also uses globally scoped variable TMGDUMPS
        ;"Note: Used by DumpRec2 above, with callback from TMGXMLE2
        ;"To WRITE out <FIELD label="NAME" id=".01" type="FREE TEXT"> or </FIELD>
        SET TMGDUMPS=$GET(TMGDUMPS)
        IF +$GET(ENDER)>0 DO
        . IF TMGDUMPS="" SET TMGDUMPS="."
        . DO SEND^TMGKERN2(TMGDUMPS)
        . SET TMGDUMPS=""
        ELSE  DO
        . IF $GET(FIELD)'="" SET TMGDUMPS=TMGDUMPS_$$RJ^XLFSTR(FIELD,6," ")_"-"
        . IF $GET(LABEL)'="" SET TMGDUMPS=TMGDUMPS_LABEL_" "
        . ;"IF $GET(TYPE)'="" SET TMGDUMPS=TMGDUMPS_"type="""_TYPE_""" "
        . SET TMGDUMPS=TMGDUMPS_": "
        QUIT
 ;
WLINE(INDENTS,LINE)
        ;"Purpose: To actually WRITE out labels for record starting and ending.
        ;"Input: Line -- The line of text to be written out.
        ;"Note: also uses globally scoped variable TMGDUMPS
        ;"Note: Used by DumpRec2 above, with callback from TMGXMLE2
        SET TMGDUMPS=$GET(TMGDUMPS)_$GET(LINE)
        QUIT
 ;
WWPLINE(INDENTS,LINE)
        ;"Purpose: To actually WRITE out line from WP field
        ;"Input: Line -- The line of text to be written out.
        ;"Note: also uses globally scoped variable TMGDUMPS
        ;"Note: Used by DumpRec2 above, with callback from TMGXMLE2
        SET TMGDUMPS=$GET(TMGDUMPS)_$GET(LINE)
        IF TMGDUMPS="" SET TMGDUMPS="."
        DO SEND^TMGKERN2(TMGDUMPS)
        SET TMGDUMPS=""
        QUIT
