TMGSIPH3 ;TMG/kst/SIPHON PROGRAM, FOR TRANSFERRING VISTA INSTANCES ;11/27/09, 2/2/14
         ;;1.0;TMG-LIB;**1**;11/27/09
 ;
 ;"TMG SIPHON PROGRAM, FOR TRANSFERRING VISTA INSTANCE
 ;"Support functions for transferring files from server
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
 ;"TRANSFILE(JNUM) -- move a remote file to local machine, overwriting local entries.
 ;"GET01FLD(JNUM,FILENUM,IEN) -Get .01 field (internal format) from server.
 ;"TRANS1FIL(JNUM,FILENUM) -move a remote file to local machine, overwriting local entries.
 ;"QRYSERVER(JNUM) -- display a given reference from the server
 ;"TRANSREF(JUNUM) -- move an absolute reference from server to local
 ;"ASKNEEDED(JNUM,OUTARRAY,INOUT,OPTIONS) --review records of needed records, and
        ;"         ask user which file, or
        ;"         which records to get, and return results of selected in array.
        ;"         This can handle either the list of needed pointers IN or OUT.
 ;"NUMNEEDED(JNUM,INOUT) -- count number of records needed from server.
 ;"CHCK4SIM(FILENUM,ARRAY,ANIEN,VALUE01,IENS) -- look at an array and see IF there is similar record already on the client.
 ;"XTRACT01FLD(ARRAY) ; --remove .01 Field values from array returned from GET RECORD & XREF, and store
 ;"GETANDFIXREC(JNUM,FILENUM,IEN,OVERWRITE,TALLY,INOUT) -- request a record from server, and integrate into local vista,
        ;"         resolving pointers locally to point to newly downloaded record.
 ;"HANDLNEEDED(JNUM,INOUT,AUTOMODE) --Ask user which records to get from server, then get them and update
        ;"         pointer translation table.

 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"TMGUSRIF, XLFSTR
 ;"=======================================================================
 ;
 ;
TRANSFILE(JNUM)
        ;"Purpose: to move a remote file to local machine, overwriting local entries.
        ;"Input: JNUM -- The job number of the background client process
        ;"Results: none
        NEW X,Y,DIC,ARRAY,%
        SET DIC=1,DIC(0)="MAEQ"
TF1     WRITE "Pick file to transfer COMPLETELY, or to resume transfer from",!
        DO ^DIC WRITE !
        IF +Y'>0 DO  QUIT:(+Y'>0)!(%=-1)
        . SET %=1
        . WRITE "File not found on this client.  Do you want to select a file",!
        . WRITE "to transfer from the server" DO YN^DICN WRITE !
        . QUIT:(%'=1)
        . WRITE "Pick file ON SERVER to transfer COMPLETELY: "
        . READ Y:$GET(DTIME,3600),!
        . IF Y["^" QUIT
        . NEW QUERY,REPLY,ERROR,RESULT
        . SET QUERY="DO DIC|1^"_Y
        . DO MSGCLIENT^TMGKERN2(JNUM,QUERY,.REPLY,.ERROR,30)
        . IF $DATA(ERROR) WRITE ERROR,! SET Y=0 QUIT
        . SET Y=$GET(REPLY(1))
        . IF +Y>0 SET ^TMG("TMGSIPH","DD",+Y,"DIFF")=0
        FOR  DO  QUIT:(DDOK'=0)
        . SET DDOK=$$PREPDD^TMGSIPH1(JNUM,+Y)
        . QUIT:(DDOK=1)
        . WRITE "Before records can be transferred from the server, the local data",!
        . WRITE "dictionary must be made compatible.  Must work on this now.",!
        . DO PRESS2GO^TMGUSRI2
        . SET DDOK=+$GET(^TMG("TMGSIPH","DD",+Y,"DIFF"))
        GOTO TF1:(DDOK'=1)
        DO TRANS1FIL(JNUM,+Y)
        GOTO TF1
 ;
 ;
GET01FLD(JNUM,FILENUM,IEN) ;
        ;"Purpose: Get .01 field (internal format) from server, or return previously obtained value.
        ;"Input: JNUM -- The job number of the background client process
        ;"       FILENUM -- The file number to compare.
        ;"       IEN -- the record to query -- Server-side IEN, not client IEN
        ;"Result: returns the .01 value or "" IF problem
        SET RESULT=$GET(^TMG("TMGSIPH",".01 VALUE",FILENUM,IEN))
        IF RESULT'="" GOTO G1DN
        NEW QUERY,REPLY,ERROR,RESULT
        SET QUERY="GET .01 FLD|"_FILENUM_"^"_IEN
        DO MSGCLIENT^TMGKERN2(JNUM,QUERY,.REPLY,.ERROR,30)
        IF $DATA(ERROR) WRITE ERROR,!
        SET RESULT=$GET(REPLY(1))
        SET ^TMG("TMGSIPH",".01 VALUE",FILENUM,IEN)=RESULT
G1DN    QUIT RESULT
 ;
 ;
TRANS1FIL(JNUM,FILENUM) ;
        ;"Purpose: to move a remote file to local machine, overwriting local entries.
        ;"Input: JNUM -- The job number of the background client process
        ;"       FILENUM -- The file number to transfer. (Not a subfile)
        ;"Output: Will SET output globals:
        ;"      ^TMG("TMGSIPH","PT XLAT",FILENUM,RemoteIEN)=LocalIEN
        ;"      ^TMG("TMGSIPH","NEEDED RECORDS","PTOUT",FILENUM,RemotePointer,ReferToNodeToBeCorrected,Piece#OfNode)=""
        ;"Results: none
        ;
        NEW MAXNUM
        NEW QUERY,ERROR,RESULT,REPLY
        SET QUERY="NUMRECS|"_FILENUM
        DO MSGCLIENT^TMGKERN2(JNUM,QUERY,.REPLY,.ERROR,15)
        IF $DATA(ERROR) WRITE ERROR,! GOTO T1FD
        SET MAXNUM=+$GET(REPLY(1))
        IF MAXNUM'>0 DO  GOTO T1FD
        . WRITE "Error: number of records=",MAXNUM,!
        NEW STARTTIME SET STARTTIME=$H
        NEW GLREF SET GLREF=$GET(^DIC(FILENUM,0,"GL"))
        NEW REF SET REF=$GET(^TMG("TMGSIPH","DOWNLOADED",FILENUM,"#PRIOR RUN#"))
        NEW % SET %=1 ;"Default=Y
        IF REF'="" DO
        . WRITE "Continue transfer of records from point of last run"
        . DO YN^DICN WRITE !
        . IF %=2 SET REF=""
        IF %=-1 GOTO T1FD
        IF REF="" SET REF=$$CREF^DILF(GLREF_""""",")
        SET GLREF=$$CREF^DILF(GLREF)
        NEW QL SET QL=$QLENGTH(REF)
        WRITE "Press ESC to abort...",!
        NEW REC SET REC=""
        NEW TMGABORT
        FOR  DO  QUIT:(REF="")!(TMGABORT=1)
        . SET TMGABORT=$$USRABORT^TMGUSRI2() QUIT:(TMGABORT=1)
        . SET QUERY="ORDREF|"_REF
        . DO MSGCLIENT^TMGKERN2(JNUM,QUERY,.REPLY,.ERROR,30)
        . IF $DATA(ERROR) DO  QUIT
        . . WRITE ERROR,!
        . . SET REF=""
        . IF $DATA(REPLY)=0 SET REF="" QUIT
        . DO STOREDATA^TMGSIPHU(.REPLY)
        . SET ^TMG("TMGSIPH","DOWNLOADED",FILENUM,"#PRIOR RUN#")=REF
        . SET REF=$GET(REPLY(1)) QUIT:(REF="")
        . SET REF=$EXTRACT(REF,1,$LENGTH(REF)-1)
        . SET REF=$$QSUBS^TMGSIPHU(REF,QL)
        . IF $QSUBSCRIPT(REF,QL)=REC do
        . . WRITE "ERROR: Record number didn't increase!",!
        . SET REC=$QSUBSCRIPT(REF,QL)
        . IF (+REC=REC) DO
        . . IF $$REAL1PTOUT^TMGSIPH1(FILENUM,REC) ;"Ignore function result
        . . SET ^TMG("TMGSIPH","PT XLAT",FILENUM,REC)=REC ;"remote and local IEN's are same
        . . SET ^TMG("TMGSIPH","DOWNLOADED",FILENUM,REC)=REC
        . IF (REC#10)=0 DO
        . . DO PROGBAR^TMGUSRI2(REC,"Progress: "_REC,0,MAXNUM,70,STARTTIME)
T1FD    QUIT
 ;
 ;
QRYSERVER(JNUM) ;
        ;"Purpose: To display a given reference from the server
        ;"Input: JNUM -- The job number of the background client process
        SET JNUM=+$GET(JNUM)
        QUIT:(+JNUM'>0)
        NEW QUERY,ERROR,RESULT,REPLY
        FOR  DO  QUIT:(QUERY="^")
        . READ "Enter reference> ",QUERY:$GET(DTIME,3600),!
        . IF (QUERY="")!(QUERY="^") SET QUERY="^" QUIT
        . ELSE  SET QUERY="GET|"_QUERY
        . DO MSGCLIENT^TMGKERN2(JNUM,QUERY,.REPLY,.ERROR,5)
        . IF $DATA(ERROR) WRITE ERROR,!
        . IF $DATA(REPLY) do
        . . WRITE "reply:",!
        . . DO ZWRITE^TMGZWR("REPLY")
        QUIT
 ;
 ;
TRANSREF(JNUM) ;
        ;"Purpose: To move an absolute reference from server to local
        SET JNUM=+$GET(JNUM)
        QUIT:(+JNUM'>0)
        WRITE "This will allow an arbitrary global to be transferred",!
        WRITE "from the server.",!
        NEW REF,QUERY,ERROR,RESULT,REPLY,%
        FOR  DO  QUIT:(REF="^")
        . READ "Enter reference (e.g. ""^ABC(123,"" or ^ to QUIT)> ",REF:$GET(DTIME,3600),!
        . IF (REF="")!(REF="^") SET REF="^" QUIT
        . SET REF=$$CREF^DILF(REF)
        . SET QUERY="GET|"_REF
        . DO MSGCLIENT^TMGKERN2(JNUM,QUERY,.REPLY,.ERROR,5)
        . IF $DATA(ERROR) WRITE ERROR,! QUIT
        . IF $DATA(REPLY) DO ZWRITE^TMGZWR("REPLY") WRITE !
        . SET %=1
        . IF $DATA(@REF) DO  QUIT:(%'=1)
        . . WRITE "WARNING: There is already data locally at ",REF,!
        . . WRITE "Do you want to OVERWRITE this local data"
        . . SET %=2
        . . DO YN^DICN WRITE !
        . DO STOREDATA^TMGSIPHU(.REPLY)
        . WRITE "Data stored locally.",!,!
        . KILL REPLY
        QUIT




ASKNEEDED(JNUM,OUTARRAY,INOUT,OPTIONS) ;
        ;"Purpose: To review records of needed records, and ask user which file, or
        ;"         which records to get, and return results of selected in array.
        ;"         This can handle either the list of needed pointers IN or OUT.
        ;"Input: JNUM -- The job number of the background client process
        ;"       OUTARRAY -- PASS BY REFERNCE, an OUT PARAMETER.  Filled as follows
        ;"           OUTARRAY(FileNum,RecordNum)=""
        ;"       INOUT -- OPTIONAL -- Default is "PTOUT".  Should be "PTIN" or "PTOUT"
        ;"       OPTIONS -- OPTIONAL default is 0.  See SELNEEDED for details.
        ;"Results: None.
        ;"NOTE: uses ^TMG("TMGSIPH","NEEDED RECORDS","PTOUT",FILENUM,RemotePointer,RefToNodeToBeCorrected,Piece#OfNode)=""
        ;"           ^TMG("TMGSIPH","NEEDED RECORDS","PTIN",FILENUM,IEN)=""
        ;
        NEW REF SET REF=$NAME(^TMG("TMGSIPH","NEEDED RECORDS",INOUT))
        DO SELNEEDED(JNUM,.OUTARRAY,REF,.OPTIONS)
        QUIT
 ;
 ;
SELNEEDED(JNUM,OUTARRAY,REF,OPTIONS) ;
        ;"Purpose: To review an array of needed records, and ask user which file, or
        ;"         which records to get, and return results of selected in array.
        ;"Input: JNUM -- The job number of the background client process
        ;"       OUTARRAY -- PASS BY REFERNCE, an OUT PARAMETER.  Filled as follows
        ;"           OUTARRAY(FileNum,RecordNum)=""
        ;"       REF -- PASS BY NAME -- The name of the variable holding the records to ask from.  Variable
        ;"              array should have this format:
        ;"                @REF@(FILENUM,RPTR)=""
        ;"                @REF@(FILENUM,RPTR)=""
        ;"       OPTIONS -- OPTIONAL default is 0.  If 1, then all records are processed without asking.
        ;"         OPTIONS("MAP MODE")=1 OPTIONAL, IF exists, then different header is displayed
        ;"         OPTIONS("NUMNEEDED")=1 OPTIONAL, IF exists, will only get up to 200 records
        ;"         OPTIONS("HEADER")=<header text> OPTIONAL.  If present, will be used for header display
        ;"Results: None.
        NEW TMGARRAY,TMGSEL,TMGSEL2
        KILL OUTARRAY
        SET INOUT=$GET(INOUT) IF INOUT'="PTIN" SET INOUT="PTOUT"
        NEW FILENUM SET FILENUM=""
        NEW AUTOMODE SET AUTOMODE=(+$GET(OPTIONS)=1)
        FOR  SET FILENUM=$ORDER(@REF@(FILENUM)) QUIT:(+FILENUM'>0)  DO
        . NEW DISPSTR SET DISPSTR="Get records from REMOTE file #"_FILENUM_" ("
        . SET DISPSTR=DISPSTR_$$FILENAME^TMGFMUT2(FILENUM)_")"
        . SET TMGARRAY(DISPSTR)=FILENUM
        NEW STIME SET STIME=$H
        NEW SHOWPROG SET SHOWPROG=0
        NEW TMGCT SET TMGCT=0
        NEW TMGDONE SET TMGDONE=0
        NEW SHORTLST SET SHORTLST=+$GET(OPTIONS("NUMNEEDED"))
        NEW HEADER
        IF $DATA(OPTIONS("HEADER")) DO
        . SET HEADER=$GET(OPTIONS("HEADER"))
        ELSE  DO
        . IF $GET(OPTIONS("MAP MODE"))=1 DO
        . . SET HEADER="Select File(s) to MAP to local records in. Press <ESC><ESC> when Done."
        . ELSE  SET HEADER="Select File(s) to get REMOTE records from. Press <ESC><ESC> when Done."
        IF AUTOMODE MERGE TMGSEL=TMGARRAY
        ELSE  DO SELECTR2^TMGUSRI3("TMGARRAY","TMGSEL",HEADER)
        NEW TMGABORT SET TMGABORT=0
        NEW IDX SET IDX=""
        FOR  SET IDX=$ORDER(TMGSEL(IDX)) QUIT:(IDX="")!TMGABORT!TMGDONE  DO
        . SET FILENUM=$GET(TMGSEL(IDX)) QUIT:FILENUM=""
        . NEW FNAME SET FNAME=$$FILENAME^TMGFMUT2(FILENUM)
        . NEW RPTR SET RPTR=""
        . KILL TMGARRAY,TMGSEL2
        . NEW RECCT SET RECCT=0
        . NEW SELALL SET SELALL=0
        . NEW ASKED SET ASKED=0
        . IF AUTOMODE=0 WRITE "GETTING NAMES OF RECORDS...",!
        . FOR  SET RPTR=$ORDER(@REF@(FILENUM,RPTR)) QUIT:(RPTR="")!SELALL!TMGABORT!TMGDONE  DO
        . . NEW DISPSTR SET DISPSTR="File: "_FNAME_", record #"_$$RJ^XLFSTR(RPTR,6)
        . . IF AUTOMODE=0 SET DISPSTR=DISPSTR_" -- "_$$GET01FLD(JNUM,FILENUM,RPTR)
        . . SET TMGARRAY(DISPSTR)=RPTR
        . . SET RECCT=RECCT+1
        . . SET TMGCT=TMGCT+1
        . . IF (SHOWPROG=0),($$HDIFF^XLFDT($H,STIME,2)>10) DO  ;"Turn on progress bar after 10 seconds.
        . . . SET SHOWPROG=1
        . . IF (SHOWPROG=1),(TMGCT>500) DO
        . . . DO PROGBAR^TMGUSRI2(100,"Gathering list of needed records "_RECCT,-1,-1,70,STIME)
        . . . SET TMGCT=0
        . . IF (RECCT>200),(ASKED=0) DO
        . . . IF SHORTLST SET TMGDONE=1,RECCT=0 QUIT
        . . . SET ASKED=1
        . . . IF AUTOMODE=1 QUIT
        . . . NEW MENU,USRSLCT
        . . . SET MENU(0)="File "_FNAME_" has > 200 records."
        . . . SET MENU(1)="Automatically Select ALL records"_$CHAR(9)_"AutoSelALL"
        . . . SET MENU(2)="Show LONG list to allow picking individual records"_$CHAR(9)_"SelectList"
        . . . NEW DONE SET DONE=0
        . . . FOR  DO  QUIT:(DONE=1)!(TMGABORT)
        . . . . WRITE #
        . . . . SET USRSLCT=$$MENU^TMGUSRI2(.MENU,"^")
        . . . . SET DONE=1
        . . . . IF USRSLCT="^" SET TMGABORT=1 QUIT
        . . . . IF USRSLCT="AutoSelALL" SET SELALL=1 QUIT
        . . . . IF USRSLCT="SelectList" QUIT
        . . . . ELSE  SET DONE=0
        . IF TMGABORT QUIT
        . IF (RECCT=1)!AUTOMODE!SELALL DO
        . . NEW TMGSKIP SET TMGSKIP=0
        . . SET TMGCT=0
        . . NEW ONEREC SET ONEREC=""
        . . FOR  SET ONEREC=$ORDER(@REF@(FILENUM,ONEREC)) QUIT:(ONEREC="")!TMGSKIP  DO
        . . . SET TMGSEL2(ONEREC)=ONEREC
        . . . IF SHORTLST,(TMGCT>200) SET TMGSKIP=1,TMGDONE=1 QUIT
        . . . SET TMGCT=TMGCT+1
        . . . SET RECCT=RECCT+1
        . . . IF (SHOWPROG=0),($$HDIFF^XLFDT($H,STIME,2)>10) DO  ;"Turn on progress bar after 10 seconds.
        . . . . SET SHOWPROG=1
        . . . IF (SHOWPROG=1),(TMGCT>500) DO
        . . . . DO PROGBAR^TMGUSRI2(100,"Gathering list of needed records "_RECCT,0,100,70,STIME)
        . . . . SET TMGCT=0
        . . SET SELALL=1
        . IF SELALL=0 DO
        . . IF $GET(OPTIONS("MAP MODE"))=1 DO
        . . . SET HEADER="Select records to MAP to local records.  Press <ESC><ESC> when Done."
        . . ELSE  SET HEADER="Select records to get from Server.  Press <ESC><ESC> when Done."
        . . DO SELECTR2^TMGUSRI3("TMGARRAY","TMGSEL2",HEADER)
        . NEW I2 SET I2=""
        . FOR  SET I2=$ORDER(TMGSEL2(I2)) QUIT:(I2="")  DO
        . . SET RPTR=$GET(TMGSEL2(I2))
        . . SET OUTARRAY(FILENUM,RPTR)=""
        ;
        QUIT
 ;
 ;
NUMNEEDED(JNUM,INOUT)
        ;"Purpose: To count number of records needed from server.
        ;"Input: JNUM -- The job number of the background client process
        ;"       INOUT -- OPTIONAL -- Default is "PTOUT".  Should be "PTIN" or "PTOUT"
        ;"Output: Returns the number of records needed.
        ;"
        NEW GETARRAY,FILENUM,RESULT
        SET INOUT=$GET(INOUT) IF INOUT'="PTIN" SET INOUT="PTOUT"
        NEW MODE SET MODE=1,MODE("NUMNEEDED")=1  ;"Will limit number counting to 200 mg
        DO ASKNEEDED(JNUM,.GETARRAY,INOUT,.MODE)
        SET FILENUM=0
        SET RESULT=0
        NEW TMGCT SET TMGCT=0
        NEW STIME SET STIME=$H
        NEW SHOWPROG SET SHOWPROG=0
        FOR  SET FILENUM=$ORDER(GETARRAY(FILENUM)) QUIT:(FILENUM="")  DO
        . NEW IEN SET IEN=""
        . FOR  SET IEN=$ORDER(GETARRAY(FILENUM,IEN),-1) QUIT:(IEN="")  DO
        . . SET RESULT=RESULT+1
        . . SET TMGCT=TMGCT+1
        . . IF (SHOWPROG=0),($$HDIFF^XLFDT($H,STIME,2)>5) DO  ;"Turn on progress bar after 5 seconds.
        . . . SET SHOWPROG=1
        . . IF (SHOWPROG=1),(TMGCT>1000) DO
        . . . DO PROGBAR^TMGUSRI2(100,"Counting records: "_TMGCT,0,100,70)
        . . . SET TMGCT=0
        IF TMGCT>200 SET TMGCT=TMGCT_"+"
        QUIT TMGCT
 ;
 ;
CHCK4SIM(FILENUM,ARRAY,ANIEN,VALUE01,IENS)
        ;"Purpose: To look at an array, as returned from server, and see IF there is
        ;"         a similar record already on the client.
        ;"Input:  FILENUM -- the fileman filenumber of file to get from remote server
        ;"        ARRAY -- The global record array, as returned from server.
        ;"        ANIEN -- PASS BY REFERENCE.  Will be filled with IEN match
        ;"                If IENS is passed (i.e. IF dealing with a subfile), then ANIEN is passed
        ;"                back in standard IENS format (e.g. '7,1234,')
        ;"        VALUE01 -- OPTIONAL.  This allows a .01 value to be passed.  If provided, then
        ;"                the ARRAY won't be searched for a .01 value.
        ;"        IENS -- OPTIONAL.  If FILENUM is a subfile, then IENS is needed for lookup.
        ;"                 IENS is modified, so **DON'T** PASS BY REFERENCE
        ;"Results: 0 IF no similar record already on the local server (i.e. NO MATCH)
        ;"         1 IF a match WAS found.
        ;"Output: ANIEN is modified.
        ;"NOTE: If .01 field of passed record array matches to 2 or more records, then NO MATCH resulted
        ;"      Also, IF file does not have a "B" cross reference, then NO MATCH resulted.
        ;"      Also, the first 30 characters (only) are tested for match in "B" xref.
        ;
        NEW RESULT SET RESULT=0
        SET ANIEN=0
        SET FILENUM=+$GET(FILENUM) ;" If in format of 'SubFile{ParentFile', then strip off parent filenum.
        NEW GREF SET GREF=$$GETGREF^TMGFMUT2(FILENUM,.IENS) ;"IENS not used IF not subfile.
        IF GREF="" GOTO C4SDN
        NEW BREF SET BREF=GREF_"""B"")"
        NEW SAVIENS SET SAVIENS=$GET(IENS)
        SET $PIECE(IENS,",",1)=""  ;"e.g. '7,2345,' --> ',2345,' to specify parent, but no particular subfile entry
        IF $DATA(@BREF)=0 GOTO C4SDN
        NEW CGREF SET CGREF=$$CREF^DILF(GREF)
        NEW GREFLEN SET GREFLEN=$QLENGTH(CGREF)
        NEW VALUE SET VALUE=$GET(VALUE01)
        NEW TMGI SET TMGI=0
        FOR  SET TMGI=$ORDER(ARRAY(TMGI)) QUIT:(TMGI="")!(VALUE'="")  DO  ;"Find .01 value
        . NEW REF SET REF=$GET(ARRAY(TMGI))
        . SET REF=$EXTRACT(REF,1,$LENGTH(REF)-1)
        . SET TMGI=TMGI+1
        . IF REF="" SET TMGI="" QUIT
        . IF $QSUBSCRIPT(REF,GREFLEN+2)'=0 QUIT  ;"Only check 0 node.
        . IF $QLENGTH(REF)'=(GREFLEN+2) QUIT  ;"Only allow  ^GREF(xxx,xxx,IEN,0)
        . SET VALUE=$EXTRACT($GET(ARRAY(TMGI)),2,10000)
        . SET VALUE=$PIECE(VALUE,"^",1)
        IF VALUE="" GOTO C4SDN
        IF (FILENUM'=9999999.27),$GET(^TMG("TMGSIPH","SKIP CHCK4SIM",FILENUM,VALUE))=1 GOTO C4SDN
        NEW TMGOUT,TMGMSG
        DO FIND^DIC(FILENUM,IENS,"@;.01I","BOQUX",VALUE,"*","B","","","TMGOUT","TMGMSG")
        DO SHOWDIER^TMGDEBU2(.TMGOUT)
        NEW CT SET CT=+$GET(TMGOUT("DILIST",0))
        IF CT=1 DO
        . ;"Ensure matched local record didn't actually come from server
        . NEW LPTR SET LPTR=+$GET(TMGOUT("DILIST",2,1))
        . IF $DATA(^TMG("TMGSIPH","DOWNLOADED",FILENUM,LPTR)) QUIT
        . IF SAVIENS'="" DO
        . . SET ANIEN=SAVIENS
        . . SET $PIECE(ANIEN,",",1)=LPTR
        . ELSE  SET ANIEN=LPTR
        . SET RESULT=1
        ELSE  IF CT>100 DO
        . SET ^TMG("TMGSIPH","SKIP CHCK4SIM",FILENUM,VALUE)=1
        ;
C4SDN   QUIT RESULT
 ;
 ;
XTRACT01FLD(ARRAY) ;
        ;"Purpose: To remove pointed-to .01 Field values from array returned from GET RECORD & XREF,
        ;"         and store these for future reference.  Removes %PTRSOUT%
        ;"Input: ARRAY -- PASS BY REFERENCE.  Results returned from GET RECORD & XREF.  Format:
        ;"          ARRAY(1)="<Ref>="
        ;"          ARRAY(2)="=<Value>"
        ;"          ARRAY(3)="<Ref>="
        ;"          ARRAY(4)="=<Value>"

        ;"          ...
        ;"          ARRAY(20)="%PTRSOUT%^PointedToFile^IEN^FIELD_VALUE"
        ;"          ARRAY(21)="%PTRSOUT%^PointedToFile^IEN^FIELD_VALUE"
        ;"          ...
        ;"Results: none
        NEW RESULT SET RESULT=0 ;Default to error.
        NEW SHOWPG SET SHOWPG=0
        NEW TMGCT SET TMGCT=0
        NEW STIME SET STIME=$H
        NEW TMGI SET TMGI=""
        FOR  SET TMGI=$ORDER(ARRAY(TMGI)) QUIT:(+TMGI'>0)  DO
        . IF (SHOWPG=0),($$HDIFF^XLFDT($H,STIME,2)>15) DO  ;"Turn on progress bar after 15 seconds.
        . . SET SHOWPG=1
        . . SET TMGMIN=$ORDER(ARRAY(0))
        . . SET TMGMAX=$ORDER(ARRAY(""),-1)
        . IF (SHOWPG=1),(TMGCT>2000) DO
        . . DO PROGBAR^TMGUSRI2(TMGI,"Extracting pointers from server data",TMGMIN,TMGMAX,70,STIME)
        . . SET TMGCT=0
        . SET TMGCT=TMGCT+1
        . IF $GET(ARRAY(TMGI))'["%PTRSOUT%" QUIT
        . NEW FILENUM SET FILENUM=$PIECE(ARRAY(TMGI),"^",2)
        . NEW IEN SET IEN=$PIECE(ARRAY(TMGI),"^",3)
        . NEW VALUE SET VALUE=$PIECE(ARRAY(TMGI),"^",4)
        . KILL ARRAY(TMGI)
        . SET ^TMG("TMGSIPH",".01 VALUE",FILENUM,IEN)=VALUE
        QUIT
 ;
 ;
GETANDFIXREC(JNUM,FILENUM,RPTR,OVERWRITE,TALLY,INOUT) ;
        ;"Purpose: To request a record from server, and integrate into local vista,
        ;"         resolving pointers locally to point to newly downloaded record.
        ;"Input:  JNUM -- The job number of the background client process
        ;"        FILENUM -- the fileman filenumber of file to get from remote server
        ;"                      Can be in format of SubFileNum{ParentFileNum{GrandParent....
        ;"        RPTR -- The record number on the server to get.
        ;"                      Can be in IENS format, e.g. '7,34532,' IF FILENUM is a subfile.
        ;"        OVERWRITE -- OPTIONAL.  If 1, then prior local records may be overwritten.
        ;"                                If '?' then figure out IF should overwrite, asking user IF needed.
        ;"        TALLY -- OPTIONAL.  PASS BY REFERENCE.  An array to keep progress stats.  Format:
        ;"                 TALLY("ALREADY LOCAL FOUND")=#
        ;"                 TALLY("DOWNLOADED")=#
        ;"                 TALLY(FILENUM,"NEW REC NEEDED")=#
        ;"                 TALLY("UNNEEDED RECORDS")=#
        ;"       INOUT -- OPTIONAL -- Default is "PTOUT".  Should be "PTIN" or "PTOUT"
        ;"NOTE:  Gobal ^TMG("TMGSIPH","NEEDED RECORDS","PTOUT") used, with format as below:
        ;"             ^TMG("TMGSIPH","NEEDED RECORDS","PTOUT",FILENUM,RemotePointer,RefToNodeToBeCorrected,Piece#OfNode)=""
        ;"       As pointers are resolved, the entries will be KILLED from the above global
        ;"Results: 1 if OK, -1 IF error, -2 IF abort
        ;
        NEW QUERY,REPLY,ERROR,NEWIEN
        NEW RESULT SET RESULT=-1 ;"Default to error
        NEW TMGABORT SET TMGABORT=0
        SET INOUT=$GET(INOUT) IF INOUT'="PTIN" SET INOUT="PTOUT"
        SET OVERWRITE=$GET(OVERWRITE)
        SET FILENUM=$GET(FILENUM)
        NEW ISSUBFIL SET ISSUBFIL=$$ISSUBFIL^TMGFMUT2(+FILENUM)
        IF +RPTR'>0 GOTO GAFRD
        SET NEWIEN=RPTR        ;"Default of not changing IEN
        SET FILENUM=+FILENUM IF FILENUM'>0 GOTO GAFRD  ;"If subfile, strip parent file number.
        NEW LPTR SET LPTR=$GET(^TMG("TMGSIPH","PT XLAT",FILENUM,RPTR))
        IF (+LPTR>0) DO  GOTO GAFR1  ;"Remote records already downloaded, so just link to it.
        . SET NEWIEN=LPTR
        . SET TALLY("ALREADY LOCAL FOUND")=+$GET(TALLY("ALREADY LOCAL FOUND"))+1
        NEW CONHANDL SET CONHANDL=$GET(^TMG("TMGSIPH","CONFLICT HANDL",FILENUM))
        NEW USELOCAL SET USELOCAL=0
        IF CONHANDL="UseLocal" DO  GOTO:(USELOCAL=1) GAF2
        . ;"If pointer is to a file specified as ALWAYS LOCAL, Handle here, IF .01 value is known.
        . NEW VALUE SET VALUE=$GET(^TMG("TMGSIPH",".01 VALUE",FILENUM,RPTR))
        . QUIT:(VALUE="")
        . NEW ANIEN
        . IF $$CHCK4SIM(FILENUM,,.ANIEN,VALUE,RPTR)=0 QUIT  ;"RPTR (as IENS) not used IF not subfile.
        . IF +ANIEN'>0 QUIT
        . SET NEWIEN=ANIEN
        . SET USELOCAL=1
        NEW GREF SET GREF=$$GETGREF^TMGFMUT2(FILENUM,RPTR) ;"RPTR (as IENS) not used IF not subfile.
        IF GREF="" GOTO GAFRD
        NEW ZREF SET ZREF=GREF_"0)"
        NEW CGREF SET CGREF=$$CREF^DILF(GREF)
        IF ISSUBFIL DO
        . NEW REF SET REF=GREF_+RPTR
        . SET QUERY="GET REF & FILE XREF|"_REF_"^"_FILENUM_"^"_RPTR
        ELSE  DO
        . SET QUERY="GET RECORD & XREF|"_FILENUM_"^"_RPTR
        DO MSGCLIENT^TMGKERN2(JNUM,QUERY,.REPLY,.ERROR,30)
        IF $DATA(ERROR) DO  GOTO GAFRD
        . WRITE ERROR,!
        IF $DATA(REPLY)=0 DO  GOTO GAFR0 ;"No data on server for record, so zero pointers
        . SET NEWIEN=0
        DO XTRACT01FLD(.REPLY)
        NEW SIMIEN
        IF $$CHCK4SIM(FILENUM,.REPLY,.SIMIEN,,RPTR) DO  ;"A prior similar record already is on client.
        . SET NEWIEN=SIMIEN  ;"If dealing with subfiles, SIMIEN will be in IENS format.
        NEW REF SET REF=GREF_+NEWIEN_")"
        IF $DATA(@REF) DO
        . NEW TEMP SET TEMP=$$GETTARGETIEN^TMGSIPHU(FILENUM,.REPLY,.NEWIEN)
        . SET REF=GREF_+NEWIEN_")" ;"NEWIEN might have changed.
        . IF TEMP="ABORT" SET RESULT=-2,TMGABORT=1 QUIT
        . IF TEMP="USELOCAL" SET USELOCAL=1 QUIT
        . IF TEMP="OVERWRITE" DO  QUIT   ;"OVERWRITE LOCAL RECORD #LPTR (KILL, THEN STORE later)
        . . KILL @REF
GAF2    IF ($GET(TMGABORT)=1)!(NEWIEN'>0) GOTO GAFRD
        IF USELOCAL=1 DO  GOTO GAFR0
        . SET TALLY("ALREADY LOCAL FOUND")=$GET(TALLY("ALREADY LOCAL FOUND"))+1
        IF $$STOREDAS^TMGSIPHU(FILENUM,NEWIEN,.REPLY)=-1 GOTO GAFRD
        SET $PIECE(@ZREF,"^",4)=+$PIECE($GET(@ZREF),"^",4)+1 ;"Update File Header to reflect added records
        IF +NEWIEN>$PIECE(@ZREF,"^",3) SET $PIECE(@ZREF,"^",3)=NEWIEN
        IF $$REAL1PTOUT^TMGSIPH1(FILENUM,NEWIEN,.TALLY) ;"Scan for pointers out.  Ignore function result
        SET ^TMG("TMGSIPH","DOWNLOADED",FILENUM,NEWIEN)=RPTR
        SET TALLY("DOWNLOADED")=+$GET(TALLY("DOWNLOADED"))+1
GAFR0   SET ^TMG("TMGSIPH","PT XLAT",FILENUM,RPTR)=NEWIEN  ;"Add entry to Pointer translation table.
        IF (RPTR'=NEWIEN) SET ^TMG("TMGSIPH","NEED RE-XREF",FILENUM)="" ;"Flag for re-cross referencing again later.
        IF USELOCAL=1 SET ^TMG("TMGSIPH","PT XLAT",FILENUM,RPTR,"L")=1 ;"Signal that local record was used
GAFR1   DO UNNEEDPTR^TMGSIPHU(FILENUM,RPTR,NEWIEN,INOUT,.TALLY)
        IF INOUT="PTIN" KILL ^TMG("TMGSIPH","NEEDED RECORDS","PTIN",FILENUM,RPTR)
        IF $$NEEDPTIN(FILENUM)!(INOUT="PTIN") DO  ;"See IF pointers IN are needed
        . IF LPTR=RPTR QUIT  ;"No need for relinking IF this record was already local.
        . DO GETPTIN^TMGSIPH4(JNUM,FILENUM,RPTR)
        SET RESULT=1
GAFRD   IF (RESULT'=-1)&(TMGABORT=1) SET RESULT=-2
        QUIT RESULT
 ;
 ;
NEEDPTIN(FILENUM) ;
        ;"Purpose: To have a centralized location for which files should automatically trigger a request
        ;"         for pointers-IN
        ;"NOTE:
        NEW RESULT SET RESULT=0
        IF FILENUM=2 SET RESULT=1
        ELSE  IF (FILENUM=9000001) SET RESULT=1
        ELSE  IF (FILENUM=8925) SET RESULT=1
        ELSE  IF (FILENUM["8925.") SET RESULT=1
        QUIT RESULT
 ;
 ;
AUTONEEDED(JNUM) ;
        ;"Purpose: To automatically get all pointers IN records and also pointers OUT records
        ;"Input: JNUM -- The job number of the background client process
        ;"Results: None
        ;
        NEW NPTO,NPTI,TALLY
AN1     SET NPTO=$$NUMNEEDED^TMGSIPH3(JNUM,"PTOUT")
        IF NPTO>0 IF $$HANDLNEEDED^TMGSIPH3(JNUM,"PTOUT",1,.TALLY)=-1 GOTO ANDN
        SET NPTI=$$NUMNEEDED^TMGSIPH3(JNUM,"PTIN")
        IF (NPTO=0)&(NPTI=0) GOTO ANDN
        IF NPTI>0 IF $$HANDLNEEDED^TMGSIPH3(JNUM,"PTIN",1,.TALLY)=-1 GOTO ANDN
        GOTO AN1
ANDN    IF $DATA(TALLY) WRITE ! DO ZWRITE^TMGZWR("TALLY")
        ELSE  WRITE "No records needed auto-downloading.",!
        DO PRESS2GO^TMGUSRI2
        QUIT
 ;
 ;
HANDLNEEDED(JNUM,INOUT,AUTOMODE,TALLY) ;
        ;"Purpose: Ask user which records to get from server, then get them and update
        ;"         pointer translation table.
        ;"Input: JNUM -- The job number of the background client process
        ;"       INOUT -- OPTIONAL -- Default is "PTOUT".  Should be "PTIN" or "PTOUT"
        ;"       AUTOMODE -- OPTIONAL default is 0.  If 1, then all records are processed without asking.
        ;"       TALLY -- OPTIONAL.  PASS BY REFERENCE.  An array to show downloads.
        ;"Results: 1 if OK, -1 IF abort.
        ;
        NEW GETARRAY,FILENUM,IEN,STIME,TMGCT,SHOWPROG,QUERY,ERROR,TMGMAX
        SET INOUT=$GET(INOUT) IF INOUT'="PTIN" SET INOUT="PTOUT"
        NEW TMGABORT SET TMGABORT=0
        NEW RESULT SET RESULT=1 ;"Default to success
HN1     DO ASKNEEDED(JNUM,.GETARRAY,INOUT,.AUTOMODE)
        IF $DATA(GETARRAY)=0 GOTO HNDN
        ;"Process JUST ONE record from each file to begin with, to try to minimize user interaction after that.
        SET FILENUM=0
        FOR  SET FILENUM=$ORDER(GETARRAY(FILENUM)) QUIT:(FILENUM="")!(TMGABORT=1)  DO
        . QUIT:($$DDOK^TMGSIPH1(JNUM,FILENUM)'=1)
        . QUIT:($$PREPXREF^TMGSIPH1(JNUM,FILENUM)'=1)
        . SET IEN=$ORDER(GETARRAY(FILENUM,""),-1) QUIT:(IEN="")
        . NEW TMP SET TMP=$$GETANDFIXREC(JNUM,FILENUM,IEN,"?",.TALLY,INOUT)
        . IF TMP=-2 SET TMGABORT=1 QUIT
        . IF TMP=-1 DO HNDLGAFE(FILENUM,IEN,.TMGABORT) QUIT
        . KILL GETARRAY(FILENUM,IEN) ;"Prevent reprocessing below
        ;"Now loop through ALL the files and records
        SET FILENUM=0,SHOWPROG=0
        FOR  SET FILENUM=$ORDER(GETARRAY(FILENUM)) QUIT:(FILENUM="")!(TMGABORT=1)  DO
        . QUIT:($$DDOK^TMGSIPH1(JNUM,FILENUM)'=1)
        . QUIT:($$PREPXREF^TMGSIPH1(JNUM,FILENUM)'=1)
        . SET TMGMAX=-1,STIME=$H,TMGCT=1,IEN=""
        . FOR  SET IEN=$ORDER(GETARRAY(FILENUM,IEN),-1) QUIT:(IEN="")!(TMGABORT=1)  DO
        . . IF TMGMAX=-1 SET TMGMAX=IEN
        . . SET TMGABORT=$$USRABORT^TMGUSRI2() QUIT:(TMGABORT=1)
        . . SET TMGCT=TMGCT+1
        . . NEW TMP SET TMP=$$GETANDFIXREC(JNUM,FILENUM,IEN,"?",.TALLY,INOUT)
        . . IF TMP=-2 SET TMGABORT=1 QUIT
        . . IF TMP=-1 DO HNDLGAFE(FILENUM,IEN,.TMGABORT) QUIT
        . . IF (SHOWPROG=0),(($PIECE($H,",",2)-$PIECE(STIME,",",2))>10) SET SHOWPROG=1
        . . IF SHOWPROG,(TMGCT#10=0) DO
        . . . WRITE #
        . . . DO PROGBAR^TMGUSRI2(TMGCT,"Progress: "_TMGCT,0,TMGMAX,70,STIME)
        . . . IF $DATA(TALLY) WRITE ! DO ZWRITE^TMGZWR("TALLY")
        IF (AUTOMODE=1)&(TMGABORT'=1) GOTO HN1 ;"Loop back and see IF more records are now needed.
        ELSE  DO
        . IF $DATA(TALLY) WRITE ! DO ZWRITE^TMGZWR("TALLY")
        . DO PRESS2GO^TMGUSRI2
HNDN    IF TMGABORT SET RESULT=-1
        QUIT RESULT
 ;
 ;
HNDLGAFE(FILENUM,RPTR,TMGABORT) ;" Handle GETANDFIXREC error.
        ;"Input: FILENUM -- The file containing the bad record
        ;"       RPTR -- the IEN of the bad record, on the server
        ;"       TMGABORT -- PASS BY REFERENCE.  An OUT parameter to abort.
        WRITE !,"Error encountered processing FILE ",$$FILENAME^TMGFMUT2(FILENUM)," (#"_FILENUM_"), REC #"_IEN,!
        NEW % SET %=2
        WRITE "Mark REC #",IEN," in FILE #",FILENUM," as an invalid server record"
        DO YN^DICN WRITE !
        IF %=-1 SET TMGABORT=1
        IF %=1 DO BADPTR(FILENUM,IEN)
HGAFEDN QUIT
 ;
 ;
BADPTR(FILENUM,RPTR) ;
        ;"Purpose: To handle a pointer to a bad record on the server.
        ;"Input: FILENUM -- The file containing the bad record
        ;"       RPTR -- the IEN of the bad record, on the server
        ;"NOTE: globally-scoped variable TMGABORT may be set.
        ;"Results: None
        NEW MENU,USRSLCT
LC2     KILL MENU,USRSLCT
        SET MENU(0)="Pick Option for Handling INVALID server record"
        NEW IDX SET IDX=1
        SET MENU(IDX)="Examine who need this bad record"_$CHAR(9)_"Examine",IDX=IDX+1
        SET MENU(IDX)="Redirect pointer to a different local record"_$CHAR(9)_"RedirToLocal",IDX=IDX+1
        SET MENU(IDX)="Change pointer to a NULL pointer"_$CHAR(9)_"MakeNull",IDX=IDX+1
        SET MENU(IDX)="Backup without making any changes"_$CHAR(9)_"Quit",IDX=IDX+1
        SET MENU(IDX)="Abort"_$CHAR(9)_"Abort",IDX=IDX+1
        ;
        WRITE #
        SET USRSLCT=$$MENU^TMGUSRI2(.MENU,"^")
        IF USRSLCT="^" GOTO LC3
        IF USRSLCT=0 SET USRSLCT=""
        IF USRSLCT="Examine" DO  GOTO:(TMGABORT=1) LC3 GOTO LC2
        . NEW ARRAY SET ARRAY(FILENUM,RPTR)=""
        . IF $$SHOWNEED^TMGSIPH5(JNUM,.ARRAY)=-1 SET TMGABORT=1 QUIT
        IF USRSLCT="RedirToLocal" DO  GOTO LC3
        . NEW DIC,X,Y
        . SET DIC=FILENUM,DIC(0)="MAEQ"
        . DO ^DIC WRITE !
        . IF +Y'>0 QUIT
        . SET ^TMG("TMGSIPH","PT XLAT",FILENUM,RPTR)=+Y
        IF USRSLCT="MakeNull" DO  GOTO LC3
        . SET ^TMG("TMGSIPH","PT XLAT",FILENUM,RPTR)=0
        IF USRSLCT="Quit" GOTO LC3
        IF USRSLCT="Abort" SET TMGABORT=1 GOTO LC3
        GOTO LC2
LC3     QUIT
 ;
 ;
MAP2LOCAL(JNUM,INOUT) ;
        ;"Purpose: Ask user which records to map to local records
        ;"Input: JNUM -- The job number of the background client process
        ;"       INOUT -- OPTIONAL -- Default is "PTOUT".  Should be "PTIN" or "PTOUT"
        ;"Results: None
        ;
        NEW GETARRAY,FILENUM,IEN,STIME,TMGCT,SHOWPROG,TALLY,QUERY,ERROR,REPLY
        SET INOUT=$GET(INOUT) IF INOUT'="PTIN" SET INOUT="PTOUT"
        NEW AUTOMODE SET AUTOMODE=0
        SET AUTOMODE("MAP MODE")=1
        DO ASKNEEDED(JNUM,.GETARRAY,INOUT,.AUTOMODE)
        SET FILENUM=0
        SET STIME=$H
        SET TMGCT=1,SHOWPROG=0
        NEW TMGABORT SET TMGABORT=0
        FOR  SET FILENUM=$ORDER(GETARRAY(FILENUM)) QUIT:(FILENUM="")!(TMGABORT=1)  DO
        . QUIT:($$DDOK^TMGSIPH1(JNUM,FILENUM)'=1)
        . QUIT:($$PREPXREF^TMGSIPH1(JNUM,FILENUM)'=1)
        . NEW TMGMAX SET TMGMAX=-1,TMGCT=1,STIME=$H
        . NEW IEN SET IEN=""
        . FOR  SET IEN=$ORDER(GETARRAY(FILENUM,IEN),-1) QUIT:(IEN="")!(TMGABORT=1)  DO
        . . IF TMGMAX=-1 SET TMGMAX=IEN
        . . SET TMGABORT=$$USRABORT^TMGUSRI2() QUIT:(TMGABORT=1)
        . . SET TMGCT=TMGCT+1
        . . IF (SHOWPROG=0),(($PIECE($H,",",2)-$PIECE(STIME,",",2))>15) SET SHOWPROG=1
        . . IF SHOWPROG,(TMGCT#2=0) DO
        . . . WRITE #
        . . . DO PROGBAR^TMGUSRI2(TMGCT,"Progress in "_FILENUM_": "_TMGCT,0,TMGMAX,70,STIME)
        . . . IF $DATA(TALLY) WRITE ! DO ZWRITE^TMGZWR("TALLY")
        . . NEW NEWIEN SET NEWIEN=0
        . . IF $$CHCK4SIM(FILENUM,,.NEWIEN,$$GET01FLD(JNUM,FILENUM,IEN))=0 QUIT  ;"Is a prior similar record already is on client?
        . . SET ^TMG("TMGSIPH","PT XLAT",FILENUM,IEN)=NEWIEN  ;"Add entry to Pointer translation table.
        . . DO UNNEEDPTR^TMGSIPHU(FILENUM,IEN,NEWIEN,INOUT,.TALLY)
        . . IF INOUT="PTIN" KILL ^TMG("TMGSIPH","NEEDED RECORDS","PTIN",FILENUM,IEN)
        . . KILL GETARRAY(FILENUM,IEN)
        SET RESULT=1
        IF $DATA(GETARRAY) DO
        . NEW TMGARRAY,TMGSEL,IEN
        . WRITE #
        . WRITE "One or more records could not be automatically matched to a local record.",!
        . WRITE "Select records to manually looked up.",!
        . DO PRESS2GO^TMGUSRI2 QUIT:$GET(TMGPTCABORT)=1
        . FOR  SET FILENUM=$ORDER(GETARRAY(FILENUM)) QUIT:(FILENUM="")  DO
        . . NEW FNAME SET FNAME=$PIECE($GET(^DIC(FILENUM,0)),"^",1)
        . . SET IEN=""
        . . FOR  SET IEN=$ORDER(GETARRAY(FILENUM,IEN),-1) QUIT:(IEN="")  DO
        . . . NEW DISPSTR SET DISPSTR="Get records from REMOTE file #"_FILENUM_" ("
        . . . SET DISPSTR="File: "_FNAME_"; Record: "_$$GET01FLD(JNUM,FILENUM,IEN)
        . . . SET TMGARRAY(DISPSTR)=FILENUM_"^"_IEN
        . NEW HEADER
        . SET HEADER="Select Record(s) in file "_FILENUM_" to MAP to local records. Press <ESC><ESC> when Done."
        . DO SELECTR2^TMGUSRI3("TMGARRAY","TMGSEL",HEADER)
        . IF $DATA(TMGSEL)=0 QUIT
        . NEW TMGI SET TMGI=""
        . FOR  SET TMGI=$ORDER(TMGSEL(TMGI)) QUIT:(TMGI="")!TMGABORT  DO
        . . NEW ENTRY SET ENTRY=$GET(TMGSEL(TMGI))
        . . SET FILENUM=+ENTRY QUIT:FILENUM'>0
        . . SET IEN=$PIECE(ENTRY,"^",2)
        . . NEW X,Y,DIC
        . . SET DIC=FILENUM,DIC(0)="MAEQ"
        . . SET DIC("A")="Lookup a match for ["_$$GET01FLD(JNUM,FILENUM,IEN)_"]: "
        . . NEW DONE SET DONE=0
        . . FOR  DO  QUIT:(+Y>0)!(DONE)!TMGABORT
        . . . NEW %
        . . . DO ^DIC WRITE !
        . . . IF +Y>0 DO  QUIT:TMGABORT
        . . . . SET %=1
        . . . . WRITE "Use [",$PIECE(Y,"^",2),"]" DO YN^DICN WRITE !
        . . . . IF %=-1 SET TMGABORT=1 QUIT
        . . . . IF %=2 SET Y=0 QUIT
        . . . IF +Y>0 QUIT
        . . . SET %=1
        . . . WRITE "Try another lookup" DO YN^DICN WRITE !
        . . . IF %=-1 SET TMGABORT=1 QUIT
        . . . IF %=2 SET DONE=1 QUIT
        . . IF +Y>0 DO
        . . . SET ^TMG("TMGSIPH","PT XLAT",FILENUM,IEN)=+Y  ;"Add entry to Pointer translation table.
        . . . DO UNNEEDPTR^TMGSIPHU(FILENUM,IEN,+Y,INOUT,.TALLY)
        . . . IF INOUT="PTIN" KILL ^TMG("TMGSIPH","NEEDED RECORDS","PTIN",FILENUM,IEN)
        . . . KILL GETARRAY(FILENUM,IEN)
        . . . SET TALLY("MANUALLY MATCHED TO LOCAL")=+$GET(TALLY("MANUALLY MATCHED TO LOCAL"))+1
        IF $DATA(TALLY) WRITE ! DO ZWRITE^TMGZWR("TALLY")
        DO PRESS2GO^TMGUSRI2
        QUIT
 ;
 ;
GETFILE