TMGSIPH4 ;TMG/kst/SIPHON PROGRAM, FOR TRANSFERRING VISTA INSTANCES ;11/27/09, 2/2/14
         ;;1.0;TMG-LIB;**1**;11/27/09
 ;
 ;"TMG SIPHON PROGRAM, FOR TRANSFERRING VISTA INSTANCE
 ;"Especially functions for pulling 1 record, and all records pointing to it, from server
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
 ;"SRVRDIC(JNUM,REPLY) --get a file and value to lookup on server
 ;"SRVFDIC(JNUM,FILENUM,REPLY) -- get value to lookup on server, in specified file.
 ;"GETNEWFL(JNUM) --  get a novel file DD from the server (one not already present on client)
 ;"GETPTIN(JNUM,FILENUM,IEN) -- as server for all pointers IN to a given record.
 ;"ASKREC(JNUM,FILENUM,INOUT) --Query user for patient name, and add to ToDo list
 ;"TRANSPT(JNUM) -- allow user to completely transfer 1 patient
 ;"TRANSREC(JNUM) -- allow user to completely transfer 1 RECORD
 ;"GETMSSNG(JNUM,FILENUM,OUTARRAY) ;Return a list of records on server, for given file, that have not been downloaded to client
 ;"CHKSPUPD(JNUM) --check a pre-determined SET of files for records on server that are not on client
 ;"CHKUPDTE(JNUM) -- check files for records on server that are not on client.
 ;"CHK1FUPD(JNUM,FILENUM,ALLRECS,TALLY) -- check 1 file for records on server that are not on client.
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"TMGKERN2
 ;"=======================================================================
 ;
SRVRDIC(JNUM,REPLY)
        ;"Purpose: to get a file and value to lookup on server
        ;"Input: JNUM -- The job number of the background client process
        ;"        REPLY -- PASS BY REFERANCE.  An OUT PARAMETER.
        ;"Output: REPLY is filled with reply from server (if any).  Format:
        ;"           REPLY("FILE")=FileNumber that search was from.
        ;"           REPLY(1)= <first line of server reply>   <-- could be 'Thinking' type messages...
        ;"           ...
        ;"           REPLY(n)= <Last line of server reply> <-- probably the line to look at IF only 1 expected
        ;"Result: none
        NEW FILE,DIC,X,Y,VALUE
        SET DIC=1,DIC(0)="MAEQ"
        SET DIC("A")="Enter FILE on server to search in: "
        DO ^DIC WRITE !
        IF +Y'>0 SET Y=$$GETNEWFL(JNUM)
        IF +Y'>0 QUIT
        DO SRVFDIC(JNUM,+Y,.REPLY)
        QUIT
 ;
 ;
SRVFDIC(JNUM,FILENUM,REPLY)
        ;"Purpose: to get value to lookup on server, in specified file.
        ;"Input:  JNUM -- The job number of the background client process
        ;"        FILENUM -- The fileman file to search in.
        ;"        REPLY -- PASS BY REFERANCE.  An OUT PARAMETER.
        ;"Output: REPLY is filled with reply from server (if any).  Format:
        ;"           REPLY("FILE")=FileNumber that search was from.
        ;"           REPLY(1)= <first line of server reply>   <-- could be 'Thinking' type messages...
        ;"           ...
        ;"           REPLY(n)= <Last line of server reply> <-- probably the line to look at IF only 1 expected
        ;"Result: none
        NEW FILE,DIC,X,Y,VALUE
        NEW FILENAME SET FILENAME=$$FILENAME^TMGFMUT2(FILENUM)
        ;"SET FILENAME=$PIECE($GET(^DIC(FILENUM,0)),"^",1)
        WRITE "Enter value in ",FILENAME," to search on server for: "
        READ VALUE:$GET(DTIME,3600) WRITE !
        IF VALUE["^" QUIT
        NEW QUERY,ERROR
        KILL REPLY
        SET QUERY="DO DIC|"_FILENUM_"^"_VALUE
        DO MSGCLIENT^TMGKERN2(JNUM,QUERY,.REPLY,.ERROR,30)
        IF $DATA(ERROR) DO  QUIT
        . WRITE ERROR,!
        SET REPLY("FILE")=FILENUM
        QUIT
 ;
 ;
GETNEWFL(JNUM) ;
        ;"Purpose: To get a novel file DD from the server (one not already present on client)
        ;"Input: JNUM -- The job number of the background client process
        ;"Output: Data dictionary for novel file my be downloaded and put into local database.
        ;"Result: Returns file number, or -1 IF error or abort.
        NEW FILENAME,FILENUM,RESULT,I
        SET RESULT=-1 ;"Default to failure
        WRITE "Enter name of file to search on server for: "
        READ FILENAME:$GET(DTIME,3600) WRITE !
        IF (FILENAME["^")!(FILENAME="") GOTO GNFLDN
        NEW QUERY,ERROR,REPLY
        SET QUERY="DO DIC|1^"_FILENAME
        DO MSGCLIENT^TMGKERN2(JNUM,QUERY,.REPLY,.ERROR,30)
        IF $DATA(ERROR) DO  GOTO GNFLDN
        . WRITE ERROR,!
        . DO PRESS2GO^TMGUSRI2
        IF $DATA(REPLY)=0 GOTO GNFLDN
        SET REPLY("FILE")=1
        SET I="" FOR  SET I=$ORDER(REPLY(I),-1) QUIT:(I="")!(+I=I)
        SET FILENUM=$GET(REPLY(I))
        IF +FILENUM'>0 GOTO GNFLDN
        SET QUERY="GET|^DIC("_+FILENUM_")"
        DO MSGCLIENT^TMGKERN2(JNUM,QUERY,.REPLY,.ERROR,30)
        IF $DATA(ERROR) DO  GOTO GNFLDN
        . WRITE ERROR,!
        . DO PRESS2GO^TMGUSRI2
        DO STOREDATA^TMGSIPHU(.REPLY)
        ;"---- Get and fix file header ----
        SET REF=$GET(^DIC(+FILENUM,0,"GL"))
        IF REF="" DO  GOTO GNFLDN
        . WRITE "UNABLE TO GET GLOBAL REFERENCE IN ^DIC(",FILENUM,",0,""GL"")",!
        . DO PRESS2GO^TMGUSRI2
        SET REF=REF
        SET QUERY="GET|"_REF_"0)"
        DO MSGCLIENT^TMGKERN2(JNUM,QUERY,.REPLY,.ERROR,30)
        IF $DATA(ERROR) DO  GOTO GNFLDN
        . WRITE ERROR,!
        . DO PRESS2GO^TMGUSRI2
        DO STOREDATA^TMGSIPHU(.REPLY)
        SET $PIECE(@(REF_"0)"),"^",3)=$ORDER(@(REF_"""@"")"),-1) ;"most recently added rec #
        SET $PIECE(@(REF_"0)"),"^",4)=$ORDER(@(REF_"""@"")"),-1) ;"supposed to be total num of recs
        SET RESULT=$$DDOK^TMGSIPH1(JNUM,FILENUM) ;
GNFLDN  QUIT RESULT
 ;
 ;
GETPTIN(JNUM,FILENUM,IEN)
        ;"Purpose: as server for all pointers IN to a given record.
        ;"Input:  JNUM -- The job number of the background client process
        ;"        FILENUM -- The fileman file to consider
        ;"        IEN -- The record number in file.  Server-side IEN
        ;"Output:  Data us stored in:  SET ^TMG("TMGSIPH","NEEDED RECORDS","PTIN",OFILE,NEWIEN)=""
        ;"Results: none.
        NEW QUERY,ERROR,REPLY
        SET QUERY="GET PTRS IN|"_FILENUM_"^"_IEN
        DO MSGCLIENT^TMGKERN2(JNUM,QUERY,.REPLY,.ERROR,30)
        ;"REPLY -- PASS BY REFERENCE, an OUT PARAMETER.  Format:
        ;"         REPLY(1)=FROMFILE^FROMIENS^FROMFLD
        ;"         REPLY(2)=FROMFILE^FROMIENS^FROMFLD  etc.
        IF $DATA(ERROR) DO  QUIT
        . WRITE ERROR,!
        NEW LINE,NEWIEN
        FOR LINE=1:1 QUIT:($DATA(REPLY(LINE))=0)  DO
        . SET NEWIEN=$PIECE(REPLY(LINE),"^",2)
        . NEW OFILE SET OFILE=+REPLY(LINE)
        . ;"IF NEWIEN["," QUIT  ;"pointers IN from subfiles will be gotten with parent records
        . IF NEWIEN["," DO
        . . NEW PFILE SET PFILE=OFILE
        . . FOR  SET PFILE=+$GET(^DD(PFILE,0,"UP")) QUIT:PFILE=0  DO
        . . . SET OFILE=OFILE_"{"_PFILE
        . SET ^TMG("TMGSIPH","NEEDED RECORDS","PTIN",OFILE,NEWIEN)=""
        QUIT
 ;
 ;
ASKREC(JNUM,FILENUM,INOUT) ;
        ;"Purpose: Query user for patient name, and add to ToDo list
        ;"Input: JNUM -- The job number of the background client process
        ;"       FILENUM -- OPTIONAL.  The fileman file.  If not provided, user will be asked for it.
        ;"       INOUT -- OPTIONAL -- Default is "PTOUT".  Should be "PTIN" or "PTOUT"
        ;"               ... NOTE: don't use 'PTOUT' ... causes problem because of difference in node numbers...
        ;"Result: none
        ;"Records that are needed are stored in ^TMG("TMGSIPH","NEEDED RECORDS","PTIN",FILENUM,IEN)=""
        SET INOUT=$GET(INOUT) IF INOUT'="PTIN" SET INOUT="PTOUT"
        NEW ARRAY,IEN,VALUE,I,REPLY
        SET FILENUM=+$GET(FILENUM)
        IF FILENUM>0 DO
        . DO SRVFDIC(JNUM,FILENUM,.ARRAY)
        ELSE  DO
        . DO SRVRDIC(JNUM,.ARRAY)
        . SET FILENUM=+$GET(ARRAY("FILE"))
        IF $DATA(ARRAY)=0 GOTO PRDN
        SET I="" FOR  SET I=$ORDER(ARRAY(I),-1) QUIT:(I="")!(+I=I)
        SET VALUE=$GET(ARRAY(I))
        IF +VALUE'>0 GOTO PRDN
        IF INOUT="PTIN" DO
        . SET ^TMG("TMGSIPH","NEEDED RECORDS","PTIN",FILENUM,+VALUE)=""
        ELSE  DO  ;"....  don't use
        . ;"^TMG("TMGSIPH","NEEDED RECORDS","PTOUT",FILENUM,RemotePointer,ReferToNodeToBeCorrected,Piece#OfNode)=""
        . ;"SET ^TMG("TMGSIPH","NEEDED RECORDS","PTOUT",FILENUM,+VALUE)=""
        WRITE $PIECE(VALUE,"^",2),!
PRDN    QUIT
 ;
 ;
TRANSPT(JNUM)
        ;"Purpose: to allow user to completely transfer 1 patient
        ;"Input: JNUM -- The job number of the background client process
        ;"Output: Records are downloaded and put into local database.
        ;"Result: none
        DO ASKREC(JNUM,2)  ;"2 = PATIENT file.
        NEW TMGABORT SET TMGABORT=0
        NEW HASTASKS SET HASTASKS=1
        FOR  QUIT:(HASTASKS=0)!(TMGABORT)  DO
        . IF $$HANDLNEEDED^TMGSIPH3(JNUM,"PTIN",1)=-1 SET TMGABORT=1 QUIT
        . IF $$HANDLNEEDED^TMGSIPH3(JNUM,"PTOUT",1)=-1 SET TMGABORT=1 QUIT
        . IF $DATA(^TMG("TMGSIPH","NEEDED RECORDS","PTIN"))>0 QUIT
        . IF $DATA(^TMG("TMGSIPH","NEEDED RECORDS","PTOUT"))>0 QUIT
        . SET HASTASKS=0 QUIT
        QUIT
 ;
 ;
TRANSREC(JNUM) ;
        ;"Purpose: to allow user to completely transfer 1 RECORD
        ;"Input: JNUM -- The job number of the background client process
        ;"Output: Records are downloaded and put into local database.
        ;"Result: none
        NEW DIC,X,Y
        NEW ARRAY,IEN,VALUE,I,REPLY,TALLY
        SET DIC=1,DIC(0)="MAEQN"
        DO ^DIC WRITE !
        IF +Y'>0 SET Y=$$GETNEWFL(JNUM)
        IF +Y'>0 GOTO TRDN
        SET FILENUM=+Y
        DO SRVFDIC(JNUM,FILENUM,.ARRAY)
        IF $DATA(ARRAY)=0 GOTO TRDN
        SET I="" FOR  SET I=$ORDER(ARRAY(I),-1) QUIT:(I="")!(+I=I)
        SET VALUE=$GET(ARRAY(I))
        NEW IEN SET IEN=+VALUE
        IF IEN'>0 GOTO TRDN
        WRITE $PIECE(VALUE,"^",2),!
        IF $$GETANDFIXREC^TMGSIPH3(JNUM,FILENUM,IEN,"?",.TALLY,"PTOUT")
        IF $DATA(TALLY) DO ZWRITE^TMGZWR("TALLY")
        DO PRESS2GO^TMGUSRI2
        ;
TRDN    QUIT
 ;
 ;
GETMSSNG(JNUM,FILENUM,OUTARRAY) ; GetMissingRecordIENs
        ;"Purpose: Return a list of records on server, for given file, that have not been downloaded to client
        ;"Input: JNUM -- The job number of the background client process
        ;"       FILENUM -- The Fileman file number.
        ;"       OUTARRAY -- PASS BY REFERENCE.  Prior contents erased.  Format:
        ;"          OUTARRAY(FILENUM,RPTR)=""
        ;"          OUTARRAY(FILENUM,RPTR)=""
        ;"Results: none
        KILL OUTARRAY
        NEW CT SET CT=0
        NEW QUERY,ERROR,REPLY,SVRHEADER
        SET QUERY="GET IEN HDR|"_FILENUM
        DO MSGCLIENT^TMGKERN2(JNUM,QUERY,.REPLY,.ERROR,30) ;"Should get LastIEN^TotalNumIENS
        IF $DATA(ERROR) WRITE ERROR,! GOTO GMDN
        SET SVRHEADER=$GET(REPLY(1)) IF SVRHEADER="" DO  GOTO GMDN
        . WRITE "Error getting File headers from server.",!
        NEW DONE SET DONE=0
        IF $GET(^TMG("TMGSIPH","RECORDS SYNC",FILENUM))=SVRHEADER DO  GOTO:DONE GMDN2
        . WRITE "According to Fileman headers, there are no NEW records added to file "_FILENUM,!
        . WRITE "since last check.",!
        . NEW % SET %=2
        . WRITE "Do complete and thorough check again anyway" DO YN^DICN WRITE !
        . SET DONE=(%'=1)
        NEW FILENAME SET FILENAME=$$FILENAME^TMGFMUT2(FILENUM)
        WRITE !,"Getting a list of all records on server for file ",FILENAME," (#",FILENUM,")",!
        SET QUERY="GET IEN LIST|"_FILENUM
        DO MSGCLIENT^TMGKERN2(JNUM,QUERY,.REPLY,.ERROR,30) ;"Should get list of all IEN's in record on server.
        IF $DATA(ERROR) WRITE ERROR,! GOTO GMDN
        SET ^TMG("TMGSIPH","RECORDS SYNC",FILENUM)=SVRHEADER
        NEW STIME SET STIME=$H
        NEW TMGCT SET TMGCT=0
        NEW SHOWPROG SET SHOWPROG=0
        NEW TMGMIN,TMGMAX
        NEW TMGABORT SET TMGABORT=0
        NEW TMGI SET TMGI=0
        FOR  SET TMGI=$ORDER(REPLY(TMGI)) QUIT:(+TMGI'>0)!TMGABORT  DO
        . NEW VALUE SET VALUE=$GET(REPLY(TMGI))  ;"Should be IEN^.01 Value (internal format)
        . NEW RPTR SET RPTR=+VALUE
        . IF +$GET(^TMG("TMGSIPH","PT XLAT",FILENUM,RPTR))'>0 DO
        . . IF $DATA(^TMG("TMGSIPH",".01 VALUE",FILENUM,RPTR))=0 DO
        . . . SET ^TMG("TMGSIPH",".01 VALUE",FILENUM,RPTR)=$PIECE(VALUE,"^",2)
        . . SET OUTARRAY(FILENUM,RPTR)=""
        . . SET CT=CT+1
        . . KILL REPLY(TMGI)
        . SET TMGABORT=$$USRABORT^TMGUSRI2() QUIT:(TMGABORT=1)
        . SET TMGCT=TMGCT+1
        . IF (SHOWPROG=0),($$HDIFF^XLFDT($H,STIME,2)>5) DO  ;"Turn on progress bar after 15 seconds.
        . . SET SHOWPROG=1
        . . SET TMGMIN=1
        . . SET TMGMAX=$ORDER(REPLY(""),-1)
        . IF (SHOWPROG=1),(TMGCT>200) DO
        . . DO PROGBAR^TMGUSRI2(TMGI,"Comparing server vs local records in File: "_FILENUM,TMGMIN,TMGMAX,70,STIME)
        . . SET TMGCT=0
GMDN    WRITE !
        WRITE CT," records found to be downloaded.",!
GMDN2   QUIT
 ;
 ;
CHKSPUPD(JNUM) ;" CHECK SPECIAL FILES FOR UPDATE
        ;"Purpose: To check a pre-determined SET of files for records on server that are not on client.
        ;"Input: JNUM -- The job number of the background client process
        ;"Output: Records my be downloaded and put into local database.
        ;"Result: none
        NEW FILENUM,TALLY,TMGABORT
        IF $DATA(^TMG("TMGSIPH","TRACKED FILES"))=0 DO
        . SET ^TMG("TMGSIPH","TRACKED FILES",8925)=1
        . SET ^TMG("TMGSIPH","TRACKED FILES",120.5)=1
        . SET ^TMG("TMGSIPH","TRACKED FILES",2005)=1
        . SET ^TMG("TMGSIPH","TRACKED FILES",22705.5)=1
        SET TMGABORT=0
        SET FILENUM=0
        FOR  SET FILENUM=$ORDER(^TMG("TMGSIPH","TRACKED FILES",FILENUM)) QUIT:(+FILENUM'>0)!TMGABORT  DO
        . IF $$CHK1FUPD(JNUM,FILENUM,1,.TALLY)=-1 SET TMGABORT=1
        DO AUTONEEDED^TMGSIPH3(JNUM)
        IF $DATA(TALLY) DO ZWRITE^TMGZWR("TALLY")
        DO PRESS2GO^TMGUSRI2
        QUIT
 ;
 ;
CHKUPDTE(JNUM,ALLRECS) ; "CHECK FOR UPDATE
        ;"Purpose: To check files for records on server that are not on client.
        ;"Input: JNUM -- The job number of the background client process
        ;"       ALLRECS -- OPTIONAL.  Default=0.  If 1, then all records are automatically selected
        ;"Output: Records my be downloaded and put into local database.
        ;"Result: none
        NEW DIC,X,Y
        NEW ARRAY,IEN,TALLY,FILENUM
        SET DIC=1,DIC(0)="MAEQN"
        WRITE "Enter FILE on server in which to search for NEW records.",!
        WRITE "(If file exists on server, but not on client, enter ^)",!
        DO ^DIC WRITE !
        IF +Y'>0 SET Y=$$GETNEWFL(JNUM)
        IF +Y'>0 GOTO CHDN
        SET FILENUM=+Y
        IF $$CHK1FUPD(JNUM,FILENUM,.ALLRECS,.TALLY) ;
        IF $DATA(TALLY) DO ZWRITE^TMGZWR("TALLY")
        DO PRESS2GO^TMGUSRI2
        ;
CHDN    QUIT
 ;
CHK1FUPD(JNUM,FILENUM,ALLRECS,TALLY) ;" CHECK 1 FILE FOR UPDATE
        ;"Purpose: To check 1 file for records on server that are not on client.
        ;"Input: JNUM -- The job number of the background client process
        ;"       FILENUM -- the file number to check.
        ;"       ALLRECS -- OPTIONAL.  Default=0.  If 1, then all records are automatically selected
        ;"       TALLY -- PASS BY REFERENCE.  An array to hold progress of downloaded files.
        ;"Output: Records my be downloaded and put into local database.
        ;"Result: 1 if OK, -1 IF abort
        NEW ARRAY,IEN
        NEW RESULT SET RESULT=1
        SET ALLRECS=+$GET(ALLRECS)
        DO GETMSSNG(JNUM,FILENUM,.ARRAY)
        IF ALLRECS'=1 DO PRESS2GO^TMGUSRI2
        IF $DATA(ARRAY)=0 GOTO CH1DN
        NEW SELARRAY,OPTIONS
        IF ALLRECS'=1 DO
        . SET OPTIONS("HEADER")="Select Server Records Missing Locally to Download <Esc><Esc> when done."
        . DO SELNEEDED^TMGSIPH3(JNUM,.SELARRAY,"ARRAY",.OPTIONS)
        ELSE  DO
        . MERGE SELARRAY=ARRAY
        NEW STIME SET STIME=$H
        NEW TMGCT SET TMGCT=0
        NEW SHOWPROG SET SHOWPROG=0
        NEW TMGMIN,TMGMAX
        NEW TMGABORT SET TMGABORT=0
        NEW RPTR SET RPTR=""
        FOR  SET RPTR=$ORDER(SELARRAY(FILENUM,RPTR)) QUIT:(+RPTR'>0)!TMGABORT  DO
        . SET TMGABORT=$$USRABORT^TMGUSRI2() QUIT:(TMGABORT=1)
        . NEW TMP SET TMP=$$GETANDFIXREC^TMGSIPH3(JNUM,FILENUM,RPTR,"?",.TALLY)
        . IF TMP=-1 DO HNDLGAFE^TMGSIPH3(FILENUM,IEN,.TMGABORT) QUIT
        . SET TMGCT=TMGCT+1
        . IF (SHOWPROG=0),($$HDIFF^XLFDT($H,STIME,2)>5) DO  ;"Turn on progress bar after 5 seconds.
        . . SET SHOWPROG=1
        . . SET TMGMIN=$ORDER(SELARRAY(FILENUM,0))
        . . SET TMGMAX=$ORDER(SELARRAY(FILENUM,""),-1)
        . IF (SHOWPROG=1),(TMGCT>50) DO
        . . DO PROGBAR^TMGUSRI2(RPTR,"Getting Records From File: "_FILENUM,TMGMIN,TMGMAX,70,STIME)
        . . SET TMGCT=0
        IF $DATA(TALLY) DO ZWRITE^TMGZWR("TALLY")
        ;
CH1DN   IF TMGABORT SET RESULT=-1
        QUIT RESULT
