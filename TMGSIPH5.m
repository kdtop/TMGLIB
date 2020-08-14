TMGSIPH5 ;TMG/kst/SIPHON PROGRAM, FOR TRANSFERRING VISTA INSTANCES ;11/27/09, 2/2/14
         ;;1.0;TMG-LIB;**1**;11/27/09
 ;
 ;"TMG SIPHON PROGRAM, FOR TRANSFERRING VISTA INSTANCE
 ;"Utility functions for working with transfers on client
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
 ;"EXAMNEED(JNUM,INOUT) -- User selects records, and then this displays who needs records.
 ;"SHOWNEED(JNUM,GETARRAY) -- show selected records
 ;"CHCK1NEED(FILENUM,RPTR,INOUT) --show who is needing one requested record
 ;"GL2FILE(CREF,FNAME)  -- Return filenumber based on global reference.
 ;"KILLNEED(JNUM,INOUT) --allow user to KILL needed needed pointers.
 ;"PREVIEW(JNUM,INOUT) --allow user view server record before downloading and installing
 ;"DELREC -- Allow user to del record and remove record that it has been previously downloaded.
 ;"DEL1REC(FILENUM,LPTR) -- allow deletion of given record and that it has been downloaded.
 ;"
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;
EXAMNEED(JNUM,INOUT) ;
        ;"Purpose:User selects records, and then this displays who needs records.
        ;"Input: JNUM -- The job number of the background client process
        ;"       INOUT -- OPTIONAL -- Default is "PTOUT".  Should be "PTIN" or "PTOUT"
        ;"Results: None
        ;
        NEW GETARRAY
        SET INOUT=$GET(INOUT) IF INOUT'="PTIN" SET INOUT="PTOUT"
        NEW OPTIONS
        SET OPTIONS("HEADER")="Select File(s) to EXAMINE. Press <ESC><ESC> when Done."
        DO ASKNEEDED^TMGSIPH3(JNUM,.GETARRAY,INOUT,.OPTIONS)
        IF $DATA(GETARRAY)=0 GOTO WNDN
        IF $$SHOWNEED(JNUM,.GETARRAY)  ;"Ignore aborts
WNDN    QUIT
 ;
 ;
SHOWNEED(JNUM,GETARRAY)
        ;"Purpose: To show selected records
        ;"Input: JNUM
        ;"       GETARRAY -- PASS BY REFERENCE.  Array as created by ASKNEEDED^TMGSIPH4
        ;"           GETARRAY(FileNum,RecordNum)=""
        ;"Results: 1 if OK, -1 IF abort
        NEW RESULT SET RESULT=1
        NEW TMGABORT SET TMGABORT=0
        NEW FILENUM SET FILENUM=0
        NEW STIME SET STIME=$H
        NEW TALLY
        NEW TMGCT SET TMGCT=1
        NEW SHOWPROG SET SHOWPROG=0
        FOR  SET FILENUM=$ORDER(GETARRAY(FILENUM)) QUIT:(FILENUM="")!(TMGABORT=1)  DO
        . QUIT:($$DDOK^TMGSIPH1(JNUM,FILENUM)'=1)
        . QUIT:($$PREPXREF^TMGSIPH1(JNUM,FILENUM)'=1)
        . NEW TMGMAX SET TMGMAX=-1
        . NEW IEN SET IEN=""
        . FOR  SET IEN=$ORDER(GETARRAY(FILENUM,IEN),-1) QUIT:(IEN="")!(TMGABORT=1)  DO
        . . IF TMGMAX=-1 SET TMGMAX=IEN
        . . SET TMGABORT=$$USRABORT^TMGUSRI2() QUIT:(TMGABORT=1)
        . . SET TMGCT=TMGCT+1
        . . IF $$CHCK1NEED(FILENUM,IEN,INOUT)=-1 DO  QUIT
        . . . NEW % SET %=1
        . . . WRITE "ABORT" DO YN^DICN WRITE !
        . . . IF %'=2 SET TMGABORT=1
        . . IF (SHOWPROG=0),(($PIECE($H,",",2)-$PIECE(STIME,",",2))>15) SET SHOWPROG=1
        . . IF SHOWPROG,(TMGCT#10=0) DO
        . . . WRITE #
        . . . DO PROGBAR^TMGUSRI2(TMGCT,"Progress: "_TMGCT,0,TMGMAX,70,STIME)
        . . . IF $DATA(TALLY) WRITE ! DO ZWRITE^TMGZWR("TALLY")
        DO PRESS2GO^TMGUSRI2
        IF ($GET(TMGPTCABORT)=1)!(TMGABORT) SET RESULT=-1
        QUIT RESULT
 ;
 ;
CHCK1NEED(FILENUM,RPTR,INOUT) ;
        ;"Purpose: To show who is needing one requested record
        ;"Input: FILENUM -- The file number to compare.
        ;"       RPTR -- The IEN of the record that was wanted from the server.
        ;"       INOUT -- OPTIONAL -- Default is "PTOUT".  Should be "PTIN" or "PTOUT"
        ;"NOTE:  Gobal ^TMG("TMGSIPH","NEEDED RECORDS","PTOUT") used, with format as below:
        ;"             ^TMG("TMGSIPH","NEEDED RECORDS","PTOUT",FILENUM,RemotePointer,RefToNodeToBeCorrected,INFO)=""
        ;"                      INFO=DataPiece^PointedToFile^PointedToReference^IENDepth^[V]
        ;"       As pointers are resolved, the entries will be KILLED from the above global
        ;"
        ;"Results: 1 for OK, -1 for abort
        ;"
        NEW RESULT SET RESULT=1
        SET FILENUM=+$GET(FILENUM) QUIT:(FILENUM'>0)
        NEW FNAME SET FNAME=$PIECE($GET(^DIC(FILENUM,0)),"^",1)
        SET RPTR=+$GET(RPTR)
        SET LPTR=+$GET(LPTR)
        NEW REF SET REF=""
        FOR  SET REF=$ORDER(^TMG("TMGSIPH","NEEDED RECORDS","PTOUT",FILENUM,RPTR,REF)) QUIT:(REF="")!(RESULT=-1)  DO
        . NEW INFO SET INFO=""
        . FOR  SET INFO=$ORDER(^TMG("TMGSIPH","NEEDED RECORDS","PTOUT",FILENUM,RPTR,REF,INFO)) QUIT:(INFO="")!(RESULT=-1)  DO
        . . NEW PCE SET PCE=+INFO
        . . NEW IENDEPTH SET IENDEPTH=$PIECE(INFO,"^",4)
        . . NEW QNUM SET QNUM=$QLENGTH(REF)-(IENDEPTH*2)   ;"e.g. ^TIU(8925,IEN,0), or e.g. ^PS(52.11,IEN,2,IEN2,0),  ^PS(52.11,IEN,2,IEN2,0,IEN3,3)
        . . NEW GL SET GL=$$QSUBS^TMGSIPHU(REF,QNUM)
        . . NEW FRFNAME SET FRFNAME="??"
        . . NEW PFROMFIL SET PFROMFIL=$$GL2FILE(GL,.FRFNAME)
        . . NEW PFROMREC SET PFROMREC=$QSUBSCRIPT(REF,QNUM+1)
        . . NEW LOC SET LOC=$QSUBSCRIPT(REF,$QLENGTH(REF))
        . . NEW FLD SET FLD=$$GETFLD^TMGSIPHU(PFROMFIL,LOC,PCE)
        . . WRITE !,"Needed Record: FILE ",FILENUM," [",FNAME,"]; #",RPTR," [",$$GET01FLD^TMGSIPH3(JNUM,FILENUM,RPTR),"] ",!
        . . WRITE "Needed by: FILE: ",PFROMFIL," [",FRFNAME,"]; #",PFROMREC,"; FLD: ",+FLD," [",$PIECE(FLD,"^",2),"]",!
        . . NEW TOSHOW,FLD SET FLD=0
        . . FOR  SET FLD=$ORDER(^DD(PFROMFIL,FLD)) QUIT:(+FLD'>0)  DO
        . . . NEW INFO SET INFO=$PIECE($GET(^DD(PFROMFIL,FLD,0)),"^",2)
        . . . QUIT:(INFO'["P")
        . . . NEW AFILE SET AFILE=+$PIECE(INFO,"P",2) QUIT:(AFILE'=2)  ;"2 = PATIENT file
        . . . SET TOSHOW(FLD)=""
        . . IF $DATA(TOSHOW) DO
        . . . WRITE "Name of patient in this record as follows:",!
        . . . DO DUMPREC^TMGDEBU3(PFROMFIL,PFROMREC,0,.TOSHOW)
        . . NEW % SET %=2
        . . WRITE "View current local record needing record" DO YN^DICN WRITE !
        . . IF %=-1 SET RESULT=-1 QUIT
        . . IF %=1 DO
        . . . DO DUMPREC^TMGDEBU3(PFROMFIL,PFROMREC)
        . . . WRITE !
        . . . DO PRESS2GO^TMGUSRI2
        . . . IF $GET(TMGPTCABORT)=1 SET RESULT=-1
        ;
        QUIT RESULT
 ;
 ;
GL2FILE(CREF,FNAME) ;
        ;"Purpose: Return filenumber based on global reference.
        ;"Input: CREF -- closed reference of root of file.
        ;"       FNAME -- OPTIONAL.  PASS BY REFERENCE.  Filled with filename, IF found.
        ;"Results: Filenumber, or 0 IF problem
        NEW RESULT SET RESULT=0
        NEW NODE0 SET NODE0=$GET(@CREF@(0)) GOTO:(NODE0="") G2FDN
        SET FNAME=$PIECE(NODE0,"^",1)
        SET RESULT=+$PIECE(NODE0,"^",2)
G2FDN   QUIT RESULT
 ;
 ;
KILLNEED(JNUM,INOUT) ;
        ;"Purpose: To allow user to KILL needed needed pointers.
        ;"Input: JNUM -- The job number of the background client process
        ;"       INOUT -- OPTIONAL -- Default is "PTOUT".  Should be "PTIN" or "PTOUT"
        ;"Results: None
        ;
        NEW GETARRAY,FILENUM,IEN,STIME,TMGCT,SHOWPROG,TALLY,QUERY,ERROR
        SET INOUT=$GET(INOUT) IF INOUT'="PTIN" SET INOUT="PTOUT"
        NEW TMGABORT SET TMGABORT=0
        NEW OPTIONS
        SET OPTIONS("HEADER")="Select File(s) to REMOVE NEEDED FROM. Press <ESC><ESC> when Done."
        DO ASKNEEDED^TMGSIPH3(JNUM,.GETARRAY,INOUT,.OPTIONS)
        IF $DATA(GETARRAY)=0 GOTO WNDN
        WRITE !,"NOTE: If the selected records are removed from the needed list,",!
        WRITE "then all the records pointing to this needed record will be left",!
        WRITE "with NULL pointers.  THIS CAN NOT BE UNDONE.",!
        WRITE "It is recommended that the individual records be EXAMINED",!
        WRITE "to better understand the linkages before deletion.",!
        WRITE "If you don't know what you are doing,then don't proceed.",!,!
        NEW % SET %=1
        WRITE "EXAMINE records first" DO YN^DICN WRITE !
        IF %=-1 GOTO KNDN
        IF %=1 IF $$SHOWNEED(JNUM,.GETARRAY)=-1 GOTO KNDN
        SET %=2
        WRITE "PROCEED WITH DELETION FROM NEEDED LIST" DO YN^DICN WRITE !
        IF %'=1 GOTO KNDN
        SET FILENUM=0
        SET STIME=$H
        SET TMGCT=1,SHOWPROG=0
        FOR  SET FILENUM=$ORDER(GETARRAY(FILENUM)) QUIT:(FILENUM="")!(TMGABORT=1)  DO
        . NEW TMGMAX SET TMGMAX=-1
        . NEW IEN SET IEN=""
        . FOR  SET IEN=$ORDER(GETARRAY(FILENUM,IEN),-1) QUIT:(IEN="")!(TMGABORT=1)  DO
        . . IF TMGMAX=-1 SET TMGMAX=IEN
        . . SET TMGABORT=$$USRABORT^TMGUSRI2() QUIT:(TMGABORT=1)
        . . SET TMGCT=TMGCT+1
        . . DO UNNEEDPTR^TMGSIPHU(FILENUM,IEN,0,.TALLY)
        . . IF (SHOWPROG=0),(($PIECE($H,",",2)-$PIECE(STIME,",",2))>15) SET SHOWPROG=1
        . . IF SHOWPROG,(TMGCT#10=0) DO
        . . . WRITE #
        . . . DO PROGBAR^TMGUSRI2(TMGCT,"Progress: "_TMGCT,0,TMGMAX,70,STIME)
        . . . IF $DATA(TALLY) WRITE ! DO ZWRITE^TMGZWR("TALLY")
        DO PRESS2GO^TMGUSRI2
KNDN    QUIT
 ;
 ;
PREVIEW(JNUM,INOUT) ;
        ;"Purpose: To allow user view server record before downloading and installing
        ;"Input: JNUM -- The job number of the background client process
        ;"       INOUT -- OPTIONAL -- Default is "PTOUT".  Should be "PTIN" or "PTOUT"
        ;"Results: None
        ;
        NEW GETARRAY,FILENUM,IEN,STIME,TMGCT,SHOWPROG,TALLY,QUERY,ERROR
        SET INOUT=$GET(INOUT) IF INOUT'="PTIN" SET INOUT="PTOUT"
        NEW TMGABORT SET TMGABORT=0
        NEW OPTIONS
        SET OPTIONS("HEADER")="Select File(s) to PREVIEW. Press <ESC><ESC> when Done."
        DO ASKNEEDED^TMGSIPH3(JNUM,.GETARRAY,INOUT,.OPTIONS)
        IF $DATA(GETARRAY)=0 GOTO PVDN
        NEW SHOWEMPTY
        NEW % SET %=2
        WRITE "Display Empty Fields" DO YN^DICN WRITE !
        IF %=-1 GOTO PVDN
        SET SHOWEMPTY=(%=1)
        SET FILENUM=0
        SET STIME=$H
        SET TMGCT=1,SHOWPROG=0
        FOR  SET FILENUM=$ORDER(GETARRAY(FILENUM)) QUIT:(FILENUM="")!(TMGABORT=1)  DO
        . NEW TMGMAX SET TMGMAX=-1
        . NEW IEN SET IEN=""
        . FOR  SET IEN=$ORDER(GETARRAY(FILENUM,IEN),-1) QUIT:(IEN="")!(TMGABORT=1)  DO
        . . IF TMGMAX=-1 SET TMGMAX=IEN
        . . SET TMGABORT=$$USRABORT^TMGUSRI2() QUIT:(TMGABORT=1)
        . . SET TMGCT=TMGCT+1
        . . SET QUERY="DUMP REC|"_FILENUM_"^"_IEN_"^"_SHOWEMPTY
        . . DO MSGCLIENT^TMGKERN2(JNUM,QUERY,.REPLY,.ERROR,30)
        . . IF $DATA(ERROR) DO  QUIT
        . . . WRITE ERROR,!
        . . . SET TMGABORT=1
        . . NEW TMGI SET TMGI=""
        . . FOR  SET TMGI=$ORDER(REPLY(TMGI)) QUIT:(TMGI="")  DO
        . . . WRITE REPLY(TMGI),!
        . . IF (SHOWPROG=0),(($PIECE($H,",",2)-$PIECE(STIME,",",2))>15) SET SHOWPROG=1
        . . IF SHOWPROG,(TMGCT#10=0) DO
        . . . WRITE #
        . . . DO PROGBAR^TMGUSRI2(TMGCT,"Progress: "_TMGCT,0,TMGMAX,70,STIME)
        . . . IF $DATA(TALLY) WRITE ! DO ZWRITE^TMGZWR("TALLY")
        DO PRESS2GO^TMGUSRI2
PVDN    QUIT
 ;
 ;
CHKPTIN(JNUM) ;
        ;"Purpose: to check for pointers in to files/records already downloaded.
        ;"Input: JNUM -- The job number of the background client process
        ;"Results: None
        NEW TMGARRAY,TMGSEL
        NEW FILENUM SET FILENUM=0
        FOR  SET FILENUM=$ORDER(^TMG("TMGSIPH","DOWNLOADED",FILENUM)) QUIT:(FILENUM'>0)  DO
        . NEW DISPSTR SET DISPSTR="Check for pointers IN to file #"_FILENUM_" ("
        . SET DISPSTR=DISPSTR_$PIECE($GET(^DIC(FILENUM,0)),"^",1)_")"
        . SET TMGARRAY(DISPSTR)=FILENUM
        NEW HEADER SET HEADER="Select File(s) to Check for POINTERS IN. Press <ESC><ESC> when Done."
        DO SELECTR2^TMGUSRI3("TMGARRAY","TMGSEL",HEADER)
        ;
        NEW TMGABORT SET TMGABORT=0
        NEW IDX SET IDX=""
        FOR  SET IDX=$ORDER(TMGSEL(IDX)) QUIT:(IDX="")!TMGABORT  DO
        . SET FILENUM=$GET(TMGSEL(IDX)) QUIT:FILENUM=""
        . SET TMGABORT=($$CHK1PTIN(JNUM,FILENUM)'=1)
        ;
        WRITE !
        DO PRESS2GO^TMGUSRI2
        QUIT
 ;
 ;
CHK1PTIN(JNUM,FILENUM) ;
        ;"Purpose: To cycle through all local records that have been downloaded and manuall
        ;"         check on server for pointers in, and que checks IF needed.
        ;"Input: JNUM -- The job number of the background client process
        ;"       FILENUM -- The file to process.
        ;"Results: 1 if OK -1 IF error/abort
        NEW RESULT SET RESULT=1
        NEW TMGABORT SET TMGABORT=0
        NEW TMGCT SET TMGCT=999
        NEW STIME SET STIME=$H
        NEW TMGMIN SET TMGMIN=$ORDER(^TMG("TMGSIPH","DOWNLOADED",FILENUM,0))
        NEW TMGMAX SET TMGMAX=$ORDER(^TMG("TMGSIPH","DOWNLOADED",FILENUM,""),-1)
        NEW LPTR SET LPTR=0
        FOR  SET LPTR=$ORDER(^TMG("TMGSIPH","DOWNLOADED",FILENUM,LPTR)) QUIT:(+LPTR'>0)!TMGABORT  DO
        . SET TMGABORT=$$USRABORT^TMGUSRI2() QUIT:(TMGABORT=1)
        . NEW RPTR SET RPTR=+$GET(^TMG("TMGSIPH","DOWNLOADED",FILENUM,LPTR))
        . QUIT:(RPTR'>0)
        . DO GETPTIN^TMGSIPH4(JNUM,FILENUM,RPTR)
        . SET TMGCT=TMGCT+1
        . IF TMGCT>25 DO
        . . DO PROGBAR^TMGUSRI2(LPTR,"Checking pointers IN to file #"_FILENUM,TMGMIN,TMGMAX,70,STIME)
        . . SET TMGCT=0
        IF TMGABORT SET RESULT=-1
        QUIT RESULT
 ;
 ;
DELREC ;
        ;"Purpose: To allow a user to delete a record on the client, and remove record that it has
        ;"         been previously downloaded. This will allow it to be downloaded again.
        WRITE !,"Select a downloaded record to delete from this client.",!
        WRITE "NOTE: All pointer to this record will be deleted.",!
        NEW X,Y,DIC,FILENUM,RESULT
        SET DIC=1,DIC(0)="MAEQ"
        DO ^DIC WRITE !
        IF +Y>0 DO
        . SET FILENUM=+Y
        . NEW % SET %=2
        . WRITE "DELETE *ALL* RECORDS IN FILE"
        . DO YN^DICN WRITE !
        . IF %=1 IF $$DELALL(FILENUM) QUIT
        . IF %=-1 QUIT
        . SET DIC=FILENUM
        . DO ^DIC WRITE !
        . IF +Y'>0 QUIT
        . SET RESULT=$$DEL1REC(FILENUM,+Y)
        . IF +RESULT=-1 DO
        . . WRITE $PIECE(RESULT,"^",2),!
        . ELSE  DO
        . . WRITE "Record deleted, and all pointers to record have been removed.",!
        . DO PRESS2GO^TMGUSRI2
        QUIT
 ;
 ;
DELALL(FILENUM)
        ;"Purpose: To allow deletion of all records in file on the client, and remove the
        ;"         notation that it has been downloaded.
        ;"Input: FILENUM -- Filenumber to delete
        ;"Result: 1 = OK,   -1^Message IF error
        SET FILENUM=$GET(FILENUM)
        NEW RESULT SET RESULT=1
        NEW % SET %=2
        WRITE "Are you CERTAIN you want to delete ALL records in file ",FILENUM
        DO YN^DICN WRITE !
        IF %'=1 SET RESULT="-1^USER ABORTED" GOTO DADN
        NEW TMGCT SET TMGCT=0
        NEW TMGABORT SET TMGABORT=0
        NEW STIME SET STIME=$H
        NEW REF SET REF=$GET(^DIC(FILENUM,0,"GL"))
        IF REF="" DO  GOTO DADN
        . SET RESULT="-1^INVALID FILENUM: "_FILENUM
        SET REF=$$CREF^DILF(REF)
        SET TMGMIN=$ORDER(@REF@(0))
        SET TMGMAX=$ORDER(@REF@("@"),-1)
        NEW TMGIEN SET TMGIEN=0
        FOR  SET TMGIEN=$ORDER(@REF@(TMGIEN)) QUIT:(+TMGIEN'>0)!TMGABORT  DO
        . SET TMGABORT=$$USRABORT^TMGUSRI2() QUIT:(TMGABORT=1)
        . IF TMGCT>100 DO
        . . DO PROGBAR^TMGUSRI2(TMGIEN,"Deleting local records in file "_FILENUM,TMGMIN,TMGMAX,70,STIME)
        . . SET TMGCT=0
        . SET TMGCT=TMGCT+1
        . SET RESULT=$$DEL1REC(FILENUM,TMGIEN,1)
        . IF +RESULT=-1 SET TMGABORT=1
        IF 'TMGABORT DO
        . KILL ^TMG("TMGSIPH","PT XLAT",FILENUM)
        . KILL ^TMG("TMGSIPH","RECORDS SYNC",FILENUM)
DADN    QUIT RESULT
 ;
 ;
DEL1REC(FILENUM,LPTR,FORCE) ;
        ;"Purpose: To allow deletion of a record on the client, and record that it has been downloaded.
        ;"Input: FILENUM -- Filenumber to delete
        ;"       LPTR -- Record number (IEN) on client to delete
        ;"       FORCE -- OPTIONAL.  If 1, then will delete even IF not prev downloaded
        ;"Result: 1 = OK,   -1^Message IF error
        NEW RESULT SET RESULT=1
        IF $GET(FORCE)=1 GOTO D1L1
        NEW ISDNLOAD SET ISDNLOAD=($DATA(^TMG("TMGSIPH","DOWNLOADED",FILENUM,LPTR))'=0)
        IF 'ISDNLOAD DO  GOTO D1RDONE
        . SET RESULT="-1^Record doesn't seem to have been downloaded. A local record was probably used instead."
D1L1    NEW OPTION
        SET OPTION(FILENUM,LPTR)=0
        DO QTMVPTR^TMGFMUT(.OPTION)
        NEW DIE,DR,DA
        SET DIE=FILENUM
        SET DR=".01///@"
        SET DA=LPTR
        DO ^DIE
        NEW RPTR SET RPTR=+$GET(^TMG("TMGSIPH","DOWNLOADED",FILENUM,LPTR))
        KILL ^TMG("TMGSIPH","DOWNLOADED",FILENUM,LPTR)
        KILL ^TMG("TMGSIPH","PT XLAT",FILENUM,RPTR)
D1RDONE QUIT RESULT
 ;
 ;