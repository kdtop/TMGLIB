TMGSIPH ;TMG/kst/SIPHON PROGRAM, FOR TRANSFERRING VISTA INSTANCES ;11/27/09, 2/2/14
         ;;1.0;TMG-LIB;**1**;11/27/09
 ;
 ;"TMG SIPHON PROGRAM, FOR TRANSFERRING VISTA INSTANCE
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
 ;"LAUNCHSERVER --Main entry point for launching server for Siphon
 ;"LAUNCHCLIENT ; -- Main entry point for launching client for Siphon

 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"TMGKERN2,TMGUSRIF
 ;"=======================================================================
 ;
 ;"Note: The following globals are used.
 ;"
 ;"^TMG("TMGSIPH","DD",FILENUM,"PTR OUT",ONEREF,ENTRY)=""
 ;"    ; Note: ENTRY=DataPiece^PointedToFile^PointedToReference^IENDepth^[V]
 ;"    ; ONEREF will have multiple IEN entries IF IENDepth>1, e.g. '^SC(IEN,"S",IEN(2),1,IEN(3),"C")'
 ;"    ;        with order of IEN, IEN(2), IEN(3), ... etc.
 ;"^TMG("TMGSIPH","NEED RE-XREF",FILENUM)=""
 ;"^TMG("TMGSIPH","RE-XREF DONE",FILENUM,IEN)=""
 ;"^TMG("TMGSIPH","DOWNLOADED",FILENUM,LocalIEN)=RemoteIEN
 ;"^TMG("TMGSIPH","NEEDED RECORDS","PTOUT",FILENUM,RemotePointer,ReferToNodeToBeCorrected,INFO)=""
 ;"                      INFO=DataPiece^PointedToFile^PointedToReference^IENDepth^[V]
 ;"^TMG("TMGSIPH","NEEDED RECORDS","PTIN",FILENUM,IEN)=""
 ;"^TMG("TMGSIPH","OVERWRITTEN",REF)=@REF
 ;"^TMG("TMGSIPH","PT XLAT",FILENUM,RemoteIEN)=LocalIEN
 ;"               ;Note: IF FILENUM is subfile, DON'T store in 123.02{123 format.  Just use 123.02
 ;"^TMG("TMGSIPH","ALWAYS OVERWRITE LOCAL",FILENUM)=""
 ;"^TMG("TMGSIPH","RECORDS SYNC",FILENUM)=LastIEN^TotalNumIENS  (header entries from server-side file)
 ;"^TMG("TMGSIPH",".01 VALUE",FILENUM,RPTR)=Value (internal format)
 ;"
 ;"----- On server side, this array is used
 ;"^TMG("PTXREF","OUT",FROMFILE,IENS,FROMFLD,P2FILE,PT)=""
 ;"^TMG("PTXREF","IN",P2FILE,PT,FROMFILE,IENS,FROMFLD)=""
 ;"^TMG("PTXREF","XREFS",FILENUM,PTR,REF)=$GET(@REF)
 ;"^TMG("TMGSIPH",".01 VALUE",FILENUM,IEN)=VALUE  ;.01 value from record IEN (server-side IEN)

 ;
LAUNCHSERVER ;
        ;"Purpose: Main entry point for launching server for Siphon
        NEW RESULT
        SET RESULT=$$RUNSERVER^TMGKERN2(6321,"HANDLMSG^TMGSIPH0",1)
        QUIT
 ;
 ;
LAUNCHCLIENT ;
        ;"Purpose: Main entry point for launching client for Siphon
        JOB RUNCLIENT^TMGKERN2("localhost",6321)
        NEW MSGJOB SET MSGJOB=$ZJOB
        NEW TMGOWSAVE
        WRITE "Background task to talk to server launched in job #",MSGJOB,!
        NEW RESULT
        NEW COUNT SET COUNT=1
LC1     HANG 0.5
        SET RESULT=$GET(^TMG("TMP","TCP",MSGJOB,"RESULT"))
        SET COUNT=COUNT+1
        IF COUNT>60 DO  QUIT  ;"60 * 0.5 = 30 seconds timeout
        . WRITE "ERROR: Timeout waiting for client in job #",MSGJOB," to connect to server",!
        IF RESULT="" GOTO LC1
        IF +RESULT'=1 GOTO LC3
        ;
        WRITE "  =====================================================",!
        WRITE "  =                                                   =",!
        WRITE "  =                 -= TMG SIPHON =-                  =",!
        WRITE "  =                                                   =",!
        WRITE "  = Transfer data from one VistA instance to another  =",!
        WRITE "  =                                                   =",!
        WRITE "  =====================================================",!,!
        WRITE "NOTE: There should be NO other VistA users on the server,",!
        WRITE "as they might make unexpected and unmanagable changes to",!
        WRITE "the server database, interfering with the transfer process.",!,!
        NEW % SET %=2
        WRITE "Make a backup copy of local records if/when overwriting"
        DO YN^DICN WRITE !,!
        IF %=-1 GOTO LC3
        SET TMGOWSAVE=(%=1)  ;"Used in STOREDAS^TMGSIPHU
        DO MSGCLIENT^TMGKERN2(MSGJOB,"GET XREF AGE",.REPLY,.ERROR,5)
        IF $DATA(ERROR) WRITE ERROR,!
        NEW XRAGE SET XRAGE=+$GET(REPLY(1))
        SET %=1
        IF XRAGE>0 DO  GOTO:(%=-1) LC3
        . WRITE "Transfer information was last altered on the server ",XRAGE,"+",!
        . WRITE "hrs ago.  This should be refereshed IF there has been any",!
        . WRITE "change to records on the the server database in the interrum.",!
        . WRITE "Refreshing can add up-front time to the transfer, but is",!
        . WRITE "important for data integrety.",!,!
        . WRITE "DELETE old info now and recreate during transfers"
        . NEW % SET %=1 IF XRAGE<2 SET %=2
        . DO YN^DICN WRITE !
        . IF %'=1 QUIT
        . DO MSGCLIENT^TMGKERN2(MSGJOB,"WIPE PT XREF",.REPLY,.ERROR,5)
        . IF $DATA(ERROR) WRITE ERROR,!
        . ELSE  WRITE "OK.  Old transfer information deleted on server.",!,!
        ;
        NEW MENU,USRSLCT,TMP
LC2     KILL MENU,USRSLCT
        SET MENU(0)="Pick Option for Siphoning information"
        NEW IDX SET IDX=1
        SET MENU(IDX)="Transfer One (1) File (by record)"_$CHAR(9)_"TransFilebyRecs",IDX=IDX+1
        SET MENU(IDX)="Transfer One (1) patient"_$CHAR(9)_"TransPatient",IDX=IDX+1
        SET MENU(IDX)="Transfer One (1) record"_$CHAR(9)_"TransRecord",IDX=IDX+1
        NEW NPTO SET NPTO=$$NUMNEEDED^TMGSIPH3(MSGJOB,"PTOUT")
        NEW NPTI SET NPTI=$$NUMNEEDED^TMGSIPH3(MSGJOB,"PTIN")
        IF NPTO>0 DO
        . SET MENU(IDX)="Work on Unresolved Pointers OUT ("_NPTO_" pending)"_$CHAR(9)_"ResolveNeededPointersOUT",IDX=IDX+1
        . SET MENU(IDX)="AUTO MODE.  Get all Unresolved Pointers OUT ("_NPTO_" pending)"_$CHAR(9)_"ALLResolveNeededPointersOUT",IDX=IDX+1
        IF NPTI>0 DO
        . SET MENU(IDX)="Work on Unresolved Pointers IN ("_NPTI_" pending)"_$CHAR(9)_"ResolveNeededPointersIN",IDX=IDX+1
        . SET MENU(IDX)="AUTO MODE.  Get all Unresolved Pointers IN ("_NPTI_" pending)"_$CHAR(9)_"ALLResolveNeededPointersIN",IDX=IDX+1
        IF (NPTO>0)&(NPTI>0) DO
        . SET MENU(IDX)="IN & OUT AUTO MODE.  Get all Unresolved Pointers IN & OUT"_$CHAR(9)_"ALLResolveNeededPointersINOUT",IDX=IDX+1
        SET MENU(IDX)="<UTILITY MENU>"_$CHAR(9)_"Utility",IDX=IDX+1
        ;
        WRITE #
        SET USRSLCT=$$MENU^TMGUSRI2(.MENU,"^")
        IF USRSLCT="^" GOTO LC3
        IF USRSLCT=0 SET USRSLCT=""
        IF USRSLCT="ResolveNeededPointersOUT" SET TMP=$$HANDLNEEDED^TMGSIPH3(MSGJOB,"PTOUT",0) GOTO LC2
        IF USRSLCT="ResolveNeededPointersIN" SET TMP=$$HANDLNEEDED^TMGSIPH3(MSGJOB,"PTIN",0) GOTO LC2
        IF USRSLCT="ALLResolveNeededPointersOUT" SET TMP=$$HANDLNEEDED^TMGSIPH3(MSGJOB,"PTOUT",1) GOTO LC2
        IF USRSLCT="ALLResolveNeededPointersIN" SET TMP=$$HANDLNEEDED^TMGSIPH3(MSGJOB,"PTIN",1) GOTO LC2
        IF USRSLCT="ALLResolveNeededPointersINOUT" DO AUTONEEDED^TMGSIPH3(MSGJOB) GOTO LC2
        IF USRSLCT="TransPatient" DO TRANSPT^TMGSIPH4(MSGJOB) GOTO LC2
        IF USRSLCT="TransRecord" DO TRANSREC^TMGSIPH4(MSGJOB) GOTO LC2
        IF USRSLCT="TransFilebyRecs" DO CHKUPDTE^TMGSIPH4(MSGJOB,1) GOTO LC2
        IF USRSLCT="Utility" DO UTILITY(MSGJOB) GOTO LC2
        GOTO LC2
        ;
LC3     DO MSGCLIENT^TMGKERN2(MSGJOB,"#BYE#",.REPLY,.ERROR,5)
        IF $DATA(ERROR) WRITE ERROR,!
        HANG 0.5
        NEW Jobs
        DO MJOBS^TMGKERNL(.Jobs)
        IF $DATA(Jobs(MSGJOB)) do
        . WRITE "Background client #",MSGJOB," seems hung!",!
        . WRITE "Try typing [ESC] in server process.  When the server QUITs",!
        . WRITE "the background client should QUIT normally.",!
        . DO PRESS2GO^TMGUSRI2
        KILL ^TMG("TMP","TCP",MSGJOB)
        QUIT
 ;
 ;
UTILITY(MSGJOB) ;
        ;"Purpose: To have utility menu
        ;"
        NEW MENU,USRSLCT
U2      KILL MENU,USRSLCT
        SET MENU(0)="Pick UTILITY Option for Siphoning information"
        NEW IDX SET IDX=1
        SET MENU(IDX)="Work with data dictionaries"_$CHAR(9)_"DataDict",IDX=IDX+1
        SET MENU(IDX)="Query server global reference entries"_$CHAR(9)_"QueryServer",IDX=IDX+1
        SET MENU(IDX)="Transfer server global reference entry"_$CHAR(9)_"TransGlobal",IDX=IDX+1
        SET MENU(IDX)="Re-Index files transferred"_$CHAR(9)_"RE-XREF",IDX=IDX+1
        SET MENU(IDX)="AUTO check for NEW records in SET server files"_$CHAR(9)_"AutoCheckForNewRecords",IDX=IDX+1
        SET MENU(IDX)="Check for NEW records in server file"_$CHAR(9)_"CheckForNewRecords",IDX=IDX+1
        SET MENU(IDX)="Check for pointers IN to downloaded records"_$CHAR(9)_"CheckForPointersIN",IDX=IDX+1
        NEW NPTO SET NPTO=$$NUMNEEDED^TMGSIPH3(MSGJOB,"PTOUT")
        NEW NPTI SET NPTI=$$NUMNEEDED^TMGSIPH3(MSGJOB,"PTIN")
        IF NPTO>0 DO
        . SET MENU(IDX)="EXAMINE Unresolved Pointers OUT ("_NPTO_" pending)"_$CHAR(9)_"ExaminePointersOUT",IDX=IDX+1
        . SET MENU(IDX)="PREVIEW Unresolved Pointers OUT ("_NPTO_" pending)"_$CHAR(9)_"PreviewPointersOUT",IDX=IDX+1
        . SET MENU(IDX)="UN-NEED Unresolved Pointers OUT ("_NPTO_" pending)"_$CHAR(9)_"UnneedPointersOUT",IDX=IDX+1
        . SET MENU(IDX)="MAP Unresolved Pointers OUT ("_NPTO_" pending) to LOCAL records"_$CHAR(9)_"MapPointersOUTtoLocal",IDX=IDX+1
        SET MENU(IDX)="Show Information nodes"_$CHAR(9)_"ShowInfoNodes",IDX=IDX+1
        SET MENU(IDX)="Show Local Data Dictionary Browser"_$CHAR(9)_"VPE-DD",IDX=IDX+1
        SET MENU(IDX)="Delete a record that has been downloaded"_$CHAR(9)_"DeleteADownloadedRec",IDX=IDX+1
        ;"SET MENU(IDX)="do FIX"_$CHAR(9)_"FIX",IDX=IDX+1
        SET MENU(IDX)="Transfer Entire File (BLOCK COPY)/ Auto-resume Transfer"_$CHAR(9)_"TransferFile",IDX=IDX+1
        ;
        WRITE #
        SET USRSLCT=$$MENU^TMGUSRI2(.MENU,"^")
        IF USRSLCT="^" GOTO U3
        IF USRSLCT=0 SET USRSLCT=""
        IF USRSLCT="DataDict" DO COMPALLD^TMGSIPH1(MSGJOB) GOTO U2
        IF USRSLCT="QueryServer" DO QRYSERVER^TMGSIPH3(MSGJOB) GOTO U2
        IF USRSLCT="TransGlobal" DO TRANSREF^TMGSIPH3(MSGJOB) GOTO U2
        IF USRSLCT="ExaminePointersOUT" DO EXAMNEED^TMGSIPH5(MSGJOB,"PTOUT") GOTO U2
        IF USRSLCT="MapPointersOUTtoLocal" DO MAP2LOCAL^TMGSIPH3(MSGJOB) GOTO U2
        IF USRSLCT="UnneedPointersOUT" DO KILLNEED^TMGSIPH5(MSGJOB,"PTOUT") GOTO U2
        IF USRSLCT="PreviewPointersOUT" DO PREVIEW^TMGSIPH5(MSGJOB,"PTOUT") GOTO U2
        IF USRSLCT="ShowInfoNodes" DO BROWSENODES^TMGMISC($NAME(^TMG("TMGSIPH"))) GOTO U2
        IF USRSLCT="VPE-DD" DO ^%ZVEMD GOTO U2
        IF USRSLCT="CheckForNewRecords" DO CHKUPDTE^TMGSIPH4(MSGJOB) GOTO U2
        IF USRSLCT="AutoCheckForNewRecords" DO CHKSPUPD^TMGSIPH4(MSGJOB) GOTO U2
        IF USRSLCT="CheckForPointersIN" DO CHKPTIN^TMGSIPH5(MSGJOB) GOTO U2
        IF USRSLCT="RE-XREF" DO XRFILES^TMGSIPH6 GOTO U2
        IF USRSLCT="DeleteADownloadedRec" DO DELREC^TMGSIPH5 GOTO U2
        ;"IF USRSLCT="FIX" DO FIXSUBFILES^TMGFIX(MSGJOB) GOTO U2
        IF USRSLCT="TransferFile" DO TRANSFILE^TMGSIPH3(MSGJOB) GOTO LC2

        ;
U3      QUIT
 ;