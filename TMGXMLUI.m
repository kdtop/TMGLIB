TMGXMLUI ;TMG/kst/XML Exporter -- User Interface ;10/26/14, 6/6/17, 3/4/21
         ;;1.0;TMG-LIB;**1**;07/12/05
 ;
 ;"TMG XML EXPORT -- USER INTERFACE FUNCTIONS
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
 ;"UI
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"WELCOME()
 ;"PRCSFILE(REFARRAY,INDENT)
 ;"GETRECS(FILE,REFRECS,INDENT)
 ;"GETTPREC(FILE,REFRECS,s)
 ;"GTMANREC(FILE,REFRECS,s)
 ;"GETFLDS(FILE,REFARRAY,INDENT)
 ;"GETMFLDS(FILE,REFARRAY,s)
 ;"ASKCSTAG(FILE,FIELD,REFARRAY,INDENT)
 ;"ASKCXFRM(FILE,FIELD,REFARRAY,INDENT)
 ;"$$FMGETFLD(FILENUM)
 ;"$$ASKGTFLD(FILENUM,INDENT)
 ;"$$PCKUSFLD(FILENUM,REFARRAY,INDENT)
 ;"CFGORFLD(FILE,REFARRAY)
 ;"SHOWARR(INDENT)
 ;"PAUSE
 ;"WRITEHDR(REFHEADER)
 ;"HDRADDL(REFHEADER,LINE)
 ;"HDRDELLINE(REFHEADER,INDEX)
 ;"SPACES(NUM)
 ;
 ;"=======================================================================
 ;"Dependencies
 ;"XLFSTR
 ;"TMGDBAPI, TMGDEBUG, TMGMISC
 ;"=======================================================================
 ;"=======================================================================
 ;
UI(REFARRAY)  ;
        ;"Purpose: To create a User Interface (UI) for creating array needed to
        ;"              export XML data from Fileman.
        ;"Input: REFARRAY -- name of array to put data into
        ;"Output: values will be put into REFARRAY.  See TMGXMLEX for format
        ;"Result: 1 if OK to continue, 0 IF error or ABORT
        ;
        NEW RESULT SET RESULT=1
        IF $DATA(IOF)=0 DO  GOTO UIDN
        . WRITE "This function requires the VistA environment to be setup first.",!
        . WRITE "Terminating.  This may be achieved via DO ^XUP, then dropping",!
        . WRITE "back to the command line and trying to run this again.",!
        . SET RESULT=0
        ;
        ;"NEW DONE SET DONE=0
        NEW HEADERARR
        NEW REFHEADER SET REFHEADER="HEADERARR"
        SET REFARRAY=$GET(REFARRAY,"TMGARRAY")
        NEW TMGXMLARR SET TMGXMLARR=REFARRAY
        NEW INDENT SET INDENT=0
        NEW TABINC SET TABINC=5
        DO HDRADDL(REFHEADER," XML Export Assistant.")
        DO HDRADDL(REFHEADER,"=========================")
        ;
        SET RESULT=$$WELCOME
        IF RESULT=0 GOTO UIDN
        SET RESULT=$$PRCSFILE(REFARRAY,INDENT+TABINC)
        IF RESULT=0 GOTO UIDN
UIDN    QUIT RESULT
        ;
WELCOME()  ;
        ;"Purpose: Decribe the wizard
        ;"Input: none
        ;"Result: 1 if OK to continue.  0 IF user ABORT requested.
        ;"Note: uses global REFHEADER
        NEW RESULT SET RESULT=1
        DO WRITEHDR(REFHEADER)
        WRITE "Welcome.  I'll walk you through the process",!
        WRITE "of choosing the data you wish to export to an ",!
        WRITE "XML file.",!!
        WRITE "Overview of planned steps:",!
        WRITE "Step 1.  Pick 1st Fileman file to export.",!
        WRITE "Step 2.  Pick records in file to export.",!
        WRITE "Step 3.  Pick fields in records to export.",!
        WRITE "Step 4.  Pick 2nd Fileman file to export.",!
        WRITE "  ... repeat cycle until done.",!!
        WRITE "To back out, enter '^' at any prompt.",!!
WCL1    WRITE "Are you ready to begin?  (Y/N/^)  YES//"
        NEW INPUT
        READ INPUT:$GET(DTIME,3600),!
        IF $TEST=0 SET INPUT="N"
        IF INPUT="" SET INPUT="Y"
        SET INPUT=$$UP^XLFSTR(INPUT)
        IF (INPUT'["Y")!(INPUT["^") DO  GOTO WCMDN
        . ;"WRITE "Goodbye.",!
        . SET RESULT=0
        IF (INPUT["?") DO  GOTO WCL1
        . WRITE "  Enter Y or YES to continue.",!
        . WRITE "  Enter N or No or ^ to exit.",!!
        . DO PAUSE()
WCMDN QUIT RESULT
        ;
PRCSFILE(REFARRAY,INDENT) ;"PROCESS FILE
        ;"Purpose: To add export options for one file, or edit previous choices
        ;"Input: REFARRAY -- pointer to (i.e. name of) array to fill with info.
        ;"         INDENT -- amount to indent from left margin
        ;"Output: ARRAY will be filled with data in appropriate format (See docs in TMGXMLEX.m)
        ;"Result: 1 if OK to continue, 0 IF ABORTed
        ;"note: uses global variable REFHEADER,TABINC
        NEW DIC,FILE
        NEW Y SET Y=0
        NEW REF
        NEW RESULT SET RESULT=1
        NEW RECORDS
        IF $GET(REFARRAY)="" SET RESULT=0 GOTO PRCFDN
        ;
        DO HDRADDL(REFHEADER,$$SPACES(INDENT)_"Step 1.  Pick a FILE for export to XML.")
        ;
        NEW ANOTHER SET ANOTHER=0
        FOR  DO  QUIT:(+Y'>0)!(RESULT=0)
        . DO WRITEHDR(REFHEADER,1)
        . IF ANOTHER DO  QUIT:(RESULT=0)!(Y'>0)
        . . WRITE !,?INDENT,"Add another file for export? (Y/N/^) NO//"
        . . NEW INPUT READ INPUT:$GET(DTIME,3600),!
        . . IF INPUT="^" SET Y=0,RESULT=0 QUIT
        . . IF INPUT="" SET INPUT="N"
        . . SET INPUT=$$UP^XLFSTR(INPUT)
        . . IF INPUT'["Y" SET Y=0 QUIT  ;"signal to QUIT
        . . SET Y=1
        . SET DIC=1
        . SET DIC(0)="AEQ"
        . SET DIC("A")=$$SPACES(INDENT)_"Enter Fileman file for XML export (^ to QUIT):  ^// "
        . DO ^DIC
        . WRITE !
        . SET FILE=+Y
        . IF FILE'>0 SET RESULT=0 QUIT
        . SET REF=$NAME(@REFARRAY@(FILE))
        . IF $$GETRECS(FILE,REF,INDENT)=0 SET Y=0,RESULT=0 QUIT
        . SET ANOTHER=1
        ;
        DO HDRDELLINE(REFHEADER)
        ;
        IF RESULT=0 GOTO PRCFDN
        ;
        WRITE !,?INDENT,"Also export pointed-to records (Y/N/^) NO// "
        NEW INPUT READ INPUT:$GET(DTIME,3600),!
        IF INPUT="^" SET RESULT=0 GOTO PRCFDN
        IF INPUT="" SET INPUT="N"
        SET INPUT=$$UP^XLFSTR(INPUT)
        IF INPUT["Y" DO
        . DO XPNDPTRS(REFARRAY)
        ;
        SET RESULT=$$ASKFLAGS(REFARRAY,INDENT)
PRCFDN  QUIT RESULT
        ;
ASKFLAGS(REFARRAY,INDENT) ;
        ;"Purpose: To ask user IF various flags are desired
        ;"Input:  REFARRAY -- pointer to (i.e. name of) array to put data into
        ;"         INDENT -- amount to indent from left margin
        ;"Note: uses global variable REFHEADER
        ;"Result: 1 if OK to continue, 0 IF ABORTed
        NEW INPUT
        SET INDENT=$GET(INDENT,0)
        NEW RESULT SET RESULT=1
        IF $GET(REFARRAY)="" SET RESULT=0 GOTO AFLDN
        NEW DEFLABEL SET DEFLABEL="TMG_VISTA_XML_EXPORT"
        ;
        NEW SYSNAME,Y
        SET SYSNAME=$GET(^TMG("XML EXPORTER","EXPORT_SYSTEM_NAME"))
        IF SYSNAME="" DO
        . DO GETENV^%ZOSV
        . SET SYSNAME=$PIECE(Y,"^",4)
        SET @REFARRAY@("EXPORT_SYSTEM_NAME")=SYSNAME
        ;
        DO WRITEHDR(REFHEADER)
        ;
        WRITE ?INDENT,"Formatting Options:",!
        WRITE ?INDENT,"----------------------",!!
        ;
        WRITE ?INDENT,"Use Default export settings? (Y/N,^)  YES// "
        READ INPUT:$GET(DTIME,3600),!!
        IF INPUT="^" SET RESULT=0 GOTO AFLDN
        IF INPUT="" SET INPUT="Y"
        IF "YesyesYES"[INPUT DO  GOTO AFLDN
        . SET @REFARRAY@("FLAGS","i")=""   ;"<-- default value of indenting
        . SET @REFARRAY@("!DOCTYPE")=DEFLABEL
        . NEW SYSNAME,Y
        . SET SYSNAME=$GET(^TMG("XML EXPORTER","EXPORT_SYSTEM_NAME"))
        ;
        WRITE ?INDENT,"During export to XML file, DO you want empty fields to be",!
        WRITE ?INDENT,"reported (vs. no data --> tag not written)?  (Y/N,^)  NO// "
        READ INPUT:$GET(DTIME,3600),!!
        IF INPUT="^" SET RESULT=0 GOTO AFLDN
        IF INPUT="" SET INPUT="N"
        IF "YesyesYES"[INPUT DO
        . SET @REFARRAY@("FLAGS","b")=""
        ;
        WRITE ?INDENT,"Do you want the XML file to have entries indented for visual",!
        WRITE ?INDENT,"organization?  This will have no meaning to another program",!
        WRITE ?INDENT,"importing the XML file, but is easier for humans to read it ",!
        WRITE ?INDENT,"this way.  Indent entries? (Y/N,^) YES// "
        READ INPUT:$GET(DTIME,3600),!!
        IF INPUT="^" SET RESULT=0 GOTO AFLDN
        IF INPUT="" SET INPUT="Y"
        IF "YesyesYES"[INPUT DO
        . SET @REFARRAY@("FLAGS","i")=""
        ;
        WRITE ?INDENT,"Do you want the exported entries to be INTERNAL Fileman values?",!
        WRITE ?INDENT,"Export INTERNAL entries? (Y/N,^) NO// "
        READ INPUT:$GET(DTIME,3600),!!
        IF INPUT="^" SET RESULT=0 GOTO AFLDN
        IF INPUT="" SET INPUT="N"
        IF "YesyesYES"[INPUT DO
        . SET @REFARRAY@("FLAGS","I")=""
        ;
        WRITE ?INDENT,"Do you want the export the Fileman data dictionary? (Y/N,^) NO// "
        READ INPUT:$GET(DTIME,3600),!!
        IF INPUT="^" SET RESULT=0 GOTO AFLDN
        IF INPUT="" SET INPUT="N"
        IF "YesyesYES"[INPUT DO
        . SET @REFARRAY@("FLAGS","D")=""
        ;
        WRITE ?INDENT,"Output export settings? (Y/N,^) YES// "
        READ INPUT:$GET(DTIME,3600),!!
        IF INPUT="^" SET RESULT=0 GOTO AFLDN
        IF INPUT="" SET INPUT="Y"
        IF "YesyesYES"[INPUT DO
        . SET @REFARRAY@("FLAGS","S")=""
        ;
        NEW DEFLABEL SET DEFLABEL="TMG_VISTA_XML_EXPORT"
        WRITE ?INDENT,"Use default XML !DOCTYPE '"_DEFLABEL_"' label? (Y/N,^) YES// "
        READ INPUT:$GET(DTIME,3600),!!
        IF INPUT="^" SET RESULT=0 GOTO AFLDN
        IF INPUT="" SET INPUT="Y"
        IF "YesyesYES"[INPUT DO
        . SET @REFARRAY@("!DOCTYPE")=DEFLABEL
        ELSE  DO  GOTO:(RESULT=0) AFLDN
        . WRITE ?INDENT,"Specify a *custom* XML !DOCTYPE label? (Y/N,^) NO// "
        . READ INPUT:$GET(DTIME,3600),!!
        . IF INPUT="^" SET RESULT=0 QUIT
        . IF INPUT="" SET INPUT="Y"
        . IF "YesyesYES"[INPUT DO
        . . WRITE "Enter label for <!DOCTYPE YourInputGoesHere>",!
        . . WRITE "Enter Label: //"
        . . READ INPUT:$GET(DTIME,3600),!!
        . . IF INPUT="^" SET RESULT=0 QUIT
        . . IF INPUT'="" SET @REFARRAY@("!DOCTYPE")=INPUT
        ;
        WRITE ?INDENT,"Enter a name for this VistA installation. ",SYSNAME,"// "
        READ INPUT:$GET(DTIME,3600),!!
        IF INPUT="^" SET RESULT=0 GOTO AFLDN
        IF INPUT="" SET INPUT=SYSNAME
        SET SYSNAME=INPUT
        SET ^TMG("XML EXPORTER","EXPORT_SYSTEM_NAME")=SYSNAME
        SET @REFARRAY@("EXPORT_SYSTEM_NAME")=SYSNAME
        ;
AFLDN   QUIT RESULT
        ;
        ;"NOTE:  I need to notice if File has already been set (i.e. user choosing file a second time
        ;"      If so give option to erase old choices and choose again
GETRECS(FILE,REFRECS,INDENT) ;
        ;"Purpose: For a given file, allow selection of records to export.
        ;"Input: FILE -- the FILE (name or number) to select from.
        ;"        pRec -- Pointer to (i.e. name of) array to fill with records nums
        ;"        INDENT -- a value to indent from left margin
        ;"Result: 1 if OK to continue, 0 IF user ABORTed.
        ;"Note: uses global variable REFHEADER,TABINC

        NEW RESULT SET RESULT=1
        NEW INPUT SET INPUT=""
        NEW FILENUMBER,FILENAME
        IF ($GET(FILE)="")!($GET(REFRECS)="") SET RESULT=0 GOTO GRDN
        NEW DEFVALUE SET DEFVALUE="X"
        ;
        IF +FILE=FILE DO
        . SET FILENUMBER=FILE
        . SET FILENAME=$$GETFNAME^TMGXMLT2(FILE)
        ELSE  DO
        . SET FILENAME=FILE
        . SET FILENUMBER=$$GETFNUM^TMGXMLT2(FILE)
        ;
        DO HDRADDL(REFHEADER,$$SPACES(INDENT)_"Step 2.  Which RECORDS to export from file "_FILENAME_"?")
        ;
        FOR  DO  QUIT:(INPUT="^")!(RESULT=0)
        . DO WRITEHDR(REFHEADER)
        . WRITE ?INDENT,"1. Export ALL records (exclusions allowed).",!
        . WRITE ?INDENT,"2. Select a Search/Sort TEMPLATE to specify records.",!
        . WRITE ?INDENT,"3. Select SPECIFIC records",!
        . WRITE ?INDENT,"4. Select records to EXCLUDE",!
        . WRITE ?INDENT,"5. View selections so far.",!
        . WRITE ?INDENT,"X. Done here.",!!
        . WRITE ?INDENT,"Select option (1-5 or X or ? or ^): "_DEFVALUE_"// "
        . READ INPUT:$GET(DTIME,3600),!!
        . IF $TEST=0 SET INPUT="^"
        . IF INPUT="" SET INPUT=DEFVALUE
        . IF ("Xx"[INPUT) DO  QUIT
        . . IF $DATA(@REFRECS)'>1 DO  QUIT:(INPUT="")
        . . . WRITE ?INDENT,"NOTE: No records were chosen for export in file: ",FILENAME,!
        . . . WRITE ?INDENT,"This means that nothing will be exported to the XML file.",!!
        . . . WRITE ?INDENT,"Do you still want to stop selecting records? (Y,N,^) NO// "
        . . . NEW Done READ Done:$GET(DTIME,3600),!
        . . . IF $TEST=0 SET Done="^"
        . . . IF (Done="")!("NOnoNo"[Done) SET INPUT=""
        . . SET INPUT="^"
        . IF INPUT="^" SET RESULT=0 QUIT
        . IF (INPUT>0)&(INPUT<6) SET DEFVALUE=INPUT
        . IF INPUT="?" DO  QUIT
        . . WRITE !
        . . WRITE ?INDENT,"  Enter '1' IF you wish to export ALL records in this file.",!
        . . WRITE ?INDENT,"              You can still specify records to exclude after this option.",!
        . . WRITE ?INDENT,"  Enter '2' IF you wish to use a pre-existing Search/Sort TEMPLATE",!
        . . WRITE ?INDENT,"              to select files.  A Search/Sort TEMPLATE can be generated",!
        . . WRITE ?INDENT,"              through the Fileman Search function.",!
        . . WRITE ?INDENT,"  Enter '3' IF you know the record nubmers (IEN values) for the",!
        . . WRITE ?INDENT,"              records you wish to export, and want to enter them",!
        . . WRITE ?INDENT,"              manually.",!
        . . WRITE ?INDENT,"  Enter '4' IF you have records to EXCLUDE.  If a record is excluded,",!
        . . WRITE ?INDENT,"               then it will NOT be output, even IF it was specified ",!
        . . WRITE ?INDENT,"               manually or was included from a Search/Sort TEMPLATE.",!
        . . WRITE ?INDENT,"  Enter '5' to view array containing settings so far.",!
        . . WRITE ?INDENT,"  Enter 'X' to exit..",!
        . . WRITE ?INDENT,"  Enter '^' to abort entire process.",!
        . . DO PAUSE(INDENT)
        . IF INPUT=1 DO
        . . SET @REFRECS@("*")=""
        . . WRITE ?INDENT,"OK.  Will export all records in file: ",FILENAME,".",!
        . . SET DEFVALUE="X"
        . . DO PAUSE(INDENT)
        . IF INPUT=2 SET RESULT=$$GETTPREC(FILE,REFRECS,"for INCLUSION ",INDENT+TABINC) SET DEFVALUE="X"
        . IF INPUT=3 SET RESULT=$$GTMANREC(FILE,REFRECS,"for INCLUSION ",INDENT+TABINC) SET DEFVALUE="X"
        . IF INPUT=4 SET RESULT=$$GETEXRCS(FILE,REFRECS,INDENT+TABINC) SET DEFVALUE="X"
        . IF INPUT=5 DO SHOWARR(INDENT)
        ;
GRDN    IF $DATA(@REFRECS)'>1 DO
        . WRITE ?INDENT,"NOTE: No records were chosen.  Aborting.",!
        . SET RESULT=0
        ELSE  WRITE ?INDENT,"Done chosing records...",!
        ;
        WRITE ?INDENT,"Now on to picking FIELDS to export.",!
        DO PAUSE(INDENT)
        IF $$GETFLDS(FILE,REF,INDENT)=0 SET Y=0,RESULT=0
        WRITE !
        DO HDRDELLINE(REFHEADER)
        QUIT RESULT
        ;
GETEXRCS(FILE,REFRECS,INDENT)  ;"GET EXCLUDE RECS
        ;"Purpose: to allow user to enter records to exclude
        ;"Input: FILE -- the FILE (name or number) to select from.
        ;"        pRec -- Pointer to (i.e. name of) array to fill with records nums
        ;"        INDENT -- a value to indent from left margin
        ;"Result: 1 if OK to continue, 0 IF user ABORTed.
        ;"Note: uses global variable REFHEADER,TABINC
        NEW RESULT SET RESULT=1
        NEW FILENUMBER,FILENAME
        NEW INPUT SET INPUT=""
        IF ($GET(FILE)="")!($GET(REFRECS)="") SET RESULT=0 GOTO GRDN
        NEW DEFVALUE SET DEFVALUE="X"
        ;
        IF +FILE=FILE DO
        . SET FILENUMBER=FILE
        . SET FILENAME=$$GETFNAME^TMGXMLT2(FILE)
        ELSE  DO
        . SET FILENAME=FILE
        . SET FILENUMBER=$$GETFNUM^TMGXMLT2(FILE)
        SET INDENT=+$GET(INDENT,0)
        ;
        DO HDRADDL(REFHEADER,$$SPACES(INDENT)_"To EXCLUDE records in file "_FILENAME_", choose:")
        ;
        FOR  DO  QUIT:(INPUT="")!(RESULT=0)
        . DO WRITEHDR(REFHEADER)
        . WRITE ?INDENT,"1. Select a Search/Sort TEMPLATE to specify records to EXCLUDE.",!
        . WRITE ?INDENT,"2. Select SPECIFIC record numbers to EXCLUDE.",!
        . WRITE ?INDENT,"3. View all the records excluded so far.",!
        . WRITE ?INDENT,"X. Done here.",!!
        . WRITE ?INDENT,"Select option (1-3 or X or ? or ^)  "_DEFVALUE_"// "
        . READ INPUT:$GET(DTIME,3600),!
        . IF $TEST=0 SET INPUT="^"
        . IF INPUT="" SET INPUT=DEFVALUE
        . IF ("Xx"[INPUT) SET INPUT=""
        . IF INPUT="^" SET RESULT=0 QUIT
        . IF (INPUT>0)&(INPUT<4) SET DEFVALUE=INPUT
        . IF INPUT="?" DO
        . . WRITE !,?INDENT,"  By excluding just certain records, you can export every record",!
        . . WRITE ?INDENT,"  EXCEPT those you specify.",!
        . . DO PAUSE(INDENT)
        . IF INPUT=1 DO
        . . NEW REFARRAY SET REFARRAY=$NAME(@REFRECS@("Rec Exclude"))
        . . SET RESULT=$$GETTPREC(FILE,REFARRAY,"for EXCLUSION ",INDENT+TABINC)
        . IF INPUT=2 DO
        . . NEW REFARRAY SET REFARRAY=$NAME(@REFRECS@("Rec Exclude"))
        . . SET RESULT=$$GTMANREC(FILE,REFARRAY,"for EXCLUSION ",INDENT+TABINC)
        . IF INPUT=3 DO SHOWARR(INDENT)
        ;
        DO HDRDELLINE(REFHEADER)
        ;
GERDN   QUIT RESULT
        ;
GETTPREC(FILE,REFRECS,s,INDENT)  ;"GET TEMPLATE RECS
        ;"Purpose: to ask user for a search/sort template to inport records from
        ;"Input -- FILE -- the file name or number to work with
        ;"           REFRECS -- pointer to (i.e. name of) array to fill
        ;"                      will probably be passed with "ARRAY(12345)"
        ;"        s -- OPTIONAL -- e.g. "for INCLUSION " or "for EXCLUSION " -- part of title.
        ;"        INDENT -- OPTIONAL -- a value to indent from left margin
        ;"Output: Data is put into REFRECS like this:
        ;"              @REFRECS@(IEN1)=""
        ;"              @REFRECS@(IEN2)=""
        ;"              @REFRECS@(IEN3)=""
        ;"Result: 1 if OK to continue, 0 IF user ABORTed.
        ;"Note: uses global variable REFHEADER (if available)
        NEW FILENUMBER,FILENAME,Y
        IF ($GET(FILE)="")!($GET(REFRECS)="") GOTO GTRDN
        NEW TEMPHDR SET REFHEADER=$GET(REFHEADER,"TEMPHDR")
        NEW RESULT SET RESULT=1
        ;
        IF +FILE=FILE DO
        . SET FILENUMBER=FILE
        . SET FILENAME=$$GETFNAME^TMGXMLT2(FILE)
        ELSE  DO
        . SET FILENAME=FILE
        . SET FILENUMBER=$$GETFNUM^TMGXMLT2(FILE)
        IF FILENUMBER'>0 DO  GOTO GTRDN
        . DO SHOWERR^TMGDEBU2(.ERRFOUND,"Error: Requested file, "_FILE_", doesn't exist.")
        . SET RESULT=0
        ;
        SET INDENT=+$GET(INDENT,0)
        ;
        DO HDRADDL(REFHEADER,$$SPACES(INDENT)_"Select records for export from a Template")
        ;
        FOR  DO  QUIT:((+Y>0)!(+Y=-1))
        . DO WRITEHDR(REFHEADER)
        . NEW DIC SET DIC=.401
        . SET DIC(0)="AEQ"
        . WRITE $$SPACES(INDENT)_"Select a Template containing records for import. ",!
        . WRITE $$SPACES(INDENT)_"(? for list, ^ to QUIT) "
        . SET DIC("A")=$$SPACES(INDENT)_"Enter Template: "
        . SET DIC("S")="IF $P($G(^DIBT(+Y,0)),""^"",4)="_FILENUMBER  ;"screen for Templates by file
        . DO ^DIC
        . WRITE !
        . IF +Y'>0 QUIT  ;"set RESULT=0
        . NEW NODE SET NODE=$GET(^DIBT(+Y,0))
        . IF $PIECE(NODE,"^",4)'=FILENUMBER DO  QUIT
        . . SET Y=0  ;"signal to try again
        . . NEW ERRFOUND
        . . DO SHOWERR^TMGDEBU2(.ERRFOUND,"Error: That template doesn't contain records from "_FILE_". Please select another.")
        . . DO PAUSE(INDENT)
        ;
        IF RESULT=0 GOTO GTRL1
        ;
        NEW COUNT SET COUNT=0
        IF (+Y>0)&($DATA(^DIBT(+Y,1))>1) DO
        . NEW INDEX SET INDEX=$ORDER(^DIBT(+Y,1,0))
        . IF INDEX'="" FOR  DO  QUIT:(INDEX="")
        . . SET @REFRECS@(INDEX)=""
        . . SET COUNT=COUNT+1
        . . SET INDEX=$ORDER(^DIBT(+Y,1,INDEX))
        ;
        WRITE ?INDENT,COUNT," Records imported.",!
        DO PAUSE(INDENT)
        ;
GTRL1   DO HDRDELLINE(REFHEADER)
GTRDN   QUIT RESULT
        ;
GTMANREC(FILE,REFRECS,s,INDENT)  ;"GET MAN RECS
        ;"Purpose: to ask user for a series of IEN values
        ;"Input: FILE -- name or number, file to get IENS's for
        ;"        REFRECS -- a pointer to (i.e. Name of) array to put IEN's into
        ;"        s -- OPTIONAL -- e.g. "for INCLUSION " or "for EXCLUSION " -- part of title.
        ;"Output: Data is put into REFRECS like this:
        ;"              @REFRECS@(IEN1)=""
        ;"              @REFRECS@(IEN2)=""
        ;"              @REFRECS@(IEN3)=""
        ;"Result: 1 if OK to continue, 0 IF user ABORTed.
        ;"Note: uses global variable REFHEADER
        NEW ERRFOUND
        NEW FILENUMBER,FILENAME
        NEW RESULT SET RESULT=1
        IF ($GET(FILE)="")!($GET(REFRECS)="") SET RESULT=0 GOTO GRDN
        ;
        IF +FILE=FILE DO
        . SET FILENUMBER=FILE
        . SET FILENAME=$$GETFNAME^TMGXMLT2(FILE)
        ELSE  DO
        . SET FILENAME=FILE
        . SET FILENUMBER=$$GETFNUM^TMGXMLT2(FILE)
        IF FILENUMBER'>0 DO  GOTO GMRDN
        . DO SHOWERR^TMGDEBU2(.ERRFOUND,"Error: Requested file, "_FILE_", doesn't exist.")
        . DO PAUSE(INDENT)
        . SET RESULT=0
        ;
        NEW OREF SET OREF=$GET(^DIC(FILENUMBER,0,"GL"))
        IF OREF="" DO  GOTO GRDN
        . DO SHOWERR^TMGDEBU2(.ERRFOUND,"Error: Can't find global reference for file: "_FILENUMBER_".")
        . DO PAUSE(INDENT)
        . SET RESULT=0
        ;
        NEW DEFVALUE SET DEFVALUE="X"
        ;
        DO HDRADDL(REFHEADER,$$SPACES(INDENT)_"Select specific record "_$GET(s)_"in file "_FILENAME)
        ;
        NEW INPUT
        FOR  DO  QUIT:(INPUT="")!(RESULT=0)
        . DO WRITEHDR(REFHEADER)
        . WRITE ?INDENT,"1. Use Fileman to find record.",!
        . WRITE ?INDENT,"2. Enter record number by hand.",!
        . WRITE ?INDENT,"3. View all the records selected so far.",!
        . WRITE ?INDENT,"X. Done here.",!
        . WRITE !,?INDENT,"Select Option (1-3 or X or ^)  "_DEFVALUE_"//"
        . READ INPUT:$GET(DTIME,3600),!!
        . IF $TEST=0 SET INPUT="^"
        . IF INPUT="" SET INPUT=DEFVALUE
        . IF "Xx"[INPUT SET INPUT="" QUIT
        . IF INPUT="^" SET RESULT=0 QUIT
        . IF (INPUT>0)&(INPUT<4) SET DEFVALUE=INPUT
        . IF INPUT=1 DO
        . . NEW DIC
        . . SET DIC=FILE
        . . SET DIC(0)="AEQ"
        . . SET DIC("A")=$$SPACES(INDENT)_"Select record in "_FILENAME_" (? for list, ^ to QUIT): "
        . . DO ^DIC
        . . WRITE !
        . . IF +Y>0 DO
        . . . WRITE !,?INDENT,"O.K.  You selected record number (IEN): ",+Y,!
        . . . SET @REFRECS@(+Y)=""
        . . . DO PAUSE(INDENT)
        . . ;" ELSE  SET RESULT=0 QUIT
        . . SET DEFVALUE="X"
        . IF INPUT=2 DO
        . . NEW IEN
        . . READ ?INDENT,"Enter record number (a.k.a. IEN) (^ to abort): ",IEN:$GET(DTIME,3600),!
        . . IF $TEST=0 SET EIN="^"
        . . IF IEN="^" SET RESULT=0 QUIT
        . . IF +IEN>0 DO
        . . . NEW REF SET REF=OREF_IEN_")"
        . . . IF $DATA(@REF)'>0 DO  QUIT
        . . . . WRITE ?INDENT,"Sorry. That record number (IEN) doesn't exist.",!
        . . . . DO PAUSE(INDENT)
        . . . SET @REFRECS@(IEN)=""
        . . . WRITE ?INDENT,"O.K.  You selected record number (IEN): ",IEN,!
        . . . DO PAUSE(INDENT)
        . IF INPUT=3 DO SHOWARR(INDENT)
        ;
        DO HDRDELLINE(REFHEADER)
        ;
GMRDN   QUIT RESULT
        ;
GETFLDS(FILE,REFARRAY,INDENT) ;"GET FIELDS
        ;"Purpose: To query the user as to which fields to export for records
        ;"Input:  FILE -- the FILE number or name to work with.
        ;"          REFARRAY -- point to (i.e. name of) ARRAY to work with.  Format discussed in TMGXMLEX.m
        ;"                      will likely be equal to "ARRAY(FILENUMBER)"
        ;"          INDENT -- a value to indent from left margin
        ;"Result: 1 if OK to continue.  0 IF user ABORTed.
        ;"Note: uses global variable REFHEADER,TABINC
        NEW RESULT SET RESULT=1
        NEW FILENUMBER,FILENAME
        IF ($GET(FILE)="")!($GET(REFARRAY)="") SET RESULT=0 GOTO GRDN
        ;
        IF +FILE=FILE DO
        . SET FILENUMBER=FILE
        . SET FILENAME=$$GETFNAME^TMGXMLT2(FILE)
        ELSE  DO
        . SET FILENAME=FILE
        . SET FILENUMBER=$$GETFNUM^TMGXMLT2(FILE)
        IF FILENUMBER'>0 DO
        . DO SHOWERR^TMGDEBU2(.ERRFOUND,"Error: Requested file, "_FILE_", doesn't exist.")
        ;
        DO HDRADDL(REFHEADER,$$SPACES(INDENT)_"Step 3.  Which FIELDS to export from file "_FILENAME_"?")
        ;
        NEW DEFVALUE SET DEFVALUE=1
        NEW INPUT
        FOR  DO  QUIT:(INPUT="")!(RESULT=0)
        . DO WRITEHDR(REFHEADER)
        . WRITE ?INDENT,"1. Export ALL fields (exclusions allowed).",!
        . WRITE ?INDENT,"2. Select SPECIFIC field numbers.",!
        . WRITE ?INDENT,"3. Select fields to EXCLUDE",!
        . WRITE ?INDENT,"4. View selections so far.",!
        . WRITE ?INDENT,"X. Done here.",!!
        . WRITE ?INDENT,"Select option (1-4 or X or ? or ^): "_DEFVALUE_"// "
        . READ INPUT:$GET(DTIME,3600),!!
        . IF $TEST=0 SET INPUT="^"
        . IF INPUT="" SET INPUT=DEFVALUE
        . IF ("Xx"[INPUT) SET INPUT=""
        . IF INPUT="^" SET RESULT=0 QUIT
        . IF (INPUT>0)&(INPUT<5) SET DEFVALUE=INPUT
        . IF INPUT="?" DO  QUIT
        . . WRITE !
        . . WRITE ?INDENT,"  Enter '1' IF you wish to export ALL fields for this file.",!
        . . WRITE ?INDENT,"              You can still specify fields  to exclude after this option.",!
        . . WRITE ?INDENT,"  Enter '2' IF you know the field numbers you wish to export,",!
        . . WRITE ?INDENT,"              and want to enter them manually.",!
        . . WRITE ?INDENT,"  Enter '3' IF you have fields to EXCLUDE.  If a field is excluded,",!
        . . WRITE ?INDENT,"               then it will NOT be output, even IF it was specified manually.",!
        . . WRITE ?INDENT,"  Enter '4' to view array containing settings so far.",!
        . . WRITE ?INDENT,"  Enter 'X' to exit..",!
        . . WRITE ?INDENT,"  Enter '^' to abort entire process.",!
        . . DO PAUSE(INDENT)
        . IF INPUT=1 DO  QUIT
        . . SET @REFARRAY@("TEMPLATE","*")=""
        . . WRITE ?INDENT,"OK.  Will export all fields (and any sub-fields) in file ",FILENAME,".",!
        . . DO PAUSE(INDENT)
        . . SET DEFVALUE="X"
        . IF INPUT=2 DO  QUIT
        . . NEW TEMP SET TEMP=$NAME(@REFARRAY@("TEMPLATE"))
        . . SET RESULT=$$GETMFLDS(FILE,TEMP,"for INCLUSION ",INDENT+TABINC)
        . IF INPUT=3 DO  QUIT
        . . NEW TEMP SET TEMP=$NAME(@REFARRAY@("TEMPLATE","Field Exclude"))
        . . SET RESULT=$$GETMFLDS(FILE,TEMP,"for EXCLUSION ",INDENT+TABINC)
        . IF INPUT=4 DO SHOWARR(INDENT)
        ;
        WRITE ?INDENT,"Done choosing FIELDS.",!
        ;
        NEW REF
        ;"set REF=$NAME(@REFARRAY@(FILE,"TEMPLATE"))
        SET REF=$NAME(@REFARRAY@("TEMPLATE"))
        SET RESULT=$$CFGORFLD(FILE,REF,INDENT)
        IF RESULT=0 SET Y=0 QUIT
        ;
        DO HDRDELLINE(REFHEADER)
        QUIT RESULT
        ;
GETMFLDS(FILE,REFARRAY,s,INDENT)  ;"GET MAN FIELDS
        ;"Purpose: to ask user for a series of field values
        ;"Input: FILE -- name or number, file to get field numbers for
        ;"        REFARRAY -- a pointer to (i.e. Name of) array to put field numbers into
        ;"              will probably be something one of the following:
        ;"                      "ARRAY(FILENUMBER,"TEMPLATE")"
        ;"                      "ARRAY(FILENUMBER,"TEMPLATE","Field Exclude")"
        ;"                      "ARRAY(FILENUMBER,RecNumber)"
        ;"        s -- OPTIONAL -- e.g. "for INCLUSION " or "for EXCLUSION " -- part of title.
        ;"      indend -- optional -- a value to indent from left margin
        ;"Output: Data is put into REFARRAY
        ;"Result: 1 if OK to continue.  0 IF user ABORTed.
        ;"Note: uses global variable REFHEADER,TABINC
        ;
        NEW ERRFOUND
        NEW FILENUMBER,FILENAME
        NEW RESULT SET RESULT=1
        IF ($GET(FILE)="")!($GET(REFARRAY)="") SET RESULT=0 GOTO GRDN
        SET INDENT=$GET(INDENT,0)
        NEW DEFVALUE SET DEFVALUE="X"
        ;
        IF +FILE=FILE DO
        . SET FILENUMBER=FILE
        . SET FILENAME=$$GETFNAME^TMGXMLT2(FILE)
        ELSE  DO
        . SET FILENAME=FILE
        . SET FILENUMBER=$$GETFNUM^TMGXMLT2(FILE)
        IF FILENUMBER'>0 DO  GOTO GRDN
        . DO SHOWERR^TMGDEBU2(.ERRFOUND,"Error: Requested file, "_FILE_", doesn't exist.")
        . SET RESULT=0
        ;
        DO HDRADDL(REFHEADER,$$SPACES(INDENT)_"Which SPECIFIC FIELDS "_$GET(s)_"to export?")
        ;
        NEW INPUT
        FOR  DO  QUIT:(INPUT="")!(RESULT=0)
        . NEW FIELD SET FIELD=0
        . DO WRITEHDR(REFHEADER)
        . WRITE ?INDENT,"1. Select ALL fields.",!
        . WRITE ?INDENT,"2. Use Fileman to find FIELD number.",!
        . WRITE ?INDENT,"3. Enter FIELD by hand.",!
        . WRITE ?INDENT,"4. Pick an UNSELECTED field.",!
        . WRITE ?INDENT,"5. View all the FIELDS selected so far.",!
        . WRITE ?INDENT,"X. Done here.",!
        . WRITE !,?INDENT,"Select Option (1-5 or X or ^)  ",DEFVALUE,"//"
        . READ INPUT:$GET(DTIME,3600),!!
        . IF $TEST=0 SET INPUT="^"
        . IF INPUT="" SET INPUT=DEFVALUE
        . IF "Xx"[INPUT SET INPUT="" QUIT
        . IF INPUT="^" SET RESULT=0 QUIT
        . IF (INPUT>0)&(INPUT<6) SET DEFVALUE=INPUT
        . IF INPUT="5" DO  QUIT
        . . DO SHOWARR(INDENT)
        . IF INPUT="1" DO
        . . WRITE "OK  All fields selected.",!
        . . SET @REFARRAY@("*")=""
        . IF INPUT="2" SET FIELD=$$FMGETFLD(FILENUMBER,INDENT)
        . IF INPUT="3" SET FIELD=$$ASKGTFLD(FILENUMBER,INDENT)
        . IF INPUT="4" SET FIELD=$$PCKUSFLD(FILENUMBER,REFARRAY,INDENT)
        . IF FIELD=-1 SET RESULT=0 QUIT
        . IF FIELD>0 DO
        . . SET @REFARRAY@(FIELD)=""
        . . IF $GET(s)'="for EXCLUSION " DO  QUIT:(RESULT=0)
        . . . SET RESULT=$$ASKCSTAG(FILENUMBER,FIELD,REFARRAY,INDENT)
        . . . IF RESULT=0 QUIT
        . . . SET RESULT=$$ASKCXFRM(FILENUMBER,FIELD,REFARRAY,INDENT)
        . . . IF RESULT=0 QUIT
        . . ;"Now, determine IF we need to DO sub-fields
        . . NEW FLDINFO
        . . DO GFLDINFO^TMGXMLT2(FILENUMBER,FIELD,"FLDINFO","LABEL")
        . . IF $GET(FLDINFO("MULTIPLE-VALUED"))>0 DO
        . . . IF $GET(FLDINFO("TYPE"))="WORD PROCESSING" QUIT
        . . . NEW SUBFILE SET SUBFILE=+$GET(FLDINFO("SPECIFIER"))
        . . . IF SUBFILE=0 QUIT
        . . . NEW FIELDLST  IF $$GTFLDLST^TMGXMLT2(SUBFILE,"FIELDLST")=0 QUIT
        . . . NEW SUBARRAY SET SUBARRAY=$NAME(@REFARRAY@(FIELD,"TEMPLATE"))
        . . . IF $$LISTCT^TMGMISC2("FIELDLST")=1 DO  QUIT
        . . . . NEW SUBFIELD SET SUBFIELD=$ORDER(FIELDLST(""))
        . . . . NEW SUBFNAME SET SUBFNAME=$$GTFLDNAM^TMGXMLT2(SUBFILE,SUBFIELD)
        . . . . WRITE ?INDENT,"Field ",$GET(FLDINFO("LABEL"))," (#",FIELD,") has exactly 1 sub-field (",SUBFNAME,")",!
        . . . . WRITE ?INDENT,"It has been automatically selected for you.",!
        . . . . SET @SUBARRAY@(SUBFIELD)=""
        . . . . IF $GET(s)'="for EXCLUSION " DO  QUIT:(RESULT=0)
        . . . . . SET RESULT=$$ASKCSTAG(SUBFILE,SUBFIELD,SUBARRAY,INDENT)
        . . . . . IF RESULT=0 QUIT
        . . . . . SET RESULT=$$ASKCXFRM(SUBFILE,SUBFIELD,SUBARRAY,INDENT)
        . . . . . IF RESULT=0 QUIT
        . . . WRITE ?INDENT,"Field ",$GET(FLDINFO("LABEL"))," (#",FIELD,") has sub-fields.  We'll select those next.",!
        . . . DO PAUSE(INDENT)
        . . . SET RESULT=$$GETMFLDS(SUBFILE,SUBARRAY,s,INDENT+TABINC)
        . DO PAUSE(INDENT)
        ;
        DO HDRDELLINE(REFHEADER)
        ;
GMFDN   QUIT RESULT
        ;
ASKCSTAG(FILE,FIELD,REFARRAY,INDENT)  ;"ASK CUSTOM TAG
        ;"Purpose: Ask user IF they want a custom output tag for a field
        ;"Input: FILENUMBER -- the name or number of the file to work with
        ;"        FIELD -- the number of the field to work with
        ;"        REFARRAY -- the array to put answer in.
        ;"              value passed will probably be like this:
        ;"              e.g. array(22704,"TEMPLATE") or
        ;"              e.g. array(22704,"TEMPLATE",2,"TEMPLATE")
        ;"       INDENT -- the indent value from left margin
        ;"Output: value is put in, IF user wants, like this
        ;"              e.g. array(22704,"TEMPLATE","TAG NAME",.01)="Custom name"
        ;"              e.g. array(22704,"TEMPLATE",2,"TEMPLATE","TRANSFORM",.01)="Custom name"
        ;"Result: 1 if OK to continue.  0 IF user ABORTed.
        NEW RESULT SET RESULT=1
        IF (+$GET(FILE)=0)!($GET(FIELD)="")!($GET(REFARRAY)="") SET RESULT=0 GOTO ACTDN
        SET INDENT=$GET(INDENT,0)
        NEW DEFTAG SET DEFTAG=$GET(@REFARRAY@("TAG NAME",FIELD))
        IF DEFTAG="" SET DEFTAG=$$GTFLDNAM^TMGXMLT2(FILE,FIELD)
        WRITE ?INDENT,"Tag name to use in XML file?  ",DEFTAG,"// "
        NEW TAGNAME READ TAGNAME:$GET(DTIME,3600),!
        IF TAGNAME="^" SET RESULT=0
        IF (TAGNAME'="")&(TAGNAME'="^") SET @REFARRAY@("TAG NAME",FIELD)=TAGNAME
ACTDN   QUIT RESULT
        ;
ASKCXFRM(FILE,FIELD,REFARRAY,INDENT)  ;"ASK CUSTOM TRANSFORM
        ;"Purpose: Ask user IF they want a custom output transform
        ;"Input: FILENUMBER -- the name or number of the file to work with
        ;"        FIELD -- the number of the field to work with
        ;"        REFARRAY -- the array to put answer in.
        ;"              value passed will probably be like this:
        ;"              e.g. array(22704,"TEMPLATE") or
        ;"              e.g. array(22704,"TEMPLATE",2,"TEMPLATE")
        ;"       INDENT -- the indent value from left margin
        ;"Output: value is put in, IF user wants, like this
        ;"              e.g. array(22704,"TEMPLATE","TRANSFORM",.01)="Custom name"
        ;"              e.g. array(22704,"TEMPLATE",2,"TRANSFORM","TAG NAME",.01)="Custom name"
        ;"Result: 1 if OK to continue.  0 IF user ABORTed.
        NEW RESULT SET RESULT=1
        IF (+$GET(FILE)=0)!($GET(FIELD)="")!($GET(REFARRAY)="") SET RESULT=0 GOTO ACXDN
        SET INDENT=$GET(INDENT,0)
        ;
        NEW DEFXFORM
        NEW XFORM SET XFORM=""
        ;
        SET DEFXFORM=$GET(@REFARRAY@("TRANSFORM",FIELD))
        FOR  DO  QUIT:(XFORM'="")!(RESULT=0)
        . IF DEFXFORM'="" WRITE ?INDENT,DEFXFORM,!
        . WRITE ?INDENT,"Custom output transform for field? (?,^)  ^//"
        . READ XFORM:$GET(DTIME,3600),!
        . IF XFORM="" SET XFORM="^"
        . IF XFORM="^" SET RESULT=0 QUIT
        . IF XFORM="?" DO  QUIT
        . . WRITE !
        . . WRITE ?INDENT,"OPTION FOR ADVANCED USERS ONLY",!
        . . WRITE ?INDENT,"An output transform is custom Mumps code that converts",!
        . . WRITE ?INDENT,"internally stored database values into information readable",!
        . . WRITE ?INDENT,"by end users.  If you don't understand this, just leave this",!
        . . WRITE ?INDENT,"option blank (i.e., just hit [ENTER])",!
        . . WRITE ?INDENT,"The following variables will be SET up:",!
        . . WRITE ?INDENT,"  X -- the value stored in the database",!
        . . WRITE ?INDENT,"  IENS -- a standard Fileman IENS",!
        . . WRITE ?INDENT,"  FILENUM -- the number of the current file or subfile",!
        . . WRITE ?INDENT,"  FIELD -- the number of the current file",!
        . . WRITE ?INDENT,"The resulting value (that should be written to the XML",!
        . . WRITE ?INDENT,"file) should be put into Y",!!
        . . DO PAUSE(INDENT)
        . . SET XFORM=""
        . ;"Note I should run some check here for valid code.
        . SET @REFARRAY@("TRANSFORM",FIELD)=XFORM
        ;
ACXDN   QUIT RESULT
        ;
FMGETFLD(FILENUMBER,INDENT) ;
        ;"Purpose: To use Fileman to pick a field
        ;"Input: FILE -- Number of file to get field numbers for
        ;"Result -- The file number selected, or 0 IF none or ABORT
        NEW RESULT SET RESULT=0
        IF +$GET(FILENUMBER)'>0 GOTO FMGFDN
        NEW DIC SET DIC="^DD("_FILENUMBER_","
        SET DIC(0)="AEQ"
        SET DIC("A")=$$SPACES(.INDENT)_"Select field (? for list, ^ to abort): "
        DO ^DIC WRITE !
        IF +Y>0 SET RESULT=+Y
FMGFDN  QUIT RESULT
        ;
ASKGTFLD(FILENUMBER,INDENT)  ;"ASK GET FIELD
        ;"Purpose: To ask user for a field number, then verify it exists.
        ;"Input: FILE -- Number of file to get field numbers for
        ;"         INDENT -- OPTIONAL -- a number of spaces to indent.
        ;"Result -- The file number selected, or 0 IF none,  or -1 IF ABORT
        NEW RESULT SET RESULT=0
        NEW FLDNAME,FIELD
        SET INDENT=$GET(INDENT,0)
        IF +$GET(FILENUMBER)'>0 GOTO AGFDN
        ;
        WRITE ?INDENT
        READ "Enter field number or name: ",FIELD:$GET(DTIME,3600)
        IF FIELD="^" SET RESULT=-1 GOTO AGFDN
        IF +FIELD=0 DO  QUIT:(+FIELD=0)
        . SET FLDNAME=FIELD
        . SET FIELD=$$FLDNUM^DILFD(FILENUMBER,FIELD)  ;"Convert Field Name to Field Number
        . WRITE " (# ",FIELD,")",!
        ELSE  DO
        . SET FLDNAME=$$GTFLDNAM^TMGXMLT2(FILENUMBER,FIELD) ;"Convert Field Number to Field Name
        . WRITE " (",FLDNAME,")",!
        IF +FIELD>0 DO
        . NEW REF SET REF="^DD("_FILENUMBER_","_FIELD_",0)"
        . IF $DATA(@REF)'>0 DO
        . . WRITE ?INDENT,"Sorry. That field number doesn't exist.",!
        . . SET FIELD=0
        . ELSE  DO
        . . SET RESULT=FIELD
AGFDN   QUIT RESULT
        ;
PCKUSFLD(FILENUMBER,REFARRAY,INDENT)  ;"PICK UNSEL FIELD
        ;"Purpose: To allow the user to pick those fields not already selected.
        ;"Input: FILENUMBER -- the file number to work from
        ;"        REFARRAY -- a pointer to (i.e. name of) array to work from.  Format same as other functions in this module
        ;"         INDENT -- OPTIONAL -- a number of spaces to indent.
        ;"Result -- The file number selected, or 0 IF none, or -1 IF ABORT
        NEW RESULT SET RESULT=0
        NEW FLDNAME,FIELD,INDEX
        SET INDENT=$GET(INDENT,0)
        IF (+$GET(FILENUMBER)'>0)!($GET(REFARRAY)="") GOTO AGFDN
        ;
        ;"Get list of available fields.
        NEW ALLFIELDS
        NEW PICKARRAY
        NEW PICKCT SET PICKCT=0
        IF $$GTFLDLST^TMGXMLT2(FILENUMBER,"ALLFIELDS")=0 GOTO PUFDN
        SET FIELD=0
        FOR  DO  QUIT:(+FIELD'>0)
        . NEW FLDNAME
        . SET FIELD=$ORDER(ALLFIELDS(FIELD))
        . IF (+FIELD>0)&($DATA(@REFARRAY@(FIELD))=0) DO
        . . SET PICKCT=PICKCT+1
        . . SET PICKARRAY(PICKCT)=FIELD
        . . SET FLDNAME=$$GTFLDNAM^TMGXMLT2(FILENUMBER,FIELD) ;"Convert Field Number to Field Name
        . . WRITE ?INDENT,PICKCT,".  ",FLDNAME," (",FIELD,")",!
        . IF (PICKCT>0)&(((PICKCT\10)=(PICKCT/10))!(+FIELD'>0)) DO
        . . NEW INPUT
        . . WRITE !,?INDENT,"Select entry (NOT field number) (1-",PICKCT,",^), ",!
        . . WRITE ?INDENT,"or ENTER to continue: // "
        . . READ INPUT:$GET(DTIME,3600),!
        . . IF $TEST=0 SET INPUT="^"
        . . IF INPUT="^" SET FIELD=-1 QUIT
        . . IF (+INPUT>0)&(+INPUT<(PICKCT+1)) DO
        . . . SET RESULT=PICKARRAY(+INPUT)
        . . . SET FIELD=0 ;"signal Done
        ;
        IF PICKCT=0 WRITE ?INDENT,"(All fields have already been selected.)",!
PUFDN   QUIT RESULT
        ;
CFGORFLD(FILE,REFARRAY,INDENT)  ;"CONFIG ORDER FIELDS
        ;"Purpose: To allow customization of fields ORDER
        ;"Input: FILE -- name or number, file to get field numbers for
        ;"        REFARRAY -- a pointer to (i.e. Name of) array to put field numbers into
        ;"              will probably be something one of the following:
        ;"                      "ARRAY(FILENUMBER,"TEMPLATE")"
        ;"                      "ARRAY(FILENUMBER,RecNumber)"
        ;"        INDENT -- a value to indent from the left margin
        ;"Output: Data is put into REFARRAY
        ;"Result: 1 if OK to continue.  0 IF user ABORTed.
        NEW ERRFOUND
        NEW FILENUMBER,FILENAME
        NEW FIELD,COUNT,INDEX
        NEW INPUT
        NEW DONEARRAY SET DONEARRAY=""
        NEW RESULT SET RESULT=1
        IF ($GET(FILE)="")!($GET(REFARRAY)="") SET RESULT=0 GOTO COFDN
        ;
        IF +FILE=FILE DO
        . SET FILENUMBER=FILE
        . SET FILENAME=$$GETFNAME^TMGXMLT2(FILE)
        ELSE  DO
        . SET FILENAME=FILE
        . SET FILENUMBER=$$GETFNUM^TMGXMLT2(FILE)
        IF FILENUMBER'>0 DO  GOTO COFDN
        . DO SHOWERR^TMGDEBU2(.ERRFOUND,"Error: Requested file, "_FILE_", doesn't exist.")
        SET INDENT=+$GET(INDENT,0)
        ;
        IF $DATA(@REFARRAY)'>1 SET @REFARRAY@("*")=""
        ;"if $DATA(@REFARRAY@("*"))>0 DO  GOTO COFDN  ;"ORDER not allowed IF all records requested.
        ;". WRITE ?INDENT,"Note: skipping option for field ordering because ALL fields",!
        ;". WRITE ?INDENT,"were selected for export.",!
        ;". WRITE ?INDENT,"(This is a technical limitation of this routine.)",!!
        ;
COFL0   WRITE ?INDENT,"Do you wish to customize the ORDER that ",!
        WRITE ?INDENT,"fields will appear in the XML file? (Y/N,^) NO// "
        NEW INPUT READ INPUT:$GET(DTIME,3600),!
        IF $TEST=0 SET INPUT="^"
        IF INPUT="^" SET RESULT=0 GOTO COFDN
        IF INPUT="" SET INPUT="N"
        SET INPUT=$$UP^XLFSTR(INPUT)
        IF INPUT'["Y" GOTO COFDN
        IF INPUT="?" DO  GOTO COFL0
        . WRITE ?INDENT,"If you want to specify the order that the fields will be exported, enter YES.",!
        ;
COFL1   NEW MAXNUM SET MAXNUM=0
        SET INDEX=$ORDER(@REFARRAY@("ORDER",""))
        IF INDEX'="" FOR  DO  QUIT:(INDEX="")
        . NEW n SET n=@REFARRAY@("ORDER",INDEX)
        . IF INDEX>MAXNUM SET MAXNUM=INDEX
        . SET INDEX=$ORDER(@REFARRAY@("ORDER",INDEX))
        ;
        SET FIELD=$ORDER(@REFARRAY@(""))
        SET COUNT=0
        NEW COUNTARRAY
        IF FIELD'="" DO
        . WRITE ?INDENT,"Choose one of the following fields:",!
        IF FIELD'="" FOR  DO  QUIT:(+FIELD'>0)
        . IF $DATA(DONEARRAY(FIELD))=0 DO
        . . SET COUNT=COUNT+1
        . . SET COUNTARRAY(COUNT)=FIELD
        . . WRITE ?INDENT,COUNT,".  Field: ",FIELD
        . . IF +FIELD=FIELD DO
        . . . WRITE "  (",$$GTFLDNAM^TMGXMLT2(FILE,FIELD),")",!
        . . ELSE  WRITE !
        . SET FIELD=$ORDER(@REFARRAY@(FIELD))
        IF COUNT=0 DO  GOTO COFDN
        . WRITE ?INDENT,"All done specifying field order.",!!
        . DO PAUSE()
        ;
COFL2   IF COUNT>1 DO
        . WRITE ?INDENT,"Note: Don't enter actual field number.",!
        . WRITE ?INDENT,"Which field should come "
        . IF MAXNUM=0 WRITE "first."
        . ELSE  WRITE "next."
        . WRITE "?  (1-"_COUNT_",^ to abort) "
        . READ INPUT:$GET(DTIME,3600),!!
        . IF $TEST=0 SET INPUT="^"
        ELSE  DO
        . WRITE ?INDENT,"Only one option left, so I'll enter it for you...",!
        . SET INPUT=1
        IF ((INPUT<1)!(INPUT>COUNT))&(INPUT'="^") GOTO COFL2
        IF INPUT="^" DO  SET RESULT=0 GOTO COFDN
        . KILL @REFARRAY@("ORDER")
        . WRITE ?INDENT,"Because the process of specifying an order",!
        . WRITE ?INDENT,"for the fields wasn't completed, the partial ",!
        . WRITE ?INDENT,"order information was deleted.",!
        . DO PAUSE(INDENT)
        SET MAXNUM=MAXNUM+1
        NEW TEMPFIELD SET TEMPFIELD=$GET(COUNTARRAY(INPUT))
        SET @REFARRAY@("ORDER",MAXNUM)=TEMPFIELD
        SET DONEARRAY(TEMPFIELD)=""
        GOTO COFL1
        ;
COFDN   QUIT RESULT
        ;
SHOWARR(INDENT)  ;"SHOW ARRAY
        ;"Purpose: To show the array that composes the XML export request
        IF ($DATA(TMGXMLARR)>0)&($DATA(@TMGXMLARR)) DO
        . WRITE !
        . NEW IDX FOR IDX=1:1:INDENT SET INDENT(IDX)=0
        . DO ARRDUMP^TMGXMLIN(TMGXMLARR,,.INDENT)
        . WRITE !
        DO PAUSE(.INDENT)
        QUIT
        ;
PAUSE(INDENT) ;
        ;"Purpose: To prompt user to hit enter to continue
        ;"Input: INDENT -- OPTIONAL -- number of spaces to indent from left margin.
        ;"              Note: to call with no value for indent, use "do PAUSE()"
        NEW TEMP
        SET INDENT=$GET(INDENT,0)
        WRITE ?INDENT
        READ "Press [Enter] to continue...",TEMP:$GET(DTIME,3600),!
        QUIT
        ;
WRITEHDR(REFHEADER,NOLF)  ;"
        ;"Purpose: to put a header at the top of the screen
        ;"              The screen will be cleared
        ;"Note: because global variable IOF is used, the VistA environement must be setup first.
        ;"Input: REFHEADER -- expected format:
        ;"              REFHEADER(1)="First Line"
        ;"              REFHEADER(2)="Second Line"
        ;"              REFHEADER("MAX LINE")=2
        ;"        NOLF -- OPTIONAL IF =1, then extra LF suppressed
        ;"Result: none
        ;
        WRITE @IOF
        IF $GET(REFHEADER)="" GOTO WHDN
        NEW MAX SET MAX=+$GET(@REFHEADER@("MAX LINE"))
        IF MAX=0 GOTO WHDN
        FOR INDEX=1:1:MAX DO
        . IF $DATA(@REFHEADER@(INDEX))=0 QUIT
        . NEW LINE SET LINE=$GET(@REFHEADER@(INDEX))
        . IF (LINE["    Step") DO
        . . IF (INDEX<MAX) DO
        . . . SET LINE=$$REPLSTR^TMGSTUT3(LINE,"    Step","(X) Step")
        . . ELSE  DO
        . . . SET LINE=$$REPLSTR^TMGSTUT3(LINE,"    Step","(_) Step")
        . WRITE LINE,!
        ;
        IF $GET(NOLF)'=0 WRITE !
        ;
WHDN    QUIT
        ;
HDRADDL(REFHEADER,LINE)  ;"HEADER ADD LINE
        ;"Purpose: To add Line to end of header array
        ;"Input: REFHEADER -- expected format:  (it is OK to pass an empty array to be filled)
        ;"              REFHEADER(1)="First Line"
        ;"              REFHEADER(2)="Second Line"
        ;"              REFHEADER("MAX LINE")=2
        ;"        LINE -- a string to be added.
        ;"result: none
        ;
        IF $GET(REFHEADER)="" GOTO HALDN
        IF $GET(LINE)="" GOTO HALDN
        NEW MAX SET MAX=+$GET(@REFHEADER@("MAX LINE"))        
        SET MAX=MAX+1
        SET @REFHEADER@(MAX)=LINE
        SET @REFHEADER@("MAX LINE")=MAX
HALDN   QUIT
        ;
HDRDELLINE(REFHEADER,INDEX)   ;"HEADER DELETE LINE
        ;"Purpose: To delete a line from the header
        ;"Input: REFHEADER -- expected format:  (it is OK to pass an empty array to be filled)
        ;"              REFHEADER(1)="First Line"
        ;"              REFHEADER(2)="Second Line"
        ;"              REFHEADER("MAX LINE")=2
        ;"        INDEX -- OPTIONAL -- default is to be the last line

        IF $GET(REFHEADER)="" GOTO HDLDN
        NEW MAX SET MAX=+$GET(@REFHEADER@("MAX LINE"))
        IF MAX=0 GOTO HDLDN
        SET INDEX=$GET(INDEX,0)
        IF INDEX=0 SET INDEX=MAX
        KILL @REFHEADER@(INDEX)
        IF INDEX<MAX for INDEX=INDEX:1:(MAX-1) DO
        . SET @REFHEADER@(INDEX)=$GET(@REFHEADER@(INDEX+1))
        . KILL @REFHEADER@(INDEX+1)
        SET @REFHEADER@("MAX LINE")=MAX-1
HDLDN   QUIT
        ;
SPACES(NUM)  ;
        ;"purpose to return Num number of spaces
        NEW RESULT SET RESULT=""
        SET NUM=+$GET(NUM,0)
        IF NUM=0 GOTO SPCDN
        NEW IDX FOR IDX=1:1:NUM SET RESULT=RESULT_" "  
SPCDN   QUIT RESULT
        ;
 ;"===================================================
 ;
GETPTROT(FILE,ARRAY)  ;"GET POTENTIAL POINTERS OUT
        ;"Purpose: to return a list of all possible pointers out, for a given file
        ;"Input: FILE -- name or number of file to investigate
        ;"       ARRAY -- PASS BY REFERENCE.  Output format:
        ;"          ARRAY(FILENUM,"POINTERS OUT",FIELDNUM,OTHERFILENUM)=""
        ;"          ARRAY(FILENUM,"POINTERS OUT",FIELDNUM,OTHERFILENUM)=""
        ;"          ARRAY(FILENUM,"POINTERS OUT",FIELDNUM,OTHERFILENUM)=""
        ;"          ARRAY("SUBFILE",SUBFILENUM,PARENTFILENUMBER,PARENTFIELDNUM)=""
        ;"          e.g. ARRAY(142,"POINTERS OUT",.06,200)=""
        ;"          e.g. ARRAY(142.01,"POINTERS OUT",1,142.1)=""
        ;"          e.g. ARRAY(142.14,"VAR POINTERS OUT",.01,60)=""
        ;"          e.g. ARRAY(142.14,"VAR POINTERS OUT",.01,71)=""
        ;"          e.g. ARRAY(142.14,"VAR POINTERS OUT",.01,81)=""
        ;"          e.g. ARRAY(142.14,"VAR POINTERS OUT",.01,120.51)=""
        ;"          e.g. ARRAY(142.14,"VAR POINTERS OUT",.01,601.71)=""
        ;"          e.g. ARRAY(142.14,"VAR POINTERS OUT",.01,811.9)=""
        ;"          e.g. ARRAY(142.14,"VAR POINTERS OUT",.01,8925.1)=""
        ;"          e.g. ARRAY(142.14,"VAR POINTERS OUT",.01,9999999.64)=""
        ;"          e.g. ARRAY(142.2,"POINTERS OUT",.01,44)=""
        ;"          e.g. ARRAY(142.2,"POINTERS OUT",.02,3.5)=""
        ;"          e.g. ARRAY("SUBFILE",142.01,142,1)=""
        ;"          e.g. ARRAY("SUBFILE",142.14,142.01,4)=""
        ;"          e.g. ARRAY("SUBFILE",142.2,142,20)=""
        ;"Results: 1 if some found, 0 if no pointers out.
        ;"NOTE: This is currently not supporting variable pointers
        NEW FILENUM
        ;"KILL ARRAY
        NEW FOUND SET FOUND=0        
        IF +FILE=FILE SET FILENUM=FILE
        ELSE  SET FILENUM=$$GETFNUM^TMGXMLT2(FILE)
        ;
        NEW FIELD SET FIELD=0
        FOR  SET FIELD=$ORDER(^DD(FILENUM,FIELD)) QUIT:(FIELD'>0)  DO
        . NEW FLDINFO SET FLDINFO=$PIECE($GET(^DD(FILENUM,FIELD,0)),"^",2)
        . IF +FLDINFO>0 DO  QUIT
        . . NEW TEMP SET FOUND=FOUND!$$GETPTROT(+FLDINFO,.TEMP) MERGE ARRAY=TEMP
        . . SET ARRAY("SUBFILE",+FLDINFO,FILENUM,FIELD)=""
        . . DO GETPTROT(+FLDINFO,.ARRAY)
        . IF FLDINFO["P" DO
        . . NEW OTHERFILE SET OTHERFILE=+$PIECE(FLDINFO,"P",2)
        . . IF $$GETFNAME^TMGXMLT2(OTHERFILE)="" DO  QUIT
        . . . ;"something should go here?
        . . SET ARRAY(FILENUM,"POINTERS OUT",FIELD,OTHERFILE)=""
        . . SET FOUND=1
        . ELSE  IF FLDINFO["V" DO
        . . NEW IDX SET IDX=0 
        . . FOR  SET IDX=$ORDER(^DD(FILENUM,FIELD,"V",IDX)) QUIT:IDX'>0  DO
        . . . NEW ZN SET ZN=$GET(^DD(FILENUM,FIELD,"V",IDX,0)) QUIT:ZN=""
        . . . SET ARRAY(FILENUM,"VAR POINTERS OUT",FIELD,+ZN)=""
        . . SET FOUND=1
        QUIT FOUND
        ;
CSTPTROT(ARRAY,RECSARRAY)  ;"CUSTOMIZE TO ACTUAL POINTERS OUT
        ;"Purpose: Given an array of pointers out (as created by GETPTROT), look at the
        ;"      specific group of records (provided in RECSARRAY) and trim out theoretical
        ;"      pointers, and only leave actual pointers in the list.
        ;"Input: ARRAY -- PASS BY REFERENCE.  Format:
        ;"          ARRAY(FILENUM,"POINTERS OUT",FIELDNUM,OTHERFILENUM)=""
        ;"          ARRAY(FILENUM,"POINTERS OUT",FIELDNUM,OTHERFILENUM)=""
        ;"          ARRAY(FILENUM,"VAR POINTERS OUT",FIELDNUM,OTHERFILENUM)=""
        ;"          ARRAY(FILENUM,"VAR POINTERS OUT",FIELDNUM,OTHERFILENUM)=""
        ;"          ARRAY("SUBFILE",SUBFILENUM,PARENTFILENUMBER,PARENTFIELDNUM)=""
        ;"          ARRAY("SUBFILE",SUBFILENUM,PARENTFILENUMBER,PARENTFIELDNUM)=""
        ;"          Note: ARRAY may well have other information in it.
        ;"       RECSARRAY -- An IN AND OUT PARAMETER.  Format
        ;"          RECSARRAY(PARENTFILENUM,TOPIEN)=""
        ;"          RECSARRAY(PARENTFILENUM,TOPIEN)=""
        ;"          RECSARRAY(PARENTFILENUM,"*")=""  <-- Will be removed and expanded to all records
        ;"          RECSARRAY("SUBFILE",IENS,FLD)=PTR   <-- the output part
        ;"Output: ARRAY pointer will be trimmed such that every pointer listed exists
        ;"       in at least one of the records in RECSARRAY
        NEW FILENUM,FIELDNUM,IEN
        WRITE !
        SET FILENUM=""
        FOR  SET FILENUM=$ORDER(RECSARRAY(FILENUM)) QUIT:(+FILENUM'>0)  DO
        . IF $DATA(RECSARRAY(FILENUM,"*"))=0 QUIT
        . KILL RECSARRAY(FILENUM,"*")
        . NEW REF SET REF=$GET(^DIC(FILENUM,0,"GL")) QUIT:REF="" 
        . NEW CREF SET CREF=$$CREF^DILF(REF)
        . SET IEN=0
        . FOR  SET IEN=$ORDER(@CREF@(IEN)) QUIT:IEN'>0  SET RECSARRAY(FILENUM,IEN)=""        
        NEW COUNT SET COUNT=1 
        SET FILENUM=""
        FOR  SET FILENUM=$ORDER(ARRAY(FILENUM)) QUIT:(+FILENUM'>0)  DO
        . IF $DATA(ARRAY("SUBFILE",FILENUM)) QUIT  ;"will handle separately below.  
        . SET FIELDNUM=""
        . FOR  SET FIELDNUM=$ORDER(ARRAY(FILENUM,"POINTERS OUT",FIELDNUM)) QUIT:(+FIELDNUM'>0)  DO
        . . ;"Now, for given file:field, DO any records in RECSARRAY contain a value?
        . . NEW REF SET REF=$GET(^DIC(FILENUM,0,"GL"))  ;"record global REF string (open ended)
        . . NEW NODE SET NODE=$GET(^DD(FILENUM,FIELDNUM,0)) ;"NODE=entire 0 NODE
        . . NEW NP SET NP=$PIECE(NODE,"^",4)       ;"get NODE;piece
        . . NEW NODE SET NODE=$PIECE(NP,";",1)  
        . . NEW PCE SET PCE=$PIECE(NP,";",2)  
        . . SET IEN=""
        . . NEW FOUND SET FOUND=0
        . . FOR  SET IEN=$ORDER(RECSARRAY(FILENUM,IEN)) QUIT:(+IEN'>0)!(FOUND=1)  DO
        . . . SET COUNT=COUNT+1 IF COUNT#500=0 DO   
        . . . . DO PROGBAR^TMGUSRI2(IEN,"Pointers out for "_FILENUM_", "_IEN,-1,-1,80)
        . . . NEW TEMPREF SET TEMPREF=REF_IEN_","""_NODE_""")"
        . . . NEW LINE SET LINE=$GET(@TEMPREF)
        . . . NEW PTR SET PTR=+$PIECE(LINE,"^",PCE)  ;"get data from database
        . . . IF PTR>0 SET FOUND=1 QUIT  ;"found at least one record in group has an actual pointer
        . . IF FOUND=1 QUIT  ;"don't cut out the theoritical pointers (but no actual data)
        . . KILL ARRAY(FILENUM,"POINTERS OUT",FIELDNUM)
        ;"Check for subfile pointers here...
        SET COUNT=1
        NEW SUBFNUM SET SUBFNUM=""
        FOR  SET SUBFNUM=$ORDER(ARRAY(SUBFNUM)) QUIT:(+SUBFNUM'>0)  DO
        . SET COUNT=COUNT+1 IF COUNT#4=0 DO PROGBAR^TMGUSRI2(SUBFNUM,"Subfile "_SUBFNUM,-1,-1,80)
        . IF $DATA(ARRAY("SUBFILE",SUBFNUM))=0 QUIT  ;"handled above    
        . DO GETSFPTR(SUBFNUM,.ARRAY,.RECSARRAY) ;"GET SUBFILE POINTERS OUT
        . IF $DATA(RECSARRAY("SUBFILE",SUBFNUM))>0 DO
        . . NEW PARENTFNUM,TEMPFNUM,IENS SET TEMPFNUM=SUBFNUM,IENS="" 
        . . FOR  SET PARENTFNUM=+$GET(^DD(TEMPFNUM,0,"UP")) QUIT:PARENTFNUM'>0  DO
        . . . NEW FLD SET FLD=+$ORDER(ARRAY("SUBFILE",TEMPFNUM,PARENTFNUM,""))
        . . . NEW FLDINFO SET FLDINFO=$PIECE($GET(^DD(SUBFNUM,FLD,0)),"^",2)
        . . . NEW NODETYPE SET NODETYPE=$SELECT(FLDINFO["V":"VAR ",1:"")_"POINTERS OUT"
        . . . IF FLD>0 SET ARRAY(PARENTFNUM,NODETYPE,FLD,TEMPFNUM)=""
        . . . SET TEMPFNUM=PARENTFNUM
        . ELSE  DO
        . . KILL ARRAY(SUBFNUM),ARRAY("SUBFILE",SUBFNUM)
        WRITE !
        QUIT
        ;
GETSFPTR(SUBFNUM,ARRAY,RECSARRAY) ;"GET SUBFILE POINTERS OUT
        ;"Input: SUBFNUM -- SUBFILE NUMBER
        ;"       ARRAY -- PASS BY REFERENCE.  Potential pointers out.  Format:
        ;"          ARRAY(FILENUM,"POINTERS OUT",FIELDNUM,OTHERFILENUM)=""
        ;"          ARRAY(FILENUM,"POINTERS OUT",FIELDNUM,OTHERFILENUM)=""
        ;"          ARRAY(FILENUM,"VAR POINTERS OUT",FIELDNUM,OTHERFILENUM)=""
        ;"          ARRAY(FILENUM,"VAR POINTERS OUT",FIELDNUM,OTHERFILENUM)=""
        ;"          ARRAY("SUBFILE",SUBFILENUM,PARENTFILENUMBER,PARENTFIELDNUM)=""
        ;"          ARRAY("SUBFILE",SUBFILENUM,PARENTFILENUMBER,PARENTFIELDNUM)=""
        ;"          Note: ARRAY may well have other information in it.
        ;"       RECSARRAY -- An IN AND OUT PARAMETER.  Format
        ;"          RECSARRAY(PARENTFILENUM,TOPIEN)=""
        ;"          RECSARRAY(PARENTFILENUM,TOPIEN)=""
        ;"          RECSARRAY("SUBFILE",IENS,FLD)=PTR   <-- the output part
        ;"Result: none
        NEW TMGZZDEBUG SET TMGZZDEBUG=0
        IF TMGZZDEBUG=0 DO
        . KILL ^TMG("TMP","GETSFPTR^TMGXMLUI")
        . SET ^TMG("TMP","GETSFPTR^TMGXMLUI","SUBFNUM")=SUBFNUM
        . MERGE ^TMG("TMP","GETSFPTR^TMGXMLUI","ARRAY")=ARRAY
        . MERGE ^TMG("TMP","GETSFPTR^TMGXMLUI","RECSARRAY")=RECSARRAY
        ELSE  DO
        . KILL SUBFNUM,ARRAY,REVCSARRAY
        . SET SUBFNUM=^TMG("TMP","GETSFPTR^TMGXMLUI","SUBFNUM")
        . MERGE ARRAY=^TMG("TMP","GETSFPTR^TMGXMLUI","ARRAY")
        . MERGE RECSARRAY=^TMG("TMP","GETSFPTR^TMGXMLUI","RECSARRAY")
        IF $DATA(ARRAY("SUBFILE",SUBFNUM))=0 QUIT
        NEW TOPFNUM SET TOPFNUM=$$TOPFILE^TMGXMLT2(SUBFNUM)
        NEW REF,REFARR SET REF=$$SFREF^TMGXMLT2(SUBFNUM,.REFARR,.ARRAY)
        NEW TOPIEN SET TOPIEN=""
        FOR  SET TOPIEN=$ORDER(RECSARRAY(TOPFNUM,TOPIEN)) QUIT:(+TOPIEN'>0)  DO
        . DO GTSFPTR2(TOPIEN,.REFARR,,.ARRAY,.RECSARRAY) 
        QUIT
        ;
GTSFPTR2(IEN,TMGIEN,IDX,ARRAY,RECSARRAY,IENS)  ;" Recursive part of GETSFPTR above
        ;"PRIVATE
        ;"Input: IEN -- pass in top level IEN, or 0 for subfile
        ;"       TMGIEN -- PASS BY REFERENCE, AN OUT PARAMETER. Format:
        ;"          e.g. TMGIEN=2  <-- highest IEN index used
        ;"          e.g. TMGIEN("FILE")=142.1
        ;"          e.g. TMGIEN("REF",1)="^GMT(142,"
        ;"          e.g. TMGIEN("REF",2)="^GMT(142,TMGIEN(1),1,"
        ;"       IDX -- used in recursive calls, don't pass on first call
        ;"       ARRAY -- PASS BY REFERENCE.  Format:
        ;"          ARRAY(FILENUM,"POINTERS OUT",FIELDNUM,OTHERFILENUM)=""
        ;"          ARRAY(FILENUM,"POINTERS OUT",FIELDNUM,OTHERFILENUM)=""
        ;"          ARRAY(FILENUM,"POINTERS OUT",FIELDNUM,OTHERFILENUM)=""
        ;"          ARRAY(FILENUM,"VAR POINTERS OUT",FIELDNUM,OTHERFILENUM)=""  
        ;"          ARRAY(FILENUM,"VAR POINTERS OUT",FIELDNUM,OTHERFILENUM)=""
        ;"          ARRAY("SUBFILE",SUBFILENUM,PARENTFILENUMBER,PARENTFIELDNUM)=""
        ;"       RECSARRAY -- An IN AND OUT PARAMETER.  Format
        ;"          RECSARRAY(PARENTFILENUM,TOPIEN)=""
        ;"          RECSARRAY(PARENTFILENUM,TOPIEN)=""
        ;"          RECSARRAY("SUBFILE",IENS,FLD)=PTR   <-- the output part
        ;"Result: none  
        SET IDX=+$GET(IDX,1)
        SET IEN=+$GET(IEN)
        SET IENS=$GET(IENS) NEW SUBIENS
        NEW REF SET REF=$GET(TMGIEN("REF",IDX)) IF REF="" QUIT
        NEW FILENUM SET FILENUM=$GET(TMGIEN("FILE"))
        IF IEN>0 DO  ;"TOP LEVEL
        . NEW CREF SET CREF=$$CREF^DILF(REF_IEN_",")
        . IF $DATA(@CREF)=0 QUIT
        . SET SUBIENS=IEN_","_IENS
        . SET TMGIEN(1)=IEN
        . DO GTSFPTR2(0,.TMGIEN,IDX+1,.ARRAY,.RECSARRAY,SUBIENS)
        ELSE  DO
        . NEW CREF SET CREF=$$CREF^DILF(REF)
        . NEW FILENUM SET FILENUM=$GET(TMGIEN("FILE"))
        . NEW SUBIEN SET SUBIEN=0
        . FOR  SET SUBIEN=$ORDER(@CREF@(SUBIEN)) QUIT:(SUBIEN'>0)  DO
        . . SET SUBIENS=SUBIEN_","_IENS
        . . IF IDX=TMGIEN DO
        . . . NEW TYPENODE,FLD SET FLD=0
        . . . FOR TYPENODE="POINTERS OUT","VAR POINTERS OUT" DO  
        . . . . FOR  SET FLD=$ORDER(ARRAY(FILENUM,TYPENODE,FLD)) QUIT:(FLD'>0)  DO
        . . . . . NEW NODE SET NODE=$GET(^DD(FILENUM,FLD,0)) ;"NODE=entire 0 NODE
        . . . . . NEW NP SET NP=$PIECE(NODE,"^",4)       ;"get NODE;piece
        . . . . . NEW NODE SET NODE=$PIECE(NP,";",1)  
        . . . . . NEW PCE SET PCE=$PIECE(NP,";",2)
        . . . . . NEW TEMP SET TEMP=$GET(@CREF@(SUBIEN,NODE))
        . . . . . NEW PTR SET PTR=$PIECE(TEMP,"^",PCE)
        . . . . . IF PTR'>0 QUIT
        . . . . . SET RECSARRAY("SUBFILE",FILENUM,SUBIENS,FLD)=PTR
        . . ELSE  DO
        . . . SET TMGIEN(IDX)=SUBIEN
        . . . DO GTSFPTR2(0,.TMGIEN,IDX+1,.ARRAY,.RECSARRAY,SUBIENS)
        QUIT 
        ;
TRIMPTRO(ARRAY)  ;"TRIM PTR OUT
        ;"Purpose: Given array of pointers out (as created by GETPTROT, or CustPtrsOut), ask which
        ;"         other files should be ignored.
        ;"Input: ARRAY. PASS BY REFERENCE.  Format:
        ;"          ARRAY(FILENUM,"POINTERS OUT",FIELDNUM,OTHERFILENUM)=""
        ;"          ARRAY(FILENUM,"VAR POINTERS OUT",FIELDNUM,OTHERFILENUM)=""
        ;"Output: for those pointers out that can be ignored, entries will be CHANGED:
        ;"          ARRAY(FILENUM,"POINTERS OUT",FIELDNUM,OTHERFILENUM)="-" <-- Ignore flag
        ;"          ARRAY(FILENUM,"POINTERS OUT",FIELDNUM,OTHERFILENUM)="+" <-- Confirmed flag
        ;"          ARRAY(FILENUM,"VAR POINTERS OUT",FIELDNUM,OTHERFILENUM)="-" <-- Ignore flag
        ;"          ARRAY(FILENUM,"VAR POINTERS OUT",FIELDNUM,OTHERFILENUM)="+" <-- Confirmed flag
        ;
        ;"first, make a temp array that groups pointers out.
        NEW ARRAY2
        NEW FILENUM SET FILENUM=0
        FOR  SET FILENUM=$ORDER(ARRAY(FILENUM)) QUIT:(+FILENUM'>0)  DO
        . NEW FIELDNUM SET FIELDNUM=0
        . NEW REF
        . NEW TYPENODE FOR TYPENODE="POINTERS OUT","VAR POINTERS OUT" DO  
        . . FOR  SET FIELDNUM=$ORDER(ARRAY(FILENUM,TYPENODE,FIELDNUM)) QUIT:(+FIELDNUM'>0)  DO
        . . . NEW OTHERFILENUM SET OTHERFILENUM=""
        . . . FOR  SET OTHERFILENUM=$ORDER(ARRAY(FILENUM,TYPENODE,FIELDNUM,OTHERFILENUM)) QUIT:OTHERFILENUM'>0  DO
        . . . . NEW REF SET REF=$NAME(ARRAY(FILENUM,TYPENODE,FIELDNUM,OTHERFILENUM))
        . . . . NEW IEN SET IEN=$ORDER(^TMG(22707.5,"B",OTHERFILENUM,""))
        . . . . IF (IEN'=""),$PIECE($GET(^TMG(22707.5,IEN,0)),"^",2)=0 DO  QUIT
        . . . . . SET ARRAY(FILENUM,TYPENODE,FIELDNUM,OTHERFILENUM)="-"
        . . . . IF (IEN'=""),$PIECE($GET(^TMG(22707.5,IEN,0)),"^",2)=1 DO  QUIT
        . . . . . SET ARRAY(FILENUM,TYPENODE,FIELDNUM,OTHERFILENUM)="+"
        . . . . SET ARRAY2(OTHERFILENUM,REF)=""
        ;
        NEW MENU,COUNT
        NEW USRINPUT,IEN
        NEW TMGFDA,TMGMSG,TMGIEN
        NEW REF,%,OTHERFILENUM
        NEW OTHERFILENUM
        IF $DATA(ARRAY2)=0 GOTO TPODN
        ;
        SET MENU(0)="Pick Which Pointers are NOT to User Data"
        SET COUNT=0
        SET OTHERFILENUM=0
        FOR  SET OTHERFILENUM=$ORDER(ARRAY2(OTHERFILENUM)) QUIT:(OTHERFILENUM="")  DO
        . IF $DATA(ARRAY("SUBFILE",OTHERFILENUM)) QUIT  
        . SET COUNT=COUNT+1
        . SET MENU(COUNT)=$$GETFNAME^TMGXMLT2(OTHERFILENUM)_$CHAR(9)_OTHERFILENUM_"^"_COUNT
        IF COUNT=0 GOTO TPOQ
        ;
TPO     SET USRINPUT=$$MENU^TMGUSRI2(.MENU)
        IF "x^"[USRINPUT GOTO TPODN
        IF USRINPUT["?" DO  GOTO TPO
        . WRITE "Explore which entry above? //"
        . NEW TEMP READ TEMP:$GET(DTIME,3600),!
        . SET TEMP=$PIECE($GET(MENU(TEMP)),$CHAR(9),2)
        . SET TEMP=$PIECE(TEMP,"^",1)
        . IF TEMP="" QUIT
        . NEW DIC,X,Y
        . SET DIC(0)="MAEQ"
        . SET DIC=+TEMP
        . WRITE "Here you can use Fileman to look at entries in file #",TEMP
        . DO ^DIC WRITE !
        SET REF=""
        SET COUNT=$PIECE(USRINPUT,"^",2)
        SET USRINPUT=$PIECE(USRINPUT,"^",1)
        FOR  SET REF=$ORDER(ARRAY2(USRINPUT,REF)) QUIT:(REF="")  DO
        . SET @REF="-"
        . KILL MENU(COUNT)
        . SET OTHERFILENUM=+$PIECE(REF,",",4)
        SET %=1
        SET IEN=$ORDER(^TMG(22707.5,"B",OTHERFILENUM,""))
        IF (IEN'=""),$PIECE($GET(^TMG(22707.5,IEN,0)),"^",2)=0 GOTO TPO
        WRITE "Remember that file '",$$GETFNAME^TMGXMLT2(OTHERFILENUM),"' DOESN'T contain ",!
        WRITE "  site-specific data (stored in File #22707.5)"
        DO YN^DICN WRITE !
        IF %'=1 GOTO TPO
        KILL TMGMSG,TMGFDA,TMGIEN
        IF +IEN>0 DO
        . SET TMGFDA(22707.5,IEN_",",.02)=0
        . DO FILE^DIE("","TMGFDA","TMGMSG")
        ELSE  DO
        . SET TMGFDA(22707.5,"+1,",.01)=OTHERFILENUM
        . SET TMGFDA(22707.5,"+1,",.02)=0
        . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
        DO SHOWDIER^TMGDEBU2(.TMGMSG)
        GOTO TPO
TPODN   IF $DATA(MENU)=0 GOTO TPOQ
        IF $ORDER(MENU(0))="" GOTO TPOQ
        NEW ENTRY SET ENTRY=0
        FOR  SET ENTRY=$ORDER(MENU(ENTRY)) QUIT:(ENTRY="")  DO
        . WRITE " -- ",$PIECE(MENU(ENTRY),$CHAR(9),1),!
        WRITE "Perminantly mark these files as CONTAINING site specific data"
        SET %=1
        DO YN^DICN WRITE !
        IF %=1 DO
        . SET ENTRY=0
        . FOR  SET ENTRY=$ORDER(MENU(ENTRY)) QUIT:(ENTRY="")  DO
        . . SET USRINPUT=$PIECE(MENU(ENTRY),$CHAR(9),2)
        . . SET OTHERFILENUM=$PIECE(USRINPUT,"^",1)
        . . SET REF=""
        . . FOR  SET REF=$ORDER(ARRAY2(OTHERFILENUM,REF)) QUIT:(REF="")  DO
        . . . SET @REF="+"
        . . SET IEN=$ORDER(^TMG(22707.5,"B",OTHERFILENUM,""))
        . . IF (IEN'=""),$PIECE($GET(^TMG(22707.5,IEN,0)),"^",2)=1 QUIT
        . . IF +IEN>0 DO
        . . . SET TMGFDA(22707.5,IEN_",",.02)=1
        . . . DO FILE^DIE("","TMGFDA","TMGMSG")
        . . ELSE  DO
        . . . KILL TMGIEN
        . . . SET TMGFDA(22707.5,"+1,",.01)=OTHERFILENUM
        . . . SET TMGFDA(22707.5,"+1,",.02)=1
        . . . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
        . . DO SHOWDIER^TMGDEBU2(.TMGMSG)
        ;
TPOQ    QUIT
        ;
GTRECSOT(RECSARRAY,PTRSARRAY,ARRAY)  ;"GET RECS OUT
        ;"Purpose: For a given SET of records in a file, determine the linked-to record #'s
        ;"         in other files through pointers out.  This will return the actual IEN's
        ;"         in other files that are being pointed to.
        ;"Input -- RECSARRAY. PASS BY REFERENCE.  Format:
        ;"              RECSARRAY(FILENUM,IENinFILE)=""
        ;"              RECSARRAY(FILENUM,IENinFILE)=""
        ;"              RECSARRAY(FILENUM,IENinFILE)=""
        ;"              RECSARRAY("SUBFILE",IENS,FLD)=PTR  
        ;"              Note: ARRAY may well have other information in it.
        ;"         PTRSARRAY.  PASS BY REFERENCE.  Format:
        ;"              PTRSARRAY(FILENUM,"POINTERS OUT",FIELDNUM,OTHERFILENUM)=""
        ;"              PTRSARRAY(FILENUM,"POINTERS OUT",FIELDNUM,OTHERFILENUM)="-" <-- flag to ignore
        ;"              PTRSARRAY(FILENUM,"VAR POINTERS OUT",FIELDNUM,OTHERFILENUM)=""
        ;"              PTRSARRAY(FILENUM,"VAR POINTERS OUT",FIELDNUM,OTHERFILENUM)="-" <-- flag to ignore
        ;"              PTRSARRAY("SUBFILE",SUBFILENUM,PARENTFILENUMBER,PARENTFIELDNUM)=""
        ;"         ARRAY. PASS BY REFERENCE.  An OUT PARAMETER.  Format:
        ;"              ARRAY(FILENUM,IEN,FIELDNUM,"LINKED TO",OTHERFILENUM,OtherIEN)=""
        ;"              ARRAY(FILENUM,IEN,FIELDNUM,"LINKED TO",OTHERFILENUM,OtherIEN)=""
        ;"              ARRAY(FILENUM,IEN,FIELDNUM,"LINKED TO",OTHERFILENUM,OtherIEN)=""
        ;"              ARRAY("X1",OTHERFILENUM,OtherIEN)=""
        ;"              ARRAY("X1",OTHERFILENUM,OtherIEN)=""
        ;"Output: ARRAY is filled as above.
        ;"NOTE: This has not yet been tested with VARIABLE POINTERS
        ;"Results: None
        NEW P2FARR
        NEW COUNT SET COUNT=1
        WRITE !
        NEW FILENUM SET FILENUM=0
        FOR  SET FILENUM=$ORDER(PTRSARRAY(FILENUM)) QUIT:(+FILENUM'>0)  DO
        . NEW RECSREF FOR RECSREF="RECSARRAY","RECSARRAY(""SUBFILE"")" DO
        . . NEW IEN SET IEN=0  ;"IEN WILL BE IENS FOR SUBFILES
        . . FOR  SET IEN=$ORDER(@RECSREF@(FILENUM,IEN)) QUIT:(+IEN'>0)  DO
        . . . NEW NODETYPE FOR NODETYPE="POINTERS OUT","VAR POINTERS OUT" DO        
        . . . . NEW FIELDNUM SET FIELDNUM=0
        . . . . FOR  SET FIELDNUM=$ORDER(PTRSARRAY(FILENUM,NODETYPE,FIELDNUM)) QUIT:(+FIELDNUM'>0)  DO
        . . . . . SET COUNT=COUNT+1 IF COUNT#300=0 DO PROGBAR^TMGUSRI2(COUNT,FILENUM_", "_NODETYPE_", "_FIELDNUM,-1,-1,80)          
        . . . . . NEW OTHERFILENUM SET OTHERFILENUM=$ORDER(PTRSARRAY(FILENUM,NODETYPE,FIELDNUM,"")) 
        . . . . . QUIT:OTHERFILENUM'>0
        . . . . . NEW FLAG SET FLAG=$GET(PTRSARRAY(FILENUM,NODETYPE,FIELDNUM,OTHERFILENUM))
        . . . . . IF FLAG="-" QUIT
        . . . . . NEW IENS SET IENS=IEN_$SELECT($EXTRACT(IEN,$LENGTH(IEN))=",":"",1:",")
        . . . . . NEW OFN SET OFN=OTHERFILENUM
        . . . . . IF $DATA(ARRAY(FILENUM,IENS,FIELDNUM)) QUIT
        . . . . . NEW OTHERIEN SET OTHERIEN=$$GET1^DIQ(FILENUM,IENS,FIELDNUM,"I")
        . . . . . IF +OTHERIEN'>0 QUIT
        . . . . . IF OTHERIEN[";" DO
        . . . . . . NEW FREF SET FREF="^"_$PIECE(OTHERIEN,";",2),OTHERIEN=+OTHERIEN
        . . . . . . SET OFN=+$GET(P2FARR(FREF)) QUIT:OFN>0
        . . . . . . NEW IDX SET IDX=0
        . . . . . . FOR  SET IDX=$ORDER(^DD(FILENUM,FIELDNUM,"V",IDX)) QUIT:(IDX'>0)!(OFN>0)  DO
        . . . . . . . NEW TEMPFNUM SET TEMPFNUM=+$GET(^DD(FILENUM,FIELDNUM,"V",IDX,0))
        . . . . . . . IF $GET(^DIC(TEMPFNUM,0,"GL"))'=FREF QUIT
        . . . . . . . SET OFN=TEMPFNUM
        . . . . . . . SET P2FARR(FREF)=OFN
        . . . . . SET ARRAY(FILENUM,IENS,FIELDNUM,"LINKED TO",OFN,OTHERIEN)=""
        . . . . . IF $DATA(@RECSREF@(OFN,OTHERIEN))=0 DO
        . . . . . . SET ARRAY("X1",OFN,OTHERIEN)="tag=POINTED_TO_RECORD"
        QUIT
        ;
XPNDPTRS(REFRECSARRAY)  ;"EXPAND POINTERS
        ;"Purpose: To take selected record SET and include records from other files that
        ;"      the selected records point to.  NOTE: only records in files that marked 
        ;"      as holding site-specific data will be added
        ;"
        NEW CHANGED
        NEW RECSARRAY
        NEW PTRSARRAY,ARRAY
        MERGE RECSARRAY=@REFRECSARRAY
T1      SET CHANGED=0
        SET FILENUM=0
        FOR  SET FILENUM=$ORDER(RECSARRAY(FILENUM)) QUIT:(FILENUM'>0)  DO
        . IF $DATA(RECSARRAY("SUBFILE",FILENUM)) QUIT        
        . IF $$GETPTROT(FILENUM,.PTRSARRAY)=0 GOTO XPNDDN
        . DO CSTPTROT(.PTRSARRAY,.RECSARRAY)
        . DO TRIMPTRO(.PTRSARRAY)
        . DO GTRECSOT(.RECSARRAY,.PTRSARRAY,.ARRAY)
        . IF $DATA(ARRAY("X1")) DO
        . . MERGE RECSARRAY=ARRAY("X1")
        . . SET CHANGED=1
        . . KILL ARRAY("X1")
        IF CHANGED=1 GOTO T1
XPNDDN  MERGE @REFRECSARRAY=RECSARRAY
        QUIT
        ;
TEST    ;
        NEW RECS,FILENUM
        ;
        IF $DATA(^TMG("TMP","KILLTHIS"))=0 DO
        . IF $$UI^TMGXMLUI("RECSARRAY")=0 QUIT
        . MERGE ^TMG("TMP","KILLTHIS")=RECS
        ELSE  DO
        . MERGE RECS=^TMG("TMP","KILLTHIS")
        ;
        DO XPNDPTRS("RECS")
        ;
        QUIT
        ;
