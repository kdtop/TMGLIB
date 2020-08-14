TMGXMLIN ;TMG/kst/XML Importer ;10/26/14
         ;;1.0;TMG-LIB;**1**;02/09/08
 ;
 ;"TMG XML IMPORT FUNCTION
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
 ;"IMPORTXML -- Import file records via XML file
 ;"ARRDUMP(ARRAYP,TMGIDX,INDENT,FLAGS)
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;
        NEW USERPATH,USERFNAME,RESULT
        NEW XMLHANDLE SET XMLHANDLE=0
        SET XMLHANDLE=$ORDER(^TMP("MXMLDOM",$J,""))
        IF XMLHANDLE>0 GOTO IXL1
        ;
        NEW TEMPARR
        NEW TREF
        SET TREF=$NAME(^TMG("TMP","KILLTHIS","MXMLDOM",777))
        IF $DATA(@TREF) DO  GOTO IXL1
        . MERGE ^TMP("MXMLDOM",$J,777)=@TREF
        . SET XMLHANDLE=777
        ;
        SET RESULT=$$GETFNAME^TMGIOUTL("Select XML Import File","/",,,.USERPATH,.USERFNAME)
        IF RESULT="" GOTO IXDN
        ;
        SET XMLHANDLE=$$LOADFILE^TMGXMLT(.USERPATH,.USERFNAME)
        IF XMLHANDLE'>0 GOTO IXDN
        ;
        KILL @TREF MERGE @TREF=^TMP("MXMLDOM",$J,XMLHANDLE)
IXL1    DO IMPORTFS(XMLHANDLE)
IXDN    IF XMLHANDLE>0 DO
        . NEW % SET %=2
        . WRITE "Delete current XML import (may reload next time)"
        . DO YN^DICN WRITE !
        . IF %'=1 QUIT
        . DO DELETE^MXMLDOM(XMLHANDLE)
        QUIT
        ;
GTDDNODE(XMLHANDLE) ;"GET DD NODE
        ;"Purpose: Get the Data Dictionary Node (stored under FILE node)
        ;"Input: XMLHANDLE -- The handle created by loading function.
        ;"Results: 0 IF node not found, otherwise node number
        NEW RESULT SET RESULT=$$GDESCNOD^TMGXMLT(XMLHANDLE,1,"DataDictionary")
        QUIT RESULT
        ;
GTSYSNAM(XMLHANDLE)  ;"GET SYS NAME
        ;"Purpose: Get label of the VistA system that exported the data
        ;"      This means that this will only work with data exported by
        ;"      TMGXMLEX code module.
        ;"Input: XMLHANDLE -- The handle created by loading function.
        ;"Results: Returns system name, or "" IF not found
        ;"Note: Expects node 1 to be <EXPORT source="MyName">
        NEW RESULT SET RESULT=$$GTATRVAL^TMGXMLT(XMLHANDLE,1,"source")
        QUIT RESULT
        ;
IMPORTFS(XMLHANDLE) ;"IMPORT FILES
        ;"Purpose: to import data stored in XML file into local database
        ;"Input: XMLHANDLE -- The handle created by loading function.
        ;"results: none
        NEW SRCSYSNAME SET SRCSYSNAME=$$GTSYSNAM(XMLHANDLE)
        IF SRCSYSNAME="" GOTO IFDN
        ;"Later put guard to ensure not re-importing to self.
        NEW ABORT SET ABORT=0
        NEW NODEFILE SET NODEFILE=0
        FOR  SET NODEFILE=$$GDESCNOD^TMGXMLT(XMLHANDLE,1,"FILE",NODEFILE) QUIT:(NODEFILE'>0)!ABORT  DO
        . SET ABORT=$$IMPORT1F(XMLHANDLE,SRCSYSNAME,NODEFILE)
IFDN    QUIT
        ;
IMPORT1F(XMLHANDLE,SRCSYSNAME,NODEFILE)  ;"IMPORT 1 FILE
        ;"Purpose: to Import 1 file from XML data.
        ;"Input: XMLHANDLE -- The handle created by loading function.
        ;"       SRCSYSNAME -- The name of the source VistA system
        ;"       PARENTNODE -- the node containing the <FILE starting data for file
        ;"Results: 0=OK to continue, 1=ABORT
        NEW ABORT SET ABORT=0
        NEW FILENUM SET FILENUM=+$$GTATRVAL^TMGXMLT(XMLHANDLE,NODEFILE,"id")
        IF FILENUM'>0 DO  GOTO IMP1DN
        . SET ABORT=1
        . WRITE "Unable to import FILE because no numeric file number in attrib id='xx'",!
        ;"Later change this so that all the DD's are checked before calling IMPORT1F
        NEW TEMP SET TEMP=$$COMPATFL(XMLHANDLE,SRCSYSNAME,NODEFILE)
        IF TEMP'>0 DO  GOTO IMP1DN
        . SET ABORT=1
        . IF TEMP=-1 QUIT
        . WRITE "Unable to import FILE #",FILENUM," because data dictionaries are incompatible.",!
        ;"new NODERECORD SET NODERECORD=$$CHILD^MXMLDOM(XMLHANDLE,NODEFILE)
        NEW NODERECORD SET NODERECORD=0
        FOR  SET NODERECORD=$$GDESCNOD^TMGXMLT(XMLHANDLE,NODEFILE,"Record",NODERECORD) QUIT:(NODERECORD'>0)!ABORT  DO
        . SET ABORT=$$IMXL1REC(XMLHANDLE,SRCSYSNAME,FILENUM,NODERECORD)
IMP1DN  QUIT ABORT
        ;        
COMPATFL(XMLHANDLE,SRCSYSNAME,NODEFILE)  ;"COMPATIBLE FILE
        ;"Purpose: to determine IF the data dictionary (i.e. File Definition) is
        ;"        compatible between the Src VistA system, and this installation.
        ;"        E.g. Does field #1 mean the same thing on both systems?
        ;"Note, a table will be maintained to store the compatibility data. (The process
        ;"      of comparing the data dictionaries is slow).
        ;"      Format:
        ;"      ^TMG("XML EXPORTER",SRCSYSNAME,"DD",FILENUM,"DATE-TIME")=Time_(H$)_of_last_comparison
        ;"      ^TMG("XML EXPORTER",SRCSYSNAME,"DD",FILENUM,"COMPATIBLE")=1 (0=NOT compat, -1=ABORTed)
        ;"      ^TMG("XML EXPORTER",SRCSYSNAME,"DD",FILENUM,"WORKING","^DD","IMPORT-EXTRA")=...
        ;"      ^TMG("XML EXPORTER",SRCSYSNAME,"DD",FILENUM,"WORKING","^DD","DIFFERENCE")=...
        ;"      ^TMG("XML EXPORTER",SRCSYSNAME,"DD",FILENUM,"WORKING","^DD","TEMP-ARRAY")=...
        ;"      ^TMG("XML EXPORTER",SRCSYSNAME,"DD",FILENUM,"WORKING","^DIC","IMPORT-EXTRA")=...
        ;"      ^TMG("XML EXPORTER",SRCSYSNAME,"DD",FILENUM,"WORKING","^DIC","DIFFERENCE")=...
        ;"      ^TMG("XML EXPORTER",SRCSYSNAME,"DD",FILENUM,"WORKING","^DIC","TEMP-ARRAY")=...
        ;"      ^TMG("XML EXPORTER",SRCSYSNAME,"DD",FILENUM,FieldNum,... exceptions information
        ;"
        ;"Note: If a prior comparision has not be made, then it will be done here, and
        ;"      user will be asked IF they want to add any missing field/files definitions.
        ;"      Also, the user will be asked to review any difference between the to
        ;"      DD's to see IF the changes are minor (allowable), or major (not compatible)
        ;"
        ;"**I would like to have some way of NOT allowing one single difference between
        ;"      DD's abort the entire process, especially when we don't know IF that
        ;"      field will even be used during the upload process. (Perhaps the upload
        ;"      data won't have any instances of that field.) Perhaps I could just store
        ;"      the difference here, and then handle only when an example of data being
        ;"      uploaded for that field arises.  Pro's: user could have example of real
        ;"      data to see IF it is appropriate to be filed.  Con's: during a long process
        ;"      (such as importing might be), it would be annoying to have sit and wait for
        ;"      possible user queries.  Better to get that all setteled before starting
        ;"      actual import.  Perhaps ask user up front, but allow a "SKIP FOR NOW"
        ;"      option.  If so, then only asked when actual data arises.
        ;"
        ;"Result: 1=is compatable, or 0 IF not, -1=ABORT
        NEW RESULT SET RESULT=0 ;"default to not compatable.
        NEW FILENUM SET FILENUM=+$$GTATRVAL^TMGXMLT(XMLHANDLE,NODEFILE,"id")
        NEW PINFOREF SET PINFOREF=$NAME(^TMG("XML EXPORTER",SRCSYSNAME,"DD",FILENUM))
        NEW PROGRESSFN,INCVAR
        NEW ERRMSG
        NEW TIMELASTCHECK SET TIMELASTCHECK=+$GET(@PINFOREF@("DATE-TIME"))
        ;"Later check how much time has elapsed since last check and ask user IF recheck
        ;"      is needed...
        SET RESULT=$GET(@PINFOREF@("COMPATIBLE"))
        IF RESULT=1 GOTO CPDN
        IF RESULT=0 DO  GOTO:(RESULT'="") CPDN
        . NEW % SET %=1
        . WRITE "Data dictionary etc. has previously been found to be incompatible.",!
        . WRITE "Recheck again" DO YN^DICN WRITE !
        . IF %=-1 SET RESULT=-1 QUIT
        . IF %=1 SET RESULT="" QUIT
        DO HANDLEDD(XMLHANDLE,NODEFILE,PINFOREF,.ERRMSG)
        IF $DATA(ERRMSG) GOTO CPSTORE
        DO HANDLDIC(XMLHANDLE,NODEFILE,PINFOREF,.ERRMSG)
        IF $DATA(ERRMSG) GOTO CPSTORE
        ;
        ;"==============================================================
        ;"Compare FileHeader. -------------------------------
        ;"==============================================================
        NEW HDRNODE SET HDRNODE=$$GDESCNOD^TMGXMLT(XMLHANDLE,NODEFILE,"FILE_HEADER")
        IF HDRNODE=0 DO  GOTO CPSTORE
        . SET ERRMSG(1)="Unable to check compatibility of File header for file "_FILENUM
        . SET ERRMSG(2)="  because a FILE_HEADER node could not be found as a child node"
        . SET ERRMSG(3)="  from node "_NODEFILE_".  Aborting."
        NEW SRCHEADER SET SRCHEADER=$$GETJNTXT^TMGXMLT(XMLHANDLE,HDRNODE)
        SET SRCHEADER=$$Trim^TMGSTUTL(SRCHEADER)
        IF SRCHEADER="" DO  GOTO CPSTORE
        . SET ERRMSG(1)="Can't find a source Header entry."
        NEW GL SET GL=$GET(^DIC(FILENUM,0,"GL"))
        IF GL="" DO  GOTO CPSTORE
        . SET ERRMSG(1)="Unable to find global file reference in ^DIC for file "_FILENUM
        SET GL=GL_"0)"
        NEW LCLHEADER SET LCLHEADER=$GET(@GL)
        ;
        IF $PIECE(SRCHEADER,"^",1,2)'=$PIECE(LCLHEADER,"^",1,2) DO  GOTO:(RESULT=-1) CPSTORE
        . SET RESULT=1
        . WRITE "There appears to be a difference in the file headers:",!
        . WRITE "SOURCE VISTA SYSTEM",!
        . WRITE "    "_$PIECE(SRCHEADER,"^",1,2)_"^...",!,!
        . WRITE "TARGET (LOCAL) VISTA SYSTEM",!
        . WRITE "    "_$PIECE(LCLHEADER,"^",1,2)_"^...",!
        . NEW % SET %=1
        . WRITE "Abort import" DO YN^DICN WRITE !
        . IF %'=2 SET RESULT=-1
        ;
        ;"SUCCESS IF WE GOT THIS FAR....
        SET RESULT=1  ;"SUCCESS
        ;
CPSTORE IF $DATA(ERRMSG) DO
        . WRITE "ERROR.  Message:",!
        . NEW IDX SET IDX=""
        . FOR  SET IDX=$ORDER(ERRMSG(IDX)) QUIT:(IDX="")  WRITE ERRMSG(IDX),!
        . DO PRESS2GO^TMGUSRI2
        . SET RESULT=-1
        SET @PINFOREF@("COMPATIBLE")=RESULT
        SET @PINFOREF@("DATE-TIME")=$H
        IF RESULT=1 DO
        . KILL @PINFOREF@("WORKING")  ;"no longer needed.
        ;
CPDN    QUIT RESULT
        ;
HANDLEDD(XMLHANDLE,NODEFILE,PINFOREF,ERRMSG)  ;
        ;"==============================================================
        ;"Handle ^DD -----------------------------
        ;"==============================================================
        NEW TEMPARR,EXTRAB,MISSINGB,DIFFARRAY
        NEW TEMPSIZE SET TEMPSIZE=100000
        NEW REFEXTRAB SET REFEXTRAB="EXTRAB"
        NEW REFDIFFARR SET REFDIFFARR="DIFFARRAY"
        NEW PDDREF SET PDDREF=$NAME(@PINFOREF@("WORKING","DD"))
        ;
        IF $DATA(@PDDREF@("IMPORT-EXTRA"))>0 MERGE EXTRAB=@PDDREF@("IMPORT-EXTRA")
        IF $DATA(@PDDREF@("DIFFERENCE"))>0 MERGE DIFFARRAY=@PDDREF@("DIFFERENCE")
        IF ($DATA(@REFEXTRAB)>0)!($DATA(@REFDIFFARR)>1) GOTO HDD2  ;"skip XML read and comparison
        ;
        IF $DATA(@PDDREF@("TEMP-ARRAY"))>0 DO  GOTO HDD1  ;"skip XML read
        . MERGE TEMPARR=@PDDREF@("TEMP-ARRAY")
        ;
        NEW DDNODE SET DDNODE=$$GDESCNOD^TMGXMLT(XMLHANDLE,NODEFILE,"DataDictionary")
        IF DDNODE=0 DO  GOTO HDDDN
        . SET ERRMSG(1)="Unable to check compatibility of data dictionary for file "_FILENUM
        . SET ERRMSG(2)="  because a DataDictionary node could not be found as a child node"
        . SET ERRMSG(3)="  from node "_NODEFILE_".  Aborting."
        ;
        SET PROGRESSFN="use $P DO PROGBAR^TMGUSRI2(INCVAR,""Reading ^DD(""_FILENUM_"")"",0,TEMPSIZE,,"""_$H_""") use IO"
        WRITE "Gathering import data dictionary (DD) information for file "_FILENUM_"...",!
        DO READARR^TMGXMLT(XMLHANDLE,DDNODE,.TEMPARR,.PROGRESSFN,.INCVAR)
        SET INCVAR=TEMPSIZE XECUTE PROGRESSFN  ;"set progress bar to 100%
        ;
        WRITE !,"                                                          " DO CUU^TMGTERM(1)
        WRITE !,"Sizing up data read in..."
        SET TEMPSIZE=$$NodeCt^TMGMISC("TEMPARR")
        WRITE " ",TEMPSIZE," nodes.",!
        KILL @PDDREF@("TEMP-ARRAY") MERGE @PDDREF@("TEMP-ARRAY")=TEMPARR
        ;
        IF $DATA(TEMPARR)=0 DO  GOTO HDDDN
        . SET ERRMSG(1)="Reading of DD array failed.  Aborting."
        ;
HDD1    ;"------ DO actual comparison
        SET INCVAR=0
        SET PROGRESSFN="use $P DO PROGBAR^TMGUSRI2(INCVAR,""^DD(""_FILENUM_"")"",0,TEMPSIZE,,"""_$H_""") use IO"
        WRITE "Comparing imported data dictionary (DD) to installed DD for File ",FILENUM,"...",!
        KILL @REFEXTRAB,@REFDIFFARR
        IF $$CompABArray^TMGMISC("^DD("_FILENUM_")","TEMPARR",REFEXTRAB,,REFDIFFARR,.PROGRESSFN,.INCVAR)=1 DO  GOTO CPDN
        . SET ERRMSG(1)="Error or abort comparing data."
        WRITE !
        SET INCVAR=TEMPSIZE XECUTE PROGRESSFN  ;"set progress bar to 100%
        DO FixArray^TMGMISC(REFEXTRAB)
        DO FixArray^TMGMISC(REFDIFFARR)
        KILL @PDDREF@("IMPORT-EXTRA") MERGE @PDDREF@("IMPORT-EXTRA")=EXTRAB
        KILL @PDDREF@("DIFFERENCE") MERGE @PDDREF@("DIFFERENCE")=DIFFARRAY
        ;
HDD2    ;" ------- process found differences
        IF $$HNDLXTRA(REFEXTRAB)=0 DO  GOTO HDDDN
        . SET ERRMSG(1)="Unable to handle extra fields or files found in data from source"
        . SET ERRMSG(2)="VistA system.  Aborting..."
        ;
        IF $$HNDLDIFF(REFDIFFARR)=0 DO  GOTO HDDDN
        . SET ERRMSG(1)="Unable to handle differences between source and destination VistA"
        . SET ERRMSG(2)="installations.  Aborting."
        ;
HDDDN   QUIT
        ;
HANDLDIC(XMLHANDLE,NODEFILE,PINFOREF,ERRMSG)  ;
        ;"==============================================================
        ;"Handle ^DIC -------------------------------
        ;"==============================================================
        NEW TEMPARR,EXTRAB,MISSINGB,DIFFARRAY
        NEW TEMPSIZE SET TEMPSIZE=100000
        NEW REFEXTRAB SET REFEXTRAB="EXTRAB"
        NEW REFDIFFARR SET REFDIFFARR="DIFFARRAY"
        NEW PDICREF SET PDICREF=$NAME(@PINFOREF@("WORKING","DIC"))
        ;
        IF $DATA(@PDICREF@("IMPORT-EXTRA"))>0 MERGE EXTRAB=@PDICREF@("IMPORT-EXTRA")
        IF $DATA(@PDICREF@("DIFFERENCE"))>0 MERGE DIFFARRAY=@PDICREF@("DIFFERENCE")
        IF ($DATA(@REFEXTRAB)>0)!($DATA(@REFDIFFARR)>1) GOTO HDIC2
        ;
        IF $DATA(@PDICREF@("TEMP-ARRAY"))>0 DO  GOTO HDIC1
        . MERGE TEMPARR=@PDICREF@("TEMP-ARRAY")
        ;
        ;"---- read XML data into temporary array
        NEW DICNODE SET DICNODE=$$GDESCNOD^TMGXMLT(XMLHANDLE,NODEFILE,"DIC_File")
        IF DICNODE=0 DO  GOTO CPSTORE
        . SET ERRMSG(1)="Unable to check compatibility of ^DIC for file "_FILENUM
        . SET ERRMSG(1)="  because a DIC_File node could not be found as a child node"
        . SET ERRMSG(1)="  from node "_NODEFILE_".  Aborting."
        ;
        SET INCVAR=0,TEMPSIZE=100000
        SET PROGRESSFN="use $P DO PROGBAR^TMGUSRI2(INCVAR,""Reading ^DIC(""_FILENUM_"")"",0,TEMPSIZE,,"""_$H_""") use IO"
        WRITE "Gathering import DIC information for file "_FILENUM_"...",!
        DO READARR^TMGXMLT(XMLHANDLE,DICNODE,.TEMPARR,.PROGRESSFN,.INCVAR)
        SET INCVAR=TEMPSIZE XECUTE PROGRESSFN  ;"set progress bar to 100%
        WRITE !,"Sizing up data read in..."
        NEW TEMPSIZE SET TEMPSIZE=$$NodeCt^TMGMISC("TEMPARR")
        WRITE " ",TEMPSIZE," nodes.",!
        KILL @PDICREF@("TEMP-ARRAY") MERGE @PDICREF@("TEMP-ARRAY")=TEMPARR
        ;
        IF $DATA(TEMPARR)=0 DO  GOTO HDICDN
        . SET ERRMSG(1)="Reading of DIC array failed.  Aborting."
        ;
HDIC1   ;"------ DO actual comparison
        SET INCVAR=0
        SET PROGRESSFN="use $P DO PROGBAR^TMGUSRI2(INCVAR,""^DIC(""_FILENUM_"")"",0,TEMPSIZE,,"""_$H_""") use IO"
        WRITE "Comparing imported DIC to installed DIC for File ",FILENUM,"...",!
        IF $$CompABArray^TMGMISC("^DIC("_FILENUM_")","TEMPARR",REFEXTRAB,,REFDIFFARR,.PROGRESSFN,.INCVAR)=1 DO  GOTO CPSTORE
        . SET ERRMSG(1)="Error or abort while comparing data."
        WRITE !,!
        DO FixArray^TMGMISC(REFEXTRAB)
        KILL @PDICREF@("IMPORT-EXTRA") MERGE @PDICREF@("IMPORT-EXTRA")=@REFEXTRAB
        DO FixArray^TMGMISC(REFDIFFARR)
        KILL @PDICREF@("DIFFERENCE") MERGE @PDICREF@("DIFFERENCE")=@REFDIFFARR
        ;
HDIC2   ;" ------- process found differences
        IF $$HNDLXTRA(REFEXTRAB)=0 DO  GOTO HDICDN
        . SET ERRMSG(1)="Unable to handle extra fields or files found in data from source"
        . SET ERRMSG(2)="VistA system.  Aborting..."
        KILL @PDICREF@("IMPORT-EXTRA") MERGE @PDICREF@("IMPORT-EXTRA")=@REFEXTRAB
        ;
        IF $$HNDLDIFF(REFDIFFARR)=0 DO  GOTO HDICDN
        . SET ERRMSG(1)="Unable to handle differences between source and destination VistA"
        . SET ERRMSG(2)="installations.  Aborting."
        KILL @PDICREF@("DIFFERENCE") MERGE @PDICREF@("DIFFERENCE")=@REFDIFFARR
HDICDN  QUIT
        ;
HNDLXTRA(REFSRCEXTRA)  ;"HANDLE EXTRA
        ;"Purpose: to handle addition of extra (non-conflicting) fields / files
        ;"         to destination (local) VistA system based on import data
        ;"Input: REFSRCEXTRA -- PASS BY NAME.  Array of additions in source System.
        ;"                      Format as per CompABArray^TMGMISC
        ;"Result: 1=OK to continue, 0=Failed resolution.
        ;"Note: this function is assuming input like this:
        ;"      @ARRAY@("^GLBNAME",filenumber,...
        NEW RESULT SET RESULT=1  ;"default to SUCCESS
        NEW USERINPUT SET USERINPUT=""
        NEW MENU
        SET MENU(0)="Pick option for handling EXTRA file info from importing VistA"
        SET MENU(1)="MERGE node(s) into the local system."
        SET MENU(2)="Do NOT add this into the local system."
        SET MENU(3)="SKIP for now.  Decide IF import actually needs these fields."
        SET MENU(4)="Choose for each INDIVIDUAL entry"
        NEW GBLREF SET GBLREF=""
        FOR  SET GBLREF=$ORDER(@REFSRCEXTRA@(GBLREF)) QUIT:(GBLREF="")!(USERINPUT="^")  DO
        . NEW FILENUM SET FILENUM=""
        . FOR  SET FILENUM=$ORDER(@REFSRCEXTRA@(GBLREF,FILENUM)) QUIT:(FILENUM="")!(USERINPUT="^")  DO
        . . WRITE !,"The Remote/Source VistA system File #",FILENUM," (",$$GETFNAME^TMGXMLT2(FILENUM),") in ",GBLREF," has Extra Information:",!
        . . NEW FIELDNUM SET FIELDNUM=""
        . . FOR  SET FIELDNUM=$ORDER(@REFSRCEXTRA@(GBLREF,FILENUM,FIELDNUM)) QUIT:(FIELDNUM="")!(USERINPUT="^")  DO
        . . . NEW SUBREF SET SUBREF=$NAME(@REFSRCEXTRA@(GBLREF,FILENUM,FIELDNUM))
        . . . WRITE #,!
        . . . WRITE "File# ",FILENUM,", Field# ",FIELDNUM," has the following:",!
        . . . DO ARRDUMP($NAME(@REFSRCEXTRA@(GBLREF,FILENUM,FIELDNUM)),,,"F")
        . . . SET USERINPUT=$$MENU^TMGUSRI2(.MENU,3)
        . . . IF USERINPUT="^" SET RESULT=0 QUIT
        . . . IF USERINPUT=3 QUIT
        . . . IF USERINPUT=2 DO  QUIT
        . . . . KILL @SUBREF
        . . . IF USERINPUT=1 DO  QUIT
        . . . . NEW WRITEREF SET WRITEREF=$qsubscript(SUBREF,1)
        . . . . NEW IDX for IDX=2:1:$QLENGTH(SUBREF) DO
        . . . . . SET WRITEREF=$NAME(@WRITEREF@($qsubscript(SUBREF,IDX)))
        . . . . IF $DATA(@WRITEREF)>0 DO  QUIT
        . . . . . WRITE "Aborting MERGE because "_WRITEREF_" already has data!",!
        . . . . MERGE @WRITEREF=@SUBREF
        . . . . KILL @SUBREF
        . . . IF USERINPUT=4 DO  QUIT
        . . . . NEW SUBNODE SET SUBNODE=""
        . . . . FOR  SET SUBNODE=$ORDER(@SUBREF@(SUBNODE)) QUIT:(SUBNODE="")!(USERINPUT="^")  DO
        . . . . . SET USERINPUT=$$HNDLXTRA($NAME(@SUBREF@(SUBNODE)))
        QUIT RESULT
        ;
HNDLDIFF(REFDIFFARR)  ;"HANDLE DIFF
        ;"Purpose: To handle difference between source and local installations.
        ;"Input: REFDIFFARR -- PASS BY NAME.  Array of differences.  Format as
        ;"                     per CompABArray^TMGMISC
        ;"Result: 1=OK to continue, 0=Failed resolution.
        ;"Note: this function probably needs to be changed to handle reformatted DIFFARRAY
        NEW RESULT SET RESULT=1  ;"default to SUCCESS
        NEW REF SET REF=""
        FOR  SET REF=$ORDER(@REFDIFFARR@("A",REF)) QUIT:(REF="")!(RESULT=0)  DO
        . NEW IDX SET IDX=""
        . FOR  SET IDX=$ORDER(@REFDIFFARR@("A",REF,IDX)) QUIT:(IDX="")!(RESULT=0)!(RESULT=2)  DO
        . . NEW LOCAL,IMPORT
        . . MERGE LOCAL=@REFDIFFARR@("A",REF,IDX)
        . . MERGE IMPORT=@REFDIFFARR@("B",REF,IDX)
        . . ;"new NAME SET NAME=$NAME(@REF@(IDX))
        . . NEW NAME SET NAME=REF
        . . SET RESULT=$$HND1DIFF(NAME,.LOCAL,.IMPORT)
        WRITE !!
        QUIT RESULT
        ;
HND1DIFF(NAME,LOCAL,IMPORT)  ;"HANDLE 1 DIFF
        ;"Scope: private
        ;"Purpose: to handle 1 difference.
        ;"Input: LOCAL PASS BY REFERENCE
        ;"       IMPORT PASS BY REFERENCE
        ;"Results: 1=OK to continue, 0=cancel IMPORT
        NEW RESULT SET RESULT=1
        WRITE #
        WRITE "For node: ",NAME,!
        WRITE "================================",!
        WRITE "LOCAL VistA has this:",!
        WRITE $GET(LOCAL),!
        WRITE !
        WRITE "IMPORTING VistA has this:",!
        WRITE $GET(IMPORT),!
        NEW MENU,USERINPUT
        SET MENU(1)="Cancel Import"
        SET MENU(2)="Ignore difference"
        SET MENU(3)="Ignore ALL for this field"
        SET MENU(4)="Overwrite LOCAL with IMPORTING"
        SET USERINPUT=$$MENU^TMGUSRI2(.MENU,2)
        IF (USERINPUT="^")!(USERINPUT=1) SET RESULT=0 GOTO H1DDN
        IF USERINPUT=2 GOTO H1DDN
        IF USERINPUT=3 SET RESULT=2 GOTO H1DDN
        IF USERINPUT=4 DO
        . WRITE "IMPLEMENT THIS FEATURE LATER... (HND1DIFF^TMGXMLIN)",!
        . SET RESULT=1
H1DDN   QUIT RESULT
        ;
HNDDICXT(REFSRCEXTRA)  ;"HANDLE DIC EXTRA
        ;"Purpose: to handle addition of extra (non-conflicting) fields / files
        ;"         to destination (local) VistA system based on import data
        ;"Input: REFSRCEXTRA -- PASS BY NAME.  Array of additions in source System.
        ;"                      Format as per CompABArray^TMGMISC
        ;"Result: 1=OK to continue, 0=Failed resolution.
        NEW RESULT SET RESULT=1  ;"default to SUCCESS
        IF $DATA(@REFSRCEXTRA)>0 DO
        . WRITE "Please modify HNDDICXT^TMGXMLIN to handle extra info from import.",!
        . DO ZWRITE^TMGZWR("REFSRCEXTRA")
        . SET RESULT=-1
        QUIT RESULT
        ;
HNDDICDF(REFDIFFARR)  ;"HANDLE DIC DIFF
        ;"Purpose: To handle difference between source and local installations.
        ;"Input: REFDIFFARR -- PASS BY NAME.  Array of differences.  Format as
        ;"                     per CompABArray^TMGMISC
        ;"Result: 1=OK to continue, 0=Failed resolution.
        NEW RESULT SET RESULT=1  ;"default to SUCCESS
        IF $DATA(@REFDIFFARR)>0 DO
        . WRITE "Please modify HNDDICDF^TMGXMLIN to handle differences from import.",!
        . SET RESULT=-1
        QUIT RESULT
        ;
IMXL1REC(XMLHANDLE,SRCSYSNAME,FILENUM,NODERECORD)  ;"IMPORT XML 1 REC
        ;"Purpose: to import 1 record
        ;"Input: XMLHANDLE -- The handle created by loading function.
        ;"       SRCSYSNAME -- The name of the source VistA system
        ;"       FILENUM -- file number of target file to up uploaded into
        ;"       NODERECORD -- the XML node pointing the the record to upload.
        ;"Assumption: The target VistA system has already been checked and is
        ;"              compatible with upload data.
        ;"            ALSO, data exported should have been in INTERNAL format.
        ;"              This is because the upload will be INTERNAL values (to try
        ;"              to bypass import transforms.)
        ;"Note: IF the XML entry for the record contains the tag="POINTED_TO_RECORD",
        ;"      Then this record is recognized as a supporting record, rather than
        ;"      primary import information.  In this case, a check will be made to
        ;"      see IF the record has already been uploaded.  If so, then it will not
        ;"      be uploaded again.
        ;"Note: A translation table for IEN's in the source system, and the target
        ;"      system will be maintained as follows:
        ;"      ^TMG("XML EXPORTER",SRCSYSNAME,FILENUM,SrcIEN)=TargetIEN
        ;"      ^TMG("XML EXPORTER",SRCSYSNAME,FILENUM,SrcIEN)=TargetIEN
        ;"      ^TMG("XML EXPORTER",SRCSYSNAME,FILENUM,SrcIEN)=TargetIEN
        ;"Note: This does not current support or hand DIFROM records, or records
        ;"      with an expectation of IEN's to match IEN's in other files etc.
        ;"      I will have to handle these problems as they come up.
        ;"Output
        ;"   ^TMG("XML EXPORTER",SRCSYSNAME,FILENUM,"NEEDS RETRY",NODERECORD,.01)=OLDTARGETIEN
        ;"   ^TMG("XML EXPORTER",SRCSYSNAME,FILENUM,"NEEDS RESOLUTION",localIEN,FIELDNUM)=OLDTARGETIEN
        ;"   ^TMG("XML EXPORTER",SRCSYSNAME,FILENUM,REMOTEIEN)=localIEN
        ;"Results: 0=OK to continue, 1=ABORT, 2=try again later
        NEW RESULT SET RESULT=0
        NEW ERRMSG
        NEW REMOTEIEN   ;"aka SrcIEN
        NEW LOCALIEN    ;"aka TargetIEN
        NEW MODE
        SET REMOTEIEN=+$$GTATRVAL^TMGXMLT(XMLHANDLE,NODERECORD,"id")
        IF REMOTEIEN'>0 DO  GOTO IM1RDN
        . SET ERRMSG(1)="Can't find import IEN in XML node# "_NODERECORD
        SET LOCALIEN=+$GET(^TMG("XML EXPORTER",SRCSYSNAME,FILENUM,REMOTEIEN))
        IF LOCALIEN>0 GOTO IM1RDN  ;"Already uploaded or found.  Done here...
        ;
        ;"Handle usual case of importing 1 record here.
        NEW TMGFDA,TMGIEN,TMGMSG
        NEW TEMPARR,PTRTOARRAY
        NEW REFFDA SET REFFDA=$NAME(TMGFDA(FILENUM,"+1,"))
        NEW ABORT SET ABORT=0
        NEW NODEFIELD SET NODEFIELD=0
        FOR  SET NODEFIELD=$$GDESCNOD^TMGXMLT(XMLHANDLE,NODERECORD,"FIELD",NODEFIELD) QUIT:(NODEFIELD'>0)!ABORT  DO
        . NEW FIELDNUM SET FIELDNUM=$$GTATRVAL^TMGXMLT(XMLHANDLE,NODEFIELD,"id")
        . NEW FIELDTYPE SET FIELDTYPE=$$GTATRVAL^TMGXMLT(XMLHANDLE,NODEFIELD,"TYPE")
        . IF FIELDTYPE="WORD-PROCESSING" QUIT  ;"handle later...  **FINISH**
        . NEW VALUE SET VALUE=$$GET1LTEXT^TMGXMLT(XMLHANDLE,NODEFIELD)
        . IF VALUE'="" SET TEMPARR(FIELDNUM)=VALUE
        . NEW P2 SET P2=$PIECE($GET(^DD(FILENUM,FIELDNUM,0)),"^",2)
        . IF P2["P" SET PTRTOARRAY(FIELDNUM)=+$PIECE(P2,"P",2)
        ;
        SET MODE=$$GTATRVAL^TMGXMLT(XMLHANDLE,NODERECORD,"tag")
        IF MODE="POINTED_TO_RECORD" DO
        . ;"See if similar record already exists in the system. (matching)
        . ;"NEW Data
        . ;"SET Data(0,"FILE")=FILENUM
        . ;"MERGE Data(1)=TEMPARR
        . ;"SET Data(1,.01,"MATCHTHIS")=1 ;" <--- Only require .01 field to match.  Enough?
        . ;"NEW PRIORIEN
        . ;"IF $$GetRecMatch^TMGDBAPI(.Data,.PRIORIEN)=0 DO  QUIT
        . ;". SET ERRMSG(1)="Error during search for prior records."
        . ;"IF PRIORIEN'>0 QUIT  ;"no pre-existing records exist on system.
        . NEW SRCHVAL SET SRCVAL=$GET(TEMPARR(.01)) QUIT:SRCHVAL=""
        . NEW GL SET GL=$GET(^DIC(FILENUM,0,"GL")) QUIT:GL=""  SET GL=$$CREF^DILF(GL)
        . NEW PRIORIEN SET PRIORIEN=+$ORDER(@GL@("B",SRCHVAL,""))
        . SET LOCALIEN=PRIORIEN
        . SET ^TMG("XML EXPORTER",SRCSYSNAME,FILENUM,REMOTEIEN)=LOCALIEN
        IF $DATA(ERRMSG)!(LOCALIEN>0) GOTO IM1RDN
        ;
        NEW MANDIEN SET MANDIEN=0 ;"manditory IEN for storage of this record (if any)
        IF $P($GET(^DD(FILENUM,.01,0)),"^",5,99)["DINUM" DO
        . NEW TARGETFILE SET TARGETFILE=+$GET(PTRTOARRAY(.01))
        . IF TARGETFILE>0 DO
        . . NEW OLDTARGETIEN SET OLDTARGETIEN=+$GET(TEMPARR(.01))
        . . NEW LOCALTARGETIEN
        . . SET LOCALTARGETIEN=+$GET(^TMG("XML EXPORTER",SRCSYSNAME,FILENUM,OLDTARGETIEN))
        . . ;"At this point, we know that this record is DINUM'd, meaning that it must
        . . ;"be filed at a specific IEN.  In this case it's IEN must match the pointer
        . . ;"stored in the .01 field.  NOTE, however, that this pointer must be resolved
        . . ;"the corresponding record on the NEW system.  So OLDTARGETIEN is resolved
        . . ;"to LOCALTARGETIEN.  If LOCALTARGETIEN=0, then this means that the other record
        . . ;"that this one is tied to has not yet been imported.  So this record should
        . . ;"be tried again after other files from import have been processed.
        . . IF LOCALTARGETIEN=0 DO  QUIT
        . . . SET ^TMG("XML EXPORTER",SRCSYSNAME,FILENUM,"NEEDS RETRY",NODERECORD,.01)=OLDTARGETIEN
        . . . SET RESULT=2 ;"2=try again later.
        . . SET MANDIEN=LOCALTARGETIEN
        . ELSE  DO
        . . ;"this is a DINUM based on something that is not a pointer
        . . SET MANDIEN=$GET(TEMPARR(.01))
        . . ;"Not sure of examples of above, but shouldn't need resolving in NEW system.
        IF RESULT=2 GOTO IM1RDN
        ;
        ;"Resolve any pointers out IF possible prior to storage.
        ;"Make note of pointer in record that will need resolving later.
        NEW RESOLVELATER
        SET FIELDNUM=""
        FOR  SET FIELDNUM=$ORDER(PTRTOARRAY(FIELDNUM)) QUIT:(+FIELDNUM'>0)  DO
        . NEW TARGETFILE SET TARGETFILE=+$GET(PTRTOARRAY(FIELDNUM))
        . NEW OLDTARGETIEN SET OLDTARGETIEN=$GET(TEMPARR(FIELDNUM))
        . NEW LOCALTARGETIEN SET LOCALTARGETIEN=+$GET(^TMG("XML EXPORTER",SRCSYSNAME,FILENUM,OLDTARGETIEN))
        . IF LOCALTARGETIEN>0 DO
        . . SET TEMPARR(FIELDNUM)=LOCALTARGETIEN  ;"<-- pointer now resolved.
        . ELSE  DO
        . . SET RESOLVELATER(FIELDNUM)=OLDTARGETIEN ;"<-- remember to resolve later.
        ;
        MERGE @REFFDA=TEMPARR  ;" SET up TMGFDA
        IF MANDIEN>0 SET TMGIEN(1)=MANDIEN  ;"specify mandated IEN to store record in.
        DO UPDATE^DIE("S","TMGFDA","TMGIEN","TMGMSG")  ;" DO actual storage.
        IF $DATA(TMGMSG("DIERR")) DO
        . DO SHOWDIER^TMGDEBU2(.TMGMSG)
        . SET RESULT=1
        ELSE  DO  ;"make notes of newly stored record
        . SET LOCALIEN=+$GET(TMGIEN(1))
        . SET ^TMG("XML EXPORTER",SRCSYSNAME,FILENUM,REMOTEIEN)=LOCALIEN
        . SET FIELDNUM=""   ;"some pointers out might not have been resolvable.  Remember this.
        . FOR  SET FIELDNUM=$ORDER(RESOLVELATER(FIELDNUM)) QUIT:(FIELDNUM="")  DO
        . . SET OLDTARGETIEN=$GET(RESOLVELATER(FIELDNUM))
        . . SET ^TMG("XML EXPORTER",SRCSYSNAME,FILENUM,"NEEDS RESOLUTION",LOCALIEN,FIELDNUM)=OLDTARGETIEN
        ;
IM1RDN  IF $DATA(ERRMSG) DO
        . WRITE "ERROR.  Message:",!
        . NEW IDX SET IDX=""
        . FOR  SET IDX=$ORDER(ERRMSG(IDX)) QUIT:(i="")  WRITE ERRMSG(IDX),!
        . DO PRESS2GO^TMGUSRI2
        . SET RESULT=1 ;" ABORT
        ;        
        QUIT RESULT
        ;
 ;"=================================================================
ARRDUMP(ARRAYP,TMGIDX,INDENT,FLAGS) ;
        ;"Purpose: An extended version of GTM's ZWRITE command
        ;"         NOTE:  See also ZWRITE^TMGZWR
        ;"Input: Uses global scope var TMGDBINDENT (if defined)
        ;"        ARRAYP: NAME of global or variable to display, i.e. "^VA(200)", "MyVar"
        ;"        TMGIDX: initial index (i.e. 5 IF wanting to start with ^VA(200,5) -- Optional
        ;"        INDENT: spacing from left margin to begin with. (A number.  Each count is 2 spaces)
        ;"                OPTIONAL: INDENT may be an array, with information about columns
        ;"                to skip.  For example:
        ;"                INDENT=3, INDENT(2)=0 --> show | for columns 1 & 3, but NOT 2
        ;"        FLAGS: OPTIONAL.  "F"-> flat (don't use tree structure)
        ;"Result: none
        IF $DATA(ARRAYP)=0 QUIT
        ;
        IF $GET(FLAGS)["F" DO  GOTO ADDONE
        . NEW REF SET REF=ARRAYP
        . NEW NODENUMS SET NODENUMS=$QLENGTH(REF)
        . NEW LVALUE SET LVALUE=$QSUBSCRIPT(REF,NODENUMS)
        . WRITE REF,"=""",$GET(@REF),"""",!
        . FOR  SET REF=$QUERY(@REF) QUIT:(REF="")!($QSUBSCRIPT(REF,NODENUMS)'=LVALUE)  do
        . . WRITE REF,"=""",$GET(@REF),"""",!
        ;
        ;"Note: I need to DO some validation to ensure ARRAYP doesn't have any null nodes.
        NEW X SET X="SET TEMP=$GET("_ARRAYP_")"
        SET X=$$UP^XLFSTR(X)
        DO ^DIM ;"a method to ensure ARRAYP doesn't have an invalid reference.
        IF $GET(X)="" QUIT
        ;
        IF '$DATA(TMGDBINDENT) NEW TMGDBINDENT
        SET TMGDBINDENT=$GET(TMGDBINDENT,0) 
        NEW TMGDEBUG SET TMGDEBUG=1  ;"Force this function to output, even IF TMGDEBUG is not defined.
        ;
        NEW TMGI
        SET TMGIDX=$GET(TMGIDX)
        SET INDENT=$GET(INDENT,0)
        NEW SAVINDEX SET SAVINDEX=TMGIDX
        DO DBINDENT^TMGDEBU4(.TMGDBINDENT)
        ;
        IF INDENT>0 DO
        . FOR TMGI=1:1:INDENT-1 DO
        . . NEW STR SET STR=""
        . . IF $GET(INDENT(TMGI),-1)=0 SET STR="  "
        . . ELSE  SET STR="| "
        . . DO DBWRITE^TMGDEBU4(.TMGDBINDENT,STR)
        . DO DBWRITE^TMGDEBU4(.TMGDBINDENT,"}~")
        ;
        IF TMGIDX'="" DO
        . IF $DATA(@ARRAYP@(TMGIDX))#10=1 DO
        . . NEW STR SET STR=@ARRAYP@(TMGIDX)
        . . IF STR="" SET STR=""""""
        . . NEW QT SET QT=""
        . . IF +TMGIDX'=TMGIDX SET QT=""""
        . . DO DBWRITE^TMGDEBU4(.TMGDBINDENT,QT_TMGIDX_QT_" = "_STR,1)
        . ELSE  DO
        . . DO DBWRITE^TMGDEBU4(.TMGDBINDENT,TMGIDX,1)
        . SET ARRAYP=$NAME(@ARRAYP@(TMGIDX))
        ELSE  DO
        . DO DBWRITE^TMGDEBU4(.TMGDBINDENT,ARRAYP,0)
        . IF $DATA(@ARRAYP)#10=1 DO
        . . DO DBWRITE^TMGDEBU4(0,"="_$GET(@ARRAYP),0)
        . DO DBWRITE^TMGDEBU4(0,"",1)
        ;
        SET TMGIDX=$ORDER(@ARRAYP@(""))
        IF TMGIDX="" GOTO ADDONE
        SET INDENT=INDENT+1
        ;
        FOR  DO  QUIT:TMGIDX=""
        . NEW IDX SET IDX=$ORDER(@ARRAYP@(TMGIDX))
        . IF IDX="" SET INDENT(INDENT)=0
        . NEW TMPINDENT MERGE TMPINDENT=INDENT
        . DO ARRDUMP(ARRAYP,TMGIDX,.TMPINDENT)  ;"Call self recursively
        . SET TMGIDX=$ORDER(@ARRAYP@(TMGIDX))
        ;
        ;"Put in a blank space at end of sub-branch
        DO DBINDENT^TMGDEBU4(.TMGDBINDENT)
        ;
        IF INDENT>0 DO
        . FOR TMGI=1:1:INDENT-1 DO
        . . NEW STR SET STR=""
        . . IF $GET(INDENT(TMGI),-1)=0 SET STR="  "
        . . ELSE  SET STR="| "
        . . DO DBWRITE^TMGDEBU4(.TMGDBINDENT,STR)
        . DO DBWRITE^TMGDEBU4(.TMGDBINDENT," ",1)
        ;
ADDONE  QUIT
        ;
 
