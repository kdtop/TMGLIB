TMGFMUT2 ;TMG/kst/Fileman utility functions ;02/19/10, 2/2/14
         ;;1.0;TMG-LIB;**1**;02/19/10
 ;
 ;"TMG FILEMAN-UTILITY FUNCTIONS
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
 ;"NOTE: This module will provide (in addition to other tools) pointer tools
 ;"       that are different than found in ^TMGFMUT.
 ;"      The approach here will be to create tables of pointer
 ;"    relationships, and then allow faster analysis from the tables.  This
 ;"   recognizes that such tables can rapidly become out of sync with the
 ;"  actual data.  Thus the tools will only be valid on a system at rest (i.e.
 ;" no users on the system).  They could be used for system maint. overnight
 ;" etc.
 ;" Several of the routines here are called from ^TMGSIPH*
 ;"Data is stored here:
 ;"^TMG("PTXREF","OUT",FROMFILE,IENS,FROMFLD,P2FILE,PT)=""
 ;"^TMG("PTXREF","IN",P2FILE,PT,FROMFILE,IENS,FROMFLD)=""
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"PREPPTO(FILENUM,FLD,ARRAY) -- SET up an easy to use array of potential pointers out from a file.
 ;"SETPTOUT(FILENUM,DESTREF,PGFN,PGFREQ,LIMITS) -- scan a given file and create an array with all pointers INTO that file.
 ;"KILLPTIX -- delete the last run of PT XREF, so it can be refreshened.
 ;"GETPTIN(PARAMS,OUT,PGFN) --get a listing of all pointers INTO requested record
 ;"BAKXREF(PARAMS,PGFN) --Make a xref of cross-references (a backward xref)
 ;"BAKSXREF(PARAMS,PGFN)-- Make a xref of cross-references (a backward xref) **OF SUBFILES**
 ;"GETXRAGE --Return, in HOURS, the time since the ^TMG("PTXREF") array has had any modification
 ;"GETGL(SUBFILENUM,IENDEPTH) --return a reference 'GL' string for subfiles, or regular files too.
 ;"GETGREF(FILENUM,IENS) -- To return a reference to a File or SUBFILE (IENS only needed when working with Subfiles)
 ;"IENCOMBO(REF,IENDEPTH,IEN) --set up global vars IEN(2),IEN(3),... etc, as needed for next combo when cycling through subfile arrays.
 ;"TOPFILEN(FILENUM) -- Return the highest level of filenumber.
 ;"ISSUBFIL(FILENUM) -- Return IF a file is a subfile.
 ;"GETIENS(IEN) --Turn IEN Array into IENS
 ;"IENS2IEN(IENS,IEN) -- Turn IENS into IEN Array, opposite of GETIENS function
 ;"GETSPFN(FILENUM) -- Turn a subfile number into 'SubFileNum{ParentFileNum{GrandParentFileNum....'
 ;"HASPTR(FILENUM) --Return IF file contains fields that are pointers to other files
 ;"HASPTRSF(FILENUM) -- Return IF file contains subfiles (or sub-subfiles) that contain pointers to other files)
 ;"FILENAME(FILENUM) -- turn a (SUB)File number into a file name.
 ;"SCANFLD(FILENUM,FLD,ACTFN,PGFN,PGFREQ,LIMITS) -- scan a given file or subfile and call user code for each entry
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;"TESTSPTO -- test out PT XREF setup.
 ;"HNDLPTIX(FILENUM,PGFN) -- prepair PT XREF for all records pointing INTO specified file.
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"TMGKERN2, TMGUSRIF, TMGDBAPI
 ;"=======================================================================
 ;
PREPPTO(FILENUM,FLD,ARRAY) ;
        ;"Purpose: To SET up an easy to use array of potential pointers out from a file.
        ;"Input: FILENUM-- the filenumber to evaluate
        ;"       FLD -- the field to check for.
        ;"       ARRAY -- PASS BY REFERENCE.  An OUT PARAMETER.  Format
        ;"          ARRAY(GREF,ENTRY)
        ;"          Note: ENTRY=DataPiece^PointedToFile^PointedToReference^IENDepth^[V]^FromFile^Fromfield^ONEREF
        ;"          ONEREF will have multipe IEN entries IF IENDepth>1, e.g. '^SC(IEN,"S",IEN(2),1,IEN(3),"C")'
        ;"          with order of IEN, IEN(2), IEN(3), ... etc.
        ;"NOTE: This function was originally coppied from SETPTOUT^TMGSIPH1
        ;
        IF +$GET(FILENUM)'=FILENUM GOTO SPODN
        NEW IENDEPTH SET IENDEPTH=1
        NEW REF SET REF=$GET(^DIC(FILENUM,0,"GL"))
        IF (REF=""),$DATA(^DD(FILENUM,0,"UP")) DO
        . SET REF=$$GETGL(FILENUM,.IENDEPTH)
        IF REF="" GOTO SPODN
        NEW GREF SET GREF=REF
        IF GREF["IEN," SET GREF=$PIECE(GREF,"IEN,",1)
        NEW ZNODE SET ZNODE=$GET(^DD(FILENUM,FLD,0))
        NEW FLDTYPE SET FLDTYPE=$PIECE(ZNODE,"^",2)
        IF (FLDTYPE'["P")&(FLDTYPE'["V") GOTO SPODN
        NEW LOC SET LOC=$PIECE(ZNODE,"^",4)
        NEW NODE SET NODE=$PIECE(LOC,";",1)
        NEW PCE SET PCE=+$PIECE(LOC,";",2)
        IF +NODE'=NODE SET NODE=""""_NODE_""""
        NEW ONEREF,SUBSCR
        SET SUBSCR=$SELECT((IENDEPTH>1):"("_IENDEPTH_")",1:"")
        SET ONEREF=REF_"IEN"_SUBSCR_","_NODE_")"
        NEW P2FILE SET P2FILE=0
        NEW VREC SET VREC=0
        NEW DONE SET DONE=0
        FOR  DO  QUIT:(DONE=1)
        . NEW ISVIRT SET ISVIRT=""
        . NEW P2REF
        . IF FLDTYPE["V" DO  QUIT:(DONE=1)
        . . SET VREC=+$ORDER(^DD(FILENUM,FLD,"V",VREC))
        . . IF VREC=0 SET DONE=1 QUIT
        . . SET P2FILE=+$GET(^DD(FILENUM,FLD,"V",VREC,0))
        . . SET ISVIRT="V"
        . . SET P2REF=$PIECE($GET(^DIC(P2FILE,0,"GL")),"^",2)
        . ELSE  DO
        . . SET P2FILE=+$PIECE(FLDTYPE,"P",2)
        . . SET P2REF=$PIECE(ZNODE,"^",3)
        . . SET DONE=1
        . NEW ENTRY
        . SET ENTRY=PCE_"^"_P2FILE_"^"_P2REF_"^"_IENDEPTH_"^"_ISVIRT_"^"_FILENUM_"^"_FLD_"^"_ONEREF
        . SET ARRAY(GREF,ENTRY)=""
SPODN   QUIT
 ;
 ;
GETIENS(IEN) ;"Turn IEN Array into IENS
        NEW RESULT SET RESULT=IEN
        NEW I SET I=1
        FOR  SET I=$ORDER(IEN(I)) QUIT:(+I'>0)  DO
        . SET RESULT=$GET(IEN(I))_","_RESULT
        IF RESULT["," SET RESULT=RESULT_","
        QUIT RESULT
 ;
 ;
IENS2IEN(IENS,IEN) ;
        ;"Purpose: Turn IENS into IEN Array, opposite of GETIENS function
        ;"Input: IENS - an IENS string to convert.  E.g. '7,2342,"
        ;"       IEN -- PASS BY REFERENCE.  An OUT PARAMETER.
        ;"Results: None.
        KILL IEN
        SET IENS=$GET(IENS)
        NEW LEN SET LEN=$LENGTH(IENS,",")-1
        NEW I FOR I=1:1:LEN DO
        . NEW IDX SET IDX=(LEN-I+1)
        . NEW VALUE SET VALUE=$PIECE(IENS,",",I)
        . IF IDX>1 SET IEN(IDX)=VALUE
        . ELSE  SET IEN=VALUE
        QUIT
 ;
 ;
SETPTOUT(FILENUM,DESTREF,PGFN,PGFREQ,LIMITS)
        ;"Purpose: To scan a given file and create an array with all pointers INTO that file.
        ;"         NOTE: The output will be a snapshot of the database that will quickly be out
        ;"               of date if/when the database changes.
        ;"Input:  FILENUM -- the Fileman file number to test. This is that file that other records will point TO
        ;"        DESTREF -- OPTIONAL.  PASS BY NAME.  The name of an array to store output into.
        ;"                   MUST BE IN CLOSED FORMAT.  If not specified, then ^TMG("PTXREF" will be used.
        ;"        PGFN -- OPTIONAL.  <Progress Function Code>
        ;"                A string of mumps code that will be executed once for every 100 records that are scanned.
        ;"                The following variables will be defined for use.
        ;"                 TMGCT -- The total number of that have been scanned so far.
        ;"                 TMGFNAME -- The file that is currently begin scanned.
        ;"                 TMGIEN -- Record number in the current file being scanned.
        ;"                 TMGMAX -- Max record number in the current file being scanned.
        ;"                 TMGMIN -- Min record number in the current file being scanned.
        ;"        PGFREQ --OPTIONAL.  The number of records that must be scanned before the Progress Fn
        ;"                      code is called.  Default = 100.
        ;"        LIMITS -- OPTIONAL.  If $DATA(LIMITS("REF"))'=0 then REF should be an array with format:
        ;"                           LIMITS("REF")=<aREF>
        ;"                           @aREF@(FILENUM,IEN)=""  <-- Forms a SET that will limit search.  Only these entries are considered.
        ;"                           @aREF@(FILENUM,IEN)=""  <--
        ;"Result: none.
        NEW RESULT SET RESULT=0
        SET FILENUM=+$GET(FILENUM) GOTO:(FILENUM=0) SPODN
        SET DESTREF=$GET(DESTREF,$NAME(^TMG("PTXREF")))
        SET PGFN=$GET(PGFN,"QUIT")
        SET PGFREQ=+$GET(PGFREQ) IF PGFREQ'>0 SET PGFREQ=100
        NEW LIMITREF SET LIMITREF=$GET(LIMITS("REF"))
        SET LIMITS=(LIMITREF'="")
        ;
        ;"Build up ARRAY, an easy to use array of potential pointers OUT from a file.
        ;"NOTE: Only files that point INTO FILENUM will be put into this array.
        NEW ARRAY
        NEW FROMFILE SET FROMFILE=0  ;"OtherFile
        FOR  SET FROMFILE=$ORDER(^DD(FILENUM,0,"PT",FROMFILE)) QUIT:(+FROMFILE'>0)  DO
        . NEW FLD SET FLD=0
        . FOR  SET FLD=$ORDER(^DD(FILENUM,0,"PT",FROMFILE,FLD)) QUIT:(+FLD'>0)  DO
        . . DO PREPPTO(FROMFILE,FLD,.ARRAY) ;
        ;
        ;"Now, cycle through possible pointers to look for real pointers.
        SET @DESTREF@("TIMESTAMP")=$H
        NEW ABORT SET ABORT=0
        NEW TMGCT SET TMGCT=0
        NEW GREF SET GREF=""
        FOR  SET GREF=$ORDER(ARRAY(GREF)) QUIT:(GREF="")!ABORT  DO
        . NEW TEMPN SET TEMPN=0
        . NEW SKIP SET SKIP=0
        . NEW FOUND SET FOUND=0
        . FOR  SET TEMPN=$ORDER(^DIC(TEMPN)) QUIT:(+TEMPN'>0)!FOUND  DO  ;"Get filenumber of GREF
        . . IF $GET(^DIC(TEMPN,0,"GL"))'=GREF QUIT
        . . SET FOUND=1
        . . SET @DESTREF@("OUT",TEMPN)=$H
        . IF SKIP QUIT
        . NEW REF SET REF=$$CREF^DILF(GREF)
        . NEW TMGMAX SET TMGMAX=$ORDER(@REF@("+"),-1)
        . NEW TMGMIN SET TMGMIN=$ORDER(@REF@(0))
        . NEW SKIP SET SKIP=0
        . NEW IEN SET IEN=0
        . FOR  SET IEN=$ORDER(@REF@(IEN)) QUIT:(+IEN'>0)!ABORT!SKIP  DO
        . . IF LIMITS DO  QUIT:SKIP  ;"If running on client side, only look at downloaded records.
        . . . IF $DATA(@LIMITREF@(TEMPN,IEN))'=0 QUIT
        . . . SET SKIP=1
        . . NEW INFO SET INFO=""
        . . FOR  SET INFO=$ORDER(ARRAY(GREF,INFO)) QUIT:(INFO="")!ABORT  DO
        . . . NEW PCE SET PCE=$PIECE(INFO,"^",1)
        . . . NEW IENDEPTH SET IENDEPTH=$PIECE(INFO,"^",4)
        . . . NEW ONREF SET ONEREF=$PIECE(INFO,"^",8,99)
        . . . NEW TEMP SET TEMP=IEN KILL IEN SET IEN=TEMP ;"clear subscripts
        . . . FOR  QUIT:($$IENCOMBO(ONEREF,IENDEPTH,.IEN)'=1)!ABORT  DO
        . . . . NEW FROMFILE SET FROMFILE=$PIECE(INFO,"^",6)
        . . . . SET TMGCT=TMGCT+1
        . . . . IF TMGCT#PGFREQ=0 DO
        . . . . . SET ABORT=$$USRABORT^TMGUSRI2() QUIT:ABORT
        . . . . . NEW TMGFNAME SET TMGFNAME=$PIECE($GET(^DIC(FROMFILE,0)),"^",1)
        . . . . . NEW TMGIEN SET TMGIEN=IEN
        . . . . . NEW $ETRAP SET $ETRAP="W ""(Invalid M Code!.  Error Trapped.)"" S $ETRAP="""",$ECODE="""""
        . . . . . XECUTE PGFN
        . . . . NEW PT SET PT=$PIECE($GET(@ONEREF),"^",PCE) ;"$$IENCOMBO sets up IEN(n).. needed for @REF
        . . . . NEW ISVIRT SET ISVIRT=($PIECE(INFO,"^",5)="V")
        . . . . NEW P2REF SET P2REF=$PIECE(INFO,"^",3)
        . . . . IF ISVIRT,$PIECE(PT,";",2)'=P2REF QUIT  ;"Loop to handle PTR with different INFO entry (V-Ptrs stored as IEN;OREF)
        . . . . SET PT=+PT QUIT:(PT'>0)
        . . . . NEW IENS SET IENS=$$GETIENS(.IEN)
        . . . . NEW P2FILE SET P2FILE=$PIECE(INFO,"^",2)
        . . . . NEW FROMFLD SET FROMFLD=$PIECE(INFO,"^",7)
        . . . . SET @DESTREF@("OUT",FROMFILE,IENS,FROMFLD,P2FILE,PT)=""
        . . . . SET @DESTREF@("IN",P2FILE,PT,FROMFILE,IENS,FROMFLD)=""
        QUIT
 ;
 ;
TESTSPTO
        ;"Purpose: test out PT XREF setup.
        NEW X,Y,DIC
        SET DIC=1,DIC(0)="MAEQ"
        DO ^DIC WRITE !
        IF +Y'>0 QUIT
        NEW TMGSTIME SET TMGSTIME=$H
        NEW PGFN SET PGFN="DO PROGBAR^TMGUSRI2(TMGIEN,TMGFNAME,TMGMIN,TMGMAX,60,TMGSTIME)"
        DO SETPTOUT(+Y,$NAME(^TMG("PTXREF")),PGFN,500)
        WRITE !,"Quitting normally.",!
        QUIT
 ;
 ;
KILLPTIX ;
        ;"Purpose: To delete the last run of PT XREF, so it can be refreshened.
        KILL ^TMG("PTXREF")
        QUIT
 ;
 ;
HNDLPTIX(FILENUM,PGFN) ;
        ;"Purpose: To prepair PT XREF for all records pointing INTO specified file.
        ;"Input: FILENUM -- The fileman file number to get pointers INTO.
        ;"       PGFN -- OPTIONAL -- M Code that wil be periodically executed to show progress.
        ;"Result: None
        SET FILENUM=+$GET(FILENUM) IF FILENUM'>0 QUIT
        NEW TMGSTIME SET TMGSTIME=$H
        DO SETPTOUT(FILENUM,$NAME(^TMG("PTXREF")),.PGFN,3000,CLSIDE)
        SET ^TMG("PTXREF","IN",FILENUM)=$H
        SET ^TMG("PTXREF")=$H
        QUIT
 ;
 ;
GETPTIN(PARAMS,OUT,PGFN) ;
        ;"Purpose: To get a listing of all pointers INTO requested record
        ;"Input: PARAMS -- this is FILENUM^IEN
        ;"       OUT  -- PASS BY REFERNCE.  Will be filled as with format:
        ;"                   OUT(1)=FROMFILE^FROMIENS^FROMFLD
        ;"                   OUT(2)=FROMFILE^FROMIENS^FROMFLD
        ;"       PGFN -- OPTIONAL -- M Code that wil be periodically executed to show progress.
        ;"                   ...
        NEW FILENUM SET FILENUM=+$PIECE(PARAMS,"^",1)
        NEW TMGCT SET TMGCT=1
        NEW IEN SET IEN=+$PIECE(PARAMS,"^",2)
        IF $DATA(^TMG("PTXREF","IN",FILENUM))'>0 DO HNDLPTIX(FILENUM,.PGFN)
        NEW FROMFILE,FROMIENS,FROMFLD
        SET (FROMFILE,FROMIENS,FROMFLD)=0
        FOR  SET FROMFILE=$ORDER(^TMG("PTXREF","IN",FILENUM,IEN,FROMFILE)) QUIT:(+FROMFILE'>0)  DO
        . FOR  SET FROMIENS=$ORDER(^TMG("PTXREF","IN",FILENUM,IEN,FROMFILE,FROMIENS)) QUIT:(+FROMIENS'>0)  DO
        . . FOR  SET FROMFLD=$ORDER(^TMG("PTXREF","IN",FILENUM,IEN,FROMFILE,FROMIENS,FROMFLD)) QUIT:(+FROMFLD'>0)  DO
        . . . SET OUT(TMGCT)=FROMFILE_"^"_FROMIENS_"^"_FROMFLD
        . . . SET TMGCT=TMGCT+1
        QUIT
 ;
 ;
BAKXREF(PARAMS,PGFN) ;
        ;"Purpose: Make a xref of cross-references (a backward xref)
        ;"Input: PARAMS -- This is FILENUM^[KEEP]
        ;"                 FILENUM -- The fileman file to work with
        ;"                 KEEP -- optional.  DEFAULT=0;  If '1', then nothing done IF xref already exists.
        ;"       PGFN -- OPTIONAL -- M Code that wil be periodically executed to show progress.
        ;"              The following globally-scoped variables will be available for use:
        ;"              FILENUM,INDEX
        ;"Output: ^TMG("PTXREF","XREFS",FILENUM,IEN,REF)=<xref value>
        ;"        e.g. ^TMG("PTXREF","XREFS",FILENUM,113,"^VA(200,""A"",8870804679,113)")=6188
        ;"Result: none.
        SET PARAMS=$GET(PARAMS)
        SET FILENUM=$PIECE(PARAMS,"^",1) IF +FILENUM'>0 GOTO BXDN
        IF FILENUM["{" DO BAKSXREF(.PARAMS,.PGFN) GOTO BXDN
        IF $DATA(^TMG("PTXREF","XREFS",FILENUM))>0 GOTO BXDN
        SET PGFN=$GET(PGFN)
        NEW STIME SET STIME=$H
        NEW GREF SET GREF=$GET(^DIC(FILENUM,0,"GL"))
        IF GREF="" QUIT  ;"Happened for file 799.6
        NEW GRLEN SET GRLEN=$LENGTH(GREF)
        NEW CGREF SET CGREF=$$CREF^DILF(GREF)
        NEW GREFQLEN SET GREFQLEN=$QLENGTH(CGREF)
        NEW REF SET REF=$QUERY(@CGREF@("@"))
        NEW INDEX,LASTINDEX SET LASTINDEX=""
        NEW DELAYCT SET DELAYCT=500 ;"ensure fires at least once to avoid timeout with many quick XREFS
        NEW DONE SET DONE=0
        KILL ^TMG("PTXREF","XREFS",FILENUM)
        IF $GET(^TMG("PTXREF"))="" SET ^TMG("PTXREF")=$H
        SET ^TMG("PTXREF","XREFS",FILENUM)=$H
        FOR  QUIT:(REF="")  DO
        . SET DELAYCT=DELAYCT+1
        . IF (DELAYCT>500),(PGFN'="") DO
        . . SET DELAYCT=0
        . . IF ($PIECE($H,",",2)-STIME)<5 QUIT
        . . SET STIME=$PIECE($H,",",2)
        . . NEW $ETRAP SET $ETRAP="SET $ETRAP="""",$ECODE="""""
        . . XECUTE PGFN
        . IF $EXTRACT(REF,1,GRLEN)'=GREF SET REF="" QUIT
        . NEW IEN SET IEN=$QSUBSCRIPT(REF,$QLENGTH(REF))
        . SET ^TMG("PTXREF","XREFS",FILENUM,IEN,REF)=$GET(@REF)
        . SET INDEX=$QSUBSCRIPT(REF,GREFQLEN+1)
        . IF INDEX'=LASTINDEX DO
        . . SET LASTINDEX=INDEX
        . . SET STIME=$PIECE($H,",",2)
        . . SET DELAYCT=0
        . . NEW $ETRAP SET $ETRAP="SET $ETRAP="""",$ECODE="""""
        . . XECUTE PGFN
        . SET REF=$QUERY(@REF)
BXDN    QUIT
 ;
 ;
BAKSXREF(PARAMS,PGFN) ;
        ;"Purpose: Make a xref of cross-references (a backward xref) **OF SUBFILES**
        ;"Input: PARAMS -- This is FILENUM^[KEEP]
        ;"                 FILENUM -- subfilenum{parentfilenum{grandparent....
        ;"                 KEEP -- optional.  DEFAULT=0;  If '1', then nothing done IF xref already exists.
        ;"       PGFN -- OPTIONAL -- M Code that wil be periodically executed to show progress.
        ;"              The following globally-scoped variables will be available for use:
        ;"              FILENUM,INDEX
        ;"Output: ^TMG("PTXREF","XREFS",SUBFILENUM,IENS,REF)=<xref value>
        ;"Result: none.
        SET PARAMS=$GET(PARAMS)
        SET FILENUM=+$PIECE(PARAMS,"^",1) ;"Just get the subfile number.
        IF FILENUM'>0 GOTO BXSDN
        IF $DATA(^TMG("PTXREF","XREFS",FILENUM))>0 GOTO BXSDN
        SET PGFN=$GET(PGFN)
        NEW IEN SET IEN=0
        NEW INDEX SET INDEX=""
        NEW IENDEPTH SET IENDEPTH=""
        NEW GREF SET GREF=$$GETGL(FILENUM,.IENDEPTH)  ;" e.g. file 44.003 --> ^SC(IEN,"S",IEN(2),1,   (open format)
        IF GREF="" QUIT  ;"Happened for file 799.6
        NEW CGREF SET CGREF=$$CREF^DILF(GREF)
        NEW J FOR J=1:1:IENDEPTH SET IEN(J)=1  ;"dummy values to satisfy $QLENGTH on line below
        NEW GREFQLEN SET GREFQLEN=$QLENGTH($NAME(@CGREF))
        NEW DELAYCT SET DELAYCT=999
        ;"NOTE: IENCOMBO is only for getting subfile combos.  It doesn't modify IEN.  So I need
        ;"to manually cycle between all the records of the top-most file. Use GETTOPFILEN^TMGFMUT2 to get this.
        NEW TOPFILE SET TOPFILE=+$$TOPFILEN(FILENUM)
        NEW TOPREF SET TOPREF=$GET(^DIC(TOPFILE,0,"GL"))
        IF TOPREF="" GOTO BXSDN
        KILL IEN SET IEN=0
        SET TOPREF=$$CREF^DILF(TOPREF)
        FOR  SET IEN=$ORDER(@TOPREF@(IEN)) QUIT:(+IEN'>0)  DO
        . FOR  DO  QUIT:(OKCOMBO=0)
        . . SET DELAYCT=DELAYCT+1
        . . IF (DELAYCT>500),(PGFN'="") DO
        . . . SET DELAYCT=0
        . . . NEW $ETRAP SET $ETRAP="SET $ETRAP="""",$ECODE="""""
        . . . XECUTE PGFN
        . . SET OKCOMBO=$$IENCOMBO^TMGFMUT2(CGREF,IENDEPTH,.IEN) ;"Sets up IEN(n).. needed for @CGREF
        . . QUIT:(OKCOMBO=0)
        . . NEW GREF SET GREF=$$OREF^DILF($NAME(@CGREF))  ;"resolve IEN vars into actual numbers
        . . NEW GRLEN SET GRLEN=$LENGTH(GREF)
        . . NEW REF SET REF=$NAME(@CGREF@("@"))
        . . FOR  DO  QUIT:(REF="")
        . . . SET REF=$QUERY(@REF)
        . . . IF $EXTRACT(REF,1,GRLEN)'=GREF SET REF="" QUIT
        . . . SET INDEX=$QSUBSCRIPT(REF,GREFQLEN+1)  ;"set up for use by PGFN
        . . . NEW PTR SET PTR=$QSUBSCRIPT(REF,$QLENGTH(REF))
        . . . NEW TMPIEN MERGE TMPIEN=IEN
        . . . SET TMPIEN(IENDEPTH+1)=PTR
        . . . NEW IENS SET IENS=$$GETIENS(.TMPIEN)
        . . . SET ^TMG("PTXREF","XREFS",FILENUM,IENS,REF)=$GET(@REF)
        . KILL IEN("DONE"),IEN("INIT")
BXSDN    QUIT
 ;
 ;
GETXRAGE() ;
        ;"Purpose: Return, in HOURS, the time since the ^TMG("PTXREF") array has had any modification
        ;"Results: 0 IF not currently defined, otherwise number of HOURS since setup.
        NEW LASTT SET LASTT=$GET(^TMG("PTXREF","TIMESTAMP"))
        NEW DELTAT SET DELTAT=0
        IF LASTT'="" SET DELTAT=$$HDIFF^XLFDT($H,LASTT,2)\(60*60)
        QUIT DELTAT
 ;
 ;
GETGL(SUBFILENUM,IENDEPTH) ;
        ;"Purpose: To return a reference 'GL' string for subfiles, and also regular files.
        ;"         E.g. file 44.003 --> ^SC(IEN,"S",IEN(2),1,
        ;"INPUT: SUBFILENUM -- The sub file number
        ;"       IENDEPTH -- PASS BY REFERENCE. Should be 1 on first call
        ;"Results: Returns an OPEN reference.
        NEW RESULT SET RESULT=""
        SET IENDEPTH=+$GET(IENDEPTH)+1
        NEW UPFILE SET UPFILE=+$GET(^DD(SUBFILENUM,0,"UP"))
        IF UPFILE'>0 DO  GOTO IDN
        . SET RESULT=$GET(^DIC(SUBFILENUM,0,"GL"))
        NEW UPFLD SET UPFLD=+$ORDER(^DD(UPFILE,"SB",SUBFILENUM,""))
        IF UPFLD'>0 GOTO IDN
        NEW NODE SET NODE=$PIECE(^DD(UPFILE,UPFLD,0),"^",4)
        SET NODE=$PIECE(NODE,";",1)
        IF +NODE'=NODE SET NODE=""""_NODE_""""
        SET RESULT=NODE_","
        NEW GREF SET GREF=$GET(^DIC(UPFILE,0,"GL"))
        NEW NUM2 SET NUM2=IENDEPTH
        IF GREF="" SET GREF=$$GETGL(UPFILE,.IENDEPTH)
        SET RESULT=GREF_"#"_$CHAR(64+NUM2)_"#,"_RESULT
IDN     NEW I,TMGSPEC
        FOR I=1:1:IENDEPTH DO
        . IF I=IENDEPTH SET TMGSPEC("#"_$CHAR(64+I)_"#")="IEN"
        . ELSE  SET TMGSPEC("#"_$CHAR(64+I)_"#")="IEN("_(IENDEPTH-I+1)_")"
        SET RESULT=$$REPLACE^XLFSTR(RESULT,.TMGSPEC)
IDN2    QUIT RESULT
 ;
 ;
GETGREF(FILENUM,IENS) ;
        ;"Purpose: To return a reference to a file or a subfile
        ;"         This function differs from GETGL in that REF from GETGREF here has actual record numbers
        ;"         put in, while REF from GETGL has variable names (e.g. IEN(2)) in it.
        ;"Input: IENS -- A standard IENS string to locate subfile.    Not used unless FILENUM is a subfile.
        ;"              NOTE: the lowest level IEN is not used.  e.g. '7,22345,' --> 7 is not used
        ;"Returns : an OPEN format reference.
        NEW GREF
        NEW IENDEPTH SET IENDEPTH=1
        SET GREF=$$GETGL(FILENUM,.IENDEPTH)
        IF $$ISSUBFIL(FILENUM)=0 GOTO GGRDN
        SET GREF=$$CREF^DILF(GREF)
        NEW IEN DO IENS2IEN(.IENS,.IEN)
        SET GREF=$NAME(@GREF) ;"Lock IEN value(s) from IENS into GREF
        SET GREF=$$OREF^DILF(GREF)
GGRDN   QUIT GREF
 ;
 ;
IENCOMBO(REF,IENDEPTH,IEN) ;
        ;"Purpose: To SET up global vars IEN(2),IEN(3),... etc, as needed for next combo when
        ;"         cycling through subfile arrays.
        ;"Input: REF -- the is the potential pointer reference, as stored in ^TMG("TMGSIPH","DD",FILENUM,"PTR OUT",REF,INFO)
        ;"              e.g. '^SC(IEN,"S",IEN(2),1,IEN(3),"C")    (and IENDEPTH would be 3 for this example)
        ;"       IENDEPTH -- The number of variables to consider.  I.e IF value=3, then REF will
        ;"                   contain IEN,IEN(2),IEN(3)
        ;"       IEN -- PASS BY REFERENCE.  This variable will serve as an array to store the
        ;"              information needed to create the next valid SET of variables needed
        ;"              to make use of the reference.  NOTE: The value of IEN itself (e.g. IEN=4),
        ;"              is not modified.
        ;"Results: 1 IF a NEW valid IEN combo has been SET up.
        ;"         0 IF there are no more subfile entries.
        ;"
        ;"NOTE!!!: If IENDEPTH=3, then this function will fail IF there are records for depth 1,2, but not 3
        ;"  Needs debugging...
        ;"
        ;
        NEW RESULT SET RESULT=0 ;"Default to invalid
        IF $DATA(IEN("DONE")) GOTO ICODN
        IF IENDEPTH=1 DO  GOTO ICODN
        . SET IEN("DONE")=1
        . SET RESULT=1
        NEW I
        SET RESULT=1 ;"Default to valid
        IF $DATA(IEN("ORDS"))=0 DO
        . FOR I=2:1:IENDEPTH SET IEN("ORDS",I)=$$CREF^DILF($PIECE(REF,"IEN("_I_")",1))
        IF +$GET(IEN("INIT"))=0 DO
        . SET IEN("INIT")=1
        . NEW INVALID SET INVALID=0
        . NEW POS FOR POS=2:1:IENDEPTH  DO  QUIT:(INVALID=1)
        . . IF $GET(IEN(POS))'="" QUIT
        . . NEW TEMPREF SET TEMPREF=IEN("ORDS",POS)
        . . SET IEN(POS)=+$ORDER(@TEMPREF@(0))
        . . IF IEN(POS)'>0 SET INVALID=1
        . IF (POS=IENDEPTH),(INVALID=0) SET RESULT=1
        ELSE  DO  ;"At this point, IEN(n),IEN(n+1),... vars should be SET to last valid combo.
        . SET I=IENDEPTH
        . NEW REF,NODE
        . FOR  DO  QUIT:(I<2)!(I=IENDEPTH)
        . . SET REF=IEN("ORDS",I)
        . . SET IEN(I)=$ORDER(@REF@(IEN(I)))
        . . IF (IEN(I)="") SET I=I-1 QUIT  ;"reached last record at this level, so backup up level
        . . IF (I<IENDEPTH) DO  ;"We have a valid record, now get next subrecord
        . . . NEW J FOR J=(I+1):1:IENDEPTH DO  QUIT:(IEN(J)="")
        . . . . SET REF=IEN("ORDS",J)
        . . . . SET IEN(J)=$ORDER(@REF@(""))
        FOR I=2:1:IENDEPTH IF +$GET(IEN(I))'>0 SET RESULT=0
ICODN   QUIT RESULT
        ;
        ;
TOPFILEN(FILENUM) ;
        ;"Purpose: Return the highest level of filenumber.  I.e. IF subfile, then return parent
        ;"         parent filenumber.  If sub-sub-file, then return higest file number that is
        ;"         not a sub file.
        ;"         If FILENUM is not a subfile, then just return same FILENUM
        ;"Results: 0 IF problem, or Top-most filenumber.
        NEW RESULT SET RESULT=0
        IF +$GET(FILENUM)'=FILENUM GOTO TFNDN
        FOR  QUIT:$DATA(^DD(FILENUM,0,"UP"))=0  DO
        . SET FILENUM=+$GET(^DD(FILENUM,0,"UP"))
        SET RESULT=FILENUM
TFNDN   QUIT RESULT
 ;
 ;
ISSUBFIL(FILENUM) ;
        ;"Purpose: Return IF a file is a subfile.
        ;"Input: FILENUM -- a File, or Subfile, number
        ;"Result: 1 IF file is a subfile
        QUIT ($DATA(^DD(FILENUM,0,"UP"))>0)
 ;
 ;
HASPTRSF(FILENUM) ;" HAS POINTER-CONTAINING SUBFILES
        ;"Purpose: Return IF file contains subfiles (or sub-subfiles) that contain pointers to other files)
        ;"Input: FILENUM -- The file number to investigatge
        ;"Results: 1 IF has pointer subfiles.
        ;";
        NEW RESULT SET RESULT=0
        NEW FLD SET FLD=0
        FOR  SET FLD=$ORDER(^DD(FILENUM,FLD)) QUIT:(+FLD'>0)!(RESULT=1)  DO
        . NEW ZNODE SET ZNODE=$GET(^DD(FILENUM,FLD,0))
        . NEW FLDTYPE SET FLDTYPE=$PIECE(ZNODE,"^",2)
        . IF (+FLDTYPE'>0) QUIT
        . NEW SUBFILEN SET SUBFILEN=+FLDTYPE
        . IF $GET(^DD(SUBFILEN,0,"UP"))'=FILENUM QUIT
        . SET RESULT=$$HASPTR(SUBFILEN)
        QUIT RESULT
 ;
 ;
HASPTR(FILENUM) ;" HAS POINTER fields
        ;"Purpose: Return IF file contains fields that are pointers to other files
        ;"Input: FILENUM -- The file number to investigatge
        ;"Results: 1 IF has pointer subfiles.
        ;"
        NEW RESULT SET RESULT=($DATA(^DD(FILENUM,0,"PT"))'=0)
        IF RESULT GOTO HPDN
        NEW FLD SET FLD=0
        FOR  SET FLD=$ORDER(^DD(FILENUM,FLD)) QUIT:(+FLD'>0)!(RESULT=1)  DO
        . NEW ZNODE SET ZNODE=$GET(^DD(FILENUM,FLD,0))
        . NEW FLDTYPE SET FLDTYPE=$PIECE(ZNODE,"^",2)
        . IF +$PIECE(FLDTYPE,"P",2)>0 SET RESULT=1 QUIT
        . IF (+FLDTYPE'>0) QUIT
        . NEW SUBFILEN SET SUBFILEN=+FLDTYPE
        . IF $GET(^DD(SUBFILEN,0,"UP"))'=FILENUM QUIT
        . SET RESULT=$$HASPTRSF(SUBFILEN)
HPDN    QUIT RESULT
 ;
 ;
FILENAME(FILENUM) ;
        ;"Purpose: to turn a File number into a file name.  ALSO, turn input with format of
        ;"         SubfileNumber{ParentFileNumber into a meaningful name too.
        ;"Input: FILENUM:  A file number, or a SubfileNumber{ParentFileNumber
        ;"Result: returns name or name{name{name
        ;"
        IF (FILENUM'["{"),$$ISSUBFIL(+FILENUM) DO
        . SET FILENUM=$$GETSPFN(FILENUM)
        NEW RESULT SET RESULT=""
        NEW I
        FOR I=1:1:$LENGTH(FILENUM,"{") DO
        . NEW ANUM SET ANUM=$PIECE(FILENUM,"{",I)
        . NEW PFILE SET PFILE=+$GET(^DD(ANUM,0,"UP"))
        . NEW ANAME
        . IF PFILE=0 DO
        . . SET ANAME=$PIECE($GET(^DIC(ANUM,0)),"^",1)
        . ELSE  DO
        . . SET ANAME=$PIECE($GET(^DD(ANUM,0)),"^",1)
        . . SET ANAME=$PIECE(ANAME,"SUB-FIELD",1)
        . . SET ANAME=$$TRIM^XLFSTR(ANAME)
        . IF RESULT'="" SET RESULT=RESULT_"{"
        . SET RESULT=RESULT_ANAME
        QUIT RESULT
 ;
 ;
GETSPFN(FILENUM) ;" Get Special Filenum
        ;"Purpose: Turn a subfile number into a 'special' subfilenumber, in format of:
        ;"         SubFileNum{ParentFileNum{GrandParentFileNum....
        ;"Results: 0 IF problem, or string as above
        NEW RESULT SET RESULT=""
        NEW FN SET FN=FILENUM
        FOR  DO  QUIT:FN=0
        . IF RESULT'="" SET RESULT=RESULT_"{"
        . SET RESULT=RESULT_FN
        . SET FN=+$GET(^DD(FN,0,"UP"))
        QUIT RESULT
 ;
 ;
SCANFLD(FILENUM,FLD,ACTFN,PGFN,PGFREQ,LIMITS) ;
        ;"Purpose: To scan a given file or subfile and call user code for each entry
        ;"Input:  FILENUM -- the Fileman file (or subfile) number to scan
        ;"                      NOTE: IENS is not required for subfile because ALL possibilities are checked.
        ;"        FLD     -- the field number to scan.
        ;"        DESTREF -- OPTIONAL.  PASS BY NAME.  The name of an array to store output into.
        ;"                   MUST BE IN CLOSED FORMAT.  If not specified, then ^TMG("PTXREF" will be used.
        ;"        ACTFN -- <Action function code>.  This is code that will be XECUTED for every subrecord entry considered.
        ;"                The following variables will be defined for use.
        ;"                      TMGVAL -- the value of the field in the current subrecord (internal format)
        ;"                      ONEREF -- a closed reference to current data node
        ;"                      IEN is an array of IEN values (can be used to make IENS)
        ;"                      IF ABORT is SET to 1, then loop will stop.
        ;"                      (see PGFN for others)
        ;"        PGFN -- OPTIONAL.  <Progress Function Code>
        ;"                A string of mumps code that will be executed once for every 100 records that are scanned.
        ;"                The following variables will be defined for use.
        ;"                 TMGCT -- The total number of that have been scanned so far.
        ;"                 TMGFNAME -- The file that is currently begin scanned.
        ;"                 TMGIEN -- Record number in the current file being scanned.
        ;"                 TMGMAX -- Max record number in the current file being scanned.
        ;"                 TMGMIN -- Min record number in the current file being scanned.
        ;"        PGFREQ --OPTIONAL.  The number of records that must be scanned before the Progress Fn
        ;"                      code is called.  Default = 100.
        ;"        LIMITS -- OPTIONAL.  If $DATA(LIMITS("REF"))'=0 then REF should be an array with format:
        ;"                           LIMITS("REF")=<aREF>
        ;"                           @aREF@(FILENUM,IEN)=""  <-- Forms a SET that will limit search.  Only these entries are considered.
        ;"                           @aREF@(FILENUM,IEN)=""  <--
        ;"NOTE: Doesn't work yet with WP fields.
        ;"      ALSO, an example of using this function is in FLDHASDATA^TMGFMUT3()
        ;"Result: none.
        SET FILENUM=+$GET(FILENUM) GOTO:(FILENUM=0) SCFDN
        SET FLD=+$GET(FLD) GOTO:(FLD=0) SCFDN
        NEW ZNODE SET ZNODE=$GET(^DD(FILENUM,FLD,0))
        NEW FLDTYPE SET FLDTYPE=$PIECE(ZNODE,"^",2)
        IF $$ISWPFLD^TMGDBAPI(FILENUM,FLD) DO  GOTO SCFDN
        . ;"DO SCANWPFLD  <--- IMPLEMENT LATER...
        IF +FLDTYPE>0 DO  GOTO SCFDN
        . SET FILENUM=+FLDTYPE
        . SET FLD=.01
        . DO SCANFLD(FILENUM,FLD,.ACTFN,.PGFN,.PGFREQ,.LIMITS)
        SET ACTFN=$GET(ACTFN,"QUIT")
        SET PGFN=$GET(PGFN)
        SET PGFREQ=+$GET(PGFREQ) IF PGFREQ'>0 SET PGFREQ=100
        NEW LIMITREF SET LIMITREF=$GET(LIMITS("REF"))
        SET LIMITS=(LIMITREF'="")
        NEW TOPFN SET TOPFN=$$TOPFILEN(FILENUM)
        NEW ISSUB SET ISSUB=(TOPFN'=FILENUM)
        NEW TMGFNAME SET TMGFNAME=$PIECE($GET(^DIC(TOPFN,0)),"^",1)
        NEW ABORT SET ABORT=0
        NEW TMGCT SET TMGCT=0
        NEW IENDEPTH  SET IENDEPTH=$SELECT((ISSUB=0):0,(1=1):1)
        NEW REF SET REF=$$GETGL(FILENUM,.IENDEPTH)  ; "e.g. file 44.003 --> ^SC(IEN,"S",IEN(2),1,
        IF REF="" GOTO SCFDN
        NEW GREF SET GREF=REF
        IF GREF["IEN," SET GREF=$PIECE(GREF,"IEN,",1)
        NEW LOC SET LOC=$PIECE(ZNODE,"^",4)
        NEW NODE SET NODE=$PIECE(LOC,";",1)
        NEW PCE SET PCE=+$PIECE(LOC,";",2)
        IF +NODE'=NODE SET NODE=""""_NODE_""""
        NEW ONEREF,SUBSCR
        SET SUBSCR=$SELECT((IENDEPTH>1):"("_IENDEPTH_")",1:"")
        SET ONEREF=REF_"IEN"_SUBSCR_","_NODE_")"  ;"note IEN variable hard coded here.
        ;
        NEW TEMPN SET TEMPN=0
        NEW SKIP SET SKIP=0
        SET REF=$$CREF^DILF(GREF)
        NEW TMGMAX SET TMGMAX=$ORDER(@REF@("+"),-1)
        NEW TMGMIN SET TMGMIN=$ORDER(@REF@(0))
        NEW SKIP SET SKIP=0
        NEW IEN SET IEN=0
        FOR  SET IEN=$ORDER(@REF@(IEN)) QUIT:(+IEN'>0)!ABORT!SKIP  DO
        . IF LIMITS DO  QUIT:SKIP  ;"If running on client side, only look at downloaded records.
        . . IF $DATA(@LIMITREF@(TEMPN,IEN))'=0 QUIT
        . . SET SKIP=1
        . SET TMGCT=TMGCT+1
        . NEW TMGIEN SET TMGIEN=IEN
        . IF TMGCT#PGFREQ=0,(PGFN'="") DO
        . . SET ABORT=$$USRABORT^TMGUSRI2() QUIT:ABORT
        . . NEW $ETRAP SET $ETRAP="W ""(Invalid M Code!.  Error Trapped.)"" S $ETRAP="""",$ECODE="""""
        . . XECUTE PGFN
        . IF ISSUB=0 DO  QUIT
        . . NEW TMGVAL SET TMGVAL=$PIECE($GET(@ONEREF),"^",PCE)
        . . DO
        . . . NEW $ETRAP SET $ETRAP="W ""(Invalid M Code!.  Error Trapped.)"" S $ETRAP="""",$ECODE="""""
        . . . XECUTE ACTFN
        . NEW TEMP SET TEMP=IEN KILL IEN SET IEN=TEMP ;"clear subscripts
        . FOR  QUIT:($$IENCOMBO(ONEREF,IENDEPTH,.IEN)'=1)!ABORT  DO
        . . SET TMGCT=TMGCT+1
        . . NEW TMGIEN SET TMGIEN=IEN
        . . NEW TMGVAL SET TMGVAL=$PIECE($GET(@ONEREF),"^",PCE) ;"$$IENCOMBO sets up IEN(n).. needed for @REF
        . . DO
        . . . NEW $ETRAP SET $ETRAP="W ""(Invalid M Code!.  Error Trapped.)"" S $ETRAP="""",$ECODE="""""
        . . . XECUTE ACTFN
        ;
SCFDN   QUIT
 ;
 ;

