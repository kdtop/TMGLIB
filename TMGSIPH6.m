TMGSIPH6 ;TMG/kst/SIPHON PROGRAM, FOR TRANSFERRING VISTA INSTANCES ;2/15/10, 2/2/14
         ;;1.0;TMG-LIB;**1**;2/15/10
 ;
 ;"TMG SIPHON PROGRAM, FOR TRANSFERRING VISTA INSTANCE
 ;"Utility functions for working with transfers on client
 ;"Especially working with XRefs of transferred records.
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"XRFILES -- allow user to select files to be re-cross referenced
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"TMGUSRIF,DIK
 ;"=======================================================================
 ;
XRFILES ;
        ;"Purpose: To allow user to select files to be re-cross referenced
        ;"Input: None
        ;"Result: None
        ;"Output: Cross-references will be KILL'ed then SET, at user's choice
        NEW TMGARRAY,TMGSEL
        NEW FILENUM SET FILENUM=0
        FOR  SET FILENUM=$ORDER(^TMG("TMGSIPH","NEED RE-XREF",FILENUM)) QUIT:(FILENUM'>0)  DO
        . NEW DISPSTR SET DISPSTR="Re-index records in file #"_FILENUM_" ("
        . SET DISPSTR=DISPSTR_$PIECE($GET(^DIC(FILENUM,0)),"^",1)_")"
        . SET TMGARRAY(DISPSTR)=FILENUM
        NEW HEADER SET HEADER="Select File(s) to REINDEX. Press <ESC><ESC> when Done."
        DO SELECTR2^TMGUSRI3("TMGARRAY","TMGSEL",HEADER)
        ;
        NEW TMGABORT SET TMGABORT=0
        NEW IDX SET IDX=""
        FOR  SET IDX=$ORDER(TMGSEL(IDX)) QUIT:(IDX="")!TMGABORT  DO
        . SET FILENUM=$GET(TMGSEL(IDX)) QUIT:FILENUM=""
        . SET TMGABORT=($$REIX1FLE(FILENUM)'=1)
        ;
        DO PRESS2GO^TMGUSRI2;
        QUIT
 ;
 ;
REIX1FLE(FILENUM) ;
        ;"Purpose: to re-index all the cross references in 1 file
        ;"Input: FILENUM -- the file to reindex.
        ;"Results: 1=OK, 0 IF error.
        ;"
        ;"NOTE: There should not be a need to re-index subfiles, becaUse those
        ;"      IEN's are not moved / translated
        NEW RESULT SET RESULT=1 ;"default success
        NEW TMGCT SET TMGCT=50
        NEW STIME SET STIME=$H
        NEW VAFCA08 SET VAFCA08=1 ;"Prevent execution of XRef AVAFC01 (--> endless loop)
        NEW DIK,DA,CGREF
        SET DIK=$GET(^DIC(FILENUM,0,"GL"))
        IF DIK="" SET RESULT=0 GOTO RXF1
        NEW CGREF SET CGREF=$$CREF^DILF(DIK)
        NEW TMGMIN SET TMGMIN=$ORDER(@CGREF@(0))
        NEW TMGMAX SET TMGMAX=$ORDER(@CGREF@("#"),-1)
        NEW TMGABORT SET TMGABORT=0
        NEW TMGERR,TMGLASTE SET TMGERR=0,TMGLASTE=0
        SET DA=0
        FOR  SET DA=$ORDER(@CGREF@(DA)) QUIT:(+DA'>0)!TMGABORT  DO
        . IF $DATA(^TMG("TMGSIPH","RE-XREF DONE",FILENUM,DA)) QUIT
        . DO
        . . ;"NEW $ETRAP SET $ETRAP="SET $ZTRAP=""B"" WRITE ""$ZTRAP="",$ZTRAP,!,""Error during XRef of FILE #"",$GET(FILENUM),""; IEN="",$GET(DA),! WRITE $ZSTATUS,! SET $ETRAP="""",$ECODE="""",TMGERR=1"
        . . NEW $ETRAP SET $ETRAP="DO HANDLERR^TMGSIPH6"
        . . DO IX^DIK  ;"Uses DIK and DA as inputs
        . . SET ^TMG("TMGSIPH","RE-XREF DONE",FILENUM,DA)="" ;"<-- not done IF error during IX^DIK
        . SET TMGABORT=$$USRABORT^TMGUSRI2()
        . SET TMGCT=TMGCT+1
        . IF TMGCT>10 DO
        . . DO PROGBAR^TMGUSRI2(DA,"Re-indexing file: "_FILENUM,TMGMIN,TMGMAX,70,STIME)
        . . SET TMGCT=0
        IF (TMGABORT=0)&(TMGERR=0) KILL ^TMG("TMGSIPH","NEED RE-XREF",FILENUM)
        ELSE  SET RESULT=0
        WRITE !
RXF1    QUIT RESULT
 ;
 ;
HANDLERR ;
        SET $ZTRAP="B"
        IF $GET(DA)'=$GET(TMGLASTE) DO
        . WRITE !,"Error during XRef of FILE #",$GET(FILENUM),"; IEN=",$GET(DA),!
        . WRITE $ZSTATUS,!
        . SET TMGERR=1
        . SET TMGLASTE=DA
        SET $ETRAP="",$ECODE=""
        QUIT

