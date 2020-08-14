TMGFMUT3 ;TMG/kst/Fileman utility functions ;12/04/10, 2/2/14
         ;;1.0;TMG-LIB;**1**;12/04/10
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
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"$$MAKEFILE(INFO) -- Silent API create a NEW Fileman file
 ;"$$DELFILE(INFO)  -- Silent API for deleting a Fileman file
 ;"$$ADDFIELD(TMGINFO,TMGOUT) ;-- API for Add    aspect of Fileman MODIFY FILE
 ;"$$DELFIELD(TMGINFO,TMGOUT) ;-- API for Delete aspect of Fileman MODIFY FILE
 ;"$$EDITFLD(TMGINFO,TMGOUT)  ;-- API for Edit   aspect of Fileman MODIFY FILE
 ;"$$FLDHASDATA(FILENUM,FLD,SHOWPG) -- scan a file and see IF any records contain data for field FLD
 ;
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;"$$DELFLDVALS(INFO) -- delete ALL entries in given field in given file
 ;"
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;" TMGDIB, TMGDICAT*, DIC, DIE, TMGFMUT2
 ;"=======================================================================
 ;
TEST ;
    NEW INFO
    SET INFO("NAME")="kill_this_file"
    SET INFO("NUMBER")=22715
    SET INFO("GL")="^TMG(22715,"
    ;"SET INFO("AUDIT") -- optional AUDIT code, default is @
    ;"SET INFO("DD") -- optional DD code, default is @
    ;"SET INFO("DEL") -- optional DEL code, default is @
    ;"SET INFO("LAYGO") -- optional LAYGO code, default is @
    ;"SET INFO("RD") -- optional RD code, default is @
    ;"SET INFO("WR") -- optional WR code, default is @
    WRITE $$MAKEFILE(.INFO)
    QUIT
    ;
TEST2
    NEW INFO
    SET INFO("NAME")="Fld Name for .04"
    SET INFO("NUMBER")=".04"
    SET INFO("FILE")=22715
    ;"WRITE $$ADD1FLD(.INFO)
    QUIT
    ;
    ;
MAKEFILE(INFO) ;
        ;"Purpose: Silent API create a NEW Fileman file
        ;"NOTE: The creation of a file automatically causes creation of generic .01 field.
        ;"Input: INFO : Pass by Reference.  An array of information needed to create file
        ;"        INFO("NAME") -- file name
        ;"        INFO("NUMBER")-- file number
        ;"        INFO("GL") -- Global reference
        ;"        INFO("AUDIT") -- optional AUDIT code, default is @
        ;"        INFO("DD") -- optional DD code, default is @
        ;"        INFO("DEL") -- optional DEL code, default is @
        ;"        INFO("LAYGO") -- optional LAYGO code, default is @
        ;"        INFO("RD") -- optional RD code, default is @
        ;"        INFO("WR") -- optional WR code, default is @
        ;
        ;"Results: 1^Success  or -1^ErrorCode
        ;
        NEW TMGRESULT SET TMGRESULT="1^Success"
        SET INFO("AUDIT")=$GET(INFO("AUDIT"),"@")
        SET INFO("DD")=$GET(INFO("DD"),"@")
        SET INFO("DEL")=$GET(INFO("DEL"),"@")
        SET INFO("LAYGO")=$GET(INFO("LAYGO"),"@")
        SET INFO("RD")=$GET(INFO("RD"),"@")
        SET INFO("WR")=$GET(INFO("WR"),"@")
        NEW TMGNAME SET TMGNAME=$GET(INFO("NAME"))
        IF TMGNAME="" DO  GOTO MFDN
        . SET TMGRESULT="-1^File NAME not provided."
        IF +$ORDER(^DIC("B",TMGNAME,0))>0 DO  GOTO MFDN
        . SET TMGRESULT="-1^File NAME is already in use."
        NEW TMGNUM SET TMGNUM=+$GET(INFO("NUMBER"))
        IF TMGNUM'>0 DO  GOTO MFDN
        . SET TMGRESULT="-1^Invalid file NUMBER.  Got '"_$GET(INFO("NUMBER"))_"'"
        IF $DATA(^DIC(TMGNUM))>0 DO  GOTO MFDN
        . SET TMGRESULT="-1^File NUMBER already in use."
        NEW TMGGL,TMGGL0 SET (TMGGL,TMGGL0)=$GET(INFO("GL"))
        IF TMGGL="" DO  GOTO MFDN
        . SET TMGRESULT="-1^File Global Reference (GL) not provided"
        IF $E(TMGGL,1)'="^" GOTO MFGLER
        SET TMGGL=$PIECE(TMGGL,"^",2)
        IF TMGGL'["(" GOTO MFGLER
        NEW GLB SET GLB=$PIECE(TMGGL,"(",1)
        IF GLB'?.A GOTO MFGLER
        NEW GNUM SET GNUM=$PIECE(TMGGL,"(",2)
        SET TMGGL="^"_GLB_"("
        IF GNUM="" GOTO MF2
        SET TMGGL=TMGGL_+GNUM_","
MF2     NEW CR SET CR=$$CREF^DILF(TMGGL)
        IF $DATA(@CR) DO  GOTO MFDN
        . SET TMGRESULT="-1^File Global Reference (GL) '"_CR_"' already used.  Got '"_TMGL0_"'"
        ;
        NEW DIC,DA
        SET DIC="^DIC("
        SET DIC(0)="L"
        SET X=TMGNAME
        SET (DA,DINUM)=TMGNUM
        DO ^DIC
        IF $PIECE(Y,"^",3)'=1 DO  GOTO MFDN
        . SET TMGRESULT="-1^Error creating file: "_X
        DO DIE^TMGDIB(+Y)
        ;
MFDN    QUIT TMGRESULT
MFGLER  SET TMGRESULT="-1^Invalid File Global Reference (GL) not provided.  Got '"_TMGL0_"'"
        GOTO MFDN
        ;
        ;
DELFILE(INFO) ;
        ;"Purpose: to provide an API for deleting a Fileman file
        ;"Input: INFO -- pass by reference.  Filled with options as above
        ;"           INFO("FILE")="SomeName"  -- Name or number of file to delete
        ;"           INFO("DELETE RECORDS")=1 -- Optional, but IF not present, delete
        ;"                                      will fail IF file contains data
        ;"Results: 1^Success or -1^Error Message
        NEW TMGRESULT SET TMGRESULT="1^Success"
        NEW TMGFILE
        SET TMGFILE=$GET(INFO("FILE"))
        IF TMGFILE="" DO  GOTO DFDN
        . SET TMGRESULT="-1^File name or number not provided."
        IF +TMGFILE'=TMGFILE DO  GOTO:(+TMGRESULT<0) DFDN
        . SET TMGFILE=+$ORDER(^DIC("B",TMGFILE,""))
        . IF TMGFILE>0 QUIT
        . SET TMGRESULT="-1^Unable to determine file NUMBER for '"_INFO("FILE")_"'."
        NEW TMGGL SET TMGGL=$GET(^DIC(TMGFILE,0,"GL"))
        IF TMGGL'="" DO  GOTO:(+TMGRESULT<0) DFDN
        . NEW GL2 SET GL2=TMGGL_"0)"
        . IF +$ORDER(@GL2)=0 QUIT
        . IF $GET(INFO("DELETE RECORDS"))=1 QUIT
        . SET TMGRESULT="-1^File has records, but 'DELETE RECORDS' not passed."
        NEW DA,DIE,DR
        SET DIE=TMGGL222882
        SET DA="^DIC("
        SET DR=".01///@"
        DO ^DIE
DFDN    QUIT TMGRESULT
        ;
        ;
DELFLDVALS(INFO) ;
        ;"Purpose: CAUTION!: This function will delete ALL entries in given field in given file
        ;"                   It does honor Fileman WRITE restrictions
        ;"Input: INFO -- Pass by Reference.  An array of info needed to add field
        ;"              INFO("FILE") --File number (not name)
        ;"              INFO("IENS") -- IF File number is a subfile, then this is required
        ;"              INFO("NUMBER") -- number of field to delete
        ;"              INFO("DELETE DATA")=1 -- Optional, but IF not present, delete
        ;"                                      will fail IF file contains data
        ;"              INFO("WRITE ACCESS") -- optional WRITE access for field delete
        ;"Result: 1^Success,  -1^ErrorMessage
        NEW TMGRESULT SET TMGRESULT="1^Success"
        NEW TMGFILE SET TMGFILE=+$GET(INFO("FILE"))
        IF TMGFILE'>0 DO  GOTO DELVALDN
        . SET TMGRESULT="-1^File number not provided."
        NEW TMGNUM SET TMGNUM=+$GET(INFO("NUMBER"))
        IF TMGNUM'>0 DO  GOTO DELVALDN
        . SET TMGRESULT="-1^Field number not provided"
        NEW ZNODE SET ZNODE=$GET(^DD(TMGFILE,TMGNUM,0))
        NEW WTACCESS SET WTACCESS=$GET(INFO("WRITE ACCESS"))
        NEW TMGIENS SET TMGIENS=$GET(INFO("IENS"),",")
        NEW TMGGL SET TMGGL=$GET(^DIC(TMGFILE,0,"GL"))
        IF TMGGL="" DO  GOTO DELVALDN
        . SET TMGRESULT="-1^Unable to find global reference for file"
        SET TMGGL=$$CREF^DILF(TMGGL)
        NEW WTACCESS SET WTACCESS=$GET(INFO("WRITE ACCESS"))
        NEW TMGSAVAC SET TMGSAVAC=$GET(DUZ(0))
        IF WTACCESS'="" SET DUZ(0)=WTACCESS
        NEW TMGIEN SET TMGIEN=0
        FOR  SET TMGIEN=$ORDER(@TMGGL@(TMGIEN)) QUIT:(+TMGIEN'>0)!(+TMGRESULT=-1)  DO
        . NEW TMGFDA,TMGMSG,TMGIENS2
        . SET $PIECE(TMGIENS,",",1)=TMGIEN
        . IF $GET(INFO("DELETE DATA"))'=1 DO  QUIT:(+TMGRESULT<0)
        . . IF $$GET1^DIQ(TMGFILE,TMGIENS,TMGNUM)="" QUIT
        . . SET TMGRESULT="-1^DELETE DATA flag not set"
        . SET TMGFDA(TMGFILE,TMGIENS,TMGNUM)="@"
        . DO FILE^DIE("EK","TMGFDA","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) DO  QUIT
        . . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        SET DUZ(0)=TMGSAVAC
        ;
DELVALDN QUIT TMGRESULT
        ;
        ;
FLDHASDATA(FILENUM,FLD,SHOWPG) ;
        ;"Purpose: To scan a file and see IF any records contain data for field FLD
        ;"Input: FILENUM -- FILE NUMBER IF file (or subfile) to be investigated
        ;"       FLD -- the field number to check.
        ;"       SHOWPG -- OPTIONAL.  Default=0.  If 1, then progress bar shown.
        ;"Results: 1=At least 1 field does has data.  0=No fields have data, or there was a problem.
        ;
        NEW RESULT SET RESULT=0
        NEW CBFN SET CBFN="QUIT:(TMGVAL="""")  SET (ABORT,RESULT)=1"
        NEW PGFN SET PGFN=""
        NEW TMGSTART SET TMGSTART=$H
        IF $GET(SHOWPG)=1 DO
        . SET PGFN="DO PROGBAR^TMGUSRI2(TMGIEN,TMGFNAME,TMGMIN,TMGMAX,70,TMGSTART)"
        DO SCANFLD^TMGFMUT2(.FILENUM,.FLD,CBFN,PGFN)
FHDDN   QUIT RESULT
        ;
ADDFIELD(TMGINFO,TMGOUT) ;"API for Add aspect of Fileman MODIFY FILE.  See TMGDICATT for input formatting
        QUIT $$ADDFIELD^TMGDICATT(.TMGINFO,.TMGOUT)
        ;
DELFIELD(TMGINFO,TMGOUT)  ;"API for Delete aspect of Fileman MODIFY FILE.  See TMGDICATT for input formatting
        QUIT $$DELFIELD^TMGDICATT(.TMGINFO,.TMGOUT)
        ;
EDITFLD(TMGINFO,TMGOUT)   ;"API for Edit aspect of Fileman MODIFY FILE.  See TMGDICATT for input formatting
        QUIT $$EDITFLD^TMGDICATT(.TMGINFO,.TMGOUT)
        ;
