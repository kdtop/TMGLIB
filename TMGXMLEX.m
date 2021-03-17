TMGXMLEX ;TMG/kst/XML Exporter ;10/26/14, 6/9/17
         ;;1.0;TMG-LIB;**1**;07/12/05
 ;
 ;"TMG XML EXPORT FUNCTION
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
 ;"EXPORT  -- To ask for parameters, select output, and do actual export
 ;
 ;"NOTE: for information about expected formats etc, see TMGXMLE2
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"Dependencies   (duplicates shown in parenthesies)
 ;"TMGXML*, TMGDBAP*, TMGDEBUG, TMGUSRIF, TMGSTUTL, TMGMISC, TMGIOUTL, XLFSTR
 ;"TMGKERNL
 ;"
 ;"=======================================================================
 ;"=======================================================================
EXPORT  ;
        ;"Purpose: To ask for parameters, select output, and do actual export
        NEW XMLARRAY
        NEW REFARR SET REFARR=$NAME(XMLARRAY)
        NEW FILENAME,ERRFOUND
        ;
        IF $$UI^TMGXMLUI(REFARR)=0 GOTO EXDN
        ;
        IF (1=0) DO  IF FILENAME="" DO  GOTO EXDN
        . WRITE "Please select an output file for the XML export",!
        . SET FILENAME=$$GETFNAME^TMGIOUTL()
        . ;"Here I need to select IO channel
        . IF FILENAME="" DO  QUIT
        . . DO SHOWERR^TMGDEBU2(.ERRFOUND,"No file selected, so aborting.")
        . SET %ZIS("HFSNAME")=FILENAME
        . SET %ZIS="Q" ;"queing allowed
        . SET %ZIS("HFSMODE")="W"  ;"WRITE MODE
        . SET IOP="HFS"
        ELSE  DO
        . WRITE "Select device to output XML data to.",!
        . WRITE "HFS (i.e. Host File System) will allow output to a file.",!
        . WRITE "(A file name will be asked after HFS is chosen)."
        . SET %ZIS("A")="Enter Output Device: "
        . SET %ZIS("B")="HFS"
        ;
        DO ^%ZIS  ;"standard device call
        IF POP DO  GOTO EXDN
        . DO SHOWERR^TMGDEBU2(.ERRFOUND,"Error opening output file.  Aborting.")
        USE IO
        ;
        DO WTXMLOUT^TMGXMLE2(REFARR,,,1)
        DO ^%ZISC ;" Close the output device
        ;
        WRITE !,"(Data written to ouput file)",!
        ;
EXDN    KILL TMGXDEBUG,ERRFOUND
        WRITE !,"Leaving XML Exporter.  Goodbye.",!
        QUIT
        ;
EXPFILE  ;"Export records of Fileman file to HFS, with one file per record
        WRITE !,"This utility will export all records of a file to a ",!
        WRITE "Host File System (HFS) file, one file per record.",!
        WRITE !,"Ready to proceed" SET %=1 DO YN^DICN WRITE ! 
        IF %'=1 QUIT
        NEW FPATH
        WRITE "Enter output path on the HFS (e.g. '/tmp'): " READ FPATH:$GET(DTIME,3600),!
        IF $EXTRACT(FPATH,$LENGTH(FPATH))'="/" SET FPATH=FPATH_"/"
        IF $$ISDIR^TMGKERNL(FPATH)'=1 DO  GOTO EXPFILE
        . WRITE !,"Sorry, that doesn't appear to be a valid directory on the HFS, try again!",!
        NEW X,Y,DIC SET DIC=1,DIC(0)="MAEQ",DIC("A")="Select Fileman FILE for export: " 
        DO ^DIC WRITE ! 
        IF Y'>0 DO  GOTO EXPFILE 
        . WRITE !,"No Fileman file selected for ouput.  Please try again!",!
        NEW FNUM SET FNUM=+Y
        NEW FNAME SET FNAME=$TRANSLATE($PIECE(Y,"^",2)," ","_")
        NEW FREF SET FREF=$GET(^DIC(FNUM,0,"GL")) IF FREF="" DO  QUIT
        . WRITE "Unable to obtain global storage location for file.  Aborting",!
        SET FREF=$$CREF^DILF(FREF)
        NEW POP SET POP=0
        NEW IEN SET IEN=0
        FOR  SET IEN=$ORDER(@FREF@(IEN)) QUIT:(IEN'>0)!POP  DO
        . NEW TMGARRAY
        . SET TMGARRAY(FNUM,"TEMPLATE","*")=""
        . ;"SET TMGARRAY("FLAGS","b")=""  ;b=output fields even if empty.  
        . SET TMGARRAY("FLAGS","i")=""    ;i=indent for human reading
        . SET TMGARRAY(FNUM,IEN)=""
        . DO XPNDPTRS^TMGXMLUI("TMGARRAY")
        . NEW RECNAME SET RECNAME=$$GET1^DIQ(FNUM,IEN_",",.01)
        . ZWR TMGARRAY
        . NEW OUTFNAME SET OUTFNAME=$TRANSLATE(FNAME_"--"_RECNAME_".xml"," ","_")
        . WRITE "WOULD OUTPUT TO: ",OUTFNAME,!
        . DO OPEN^%ZISH("TMGXML",FPATH,OUTFNAME,"W") 
        . QUIT:POP
        . USE IO
        . DO WTXMLOUT^TMGXMLE2("TMGARRAY",,,1)
        . DO CLOSE^%ZISH("TMGXML")
        QUIT
        ;
