TMGSIPH1 ;TMG/kst/SIPHON PROGRAM, FOR TRANSFERRING VISTA INSTANCES ;11/27/09, 2/2/14
         ;;1.0;TMG-LIB;**1**;11/27/09
 ;
 ;"TMG SIPHON PROGRAM, FOR TRANSFERRING VISTA INSTANCE
 ;"Especially functions for working with the data dictionaries.
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
 ;"COMPALLD(JNUM) --ask user for file name and compare data dictionaries.
 ;"DDOK(JNUM,FILENUM) --check that data dictionary is ready, interacting with user as needed
 ;"PREPDD(JNUM,FILENUM) --Ensure the data dictonary is ready for the local client
 ;"COMPDD(JNUM,FILENUM,ARRAY) --compare data dictionary from Remote to local machine.
 ;"PROCESSDIFF(FILENUM,ARRAY) -- take array of differences (as created by COMPDD) and see IF user wants to copy remote changes to local machine.
 ;"HASFLDMISS(ARRAY) -- determine IF file has fields missing in local machine.
 ;"ADDFLDMISSING(ARRAY) --allow user to pick filed to add to local data dictionary.
 ;"ADD1FLD(FILENUM,FLD,ARRAY) --add all the nodes for file (or subfile) field to local data dictionary.
 ;"VFLDMISSING(ARRAY) --display fields missing in local machine.
 ;"GETMISFLD(ARRAY,MISFLDS) --display fields missing in local machine.
 ;"VIEW1FLDMISSING(FILENUM,FLD,ARRAY) --show the data for 1 field to be displayed.
 ;"HASWMISSING(ARRAY) -- determine IF there are any Nodes missing in local machine.
 ;"VIEWMISSING(ARRAY) -- display Nodes missing in local machine.
 ;"ADDMISSING(ARRAY)  -- add remote changes into this machine, IF wanted.
 ;"HASDIFF(ARRAY)  -- determine IF there are values that differ between remote and local VistA
 ;"VIEWDIFF(ARRAY) -- display values that differ between remote and local VistA
 ;"RSLVDIFF(ARRAY) -- allow storing values that differ between remote and local VistA
 ;"SETPTOUT(FILENUM) --set up an easy to use array of potential pointers out from a file.
 ;"SETALLPTO  -- To cycle through ALL files and call SETPTOUT for each file.
 ;"REAL1PTOUT(FILENUM,IEN,TALLY) --compare 1 record in the specified file that has been downloaded from the
 ;"         server, but not yet processed, and look for actual pointers out.
 ;"         If pointers out refer to records already gotten from server, then pointer is
 ;"         fixed immediately.  Otherwise pointer is added to list of fixes needed.
 ;"REALPTOUT(FILENUM) --  DEPRECIATED --compare all recorda in the specified file and look for actual pointers out.
 ;"PREPXREF(JNUM,FILENUM)  -- ask the server to pepair organized cross references.

 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"TMGKERN2, TMGUSRIF
 ;"=======================================================================
 ;
COMPALLD(JNUM) ;
        ;"Purpose: ask user for file name and compare data dictionaries.
        ;"Input: JNUM -- The job number of the background client process
        ;"
        NEW X,Y,DIC,ARRAY
        SET DIC=1,DIC(0)="MAEQ"
LCAD    DO ^DIC WRITE !
        IF +Y'>0 QUIT
        DO COMPDD(JNUM,+Y,.ARRAY)
        DO PROCESSDIFF(+Y,.ARRAY)
        ;"GOTO LCAD
        QUIT
 ;
 ;
DDOK(JNUM,FILENUM) ;
        ;"Purpose: To check that data dictionary is ready, interacting with user as needed
        ;"Input: JNUM -- The job number of the background client process
        ;"       FILENUM -- The file number to work on, or subfilenumber{parentfilenumber{grandparent...
        ;"Results : 1 IF DD is ready.  -1 IF user aborted.
        ;"NOTE: globally-scoped var TMGABORT may be SET to 1 to cause drop back to main menu.
        NEW DDOK SET DDOK=0
        SET FILENUM=+$GET(FILENUM) ;"if subfile, strip parent filenumber
        FOR  DO  QUIT:(DDOK'=0)!($GET(TMGABORT)=1)
        . SET DDOK=+$GET(^TMG("TMGSIPH","DD",FILENUM,"DIFF"))
        . QUIT:(DDOK=1)
        . ;"WRITE "Before records can be transferred from the server, the local data",!
        . ;"WRITE "dictionary must be made compatible.  Must work on this now.",!
        . ;"DO PRESS2GO^TMGUSRI2 ;"will SET global-scope var TMGPTCABORT IF aborted.
        . IF $GET(TMGPTCABORT)=1 SET DDOK=-1,TMGABORT=1 QUIT
        . SET DDOK=$$PREPDD(JNUM,FILENUM)
        QUIT DDOK
 ;
 ;
PREPDD(JNUM,FILENUM) ;
        ;"Purpose: Ensure the data dictonary is ready for the local client
        ;"Input: JNUM -- The job number of the background client process
        ;"       FILENUM -- The file number to work on
        ;"Results : 1 IF DD is ready.  0 or -1 IF user aborted.
        ;"NOTE: globally-scoped var TMGABORT may be SET to 1 to cause drop back to main menu.
        NEW ARRAY,RESULT
        SET RESULT=$GET(^TMG("TMGSIPH","DD",FILENUM,"DIFF"))
        IF RESULT=1 GOTO PDDN  ;"Signal that DD has been resolved
        DO COMPDD(JNUM,FILENUM,.ARRAY)
        IF $DATA(ARRAY) DO
        . DO PROCESSDIFF(FILENUM,.ARRAY)
        . SET RESULT=+$GET(^TMG("TMGSIPH","DD",FILENUM,"DIFF")) ;"Signal that DD has been looked at
        ELSE  DO
        . SET ^TMG("TMGSIPH","DD",FILENUM,"DIFF")=1 ;"Signal that DD has been looked at
        . SET RESULT=1
PDDN    QUIT RESULT
 ;
 ;
COMPDD(JNUM,FILENUM,ARRAY) ;
        ;"Purpose: To compare data dictionary from Remote to local machine.
        ;"Input: JNUM -- The job number of the background client process
        ;"       FILENUM -- The file number to compare.
        ;"       ARRAY -- Pass by REFERENCE, an OUT PARAMETER.
        ;"          ARRAY("MISSING NODE",NodeStr)=RemoteValue
        ;"          ARRAY("DIFF VALUE",NodeStr,"L")=LocalValue
        ;"          ARRAY("DIFF VALUE",NodeStr,"R")=RemoteValue
        ;"Results: none
        NEW QUERY,ERROR,RESULT,REPLY
        KILL ARRAY
        SET FILENUM=+$GET(FILENUM)
        SET ARRAY("FILE")=FILENUM
        SET QUERY="GET DD|"_FILENUM
        DO MSGCLIENT^TMGKERN2(JNUM,QUERY,.REPLY,.ERROR,5)
        IF $DATA(ERROR) WRITE ERROR,! GOTO CDDD
        NEW TMGI SET TMGI=1
        NEW REF,VALUE
        FOR  DO  SET TMGI=TMGI+2 QUIT:(REF="")
        . SET REF=$GET(REPLY(TMGI)) QUIT:(REF="")
        . SET REF=$EXTRACT(REF,1,$LENGTH(REF)-1) ;"Cleave terminal "="
        . SET VALUE=$GET(REPLY(TMGI+1))
        . SET VALUE=$EXTRACT(VALUE,2,$LENGTH(VALUE))
        . IF $DATA(@REF)=0 DO  QUIT
        . . SET ARRAY("MISSING NODE",REF)=VALUE
        . IF $GET(@REF)'=VALUE DO  QUIT
        . . SET ARRAY("DIFF VALUE",REF,"L")=$GET(@REF)
        . . SET ARRAY("DIFF VALUE",REF,"R")=VALUE
CDDD    QUIT
 ;
 ;
PROCESSDIFF(FILENUM,ARRAY) ;
        ;"Purpose: To take array of differences (as created by COMPDD) and
        ;"      see IF user wants to copy remote changes to local machine.
        ;"Input -- FILENUM -- The Fileman file number
        ;"         ARRAY -- Pass by REFERENCE.  As created by COMPDD
        ;"Result: None
        ;"NOTE: globally-scoped var TMGABORT may be SET to 1 to cause drop back to main menu.
        ;
        NEW MENU,USRSLCT,IDX,%
        NEW FNAME SET FNAME=$PIECE($GET(^DIC(FILENUM,0)),"^",1)
CD1     KILL MENU
        SET IDX=1
        SET MENU(0)="Pick Option for Comparing Differences in File #"_$GET(ARRAY("FILE"))_" "_FNAME
        IF $$HASFLDMISS(.ARRAY) DO
        . SET MENU(IDX)="View missing local FIELDS"_$CHAR(9)_"ViewFldMissing" SET IDX=IDX+1
        . SET MENU(IDX)="Add missing local FIELDS"_$CHAR(9)_"AddFldMissing" SET IDX=IDX+1
        IF $$HASWMISSING(.ARRAY) DO
        . SET MENU(IDX)="View missing local nodes"_$CHAR(9)_"ViewMissing" SET IDX=IDX+1
        . SET MENU(IDX)="Add missing local nodes"_$CHAR(9)_"AddMissing" SET IDX=IDX+1
        IF $$HASDIFF(.ARRAY) DO
        . SET MENU(IDX)="View conflicts between remote and local VistA"_$CHAR(9)_"ViewDiff" SET IDX=IDX+1
        . SET MENU(IDX)="Resolve conflicts between remote and local VistA"_$CHAR(9)_"ResolveDiff" SET IDX=IDX+1
        IF IDX>1 DO
        . SET MENU(IDX)="Launch local data dictionary browser"_$CHAR(9)_"VPEDD" SET IDX=IDX+1
        ELSE  DO  GOTO CDDN2
        . SET ^TMG("TMGSIPH","DD",FILENUM,"DIFF")=1
        . ;"WRITE "Local Data Dictionary is OK.  Nothing to be done.",!
        . ;"DO PRESS2GO^TMGUSRI2
        SET MENU(IDX)="DONE with fixing differences"_$CHAR(9)_"Done" SET IDX=IDX+1
        SET MENU(IDX)="ABORT entire process"_$CHAR(9)_"Abort" SET IDX=IDX+1
        ;
        WRITE #
        WRITE "********************************************************************",!
        WRITE "File name: "_FNAME,!
        WRITE "Before records can be transferred from the server, the local data",!
        WRITE "dictionary must be made compatible.  Please work on this now.",!
        WRITE "********************************************************************",!
        SET USRSLCT=$$MENU^TMGUSRI2(.MENU,"^")
        IF USRSLCT="^" GOTO CDDONE
        IF USRSLCT=0 SET USRSLCT=""
        ;
        IF USRSLCT="ViewFldMissing" DO VFLDMISSING(.ARRAY) GOTO CD1
        IF USRSLCT="AddFldMissing" DO ADDFLDMISSING(.ARRAY) GOTO CD1
        IF USRSLCT="ViewMissing" DO VIEWMISSING(.ARRAY) GOTO CD1
        IF USRSLCT="AddMissing" DO ADDMISSING(.ARRAY) GOTO CD1
        IF USRSLCT="ViewDiff" DO VIEWDIFF(.ARRAY) GOTO CD1
        IF USRSLCT="ResolveDiff" DO RSLVDIFF(.ARRAY) GOTO CD1
        IF USRSLCT="VPEDD" DO ^%ZVEMD GOTO CD1
        IF USRSLCT="Done" SET %=1 GOTO CDDN1
        IF USRSLCT="Abort" SET TMGABORT=1 GOTO CDDN2
        ;
CDDONE  SET %=2
        WRITE "Have all conflicts for this file been resolved (^ to abort)"
        DO YN^DICN WRITE !
CDDN1        IF %=1 SET ^TMG("TMGSIPH","DD",FILENUM,"DIFF")=1 ;"Signal that DD has been processed
        ELSE  IF %=-1 SET ^TMG("TMGSIPH","DD",FILENUM,"DIFF")=-1 ;"Signal of abort
        ELSE  SET ^TMG("TMGSIPH","DD",FILENUM,"DIFF")=0 ;"Signal that DD needs processing
CDDN2   QUIT
 ;
 ;
HASFLDMISS(ARRAY) ;
        ;"Purpose: to determine IF file has fields missing in local machine.
        ;"Input -- ARRAY -- Pass by REFERENCE.  As created by COMPDD
        ;"          ARRAY("MISSING NODE",NodeStr)=RemoteValue
        ;"          ARRAY("MISSING NODE",NodeStr)=RemoteValue
        ;"Results: 1 IF has missing fields, 0 IF not
        NEW MISFLDS
        DO GETMISFLD(.ARRAY,.MISFLDS)
        NEW FOUND SET FOUND=0
        NEW FILENUM SET FILENUM=0
        FOR  SET FILENUM=$ORDER(MISFLDS(FILENUM)) QUIT:(FILENUM'>0)!FOUND  DO
        . NEW FLDNAME SET FLDNAME=""
        . FOR  SET FLDNAME=$ORDER(MISFLDS(FILENUM,FLDNAME)) QUIT:(FLDNAME="")!FOUND  DO
        . . SET FOUND=1
        QUIT (FOUND=1)
 ;
 ;
ADDFLDMISSING(ARRAY) ;
        ;"Purpose: To allow user to pick filed to add to local data dictionary.
        ;"Input -- ARRAY -- Pass by REFERENCE.  As created by COMPDD
        ;"          ARRAY("MISSING NODE",NodeStr)=RemoteValue
        ;"          ARRAY("MISSING NODE",NodeStr)=RemoteValue
        NEW MISFLDS
        DO GETMISFLD(.ARRAY,.MISFLDS)
        NEW ABORT SET ABORT=0
        NEW FILENUM SET FILENUM=0
        FOR  SET FILENUM=$ORDER(MISFLDS(FILENUM)) QUIT:(FILENUM'>0)!ABORT  DO
        . NEW MENU,USRSLCT
        . SET MENU(0)="Pick FIELD to add to local data dictionary, File #"_FILENUM
        . NEW I SET I=1
        . NEW FLDNAME SET FLDNAME=""
        . FOR  SET FLDNAME=$ORDER(MISFLDS(FILENUM,FLDNAME)) QUIT:(FLDNAME="")  DO
        . . NEW FLD SET FLD=$GET(MISFLDS(FILENUM,FLDNAME))
        . . SET MENU(I)="Field "_FLDNAME_" ("_FLD_")"_$CHAR(9)_FLD
        . . SET I=I+1
        . NEW DONE SET DONE=0
        . FOR  DO  QUIT:DONE
        . . IF $ORDER(MENU(0))="" SET DONE=1 QUIT
        . . WRITE #
        . . SET USRSLCT=$$MENU^TMGUSRI2(.MENU,"^")
        . . IF USRSLCT="^" SET (DONE,ABORT)=1 QUIT
        . . IF USRSLCT="" SET DONE=1 QUIT
        . . IF +USRSLCT>0 IF $$ADD1FLD(FILENUM,+USRSLCT,.ARRAY) DO
        . . . NEW J SET J=0
        . . . FOR  SET J=$ORDER(MENU(J)) QUIT:(J="")  DO
        . . . . IF MENU(J)[($CHAR(9)_+USRSLCT) KILL MENU(J)
        QUIT
 ;
 ;
ADD1FLD(FILENUM,FLD,ARRAY) ;
        ;"Purpose: To add all the nodes for file (or subfile) field to local data dictionary.
        ;"Input: FILENUM -- The Fileman file
        ;"       FLD -- The fieldman field to add
        ;"       ARRAY -- Pass by REFERENCE.  As created by COMPDD
        ;"          ARRAY("MISSING NODE",NodeStr)=RemoteValue
        ;"          ARRAY("MISSING NODE",NodeStr)=RemoteValue
        ;"Result: 1 IF added, 0 IF not
        NEW RESULT SET RESULT=0
        NEW REF SET REF=""
        FOR  SET REF=$ORDER(ARRAY("MISSING NODE",REF)) QUIT:(REF="")  DO
        . IF $QSUBSCRIPT(REF,0)'="^DD" QUIT
        . IF $QSUBSCRIPT(REF,1)'=FILENUM QUIT
        . NEW SUB2 SET SUB2=$QSUBSCRIPT(REF,2)
        . NEW SUB3 SET SUB3=$QSUBSCRIPT(REF,3)
        . NEW LASTSUB SET LASTSUB=$QSUBSCRIPT(REF,$QLENGTH(REF))
        . NEW VALUE SET VALUE=$GET(ARRAY("MISSING NODE",REF))
        . NEW ADD SET ADD=0
        . IF (SUB2'=+SUB2),(LASTSUB=FLD) SET ADD=1  ;"Handle xrefs
        . IF SUB2=FLD SET ADD=1
        . IF FLD="*" SET ADD=1
        . IF SUB2=0 DO
        . . NEW SUB3 SET SUB3=$QSUBSCRIPT(REF,3)
        . . IF (SUB3="ID"),(LASTSUB=FLD) SET ADD=1 ;"Write identifier nodes
        . . IF (SUB3="IX"),(LASTSUB=FLD) SET ADD=1 ;"Indexes
        . . IF (SUB3="PT"),(LASTSUB=FLD) SET ADD=1 ;"Pointers IN to file
        . . ELSE  DO
        . . . NEW TEMP SET TEMP=1 ;"Breakpoint to see what is NOT being handled.
        . IF ADD'=1 QUIT
        . IF SUB3=0,SUB2>0 DO
        . . NEW PT SET PT=+$PIECE(VALUE,"^",2)
        . . NEW SUBREF SET SUBREF=$NAME(^DD(PT,0))
        . . IF $DATA(ARRAY("MISSING NODE",SUBREF)) IF $$ADD1FLD(PT,"*",.ARRAY)
        . SET @REF=VALUE
        . WRITE "ADDED ",REF,!
        . KILL ARRAY("MISSING NODE",REF)
        . SET RESULT=1
        WRITE !,"Done.",!
        DO PRESS2GO^TMGUSRI2
        QUIT RESULT
 ;
 ;
VFLDMISSING(ARRAY) ;
        ;"Purpose: to display fields missing in local machine.
        ;"Input -- ARRAY -- Pass by REFERENCE.  As created by COMPDD
        ;"          ARRAY("MISSING NODE",NodeStr)=RemoteValue
        ;"          ARRAY("MISSING NODE",NodeStr)=RemoteValue
        NEW NAME,FOUND
        WRITE "The following FIELDS are present on the remote VistA, but",!
        WRITE "are missing from the local machine.",!,!
        NEW MISFLDS
        DO GETMISFLD(.ARRAY,.MISFLDS)
        NEW FOUND SET FOUND=0
        NEW ABORT SET ABORT=0
        NEW FILENUM SET FILENUM=0
        FOR  SET FILENUM=$ORDER(MISFLDS(FILENUM)) QUIT:(FILENUM'>0)!ABORT  DO
        . NEW MENU,USRSLCT
        . SET MENU(0)="Pick FIELD to examine in File #"_FILENUM
        . NEW I SET I=1
        . NEW FLDNAME SET FLDNAME=""
        . FOR  SET FLDNAME=$ORDER(MISFLDS(FILENUM,FLDNAME)) QUIT:(FLDNAME="")  DO
        . . NEW FLD SET FLD=$GET(MISFLDS(FILENUM,FLDNAME))
        . . SET MENU(I)="Field "_FLDNAME_" ("_FLD_")"_$CHAR(9)_FLD
        . . SET I=I+1
        . IF I>1 SET FOUND=1
        . ELSE  QUIT
        . NEW DONE SET DONE=0
        . FOR  DO  QUIT:DONE
        . . WRITE #
        . . SET USRSLCT=$$MENU^TMGUSRI2(.MENU,"^")
        . . IF USRSLCT="^" SET (DONE,ABORT)=1 QUIT
        . . IF USRSLCT="" SET DONE=1 QUIT
        . . IF +USRSLCT>0 DO VIEW1FLDMISSING(FILENUM,+USRSLCT,.ARRAY)
        IF FOUND=0 DO
        . WRITE "<<None>>",!
        . DO PRESS2GO^TMGUSRI2
        QUIT
 ;
 ;
GETMISFLD(ARRAY,MISFLDS) ;
        ;"Purpose: to display fields missing in local machine.
        ;"Input -- ARRAY -- Pass by REFERENCE.  As created by COMPDD
        ;"          ARRAY("MISSING NODE",NodeStr)=RemoteValue
        ;"          ARRAY("MISSING NODE",NodeStr)=RemoteValue
        ;"        MISFLDS -- PASS BY REFERENCE, AN OUT PARAMETER.  Format:
        ;"                MISFLDS(FILENUM,FIELDNAME)=FieldNumber
        NEW REF,VALUE,FOUND
        NEW FLD,LASTFLD SET LASTFLD=""
        SET REF=""
        FOR  SET REF=$ORDER(ARRAY("MISSING NODE",REF)) QUIT:(REF="")  DO
        . IF $QSUBSCRIPT(REF,0)'="^DD" QUIT
        . SET FLD=$QSUBSCRIPT(REF,2)
        . QUIT:(FLD=LASTFLD)
        . IF $QSUBSCRIPT(REF,3)'=0 QUIT
        . SET LASTFLD=FLD
        . NEW FILENUM SET FILENUM=$QSUBSCRIPT(REF,1)
        . NEW FLDNAME SET FLDNAME=$PIECE($GET(ARRAY("MISSING NODE",REF)),"^",1)
        . QUIT:(FLDNAME="")
        . SET MISFLDS(FILENUM,FLDNAME)=FLD
        QUIT
 ;
 ;
VIEW1FLDMISSING(FILENUM,FLD,ARRAY) ;
        ;"Purpose: To show the data for 1 field to be displayed.
        ;"Input: FILENUM -- The Fileman file
        ;"       FLD -- The fieldman field to add
        ;"       ARRAY -- Pass by REFERENCE.  As created by COMPDD
        ;"          ARRAY("MISSING NODE",NodeStr)=RemoteValue
        ;"          ARRAY("MISSING NODE",NodeStr)=RemoteValue
        NEW LINECT SET LINECT=0
        SET NAME="",FOUND=0
        NEW REF SET REF=""
        FOR  SET REF=$ORDER(ARRAY("MISSING NODE",REF)) QUIT:(REF="")  DO
        . IF $QSUBSCRIPT(REF,0)'="^DD" QUIT
        . IF $QSUBSCRIPT(REF,1)'=FILENUM QUIT
        . NEW SUB2 SET SUB2=$QSUBSCRIPT(REF,2)
        . NEW LASTSUB SET LASTSUB=$QSUBSCRIPT(REF,$QLENGTH(REF))
        . NEW ADD SET ADD=0
        . IF (SUB2'=+SUB2),(LASTSUB=FLD) SET ADD=1  ;"Handle xrefs
        . IF SUB2=0 DO
        . . NEW SUB3 SET SUB3=$QSUBSCRIPT(REF,3)
        . . IF (SUB3="ID"),(LASTSUB=FLD) SET ADD=1 ;"Write identifier nodes
        . . IF (SUB3="IX"),(LASTSUB=FLD) SET ADD=1 ;"Indexes
        . . IF (SUB3="PT"),(LASTSUB=FLD) SET ADD=1 ;"Pointers IN to file
        . IF SUB2=FLD SET ADD=1
        . IF ADD'=1 QUIT
        . WRITE REF,"=",$GET(ARRAY("MISSING NODE",REF)),!
        . SET FOUND=1
        . SET LINECT=LINECT+1
        . IF LINECT=23 SET LINECT=0 DO PRESS2GO^TMGUSRI2
        WRITE !,"Done.",!
        IF FOUND=0 WRITE "<<NONE>>",!
        DO PRESS2GO^TMGUSRI2
        QUIT
 ;
 ;
HASWMISSING(ARRAY) ;
        ;"Purpose: to determine IF there are any Nodes missing in local machine.
        ;"Input -- ARRAY -- Pass by REFERENCE.  As created by COMPDD
        ;"          ARRAY("MISSING NODE",NodeStr)=RemoteValue
        ;"          ARRAY("MISSING NODE",NodeStr)=RemoteValue
        ;"Results: 1 IF has data, 0 IF not
        NEW REF,VALUE,FOUND
        NEW LINECT SET LINECT=0
        SET REF="",FOUND=0
        FOR  SET REF=$ORDER(ARRAY("MISSING NODE",REF)) QUIT:(REF="")  DO
        . SET FOUND=1
        QUIT (FOUND=1)
 ;
 ;
VIEWMISSING(ARRAY) ;
        ;"Purpose: to display Nodes missing in local machine.
        ;"Input -- ARRAY -- Pass by REFERENCE.  As created by COMPDD
        ;"          ARRAY("MISSING NODE",NodeStr)=RemoteValue
        ;"          ARRAY("MISSING NODE",NodeStr)=RemoteValue
        NEW REF,VALUE,FOUND
        WRITE "The following nodes are present on the remote VistA, but",!
        WRITE "are missing from the local machine.",!,!
        NEW LINECT SET LINECT=0
        SET REF="",FOUND=0
        FOR  SET REF=$ORDER(ARRAY("MISSING NODE",REF)) QUIT:(REF="")!($GET(TMGPTCABORT)=1)  DO
        . SET FOUND=1
        . WRITE REF,"=",$GET(ARRAY("MISSING NODE",REF)),!
        . SET LINECT=LINECT+1
        . IF LINECT=23 SET LINECT=0 DO PRESS2GO^TMGUSRI2
        IF FOUND=0 WRITE "<<NONE>>",!
        IF $GET(TMGPTCABORT)'=1 DO PRESS2GO^TMGUSRI2
        QUIT
        ;
ADDMISSING(ARRAY) ;
        ;"Purpose:  To add remote changes into this machine, IF wanted.
        ;"Input -- ARRAY -- Pass by REFERENCE.  As created by COMPDD
        NEW ASKARRAY,SELARRAY
        NEW REF SET REF=""
        FOR  SET REF=$ORDER(ARRAY("MISSING NODE",REF)) QUIT:(REF="")  DO
        . NEW VALUE SET VALUE=$GET(ARRAY("MISSING NODE",REF))
        . SET VALUE=$EXTRACT(VALUE,1,70-$LENGTH(REF))
        . SET ASKARRAY(REF_"="_VALUE)=REF
        NEW HDR SET HDR="Pick Nodes to be added to local data dictionary. <ESC><ESC> when done."
        DO SELECTR2^TMGUSRI3("ASKARRAY","SELARRAY",HDR)
        NEW TMGI SET TMGI=""
        FOR  SET TMGI=$ORDER(SELARRAY(TMGI)) QUIT:(TMGI="")  DO
        . SET REF=$GET(SELARRAY(TMGI))
        . NEW VALUE SET VALUE=$GET(ARRAY("MISSING NODE",REF))
        . SET @REF=VALUE
        . WRITE "ADDED ",REF,!
        . KILL ARRAY("MISSING NODE",REF)
        WRITE !,"Done.",!
        DO PRESS2GO^TMGUSRI2
        QUIT
        ;
HASDIFF(ARRAY) ;
        ;"Purpose: to determine IF there are values that differ between remote and local VistA
        ;"Input -- ARRAY -- Pass by REFERENCE.  As created by COMPDD
        ;"          ARRAY("DIFF VALUE",NodeStr,"L")=LocalValue
        ;"          ARRAY("DIFF VALUE",NodeStr,"R")=RemoteValue
        NEW REF,FOUND
        SET REF="",FOUND=0
        FOR  SET REF=$ORDER(ARRAY("DIFF VALUE",REF)) QUIT:(REF="")!(FOUND)  DO
        . SET FOUND=1
        QUIT (FOUND=1)
        ;
VIEWDIFF(ARRAY) ;
        ;"Purpose: to display values that differ between remote and local VistA
        ;"Input -- ARRAY -- Pass by REFERENCE.  As created by COMPDD
        ;"          ARRAY("DIFF VALUE",NodeStr,"L")=LocalValue
        ;"          ARRAY("DIFF VALUE",NodeStr,"R")=RemoteValue
        NEW REF,VALUE,FOUND
        WRITE "The following nodes DIFFER between remote and local VistAs",!,!
        SET REF="",FOUND=0
        FOR  SET REF=$ORDER(ARRAY("DIFF VALUE",REF)) QUIT:(REF="")  DO
        . SET FOUND=1
        . WRITE REF,!
        . WRITE " Local: ",$GET(ARRAY("DIFF VALUE",REF,"L")),!
        . WRITE " Remote:",$GET(ARRAY("DIFF VALUE",REF,"R")),!
        IF FOUND=0 WRITE "<<NONE>>",!
        DO PRESS2GO^TMGUSRI2
        QUIT
        ;
RSLVDIFF(ARRAY) ;
        ;"Purpose: To allow storing values that differ between remote and local VistA
        ;"Input -- ARRAY -- Pass by REFERENCE.  As created by COMPDD
        ;"          ARRAY("DIFF VALUE",NodeStr,"L")=LocalValue
        ;"          ARRAY("DIFF VALUE",NodeStr,"R")=RemoteValue
        NEW REF,VALUE,FOUND,%
        WRITE "The following nodes DIFFER between remote and local VistAs",!,!
        SET REF="",FOUND=0,%=2
        FOR  SET REF=$ORDER(ARRAY("DIFF VALUE",REF)) QUIT:(REF="")!(%=-1)  DO
        . SET FOUND=1
        . WRITE REF,!
        . WRITE " Local: ",$GET(ARRAY("DIFF VALUE",REF,"L")),!
        . WRITE " Remote:",$GET(ARRAY("DIFF VALUE",REF,"R")),!
        . SET %=2
        . WRITE "Overwrite LOCAL value with REMOTE" DO YN^DICN WRITE !
        . IF %=2 KILL ARRAY("DIFF VALUE",REF)
        . IF %'=1 QUIT
        . SET @REF=$GET(ARRAY("DIFF VALUE",REF,"R"))
        . WRITE " OVERWRITTEN",!
        . KILL ARRAY("DIFF VALUE",REF)
        IF FOUND=0 WRITE "<<NONE>>",!
        DO PRESS2GO^TMGUSRI2
        QUIT
 ;
 ;
SETPTOUT(FILENUM) ;
        ;"Purpose: To SET up an easy to use array of potential pointers out from a file.
        ;"Input: FILENUM-- the filenumber to evaluate
        ;"Output:  Data will be stored in ^TMG("TMGSIPH","DD",FILENUM,"PTR OUT",ONEREF,ENTRY)
        ;"    ; Note: ENTRY=DataPiece^PointedToFile^PointedToReference^IENDepth^[V]
        ;"    ; ONEREF will have multipe IEN entries IF IENDepth>1, e.g. '^SC(IEN,"S",IEN(2),1,IEN(3),"C")'
        ;"    ;        with order of IEN, IEN(2), IEN(3), ... etc.
        ;"Results: 1= success, -1=error
        ;
        NEW RESULT SET RESULT=-1
        IF +$GET(FILENUM)'=FILENUM GOTO SPODN
        NEW IENDEPTH SET IENDEPTH=1
        NEW ISSUBFIL SET ISSUBFIL=0
        NEW REF SET REF=$GET(^DIC(FILENUM,0,"GL"))
        IF (REF=""),$DATA(^DD(FILENUM,0,"UP")) DO
        . SET REF=$$GETGL^TMGFMUT2(FILENUM,.IENDEPTH)
        . SET ISSUBFIL=1
        IF REF="" GOTO SPODN
        KILL ^TMG("TMGSIPH","DD",FILENUM,"PTR OUT")  ;"If FILENUM is subfile, nothing to kill...
        NEW FLD SET FLD=0
        FOR  SET FLD=$ORDER(^DD(FILENUM,FLD)) QUIT:(+FLD'>0)  DO
        . NEW ZNODE SET ZNODE=$GET(^DD(FILENUM,FLD,0))
        . NEW FLDTYPE SET FLDTYPE=$PIECE(ZNODE,"^",2)
        . IF (FLDTYPE'["P")&(FLDTYPE'["V")&(+FLDTYPE'>0) QUIT
        . IF $PIECE($GET(^DD(+FLDTYPE,.01,0)),"^",2)["W" QUIT  ;"WP fields look like subfiles, but really aren't
        . NEW LOC SET LOC=$PIECE(ZNODE,"^",4)
        . NEW NODE SET NODE=$PIECE(LOC,";",1)
        . NEW PCE SET PCE=+$PIECE(LOC,";",2)
        . IF +NODE'=NODE SET NODE=""""_NODE_""""
        . NEW ONEREF,SUBSCR
        . SET SUBSCR=$SELECT((IENDEPTH>1):"("_IENDEPTH_")",1:"")
        . SET ONEREF=REF_"IEN"_SUBSCR_","_NODE_")"
        . NEW P2FILE
        . NEW VREC SET VREC=0
        . NEW DONE SET DONE=0
        . FOR  DO  QUIT:(DONE=1)
        . . NEW ISVIRT SET ISVIRT=""
        . . NEW P2REF
        . . SET P2FILE=0
        . . IF FLDTYPE["V" DO  QUIT:(DONE=1)
        . . . SET VREC=+$ORDER(^DD(FILENUM,FLD,"V",VREC))
        . . . IF VREC=0 SET DONE=1 QUIT
        . . . SET P2FILE=+$GET(^DD(FILENUM,FLD,"V",VREC,0))
        . . . SET ISVIRT="V"
        . . . SET P2REF=$PIECE($GET(^DIC(P2FILE,0,"GL")),"^",2)
        . . ELSE  DO  QUIT:(P2FILE=0)
        . . . IF +FLDTYPE>0 IF $$SETPTOUT(+FLDTYPE) SET DONE=1 QUIT  ;"Handle subfile.
        . . . SET P2FILE=+$PIECE(FLDTYPE,"P",2)
        . . . SET P2REF=$PIECE(ZNODE,"^",3)
        . . . SET DONE=1
        . . NEW ENTRY SET ENTRY=PCE_"^"_P2FILE_"^"_P2REF_"^"_IENDEPTH_"^"_ISVIRT
        . . SET ^TMG("TMGSIPH","DD",$$TOPFILEN^TMGFMUT2(FILENUM),"PTR OUT",ONEREF,ENTRY)=""
        . . SET ^TMG("TMGSIPH","DD",FILENUM,"PTR OUT",ONEREF,ENTRY)=""  ;"Not sure which is used throughout, so store both ways.
        SET RESULT=1
SPODN   QUIT RESULT
        ;
SETALLPTO ;" Set All Pointers Out
        ;"Purpose: To cycle through ALL files and call SETPTOUT for each file.
        ;"Input: None
        ;"Output: Data will be stored...
        ;"Results: None
        NEW FILENUM SET FILENUM=0
        NEW STIME SET STIME=$H
        NEW FILEMAXCT SET FILEMAXCT=0
        FOR  SET FILENUM=$ORDER(^DD(FILENUM)) QUIT:(+FILENUM'>0)  SET FILEMAXCT=FILEMAXCT+1
        NEW FILECT SET FILECT=0
        SET FILENUM=0
        FOR  SET FILENUM=$ORDER(^DIC(FILENUM)) QUIT:(+FILENUM'>0)  DO
        . SET FILECT=FILECT+1
        . NEW FILENAME SET FILENAME=$PIECE($GET(^DIC(FILENUM,0)),"^",1)
        . DO PROGBAR^TMGUSRI2(FILECT,"Progress: "_FILENAME,0,FILEMAXCT,70,STIME)
        . IF $DATA(^TMG("TMGSIPH","DD",FILENUM,"PTR OUT")) QUIT
        . IF $$SETPTOUT(FILENUM) ;"ignore result
        ;"Now handle subfiles.
        SET FILENUM=0
        FOR  SET FILENUM=$ORDER(^DD(FILENUM)) QUIT:(+FILENUM'>0)  DO
        . SET FILECT=FILECT+1
        . DO PROGBAR^TMGUSRI2(FILECT,"Progress: "_FILENUM,0,FILEMAXCT,70,STIME)
        . IF $DATA(^DIC(FILENUM)) QUIT
        . IF $$SETPTOUT(FILENUM) ;"ignore result
        WRITE !,FILECT," Files processed.",!
        DO PRESS2GO^TMGUSRI2
        QUIT
        ;
REAL1PTOUT(FILENUM,IEN,TALLY) ;
        ;"Purpose: to compare 1 record in the specified file that has been downloaded from the
        ;"         server, but not yet processed, and look for actual pointers out.
        ;"         If pointers out refer to records already gotten from server, then pointer is
        ;"         fixed immediately.  Otherwise pointer is added to list of fixes needed.
        ;"Input: FILENUM -- the Fileman file (or subfile) number to look at
        ;"       IEN -- The record number to look at.
        ;"              If FILENUM is a subfile, pass IENS info in IEN (e.g. '3,2345,')
        ;"       TALLY -- OPTIONAL.  PASS BY REFERENCE.  An array to keep progress stats.  Format:
        ;"                 TALLY("ALREADY LOCAL FOUND")=#
        ;"                 TALLY("FIXED LINK TO ALREADY-DOWNLOADED RECORD")=#
        ;"                 TALLY(FILENUM,"NEW REC NEEDED")=#
        ;"Output: Sets global records to show unresolved pointers:
        ;"      ^TMG("TMGSIPH","NEEDED RECORDS","PTOUT",FILENUM,RemotePointer,ReferToNodeToBeCorrected,INFO)=""
        ;"              INFO=DataPiece^PointedToFile^PointedToReference^IENDepth^[V]
        ;"Result: 1 = OK, -1 = error
        ;"NOTE:
        ;"  Uses data from ^TMG("TMGSIPH","DD",FILENUM,"PTR OUT",ONEREF,ENTRY)
        ;"  ENTRY=DataPiece^PointedToFile^PointedToReference^IENDepth^[V]
        ;"  ONEREF will have multipe IEN entries IF IENDepth>1, e.g. '^SC(IEN,"S",IEN(2),1,IEN(3),"C")'
        ;"    with order of IEN, IEN(2), IEN(3), ... etc.
        ;
        NEW RESULT SET RESULT=-1
        SET FILENUM=+$GET(FILENUM)
        IF FILENUM'>0 GOTO RP1ODN
        IF $DATA(^TMG("TMGSIPH","DOWNLOADED",FILENUM,IEN)) DO  GOTO RP1ODN ;"Already processed
        . SET RESULT=1
        . SET TALLY("ALREADY LOCAL FOUND")=+$GET(TALLY("ALREADY LOCAL FOUND"))+1
        IF +$GET(^TMG("TMGSIPH","DD",FILENUM))=0 DO
        . IF $$SETPTOUT(FILENUM) SET ^TMG("TMGSIPH","DD",FILENUM)=1
        NEW SAVIENS SET SAVIENS=IEN
        NEW REF SET REF=""
        FOR  SET REF=$ORDER(^TMG("TMGSIPH","DD",FILENUM,"PTR OUT",REF)) QUIT:(REF="")  DO
        . NEW INFO SET INFO=""
        . FOR  SET INFO=$ORDER(^TMG("TMGSIPH","DD",FILENUM,"PTR OUT",REF,INFO)) QUIT:(INFO="")  DO
        . . NEW PCE SET PCE=+INFO
        . . NEW P2FILE SET P2FILE=$PIECE(INFO,"^",2)
        . . NEW P2REF SET P2REF=$PIECE(INFO,"^",3)
        . . NEW IENDEPTH SET IENDEPTH=$PIECE(INFO,"^",4)
        . . NEW ISVIRT SET ISVIRT=($PIECE(INFO,"^",5)="V")
        . . NEW TEMP SET TEMP=+IEN KILL IEN SET IEN=TEMP  ;"kill subnodes.  Prob won't work with sub-sub files.
        . . NEW OKCOMBO
        . . FOR  DO  QUIT:(OKCOMBO=0)
        . . . SET OKCOMBO=$$IENCOMBO^TMGFMUT2(REF,IENDEPTH,.IEN) ;"Sets up IEN(n).. needed for @REF
        . . . QUIT:(OKCOMBO=0)
        . . . NEW RPTR SET RPTR=$PIECE($GET(@REF),"^",PCE)
        . . . IF ISVIRT,$PIECE(RPTR,";",2)'=P2REF QUIT  ;"Loop to handle PTR with different INFO entry (V-Ptrs stored as IEN;OREF)
        . . . SET RPTR=+RPTR QUIT:(RPTR'>0)
        . . . NEW LPTR SET LPTR=+$GET(^TMG("TMGSIPH","PT XLAT",P2FILE,RPTR))
        . . . IF (LPTR>0) DO  QUIT
        . . . . IF LPTR'=RPTR SET $PIECE(@REF,"^",PCE)=LPTR
        . . . . SET TALLY("FIXED LINK TO ALREADY-DOWNLOADED RECORD")=1+$GET(TALLY("FIXED LINK TO ALREADY-DOWNLOADED RECORD"))
        . . . ;"SET ^TMG("TMGSIPH","UNRESOLVED",FILENUM,$NAME(@REF),INFO)=RPTR
        . . . SET ^TMG("TMGSIPH","NEEDED RECORDS","PTOUT",P2FILE,RPTR,$NAME(@REF),INFO)=""
        . . . SET TALLY(FILENUM,"NEW REC NEEDED")=+$GET(TALLY(FILENUM,"NEW REC NEEDED"))+1
        . . KILL IEN("DONE"),IEN("INIT")
        SET RESULT=1
RP1ODN  QUIT RESULT
 ;
 ;
REALPTOUT(FILENUM) ;"  DEPRECIATED
        ;"Purpose: to compare all records in the specified file and look for actual pointers out.
        ;"Input: FILENUM -- the Fileman file number to look at
        ;"Result: 1 = OK, -1 = error
        ;
        NEW RESULT SET RESULT=-1
        IF +$GET(FILENUM)'=FILENUM GOTO RPODN
        NEW REF SET REF=$GET(^DIC(FILENUM,0,"GL"))
        NEW CREF SET CREF=$$CREF^DILF(REF)
        IF REF="" GOTO RPODN
        ;"KILL ^TMG("TMGSIPH","UNRESOLVED",FILENUM)
        NEW STARTTIME SET STARTTIME=$H
        NEW MAXNUM SET MAXNUM=$ORDER(@(REF_"""A"")"),-1)
        WRITE MAXNUM," records to check for unresolved pointers in file #",FILENUM,!
        WRITE "Press ESC to abort...",!
        NEW IEN SET IEN=0
        NEW TMGABORT SET TMGABORT=0
        FOR  SET IEN=$ORDER(@CREF@(IEN)) QUIT:(+IEN'>0)!(TMGABORT=1)  DO
        . SET TMGABORT=$$USRABORT^TMGUSRI2() QUIT:(TMGABORT=1)
        . NEW TEMP SET TEMP=$$REAL1PTOUT(FILENUM,IEN)
        . IF (IEN#10)=0 DO
        . . DO PROGBAR^TMGUSRI2(IEN,"Progress: "_IEN,0,MAXNUM,70,STARTTIME)
        SET RESULT=1
RPODN   QUIT RESULT
 ;
 ;
PREPXREF(JNUM,FILENUM)  ;
        ;"Purpose: To ask the server to pepair organized cross references.
        ;"Input:  JNUM -- The job number of the background client process
        ;"        FILENUM -- The Fileman file to transfer
        ;"Results: 1 if OK, 0 IF error.
        NEW REPLY,ERROR,RESULT
        SET RESULT=1
        SET QUERY="PREP XREFS|"_FILENUM_"^1"
        DO MSGCLIENT^TMGKERN2(JNUM,QUERY,.REPLY,.ERROR,15)  ;"ignore REPLY
        IF $DATA(ERROR) DO
        . WRITE ERROR,!
        . SET RESULT=0
        QUIT RESULT