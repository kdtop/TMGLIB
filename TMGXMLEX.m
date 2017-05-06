TMGXMLEX ;TMG/kst/XML Exporter ;10/26/14
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
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"Dependencies   (duplicates shown in parenthesies)
 ;"TMGXML*, TMGDBAP*, TMGDEBUG, TMGUSRIF, TMGSTUTL, TMGMISC, TMGIOUTL, XLFSTR
 ;"
 ;"=======================================================================
 ;"=======================================================================
EXPORT  ;
        ;"Purpose: To ask for parameters, select output, and DO actual export
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
