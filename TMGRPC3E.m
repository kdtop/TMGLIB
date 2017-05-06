TMGRPC3E ;TMG/kst/Support Functions for GUI_Config ;08/31/08, 7/7/10, 3/31/13, 2/2/14
         ;;1.0;TMG-LIB;**1**;08/31/08
 ;
 ;"TMG RPC FUNCTIONS for a GUI config program
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
 ;" RPC -- Public Functions.
 ;"=======================================================================
 ;" <none>
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"GETEMPTY(TMGOUT,TMGPARAMS) -- get stub entries for a file (typically a subfile)
 ;"GETHELPM(TMGOUT,TMGPARAMS)  -- retrieve the help message for a given field.
 ;"STUBNEWR(TMGFILE,NewValue,TMGOUT) -- create a stub entry in the file (200), to be filled in elsewhere
 ;"GETIFWP(TMGOUT,TMGPARAMS) -- return IF filenumber is a WP subfile
 ;"GETWPFLD(TMGOUT,TMGPARAMS) -- retrieve on word processing (WP) field entry
 ;"PSTWPFLD(TMGOUT,TMGPARAMS,INPUT) -- store a word processing (WP) field entry
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;"Dependencies:
 ;"  TMGRPC3* only
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;
GETEMPTY(TMGOUT,TMGPARAMS) ;"GET EMPTY ENTRY
        ;"Purpose: to get stub entries for a file (typically a subfile)
        ;"              "GET EMPTY ENTRY"  -- getting stub entries for subfiles typically
        ;"                   TMGPARAMS: file entry (file or subfile number)
        ;"Input: TMGOUT --an OUT ARRAY, filled as follows:
        ;"          TMGOUT(0)="1^Success" or "-1^Message"
        ;"          TMGOUT(1)=File^^FieldNum^^DDInfo...
        ;"          TMGOUT(2)=File^^FieldNum^^DDInfo...
        ;"          Etc ...
        ;"       Params: filenumber (typically a subfilenumber)
        ;"Result: None
        SET TMGOUT(0)="1^Success"
        NEW TMGCOUNT SET TMGCOUNT=1
        NEW TMGFNUM SET TMGFNUM=+$GET(TMGPARAMS)
        NEW TMGFLDNUM SET TMGFLDNUM=0
        IF $DATA(^DD(TMGFNUM))=0 DO  GOTO GEPDN
        . SET TMGOUT(0)="-1^Invalid file number.  Got: "_$GET(TMGPARAMS)
        FOR  SET TMGFLDNUM=$ORDER(^DD(TMGFNUM,TMGFLDNUM)) QUIT:(+TMGFLDNUM'>0)  DO
        . NEW TMGDDINFO SET TMGDDINFO=$PIECE($GET(^DD(TMGFNUM,TMGFLDNUM,0)),"^",1,4)
        . NEW TMGFLDVAL SET TMGFLDVAL=$$GETEFLD(TMGFNUM,TMGFLDNUM)
        . SET TMGOUT(TMGCOUNT)=TMGFNUM_"^^"_TMGFLDNUM_"^"_TMGFLDVAL_"^"_TMGDDINFO
        . SET TMGCOUNT=TMGCOUNT+1
        . IF $PIECE(TMGDDINFO,"^",2)["V" DO
        . . SET TMGCOUNT=$$ADDVINFO^TMGRPC3B(.TMGOUT,TMGFNUM,TMGFLDNUM)
GEPDN   QUIT
 ;
GETEFLD(FILENUM,FLDNUM) ;"GET EMPTY FIELD VALUE
        NEW RESULT SET RESULT=""
        IF FLDNUM=".01" SET RESULT="<NEW>" GOTO GEFDN
        IF FILENUM="811.97" DO  ;"REMINDER DEFINITION:BASELINE AGE FINDINGS
        . IF FLDNUM=".01" SET RESULT="1Y"
        . IF (FLDNUM="5")!(FLDNUM="6") SET RESULT="0"
        ;"Can add more special cases later        
GEFDN   QUIT RESULT        
 ;
GETHELPM(TMGOUT,TMGPARAMS) ;
        ;"Purpose: to retrieve the help message for a given field.
        ;"Input: TMGOUT -- an OUT PARAMETER, PASS BY REFERENCE.
        ;"       TMGPARAMS -- file^field^HelpType^IENS
        ;"Output: TMGOUT is filled as follows:
        ;"          TMGOUT(0)="1^Success" or "-1^Message"
        ;"          TMGOUT(1...?)=Help Message
        NEW TMGMSG
        NEW TMGINDEX SET TMGINDEX=1
        NEW TMGFILE SET TMGFILE=+$PIECE(TMGPARAMS,"^",1)
        NEW TMGFIELD SET TMGFIELD=+$PIECE(TMGPARAMS,"^",2)
        NEW TMGHELPTYPE SET TMGHELPTYPE=$PIECE(TMGPARAMS,"^",3)
        NEW TMGIENS SET TMGIENS=$PIECE(TMGPARAMS,"^",4)
        IF TMGFILE'>0 DO  GOTO GHMDONE
        . SET TMGOUT(0)="-1^No file number supplied"
        IF TMGFIELD'>0 DO  GOTO GHMDONE
        . SET TMGOUT(0)="-1^No Field Number supplied"
        DO HELP^DIE(TMGFILE,TMGIENS,TMGFIELD,TMGHELPTYPE,"TMGMSG")
        NEW TMGI SET TMGI=""
        FOR  SET TMGI=$ORDER(TMGMSG("DIHELP",TMGI)) QUIT:(TMGI="")  DO
        . SET TMGOUT(TMGINDEX)=TMGMSG("DIHELP",TMGI)
        . SET TMGINDEX=TMGINDEX+1
        SET TMGOUT(0)="1^Success"
GHMDONE QUIT
 ;
STUBNEWR(TMGFILE,TMGNEWVALUE,TMGOUT) ;"STUB NEW RECORD
        ;"Purpose: to create a stub entry in the file (200), to be filled in elsewhere
        ;"Input: TMGNEWVALUE -- a value for the .01 field
        ;"       TMGOUT -- PASS BY REFERENCE, an OUT PARAMETER.
        ;"Output: A NEW record will be added to file, IF no errors
        ;"       TMGOUT(0): 1^Success^NewIEN   or -1^See Fileman message
        ;"       TMGOUT(1) will contain Fileman error, IF any
        ;"Results: none
        ;"NOTICE: This function makes no promise that the creation of the
        ;"      NEW record will succeed.  For example, IF there are required
        ;"      fields, the creation will fail.  So users should check for success
        ;"      result and error feedback.
        ;"Results: none
        NEW TMGFDA,TMGIEN,TMGMSG,DIC
        SET DIC(0)=""  ;"I get a crash without this, for some reason...
        SET TMGFDA(TMGFILE,"+1,",.01)=TMGNEWVALUE
        DO UPDATE^DIE("S","TMGFDA","TMGIEN","TMGMSG")
        ;
        IF $DATA(TMGMSG("DIERR")) DO  GOTO SNPDONE
        . SET TMGOUT(0)="-1^See Fileman message"
        . SET TMGOUT(1)=$$GETERSTR^TMGRPC3G(.TMGMSG)
        ;
        SET TMGOUT(0)="1^Success^"_$GET(TMGIEN(1))
SNPDONE QUIT
 ;
GETIFWP(TMGOUT,TMGPARAMS) ;"GET IF WP FIELD
        ;"Purpose: to return IF filenumber is a WP subfile
        ;"Input: TMGOUT -- an OUT PARAMETER, PASS BY REFERENCE.
        ;"       TMGPARAMS -- file^field
        ;"Output:
        ;"       TMGOUT(0): "1^Success^YES/NO" or "-1^Message"
        ;"            "YES" IF is a WP subfile, otherwise "NO"
        ;"       TMGOUT(1) will contain Fileman error, IF any
        ;"Results: none
        NEW TMGRESULT
        NEW TMGFILE SET TMGFILE=+$PIECE(TMGPARAMS,"^",1)
        NEW TMGFIELD SET TMGFIELD=+$PIECE(TMGPARAMS,"^",2)
        IF TMGFILE'>0 DO  GOTO GWPSDONE
        . SET TMGOUT(0)="-1^No file number supplied"
        IF TMGFIELD'>0 DO  GOTO GWPSDONE
        . SET TMGOUT(0)="-1^No Field Number supplied"
        ;
        DO FIELD^DID(TMGFILE,TMGFIELD,"","TYPE","TMGARRAY","TMGMSG")
        ;
        IF $DATA(TMGMSG("DIERR")) DO  GOTO GWPSDONE
        . SET TMGOUT(0)="-1^See Fileman message"
        . SET TMGOUT(1)=$$GETERSTR^TMGRPC3G(.TMGMSG)
        ;
        IF $GET(TMGARRAY("TYPE"))="WORD-PROCESSING" DO
        . SET TMGRESULT="YES"
        ELSE  DO
        . SET TMGRESULT="NO"
        ;
        SET TMGOUT(0)="1^Success^"_TMGRESULT
GWPSDONE ;
        QUIT
 ;
GETWPFLD(TMGOUT,TMGPARAMS) ;"GET WP FIELD
        ;"Purpose: To retrieve on word processing (WP) field entry
        ;"Input: TMGOUT -- an OUT PARAMETER, PASS BY REFERENCE.
        ;"       TMGPARAMS -- FileNum^Field^IENS (e.g. 'GET ONE WP FIELD^200^73,')
        ;"Output:
        ;"       TMGOUT(0): "1^Success" or "-1^Message"
        ;"       TMGOUT(1) will contain Fileman error, IF any
        ;"       - or to return WP array -
        ;"       TMGOUT(1)=1st line of text
        ;"       TMGOUT(2)=2nd line of text
        ;"       etc..
        ;"Results: None
        NEW TMGFILE SET TMGFILE=+$PIECE(TMGPARAMS,"^",1)
        NEW TMGFIELD SET TMGFIELD=+$PIECE(TMGPARAMS,"^",2)
        NEW TMGIENS SET TMGIENS=$PIECE(TMGPARAMS,"^",3)  ;//kt removed +$PIECE
        IF TMGFILE'>0 DO  GOTO GWPDONE
        . SET TMGOUT(0)="-1^No file number supplied"
        IF TMGFIELD'>0 DO  GOTO GWPDONE
        . SET TMGOUT(0)="-1^No Field Number supplied"
        IF TMGIENS="" DO  GOTO GWPDONE
        . SET TMGOUT(0)="-1^No IENS supplied"
        ;
        NEW TMGARRAY,TMGMSG,TMGTEMP
        SET TMGTEMP=$$GET1^DIQ(TMGFILE,TMGIENS,TMGFIELD,"","TMGARRAY","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO GWPDONE
        . SET TMGOUT(0)="-1^See Fileman message"
        . SET TMGOUT(1)=$$GETERSTR^TMGRPC3G(.TMGMSG)
        . SET TMGOUT(2)="Actual call was:"
        . SET TMGOUT(3)="$$GET1^DIQ("_TMGFILE_","_TMGIENS_","_TMGFIELD_","""",""TMGARRAY"",""TMGMSG"")"
        ;
        SET TMGOUT(0)="1^Success"
        NEW TMGINDEX SET TMGINDEX=1
        NEW TMGI SET TMGI=0
        FOR  SET TMGI=$ORDER(TMGARRAY(TMGI)) QUIT:(TMGI="")  DO
        . SET TMGOUT(TMGINDEX)=TMGARRAY(TMGI)
        . SET TMGINDEX=TMGINDEX+1
GWPDONE QUIT
 ;
PSTWPFLD(TMGOUT,TMGPARAMS,INPUT) ;"POST WP FIELD
        ;"Purpose: To store a word processing (WP) field entry
        ;"Input: TMGOUT -- an OUT PARAMETER, PASS BY REFERENCE.
        ;"       TMGPARAMS -- FileNum^FieldNum^IENS (e.g. 'GET ONE WP FIELD^200^73,')
        ;"       INPUT -- holds the WP itself to be stored:
        ;"                   INPUT(0)=0TH line
        ;"                   INPUT(1)=1st line
        ;"                   INPUT(2)=2nd line
        ;"                   ...
        ;"Output:
        ;"       TMGOUT(0): "1^Success" or "-1^Message"
        ;"       TMGOUT(1) will contain Fileman error, IF any
        ;"Results: None
        NEW TMGFILE SET TMGFILE=+$PIECE(TMGPARAMS,"^",1)
        NEW TMGFIELD SET TMGFIELD=+$PIECE(TMGPARAMS,"^",2)
        NEW TMGIENS SET TMGIENS=$PIECE(TMGPARAMS,"^",3) ;//kt removed +$PIECE
        IF TMGFILE'>0 DO  GOTO PWPDONE
        . SET TMGOUT(0)="-1^No file number supplied"
        IF TMGFIELD'>0 DO  GOTO PWPDONE
        . SET TMGOUT(0)="-1^No Field Number supplied"
        IF TMGIENS="" DO  GOTO PWPDONE
        . SET TMGOUT(0)="-1^No IENS supplied"
        IF $LENGTH(TMGIENS,",")=1 SET TMGIENS=TMGIENS_","
        ;
        DO WP^DIE(TMGFILE,TMGIENS,TMGFIELD,"K","INPUT","TMGMSG")
        ;
        IF $DATA(TMGMSG("DIERR")) DO  GOTO GWPDONE
        . SET TMGOUT(0)="-1^See Fileman message"
        . SET TMGOUT(1)=$$GETERSTR^TMGRPC3G(.TMGMSG)
        ;
        SET TMGOUT(0)="1^Success"
PWPDONE QUIT
 ;
GETRCODE(TMGOUT,TMGPARAMS) ;"GET GODE ROUTINE
        ;"Purpose: To retrieve mumps code in specified routine. 
        ;"Input:  TMGOUT -- an OUT PARAMETER, PASS BY REFERENCE.
        ;"        TMGPARAMS -- RoutineName (e.g. 'DIT3')
        ;"Output: TMGOUT(0): "1^Success" or "-1^Message"
        ;"        TMGOUT(1) -- 1st line of code
        ;"        TMGOUT(2) -- 2nd line of code  etc. 
        ;"       etc..
        ;"Results: None
        SET TMGOUT(0)="1^Success"
        NEW ROUTINE SET ROUTINE=$$TRIM^XLFSTR(TMGPARAMS)
        IF ROUTINE["^" SET ROUTINE=$PIECE(ROUTINE,"^",2)
        IF ROUTINE="" DO  GOTO GRCDN
        . SET TMGOUT(0)="-1^No routine name specified"
        NEW STR,IDX SET IDX=0
        FOR  DO  QUIT:(STR="")
        . SET IDX=IDX+1
        . NEW REF SET REF="+"_IDX_"^"_ROUTINE
        . SET STR=$TEXT(@REF)
        . NEW TRIMSTR SET TRIMSTR=$$TRIM^XLFSTR(STR)
        . IF TRIMSTR'="" SET TMGOUT(IDX)=STR
        IF $DATA(TMGOUT(1))=0 DO  GOTO GRCDN
        . SET TMGOUT(0)="-1^No text found in routine '"_ROUTINE_"'"
GRCDN  QUIT
 
