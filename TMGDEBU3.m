TMGDEBU3 ;TMG/kst/Debug utilities: logging, record dump ;3/18/15, 6/12/17
         ;;1.0;TMG-LIB;**1**;07/12/05
 ;
 ;"TMG DEBUG UTILITIES
 ;"Kevin Toppenberg MD
 ;"SACC-Compliant version 
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
 ;"ASKDUMP(FILE,IENS,OPTION) --A record dumper -- An alternative to Fileman Inquire
 ;"DUMPREC(FILENUM,IENS,SHOWEMPTY,FIELDSARRAY) -- dump (display) a record, NOT using ^DII (Fileman's Inquire code)
 ;"DIDUMP(FILENUM,IEN)  -- dump (display) a record, using Fileman functionality.
 ;
 ;"=======================================================================
 ;"Private API functions
 ;"=======================================================================
 ;"WTRECLAB(IEN,ENDER) -- actually WRITE out labels for record starting and ending.
 ;"WTFLDLAB(LABEL,FIELD,TYPE,ENDER) -- code that actually does writing of labels etc for output
 ;"WTLINE(LINE) -- actually WRITE out labels for record starting and ending.
 ;"WTWPLN(LINE) -- actually WRITE out line from WP field
 ;
 ;"=======================================================================
 ;"DEPENDENCIES  TMGDEBU2, TMGDBAP3,  TMGUSRI2, TMGXMLE2
 ;"=======================================================================
 ;"=======================================================================
 ;
DIDUMP(FILENUM,IEN)  ;
        ;"Purpose: to dump (display) a record, using Fileman functionality.
        ;"Input: FILENUM -- the number of the file to dump from
        ;"       IEN -- the record number to display
        ;"Note: this code is modified from INQ^DII
        ;
        NEW DIC,X,Y,DI,DPP,DK,DICSS
        SET X=FILENUM,Y=X
        SET DI=$GET(^DIC(FILENUM,0,"GL")) IF DI="" QUIT
        SET DPP(1)=FILENUM_"^^^@"
        SET DK=FILENUM
        K ^UTILITY($J),^(U,$J),DIC,DIQ,DISV,DIBT,DICS
        SET DIK=1
        SET ^UTILITY(U,$J,DIK,IEN)=""   ;"<-- note, to have multiple IEN's shown, iterate via DIK
        DO S^DII  ;"Jump into Fileman code.
        QUIT
        ;
ASKDUMPF(FILENUM)  ;"RECORD DUMPER FOR FILE
        NEW X SET X=$GET(FILENUM)
        DO ASKSCRN^TMGDBAP3  ;"GLOBALLY-SCOPED X IS IN AND OUT PARAMETER
        SET FILENUM=X IF FILENUM'>0 GOTO ADFDN
        NEW IENS SET IENS=$$ASKIENS^TMGDBAP3(FILENUM)
ADFDN   QUIT

ASKDUMP(FILE,IENS,OPTION) ;"A record dumper -- An alternative to Fileman Inquire
        ;"Input: FILE -- OPTIONAL.  File name or number.
        ;"       IENS: OPTIONAL.  Allows for supplying a partial IENS supplying a
        ;"                      partial path.  E.g. If a full IENS to FILENUM
        ;"                    would be '2,3,4455,' and IF the IENS supplied is
        ;"                  '3,4455,' then only the missing IEN (in this case 2)
        ;"                 would be asked.
        ;"       OPTION -- OPTIONAL.  PASS BY REFERENCE.  Format:
        ;"                OPTION("NO LOOP")=1  Doesn't loop back and ask for another.
        ;"RESULT: none
        WRITE !!,"  -= RECORD DUMPER =-",!
        NEW FIENS,FILENUM
AL1     SET FIENS=$$ASKFIENS^TMGDBAP3(.FILE,.IENS)
        IF (FIENS["?")!(FIENS="^") GOTO ASKDN
        SET FILENUM=$PIECE(FIENS,"^",1)
        SET IENS=$PIECE(FIENS,"^",2)
AL2     SET IENS=$$ASKIENS^TMGDBAP3(FILENUM,IENS)
        IF (IENS["?")!(IENS="") GOTO AL1
        NEW % SET %=2
        WRITE "Display empty fields"
        DO YN^DICN
        IF %=-1 WRITE ! GOTO ASKDN
        NEW %ZIS
        SET %ZIS("A")="Enter Output Device: "
        SET %ZIS("B")="HOME"
        DO ^%ZIS  ;"standard device call
        IF POP DO  GOTO ASKDN
        . DO SHOWERR^TMGDEBU2(,"Error opening output.  Aborting.")
        USE IO
        ;"Do the output
        WRITE ! DO DUMPREC(FILENUM,IENS,(%=1))
        ;" Close the output device
        DO ^%ZISC
        DO PRESS2GO^TMGUSRI2
        ;"NEW TEMP
        SET IENS=$PIECE(IENS,",",2,99)  ;"force Pick of NEW record to dump
        IF +IENS>0 GOTO AL2
        IF $GET(OPTION("NO LOOP"))'=1 GOTO AL1
ASKDN   QUIT
        ;
DUMPREC(FILENUM,IENS,SHOWEMPTY,FIELDSARRAY)  ;
        ;"Purpose: to dump (display) a record, NOT using ^DII (Fileman's Inquire code)
        ;"Input: FILENUM -- the number of the file to dump from
        ;"       IENS -- the record number to display (or IENS: #,#,#,)
        ;"       SHOWEMPTY -- OPTIONAL;  IF 1 then empty fields will be displayed
        ;"       FIELDSARRAY -- OPTIONAL.  PASS BY REFERENCE.
        ;"          Allows user to specify which fields to show.  Format:
        ;"            FIELDSARRAY(FieldtoShow)="" <-- FieldtoShow is name or number
        ;"            FIELDSARRAY(FieldtoShow)="" <-- FieldtoShow is name or number
        ;"          Default is an empty array, in which all fields are considered
        ;"Result: None
        NEW FIELDS
        SET FIELDS("*")=""
        NEW FLAGS SET FLAGS="ip"
        IF $GET(SHOWEMPTY)=1 SET FLAGS=FLAGS_"b"
        ;
        WRITE "Record# ",IENS," in FILE: ",FILENUM,!
        NEW AFIELD,FIELDNAME
        IF $DATA(FIELDSARRAY)=0 DO
        . SET AFIELD=$ORDER(^DD(FILENUM,0))
        . IF +AFIELD>0 FOR  DO  QUIT:(+AFIELD'>0)
        . . SET FIELDNAME=$PIECE(^DD(FILENUM,AFIELD,0),"^",1)
        . . SET FIELDS("TAG NAME",AFIELD)=FIELDNAME_"("_AFIELD_")"
        . . SET AFIELD=$ORDER(^DD(FILENUM,AFIELD))
        ELSE  DO   ;"Handle case of showing ONLY requested fields
        . NEW TEMP SET TEMP=""
        . FOR  SET TEMP=$ORDER(FIELDSARRAY(TEMP)) QUIT:(TEMP="")  DO
        . . IF +TEMP=TEMP DO
        . . . SET AFIELD=+TEMP
        . . . SET FIELDNAME=$PIECE(^DD(FILENUM,AFIELD,0),"^",1)
        . . ELSE  DO
        . . . SET FIELDNAME=TEMP
        . . . IF $$SETFFNUM^TMGDBAP3(FILENUM,FIELDNAME,,.AFIELD)=0 QUIT
        . . SET FIELDS("TAG NAME",AFIELD)=FIELDNAME_"("_AFIELD_")"
        . ;"Now exclude those fields not specifically included
        . SET AFIELD=0
        . FOR  SET AFIELD=$ORDER(^DD(FILENUM,AFIELD)) QUIT:(+AFIELD'>0)  DO
        . . IF $DATA(FIELDS("TAG NAME",AFIELD))'=0 QUIT
        . . SET FIELDNAME=$PIECE(^DD(FILENUM,AFIELD,0),"^",1)
        . . SET FIELDS("Field Exclude",AFIELD)=""
        ;
        NEW RFUNC,FFUNC,LFUNC,WPLFUNC
        SET RFUNC="WTRECLAB^TMGDEBU3"
        SET FFUNC="WTFLDLAB^TMGDEBU3"
        SET LFUNC="WTLINE^TMGDEBU3"
        SET WPLFUNC="WTWPLN^TMGDEBU3"
        IF +IENS=IENS DO
        . DO WRIT1REC^TMGXMLE4(FILENUM,IENS,.FIELDS,FLAGS,,,"",RFUNC,FFUNC,LFUNC,WPLFUNC)
        ELSE  DO  ;"dump a subfile record
        . DO WRIT1REC^TMGXMLE4(FILENUM,+IENS,.FIELDS,FLAGS,,IENS,"",RFUNC,FFUNC,LFUNC,WPLFUNC)
        QUIT
        ;
WTRECLAB(IEN,ENDER) ;
        ;"Purpose: To actually WRITE out labels for record starting and ending.
        ;"      IEN -- the IEN (record number) of the record
        ;"      ENDER -- OPTIONAL IF 1, then ends field.
        ;"Results: none.
        ;"Note: Used by DUMPREC above, with callback from TMGXMLE2
        IF +$GET(ENDER)>0 WRITE !
        ELSE  WRITE "     Multiple Entry #",IEN,"",!
        QUIT
        ;
WTFLDLAB(LABEL,FIELD,TYPE,ENDER,IGNORE) ;
        ;"Purpose: This is the code that actually does writing of labels etc for output
        ;"      This is a CUSTOM CALL BACK function called by WRIT1FLD^TMGXMLE2
        ;"Input: LABEL -- OPTIONAL -- Name of label, to WRITE after  'label='
        ;"       FIELD -- OPTIONAL -- Name of field, to WRITE after  'id='
        ;"       TYPE -- OPTIONAL -- type of field, to WRITE after  'type='
        ;"       ENDER -- OPTIONAL IF 1, then ends field.
        ;"       IGNORE -- a properties array, that is ignored here.  
        ;"Results: none.
        ;"Note: Used by DUMPREC above, with callback from TMGXMLE2
        ;
        ;"To WRITE out <Field label="NAME" id=".01" type="FREE TEXT"> or </Field>
        ;
        IF +$GET(ENDER)>0 DO
        . IF $GET(^TMP("FORMATED OUT TMGDEBU3",$J))="" QUIT  ;"try to prevent sequential empty lines 
        . WRITE !
        . KILL ^TMP("FORMATED OUT TMGDEBU3",$J)
        ELSE  DO
        . KILL ^TMP("FORMATED OUT TMGDEBU3",$J)
        . NEW STR SET STR=FIELD
        . NEW OUT SET OUT=""
        . IF $GET(FIELD)'="" SET OUT=OUT_$$RJ^XLFSTR(.STR,6," ")_"-"
        . IF $GET(LABEL)'="" SET OUT=OUT_LABEL_" "
        . ;"IF $GET(TYPE)'="" WRITE "type=""",TYPE,""" "
        . SET OUT=OUT_": "
        . WRITE OUT SET ^TMP("FORMATED OUT TMGDEBU3",$J)=OUT
        QUIT
        ;
WTLINE(LINE)  ;
        ;"Purpose: To actually WRITE out labels for record starting and ending.
        ;"Input: LINE -- The line of text to be written out.
        ;"Results: none.
        ;"Note: Used by DUMPREC above, with callback from TMGXMLE2
        WRITE LINE
        QUIT
        ;
WTWPLN(LINE) ;
        ;"Purpose: To actually WRITE out line from WP field
        ;"Input: LINE -- The line of text to be written out.
        ;"Results: none.
        ;"Note: Used by DUMPREC above, with callback from TMGXMLE2
        WRITE LINE,!
        QUIT
        ;