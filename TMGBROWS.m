TMGBROWS ;TMG/kst/Record browser ;03/25/06, 2/2/14
         ;;1.0;TMG-LIB;**1**;03/10/07

 ;" TMG BROWSE RECORDS
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
 ;"ASKBROWSE -- browse records, and follow pointers

 ;"=======================================================================
 ;" Private Functions.
 ;"=======================================================================
 ;"Browse(FileNum,IENS,ShowEmpty) --Browse a record, allowing display of
 ;"      the current record, or follow pointers to other records.
 ;"DispRec(FileNum,IEN) -- To display record

 ;"=======================================================================


ASKBROWSE
        ;"Purpose: To browse records, and follow pointers.

        WRITE !!,"  -= RECORD BROWSE =-",!
        NEW FIENS,IENS
AL1
        SET FIENS=$$ASKFIENS^TMGDBAP3()
        IF (FIENS["?")!(FIENS="^") GOTO ASKDone

        SET FileNum=$PIECE(FIENS,"^",1)
        SET IENS=$PIECE(FIENS,"^",2)

AL2
        SET IENS=$$ASKIENS^TMGDBAP3(FileNum,IENS)
        IF (IENS["?")!(IENS="") GOTO AL1

        NEW % SET %=2
        WRITE "Display empty fields"
        DO YN^DICN
        IF %=-1 WRITE ! GOTO ASKDone

        ;"Do the output
        WRITE ! DO Browse(FileNum,IENS,(%=1))
        SET IENS=$PIECE(IENS,",",2,99)  ;"force Pick of new record to dump
        IF +IENS>0 GOTO AL2
        GOTO AL1


ASKDone
        QUIT


Browse(FileNum,IENS,ShowEmpty)
        ;"Purpose: Browse a record, allowing display of the current record, or
        ;"         follow pointers to other records.
        ;"Input: FileNum -- the number of the file to browse
        ;"       IENS -- the record number to display (or IENS: #,#,#,)
        ;"       ShowEmpty -- OPTIONAL;  IF 1 then empty fields will be displayed

        NEW FldInfo,field
        DO GetPtrsOUT^TMGDBAPI(FileNum,.FldInfo)

        IF $EXTRACT(IENS,$LENGTH(IENS))'="," SET IENS=IENS_","

        SET field=""
        FOR  SET field=$ORDER(FldInfo(field)) QUIT:(field="")  do
        . NEW name SET name=$$GetFldName^TMGDBAPI(FileNum,field)
        . SET FldInfo(field,"NAME")=name

        NEW Menu
        NEW count SET count=1
                WRITE "File: ",$$GETFNAME^TMGDBAP2(FileNum),!

        SET Menu(0)="File: "_$$GETFNAME^TMGDBAP2(FileNum)_" ("_FileNum_"), Record: "_IENS
        SET field=""
        FOR  SET field=$ORDER(FldInfo(field)) QUIT:(field="")  do
        . NEW ptr SET ptr=$$GET1^DIQ(FileNum,IENS,field,"I")
        . NEW otherName SET otherName=$$GET1^DIQ(FileNum,IENS,field)
        . IF ptr="" QUIT
        . NEW name SET name=$$GetFldName^TMGDBAPI(FileNum,field)
        . SET Menu(count)="BROWSE: ("_field_") "_name_"--> "_otherName_$CHAR(9)_FldInfo(field)_"|"_ptr
        . SET count=count+1

        SET Menu(count)="DUMP entire record"_$CHAR(9)_"DUMP"

M0      WRITE #
        SET UsrSlct=$$MENU^TMGUSRI2(.Menu,"^")

        IF UsrSlct="DUMP" DO DispRec(FileNum,IENS) GOTO M0
        IF UsrSlct["|" DO  GOTO M0
        . NEW newFile SET newFile=$PIECE(UsrSlct,"|",1)
        . NEW IEN SET IEN=$PIECE(UsrSlct,"|",2)_","
        . DO Browse(newFile,IEN,.ShowEmpty)
        IF UsrSlct="^" GOTO MenuDone
        GOTO M0

MenuDone
        QUIT


DispRec(FileNum,IEN)
        ;"Purpose: To display record
        ;"Input:  FileNum -- The File number to display
        ;"        IEN -- the IEN (record number) to display in file.
        ;"Results: none

        WRITE "File: ",$$GETFNAME^TMGDBAP2(FileNum),!
        DO DUMPREC^TMGDEBU3(FileNum,IEN_",",0)
        DO PRESS2GO^TMGUSRI2

        QUIT

