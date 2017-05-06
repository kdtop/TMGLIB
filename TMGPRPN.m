TMGPRPN  ;TMG/kst/Print Notes Fns. ;03/25/06, 2/2/14
         ;;1.0;TMG-LIB;**1**;04/25/04

 ;"TMG PRINT NOTES FUNCTIONS
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
 ;"CONTPRNT -- print notes for chosed patient, contigiously or divided
 ;"CONTPRN2(PtIEN) -- print notes for specified patient
 ;"PRPNQUIET(OPTIONS) -- print notes based on input options

 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================


 ;"=======================================================================
 ;"=======================================================================

CONTPRNT
        ;"Purpose: To ask for patient name, and date range, and output device
        ;"        and then print notes contigously (i.e. not a separate page
        ;"        for each note), or on separate pages
        ;"Input: none -- will ask user for values
        ;"Output: none -- will print to chosen device based on user preference

        NEW Options

        WRITE !,"-- PRINT NOTES FOR A PATIENT, CONTIGIOUSLY -- ",!!

        SET DIC=2  ;"PATIENT file
        SET DIC(0)="MAQE"
        SET DIC("A")="Enter name of Patient to print note for (^ to abort): "
        DO ^DIC

        DO CONTPRN2(+Y)
RADone
        QUIT


CONTPRN2(PtIEN)
        ;"Purpose: For specified patient, ask for date range, output device,
        ;"        and IF to print notes contigously (i.e. not a separate page
        ;"        for each note) or on separate pages, and IF to list avail notes.
        ;"Input: PtIEN -- record number  in file #2
        ;"Output: none -- will print to chosen device based on user preference

        NEW Options

        WRITE !

        SET Options("PATIENT")=$GET(PtIEN,-1)
        IF Options("PATIENT")'>0 DO  GOTO CP2Done
        . WRITE !,"No patient selected.  Aborting.",!

        NEW YN,index
        read !,"Show list of available notes? (^ to abort): YES// ",YN:$GET(DTIME,3600)
        IF YN="" SET YN="Y"
        IF YN="^" WRITE "Aborting.",! GOTO CP2Done
        IF ($$UP^XLFSTR(YN)["Y") do
        . WRITE !,"Available notes",!
        . WRITE "---------------",!
        . SET index=$ORDER(^TIU(8925,"C",PtIEN,""),1)
        . FOR  DO  QUIT:(index="")
        . . IF index="" QUIT  ;"note index is DocIEN
        . . NEW S,Date,DateS,DocTIEN,TypeName,X,Y
        . . SET Date=$PIECE($GET(^TIU(8925,index,13)),"^",1)
        . . SET Y="D" SET DateS=$$FMTE^XLFDT(Date)
        . . SET DocTIEN=$PIECE($GET(^TIU(8925,index,0)),"^",1)
        . . SET TypeName=$PIECE($GET(^TIU(8925.1,DocTIEN,0)),"^",1)
        . . IF TypeName="" SET TypeName="(Unknown document type): "_DocTIEN
        . . WRITE DateS," -- ",TypeName,!
        . . SET index=$ORDER(^TIU(8925,"C",PtIEN,index),1)

        NEW %DT
        SET %DT="AEP"
        SET %DT("A")="Enter starting date (^ to abort): "
        DO ^%DT
        IF Y=-1 DO  GOTO CP2Done
        . WRITE "Invalid date.  Aborting.",!
        SET Options("START")=Y

        SET %DT("A")="Enter ending date (^ to abort): "
        DO ^%DT
        IF Y=-1 DO  GOTO CP2Done
        . WRITE "Invalid date.  Aborting report.",!
        SET Options("END")=Y

        NEW ContMode
        read !,"Print each note on a separate page? NO// ",ContMode:$GET(DTIME,3600),!
        IF ContMode="" SET ContMode="N"
        SET Options("CONTMODE")=($$UP^XLFSTR(ContMode)["N")
        IF ContMode="^" WRITE "Aborting.",! GOTO CP2Done

        SET %ZIS("A")="Enter output printer or device (^ to abort): "
        DO ^%ZIS
        IF POP DO  GOTO CP2Done
        . WRITE !,"Error selecting output printer or device. Aborting report.",!

        use IO
        DO PRPNQUIET(.Options)
        use IO(0)

        DO ^%ZISC

        WRITE !,"Done.  Good bye!",!!
CP2Done
        QUIT


PRPNQUIET(OPTIONS)
        ;"Purpose: To create a report on transcription productivity based on
        ;"        options specified in OPTIONS.
        ;"Input: The following elements in OPTIONS should be defined
        ;"        0PTIONS("PATIENT")  ;"the IEN of the user (IEN from file 200)
        ;"        OPTIONS("START") ;"Earliest date of documents, in Fileman internal format
        ;"        OPTIONS("END")   ;"Latest date of documents, in Fileman internal format
        ;"        OPTIONS("CONTMODE") ;"if 1, then notes printed contigiously
        ;"Note: This will create a report by writing to the current device
        ;"        If the user wants output to go to a DEVICE, then they should call
        ;"        ^%ZIS prior to calling this function, then use IO,
        ;"        then when done, use IO(0) and call ^%ZISC to close

        NEW PtIEN
        NEW index SET index=""

        SET PtIEN=+$GET(OPTIONS("PATIENT"))
        IF PtIEN=0 DO  GOTO PQDone
        . WRITE "No patient record number supplied. Aborting.",!
        SET StartDT=+$GET(OPTIONS("START"))
        IF (StartDT=0) do
        . WRITE "No start date specified. Aborting.",!
        SET EndDT=+$GET(OPTIONS("END"))
        IF (EndDT=0) do
        . WRITE "No end date specified. Aborting.",!

        KILL ^TMP("TIUPR",$JOB)
        SET index=$ORDER(^TIU(8925,"C",PtIEN,""))
        FOR  DO  QUIT:(index="")
        . IF index="" QUIT  ;"note index is DocIEN
        . NEW S,SSN,DATE
        . SET SSN=$Piece(^DPT(PtIEN,0),"^",9)
        . Set DATE=$PIECE($GET(^TIU(8925,index,13)),"^",1)
        . IF (DATE'<StartDT)&(DATE'>EndDT) do
        . . Set ^TMP("TIUPR",$JOB,SSN_";"_PtIEN,DATE,index)="VistA EMR"
        . SET index=$ORDER(^TIU(8925,"C",PtIEN,index))

        DO PRINT^TIUPRPN1(1,1) ;0=> Chart Copy, 1=>Contigious

PQDone
        QUIT


