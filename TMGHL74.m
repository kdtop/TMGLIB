TMGHL74 ;TMG/kst-HL7 transformation engine processing ;8/14/15
              ;;1.0;TMG-LIB;**1**;09/20/13
 ;
 ;"TMG HL7 TRANSFORMATION FUNCTIONS
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"NOTE: this is code for working with labs from **[Laughlin lab]**
 ;"      FYI -- Pathgroup code is in TMGHL73
 ;"             Laughlin RADIOLOGY is in TMGHL74R
 ;"             common code is in TMGHL72
 ;"=======================================================================
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"TEST  -- Pick file and manually send through filing process.   
 ;"BATCH -- Launch processing through all files in folder for laughlin lab
 ;"
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;"XMSG    -- Process entire message before processing segments
 ;"XMSH15  -- Process MSH segment, FLD 15
 ;"XMSH16  -- Process MSH segment, FLD 16
 ;"PID     -- transform the PID segment, esp SSN
 ;"XORC1   -- Process empty ORC message, field 1
 ;"XORC12  -- Process empty ORC message, field 12
 ;"XORC13  -- Process empty ORC message, field 13
 ;"OBR     -- setup for OBR fields.
 ;"OBR4    -- To transform the OBR segment, field 4
 ;"OBR15   -- Transform Secimen source
 ;"OBR16   -- Transform Ordering provider.
 ;"OBX3    -- transform the OBX segment, field 3 -- Observation Identifier
 ;"OBX5    -- transform the OBX segment, field 5 -- Observation value
 ;"OBX15   -- transform the OBX segment, field 15 ---- Producer's ID
 ;"OBX16   -- transform the OBX segment, field 16 ---- Responsibile Observer
 ;"OBX18   -- transform the OBX segment, field 18 ---- Equipment Identifier (EI)
 ;"NTE3    -- transform the NTE segment, field 3
 ;"XFTEST(FLDVAL,TMGU) -- convert test code into value acceptable to VistA
 ;"SUORL   -- Setup TMGINFO("ORL"), TMGINFO("LOC"), TMGINFO("INSTNAME")
 ;" 
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"TMGSTUTL, all the HL*, LA* code that the HL processing path normally calls.
 ;"=======================================================================
 ;
TEST  ;"Pick file and manually send through filing process.
        ;"DO TEST^TMGHL72("/home/laughlin_results")
        DO TEST^TMGHL71("/home/laughlin_results")
        QUIT
        ;
BATCH   ;"Launch processing through all files in folder.
        ;"NEW DIR SET DIR="/home/laughlin_results"
        ;"NEW DONEPATH SET DONEPATH="/mnt/WinServer/LaughlinHL7"
        NEW DIR SET DIR="/mnt/WinServer/LaughlinHL7"  ;"//kt 12/23/16
        NEW DONEPATH SET DONEPATH=DIR                 ;"//kt 12/23/16
        DO HLDIRIN^TMGHL71(DIR,1000,10,DONEPATH)
        QUIT
        ;
        ;"---------------------------------------------------------------
        ;"===============================================================
        ;"|  Below are the call-back functions to handle transformation |
        ;"|  hooks, called by the XFMSG^TMGHL7X engine                  |
        ;"===============================================================
        ;"---------------------------------------------------------------
        ;
MSG    ;"Purpose: Process entire message before processing segments
        DO XMSG^TMGHL72 
        QUIT
        ;
MSG2    ;"Purpose: Process entire message after processing segments
        DO XMSG2^TMGHL72 
        QUIT
        ;
MSH3    ;"Purpose: Process MSH segment, FLD 4 (Sending Application)
        ;"SET TMGVALUE="LA7V HOST LMH"
        QUIT
        ;
MSH4  ;"Purpose: Process MSH segment, FLD 4 (Sending Facility)
        SET TMGVALUE="LAUGHLIN MEM HOSPITAL"
        DO XMSH4^TMGHL72
        QUIT
        ;
MSH15  ;"Purpose: Process MSH segment, FLD 15
        DO XMSH15^TMGHL72
        QUIT
        ;
MSH16  ;"Purpose: Process MSH segment, FLD 16
        DO XMSH16^TMGHL72 
        QUIT
        ;
PID     ;"Purpose: To transform the PID segment, esp SSN
        DO PID^TMGHL72
        QUIT
        ;
PV18    ;"Purpose: Process entire PV1-8 segment
        ;"NOTE: Pathgroup doesn't send order information in OBR16, so will use
        ;"      information from here to fix that.
        ;"In Pathrgoup messages, PV1 comes before any OBR segments.
        NEW LNAME SET LNAME=$PIECE(TMGVALUE,TMGU(2),2)
        NEW FNAME SET FNAME=$PIECE(TMGVALUE,TMGU(2),3)
        IF ($$UP^XLFSTR(LNAME)="TOPPENBERG")&($$UP^XLFSTR(FNAME)="EE") DO
        . SET $PIECE(TMGVALUE,TMGU(2),3)="Marcia"
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(TMGHL7MSG(IDX)) QUIT:(+IDX'>0)  DO
        . NEW SEGTYPE SET SEGTYPE=$GET(TMGHL7MSG(IDX,"SEG")) QUIT:SEGTYPE=""
        . IF SEGTYPE="OBR" DO
        . . ;"NEW PROV SET PROV=$$GETPCE^TMGHL7X2(.TMGHL7MSG,IDX,16)
        . . ;"IF PROV'="" QUIT  ;"if value provided, don't overwrite.        
        . . DO SETPCE^TMGHL7X2(TMGVALUE,.TMGHL7MSG,.TMGU,IDX,16)
        DO SETPCE^TMGHL7X2(TMGVALUE,.TMGHL7MSG,.TMGU,"ORC",12)
        QUIT
        ;
ORC1   ;"Purpose: Process empty ORC message, field 1
        DO XORC1^TMGHL72
        QUIT
        ;
ORC12  ;"Purpose: Process empty ORC message, field 12
        DO XORC12^TMGHL72
        QUIT
        ;
ORC13  ;"Purpose: Process empty ORC message, field 13
        DO XORC13^TMGHL72
        ;"SET $PIECE(TMGVALUE,"^",1)="Laughlin_Office"
        SET $PIECE(TMGVALUE,"^",1)="Family Phys Of Greeneville"
        SET $PIECE(TMGVALUE,"^",2)="69" 
        QUIT
        ;
OBR     ;"Purppse: setup for OBR fields.
        DO OBR^TMGHL72
        
        QUIT
OBR4    ;"Purpose: To transform the OBR segment, field 4
        DO OBR4^TMGHL72
        QUIT
        ;
OBR15   ;"Transform Secimen source
        DO OBR15^TMGHL73
        QUIT
        ;
OBR16   ;"Transform Ordering provider.
        DO OBR16^TMGHL72
        QUIT
        ;
OBX3    ;"Purpose: To transform the OBX segment, field 3 -- Observation Identifier
        DO OBX3^TMGHL72
        QUIT
        ;
OBX5    ;"Purpose: To transform the OBX segment, field 5 -- Observation value
        DO OBX5^TMGHL72
        QUIT
        ;
OBX15   ;"Purpose: To transform the OBX segment, field 15 ---- Producer's ID
        DO OBX15^TMGHL72
        QUIT
        ;
OBX16   ;"Purpose: To transform the OBX segment, field 16 ---- Responsibile Observer
        DO OBX16^TMGHL72
        QUIT
        ;
OBX18   ;"Purpose: To transform the OBX segment, field 18 ---- Equipment Identifier (EI)
        DO OBX18^TMGHL72
        QUIT
        ;
NTE3    ;"Purpose: To transform the NTE segment, field 3 (the comments)
        DO NTE3^TMGHL72                         
        QUIT
        ;
SUPROV  ;"Purpose: Setup TMGINFO("PROV") -- Ordering provider.
        DO SUPROV^TMGHL72                
        QUIT   
        ;
SUORL   ;"Purpose: Setup TMGINFO("ORL") and TMGINFO("LOC") and TMGINFO("INSTNAME")
        DO SUORL^TMGHL72
        QUIT
        ;
