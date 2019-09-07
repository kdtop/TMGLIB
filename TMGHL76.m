TMGHL76 ;TMG/kst-HL7 transformation engine processing ;4/11/2019
              ;;1.0;TMG-LIB;**1**;09/20/13
 ;
 ;"TMG HL7 TRANSFORMATION FUNCTIONS
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 3/27/2019  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"NOTE: this is code for working with labs from **[GCHE lab]**
 ;"NOTE: GCHE is Greeneville Community Hospital EAST (formally 'Laughlin')
 ;"      I am making a separate XForm file because new GCHE Epic system will likely be different. 
 ;"      FYI -- Pathgroup code is in TMGHL73
 ;"             Laughlin code is in TMGHL74
 ;"             Laughlin RADIOLOGY is in TMGHL74R
 ;"             Quest code is in TMGHL75
 ;"             common code is in TMGHL72
 ;"             GCHE LAB code is TMGHL76
 ;"             GCHE RADIOLOGY code is TMGHL76R
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
        DO TEST^TMGHL71("/mnt/WinServer/LaughlinHL7")
        QUIT
        ;
BATCH   ;"Launch processing through all files in folder.
        DO HLDIRIN^TMGHL71("/mnt/WinServer/LaughlinHL7",1000,10)
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
        DO MSG^TMGHL74
        QUIT
        ;
MSG2    ;"Purpose: Process entire message after processing segments
        DO MSG2^TMGHL74 
        KILL TMGLASTOBR4,TMGLASTOBX3,TMGOBXCOUNT
        QUIT
        ;
MSH3    ;"Purpose: Process MSH segment, FLD 4 (Sending Application)
        QUIT
        ;
MSH4  ;"Purpose: Process MSH segment, FLD 4 (Sending Facility)
        SET TMGVALUE="GREENEVILLE COMMUNITY HOSP E"
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
        DO PV18^TMGHL74
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
        DO ORC13^TMGHL74
        QUIT
        ;
OBR     ;"Purppse: setup for OBR fields.
        ;"Uses TMGHL7MSG,TMGSEGN,TMGU in global scope
        IF $GET(TMGHL7MSG("STAGE"))="PRE" DO  QUIT
        . DO HNDUPOBX^TMGHL72(.TMGHL7MSG,TMGSEGN,.TMGU)
        DO OBR^TMGHL72        
        QUIT
        ;
OBR3    ;"Purpose: To transform the OBR segment, field 3
        QUIT
        ;
OBR4    ;"Purpose: To transform the OBR segment, field 4
        SET TMGLASTOBR4=TMGVALUE  ;"this will be later killed in MSG2^TMGHL76
        SET TMGLASTOBX3=""        ;"Rest since going into different order (OBR)
        SET TMGOBXCOUNT=0
        IF $GET(TMGHL7MSG("STAGE"))="PRE" QUIT
        DO OBR4^TMGHL72
        QUIT
        ;
OBR15   ;"Transform Secimen source
        IF $GET(TMGHL7MSG("STAGE"))="PRE" QUIT
        DO OBR15^TMGHL73
        QUIT
        ;
OBR16   ;"Transform Ordering provider.
        DO OBR16^TMGHL72
        QUIT
        ;
OBRDN   ;"Purpose: setup for OBR fields, called *after* fields, subfields etc are processed
        ;"This allows putting information about the ordered test(s) into the comment section
        ;"Uses globally scoped vars: TMGSEGN, TMGDD
        DO OBRDN^TMGHL74
        QUIT
        ;
OBX3    ;"Purpose: To transform the OBX segment, field 3 -- Observation Identifier
        ;"Input: Uses globally scoped vars: TMGHL7MSG, TMGU, TMGVALUE, IEN62D4,
        ;"       TMGSEGN, TMGINFO, TMGENV
        ;"Example TMGVALUE -- 'CRE^CREATININE'
        SET TMGOBXCOUNT=$GET(TMGOBXCOUNT)+1
        IF TMGVALUE="" DO  ;"For some reason, GCHE is not sending ID for some tests
        . NEW INFO MERGE INFO=TMGHL7MSG(TMGSEGN)
        . NEW OBRNAME SET OBRNAME=$PIECE($GET(TMGLASTOBR4),TMGU(2),2)
        . IF "COMPREHENSIVE METABOLIC PANEL^BASIC METABOLIC PANEL"[OBRNAME DO
        . . ;"IF $PIECE($GET(TMGLASTOBX3),TMGU(2),2)'="GFR" QUIT
        . . IF $GET(INFO(6))="mOsm/k" DO  QUIT  ;"real life example: OBX|19|ST|||285.0|mOsm/k|261.0-280.0|H|||F||4||1230000058|
        . . . SET TMGVALUE="TMG-OSMOC^GCHE-CALC-OSMO"
        . . IF $GET(INFO(6))="Ratio" DO  QUIT  ;"real life example:  OBX|18|ST||||Ratio|7.0-25.0||||F||4||1230000058|
        . . . SET TMGVALUE="TMG-BUN/CR^GCHE-BUN/CR-RATIO"
        . IF "IRON AND TIBC"[OBRNAME DO
        . . IF $GET(INFO(6))="%" DO  QUIT  ;"real life example: OBX|1|ST|||9|%|14-50|L|||F||4||1230000058|
        . . . SET TMGVALUE="TMG-TIBC-PCT-SAT^GCHE-TIBC-PCT-SAT"
        . . IF $GET(INFO(7))="250-400" DO  QUIT  ;"real life example: OBX|2|ST|||426||250-400|H|||F||4||1230000058| 
        . . . SET TMGVALUE="TMG-CUST-TIBC^GCHE-CUST-TIBC"
        . IF TMGVALUE="",TMGOBXCOUNT=1,$GET(TMGLASTOBR4)'="" DO  ;"//real life example: given OBR for CRP, and then isolated OBX with just result for CRP, no name of test
        . . SET TMGVALUE=TMGLASTOBR4
        . IF TMGVALUE="" QUIT
        . NEW TEMPARR,TEST SET TEST=TMGVALUE
        . SET TMGRESULT=$$GETMAP^TMGHL70B(.TMGENV,TEST,"R",.TEMPARR)
        . MERGE TMGHL7MSG(TMGSEGN,"RESULT")=TEMPARR
        . MERGE TMGHL7MSG("RESULT",TMGSEGN)=TEMPARR
        . SET TMGVALUE=$GET(TEMPARR("IEN60"))
        . SET TMGHL7MSG(TMGSEGN,3)=TMGVALUE
        . DO REFRESHM^TMGHL7X2(.TMGHL7MSG,.TMGU,TMGSEGN,TMGFLDN)
        DO OBX3^TMGHL72
        SET TMGLASTOBX3=TMGVALUE
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
        ;"Note: This handles NTE's after OBX's.  
        ;"      NTE's after OBR's are handled in OBRDN
        DO NTE3^TMGHL74                         
        QUIT
        ;
SUORL   ;"Purpose: Setup TMGINFO("ORL") and TMGINFO("LOC") and TMGINFO("INSTNAME")
        DO SUORL^TMGHL72
        QUIT
        ;
