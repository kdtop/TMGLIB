TMGHL78 ;TMG/kst-HL7 transformation engine processing ;6/15/2026
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
 ;"NOTE: this is code for working with labs from **[LabCorp lab -- via WebScrape]**
 ;"      FYI -- Pathgroup code is in TMGHL73
 ;"             Laughlin code is in TMGHL74
 ;"             Laughlin RADIOLOGY is in TMGHL74R
 ;"             Quest code is in TMGHL75
 ;"             common code is in TMGHL72
 ;"             GCHE LAB code is TMGHL76
 ;"             GCHE RADIOLOGY code is TMGHL76R
 ;"             LabCorp Web code is TMGHL78
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
        DO TEST^TMGHL71("/mnt/WinServer/LabCorpScrapedHL7")
        QUIT
        ;
BATCH   ;"Launch processing through all files in folder.
        DO HLDIRIN^TMGHL71("/mnt/WinServer/LabCorpScrapedHL7",1000,10)
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
        KILL TMGIGNOREOBR
        QUIT
        ;
MSH3    ;"Purpose: Process MSH segment, FLD 4 (Sending Application)
        QUIT
        ;
MSH4  ;"Purpose: Process MSH segment, FLD 4 (Sending Facility)
        IF $GET(TMGHL7MSG("STAGE"))="PRE" DO  QUIT
        . DO XMSH4^TMGHL72
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
ORC2   ;"Purpose: Process empty ORC message, field 1
        DO ORC2^TMGHL72
        QUIT
        ;
ORC12  ;"Purpose: Process empty ORC message, field 12
        ;"Note for block below:   //kt 7/24/26
        ;"  Fileman couldn't find unique match for TOPPENBERG,M because both Matthew and Marcia Toppenberg are in system
        IF TMGVALUE="^TOPPENBER^M" DO   
        . SET TMGVALUE="^TOPPENBERG^MARCIA DEE"
        . ;"Ensure TMGHL7MSG changed and refreshed before calling XORC12^TMGHL72, 
        . ;"   which sets up TMGINFO("PROV") preferentially from first OBR.16
        . DO SETPCE^TMGHL7X2(TMGVALUE,.TMGHL7MSG,.TMGU,"OBR",16)   
        . DO SETPCE^TMGHL7X2(TMGVALUE,.TMGHL7MSG,.TMGU,TMGSEGN,TMGFLDN)  ;"Ensure TMGHL7MSG changed before calling XORC12^TMGHL72 
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
        . NEW TEMP SET TEMP=$$HNDUPOBX^TMGHL72(.TMGHL7MSG,TMGSEGN,.TMGU)
        . IF TEMP<0 SET TMGXERR=$PIECE(TEMP,"^",2,99)   
        DO OBR^TMGHL72        
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
        DO OBR15^TMGHL73
        QUIT
        ;
OBR16   ;"Transform Ordering provider.
        IF TMGVALUE="^TOPPENBER^M" DO
        . SET TMGVALUE="^TOPPENBERG^MARCIA DEE"
        IF TMGVALUE["^PROVIDER^HISTORICAL^" DO
        . SET TMGHL7MSG("IGNORE","OBR",TMGSEGN)=1
        DO OBR16^TMGHL72
        QUIT
        ;
OBRDN   ;"Purpose: setup for OBR fields, called *after* fields, subfields etc are processed
        ;"This allows putting information about the ordered test(s) into the comment section
        ;"Uses globally scoped vars: TMGSEGN, TMGDD
        DO OBRDN^TMGHL74
        QUIT
        ;
OBX     ;"Purpose: To transform the entire OBX segment -- Observation Identifier
        ;"Input: Uses globally scoped vars: TMGHL7MSG, TMGU, TMGVALUE, 
        ;"       TMGSEGN, TMGINFO, TMGENV
        DO OBX^TMGHL72
        QUIT
        ;
OBX3    ;"Purpose: To transform the OBX segment, field 3 -- Observation Identifier
        ;"Input: Uses globally scoped vars: TMGHL7MSG, TMGU, TMGVALUE, IEN62D4,
        ;"       TMGSEGN, TMGINFO, TMGENV
        ;"Example TMGVALUE -- 'CRE^CREATININE'
        SET TMGOBXCOUNT=$GET(TMGOBXCOUNT)+1
        DO OBX3^TMGHL72
        SET TMGLASTOBX3=TMGVALUE
        QUIT
        ;
OBX5    ;"Purpose: To transform the OBX segment, field 5 -- Observation value
        NEW LEN SET LEN=$LENGTH(TMGVALUE)
        IF LEN=0 DO   
        . IF $D(TMGHL7MSG("B","NTE",TMGSEGN+1)) DO  QUIT
        . . SET TMGVALUE="(See note below)"
        . SET TMGVALUE=" "
        ELSE  IF LEN>70,TMGSTAGE="PRE" DO  ;"7/20/26
        . NEW CMNT SET CMNT=TMGVALUE
        . SET TMGVALUE="(See note below)"
        . NEW TMP SET TMP(1)=CMNT
        . DO INSRTNTE^TMGHL72(.TMP,.TMGHL7MSG,.TMGU,TMGSEGN)
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
        DO NTE3^TMGHL73                        
        QUIT
        ;
SUORL   ;"Purpose: Setup TMGINFO("ORL") and TMGINFO("LOC") and TMGINFO("INSTNAME")
        DO SUORL^TMGHL72
        QUIT
        ;
