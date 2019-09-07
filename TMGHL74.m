TMGHL74 ;TMG/kst-HL7 transformation engine processing ;8/14/15, 6/14/17, 4/11/19
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
        DO KILLDNRS^TMGHL72(.TMGHL7MSG)
        DO XMSG^TMGHL72 
        QUIT
        ;
MSG2    ;"Purpose: Process entire message after processing segments
        DO XMSG2^TMGHL72 
        IF $GET(TMGHL7MSG("STAGE"))="PRE" QUIT
        DO XMSG2B^TMGHL72
        QUIT
        ;
MSH3    ;"Purpose: Process MSH segment, FLD 4 (Sending Application)
        ;"SET TMGVALUE="LA7V HOST LMH"
        QUIT
        ;
MSH4  ;"Purpose: Process MSH segment, FLD 4 (Sending Facility)
        ;"SET TMGVALUE="LAUGHLIN MEM HOSPITAL"
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
        SET $PIECE(TMGVALUE,"^",1)="Family Phys Of Greeneville"
        SET $PIECE(TMGVALUE,"^",2)="69" 
        QUIT
        ;
OBR     ;"Purppse: setup for OBR fields.
        ;"Uses TMGHL7MSG,TMGSEGN,TMGU in global scope
        IF $GET(TMGHL7MSG("STAGE"))="PRE" DO  QUIT
        . DO HNDUPOBX^TMGHL72(.TMGHL7MSG,TMGSEGN,.TMGU)
        DO OBR^TMGHL72
        
        QUIT
OBR4    ;"Purpose: To transform the OBR segment, field 4
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
        IF $GET(TMGHL7MSG("STAGE"))="PRE" QUIT
        NEW TEMP
        DO LABLDATA^TMGHL72(.TEMP,.TMGHL7MSG,"OBR",TMGSEGN) ;
        ;
        NEW ORDINFO MERGE ORDINFO=TMGHL7MSG("ORDER",TMGSEGN)
        NEW TESTNAME SET TESTNAME=$PIECE($GET(TMGHL7MSG("ORDER",TMGSEGN,"IEN60")),"^",2)
        ;
        NEW INFO,PROV,PID  
        NEW ONEACSN SET ONEACSN=$PIECE($GET(TEMP("Filler Order Number")),"^",1)
        NEW LABADDR SET LABADDR=$GET(TEMP("Filler Field 2"))
        SET PROV=$GET(TEMP("Ordering Provider"))
        SET PROV=$$HL7N2FMN^TMGHL72(.TMGU,PROV)
        ;"SET PROV=$PIECE(PROV,TMGU(2),2,3)
        ;"SET PROV=$$TRIM^XLFSTR($TRANSLATE(PROV,"^"," "))
        IF PROV="" DO
        . NEW TEMP2 DO LABLDATA^TMGHL72(.TEMP2,.TMGHL7MSG,"ORC") 
        . SET PROV=$GET(TEMP2("Ordering Provider"))
        . SET PROV=$$HL7N2FMN^TMGHL72(.TMGU,PROV)
        . ;"SET PROV=$PIECE(PROV,TMGU(2),2,3)
        . ;"SET PROV=$TRANSLATE(PROV,"^"," ")
        IF PROV["DOCTOR",PROV["UNSPECIFIED",$DATA(TMGINFO("PROV","ORIGINAL")) DO
        . SET PROV=$$HL7N2FMN^TMGHL72(.TMGU,$GET(TMGINFO("PROV","ORIGINAL")))
        . IF PROV["" SET PROV=$TRANSLATE(PROV,"""","'") 
        NEW OBSDT SET OBSDT=$GET(TEMP("Observation Date/Time"))
        SET OBSDT=$$HL72FMDT^TMGHL7U3(OBSDT)
        SET OBSDT=$$FMTE^XLFDT(OBSDT)
        NEW RECDT SET RECDT=$GET(TEMP("Specimen Received Date/Time"))
        SET RECDT=$$HL72FMDT^TMGHL7U3(RECDT)
        SET RECDT=$$FMTE^XLFDT(RECDT)        
        NEW RPTDT SET RPTDT=$GET(TEMP("Results Rpt/Status Chng - Date/Time"))
        SET RPTDT=$$HL72FMDT^TMGHL7U3(RPTDT)
        SET RPTDT=$$FMTE^XLFDT(RPTDT)
        ;"NEW STATUS SET STATUS=$GET(TEMP("Result Status"))
        ;"IF STATUS="F" SET STATUS="FINAL"
        ;"IF STATUS="I" SET STATUS="INCOMPLETE/PRELIMINARY"
        ;"IF STATUS="C" SET STATUS="CORRECTED"
        ;"IF STATUS="P" SET STATUS="PRELIMINARY"
        ;"IF STATUS="X" SET STATUS="TEST CANCELED"
        NEW STATARR DO SUMOBXSTA^TMGHL72(.TMGHL7MSG,TMGSEGN,.STATARR)
        NEW OBRCOMMENTS DO CHKOBRNT^TMGHL72(.TMGHL7MSG,TMGSEGN,.OBRCOMMENTS) ;"Handle OBR notes
        ;       
        NEW TEMP2 DO LABLDATA^TMGHL72(.TEMP2,.TMGHL7MSG,"PID") ;
        NEW PID SET PID=$GET(TEMP2("Patient ID"))
        IF PID="" SET PID=$GET(TEMP2("Alternate Patient ID - PID"))
        IF PID="" SET PID=$GET(TEMP2("SSN Number - Patient"))
        NEW GENDER SET GENDER=$GET(TEMP2("Sex"))
        IF GENDER="F" SET GENDER="FEMALE"
        IF GENDER="M" SET GENDER="MALE"
        NEW PTDOB SET PTDOB=$GET(TEMP2("Date/Time Of Birth"))
        SET PTDOB=$$HL72FMDT^TMGHL7U3(PTDOB)
        SET PTDOB=$$FMTE^XLFDT(PTDOB,"2D")
        NEW PTNAME SET PTNAME=$TRANSLATE($GET(TEMP2("Patient Name")),TMGU(2),",")
        NEW ACCTN SET ACCTN=$GET(TEMP2("Patient Account Number"))
        NEW PATIENT SET PATIENT=PTNAME_" ("_PTDOB_"), "_GENDER
        IF ACCTN'="" SET PATIENT=PATIENT_", Acct #"_ACCTN
        ;
        NEW LINE,ARR,FLD,VALUE SET FLD=""   
        NEW INDENT SET INDENT="  "
        DO ADDTOARR^TMGHL72(.ARR,$$DBLN^TMGHL72())
        DO ADD2ARRI^TMGHL72(.ARR,"Test ordered: ",TESTNAME)
        DO ADD2ARRI^TMGHL72(.ARR,"Ordering Provider: ",PROV)
        DO ADD2ARRI^TMGHL72(.ARR,"Lab Accession Number: ",ONEACSN)
        DO ADD2ARRI^TMGHL72(.ARR,"Patient: ",PATIENT)
        DO ADD2ARRI^TMGHL72(.ARR,"Lab Patient ID: ",PID)
        SET LINE="Specimen Collection Date: "_OBSDT
        IF $GET(TMGHL75OBRCOLDT)=1 SET LINE=LINE_" <-- see *NOTE*"
        DO ADDTOARR^TMGHL72(.ARR,LINE)
        IF $GET(TMGHL75OBRCOLDT)=1 DO
        . DO ADDTOARR^TMGHL72(.ARR,"  *NOTE*: Collection date/time not provided.")    
        . DO ADDTOARR^TMGHL72(.ARR,"          Using date/time lab RECEIVED instead.")    
        KILL TMGHL75OBRCOLDT
        DO ADD2ARRI^TMGHL72(.ARR,"Specimen Received Date: ",RECDT)
        DO ADD2ARRI^TMGHL72(.ARR,"Result Report Date: ",RPTDT)
        ;"DO ADD2ARRI^TMGHL72(.ARR,"Result Status: ",STATUS)
        DO ADDA2ARR^TMGHL72(.ARR,.STATARR) 
        DO ADDA2ARR^TMGHL72(.ARR,.OBRCOMMENTS)  ;"nothing added if array empty 
        DO ADDTOARR^TMGHL72(.ARR,$$DBLN^TMGHL72())
        ;           
        DO INSRTNTE^TMGHL72(.ARR,.TMGHL7MSG,.TMGU,TMGSEGN)  
        QUIT
        ;
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
        ;"Note: This handles NTE's after OBX's.  
        ;"      NTE's after OBR's are handled in OBRDN
        DO NTE3^TMGHL72                         
        IF $GET(TMGHL7MSG("STAGE"))="PRE" QUIT
        ;"NEW OBXIDX SET OBXIDX=+$GET(TMGINFO("MOST RECENT OBX","SEGN"))
        NEW OBXIDX SET OBXIDX=+$GET(TMGLASTOBX("SEGN"))
        IF $ORDER(TMGHL7MSG(OBXIDX))=TMGSEGN DO
        . ;"NEW LABNAME SET LABNAME=$GET(TMGINFO("MOST RECENT OBX")) QUIT:LABNAME=""
        . NEW LABNAME SET LABNAME=$GET(TMGLASTOBX("NAME")) QUIT:LABNAME=""
        . NEW LINE SET LINE="Comment for: "_LABNAME
        . DO PREFIXNT^TMGHL72(LINE,.TMGHL7MSG,.TMGU,TMGSEGN)  ;"PREFIX NOTE (INSERT LINE BEFORE INDEX LINE)
        IF $$ISFINALN^TMGHL72(.TMGHL7MSG,TMGSEGN) DO
        . NEW ARR SET ARR(1)=$$DBLN^TMGHL72
        . DO APPNDNTE^TMGHL72(.ARR,.TMGHL7MSG,.TMGU,TMGSEGN)   ;"APPEND LINE AFTER NOTE 
        QUIT
        ;
  ;"SUPROV  ;"Purpose: Setup TMGINFO("PROV") -- Ordering provider.
  ;"        DO SUPROV^TMGHL72                
  ;"        QUIT   
        ;
SUORL   ;"Purpose: Setup TMGINFO("ORL") and TMGINFO("LOC") and TMGINFO("INSTNAME")
        DO SUORL^TMGHL72
        QUIT
        ;
