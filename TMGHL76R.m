TMGHL76R ;TMG/kst-HL7 transformation engine processing ;4/11/19, 3/24/21
              ;;1.0;TMG-LIB;**1**;11/14/16
 ;
 ;"TMG HL7 TRANSFORMATION FUNCTIONS
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 4/11/19  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"NOTE: this is code for working with labs from **[GCHE lab]** <-- actually, I think this should be GCHE RAD
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
TEST    ;"Pick file and manually send through filing process.
        NEW OPTION SET OPTION("NO MOVE")=1
        SET OPTION("NO ALERT")=0
        DO TEST^TMGHL71("/mnt/WinServer/LaughlinHL7",.OPTION)
        QUIT
        ;
BATCH   ;"NOTE: Laughlin radiology reports will be dumped in with 
        ;"      lab reports, so I will NOT call this BATCH label separately.
        ;"      Instead, Taskman will call BATCH^TMGHL74, and then the XFORM 
        ;"      process will recognize the LAB application, and load the record 
        ;"      in file #22720 that, in turn, calls the functions in this file.  
        QUIT
        ;
ISLMHRAD(TMGHL7MSG) ;"Test if message is LMH/GCHE rad HL7 message.
        ;"Input: TMGHL7MSG -- PASS BY REFERENCE.  
        ;"Result: 1 if is rad HL7, 0 otherwise
        ;"note: this is called by SETMAPS^TMGHL70B
        NEW APP SET APP=$GET(TMGHL7MSG(1,3))
        NEW SNDR SET SNDR=$GET(TMGHL7MSG(1,4))
        NEW TMGRESULT SET TMGRESULT=0
        IF (APP="RM")&((SNDR="LMH")!(SNDR["LAUGHLIN")!(SNDR["GCHE")) DO
        . SET TMGRESULT=1
        QUIT TMGRESULT
        ;"
        ;"===============================================================
        ;"|  Below are the call-back functions to handle transformation |
        ;"|  hooks, called by the XFMSG^TMGHL7X engine                  |
        ;"===============================================================
        ;
MSG     ;"Purpose: Process entire message before processing segments
        DO FIXRPT(.TMGHL7MSG,.TMGU) 
        DO XMSG^TMGHL72 
        ;"//kt 415/21  KILL TMGLASTOBR4,TMGLASTOBX3,TMGOBXCOUNT,TMGINFO
        QUIT
        ;
MSG2    ;"Purpose: Process entire message after processing segments
        DO XMSG2^TMGHL72 
        ;"//kt 415/21 KILL TMGEXAMIDX,TMGINFO
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
        DO PID^TMGHL74R
        ;"DO PID^TMGHL72        
        ;"NEW TMGDFN SET TMGDFN=$PIECE(TMGVALUE,TMGU(1),4)
        ;"SET TMGHL7MSG("RAD STUDY","DFN")=TMGDFN
        QUIT
        ;
PV18    ;"Purpose: Process entire PV18 segment
        ;"NOTE: Pathgroup doesn't send order information in OBR16, so will use
        ;"      information from here to fix that.
        ;"In Pathrgoup messages, PV1 comes before any OBR segments.
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
        DO XORC13^TMGHL72
        SET $PIECE(TMGVALUE,"^",1)="Family Phys Of Greeneville"
        SET $PIECE(TMGVALUE,"^",2)="69" 
        QUIT
        ;
OBR     ;"Purppse: setup for OBR fields.
        ;"Uses TMGEXAMIDX,TMGSEGN in global scope
        ;"//kt 6/11/20 --> This does mapping for labs, not needed for Rad studies...   DO OBR^TMGHL72

        SET TMGEXAMIDX=+$GET(TMGEXAMIDX)+1  ;"will be killed in MSG2 at end of process
        SET TMGHL7MSG("RAD STUDY",TMGEXAMIDX,"OBR")=TMGSEGN
        QUIT
        ;
OBR4    ;"Purpose: To transform the OBR segment, field 4  'UNIVERSAL SERVICE ID'
        ;"Uses TMGU,TMGEXAMIDX,TMGSEGN in global scope
        ;"Example of input in TMGVALUE: 'BONDE^BONE DENSITOMETRY^^WDC'
        IF $PIECE(TMGVALUE,TMGU(2),1)="" DO
        . IF $PIECE(TMGVALUE,TMGU(2),5)="EKG 12-LEAD" DO
        . . SET $PIECE(TMGVALUE,TMGU(2),1)="EKG12LEAD"
        . . SET $PIECE(TMGVALUE,TMGU(2),2)="EKG 12-LEAD"
        . . ;"---- Finish later.  NOTE: It is the actual filing process that needs to be fixed.  
        . . ;"SET TMGXERR="Finish implementing ECG import in OBR4^TMGHL76R"
        . . ;"SET TMGRESULT="-1^"_TMGXERR
        SET TMGLASTOBR4=TMGVALUE  ;"this will be later killed in MSG2^TMGHL76R
        SET TMGLASTOBX3=""        ;"Rest since going into different order (OBR)
        DO OBR4^TMGHL74R
OBR4DN  QUIT
        ;
OBR7    ;"Purpose: To transform the OBR segment, field 7  'OBSERVATION DATE TIME'
        DO OBR7^TMGHL74R
        QUIT
        ;
OBR16   ;"Transform Ordering provider.
        DO OBR16^TMGHL74R
        QUIT
        ;
OBR32   ;"Transform Principle results interpretor (radiologist)
        ;"Input: Uses globally scoped vars: TMGVALUE,TMGHL7MSG,TMGU
        IF TMGVALUE="",$GET(TMGHL7MSG(TMGSEGN,4,1))="EKG12LEAD" DO
        . SET TMGVALUE=TMGU(2)_"DOCTOR,UNSPECIFIED"
        DO OBR32^TMGHL74R
OBR32DN QUIT        
        ;
OBX5    ;"Purpose: To transform the OBX segment, field 5 -- Observation value
        IF $GET(TMGHL7MSG("STAGE"))'="FINAL" QUIT
        SET TMGEXAMIDX=+$GET(TMGEXAMIDX)  ;"will be killed in MSG2 at end of process
        NEW TEMP MERGE TEMP=TMGHL7MSG(TMGSEGN,5,1)
        NEW OUT DO PARSRPT^TMGHL76R(.OUT,.TEMP) ;"<-- this differs from OBX5^TMGHL74R
        MERGE TMGHL7MSG("RAD STUDY",TMGEXAMIDX)=OUT
        SET TMGHL7MSG("RAD STUDY",TMGEXAMIDX,"OBX")=TMGSEGN
        QUIT
        ;
OBX14   ;"Purpose: To transform the OBX segment, field 14 -- Date/Time of the Observation
        DO OBX14^TMGHL72
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
PARSRPT(OUT,ARR)  ;"Split report into RPT (report), IMP (impression), HX (additional clinical history) sections
        NEW SECTION SET SECTION="RPT"
        NEW OUTIDX SET OUTIDX=1
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:+IDX'>0  DO
        . NEW S SET S=$GET(ARR(IDX))
        . NEW UP SET UP=$$TRIM^XLFSTR($$UP^XLFSTR(S))
        . NEW PARTB SET PARTB=""
        . NEW DIV SET DIV=""
        . IF UP="HISTORY"           SET SECTION="HX",S="HISTORY:"
        . ELSE  IF UP["HISTORY:"    SET SECTION="HX"
        . ELSE  IF UP="FINDINGS"    SET SECTION="RPT",S="FINDINGS:"
        . ELSE  IF UP["TECHNIQUE:"  SET SECTION="RPT"        
        . ELSE  IF UP["CLINICAL INFORMATION:" SET SECTION="HX"        
        . ELSE  IF UP["COMPARISON:" SET SECTION="RPT"                
        . ELSE  IF UP["FINDINGS:"   SET SECTION="RPT"
        . ELSE  IF UP["CONCLUSION:" SET SECTION="IMP",DIV="CONCLUSION:"
        . ELSE  IF UP["IMPRESSION:" SET SECTION="IMP",DIV="IMPRESSION:"
        . ELSE  IF UP["INTERPRETATION SUMMARY" SET SECTION="IMP",DIV="INTERPRETATION SUMMARY"
        . ELSE  IF S["ASSESSMENT:"  SET SECTION="IMP",DIV="ASSESSMENT:"
        . ;"ELSE  IF S["IMPRESSION"   SET SECTION="IMP",DIV="IMPRESSION"   
        . IF DIV'="" DO
        . . DO CLEAVSTR^TMGSTUT2(.S,DIV,.PARTB,1)
        . . IF S'="" SET OUT(SECTION,OUTIDX)=S
        . . SET S=PARTB,OUTIDX=1
        . IF S["Signer Name:" DO
        . . SET OUT(SECTION,OUTIDX)="",OUTIDX=OUTIDX+1        
        . . SET OUT(SECTION,OUTIDX)="-------------------",OUTIDX=OUTIDX+1        
        . IF OUTIDX=1,S="" QUIT
        . SET OUT(SECTION,OUTIDX)=S,OUTIDX=OUTIDX+1        
        QUIT
        ;        
FIXRPT(TMGHL7MSG,TMGU) ;"
        ;"Purpose: to covert form of EPIC radiology report messages into that previously
        ;"         handled for Laughlin radiology
        IF $$CHK4OBGWCG^TMGHL7U2("",.TMGU) DO   ;"test based from TMGMSG, in glocal scope
        . IF $GET(TMGHL7MSG("STAGE"))'="FINAL" QUIT
        . DO FIXOBGWCGRPT(.TMGHL7MSG,.TMGU)
        NEW ARRAY,ARRI SET ARRI=0
        NEW OBRFOUND,OBXROUND,DONE SET (OBRFOUND,OBXFOUND,DONE)=0
        NEW STATUS SET STATUS=""
        NEW NTEINSERTNUM SET NTEINSERTNUM=0
        NEW SEGNUM SET SEGNUM=0
        FOR  SET SEGNUM=$ORDER(TMGHL7MSG(SEGNUM)) QUIT:SEGNUM'>0  DO
        . NEW SEGNAME SET SEGNAME=$GET(TMGHL7MSG(SEGNUM,"SEG"))
        . IF (OBRFOUND=0) DO  QUIT
        . . IF SEGNAME="OBR" DO
        . . . SET OBRFOUND=1 QUIT
        . IF OBRFOUND,SEGNAME="OBX" DO
        . . SET OBXFOUND=1
        . . NEW TEST SET TEST=$GET(TMGHL7MSG(SEGNUM,3))        
        . . ;"ELH ADDED &ADT FOR ADDENDUMS  7/25/19
        . . IF (TEST'="&ADT")&(TEST'="&GDT")&(TEST'="&IMP") SET DONE=1 QUIT
        . . IF STATUS="" SET STATUS=$GET(TMGHL7MSG(SEGNUM,11))
        . . IF NTEINSERTNUM=0 SET NTEINSERTNUM=SEGNUM
        . . NEW LINE SET LINE=$GET(TMGHL7MSG(SEGNUM,5))
        . . SET ARRI=ARRI+1,ARRAY(ARRI)=LINE
        . . KILL TMGHL7MSG(SEGNUM)
        . . KILL TMGHL7MSG("RESULT",SEGNUM)
        . . KILL TMGHL7MSG("B","OBX",SEGNUM)
        . . IF $GET(TMGHL7MSG("PO",SEGNUM))=SEGNUM KILL TMGHL7MSG("PO",SEGNUM)
        . ELSE  IF OBRFOUND,OBXFOUND SET DONE=1
        . IF OBRFOUND,OBXFOUND,DONE DO
        . . DO PUSHARRAY(.TMGHL7MSG,.TMGU,.ARRAY,NTEINSERTNUM,STATUS)
        . . SET TMGHL7MSG("PO",NTEINSERTNUM)=NTEINSERTNUM
        . . SET (OBRFOUND,OBXFOUND,DONE)=0
        . . SET STATUS=""
        . . SET ARRI=0 KILL ARRAY
        IF $DATA(ARRAY) DO PUSHARRAY(.TMGHL7MSG,.TMGU,.ARRAY,NTEINSERTNUM,STATUS)
        QUIT
        ;
FIXOBGWCGRPT(TMGHL7MSG,TMGU)  ;"Fix crazy US reports coming from GCHE OB group (GCHW OB WCG)
        ;"Note: Some of the report is in NTE segments after OBR, and some of report
        ;"      is in OBX segments.  
        ;"To make like other RAD reports, should be : OBX|#|ST|&IMP|1| abcdefg text...  
        NEW ARRAY,ARRI SET ARRI=0
        NEW OBRFOUND,OBXROUND,DONE SET (OBRFOUND,OBXFOUND,DONE)=0
        NEW STATUS SET STATUS=""
        NEW NTEINSERTNUM SET NTEINSERTNUM=0
        NEW SEGNUM SET SEGNUM=0
        FOR  SET SEGNUM=$ORDER(TMGHL7MSG(SEGNUM)) QUIT:SEGNUM'>0  DO
        . NEW SEGNAME SET SEGNAME=$GET(TMGHL7MSG(SEGNUM,"SEG"))
        . IF (OBRFOUND=0) DO  QUIT
        . . IF SEGNAME="OBR" DO
        . . . SET OBRFOUND=1 QUIT
        . IF OBRFOUND,(OBXFOUND=0),SEGNAME="NTE" DO
        . . NEW LINE SET LINE=$GET(TMGHL7MSG(SEGNUM,3))
        . . SET ARRI=ARRI+1,ARRAY(ARRI)=LINE
        . . KILL TMGHL7MSG(SEGNUM)
        . . KILL TMGHL7MSG("RESULT",SEGNUM)
        . . KILL TMGHL7MSG("B","NTE",SEGNUM)
        . . IF $GET(TMGHL7MSG("PO",SEGNUM))=SEGNUM KILL TMGHL7MSG("PO",SEGNUM)
        . IF OBRFOUND,SEGNAME="OBX" DO
        . . IF OBXFOUND=0,$DATA(ARRAY)>0 DO
        . . . SET ARRI=ARRI+1,ARRAY(ARRI)=" "
        . . . SET ARRI=ARRI+1,ARRAY(ARRI)=" ---------------------- "
        . . . SET ARRI=ARRI+1,ARRAY(ARRI)=" "
        . . SET OBXFOUND=1
        . . IF NTEINSERTNUM=0 SET NTEINSERTNUM=SEGNUM
        . . NEW ID SET ID=$GET(TMGHL7MSG(SEGNUM,3))
        . . FOR  QUIT:($EXTRACT(ID,1)'=TMGU(2))  SET ID=$EXTRACT(ID,2,$LENGTH(ID))
        . . NEW VAL SET VAL=$GET(TMGHL7MSG(SEGNUM,5))
        . . NEW UNITS SET UNITS=$GET(TMGHL7MSG(SEGNUM,6))
        . . NEW LINE SET LINE=ID_": "_VAL_" "_UNITS
        . . SET ARRI=ARRI+1,ARRAY(ARRI)=LINE
        . . IF STATUS="" SET STATUS=$GET(TMGHL7MSG(SEGNUM,11))
        . . KILL TMGHL7MSG(SEGNUM)
        . . KILL TMGHL7MSG("RESULT",SEGNUM)
        . . KILL TMGHL7MSG("B","OBX",SEGNUM)
        . . IF $GET(TMGHL7MSG("PO",SEGNUM))=SEGNUM KILL TMGHL7MSG("PO",SEGNUM)
        . ELSE  IF OBRFOUND,OBXFOUND SET DONE=1
        . IF OBRFOUND,OBXFOUND,DONE DO
        . . DO PUSHA4OB(.TMGHL7MSG,.TMGU,.ARRAY,NTEINSERTNUM,STATUS)
        . . SET TMGHL7MSG("PO",NTEINSERTNUM)=NTEINSERTNUM
        . . SET (OBRFOUND,OBXFOUND,DONE)=0
        . . SET STATUS=""
        . . SET ARRI=0 KILL ARRAY
        IF $DATA(ARRAY) DO PUSHA4OB(.TMGHL7MSG,.TMGU,.ARRAY,NTEINSERTNUM)
        QUIT
PUSHA4OB(TMGHL7MSG,TMGU,ARRAY,SEGNUM,STATUS) ;
        NEW DIV1 SET DIV1=$GET(TMGU(1),"|")
        NEW DIV3 SET DIV3=$GET(TMGU(3),"~")
        NEW INC SET INC=1
        NEW STR SET STR=""
        NEW ARRI SET ARRI=0
        FOR  SET ARRI=$ORDER(ARRAY(ARRI)) QUIT:ARRI'>0  DO
        . NEW LINE SET LINE=$GET(ARRAY(ARRI))
        . SET LINE="OBX"_DIV1_ARRI_DIV1_"ST"_DIV1_"&IMP"_DIV1_"1"_DIV1_LINE
        . SET $PIECE(LINE,DIV1,11+1)=STATUS
        . SET TMGHL7MSG(SEGNUM)=LINE
        . NEW LASTPOIDX SET LASTPOIDX=$ORDER(TMGHL7MSG("PO",""),-1) 
        . SET TMGHL7MSG("PO",LASTPOIDX+1)=SEGNUM
        . FOR  QUIT:($DATA(TMGHL7MSG(SEGNUM+INC))=0)  SET INC=INC/10
        . SET SEGNUM=SEGNUM+INC
        SET (OBRFOUND,OBXFOUND,DONE)=0        
        KILL ARRAY
        DO REFRESHM^TMGHL7X2(.TMGHL7MSG,.TMGU)
        QUIT
        ;
PUSHARRAY(TMGHL7MSG,TMGU,ARRAY,SEGNUM,STATUS) ;
        NEW DIV1 SET DIV1=$GET(TMGU(1),"|")
        NEW DIV3 SET DIV3=$GET(TMGU(3),"~")
        NEW STR SET STR=""
        NEW ARRI SET ARRI=0
        FOR  SET ARRI=$ORDER(ARRAY(ARRI)) QUIT:ARRI'>0  DO
        . IF STR'="" SET STR=STR_DIV3
        . SET STR=STR_$GET(ARRAY(ARRI))
        NEW SEG SET SEG="OBX"_DIV1_DIV1_DIV1_"R^REPORT^L"_DIV1_DIV1_STR
        SET $PIECE(SEG,DIV1,11+1)=STATUS SET STATUS=""
        SET TMGHL7MSG(SEGNUM)=SEG
        SET (OBRFOUND,OBXFOUND,DONE)=0
        NEW LASTPOIDX SET LASTPOIDX=$ORDER(TMGHL7MSG("PO",""),-1) 
        SET TMGHL7MSG("PO",LASTPOIDX+1)=SEGNUM 
        KILL ARRAY
        DO REFRESHM^TMGHL7X2(.TMGHL7MSG,.TMGU)
        QUIT
        ;
FILERAD(TMGENV,TMGHL7MSG)  ;
        ;"Input: TMGENV -- PASS BY REFERENCE
        ;"       TMGHL7MSG -- PASS BY REFERENCE
        ;"Results: 1 if OK, or -1^Message if error
        ;"Here I will file the radiology report.  
        ;"Use code in TMGRAU01 for filing...
        QUIT $$FILERAD^TMGHL74R(.TMGENV,.TMGHL7MSG) 
        ;
NOFILE(TMGENV,TMGHL7MSG)  ;"**DON'T** FILE THE MESSAGE
        ;"NOTE: This is a NULL sink, where by the message is NOT filed
        ;"      This is used for some HL7 messages that contain info
        ;"      that does not need to be stored, and should be ignored. 
        ;"Input: TMGENV -- PASS BY REFERENCE
        ;"       TMGHL7MSG -- PASS BY REFERENCE
        ;"Results: 1^OK, 
        QUIT "1^OK"
        ;