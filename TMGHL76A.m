TMGHL76A ;TMG/kst-HL7 transformation engine processing ;7/30/19
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
 ;"NOTE: this is code for working with ADT messages from **[GCHE]**
 ;"NOTE: GCHE is Greeneville Community Hospital EAST (formally 'Laughlin')
 ;"      I am making a separate XForm file because new GCHE Epic system will likely be different. 
 ;"      FYI -- Pathgroup code is in TMGHL73
 ;"             Laughlin code is in TMGHL74
 ;"             Laughlin RADIOLOGY is in TMGHL74R
 ;"             Quest code is in TMGHL75
 ;"             common code is in TMGHL72
 ;"             GCHE LAB code is TMGHL76
 ;"             GCHE RADIOLOGY code is TMGHL76R
 ;"             GCHE ADT code is TMGHL76A
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
        SET OPTION("NO ALERT")=1
        DO TEST^TMGHL71("/mnt/WinServer/BalladADTHL7",.OPTION)
        QUIT
        ;
BATCH   ;"NOTE: Laughlin radiology reports will be dumped in with 
        ;"      lab reports, so I will NOT call this BATCH label separately.
        ;"      Instead, Taskman will call BATCH^TMGHL74, and then the XFORM 
        ;"      process will recognize the LAB application, and load the record 
        ;"      in file #22720 that, in turn, calls the functions in this file.  
        QUIT
        ;
        ;"===============================================================
        ;"|  Below are the call-back functions to handle transformation |
        ;"|  hooks, called by the XFMSG^TMGHL7X engine                  |
        ;"===============================================================
        ;
MSG     ;"Purpose: Process entire message before processing segments
        DO FIXRPT(.TMGHL7MSG,.TMGU) 
        DO XMSG^TMGHL72 
        KILL TMGLASTOBR4,TMGLASTOBX3,TMGOBXCOUNT
        QUIT
        ;
MSG2    ;"Purpose: Process entire message after processing segments
        DO XMSG2^TMGHL72 
        KILL TMGEXAMIDX
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
        NEW DFN SET DFN=$PIECE(TMGVALUE,TMGU(1),4)
        SET TMGHL7MSG("RAD STUDY","DFN")=DFN
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
        . . DO PUSHARRAY(.TMGHL7MSG,.TMGU,.ARRAY,NTEINSERTNUM)
        . . SET TMGHL7MSG("PO",NTEINSERTNUM)=NTEINSERTNUM
        . . SET (OBRFOUND,OBXFOUND,DONE)=0
        . . SET STATUS=""
        . . SET ARRI=0 KILL ARRAY
        IF $DATA(ARRAY) DO PUSHARRAY(.TMGHL7MSG,.TMGU,.ARRAY,NTEINSERTNUM)
        QUIT
        ;
PUSHARRAY(TMGHL7MSG,TMGU,ARRAY,SEGNUM) ;" <-- this differs from PUSHARRAY^TMGHL74R
        NEW DIV1 SET DIV1=$GET(TMGU(1),"~")
        NEW DIV3 SET DIV3=$GET(TMGU(3),"~")
        NEW STR SET STR=""
        NEW ARRI SET ARRI=0
        FOR  SET ARRI=$ORDER(ARRAY(ARRI)) QUIT:ARRI'>0  DO
        . IF STR'="" SET STR=STR_DIV3
        . SET STR=STR_$GET(ARRAY(ARRI))
        NEW SEG SET SEG="OBX"_DIV1_DIV1_DIV1_"R^REPORT^L"_DIV1_DIV1_STR
        SET $PIECE(SEG,DIV1,11+1)=STATUS SET STATUS=""
        SET TMGHL7MSG(NTEINSERTNUM)=SEG
        SET (OBRFOUND,OBXFOUND,DONE)=0
        NEW LASTPOIDX SET LASTPOIDX=$ORDER(TMGHL7MSG("PO",""),-1) ;" <-- this differs from PUSHARRAY^TMGHL74R
        SET TMGHL7MSG("PO",LASTPOIDX+1)=NTEINSERTNUM  ;" <-- this differs from PUSHARRAY^TMGHL74R
        KILL ARRAY
        DO REFRESHM^TMGHL7X2(.TMGHL7MSG,.TMGU)
        QUIT
        ;
FILEADT(TMGENV,TMGHL7MSG)  ;
        ;"Input: TMGENV -- PASS BY REFERENCE
        ;"       TMGHL7MSG -- PASS BY REFERENCE
        ;"Results: 1 if OK, or -1^Message if error
        ;"Here I will file the ADT report.  
        ;"Use code in TMGRAU01 for filing...
        ;"ZWR TMGENV
        ;"W "******************************",!
        ;"ZWR TMGHL7MSG

        NEW ADTEVENT,TMGDUZ,MESSAGE,TMGRESULT,ADTDATE,ADTEDATEPCPDUZ
  GOTO FADN
        SET ADTEVENT=$G(TMGHL7MSG(2,1))
        SET ADTDATE=$$HL72FMDT^TMGHL7U3($G(TMGHL7MSG(2,2)))
        SET ADTEDATE=$$EXTDATE^TMGDATE(ADTDATE)
        NEW DFN SET DFN=$G(TMGHL7MSG(3,4))
        DO GETPROV^TMGPROV1(.PCPDUZ,DFN,0)
        IF ADTEVENT="" DO  ;"ALERT EDDIE AS AN ERROR HAS OCCURRED
        . SET TMGDUZ=150
        . SET MESSAGE="ERROR: ADTEVENT COULD NOT BE FOUND IN MESSAGE "
        ELSE  IF ADTEVENT="A01" DO  ;"ADMISSION
        . IF PCPDUZ'>0 DO
        . . SET TMGDUZ=150
        . . SET MESSAGE="ERROR: PCP COULD NOT BE DETERMINED FOR "_$P($G(^DPT(DFN,0)),"^",1)
        . ELSE  DO
        . . SET TMGDUZ=PCPDUZ
        . . SET MESSAGE=" ADMITTED "
        ELSE  IF ADTEVENT="A03" DO  ;"DISCHARGE
        . SET TMGDUZ=150
        . SET MESSAGE=" DISCHARGED "
        IF TMGDUZ'>0 GOTO FADTDN
        IF MESSAGE'["ERROR" SET MESSAGE=$P($G(^DPT(DFN,0)),"^",1)_" WAS "_MESSAGE_" ON "_ADTEDATE
FADN
        NEW DFN SET DFN=74592
        SET TMGDUZ=150
        SET MESSAGE="PATIENT WAS ADMITTED"
        ;"SET XQADATA="OR,"_DFN_",22701;150;3190901.190738"
        NEW XQA,XQAARCH,XQADATA,XQAFLG,XQAGUID,XQAID,XQAMSG,XQAOPT,XQAROU,XQASUPV,XQASURO,XQATEXT,XQALERR
        SET XQADATA="OR,"_DFN_",22701;150;3190901.190738"
        SET XQA(TMGDUZ)="" 
        SET XQAMSG=MESSAGE
        DO SETUP1^XQALERT
    

        ;"DO INFRMALT^TMGXQAL(.RESULT,TMGDUZ,MESSAGE)        
        ;"NEW RECIPS SET RECIPS(TMGDUZ)=""
        ;"DO SNDALRT2(DFN,ADTDATE,RECIPS)
               
FADTDN
        QUIT 1  ;"DO FILING HERE FOR NOW... $$FILEADT^TMGHL76F(.TMGENV,.TMGHL7MSG) 
        ;
SNDALRT2(RADFN,IDT,RECIPS)  ;
  ;" Input: RADFN:    Patient DFN (IEN IN RAD/NUC MED PATIENT)
  ;"        IDT:      Exam timestamp (inverse)  (IEN IN REGISTERED EXAMS
  ;70.02)
  ;"        IEN70D03: Exam IEN70.03 (EXMINATION SUBFILE)
  ;"        RECIPS:   PASS BY REFERENCE.  List of recipients of alert. 
  ;Format:
  ;"              RECIPS(IEN200)=""
  ;"        OPTION:   OPTIONAL.  PASS BY REFERENCE.  FORMAT:
  ;"            OPTION("LEVEL") = 1 (DEFAULT) --> NON-CRITICAL
  ;"                            = 2 --> Abnormal, needs attention
  ;"            OPTION("ADDENDUM") = 0 (DEFAULT) --> not addended
  ;"                               = 1 --> addended.
  SET TMGRESULT="1^OK"
  SET RECIPS(150)=""
  SET IDT="3190101.1111"
  ;"NEW ZN SET ZN=$GET(^RADPT(RADFN,"DT",IDT,"P",IEN70D03,0))
  ;"NEW RARPT SET RARPT=+$PIECE(ZN,"^",17)  ;"This is an IEN74 <-- REQUIRED in  OE3^RAUTL00
  ;"IF RARPT'>0 DO  GOTO SA2DN
  ;". SET TMGRESULT="-1^No linked report in field #17 of file 70.03. 
  ;"IENS="_IEN70D03_","_IDT_","_RADFN_","
  NEW RA751,RAIENS,RAMSG,RANOTE,RAOIFN,RAREQPHY,X1
  SET X1="" ;"=$SELECT($DATA(^RAMIS(71,+$PIECE(X,"^",2),0)):$PIECE(^(0),"^"),1:"")
  SET RA751=""  ;"$GET(^RAO(75.1,+$PIECE(X,"^",11),0))
  SET RAIENS=IDT_"~" ;"_IEN70D03
  SET RANOTE="22701^HL7 MESSAGE:  "_$EXTRACT(X1,1,25)
  SET RAMSG=$PIECE($GET(RANOTE),"^",2)
  SET RAOIFN="" ;"$PIECE(RA751,"^",7)
  MERGE RAREQPHY=RECIPS
  DO EN^ORB3(+$GET(RANOTE),RADFN,RAOIFN,.RAREQPHY,RAMSG,RAIENS)
SA2DN ;
  QUIT TMGRESULT
  ;"