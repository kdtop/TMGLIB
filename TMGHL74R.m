TMGHL74R ;TMG/kst-HL7 transformation engine processing ;11/14/16
              ;;1.0;TMG-LIB;**1**;11/14/16
 ;
 ;"TMG HL7 TRANSFORMATION FUNCTIONS
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 11/14/16  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"NOTE: this is code for working with **radiology** from **[Laughlin]**
 ;"      FYI --    Laughlin LAB is in TMGHL74
 ;"                Pathgroup code is in TMGHL73
 ;"                common code is in TMGHL72
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
        ;"===============================================================
        ;"|  Below are the call-back functions to handle transformation |
        ;"|  hooks, called by the XFMSG^TMGHL7X engine                  |
        ;"===============================================================
        ;
MSG     ;"Purpose: Process entire message before processing segments
        DO XMSG^TMGHL72 
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
OBR     ;"Purppse: setup for OBR fields.
        ;"Uses TMGEXAMIDX,TMGSEGN in global scope
        DO OBR^TMGHL72
        SET TMGEXAMIDX=+$GET(TMGEXAMIDX)+1  ;"will be killed in MSG2 at end of process
        SET TMGHL7MSG("RAD STUDY",TMGEXAMIDX,"OBR")=TMGSEGN
        QUIT
        ;
OBR4    ;"Purpose: To transform the OBR segment, field 4  'UNIVERSAL SERVICE ID'
        ;"Uses TMGU,TMGEXAMIDX,TMGSEGN in global scope
        ;"Example of input in TMGVALUE: 'BONDE^BONE DENSITOMETRY^^WDC'
        NEW TMGFDA,TMGMSG,TMGIEN,ZN
        NEW INST SET INST=71  ;"HARD CODED TO LAUGHLIN IEN IN FILE #4
        NEW CODE SET CODE=$PIECE(TMGVALUE,TMGU(2),1)
        IF CODE="" DO  GOTO OBR4DN
        . SET TMGXERR="N OBR4^TMGHL74R, ERROR.  Study CODE not provided."
        NEW LMHCODE SET LMHCODE="LMH-"_CODE
        NEW NAME SET NAME=$PIECE(TMGVALUE,TMGU(2),2)
        NEW PROCIEN SET PROCIEN=+$ORDER(^RAMIS(71,"E",LMHCODE,0))
        IF PROCIEN>0 GOTO OBR4B
OBR4A   ;"-- ADD NEW PROCEDURE RECORD INF #71     
        SET TMGFDA(71,"+1,",.01)=NAME
        SET TMGFDA(71,"+1,",6)="D"  ;"DETAILED"  -- TYPE OF PROCEDURE
        SET TMGFDA(71,"+1,",8)="N"  ;"NO"  -ORIGINAL PROCEDURE?
        ;"SET TMGFDA(71,"+1,",9)=CPTIEN  ;"CPT CODE
        SET TMGFDA(71,"+1,",12)="1"  ;"'GENERAL RADIOLOGY' IEN -- hard coded IEN
        DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO OBR4DN
        . SET TMGXERR="IN OBR4^TMGHL74R, ERROR while adding to file #71.  Msg:"_$$GETERRST^TMGDEBU2(TMGMSG("DIERR"))
        SET PROCIEN=$GET(TMGIEN(1))
        IF PROCIEN'>0 DO  GOTO OBR4DN
        . SET TMGXERR="IN OBR4^TMGHL74R, Unable to located added record in #71."        
        ;"Add Synonym for record                 
        KILL TMGFDA,TMGMSG,TMGIEN
        SET TMGFDA(71.01,"+1,"_PROCIEN_",",.01)=LMHCODE
        DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO OBR4DN
        . SET TMGXERR="IN OBR4^TMGHL74R, ERROR while adding synonym to file #71.01.  Msg:"_$$GETERRST^TMGDEBU2(TMGMSG("DIERR"))
OBR4B   SET TMGHL7MSG("RAD STUDY",TMGEXAMIDX,"PROC")=PROCIEN
        ;"Ensure CPT is set up for study (will fail when new proc added -- user can fix via alert handler)
        NEW CPTIEN SET CPTIEN=0
        NEW CPT SET CPT=""
        SET ZN=$GET(^RAMIS(71,PROCIEN,0))
        SET CPTIEN=$PIECE(ZN,"^",9)
        IF CPTIEN'>0 DO  GOTO OBR4DN
        . SET TMGXERR="Missing CPT: File# 71, record# "_PROCIEN_", field# 9, does not contain CPT code.  Please fix."
        SET CPT=$PIECE($GET(^ICPT(CPTIEN,0)),"^",1)
        SET TMGHL7MSG("RAD STUDY",TMGEXAMIDX,"CPT")=CPT
OBR4DN  QUIT
        ;
OBR7    ;"Purpose: To transform the OBR segment, field 7  'OBSERVATION DATE TIME'
        SET TMGHL7MSG("RAD STUDY",TMGEXAMIDX,"DT")=TMGVALUE
        QUIT
        ;
OBR16   ;"Transform Ordering provider.
        DO OBR16^TMGHL72
        NEW PROV SET PROV=$GET(TMGINFO("PROV"))
        IF +PROV'>0 DO  GOTO OBR16DN
        . SET TMGXERR="IN OBR16^TMGHL74R: Unable to determine ordering provider"
        SET TMGHL7MSG("RAD STUDY",TMGEXAMIDX,"PROV")=PROV        
OBR16DN QUIT
        ;
OBR32   ;"Transform Principle results interpretor (radiologist)
        ;"Input: Uses globally scoped vars: TMGVALUE,TMGHL7MSG,TMGU
        NEW TEMP SET TEMP=$$ENSURAD(TMGVALUE,TMGU(2))
        IF +TEMP=-1 SET TMGXERR=TEMP GOTO OBR32DN
        SET TMGEXAMIDX=+$GET(TMGEXAMIDX)  ;"will be killed in MSG2 at end of process
        SET TMGHL7MSG("RAD STUDY",TMGEXAMIDX,"RADPROV")=TEMP        
        SET TMGVALUE=TEMP
OBR32DN QUIT        
        ;
OBX5    ;"Purpose: To transform the OBX segment, field 5 -- Observation value
        IF $GET(TMGHL7MSG("STAGE"))'="FINAL" QUIT
        SET TMGEXAMIDX=+$GET(TMGEXAMIDX)  ;"will be killed in MSG2 at end of process
        NEW TEMP MERGE TEMP=TMGHL7MSG(TMGSEGN,5,1)
        NEW OUT DO PARSRPT(.OUT,.TEMP)
        MERGE TMGHL7MSG("RAD STUDY",TMGEXAMIDX)=OUT
        SET TMGHL7MSG("RAD STUDY",TMGEXAMIDX,"OBX")=TMGSEGN
        QUIT
        ;
PARSRPT(OUT,ARR)  ;"Split report into RPT (report), IMP (impression), HX (additional clinical history) sections
        ;"Note: for now, HX will not be split out.
        NEW SECTION SET SECTION="RPT"
        NEW OUTIDX SET OUTIDX=1
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:+IDX'>0  DO
        . NEW S SET S=$GET(ARR(IDX))
        . NEW PARTB SET PARTB=""
        . IF SECTION="RPT" DO
        . . NEW UP SET UP=$$UP^XLFSTR(S)
        . . NEW DIV SET DIV=""
        . . IF UP["IMPRESSION:" SET DIV="IMPRESSION:"
        . . ELSE  IF S["ASSESSMENT:" SET DIV="ASSESSMENT:"
        . . ELSE  IF S["IMPRESSION" SET DIV="IMPRESSION"  ;<-- Caution: MIGHT BE USED IN NORMAL DESCRIPTION NARRATIVE
        . . IF DIV="" QUIT
        . . DO CLEAVSTR^TMGSTUT2(.S,DIV,.PARTB,1)
        . . IF S'="" SET OUT(SECTION,OUTIDX)=S
        . . SET SECTION="IMP",S=PARTB,OUTIDX=1
        . IF OUTIDX=1,S="" QUIT
        . SET OUT(SECTION,OUTIDX)=S,OUTIDX=OUTIDX+1        
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
ENSURAD(HL7NAME,TMGU) ;"Ensure radiology name is registered with VistA
        ;"Input: Name, as provided by HL7 Message.  NOTE: Since the parser is
        ;"       called more than once, the input may also be in the format
        ;"       as per RESULT below.  
        ;"       TMGU -- The delimitor used to separate name parts (not the full array)
        ;"If user doesn't already exist, then added via call to ADDRAD
        ;"Result: VistA format name: 'DFN^LNAME,FNAME^TMG, OR -1^Message 
        NEW X,Y,NAME SET NAME=""
        NEW TMGRESULT SET TMGRESULT="-1"
        SET HL7NAME=$GET(HL7NAME)
        IF HL7NAME="" DO  GOTO ENRADDN
        . SET TMGRESULT="-1^No radiologist name provided at ENSURAD^TMGHL74R"
        IF (+HL7NAME>0)&($PIECE(HL7NAME,TMGU,3)="TMG") DO  GOTO ENRAD2 
        . SET TMGRESULT=HL7NAME 
        ;"Convert HL7Name into LNAME,FNAME
        SET NAME=$P(HL7NAME,TMGU,2)_","_$P(HL7NAME,TMGU,3)
        NEW DIC SET DIC(0)="M",DIC=200,X=NAME DO ^DIC
        IF Y>0 DO  GOTO ENRAD2
        . SET TMGRESULT=+Y_"^"_$PIECE(Y,"^",2)_"^"_"TMG" 
        SET TMGRESULT=$$ADDRAD(NAME,TMGU)
        IF TMGRESULT<0 GOTO ENRADDN
ENRAD2  NEW TEMP SET TEMP=$$NSURDPER(TMGRESULT)
        IF TEMP<0 SET TMGRESULT=TEMP
ENRADDN QUIT TMGRESULT
        ;
ADDRAD(FMNAME,TMGU)  ;"ADD RADIOLOGIST TO NEW PATIENT FILE.
        ;"Input: Name of radiologist to add: 'LNAME,FNAME' 
        ;"Return 'DFN^<NAME>^TMG', or -1^Message
        NEW TMGFDA,TMGIEN,TMGIENS,TMGMSG,DIC,TMGRESULT
        SET TMGRESULT=0
        SET DIC(0)=""
        SET TMGFDA(200,"+1,",.01)=FMNAME
        DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO ADDDN
        . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(TMGMSG("DIERR"))
        SET TMGIEN=+$GET(TMGIEN(1))
        IF +TMGIEN<1 DO  GOTO ADDDN
        . SET TMGRESULT="-1^USER NOT CREATED FOR UNKNOWN REASON"
        SET TMGRESULT=TMGIEN_"^"_FMNAME_"^TMG"
ADDDN   QUIT TMGRESULT
        ;                
NSURDPER(RAPROV)  ;"Ensure radiologist record had proper permissions etc
        ;"INPUT: RAPROV -- DFN^LNAME,FNAME^TMG
        ;"Results: 1^OK, or -1^Message
        NEW TMGFDA,TMGIEN,TMGIENS,TMGMSG,DIC,TMGRESULT,FOUND,SUBIEN
        SET TMGIEN=+RAPROV
        NEW FMNAME SET FMNAME=$PIECE(RAPROV,"^",2)
        IF TMGIEN'>0 DO  GOTO NSURDN 
        . SET TMGRESULT="-1^IN NSURDPER^TMGHL74R: TMGDFN invalid.  Got ["_TMGDFN_"]"
        SET TMGRESULT="1^OK"
        NEW ZN SET ZN=$GET(^VA(200,TMGIEN,0))
        SET TMGIENS=TMGIEN_","
        IF $PIECE(ZN,"^",4)'="#" SET TMGFDA(200,TMGIENS,3)="#"    ;"FILE MANAGER ACCESS CODE
        IF $PIECE(ZN,"^",7)'="1" SET TMGFDA(200,TMGIENS,7)="YES"  ;"DISUSER  <-- INACTIVATES USER SO CAN'T LOG IN WITH ETC.
        IF $PIECE(ZN,"^",9)'="2" SET TMGFDA(200,TMGIENS,8)="`2"   ;"TITLE.  HARD CODED #2 IN FILE 3.1 = [Physician]
        NEW BLOCKNAME SET BLOCKNAME=$PIECE(FMNAME,",",2)_" "_$PIECE(FMNAME,",",1)
        IF $PIECE($GET(^VA(200,TMGIEN,20)),"^",2)'=BLOCKNAME SET TMGFDA(200,TMGIENS,20.2)=BLOCKNAME
        IF $PIECE($GET(^VA(200,TMGIEN,5)),"^",1)'=2 SET TMGFDA(200,TMGIENS,29)="`2"  ;"SERVICE/SECTION.  HARD CODED #2 IN FILE 49 = [MEDICINE]
        IF $PIECE($GET(^VA(200,TMGIEN,"PS")),"^",5)'=1 SET TMGFDA(200,TMGIENS,53.5)="`1"  ;"PROVIDER CLASS.  HARD CODED #1 IN FILE 7 = [PHYSICIAN]
        IF $DATA(TMGFDA) DO FILE^DIE("E","TMGFDA","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO NSURDN
        . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(TMGMSG("DIERR"))
        ;"--FIELD# 16 (DIVISON) is subfile 200.02
        KILL TMGFDA
        NEW DIV SET DIV=69  ;"DIVISION.  HARD CODED #69 IN FILE 4 = [Family Phys of Greeneville  TN]
        SET SUBIEN=$ORDER(^VA(200,TMGIEN,2,"B",DIV,0))
        IF SUBIEN>0 GOTO NSUR2
        SET TMGFDA(200.02,"+1,"_TMGIENS,.01)="`"_DIV 
        SET TMGFDA(200.02,"+1,"_TMGIENS,1)="Yes"
        DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO NSURDN
        . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(TMGMSG("DIERR"))
NSUR2   ;"-- FIELD# 51 (KEYS) is subfile 200.051
        NEW KEY FOR KEY="RA ALLOC","RA VERIFY" DO  QUIT:TMGRESULT<0
        . NEW KEYIEN SET KEYIEN=$ORDER(^DIC(19.1,"B",KEY,0))
        . IF KEYIEN'>0 DO  QUIT
        . . SET TMGRESULT="-1^In NSURDPER^TMGHL74R: Unable to find KEY ["_KEY_"] in file 19.1"
        . NEW SUBIEN SET SUBIEN=$ORDER(^VA(200,TMGIEN,51,"B",KEYIEN,0))
        . IF SUBIEN>0 QUIT  ;"ALREADY PRESENT
        . KILL TMGFDA
        . SET TMGFDA(200.051,"+1,"_TMGIENS,.01)="`"_KEYIEN  ;"KEY
        . DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) DO  QUIT
        . . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(TMGMSG("DIERR"))
        IF TMGRESULT<0 GOTO NSURDN
NSUR3   ;"--FIELD #72 (RAD/NUC MED CLASSIFICATION) is subfile 200.072
        SET (FOUND,SUBIEN)=0
        FOR  SET SUBIEN=$ORDER(^VA(200,TMGIEN,"RAC",SUBIEN)) QUIT:(SUBIEN'>0)!FOUND  DO
        . SET FOUND=($PIECE($GET(^VA(200,TMGIEN,"RAC",SUBIEN,0)),"^",1)="S")
        IF FOUND GOTO NSUR4
        KILL TMGFDA
        SET TMGFDA(200.072,"+1,"_TMGIENS,.01)="staff"  ;"
        DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO NSURDN
        . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(TMGMSG("DIERR"))
NSUR4   ;"--FIELD #8932.1 (PERSON CLASS) if subfile 200.05
        NEW PCLASS SET PCLASS=766  ;"PERSON CLASS.  HARD CODED #766 IN 8932.1 = [Allopathic and Osteopathic Physicians 207Q00000X] 
        SET (FOUND,SUBIEN)=0
        FOR  SET SUBIEN=$ORDER(^VA(200,TMGIEN,"USC1",SUBIEN)) QUIT:(SUBIEN'>0)!FOUND  DO
        . SET FOUND=($PIECE($GET(^VA(200,TMGIEN,"USC1",SUBIEN,0)),"^",1)=PCLASS)
        IF FOUND GOTO NSUR5
        KILL TMGFDA   ;"set up PERSON CLASS subfile
        SET TMGFDA(200.05,"+1,"_TMGIENS,.01)="`"_PCLASS  
        SET TMGFDA(200.05,"+1,"_TMGIENS,2)="T-90"     ;"EFFECTIVE DATE
        DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO NSURDN
        . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(TMGMSG("DIERR"))
NSUR5   ;"--more adds here if needed        
NSURDN  QUIT TMGRESULT
        ;"
FILERAD(TMGENV,TMGHL7MSG)  ;
        ;"Input: TMGENV -- PASS BY REFERENCE
        ;"       TMGHL7MSG -- PASS BY REFERENCE
        ;"Results: 1 if OK, or -1^Message if error
        ;"Here I will file the radiology report.  
        ;"Use code in TMGRAU01 for filing...
        NEW DATA
        ;"SET DATA("DIV")="LAUGHLIN"   ;"HARD CODED because this entire file is just for Laughlin Rad interface
        SET DATA("DIV")=$GET(TMGENV("INST"),"GREENEVILLE COMMUNITY HOSP E")           
        SET DATA("LOC")="LAUGHLIN"  ;"HARD CODED because this entire file is just for Laughlin Rad interface
        NEW TMGRESULT SET TMGRESULT="1^OK"
        ;"Cycle through all studies found in report
        NEW EXAMIDX SET EXAMIDX=0
        FOR  SET EXAMIDX=$ORDER(TMGHL7MSG("RAD STUDY",EXAMIDX)) QUIT:(EXAMIDX'>0)!(+TMGRESULT'>0)  DO
        . NEW DT SET DT=$GET(TMGHL7MSG("RAD STUDY",EXAMIDX,"DT"))
        . SET DT=$$HL72FMDT^TMGHL7U3(DT)
        . IF +DT'>0 DO  QUIT
        . . SET TMGRESULT="-1^DATE-TIME for study not found in FILERAD^TMGHL74R.  Got ["_DT_"]"
        . SET DATA("DT")=DT
        . SET DATA(EXAMIDX,"DT REPORTED")=DT
        . NEW PROCIEN SET PROCIEN=$GET(TMGHL7MSG("RAD STUDY",EXAMIDX,"PROC"))        
        . ;"IF PROCIEN'>0 DO
        . ;". NEW CPT SET CPT=$GET(TMGHL7MSG("RAD STUDY",EXAMIDX,"CPT"))
        . ;". SET PROCIEN=$$GETPROC^TMGRAU01(CPT)  
        . IF PROCIEN>0 DO
        . . SET DATA(1,"PROC")=PROCIEN
        . . NEW ZN SET ZN=$GET(^RAMIS(71,PROCIEN,0))
        . . NEW TYPEIEN SET TYPEIEN=$PIECE(ZN,"^",12)
        . . IF TYPEIEN>0 SET DATA("TYPE")=TYPEIEN
        . NEW RADPROV SET RADPROV=+$GET(TMGHL7MSG("RAD STUDY",EXAMIDX,"RADPROV"))  ;" DFN^LNAME,FNAME^TMG 
        . SET DATA(EXAMIDX,"RADPROV")=RADPROV
        . NEW PROV SET PROV=+$GET(TMGHL7MSG("RAD STUDY",EXAMIDX,"PROV"))  ;" IEN^LNAME,FNAME
        . SET DATA(EXAMIDX,"PROV")=PROV        
        . MERGE DATA(EXAMIDX,"RPT")=TMGHL7MSG("RAD STUDY",EXAMIDX,"RPT")   
        . MERGE DATA(EXAMIDX,"HX")=TMGHL7MSG("RAD STUDY",EXAMIDX,"HX")   
        . MERGE DATA(EXAMIDX,"IMP")=TMGHL7MSG("RAD STUDY",EXAMIDX,"IMP")
        . SET DATA(EXAMIDX,"STATUS")="2^COMPLETE"  ;"I don't think they send incomplete rad reports.   
        NEW DFN SET DFN=+$GET(TMGHL7MSG("RAD STUDY","DFN"))
        IF DFN'>0 DO  GOTO FLRDDN
        . ;"SET TMGRESULT="-1^DFN not found in FILERAD^TMGHL74R.  Got ["_DFN_"]"
        . NEW NAMEDOB SET NAMEDOB=$$GETNMDOB^TMGHL7U3(.TMGHL7MSG)
        . SET TMGRESULT="-1^Patient not found in system: "_NAMEDOB         
        SET TMGRESULT=$$REGEXAM^TMGRAU01(DFN,.DATA)
        IF TMGRESULT<0 GOTO FLRDDN
        SET TMGRESULT=$$STOREXAM^TMGRAU01(.DATA)
        IF TMGRESULT<0 GOTO FLRDDN
        SET TMGRESULT=$$SNDALRTS^TMGRAU01(.DATA)
FLRDDN  QUIT TMGRESULT
        ;