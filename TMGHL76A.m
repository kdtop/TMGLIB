TMGHL76A ;TMG/kst-HL7 transformation engine processing ;7/30/19, 2/27/20, 3/24/21
              ;;1.0;TMG-LIB;**1**;11/14/16
 ;
 ;"TMG HL7 TRANSFORMATION FUNCTIONS 
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 4/11/19  Knothevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"NOTE: this is code for working with ADT messages from BALLAD
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
        ;"      process will recognize the ADT application, and load the record 
        ;"      in file #22720 that, in turn, calls the functions in this file.  
        ;"NEW OPTION SET OPTION("NO ALERT")=1
        DO HLDIRIN^TMGHL71("/mnt/WinServer/BalladADTHL7",1000,100)
        QUIT
        ;
ISADT(TMGHL7MSG)  ;"Test if message is an HL7 ADT message
        ;"Input: TMGHL7MSG -- PASS BY REFERENCE.  
        ;"Result: 1 if is ADT HL7, 0 otherwise
        ;"note: this is called by SETMAPS^TMGHL70B
        NEW TMGRESULT SET TMGRESULT=($GET(TMGHL7MSG(1,9,1))="ADT")
        QUIT TMGRESULT
        ;
        ;"===============================================================
        ;"|  Below are the call-back functions to handle transformation |
        ;"|  hooks, called by the XFMSG^TMGHL7X engine                  |
        ;"===============================================================
        ;
MSG     ;"Purpose: Process entire message before processing segments
        DO XMSG^TMGHL72 
        KILL TMGLASTOBR4,TMGLASTOBR4XF,TMGLASTOBX3,TMGOBXCOUNT
        QUIT
        ;
MSG2    ;"Purpose: Process entire message after processing segments
        DO XMSG2^TMGHL72 
        KILL TMGEXAMIDX
        KILL TMGADTTYPES
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
        ;"NEW TMGDFN SET TMGDFN=$PIECE(TMGVALUE,TMGU(1),4)
        ;"SET TMGHL7MSG("RAD STUDY","DFN")=+TMGDFN  ;"//kt added + 4/26/21
        QUIT
        ;
ORC1   ;"Purpose: Process empty ORC message, field 1
        DO XORC1^TMGHL72
        QUIT
        ;
ORC12  ;"Purpose: Process empty ORC message, field 12
        ;"don't process this 2/15/21  DO XORC12^TMGHL72
        QUIT
        ;
ORC13  ;"Purpose: Process empty ORC message, field 13
        DO XORC13^TMGHL72
        SET $PIECE(TMGVALUE,"^",1)="Family Phys Of Greeneville"
        SET $PIECE(TMGVALUE,"^",2)="69" 
        QUIT
        ;     
FILEADT(TMGENV,TMGHL7MSG)  ;"File the ADT report.
        ;"Input: TMGENV -- PASS BY REFERENCE
        ;"       TMGHL7MSG -- PASS BY REFERENCE
        ;"Results: 1 if OK, or -1^Message if error
        ;"         NOTE: if undesired ADT type, this will NOT file the message, but will still return 1^SUCCESS
        NEW MESSAGE,ADTEDATE,PCPDUZ,NOTETITLE,ARR
        NEW TMGRESULT SET TMGRESULT="1^SUCCESS"
        IF $DATA(TMGADTTYPES)=0 DO LOADTYPS(.TMGADTTYPES) ;"//NOTE: TMGADTTYPES is killed is MSG2^TMGHL76A
        NEW EVENTIDX SET EVENTIDX=$ORDER(TMGHL7MSG("B","EVN",0))
        IF EVENTIDX'>0 DO  GOTO FADTDN
        . SET TMGRESULT="-1^Unable to find EVN segment in HL7 message"
        ;"
     ;" EVN Fields
        ;"  EVN.1 - Event Type Code
        ;"  EVN.2 - Recorded Date/Time
        ;"  EVN.4 - Event Reason Code
        ;"
        NEW ADTEVENT SET ADTEVENT=$G(TMGHL7MSG(EVENTIDX,1))                     ;"Event Type Code
        NEW ADTEVENTNAME SET ADTEVENTNAME=$G(TMGADTTYPES(ADTEVENT))             
        NEW ADTDATE SET ADTDATE=$$HL72FMDT^TMGHL7U3($G(TMGHL7MSG(EVENTIDX,2)))  ;"Recorded Date/Time
        NEW EVENTTYPE SET EVENTTYPE=$$UP^XLFSTR($G(TMGHL7MSG(EVENTIDX,4)))      ;"Event Reason Code
        NEW VISITTYPE SET VISITTYPE=""
        SET ADTEDATE=$$EXTDATE^TMGDATE(ADTDATE)
        ;"
        NEW PIDIDX SET PIDIDX=$ORDER(TMGHL7MSG("B","PID",0))
        IF PIDIDX'>0 DO  GOTO FADTDN
        . SET TMGRESULT="-1^Unable to find PID segment in HL7 message"
        NEW TMGDFN SET TMGDFN=+$GET(TMGHL7MSG(PIDIDX,4))  ;"//kt added + 4/26/21
        NEW NOTETEXT,NOTETITLE,AUTHORNAME
        DO GETPROV^TMGPROV1(.PCPDUZ,TMGDFN,DUZ)
        IF PCPDUZ'>0 DO  GOTO FADTDN
        . SET TMGRESULT="-1^PCP COULD NOT BE DETERMINED FOR "_$P($G(^DPT(TMGDFN,0)),"^",1)_"  FROM HL7 ADT MESSAGE"
        NEW EDDIEMSG SET EDDIEMSG=""
        ;"NEW ALRTDUZ SET ALRTDUZ=150  ;"//hard coded to Eddie Hagood user
        NEW ALRTDUZ SET ALRTDUZ=1053  ;"//Setting to Lindsey
        NEW ERDCNOTE SET ERDCNOTE=0
        ;"
     ;" USED HL7 SEGMENTS
        ;"   PID - Patient Information
        ;"   PD1 - Patient Additional Demographic
        ;"   PV1 - Patient Visit
        ;"   PV2 - Patient Visit - Additional Information
        ;"
        NEW PATNAME SET PATNAME=$P($G(^DPT(TMGDFN,0)),"^",1)
        NEW PV1IDX SET PV1IDX=+$ORDER(TMGHL7MSG("B","PV1",0))
        NEW PV2IDX SET PV2IDX=+$ORDER(TMGHL7MSG("B","PV2",0))
        NEW PD1IDX SET PD1IDX=+$ORDER(TMGHL7MSG("B","PD1",0))
        ;"
     ;" USED HL7 FIELDS
        ;"   PD1.3 - Patient Primary Facility
        ;"   PV1.2 - Patient Class
        ;"   PV1.3 - Assigned Patient Location
        ;"   PV1.4 - Admission Type
        ;"   PV1.7 - Attending Doctor (Subpiece 3,Subpiece 2 = LName,FName)
        ;"   PV1.45 - Discharge Date/Time
        ;"   PV2.12 - Visit Description 
        ;"
        NEW PRIMFACILITY SET PRIMFACILITY=$GET(TMGHL7MSG(PD1IDX,3,1),"")        ;"Patient Primary Facility
        NEW ASSIGNEDFACILITY SET ASSIGNEDFACILITY=$GET(TMGHL7MSG(+PV1IDX,3,1))  ;"Assigned Patient Location
        NEW PREVLOC SET PREVLOC=$GET(TMGHL7MSG(+PV1IDX,4))                      ;"Admission Type (Points to Prev Location)
        NEW PV2VISITTYPE SET PV2VISITTYPE=$GET(TMGHL7MSG(+PV2IDX,12))           ;"Visit Description            
        NEW PATCLASS SET PATCLASS=$GET(TMGHL7MSG(+PV1IDX,2))                    ;"Patient Class
        NEW DCDATE SET DCDATE=$GET(TMGHL7MSG(+PV1IDX,45))                        ;"Discharge Date/Time (Only used if A08 outpatient visit)
        NEW PHYSICIAN SET PHYSICIAN=$GET(TMGHL7MSG(+PV1IDX,7,2))_","_$GET(TMGHL7MSG(+PV1IDX,7,3)) ;"Physician Name
        ;"
     ;" Message Exclusions Below
        ;"
        IF "INPATIENT^OBSERVATION^EMERGENCY^HOSPITAL OUT^OUTPATIENT"'[PATCLASS GOTO FADTDN  ;"Ignore all other patient classes
        IF PV2VISITTYPE["PRE-ADMISSION TESTING" GOTO FADTDN                                 ;"Ignore Pre-Admissions 3/31/20
        IF PV2VISITTYPE["RAD FILM PERMANENT TRANSFER" GOTO FADTDN                           ;"Ignore Film Transfers 10/12/21
        IF (ADTEVENT="A03")&(ASSIGNEDFACILITY="GCHE WOMENS") GOTO FADTDN                    ;"Ignore Women's Center D/Cs 11/1/24
        IF (ADTEVENT="A03")&(ASSIGNEDFACILITY="GCHE MRI") GOTO FADTDN                       ;"Ignore MRI D/Cs 11/1/24
        IF (ADTEVENT="A03")&(PATCLASS="OUTPATIENT") GOTO FADTDN                             ;"Ignore Outpatient D/Cs 11/1/24
        ;"
     ;" TIU NOTE TITLES
        NEW ERNOTETITLE SET ERNOTETITLE="HOSPITAL EMERGENCY ROOM (HL7)"
        NEW ADMITNOTETITLE SET ADMITNOTETITLE="HOSPITAL ADMISSION (HL7)"
        NEW DCNOTETITLE SET DCNOTETITLE="HOSPITAL DISCHARGE (HL7)"
        NEW TRANSNOTETITLE SET TRANSNOTETITLE="HOSPITAL ER TO INPATIENT TRANSFER (HL7)"
        NEW PROCNOTETITLE SET PROCNOTETITLE="HOSPITAL OUTPATIENT PROCEDURE (HL7)"
        NEW EXTOVNOTETITLE SET EXTOVNOTETITLE="EXTERNAL OFFICE VISIT (HL7)"
        ;"
     ;"  **ADMISSION VISIT**
        IF ADTEVENT="A01" DO      
        . IF PATCLASS="EMERGENCY" DO
        . . SET VISITTYPE="EMERGENCY DEPARTMENT"
        . . SET NOTETITLE=ERNOTETITLE
        . . SET EDDIEMSG=PATNAME_" ENTERED THE ER ON "_ADTEDATE
        . ELSE  DO
        . . SET VISITTYPE="INPATIENT"
        . . SET NOTETITLE=ADMITNOTETITLE
        . . SET EDDIEMSG=PATNAME_" WAS ADMITTED ON "_ADTEDATE
        ;"
     ;"  **PATIENT TRANSFER**
        IF ADTEVENT="A02" DO  ;"TRANSFER   ADDED ON 6/11/24
        . IF PATCLASS="INPATIENT" DO
        . . IF PREVLOC="ER" DO  ;"THIS SHOULD POINT TO THE FACT THAT THE PATIENT IS NOW INPATIENT BUT WAS PREVIOUSLY IN THE ER
        . . . SET VISITTYPE="TRANSFER, ED TO INPATIENT"
        . . . SET NOTETITLE=TRANSNOTETITLE
        . . . SET EDDIEMSG=PATNAME_" ADMITTED FROM ED ON "_ADTEDATE
        ;"
     ;"  **DISCHARGE**
        IF ADTEVENT="A03" DO  ;"DISCHARGE
        . IF PATCLASS="EMERGENCY" DO
        . . SET VISITTYPE="EMERGENCY DEPARTMENT"
        . . SET NOTETITLE=ERNOTETITLE
        . . SET EDDIEMSG=PATNAME_" WAS D/C'd FROM THE ER ON "_ADTEDATE
        . . SET ERDCNOTE=1
        . ELSE  IF PATCLASS="HOSPITAL OUT" DO
        . . SET VISITTYPE="OUTPATIENT PROCEDURE"
        . . SET NOTETITLE=PROCNOTETITLE     
        . . SET EDDIEMSG=PATNAME_" HAD AN OUTPATIENT PROCEDURE ON "_ADTEDATE
        . ELSE  DO
        . . SET VISITTYPE="UNKNOWN/"_ASSIGNEDFACILITY
        . . SET NOTETITLE=DCNOTETITLE
        . . SET EDDIEMSG=PATNAME_" WAS DISCHARGED ON "_ADTEDATE
        . . SET ERDCNOTE=1
        ;
     ;"  **EXTERNAL OUTPATIENT VISIT**
        IF ADTEVENT="A08" DO ;"CHECKOUT FOR OFFICE VISIT  --  ADDED ON 10/24/24
        . IF (PV2VISITTYPE["OFFICE VISIT")&(EVENTTYPE["ENCOUNTER CLOSE") DO  ;"ONLY HANDLE IF AN "ENCOUNTER CLOSE"
        . . IF PV2VISITTYPE'="" DO
        . . . SET VISITTYPE=PV2VISITTYPE
        . . ELSE  DO
        . . . SET VISITTYPE="OFFICE VISIT"
        . . SET NOTETITLE=EXTOVNOTETITLE
        . . SET PRIMFACILITY=ASSIGNEDFACILITY
        . . SET ADTEVENTNAME="END OF VISIT"
        . . SET EDDIEMSG=PATNAME_" WAS SEEN AT "_PRIMFACILITY_" ON "_ADTEDATE
        . . IF DCDATE'="" SET DCDATE=$$HL72FMDT^TMGHL7U3(DCDATE)
        . . SET DCDATE=$$EXTDATE^TMGDATE(DCDATE)
        . . SET ADTEDATE=ADTEDATE_" ("_DCDATE_")"
        ;"
     ;"  Populate the data for the TIU Note
        NEW IDX SET IDX=1;
        IF ASSIGNEDFACILITY'="" SET ASSIGNEDFACILITY=$$TRANSFACNAME(ASSIGNEDFACILITY)
        SET ARR(IDX)="PATIENT NAME^"_PATNAME       SET IDX=IDX+1
        SET ARR(IDX)="FACILITY^"_ASSIGNEDFACILITY  SET IDX=IDX+1
        SET ARR(IDX)="DATE^"_ADTEDATE              SET IDX=IDX+1
        SET ARR(IDX)="VISIT TYPE^"_VISITTYPE       SET IDX=IDX+1
        SET ARR(IDX)="EVENT TYPE^"_ADTEVENTNAME    SET IDX=IDX+1
        SET ARR(IDX)="PHYSICIAN^"_PHYSICIAN        SET IDX=IDX+1
        ;
     ;"  Get the Diagnosis codes and turn 
        NEW DLGIDX SET DLGIDX=0
        NEW HASCOVIDDX SET HASCOVIDDX=0
        FOR  SET DLGIDX=$ORDER(TMGHL7MSG("B","DG1",DLGIDX)) QUIT:DLGIDX'>0  DO  
        . NEW DIAGSTR SET DIAGSTR=$$DLGINFO(.TMGHL7MSG,DLGIDX)
        . SET ARR(IDX)="VISIT DX^"_DIAGSTR,IDX=IDX+1
        . ;" LEAVE THIS TURNED OFF FOR NOW  10/13/22  IF DIAGSTR["COVID" SET HASCOVIDDX=1
        ;
     ;"   Create the Alert if a message has been assigned to it
        IF EDDIEMSG'="" DO INFRMALT^TMGXQAL(.ALERTRESULT,ALRTDUZ,EDDIEMSG)
        ;"
     ;"   If we have a PCP and a VisitType, Create the TIU Note
        ;"IF PCPDUZ>0,$DATA(VISITTYPE) DO
        IF PCPDUZ>0,VISITTYPE'="" DO
        . SET TMGRESULT=$$MAKENOTE(TMGDFN,PCPDUZ,ADTDATE,NOTETITLE,.ARR,ERDCNOTE)
        . ;"2/4/21 IF COVID DX WAS FOUND AND THE EVENT IS A D/C MAKE A NOTE
        . IF (HASCOVIDDX=1)&(ADTEVENT="A03") DO COVDNOTE(TMGDFN,ADTDATE,PCPDUZ)
FADTDN  ;
        QUIT TMGRESULT   
        ;
MAKENOTE(TMGDFN,TMGDUZ,DOS,NOTETITLE,ARR,ERDCNOTE)
        NEW TIUIEN,NOTETEXT,TMGRESULT
        SET TMGRESULT="1^SUCCESS"
        NEW REFNOTETEXT SET REFNOTETEXT=$NAME(NOTETEXT("TEXT"))
        NEW ALLERGIES SET ALLERGIES=$$DETALRGY2^TMGTIUO3(TMGDFN)
        NEW USERNAME SET USERNAME=$PIECE($GET(^VA(200,TMGDUZ,0)),"^",1)
        ;"
        ;" --- CREATE TIU NOTE/ADDENDUM ---
        IF $$NOTEEXISTS(TMGDFN,TMGDUZ,DOS,NOTETITLE,.TIUIEN)=1 DO
        . NEW TIUX SET TIUX(1202)=TMGDUZ,TIUX(1301)=DOS
        . DO MAKEADD^TIUSRVP(.TIUIEN,TIUIEN,.TIUX,1)
        ELSE  DO
        . IF NOTETITLE["DISCHARGE" SET DOS=DOS_".999999"
        . ELSE  SET DOS=DOS_".000001"
        . DO BLANKTIU^TMGRPC1(.TIUIEN,TMGDFN,USERNAME,6,DOS,NOTETITLE)
        IF TIUIEN<1 DO  GOTO MKNDN
        . SET TMGRESULT="-1^Unable to create TIU note.  Msg:"_$P(TIUIEN,"^",2)
        ;
        ;" --- COMPOSE THE NOTE ---
        NEW IDX SET IDX=1
        SET NOTETEXT("TEXT",IDX,0)="<!DOCTYPE HTML PUBLIC ""-//WC3//DTD HTML 3.2//EN"">  <HTML><body>",IDX=IDX+1    ;"HTML HEADER
        SET NOTETEXT("TEXT",IDX,0)=ALLERGIES_"<p>",IDX=IDX+1
        DO ARR2TABL(.ARR,REFNOTETEXT,.IDX)  ;"Add table with ADT information.  
        SET NOTETEXT("TEXT",IDX,0)="<p><p>Office staff has been notified to get records for this encounter.<p>",IDX=IDX+1
        ;"DO HL72PRE(.TMGHL7MSG,REFNOTETEXT,.IDX) ;"Show raw HL7 data in <pre> block. 
        SET NOTETEXT("TEXT",IDX,0)="</body></HTML>"
        SET NOTETEXT("HDR")="1^1"
        ;
        NEW RESULT DO SETTEXT^TMGTIUS1(.RESULT,TIUIEN,.NOTETEXT,0)
        DO SEND^TIUALRT(TIUIEN)
        IF ERDCNOTE=1 DO
        . NEW USERARR,TEMPRESULT
        . SET USERARR(1)="259^Shipley,Sabrina^- CMA"
        . DO IDSIGNRS^TIULX(.TEMPRESULT,TIUIEN,.USERARR)
        . ;"IGNORE ERRORS IN TEMPRESULT FOR TIME BEING
MKNDN   ;
        QUIT TMGRESULT
        ;"
COVDNOTE(TMGDFN,DOS,TMGDUZ)
        NEW TIUIEN,NOTETEXT,TMGRESULT,NOTETITLE
        SET NOTETITLE="NOTE"
        NEW USERNAME SET USERNAME=$PIECE($GET(^VA(200,TMGDUZ,0)),"^",1)
        ;"
        ;" --- CREATE TIU NOTE/ADDENDUM ---
        SET DOS=DOS_".999999"
        DO BLANKTIU^TMGRPC1(.TIUIEN,TMGDFN,USERNAME,6,DOS,NOTETITLE)
        IF TIUIEN<1 DO  GOTO CNDN
        . NEW ALRTRESULT
        . DO INFRMALT^TMGXQAL(.ALRTRESULT,150,"ERROR CREATING BLANK NOTE FOR: "_TMGDFN)
        ;
        ;" --- COMPOSE THE NOTE ---
        NEW IDX SET IDX=1
        SET NOTETEXT("TEXT",IDX,0)="<!DOCTYPE HTML PUBLIC ""-//WC3//DTD HTML 3.2//EN"">  <HTML><body>",IDX=IDX+1    ;"HTML HEADER
        SET NOTETEXT("TEXT",IDX,0)="REVIEW HOSPITAL NOTES AND CONSIDER FOLLOW UP FOR COVID",IDX=IDX+1         
        SET NOTETEXT("TEXT",IDX,0)="</body></html>"
        SET NOTETEXT("HDR")="1^1"
        ;
        NEW RESULT DO SETTEXT^TMGTIUS1(.RESULT,TIUIEN,.NOTETEXT,0)
        DO SEND^TIUALRT(TIUIEN)
CNDN   ;
        QUIT
        ;"        
NOTEEXISTS(TMGDFN,DUZ,DOS,NOTETITLE,RTNTIUIEN)  ;"
        ;"Checks to see if a note exists on this DOS for patient
        NEW TMGRESULT SET TMGRESULT=0
        NEW DATE SET DATE=$P(DOS,".",1)
        NEW NOTETYPE SET NOTETYPE=+$O(^TIU(8925.1,"B",NOTETITLE,0))
        IF NOTETYPE'>0 GOTO NEDN
        FOR  SET DATE=$O(^TIU(8925,"ZTMGPTDT",TMGDFN,DATE)) QUIT:(DATE'>0)!(DATE'[$P(DOS,".",1))  DO
        . NEW TIUIEN SET TIUIEN=0
        . FOR  SET TIUIEN=$O(^TIU(8925,"ZTMGPTDT",TMGDFN,DATE,TIUIEN)) QUIT:TIUIEN'>0  DO
        . . NEW THISTYPE,THISAUTHOR
        . . SET THISTYPE=$P($G(^TIU(8925,TIUIEN,0)),"^",1)
        . . SET THISAUTHOR=$P($G(^TIU(8925,TIUIEN,12)),"^",2)
        . . IF (THISAUTHOR=DUZ)&(NOTETYPE=THISTYPE) DO
        . . . SET TMGRESULT=1
        . . . SET RTNTIUIEN=TIUIEN
NEDN        
        QUIT TMGRESULT
        ;"
DLGINFO(TMGHL7MSG,DLGIDX)  ;"Get diagnosis info
        ;"Result: string describing diagnosis. 
        NEW CODE SET CODE=$GET(TMGHL7MSG(DLGIDX,3,1))
        NEW NARRATIVE SET NARRATIVE=$GET(TMGHL7MSG(DLGIDX,4))
        NEW RESULT SET RESULT=CODE
        IF RESULT'="" SET RESULT=RESULT_": "
        SET RESULT=RESULT_NARRATIVE
        QUIT RESULT
        ;
ARR2TABL(ARR,REFHTML,JDX,STYLE) ;"Convert array to HTML table. 
        ;"Input: ARR -- PASS BY REFERENCE.  Expected format: ARR(INDEX#)=<text for column1>^<text for column2>
        ;"       REFHTML -- AN OUT PARAMETER.  PASS BY NAME.  Output format:  
        ;"              @REFHTML@(1,0)=<HTML TEXT LINE 1>
        ;"              @REFHTML@(2,0)=<HTML TEXT LINE 2> ... etc.
        ;"       JDX -- PASS BY REFERENCE.  Index output text array
        ;"       STYLE -- OPTIONAL.  Default is "BORDER=1"  This is added to <table> tag for formatting.  
        ;"Result: none
        SET STYLE=$GET(STYLE,"BORDER=1")
        NEW IDX SET IDX=""
        SET @REFHTML@(JDX,0)="<table "_STYLE_">",JDX=JDX+1
        FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:IDX'>0  DO
        . NEW LINE SET LINE=$GET(ARR(IDX)) QUIT:LINE=""
        . NEW COL1 SET COL1=$PIECE(LINE,"^",1)
        . NEW COL2 SET COL2=$PIECE(LINE,"^",2,99)
        . SET @REFHTML@(JDX,0)="<tr><td>"_COL1_"</td><td>"_COL2_"</td></tr>",JDX=JDX+1
        SET @REFHTML@(JDX,0)="</table>",JDX=JDX+1
        QUIT
        ;
HL72PRE(ARR,REFHTML,JDX,STYLE) ;"Convert array to PRE block. 
        ;"Input: ARR -- PASS BY REFERENCE.  Expected format: ARR(INDEX#)=<text for column1>^<text for column2>
        ;"       REFHTML -- AN OUT PARAMETER.  PASS BY NAME.  Output format:  
        ;"              @REFHTML@(1,0)=<HTML TEXT LINE 1>
        ;"              @REFHTML@(2,0)=<HTML TEXT LINE 2> ... etc.
        ;"       JDX -- PASS BY REFERENCE.  Index output text array
        ;"       STYLE -- OPTIONAL.  Default is style as below...  
        ;"Result: none
        SET STYLE=$GET(STYLE,"""BORDER-TOP: gray 1px solid;BORDER-BOTTOM: gray 1px solid;BORDER-RIGHT: gray 1px solid;BORDER-LEFT: gray 1px solid;"" width=""200%""")
        NEW IDX SET IDX=""
        SET @REFHTML@(JDX,0)="<p><p>========RAW HL7 DATA BELOW========<p>",JDX=JDX+1
        SET @REFHTML@(JDX,0)="<pre "_STYLE_">",JDX=JDX+1        
        FOR  SET IDX=$O(TMGHL7MSG(IDX)) QUIT:+IDX'>0  DO
        . NEW LINE SET LINE=$GET(TMGHL7MSG(IDX)) QUIT:LINE=""
        . SET @REFHTML@(JDX,0)=LINE,JDX=JDX+1        
        SET @REFHTML@(JDX,0)="</pre>",JDX=JDX+1
        QUIT
        ;
SETALERT(ERRTEXT,AMSG,IEN772,IEN773,AMODE,TMGENV) ;
        ;"NOTE: for now, I am not using AMODE.  Overwritting locally anyway, to ADT
        DO SETALERT^TMGHL7E(.ERRTEXT,.AMSG,.IEN772,.IEN773,"ADT",.TMGENV)
        QUIT
        ;"        
LOADTYPS(ARR) ;" Load names of ADT event names into ARR
TYPES   ;"//----- ADT TYPES ----      
        ;;"A01^Admit/visit notification
        ;;"A02^Transfer a patient
        ;;"A03^Discharge/end visit
        ;;"A04^Register a patient
        ;;"A05^Pre-admit a patient
        ;;"A06^Change an outpatient to an inpatient
        ;;"A07^Change an inpatient to an outpatient
        ;;"A08^Update patient information
        ;;"A09^Patient departing - tracking
        ;;"A10^Patient arriving - tracking
        ;;"A11^Cancel admit/visit notification
        ;;"A12^Cancel transfer
        ;;"A13^Cancel discharge/end visit
        ;;"A14^Pending admit
        ;;"A15^Pending transfer
        ;;"A16^Pending discharge
        ;;"A17^Swap patients
        ;;"A18^Merge patient information
        ;;"A19^QRY/ADR - Patient query
        ;;"A20^Bed status update
        ;;"A21^Patient goes on a �leave of absence�
        ;;"A22^Patient returns from a �leave of absence�
        ;;"A23^Delete a patient record
        ;;"A24^Link patient information
        ;;"A25^Cancel pending discharge
        ;;"A26^Cancel pending transfer
        ;;"A27^Cancel pending admit
        ;;"A28^Add person information
        ;;"A29^Delete person information
        ;;"A30^Merge person information
        ;;"A31^Update person information
        ;;"A32^Cancel patient arriving - tracking
        ;;"A33^Cancel patient departing - tracking
        ;;"A34^Merge patient information - patient I
        ;;"A35^Merge patient information - account only
        ;;"A36^Merge patient information - patient ID and account number
        ;;"A37^Unlink patient information
        ;;"A38^Cancel pre-admit
        ;;"A39^Merge person - patient ID                      
        ;;"A40^Merge patient - patient identifier list
        ;;"A41^Merge account - patient account num
        ;;"A42^Merge visit - visit number
        ;;"A43^Move patient information - patient identifier list
        ;;"A44^Move account information - patient account number
        ;;"A45^Move visit information - visit number
        ;;"A46^Change patient ID
        ;;"A47^Change patient identifier list
        ;;"A48^Change alternate patient ID
        ;;"A49^Change patient account number
        ;;"A50^Change visit number
        ;;"A51^Change alternate visit ID
        ;;"^
        NEW IDX SET IDX=0
        FOR  DO  QUIT:(IDX'>0)
        . SET IDX=IDX+1 
        . NEW LINE SET LINE=$$TRIM^XLFSTR($TEXT(TYPES+IDX^TMGHL76A))
        . SET LINE=$EXTRACT(LINE,4,$LENGTH(LINE))
        . NEW CODE SET CODE=$PIECE(LINE,"^",1)
        . NEW NAME SET NAME=$PIECE(LINE,"^",2)
        . IF CODE="" SET IDX=-1 QUIT
        . SET ARR(CODE)=NAME
        QUIT        
        ;
TRANSFACNAME(ASSIGNEDFACILITY)  ;"Check 22761 to see if name has different name
        IF '$D(^TMG(22761,"B",ASSIGNEDFACILITY)) DO  GOTO TFNDN
        . IF ASSIGNEDFACILITY["JCMC" SET ASSIGNEDFACILITY="Johnson City Med Center"
        NEW NAMEIDX SET NAMEIDX=+$O(^TMG(22761,"B",ASSIGNEDFACILITY,0))
        IF NAMEIDX'>0 GOTO TFNDN
        SET ASSIGNEDFACILITY=$P($G(^TMG(22761,NAMEIDX,0)),"^",2)
TFNDN        
        QUIT ASSIGNEDFACILITY
        ;"
TESTADD
        NEW TMGDFN,DOS,NOTETITLE,TIUIEN,TMGDUZ,USERNAME
        ;"
        SET TMGDFN=75492,DOS="3200424.1500",NOTETITLE="ACUTE MEDICAL ISSUE VISIT",USERNAME="HAGOOD,EDDIE L",TMGDUZ=150
        IF $$NOTEEXISTS(TMGDFN,TMGDUZ,DOS,NOTETITLE,.TIUIEN)=1 DO
        . NEW TIUX,RTNTIUIEN SET TIUX(1202)=TMGDUZ,TIUX(1301)=DOS
        . DO MAKEADD^TIUSRVP(.RTNTIUIEN,TIUIEN,.TIUX,1)
        . SET TIUIEN=RTNTIUIEN
        ELSE  DO
        . DO BLANKTIU^TMGRPC1(.TIUIEN,TMGDFN,USERNAME,6,DOS,NOTETITLE)
      
        QUIT
               
