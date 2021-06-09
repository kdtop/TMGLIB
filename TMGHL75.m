TMGHL75  ;TMG/kst-HL7 transformation engine processing ;6/14/17, 4/11/19
        ;;1.0;TMG-LIB;**1**;10/25/15;Build 61
 ;
 ;"TMG HL7 TRANSFORMATION FUNCTIONS
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 10/25/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"NOTE: this is code for working with labs from **[QUEST DIAGNOSTICS]**
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
TEST   ;"Pick file and manually send through filing process.
  DO TEST^TMGHL71("/mnt/WinServer/")
  QUIT
  ;
BATCH    ;"Launch processing through all files in folder.
  NEW DIR SET DIR="/mnt/WinServer"
  NEW DONEPATH SET DONEPATH="/mnt/WinServer"
  NEW OPTION SET OPTION("AUTO REGISTER MODE")=1  ;"Auto register new labs
  DO HLDIRIN^TMGHL71(DIR,1000,10,DONEPATH,".hl7",.OPTION)
  QUIT
  ;
  ;"+-----------------------------------------------------------------+
  ;"| =============================================================== |
  ;"| |  Below are the call-back functions to handle transformation | |
  ;"| |  hooks, called by the XFMSG^TMGHL7X engine                  | |
  ;"| |  These are stored in FILE 22720, TMG HL7 MESSAGE TRANSFORM -| | 
  ;"| |     SETTINGS                                                | |
  ;"| =============================================================== |
  ;"+-----------------------------------------------------------------+
  ;
MSG     ;"Purpose: Process entire message BEFORE processing segments
  ;"Input: Uses globally scoped vars: TMGHL7MSG, TMGU, HLREC
  ;
  ;"STARTING GLOBAL SCOPE OF VARS: TMGNTEADD, TMGDD -- to be killed in MSG2()
  DO KILLDNRS^TMGHL72(.TMGHL7MSG)
  DO XMSG^TMGHL72 
  QUIT
  ;
MSG2     ;"Purpose: Process entire message AFTER processing segments
  DO XMSG2^TMGHL72  
  IF $GET(TMGHL7MSG("STAGE"))="PRE" QUIT
  DO XMSG2B^TMGHL72
  QUIT
  ;
SHOWPART(ARR,INFO,NODE,PRINTNAME)   ;
  IF $$CHILDCT($NAME(INFO(NODE)))=1 DO
  . SET VALUE=$ORDER(INFO(NODE,""))
  . SET LINE=PRINTNAME_VALUE
  . DO ADDTOARR^TMGHL72(.ARR,LINE)
  ELSE  DO
  . NEW VALUE SET VALUE=""
  . FOR  SET VALUE=$ORDER(INFO(NODE,VALUE)) QUIT:VALUE=""  DO
  . . NEW SHOWN SET SHOWN=0
  . . NEW TESTNAME SET TESTNAME=""
  . . FOR  SET TESTNAME=$ORDER(INFO(NODE,VALUE,TESTNAME)) QUIT:TESTNAME=""  DO
  . . . SET LINE=PRINTNAME_VALUE_" for test: "_TESTNAME
  . . . DO ADDTOARR^TMGHL72(.ARR,LINE)
  . . . SET SHOWN=1
  . . IF SHOWN=0 DO
  . . . SET LINE=PRINTNAME_VALUE
  . . . DO ADDTOARR^TMGHL72(.ARR,LINE)
  QUIT
  ;        
CHILDCT(REF)   ;"Return number of child nodes for ref
  NEW CT SET CT=0
  NEW IDX SET IDX=""
  FOR  SET IDX=$ORDER(@REF@(IDX)) QUIT:IDX=""  DO
  . SET CT=CT+1
  QUIT CT
  ;
MSH3     ;"Purpose: Process MSH segment, FLD 4 (Sending Application)
  ;"SET TMGVALUE="LA7V HOST QUEST"
  QUIT
  ;
MSH4   ;"Purpose: Process MSH segment, FLD 4 (Sending Facility)
  IF $GET(TMGHL7MSG("STAGE"))="PRE" QUIT
  ;"SET TMGVALUE="Quest Diagnostics-Wood Dale"
  NEW IEN22720 SET IEN22720=+$ORDER(^TMG(22720,"D",TMGVALUE,0))
  IF IEN22720'>0 DO  QUIT
  . SET TMGXERR="IN MSH4^TMGHL75: Unable to find entry to match '"_TMGVALUE_"' in 'D' cross reference."
  NEW IEN4 SET IEN4=$PIECE($GET(^TMG(22720,IEN22720,0)),"^",2)
  IF IEN4'>0 DO  QUIT
  . SET TMGXERR="IN MSH4^TMGHL75: No institution (field #1)defined for record #"_IEN22720_" in file 22720"
  NEW ZN SET ZN=$GET(^DIC(4,IEN4,0))
  NEW INAME SET INAME=$PIECE(ZN,"^",1)
  SET TMGVALUE=INAME_TMGU(2)_IEN4
  ;"DO XMSH4^TMGHL72
  QUIT
  ;
MSH15   ;"Purpose: Process MSH segment, FLD 15 -- Accept Acknowledgment Type (ID)
  DO XMSH15^TMGHL72
  QUIT
  ;
MSH16   ;"Purpose: Process MSH segment, FLD 16 -- Application Acknowledgment Type (ID)
  DO XMSH16^TMGHL72 
  QUIT
  ;
PID      ;"Purpose: To transform the PID segment, esp SSN
  ;"Input: Uses globally scoped vars: TMGHL7MSG, TMGU, TMGNTEADD, TMGSEGN, TMGDD
  IF $GET(TMGHL7MSG("STAGE"))="PRE" QUIT
  DO PID^TMGHL72
  QUIT
  ;
PID5     ;"Purpose: to transform patient name, if needed
  QUIT
  ;
PV18     ;"Purpose: Process entire PV1-8 segment
  ;"NOTE: Pathgroup doesn't send order information in OBR16, so will use
  ;"      information from here to fix that.
  ;"In Pathrgoup messages, PV1 comes before any OBR segments.
  NEW LNAME SET LNAME=$PIECE(TMGVALUE,TMGU(2),2)
  NEW FNAME SET FNAME=$PIECE(TMGVALUE,TMGU(2),3)
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(TMGHL7MSG(IDX)) QUIT:(+IDX'>0)  DO
  . NEW SEGTYPE SET SEGTYPE=$GET(TMGHL7MSG(IDX,"SEG")) QUIT:SEGTYPE=""
  . IF SEGTYPE="OBR" DO
  . . DO SETPCE^TMGHL7X2(TMGVALUE,.TMGHL7MSG,.TMGU,IDX,16)
  DO SETPCE^TMGHL7X2(TMGVALUE,.TMGHL7MSG,.TMGU,"ORC",12)
  QUIT
  ;
ORC     ;"Purpose: Process entire ORC message
  ;"Uses Globally scoped vars: TMGSEGN (set up in from TMGHL7X* code before calling here.)
  ;"     also TMGDD
  IF $DATA(TMGHL7MSG(TMGSEGN,13))=0 DO
  . SET TMGHL7MSG(TMGSEGN,13)="?"
  IF $GET(TMGINFO("PROV"))="" DO SUPROV^TMGHL72("ORC",12)
  ;
  QUIT
  ;
ORC1    ;"Purpose: Process ORC message, field 1
  DO XORC1^TMGHL72
  QUIT
  ;
ORC12   ;"Purpose: Process ORC message, field 12
  DO XORC12^TMGHL72
  QUIT
  ;
ORC13   ;"Purpose: Process ORC message, field 13
  DO XORC13^TMGHL72
  SET $PIECE(TMGVALUE,"^",1)="MSP-MULTI-SPECIALTY PHYSICIANS"
  SET $PIECE(TMGVALUE,"^",2)="1" 
  QUIT
  ;
OBR      ;"Purpose: setup for OBR fields
  ;"Uses globally scoped vars: TMGHL7MSG,TMGSEGN,TMGU,TMGINFO
  ;"Creates globally scoped var TMGHL75OBRCOLDT, will be killed in OBRDN
  NEW ASEG MERGE ASEG=TMGHL7MSG(TMGSEGN)
  ;"If collection date/time not specified, use lab reception date/time
  IF $GET(ASEG(7))="",$GET(ASEG(14))'="" DO   ;"#7 = collection date-time, #14=received date-time 
  . NEW DT SET DT=ASEG(14)
  . ;"NO, see note below --> SET DT=$EXTRACT(DT,1,8)  ;"strip time and use only collection DATE
  . ;"NOTICE: If I don't keep time, then I might store duplicate messages in cases
  . ;"   where the lab sends a correction or final result, if the DT in not
  . ;"   sufficiently precise.  So I will use the full collection DT 
  . NEW FMDT SET FMDT=$$HL72FMDT^TMGHL7U3(DT)
  . NEW DATESUSED DO PRIORLDT^TMGLRW01(.TMGHL7MSG,.DATESUSED,.TMGINFO)
  . SET FMDT=$$UNIQUEDT^TMGLRW01(FMDT,.DATESUSED)
  . SET DT=$$FMDT2HL7^TMGHL7U3(FMDT)
  . DO SETPCE^TMGHL7X2(DT,.TMGHL7MSG,.TMGU,TMGSEGN,7)
  . SET TMGHL75OBRCOLDT=1  ;"this is a flag to make a note of the change in OBRDN
  DO OBR^TMGHL72
  QUIT
  ;
OBR4     ;"Purpose: To transform the OBR segment, field 4
  IF $GET(TMGHL7MSG("STAGE"))="PRE" QUIT
  DO OBR4^TMGHL72
  QUIT
  ;
OBR15    ;"Transform Secimen source
  ;"FYI -- Quest doesn't send a specimen source.  
  IF $GET(TMGHL7MSG("STAGE"))="PRE" QUIT
  DO SUSPEC^TMGHL72
  QUIT
  ;
OBR16    ;"Transform Ordering provider.
  DO OBR16^TMGHL72
  QUIT
  ;
OBRDN    ;"Purpose: setup for OBR fields, called *after* fields, subfields etc are processed
  ;"Uses globally scoped vars: TMGSEGN, TMGDD
  IF $GET(TMGHL7MSG("STAGE"))="PRE" QUIT
  NEW TEMP
  DO LABLDATA^TMGHL72(.TEMP,.TMGHL7MSG,"OBR",TMGSEGN) ;
  ;
  NEW ORDINFO MERGE ORDINFO=TMGHL7MSG("ORDER",TMGSEGN)
  NEW TESTNAME SET TESTNAME=$PIECE($GET(TMGHL7MSG("ORDER",TMGSEGN,"IEN60")),"^",2)
  ;
  NEW INFO,PROV,PID  
  ;"NEW TESTNAME SET TESTNAME=$GET(TEMP("Universal Service ID"))
  ;"SET TESTNAME=$PIECE(TESTNAME,TMGU(2),2)
  NEW ONEACSN SET ONEACSN=$GET(TEMP("Filler Order Number"))
  NEW LABADDR SET LABADDR=$GET(TEMP("Filler Field 2"))
  ;"SET LABADDR=$PIECE(LABADDR,TMGU(2),2,999)
  ;"SET LABADDR=$$REPLSTR^TMGSTUT3(LABADDR,"^","; ")
  SET PROV=$GET(TEMP("Ordering Provider"))
  SET PROV=$PIECE(PROV,TMGU(2),2,3)
  SET PROV=$$TRIM^XLFSTR($TRANSLATE(PROV,"^"," "))
  IF PROV="" DO
  . NEW TEMP2 DO LABLDATA^TMGHL72(.TEMP2,.TMGHL7MSG,"ORC") 
  . SET PROV=$GET(TEMP2("Ordering Provider"))
  . SET PROV=$PIECE(PROV,TMGU(2),2,3)
  . SET PROV=$TRANSLATE(PROV,"^"," ")        
  NEW OBSDT SET OBSDT=$GET(TEMP("Observation Date/Time"))
  SET OBSDT=$$HL72FMDT^TMGHL7U3(OBSDT)
  SET OBSDT=$$FMTE^XLFDT(OBSDT)
  NEW RECDT SET RECDT=$GET(TEMP("Specimen Received Date/Time"))
  SET RECDT=$$HL72FMDT^TMGHL7U3(RECDT)
  SET RECDT=$$FMTE^XLFDT(RECDT)        
  NEW RPTDT SET RPTDT=$GET(TEMP("Results Rpt/Status Chng - Date/Time"))
  SET RPTDT=$$HL72FMDT^TMGHL7U3(RPTDT)
  SET RPTDT=$$FMTE^XLFDT(RPTDT)
  NEW STATUS SET STATUS=$GET(TEMP("Result Status"))
  IF STATUS="F" SET STATUS="FINAL"
  IF STATUS="I" SET STATUS="INCOMPLETE/PRELIMINARY"
  IF STATUS="C" SET STATUS="CORRECTED"
  IF STATUS="P" SET STATUS="PRELIMINARY"
  IF STATUS="X" SET STATUS="TEST CANCELED"
  ;       
  NEW TEMP2 DO LABLDATA^TMGHL72(.TEMP2,.TMGHL7MSG,"PID") ;
  NEW PID SET PID=$GET(TEMP2("Patient ID"))
  NEW GENDER SET GENDER=$GET(TEMP2("Sex"))
  IF GENDER="F" SET GENDER="FEMALE"
  IF GENDER="M" SET GENDER="MALE"
  NEW PTDOB SET PTDOB=$GET(TEMP2("Date/Time Of Birth"))
  SET PTDOB=$$HL72FMDT^TMGHL7U3(PTDOB)
  SET PTDOB=$$FMTE^XLFDT(PTDOB,"2D")
  NEW PTNAME SET PTNAME=$TRANSLATE($GET(TEMP2("Patient Name")),TMGU(2),",")
  NEW ACCTN SET ACCTN=$GET(TEMP2("Patient Account Number"))
  NEW PATIENT SET PATIENT=PTNAME_" ("_PTDOB_"), "_GENDER_", Acct #"_ACCTN
  ;
  NEW LINE,ARR,FLD,VALUE SET FLD=""   
  NEW INDENT SET INDENT="  "
  DO ADDTOARR^TMGHL72(.ARR,$$DBLN^TMGHL72())
  DO ADDTOARR^TMGHL72(.ARR,"Test ordered: "_TESTNAME)
  DO ADDTOARR^TMGHL72(.ARR,"Ordering Provider: "_PROV)
  DO ADDTOARR^TMGHL72(.ARR,"Lab Accession Number: "_ONEACSN)
  DO ADDTOARR^TMGHL72(.ARR,"Patient: "_PATIENT)
  DO ADDTOARR^TMGHL72(.ARR,"Lab Patient ID: "_PID)
  SET LINE="Specimen Collection Date: "_OBSDT
  IF $GET(TMGHL75OBRCOLDT)=1 SET LINE=LINE_" <-- see *NOTE*"
  DO ADDTOARR^TMGHL72(.ARR,LINE)
  IF $GET(TMGHL75OBRCOLDT)=1 DO
  . DO ADDTOARR^TMGHL72(.ARR,"  *NOTE*: Collection date/time not provided.")    
  . DO ADDTOARR^TMGHL72(.ARR,"          Using date/time lab RECEIVED instead.")    
  KILL TMGHL75OBRCOLDT
  DO ADDTOARR^TMGHL72(.ARR,"Specimen Received Date: "_RECDT)
  DO ADDTOARR^TMGHL72(.ARR,"Result Report Date: "_RPTDT)
  DO ADDTOARR^TMGHL72(.ARR,"Result Status: "_STATUS)
  IF LABADDR'="" DO
  . DO ADDTOARR^TMGHL72(.ARR,"Test Performed by: ")
  . DO ADDTOARR^TMGHL72(.ARR,INDENT_$PIECE(LABADDR,TMGU(2),2))  ;"Addr1
  . DO ADDTOARR^TMGHL72(.ARR,INDENT_$PIECE(LABADDR,TMGU(2),3))  ;"Addr2
  . SET LINE=$PIECE(LABADDR,TMGU(2),4)_", "             ;"City
  . SET LINE=LINE_$PIECE(LABADDR,TMGU(2),5)_" "         ;"State
  . SET LINE=LINE_$PIECE(LABADDR,TMGU(2),6)             ;"ZIP
  . DO ADDTOARR^TMGHL72(.ARR,INDENT_LINE)  
  . IF $PIECE(LABADDR,TMGU(2),8)'="" DO  ;"2nd address found...
  . . DO ADDTOARR^TMGHL72(.ARR,INDENT_$PIECE(LABADDR,TMGU(2),7))  ;"Addr1
  . . DO ADDTOARR^TMGHL72(.ARR,INDENT_$PIECE(LABADDR,TMGU(2),8))  ;"Addr2
  . . SET LINE=$PIECE(LABADDR,TMGU(2),9)                  ;"City
  . . IF LINE'="" SET LINE=LINE_", "   
  . . SET LINE=LINE_$PIECE(LABADDR,TMGU(2),10)_" "        ;"State
  . . SET LINE=LINE_$PIECE(LABADDR,TMGU(2),11)            ;"ZIP
  . . DO ADDTOARR^TMGHL72(.ARR,INDENT_LINE)
  . . DO ADDTOARR^TMGHL72(.ARR,INDENT_$PIECE(LABADDR,TMGU(2),12))  ;"responsible physician
  . ELSE  DO  
  . . DO ADDTOARR^TMGHL72(.ARR,INDENT_$PIECE(LABADDR,TMGU(2),7))  ;"responsible physician
  ELSE  DO
  . DO ADDTOARR^TMGHL72(.ARR,"Test Performed by:  (not provided)")
  DO ADDTOARR^TMGHL72(.ARR,$$DBLN^TMGHL72())
  ;           
  DO INSRTNTE^TMGHL72(.ARR,.TMGHL7MSG,.TMGU,TMGSEGN)  
  QUIT
  ;
  ;        
OBX  ;"Purpose: to transform the entire OBX segment before any fields are processed
  ;"Uses TMGSEGN, that is set up in from TMGHL7X* code before calling here. 
  DO OBX^TMGHL72
  NEW ASEG MERGE ASEG=TMGHL7MSG(TMGSEGN)
        ;"Format of ASEG(3)=<TestCode>^<Test Text>^<Coding System>^<Alt Id>^<Alt Text>^<Alt coding system>
  NEW QALTID SET QALTID=$$TRIM^XLFSTR($GET(ASEG(3,4)))
  NEW QALTNAME SET QALTNAME=$$TRIM^XLFSTR($GET(ASEG(3,5)))
  IF QALTNAME="",QALTID'="" DO
  . SET QALTNAME="TEST "_QALTID
  . DO SETPCE^TMGHL7X2(QALTNAME,.TMGHL7MSG,.TMGU,TMGSEGN,3,5)
  QUIT
  ;
OBX3     ;"Purpose: To transform the OBX segment, field 3 -- Observation Identifier
  IF $GET(TMGHL7MSG("STAGE"))="PRE" QUIT
  DO OBX3^TMGHL72
  QUIT
  ;
OBX5     ;"Purpose: To transform the OBX segment, field 5 -- Observation value
  IF $GET(TMGHL7MSG("STAGE"))="PRE" QUIT
  IF TMGVALUE="TNP" SET TMGVALUE="(test not performed)"
  IF TMGVALUE="" DO
  . NEW NEXTSEGN SET NEXTSEGN=+$ORDER(TMGHL7MSG(TMGSEGN))
  . NEW NEXTSEG SET NEXTSEG=""
  . IF NEXTSEGN>0 SET NEXTSEG=$GET(TMGHL7MSG(NEXTSEGN,"SEG"))
  . IF NEXTSEG'="NTE" QUIT
  . SET TMGVALUE="(see in comments below)"
  . IF $$NUMNTEFO^TMGHL72(TMGSEGN)'=1 QUIT
  . NEW ASEG MERGE ASEG=TMGHL7MSG(NEXTSEGN)
  . NEW ANTE SET ANTE=$GET(ASEG(3))
  . IF $LENGTH(ANTE)>60 QUIT
  . SET TMGVALUE=ANTE
  . KILL TMGHL7MSG(NEXTSEGN)
  IF TMGVALUE="" SET TMGVALUE=" "
  DO OBX5^TMGHL72
  QUIT
  ;
OBX15    ;"Purpose: To transform the OBX segment, field 15 ---- Producer's ID
  DO OBX15^TMGHL72
  QUIT
  ;
OBX16    ;"Purpose: To transform the OBX segment, field 16 ---- Responsibile Observer
  DO OBX16^TMGHL72
  QUIT
  ;
OBX18    ;"Purpose: To transform the OBX segment, field 18 ---- Equipment Identifier (EI)
  DO OBX18^TMGHL72
  QUIT
  ;
NTE  ;"Transfor the entire NTE segment
  ;"Input: Uses globally scoped vars: TMGHL7MSG, TMGU, TMGSEGN
  NEW ASEG SET ASEG=$GET(TMGHL7MSG(TMGSEGN))
  IF $LENGTH(ASEG,TMGU(1))<4 DO
  . DO SETPCE^TMGHL7X2(" ",.TMGHL7MSG,.TMGU,TMGSEGN,3)
  QUIT
  ;
NTE3     ;"Purpose: To transform the NTE segment, field 3 (the comments)
  ;"Uses TMGSEGN, that is set up in from TMGHL7X* code before calling here. 
  IF $GET(TMGHL7MSG("STAGE"))="PRE" QUIT
  NEW NTELN SET NTELN=+$GET(TMGHL7MSG(TMGSEGN,1))
  NEW DIVLNLEN SET DIVLNLEN=50
  ;"SET TMGLASTOBXSEGN=+$GET(TMGLASTOBXSEGN)
  SET TMGLASTOBX("SEGN")=+$GET(TMGLASTOBX("SEGN"))
  IF TMGLASTOBX("SEGN")>0,(NTELN=1) DO
  . NEW TESTNAME SET TESTNAME=$PIECE($GET(TMGHL7MSG(TMGLASTOBX("SEGN"),"RESULT","IEN60")),"^",2)
  . NEW LINE SET LINE="~~~ Comment for: "_TESTNAME_" "
  . FOR  QUIT:$LENGTH(LINE)>DIVLNLEN  SET LINE=LINE_"~"
  . DO PREFIXNT^TMGHL72(LINE,.TMGHL7MSG,.TMGU,TMGSEGN)
  DO NTE3^TMGHL72
  NEW NEXTSEGN SET NEXTSEGN=+$ORDER(TMGHL7MSG(TMGSEGN))
  NEW NEXTSEG SET NEXTSEG=""
  IF NEXTSEGN>0 SET NEXTSEG=$GET(TMGHL7MSG(NEXTSEGN,"SEG"))
  IF (NEXTSEG'=""),(NEXTSEG'="NTE"),(TMGLASTOBX("SEGN")>0) DO  
  . NEW ARR,LINE SET LINE=""
  . FOR  QUIT:$LENGTH(LINE)>DIVLNLEN  SET LINE=LINE_"~"
  . DO ADDTOARR^TMGHL72(.ARR,LINE)
  . DO APPNDNTE^TMGHL72(.ARR,.TMGHL7MSG,.TMGU,TMGSEGN)
  QUIT
  ;
SUORL    ;"Purpose: Setup TMGINFO("ORL") and TMGINFO("LOC") and TMGINFO("INSTNAME")
  DO SUORL^TMGHL72
  QUIT
  ;