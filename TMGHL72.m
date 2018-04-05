TMGHL72 ;TMG/kst-HL7 transformation engine processing ;5/9/17, 4/1/18
              ;;1.0;TMG-LIB;**1**;03/26/11
 ;
 ;"TMG HL7 TRANSFORMATION CALL-BACK FUNCTIONS
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"NOTE: This is common code, usable by multiple labs. 
 ;"      FYI -- Pathgroup code is in TMGHL73
 ;"             Laughlin RADIOLOGY is in TMGHL74R
 ;"             Laughlin LAB code is in TMGHL74
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;"<Many call-back hooks for transformation engine> 
 ;"XMSG    Process entire message before processing segments
 ;"XMSG2   Process entire message after processing segments
 ;"XMSG2B  
 ;"XMSH4   Process MSH segment, FLD 4 (Sending Facility)
 ;"XMSH15  Process MSH segment, FLD 15
 ;"XMSH16  Process MSH segment, FLD 16
 ;"PID     transform the PID segment, esp SSN
 ;"XORC1   Process empty ORC message, field 1
 ;"ORC2    Process ORC message, field 2
 ;"XORC12  Process empty ORC message, field 12
 ;"XORC13  Process empty ORC message, field 13
 ;"OBR     setup for OBR fields.
 ;"OBR4    transform the OBR segment, field 4
 ;"OBR15   Transform Secimen source
 ;"OBR16   Transform Ordering provider.
 ;"OBX     transform the entire OBX segment before any fields are processed
 ;"OBX3    transform the OBX segment, field 3 -- Observation Identifier
 ;"OBX5    transform the OBX segment, field 5 -- Observation value
 ;"OBX14   transform the OBX segment, field 14 ---- Date/Time of observation
 ;"OBX15   transform the OBX segment, field 15 ---- Producer's ID
 ;"OBX16   transform the OBX segment, field 16 ---- Responsibile Observer
 ;"OBX18   transform the OBX segment, field 18 ---- Equipment Identifier (EI)
 ;"NTE3    transform the NTE segment, field 3 (the comments)
 ;"SUPROV  Setup TMGINFO("PROV") -- Ordering provider.
 ;"HL7N2FMN(TMGU,HL7NAME,LNAME,FNAME,MNAME)  --CONVERT HL7-FORMAT NAME TO FILEMAN-FORMAT NAME
 ;"SUORL   --Purpose: Setup TMGINFO("ORL") and TMGINFO("LOC") -- Ordering locations
 ;"VALIDVAL(TMGENV,TMGWKLD,TMGVALUE,MAP) --Determine IF a lab/result value can be stored of a given lab
 ;"SUSPEC  --Transform Secimen source
 ;"ADD2NTE(ARR,TMGHL7MSG,TMGU)   -- Add lines from ARR as extra NTE segments
 ;"ADD2ARRI(ARR,LABEL,VALUE)  --ADD TO ARRAY IF VALUE IS NOT ""
 ;"ADDTOARR(ARR,LINE)   ;
 ;"ADDA2ARR(OUT,INARR) -- ADD ARRY TO OUTPUT ARRAY
 ;"ISFINALN(TMGHL7MSG,SEGN)  -- IS NTE BLOCK AT THE END OF THE MESSAGE?  I.E. NO FOLLOWING SEGMENTS?
 ;"DBLN()  -- Return ascii text line
 ;"NUMNTEFO(SEGN)  -- NUMBER OF NTE SEGMENTS THAT FOLLOW AFTER SEGN
 ;"LABLDATA(OUT,TMGHL7MSG,SEG,SEGN)  ;
 ;"INSRTNTE(ARR,TMGHL7MSG,TMGU,SEGNPRIOR)   -- Insert note segment after SEGNPRIOR from ARR
 ;"PREFIXNT(LINE,TMGHL7MSG,TMGU,SEGN)  -- PREFIX NOTE (INSERT LINE BEFORE INDEX LINE)
 ;"PREFXNTA(ARR,TMGHL7MSG,TMGU,SEGN)  -- PREFIX NOTE ARRAY (INSERT ARRAY BEFORE INDEX LINE)
 ;"APPNDNTE(ARR,TMGHL7MSG,TMGU,LASTNTESEGN)   -- APPEND NOTE 
 ;"KILLDNRS(TMGHL7MSG)  -- Scan an remove all OBX's with lab value of "DNR", and also associated NTE segs.
 ;"DELSEG(TMGHL7MSG,SEGN)  -- DELETE SEGMENT, AND REFERENCES TO IT IN THE CROSS REFERENCES
 ;"GETSEGAR(TMGHL7MSG,SEGN,SEGNAME,OUT) -- Return array with all segments matching SEGNAME *following* SEGN, until non-match found
 ;"GETOBXAR(TMGHL7MSG,SEGN,OUT) -- Return array with all "OBX" segments *following* SEGN, until non-OBX found
 ;"GETNTARR(TMGHL7MSG,SEGN,OUT) -- Return array with all "NTE" segments *following* SEGN, until non-NTE found
 ;"SUMOBXAR(OBXAR,OUT)  -- Summarize OBX Array 
 ;"ADD2RSLT(RESULT,ARR,SEGN,FLD,PREFIX) -- A utility function for HNDUPOBX below
 ;"HNDUPOBX(TMGHL7MSG,SEGN,TMGU) -- Handle situation with an OBR having duplicate OBX's
 ;"SUMOBXSTA(TMGHL7MSG,SEGN,OUT) -- Get net status from OBX's after an OBR
 ;"SUMNTARR(TMGHL7MSG,SEGN,OUT) -- Convert NTE's after SEGN into simple ARR(#)=Text array
 ;"CHKOBRNT(TMGHL7MSG,SEGN,OBRCOMMENTS) -- CHECK / Handle NTE's that follow OBR (i.e. order comments) 
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"TMGSTUTL, all the HL*, LA* code that the HL processing path normally calls.
 ;"=======================================================================
 ; 
 ;"---------------------------------------------------------------
 ;"===============================================================
 ;"   Below are the call-back functions to handle transformation
 ;"   hooks, called by the XFMSG^TMGHL7X engine
 ;"===============================================================
 ;"---------------------------------------------------------------
 ;
XMSG    ;"Purpose: Process entire message before processing segments
        ;"Input: Uses globally scoped vars: TMGHL7MSG, TMGU, HLREC
        NEW X,Y
        IF $GET(IEN62D4)>0 GOTO XMSGB
        SET DIC=62.4,DIC(0)="M"
        SET X=$GET(HLREC("SAN"))
        IF X="" DO
        . SET X=$$GETPCE^TMGHL7X2(.TMGHL7MSG,1,2)  ;"
        IF X="" DO  GOTO XMDN
        . SET TMGXERR="In XMSG.TMGHL72: HLREC(""SAN"") empty."
        DO ^DIC
        IF +Y'>0 DO  GOTO XMDN
        . SET TMGXERR="In XMSG.TMGHL72: Unable to find AUTO INSTRUMENT (62.4) record matching sending application name '"_X_"'"
        IF $DATA(IEN62D4) NEW IEN62D4
        SET IEN62D4=+Y  ;"cleaned up in XMSG2
XMSGB   IF $DATA(TMGU)=0 DO  GOTO XMDN
        . SET TMGXERR="In XMSG.TMGHL72: Array with divisor chars (TMGU) not SET up."
        ;
        ;"Place empty ORC segment in, if not already present
        NEW SEGN SET SEGN=+$ORDER(TMGHL7MSG("B","ORC",0))
        IF SEGN>0 GOTO XMSGC
        NEW PIDSEGN SET PIDSEGN=+$ORDER(TMGHL7MSG("B","PID",0))
        IF PIDSEGN'>0 DO  GOTO XMDN
        . SET TMGXERR="In XMSG.TMGHL72: PID segment not found in message."
        NEW ORCSEGN SET ORCSEGN=PIDSEGN+0.5
        NEW S SET S="ORC"
        ;"Create segment with empty fields, so that transforms will be called for them
        NEW I FOR I=2:1:14 SET $PIECE(S,TMGU(1),I)=""
        DO PRSESEG^TMGHL7X2(ORCSEGN,S,.TMGHL7MSG,.TMGU)
        ;
        NEW DONE SET DONE=0
        NEW ORD SET ORD=0
        FOR  SET ORD=$ORDER(TMGHL7MSG("PO",ORD)) QUIT:(+ORD'>0)!DONE  DO
        . IF TMGHL7MSG("PO",ORD)'=PIDSEGN QUIT
        . SET TMGHL7MSG("PO",ORD+0.5)=ORCSEGN
        . SET DONE=1
        ;
XMSGC   KILL TMGLASTOBX,TMGNTEADD,TMGDD
        DO GETDD^TMGHL7X3(.TMGDD)
        DO ENSURSEG^TMGHL7X2(.TMGHL7MSG,"NTE",.TMGU)
XMDN    QUIT
        ;
        ;
XMSG2   ;"Purpose: Process entire message after processing segments
        KILL IEN62D4,TMGLASTOBX,TMGNTEADD
        QUIT
        ;
XMSG2B  ;        
        NEW ARR DO ADDTOARR^TMGHL72(.ARR,$$DBLN^TMGHL72())  ;"Add terminal ascii line to notes
        DO ADD2NTE^TMGHL72(.ARR,.TMGHL7MSG,.TMGU)
        KILL TMGINFO,TMGDD
        QUIT
        ;
XMSH4   ;"Purpose: Process MSH segment, FLD 4 (Sending Facility)
        ;"Add a second piece that is pointer to INSTITUTION file (#4)
        NEW DIC,X,Y SET DIC=4,DIC(0)="M"
        SET X=TMGVALUE
        DO ^DIC
        IF Y>0 DO
        . SET TMGHL7MSG(TMGSEGN,4,1)=TMGVALUE
        . SET TMGHL7MSG(TMGSEGN,4,2)=+Y
        . SET $PIECE(TMGVALUE,TMGU(2),2)=+Y
        ELSE  DO
        . SET TMGXERR="IN XMSH4^TMGHL72: Unable to find entry in INSTITUTION (#4) file to match '"_TMGVALUE_"'"
        QUIT
        ;
XMSH15  ;"Purpose: Process MSH segment, FLD 15
        SET TMGVALUE="NE"
        QUIT
XMSH16  ;"Purpose: Process MSH segment, FLD 16
        SET TMGVALUE="NE"
        QUIT
        ;
PID     ;"Purpose: To transform the PID segment, esp SSN
        ;"Input: Uses globally scoped vars: TMGHL7MSG, TMGU, TMGVALUE
        ;"Will try to put DFN into PID-4 ("alternate PID") field
        NEW SOURCE SET SOURCE=$PIECE(TMGVALUE,TMGU(1),19)
        NEW NAME SET NAME=$$GETPCE^TMGHL7X2(.TMGHL7MSG,"PID",5)
        IF NAME["""" DO
        . SET NAME=$TRANSLATE(NAME,"""","")  ;"remove any quotes("") from names
        . SET $PIECE(TMGVALUE,TMGU(1),5)=NAME
        . DO SETPCE^TMGHL7X2(NAME,.TMGHL7MSG,.TMGU,TMGSEGN,5)
        NEW DFN SET DFN=-1
        ;"IF SOURCE="" DO  GOTO PIDDN
        ;". SET TMGXERR="In PID.TMGHL72: No SSN provided in field 19 of 'PID' segment in HL7 message"
        IF SOURCE'="" DO  
        . SET SOURCE=$TRANSLATE(SOURCE,"-","")
        . IF SOURCE="999999999" SET SOURCE=""
        . SET $PIECE(TMGVALUE,TMGU(1),19)=SOURCE
        . SET DFN=$$GETDFN^LA7VHLU2(SOURCE,1)
        . SET $PIECE(TMGVALUE,TMGU(1),19)=SOURCE
        . SET $PIECE(SOURCE,TMGU(2),4)=170
        . SET $PIECE(SOURCE,TMGU(2),5)="SS"
        . SET $PIECE(TMGVALUE,TMGU(1),3)=SOURCE  ;"put SSN in as patient identifier.
        IF DFN=-1 DO
        . NEW INFO SET INFO=""
        . NEW NAME SET NAME=$$GETPCE^TMGHL7X2(.TMGHL7MSG,"PID",5)
        . NEW LNAME SET LNAME=$PIECE(NAME,TMGU(2),1)        
        . NEW FNAME SET FNAME=$PIECE(NAME,TMGU(2),2)
        . SET NAME=LNAME_","_FNAME   
        . NEW HL7DOB SET HL7DOB=$$GETPCE^TMGHL7X2(.TMGHL7MSG,"PID",7)
        . NEW FMDT SET FMDT=$$HL72FMDT^TMGHL7U3(HL7DOB)
        . NEW SEX SET SEX=$$GETPCE^TMGHL7X2(.TMGHL7MSG,"PID",8)
        . SET INFO("SSNUM")=SOURCE
        . SET INFO("NAME")=NAME
        . SET INFO("DOB")=FMDT
        . SET INFO("SEX")=SEX
        . SET DFN=$$GETDFN^TMGGDFN(.INFO,0)        
        SET $PIECE(TMGVALUE,TMGU(1),4)=DFN
        ;"The following IF block, autoregistered the patient somehow and DFN logic was incorrect. Created duplicate patients.
        ;"IF (DFN'>0),(SOURCE?9N) DO  ;"If patient doesn't have SSN, then store now.
        ;". NEW SSN SET SSN=$PIECE($GET(^DPT(DFN,0)),"^",9)
        ;". IF SSN'="" QUIT  ;"If already has SSN, then QUIT
        ;". NEW TMGFDA,TMGMSG
        ;". SET TMGFDA(2,DFN_",",.09)=SOURCE
        ;". DO FILE^DIE("K","TMGFDA","TMGMSG")
        ;". IF $DATA(TMGMSG("DIERR")) DO
        ;". . SET TMGXERR=$$GETERRST^TMGDEBU2(.TMGMSG)
        ;"NEW DFN SET DFN=$$GETDFN^LA7VHLU2(SOURCE,1)
PIDDN   QUIT
        ;
XORC1   ;"Purpose: Process empty ORC message, field 1
        SET TMGVALUE="NW"
        QUIT
        ;"
ORC2    ;"Purpose: Process ORC message, field 2
        SET TMGVALUE=$PIECE(+$GET(TMGVALUE),"^",1)        
        IF TMGVALUE>0 DO CMPLTORD^TMGHL7U(TMGVALUE,.TMGHL7MSG)
        QUIT
        ;"
XORC12  ;"Purpose: Process empty ORC message, field 12
        NEW PROV SET PROV=$GET(TMGINFO("PROV"))
        IF PROV="" DO SUPROV SET PROV=$GET(TMGINFO("PROV"))
        IF PROV="" SET TMGXERR="Unable to determine provider for field #12 of ORC segment"
        SET TMGVALUE=PROV
        DO TMGLOG^HLCSTCP1("In XORC12.TMGHL72")
        QUIT
XORC13  ;"Purpose: Process empty ORC message, field 13
        NEW ORL SET ORL=$GET(TMGINFO("ORL"))
        IF ORL="" DO SUORL SET ORL=$GET(TMGINFO("ORL"))
        IF ORL="" SET TMGXERR="Unable to determine ordering location for field #13 of ORC segment"
        SET TMGVALUE=ORL
        QUIT
        ;
OBR     ;"Purpose: setup for OBR fields.
        ;"Input: Uses globally scoped vars: TMGVALUE,TMGENV,TMGHL7MSG,TMGSEGN
        ;"NEW CE SET CE=$PIECE(TMGVALUE,TMGU(1),4)  ;"e.g. 'CRE^CREATININE'
        ;"NEW VACODE SET VACODE=$$XFTEST(.TMGENV,CE,"O")  ;"WRKLDCode^LabPrintName^LabName^LabIEN60, or -1^Error Message        
        NEW ARR MERGE ARR=TMGHL7MSG(TMGSEGN,"ORDER")
        NEW NLT SET NLT=$GET(ARR("NLT"),"??")
        NEW IEN60 SET IEN60=$GET(ARR("IEN60"),"??")
        NEW LABNAME SET LABNAME=$PIECE(IEN60,"^",2)
        NEW LABPRNAME SET LABPRNAME=$PIECE(IEN60,"^",3)
        SET VACODE=NLT_"^"_LABPRNAME_"^"_LABNAME_"^"_+IEN60
        ;
        SET TMGINFO("VACODE")=VACODE
        SET TMGINFO("VACODE","OBR")=VACODE
OBRDN   QUIT

OBR4    ;"Purpose: To transform the OBR segment, field 4
        ;"Input: Uses globally scoped vars: TMGVALUE
        ;"Transform 'Universal Service ID' -- test name
        NEW VACODE SET VACODE=$GET(TMGINFO("VACODE"))
        IF VACODE="" SET TMGXERR="In OBR4.TMGHL72: OBR setup code didn't fire to setup TMGINFO(""VACODE"")."
        SET TMGVALUE=$P(VACODE,"^",1)_TMGU(2)_$P(VACODE,"^",2)_TMGU(2)_"99VA64"
OBR4DN  QUIT
        ;
OBR15   ;"Transform Secimen source
        ;"NOTE: consider later if this could be merged with SUSPEC^TMGHL72
        NEW VACODE SET VACODE=$GET(TMGINFO("VACODE"))
        IF VACODE="" SET TMGXERR="In OBR15.TMGHL72: OBR setup code didn't fire to setup TMGINFO(""VACODE"")." GOTO OBR15DN
        NEW SOURCE SET SOURCE=TMGVALUE
        NEW ORIGSRC SET ORIGSRC=SOURCE
        NEW IEN60 SET IEN60=$PIECE(VACODE,"^",4)
        NEW IEN64D061 SET IEN64D061=$PIECE($GET(^LAB(60,IEN60,"TMG")),"^",1)
        IF IEN64D061'>0 DO  GOTO OBR15DN
        . SET TMGXERR="In OBR15.TMGHL72: No default specimen source found for record #"_IEN60_" in field 22700 in file# 60"
        NEW ZN SET ZN=$GET(^LAB(64.061,IEN64D061,0))
        NEW CODE SET CODE=$PIECE(ZN,"^",2)
        IF CODE="" DO  GOTO OBR15DN
        . SET TMGXERR="In OBR15.TMGHL72: No specimen source code found in File 64.061, record #"_IEN64D061
        NEW CODENAME SET CODENAME=$PIECE(ZN,"^",1)
        IF CODENAME="" SET CODENAME=CODE
        SET SOURCE=CODE_TMGU(2)_CODENAME_TMGU(2)_"HL70070"
        IF (ORIGSRC'=""),(ORIGSRC'=SOURCE) DO
        . ;"Later may need processing IF hospital will send specimen.
        . SET TMGXERR="HL7 message unexpectedly has source value in OBR segment #15.  Edit TMGHL71.m code to handle.  Got '"_SOURCE_"'"
        SET TMGVALUE=SOURCE
OBR15DN QUIT
        ;
OBR16   ;"Transform Ordering provider.
        NEW PROV SET PROV=$GET(TMGINFO("PROV"))
        IF PROV="" DO SUPROV SET PROV=$GET(TMGINFO("PROV"))
        SET TMGVALUE=PROV
        QUIT
        ;
OBX     ;"Purpose: to transform the entire OBX segment before any fields are processed
        ;"Uses TMGSEGN, that is set up in from TMGHL7X* code before calling here.
        ;"SET TMGLASTOBXSEGN=TMGSEGN  ;"Will be killed in MSG2^TMHL72        
        SET TMGLASTOBX("SEGN")=TMGSEGN  ;"Will be killed in MSG2^TMHL72        
        QUIT
        ;
OBX3    ;"Purpose: To transform the OBX segment, field 3 -- Observation Identifier
        ;"Input: Uses globally scoped vars: TMGHL7MSG, TMGU, TMGVALUE, IEN62D4,
        ;"       TMGSEGN, TMGINFO, TMGENV
        ;"Example TMGVALUE -- 'CRE^CREATININE'
        ;"NEW VACODE SET VACODE=$$XFTEST(.TMGENV,TMGVALUE,"R")   ;"WRKLDCode^LabPrintName^LabName^LabIEN60, or -1^Error Message
        ;"SET TMGVALUE=$P(VACODE,"^",1)_TMGU(2)_$P(VACODE,"^",2)_TMGU(2)_"99VA64"
        NEW ARR MERGE ARR=TMGHL7MSG(TMGSEGN,"RESULT")
        NEW NLT SET NLT=$GET(ARR("NLT"),"??")
        NEW LABPRNAME SET LABPRNAME=$PIECE($GET(ARR("IEN60")),"^",3)
        SET TMGVALUE=NLT_TMGU(2)_LABPRNAME_TMGU(2)_"99VA64"
        NEW LABNAME SET LABNAME=$PIECE($GET(ARR("IEN60")),"^",2)
        IF LABNAME="" SET LABNAME=LABPRNAME
        SET TMGLASTOBX("NAME")=LABNAME
        SET TMGLASTOBX("SEGN")=TMGSEGN
        ;"SET TMGINFO("MOST RECENT OBX")=LABNAME
        ;"SET TMGINFO("MOST RECENT OBX","SEGN")=TMGSEGN
OBX2DN  QUIT
        ;
OBX5    ;"Purpose: To transform the OBX segment, field 5 -- Observation value
        ;"Input: Uses globally scoped vars: TMGHL7MSG, TMGU, TMGVALUE, TMGSEGN, IEN22720, TMGENV
        ;"       Setup in XFMSG^TMGHL7X (transformation engine that eventually leads to this function)        
        ;"Example TMGVALUE -- '< 0.1'
        IF TMGVALUE["^" SET TMGVALUE=$PIECE(TMGVALUE,"^",1)
        NEW TMGWKLD SET TMGWKLD=$$GETPCE^TMGHL7X2(.TMGHL7MSG,TMGSEGN,3,1)  ;" TMGWKLD= TEST ID
        NEW MAP MERGE MAP=TMGHL7MSG(TMGSEGN,"RESULT")
        NEW TMGRESULT SET TMGRESULT=$$VALIDVAL(.TMGENV,TMGWKLD,TMGVALUE,.MAP)
        IF TMGRESULT=1 GOTO OBX5DN
        DO TMGLOG^HLCSTCP1("In OBX5.TMGHL72: TMGRESULT="_TMGRESULT)
        IF TMGRESULT'["[INVALID-VAL]" GOTO OBX5DN
        ;"If valid mapping found, then just use it and continue
        IF $DATA(MAP("REPLACEMENT MAP",TMGVALUE))>0 DO
        . SET TMGVALUE=$GET(MAP("REPLACEMENT MAP",TMGVALUE))
        . SET TMGRESULT=1 ;"Successful mapping 
        . DO TMGLOG^HLCSTCP1("In OBX5.TMGHL72:Success.  New TMGVALUE="_TMGVALUE)
OBX5DN  IF TMGRESULT<0 SET TMGXERR=$PIECE(TMGRESULT,"^",2,99)
        QUIT
OBX14   ;"Purpose: To transform the OBX segment, field 14 ---- Date/Time of observation
        ;"THIS NEEDS TO BE COMPLETED
        QUIT
OBX15   ;"Purpose: To transform the OBX segment, field 15 ---- Producer's ID
        ;"Input: Uses globally scoped vars: TMGHL7MSG, TMGU, TMGVALUE, IEN62D4,TMGINFO
        NEW ORL SET ORL=$GET(TMGINFO("ORL"))
        IF ORL="" DO SUORL SET ORL=$GET(TMGINFO("ORL"))
        IF ORL="" SET TMGXERR="Unable to determine ordering location for field #15 of OBX segment"
        SET TMGVALUE=ORL
        QUIT
OBX16   ;"Purpose: To transform the OBX segment, field 16 ---- Responsibile Observer
        ;"Input: Uses globally scoped vars: TMGHL7MSG, TMGU, TMGVALUE, IEN62D4,TMGINFO
        NEW PROV SET PROV=$GET(TMGINFO("PROV"))
        IF PROV="" DO SUPROV SET PROV=$GET(TMGINFO("PROV"))
        IF PROV="" SET TMGXERR="Unable to determine provider for field #16 of OBX segment"
        SET TMGVALUE=PROV
        QUIT
OBX18   ;"Purpose: To transform the OBX segment, field 18 ---- Equipment Identifier (EI)
        NEW TMGINSTNAME SET TMGINSTNAME=$GET(TMGINFO("INSTNAME"))
        IF TMGINSTNAME="" DO SUPROV SET TMGINSTNAME=$GET(TMGINFO("INSTNAME"))
        IF TMGINSTNAME="" SET TMGXERR="Unable to determine instituion name for field #18 of OBX segment"
        SET TMGVALUE=TMGINSTNAME
        QUIT
        ;
NTE3    ;"Purpose: To transform the NTE segment, field 3 (the comments)
        SET TMGVALUE=$TRANSLATE(TMGVALUE,"""","'") ;" quote char disallowed by input transform.
        IF $EXTRACT(TMGVALUE,1)="-" SET TMGVALUE=" "_TMGVALUE   ;""-" not allowed in space 1
        IF TMGVALUE["^" SET TMGVALUE=$$REPLSTR^TMGSTUT3(TMGVALUE,"^","/\") 
        IF TMGVALUE["""" SET TMGVALUE=$TRANSLATE(TMGVALUE,"""","'") 
        IF TMGVALUE="" SET TMGVALUE="  "
        NEW TMGCODE SET TMGCODE=$PIECE($GET(^DD(63.041,.01,0)),"^",5,999)
        NEW X SET X=TMGVALUE
        IF '$DATA(X) GOTO NTE3DN
        DO 
        . NEW DA,LRX 
        . SET DA(2)=+$ORDER(^LR(0)) ;"test lab against arbitrary lab user IEN
        . IF DA(2)'>0 QUIT
        . SET DA(1)=$ORDER(^LR(DA(2),"CH",0))
        . IF DA(1)'>0 QUIT
        . SET DA=+$ORDER(^LR(DA(2),"CH",DA(1),1,0))
        . NEW $ETRAP SET $ETRAP="WRITE ""(Invalid M Code!.  Error Trapped.)"",$ECODE,! SET $ETRAP="""",$ECODE="""""
        . XECUTE TMGCODE		 
        IF $DATA(X) GOTO NTE3DN
        SET TMGXERR="Comment line fails input transform: ["_TMGVALUE_"]"
        SET TMGRESULT="-1^"_TMGXERR
NTE3DN  QUIT
        ;                
        ;        
SUPROV  ;"Purpose: Setup TMGINFO("PROV") -- Ordering provider.
        ;"Input: Uses globally scoped vars: TMGHL7MSG, TMGU, HLREC
        ;"Output: Sets globally scoped variable TMGINFO("PROV")
        ;"Results: None.  TMGXERR SET IF error
        KILL TMGINFO("PROV")
        NEW PROV SET PROV=$$GETPCE^TMGHL7X2(.TMGHL7MSG,"OBR",16)
        IF $PIECE(PROV,TMGU(2),13)="NPI" DO
        . NEW NPI,ADUZ
        . SET NPI=$PIECE(PROV,TMGU(2),1)
        . IF NPI="" DO  QUIT
        . . SET TMGXERR="In SPROV.TMGHL72: Set to NPI, but NPI is blank in field #16 of 'OBR' segment in HL7 message"
        . SET ADUZ=+$ORDER(^VA(200,"ANPI",NPI,0))
        . IF ADUZ'>0 DO  QUIT
        . . SET PROV="^Doctor^Unspecified^"
        . ;"IF ADUZ'>0 DO  QUIT
        . ;". SET TMGXERR="In SPROV.TMGHL72: NPI "_NPI_" could not be found in ^VA(200,'ANPI' index"
        . NEW NAME SET NAME=$PIECE($GET(^VA(200,ADUZ,0)),"^",1)
        . IF NAME="" DO  QUIT
        . . SET TMGXERR="In SPROV.TMGHL72: ADUZ "_ADUZ_" did not return a value name."
        . NEW LNAME,FNAME
        . SET LNAME=$PIECE(NAME,",",1)
        . SET FNAME=$PIECE(NAME,",",2)
        . SET FNAME=$PIECE(FNAME," ",1)
        . SET PROV=ADUZ_TMGU(2)_LNAME_TMGU(2)_FNAME
        . SET TMGINFO("PROV")=PROV
        IF $DATA(TMGINFO("PROV"))=0,$GET(TMGXERR)="" DO
        . NEW LNAME,FNAME,MNAME
        . SET TMGINFO("PROV","ORIGINAL")=PROV
        . IF $$UP^XLFSTR(PROV)'["TOPPENBERG" SET PROV="^Doctor^Unspecified^"
        . IF PROV="" DO  QUIT
        . . SET TMGXERR="In SUPROV.TMGHL72: Ordering provider not provided in field #16 or 'OBR' segment in HL7 message"
        . ;"SET LNAME=$PIECE(PROV,TMGU(2),2)
        . ;"SET FNAME=$PIECE(PROV,TMGU(2),3)
        . ;"SET MNAME=$PIECE(PROV,TMGU(2),4)
        . ;"IF MNAME["-STATCARE" SET MNAME=""
        . ;"IF MNAME["-APP" SET MNAME=""
        . ;"NEW NAME SET NAME=LNAME_","_FNAME_" "_MNAME
        . ;"IF (LNAME="TOPPENBERG")&(FNAME="EE") SET FNAME="MARCIA"  
        . ;"SET NAME=$$TRIM^XLFSTR(NAME)
        . SET NAME=$$HL7N2FMN(.TMGU,PROV,.LNAME,.FNAME,.MNAME)
        . NEW DIC,X,Y SET DIC=200,DIC(0)="M",X=NAME
        . DO ^DIC
        . IF Y'>0 DO  QUIT
        . . SET TMGXERR="In SUPROV.TMGHL72: Unable find provider in lookup: '"_NAME_"'"
        . SET PROV=+Y_TMGU(2)_LNAME_TMGU(2)_FNAME
        . SET $PIECE(PROV,TMGU(2),13)="TMGDUZ"
        . SET TMGINFO("PROV")=PROV
SPVDN   QUIT
        ;
HL7N2FMN(TMGU,HL7NAME,LNAME,FNAME,MNAME)  ;"CONVERT HL7-FORMAT NAME TO FILEMAN-FORMAT NAME
        SET LNAME=$PIECE(HL7NAME,TMGU(2),2)
        SET FNAME=$PIECE(HL7NAME,TMGU(2),3)
        SET MNAME=$PIECE(HL7NAME,TMGU(2),4)
        IF MNAME["-STATCARE" SET MNAME=""
        IF MNAME["-APP" SET MNAME=""
        iF (LNAME="TOPPENBERG")&(FNAME="EE") SET FNAME="MARCIA"  
        NEW TMGRESULT SET TMGRESULT=LNAME_","_FNAME_" "_MNAME        
        QUIT $$TRIM^XLFSTR($$UP^XLFSTR(TMGRESULT))
        ;
SUORL   ;"Purpose: Setup TMGINFO("ORL") and TMGINFO("LOC") -- Ordering locations
        ;"   also TMGINFO("INSTNAME")
        ;"Input: Uses globally scoped vars: TMGHL7MSG, TMGU, HLREC,IEN62D4
        ;"Output: Sets globally scoped variable TMGINFO()
        ;"Results: None.  TMGXERR SET IF error
        NEW TMGINSTIT SET TMGINSTIT=+$PIECE($GET(^LAB(62.4,IEN62D4,"TMG")),"^",2)
        IF TMGINSTIT'>0 DO  GOTO SPODN
        . SET TMGXERR="In SUORL.TMGHL72: No peforming institution found in field #22701, TMG PERFORMING INSTITUTION (a custom text field) in AUTO INSTRUMENT record #"_IEN62D4
        NEW TMGSTATION SET TMGSTATION=$PIECE($GET(^DIC(4,TMGINSTIT,99)),"^",1)
        IF TMGSTATION="" DO  GOTO SPODN
        . SET TMGXERR="In SUORL.TMGHL72: No STATION NUMBER (Field #99) found in INSTITUTION file (#4), record #"_TMGINSTIT
        NEW TMGINSTNAME SET TMGINSTNAME=$PIECE($GET(^DIC(4,TMGINSTIT,0)),"^",1)
        SET TMGINFO("INSTNAME")=TMGINSTNAME
        NEW TMGOBSERVER SET TMGOBSERVER=TMGSTATION_TMGU(2)_TMGINSTNAME_TMGU(2)_"99VA4"
        SET TMGINFO("LOC")=TMGOBSERVER
        SET TMGINFO("ORL")=TMGINSTNAME_TMGU(2)_TMGU(2)_TMGU(2)_TMGSTATION
SPODN   QUIT
        ;
VALIDVAL(TMGENV,TMGWKLD,TMGVALUE,MAP) ;"
        ;"Purpose: To determine IF a lab/result value can be stored of a given lab
        ;"Input: TMGENV, -- PASS BY REFERENCE.  The lab environment array.
        ;"                  TMGENV(<other values.  see SETUPENV^TMGHL7U>)
        ;"                  TMGENV("INTERACTIVE MODE")=1 will allow autofix of problems. 
        ;"       TMGWKLD -- This determines the test to check
        ;"       TMGVALUE -- The is the value to be tested for validity.
        ;"       MAP -- PASS BY REFERENCE.  ARRAY with mapping info. OPTIONAL 
        ;"Results: 1 if OK to store, -1^Message IF problem.
        ;"Also TMGXERR is SET IF error.
        NEW HLP SET HLP=""
        NEW TMGRESULT SET TMGRESULT=1
        SET TMGVALUE=$GET(TMGVALUE)
        IF TMGVALUE="" GOTO VVDN
        SET TMGWKLD=$GET(TMGWKLD)
        IF TMGWKLD="" DO  GOTO VVDN
        . SET TMGRESULT="-1^WKLD code not provided to VALIDVAL.TMGHL72"
        IF $DATA(MAP)=0 SET TMGRESULT=$$GETMAP^TMGHL70B(.TMGENV,TMGWKLD,"R",.MAP)
        IF TMGRESULT'>0 GOTO VVDN
        NEW IEN64 SET IEN64=+$GET(MAP("IEN64"))
        IF IEN64'>0 DO  GOTO VVDN
        . SET TMGRESULT="-1^In VALIDVAL^TMGHL72.  Mapped value for IEN64 not found for '"_TMGWKLD_"'"
        NEW IEN60 SET IEN60=+$GET(MAP("IEN60"))
        IF IEN60'>0 DO  GOTO VVDN
        . SET TMGRESULT="-1^In VALIDVAL^TMGHL72.  Mapped value for IEN60 not found for '"_TMGWKLD_"'"
        NEW STORAGE SET STORAGE=$GET(MAP("STORAGE"))
        NEW FLD63D04 SET FLD63D04=+$PIECE(STORAGE,"^",2)  ;"+$PIECE($GET(^LAB(60,IEN60,.2)),"^",1)
        IF FLD63D04'>0 DO  GOTO VVDN
        . SET TMGRESULT="-1^In VALIDVAL^TMGHL72.  Mapped value for STORAGE not found for '"_TMGWKLD_"'"
        NEW ZNODE SET ZNODE=$GET(^DD(63.04,FLD63D04,0))
VVBADCH IF TMGVALUE["^" DO  GOTO VV2
        . SET HLP="Values can't contain '^' character."
VVSET1  IF $PIECE(ZNODE,"^",2)["S" DO
        . NEW MATCH SET MATCH=0
        . NEW PARTMATCH SET PARTMATCH=0
        . NEW SET SET SET=$PIECE(ZNODE,"^",3)
        . NEW I FOR I=1:1:$LENGTH(SET,";") DO  
        . . NEW OPT SET OPT=$PIECE(SET,";",I)
        . . NEW PCE FOR PCE=1:1:2 DO  
        . . . NEW MATCHVAL SET MATCHVAL=$PIECE(OPT,":",PCE)
        . . . IF (PCE=2),($PIECE(OPT,":",1)=$PIECE(OPT,":",2)) QUIT   ;"e.g. 1+:1+;2+:2+;3+:3+...
        . . . IF TMGVALUE=MATCHVAL SET MATCH=1
        . . . IF $EXTRACT(MATCHVAL,1,$LENGTH(TMGVALUE))=TMGVALUE SET PARTMATCH=PARTMATCH+1
        . IF MATCH=0 DO
        . . SET TMGRESULT=-1
        . . SET HLP="[HELP]:Should be one of these SET options: "_SET
        . ELSE  IF PARTMATCH>1 DO
        . . SET TMGRESULT=-1
        . . SET HLP="[HELP]: Value '"_TMGVALUE_"' matches against more than one option in SET: "_SET_".  Edit DD to fix."
        IF TMGRESULT<0 GOTO VV2
        NEW TMGCODE SET TMGCODE=$PIECE(ZNODE,"^",5,999)
        IF TMGCODE="" GOTO VVDN
        NEW X SET X=TMGVALUE
        DO
        . NEW $ETRAP
        . SET $ETRAP="SET TMGXERR=""Error trapped executing input transform code: '""_TMGCODE_""' "
        . SET $ETRAP=$ETRAP_"$ZSTATUS=""_$ZSTATUS_""; $ECODE=""_$ECODE "
        . SET $ETRAP=$ETRAP_"SET $ETRAP="""",$ECODE="""" "
        . XECUTE TMGCODE
        IF '$DATA(X) GOTO VV1 ;"input transform failed.
        ;"Put extra checks here IF needed ...
        GOTO VVDN
VV1     IF $DATA(TMGXERR) SET TMGRESULT="-1^"_TMGXERR GOTO VVDN
        SET HLP="[HELP]: Fileman --> '"_$GET(^DD(63.04,FLD63D04,3))_"' [CODE]: "_TMGCODE
VV2     SET TMGRESULT="-1^[INVALID-VAL] Lab value: {"_TMGVALUE_"} not acceptable for storage in "
        SET TMGRESULT=TMGRESULT_"database for test {WKLD CODE: "_TMGWKLD_"} "
        SET TMGRESULT=TMGRESULT_"{IEN64: "_IEN64_"} {IEN60: "_IEN60_"} {IEN63.04: "_FLD63D04_"}.  "
        SET TMGRESULT=TMGRESULT_HLP
        ;"SET TMGRESULT="-1^[INVALID-VAL] Lab value: {"_TMGVALUE_"} not acceptable for storage in "
        ;"SET TMGRESULT=TMGRESULT_"database for test {"_TMGWKLD_"}.  "_HLP
VVDN    QUIT TMGRESULT
 ;
SUSPEC  ;"Transform Secimen source
        NEW VACODE SET VACODE=$GET(TMGINFO("VACODE"))
        IF VACODE="" SET TMGXERR="In SUSPEC^TMGHL72: OBR setup code didn't fire to setup TMGINFO(""VACODE"")." GOTO SUSPCDN
        IF $TRANSLATE(TMGVALUE,TMGU(2),"")="" SET TMGVALUE=""
        IF $PIECE(TMGVALUE,TMGU(2),1)="URINE" SET $PIECE(TMGVALUE,TMGU(2),1)="UR"
        NEW HL7SCHEME SET HL7SCHEME="HL70070"
        IF TMGVALUE'="",$PIECE(TMGVALUE,TMGU(2),3)="" SET $PIECE(TMGVALUE,TMGU(2),3)=HL7SCHEME
        NEW DIV SET DIV=$GET(TMGU(1))_$GET(TMGU(2))_$GET(TMGU(3))
        NEW ORDINFO MERGE ORDINFO=TMGHL7MSG(TMGSEGN,"ORDER")
        NEW IEN60 SET IEN60=$GET(ORDINFO("IEN60"))  ;"FILE 60 = LABORATORY TEST
        IF +IEN60'>0 DO  GOTO SUSPCDN
        . SET TMGXERR="IN OBR15^TMGHL73: No value found for IEN60 (Test probably needs to be set up.)"
        NEW IEN64D061 SET IEN64D061=$GET(ORDINFO("SPECIMEN (64.061)"))
        IF IEN64D061'>0 DO  GOTO SUSPCDN
        . SET TMGXERR="In SUSPEC^TMGHL72: No default specimen source IEN 64.061 found for OBR segment, line #"_TMGSEGN
        NEW IEN61 SET IEN61=+$GET(ORDINFO("SPECIMEN (61)"))  ;"FILE 61 = TOPOGRAPHY FIELD, used in SPECIMEN field in LAB DATA (63) file. 
        IF IEN61'>0 DO  GOTO SUSPCDN
        . SET TMGXERR="In SUSPEC^TMGHL72: No default specimen source IEN 61 found for OBR segment, line #"_TMGSEGN
        ;
        NEW ZN SET ZN=$GET(^LAB(64.061,IEN64D061,0))
        NEW CODE SET CODE=$PIECE(ZN,"^",2) IF CODE="" DO  GOTO SUSPCDN
        . SET TMGXERR="In SUSPEC^TMGHL72: No specimen source code found in File 64.061, record #"_IEN64D061
        NEW CODENAME SET CODENAME=$PIECE(ZN,"^",1) IF CODENAME="" SET CODENAME=CODE
        NEW UPCODENAME SET UPCODENAME=$$UP^XLFSTR(CODENAME)
        NEW HL7MSGSRC SET HL7MSGSRC=TMGVALUE IF HL7MSGSRC'="" SET $PIECE(HL7MSGSRC,TMGU(2),4)=IEN61
        NEW EXPECTEDSRC SET EXPECTEDSRC=CODE_TMGU(2)_CODENAME_TMGU(2)_"HL70070"_TMGU(2)_IEN61
        IF DIV'="" SET HL7MSGSRC=$TRANSLATE(HL7MSGSRC,DIV,"")  ;"See IF value is just empty field dividers
        IF HL7MSGSRC="" DO  GOTO SUSPCDN  ;"If not value provided, then use expected default.
        . SET TMGVALUE=EXPECTEDSRC
        NEW ACCEPTHL7SOURCE SET ACCEPTHL7SOURCE=0
        SET ACCEPTHL7SOURCE=1  ;"//kt 9/15/13 -- mod to just take whatever source HL7 message gives...
        IF $PIECE(VACODE,"^",3)["CULTURE" SET ACCEPTHL7SOURCE=1 ;"Hardcoded fix of particular problem.
        IF UPCODENAME="OTHER" SET ACCEPTHL7SOURCE=1 ;"If source is OTHER  then allow any provided source.
        IF UPCODENAME["MISCELLANEOUS" SET ACCEPTHL7SOURCE=1 ;"If expect is MISC. then allow any provided source.
        IF HL7MSGSRC=EXPECTEDSRC SET ACCEPTHL7SOURCE=1 ;"found HL7 value is same as expected, so OK
        IF ACCEPTHL7SOURCE DO  GOTO SUSPCDN
        . ;"Try to match source from HL7 message to entry in 61 (TOPOGRAPHY FILE)
        . NEW DIC SET DIC=61,DIC(0)="M"
        . NEW X,Y SET X=$PIECE(TMGVALUE,TMGU(2),2)
        . DO ^DIC
        . IF Y'>0 DO  QUIT
        . . SET TMGVALUE=EXPECTEDSRC ;"Can't map provided source name, so just use expected value. 
        . NEW SPECNAME SET SPECNAME=$PIECE(Y,"^",2)
        . SET TMGVALUE=SPECNAME_TMGU(2)_SPECNAME_TMGU(2)_HL7SCHEME_TMGU(2)_+Y  ;"Y=IEN60
        ;
        ;"At this point, we have an HL7 message provided that is different than expected. 
        SET TMGXERR="HL7 message has an unexpected value for source in OBR segment #15.  "
        SET TMGXERR=TMGXERR_"Expected: '"_EXPECTEDSRC_"', but got '"_TMGVALUE_"'.  " 
        SET TMGXERR=TMGXERR_"Edit SUSPEC^TMGHL72 code to handle."
SUSPCDN QUIT
        ;
 ;"----------------------------------------------------------------------
ADD2NTE(ARR,TMGHL7MSG,TMGU)   ;" Add lines from ARR as extra NTE segments
        ;"Input: ARR -- PASS BY REFERENCE.  The array to add.  Format:
        ;"   ARR(#)=<line of text>
        ;"      TMGHL7MSG -- the array to store in. PASS BY REFERENCE.
        ;"      TMGU -- The array with divisor chars.
        ;"Results: none
        ;"NOTICE: Some messages have separate message sections.  This just adds
        ;"        after last NTE segment.  
        NEW ARRIDX SET ARRIDX=""
        FOR  SET ARRIDX=$ORDER(ARR(ARRIDX)) QUIT:+ARRIDX'>0  DO
        . NEW LINE SET LINE=$GET(ARR(ARRIDX))
        . NEW NTEIDX SET NTEIDX=$ORDER(TMGHL7MSG("B","NTE",""),-1)
        . NEW NTENUM SET NTENUM=$GET(TMGHL7MSG(NTEIDX,1))
        . NEW NEWSEGN SET NEWSEGN=NTEIDX+1
        . FOR  QUIT:($DATA(TMGHL7MSG(NEWSEGN))=0)!(NEWSEGN=NTEIDX)  SET NEWSEGN=NEWSEGN-0.1
        . IF NEWSEGN=NTEIDX QUIT
        . ;"DO SETPCE("NTE",.TMGHL7MSG,.TMGU,NEWSEGN,1) ;
        . DO SETPCE^TMGHL7X2(NTENUM+1,.TMGHL7MSG,.TMGU,NEWSEGN,1) ;
        . SET $PIECE(TMGHL7MSG(NEWSEGN),TMGU(1),1)="NTE"
        . DO SETPCE^TMGHL7X2(LINE,.TMGHL7MSG,.TMGU,NEWSEGN,3) ;
        . SET $PIECE(TMGHL7MSG(NEWSEGN),TMGU(1),1)="NTE"
        . SET TMGHL7MSG(NEWSEGN,"SEG")="NTE"       
        . SET TMGHL7MSG("B","NTE",NEWSEGN)=""       
        QUIT        
        ;
ADD2ARRI(ARR,LABEL,VALUE)  ;"ADD TO ARRAY IF VALUE IS NOT ""
        IF $$TRIM^XLFSTR(VALUE)="" QUIT
        NEW LINE SET LINE=LABEL_VALUE
        DO ADDTOARR(.ARR,LINE)
        QUIT
ADDTOARR(ARR,LINE)   ;
        NEW IDX SET IDX=$ORDER(ARR(""),-1)
        IF LINE=$$DBLN,$GET(ARR(IDX))=LINE QUIT  ;"avoid duplicated double lines.
        SET IDX=IDX+1
        SET ARR(IDX)=LINE 
        QUIT
        ;
ADDA2ARR(OUT,INARR) ;"ADD ARRY TO OUTPUT ARRAY
        NEW IDX SET IDX=""
        FOR  SET IDX=$ORDER(INARR(IDX)) QUIT:IDX=""  DO
        . DO ADDTOARR(.OUT,$GET(INARR(IDX)))
        QUIT
        ;
ISFINALN(TMGHL7MSG,SEGN)  ;"IS NTE BLOCK AT THE END OF THE MESSAGE?  I.E. NO FOLLOWING SEGMENTS?
        NEW SEGN2 SET SEGN2=$ORDER(TMGHL7MSG(SEGN))
        IF SEGN2'>0 QUIT 1
        IF $GET(TMGHL7MSG(SEGN2,"SEG"))="NTE" QUIT 0
        QUIT 1
        ;
DBLN()  ;"Return ascii text line
        QUIT "===================================================="
        ;        
NUMNTEFO(SEGN)  ;"NUMBER OF NTE SEGMENTS THAT FOLLOW AFTER SEGN
        ;"Input: Uses globally scoped vars: TMGHL7MSG
        NEW TMGRESULT SET TMGRESULT=0
        NEW NEXTSEGN SET NEXTSEGN=SEGN
        NEW DONE SET DONE=0
        FOR  SET NEXTSEGN=+$ORDER(TMGHL7MSG(NEXTSEGN)) QUIT:DONE  DO
        . IF +NEXTSEGN'>0 SET DONE=1 QUIT
        . NEW NEXTSEG SET NEXTSEG=$GET(TMGHL7MSG(NEXTSEGN,"SEG"))
        . IF NEXTSEG'="NTE" SET DONE=1 QUIT
        . SET TMGRESULT=TMGRESULT+1
        QUIT TMGRESULT
        ;
LABLDATA(OUT,TMGHL7MSG,SEG,SEGN)  ;
        ;"Uses globally scoped vars: TMGDD
        NEW ASEG 
        SET SEGN=+$GET(SEGN)
        IF SEGN>0 MERGE ASEG=TMGHL7MSG(SEGN)
        ELSE  DO GETSEG^TMGHL7X2(.ASEG,.TMGHL7MSG,SEG)
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(ASEG(IDX)) QUIT:+IDX'>0  DO
        . NEW VALUE SET VALUE=$GET(ASEG(IDX)) QUIT:VALUE=""
        . NEW FLDNAME SET FLDNAME=$GET(TMGDD(SEG,IDX),"?")
        . SET OUT(FLDNAME)=VALUE
        QUIT
        ;
INSRTNTE(ARR,TMGHL7MSG,TMGU,SEGNPRIOR)   ;"Insert note segment after SEGNPRIOR from ARR
        ;"Input: ARR -- PASS BY REFERENCE.  The array to add.  Format:
        ;"   ARR(#)=<line of text>
        ;"      TMGHL7MSG -- the array to store in. PASS BY REFERENCE.
        ;"      TMGU -- The array with divisor chars.
        ;"      SEGNPRIOR -- The segment number that the NTE array is to be inserted AFTER
        ;"NOTE: If there is a NTE segment directly after SEGNPRIOR, then that NTE
        ;"      block will be appended to.
        ;"Results: none
        NEW NEXTSEGN SET NEXTSEGN=+$ORDER(TMGHL7MSG(SEGNPRIOR))
        NEW SEGNAME SET SEGNAME=$GET(TMGHL7MSG(NEXTSEGN,"SEG"))
        IF SEGNAME="NTE" DO  GOTO INSNDN
        . NEW LASTNTESEGN SET LASTNTESEGN=NEXTSEGN
        . FOR  SET NEXTSEGN=$ORDER(TMGHL7MSG(NEXTSEGN)) QUIT:(+NEXTSEGN'>0)!($GET(TMGHL7MSG(NEXTSEGN,"SEG"))'="NTE")  DO
        . . SET LASTNTESEGN=NEXTSEGN
        . DO APPNDNTE^TMGHL72(.ARR,.TMGHL7MSG,.TMGU,LASTNTESEGN)  ;"APPEND NOTE
        NEW NTENUM SET NTENUM=1
        NEW FIRST SET FIRST=1
        NEW NEWSEGN  
        NEW ARRIDX SET ARRIDX=""
        FOR  SET ARRIDX=$ORDER(ARR(ARRIDX)) QUIT:+ARRIDX'>0  DO
        . NEW LINE SET LINE=$GET(ARR(ARRIDX))
        . IF FIRST SET NEWSEGN=SEGNPRIOR+0.5 SET FIRST=0
        . ELSE  SET NEWSEGN=SEGNPRIOR+0.01
        . DO SETPCE^TMGHL7X2(NTENUM,.TMGHL7MSG,.TMGU,NEWSEGN,1) 
        . SET NTENUM=NTENUM+1 
        . SET $PIECE(TMGHL7MSG(NEWSEGN),TMGU(1),1)="NTE"
        . DO SETPCE^TMGHL7X2(LINE,.TMGHL7MSG,.TMGU,NEWSEGN,3) ;
        . SET $PIECE(TMGHL7MSG(NEWSEGN),TMGU(1),1)="NTE"
        . SET TMGHL7MSG(NEWSEGN,"SEG")="NTE"       
        . SET TMGHL7MSG("B","NTE",NEWSEGN)=""
        . SET SEGNPRIOR=NEWSEGN
INSNDN  QUIT
        ;        
PREFIXNT(LINE,TMGHL7MSG,TMGU,SEGN)  ;"PREFIX NOTE (INSERT LINE BEFORE INDEX LINE)
        ;"Input: LINE -- A SINGLE LINE TO PREFIX
        ;"      TMGHL7MSG -- the array to store in. PASS BY REFERENCE.
        ;"      TMGU -- The array with divisor chars.
        ;"      SEGN -- The segment number of the line in the NTE array to insert before
        ;"  that ARR is to be appended to.
        ;"NOTE: This assumes that inputs are valid and that NTE segment exists
        ;"Results: none
        NEW NTEIDX SET NTEIDX=SEGN
        NEW NTENUM SET NTENUM=$GET(TMGHL7MSG(NTEIDX,1))
        NEW NEWSEGN SET NEWSEGN=NTEIDX
        FOR  SET NEWSEGN=NEWSEGN-0.01,NTENUM=NTENUM-0.01 QUIT:$DATA(TMGHL7MSG(NEWSEGN))=0
        DO SETPCE^TMGHL7X2(NTENUM,.TMGHL7MSG,.TMGU,NEWSEGN,1) 
        SET $PIECE(TMGHL7MSG(NEWSEGN),TMGU(1),1)="NTE"
        DO SETPCE^TMGHL7X2(LINE,.TMGHL7MSG,.TMGU,NEWSEGN,3) ;
        SET $PIECE(TMGHL7MSG(NEWSEGN),TMGU(1),1)="NTE"
        SET TMGHL7MSG(NEWSEGN,"SEG")="NTE"       
        SET TMGHL7MSG("B","NTE",NEWSEGN)=""
        QUIT         
        ;
PREFXNTA(ARR,TMGHL7MSG,TMGU,SEGN)  ;"PREFIX NOTE ARRAY (INSERT ARRAY BEFORE INDEX LINE)
        ;"Input: ARR -- PASS BY REFERENCE.  format: ARR(#)=<line of text>
        ;"      TMGHL7MSG -- the array to store in. PASS BY REFERENCE.
        ;"      TMGU -- The array with divisor chars.
        ;"      SEGN -- The segment number of the line in the NTE array to insert before
        ;"  that ARR is to be appended to.
        ;"NOTE: This assumes that inputs are valid and that NTE segment exists
        ;"Results: none
        NEW NTENUM SET NTENUM=$GET(TMGHL7MSG(SEGN,1))
        NEW PRIORSEGN SET PRIORSEGN=+$ORDER(TMGHL7MSG(SEGN),-1)
        NEW NEWSEGN SET NEWSEGN=PRIORSEGN
        NEW ARRIDX SET ARRIDX=""
        FOR  SET ARRIDX=$ORDER(ARR(ARRIDX)) QUIT:ARRIDX=""  DO
        . NEW LINE SET LINE=$GET(ARR(ARRIDX)) IF LINE="" SET LINE="   "
        . FOR  SET NEWSEGN=NEWSEGN+.01 QUIT:$DATA(TMGHL7MSG(NEWSEGN))=0
        . DO SETPCE^TMGHL7X2(NTENUM,.TMGHL7MSG,.TMGU,NEWSEGN,1)
        . SET $PIECE(TMGHL7MSG(NEWSEGN),TMGU(1),1)="NTE"
        . DO SETPCE^TMGHL7X2(LINE,.TMGHL7MSG,.TMGU,NEWSEGN,3) ;
        . SET $PIECE(TMGHL7MSG(NEWSEGN),TMGU(1),1)="NTE"
        . SET TMGHL7MSG(NEWSEGN,"SEG")="NTE"       
        . SET TMGHL7MSG("B","NTE",NEWSEGN)=""
        QUIT         
        ;
APPNDNTE(ARR,TMGHL7MSG,TMGU,LASTNTESEGN)   ;"APPEND NOTE 
        ;"Input: ARR -- PASS BY REFERENCE.  The array to add.  Format:
        ;"   ARR(#)=<line of text>
        ;"      TMGHL7MSG -- the array to store in. PASS BY REFERENCE.
        ;"      TMGU -- The array with divisor chars.
        ;"      LASTNTESEGN -- The segment number of the **last** line in the NTE array
        ;"  that ARR is to be appended to.
        ;"NOTE: This assumes that inputs are valid and that NTE segment exists
        ;"Results: none
        NEW FIRST SET FIRST=1
        NEW NTEIDX SET NTEIDX=LASTNTESEGN
        NEW ARRIDX SET ARRIDX=""
        FOR  SET ARRIDX=$ORDER(ARR(ARRIDX)) QUIT:+ARRIDX'>0  DO
        . NEW LINE SET LINE=$GET(ARR(ARRIDX))
        . NEW NTENUM SET NTENUM=$GET(TMGHL7MSG(NTEIDX,1))
        . NEW NEWSEGN 
        . IF FIRST SET NEWSEGN=NTEIDX+0.5 SET FIRST=0
        . ELSE  SET NEWSEGN=NTEIDX+0.01
        . FOR  QUIT:($DATA(TMGHL7MSG(NEWSEGN))=0)!(NEWSEGN=NTEIDX)  SET NEWSEGN=NEWSEGN-0.001
        . IF NEWSEGN=NTEIDX QUIT
        . DO SETPCE^TMGHL7X2(NTENUM+1,.TMGHL7MSG,.TMGU,NEWSEGN,1) ;
        . SET $PIECE(TMGHL7MSG(NEWSEGN),TMGU(1),1)="NTE"
        . DO SETPCE^TMGHL7X2(LINE,.TMGHL7MSG,.TMGU,NEWSEGN,3) ;
        . SET $PIECE(TMGHL7MSG(NEWSEGN),TMGU(1),1)="NTE"
        . SET TMGHL7MSG(NEWSEGN,"SEG")="NTE"       
        . SET TMGHL7MSG("B","NTE",NEWSEGN)=""
        . SET NTEIDX=NEWSEGN
        QUIT         
        ;   
KILLDNRS(TMGHL7MSG)  ;"Scan an remove all OBX's with lab value of "DNR", and also associated NTE segs.
        NEW ASEG SET ASEGN=""
        FOR  SET ASEGN=$ORDER(TMGHL7MSG(ASEGN)) QUIT:+ASEGN'>0  DO
        . IF $GET(TMGHL7MSG(ASEGN,"SEG"))'="OBX" QUIT
        . IF $GET(TMGHL7MSG(ASEGN,5))'="DNR" QUIT
        . DO DELSEG(.TMGHL7MSG,ASEGN) 
        . ;"NOW KILL ANY NTE SEGMENTS THAT DIRECTLY FOLLOW THIS OBX
        . NEW SEGN2,DONE SET SEGN2=ASEGN,DONE=0
        . FOR  SET SEGN2=$ORDER(TMGHL7MSG(SEGN2)) QUIT:DONE!(+SEGN2'>0)  DO  
        . . IF $GET(TMGHL7MSG(SEGN2,"SEG"))'="NTE" SET DONE=1 QUIT
        . . IF $$ISFINALN^TMGHL72(.TMGHL7MSG,SEGN2)=1 SET DONE=1 QUIT    
        . . DO DELSEG(.TMGHL7MSG,SEGN2)  
        QUIT
        ;
DELSEG(TMGHL7MSG,SEGN)  ;"DELETE SEGMENT, AND REFERENCES TO IT IN THE CROSS REFERENCES
        KILL TMGHL7MSG(SEGN),TMGHL7MSG("B","OBX",SEGN)   ;"KILL SEGMENT AND B INDEX ENTRY
        NEW APO SET APO=""
        FOR  SET APO=$ORDER(TMGHL7MSG("PO",APO)) QUIT:+APO'>0  DO   ;"KILL PROCESS ORDER ENTRIES
        . IF $GET(TMGHL7MSG("PO",APO))=SEGN KILL TMGHL7MSG("PO",APO)
        QUIT
        ;
GETSEGAR(TMGHL7MSG,SEGN,SEGNAME,OUT) ;
        ;"Return array with all segments matching SEGNAME *following* SEGN, until non-match found
        ;"A utility function for functions below
        NEW ASEGN SET ASEGN=SEGN
        NEW DONE SET DONE=0
        FOR  SET ASEGN=$ORDER(TMGHL7MSG(ASEGN)) QUIT:(ASEGN'>0)!DONE  DO
        . IF $GET(TMGHL7MSG(ASEGN,"SEG"))'=SEGNAME SET DONE=1 QUIT
        . MERGE OUT(ASEGN)=TMGHL7MSG(ASEGN)
        QUIT
        ;
GETOBXAR(TMGHL7MSG,SEGN,OUT) ;"Return array with all "OBX" segments *following* SEGN, until non-OBX found
        ;"A utility function for HNDUPOBX below
        DO GETSEGAR(.TMGHL7MSG,.SEGN,"OBX",.OUT) 
        QUIT
        ;
GETNTARR(TMGHL7MSG,SEGN,OUT) ;"Return array with all "NTE" segments *following* SEGN, until non-NTE found
        ;"A utility function for HNDUPOBX below
        DO GETSEGAR(.TMGHL7MSG,.SEGN,"NTE",.OUT) 
        QUIT
        ;        
SUMOBXAR(OBXAR,OUT)  ;"Summarize OBX Array 
        ;"A utility function for HNDUPOBX below
        ;"INPUT: OBXAR -- OUT array as created by GETOBXAR
        ;"       OUT -- PASS BY REFERENCE, AN OUT PARAMETER
        NEW ASEGN SET ASEGN=""
        FOR  SET ASEGN=$ORDER(OBXAR(ASEGN)) QUIT:ASEGN'>0  DO
        . NEW TEST SET TEST=$GET(OBXAR(ASEGN,3,1))
        . NEW RESULT 
        . DO ADD2RSLT(.RESULT,.OBXAR,ASEGN,5)  ;"OBSERVATION VALUE
        . DO ADD2RSLT(.RESULT,.OBXAR,ASEGN,6)  ;"OBSERVATION UNITS
        . DO ADD2RSLT(.RESULT,.OBXAR,ASEGN,7,"Ref:")  ;"OBSERVATION REF RANGE
        . IF $GET(OBXAR(ASEGN,8))'="N" DO
        . . DO ADD2RSLT(.RESULT,.OBXAR,ASEGN,8,"Flags:")  ;"OBSERVATION ABNORMAL FLAGS
        . DO ADD2RSLT(.RESULT,.OBXAR,ASEGN,9,"Probability:")  ;"OBSERVATION PROBABILITY
        . DO ADD2RSLT(.RESULT,.OBXAR,ASEGN,10,"Nature:") ;"OBSERVATION NATURE OF ABNORMAL TEST
        . ;"DO ADD2RSLT(.RESULT,.OBXAR,ASEGN,11,"Status:") ;"OBSERVATION STATUS
        . SET OUT(ASEGN)=TEST_"^"_RESULT
        . SET OUT("B",TEST,ASEGN)=RESULT
        . NEW CT SET CT=$GET(OUT("COUNT",TEST))+1
        . SET OUT("COUNT",TEST)=CT
        . IF CT>1 SET OUT("HAS DUP")=1        
        QUIT
        ;
ADD2RSLT(RESULT,ARR,SEGN,FLD,PREFIX) ;
        ;"A utility function for HNDUPOBX below
        SET RESULT=$GET(RESULT)
        NEW TMP SET TMP=$GET(ARR(ASEGN,FLD))
        SET TMP=$$REPLSTR^TMGSTUT3(TMP,"^"," / ")
        IF TMP'="",RESULT'="" DO
        . SET RESULT=RESULT_" "_$GET(PREFIX)
        SET RESULT=RESULT_TMP
        QUIT
        ;
HNDUPOBX(TMGHL7MSG,SEGN,TMGU) ;"Handle situation with an OBR having duplicate OBX's
        ;"NOTE: This code is to handle situation were a given OBR has multiple OBX's
        ;"      with differening results for the same test name.  For example, Laughlin
        ;"      was putting comment-style results into multiple UCULT test result OBX's.
        ;"      Without this code, only the last value was being stored, and the
        ;"      others were being overwritten.  
        NEW TMP DO GETOBXAR(.TMGHL7MSG,SEGN,.TMP)
        NEW ARR DO SUMOBXAR(.TMP,.ARR)
        IF $GET(ARR("HAS DUP"))'=1 QUIT
        NEW NOTE,CHANGE,CIDX SET CIDX=0
        NEW TEST SET TEST=""
        FOR  SET TEST=$ORDER(ARR("B",TEST)) QUIT:TEST=""  DO
        . IF $GET(ARR("COUNT",TEST))'>1 QUIT
        . NEW NIDX,ASEG SET (NIDX,ASEG)=0
        . NEW FIRSTLINE SET FIRSTLINE=1
        . FOR  SET ASEG=$ORDER(ARR("B",TEST,ASEG)) QUIT:ASEG'>0  DO
        . . SET NIDX=NIDX+1,NOTE(TEST,NIDX)=$GET(ARR("B",TEST,ASEG))
        . . NEW ACTION
        . . IF FIRSTLINE DO
        . . . SET FIRSTLINE=0
        . . . SET ACTION=ASEG_"^CHANGE^See Comment Below:"
        . . . SET NOTE(TEST,"INSERT")=ASEG
        . . ELSE  DO
        . . . SET ACTION=ASEG_"^KILL"
        . . SET CIDX=CIDX+1,CHANGE(CIDX)=ACTION
        ;"NOW EFFECT CHANGES
        SET CIDX=0
        FOR  SET CIDX=$ORDER(CHANGE(CIDX)) QUIT:CIDX'>0  DO
        . NEW STR SET STR=$GET(CHANGE(CIDX))
        . NEW ASEG SET ASEG=+STR QUIT:ASEG'>0
        . NEW CMD SET CMD=$PIECE(STR,"^",2) 
        . IF CMD="KILL" DO
        . . KILL TMGHL7MSG("B","OBX",ASEG)
        . . KILL TMGHL7MSG("PO",ASEG)
        . . KILL TMGHL7MSG("ORDER",ASEG)
        . . KILL TMGHL7MSG(ASEG)
        . IF CMD="CHANGE" DO
        . . NEW VALUE SET VALUE=$PIECE(STR,"^",3)
        . . DO SETPCE^TMGHL7X2(VALUE,.TMGHL7MSG,.TMGU,ASEG,5)
        SET TEST=""
        FOR  SET TEST=$ORDER(NOTE(TEST)) QUIT:TEST=""  DO
        . NEW ASEG SET ASEG=NOTE(TEST,"INSERT") KILL NOTE(TEST,"INSERT")
        . NEW TEMPNOTE MERGE TEMPNOTE=NOTE(TEST)
        . IF ASEG>0 DO INSRTNTE(.TEMPNOTE,.TMGHL7MSG,.TMGU,ASEG)   ;"Insert note segment after SEGNPRIOR from ARR
        QUIT
        ;
SUMOBXSTA(TMGHL7MSG,SEGN,OUT) ;"Get net status from OBX's after an OBR
        ;"Input: TMGHL7MSG -- the standard array holding parsed message
        ;"       SEGN -- this should be the  SEG# of the OBR segment
        ;"       OUT -- PASS BY REFERENCE, AN OUT PARAMETER.  Format:
        ;"         OUT(1)="Result Status: xxxxxxxxx"
        ;"         OUT(#)=<more text if needed>
        ;"Output: Returns string summarizing status.
        NEW OBXSEGN SET OBXSEGN=$ORDER(TMGHL7MSG("B","OBX",SEGN))-0.001        
        NEW OBXARR DO GETOBXAR(.TMGHL7MSG,OBXSEGN,.OBXARR)
        NEW IDX SET IDX=""
        FOR  SET IDX=$ORDER(OBXARR(IDX)) QUIT:IDX'>0  DO
        . NEW STATUS SET STATUS=OBXARR(IDX,11)
        . IF STATUS="F" SET STATUS="FINAL"
        . IF STATUS="I" SET STATUS="INCOMPLETE/PRELIMINARY"
        . IF STATUS="C" SET STATUS="CORRECTED"
        . IF STATUS="P" SET STATUS="PRELIMINARY"
        . IF STATUS="X" SET STATUS="TEST CANCELED"
        . IF $DATA(OBXARR("STATUS",STATUS))=0 DO
        . . SET OBXARR("STATUS","@COUNT")=$GET(OBXARR("STATUS","@COUNT"))+1
        . SET OBXARR("STATUS",STATUS,IDX)=""
        . SET OBXARR("STATUS",STATUS,"COUNT")=$GET(OBXARR("STATUS",STATUS,"COUNT"))+1
        . KILL OBXARR(IDX)
        NEW NETSTATUS SET NETSTATUS="(none)"
        IF $GET(OBXARR("STATUS","@COUNT"))=1 DO
        . SET OUT(1)="Result Status: "_$ORDER(OBXARR("STATUS","@COUNT"))
        ELSE  DO
        . SET OUT(1)="Result Status: (multiple)",IDX=1
        . NEW ASTATUS SET ASTATUS="@COUNT"
        . FOR  SET ASTATUS=$ORDER(OBXARR("STATUS",ASTATUS)) QUIT:ASTATUS=""  DO
        . . NEW CT SET CT=+$GET(OBXARR("STATUS",ASTATUS,"COUNT")) QUIT:CT'>0
        . . NEW STR SET STR=CT_" Test"_$SELECT(CT>1:"s",1:"")_" -- "_ASTATUS
        . . SET IDX=IDX+1,OUT(IDX)=STR
        QUIT
        ;
SUMNTARR(TMGHL7MSG,SEGN,OUT) ;"Convert NTE's after SEGN into simple ARR(#)=Text array
        KILL OUT 
        NEW TMP DO GETNTARR(.TMGHL7MSG,.SEGN,.TMP)
        NEW IDX SET IDX=""
        FOR  SET IDX=$ORDER(TMP(IDX)) QUIT:IDX'>0  DO
        . SET OUT(IDX)=$GET(TMGHL7MSG(IDX,3))   
        QUIT
CHKOBRNT(TMGHL7MSG,SEGN,OBRCOMMENTS) ;"CHECK / Handle NTE's that follow OBR (i.e. order comments)
        ;"This will remove NTE's following an OBR, and return them in simple array
        ;"   for use with adding to other order comments (called from OBRDN)  
        KILL OBRCOMMENTS DO SUMNTARR(.TMGHL7MSG,.SEGN,.OBRCOMMENTS) 
        IF $DATA(OBRCOMMENTS) DO  ;"INDEX IN OBRCOMMENTS MATCHES THAT IN TMGHL7MSG
        . NEW IDX SET IDX=0 FOR  SET IDX=$ORDER(OBRCOMMENTS(IDX)) QUIT:IDX'>0  DO
        . . SET OBRCOMMENTS(IDX)="   "_$GET(OBRCOMMENTS(IDX))
        . . KILL TMGHL7MSG(IDX),TMGHL7MSG("B","NTE",IDX),TMGHL7MSG("PO",IDX)
        . SET OBRCOMMENTS(.5)="Order comments:"
        QUIT