TMGHL73 ;TMG/kst-HL7 transformation engine processing ;2/18/14
              ;;1.0;TMG-LIB;**1**;06/23/13
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
 ;"NOTE: this is code for working with labs from **[Pathgroup lab]**
 ;"      FYI -- Laughlin LAB code is in TMGHL74
 ;"             Laughlin RADIOLOGY is in TMGHL74R
 ;"               common code is in TMGHL72
 ;"=======================================================================
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"TEST  -- Pick file and manually send through filing process.   
 ;"BATCH -- Launch processing through all files in folder for PathGroup lab
 ;"HL7FIN(FNAME)  -- File input HL7 message files from PATHGROUP into POC lab filer
 ;"HL7IN -- Entry point, that could be  called from LA7V Process Results from PathGroup.
 ;"HL7MSGIN(TMGMSG) -- Entry point to process message, stored in TMGMSG
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
        ;"DO TEST^TMGHL72("/mnt/WinServer/PathgroupHL7")
        DO TEST^TMGHL71("/mnt/WinServer/PathgroupHL7")
        QUIT
        ; 
BATCH   ;"Launch processing through all files in folder.
        NEW DIR SET DIR="/mnt/WinServer/PathgroupHL7"
        ;"DO HLDIRIN^TMGHL72(DIR,1000,10)
        DO HLDIRIN^TMGHL71(DIR,1000,10)
        QUIT
        ;
HLDIRIN(DIRNAME,COUNT,MAXERRCT,DONEPATH) ;"DEPRECIATED
        ;"DO HLDIRIN^TMGHL72(.DIRNAME,.COUNT,.MAXERRCT,.DONEPATH) 
        DO HLDIRIN^TMGHL71(.DIRNAME,.COUNT,.MAXERRCT,.DONEPATH) 
        QUIT
        ;        
HL7FIN(FNAME,NOALERT,DONEPATH)  ;"DEPRECIATED.  POC file input HL7 message files from PATHGROUP
        ;"QUIT $$HL7FIN^TMGHL72(.FNAME,.NOALERT,.DONEPATH) 
        QUIT $$HL7FIN^TMGHL71(.FNAME,.NOALERT,.DONEPATH) 
        ;
HL7IN(NOALERT)  ;"Purpose: Entry point, that could be  called from LA7V Process Results from PathGroup.
        ;"QUIT $$HL7IN^TMGHL72(.NOALERT)
        QUIT $$HL7IN^TMGHL71(.NOALERT)
        ;        
HL7MSGIN(TMGMSG,NOALERT,OPTION) ;
        ;"QUIT $$HL7MSGIN^TMGHL72(.TMGMSG,.NOALERT,.OPTION)
        QUIT $$HL7MSGIN^TMGHL72(.TMGMSG,.NOALERT,.OPTION)
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
MSH3    ;"Purpose: Process MSH segment, FLD 4 (Sending Application)
        ;"SET TMGVALUE="LA7V HOST PG"
        QUIT
        
MSH4    ;"Purpose: Process MSH segment, FLD 4 (Sending Facility)
        SET TMGVALUE="PATHGROUP"
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
ORC2    ;"Purpose: Process ORC message, field 2
        DO ORC2^TMGHL72
        QUIT
ORC12  ;"Purpose: Process empty ORC message, field 12
        DO XORC12^TMGHL72
        QUIT
ORC13  ;"Purpose: Process empty ORC message, field 13
        DO XORC13^TMGHL72
        ;"SET $PIECE(TMGVALUE,"^",1)="Laughlin_Office"
        SET $PIECE(TMGVALUE,"^",1)="Family Phys Of Greeneville"
        SET $PIECE(TMGVALUE,"^",2)="69" 
        QUIT
        ;
        ;
OBR     ;"Purppse: setup for OBR fields.
        DO OBR^TMGHL72
        QUIT
OBR4    ;"Purpose: To transform the OBR segment, field 4
        DO OBR4^TMGHL72
        QUIT
OBR15   ;"Transform Secimen source
        NEW VACODE SET VACODE=$GET(TMGINFO("VACODE"))
        IF VACODE="" SET TMGXERR="In OBR15.TMGHL73: OBR setup code didn't fire to setup TMGINFO(""VACODE"")." GOTO OBR15DN
        IF $TRANSLATE(TMGVALUE,TMGU(2),"")="" SET TMGVALUE=""
        IF $PIECE(TMGVALUE,TMGU(2),1)="URINE" SET $PIECE(TMGVALUE,TMGU(2),1)="UR"
        NEW HL7SCHEME SET HL7SCHEME="HL70070"
        IF TMGVALUE'="",$PIECE(TMGVALUE,TMGU(2),3)="" SET $PIECE(TMGVALUE,TMGU(2),3)=HL7SCHEME
        NEW DIV SET DIV=$GET(TMGU(1))_$GET(TMGU(2))_$GET(TMGU(3))
        NEW ORDINFO MERGE ORDINFO=TMGHL7MSG(TMGSEGN,"ORDER")
        NEW IEN60 SET IEN60=$GET(ORDINFO("IEN60"))  ;"FILE 60 = LABORATORY TEST
        IF +IEN60'>0 DO  GOTO OBR15DN
        . SET TMGXERR="IN OBR15^TMGHL73: No value found for IEN60 (Test probably needs to be set up.)"
        NEW IEN64D061 SET IEN64D061=$GET(ORDINFO("SPECIMEN (64.061)"))
        IF IEN64D061'>0 DO  GOTO OBR15DN
        . SET TMGXERR="In OBR15.TMGHL73: No default specimen source IEN 64.061 found for OBR segment, line #"_TMGSEGN
        NEW IEN61 SET IEN61=+$GET(ORDINFO("SPECIMEN (61)"))  ;"FILE 61 = TOPOGRAPHY FIELD, used in SPECIMEN field in LAB DATA (63) file. 
        IF IEN61'>0 DO  GOTO OBR15DN
        . SET TMGXERR="In OBR15.TMGHL73: No default specimen source IEN 61 found for OBR segment, line #"_TMGSEGN
        ;
        NEW ZN SET ZN=$GET(^LAB(64.061,IEN64D061,0))
        NEW CODE SET CODE=$PIECE(ZN,"^",2) IF CODE="" DO  GOTO OBR15DN
        . SET TMGXERR="In OBR15.TMGHL73: No specimen source code found in File 64.061, record #"_IEN64D061
        NEW CODENAME SET CODENAME=$PIECE(ZN,"^",1) IF CODENAME="" SET CODENAME=CODE
        NEW UPCODENAME SET UPCODENAME=$$UP^XLFSTR(CODENAME)
        NEW HL7MSGSRC SET HL7MSGSRC=TMGVALUE IF HL7MSGSRC'="" SET $PIECE(HL7MSGSRC,TMGU(2),4)=IEN61
        NEW EXPECTEDSRC SET EXPECTEDSRC=CODE_TMGU(2)_CODENAME_TMGU(2)_"HL70070"_TMGU(2)_IEN61
        IF DIV'="" SET HL7MSGSRC=$TRANSLATE(HL7MSGSRC,DIV,"")  ;"See IF value is just empty field dividers
        IF HL7MSGSRC="" DO  GOTO OBR15DN  ;"If not value provided, then use expected default.
        . SET TMGVALUE=EXPECTEDSRC
        NEW ACCEPTHL7SOURCE SET ACCEPTHL7SOURCE=0
        SET ACCEPTHL7SOURCE=1  ;"//kt 9/15/13 -- mod to just take whatever source HL7 message gives...
        IF $PIECE(VACODE,"^",3)["CULTURE" SET ACCEPTHL7SOURCE=1 ;"Hardcoded fix of particular problem.
        IF UPCODENAME="OTHER" SET ACCEPTHL7SOURCE=1 ;"If source is OTHER  then allow any provided source.
        IF UPCODENAME["MISCELLANEOUS" SET ACCEPTHL7SOURCE=1 ;"If expect is MISC. then allow any provided source.
        IF HL7MSGSRC=EXPECTEDSRC SET ACCEPTHL7SOURCE=1 ;"found HL7 value is same as expected, so OK
        IF ACCEPTHL7SOURCE DO  GOTO OBR15DN
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
        SET TMGXERR=TMGXERR_"Edit OBR15^TMGHL73 code to handle."
OBR15DN QUIT
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
OBX15   ;"Purpose: To transform the OBX segment, field 15 ---- Producer's ID
        DO OBX15^TMGHL72
        QUIT
OBX16   ;"Purpose: To transform the OBX segment, field 16 ---- Responsibile Observer
        ;"Input: Uses globally scoped vars: TMGHL7MSG, TMGU, TMGVALUE, IEN62D4,TMGINFO
        DO OBX16^TMGHL72
        QUIT
OBX18   ;"Purpose: To transform the OBX segment, field 18 ---- Equipment Identifier (EI)
        DO OBX18^TMGHL72
        QUIT
        ;
        ;
NTE3    ;"Purpose: To transform the NTE segment, field 3 (the comments)
        DO NTE3^TMGHL72
        QUIT
        ;
        ;
SUPROV  ;"Purpose: Setup TMGINFO("PROV") -- Ordering provider.
        ;"Input: Uses globally scoped vars: TMGHL7MSG, TMGU, HLREC
        ;"Output: Sets globally scoped variable TMGINFO("PROV")
        ;"Results: None.  TMGXERR SET IF error
        DO SUPROV^TMGHL72
        QUIT                    
        ;"//kt redirected code to one common function on 1/16/15
        ;" NEW PROV SET PROV=$$GETPCE^TMGHL7X2(.TMGHL7MSG,"OBR",16)
        ;" IF $$UP^XLFSTR(PROV)'["TOPPENBERG" SET PROV="^Doctor^Unspecified^"
        ;" IF PROV="" DO  GOTO SPVDN
        ;" . SET TMGXERR="In SUPROV.TMGHL73: Ordering provider not provided in field #16 or 'OBR' segment in HL7 message"
        ;" NEW LNAME,FNAME,MNAME
        ;" SET LNAME=$$UP^XLFSTR($PIECE(PROV,TMGU(2),2))
        ;" SET FNAME=$$UP^XLFSTR($PIECE(PROV,TMGU(2),3))
        ;" SET MNAME=$$UP^XLFSTR($PIECE(PROV,TMGU(2),4))
        ;" IF (LNAME="TOPPENBERG")&(FNAME="EE") SET FNAME="MARCIA"  
        ;" NEW NAME SET NAME=LNAME_","_FNAME_" "_MNAME
        ;" SET NAME=$$TRIM^XLFSTR(NAME)
        ;" NEW DIC,X,Y
        ;" SET DIC=200,DIC(0)="M"
        ;" SET X=NAME
        ;" DO ^DIC
        ;" IF Y'>0 DO  GOTO SPVDN
        ;" . SET TMGXERR="In SUPROV.TMGHL73: Unable find provider in lookup: '"_NAME_"'"
        ;" SET PROV=+Y_TMGU(2)_LNAME_TMGU(2)_FNAME
        ;" SET TMGINFO("PROV")=PROV
SPVDN   QUIT
        ;
SUORL   ;"Purpose: Setup TMGINFO("ORL") and TMGINFO("LOC") -- Ordering locations
        ;"   also TMGINFO("INSTNAME")
        ;"Results: None.  TMGXERR SET IF error
        DO SUORL^TMGHL72
        QUIT
        ;

