TMGHL72 ;TMG/kst-HL7 transformation engine processing ;6/23/16, 8/9/16, 5/9/17
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
 ;"NOTE: This is code for working with labs from both Laughlin lab and Pathgroup Labs
 ;"      i.e. this is common code, usable by multiple labs. 
 ;"      FYI -- Pathgroup code is in TMGHL73
 ;"             Laughlin RADIOLOGY is in TMGHL74R
 ;"             Laughlin LAB code is in TMGHL74
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"TEST(DEFPATH) --Pick file and manually send through filing process.
 ;"HLDIRIN(DIRNAME,COUNT,MAXERRCT,DONEPATH) -- Import files from a directory
 ;"HL7FIN(FNAME,NOALERT,DONEPATH) -- Entry point for processing HL7 files loaded from HFS
 ;"HL7IN(NOALERT) -- Entry point, that could be  called from HL7 lab import engine
 ;"HL7MSGIN(TMGMSG,NOALERT,OPTION) -- Entry point to process message, stored in TMGMSG

 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;"<Many call-back hooks for transformation engine>
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"TMGSTUTL, all the HL*, LA* code that the HL processing path normally calls.
 ;"=======================================================================
 ; 
 ;
 ;//NOTE: 8/2016 -- some common code moved to TMGHL71
 ;
 ;"TEST(DEFPATH)  ;"Pick file and manually send through filing process.
 ;"        SET DEFPATH=$GET(DEFPATH,"/")
 ;"        NEW OPTION SET OPTION("PATH")=DEFPATH
 ;"        NEW FNAME,%,TMGRESULT 
 ;"TSTL1   SET FNAME=$$FBROWSE^TMGIOUT2(.OPTION)
 ;"        IF FNAME="" GOTO TSTDN
 ;"        SET TMGRESULT=$$HL7FIN^TMGHL72(FNAME,1)  ;"1=NO ALERT
 ;"        IF TMGRESULT'>0 DO
 ;"        . WRITE "Filing that HL7 message created an error.  Alert should have been created.",!
 ;"        . WRITE "Message: ",$PIECE(TMGRESULT,"^",2,99),!
 ;"        ELSE  DO
 ;"        . WRITE "HL7 message was successfully processed.",!
 ;"        SET %=1
 ;"        WRITE !,"Pick another HL7 file to process" DO YN^DICN WRITE !
 ;"        IF %=1 GOTO TSTL1
 ;"TSTDN   QUIT
 ;"        ;
 ;"TEST2   ;"Test one particular stored message (MUST BE EDITED TO BE USED)
 ;"        NEW HLMTIEN SET HLMTIEN=543
 ;"        NEW HLMTIENS SET HLMTIENS=544
 ;"        NEW HLREC
 ;"        WRITE $$HL7IN^TMGHL72(1)  ;"1=no alert. 
 ;"        QUIT
 ;"        ;
 ;"HLDIRIN(DIRNAME,COUNT,MAXERRCT,DONEPATH,EXT) ;
 ;"        ;"Purpose: Import files from a directory
 ;"        ;"Input: DIRNAME -- the directory to read files from 
 ;"        ;"       COUNT -- OPTIONAL.  Default=999999
 ;"        ;"       MAXERRCT -- OPTIONAL.  Default=999999.  Will stop when this number of errors encountered 
 ;"        ;"       DONEPATH -- Optional.  If provided, then specifies root of
 ;"        ;"                  folder to moved completed messages.  NOTE: there must
 ;"        ;"                  be subfolders ./Processed  and ./Failed_Messages defined.
 ;"        ;"       EXT -- OPTIONAL.  Default = '.txt'.  
 ;"        ;"Result: none
 ;"        NEW FILELIST
 ;"        SET COUNT=+$GET(COUNT,999999)
 ;"        SET MAXERRCT=+$GET(MAXERRCT,999999)
 ;"        SET EXT=$$TRIM^XLFSTR($GET(EXT,".txt"))
 ;"        IF $EXTRACT(EXT,1)'="." SET EXT="."_EXT
 ;"        SET EXT="*"_EXT
 ;"        NEW SRCH SET SRCH(EXT)="",SRCH($$UP^XLFSTR(EXT))="",SRCH($$LOW^XLFSTR(EXT))=""
 ;"        NEW TMGRESULT SET TMGRESULT=$$LIST^%ZISH(DIRNAME,"SRCH","FILELIST")
 ;"        IF TMGRESULT=0 GOTO HLDDN
 ;"        NEW FNAME SET FNAME=""
 ;"        FOR  SET FNAME=$ORDER(FILELIST(FNAME)) QUIT:(FNAME="")!(COUNT'>0)!(MAXERRCT'>0)  DO
 ;"        . WRITE FNAME," --> "
 ;"        . SET TMGRESULT=$$HL7FIN(DIRNAME_"/"_FNAME,0,.DONEPATH)
 ;"        . IF TMGRESULT=1 WRITE "Processed OK.",!
 ;"        . ELSE  DO
 ;"        . . WRITE "ALERT",!
 ;"        . . WRITE "  ",$PIECE(TMGRESULT,"^",2,99),!
 ;"        . . SET MAXERRCT=MAXERRCT-1
 ;"        . SET COUNT=COUNT-1
 ;"HLDDN   QUIT        
 ;"        ; 
 ;"HL7FIN(FNAME,NOALERT,DONEPATH)  ;"POC file input HL7 message files from lab
 ;"        ;"Purpose: Entry point for processing HL7 files loaded from HFS
 ;"        ;"Input: FNAME -- full filepathname of HL7 file to load and file.  
 ;"        ;"       NOALERT -- Optional.  If 1 then no alert made.
 ;"        ;"       DONEPATH -- Optional.  If provided, then specifies root of
 ;"        ;"                  folder to moved completed messages.  NOTE: there must
 ;"        ;"                  be subfolders ./Processed  and ./Failed_Messages defined.
 ;"        ;"Results: 1 if OK, or -1^Message IF error
 ;"        NEW HLREC,HL,HLQUIT,HLNODE,HLNEXT,HLHDRO
 ;"        NEW TMGRESULT
 ;"        NEW HLMTIEN SET HLMTIEN=0
 ;"        NEW HLMTIENS SET HLMTIENS=0
 ;"        SET TMGRESULT=$$MKHL7MSG^TMGHL7U2(FNAME,.HLMTIEN,.HLMTIENS) ;"MAKE HL7 MESSAGE 
 ;"        IF TMGRESULT<0 DO  GOTO FHL2DN
 ;"        . DO SETALRT2^TMGHL7E(TMGRESULT)
 ;"        SET TMGRESULT=$$HL7IN(.NOALERT)
 ;"        IF +TMGRESULT=1 SET TMGRESULT=$$KLHL7MSG^TMGHL7U2(HLMTIENS)  ;"kills records in 773 and linked 772
 ;"FHL2DN  NEW OUTNAME,OUTPATH
 ;"        DO SPLITFPN^TMGIOUTL(FNAME,.OUTPATH,.OUTNAME,"/")
 ;"        IF $GET(DONEPATH)'="" SET OUTPATH=DONEPATH
 ;"        IF $EXTRACT(OUTPATH,$LENGTH(OUTPATH))'="/" SET OUTPATH=OUTPATH_"/"
 ;"        NEW DESTFOLDER SET DESTFOLDER=$SELECT(TMGRESULT>0:"Processed/",1:"Failed_Messages/")
 ;"        SET OUTPATH=OUTPATH_DESTFOLDER
 ;"        IF $$MOVE^TMGKERNL(FNAME,OUTPATH_OUTNAME)>0 DO  
 ;"        . NEW TEMP SET TEMP="Unable to move file '"_OUTNAME_"' to folder '"_OUTPATH_"'"
 ;"        . IF +TMGRESULT=1 SET TMGRESULT="-1^"_TEMP
 ;"        . ELSE  SET TMGRESULT=TMGRESULT_" AND ALSO "_TEMP
 ;"        QUIT TMGRESULT
 ;"        ;
 ;"HL7IN(NOALERT)  ;"Purpose: Entry point, that could be  called from LA7V Process Results from PathGroup.
 ;"        ;"Input:  NOALERT -- Optional.  If 1 then no alert made. 
 ;"        ;"  ALSO -- Several globally-scoped variables are  used:
 ;"        ;"        HLMTIEN -- An IEN in 772     ??
 ;"        ;"        HLMTIENS -- in IEN in 773    ??
 ;"        ;"        HLREC                        ??
 ;"        ;"Results: 1 if OK, or -1^Message IF error
 ;"        NEW TMGHL7MSG,MSGSTORE,TMGRESULT,IEN62D4,IEN22720
 ;"        NEW TMGHL7DEBUG SET TMGHL7DEBUG=0
 ;"        NEW TMGMSG,TMGENV
 ;"        NEW IEN772,IEN773,TMGHLZZZ
 ;"        KILL ^TMG("TMP","TMGHL71") ;"kill message log for all processes.
 ;"        SET TMGRESULT=$$SETFMENV^TMGHL7U()  ;"SETUP FILE MESSAGE ENVIRONMENT. 
 ;"        IF TMGRESULT<0 GOTO HLIERR
 ;"        SET TMGRESULT=$$FROM772^TMGHL7U2(IEN772,IEN773,.TMGMSG)
 ;"        IF TMGRESULT<0 GOTO HLIERR        
 ;"        SET TMGRESULT=$$HL7MSGIN(.TMGMSG,.NOALERT)
 ;"        GOTO H7IN2DN
 ;"HLIERR IF TMGRESULT<0 DO  GOTO H7IN2DN
 ;"       . DO SETALRT2^TMGHL7E(TMGRESULT)
 ;"H7IN2DN QUIT TMGRESULT                        
 ;"        ;  
 ;"HL7MSGIN(TMGMSG,NOALERT,OPTION) ;
 ;"        ;"Purpose: Entry point to process message, stored in TMGMSG
 ;"        ;"Input: TMGMSG -- PASS BY REFERENCE.  HL7 message to process.  Format
 ;"        ;"          TMGMSG(#)=<line of HL7 message>
 ;"        ;"       NOALERT -- OPTIONAL.  If 1 then no alert is created for errors
 ;"        ;"          NOTE: if OPTION("GUI")=1, then this value is forced to 1.
 ;"        ;"       OPTION -- OPTIONAL.  PASS BY REFERENCE. See HL7PARSE for description
 ;"        ;"Results: 1 if OK, or -1^Message if error
 ;"        NEW TMGHL7MSG,TMGU,TMGENV
 ;"        NEW TMGRESULT SET TMGRESULT=1
 ;"        SET NOALERT=+$GET(NOALERT)
 ;"        IF +$GET(OPTION("GUI")) SET NOALERT=1
 ;"        SET TMGRESULT=$$HL7PARSE(.TMGHL7MSG,.TMGENV,.TMGMSG,.OPTION)        
 ;"        IF TMGRESULT<0 GOTO HLI3ERR
 ;"        IF +$GET(OPTION("GUI","NO FILE"))=1 GOTO H7IN3DN
 ;"        SET TMGRESULT=$$FILEMSG^TMGLRW01(.TMGENV,.TMGHL7MSG)
 ;"HLI3ERR IF (TMGRESULT<0),(NOALERT'=1) DO SETALRT2^TMGHL7E(TMGRESULT)
 ;"H7IN3DN QUIT TMGRESULT
 ;"        ;
 ;"HL7PARSE(TMGHL7MSG,TMGENV,TMGMSG,OPTION) ;
 ;"        ;"Purpose: Entry point to process message, stored in TMGMSG
 ;"        ;"Input: TMGHL7MSG -- PASS BY REFERENCE, AN OUT PARAMETER.
 ;"        ;"       TMGENV -- PASS BY REFERENCE, AN OUT PARAMETER.
 ;"        ;"       TMGMSG -- PASS BY REFERENCE.  HL7 message to process.  Format
 ;"        ;"          TMGMSG(#)=<line of HL7 message>
 ;"        ;"       OPTION -- OPTIONAL.  PASS BY REFERENCE. Format:
 ;"        ;"          OPTION("GUI")=1  <-- signal that GUI is driving this code
 ;"        ;"          OPTION("GUI","NO FILE")=1  <-- signal to just test parsing, don't actually file. 
 ;"        ;"          OPTION("GUI","MSG",#)=<message from server code to GUI> <-- OUT PARAMETER
 ;"        ;"          OPTION("GUI","MSG",#,"REPLY")=<reply from GUI to server code>   <-- IN PARAMETER
 ;"        ;"Results: 1 if OK, or -1^Message if error
 ;"        NEW IEN62D4,IEN22720,FS,ECH
 ;"        NEW TMGRESULT SET TMGRESULT=1
 ;"        SET NOALERT=+$GET(NOALERT)
 ;"        KILL ^TMG("TMP","TMGHL71") ;"kill message log for all processes.
 ;"        SET TMGRESULT=$$SETUPENV^TMGHL7U(.TMGMSG,.TMGENV,0)
 ;"        IF TMGRESULT<0 GOTO PARSDN
 ;"        IF +$GET(OPTION("GUI")) DO
 ;"        . SET NOALERT=1
 ;"        . MERGE TMGENV("GUI")=OPTION("GUI")
 ;"        . SET TMGENV("INTERACTIVE MODE")=1
 ;"        SET IEN62D4=TMGENV("IEN 62.4")  ;"I think this is used in global scope during transformation.
 ;"        SET IEN22720=TMGENV("IEN 22720")
 ;"        SET TMGRESULT=$$PARSMSG2^TMGHL7X2(.TMGENV,.TMGMSG,.TMGHL7MSG) ;
 ;"        IF TMGRESULT<0 GOTO PARSDN
 ;"        SET TMGRESULT=$$XFMSG^TMGHL7X(.TMGENV,.TMGHL7MSG)
 ;"PARSDN  KILL OPTION("GUI") MERGE OPTION("GUI")=TMGENV("GUI")
 ;"        QUIT TMGRESULT
 ;"        ;   
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
        IF $GET(IEN62D4)>0 GOTO XM2
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
XM2     IF $DATA(TMGU)=0 DO  GOTO XMDN
        . SET TMGXERR="In XMSG.TMGHL72: Array with divisor chars (TMGU) not SET up."
        ;
        ;"Place empty ORC segment in, IF not already present
        NEW SEGN SET SEGN=+$ORDER(TMGHL7MSG("B","ORC",0))
        IF SEGN>0 GOTO XMDN
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
XMDN    QUIT
        ;
XMSG2   ;
        KILL IEN62D4,TMGINFO
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
        . NEW LNAME SET LNAME=$PIECE(NAME,"^",1)        
        . NEW FNAME SET FNAME=$PIECE(NAME,"^",2)
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
        ;        
SUPROV  ;"Purpose: Setup TMGINFO("PROV") -- Ordering provider.
        ;"Input: Uses globally scoped vars: TMGHL7MSG, TMGU, HLREC
        ;"Output: Sets globally scoped variable TMGINFO("PROV")
        ;"Results: None.  TMGXERR SET IF error
        NEW PROV SET PROV=$$GETPCE^TMGHL7X2(.TMGHL7MSG,"OBR",16)
        IF $PIECE(PROV,TMGU(2),13)="NPI" DO
        . NEW NPI,ADUZ
        . SET NPI=$PIECE(PROV,TMGU(2),1)
        . IF NPI="" DO  QUIT
        . . SET TMGXERR="In SPROV.TMGHL72: Set to NPI, but NPI is blank in field #16 of 'OBR' segment in HL7 message"
        . SET ADUZ=+$ORDER(^VA(200,"ANPI",NPI,0))
        . IF ADUZ'>0 DO  QUIT
        . . SET TMGXERR="In SPROV.TMGHL72: NPI "_NPI_" could not be found in ^VA(200,'ANPI' index"
        . NEW NAME SET NAME=$PIECE($GET(^VA(200,ADUZ,0)),"^",1)
        . IF NAME="" DO  QUIT
        . . SET TMGXERR="In SPROV.TMGHL72: ADUZ "_ADUZ_" did not return a value name."
        . NEW LNAME,FNAME
        . SET LNAME=$PIECE(NAME,",",1)
        . SET FNAME=$PIECE(NAME,",",2)
        . SET FNAME=$PIECE(FNAME," ",1)
        . SET PROV=ADUZ_TMGU(2)_LNAME_TMGU(2)_FNAME
        . SET TMGINFO("PROV")=PROV
        ELSE  DO
        . IF $$UP^XLFSTR(PROV)'["TOPPENBERG" SET PROV="^Doctor^Unspecified^"
        . IF PROV="" DO  QUIT
        . . SET TMGXERR="In SUPROV.TMGHL72: Ordering provider not provided in field #16 or 'OBR' segment in HL7 message"
        . NEW LNAME,FNAME,MNAME
        . SET LNAME=$PIECE(PROV,TMGU(2),2)
        . SET FNAME=$PIECE(PROV,TMGU(2),3)
        . SET MNAME=$PIECE(PROV,TMGU(2),4)
        . IF MNAME["-STATCARE" SET MNAME=""
        . IF MNAME["-APP" SET MNAME=""
        . NEW NAME SET NAME=LNAME_","_FNAME_" "_MNAME
        . IF (LNAME="TOPPENBERG")&(FNAME="EE") SET FNAME="MARCIA"  
        . SET NAME=$$TRIM^XLFSTR(NAME)
        . NEW DIC,X,Y
        . SET DIC=200,DIC(0)="M"
        . SET X=NAME
        . DO ^DIC
        . IF Y'>0 DO  QUIT
        . . SET TMGXERR="In SUPROV.TMGHL72: Unable find provider in lookup: '"_NAME_"'"
        . SET PROV=+Y_TMGU(2)_LNAME_TMGU(2)_FNAME
        . SET $PIECE(PROV,TMGU(2),13)="TMGDUZ"
        . SET TMGINFO("PROV")=PROV
SPVDN   QUIT
        ;
SUORL   ;"Purpose: Setup TMGINFO("ORL") and TMGINFO("LOC") -- Ordering locations
        ;"   also TMGINFO("INSTNAME")
        ;"Input: Uses globally scoped vars: TMGHL7MSG, TMGU, HLREC,IEN62D4
        ;"Output: Sets globally scoped variable TMGINFO("PROV")
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

