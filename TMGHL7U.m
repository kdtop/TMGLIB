TMGHL7U ;TMG/kst-HL7 transformation utility functions ;10/20/15, 8/11/16, 3/24/21
              ;;1.0;TMG-LIB;**1**;03/28/11
 ;
 ;"TMG HL7 TRANSFORMATION FUNCTIONS UTILITY
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"Especially 
 ;"=======================================================================
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"SETUPENV(TMGMSG,TMGENV,VERBOSE) -- Establish array with common variable values needed throughout. 
 ;"SETENV  -- general setup of environment for incoming HL7 message transformation and then VistA processing.
 ;"SETFMENV(IEN772,IEN773,IEN22720,TMGU)  --SETUP FILE MESSAGE TYPE MESSAGES ENVIRONMENT, for PATHGROUP txt files.
 ;"GETDIV(TMGMSG,IEN22720,TMGU) -- Get division characters, from HL7 message or defaults 
 ;"LMAPAPI(TMGENV,TESTID,OUT) --Gather mapping information between lab code and LABORATORY TEST entry.
 ;"IEN60API(TMGENV,IEN60,OUT) -- Gather mapping information between lab code and LABORATORY TEST entry.
 ;"LMAPAPI2(TMGENV,NLT,OUT) --Gather mapping information between NLT --> storage information 
 ;"LMAPAPI3(TMGENV,NLT,OUT) -- Gather mapping information between NLT --> storage information
 ;"GETWLMAP(TMGENV,IEN68D24,TMGMAP,VB) -- Get/check LOAD/WORK LIST mapping (holds orderable items)
 ;"GETAIMAP(TMGENV,IEN64D41,TMGMAP,VB) -- Get/check AUTO INSTRUMENT mapping (holds resultable items)
 ;"EXPND60(TEST,ORDER) -- Expand lab test panel to include component tests 
 ;"CMPLTORD(ORDERNUM) -- Set a given order number to complete status
 ;
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"TMGSTUTL, all the HL*, LA* code that the HL processing path normally calls.
 ;"=======================================================================
 ;
SETUPENV(TMGMSG,TMGENV,VERBOSE) ; 
        ;"Purpose: Establish array with common variable values needed throughout. 
        ;"Input: TMGMSG -- OPTIONAL.  PASS BY REFERENCE. 
        ;"              This can be the message to work
        ;"              on.  If not provided, then user will be prompted to
        ;"              load one in via an editor.  Input format:
        ;"              TMGMSG(1)="1st line"
        ;"              TMGMSG(2)="2nd line etc."
        ;"       TMGENV -- PASS BY REFERENCE.  Lab environment.  AN OUT PARAMETER. 
        ;"           TMGENV("PREFIX") -- e.g. "LMH"
        ;"           TMGENV("IEN 68.2") -- IEN in LOAD/WORK LIST (holds orderable items)
        ;"           TMGENV("IEN 62.4") -- IEN in AUTO INSTRUMENT (holds resultable items)
        ;"           TMGENV("IEN PROFILE IN 68.2")=1
        ;"           TMGENV("IEN 22720")=IEN22720
        ;"           TMGENV("INST")=TMGINST
        ;"           TMGENV("TMGU",1)=FS
        ;"           TMGENV("TMGU",2)=$EXTRACT(ECH,1)
        ;"           TMGENV("TMGU",3)=$EXTRACT(ECH,4)
        ;"       VERBOSE: OPTIONAL.  Default=0.  If 1, then user is interacted with, messages put on screen etc. 
        ;"Result: 1 if OK, or -1^Message
        NEW TMGRESULT SET TMGRESULT=1
        SET VERBOSE=+$GET(VERBOSE)
        NEW TMGHL7MSG,DIC,X,Y,%,HL7INST,HL7APP
        NEW IEN62D4 SET IEN62D4=0
        NEW IEN68D2 SET IEN68D2=0
        NEW IEN22720 SET IEN22720=0
        NEW TMGINST SET TMGINST=""
        NEW TMGLABPREFIX SET TMGLABPREFIX=""
        ;
        IF $DATA(TMGMSG)>0 GOTO SU2        
        WRITE "This code requires a sample HL7 message.",!
        SET %=1
        WRITE "Load sample HL7 message now" DO YN^DICN WRITE !
        IF %=-1 GOTO SUEDN
        IF %=2 GOTO SU2
        DO FILEMENU^TMGHL70(.TMGMSG,2)
        IF $DATA(TMGMSG)'>0 DO  GOTO SUEDN
        . SET TMGRESULT="-1^No message to work from"         
        ;        
SU2     KILL TMGENV
        ;
        NEW SEG SET SEG=$GET(TMGMSG($ORDER(TMGMSG(0))))
        NEW SEGNAME SET SEGNAME=$EXTRACT(SEG,1,3)
        IF SEGNAME'="MSH" SET TMGRESULT="-1^'MSH' not found on first line.  Got: '"_SEG_"'"
        IF TMGRESULT<0 GOTO SUEDN
        NEW MSH SET MSH=SEG
        NEW FS SET FS=$EXTRACT(MSH,4)
        NEW ECH SET ECH=$PIECE(MSH,FS,2)
        NEW TMGU DO SUTMGU^TMGHL7X2(.TMGU,FS,ECH) MERGE TMGENV("TMGU")=TMGU
        ;
        ;"SET HL7INST=$PIECE($PIECE(MSH,FS,4),TMGU(2),1)
        ;"SET HL7APP=$PIECE($PIECE(MSH,FS,3),TMGU(2),1)
SU3     SET IEN22720=$$GETCFG2^TMGHL70(MSH,.TMGU,.HL7INST,.HL7APP)
        IF +IEN22720<0 SET TMGRESULT=IEN22720 GOTO SUEDN
        IF IEN22720>0 DO  
        . SET IEN62D4=$PIECE($GET(^TMG(22720,IEN22720,0)),"^",3)
        . SET IEN68D2=$PIECE($GET(^TMG(22720,IEN22720,0)),"^",4)
        ;"------------                
        IF IEN62D4'>0 SET IEN62D4=$$GETAI^TMGHL70C(HL7INST) ;
        IF IEN62D4'>0 DO  GOTO SUEDN
        . NEW MSG SET MSG="Unable to determine AUTO INSTRUMENT to use in GETAI.TMGHL70C()"
        . SET TMGRESULT="-1^"_MSG
        . IF VERBOSE WRITE MSG,! DO PRESS2GO^TMGUSRI2
        ;
SU5     SET TMGINST=$$GET1^DIQ(62.4,IEN62D4_",",22701)
        IF TMGINST="" DO  GOTO SUEDN
        . SET TMGRESULT="-1^Field 22701 (TMG PERFORMING INSTITUTION) in record #"_IEN62D4_" in file 62.4 empty."
        . IF VERBOSE DO
        . . WRITE "No value found for field 22701 (TMG PERFORMING INSTITUTION) for",!
        . . WRITE "this record.  This means that appropriate TMG patch has not been",!
        . . WRITE "installed, or value has not been set.  Please correct and try again.",!
        . . DO PRESS2GO^TMGUSRI2
        ;
        SET TMGLABPREFIX=$$GET1^DIQ(62.4,IEN62D4_",",22700)
        IF TMGLABPREFIX="" DO  GOTO SUEDN
        . SET TMGRESULT="-1^Field 22700 (TMG LAB PREFIX CODE) in record #"_IEN62D4_" in file 62.4 empty."
        . IF VERBOSE DO
        . . WRITE "No value found for field 22700 (TMG LAB PREFIX CODE) for",!
        . . WRITE "this record.  Please correct and try again.",!
        . . DO PRESS2GO^TMGUSRI2        
        ;
        IF +IEN68D2'>0 SET IEN68D2=$GET(^TMG("TMP","TMGHL70","IEN 68.2"))
        IF +IEN68D2>0 GOTO SU6
        IF 'VERBOSE DO  GOTO SUEDN
        . SET TMGRESULT="-1^No default LOAD/WORK LIST stored at "+$NAME(^TMG("TMP","TMGHL70","IEN 68.2"))_", and not verbose mode to ask user."  
        WRITE !,"Next, please pick the 'LOAD/WORK LIST' entry that",!
        WRITE "holds will hold a list of all tests that can be ",!
        WRITE "ORDERED. NOTE: 'Orders' come in the OBR segment of",!
        WRITE "the HL7 message, and are configured separately from",!
        WRITE "RESULTS (found in OBX segments).",!
        SET DIC=68.2,DIC(0)="MAEQ"
        DO ^DIC WRITE ! SET IEN68D2=Y
        IF IEN68D2'>0 DO  GOTO SUEDN
        . SET TMGRESULT="-1^No LOAD/WORK LIST selected."  
        SET ^TMG("TMP","TMGHL70","IEN 68.2")=IEN68D2
SU6     IF IEN22720>0 GOTO SU8
        SET IEN22720=+$ORDER(^TMG(22720,"AAI",IEN62D4,0)) IF IEN22720>0 GOTO SU8        
        SET IEN22720=+$ORDER(^TMG(22720,"ALWL",IEN68D2,0)) IF IEN22720>0 GOTO SU8
        SET TMGRESULT="-1^Unable to determine record# in 22720 in SETUPENV.TMGHL7U"
        IF VERBOSE DO  GOTO SUEDN
        . WRITE !,"Problem.  Could not find a record in file 22720 to with either a",!
        . WRITE "  field value of `",IEN62D4," for field LINKED AUTO INSTRUMENT,",!
        . WRITE "  or a field value of `",IEN68D2," for field LINKED LOAW/WORK LIST.",!
        . WRITE "Please fix problem and try again",!
        . DO PRESS2GO^TMGUSRI2
        ;        
SU8     SET TMGENV("PREFIX")=TMGLABPREFIX
        SET TMGENV("IEN 62.4")=IEN62D4
        SET TMGENV("IEN 68.2")=IEN68D2
        SET TMGENV("IEN PROFILE IN 68.2")=1        
        SET TMGENV("IEN 22720")=IEN22720
        SET TMGENV("INST")=TMGINST
        NEW N100 SET N100=^TMG(22720,IEN22720,100)
        NEW FILER SET FILER=$PIECE(N100,"^",1,2) 
        NEW ALERT SET ALERT=$PIECE(N100,"^",3,4) 
        SET TMGENV("FILER CODE")=FILER
        SET TMGENV("ALERT CODE")=ALERT
SUEDN   QUIT TMGRESULT
        ; 
SETENV()  ;" -- NOT USED, CONSIDER DELETING LATER...
        ;"Purpose: general setup of environment for incoming HL7 message
        ;"      transformation and then VistA processing.
        ;"Input: uses HLMTIEN, HLMTIENS in global scope.  
        ;"Output: Sets variables with global scope:
        ;"        MSGSTORE,  <--- removing 4/26/19 
        ;"        TMGHL7MSG, IEN22720, TMGU, 
        ;"Temp backup of original message.
        ;"Results: 1 if OK, or -1^Message
        ;
        ;"NOTE: SETUPENV^TMGHL7U setups up environment for editing mapping etc.
        ;"      This function sets up environment for processing HL7 messages
        ;"      --Consider merging these two  <-- NO.  Rad filer needs to avoid SETUPENV code.  
        ;
        NEW TMGRESULT SET TMGRESULT=1
        ;
        SET IEN772=+$GET(HLMTIEN) IF IEN772'>0 DO  GOTO SEDN
        . SET TMGRESULT="-1^IEN in file 772 not provided to HL7XFRM^TMGHL72"
        SET IEN773=+$GET(HLMTIENS) IF IEN773'>0 DO  GOTO SEDN
        . SET TMGRESULT="-1^IEN in file 773 not provided to HL7XFRM^TMGHL72"
        NEW TMGSAN SET TMGSAN=$GET(HLREC("SAN")) IF TMGSAN="" DO  GOTO SEDN
        . SET TMGRESULT="-1^Sending application name not provided to HL7XFRM^TMGHL72"
        NEW DIC,X,Y
        SET DIC=62.4,DIC(0)="M"
        SET X=TMGSAN
        DO ^DIC
        IF +Y'>0 DO  GOTO SEDN
        . SET TMGRESULT="-1^Unable to find AUTO INSTRUMENT (62.4) record matching sending application name '"_TMGSAN_"'"
        SET TMGHL7MSG("IEN62.4")=+Y
        SET IEN62D4=+Y
        SET DIC=22720
        SET X=TMGSAN
        DO ^DIC
        IF +Y'>0 DO  GOTO SEDN
        . SET TMGRESULT="-1^Unable to find file TMG HL7 MESSAGE TRANSFORM SETTINGS (22720) record matching sending application name '"_TMGSAN_"'"
        SET IEN22720=+Y
        NEW FS SET FS=$GET(HLREC("FS"))  ;"FIELD SEPARATOR
        IF FS="" DO  GOTO SEDN
        . SET TMGRESULT="-1^Field separator character not provide TOHL7XFRM^TMGHL72"
        NEW ECH SET ECH=$GET(HLREC("ECH")) ;"ENCODING CHARACTERS
        IF ECH="" DO  GOTO SEDN
        . SET TMGRESULT="-1^Enchoding characters not provide TOHL7XFRM^TMGHL72"
SEV2    DO SUTMGU^TMGHL7X2(.TMGU,FS,ECH)
SEDN    QUIT TMGRESULT
        ;
SETFMENV(IEN772,IEN773,IEN22720,TMGU)  ;"SETUP FILE MESSAGE TYPE MESSAGES ENVIRONMENT. 
        ;"Purpose: setup of environment for processing PATHGROUP txt files.
        ;"Input:  
        ;"Results: 1 if OK, or -1^Message
        ;
        ;"NOTE: SETUPENV^TMGHL7U setups up environment for editing mapping etc.
        ;"      This function sets up environment for processing HL7 messages
        ;"      --Consider merging these
        ;
        NEW TMGRESULT SET TMGRESULT=1
        NEW FS,ECH
        SET (FS,HLREC("FS"))="|"
        SET (ECH,HLREC("ECH"))="^~\&"   ;"ENCODING CHARACTERS
        SET HLQUIT=0,HLNODE="",HLNEXT="D HLNEXT^HLCSUTL"     
        ;
        NEW MSH SET MSH=$GET(^HLMA(IEN773,"MSH",1,0))
        NEW INFO SET TMGRESULT=$$MSH2IENA^TMGHL7U2(MSH,.INFO) ;"MSH HEADER TO IEN INFO ARRAY
        IF TMGRESULT'>0 GOTO SEFMDN
        ;"Is this needed?? --> MERGE TMGHL7MSG=INFO
        SET IEN22720=INFO("IEN22720")
        DO SUTMGU^TMGHL7X2(.TMGU,FS,ECH)
SEFMDN  QUIT TMGRESULT
        ;        
GETDIV(TMGMSG,IEN22720,TMGU) ;
        ;"Purpose: Get division characters, either from provided test HL7 message,
        ;"         or from default values in 22720, IF provided.
        ;"Input: TMGMSG,-- array of text message
        ;"       IEN22720 -- optional
        ;"       TMGU -- OUT PARAMETER.
        ;"Result: 1 if OK, or -1^Message IF problem. 
        ;"Output: TMGU may be filled. 
        NEW TMGRESULT SET TMGRESULT=1
        NEW DIVSTR SET DIVSTR=""
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(TMGMSG(IDX)) QUIT:(+IDX'>0)!(DIVSTR'="")  DO
        . NEW STR SET STR=$GET(TMGMSG(IDX)) QUIT:STR=""
        . IF $EXTRACT(STR,1,3)'="MSH" QUIT
        . SET DIVSTR=$EXTRACT(STR,4,8)                
        IF DIVSTR'="" GOTO GDV2
        SET IEN22720=+$GET(IEN22720)
        SET DIVSTR=$GET(^TMG(22720,IEN22720,3))
        IF DIVSTR="" DO  GOTO GDVDN
        . SET TMGRESULT="-1^Unable to determine HL7 divisor symbols from sample HL7 message or record #"_IEN22720_" in file 22720."
GDV2    NEW FS SET FS=$EXTRACT(DIVSTR,1)
        NEW ECH SET ECH=$EXTRACT(DIVSTR,2,99)
        DO SUTMGU^TMGHL7X2(.TMGU,FS,ECH)        
GDVDN   QUIT TMGRESULT
        ;
LMAPAPI(TMGENV,TEST,OUT) ;
        ;"Purpose: Gather mapping information between lab code and LABORATORY TEST entry.
        ;"Input: TMGENV -- PASS BY REFERENCE.  Lab environment
        ;"           TMGENV("PREFIX") -- e.g. "LMH"
        ;"           TMGENV("IEN 68.2") -- IEN in LOAD/WORK LIST (holds orderable items)
        ;"           TMGENV("IEN 62.4") -- IEN in AUTO INSTRUMENT (holds resultable items)                
        ;"       TEST -- lab code as found in HL7 message, e.g. OSMOC
        ;"                  or Lab_Code^Lab_Name  (i.e. Lab_Name is optional)
        ;"                  or <TestCode>^<Test Text>^<Coding System>^<Alternative Identifier>^<Alternative Text>^<Alternative coding system>
        ;"       OUT -- PASS AS REFERENCE.  Format:
        ;"          OUT(<TestID>,"SYN60")=SYNONYM (i.e. with site prefix attached, trimmed to 60 chars)  //changed 30 -> 60 chars
        ;"          OUT(<TestID>,"SYNONYM")=SYNONYM (prefix+TESTNAME) 
        ;"          OUT(<TestID>,"IEN 60")=IEN60^Name
        ;"          OUT(<TestID>,"MAP SYN->60")=IEN60^Name
        ;"          OUT(<TestID>,"VA CODE")=IEN64^WKLDCode^Name  (pointed-to information from IEN60)
        ;"          OUT(<TestID>,"NLT CODE")=IEN64^NLTCODE^NLT_name
        ;"          OUT(<TestID>,"ORDERABLES","MAP 60->68.24")=IEN68D24
        ;"          OUT(<TestID>,"RESULTABLES","MAP NLT->62.41")=IEN62D41
        ;"          OUT(<TestID>,"RESULTABLES","MAP 62.41->60")=NEWIEN60^NAME
        ;"          OUT(<TestID>,"SPECIMEN (64.061)")=IEN64D061^Name
        ;"          OUT(<TestID>,"SPECIMEN (61)")=IEN61^Name
        ;"          OUT(<TestID>,"ALT TEST ID")=ALT TEST ID
        ;"          OUT(<TestID>,"ALT TEST NAME")=ALT TEST NAME
        ;"       TMGLABPREFIX -- initials unique to HL7 source, e.g. PG
        ;"       IEN62D4 -- IEN in AUTO INSTRUMENT file (holds resultable tests)
        ;"       IEN68D2 -- IEN in LOAD/WORK LIST (holds orderable tests)
        ;"Result: 1 if OK, or 0^Message of mapping problem, or -1^Step#^Message if error.
        NEW TMGRESULT SET TMGRESULT=1
        NEW TMGLABPREFIX SET TMGLABPREFIX=TMGENV("PREFIX")
        NEW IEN62D4 SET IEN62D4=TMGENV("IEN 62.4")
        NEW IEN68D2 SET IEN68D2=TMGENV("IEN 68.2")
        NEW TMGU MERGE TMGU=TMGENV("TMGU")
        NEW TESTID SET TESTID=$PIECE(TEST,TMGU(2),1)
        NEW TESTNAME SET TESTNAME=$PIECE(TEST,TMGU(2),2)
        NEW ALTTESTID SET ALTTESTID=$PIECE(TEST,TMGU(2),4)
        NEW ALTTESTNAME SET ALTTESTNAME=$PIECE(TEST,TMGU(2),5)
        NEW X,Y,IEN61,IEN62,IEN64,TEMP,SYNONYM,TEMPNAME
        NEW IEN60 SET IEN60=0
VMA1    FOR TEMPNAME=TESTID,TESTNAME,ALTTESTID,ALTTESTNAME QUIT:(+IEN60>0)  DO
        . IF $$TRIM^XLFSTR(TEMPNAME)="" QUIT
        . SET SYNONYM=TMGLABPREFIX_"-"_TEMPNAME
        . SET OUT(TESTID,"SYNONYM")=SYNONYM  ;"<-- this will store only for TESTNAME, TESTID will get overwritten..
        . NEW SYN60 SET SYN60=$E(SYNONYM,1,60)  ;"//kt CHANGED 30 -> 60 chars  SYN30 -> SYN60
        . SET OUT(TESTID,"SYN60")=SYN60  ;"//kt changed 30 -> 60
        . SET IEN60=+$ORDER(^LAB(60,"B",SYN60,""))  ;"//kt changed 30 -> 60
        . SET OUT(TESTID,"IEN60 source is via ^LAB(60,""B"","""_SYN60_""")")=IEN60
        IF IEN60'>0 DO  GOTO VMADN
        . SET TMGRESULT="-1^1^Can't find an existing map for test '"_TESTID_"'"
        . SET IEN60="??"
        NEW TMPOUT
        SET TMGRESULT=$$IEN60API(.TMGENV,.IEN60,.TMPOUT)
        MERGE OUT(TESTID)=TMPOUT
        ;" <------------------------------------>
        GOTO VMADN
        ;"NEW TESTNAME SET TESTNAME=$$GET1^DIQ(60,IEN60_",",.01)
        ;"SET OUT(TESTID,"MAP SYN->60")=IEN60_"^"_TESTNAME
        ;"SET OUT(TESTID,"IEN60")=IEN60_"^"_TESTNAME
        ;"NEW IEN64D061 SET IEN64D061=+$PIECE($GET(^LAB(60,+IEN60,"TMG")),"^",1)
        ;"SET OUT(TESTID,"SPECIMEN (64.061)")=IEN64D061_"^"_$PIECE($GET(^LAB(64.061,IEN64D061,0)),"^",1)
        ;"NEW IEN61 SET IEN61=+$PIECE($GET(^LAB(60,+IEN60,"TMG")),"^",2)
        ;"SET OUT(TESTID,"SPECIMEN (61)")=IEN61_"^"_$PIECE($GET(^LAB(61,IEN61,0)),"^",1)
        ;"SET IEN64=+$PIECE($GET(^LAB(60,IEN60,64)),"^",1)  ;"64;1 = NATIONAL VA LAB CODE
        ;"IF IEN64'>0 DO
        ;". IF TMGRESULT'<0 SET TMGRESULT="-1^Unable to find value for IEN64 at ^LAB(60,"_IEN60_",64), piece 1)"
        ;". SET IEN64="??"
        ;"SET TEMP=$GET(^LAM(IEN64,0))
        ;"SET OUT(TESTID,"VA CODE")=IEN64_"^"_$P(TEMP,"^",2)_"^"_$P(TEMP,"^",1) 
        ;"SET IEN64=$PIECE($GET(^LAB(60,IEN60,64)),"^",2)   ;"64;2= RESULT NLT CODE
        ;"IF IEN64'>0 DO
        ;". IF TMGRESULT'<0 SET TMGRESULT="-1^Unable to find value for IEN64 at ^LAB(60,"_IEN60_",64), piece 2)"
        ;". SET IEN64="??"
        ;"SET TEMP=$GET(^LAM(IEN64,0))
        ;"NEW NLTCODE SET NLTCODE=$PIECE(TEMP,"^",2)
        ;"SET OUT(TESTID,"NLT CODE")=IEN64_"^"_NLTCODE_"^"_$P(TEMP,"^",1)
        ;"NEW PROFILE SET PROFILE=1  ;"CHECK!!! <-- I don't know IF this is always correct
        ;"NEW IEN68D24 SET IEN68D24=+$ORDER(^LRO(68.2,+IEN68D2,10,PROFILE,1,"B",+IEN60,"")) ;
        ;"IF IEN68D24'>0 DO  ;"GOTO VMADN
        ;". SET TMGRESULT="-1^2^Test not found -- i.e. not individually orderable)"
        ;". SET IEN68D24="??"
        ;"SET OUT(TESTID,"ORDERABLES","MAP 60->68.24")=IEN68D24
        ;";"Look up NLT code in auto instrument file, and get pointed to ien60.
        ;"IF NLTCODE'>0 GOTO VMADN
        ;"NEW IEN62D41 SET IEN62D41=+$ORDER(^LAB(62.4,IEN62D4,3,"AC",NLTCODE,""))
        ;"IF IEN62D41'>0 DO  ;"GOTO VMADN
        ;". SET IEN62D41="??"
        ;". IF TMGRESULT'<0 SET TMGRESULT="-1^3^Unable to find mapped NLT code '"_NLTCODE_" in AC index in file 62.4"
        ;"SET OUT(TESTID,"RESULTABLES","MAP NLT->62.41")=IEN62D41
        ;"NEW NEWIEN60 SET NEWIEN60=+$PIECE($GET(^LAB(62.4,IEN62D4,3,IEN62D41,0)),"^",1)
        ;"IF NEWIEN60'>0 SET NEWIEN60="??"
        ;"SET OUT(TESTID,"RESULTABLES","MAP 62.41->60")=NEWIEN60_"^"_$PIECE($GET(^LAB(60,NEWIEN60,0)),"^",1)
        ;"IF NEWIEN60'=IEN60 DO
        ;". IF TMGRESULT'<0 SET TMGRESULT="0^Notice that pointed-to tests (file #60) are different!"
VMADN   QUIT TMGRESULT
        ;
IEN60API(TMGENV,IEN60,OUT,ERR) ;
        ;"Purpose: Gather mapping information between lab code and LABORATORY TEST entry.
        ;"Input: TMGENV -- PASS BY REFERENCE.  Lab environment
        ;"           TMGENV("PREFIX") -- e.g. "LMH"
        ;"           TMGENV("IEN 68.2") -- IEN in LOAD/WORK LIST (holds orderable items)
        ;"           TMGENV("IEN 62.4") -- IEN in AUTO INSTRUMENT (holds resultable items)                
        ;"       IEN60 -- IEN 60.  DON'T PASS BY REFERENCE, it is made to be only numeric (strips off other pieces)
        ;"       OUT -- PASS AS REFERENCE.  Format:
        ;"          OUT("SYN60")=SYNONYM (i.e. with site prefix attached, trimmed to 60 chars)  //kt changed 30 -> 60
        ;"          OUT("IEN60")=IEN60^Name^PrintName
        ;"          OUT("MAP SYN->60")=IEN60^Name^PrintName
        ;"          OUT("VA CODE")=IEN64^WKLDCode^Name  (pointed-to information from IEN60)
        ;"          OUT("NLT CODE")=IEN64^NLTCODE^NLT_name
        ;"          OUT("STORAGE FLD 63.04")=FLD63D04^DataName
        ;"          OUT("ORDERABLES","MAP 60->68.24")=IEN68D24
        ;"          OUT("RESULTABLES","MAP NLT->62.41")=IEN62D41
        ;"          OUT("RESULTABLES","MAP 62.41->60")=NEWIEN60^NAME
        ;"          OUT("SPECIMEN (64.061)")=IEN64D061^Name
        ;"          OUT("SPECIMEN (61)")=IEN61^Name
        ;"       ERR -- OPTIONAL.  PASS BY REFERENCE.  An OUT parameter.  Format:
        ;"         ERR(index)=<error message>
        ;"Result: 1 if OK, or 0^Message of mapping problem, or -1^Step#^Message if error.
        NEW TMGRESULT SET TMGRESULT=1
        NEW TMGLABPREFIX SET TMGLABPREFIX=TMGENV("PREFIX")
        NEW IEN62D4 SET IEN62D4=TMGENV("IEN 62.4")
        NEW IEN68D2 SET IEN68D2=TMGENV("IEN 68.2")
        NEW X,Y,IEN61,IEN62,IEN64,TEMP
        NEW EIDX SET EIDX=1
        SET IEN60=+$GET(IEN60)
        IF IEN60'>0 DO  GOTO I60DN
        . SET TMGRESULT="-1^No IEN60 provided."
        . SET ERR(EIDX)=TMGRESULT,EIDX=EIDX+1
        ;"NEW TESTNAME SET TESTNAME=$$GET1^DIQ(60,IEN60_",",.01)     
        NEW TESTNAME SET TESTNAME=$PIECE($GET(^LAB(60,+IEN60,0)),"^",1)
        NEW LABPRNAME SET LABPRNAME=$PIECE($GET(^LAB(60,+IEN60,.1)),"^",1)        
        SET OUT("MAP SYN->60")=IEN60_"^"_TESTNAME_"^"_LABPRNAME
        SET OUT("IEN60")=IEN60_"^"_TESTNAME_"^"_LABPRNAME
        NEW IEN64D061 SET IEN64D061=+$PIECE($GET(^LAB(60,+IEN60,"TMG")),"^",1)
        ;"SET OUT("NOTE: IEN SPECIMEN (64.061) source is via ""^LAB(60,"_+IEN60_",""TMG""), piece #1")=IEN64D061 
        SET OUT("SPECIMEN (64.061)")=IEN64D061_"^"_$PIECE($GET(^LAB(64.061,IEN64D061,0)),"^",1)
        SET OUT("SPECIMEN (64.061)","NOTE: IEN SPECIMEN (64.061) source is via ""^LAB(60,"_+IEN60_",""TMG""), piece #1")=IEN64D061 
        NEW IEN61 SET IEN61=+$PIECE($GET(^LAB(60,+IEN60,"TMG")),"^",2)
        ;"SET OUT("NOTE: IEN SPECIMEN (61) source is via ""^LAB(60,"_+IEN60_",""TMG""), piece #2")=IEN61 
        SET OUT("SPECIMEN (61)")=IEN61_"^"_$PIECE($GET(^LAB(61,IEN61,0)),"^",1)
        SET OUT("SPECIMEN (61)","NOTE: IEN SPECIMEN (61) source is via ""^LAB(60,"_+IEN60_",""TMG""), piece #2")=IEN61 
        SET IEN64=+$PIECE($GET(^LAB(60,IEN60,64)),"^",1)  ;"64;1 = NATIONAL VA LAB CODE
        ;"SET OUT("NOTE: IEN 64 (NLT) source is via ""^LAB(60,"_+IEN60_",64), piece #1")=IEN64 
        IF IEN64'>0 DO
        . IF TMGRESULT'<0 DO
        . . SET TMGRESULT="-1^Unable to find value for IEN64 at ^LAB(60,"_IEN60_",64), piece 1)"
        . . SET ERR(EIDX)=TMGRESULT,EIDX=EIDX+1
        . SET IEN64="??"
        SET TEMP=$GET(^LAM(IEN64,0))
        SET OUT("VA CODE")=IEN64_"^"_$P(TEMP,"^",2)_"^"_$P(TEMP,"^",1) 
        SET OUT("VA CODE","NOTE: IEN 64 (NLT) source is via ""^LAB(60,"_+IEN60_",64), piece #1")=IEN64 
        ;"SET OUT("NOTE: VA CODE info source is via ""^LAM("_+IEN64_",0)")=TEMP 
        SET OUT("VA CODE","NOTE: VA CODE info source is via ""^LAM("_+IEN64_",0)")=TEMP 
        SET IEN64=$PIECE($GET(^LAB(60,IEN60,64)),"^",2)   ;"64;2= RESULT NLT CODE
        ;"SET OUT("NOTE: 2nd IEN 64 (National VA Lab Code) source is via ""^LAB(60,"_+IEN60_",64), piece #2")=IEN64 
        SET OUT("VA CODE","NOTE: 2nd IEN 64 (National VA Lab Code) source is via ""^LAB(60,"_+IEN60_",64), piece #2")=IEN64 
        IF IEN64'>0 DO
        . IF TMGRESULT'<0 DO
        . . SET TMGRESULT="-1^Unable to find value for IEN64 at ^LAB(60,"_IEN60_",64), piece 2)"
        . . SET ERR(EIDX)=TMGRESULT,EIDX=EIDX+1
        . SET IEN64="??"
        SET TEMP=$GET(^LAM(IEN64,0))
        NEW NLTCODE SET NLTCODE=$PIECE(TEMP,"^",2)
        SET OUT("NLT CODE")=IEN64_"^"_NLTCODE_"^"_$P(TEMP,"^",1)
        ;"SET OUT("NOTE: NLT CODE source is via ""^LAM("_+IEN64_",0), piece #2")=NLTCODE 
        SET OUT("NLT CODE","NOTE: NLT CODE source is via ""^LAM("_+IEN64_",0), piece #2")=NLTCODE 
        NEW FLD63D04 SET FLD63D04=+$PIECE($GET(^LAB(60,IEN60,.2)),"^",1)
        NEW DATANAME SET DATANAME=$PIECE($GET(^DD(63.04,FLD63D04,0)),"^",1)
        SET OUT("STORAGE FLD 63.04")=FLD63D04_"^"_DATANAME
        ;"SET OUT("NOTE: STORAGE FLD 63.04 source is via ""^LAB(60,"_+IEN60_",.2), piece #1")=FLD63D04_"^"_DATANAME 
        SET OUT("STORAGE FLD 63.04","NOTE: STORAGE FLD 63.04 source is via ""^LAB(60,"_+IEN60_",.2), piece #1")=FLD63D04_"^"_DATANAME 
        NEW PROFILE SET PROFILE=1  ;"CHECK!!! <-- I don't know if this is always correct
        NEW IEN68D24 SET IEN68D24=+$ORDER(^LRO(68.2,+IEN68D2,10,PROFILE,1,"B",+IEN60,"")) ;
        ;"SET OUT("NOTE: IEN 68.24 source is via ""^LRO(68.2,"_+IEN68D2_",10,"_PROFILE_",1,""B"","_+IEN60_","""")")=IEN68D24 
        IF IEN68D24'>0 DO  ;"GOTO VMADN
        . SET TMGRESULT="-1^2^Test not found -- i.e. not individually orderable)"
        . SET ERR(EIDX)=TMGRESULT,EIDX=EIDX+1
        . SET IEN68D24="??"
        ;"SET OUT("ORDERABLES","MAP 60->68.24")=IEN68D24
        SET OUT("ORDERABLES","MAP 60->68.24","NOTE: IEN 68.24 source is via ""^LRO(68.2,"_+IEN68D2_",10,"_PROFILE_",1,""B"","_+IEN60_","""")")=IEN68D24 
        ;"Look up NLT code in auto instrument file, and get pointed to ien60.
        IF NLTCODE'>0 GOTO VMADN
        NEW IEN62D41 SET IEN62D41=+$ORDER(^LAB(62.4,IEN62D4,3,"AC",NLTCODE,""))
        ;"SET OUT("NOTE: IEN62.41 source is via ""^LAB(62.4,"_+IEN62D4_",3,""AC"","""_NLTCODE_""","")")=IEN62D41
        IF IEN62D41'>0 DO  ;"GOTO VMADN
        . SET IEN62D41="??"
        . IF TMGRESULT'<0 DO
        . . SET TMGRESULT="-1^3^Unable to find mapped NLT code '"_NLTCODE_" in AC index in file 62.4"
        . . SET ERR(EIDX)=TMGRESULT,EIDX=EIDX+1
        SET OUT("RESULTABLES","MAP NLT->62.41")=IEN62D41
        SET OUT("RESULTABLES","MAP NLT->62.41","NOTE: IEN62.41 source is via ""^LAB(62.4,"_+IEN62D4_",3,""AC"","""_NLTCODE_""","")")=IEN62D41        
        NEW NEWIEN60 SET NEWIEN60=+$PIECE($GET(^LAB(62.4,IEN62D4,3,IEN62D41,0)),"^",1)
        ;"SET OUT("NOTE: NEWIEN60 source is via ""^LAB(62.4,"_IEN62D4_",3,"_IEN62D41_",0)"", piece #1)")=NEWIEN60
        SET OUT("IEN60 #2")=NEWIEN60
        SET OUT("IEN60 #2","NOTE: NEWIEN60 source is via ""^LAB(62.4,"_IEN62D4_",3,"_IEN62D41_",0)"", piece #1)")=NEWIEN60        
        IF NEWIEN60'>0 SET NEWIEN60="??"
        SET OUT("RESULTABLES","MAP 62.41->60")=NEWIEN60_"^"_$PIECE($GET(^LAB(60,NEWIEN60,0)),"^",1)
        IF NEWIEN60'=IEN60 DO
        . IF TMGRESULT'<0 DO
        . . SET TMGRESULT="0^Notice that pointed-to tests (file #60) are different!"
        . . SET ERR(EIDX)=TMGRESULT,EIDX=EIDX+1
I60DN   QUIT TMGRESULT
        ;        
LMAPAPI2(TMGENV,NLT,OUT) ;"GATHER NATIONAL LABORATORY TEST (NLT) MAPPING.
        ;"Purpose: Gather mapping information between NLT --> storage information
        ;"Input: TMGENV -- PASS BY REFERENCE.  Lab environment
        ;"           TMGENV("PREFIX") -- e.g. "LMH"
        ;"           TMGENV("IEN 68.2") -- IEN in LOAD/WORK LIST (holds orderable items)
        ;"           TMGENV("IEN 62.4") -- IEN in AUTO INSTRUMENT (holds resultable items)                
        ;"       NLT -- NATIONAL LAB TEST / WKLD CODE (e.g. "81172.0000"), which is 1 field in file 64 (WKLD CODE)
        ;"       OUT -- PASS AS REFERENCE.  Format:
        ;"          OUT(NLT,"IEN64")=IEN64^NLT^NAME    ;64 is WKLD CODE file
        ;"          OUT(NLT,"AUTO INSTRUMENT TEST")=IENS^TESTNAME  
        ;"          OUT(NLT,"STORAGE")=STORAGE Code^FIELD^FieldName
        ;"          OUT(NLT,"IEN 60")=IEN 60
        ;"Result: 1 if OK, or 0^Message of mapping problem, or -1^Step#^Message if error.
        NEW TMGRESULT SET TMGRESULT=1
        NEW IEN62D4 SET IEN62D4=TMGENV("IEN 62.4")
        KILL OUT
        SET NLT=$GET(NLT)
        ;"//kt note 10/20/19.  I had situation were input was "84295.000^NA^99VA64", so changing to work with only p1
        ;"     However, this means that this code will no longer alter input NLT variable.  I see that this was
        ;"     being called with NLT by reference, so I don't know if this will break anything or not.   
        NEW NLTP1 SET NLTP1=$PIECE(NLT,"^",1)
        IF NLTP1="" DO  GOTO VMA2DN
        . SET TMGRESULT="-1^NLT code not provided."
        NEW COUNT SET COUNT=0,COUNT(1)=""
        NEW IEN64 SET IEN64=0
        FOR  SET IEN64=$ORDER(^LAM("C",NLTP1_" ",IEN64)) QUIT:(+IEN64'>0)  DO
        . NEW NAME SET NAME=$PIECE($GET(^LAM(IEN64,0)),"^",1)
        . SET OUT(NLTP1,"IEN64")=IEN64_"^"_NLTP1_"^"_NAME
        . SET COUNT=COUNT+1
        . SET COUNT(1)=COUNT(1)_"`"_IEN64_";"
        IF COUNT=0 DO  GOTO VMA2DN
        . SET TMGRESULT="-1^"_NLTP1_" not found in file 64 (WKLD CODE)" 
        IF COUNT>1 DO  GOTO VMA2DN
        . SET TMGRESULT="-1^"_NLTP1_" linked to more than one entry in 64 (WKLD CODE): "_COUNT(1)
        KILL COUNT SET COUNT=0,COUNT(1)=""
        NEW IEN62D41 SET IEN62D41=0
        FOR  SET IEN62D41=$ORDER(^LAB(62.4,IEN62D4,3,"AC",NLTP1,IEN62D41)) QUIT:(+IEN62D41'>0)  DO
        . NEW IENS SET IENS=IEN62D41_","_IEN62D4_","
        . SET COUNT=COUNT+1
        . SET COUNT(1)=COUNT(1)_"`"_IENS_";"
        . NEW TESTNAME SET TESTNAME=$$GET1^DIQ(62.41,IENS,.01)
        . SET OUT(NLTP1,"AUTO INSTRUMENT TEST")=IENS_"^"_TESTNAME
        . NEW STORAGE SET STORAGE=$$GET1^DIQ(62.41,IENS,11)
        . SET OUT(NLTP1,"IEN 60")=$$GET1^DIQ(62.41,IENS,.01,"I")
        . IF $EXTRACT(STORAGE,1,3)="TV(" DO
        . . NEW FIELD SET FIELD=+$PIECE(STORAGE,"TV(",2)
        . . NEW ZN SET ZN=$GET(^DD(63.04,FIELD,0))
        . . NEW NAME SET NAME=$PIECE(ZN,"^",1)
        . . SET OUT(NLTP1,"STORAGE")=STORAGE_"^"_FIELD_"^"_NAME
        . ELSE  DO
        . . SET OUT(NLTP1,"STORAGE")=STORAGE
        . SET OUT(NLTP1,"STORAGE","source:")="File AUTO INSTRUMENT (#62.4) `"_IEN62D4_", multiple-field CHEM TESTS (#62.41) `"_IEN62D41 
        IF COUNT=0 DO  GOTO VMA2DN
        . SET TMGRESULT="-1^"_NLTP1_" NOT found in AUTO INSTRUMENT file (#62.4)" 
        IF COUNT>1 DO  GOTO VMA2DN
        . SET TMGRESULT="-1^"_NLTP1_" linked to more than one entry in AUTO INSTRUMENT file (#62.4): "_COUNT(1)
VMA2DN  QUIT TMGRESULT
        ;
LMAPAPI3(TMGENV,NLT,OUT) ;"GATHER NATIONAL LABORATORY TEST (NLT) MAPPING.  "//NOT USED??
        ;"Purpose: Gather mapping information between NLT --> storage information
        ;"Input: TMGENV -- PASS BY REFERENCE.  Lab environment
        ;"           TMGENV("PREFIX") -- e.g. "LMH"
        ;"           TMGENV("IEN 68.2") -- IEN in LOAD/WORK LIST (holds orderable items)
        ;"           TMGENV("IEN 62.4") -- IEN in AUTO INSTRUMENT (holds resultable items)                
        ;"           TMGENV("IEN PROFILE IN 68.2")=1  ;This is really IEN68D23, IN IEN68D2
        ;"       NLT -- NATIONAL LAB TEST / WKLD CODE (e.g. "81172.0000"), which is 1 field in file 64 (WKLD CODE)
        ;"       OUT -- PASS AS REFERENCE.  Format:
        ;"          OUT(NLT,"IEN64")=IEN64^NLT^NAME    ;64 is WKLD CODE file
        ;"          OUT(NLT,"AUTO INSTRUMENT TEST")=IENS^TESTNAME  
        ;"          OUT(NLT,"LWL TEST")=IENS (or '??' IF not found) ;IENS is for file 68.24 (in 68.23 in 68.2)  LWL = LOAD/WORKLOAD
        ;"Result: 1 if OK, or 0^Message of mapping problem, or -1^Step#^Message IF error.
        NEW TMGRESULT SET TMGRESULT=1
        NEW IEN62D4 SET IEN62D4=TMGENV("IEN 62.4")
        NEW IEN68D2 SET IEN68D2=TMGENV("IEN 68.2")
        SET TMGRESULT=$$LMAPAPI2(.TMGENV,NLT,.OUT) ;"GATHER NATIONAL LABORATORY TEST (NLT) MAPPING.
        IF +TMGRESULT'>0 GOTO VMA3DN
        NEW IEN60 SET IEN60=+$GET(OUT(NLT,"IEN 60"))
        IF IEN60'>0 GOTO VMA3DN
        NEW IEN68D23 SET IEN68D23=TMGENV("IEN PROFILE IN 68.2")
        NEW IEN68D24 SET IEN68D24=$ORDER(^LRO(68.2,IEN68D2,10,IEN68D23,1,"B",IEN60,0))
        IF +IEN68D24'>0 SET IEN68D24="??"
        SET OUT(NLT,"LWL TEST")=IEN68D24_","_IEN68D23_","_IEN68D2_","
        ;"
VMA3DN  QUIT TMGRESULT
        ;
GETWLMAP(TMGENV,IEN68D24,TMGMAP,VB) ;
        ;"Purpose: Check LOAD/WORK LIST mapping (holds orderable itesm)
        ;"Input: TMGENV -- PASS BY REFERENCE.  Lab environment
        ;"           TMGENV("PREFIX") -- e.g. "LMH"
        ;"           TMGENV("IEN 68.2") -- IEN in LOAD/WORK LIST (holds orderable items)
        ;"           TMGENV("IEN 62.4") -- IEN in AUTO INSTRUMENT (holds resultable items)                
        ;"           TMGENV("IEN PROFILE IN 68.2")=1  ;This is really IEN68D23, IN IEN68D2
        ;"       IEN68D24 -- IEN 68.24 (TEST field in PROFILE field in LOAD/WORK LIST file)
        ;"       TMGMAP -- PASS BY REFERENCE.  AN OUT PARAMETER.
        ;"         TMGMAP("LOAD/WORK LIST",IEN68D24,"LR60")=LR60
        ;"         TMGMAP("LOAD/WORK LIST",IEN68D24,"LR64")=LR64
        ;"         TMGMAP("LOAD/WORK LIST",IEN68D24,"LR61")=LR61
        ;"         TMGMAP("LOAD/WORK LIST",IEN68D24,"LR64.2")=LR642
        ;"         TMGMAP("LOAD/WORK LIST",IEN68D24,"LR62")=LR62
        ;"         TMGMAP("LOAD/WORK LIST",IEN68D24,"TEST NAME")=LABORATORY TEST NAME (trimmed to 25 chars)
        ;"         TMGMAP("LOAD/WORK LIST",IEN68D24,"ORDER NLT CODE")=NLT Code, or '<MISSING>'
        ;"         TMGMAP("LOAD/WORK LIST",IEN68D24,"SPECIMEN(IEN)")=Specimen IEN, or '<MISSING>'
        ;"         TMGMAP("LOAD/WORK LIST",IEN68D24,"LR64.061")=LR64D061, or '<MISSING>'
        ;"         TMGMAP("LOAD/WORK LIST",IEN68D24,"LR64.061(0)")=LR64D061(0)
        ;"         TMGMAP("LOAD/WORK LIST",IEN68D24,"HL7 SPEC")=HL Spec, or '<MISSING>'
        ;"         TMGMAP("LOAD/WORK LIST",IEN68D24,"ORDER NLT NAME")=NLT Name, or '<MISSING>'
        ;"         TMGMAP("LOAD/WORK LIST",IEN68D24,"COLLECTION SAMPLE")=Sample, or '<MISSING>'
        ;"         TMGMAP("LOAD/WORK LIST",IEN68D24,"WKLD CODE")=WkldCode, or '<MISSING>'
        ;"         TMGMAP("LOAD/WORK LIST",IEN68D24,"NOTE")=X (optional)
        ;"         TMGMAP("LOAD/WORK LIST",IEN68D24,"ZZ ------------------")=""
        ;"       VB -- VERBOSE (1 for true, 0 for false) Default=0
        ;"Result: 1 if OK, or -1 IF problem (i.e. something missing) 
        ;//TO-DO -- move mapping to LMAPAPI functions.
        SET VB=+$GET(VB) ;"VERBOSE
        
        NEW IEN68D2 SET IEN68D2=TMGENV("IEN 68.2")
        NEW IEN68D23 SET IEN68D23=TMGENV("IEN PROFILE IN 68.2")
        NEW X,Y
        NEW LA7EXIT,LA7INTYP,LA7LINE,LA7LINE2,LA7NOW,LA7PAGE,LA7CODE
        NEW LA76248,LR60,LR61,LR62,LR64,LR642
        NEW MISSING SET MISSING="<MISSING>"
        NEW TMGRESULT SET TMGRESULT=1
        ;
        SET X=^LRO(68.2,IEN68D2,10,IEN68D23,1,IEN68D24,0)
        SET LR60=+X
        SET TMGMAP("LOAD/WORK LIST",IEN68D24,"LR60")=LR60
        SET LR64=+$G(^LAB(60,LR60,64))
        SET TMGMAP("LOAD/WORK LIST",IEN68D24,"LR64")=LR64
        SET LR64(0)=$G(^LAM(LR64,0))
        SET LR61=+$P(X,"^",2)
        SET TMGMAP("LOAD/WORK LIST",IEN68D24,"LR61")=LR61
        SET LR642=+$P(X,"^",4)
        SET TMGMAP("LOAD/WORK LIST",IEN68D24,"LR64.2")=LR642
        SET LR62=0
        IF LR61 SET LR62=$P(X,"^",5)
        IF 'LR62,LR61 SET LR62=$$GET1^DIQ(61,LR61_",",4.1,"I")
        SET TMGMAP("LOAD/WORK LIST",IEN68D24,"LR62")=LR62
        IF VB WRITE !,$J(IEN68D24,2),?3,$E($P(^LAB(60,LR60,0),"^"),1,25)
        SET TMGMAP("LOAD/WORK LIST",IEN68D24,"TEST NAME")=$E($P(^LAB(60,LR60,0),"^"),1,25)
        SET X=+$P($GET(LR64(0)),"^",2)
        SET TMGMAP("LOAD/WORK LIST",IEN68D24,"ORDER NLT CODE")=$S(X'="":X,1:MISSING)
        IF VB WRITE ?30,$S(X'="":X,1:"<Missing>")
        IF LR61 DO
        . SET X="("_LR61_")"
        . SET X=$E($P(^LAB(61,LR61,0),"^"),1,19-$L(X))_X
        ELSE  S X="<Missing>"
        IF VB WRITE ?50,X
        SET TMGMAP("LOAD/WORK LIST",IEN68D24,"SPECIMEN(IEN)")=$S(X'="":X,1:MISSING)
        NEW LR64D061 SET LR64D061=+$PIECE($GET(^LAB(61,LR61,0)),"^",9)
        SET TMGMAP("LOAD/WORK LIST",IEN68D24,"LR64.061")=$S(LR64D061>0:LR64D061,1:MISSING)
        IF LR64D061>0 DO
        . SET LR64D061(0)=$GET(^LAB(64.061,LR64D061,0))
        . SET TMGMAP("LOAD/WORK LIST",IEN68D24,"LR64.061(0)")=$S(LR64D061(0)'="":LR64D061(0),1:MISSING)
        SET X=$S(LR61:$E($$GET1^DIQ(61,LR61_",","LEDI HL7:HL7 ABBR"),1,14),1:" ")
        IF VB WRITE ?70,$S(X'="":X,1:"<Missing>")
        SET TMGMAP("LOAD/WORK LIST",IEN68D24,"HL7 SPEC")=$S(X'="":X,1:MISSING)
        IF VB WRITE !,?30,$P(LR64(0),"^")
        SET X=$P(LR64(0),"^")
        SET TMGMAP("LOAD/WORK LIST",IEN68D24,"ORDER NLT NAME")=$S(X'="":X,1:MISSING)
        SET X=$S(LR62:$P(^LAB(62,LR62,0),"^"),'LR61:"",1:"<Missing>")
        IF VB WRITE ?50,X
        SET TMGMAP("LOAD/WORK LIST",IEN68D24,"COLLECTION SAMPLE")=$S(X'="":X,1:MISSING)
        SET X=$S(LR642:$P($G(^LAB(64.2,LR642,0)),"^",2),1:"")
        IF VB WRITE ?70,$S(X'="":X,1:"No Mapping"),!
        SET TMGMAP("LOAD/WORK LIST",IEN68D24,"WKLD CODE")=$S(X'="":X,1:MISSING)
        IF LR64<1 DO
        . SET X="Warning - test does not have NATIONAL VA LAB CODE assigned."
        . IF VB W ?3,X,!
        . SET TMGMAP("LOAD/WORK LIST",IEN68D24,"NOTE")=X
        SET TMGMAP("LOAD/WORK LIST",IEN68D24,"ZZ ------------------")=""
        ;
        NEW TMP SET TMP=""
        FOR  SET TMP=$ORDER(TMGMAP("LOAD/WORK LIST",IEN68D24,TMP)) QUIT:(TMP="")  DO
        . IF $GET(TMGMAP("LOAD/WORK LIST",IEN68D24,TMP))'=MISSING QUIT
        .  SET TMGRESULT=-1
        QUIT TMGRESULT
        ;
GETAIMAP(TMGENV,IEN64D41,TMGMAP,VB) ;
        ;"Purpose: Get/check AUTO INSTRUMENT mapping
        ;"Input: TMGENV -- PASS BY REFERENCE.  Lab environment
        ;"           TMGENV("PREFIX") -- e.g. "LMH"
        ;"           TMGENV("IEN 68.2") -- IEN in LOAD/WORK LIST (holds orderable items)
        ;"           TMGENV("IEN 62.4") -- IEN in AUTO INSTRUMENT (holds resultable items)                
        ;"           TMGENV("IEN PROFILE IN 68.2")=1  ;This is really IEN68D23, IN IEN68D2
        ;"       IEN64D41 -- IEN62.41 (CHEM TESTS field in AUTO INSTRUMENT file) 
        ;"       TMGMAP -- PASS BY REFERENCE.  AN OUT PARAMETER. Format:
        ;"         TMGMAP("AUTO INSTRUMENT",IEN64D41,"IENS")=IENS
        ;"         TMGMAP("AUTO INSTRUMENT",IEN64D41,"LR60")=LR60
        ;"         TMGMAP("AUTO INSTRUMENT",IEN64D41,"LR60","NOTE")='#62.4, IENS: xx .01 field -> #60 IEN=xx'
        ;"         TMGMAP("AUTO INSTRUMENT",IEN64D41,"LR64.41")=IEN64D41
        ;"         TMGMAP("AUTO INSTRUMENT",IEN64D41,"LR61")=IEN61
        ;"         TMGMAP("AUTO INSTRUMENT",IEN64D41,"TEST NAME")=LABORATORY TEST NAME (trimmed to 25 chars)
        ;"         TMGMAP("AUTO INSTRUMENT",IEN64D41,"UI TEST CODE")=Code, or '<MISSING>'
        ;"         TMGMAP("AUTO INSTRUMENT",IEN64D41,"UI TEST CODE","NOTE")='#62.4, IENS:xx 6 field'
        ;"         TMGMAP("AUTO INSTRUMENT",IEN64D41,"SPECIMEN(IEN)")=IEN, or '<MISSING>'
        ;"         TMGMAP("AUTO INSTRUMENT",IEN64D41,"DATANAME(IEN)")=$$GET1^DIQ(60,LR60_",",400)
        ;"         TMGMAP("AUTO INSTRUMENT",IEN64D41,"UI TEST CODE NAME")=code, or '<MISSING>'
        ;"         TMGMAP("AUTO INSTRUMENT",#,"HL7 SPEC")=Spec, or '<MISSING>'
        ;"         TMGMAP("AUTO INSTRUMENT",IEN64D41,"LR64")=LR64
        ;"         TMGMAP("AUTO INSTRUMENT",IEN64D41,"LR64","NOTE")='#62.4, IENS:xxx .01 field -> #60: fld 64.1 (RESULT NLT CODE)=xxx'
        ;"         TMGMAP("AUTO INSTRUMENT",#,"LR64")='Warning - test does not have RESULT NLT CODE assigned.'
        ;"         TMGMAP("AUTO INSTRUMENT",IEN64D41,"RESULT NLT CODE")=NLTCODE
        ;"         TMGMAP("AUTO INSTRUMENT",IEN64D41,"RESULT NLT CODE","NOTE")='LR64 -> fld 1'
        ;"         TMGMAP("AUTO INSTRUMENT",IEN64D41,"RESULT NLT NAME")=NLTName
        ;"         TMGMAP("AUTO INSTRUMENT",IEN64D41,"LR64 NOTE")='Warning - RESULT NLT CODE does not match UI TEST CODE.'
        ;"         TMGMAP("AUTO INSTRUMENT",IEN64D41,"ZZ ------------------")=""
        ;"       VB -- VERBOSE (1 for true, 0 for false)  DEFAULT is 0
        ;"Result: 1 if OK, or -1 IF problem (i.e. something missing) 
        ;//TO-DO -- move mapping to LMAPAPI functions.
        ;
        SET VB=+$GET(VB) ;"VERBOSE
        NEW IEN62D4 SET IEN62D4=TMGENV("IEN 62.4")
        NEW X,Y
        NEW LA7EXIT,LA7INTYP,LA7LINE,LA7LINE2,LA7NOW,LA7PAGE,LA7CODE
        NEW LA76248,LR60,LR61,LR62,LR64,LR642
        NEW MISSING SET MISSING="<MISSING>"
        NEW TMGRESULT SET TMGRESULT=1
        NEW IENS SET IENS=IEN64D41_","_IEN62D4_","
        SET TMGMAP("AUTO INSTRUMENT",IEN64D41,"IENS")=IENS
        SET X=^LAB(62.4,IEN62D4,3,IEN64D41,0)
        SET X(2)=$G(^LAB(62.4,IEN62D4,3,IEN64D41,2))
        SET LR60=+X
        SET TMGMAP("AUTO INSTRUMENT",IEN64D41,"LR60")=LR60
        SET TMGMAP("AUTO INSTRUMENT",IEN64D41,"LR60","NOTE")="#62.4, IENS:"_IENS_" .01 field -> #60 IEN="_LR60
        SET TMGMAP("AUTO INSTRUMENT",IEN64D41,"LR64.41")=IEN64D41
        SET LR61=$P(X(2),"^",13)
        SET TMGMAP("AUTO INSTRUMENT",IEN64D41,"LR61")=LR61
        IF VB W !,$J(I,2),?3,$E($P(^LAB(60,LR60,0),"^"),1,25)
        SET TMGMAP("AUTO INSTRUMENT",IEN64D41,"TEST NAME")=$E($P(^LAB(60,LR60,0),"^"),1,25)
        SET LA7CODE=$P(X,"^",6)
        IF VB W ?30,$S(LA7CODE'="":LA7CODE,1:"<Missing>")
        ;"SET TMGMAP("AUTO INSTRUMENT",IEN64D41,"RESULT NLT CODE")=$S(LA7CODE'="":LA7CODE,1:MISSING)
        SET TMGMAP("AUTO INSTRUMENT",IEN64D41,"UI TEST CODE")=$S(LA7CODE'="":LA7CODE,1:MISSING)
        SET TMGMAP("AUTO INSTRUMENT",IEN64D41,"UI TEST CODE","NOTE")="#62.4, IENS:"_IENS_" 6 field"
        I LR61 S X=$P(^LAB(61,LR61,0),"^")_"("_LR61_")"
        E  S X="<Missing>"
        IF VB W ?55,X
        SET TMGMAP("AUTO INSTRUMENT",IEN64D41,"SPECIMEN(IEN)")=$S(X'="":X,1:MISSING)
        S X="("_$P($$GET1^DIQ(60,LR60_",",5),";",2)_")"
        IF VB W !,?3,$E($$GET1^DIQ(60,LR60_",",400),1,25-$L(X))_X
        SET TMGMAP("AUTO INSTRUMENT",IEN64D41,"DATANAME(IEN)")=$$GET1^DIQ(60,LR60_",",400)
        I LA7CODE?5N1"."4N D
        . S Y=$O(^LAM("C",LA7CODE_" ",0))
        . NEW X SET X=MISSING
        . I Y DO
        . . SET X=$P(^LAM(Y,0),"^")
        . . IF VB W ?30,$E(X,1,20)
        . SET TMGMAP("AUTO INSTRUMENT",IEN64D41,"UI TEST CODE NAME")=$S(X'="":X,1:MISSING)
        . ;"SET TMGMAP("AUTO INSTRUMENT",IEN64D41,"RESULT NLT NAME")=$S(X'="":X,1:MISSING)
        S X=$S(LR61:$E($$GET1^DIQ(61,LR61_",","LEDI HL7:HL7 ABBR"),1,14),1:" ")
        IF VB W ?55,$S(X'="":X,1:MISSING),!
        SET TMGMAP("AUTO INSTRUMENT",I,"HL7 SPEC")=$S(X'="":X,1:MISSING)
        SET LR64=+$P($G(^LAB(60,LR60,64)),"^",2)
        SET TMGMAP("AUTO INSTRUMENT",IEN64D41,"LR64")=LR64
        SET TMGMAP("AUTO INSTRUMENT",IEN64D41,"LR64","NOTE")="#62.4, IENS:"_IENS_" .01 field -> #60: fld 64.1 (RESULT NLT CODE)="_LR64
        SET LR64(0)=$G(^LAM(LR64,0))
        I LR64<1 DO
        . IF VB W ?3,"Warning - test does not have RESULT NLT CODE assigned.",!
        . SET TMGMAP("AUTO INSTRUMENT",I,"LR64")="Warning - test does not have RESULT NLT CODE assigned."
        I LR64>0 DO
        . NEW NLTCODE SET NLTCODE=$P(LR64(0),"^",2)
        . SET TMGMAP("AUTO INSTRUMENT",IEN64D41,"RESULT NLT CODE")=NLTCODE
        . SET TMGMAP("AUTO INSTRUMENT",IEN64D41,"RESULT NLT CODE","NOTE")="LR64 -> fld 1"
        . SET TMGMAP("AUTO INSTRUMENT",IEN64D41,"RESULT NLT NAME")=$P(LR64(0),"^",1)
        . IF NLTCODE'=LA7CODE DO
        . . IF VB W ?3,"Warning - RESULT NLT CODE does not match UI TEST CODE."
        . . SET TMGMAP("AUTO INSTRUMENT",IEN64D41,"LR64 NOTE")="Warning - RESULT NLT CODE does not match UI TEST CODE."
        SET TMGMAP("AUTO INSTRUMENT",IEN64D41,"ZZ ------------------")=""
        ;
        NEW TMP SET TMP=""
        FOR  SET TMP=$ORDER(TMGMAP("AUTO INSTRUMENT",IEN64D41,TMP)) QUIT:(TMP="")  DO
        . IF $GET(TMGMAP("AUTO INSTRUMENT",IEN64D41,TMP))[MISSING SET TMGRESULT=-1 QUIT
        . IF $GET(TMGMAP("AUTO INSTRUMENT",IEN64D41,TMP))["Warning -" SET TMGRESULT=-1 QUIT
        ;
        QUIT TMGRESULT
        ;
EXPND60(TEST,ORDER) ;"Expand lab test panel to include component tests 
        ;"Purpose: to expand a LABORATORY TEST to an arry that include component tests (if panel)
        ;"INPUT: TEST -- LABORATORY TEST to expand.  Format: WkLdCode^IEN60 
        ;"       ORDER.  PASS BY REFERENCE.  AN  OUT PARAMETER.  Format:
        ;"          ORDER("WKLD",'WkLdCode^IEN60')=LabName <-- one entry for self, and one for each component test IF panel
        ;"          ORDER("IEN60",IEN60)=WkLdCode
        ;"Result: none.
        NEW IEN60 SET IEN60=$PIECE(TEST,"^",2)
        SET ORDER("WKLD",TEST)=$$GET1^DIQ(60,IEN60,.01)
        SET ORDER("IEN60",IEN60)=$PIECE(TEST,"^",1)
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(^LAB(60,IEN60,2,IDX)) QUIT:(+IDX'>0)  DO
        . NEW SUBTEST,SUBWKLD,SUBIEN60,IEN64
        . SET SUBIEN60=$PIECE($GET(^LAB(60,IEN60,2,IDX,0)),"^",1)
        . SET IEN64=$PIECE($GET(^LAB(60,SUBIEN60,64)),"^",1)
        . IF IEN64>0 SET SUBWKLD=$PIECE($GET(^LAM(IEN64,0)),"^",2)
        . ELSE  SET SUBWKLD="(none)"
        . DO EXPND60(SUBWKLD_"^"_SUBIEN60,.ORDER)
EXPDN   QUIT        
        ;        
GRSLTMAP(TMGENV,WKLD,OUT)  ;"GET RESULT --> REPLACEMENT MAP        
        ;"Purpose: to return array showing any result mapping (e.g. "few"-->"1+") for given test
        ;"Input: TMGENV -- PASS BY REFERENCE.  Lab environment
        ;"           TMGENV("IEN 22720")
        ;"           TMGENV(<other values>)
        ;"       WKLD -- Either IEN64 or a NLT code (#####.####)
        ;"       OUT -- PASS BY REFERENCE, AN OUT PARAMETER.  Prior values killed.  Format:
        ;"          OUT(<HL7VALUE>)=<ReplacementValue>   <-- IF none found, then array is empty. 
        ;"Result: 1 if OK, or -1^Message IF problem.
        NEW TMGRESULT SET TMGRESULT=1
        KILL OUT
        SET WKLD=$GET(WKLD)
        NEW IEN64 
        IF (WKLD?5N1"."4N) DO
        . SET IEN64=$ORDER(^LAM("C",WKLD_" ",0))
        ELSE  DO
        . SET IEN64=+WKLD
        IF IEN64'>0 DO  GOTO GRMDN
        . SET TMGRESULT="-1^Couldn't determine IEN in 64 from '"_WKLD_"'"
        NEW IEN22720 SET IEN22720=TMGENV("IEN 22720")
        NEW IEN22720D1 SET IEN22720D1=0
        FOR  SET IEN22720D1=$ORDER(^TMG(22720,IEN22720,20,"B",IEN64,IEN22720D1)) QUIT:(+IEN22720D1'>0)  DO
        . NEW IEN SET IEN=0
        . FOR  SET IEN=$ORDER(^TMG(22720,IEN22720,20,IEN22720D1,1,IEN)) QUIT:(+IEN'>0)  DO
        . . NEW HL7VAL SET HL7VAL=$PIECE($GET(^TMG(22720,IEN22720,20,IEN22720D1,1,IEN,0)),"^",1) 
        . . NEW NEWVAL SET NEWVAL=$PIECE($GET(^TMG(22720,IEN22720,20,IEN22720D1,1,IEN,0)),"^",2) 
        . . IF (HL7VAL="") QUIT
        . . SET OUT(HL7VAL)=NEWVAL
GRMDN   QUIT TMGRESULT
        ;
TMGLOG(MSG,ARR) ; "//kt added entire function for log messages for debugging
        ;"'@' --> delete entire log
        SET MSG=$GET(MSG)
        IF MSG="@" DO  QUIT  
        . KILL ^TMG("TMP","TMGHL71",$J,"LOG")
        NEW TMGI SET TMGI=$GET(^TMG("TMP","TMGHL71",$J,"LOG"))+1
        SET ^TMG("TMP","TMGHL71",$J,"LOG",TMGI)=MSG
        MERGE ^TMG("TMP","TMGHL71",$J,"LOG",TMGI,"ARR")=ARR
        SET ^TMG("TMP","TMGHL71",$J,"LOG")=TMGI
        QUIT
	    ;	
CMPLTORD(ORDERNUM,TMGHL7MSG) ;"
       ;"Purpose: Set a given order number to complete status
       ;"Input: ORDERNUM - IEN of order in file #100
       ;"       uses TMGHL7MSG in global scope
       ;"Result: "1^SUCCESS" or "-1^Error Message"
       NEW SSN,TMGDFN
       NEW TMGRESULT SET TMGRESULT="1^SUCCESS"       
       ;"2/27/20. TEST THE NOTE AND IF STANDING ORDER DON'T COMPLETE
       DO LNKORD2R(ORDERNUM,.TMGHL7MSG)    ;"ADDED 7/18/23
       NEW STANDING SET STANDING=$$STANDING(ORDERNUM)
       IF STANDING=1 GOTO CODN
       NEW COMPLETEIEN SET COMPLETEIEN=+$ORDER(^ORD(100.01,"B","COMPLETE",0))
       IF COMPLETEIEN'>0 DO  GOTO CODN
       . SET TMGRESULT="-1^COULD NOT LOCATE COMPLETE STATUS IN FILE 100.01"
       NEW ORDDFN SET ORDDFN=+$PIECE($GET(^OR(100,ORDERNUM,0)),"^",2)
       IF ORDDFN'>0 SET TMGRESULT="-1^NO DFN WAS FOUND IN ORDER IN ORDER "_ORDERNUM GOTO CODN
       NEW PIDIDX SET PIDIDX=+$ORDER(TMGHL7MSG("B","PID",0))
       IF PIDIDX'>0 SET TMGRESULT="-1^PID COULD NOT BE LOCATED IN ORDER "_ORDERNUM GOTO CODN
       NEW TMGDFN SET TMGDFN=+$GET(TMGHL7MSG(PIDIDX,4))
       IF TMGDFN=ORDDFN GOTO CHNGE
       SET SSN=+$GET(TMGHL7MSG(PIDIDX,19))
       IF SSN'>0 SET TMGRESULT="-1^COULD NOT FIND SSN IN HL7 ARRAY IN ORDER "_ORDERNUM GOTO CODN
       NEW ORDSSN SET ORDSSN=+$PIECE($GET(^DPT(ORDDFN,0)),"^",9)
       IF ORDSSN'>0 SET TMGRESULT="-1^NO SSN FOUND FOR PATIENT IN ORDER "_ORDERNUM GOTO CODN 
       IF ORDSSN'=SSN SET TMGRESULT="-1^SSN ON HL7 DOES NOT MATCH ORDER SSN IN ORDER "_ORDERNUM GOTO CODN
CHNGE  SET $PIECE(^OR(100,ORDERNUM,3),"^",3)=COMPLETEIEN
       IF $PIECE($GET(^OR(100,ORDERNUM,3)),"^",3)'=COMPLETEIEN DO
       . SET TMGRESULT="-1^STATUS NOT SAVED FOR UNKNOWN REASON."
CODN   IF $P(TMGRESULT,"^",1)="-1" DO
       . NEW ALERTRESULT
       . DO INFRMALT^TMGXQAL(.ALERTRESULT,"150",$P(TMGRESULT,"^",2))
       QUIT TMGRESULT
       ;"
LNKORD2R(ORDERNUM,TMGHL7MSG)  ;"
       ;"Purpose: Link the order number to the results
       NEW TMGFDA,TMGIEN,TMGMSG
       NEW PIDIDX SET PIDIDX=+$ORDER(TMGHL7MSG("B","PID",0))
       IF PIDIDX'>0 SET TMGRESULT="-1^PID COULD NOT BE LOCATED IN ORDER "_ORDERNUM GOTO LNKDN
       NEW TMGDFN SET TMGDFN=+$GET(TMGHL7MSG(PIDIDX,4))
       IF TMGDFN'>0 DO  GOTO LNKDN
       . SET TMGRESULT="-1^NO DFN FOUND"
       NEW TMGRESULT SET TMGRESULT=""
       ;"====== Store metadata =========
       IF $DATA(^TMG(22756,TMGDFN)) GOTO L1
       ;"-- make top level record
       SET TMGFDA(22756,"+1,",.01)=TMGDFN
       SET TMGIEN(1)=TMGDFN
       DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
       IF $DATA(TMGMSG("DIERROR")) DO  GOTO LNKDN
       . SET TMGRESULT="-1^Error: "_$$GETERRST^TMGDEBU2(.TMGMSG)        
L1 ;"-- add subrecord --
       KILL TMGFDA,TMGIEN,TMGMSG
       IF $D(^TMG(22756,TMGDFN,1,"B",ORDERNUM)) GOTO LNKDN  ;"ALREADY REGISTERED
       NEW IENS SET IENS="+1,"_TMGDFN_","
       SET TMGFDA(22756.01,IENS,.01)=ORDERNUM
       ;"
       NEW ORCSEG SET ORCSEG=$ORDER(TMGHL7MSG("B","ORC",0)) IF ORCSEG'>0 DO  GOTO LNKDN
       . SET TMGRESULT="-1^Error: In STOREPDF.TMGLRPD1, ORC segment not found in HL7 message array"
       NEW HL7DT SET HL7DT=$GET(TMGHL7MSG(ORCSEG,9))  ;"ORC collection date  
       IF HL7DT="" DO  GOTO LNKDN
       . SET TMGRESULT="-1^Error: In STOREPDF.TMGLRPD1, Unable to get ORC collection date for HL7 message"  
       NEW FMDT SET FMDT=$$HL72FMDT^TMGHL7U3(HL7DT)
       ;"       
       SET TMGFDA(22756.01,IENS,.02)=FMDT
       DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
       IF $DATA(TMGMSG("DIERROR")) DO  GOTO LNKDN
       . SET TMGRESULT="-1^Error: "_$$GETERRST^TMGDEBU2(.TMGMSG)     
       IF TMGRESULT'="" DO INFRMALT^TMGXQAL(.ALERTRESULT,"150",$P(TMGRESULT,"^",2))
LNKDN ;
       QUIT
       ;"
STANDING(ORDERNUM)  ;"CHECK TO SEE IF ORDER IS STANDING
       NEW TMGRESULT SET TMGRESULT=0
       NEW ORDERTEXT,IDX
       SET IDX=0,ORDERTEXT=""
       FOR  SET IDX=$O(^OR(100,ORDERNUM,8,1,.1,IDX)) QUIT:IDX'>0  DO
       . SET ORDERTEXT=ORDERTEXT_$G(^OR(100,ORDERNUM,8,1,.1,IDX,0))
       IF ORDERTEXT["STANDING ORDER" SET TMGRESULT=1
       QUIT TMGRESULT
                 ;"