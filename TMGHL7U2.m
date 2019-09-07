TMGHL7U2 ;TMG/kst-HL7 transformation utility functions ;3/7/18, 5/22/18, 4/26/19
              ;;1.0;TMG-LIB;**1**;7/21/13
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
 ;"Especially routines regarding reading and writing HL7 MESSAGES
 ;"   into files 772 & 773, or alternatively, the host file system (HFS)
 ;"=======================================================================
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"MKHL7MSG(FNAME,IEN772,IEN773) --take file name, and create a NEW HL7 message records
 ;"KLHL7MSG(IEN773) -- KILL HL7 MESSAGE  (2 records, 1 each in 772, 773)
 ;"LOADHL7(FNAME,ARRAY,MSH) -- LOAD HL7 FILE INTO ARRAY, and MSH header segment
 ;"MKHLMARR(MSGARRAY,MSH,IEN772,IEN773) --Take input message array, and create a NEW HL7 message
 ;"MKHLMAR2(MSGARRAY,IEN772,IEN773) --Take input message array, and create a NEW HL7 message
 ;"STUBHL7M(MSH,IEN772,IEN773) --Create stub records in 772, 773
 ;"FROM772(IEN772,IEN773,MSGARRAY) --FILL MSGARRAY FROM FILES 772 & 773
 ;"FROM772H(IEN772,IEN773,MSGARRAY,MSH) -- FILL MSGARRAY+MSH FROM FILES 772 & 773
 ;"TO772(MSGARRAY,IEN772,IEN773) --PUT MSGARRAY INTO FILES 772 & 773 (PRE-EXISTING RECORDS)
 ;"TO772H(MSGARRAY,MSH,IEN772,IEN773) --PUT MSH+MSGARRAY INTO FILES 772 & 773 (PRE-EXISTING RECORDS)
 ;
 ;"SAVEMSG(TMGTESTMSG) -- save currently loaded HL7 Message to a HFS file
 ;"SAVMSGFM(TMGTESTMSG,IEN772,IEN773) --Save Currently loaded HL7 message to files 772/773
 ;"LOADMSGF(TMGTESTMSG) -- Load an HL7 message from files 772/773
 ;"LOADMSG(TMGTESTMSG) --load in message
 ;"LOADMSG2(TMGTESTMSG) -- Get message by allowing user to pick from HFS
 ;"EDITMSG(TMGTESTMSG) --Edit message array via HFS (Linux) joe text editor.
 ;"VIEWMSG(MSG) -- Display currently loaded HL7 Message.
 ;"VIEWDIFF(OUT,FPNAME1,FPNAME2,PARAMS) -- GET DIFF OUTPUT BETWEEN 2 HL7 MESSAGE FILES
 ;"VWMSGDIF(OUT,MSG1,MSG2,PARAMS) -- GET DIFF OUTPUT BETWEEN 2 HL7 MESSAGE ARRAYS
 ;"SUPPTIME() -- Suppress time of labwork
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"TMGSTUTL, all the HL*, LA* code that the HL processing path normally calls.
 ;"=======================================================================
 ;
MKHL7MSG(FNAME,IEN772,IEN773) ;"MAKE HL7 MESSAGE  (2 records, 1 each in 772, 773)
        ;"Purpose: To take FName, and create a NEW HL7 message, for use by prior parsing system
        ;"Input: FNAME -- Full filepath and name for file to be processed. 
        ;"       IEN772 -- Pass by REFERENCE.  AN OUT PARAMETER.  IEN in 772, HL7 MESSAGE TEXT
        ;"       IEN773 -- Pass by REFERENCE.  AN OUT PARAMETER.  IEN in 773, HL7 MESSAGE HEADER
        ;"Results: 1 if OK, or -1^Error Message
        ;
        NEW TMGRESULT,MSGARRAY,MSH
        SET TMGRESULT=$$LOADHL7(FNAME,.MSGARRAY,.MSH) ;"LOAD FILE INTO ARRAY
        IF TMGRESULT'>0 GOTO M7MDN
        SET TMGRESULT=$$MKHLMARR(.MSGARRAY,MSH,.IEN772,.IEN773)
M7MDN   QUIT TMGRESULT
        ;
KLHL7MSG(IEN773) ;"KILL HL7 MESSAGE  (2 records, 1 each in 772, 773)
        ;"Purpose: Remove/kill records for HL7 message
        ;"Input: IEN773 -- Pass by REFERENCE.  AN OUT PARAMETER.  IEN in 773, HL7 MESSAGE HEADER
        ;"Results: 1 if OK, or -1^File773 Error Message^File772 Error Message
        NEW TMGRESULT SET TMGRESULT=1
        SET IEN773=$GET(IEN773)
        IF +IEN773'>0 DO  GOTO K7MDN
        . SET TMGRESULT="-1^No value supplied for IEN773.  Got '"_IEN773_"'"
        NEW IEN772 SET IEN772=+$PIECE($GET(^HLMA(IEN773,0)),"^",1)
        IF IEN772'>0 DO  GOTO K7MDN
        . SET TMGRESULT="-1^^No linked value for IEN772 found in IEN773 '"_IEN773_"'"
        NEW TMGFDA,TMGMSG
        SET TMGFDA(773,+IEN773_",",.01)="@"
        DO FILE^DIE("E","TMGFDA","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO
        . IF +TMGRESULT=1 SET TMGRESULT=-1
        . SET $PIECE(TMGRESULT,"^",2)="FILE 773: "_$$GETERRST^TMGDEBU2(.TMGMSG)
        KILL TMGFDA,TMGMSG
        SET TMGFDA(772,IEN772_",",.01)="@"
        DO FILE^DIE("E","TMGFDA","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  
        . IF +TMGRESULT=1 SET TMGRESULT=-1
        . SET $PIECE(TMGRESULT,"^",3)="FILE 772: "_$$GETERRST^TMGDEBU2(.TMGMSG)
K7MDN   QUIT TMGRESULT
        ;
LOADHL7(FNAME,ARRAY,MSH) ;"LOAD FILE INTO ARRAY
        ;"Input: FNAME -- filename (with path)
        ;"       ARRAY -- PASS BY REFERENCE, AN OUT PARAMETER.  Format:
        ;"                  ARRAY(#)=<text>
        ;"       MSH   -- PASS BY REFERENCE, AN OUT PARAMETER.  Filled with MSH segment
        ;"Result: 1 if OK, or -1^Error Message
        NEW TMGRESULT,OPTION
        SET OPTION("LINE-TERM")=$CHAR(13)  ;"NOTE: HL7 messages have just #13 as line terminator. 
        SET OPTION("OVERFLOW")=1 ;"Overflow portion is concat'd to the orig line (making length>255)
        SET TMGRESULT=$$HFS2ARFP^TMGIOUT3(FNAME,"ARRAY",.OPTION)
        IF $$LISTCT^TMGMISC2("ARRAY")=1 DO
        . SET LINE=$GET(ARRAY(1)) QUIT:(LINE="")!($EXTRACT(LINE,1,3)'="MSH")
        . NEW DIVCH SET DIVCH=$EXTRACT(LINE,4)
        . IF LINE'["PID"_DIVCH QUIT
        . KILL OPTION("LINE-TERM")  ;"NOTE: Try again without special line divider    
        . SET TMGRESULT=$$HFS2ARFP^TMGIOUT3(FNAME,"ARRAY",.OPTION)
        NEW LINENUM SET LINENUM=$ORDER(ARRAY(0))
        SET MSH=$GET(ARRAY(LINENUM)) 
        NEW BOM SET BOM=$CHAR(239)_$CHAR(187)_$CHAR(191)
        NEW PREFIX SET PREFIX=$EXTRACT(MSH,1,3)
        IF PREFIX=BOM DO
        . SET MSH=$EXTRACT(MSH,4,$LENGTH(MSH))
        . SET ARRAY(LINENUM)=MSH
        IF MSH'["MSH" DO              
        . SET TMGRESULT="-1^'MSH' not found on first line.  Got: '"_MSH_"'"
LDHL7DN QUIT TMGRESULT        
        ;
STUBHL7M(MSH,IEN772,IEN773) ;"Create stub records in 772, 773 
        ;"Input: MSH -- the message header segment. 
        ;"       IEN772 -- Pass by REFERENCE.  AN OUT PARAMETER.  IEN in 772, HL7 MESSAGE TEXT
        ;"       IEN773 -- Pass by REFERENCE.  AN OUT PARAMETER.  IEN in 773, HL7 MESSAGE HEADER
        ;"Results: 1 if OK, or -1^Error Message
        NEW MSGARRAY SET MSGARRAY(2)="Stub text.  To be overwritten later..."
        QUIT $$MKHLMARR(.MSGARRAY,.MSH,.IEN772,.IEN773)
        ;                 
MKHLMARR(MSGARRAY,MSH,IEN772,IEN773) ;"MAKE HL7 MESSAGE FROM ARRAY  (2 records, 1 each in 772, 773)
        ;"Purpose: To take input message array, and create a NEW HL7 MESSAGE TEXT, for use by parsing system
        ;"Input: MSGARRAY --  PASS BY REFERENCE, Array holding text of full HL7 message.  Format:
        ;"                  MSGARRAY(#)=<text>  **<-- shouldn't contain MSH segment (I think)
        ;"       MSH -- HL7 message header
        ;"       IEN772 -- Pass by REFERENCE.  AN OUT PARAMETER.  IEN in 772, HL7 MESSAGE TEXT
        ;"       IEN773 -- Pass by REFERENCE.  AN OUT PARAMETER.  IEN in 773, HL7 MESSAGE HEADER
        ;"GLOBALLY SCOPED VARIABLES USED: INFO("IEN101")
        ;"                                INFO("IEN771")    
        ;"                                INFO("IEN771.2")  
        ;"                                INFO("IEN779.001")
        ;"                                INFO("HL7 PURGE DT") <-- OPTIONAL, default is "T+7@0800" 
        ;"Results: 1 if OK, or -1^Error Message
        NEW TMGRESULT SET TMGRESULT=0
        SET MSH=$GET(MSH) IF MSH="" DO  GOTO M7MADN 
        . SET TMGRESULT="-1^A valid MSH segment not presented to MKHLMARR^TMGHL73"
        NEW INFO SET TMGRESULT=$$MSH2IENA(.MSH,.INFO) ;"MSH HEADER TO IEN INFO ARRAY.  May modify MSH
        NEW IDX SET IDX=$ORDER(MSGARRAY(""))
        NEW LINE1 SET LINE1=$GET(MSGARRAY(IDX))
        IF $EXTRACT(LINE1,1,3)="MSH",LINE1'=MSH DO
        . SET MSGARRAY(IDX)=MSH
        IF TMGRESULT'>0 GOTO M7MADN
        NEW NOW SET NOW=$$FMTE^XLFDT($$NOW^XLFDT,"5")
        NEW MSGID SET MSGID=$TRANSLATE($H,",","")
        NEW PURGEDT SET PURGEDT=$GET(INFO("HL7 PURGE DT"),"T+7@0800")    
        NEW TMGFDA,TMGIEN,TMGMSG                      
        ;"SETUP RECORD IN HL7 MESSAGE TEXT FILE. (#772)
        SET TMGFDA(772,"+1,",.01)=NOW                  ;"DATE
        SET TMGFDA(772,"+1,",2)="`"_INFO("IEN771")     ;"(was 'LA7V HOST PG')    ;SERVER APPLICATION
        SET TMGFDA(772,"+1,",2.02)=PURGEDT             ;"FAST PURGE DT/TM
        SET TMGFDA(772,"+1,",4)="INCOMING"             ;"TRANSMISSION TYPE
        SET TMGFDA(772,"+1,",6)=MSGID                  ;"MESSAGE ID
        SET TMGFDA(772,"+1,",9)="IMMEDIATE"            ;"PRIORITY
        SET TMGFDA(772,"+1,",10)="`"_INFO("IEN101")    ;"(was 'LA7V Process Results from PathGroup' ;RELATED EVENT PROTOCOL
        SET TMGFDA(772,"+1,",14)="SINGLE MESSAGE"      ;"MESSAGE TYPE
        SET TMGFDA(772,"+1,",20)="AWAITING PROCESSING" ;"STATUS
        SET TMGFDA(772,"+1,",101)=1                    ;"NO. OF EVENTS IN MESSAGE
        DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO M7MADN
        . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        SET IEN772=+$GET(TMGIEN(1))
        IF IEN772'>0 DO  GOTO M7MADN
        . SET TMGRESULT="-1^Unable to locate record of newly added HL7 message."
        DO WP^DIE(772,IEN772_",",200,"K","MSGARRAY","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO M7MADN
        . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)        
        ;"SETUP RECORD IN HL7 MESSAGE ADMINISTRATION
        KILL TMGFDA,TMGIEN,TMGMSG            
        SET TMGFDA(773,"+1,",.01)="`"_IEN772           ;"DATE/TIME ENTERED
        SET TMGFDA(773,"+1,",2)=MSGID                  ;"MESSAGE ID
        SET TMGFDA(773,"+1,",2.02)=PURGEDT             ;"FAST PURGE DT/TM
        SET TMGFDA(773,"+1,",3)="INCOMING"             ;"TRANSMISSION TYPE
        SET TMGFDA(773,"+1,",4)="IMMEDIATE"            ;"PRIORITY
        SET TMGFDA(773,"+1,",8)="`"_INFO("IEN101")     ;"(was '`4390' LA7V Process Results from PathGroup)
        SET TMGFDA(773,"+1,",13)="`"_INFO("IEN771")    ;" (was 'LA7V HOST PG') ;SENDING APPLICATION
        SET TMGFDA(773,"+1,",15)="`"_INFO("IEN771.2")  ;"(was 'ORL') ;MESSAGE TYPE
        SET TMGFDA(773,"+1,",16)="`"_INFO("IEN779.001") ;"(was 'R01') ;"EVENT TYPE
        SET TMGFDA(773,"+1,",20)="AWAITING PROCESSING" ;"STATUS
        DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO M7MADN
        . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        SET IEN773=+$GET(TMGIEN(1))
        IF IEN773'>0 DO  GOTO M7MADN
        . SET TMGRESULT="-1^Unable to locate record of newly added HL7 message administration record"
        KILL MSGARRAY 
        SET MSGARRAY(1)=MSH
        DO WP^DIE(773,IEN773_",",200,"K","MSGARRAY","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO M7MDN
        . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)        
M7MADN  QUIT TMGRESULT
        ;
MKHLMAR2(MSGARRAY,IEN772,IEN773)  ;"Take input message array, and create a NEW HL7 message
        ;"Purpose: To take input message array, and create a NEW HL7 MESSAGE TEXT, for use by parsing system
        ;"Input: MSGARRAY --  PASS BY REFERENCE, Array holding text of full HL7 message.  Format:
        ;"            MSGARRAY(#)=<text> , **including MSH as first line**
        ;"       IEN772 -- Pass by REFERENCE.  AN OUT PARAMETER.  IEN in 772, HL7 MESSAGE TEXT
        ;"       IEN773 -- Pass by REFERENCE.  AN OUT PARAMETER.  IEN in 773, HL7 MESSAGE HEADER
        ;"GLOBALLY SCOPED VARIABLES USED: INFO("IEN101")
        ;"                                INFO("IEN771")    
        ;"                                INFO("IEN771.2")  
        ;"                                INFO("IEN779.001")
        ;"                                INFO("HL7 PURGE DT") <-- OPTIONAL, default is "T+7@0800" 
        ;"Results: 1 if OK, or -1^Error Message
        NEW IDX SET IDX=+$ORDER(MSGARRAY(""))  
        NEW MSH SET MSH=$GET(MSGARRAY(IDX))    
        NEW ARR MERGE ARR=MSGARRAY KILL ARR(IDX)
        SET TMGRESULT=$$MKHLMARR(.ARR,MSH,.IEN772,.IEN773)  ;"MAKE HL7 MESSAGE FROM ARRAY  (2 records, 1 each in 772, 773)
        QUIT TMGRESULT
        ;   
XFRMFACILITY(FACILITY) ;"
        ;"Purpose: transform sending facility before any processing is done.
        ;"NOTE: With Epic, I have started getting messages from hospital all through
        ;"     the network.  I think all messages will use same code names etc,
        ;"     so I will map them all to a generic hospital name.
        ;"Input: FACILITY -- an IN AND OUT PARAMETER
        ;"RESULT: 1 if modified, 0 if no change.  
        NEW RESULT SET RESULT=0
        IF (FACILITY="GCHW")!(FACILITY="BRMC")!(FACILITY="BHMA") DO
        . SET FACILITY="BALLADNETWORK"
        . SET RESULT=1
        QUIT RESULT
        ;
MSH2IENA(MSH,INFO) ;"MSH HEADER TO IEN INFO ARRAY
        ;"Input: MSH -- string containing the MSH segment
        ;"       INFO -- PASS BY REFERENCE.  AN OUT PARAMETER.  
        ;"Output: INFO filled.  Format:
        ;"           INFO("IEN62.4")=IEN62D4
        ;"           INFO("IEN62.4","NAME")=name
        ;"           INFO("IEN68.2")=IEN68D2
        ;"           INFO("IEN68.2","NAME")=name
        ;"           INFO("IEN771")=IEN IN 771 (HL7 APPLICATION PARAMETER). e.g. 186
        ;"           INFO("IEN771","NAME")=Name.  e.g. 'LA7V HOST LMH'
        ;"           INFO("IEN771.2")=IEN IN 771.2 (HL7 MESSAGE TYPE)  e.g. 3
        ;"           INFO("IEN771.2","NAME")=Name.  e.g. 'ORU'
        ;"           INFO("IEN771.5")=+Y
        ;"           INFO("IEN771.5","NAME")=name        
        ;"           INFO("IEN779.001")=IEN IN 779.001 (HL7 EVENT TYPE CODE.  e.g. 46
        ;"           INFO("IEN779.001","NAME")=Name.  e.g. 'R01'
        ;"           INFO("IEN101")=IEN101 (PROTOCOL) file  e.g. 4269
        ;"           INFO("IEN101","NAME")=Name.  e.g. 'LA7V Receive Results from LMH'
        ;"           INFO("PREFIX")=PREFIX
        ;"           INFO("IEN22720")=IEN22720
        ;"           INFO("IEN22720","NAME")=name
        ;"Result: 1^OK, or -1^Error Message
        ;
        NEW TMGRESULT SET TMGRESULT="1^OK"
        NEW TMGU DO SUTMGU^TMGHL7X2(.TMGU,$EXTRACT(MSH,4),$EXTRACT(MSH,5,8))
        NEW FACILITY SET FACILITY=$PIECE(MSH,TMGU(1),4)
        IF $$XFRMFACILITY(.FACILITY) DO
        . SET $PIECE(MSH,TMGU(1),4)=FACILITY
        NEW X,Y,DIC,D SET DIC=771,DIC(0)="",D="TMGFACILITY",X=FACILITY  ;"Setup search on just custom XRef TMGFACILITY in file 771
        DO IX^DIC KILL D 
        IF +Y'>0 DO  GOTO MH2ADN
        . SET TMGRESULT="-1^Unable to find unique match for HL7 facility '"_FACILITY_"' in cross reference TMGFACILITY in file 771."
        SET INFO("IEN771")=+Y
        SET INFO("IEN771","NAME")=$PIECE($GET(^HL(771,+Y,0)),"^",1)
        NEW IEN771 SET IEN771=+Y
        ;
        NEW MSGTYPE SET MSGTYPE=$PIECE(MSH,TMGU(1),9)
        NEW TRANMTYPE SET TRANMTYPE=$PIECE(MSGTYPE,TMGU(2),1)
        SET DIC=771.2,DIC(0)="M",X=TRANMTYPE DO ^DIC
        IF +Y'>0 DO  GOTO MH2ADN
        . SET TMGRESULT="-1^Unable to find unique match for HL7 TRANSACTION MESSAGE TYPE '"_TRANMTYPE_"' file 771.2"
        SET INFO("IEN771.2")=+Y
        SET INFO("IEN771.2","NAME")=$PIECE($GET(^HL(771.2,+Y,0)),"^",1)
        NEW IEN771D2 SET IEN771D2=+Y
        ;                
        NEW EVNTMTYPE SET EVNTMTYPE=$PIECE(MSGTYPE,TMGU(2),2)
        SET DIC=779.001,DIC(0)="M",X=EVNTMTYPE DO ^DIC
        IF +Y'>0 DO  GOTO MH2ADN
        . SET TMGRESULT="-1^Unable to find unique match for HL7 TRANSACTION EVENT TYPE '"_EVNTMTYPE_"' file 779.001"
        SET INFO("IEN779.001")=+Y
        SET INFO("IEN779.001","NAME")=$PIECE($GET(^HL(779.001,+Y,0)),"^",1)
        NEW IEN779D001 SET IEN779D001=+Y
        ;
        NEW VERID SET VERID=$PIECE(MSH,TMGU(1),12)        
        SET DIC=771.5,DIC(0)="M",X=VERID DO ^DIC
        IF +Y'>0 DO  GOTO MH2ADN
        . SET TMGRESULT="-1^Unable to find unique match for HL7 VERSION ID '"_VERID_"' file 771.5"
        SET INFO("IEN771.5")=+Y
        SET INFO("IEN771.5","NAME")=$PIECE($GET(^HL(771.5,+Y,0)),"^",1)
        NEW IEN771D5 SET IEN771D5=+Y
        ;
        NEW IEN101 SET IEN101=$ORDER(^ORD(101,"AHL1",IEN771,IEN771D2,IEN779D001,IEN771D5,0))
        IF IEN101'>0 DO  GOTO MH2ADN
        . SET TMGRESULT="-1^Unable to find an HL7 PROTOCOL (file 101) entry matching info: "
        . SET TMGRESULT=TMGRESULT_"Application='"_INFO("IEN771","NAME")_"', "
        . SET TMGRESULT=TMGRESULT_"Message Type='"_INFO("IEN771.2","NAME")_"', "
        . SET TMGRESULT=TMGRESULT_"Event Type='"_INFO("IEN779.001","NAME")_"', "
        . SET TMGRESULT=TMGRESULT_"Version ID='"_INFO("IEN771.5","NAME")_"."
        . SET TMGRESULT=TMGRESULT_"using cross reference 'AHL1'."
        SET INFO("IEN101")=IEN101
        SET INFO("IEN101","NAME")=$PIECE($GET(^ORD(101,IEN101,0)),"^",1)
        ;
        NEW IEN62D4 SET IEN62D4=$ORDER(^LAB(62.4,"ATMGPROTOCOL",IEN101,0))
        IF IEN62D4'>0 SET IEN62D4=$ORDER(^LAB(62.4,"ATMGPROTOCOL2",IEN101,0))
        IF IEN62D4'>0 DO  GOTO MH2ADN
        . SET TMGRESULT="-1^Can't find an AUTO INSTRUMENT (file #62.4) record with a "
        . SET TMGRESULT=TMGRESULT_"field TMG LINKED PROTOCOL (#22702) matching IEN '"_IEN101_"' in cross reference ATMGPROTOCOL."
        SET INFO("IEN62.4")=IEN62D4
        SET INFO("IEN62.4","NAME")=$PIECE($GET(^LAB(62.4,IEN62D4,0)),"^",1)
        ;
        NEW PREFIX SET PREFIX=$PIECE($GET(^LAB(62.4,IEN62D4,"TMG")),"^",1)
        IF PREFIX="" DO  GOTO MH2ADN
        . SET TMGRESULT="-1^Lab prefix code is not defined in AUTO INSTRUMENT (file #62.4) record '"_IEN62D4_"'."
        SET INFO("PREFIX")=PREFIX
        ;
        NEW IEN22720 SET IEN22720=$ORDER(^TMG(22720,"AAI",IEN62D4,0))
        IF IEN22720'>0 DO  GOTO MH2ADN
        . SET TMGRESULT="-1^Unable to find TMG HL7 MESSAGE TRANSFORM SETTINGS (file #22720) with "
        . SET TMGRESULT=TMGRESULT_"LINKED AUTO INSTRUMENT (fld #1.5) of '"_IEN62D4_"'."
        SET INFO("IEN22720")=IEN22720
        SET INFO("IEN22720","NAME")=$PIECE($GET(^TMG(22720,IEN22720,0)),"^",1)
        ;
        NEW IEN68D2 SET IEN68D2=$PIECE($GET(^TMG(22720,IEN22720,0)),"^",4)
        IF IEN68D2'>0 DO  GOTO MH2ADN
        . SET TMGRESULT="-1^Record #"_IEN22720_" in file TMG HL7 MESSGE TRANSFORM SETTINGS "
        . SET TMGRESULT=TMGRESULT_" doesn't have a value for field LINKED LOAD/WORK LIST (#1.6)."
        SET INFO("IEN68.2")=IEN68D2
        SET INFO("IEN68.2","NAME")=$PIECE($GET(^LRO(68.2,IEN68D2,0)),"^",1)
        ;
MH2ADN  QUIT TMGRESULT
        ;
FROM772(IEN772,IEN773,MSGARRAY) ;"FILL MSGARRAY FROM FILES 772 & 773
        ;"Input: IEN772 -- IEN in file 772.  Record must already exist.
        ;"       IEN773 -- IEN in file 773.  Record must already exist.
        ;"       MSGARRAY.  PASS BY REFERENCE.  AN OUT PARAMETER.  Array with message.  Format:
        ;"          MSGARRAY(1)=MSH segment
        ;"          MSGARRAY(#)=<line of text>
        ;"       MSH -- string representing MSH segment. 
        ;"Results: 1 if OK, or -1^Error IF problem.
        NEW MSH,TEMPARR,TMGRESULT
        KILL MSGARRAY
        SET TMGRESULT=$$FROM772H(IEN772,IEN773,.TEMPARR,.MSH)
        IF TMGRESULT<0 GOTO F772DN
        SET MSGARRAY(1)=MSH
        NEW AI SET AI=2
        NEW IDX SET IDX=0 
        FOR  SET IDX=$ORDER(TEMPARR(IDX)) QUIT:(IDX="")  DO
        . SET MSGARRAY(AI)=$GET(TEMPARR(IDX)),AI=AI+1
F772DN  QUIT TMGRESULT        
        
FROM772H(IEN772,IEN773,MSGARRAY,MSH) ;"FILL MSGARRAY FROM FILES 772 & 773
        ;"Input: IEN772 -- IEN in file 772.  Record must already exist.
        ;"       IEN773 -- IEN in file 773.  Record must already exist.
        ;"       MSGARRAY.  PASS BY REFERENCE.  AN OUT PARAMETER.  Array with message.  Format:
        ;"          MSGARRAY(#)=<line of text>  <-- doesn't include MSH segment. 
        ;"       MSH -- PASS BY REFERENCE.  AN OUT PARAMETER.  Filled with MSH segment. 
        ;"Results: 1 if OK, or -1^Error IF problem.
        NEW TMGRESULT SET TMGRESULT=1
        SET IEN772=$GET(IEN772) IF $DATA(^HL(772,IEN772))'>0 DO  GOTO F772HDN
        . SET TMGRESULT="-1^Invalid IEN 772.  Got '"_IEN772_"'"
        SET IEN773=$GET(IEN773) IF $DATA(^HLMA(IEN773))'>0 DO  GOTO F772HDN
        . SET TMGRESULT="-1^Invalid IEN 773.  Got '"_IEN773_"'"
        SET MSH=$GET(^HLMA(IEN773,"MSH",1,0))
        NEW REF SET REF=$NAME(^HL(772,IEN772,"IN"))
        NEW ARRAY
        DO WP2ARRAY^TMGSTUT2(REF,"ARRAY")
        NEW MSHFOUND SET MSHFOUND=0
        NEW IDX SET IDX=""
        FOR  SET IDX=$ORDER(ARRAY(IDX)) DO  QUIT:(MSHFOUND=1)!(IDX="")  
        . NEW LINE SET LINE=$GET(ARRAY(IDX))
        . SET MSHFOUND=($EXTRACT(LINE,1,3)="MSH") 
        . KILL ARRAY(IDX)
        NEW IDX SET IDX=0 FOR  SET IDX=$ORDER(ARRAY(IDX)) QUIT:(IDX="")  DO
        . IF $GET(ARRAY(IDX))="" KILL ARRAY(IDX)
        KILL MSGARRAY MERGE MSGARRAY=ARRAY        
F772HDN QUIT TMGRESULT        
        ;
TO772(MSGARRAY,IEN772,IEN773) ;"PUT MSGARRAY INTO FILES 772 & 773 (PRE-EXISTING RECORDS)
        ;"Input: MSGARRAY.  PASS BY REFERENCE.  Array with message.  Format:
        ;"          MSGARRAY(1)=MSH segment
        ;"          MSGARRAY(#)=<line of text>  
        ;"       MSH -- string representing MSH segment. 
        ;"       IEN772 -- IEN in file 772.  Record must already exist.
        ;"       IEN773 -- IEN in file 773.  Record must already exist.
        ;"Results: 1 if OK, or -1^Error IF problem.
        NEW MSH SET MSH=$GET(MSGARRAY(1))
        NEW ARR MERGE ARR=MSGARRAY KILL ARR(1)
        QUIT $$TO772H(.ARR,MSH,.IEN772,.IEN773)
        ;
TO772H(MSGARRAY,MSH,IEN772,IEN773) ;"PUT MSH+MSGARRAY INTO FILES 772 & 773 (PRE-EXISTING RECORDS)
        ;"Input: MSGARRAY.  PASS BY REFERENCE.  Array with message.  Format:
        ;"          MSGARRAY(#)=<line of text>  <-- doesn't include the MSH segment.
        ;"       MSH -- string representing MSH segment. 
        ;"       IEN772 -- IEN in file 772.  Record must already exist.
        ;"       IEN773 -- IEN in file 773.  Record must already exist.
        ;"Results: 1 if OK, or -1^Error IF problem.
        NEW TMGRESULT SET TMGRESULT=1
        IF $DATA(MSGARRAY)'>0 DO  GOTO T772HDN
        . SET TMGRESULT="-1^MSGARRAY array empty."
        SET MSH=$GET(MSH) IF MSH="" DO  GOTO T772HDN
        . SET TMGRESULT="-1^MSH string empty."
        SET IEN772=$GET(IEN772) IF $DATA(^HL(772,IEN772))'>0 DO  GOTO T772HDN
        . SET TMGRESULT="-1^Invalid IEN 772.  Got '"_IEN772_"'"
        SET IEN773=$GET(IEN773) IF $DATA(^HLMA(IEN773))'>0 DO  GOTO T772HDN
        . SET TMGRESULT="-1^Invalid IEN 773.  Got '"_IEN773_"'"
        SET ^HLMA(IEN773,"MSH",1,0)=MSH
        NEW REF SET REF=$NAME(^HL(772,IEN772,"IN"))
        DO ARRAY2WP^TMGSTUT2("MSGARRAY",REF)        
T772HDN QUIT TMGRESULT
        ;
SAVEMSG(TMGTESTMSG) ;
        ;"Purpose: To save currently loaded HL7 Message to a HFS file
        NEW FPATH,FNAME
        IF $$GETFNAME^TMGIOUTL("Please Select Filename for Write HL7 Message","/tmp/","","/",.FPATH,.FNAME)="" QUIT
        NEW TMGRESULT SET TMGRESULT=$$SAVEMSGACTUAL(FPATH,FNAME,.TMGTESTMSG)
        WRITE "File WRITE ",$SELECT(TMGRESULT=1:"succeeded",1:"FAILED"),"."
        ;"KILL ^TMG("TMP","TMGHL71",$J,"TMGTESTMSG") 
        ;"MERGE ^TMG("TMP","TMGHL71",$J,"TMGTESTMSG")=TMGTESTMSG
        ;"WRITE "File WRITE "
        ;"IF $$GTF^%ZISH($NAME(^TMG("TMP","TMGHL71",$J,"TMGTESTMSG",1)),5,FPATH,FNAME)=1 DO
        ;". WRITE "succeeded.",!
        ;"ELSE  DO
        ;". WRITE "failed.",!
        DO PRESS2GO^TMGUSRI2
SMGDN   ;"KILL ^TMG("TMP","TMGHL71",$J,"TMGTESTMSG")
        QUIT
        ;   
SAVEMSGACTUAL(FPATH,FNAME,MSG) ;
        KILL ^TMG("TMP","TMGHL71",$J,"TMGTESTMSG") 
        MERGE ^TMG("TMP","TMGHL71",$J,"TMGTESTMSG")=MSG
        NEW TMGRESULT
        IF $$GTF^%ZISH($NAME(^TMG("TMP","TMGHL71",$J,"TMGTESTMSG",1)),5,FPATH,FNAME)=1 DO
        . SET TMGRESULT=1
        ELSE  DO
        . SET TMGRESULT="-1^HFS write failed"
        KILL ^TMG("TMP","TMGHL71",$J,"TMGTESTMSG")
        QUIT TMGRESULT
        ;
SAVMSGFM(TMGTESTMSG,IEN772,IEN773) ;"Save Currently loaded HL7 message to files 772/773
        ;"Purpose: Save currently loaded HL7 Message to a Fileman HL7 MESSAGE ... file
        ;"TO-DO MOVE TO TMGHL7US
        NEW TMGRESULT
        SET TMGRESULT=$$TO772^TMGHL7U2(.TMGTESTMSG,IEN772,IEN773)
        IF TMGRESULT<0 DO
        . WRITE $PIECE(TMGRESULT,"^",2)
        . DO PRESS2GO^TMGUSRI2
        QUIT        
        ;        
LOADMSGF(TMGTESTMSG) ;"Load an HL7 message from files 772/773
        ;"
        NEW DIC,X,Y
        SET DIC=773,DIC(0)="MAEQ",DIC("A")="ENTER DATE TIME OF HL7 MESSAGE: "
        DO ^DIC WRITE !
        IF Y'>0 GOTO LMGFDN        
        NEW IEN773 SET IEN773=+Y
        NEW IEN772 SET IEN772=+$PIECE($GET(^HLMA(IEN773,0)),"^",1)
        IF IEN772'>0 DO  GOTO LMGFDN
        . WRITE "Unable to find record from HL7 MESSAGE TEXT (772) linked to",!
        . WRITE "record #",IEN773," in file HL7 MESSAGE ADMINISTRATION",!
        . DO PRESS2GO^TMGUSRI2
        . KILL IEN772,IEN773  
        NEW TMGRESULT SET TMGRESULT=$$FROM772^TMGHL7U2(IEN772,IEN773,.TMGTESTMSG)
        IF TMGRESULT<0 DO  GOTO LMGFDN
        . WRITE $PIECE(TMGRESULT,"^",2),!
        . DO PRESS2GO^TMGUSRI2
LMGFDN  QUIT
        ;        
LOADMSG(TMGTESTMSG) ;"Get message by allowing user to paste one into editor
        ;"Purpose: load in message
        NEW TMGRESULT,TMGINSTRUCT
        IF $DATA(TMGTESTMSG) DO
        . NEW % SET %=2
        . WRITE "Clear current test message and load another"
        . DO YN^DICN
        . IF %=1 KILL TMGTESTMSG
        IF $DATA(TMGTESTMSG) GOTO LMDN
        KILL TMGHL7MSG
        SET TMGINSTRUCT="Delete these lines, and replace them with the HL7 message to work on."
        SET TMGTESTMSG(1)=TMGINSTRUCT
        SET TMGTESTMSG(2)="Then exit editor to continue."
        DO EDITARR^TMGEDIT($NAME(TMGTESTMSG),"joe")
        IF ($GET(TMGTESTMSG(1))=TMGINSTRUCT)!($GET(TMGTESTMSG(1))="") DO
        . WRITE "Sorry.  No HL7 Message.",!
        . KILL TMGTESTMSG
        . DO PRESS2GO^TMGUSRI2
LMDN    QUIT
        ;
LOADMSG2(TMGTESTMSG) ;"Get message by allowing user to pick from HFS
        ;"Purpose: load in message
        ;"Output: TMGTESTMSG(#)=<line of text>
        NEW TMGRESULT,TMGINSTRUCT
        IF $DATA(TMGTESTMSG) DO
        . NEW % SET %=2
        . WRITE "Clear current test message and load another"
        . DO YN^DICN
        . IF %=1 KILL TMGTESTMSG
        IF $DATA(TMGTESTMSG) GOTO LM2DN
        KILL TMGHL7MSG
        NEW FNAME,PATH,NAME 
        SET FNAME=$$GETFNAME^TMGIOUTL("Select Sample HL7 Message file",,,,.PATH,.NAME)
        IF FNAME="" DO  GOTO LM2DN
        . WRITE "No HL7 Message Selected.",!
        . KILL TMGTESTMSG
        . DO PRESS2GO^TMGUSRI2
        NEW OPTION SET OPTION("OVERFLOW")=1
        ;"SET OPTION("LINE-TERM")=$CHAR(10)        
        SET OPTION("LINE-TERM")=$CHAR(13)   ;"NOTE: HL7 messages have just #13 as line terminator. 
        DO HFS2ARR^TMGIOUT3(PATH,NAME,"TMGTESTMSG",.OPTION)
        IF '$DATA(TMGTESTMSG) DO
        . WRITE "Sorry.  No HL7 Message.",!
        . KILL TMGTESTMSG
        . DO PRESS2GO^TMGUSRI2
LM2DN   QUIT
        ;                
EDITMSG(TMGTESTMSG) ;"Edit message array via HFS (Linux) joe text editor. 
        ;"Input:
        NEW TMGRESULT SET TMGRESULT=$$EditArray^TMGKERNL(.TMGTESTMSG,"joe")
        IF TMGRESULT'>0 DO
        . WRITE "Edit of currently loaded HL7 message was not successful.",!
        . DO PRESS2GO^TMGUSRI2
        QUIT
        ;        
VIEWMSG0(MSG) ;
        ;"Purpose: Display currently loaded HL7 Message.
        ;"Input: MSG -- Pass by reference.
        SET IOS=$GET(IOS,60)
        NEW I SET I=""
        FOR  SET I=$ORDER(MSG(I)) QUIT:(I="")  DO
        . NEW S SET S=$GET(MSG(I))
        . NEW S2 SET S2=""
        . NEW FIRSTLINE SET FIRSTLINE=1
        . FOR  QUIT:S=""  DO
        . . NEW MARGIN SET MARGIN=$SELECT(FIRSTLINE:IOS,1:IOS-3)
        . . SET S2=$EXTRACT(S,MARGIN+1,999)
        . . SET S=$EXTRACT(S,1,MARGIN)
        . . IF 'FIRSTLINE WRITE "   "
        . . WRITE S,!
        . . SET S=S2
        . . SET FIRSTLINE=0
        DO PRESS2GO^TMGUSRI2
        QUIT
        ;        
VIEWMSG(MSG) ;
        ;"Purpose: Display currently loaded HL7 Message.
        ;"Input: MSG -- Pass by reference.
        NEW %ZIS
        SET %ZIS("A")="Enter Output Device: "
        SET %ZIS("B")="HOME"
        DO ^%ZIS  ;"standard device call
        IF POP DO  QUIT
        . DO SHOWERR^TMGDEBU2(,"Error opening output.  Aborting.")
        USE IO
        NEW I SET I=""
        FOR  SET I=$ORDER(MSG(I)) QUIT:(I="")  DO
        . NEW S SET S=$GET(MSG(I))
        . WRITE S,!
        ;" Close the output device
        DO ^%ZISC        
        DO PRESS2GO^TMGUSRI2
        QUIT
        ;        
VIEWDIFF(OUT,FPNAME1,FPNAME2,PARAMS) ;"GET DIFF OUTPUT BETWEEN 2 HL7 MESSAGE FILES
        ;"INPUT: OUT -- PASS BY REFERENCE.  AN OUT PARAMETER. Format:
        ;"          OUT(#)=line of text from diff comparison. 
        ;"       FPNAME1 -- path+filename of first HL7 file to compare
        ;"       FPNAME2 -- path+filename of first HL7 file to compare
        ;"       PARAMS: (optional)  Contains command options, all in one long string.
        ;"          e.g. "--ignore-case"   or "-Z"   See Linux documentation for details
        NEW MSG1,MSG2
        NEW RESULT
        SET RESULT=$$LOADHL7(FPNAME1,.MSG1) ;"LOAD FILE INTO ARRAY
        IF RESULT'>0 GOTO VWDIFDN
        SET RESULT=$$LOADHL7(FPNAME2,.MSG2) ;"LOAD FILE INTO ARRAY
        IF RESULT'>0 GOTO VWDIFDN
        SET RESULT=$$VWMSGDIF(.MSG1,.MSG2,.PARAMS)
VWDIFDN QUIT RESULT        
        ;
VIEWDIF2(FPNAME1,FPNAME2,PARAMS) ;"Use vimdiff on 2 HL7 MESSAGE FILES
        ;"INPUT: FPNAME1 -- path+filename of first HL7 file to compare
        ;"       FPNAME2 -- path+filename of first HL7 file to compare
        ;"       PARAMS: (optional)  Contains command options, all in one long string.
        ;"          e.g. "-o"   or "-O"   See Linux documentation for details
        NEW MSG1,MSG2
        NEW RESULT
        SET RESULT=$$LOADHL7(FPNAME1,.MSG1) ;"LOAD FILE INTO ARRAY
        IF RESULT'>0 GOTO VWDF2DN
        SET RESULT=$$LOADHL7(FPNAME2,.MSG2) ;"LOAD FILE INTO ARRAY
        IF RESULT'>0 GOTO VWDF2DN
        SET RESULT=$$VWMDIF2(.MSG1,.MSG2,.PARAMS)
VWDF2DN QUIT RESULT        
        ;
VWMSGDIF(OUT,MSG1,MSG2,PARAMS)  ;"GET DIFF OUTPUT BETWEEN 2 HL7 MESSAGE ARRAYS
        ;"INPUT: OUT -- PASS BY REFERENCE.  AN OUT PARAMETER. Format:
        ;"          OUT(#)=line of text from diff comparison. 
        ;"       MSG1 -- PASS BY REFERENCE.  First array to compare.  Format:
        ;"          MSG1(#)=line of HL7 message
        ;"       MSG2 -- PASS BY REFERENCE.  First array to compare.  Format:
        ;"          MSG2(#)=line of HL7 message
        ;"       PARAMS: (optional)  Contains command options, all in one long string.
        ;"          e.g. "--ignore-case"   or "-Z"   See Linux documentation for details
        NEW RESULT SET RESULT=$$DIFF^TMGKERNL(.OUT,.MSG1,.MSG2,.PARAMS)
        QUIT RESULT
        ;
VWMDIF2(MSG1,MSG2,PARAMS)  ;"USE vimdiff BETWEEN 2 HL7 MESSAGE ARRAYS
        ;"INPUT: MSG1 -- PASS BY REFERENCE.  First array to compare.  Format:
        ;"          MSG1(#)=line of HL7 message
        ;"       MSG2 -- PASS BY REFERENCE.  First array to compare.  Format:
        ;"          MSG2(#)=line of HL7 message
        ;"       PARAMS: (optional)  Contains command options, all in one long string.
        ;"          e.g. "-o"   or "-O"   See Linux documentation for details
        NEW RESULT SET RESULT=$$VIMADIFF^TMGKERNL(.MSG1,.MSG2,.PARAMS)
        QUIT RESULT
        ;
SUPPTIME() ;
        ;"Purpose: Determine if a lab test should have the
        ;"         time suppressed
        QUIT 0  ;"//kt changed 2/11/15  (Eddie, Sorry for waffling...)
        ;"