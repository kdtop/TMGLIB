TMGHL7X3 ;TMG/kst-HL7 transformation engine utilities ;10/20/15
              ;;1.0;TMG-LIB;**1**;03/28/11
 ;
 ;"TMG HL7 TRANSFORMATION UTILITY FUNCTIONS
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright^c^ 10/20/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"Especially code to parse HL7 message into usable array, and also compile
 ;"  from that array back into flat-file style message.
 ;"=======================================================================
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;                       
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"
 ;"=======================================================================
GETDD(OUT)  ;"Get 'Data dictionary' for HL7 fields
  ;"Input: OUT -- PASS BY REFERENCE, AN OUT PARAMETER
  NEW I,DONE SET DONE=0
  FOR I=1:1 QUIT:DONE  DO
  . NEW LINE SET LINE=$TEXT(HL7DD+I^TMGHL7X3)
  . SET LINE=$PIECE(LINE,";;""",2)
  . IF LINE=";" QUIT
  . IF LINE="<END>" SET DONE=1 QUIT
  . NEW TEMP SET TEMP=$PIECE(LINE,"^",1)
  . NEW SEG SET SEG=$PIECE(TEMP,"-",1)
  . NEW SEGN SET SEGN=+$PIECE(TEMP,"-",2)
  . NEW NAME SET NAME=$PIECE(LINE,"^",2)
  . NEW CODE SET CODE=$PIECE(LINE,"^",3)
  . NEW OPT SET OPT=$PIECE(LINE,"^",4) SET OPT=(OPT="optional")
  . NEW REPEAT SET REPEAT=$PIECE(LINE,"^",5) SET REPEAT=(REPEAT="repeating")
  . SET OUT(SEG,SEGN)=NAME
  . SET OUT(SEG,SEGN,0)=CODE_"^"_OPT_"^"_REPEAT
  QUIT
  ;
HL7DD  ;
  ;;"MSH-1^Field Separator^ST^
  ;;"MSH-2^Encoding Characters^ST^
  ;;"MSH-3^Sending Application^HD^optional
  ;;"MSH-4^Sending Facility^HD^optional
  ;;"MSH-5^Receiving Application^HD^optional
  ;;"MSH-6^Receiving Facility^HD^optional
  ;;"MSH-7^Date/Time Of Message^TS^optional
  ;;"MSH-8^Security^ST^optional
  ;;"MSH-9^Message Type^MSG^
  ;;"MSH-10^Message Control ID^ST^
  ;;"MSH-11^Processing ID^PT^
  ;;"MSH-12^Version ID^VID^
  ;;"MSH-13^Sequence Number^NM^optional
  ;;"MSH-14^Continuation Pointer^ST^optional
  ;;"MSH-15^Accept Acknowledgment Type^ID^optional
  ;;"MSH-16^Application Acknowledgment Type^ID^optional
  ;;"MSH-17^Country Code^ID^optional
  ;;"MSH-18^Character Set^ID^optional^repeating
  ;;"MSH-19^Principal Language Of Message^CE^optional
  ;;"MSH-20^Alternate Character Set Handling Scheme^ID^optional
  ;;";
  ;;"PID-1^Set ID - PID^SI^optional
  ;;"PID-2^Patient ID^CX^optional
  ;;"PID-3^Patient Identifier List^CX^ repeating
  ;;"PID-4^Alternate Patient ID - PID^CX^optional^repeating
  ;;"PID-5^Patient Name^XPN^ repeating
  ;;"PID-6^Mother’s Maiden Name^XPN^optional^repeating  
  ;;"PID-7^Date/Time Of Birth^TS^optional
  ;;"PID-8^Sex^IS^optional
  ;;"PID-9^Patient Alias^XPN^optional^repeating
  ;;"PID-10^Race^CE^optional^repeating
  ;;"PID-11^Patient Address^XAD^optional^repeating
  ;;"PID-12^County Code^IS^optional
  ;;"PID-13^Phone Number - Home^XTN^optional^repeating
  ;;"PID-14^Phone Number - Business^XTN^optional^repeating
  ;;"PID-15^Primary Language^CE^optional
  ;;"PID-16^Marital Status^CE^optional
  ;;"PID-17^Religion^CE^optional
  ;;"PID-18^Patient Account Number^CX^optional
  ;;"PID-19^SSN Number - Patient^ST^optional
  ;;"PID-20^Driver's License Number - Patient^DLN^optional
  ;;"PID-21^Mother's Identifier^CX^optional^repeating
  ;;"PID-22^Ethnic Group^CE^optional^repeating
  ;;"PID-23^Birth Place^ST^optional
  ;;"PID-24^Multiple Birth Indicator^ID^optional
  ;;"PID-25^Birth Order^NM^optional
  ;;"PID-26^Citizenship^CE^optional^repeating
  ;;"PID-27^Veterans Military Status^CE^optional
  ;;"PID-28^Nationality^CE^optional
  ;;"PID-29^Patient Death Date and Time^TS^optional
  ;;"PID-30^Patient Death Indicator^ID^optional
  ;;";
  ;;"NTE-1^Set ID - NTE^SI^optional
  ;;"NTE-2^Source of Comment^ID^optional
  ;;"NTE-3^Comment^FT^optional^repeating
  ;;"NTE-4^Comment Type^CE^optional
  ;;";
  ;;"ORC-1^Order Control^ID^
  ;;"ORC-2^Placer Order Number^EI^optional
  ;;"ORC-3^Filler Order Number^EI^optional
  ;;"ORC-4^Placer Group Number^EI^optional
  ;;"ORC-5^Order Status^ID^optional
  ;;"ORC-6^Response Flag^ID^optional
  ;;"ORC-7^Quantity/Timing^TQ^optional^repeating
  ;;"ORC-8^Parent Order^EIP^optional
  ;;"ORC-9^Date/Time of Transaction^TS^optional
  ;;"ORC-10^Entered By^XCN^optional^repeating
  ;;"ORC-11^Verified By^XCN^optional^repeating
  ;;"ORC-12^Ordering Provider^XCN^optional^repeating
  ;;"ORC-13^Enterer's Location^PL^optional
  ;;"ORC-14^Call Back Phone Number^XTN^optional^repeating
  ;;"ORC-15^Order Effective Date/Time^TS^optional
  ;;"ORC-16^Order Control Code Reason^CE^optional
  ;;"ORC-17^Entering Organization^CE^optional
  ;;"ORC-18^Entering Device^CE^optional
  ;;"ORC-19^Action By^XCN^optional^repeating
  ;;"ORC-20^Advanced Beneficiary Notice Code^CE^optional
  ;;"ORC-21^Ordering Facility Name^XON^optional^repeating
  ;;"ORC-22^Ordering Facility Address^XAD^optional^repeating
  ;;"ORC-23^Ordering Facility Phone Number^XTN^optional^repeating
  ;;"ORC-24^Ordering Provider Address^XAD^optional^repeating
  ;;"ORC-25^Order Status Modifier^CWE^optional
  ;;"ORC-26^Advanced Beneficiary Notice Override Reason^CWE^optional
  ;;"ORC-27^Filler's Expected Availability Date/Time^TS^optional
  ;;"ORC-28^Confidentiality Code^CWE^optional
  ;;"ORC-29^Order Type^CWE^optional
  ;;"ORC-30^Enterer Authorization Mode^CNE^optional
  ;;";
  ;;"OBR-1^Set ID - OBR^SI^optional
  ;;"OBR-2^Placer Order Number^EI^optional
  ;;"OBR-3^Filler Order Number^EI^optional
  ;;"OBR-4^Universal Service ID^CE^
  ;;"OBR-5^Priority-OBR^ID^optional
  ;;"OBR-6^Requested Date/time^TS^optional
  ;;"OBR-7^Observation Date/Time^TS^optional
  ;;"OBR-8^Observation End Date/Time^TS^optional
  ;;"OBR-9^Collection Volume^CQ^optional
  ;;"OBR-10^Collector Identifier^XCN^optional^repeating
  ;;"OBR-11^Specimen Action Code^ID^optional
  ;;"OBR-12^Danger Code^CE^optional
  ;;"OBR-13^Relevant Clinical Info.^ST^optional
  ;;"OBR-14^Specimen Received Date/Time^TS^optional
  ;;"OBR-15^Specimen Source^SPS^optional
  ;;"OBR-16^Ordering Provider^XCN^optional^repeating
  ;;"OBR-17^Order Callback Phone Number^XTN^optional^repeating
  ;;"OBR-18^Placer Field 1^ST^optional
  ;;"OBR-19^Placer Field 2^ST^optional
  ;;"OBR-20^Filler Field 1^ST^optional
  ;;"OBR-21^Filler Field 2^ST^optional
  ;;"OBR-22^Results Rpt/Status Chng - Date/Time^TS^optional
  ;;"OBR-23^Charge to Practice^MOC^optional
  ;;"OBR-24^Diagnostic Serv Sect ID^ID^optional
  ;;"OBR-25^Result Status^ID^optional
  ;;"OBR-26^Parent Result^PRL^optional
  ;;"OBR-27^Quantity/Timing^TQ^optional^repeating
  ;;"OBR-28^Result Copies To^XCN^optional^repeating
  ;;"OBR-29^Parent Number^EIP^optional
  ;;"OBR-30^Transportation Mode^ID^optional
  ;;"OBR-31^Reason for Study^CE^optional^repeating
  ;;"OBR-32^Principal Result Interpreter^NDL^optional
  ;;"OBR-33^Assistant Result Interpreter^NDL^optional^repeating
  ;;"OBR-34^Technician^NDL^optional^repeating
  ;;"OBR-35^Transcriptionist^NDL^optional^repeating
  ;;"OBR-36^Scheduled Date/Time^TS^optional
  ;;"OBR-37^Number of Sample Containers^NM^optional
  ;;"OBR-38^Transport Logistics of Collected Sample^CE^optional^repeating
  ;;"OBR-39^Collector’s Comment^CE^optional^repeating
  ;;"OBR-40^Transport Arrangement Responsibility^CE^optional
  ;;"OBR-41^Transport Arranged^ID^optional
  ;;"OBR-42^Escort Required^ID^optional
  ;;"OBR-43^Planned Patient Transport Comment^CE^optional^repeating
  ;;"OBR-44^Procedure Code^CE^optional
  ;;"OBR-45^Procedure Code Modifier^CE^optional^repeating
  ;;";
  ;;"OBX-1^Set ID - OBX^SI^optional
  ;;"OBX-2^Value Type^ID^
  ;;"OBX-3^Observation Identifier^CE^
  ;;"OBX-4^Observation Sub-ID^ST^optional
  ;;"OBX-5^Observation Value^Varies^optional^repeating
  ;;"OBX-6^Units^CE^optional
  ;;"OBX-7^References Range^ST^optional
  ;;"OBX-8^Abnormal Flags^ID^optional^repeating
  ;;"OBX-9^Probability^NM^optional
  ;;"OBX-10^Nature of Abnormal Test^ID^optional
  ;;"OBX-11^Observ Result Status^ID^
  ;;"OBX-12^Date Last Obs Normal Values^TS^optional
  ;;"OBX-13^User Defined Access Checks^ST^optional
  ;;"OBX-14^Date/Time of the Observation^TS^optional
  ;;"OBX-15^Producer's ID^CE^optional
  ;;"OBX-16^Responsible Observer^XCN^optional
  ;;"OBX-17^Observation Method^CE^optional^repeating
  ;;"<END>
  ;
HL7SEGS ; HL7 Segment Headers
  ;;"ABS
  ;;"ACC
  ;;"ADD
  ;;"ADJ
  ;;"AFF
  ;;"AIG
  ;;"AIL
  ;;"AIP
  ;;"AIS
  ;;"ALI
  ;;"APR
  ;;"ARQ
  ;;"ARV
  ;;"AUT
  ;;"BHS
  ;;"BLC
  ;;"BLG
  ;;"BPO
  ;;"BPX
  ;;"BTS
  ;;"BTX
  ;;"CDM
  ;;"CER
  ;;"CM0
  ;;"CM1
  ;;"CM2
  ;;"CNS
  ;;"CON
  ;;"CSP
  ;;"CSR
  ;;"CSS
  ;;"CTD
  ;;"CT1
  ;;"DB1
  ;;"DG1
  ;;"DM1
  ;;"DRG
  ;;"DSC
  ;;"DSP
  ;;"ECD
  ;;"ECR
  ;;"EDU
  ;;"EQP
  ;;"EQU
  ;;"ERR
  ;;"EVN
  ;;"FAC
  ;;"FHS
  ;;"FTI
  ;;"FTS
  ;;"GOL
  ;;"GP1
  ;;"GP2
  ;;"GTI
  ;;"IAM
  ;;"IIM
  ;;"ILT
  ;;"IN1
  ;;"IN2
  ;;"IN3
  ;;"INV
  ;;"IPC
  ;;"IPR
  ;;"ISD
  ;;"ITM
  ;;"IVC
  ;;"IVT
  ;;"LAN
  ;;"LCC
  ;;"LCH
  ;;"LDP
  ;;"LOC
  ;;"LRL
  ;;"MFA
  ;;"MFE
  ;;"MFI
  ;;"MRG
  ;;"MSA
  ;;"MSH
  ;;"NCK
  ;;"NDS
  ;;"NK1
  ;;"NPU
  ;;"NSC
  ;;"NST
  ;;"NTE
  ;;"OBR
  ;;"OBX
  ;;"ODS
  ;;"ODT
  ;;"OM1
  ;;"OM2
  ;;"OM3
  ;;"OM4
  ;;"OM5
  ;;"OM6
  ;;"OM7
  ;;"ORC
  ;;"ORG
  ;;"OVR
  ;;"PCE
  ;;"PCR
  ;;"PD1
  ;;"PDA
  ;;"PDC
  ;;"PEO
  ;;"PES
  ;;"PID
  ;;"PKG
  ;;"PMT
  ;;"PR1
  ;;"PRA
  ;;"PRB
  ;;"PRC
  ;;"PRD
  ;;"PSG
  ;;"PSH
  ;;"PSL
  ;;"PSS
  ;;"PTH
  ;;"PV1
  ;;"PV2
  ;;"PYE
  ;;"QAK
  ;;"QID
  ;;"QPD
  ;;"QRD
  ;;"QRF
  ;;"QRI
  ;;"RCP
  ;;"RDF
  ;;"RDT
  ;;"REL
  ;;"RF1
  ;;"RFI
  ;;"RGS
  ;;"RMI
  ;;"ROL
  ;;"RQ1
  ;;"RQD
  ;;"RXA
  ;;"RXB
  ;;"RXC
  ;;"RXD
  ;;"RXE
  ;;"RXG
  ;;"RXO
  ;;"RXR
  ;;"SAC
  ;;"SCD
  ;;"SCH
  ;;"SCP
  ;;"SDD
  ;;"SFT
  ;;"SID
  ;;"SLT
  ;;"SPM
  ;;"STF
  ;;"STZ
  ;;"TCC
  ;;"TCD
  ;;"TQ1
  ;;"TQ2
  ;;"TXA
  ;;"UAC
  ;;"UB1
  ;;"UB2
  ;;"URD
  ;;"URS
  ;;"VAR
  ;;"VND
  ;;"ZL7
  ;;"<DONE>
  ;
  QUIT         
  ;