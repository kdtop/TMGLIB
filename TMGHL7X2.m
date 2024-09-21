TMGHL7X2 ;TMG/kst-HL7 transformation engine processing ;10/21/15, 9/2/24
              ;;1.0;TMG-LIB;**1**;03/28/11
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
 ;"Especially code to parse HL7 message into usable array, and also compile
 ;"  from that array back into flat-file style message.
 ;"=======================================================================
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"PARSEMSG(IEN772,IEN773,FS,ECH,TMGHL7MSG) -- take pointers to files 772,773, and create an array with parsed elements
 ;"DEPRECIATED -- PARSMSG2(TMGENV,TMGTESTMSG,TMGHL7MSG,TMGU) --parse HL7 Message into a usable array.
 ;"COMPILEM(IEN772,IEN773,TMGHL7MSG) -- TAKE completed TMGHL7MSG arran, and put back into files 772,773
 ;"TESTCOMP(TMGHL7MSG,OUTARRAY) -- take completed TMGHL7MSG array, and compile into test array
 ;"FLATNARR(TMGHL7MSG,ARRAY,TMGU) -- Reverse of PRSEARRY.  
 ;"PRSEARRY(ARRAY,TMGHL7MSG,TMGU) -- take an ARRAY, containing HL7 message, and parse to TMGHL7MSG
 ;"REFRESHM(TMGHL7MSG,TMGU,SEGN,FLDN,COMPN,SCMPN) --refresh the array after changes are made to one part.
 ;"$$GETPCE(TMGHL7MSG,SEG,FLDN,COMPN,SCMPN) --return a requested piece parsed TMGHL7MSG array
 ;"SETPCE(VALUE,TMGHL7MSG,TMGU,SEG,FLDN,COMPN,SCMPN) --Set a requested piece parsed TMGHL7MSG array
 ;"ENSURSEG(TMGHL7MSG,SEG,TMGU,SEGN) -- Ensure segment exists in message
 ;"GETSEG(OUT,TMGHL7MSG,SEG) -- return array of parsed SEG from TMGHL7MSG
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;"PRSESEG(SEGN,S,TMGHL7MSG,TMGU) ;
 ;"PRSEFLD(SEGN,FLDNUM,S,TMGHL7MSG,TMGU) ;
 ;"PRSECOMP(SEGN,FLDNUM,COMPNUM,FIELD,TMGHL7MSG,TMGU) ;
 ;"PRSESCMP(SEGN,FLDNUM,COMPNUM,SUBCOMPN,COMPVAL,TMGHL7MSG,TMGU) ;
 ;
 ;"BUBLSCMP(SEGN,FLDNUM,COMPNUM,SUBCOMPN,TMGHL7MSG,TMGU) --Compile changes to sub-component into parent component
 ;"BUBLCOMP(SEGN,FLDNUM,COMPNUM,TMGHL7MSG,TMGU) -- Compile changes to component into parent field
 ;"BUBLFLD(SEGN,FLDNUM,TMGHL7MSG,TMGU) -- Compile changes to field into parent segment
 ;"
 ;"SUTMGU(TMGU,FS,ECH) --Setup array with divisor characters.
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"TMGSTUTL, all the HL*, LA* code that the HL processing path normally calls.
 ;"=======================================================================
 ;
PARSEMSG(IEN22720,IEN772,IEN773,TMGHL7MSG,TMGU) ;
        ;"Purpose: to take pointers to files 772,773, and create an array with parsed elements
        ;"       NOTE: I think that this is just supposed to be separating the parts
        ;"             from HL7 format into a tree structure.  It is not supposed
        ;"             to transform (fix) the various message elements. 
        ;"Input: IEN22720 -- IEN in TMG HL7 MESSAGE TRANSFORM SETTINGS file
        ;"       IEN772 -- IEN in HL7 MESSAGE TEXT file
        ;"       IEN773 -- IEN in HL7 MESSAGE ADMINISTRATION file. 
        ;"       TMGHL7MSG -- an OUT PARAMETER.  PASS BY REFERENCE.
        ;"       TMGU -- An array with divisor characters. PASS BY REFERENCE.  Filled IF passed in empty
        ;"       TMGHL7MSG -- Pass by REFERENCE.  AN OUT PARAMETER.  Filled with parsed message
        ;"NOTE: -- HL7 messages are divided up into fields, components, and subcomponents
        ;"      --For example below, I will call the top level field F1, the components as F2,
        ;"      and the sub-components as F3
        ;"      --Empty sub-fields are left null (not created)
        ;"Output: TMGHL7MSG(<Index#>)=<Segment name>  e.g. MSH or OBR etc.
        ;"        TMGHL7MSG(<Index#>,1)=Value of 1st field in segment
        ;"        TMGHL7MSG(<Index#>,1,1)=Value of 1st sub-field in the 1st field in segment
        ;"        TMGHL7MSG(<Index#>,2)=Value of 2nd field in segment
        ;"        TMGHL7MSG(<Index#>,F1,F2,F3)=<Value>
        ;"        TMGHL7MSG(<Index#>,"SEG")=Segment type, e.g. MSH, OBR, OBX etc. 
        ;"        TMGHL7MSG("B",<SegmentName>,<Index#>)=""
        ;"        TMGHL7MSG("PO",<Processing Order>,<Index#>)=""
        ;"        TMGHL7MSG("ECH")=<encoding chars>
        ;"        TMGHL7MSG("FS")=<field separator character>
        ;"Example: IF the 8th segment in the HL7 message was:
        ;"         OBX|3|ST|UBL^BLOOD,URINE^L||NEGATIVE||NEGATIVE|N||A^S|F|||1234|ML^MAIN LAB^L|||
        ;"    It would parse to:
        ;"        TMGHL7MSG(8)="OBX|3|ST|UBL^BLOOD,URINE^L||NEGATIVE||NEGATIVE|N||A^S|F|||1234|ML^MAIN LAB^L|||"
        ;"        TMGHL7MSG(8,"SEG")="OBX"
        ;"        TMGHL7MSG(8,1)="3"
        ;"        TMGHL7MSG(8,2)="ST"
        ;"        TMGHL7MSG(8,3)="UBL^BLOOD,URINE^L"
        ;"        TMGHL7MSG(8,3,1)="UBL"
        ;"        TMGHL7MSG(8,3,2)="BLOOD"
        ;"        TMGHL7MSG(8,3,3)="L"
        ;"        TMGHL7MSG(8,4)=""
        ;"        TMGHL7MSG(8,5)="NEGATIVE"
        ;"        TMGHL7MSG(8,6)=""
        ;"        TMGHL7MSG(8,7)="NEGATIVE"
        ;"        TMGHL7MSG(8,8)="N"
        ;"        TMGHL7MSG(8,9)=""
        ;"        TMGHL7MSG(8,10)="A^S"
        ;"        TMGHL7MSG(8,10,1)="A"
        ;"        TMGHL7MSG(8,10,2)="S"
        ;"        TMGHL7MSG(8,11)="F"
        ;"        TMGHL7MSG(8,12)=""
        ;"        TMGHL7MSG(8,13)=""
        ;"        TMGHL7MSG(8,14)="1234"
        ;"        TMGHL7MSG(8,15)="ML^MAIN LAB^L"
        ;"        TMGHL7MSG(8,15,1)="M"
        ;"        TMGHL7MSG(8,15,2)="MAIN LAB"
        ;"        TMGHL7MSG(8,15,3)="L"
        ;"        TMGHL7MSG(8,16)=""
        ;"        TMGHL7MSG(8,17)=""
        ;"        TMGHL7MSG(8,18)=""
        ;"        TMGHL7MSG("B","OBX",8)=""   <-- E.g.  Will act as index of OBX segments.
        ;"        TMGHL7MSG("PO",#)=NodeNumber  "PO"= Processing Order.
        ;"        TMGHL7MSG("ECH")=<encoding chars>
        ;"        TMGHL7MSG("FS")=<field separator character>
        ;"Results: 1 if OK, or -1^Error Message
        NEW TMGRESULT SET TMGRESULT=1
        NEW ARRAY,WPI,TEMP,LINE
        KILL TMGHL7MSG
        IF $DATA(^HLMA(IEN773,"MSH",1,0))=0 DO  GOTO PMDN
        . SET TMGRESULT="-1^IN PARSEMSG.TMGHL7X2:  No data found at "_$NAME(^HLMA(IEN773,"MSH",1,0))
        IF $DATA(^HL(772,IEN772,"IN"))=0 DO  GOTO PMDN
        . SET TMGRESULT="-1^IN PARSEMSG.TMGHL7X2: No data found at "_$NAME(^HL(772,IEN772,"IN"))
        SET WPI=1
        SET ARRAY(WPI)=$GET(^HLMA(IEN773,"MSH",1,0))
        MERGE TEMP=^HL(772,IEN772,"IN")
        SET LINE=0
        FOR  SET LINE=$ORDER(TEMP(LINE)) QUIT:(+LINE'>0)  DO
        . NEW S SET S=$GET(TEMP(LINE,0)) IF S="" QUIT
        . SET WPI=WPI+1,ARRAY(WPI)=S
        SET TMGRESULT=$$PRSEARRY(IEN22720,.ARRAY,.TMGHL7MSG,.TMGU) ;
PMDN    QUIT TMGRESULT
        ;
PARSMSG2(TMGENV,TMGTESTMSG,TMGHL7MSG,TMGU) ;" (INTERACTIVE WITH USER)   -- DEPRECIATED   
        ;"Purpose: To parse HL7 Message into a usable array.
        ;"         ALSO -- this launches an interactive process to fix problems, if found. 
        ;"Note: uses globally-scoped vars" TMGLABPREFIX, IEN68D2, IEN62D4
        ;"NOTE: I think that this function does more than just separating the parts
        ;"             from HL7 format into a tree structure.  The SETMAPS()
        ;"             function does mapping of tests names to VistA tests 
        KILL TMGHL7MSG
        ;"//kt 6/1/20 -- not used(?) -- SET AUTOFIX=+$GET(AUTOFIX)
        NEW TMGRESULT SET TMGRESULT=$$PRSEARRY^TMGHL7X2(,.TMGTESTMSG,.TMGHL7MSG,.TMGU)
        IF +TMGRESULT<0 GOTO PH7DN
        SET TMGRESULT=$$SETMAPS^TMGHL70B(.TMGENV,.TMGHL7MSG)
PH7DN   QUIT TMGRESULT    
        ;
COMPILEM(IEN772,IEN773,TMGHL7MSG,MODE,TMGRESIDUAL) ;
        ;"Purpose: take completed TMGHL7MSG array, and put back into files 772,773
        ;"Input: IEN772 -- IEN in HL7 MESSAGE TEXT file
        ;"       IEN773 -- IEN in HL7 MESSAGE ADMINISTRATION file. 
        ;"      TMGHL7MSG -- Pass by REFERENCE. The message array to be compiled
        ;"      MODE -- 1. Put entire TMGHL7MSG array into files 772,773. TMGRESIDUAL returned null.  <-- DEFAULT
        ;"              2. Put only results with same specimen type into 772,773.  TMGRESIDUAL returns remainder.
        ;"              3. Put only 1st result into 772,773. TMGRESIDUAL returns remainder.
        ;"      TMGRESIDUAL -- Pass by REFERENCE, AN OUT PARAMETER. 
        ;"                    The parts of the message that could not be included
        ;"                    info compiled message.  This will store results with 
        ;"                    E.g. IF message contains BLOOD and SERUM results, then
        ;"                    only ONE type of specimen will be returned, and all
        ;"                    all others will be put into TMGRESIDUAL.
        ;"             Prior values killed prior in this function
        ;"Result: 1 if OK, or -1^Message IF problem
        NEW TMGRESULT,MSH,ARRAY
        SET TMGRESULT=$$COMP2ARR(.TMGHL7MSG,.ARRAY,.MSH,.MODE,.TMGRESIDUAL)
        IF +TMGRESULT<0 GOTO CPLMDN
        SET TMGRESULT=$$TO772H^TMGHL7U2(.ARRAY,.MSH,IEN772,IEN773)
CPLMDN  QUIT TMGRESULT
        ;
COMP2ARR(TMGHL7MSG,ARRAY,MSH,MODE,TMGRESIDUAL) ;
        ;"Purpose: take completed TMGHL7MSG array, and convert back to simple array
        ;"Input:  TMGHL7MSG -- Pass by REFERENCE.  The message to reassemble
        ;"        ARRAY -- PASS BY REFERENCE, AN OUT PARAMETER.  Format:
        ;"          ARRAY(#)=<line of text comprising HL7 message>  <-- doesn't include MSH
        ;"        MSH -- HL7 message header -- PASS BY REFERENCE, AN OUT PARAMETER
        ;"        MODE -- 1. Put entire TMGHL7MSG array into files 772,773. TMGRESIDUAL returned null. <-- DEFAULT
        ;"                2. Put only results with same specimen type into 772,773.  TMGRESIDUAL returns remainder.
        ;"                3. Put only 1st result into 772,773. TMGRESIDUAL returns remainder.
        ;"        TMGRESIDUAL -- Pass by REFERENCE, AN OUT PARAMETER. 
        ;"                    The parts of the message that could not be included
        ;"                    info compiled message.  This will store results with 
        ;"                    E.g. IF message contains BLOOD and SERUM results, then
        ;"                    only ONE type of specimen will be returned, and all
        ;"                    all others will be put into TMGRESIDUAL.
        ;"             Prior values killed prior in this function
        ;"Result: 1 if OK, or -1^Message IF problem
        NEW TMGRESULT SET TMGRESULT=1
        NEW SEG,SEGN,MSHSEGN,FIRSTSPEC,SEGTYPE 
        KILL ARRAY,MSH,TMGRESIDUAL
        SET MSHSEGN=$ORDER(TMGHL7MSG("B","MSH",0))
        IF MSHSEGN'>0 DO  GOTO CP2ARDN
        . SET TMGRESULT="-1^IN COMPILEM.TMGHL7X2: Can't find 'MSH' header in TMGHL7MSG"
        SET MSH=$GET(TMGHL7MSG(MSHSEGN))
        NEW DIVCH SET DIVCH=$EXTRACT(MSH,4)
        SET MODE=+$GET(MODE) IF MODE'>0 SET MODE=1
        NEW IDX SET IDX=+$ORDER(TMGHL7MSG("OBR-SPEC","ORDER",0))
        NEW SPEC SET SPEC=$GET(TMGHL7MSG("OBR-SPEC","ORDER",IDX)) ;"used when MODE=2
        IF (IDX'>0)!(SPEC="") DO  GOTO CP2ARDN
        . SET TMGRESULT="-1^Unable to find any OBR specimen in node TMGHL7MSG(""OBR-SPEC"",""ORDER"")"
        NEW MSGSPLIT SET MSGSPLIT=0
        NEW TOPDONE SET TOPDONE=0
        NEW OBRFOUND SET OBRFOUND=0  ;"Number of OBR's found.
        NEW SKIPARESULT SET SKIPARESULT=0
        NEW AI SET AI=1
        SET SEGN=0
        FOR  SET SEGN=$ORDER(TMGHL7MSG(SEGN)) QUIT:(+SEGN'>0)  DO
        . IF ('TOPDONE)!(SKIPARESULT) DO
        . . IF MODE>1 MERGE TMGRESIDUAL(SEGN)=TMGHL7MSG(SEGN)
        . IF SEGN=MSHSEGN QUIT
        . SET SEG=$GET(TMGHL7MSG(SEGN))
        . SET SEGTYPE=$PIECE(SEG,DIVCH,1)
        . IF SEGTYPE="OBR" DO        
        . . SET TOPDONE=1
        . . SET OBRFOUND=OBRFOUND+1
        . . IF MODE=3,OBRFOUND>1 SET SKIPARESULT=1 QUIT
        . . ;"Does this OBR matches allowed specimen for entire message?
        . . IF MODE=2 SET SKIPARESULT=($DATA(TMGHL7MSG("OBR-SPEC",SPEC,SEGN))'>0) 
        . IF SKIPARESULT DO  QUIT
        . . SET MSGSPLIT=1
        . . IF MODE>1 MERGE TMGRESIDUAL(SEGN)=TMGHL7MSG(SEGN)
        . ELSE  IF TOPDONE KILL TMGRESIDUAL(SEGN)
        . SET ARRAY(AI)=SEG,AI=AI+1
        . SET ARRAY(AI)="",AI=AI+1  ;"separate lines with blank lines as original format
        ;
        ;"Set up XREF nodes for TMGRESIDUAL IF to be used.
        ;"NOTE: IF additional XREF nodes are created in TMGHL7MSG, then code must be added below to handle them
        IF MSGSPLIT DO   
        . NEW XREF FOR XREF="B","OBR-SPEC" DO
        . . SET SEGTYPE=""
        . . FOR  SET SEGTYPE=$ORDER(TMGHL7MSG(XREF,SEGTYPE)) QUIT:(SEGTYPE="")  DO
        . . . SET SEGN=0
        . . . FOR  SET SEGN=$ORDER(TMGHL7MSG(XREF,SEGTYPE,SEGN)) QUIT:(+SEGN'>0)  DO
        . . . . IF $DATA(TMGRESIDUAL(SEGN))'>0 QUIT
        . . . . SET TMGRESIDUAL(XREF,SEGTYPE,SEGN)=$GET(TMGHL7MSG(XREF,SEGTYPE,SEGN))
        . SET XREF="PO" 
        . SET SEGN=0
        . FOR  SET SEGN=$ORDER(TMGHL7MSG(XREF,SEGN)) QUIT:(+SEGN'>0)  DO
        . . IF $DATA(TMGRESIDUAL(SEGN))'>0 QUIT
        . . SET TMGRESIDUAL(XREF,SEGN)=""
        ELSE  KILL TMGRESIDUAL
CP2ARDN QUIT TMGRESULT
        ;
TESTCOMP(TMGHL7MSG,OUTARRAY) ;
        ;"Purpose: take completed TMGHL7MSG array, and compile into test array
        ;"Input: TMGHL7MSG -- Pass by REFERENCE.
        ;"       OUTARRAY -- filled with result
        ;"Results: 1 if OK, or -1^Message
        NEW TMGRESULT SET TMGRESULT=1
        ;"--restore MSH into file 773 ---
        NEW SEGN SET SEGN=$ORDER(TMGHL7MSG("B","MSH",0))
        IF SEGN'>0 DO  GOTO TCPMDN
        . SET TMGRESULT="-1^IN COMPILEM.TMGHL7X2: Can't find 'MSH' header in TMGHL7MSG"
        NEW MSHSEGN SET MSHSEGN=SEGN
        NEW AI SET AI=1
        SET OUTARRAY(AI)=$GET(TMGHL7MSG(SEGN)),AI=AI+1
        SET SEGN=0
        FOR  SET SEGN=$ORDER(TMGHL7MSG(SEGN)) QUIT:(+SEGN'>0)  DO
        . IF SEGN=MSHSEGN QUIT
        . SET OUTARRAY(AI)=$GET(TMGHL7MSG(SEGN)),AI=AI+1
        . SET OUTARRAY(AI)="",AI=AI+1  ;"separate lines with blank lines as original format
TCPMDN  QUIT TMGRESULT
        ;
FLATNARR(TMGHL7MSG,ARRAY,TMGU) ;"Reverse of PRSEARRY.  
        ;"Purpose: Reverse of PRSEARRY.  Takes parsed array and flatten it.
        ;"Input: TMGHL7MSG -- PASS BY REFERENCE.  Format as per PRSEARRY() below.  
        ;"       OUTARR -- PASS BY REFERENCE.  OUT PARAMETER.  Prior contents KILLED   
        ;"              ARRAY(1)="1st line" <-- must be MSH segment
        ;"              ARRAY(2)="2nd line etc."
        ;"       TMGU -- Array of divisor characters.  
        DO REFRESHM^TMGHL7X2(.TMGHL7MSG,.TMGU)
        KILL ARRAY
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(TMGHL7MSG(IDX)) QUIT:IDX'>0  DO
        . SET ARRAY(IDX)=$GET(TMGHL7MSG(IDX))
        QUIT
        ;
PRSEARRY(IEN22720,ARRAY,TMGHL7MSG,TMGU) ;
        ;"Purpose: to take an ARRAY, containing HL7 message, and parse to TMGHL7MSG
        ;"Input: IEN22720 --OPTIONAL.  IEN in TMG HL7 MESSAGE TRANSFORM SETTINGS file
        ;"       ARRAY -- pass by REFERENCE.  Format
        ;"               ARRAY(1)="1st line" <-- must be MSH segment
        ;"               ARRAY(2)="2nd line etc."
        ;"       TMGHL7MSG -- an OUT PARAMETER.  PASS BY REFERENCE.
        ;"               See PARSEMSG() for format.
        ;"       TMGU -- Array of divisor characters.  Filled if passed in empty
        ;"Output: TMGHL7MSG <--- will have all formatting found in PARSEMSG^TMGHL7X2
        ;"Result: 1 if OK, -1^Error Message IF error.
        NEW TMGRESULT SET TMGRESULT=1                  
        IF $DATA(TMGU)=0 DO
        . NEW MSH SET MSH=$GET(ARRAY(1))
        . IF $EXTRACT(MSH,1,3)'="MSH" DO  QUIT
        . . SET TMGRESULT="-1^IN PRSEARRY.TMGHL7X2: MSH segment not found in first line of message."
        . . MERGE ^TMG("TMP","TMGHL71",$J,"ARRAY")=ARRAY
        . NEW FS,ECH
        . SET FS=$EXTRACT(MSH,4)
        . SET ECH=$EXTRACT(MSH,5,8)
        . IF ($GET(FS)="")!($GET(ECH)="") DO  QUIT
        . . SET TMGRESULT="-1^IN PRSEARRY.TMGHL7X2: Values for FS or ECH not provided"
        . DO SUTMGU(.TMGU,FS,ECH)
        . SET TMGHL7MSG("ECH")=ECH
        . SET TMGHL7MSG("FS")=FS
        IF TMGRESULT<0 GOTO PADN
        KILL TMGHL7MSG
        NEW WPI SET WPI=0
        ;"SET TMGHL7MSG(WPI)=MSH
        NEW LINE SET LINE=0
        FOR  SET LINE=$ORDER(ARRAY(LINE)) QUIT:(+LINE'>0)  DO
        . NEW S SET S=$GET(ARRAY(LINE)) IF S="" QUIT
        . SET WPI=WPI+1
        . DO PRSESEG(WPI,S,.TMGHL7MSG,.TMGU) ;
        DO SETPOIDX(.IEN22720,.TMGHL7MSG) ;"Set up Processing Order (PO) index
PADN    QUIT TMGRESULT
        ;
PRSESEG(SEGN,S,TMGHL7MSG,TMGU) ;
        SET TMGHL7MSG(SEGN)=S
        NEW SEG SET SEG=$PIECE(S,TMGU(1),1)
        SET TMGHL7MSG(SEGN,"SEG")=SEG
        SET TMGHL7MSG("B",SEG,SEGN)=""
        IF $EXTRACT(S,1,3)="MSH" DO
        . SET S=$EXTRACT(S,4,$LENGTH(S)) ;"trip off segment type
        ELSE  DO
        . SET S=$PIECE(S,TMGU(1),2,999) ;"trip off segment type
        NEW FLDNUM
        FOR FLDNUM=1:1:$LENGTH(S,TMGU(1)) DO
        . DO PRSEFLD(SEGN,FLDNUM,$PIECE(S,TMGU(1),FLDNUM),.TMGHL7MSG,.TMGU) ;
        QUIT
        ;
PRSEFLD(SEGN,FLDNUM,VALUE,TMGHL7MSG,TMGU) ;"PARSE FIELD
        SET TMGHL7MSG(SEGN,FLDNUM)=VALUE
        NEW HASDIV SET HASDIV=0
        NEW IDX FOR IDX=1:1:$LENGTH(TMGU(2,"S")) QUIT:HASDIV=1  IF VALUE[TMGU(IDX) SET HASDIV=1
        IF HASDIV=0 GOTO PFDN
        IF VALUE'[TMGU(2) SET VALUE=VALUE_TMGU(2) ;"Handle if no field divider, but has sub-dividers
        NEW COMPNUM ;"component level
        FOR COMPNUM=1:1:$LENGTH(VALUE,TMGU(2)) DO
        . DO PRSECOMP(SEGN,FLDNUM,COMPNUM,$PIECE(VALUE,TMGU(2),COMPNUM),.TMGHL7MSG,.TMGU) 
PFDN    QUIT
        ;
PRSECOMP(SEGN,FLDNUM,COMPNUM,VALUE,TMGHL7MSG,TMGU) ;"PARSE COMPONENT
        IF VALUE="" GOTO PCDN
        SET TMGHL7MSG(SEGN,FLDNUM,COMPNUM)=VALUE
        NEW HASDIV SET HASDIV=0
        NEW IDX FOR IDX=1:1:$LENGTH(TMGU(3,"S")) QUIT:HASDIV=1  IF VALUE[TMGU(IDX) SET HASDIV=1
        IF HASDIV=0 GOTO PCDN
        IF VALUE'[TMGU(3) SET VALUE=VALUE_TMGU(3) ;"Handle if no comp divider, but has sub-dividers
        NEW SUBCOMPN
        FOR SUBCOMPN=1:1:$LENGTH(VALUE,TMGU(3)) DO
        . DO PRSESCMP(SEGN,FLDNUM,COMPNUM,SUBCOMPN,$PIECE(VALUE,TMGU(3),SUBCOMPN),.TMGHL7MSG,.TMGU) ;
PCDN    QUIT
        ;
PRSESCMP(SEGN,FLDNUM,COMPNUM,SUBCOMPN,VALUE,TMGHL7MSG,TMGU) ;"PARSE SUB-COMPONENT
        IF VALUE="" GOTO PSCDN
        SET TMGHL7MSG(SEGN,FLDNUM,COMPNUM,SUBCOMPN)=VALUE
        NEW HASDIV SET HASDIV=0
        NEW IDX FOR IDX=1:1:$LENGTH(TMGU(4,"S")) QUIT:HASDIV=1  IF VALUE[TMGU(IDX) SET HASDIV=1
        IF HASDIV=0 GOTO PSCDN
        IF VALUE'[TMGU(3) SET VALUE=VALUE_TMGU(4) ;"Handle if no comp divider, but has sub-dividers
        NEW SUBN
        FOR SUBN=1:1:$LENGTH(VALUE,TMGU(4)) DO
        . DO PRSES2CP(SEGN,FLDNUM,COMPNUM,SUBCOMPN,SUBN,$PIECE(VALUE,TMGU(4),SUBN),.TMGHL7MSG,.TMGU) ;
PSCDN   QUIT
        ;
PRSES2CP(SEGN,FLDNUM,COMPNUM,SUBCOMPN,SUB2COMPN,VALUE,TMGHL7MSG,TMGU) ;"PARSE SUB-2-COMPONENT
        IF VALUE="" GOTO PS2CDN
        SET TMGHL7MSG(SEGN,FLDNUM,COMPNUM,SUBCOMPN,SUB2COMPN)=VALUE
PS2CDN   QUIT
        ;
BUBLSCMP(SEGN,FLDNUM,COMPNUM,SUBCOMPN,TMGHL7MSG,TMGU) ;
        ;"Buble changes upward in tree.
        ;"Compile changes to subcomponent into parent component
        NEW VALUE SET VALUE=""
        NEW I SET I=""
        FOR  SET I=$ORDER(TMGHL7MSG(SEGN,FLDNUM,COMPNUM,I)) QUIT:(I="")  DO
        . SET $PIECE(VALUE,TMGU(3),I)=$GET(TMGHL7MSG(SEGN,FLDNUM,COMPNUM,I))
        SET TMGHL7MSG(SEGN,FLDNUM,COMPNUM)=VALUE
        DO BUBLCOMP(SEGN,FLDNUM,COMPNUM,.TMGHL7MSG,.TMGU) ;
        QUIT
        ;
BUBLCOMP(SEGN,FLDNUM,COMPNUM,TMGHL7MSG,TMGU) ;
        ;"Buble changes upward in tree.
        ;"Compile changes to component into parent field
        NEW VALUE SET VALUE=""
        NEW I SET I=""
        FOR  SET I=$ORDER(TMGHL7MSG(SEGN,FLDNUM,I)) QUIT:(I="")  DO
        . SET $PIECE(VALUE,TMGU(2),I)=$GET(TMGHL7MSG(SEGN,FLDNUM,I))
        SET TMGHL7MSG(SEGN,FLDNUM)=VALUE
        DO BUBLFLD(SEGN,FLDNUM,.TMGHL7MSG,.TMGU) ;
        QUIT
        ;
BUBLFLD(SEGN,FLDNUM,TMGHL7MSG,TMGU) ;
        ;"Buble changes upward in tree.
        ;"Compile changes to field into parent segment
        NEW VALUE SET VALUE=""
        NEW I SET I=""
        FOR  SET I=$ORDER(TMGHL7MSG(SEGN,I)) QUIT:(I="")  DO
        . SET $PIECE(VALUE,TMGU(1),I)=$GET(TMGHL7MSG(SEGN,I))
        NEW SEG SET SEG=$GET(TMGHL7MSG(SEGN,"SEG"))
        IF SEG="MSH" SET TMGHL7MSG(SEGN)=SEG_VALUE 
        ELSE  SET TMGHL7MSG(SEGN)=SEG_TMGU(1)_VALUE
        QUIT
        ;
REFRESHM(TMGHL7MSG,TMGU,SEGN,FLDN,COMPN,SCMPN) ;
        ;"Purpose: to refresh the array after changes are made to one part.
        ;"Input: TMGHL7MSG -- required PASS BY REFERENCE.  The array (created by PRSEARRY() or PARSMSG2())
        ;"       TMGU -- Array of divisor characters (see other functions for docs)
        ;"       SEGN -- Optional.  segment number
        ;"       FLDN -- Optional. Field number
        ;"       COMPN -- Optional. Component number
        ;"       SCMPN -- Optional.  Sub-component number
        ;"      **NOTE: Depending on the level of the array edited, one or more of the
        ;"              place numbers may be left blank.  For example, if the 3rd segment
        ;"              was edited, then SEGN=3 would be all that was passed.
        ;"              But if a subcomponent is passed, then the SEGN,FLDN,COMPN would
        ;"              have to be passed in addition to the SCMPN
        NEW VALUE,LEVEL
        SET LEVEL=0
        IF $GET(SEGN)="" GOTO RM0
        SET LEVEL=1
        IF $GET(FLDN)="" GOTO RM1
        SET LEVEL=2
        IF $GET(COMPN)="" GOTO RM2
        SET LEVEL=3
        IF $GET(SCMPN)="" GOTO RM3
        SET LEVEL=4
        GOTO RM4
RM0     ;"Entire message was edited.
        SET SEGN=0
        FOR  SET SEGN=$ORDER(TMGHL7MSG(SEGN)) QUIT:+SEGN'>0  DO
        . DO RM1 ;"process 1 segment
        GOTO RMDN
RM1     ;"Segment was edited
        SET VALUE=$GET(TMGHL7MSG(SEGN))
        NEW SEG SET SEG=$GET(TMGHL7MSG(SEGN,"SEG"))
        IF SEG'="" KILL TMGHL7MSG("B",SEG,SEGN)
        KILL TMGHL7MSG(SEGN)  ;"kill node to reparse.
        DO PRSESEG(SEGN,VALUE,.TMGHL7MSG,.TMGU) ;  ;" propagate down tree
        MERGE TMGHL7MSG(SEGN,"ORDER")=TMGHL7MSG("ORDER",SEGN)
        MERGE TMGHL7MSG(SEGN,"RESULT")=TMGHL7MSG("RESULT",SEGN)
        GOTO RMDN
RM2     ;"Field was edited.
        SET VALUE=$GET(TMGHL7MSG(SEGN,FLDN))
        KILL TMGHL7MSG(SEGN,FLDN)  ;"kill node to reparse.
        DO PRSEFLD(SEGN,FLDN,VALUE,.TMGHL7MSG,.TMGU) ;" propagate down tree
        DO BUBLFLD(SEGN,FLDN,.TMGHL7MSG,.TMGU) ;" propagate up tree
        GOTO RMDN
RM3     ;"Component was edited
        SET VALUE=$GET(TMGHL7MSG(SEGN,FLDN,COMPN))
        KILL TMGHL7MSG(SEGN,FLDN,COMPN)  ;"kill node to reparse.
        DO PRSECOMP(SEGN,FLDN,COMPN,VALUE,.TMGHL7MSG,.TMGU) ;" propagate down tree
        DO BUBLCOMP(SEGN,FLDN,COMPN,.TMGHL7MSG,.TMGU) ;" propagate up tree
        GOTO RMDN
RM4     ;"Sub-component was edited
        DO BUBLSCMP(SEGN,FLDN,COMPN,SCMPN,.TMGHL7MSG,.TMGU) ;" propagate up tree
RMDN    QUIT
        ;
SETPOIDX(IEN22720,TMGHL7MSG) ;
        ;"Purpose: Set up Processing Order (PO) index for particular 22720 file
        KILL TMGHL7MSG("PO")
        ;"7/2013 note: Order processing will now be in the order of appearance in
        ;"            the HL7 message.  This makes more sense, when an OBX segment
        ;"           needs to refer to the corresponding OBR segment.  It doesn't
        ;"          make sense to process all the OBR's separately, etc.
        ;"         This way, individual segment or field handlers can make use
        ;"        of TMGINFO(*) array that contains information about current ordered test
        NEW CT SET CT=0
        NEW IDX SET IDX=1
        FOR  SET CT=$ORDER(TMGHL7MSG(CT)) QUIT:(+CT'>0)  DO
        . SET TMGHL7MSG("PO",IDX)=CT,IDX=IDX+1
        QUIT
        ;
        ;"DEPRECIATED CODE BELOW.
        NEW FOUND,TEMP
        NEW CT SET CT=0
        NEW ORD SET ORD=0
        ;"First get ordered entries from file 22720
        FOR  SET ORD=$ORDER(^TMG(22720,IEN22720,11,"AC",ORD)) QUIT:(+ORD'>0)  DO
        . NEW IENSEG SET IENSEG=0
        . FOR  SET IENSEG=$ORDER(^TMG(22720,IEN22720,11,"AC",ORD,IENSEG)) QUIT:(+IENSEG'>0)  DO
        . . NEW SEGNAME SET SEGNAME=$PIECE($GET(^TMG(22720,IEN22720,11,IENSEG,0)),"^",1)
        . . IF SEGNAME="" QUIT
        . . IF $GET(FOUND(SEGNAME))=1 QUIT
        . . SET CT=CT+1 SET TEMP(CT)=SEGNAME
        . . SET FOUND(SEGNAME)=1
        ;"Next get rest of entries from file 22720
        NEW IENSEG SET IENSEG=0
        FOR  SET IENSEG=$ORDER(^TMG(22720,IEN22720,11,IENSEG)) QUIT:(+IENSEG'>0)  DO
        . NEW SEGNAME SET SEGNAME=$PIECE($GET(^TMG(22720,IEN22720,11,IENSEG,0)),"^",1)
        . IF $GET(FOUND(SEGNAME))=1 QUIT
        . SET CT=CT+1 SET TEMP(CT)=SEGNAME
        . SET FOUND(SEGNAME)=1
        ;"Now order the entries in TMGHL7MSG according to above
        KILL FOUND
        NEW PO SET PO=0
        SET ORD=0
        FOR  SET ORD=$ORDER(TEMP(ORD)) QUIT:(+ORD'>0)  DO
        . NEW SEGNAME SET SEGNAME=TEMP(ORD)
        . NEW I SET I=0
        . FOR  SET I=$ORDER(TMGHL7MSG("B",SEGNAME,I)) QUIT:(+I'>0)  DO
        . . SET PO=PO+1,TMGHL7MSG("PO",PO)=I
        . . SET FOUND(I)=1
        ;"Finally, put in all segments in message not already accounted for
        SET ORD=0
        FOR  SET ORD=$ORDER(TMGHL7MSG(ORD)) QUIT:(+ORD'>0)  DO
        . IF $GET(FOUND(ORD))=1 QUIT
        . SET PO=PO+1,TMGHL7MSG("PO",PO)=ORD
        QUIT
        ;
MSH2TMGU(MSH,TMGU)  ;"Setup TMGU from MSH
        NEW TMGRESULT SET TMGRESULT="1^OK"
        NEW FS,ECH
        SET FS=$EXTRACT(MSH,4)
        SET ECH=$EXTRACT(MSH,5,8)
        IF ($GET(FS)="")!($GET(ECH)="") DO
        . SET TMGRESULT="-1^IN PRSEARRY.TMGHL7X2: Values for FS or ECH not provided"
        ELSE  DO SUTMGU(.TMGU,FS,ECH)
        QUIT TMGRESULT
        ;
SUTMGU(TMGU,FS,ECH) ;
        SET TMGU(1)=FS
        SET TMGU(2)=$EXTRACT(ECH,1)
        ;"//kt 12/9/16 --> original. SET TMGU(3)=$EXTRACT(ECH,4)
        SET TMGU(3)=$EXTRACT(ECH,2)  ;"//kt 12/9/16 I am not sure why it was not this way originally....
        SET TMGU(4)=$EXTRACT(ECH,3)
        SET TMGU(5)=$EXTRACT(ECH,4)
        SET TMGU(2,"S")=TMGU(2)_TMGU(3)_TMGU(4)_TMGU(5)
        SET TMGU(3,"S")=TMGU(3)_TMGU(4)_TMGU(5)
        SET TMGU(4,"S")=TMGU(4)_TMGU(5)
        SET TMGU(5,"S")=TMGU(5)
        QUIT
        ;
ORGSPECS(TMGHL7MSG,TMGU) ;
        ;"Purpose: Create index specifying which specimens are used for which OBR segments
        ;"Input: TMGHL7MSG -- PASS BY REFERENCE.  The message array to modify (created by PRSEARRY())
        ;"       TMGU -- Array of divisor characters (see other functions for docs)
        ;"Output: Added node:
        ;"         TMGHL7MSG("OBR-SPEC",<SPEC>,SegmentIndex)="
        ;"         TMGHL7MSG("OBR-SPEC","ORDER",SegmentIndex)=<SPEC>
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(TMGHL7MSG(IDX)) QUIT:(+IDX'>0)  DO
        . NEW SEGNAME SET SEGNAME=$PIECE($GET(TMGHL7MSG(IDX)),TMGU(1),1)
        . IF SEGNAME'="OBR" QUIT
        . NEW SPEC SET SPEC=$GET(TMGHL7MSG(IDX,15,1)) 
        . IF SPEC="" QUIT
        . SET TMGHL7MSG("OBR-SPEC",SPEC,IDX)=""
        . SET TMGHL7MSG("OBR-SPEC","ORDER",IDX)=SPEC
        QUIT
        ;"--------------------------------------------------------------------
        ;"--------------------------------------------------------------------        
GETPCE(TMGHL7MSG,SEG,FLDN,COMPN,SCMPN) ;
        ;"Purpose: return a requested piece parsed TMGHL7MSG array
        ;"Input : TMGHL7MSG -- the array to read from. PASS BY REFERENCE.
        ;"        SEG -- the segment *number* in the array, OR
        ;"               the segment *NAME* desired
        ;"        FLDN -- OPTIONAL. The field number
        ;"        COMPN -- OPTIONAL. The component number
        ;"        SCMPN -- OPTIONAL.  The sub-component number
        ;"NOTE: IF SCMPN provided, then COMPN,FLDN,SEG all required
        ;"      IF COMPN provided, then FLDN,SEG all required
        ;"Results -- returns piece, or "" IF problem.
        NEW TMGRESULT SET TMGRESULT=""
        SET SEG=$GET(SEG)
        NEW SEGN SET SEGN=0
        IF +SEG=SEG SET SEGN=SEG
        ELSE  SET SEGN=+$ORDER(TMGHL7MSG("B",SEG,0))
        IF SEGN'>0 GOTO GPCDN
        SET SCMPN=+$GET(SCMPN)
        SET COMPN=+$GET(COMPN)
        SET FLDN=+$GET(FLDN)
        IF SCMPN>0 DO  GOTO GPCDN
        . SET TMGRESULT=$GET(TMGHL7MSG(SEGN,FLDN,COMPN,SCMPN))
        IF COMPN>0 DO  GOTO GPCDN
        . SET TMGRESULT=$GET(TMGHL7MSG(SEGN,FLDN,COMPN))
        IF FLDN>0 DO  GOTO GPCDN
        . SET TMGRESULT=$GET(TMGHL7MSG(SEGN,FLDN))
        SET TMGRESULT=$GET(TMGHL7MSG(SEGN))
GPCDN   QUIT TMGRESULT
        ;
SETPCE(VALUE,TMGHL7MSG,TMGU,SEG,FLDN,COMPN,SCMPN) ;
        ;"Purpose: Set a requested piece parsed TMGHL7MSG array
        ;"Input : VALUE -- the string to store at location sepecified below
        ;"        TMGHL7MSG -- the array to store in. PASS BY REFERENCE.
        ;"        TMGU -- The array with divisor chars.
        ;"        SEG -- the segment *number* in the array, OR
        ;"               the segment *NAME* desired <-- ? WORKING ?
        ;"        FLDN -- OPTIONAL. The field number
        ;"        COMPN -- OPTIONAL. The component number
        ;"        SCMPN -- OPTIONAL.  The sub-component number
        ;"NOTE: IF SCMPN provided, then COMPN,FLDN,SEG all required
        ;"      IF COMPN provided, then FLDN,SEG all required
        ;"Results: none
        SET VALUE=$GET(VALUE)
        SET SEG=$GET(SEG)
        NEW SEGN SET SEGN=0
        IF +SEG=SEG SET SEGN=SEG
        ELSE  SET SEGN=+$ORDER(TMGHL7MSG("B",SEG,0))
        IF SEGN'>0 GOTO SPCDN
        SET SCMPN=+$GET(SCMPN)
        SET COMPN=+$GET(COMPN)
        SET FLDN=+$GET(FLDN)
        IF SCMPN>0 DO  GOTO SPCDN
        . SET TMGHL7MSG(SEGN,FLDN,COMPN,SCMPN)=VALUE
        . DO REFRESHM(.TMGHL7MSG,.TMGU,SEGN,FLDN,COMPN,SCMPN) ;
        IF COMPN>0 DO  GOTO SPCDN
        . SET TMGHL7MSG(SEGN,FLDN,COMPN)=VALUE
        . DO REFRESHM(.TMGHL7MSG,.TMGU,SEGN,FLDN,COMPN) ;
        IF FLDN>0 DO  GOTO SPCDN
        . SET TMGHL7MSG(SEGN,FLDN)=VALUE
        . DO REFRESHM(.TMGHL7MSG,.TMGU,SEGN,FLDN) ;
        ;"Default-----------------
        SET TMGHL7MSG(SEGN)=VALUE
        DO REFRESHM(.TMGHL7MSG,.TMGU,SEGN) ;
SPCDN   QUIT
        ;
ENSURSEG(TMGHL7MSG,SEG,TMGU,SEGN) ;Ensure segment exists in message
        ;"Input: TMGHL7MSG -- the array to store in. PASS BY REFERENCE.
        ;"       SEG -- the name of the desired segment to ensure existence.  E.g. 'PID'
        ;"       TMGU -- The array with divisor chars.  PASS BY REFERENCE.
        ;"       SEGN -- Optional -- The desired segment number to use when adding
        ;"              NOTE: ignored if segment already exists.  And if segment
        ;"                  number already exists, then number will be changed to unique.
        ;"Result: 1^OK, or -1^Error message, if problem.
        NEW TMGRESULT SET TMGRESULT="1^OK"
        SET SEG=$GET(SEG) IF SEG="" DO  GOTO NSRSGDN
        . SET TMGRESULT="-1^Segment name not provided to ENSURSEG^TMGHL7X2"
        NEW PRIORSEGN SET PRIORSEGN=+$ORDER(TMGHL7MSG("B",SEG,0))
        IF PRIORSEGN>0 GOTO NSRSGDN
        SET SEGN=+$GET(SEGN)
        IF SEGN=0 SET SEGN=+$ORDER(TMGHL7MSG("@"),-1)+1
        ELSE  FOR  QUIT:$DATA(TMGHL7MSG(SEGN))=0  SET SEGN=SEGN+0.1        
        NEW S SET S=SEG
        ;"Create segment with empty fields, so that transforms will be called for them
        NEW I FOR I=2:1:14 SET $PIECE(S,TMGU(1),I)=""
        DO PRSESEG^TMGHL7X2(SEGN,S,.TMGHL7MSG,.TMGU)
NSRSGDN QUIT TMGRESULT
        ;
GETSEG(OUT,TMGHL7MSG,SEG) ;" Return array of parsed SEG from TMGHL7MSG
        ;"Input: OUT -- PASS BY REFERENCE, AN OUT PARAMETER. PRIOR VALUES KILLED
        ;"       TMGHL7MSG -- the array to store in. PASS BY REFERENCE.
        ;"       SEG -- the name of the desired segment to ensure existence.  E.g. 'PID'
        ;"          If multiple SEG's exist, then the FIRST ONE is returned
        ;"Result: none
        NEW SEGN SET SEGN=+$ORDER(TMGHL7MSG("B",$GET(SEG),""))
        KILL OUT MERGE OUT=TMGHL7MSG(SEGN)
        QUIT
        ;
