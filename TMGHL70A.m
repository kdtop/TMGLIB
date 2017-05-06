TMGHL70A ;TMG/kst-Installation/config tools for POC HL7 processing ;03/12/11, 2/2/14
              ;;1.0;TMG-LIB;**1**;03/12/11
 ;
 ;"TMG POC-UTILITY FUNCTIONS
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"TESTMAP(TMGENV,TMGTESTMSG,TMGHL7MSG) -- Test code mappings for POC setup
 ;
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;"CHKHLMAP(TMGENV,ONESET,TMGMAP) -- CHECK HL7 MESSAGE FOR MAPPING PROBLEMS: order vs results. 
 ;"FIXMAPS(TMGENV,TMGMAP) --fix mapping problems.
 ;"FIXHL7SPEC(TMGENV,TMGMAP) -- Fix Missing HL7-Spec
 ;"FIX1SPEC(TMGENV,TMGMAP) -- Fix 1 Missing HL7-Spec
 ;"AIMISMATCH(TMGENV,TMGMAP) -- handle missing entries in AUTO INSTRUMENT file.
 ;"FIX1MISM(TMGENV,TMGMAP) -- Fix one mismatch
 ;"NOAIREC(TMGENV,TMGMAP) -- handle missing entries in AUTO INSTRUMENT file.
 ;"FIX1AIR(TMGENV,TMGMAP) -- Fix 1 AI record
 ;"NODATANM(TMGENV,TMGHL7MSG) -- Fix ""Dataname ### not in accession _____ or Load/work list ___ test profile"
 ;"LWLMSMTH(TMGENV,TMGHL7MSG) --LOAD WORK LIST MISMATCH
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;" TMGEDIT,TMGUSRIF, TMGDEBUG
 ;" Note: this uses Linux functionality.
 ;"=======================================================================
 ;
TESTMAP(TMGENV,TMGTESTMSG,TMGHL7MSG) ;"Test code mappings for POC setup
        ;"This code is derived from: PRINT^LA7PCFG
        ;"Input: TMGENV -- PASS BY REFERENCE.  Lab environment
        ;"           TMGENV("PREFIX") -- e.g. "LMH"
        ;"           TMGENV("IEN 68.2") -- IEN in LOAD/WORK LIST (holds orderable items)
        ;"           TMGENV("IEN 62.4") -- IEN in AUTO INSTRUMENT (holds resultable items)        
        ;"           TMGENV(<other entries>)= etc.              
        NEW % SET %=2 
        IF '$DATA(TMGTESTMSG) DO
        . WRITE "Use actual HL7 message for extra mapping testing"
        . DO YN^DICN WRITE !
        . IF %'=1 QUIT
        . DO FILEMENU^TMGHL70(.TMGTESTMSG)
        IF %=-1 GOTO TMPDN
        IF $DATA(TMGTESTMSG),'$DATA(TMGHL7MSG) DO
        . SET %=1
        . WRITE !,"HL7 message must now be transformed before mapping",!
        . WRITE "  checks can performed.",!
        . WRITE "NOTE: during the transformation process, IF NEW tests",!
        . WRITE "  are encountered, you will be prompted to add them to",!
        . WRITE "  the system before continuing.",!
        . WRITE "Transform message now" DO YN^DICN WRITE !
        . IF %=1 DO TESTPARS^TMGHL70(.TMGTESTMSG,.TMGHL7MSG)  ;"Sets up TMGHL7MSG
        IF %=-1 GOTO TMPDN
        ;
        NEW DIC,LA7624,X,Y
        ;
        SET LA7624=TMGENV("IEN 62.4")  ;"$$GETAI^TMGHL70() ;
        IF LA7624'>0 QUIT
        ;
        NEW I,X,Y
        NEW LA7EXIT,LA7INTYP,LA7LINE,LA7LINE2,LA7NOW,LA7PAGE,LA7CODE
        NEW LA76248,LR60,LR61,LR62,LR64,LR642,LRLL,LRPROF
        ;
        SET LA7NOW=$$HTE^XLFDT($H,"1D")
        SET (LA7EXIT,LA7PAGE)=0
        SET LA7624(0)=$G(^LAB(62.4,LA7624,0))
        SET LA76248=$P(LA7624(0),"^",8)
        SET LA7INTYP=$P(^LAHM(62.48,LA76248,0),"^",9)
        SET LRLL=$P(LA7624(0),"^",4)
        SET LRPROF=$O(^LRO(68.2,LRLL,10,0))
        SET LA7LINE=$$REPEAT^XLFSTR("=",IOM)
        SET LA7LINE2=$$REPEAT^XLFSTR("-",IOM)
        ;"DO HDR^LA7PCFG
        S LA7PAGE=LA7PAGE+1
        W !,"Point of Care Test Code Mapping"
        W !," for interface: ",$P(LA7624(0),"^")
        W !,LA7LINE,!
        ;
        WRITE !!,"VistA ADT feed enabled: ",$S(LA7INTYP=21:"YES",LA7INTYP=20:"NO",1:"UNKNOWN"),!!
        ;
        NEW VB SET VB=0
        IF VB DO SH1^LA7PCFG
        ;
        NEW TMGMAP
        SET I=0
        FOR  SET I=$O(^LRO(68.2,LRLL,10,LRPROF,1,I)) Q:'I  D  Q:LA7EXIT
        . NEW ONEMAP
        . IF $$GETWLMAP^TMGHL7U(.TMGENV,I,.ONEMAP,VB)=1 QUIT
        . MERGE TMGMAP=ONEMAP
        ;
        IF VB DO SH2^LA7PCFG
        ;
        SET I=0
        FOR  S I=$O(^LAB(62.4,LA7624,3,I)) Q:'I  D  Q:LA7EXIT
        . NEW ONEMAP
        . IF $$GETAIMAP^TMGHL7U(.TMGENV,I,.ONEMAP,VB)=1 QUIT
        . MERGE TMGMAP=ONEMAP
        ;
        IF '$DATA(TMGHL7MSG) GOTO TMPFX
        SET I=0
        FOR  SET I=$ORDER(TMGHL7MSG("HL7","GROUP",I)) QUIT:(+I'>0)  DO
        . NEW ONESET MERGE ONESET=TMGHL7MSG("HL7","GROUP",I)
        . NEW ONEMAP
        . DO CHKHLMAP(.TMGENV,.ONESET,.ONEMAP)
        . MERGE TMGMAP=ONEMAP
        ;
TMPFX   SET %=1
        IF '$DATA(TMGMAP) DO
        . WRITE "No problems found with mapping.",!
        . SET %=1
        . ;"WRITE !,"Try fixing specific problems anyway" DO YN^DICN WRITE !
        IF %=-1 GOTO TMPDN
        IF ($DATA(TMGMAP))!(%=1) DO FIXMAPS(.TMGENV,.TMGMAP) ;
TMPDN   QUIT
        ;
CHKHLMAP(TMGENV,ONESET,TMGMAP) ;"CHECK HL7 MESSAGE FOR MAPPING PROBLEMS: order vs results. 
        ;"Input: TMGENV -- PASS BY REFERENCE.  Lab environment
        ;"           TMGENV("PREFIX") -- e.g. "LMH"
        ;"           TMGENV("IEN 68.2") -- IEN in LOAD/WORK LIST (holds orderable items)
        ;"           TMGENV("IEN 62.4") -- IEN in AUTO INSTRUMENT (holds resultable items)        
        ;"           TMGENV(<other entries>)= etc.              
        ;"       ONESET -- PASS BY REFERENCE.  One group of array as created by PRSEARRY^TMGHL7X2.  Format:
        ;"         ONESET(#,<SEGMENT_NAME>,#)=segment string (with type (e.g. OBR) stripped)        
        ;"         ONESET(#,"ORDER","IEN60")=IEN60        
        ;"         ONESET(#,"ORDER","NLT")=NLTCode^IEN        
        ;"         ONESET(#,"ORDER","SPECIMEN")=ien        
        ;"         ONESET(#,"ORDER","SPECIMEN 64.061")=IEN64d061        
        ;"         ONESET(#,"RESULTS",#)=HL7 Message Field #3 (i.e. Test Identifier) (e.g. '6690-2^WBC^LN')        
        ;"         ONESET(#,"RESULTS",#,"NLT")=Mapped NLTCode^#   
        ;"       TMGMAP -- PASS BY REFERENCE, AN OUT PARAMETER.  Format:
        ;"          TMGMAP("ACCESSION","ORDER")=NLTCode, or '<MISSING>'
        ;"Result: 1 if OK, or -1 IF problem.        
        NEW TMGRESULT SET TMGRESULT=-1
        NEW MISSING SET MISSING="<MISSING>"
        NEW ORDER SET ORDER=$GET(ONESET("ORDER","NLT"),MISSING)
        ;"SET TMGMAP("ACCESSION","ORDER")=ORDER
        IF ORDER=MISSING GOTO CHLMDN
        DO EXPND60^TMGHL7U(ORDER,.ORDER)
        NEW CT SET CT=1
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(ONESET("RESULTS",IDX)) QUIT:(+IDX'>0)  DO 
        . NEW WKLD SET WKLD=$GET(ONESET("RESULTS",IDX,"NLT"))
        . IF $DATA(ORDER("WKLD",WKLD)) QUIT
        . SET TMGMAP("ACCESSION","RESULT WITHOUT ORDER",WKLD)=ORDER 
        IF '$DATA(TMGMAP("ACCESSION","RESULT WITHOUT ORDER")) SET TMGRESULT=1        
CHLMDN  QUIT
        ;
FIXMAPS(TMGENV,TMGMAP) ;
        ;"Purpose: To fix mapping problems.
        ;"NOTE: may use TMGHL7MSG via global scope        
        ;"Input: TMGENV -- PASS BY REFERENCE.  Lab environment
        ;"           TMGENV("PREFIX") -- e.g. "LMH"
        ;"           TMGENV("IEN 68.2") -- IEN in LOAD/WORK LIST (holds orderable items)
        ;"           TMGENV("IEN 62.4") -- IEN in AUTO INSTRUMENT (holds resultable items)        
        ;"           TMGENV(<other entries>)= etc.              
        ;"       TMGMAP -- Pass by REFERENCE
        ;
        KILL TMGUSERINPUT,TMGMNU
M1      ;
        KILL TMGMNU,TMGMNUI SET TMGMNUI=0
        SET TMGMNU(TMGMNUI)="Pick Option for fixing Mapping. ",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="Fix Missing HL7-Spec"_$CHAR(9)_"HL7Spec",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="Fix ""RESULT NLT CODE does not match UI TEST CODE"""_$CHAR(9)_"AIMismatch",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="Fix ""Msg #112, Dataname ### not in accession _____ or ..."_$CHAR(9)_"MissingDataName",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="Fix ""Msg #172, Order NLT code ### not linked ... "_$CHAR(9)_"MissingLWL",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="xxxx Fix RESULT NLT CODE missing (or something)"_$CHAR(9)_"AIMissing",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="(Add more later as problems arise, in TMGHL70A routine)"_$CHAR(9)_"Null",TMGMNUI=TMGMNUI+1
        ;
        SET TMGUSERINPUT=$$MENU^TMGUSRI2(.TMGMNU,"^")
        KILL TMGMNU ;"Prevent from cluttering variable table during debug run
        ;
        IF TMGUSERINPUT="HL7Spec" DO FIXHL7SPEC(.TMGENV,.TMGMAP) GOTO M1
        IF TMGUSERINPUT="AIMissing" DO NOAIREC(.TMGENV,.TMGMAP) GOTO M1
        IF TMGUSERINPUT="AIMismatch" DO AIMISMATCH(.TMGENV,.TMGMAP) GOTO M1
        IF TMGUSERINPUT="MissingLWL" DO LWLMSMTH(.TMGENV,.TMGHL7MSG) GOTO M1
        IF TMGUSERINPUT="MissingDataName" DO NODATANM(.TMGENV,.TMGHL7MSG) GOTO M1      
        ;
        IF TMGUSERINPUT="^" GOTO FMDN
        IF TMGUSERINPUT=0 SET TMGUSERINPUT=""
        GOTO M1
        ;
FMDN    QUIT
        ;
FIXHL7SPEC(TMGENV,TMGMAP) ;
        ;"Input: TMGENV -- PASS BY REFERENCE.  Lab environment
        ;"           TMGENV("PREFIX") -- e.g. "LMH"
        ;"           TMGENV("IEN 68.2") -- IEN in LOAD/WORK LIST (holds orderable items)
        ;"           TMGENV("IEN 62.4") -- IEN in AUTO INSTRUMENT (holds resultable items)        
        ;"           TMGENV(<other entries>)= etc.              
        ;"       TMGMAP --
        ;"Result: none
        NEW I SET I=""
        NEW DONE SET DONE=0
        FOR  SET I=$ORDER(TMGMAP("LOAD/WORK LIST",I)) QUIT:(I="")!DONE  DO
        . NEW ONEREC MERGE ONEREC=TMGMAP("LOAD/WORK LIST",I)
        . IF $$FIX1SPEC(.TMGENV,.ONEREC)=-1 SET DONE=1 QUIT
        QUIT
        ;
FIX1SPEC(TMGENV,TMGMAP) ;
        ;"Purpose:
        ;"Input: TMGENV -- PASS BY REFERENCE.  Lab environment
        ;"           TMGENV("PREFIX") -- e.g. "LMH"
        ;"           TMGENV("IEN 68.2") -- IEN in LOAD/WORK LIST (holds orderable items)
        ;"           TMGENV("IEN 62.4") -- IEN in AUTO INSTRUMENT (holds resultable items)        
        ;"           TMGENV(<other entries>)= etc.              
        ;"       TMGMAP -- Sample as below.
        ;"           TMGMAP("TEST NAME")="CBC & MORPHOLOGY (WITH DI"
        ;"           TMGMAP("COLLECTION SAMPLE")="SERUM"
        ;"           TMGMAP("HL7 SPEC")="<MISSING>"
        ;"           TMGMAP("LR60")=276
        ;"           TMGMAP("LR61")=70
        ;"           TMGMAP("LR62")=4
        ;"           TMGMAP("LR64")=2679
        ;"           TMGMAP("LR64.2")=2772
        ;"           TMGMAP("ORDER NLT CODE")="84999.0000"
        ;"           TMGMAP("ORDER NLT NAME")="Hemogram Manual"
        ;"           TMGMAP("SPECIMEN(IEN)")="BLOOD(70)"
        ;"           TMGMAP("WKLD CODE")=".0000"
        ;"           TMGMAP("ZZ ------------------")=""
        ;"Result: 1 if OK,  -1^Message IF error.
        NEW MISSING SET MISSING="<MISSING>"
        NEW TMGRESULT SET TMGRESULT=1
        IF $GET(TMGMAP("HL7 SPEC"))'="<MISSING>" GOTO F1SDN
        NEW LR61 SET LR61=+$GET(TMGMAP("LR61"))
        IF LR61'>0 DO  GOTO F1SDN
        . SET TMGRESULT="-1^Unable to get LR61"
        NEW LR64D061 SET LR64D061=$GET(TMGMAP("LR64.061"))
        IF LR64D061=MISSING DO  GOTO F1SDN
        . WRITE "Please fix missing LEDI HL7 field in "_$$GET1^DIQ(61,LR61_",",.01)_"("_LR61_") record in TOPOGRAPHY file",!
        . NEW DIE SET DIE="^LAB(61,"
        . NEW DA SET DA=LR61
        . NEW DR SET DR=".09"
        . LOCK +^LAB(61,LR61):1
        . IF '$TEST WRITE "RECORD LOCKED.",! QUIT
        . DO ^DIE
        . LOCK +^LAB(61,LR61):1
F1SDN   QUIT TMGRESULT
        ;
AIMISMATCH(TMGENV,TMGMAP) ;
        ;"NOTE: I thought the problem was that the record had not been added
        ;"      into the AUTO INSTRUMENT. But the real problem was that the UI TEST CODE
        ;"      did not match rec->60->NLT code.
        ;"      --I will leave this code here and see IF I need it later....
        ;"Purpose: handle missing entries in AUTO INSTRUMENT file.
        ;"Input: TMGENV -- PASS BY REFERENCE.  Lab environment
        ;"           TMGENV("PREFIX") -- e.g. "LMH"
        ;"           TMGENV("IEN 68.2") -- IEN in LOAD/WORK LIST (holds orderable items)
        ;"           TMGENV("IEN 62.4") -- IEN in AUTO INSTRUMENT (holds resultable items)        
        ;"           TMGENV(<other entries>)= etc.              
        ;"       TMGMAP --
        ;"Result: none
        NEW TMGUSERINPUT,TMGMNU
        NEW I SET I=""
AMM1    KILL TMGMNU
        SET TMGMNU(0)="Pick Test to Match up NLT Codes in AUTO INSTRUMENT"
        ;
        FOR  SET I=$ORDER(TMGMAP("AUTO INSTRUMENT",I)) QUIT:(I="")  DO
        . NEW L SET L=""
        . FOR  SET L=$ORDER(TMGMAP("AUTO INSTRUMENT",I,L)) QUIT:(L="")  DO
        . . IF $GET(TMGMAP("AUTO INSTRUMENT",I,L))'["Warning - RESULT NLT CODE does not match UI TEST CODE." QUIT
        . . SET TMGMNU(I)=$GET(TMGMAP("AUTO INSTRUMENT",I,"TEST NAME"))_" -> "_$GET(TMGMAP("AUTO INSTRUMENT",I,"RESULT NLT CODE"))_$CHAR(9)_I
        ;
        SET TMGUSERINPUT=$$MENU^TMGUSRI2(.TMGMNU,"^")
        KILL TMGMNU ;"Prevent from cluttering variable table during debug run
        ;
        IF TMGUSERINPUT>0 DO  GOTO AMM1
        . NEW ONEREC MERGE ONEREC=TMGMAP("AUTO INSTRUMENT",TMGUSERINPUT)
        . NEW TEMP SET TEMP=$$FIX1MISM(.TMGENV,.ONEREC)
        . IF TEMP<0 DO  QUIT
        . . WRITE $PIECE(TEMP,"^",2),!
        . . DO PRESS2GO^TMGUSRI2
        . KILL TMGMAP("AUTO INSTRUMENT",TMGUSERINPUT)
        . KILL TMGMNU(TMGUSERINPUT)
        IF TMGUSERINPUT="^" GOTO AMMDN
        IF TMGUSERINPUT=0 GOTO AMMDN
        GOTO AMM1
        ;
AMMDN   QUIT
        ;
FIX1MISM(TMGENV,TMGMAP) ;
        ;"Purpose: Fix one mismatch
        ;"Input: TMGENV -- PASS BY REFERENCE.  Lab environment
        ;"           TMGENV("PREFIX") -- e.g. "LMH"
        ;"           TMGENV("IEN 68.2") -- IEN in LOAD/WORK LIST (holds orderable items)
        ;"           TMGENV("IEN 62.4") -- IEN in AUTO INSTRUMENT (holds resultable items)        
        ;"           TMGENV(<other entries>)= etc.              
        ;"       TMGMAP -- Sample as below.
        ;"           TMGMAP("TEST NAME")="CBC & MORPHOLOGY (WITH DI"
        ;"           TMGMAP("HL7 SPEC")="SER"
        ;"           TMGMAP("LR60")=276
        ;"           TMGMAP("LR61")=70
        ;"           TMGMAP("LR62")=4
        ;"           TMGMAP("LR64")=2679
        ;"           TMGMAP("LR64.2")=2772
        ;"           TMGMAP("LR64 NOTE")="Warning - RESULT NLT CODE does not match UI TEST CODE."
        ;"           TMGMAP("RESULT NLT CODE")="92818.0000"
        ;"           TMGMAP("RESULT NLT NAME")="Vitamin D+metab"
        ;"           TMGMAP("SPECIMEN(IEN)")="SERUM(72)"
        ;"           TMGMAP("TEST NAME")="VITAMIN D (TOTAL)"
        ;"           TMGMAP("UI TEST CODE")="83997.0000"
        ;"           TMGMAP("UI TEST CODE","NOTE")="#62.4, IENS:59,203, 6 field"
        ;"           TMGMAP("UI TEST CODE -> NAME")="Vitamin D 25 Hydroxy"
        ;"           TMGMAP("ZZ ------------------")=""
        ;"Result: 1 if OK,  -1^Message IF error.
        DO ZWRITE^TMGZWR("TMGMAP")
        NEW % SET %=2
        WRITE !,"Change UI TEST CODE --> value of RESULT NLT CODE"
        DO YN^DICN WRITE !
        IF %'=1 GOTO F1MDN
        NEW TMGRESULT SET TMGRESULT=1
        NEW NLTCODE SET NLTCODE=$GET(TMGMAP("RESULT NLT CODE"))
        IF NLTCODE="" DO  GOTO F1MDN
        . SET TMGRESULT="-1^NLT Code missing"
        NEW IENS SET IENS=$GET(TMGMAP("IENS"))
        IF IENS="" DO  GOTO F1MDN
        . SET TMGRESULT="-1^IENS missing"
        WRITE "Changing record in AUTO INSTRUMENT (IENS=",IENS,") from '",$GET(TMGMAP("UI TEST CODE")),"' --> '",NLTCODE,"'",!
        NEW TMGFDA,TMGMSG
        SET TMGFDA(62.41,IENS,6)=NLTCODE
        DO FILE^DIE("K","TMGFDA","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO F1MDN
        . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
F1MDN   QUIT TMGRESULT
        ;
NOAIREC(TMGENV,TMGMAP) ;
        ;"NOTE: I thought the problem was that the record had not been added
        ;"      into the AUTO INSTRUMENT. But the real problem was that the UI TEST CODE
        ;"      did not match rec->60->NLT code.
        ;"      --I will leave this code here and see IF I need it later....
        ;"Purpose: handle missing entries in AUTO INSTRUMENT file.
        ;"Input: TMGENV -- PASS BY REFERENCE.  Lab environment
        ;"           TMGENV("PREFIX") -- e.g. "LMH"
        ;"           TMGENV("IEN 68.2") -- IEN in LOAD/WORK LIST (holds orderable items)
        ;"           TMGENV("IEN 62.4") -- IEN in AUTO INSTRUMENT (holds resultable items)        
        ;"           TMGENV(<other entries>)= etc.              
        ;"       TMGMAP --
        NEW TMGUSERINPUT,TMGMNU
        NEW I SET I=""
AM1     KILL TMGMNUI SET TMGMNUI=0
        SET TMGMNU(TMGMNUI)="Pick Test to Add into AUTO INSTRUMENT",TMGMNUI=TMGMNUI+1
        ;
        FOR  SET I=$ORDER(TMGMAP("AUTO INSTRUMENT",I)) QUIT:(I="")  DO
        . NEW L SET L=""
        . FOR  SET L=$ORDER(TMGMAP("AUTO INSTRUMENT",I,L)) QUIT:(L="")  DO
        . . IF $GET(TMGMAP("AUTO INSTRUMENT",I,L))'["Warning -" QUIT
        . . SET TMGMNU(TMGMNUI)=$GET(TMGMAP("AUTO INSTRUMENT",I,"TEST NAME"))_" -> "_$GET(TMGMAP("AUTO INSTRUMENT",I,"RESULT NLT CODE"))_$CHAR(9)_I,TMGMNUI=TMGMNUI+1
        ;
        SET TMGUSERINPUT=+$$MENU^TMGUSRI2(.TMGMNU,"^")
        KILL TMGMNU ;"Prevent from cluttering variable table during debug run
        ;
        IF TMGUSERINPUT>0 DO  GOTO AM1
        . NEW ONEREC MERGE ONEREC=TMGMAP("AUTO INSTRUMENT",TMGUSERINPUT)
        . NEW TEMP SET TEMP=$$FIX1AIR(.TMGENV,.ONEREC)
        . IF TEMP<0 WRITE $PIECE(TEMP,"^",2)
        IF TMGUSERINPUT="^" GOTO AMDN
        IF TMGUSERINPUT=0 SET TMGUSERINPUT=""
        GOTO AM1
        ;
AMDN    QUIT
        ;
FIX1AIR(TMGENV,TMGMAP) ;
        ;"Purpose:
        ;"Input: TMGENV -- PASS BY REFERENCE.  Lab environment
        ;"           TMGENV("PREFIX") -- e.g. "LMH"
        ;"           TMGENV("IEN 68.2") -- IEN in LOAD/WORK LIST (holds orderable items)
        ;"           TMGENV("IEN 62.4") -- IEN in AUTO INSTRUMENT (holds resultable items)        
        ;"           TMGENV(<other entries>)= etc.              
        ;"       TMGMAP -- Sample as below.
        ;"           TMGMAP("TEST NAME")="CBC & MORPHOLOGY (WITH DI"
        ;"           TMGMAP("HL7 SPEC")="SER"
        ;"           TMGMAP("LR60")=276
        ;"           TMGMAP("LR61")=70
        ;"           TMGMAP("LR62")=4
        ;"           TMGMAP("LR64")=2679
        ;"           TMGMAP("LR64.2")=2772
        ;"           TMGMAP("LR64 NOTE")="Warning - RESULT NLT CODE does not match UI TEST CODE."
        ;"           TMGMAP("RESULT NLT CODE")="84999.0000"
        ;"           TMGMAP("RESULT NLT NAME")="Hemogram Manual"
        ;"           TMGMAP("SPECIMEN(IEN)")="BLOOD(70)"
        ;"           TMGMAP("WKLD CODE")=".0000"
        ;"           TMGMAP("ZZ ------------------")=""
        ;"Result: 1 if OK,  -1^Message IF error.
        NEW TMGRESULT SET TMGRESULT=1
        NEW IEN62D4 SET IEN62D4=TMGENV("IEN 62.4")  ;"$$GETAI^TMGHL70() ;
        IF IEN62D4'>0 DO  GOTO F1ADN
        . SET TMGRESULT="-1^Unable to get AUTO INSTRUMENT to use."
        NEW TEMP SET TEMP=$ORDER(^LAB(62.4,+IEN62D4,3,"TMGTEST",+IEN60,"")) ;"Already added.
        IF TEMP>0 DO  GOTO F1ADN
        NEW TESTNAME SET TESTNAME=$GET(TMGMAP("TEST NAME"))
        IF TESTNAME="" DO  GOTO F1ADN
        . SET TMGRESULT="-1^Name of lab test missing."
        NEW NLTCODE SET NLTCODE=$GET(TMGMAP("RESULT NLT CODE"))
        IF NLTCODE="" DO  GOTO F1ADN
        . SET TMGRESULT="-1^NLT Code missing"
        NEW IEN60 SET IEN60=+$GET(TMGMAP("LR60"))
        IF IEN60'>0 DO  GOTO F1ADN
        . SET TMGRESULT="-1^Record number for file #60 missing"
        NEW IEN61 SET IEN61=+$GET(TMGMAP("LR61"))
        IF IEN61'>0 DO  GOTO F1ADN
        . SET TMGRESULT="-1^Record number for file #61 missing"
        SET TMGRESULT=$$ADD2AI^TMGHL70C(.TMGENV,TESTNAME,NLTCODE,IEN60,IEN61) ;
F1ADN   QUIT TMGRESULT
        ;
NODATANM(TMGENV,TMGHL7MSG) ;
        ;"Purpose: Fix ""Dataname ### not in accession _____ or Load/work list ___ test profile"
        ;"Input: TMGENV -- PASS BY REFERENCE.  Lab environment
        ;"           TMGENV("PREFIX") -- e.g. "LMH"
        ;"           TMGENV("IEN 68.2") -- IEN in LOAD/WORK LIST (holds orderable items)
        ;"           TMGENV("IEN 62.4") -- IEN in AUTO INSTRUMENT (holds resultable items)        
        ;"           TMGENV(<other entries>)= etc.              
        ;"       TMGHL7MSG -- PASS BY REFERENCE.   Array as created by PRSEARRY^TMGHL7X2.
        NEW ACCIDD,%,IDX
        NEW ERR SET ERR=""
        WRITE !
        WRITE "Attempt to fix problem causing Lab Error: Msg #112",!
        WRITE "---------------------------------------------------"
        WRITE !,"Must work on actual error message from the lab filling system.",!
        WRITE "Example: ",!
        WRITE "  ""Msg #112, Dataname 384 not in accession PG31880054 or Load/work list PATHGROUP LIST test profile.""",!,!
        NEW DATANAME READ "Enter 'Dataname' number (e.g. 384) from message (^ to abort): ",DATANAME:$GET(DTIME,3600),!
        IF "^"[DATANAME GOTO NDNDN
        IF +DATANAME'=DATANAME SET ERR="Dataname must be a number" GOTO NDNDN
        NEW NAME SET NAME=$PIECE($GET(^DD(63.04,DATANAME,0)),"^",1)
        IF NAME="" SET ERR="Couldn't find this dataname in FIELD #4 in LAB DATA file (#63)"  GOTO NDNDN
        WRITE "Dataname #",DATANAME," is the storage area named '",NAME,"' in file LAB DATA (63)",!
        NEW LOOKUP SET LOOKUP="CH;"_DATANAME_";1"
        NEW IEN60 SET IEN60=+$ORDER(^LAB(60,"C",LOOKUP,0))
        IF IEN60'>0 SET ERR="Couldn't find a LABORATORY TEST that uses storage '"_LOOKUP_"'"
        NEW LABTESTNAME SET LABTESTNAME=$$GET1^DIQ(60,IEN60,.01)
        NEW IEN64 SET IEN64=+$PIECE($GET(^LAB(60,IEN60,64)),"^",1)  ;"64;1 = NATIONAL VA LAB CODE
        WRITE "LABORATORY TEST '",LABTESTNAME,"' (#",IEN60,") is linked to this storage location.",!
        NEW LABVACODE SET LABVACODE=$PIECE($GET(^LAM(IEN64,0)),"^",2)
        NEW WKLDNAME SET WKLDNAME=$PIECE($GET(^LAM(IEN64,0)),"^",1)
        WRITE "    VA Workload Code: ",LABVACODE," ('"_IEN64_") -- ",WKLDNAME,!
        SET IEN64=$PIECE($GET(^LAB(60,IEN60,64)),"^",2)   ;"64;2= RESULT NLT CODE
        NEW LABNLTCODE SET LABNLTCODE=$PIECE($GET(^LAM(IEN64,0)),"^",2)        
        SET WKLDNAME=$PIECE($GET(^LAM(IEN64,0)),"^",1)
        WRITE "    NLT Code: ",LABNLTCODE," ('"_IEN64_") -- ",WKLDNAME,!        
        WRITE !
GACCID  WRITE "Next, refer again to the original error message from the lab filing system.",!
        READ "Enter accession ID (e.g. PG31880054) (^ to abort): ",ACCID:$GET(DTIME,3600),!
        IF "^"[DATANAME GOTO NDNDN
        IF '$DATA(^LRO(68,"C",ACCID)) DO  GOTO GACCID:(%=1),NDNDN
        . WRITE "No record found in ACCESSION file (#68) matching ID ",ACCID,!
        . SET %=1
        . WRITE "Try again" DO YN^DICN WRITE !
        . IF %=1 QUIT
        . SET ERR="Unable continue processing error message without matching Accession ID"
        ;"Expected index format: example -- ^LRO(68,"C","PG31880054",39,3130707,54)="" <-- should be only one of each IEN nodes
        NEW IEN1,IEN2,IEN3,IENS
        SET ERR="Unable to process 'C' index in ^LRO(68,'C', for "_ACCID 
        SET IEN1=+$ORDER(^LRO(68,"C",ACCID,0)) IF IEN1'>0 GOTO NDNDN 
        SET IEN2=+$ORDER(^LRO(68,"C",ACCID,IEN1,0)) IF IEN2'>0 GOTO NDNDN 
        SET IEN3=+$ORDER(^LRO(68,"C",ACCID,IEN1,IEN2,0)) IF IEN3'>0 GOTO NDNDN
        SET ERR=""
        SET IENS=IEN3_","_IEN2_","_IEN1_","
        NEW OROOT SET OROOT=$$GETGREF^TMGFMUT2(68.02,IENS)
        NEW CROOT SET CROOT=OROOT_IEN3_",4,IDX)"
        NEW ACCSET ;"ACCESSION SET
        NEW FOUND SET FOUND=0
        NEW SHOWN SET SHOWN=0
        WRITE "Ordered tests stored in this ACCESSION record...",!
        WRITE "------------------------------------------------",!
        SET IDX=0
        FOR  SET IDX=$ORDER(@CROOT) QUIT:(+IDX'>0)  DO
        . NEW ZN SET ZN=$GET(@CROOT@(0))
        . NEW NEWIEN60 SET NEWIEN60=$PIECE(ZN,"^",1)
        . SET ACCSET(NEWIEN60)=""
        . NEW TESTNAME SET TESTNAME=$PIECE($GET(^LAB(60,NEWIEN60,0)),"^",1)
        . WRITE "Order name: ",TESTNAME,!
        . NEW IEN64 SET IEN64=+$PIECE($GET(^LAB(60,NEWIEN60,64)),"^",1)  ;"64;1 = NATIONAL VA LAB CODE
        . NEW WKLDNAME SET WKLDNAME=$PIECE($GET(^LAM(IEN64,0)),"^",1)
        . NEW WKLD SET WKLD=$PIECE($GET(^LAM(IEN64,0)),"^",2)
        . WRITE "    Workload Code: ",WKLD," ('"_IEN64_") -- ",WKLDNAME,!
        . SET IEN64=$PIECE($GET(^LAB(60,NEWIEN60,64)),"^",2)   ;"64;2= RESULT NLT CODE
        . SET WKLDNAME=$PIECE($GET(^LAM(IEN64,0)),"^",1)
        . SET WKLD=$PIECE($GET(^LAM(IEN64,0)),"^",2)
        . WRITE "    NLT Code: ",WKLD," ('"_IEN64_") -- ",WKLDNAME,!
        . NEW ORDERSET DO EXPND60^TMGHL7U(WKLD_"^"_NEWIEN60,.ORDERSET)
        . IF '$DATA(ORDERSET("IEN60",IEN60)) DO
        . . WRITE "    LAB TEST '",LABTESTNAME,"' doesn't match this, and is not a component IF panel.",!
        . ELSE  DO  
        . . SET FOUND=1
        . WRITE !
        . SET SHOWN=1
        IF SHOWN=0 WRITE "(NONE)",!
        WRITE "------------------------------------------------",!
        SET %=2
        WRITE !,"Dump out the full accession matching ",ACCID
        DO YN^DICN WRITE !
        IF %=-1 GOTO NDNDN
        IF %=1 DO
        . DO DUMPREC^TMGDEBU3(68.02,IENS,1)
        . DO PRESS2GO^TMGUSRI2
        ;
        WRITE !
        IF FOUND=0 DO
        . WRITE "The above confirms that ",LABTESTNAME," is not in accession (order) list.",!
        . WRITE "This code is not able to determine why.",!,!
        ;
        SET FOUND=0
        SET %=2
        IF $DATA(TMGHL7MSG) DO
        . WRITE !,"A sample HL7 message has been loaded.",!
        . WRITE "Is this the same message that created Msg #112 error we are working on"
        . DO YN^DICN WRITE !
        . IF %'=1 QUIT
        . SET %=2
        . ;"WRITE !,"Dump out the full HL7 message, as parsed before passing to lab system"
        . WRITE !,"Use this HL7 message, for further analysis"
        . DO YN^DICN WRITE !
        IF %=-1 GOTO NDNDN
        IF %'=1 GOTO NDN2
        ;"DO PRESS2GO^TMGUSRI2
        WRITE !,"Cycling through order groups in the HL7 message...",!
        WRITE "----------------------------------------------------",!
        SET IDX=0
        FOR  SET IDX=$ORDER(TMGHL7MSG("HL7","GROUP",IDX)) QUIT:(+IDX'>0)  DO
        . NEW OBRIEN60 SET OBRIEN60=$GET(TMGHL7MSG("HL7","GROUP",IDX,"ORDER","IEN60"))
        . NEW OBRWKLD SET OBRWKLD=$GET(TMGHL7MSG("HL7","GROUP",IDX,"ORDER","NLT"))
        . NEW ORDERSET DO EXPND60^TMGHL7U(OBRWKLD,.ORDERSET)
        . NEW NAME SET NAME=$PIECE($GET(^LAB(60,OBRIEN60,0)),"^",1)
        . WRITE "Found ",NAME,!
        . IF $DATA(ORDERSET("IEN60",IEN60)) DO
        . . WRITE " --> MATCH!",!
        . . NEW JDX SET JDX=$ORDER(TMGHL7MSG("HL7","GROUP",IDX,"OBR",0))
        . . NEW SEG SET SEG=$GET(TMGHL7MSG("HL7","GROUP",IDX,"OBR",JDX))
        . . WRITE SEG,!,!
        . . SET FOUND=1
        . . NEW HL7TESTNAME SET HL7TESTNAME=$PIECE(SEG,"|",4)
        . . WRITE """",HL7TESTNAME,""" gets transformed in TMG code to: ",!
        . . WRITE "  ",NAME," ('",OBRIEN60,")  ",$PIECE(OBRWKLD,"^",1),!
        . . WRITE "   which matches or contains """,LABTESTNAME,""" test (",LABVACODE,")",!
        . . WRITE !,!,"(Continuing through other orders, though other matches not expected.",!
        . . DO PRESS2GO^TMGUSRI2
        . ELSE  DO
        . . WRITE "  LAB TEST '",LABTESTNAME,"' doesn't match this, and is not a component IF panel.",!
        WRITE "----------------------------------------------------",!
        ;
        IF FOUND DO
        . WRITE !,"The test WAS found in HL7 message, so it is unclear why it is missing",!
        . WRITE "  from the ACCESSION record.",!
        ELSE  DO
        . WRITE !,"The test was NOT found in HL7 message, so that probably explains why it is missing",!
        . WRITE "  from the ACCESSION record.",!        
        ;
NDN2    WRITE "More to be done.  Finish in NODATANM^TMGHL70A",! 
        ;
NDNDN   IF ERR'="" DO
        . WRITE ERR,!
        . DO PRESS2GO^TMGUSRI2
        WRITE !
        QUIT        
        ;
LWLMSMTH(TMGENV,TMGHL7MSG) ;"LOAD WORK LIST MISMATCH
        ;"Purpose
        ;"Input: TMGENV -- PASS BY REFERENCE.  Lab environment
        ;"           TMGENV("PREFIX") -- e.g. "LMH"
        ;"           TMGENV("IEN 68.2") -- IEN in LOAD/WORK LIST (holds orderable items)
        ;"           TMGENV("IEN 62.4") -- IEN in AUTO INSTRUMENT (holds resultable items)        
        ;"           TMGENV(<other entries>)= etc.              
        ;"       TMGHL7MSG -- standard parsed HL7 message
        ;"       Also uses TMGERROR, but no error IF not present (but will abort)
        ;"WRITE "Finish LDLMISMATCH^TMGHL70A"
        ;"DO PRESS2GO^TMGUSRI2
        ;"QUIT
        SET TMGERROR=$GET(TMGERROR)
        IF TMGERROR="" DO  GOTO LWLDN
        . WRITE "This handler was designed to handle alerts SET during the POC",!
        . WRITE "filing system.  This sets up TMGERROR variable.  This wasn't",!
        . WRITE "found, so can't continue.  Aborting.",!
        . DO PRESS2GO^TMGUSRI2
        
        NEW ACCIDD,%,IDX
        NEW ERR SET ERR=""
        WRITE !
        WRITE "Attempt to fix problem causing Lab Error: Msg #172",!
        WRITE "---------------------------------------------------"
        WRITE !,"Must work on actual error message from the lab filling system.",!
        WRITE "Example: ",!
        WRITE "  ""Msg #172, Order NLT Code 81132.0000 is not linked to a test in",!
        WRITE "    LOAD/WORK LIST PATHGROUP LIST profile ROUTINE for HL7 specimen",!
        WRITE "    type BLDV.",!
        NEW NLT READ "Enter 'NLT Code' number (e.g. 81132.0000) from message (^ to abort): ",NLT:$GET(DTIME,3600),!
        IF "^"[NLT GOTO LWLDN
        IF +DATANAME'=DATANAME SET ERR="Dataname must be a number" GOTO NDNDN
        NEW NAME SET NAME=$PIECE($GET(^DD(63.04,DATANAME,0)),"^",1)
        IF NAME="" SET ERR="Couldn't find this dataname in FIELD #4 in LAB DATA file (#63)"  GOTO NDNDN
        WRITE "Dataname #",DATANAME," is the storage area named '",NAME,"' in file LAB DATA (63)",!
        NEW LOOKUP SET LOOKUP="CH;"_DATANAME_";1"
        NEW IEN60 SET IEN60=+$ORDER(^LAB(60,"C",LOOKUP,0))
        IF IEN60'>0 SET ERR="Couldn't find a LABORATORY TEST that uses storage '"_LOOKUP_"'"
        NEW LABTESTNAME SET LABTESTNAME=$$GET1^DIQ(60,IEN60,.01)
        NEW IEN64 SET IEN64=+$PIECE($GET(^LAB(60,IEN60,64)),"^",1)  ;"64;1 = NATIONAL VA LAB CODE
        WRITE "LABORATORY TEST '",LABTESTNAME,"' (#",IEN60,") is linked to this storage location.",!
        NEW LABVACODE SET LABVACODE=$PIECE($GET(^LAM(IEN64,0)),"^",2)
        NEW WKLDNAME SET WKLDNAME=$PIECE($GET(^LAM(IEN64,0)),"^",1)
        WRITE "    VA Workload Code: ",LABVACODE," ('"_IEN64_") -- ",WKLDNAME,!
        SET IEN64=$PIECE($GET(^LAB(60,IEN60,64)),"^",2)   ;"64;2= RESULT NLT CODE
        NEW LABNLTCODE SET LABNLTCODE=$PIECE($GET(^LAM(IEN64,0)),"^",2)        
        SET WKLDNAME=$PIECE($GET(^LAM(IEN64,0)),"^",1)
        WRITE "    NLT Code: ",LABNLTCODE," ('"_IEN64_") -- ",WKLDNAME,!        
        WRITE !
GACCID2  WRITE "Next, refer again to the original error message from the lab filing system.",!
        READ "Enter accession ID (e.g. PG31880054) (^ to abort): ",ACCID:$GET(DTIME,3600),!
        IF "^"[DATANAME GOTO NDNDN
        IF '$DATA(^LRO(68,"C",ACCID)) DO  GOTO GACCID2:(%=1),NDNDN
        . WRITE "No record found in ACCESSION file (#68) matching ID ",ACCID,!
        . SET %=1
        . WRITE "Try again" DO YN^DICN WRITE !
        . IF %=1 QUIT
        . SET ERR="Unable continue processing error message without matching Accession ID"
        ;"Expected index format: example -- ^LRO(68,"C","PG31880054",39,3130707,54)="" <-- should be only one of each IEN nodes
        NEW IEN1,IEN2,IEN3,IENS
        SET ERR="Unable to process 'C' index in ^LRO(68,'C', for "_ACCID 
        SET IEN1=+$ORDER(^LRO(68,"C",ACCID,0)) IF IEN1'>0 GOTO NDNDN 
        SET IEN2=+$ORDER(^LRO(68,"C",ACCID,IEN1,0)) IF IEN2'>0 GOTO NDNDN 
        SET IEN3=+$ORDER(^LRO(68,"C",ACCID,IEN1,IEN2,0)) IF IEN3'>0 GOTO NDNDN
        SET ERR=""
        SET IENS=IEN3_","_IEN2_","_IEN1_","
        NEW OROOT SET OROOT=$$GETGREF^TMGFMUT2(68.02,IENS)
        NEW CROOT SET CROOT=OROOT_IEN3_",4,IDX)"
        NEW ACCSET ;"ACCESSION SET
        NEW FOUND SET FOUND=0
        NEW SHOWN SET SHOWN=0
        WRITE "Ordered tests stored in this ACCESSION record...",!
        WRITE "------------------------------------------------",!
        SET IDX=0
        FOR  SET IDX=$ORDER(@CROOT) QUIT:(+IDX'>0)  DO
        . NEW ZN SET ZN=$GET(@CROOT@(0))
        . NEW NEWIEN60 SET NEWIEN60=$PIECE(ZN,"^",1)
        . SET ACCSET(NEWIEN60)=""
        . NEW TESTNAME SET TESTNAME=$PIECE($GET(^LAB(60,NEWIEN60,0)),"^",1)
        . WRITE "Order name: ",TESTNAME,!
        . NEW IEN64 SET IEN64=+$PIECE($GET(^LAB(60,NEWIEN60,64)),"^",1)  ;"64;1 = NATIONAL VA LAB CODE
        . NEW WKLDNAME SET WKLDNAME=$PIECE($GET(^LAM(IEN64,0)),"^",1)
        . NEW WKLD SET WKLD=$PIECE($GET(^LAM(IEN64,0)),"^",2)
        . WRITE "    Workload Code: ",WKLD," ('"_IEN64_") -- ",WKLDNAME,!
        . SET IEN64=$PIECE($GET(^LAB(60,NEWIEN60,64)),"^",2)   ;"64;2= RESULT NLT CODE
        . SET WKLDNAME=$PIECE($GET(^LAM(IEN64,0)),"^",1)
        . SET WKLD=$PIECE($GET(^LAM(IEN64,0)),"^",2)
        . WRITE "    NLT Code: ",WKLD," ('"_IEN64_") -- ",WKLDNAME,!
        . NEW ORDERSET DO EXPND60^TMGHL7U(WKLD_"^"_NEWIEN60,.ORDERSET)
        . IF '$DATA(ORDERSET("IEN60",IEN60)) DO
        . . WRITE "    LAB TEST '",LABTESTNAME,"' doesn't match this, and is not a component IF panel.",!
        . ELSE  DO
        . . SET FOUND=1
        . WRITE !
        . SET SHOWN=1
        IF SHOWN=0 WRITE "(NONE)",!
        WRITE "------------------------------------------------",!
        SET %=2
        WRITE !,"Dump out the full accession matching ",ACCID
        DO YN^DICN WRITE !
        IF %=-1 GOTO NDNDN
        IF %=1 DO
        . DO DUMPREC^TMGDEBU3(68.02,IENS,1)
        . DO PRESS2GO^TMGUSRI2
        ;
        WRITE !
        IF FOUND=0 DO
        . WRITE "The above confirms that ",LABTESTNAME," is not in accession (order) list.",!
        . WRITE "This code is not able to determine why.",!,!
        ;
        SET FOUND=0
        SET %=2
        IF $DATA(TMGHL7MSG) DO
        . WRITE !,"A sample HL7 message has been loaded.",!
        . WRITE "Is this the same message that created Msg #112 error we are working on"
        . DO YN^DICN WRITE !
        . IF %'=1 QUIT
        . SET %=2
        . ;"WRITE !,"Dump out the full HL7 message, as parsed before passing to lab system"
        . WRITE !,"Use this HL7 message, for further analysis"
        . DO YN^DICN WRITE !
        IF %=-1 GOTO NDNDN
        IF %'=1 GOTO NDN2
        ;"DO PRESS2GO^TMGUSRI2
        WRITE !,"Cycling through order groups in the HL7 message...",!
        WRITE "----------------------------------------------------",!
        SET IDX=0
        FOR  SET IDX=$ORDER(TMGHL7MSG("HL7","GROUP",IDX)) QUIT:(+IDX'>0)  DO
        . NEW OBRIEN60 SET OBRIEN60=$GET(TMGHL7MSG("HL7","GROUP",IDX,"ORDER","IEN60"))
        . NEW OBRWKLD SET OBRWKLD=$GET(TMGHL7MSG("HL7","GROUP",IDX,"ORDER","NLT"))
        . NEW ORDERSET DO EXPND60^TMGHL7U(OBRWKLD,.ORDERSET)
        . NEW NAME SET NAME=$PIECE($GET(^LAB(60,OBRIEN60,0)),"^",1)
        . WRITE "Found ",NAME,!
        . IF $DATA(ORDERSET("IEN60",IEN60)) DO
        . . WRITE " --> MATCH!",!
        . . NEW JDX SET JDX=$ORDER(TMGHL7MSG("HL7","GROUP",IDX,"OBR",0))
        . . NEW SEG SET SEG=$GET(TMGHL7MSG("HL7","GROUP",IDX,"OBR",JDX))
        . . WRITE SEG,!,!
        . . SET FOUND=1
        . . NEW HL7TESTNAME SET HL7TESTNAME=$PIECE(SEG,"|",4)
        . . WRITE """",HL7TESTNAME,""" gets transformed in TMG code to: ",!
        . . WRITE "  ",NAME," ('",OBRIEN60,")  ",$PIECE(OBRWKLD,"^",1),!
        . . WRITE "   which matches or contains """,LABTESTNAME,""" test (",LABVACODE,")",!
        . . WRITE !,!,"(Continuing through other orders, though other matches not expected.",!
        . . DO PRESS2GO^TMGUSRI2
        . ELSE  DO
        . . WRITE "  LAB TEST '",LABTESTNAME,"' doesn't match this, and is not a component IF panel.",!
        WRITE "----------------------------------------------------",!
        ;
        IF FOUND DO
        . WRITE !,"The test WAS found in HL7 message, so it is unclear why it is missing",!
        . WRITE "  from the ACCESSION record.",!
        ELSE  DO
        . WRITE !,"The test was NOT found in HL7 message, so that probably explains why it is missing",!
        . WRITE "  from the ACCESSION record.",!        
        ;
        ;
LWLDN   WRITE "More to be done.  Finish in LWLMISMATCH^TMGHL70A",! 
        
