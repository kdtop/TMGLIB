TMGHL70D ;TMG/kst-HL7 transformation utility functions ;7/28/13, 2/2/14
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
 ;"Especially Editing mapped files. 
 ;"=======================================================================
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ; 
 ;"EDITATOMIC(TMGENV) --EDIT ATOMIC TEST. 
 ;"EDITWKLD(IEN60) -- EDIT WKLD CODE
 ;"EDITDN(TMGENV,IEN60) -- EDIT DATA NAME
 ;"EDITEDN -- Launch option to edit data names
 ;"EDTIXFM(IEN60) -- Allow editing of input transform.
 ;"DELMAP(TMGENV)  -- remove maping between lab code and LABORATORY TEST entry.
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"=======================================================================
 ;
EDITATOMIC(TMGENV,IEN60,INDENTN) ;
        ;"Purpose: Edit lab test in file 60
        ;"Input: TMGENV -- PASS BY REFERENCE.  Lab environment
        ;"           TMGENV("PREFIX") -- e.g. "LMH"
        ;"           TMGENV("IEN 68.2") -- IEN in LOAD/WORK LIST (holds orderable items)
        ;"           TMGENV("IEN 62.4") -- IEN in AUTO INSTRUMENT (holds resultable items)        
        ;"           TMGENV(<other entries>)= etc.              
        ;"       IEN60 -- OPTIONAL.  IEN IN LABORATORY TEST^TestName
        ;"Result: None
        ;
        ;"NEW TEMP SET TEMP=$$RUNOPT^TMGFMUT("LRDIEATOMIC")
        ;"IF +TEMP'>0 DO  GOTO AADN
        ;". WRITE $PIECE(TEMP,"^",2),!
        ;". DO PRESS2GO^TMGUSRI2
        ;"QUIT
        NEW TESTNAME,IEN64
        NEW TMGUSERINPUT,TMGMNU,TMGMNUI,WKLDNAME,WKLDCODE,DATANAME,DATAP,INXFRM
        SET IEN60=$GET(IEN60)
        IF IEN60>0 DO  GOTO EAM1
        . SET TESTNAME=$PIECE(IEN60,"^",2)
        . SET IEN60=+IEN60
        ;"NEW DIC,X,Y
        ;"SET DIC=60,DIC(0)="MAEQ"
        ;"DO ^DIC WRITE !
        NEW Y SET Y=$$SELLAB60^TMGHL70C()
        IF +Y'>0 GOTO EADN
        SET IEN60=+Y        
        SET TESTNAME=$PIECE(Y,"^",2)
        SET INDENTN=+$GET(INDENTN)
        NEW MAP,TMGTEMP         
EAM1    KILL TMGUSERINPUT,TMGMNU
        KILL TMGMNUI SET TMGMNUI=0
        KILL MAP SET TMGTEMP=$$IEN60API^TMGHL7U(.TMGENV,IEN60,.MAP)
            SET IEN64=+$GET(MAP("VA CODE"))  ;"SET IEN64=+$PIECE($GET(^LAB(60,IEN60,64)),"^",1)        
        SET WKLDNAME=$PIECE($GET(MAP("VA CODE")),"^",3)   ;"$PIECE($GET(^LAM(IEN64,0)),"^",1)
        SET WKLDCODE=$PIECE($GET(MAP("VA CODE")),"^",2)  ;"$PIECE($GET(^LAM(IEN64,0)),"^",2)
        ;"SET DATAP=+$PIECE($GET(^LAB(60,IEN60,.2)),"^",1)
        SET DATANAME=$PIECE($GET(MAP("STORAGE FLD 63.04")),"^",2)   ;"SET DATANAME=$PIECE($GET(^DD(63.04,DATAP,0)),"^",1)
        SET INXFRM=$$GET1^DIQ(60,IEN60,410)
        SET TMGMNU(TMGMNUI)="Pick Part of Atomic Test '"_TESTNAME_"' (#"_IEN60_") to Edit",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="Linked WKLD CODE --"_WKLDNAME_" ("_WKLDCODE_")"_$CHAR(9)_"WKLDCODE",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="Linked DATA NAME (storage location) -- "_DATANAME_$CHAR(9)_"DataName",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="Input Transform (storage shield) --"_$EXTRACT(INXFRM,1,25)_"..."_$CHAR(9)_"InXFRM",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="Done."_$CHAR(9)_"^",TMGMNUI=TMGMNUI+1
        ;
        SET TMGUSERINPUT=$$MENU^TMGUSRI2(.TMGMNU,"^")
        KILL TMGMNU ;"Prevent from cluttering variable table during debug run
        ;
        IF TMGUSERINPUT="WKLDCODE" DO EDITWKLD(IEN60) GOTO EAM1
        IF TMGUSERINPUT="DataName" DO  GOTO EAM1
        . IF DATANAME'="" DO EDITDN(.TMGENV,IEN60)
        . ELSE  DO
        . . NEW TEMP SET TEMP=$$LINKDN^TMGHL70C(.TMGENV,TESTNAME,IEN60,INDENTN+2)
        . . IF TEMP>0 QUIT
        . . WRITE $PIECE(TEMP,"^",2,99),!
        . . DO PRESS2GO^TMGUSRI2
        IF TMGUSERINPUT="InXFRM" DO EDTIXFM(IEN60) GOTO EAM1
        IF TMGUSERINPUT="^" GOTO EAMDN
        IF TMGUSERINPUT=0 SET TMGUSERINPUT=""
        GOTO EAM1
        ;
EAMDN    QUIT
        ;
EDITWKLD(IEN60) ;
        ;"Purpose: To edit fields 64 (and copy to 64.1) (WKLD CODE) in file #60
        NEW TESTNAME SET TESTNAME=$PIECE($GET(^LAB(60,IEN60,0)),"^",1)
        WRITE "EDIT TEST: ",TESTNAME,!
        NEW IEN64 SET IEN64=+$PIECE($GET(^LAB(60,IEN60,64)),"^",1)
        NEW DIE,DR,DA,DIC,X,Y
        SET DIC=64,DIC(0)="MAEQ"
        ;"SET DIC("A")="Select WKLD CODE: "
        ;"DO ^DIC WRITE !
        SET Y=$$ASKWKLD^TMGHL70C(TESTNAME)
        IF Y<0 GOTO EWDN
        NEW NEWIEN64 SET NEWIEN64=+Y
        IF NEWIEN64=IEN64 DO  GOTO EWDN
        . WRITE "No change needed.",!
        . DO PRESS2GO^TMGUSRI2
        NEW TMGFDA,TMGMSG
        SET TMGFDA(60,IEN60_",",64)=NEWIEN64
        SET TMGFDA(60,IEN60_",",64.1)=NEWIEN64
        DO FILE^DIE("K","TMGFDA","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO EWDN
        . WRITE "ERROR:",!
        . WRITE $$GETERRST^TMGDEBU2(.TMGMSG),!
        . DO PRESS2GO^TMGUSRI2
        ;"Here I need to fix up auto instrument file...
        NEW WKLDNAM SET WKLDNAME=$PIECE($GET(^LAM(NEWIEN64,0)),"^",1)
        NEW WKLDCODE SET WKLDCODE=$PIECE($GET(^LAM(NEWIEN64,0)),"^",2)
         NEW IEN62D41 SET IEN62D41=+$ORDER(^LAB(62.4,IEN62D4,3,"TMGTEST",IEN60,0))
        IF IEN62D41'>0 DO  GOTO EWDN
        . WRITE "Unable to find entry in AUTO INSTRUMENT file pointing to ",TESTNAME,!
        . DO PRESS2GO^TMGUSRI2
        KILL TMGFDA,TMGMSG
        SET TMGFDA(62.41,IEN62D41_","_IEN62D4_",",6)=WKLDCODE
        DO FILE^DIE("EK","TMGFDA","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO EWDN
        . WRITE "ERROR:",!
        . WRITE $$GETERRST^TMGDEBU2(.TMGMSG),!
        . DO PRESS2GO^TMGUSRI2
EWDN    QUIT
        ;
EDITDN(TMGENV,IEN60) ;
        ;"Note: the dataname is also stored as free text in field 11 in 62.41 in 62.4 (AUTO INSTRUMENT)
        ;"      It is SET by a trigger field on the TEST field at that same point.
        ;"      So DO I need to trigger that IF dataname is changed here?? (I think I probably do)
        ;"Purpose: Edit the DATANAME for file 60
        NEW DIE,DR,DA
        SET DIE=60,DA=IEN60
        SET DR=400
        DO ^DIE
        DO REFRSHDN^TMGHL70C(.TMGENV,IEN60) 
EADN    QUIT        
        ;
EDITEDN ;
        ;"Launch option to edit data names
        IF '$D(^XUSEC("LRLIASON",DUZ)) DO  GOTO EDNDN
        . WRITE !,"You don't have access to this functionality.",!
        . WRITE "YOu need the LRLIASON key.",!
        . DO PRESS2GO^TMGUSRI2
        DO ^LRWU6
        WRITE !
EDNDN   QUIT
        ;        
EDTIXFM(IEN60) ;
        ;"Purpose: Allow editing of input transform.
        NEW IEN63D04 SET IEN63D04=+$PIECE($GET(^LAB(60,IEN60,.2)),"^",1)
        IF IEN63D04'>0 DO  GOTO EXFMDN
        . WRITE "^Record #"_IEN60_" in file #60 is missing value in field #400",!
        . DO PRESS2GO^TMGUSRI2
        NEW ZNODE SET ZNODE=$GET(^DD(63.04,IEN63D04,0))
        NEW TMGCODE SET TMGCODE=$PIECE(ZNODE,"^",5,999)
        NEW NEWCODE,%,X
EXFM1        WRITE "CURRENT CODE: ",TMGCODE,!
        READ "ENTER NEW CODE (^ to abort): ",NEWCODE:$GET(DTIME,3600),!
        IF NEWCODE="^" GOTO EXFMDN
        IF NEWCODE="" GOTO EXFM1
        SET X=NEWCODE DO ^DIM ;"Code validator
        IF $DATA(X)=0 DO  GOTO EXFM1
        . WRITE "Fileman code validator rejects this code.  Please try again.",!
        . DO PRESS2GO^TMGUSRI2
        WRITE !,"Use this code:",!,"  ",NEWCODE,!,"as NEW input transform"
        SET %=2
        DO YN^DICN WRITE !
        IF %=2 GOTO EXFM1
        IF %=-1 GOTO EXFMDN
        SET ZNODE=$PIECE(ZNODE,"^",1,4)_"^"_NEWCODE
        SET ^DD(63.04,IEN63D04,0)=ZNODE
        DO PRESS2GO^TMGUSRI2
EXFMDN        QUIT
        ;
DELMAP(TMGENV,TMGTESTMSG,TMGHL7MSG)  ;"DELETE MAPPING 
        ;"Purpose: remove maping between lab code and LABORATORY TEST entry.
        ;"Input: TMGENV -- Environment array.  See definition elsewhere. 
        ;"       TMGTESTMSG 
        ;"       TMGHL7MSG 
        ;"Note: uses globally-scoped vars" TMGLABPREFIX
        ;"NOTE:  Future fix needed!!
        ;"  this removes maping between TESTID and IEN60.  However, it should ALSO ensure
        ;"  that the mapping between corresponding TESTNAME is removed.  
        ;"  It should allow user to pick from HL7 message (as is done for showing map), and then
        ;"  remove map for each...  TO DO...
        ;
        ;"WRITE !,"TO DO: change DELMAP^TMGHL70D such that can pick code, as in GETTESTFROM^TMGHL70",!,!        
        ;"WRITE "Enter lab code as found in HL7 message, e.g. OSMOC (^ to abort): "
        ;"NEW TESTID READ TESTID:$GET(DTIME,3600),!
        ;
        NEW TESTID
        IF $DATA(TMGTESTMSG)=0 DO  GOTO DLM2
        . WRITE !,"Enter lab code as found in HL7 message, e.g. OSMOC (^ to abort): "
        . READ TESTID:$GET(DTIME,3600),!
        ;        
        NEW TMGU MERGE TMGU=TMGENV("TMGU")
        NEW TEST SET TEST=$$GETTESTFROM^TMGHL70(.TMGTESTMSG,.TMGHL7MSG,.TMGU) ;"GET LAB TEST FROM TEST HL7 MESSAGE
        ;"sample return: 1989-3^Vitamin D 25-Hydroxy^LN'  //kt changed 6/5/20.  Had returned just TestID before.
        SET TESTID=$PIECE(TEST,TMGU(2),1)
        IF TEST'["^" GOTO DLM2
        NEW TESTNAME SET TESTNAME=$PIECE(TEST,TMGU(2),2)
        WRITE !,"NOTE: Mapping should be deleted by BOTH TestID AND TestName",!
        WRITE "      This is because when processing HL7 message, if test can't be",!
        WRITE "      found by ID, then the system falls back to lookup by name.",!
        WRITE "      Thus after removing TESTID map, the map might persist via TESTNAME",! 
        ;
        NEW TMGUSERINPUT,TMGMNU,TMGMNUI
DLM1    KILL TMGMNUI SET TMGMNUI=0
        SET TMGMNU(TMGMNUI)="Pick Which Mapping To DELETE",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="TestID: "_TESTID_$CHAR(9)_"TestID",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="Test Name: "_TESTNAME_$CHAR(9)_"TestName",TMGMNUI=TMGMNUI+1
        WRITE !
        SET TMGUSERINPUT=$$MENU^TMGUSRI2(.TMGMNU,"^")
        ;
        IF TMGUSERINPUT="TestID" GOTO DLM2
        IF TMGUSERINPUT="TestName" SET TESTID=TESTNAME GOTO DLM2
        IF TMGUSERINPUT="^" GOTO DMDN
        IF TMGUSERINPUT=0 SET TMGUSERINPUT=""
        GOTO DLM1
        ;                
DLM2    IF "^"[TESTID GOTO DMDN
        NEW TMGLABPREFIX SET TMGLABPREFIX=$GET(TMGENV("PREFIX"))
        NEW SYNONYM SET SYNONYM=TMGLABPREFIX_"-"_TESTID
        NEW SYN60 SET SYN60=$E(SYNONYM,1,30)  ;"//kt changed 30 -> 60
        NEW X,Y,IEN60,IEN61,IEN62,IEN64
        ;"First see IF already added.
        SET IEN60=+$ORDER(^LAB(60,"B",SYN60,""))  ;"//kt SYN30 -> SYN60
        IF IEN60'>0 DO  GOTO DMDN
        . WRITE "Can't find an existing map for that test.",!
        . DO PRESS2GO^TMGUSRI2
        SET Y=IEN60_"^"_$$GET1^DIQ(60,IEN60_",",.01)
        NEW VACODE SET VACODE=$$GET1^DIQ(60,IEN60_",","64.1:1")
        WRITE "Remove mapping between ",$PIECE(Y,"^",2)," <--> ",TESTID
        NEW % SET %=2
        DO YN^DICN WRITE !
        IF %'=1 GOTO DMDN
        NEW IEN60D1 SET IEN60D1=+$ORDER(^LAB(60,IEN60,5,"B",SYN60,""))  ;"//SYN30 -> SYN60
        IF IEN60D1'>0 DO  GOTO DMDN
        . WRITE "Unable to find '",SYN60,"' in ^LAB(60,IEN60,5,""B"") index",!
        . WRITE "Unable to remove mapping.",!
        . DO PRESS2GO^TMGUSRI2
        NEW TMGFDA,TMGMSG
        SET TMGFDA(60.1,IEN60D1_","_IEN60_",",.01)="@"
        DO FILE^DIE("EK","TMGFDA","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  QUIT
        . WRITE "ERROR:",!
        . WRITE $$GETERRST^TMGDEBU2(.TMGMSG),!
        . DO PRESS2GO^TMGUSRI2
DMDN    QUIT
        ;
 
