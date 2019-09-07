TMGHL70 ;TMG/kst-Installation/config tools for POC HL7 processing ;8/12/15, 11/14/16
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
 ;"SETUP(TMGTESTMSG) --help add NEW tests automatically to various files,
 ;"                    to enable use with POC HL7 processing.
 ;"FILEMENU(TMGTESTMSG,INDENTN) --HANDLE/SHOW FILE MENU
 ;"
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;"UTILITY(TMGENV,INDENTN) --HANDLE/SHOW UTILITY MENU
 ;"TESTPARS(TMGENV,TMGTESTMSG,TMGHL7MSG,INDENTN) -- add one test to system, so it's result can be accepted into VistA
 ;"PRSMSH(LINE,ARRAY) -- Parse MSH segment  DEPRECIATED
 ;"MAPMENU(TMGENV,INDENTN) -- show Mapping menu, and interact with user...
 ;"VMPCK(TMGENV)  -- VIEW MAP, PICKING TYPE TO VIEW. 
 ;"VIEWMNLT(TMGENV,NLT) -- VIEW NATIONAL LABORATORY TEST (NLT) MAPPING. 
 ;"VIEWMAP(TMGENV,TESTID) -- Show maping between lab code and LABORATORY TEST entry.
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;" TMGEDIT,TMGUSRIF, TMGDEBUG
 ;" Note: this uses Linux functionality.
 ;"=======================================================================
 ;
SETUP(TMGTESTMSG,INDENTN) ;
        ;"Purpose: To help add NEW tests automatically to various files,
        ;"         to enable use with POC HL7 processing.
        ;"Input: TMGTESTMSG -- optional.  This can be the message to work
        ;"              on.  If not provided, then user will be prompted to
        ;"              load one in via an editor.  Input format:
        ;"              TMGTESTMSG(1)="1st line"
        ;"              TMGTESTMSG(2)="2nd line etc."
        ;"       INDENTN -- OPTIONAL.  the number of spaces to indent the menu display        
        ;"Note: uses globally-scoped vars" TMGLABPREFIX, IEN62D4
        ;"Result: None
        NEW TMGUSERINPUT,TMGHL7MSG,TMGENV
        NEW TMGRESULT,IEN22720 
        NEW TMGMNU,TMGMNUI,TMGTEMP
        SET INDENTN=+$GET(INDENTN)
        NEW INDENTSTR SET INDENTSTR=$J("",INDENTN)
        WRITE !,!
        WRITE INDENTSTR,"Welcome to the TMG HL7 Message Lab Setup Assistant.",!
        WRITE INDENTSTR,"-------------------------------------------",!
SU1     SET TMGRESULT=$$SETUPENV^TMGHL7U(.TMGTESTMSG,.TMGENV,1) 
        SET TMGENV("INTERACTIVE MODE")=1
        IF TMGRESULT'>0 GOTO SUDN
        SET IEN22720=TMGENV("IEN 22720")                
M1      KILL TMGUSERINPUT,TMGMNU
        KILL TMGMNUI SET TMGMNUI=0
        SET TMGMNU(-1,"INDENT")=INDENTN
        SET TMGMNU(TMGMNUI)="Pick HL7 Lab Setup Option for "_TMGENV("INST"),TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="Setup/Test HL7 message test result from lab provider"_$CHAR(9)_"TestParse",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="HL7 Message Transform <MENU>"_$CHAR(9)_"XFRMMenu",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="HL7 Message FILE <MENU>"_$CHAR(9)_"FileMenu",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="Utility <MENU>"_$CHAR(9)_"UtilMenu",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="Pick lab source other than "_TMGENV("INST")_" to work on."_$CHAR(9)_"OtherInst",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="Done setting up NEW HL7 Test."_$CHAR(9)_"^",TMGMNUI=TMGMNUI+1
        ;
        SET TMGUSERINPUT=$$MENU^TMGUSRI2(.TMGMNU,"^")
        KILL TMGMNU ;"Prevent from cluttering variable table during debug run
        ;
        IF TMGUSERINPUT="TestParse" DO TESTPARS(.TMGENV,.TMGTESTMSG,.TMGHL7MSG,INDENTN+2) GOTO M1
        IF TMGUSERINPUT="PasteMsg" DO LOADMSG^TMGHL7U2(.TMGTESTMSG) GOTO M1
        IF TMGUSERINPUT="UtilMenu" DO UTILITY(.TMGENV,INDENTN+2) GOTO M1
        IF TMGUSERINPUT="FileMenu" DO FILEMENU(.TMGTESTMSG,INDENTN+2) GOTO M1
        IF TMGUSERINPUT="XFRMMenu" DO SETUP^TMGHL7S(.TMGENV,.TMGTESTMSG,INDENTN+2) GOTO M1
        ;
        IF TMGUSERINPUT="OtherInst" DO  GOTO SU1
        . KILL ^TMG("TMP","TMGHL70","IEN 68.2")
        . KILL ^TMG("TMP","TMGHL70","IEN 62.4")
        ;"IF TMGUSERINPUT="EditTest" DO EDITTEST GOTO M1
        IF TMGUSERINPUT="MapMenu" DO MAPMENU(.TMGENV,INDENTN+2) GOTO M1
        IF TMGUSERINPUT="LRWU5" DO ^LRWU5 GOTO M1
        IF TMGUSERINPUT="Dump" DO ASKDUMP^TMGDEBU3 GOTO M1
        IF TMGUSERINPUT="^" GOTO SUDN
        IF TMGUSERINPUT=0 SET TMGUSERINPUT=""
        GOTO M1
        ;
SUDN    WRITE "Quitting.  Goodbye",!
        QUIT
        ;
UTILITY(TMGENV,INDENTN) ;"HANDLE/SHOW UTILITY MENU
        ;"Input: TMGENV -- PASS BY REFERENCE.  Lab environment
        ;"           TMGENV("PREFIX") -- e.g. "LMH"
        ;"           TMGENV("IEN 68.2") -- IEN in LOAD/WORK LIST (holds orderable items)
        ;"           TMGENV("IEN 62.4") -- IEN in AUTO INSTRUMENT (holds resultable items)        
        ;"           TMGENV(<other entries>)= etc.              
        ;"       INDENTN -- the number of spaces to indent the menu display
        ;"Result: none
        NEW TMGUSERINPUT,TMGMNU,TMGMNUI,TMGTEMP
UT1     KILL TMGUSERINPUT,TMGMNU
        ;
        KILL TMGMNUI SET TMGMNUI=0
        SET TMGMNU(-1,"INDENT")=INDENTN
        SET TMGMNU(TMGMNUI)="Pick HL7 Lab Setup Utilty Option for "_TMGENV("INST"),TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="Modify an existing data name"_$CHAR(9)_"LRWU6",TMGMNUI=TMGMNUI+1
        ;"SET TMGMNU(TMGMNUI)="Add atomic data name"_$CHAR(9)_"LRWU5",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="Add atomic lab tests"_$CHAR(9)_"AddAtomic",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="Edit atomic lab tests"_$CHAR(9)_"EditAtomic",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="Check/Fix mapping of tests <MENU>"_$CHAR(9)_"MapMenu",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="View arbitrary record in file."_$CHAR(9)_"Dump",TMGMNUI=TMGMNUI+1
        ;"SET TMGMNU(TMGMNUI)="<HL7 Message FILE MENU>"_$CHAR(9)_"FileMenu",TMGMNUI=TMGMNUI+1

        SET TMGMNU(TMGMNUI)="Done"_$CHAR(9)_"^",TMGMNUI=TMGMNUI+1
        ;
        SET TMGUSERINPUT=$$MENU^TMGUSRI2(.TMGMNU,"^")
        KILL TMGMNU ;"Prevent from cluttering variable table during debug run
        ;
        IF TMGUSERINPUT="MapMenu" DO MAPMENU(.TMGENV,INDENTN+2) GOTO UT1
        IF TMGUSERINPUT="LRWU5" DO ^LRWU5 GOTO UT1
        IF TMGUSERINPUT="LRWU6" DO EDITEDN^TMGHL70D GOTO UT1
        IF TMGUSERINPUT="AddAtomic" SET TMGTEMP=$$ADDATOMIC^TMGHL70C() GOTO UT1
        IF TMGUSERINPUT="Dump" DO ASKDUMP^TMGDEBU3 GOTO UT1
        IF TMGUSERINPUT="EditAtomic" DO EDITATOMIC^TMGHL70D(.TMGENV) GOTO UT1
        ;"IF TMGUSERINPUT="FileMenu" DO FILEMENU(.TMGTESTMSG,INDENTN+2) GOTO UT1
        IF TMGUSERINPUT="^" GOTO UTDN
        IF TMGUSERINPUT=0 SET TMGUSERINPUT=""
        GOTO UT1
        ;
UTDN    QUIT
        ;
FILEMENU(TMGTESTMSG,INDENTN) ;"HANDLE/SHOW FILE MENU
        ;"Input: TMGENV -- PASS BY REFERENCE.  Lab environment
        ;"           TMGENV("PREFIX") -- e.g. "LMH"
        ;"           TMGENV("IEN 68.2") -- IEN in LOAD/WORK LIST (holds orderable items)
        ;"           TMGENV("IEN 62.4") -- IEN in AUTO INSTRUMENT (holds resultable items)        
        ;"           TMGENV(<other entries>)= etc.              
        ;"       TMGTESTMSG -- the message to work on.
        ;"       INDENTN -- the number of spaces to indent the menu display
        ;"NOTE: may access IEN772,IEN773,HLMIEN,HLMIENS, but no error IF absent.
        ;"Result: none
        NEW TMGUSERINPUT,TMGMNU,TMGMNUI
        IF '$DATA(IEN772),$DATA(HLMIEN) MERGE IEN772=HLMIEN
        IF '$DATA(IEN773),$DATA(HLMIENS) MERGE IEN773=HLMIENS
        IF $DATA(IEN773),'$DATA(IEN772) NEW IEN773 ;"If both not present, hide present
        IF $DATA(IEN772),'$DATA(IEN773) NEW IEN772 ;"If both not present, hide present
        ;
FMU1    KILL TMGUSERINPUT,TMGMNU
        ;
        KILL TMGMNUI SET TMGMNUI=0
        SET TMGMNU(-1,"INDENT")=INDENTN
        SET TMGMNU(TMGMNUI)="Pick HL7 Message FILE Option",TMGMNUI=TMGMNUI+1
        IF $DATA(TMGTESTMSG)=0 DO
        . SET TMGMNU(TMGMNUI)="Load a test HL7 message from host text file"_$CHAR(9)_"LoadMsg",TMGMNUI=TMGMNUI+1
        . SET TMGMNU(TMGMNUI)="Paste a test HL7 message into editor"_$CHAR(9)_"PasteMsg",TMGMNUI=TMGMNUI+1
        . SET TMGMNU(TMGMNUI)="Load a test HL7 message from HL7 MESSAGE TEXT file"_$CHAR(9)_"LoadFMMsg",TMGMNUI=TMGMNUI+1
        ELSE  DO
        . SET TMGMNU(TMGMNUI)="View current HL7 message"_$CHAR(9)_"ViewMsg",TMGMNUI=TMGMNUI+1
        . SET TMGMNU(TMGMNUI)="Clear currently loaded test HL7 message"_$CHAR(9)_"ClearMsg",TMGMNUI=TMGMNUI+1
        . SET TMGMNU(TMGMNUI)="Save currently loaded test HL7 message to HFS file"_$CHAR(9)_"SaveMsg",TMGMNUI=TMGMNUI+1
        . SET TMGMNU(TMGMNUI)="Edit loaded HL7 test message"_$CHAR(9)_"EditMsg",TMGMNUI=TMGMNUI+1
        . IF $DATA(IEN772)&$DATA(IEN773) DO
        . . SET TMGMNU(TMGMNUI)="Save currently loaded test HL7 message to Fileman File"_$CHAR(9)_"SaveMsgFM",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="Done with FILE options."_$CHAR(9)_"^",TMGMNUI=TMGMNUI+1
        ;
        SET TMGUSERINPUT=$$MENU^TMGUSRI2(.TMGMNU,"^")
        KILL TMGMNU ;"Prevent from cluttering variable table during debug run
        ;
        IF TMGUSERINPUT="PasteMsg" DO LOADMSG^TMGHL7U2(.TMGTESTMSG) GOTO FMU1
        IF TMGUSERINPUT="LoadMsg" DO LOADMSG2^TMGHL7U2(.TMGTESTMSG) GOTO FMU1
        IF TMGUSERINPUT="LoadFMMsg" DO LOADMSGF^TMGHL7U2(.TMGTESTMSG) GOTO FMU1        
        IF TMGUSERINPUT="ClearMsg" KILL TMGTESTMSG GOTO FMU1
        IF TMGUSERINPUT="ViewMsg" DO VIEWMSG^TMGHL7U2(.TMGTESTMSG) GOTO FMU1
        IF TMGUSERINPUT="EditMsg" DO EDITMSG^TMGHL7U2(.TMGTESTMSG) GOTO FMU1
        IF TMGUSERINPUT="SaveMsg" DO SAVEMSG^TMGHL7U2(.TMGTESTMSG) GOTO FMU1
        IF TMGUSERINPUT="SaveMsgFM" DO SAVMSGFM^TMGHL7U2(.TMGTESTMSG,.IEN772,.IEN773) GOTO FMU1
        IF TMGUSERINPUT="^" GOTO FMUDN
        IF TMGUSERINPUT=0 SET TMGUSERINPUT=""
        GOTO FMU1
        ;
FMUDN   QUIT
        ;
TESTPARS(TMGENV,TMGTESTMSG,TMGHL7MSG,INDENTN) ;
        ;"Purpose: Test parse, and add one test to system, so it's result can be accepted into VistA
        ;"         If problems are found, user will be queried to correct problems. 
        ;"Input: TMGENV -- PASS BY REFERENCE.  Lab environment
        ;"           TMGENV("PREFIX") -- e.g. "LMH"
        ;"           TMGENV("IEN 68.2") -- IEN in LOAD/WORK LIST (holds orderable items)
        ;"           TMGENV("IEN 62.4") -- IEN in AUTO INSTRUMENT (holds resultable items)        
        ;"           TMGENV(<other entries>)= etc.              
        ;"       TMGTESTMSG -- the message to work on.
        ;"       TMGHL7MSG --PASS BY REFERENCE.  AN OUT PARAMETER.
        ;"Result: none
        NEW TMGRESULT SET TMGRESULT=1
        SET INDENTN=+$GET(INDENTN)
        NEW INDENTSTR SET INDENTSTR=$JUSTIFY("",INDENTN)
        IF $DATA(TMGTESTMSG)>0 DO
        . SET TMGENV("INDENTN")=INDENTN
        . SET TMGRESULT=$$PARSMSG2^TMGHL7X2(.TMGENV,.TMGTESTMSG,.TMGHL7MSG)
        . ;"---------------------------------------------------
        . ;"//kt added below 4/9/19 because a test was failing during full parse, but succeeding here and thus couldn't fix.              
        . IF TMGRESULT<0 QUIT
        . SET TMGHL7MSG("STAGE")="PRE"
        . SET TMGRESULT=$$XFMSG^TMGHL7X(.TMGENV,.TMGHL7MSG)
        . IF TMGRESULT<0 QUIT
        . SET TMGRESULT=$$DOMORE^TMGHL7X2(.TMGENV,.TMGHL7MSG)
        . IF TMGRESULT<0 QUIT
        . SET TMGHL7MSG("STAGE")="FINAL"
        . SET TMGRESULT=$$XFMSG^TMGHL7X(.TMGENV,.TMGHL7MSG)
        . SET TMGHL7MSG("STAGE")=""
        . ;"---------------------------------------------------
        ELSE  DO
        . SET TMGRESULT="-1^No message provided to parse, in TESTPARS.TMGHL70"
        WRITE !,INDENTSTR        
        IF TMGRESULT<0 DO  
        . WRITE $PIECE(TMGRESULT,"^",2),!
        ELSE  DO  
        . WRITE "Transform and processing of HL7 message was OK.",! 
        WRITE INDENTSTR        
        DO PRESS2GO^TMGUSRI2
        WRITE !
ADDTDN  QUIT
        ;
MAPMENU(TMGENV,INDENTN) ;
        ;"Purpose: show Mapping menu, and interact with user...
        ;"Input: TMGENV -- PASS BY REFERENCE.  Lab environment
        ;"           TMGENV("PREFIX") -- e.g. "LMH"
        ;"           TMGENV("IEN 68.2") -- IEN in LOAD/WORK LIST (holds orderable items)
        ;"           TMGENV("IEN 62.4") -- IEN in AUTO INSTRUMENT (holds resultable items)        
        ;"           TMGENV(<other entries>)= etc.              
        ;"       INDENTN -- the number of spaces to indent the menu display
        ;"Result: none
        NEW TMGUSERINPUT,TMGMNU,TMGMNUI
MMM1    SET TMGMNUI=0
        SET TMGMNU(-1,"INDENT")=INDENTN
        SET TMGMNU(TMGMNUI)="Check, Fix, Edit Mapping of tests",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="Check/Fix mapping of tests"_$CHAR(9)_"CheckMap",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="POC mapping display of tests"_$CHAR(9)_"ShowMap",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="Show map of 1 test"_$CHAR(9)_"VIEWMAP",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="Remove mapping of 1 test"_$CHAR(9)_"DelMap",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="View arbitrary record in file."_$CHAR(9)_"Dump",TMGMNUI=TMGMNUI+1
        ;
        WRITE !
        SET TMGUSERINPUT=$$MENU^TMGUSRI2(.TMGMNU,"^")
        KILL TMGMNU ;"Prevent from cluttering variable table during debug run
        ;
        IF TMGUSERINPUT="CheckMap" DO TESTMAP^TMGHL70A(.TMGENV,.TMGTESTMSG,.TMGHL7MSG) GOTO MMM1
        IF TMGUSERINPUT="ShowMap" DO PRINT^LA7PCFG GOTO MMM1
        IF TMGUSERINPUT="DelMap" DO DELMAP^TMGHL70D(.TMGENV) GOTO MMM1
        IF TMGUSERINPUT="VIEWMAP" DO VMPCK(.TMGENV,INDENTN) GOTO MMM1
        IF TMGUSERINPUT="Dump" DO ASKDUMP^TMGDEBU3 GOTO MMM1
        IF TMGUSERINPUT="^" GOTO MMDN
        IF TMGUSERINPUT=0 SET TMGUSERINPUT=""
        GOTO MMM1
MMDN    QUIT
        ;
VMPCK(TMGENV,INDENTN)  ;"VIEW MAP, PICKING TYPE TO VIEW. 
        ;"Input: TMGENV -- PASS BY REFERENCE.  Lab environment
        ;"           TMGENV("PREFIX") -- e.g. "LMH"
        ;"           TMGENV("IEN 68.2") -- IEN in LOAD/WORK LIST (holds orderable items)
        ;"           TMGENV("IEN 62.4") -- IEN in AUTO INSTRUMENT (holds resultable items)        
        ;"           TMGENV(<other entries>)= etc.              
        ;"Result: none
        NEW TMGUSERINPUT,TMGMNU,TMGMNUI
VMPM1   KILL TMGMNUI SET TMGMNUI=0
        SET TMGMNU(-1,"INDENT")=INDENTN
        SET TMGMNU(TMGMNUI)="Pick Type of 1-Test Mapping To View",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="HL7: Show map of test code from HL7 message (from lab provider)"_$CHAR(9)_"FromLab",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="NLT: Show map of transformed NLT code"_$CHAR(9)_"NLTMap",TMGMNUI=TMGMNUI+1
        WRITE !
        SET TMGUSERINPUT=$$MENU^TMGUSRI2(.TMGMNU,"^")
        KILL TMGMNU ;"Prevent from cluttering variable table during debug run
        ;
        IF TMGUSERINPUT="FromLab" DO VIEWMAP(.TMGENV) GOTO VMPM1
        IF TMGUSERINPUT="NLTMap" DO VIEWMNLT(.TMGENV) GOTO VMPM1
        IF TMGUSERINPUT="^" GOTO VMPDN
        IF TMGUSERINPUT=0 SET TMGUSERINPUT=""
        GOTO VMPM1
VMPDN   QUIT
        ;        
VIEWMNLT(TMGENV,NLT)   ;"VIEW NATIONAL LABORATORY TEST (NLT) MAPPING. 
        ;"Purpose: View mapping from NLT code (used during actual filing of lab)
        ;"Input: TMGENV -- PASS BY REFERENCE.  Lab environment
        ;"           TMGENV("PREFIX") -- e.g. "LMH"
        ;"           TMGENV("IEN 68.2") -- IEN in LOAD/WORK LIST (holds orderable items)
        ;"           TMGENV("IEN 62.4") -- IEN in AUTO INSTRUMENT (holds resultable items)                
        ;"           TMGENV(<other entries>)= etc.              
        ;"       NLT -- Optional.  If not provided, then user is prompted for value.
        ;"              This is WKLD CODE (e.g. "81172.0000"), which is 1 field in file 64 (WKLD CODE)
        ;"Result: none
        SET IEN62D4=TMGENV("IEN 62.4")
        SET NLT=$GET(NLT)
        IF NLT="" DO
        . NEW IEN64,DIC,X,Y
        . SET DIC=64,DIC(0)="MAEQ",DIC("A")="Select WKLD CODE / NLT CODE: "
        . DO ^DIC WRITE !
        . IF +Y'>0 QUIT
        . SET NLT=$PIECE($GET(^LAM(+Y,0)),"^",2)
        IF NLT="" DO  GOTO VMNLTDN
        . WRITE "NLT CODE not specified, so can't show map.",!
        NEW MAP,TMGRESULT
        SET TMGRESULT=$$LMAPAPI2^TMGHL7U(.TMGENV,NLT,.MAP) ;"Get actual mapping
        IF TMGRESULT'>0 DO  GOTO VMNLTDN
        . WRITE TMGRESULT,!
        NEW STR SET STR=$GET(MAP(NLT,"IEN64"))        
        WRITE !,"Via C index in file 64 (WKLD CODE) ...",!
        WRITE "  """,NLT," "" --> WKLD CODE #",+STR,", Name: """,$PIECE(STR,"^",3),"""",!
        WRITE !,"Via AC index in file 62.4 (AUTO INSTRUMENT) ...",!
        SET STR=$GET(MAP(NLT,"AUTO INSTRUMENT TEST"))
        WRITE "  """,NLT,""" --> """,$PIECE(STR,"^",2),""" (`",$PIECE(STR,"^",1),")",!
        SET STR=$GET(MAP(NLT,"STORAGE"))
        NEW FIELD SET FIELD=$PIECE(STR,"^",2) IF FIELD="" SET FIELD="??"
        NEW FLDNAME SET FLDNAME=$PIECE(STR,"^",3) IF FLDNAME="" SET FLDNAME="??" 
        WRITE "    Storage: ",$PIECE(STR,"^",1)," --> """,FLDNAME,""" field (#",FIELD,") in 63.04 (CHEM...), in 63 (LAB DATA)",!
        WRITE !
VMNLTDN DO PRESS2GO^TMGUSRI2
        QUIT
        ;        
VIEWMAP(TMGENV,TESTID) ;
        ;"Purpose: Show maping between lab code and LABORATORY TEST entry.
        ;"Input: TMGENV -- PASS BY REFERENCE.  Lab environment
        ;"           TMGENV("PREFIX") -- e.g. "LMH"
        ;"           TMGENV("IEN 68.2") -- IEN in LOAD/WORK LIST (holds orderable items)
        ;"           TMGENV("IEN 62.4") -- IEN in AUTO INSTRUMENT (holds resultable items)                
        ;"           TMGENV(<other entries>)= etc.              
        ;"       TESTID -- Optional.  If not provided, then user is prompted for value. 
        ;"Note: uses globally-scoped vars" TMGLABPREFIX, IEN62D4, IEN68D2, TMGTESTMSG
        NEW TMGZZ SET TMGZZ=0
        IF TMGZZ=1 DO
        . KILL TMGENV
        . MERGE TMGENV=^TMG("TMP","VIEWMAP^TMGHL70","TMGENV") 
        ELSE  DO
        . KILL ^TMG("TMP","VIEWMAP^TMGHL70")
        . MERGE ^TMG("TMP","VIEWMAP^TMGHL70","TMGENV")=TMGENV 
        NEW X,Y,IEN60,IEN61,IEN62,IEN64,TEMP
        NEW TESTNAME,%,VACODE,NEWIEN60,IEN62D41,SYN60,SYNONYM,TMGRESULT
        SET TESTID=$GET(TESTID)
VM1     IF TESTID="" DO
        . IF $DATA(TMGTESTMSG) DO
        . . NEW TMGU MERGE TMGU=TMGENV("TMGU")
        . . SET TESTID=$$GETIDFRM(.TMGTESTMSG,.TMGU) ;"GET TEST ID FROM TEST HL7 MESSAGE
        . ELSE  DO
        . . WRITE !,"Enter lab code as found in HL7 message, e.g. OSMOC (^ to abort): "
        . . READ TESTID:$GET(DTIME,3600),!
        IF "^"[TESTID GOTO VMDN
        WRITE "-----",!
        WRITE "Using this TMGENV:",!
        IF $DATA(TMGENV) ZWRITE TMGENV(*)
        WRITE "-----",!
        NEW ARR
        SET TMGRESULT=$$LMAPAPI^TMGHL7U(.TMGENV,TESTID,.ARR) ;"Get actual mapping
        DO
        . WRITE "-----",!
        . WRITE "Using this mapping array (from $$LMAPAPI^TMGHL7U):",!
        . DO ArrayDump^TMGIDE($NAME(ARR(TESTID)))
        . WRITE "-----",!
        
        SET SYNONYM=$GET(ARR(TESTID,"SYNONYM"))
        SET SYN60=$GET(ARR(TESTID,"SYN60"))  ;"//kt changed 30 -> 60
        WRITE "Using '",SYNONYM,"' as a synonym for a lab test in File# 60, via 'B' index...",!
        ;"First see if already added.
        SET IEN60=$GET(ARR(TESTID,"MAP SYN->60"))  ;"IEN^NAME
        IF (+TMGRESULT<0)&($PIECE(TMGRESULT,"^",2)=1) DO  GOTO VMDN
        . WRITE $PIECE(TMGRESULT,"^",3),!
        . DO PRESS2GO^TMGUSRI2
        SET TESTNAME=$PIECE(IEN60,"^",2)
        WRITE "Current map is:",!
        WRITE " ",TESTID," -->",!
        WRITE "   ",$PIECE(IEN60,"^",2),"(#",+IEN60," in file #60)",!
        SET VACODE=$GET(ARR(TESTID,"VA CODE")) ;"IEN64^WkLdcode^Name
        WRITE "     NATIONAL VA LAB CODE: #",+VACODE," (in File# 64), ",$P(VACODE,"^",2)," -- ",$P(VACODE,"^",3),!
        NEW NLTCODE SET NLTCODE=$GET(ARR(TESTID,"NLT CODE")) ;"IEN64^NLTCode^Name
        WRITE "     RESULT NLT CODE: #",+NLTCODE," (in File# 64)tig, ",$PIECE(NLTCODE,"^",2)," -- ",$P(NLTCODE,"^",3),!
VM2     WRITE !
        WRITE "In LOAD/WORK LIST (#68.2) (holds orderable items):",!
        NEW IEN68D24 SET IEN68D24=$GET(ARR(TESTID,"ORDERABLES","MAP 60->68.24"))  ;"IEN68D24
        IF (+TMGRESULT<0)&($PIECE(TMGRESULT,"^",2)=2) DO  GOTO VMDN
        . WRITE "  ",$PIECE(TMGRESULT,"^",3),!
        . DO PRESS2GO^TMGUSRI2
        WRITE " PROFILE(subrec# 1):TEST(subrec# ",IEN68D24,"):TEST --> (#",+IEN60," in file #60) ",$PIECE(IEN60,"^",2),!
        WRITE !
        WRITE "In AUTO INSTRUMENT (#62.4) file (holds resultable items):",!
        ;"Look up NLT code in auto instrument file, and get pointed to ien60.
        SET IEN62D41=$GET(ARR(TESTID,"RESULTABLES","MAP NLT->62.41")) ;"IEN62D41
        IF (+TMGRESULT<0)&($PIECE(TMGRESULT,"^",2)=3) DO  GOTO VMDN
        . WRITE "  ",$PIECE(TMGRESULT,"^",3),!
        . DO PRESS2GO^TMGUSRI2
        WRITE "  ",$PIECE(NLTCODE,"^",2),", via cross reference 'AC', --> CHEM TESTS (subrec# ",IEN62D41,")",!
        SET NEWIEN60=$GET(ARR(TESTID,"RESULTABLES","MAP 62.41->60"))  ;"IEN60^NAME
        WRITE "    --> (#",+NEWIEN60," in File #60) ",$PIECE(NEWIEN60,"6",2),!
        IF +TMGRESULT=0 DO
        . WRITE !,"***PROBLEM DETECTED***  Notice that pointed-to tests (file #60) are different!",!
        WRITE !
VM3     SET %=2
        WRITE "View entire record for test ",TESTNAME DO YN^DICN WRITE !
        IF %=1 DO
        . DO DUMPREC^TMGDEBU3(60,+IEN60,0)
        . WRITE !
VMDN    QUIT
        ;
GETIDFRM(TESTMSG,TMGU) ;"GET TEST ID FROM TEST HL7 MESSAGE
        ;"Input: TESTMSG -- PASS BY REFERENCE.  FORMAT:
        ;"          TESTMSG(#)=<TEXT>, E.g. TESTMSG(133)= "NTE|1|L|Interpretation of Vitamin D 25 OH:|"
        ;"       TMGU -- ARRAY WITH DIVIDER INFO, E.G. 
        ;"       TMGU(1)="|"
        ;"       TMGU(2)="^"
        ;"       TMGU(3)="~"
        ;"       TMGU(4)="\"
        ;"       TMGU(5)="&"
        ;"Result: returns 'testID', or "" if none chosen.    e.g., if user picks:
        ;"         Vitamin D 25-Hydroxy (ID: 1989-3), then '1989-3"
        NEW MENU,TMGUSERINPUT,MENUCT SET MENUCT=0
        SET MENU(0)="Select lab result from test message to show mapping."
        NEW TMGRESULT SET TMGRESULT=""
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(TESTMSG(IDX)) QUIT:IDX'>0  DO
        . NEW LINE SET LINE=$GET(TESTMSG(IDX)) QUIT:LINE=""
        . NEW TYPE SET TYPE=$PIECE(LINE,TMGU(1),1)
        . IF TYPE'="OBX" QUIT
        . NEW LAB SET LAB=$PIECE(LINE,TMGU(1),4) QUIT:LAB=""
        . NEW ID SET ID=$PIECE(LAB,TMGU(2),1)
        . NEW LABNAME SET LABNAME=$PIECE(LAB,TMGU(2),2)
        . SET MENUCT=MENUCT+1,MENU(MENUCT)=LABNAME_" (ID: "_ID_")"_$CHAR(9)_ID
        SET MENUCT=MENUCT+1,MENU(MENUCT)="Manual entry of a lab code (e.g. OSMOC)"_$CHAR(9)_"<MANUAL>" 
        SET TMGUSERINPUT=$$MENU^TMGUSRI2(.MENU,"^")
        IF TMGUSERINPUT="<MANUAL>" DO
        . WRITE !,"Enter lab code as found in HL7 message, e.g. OSMOC (^ to abort): "
        . READ TMGUSERINPUT:$GET(DTIME,3600),!
        IF TMGUSERINPUT="^" SET TMGUSERINPUT=""
        SET TMGRESULT=TMGUSERINPUT
        QUIT TMGRESULT
        ;
GETCFG(HL7INST,HL7APP) ;"DEPRECIATED
        ;"Purpose: To get TMGH HL7 MESSAGE TRANSFORM SETTINGS 
        ;"Input: HL7INST -- Institution, as found in HL7 message, piece #4.
        ;"       HL7APP -- Sending applications, as found in HL7 message, piece #3.
        ;"Result: IEN in 22720 or -1^message if not found or problem. 
        NEW IEN22720 SET IEN22720=0
        SET HL7INST=$GET(HL7INST,"?")
        SET HL7APP=$GET(HL7APP)
        NEW FOUND SET FOUND=0
        FOR  QUIT:FOUND  SET IEN22720=+$ORDER(^TMG(22720,"D",HL7INST,IEN22720)) QUIT:(+IEN22720'>0)!FOUND  DO
        . IF HL7APP="" SET FOUND=1 QUIT  ;"If not APP specified, then use first found. 
        . IF $DATA(^TMG(22720,"EAPP",HL7APP,IEN22720))>0 SET FOUND=1 QUIT
        IF FOUND'>0 SET IEN22720=0
        IF IEN22720'>0 DO
        . SET IEN22720="-1^Can't find entry in 22720 matching SENDING FACILITY=["_HL7INST_"], SENDING APPLICATION=["_HL7APP_"]"         
        QUIT IEN22720
        ;
GETCFG2(MSH,TMGU,HL7INST,HL7APP) ;
        ;"Purpose: To get TMGH HL7 MESSAGE TRANSFORM SETTINGS 
        ;"Input: MSH -- the MSH segment of the HL7 message
        ;"       TMGU -- the array with delimeters
        ;"       HL7INST -- an OUT PARAMETER
        ;"       HL7APP -- an OUT PARAMETER.  
        ;"Result: IEN in 22720 or -1^message if not found or problem.
        
        SET HL7INST=$PIECE($PIECE(MSH,TMGU(1),4),TMGU(2),1)
        SET HL7APP=$PIECE($PIECE(MSH,TMGU(1),3),TMGU(2),1)
        NEW MSGTYPE SET MSGTYPE=$PIECE($PIECE(MSH,TMGU(1),9),TMGU(2),1)
        NEW IEN22720 SET IEN22720=0
        SET HL7INST=$GET(HL7INST,"?")
        SET HL7APP=$GET(HL7APP)
        NEW FOUND SET FOUND=0
        FOR  QUIT:FOUND  SET IEN22720=+$ORDER(^TMG(22720,"D",HL7INST,IEN22720)) QUIT:(+IEN22720'>0)!FOUND  DO
        . IF HL7APP="" SET FOUND=1 QUIT  ;"If not APP specified, then use first found. 
        . NEW SUBIEN SET SUBIEN=0
        . NEW SKIP SET SKIP=1
        . FOR  SET SUBIEN=$ORDER(^TMG(22720,IEN22720,23,SUBIEN)) QUIT:SUBIEN'>0  DO
        . .  ;"TO DO, EXAMINE RECORD TO SEE IF MATCHES MESSAGE TYPE.  
        . . IF $P($G(^TMG(22720,IEN22720,23,SUBIEN,0)),"^",1)=MSGTYPE SET SKIP=0
        . IF SKIP=1 QUIT
        . IF $DATA(^TMG(22720,"EAPP",HL7APP,IEN22720))>0 SET FOUND=1 QUIT
        IF FOUND'>0 SET IEN22720=0
        IF IEN22720'>0 DO
        . SET IEN22720="-1^Can't find entry in 22720 matching SENDING FACILITY=["_HL7INST_"], SENDING APPLICATION=["_HL7APP_"]"         
        QUIT IEN22720
        ;        
