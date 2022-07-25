TMGHL70C ;TMG/kst-HL7 transformation utility functions ; 12/13/15
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
 ;"Especially interacting with user to correct missing/incorect test setup.  
 ;"=======================================================================
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"PCKADD60(NAME,DONEMSG,INDENTN,MODE) -- Select or add LAB TEST info file 60
 ;"ADDATOMIC(NAME,INDENTN) -- DD ATOMIC TEST. 
 ;"PKADDDN(NAME,DONEMSG,INDENTN) -- PICK/ADD DATA NAME
 ;"LINKDN(TMGENV,TESTNAME,IEN60,INDENTN) --FIX MISSING DATANAME for IEN60
 ;"STOREDN(TMGENV,IEN60,FLD63D04) --FIX MISSING DATANAME for IEN60
 ;"ADD60((NAME,PRINTNAME,FLD63D04,TMGENV) --Add NEW entry into LABORATORY TEST (file 60)
 ;"ASKWKLD(TESTNAME,EXCLUDELIST,INDENTN) -- Ask user to pick WKLD CODE, or make a NEW one if needed.
 ;"PCKWKLD(TESTNAME,INDENTN) -- Pick Workload code (WKLD CODE)
 ;"ADDWKLDC(TESTNAME) -- ADD NEW WKLD CODE record
 ;"NEWWKLD(TESTNAME,WKLDCODE)  -- Silently add new workload code.
 ;"ADDSYN(IEN60,SYNONYM) -- Add NEW synonym to LAB TEST record
 ;"ASKSPEC(TESTNAME,INDENTN) -- Ask user for specimen for test.
 ;"ASKSPEC2(TESTNAME,INFO,INDENTN) -- Ask user for specimen (from file 64.061) for test.
 ;"ADD2AI(TMGENV,TESTNAME,NLTCODE,IEN60,IEN61) -- add a test into AUTO INSTRUMENT
 ;"SELLAB60(TESTNAME) --Ask user to pick record in file 60 to match TEST NAME
 ;"GETAI(HL7INST) -- Get AUTO INSTRUMENT to work on, using stored value if available.
 ; 
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"=======================================================================
 ;
PCKADD60(NAME,DONEMSG,INDENTN,MODE) ;
        ;"Input: NAME -- [OPTIONAL] -- Display Name of test being added.
        ;"         or NAME^Alt-Name
        ;"       DONEMSG -- OPTIONAL.  Message to show in menu to indicate done.
        ;"       INDENTN -- [OPTIONAL] Number of spaces to indent
        ;"       MODE -- [OPTIONAL] 'O' for order-type tests (OBR) or 'R' resulting-type tests (OBX) 
        ;"Result: NewlyAddedIEN60^Name if OK, or -1^Message.
        NEW TMGRESULT SET TMGRESULT=0
        NEW TMGMNU,TMGUSERINPUT
        SET NAME=$GET(NAME)
        NEW ALTNAME SET ALTNAME=$PIECE(NAME,"^",2)
        NEW TESTNAME SET TESTNAME=$PIECE(NAME,"^",1)
        SET DONEMSG=$GET(DONEMSG,"Done.")
        NEW DATANAME SET DATANAME=""      
        SET INDENTN=+$GET(INDENTN)
PA60    KILL TMGUSERINPUT,TMGMNU
        SET TMGMNU(-1,"INDENT")=INDENTN
        SET TMGMNU(0)="Pick option for selecting LABORATORY TEST to match:"
        SET TMGMNU(0,1)=" HL7 test name '"_TESTNAME_"'"
        IF (ALTNAME'="")&(ALTNAME'=TESTNAME) SET TMGMNU(0,1)=TMGMNU(0,1)_" aka '"_ALTNAME_"'"
        SET TMGMNU(1)="Search and pick existing LAB TEST name for reuse."_$CHAR(9)_"SEARCH"
        SET TMGMNU(2)="Add a NEW LAB TEST to system."_$CHAR(9)_"NEW"
        SET TMGMNU(3)=DONEMSG_$CHAR(9)_"DONE"
        SET DATANAME=""
        WRITE !
        SET TMGUSERINPUT=$$MENU^TMGUSRI2(.TMGMNU,"^")
        KILL TMGMNU ;"Prevent from cluttering variable table during debug run
        IF TMGUSERINPUT="SEARCH" DO
        . SET TMGRESULT=$$SELLAB60(NAME) ;"Ask user to pick record in file 60 to match TEST NAME 
        IF TMGUSERINPUT="NEW" DO  
        . SET TMGRESULT=$$ADDATOMIC(NAME,INDENTN+2,.MODE) ;
        IF TMGUSERINPUT="^" GOTO PA60DN
        IF TMGUSERINPUT="DONE" GOTO PA60DN
        IF TMGRESULT>0 GOTO PA60DN
        GOTO PA60
PA60DN  QUIT TMGRESULT
        ;
ADDATOMIC(NAME,INDENTN,MODE) ;
        ;"Input: NAME -- [OPTIONAL] -- Display Name of test being added.
        ;"         or NAME^Alt-Name
        ;"       INDENTN -- [OPTIONAL] Number of spaces to indent
        ;"       MODE -- [OPTIONAL] 'O' for order-type tests (OBR) or 'R' resulting-type tests (OBX)
        ;"NOTE: uses TMGENV in global scope.  
        ;"Result: NewlyAddedIEN60^Name if OK, or -1^Message.
        ;"
        NEW TMGRESULT SET TMGRESULT="-1^Unknown problem"
        NEW IEN60,IEN63D04 SET (IEN60,IEN63D04)=0        
        SET NAME=$GET(NAME)
        NEW ALTNAME SET ALTNAME=$PIECE(NAME,"^",2)
        NEW TESTNAME SET TESTNAME=$PIECE(NAME,"^",1)
        NEW HINTNAME SET HINTNAME="'"_TESTNAME_"'"
        IF (ALTNAME'="")&(ALTNAME'=HINTNAME) SET HINTNAME=HINTNAME_" aka '"_ALTNAME_"'"
        NEW FLD63D04 SET FLD63D04="" 
        NEW NEWTESTNAME SET NEWTESTNAME=$$UP^XLFSTR(NAME)
        NEW INDENTSTR SET INDENTSTR=$JUSTIFY("",INDENTN)
        WRITE !
        WRITE INDENTSTR,"===========================================",!
        WRITE INDENTSTR,"Add a New LABORATORY TEST"
        IF NAME'="" WRITE ": ",HINTNAME
        IF $GET(MODE)="R" GOTO AA0 ;"Never add dataname for OBR tests.  Can add later if same name found in OBX segment
        NEW TMGPANEL,% SET %=2
        WRITE !
        WRITE INDENTSTR,"===========================================",!
        WRITE INDENTSTR,"Is ",$SELECT(HINTNAME'="":HINTNAME,1:"the NEW test")," a PANEL?",!
        WRITE INDENTSTR,"  I.e. a group of other tests.  E.g. CBC.",!
        WRITE INDENTSTR,"  Enter NO if you are unsure.",!
        WRITE INDENTSTR,"  PANEL test: " DO YN^DICN WRITE ! SET TMGPANEL=%
        IF TMGPANEL=-1 SET TMGRESULT="-1^User aborted" GOTO AADN
        IF TMGPANEL=1 GOTO AA1
AA0     WRITE !,INDENTSTR,"Before ",$SELECT(HINTNAME'="":HINTNAME,1:"the NEW test")," can be added, a storage location",!        
        WRITE INDENTSTR,"  (called a'DATA NAME') must first be added, or an existing one selected.",!
        ;"WRITE INDENTSTR DO PRESS2GO^TMGUSRI2
        ;
        NEW TMGTEMP SET TMGTEMP=$$PKADDDN(NAME,"Continue to NEXT STEP: adding test.",.INDENTN)
        IF TMGTEMP<1 SET TMGRESULT=TMGTEMP GOTO AADN
        SET FLD63D04=$PIECE(TMGTEMP,"^",1)
        SET NEWTESTNAME=$PIECE(TMGTEMP,"^",2)
        ; 
        WRITE !,!,INDENTSTR,"OK.  Now add to LABORATORY TEST.",!        
        SET PRINTNAME=$$GETPRTNAME(TESTNAME,INDENTN)
        IF +PRINTNAME=-1 SET TMGRESULT=PRINTNAME GOTO AADN
AA3     SET IEN60=$$ADD60(NEWTESTNAME,PRINTNAME,FLD63D04,.TMGENV) ;"not interactive
        IF +IEN60'>0 DO  GOTO AADN
        . SET TMGRESULT=IEN60
        SET TMGRESULT=IEN60
        WRITE !,!,INDENTSTR,"OK.  Newly added test should be available to use.",!
AADN    QUIT TMGRESULT
        ;
GETPRTNAME(TESTNAME,INDENTN) ;"Get PRINT NAME for TEST NAME
        ;"Input: TESTNAME -- Name of test to get print name for
        ;"       INDENTN -- [OPTIONAL] Number of spaces to indent
        ;"Result: Returns PRINT NAME, or -1^error if problem or abort.
        ;"NOTE: This value will be stored in field 51 in file 60.  
        ;"NOTE: The index for these values is ^LAB(60,"D",
        ;"NOTE: PRINT NAMES should be unique.
        ;"NOTE: This code is used as the default test heading in cumulative lab test reports
        NEW TMGRESULT SET TMGRESULT=""
        SET INDENTN=+$GET(INDENTN)
        NEW INDENTSTR SET INDENTSTR="" 
        IF INDENTN>0 SET INDENTSTR=$JUSTIFY(" ",INDENTN)
        NEW TMGMNU,TMGUSERINPUT,MNUNUM        
        SET TESTNAME=$GET(TESTNAME,"?")
        NEW PRINTNAME SET PRINTNAME=""
        NEW AUTONAME SET AUTONAME=""
        IF $LENGTH(TESTNAME)'>7 SET TMGRESULT=TESTNAME GOTO GPRNDN
        ;"Split TESTNAME into an array of words. 
        NEW ARR,ARRI,STR,IDX,CH
        SET ARRI=0,STR=""
        FOR IDX=1:1:$LENGTH(TESTNAME) DO
        . SET CH=$EXTRACT(TESTNAME,IDX)
        . IF " /-.;,"[CH DO
        . . IF STR'="" SET ARRI=ARRI+1,ARR(ARRI)=STR,STR=""   ;"//kt changed 6/20/22
        . ELSE  DO
        . . SET STR=STR_CH
        IF STR'="" SET ARR(ARRI)=STR,STR=""
        ;"Now, assemble name from first few letters of each word.         
        IF ARRI>7 SET ARRI=7  ;"truncate if > 7 words.  
        NEW LPI SET LPI=7\ARRI  ;"LPI means LETTERS PER INDEX (of array)
        NEW EXTRA SET EXTRA=7#ARRI  ;"remainder
        SET PRINTNAME=""
        FOR IDX=1:1:ARRI DO
        . NEW NUM SET NUM=LPI
        . IF EXTRA>0 SET NUM=NUM+EXTRA,EXTRA=0
        . SET AUTONAME=AUTONAME_$EXTRACT(ARR(IDX),1,NUM)
        ;"Now ensure name not chosen before
        NEW ABORT SET ABORT=0
        NEW NUM SET NUM=1
        FOR  QUIT:($DATA(^LAB(60,"D",AUTONAME))=0)!ABORT  DO  
        . NEW ANAME SET ANAME=$EXTRACT(AUTONAME,1,7-$LENGTH(NUM))_NUM
        . IF $DATA(^LAB(60,"D",ANAME))=0 SET AUTONAME=ANAME QUIT
        . SET NUM=NUM+1
        . IF NUM=9999999 SET ABORT=1        
GPN0    ;
        IF +TMGRESULT=-1 GOTO GPNDN
        KILL TMGUSERINPUT,TMGMNU
        SET TMGMNU(-1,"INDENT")=INDENTN
        SET TMGMNU(0)="Pick option for test PRINT NAME"
        SET TMGMNU(0,1)="TEST NAME: "_TESTNAME
        IF ABORT SET TMGMNU(0,2)="NOTE: unable to auto-generate name"
        SET MNUNUM=1
        IF ABORT=0 SET TMGMNU(MNUNUM)="USE '"_AUTONAME_"'"_$CHAR(9)_"AUTO",MNUNUM=MNUNUM+1
        IF (PRINTNAME'=""),(PRINTNAME'=AUTONAME) SET TMGMNU(MNUNUM)="USE '"_PRINTNAME_"'"_$CHAR(9)_"PRINTNAME",MNUNUM=MNUNUM+1
        SET TMGMNU(MNUNUM)="Enter CUSTOM name"_$CHAR(9)_"CUSTOM",MNUNUM=MNUNUM+1
        WRITE !
        SET TMGUSERINPUT=$$MENU^TMGUSRI2(.TMGMNU,"1")
        KILL TMGMNU ;"Prevent from cluttering variable table during debug run
        IF TMGUSERINPUT="AUTO" DO  GOTO GPNDN
        . SET TMGRESULT=AUTONAME
        IF TMGUSERINPUT="PRINTNAME" DO  GOTO GPNDN
        . SET TMGRESULT=PRINTNAME
        IF TMGUSERINPUT="^" DO  GOTO GPNDN
        . SET TMGRESULT="-1^User aborted"  ;"default to failure
        IF TMGUSERINPUT="CUSTOM" DO  GOTO GPNDN:(PRINTNAME["^"),GPN0
        . WRITE INDENTSTR,"Cursor keys enabled.",!
        . WRITE INDENTSTR SET PRINTNAME=$$EDITBOX^TMGUSRI6("",7,"_",0,0,7)
        GOTO GPN0
GPNDN   ;        
        QUIT TMGRESULT
        ;"OLD METHOD BELOW.  Delete later...
AA1     WRITE !,!,INDENTSTR,"OK.  Now add to LABORATORY TEST.",!
AA2     WRITE INDENTSTR,"Enter a name to use as display code for test for:",!
        WRITE INDENTSTR," ",HINTNAME,!  
        WRITE INDENTSTR,"E.g. 'CBC' is commonly used for a COMPLETE BLOOD COUNT. ",!
        WRITE INDENTSTR,"Must be 1-7 characters.  This code is used as the default test",!
        WRITE INDENTSTR,"heading in cumulative lab test reports.",!,!
        SET PRINTNAME=$$READLEN^TMGUSRI4("Enter Code (^ to abort): ",7,.INDENTN)
        IF PRINTNAME="" WRITE INDENTSTR,"?? Nothing entered. Try again.",! GOTO AA2
        IF $LENGTH(PRINTNAME)>7 WRITE INDENTSTR,"Too long.  Try again.",! GOTO AA2
        IF PRINTNAME="^" SET TMGRESULT="-1^User aborted." GOTO AADN
        IF PRINTNAME["^" WRITE INDENTSTR,"Name can't contain '^'",! GOTO AA2
        WRITE INDENTSTR,"Use '",PRINTNAME,"': " SET %=1 DO YN^DICN WRITE !
        IF %=-1 SET TMGRESULT="-1^User aborted" GOTO AADN
        IF %=2 GOTO AA2        
GPRNDN  QUIT PRINTNAME        
        ;
PKADDDN(NAME,DONEMSG,INDENTN) ;"PICK/ADD DATA NAME
        ;"Purpose: 
        ;"Input: NAME -- OPTIONAL. Display Name of test being added.
        ;"         or NAME^Alt-Name
        ;"       DONEMSG -- OPTIONAL.  Message to show in menu to indicate done.
        ;"Result: FieldNumber^Name, or 0 if none added, or -1^Message if problem.
        NEW TMGRESULT SET TMGRESULT=0
        NEW FLD63D04
        NEW TMGMNU,TMGUSERINPUT
        SET NAME=$GET(NAME)
        NEW ALTNAME SET ALTNAME=$PIECE(NAME,"^",2)
        NEW TESTNAME SET TESTNAME=$PIECE(NAME,"^",1)
        NEW HINTNAME SET HINTNAME=TESTNAME
        IF (ALTNAME'="")&(ALTNAME'=HINTNAME) SET HINTNAME=HINTNAME_" aka '"_ALTNAME_"'"
        SET DONEMSG=$GET(DONEMSG,"Done.")
        NEW DATANAME SET DATANAME=""      
        SET INDENTN=+$GET(INDENTN)
PAA0    SET TMGMNU(-1,"INDENT")=INDENTN
        SET TMGMNU(0)="Pick option for selecting DATA NAME storage location"
        SET TMGMNU(0,1)=" for test: "_HINTNAME
        IF DATANAME'="" DO
        . SET TMGMNU(0,2)="Current selection: """_DATANAME_""""
        SET TMGMNU(0)="Pick option for selecting DATA NAME storage location."
        SET TMGMNU(1)="Search and pick existing DATA NAME for reuse."_$CHAR(9)_"SEARCH"
        SET TMGMNU(2)="Add a NEW DATA NAME to system."_$CHAR(9)_"NEW"
        SET TMGMNU(3)="AUTO CREATE '"_TESTNAME_"' as NEW DATA NAME"_$CHAR(9)_"AUTO"
        IF (ALTNAME'=TESTNAME)&(ALTNAME'="") SET TMGMNU(4)="AUTO CREATE '"_ALTNAME_"' as NEW DATA NAME"_$CHAR(9)_"AUTO2"
        SET TMGMNU(5)=DONEMSG_$CHAR(9)_"DONE"
        SET DATANAME=""
        WRITE !
        SET TMGUSERINPUT=$$MENU^TMGUSRI2(.TMGMNU,"^")
        KILL TMGMNU ;"Prevent from cluttering variable table during debug run
        IF TMGUSERINPUT="DONE" GOTO ADNDN
        IF TMGUSERINPUT="SEARCH" DO  
        . NEW OPTION SET OPTION("NCS")=1
        . SET OPTION("HEADER",1)="{{BOLD}}Pick DATA NAME storage location for{{NORM}} {{RED}}"_$SELECT(NAME'="":NAME,1:"the NEW test")_"{{NORM}}"
        . SET SELOPT("HEADER",2)="{{BOLD}}NOTE: If no match is found, press ^ for chance to add NEW entry{{NORM}}"
        . SET OPTION("COLORS","NORM")="7^4"
        . SET OPTION("COLORS","BOLD")="14^4"
        . SET OPTION("COLORS","RED")="14^1"
        . SET OPTION("SCRN WIDTH")=70
        . NEW IEN SET IEN=+$$FLDSEL^TMGUSRI4(63.04,.OPTION) WRITE #
        . IF IEN'>0 QUIT
        . SET DATANAME=$PIECE($GET(^DD(63.04,IEN,0)),"^",1)
        . WRITE "USE '",DATANAME,"' as storage location (DATA NAME) ",!,"for ",$SELECT(NAME'="":NAME,1:"the NEW test")
        . NEW % SET %=2 DO YN^DICN WRITE !
        . IF %=-1 SET TMGUSRINPUT="^" QUIT
        . IF %'=1 SET DATANAME=""
        . IF %=1 SET TMGRESULT=IEN_"^"_DATANAME
        IF TMGUSERINPUT="NEW" DO  
        . SET FLD63D04=$$ADD63D04() ;
        . IF +FLD63D04>0 SET DATANAME=$PIECE(FLD63D04,"^",2),TMGRESULT=FLD63D04 QUIT
        . IF +FLD63D04'>0 SET TMGRESULT=FLD63D04 QUIT
        . WRITE "Nothing added.",!
        . WRITE $PIECE(FLD63D04,"^",2,99),!
        . DO PRESS2GO^TMGUSRI2        
        IF TMGUSERINPUT["AUTO" DO
        . NEW ADDNAME SET ADDNAME=TESTNAME
        . IF TMGUSERINPUT="AUTO2" SET ADDNAME=ALTNAME
        . SET FLD63D04=$$AUTOADDN(ADDNAME) ;"AUTO ADD DATANAME, into 63.04
        . IF +FLD63D04>0 SET DATANAME=$PIECE(FLD63D04,"^",2),TMGRESULT=FLD63D04 QUIT
        . IF +FLD63D04'>0 SET TMGRESULT=FLD63D04 QUIT
        . WRITE "Nothing added.",!
        . WRITE $PIECE(FLD63D04,"^",2,99),!
        . DO PRESS2GO^TMGUSRI2        
        IF TMGUSERINPUT="^" SET TMGRESULT="-1^User aborted" GOTO ADNDN
        IF TMGRESULT>0 GOTO ADNDN
        IF +TMGRESULT'>0 DO
        . WRITE $PIECE(TMGRESULT,"^",2,99),!
        . DO PRESS2GO^TMGUSRI2
        GOTO PAA0
ADNDN   QUIT TMGRESULT
        ;
LINKDN(TMGENV,TESTNAME,IEN60,INDENTN) ;"FIX MISSING DATANAME for IEN60
        ;"Result: 1 if OK, or -1^Message if error. 
        NEW TMGRESULT SET TMGRESULT=1
        NEW FLD63D04 SET FLD63D04=$$PKADDDN^TMGHL70C(TESTNAME,"Done.",.INDENTN)
        IF +FLD63D04<0 SET TMGRESULT=FLD63D04 GOTO LKDNDN
        SET TMGRESULT=$$STOREDN(.TMGENV,.IEN60,FLD63D04) ;"FIX MISSING DATANAME for IEN60
LKDNDN  QUIT TMGRESULT
        ;
STOREDN(TMGENV,IEN60,FLD63D04) ;"FIX MISSING DATANAME for IEN60
        ;"Result: 1 if OK, or -1^Message if error. 
        NEW TMGRESULT SET TMGRESULT=1
        SET IEN60=+$GET(IEN60)
        IF IEN60'>0 DO  GOTO STRDNDN
        . SET TMGRESULT="-1^In STOREDN.TMGHL70D.  No IEN60 provided"
        SET FLD63D04=+$GET(FLD63D04)
        IF +FLD63D04<0 DO  GOTO STRDNDN
        . SET TMGRESULT="-1^In STOREDN.TMGHL70D.  No FLD63D04 provided"
        NEW TMGFDA,TMGMSG
        SET TMGFDA(60,IEN60_",",400)=+FLD63D04
        DO FILE^DIE("","TMGFDA","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO STRDNDN  
        . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        DO REFRSHDN(.TMGENV,IEN60) 
STRDNDN QUIT TMGRESULT
        ;
REFRSHDN(TMGENV,IEN60) ;
        ;"Purpose: Refresh trigger XRef on .01 field, to dataname is refreshed. 
        ;"Note: the dataname is also stored as free text in field 11 in 62.41 in 62.4 (AUTO INSTRUMENT)
        ;"      It is SET by a trigger field on the TEST field in that same subfile.  
        SET IEN60=+$GET(IEN60)
        IF IEN60'>0 GOTO RFDNDN        
        NEW IEN62D4 SET IEN62D4=TMGENV("IEN 62.4")
        NEW DIK 
        SET DIK="^LAB(62.4,"_IEN62D4_",3,"
        SET DIK(1)=".01"  ;"all cross-references for field .01
        SET DA(1)=IEN62D4
        NEW IEN62D41 SET IEN62D41=0
        FOR  SET IEN62D41=$ORDER(^LAB(62.4,IEN62D4,3,"TMGTEST",IEN60,IEN62D41)) QUIT:(+IEN62D41'>0)  DO
        . SET DA=IEN62D41
        . DO EN1^DIK  ;"execute the SET logic of the XREF's 
RFDNDN  QUIT
        ;        
ADD60(NAME,PRINTNAME,FLD63D04,TMGENV) ;
        ;"Purpose: add new, bare-bones, entry into LABORATORY TEST (file 60)
        ;"Input: NAME -- text, to be name of NEW test
        ;"       PRINTNAME -- 1-7 characters.  Used for lab name in cumulative reports
        ;"       FLD63D04 -- the 'DATANAME' / field used in 63.04 (in 63) for storage. OPTIONAL
        ;"       TMGENV -- PASS BY REFERENCE.  The ENV for HL7 message
        ;"Result: returns NewlyAddedIEN60^Name, or 0 if none added, or -1^Message if problem.
        NEW TMGRESULT SET TMGRESULT=0
        SET FLD63D04=$GET(FLD63D04)
        NEW TMGFDA,TMGIEN,TMGMSG
        SET TMGFDA(60,"+1,",.01)=NAME        ;"NAME
        SET TMGFDA(60,"+1,",3)="BOTH"        ;"TYPE
        SET TMGFDA(60,"+1,",4)="CH"          ;"SUBSCRIPT
        SET TMGFDA(60,"+1,",17)="STAT"       ;"HIGHEST URGENCY ALLOWED
        SET TMGFDA(60,"+1,",51)=PRINTNAME    ;"PRINT NAME
        IF FLD63D04>0 DO
        . SET TMGFDA(60,"+1,",400)="`"_FLD63D04 ;"DATANAME
        DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO A60DN  
        . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        NEW IEN60 SET IEN60=+$GET(TMGIEN(1))
        IF IEN60'>0 DO  GOTO A60DN
        . SET TMGRESULT="-1^Unable to locate newly added record into file #60"
        SET TMGRESULT=IEN60_"^"_NAME
        IF FLD63D04>0,$PIECE($GET(^LAB(60,+IEN60,.2)),"^",1)'>0 DO  ;"For some reason filing into field 400 doesn't take...
        . NEW TEMP SET TEMP=$$STOREDN(.TMGENV,IEN60,FLD63D04) ;"FIX MISSING DATANAME for IEN60
        . IF +TEMP<0 SET TMGRESULT=TEMP
A60DN   QUIT TMGRESULT
        ;
AUTOADDN(NAME) ;"AUTO ADD DATANAME, into 63.04
        ;"Purpose: to automatically add NAME as NEW field into file 63.04, as a NEW DATA NAME
        ;"Result: returns NewlyAddedFieldNumber^Name, or 0 if none added, or -1^Message if problem.
        ;"NOTE: This does direct data dictionary writes.  This is likely not SACC compliant.
        SET NAME=$$UP^XLFSTR(NAME)
        NEW TMGRESULT SET TMGRESULT=0
        NEW FLD SET FLD=+$ORDER(^DD(63.04,"B",NAME,0)) IF FLD>0 DO  GOTO AADNDN
        . SET TMGRESULT="-1^"_NAME_" is already field #"_FLD
        NEW NEWFLD SET NEWFLD=$ORDER(^DD(63.04,"@"),-1)+1
        NEW ZN SET ZN="<NAME>^F^^<STORE>^K:$L(X)>50!($L(X)<1) X"
        SET $PIECE(ZN,"^",1)=NAME
        SET $PIECE(ZN,"^",4)=NEWFLD_";1"
        SET ^DD(63.04,NEWFLD,0)=ZN
        SET ^DD(63.04,NEWFLD,3)="ANSWER MUST BE 1-50 CHARACTERS IN LENGTH"
        SET ^DD(63.04,"B",NAME,NEWFLD)=""
        SET ^DD(63.04,"GL",NEWFLD,1,NEWFLD)=""
        SET $PIECE(^DD(63.04,0),"^",3)=NEWFLD
        SET $PIECE(^DD(63.04,0),"^",4)=$PIECE(^DD(63.04,0),"^",4)+1
        SET TMGRESULT=NEWFLD_"^"_NAME
AADNDN  QUIT TMGRESULT        
        ;
ADD63D04() ;
        ;"Purpose: add a NEW DATA NAME--which is really a NEW field in 63.04 IN 63 (LABORATORY TEST)
        ;"Input: none
        ;"Result: returns NewlyAddedFieldNumber^Name, or 0 if none added, or -1^Message if problem.
        NEW TMGRESULT SET TMGRESULT=0
        NEW DDSTORE,NEWDD
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(^DD(63.04,IDX)) QUIT:(+IDX'>0)  MERGE DDSTORE(IDX)=^DD(63.04,IDX,0)
        DO ^LRWU5 ;"<--- here is where the entry is actually added.
        SET IDX=0
        FOR  SET IDX=$ORDER(^DD(63.04,IDX)) QUIT:(+IDX'>0)  MERGE:($DATA(DDSTORE(IDX))=0) NEWDD(IDX)=^DD(63.04,IDX)
        KILL DDSTORE NEW CT SET (CT,IDX)=0
        SET IDX=0
        FOR  SET IDX=$ORDER(NEWDD(IDX)) QUIT:(+IDX'>0)  SET CT=CT+1
        IF CT>1 DO  GOTO ADNMDN
        . SET TMGRESULT="-1^More than 1 Data Name added, Unexpected."
        IF CT=0 GOTO ADNMDN
        SET IDX=$ORDER(NEWDD(0))
        NEW NAME SET NAME=$PIECE($GET(NEWDD(IDX,0)),"^",1)
        SET TMGRESULT=IDX_"^"_NAME
ADNMDN  QUIT TMGRESULT
        ;
ASKWKLD(TESTNAME,EXCLUDELIST,INDENTN) ;
        ;"Purpose: Ask user to pick WKLD CODE, or make a NEW one if needed.  (FILE 64 = WKLD CODE)
        ;"Input: TESTNAME -- Name of test to add code for.
        ;"       EXCLUDELIST -- PASS BY REFERENCE.  Format:
        ;"          EXCLUDELIST=<root to 62.41 file>         
        ;"          EXCLUDELIST(NLT,IEN62.41)="" 
        ;"       INDENTN -- OPTIONAL.  Number of spaces to indent. 
        ;"Results: IEN64^Name^WKLD_CODE, or 0 if none.
        NEW TMGRESULT SET TMGRESULT=0
        NEW TMGUSERINPUT,TMGMNU,%,Y,IEN62D41
        SET INDENTN=+$GET(INDENTN)
GWLC1   SET TMGMNU(-1,"INDENT")=INDENTN
        SET TMGMNU(0)="Pick option for setting up WKLD CODE entry for '"_TESTNAME_"'"
        SET TMGMNU(1)="Pick existing WKLD CODE"_$CHAR(9)_"SEARCH"
        SET TMGMNU(2)="Create a NEW WKLD CODE"_$CHAR(9)_"NEW"
        SET TMGUSERINPUT=$$MENU^TMGUSRI2(.TMGMNU,"^")
        KILL TMGMNU ;"Prevent from cluttering variable table during debug run
        IF TMGUSERINPUT="^" GOTO GWLCDN
        IF TMGUSERINPUT="SEARCH" DO  GOTO:(+TMGRESULT>0) GWLCHK
        . SET TMGRESULT=$$PCKWKLD^TMGHL70C(TESTNAME,INDENTN)        
        IF TMGUSERINPUT="NEW" DO  GOTO:(+TMGRESULT>0) GWLCHK
        . SET TMGRESULT=$$ADDWKLDC(TESTNAME,INDENTN+2)
        GOTO GWLC1
GWLCHK  SET IEN62D41=$$EXCLWKLD(TESTNAME,.EXCLUDELIST)
        IF IEN62D41>0 DO  GOTO GWLC1
        . WRITE "Sorry, that code has already been used for ",$PIECE(IEN62D41,"^",2) 
        . WRITE "(IEN60 #",$PIECE(IEN62D41,"^",1)," Please pick another.",!
        . DO PRESS2GO^TMGUSRI2
GWLCDN  QUIT TMGRESULT
        ;
EXCLWKLD(TESTNAME,EXCLUDELIST) ;
        ;"Purpose: determine if specified test is present in EXCLUDELIST
        ;"Returns: IEN62.41^TestName(60) if to be excluded, 0 otherwise.
        NEW NLT SET NLT=$PIECE(TESTNAME,"^",3)
        NEW IEN62D41 SET IEN62D41=+$ORDER(EXCLUDELIST(NLT,0))
        IF IEN62D41'>0 GOTO EXWKDN
        NEW ROOT SET ROOT=$GET(EXCLUDELIST)
        NEW ZNODE IF ROOT'="" SET ZNODE=$GET(@ROOT@(IEN62D41,0))
        NEW IEN60 SET IEN60=+$PIECE(ZNODE,"^",1)
        NEW NAME SET NAME=$$GET1^DIQ(60,IEN60,.01)
        SET IEN62D41=IEN62D41_"^"_NAME
EXWKDN  QUIT IEN62D41
        ;
PCKWKLD(TESTNAME,INDENTN) ;
        ;"Purpose: Allow user to pick a WKLD Code
        ;"Result: IEN64^NAME^WkldCode, or 0 if none
        SET INDENTN=+$GET(INDENTN)
        NEW INDENTSTR SET INDENTSTR="" 
        IF INDENTN>0 SET INDENTSTR=$JUSTIFY(" ",INDENTN)
        NEW SELOPT,Y
        SET SELOPT("NCS")=1 ;"not case sensitive
        SET SELOPT("HEADER",1)="Please selected a record from the WKLD CODE file"
        SET SELOPT("HEADER",2)="that matches: {{RED}}"_TESTNAME_"{{NORM}}"
        SET SELOPT("HEADER",3)="{{BOLD}}NOTE: If no match is found, press ^ for chance to add NEW entry{{NORM}}"
        SET SELOPT("COLORS","NORM")="7^4"
        SET SELOPT("COLORS","BOLD")="14^4"
        SET SELOPT("COLORS","RED")="14^1"
        SET SELOPT("COLORS","FOOTER")="14^6"
        SET SELOPT("SCRN WIDTH")=70
        SET Y=$$RECSEL^TMGUSRI4(64,,.SELOPT) WRITE #
        IF +Y'>0 GOTO PWLCDN  
        NEW NAME SET NAME=$PIECE(Y,"^",2)
        IF NAME="" SET NAME=$$GET1^DIQ(64,+Y,.01)
        NEW WKLDCODE SET WKLDCODE=$$GET1^DIQ(64,+Y,1)
        WRITE INDENTSTR,"The code for ",NAME," is ",WKLDCODE,!
        SET Y=+Y_"^"_NAME_"^"_WKLDCODE
PWLCDN  QUIT Y
        ;
ADDWKLDC(TESTNAME,INDENTN) ;"ADD NEW WKLD CODE record  (FILE 64 = WKLD CODE)
        ;"Input: TESTNAME -- Name of test to add code for.
        ;"Results: IEN64^Name^WKLD_CODE, or 0 if none.
        NEW TMGRESULT SET TMGRESULT=0
        NEW TMGUSERINPUT,TMGMNU,%,Y
        NEW WKLDCODE SET WKLDCODE=0        
        SET INDENTN=+$GET(INDENTN)
        NEW INDENTSTR SET INDENTSTR="" 
        IF INDENTN>0 SET INDENTSTR=$JUSTIFY(" ",INDENTN)
AWLC1   SET TMGMNU(-1,"INDENT")=INDENTN        
        SET TMGMNU(0)="Pick option for creating number for WKLD CODE entry for '"_TESTNAME_"'"
        SET TMGMNU(1)="Find similar entry to pattern number after"_$CHAR(9)_"SEARCH"
        SET TMGMNU(2)="Enter NEW #####.#### patterned number"_$CHAR(9)_"NEW"
        SET TMGUSERINPUT=$$MENU^TMGUSRI2(.TMGMNU,"^")
        KILL TMGMNU ;"Prevent from cluttering variable table during debug run
        IF TMGUSERINPUT="^" GOTO AWLCDN
        IF TMGUSERINPUT="SEARCH" DO
        . SET Y=$$PCKWKLD^TMGHL70C(TESTNAME,INDENTN) 
        . IF Y'>0 QUIT
        . NEW DONE SET DONE=0
        . SET WKLDCODE=$PIECE(Y,"^",3)
        . NEW INTPART SET INTPART=$PIECE(WKLDCODE,".",1)
        . NEW DECPART SET DECPART=$PIECE(WKLDCODE,".",2)
        . FOR  DO  QUIT:DONE
        . . SET DECPART=DECPART+1
        . . SET DECPART=$$RJ^XLFSTR(DECPART,"4","0")
        . . SET WKLDCODE=INTPART_"."_DECPART
        . . SET DONE=($DATA(^LAM("C",WKLDCODE_" "))=0)
        . WRITE INDENTSTR,"Use ",WKLDCODE," for NEW work load record for ",!
        . WRITE INDENTSTR,TESTNAME
        . SET %=1 DO YN^DICN WRITE !
        . IF %'=1 SET WKLDCODE=0
        IF TMGUSERINPUT="NEW" DO
        . NEW INPUT,DONE SET DONE=0
        . FOR  DO  QUIT:DONE
        . . WRITE INDENTSTR,"ENTER NEW CODE (#####.#### format) (^ to abort): "
        . . READ INPUT:$GET(DTIME,3600)
        . . IF INPUT="^" SET DONE=1
        . . IF INPUT?5N1"."4N SET WKLDCODE=INPUT,DONE=1 QUIT
        IF WKLDCODE>0 DO  GOTO:(TMGRESULT>0) AWLCDN
        . NEW TEMP SET TEMP=$$NEWWKLD(TESTNAME,WKLDCODE)
        . IF TEMP<0 DO  QUIT
        . . WRITE $PIECE(TEMP,"^",2,99)
        . . DO PRESS2GO^TMGUSRI2
        . ELSE  SET TMGRESULT=TEMP
        GOTO AWLC1         
AWLCDN  QUIT TMGRESULT
        ;
NEWWKLD(TESTNAME,WKLDCODE)  ;"Silently add new workload code.
        ;"Result: -1^Message, or IEN^Name^Code
        NEW TMGFDA,TMGMSG,TMGIEN
        NEW TMGRESULT
        SET TMGFDA(64,"+1,",.01)=TESTNAME
        SET TMGFDA(64,"+1,",1)=WKLDCODE
        SET TMGFDA(64,"+1,",6)="NO"
        SET TMGFDA(64,"+1,",12)="NOT SPECIFIED"
        SET TMGFDA(64,"+1,",13)="Chemistry"
        DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO NWKLDN
        . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        ELSE  DO
        . NEW Y SET Y=+$GET(TMGIEN(1))
        . NEW NAME SET NAME=$$GET1^DIQ(64,Y,.01)
        . SET TMGRESULT=Y_"^"_NAME_"^"_WKLDCODE
NWKLDN  QUIT TMGRESULT
        ;
ADDSYN(IEN60,SYNONYM) ;
        ;"Add NEW synonym to LAB TEST record
        ;"Results: 1 if OK, -1^Message if problem.
        KILL TMGFDA,TMGMSG,TMGIEN
        SET TMGFDA(60.1,"+1,"_+IEN60_",",.01)=SYNONYM
        DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  
        . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        ELSE  SET TMGRESULT=1
        QUIT TMGRESULT
        ;
ASKSPEC(TESTNAME,INDENTN) ;
        ;"Purpose: Ask user for specimen for test.
        ;"Input: None
        ;"Results: IEN61^IEN62 if OK, -1^Message if problem.
        NEW TMGRESULT SET TMGRESULT="-1^No test specimen selected"
        NEW IEN61 SET IEN61=0
        NEW IEN62 SET IEN62=0
        NEW TMGUSERINPUT,TMGMNU 
        NEW X,Y
        NEW DIC SET DIC=61
        KILL DUOUT,DTOUT
        SET INDENTN=+$GET(INDENTN)
AM1     IF $DATA(DUOUT) GOTO ASDN
        KILL DIC(0),DIC("A")
        SET TMGMNU(-1,"INDENT")=INDENTN
        SET TMGMNU(0)="Pick SPECIMEN for test '"_TESTNAME_"'"
        SET TMGMNU(1)="SERUM (Use this for most chemestry tests)"_$CHAR(9)_"SERUM"
        SET TMGMNU(2)="BLOOD"_$CHAR(9)_"BLOOD"
        SET TMGMNU(3)="URINE"_$CHAR(9)_"URINE"
        SET TMGMNU(4)="CEREBROSPINAL FLUID"_$CHAR(9)_"CEREBROSPINAL FLUID"
        SET TMGMNU(5)="SKIN"_$CHAR(9)_"SKIN"
        SET TMGMNU(6)="OTHER"_$CHAR(9)_"OTHER"
        SET TMGMNU(7)="Search for other specimen name"_$CHAR(9)_"*"
        ;
        SET TMGUSERINPUT=$$MENU^TMGUSRI2(.TMGMNU,"^")
        KILL TMGMNU ;"Prevent from cluttering variable table during debug run
        ;
        IF TMGUSERINPUT="*" DO  GOTO AM2
        . SET DIC(0)="MAEQ"
        . SET DIC("A")="Select SPECIMEN for test: "
        . DO ^DIC WRITE !
        . IF Y'>0 QUIT
        . SET IEN61=+Y
        IF TMGUSERINPUT="^" GOTO ASDN
        IF TMGUSERINPUT=0 SET TMGUSERINPUT=""
        SET DIC(0)="M"
        SET X=TMGUSERINPUT
        DO ^DIC WRITE !
        IF Y'>0 DO  GOTO AM1
        . WRITE "This VistA installation does not seem to have a unique",!
        . WRITE "entry for "_TMGUSERINPUT_".",!
        . WRITE "Try 'Search for other specimen name' and search manually.",!
        . DO PRESS2GO^TMGUSRI2
        SET IEN61=+Y
AM2     IF IEN61'>0 GOTO AM1
        ;"Try to find matching value for file 62 COLLECTION SAMPLE.
        SET IEN62=$$GET62F61(IEN61)  ;"GET IEN62 FROM IEN61
        ;"NEW IEN SET IEN=0
        ;"FOR  SET IEN=$ORDER(^LAB(62,IEN)) QUIT:(+IEN'>0)!(IEN62>0)  DO
        ;". IF +$PIECE($GET(^LAB(62,IEN,0)),"^",2)'=IEN61 QUIT
        ;". SET IEN62=IEN
        IF IEN62>0 GOTO AM5
        KILL DIC SET DIC=62,DIC(0)="MAEQ"
AM3     WRITE "A 'COLLECTION SAMPLE' is also required for "_TESTNAME,".",!
        WRITE "Unable to find match automatically.  Please search manually.",!
        KILL DUOUT,DTOUT
        DO ^DIC WRITE !
        IF $DATA(DUOUT) DO  GOTO ASDN
        . SET TMGRESULT="-1^User aborted from selecting COLLECTION SAMPLE"
        IF Y'>0 GOTO AM3
        SET IEN62=+Y
AM5     SET TMGRESULT=+IEN61_"^"_+IEN62
ASDN    QUIT TMGRESULT
        ;
GET62F61(IEN61)  ;"GET IEN62 FROM IEN61
        NEW IEN62 SET IEN62=0
        NEW IEN SET IEN=0
        FOR  SET IEN=$ORDER(^LAB(62,IEN)) QUIT:(+IEN'>0)!(IEN62>0)  DO
        . IF +$PIECE($GET(^LAB(62,IEN,0)),"^",2)'=IEN61 QUIT
        . SET IEN62=IEN
        QUIT IEN62
        ;
ASKSPEC2(TESTNAME,INFO,INDENTN) ;
        ;"Purpose: Ask user for specimen (from file 64.061) for test.
        ;"Input: TESTNAME -- name to match
        ;"       INFO -- Array of information collected so far
        ;"       INDENTN -- amount to indent from left margin
        ;"Results: IEN64.061 if OK, -1^Message if problem.
        NEW TMGRESULT SET TMGRESULT="-1^No test specimen selected"
        NEW IEN64D061 SET IEN64D061=0
        NEW TMGUSERINPUT,TMGMNU
        NEW X,Y
        NEW DIC SET DIC=64.061
        KILL DUOUT,DTOUT
        NEW IEN61 SET IEN61=+$GET(INFO("IEN61"))
        NEW OTHERSPECNAME SET OTHERSPECNAME=""
        IF IEN61>0 DO  GOTO:(IEN64D061>0) AM22
        . SET DIC(0)="MX"
        . SET OTHERSPECNAME=$PIECE($GET(^LAB(61,IEN61,0)),"^",1)
        . SET X=OTHERSPECNAME
        . DO ^DIC
        . IF Y'>0 QUIT
        . SET IEN64D061=+Y
        SET INDENTN=+$GET(INDENTN)
AM21    IF $DATA(DUOUT) GOTO AS2DN
        SET IEN64D061=0
        KILL DIC(0),DIC("A")
        SET TMGMNU(-1,"INDENT")=INDENTN
        SET TMGMNU(0)="Pick SPECIMEN for test '"_TESTNAME_"'"
        SET TMGMNU(1)="SERUM (Use this for most chemestry tests)"_$CHAR(9)_"SERUM"
        SET TMGMNU(2)="BLOOD"_$CHAR(9)_"BLOOD VENOUS"
        SET TMGMNU(3)="URINE"_$CHAR(9)_"URINE"
        SET TMGMNU(4)="CEREBROSPINAL FLUID"_$CHAR(9)_"CSF"
        SET TMGMNU(5)="SKIN"_$CHAR(9)_"SKIN"
        SET TMGMNU(6)="OTHER"_$CHAR(9)_"OTHER"
        SET TMGMNU(7)="Search for other specimen name"_$CHAR(9)_"SEARCH"
        ;
        SET TMGUSERINPUT=$$MENU^TMGUSRI2(.TMGMNU,"^")
        KILL TMGMNU ;"Prevent from cluttering variable table during debug run
        ;
        IF TMGUSERINPUT="SEARCH" DO  GOTO AM22
        . SET DIC(0)="MAEQ"
        . SET DIC("A")="Select SPECIMEN for test: "
        . DO ^DIC
        . IF Y'>0 QUIT
        . SET IEN64D061=+Y
        IF TMGUSERINPUT="^" GOTO AS2DN
        IF TMGUSERINPUT=0 SET TMGUSERINPUT=""
        ELSE  DO
        . SET DIC(0)="MX"
        . SET X=TMGUSERINPUT
        . DO ^DIC WRITE !
        . IF Y'>0 DO  QUIT
        . . WRITE "This VistA installation does not seem to have a unique",!
        . . WRITE "entry for "_TMGUSERINPUT_".",!
        . . WRITE "Try 'Search for other specimen name' and search manually.",!
        . . DO PRESS2GO^TMGUSRI2
        . SET IEN64D061=Y
AM22    IF IEN64D061'>0 GOTO AM21
        SET TMGRESULT=IEN64D061        
AS2DN   QUIT TMGRESULT
        ;
ADD2AI(TMGENV,TESTNAME,NLTCODE,IEN60,IEN61) ;
        ;"Purpose: To add a test into AUTO INSTRUMENT
        ;"Note: This function does not test for pre-existencs --> could lead to double entries
        ;"Note: this functon does not DO input validation.
        ;"Input: TMGENV -- Environment array.  See definition elsewhere. 
        ;"       TESTNAME -- Display name of test.
        ;"       NLT -- e.g. 81172.0000
        ;"       IEN60-- IEN in LABORATORY TEST file
        ;"       IEN61 -- IEN in TOPOGRAPHY FIELD file (used for SPECIMEN)
        ;"Result: 1^NewIEN62D41  if OK, or -1^ErrorMessage
        NEW TMGRESULT SET TMGRESULT=0
        NEW IEN62D4 SET IEN62D4=TMGENV("IEN 62.4")
        NEW TMPIEN SET TMPIEN=$ORDER(^LAB(62.4,IEN62D4,3,"AC",NLT,0))
        IF TMPIEN>0 DO  GOTO:TMGRESULT'=0 A2AIDN
        . NEW TMPIEN60 SET TMPIEN60=$PIECE($GET(^LAB(62.4,IEN62D4,3,TMPIEN,0)),"^",1)
        . IF TMPIEN60=IEN60 DO
        . . SET TMGRESULT="1^"_TMPIEN
        . ELSE  DO
        . . SET TMGRESULT="-1^Already linked to another entry."
        WRITE "ADDING ",TESTNAME," into AUTO INSTRUMENT record",!
        NEW TMGFDA,TMGMSG,TMGIEN
        NEW IENS SET IENS="+1,"_IEN62D4_","
        ;"Code patterned after code in LA7PCFG.m
        SET TMGFDA(62.41,IENS,.01)=+IEN60      ;"TEST [-> 60]  NOTE: setting this causes trigger --> sets field 11
        SET TMGFDA(62.41,IENS,6)=NLTCODE       ;"UI TEST CODE [F]
        SET TMGFDA(62.41,IENS,8)=+IEN61        ;"SPECIMEN [-> 61]
        SET TMGFDA(62.41,IENS,14)=2            ;"ACCEPT RESULTS FOR THIS TEST [S]  //2=FINAL ONLY
        SET TMGFDA(62.41,IENS,18)=2            ;"STORE REMARKS [S]    //2=FINAL ONLY
        SET TMGFDA(62.41,IENS,19)="For "_TESTNAME_": "   ;"REMARK PREFIX [F]
        SET TMGFDA(62.41,IENS,21)=1            ;"STORE REFERENCE RANGE [S] //1=YES
        DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO A2AIDN  
        . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        NEW IEN62D41 SET IEN62D41=+$GET(TMGIEN(1))
        IF IEN62D41'>0 GOTO A2AIDN
        SET TMGRESULT="1^"_IEN62D41
A2AIDN  QUIT TMGRESULT
        ;
SELLAB60(TESTNAME) ;"Ask user to pick record in file 60 to match TEST NAME 
        ;"Input: NAME -- Display Name of test being added.
        ;"         or NAME^Alt-Name
        ;"Result: IEN60^Name, or 0 if none chosen, or -1^Message if error. 
        NEW SELOPT,Y 
        SET TESTNAME=$GET(TESTNAME)
        NEW ALTNAME SET ALTNAME=$PIECE(TESTNAME,"^",2)
        SET TESTNAME=$PIECE(TESTNAME,"^",1)
        NEW HINTNAME SET HINTNAME=TESTNAME
        IF (ALTNAME'="")&(ALTNAME'=HINTNAME) SET HINTNAME="'"_HINTNAME_"' aka '"_ALTNAME_"'"
        SET SELOPT("NCS")=1 ;"not case sensitive
        SET SELOPT("HEADER",1)="Please select a record from the LABORATORY TEST file"
        IF TESTNAME'="" DO
        . SET SELOPT("HEADER",2)="that matches: {{RED}}"_HINTNAME_"{{NORM}}"
        . SET SELOPT("HEADER",3)="{{BOLD}}NOTE: if no match is found, press ^ for chance to add NEW entry{{NORM}}"
        SET SELOPT("COLORS","NORM")="7^4"
        SET SELOPT("COLORS","BOLD")="14^4"
        SET SELOPT("COLORS","RED")="14^1"
        SET SELOPT("COLORS","FOOTER")="14^6"
        SET SELOPT("SCRN WIDTH")=70
        WRITE #
        SET Y=$$RECSEL^TMGUSRI4(60,,.SELOPT)
        WRITE #
        QUIT Y
        ;
GETAI(HL7INST) ;
        ;"Purpose: To get AUTO INSTRUMENT to work on, using stored value if available.
        ;"Input: HL7INST -- Institution, as found in HL7 message.  
        ;"Result: IEN in 62.4, or -1 if not found or problem. 
        NEW IEN62D4 SET IEN62D4=0
        NEW IEN22720 SET IEN22720=0
        SET HL7INST=$GET(HL7INST) IF HL7INST="" SET HL7INST="?"
        SET IEN22720=+$ORDER(^TMG(22720,"D",HL7INST,0))
        IF IEN22720>0 DO  GOTO GAIDN
        . SET IEN62D4=$PIECE($GET(^TMG(22720,IEN22720,0)),"^",3)
        SET IEN62D4=$GET(^TMG("TMP","TMGHL70","IEN 62.4"))
        IF +IEN62D4>0 GOTO GAIDN
        WRITE "First we must pick the AUTO INSTRUMENT entry that",!
        WRITE "will contain the a list of all tests for which ",!
        WRITE "RESULTS can be accepted from a given laboratory.",!
        DO ^DIC WRITE !
        IF Y'>0 GOTO GAIDN
        SET IEN62D4=+Y
        SET ^TMG("TMP","TMGHL70","IEN 62.4")=IEN62D4
GAIDN   QUIT IEN62D4
        ;
