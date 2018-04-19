TMGHL7H ;TMG/kst-HL7 transformation engine processing ;03/26/11, 2/2/14
              ;;1.0;TMG-LIB;**1**;03/26/11
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
 ;"=======================================================================
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"HELP(LEVEL,MODE) -- Executable help for fileman entry for file 22720
 ;"
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"TMGUSRIF
 ;"=======================================================================
 ;
HELP(LEVEL,MODE) ;
        ;"Purpose: Executable help for fileman entry for file 22720
        ;"Input: LEVEL -- 0=Entire message
        ;"                1=Segment level
        ;"                2=Field level
        ;"                3=Component level
        ;"                4=Subcomponent level
        ;"       MODE --  "PRE" -- pre-run xform code field
        ;"                "POST" -- post-run xform code field
        ;"                "" or null, neither above.
        SET LEVEL=$GET(LEVEL)
        SET MODE=$GET(MODE)
        ;"WRITE "TEST HELP.  LEVEL=",LEVEL," MODE=",MODE,!
        NEW LEVELNAME SET LEVELNAME=""
        IF LEVEL=0 SET LEVELNAME="ENTIRE HL7 MESSAGE"
        ELSE  IF LEVEL=1 SET LEVELNAME="ENTIRE SEGMENT"
        ELSE  IF LEVEL=2 SET LEVELNAME="ONE FIELD"
        ELSE  IF LEVEL=3 SET LEVELNAME="ONE COMPONENT"
        ELSE  IF LEVEL=4 SET LEVELNAME="ONE SUBCOMPONENT"
        NEW TMGUSERINPUT,TMGMNU,TMGMNUI
M1      ;
        KILL TMGMNU,TMGMNUI SET TMGMNUI=0
        SET TMGMNU(TMGMNUI)="TRANSFORMATION CODE HELP FOR "_LEVELNAME,TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="Overview"_$CHAR(9)_"Overview",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="Variables available for use"_$CHAR(9)_"Vars",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="Why is code rejected?"_$CHAR(9)_"Validity",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="Example code"_$CHAR(9)_"Example",TMGMNUI=TMGMNUI+1
        ;
        WRITE #
        SET TMGUSERINPUT=$$MENU^TMGUSRI2(.TMGMNU,"^")
        KILL TMGMNU ;"Prevent from cluttering variable table during debug run
        ;
        IF TMGUSERINPUT="Overview" DO OVERVIEW(LEVEL,MODE) GOTO M1
        IF TMGUSERINPUT="Vars" DO VARS(LEVEL,MODE) GOTO M1
        IF TMGUSERINPUT="Example" DO EXAMP(LEVEL,MODE) GOTO M1
        IF TMGUSERINPUT="Validity" DO VALIDITY GOTO M1
        ;
        IF TMGUSERINPUT="^" GOTO M1DN
        IF TMGUSERINPUT=0 SET TMGUSERINPUT=""
        GOTO M1
        ;
M1DN    QUIT
        ;
VALIDITY ;
        ;"Purpose: Explain validity check
        WRITE "Code is stored in the database using FILEMAN.  During the",!
        WRITE "storage process, FILEMAN checks that user code is valid.",!
        WRITE "Part of this check is that the code must be UPPER CASE.",!
        DO PRESS2GO^TMGUSRI2
        QUIT
        ;
OVERVIEW(LEVEL,MODE) ;
        ;"Purpose: to provide overview help
        ;"USES LEVELNAME, MODE -- globally scoped
        ;
        WRITE !,"OVERVIEW",!
        WRITE "------------",!
        WRITE "  The purpose of transform code is to change an element of the ",!
        WRITE "HL7 message, so that it will be in proper format for VistA to",!
        WRITE "process.",!
        WRITE "  This transformation must occur in custom code, written in MUMPS,",!
        WRITE "that will be called by the transformation engine.",!
        WRITE "  The message may be processed on several different levels: as an",!
        WRITE "entire message, or as a single segment, field, component, or sub-",!
        WRITE "component.  This help will tailor the instructions given depending",!
        WRITE "on the level the field represents.",!
        WRITE "  It is permissible for fields to be left blank.  In those",!
        WRITE "cases, the message will not be processed at that level, and code",!
        WRITE "for a sub-level could handle a particular issue.",!
        DO PRESS2GO^TMGUSRI2
        WRITE "  Each particular part that is to be changed will require a separate",!
        WRITE "entry for transform code.  For example, IF the 2nd component of the",!
        WRITE "3rd field of the OBX segment in the HL7 message needed to be",!
        WRITE "modified, then one would SET up the following records to have the",!
        WRITE "example code CMP23OBX^MYMOD to be executed.",!
        WRITE "  FLD 11 -- SEGMENT XFORM (multiple)",!
        WRITE "    FLD .01 -- SEGMENT FOR XFORM = ""OBX""",!
        WRITE "    FLD 11 -- FIELD XFORM (multiple)",!
        WRITE "      FLD .01 -- FIELD NUMBER FOR XFORM = 3",!
        WRITE "      FLD 11 -- COMPONENT XFORM (multiple)",!
        WRITE "        FLD .01 -- COMPONENT NUMBER FOR XFORM = 2",!
        WRITE "        FLD 10 -- PRERUN XFORM CODE = ""DO CMP23OBX^MYMOD""  <-- example",!
        ;
OVL1    IF LEVEL>3 GOTO OVL4
        WRITE "This field holds ",MODE,"-run code for ",LEVELNAME,!
        IF MODE="PRE" DO PRE
        IF MODE="POST" DO POST
        GOTO OVDN
OVL4    WRITE "This field holds transform code for ",LEVELNAME,!
OVDN    WRITE "See help sections about available variables to learn how to make",!
        WRITE "actual chages.",!
        DO PRESS2GO^TMGUSRI2
        QUIT
        ;
PRE     WRITE "This is code that will be executed BEFORE any code that exists",!
        WRITE "for processing sub-pieces of the HL7 message and before any POST-",!
        WRITE "RUN transform code.",!
        WRITE "The variable TMGVALUE contains the entire piece.  If this piece",!
        WRITE "contains sub-pieces, then the provided code must detect these",!
        WRITE "and handle them.  It is permissible to leave this field blank.",!
        QUIT
        ;
POST    WRITE "This is code that will be executed AFTER any code that exists",!
        WRITE "for PRERUN transform code, and after any code for processing sub-",!
        WRITE "pieces of the HL7 message.",!
        WRITE "The variable TMGVALUE contains the entire piece.  If this piece",!
        WRITE "contains sub-pieces, then the provided code must detect these",!
        WRITE "and handle them.  It is permissible to leave this field blank.",!
        QUIT
        ;
VARS(LEVEL,MODE) ;
        ;
        NEW TMGUSERINPUT,TMGMNU,TMGMNUI
M2      ;
        KILL TMGMNU,TMGMNUI SET TMGMNUI=0
        SET TMGMNU(TMGMNUI)="VARIABLES AVAILABLE FOR TRANSFORM CODE FOR "_LEVELNAME,TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="TMGHL7MSG -- Array with entire HL7 message parsed"_$CHAR(9)_"TMGHL7MSG",TMGMNUI=TMGMNUI+1
        IF LEVEL>0 DO
        . SET TMGMNU(TMGMNUI)="TMGSEG -- name of SEGMENT being processed"_$CHAR(9)_"TMGSEG",TMGMNUI=TMGMNUI+1
        IF LEVEL>0 DO
        IF LEVEL>0 DO
        . SET TMGMNU(TMGMNUI)="TMGVALUE -- value of particular "_$PIECE(LEVELNAME," ",2,3)_" being processed"_$CHAR(9)_"TMGVALUE",TMGMNUI=TMGMNUI+1
        IF (LEVEL>0)&(LEVEL<4) DO
        . SET TMGMNU(TMGMNUI)="TMGHASSUBS -- If has sub-pieces"_$CHAR(9)_"TMGHASSUBS",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="TMGU -- the divisor characters for HL7 message"_$CHAR(9)_"TMGU",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="TMGXERR -- Error channel"_$CHAR(9)_"TMXERR",TMGMNUI=TMGMNUI+1
        ;
        WRITE #
        SET TMGUSERINPUT=$$MENU^TMGUSRI2(.TMGMNU,"^")
        KILL TMGMNU ;"Prevent from cluttering variable table during debug run
        ;
        IF TMGUSERINPUT="TMGHL7MSG" DO HL7MSG(LEVEL) GOTO M2
        IF TMGUSERINPUT="TMGSEG" DO TMGSEG GOTO M2
        IF TMGUSERINPUT="TMGNUM" DO TMGNUM(LEVEL) GOTO M2
        IF TMGUSERINPUT="TMGVALUE" DO TMGVAL(LEVEL,MODE) GOTO M2
        IF TMGUSERINPUT="TMGU" DO TMGU GOTO M2
        IF TMGUSERINPUT="TMGHASSUBS" DO TMGSUBS(LEVEL) GOTO M2
        IF TMGUSERINPUT="TMXERR" DO TMGXERR GOTO M2
        ;
        IF TMGUSERINPUT="^" GOTO M2DN
        IF TMGUSERINPUT=0 SET TMGUSERINPUT=""
        GOTO M2
        ;
M2DN    QUIT
        ;
HL7MSG(LEVEL) ;
        WRITE !,"TMGHL7MSG",!
        WRITE "------------------",!
        WRITE "  TMGHL7MSG is an arry that contains the entire HL7 message",!
        WRITE "parsed out.",!
        WRITE "  User code may modify TMGHL7MSG, and changes will be reflected",!
        WRITE "in the final HL7 message.",!
        IF LEVEL>0 DO
        . WRITE "  However, after transform code is executed, TMGVALUE will be stored",!
        . WRITE "back into the array, at the appropriate piece.  This could overwrite",!
        . WRITE "changes made directly to TMGHL7MSG.",!,!
        WRITE "TMGHL7MSG has the following format:",!
        WRITE !
        WRITE "Each segment will be parsed into TMGHL7MSG(#), in",!
        WRITE "the order they are found in the original HL7 message.",!
        WRITE !
        WRITE "Example: IF the 8th segment in the HL7 message was:",!
        WRITE "     OBX|3|ST|UBL^BLOOD,URINE^L||NEGATIVE||NEGATIVE|N||A^S|F|||1234|ML^MAIN LAB",!
        WRITE !
        WRITE "    TMGHL7MSG(8)=""OBX|3|ST|UBL^BLOOD,URINE^L||NEGATIVE||NEGATIVE|N||A^S|F|||123""",!
        WRITE "    TMGHL7MSG(8,""SEG"")=""OBX""",!
        WRITE "    TMGHL7MSG(8,1)=""3""",!
        WRITE "    TMGHL7MSG(8,2)=""ST""",!
        WRITE "    TMGHL7MSG(8,3)=""UBL^BLOOD,URINE^L""",!
        DO PRESS2GO^TMGUSRI2
        WRITE "    TMGHL7MSG(8,3,1)=""UBL""",!
        WRITE "    TMGHL7MSG(8,3,2)=""BLOOD""",!
        WRITE "    TMGHL7MSG(8,3,3)=""L""",!
        WRITE "    TMGHL7MSG(8,4)=""",!
        WRITE "    TMGHL7MSG(8,5)=""NEGATIVE""",!
        WRITE "    TMGHL7MSG(8,6)=""",!
        WRITE "    TMGHL7MSG(8,7)=""NEGATIVE""",!
        WRITE "    TMGHL7MSG(8,8)=""N""",!
        WRITE "    TMGHL7MSG(8,9)="" ",!
        WRITE "    TMGHL7MSG(8,10)=""A^S""",!
        WRITE "    TMGHL7MSG(8,10,1)=""A""",!
        WRITE "    TMGHL7MSG(8,10,2)=""S""",!
        WRITE "    TMGHL7MSG(8,11)=""F""",!
        WRITE "    TMGHL7MSG(8,12)="" ",!
        WRITE "    TMGHL7MSG(8,13)="" ",!
        DO PRESS2GO^TMGUSRI2
        WRITE "    TMGHL7MSG(8,14)=""1234""",!
        WRITE "    TMGHL7MSG(8,15)=""ML^MAIN LAB^L""",!
        WRITE "    TMGHL7MSG(8,15,1)=""M""",!
        WRITE "    TMGHL7MSG(8,15,2)=""MAIN LAB"" ",!
        WRITE "    TMGHL7MSG(8,15,3)=""L""",!
        WRITE "    TMGHL7MSG(8,16)=""   ",!
        WRITE "    TMGHL7MSG(8,17)=""   ",!
        WRITE "    TMGHL7MSG(8,18)=""   ",!
        WRITE "    TMGHL7MSG(""B"",""MSH"",1)=""   <-- will act as index.",!
        WRITE "    TMGHL7MSG(""B"",""PID"",2)="""
        WRITE "    ...",!
        WRITE "    TMGHL7MSG(""B"",""OBX"",8)="""
        WRITE "    ...",!
        WRITE "    TMGHL7MSG(""PO"",1,8)=""   <-- Processing order index.",!
        WRITE "    TMGHL7MSG(""PO"",2,2)="""
        WRITE "    TMGHL7MSG(""PO"",3,5)="""
        WRITE "    TMGHL7MSG(""PO"",4,2)="""
        WRITE "    ...",!
        DO PRESS2GO^TMGUSRI2
        QUIT
        ;
TMGSEG  ;
        WRITE !,"TMGSEG",!
        WRITE "------------------",!
        WRITE "TMGSEG will contain the name of the current segment being",!
        WRITE "processed.  E.g. IF MSH being processed, then ""MSH"" will",!
        WRITE "stored in TMGSEG",!
        DO PRESS2GO^TMGUSRI2
        QUIT
        ;
TMGNUM(LEVEL) ;
        WRITE !,"TMGNUM",!
        WRITE "------------------",!
        WRITE "TMGNUM will contain the index number of the ",$PIECE(LEVELNAME," ",2,3),!
        WRITE "being processed.  E.g. IF it is the 3rd one, TMGNUM=3.",!
        DO PRESS2GO^TMGUSRI2
        QUIT
        ;
TMGVAL(LEVEL,MODE) ;
        WRITE !,"TMGVALUE",!
        WRITE "------------------",!
        WRITE "TMGVALUE will contain the value of the piece to be modified, and it",!
        WRITE "is this value that will be stored back into TMGHL7MSG, and ultimately",!
        WRITE "be included in the final HL7 message.",!
        WRITE "To be clear, THIS IS THE VARIABLE TO MODIFY IN ONE'S CODE.",!
        IF MODE="" GOTO TMGV2
        WRITE "Because this is ",MODE,"-run code, TMGVALUE will contain the text of",!
        WRITE "the ",LEVELNAME
        IF LEVEL=1 DO
        . WRITE " which will contain all the fields of the segment,",!
        . WRITE " but the first piece (e.g. 'PID') will be removed so that",!
        . WRITE " the first field will be piece #1 of TMGVALUE.",!
        IF LEVEL=2 DO
        . WRITE " which may contain components of the field.",!
        IF LEVEL=3 DO
        . WRITE " which may contain sub-components of the component.",!
        ELSE  WRITE !
        IF (MODE="PRE") DO
        . WRITE "TMGVAL will be stored back into TMGHL7MSG before executing any",!
        . WRITE "code for processing sub-pieces, or any POSTRUN code.",!
TMGV2   DO PRESS2GO^TMGUSRI2
        QUIT
        ;
TMGU    ;
        WRITE !,"TMGU",!
        WRITE "------------------",!
        WRITE "TMGU will contain divisor characters for the HL7 message.",!
        WRITE "These are SET by the sender of the message.",!
        WRITE "TMGU(1) -- divisor char that separates fields",!
        WRITE "TMGU(2) -- divisor char that separates components",!
        WRITE "TMGU(1) -- divisor char that separates sub-compoents",!
        DO PRESS2GO^TMGUSRI2
        QUIT
        ;
TMGSUBS(LEVEL) ;
        WRITE !,"TMGHASSUBS",!
        WRITE "------------------",!
        WRITE "TMGHASSUBS will equal 1 (=1) IF TMGVALUE has sub pieces.",!
        WRITE "Otherwise it will equal 0 (=0).",!
        IF LEVEL=1 DO
        . WRITE "I.e. IF segment contains fields.",!
        IF LEVEL=2 DO
        . WRITE "I.e. IF field contains components",!
        IF LEVEL=3 DO
        . WRITE "I.e. IF component contains sub-components",!
        DO PRESS2GO^TMGUSRI2
        QUIT
        ;
TMGXERR ;
        WRITE !,"TMGXERR",!
        WRITE "------------------",!
        WRITE "TMGXERR is available to functions for use in passing",!
        WRITE "back errors.  Any value assigned to TMGXERR will be",!
        WRITE "interpreted as an error message, and will cause further",!
        WRITE "processing to be aborted, and the error reported.",!
        DO PRESS2GO^TMGUSRI2
        QUIT
        ;
EXAMP(LEVEL,MODE) ;
        ;
        NEW TMGUSERINPUT,TMGMNU,TMGMNUI
M3      ;
        KILL TMGMNU,TMGMNUI SET TMGMNUI=0
        SET TMGMNU(TMGMNUI)="CODE EXAMPLES FOR "_LEVELNAME,TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="General principles"_$CHAR(9)_"GENPRNC",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="Manipulating TMGHL7MSG directly"_$CHAR(9)_"DEMOHL7",TMGMNUI=TMGMNUI+1
        IF LEVEL=1 DO
        . SET TMGMNU(TMGMNUI)="Working with entire segment"_$CHAR(9)_"DEMOSEG",TMGMNUI=TMGMNUI+1
        IF LEVEL=2 DO
        . SET TMGMNU(TMGMNUI)="Working with one field"_$CHAR(9)_"DEMOFLD",TMGMNUI=TMGMNUI+1
        IF LEVEL=3 DO
        . SET TMGMNU(TMGMNUI)="Working with one component"_$CHAR(9)_"DEMOCMP",TMGMNUI=TMGMNUI+1
        IF LEVEL=4 DO
        . SET TMGMNU(TMGMNUI)="Working with one subcomponent"_$CHAR(9)_"DEMOSCMP",TMGMNUI=TMGMNUI+1
        ;
        WRITE #
        SET TMGUSERINPUT=$$MENU^TMGUSRI2(.TMGMNU,"^")
        KILL TMGMNU ;"Prevent from cluttering variable table during debug run
        ;
        IF TMGUSERINPUT="GENPRNC" DO GENPRNC GOTO M3
        IF TMGUSERINPUT="DEMOHL7" DO DEMOHL7 GOTO M3
        IF TMGUSERINPUT="DEMOSEG" DO DEMOSEG GOTO M3
        IF TMGUSERINPUT="DEMOFLD" DO DEMOFLD GOTO M3
        IF TMGUSERINPUT="DEMOCMP" DO DEMOCMP GOTO M3
        IF TMGUSERINPUT="DEMOSCMP" DO DEMOSCMP GOTO M3
        ;
        IF TMGUSERINPUT="^" GOTO M3DN
        IF TMGUSERINPUT=0 SET TMGUSERINPUT=""
        GOTO M3
        ;
M3DN    QUIT
        ;
GENPRNC WRITE !,"Storing complex code directly in the XFORM fields makes for difficult",!
        WRITE "debugging and code maintenance.  So it is recommended that the field",!
        WRITE "store simple code as follows",!
        WRITE "  'DO OBX23^MYMOD'",!
        WRITE "Then store the more complex logic of your transform in a code module.",!
        DO PRESS2GO^TMGUSRI2
        QUIT
        ;
DEMOHL7 ;
        WRITE !,"To work with the message on the level of the TMGHL7MSG array,",!
        WRITE "simple read the array from the appropriate segment, field, etc",!
        WRITE "(see TMGHL7MSG help for format), and make the modifications directly.",!
        WRITE "When the transformation engine has run all possible transform codes,",!
        WRITE "it will assemble the array back into a standard HL7 message",!
        DO PRESS2GO^TMGUSRI2
        QUIT
        ;
DEMOSEG WRITE !,"To work with the message on the level of the entire segment, then",!
        WRITE "determine which field needs to be modified.  Use code like this to",!
        WRITE "obtain the particular entry <x>:",!
        WRITE "  NEW TEMP SET TEMP=$PIECE(TMGVALUE,TMGU(1),<x>)",!
        WRITE "then perform any testing or modification needed.",!
        WRITE "Note that TEMP (a field) may contain component pieces.",!
        WRITE "Note also that IF information needed to be extracted from other ",!
        WRITE "segments, TMGHL7MSG is always available.",!
        WRITE "When done, replace the value like this:",!
        WRITE "  SET $PIECE(TMGVALUE,TMGU(I),<x>)=TEMP",!
        DO PRESS2GO^TMGUSRI2
        QUIT
        ;
DEMOFLD WRITE !,"To modify just one field, work directly with TMGVALUE, which will",!
        WRITE "contain the field value.  To determine IF there are components",!
        WRITE "contained in the field, use code like this:",!
        WRITE "IF TMGVALUE[TMGU(2) SET TEMP=$PIECE(TMGVALUE,TMGU(2),1) etc.",!
        WRITE "After testing and modifying TMGVALUE, QUIT the procedure and",!
        WRITE "TMGVALUE will be stored again into TMGHL7MSG",!
        DO PRESS2GO^TMGUSRI2
        QUIT
        ;
DEMOCMP WRITE !,"To modify just one component, work directly with TMGVALUE, which",!
        WRITE "will contain the component value.  To determine IF there are sub-",!
        WRITE "components contained in the field, use code like this:",!
        WRITE "IF TMGVALUE[TMGU(3) SET TEMP=$PIECE(TMGVALUE,TMGU(2),1) etc.",!
        WRITE "After testing and modifying TMGVALUE, QUIT the procedure and",!
        WRITE "TMGVALUE will be stored again into TMGHL7MSG",!
        DO PRESS2GO^TMGUSRI2
        QUIT
        ;
DEMOSCMP WRITE !,"To modify just one sub-component, work directly with TMGVALUE, which",!
        WRITE "which will contain the sub-component value.  ",!
        WRITE "After testing and modifying TMGVALUE, QUIT the procedure and",!
        WRITE "TMGVALUE will be stored again into TMGHL7MSG",1
        DO PRESS2GO^TMGUSRI2
        QUIT
        ;

