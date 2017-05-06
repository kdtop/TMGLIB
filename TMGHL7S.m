TMGHL7S ;TMG/kst-HL7 transformation engine processing ;03/28/11, 2/2/14
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
 ;"=======================================================================
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"SETUP(IEN22720,TMGTESTMSG) --set up transformation code.
 ;"
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;"NSURTMGU(TMGTESTMSG,IEN22720,TMGU) ;ENSURE SETUP TMGU (divider chars)  
 ;"LOAD(TMGTESTMSG,TMGHL7MSG,TMGU) -- load a test message for reference during setup.
 ;"LOAD772(IEN773,TMGTESTMSG,TMGHL7MSG,TMGU) -- load a test message for reference during setup.
 ;"XFORM(IEN22720,TMGHL7MSG,TMGU,INDENTN) -- TRANSFORM MENU
 ;"PREMSG(IEN22720) ;
 ;"POSTMSG(IEN22720) ;
 ;"ENTMSG(IEN22720,DR) ;
 ;"SEG(IEN22720,TMGHL7MSG,TMGU,INDENTN) ;
 ;"ONESEG(IEN22720,IENSEG,SEGN,TMGHL7MSG,TMGU,INDENTN) ;
 ;"APPSEG(IEN22720,IENSEG,INDENTN) ;"Ask Pre vs Pos
 ;"PRESEG(IEN22720,IENSEG) ;
 ;"POSTSEG(IEN22720,IENSEG) ;
 ;"ENTSEG(IEN22720,IENSEG,FLD) ;
 ;"EDITORDER(IEN22720,IENSEG,TMGHL7MSG) ;
 ;"ONEFLD(IEN22720,IENSEG,SEGN,FLDN,TMGHL7MSG,TMGU,LABEL,INDENTN) ;
 ;"APPFLD(IEN22720,IENSEG,FLDN,INDENTN) --Ask Pre vs Pos
 ;"PREFLD(IEN22720,IENSEG,FLDN) ;
 ;"POSTFLD(IEN22720,IENSEG,FLDN) ;
 ;"ENTFLD(IEN22720,IENSEG,FLDN,FLD) ;
 ;"ONECOMP(IEN22720,IENSEG,SEGN,FLDN,COMPN,TMGHL7MSG,TMGU,LABEL,INDENTN) ;
 ;"APPCMP(IEN22720,IENSEG,FLDN,COMPN,INDENTN) --Ask Pre vs Pos
 ;"PRECMP(IEN22720,IENSEG,FLDN,COMPN) ;
 ;"POSTCMP(IEN22720,IENSEG,FLDN,COMPN) ;
 ;"ENTCMP(IEN22720,IENSEG,FLDN,COMPN,FLD) ;
 ;"ONESCOMP(IEN22720,IENSEG,SEGN,FLDN,COMPN,SCMPN,TMGHL7MSG,TMGU,LABEL)
 ;"ENTSCMP(IEN22720,IENSEG,FLDN,COMPN,SCMPN,FLD) ;
 ;"ASKPP(LABEL1,LABEL2,IEN22720,IENSEG,FLDN,COMPN,INDENTN) ;
 ;"HASCODE0(IEN22720,PREPOST) ;
 ;"HASCODE1(IEN22720,IENSEG,PREPOST) -- Check segments and below
 ;"HASCODE2(IEN22720,IENSEG,FLDN,PREPOST) -- Check fields and below
 ;"HASCODE3(IEN22720,IENSEG,FLDN,COMPN,PREPOST) -- Check components and below
 ;"HASCODE4(IEN22720,IENSEG,FLDN,COMPN,SCMPN,) -- Check sub-component
 ;"PROCORD(TMGHL7MSG) -- show processing order
 ;"VIEWALL(IEN22720) -- dump out all current code
 ;"VIEWSEG(IEN22720,IENSEG) --take parsed message array, and applied transforms
 ;"VIEWFLD(IEN22720,IENSEG,TMGFLDN) --take parsed message array, and applied transforms
 ;"VIEWCOMP(IEN22720,IENSEG,TMGFLDN,TMGCOMPN) --take parsed message array, and applied transforms
 ;"VIEWSCMP(IEN22720,IENSEG,TMGFLDN,TMGCOMPN,TMGSCMPN) -- take parsed message array, and applied transforms
 ;"SUMAP(IEN22720,TMGWKLD,IEN64,IEN63D04,TMGINVAL,TMGHLP,INDENTN) ; --set up mapping of invalid --> VALID values 
 ;"FIXIXRM(IEN63D04,TMGVALUE,TMGNAME,INDENTN) --FIX INPUT TRANSFORM
 ;"AUTOFXST(SETSTR,IEN63D04,TMGVALUE,INDENTN) --AUTO (with user input) FIX ^DD for sets
 ;"EXTENDST(SETSTR,TMGVALUE,INDENTN) --EXTEND (ADD) TO SET
 ;"AUTOSFXR(IXFRM,IXFRMHELP,TMGVALUE) --AUTO (with user input) FIX TRANSFORM for strings
 ;"AUTOFXRM(IXFRM,IXFRMHELP,TMGVALUE) --AUTO (with user input) FIX TRANSFORM for numbers
 ;"SUMREPL(IEN22720,IEN64,TMGINVAL,INDENTN) --SET UP REPLACEMENT VALUE 
 ;"SUMCODE(IEN22720,IEN64,TMGINVAL,TMGNAME,TMGWKLD) --SET UP CODE TO APPLY TO ALL VALUES
 ;"STORMAP(IEN22720,IEN64,TMGINVAL,TMGNEWVAL,TMGCODE) -- store a mapping of TMGINVAL --> TMGNEWVAL
 ;"SUMFRERR(IEN22720,TMGRESULT,INDENTN) --Set up Map from Error Msg
 ;"TESTXFRM(TMGENV,TMGTESTMSG,TMGHL7MSG,TMGU,INDENTN) --show result of transform        
 ;" 
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"TMGDEBUG,TMGSTUTL,TMGHL7X2,TMGUSRIF
 ;"=======================================================================
 ;
SETUP(TMGENV,TMGTESTMSG,INDENTN) ;
        ;"Purpose: To SET up transformation code.
        ;"Input: TMGENV -- PASS BY REFERENCE.  Lab environment.  Setup in SETUPENV^TMGHL7U 
        ;"           TMGENV("PREFIX") -- e.g. "LMH"
        ;"           TMGENV("IEN 68.2") -- IEN in LOAD/WORK LIST (holds orderable items)
        ;"           TMGENV("IEN 62.4") -- IEN in AUTO INSTRUMENT (holds resultable items)
        ;"           TMGENV("IEN PROFILE IN 68.2")=1
        ;"           TMGENV("IEN 22720")=IEN22720
        ;"           TMGENV("INST")=TMGINST
        ;"        TMGTESTMSG -- optional.  This can be the message to work
        ;"              on.  If not provided, then user will be prompted to
        ;"              load one in via an editor.  Input format:
        ;"              TMGTESTMSG(1)="1st line"
        ;"              TMGTESTMSG(2)="2nd line etc."
        ;"        INDENTN -- OPTIONAL.  the number of spaces to indent the menu display
        ;
        ;"Result: none
        NEW DIC,X,Y,%,TMGU
        NEW TMGINST SET TMGINST=""
        NEW TMGRESULT SET TMGRESULT=1
        SET INDENTN=+$GET(INDENTN)
        SET IEN22720=+$GET(TMGENV("IEN 22720"))
        IF IEN22720>0 DO  GOTO SU2
        . SET TMGINST=$PIECE($GET(^TMG(22720,IEN22720,0)),"^",1)
        SET DIC=22720,DIC(0)="MAEQL"
        DO ^DIC WRITE !
        IF Y'>0 GOTO SUDN
        IF $PIECE(Y,"^",3)'="" DO
        . WRITE "This is a NEW record.  Needs to be SET up...",!
        . ;"FINISH.... USE ^DIE
        NEW TMGINST SET TMGINST=$PIECE(Y,"^",2)
        SET IEN22720=+Y
SU2     NEW TMGUSERINPUT,TMGMNU
        NEW TMGMNUI
        NEW INDENTSTR SET INDENTSTR=$J("",INDENTN)
        WRITE !,!
        WRITE INDENTSTR,"Welcome to the HL7 Transformation engine assistant.",!
        WRITE INDENTSTR,"-------------------------------------------------------",!
        NEW TMGHL7MSG,TMGU
        IF $DATA(TMGTESTMSG)>0 GOTO M0        
        WRITE "This code works best when using a sample message.",!
        SET %=1
        WRITE "Load sample HL7 message now" DO YN^DICN WRITE !
        IF %=-1 GOTO SUDN
        IF %=2 GOTO M1
        DO FILEMENU^TMGHL70(.TMGTESTMSG)
M0      DO NSURTMGU(.TMGTESTMSG,.IEN22720,.TMGU)         
        SET TMGRESULT=$$PRSEARRY^TMGHL7X2(IEN22720,.TMGTESTMSG,.TMGHL7MSG) ;
        IF TMGRESULT<0 DO  GOTO SUDN
        . WRITE "Error parsing passed test HL7 message:",!
        . WRITE $PIECE(TMGRESULT,"^",2),!
        . DO PRESS2GO^TMGUSRI2
M1      DO NSURTMGU(.TMGTESTMSG,.IEN22720,.TMGU)
        KILL TMGUSERINPUT,TMGMNU
        KILL TMGMNUI SET TMGMNUI=0
        SET TMGMNU(-1,"INDENT")=INDENTN
        SET TMGMNU(TMGMNUI)="Pick HL7 TRANSFORMATION ENGINE Option. "_TMGINST,TMGMNUI=TMGMNUI+1
        IF $DATA(TMGTESTMSG) DO
        . SET TMGMNU(TMGMNUI)="View current HL7 message"_$CHAR(9)_"ViewMsg",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="<Transform Code Setup Menu>"_$CHAR(9)_"AddXForm",TMGMNUI=TMGMNUI+1
        IF $DATA(TMGHL7MSG) DO
        . SET TMGMNU(TMGMNUI)="Test transform settings on loaded test HL7 message"_$CHAR(9)_"TestXForm",TMGMNUI=TMGMNUI+1
        ;
        SET TMGUSERINPUT=$$MENU^TMGUSRI2(.TMGMNU,"^")
        KILL TMGMNU ;"Prevent from cluttering variable table during debug run
        ;
M1B     IF TMGUSERINPUT="ViewMsg" DO VIEWMSG^TMGHL7U2(.TMGTESTMSG) GOTO M1
        IF TMGUSERINPUT="LoadMsg" DO LOAD(.TMGTESTMSG,.TMGHL7MSG,.TMGU) GOTO M1
        IF TMGUSERINPUT="AddXForm" DO XFORM(IEN22720,.TMGHL7MSG,.TMGU,INDENTN+2) GOTO M1
        IF TMGUSERINPUT="ClearMsg" KILL TMGTESTMSG GOTO M1
        IF TMGUSERINPUT="TestXForm" DO TESTXFRM(.TMGENV,.TMGTESTMSG,.TMGHL7MSG,.TMGU,INDENTN+2) GOTO M1
        IF TMGUSERINPUT="^" GOTO SUDN
        IF TMGUSERINPUT=0 SET TMGUSERINPUT=""
        GOTO M1
        ;
SUDN    WRITE "Quitting transformation engine.",!
        QUIT
        ;
NSURTMGU(TMGTESTMSG,IEN22720,TMGU) ;"ENSURE SETUP TMGU (divider chars)
        IF $DATA(TMGU)'>0 DO 
        . SET TMGRESULT=$$GETDIV^TMGHL7U(.TMGTESTMSG,.IEN22720,.TMGU)
        . IF TMGRESULT>0 QUIT
        . WRITE $PIECE(TMGRESULT,"^",2),!
        . DO PRESS2GO^TMGUSRI2
        QUIT
        
LOAD(TMGTESTMSG,TMGHL7MSG,TMGU) ;
        ;"Purpose: to load a test message for reference during setup.
        ;"Input: TMGTESTMSG -- PASS BY REFERENCE. AN OUT PARAMETER. 
        ;"       TMGHL7MSG -- PASS BY REFERENCE. AN OUT PARAMETER. 
        ;"       TMGU -- PASS BY REFERENCE. AN OUT PARAMETER. 
        ;"Accesses IEN22720 by global scope
        ;"Result: none
        NEW TMGHLMSG,TMGINSTRUCT
        SET TMGINSTRUCT="Delete these lines, and replace them with the HL7 message to work on."
        SET TMGTESTMSG(1)=TMGINSTRUCT
        SET TMGTESTMSG(2)="Then exit editor to continue."
        DO EDITARR^TMGEDIT($NAME(TMGTESTMSG),"joe")
        IF ($GET(TMGTESTMSG(1))=TMGINSTRUCT)!($GET(TMGTESTMSG(1))="") DO  GOTO LOADDN
        . WRITE "Sorry.  No HL7 Message.",!
        . DO PRESS2GO^TMGUSRI2
AD2     SET TMGRESULT=$$PRSEARRY^TMGHL7X2(IEN22720,.TMGTESTMSG,.TMGHL7MSG,.TMGU) ;
        IF TMGRESULT<0 DO  GOTO LOADDN
        . WRITE $PIECE(TMGRESULT,"^",2),!
        . DO PRESS2GO^TMGUSRI2
LOADDN  QUIT
        ;
LOAD772(IEN773,TMGTESTMSG,TMGHL7MSG,TMGU) ;
        ;"Purpose: to load a test message for reference during setup.
        ;"Input: IEN773 -- IEN IN 773 (HL7 MESSAGE ADMINISTRATION)
        ;"       TMGTESTMSG -- PASS BY REFERENCE. AN OUT PARAMETER. 
        ;"       TMGHL7MSG -- PASS BY REFERENCE. AN OUT PARAMETER. 
        ;"       TMGU -- PASS BY REFERENCE. AN OUT PARAMETER. 
        ;"Accesses IEN22720 by global scope (but not required)
        ;"Result: none
        SET IEN773=+$GET(IEN773) IF IEN773'>0 GOTO LD772DN
        NEW MSH SET MSH=$GET(^HLMA(IEN773,"MSH",1,0))
        IF MSH="" GOTO LD772DN
        NEW IEN772 SET IEN772=+$PIECE($GET(^HLMA(IEN773,0)),"^",1) 
        IF IEN772'>0 GOTO LD772DN
        NEW ARI SET ARI=1
        SET TMGTESTMSG(ARI)=MSH,ARI=ARI+1
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(^HL(772,IEN772,"IN",IDX)) QUIT:(+IDX'>0)  DO
        . NEW STR SET STR=$GET(^HL(772,IEN772,"IN",IDX,0)) QUIT:STR=""
        . NEW P SET P=0
        . FOR P=1:1:$LENGTH(STR) DO
        . . NEW A SET A=$ASCII($EXTRACT(STR,P))
        . . IF ((A<32)!(A>126))&(A'=10)&(A'=13) DO
        . . . SET STR=$EXTRACT(STR,1,P-1)_" "_$EXTRACT(STR,P+1,$LENGTH(STR))
        . SET TMGTESTMSG(ARI)=STR,ARI=ARI+1
        NEW TMGRESULT
        IF +$GET(IEN22720)'>0 DO   GOTO:(TMGRESULT<0) LD72
        . NEW HLMTIEN SET HLMTIEN=IEN772
        . NEW HLMTIENS SET HLMTIENS=IEN773
        . SET TMGRESULT=$$SETFMENV^TMGHL7U()
        SET TMGRESULT=$$PRSEARRY^TMGHL7X2(IEN22720,.TMGTESTMSG,.TMGHL7MSG,.TMGU) ;
LD72    IF TMGRESULT<0 DO  GOTO LD772DN
        . WRITE $PIECE(TMGRESULT,"^",2),!
        . DO PRESS2GO^TMGUSRI2
LD772DN QUIT
        ;
XFORM(IEN22720,TMGHL7MSG,TMGU,INDENTN)   ;
        ;
        NEW TMGUSERINPUT,TMGMNU
        NEW TMGMNUI,DISP
M2      ;
        KILL TMGUSERINPUT,TMGMNU
        KILL TMGMNUI SET TMGMNUI=0
        SET INDENTN=+$GET(INDENTN)
        SET TMGMNU(-1,"INDENT")=INDENTN
        SET TMGMNU(TMGMNUI)="Pick what to add transform code for.",TMGMNUI=TMGMNUI+1
        SET DISP="Entire HL7 message, before segments processed"
        IF $$HASCODE0(IEN22720,"PRE") SET DISP="(*)"_DISP
        SET TMGMNU(TMGMNUI)=DISP_$CHAR(9)_"PreMsg",TMGMNUI=TMGMNUI+1
        SET DISP="A particular segment"
        IF $$HASCODE0(IEN22720,"CHILDONLY") SET DISP="(*)"_DISP
        SET TMGMNU(TMGMNUI)=DISP_$CHAR(9)_"Seg",TMGMNUI=TMGMNUI+1
        SET DISP="Entire HL7 message, after segments processed"
        IF $$HASCODE0(IEN22720,"POST") SET DISP="(*)"_DISP
        SET TMGMNU(TMGMNUI)=DISP_$CHAR(9)_"PostMsg",TMGMNUI=TMGMNUI+1
        IF $DATA(TMGTESTMSG) DO
        . SET TMGMNU(TMGMNUI)="View current HL7 message"_$CHAR(9)_"ViewMsg",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="Dump all transformation code"_$CHAR(9)_"ViewAll",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="Show segment processing order"_$CHAR(9)_"ProcessOrder",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="Done adding transform code"_$CHAR(9)_"^",TMGMNUI=TMGMNUI+1
        ;
        WRITE #
        SET TMGUSERINPUT=$$MENU^TMGUSRI2(.TMGMNU,"^")
        KILL TMGMNU ;"Prevent from cluttering variable table during debug run
        ;
        IF TMGUSERINPUT="PreMsg" DO PREMSG(IEN22720) GOTO M2
        IF TMGUSERINPUT="Seg" DO SEG(IEN22720,.TMGHL7MSG,.TMGU,INDENTN+2) GOTO M2
        IF TMGUSERINPUT="PostMsg" DO POSTMSG(IEN22720) GOTO M2
        IF TMGUSERINPUT="ViewAll" DO VIEWALL(IEN22720) GOTO M2
        IF TMGUSERINPUT="ProcessOrder" DO PROCORD(.TMGHL7MSG) GOTO M2
        IF TMGUSERINPUT="^" GOTO M2DN
        IF TMGUSERINPUT=0 SET TMGUSERINPUT=""
        GOTO M2
        ;
M2DN    QUIT
        ;
PREMSG(IEN22720) ;
        DO ENTMSG(IEN22720,"10")
        QUIT
        ;
POSTMSG(IEN22720) ;
        DO ENTMSG(IEN22720,"12")
        QUIT
        ;
ENTMSG(IEN22720,DR) ;
        WRITE "Enter in transform code to handle entire message.  '?' for help.",!
        NEW DIE,DA
        SET DIE=22720
        SET DA=IEN22720
        DO ^DIE
        QUIT
        ;
SEG(IEN22720,TMGHL7MSG,TMGU,INDENTN) ;
        ;
        NEW TMGUSERINPUT,TMGMNU
        NEW TMGMNUI,TMGFOUND
        NEW IENSEG SET IENSEG=0
        NEW SEGN SET SEGN=""
        NEW MAXSEGN SET MAXSEGN=0
M3      ;
        KILL TMGUSERINPUT,TMGMNU,TMGFOUND
        SET INDENTN=+$GET(INDENTN)
        SET TMGMNU(-1,"INDENT")=INDENTN
        SET SEGN=""
        SET TMGMNUI=1
        ;"Get segments form example/current message
        FOR  SET SEGN=$ORDER(TMGHL7MSG(SEGN)) QUIT:(+SEGN'>0)  DO
        . IF SEGN>MAXSEGN SET MAXSEGN=SEGN
        . NEW SEG SET SEG=$GET(TMGHL7MSG(SEGN,"SEG")) QUIT:SEG=""
        . SET IENSEG=+$ORDER(^TMG(22720,IEN22720,11,"B",SEG,""))
        . IF $DATA(TMGFOUND(SEG)) SET TMGFOUND=+$GET(TMGFOUND)+1
        . ELSE  SET TMGFOUND=0
        . SET TMGFOUND(SEG)=1
        . NEW DISP SET DISP="Segment: "_SEG
        . IF +$GET(TMGFOUND)>0 SET DISP=DISP_", example #"_(TMGFOUND+1)
        . IF $$HASCODE1(IEN22720,IENSEG) SET DISP="(*) "_DISP
        . SET TMGMNU(TMGMNUI)=DISP_$CHAR(9)_SEGN_"^"_IENSEG,TMGMNUI=TMGMNUI+1
        ;"Get segments from file 22720, not in example,current message
        SET IENSEG=0
        FOR  SET IENSEG=$ORDER(^TMG(22720,IEN22720,11,IENSEG)) QUIT:(+IENSEG'>0)  DO
        . NEW SEG SET SEG=$PIECE($GET(^TMG(22720,IEN22720,11,IENSEG,0)),"^",1) QUIT:SEG=""
        . IF $GET(TMGFOUND(SEG))=1 QUIT
        . NEW DISP SET DISP="Segment: "_SEG
        . IF $$HASCODE1(IEN22720,IENSEG) SET DISP="(*) "_DISP
        . SET TMGMNU(TMGMNUI)=DISP_$CHAR(9)_"0^"_IENSEG,TMGMNUI=TMGMNUI+1
        . SET MAXSEGN=MAXSEGN+1
        SET TMGMNU(TMGMNUI)="(A segment number not in example)"_$CHAR(9)_"OtherNum",TMGMNUI=TMGMNUI+1
        IF $DATA(TMGMNU) DO
        . SET TMGMNU(0)="Pick Segment to be Transformed"
        . IF $DATA(TMGTESTMSG) DO
        . . SET TMGMNU(TMGMNUI)="View current HL7 message"_$CHAR(9)_"ViewMsg",TMGMNUI=TMGMNUI+1
        . SET TMGMNU(TMGMNUI)="Done"_$CHAR(9)_"^",TMGMNUI=TMGMNUI+1
        ELSE  GOTO M3B
        ;
        WRITE #
        SET TMGUSERINPUT=$$MENU^TMGUSRI2(.TMGMNU,"^")
        KILL TMGMNU ;"Prevent from cluttering variable table during debug run
        ;
        IF TMGUSERINPUT="OtherNum" DO
        . SET TMGUSERINPUT=MAXSEGN+1
        . ;"READ "Enter in custom segment Number: ",TMGUSERINPUT:$GET(DTIME,3600),!
        IF (+TMGUSERINPUT>0)!(+$PIECE(TMGUSERINPUT,"^",2)>0) DO  GOTO M3
        . DO ONESEG(IEN22720,+$PIECE(TMGUSERINPUT,"^",2),+TMGUSERINPUT,.TMGHL7MSG,.TMGU,INDENTN+2)
        IF TMGUSERINPUT="ViewMsg" DO VIEWMSG^TMGHL7U2(.TMGTESTMSG) GOTO M3DN
        IF TMGUSERINPUT="^" GOTO M3DN
        IF TMGUSERINPUT=0 SET TMGUSERINPUT=""
        GOTO M3
        ;
M3B     NEW DIE,DR,DA
        SET DIE=22720,DA=IEN22720
        SET DR="11"
        SET DR(2,22720.011)=".01"  ;"<--- FINISH AND TEST
        WRITE "To be completed.  Allow adding NEW SEG's to handle",!
        DO PRESS2GO^TMGUSRI2
M3DN    QUIT
        ;
        ;"----------------------------------------------------
ONESEG(IEN22720,IENSEG,SEGN,TMGHL7MSG,TMGU,INDENTN) ;
        ;"Input: IENSEG -- optional. IEN of segment record.  Record added IF not provided
        ;"       SEGN -- optional.  Index number of segment in TMGHL7MSG
        ;"       TMGHL7MSG
        ;"       TMGU
        ;"       INDENTN -- OPTIONAL.  the number of spaces to indent the menu display
        ;"Note: Either SEGN or IENSEG must be provided.
        NEW TMGRESULT SET TMGRESULT=1
        SET SEGN=+$GET(SEGN)
        SET IENSEG=+$GET(IENSEG)
        NEW SAMPL SET SAMPL=$GET(TMGHL7MSG(SEGN))
        NEW SEGNAME SET SEGNAME=$GET(TMGHL7MSG(SEGN,"SEG"))
        IF SEGNAME="",(IENSEG>0) DO
        . SET SEGNAME=$PIECE($GET(^TMG(22720,IEN22720,11,IENSEG,0)),"^",1)
        IF SEGNAME="" DO  GOTO:(SEGNAME="") OSGDN
        . NEW % SET %=2
        . FOR  QUIT:(%=-1)!(SEGNAME'="")  DO
        . . WRITE "Enter 3-letter name for segment #",SEGN,": "
        . . READ SEGNAME:$GET(DTIME,3600),!
        . . IF SEGNAME["^" SET SEGNAME="",%=-1 QUIT
        . . IF $LENGTH(SEGNAME)'=3 DO  QUIT
        . . . WRITE "Invalid entry.  Must be exactly 3 characters (^ to abort)",!
        . . . SET SEGNAME=""
        . . SET %=2
        . . SET SEGNAME=$$UP^XLFSTR(SEGNAME)
        . . WRITE "Use '",SEGNAME,"' for segment #",SEGN
        . . DO YN^DICN WRITE !
        . . IF %'=1 SET SEGNAME=""
OS2     IF IENSEG'>0 DO  ;"add NEW subfile record
        . NEW TMGFDA,TMGIEN,TMGMSG
        . SET TMGFDA(22720.011,"+1,"_IEN22720_",",.01)=SEGNAME
        . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) DO  QUIT
        . . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        . SET IENSEG=+$GET(TMGIEN(1))
        . IF IENSEG'>0 DO  QUIT
        . . SET TMGRESULT="-1^Unable to locate added subrecord in 22720.011"
        IF TMGRESULT<0 DO  GOTO OSGDN
        . WRITE "ERROR creating sub-record:",!
        . WRITE $PIECE(TMGRESULT,"^",2),!
        . DO PRESS2GO^TMGUSRI2
        NEW TMGUSERINPUT,TMGMNU
        NEW TMGMNUI,FLDN,FOUND
M4      ;
        KILL TMGUSERINPUT,TMGMNU,FOUND
        SET INDENTN=+$GET(INDENTN)
        SET TMGMNU(-1,"INDENT")=INDENTN
        SET TMGMNUI=1,FLDN=0
        FOR  SET FLDN=$ORDER(TMGHL7MSG(SEGN,FLDN)) QUIT:(+FLDN'>0)  DO
        . NEW S SET S=$GET(TMGHL7MSG(SEGN,FLDN)) QUIT:$$TRIM^XLFSTR(S)=""
        . SET FOUND(FLDN)=1
        . NEW DISP SET DISP="Field #"_FLDN_",  e.g. '"_S_"'"
        . IF $$HASCODE2(IEN22720,IENSEG,FLDN) SET DISP="(*) "_DISP
        . SET TMGMNU(FLDN)=DISP_$CHAR(9)_FLDN ;"TMGMNUI=TMGMNUI+1
        SET FLDN=0
        FOR  SET FLDN=$ORDER(^TMG(22720,IEN22720,11,IENSEG,11,FLDN)) QUIT:(+FLDN'>0)  DO
        . IF $GET(FOUND(FLDN))=1 QUIT
        . NEW DISP SET DISP="Field #"_FLDN
        . IF $$HASCODE2(IEN22720,IENSEG,FLDN) SET DISP="(*) "_DISP
        . SET TMGMNU(FLDN)=DISP_$CHAR(9)_FLDN ;"TMGMNUI=TMGMNUI+1
        SET TMGMNU(0)="Pick what to add transform code for.  ["_SEGNAME_"]"
        ;"SET TMGMNU(.5)="Entire segment before fields processed"_$CHAR(9)_"Prerun"
        SET TMGMNUI=$ORDER(TMGMNU(""),-1)+1
        SET TMGMNU(TMGMNUI)="(A Field number not in example)"_$CHAR(9)_"OtherNum",TMGMNUI=TMGMNUI+1
        ;"SET TMGMNU(TMGMNUI)="Entire segment before fields processed"_$CHAR(9)_"Postrun",TMGMNUI=TMGMNUI+1
        SET DISP="Whole Segment: '"_$EXTRACT(SAMPL,1,25)_"...'"
        IF $$HASCODE1(IEN22720,IENSEG,"NOCHILD") SET DISP="(*) "_DISP
        SET TMGMNU(TMGMNUI)=DISP_$CHAR(9)_"XForm",TMGMNUI=TMGMNUI+1
        IF SAMPL'="" DO
        . SET TMGMNU(TMGMNUI)="View current example segment"_$CHAR(9)_"Example",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="Edit Processing Order number for this segment"_$CHAR(9)_"Order",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="Done adding transform code"_$CHAR(9)_"^",TMGMNUI=TMGMNUI+1
        ;
        WRITE #
        SET TMGUSERINPUT=$$MENU^TMGUSRI2(.TMGMNU,"^")
        KILL TMGMNU ;"Prevent from cluttering variable table during debug run
        ;
        IF TMGUSERINPUT="OtherNum" DO
        . READ "Enter in custom Field Number: ",TMGUSERINPUT:$GET(DTIME,3600),!
        IF +TMGUSERINPUT>0 DO  GOTO M4
        . DO ONEFLD(IEN22720,IENSEG,SEGN,+TMGUSERINPUT,.TMGHL7MSG,.TMGU,SEGNAME_";"_+TMGUSERINPUT,INDENTN+2)
        IF TMGUSERINPUT="XForm" DO APPSEG(IEN22720,IENSEG,INDENTN+2) GOTO M4
        IF TMGUSERINPUT="Prerun" DO PRESEG(IEN22720,IENSEG) GOTO M4
        IF TMGUSERINPUT="Postrun" DO POSTSEG(IEN22720,IENSEG) GOTO M4
        IF TMGUSERINPUT="Order" DO EDITORDER(IEN22720,IENSEG,.TMGHL7MSG) GOTO M4
        IF TMGUSERINPUT="Example" DO  GOTO M4
        . WRITE !,"Example segment from test HL7 message:",!
        . WRITE SAMPL,!
        . DO PRESS2GO^TMGUSRI2
        IF TMGUSERINPUT="^" GOTO OSGDN
        IF TMGUSERINPUT=0 SET TMGUSERINPUT=""
        GOTO M4
OSG2    ;
        WRITE "Finish... allow picking of fields even without example.",!
        DO PRESS2GO^TMGUSRI2
OSGDN   QUIT
        ;
APPSEG(IEN22720,IENSEG,INDENTN) ;"Ask Pre vs Pos
        NEW % SET %=$$ASKPP("COMPONENT","SUB-COMPONENT",IEN22720,IENSEG,,,.INDENTN)
        IF %=1 DO PRESEG(IEN22720,IENSEG) ;
        IF %=2 DO POSTSEG(IEN22720,IENSEG) ;
        QUIT
        ;
PRESEG(IEN22720,IENSEG) ;
        DO ENTSEG(IEN22720,IENSEG,"10")
        QUIT
        ;
POSTSEG(IEN22720,IENSEG) ;
        DO ENTSEG(IEN22720,IENSEG,"12")
        QUIT
        ;
ENTSEG(IEN22720,IENSEG,FLD) ;
        WRITE "Enter in transform code to handle entire segment.  '?' for help.",!
        NEW DIE,DA,DR
        SET DIE="^TMG(22720,"_IEN22720_",11,"
        SET DA=IENSEG
        SET DA(1)=IEN22720
        SET DR=FLD
        DO ^DIE
        QUIT
        ;
EDITORDER(IEN22720,IENSEG,TMGHL7MSG) ;
        WRITE "Enter in the processing sequence number for this segment.  '?' for help.",!
        NEW DIE,DA,DR
        SET DIE="^TMG(22720,"_IEN22720_",11,"
        SET DA=IENSEG
        SET DA(1)=IEN22720
        SET DR="1"
        DO ^DIE
        DO SETPOIDX^TMGHL7X2(IEN22720,.TMGHL7MSG)
        QUIT

        ;"----------------------------------------------------
ONEFLD(IEN22720,IENSEG,SEGN,FLDN,TMGHL7MSG,TMGU,LABEL,INDENTN) ;
        ;"Input: IENSEG -- optional. IEN of segment record.  Record added IF not provided
        ;"       SEGN -- optional.  Index number of segment in TMGHL7MSG
        ;"Note: Either SEGN or IENSEG must be provided.
        ;"
        NEW TMGRESULT SET TMGRESULT=1
        SET SEGN=+$GET(SEGN)
        NEW SAMPL SET SAMPL=$GET(TMGHL7MSG(SEGN,FLDN))
        IF $DATA(^TMG(22720,IEN22720,11,IENSEG,11,FLDN))=0 DO  ;"add NEW subfile record
        . NEW TMGFDA,TMGIEN,TMGMSG
        . SET TMGFDA(22720.1111,"+1,"_IENSEG_","_IEN22720_",",.01)=FLDN
        . SET TMGIEN(1)=FLDN
        . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) DO  QUIT
        . . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        . SET FLDN=+$GET(TMGIEN(1))
        . IF FLDN'>0 DO  QUIT
        . . SET TMGRESULT="-1^Unable to locate added subrecord in 22720.1111"
        IF TMGRESULT<0 DO  GOTO OFLDN
        . WRITE "ERROR creating sub-record:",!
        . WRITE $PIECE(TMGRESULT,"^",2),!
        . DO PRESS2GO^TMGUSRI2
        NEW TMGUSERINPUT,TMGMNU
        NEW TMGMNUI,COMPN,FOUND
M5      ;
        KILL TMGUSERINPUT,TMGMNU
        SET INDENTN=+$GET(INDENTN)
        SET TMGMNU(-1,"INDENT")=INDENTN
        SET TMGMNUI=1,COMPN=0
        FOR  SET COMPN=$ORDER(TMGHL7MSG(SEGN,FLDN,COMPN)) QUIT:(+COMPN'>0)  DO
        . NEW S SET S=$GET(TMGHL7MSG(SEGN,FLDN,COMPN)) QUIT:$$TRIM^XLFSTR(S)=""
        . SET FOUND(COMPN)=1
        . NEW DISP SET DISP="Component #"_COMPN_",  e.g. '"_S_"'"
        . IF $$HASCODE3(IEN22720,IENSEG,FLDN,COMPN) SET DISP="(*) "_DISP
        . SET TMGMNU(COMPN)=DISP_$CHAR(9)_COMPN
        SET COMPN=0
        FOR  SET COMPN=$ORDER(^TMG(22720,IEN22720,11,IENSEG,11,FLDN,11,COMPN)) QUIT:(+COMPN'>0)  DO
        . IF $GET(FOUND(COMPN))=1 QUIT
        . NEW DISP SET DISP="Component #"_COMPN
        . IF $$HASCODE3(IEN22720,IENSEG,FLDN,COMPN) SET DISP="(*) "_DISP
        . SET TMGMNU(COMPN)=DISP_$CHAR(9)_COMPN
        SET TMGMNU(0)="Pick what to add transform code for.  ["_LABEL_"]"
        ;"SET TMGMNU(.5)="Field before components processed, e.g. "_SAMPL_$CHAR(9)_"Prerun"
        SET TMGMNUI=$ORDER(TMGMNU(""),-1)+1
        SET TMGMNU(TMGMNUI)="(A Component number not in example)"_$CHAR(9)_"OtherNum",TMGMNUI=TMGMNUI+1
        ;"SET TMGMNU(TMGMNUI)="Field before components processed, e.g. "_SAMPL_$CHAR(9)_"Postrun",TMGMNUI=TMGMNUI+1
        SET DISP="Whole Field: '"_SAMPL_"'"
        IF $$HASCODE2(IEN22720,IENSEG,FLDN,"NOCHILD") SET DISP="(*) "_DISP
        SET TMGMNU(TMGMNUI)=DISP_$CHAR(9)_"XForm",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="Done adding transform code"_$CHAR(9)_"^",TMGMNUI=TMGMNUI+1
        ;
        WRITE #
        SET TMGUSERINPUT=$$MENU^TMGUSRI2(.TMGMNU,"^")
        KILL TMGMNU ;"Prevent from cluttering variable table during debug run
        ;
        IF TMGUSERINPUT="OtherNum" DO
        . READ "Enter in custom Component Number",TMGUSERINPUT:$GET(DTIME,3600),!
        IF +TMGUSERINPUT>0 DO  GOTO M5
        . DO ONECOMP(IEN22720,IENSEG,SEGN,FLDN,+TMGUSERINPUT,.TMGHL7MSG,.TMGU,LABEL_";"_+TMGUSERINPUT,INDENTN+2)
        IF TMGUSERINPUT="XForm" DO APPFLD(IEN22720,IENSEG,FLDN,INDENTN+2) GOTO M5
        IF TMGUSERINPUT="Prerun" DO PREFLD(IEN22720,IENSEG,FLDN) GOTO M5
        IF TMGUSERINPUT="Postrun" DO POSTFLD(IEN22720,IENSEG,FLDN) GOTO M5
        IF TMGUSERINPUT="Example" DO  GOTO M5
        . WRITE !,"Example segment from test HL7 message:",!
        . WRITE SAMPL,!
        . DO PRESS2GO^TMGUSRI2
        IF TMGUSERINPUT="^" GOTO OFLDN
        IF TMGUSERINPUT=0 SET TMGUSERINPUT=""
        GOTO M5
        ;
        WRITE "Finish... allow picking of fields even without example.",!
        DO PRESS2GO^TMGUSRI2
OFLDN   QUIT
        ;
APPFLD(IEN22720,IENSEG,FLDN,INDENTN) ;"Ask Pre vs Pos
        NEW % SET %=$$ASKPP("COMPONENT","SUB-COMPONENT",IEN22720,IENSEG,FLDN,,.INDENTN)
        IF %=1 DO PREFLD(IEN22720,IENSEG,FLDN)
        IF %=2 DO POSTFLD(IEN22720,IENSEG,FLDN)
        QUIT
        ;
PREFLD(IEN22720,IENSEG,FLDN) ;
        DO ENTFLD(IEN22720,IENSEG,FLDN,"10")
        QUIT
        ;
POSTFLD(IEN22720,IENSEG,FLDN) ;
        DO ENTFLD(IEN22720,IENSEG,FLDN,"12")
        QUIT
        ;
ENTFLD(IEN22720,IENSEG,FLDN,FLD) ;
        NEW DIE,DA,DR
        SET DIE="^TMG(22720,"_IEN22720_",11,"_IENSEG_",11,"
        SET DA=FLDN
        SET DA(1)=IEN22720
        SET DA(2)=IENSEG
        SET DR=FLD
        WRITE "Enter in transform code to handle this field.  '?' for help."
        DO ^DIE
        QUIT
        ;
        ;"----------------------------------------------------
ONECOMP(IEN22720,IENSEG,SEGN,FLDN,COMPN,TMGHL7MSG,TMGU,LABEL,INDENTN) ;
        ;"
        NEW TMGRESULT SET TMGRESULT=1
        NEW SAMPL SET SAMPL=$GET(TMGHL7MSG(SEGN,FLDN,COMPN))
        IF $DATA(^TMG(22720,IEN22720,11,IENSEG,11,FLDN,11,COMPN))=0 DO  ;"add NEW subfile record
        . NEW TMGFDA,TMGIEN,TMGMSG
        . SET TMGFDA(22720.111111,"+1,"_FLDN_","_IENSEG_","_IEN22720_",",.01)=COMPN
        . SET TMGIEN(1)=COMPN
        . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) DO  QUIT
        . . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        . SET FLDN=+$GET(TMGIEN(1))
        . IF FLDN'>0 DO  QUIT
        . . SET TMGRESULT="-1^Unable to locate added subrecord in 22720.1111"
        IF TMGRESULT<0 DO  GOTO OCPDN
        . WRITE "ERROR creating sub-record:",!
        . WRITE $PIECE(TMGRESULT,"^",2),!
        . DO PRESS2GO^TMGUSRI2
        NEW TMGUSERINPUT,TMGMNU
        NEW TMGMNUI,FIELD,FOUND
M6      ;
        KILL TMGUSERINPUT,TMGMNU,FOUND
        SET INDENTN=+$GET(INDENTN)
        SET TMGMNU(-1,"INDENT")=INDENTN
        SET TMGMNUI=1
        NEW SCMPN SET SCMPN=0
        FOR  SET SCMPN=$ORDER(TMGHL7MSG(SEGN,FLDN,COMPN,SCMPN)) QUIT:(+SCMPN'>0)  DO
        . NEW S SET S=$GET(TMGHL7MSG(SEGN,FLDN,COMPN,SCMPN)) QUIT:$$TRIM^XLFSTR(S)=""
        . SET FOUND(SCMPN)=1
        . NEW DISP SET DISP="Sub-component #"_SCMPN_",  e.g. '"_S_"'"
        . IF $$HASCODE3(IEN22720,IENSEG,FLDN,COMPN,SCMPN) SET DISP="(*) "_DISP
        . ;"SET TMGMNU(COMP)=DISP_$CHAR(9)_COMP
        . SET TMGMNU(SCMPN)=DISP_$CHAR(9)_SCMPN
        SET SCMPN=0
        FOR  SET SCMPN=$ORDER(^TMG(22720,IEN22720,11,IENSEG,11,FLDN,11,COMPN,11,SCMPN)) QUIT:(+SCMPN'>0)  DO
        . IF $GET(FOUND(SCMPN))=1 QUIT
        . NEW DISP SET DISP="Sub-component #"_SCMPN
        . IF $$HASCODE4(IEN22720,IENSEG,FLDN,COMPN,SCMPN) SET DISP="(*) "_DISP
        . SET TMGMNU(SCMPN)=DISP_$CHAR(9)_SCMPN
        SET TMGMNU(0)="Pick what to add transform code for.  ["_LABEL_"]"
        SET TMGMNUI=$ORDER(TMGMNU(""),-1)+1
        SET TMGMNU(TMGMNUI)="(A Sub-component number not in example)"_$CHAR(9)_"OtherNum",TMGMNUI=TMGMNUI+1
        SET DISP="Whole Component: '"_SAMPL_"'"
        IF $$HASCODE3(IEN22720,IENSEG,FLDN,COMPN,"NOCHILD") SET DISP="(*) "_DISP
        SET TMGMNU(TMGMNUI)=DISP_$CHAR(9)_"XForm",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="Done adding transform code"_$CHAR(9)_"^",TMGMNUI=TMGMNUI+1
        ;
        WRITE #
        SET TMGUSERINPUT=$$MENU^TMGUSRI2(.TMGMNU,"^")
        KILL TMGMNU ;"Prevent from cluttering variable table during debug run
        ;
        IF TMGUSERINPUT="OtherNum" DO
        . READ "Enter in custom Sub-component Number",TMGUSERINPUT:$GET(DTIME,3600),!
        IF +TMGUSERINPUT>0 DO  GOTO M6
        . DO ONESCOMP(IEN22720,IENSEG,SEGN,FLDN,COMPN,+TMGUSERINPUT,.TMGHL7MSG,.TMGU,LABEL_";"_+TMGUSERINPUT)
        IF TMGUSERINPUT="XForm" DO APPCMP(IEN22720,IENSEG,FLDN,COMPN,INDENTN+2) GOTO M6
        IF TMGUSERINPUT="Example" DO  GOTO M6
        . WRITE !,"Example segment from test HL7 message:",!
        . WRITE SAMPL,!
        . DO PRESS2GO^TMGUSRI2
        IF TMGUSERINPUT="^" GOTO OCPDN
        IF TMGUSERINPUT=0 SET TMGUSERINPUT=""
        GOTO M6
        ;
        WRITE "Finish... allow picking of fields even without example.",!
        DO PRESS2GO^TMGUSRI2
OCPDN   QUIT
        ;
APPCMP(IEN22720,IENSEG,FLDN,COMPN,INDENTN) ;"Ask Pre vs Pos
        NEW % SET %=$$ASKPP("COMPONENT","SUB-COMPONENT",IEN22720,IENSEG,FLDN,COMPN,.INDENTN)
        IF %=1 DO PRECMP(IEN22720,IENSEG,FLDN,COMPN)
        IF %=2 DO POSTCMP(IEN22720,IENSEG,FLDN,COMPN)
        QUIT
        ;
PRECMP(IEN22720,IENSEG,FLDN,COMPN) ;
        DO ENTCMP(IEN22720,IENSEG,FLDN,COMPN,"10")
        QUIT
        ;
POSTCMP(IEN22720,IENSEG,FLDN,COMPN) ;
        DO ENTCMP(IEN22720,IENSEG,FLDN,COMPN,"12")
        QUIT
        ;
ENTCMP(IEN22720,IENSEG,FLDN,COMPN,FLD) ;
        NEW DIE,DA,DR
        SET DIE="^TMG(22720,"_IEN22720_",11,"_IENSEG_",11,"_FLDN_",11,"
        SET DA=COMPN
        SET DA(1)=IEN22720
        SET DA(2)=IENSEG
        SET DA(3)=FLDN
        SET DR=FLD
        WRITE "Enter in transform code to handle this component.  '?' for help."
        DO ^DIE
        QUIT
        ;
        ;"----------------------------------------------------
ONESCOMP(IEN22720,IENSEG,SEGN,FLDN,COMPN,SCMPN,TMGHL7MSG,TMGU,LABEL)
        ;"
        NEW TMGRESULT SET TMGRESULT=1
        NEW SAMPL SET SAMPL=$GET(TMGHL7MSG(SEGN,FLDN,COMPN,SCMPN))
        IF $DATA(^TMG(22720,IEN22720,11,IENSEG,11,FLDN,11,COMPN,11,SCMPN))=0 DO  ;"add NEW subfile record
        . NEW TMGFDA,TMGIEN,TMGMSG
        . SET TMGFDA(22720.11111111,"+1,"_COMPN_","_FLDN_","_IENSEG_","_IEN22720_",",.01)=COMPN
        . SET TMGIEN(1)=SCMPN
        . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) DO  QUIT
        . . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        . SET FLDN=+$GET(TMGIEN(1))
        . IF FLDN'>0 DO  QUIT
        . . SET TMGRESULT="-1^Unable to locate added subrecord in 22720.1111"
        IF TMGRESULT<0 DO  GOTO OSCPDN
        . WRITE "ERROR creating sub-record:",!
        . WRITE $PIECE(TMGRESULT,"^",2),!
        . DO PRESS2GO^TMGUSRI2
        DO ENTSCMP(IEN22720,IENSEG,FLDN,COMPN,SCMPN,"10") ;
OSCPDN  QUIT
        ;
ENTSCMP(IEN22720,IENSEG,FLDN,COMPN,SCMPN,FLD) ;
        NEW DIE,DA,DR
        SET DIE="^TMG(22720,"_IEN22720_",11,"_IENSEG_",11,"_FLDN_",11,"_COMPN_",11,"
        SET DA=SCMPN
        SET DA(1)=IEN22720
        SET DA(2)=IENSEG
        SET DA(3)=FLDN
        SET DA(3)=COMPN
        SET DR=FLD
        WRITE "Enter in transform code to handle this subcomponent.  '?' for help."
        DO ^DIE
        QUIT
        ;
        ;"----------------------------------------------------
ASKPP(LABEL1,LABEL2,IEN22720,IENSEG,FLDN,COMPN,INDENTN) ;
        ;"Input: LABEL1=name of this level being handled;
        ;"         LABEL2=neame of child level
        NEW TMGRESULT SET TMGRESULT=0
        NEW TMGUSERINPUT,TMGMNU,DISP
        NEW LEVEL SET LEVEL=0
        IF $GET(COMPN)'="" SET LEVEL=3 GOTO M7
        IF $GET(FLDN)'="" SET LEVEL=2 GOTO M7
        IF $GET(IENSEG)'="" SET LEVEL=1 GOTO M7
        IF $GET(IEN22720)=""  GOTO APPDN
M7      ;
        KILL TMGUSERINPUT,TMGMNU
        SET INDENTN=+$GET(INDENTN)
        SET TMGMNU(-1,"INDENT")=INDENTN
        KILL TMGMNUI SET TMGMNUI=0
        SET TMGMNU(TMGMNUI)="Pre-run or Post-run transform code?",TMGMNUI=TMGMNUI+1
        SET DISP="Transform code for "_LABEL1_", BEFORE processing any "_LABEL2
        IF LEVEL=1,$$HASCODE1(IEN22720,IENSEG,"PRE") SET DISP="(*) "_DISP
        IF LEVEL=2,$$HASCODE2(IEN22720,IENSEG,FLDN,"PRE") SET DISP="(*) "_DISP
        IF LEVEL=3,$$HASCODE3(IEN22720,IENSEG,FLDN,COMPN,"PRE") SET DISP="(*) "_DISP
        SET TMGMNU(TMGMNUI)=DISP_$CHAR(9)_"PRE",TMGMNUI=TMGMNUI+1

        SET DISP="Transform code for "_LABEL1_", AFTER processing any "_LABEL2
        IF LEVEL=1,$$HASCODE1(IEN22720,IENSEG,"POST") SET DISP="(*) "_DISP
        IF LEVEL=2,$$HASCODE2(IEN22720,IENSEG,FLDN,"POST") SET DISP="(*) "_DISP
        IF LEVEL=3,$$HASCODE3(IEN22720,IENSEG,FLDN,COMPN,"POST") SET DISP="(*) "_DISP
        SET TMGMNU(TMGMNUI)=DISP_$CHAR(9)_"POST",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="(Neither, QUIT)"_$CHAR(9)_"^",TMGMNUI=TMGMNUI+1
        ;
        WRITE #
        SET TMGUSERINPUT=$$MENU^TMGUSRI2(.TMGMNU,"1")
        KILL TMGMNU ;"Prevent from cluttering variable table during debug run
        ;
        IF TMGUSERINPUT="PRE" SET TMGRESULT=1 GOTO APPDN
        IF TMGUSERINPUT="POST" SET TMGRESULT=2 GOTO APPDN
        IF TMGUSERINPUT="^" GOTO APPDN
        IF TMGUSERINPUT=0 SET TMGUSERINPUT=""
        GOTO M7
APPDN   QUIT TMGRESULT
        ;
        ;"----------------------------------------------------
        ;"====================================================
        ;"----------------------------------------------------
        ;
HASCODE0(IEN22720,PREPOST) ;
        ;"Resuts: 1 IF has code (itself or children), 0 IF not
        ;"Input: IEN22720
        ;"         PREPOST - Optional.  If PRE, then precode only checked.
        ;"                IF POST, then only postcode checked.
        ;"                IF NOCHILD then only PRE/POST are checked, not children
        ;"                IF CHILDONLY, then only children checked.
        ;"                Otherwise, both checked AND CHILDREN
        NEW TMGRESULT SET TMGRESULT=0
        SET PREPOST=$GET(PREPOST)
        IF PREPOST="CHILDONLY" SET PREPOST="" GOTO H0
        IF PREPOST'="POST",$GET(^TMG(22720,IEN22720,10))'="" SET TMGRESULT=1
        IF PREPOST'="PRE",$GET(^TMG(22720,IEN22720,12))'="" SET TMGRESULT=1
        IF PREPOST'="" GOTO HC0DN
H0      NEW IENSEG SET IENSEG=0
        FOR  SET IENSEG=$ORDER(^TMG(22720,IEN22720,11,IENSEG)) QUIT:(+IENSEG'>0)!(TMGRESULT=1)  DO
        . SET TMGRESULT=$$HASCODE1(IEN22720,IENSEG,PREPOST)
HC0DN   QUIT TMGRESULT
        ;
HASCODE1(IEN22720,IENSEG,PREPOST) ;" Check segments and below
        NEW TMGRESULT SET TMGRESULT=0
        SET PREPOST=$GET(PREPOST)
        IF PREPOST="CHILDONLY" SET PREPOST="" GOTO H1
        IF PREPOST'="POST",$GET(^TMG(22720,IEN22720,11,IENSEG,10))'="" SET TMGRESULT=1
        IF PREPOST'="PRE",$GET(^TMG(22720,IEN22720,11,IENSEG,12))'="" SET TMGRESULT=1
        IF PREPOST'="" GOTO HC1DN
H1      NEW FLDN SET FLDN=0
        FOR  SET FLDN=$ORDER(^TMG(22720,IEN22720,11,IENSEG,11,FLDN)) QUIT:(+FLDN'>0)!(TMGRESULT=1)  DO
        . SET TMGRESULT=$$HASCODE2(IEN22720,IENSEG,FLDN,PREPOST)
HC1DN   QUIT TMGRESULT
        ;
HASCODE2(IEN22720,IENSEG,FLDN,PREPOST) ;" Check fields and below
        NEW TMGRESULT SET TMGRESULT=0
        SET PREPOST=$GET(PREPOST)
        IF PREPOST="CHILDONLY" SET PREPOST="" GOTO H2
        IF PREPOST'="POST",$GET(^TMG(22720,IEN22720,11,IENSEG,11,FLDN,10))'="" SET TMGRESULT=1
        IF PREPOST'="PRE",$GET(^TMG(22720,IEN22720,11,IENSEG,11,FLDN,12))'="" SET TMGRESULT=1
        IF PREPOST'="" GOTO HC2DN
H2      NEW COMPN SET COMPN=0
        FOR  SET COMPN=$ORDER(^TMG(22720,IEN22720,11,IENSEG,11,FLDN,11,COMPN)) QUIT:(+COMPN'>0)!(TMGRESULT=1)  DO
        . SET TMGRESULT=$$HASCODE3(IEN22720,IENSEG,FLDN,COMPN,PREPOST)
HC2DN   QUIT TMGRESULT
        ;
HASCODE3(IEN22720,IENSEG,FLDN,COMPN,PREPOST) ;" Check components and below
        NEW TMGRESULT SET TMGRESULT=0
        SET PREPOST=$GET(PREPOST)
        IF PREPOST="CHILDONLY" SET PREPOST="" GOTO H3
        IF PREPOST'="POST",$GET(^TMG(22720,IEN22720,11,IENSEG,11,FLDN,11,COMPN,10))'="" SET TMGRESULT=1
        IF PREPOST'="PRE",$GET(^TMG(22720,IEN22720,11,IENSEG,11,FLDN,11,COMPN,12))'="" SET TMGRESULT=1
        IF PREPOST'="" GOTO HC3DN
H3      NEW SCMPN SET SCMPN=0
        FOR  SET SCMPN=$ORDER(^TMG(22720,IEN22720,11,IENSEG,11,FLDN,11,COMPN,11,SCMPN)) QUIT:(+SCMPN'>0)!(TMGRESULT=1)  DO
        . SET TMGRESULT=$$HASCODE4(IEN22720,IENSEG,FLDN,COMPN,SCMPN)
HC3DN   QUIT TMGRESULT
        ;
HASCODE4(IEN22720,IENSEG,FLDN,COMPN,SCMPN,) ;" Check sub-component
        NEW TMGRESULT SET TMGRESULT=0
        IF $GET(^TMG(22720,IEN22720,11,IENSEG,11,FLDN,11,COMPN,11,SCMPN,10))'="" SET TMGRESULT=1
        QUIT TMGRESULT
        ;
        ;"----------------------------------------------------
        ;"====================================================
        ;"----------------------------------------------------
        ;
PROCORD(TMGHL7MSG) ;
        ;"Purpose: show processing order
        WRITE !,"Below is the order that segments will be processed in:",!
        NEW ORD SET ORD=0
        FOR  SET ORD=$ORDER(TMGHL7MSG("PO",ORD)) QUIT:(+ORD'>0)  DO
        . WRITE ORD,". "
        . NEW SEGN SET SEGN=+$GET(TMGHL7MSG("PO",ORD))
        . IF SEGN'>0 DO  QUIT
        . . WRITE "?? segment number missing",!
        . NEW SEGNAME SET SEGNAME=$GET(TMGHL7MSG(SEGN,"SEG"))
        . IF SEGNAME="" DO  QUIT
        . . WRITE "?? segment name for segment #",SEGN," is missing",!
        . WRITE SEGNAME
        . NEW SAMPL SET SAMPL=$GET(TMGHL7MSG(SEGN))
        . IF SAMPL="" WRITE ! QUIT
        . WRITE "   e.g. ",$EXTRACT(SAMPL,1,30),"...",!
        DO PRESS2GO^TMGUSRI2
        QUIT
        ;
VIEWALL(IEN22720) ;
        ;"Purpose: to dump out all current code
        ;"Input: IEN22720 -- IEN in file TMG HL7 MESSAGE TRANSFORM SETTINGS
        WRITE "Transformation code (if present) will be shown below.",!
        ;"------------ Entire message prerun code ------------
        NEW TMGCODREF SET TMGCODREF=$NAME(^TMG(22720,IEN22720,10))
        NEW TMGCODE SET TMGCODE=$GET(@TMGCODREF)
        IF TMGCODE'="" WRITE "ENTIRE MESSAGE (before segments): [",TMGCODE,"]",!
        ;"------------ Code for each segment ---------------
        NEW IENSEG SET IENSEG=0
        FOR  SET IENSEG=$ORDER(^TMG(22720,IEN22720,11,IENSEG)) QUIT:(+IENSEG'>0)  DO
        . WRITE " ",IENSEG,". SEGMENT ",$GET(^TMG(22720,IEN22720,11,IENSEG,0)),": ",!
        . ;"NEW CODE SET CODE=$GET(^TMG(22720,IEN22720,11,IENSEG,10))
        . ;"IF CODE="" SET CODE="<none>"
        . ;"WRITE "[",CODE,"]",!
        . DO VIEWSEG(IEN22720,IENSEG)
        . DO PRESS2GO^TMGUSRI2
        ;"------------ Entire message postrun code ------------
        SET TMGCODREF=$NAME(^TMG(22720,IEN22720,12))
        NEW TMGCODE SET TMGCODE=$GET(@TMGCODREF)
        IF TMGCODE'="" DO
        . WRITE "ENTIRE MESSAGE (after segments): [",TMGCODE,"]",!
        . DO PRESS2GO^TMGUSRI2
        QUIT
        ;
VIEWSEG(IEN22720,IENSEG) ;
        ;"Purpose: To take parsed message array, and applied transforms
        ;"Input: IEN22720 -- IEN in file TMG HL7 MESSAGE TRANSFORM SETTINGS
        ;"       IENSEG -- The segment number being handled.
        NEW TMGCODREF SET TMGCODREF=$NAME(^TMG(22720,IEN22720,11,IENSEG,10))
        NEW TMGCODE SET TMGCODE=$GET(@TMGCODREF)
        IF TMGCODE'="" WRITE "    ENTIRE SEGMENT (before fields):  [",TMGCODE,"]",!
        ;"------------ Code for each field ---------------
        NEW TMGFLDN SET TMGFLDN=0
        FOR  SET TMGFLDN=$ORDER(^TMG(22720,IEN22720,11,IENSEG,11,TMGFLDN)) QUIT:(+TMGFLDN'>0)  DO
        . WRITE "    FIELD ",$GET(^TMG(22720,IEN22720,11,IENSEG,11,TMGFLDN,0)),": ",!
        . ;"NEW CODE SET CODE=$GET(^TMG(22720,IEN22720,11,IENSEG,11,TMGFLDN,10))
        . ;"IF CODE="" SET CODE="<none>"
        . ;"WRITE "[",CODE,"]",!
        . DO VIEWFLD(IEN22720,IENSEG,TMGFLDN)
        ;"------------ Entire seg, postrun code ------------
        SET TMGCODREF=$NAME(^TMG(22720,IEN22720,11,IENSEG,12))
        NEW TMGCODE SET TMGCODE=$GET(@TMGCODREF)
        IF TMGCODE'="" WRITE "    ENTIRE SEGMENT (after fields): [",TMGCODE,"]",!
        QUIT
        ;
VIEWFLD(IEN22720,IENSEG,TMGFLDN) ;
        ;"Purpose: To take parsed message array, and applied transforms
        ;"Input: IEN22720 -- IEN in file TMG HL7 MESSAGE TRANSFORM SETTINGS
        ;"       IENSEG -- the segment number being handled
        ;"       TMGFLDN -- The field number being handled.
        NEW TMGCODREF SET TMGCODREF=$NAME(^TMG(22720,IEN22720,11,IENSEG,11,TMGFLDN,10))
        NEW TMGCODE SET TMGCODE=$GET(@TMGCODREF)
        IF TMGCODE'="" WRITE "      ENTIRE FIELD ",TMGFLDN," (before components): [",TMGCODE,"]",!
        ;"------------ Code for each component ---------------
        NEW TMGCOMPN SET TMGCOMPN=0
        FOR  SET TMGCOMPN=$ORDER(^TMG(22720,IEN22720,11,IENSEG,11,TMGFLDN,11,TMGCOMPN)) QUIT:(+TMGCOMPN'>0)  DO
        . WRITE "      COMPONENT ",$GET(^TMG(22720,IEN22720,11,IENSEG,11,TMGFLDN,11,TMGCOMPN,0)),": ",!
        . ;"NEW CODE SET CODE=$GET(^TMG(22720,IEN22720,11,IENSEG,11,TMGFLDN,11,TMGCOMPN,10))
        . ;"IF CODE="" SET CODE="<none>"
        . ;"WRITE "[",CODE,"]",!
        . DO VIEWCOMP(IEN22720,IENSEG,TMGFLDN,TMGCOMPN)
        ;"------------ Entire seg, postrun code ------------
        SET TMGCODREF=$NAME(^TMG(22720,IEN22720,11,IENSEG,11,TMGFLDN,12))
        NEW TMGCODE SET TMGCODE=$GET(@TMGCODREF)
        IF TMGCODE'="" WRITE "      ENTIRE FIELD ",TMGFLDN," (after components): [",TMGCODE,"]",!
        QUIT
        ;
VIEWCOMP(IEN22720,IENSEG,TMGFLDN,TMGCOMPN) ;
        ;"Purpose: To take parsed message array, and applied transforms
        ;"Input: IEN22720 -- IEN in file TMG HL7 MESSAGE TRANSFORM SETTINGS
        ;"       IENSEG -- the segment number being handled
        ;"       TMGFLDN -- The field number being handled.
        ;"       TMGCOMPN -- the number of the component being handled.
        NEW TMGCODREF SET TMGCODREF=$NAME(^TMG(22720,IEN22720,11,IENSEG,11,TMGFLDN,11,TMGCOMPN,10))
        NEW TMGCODE SET TMGCODE=$GET(@TMGCODREF)
        IF TMGCODE'="" WRITE "        ENTIRE COMPONENT ",TMGCOMPN," (before sub-components): [",TMGCODE,"]",!
        ;"------------ Code for each subcomponent ---------------
        NEW TMGSCMPN SET TMGSCMPN=0
        FOR  SET TMGSCMPN=$ORDER(^TMG(22720,IEN22720,11,IENSEG,11,TMGFLDN,11,TMGCOMPN,11,TMGSCMPN)) QUIT:(+TMGSCMPN'>0)  DO
        . ;"WRITE "        SUB-COMP ",$GET(^TMG(22720,IEN22720,11,IENSEG,11,TMGFLDN,11,TMGCOMPN,11,TMGSCMPN,0)),": ",!
        . ;"NEW CODE SET CODE=$GET(^TMG(22720,IEN22720,11,IENSEG,11,TMGFLDN,11,TMGCOMPN,11,TMGSCMPN,10))
        . ;"IF CODE="" SET CODE="<none>"
        . ;"WRITE "[",CODE,"]",!
        . DO VIEWSCMP(IEN22720,IENSEG,TMGFLDN,TMGCOMPN,TMGSCMPN)
        ;"------------ Entire seg, postrun code ------------
        SET TMGCODREF=$NAME(^TMG(22720,IEN22720,11,IENSEG,11,TMGFLDN,11,TMGCOMPN,12))
        NEW TMGCODE SET TMGCODE=$GET(@TMGCODREF)
        IF TMGCODE'="" WRITE "        ENTIRE COMPONENT ",TMGCOMPN," (after sub-components): [",TMGCODE,"]",!
        QUIT
        ;
VIEWSCMP(IEN22720,IENSEG,TMGFLDN,TMGCOMPN,TMGSCMPN) ;
        ;"Purpose: To take parsed message array, and applied transforms
        ;"Input: IEN22720 -- IEN in file TMG HL7 MESSAGE TRANSFORM SETTINGS
        ;"       IENSEG -- the segment number being handled
        ;"       TMGFLDN -- The field number being handled.
        ;"       TMGCOMPN -- the number of the component being handled.
        ;"         TMGSCMPN -- the number of the subcomponent being handled.
        NEW TMGCODREF SET TMGCODREF=$NAME(^TMG(22720,IEN22720,11,IENSEG,11,TMGFLDN,11,TMGCOMPN,11,TMGSCMPN,10))
        NEW TMGCODE SET TMGCODE=$GET(@TMGCODREF)
        IF TMGCODE'="" WRITE "          SUB-COMPONENT ",TMGSCMPN,": [",TMGCODE,"]",!
        QUIT
        ;"----------------------------------------------------
        ;"====================================================
        ;"----------------------------------------------------
        ;
SUMAP(IEN22720,TMGWKLD,IEN64,IEN63D04,TMGINVAL,TMGHLP,INDENTN) ;
        ;"Purpose: To SET up a mapping of invalid incoming values (e.g. 'NEGATIVE')
        ;"         to acceptable values that Fileman can store.
        ;"Input: IEN22720 -- The IEN of the record to store mapping in
        ;"       TMGWKLD -- a string containing the WKLD CODE that identifies the test
        ;"       IEN64 -- IEN in file 64  (may have other pieces)
        ;"       IEN63D04, -- IEN in 63.04 (storage IEN in file 63 parent file)
        ;"       TMGINVAL -- the abherent value that would be need to be fixed.
        ;"       TMGHLP -- Optional.  Fileman help prompt text to show user.
        ;"       INDENTN -- OPTIONAL.  the number of spaces to indent the menu display
        ;"Output: Data will be stored in database IF user agrees.
        ;"Results: none
        NEW TMGVAL,%
        NEW TMGUSERINPUT,TMGMENU
        SET INDENTN=+$GET(INDENTN)
        NEW INDENTSTR SET INDENTSTR=$JUSTIFY("",INDENTN)
        WRITE !,!,INDENTSTR,"Set up mapping for result '",TMGINVAL,"' for test '",TMGWKLD,"'.",!
        WRITE INDENTSTR,"-------------------------------------------------------",!
        ;"NEW IEN64 SET IEN64=+$ORDER(^LAM("C",TMGWKLD_" ",0))
        IF IEN64'>0 DO  GOTO SMPDN
        . WRITE "In SUMAP^TMGHL7S.  IEN 64 not provided to match '",TMGWKLD,"'",!
        . DO PRESS2GO^TMGUSRI2
        NEW TMGNAME SET TMGNAME=$$GET1^DIQ(64,+IEN64_",",.01)
        WRITE INDENTSTR,"Test: ",TMGNAME," #(",TMGWKLD,")",!
        WRITE INDENTSTR,"Invalid value: '",TMGINVAL,"'",!
        IF $GET(TMGHLP)'="" WRITE INDENTSTR,"Help: ",TMGHLP,!
SMP0    KILL TMGUSERINPUT,TMGMENU
        SET TMGMENU(-1,"INDENT")=INDENTN
        SET TMGMENU(0)="Pick option to fix input for "_TMGNAME_" #("_TMGWKLD_")"
        SET TMGMENU(1)="Add replacement value for: "_TMGINVAL_$CHAR(9)_"REPLACE"
        SET TMGMENU(2)="Enter code to fix ALL values for "_TMGNAME_$CHAR(9)_"CODE"
        SET TMGMENU(3)="Edit database to allow storage of '"_TMGINVAL_"'"_$CHAR(9)_"FIXIXRM"
        SET TMGUSERINPUT=$$MENU^TMGUSRI2(.TMGMENU,"^")
        KILL TMGMENU ;"Prevent from cluttering variable table during debug run
        SET %=2
        IF TMGUSERINPUT="REPLACE" DO SUMREPL(+IEN22720,+IEN64,TMGINVAL,INDENTN) GOTO SMP0
        IF TMGUSERINPUT="CODE" DO SUMCODE(+IEN22720,+IEN64,TMGINVAL,TMGNAME,TMGWKLD) GOTO SMP0
        IF TMGUSERINPUT="FIXIXRM" DO  GOTO SMPDN:(%=1),SMP0
        . DO FIXIXRM(+IEN63D04,TMGINVAL,TMGNAME,INDENTN+2)
        . ;"WRITE "Done with fix" DO YN^DICN WRITE !,!
        IF TMGUSERINPUT="^" GOTO SMPDN
        GOTO SMP0        
SMPDN   QUIT  ;
        ;
FIXIXRM(IEN63D04,TMGVALUE,TMGNAME,INDENTN) ;"FIX INPUT TRANSFORM
        ;"Purpose: to allow user to alter the input transform for the storage field, 
        ;"         such that it can store TMGVALUE
        NEW %
        SET TMGNAME=$GET(TMGNAME)
        IF $GET(DUZ(0))'["@" DO  GOTO FXRDN
        . WRITE "Sorry.  You DO not have authority to modify database for ",TMGNAME,!
        . DO PRESS2GO^TMGUSRI2
        SET IEN63D04=+$GET(IEN63D04) IF IEN63D04'>0 DO  GOTO FXRDN
        . WRITE "No record specified in file #63.04 matching for '",TMGNAME,"' for function FIXIXRM^TMGHL7S.  Aborting",!
        . DO PRESS2GO^TMGUSRI2
        SET TMGVALUE=$GET(TMGVALUE) IF TMGVALUE="" DO  GOTO FXRDN
        . WRITE "No abnormal valued pased record to function FIXIXRM^TMGHL7S.  Aborting",!
        . DO PRESS2GO^TMGUSRI2
        NEW ISNUMERIC SET ISNUMERIC=$$ISNUM^TMGSTUT3(TMGVALUE)
        NEW ZNODE SET ZNODE=$GET(^DD(63.04,IEN63D04,0))
        NEW IXFRM SET IXFRM=$PIECE(ZNODE,"^",5,99)
        NEW IXFRMHELP SET IXFRMHELP=$GET(^DD(63.04,IEN63D04,3))
        NEW CANAUTO SET CANAUTO=(IXFRM["S Q9=")&(IXFRM["D ^LRNUM")&ISNUMERIC
        IF 'CANAUTO GOTO FXX0
        SET %=1 WRITE "Try to automatically change allowed number of digits",!
        WRITE "of number (YES to autofix, NO to edit manually)" DO YN^DICN WRITE !
        GOTO FXRDN:(%=-1),FXX0:(%=2)
        IF +$$AUTOFXRM(.IXFRM,.IXFRMHELP,.TMGVALUE)=-1 GOTO FXRDN ;"Error state
        GOTO FXXOUT
FXX0    SET CANAUTO=(IXFRM["K:$L(X)>")&(IXFRM["$L(X)<")
        IF 'CANAUTO GOTO FXX1
        SET %=1 WRITE "Try to automatically change allowed string length for storage",!
        WRITE "of this value (YES to autofix, NO to edit manually)" DO YN^DICN WRITE !
        GOTO FXRDN:(%=-1),FXX1:(%=2)
        IF +$$AUTOSFXR(.IXFRM,.IXFRMHELP,.TMGVALUE)=-1 GOTO FXRDN ;"Error state
        GOTO FXXOUT
FXX1    SET CANAUTO=($PIECE(ZNODE,"^",2)["S")
        IF 'CANAUTO GOTO FXX2
        SET %=1 WRITE "Try to fix database to allow a different value, when only certain",!
        WRITE "members of a SET are allowed (YES to autofix, NO to edit manually)" DO YN^DICN WRITE !
        GOTO FXRDN:(%=-1),FXX2:(%=2)
        NEW SETSTR SET SETSTR=$PIECE(ZNODE,"^",3)
        IF +$$AUTOFXST(.SETSTR,IEN63D04,TMGVALUE,.INDENTN)=-1 GOTO FXRDN ;"Error state
        SET $PIECE(^DD(63.04,IEN63D04,0),"^",3)=SETSTR
        GOTO FXRDN
FXX2    ;"Here I can allow arbitrary editing of input tranform. 
        ;"WRITE "Sorry, can't yet handle this input transform.",!
        ;"WRITE "This can be implemented in FIXIXRM^TMGHL7S",!
        SET %=1 WRITE "Manually edit database definition to allow storage" DO YN^DICN WRITE !
        IF %'=1 GOTO FXRDN
        NEW DDCODES SET DDCODES=$PIECE($GET(^DD(63.04,IEN63D04,0)),"^",2)
        IF DDCODES["X" DO  ;"X code prevents Fileman tool from working
        . SET DDCODES=$TRANSLATE(DDCODES,"X","")
        . SET $PIECE(^DD(63.04,IEN63D04,0),"^",2)=DDCODES
        WRITE !
        WRITE INDENTSTR,"When prompted for FILE, enter '63'",!
        WRITE INDENTSTR,"When prompted for FIELD, enter '4'",!
        WRITE INDENTSTR,"When prompted for Sub-FIELD, enter '"_IEN63D04_"'",!
        WRITE INDENTSTR,"(For further instructions on usage, see Fileman documentation",!
        DO ^DICATT WRITE #
        WRITE !,"Done with editing database.",!,"Now try processing again,...",!        
        DO PRESS2GO^TMGUSRI2
        GOTO FXRDN
        ;"Store input transform and help message back into data dictionary
FXXOUT  NEW TEMP SET TEMP=$PIECE($GET(^DD(63.04,IEN63D04,0)),"^",1,4)_"^"_IXFRM
        SET ^DD(63.04,IEN63D04,0)=TEMP
        SET ^DD(63.04,IEN63D04,3)=IXFRMHELP
FXRDN   QUIT
        ;
AUTOFXST(SETSTR,IEN63D04,TMGVALUE,INDENTN) ;"AUTO (with user input) FIX ^DD for sets
        ;"Purpose: Walk user through fixing SET values in data dictionary (DD)        
        ;"Input: SETSTR -- PASS BY REFERENCE.  AN IN AND OUT PARAMETER. 
        ;"       IEN63D04 -- IEN 63.04 (CHEM TESTS), IN 63.
        ;"       TMGVALUE -- Value that caused problem.
        ;"Result: 1 means OK, -1 means error
        ;"Output: Mumps code for NEW input transform
        NEW TMGUSERINPUT,TMGMNU,TMGMNUI
        NEW TMGRESULT SET TMGRESULT=1
FXSTM0  ;
        KILL TMGUSERINPUT,TMGMNU
        KILL TMGMNUI SET TMGMNUI=0
        SET INDENTN=+$GET(INDENTN)
        NEW INDENTSTR SET INDENTSTR=$JUSTIFY("",INDENTN)
        SET TMGMNU(-1,"INDENT")=INDENTN
        SET TMGMNU(TMGMNUI)="Pick option to edit allowed SET entries.",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="Launch FILEMAN data-dictionary editor"_$CHAR(9)_"Fileman",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="MANUALLY add NEW value option to set"_$CHAR(9)_"Add2Set",TMGMNUI=TMGMNUI+1
        SET TMGMNU(TMGMNUI)="Done editing SET"_$CHAR(9)_"^",TMGMNUI=TMGMNUI+1
        ;
        SET TMGUSERINPUT=$$MENU^TMGUSRI2(.TMGMNU,"^")
        KILL TMGMNU ;"Prevent from cluttering variable table during debug run
        ;
        IF TMGUSERINPUT="Fileman" DO  GOTO FXSTM0
        . WRITE !
        . WRITE INDENTSTR,"When prompted for FILE, enter '63'",!
        . WRITE INDENTSTR,"When prompted for FIELD, enter '4'",!
        . WRITE INDENTSTR,"When prompted for Sub-FIELD, enter '"_IEN63D04_"'",!
        . WRITE INDENTSTR,"(For further instructions on usage, see Fileman documentation",!
        . DO ^DICATT WRITE #
        . NEW ZNODE SET ZNODE=$GET(^DD(63.04,IEN63D04,0))
        . SET SETSTR=$PIECE(ZNODE,"^",3)
        IF TMGUSERINPUT="Add2Set" DO   GOTO:(TMGRESULT>0) FXSTDN,FXSTM0
        . SET TMGRESULT=$$EXTENDST(.SETSTR,TMGVALUE,INDENTN+2)
        IF TMGUSERINPUT="^" GOTO FXSTDN
        IF TMGUSERINPUT=0 SET TMGUSERINPUT=""
        GOTO FXSTM0
FXSTDN  QUIT TMGRESULT
        ;
EXTENDST(SETSTR,TMGVALUE,INDENTN) ;"EXTEND (ADD) TO SET
        ;"Input: SETSTR.  this is piece 3 of the data dictionary 0 node for field.  The SET definition
        ;"       INDENTN -- OPTIONAL.  the number of spaces to indent the menu display
        ;"Result: 1 means OK, -1 means error or abort
        NEW TMGRESULT SET TMGRESULT=1
        NEW INTERNAL,EXTERNAL
        SET INDENTN=+$GET(INDENTN)
        NEW INDENTSTR SET INDENTSTR=$JUSTIFY("",INDENTN)
        WRITE !,INDENTSTR,"------------------------------------",!
        WRITE INDENTSTR,"Add SET entry definition to allow storage of '",TMGVALUE,"'",!
        WRITE INDENTSTR,"------------------------------------",!
        WRITE INDENTSTR_"Enter NEW INternal VALUE: " READ INTERNAL:$GET(DTIME,3600),!
        IF (INTERNAL="")!(INTERNAL["^") SET TMGRESULT=-1 GOTO EXTSTDN
        WRITE INDENTSTR_"Enter NEW EXternal VALUE: " READ EXTERNAL:$GET(DTIME,3600),!
        IF (EXTERNAL="")!(EXTERNAL["^") SET TMGRESULT=-1 GOTO EXTSTDN
        NEW FRAG SET FRAG=INTERNAL_":"_EXTERNAL_";"
        IF SETSTR[FRAG DO  GOTO EXTSTDN
        . WRITE INDENTSTR_"Set definition already contains '"_FRAG_"'"
        . DO PRESS2GO^TMGUSRI2
        WRITE INDENTSTR,"'",INTERNAL,"' will be stored, representing '",EXTERNAL,"'",!
        NEW % SET %=2
        WRITE INDENTSTR_"Add to SET now" DO YN^DICN WRITE !
        IF %'=1 SET TMGRESULT=-1 GOTO EXTSTDN
        SET SETSTR=SETSTR_FRAG
EXTSTDN QUIT TMGRESULT
        ;
AUTOSFXR(IXFRM,IXFRMHELP,TMGVALUE) ;"AUTO (with user input) FIX TRANSFORM for strings
        ;"Purpose: Walk user through fixing input transform
        ;"Input: IXFRM -- PASS BY REFERENCE
        ;"         expected input: K:$L(X)>12!($L(X)<1) X  , for example string 1-12 length
        ;"       IXFRMHELP -- PASS BY REFERENCE
        ;"       TMGVALUE -- Value that caused problem.
        ;"Result: 1 means OK, -1 means error
        ;"Output: Mumps code for NEW input transform
        NEW TMGRESULT SET TMGRESULT=1
        NEW LOWLEN,HILEN
        SET CHANGED=0
        SET LOWLEN=+$PIECE(IXFRM,"$L(X)<",2)
        SET HILEN=+$PIECE(IXFRM,"$L(X)>",2)
        SET STRLEN=$LENGTH(TMGVALUE)
        FOR  QUIT:(TMGRESULT=-1)!(STRLEN'<LOWLEN)  DO
        . WRITE "The lab value (",TMGVALUE,") is shorter than allowed string length (",LOWLEN,").",!
        . READ "What would you like the minimum allowed length SET to? (^ to abort) ",LOWLEN:$GET(DTIME,3600),!,!
        . IF LOWLEN="^" SET TMGRESULT=-1 QUIT
        . SET LOWLEN=+LOWLEN
        . SET CHANGED=1
        FOR  QUIT:(TMGRESULT=-1)!(STRLEN'>HILEN)  DO
        . WRITE "The lab value (",TMGVALUE,") is longer than allowed string length (",HILEN,").",!
        . WRITE "It is ",$LENGTH(TMGVALUE)," characters long.",!
        . READ "What would you like the maximum allowed length SET to? (^ to abort) ",HILEN:$GET(DTIME,3600),!,!
        . IF HILEN="^" SET TMGRESULT=-1 QUIT
        . SET HILEN=+HILEN
        . SET CHANGED=1
        IF CHANGED=1 DO
        . SET IXFRM="K:$L(X)>"_HILEN_"!($L(X)<"_LOWLEN_") X"
        . SET IXFRMHELP="ANSWER MUST BE "_LOWLEN_"-"_HILEN_" CHARACTERS IN LENGTH"
        QUIT TMGRESULT
        ;
AUTOFXRM(IXFRM,IXFRMHELP,TMGVALUE) ;"AUTO (with user input) FIX TRANSFORM for numbers
        ;"Purpose: Walk user through fixing input transform
        ;"Input: IXFRM -- PASS BY REFERENCE
        ;"       IXFRMHELP -- PASS BY REFERENCE
        ;"       TMGVALUE -- Value that caused problem.
        ;"Result: 1 means OK, -1 means error
        ;"Output: Mumps code for NEW input transform
        ;
        ;"Check low value
        NEW RANGES SET RANGES=$PIECE(IXFRM,"""",2)
        NEW LOW,HIGH,DECPLACES,DECLEN,CHANGED,RESULT
        SET RESULT=1
        SET CHANGED=0
        SET LOW=$PIECE(RANGES,",",1),HIGH=$PIECE(RANGES,",",2),DECPLACES=$PIECE(RANGES,",",3)
        SET DECLEN=$LENGTH($PIECE(TMGVALUE,".",2))
        FOR  QUIT:(RESULT=-1)!(LOW'>TMGVALUE)  DO
        . WRITE "The lab value (",TMGVALUE,") is less than the low range (",LOW,").",!
        . READ "What would you like the low range SET to? (^ to abort) ",LOW:$GET(DTIME,3600),!,!
        . IF LOW="^" SET RESULT=-1 QUIT
        . SET LOW=+LOW
        . SET CHANGED=1
        FOR  QUIT:(RESULT=-1)!(HIGH'<TMGVALUE)  DO
        . WRITE "The lab value (",TMGVALUE,") is larger than the high range (",HIGH,").",!
        . READ "What would you like the high range SET to? (^ to abort) ",HIGH:$GET(DTIME,3600),!,!
        . IF HIGH="^" SET RESULT=-1 QUIT
        . SET HIGH=+HIGH
        . SET CHANGED=1
        FOR  QUIT:(RESULT=-1)!(DECPLACES'<DECLEN)  DO
        . WRITE "The lab value '"_TMGVALUE_"' has ",DECLEN," decimal places, but only "_DECPLACES_" "_$SELECT(DECPLACES=1:"is",1:"are")_" allowed.",!
        . READ "How many decimal places would you like to SET this to? (^ to abort): ",DECPLACES:$GET(DTIME,3600),!,!
        . IF DECPLACES="^" SET RESULT=-1 QUIT
        . SET DECPLACES=+DECPLACES
        . SET CHANGED=1
        IF CHANGED=1 DO
        . SET IXFRM="S Q9="""_LOW_","_HIGH_","_DECPLACES_""" D ^LRNUM"
        . SET IXFRMHELP="TYPE A NUMBER BETWEEN "_LOW_" AND "_HIGH_" WITH "_DECPLACES_" DECIMAL PLACES"
AUTODN        ;
        QUIT RESULT
        ;
SUMREPL(IEN22720,IEN64,TMGINVAL,INDENTN) ;"SET UP REPLACEMENT VALUE 
        NEW TMGNEWVAL
        WRITE !
        SET INDENTN=+$GET(INDENTN)
        NEW INDENTSTR SET INDENTSTR=$JUSTIFY("",INDENTN)
SMP1    WRITE INDENTSTR,"Enter replacement value for '"_TMGINVAL_"': // "
        READ TMGNEWVAL:$GET(DTIME,3600)
        IF TMGNEWVAL["?" DO  GOTO SMP1
        . WRITE !
        . IF $GET(TMGHLP)'="" WRITE "Help: ",TMGHLP,! QUIT
        . WRITE INDENTSTR,"(Sorry, no help available)",! ;//<--- ADD LATER
        IF TMGNEWVAL="^" WRITE ! GOTO SMPDN
        IF TMGNEWVAL="" WRITE "??",! GOTO SMP1
        WRITE !,INDENTSTR,"For ",TMGNAME,":",!
        WRITE INDENTSTR,"    Map incoming '",TMGINVAL,"' --> '",TMGNEWVAL
        SET %=2
        DO YN^DICN WRITE !
        IF %=2 GOTO SMP1
        IF %=-1 GOTO SMPDN
        DO STORMAP(+IEN22720,+IEN64,TMGINVAL,TMGNEWVAL)
        WRITE INDENTSTR DO PRESS2GO^TMGUSRI2
        QUIT
        ;
SUMCODE(IEN22720,IEN64,TMGINVAL,TMGNAME,TMGWKLD) ;"SET UP CODE TO APPLY TO ALL VALUES
        NEW TMGCODE
        WRITE !,"Enter Mumps code to fix ALL values for "_TMGNAME_" #("_TMGWKLD_")",!
        WRITE "> " READ TMGCODE:$GET(DTIME,3600)
        IF TMGCODE["?" DO  GOTO SMP1
        . WRITE !
        . ;"IF $GET(TMGHLP)'="" WRITE "Help: ",TMGHLP,! QUIT
        . WRITE "(Sorry, no help available)",! ;//<--- ADD LATER
        IF (TMGCODE="^")!(TMGNEWVAL="") GOTO SUMCDN
        DO STORMAP(+IEN22720,+IEN64,TMGINVAL,"",TMGCODE) 
SUMCDN  QUIT
        ;
STORMAP(IEN22720,IEN64,TMGINVAL,TMGNEWVAL,TMGCODE) ;
        ;"Purpose: To store a mapping of TMGINVAL --> TMGNEWVAL
        ;"         to acceptable values that Fileman can store.
        ;"Input: IEN22720 -- The IEN of the record to store mapping in
        ;"       IEN64 -- IEN in file 64 corresponding to the WKLD CODE that identifies the test
        ;"       TMGINVAL -- the abherent value that would be need to be fixed.
        ;"       TMGNEWVAL -- the NEW value that will be used to replace TMGINVAL
        ;"       TMGCODE -- [OPTIONAL]. NOTE: IF provided, then TMGNEWVAL is ignored.
        ;"              This is mumps code used to store into field 2 (CODE FOR MODIFICATION)
        ;"Output: Data will be stored in database.
        ;"Results: none
        NEW IEN22720D01 SET IEN22720D01=+$ORDER(^TMG(22720,IEN22720,20,"B",+IEN64,0))
        IF IEN22720D01>0 GOTO STMP2
        NEW TMGFDA,TMGIEN,TMGMSG
        SET TMGFDA(22720.01,"+1,"_IEN22720_",",.01)=+IEN64
        DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO STMPDN
        . WRITE $$GETERRST^TMGDEBU2(.TMGMSG)
        SET IEN22720D01=+$GET(TMGIEN(1))
        IF IEN22720D01'>0 DO  QUIT
        . WRITE "Unable to locate added subrecord in 22720.01"
STMP2   SET TMGCODE=$GET(TMGCODE) 
        IF TMGCODE="" GOTO STMP3
        SET TMGFDA(22720.01,IEN22720D01_","_IEN22720_",",2)=TMGCODE
        DO FILE^DIE("K","TMGFDA","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) WRITE $$GETERRST^TMGDEBU2(.TMGMSG)
        GOTO STMPDN
STMP3   NEW IEN22720D11 SET IEN22720D11=+$ORDER(^TMG(22720,IEN22720,20,IEN22720D01,1,"B",TMGINVAL,0))
        IF IEN22720D11>0 DO  GOTO STMPDN
        . NEW DIE SET DIE="^TMG(22720,"_IEN22720_",20,"_IEN22720D01_",1,"
        . NEW DA SET DA=IEN22720D11
        . SET DR="1"
        . DO ^DIE
        KILL TMGFDA,TMGIEN,TMGMSG
        SET TMGFDA(22720.11,"+1,"_IEN22720D01_","_IEN22720_",",.01)=TMGINVAL
        DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO STMPDN
        . WRITE $$GETERRST^TMGDEBU2(.TMGMSG)
        SET IEN22720D11=+$GET(TMGIEN(1))
        KILL TMGFDA,TMGIEN,TMGMSG
        SET TMGFDA(22720.11,IEN22720D11_","_IEN22720D01_","_IEN22720_",",1)=TMGNEWVAL
        DO FILE^DIE("K","TMGFDA","TMGMSG")
STMP4   IF $DATA(TMGMSG("DIERR")) DO  GOTO STMPDN
        . WRITE $$GETERRST^TMGDEBU2(.TMGMSG)
STMPDN  QUIT
        ;
SUMFRERR(IEN22720,TMGRESULT,INDENTN) ;"Set up Map from Error Msg
        ;"Purpose: To take error result and use to ask about mapping.
        ;"Input: IEN22720 -- The IEN of the record to store mapping in
        ;"       TMGRESULT -- The error message.
        ;"        e.g. -1^[INVALID-VAL] Lab value: {NEGATIVE} not acceptable for storage in database for test {8350.000}.  Fileman Help Prompt: 'Number 1-10'
        ;"       INDENTN -- OPTIONAL.  the number of spaces to indent the menu display
        ;"Output: Data will be stored in database.
        ;"Results: none
        SET TMGRESULT=$GET(TMGRESULT)
        IF TMGRESULT'["[INVALID-VAL]" DO  GOTO SUFEDN
        . WRITE !,"Sorry.  Unable to automatically handle this error.",!,!
        . IF TMGRESULT'["Fileman says:" QUIT
        . WRITE "For some reason, routine VALIDVAL^TMGHL72 failed to flag",!
        . WRITE "this invalid value.  Please ask programming team to fix.",!
        NEW TMGINVAL SET TMGINVAL=$PIECE($PIECE(TMGRESULT,"{",2),"}",1)
        NEW TMGWKLD SET TMGWKLD=$$TRIM^XLFSTR($PIECE($PIECE(TMGRESULT,"{WKLD CODE:",2),"}",1))
        NEW IEN64 SET IEN64=$$TRIM^XLFSTR($PIECE($PIECE(TMGRESULT,"{IEN64:",2),"}",1))
        NEW IEN60 SET IEN60=$$TRIM^XLFSTR($PIECE($PIECE(TMGRESULT,"{IEN60:",2),"}",1))
        NEW IEN63D04 SET IEN63D04=$$TRIM^XLFSTR($PIECE($PIECE(TMGRESULT,"{IEN63.04:",2),"}",1))
        NEW TMGHLP SET TMGHLP=$PIECE(TMGRESULT,"[HELP]:",2)        
        DO SUMAP(IEN22720,TMGWKLD,IEN64,IEN63D04,TMGINVAL,TMGHLP,.INDENTN) ;
SUFEDN  QUIT
        ;
TESTXFRM(TMGENV,TMGTESTMSG,TMGHL7MSG,TMGU,INDENTN) ;
        ;"Purpose: To show result of transform        
        NEW SHOWARRAY,TMGRESULT        
        SET TMGRESULT=$$SETUPENV^TMGHL7U(.TMGTESTMSG,.TMGENV,0)
        IF TMGRESULT<0 GOTO TXFMDN        
        NEW IEN227270 SET IEN22720=TMGENV("IEN 22720")
        SET TMGRESULT=$$PRSEARRY^TMGHL7X2(IEN22720,.TMGTESTMSG,.TMGHL7MSG) ;
        IF TMGRESULT<0 GOTO TXFMDN
        SET TMGRESULT=$$DOMORE^TMGHL7X2(.TMGENV,.TMGHL7MSG)
        IF TMGRESULT<0 GOTO TXFMDN
        SET TMGRESULT=$$XFMSG^TMGHL7X(.TMGENV,.TMGHL7MSG)
        IF TMGRESULT<0 GOTO TXFMDN
        SET TMGRESULT=$$TESTCOMP^TMGHL7X2(.TMGHL7MSG,.SHOWARRAY)
        IF TMGRESULT<0 GOTO TXFMDN
        NEW SHOWN SET SHOWN=0
        NEW I SET I=0
        FOR  SET I=$ORDER(SHOWARRAY(I)) QUIT:I=""  DO
        . WRITE SHOWARRAY(I),!
        . SET SHOWN=1
        IF SHOWN=0 DO
        . WRITE "<Nothing to show>",!
        DO PRESS2GO^TMGUSRI2
TXFMDN  IF TMGRESULT["[INVALID-VAL]" DO
        . WRITE !,"Some test result values were invalid.",!
        . WRITE "Work on mapping to valid results to fix this issue"
        . NEW % SET %=1
        . DO YN^DICN WRITE !
        . IF %'=1 QUIT
        . DO SUMFRERR(IEN22720,TMGRESULT,.INDENTN)
        . SET TMGRESULT=1
        . WRITE "This just addressed the 1st issue.  It is recommended that",!
        . WRITE "one keep selecting 'Test the HL7 message' until all",!
        . WRITE "problems are resolved.",!
        . DO PRESS2GO^TMGUSRI2
        IF TMGRESULT<0 DO
        . WRITE $PIECE(TMGRESULT,"^",2,99),!
        . DO PRESS2GO^TMGUSRI2
        IF +TMGRESULT>0 DO
        . WRITE "No problems found.",!
        . DO PRESS2GO^TMGUSRI2
        QUIT
