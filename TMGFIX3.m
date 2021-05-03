TMGFIX3 ;TMG/kst/Fixes for meaningful use ;2/2/14, 5/18/18, 3/24/21
         ;;1.0;TMG-LIB;**1**;8/17/12
 ;
 ;"FIXES related to TMG C0Q FUNCTIONS
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"NOTE: 1) Code to fix TOBACCO table problems.
 ;"      2) Converting Tables to REMINDER entries. 
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"DOSCAN -- Fix patient's TOBACCO entry in the text table.
 ;"PICKFIX -- pick all entries to SET to YES or NO, and fix them.
 ;"DOSCAN2  -- Convert various table entries into corresponding reminder entries
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"GETHDR(TEXT) --LOAD UP HEADER ARRAY
 ;"MAKENOTE(VALUE,YN,TMGDFN,TXTARRAY) -- compose a note with information for NEW table.
 ;"SAVENOTE(TMGDFN,TEXT) -- save TEXT array as a NEW entry in file 8925
 ;
 ;"DOASCAN(TABLE,ELEMENT,FOUNDVPT,REFUSEDVPT) -- cycle through patients, get a table entry and create corresponding reminder entry  
 ;"TAB2REM(TMGDFN,TABLE,ELEMENT,DATAREF) -- Get table value for given patient. 
 ;"SEL2ARR(DATAREF,ELEMENT,MESSAGE,OUTREF) -- Allow user to select entries that match desired action    
 ;"STR2DATES(STR,OUT) -- Convert string containing dates and other text, into array of FM dates
 ;"ISDATE(WORD,OUTFMDT) -- IF word is a date, and IF so, turn into FM date
 ; 
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"=======================================================================
 ;"   C0QMU12, TMGSTUT2, TMGUSRI2, TMGMISC2, TMGC0Q*, TMGPXRU1
 ;"=======================================================================
 ;
DOSCAN  ;
        ;"Purpose: Function to cycle through every user, and get the results of their
        ;"         Tobacco table entry.  Thus I can look for patterns to learn
        ;"         to interpret it's use.
        KILL ^TMG("TMP","TOBACCO")
        NEW STIME SET STIME=$H
        NEW DONE SET DONE=0
        NEW DFNMAX SET DFNMAX=$ORDER(^DPT("!"),-1)
        NEW TMGDFN SET TMGDFN=0
        FOR  SET TMGDFN=$ORDER(^DPT(TMGDFN)) QUIT:(+TMGDFN'>0)  DO
        . IF TMGDFN#5=0 DO
        . . DO PROGBAR^TMGUSRI2(TMGDFN,"PROGRESS: "_TMGDFN,0,DFNMAX,60,STIME)
        . . IF $$USRABORT^TMGUSRI2("scanning patients") SET DONE=1
        . NEW S
        . ;"doesn't exist, fix later --> IF $$TMGSMOKING^TMGC0Q02(TMGDFN,.S) ;"Ignore result.  Get back S
        . IF S="" QUIT
        . IF S="<NO DATA>" QUIT
        . SET ^TMG("TMP","TOBACCO",TMGDFN)=S
        QUIT
        ;

HEADER  ;
        ;;"  Standardizing notation of tobacco use.
        ;;"  Patient's *prior* use notation reformatted
        ;;"  Patient did NOT provide NEW information for this.
        ;;"
        ;;"<END>
        ;
GETHDR(TEXT,LABEL) ;"LOAD UP HEADER ARRAY
        ;"Results: TEXT is loaded as follows:
        ;"     TEXT(#,0)=<line text>
        NEW DONE SET DONE=0
        KILL TEXT
        NEW CT SET CT=1
        NEW I FOR I=1:1 DO  QUIT:DONE
        . NEW S SET S=$TEXT(@LABEL+I)
        . IF S="" SET DONE=1 QUIT
        . SET S=$PIECE(S,";;""",2)
        . IF S="<END>" SET DONE=1 QUIT
        . SET TEXT(CT,0)=S
        . SET CT=CT+1
        QUIT
        ;
PICKFIX ;
        ;"Purpose: pick all entries to SET to YES or NO, and fix them.
        NEW TITLE,%,MENU,TMGDFN,TMGPICK,TMGRESULT,REPLY,XREFDFN
        WRITE !,!,"TOBACCO USE FIXER.",!
M1      KILL MENU
        SET MENU(0)="Pick type to browse for"
        SET MENU(1)="Select responses that mean YES tobacco use"_$CHAR(9)_"YES"
        SET MENU(2)="Select responses that mean NO tobacco use"_$CHAR(9)_"NO"
        SET MENU(3)="Rescan all patients"_$CHAR(9)_"RESCAN"
        SET MENU(4)="QUIT"_$CHAR(9)_"^"
        NEW YN SET YN=$$MENU^TMGUSRI2(.MENU)
        IF YN="^" GOTO PSDN
        IF YN="RESCAN" DO DOSCAN GOTO M1
PS1     WRITE !,"Prepairing...."
        ;"KILL XREFDFN
        KILL TMGPICK
        SET TMGDFN=0
        FOR  SET TMGDFN=$ORDER(^TMG("TMP","TOBACCO",TMGDFN)) QUIT:(+TMGDFN'>0)  DO
        . SET REPLY=$GET(^TMG("TMP","TOBACCO",TMGDFN))
        . IF REPLY="" QUIT
        . IF REPLY="<NO DATA>" QUIT
        . IF $EXTRACT(REPLY,1,2)="NO" QUIT
        . IF $EXTRACT(REPLY,1,3)="YES" QUIT
        . SET TMGPICK(REPLY_"^ "_$$GET1^DIQ(2,TMGDFN,.01))=TMGDFN
        . ;"SET XREFDFN(TMGDFN)=REPLY
        KILL TMGRESULT
        DO SELECTR2^TMGUSRI3("TMGPICK","TMGRESULT","Pick replies that mean "_YN_" tobacco use. <Esc><Esc> when done.")
        SET REPLY=""
        FOR  SET REPLY=$ORDER(TMGRESULT(REPLY)) QUIT:REPLY=""  DO
        . NEW TMGDFN SET TMGDFN=+$GET(TMGRESULT(REPLY))
        . IF TMGDFN'>0 QUIT
        . NEW TEXT,VALUE
        . SET VALUE=$PIECE(REPLY,"^",1)
        . DO MAKENOTE(VALUE,YN,TMGDFN,.TEXT)
        . NEW DIDSAVE SET DIDSAVE=$$SAVENOTE(TMGDFN,.TEXT)
        . IF DIDSAVE DO
        . . KILL ^TMG("TMP","TOBACCO",TMGDFN)
        GOTO M1
PSDN    QUIT
        ;
MAKENOTE(VALUE,YN,TMGDFN,TXTARRAY)  ;
        ;"Purpose: to compose a note with information for NEW table.
        ;"Input: VALUE -- PASS BY REFERENCE, to get NEW value back out.
        KILL TXTARRAY
        DO GETHDR(.TXTARRAY,"HEADER")
        SET VALUE=YN_". "_VALUE
        NEW S SET S=$$SETTABL(TMGDFN,"SOCIAL HX","TOBACCO",VALUE)
        NEW CT SET CT=+$ORDER(TXTARRAY(""),-1)+1
        NEW ARR DO SPLIT2AR^TMGSTUT2(S,$CHAR(13)_$CHAR(10),.ARR,CT)
        SET CT=0 FOR  SET CT=$ORDER(ARR(CT)) QUIT:+CT'>0  DO
        . SET TXTARRAY(CT,0)=$GET(ARR(CT))
        NEW CT SET CT=+$ORDER(TXTARRAY(""),-1)+1
        NEW I FOR I=1:1:3 DO
        . SET CT=CT+1
        . SET TXTARRAY(CT,0)=" "
MNDN    QUIT
        ;
SETTABL(TMGDFN,LABEL,KEY,VALUE) ;
    ;"Purpose: to get a table, just like GETTABL1, but then SET KEY=VALUE in the table
    ;"NOTE: not currently designed to handle MEDICATIONS table.
    NEW RESULT SET RESULT=""
    NEW ARRAY
    IF $GET(LABEL)="" GOTO STDN
    NEW SPACES SET SPACES=""
    DO GETSPECL^TMGTIUO4(TMGDFN,LABEL,"BLANK_LINE",48,.ARRAY,1,.SPACES)  ;"mode 1 = only last table; 2=compile
    SET RESULT=SPACES_"-- "_LABEL_" ---------"_$CHAR(13)_$CHAR(10)
    DO STUBRECS^TMGTIIUO6(.TMGDFN,.ARRAY,LABEL)
    IF $DATA(ARRAY("KEY-VALUE",$$UP^XLFSTR(KEY))) DO
    . NEW TS SET TS=$GET(ARRAY("KEY-VALUE",$$UP^XLFSTR(KEY),"LINE"))
    . SET $PIECE(TS,": ",2,99)=VALUE
    . SET ARRAY("KEY-VALUE",$$UP^XLFSTR(KEY),"LINE")=TS
    ELSE  DO
    . SET ARRAY("KEY-VALUE",$$UP^XLFSTR(KEY))=VALUE
    . SET ARRAY("KEY-VALUE",$$UP^XLFSTR(KEY),"LINE")=SPACES_" "_VALUE
    SET RESULT=RESULT_$$ARRAY2ST^TMGTIUO4(.ARRAY,.SPACES)
STDN    QUIT RESULT
    ;        
SAVENOTE(TMGDFN,TEXT,FORCE) ;
        ;"Purpose: save TEXT array as a NEW entry in file 8925
        ;"Result: 1 IF note saved, 0 IF not, -1 IF aborted
        NEW RESULT SET RESULT=0
        WRITE #
        WRITE !,$PIECE($GET(^DPT(TMGDFN,0)),"^",1)," (",$$GET1^DIQ(2,TMGDFN,.03,"E"),")",!
        WRITE "----------------------------",!
        DO ZWRITE^TMGZWR("TEXT")
        NEW % SET %=2
        IF $GET(FORCE)=1 SET %=1
        ELSE  WRITE !,"CREATE NOTE AS ABOVE" DO YN^DICN WRITE !
        IF %=-1 SET RESULT=-1 GOTO SVDN
        IF %=2 SET RESULT=0 GOTO SVDN
        ;"SAVE NOTE HERE....
        NEW TMGFDA,TMGMSG,TMGIEN
        ;"*** FIX -- next time, note needs a HOSPITAL LOCATION value
        SET TMGFDA(8925,"+1,",.01)="`1411"  ;"Hard coded 'NOTE' title.
        SET TMGFDA(8925,"+1,",.02)="`"_TMGDFN
        SET TMGFDA(8925,"+1,",.05)="COMPLETED"  ;`7"  ;"Hard coded COMPLETED status
        SET TMGFDA(8925,"+1,",.07)="NOW"
        SET TMGFDA(8925,"+1,",1202)="`"_DUZ
        SET TMGFDA(8925,"+1,",1204)="`"_DUZ
        SET TMGFDA(8925,"+1,",1205)="Laughlin_Office"
        SET TMGFDA(8925,"+1,",1502)="`"_DUZ
        SET TMGFDA(8925,"+1,",1504)=$$GET1^DIQ(200,DUZ,.01)
        SET TMGFDA(8925,"+1,",1301)="NOW"
        SET TMGFDA(8925,"+1,",1303)="remote procedure"
        SET TMGFDA(8925,"+1,",1501)="NOW"
        SET TMGFDA(8925,"+1,",1505)="electronic"
        DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO SVDN
        . SET RESULT=-1
        . DO SHOWERR^TMGDEBU2(,.TMGMSG)
        NEW IEN SET IEN=+$GET(TMGIEN(1))
        IF IEN'>0 DO  GOTO SVDN
        . WRITE "ERROR: can't find IEN of added record",!
        . SET RESULT=-1
        DO WP^DIE(8925,IEN_",",2,"","TEXT","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO SVDN
        . SET RESULT=-1
        . DO SHOWERR^TMGDEBU2(,.TMGMSG)
        SET RESULT=1
SVDN    QUIT RESULT
        ;
FIXNOTE  ;"MAKE NOTE function above didn't have table formatted properly.
        ;"  will change '-- SOCIAL HX ---------' TO '-- [SOCIAL HX] ---------'
        ;"I made a SORT template with all the notes that need to be changed.
        NEW TIUIEN SET TIUIEN=0
        FOR  SET TIUIEN=$ORDER(^DIBT(925,1,TIUIEN)) QUIT:+TIUIEN'>0  DO
        . NEW LINE SET LINE=0
        . FOR  SET LINE=$ORDER(^TIU(8925,TIUIEN,"TEXT",LINE)) QUIT:+LINE'>0  DO
        . . NEW STR SET STR=$GET(^TIU(8925,TIUIEN,"TEXT",LINE,0)) QUIT:STR=""
        . . IF STR'["-- SOCIAL HX ---------" QUIT
        . . SET STR=$$REPLSTR^TMGSTUT3(STR,"SOCIAL HX","[SOCIAL HX]")
        . . SET ^TIU(8925,TIUIEN,"TEXT",LINE,0)=STR
        . . WRITE "#"
        WRITE !
        QUIT
        ;
        ;"========================================================================
        ;"========================================================================
        ;"========================================================================
FIXPNEUM  ;"FIX AND STANDARDIZE PNEUMOVAX ENTRIES
        NEW %ZIS
        SET %ZIS("A")="Enter Output Device: "
        SET %ZIS("B")="HOME"
        DO ^%ZIS  ;"standard device call
        IF POP DO  QUIT
        . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output.  Aborting.")
        USE IO
        ;"Do the output
        NEW COL2 SET COL2=30
        NEW COL3 SET COL3=60
        NEW SDTE,EDTE
        NEW PTNAME 
        NEW CT SET CT=0
        WRITE !,!,"PROPOSED CHANGES TO PNEUMOVAX ENTRY",!
        WRITE "PATIENT NAME",?COL2,"PROPOSED CHANGE",?COL3,"PRIOR DATA",!
        WRITE "------------",?COL2,"---------------",?COL3,"----------",!
        NEW TMGDFN SET TMGDFN=0
        FOR  SET TMGDFN=$ORDER(^DPT(TMGDFN)) QUIT:(+TMGDFN'>0)  DO
        . SET PTNAME=$PIECE(^DPT(TMGDFN,0),"^",1)
        . SET CT=CT+1
        . NEW ARRAY
        . SET S=$$GETTABLX^TMGTIUO6(TMGDFN,"STUDIES",.ARRAY)
        . SET S=$GET(ARRAY("KEY-VALUE","PNEUMOVAX"))
        . SET S=$$UP^XLFSTR($$TRIM^XLFSTR(S))
        . IF S="<NO DATA>" SET S=""
        . IF S="." SET S=""
        . IF S="" QUIT
        . IF S'["ORDERED" QUIT
        . NEW ARR,MSG
        . DO PARSEPVX^TMGTIUO7(S,.ARR,.MSG)
        . WRITE "#",TMGDFN," ",PTNAME," ",?COL2
        . IF $DATA(MSG) WRITE !,MSG,!
        . ;IF $DATA(ARR) DO ZWRITE^TMGZWR("ARR")
        . NEW NEWSTR SET NEWSTR=""
        . NEW FMDATE SET FMDATE=""
        . NEW REACTION SET REACTION=""
        . NEW FIRST SET FIRST=1
        . NEW MODE SET MODE=""
        . ;"FOLLOWING BLOCK IS TEMPORARY!!
        . FOR  SET FMDATE=$ORDER(ARR(FMDATE),-1) QUIT:(FMDATE="")  DO
        . . IF FMDATE="REACTION" QUIT
        . . NEW TMEPMODE SET TEMPMODE=""
        . . FOR  SET TEMPMODE=$ORDER(ARR(FMDATE,TEMPMODE)) QUIT:TEMPMODE=""  DO
        . . . IF TEMPMODE="O" DO
        . . . . SET ARR(FMDATE,"Y")=$GET(ARR(FMDATE,TEMPMODE))
        . . . . KILL ARR(FMDATE,TEMPMODE)        
        . SET FMDATE="",MODE=""
        . FOR  SET FMDATE=$ORDER(ARR(FMDATE),-1) QUIT:(FMDATE="")  DO
        . . IF FMDATE="REACTION" DO  QUIT
        . . . NEW TEMPS SET TEMPS=$GET(ARR(FMDATE)) QUIT:TEMPS=""
        . . . SET REACTION=TEMPS
        . . NEW TMEPMODE SET TEMPMODE=""
        . . FOR  SET TEMPMODE=$ORDER(ARR(FMDATE,TEMPMODE)) QUIT:TEMPMODE=""  DO
        . . . IF TEMPMODE'=MODE DO
        . . . . SET MODE=TEMPMODE
        . . . . SET MODESTR="??"
        . . . . IF MODE="Y" SET MODESTR="Given:" 
        . . . . IF MODE="O" SET MODESTR="Ordered:" 
        . . . . IF MODE="D" SET MODESTR="Declined:" 
        . . . . IF MODE="S" SET MODESTR="Script:" 
        . . . . IF MODE="R" SET MODESTR="Refused:"
        . . . . IF NEWSTR'="" SET NEWSTR=NEWSTR_"; "
        . . . . SET NEWSTR=NEWSTR_MODESTR
        . . . IF $EXTRACT(NEWSTR,$LENGTH(NEWSTR))=":" SET NEWSTR=NEWSTR_" "
        . . . ELSE  SET NEWSTR=NEWSTR_"; "
        . . . NEW DATESTR SET DATESTR=$$FMTE^XLFDT(FMDATE,"2D")
        . . . IF +$PIECE(DATESTR,"/",2)=0 SET DATESTR=$PIECE(DATESTR,"/",1)_"/"_$PIECE(DATESTR,"/",3)
        . . . IF +$PIECE(DATESTR,"/",1)=0 DO
        . . . . SET DATESTR=$$FMTE^XLFDT(FMDATE,"5D")
        . . . . SET DATESTR="~"_$PIECE(DATESTR,"/",$LENGTH(DATESTR,"/"))
        . . . SET NEWSTR=NEWSTR_DATESTR
        . . . IF MODE="Y",FIRST=1 DO
        . . . . NEW PTAGE SET PTAGE=$$AGEONDAT^TMGTIUO3(TMGDFN,FMDATE)
        . . . . SET NEWSTR=NEWSTR_" ("_PTAGE_"y)"
        . . . . ;"SET FIRST=0  ;"remove comment to only add year to first entry
        . . . IF REACTION'="" SET NEWSTR=NEWSTR_" REACTION: "_REACTION
        . WRITE NEWSTR,?COL3
        . IF $X>COL3 WRITE !,?COL3
        . WRITE S,!
        . NEW TXTARRAY
        . DO MKNOTE2(NEWSTR,TMGDFN,.TXTARRAY)  ;
        . NEW DIDSAVE SET DIDSAVE=$$SAVENOTE(TMGDFN,.TXTARRAY,1)
        ;" Close the output device
        DO ^%ZISC
        QUIT
        ;        
MKNOTE2(VALUE,TMGDFN,TXTARRAY)  ;
        ;"Purpose: to compose a note with information for NEW table.
        ;"Input: VALUE -- PASS BY REFERENCE, to get NEW value back out.
        KILL TXTARRAY
        DO GETHDR(.TXTARRAY,"HDR3")
        NEW S SET S=$$SETTABL(TMGDFN,"[STUDIES]","Pneumovax",VALUE)
        NEW CT SET CT=+$ORDER(TXTARRAY(""),-1)+1
        NEW ARR DO SPLIT2AR^TMGSTUT2(S,$CHAR(13)_$CHAR(10),.ARR,CT)
        SET CT=0 FOR  SET CT=$ORDER(ARR(CT)) QUIT:+CT'>0  DO
        . SET TXTARRAY(CT,0)=$GET(ARR(CT))
        NEW CT SET CT=+$ORDER(TXTARRAY(""),-1)+1
        NEW I FOR I=1:1:3 DO
        . SET CT=CT+1
        . SET TXTARRAY(CT,0)=" "
        QUIT
        ;
HDR2    ;
        ;;"  Standardizing notation of pneumococcal use.
        ;;"  Patient's *prior* use notation reformatted
        ;;"  Patient did NOT provide NEW information for this.
        ;;"
        ;;"<END>
        ;
HDR3    ;
        ;;"  Updating pneumococcal status after review of 
        ;;"  patient paper chart for records of doses given. 
        ;;"
        ;;"<END>
        ;
 ;"============================================================
 ;"============================================================
 ;"============================================================
 ;"============================================================
DOSCAN2  ;
        ;"Purpose: Function to cycle through every user, and get the results of their
        ;"         _____ table entry and create reminder entries that corresponds.
        NEW RESULT SET RESULT="1^Nothing done."
        ;"SET RESULT=$$DOASCAN("STUDIES","ZOSTAVAX",,"IM.TMG ZOSTAVAX","HF.TMG REFUSED Zostavax")
        ;"SET RESULT=$$DOASCAN("STUDIES","PNEUMOVAX",,"IM.PNEUMOVAX","HF.TMG REFUSED Pneumococcal")
        ;"SET RESULT=$$DOASCAN("STUDIES","TD","TdaP","IM.TETANUS, DIPHTHERIA, ACELLULAR PERTUSSIS","HF.TMG REFUSED TdaP")
        ;"SET RESULT=$$DOASCAN("STUDIES","TD","Td","IM.TETANUS DIPTHERIA (TD-ADULT)","HF.TMG REFUSED Td")
        ;"SET RESULT=$$DOASCAN("STUDIES","TDAP / TD","TdaP","IM.TETANUS, DIPHTHERIA, ACELLULAR PERTUSSIS","HF.TMG REFUSED TdaP")
        ;"SET RESULT=$$DOASCAN("STUDIES","TDAP / TD","Td","IM.TETANUS DIPTHERIA (TD-ADULT)","HF.TMG REFUSED Td")
        ;"SET RESULT=$$DOASCAN("STUDIES","TDAP","TdaP","IM.TETANUS, DIPHTHERIA, ACELLULAR PERTUSSIS","HF.TMG REFUSED TdaP")
        ;"SET RESULT=$$DOASCAN("STUDIES","TDAP","Td","IM.TETANUS DIPTHERIA (TD-ADULT)","HF.TMG REFUSED Td")
        ;"SET RESULT=$$DOASCAN("STUDIES","TD","Td","IM.TETANUS DIPTHERIA (TD-ADULT)","HF.TMG REFUSED Td")
        ;"SET RESULT=$$DOASCAN("STUDIES","HEPATITIS C SCREEN",,"HF.TMG HEP C AB SCREEN","HF.TMG REFUSED HEP C AB SCREEN")
        ;"SET RESULT=$$DOASCAN("STUDIES","COLONOSCOPY",,"HF.TMG COLONOSCOPY COMPLETED","HF.TMG COLONOSCOPY REFUSED")
        ;"SET RESULT=$$DOASCAN("STUDIES","GLAUCOMA EYE EXAM",,"HF.TMG GLAUCOMA SCREEN DONE","HF.TMG GLAUCOMA SCREEN REFUSED")
        SET RESULT=$$DOASCAN("STUDIES","MAMMOGRAM",,"HF.TMG MAMMOGRAM/IMAGING DONE","HF.TMG REFUSED MAMMOGRAM")
        WRITE !,$PIECE(RESULT,"^",2)
        WRITE !,"Goodbye.",!
        QUIT
;        
DOASCAN(TABLE,ELEMENT,KEYTERM,FOUNDVPT,REFUSEDVPT)  ;
        ;"Purpose: Function to cycle through every user, and get the results of their
        ;"         specified table entry and create reminder entries that correspond.
        ;"Input: TABLE -- Name of table, E.g. 'STUDIES'
        ;"       ELEMENT -- Name of line in table to examine. -- E.g. Colonoscopy
        ;"       KEYTERM -- Optional.  If present, a key term to look for on line. 
        ;"       FOUNDVPT -- Factor (etc) indicating patient has had test done.  E.g. IM.PNEUMOVAX
        ;"       REFUSEDVPT -- Factor (etc) indicating patient has REFUSED test.  E.g. HF.TMG REFUSED Pneumococcal
        ;"Result : 1^Success, or -1^Message IF error.
        NEW TMGRESULT SET TMGRESULT="1^Success"
        SET ELEMENT=$$UP^XLFSTR(ELEMENT)
        SET KEYTERM=$GET(KEYTERM)
        NEW TMGDFN SET TMGDFN=0
        WRITE !,"Text Tables --> Reminders Scanner.",!,!
        WRITE "Scanning table '",TABLE,"' for element '",ELEMENT,"'.",!
        NEW % SET %=2
        NEW ROOTREF SET ROOTREF=$NAME(^TMG("TMP","TABLE->REMINDERS",TABLE,ELEMENT))
        IF $DATA(@ROOTREF) DO
        . WRITE "Data from prior run found.",!
        . WRITE "DELETE prior data" DO YN^DICN WRITE !
        . IF %=1 KILL @ROOTREF QUIT
        . IF %=-1 QUIT
        . SET %=2
        . WRITE "Continue scanning" DO YN^DICN WRITE !
        . IF %=-1 QUIT
        . IF %=1 SET TMGDFN=+$GET(@ROOTREF@("LAST DFN")) QUIT
        . SET TMGDFN="DONE"
        IF %=-1 GOTO DS2DN
        NEW STIME SET STIME=$H
        NEW DONE SET DONE=0
        NEW FOUNDREF SET FOUNDREF=$NAME(@ROOTREF@("FOUND")) KILL @FOUNDREF
        NEW REFUSEDREF SET REFUSEDREF=$NAME(@ROOTREF@("REFUSED")) KILL @REFUSEDREF
        NEW DATAREF SET DATAREF=$NAME(@ROOTREF@("DATA"))
        IF TMGDFN'="DONE" DO
        . NEW DFNMAX SET DFNMAX=$ORDER(^DPT("!"),-1)
        . FOR  SET TMGDFN=$ORDER(^DPT(TMGDFN)) QUIT:(+TMGDFN'>0)!DONE  DO
        . . IF TMGDFN#5=0 DO
        . . . DO PROGBAR^TMGUSRI2(TMGDFN,"PROGRESS: "_TMGDFN,0,DFNMAX,60,STIME)
        . . . IF $$USRABORT^TMGUSRI2("scanning patients") DO
        . . . . SET DONE=1,TMGRESULT="-1^Aborted"
        . . DO TAB2REM(TMGDFN,TABLE,ELEMENT,.KEYTERM,DATAREF)
        . . SET @ROOTREF@("LAST DFN")=TMGDFN
        IF +TMGRESULT'>0 GOTO DS2DN
DS2A    DO SEL2ARR(DATAREF,ELEMENT,KEYTERM,"GIVEN",FOUNDREF,FOUNDVPT)
        SET TMGRESULT=$$NSUREHAS^TMGPXRU1(FOUNDREF,FOUNDVPT)
        IF +TMGRESULT'>0 GOTO DS2DN
        DO SEL2ARR(DATAREF,ELEMENT,KEYTERM,"REFUSED",REFUSEDREF,REFUSEDVPT)
        SET TMGRESULT=$$NSUREHAS^TMGPXRU1(REFUSEDREF,REFUSEDVPT)
        IF +TMGRESULT'>0 GOTO DS2DN
DS2DN   QUIT TMGRESULT
        ;
TAB2REM(TMGDFN,TABLE,ELEMENT,KEYTERM,DATAREF) ;
        ;"Input: TMGDFN -- patient IEN
        ;"       TABLE -- Table name, e.g. 'STUDIES'
        ;"       ELEMENT -- Table element name, e.g. 'ZOSTAVAX'  (should be upper case)
        ;"       DATAREF -- REF of output
        ;"Result: none.
        NEW TABARRAY,VALUE
        SET KEYTERM=$GET(KEYTERM)
        DO GETSPECL^TMGTIUO4(TMGDFN,TABLE,"BLANK_LINE",99,.TABARRAY,0,"")  ;"mode 1 = only last table
        IF $DATA(TABARRAY)=0 GOTO T2RDN
        SET VALUE=$$TRIM^XLFSTR($GET(TABARRAY("KEY-VALUE",ELEMENT)))
        IF (VALUE="")!(VALUE=".")!($$UP^XLFSTR(VALUE)["NO DATA") GOTO T2RDN
        IF (KEYTERM'=""),(VALUE'[KEYTERM) GOTO T2RDN
        SET @DATAREF@(TMGDFN)=VALUE
T2RDN   QUIT
        ;
SEL2ARR(DATAREF,ELEMENT,KEYTERM,MESSAGE,OUTREF,FACTOR) ;   
        ;"Input: DATAREF -- REF of data source
        ;"       ELEMENT -- Table element name, e.g. 'ZOSTAVAX'  (should be upper case)
        ;"       KEYTERM -- key term
        ;"       MESSAGE -- Name message to show user
        ;"       OUTREF -- Output reference. 
        ;"       FACTOR -- Factor being considered.  If patient has, will be skipped.
        ;"Result: none.
        NEW MSG SET MSG="Select items: "_KEYTERM_" has been "_MESSAGE
        WRITE !,!,MSG,!
        WRITE "If line contains a mixed message, select IF ANY part of the line matches.",!
        DO PRESS2GO^TMGUSRI2
        NEW SELARR,OUTARR,STR
        NEW ABORT SET ABORT=0
        NEW TMGDFN SET TMGDFN=0
        FOR  SET TMGDFN=$ORDER(@DATAREF@(TMGDFN)) QUIT:(+TMGDFN'>0)!ABORT  DO
        . SET STR=$GET(@DATAREF@(TMGDFN)) QUIT:STR=""
        . NEW TEMP SET TEMP=$$IFHAS^TMGPXRU1(TMGDFN,FACTOR)
        . IF +TEMP=1 QUIT  ;"Pt already has
        . IF +TEMP=-1 DO  QUIT
        . . WRITE !,"Error with DFN=",TMGDFN," MSG=",$PIECE(TEMP,"^",2),!
        . IF $DATA(@OUTREF@(TMGDFN)) QUIT  ;"already done.
        . SET STR=$$GET1^DIQ(2,TMGDFN,.01)_": "_STR
        . SET SELARR(STR,TMGDFN)="" 
        IF $DATA(SELARR)=0 GOTO S2ADN
        DO SELECTR2^TMGUSRI3("SELARR","OUTARR",MSG_". <ESC><ESC> to exit")
        KILL SELARR
        SET STR="" 
        FOR  SET STR=$ORDER(OUTARR(STR)) QUIT:(STR="")  DO
        . NEW DATES DO STR2DATES(STR,.DATES)
        . SET TMGDFN="" FOR  SET TMGDFN=$ORDER(OUTARR(STR,TMGDFN)) QUIT:(TMGDFN="")  DO
        . . NEW FMDT SET FMDT="" 
        . . FOR  SET FMDT=$ORDER(DATES(FMDT)) QUIT:(FMDT="")  DO
        . . . NEW S1 SET S1=STR FOR  Q:$LENGTH(S1)>40  SET S1=S1_" "
        . . . ;"NEW S2 SET S2=$PIECE(S1,":",1)_": "_MESSAGE_" ["_$$FMTE^XLFDT(FMDT)_"]<--"_$PIECE(S1,":",2)
        . . . NEW S2 SET S2=$PIECE(S1,":",1)_": ["_$$FMTE^XLFDT(FMDT)_"]: "_$PIECE(S1,":",2)
        . . . SET SELARR(S2,TMGDFN_"^"_FMDT,"SEL")=""
        IF $DATA(SELARR)=0 GOTO S2ADN
        KILL OUTARR
        DO SELECTR2^TMGUSRI3("SELARR","OUTARR","Deselect any erroneous entries for: "_MESSAGE_". <ESC><ESC> to exit")
        SET STR="" 
        FOR  SET STR=$ORDER(OUTARR(STR)) QUIT:(STR="")  DO
        . NEW REPLY SET REPLY=""
        . FOR  SET REPLY=$ORDER(OUTARR(STR,REPLY)) QUIT:(REPLY="")  DO
        . . SET TMGDFN=$PIECE(REPLY,"^",1)
        . . NEW FMDT SET FMDT=$PIECE(REPLY,"^",2)
        . . SET @OUTREF@(TMGDFN,FMDT)=""
S2ADN   QUIT
        ;
STR2DATES(STR,OUT) ;
        ;"Purpose: Convert string containing dates and other text, into array of FM dates
        SET STR=$TRANSLATE(STR,".,;-[]()<>~:'""","                ")
        NEW TMGINDENT,TEMPARR DO SPLIT2AR^TMGSTUT2(STR," ",.TEMPARR)
        NEW NOW SET NOW=$$NOW^XLFDT
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(TEMPARR(IDX)) QUIT:(+IDX'>0)  DO
        . NEW WORD SET WORD=$GET(TEMPARR(IDX))
        . IF $LENGTH(WORD)<3 QUIT
        . NEW FMDT IF $$ISDATE(WORD,.FMDT)=0 QUIT
        . IF FMDT>NOW QUIT  ;"ignore future dates.
        . SET OUT(FMDT,WORD)=""
        QUIT
        ;
ISDATE(WORD,OUTFMDT) ;
        ;"Determine IF word is a date, and IF so, turn into FM date
        NEW RESULT SET RESULT=0
        SET WORD=$TRANSLATE(WORD,"~),;?.""","")
        IF $EXTRACT(WORD,1,2)="on" SET WORD=$PIECE(WORD,"on",2)
        IF +WORD'>0 GOTO IDDN
        NEW %DT,X,Y 
        SET %DT="P",X=WORD
        IF WORD["/",$LENGTH(WORD,"/")=2 SET %DT=%DT_"M"  ;"just month/year
        DO ^%DT
        IF Y'>0 GOTO IDDN
        
        SET OUTFMDT=Y,RESULT=1        
IDDN    QUIT RESULT        
        ;
SCAN4MAM ;" Cycle through all mammogram consults 
        ;" to ensure that every patient who has a completed mammogram and
        ;" ordered mammogram... has the proper Health Factor
        ;" 4/19/13
        NEW MAMMOIEN SET MAMMOIEN=+$ORDER(^GMR(123.5,"B","MAMMOGRAM",""))
        IF MAMMOIEN'>0 DO  GOTO MRPTDN
        . WRITE "Can't locate record for MAMMOGRAM consults.  Aborting.",!
        NEW COMPLIEN SET COMPLIEN=+$ORDER(^ORD(100.01,"B","COMPLETE",""))
        IF COMPLIEN'>0 DO  GOTO MRPTDN
        . WRITE "Can't find record for COMPLETE status.  Aborting.",!
        NEW PENDIEN SET PENDIEN=+$ORDER(^ORD(100.01,"B","PENDING",""))
        NEW X,ORDDATE DO NOW^%DTC NEW NOWDATE SET NOWDATE=X
        NEW IDX SET IDX=""
        NEW COMMATCHES,PENDMATCHES,RESOLVEDATE
        NEW TIUIEN
        FOR  SET IDX=+$ORDER(^GMR(123,"C",MAMMOIEN,IDX)) QUIT:(IDX'>0)  DO
        . NEW S
        . NEW ZNODE SET ZNODE=$GET(^GMR(123,IDX,0))
        . NEW STATUS SET STATUS=$PIECE(ZNODE,"^",12)
        . SET ORDDATE=$PIECE(ZNODE,"^",7)  ;"date of request
        . NEW PTIEN SET PTIEN=+$PIECE(ZNODE,"^",2)
        . IF STATUS=COMPLIEN DO
        . . ;A COMPLETED STATUS WILL RECEIVE TWO HF... ONE FOR ORDERED ONE FOR COMPLETED
        . . SET TIUIEN=$PIECE(ZNODE,"^",20)
        . . IF TIUIEN="" DO
        . . . WRITE ZNODE," IS EMPTY.",!
        . . . SET RESOLVEDDATE=ORDDATE
        . . ELSE  SET RESOLVEDDATE=$PIECE($GET(^TIU(8925,TIUIEN,0)),"^",7)
        . . SET COMMATCHES(PTIEN,RESOLVEDDATE)=""
        . . SET PENDMATCHES(PTIEN,ORDDATE)=""
        . ELSE  IF STATUS=PENDIEN DO
        . . ;ASSIGN TO ORDERED, NOT COMPLETED
        . . SET PENDMATCHES(PTIEN,ORDDATE)=""
        ;
        ;
        ;"WRITE "AND NOW FOR THE RESULTS. PLEASE SPOT CHECK THESE.",!
        ;"NEW TMGDFN SET TMGDFN=1
        ;"NEW DATE,NAME,Y,TEMPARRAY
        ;"FOR  SET TMGDFN=$ORDER(COMMATCHES(TMGDFN)) QUIT:(TMGDFN'>0)  DO
        ;". SET DATE=1
        ;". SET NAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
        ;". FOR  SET DATE=$ORDER(COMMATCHES(TMGDFN,DATE)) QUIT:(DATE'>0)  DO
        ;". . SET Y=DATE
        ;". . DO DD^%DT
        ;". . ;WRITE "        DATE: ",Y,!
        ;". . SET TEMPARRAY(NAME,Y)="COMPLETED"
        ;". ;WRITE "SHOWS COMPLETED=======",!,!
        ;"SET TMGDFN=1
        ;"FOR  SET TMGDFN=$ORDER(PENDMATCHES(TMGDFN)) QUIT:(TMGDFN'>0)  DO
        ;". SET DATE=1
        ;". SET NAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
        ;". FOR  SET DATE=$ORDER(PENDMATCHES(TMGDFN,DATE)) QUIT:(DATE'>0)  DO
        ;". . SET Y=DATE
        ;". . DO DD^%DT
        ;". . ;WRITE "        DATE: ",Y,!
        ;". . SET TEMPARRAY(NAME,Y)="ORDERED"
        ;". ;WRITE "SHOWS ORDERED=======",!,!
        ;"SET NAME=1
        ;"NEW DATE,STAT
        ;"FOR  SET NAME=$ORDER(TEMPARRAY(NAME)) QUIT:(NAME="")  DO
        ;". WRITE NAME," =============",!
        ;". SET DATE=1
        ;". FOR  SET DATE=$ORDER(TEMPARRAY(NAME,DATE)) QUIT:(DATE="")  DO
        ;". . SET STAT=$GET(TEMPARRAY(NAME,DATE))
        ;". . IF STAT="ORDERED" WRITE "     ORDERED: ",DATE,!
        ;". . ELSE  WRITE "     COMPLETED: ",DATE,!
        ;". WRITE "=========================================",!,!
        ;"FILE THESE LISTS
        WRITE "FILING ARRAYS NOW",!
        NEW TMGRESULT
        SET TMGRESULT=$$NSUREHAS^TMGPXRU1("COMMATCHES","HF.TMG MAMMOGRAM/IMAGING DONE")
        SET TMGRESULT=$$NSUREHAS^TMGPXRU1("PENDMATCHES","HF.TMG MAMMOGRAM/IMAGING ORDERED")
MRPTDN  QUIT
        ;
