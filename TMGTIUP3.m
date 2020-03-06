TMGTIUP3 ;TMG/kst-TIU processing Functions ;4/11/17, 6/26/17, 5/21/18
         ;;1.0;TMG-LIB;**1**;5/1/13
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
 ;" RPC -- Public Functions.
 ;"=======================================================================
 ;"PROCESS(TMGRESULT,TMGIN) -- 
 ;"FRSHTABL(TMGRESULT,TMGIN,OPTION) -- REFRESH TABLES
 ;"GETTABLS(TABLES,OPTION) -- Get list of all defined text tables, minus protected ones
 ;"TABLEND(LINE,PARTB) --Determine if HTML-coded line includes the end of a table.
 ;"PRTIUHTM(TEXT)  --PARSE HTML IN TYPICAL FORMAT FOR FPG/TMG NOTES, INTO ARRAY (HANDLING TABLES)  
 ;"SAVAMED(TMGIN)  ;"SAVE ARRAY (containing a note) containing MEDS to 22733.2 
 ;"
 ;"=======================================================================
 ;"Dependancies:
 ;" TMGHTM1, TMGTIUOJ, TMGTIUO6, XLFSTR, TMGSTUT2, TMGSTUT3
 ;"=======================================================================
 ;
PROCESS(TMGRESULT,TMGIN,OPTION) ;
        ;"As of 5/20/18, called from:
        ;"   PROCESS^TMGRPC1H <-- RPC call 'TMG CPRS PROCESS NOTE'
        ;"   GETHPI^TMGTIUP2 <-- LASTHPI^TMGTIUP2 <-- LASTHPI^TMGTIUOJ <-- TIU TEXT OBJECT 'TMG LAST HPI'
        ;"Purpose: Entrypoint for RPC to effect processing a note from CPRS
        ;"Input: TMGRESULT -- PASS BY REFERENCE, an OUT PARAMETER.
        ;"       TMGIN -- Input from client.  Format:
        ;"          TMGIN("DFN")=<DFN>  (IEN in PATIENT file)
        ;"          TMGIN("NoteIEN")=<TIUIEN>
        ;"          TMGIN("TEXT",1) = 1st line of text
        ;"          TMGIN("TEXT",2) = 2nd line of text, etc
        ;"       OPTION -- PASS BY REFERENCE.  OPTIONAL
        ;"          OPTION("FORCE PROCESS")=# (default is 1) If 1 note is processed even if tag is absent
        ;"          OPTION("ALL NOTES")= Use all notes (instead of only completed notes)
        ;"          OPTION("FORCE REFRESH TABLE",<TABLE_NAME>)=1 <-- don't allow <TABLE_NAME> to be excluded.         
        ;"Output: TMGRESULT(0)="1^Success", or "-1^Error Message"
        ;"        TMGRESULT(1)=1st line of return text
        ;"        TMGRESULT(2)=2nd line of return text, etc.
        ;"Result: none
        NEW TMGZZ SET TMGZZ=0  ;"Set to 1 during debug if needed.
        IF TMGZZ=1 DO
        . KILL TMGIN,OPTION
        . MERGE TMGIN=^TMG("TMG","RPC","TMG CPRS PROCESS NOTE","TMGIN")        
        . MERGE OPTION=^TMG("TMG","RPC","TMG CPRS PROCESS NOTE","OPTION")        
        ELSE  DO
        . KILL ^TMG("TMG","RPC","TMG CPRS PROCESS NOTE")
        . MERGE ^TMG("TMG","RPC","TMG CPRS PROCESS NOTE","TMGIN")=TMGIN
        . MERGE ^TMG("TMG","RPC","TMG CPRS PROCESS NOTE","OPTION")=OPTION
        ;
        NEW FORCE,TAGFOUND KILL TMGRESULT
        SET FORCE=+$GET(OPTION("FORCE PROCESS"),1) ;"I.e. force processing regardless of tag presence.  
        SET TAGFOUND=$$STRIPARR^TMGSTUT3($NAME(TMGIN("TEXT")),"PROCESS NOTE NOT DONE")  ;"remove process tag
        IF (TAGFOUND=0)&(FORCE=0) DO  GOTO PRODN 
        . MERGE TMGRESULT=TMGIN("TEXT")
        SET OPTION("HTML")=$$ISHTMREF^TMGHTM1($NAME(TMGIN("TEXT")))
        DO FRSHTABL(.TMGRESULT,.TMGIN,.OPTION) 
        ;"NOTE: Later, other tasks could also be done at this time. 
PRODN   QUIT
        ;       
REFRSHTB(ITEMARRAY,DFN)  ;"REFRESH TABLES.  -- DEPRECIATED
        ;"//NOTE: As of 5/20/18, this REFRSHTB() doesn't seem to be in use.  
        ;"        This code refreshes tables that have already been parsed out from HPI
        ;"        Section.  But it would be unable to parse the FINAL MEDICATIONS table, for example.
        ;"        Compare to FRSHTABL() which uses a stream approach and can work with all tables.
        ;"INPUT: ITEMARRAY -- PASS BY REFERENCE.  AN OUT PARAMETER. FORMAT:
        ;"          ITEMARRAY(Ref#)=<Full section text>
        ;"          ITEMARRAY(Ref#,#)=different parts of section
        ;"          ITEMARRAY("TEXT",Ref#)=Title of section  
        ;"          ITEMARRAY("TEXT",Ref#,#)=sequential parts of section  
        ;"             ITEMARRAY("TEXT",3)="Dyspepsia"  
        ;"             ITEMARRAY("TEXT",3,1)=part 1, e.g. text, e.g. [GROUP A&B]
        ;"                ITEMARRAY("TEXT",3,1)="[GROUP]"
        ;"                ITEMARRAY("TEXT",3,1,"GROUP")="A&B"
        ;"             ITEMARRAY("TEXT",3,2)=part 2, e.g. name of inline table
        ;"                ITEMARRAY("TEXT",3,2)="[TABLE]"  <-- signal this part is a table. 
        ;"                ITEMARRAY("TEXT",3,2,"TABLE")=WT   <-- WT is name of table
        ;"                ITEMARRAY("TEXT",3,2,"TEXT")=<TEXT OF TABLE>
        ;"                ITEMARRAY("TEXT",3,2,"INLINE")=0 or 1        
        ;"            ITEMARRAY("TEXT",Ref#,3)=part 3, e.g. more text
        ;"            ITEMARRAY("TEXT",Ref#,4)=part 4, e.g. name of table  
        ;"            ITEMARRAY("TEXT",Ref#,"GROUPX",#)=""  <-- index of GROUP nodes
        ;"            ITEMARRAY("TEXT",Ref#,"TABLEX",#)=""  <-- index of TABLE nodes        
        ;"       DFN - IEN of the patient
        ;"Result: 1^OK, or -1^Error message
        NEW TMGRESULT SET TMGRESULT="1^OK"
        NEW PARAIDX SET PARAIDX=0
        FOR  SET PARAIDX=$ORDER(ITEMARRAY("TEXT",PARAIDX)) QUIT:PARAIDX'>0  DO
        . NEW JDX SET JDX=0
        . FOR  SET JDX=$ORDER(ITEMARRAY("TEXT",PARAIDX,JDX)) QUIT:JDX'>0  DO
        . . IF $GET(ITEMARRAY("TEXT",PARAIDX,JDX))="[TABLE]" DO
        . . . NEW TABLENAME SET TABLENAME=$GET(ITEMARRAY("TEXT",PARAIDX,JDX,"TABLE"))
        . . . IF TABLENAME'="" DO
        . . . . NEW TABLESTR,TABLEARR
        . . . . SET TABLESTR=$$GETTABLX^TMGTIUO6(DFN,TABLENAME,.TABLEARR)
        . . . . SET ITEMARRAY("TEXT",PARAIDX,JDX,"TEXT")=TABLESTR
        QUIT TMGRESULT
        ;        
FRSHTABL(TMGRESULT,TMGIN,OPTION) ;"REFRESH TABLES
        ;"NOTE: As of 5/20/18, only called from PROCESS^TMGTIUP3()
        ;"Input: TMGRESULT -- PASS BY REFERENCE, an OUT PARAMETER.  Format:
        ;"          TMGRESULT(#)=<line of text>
        ;"       TMGIN -- Input from client.  Format:
        ;"              TMGIN("DFN")=<DFN>  (IEN in PATIENT file)
        ;"              TMGIN("TEXT",1) = 1st line of text
        ;"              TMGIN("TEXT",2) = 2nd line of text, etc
        ;"       OPTION -- PASS BY REFERENCE.  OPTIONAL
        ;"            OPTION("HTML")=1 if text is in HTML format. 
        ;"            OPTION("ALL NOTES")=(OPTIONAL) - Use all notes (as
        ;"                               opposed to only completed notes)
        ;"            OPTION("FORCE REFRESH TABLE",<TABLE_NAME>)=1 <-- don't allow <TABLE_NAME> to be excluded.         
        ;"            OPTION("DIRECT HTML INSERTION")=1  <-- Output should be ready to insert directly into HTML DOM
        ;"Result: None
        SET TMGRESULT(0)="1^Success"
        NEW ATABLE,TABLES DO GETTABLS(.TABLES,.OPTION)
        NEW TMGDFN SET TMGDFN=+$GET(TMGIN("DFN"))
        NEW TERMINALCHARS SET TERMINALCHARS=""
        NEW FOUNDTABLE SET FOUNDTABLE=""
        NEW OUTLNUM SET OUTLNUM=0
        NEW HTML SET HTML=+$GET(OPTION("HTML"))
        NEW DIRHTMLINSERT SET DIRHTMLINSERT=+$GET(OPTION("DIRECT HTML INSERTION"))
        NEW TAGS
        NEW DONE SET DONE=0
        NEW INSCRIPT SET INSCRIPT=0
        NEW LNUM SET LNUM=$ORDER(TMGIN("TEXT",0))
        NEW INLINEMODE SET INLINEMODE=0
        FOR  QUIT:(+LNUM'>0)!DONE  DO
        . NEW LINE 
        . IF HTML DO
        . . SET LINE=$$GETNLHTM($NAME(TMGIN("TEXT")),.LNUM,.INSCRIPT,.TAGS) ;"Get next line (html)
        . ELSE  DO
        . . SET LINE=$$GETNL($NAME(TMGIN("TEXT")),.LNUM) ;"Get next line
        . IF INSCRIPT DO  QUIT  ;"don't search for tables to refresh inside script javascript code
        . . SET OUTLNUM=OUTLNUM+1,TMGRESULT(OUTLNUM)=LINE
        . IF LNUM="" SET DONE=1
        . IF FOUNDTABLE'="" DO  ;"a table start has been found 
        . . NEW TL SET TL=$SELECT(HTML:$$HTMLTRIM^TMGHTM1(LINE),1:$$TRIM^XLFSTR(LINE))
        . . NEW PARTB SET PARTB="" 
        . . NEW ENDFOUND SET ENDFOUND=0
        . . IF INLINEMODE DO
        . . . IF LINE[TERMINALCHARS DO
        . . . . SET ENDFOUND=1
        . . . . SET LINE=$PIECE(LINE,TERMINALCHARS,2)
        . . ELSE  DO
        . . . IF HTML DO  
        . . . . SET ENDFOUND=$$TABLEND(LINE,.PARTB)
        . . . . IF ENDFOUND SET LINE=PARTB 
        . . . ELSE  SET ENDFOUND=(LINE="")
        . . IF 'ENDFOUND QUIT
        . . NEW TABLESTR,TABLEARR
        . . SET TABLESTR=$$GETTABLX^TMGTIUO6(TMGDFN,FOUNDTABLE,.TABLEARR,.OPTION)
        . . NEW TEMPARR
        . . IF 'INLINEMODE DO
        . . . DO SPLIT2AR^TMGSTUT2(TABLESTR,$CHAR(13,10),.TEMPARR,OUTLNUM+1)
        . . . KILL TEMPARR("MAXNODE")
        . . . ;"IF HTML,(DIRHTMLINSERT'=1) DO TXT2HTML^TMGHTM1(.TEMPARR)
        . . . MERGE TMGRESULT=TEMPARR
        . . ELSE  DO
        . . . SET LINE=$$SYMENC^MXMLUTL(TABLESTR)_LINE
        . . SET OUTLNUM=$ORDER(TMGRESULT(""),-1)
        . . SET OUTLNUM=OUTLNUM+1,TMGRESULT(OUTLNUM)=LINE
        . . SET FOUNDTABLE=""
        . . SET TERMINALCHARS=""
        . . SET INLINEMODE=0
        . ELSE  DO  ;"Searching for the start of a table. 
        . . SET ATABLE=""
        . . FOR  SET ATABLE=$ORDER(TABLES(ATABLE)) QUIT:(ATABLE="")!(FOUNDTABLE'="")  DO
        . . . IF LINE[ATABLE DO
        . . . . NEW NAME,IEN
        . . . . SET NAME=$$HTML2TXS^TMGHTM1($PIECE($PIECE(ATABLE,"]",1),"[",2))
        . . . . SET IEN=$ORDER(^TMG(22708,"B",NAME,0))
        . . . . NEW ZN SET ZN=$GET(^TMG(22708,IEN,0))
        . . . . IF $PIECE(ZN,"^",5)="I" DO   ;"Handle InLine Tables Here
        . . . . . NEW TERMCHARS,INPARTA,INPARTB
        . . . . . SET TERMCHARS=$PIECE($GET(^TMG(22708,IEN,4)),"^",1)
        . . . . . SET INPARTA=$PIECE(LINE,ATABLE,1)
        . . . . . IF LINE[TERMCHARS DO
        . . . . . . ;" Old method, didn't account for 2 possible inline tables in one line -> SET INPARTB=$PIECE(LINE,TERMCHARS,2)
        . . . . . . SET INPARTB=$E(LINE,$L(INPARTA),$L(LINE)) ;"GET REST OF LINE HERE 
        . . . . . . SET INPARTB=$PIECE(INPARTB,TERMCHARS,2,999)
        . . . . . . NEW TABLESTR SET TABLESTR=$$GETTABLX^TMGTIUO6(TMGDFN,ATABLE)
        . . . . . . SET TABLESTR=$$SYMENC^MXMLUTL(TABLESTR) 
        . . . . . . SET LINE=INPARTA_TABLESTR_INPARTB 
        . . . . . ELSE  DO
        . . . . . . SET FOUNDTABLE=TABLES(ATABLE)
        . . . . . . SET INLINEMODE=1
        . . . . . . SET TERMINALCHARS=TERMCHARS
        . . . . . . IF HTML DO
        . . . . . . . SET LINE=INPARTA
        . . . . . . ELSE  DO
        . . . . . . . SET LINE="#!$<SKIP<$!#"
        . . . . ;"Finished with InLine Tables
        . . . . ELSE  DO
        . . . . . SET FOUNDTABLE=TABLES(ATABLE)
        . . . . . IF HTML DO
        . . . . . . SET LINE=$PIECE(LINE,ATABLE,1)
        . . . . . . SET LINE=$$HTMLTRIM^TMGHTM1(LINE,"R"," -")
        . . . . . ELSE  DO
        . . . . . . SET LINE="#!$<SKIP<$!#"
        . . IF LINE="#!$<SKIP<$!#" QUIT
        . . SET OUTLNUM=OUTLNUM+1,TMGRESULT(OUTLNUM)=LINE
        QUIT        
        ; 
GETNLHTM(REFARR,IDX,INSCRIPT,TAGS) ;"Get next line (html text)
        ;"Input: REFARR -- pass by NAME <-- original array is modified during processing
        ;"       IDX -- PASS BY REFERENCE. <-- modified during processing
        ;"       INSCRIPT -- PASS BY REFERENCE.  BOOLEAN
        NEW LINE SET LINE=""
        SET IDX=$GET(IDX)
        NEW DONE SET DONE=0
        IF $DATA(TAGS)=0 DO
        . SET TAGS("SCRIPT")=""
        . SET TAGS("BR")=""
        . SET TAGS("P")=""
        . SET TAGS("LI")=""
        . SET TAGS("HEAD")=""
        . SET TAGS("BODY")=""
        . SET TAGS("STYLE")=""
        . SET TAGS("HTML")=""
        . SET TAGS("META")=""
        . SET TAGS("BUTTON")=""
        . SET TAGS("INPUT")=""
        . SET TAGS("RADIO")=""
        . SET TAGS("SPAN")=""
        . SET TAGS("DIV")=""
        FOR  QUIT:DONE!(IDX="")  DO
        . NEW NEWPART SET NEWPART=$GET(@REFARR@(IDX))
        . NEW ORD
        . NEW DIV SET DIV=""
        . FOR  SET DIV=$ORDER(TAGS(DIV)) QUIT:DONE!(DIV="")  DO
        . . NEW POS 
        . . NEW OTAG SET OTAG="<"_DIV
        . . NEW CTAG SET CTAG="</"_DIV
        . . SET POS=$FIND(NEWPART,OTAG)
        . . IF POS>0 SET ORD(POS)=OTAG 
        . . SET POS=$FIND(NEWPART,CTAG)
        . . IF POS>0 SET ORD(POS)=CTAG
        . NEW FIRSTPOS SET FIRSTPOS=$ORDER(ORD(0))
        . IF FIRSTPOS>0 DO   ;"@REFARR@(IDX) contains at least 1 wanted tag
        . . SET DONE=1
        . . NEW TAG SET TAG=ORD(FIRSTPOS)        
        . . IF TAG["SCRIPT" SET INSCRIPT='INSCRIPT        
        . . NEW POS SET POS=$FIND(NEWPART,">",FIRSTPOS) ;"returns index AFTER find, or 0        
        . . IF POS>0 DO
        . . . SET @REFARR@(IDX)=$EXTRACT(NEWPART,POS,$LENGTH(NEWPART))
        . . . SET NEWPART=$EXTRACT(NEWPART,1,POS-1)
        . . ELSE  DO  ;"closing '>' of tag not found ... shouldn't happen
        . . . SET @REFARR@(IDX)=""
        . . SET LINE=LINE_NEWPART
        . . IF $GET(@REFARR@(IDX))="" SET IDX=$ORDER(@REFARR@(IDX))
        . ELSE  DO  ;"@REFARR@(IDX) doesn't contain any wanted tags
        . . SET LINE=LINE_NEWPART
        . . SET @REFARR@(IDX)=""
        . . SET IDX=$ORDER(@REFARR@(IDX))
        . IF INSCRIPT SET DONE=1
GNLDN   QUIT LINE        
        ;
GETNL(REFARR,IDX) ;"Get next line (non-html text)
        ;"Input: REFARR -- pass by NAME <-- original array is modified during processing
        ;"       IDX -- PASS BY REFERENCE. <-- modified during processing
        SET IDX=+$GET(IDX)
        NEW LINE SET LINE=$GET(@REFARR@(IDX))
        SET @REFARR@(IDX)=""
        SET IDX=$ORDER(@REFARR@(IDX))
        QUIT LINE        
        ;
GETTABLS(TABLES,OPTION) ;"Get list of all defined text tables, minus protected ones
        ;"Note: Some tables don't need refreshing because they are fresh from
        ;"      insertion as TIU TEXT OBJECTS.  These are removed from list.
        ;"Input: TABLES -- an OUT PARAMETER
        ;"       HTML -- 1 IF should be in HTML format.
        ;"       OPTION -- OPTIONAL.
        ;"             OPTION("HTML")=1 if text is in HTML format. 
        ;"             OPTION("FORCE REFRESH TABLE",<TABLE_NAME>)=1 <-- don't allow <TABLE_NAME> to be excluded.         
        ;"        e.g. OPTION("FORCE REFRESH TABLE","FINAL MEDICATIONS")=1 <-- don't allow "FINAL MEDICATIONS" to be excluded.         
        ;"Result : none
        ;
GTL1    ;"Below are tables that will NOT be refreshed during PROCESS
        ;;MEDICATIONS
        ;;FINAL MEDICATIONS        
        ;;PROBLEM LIST
        ;;SURGERIES
        ;;SOCIAL HX
        ;;FAMILY HX
        ;;STUDIES
        ;;PROVIDER TEAM
        ;;IMMUNIZATIONS
        ;;HEALTH FACTORS
        ;;FOLLOWUP ITEMS
        ;;
        DO GETTBLST^TMGTIUO6(.TABLES)
        NEW HTML SET HTML=+$GET(OPTION("HTML"))
        NEW LINE SET LINE=1
        NEW LABEL
        FOR  SET LABEL=$$TRIM^XLFSTR($PIECE($TEXT(GTL1+LINE^TMGTIUP3),";;",2)),LINE=LINE+1 QUIT:LABEL=""  DO
        . IF $GET(OPTION("FORCE REFRESH TABLE",LABEL))=1 QUIT ;"don't kill from TABLES array if FORCE exception found. 
        . KILL TABLES("["_LABEL_"]")        
        IF HTML DO
        . NEW TEMPARR,TEMPARR2,LBL,IDX SET IDX=0
        . SET LBL="" FOR  SET LBL=$ORDER(TABLES(LBL)) QUIT:LBL=""  SET IDX=IDX+1,TEMPARR(IDX)=LBL
        . DO TXT2HTML^TMGHTM1(.TEMPARR) 
        . SET IDX=0 FOR  SET IDX=$ORDER(TEMPARR(IDX)) QUIT:IDX'>0  DO
        . . ;"NEW ENCODEDLBL SET ENCODEDLBL=$GET(TEMPARR(IDX))
        . . ;"SET TABLES(ENCODEDLBL)=ENCODEDLBL
        . . ;"original lines above. the array wasn't being encoded with HTML
        . . NEW LABEL SET LABEL=$G(TEMPARR(IDX))
        . . NEW ENCODEDLBL SET ENCODEDLBL=$$REPLSTR^TMGSTUT3(LABEL," ","&nbsp;")
        . . SET TABLES(ENCODEDLBL)=LABEL
        . . SET TABLES(LABEL)=LABEL
        . ;"//KT 5/23/18  There was situation where LBL="[WT]", and subtracting 4 from length left "", which prevented cycling with $O and idx'=""
        . ;" NEW TEMPARR,TEMPARR2,LBL,IDX SET IDX=0
        . ;" SET LBL="" FOR  SET LBL=$ORDER(TABLES(LBL)) QUIT:LBL=""  SET IDX=IDX+1,TEMPARR(IDX)=LBL
        . ;" MERGE TEMPARR2=TEMPARR
        . ;" DO TXT2HTML^TMGHTM1(.TEMPARR) 
        . ;" SET IDX=0 FOR  SET IDX=$ORDER(TEMPARR(IDX)) QUIT:IDX=""  DO
        . ;" . SET LBL=$GET(TEMPARR(IDX))
        . ;" . SET LBL=$EXTRACT(LBL,1,$LENGTH(LBL)-4)  ;"//kt note 5/23/18 <--- I don't understand what this is used for
        . ;" . SET TABLES(LBL)=$GET(TEMPARR2(IDX))
        . ;" . NEW LBL2 SET LBL2=$$REPLSTR^TMGSTUT3(LBL," ","&nbsp;")   ;"//kt 8/1/17 
        . ;" . SET TABLES(LBL2)=$GET(TEMPARR2(IDX))        ;"//kt 8/1/17
        . ;" . SET TABLES($$HTML2TXS^TMGHTM1($GET(TEMPARR(IDX))))=$GET(TEMPARR2(IDX))
        ELSE  DO
        . NEW LBL SET LBL="" 
        . FOR  SET LBL=$ORDER(TABLES(LBL)) QUIT:LBL=""  SET TABLES(LBL)=LBL     
        ;"KILL TABLES("[FOLLOWUP&nbsp;ITEMS]")
        ;"KILL TABLES("[FOLLOWUP ITEMS]")
        QUIT
        ;
TABLEND(LINE,PARTB) ;" HAS TABLE END  
        ;"Purpose: Determine if HTML-coded line includes the end of a table.
        ;"         It is expected that begining of table has been found
        ;"Input: LINE --The line to check.  WILL BE MODIFIED IF PASSED BY REFERENCE. 
        ;"       PARTB -- an OUT PARAMETER.  PASS BY REFERENCE.
        ;"              The residual part of the line (if any) that 
        ;"              represents text AFTER the table.
        ;"Result: 1 if end of table found.  0 otherwise.
        ;"Output: PARTB is filled with residual line (if any)
        NEW TMGRESULT SET TMGRESULT=0
        SET PARTB=""
        NEW DIVPOS,DIV,FOUND,EITHER
        SET EITHER=0 FOR  DO  QUIT:EITHER=0          
        . SET FOUND=1 FOR  QUIT:FOUND=0  DO 
        . . DO RPTAGS^TMGHTM1(.LINE,"&nbsp;<BR>","<BR>",.FOUND)
        . . SET EITHER=EITHER!FOUND
        . SET FOUND=1 FOR  QUIT:FOUND=0  DO
        . . DO RPTAGS^TMGHTM1(.LINE," <BR>","<BR>",.FOUND)
        . . SET EITHER=EITHER!FOUND        
        FOR DIV="<P>","</P>","<LI>","</LI>","<BR><BR>" DO  QUIT:TMGRESULT=1
        . SET DIVPOS($FIND(LINE,DIV))=DIV
        KILL DIVPOS(0)
        SET TMGRESULT=($DATA(DIVPOS)>0)
        IF TMGRESULT=1 DO
        . NEW DIVIDX SET DIVIDX=$ORDER(DIVPOS(0))
        . SET DIV=$GET(DIVPOS(DIVIDX))
        . SET PARTB=DIV_$PIECE(LINE,DIV,2,9999)
        ELSE  DO
        . SET LINE=$$HTMLTRIM^TMGHTM1(LINE,""," ",1)
        . IF LINE="" SET PARTB="<BR>",TMGRESULT=1
        QUIT TMGRESULT
        ;
CHNGES(TMGRESULT,XU1) ;change ES, Return 0 = success
       SET TMGRESULT="0^SUCCESSFUL"
       N XU2,XU3,XU4 
       S U="^",XU2=$P(XU1,U,2),XU3=$P(XU1,U,3),XU1=$P(XU1,U)
       S XU1=$$DECRYP^XUSRB1(XU1),XU2=$$DECRYP^XUSRB1(XU2),XU3=$$DECRYP^XUSRB1(XU3)
       ;"Test XU1, if correct then proceed
       NEW CES,X2
       S X2=$G(^VA(200,+$G(DUZ),20)),CES=$P(X2,U,4)
       IF XU1'=CES SET TMGRESULT="-1^INCORRECT CURRENT ELECTRONIC SIGNATURE-"_XU1_"-"_CES GOTO CESDN
       ;"here compare XU2 to XU3, if same then set to ES
       IF XU2'=XU3 SET TMGRESULT="-1^CONFIRMATION SIGNATURE DOES NOT MATCH" GOTO CESDN
       SET $P(^VA(200,DUZ,20),"^",4)=XU2
       ;"S XU3=$$BRCVC^XUS2(XU1,XU2),RET(0)=+XU3,RET(1)=$P(XU3,U,2,9)
       ;"I XU3>0 S DUZ=0 ;Clean-up if not changed.
       ;"I 'XU3,XU4 D KILL^XWBSEC("XUS DUZ")
CESDN  QUIT        
       ;
PRTIUHTM(TEXT,TABLES)  ;"PARSE HTML IN TYPICAL FORMAT FOR FPG/TMG NOTES, INTO ARRAY (HANDLING TABLES)  
       ;"//As of 5/20/18, only called from PRCSSTXT^TMGTIUIP2() 
       ;"INPUT: TEXT -- PASS BY REFERENCE.  AN IN & OUT PARAMETER.  
       ;"           For input, TEXT = HTML string to process.  This is expected to be
       ;"             the text for one section of HPI part of progress note
       ;"           For output -- FORMAT:
       ;"           TEXT(1)=part 1, e.g. text, e.g. [GROUP A&B]
       ;"                TEXT(1)="[GROUP]"
       ;"                TEXT(1,"GROUP")="A&B"
       ;"           TEXT(2)=part 2, e.g. name of inline table
       ;"                TEXT(2)="[TABLE]"  <-- signal this part is a table. 
       ;"                TEXT(2,"TABLE")=WT   <-- WT is name of table
       ;"                TEXT(2,"TEXT")=<TEXT OF TABLE>
       ;"                TEXT(2,"INLINE")=0 or 1
       ;"           TEXT(3)=part 3, e.g. more text
       ;"           TEXT(4)=part 4, e.g. name of table
       ;"           TEXT("GROUPX",#)=""  <-- index of GROUP nodes
       ;"           TEXT("TABLEX",#)=""  <-- index of TABLE nodes
       ;"           ... etc. 
       ;"       TABLES -- OPTIONAL.  PASS BY REFERENCE.  Allows reuse from prior calls.
       NEW OPTION SET OPTION("HTML")=1
       IF '$DATA(TABLES) DO GETTABLS(.TABLES,.OPTION) ;"Get list of all defined text tables, minus protected ones
       NEW IDX SET IDX=0
       NEW STARTPOS SET STARTPOS=0
       NEW STR SET STR=TEXT
       FOR  QUIT:STR=""  DO
       . NEW DIV SET DIV=$$NEXTCH^TMGSTUT3(STR,STARTPOS,"-- [","--&nbsp;[","[","{E-Scribe}")
       . ;"FUTURE EDDIE - HERE IS WHERE YOU NEED TO LOOK FOR THE MISCOUNT WHEN [] ARE USED. LOVE PRESENT EDDIE
       . IF DIV="" DO  QUIT
       . . SET IDX=IDX+1,TEXT(IDX)=$$REPLSTR^TMGSTUT3(STR,"&nbsp;"," ")
       . . SET STR="",STARTPOS=0 
       . NEW STRA,STRA2,STRB SET STRA=""
       . IF STARTPOS>0 DO
       . . SET STRA=$EXTRACT(STR,1,STARTPOS-1)
       . . ;"SET STR=$EXTRACT(STR,STARTPOS,$LENGTH(STR))
       . SET STRA=STRA_$PIECE(STR,DIV,1) 
       . IF STRA'="" DO  QUIT
       . . SET STRA2=$$REPLSTR^TMGSTUT3(STRA,"&nbsp;"," ")
       . . SET IDX=IDX+1,TEXT(IDX)=STRA2
       . . SET STR=$EXTRACT(STR,$LENGTH(STRA)+1,$LENGTH(STR))
       . . SET STARTPOS=0
       . ;"At this point, DIV starts at the first character
       . NEW TEMP SET TEMP=$PIECE($PIECE(STR,"]",1),DIV,2)
       . NEW NAME SET NAME=$$REPLSTR^TMGSTUT3(TEMP,"&nbsp;"," ")
       . IF ($PIECE(NAME," ",1)="GROUP")&($L(NAME," ")<6) DO  QUIT
       . . SET STRA=DIV_TEMP_"]",STRA2=DIV_NAME_"]"
       . . SET IDX=IDX+1,TEXT(IDX)="[GROUP]"  ;"STRA2
       . . NEW GRPLIST SET GRPLIST=$PIECE(NAME," ",2,99)
       . . ;"above commented to allow only one "word" as group so a missing bracket doesn't cause chaos
       . . ;"NEW GRPLIST SET GRPLIST=$PIECE(NAME," ",2)
       . . SET TEXT(IDX,"GROUP")=GRPLIST
       . . SET GRPLIST=$$HTML2TXS^TMGHTM1(GRPLIST)
       . . FOR  QUIT:GRPLIST=""  DO
       . . . NEW DIV SET DIV=$$NEXTCH^TMGSTUT3(GRPLIST,0," ",",",";","&")
       . . . NEW STRA
       . . . IF DIV="" SET STRA=GRPLIST
       . . . ELSE  DO
       . . . . SET STRA=$PIECE(GRPLIST,DIV,1)
       . . . SET GRPLIST=$EXTRACT(GRPLIST,$LENGTH(STRA_DIV)+1,$LENGTH(GRPLIST))
       . . . SET STRA=$$TRIM^XLFSTR(STRA) QUIT:STRA=""
       . . . SET TEXT(IDX,"GROUP","LIST",STRA)=""
       . . SET TEXT("GROUPX",IDX)=""
       . . SET STR=$EXTRACT(STR,$LENGTH(STRA)+1,$LENGTH(STR))
       . . SET STARTPOS=0
       . IF ($PIECE(NAME," ",1)="GROUP")&($L(NAME," ")'<6) DO  QUIT
       . . SET STR=$EXTRACT(STR,$LENGTH("GROUP ")+1,$LENGTH(STR))
       . IF ($DATA(TABLES("["_$$UP^XLFSTR(NAME)_"]")))!($DATA(TABLES("["_NAME_"]"))) DO  QUIT
       . . SET IDX=IDX+1,TEXT(IDX)="[TABLE]"
       . . SET TEXT(IDX,"TABLE")=NAME
       . . NEW INLINE SET INLINE=$$ISINLINE^TMGTIUO6(NAME)
       . . SET TEXT(IDX,"INLINE")=INLINE
       . . IF INLINE DO
       . . . NEW P2 SET P2=$FIND(STR,"]]")
       . . . IF P2=0 DO  QUIT  ;"end of inline table not found. 
       . . . . SET STRA=STR,STR=""
       . . . . SET TEXT(IDX,"TEXT")=STRA
       . . . SET STRA=$EXTRACT(STR,1,P2-1)
       . . . SET TEXT(IDX,"TEXT")=STRA
       . . . SET STR=$EXTRACT(STR,$LENGTH(STRA)+1,$LENGTH(STR))
       . . . SET STARTPOS=0
       . . ELSE  DO  ;"standard table.  
       . . . IF $$TABLEND(.STR,.STRB) DO
       . . . . NEW LENA SET LENA=$LENGTH(STR)-$LENGTH(STRB)
       . . . . SET STRA=$EXTRACT(STR,1,LENA),STR=STRB
       . . . ELSE  DO
       . . . . SET STRA=STR,STR=""
       . . . SET TEXT(IDX,"TEXT")=STRA
       . . SET STARTPOS=0
       . DO
       . . ;"Notes: we have run into an issue where bracketed portions of
       . . ;"       the HPI, which are not tables... are causing missing
       . . ;"       data. For example, "Patient's foot [right] is swollen"
       . . ;"       will results is duplicated text and then corrupt any other tables in this
       . . ;"       section. The following is an attempt to ignore this text
       . . ;"       and pass it back out.  3/3/20
       . . ;" I don't believe the ELSE is needed, but I want to test this
       . . ;"     for several days in action to ensure it isn't
       . . IF (DIV="[")&('$DATA(TABLES("["_NAME_"]"))) DO
       . . . NEW TEMPPOS SET TEMPPOS=$LENGTH("["_TEMP_"]")+1
       . . . NEW TEMPSTR
       . . . SET TEMPSTR=$E(STR,0,TEMPPOS),STR=$E(STR,TEMPPOS,$L(STR))
       . . . SET TEXT(IDX)=$G(TEXT(IDX))_TEMPSTR
       . . . SET STARTPOS=0
       . . ELSE  DO
       . . . SET STARTPOS=$LENGTH("["_TEMP_"]")+1
       QUIT
       ;
SAVAMED(TMGIN)  ;"SAVE ARRAY (containing a note) containing MEDS to 22733.2 
       ;"Purpose: This function scans through input text looking for [MEDICATION]
       ;"         table, and saves this to storage, for use with refreshing tables. 
       ;"Input:   TMGIN -- Input from client.  Format:
       ;"              TMGIN("DFN")=<DFN>  (IEN in PATIENT file)
       ;"              TMGIN("NoteIEN")=<TIUIEN>
       ;"              TMGIN("TEXT",1) = 1st line of text
       ;"              TMGIN("TEXT",2) = 2nd line of text, etc
       ;"Output: TMGIN will contain the text as sent, with changes
       ;"Result: none
       NEW MEDARR,DFN SET DFN=TMGIN("DFN")
       NEW TEMP MERGE TEMP=TMGIN("TEXT")
       DO XTRCTREF^TMGTIUO5("TEMP","[MEDICATIONS]","BLANK_LINE",.MEDARR)       
       DO FIXABVA^TMGTIUO6(.MEDARR)
       DO SORTMARR^TMGTIUO5(.MEDARR) ;"splits to KEY-VALUE pairs etc.              
       DO SAVETABL^TMGRX007(DFN,.MEDARR)  ;"SAVE MEDICATION TABLE.
       KILL TMGGSMEDLIST(DFN) ;"clear out any cached med table
       QUIT
       ;"       