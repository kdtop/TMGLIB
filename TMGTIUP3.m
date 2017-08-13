TMGTIUP3 ;TMG/kst-TIU processing Functions ;4/11/17, 6/26/17
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
 ;"PROCESS(TMGRESULT,TMGIN) -- Entrypoint for RPC to effect processing a note from CPRS
 ;"FRSHTABL(TMGRESULT,TMGIN,HTML) -- REFRESH TABLES
 ;"GETTABLS(TABLES,HTML) -- Get list of all defined text tables, minus protected ones
 ;"TABLEND(LINE,PARTB) --Determine if HTML-coded line includes the end of a table.
 ;"PRTIUHTM(TEXT)  --PARSE HTML IN TYPICAL FORMAT FOR FPG/TMG NOTES, INTO ARRAY (HANDLING TABLES)  
 ;"
 ;"=======================================================================
 ;"Dependancies:
 ;" TMGHTM1, TMGTIUOJ, TMGTIUO6, XLFSTR, TMGSTUT2, TMGSTUT3
 ;"=======================================================================
 ;
PROCESS(TMGRESULT,TMGIN,FORCE) ;
        ;"Purpose: Entrypoint for RPC to effect processing a note from CPRS
        ;"Input: TMGRESULT -- PASS BY REFERENCE, an OUT PARAMETER.
        ;"       TMGIN -- Input from client.  Format:
        ;"              TMGIN("DFN")=<DFN>  (IEN in PATIENT file)
        ;"              TMGIN("TEXT",1) = 1st line of text
        ;"              TMGIN("TEXT",2) = 2nd line of text, etc
        ;"       FORCE -- ADDED 12/12/16. If not sent, is set to 0
        ;"                If 1 is sent, note is processed even if
        ;"                tag is absent
        ;"Output: TMGRESULT(0)="1^Success", or "-1^Error Message"
        ;"        TMGRESULT(1)=1st line of return text
        ;"        TMGRESULT(2)=2nd line of return text, etc.
        ;"Result: none
        SET FORCE=+$GET(FORCE)
        KILL TMGRESULT
        NEW TMGZZ SET TMGZZ=0  ;"Set to 1 during debug IF needed.
        IF TMGZZ=1 DO
        . KILL TMGIN,TIU
        . MERGE TMGIN=^TMG("TMG","RPC","TMG CPRS PROCESS NOTE","TMGIN")        
        . MERGE TIU=^TMG("TMG","RPC","TMG CPRS PROCESS NOTE","TIU")        
        ELSE  DO
        . KILL ^TMG("TMG","RPC","TMG CPRS PROCESS NOTE")
        . MERGE ^TMG("TMG","RPC","TMG CPRS PROCESS NOTE","TMGIN")=TMGIN
        . MERGE ^TMG("TMG","RPC","TMG CPRS PROCESS NOTE","TIU")=TIU
        ;
        ;"REMOVE "PROCESS NOTE" REMINDER
        NEW NOTDONETAG SET NOTDONETAG="PROCESS NOTE NOT DONE"
        NEW LINENUM SET LINENUM=0
        NEW TEMPARRAY,LINETEXT,DONTPROCESS
        SET DONTPROCESS=1
        SET TEMPARRAY("DFN")=TMGIN("DFN")
        FOR  SET LINENUM=$ORDER(TMGIN("TEXT",LINENUM)) QUIT:LINENUM'>0  DO
        . SET LINETEXT=$GET(TMGIN("TEXT",LINENUM))
        . IF LINETEXT[NOTDONETAG DO   ;"//kt 9/11/13
        . . SET DONTPROCESS=0
        . . SET LINETEXT=$PIECE(LINETEXT,NOTDONETAG,1)_$PIECE(LINETEXT,NOTDONETAG,2)
        . SET TEMPARRAY("TEXT",LINENUM)=LINETEXT
        IF (DONTPROCESS=1)&(FORCE=0) DO  GOTO PRODN 
        . MERGE TMGRESULT=TMGIN("TEXT")
        . ;"DO SETPLAN(.TMGRESULT,.TMGIN)
        KILL TMGIN
        MERGE TMGIN=TEMPARRAY
        ;"DONE REMOVING REMINDER
        NEW HTML SET HTML=$$ISHTMREF^TMGHTM1($NAME(TMGIN("TEXT")))
        DO FRSHTABL(.TMGRESULT,.TMGIN,HTML) 
        ;"NOTE: Later, other tasks could also be done at this time. 
PRODN   QUIT
        ;        
FRSHTABL(TMGRESULT,TMGIN,HTML) ;"REFRESH TABLES
        ;"Input: TMGRESULT -- PASS BY REFERENCE, an OUT PARAMETER.
        ;"       TMGIN -- Input from client.  Format:
        ;"              TMGIN("DFN")=<DFN>  (IEN in PATIENT file)
        ;"              TMGIN("TEXT",1) = 1st line of text
        ;"              TMGIN("TEXT",2) = 2nd line of text, etc
        ;"       FORCE -- ADDED 12/12/16. If not sent, is set to 0
        ;"                If 1 is sent, note is processed even if
        ;"                tag is absent
        ;"       HTML -- 1 if note is in HTML format
        ;"Result: None
        SET TMGRESULT(0)="1^Success"
        NEW ATABLE,TABLES DO GETTABLS(.TABLES,HTML)
        NEW TMGDFN SET TMGDFN=+$GET(TMGIN("DFN"))
        NEW TERMINALCHARS SET TERMINALCHARS=""
        NEW FOUNDTABLE SET FOUNDTABLE=""
        NEW OUTLNUM SET OUTLNUM=0
        NEW TAGS
        NEW DONE SET DONE=0
        NEW INSCRIPT SET INSCRIPT=0
        NEW LNUM SET LNUM=$ORDER(TMGIN("TEXT",0))
        NEW INLINEMODE SET INLINEMODE=0
        ;"FOR  SET LNUM=$ORDER(TMGIN("TEXT",LNUM)) QUIT:(+LNUM'>0)!DONE  DO
        FOR  QUIT:(+LNUM'>0)!DONE  DO
        . NEW LINE SET LINE=$$GETNEXTL($NAME(TMGIN("TEXT")),.LNUM,.INSCRIPT,.TAGS) ;"Get next line
        . IF INSCRIPT DO  QUIT  ;"don't search for tables to refresh inside script javascript code
        . . SET OUTLNUM=OUTLNUM+1,TMGRESULT(OUTLNUM)=LINE
        . IF LNUM="" SET DONE=1
        . ;"NEW LINE SET LINE=$GET(TMGIN("TEXT",LNUM))
        . IF FOUNDTABLE'="" DO
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
        . . SET TABLESTR=$$GETTABLX^TMGTIUOJ(TMGDFN,FOUNDTABLE,.TABLEARR)
        . . NEW TEMPARR
        . . IF 'INLINEMODE DO
        . . . DO SPLIT2AR^TMGSTUT2(TABLESTR,$CHAR(13,10),.TEMPARR,OUTLNUM+1)
        . . . KILL TEMPARR("MAXNODE")
        . . . IF HTML DO 
        . . . . DO TXT2HTML^TMGHTM1(.TEMPARR)
        . . . ;"NEW IDX SET IDX="" FOR  SET IDX=$ORDER(TEMPARR(IDX)) QUIT:IDX=""  DO
        . . . ;". ;"SET TEMPARR(IDX)=TEMPARR(IDX)_$CHAR(13,10)
        . . . MERGE TMGRESULT=TEMPARR
        . . ELSE  DO
        . . . SET LINE=$$SYMENC^MXMLUTL(TABLESTR)_LINE
        . . SET OUTLNUM=$ORDER(TMGRESULT(""),-1)
        . . SET OUTLNUM=OUTLNUM+1,TMGRESULT(OUTLNUM)=LINE
        . . SET FOUNDTABLE=""
        . . SET TERMINALCHARS=""
        . . SET INLINEMODE=0
        . ELSE  DO
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
        . . . . . . SET INPARTB=$PIECE(LINE,TERMCHARS,2)
        . . . . . . NEW TABLESTR SET TABLESTR=$$GETTABLX^TMGTIUOJ(TMGDFN,ATABLE)
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
GETNEXTL(REFARR,IDX,INSCRIPT,TAGS) ;"Get next line
        ;"Input: REFARR -- pass by NAME <-- original array is modified during processing
        ;"       IDX -- PASS BY REFERENCE.
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
        
GETTABLS(TABLES,HTML) ;"Get list of all defined text tables, minus protected ones
        ;"Note: Some tables don't need refreshing becuase they are fresh from
        ;"      insertion as TIU TEXT OBJECTS.  These are removed from list.
        ;"Input: TABLES -- an OUT PARAMETER
        ;"       HTML -- 1 IF should be in HTML format.
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
        NEW LINE SET LINE=1
        NEW LABEL
        FOR  SET LABEL=$$TRIM^XLFSTR($PIECE($TEXT(GTL1+LINE^TMGTIUP3),";;",2)),LINE=LINE+1 QUIT:LABEL=""  DO
        . KILL TABLES("["_LABEL_"]")        
        IF HTML DO
        . NEW TEMPARR,TEMPARR2,LBL,IDX SET IDX=0
        . SET LBL="" FOR  SET LBL=$ORDER(TABLES(LBL)) QUIT:LBL=""  SET IDX=IDX+1,TEMPARR(IDX)=LBL
        . MERGE TEMPARR2=TEMPARR
        . DO TXT2HTML^TMGHTM1(.TEMPARR) KILL TABLES
        . SET IDX=0 FOR  SET IDX=$ORDER(TEMPARR(IDX)) QUIT:IDX=""  DO
        . . SET LBL=$GET(TEMPARR(IDX))
        . . SET LBL=$EXTRACT(LBL,1,$LENGTH(LBL)-4)
        . . SET TABLES(LBL)=$GET(TEMPARR2(IDX))
        . . NEW LBL2 SET LBL2=$$REPLSTR^TMGSTUT3(LBL," ","&nbsp;")   ;"//kt 8/1/17 
        . . SET TABLES(LBL2)=$GET(TEMPARR2(IDX))        ;"//kt 8/1/17
        . . SET TABLES($$HTML2TXS^TMGHTM1($GET(TEMPARR(IDX))))=$GET(TEMPARR2(IDX))
        ELSE  DO
        . NEW LBL SET LBL="" 
        . FOR  SET LBL=$ORDER(TABLES(LBL)) QUIT:LBL=""  SET TABLES(LBL)=LBL     
        KILL TABLES("[FOLLOWUP&nbsp;ITEMS]")
        KILL TABLES("[FOLLOWUP ITEMS]")
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
SETPLAN(TMGRESULT,TMGIN)  ;"-- NOT CURRENTLY USED (?)
        NEW LINENUM SET LINENUM=0 
        NEW FOUNDHPI SET FOUNDHPI=0
        NEW OUTLNUM SET OUTLNUM=0
        NEW TMPTEXTARR,FOUNDAP,LINETEXT,NEWNUM
        SET NEWNUM=0
        SET FOUNDAP=0
        FOR  SET LINENUM=$ORDER(TMGIN("TEXT",LINENUM)) QUIT:LINENUM'>0  DO
        . SET LINETEXT=$GET(TMGIN("TEXT",LINENUM))
        . IF FOUNDHPI=1 DO
        . . IF LINETEXT["PAST MEDICAL HISTORY" DO
        . . . SET FOUNDHPI=0
        . . ELSE  DO
        . . . SET OUTLNUM=OUTLNUM+1
        . . . SET TMPTEXTARR(OUTLNUM)=LINETEXT
        . . . SET NEWNUM=NEWNUM+1
        . . . SET TMGRESULT(NEWNUM)=LINETEXT
        . ELSE  IF FOUNDAP=1 DO
        . . NEW TMPCOUNT
        . . FOR TMPCOUNT=1:1:OUTLNUM DO
        . . . SET LINETEXT=$GET(TMPTEXTARR(TMPCOUNT))
        . . . SET NEWNUM=NEWNUM+1
        . . . SET TMGRESULT(NEWNUM)=LINETEXT
        . . SET FOUNDAP=0
        . ELSE  DO
        . . SET NEWNUM=NEWNUM+1
        . . SET TMGRESULT(NEWNUM)=LINETEXT
        . . IF LINETEXT["(HPI)" SET FOUNDHPI=1
        . . IF LINETEXT["PLAN:" SET FOUNDAP=1
        QUIT
        ;"
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
       IF '$DATA(TABLES) DO GETTABLS^TMGTIUP3(.TABLES,1) ;"Get list of all defined text tables, minus protected ones
       NEW IDX SET IDX=0
       NEW STARTPOS SET STARTPOS=0
       NEW STR SET STR=TEXT
       FOR  QUIT:STR=""  DO
       . NEW DIV SET DIV=$$NEXTCH^TMGSTUT3(STR,STARTPOS,"-- [","--&nbsp;[","[","{E-Scribe}")
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
       . IF $PIECE(NAME," ",1)="GROUP" DO  QUIT
       . . SET STRA=DIV_TEMP_"]",STRA2=DIV_NAME_"]"
       . . SET IDX=IDX+1,TEXT(IDX)="[GROUP]"  ;"STRA2
       . . NEW GRPLIST SET GRPLIST=$PIECE(NAME," ",2,99)
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
       . IF $DATA(TABLES("["_$$UP^XLFSTR(NAME)_"]")) DO  QUIT
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
       . . SET STARTPOS=$LENGTH("["_TEMP_"]")+1
       QUIT
       ;
