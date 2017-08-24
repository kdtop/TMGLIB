TMGTIUP2 ;TMG/kst-TMG TIU NOTE PARSING FUNCTIONS ; 4/11/17; 6/26/17
         ;;1.0;TMG-LIB;**1,17**;4/11/17
 ;
 ;"Eddie Hagood
 ;"Kevin Toppenberg MD
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 4/11/17  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;"PUBLIC FUNCTIONS
 ;"=======================================================================
 ;"LASTTIU(DFN,SRCHTEXT)  -- FIND LAST NOTE WITH HPI SECTION
 ;"LASTHPI(DFN) -- Return the last HPI section, with processing, formatting etc.
 ;"GETHPI(IEN8925) -- Get HPI section as one long string, with processing, formatting etc.              
 ;"
 ;"=======================================================================
 ;"PRIVATE FUNCTIONS
 ;"=======================================================================
 ;"NEXT(STR,FIRST,SECOND) --  determine which delimiter comes first and Return it
 ;"TRAILTRM(STR)   
 ;"ENDTEXT(STR)  -- RETURN THE END OF THE TEXT (ACCOUNTING FOR SPACES AND TAGS)
 ;"ITALICS(SECTION)  --  This will remove then replace italics 
 ;"INLINE(SECTION) 
 ;"GROUP(IDX,TOPICS,NUMOFGROUPS)
 ;"GETLETTER(GRPNUMBER)  
 ;"FIXHTML(DOMNAME,ERR)  --A callback function for fixing HTML 
 ;"DELNODES(SRCH,DOCID,ERR)  
 ;"
 ;"=======================================================================
 ;"Dependancies :  
 ;"=======================================================================
TESTLHPI ;
        NEW X,Y,DIC SET DIC(0)="MAEQ",DIC=2
        DO ^DIC QUIT:+Y'>0
        WRITE $$LASTHPI(+Y)
        QUIT
        ;
T2()    ;
        NEW TIULASTOV 
        SET TIULASTOV=478071 ;"//C. HICK   NOTE: DON'T PUT FULL PATIENT NAMES HERE        
        QUIT $$GETHPI(TIULASTOV)
        ;       
LASTHPI(DFN)  ;"Return the last HPI section, with processing, formatting etc.
        ;"FIND LAST NOTE WITH HPI SECTION
        NEW TIULASTOV SET TIULASTOV=$$LASTTIU(DFN,"HISTORY OF PRESENT ILLNESS (HPI):")
        IF TIULASTOV=0 QUIT ""
        QUIT $$GETHPI(TIULASTOV)
        ;                   
LASTTIU(DFN,SRCHTEXT)  ;
        ;"FIND LAST NOTE WITH HPI SECTION
        NEW TIUIEN SET TIUIEN=9999999
        NEW TIULASTOV SET TIULASTOV=0
        FOR  SET TIUIEN=$ORDER(^TIU(8925,"C",DFN,TIUIEN),-1) QUIT:(TIUIEN'>0)!(TIULASTOV'=0)  DO
        . NEW IDX SET IDX=0
        . IF $P($G(^TIU(8925,TIUIEN,0)),"^",5)'=7 QUIT ;"ONLY USE SIGNED DOCS
        . NEW TEXT SET TEXT=""
        . FOR  SET IDX=$ORDER(^TIU(8925,TIUIEN,"TEXT",IDX)) QUIT:IDX'>0  DO
        . . SET TEXT=TEXT_$GET(^TIU(8925,TIUIEN,"TEXT",IDX,0))
        . IF TEXT'["HISTORY OF PRESENT ILLNESS (HPI):" QUIT
        . SET TIULASTOV=TIUIEN
        QUIT TIULASTOV
        ;              
GETHPI(IEN8925,ITEMARRAY,OUT) ;"Get HPI section as one long string, with processing, formatting etc.              
        ;"RUN PROCESS NOTE HERE
        NEW TMGHPI SET TMGHPI=""
        NEW IDX,TIUARRAY,PROCESSEDARR,OPTION SET IDX=0
        NEW DFN SET DFN=+$PIECE($GET(^TIU(8925,IEN8925,0)),"^",2)
        SET TIUARRAY("DFN")=DFN
        FOR  SET IDX=$ORDER(^TIU(8925,IEN8925,"TEXT",IDX)) QUIT:IDX'>0  DO
        . SET TIUARRAY("TEXT",IDX)=$GET(^TIU(8925,IEN8925,"TEXT",IDX,0))
        DO PROCESS^TMGTIUP3(.PROCESSEDARR,.TIUARRAY,1) ;
        NEW TEMP
        SET TEMP=$$PARSEARR(.PROCESSEDARR,.ITEMARRAY,.OPTION)  ;"Parse note array into formatted array
        IF TEMP'>0 SET TMGHPI=$PIECE(TEMP,"^",2) GOTO LHDN  ;"Return error message as HPI text
        IF DUZ=150 DO
        . SET TEMP=$$REFRSHTB(.ITEMARRAY,DFN)
        IF TEMP'>0 SET TMGHPI=$PIECE(TEMP,"^",2) GOTO LHDN  ;"Return error message as HPI text
        SET OPTION("BULLETS")=(+$GET(DUZ)'=83)                
        NEW ZZTMG 
        SET ZZTMG=(+$GET(DUZ)=168)
        SET ZZTMG=1  ;"<--- REMOVE TO PUT DR DEE BACK TO PRIOR METHOD.  
        IF ZZTMG=1 DO  GOTO LHDN
        . SET OPTION("TRAILING <BR>")=1  ;"Add blank line to end of each section
        . SET TMGHPI=$$COMPHPI(.ITEMARRAY,.OPTION,.OUT)  ;"COMPILE HPI    //kt 7/10/17 
        ELSE  DO
        . SET TMGHPI=$$COMPHPI0(.ITEMARRAY,.OPTION,.OUT)  ;"EDDIE'S WORKING COMPILER OF HPI    
LHDN    QUIT TMGHPI
        ;
REFRSHTB(ITEMARRAY,DFN)  ;"REFRESH TABLES. 
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
        . . . . SET TABLESTR=$$GETTABLX^TMGTIUOJ(DFN,TABLENAME,.TABLEARR)
        . . . . SET ITEMARRAY("TEXT",PARAIDX,JDX,"TEXT")=TABLESTR
        QUIT TMGRESULT
        ;
PARSEARR(TIUARRAY,ITEMARRAY,OPTION)  ;"Parse note array into formatted array 
        ;"NOTE: See also TRIGGER1^TMGC0Q04 -> SUMNOTE^TMGTIUP1 --> PARSESCT^TMGTIUP1 for summarizing notes
        ;"Input: TIUARRAY -- PASS BY REFERENCE.  FORMAT:
        ;"          TIUARRAY(#)=<note text>  <-- array holds ENTIRE typical TMG note
        ;"       ITEMARRAY -- PASS BY REFERENCE.  AN OUT PARAMETER. FORMAT:
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
        ;"       OPTION -PASS BY REFERENCE.  AN OUT PARAMETER. FORMAT:
        ;"          OPTION("AUTOGROUPING") = 0 OR 1
        ;"          OPTION("NUMOFGROUPS") =
        ;"          OPTION("GROUPING") =
        ;"Result: 1^OK, or -1^Error message
        NEW TMGRESULT SET TMGRESULT="1^OK"
        ;        
        ;"NOTE: I encountered situation where there were so many <FONT ..> tags, that
        ;"      the DOM processor (below) was blowing the stack. So this is a very
        ;"      crude solution for that problem.
        DO KILLFONT(.TIUARRAY)  
        ;
        ;"PROCESS NOTE VIA HTML DOM, using callback function.
        NEW TEMP SET TEMP=$$PROCESS^TMGHTM2(.TIUARRAY,"FIXHTML^TMGTIUP2")
        IF +TEMP<0 SET TMGHPI="ERROR: "_$PIECE(TEMP,"^",2,99) GOTO LHDN
        ;
        NEW IDX,TMGHPI SET TMGHPI=""
        ;"CONVERT ENTIRE NOTE INTO LONG STRING (TMGHPI)
        SET IDX=0 FOR  SET IDX=$ORDER(TIUARRAY(IDX)) QUIT:IDX'>0  DO
        . SET TMGHPI=TMGHPI_$GET(TIUARRAY(IDX))
        IF TMGHPI="" SET TMGRESULT="-1^No text for not found for processing." GOTO PRSDN
        ;
        ;"GET GROUPING SIGNAL, IF PRESENT
        SET OPTION("AUTOGROUPING")=0
        SET OPTION("NUMOFGROUPS")=0
        IF TMGHPI["[GROUP AUTO " DO
        . SET OPTION("NUMOFGROUPS")=+$P($P(TMGHPI,"[GROUP AUTO ",2),"]",1)
        . IF OPTION("NUMOFGROUPS")>0 SET OPTION("AUTOGROUPING")=1
        ;
        ;"EXTRACT JUST HPI PART
        ;"== SET UP MARKERS FOR BEGINNING AND ENDING OF DESIRED HPI SECTION =======
        NEW STARTARR,ENDARR
        SET STARTARR("<b>HISTORY OF PRESENT ILLNESS (HPI)</b>")=""
        SET STARTARR("<B>HISTORY OF PRESENT ILLNESS (HPI)</B>")=""
        SET STARTARR("HISTORY OF PRESENT ILLNESS (HPI)")=""
        ;"SET STARTARR(<MORE HERE IF NEEDED>) ...------------------------
        SET ENDARR("<STRONG>PAST MEDICAL HISTORY (PMH)")=""
        SET ENDARR("<B>PAST MEDICAL HISTORY (PMH)")=""
        SET ENDARR("<b>PAST MEDICAL HISTORY (PMH)")=""
        SET ENDARR("PAST MEDICAL HISTORY (PMH)")=""
        ;"SET ENDARR(<MORE HERE IF NEEDED>)... --------------------------
        ;
        NEW STARTDIV,ENDDIV SET (STARTDIV,ENDDIV)=""
        FOR  SET STARTDIV=$ORDER(STARTARR(STARTDIV))  QUIT:TMGHPI[STARTDIV  ;"when STARTDIV="", <text>["" is always TRUE
        IF STARTDIV="" SET TMGRESULT="-1^Unable to find 'HISTORY OF PRESENT ILLNESS (HPI)' to start getting HPI" GOTO PRSDN
        FOR  SET ENDDIV=$ORDER(ENDARR(ENDDIV))  QUIT:TMGHPI[ENDDIV  ;"when ENDDIV="", <text>["" is always TRUE
        IF ENDDIV="" SET TMGRESULT="-1^Unable to find 'PAST MEDICAL HISTORY (PMH)' as end of HPI section" GOTO PRSDN
        SET TMGHPI=$PIECE(TMGHPI,STARTDIV,2)
        SET TMGHPI=$PIECE(TMGHPI,ENDDIV,1)
        ;"
        IF TMGHPI="" SET TMGRESULT="-1^No text for HPI found between opening and closing markers." GOTO PRSDN
        ;"
        SET TMGHPI=$$UPTAGS^TMGHTM2(TMGHPI)  ;"force all tags to UPPER CASE
        ;"Remove/replace unwanted tags / strings from note  
        DO RPTAGS^TMGHTM1(.TMGHPI,"<BR />","<BR>")
        DO RPTAGS^TMGHTM1(.TMGHPI,"<P />","<P>")
        DO RMTAGS^TMGHTM1(.TMGHPI,"=== HPI ISSUES BELOW WERE NOT ADDRESSED TODAY ===")
        DO RMTAGS^TMGHTM1(.TMGHPI,"--&nbsp;[FOLLOWUP&nbsp;ITEMS]&nbsp;---------")
        DO RMTAGS^TMGHTM1(.TMGHPI,"-- [FOLLOWUP ITEMS] ---------")
        DO RPTAGS^TMGHTM1(.TMGHPI,"<LI>  <P>","<LI> ")
        DO RMTAGS^TMGHTM1(.TMGHPI,"<I>")   ;"//kt should have been already removed via DOM processing
        DO RMTAGS^TMGHTM1(.TMGHPI,"<EM>")  ;"//kt should have been already removed via DOM processing
        DO RMTAGS^TMGHTM1(.TMGHPI,"</I>")  ;"//kt should have been already removed via DOM processing
        DO RMTAGS^TMGHTM1(.TMGHPI,"</EM>") ;"//kt should have been already removed via DOM processing         
        ;"                           
        ;"Parse Items
        NEW TABLES,SECTION SET IDX=1       
        NEW DELIMITER SET DELIMITER=$SELECT(TMGHPI["<LI>":"<LI>",1:"*") 
        SET TMGHPI=$P(TMGHPI,DELIMITER,2,999)                            
        NEW PREVFOUND SET PREVFOUND=0
        SET OPTION("GROUPING")=0
        FOR  QUIT:TMGHPI=""  DO  
        . NEW SECTION SET SECTION=$P(TMGHPI,DELIMITER,1)
        . NEW TITLE,TEXTARR DO SPLITTL(SECTION,.TITLE,.TEXTARR,.TABLES) ;"return title of section  
        . IF TMGHPI[DELIMITER SET TMGHPI=$P(TMGHPI,DELIMITER,2,999)
        . ELSE  SET TMGHPI=""
        . IF TITLE["ALLERGIES" QUIT
        . SET SECTION=$$TRIM^XLFSTR(SECTION)
        . DO RMTAGS^TMGHTM1(.SECTION,"</LI>")
        . IF (SECTION="")!(SECTION="<P>")!(SECTION="<BR>")!(SECTION="<BR><BR>")!(SECTION="<BR></P>") DO
        . . ;Skip section
        . ELSE  DO        
        . . SET SECTION=$$HTMLTRIM^TMGHTM1(SECTION,"LR")
        . . SET SECTION=$$TRAILTRM(SECTION)
        . . SET SECTION=$$ITALICS(SECTION)
        . . SET ITEMARRAY(IDX)=SECTION
        . . MERGE ITEMARRAY("TEXT",IDX)=TEXTARR 
        . . SET ITEMARRAY("TEXT",IDX)=TITLE 
        . . SET IDX=IDX+1
        . . IF $$UP^XLFSTR(SECTION)["PREVENT" SET PREVFOUND=1
        . . IF $$UP^XLFSTR(SECTION)["[GROUP" SET OPTION("GROUPING")=1
        . . IF $$UP^XLFSTR(SECTION)["(GROUP" SET OPTION("GROUPING")=1
        IF PREVFOUND=0 DO  ;"if prevention section not found, add blank one
        . SET ITEMARRAY(IDX)="<U>Prevention</U>: (data needed)"
        . SET IDX=IDX+1
PRSDN   QUIT TMGRESULT
        ;
COMPHPI0(ITEMARRAY,OPTION,OUT)  ;"EDDIE'S WORKING COMPILER OF HPI
        ;"Purpose: Reassemble ordered list, removing undesired sections
        ;"INPUT: ITEMARRAY -- PASS BY REFERENCE.  FORMAT -- SEE PARSEARR() above        
        ;"       OPTION -PASS BY REFERENCE.  FORMAT:
        ;"          OPTION("AUTOGROUPING") = 0 OR 1
        ;"          OPTION("NUMOFGROUPS") =
        ;"          OPTION("GROUPING") =
        ;"          OPTION("BULLETS") = 0 OR 1
        ;"       OUT -- PASS BY REFERENCE.  OPTIONAL.  Will get back formatted array with structured HPI.
        ;"           OUT(#)=<TEXT>
        ;"           OUT=<LINE COUNT>
        ;"Result: Returns HPI section as one long string.  
        NEW TOPICS SET TOPICS=+$ORDER(ITEMARRAY(""),-1)
        NEW WARNING SET WARNING=(TOPICS>10)
        NEW AUTOGROUPING SET AUTOGROUPING=+$GET(OPTION("AUTOGROUPING"))
        NEW NUMOFGROUPS  SET NUMOFGROUPS=$GET(OPTION("NUMOFGROUPS"))
        NEW GROUPING SET GROUPING=$GET(OPTION("GROUPING"))
        NEW BULLETS SET BULLETS=$GET(OPTION("BULLETS"))
        NEW TMGHPI SET TMGHPI=""
        IF GROUPING=1 SET AUTOGROUPING=0  ;"IF ALREADY GROUPING, DON'T ATTEMPT TO AUTOGROUP
        ;
        IF (WARNING=1)&(GROUPING=0)&(+$GET(DUZ)'=83) DO
        . SET TMGHPI="{HTML:<B><FONT style=""BACKGROUND-COLOR:#ff0000"">}CONSIDER GROUPING. PATIENT HAS "_TOPICS_" TOPICS.{HTML:</B></FONT>}"
        ELSE  DO
        . SET TMGHPI=""
        IF BULLETS=1 DO
        . SET TMGHPI=TMGHPI_"<UL>"
        ELSE  DO
        . SET TMGHPI=TMGHPI_""
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(ITEMARRAY(IDX)) QUIT:IDX'>0  DO
        . IF $GET(ITEMARRAY(IDX))["<U>ALLERGIES</U>" QUIT
        . IF BULLETS=1 DO
        . . IF AUTOGROUPING=1 DO
        . . . NEW BRKTAG SET BRKTAG=":"
        . . . IF $G(ITEMARRAY(IDX))["</U>:" SET BRKTAG="</U>:"
        . . . IF $G(ITEMARRAY(IDX))["</U> :" SET BRKTAG="</U> :"
        . . . SET TMGHPI=TMGHPI_"<LI>"_$$TRIM^XLFSTR($P($GET(ITEMARRAY(IDX)),BRKTAG,1))_BRKTAG_$$GROUP(IDX,TOPICS,NUMOFGROUPS)_$$TRIM^XLFSTR($P($GET(ITEMARRAY(IDX)),BRKTAG,2))_"</LI>"
        . . ELSE  DO
        . . . SET TMGHPI=TMGHPI_"<LI>"_$GET(ITEMARRAY(IDX))_"</LI>"
        . ELSE  DO
        . . SET TMGHPI=TMGHPI_"     * "_$GET(ITEMARRAY(IDX))_"<P>"
        IF BULLETS=1 SET TMGHPI=TMGHPI_"</UL>"
        QUIT TMGHPI
        ;
COMPHPI(ITEMARRAY,OPTION,OUT)  ;"COMPILE HPI    //kt 7/10/17  
        ;"Input: ITEMARRAY -- PASS BY REFERENCE.  Format:
        ;"            ITEMARRAY(Ref#)=<Full section text>
        ;"            ITEMARRAY("TITLE",Ref#)=<SECTION TITLE>
        ;"            ITEMARRAY("TEXT",Ref#)=Text of section without title.  
        ;"            ITEMARRAY("TEXT",Ref#,...)=Text of section without title.  
        ;"       OPTION -PASS BY REFERENCE.  FORMAT:
        ;"          OPTION("AUTOGROUPING") = 
        ;"          OPTION("NUMOFGROUPS") =
        ;"          OPTION("GROUPING") =
        ;"          OPTION("BULLETS") = 0 OR 1
        ;"       OUT -- PASS BY REFERENCE.  OPTIONAL.  Will get back formatted array with structured HPI.
        ;"           OUT(#)=<TEXT>
        ;"           OUT=<LINE COUNT>
        ;"Results: returns one long string comprising HPI
        NEW AUTOGROUPING SET AUTOGROUPING=+$GET(OPTION("AUTOGROUPING"))
        NEW NUMOFGROUPS  SET NUMOFGROUPS=$GET(OPTION("NUMOFGROUPS"))
        NEW GROUPING SET GROUPING=$GET(OPTION("GROUPING"))
        NEW BULLETS SET BULLETS=$GET(OPTION("BULLETS"))
        NEW ADDBR SET ADDBR=+$GET(OPTION("TRAILING <BR>"))
        NEW TMGHPI SET TMGHPI=""
        IF GROUPING=1 SET AUTOGROUPING=0  ;"IF ALREADY GROUPING, DON'T ATTEMPT TO AUTOGROUP
        ;
        NEW IDX,SECTIONCT SET SECTIONCT=0,IDX=0
        FOR  SET IDX=$ORDER(ITEMARRAY(IDX)) QUIT:IDX'>0  SET SECTIONCT=SECTIONCT+1
        NEW WARNING SET WARNING=(SECTIONCT>10)&'GROUPING      
        NEW DELIM DO SUDELIM(.DELIM) 
        ;
        NEW TMGHPI SET TMGHPI=""
        IF (WARNING=1)&(GROUPING=0)&(+$GET(DUZ)'=83) DO
        . SET TMGHPI=TMGHPI_$$ADDSTR($$GRPNGSTR(SECTIONCT),.OUT)  
        IF BULLETS SET TMGHPI=TMGHPI_$$ADDSTR("<UL>",.OUT)  ;"//add to TMGHPI string and OUT array
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(ITEMARRAY("TEXT",IDX)) QUIT:IDX'>0  DO
        . NEW TITLE SET TITLE=$GET(ITEMARRAY("TEXT",IDX))
        . NEW LINE SET LINE=DELIM(BULLETS,"START")_$$FORMATTL(TITLE)  ;"FORMAT TITLE
        . IF AUTOGROUPING>0 SET LINE=LINE_$$GROUP(IDX,SECTIONCT,NUMOFGROUPS)_" "
        . NEW TEXTARR MERGE TEXTARR=ITEMARRAY("TEXT",IDX) 
        . NEW LASTSECT SET LASTSECT=""
        . NEW PART SET PART=0
        . FOR  SET PART=$ORDER(TEXTARR(PART)) QUIT:PART'>0  DO
        . . NEW STR SET STR=$GET(TEXTARR(PART)) QUIT:STR=""
        . . IF STR="[GROUP]" DO  QUIT
        . . . SET LINE=LINE_"[GROUP "_$GET(TEXTARR(PART,"GROUP"))_"] "
        . . . SET LASTSECT="GROUP"
        . . ELSE  IF STR="[TABLE]" DO  QUIT
        . . . NEW INLINE SET INLINE=+$GET(TEXTARR(PART,"INLINE"))
        . . . IF 'INLINE SET LINE=LINE_"<BR><BR>"
        . . . SET LINE=LINE_$GET(TEXTARR(PART,"TEXT"))_" "
        . . . IF 'INLINE SET LINE=LINE_"<BR>"
        . . . SET LASTSECT="TABLE"
        . . DO  QUIT
        . . . NEW TEXT SET TEXT=$GET(TEXTARR(PART))
        . . . SET LINE=LINE_$$FORMATTX(TEXT) ;"FORMAT BODY TEXT OF ONE SECION
        . . . SET LASTSECT="TEXT"
        . IF ADDBR,LASTSECT="TEXT" SET LINE=LINE_"<BR>"
        . SET LINE=LINE_DELIM(BULLETS,"END")        
        . SET TMGHPI=TMGHPI_$$ADDSTR(LINE,.OUT)  ;"//add to TMGHPI string and OUT array
        IF BULLETS SET TMGHPI=TMGHPI_$$ADDSTR("</UL>",.OUT)
        QUIT TMGHPI
        ;
GRPNGSTR(SECTIONCT) ;"GET GROUPING STRING
       NEW TEMP SET TEMP="{HTML:<B><FONT style=""BACKGROUND-COLOR:#ff0000"">}CONSIDER GROUPING."
       SET TEMP=TEMP_"PATIENT HAS "_SECTIONCT_" TOPICS.{HTML:</B></FONT>}"
       QUIT TEMP
       ;
ADDSTR(STR,OUT)  ;"ADD STR TO OUTPUT ARRAY, AND RETURN INPUT
        SET OUT=+$GET(OUT)+1,OUT(OUT)=STR
        QUIT STR
        ;
NEXT(STR,FIRST,SECOND) ;"
        ;"Purpose: Check string to determine which delimiter comes first and
        ;"         Return it
        ;"NOTE: this function is similar to NEXTCH^TMGSTUT3()
        NEW TMGRESULT
        NEW POS1 SET POS1=$F(STR,FIRST)
        NEW POS2 SET POS2=$F(STR,SECOND)
        IF (POS1=0)&(POS2>0) SET TMGRESULT=SECOND GOTO NXDN
        IF (POS2=0)&(POS1>0) SET TMGRESULT=FIRST GOTO NXDN
        IF POS1<POS2 DO
        . SET TMGRESULT=FIRST
        ELSE  DO
        . SET TMGRESULT=SECOND
NXDN    QUIT TMGRESULT
        ;
SUDELIM(ARR) ;"//kt 7/10/17
        SET ARR(0,"START")="&nbsp;&nbsp;&nbsp;&nbsp;* " 
        SET ARR(0,"END")="<P>" 
        SET ARR(1,"START")="<LI>" 
        SET ARR(1,"END")="</LI>" 
        QUIT
        ;
SPLITTL(SECTION,TITLE,TEXTARR,TABLES) ;"Split title and main text of section, and parse section into parts
        ;"Input: SECTION -- the text to be parsed
        ;"       TITLE -- PASS BY REFERENCE.  AN OUT PARAMETER.  This is section TITLE  
        ;"       TEXTARR -- PASS BY REFERENCE.  AN OUT PARAMETER.  This is SECTION with title stripped, and cleaned.  
        ;"       TABLES -- OPTIONAL.  PASS BY REFERENCE.  Allows reuse from prior calls.  
        ;"Results: NONE 
        NEW DIV SET DIV=$$NEXTCH^TMGSTUT3(SECTION,0,":",".","--","---","----")  ;"can add up to 7 strs to check for
        IF DIV'="" DO
        . NEW POS SET POS=$FIND(SECTION,DIV)-$LENGTH(DIV)
        . SET TITLE=$EXTRACT(SECTION,1,POS-1) 
        . SET TEXTARR=$$TRIM^XLFSTR($PIECE(SECTION,DIV,2,999))
        ELSE  DO   ;"None of the dividers were found
        . NEW CUTLEN SET CUTLEN=30
        . NEW LEN SET LEN=$LENGTH(SECTION)
        . NEW POS SET POS=$SELECT(LEN>CUTLEN:CUTLEN,1:LEN)
        . SET TITLE=$EXTRACT(SECTION,1,POS) 
        . SET TEXTARR=$EXTRACT(SECTION,POS+1,$LENGTH(SECTION))
        DO PRCSSTXT(.TEXTARR,.TABLES)  ;"//process and parse text into array, handling tables
        SET TITLE=$$STRIPTAG^TMGHTM1(TITLE)
        QUIT
        ;
FORMATTL(TITLE)  ;"FORMAT TITLE
        SET TITLE=$$HTMLTRIM^TMGHTM1(TITLE)
        QUIT "<U>"_TITLE_"</U>"_": "
        ;
FORMATTX(TEXT) ;"FORMAT BODY TEXT OF ONE SECION
        NEW TMGRESULT 
        SET TMGRESULT="<I>"_TEXT_"</I>"_"... "
        ;"SET TMGRESULT=$$ITALICS(TEXT)    ;"This will remove the italics and add one single
        QUIT TMGRESULT
        ;
PRCSSTXT(TEXTARR,TABLES)  ;"Process, parse, clean text for one section for unmatching tags etc.
        ;"INPUT: TEXTARR -- PASS BY REFERENCE.  AN OUT PARAMETER.  FORMAT:
        ;"           TEXTARR(1)=part 1, e.g. text, e.g. [GROUP A&B]
        ;"                TEXTARR(1)="[GROUP]"
        ;"                TEXTARR(1,"GROUP")="A&B"
        ;"           TEXTARR(2)=part 2, e.g. name of inline table
        ;"                TEXTARR(2)="[TABLE]"  <-- signal this part is a table. 
        ;"                TEXTARR(2,"TABLE")=WT   <-- WT is name of table
        ;"                TEXTARR(2,"TEXT")=<TEXT OF TABLE>
        ;"           TEXTARR(3)=part 3, e.g. more text
        ;"           ... etc. 
        ;"       TABLES -- OPTIONAL.  PASS BY REFERENCE.  Allows reuse from prior calls.  
        ;"Results: none
        DO RPTAGS^TMGHTM1(.TEXTARR,"<P>","<BR>") DO RMTAGS^TMGHTM1(.TEXTARR,"</P>") ;"convert <P>...</P> into <BR>...
        DO RMTAGS^TMGHTM1(.TEXTARR,"<LI>") 
        DO RMTAGS^TMGHTM1(.TEXTARR,"</LI>")
        SET TEXTARR=$$MATCHTAG^TMGHTM1(TEXTARR) ;"ENSURE MATCHING OPEN/CLOSE TAGS
        DO PRTIUHTM^TMGTIUP3(.TEXTARR,.TABLES)  ;"PARSE TEXTARR into parts, handling tables.  
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(TEXTARR(IDX)) QUIT:IDX'>0  DO
        . NEW REF,STR SET STR=$GET(TEXTARR(IDX))
        . IF STR="[GROUP]" SET REF=$NAME(TEXTARR(IDX,"GROUP"))
        . ELSE  IF STR="[TABLE]" SET REF=$NAME(TEXTARR(IDX,"TEXT"))
        . ELSE  SET REF=$NAME(TEXTARR(IDX))
        . SET STR=$GET(@REF)
        . SET STR=$$HTMLTRIM^TMGHTM1(STR)
        . ;"SET STR=$$MATCHTAG^TMGHTM1(STR) ;"<-- NO, was causing problems when open/close split across different lines. 
        . SET STR=$$TRAILTRM(STR)
        . SET @REF=STR
        SET TEXTARR=""
        QUIT
        ;
TRAILTRM(STR)  ;"TRIM FROM TRAILING PART OF STR
        NEW TRIMARR,FRAG SET FRAG=""
        SET TRIMARR("<P>")=""
        SET TRIMARR("<BR>")=""
        SET TRIMARR("</P>")=""
        SET TRIMARR("...")=""
        SET TRIMARR("</FONT>")=""
        SET TRIMARR(" ")=""
        NEW FOUND
        FOR  DO  QUIT:FOUND=0
        . SET FOUND=0
        . FOR  SET FRAG=$ORDER(TRIMARR(FRAG)) QUIT:FRAG=""  DO
        . . IF $$RMATCH^TMGSTUT3(STR,FRAG)=0 QUIT
        . . SET FOUND=1
        . . SET STR=$EXTRACT(STR,1,$LENGTH(STR)-$LENGTH(FRAG))
        QUIT STR
        ;"NEW TRIMMING 
TRIM    ;"SET STR=$$TRIM^XLFSTR(STR)
        ;"SET TRIMMING=0
        ;"IF $E(STR,$L(STR)-2,$L(STR))="<P>" DO
        ;". SET STR=$E(STR,1,$L(STR)-3)
        ;". SET TRIMMING=1
        ;"IF $E(STR,$L(STR)-3,$L(STR))="<BR>" DO
        ;". SET STR=$E(STR,1,$L(STR)-4)
        ;". SET TRIMMING=1
        ;"IF $E(STR,$L(STR)-3,$L(STR))="</P>" DO
        ;". SET STR=$E(STR,1,$L(STR)-4)
        ;". SET TRIMMING=1
        ;"IF $E(STR,$L(STR)-2,$L(STR))="..." DO
        ;". SET STR=$E(STR,1,$L(STR)-3)
        ;". SET TRIMMING=1      
        ;"IF $E(STR,$L(STR)-6,$L(STR))="</FONT>" DO
        ;". SET STR=$E(STR,1,$L(STR)-7)
        ;". SET TRIMMING=1
        ;"IF TRIMMING=1 GOTO TRIM    
BTDN    ;"QUIT STR
        ;"
ENDTEXT(STR)  ;"RETURN THE END OF THE TEXT (ACCOUNTING FOR SPACES AND TAGS)
        NEW TMGRESULT SET TMGRESULT=$L(STR)
        NEW TRIMMING
SRCH    SET STR=$$TRIM^XLFSTR(STR)
        SET TRIMMING=0
        IF $E(STR,$L(STR)-2,$L(STR))="<P>" DO
        . SET STR=$E(STR,1,$L(STR)-3)
        . SET TRIMMING=1
        IF $E(STR,$L(STR)-3,$L(STR))="<BR>" DO
        . SET STR=$E(STR,1,$L(STR)-4)
        . SET TRIMMING=1
        IF $E(STR,$L(STR)-3,$L(STR))="</P>" DO
        . SET STR=$E(STR,1,$L(STR)-4)
        . SET TRIMMING=1
        IF $E(STR,$L(STR)-6,$L(STR))="</FONT>" DO
        . SET STR=$E(STR,1,$L(STR)-7)
        . SET TRIMMING=1
        SET TMGRESULT=$L(STR)
        IF TRIMMING=1 GOTO SRCH
        QUIT TMGRESULT
        ;"
ITALICS(SECTION)    ;"This will remove the italics and add one single
        ;"set of italics for the entire section before any table is found
        ;"Remove existing italics tags
        DO RMTAGS^TMGHTM1(.SECTION,"<I>")   ;"//kt should have been already removed via DOM processing
        DO RMTAGS^TMGHTM1(.SECTION,"</I>")  ;"//kt should have been already removed via DOM processing
        DO RMTAGS^TMGHTM1(.SECTION,"<EM>")  ;"//kt should have been already removed via DOM processing
        DO RMTAGS^TMGHTM1(.SECTION,"</EM>") ;"//kt should have been already removed via DOM processing
        ;"Add end italics tags
        IF (SECTION["-- [")!(SECTION["--&nbsp;[") DO
        . NEW POS1,POS2,LEN
        . IF SECTION["-- [" DO
        . . SET POS2=$F(SECTION,"-- [")
        . . SET POS2=POS2-5
        . ELSE  DO
        . . SET POS2=$F(SECTION,"--&nbsp;[")
        . . SET POS2=POS2-10
        . NEW P1,P2 
        . SET P1=$E(SECTION,0,POS2)
        . SET POS1=$$ENDTEXT(P1)
        . SET P1=$E(SECTION,0,POS1)
        . SET P2=$E(SECTION,POS1+1,$L(SECTION))
        . IF $E(P1,$L(P1)-2,$L(P1))="..." DO
        . . SET P1=$E(P1,1,$L(P1)-3)
        . SET SECTION=P1_"</I>..."_P2_"<BR>"
        ELSE  DO
        . SET SECTION=SECTION_"</I>..."
        ;"Add beginning tags
        NEW POS SET POS=$F(SECTION,">:")
        IF POS'>0 DO
        . SET POS=$F(SECTION,":")
        IF POS>0 DO
        . NEW P1,P2 SET P1=$E(SECTION,0,POS-1),P2=$E(SECTION,POS,$L(SECTION))
        . SET SECTION=P1_"<I>"_P2        
        ELSE  DO
        . SET SECTION="<I>"_SECTION
        DO INLINE(.SECTION)
        QUIT SECTION
        ;"
INLINE(SECTION) ;"
        IF SECTION'["]]" GOTO ILDN
        SET SECTION=$P(SECTION,"[",1)_"</I>["_$P(SECTION,"[",2,999)
        SET SECTION=$P(SECTION,"]]",1)_"]]<I>"_$P(SECTION,"]]",2,999)
ILDN    QUIT SECTION
        ;"
GROUP(IDX,TOPICS,NUMOFGROUPS)
        IF NUMOFGROUPS>9 SET NUMOFGROUPS=9   ;"MAX ALLOWED
        NEW TMGRESULT SET TMGRESULT=""  ;""[GROUP A] "
        NEW NUMPERGRP SET NUMPERGRP=TOPICS/NUMOFGROUPS
        IF NUMPERGRP["." DO
        . NEW DEC SET DEC=$P(NUMOFGROUPS,".",2)
        . IF DEC>4 DO
        . . SET NUMPERGRP=NUMPERGRP+1
        NEW GRPNUMBER SET GRPNUMBER=$P(IDX/NUMPERGRP,".",1)+1
        IF GRPNUMBER>NUMOFGROUPS SET GRPNUMBER=NUMOFGROUPS
        SET TMGRESULT=" [GROUP "_$$GETLETTER(GRPNUMBER)_"] "
        QUIT TMGRESULT
        ;"
GETLETTER(GRPNUMBER)  ;"
        QUIT $TR(GRPNUMBER,"123456789","ABCDEFGHI")
        ;"
KILLFONT(TIUARRAY) ;"Kill certain FONT tags
        ;"Input: TIUARRAY -- PASS BY REFERENCE.  FORMAT:
        ;"          TIUARRAY(#)=<note text>  <-- array holds ENTIRE typical TMG note
        ;"Results: none
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(TIUARRAY(IDX)) QUIT:IDX'>0  DO
        . NEW LINE SET LINE=$GET(TIUARRAY(IDX)) QUIT:LINE=""
        . NEW INITLINE SET INITLINE=LINE
        . DO RMTAGS^TMGHTM1(.LINE,"<FONT size=3>")   
        . DO RMTAGS^TMGHTM1(.LINE,"<FONT size=1>")
        . IF LINE'=INITLINE SET TIUARRAY(IDX)=LINE
        QUIT
        ;
FIXHTML(DOMNAME,ERR)  ;"A callback function for fixing HTML 
        ;"Called from LASTHPI^TMGTIUP2() via PROCESS^TMGHTM2()   
        NEW DOCID SET DOCID=$$getDocumentNode^%zewdDOM(DOMNAME)
        IF DOCID="" QUIT
        DO DELNODES("//font",DOCID,.ERR)   ;"//REMOVE FONTS, e.g. <FONT size=3>  
        IF $GET(ERR)'="" QUIT
        DO DELNODES("//meta",DOCID,.ERR)   ;"//REMOVE <META name=GENERATOR content=""MSHTML 6.00.6000.17107"">"
        IF $GET(ERR)'="" QUIT
        DO DELNODES("//em",DOCID,.ERR)     ;"//REMOVE <EM  >
        IF $GET(ERR)'="" QUIT
        DO DELNODES("//i",DOCID,.ERR)      ;"//REMOVE <I>  
        IF $GET(ERR)'="" QUIT
        DO DELNODES("//strong",DOCID,.ERR) ;"//REMOVE <STRONG>
        IF $GET(ERR)'="" QUIT
        DO DELATTR("style",DOCID,.ERR)     ;"//REMOVE all nodes' style attribute  
        IF $GET(ERR)'="" QUIT
        DO SCRNCLAS(DOCID,.ERR)       
        IF $GET(ERR)'="" QUIT
        IF 1=0 DO  QUIT:($GET(ERR)'="")   ;"REMOVE TABLE TAGS. 
        . DO DELNODES("//table",DOCID,.ERR)  ;"//REMOVE <TABLE>  
        . IF $GET(ERR)'="" QUIT
        . DO DELNODES("//tr",DOCID,.ERR)     ;"//REMOVE <TR>  
        . IF $GET(ERR)'="" QUIT
        . DO DELNODES("//td",DOCID,.ERR)     ;"//REMOVE <TD>  
        . IF $GET(ERR)'="" QUIT
        . DO DELNODES("//caption",DOCID,.ERR)  ;"//REMOVE <CAPTION>  
        . IF $GET(ERR)'="" QUIT
        . DO DELNODES("//tbody",DOCID,.ERR)  ;"//REMOVE <TBODY>  
        . IF $GET(ERR)'="" QUIT
        ;"more here later if needed...
        QUIT
        ;
DELNODES(SRCH,DOCID,ERR)  ;
       NEW NODES,CT SET CT=$$select^%zewdXPath(SRCH,DOCID,.NODES)
       IF ERR'="" QUIT  
       NEW OID,OIDIDX SET OIDIDX=""
       FOR  SET OIDIDX=$ORDER(NODES(OIDIDX)) QUIT:(OIDIDX="")!(ERR'="")  DO
       . SET OID=$GET(NODES(OIDIDX)) QUIT:OID=""
       . SET ERR=$$removeIntermediateNode^%zewdDOM(OID,1)
       QUIT
       ;
DELATTR(ATTRNAME,DOCID,ERR)  ;       
       NEW SRCH SET SRCH="//*/attribute::"_ATTRNAME
       NEW NODES,CT SET CT=$$select^%zewdXPath(SRCH,DOCID,.NODES)
       IF ERR'="" QUIT  ;"IMPLEMENT ERROR HANDLING SOMEDAY...
       NEW OID,OIDIDX SET OIDIDX=""
       FOR  SET OIDIDX=$ORDER(NODES(OIDIDX)) QUIT:(OIDIDX="")!(ERR'="")  DO
       . SET OID=$GET(NODES(OIDIDX)) QUIT:OID=""
       . SET OID=$$getParentNode^%zewdDOM(OID)
       . DO removeAttribute^%zewdDOM(ATTRNAME,OID,1)
       . ;"DO setAttribute^%zewdDOM(ATTRNAME,"",OID)
       QUIT
       ;
SCRNCLAS(DOCID,ERR) ;       
       NEW SRCH SET SRCH="//*/attribute::class"
       NEW NODES,CT SET CT=$$select^%zewdXPath(SRCH,DOCID,.NODES)
       IF ERR'="" QUIT  ;"IMPLEMENT ERROR HANDLING SOMEDAY...
       NEW OID,OIDIDX SET OIDIDX=""
       FOR  SET OIDIDX=$ORDER(NODES(OIDIDX)) QUIT:(OIDIDX="")!(ERR'="")  DO
       . SET OID=$GET(NODES(OIDIDX)) QUIT:OID=""
       . SET OID=$$getParentNode^%zewdDOM(OID)
       . NEW CLASS SET CLASS=$$getAttribute^%zewdDOM("class",OID)
       . NEW CLASS2,IDX SET CLASS2=""
       . FOR IDX=1:1:$LENGTH(CLASS," ") DO 
       . . NEW ACLASS SET ACLASS=$$UP^XLFSTR($PIECE(CLASS," ",IDX))
       . . ;"NOTE: Line below  will effect deletion of all classes not starting with TMG namespace
       . . IF $$LMATCH^TMGSTUT3(ACLASS,"TMG")=0 QUIT
       . . IF CLASS2'="" SET CLASS2=CLASS2_" "
       . . SET CLASS2=CLASS2_ACLASS
       . IF CLASS2="" DO
       . . DO removeAttribute^%zewdDOM("class",OID,1)
       . ELSE  DO
       . . DO setAttribute^%zewdDOM("class",CLASS2,OID)
       QUIT
       ;
