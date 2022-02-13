TMGTIUP2 ;TMG/kst-TMG TIU NOTE PARSING FUNCTIONS ; 10/18/17, 5/21/18, 3/24/21
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
 ;"LASTTIU(TMGDFN,SRCHTEXT)  -- FIND LAST NOTE WITH HPI SECTION
 ;"LASTHPI(TMGDFN) -- Return the last HPI section, with processing, formatting etc.
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
 ;"ALLGRPS(TMGHPI,DELIMITER)  --Return a list of all groups for a provided HPI
 ;"TOPICFORALL(.TOPIC4ALL)  --Set the array for all topics that should be included with all groups

 ;"
 ;"=======================================================================
 ;"Dependancies :  
 ;"=======================================================================
TESTLHPI ;
        NEW X,Y,DIC SET DIC(0)="MAEQ",DIC=2
        DO ^DIC QUIT:+Y'>0
        NEW % SET %=2
        WRITE !,"CONSIDER AWV VISIT" DO YN^DICN WRITE !
        IF %=-1 QUIT
        WRITE $$LASTHPI(+Y,%=1)
        QUIT
        ;
T2()    ;" NOTE: DON'T PUT FULL PATIENT NAMES HERE
        NEW TIULASTOV SET TIULASTOV=601420; //zzt,ba         
        NEW ITEMARRAY,OUT  ;"<-- For now, these arrays are not being used.  
        NEW OPTION SET OPTION("FORCE PROCESS")=1
        QUIT $$GETHPI(TIULASTOV,.ITEMARRAY,.OUT,.OPTION)
        ;       
LASTHPI(TMGDFN,AWV)  ;"Return the last HPI section, with processing, formatting etc.
        ;"FIND LAST NOTE WITH HPI SECTION
        ;"ADDED AWV AS AN OPTION TO BE USED WHEN COMPILING THE HPI, TO REORDER DIFFERENTLY
        NEW TIULASTOV SET TIULASTOV=$$LASTTIU(TMGDFN,"HISTORY OF PRESENT ILLNESS (HPI):")
        IF TIULASTOV=0 QUIT ""           
        NEW ITEMARRAY,OUT  ;"<-- For now, these arrays are not being used.  
        ;"NOTE: The processing done in $$GETHPI below will probably be later
        ;"   redone via Process Note. However, I will leave in because if we
        ;"   generate a partial note, i.e. bringing in JUST HPI, one would expect
        ;"   the tables to have the most up-to-date information, without having
        ;"   to do a Process Note to refresh them.  It shouldn't add too much
        ;"   computing burden. 
        NEW OPTION SET OPTION("FORCE PROCESS")=1
        SET OPTION("AWV")=+$G(AWV)
        QUIT $$GETHPI(TIULASTOV,.ITEMARRAY,.OUT,.OPTION)
        ;                   
LASTTIU(TMGDFN,SRCHTEXT)  ;
        ;"FIND LAST NOTE WITH HPI SECTION
        NEW TIUIEN SET TIUIEN=9999999
        NEW TIULASTOV SET TIULASTOV=0
        FOR  SET TIUIEN=$ORDER(^TIU(8925,"C",TMGDFN,TIUIEN),-1) QUIT:(TIUIEN'>0)!(TIULASTOV'=0)  DO
        . NEW IDX SET IDX=0
        . IF $P($G(^TIU(8925,TIUIEN,0)),"^",5)'=7 QUIT ;"ONLY USE SIGNED DOCS
        . NEW TEXT SET TEXT=""
        . FOR  SET IDX=$ORDER(^TIU(8925,TIUIEN,"TEXT",IDX)) QUIT:IDX'>0  DO
        . . SET TEXT=TEXT_$GET(^TIU(8925,TIUIEN,"TEXT",IDX,0))
        . IF TEXT'["HISTORY OF PRESENT ILLNESS (HPI)" QUIT   ;"elh  removed the ":" from the search
        . SET TIULASTOV=TIUIEN
        QUIT TIULASTOV
        ;              
GETHPI(IEN8925,ITEMARRAY,OUT,OPTION) ;"Get HPI section as one long string, with processing, formatting etc.
        ;"NOTE: as of 5/20/18, only called by LASTHPI() above (and a test function, T2(), above)
        ;"INPUT:  IEN8925 -- TIIU DOCUMENT IEN
        ;"        ITEMARRAY -- PASS BY REFERNCE.  An OUT PARAMETER.  See PARSEARR() for format
        ;"        OUT -- PASS BY REFERENCE.  An OUT PARAMETER.  Array form of HPI. 
        ;"           OUT(#)=<TEXT>
        ;"           OUT=<LINE COUNT>
        ;"        OPTION -- PASS BY REFERENCE.  OPTIONAL
        ;"          OPTION("FORCE PROCESS")=# (default is 1) If 1 note is processed even if tag is absent
        NEW TMGHPI SET TMGHPI=""
        NEW TIUARRAY,PROCESSEDARR,IDX SET IDX=0
        NEW TMGDFN SET TMGDFN=+$PIECE($GET(^TIU(8925,IEN8925,0)),"^",2)
        SET TIUARRAY("DFN")=TMGDFN
        SET ITEMARRAY("DFN")=TMGDFN  ;"5/30/19
        FOR  SET IDX=$ORDER(^TIU(8925,IEN8925,"TEXT",IDX)) QUIT:IDX'>0  DO
        . SET TIUARRAY("TEXT",IDX)=$GET(^TIU(8925,IEN8925,"TEXT",IDX,0))
        DO PROCESS^TMGTIUP3(.PROCESSEDARR,.TIUARRAY,.OPTION) 
        DO SCRUBESCRIBE(.PROCESSEDARR) ;"SCRUB ARRAY FOR ESCRIBE TAGS
        NEW RTNNOTE
        NEW TEMP SET TEMP=$$PARSEARR(.PROCESSEDARR,.ITEMARRAY,.OPTION,.RTNNOTE)  ;"Parse note array into formatted array
        IF TEMP'>0 SET TMGHPI=$PIECE(TEMP,"^",2) GOTO LHDN  ;"Return error message as HPI text
        ;" SET OPTION("BULLETS")=(+$GET(DUZ)'=83)  ;"<--- Eddie, please convert to use parameter.
        ;"Once below is tested, above line can be removed
        SET OPTION("BULLETS")=$$GETINIVALUE^TMGINI01(DUZ,"Use Bullets In HPI",1)
        SET OPTION("TRAILING <BR>")=1  ;"Add blank line to end of each section
        SET TMGHPI=$$COMPHPI(.ITEMARRAY,.OPTION,.OUT)  ;"COMPILE HPI   
LHDN    QUIT TMGHPI
        ;
PARSEARR(TIUARRAY,ITEMARRAY,OPTION,RTNNOTE)  ;"Parse note array into formatted array 
        ;"NOTE: See also TRIGGER1^TMGC0Q04 -> SUMNOTE^TMGTIUP1 --> PARSESCT^TMGTIUP1 for summarizing notes
        ;"//As of 5/20/18, only called from GETHPI^TMGTIUP2()               
        ;"Input: TIUARRAY -- PASS BY REFERENCE.  FORMAT:
        ;"          TIUARRAY(#)=<note text>  <-- array holds ENTIRE typical TMG note
        ;"       ITEMARRAY -- PASS BY REFERENCE.  AN OUT PARAMETER. FORMAT:
        ;"          ITEMARRAY(Ref#)=<Full section text>
        ;"          ITEMARRAY(Ref#,#)=different parts of section
        ;"          ITEMARRAY("TEXT",Ref#)=Title of section  
        ;"          ITEMARRAY("TEXT",Ref#,#)=sequential parts of section  
        ;"          ITEMARRAY("TEXT",3)="Dyspepsia"  
        ;"          ITEMARRAY("TEXT",3,1)=part 1, e.g. text, e.g. [GROUP A&B]
        ;"          ITEMARRAY("TEXT",3,1)="[GROUP]"
        ;"          ITEMARRAY("TEXT",3,1,"GROUP")="A&B"
        ;"          ITEMARRAY("TEXT",3,1,"GROUP","LIST","A&B")=""
        ;"          ITEMARRAY("TEXT",3,2)=part 2, e.g. name of inline table
        ;"          ITEMARRAY("TEXT",3,2)="[TABLE]"  <-- signal this part is a table. 
        ;"          ITEMARRAY("TEXT",3,2,"TABLE")=WT   <-- WT is name of table
        ;"          ITEMARRAY("TEXT",3,2,"TEXT")=<TEXT OF TABLE>
        ;"          ITEMARRAY("TEXT",3,2,"INLINE")=0 or 1        
        ;"          ITEMARRAY("TEXT",Ref#,3)=part 3, e.g. more text
        ;"          ITEMARRAY("TEXT",Ref#,4)=part 4, e.g. name of table  
        ;"          ITEMARRAY("TEXT",Ref#,"GROUPX",#)=""  <-- index of GROUP nodes
        ;"          ITEMARRAY("TEXT",Ref#,"TABLEX",#)=""  <-- index of TABLE nodes        
        ;"       OPTION -PASS BY REFERENCE.  AN OUT PARAMETER. FORMAT:
        ;"          OPTION("AUTOGROUPING") = 0 OR 1
        ;"          OPTION("NUMOFGROUPS") =
        ;"          OPTION("FORCEAUTOGROUP") = 1 if forcing from macro
        ;"          OPTION("GROUP-ORDER") = "C"  <---- if directions found in note.
        ;"          OPTION("RETURN-REST") = 0 OR1 , IF 1 RETURN NOTE WITHOUT HPI IN RTNNOTE
        ;"Result: 1^OK, or -1^Error message
        NEW TMGRESULT SET TMGRESULT="1^OK"
        NEW DORTNNOTE SET DORTNNOTE=+$G(OPTION("RETURN-REST"))
        NEW ORIGNOTE,IDX SET ORIGNOTE="",IDX=0
        SET IDX=0 FOR  SET IDX=$ORDER(TIUARRAY(IDX)) QUIT:IDX'>0  DO
        . SET ORIGNOTE=ORIGNOTE_$GET(TIUARRAY(IDX))
        ;        
        DO GTGRPORD(.TIUARRAY,.OPTION) ;"GET GROUP ORDER.  May set OPTION("GROUP-ORDER")  ;"//kt 5/1/18
        IF $G(OPTION("GROUP-ORDER"))="" DO GTOLDORD(.TIUARRAY,.OPTION)
        ;
        ;"NOTE: I encountered situation where there were so many <FONT ..> tags, that
        ;"      the DOM processor (below) was blowing the stack. 
        DO KILLFONT(.TIUARRAY)  
        ;
        ;"PROCESS NOTE VIA HTML DOM, using callback function.
        NEW TEMP SET TEMP=$$PROCESS^TMGHTM2(.TIUARRAY,"FIXHTML^TMGTIUP2")
        IF +TEMP<0 SET TMGHPI="ERROR: "_$PIECE(TEMP,"^",2,99) GOTO LHDN
        ;
        NEW TMGHPI SET TMGHPI=""
        ;"CONVERT ENTIRE NOTE INTO LONG STRING (TMGHPI)
        SET IDX=0 FOR  SET IDX=$ORDER(TIUARRAY(IDX)) QUIT:IDX'>0  DO
        . SET TMGHPI=TMGHPI_$GET(TIUARRAY(IDX))
        IF TMGHPI="" SET TMGRESULT="-1^No text for not found for processing." GOTO PRSDN
        ;
        ;"GET GROUPING SIGNAL, IF PRESENT
        IF +$G(OPTION("FORCEAUTOGROUP"))'>0 DO  ;"ELH ADDED IF FOR MACRO TO FORCE AUTOGROUPING
        . SET OPTION("AUTOGROUPING")=0
        . SET OPTION("NUMOFGROUPS")=0
        . IF TMGHPI["[GROUP AUTO " DO
        . . SET OPTION("NUMOFGROUPS")=+$P($P(TMGHPI,"[GROUP AUTO ",2),"]",1)
        . . IF OPTION("NUMOFGROUPS")>0 SET OPTION("AUTOGROUPING")=1
        ;
        IF TMGHPI["GROUP OFF" SET OPTION("GROUP OFF")=1  ;"ADDED 7/25/19
        ELSE  SET OPTION("GROUP OFF")=0   
        ;"EXTRACT JUST HPI PART
        ;"== SET UP MARKERS FOR BEGINNING AND ENDING OF DESIRED HPI SECTION =======
        NEW STARTARR,ENDARR
        SET STARTARR("<b>HISTORY OF PRESENT ILLNESS (HPI):</b>")=""
        SET STARTARR("<B>HISTORY OF PRESENT ILLNESS (HPI):</B>")=""
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
        ;"ELH ADDED TO RETURN REST OF NOTE
        IF DORTNNOTE=1 DO
        . ;"SET RTNNOTE=$PIECE(TMGHPI,STARTDIV,1)_STARTDIV_"<BR>"_"@@TMGHPI@@"
        . ;"SET RTNNOTE=RTNNOTE_ENDDIV_$PIECE(TMGHPI,ENDDIV,2)
        . NEW ORIGSTARTDIV,ORIGENDDIV SET (ORIGSTARTDIV,ORIGENDDIV)=""
        . FOR  SET ORIGSTARTDIV=$ORDER(STARTARR(ORIGSTARTDIV))  QUIT:ORIGNOTE[ORIGSTARTDIV  ;"when STARTDIV="", <text>["" is always TRUE
        . FOR  SET ORIGENDDIV=$ORDER(ENDARR(ORIGENDDIV))  QUIT:ORIGNOTE[ORIGENDDIV  ;"when ENDDIV="", <text>["" is always TRUE
        . SET RTNNOTE=$PIECE(ORIGNOTE,ORIGSTARTDIV,1)_STARTDIV_"<BR>"_"@@TMGHPI@@"
        . SET RTNNOTE=RTNNOTE_ENDDIV_$PIECE(ORIGNOTE,ORIGENDDIV,2)
        ;"
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
        DO RPTAGS^TMGHTM1(.TMGHPI,"[group ","[GROUP ")  ;"force group tags to be ucase 11/13/18
        DO RPTAGS^TMGHTM1(.TMGHPI,"[Group ","[GROUP ")
        DO RMTAGS^TMGHTM1(.TMGHPI,"<I>")   ;"//kt should have been already removed via DOM processing
        DO RMTAGS^TMGHTM1(.TMGHPI,"<EM>")  ;"//kt should have been already removed via DOM processing
        DO RMTAGS^TMGHTM1(.TMGHPI,"</I>")  ;"//kt should have been already removed via DOM processing
        DO RMTAGS^TMGHTM1(.TMGHPI,"</EM>") ;"//kt should have been already removed via DOM processing         
        ;"                           
        ;"Parse Items
        NEW TABLES,SECTION SET IDX=1       
        ;"NEW DELIMITER SET DELIMITER=$SELECT(TMGHPI["<LI>":"<LI>",1:"*") 
        ;"10-12-17, set delimiter below instead to catch the first delimiter
        ;"          found rather than to give weight to ordered list items
        NEW DELIMITER SET DELIMITER=$$NEXTCH^TMGSTUT3(TMGHPI,0,"<LI>","*")
        ;"If the delimiter is *, then we will replace any <LI>'s to *
        ;"IF DELIMITER="*" SET TMGHPI=$$REPLACE^TMGHTM1(TMGHPI,"<LI>","*")
        IF DELIMITER="*" SET TMGHPI=$$REPLSTR^TMGSTUT3(TMGHPI,"<LI>","*")
        SET TMGHPI=$P(TMGHPI,DELIMITER,2,999)                            
        NEW PREVFOUND SET PREVFOUND=0
        NEW SOCIALFOUND SET SOCIALFOUND=0
        NEW CONTRAFOUND SET CONTRAFOUND=0
        SET OPTION("GROUPING")=0
        ;"
        ;"ALLGRPS is an array containing all the groups listed in the HPI,
        ;"ALLGRPSTR is the comma delimited list
        NEW ALLGRPS,ALLGRPSTR SET ALLGRPSTR=$$ALLGRPS(TMGHPI,DELIMITER,.ALLGRPS)
        ;"NEW GROUPARR
        ;"TOPIC4ALL is an array of all titles
        NEW TOPIC4ALL DO TOPICFORALL(.TOPIC4ALL)
        ;"
        FOR  QUIT:TMGHPI=""  DO  
        . NEW SECTION SET SECTION=$P(TMGHPI,DELIMITER,1)
        . ;"HERE I NEED TO GET TITLE AND CHECK TO SEE IF GROUP NEEDS TO BE REPLACED WITH ALL GROUPSTR
        . NEW TITLE,TEXTARR DO SPLITTL(SECTION,.TITLE,.TEXTARR,.TABLES) ;"return title of section  
        . IF TMGHPI[DELIMITER SET TMGHPI=$P(TMGHPI,DELIMITER,2,999)
        . ELSE  SET TMGHPI=""
        . IF TITLE["ALLERGIES" QUIT
        . SET SECTION=$$TRIM^XLFSTR(SECTION)
        . DO RMTAGS^TMGHTM1(.SECTION,"</LI>")
        . IF ($$TRIMSECT(SECTION)="")!(SECTION="<P>")!(SECTION="<BR>")!(SECTION="<BR><BR>")!(SECTION="<BR></P>")!(SECTION="<U></U>:")!(SECTION=":") DO
        . . ;Skip section
        . ELSE  DO        
        . . SET SECTION=$$HTMLTRIM^TMGHTM1(SECTION,"LR")
        . . SET SECTION=$$TRAILTRM(SECTION)
        . . SET SECTION=$$ITALICS(SECTION)
        . . SET ITEMARRAY(IDX)=SECTION
        . . MERGE ITEMARRAY("TEXT",IDX)=TEXTARR 
        . . SET ITEMARRAY("TEXT",IDX)=TITLE
        . . ;"This if is added to test for a Title that is to be included in
        . . ;"all groups. The else below it was the previous code
        . . IF $D(TOPIC4ALL($$UP^XLFSTR(TITLE))) DO  ;"IF INCLUDED IN ALL GROUPS 
        . . . NEW GRP SET GRP=""
        . . . FOR  SET GRP=$ORDER(ALLGRPS(GRP)) QUIT:GRP=""  DO
        . . . . SET ITEMARRAY("GROUP",GRP,IDX)=""
        . . . . SET ITEMARRAY("GROUP",GRP,"COUNT")=+$G(ITEMARRAY("GROUP",GRP,"COUNT"))+1
        . . . SET ITEMARRAY("TEXT",IDX,1,"GROUP")=ALLGRPSTR
        . . . MERGE ITEMARRAY("TEXT",IDX,1,"GROUP","LIST")=ALLGRPS
        . . ELSE  DO                                ;"IF TREATED AS SET
        . . . NEW JDX SET JDX=0
        . . . FOR  SET JDX=$ORDER(TEXTARR(JDX)) QUIT:JDX'>0  DO
        . . . . NEW GRP SET GRP=""
        . . . . FOR  SET GRP=$ORDER(TEXTARR(JDX,"GROUP","LIST",GRP)) QUIT:GRP=""  DO
        . . . . . SET ITEMARRAY("GROUP",GRP,IDX)=""
        . . . . . SET ITEMARRAY("GROUP",GRP,"COUNT")=+$G(ITEMARRAY("GROUP",GRP,"COUNT"))+1
        . . SET IDX=IDX+1
        . . ;"IF $$UP^XLFSTR(SECTION)["PREVENT" SET PREVFOUND=1
        . . ;"JUST CHECK TITLE INSTEAD OF ENTIRE SECTION 4/8/19
        . . IF $$UP^XLFSTR(TITLE)["PREVENT" SET PREVFOUND=1
        . . IF $$UP^XLFSTR(TITLE)["SOCIAL" SET SOCIALFOUND=1
        . . IF $$UP^XLFSTR(TITLE)["CONTRACEPTION" SET CONTRAFOUND=1
        . . IF $$UP^XLFSTR(SECTION)["[GROUP" SET OPTION("GROUPING")=1
        . . IF $$UP^XLFSTR(SECTION)["(GROUP" SET OPTION("GROUPING")=1
        IF PREVFOUND=0 DO  ;"if prevention section not found, add blank one
        . SET ITEMARRAY(IDX)="<U>Prevention</U>: (data needed)"
        . SET ITEMARRAY("TEXT",IDX)="Prevention"
        . SET ITEMARRAY("TEXT",IDX,1)="(data needed)" 
        . SET IDX=IDX+1
        IF SOCIALFOUND=0 DO  ;"if socoail section not found, add blank one
        . SET ITEMARRAY(IDX)="<U>Social</U>: (data needed)"
        . SET ITEMARRAY("TEXT",IDX)="Social"
        . SET ITEMARRAY("TEXT",IDX,1)="(data needed)" 
        . SET IDX=IDX+1
        IF CONTRAFOUND=0 DO
        . NEW TMGDFN SET TMGDFN=+$G(ITEMARRAY("DFN"))
        . NEW AGE K VADM SET AGE=+$$AGE^TIULO(TMGDFN)
        . NEW GENDER SET GENDER=$P($G(^DPT(TMGDFN,0)),"^",2)
        . IF (GENDER="F")&(AGE>14)&(AGE<56) DO  ;"if contraception section not found for females between 15-55
        . . SET ITEMARRAY(IDX)="<U>Contraception</U>: (data needed)"
        . . SET ITEMARRAY("TEXT",IDX)="Contraception"
        . . SET ITEMARRAY("TEXT",IDX,1)="(data needed)"
        . . SET IDX=IDX+1
PRSDN   QUIT TMGRESULT
        ;
ALLGRPS(TMGHPI,DELIMITER,GROUPARR)  ;"Return a list of all groups for a provided HPI
        NEW SECTION,TABLES
        FOR  QUIT:TMGHPI=""  DO
        . NEW SECTION SET SECTION=$P(TMGHPI,DELIMITER,1)
        . NEW TITLE,TEXTARR DO SPLITTL(SECTION,.TITLE,.TEXTARR,.TABLES) ;"return title of section
        . IF TMGHPI[DELIMITER SET TMGHPI=$P(TMGHPI,DELIMITER,2,999)
        . ELSE  SET TMGHPI=""
        . NEW I SET I=$O(TEXTARR("GROUPX",0))
        . NEW GROUP SET GROUP=""
        . FOR  SET GROUP=$O(TEXTARR(I,"GROUP","LIST",GROUP)) QUIT:GROUP=""  DO        
        . . SET GROUPARR(GROUP)=""
        NEW GROUPSTR SET GROUPSTR="",GROUP=""
        FOR  SET GROUP=$O(GROUPARR(GROUP)) QUIT:GROUP=""  DO
        . IF GROUPSTR'="" SET GROUPSTR=GROUPSTR_","
        . SET GROUPSTR=GROUPSTR_GROUP        
        QUIT GROUPSTR
        ;"
TOPICFORALL(TOPIC4ALL)  ;"Set the array for all topics that should be included with all groups
        SET TOPIC4ALL("SOCIAL")=""
        SET TOPIC4ALL("PREVENTION")=""
        QUIT
        ;"
TRIMSECT(SECTION) ;"This removes tags and trims to determine if section is
                  ;"actually empty
        NEW TRIMMED SET TRIMMED=SECTION
        DO RMTAGS^TMGHTM1(.TRIMMED,"</B>") 
        DO RMTAGS^TMGHTM1(.TRIMMED,"<B>")
        DO RMTAGS^TMGHTM1(.TRIMMED,"<BR>")
        SET TRIMMED=$$TRIM^XLFSTR(TRIMMED)
        QUIT TRIMMED
        ;"
GTGRPORD(TIUARRAY,OPTION) ;"Get follow-up grouping order.  E.g. Follow up in 3 months for group C problems --> isolate "C"
        ;"Input: TIUARRAY -- PASS BY REFERENCE.  FORMAT:
        ;"          TIUARRAY(#)=<note text>  <-- array holds ENTIRE typical TMG note
        ;"       OPTION -PASS BY REFERENCE.  AN OUT PARAMETER. FORMAT:
        ;"          OPTION("GROUP-ORDER") = "C"  <---- if directions found in note.  
        ;"Result: None
        NEW FUDATE,LINETEXT
        IF $$GTARINFO^TMGTIU10("TIUARRAY",0,,.LINETEXT,0)  ;"drop result
        SET LINETEXT=$$UP^XLFSTR(LINETEXT)
        SET LINETEXT=$PIECE(LINETEXT,"GROUP ",2) QUIT:LINETEXT=""
        SET LINETEXT=$PIECE(LINETEXT,"PROBLEM",1) QUIT:LINETEXT=""
        SET LINETEXT=$$TRIM^XLFSTR(LINETEXT)  QUIT:LINETEXT=""
        ;"Supported formats:  C   or   C,B   or   C and B   or   A,B, and C
        NEW GROUPORDER SET GROUPORDER=""
        NEW OPT SET OPT(",")=" ",OPT("AND")=" "
        SET LINETEXT=$$REPLACE^XLFSTR(LINETEXT,.OPT)
        NEW IDX FOR IDX=1:1:$LENGTH(LINETEXT," ") DO
        . NEW ONE SET ONE=$$TRIM^XLFSTR($PIECE(LINETEXT," ",IDX)) QUIT:ONE=""
        . IF $LENGTH(ONE)>1 QUIT  ;"only allow 1 letter length group names
        . SET GROUPORDER=GROUPORDER_$SELECT(GROUPORDER'="":",",1:"")_ONE
        IF GROUPORDER'="" SET OPTION("GROUP-ORDER")=GROUPORDER
        QUIT
        ;
GTOLDORD(TEXTARR,OPTION)  ;"TRY TO FIND OLDER FOLLOWUP GROUPING ORDER IF FIRST
        ;"ATTEMPT FAILED
        NEW IDX SET IDX=0
        NEW GROUP SET GROUP=""
        NEW DONE SET DONE=0
        NEW INSIDE SET INSIDE=0
        FOR  SET IDX=$O(TEXTARR(IDX)) QUIT:(IDX'>0)!(DONE=1)  DO
        . NEW TEXT SET TEXT=$$UP^XLFSTR($G(TEXTARR(IDX)))
        . IF INSIDE=1 DO
        . . IF (TEXT["GROUP")&(TEXT["PROBLEM") DO
        . . . SET GROUP=$$TRIM^XLFSTR($P($P(TEXT,"GROUP",2),"PROBLEM",1))
        . . . IF GROUP'="" SET DONE=1
        . IF TEXT["FOLLOW-UP INFORMATION FROM PRIOR NOTE" DO
        . . SET INSIDE=1
        . IF TEXT["CHIEF COMPLAINT" SET DONE=1
        IF GROUP'="" SET OPTION("GROUP-ORDER")=GROUP
        QUIT
        ;"
  ;"Remove later  5/21/18      
  ;"COMPHPI0(ITEMARRAY,OPTION,OUT)  ;"EDDIE'S WORKING COMPILER OF HPI
  ;"        ;"Purpose: Reassemble ordered list, removing undesired sections
  ;"        ;"INPUT: ITEMARRAY -- PASS BY REFERENCE.  FORMAT -- SEE PARSEARR() above        
  ;"        ;"       OPTION -PASS BY REFERENCE.  FORMAT:
  ;"        ;"          OPTION("AUTOGROUPING") = 0 OR 1
  ;"        ;"          OPTION("NUMOFGROUPS") =
  ;"        ;"          OPTION("GROUPING") =
  ;"        ;"          OPTION("BULLETS") = 0 OR 1
  ;"        ;"       OUT -- PASS BY REFERENCE.  OPTIONAL.  Will get back formatted array with structured HPI.
  ;"        ;"           OUT(#)=<TEXT>
  ;"        ;"           OUT=<LINE COUNT>
  ;"        ;"Result: Returns HPI section as one long string.  
  ;"        NEW TOPICS SET TOPICS=+$ORDER(ITEMARRAY(""),-1)
  ;"        NEW WARNING SET WARNING=(TOPICS>10)
  ;"        NEW AUTOGROUPING SET AUTOGROUPING=+$GET(OPTION("AUTOGROUPING"))
  ;"        NEW NUMOFGROUPS  SET NUMOFGROUPS=$GET(OPTION("NUMOFGROUPS"))
  ;"        NEW GROUPING SET GROUPING=$GET(OPTION("GROUPING"))
  ;"        NEW BULLETS SET BULLETS=$GET(OPTION("BULLETS"))
  ;"        NEW TMGHPI SET TMGHPI=""
  ;"        IF GROUPING=1 SET AUTOGROUPING=0  ;"IF ALREADY GROUPING, DON'T ATTEMPT TO AUTOGROUP
  ;"        ;
  ;"        IF (WARNING=1)&(GROUPING=0)&(+$GET(DUZ)'=83) DO
  ;"        . SET TMGHPI="{HTML:<B><FONT style=""BACKGROUND-COLOR:#ff0000"">}CONSIDER GROUPING. PATIENT HAS "_TOPICS_" TOPICS.{HTML:</B></FONT>}"
  ;"        ELSE  DO
  ;"        . SET TMGHPI=""
  ;"        IF BULLETS=1 DO
  ;"        . SET TMGHPI=TMGHPI_"<UL>"
  ;"        ELSE  DO
  ;"        . SET TMGHPI=TMGHPI_""
  ;"        NEW IDX SET IDX=0
  ;"        FOR  SET IDX=$ORDER(ITEMARRAY(IDX)) QUIT:IDX'>0  DO
  ;"        . IF $GET(ITEMARRAY(IDX))["<U>ALLERGIES</U>" QUIT
  ;"        . IF BULLETS=1 DO
  ;"        . . IF AUTOGROUPING=1 DO
  ;"        . . . NEW BRKTAG SET BRKTAG=":"
  ;"        . . . IF $G(ITEMARRAY(IDX))["</U>:" SET BRKTAG="</U>:"
  ;"        . . . IF $G(ITEMARRAY(IDX))["</U> :" SET BRKTAG="</U> :"
  ;"        . . . SET TMGHPI=TMGHPI_"<LI>"_$$TRIM^XLFSTR($P($GET(ITEMARRAY(IDX)),BRKTAG,1))_BRKTAG_$$GROUP(IDX,TOPICS,NUMOFGROUPS)_$$TRIM^XLFSTR($P($GET(ITEMARRAY(IDX)),BRKTAG,2))_"</LI>"
  ;"        . . ELSE  DO
  ;"        . . . SET TMGHPI=TMGHPI_"<LI>"_$GET(ITEMARRAY(IDX))_"</LI>"
  ;"        . ELSE  DO
  ;"        . . SET TMGHPI=TMGHPI_"     * "_$GET(ITEMARRAY(IDX))_"<P>"
  ;"        IF BULLETS=1 SET TMGHPI=TMGHPI_"</UL>"
  ;"        QUIT TMGHPI
  ;"        ;
COMPHPI(ITEMARRAY,OPTION,OUT)  ;"COMPILE HPI    
        ;"Input: ITEMARRAY -- PASS BY REFERENCE.  Format:
        ;"            ITEMARRAY(Ref#)=<Full section text>
        ;"            ITEMARRAY("TITLE",Ref#)=<SECTION TITLE>
        ;"            ITEMARRAY("TEXT"... See PARSEARR() for format! 
        ;"            ITEMARRAY("GROUP",<GRP>,Ref#)=""  -- an index of items by group
        ;"              e.g. ITEMARRAY("GROUP","C",Ref#)=""  -- an index of items by group
        ;"            ITEMARRAY("GROUP",<GRP>,"COUNT")=number of items in group
        ;"       OPTION -PASS BY REFERENCE.  FORMAT:
        ;"          OPTION("AUTOGROUPING") = 
        ;"          OPTION("NUMOFGROUPS") =
        ;"          OPTION("GROUPING") =
        ;"          OPTION("BULLETS") = 0 OR 1        
        ;"          OPTION("GROUP-ORDER") = "A", This will output any group A, and then will output any B, then C etc.
        ;"                                or "A,B,C" This will output group A, then B, then C, and then non-grouped
        ;"                                or "C,B,A" This will output group C, then B, then A, and then non-grouped
        ;"                                or "C" This will output group C, then remaing groups in alphabetical order.                  
        ;"                                or "" (or undefined), order should be as in prior note  
        ;"          OPTION("AWV") = 0 OR 1 .  1 for Annual Wellness Visit -- to be implemented....
        ;"       OUT -- PASS BY REFERENCE.  OPTIONAL.  Will get back formatted array with structured HPI.
        ;"           OUT(#)=<TEXT>
        ;"           OUT=<LINE COUNT>
        ;"Results: returns one long string comprising HPI
        NEW AUTOGROUPING SET AUTOGROUPING=+$GET(OPTION("AUTOGROUPING"))
        NEW NUMOFGROUPS  SET NUMOFGROUPS=$GET(OPTION("NUMOFGROUPS"))
        NEW GROUPING SET GROUPING=$GET(OPTION("GROUPING"))
        NEW BULLETS SET BULLETS=$GET(OPTION("BULLETS"))
        NEW ADDBR SET ADDBR=+$GET(OPTION("TRAILING <BR>"))
        NEW GROUPORDER SET GROUPORDER=$GET(OPTION("GROUP-ORDER"))
        NEW GROUPOFF SET GROUPOFF=+$G(OPTION("GROUP OFF"))
        NEW AWV SET AWV=+$G(OPTION("AWV"))
        NEW TMGHPI SET TMGHPI=""
        IF GROUPING=1 SET AUTOGROUPING=0  ;"IF ALREADY GROUPING, DON'T ATTEMPT TO AUTOGROUP
        NEW IDX,SECTIONCT SET SECTIONCT=0,IDX=0
        FOR  SET IDX=$ORDER(ITEMARRAY(IDX)) QUIT:IDX'>0  SET SECTIONCT=SECTIONCT+1
        IF AUTOGROUPING>0 DO
        . DO AUTOGRP(.ITEMARRAY,NUMOFGROUPS,SECTIONCT)
        . SET GROUPING=1
        IF AWV=1 DO  ;"turn off grouping
        . SET GROUPING=0,AUTOGROUPING=0,GROUPORDER=""
        IF DUZ=168 DO
        . IF GROUPORDER'="" SET TMGHPI=$$WRAPTEXT^TMGTIUOT("Items arranged in Following Order:"_GROUPORDER_"<BR>","#e0ac11",.OPTION)
        . IF GROUPING=1 DO
        . . NEW UNGROUPED
        . . SET UNGROUPED=$$GETNOGRP(.ITEMARRAY)
        . . NEW GRPSTR,GRPNAME
        . . SET GRPSTR="",GRPNAME=""
        . . NEW GRPCOUNT,TOTALITEMS,AVERAGE
        . . SET GRPCOUNT=0,TOTALITEMS=0
        . . FOR  SET GRPNAME=$O(ITEMARRAY("GROUP",GRPNAME)) QUIT:GRPNAME=""  DO
        . . . IF GRPSTR'="" DO 
        . . . . SET GRPSTR=GRPSTR_", "
        . . . SET GRPCOUNT=GRPCOUNT+1
        . . . NEW THISCOUNT SET THISCOUNT=+$G(ITEMARRAY("GROUP",GRPNAME,"COUNT"))
        . . . SET GRPSTR=GRPSTR_GRPNAME_"="_THISCOUNT
        . . . SET TOTALITEMS=TOTALITEMS+THISCOUNT
        . . SET TOTALITEMS=TOTALITEMS+UNGROUPED
        . . IF (TOTALITEMS'>0)!(GRPCOUNT'>0) DO
        . . . SET AVERAGE="ERROR CALCULATING"
        . . ELSE  DO
        . . . SET AVERAGE=(TOTALITEMS/GRPCOUNT)\1
        . . IF UNGROUPED>0 DO
        . . . NEW HANDLEDUNGRPED 
        . . . IF GROUPORDER'="" SET HANDLEDUNGRPED="Set to current group order"
        . . . ELSE  SET HANDLEDUNGRPED="At top of note"
        . . . SET GRPSTR=GRPSTR_", Ungrouped="_UNGROUPED_" ("_HANDLEDUNGRPED_")"
        . . SET GRPSTR="Grouping count: "_GRPSTR_" Avg per group: "_AVERAGE_"<BR>"
        . . ;"REMOVED BELOW FOR NOW, PER DR. K    5/19/20
        . . ;"SET TMGHPI=TMGHPI_$$WRAPTEXT^TMGTIUOT(GRPSTR,"#e0ac11",.OPTION)        
        ;"        
        NEW WARNING SET WARNING=(SECTIONCT>10)&'GROUPING      
        NEW DELIM DO SUDELIM(.DELIM) 
        ;
        IF (WARNING=1)&(GROUPING=0)&(+$GET(DUZ)'=83)&(AWV'=1) DO
        . SET TMGHPI=TMGHPI_$$ADDSTR($$GRPNGSTR(SECTIONCT),.OUT)  
        IF BULLETS SET TMGHPI=TMGHPI_$$ADDSTR("<UL>",.OUT)  ;"//add to TMGHPI string and OUT array        
        NEW SEQARR DO GETSEQAR(.SEQARR,.ITEMARRAY,GROUPORDER,.OPTION)  ;"Get process sequencing order.   
        ;"FOR  SET IDX=$ORDER(ITEMARRAY("TEXT",IDX)) QUIT:IDX'>0  DO
        NEW CT SET CT=0
        FOR  SET CT=$ORDER(SEQARR(CT)) QUIT:CT'>0  DO
        . NEW IDX SET IDX=$GET(SEQARR(CT))   
        . NEW TITLE SET TITLE=$GET(ITEMARRAY("TEXT",IDX))
        . NEW LINE SET LINE=DELIM(BULLETS,"START")_$$FORMATTL(TITLE)  ;"FORMAT TITLE
        . ;"IF AUTOGROUPING>0 SET LINE=LINE_$$GROUP(IDX,SECTIONCT,NUMOFGROUPS)_" "
        . NEW TEXTARR MERGE TEXTARR=ITEMARRAY("TEXT",IDX) 
        . NEW LASTSECT SET LASTSECT=""
        . NEW PART SET PART=0
        . FOR  SET PART=$ORDER(TEXTARR(PART)) QUIT:PART'>0  DO
        . . NEW STR SET STR=$GET(TEXTARR(PART)) QUIT:STR=""
        . . IF STR="[GROUP]" DO  QUIT
        . . . ;"7/25/19 ADDED GROUPOFF TO SUPPRESS FORWARDING OF GROUPS
        . . . IF GROUPOFF=0 SET LINE=LINE_"[GROUP "_$GET(TEXTARR(PART,"GROUP"))_"] "
        . . . SET LASTSECT="GROUP"
        . . ELSE  IF STR="[TABLE]" DO  QUIT
        . . . NEW INLINE SET INLINE=+$GET(TEXTARR(PART,"INLINE"))
        . . . IF 'INLINE SET LINE=LINE_"<BR><BR>"
        . . . SET LINE=LINE_$GET(TEXTARR(PART,"TEXT"))_" "
        . . . IF 'INLINE SET LINE=LINE_"<BR>"
        . . . SET LASTSECT="TABLE"
        . . DO  QUIT
        . . . IF (DUZ=168)&(LINE'["GROUP")&(GROUPORDER'="") DO   ;"ELH ADDED THIS IF FOR TOPICS WITHOUT GROUPS, ASSUMING THE GROUPORDER  4/28/20
        . . . . SET LINE=LINE_"[GROUP "_GROUPORDER_"] "
        . . . NEW TEXT SET TEXT=$GET(TEXTARR(PART))
        . . . SET LINE=LINE_$$FORMATTX(TEXT) ;"FORMAT BODY TEXT OF ONE SECION
        . . . SET LASTSECT="TEXT"
        . IF ADDBR,LASTSECT="TEXT" SET LINE=LINE_"<BR>"
        . SET LINE=LINE_DELIM(BULLETS,"END")        
        . SET TMGHPI=TMGHPI_$$ADDSTR(LINE,.OUT)  ;"//add to TMGHPI string and OUT array
        IF BULLETS SET TMGHPI=TMGHPI_$$ADDSTR("</UL>",.OUT)
        QUIT TMGHPI
        ;
GETNOGRP(ITEMARRAY)  ;"FIND WHICH SECTIONS DON'T HAVE GROUPS
        ;"NEW TEST SET TEST=0
        ;"IF TEST=1 MERGE ^TMG("ITEMARRAY")=ITEMARRAY
        ;"ELSE  MERGE ITEMARRAY=^TMG("ITEMARRAY")
        NEW RESULT SET RESULT=0
        NEW IDX SET IDX=0
        FOR  SET IDX=$O(ITEMARRAY("TEXT",IDX)) QUIT:IDX'>0  DO
        . IF '$D(ITEMARRAY("TEXT",IDX,"GROUPX")) SET RESULT=RESULT+1
        QUIT RESULT
        ;"
AUTOGRP(ITEMARR,NUMOFGRPS,SECTIONCOUNT)  ;"AUTO GROUP TOPICS
        NEW CT SET CT=0
        NEW DELIM DO SUDELIM(.DELIM)
        FOR  SET CT=$ORDER(ITEMARR(CT)) QUIT:CT'>0  DO
        . NEW IDX SET IDX=CT  ;"$GET(ITEMARR(CT))
        . NEW TITLE SET TITLE=$GET(ITEMARR("TEXT",IDX))
        . NEW LINE SET LINE=DELIM(BULLETS,"START")_$$FORMATTL(TITLE) ;"FORMAT TITLE
        . NEW GROUPLINE SET GROUPLINE=$$GROUP(IDX,SECTIONCOUNT,NUMOFGRPS)
        . NEW GRP SET GRP=$P($P(GROUPLINE,"GROUP ",2),"]",1)
        . SET LINE=LINE_GROUPLINE_" "
        . ;"SET ITEMARR("TEXT",IDX,1)=GROUPLINE_$G(LINE,TITLE,2)
        . NEW TEMPIDX SET TEMPIDX=$ORDER(ITEMARR("TEXT",IDX,""))-0.5
        . SET ITEMARR("TEXT",IDX,TEMPIDX)="[GROUP]"
        . SET ITEMARR("TEXT",IDX,TEMPIDX,"GROUP")=GRP
        . SET ITEMARR("TEXT",IDX,TEMPIDX,"GROUP","LIST",GRP)=""
        . SET ITEMARR("GROUP",GRP,IDX)=""
        . SET ITEMARR("GROUP",GRP,"COUNT")=+$G(ITEMARR("GROUP",GRP,"COUNT"))+1  
        . SET ITEMARR("TEXT",IDX,"GROUPX")=GRP
        QUIT
        ;"
GETSEQAR(SEQARR,ITEMARRAY,GRPORDER,OPTION)  ;"Get process sequencing order.
        ;"Input:  SEQARR
        ;"        ITEMARRAY
        ;"        GRPORDER
        ;"        OPTION
        ;"Output is SEQARR.  Format (SEQARR(#)=IDX order.  IDX is used as  ITEMARRAY("TITLE",IDX)
        NEW AWV SET AWV=+$G(OPTION("AWV"))
        NEW IDX
        SET GRPORDER=$$TRIM^XLFSTR($GET(GRPORDER))
        IF (AWV=1)&(DUZ=168) SET GRPORDER="A,B,C,D"
        ;"eddie adding 10/23/18
        NEW BREAKLINE SET BREAKLINE=-1
        IF (GRPORDER="")&(AWV=0) SET GRPORDER=$$GETFIRST(.ITEMARRAY)
        ELSE  DO ADDBREAK(.ITEMARRAY,.BREAKLINE)        
        IF GRPORDER'="" DO
        . ;"Create a sequence array based on requested grouping order
        . NEW USEDIDXARR,USEDGRPARR,GRP,LASTGRP,CT
        . SET CT=7  ;"WE WILL LEAVE 4-6 FOR UNGROUPED, counting in tenths 10/11/18, 1 FOR SOCIAL, 2 FOR PREVENTION
        . IF AWV=1 DO
        . . SET SEQARR(3)=BREAKLINE,USEDIDXARR(BREAKLINE)=1
        . NEW GROUP
        . FOR PN=1:1:$LENGTH(GRPORDER,",") DO  ;"NOTE: GRPORDER may not mention all available group names
        . . SET GROUP=$$TRIM^XLFSTR($PIECE(GRPORDER,",",PN))
        . . SET LASTGRP=GROUP
        . . DO GTSQ1AR(.ITEMARRAY,.SEQARR,GROUP,.CT,.USEDIDXARR,AWV)  
        . ;"Now that . . we have requested groups, continue with other groups, starting after last used group
        . ;"E.g. if we had groups A,B,C,D in note, and requested group of B, then get next C, then D
        . IF (BREAKLINE>-1)&(AWV=0) DO
        . . SET CT=CT+1,SEQARR(CT)=BREAKLINE,USEDIDXARR(BREAKLINE)=1
        . SET GROUP=LASTGRP
        . FOR  SET GROUP=$ORDER(ITEMARRAY("GROUP",GROUP)) QUIT:GROUP=""  DO
        . . DO GTSQ1AR(.ITEMARRAY,.SEQARR,GROUP,.CT,.USEDIDXARR,AWV)  
        . ;"Now start over again at beginning of list. In example above, loop back to beginning of list and get group A
        . SET GROUP=""
        . FOR  SET GROUP=$ORDER(ITEMARRAY("GROUP",GROUP)) QUIT:GROUP=""  DO
        . . DO GTSQ1AR(.ITEMARRAY,.SEQARR,GROUP,.CT,.USEDIDXARR,AWV)  
        . ;"Lastly, go through every item, which might include items NOT in ANY group, and add them
        . SET IDX=0
        . SET CT=4
        . FOR  SET IDX=$ORDER(ITEMARRAY("TEXT",IDX)) QUIT:IDX'>0  DO
        . . IF $GET(USEDIDXARR(IDX))>0 QUIT  ;"already used 
        . . SET CT=CT+.1,SEQARR(CT)=IDX        
        ELSE  DO
        . ;"Create a sequence array based on order of appearance in prior note. 
        . NEW CT SET CT=4
        . NEW IDX SET IDX=0
        . FOR  SET IDX=$ORDER(ITEMARRAY("TEXT",IDX)) QUIT:IDX'>0  DO
        . . IF $$UP^XLFSTR(ITEMARRAY("TEXT",IDX))["SOCIAL" DO
        . . . SET SEQARR(1)=IDX
        . . ELSE  IF ($$UP^XLFSTR(ITEMARRAY("TEXT",IDX))["PREVENTION")&(AWV=1) DO
        . . . SET SEQARR(2)=IDX
        . . ELSE  IF ($$UP^XLFSTR(ITEMARRAY("TEXT",IDX))["HPI ISSUES BELOW WERE")&(AWV=1) DO
        . . . SET SEQARR(3)=IDX
        . . ELSE  DO
        . . . SET CT=CT+1,SEQARR(CT)=IDX
        . ;
        . ;" Move Prevention bullet to last position //kt 3/31/21
        . ;" This is already done in GTSQ1AR for notes with groups
        . NEW IDX SET IDX=0
        . NEW DONE SET DONE=0
        . FOR  SET IDX=$ORDER(SEQARR(IDX)) QUIT:(IDX'>0)!(AWV=1)!(DONE=1)  DO
        . . NEW JDX SET JDX=$GET(SEQARR(IDX)) QUIT:JDX'>0
        . . NEW TOPIC SET TOPIC=$GET(ITEMARRAY("TEXT",JDX))
        . . SET TOPIC=$$UP^XLFSTR(TOPIC)
        . . IF TOPIC'="PREVENTION" QUIT
        . . KILL SEQARR(IDX)
        . . NEW LAST SET LAST=$ORDER(SEQARR(""),-1)
        . . SET SEQARR(LAST+1)=JDX
        . . SET DONE=1 ;"This lets us quit the for loop without looping back to Prevention at the SEQARR's end
        QUIT
        ;
GETFIRST(ITEMARRAY)  ;"This function is used when no group is listed in the
                     ;"order
        NEW TMGRESULT SET TMGRESULT=""
        NEW IDX SET IDX=0
        FOR  SET IDX=$O(ITEMARRAY(IDX)) QUIT:(IDX'>0)!(TMGRESULT'="")  DO
        . NEW GROUP SET GROUP=$G(ITEMARRAY("TEXT",IDX,1,"GROUP"))
        . IF (GROUP'="")&($L(GROUP)=1) SET TMGRESULT=GROUP
        QUIT TMGRESULT
        ;"
ADDBREAK(ITEMARRAY,BREAKLINE)  ;"ADD A BREAK SECTION
        SET BREAKLINE=999
        SET BREAKLINE=$O(ITEMARRAY(BREAKLINE),-1)+1
        SET ITEMARRAY(BREAKLINE)=$$NOTADDRE^TMGTIUOT
        SET ITEMARRAY("TEXT",BREAKLINE)=$$NOTADDRE^TMGTIUOT
        QUIT
        ;"
GTSQ1AR(ITEMARRAY,SEQARR,GROUP,CT,USEDIDXARR,AWV)  ;
        SET GROUP=$GET(GROUP)
        SET AWV=+$G(AWV)
        IF $DATA(USEDGRPARR(GROUP)) QUIT
        SET USEDGRPARR(GROUP)=""
        NEW IDX SET IDX=0
        NEW PREVIDX SET PREVIDX=0
        FOR  SET IDX=$ORDER(ITEMARRAY("GROUP",GROUP,IDX)) QUIT:IDX'>0  DO
        . IF $GET(USEDIDXARR(IDX))>0 QUIT  ;"already used  (some index can be in multiple groups)
        . IF $$UP^XLFSTR(ITEMARRAY("TEXT",IDX))["SOCIAL" DO
        . . SET SEQARR(1)=IDX
        . . SET USEDIDXARR(IDX)=1
        . ELSE  IF $$UP^XLFSTR(ITEMARRAY("TEXT",IDX))["PREVENTION" DO
        . . IF AWV=1 DO
        . . . SET SEQARR(2)=IDX
        . . . SET USEDIDXARR(IDX)=1
        . . ELSE  DO
        . . . SET PREVIDX=IDX
        . ELSE  DO
        . . SET CT=CT+1,SEQARR(CT)=IDX,USEDIDXARR(IDX)=1
        IF PREVIDX>0 DO  ;"PREV WAS FOUND AND IS MOVED TO BOTTOM OF SEQ FOR THIS GROUP
        . SET CT=CT+1,SEQARR(CT)=PREVIDX,USEDIDXARR(PREVIDX)=1
        QUIT
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
        ;"//As of 5/20/18, only called from PARSEARR^TMGTIUIP2()  ;"Parse note array into formatted array 
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
        IF TEXT="" QUIT TEXT
        SET TEXT=$$REMOLDDT(.TEXT)
        SET TMGRESULT="<I>"_TEXT_"</I>"_"... "
        IF DUZ'=83 SET TMGRESULT=TMGRESULT_$$TODAY^TMGDATE(1,1)_": "
        ;"SET TMGRESULT=$$ITALICS(TEXT)    ;"This will remove the italics and add one single
        QUIT TMGRESULT
        ;
REMOLDDT(TEXT)  ;"REMOVE THE OLD DATES
        SET TEXT=$G(TEXT)
        NEW NEWTEXT
        SET NEWTEXT=$$TRIM^XLFSTR(TEXT)
        NEW LASTPIECE SET LASTPIECE=$$NUMPIECE(NEWTEXT," ")
        ;"
        NEW DONE
        SET DONE=0
        FOR  QUIT:DONE=1  DO
        . NEW STR SET STR=$P(NEWTEXT," ",LASTPIECE)
        . ;"
        . IF STR="..." SET LASTPIECE=LASTPIECE-1 QUIT
        . NEW ISDATE SET ISDATE=0
        . IF STR[":" DO
        . . SET STR=$P(STR,":",1),STR=$$INTDATE^TMGDATE(STR)
        . . IF STR'="-1" SET ISDATE=1
        . ;"
        . IF ISDATE SET LASTPIECE=LASTPIECE-1 QUIT
        . SET DONE=1
        SET NEWTEXT=$PIECE(NEWTEXT," ",1,LASTPIECE)              
        ;" Remove ending periods
        NEW LASTCHAR SET DONE=0
        FOR  QUIT:DONE=1  DO
        . SET LASTCHAR=$E(NEWTEXT,$L(NEWTEXT),$L(NEWTEXT))
        . IF LASTCHAR="." DO
        . . SET NEWTEXT=$E(NEWTEXT,0,$L(NEWTEXT)-1)
        . ELSE  SET DONE=1
        QUIT NEWTEXT
        ;"
NUMPIECE(STRING,DELIM)
        NEW COUNT,DONE,CHAR,CHARNUM
        SET (CHARNUM,DONE)=0,COUNT=1
        FOR CHARNUM=1:1:$L(STRING)  DO
        . IF $E(STRING,CHARNUM)=DELIM SET COUNT=COUNT+1
        QUIT COUNT
        ;"
PRCSSTXT(TEXTARR,TABLES)  ;"Process, parse, clean text for one section for unmatching tags etc.
        ;"//As of 5/20/18, only called from SPLITTL^TMGTIUIP2() 
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
        . SET SECTION=P1_"</I>... "_$$TODAY^TMGDATE(1)_" "_P2_"<BR>"
        ELSE  DO
        . SET SECTION=SECTION_"</I>..."_$$TODAY^TMGDATE(1)_" "
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
KILLFONT(TIUARRAY) ;"Kill FONT tags
        ;"Input: TIUARRAY -- PASS BY REFERENCE.  FORMAT:
        ;"          TIUARRAY(#)=<note text>  <-- array holds ENTIRE typical TMG note
        ;"NOTE: I had to do this because some notes were getting so clogged with 
        ;"      font tags, that the DOM processor was blowing it's stack.  
        ;"Results: none
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(TIUARRAY(IDX)) QUIT:IDX'>0  DO
        . NEW LINE SET LINE=$GET(TIUARRAY(IDX)) QUIT:LINE=""
        . NEW INITLINE SET INITLINE=LINE   
        . DO RMTAG2^TMGHTM1(.LINE,"FONT") 
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
        DO FIXIMG(DOMNAME)
        IF $GET(ERR)'="" QUIT
        ;"more here later if needed...
        QUIT
        ;
FIXIMG(DOMNAME,ERR)  ;"Fix IMG tags for dynamic refreshing graph images.
        ;"Look for IMG tags with tmg_cmd and tmg_datestr attributes.  
        ;"     If found, then add tmg_needs_refresh=1 attribute.  
        NEW STATUS,NODEARR
        SET STATUS=$$getElementsArrayByTagName^%zewdDOM("img",DOMNAME,"",.NODEARR) ;"//get array with all IMG tag nodes.
        NEW NODEID SET NODEID=""
        FOR  SET NODEID=$ORDER(NODEARR(NODEID)) QUIT:NODEID=""  DO
        . NEW ATTRARR
        . SET STATUS=$$getAttributeValues^%zewdDOM(NODEID,.ATTRARR)
        . NEW FOUNDCMD,FOUNDDT SET (FOUNDCMD,FOUNDDT)=0
        . NEW ATTR SET ATTR=""
        . FOR  SET ATTR=$ORDER(ATTRARR(ATTR)) QUIT:ATTR=""  DO
        . . NEW VAL SET VAL=$GET(ATTRARR(ATTR))
        . . IF ATTR="tmg_cmd",(VAL'="") SET FOUNDCMD=1
        . . IF ATTR="tmg_datestr",(VAL'="") SET FOUNDDT=1
        . IF FOUNDCMD,FOUNDDT DO
        . . DO setAttribute^%zewdDOM("tmg_needs_refresh","1",NODEID)
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
SCRUBESCRIBE(ARR) ;"SCRUB ARRAY FOR ESCRIBE TAGS
       ;"INPUT: ARR.  Expected format: ARR(#)=<line of text>
       IF $$REPLARR^TMGSTUT3("ARR","{E-Scribe}","[E-Scribe]") ;"ignore results
       IF $$REPLARR^TMGSTUT3("ARR","{/E-Scribe}","[/E-Scribe]") ;"ignore results
       QUIT
       ;
