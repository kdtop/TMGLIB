TMGTIUP1 ;TMG/kst-TMG TIU NOTE PARSING FUNCTIONS ; 4/11/17
         ;;1.0;TMG-LIB;**1,17**;4/11/17
 ;
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
 ;"SUMNOTE(TIUIEN,ARRAY) -- To take a given note in file 8925, and parse HPI and A&P into array
 ;"
 ;"=======================================================================
 ;"PRIVATE FUNCTIONS
 ;"=======================================================================
 ;"PARSESCT(TEMPARR,TIUIEN,SECTION,ARRAY) -- parse one section on note array
 ;"FRMATTTL(TITLE) -- format titles, so that similar terms sort together.
 ;"PARSESCT2(TEMPARR,TIUIEN,SECTION,ARRAY)  -- parse one section on note array
 ;"ADDLINE(REF,TEXT) -- add text line to end of array.  
 ;"FRMATTTL(TITLE)  -- format titles, so that similar terms sort together.
 ;"GETMODS(MODS) -- load up MODS variable.
 ;"CHKDIV(LINESTR,DIV) 
 ;"
 ;"=======================================================================
 ;"Dependancies : 
 ;"=======================================================================
TESTSUM ;"Test summarizing one note.
        NEW DIC,X,Y
        SET DIC=8925,DIC(0)="MAEQ"
        DO ^DIC WRITE !
        QUIT:+Y'>0
        NEW ARRAY DO SUMNOTE^TMGTIUP1(+Y,.ARRAY) 
        IF $DATA(ARRAY) DO ZWRITE^TMGZWR("ARRAY")
        QUIT
        ;
SUMNOTE(TIUIEN,ARRAY,OPTION) ;
        ;"Purpose: To take a given note in file 8925, and parse HPI and A&P into array
        ;"Input: TIUIEN -- IEN in 8925
        ;"       ARRAY -- PASS BY REFERENCE.  An OUT PARAMETER.
        ;"       OPTION -- options to affect parsing etc.  See downstream documentation
        ;"Results: none
        ;"Output: ARRAY filled as below
        ;"              ARRAY(TIUIEN,"FULL","HPI",<TOPIC NAME>,Line#)=text
        ;"              ARRAY(TIUIEN,"FULL","A&P",<TOPIC NAME>,Line#)=text
        ;"              ARRAY(TIUIEN,"HPI",#)=<TOPIC NAME>^<First line of paragraph>
        ;"              ARRAY(TIUIEN,"A&P",#)=<TOPIC NAME>^<First line of paragraph>
        ;"              ARRAY(TIUIEN,"TITLE",<TOPIC NAME>)=<topic name as originally in text>
        ;"              ARRAY(TIUIEN,"SEQ#",<SECTION>,#)=<TOPIC NAME>
        ;"              ARRAY(TIUIEN,"SEQ#",<SECTION>)=# OF TOPICS
        NEW TEMPARR
        NEW SECTION SET SECTION=""
        NEW SECTNUM SET SECTNUM=0
        NEW LINEI SET LINEI=0
        SET TIUIEN=+$GET(TIUIEN) GOTO:TIUIEN'>0 SNDN
        NEW TEXTARR MERGE TEXTARR=^TIU(8925,TIUIEN,"TEXT")
        NEW ITEMARRAY,OUT
        SET OPTION("FORCE PROCESS")=0
        SET OPTION("SKIP REFRESH TABLES")=1   
        SET OPTION("THREADS")=1
        ;"IF $$GETHPI^TMGTIUP2(TIUIEN,.ITEMARRAY,.OUT,.OPTION)  ;"IGNORE RESULT
        DO PARSETIU^TMGTIUP2(TIUIEN,.ITEMARRAY,.OPTION) ;"parse HPI section of TIU NOTE with processing, formatting etc.
        DO PARSESCT2(.ITEMARRAY,.ARRAY,TIUIEN)
SNDN    QUIT
        ;
  ;"HPIDIV(LINESTR)  ;
  ;"        ;"Purpose: Determine if LINESTR text indicates change into HPI parts
  ;"        ;"Input: LINES -- 1 line of text
  ;"        ;"Result: 1 IF LINESTR is a section divider, 0 otherwise.
  ;"        NEW RESULT SET RESULT=1
  ;"        IF LINESTR["HPI" GOTO HPDN
  ;"        NEW TEMPS SET TEMPS=$$UP^XLFSTR(LINESTR)
  ;"        IF TEMPS["HISTORY",TEMPS["PRESENT",TEMPS["ILLNESS" GOTO HPDN
  ;"        SET RESULT=0
  ;"HPDN    QUIT RESULT
  ;"        ;        
  ;"PMHDIV(LINESTR)  ;
  ;"        ;"Purpose: Determine if LINESTR text indicates change from HPI to PMH parts
  ;"        ;"Input: LINES -- 1 line of text
  ;"        ;"Result: 1 IF LINESTR is a section divider, 0 otherwise.
  ;"        NEW RESULT SET RESULT=1
  ;"        IF LINESTR["(PMH)" GOTO PDDN
  ;"        IF LINESTR["ROS:" GOTO PDDN
  ;"        ;"Allergies are now located in HPI section -> IF LINESTR["ALLERGIES:" GOTO PDDN
  ;"        IF LINESTR["OBJECTIVE" GOTO PDDN
  ;"        IF LINESTR["[MEDICATIONS]" GOTO PDDN
  ;"        IF LINESTR["[PROBLEM LIST]" GOTO PDDN
  ;"        IF LINESTR["PMFS" GOTO PDDN
  ;"        SET RESULT=0
  ;"PDDN    QUIT RESULT
  ;"        ;
  ;"ASSEMENT(LINESTR) ;
  ;"        ;"Purpose: Determine IF LINES text indicates change into Assesment & Plan parts
  ;"        ;"Input: LINES -- 1 line of text
  ;"        ;"Result: 1 IF LINESTR is a section divider, 0 otherwise.
  ;"        NEW RESULT SET RESULT=1
  ;"        NEW TEMPS SET TEMPS=$$UP^XLFSTR(LINESTR)
  ;"        IF (TEMPS["ASSESMENT")&(TEMPS["PLAN") GOTO ASDN
  ;"        IF (TEMPS["ASSESSMENT")&(TEMPS["PLAN") GOTO ASDN
  ;"        SET RESULT=0
  ;"ASDN    QUIT RESULT
  ;"        ;    
PARSESCT(TEMPARR,TIUIEN,SECTION,ARRAY)  ;"PARSE SECTIONS
        ;"NOTE: see also PARSEARR^TMGTIUP2 regarding parsing sections
        ;"Purpose: parse one section on note array
        ;"Input:  TEMPARR -- PASS BY REFERENCE.  Array as created in SUMNOTE()
        ;"        TIUIEN -- IEN 8925
        ;"        SECTION -- "HPI" or "A&P"
        ;"        ARRAY -- PASS BY REFERENCE.  AN OUT PARAMETER.
        ;"Output: ARRAY filled as below
        ;"              ARRAY(TIUIEN,"FULL",<SECTION>,<TOPIC NAME>,Line#)=text
        ;"              ARRAY(TIUIEN,<SECTION>,#)=<TOPIC NAME>^<First line of paragraph>
        ;"              ARRAY(TIUIEN,"TITLE",<TOPIC NAME>)=<topic name as originally in text>
        ;"              ARRAY(TIUIEN,"SEQ#",<SECTION>,#)=<TOPIC NAME>
        ;"              ARRAY(TIUIEN,"SEQ#",<SECTION>)=# OF TOPICS
        ;"Results: none
        NEW LINEI SET LINEI=0
        NEW TITLE SET TITLE=""
        NEW ORIGTITLE SET ORIGTITLE=""
        NEW CT SET CT=0
        NEW TMGDOCSCANMODS
        NEW INBULLET SET INBULLET=0
        FOR  SET LINEI=$ORDER(TEMPARR(TIUIEN,SECTION,LINEI)) QUIT:(+LINEI'>0)  DO
        . NEW LINESTR SET LINESTR=$GET(TEMPARR(TIUIEN,SECTION,LINEI))
        . SET LINESTR=$$TRIM^XLFSTR(LINESTR)
        . IF +LINESTR>0 DO
        . . NEW NUM SET NUM=+LINESTR
        . . IF NUM>15 QUIT
        . . NEW TS SET TS=NUM_". "
        . . IF $EXTRACT(LINESTR,1,$LENGTH(TS))'=TS QUIT
        . . SET LINESTR="*"_$PIECE(LINESTR,NUM,2,999)
        . IF $EXTRACT(LINESTR,1)="*" DO
        . . SET CT=CT+1
        . . SET LINESTR=$PIECE(LINESTR,"*",2)
        . . SET LINESTR=$$TRIM^XLFSTR(LINESTR)
        . . IF $EXTRACT(LINESTR)="." SET LINESTR=$$TRIM^XLFSTR($EXTRACT(LINESTR,2,$LENGTH(LINESTR)))
        . . SET (TITLE,ORIGTITLE)=""
        . . NEW DIV FOR DIV=":","--","  ",".","-",";" DO  QUIT:TITLE'=""
        . . . SET TITLE=$$CHKDIV(.LINESTR,DIV)
        . . NEW SHORTLINE SET SHORTLINE=$EXTRACT(LINESTR,1,45)
        . . SET ORIGTITLE=TITLE
        . . DO FRMATTTL(.TITLE)
        . . IF (CT=1)&(TITLE="ALLERGIES") DO
        . . . SET TITLE=""
        . . ELSE  DO
        . . . SET ARRAY(TIUIEN,SECTION,CT)=TITLE_"^"_SHORTLINE
        . . . NEW SEQ SET SEQ=$GET(ARRAY(TIUIEN,"SEQ#",SECTION))+1
        . . . SET ARRAY(TIUIEN,"SEQ#",SECTION)=SEQ
        . . . SET ARRAY(TIUIEN,"SEQ#",SECTION,SEQ)=TITLE
        . IF TITLE'="" DO
        . . DO ADDLINE($NAME(ARRAY(TIUIEN,"FULL",SECTION,TITLE)),LINESTR)
        . . SET ARRAY(TIUIEN,"TITLE",TITLE)=ORIGTITLE
        QUIT
        ;
TESTPARSE(TIUIEN)
        NEW ITEMARRAY,OUT,OPTION,ARRAY
        IF $$GETHPI^TMGTIUP2(TIUIEN,.ITEMARRAY,.OUT,.OPTION)  ;"IGNORE RESULT
        DO PARSESCT2(.ITEMARRAY,.ARRAY,TIUIEN)
        ZWR ARRAY
        QUIT
        ;"
PARSESCT2(ITEMARRAY,ARRAY,TIUIEN)  ;
        ;"NOTE: see also PARSEARR^TMGTIUP2 regarding parsing sections
        ;"Purpose: parse one section on note array
        ;"Input:  ITEMARRAY -- PASS BY REFERENCE.  FORMAT:
        ;"               ITEMARRAY(Ref#)=<Full section text>
        ;"               ITEMARRAY(Ref#,#)=different parts of section
        ;"               ITEMARRAY("TEXT",Ref#)=Title of section   
        ;"               ITEMARRAY("TEXT",Ref#,#)=sequential parts of section   
        ;"                   ITEMARRAY("TEXT",3)="Dyspepsia"   
        ;"                   ITEMARRAY("TEXT",3,1)=part 1, e.g. text, e.g. [GROUP A&B]
        ;"                        ITEMARRAY("TEXT",3,1)="[GROUP]"
        ;"                        ITEMARRAY("TEXT",3,1,"GROUP")="A&B"
        ;"                   ITEMARRAY("TEXT",3,2)=part 2, e.g. name of inline table
        ;"                        ITEMARRAY("TEXT",3,2)="[TABLE]"   <-- signal this part is a table. 
        ;"                        ITEMARRAY("TEXT",3,2,"TABLE")=WT    <-- WT is name of table
        ;"                        ITEMARRAY("TEXT",3,2,"TEXT")=<TEXT OF TABLE>
        ;"                        ITEMARRAY("TEXT",3,2,"INLINE")=0 or 1            
        ;"                  ITEMARRAY("TEXT",Ref#,3)=part 3, e.g. more text
        ;"                  ITEMARRAY("TEXT",Ref#,4)=part 4, e.g. name of table   
        ;"                  ITEMARRAY("TEXT",Ref#,"GROUPX",#)=""   <-- index of GROUP nodes
        ;"                  ITEMARRAY("TEXT",Ref#,"TABLEX",#)=""   <-- index of TABLE nodes 
        ;"Output: ARRAY filled as below
        ;"          -    ARRAY(TIUIEN,"FULL",<SECTION>,<TOPIC NAME>,Line#)=text
        ;"          -    ARRAY(TIUIEN,<SECTION>,#)=<TOPIC NAME>^<First line of paragraph>
        ;"          -    ARRAY(TIUIEN,"TITLE",<TOPIC NAME>)=<topic name as originally in text>
        ;"          -    ARRAY(TIUIEN,"SEQ#",<SECTION>,#)=<TOPIC NAME>
        ;"               ARRAY(TIUIEN,"SEQ#",<SECTION>)=# OF TOPICS
        ;"          -    ARRAY(TIUIEN,"FULL","HPI",<TOPIC NAME>,Line#)=text
        ;"               ARRAY(TIUIEN,"HPI",#)=<TOPIC NAME>^<First line of paragraph>
        NEW IDX SET IDX=0
        FOR  SET IDX=$O(ITEMARRAY("TEXT",IDX)) QUIT:IDX'>0  DO
        . NEW TITLE SET TITLE=$G(ITEMARRAY("TEXT",IDX))
        . NEW TEXT,TEXTIDX SET TEXT="",TEXTIDX=0
        . FOR  SET TEXTIDX=$O(ITEMARRAY("TEXT",IDX,TEXTIDX)) QUIT:TEXTIDX'>0  DO
        . . NEW THISTEXT SET THISTEXT=$G(ITEMARRAY("TEXT",IDX,TEXTIDX))
        . . IF THISTEXT["[TABLE" SET THISTEXT=$G(ITEMARRAY("TEXT",IDX,TEXTIDX,"TEXT"))
        . . IF THISTEXT["[GROUP" SET THISTEXT=""
        . . SET TEXT=TEXT_THISTEXT
        . SET ARRAY(TIUIEN,"FULL","HPI",TITLE,IDX)=TEXT
        . ;"The below element is what is used to file the data
        . SET ARRAY(TIUIEN,"HPI",IDX)=TITLE_"^"_$EXTRACT(TEXT,1,45)
        . SET ARRAY(TIUIEN,"TITLE",TITLE)=TITLE
        . SET ARRAY(TIUIEN,"SEQ#","HPI",IDX)=TITLE
        NEW SECTION SET SECTION=""
        FOR  SET SECTION=$ORDER(ITEMARRAY("THREAD",SECTION)) QUIT:SECTION=""  DO
        . NEW IDX SET IDX=$GET(ITEMARRAY("THREAD",SECTION)) QUIT:IDX=0
        . NEW ADT SET ADT=$ORDER(ITEMARRAY("THREAD",SECTION,0)) QUIT:ADT'>0
        . NEW STR SET STR=$GET(ITEMARRAY("THREAD",SECTION,ADT)) QUIT:STR=""
        . NEW SUBIDX SET SUBIDX=""
        . FOR  SET SUBIDX=$ORDER(ITEMARRAY("THREAD",SECTION,ADT,SUBIDX)) QUIT:SUBIDX=""  DO
        . . SET STR=STR_" <<-AND->> "_$GET(ITEMARRAY("THREAD",SECTION,ADT,SUBIDX))
        . SET ARRAY(TIUIEN,"THREAD",IDX)=SECTION_"^"_STR
        QUIT
        ;"
ADDLINE(REF,TEXT) ;
        ;"Purpose: add text line to end of array.  
        ;"Input: REF -- Close reference, not including index
        NEW CT SET CT=$ORDER(@REF@(""),-1)+1
        SET @REF@(CT)=TEXT
        QUIT
        ;
FRMATTTL(TITLE)  ;"FORMAT TITLE
        ;"Purpose: to format titles, so that similar terms sort together.
        ;"Input: TITLE -- the title to format
        ;"     **uses TMGDOCSCANMODS in global scope
        IF $DATA(TMGDOCSCANMODS)=0 DO GETMODS(.TMGDOCSCANMODS)
        SET TITLE=$$UP^XLFSTR(TITLE)
        NEW DONE SET DONE=0
        FOR  DO  QUIT:DONE
        . SET DONE=1
        . FOR  QUIT:(",-./ "'[$EXTRACT(TITLE,1))!(TITLE="")  SET TITLE=$EXTRACT(TITLE,2,$LENGTH(TITLE))
        . IF $EXTRACT(TITLE,1)="""" DO  QUIT
        . . SET TITLE=$TRANSLATE(TITLE,"""","")
        . . SET DONE=0
        . NEW J SET J=""
        . NEW MODDONE SET MODDONE=0
        . FOR  SET J=$ORDER(TMGDOCSCANMODS(J)) QUIT:(J="")!(MODDONE=1)  DO
        . . NEW AMOD SET AMOD=$GET(TMGDOCSCANMODS(J)) QUIT:AMOD=""
        . . IF TITLE'[AMOD QUIT
        . . NEW PARTA,PARTB
        . . SET PARTA=$PIECE(TITLE,AMOD,1) QUIT:PARTA'=""
        . . SET PARTB=$$TRIM^XLFSTR($PIECE(TITLE,AMOD,2,999))
        . . FOR  QUIT:(",-./ "'[$EXTRACT(PARTB,1))!(PARTB="")  SET PARTB=$EXTRACT(PARTB,2,$LENGTH(PARTB))
        . . SET MODDONE=1
        . . IF $PIECE(PARTB,"(",1)="" SET DONE=1 QUIT
        . . SET TITLE=PARTB_" "_"("_AMOD_")"
        . . SET DONE=0
        . QUIT
        SET TITLE=$$TRIM^XLFSTR(TITLE)
        QUIT
        ;        
GETMODS(MODS)  ;
        ;"Purpose: to load up MODS variable.
        ;"Input: MODS -- PASS BY REFERENCE, AN OUT PARAMETER
        NEW I
        NEW TERM SET TERM=""
        FOR I=0:1 DO  QUIT:(TERM="<END>")!(TERM="")
        . SET TERM=$TEXT(GM+I)
        . SET TERM=$PIECE(TERM,";;""",2)
        . SET MODS(I)=TERM
        QUIT
GM      ;;"(B)
        ;;"(L)
        ;;"(R)
        ;;"C/O
        ;;"ABNORMAL
        ;;"ACUTE
        ;;"ATYPICAL
        ;;"APPARENT
        ;;"CHRONIC
        ;;"COMPLAINS OF
        ;;"COMPLAINTS OF
        ;;"COMPLAINT OF
        ;;"CONCERNS OF
        ;;"DIET CONTROLLED
        ;;"H/O
        ;;"EXCESSIVE
        ;;"EXCESS
        ;;"EXPOSURE TO
        ;;"ENVIRONMENTAL
        ;;"ENVIRONENTAL
        ;;"ENVIROMENTAL
        ;;"ENVIROMENTL
        ;;"EPIGASTRIC
        ;;"EPISODE OF
        ;;"EPISODES OF
        ;;"EPISODES
        ;;"ESSENTIAL
        ;;"GENERAL
        ;;"HISTORY OF
        ;;"HISTORY
        ;;"INTERMITENT
        ;;"INTERMITTANT
        ;;"INTERMITTENT
        ;;"INTERNAL
        ;;"INT.
        ;;"INT
        ;;"LEFT-SIDED
        ;;"LEFT
        ;;"LARGE
        ;;"LOW-GRADE
        ;;"LOW
        ;;"EXTERNAL
        ;;"MILD
        ;;"MINIMAL
        ;;"PERSISTENT
        ;;"POSSIBLE
        ;;"POSSIBLY
        ;;"PROBABLE
        ;;"PROBABLY
        ;;"PRIOR
        ;;"QUESTION OF
        ;;"REACTIVE
        ;;"RECURRENT
        ;;"RECENT
        ;;"RIGHT-SIDED
        ;;"RIGHT
        ;;"SEVERE
        ;;"???
        ;;"??
        ;;"?
        ;;"<END>
        ;        
CHKDIV(LINESTR,DIV) ;
        NEW RESULT SET RESULT=""
        IF LINESTR[DIV DO
        . SET RESULT=$PIECE(LINESTR,DIV,1)
        . IF $LENGTH(RESULT)>60 SET RESULT="" QUIT
        . IF (DIV="."),(+RESULT>0) DO
        . . ;"Look for e.g. 250.00 pattern (i.e. '.' is not a true divider)
        . . NEW PARTB SET PARTB=$PIECE(LINESTR,DIV,2)
        . . NEW NUM SET NUM=""
        . . NEW DONE SET DONE=0
        . . NEW I FOR I=1:1:$LENGTH(PARTB) DO  QUIT:DONE
        . . . NEW DIGIT SET DIGIT=$EXTRACT(PARTB,I)
        . . . IF "1234567890"'[DIGIT SET DONE=1 QUIT
        . . . SET NUM=NUM_DIGIT
        . . IF NUM'="" SET RESULT="" QUIT  ;"Found 250.00 pattern, so reject DIV
        . IF RESULT="" QUIT
        . SET LINESTR=$PIECE(LINESTR,DIV,2,999)
        QUIT RESULT
        ;        