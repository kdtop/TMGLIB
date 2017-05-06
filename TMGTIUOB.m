TMGTIUOB ;TMG/kst-TIU OBJECTS ; 06/30/15, 4/11/17
         ;;1.0;TMG-LIB;**1,17**;06/30/15
 ;"
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
 ;"PUBLIC FUNCTIONS
 ;"=======================================================================
 ;"
 ;"=======================================================================
 ;"PRIVATE FUNCTIONS
 ;"=======================================================================
 ;"
 ;"=======================================================================
 ;"=======================================================================
 ;
TEST ;
  NEW DFN SET DFN=14191 ;"//ZZTEST,CAROLYN SUE
  NEW PROBIEN SET PROBIEN=24163
  NEW TXT SET TXT=$$PROBSUM(PROBIEN)
  WRITE TXT,!
  QUIT
  ;
NOTECOMP(INFO)  ;"TIU TEXT OBJECT entry point for TMG NOTE COMPONENT
  ;"Input: INFO -- 'IEN^NAME^ICD^HtmlMode' -- passed in from CPRS template.  
  ;"                IEN in PROBLEM file (9000011)
  ;"NOTE: the input parameters are set by the text template that includes the TMG NOTE COMPONENT
  ;"      text object.  E.g. |TMG NOTE COMPONENT{%PROBIEN%^%PROBNAME%^%PROBICD%^%HTML%}|
  ;"Result: One long line of text that will be inserted into CPRS to comprise text object
  QUIT $$PROBSUM(.INFO)
  ;
PROBSUM(INFO) ;"TIU TEXT OBJECT entry point for TMG NOTE PROB SUMMARY
  ;"Input: INFO -- 'IEN^NAME^ICD^HtmlMode' -- passed in from CPRS template.  IEN in PROBLEM file (9000011)
  ;"NOTE: Uses DFN in global scope
  ;"Result: One long line of text that will be inserted into CPRS to comprise text object
  ;
  SET INFO=$GET(INFO)
  IF '$DATA(TMGZZ) NEW TMGZZ SET TMGZZ=0
  IF TMGZZ=1 DO
  . SET INFO=$GET(^TMG("TMP","TMGTIUOB","INFO"))
  . SET DFN=$GET(^TMG("TMP","TMGTIUOB","DFN"))
  ELSE  DO
  . SET ^TMG("TMP","TMGTIUOB","INFO")=INFO
  . SET ^TMG("TMP","TMGTIUOB","DFN")=DFN
  NEW IEN SET IEN=+INFO
  NEW PROBNAME SET PROBNAME=$PIECE(INFO,"^",2)
  NEW ICD SET ICD=$PIECE(INFO,"^",3)
  NEW HTML SET HTML=($PIECE(INFO,"^",4)="1")  
  NEW DATA
  SET DATA("ID")=IEN
  SET DATA("NAME")=PROBNAME
  SET DATA("ICD")=ICD
  DO GETTOPIC(.DATA,DFN,IEN,HTML)  ;"Load information, if avail, for topics linked to problem.  
  DO GETCOMP(.DATA,DFN,IEN) ;"Get prior info based on problem IEN
  DO ADJDATA(.DATA)  ;"ADJUST DATA
  ZLINK "TMGTIUOC"  ;"<--remove once TMGTIUOC development finished.
  NEW OPTION SET OPTION("PLAINTEXT")='HTML   
  NEW TMGRESULT SET TMGRESULT=$$ASMNTOBJ^TMGTIUOC(.DATA,.OPTION)  ;"Assemble data into output text
  QUIT TMGRESULT
  ;
GETTOPIC(OUT,DFN,PROBIEN,HTML)  ;"Load information, if avail, for topics linked to problem.
  ;"Input: OUT -- PASS BY REFERENCE.  AN OUT PARAMETER.  Format:
  ;"          OUT("PRIOR",<FMDATE>,"TEXT",#)=<long line of text>
  ;"          OUT("PRIOR",<FMDATE>,"TABLE",<TABLE NAME>)=""   <-- no need to include old table text
  ;"          OUT("TABIX",<TABLE NAME>,<FMDATE>)=""
  ;"       DFN -- Patient IEN
  ;"       PROBIEN -- IEN in PROBLEM file -- IEN9000011
  ;"       HTML: 1 if text is HTML
  ;"Result: none
  NEW TOPICARR DO TOP4PROB(.TOPICARR,IEN) ;"Get topic names for given problem IEN
  NEW ATOPIC SET ATOPIC=""
  FOR  SET ATOPIC=$ORDER(TOPICARR(ATOPIC)) QUIT:ATOPIC=""  DO
  . NEW TEMP DO GETOPIC1(.TEMP,ATOPIC,HTML) ;"Get prior note info based on one topic name
  . NEW DT SET DT=0
  . FOR  SET DT=$ORDER(TEMP("PRIOR",DT)) QUIT:DT=""  DO
  . . NEW CT SET CT=+$ORDER(OUT("PRIOR",DT,"TEXT",""),-1)
  . . NEW LINE SET LINE=""
  . . NEW IDX SET IDX=0
  . . FOR  SET IDX=$ORDER(TEMP("PRIOR",DT,ATOPIC,IDX)) QUIT:IDX'>0  DO
  . . . NEW STR SET STR=$GET(TEMP("PRIOR",DT,ATOPIC,IDX)) QUIT:STR=""
  . . . IF LINE'="",$EXTRACT(LINE,$LENGTH(LINE))'=" " SET LINE=LINE_" "
  . . . SET LINE=LINE_STR  
  . . SET OUT("PRIOR",DT,"TEXT",CT+1)=ATOPIC_": "_LINE
  . . MERGE OUT("PRIOR",DT,"TABLE")=TEMP("PRIOR",DT,"TABLE")
  . . MERGE OUT("TABIX")=TEMP("TABIX")
  QUIT
  ;
TOP4PROB(OUT,PROBIEN) ;"Get topic names for given problem IEN
  ;"Input: OUT -- PASS BY REFERENCE.  AN OUT PARAMETER.  Format:
  ;"         OUT(<TOPIC NAME>)=""
  ;"       PROBIEN -- IEN in PROBLEM file (9000011)
  ;"NOTE: Uses DFN in global scope
  ;"Results: none
  NEW TEMP,IN
  SET IN(1)="GET^"_DFN_"^PROB="_IEN
  DO TOPRBLNK^TMGTIUT3(.TEMP,.IN)
  NEW LINE SET LINE=$PIECE($GET(TEMP(1)),"^",5)
  NEW TOPICS SET TOPICS=$PIECE(LINE,"TOPIC=",2)
  NEW IDX FOR IDX=1:1:$LENGTH(TOPICS,",") DO
  . NEW ATOPIC SET ATOPIC=$PIECE(TOPICS,",",IDX)
  . IF ATOPIC'="" SET OUT(ATOPIC)=""  
  QUIT
  ;
GETOPIC1(OUT,TOPICNAME,HTML) ;"Get prior note info based on one topic name
  ;"Input: OUT -- PASS BY REFERENCE.  AN OUT PARAMETER.  Format:
  ;"          OUT("PRIOR",<FMDATE>,<TOPICNAME>,#)=<long line of text>
  ;"          OUT("PRIOR",<FMDATE>,"TABLE",<TABLE NAME>)=""   <-- no need to include old table text
  ;"          OUT("TABIX",<TABLE NAME>,<FMDATE>)=""
  ;"       TOPICNAME -- Name of topic to get data for
  ;"       HTML -- 1 if note is in HTML format
  ;"NOTE: Uses DFN in global scope
  ;"Result: none   
  ;
  NEW TEMP  
  NEW OPTION 
  ;"SET OPTION("LAST")=1  ;"Get only last entry
  DO TOPICS^TMGTIUT3(.TEMP,"SUM1",DFN,"HPI",TOPICNAME,0,9999999,.OPTION)
  IF +$GET(TEMP(0))'=1 QUIT
  NEW TEXT
  NEW CURDT SET CURDT=""
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(TEMP(IDX)) QUIT:+IDX'>0  DO
  . NEW LINE SET LINE=$GET(TEMP(IDX)) QUIT:LINE=""
  . NEW FMDATE SET FMDATE=$PIECE(LINE,"^",1) QUIT:FMDATE=""
  . IF CURDT="" SET CURDT=FMDATE
  . NEW CT SET CT=$PIECE(LINE,"^",2) QUIT:CT=""
  . NEW TXT SET TXT=$PIECE(LINE,"^",3,99)
  . NEW LAST SET LAST=(+$ORDER(TEMP(IDX))'>0)
  . NEW DATECHANGE SET DATECHANGE=((CURDT'=FMDATE)&(CURDT'=""))
  . NEW JDX SET JDX=+$ORDER(TEXT(""),-1)+1
  . IF 'DATECHANGE SET TEXT(JDX)=TXT  
  . IF LAST!DATECHANGE DO
  . . NEW TABLES DO PRCSTEXT(.TABLES,.TEXT,.HTML)
  . . NEW ATABLE SET ATABLE=""
  . . FOR  SET ATABLE=$ORDER(TABLES(ATABLE)) QUIT:ATABLE=""  DO
  . . . SET OUT("PRIOR",CURDT,"TABLE",ATABLE)=""
  . . . SET OUT("TABIX",ATABLE,CURDT)="" 
  . . NEW IDX2 SET IDX2=0
  . . MERGE OUT("PRIOR",CURDT,TOPICNAME)=TEXT
  . . KILL TEXT
  . . SET CURDT=FMDATE
  . IF DATECHANGE SET TEXT(JDX)=TXT
  QUIT
  ;
PRCSTEXT(OUTTABLES,TEXT,HTML)  ;"PROCESS TEXT -- FIX TEXT ARRAY HERE -- strip tables etc....
  ;"Input: OUTTABLES -- PASS BY REFERENE.  AN OUT PARAMETER.  Format
  ;"           OUTTABLES(<TABLE NAME>)=""
  ;"       TEXT -- PASS BY REFERENCE.  AND IN AND OUT PARAMETER.  Format:
  ;"           TEXT(#)=<line of text>
  ;"       HTML -- 1 if note is in HTML format
  ;"Result: none  
  NEW FOUNDTABLE SET FOUNDTABLE=0
  NEW TABLES,TEMPTEXT,NEWLINE,INLINEMODE,TERMCHARS
  NEW PARTA,PARTB
  SET NEWLINE=1,INLINEMODE=0,TERMCHARS=""
  NEW TABLES DO GETTABLS^TMGTIUP3(.TABLES,.HTML)
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(TEXT(IDX)) QUIT:+IDX'>0  DO
  . NEW LINE SET LINE=$$TRIM^XLFSTR($GET(TEXT(IDX)))
  . IF FOUNDTABLE'=1 DO
  . . NEW TABLENAME SET TABLENAME=$$HASTABL(LINE,.TABLES) ;"DOES LINE CONTAIN TABLE START?
  . . IF TABLENAME'="" DO  ;"table name found
  . . . SET FOUNDTABLE=1
  . . . SET OUTTABLES(TABLENAME)="" 
  . . . NEW IEN SET IEN=+$ORDER(^TMG(22708,"B",TABLENAME,0))
  . . . NEW ZN SET ZN=$GET(^TMG(22708,IEN,0))
  . . . IF $PIECE(ZN,"^",5)="I" DO   ;"HANDLE INLINE TABLES HERE
  . . . . SET PARTA=$PIECE(LINE,TABLENAME,1)
  . . . . SET TERMCHARS=$PIECE($GET(^TMG(22708,IEN,4)),"^",1)
  . . . . SET PARTB=$PIECE(LINE,TABLENAME,2)
  . . . . IF PARTB[TERMCHARS DO
  . . . . . SET LINE=PARTA_PARTB
  . . . . . SET FOUNDTABLE=0,INLINEMODE=0,TERMCHARS=""
  . . . . ELSE  DO
  . . . . . SET LINE=PARTA
  . . . . SET TEMPTEXT(NEWLINE)=LINE,NEWLINE=NEWLINE+1
  . . ELSE  SET TEMPTEXT(NEWLINE)=LINE,NEWLINE=NEWLINE+1
  . ELSE  DO  ;"Already in middle of table.
  . . IF INLINEMODE DO
  . . . IF LINE]TERMCHARS DO
  . . . . SET LINE=$PIECE(LINE,TERMCHARS,2)
  . . . . SET FOUNDTABLE=0,INLINEMODE=0
  . . . ELSE  DO
  . . . . SET LINE=""
  . . ELSE  DO
  . . . NEW ENDFOUND SET ENDFOUND=$$TABLEND^TMGTIUP3(LINE,.PARTB)
  . . . IF PARTB="<BR>" SET PARTB=""
  . . . IF ENDFOUND DO
  . . . . SET LINE=PARTB
  . . . . SET FOUNDTABLE=0
  . . . ELSE  DO
  . . . . SET LINE=""
  . . IF LINE'="" DO
  . . . SET TEMPTEXT(NEWLINE)=LINE,NEWLINE=NEWLINE+1
  ;"Remove trailing blank lines etc
  SET IDX=""
  FOR  SET IDX=$ORDER(TEMPTEXT(IDX),-1) QUIT:+IDX'>0  DO  QUIT:IDX=-1
  . IF $$TRIM^XLFSTR($GET(TEMPTEXT(IDX)))="" KILL TEMPTEXT(IDX) QUIT
  . SET IDX=-1
  KILL TEXT MERGE TEXT=TEMPTEXT
  QUIT
  ;
HASTABL(LINE,TABLES) ;"DOES LINE CONTAIN TABLE START?
  ;"Result: "" if not found, or '<TABLENAME>'   
  NEW RESULT SET RESULT=""
  NEW ATABLE SET ATABLE=""
  FOR  SET ATABLE=$ORDER(TABLES(ATABLE)) QUIT:(ATABLE="")!(RESULT'="")  DO
  . IF LINE'[ATABLE QUIT
  . SET RESULT=$$HTML2TXS^TMGHTM1($PIECE($PIECE(ATABLE,"]",1),"[",2))
  QUIT RESULT
  ;
GETCOMP(OUT,DFN,PROBIEN) ;"Get prior info based on problem IEN
  ;"Input: OUT -- PASS BY REFERENCE.  AN OUT PARAMETER.  Format:
  ;"          OUT("PRIOR",<FMDATE>,"TEXT",#)=<long line of text>
  ;"          OUT("PRIOR",<FMDATE>,"TABLE",<TABLE NAME>)=""   <-- no need to include old table text
  ;"          OUT("TABIX",<TABLE NAME>,<FMDATE>)=""
  ;"       DFN -- Patient IEN
  ;"       PROBIEN -- IEN in PROBLEM file -- IEN9000011
  ;"Result: none      
  ;
  NEW TEMPOUT DO READSEC^TMGTIU11(DFN,PROBIEN,"ATMGPROB",0,.TEMPOUT)
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(TEMPOUT(IDX)) QUIT:+IDX'>0  DO
  . NEW ENTRY SET ENTRY=$GET(TEMPOUT(IDX))  ;"Format is DOC^IEN8925^SDATE^AUTHOR^SUBJECT
  . IF $PIECE(ENTRY,"^",1)'="DOC" QUIT
  . NEW IEN8925 SET IEN8925=+$PIECE(ENTRY,"^",2) QUIT:IEN8925'>0
  . NEW SDATE SET SDATE=$PIECE(ENTRY,"^",3)
  . NEW AUTHOR SET AUTHOR=$PIECE(ENTRY,"^",4)
  . NEW SUBJECT SET SUBJECT=$PIECE(ENTRY,"^",5)
  . DO COMPEXTC(.OUT,IEN8925,SDATE,AUTHOR,SUBJECT)
  QUIT
  ;
COMPEXTC(OUT,IEN8925,SDATE,AUTHOR,SUBJECT)  ;"COMPONENT EXTRACT
  ;"Input: OUT -- PASS BY REFERENCE.  AN OUT PARAMETER.  Format:
  ;"          OUT("PRIOR",<FMDATE>,"TEXT",#)=<long line of text>
  ;"          OUT("PRIOR",<FMDATE>,"TABLE",<TABLE NAME>)=""   <-- no need to include old table text
  ;"          OUT("TABIX",<TABLE NAME>,<FMDATE>)=""
  NEW TEMP DO EXTRCTAB^TMGTIUOC(.TEMP,IEN8925)
  NEW FMDT SET FMDT=""
  FOR  SET FMDT=$ORDER(TEMP(FMDT)) QUIT:+FMDT'>0  DO
  . NEW TEXT SET TEXT=$GET(TEMP(FMDT,"TEXT"))
  . IF TEXT'="" SET OUT("PRIOR",FMDT,"TEXT",1)=TEXT
  . NEW TABLE SET TABLE=""
  . FOR  SET TABLE=$ORDER(TEMP(FMDT,"TABLE",TABLE)) QUIT:TABLE=""  DO
  . . SET OUT("PRIOR",FMDT,"TABLE",TABLE)=""
  . . SET OUT("TABIX",TABLE,FMDT)=""
  QUIT
  ;
ADJDATA(DATA)  ;"ADJUST DATA
  ;"Input: DATA -- pass by reference.  format:
  ;"          DATA("NAME")=<Title text>
  ;"          DATA("ID")=<Problem IEN>
  ;"          DATA("PRIOR",<FMDATE>,"TEXT",#)=<long line of text>
  ;"          DATA("PRIOR",<FMDATE>,"TABLE",<TABLE NAME>)=""   <-- no need to include old table text
  ;"          DATA("TABIX",<TABLE NAME>,<FMDATE>)=""
  ;"OUTPUT:  DATA is modified as below.  Format
  ;"          DATA("NAME")=<Title text>
  ;"          DATA("ID")=<Problem IEN>
  ;"          DATA("PRIOR",<FMDATE>,"TEXT",#)=<long line of text>
  ;"          DATA("TABLES",<TABLE NAME>)=""
  ;"NOTE: Here I can remove tables if absent from a later note compared 
  ;"     to present at an earlier note.  
  QUIT;

