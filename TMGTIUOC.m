TMGTIUOC ;TMG/kst-TIU OBJECTS ; 06/30/15
         ;;1.0;TMG-LIB;**1,17**;06/30/15
 ;"
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
 ;"Dependencies: TMGSTUT2, TMGTIUOJ
 ;"=======================================================================
 ;
ASMNTOBJ(DATA,OPTION) ;"Assemble note object output (continuation from TMGTIUOB)
  ;"Input: DATA -- pass by reference.  format:
  ;"          DATA("NAME")=<Title text>
  ;"          DATA("ID")=<Problem IEN>
  ;"          DATA("PRIOR",<FMDATE>,"TEXT",#)=<long line of text>  ;"<--- CHANGED ... FIX!
  ;"          DATA("TABLES",<TABLE NAME>)=""
  ;"       OPTION -- PASS BY REFERENCE.  OPTIONAL.  Format:
  ;"          OPTION("PLAINTEXT")=1 <-- if plain text requested.  Default is HTML
  ;"                               For plaintext, ascii art used.  Width = 60 chars
  ;"NOTE: uses DFN in global scope
  ;"Result: One long line of text that will be inserted into CPRS to comprise text object
  ;
  ;"Desired output:
  ;" +.......................................................................+"
  ;" :  <NAME> (ID: <ID>)                                                    :"
  ;" +...........+...........................................................+"
  ;" :  1/1/12   : TEXT HERE... (IN ITALICS)                                 :"
  ;" :           : TEXT HERE... (IN ITALICS)                                 :"
  ;" +...........+...........................................................+"
  ;" :  2/2/13   : TEXT HERE... (IN ITALICS)                                 :"
  ;" :           : TEXT HERE... (IN ITALICS)                                 :"
  ;" +...........+...........................................................+"
  ;" :  DATA     : [HTN:                                                     :"
  ;" :           : ...                                                       :"
  ;" :           : ...                                                       :"
  ;" :           :                                                           :"
  ;" :           : [DM-2:                                                    :"
  ;" :           : ...                                                       :"
  ;" :           : ...                                                       :"
  ;" :           :                                                           :"
  ;" +...........+...........................................................+"
  ;" :  TODAY    : (blank space for provider to add new text)                :"
  ;" :           :                                                           :"
  ;" +...........+...........................................................+"
  ;
  NEW RESULT SET RESULT=""
  NEW PLAINTEXT SET PLAINTEXT=+$GET(OPTION("PLAINTEXT"))
  IF PLAINTEXT=1 DO
  . SET RESULT=$$PLNTABLE(.DATA) ;"Assemble PLAIN TEXT table.  
  ELSE  DO
  . SET RESULT=$$HTMTABLE(.DATA) ;"Assemble HTML  table.
  QUIT RESULT
  ;  
HTMTABLE(DATA) ;"Assemble HTML  table. 
  ;"Input: DATA -- pass by reference.  format:
  ;"          DATA("NAME")=<Title text>
  ;"          DATA("ID")=<Problem IEN>
  ;"          DATA("PRIOR",<FMDATE>,"TEXT",#)=<long line of text>
  ;"          DATA("TABLES",<TABLE NAME>)=""
  ;"NOTE: uses DFN in global scope
  ;"Result: One long line of text that will be inserted into CPRS to comprise text object
  NEW TMGRESULT
  NEW COL2WIDTH 
  NEW PROBIEN SET PROBIEN=+$GET(DATA("ID"))
  NEW NAME SET NAME=$GET(DATA("NAME"))
  IF NAME["(SCT " SET NAME=$PIECE(NAME,"(SCT",1)
  NEW ICD SET ICD=$GET(DATA("ICD"))
  IF ICD'="" SET NAME=$$TRIM^XLFSTR(NAME)_" ("_ICD_")"
  NEW HEADTEXT SET HEADTEXT="<B>"_NAME_"</B> "
  DO SETHEAD(.TMGRESULT,PROBIEN_":"_ICD,HEADTEXT)
  ;" Loop through dates
  NEW FMDATES SET FMDATE=0
  SET COL2WIDTH=0
  NEW ADDED SET ADDED=0 
  ;"Add rows for prior entries. 
  FOR  SET FMDATE=$ORDER(DATA("PRIOR",FMDATE)) QUIT:FMDATE'>0  DO
  . DO STARTROW(.TMGRESULT,"PRIOR:"_FMDATE)
  . ;"NEW Y SET Y=FMDATE D DD^%DT
  . SET TMGRESULT=TMGRESULT_$$FMTE^XLFDT(FMDATE,"2D")
  . SET ADDED=1
  . DO NEXTCOL(.TMGRESULT)
  . NEW LINE,TEXT
  . SET LINE=0,TEXT=""
  . FOR  SET LINE=$ORDER(DATA("PRIOR",FMDATE,"TEXT",LINE)) QUIT:LINE'>0  DO
  . . SET TEXT=TEXT_$GET(DATA("PRIOR",FMDATE,"TEXT",LINE))
  . SET TMGRESULT=TMGRESULT_"<I>"_TEXT_"</I>"
  . IF $LENGTH(TEXT)>COL2WIDTH SET COL2WIDTH=$LENGTH(TEXT)
  . DO ENDROW(.TMGRESULT)
  IF (1=0)&'ADDED DO
  . DO STARTROW(.TMGRESULT,"PRIOR")
  . SET TMGRESULT=TMGRESULT_"PRIOR"
  . DO NEXTCOL(.TMGRESULT)
  . ;"SET TMGRESULT=TMGRESULT_" "
  . DO ENDROW(.TMGRESULT)
  ;" Loop through tables
  SET ADDED=0 
  NEW TABLENAME SET TABLENAME=""
  FOR  SET TABLENAME=$ORDER(DATA("TABIX",TABLENAME)) QUIT:TABLENAME=""  DO
  . DO STARTROW(.TMGRESULT,"TABLE:"_TABLENAME)
  . SET TMGRESULT=TMGRESULT_"DATA"
  . SET ADDED=1
  . DO NEXTCOL(.TMGRESULT)
  . NEW TABLETEXT,TBLTAG
  . SET TBLTAG="["_TABLENAME_"]"
  . NEW TABLEARR
  . ;"SET TABLETEXT=$$GETTABLX^TMGTIUO6(+$G(DFN),TBLTAG,.TABLEARR)
  . IF $$GETTABLX^TMGTIUO6(+$G(DFN),TBLTAG,.TABLEARR)
  . SET TABLETEXT=""
  . NEW IDX SET IDX=""
  . FOR  SET IDX=$ORDER(TABLEARR(IDX)) QUIT:+IDX'=IDX  DO
  . . IF TABLETEXT'="" SET TABLETEXT=TABLETEXT_"<br>"
  . . IF $LENGTH(TABLEARR(IDX))>COL2WIDTH SET COL2WIDTH=$LENGTH(TABLEARR(IDX))
  . . SET TABLETEXT=TABLETEXT_TABLEARR(IDX)
  . SET TMGRESULT=TMGRESULT_TABLETEXT   
  . DO ENDROW(.TMGRESULT)
  IF 'ADDED DO
  . DO STARTROW(.TMGRESULT,"TABLE")
  . SET TMGRESULT=TMGRESULT_"DATA"
  . DO NEXTCOL(.TMGRESULT)
  . ;"SET TMGRESULT=TMGRESULT_" "
  . DO ENDROW(.TMGRESULT)
  DO STARTROW(.TMGRESULT,"TODAY")
  SET TMGRESULT=TMGRESULT_"TODAY"
  DO NEXTCOL(.TMGRESULT)
  IF COL2WIDTH<20 DO ADDSPCS(.TMGRESULT,20-COL2WIDTH) ;"
  DO ENDROW(.TMGRESULT)
  DO ENDTBL(.TMGRESULT)
  QUIT TMGRESULT
  ;
SETHEAD(OUT,ID,TEXT)  ;" SET THE HEADER OF THE TABLE
  NEW S SET S="<table "
  SET S=S_"border=1 "
  SET S=S_"id="""_ID_""" "
  SET S=S_"style=""border-collapse:collapse"" "  
  SET S=S_"><caption style=""background-color:#80ccff"" >"_TEXT_"</caption>"
  SET OUT=$$HTML(S,1)
  QUIT
  ;
STARTROW(OUT,ID) ;" START A ROW
  SET ID=$GET(ID)
  NEW S SET S="<tr style=""background-color:white;"""
  SET S=S_"><td style=""background-color:#ccebff;"" "
  IF ID'="" SET S=S_"id="""_ID_""" "
  SET S=S_">"
  SET OUT=OUT_$$HTML(S)
  QUIT
  ;
ENDROW(OUT)  ;"END A ROW
  SET OUT=OUT_$$HTML("</td></tr>",1)
  QUIT
  ;
NEXTCOL(OUT)  ;"END AND START NEW COLUMN
  SET OUT=OUT_$$HTML("</td><td>")
  QUIT
  ;
ENDTBL(OUT)  ;"END THE TABLE
  SET OUT=OUT_$$HTML("</table>",1)
  QUIT
  ;
ADDSPCS(OUT,NUM)  ;"ADD SPACES
  NEW I FOR I=1:1:NUM SET OUT=OUT_"&nbsp;"
  QUIT
  ;
HTML(S,ADDCRLF)
  SET ADDCRLF=+$GET(ADDCRLF)
  NEW RESULT SET RESULT="{HTML:"_S_"}"
  IF ADDCRLF SET RESULT=RESULT_"<NO-BR>"_$CHAR(13,10)
  QUIT RESULT
  ;"----------------------------------------------------------------------------
  ;
EXTRCTAB(OUT,IEN8925)  ;"extract information from table, as constructed by HTMTABLE() above
  ;"Input: OUT -- PASS BY REFERENCE, see format below
  ;"       IEN8925 -- The text document that contains HTML table.
  ;"Uses DUZ from global scope
  ;"Output:  OUT(FMDATE,"TEXT")=<long line of all narrative text in TODAY row
  ;"         OUT(FMDATE,"TABLE",<TABLE NAME>)=""  Names of any tables held in DATA row.
  ;"Result: none
  SET IEN8925=+$GET(IEN8925)
  NEW AUTHOR SET AUTHOR=$PIECE($GET(^TIU(8925,IEN8925,12)),"^",2)
  NEW FMDT SET FMDT=$PIECE($GET(^TIU(8925,IEN8925,0)),"^",7)
  NEW ALL SET ALL=$$WP2STR^TMGSTUT2($NAME(^TIU(8925,IEN8925,"TEXT"))," ",9999999)
  NEW SPEC,TAG 
  FOR TAG="table","td","tr" DO
  . NEW UTAG SET UTAG=$$UP^XLFSTR(TAG)
  . SET SPEC("<"_TAG)="<"_UTAG
  . SET SPEC("</"_TAG)="</"_UTAG
  SET SPEC("id=")="ID=",SPEC("&nbsp;")=" "
  SET ALL=$$REPLACE^XLFSTR(ALL,.SPEC)  
  FOR  QUIT:ALL'["<TABLE"  DO
  . NEW ONETABLE
  . SET ONETABLE=$$PULLTAG(.ALL,"<TABLE","</TABLE>")
  . FOR  QUIT:ONETABLE'["<TR"  DO
  . . NEW PARAMS
  . . NEW ONEROW SET ONEROW=$$PULLTAG(.ONETABLE,"<TR","</TR>",.PARAMS)
  . . IF $$PULLTAG(.ONEROW,"<TD","</TD>") ;"discard first cell
  . . NEW TEXT SET TEXT=$$TRIM^XLFSTR($$PULLTAG(.ONEROW,"<TD","</TD>"))
  . . IF PARAMS("ID")="TABLE" DO
  . . . FOR  QUIT:TEXT'["["  DO
  . . . . NEW TEMP SET TEMP=""
  . . . . SET TEXT=$PIECE(TEXT,"[",2,99)
  . . . . IF TEXT["]" SET TEMP=$PIECE(TEXT,"]",1),TEXT=$PIECE(TEXT,"]",2,99)
  . . . . IF TEMP'="" SET OUT(FMDT,"TABLE",TEMP)=""
  . . IF PARAMS("ID")="TODAY" DO
  . . . IF AUTHOR'=DUZ DO
  . . . . SET AUTHOR=$PIECE($GET(^VA(200,AUTHOR,0)),"^",1)
  . . . . SET TEXT=TEXT_" --Author: "_AUTHOR
  . . . SET OUT(FMDT,"TEXT")=TEXT
  ;
  QUIT
  ;
PULLTAG(STR,OPENTAG,CLOSETAG,PARAMS)  ;"remove and return all text between OPENTAG and CLOSETAG
  ;"Input: STR -- PASS BY REFERENCE.  The source string, to be modified
  ;"       OPENTAG -- open format of starting tag, e.g. '<TABLE
  ;"       CLOSETAG -- closed format of ending tag, .e.g. '</TABLE>'
  ;"       PARAMS -- PASS BY REFERENCE.  The contents of the opening tag
  ;"          PARAMS("ID")=ID parameter, or "" if none. 
  NEW PARTA,PARTB,RESULT,POS
  SET PARTA=$PIECE(STR,OPENTAG,1)
  SET RESULT=$PIECE(STR,OPENTAG,2)
  SET POS=$FIND(RESULT,">")
  SET PARAMS=$EXTRACT(RESULT,1,POS-2)
  SET RESULT=$EXTRACT(RESULT,POS,$LENGTH(RESULT))  
  SET RESULT=$PIECE(RESULT,CLOSETAG,1)
  SET PARTB=$PIECE(STR,CLOSETAG,2,999)
  SET STR=PARTA_PARTB
  ;
  NEW ID SET ID=""
  NEW TEMP SET TEMP=""
  IF PARAMS["ID=" DO
  . SET TEMP=$PIECE(PARAMS,"ID=",2)  
  ELSE  IF OPENTAG="<TR",RESULT["ID=" DO
  . SET TEMP=$PIECE(RESULT,"ID=",2)
  IF TEMP'="" DO
  . IF $EXTRACT(TEMP,1)="""" DO
  . . SET TEMP=$EXTRACT(TEMP,2,$LENGTH(TEMP)),TEMP=$PIECE(TEMP,"""",1)
  . ELSE  DO
  . . SET TEMP=$PIECE(TEMP," ",1)
  . SET ID=TEMP
  SET PARAMS("ID")=ID
  QUIT RESULT  
  ;"--PLAIN TEXT TABLE STUFF BELOW ---------------------------------------------
PLNTABLE(DATA) ;"Assemble PLAIN TEXT table. 
  ;"Input: DATA -- pass by reference.  format:
  ;"          DATA("NAME")=<Title text>
  ;"          DATA("ID")=<Problem IEN>
  ;"          DATA("PRIOR",<FMDATE>,<PROB/TOPIC NAME>,#)=<long line of text>
  ;"          DATA("TABLES",<TABLE NAME>)=""
  ;"Result: One long line of text that will be inserted into CPRS to comprise text object
  NEW WIDTH SET WIDTH=80
  NEW COL1WIDTH SET COL1WIDTH=10  ;"WIDTH INSIDE CELL
  NEW COL2WIDTH SET COL2WIDTH=WIDTH-COL1WIDTH-3  ;"WIDTH INSIDE CELL
  NEW RESULT SET RESULT=$$HLINE(WIDTH)_$CHAR(13,10)
  NEW CONTENT
  ;"HEADER ROW -------------
  SET CONTENT(1,"WIDTH")=WIDTH-2
  SET CONTENT(1)=$GET(DATA("NAME"))_" (ID: "_$GET(DATA("ID"))_") "
  SET RESULT=RESULT_$$HCELLS(WIDTH,.CONTENT)_$CHAR(13,10) ;"line with text in specified columns
  SET RESULT=RESULT_$$HLINE(WIDTH)_$CHAR(13,10)
  ;
  ;" Date / narritive rows -----------------
  NEW FMDATES SET FMDATE=0
  FOR  SET FMDATE=$ORDER(DATA("PRIOR",FMDATE)) QUIT:FMDATE'>0  DO
  . NEW PROBNAME SET PROBNAME=""
  . FOR  SET PROBNAME=$ORDER(DATA("PRIOR",FMDATE,PROBNAME)) QUIT:PROBNAME=""  DO
  . . IF PROBNAME="TABLE" QUIT
  . . NEW LONGTEXT SET LONGTEXT=""
  . . NEW IDX SET IDX=0
  . . FOR  SET IDX=$ORDER(DATA("PRIOR",FMDATE,PROBNAME,IDX)) QUIT:+IDX'>0  DO
  . . . NEW LINE SET LINE=$GET(DATA("PRIOR",FMDATE,PROBNAME,IDX))
  . . . IF (LONGTEXT'=""),($EXTRACT(LONGTEXT,$LENGTH(LONGTEXT))'=" ") SET LONGTEXT=LONGTEXT_" "
  . . . SET LONGTEXT=LONGTEXT_LINE
  . . NEW WRAPARR IF $$SPLITLN^TMGSTUT2(LONGTEXT,.WRAPARR,COL2WIDTH) 
  . . SET RESULT=RESULT_$$HARRCELL(WIDTH,COL1WIDTH,$$FMTE^XLFDT(FMDATE,"2D"),.WRAPARR) ;"Returns multiple lines if needed
  ;
  ;"Data / Tables rows ------------------------ 
  NEW TABLENAME SET TABLENAME=""
  FOR  SET TABLENAME=$ORDER(DATA("TABIX",TABLENAME)) QUIT:TABLENAME=""  DO
  . NEW TEMP SET TEMP=0
  . NEW TABARR IF $$GETTABLX^TMGTIUO6(DFN,TABLENAME,.TABARR)  ;"drop result
  . SET TEMP(+TEMP+1)="["_TABLENAME_"]",TEMP=TEMP+1
  . NEW IDX SET IDX=""
  . FOR  SET IDX=$ORDER(TABARR("KEY-VALUE",IDX)) QUIT:IDX=""  DO
  . . NEW LINE SET LINE=$$TRIM^XLFSTR($GET(TABARR("KEY-VALUE",IDX,"LINE")))
  . . SET TEMP(+TEMP+1)=LINE,TEMP=TEMP+1
  . SET TEMP(+TEMP+1)=" ",TEMP=TEMP+1
  . ;" Data tables row
  . IF $DATA(TEMP)\10>0 SET RESULT=RESULT_$$HARRCELL(WIDTH,COL1WIDTH,"DATA",.TEMP) 
  ;  
  ;" Room for today's narrative row
  KILL TEMP SET TEMP(1)=" ",TEMP(2)=" "
  SET RESULT=RESULT_$$HARRCELL(WIDTH,COL1WIDTH,"TODAY",.TEMP)
  QUIT RESULT
  ;  
HLINE(WIDTH,COLS)  ;"Return text line with specified columns connector points.
  ;"Input: WIDTH -- total width
  ;"       COLS -- OPTIONAL.  String describing columns.  E.g. "10,24" -- means draw
  ;"                vertical column-dividing lines as positions 10 and 24
  ;"Result: returns text of line
  SET WIDTH=+$GET(WIDTH) IF WIDTH'>0 SET WIDTH=60
  SET COLS=$$TRIM^XLFSTR($GET(COLS)) 
  IF (COLS'=""),($EXTRACT(COLS,$LENGTH(COLS))'=",") SET COLS=COLS_","
  SET COLS=COLS_WIDTH
  NEW RESULT SET RESULT="+"
  FOR  QUIT:COLS=""  DO
  . FOR  QUIT:$LENGTH(RESULT)'<(+COLS-1)  SET RESULT=RESULT_"="
  . SET COLS=$PIECE(COLS,",",2,999)
  . SET RESULT=RESULT_"+"
  QUIT RESULT
  ;
HARRCELL(WIDTH,COL1WIDTH,COL1TITLE,TEXTARR) ;"HORIZONTAL ARR CELLS
  ;"Input: WIDTH -- total width
  ;"       CONTENT.  PASS BY REFERENCE.  Format:
  ;"          e.g. CONTENT(1,"WIDTH")=10   <-- shows column 1 width is 10   ;"WIDTH INSIDE CELL
  ;"               CONTENT(1,"ALIGN")="C"   <-- shows column 1 centered.  OPTIONAL
  ;"               CONTENT(1)=<Text to put into cell> <-- cut off it doesn't fit in width
  ;"               CONTENT(2,"WIDTH")=30   <-- shows column 1 width is 30   ;"WIDTH INSIDE CELL
  ;"               CONTENT(2,"ALIGN")="C"   <-- shows column 1 centered.  OPTIONAL
  ;"               CONTENT(2,"ARR",#)=<Text to put into cell> <-- cut off it doesn't fit in width
  ;"Result: returns text of line
  NEW RESULT SET RESULT=""
  NEW COL2WIDTH SET COL2WIDTH=WIDTH-COL1WIDTH-3  ;"WIDTH INSIDE CELL
  KILL CONTENT
  SET CONTENT(1,"WIDTH")=COL1WIDTH
  SET CONTENT(1,"ALIGN")="C"
  SET CONTENT(1)=COL1TITLE
  SET CONTENT(2,"WIDTH")=COL2WIDTH
  SET CONTENT(2,"ALIGN")="L"
  SET CONTENT(2)=$GET(TEXTARR(1))
  SET RESULT=RESULT_$$HCELLS(WIDTH,.CONTENT)_$CHAR(13,10) ;"line with text in specified columns
  SET IDX=1
  FOR  SET IDX=$ORDER(TEXTARR(IDX)) QUIT:IDX'>0  DO  ;"Add 2..n lines of narrative text
  . KILL CONTENT
  . SET CONTENT(1,"WIDTH")=COL1WIDTH
  . SET CONTENT(1)=""
  . SET CONTENT(2,"WIDTH")=COL2WIDTH
  . SET CONTENT(2,"ALIGN")="L"
  . SET CONTENT(2)=$GET(TEXTARR(IDX))
  . SET RESULT=RESULT_$$HCELLS(WIDTH,.CONTENT)_$CHAR(13,10) ;"line with text in specified columns
  SET RESULT=RESULT_$$HLINE(WIDTH,COL1WIDTH+2)_$CHAR(13,10)
  QUIT RESULT
  ;
HCELLS(WIDTH,CONTENT) ;"Return text line with text in specified columns
  ;"Input: WIDTH -- total width
  ;"       CONTENT.  PASS BY REFERENCE.  Format:
  ;"          e.g. CONTENT(1,"WIDTH")=10   <-- shows column 1 width is 10.  WIDTH INSIDE CELLS
  ;"               CONTENT(1,"ALIGN")="C"   <-- shows column 1 centered.  OPTIONAL
  ;"                       "L" is left-aligned (Default), "R" is right-aligned.
  ;"               CONTENT(1)=<Text to put into cell> <-- cut off it doesn't fit in width
  ;"Result: returns text of line
  NEW RESULT SET RESULT=""""
  NEW IDX SET IDX=""
  FOR  SET IDX=$ORDER(CONTENT(IDX)) QUIT:+IDX'>0  DO
  . NEW WIDTH SET WIDTH=+$GET(CONTENT(IDX,"WIDTH")) IF WIDTH=0 SET WIDTH=10
  . NEW ALIGN SET ALIGN=$EXTRACT($GET(CONTENT(IDX,"ALIGN")),1) 
  . IF (ALIGN="")!("LRC"'[ALIGN) SET ALIGN="L"
  . NEW TEMP SET TEMP=$$CELLTEXT($GET(CONTENT(IDX)),WIDTH,ALIGN)
  . SET RESULT=RESULT_TEMP_""""
  QUIT RESULT
  ;
CELLTEXT(TEXT,WIDTH,ALIGN) ;"GET TEXT TO FIT INTO SPECIFIED WIDTH
  ;"Input: TEXT -- the string to insert
  ;"       WIDTH -- the allowed width of the cell. This is the number
  ;"               of characters INSIDE the cell (not including the characters
  ;"               needed to draw the cell walls.  
  ;"       ALIGN -- "L","R", or "C"
  ;"Results: return string that is WIDTH in length
  NEW RESULT
  SET WIDTH=$GET(WIDTH) 
  NEW DELTA SET DELTA=WIDTH-$LENGTH(TEXT)
  IF DELTA<0 DO
  . SET TEXT=$EXTRACT(TEXT,1,WIDTH-4)_"..."
  . SET DELTA=1
  IF DELTA>0 DO
  . IF ALIGN="R" DO
  . . SET RESULT=$$RJ^XLFSTR(TEXT,WIDTH-1)_" " 
  . ELSE  IF ALIGN="C" DO
  . . SET RESULT=$$CJ^XLFSTR(TEXT,WIDTH)
  . ELSE  DO
  . . SET RESULT=" "_$$LJ^XLFSTR(TEXT,WIDTH-1)
  ELSE  IF DELTA=0 DO
  . SET RESULT=TEXT  
  QUIT RESULT
  ;
