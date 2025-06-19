TMGHTM3 ;TMG/kst-HTML utilities ; 9/12/17
         ;;1.0;TMG-LIB;**1,17**;9/12/17
 ;
 ;"Utility functions related to documents with HTML formatting
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 9/12/17  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"---------------------------------------------------------------------------
 ;"PUBLIC FUNCTIONS
 ;"---------------------------------------------------------------------------
 ;"HTMTABLE(OUTREF,CONTENTS,IDX)  --MAKE HTML CODE FOR A TABLE
 ;
 ;"---------------------------------------------------------------------------
 ;"PRIVATE FUNCTIONS
 ;"---------------------------------------------------------------------------
 ;
 ;"---------------------------------------------------------------------------
 ;"DEPENDENCIES:
 ;
 ;"---------------------------------------------------------------------------
 ; 
HTMTABLE(OUTREF,CONTENTS,IDX)  ;"MAKE HTML CODE FOR A TABLE
  ;"Copied and modified from SETHTML^TMGRPT2 by E. Hagood.
  ;"Input: OUTREF -- AN OUT PARAMETER 
  ;"          @OUTREF@(#)= <line of HTML>
  ;"       CONTENTS -- INPUT DATA.  Pass by reference.  Format:
  ;"            CONTENTS("ATTRS")=Attributes to put into cell <TABLE> tag. OPTIONAL
  ;"            CONTENTS("BGCOLOR")=color value (e.g. #c4e3ed) to set background color for entire table. OPTIONAL
  ;"            CONTENTS(ROW#,"HEADING")=1 <-- if 1 then is a header row, otherwise not. OPTIONAL  
  ;"            CONTENTS(ROW#,"ATTRS")=Attributes to put into cell <TR> tag. OPTIONAL  
  ;"            CONTENTS(ROW#,"BGCOLOR")=color value (e.g. #c4e3ed) to set background color for row.  OPTIONAL
  ;"            CONTENTS(ROW#,COL#)=<CELL DATA>
  ;"            CONTENTS(ROW#,COL#,"ATTRS")=Attributes to put into cell <TD> tag. OPTIONAL
  ;"            CONTENTS(ROW#,COL#,"PRE")=HTML TAGS (e.g. <B>) to put in cell before data.  OPTIONAL
  ;"            CONTENTS(ROW#,COL#,"POST")=HTML TAGS (e.g. </B>) to put in cell after data.  OPTIONAL
  ;"            CONTENTS(ROW#,COL#,"BGCOLOR")=color value (e.g. #c4e3ed) to set background color for cell  OPTIONAL
  ;"          (note: first ROW# should be >0)
  ;"       IDX -- OPTIONAL.  Default=1.  The index to start puting HTML codes into @OUTREF at.  
  ;"Results -- none
  SET IDX=+$GET(IDX,1)
  NEW LINE,ROW,ATTRS,BGCOLOR
  SET ATTRS=$GET(CONTENTS("ATTRS")) IF ATTRS'="" SET ATTRS=" "_ATTRS
  SET BGCOLOR=$GET(CONTENTS("BGCOLOR")) IF BGCOLOR'="" SET BGCOLOR=" bgcolor="_BGCOLOR
  SET @OUTREF@(IDX)="<TABLE"_ATTRS_BGCOLOR_">",IDX=IDX+1
  SET ROW=0
  FOR  SET ROW=$ORDER(CONTENTS(ROW)) QUIT:ROW'>0  DO
  . SET ATTRS=$GET(CONTENTS(ROW,"ATTRS")) IF ATTRS'="" SET ATTRS=" "_ATTRS
  . SET BGCOLOR=$GET(CONTENTS(ROW,"BGCOLOR")) IF BGCOLOR'="" SET BGCOLOR=" bgcolor="_BGCOLOR
  . SET @OUTREF@(IDX)="  <TR"_ATTRS_BGCOLOR_">",IDX=IDX+1
  . NEW HEADER SET HEADER=+$GET(CONTENTS(ROW,"HEADING"))
  . NEW CELLTAG SET CELLTAG=$SELECT(HEADER:"TH",1:"TD")
  . NEW LINE SET LINE=""
  . NEW COL SET COL=0
  . FOR  SET COL=$ORDER(CONTENTS(ROW,COL)) QUIT:COL'>0  DO
  . . SET ATTRS=$GET(CONTENTS(ROW,COL,"ATTRS")) IF ATTRS'="" SET ATTRS=" "_ATTRS
  . . SET BGCOLOR=$GET(CONTENTS(ROW,COL,"BGCOLOR")) IF BGCOLOR'="" SET BGCOLOR=" bgcolor="_BGCOLOR
  . . SET LINE="    <"_CELLTAG_ATTRS_BGCOLOR_">"
  . . NEW VALUE SET VALUE=$GET(CONTENTS(ROW,COL))
  . . SET VALUE=$$SYMENC^MXMLUTL(VALUE)
  . . SET LINE=LINE_$GET(CONTENTS(ROW,COL,"PRE"))_VALUE_$GET(CONTENTS(ROW,COL,"POST"))_"</"_CELLTAG_">"
  . . SET @OUTREF@(IDX)=LINE,IDX=IDX+1,LINE=""
  . SET @OUTREF@(IDX)="  </TR>",IDX=IDX+1,LINE=""
  SET @OUTREF@(IDX)="</TABLE>",IDX=IDX+1,LINE=""
  QUIT        
  ;
HTMLMODE(TMGRESULT,MODE) ;"
  ;"Purpose: Set HTML mode global value
  SET TMGRESULT="1^SUCCESSFUL"
  SET TMGCPRSHTMLMODE=MODE
  QUIT
  ;"
  ;"----------------------------------------------------------------
  ;"----------------------------------------------------------------
  ;
GETCODE(OUTREF,TAG,ROUTINE,IDX) ;"Read HTM (or other) code into OUTREF
  ZLINK ROUTINE
  NEW OFFSET
  IF $GET(IDX)'>0 SET IDX=$ORDER(@OUTREF@(""),-1)+1
  NEW DONE SET DONE=0
  FOR OFFSET=1:1 DO  QUIT:DONE
  . NEW LINE SET LINE=$TEXT(@TAG+OFFSET^@ROUTINE)
  . SET LINE=$PIECE(LINE,";;",2)
  . IF LINE["DONE_WITH_HTML" SET DONE=1 QUIT
  . SET @OUTREF@($I(IDX))=LINE
  QUIT
  ;  
HTMT2DOC(OUTREF,TAG,RTN,INFO) ;"HTML Template 2 HTML DOCument, using source info
  ;"INPUT: OUTREF -- PASS BY NAME.  E.g. "MYARR".  SEE HTMTA2DOC
  ;"       TAG -- TAG name where template can be obtained, via $TEXT() read
  ;"       RTN -- ROUTINE name where template can be obtained, via $TEXT() read
  ;"       INFO -- SEE HTMTA2DOC
  ;"OUTPUT: composite HTML is put into OUT array.  Format: OUT(#)=<line of HTML>
  ;"Result: none
  ;"NOTE: Template should be stored in mumps file with following format:
  ;"      -- HTML code should begin on line immediately following TAG
  ;"      -- each line should begin with ';;', which will be stripped.  
  ;"      -- at end of HTML code, there should be separate terminating line containing: DONE_WITH_HTML
  ;"     The routine will be ZLINKED prior to reading, so any possible edits will be immediatedly effected
  NEW TEMPLATE
  DO GETCODE("TEMPLATE",TAG,RTN)
  DO HTMTA2DOC(OUTREF,.TEMPLATE,.INFO)
  QUIT
  ;
HTMTA2DOC(OUTREF,TEMPLATE,INFO) ;"HTML Template ARRAY 2 HTML DOCument, using source info
  ;"INPUT: OUTREF -- PASS BY NAME.  E.g. "MYARR".  Format: @OUTREF@(#)=<line of HTML>
  ;"       TEMPLATE -- PASS BY REFERENCE.  Format: TEMPLATE(#)=<line of HTML template>
  ;"              NOTE: Template is expected to contain tagged-text blocks that will be replaced with 
  ;"                    test stored in INFO array
  ;"                   Tag format: {@@@[<block_name>]@@@}  e.g. {@@@[TOC]@@@} <--- replaced with 'TOC' content
  ;"                      If any tags are found without corresponding block in INFO, they will be deleted. 
  ;"                      block_name is NOT case-sensitive in TEMPLATE.  Should be UPPERCASE in INFO array
  ;"       INFO -- PASS BY REFERENCE.  Format:
  ;"                   INFO("DATA",<block_name,#)=line of HTML-valid text to put into template
  ;"OUTPUT: composite HTML is put into OUT array.  Format: @OUTREF@(#)=<line of HTML>
  ;"Result: none
  ;"NOTE: The limit of string length in yottadb is 1 mb.  Web pages can exceed this.  So
  ;"      this function will anticipate that it will be broken up into multiple lines,
  ;"      as user might generate during editing.  The key point being that the 
  ;"      tag names must all be on one line.  No attempt will be made to consider
  ;"      line wrapping.  
  ;
  NEW TIDX SET TIDX=0  ;"TEMPLATE IDX
  NEW OIDX SET OIDX=0  ;"OUTPUT IDX
  NEW OTAG SET OTAG="{@@@["  ;"OPEN TAG
  NEW CTAG SET CTAG="]@@@}"  ;"CLOSE TAG
  FOR  SET TIDX=$ORDER(TEMPLATE(TIDX)) QUIT:TIDX'>0  DO
  . NEW LINE SET LINE=$GET(TEMPLATE(TIDX)) QUIT:LINE=""
  . IF LINE[OTAG DO  QUIT
  . . NEW PARTA SET PARTA=$PIECE(LINE,OTAG,1)
  . . NEW TAGNAME SET TAGNAME=$PIECE($PIECE(LINE,OTAG,2),CTAG,1)
  . . SET TAGNAME=$$UP^XLFSTR(TAGNAME)
  . . NEW PARTB SET PARTB=$PIECE(LINE,CTAG,2)
  . . NEW DIDX SET DIDX=0  ;"DATA IDX
  . . SET @OUTREF@($INCR(OIDX))=PARTA  ;"This will be on it's own line. 
  . . FOR  SET DIDX=$ORDER(INFO("DATA",TAGNAME,DIDX)) DO  QUIT:DIDX'>0
  . . . NEW DATALINE SET DATALINE=$GET(INFO("DATA",TAGNAME,DIDX))
  . . . SET @OUTREF@($INCR(OIDX))=DATALINE
  . . SET @OUTREF@($INCR(OIDX))=PARTB  ;"This will be on it's own line.
  . SET @OUTREF@($INCR(OIDX))=LINE
  QUIT
  ;