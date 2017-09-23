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
