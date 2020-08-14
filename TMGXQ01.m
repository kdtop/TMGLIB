TMGXQ01 ;TMG/kst-Custom alert manager ;8/23/19
              ;;1.0;TMG-LIB;**1**;8/23/19
 ;
 ;"TMG Alert manager
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 8/23/19 Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"=======================================================================
 ;
LISTALERTS(REF) ;
  ;"NOTE: called from ASK^XQALERT1
  ;"INPUT: REF = name of reference holding alerts to show, typically: '^TMP("XQ",$J,"XQA")'
  ;"Result: returns alert numbers that are selected from scroller
  WRITE #
  NEW TMGXQARR
  NEW OPTION 
  SET OPTION("NCS")=1
  SET OPTION("INDX")=1
  SET OPTION("ON SELECT")="HNDLSEL^TMGXQ01"
  SET OPTION("MULTISEL")=1
  SET OPTION("HEADER",1)="Select alert(s) for processing"
  SET OPTION("HEADER",2)="Use [INSERT] or [+] key, or [SPACE] key as first character."
  SET OPTION("HEADER",3)="[CTRL-A] toggle ALL. [ENTER] select and return."
  ;"SET OPTION("TMGXQ01","REF")=REF
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(@REF@(IDX)) QUIT:IDX'>0  DO
  . NEW INFO MERGE INFO=@REF@(IDX)
  . ;"INFO = 0 node for alert
  . ;"INFO(1) = IEN of the alert
  . ;"INFO(2) = data for the alert
  . ;"INFO(3) = comment for display
  . ;"INFO(4) = long info text
  . SET TMGXQARR($PIECE(INFO,"^",3),IDX)=""
  . ;"Eddie added the IF in case the text of the alert wasn't unique
  . ;"IF $DATA(TMGXQARR($PIECE(INFO,"^",3))) DO
  . ;". NEW NAME SET NAME=$PIECE(INFO,"^",3)_" "_IDX
  . ;". SET TMGXQARR(NAME)=IDX
  . ;"ELSE  DO
  . ;". SET TMGXQARR($PIECE(INFO,"^",3))=IDX
  QUIT $$LISTSEL^TMGUSRI4("TMGXQARR",.OPTION)
  ;
HNDLSEL(TMGPSCRLARR,OPTION,INFO) ;
  ;"Called as event handler from Scroller.  Set up in OPTION("ON SELECT") ABOVE.  
  NEW TEMPARR
  SET TEMPARR(+$GET(INFO("CURRENT LINE","RETURN")))=""
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(@TMGPSCRLARR@("SELECTED",IDX)) QUIT:IDX'>0  DO
  . NEW TEXT SET TEXT=$ORDER(@TMGPSCRLARR@(IDX,"")) QUIT:TEXT=""
  . NEW RETURN SET RETURN=+$GET(@TMGPSCRLARR@(IDX,TEXT)) QUIT:RETURN'>0
  . SET TEMPARR(RETURN)=""
  SET IDX=0,TMGRESULT=""
  FOR  SET IDX=$ORDER(TEMPARR(IDX)) QUIT:IDX'>0  DO
  . IF TMGRESULT'="" SET TMGRESULT=TMGRESULT_","
  . SET TMGRESULT=TMGRESULT_IDX 
  SET TMGSCLRMSG="^"  ;"Tells scroller to exit.  
  ;"NOTE: TMGRESULT is used in global scope, and is used for result of scroller.  
  QUIT
