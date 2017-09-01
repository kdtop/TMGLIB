TMGRX003 ;TMG/kst/Patient medication code; 08/25/17
       ;;1.0;TMG-LIB;**1**;08/25/17
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 08/25/17  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"EXTERNAL(ARR)  -- TURN MEDICATION ARRAY INTO OUTPUT STRING  
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"GETBRAND(IEN22733)  -- GET BRANDNAME
 ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"=======================================================================
 ;"Uses:  
 ;"=======================================================================
 ;        
EXTERNAL(ARR)  ;"TURN MEDICATION ARRAY INTO OUTPUT STRING  
  ;"Input: ARR -- PASS BY REFERENCE.  
  ;"         This is array as output by PARSELN^TMGRX001.  Format:
  ;"         ARR("ORIG")=The original unparsed line.  
  ;"         ARR("NOTE")=<any notes>  DELIMITED BY "<---" OR '>---" ...
  ;"         ARR("MED NAME")=<NAME OF RX>
  ;"         ARR("GENERIC")=<NAME OF RX>
  ;"         ARR("BRAND")=<NAME OF RX>
  ;"         ARR("MODIFIER")="ER"
  ;"         ARR("SIG")="{{ROUTE}} {{FREQ}} PRN HEADACHES"                                      
  ;"         ARR("SIG","FREQ" = BIDlp): step OVER// SHOW ARR                                       
  ;"         ARR("SIG","ROUTE" = PO
  ;"         ARR("DOSE IENS",<IENS>)=<DOSE_NAME> , e.g. TAB
  ;"         ARR("OTC")=1  <-- if specified as OTC
  ;"         ARR("DOSE")=#
  ;"         ARR("UNITS")=E.G. "MG"
  ;"         ARR("IEN50.607")=#  
  ;"         ARR("PREFACE")= e.g. HOLD, HOLDING, OFF
  ;"         ARR("IEN22733")=<IEN 22733>
  NEW RESULT SET RESULT=""
  NEW FAILURE SET FAILURE=0
  NEW IEN22733 SET IEN22733=+$GET(ARR("IEN22733")) GOTO:IEN22733'>0 A2SER  
  NEW PRINTMODE SET PRINTMODE=$PIECE($GET(^TMG(22733,IEN22733,"1.5")),"^",3)
  IF PRINTMODE="" SET PRINTMODE="GE"
  DO  ;"PREFACE
  . NEW PREFACE SET PREFACE=$GET(ARR("PREFACE"))
  . IF PREFACE'="" SET RESULT=RESULT_PREFACE
  DO  ;"OTC
  . IF $GET(ARR("OTC"))=1 SET RESULT=RESULT_"OTC "
  DO   ;"MED NAME
  . NEW CONTROLLED SET CONTROLLED=($PIECE($GET(^TMG(22733,IEN22733,"1.5")),"^",2)="Y")
  . IF "^GE^BO^"[PRINTMODE DO  QUIT:FAILURE  ;"GEneric name only, or Both.
  . . NEW RXNAME SET RXNAME=""
  . . SET RXNAME=$GET(ARR("GENERIC"))
  . . IF RXNAME="" SET RXNAME=$PIECE($GET(^TMG(22733,IEN22733,0)),"^",1)
  . . IF RXNAME="" SET RXNAME="["_$GET(ARR("MED NAME"))_"]"  ;"<-- shouldn't happen
  . . IF RXNAME="" SET FAILURE=1 QUIT
  . . IF CONTROLLED SET RXNAME=$$UP^XLFSTR(RXNAME)
  . . ELSE  SET RXNAME=$$LOW^XLFSTR(RXNAME)
  . . IF RESULT'="" SET RESULT=RESULT_" "
  . . SET RESULT=RESULT_RXNAME
  . IF "^BR^BO^"[PRINTMODE DO  QUIT:FAILURE  ;"BRand name only, or both.
  . . NEW RXNAME2 SET RXNAME2=$$GETBRAND(IEN22733) 
  . . IF RXNAME2="" SET RXNAME2=$GET(ARR("BRAND"))
  . . IF RXNAME2="" SET RXNAME2="["_$GET(ARR("MED NAME"))_"]"  ;"<-- shouldn't happen
  . . IF RXNAME2="" SET FAILURE=1 QUIT
  . . IF CONTROLLED SET RXNAME2=$$UP^XLFSTR(RXNAME2)
  . . ELSE  SET RXNAME2=$$CAP1ST^TMGSTUT2(RXNAME2)
  . . IF PRINTMODE="BO" SET RXNAME2="("_RXNAME2_")"
  . . IF RESULT'="" SET RESULT=RESULT_" "
  . . SET RESULT=RESULT_RXNAME2
  IF FAILURE GOTO A2SER
  DO  ;"MODIFIER
  . IF $GET(ARR("MODIFIER"))'="" SET RESULT=RESULT_ARR("MODIFIER")_" "
  DO  ;"DOSE
  . NEW DOSE SET DOSE=$GET(ARR("DOSE")) 
  . IF DOSE="" SET DOSE="_?_dose"
  . SET RESULT=RESULT_" "_DOSE
  DO  ;"UNITS
  . NEW UNITS SET UNITS=""
  . NEW UNITIEN SET UNITIEN=+$GET(ARR("IEN50.607"))
  . SET UNITS=$PIECE($GET(^PS(50.607,UNITIEN,0)),"^",1)
  . IF UNITS="" DO
  . . NEW IENS SET IENS=$ORDER(ARR("DOSE IENS",""))
  . . QUIT:IENS=""
  . . SET UNITS=$$GET1^DIQ(22733.03,IENS,.02)
  . IF UNITS="" DO
  . . NEW RAWUNITS SET RAWUNITS=$GET(ARR("UNITS"))
  . . IF RAWUNITS'="" SET UNITS="["_$GET(ARR("UNITS"))_"]"  
  . IF 1=0,UNITS="" DO   ;"<--- REMOVE 1=0, TO FORCE UNITS PROMPT
  . . SET UNITS="_?_units"
  . IF UNITS="" QUIT 
  . SET RESULT=RESULT_" "_$$LOW^XLFSTR(UNITS)
  DO  ;"SIG
  . NEW SIG MERGE SIG=ARR("SIG")
  . SET SIG=$$FIXSIG(.SIG)
  . IF SIG'="" SET RESULT=RESULT_"; "_SIG_" "
  DO  ;"NOTES
  . NEW NOTE SET NOTE=$GET(ARR("NOTE"))
  . IF NOTE'="" SET RESULT=RESULT_"Note: "_NOTE_" "
  ;
  GOTO A2SDN
A2SER  ;
  SET RESULT=$GET(ARR("ORIG"))
  IF RESULT="" SET RESULT="????"
  ELSE  SET RESULT="'"_RESULT_"'"
A2SDN ;
  QUIT RESULT
  ;
GETBRAND(IEN22733)  ;"GET BRANDNAME
  NEW RESULT SET RESULT=""
  NEW IDX SET IDX=0
  NEW BRANDS
  NEW ABRAND SET ABRAND=""
  FOR  SET ABRAND=$ORDER(^TMG(22733,IEN22733,1,"B",ABRAND)) QUIT:ABRAND=""  DO
  . SET IDX=IDX+1,BRANDS(IDX)=ABRAND
  IF IDX>1 DO
  . NEW SUBIEN SET SUBIEN=0
  . FOR  SET SUBIEN=$ORDER(^TMG(22733,IEN22733,1,SUBIEN)) QUIT:(SUBIEN'>0)!(RESULT'="")  DO
  . . NEW ZN SET ZN=$GET(^TMG(22733,IEN22733,1,SUBIEN,0))
  . . IF $PIECE(ZN,"^",2)="Y" SET RESULT=$PIECE(ZN,"^",1) 
  . IF RESULT="" SET RESULT=$GET(BRANDS(1)) 
  ELSE  SET RESULT=$GET(BRANDS(1))
  QUIT RESULT
  ;
FIXSIG(SIG) ;"FIX UP THE SIG PART OF LINE
  ;"NOTE: If wanted, could covert "PO" to "By mouth" etc if wanted
  NEW ARR DO LINE2ARR^TMGRX004(.ARR,SIG)
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(ARR("WORD",IDX)) QUIT:IDX'>0  DO
  . NEW WORD SET WORD=$GET(ARR("WORD",IDX))
  . IF WORD'["{{" QUIT
  . SET WORD=$PIECE($PIECE(WORD,"}}",1),"{{",2)
  . NEW NEWWORD SET NEWWORD=$GET(SIG(WORD))
  . IF NEWWORD="" QUIT
  . SET ARR("WORD",IDX)=NEWWORD
  SET SIG=$$ARR2LINE^TMGRX004(.ARR)
  QUIT SIG