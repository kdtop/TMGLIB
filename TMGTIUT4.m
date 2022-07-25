TMGTIUT4 ;TMG/kst-TIU-related code ; 5/20/15, 5/7/16
   ;;1.0;TMG-LIB;**1**;5/20/15
  ;
  ;"Code related to TIU components
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
  ;"TMGXTRA(TIUDA,TIUL,PREFIX) --Give information about child docs
  ;"FIREINH1(TIUDA) --FIRE INHERITED POST-SIGNATURE EVENT HANDLERS
  ;"FIREINHE(TIUDA,FLD)  --FIRE EVENTS INHERITED FROM TIU DOC-TYPE HEIRARCHY
;"=======================================================================
  ;"PRIVATE API FUNCTIONS
  ;"=======================================================================
  ;
  ;"=======================================================================
  ;"DEPENDENCIES: 
  ;"=======================================================================
  ;
TMGXTRA(TIUDA,TIUL,PREFIX)  ;"//kt added to give information about child docs
  ;"Purpose: this is a wedge function, called from SOURCE^TIUSRV
  ;"     It is to give additional information during the DETAIL view of notes in CPRS
  ;"Input: TIUDA -- The document being viewed
  ;"       TIUL -- PASSED BY REFERENCE.  This is the output line index
  ;"       PREFIX -- PASSED BY REFERENCE.  Expected to be blank on first call.
  ;"               Used for reenterant calls.  
  SET PREFIX=$GET(PREFIX)
  SET TIUDA=+$GET(TIUDA)
  NEW TITLE SET TITLE=$PIECE($GET(^TIU(8925,TIUDA,17)),"^",1)  
  NEW OUT SET OUT=PREFIX_"Doc #"_TIUDA
  IF TITLE'="" SET OUT=OUT_" ["_TITLE_"]"
  SET TIUL=TIUL+1 DO SET^TIUSRV(TIUL,2,OUT) SET OUT=""
  NEW SUBPREFIX 
  ;"IF PREFIX="" SET SUBPREFIX="+--"
  ;"ELSE  SET SUBPREFIX="   "_PREFIX
  SET SUBPREFIX="+--"_PREFIX
  NEW CHILD SET CHILD=0
  FOR  SET CHILD=$ORDER(^TIU(8925,"DAD",TIUDA,CHILD)) QUIT:+CHILD'>0  DO
  . DO TMGXTRA(CHILD,.TIUL,SUBPREFIX)  ;"//kt added to give information about child docs
  QUIT  
  ;
FIREINH1(TIUDA) ;"FIRE INHERITED POST-SIGNATURE EVENT HANDLERS
  DO 
  . NEW TMGSTACK ZSHOW "S":TMGSTACK
  . KILL ^TMG("TMP","FIREINH1^TMGTIU4","STACK")        
  . MERGE ^TMG("TMP","FIREINH1^TMGTIU4","STACK")=TMGSTACK        
  DO FIREINHE(TIUDA,4.9)  ;"FLD 4.9 IS POST-SIGNATURE CODE
  QUIT
  ;  
FIREINHE(TIUDA,FLD)  ;"FIRE EVENTS INHERITED FROM TIU DOC TYPE HEIRARCHY
  ;"NOTE: Events are fired in order from top-most document time, then child, then grand-child, etc
  ;"Input: TIUDA -- IEN in 8925, the document being considered.
  SET TIUDA=+$GET(TIUDA) IF TIUDA'>0 QUIT
  SET FLD=+$GET(FLD)  
  NEW IEN8925D1 SET IEN8925D1=+$GET(^TIU(8925,TIUDA,0))
  NEW ARR DO GETINHAR(.ARR,IEN8925D1,FLD)
  NEW IDX SET IDX=""
  FOR  SET IDX=$ORDER(ARR(IDX),-1) QUIT:IDX'>1  DO   ;"SKIP INDEX 1, THE ORIGINAL DOC
  . NEW CODE SET CODE=$GET(ARR(IDX,FLD)) IF CODE="" QUIT
  . XECUTE CODE
  QUIT
  ;
SHOWINH ;
  NEW X,Y,DIC SET DIC=8925.1,DIC(0)="MAEQ"
  DO ^DIC WRITE ! 
  QUIT:+Y'>0
  NEW ARR D GETINHAR(.ARR,+Y)
  ZWR ARR
  QUIT
  ;
GETINHAR(OUT,IEN8925D1,FLD)  ;"GET INHERITENCE STACK FOR GIVEN DOCUMENT TYPE
  SET IEN8925D1=+$GET(IEN8925D1) IF IEN8925D1'>0 QUIT
  SET FLD=+$GET(FLD)
  NEW PARENTIEN SET PARENTIEN=+$ORDER(^TIU(8925.1,"AD",IEN8925D1,0))
  NEW IDX SET IDX=+$ORDER(OUT(""),-1)+1
  SET OUT(IDX,"IEN")=IEN8925D1
  SET OUT(IDX,"NAME")=$PIECE($GET(^TIU(8925.1,IEN8925D1,0)),"^",1)
  SET OUT(IDX,FLD)=$$GET1^DIQ(8925.1,IEN8925D1_",",FLD)
  SET OUT(IDX,"PARENT")=PARENTIEN
  DO GETINHAR(.OUT,PARENTIEN,.FLD)
  QUIT
  ;
DUPNOTE(TMGRESULT,TMGDFN,TMGDUZ,TIUDATE,TITLEIEN)  ;" RPC: TMG SCANNER DOES NOTE EXIST
  ;"DOES THIS NOTE TITLE ALREADY EXISTS FOR DATE LOCATION USER AUTHOR COMBO?
  SET TMGRESULT="1^OK"
  ;"IF NOTE TITLE ALREADY EXISTS, SET TO -1^NOTE EXISTS
  NEW THISDATE SET THISDATE=$P(TIUDATE,".",1)_".000001"
  SET TIUDATE=$P(TIUDATE,".",1)
  FOR  SET THISDATE=$O(^TIU(8925,"ZTMGPTDT",TMGDFN,THISDATE)) QUIT:(THISDATE'>0)  DO
  . IF $P(THISDATE,".",1)'=TIUDATE QUIT
  . NEW THISIEN SET THISIEN=0
  . FOR  SET THISIEN=$O(^TIU(8925,"ZTMGPTDT",TMGDFN,THISDATE,THISIEN)) QUIT:(THISIEN'>0)  DO
  . . NEW THISAUTHOR,THISTYPE
  . . SET THISTYPE=$P($G(^TIU(8925,THISIEN,0)),"^",1)
  . . IF THISTYPE'=TITLEIEN QUIT
  . . SET THISAUTHOR=$P($G(^TIU(8925,THISIEN,12)),"^",2)
  . . IF THISAUTHOR=TMGDUZ SET TMGRESULT="-1^"_$P($G(^TIU(8925.1,TITLEIEN,0)),"^",1)_" TITLE ALREADY EXISTS FOR THIS DATE"
  QUIT
  ;"