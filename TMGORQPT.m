TMGORQPT  ;TMG/kst-ORQPT ;6/23/15
      ;;1.0;TMG-LIB;**1**;6/23/15
      ;
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
GLSTRPT(TMGRESULT,LSTSTR) ;
   ;"Purpose: This RPC is designed to take the list details
   ;"         from the Edit List/Teams form and return
   ;"         a comma delimited array, which will be stored
   ;"         in the user selected directory.
   ;"Input: TMGRESULT -- Return array
   ;"       LSTSTR -- IEN^Name^...
   NEW TMGDEBUG SET TMGDEBUG=1
   IF TMGDEBUG=1 DO
   . SET LSTSTR=$GET(^TMP("TMG","GLSTRPT"))
   ELSE  DO
   . SET ^TMP("TMG","GLSTRPT")=LSTSTR
   NEW LSTIEN,LSTNAME
   SET LSTIEN=$PIECE(LSTSTR,"^",1)
   SET LSTNAME=$PIECE(LSTSTR,"^",2)
   NEW PTLIST 
   DO TEAMPTS^ORQPTQ1(.PTLIST,LSTIEN)
   NEW I,DFN,NAME 
   SET I=0
   FOR  SET I=$ORDER(PTLIST(I)) QUIT:I'>0  DO
   . SET DFN=$PIECE(PTLIST(I),"^",1)
   . SET NAME=$PIECE(PTLIST(I),"^",2)
   . SET TMGRESULT(I)=$$GETPDATA(DFN,NAME)
   SET TMGRESULT(0)="DFN,NAME,DOB,GENDER,ENCOUNTER DATE,LAST DX,CPT"
   QUIT
   ;"
GETPDATA(DFN,NAME)  ;"
   ;"Purpose: To return patient data based on the provided DFN
   NEW TMGRESULT SET TMGRESULT=""
   NEW DOB,GENDER,ENCOUNTER,DX,CPT
   NEW ZN SET ZN=$GET(^DPT(DFN,0))
   SET DOB=$PIECE(ZN,"^",3)
   SET GENDER=$PIECE(ZN,"^",2)
   SET ENCOUNTER=""
   SET DX=""
   SET CPT=""
   SET TMGRESULT=DFN_","_NAME_","_DOB_","_GENDER_","_ENCOUNTER_","_DX_","_CPT
   QUIT TMGRESULT
   ;" 