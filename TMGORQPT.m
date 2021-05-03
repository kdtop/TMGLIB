TMGORQPT  ;TMG/kst-ORQPT ;6/23/15, 3/24/21
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
   NEW I,TMGDFN,NAME 
   SET I=0
   FOR  SET I=$ORDER(PTLIST(I)) QUIT:I'>0  DO
   . SET TMGDFN=$PIECE(PTLIST(I),"^",1)
   . SET NAME=$PIECE(PTLIST(I),"^",2)
   . SET TMGRESULT(I)=$$GETPDATA(TMGDFN,NAME)
   SET TMGRESULT(0)="DFN,NAME,DOB,GENDER,ENCOUNTER DATE,LAST DX,CPT"
   QUIT
   ;"
GETPDATA(TMGDFN,NAME)  ;"
   ;"Purpose: To return patient data based on the provided TMGDFN
   NEW TMGRESULT SET TMGRESULT=""
   NEW DOB,GENDER,ENCOUNTER,DX,CPT
   NEW ZN SET ZN=$GET(^DPT(TMGDFN,0))
   SET DOB=$PIECE(ZN,"^",3)
   SET GENDER=$PIECE(ZN,"^",2)
   SET ENCOUNTER=""
   SET DX=""
   SET CPT=""
   SET TMGRESULT=TMGDFN_","_NAME_","_DOB_","_GENDER_","_ENCOUNTER_","_DX_","_CPT
   QUIT TMGRESULT
   ;" 