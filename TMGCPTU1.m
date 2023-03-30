TMGCPTU1 ;TMG/kst-HL7 Handling CPTs; 4/26/19
              ;;1.0;TMG-LIB;**1**; 4/26/19
  ;
  ;"Handling CPT 
  ;
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;"Copyright (c) 4/26/19  Kevin S. Toppenberg MD
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
  ;"ADDCPT -- Interactive method to add a new CPT
  ;"
  ;"=======================================================================
  ;" API - Private Functions
  ;"=======================================================================
  ;"=======================================================================
  ;"Dependancies
  ;"=======================================================================
  ;"
  ;"=======================================================================
  ;"==============================================================
  ;
ADDCPT  ;"Interactive method to add a new CPT
  NEW CPT,CPTNAME,CPTDESC
  NEW TMGFDA,TMGMSG,TMGIEN
GETCPT
  READ !,"CPT TO ADD:",CPT
  IF CPT="^" QUIT
  IF CPT?5.N=0 WRITE !,"CPT SHOULD BE EXACTLY 5 DIGITS IN LENGTH.",! GOTO GETCPT
  WRITE !
  IF $DATA(^ICPT(CPT)) DO  GOTO ACEDIT
  . WRITE "THIS ENTRY SEEMS TO EXIST. OPENING FOR EDIT NOW.",!
  READ "CPT SHORT NAME:",CPTDESC
  ;"
  SET TMGIEN(1)=CPT
  SET TMGFDA(81,"+1,",.01)=CPT
  SET TMGFDA(81,"+1,",2)=CPTDESC
  DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG) DO  GOTO ACDN
  . DO SHOWDIER^TMGDEBU2(.TMGMSG)
  WRITE !,!,"STUB ENTRY CREATED, BUT ADDITIONAL FIELDS STILL NEEDED...",!
ACEDIT
  NEW DA,DIE,DR SET DA=CPT,DIE=81,DR=".01;2;8;50;60;61;62"
  DO ^DIE 
ACDN
  QUIT
  ;"
