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
GETCPT ;
  READ !,"ENTER CPT TO ADD OR EDIT (^ to abort): ",CPT WRITE !
  IF CPT["^" QUIT
  IF $LENGTH(CPT)'=5 WRITE "CPT code should be exactly 5 characters",! GOTO GETCPT
  SET CPT=$$UP^XLFSTR(CPT)
  SET TMGIEN=+$ORDER(^ICPT("B",CPT,0))
  IF (+CPT=CPT),(TMGIEN=0),($DATA(^ICPT(+CPT))>0) DO  GOTO ACDN
  . WRITE "ERROR: Record found at IEN=",+CPT,", but not in index.  Aborting",!
  IF TMGIEN>0 DO  GOTO ACEDIT
  . WRITE "THIS ENTRY ALREADY EXISTS. OPENING FOR EDIT NOW.",!
  ;  
  IF +CPT'=CPT DO
  . ;"NOTE: If putting in custom CPT codes at an arbitrary IEN, then these could be 
  . ;"      potentially be clobbered with future CPT patch from VA.  So will put
  . ;"      these as very large IEN numbers: 227,000,000 - 228,000,000
  . SET TMGIEN=+$ORDER(^ICPT(228000000),-1)
  . IF TMGIEN<227000000 SET TMGIEN=227000000
  . ELSE  SET TMGIEN=TMGIEN+1
  ELSE  DO
  . SET TMGIEN=+CPT
  ;
  READ "ENTER CPT SHORT NAME: ",CPTDESC WRITE !
  IF (CPTDESC="")!(CPTDESC["^") WRITE !,"Restarting...",! GOTO GETCPT
  SET TMGIEN(1)=TMGIEN
  SET TMGFDA(81,"+1,",.01)=CPT
  SET TMGFDA(81,"+1,",2)=CPTDESC
  DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG) DO  GOTO ACDN
  . DO SHOWDIER^TMGDEBU2(.TMGMSG)
  WRITE !,!,"STUB ENTRY CREATED, BUT ADDITIONAL FIELDS STILL NEEDED...",!
  ;
ACEDIT
  NEW FIELDS SET FIELDS=".01;2;3;6;8;50;60;61;62"
  NEW NUMFLDS SET NUMFLDS=$LENGTH(FIELDS,";")
  NEW DA,DIE,DR SET DA=TMGIEN,DIE=81,DR=".01;2;3;6;8;50;60;61;62"
  NEW IDX,TMGOUT
  FOR IDX=1:1:NUMFLDS DO
  . NEW AFLD SET AFLD=$PIECE(FIELDS,";",IDX) QUIT:AFLD=""
  . DO FIELD^DID(81,AFLD,"","MULTIPLE-VALUED;LABEL","TMGOUT","TMGMSG")
  . NEW LABEL SET LABEL=$GET(TMGOUT("LABEL"))
  . IF $GET(TMGOUT("MULTIPLE-VALUED"))=1 DO
  . . WRITE !,"EDIT ",LABEL,": <MULTIPLE-VALUED>"
  . SET DR=AFLD
  . DO ^DIE 
ACDN
  WRITE !,"Goodbye",!
  QUIT
  ;"
CPTACTIVE(IEN81) ;"Is CPT code (passed as IEN) active currently?
  ;"Alternative option, figure out how to use STATCHK^ICPTAPIU
  NEW RESULT
  NEW ZN SET ZN=$GET(^ICPT(+IEN81,0))
  SET RESULT=($PIECE(ZN,"^",4)'=1)
  QUIT RESULT
  ;
CPTACTV2(CPTCODE) ;"Is CPT code (pass by actual CPT code) active currently?
  NEW IEN81 SET IEN81=+$ORDER(^ICPT("B",CPTCODE,""))
  IF IEN81'>0 QUIT 0
  QUIT $$CPTACTIVE(IEN81)
  ;