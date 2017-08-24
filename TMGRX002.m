TMGRX002 ;TMG/kst/Patient medication code; 08/23/17
       ;;1.0;TMG-LIB;**1**;08/23/17
 ;
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 08/23/17  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"=======================================================================
 ;"Uses:  
 ;"=======================================================================
 ;        
BIGLIST ;
  KILL ^TMG("TMP","EXAMPLE MEDS")
  NEW MIN SET MIN=$ORDER(^DPT(0))
  NEW MAX SET MAX=$ORDER(^DPT("@"),-1)
  NEW STARTH SET STARTH=$H
  NEW DFN SET DFN=0
  FOR  SET DFN=$ORDER(^DPT(DFN)) QUIT:DFN'>0  DO
  . IF DFN#10=0 DO PROGBAR^TMGUSRI2(DFN,"",1,MAX,60,STARTH)
  . NEW ARR,TEMP
  . DO MEDLIST^TMGTIUOJ(.TEMP,DFN,.ARR)
  . KILL ARR("KEY-VALUE")
  . NEW IDX SET IDX=0 
  . FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:IDX'>0  DO
  . . NEW LINE SET LINE=$$TRIM^XLFSTR($$UP^XLFSTR($GET(ARR(IDX)))) 
  . . IF $EXTRACT(LINE,1)="*" QUIT
  . . IF LINE["[MEDICATION" QUIT
  . . SET ^TMG("TMP","EXAMPLE MEDS",LINE)=""
  QUIT
  ;
SELRX ;
  NEW REF SET REF=$NAME(^TMG("TMP","EXAMPLE MEDS"))
  NEW OUT
  DO SELECTOR^TMGUSRI3(REF,"OUT","Pick meds")
  NEW ABORT SET ABORT=0
  NEW LINE SET LINE=""
  FOR  SET LINE=$ORDER(OUT(LINE)) QUIT:(LINE="")!ABORT  DO
  . DO CHKREGRX(LINE,.ABORT) 
  QUIT
  ;
CHKREGRX(LINE,ABORT) ;
  NEW ARR DO PARSELN^TMGRX001(.ARR,LINE)
  NEW IEN22733 SET IEN22733=+$GET(ARR("IEN22733"))
  IF IEN22733'>0 DO  QUIT  
  . WRITE "Unable to find medication for:",!
  . WRITE LINE,!
  . SET %=1 WRITE "Add med to system now" DO YN^DICN WRITE !
  . IF %=-1 SET ABORT=1 QUIT
  . IF %'=1 QUIT
  . NEW GENERIC WRITE "GENERIC NAME: " READ GENERIC:DTIME WRITE ! 
  . IF GENERIC="^" SET ABORT=1 QUIT
  . NEW BRAND WRITE "BRAND NAME: " READ BRAND:DTIME WRITE !
  . NEW IEN SET IEN=0
  . NEW FMIDX FOR FMIDX="B","B2","BRAND" QUIT:(IEN>0)!ABORT  DO
  . . SET IEN=$ORDER(^TMG(22733,FMIDX,GENERIC,0))
  . . IF IEN>0 WRITE "'",GENERIC,"' ALREADY PRESENT, Found in ",FMIDX," index." SET ABORT=1
  . IF ABORT QUIT
  . NEW TMGFDA,TMGIEN,TMGMSG
  . SET TMGFDA(22733,"+1,",.01)=GENERIC
  . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  . IF $DATA(TMGMSG("DIERR")) DO  QUIT:ABORT
  . . WRITE "ERROR: "_$$GETERRST^TMGDEBU2(.TMGMSG) 
  . . SET ABORT=1
  . SET IEN=$GET(TMGIEN(1)) IF IEN'>0 DO  QUIT
  . . WRITE "UNABLE TO FIND IEN OF ADDED FILE",! SET ABORT=1
  . IF BRAND="" QUIT
  . NEW IEN2 SET IEN2=0 FOR FMIDX="B","B2","BRAND" QUIT:(IEN2>0)!ABORT  DO
  . . SET IEN2=$ORDER(^TMG(22733,FMIDX,BRAND,0))
  . . IF IEN2>0 WRITE "'",BRAND,"' ALREADY PRESENT, Found in ",FMIDX," index." SET ABORT=1
  . IF ABORT QUIT
  . KILL TMGFDA,TMGIEN,TMGMSG
  . SET TMGFDA(22733.01,"+1,"_IEN_",",.01)=BRAND
  . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  . IF $DATA(TMGMSG("DIERR")) DO  QUIT:ABORT
  . . WRITE "ERROR: "_$$GETERRST^TMGDEBU2(.TMGMSG) 
  . . SET ABORT=1
  . NEW DIE,DA,DR SET DIE=22733,DA=IEN,DR=".01:9999"
  . DO ^DIE
  ELSE  DO  ;"Drug found, check doses
  . IF $ORDER(ARR("DOSE IENS",""))'="" QUIT
  . NEW DOSE SET DOSE=$GET(ARR("DOSE")) QUIT:DOSE=""
  . NEW IEN50D607 SET IEN50D607=+$GET(ARR("IEN50.607"))
  . SET %=1 WRITE "Add dose '",DOSE,"' to system now" DO YN^DICN WRITE !
  . IF %'=1 QUIT
  . NEW X,Y,DIC SET DIC=50.606,DIC(0)="MAEQ" 
  . DO ^DIC WRITE ! IF Y'>0 QUIT
  . NEW ABORT SET ABORT=0
  . NEW IEN56D606 SET IEN56D606=+Y
  . NEW SUBIEN SET SUBIEN=$ORDER(^TMG(22733,IEN22733,2,"B",+Y,0))
  . IF SUBIEN'>0 DO  QUIT:ABORT
  . . NEW TMGFDA,TMGIEN,TMGMSG
  . . SET TMGFDA(22733.02,"+1,"_IEN22733_",",.01)=IEN56D606
  . . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  . . IF $DATA(TMGMSG("DIERR")) DO  QUIT
  . . . WRITE "ERROR: "_$$GETERRST^TMGDEBU2(.TMGMSG)
  . . . SET ABORT=1
  . . SET SUBIEN=$GET(TMGIEN(1)) IF SUBIEN'>0 DO  QUIT
  . . . WRITE "UNABLE TO FIND IEN OF ADDED FILE",!
  . . . SET ABORT=1
  . KILL TMGFDA,TMGIEN,TMGMSG
  . SET TMGFDA(22733.03,"+1,"_SUBIEN_","_IEN22733_",",.01)=DOSE
  . IF IEN50D607>0 SET TMGFDA(22733.03,"+1,"_SUBIEN_","_IEN22733_",",.02)=IEN50D607
  . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  . IF $DATA(TMGMSG("DIERR")) DO  QUIT
  . . WRITE "ERROR: "_$$GETERRST^TMGDEBU2(.TMGMSG)
  . NEW DIE,DA,DR SET DIE=22733,DA=IEN22733,DR=".01:9999"
  . DO ^DIE
  QUIT
  ;
