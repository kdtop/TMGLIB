TMGRX002 ;TMG/kst/Patient medication code; 08/23/17
       ;;1.0;TMG-LIB;**1**;08/23/17
 ;       
 ;"Code for handling parsed medication array
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
 ;"BIGLIST -- Gather a list of all possible med lines, for testing
 ;"SELRX -- Pick meds from list, for testing parsing.  
 ;"T2 -- Test just 1 sample line for testing
 ;"CHKREGRX(LINE,ABORT,TRAIN) -- Interactive test and registration of Rx. 
 ;"REGRX(ARR)  -- REGISTER A MEDICATION
 ;"REGDOSE(ARR) -- REGISTER DOSE
 ;"EDITRX(IEN22733)  -- Edit a med record
 ;"SELRX2 -- CHECK WHICH PARSES ARE GOOD. 
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
  DO SELECTOR^TMGUSRI3(REF,"OUT","Pick meds. <ESC><ESC> to exit.")
  NEW ABORT SET ABORT=0
  NEW LINE SET LINE=""
  FOR  SET LINE=$ORDER(OUT(LINE)) QUIT:(LINE="")!ABORT  DO
  . DO CHKREGRX(LINE,.ABORT,1) 
  QUIT
  ;
T2 ;
  NEW LINE SET LINE="HYDROCODONE/ACET (LORTAB) 5/325 PO BID"
  DO CHKREGRX(LINE,.ABORT,1)
  QUIT
  ;
CHKREGRX(LINE,ABORT,TRAIN) ;
  ;"NOTE: This is an interactive process, expecting user to be at command-line.
CKR1 ;  
  NEW ARR DO PARSELN^TMGRX001(.ARR,LINE,.TRAIN)
  WRITE !,"Prossing medication entry:",!
  WRITE "  ",LINE,!
  NEW IEN22733 SET IEN22733=+$GET(ARR("IEN22733"))
  NEW NEEDRX SET NEEDRX=(IEN22733'>0)
  NEW NEEDDOSE SET NEEDDOSE=($ORDER(ARR("DOSE IENS",""))="")
  IF ('NEEDRX)&('NEEDDOSE) GOTO CKR2    
  NEW USRPICK,MENU,IDX
  IF NEEDRX WRITE !,"Medication doesn't appear to be registered in system",!
  ELSE  IF NEEDRX WRITE !,"Dose doesn't appear to be registered in system",!
CKRM ;          
  SET IDX=0
  KILL MENU SET MENU(IDX)="Select Option:"
  SET MENU(IDX,1)="Line: "_LINE
  IF NEEDRX SET IDX=IDX+1,MENU(IDX)="Register medication now"_$CHAR(9)_"REGRX"
  ELSE  IF NEEDDOSE SET IDX=IDX+1,MENU(IDX)="Register dose now"_$CHAR(9)_"REGDOSE"
  SET IDX=IDX+1,MENU(IDX)="Edit another record to add ALIAS (or misspelling)"_$CHAR(9)_"EDIT"
  SET IDX=IDX+1,MENU(IDX)="Skip this entry"_$CHAR(9)_"SKIP"
  SET IDX=IDX+1,MENU(IDX)="Dump (examine) another record"_$CHAR(9)_"DUMP"
  SET IDX=IDX+1,MENU(IDX)="Reparse"_$CHAR(9)_"REPARSE"
  SET IDX=IDX+1,MENU(IDX)="QUIT"_$CHAR(9)_"DONE"
  SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
  IF USRPICK="^" GOTO CKRDN
  IF USRPICK="REPARSE" GOTO CKR1  
  IF USRPICK="REGRX" DO  GOTO CKR1
  . DO REGRX(.ARR)  ;"REGISTER A MEDICATION
  IF USRPICK="REGDOSE" DO  GOTO CKR1
  . DO REGDOSE(.ARR)  ;"REGISTER DOSE
  IF USRPICK="EDIT" DO  GOTO CKR1
  . NEW DIC,X,Y SET DIC=22733,DIC(0)="MAEQ" 
  . WRITE ! DO ^DIC QUIT:Y'>0
  . DO EDITRX(+Y) 
  IF USRPICK="SKIP" GOTO CKRDN
  IF USRPICK="DUMP" DO  GOTO CKRM
  . NEW OPTION SET OPTION("NO LOOP")=1
  . DO ASKDUMP^TMGDEBU3(22733,,.OPTION)
  IF USRPICK="DONE" DO  GOTO CKR2
  GOTO CKRM
  ;
CKR2 ;  
  WRITE "PROCESSED VERSION:",!,"  ",$$EXTERNAL^TMGRX003(.ARR),!
CKRDN ;
  QUIT
  ;
REGRX(ARR)  ;"REGISTER A MEDICATION
  NEW ABORT SET ABORT=0
  NEW GENERIC WRITE "GENERIC NAME: " READ GENERIC:DTIME WRITE ! 
  IF GENERIC="^" QUIT
  NEW IEN SET IEN=0
  NEW FMIDX FOR FMIDX="B","B2","BRAND" QUIT:(IEN>0)!ABORT  DO
  . SET IEN=$ORDER(^TMG(22733,FMIDX,GENERIC,0))
  . IF IEN>0 WRITE "'",GENERIC,"' ALREADY PRESENT, Found in ",FMIDX," index." SET ABORT=1
  IF ABORT QUIT
  NEW TMGFDA,TMGIEN,TMGMSG
  SET TMGFDA(22733,"+1,",.01)=GENERIC
  DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  QUIT:ABORT
  . WRITE "ERROR: "_$$GETERRST^TMGDEBU2(.TMGMSG) 
  . SET ABORT=1
  SET IEN=$GET(TMGIEN(1)) IF IEN'>0 DO  QUIT
  . WRITE "UNABLE TO FIND IEN OF ADDED FILE",! SET ABORT=1
  NEW BRAND WRITE "BRAND NAME (optional): " READ BRAND:DTIME WRITE !
  IF BRAND'="" DO
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
  IF ABORT QUIT
  WRITE !,"Added.  Now edit record to fill in any missing details.",!
  DO EDITRX(IEN)  
  QUIT
  ;
REGDOSE(ARR)  ;"REGISTER DOSE
  NEW IEN50D607 SET IEN50D607=+$GET(ARR("IEN50.607"))
  NEW DOSE SET DOSE=$GET(ARR("DOSE"))
  IF DOSE="" DO  QUIT:DOSE=""
  . WRITE "DOSE: " READ DOSE:DTIME WRITE !   
  NEW X,Y,DIC SET DIC=50.606,DIC(0)="MAEQ" 
  SET DIC("A")="Select DOSAGE FORM NAME (e.g. 'TAB'): "
  DO ^DIC WRITE ! IF Y'>0 QUIT
  NEW ABORT SET ABORT=0
  NEW IEN56D606 SET IEN56D606=+Y
  NEW SUBIEN SET SUBIEN=$ORDER(^TMG(22733,IEN22733,2,"B",+Y,0))
  IF SUBIEN'>0 DO  QUIT:ABORT
  . NEW TMGFDA,TMGIEN,TMGMSG
  . SET TMGFDA(22733.02,"+1,"_IEN22733_",",.01)=IEN56D606
  . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  . IF $DATA(TMGMSG("DIERR")) DO  QUIT
  . . WRITE "ERROR: "_$$GETERRST^TMGDEBU2(.TMGMSG)
  . . SET ABORT=1
  . SET SUBIEN=$GET(TMGIEN(1)) IF SUBIEN'>0 DO  QUIT
  . . WRITE "UNABLE TO FIND IEN OF ADDED FILE",!
  . . SET ABORT=1
  KILL TMGFDA,TMGIEN,TMGMSG
  SET TMGFDA(22733.03,"+1,"_SUBIEN_","_IEN22733_",",.01)=DOSE
  IF IEN50D607>0 SET TMGFDA(22733.03,"+1,"_SUBIEN_","_IEN22733_",",.02)=IEN50D607
  DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  QUIT
  . WRITE "ERROR: "_$$GETERRST^TMGDEBU2(.TMGMSG)
  WRITE !,"Added.  Now edit record to fill in any missing details.",!
  DO EDITRX(IEN22733) 
  ;
  QUIT
  ;
EDITRX(IEN22733)  ;
  NEW DIE,DA,DR SET DIE=22733,DA=IEN22733,DR=".01:9999"
  DO ^DIE WRITE !
  QUIT
  ;
SELRX2 ; "CHECK WHICH PARSES ARE GOOD. 
  NEW REF SET REF=$NAME(^TMG("TMP","EXAMPLE MEDS"))
  NEW OUT
  DO SELECTOR^TMGUSRI3(REF,"OUT","Pick meds. <ESC><ESC> to exit.")
  NEW ABORT SET ABORT=0
  NEW PICK2
  NEW IDX SET IDX=1
  NEW LINE SET LINE=""
  FOR  SET LINE=$ORDER(OUT(LINE)) QUIT:(LINE="")!ABORT  DO
  . NEW ARR DO PARSELN^TMGRX001(.ARR,LINE)
  . NEW LINE2 SET LINE2=$$EXTERNAL^TMGRX003(.ARR)
  . SET PICK2($$RJ^XLFSTR(IDX,4,"0")_"A "_LINE_" ==> ")=""
  . SET PICK2($$RJ^XLFSTR(IDX,4,"0")_"B "_LINE2)=LINE
  . SET IDX=IDX+1
  NEW OUT2  
  DO SELECTOR^TMGUSRI3("PICK2","OUT2","Pick lines with good parsing. <ESC><ESC> to exit.")
  NEW SOMESHOWN SET SOMESHOWN=0
  SET IDX="" FOR  SET IDX=$ORDER(OUT2(IDX)) QUIT:IDX=""  DO
  . NEW LINE SET LINE=$GET(OUT2(IDX)) QUIT:LINE=""
  . WRITE "delete: ",LINE,!
  . SET SOMESHOWN=1
  IF SOMESHOWN=0 GOTO SRX2DN
  SET %=2
  WRITE "OK to remove above from list of example meds" DO YN^DICN
  IF %=1 DO
  . SET IDX="" FOR  SET IDX=$ORDER(OUT2(IDX)) QUIT:IDX=""  DO
  . . NEW LINE SET LINE=$GET(OUT2(IDX)) QUIT:LINE=""
  . . KILL ^TMG("TMP","EXAMPLE MEDS",LINE)
SRX2DN ;  
  QUIT
  ;

