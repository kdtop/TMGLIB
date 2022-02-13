TMGLRWU4 ;TMG/kst-Utility for entering data to LAB DATA file ;4/1/18, 2/16/21
              ;;1.0;TMG-LIB;**1**;9/13/13
 ;
 ;"TMG LAB ENTRY UTILITY
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 4/1/18  Kevin S. Toppenberg MD
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
 ;"DELLAB(LRDFN,DT,FLD)  -- DELETE 1 LAB 
 ;"PRIORLDT(TMGHL7MSG,DATESUSED,MSGINFO) -- PRIOR LAB DATES
 ;"ASKDELAB  -- Interact with user and delete stored LAB
 ;"ASKDELRAD(TMGDFN) -- Interact with user and delete stored RAD STUDY
 ;"
 ;"DUPSCAN  -- Scan for duplicate labs
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;
ASKDELAB(TMGDFN) ;
        ;"INPUT: DFN-- optional
        WRITE !,!,"--------------------------------------------------------",!
        WRITE "This utility will cause PERMANENT DELETION of stored lab values. CAUTION!",!
        WRITE "--------------------------------------------------------",!
        NEW X,Y,DIC SET DIC=2,DIC(0)="MAEQ"
        SET TMGDFN=$GET(TMGDFN) 
        IF TMGDFN>0 SET Y=TMGDFN
        ELSE  DO ^DIC WRITE !
ADL1    IF Y'>0 DO  GOTO ADLDN
        . WRITE "No patient selected.  Aborting.",!
        NEW LRDFN SET LRDFN=+$PIECE($GET(^DPT(+Y,"LR")),"^",1)
        IF LRDFN'>0 DO  GOTO ADLDN
        . WRITE "Unable to determine LRDFN in ^DPT("_+Y_",""LR"").  Aborting.",!
        NEW %DT,X,Y SET %DT="AEP"
        SET %DT("A")="Enter date of labs to delete: "
        DO ^%DT WRITE !
        IF Y'>0 DO  GOTO ADLDN
        . WRITE "No date selected.  Aborting.",!
        NEW MENU,MENUCT,USRPICK,ADT,FLD
ADLL0   KILL MENU SET MENUCT=0
        SET MENU(0)="Pick labs to delete"
        NEW SRDT SET SRDT=$$FMDT2RDT^TMGLRWU1(Y+1)  ;"//WAS Y+1
        NEW ERDT SET ERDT=$$FMDT2RDT^TMGLRWU1(Y)  ;"WAS Y
        NEW RDT SET RDT=SRDT
        FOR  SET RDT=+$ORDER(^LR(LRDFN,"CH",RDT)) QUIT:(RDT>ERDT)!(RDT=0)  DO
        . NEW LABFLD SET LABFLD=1
        . FOR  SET LABFLD=+$ORDER(^LR(LRDFN,"CH",RDT,LABFLD)) QUIT:LABFLD'>0  DO
        . . NEW DATANAME SET DATANAME=$PIECE($GET(^DD(63.04,LABFLD,0)),"^",1)
        . . IF DATANAME="" SET DATANAME="(?? LAB NAME ??)"
        . . NEW LINE SET LINE=$GET(^LR(LRDFN,"CH",RDT,LABFLD))
        . . NEW VALUE SET VALUE=$PIECE(LINE,"^",1)
        . . SET MENUCT=MENUCT+1,MENU(MENUCT)=DATANAME_" = "_VALUE_$CHAR(9)_LRDFN_";"_RDT_";"_LABFLD
        IF MENUCT=0 DO  GOTO ADLDN
        . WRITE "No labs found for patient on specified data.  Aborting."
        SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
        IF "^"[USRPICK GOTO ADLDN
        SET ADT=$$RDT2FMDT^TMGLRWU1($PIECE(USRPICK,";",2))
        SET FLD=$PIECE(USRPICK,";",3)
        SET %=1 WRITE !,"ARE YOU SURE YOU WANT TO DELETE THIS LAB?" DO YN^DICN WRITE !
        IF %'=1 GOTO ADLL0
        DO DELLAB(LRDFN,ADT,FLD)
        SET %=1 WRITE !,"DELETE ANOTHER" DO YN^DICN WRITE !
        IF %=1 GOTO ADLL0
ADLDN   QUIT
        ; 
DELLAB(LRDFN,FMDT,FLD)  ;"DELETE 1 LAB
        ;"IMPLEMENT
        NEW TMGFDA,TMGMSG,TMGRESULT SET TMGRESULT="1^OK"
        NEW RDT SET RDT=$$FMDT2RDT^TMGLRWU1(FMDT)
        NEW IENS SET IENS=RDT_","_LRDFN_","
        SET TMGFDA(63.04,IENS,FLD)="@"
        DO FILE^DIE("EK","TMGFDA","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO DLBDN
        . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        ;"Now kill all the field values that are non-standard Fileman 
        KILL ^LR(LRDFN,"CH",RDT,FLD) ;"originally put here by SET in FILE1ARR()
        IF $ORDER(^LR(LRDFN,"CH",RDT,1))>0 GOTO DLBDN  ;"there are still other lab field entries for this date
        SET TMGFDA(63.04,IENS,.01)="@"  ;"DELETE ENTIRE SUBRECORD when only comment remains
        DO FILE^DIE("EK","TMGFDA","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO DLBDN
        . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        GOTO DLBDN        
        ;"IF $DATA(^LR(LRDFN,"CH",RDT,1))=0 GOTO DLBDN ;"No comment present (field 1)
        ;"KILL TMGMSG
        ;"DO WP^DIE(63.04,IENS,.99,"K","@","TMGMSG")
        ;"IF $DATA(TMGMSG("DIERR")) DO  ;"GOTO DLBDN
        ;". SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        ;"IF TMGRESULT'>1 DO  ;"<--- REMOVE LATER AFTER GETTING WP^DIE ABOVE WORKING
        ;". KILL ^LR(LRDFN,"CH",RDT,1)
        ;". SET TMGRESULT="1^OK"
DLBDN   QUIT TMGRESULT
  ;
PRIORLDT(TMGHL7MSG,DATESUSED,MSGINFO) ;"PRIOR LAB DATES
        ;"Purpose: Fill DATESUSED with dates already filed in database on same day
        ;"Input: TMGHL7MSG -- PASS BY REFERNCE.  Array as created by PRSEARRY^TMGHL7X2 
        ;"       DATESUSED --PASS BY REFERENCE.  OUT PARAMETER. Array of other date/times already found.
        ;"          DATESUSED(FMDT)="" <-- was DATESUSED(FMDT_" ")=""
        ;"       MSGINFO.  PASS BY REFERENCE.  Array as created by GETINFO()
        ;"Results: none
        NEW LRDFN SET LRDFN=+$GET(MSGINFO("LRDFN"))
        NEW TEMPARR
        NEW OBRIDX SET OBRIDX=0
        FOR   SET OBRIDX=$ORDER(TMGHL7MSG("B","OBR",OBRIDX)) QUIT:(+OBRIDX'>0)!(+TMGRESULT<0)   DO
        . NEW DT SET DT=$GET(TMGHL7MSG(OBRIDX,7)) QUIT:DT=""
        . SET DT=$$HL72FMDT^TMGHL7U3(DT)
        . SET TEMPARR(DT\1)=""
        NEW DT SET DT=0
        FOR  SET DT=$ORDER(TEMPARR(DT)) QUIT:DT=""  DO
        . NEW RDT SET RDT=$$FMDT2RDT^TMGLRWU1(DT)
        . NEW LABRDT SET LABRDT=RDT
        . FOR  SET LABRDT=$ORDER(^LR(LRDFN,"CH",LABRDT),-1) QUIT:(+LABRDT'>0)!(LABRDT<(RDT-1.5))  DO
        . . NEW FMDT SET FMDT=$$RDT2FMDT^TMGLRWU1(LABRDT)
        . . SET DATESUSED(FMDT)=LABRDT
        . . NEW ALAB SET ALAB=0 FOR  SET ALAB=$ORDER(^LR(LRDFN,"CH",LABRDT,ALAB)) QUIT:+ALAB'>0  DO
        . . . SET DATESUSED(FMDT,ALAB)=LABRDT
        . . . SET DATESUSED("B",ALAB,FMDT)=LABRDT
        QUIT
        ;
ASKDELRAD(TMGDFN) ;"ARRRGGG!!! I wrote this function twice!!  see ASKDELRAD^TMGRAU01
        ;"INPUT: DFN-- optional
        WRITE !,!,"--------------------------------------------------------------------------------",!
        WRITE "This utility will cause PERMANENT DELETION of stored radiology studies. CAUTION!",!
        WRITE "--------------------------------------------------------------------------------",!
        NEW X,Y,DIC SET DIC=2,DIC(0)="MAEQ"
        SET TMGDFN=$GET(TMGDFN) 
        IF TMGDFN>0 SET Y=TMGDFN
        ELSE  DO ^DIC WRITE !
ADR1    IF Y'>0 DO  GOTO ADRDN
        . WRITE "No patient selected.  Aborting.",!
        SET TMGDFN=+Y        
        NEW %DT,X,Y SET %DT="AEP"
        SET %DT("A")="Enter date of radiographic studies to delete: "
        DO ^%DT WRITE !
        IF Y'>0 DO  GOTO ADRDN
        . WRITE "No date selected.  Aborting.",!
        NEW MENU,MENUCT,USRPICK,ADT,FLD
ADRL0   KILL MENU SET MENUCT=0
        SET MENU(0)="Pick radiographic studies to delete"
        NEW SRDT SET SRDT=$$FMDT2RDT^TMGLRWU1(Y+1)  
        NEW ERDT SET ERDT=$$FMDT2RDT^TMGLRWU1(Y) 
        NEW RDT SET RDT=SRDT
        FOR  SET RDT=+$ORDER(^RADPT(TMGDFN,"DT",RDT)) QUIT:((RDT\1)>(ERDT\1))!(RDT=0)  DO
        . NEW SUBIEN SET SUBIEN=0
        . FOR  SET SUBIEN=+$ORDER(^RADPT(TMGDFN,"DT",RDT,"P",SUBIEN)) QUIT:SUBIEN'>0  DO
        . . NEW DT SET DT=$$RDT2FMDT^TMGLRWU1(RDT)
        . . NEW IENS SET IENS=SUBIEN_","_RDT_","_TMGDFN_","
        . . NEW FLD SET FLD=".01;2"
        . . NEW TMGMSG,TMGERR
        . . DO GETS^DIQ(70.03,IENS,FLD,"E","TMGMSG","TMGERR")
        . . IF $DATA(TMGMSG("DIERR")) DO  QUIT
        . . . WRITE "ERROR: "_$$GETERRST^TMGDEBU2(.TMGMSG),!
        . . NEW CASENUM SET CASENUM=$GET(TMGMSG(70.03,IENS,.01,"E"))
        . . NEW STUDYNAME SET STUDYNAME=$GET(TMGMSG(70.03,IENS,2,"E"))
        . . NEW STR SET STR=$$FMTE^XLFDT(DT,"5D")_" "_STUDYNAME_" (case# "_CASENUM_")"
        . . SET MENUCT=MENUCT+1,MENU(MENUCT)=STR_$CHAR(9)_MENUCT_";"_IENS
        IF MENUCT=0 DO  GOTO ADRDN
        . WRITE "No radiographic studies  found for patient on specified data.  Aborting.",!
        SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
        IF "^"[USRPICK GOTO ADRDN
        SET MENUCT=$PIECE(USRPICK,";",1)
        NEW STR SET STR=$PIECE($GET(MENU(MENUCT)),$CHAR(9),1)
        WRITE !,"Permanently delete study (CAN NOT BE UNDONE):",!
        WRITE "    ",STR,!
        WRITE "PERMANENTLY DELETE" SET %=2 DO YN^DICN WRITE !
        IF %=-1 GOTO ADRDN
        IF %'=1 GOTO ADRL1
        NEW IENS SET IENS=$PIECE(USRPICK,";",2)
        NEW TMGFDA SET TMGFDA(70.03,IENS,.01)="@"
        NEW TMGERR
        DO FILE^DIE("E","TMGFDA","TMGERR")
        IF $DATA(TMGMSG("DIERR")) DO
        . WRITE "ERROR: "_$$GETERRST^TMGDEBU2(.TMGMSG),!
        ELSE  WRITE !,"DELETED.",!        
ADRL1   SET %=1 WRITE !,"DELETE ANOTHER" DO YN^DICN WRITE !
        IF %=1 GOTO ADRL0
ADRDN   QUIT
        ;         
 ;"=====================
 ;"The code looks for instances where the same lab is filed multiple times,
 ;"  with a timestamp within the same minute (differing only by seconds).
 ;"  Depending on how variables are set below, it will also delete the bad values
DUPSCAN  ;"Scan for duplicate labs
  NEW ARR
  NEW BUILDUP SET BUILDUP=0
  NEW LRDFN SET LRDFN=3 ;"ignore 1-3
  FOR  SET LRDFN=$ORDER(^LR(LRDFN)) QUIT:LRDFN'>0  DO
  . NEW TMGDFN SET TMGDFN=$ORDER(^DPT("ATMGLR",LRDFN,0))
  . IF 'BUILDUP KILL ARR
  . DO SCAN1(.ARR,TMGDFN)
  . DO FILTR1(.ARR,TMGDFN)
  . IF $DATA(ARR)=0 QUIT
  . WRITE !,$GET(ARR(TMGDFN,"NAME"))
  . IF $GET(TMGKILL)'=1 QUIT   ;"<----- set TMGKILL prior to running to actually delete entries.  
  . DO DEL1(.ARR,TMGDFN,LRDFN)
  WRITE !
  QUIT
  ;
SCAN1(OUT,TMGDFN)  ;
  NEW PTNAME SET PTNAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
  SET OUT(TMGDFN,"NAME")=PTNAME
  SET OUT(TMGDFN,"LRDFN")=LRDFN
  NEW RDT SET RDT=0
  FOR  SET RDT=$ORDER(^LR(LRDFN,"CH",RDT)) QUIT:RDT'>0  DO
  . IF RDT\1=RDT QUIT  ;"ignore if no seconds
  . NEW FMDT SET FMDT=$$RDT2FMDT^TMGLRWU1(RDT)
  . NEW LABFLD SET LABFLD=1  ;"1 IS THE COMMENT FOR LAB, START AFTER THAT...
  . FOR  SET LABFLD=$ORDER(^LR(LRDFN,"CH",RDT,LABFLD)) QUIT:LABFLD'>0  DO
  . . NEW LABNAME SET LABNAME=$PIECE($GET(^DD(63.04,LABFLD,0)),"^",1)
  . . SET OUT(TMGDFN,LABFLD,"NAME")=LABNAME
  . . SET OUT(TMGDFN,LABFLD,FMDT)=RDT  
  QUIT
  ;
FILTR1(ARR,TMGDFN)  ;"FILTER 1 ARRAY, AS MADE BY SCAN1
  NEW LABFLD SET LABFLD=0
  FOR  SET LABFLD=$ORDER(ARR(TMGDFN,LABFLD)) QUIT:LABFLD'>0  DO
  . NEW FIRSTDT SET FIRSTDT=0
  . NEW LASTDT SET LASTDT=0
  . NEW FMDT SET FMDT=0
  . FOR  SET FMDT=$ORDER(ARR(TMGDFN,LABFLD,FMDT)) QUIT:FMDT'>0  DO
  . . IF FIRSTDT=0 SET FIRSTDT=FMDT
  . . IF LASTDT=0 SET LASTDT=FMDT QUIT
  . . IF $EXTRACT(FMDT,1,12)'=$EXTRACT(LASTDT,1,12) DO  QUIT
  . . . KILL ARR(TMGDFN,LABFLD,FMDT)
  . . . SET LASTDT=DT
  . IF $ORDER(ARR(TMGDFN,LABFLD,FIRSTDT))="NAME" DO
  . . KILL ARR(TMGDFN,LABFLD,FIRSTDT)
  . . KILL ARR(TMGDFN,LABFLD,"NAME")
  IF $ORDER(ARR(TMGDFN,""))="LRDFN" KILL ARR(TMGDFN)
  QUIT
  ;
DEL1(ARR,TMGDFN,LRDFN) ;"DELETE DUPLICATE LAB ENTRIES
  NEW LABFLD SET LABFLD=0
  FOR  SET LABFLD=$ORDER(ARR(TMGDFN,LABFLD)) QUIT:LABFLD'>0  DO
  . NEW FMDT SET FMDT=0
  . FOR  SET FMDT=$ORDER(ARR(TMGDFN,LABFLD,FMDT)) QUIT:FMDT'>0  DO
  . . IF $ORDER(ARR(TMGDFN,LABFLD,FMDT))="NAME" QUIT  ;"SAVE only entry just before "NAME"
  . . DO DELLAB(LRDFN,FMDT,LABFLD)  ;"DELETE 1 LAB
  . . KILL ARR(TMGDFN,LABFLD,FMDT)
  QUIT
  ;
