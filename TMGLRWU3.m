TMGLRWU4 ;TMG/kst-Utility for entering data to LAB DATA file ;4/1/18
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
 ;"ASKDELAB  -- Interact with user and delete stored
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
ASKDELAB(DFN) ;
        ;"INPUT: DFN-- optional
        WRITE !,!,"--------------------------------------------------------",!
        WRITE "This utility will cause PERMANENT DELETION of store lab values. CAUTION!",!
        WRITE "--------------------------------------------------------",!
        NEW X,Y,DIC SET DIC=2,DIC(0)="MAEQ"
        SET DFN=$GET(DFN) 
        IF DFN>0 SET Y=DFN
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
        ;"Input: TMGHL7MSG -- PASS BY REFERNCE.  Array as created by PARSMSG2^TMGHL7X2
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
 ;"=====================
 ;"The code looks for instances where the same lab is filed multiple times,
 ;"  with a timestamp within the same minute (differing only by seconds).
 ;"  Depending on how variables are set below, it will also delete the bad values
DUPSCAN  ;"Scan for duplicate labs
  NEW ARR
  NEW BUILDUP SET BUILDUP=0
  NEW LRDFN SET LRDFN=3 ;"ignore 1-3
  FOR  SET LRDFN=$ORDER(^LR(LRDFN)) QUIT:LRDFN'>0  DO
  . NEW DFN SET DFN=$ORDER(^DPT("ATMGLR",LRDFN,0))
  . IF 'BUILDUP KILL ARR
  . DO SCAN1(.ARR,DFN)
  . DO FILTR1(.ARR,DFN)
  . IF $DATA(ARR)=0 QUIT
  . WRITE !,$GET(ARR(DFN,"NAME"))
  . IF $GET(TMGKILL)'=1 QUIT   ;"<----- set TMGKILL prior to running to actually delete entries.  
  . DO DEL1(.ARR,DFN,LRDFN)
  WRITE !
  QUIT
  ;
SCAN1(OUT,DFN)  ;
  NEW PTNAME SET PTNAME=$PIECE($GET(^DPT(DFN,0)),"^",1)
  SET OUT(DFN,"NAME")=PTNAME
  SET OUT(DFN,"LRDFN")=LRDFN
  NEW RDT SET RDT=0
  FOR  SET RDT=$ORDER(^LR(LRDFN,"CH",RDT)) QUIT:RDT'>0  DO
  . IF RDT\1=RDT QUIT  ;"ignore if no seconds
  . NEW FMDT SET FMDT=$$RDT2FMDT^TMGLRWU1(RDT)
  . NEW LABFLD SET LABFLD=1  ;"1 IS THE COMMENT FOR LAB, START AFTER THAT...
  . FOR  SET LABFLD=$ORDER(^LR(LRDFN,"CH",RDT,LABFLD)) QUIT:LABFLD'>0  DO
  . . NEW LABNAME SET LABNAME=$PIECE($GET(^DD(63.04,LABFLD,0)),"^",1)
  . . SET OUT(DFN,LABFLD,"NAME")=LABNAME
  . . SET OUT(DFN,LABFLD,FMDT)=RDT  
  QUIT
  ;
FILTR1(ARR,DFN)  ;"FILTER 1 ARRAY, AS MADE BY SCAN1
  NEW LABFLD SET LABFLD=0
  FOR  SET LABFLD=$ORDER(ARR(DFN,LABFLD)) QUIT:LABFLD'>0  DO
  . NEW FIRSTDT SET FIRSTDT=0
  . NEW LASTDT SET LASTDT=0
  . NEW FMDT SET FMDT=0
  . FOR  SET FMDT=$ORDER(ARR(DFN,LABFLD,FMDT)) QUIT:FMDT'>0  DO
  . . IF FIRSTDT=0 SET FIRSTDT=FMDT
  . . IF LASTDT=0 SET LASTDT=FMDT QUIT
  . . IF $EXTRACT(FMDT,1,12)'=$EXTRACT(LASTDT,1,12) DO  QUIT
  . . . KILL ARR(DFN,LABFLD,FMDT)
  . . . SET LASTDT=DT
  . IF $ORDER(ARR(DFN,LABFLD,FIRSTDT))="NAME" DO
  . . KILL ARR(DFN,LABFLD,FIRSTDT)
  . . KILL ARR(DFN,LABFLD,"NAME")
  IF $ORDER(ARR(DFN,""))="LRDFN" KILL ARR(DFN)
  QUIT
  ;
DEL1(ARR,DFN,LRDFN) ;"DELETE DUPLICATE LAB ENTRIES
  NEW LABFLD SET LABFLD=0
  FOR  SET LABFLD=$ORDER(ARR(DFN,LABFLD)) QUIT:LABFLD'>0  DO
  . NEW FMDT SET FMDT=0
  . FOR  SET FMDT=$ORDER(ARR(DFN,LABFLD,FMDT)) QUIT:FMDT'>0  DO
  . . IF $ORDER(ARR(DFN,LABFLD,FMDT))="NAME" QUIT  ;"SAVE only entry just before "NAME"
  . . DO DELLAB(LRDFN,FMDT,LABFLD)  ;"DELETE 1 LAB
  . . KILL ARR(DFN,LABFLD,FMDT)
  QUIT
  ;
