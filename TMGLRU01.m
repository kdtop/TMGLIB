TMGLRU01 ;TMG/kst-Entry point LAB REPORTS ;12/16/15, 4/1/18
              ;;1.0;TMG-LIB;**1**;1/21/15
 ;
 ;"TMG LAB REPORTS
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
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"LABDATES(ORY,DFN,REPORTID,HSTYPE,DAYSBACK,SECTION,SDT,EDT) -- CPRS Lab report (Lab Dates)
 ;"FIXDISP  --ROLL AND SCROLL UTILITY TO ADJUST RELATIVE DISPLAY ORDERS OF LABS 
 ;"=======================================================================
 ;
ADDLINE(REF,TEXT) ;
  NEW IDX SET IDX=+$ORDER(@REF@(""),-1)+1
  SET @REF@(IDX)=TEXT
  QUIT
  ;
LABDATES(ORY,DFN,REPORTID,HSTYPE,DAYSBACK,SECTION,SDT,EDT)   ;"  
  ;"Purpose: Code for report: CPRS Lab report (Lab Dates)
  ;"Input: ORY -- PASS BY REFERENCE.  AN OUT PARAMETER.  Gets REFERENCE of output array
  ;"       DFN -- Patient IEN
  ;"       REPORTID -- (not used)
  ;"       HSTYPE -- (not used)
  ;"       DAYSBACK -- (not used)
  ;"       SECTION -- (not used)
  ;"       SDT -- Start date FMDT format.  Optional.  Default is 0 (earliest)
  ;"       EDT -- End date FMDT format.  Optional.  Default is 9999999 (last possible)
  NEW REF SET REF=$NAME(^TMG("TMGLRU01",$J))
  SET DFN=+$GET(DFN)
  NEW LRDFN SET LRDFN=+$GET(^DPT(DFN,"LR"))
  SET SDT=+$GET(SDT)
  SET EDT=+$GET(EDT) IF EDT'>0 SET EDT=9999999
  NEW FOUND,RDT SET RDT=0
  FOR  SET RDT=$ORDER(^LR(LRDFN,"CH",RDT)) QUIT:(RDT'>0)  DO
  . NEW LABIDX SET LABIDX=0
  . FOR  SET LABIDX=$ORDER(^LR(LRDFN,"CH",RDT,LABIDX)) QUIT:(+LABIDX'>0)  DO
  . . SET FOUND(RDT\1)=$GET(FOUND(RDT\1))+1
  DO ADDLINE(REF,"Dates Overview for Labs")
  DO ADDLINE(REF,"-----------------------")
  SET RDT=0
  NEW TEXT
  FOR  SET RDT=$ORDER(FOUND(RDT)) QUIT:+RDT'>0  DO
  . NEW DT SET DT=$$RDT2FMDT^TMGLRWU1(RDT)
  . IF (DT<SDT)!(DT>EDT) QUIT
  . SET TEXT="  "_$$RJ^XLFSTR(+$GET(FOUND(RDT)),3," ")_" lab tests on "_$$FMTE^XLFDT(DT,"1D")
  . DO ADDLINE(REF,TEXT)
  SET ORY=REF
  QUIT 
  ;"
FIXDISP  ;"ROLL AND SCROLL UTILITY TO ADJUST RELATIVE DISPLAY ORDERS OF LABS
  WRITE "Utility to alter display order of labs.",!
  WRITE "First we must locate sample to labs to reference.",!
  NEW X,Y,DFN,DIC
PKPT ;  
  SET DIC=2,DIC(0)="MAEQ",DIC("A")="Select patient with sample labs: "
  DO ^DIC WRITE ! QUIT:+Y'>0
  SET DFN=+Y
  NEW LRDFN SET LRDFN=+$GET(^DPT(DFN,"LR"))
  IF LRDFN'>0 DO  GOTO FDSPDN
  . WRITE "That patient does not have any labs!  Quitting",!
PKLB ;  
  SET DIC="^LR("_LRDFN_",""CH""," SET DIC("A")="Enter exact DATE/TIME of lab: "
  DO ^DIC WRITE !
  IF Y'>-1 GOTO PKPT
  NEW RDT SET RDT=+Y
  NEW DT SET DT=$$RDT2FMDT^TMGLRWU1(RDT)
  NEW PANEL,MENU,SEQ,CT,INPUT
  NEW DA,DR,DIE SET DIE=60,DR=56,DIE("NO^")="OUTOK"
MNU1
  DO GETARR(.PANEL,DFN,DT)
  SET MENU(0)="Pick item to alter display order"
  SET CT=1,SEQ=""
  FOR  SET SEQ=$ORDER(PANEL("SEQ",SEQ)) QUIT:SEQ=""  DO
  . NEW IDX SET IDX=0
  . FOR  SET IDX=$ORDER(PANEL("SEQ",SEQ,IDX)) QUIT:IDX'>0  DO
  . . NEW ITEM SET ITEM=$GET(PANEL(IDX)) QUIT:ITEM=""
  . . NEW ORDER SET ORDER=$PIECE(ITEM,"^",3)
  . . IF ORDER="" SET ORDER="?"
  . . SET MENU(CT)=" Seq# "_ORDER_" -- "_$PIECE(ITEM,"^",2)_$CHAR(9)_+ITEM
  . . SET CT=CT+1  
  SET INPUT=$$MENU^TMGUSRI2(.MENU,"^") 
  IF INPUT="^" GOTO PKLB
  WRITE INPUT
  IF $DATA(PANEL("B",+INPUT))=0 WRITE !,"NO DATA!",! GOTO MNU1
  SET DA=+INPUT
  WRITE "Edit 'PRINT ORDER' to alter display sequence...",!
  DO ^DIE WRITE !
  GOTO MNU1
FDSPDN ;  
  QUIT
  ;
GETARR(OUT,DFN,DT)  ;"USED BY FIXDISP ABOVE
  KILL OUT NEW TEMP
  DO INTERIMG^ORWLRR(.TEMP,DFN,DT-0.000001,-1,1)
  MERGE TEMP=@TEMP KILL TEMP(1)
  NEW CT SET CT=1
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(TEMP(IDX)) QUIT:IDX'>0  DO
  . NEW LINE SET LINE=$GET(TEMP(IDX))
  . IF +LINE'>0 QUIT
  . NEW IEN60,NODE,SEQ SET IEN60=+LINE,NODE=^LAB(60,IEN60,.1),SEQ=$PIECE(NODE,"^",6)
  . SET OUT(CT)=$PIECE(LINE,"^",1,2)_"^"_SEQ
  . SET OUT("B",IEN60)=""
  . SET OUT("SEQ",+SEQ,CT)=""
  . SET CT=CT+1
  QUIT
  ;
WASSEEN(OUT,DFN,DATE,DUZ)  ;"HAS LAB BEEN SEEN BY USER, IF NOT LOG
  ;"OUT = "-1" for unseen OR "1" for seen
  ;"LRDFN should not be needed but if so it can be added later with this
  ;"     call
  ;"NEW LRDFN
  ;"D DEMO^LR7OGU(DFN,.LRDFN)
  NEW TMGDEBUG SET TMGDEBUG=0
  IF TMGDEBUG=1 DO
  . SET DFN=$GET(^TMP("TMGLRU01","DFN"))
  . SET DATE=$GET(^TMP("TMGLRU01","DATE"))
  . SET DUZ=$GET(^TMP("TMGLRU01","DUZ"))
  ELSE  DO
  . SET ^TMP("TMGLRU01","DFN")=DFN
  . SET ^TMP("TMGLRU01","DATE")=DATE
  . SET ^TMP("TMGLRU01","DUZ")=DUZ
  NEW PTIEN,DTIEN,DUZIEN
  SET OUT="-1"
  SET PTIEN=+$ORDER(^TMG(22732,"B",DFN,0))
  IF PTIEN'>0 GOTO WSERR
  SET DTIEN=+$ORDER(^TMG(22732,PTIEN,1,"B",DATE,0))
  IF DTIEN'>0 GOTO WSERR
  SET DUZIEN=+$ORDER(^TMG(22732,PTIEN,1,DTIEN,1,"B",DUZ,0))
  IF DUZIEN'>0 GOTO WSERR
  NEW Y SET Y=$P($G(^TMG(22732,PTIEN,1,DTIEN,1,DUZIEN,0)),"^",2)
  X ^DD("DD")
  SET OUT="1^First Reviewed By You: "_Y GOTO WSDN
WSERR
  SET OUT="-1^"_$$SETSEEN(DFN,DATE,DUZ)
WSDN
  QUIT
  ;"
SETSEEN(DFN,DATE,DUZ)   ;"SET LAB SET AS SEEN BY THE USER
  NEW TMGFDA,TMGIEN,TMGMSG,PTIEN,TMGRESULT
  SET TMGRESULT="ENTRY CREATED"
  SET TMGIEN=+$ORDER(^TMG(22732,"B",DFN,0))
  IF TMGIEN'>0 DO
  . SET TMGFDA(22732,"+1,",.01)=DFN 
  . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  . IF $DATA(TMGMSG("DIERR")) DO 
  . . SET TMGRESULT=$$GETERRST^TMGDEBU2(.TMGMSG)
  . SET PTIEN=TMGIEN(1)
  ELSE  DO
  . SET PTIEN=TMGIEN
  IF PTIEN'>0 GOTO SSDN
  ;"TEST DATE
  NEW DTIEN
  SET DTIEN=+$ORDER(^TMG(22732,PTIEN,1,"B",DATE,0))
  IF DTIEN'>0 DO
  . KILL TMGFDA,TMGIEN
  . SET TMGFDA(22732.01,"+1,"_PTIEN_",",.01)=DATE
  . DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
  . IF $DATA(TMGMSG("DIERR")) DO
  . . SET TMGRESULT=$$GETERRST^TMGDEBU2(.TMGMSG)
  . SET DTIEN=TMGIEN(1)
  IF DTIEN'>0 GOTO SSDN
  ;"ADD DFN
  NEW DUZIEN    
  SET DUZIEN=+$ORDER(^TMG(22732,PTIEN,1,DTIEN,1,"B",DUZ,0))
  KILL TMGFDA,TMGMSG,TMGIEN
  IF DUZIEN'>0 DO
  . SET TMGFDA(22732.11,"+1,"_DTIEN_","_PTIEN_",",.01)=DUZ
  . SET TMGFDA(22732.11,"+1,"_DTIEN_","_PTIEN_",",1)=$$NOW^XLFDT
  ELSE  DO
  . ;THERE SHOULDN'T BE A CASE WHERE THE DFN ISN'T FOUND, SO NOTHING HERE
  DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO
  . SET TMGRESULT=$$GETERRST^TMGDEBU2(.TMGMSG)
SSDN
  QUIT TMGRESULT
  ;"