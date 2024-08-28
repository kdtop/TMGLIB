TMGC0Q04 ;TMG/kst/TMG meanful use util code ;2/2/14, 5/6/18, 3/24/21
         ;;1.0;TMG-LIB;**1**;8/12/12
  ;
  ;"TMG C0Q FUNCTIONS
  ;
  ;"--This code is for parsing a TIU DOCUMENT and extracting the paragraph titles
  ;"  for determing the topics discussed at a given visit.
  ;"--Results are stored in file 22719.
  ;"--There is a function here for parsing all documents at one time.  But the
  ;"  ultimate use is SET up such that a POST-SIGNATURE hook from TIU calls code
  ;"  here after a document is completed.
  ;"--Also, XREF code for TIU DOCUMENT for Consult /Continuity of Care note titles
  ;
  ;"NOTE: Much of code here moved to TMGTIUT6 on 8/8/24
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
  ;"XRCONSLT(X,MODE) -- XRef set/kill function for TIU DOCUMENT field, .01 field
  ;
  ;"=======================================================================
  ;"PRIVATE API FUNCTIONS
  ;"=======================================================================
  ;"CNSLTPT() --get hard-coded pointer for CONSULT / PATIENT CONTINUITY SUMMARY
  ;"SETOLD  
  ;"CHECKDT(TIUIEN)   
  ;"TESTPT  --SHOW INFORMATION FOR A SELECTED PATIENT.  
  ;"SHOWINFO(IEN22719) 
  ;"SHOWSUB(IEN22719,NODE) 
  ;"OUTXFRM(Y) --OUTPUT TRANSFORM FOR .01 FIELD FOR FILE 22719  **BE CAREFUL WITH EDITS!**
  ;"    
  ;
  ;"=======================================================================
  ;"DEPENDENCIES
  ;"=======================================================================
  ;
  ;"=======================================================================
  ;
SUMMALL ;"depreciated.  use code in TMGTIUT6
  DO SUMMALL^TIUT6
  QUIT
  ;
ASKSUMM1 ;"depreciated.  use code in TMGTIUT6
  DO ASKSUMM1^TMGTIUT6
  QUIT   
  ;
SUMM1(TIUIEN,OPTION)  ;"depreciated.  use code in TMGTIUT6
  DO SUMM1^TMGTIUT6(.TIUIEN,.OPTION)  ;
  QUIT
  ;  
TRIGALL ;"depreciated.  use code in TMGTIUT6
  DO TRIGALL^TMGTIUT6
  QUIT                 
  ;
ASKTRIG ;"depreciated.  use code in TMGTIUT6
  DO ASKTRIG^TMGTIUT6
  QUIT
  ;
TEST1  ;"depreciated.  use code in TMGTIUT6
  DO TEST1^TMGTIUT6
  QUIT        
  ;
TRIGJOB(TIUIEN,QUIET,TMGDOCSCANMODS,ONLYOV) ;"depreciated.  use code in TMGTIUT6
  DO TRIGJOB^TMGTIUT6(.TIUIEN,.QUIET,.TMGDOCSCANMODS,.ONLYOV) 
  QUIT
  ;
XRCONSLT(X,DA,MODE)  ;"XREF entry code for file 8925, field .01 XREF "ATMGCNSLT"
  ;"Purpose: XRef set/kill function for TIU DOCUMENT field, .01 field 
  ;"NOTE: This xref will only be SET for values when X --> "CONSULT / PATIENT CONTINUITY SUMMARY"
  ;"      Because this code is site specific, on our system, this title is IEN #1486"
  ;"      And THIS WILL BE HARD CODED HEREIN
  ;"Input: X -- the value of the .01 field of file 8925 (will be pointer to file 8925.1)
  ;"       DA -- the IEN of the document being edited.
  ;"       MODE -- Shoulde be "S" for SET statement, or "K" for a KILL statement
  ;"Output: Will effect creation of XREF like this: ^TIU(8925,"ATMGCNSLT",EpisodeBeginDate,DocIEN)=""
  ;"RESULT: none
  IF $GET(X)'=$$CNSLTPT() QUIT
  IF +$GET(DA)'>0 QUIT
  SET DT=+$PIECE($GET(^TIU(8925,+DA,0)),"^",7) 
  IF DT'>0 QUIT
  SET MODE=$GET(MODE)
  IF MODE="K" KILL ^TIU(8925,"ATMGCNSLT",DT,+DA)
  ELSE  IF MODE="S" SET ^TIU(8925,"ATMGCNSLT",DT,+DA)=""
  QUIT
  ;
CNSLTPT() ;"get hard-coded pointer for CONSULT / PATIENT CONTINUITY SUMMARY
  QUIT 1486
  ;
SETOLD  ;
  ;"Purpose: Set the ATMGCNSLT index 
  NEW TIUIEN SET TIUIEN=0
  FOR  SET TIUIEN=$ORDER(^TIU(8925,"B",$$CNSLTPT(),TIUIEN)) QUIT:TIUIEN'>0  DO
  . WRITE "SETTING TIUIEN "_TIUIEN,!
  . DO XRCONSLT($$CNSLTPT(),TIUIEN,"S")
  ;"Continue into CKDTOLD to now place DueDates into index
CKDTOLD ;"PROCESS DOCS ALREADY SIGNED.
  NEW DOCDT SET DOCDT=0
  FOR  SET DOCDT=$ORDER(^TIU(8925,"ATMGCNSLT",DOCDT)) QUIT:(+DOCDT'>0)  DO
  . NEW TIUIEN SET TIUIEN=0
  . FOR  SET TIUIEN=$ORDER(^TIU(8925,"ATMGCNSLT",DOCDT,TIUIEN)) QUIT:(+TIUIEN'>0)  DO
  . . NEW STATUS SET STATUS=$$GET1^DIQ(8925,TIUIEN,.05)
  . . WRITE TIUIEN," --> ",STATUS,!
  . . IF STATUS'="COMPLETED" QUIT
  . . DO CHECKDT^TMGTIUT6(TIUIEN)
  QUIT
  ;
CHECKDT(TIUIEN)   ;"depreciated.  use code in TMGTIUT6
  DO CHECKDT^TMGTIUT6(TIUIEN)   ;
  QUIT
  ;
TESTPT  ;"SHOW INFORMATION FOR A SELECTED PATIENT.  
  NEW X,Y,DIC,ADFN,IEN22719 SET DIC=2,DIC(0)="MAEQ" 
TP1 ;
  DO ^DIC WRITE ! IF Y'>0 QUIT
  SET ADFN=+Y,IEN22719=0
  FOR  SET IEN22719=$ORDER(^TMG(22719,"DFN",ADFN,IEN22719)) QUIT:+IEN22719'>0  DO
  . DO SHOWINFO(IEN22719)
  NEW % SET %=2 WRITE "Pick another patient" DO YN^DICN
  IF %=1 GOTO TP1 
  QUIT
SHOWINFO(IEN22719) ;
  WRITE "NOTE: ",$$GET1^DIQ(22719,IEN22719,.03),!
  WRITE "  HPI",! DO SHOWSUB(IEN22719,2)
  WRITE "  PMH",! DO SHOWSUB(IEN22719,3)         
  QUIT
SHOWSUB(IEN22719,NODE) ;
  NEW STR SET STR=""
  FOR  SET STR=$ORDER(^TMG(22719,IEN22719,NODE,"B",STR)) QUIT:STR=""  DO
  . WRITE "  --",STR,!
  QUIT
  ;
OUTXFRM(Y) ;"OUTPUT TRANSFORM FOR .01 FIELD FOR FILE 22719  **BE CAREFUL WITH EDITS!**
  ;"INPUT -- Y THE VALUE OF THE .01 FIELD
  N ZZ,REF
  NEW IEN SET IEN=+Y
  D GETS^DIQ(8925,IEN,".01;.02;.07","E","ZZ")
  S REF="ZZ(8925,"""_IEN_","")"
  SET Y=$G(@REF@(.01,"E"))_" - "_$GET(@REF@(.02,"E"))_" - "
  SET Y=Y_$P($G(@REF@(.07,"E")),"@",1)_" (`"_IEN_")"
  QUIT Y
  ;
CHK6CIT(TIUIEN)  ;"depreciated.  use code in TMGTIUT6
  DO CHK6CIT^TMGTIUT6(TIUIEN)  
  QUIT
  ;"
ADD6CIT  ;"depreciated.  use code in TMGTIUT6  
  DO ADD6CIT^TMGTIUT6  
  QUIT
  ;"
ADDLSIGN(TIUIEN)  ;"depreciated.  use code in TMGTIUT6
  DO ADDLSIGN^TMGTIUT6(TIUIEN)  ;"Add an addl signer when needed  ;"11/23/21
  ;