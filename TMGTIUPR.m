TMGTIUPR ;TMG/kst-Text objects for use in CPRS ; 7/26/2018
         ;;1.0;TMG-LIB;**1,17**;7/26/18
 ;
 ;"Kevin Toppenberg MD
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 7/26/2018  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"
 ;"=======================================================================
 ;"PUBLIC FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"PRIVATE FUNCTIONS
 ;"=======================================================================
MULTIPRT(TMGRESULT,LSTTOPRINT)  ;"
   ;"PURPOSE: This function is designed to compile all text from provided
   ;"         notes into one array, that the client can then pass to
   ;"         a TWebBrowser to print as one document with page breaks.
   SET TMGRESULT(0)="1^SUCCESSFUL"
   NEW TIUIEN SET TIUIEN=999999
   NEW TMGTEST SET TMGTEST=0
   SET BREAK="<p style=""page-break-before: always"">"
   IF TMGTEST=0 DO
   . MERGE ^TMP("TMG","MULTIPRT","LIST")=LSTTOPRINT
   ELSE  DO
   . MERGE LSTTOPRINT=^TMP("TMG","MULTIPRT","LIST")
   FOR  SET TIUIEN=$O(LSTTOPRINT(TIUIEN),-1) QUIT:TIUIEN'>0  DO
   . ;"SET TMGRESULT(TIUIEN)="GOT "_$G(LSTTOPRINT(TIUIEN))
   . NEW REF,NOTEARR
   . IF $$EXCLUDE(TIUIEN)=1 QUIT
   . DO TGET^TIUSRVR1(.REF,TIUIEN)
   . ;"DO PRINTW^ORWTIU(.REF,TIUIEN,2)
   . MERGE NOTEARR=@REF   
   . DO ADD(.TMGRESULT,.NOTEARR,BREAK)
   QUIT
   ;"
ADD(RESULT,NOTEARR,BREAK)  ;"
   ;"Purpose: This function adds contents of REF array to result, 
   ;"         after inserting "BREAK", which is the printing line
   ;"         break
   NEW OUTIDX SET OUTIDX=$O(RESULT(999999),-1)+1
   NEW IDX SET IDX=0
   IF OUTIDX>1 DO
   . SET RESULT(OUTIDX)=BREAK,OUTIDX=OUTIDX+1
   ;"NEW NOTEARR SET NOTEARR=@REF
   FOR  SET IDX=$O(NOTEARR(IDX)) QUIT:IDX'>0  DO
   . SET RESULT(OUTIDX)=$G(NOTEARR(IDX))
   . SET OUTIDX=OUTIDX+1
   QUIT
   ;"
EXCLUDE(TIUIEN)  ;"
   NEW TMGRESULT SET TMGRESULT=0
   ;"CHECK NOTE STATUS. ONLY INCLUDE SIGNED NOTES
   NEW STATUS SET STATUS=$P($G(^TIU(8925,TIUIEN,0)),"^",5)
   IF STATUS'=7 DO  GOTO EXDN
   . SET TMGRESULT=1
   ;"CHECK NOTE TO SEE IF USER CAN PRINT IT.
   NEW CANPRINT
   ;"SET CANPRINT=$S(TIULVL="DOC":$$CANDO^TIULP(TIUDA,"PRINT RECORD"),1:1)
   SET CANPRINT=$P($$CANDO^TIULP(TIUIEN,"PRINT RECORD"),"^",1)
   IF CANPRINT=0 DO  GOTO EXDN
   . SET TMGRESULT=1
EXDN
   QUIT TMGRESULT
   ;"
