TMGRPC1H ;TMG/kst-RPC Functions ;5/1/13, 2/2/14
         ;;1.0;TMG-LIB;**1**;5/1/13
 ;
 ;"TMG RPC FUNCTIONS especially related to CPRS
 ;"  Process Note (note sent from CPRS for extra processing, then returned)
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
 ;" RPC -- Public Functions.
 ;"=======================================================================
 ;"PROCESS(TMGRESULT,TMGIN) -- Entrypoint for RPC to effect processing a note from CPRS
 ;"
 ;"=======================================================================
 ;"Dependancies:
 ;" TMGHTM1, TMGTIUOJ, TMGTIUO6, XLFSTR, TMGSTUT2, TMGSTUT3
 ;"=======================================================================
 ;
PROCESS(TMGRESULT,TMGIN,FORCE) ;"Entry point for RPC: TMG CPRS PROCESS NOTE
  ;"Input:  TMGRESULT -- output value
  ;"        TMGIN -- Input from client.  Format:
  ;"          TMGIN("DFN")=<DFN>  (IEN in PATIENT file)
  ;"          TMGIN("NoteIEN")=<TIUIEN>
  ;"          TMGIN("TEXT",1) = 1st line of text
  ;"          TMGIN("TEXT",2) = 2nd line of text, etc
  ;"        FORCE -- Optional. If 1,  note is processed even if tag is absent. Default is 0.
  ;"            UPDATE NOTE: we are forcing this to 1 below, regardless of RPC input
  NEW TMGZZDEBUG SET TMGZZDEBUG=0
  IF TMGZZDEBUG DO
  . KILL TMGIN,FORCE
  . MERGE TMGIN=^TMG("TMP","PROCESS^TMGRPC1H","TMGIN")
  . MERGE FORCE=^TMG("TMP","PROCESS^TMGRPC1H","FORCE")
  ELSE  DO
  . KILL ^TMG("TMP","PROCESS^TMGRPC1H")
  . MERGE ^TMG("TMP","PROCESS^TMGRPC1H","TMGIN")=TMGIN
  . MERGE ^TMG("TMP","PROCESS^TMGRPC1H","FORCE")=FORCE
  NEW OPTION 
  SET OPTION("ALL NOTES")=1
  SET FORCE=1 ;"ELH  Always process note  //kt moved from PROCESS^TMGTIUP3 to here 5/20/18  
  SET OPTION("FORCE PROCESS")=$GET(FORCE)  
  NEW USENEWMETHOD SET USENEWMETHOD=1
  IF USENEWMETHOD DO
  . DO SAVAMED^TMGTIUP3(.TMGIN)  ;"Move MEDICATION table to FINAL MEDICATION table
  . SET OPTION("FORCE REFRESH TABLE","FINAL MEDICATIONS")=1
  ELSE  DO
  . DO MOVEMEDS^TMGTIUP3(.TMGIN)  ;"Move MEDICATION table to FINAL MEDICATION table
  SET OPTION("DIRECT HTML INSERTION")=1  ;"//kt 5/23/18
  DO PROCESS^TMGTIUP3(.TMGRESULT,.TMGIN,.OPTION) ;
  QUIT
  ;
MOVEMEDS(TMGIN)  ;"Used as part of the RPC: TMG CPRS PROCESS NOTE  -- depreciated (see above)
  ;"Purpose: This function replaces the FINAL MEDICATION table with the
  ;"         MEDICATION table found within the note.
  ;"Input:   TMGIN -- Input from client.  Format:
  ;"              TMGIN("DFN")=<DFN>  (IEN in PATIENT file)
  ;"              TMGIN("NoteIEN")=<TIUIEN>
  ;"              TMGIN("TEXT",1) = 1st line of text
  ;"              TMGIN("TEXT",2) = 2nd line of text, etc
  ;"Output: TMGIN will contain the text as sent, with changes
  ;"Result: none
  NEW IDX,TEMPNOTE,TEMPIDX,MEDARR,MEDIDX,FOUNDMED,ENDFOUND,PARTB
  SET (IDX,TEMPIDX,MEDIDX,FOUNDMED)=0
  NEW FOUNDFINAL SET FOUNDFINAL=0
  FOR  SET IDX=$ORDER(TMGIN("TEXT",IDX)) QUIT:IDX'>0  DO
  . NEW LINE SET LINE=$G(TMGIN("TEXT",IDX))
  . IF FOUNDMED=1 DO
  . . IF LINE["<P>" SET FOUNDMED=0
  . . ELSE  DO
  . . . SET MEDIDX=MEDIDX+1
  . . . SET MEDARR(MEDIDX)=LINE
  . IF LINE["[MEDICATION" SET FOUNDMED=1
  . ;"
  . ELSE  DO
  . . IF LINE["<P>" SET FOUNDFINAL=0
  . IF FOUNDFINAL'=1 DO 
  . . SET TEMPIDX=TEMPIDX+1
  . . SET TEMPNOTE(TEMPIDX)=LINE
  . IF LINE["[FINAL MED" DO
  . . SET FOUNDFINAL=1
  . . SET MEDIDX=0
  . . FOR  SET MEDIDX=$ORDER(MEDARR(MEDIDX)) QUIT:MEDIDX'>0  DO
  . . . SET TEMPIDX=TEMPIDX+1
  . . . SET TEMPNOTE(TEMPIDX)=$G(MEDARR(MEDIDX))
  KILL TMGIN("TEXT")
  MERGE TMGIN("TEXT")=TEMPNOTE
  NEW DFN SET DFN=$G(TMGIN("DFN"))
  ;"NOW THAT MEDS ARE MOVED... STORE THE MEDARR INTO GLOBAL, SO THAT
  ;"      REFRESH TABLES WILL USE IT.
  NEW STORENAME SET STORENAME="MEDICATIONS TABLE-"_DFN
  NEW REF SET REF=$$GETMPREF^TMGMISC2(STORENAME)
  NEW PRIORTIME SET PRIORTIME=+$GET(@REF@("D","HTIME"))
  NEW DELTASEC SET DELTASEC=$$HDIFF^XLFDT($H,PRIORTIME,2)  ;"returns seconds
  NEW ARR,H SET H=$H MERGE ARR("HTIME")=H
  ;"ARRANGE MEDARR INTO PROPER FORMAT
  NEW TMPMEDARR,IDX,TMPIDX
  SET IDX=0,TMPIDX=1       
  FOR  SET IDX=$O(MEDARR(IDX)) QUIT:IDX'>0  DO
  . IF $E($G(MEDARR(IDX)))="*" DO
  . . NEW P1,P2,VALUE
  . . SET VALUE=$G(MEDARR(IDX))
  . . SET P1=$P(VALUE," = ",1),P2=$P(VALUE," = ",2)
  . . SET TMPMEDARR("KEY-VALUE",P1)=P2
  . . SET TMPMEDARR("KEY-VALUE",P1,"LINE")=VALUE
  . ELSE  DO
  . . SET TMPMEDARR(TMPIDX)=$G(MEDARR(IDX))
  . . SET TMPIDX=TMPIDX+1
  SET TMPMEDARR("KEY-VALUE","SOURCE-DATE")=$$NOW^XLFDT()
  MERGE ARR("TABLE")=TMPMEDARR
  DO TMPSAVE^TMGMISC2(.ARR,STORENAME,"5M")
  QUIT
  ;
