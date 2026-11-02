TMGRX011 ;TMG/kst/Patient medication list history code; 11/1/26
       ;;1.0;TMG-LIB;**1**;11/1/26
 ;
 ;"Code for dealing with saving patients medication list into TMG PATIENT MED LIST HISTORY
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 11/1/26  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"
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
TESTRXHX ;"//kt //codex added entire function 11/1/26
  ;"Select a patient and medication-history command, then display the result.
  ;"Input: none.
  ;"Result: none.
  NEW DIC,X,Y SET DIC=2,DIC(0)="MAEQ"
  DO ^DIC WRITE !
  IF +Y'>0 QUIT
  NEW TMGDFN SET TMGDFN=+Y
  NEW MENU SET MENU(0)="Select a medication history command"
  SET MENU(1)="List all medication tables"_$CHAR(9)_"LIST ALL"
  SET MENU(2)="List medication summary"_$CHAR(9)_"LIST SUMMARY"
  SET MENU(3)="List parsed medication summary"_$CHAR(9)_"LIST SUMMARY:PARSED"  ;"//kt //codex 11/1/26
  SET MENU(4)="List notes for a medication"_$CHAR(9)_"LIST TIU"  ;"//kt //codex 11/1/26
  NEW CMD SET CMD=$$MENU^TMGUSRI2(.MENU,"^")
  IF CMD="^" QUIT
  ;"//kt //codex begin mod 2026-11-01 --
  IF CMD="LIST TIU" DO
  . NEW DIC,X,Y SET DIC=22733,DIC(0)="MAEQ"
  . DO ^DIC WRITE !
  . IF +Y'>0 SET CMD="" QUIT
  . SET CMD=CMD_":"_+Y
  IF CMD="" QUIT
  ;"//kt //codex end mod 2026-11-01 --
  NEW TMGRESULT DO RXHXCMD(.TMGRESULT,TMGDFN,CMD,,,)
  IF $DATA(TMGRESULT)=0 WRITE !,"No results.",! QUIT
  ZWRITE TMGRESULT
  QUIT
  ;
RXHXCMD(TMGRESULT,TMGDFN,PARAMS,SDT,EDT,DATA) ;"Command entry point for patient medication history. //kt //codex added entire function 11/1/26
  ;"Input:  TMGRESULT -- pass by reference; status in (0), data lines in (1) onward.
  ;"        TMGDFN -- patient IEN
  ;"        PARAMS -- CMD:<parameters>.  CMD is the command name, optionally followed by :<parameters>.
  ;"          CMD="LIST ALL":
  ;"            PARAMS: "LIST ALL"; no additional parameters.
  ;"            TMGRESULT(#)="0^<TIU document IEN>^<document FMDT>" -- table metadata.
  ;"            TMGRESULT(#)="1^<medication line>" -- entry from incoming table.
  ;"            TMGRESULT(#)="2^<medication line>" -- entry from final table.
  ;"            Medication line: original text^parsed text^medication IEN^name^strength^units.
  ;"            Each 0 line applies to subsequent medication lines until the next 0 line.
  ;"
  ;"          CMD="LIST SUMMARY":
  ;"            PARAMS: "LIST SUMMARY" or "LIST SUMMARY:PARSED".
  ;"            PARSED groups by stored parsed text, falling back to original if blank.
  ;"            Without PARSED, original text is used. Surrounding spaces are trimmed.
  ;"            TMGRESULT(#)="0^<medication name>" -- heading; blank for unmatched lines.
  ;"            TMGRESULT(#)="1^<medication text>^<first FMDT>^<last FMDT>".
  ;"            TMGRESULT(#)="2^<medication IEN>^<name>^<strength>^<units>".
  ;"            Type 2 lines apply to the preceding type 1 line and may be repeated.
  ;"            Incoming and final observations are combined; dates are first and last
  ;"            observations of identical text, not proof of continuous use.
  ;"
  ;"          CMD="LIST TIU":
  ;"            PARAMS: "LIST TIU:<medication IEN>"; IEN is from file 22733.
  ;"            TMGRESULT(#)="<TIU document IEN>^<document FMDT>".
  ;"            Each matching note is returned once, even if multiple table lines match.
  ;"        SDT, EDT -- optional FileMan inclusive start and end dates.
  ;"            SDT defaults to 0; EDT defaults to 9999999. A date-only EDT includes the full day.
  ;"        DATA -- optional command-specific input array.
  ;"Result: None.  Overall result returned in TMGRESULT(0)
  ;"  TMGRESULT(0)="1^OK" on success, including when there are no matching Rx data.
  ;"  TMGRESULT(0)="-1^<error message>" on error.
  ;"
  KILL TMGRESULT
  IF +$GET(TMGDFN)'>0 SET TMGRESULT(0)="-1^Numeric patient ID (DFN) not provided" QUIT  ;"//kt //codex 11/1/26
  NEW CMD SET CMD=$PIECE($GET(PARAMS),":",1)
  IF CMD="LIST ALL" DO  GOTO RHCDN
  . DO LISTHIST(.TMGRESULT,+TMGDFN,.SDT,.EDT)
  IF CMD="LIST SUMMARY" DO  GOTO RHCDN  ;"//kt //codex 11/1/26
  . ;"//kt //codex begin mod 2026-11-01 --
  . NEW OPTION SET OPTION("PARAMS")=$PIECE($GET(PARAMS),":",2,999)
  . DO LISTSUM(.TMGRESULT,+TMGDFN,.SDT,.EDT,.OPTION)
  . ;"//kt //codex end mod 2026-11-01 --
  ;"//kt //codex begin mod 2026-11-01 --
  IF CMD="LIST TIU" DO  GOTO RHCDN
  . NEW MEDIEN SET MEDIEN=$PIECE($GET(PARAMS),":",2)
  . DO LISTTIU(.TMGRESULT,+TMGDFN,MEDIEN,$GET(SDT),$GET(EDT))
  ;"//kt //codex end mod 2026-11-01 --
  SET TMGRESULT(1)="-1^Medication history command not implemented: "_CMD
RHCDN ;
  ;"//kt //codex begin mod 2026-11-01 --
  IF $PIECE($GET(TMGRESULT(1)),"^",1)="-1" DO
  . SET TMGRESULT(0)=TMGRESULT(1) KILL TMGRESULT(1)
  ELSE  SET TMGRESULT(0)="1^OK"
  ;"//kt //codex end mod 2026-11-01 --
  QUIT
  ;
LISTTIU(OUT,TMGDFN,MEDIEN,SDT,EDT) ;"//kt //codex added entire function 11/1/26
  ;"Input: OUT -- PASS BY REFERENCE. An OUT parameter. Format:
  ;"         OUT(#)=<TIU document IEN>^<document FMDT>, one line per note.
  ;"       TMGDFN -- patient IEN; MEDIEN -- medication IEN in file 22733.
  ;"       SDT,EDT -- OPTIONAL inclusive FMDT range; date-only EDT includes the full day.
  ;"Result: none. OUT(1)=-1^error if the request or history cannot be read.
  ;"Notes are listed once even when both tables or multiple lines match.
  KILL OUT
  IF ($GET(MEDIEN)'?1.N)!(+$GET(MEDIEN)'>0) SET OUT(1)="-1^A medication IEN is required" QUIT
  NEW LIST DO LISTHIST(.LIST,TMGDFN,$GET(SDT),$GET(EDT))
  IF $PIECE($GET(LIST(1)),"^",1)="-1" SET OUT(1)=LIST(1) QUIT
  NEW IDX,LINE,TYPE,TIUIEN,NOTEDT,SEEN,COUNT SET (IDX,TIUIEN,NOTEDT,COUNT)=0
  FOR  SET IDX=$ORDER(LIST(IDX)) QUIT:IDX'>0  DO
  . SET LINE=$GET(LIST(IDX)),TYPE=$PIECE(LINE,"^",1)
  . IF TYPE=0 SET TIUIEN=+$PIECE(LINE,"^",2),NOTEDT=+$PIECE(LINE,"^",3) QUIT
  . IF (TYPE'=1)&(TYPE'=2) QUIT
  . IF TIUIEN'>0 QUIT
  . IF +$PIECE(LINE,"^",4)'=+MEDIEN QUIT
  . IF $DATA(SEEN(TIUIEN)) QUIT
  . SET SEEN(TIUIEN)=1,COUNT=COUNT+1,OUT(COUNT)=TIUIEN_"^"_NOTEDT
  QUIT
  ;
LISTSUM(OUT,TMGDFN,SDT,EDT,OPTION) ;"//kt //codex added entire function 11/1/26
  ;"Input: OUT -- PASS BY REFERENCE. An OUT parameter. Format:
  ;"         OUT(#)="0^<medication name>" -- heading for following entries.
  ;"         OUT(#)="1^<medication text>^<first FMDT>^<last FMDT>"
  ;"         OUT(#)="2^<medication IEN>^<medication name>^<strength>^<units>"
  ;"         Type 2 lines apply to the preceding type 1 entry; multiple
  ;"         lines are returned if the entry has differing medication details.
  ;"         An empty medication name indicates an unmatched medication.
  ;"       TMGDFN -- PATIENT record number.
  ;"       SDT,EDT -- OPTIONAL.  inclusive FMDT range; date-only EDT includes the full day.
  ;"       OPTION("PARAMS") -- OPTIONAL. If it contains PARSED, use parsed text;
  ;"         otherwise use original text. If parsed text is blank, use original text.
  ;"Result: none. OUT(1)=-1^error if the history cannot be read.
  ;"Incoming and final observations are combined. Identical text is grouped
  ;"even if other text appeared between observations; dates are not intervals.
  KILL OUT
  NEW LIST DO LISTHIST(.LIST,TMGDFN,$GET(SDT),$GET(EDT))
  IF $PIECE($GET(LIST(1)),"^",1)="-1" SET OUT(1)=LIST(1) QUIT
  NEW GROUPS,IDX,LINE,TYPE,NOTEDT,ORIG,MEDNAME,GRP,PREV,MEDINFO  ;"//kt //codex 11/1/26
  NEW USEPARSE SET USEPARSE=($$UP^XLFSTR($GET(OPTION("PARAMS")))["PARSED")  ;"//kt //codex 11/1/26
  SET (IDX,NOTEDT)=0
  FOR  SET IDX=$ORDER(LIST(IDX)) QUIT:IDX'>0  DO
  . SET LINE=$GET(LIST(IDX)),TYPE=$PIECE(LINE,"^",1)
  . IF TYPE=0 SET NOTEDT=+$PIECE(LINE,"^",3) QUIT
  . IF (TYPE'=1)&(TYPE'=2) QUIT
  . IF NOTEDT'>0 QUIT
  . SET ORIG=$$TRIM^XLFSTR($PIECE(LINE,"^",2))
  . IF USEPARSE,$$TRIM^XLFSTR($PIECE(LINE,"^",3))'="" SET ORIG=$$TRIM^XLFSTR($PIECE(LINE,"^",3))  ;"//kt //codex 11/1/26
  . IF ORIG="" QUIT
  . SET MEDNAME=$PIECE(LINE,"^",5),GRP=$SELECT(MEDNAME="":"U",1:"M")  ;"//kt //codex 11/1/26
  . ;"//kt //codex begin mod 2026-11-01 --
  . SET MEDINFO=$PIECE(LINE,"^",4)_"^"_MEDNAME_"^"_$PIECE(LINE,"^",6)_"^"_$PIECE(LINE,"^",7)
  . SET GROUPS(GRP,MEDNAME,ORIG,"INFO",MEDINFO)=""
  . ;"//kt //codex end mod 2026-11-01 --
  . SET PREV=$GET(GROUPS(GRP,MEDNAME,ORIG))
  . IF PREV="" SET GROUPS(GRP,MEDNAME,ORIG)=NOTEDT_"^"_NOTEDT QUIT
  . IF NOTEDT<$PIECE(PREV,"^",1) SET $PIECE(PREV,"^",1)=NOTEDT
  . IF NOTEDT>$PIECE(PREV,"^",2) SET $PIECE(PREV,"^",2)=NOTEDT
  . SET GROUPS(GRP,MEDNAME,ORIG)=PREV
  NEW COUNT SET COUNT=0,GRP=""
  FOR  SET GRP=$ORDER(GROUPS(GRP)) QUIT:GRP=""  DO
  . SET MEDNAME=""
  . FOR  SET MEDNAME=$ORDER(GROUPS(GRP,MEDNAME)) QUIT:MEDNAME=""  DO
  . . DO SUMGRP(.OUT,.COUNT,.GROUPS,GRP,MEDNAME)
  . IF $DATA(GROUPS(GRP,"")) DO SUMGRP(.OUT,.COUNT,.GROUPS,GRP,"")
  QUIT
  ;
SUMGRP(OUT,COUNT,GROUPS,GRP,MEDNAME) ;"//kt //codex added entire function 11/1/26
  ;"Input: OUT,COUNT -- PASS BY REFERENCE. Append one medication group.
  ;"       GROUPS -- PASS BY REFERENCE. Parsed groups from LISTSUM.
  ;"       GRP,MEDNAME -- group type and medication name.
  ;"Result: none.
  SET COUNT=COUNT+1,OUT(COUNT)="0^"_MEDNAME
  NEW ORIG SET ORIG=""
  FOR  SET ORIG=$ORDER(GROUPS(GRP,MEDNAME,ORIG)) QUIT:ORIG=""  DO
  . SET COUNT=COUNT+1,OUT(COUNT)="1^"_ORIG_"^"_GROUPS(GRP,MEDNAME,ORIG)
  . ;"//kt //codex begin mod 2026-11-01 --
  . NEW MEDINFO SET MEDINFO=""
  . FOR  SET MEDINFO=$ORDER(GROUPS(GRP,MEDNAME,ORIG,"INFO",MEDINFO)) QUIT:MEDINFO=""  DO
  . . SET COUNT=COUNT+1,OUT(COUNT)="2^"_MEDINFO
  . ;"//kt //codex end mod 2026-11-01 --
  QUIT
  ;
LISTHIST(OUT,TMGDFN,SDT,EDT) ;"//kt //codex added entire function 11/1/26
  ;"Input: OUT -- PASS BY REFERNECE.  An OUT parameter.  Format:
  ;"        OUT(#)=<data line>  Data line syntax:
  ;"          OUT(#)=<line type>^<data>
  ;"          OUT(#)='0^<TIUIEN>^<FMDT>   0 is metadata for following lines, until another 0 node
  ;"          OUT(#)='1^<medication line>  1 is an INCOMING med table entry line
  ;"          OUT(#)='2^<medication line>  1 is an FINAL med table entry line
  ;"          Medication line fields: original text^parsed text^medication IEN^medication name^strength^units.  ;"//kt //codex 11/1/26
  ;"       TMGDFN -- PATIENT record number
  ;"       SDT,EDT -- FMDT for date rante.  Dates are inclusive; a date-only EDT includes the whole day.
  ;"Result: none
  KILL OUT
  SET SDT=+$GET(SDT),EDT=+$GET(EDT)
  IF EDT'>0 SET EDT=9999999
  IF EDT=EDT\1 SET EDT=EDT+.235959
  IF SDT>EDT SET OUT(1)="-1^Start date is after end date" QUIT
  NEW GL,ROOT SET GL=$GET(^DIC(22733.5,0,"GL"))
  IF $EXTRACT(GL,$LENGTH(GL))'="," SET OUT(1)="-1^Invalid history file global root" QUIT
  SET ROOT=$EXTRACT(GL,1,$LENGTH(GL)-1)_")"
  NEW PARENT SET PARENT=+$ORDER(@ROOT@("B",TMGDFN,0))
  IF PARENT'>0 QUIT
  IF +$ORDER(@ROOT@("B",TMGDFN,PARENT))>0 SET OUT(1)="-1^Duplicate patient history records" QUIT
  NEW HX,NODE,TIUIEN,TYPE,NOTEDT,SELECTED  ;"//kt //codex 11/1/26
  SET HX=0
  FOR  SET HX=$ORDER(@ROOT@(PARENT,1,HX)) QUIT:HX'>0  DO
  . SET NODE=$GET(@ROOT@(PARENT,1,HX,0))
  . SET TIUIEN=+NODE,TYPE=$PIECE(NODE,"^",2),NOTEDT=+$PIECE(NODE,"^",3)
  . IF (TIUIEN'>0)!(NOTEDT'>0)!((TYPE'="I")&(TYPE'="F")) QUIT
  . IF (NOTEDT<SDT)!(NOTEDT>EDT) QUIT  ;"//kt //codex 11/1/26
  . SET SELECTED(NOTEDT,TIUIEN,HX)=TYPE
  NEW COUNT SET COUNT=0,NOTEDT=0
  FOR  SET NOTEDT=$ORDER(SELECTED(NOTEDT)) QUIT:NOTEDT'>0  DO
  . SET TIUIEN=0
  . FOR  SET TIUIEN=$ORDER(SELECTED(NOTEDT,TIUIEN)) QUIT:TIUIEN'>0  DO
  . . SET HX=0
  . . FOR  SET HX=$ORDER(SELECTED(NOTEDT,TIUIEN,HX)) QUIT:HX'>0  DO
  . . . SET TYPE=SELECTED(NOTEDT,TIUIEN,HX)
  . . . SET COUNT=COUNT+1,OUT(COUNT)="0^"_TIUIEN_"^"_NOTEDT
  . . . NEW MEDIEN SET MEDIEN=0
  . . . FOR  SET MEDIEN=$ORDER(@ROOT@(PARENT,1,HX,1,MEDIEN)) QUIT:MEDIEN'>0  DO
  . . . . NEW ORIG,PARSED,MEDNODE,MEDNAME,LINE
  . . . . SET ORIG=$PIECE($GET(@ROOT@(PARENT,1,HX,1,MEDIEN,0)),"^",1)
  . . . . SET PARSED=$GET(@ROOT@(PARENT,1,HX,1,MEDIEN,2))
  . . . . SET MEDNODE=$GET(@ROOT@(PARENT,1,HX,1,MEDIEN,3))
  . . . . SET MEDNAME=$PIECE($GET(^TMG(22733,+MEDNODE,0)),"^",1)
  . . . . ;"//kt //codex begin mod 2026-11-01 --
  . . . . SET LINE=$TRANSLATE(ORIG,"^","~")_"^"_$TRANSLATE(PARSED,"^","~")_"^"_$PIECE(MEDNODE,"^",1)
  . . . . SET LINE=LINE_"^"_$TRANSLATE(MEDNAME,"^","~")_"^"_$PIECE(MEDNODE,"^",2)_"^"_$PIECE(MEDNODE,"^",3)
  . . . . ;"//kt //codex end mod 2026-11-01 --
  . . . . SET COUNT=COUNT+1,OUT(COUNT)=$SELECT(TYPE="I":1,1:2)_"^"_LINE
  QUIT
  ;
REBUILDALL ;"//kt //codex added entire function 11/1/26
  ;"Rebuild medication history for all patients, with resumable patient markers.
  NEW RESTARTREF,RESUMEANS SET RESTARTREF=$NAME(^TMG("TMP","REBUILDALL^TMGRX011")),RESUMEANS=1
  IF $DATA(@RESTARTREF) DO
  . NEW % SET %=1
  . WRITE !,"Resume the previous medication history rebuild"
  . DO YN^DICN WRITE !
  . IF %=2 KILL @RESTARTREF
  . SET RESUMEANS=$SELECT((%=1)!(%=2):%,1:-1)
  ;"YN^DICN may return an abort; do not clear markers or continue then.
  IF $GET(RESUMEANS)=-1 QUIT
  NEW TOTAL,TMGDFN SET (TOTAL,TMGDFN)=0
  FOR  SET TMGDFN=$ORDER(^DPT(TMGDFN)) QUIT:TMGDFN'>0  DO
  . IF '$DATA(^DPT(TMGDFN,0)) QUIT
  . IF $DATA(@RESTARTREF@(TMGDFN)) QUIT
  . SET TOTAL=TOTAL+1
  IF TOTAL=0 WRITE !,"No patients remain to be processed.",! QUIT
  NEW COUNT,FAILED,DONE,STARTTIME,ERRORS
  SET (COUNT,FAILED,DONE,TMGDFN)=0,STARTTIME=$H
  WRITE !,"Press ESC between patients to stop and resume later.",!
  FOR  SET TMGDFN=$ORDER(^DPT(TMGDFN)) QUIT:(TMGDFN'>0)!DONE  DO
  . IF '$DATA(^DPT(TMGDFN,0)) QUIT
  . IF $DATA(@RESTARTREF@(TMGDFN)) QUIT
  . IF $$USRABORT^TMGUSRI2("medication history rebuild") SET DONE=1 QUIT
  . NEW PTLABEL SET PTLABEL="Patient: "_$PIECE($GET(^DPT(TMGDFN,0)),"^",1)  ;"//kt //codex 11/1/26
  . DO PROGBAR^TMGUSRI2(COUNT,PTLABEL,0,TOTAL,80,STARTTIME)  ;"//kt //codex 11/1/26
  . NEW PROGBARSTATE MERGE PROGBARSTATE=^TMP("TMG-PROGRESS-BAR",$J)
  . WRITE !
  . NEW OPTION,STATUS SET OPTION("SHOW PROG")=1,OPTION("NESTED")=1,OPTION("SHOW ERRORS")=1  ;"//kt //codex 11/1/26
  . DO PARSE1PT(TMGDFN,.OPTION,.STATUS)
  . IF $GET(STATUS("SCANNED"))=0 DO PROGBAR^TMGUSRI2(1,"No notes",0,1,60,$H)
  . DO CUU^TMGTERM(1)
  . KILL ^TMP("TMG-PROGRESS-BAR",$J) MERGE ^TMP("TMG-PROGRESS-BAR",$J)=PROGBARSTATE
  . IF STATUS>0 SET @RESTARTREF@(TMGDFN)=""
  . ELSE  DO
  . . SET FAILED=FAILED+1,ERRORS(TMGDFN)=""
  . . MERGE ERRORS(TMGDFN)=STATUS("ERROR")
  . SET COUNT=COUNT+1
  . DO PROGBAR^TMGUSRI2(COUNT,PTLABEL,0,TOTAL,80,STARTTIME)  ;"//kt //codex 11/1/26
  WRITE !,$SELECT(DONE:"Rebuild stopped.",1:"Rebuild finished.")," Patients processed: ",COUNT,"; with errors: ",FAILED,!
  SET TMGDFN=0
  FOR  SET TMGDFN=$ORDER(ERRORS(TMGDFN)) QUIT:TMGDFN'>0  DO
  . NEW IEN8925 SET IEN8925=0
  . FOR  SET IEN8925=$ORDER(ERRORS(TMGDFN,IEN8925)) QUIT:IEN8925'>0  DO
  . . WRITE !,"Patient IEN ",TMGDFN,", TIU IEN ",IEN8925,": ",ERRORS(TMGDFN,IEN8925)
  IF FAILED WRITE !
  QUIT
  ;
 ;"-----------------------------------------------------------------------  //kt //codex 11/1/26
TEST3 ;"//kt //codex added entire function 11/1/26
  ;"Select a patient and process medication history.
  NEW X,Y,DIC SET DIC=2,DIC(0)="MAEQ"
  DO ^DIC WRITE !
  IF +Y'>0 QUIT
  NEW TMGDFN SET TMGDFN=+Y
  ;"//kt //codex begin mod 2026-11-01 --
  NEW OPTION SET OPTION("SHOW PROG")=1
  DO PARSE1PT(TMGDFN,.OPTION)
  ;"//kt //codex end mod 2026-11-01 --
  QUIT
  ;
TEST2 ;"//kt //codex added entire function 11/1/26
  ;"Parse the specified test document and file its medication history.
  NEW IEN8925,OUT,RESULT SET IEN8925=267147
  DO PARSETIU(IEN8925,.OUT)
  DO SHOWPARSE(.OUT)
  SET RESULT=$$FILEHIST(IEN8925,.OUT)
  WRITE !,"History filing: ",RESULT,!
  QUIT
  ;
TEST ;"//kt //codex added entire function 11/1/26
  ;"Select a patient and a note containing an incoming or final medication table.
  NEW X,Y,DIC SET DIC=2,DIC(0)="MAEQ"
  DO ^DIC WRITE !
  IF +Y'>0 QUIT
  NEW TMGDFN SET TMGDFN=+Y
  NEW NOTES,OPTION SET OPTION("ALL NOTES")=1
  WRITE !,"Searching patient notes for medication tables...",!
  DO GNOTELST^TMGTIUO5(TMGDFN,.NOTES,999999,.OPTION)
  NEW TOTAL,SCANNED,STARTTIME SET (TOTAL,SCANNED)=0,STARTTIME=$H
  NEW CHECKDT,CHECKIEN SET CHECKDT=""
  FOR  SET CHECKDT=$ORDER(NOTES(CHECKDT)) QUIT:CHECKDT=""  DO
  . SET CHECKIEN=0
  . FOR  SET CHECKIEN=$ORDER(NOTES(CHECKDT,CHECKIEN)) QUIT:CHECKIEN'>0  SET TOTAL=TOTAL+1
  NEW MENU,COUNT,NOTEDT,IEN8925 SET COUNT=0,NOTEDT=""
  SET MENU(0)="Select a note containing a medication table"
  FOR  SET NOTEDT=$ORDER(NOTES(NOTEDT),-1) QUIT:NOTEDT=""  DO
  . SET IEN8925=0
  . FOR  SET IEN8925=$ORDER(NOTES(NOTEDT,IEN8925)) QUIT:IEN8925'>0  DO
  . . NEW TEMP,MARKER,FOUND SET FOUND=""
  . . FOR MARKER="[MEDICATIONS]","[FINAL MEDICATIONS]" DO
  . . . KILL TEMP
  . . . IF $$XTRCTSPC^TMGTIUO5(IEN8925,MARKER,"BLANK_LINE",.TEMP) DO
  . . . . SET FOUND=FOUND_$SELECT(FOUND'="":", ",1:"")_MARKER
  . . SET SCANNED=SCANNED+1
  . . IF (SCANNED#10=0)!(SCANNED=TOTAL) DO PROGBAR^TMGUSRI2(SCANNED,"Scanning notes",1,TOTAL,60,STARTTIME)
  . . QUIT:FOUND=""
  . . NEW ZN,TITLE SET ZN=$GET(^TIU(8925,IEN8925,0))
  . . SET TITLE=$PIECE($GET(^TIU(8925.1,+ZN,0)),"^",1)
  . . SET COUNT=COUNT+1
  . . SET MENU(COUNT)=$$FMTE^XLFDT(NOTEDT)_"  "_TITLE_$CHAR(9)_IEN8925
  IF TOTAL>0 WRITE !
  IF COUNT=0 WRITE !,"No notes with medication tables were found.",! QUIT
  NEW PICK SET PICK=$$MENU^TMGUSRI2(.MENU,"^")
  IF +PICK'>0 QUIT
  WRITE !,"Selected TIU document IEN: ",+PICK,!
  NEW RESULT DO PARSETIU(+PICK,.RESULT)
  DO SHOWPARSE(.RESULT)
  QUIT
  ;
 ;"-----------------------------------------------------------------------  //kt //codex 11/1/26
PARSE1PT(TMGDFN,OPTION,STATUS) ;"//kt //codex added entire function 11/1/26
  ;"Parse and file medication tables from the notes of one patient.
  ;"OPTION("SHOW PROG")=1 enables the progress bar. Default is off.  //kt //codex 11/1/26
  ;"OPTION("NESTED")=1 suppresses routine messages for a nested progress display.  ;"//kt //codex 11/1/26
  ;"OPTION("SHOW ERRORS")=1 displays each filing error immediately.  ;"//kt //codex 11/1/26
  ;"STATUS returns 1 on success, or -1 on errors, with errors by TIU IEN.  ;"//kt //codex 11/1/26
  KILL STATUS SET STATUS=-1  ;"//kt //codex 11/1/26
  SET TMGDFN=+$GET(TMGDFN)
  IF TMGDFN'>0 WRITE !,"Invalid patient IEN.",! QUIT
  ;"//kt //codex begin mod 2026-11-01 --
  NEW NOTES,NOTEOPT SET NOTEOPT("ALL NOTES")=1
  IF '$GET(OPTION("NESTED")) WRITE !,"Getting patient notes...",!  ;"//kt //codex 11/1/26
  DO GNOTELST^TMGTIUO5(TMGDFN,.NOTES,999999,.NOTEOPT)
  ;"//kt //codex end mod 2026-11-01 --
  NEW NOTEDT,IEN8925,TOTAL SET NOTEDT="",TOTAL=0
  FOR  SET NOTEDT=$ORDER(NOTES(NOTEDT)) QUIT:NOTEDT=""  DO
  . SET IEN8925=0
  . FOR  SET IEN8925=$ORDER(NOTES(NOTEDT,IEN8925)) QUIT:IEN8925'>0  SET TOTAL=TOTAL+1
  IF TOTAL=0 DO  QUIT  ;"//kt //codex 11/1/26
  . SET STATUS=1,STATUS("SCANNED")=0
  . IF '$GET(OPTION("NESTED")) WRITE !,"No notes found.",!
  NEW SCANNED,FILED,SKIPPED,FAILED,STARTTIME,ERRORS
  SET (SCANNED,FILED,SKIPPED,FAILED)=0,STARTTIME=$H,NOTEDT=""
  FOR  SET NOTEDT=$ORDER(NOTES(NOTEDT)) QUIT:NOTEDT=""  DO
  . SET IEN8925=0
  . FOR  SET IEN8925=$ORDER(NOTES(NOTEDT,IEN8925)) QUIT:IEN8925'>0  DO
  . . NEW OUT,RESULT DO PARSETIU(IEN8925,.OUT)
  . . IF '$GET(OUT("I","FOUND")),'$GET(OUT("F","FOUND")) SET SKIPPED=SKIPPED+1
  . . ELSE  DO
  . . . SET RESULT=$$FILEHIST(IEN8925,.OUT)
  . . . IF RESULT>0 SET FILED=FILED+1 QUIT
  . . . SET FAILED=FAILED+1,ERRORS(IEN8925)=RESULT
  . . . IF $GET(OPTION("SHOW ERRORS"))=1 WRITE !,"Patient IEN ",TMGDFN,", TIU IEN ",IEN8925,": ",RESULT,!,!  ;"//kt //codex 11/1/26
  . . SET SCANNED=SCANNED+1
  . . IF +$GET(OPTION("SHOW PROG"))=1,((SCANNED#10=0)!(SCANNED=TOTAL)) DO PROGBAR^TMGUSRI2(SCANNED,"Processing notes",0,TOTAL,60,STARTTIME)  ;"//kt //codex 11/1/26
  ;"//kt //codex begin mod 2026-11-01 --
  SET STATUS=$SELECT(FAILED>0:-1,1:1),STATUS("SCANNED")=SCANNED,STATUS("FILED")=FILED,STATUS("FAILED")=FAILED
  MERGE STATUS("ERROR")=ERRORS
  IF $GET(OPTION("NESTED")) QUIT
  WRITE !,"Notes scanned: ",SCANNED,"; filed: ",FILED,"; without medication tables: ",SKIPPED,"; errors: ",FAILED,!
  SET IEN8925=0
  FOR  SET IEN8925=$ORDER(ERRORS(IEN8925)) QUIT:IEN8925'>0  WRITE !,"TIU IEN ",IEN8925,": ",ERRORS(IEN8925)
  IF FAILED WRITE !
  ;"//kt //codex end mod 2026-11-01 --
  QUIT
  ;
SHOWTABL(IEN8925) ;"//kt //codex added entire function 11/1/26
  ;"Extract and display the incoming and final medication tables from a TIU note.
  NEW MARKER,TABLE,FOUND
  FOR MARKER="[MEDICATIONS]","[FINAL MEDICATIONS]" DO
  . KILL TABLE
  . SET FOUND=$$XTRCTSPC^TMGTIUO5(+$GET(IEN8925),MARKER,"BLANK_LINE",.TABLE)
  . WRITE !,MARKER,!
  . IF 'FOUND WRITE "Table not found.",! QUIT
  . ZWRITE TABLE
  QUIT
  ;
PARSETIU(IEN8925,OUT) ;"//kt //codex added entire function 11/1/26
  ;"Extract and parse both medication tables from a TIU document.
  ;"OUT(type,"FOUND")=1 when the table was extracted; type is I or F.
  ;"OUT(type,line,"ORIG TEXT"/"PARSED TEXT"/"MEDICATION"/"STRENGTH"/"UNITS")
  ;"contains full untruncated text and the fields intended for history storage.
  KILL OUT
  NEW TYPE,MARKER,TABLE,IDX,COUNT,LINE,INFO,PARSED
  FOR TYPE="I","F" DO
  . SET MARKER=$SELECT(TYPE="I":"[MEDICATIONS]",1:"[FINAL MEDICATIONS]")
  . KILL TABLE
  . SET OUT(TYPE,"FOUND")=$$XTRCTSPC^TMGTIUO5(+$GET(IEN8925),MARKER,"BLANK_LINE",.TABLE)
  . IF 'OUT(TYPE,"FOUND") QUIT
  . SET (IDX,COUNT)=0
  . FOR  SET IDX=$ORDER(TABLE(IDX)) QUIT:IDX'>0  DO
  . . SET LINE=$$TRIM^XLFSTR($TRANSLATE($GET(TABLE(IDX)),$CHAR(9)," "))
  . . QUIT:LINE=""
  . . QUIT:$EXTRACT(LINE,1)="*"
  . . QUIT:LINE["[OLD ENTRY]"
  . . QUIT:LINE["[MEDICATION"
  . . QUIT:LINE["[FINAL MEDICATION"
  . . KILL INFO
  . . DO PARSELN^TMGRX001(.INFO,LINE)
  . . SET PARSED=$$TRIM^XLFSTR($$EXTERNAL^TMGRX003(.INFO))
  . . SET COUNT=COUNT+1
  . . SET OUT(TYPE,COUNT,"ORIG TEXT")=LINE
  . . SET OUT(TYPE,COUNT,"PARSED TEXT")=PARSED
  . . SET OUT(TYPE,COUNT,"MEDICATION")=$GET(INFO("IEN22733"))
  . . SET OUT(TYPE,COUNT,"STRENGTH")=$GET(INFO("STRENGTH"))
  . . SET OUT(TYPE,COUNT,"UNITS")=$GET(INFO("UNITS"))
  QUIT
  ;
SHOWPARSE(OUT) ;"//kt //codex added entire function 11/1/26
  ;"Display parsed history fields for review without filing them.
  NEW TYPE,IDX
  FOR TYPE="I","F" DO
  . WRITE !,$SELECT(TYPE="I":"[MEDICATIONS]",1:"[FINAL MEDICATIONS]"),!
  . IF '$GET(OUT(TYPE,"FOUND")) WRITE "Table not found.",! QUIT
  . SET IDX=0
  . FOR  SET IDX=$ORDER(OUT(TYPE,IDX)) QUIT:IDX'>0  DO
  . . WRITE !,IDX,". ",$GET(OUT(TYPE,IDX,"ORIG TEXT")),!
  . . WRITE "   Parsed: ",$GET(OUT(TYPE,IDX,"PARSED TEXT")),!
  . . WRITE "   Medication IEN: ",$GET(OUT(TYPE,IDX,"MEDICATION"))
  . . WRITE "  Strength: ",$GET(OUT(TYPE,IDX,"STRENGTH")),"  Units: ",$GET(OUT(TYPE,IDX,"UNITS")),!
  QUIT
  ;
FILEHIST(IEN8925,OUT) ;"//kt //codex added entire function 11/1/26
  ;"File parsed tables; replace entries for the same patient, note and table type.
  ;"OUT is the array returned by PARSETIU.  Result: 1^OK or -1^error.
  ;"Only tables found in OUT are filed.  FileMan changes are one transaction.
  NEW RESULT SET RESULT="1^OK"
  SET IEN8925=+$GET(IEN8925)
  NEW ZN SET ZN=$GET(^TIU(8925,IEN8925,0))
  NEW TMGDFN SET TMGDFN=+$PIECE(ZN,"^",2)
  NEW NOTEDT SET NOTEDT=+$PIECE(ZN,"^",7)
  IF (IEN8925'>0)!(TMGDFN'>0) QUIT "-1^Invalid TIU document or patient"
  IF NOTEDT'>0 QUIT "-1^Document episode date/time is missing"
  IF '$GET(OUT("I","FOUND")),'$GET(OUT("F","FOUND")) QUIT "-1^No medication tables found"
  NEW GL,ROOT SET GL=$GET(^DIC(22733.5,0,"GL"))
  IF $EXTRACT(GL,$LENGTH(GL))'="," QUIT "-1^Invalid history file global root"
  SET ROOT=$EXTRACT(GL,1,$LENGTH(GL)-1)_")"
  IF $TLEVEL>0 QUIT "-1^Cannot file inside an existing transaction"
  LOCK +^TMG("LOCK","MED HISTORY",TMGDFN):10 ELSE  QUIT "-1^Patient medication history is busy"
  TSTART ():SERIAL
  NEW PARENT SET PARENT=+$ORDER(@ROOT@("B",TMGDFN,0))
  IF PARENT>0,+$ORDER(@ROOT@("B",TMGDFN,PARENT))>0 SET RESULT="-1^Duplicate patient history records"
  IF (RESULT>0),(PARENT'>0) DO
  . NEW TMGFDA,IENS,MSG
  . SET TMGFDA(22733.5,"+1,",.01)=TMGDFN
  . DO UPDATE^DIE("","TMGFDA","IENS","MSG")
  . ;"//kt //codex end mod 2026-11-01 --
  . IF $DATA(MSG("DIERR")) SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.MSG) QUIT
  . SET PARENT=+$GET(IENS(1))
  . IF PARENT'>0 SET RESULT="-1^Unable to determine patient history IEN"
  NEW TYPE FOR TYPE="I","F" DO  QUIT:RESULT'>0
  . IF '$GET(OUT(TYPE,"FOUND")) QUIT
  . NEW HX,HXIEN,MATCHES SET (HX,HXIEN,MATCHES)=0
  . FOR  SET HX=$ORDER(@ROOT@(PARENT,1,HX)) QUIT:HX'>0  DO
  . . NEW NODE SET NODE=$GET(@ROOT@(PARENT,1,HX,0))
  . . IF (+NODE=IEN8925),($PIECE(NODE,"^",2)=TYPE) SET HXIEN=HX,MATCHES=MATCHES+1
  . IF MATCHES>1 SET RESULT="-1^Duplicate history entries for document and table type "_TYPE QUIT
  . NEW TMGFDA,IENS,MSG
  . IF HXIEN'>0 DO
  . . SET TMGFDA(22733.51,"+1,"_PARENT_",",.01)=IEN8925
  . . SET TMGFDA(22733.51,"+1,"_PARENT_",",.02)=TYPE
  . . SET TMGFDA(22733.51,"+1,"_PARENT_",",.03)=NOTEDT
  . . DO UPDATE^DIE("","TMGFDA","IENS","MSG")
  . . IF $DATA(MSG("DIERR")) SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.MSG) QUIT
  . . SET HXIEN=+$GET(IENS(1))
  . . IF HXIEN'>0 SET RESULT="-1^Unable to determine history entry IEN"
  . ELSE  DO
  . . SET TMGFDA(22733.51,HXIEN_","_PARENT_",",.03)=NOTEDT
  . . DO FILE^DIE("","TMGFDA","MSG")
  . . IF $DATA(MSG("DIERR")) SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.MSG) QUIT
  . QUIT:RESULT'>0
  . NEW DA,DIK,REF SET REF=$NAME(@ROOT@(PARENT,1,HXIEN,1))
  . SET DIK=$EXTRACT(REF,1,$LENGTH(REF)-1)_",",DA(2)=PARENT,DA(1)=HXIEN,DA=0
  . FOR  SET DA=$ORDER(@ROOT@(PARENT,1,HXIEN,1,DA)) QUIT:DA'>0  DO ^DIK
  . NEW IDX SET IDX=0
  . FOR  SET IDX=$ORDER(OUT(TYPE,IDX)) QUIT:(IDX'>0)!(RESULT'>0)  DO
  . . NEW TMGFDA,IENS,MSG,ENTRY,ORIG,PARSED,MED,STRENGTH,UNITS
  . . SET ENTRY="+1,"_HXIEN_","_PARENT_","
  . . SET ORIG=$GET(OUT(TYPE,IDX,"ORIG TEXT")),PARSED=$GET(OUT(TYPE,IDX,"PARSED TEXT"))
  . . IF $LENGTH(ORIG)>250 SET ORIG=$EXTRACT(ORIG,1,247)_"..."
  . . IF $LENGTH(PARSED)>250 SET PARSED=$EXTRACT(PARSED,1,247)_"..."
  . . SET MED=+$GET(OUT(TYPE,IDX,"MEDICATION"))
  . . SET STRENGTH=$GET(OUT(TYPE,IDX,"STRENGTH")),UNITS=$GET(OUT(TYPE,IDX,"UNITS"))
  . . ;"//kt //codex begin mod 2026-11-01 --
  . . IF ($LENGTH(STRENGTH)>32)!($LENGTH(UNITS)>16) DO  QUIT
  . . . SET RESULT="-1^Field exceeds maximum length: "
  . . . IF $LENGTH(STRENGTH)>32 SET RESULT=RESULT_"STRENGTH ("_$LENGTH(STRENGTH)_", max 32): ["_STRENGTH_"] "
  . . . IF $LENGTH(UNITS)>16 SET RESULT=RESULT_"UNITS ("_$LENGTH(UNITS)_", max 16): ["_UNITS_"]"
  . . IF PARSED["^" SET RESULT="-1^PARSED TEXT contains a caret: ["_PARSED_"]" QUIT
  . . ;"//kt //codex end mod 2026-11-01 --
  . . SET TMGFDA(22733.511,ENTRY,.01)=ORIG
  . . IF PARSED'="" SET TMGFDA(22733.511,ENTRY,.02)=PARSED
  . . IF MED>0 SET TMGFDA(22733.511,ENTRY,1)=MED
  . . IF STRENGTH'="" SET TMGFDA(22733.511,ENTRY,1.2)=STRENGTH
  . . IF UNITS'="" SET TMGFDA(22733.511,ENTRY,1.3)=UNITS
  . . DO UPDATE^DIE("","TMGFDA","IENS","MSG")
  . . IF $DATA(MSG("DIERR")) SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.MSG)
  IF RESULT>0 TCOMMIT
  ELSE  TROLLBACK
  LOCK -^TMG("LOCK","MED HISTORY",TMGDFN)
  QUIT RESULT
  ;
