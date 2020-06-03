TMGQIO ;TMG/kst/Quiet IO routines ;03/25/06, 2/2/14
        ;;1.0;TMG-LIB;**1**;11/01/04
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"'QUIET IO   To provide routines for QUITe (non-interactive) IO that programs can call.
 ;"=============================================================================
 ;"
 ;"'QUIET IO"
 ;"
 ;"Purpose:
 ;"  To provide routines for QUITe (non-interactive) IO that programs can call.
 ;"     i.e. replacement routines for READ and WRITE
 ;"
 ;"Functions:
 ;"  OUTP(SILENT,A,B,C,D,E,F,G,H,I,J)
 ;"  WOUT(S)
 ;"  SILENTW(S) -- puts output into INFO("TEXT")
 ;"  INP(VAR,SILENT,TIMEOUT,SILNTVAL,A,B,C,D,E,F,G,H,I,J)
 ;"
 ;"Dependancies:
 ;"  IF TMGDEBUG defined, then requires TMGDEBUG.m
 ;"=============================================================================

OUTP(SILENT,A,B,C,D,E,F,G,H,I,J)
  ;"Purpose: To provide an output channel for this program module.  Will allow
  ;"    converting to a "SILENT-OUTPUT" mode.
  ;
  IF $GET(SILENT,0)=1 DO  GOTO OPQUIT
  . IF '$$SILENTW(.A) QUIT
  . IF '$$SILENTW(.B) QUIT
  . IF '$$SILENTW(.C) QUIT
  . IF '$$SILENTW(.D) QUIT
  . IF '$$SILENTW(.E) QUIT
  . IF '$$SILENTW(.F) QUIT
  . IF '$$SILENTW(.G) QUIT
  . IF '$$SILENTW(.H) QUIT
  . IF '$$SILENTW(.I) QUIT
  . IF '$$SILENTW(.J) QUIT
  ELSE  DO  GOTO OPQUIT
  . IF '$$WOUT(.A) QUIT
  . IF '$$WOUT(.B) QUIT
  . IF '$$WOUT(.C) QUIT
  . IF '$$WOUT(.D) QUIT
  . IF '$$WOUT(.E) QUIT
  . IF '$$WOUT(.F) QUIT
  . IF '$$WOUT(.G) QUIT
  . IF '$$WOUT(.H) QUIT
  . IF '$$WOUT(.I) QUIT
  . IF '$$WOUT(.J) QUIT
  ;
OPQUIT
  QUIT
  ;
  ;
WOUT(S)
  ;"Purpose: To WRITE out S, or newline IF "!" passed
  ;"Result: 1 IF text output, 0 IF it wasn't
  ;
  ;"IF $GET(TMGDEBUG)>0 DO DEBUGMSG^TMGDEBU4(.TMGDBINDENT,"S='",$GET(S),"'")
  NEW RESULT SET RESULT=0
  IF $DATA(S)'=0 DO
  . SET RESULT=1
  . IF S="!" WRITE ! QUIT
  . IF ($EXTRACT(S,1)="?")&(+$EXTRACT(S,2,256)>0) DO
  . . NEW INDENT,I
  . . SET INDENT=+$EXTRACT(S,2,256)
  . . FOR I=1:1:INDENT WRITE " "
  . ELSE  WRITE S
  QUIT RESULT
  ;
  ;
SILENTW(S)
  ;"Purpose: To take text and put in INFO Array
  ;"Result: 1 IF text output, 0 IF it wasn't
  ;
 NEW RESULT SET RESULT=0
 IF $DATA(S)=0 GOTO SWQ
 NEW LINE
 SET LINE=$GET(INFO("TEXT","LINES"),1)
 DO DEBUGMSG^TMGDEBU4(.TMGDBINDENT,"s=",S)
 IF S="!" DO
 . IF $DATA(INFO("TEXT",LINE))=0 SET INFO("TEXT",LINE)=" "
 . SET INFO("TEXT","LINES")=LINE+1
 ELSE  DO
 . IF $EXTRACT(S,1)="?" SET S="" ;"Ignore ?x's
 . SET INFO("TEXT",LINE)=$GET(INFO("TEXT",LINE)," ")_S
 SET RESULT=1
SWQ
 QUIT RESULT
 ;
 ;
INP(VAR,SILENT,TIMEOUT,SILNTVAL,A,B,C,D,E,F,G,H,I,J)
        ;"Purpose: To provide an input that may or may not be silent
        ;"Input VAR: variable to input. SHOULD PASS BY REFERENCE
        ;"      SILENT: 1=silent (will get value from SILNTVAL), 0=interactive
        ;"      TIMEOUT: value to timeout user input (optional, will default to 120)
        ;"      SILNTVAL: the value to use to assign VAR IF SILENT=1
        ;"      A..J: optional prompts for input IF not in silent mode
 SET SILENT=$GET(SILENT,0)
 IF SILENT=1 DO
 . SET VAR=$GET(SILNTVAL)
 . IF $GET(TMGDEBUG)>0 DO DEBUGMSG^TMGDEBU4(.TMGDBINDENT,"Silent input used=",VAR)
 . DO OUTP(SILNTOUT,VAR,"!") ;//to show log the value used.
 ELSE  DO
 . DO OUTP(SILNTOUT,.A,.B,.C,.D,.E,.F,.G,.H,.I,.J)
 . SET TIMEOUT=$GET(TIMEOUT,120)
 . READ VAR:TIMEOUT
 . DO OUTP(SILNTOUT,"!")
 ;
 QUIT
 ;
 ;
