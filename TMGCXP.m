TMGCXP ;TMG/kst/Chart Exporter ;3/4/21
         ;;1.0;TMG-LIB;**1**;07/12/05
 ;  
 ;"TMG Chart exporter
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 3/4/21  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"EXPORT  -- To ask for parameters, select output, and do actual export
 ;
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"=======================================================================
 
CHARTEXP(DESTDIR)  ;"CHART EXPORTER
  SET DESTDIR=$GET(DESTDIR) 
  NEW RESULT SET RESULT="1^OK"
  NEW LIST DO GETLIST(.LIST)
  NEW INFO SET INFO=""
  FOR  SET INFO=$ORDER(LIST(INFO)) QUIT:(INFO="")!(RESULT'>0)  DO
  . NEW FNAME SET FNAME=$PIECE(INFO,"^",1)
  . NEW FMNUM SET FMNUM=$PIECE(INFO,"^",2)
  . SET FNAME="exported_"_$TRANSLATE(FNAME," ","_")_"["_FMNUM_"].txt"
  . SET RESULT=$$EXPORTALLRECS(FMNUM,DESTDIR,FNAME)
  . IF RESULT'>0 DO  QUIT
  . . WRITE !,"ERROR: ",$PIECE(RESULT,"^",2),!
  . WRITE !,"DONE WITH: ",INFO,!
  WRITE !,"Leaving Chart Exporter.  Goodbye.",!
  QUIT
  ;
EXPORTALLRECS(FMNUM,PATH,FNAME) ;"Export all records
  ;"Input: FMNUM -- NUMBER of fileman file
  ;"       PATH -- ouput file path on HFS
  ;"       FNAME -- filename on HFS
  ;"NOTE: pattern copied from EXPORT^TMGXMLEX
        ;"Purpose: To ask for parameters, select output, and do actual export
  NEW EXPARRAY,REFARR SET REFARR=$NAME(EXPARRAY)
  NEW TMGRESULT
  NEW HANDLE SET HANDLE="TMG_EXPORT_HANDLE_"_$JOB
  ;
  SET TMGRESULT=$$SETUPARR(FMNUM,REFARR)
  IF TMGRESULT'>0 GOTO EARDN
  ;
  DO OPEN^%ZISH(HANDLE,PATH,FNAME,"W")  ;"result stored in POP variable 
  IF POP DO  GOTO EARDN
  . SET TMGRESULT="-1^Error opening output file: "_FNAME
  ;
  NEW TMGDEBUG SET TMGDEBUG=0
  IF TMGDEBUG=0 USE IO
  ;
  DO WTXMLOUT^TMGXMLE2(REFARR,,,1)
  ;
  DO CLOSE^%ZISH(HANDLE)  ;" Close the output device
  ;
  GOTO EARDN
EARDN  ;
  KILL TMGXDEBUG,ERRFOUND
  QUIT TMGRESULT        
 ;
SETUPARR(FMNUM,REFARR)  ;"Setup array for exporting Fileman file
  ;"INPUT: FMNUM -- NUMBER of fileman file
  ;"       REFARR -- Pass by NAME of array
  ;"RESULT: 1^OK, or -1^ErrMessage
  ;"NOTE: See documentation in TMGXMLE2 for format of array to setup. 
  ;
  NEW TMGRESULT SET TMGRESULT="1^OK"  ;"DEFAULT
  SET FMNUM=+$GET(FMNUM)
  IF FMNUM'>0 DO  GOTO SUADN
  . SET TMGRESULT="-1^Fileman file number not provided to SETUPARR^TMGCXP"
  SET @REFARR@(FMNUM,"*")=""
  SET @REFARR@("!DOCTYPE")="TMG_EXPORT"
  SET @REFARR@("EXPORT_SYSTEM_NAME")="SOURCE VISTA"
  ;
SUADN 
  QUIT TMGRESULT
  ;
GETLIST(OUT)  ;"Read list at DATA into an array
  NEW TMGI
  NEW DONE SET DONE=0
  FOR TMGI=1:1 DO  QUIT:DONE
  . NEW INFO SET INFO=$PIECE($TEXT(DATA+TMGI^TMGCXP),";;",2)
  . SET INFO=$$TRIM^XLFSTR(INFO)
  . IF INFO="<END>" SET DONE=1 QUIT
  . SET OUT(INFO)=""
  QUIT
 ;
 ;
DATA   ;"NAMES OF FILEMAN FILES TO EXPORT
  ;;NEW PERSON^200
  ;;PATIENT^2
  ;;GMRV VITAL MEASUREMENT^120.5
  ;;PHARMACY PATIENT^55
  ;;PROBLEM^9000011
  ;;LAB DATA^63
  ;;TIU DOCUMENT^8925
  ;;<END>
