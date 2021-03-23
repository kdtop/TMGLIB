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
 ;
 ;"NOTE: This code was written as part of a contract project to export patient
 ;"       data data from a VistA system that was being retired
 ;
ASKEXP1  ;"Pick 1 patient and export data for 1 patient
  NEW X,Y,DIC SET DIC=2,DIC(0)="MAEQ"
  DO ^DIC WRITE !
  IF Y'>0 WRITE "No patient selected.  Aborting."  QUIT   
  ;
  NEW % SET %=2
  WRITE "Display empty fields"
  DO YN^DICN WRITE !
  IF %=-1 GOTO ASKDN
  NEW SHOWEMPTY SET SHOWEMPTY=(%=1)
  ;
  NEW OPTION
  NEW % SET %=2
  WRITE "XML output mode"
  DO YN^DICN WRITE !
  IF %=-1 GOTO ASKDN
  IF %=1 DO  ;"Note: if not set here, then defaults will be used downstream.
  . SET OPTION("WRITE FILE LABEL FN")="WTFILLBL^TMGXMLE5"       
  . SET OPTION("WRITE REC LABEL FN")="WTRLABEL^TMGXMLE5"  
  . SET OPTION("WRITE FLD LABEL FN")="WTFLABEL^TMGXMLE5" 
  . SET OPTION("WRITE LINE FN")="WTLINE^TMGXMLE5" 
  . SET OPTION("WRITE WP LINE")="WWPLINE^TMGXMLE5"
  ;
  NEW % SET %=2
  WRITE "Show progress bar"
  DO YN^DICN WRITE !
  IF %=-1 GOTO ASKDN
  IF %=1 DO
  . SET OPTION("SHOW PROG")=1
  ;
  NEW %ZIS
  SET %ZIS("A")="Enter Output Device: "
  SET %ZIS("B")="HOME"
  DO ^%ZIS  ;"standard device call
  IF POP DO  GOTO ASKDN
  . DO SHOWERR^TMGDEBU2(,"Error opening output.  Aborting.")
  USE IO
  ;  
  DO EXP1PT(+Y,SHOWEMPTY,.OPTION)  
  DO ^%ZISC
  DO PRESS2GO^TMGUSRI2
  ;  
ASKDN ;
  QUIT
  ;
EXP1PT2F(DFN,DESTDIR)  ;"Export data for 1 given patient
  ;"Input: DFN -- patient record number
  SET DESTDIR=$GET(DESTDIR) 
  IF DESTDIR="" SET DESTDIR="C:\USERS\DANIEL.SCHNEIDER\DESKTOP\exportedData\"
  NEW PTNAME SET PTNAME=$$GET1^DIQ(2,DFN,.01)
  SET FNAME="exported patient ["_$TRANSLATE(PTNAME," ","_")_"].txt"
  NEW HANDLE SET HANDLE="TMG_EXPORT_HANDLE_"_$JOB
   
  DO OPEN^%ZISH(HANDLE,PATH,FNAME,"W")  ;"result stored in POP variable 
  IF POP DO  GOTO EARDN
  . SET TMGRESULT="-1^Error opening output file: "_FNAME

  DO EXP1PT(.DFN)
  
  DO CLOSE^%ZISH(HANDLE)  ;" Close the output device
  
  WRITE !,"Leaving Chart Exporter.  Goodbye.",!
  
  QUIT RESULT       

EXP1PT(DFN,SHOWEMPTY,OPTION)  ;"Export data for 1 given patient
  ;"Input: DFN -- patient record number
  NEW PTNAME SET PTNAME=$$GET1^DIQ(2,DFN,.01)
  WRITE !,!,"DUMPING INFORMATION FOR PATIENT: ",PTNAME,!,!
  NEW RESULT SET RESULT="1^OK"
  NEW IEN SET IEN=0
  NEW LIST DO GETLIST(.LIST)
  NEW INFO,JDX SET JDX=0
  FOR  SET JDX=$ORDER(LIST(JDX)) QUIT:(JDX="")!(RESULT'>0)  DO
  . SET INFO=LIST(JDX)
  . NEW FNAME SET FNAME=$PIECE(INFO,"^",1)
  . WRITE "========= FILE: ",FNAME," =========",!
  . NEW FMNUM SET FMNUM=$PIECE(INFO,"^",2)
  . NEW LNKFLD SET LNKFLD=$PIECE(INFO,"^",3)                      
  . IF +LNKFLD=0 DO
  . . IF LNKFLD="LRDFN" DO
  . . . SET IEN=$PIECE($GET(^DPT(DFN,"LR")),"^",1)
  . . ELSE  DO
  . . . SET IEN=DFN
  . . DO DUMPREC^TMGDEBU3(FMNUM,IEN,.SHOWEMPTY,,.OPTION)
  . ELSE  DO
  . . NEW IDX SET IDX=$PIECE(INFO,"^",4)
  . . NEW RECS SET RESULT=$$GETRECS(.RECS,FMNUM,LNKFLD,IDX,DFN)
  . . IF RESULT'>0 DO  QUIT
  . . . WRITE !,"ERROR: ",$PIECE(RESULT,"^",2),!  
  . . NEW IEN SET IEN=0
  . . FOR  SET IEN=$ORDER(RECS(FMNUM,IEN)) QUIT:IEN'>0  DO
  . . . DO DUMPREC^TMGDEBU3(FMNUM,IEN,.SHOWEMPTY,,.OPTION)
  . ;"WRITE "========= DONE WITH:",FNAME," =========",!
  ;
  WRITE !,"Leaving Chart Exporter.  Goodbye.",!
  QUIT RESULT
  ;  
GETRECS(OUT,FILE,FLD,IDX,DFN) ;"Search records for matching DFN in field
  NEW RESULT SET RESULT="1^OK"
  NEW OREF SET OREF=$$GETGL^TMGFMUT2(FILE,1)
  IF OREF="" DO  GOTO GRDN
  . SET RESULT="-1^Unable to get global reference for file: "_FILE
  NEW CREF SET CREF=$$CREF^DILF(OREF)
  NEW IEN SET IEN=0
  IF IDX="!" DO  ;"search all records
  . NEW FLDINFO DO GFLDINFO^TMGDBAP3(FILE,FLD,"FLDINFO")
  . NEW STORELOC SET STORELOC=$GET(FLDINFO("STORELOC"))
  . NEW NODE,PCE SET NODE=$PIECE(STORELOC,";",1),PCE=+$PIECE(STORELOC,";",2)
  . IF (NODE="")!(PCE'>0) DO  QUIT
  . . SET RESULT="-1^Unable to get node or piece for storage location."
  . FOR  SET IEN=$ORDER(@CREF@(IEN)) QUIT:IEN'>0  DO
  . . NEW VAL SET VAL=$PIECE($GET(@CREF@(IEN,NODE)),"^",PCE)
  . . IF VAL'=DFN QUIT
  . . SET OUT(FILE,IEN)=""
  ELSE  IF IDX'="" DO  ;"use index
  . FOR  SET IEN=$ORDER(@CREF@(IDX,DFN,IEN)) QUIT:IEN'>0  DO
  . . SET OUT(FILE,IEN)=""
GRDN  ;  
  QUIT RESULT

  ;"--------------------------------------------
 
CHARTEXP(DESTDIR)  ;"CHART EXPORTER
  SET DESTDIR=$GET(DESTDIR) 
  IF DESTDIR="" SET DESTDIR="C:\USERS\DANIEL.SCHNEIDER\DESKTOP\exportedData\"
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
  SET @REFARR@("FLAGS","i")=""   ;"indent xml output
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
  . IF $EXTRACT(INFO,1)="-" QUIT
  . IF INFO="<END>" SET DONE=1 QUIT
  . SET OUT(TMGI)=INFO
  QUIT
 ;
 ;
DATA   ;"NAMES OF FILEMAN FILES TO EXPORT
  ;;PATIENT^2^0
  ;;IHS PATIENT^9000001^0
  ;;LAB DATA^63^LRDFN
  ;;PATIENT ALLERGY^120.8^.01^B
  ;;GMRV VITAL MEASUREMENT^120.5^.02^C
  ;;PHARMACY PATIENT^55^.01^B
  ;;PROBLEM^9000011^.02^!
  ;;TIU DOCUMENT^8925^.02^C
  ;;NEW PERSON^200
  ;;ADVERSE REACTION ASSESSMENT^120.86^.01^B
  ;;VISIT^9000010^.05^C
  ;;V PROVIDER^9000010.06^.02^C
  ;;V POV^9000010.07^.02^C
  ;;V IMMUNIZATION^9000010.11^.02^C
  ;;V SKIN TEST^9000010.12^.02^C
  ;;V EXAM^9000010.13^.02^C
  ;;V TREATMENT^9000010.15^.02^C
  ;;V PATIENT ED^9000010.16^.02^C
  ;;V CPT^9000010.18^.02^C
  ;;V HEALTH FACTORS^9000010.23^.02^C
  ;;<END>
