TMGCXP ;TMG/kst/Chart Exporter ;3/4/21, 9/29/21
         ;;1.0;TMG-LIB;**1**;07/12/05
 ;
 ;"TMG Chart exporter
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 3/4/21 Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"MENU ; 
 ;"CHARTEXP(DESTDIR,OPTION) -- CHART EXPORTER  
 ;"EXPORTALLFILES(DESTDIR,OPTION)  ;
 ;"ASKEXP1UTIL(OPTION)  -- Pick 1 patient and export data for 1 patient.  With utility options
 ;"ASKEXP1(DESTDIR,OPTION)  -- Pick 1 patient and export data for 1 patient.  With utility options
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"EXP1PT2F(TMGDFN,DESTDIR,OPTION) -- Export data for 1 given patient
 ;"EXP1PT(TMGDFN,SHOWEMPTY,OPTION) -- Export data for 1 given patient
 ;"EXPORTFILES(LIST,DESTDIR,OPTION) ;
 ;"SETUPARR(FMNUM,REFARR,RECARR) -- Setup array for exporting Fileman file
 ;"GETALLFILES(OUT) -- Get list of ALL Fileman files into array  
 ;"GETLIST(OUT) -- Read list at DATA into an array
 ;"EXPORTALLRECS(FMNUM,PATH,FNAME,OPTION) -- Export all records
 ;"EXPORTICD(DESTDIR,OPTION) ;
 ;"CHECKRX() ;
 ;"EXPORTPCP() ;
 ;"GETRECS(OUT,FILE,FLD,IDX,TMGDFN) -- Search records for matching TMGDFN in field
 ;"SETUPOPTION(OPTION) ;
 ;"ENSURDIR(DIR)  -- Ensure directory exists
 ;"RENAME(DIR,OLDFNAME,NEWFNAME)  ;
 ;"OUTPUTLOG(LOGFILE,MSG) ;
 ;"GETDIR(ROOTDIR,NUM,MODE)  ;
 ;"INIT() ;
 ;"FILEXIST(PATH,FNAME) -- copied and modified from FILEXIST^TMGIOUTL 
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;
 ;"NOTE: This code was written as part of a contract project to export patient
 ;" data data from a VistA system that was being retired
 ;
MENU ; 
  DO INIT()
  NEW MENU,TMGUSERINPUT
  NEW DESTDIR SET DESTDIR="C:\USERS\DANIEL.SCHNEIDER\DESKTOP\exportedData\"   
  NEW MIDX SET MIDX=1
  NEW OPTION
  SET OPTION("LOGFILE")=DESTDIR_"^"_"ExportLog.txt"
  SET OPTION("PROGRESSBAR")=1
  SET OPTION("TREEDIR")=0
M1 ;
  KILL MENU
  SET MENU(0)="Enter Option"
  SET MENU(MIDX)="Change Dest Dir (Current: '"_DESTDIR_"')"_$CHAR(9)_"DESTDIR",MIDX=MIDX+1
  SET MENU(MIDX)="Export records from LIST of files"_$CHAR(9)_"EXPORTLIST",MIDX=MIDX+1
  SET MENU(MIDX)="Export ALL files"_$CHAR(9)_"EXPORTALL",MIDX=MIDX+1
  SET MENU(MIDX)="Export data for 1 patient"_$CHAR(9)_"EXPORT1PATIENT",MIDX=MIDX+1
  SET MENU(MIDX)="Export data for 1 patient (UTILITY mode)"_$CHAR(9)_"EXPORT1PTUTIL",MIDX=MIDX+1      
  SET MENU(MIDX)="Export ICD records linked to ZZHDCREGISTRY entries"_$CHAR(9)_"EXPORTICD",MIDX=MIDX+1
  SET MENU(MIDX)=$SELECT($GET(OPTION("TREEDIR"))=1:"Enable",1:"Disable")_" output into nested directories"_$CHAR(9)_"TOGGLETREE",MIDX=MIDX+1
  SET MENU(MIDX)="Show PCE team PCP"_$CHAR(9)_"EXPORTPCP",MIDX=MIDX+1
  SET MENU(MIDX)="Show CheckRx utility"_$CHAR(9)_"CHECKRX",MIDX=MIDX+1
  SET MENU(MIDX)=$SELECT($GET(OPTION("PROGRESSBAR"))=1:"Enable",1:"Disable")_" progress bar during output"_$CHAR(9)_"TOGGLEPROG",MIDX=MIDX+1
  SET MENU(MIDX)="QUIT"_$CHAR(9)_"^",MIDX=MIDX+1
  ;
  SET TMGUSERINPUT=$$MENU^TMGUSRI2(.MENU,"^")
  IF TMGUSERINPUT="^" GOTO MNUDN
  IF TMGUSERINPUT=0 SET TMGUSERINPUT=""
  ;                                                      
M2 ;
  IF TMGUSERINPUT="DESTDIR" DO  GOTO M1
  . WRITE !,"Current output directory is:",!
  . WRITE "  [",DESTDIR,"]",!
  . NEW TEMP
  . WRITE "Enter new (^ to abort): " READ TEMP
  . IF "\/"'[$EXTRACT(TEMP,$LENGTH(TEMP)) SET TEMP=TEMP_"/"
  . WRITE !,"Set output directory to: [",TEMP,"]"
  . NEW % SET %=1 DO YN^DICN WRITE !
  . IF %'=1 QUIT
  . SET DESTDIR=TEMP
  . IF $GET(OPTION("LOGFILE"))'="" SET $PIECE(OPTION("LOGFILE"),"^",1)=TEMP
  IF TMGUSERINPUT="EXPORTLIST" DO  GOTO M1
  . DO CHARTEXP(DESTDIR,.OPTION)
  IF TMGUSERINPUT="EXPORTALL" DO  GOTO M1
  . DO EXPORTALLFILES(DESTDIR,.OPTION)
  IF TMGUSERINPUT="EXPORT1PATIENT" DO  GOTO M1
  . DO ASKEXP1(DESTDIR,.OPTION)
  IF TMGUSERINPUT="EXPORT1PTUTIL" DO  GOTO M1
  . DO ASKEXP1UTIL(.OPTION)
  IF TMGUSERINPUT="EXPORTICD" DO  GOTO M1
  . DO EXPORTICD(DESTDIR,.OPTION) ;  
  IF TMGUSERINPUT="TOGGLETREE" DO  GOTO M1
  . WRITE !,!,"note: Nested directory output not yet implemented, so leaving OFF",!,!
  . QUIT  ;"fix later...
  . SET OPTION("TREEDIR")='$GET(OPTION("TREEDIR"))
  IF TMGUSERINPUT="TOGGLEPROG" DO  GOTO M1
  . SET OPTION("TOGGLEPROG")='$GET(OPTION("TOGGLEPROG"))
  IF TMGUSERINPUT="EXPORTPCP" DO  GOTO M1
  . DO EXPORTPCP()
  IF TMGUSERINPUT="CHECKRX" DO  GOTO M1
  . DO CHECKRX()
  ;
  GOTO M1
MNUDN ;
  QUIT
  ;"===================================================== 
  ;"===================================================== 
  ;
CHARTEXP(DESTDIR,OPTION) ;"CHART EXPORTER  
  DO INIT()
  SET DESTDIR=$GET(DESTDIR)
  IF DESTDIR="" SET DESTDIR="C:\USERS\DANIEL.SCHNEIDER\DESKTOP\exportedData\"
  NEW RESULT SET RESULT="1^OK"
  NEW LIST DO GETLIST(.LIST)
  DO EXPORTFILES(.LIST,DESTDIR,.OPTION)
  WRITE !,"Leaving Chart Exporter. Goodbye.",!
  QUIT
  ;
EXPORTALLFILES(DESTDIR,OPTION)  ;
  NEW LIST DO GETALLFILES(.LIST)
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(LIST(IDX)) QUIT:(IDX="")  DO
  . NEW INFO SET INFO=$GET(LIST(IDX)) QUIT:INFO=""
  . NEW FNAME SET FNAME=$PIECE(INFO,"^",1)
  . NEW FMNUM SET FMNUM=$PIECE(INFO,"^",2)
  . NEW TEMPFNAME SET TEMPFNAME="exported_"_$TRANSLATE(FNAME," ","_")_"["_FMNUM_"]"
  . NEW PARTFNAME SET PARTFNAME=TEMPFNAME_".partial"
  . NEW DONEFNAME SET DONEFNAME=TEMPFNAME_".txt"
  . NEW OUTDIR SET OUTDIR=$$GETDIR(DESTDIR,FMNUM,1)
  . DO EXPORTFILES(.LIST,OUTDIR,.OPTION)
  QUIT
  ;  
ASKEXP1UTIL(OPTION)  ;"Pick 1 patient and export data for 1 patient.  With utility options
  DO INIT()
  NEW X,Y,DIC SET DIC=2,DIC(0)="MAEQ"
  DO ^DIC WRITE !
  IF Y'>0 WRITE "No patient selected. Aborting."  QUIT   
  ;
  NEW % SET %=2
  WRITE "Display empty fields"
  DO YN^DICN WRITE !
  IF %=-1 GOTO ASKDN
  NEW SHOWEMPTY SET SHOWEMPTY=(%=1)
  ;
  NEW % SET %=2
  WRITE "XML output mode"
  DO YN^DICN WRITE !
  IF %=-1 GOTO ASKDN
  IF %=1 DO SETUPOPTION(.OPTION)  ;"Note: if not set here, then defaults will be used downstream. 
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
  DO ^%ZIS ;"standard device call
  IF POP DO  GOTO ASKDN
  . DO SHOWERR^TMGDEBU2(,"Error opening output. Aborting.")
  USE IO
  ;
  ZWRITE OPTION
  DO EXP1PT(+Y,SHOWEMPTY,.OPTION)
  DO ^%ZISC
  WRITE !,"Press ENTER to continue"
  READ X
  ;"DO PRESS2GO^TMGUSRI2
  ;
ASKDN ;
  QUIT
  ;
ASKEXP1(DESTDIR,OPTION)  ;"Pick 1 patient and export data for 1 patient.  With utility options
  NEW X,Y,DIC SET DIC=2,DIC(0)="MAEQ"
  DO ^DIC WRITE !
  IF Y'>0 WRITE "No patient selected. Aborting."  QUIT   
  DO EXP1PT2F(+Y,DESTDIR,.OPTION)
  QUIT
  ;
  ;"--------------------------------------------
  ;"Intermediate level  functions. 
  ;"--------------------------------------------
EXP1PT2F(TMGDFN,DESTDIR,OPTION) ;"Export data for 1 given patient
  ;"Input: TMGDFN -- patient record number
  ;
  DO SETUPOPTION(.OPTION) 
  ;
  NEW TMGRESULT SET TMGRESULT="1^OK"
  NEW PTNAME SET PTNAME=$$GET1^DIQ(2,TMGDFN,.01)
  SET FNAME="exported patient ["_$TRANSLATE(PTNAME," ","_")_"].txt"
  NEW HANDLE SET HANDLE="TMG_EXPORT_HANDLE_"_$JOB   
  DO OPEN^%ZISH(HANDLE,PATH,FNAME,"W") ;"result stored in POP variable
  IF POP DO  GOTO EXP1DN
  . SET TMGRESULT="-1^Error opening output file: "_FNAME
  DO EXP1PT(.TMGDFN,0,.OPTION)
  DO CLOSE^%ZISH(HANDLE) ;" Close the output device
  WRITE !,"Leaving Chart Exporter. Goodbye.",!
EXP1DN ;  
  QUIT TMGRESULT       
  ;
EXP1PT(TMGDFN,SHOWEMPTY,OPTION) ;"Export data for 1 given patient
  ;"Input: TMGDFN -- patient record number
  NEW PTNAME SET PTNAME=$$GET1^DIQ(2,TMGDFN,.01)
  WRITE !,!,"DUMPING INFORMATION FOR PATIENT: ",PTNAME,!,!
  NEW RESULT SET RESULT="1^OK"
  NEW IEN SET IEN=0
  NEW LIST DO GETLIST(.LIST)
  NEW INFO,JDX SET JDX=0
  FOR  SET JDX=$ORDER(LIST(JDX)) QUIT:(JDX="")!(RESULT'>0)  DO
  . SET INFO=LIST(JDX)
  . NEW FNAME SET FNAME=$PIECE(INFO,"^",1)
  . NEW FMNUM SET FMNUM=$PIECE(INFO,"^",2)
  . NEW LNKFLD SET LNKFLD=$PIECE(INFO,"^",3)
  . IF LNKFLD="@" QUIT  ;"//kt 5/5/21 -- if not linked to patient, don't export
  . WRITE "========= FILE: ",FNAME," =========",!
  . IF +LNKFLD=0 DO
  . . IF LNKFLD="LRDFN" DO
  . . . SET IEN=+$PIECE($GET(^DPT(TMGDFN,"LR")),"^",1)
  . . ELSE  DO
  . . . SET IEN=TMGDFN
  . . DO DUMPREC^TMGDEBU3(FMNUM,IEN,.SHOWEMPTY,,.OPTION)
  . ELSE  DO
  . . NEW IDX SET IDX=$PIECE(INFO,"^",4)
  . . NEW RECS SET RESULT=$$GETRECS(.RECS,FMNUM,LNKFLD,IDX,TMGDFN)
  . . IF RESULT'>0 DO  QUIT
  . . . WRITE !,"ERROR: ",$PIECE(RESULT,"^",2),!
  . . NEW IEN SET IEN=0
  . . FOR  SET IEN=$ORDER(RECS(FMNUM,IEN)) QUIT:IEN'>0  DO
  . . . DO DUMPREC^TMGDEBU3(FMNUM,IEN,.SHOWEMPTY,,.OPTION)
  . ;"WRITE "========= DONE WITH:",FNAME," =========",!
  ;
  WRITE !,"Leaving Chart Exporter. Goodbye.",!
  QUIT RESULT
  ;  
EXPORTFILES(LIST,DESTDIR,OPTION) ;
  ;"Input: LIST -- pass by reference.  Format: 
  ;           LIST(#)="<File Name>^<File Number>^<Field with linking pointer>^<Index name>
  ;"              (note: pieces 2, 3 used when exporting chart data for just 1 patient.)
  ;"       DESTDIR -- Root of folder to write files to.  NOTE: must have trailing divisor, e.g. "/" 
  ;"       OPTION -- optional.  
  ;"          OPTION("TREEDIR")=1 Optional.  If present, then exported files are put into tree directory structure
  ;"                              If not present, all files are put into DESTDIR
  ;"          OPTION("LOGFILE")=path^filename_of_log_file    Optional.  If not provided, log not written
  ;"Result:  None
  NEW TREEDIR SET TREEDIR=+$GET(OPTION("TREEDIR"))
  NEW LOGFILE SET LOGFILE=$GET(OPTION("LOGFILE"))
  NEW STATUS SET STATUS=""
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(LIST(IDX)) QUIT:(IDX="")!(RESULT'>0)  DO
  . SET STATUS="ERROR"  ;"default
  . NEW INFO SET INFO=$GET(LIST(IDX)) QUIT:INFO=""
  . NEW FNAME SET FNAME=$PIECE(INFO,"^",1)
  . NEW FMNUM SET FMNUM=$PIECE(INFO,"^",2)
  . NEW TEMPFNAME SET TEMPFNAME="exported_"_$TRANSLATE(FNAME," ","_")_"["_FMNUM_"]"
  . NEW PARTFNAME SET PARTFNAME=TEMPFNAME_".partial"
  . NEW DONEFNAME SET DONEFNAME=TEMPFNAME_".txt"
  . NEW OUTDIR SET OUTDIR=$$GETDIR(DESTDIR,FMNUM,TREEDIR)
  . SET RESULT=$$EXPORTALLRECS(FMNUM,OUTDIR,PARTFNAME,.OPTION)
  . IF RESULT>0 DO                                               
  . . SET STATUS="SUCCESS"
  . . NEW TEMP SET TEMP=$$RENAME(OUTDIR,PARTFNAME,DONEFNAME)
  . . IF TEMP'>0 DO  QUIT  
  . . . IF LOGFILE'="" DO OUTPUTLOG(LOGFILE,"ERROR attempting rename from "_PARTFNAME_" to "_DONEFNAME_" in directory "_OUTDIR)
  . WRITE STATUS,"  WITH: ",INFO,!
  . IF LOGFILE'="" DO OUTPUTLOG(LOGFILE,STATUS_" writing "_TEMPFNAME)
  QUIT;
  ;
SETUPARR(FMNUM,REFARR,RECARR) ;"Setup array for exporting Fileman file
  ;"INPUT: FMNUM -- NUMBER of fileman file
  ;" REFARR -- Pass by NAME of array
  ;" RECARR -- OPTIONAL. Format:
  ;" RECARR(1234)=""
  ;" RECARR(2345)=""
  ;" if not provided, then "*" for all records is used.
  ;"RESULT: 1^OK, or -1^ErrMessage
  ;"NOTE: See documentation in TMGXMLE2 for format of array to setup.
  ;
  NEW TMGRESULT SET TMGRESULT="1^OK"  ;"DEFAULT
  SET FMNUM=+$GET(FMNUM)
  IF FMNUM'>0 DO  GOTO SUADN
  . SET TMGRESULT="-1^Fileman file number not provided to SETUPARR^TMGCXP"
  IF $DATA(RECARR)=0 DO
  . SET @REFARR@(FMNUM,"*")=""
  ELSE  DO
  . MERGE @REFARR@(FMNUM)=RECARR
  SET @REFARR@("FLAGS","i")=""   ;"indent xml output
  SET @REFARR@("!DOCTYPE")="TMG_EXPORT"
  SET @REFARR@("EXPORT_SYSTEM_NAME")="SOURCE VISTA"
  ;
SUADN 
  QUIT TMGRESULT
  ;  
GETALLFILES(OUT) ;Get list of ALL Fileman files into array  
  ;"Input:  OUT -- pass by reference, and OUT parameter.  Format:
  ;           OUT(#)="<File Name>^<File Number>
  NEW IDX SET IDX=$ORDER(OUT(""),-1)+1
  NEW FNUM SET FNUM=""
  FOR  SET FNUM=$ORDER(^DIC(FNUM)) QUIT:+FNUM'=FNUM  DO
  . NEW ZN SET ZN=$GET(^DIC(FNUM,0)) QUIT:ZN=""
  . SET OUT(IDX)=$PIECE(ZN,"^",1)_"^"_+$PIECE(ZN,"^",2),IDX=IDX+1
  QUIT
  ;  
GETLIST(OUT) ;"Read list at DATA into an array
  ;"Input:  OUT -- pass by reference, and OUT parameter.  Format:
  ;           OUT(#)="<File Name>^<File Number>^<Field with linking pointer>^<Index name>
  ;"Result: none.  
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
EXPORTALLRECS(FMNUM,PATH,FNAME,OPTION) ;"Export all records
  ;"Input: FMNUM -- NUMBER of fileman file
  ;" PATH -- ouput file path on HFS
  ;" FNAME -- filename on HFS
  ;" OPTION -- optional.  If OPTION("PROGRESSBAR")=1, then progress bar shown.  
  ;"NOTE: pattern copied from EXPORT^TMGXMLEX
        ;"Purpose: To ask for parameters, select output, and do actual export
  NEW EXPARRAY,REFARR SET REFARR=$NAME(EXPARRAY)
  NEW SHOWPROG SET SHOWPROG=+$GET(OPTION("PROGRESSBAR"))
  NEW TMGRESULT
  NEW HANDLE SET HANDLE="TMG_EXPORT_HANDLE_"_$JOB
  SET TMGRESULT=$$SETUPARR(FMNUM,REFARR)
  IF TMGRESULT'>0 GOTO EARDN
  DO OPEN^%ZISH(HANDLE,PATH,FNAME,"W") ;"result stored in POP variable
  IF POP DO  GOTO EARDN
  . SET TMGRESULT="-1^Error opening output file: "_FNAME
  NEW TMGDEBUG SET TMGDEBUG=0
  IF TMGDEBUG=0 USE IO
  DO WTXMLOUT^TMGXMLE2(REFARR,,,SHOWPROG)
  DO CLOSE^%ZISH(HANDLE) ;" Close the output device
EARDN  ;
  KILL TMGXDEBUG,ERRFOUND
  QUIT TMGRESULT        
  ;
  ;"--------------------------------------------
  ;"Alternative export functions. 
  ;"--------------------------------------------
EXPORTICD(DESTDIR,OPTION) ;" Cycle through every entry in ZZHDCREGISTRY file, and export all pointed-to ICD records
  ;"Input DESTDIR -- the output directory
  ;"      OPTION -- optional.  If OPTION("PROGRESSBAR")=1, then progress bar shown.  
  ;"Result: 1^OK, or -1^Error message
  ;"NOTE: filed #4, "HD DIAGNOSIS CODE" is the only field pointing to ICD DIAGNOSIS (#80)
  ;"^DIZ(9001969
  NEW IENARR
  NEW IEN SET IEN=0
  FOR  SET IEN=$ORDER(^DIZ(9001969,IEN)) QUIT:IEN'>0  DO
  . NEW NODE SET NODE=$GET(^DIZ(9001969,IEN,2)) QUIT:NODE=""
  . NEW PTR SET PTR=+$PIECE(NODE,"^",1) QUIT:PTR'>0
  . SET IENARR(PTR)=""
  ;
  NEW FNAME SET FNAME="exported_ICD_DIAGNOSIS_FILE[80].txt"  
  NEW SHOWPROG SET SHOWPROG=+$GET(OPTION("PROGRESSBAR"))
  NEW EXPARRAY,REFARR SET REFARR=$NAME(EXPARRAY)
  NEW TMGRESULT
  NEW HANDLE SET HANDLE="TMG_EXPORT_HANDLE_"_$JOB
  SET TMGRESULT=$$SETUPARR(80,REFARR,.IENARR)
  IF TMGRESULT'>0 GOTO EICDDN
  DO OPEN^%ZISH(HANDLE,DESTDIR,FNAME,"W") ;"result stored in POP variable
  IF POP DO  GOTO EICDDN
  . SET TMGRESULT="-1^Error opening output file: "_FNAME
  NEW TMGDEBUG SET TMGDEBUG=0
  IF TMGDEBUG=0 USE IO
  DO WTXMLOUT^TMGXMLE2(REFARR,,,SHOWPROG)
  DO CLOSE^%ZISH(HANDLE) ;" Close the output device
EICDDN  ;
  KILL ERRFOUND
  QUIT TMGRESULT        
  ;
CHECKRX() ;
  NEW CT SET CT=0
  NEW IEN SET IEN=0
  FOR  SET IEN=$ORDER(^PSDRUG(IEN)) QUIT:IEN'>0  DO
  . NEW ZN SET ZN=$GET(^PSDRUG(IEN,0))
  . NEW W SET W=$GET(^PSDRUG(IEN,"WARN"))
  . NEW OLDWARN SET OLDWARN=($PIECE(ZN,"^",8)'="")
  . NEW NEWWARN SET NEWWARN=($PIECE(W,"^",1)'="")
  . IF (OLDWARN=0)&(NEWWARN=0) QUIT
  . NEW WARN SET WARN=$$DRUG^PSSWRNA(IEN,0)
  . SET CT=CT+1
  . WRITE CT," (",IEN,") ",$P(ZN,"^",1)," --> ",WARN,!
  QUIT
  ;
EXPORTPCP() ;
  NEW CT SET CT=0
  NEW TMGDFN SET TMGDFN=31709-1
  FOR  SET TMGDFN=$ORDER(^DPT(TMGDFN)) QUIT:TMGDFN'>0  DO
  . NEW LS
  . DO TEAMGET^VFDCPCMM(.LST,TMGDFN)
  . IF '$DATA(LST) DO  QUIT
  . . SET CT=CT+1
  . . IF CT#25 WRITE !
  . . WRITE "." QUIT
  . ;"D PCEDETAIL^VFDCPCMM1(.VFDR,.VFDL,VFDDFN)
  . DO PCEDETAIL^VFDCPCMM(.LST,TMGDFN) ; Hijacking RPC ORWPT1 PCEDETAIL
  . ZWRITE LST
  QUIT
  ;  
  ;"--------------------------------------------
  ;"Utilities
  ;"--------------------------------------------
GETRECS(OUT,FILE,FLD,IDX,TMGDFN) ;"Search records for matching TMGDFN in field
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
  . . IF VAL'=TMGDFN QUIT
  . . SET OUT(FILE,IEN)=""
  ELSE  IF IDX'="" DO  ;"use index
  . FOR  SET IEN=$ORDER(@CREF@(IDX,TMGDFN,IEN)) QUIT:IEN'>0  DO
  . . SET OUT(FILE,IEN)=""
GRDN  ;
  QUIT RESULT
  ;  
SETUPOPTION(OPTION) ;
  SET OPTION("WRITE FILE LABEL FN")="WTFILLBL^TMGXMLE5"       
  SET OPTION("WRITE REC LABEL FN")="WTRLABEL^TMGXMLE5"  
  SET OPTION("WRITE FLD LABEL FN")="WTFLABEL^TMGXMLE5" 
  SET OPTION("WRITE LINE FN")="WTLINE^TMGXMLE5" 
  SET OPTION("WRITE WP LINE")="WWPLINE^TMGXMLE5"
  QUIT
  ;
ENSURDIR(DIR)  ;"Ensure directory exists
  ;"... finish later.  
  QUIT
  ;
RENAME(DIR,OLDFNAME,NEWFNAME)  ;
  ;"Result: 1 = success, 0 = failure
  NEW RESULT SET RESULT=$$MV^%ZISH(DIR,OLDFNAME,DIR,NEWFNAME)
  QUIT RESULT
  ;
OUTPUTLOG(LOGFILE,MSG) ;
  ;"Input: LOGFILE.  Format: path^filename_of_log_fil
  NEW PATH SET PATH=$PIECE(LOGFILE,"^",1)
  NEW FNAME SET FNAME=$PIECE(LOGFILE,"^",2)
  NEW HANDLE SET HANDLE="TMGEXPORT"
  DO OPEN^%ZISH(HANDLE,PATH,FNAME,"A")
  QUIT:POP  ;"error state...
  USE IO
  NEW NOW SET NOW=$$NOW^XLFDT
  WRITE $$FMTE^XLFDT(NOW,"5FPSZ")," ",MSG,!
  DO CLOSE^%ZISH(HANDLE)
  QUIT
  ;  
GETDIR(ROOTDIR,NUM,MODE)  ;
  ;"Input: ROOTDIR -- root directory  NOTE: must contain trailing divisor ("/")
  ;"       NUM -- The Fileman number, e.g. 8925.1  (note: can contain decimal)
  ;"       MODE -- 1 --> return a directory tree based on file number
  ;"               0 --> just return ROOTDIR
  ;"Result:  e.g. If NUM=8925.1  --> <ROOTDIR>_"8000_to_8999/"
  NEW RESULT SET RESULT=ROOTDIR  
  IF MODE=1 DO
  . NEW TEMP SET TEMP=NUM\1000
  . NEW PAD
  . IF $LENGTH(TEMP)<4 DO
  . . SET PAD=$$RJ^XLFSTR(TEMP,4,"0")
  . ELSE  DO   ;"//e.g. 1130580001.501 --> TEMP="1130580"
  . . SET PAD=TEMP
  . NEW DIR SET DIR=PAD_"000_to_"_PAD_"999/"
  . SET RESULT=ROOTDIR_DIR
  QUIT RESULT
  ;
INIT() ;
  DO DT^DICRW ;"Ensure fileman variables setup.
  IF +$GET(DUZ)'>0 DO
  . WRITE !,"First, lets set up environment",!
  . NEW DIC SET DIC=200
  . SET DIC(0)="MAEQ"
  . SET DIC("A")="Please type your name: "
  . SET DIC("?")="Please enter your user name, to setup environmental variables."
  . DO ^DIC WRITE !
  . IF +Y'>0 QUIT
  . DO DUZ^XUP(+Y)
  QUIT
  ;  
FILEXIST(PATH,FNAME) ; copied and modified from FILEXIST^TMGIOUTL
  ;"Purpose: To determine if file exists.
  ;"Input: PATH --path of file to test, e.g. "/tmp/myfiles/a/"
  ;"Input: FNAME -- the name of file to test, e.g. "test.txt"
  ;"Results: 1 if file exists (and is unique), 0 if not
  ;"Note: If PATH_FNAME indicates a directory, then 0 is returned.
  ;"      Note IF FULLNAMEPATH contains a * pattern, that would cause multiple
  ;"              files to be returned, then filename is not unique, and function
  ;"              will RETURN THAT IT IS NOT A (unique) FILE
  ;
  NEW TMGMASK,TMGFILES
  NEW RESULT SET RESULT=0
  SET TMGMASK(FNAME)=""
  IF $$LIST^%ZISH(PATH,"TMGMASK","TMGFILES")=1 DO
  . SET RESULT=($DATA(TMGFILE(FNAME))>0)
  QUIT RESULT
  ;  
  ;"--------------------------------------------
  ;" Data
  ;"--------------------------------------------
  ;"NAMES OF FILEMAN FILES TO EXPORT
  ;"FORMAT: ;;<File Name>^<File Number>^<Field with linking pointer>^<Index name>
DATA ;
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
  ;;RACE^10^@^@
  ;;ZZ HD FOREIGN COUNTRIES^9001970^@^@
  ;;ZZ LETTER^9001600^@^@
  ;;ZZ MEXICAN STATES^9001603^@^@
  ;;ZZHDC LAB TESTS^9001610^@^@
  ;;ZZHDC MEDICATIONS^9901602^@^@
  ;;ZZHDC PATHOLOGY FILE^9001550^@^@
  ;;ZZHDCREGISTRY^9001969^.01^B
  ;;ZZINDEXCASE^9001971^@^@
  ;;ZZINDNUMBERS^9001613^@^@
  ;;ZZTHALIDPROVIDERS^9001612^@^@
  ;;ORDER^100^.02^AC
  ;;REQUEST-CONSULTATION^123^.02^AD
  ;;OUTPATIENT ENCOUNTER^409.68^.02^ADFN
  ;;OUTPATIENT PROFILE^404.41^.01^B
  ;;INSURANCE COMPANY^36^0
  ;;PENDING OUTPATIENT ORDERS^52.41^0
  ;;PRESCRIPTION^52^2^APSOD
  ;;VFD PATIENT^21600^.01^B
  ;;DSIO EXTERNAL ENTITIES^19641.1^0
  ;;REQUEST-CONSULTATION^123^0
  ;;<END>
 ;
 ;
 ;;OPTION --> ZZ NHDP DISABILITY DATA INPUT