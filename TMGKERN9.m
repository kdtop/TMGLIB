TMGKERN9 ;TMG/kst/Interface to interact with web scraper for Veradigm; 5/23/24
         ;;1.0;TMG-LIB;**1**;5/23/24
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 5/23/24  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;
TEST0 ;
  NEW PTS 
  SET PTS(16835)=""
  SET PTS(48151)=""
  NEW OPTION SET OPTION("DEBUG")=1
  NEW DATA,MSG DO TEST1(.DATA,.PTS,.MSG,.OPTION)
  QUIT
  ;
TEST1(DATA,ARR,MSG,OPTION) ;
  NEW PATLIST
  NEW IDX SET IDX=1
  NEW DONE SET DONE=$DATA(ARR)
  FOR  QUIT:DONE  DO  
  . NEW DIC,X,Y SET DIC=2,DIC(0)="MAEQ"
  . DO ^DIC WRITE !
  . IF +Y>0 SET ARR(+Y)=""
  . SET %=1 WRITE !,"Pick another patient" DO YN^DICN WRITE !
  . SET DONE=(%'=1)
  DO FORMATARR(.PATLIST,.ARR)
  IF $DATA(PATLIST) ZWRITE PATLIST 
  WRITE !,"Scrape Veradigm for these patients" DO YN^DICN WRITE !
  IF %'=1 QUIT
  NEW RESULT,MSG
  SET RESULT=$$BATCHMEDS(.DATA,.PATLIST,.MSG,.OPTION)      
  QUIT
  ;
TEST2(DATE)  ;"Get records for patient for date of service (e.g. given day in clinic)
  ;"INPUT:  DATE -- OPTIONAL.  If not provided, user is asked.  Fileman format if provided.
  NEW OPTION
  SET OPTION("DEBUG")=2
  SET OPTION("PROGRESS")=2
  SET OPTION("ON STATUS LINE")="HNDLSTAT^TMGKERN9" 
  SET OPTION("ON DEBUG LINE")="HNDLDEBUG^TMGKERN9" 
  DO TEST3(.DATE,.OPTION)
  QUIT
  ;
HNDLSTAT(LINE) ;
  WRITE ?2,LINE,!
  QUIT
  ;
HNDLDEBUG(LINE)  ;
  WRITE ?15,LINE,!
  QUIT
  ;
TEST3(DATE,OPTION)  ;"Get records for patient for date of service (e.g. given day in clinic)
  ;"INPUT:  DATE -- OPTIONAL.  If not provided, user is asked.  Fileman format if provided.   
  NEW PTS,RESULTS,TEMP,MSG,DATA
  NEW REF SET REF=$NAME(^TMG("TMP","TEST3^TMGKERN9"))
  NEW ZZDEBUG SET ZZDEBUG=0
  IF ZZDEBUG=1 DO  GOTO T2L2
  . MERGE PTS=@REF@("PTS")
  . MERGE RESULT=@REF@("RESULTS")
  . MERGE TEMP=@REF@("TEMP")
  . MERGE MSG=@REF@("MSG")
  . MERGE DATA=@REF@("DATA") 
  NEW X,Y,%DT SET X=$GET(DATE)
  DO ^%DT SET DATE=Y
  IF DATE'>0 DO
  . SET %DT="AE",%DT("A")="Enter DATE to retrieve: "
  . DO ^%DT SET DATE=Y WRITE !
  IF Y'>0 WRITE "INVALID DATE. ABORTING",! QUIT
  DO GETPTBYHX(.TEMP,,DATE,DATE,1) 
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(TEMP(IDX)) QUIT:IDX'>0  SET PTS(IDX)=""
  KILL PTS(0) ;"kill any invalid lines....  
  IF +$GET(OPTION("DEBUG"))<1 SET OPTION("DEBUG")=1
  NEW PATLIST DO FORMATARR(.PATLIST,.PTS)
  IF $DATA(PATLIST) ZWRITE PATLIST 
  WRITE !,"Scrape Veradigm for these patients" DO YN^DICN WRITE !
  IF %'=1 QUIT
  SET RESULT=$$BATCHMEDS(.DATA,.PATLIST,.MSG,.OPTION)      
  KILL @REF@("PTS") MERGE @REF@("PTS")=PTS
  KILL @REF@("RESULTS") MERGE @REF@("RESULTS")=RESULTS
  KILL @REF@("TEMP") MERGE @REF@("TEMP")=TEMP 
  KILL @REF@("MSG") MERGE @REF@("MSG")=MSG 
  KILL @REF@("DATA") MERGE @REF@("DATA")=DATA 
T2L2 ;
  DO FILERXS(REF)
  QUIT
  ;
  ;"====================================================================
FORMATARR(OUT,ARR) ;
  ;"ARR -- FORMAT: ARR(DFN)=""
  ;"OUT -- FORMAT: OUT(ADFN)=LNAME^FNAME^DOBDATESTR^^^DFN
  NEW IDX SET IDX=1
  NEW ADFN SET ADFN=0
  FOR  SET ADFN=$ORDER(ARR(ADFN)) QUIT:ADFN'>0  DO
  . NEW ZN SET ZN=$GET(^DPT(ADFN,0))
  . NEW NAME SET NAME=$PIECE(ZN,"^",1)
  . NEW LNAME SET LNAME=$PIECE(NAME,",",1)
  . NEW FNAME SET FNAME=$PIECE(NAME,",",2)
  . NEW DOBFMDT SET DOBFMDT=$PIECE(ZN,"^",3)
  . NEW DOBSTR SET DOBSTR=$$FMTE^XLFDT(DOBFMDT,"5DZ")
  . NEW LINE SET LINE=LNAME_"^"_FNAME_"^"_DOBSTR_"^^^"_ADFN
  . SET OUT(IDX)=LINE,IDX=IDX+1
  QUIT
  ;
GETPTBYHX(OUT,ADUZ,SDT,EDT,MODE) ;"
  ;"INPUT: OUT -- PASS BY REFERENCE, AN OUT PARAMETER.  Format:
  ;"          OUT(<DFN>,<FM_DATE>)=""
  ;"       DUZ -- OPTIONAL. USER IEN.  If none, defaults to ALL users.  
  ;"       SDT -- FILEMAN START DATE RANGE
  ;"       EDT -- FILEMAN END DATE RANGE
  ;"       MODE -- THIS DETERMINES WHAT LIST IS RETURNED
  ;"               0 (DEFAULT) - PATIENT LIST FROM AUDIT LOG
  ;"               1 - PATIENT LIST FROM SCHEDULE
  ;"NOTE: Perhaps later I could also include patients that have had TIU DOCUMENT for date.   
  NEW AUDITARR
  NEW NAMEPIECE,DTPIECE
  NEW NAMEDELIM,DTDELIM
  SET MODE=+$GET(MODE)
  IF MODE=0 DO
  . ;"Get audit listing of accessed charts.  
  . DO GETAUDT1^TMGAUDT(.AUDITARR,.ADUZ,SDT,EDT,1)   ;"Last parameter of 1 -> Sort by Name
  . SET NAMEPIECE=1,NAMEDELIM="^"
  . SET DTPIECE=2,DTDELIM=" -- "
  ELSE  IF MODE=1 DO
  . ;"Get schedule  
  . DO GETAPPT^TMGAPPT1(.AUDITARR,.ADUZ,SDT,EDT,1)   ;"Last parameter of 1 -> Sort by Name
  . SET NAMEPIECE=1,NAMEDELIM="^"
  . SET DTPIECE=5,DTDELIM="^"
  NEW OUTIDX SET OUTIDX=0
  FOR  SET OUTIDX=$ORDER(AUDITARR(OUTIDX)) QUIT:OUTIDX'>0  DO
  . NEW LINE SET LINE=$G(AUDITARR(OUTIDX))
  . NEW TMGDFN,TMGDATE
  . SET TMGDFN=$PIECE(LINE,NAMEDELIM,NAMEPIECE),TMGDATE=$P(LINE,DTDELIM,DTPIECE)
  . IF MODE=0 SET TMGDATE=$$INTDATE(TMGDATE)
  . SET OUT(TMGDFN,TMGDATE)=""
  QUIT            
  ;
INTDATE(EXTDATE) ;"Convert YYYY/MM/DD@hh:mm:ss to FM Format
  NEW OUTDATE 
  NEW DATE,TIME
  SET DATE=$P(EXTDATE,"@",1),TIME="@"_$P(EXTDATE,"@",2)
  IF TIME="@99:99" SET TIME=""
  SET DATE=$P(DATE,"/",2)_"/"_$P(DATE,"/",3)_"/"_$P(DATE,"/",1)_TIME
  SET OUTDATE=$$EXT2FMDT^TMGDATE(DATE)
  IF OUTDATE="-1" WRITE EXTDATE,!
  QUIT OUTDATE
  ;  
  ;"============================================================
  ;  
BATCHMEDS(OUT,PATLIST,MSG,OPTION) ;"USE OPTIONAL BACKGROUND TASK TO ALLOW FOREGROUND MONITORING. 
  ;"Purpose: call out to node.js / puppeteer and scrape Veradigm for meds for list of patients
  ;"INPUT: OUT -- PASS BY REFERENCE.  AN OUT PARAMETER.  FORMAT:
  ;"          OUT(IDX)=<PATIENT STRING, AS FOUND IN PATLIST>
  ;"          OUT(IDX)='Error processing: '_FILENAME   <--- if error loading patient result file.  
  ;"          OUT(IDX,#)=<RX ARRAY>  -- see format in PARSERX().  # is index, 1 for each med in patient med list. 
  ;"       PATLIST -- PASS BY REFERENCE.  FORMAT:
  ;"          PATLIST(IDX)='LNAME^FNAME^DOB^SDT^EDT^DFN'   
  ;"                note: IDX must be sequential numbers, e.g. 1,2,3... due to limitations in GTF^%ZISH. 1,3,5,7, or DFN's would NOT work
  ;"                      LNAME -- patient last name
  ;"                      FNAME -- patient first name.  NOTE: only first part used.  E.g. JOHN BOY --> only JOHN used
  ;"                      DOB -- patient DOB
  ;"                      SDT -- optional start of search date range.  
  ;"                      EDT -- optional end of search date range.  
  ;"                               Date format is mm/dd/yyyy (e.g. 5/12/2024)
  ;"                      DFN -- recommended to allow tying results back to patient
  ;"       MSG -- OPTIONAL.  PASS BY REFERENCE.  AN OUT PARAMETER.  FORMAT" 
  ;"           MSG("STD OUT",#) -- lines of output (if any), output by node to std out at command-line
  ;"           MSG("ERR OUT",#) -- lines of output (if any), output by node to error channel at command-line
  ;"           MSG("LOG",#)     -- lines of output (if any), debug_run_log.txt
  ;"           MSG("STATUS",#)  -- lines of output (if any), status.txt file
  ;"       OPTION -- OPTIONAL -- PASS BY REFERENCE.  
  ;"           OPTION("DEBUG")=# -- default is 0.  Used to set debug run level for veridigm.js 
  ;"                               0 -> no output
  ;"                               1 -> output most basic info to status.txt (and debug_run_log)
  ;"                               2 -> log most function calls in Veradigm.js to debug_run_log
  ;"                               3 -> output maximum level, all the enters and exits of functions, 
  ;"                                        including puppeteer.js calls, to debug_run_log    
  ;"           OPTION("PROGRESS")=0, 1, or 2 -- default is 0.  
  ;"                       If >=1, then outputs progress from status.txt.  Will force DEBUG to be at least 1
  ;"                       if >=2, then also outputs debug_run_log.txt.    Will force DEBUG to be at least 1 
  ;"                       If >=1 and no event handlers provided, output will be written to the console.
  ;"                          This will involve running puppeteer script in separate background process
  ;"                       If 0 (or absent), then puppeteer script run in this process. 
  ;"           OPTION("ON STATUS LINE")=<function name>  e.g. "HNDLSTAT^TMGRTN"  <-- function must take 1 parameter, the line to output.  E.g. HNDLSTAT^TMGRTN(LINE)
  ;"                       NOTE: If provided, this will force OPTION("DEBUG") to be at least 1
  ;"           OPTION("ON DEBUG LINE")=<function name>   e.g. "HNDLDEBUG^TMGRTN"  <-- function must take 1 parameter, the line to output.  E.g. HNDLDEBUG^TMGRTN(LINE)
  ;"                       NOTE: If provided, this will force OPTION("DEBUG") to be at least 1
  ;"Result: 1^OK or -1^ErrorMessage
  NEW TMGIO SET TMGIO=$IO
  NEW TEMP,DEVSETUP,DEVSAVE DO DEV2ARR^TMGKERN1($IO,.DEVSAVE,,.DEVSETUP)
  NEW RESULT SET RESULT="1^OK"  ;"DEFAULT
  NEW CRED SET TEMP=$$GETCRDNTL(.CRED,"Veradigm")
  IF TEMP'>0 DO  GOTO BMDN2
  . SET RESULT="-1^Error getting Veridigm credentials"
  NEW HFSPATH SET HFSPATH=CRED("HFS PATH")
  IF $EXTRACT(HFSPATH,$LENGTH(HFSPATH))'="/" SET HFSPATH=HFSPATH_"/"
  NEW OUTPATH SET OUTPATH=HFSPATH_"output/"
  NEW PTLSTFNAME SET PTLSTFNAME=$$UNIQUE^%ZISUTL("patients.csv")
  SET TEMP=$$ARR2HFS^TMGIOUT3("PATLIST",OUTPATH,PTLSTFNAME)   ;"RETURNS: 0 IF failure, 1 IF success
  IF TEMP'>0 DO  GOTO BMDN2 
  . SET RESULT="-1^Error saving "_HFSPATH_PTLSTFNAME
  NEW DEBUG SET DEBUG=+$GET(OPTION("DEBUG"))
  NEW PROGRESS SET PROGRESS=+$GET(OPTION("PROGRESS"))
  NEW STATFN SET STATFN=$GET(OPTION("ON STATUS LINE"))
  NEW DBGLOGFN SET DBGLOGFN=$GET(OPTION("ON DEBUG LINE"))
  NEW SHOWSTAT SET SHOWSTAT=(STATFN]"")!(PROGRESS>0)
  NEW SHOWDBLOG SET SHOWDBLOG=(DBGLOGFN]"")!(PROGRESS>1)
  IF (SHOWSTAT!SHOWDBLOG),DEBUG=0 SET DEBUG=1
  NEW FOREGROUND SET FOREGROUND=(DEBUG=0)
  NEW STATFNAME SET STATFNAME=HFSPATH_"status.txt"
  NEW RUNLOGFNAME SET RUNLOGFNAME=HFSPATH_"debug_run_log.txt"
  DO LINUXCMD^TMGKERNL("touch "_STATFNAME)    ;"ensure status file exists.
  DO LINUXCMD^TMGKERNL("touch "_RUNLOGFNAME)  ;"ensure debug_run_log file exists.
  ;
  NEW MSGREF SET MSGREF=$NAME(^TMP($J,"BATCHMEDS")) KILL @MSGREF
  IF FOREGROUND DO
  . ;"Call script runner in this foreground task.  
  . DO RUNSCRIPT(PTLSTFNAME,MSGREF,DEBUG)  
  ELSE  DO      
  . ;"Launch script runner in background task.
  . WRITE "Launching Puppeteer script in background task...",!
  . JOB RUNSCRIPT(PTLSTFNAME,MSGREF,DEBUG)  ;"NOTE: DEBUG WILL BE AT LEAST 1, SO STATUS IS OUTPUT  
  . ;"NOTE: The above task will output to @MSGREF@("RESULT") when it is completed.  
  . WRITE "Listening to out from script in foreground task (NOTE: this job: ",$J,")",!
  . ;
  DO
  . ;"Setup TAIL of status file to monitor progress
  . NEW STATPIPE SET STATPIPE="STATPIPEHandle"
  . OPEN STATPIPE:(SHELL="/bin/sh":COMMAND="/bin/sh":stderr="errIO")::"pipe"   ;"NOTE: errIO stream is ignored
  . USE STATPIPE WRITE "tail -f "_STATFNAME USE TMGIO ;"//send command to PIPE  <-- tail should now start monitoring file.
  . NEW LOGPIPE SET LOGPIPE="LOGPIPEHandle"
  . ;"Optionally setup TAIL of debug_run_log file to monitor progress
  . IF SHOWDBLOG DO
  . . OPEN LOGPIPE:(SHELL="/bin/sh":COMMAND="/bin/sh":stderr="err2IO")::"pipe" ;"NOTE: err2IO stream is ignored
  . . USE LOGPIPE WRITE "tail -f "_RUNLOGFNAME  USE TMGIO ;"//send command to PIPE  <-- tail should now start monitoring file.  
  . NEW LINE,GOODREAD,DONE SET DONE=0
  . FOR  DO  QUIT:DONE
  . . USE STATPIPE READ LINE:0 SET GOODREAD=$TEST USE TMGIO
  . . IF GOODREAD DO
  . . . IF STATFN]"" DO  QUIT         
  . . . . XECUTE "DO "_STATFN_"(LINE)"    ;"<-- SEND LINE TO CALLBACK FUNCTION IF PROVIDED 
  . . . USE TMGIO
  . . . IF SHOWDBLOG WRITE "STATUS: "   ;"<--something to differentiate between STATUS and RUN_LOG 
  . . . WRITE LINE,!  ;"DEFAULT TO CONSOLE OUTPUT.  
  . . . ;"USE STATPIPE
  . . IF SHOWDBLOG DO
  . . . USE LOGPIPE READ LINE:0 SET GOODREAD=$TEST USE TMGIO
  . . . IF GOODREAD DO
  . . . . IF DBGLOGFN]"" DO  QUIT    
  . . . . . XECUTE "DO "_DBGLOGFN_"(LINE)"  ;"<-- SEND LINE TO CALLBACK FUNCTION IF PROVIDED  
  . . . . USE TMGIO
  . . . . WRITE "RUN_LOG: ",LINE,!  ;"DEFAULT TO CONSOLE OUTPUT.  
  . . . . ;"USE STATPIPE
  . . NEW JOBSTATUS SET JOBSTATUS=$GET(@MSGREF@("RESULT"))
  . . IF +JOBSTATUS=0 QUIT  ;"still '0^WORKING'  <-- REPEAT LOOP
  . . USE STATPIPE WRITE $CHAR(3) USE TMGIO             ;" ^C  or end-of-text (EOT).  Should cause tail to quit
  . . IF SHOWDBLOG USE LOGPIPE WRITE $CHAR(3) USE TMGIO ;" ^C  or end-of-text (EOT).  Should cause tail to quit
  . . SET DONE=1
  . CLOSE STATPIPE
  . IF SHOWDBLOG CLOSE LOGPIPE
  ;
  USE TMGIO
  ;"Transfer and clear messages (if any) from RUNSCRIPT background task.   
  MERGE MSG("STD OUT")=@MSGREF@("STD OUT")
  MERGE MSG("ER OUT")=@MSGREF@("ERR OUT")
  SET RESULT=@MSGREF@("RESULT") 
  KILL @MSGREF
  DO LOADNCLEAR(.OUT,.PATLIST,HFSPATH,OUTPATH,PTLSTFNAME) ;  
BMDN2 ;
  DO RESTORDEV^TMGKERN1(.DEVSAVE,.DEVSETUP)
  QUIT RESULT
  ;
RUNSCRIPT(PTLSTFNAME,MSGREF,DEBUG)  ;"Launch varadigm.js script via node.js
  ;"NOTE: This function may be run in either foreground or background process.  
  NEW TMGIO SET TMGIO=$IO
  SET @MSGREF@("RESULT")="0^WORKING" 
  NEW CRED SET TEMP=$$GETCRDNTL(.CRED,"Veradigm")
  IF TEMP'>0 DO  GOTO RSDN
  . SET @MSGREF@("RESULT")="-1^Error getting Veridigm credentials"
  NEW HFSPATH SET HFSPATH=CRED("HFS PATH")
  IF $EXTRACT(HFSPATH,$LENGTH(HFSPATH))'="/" SET HFSPATH=HFSPATH_"/"
  NEW OUTPATH SET OUTPATH=HFSPATH_"output/"
  ;"NEW JSCRIPT SET JSCRIPT="veradigm.js"
  NEW JSCRIPT SET JSCRIPT="veradigm_simple_debug.js"  ;"<--- for debugging only...
  ;      
  NEW CMDSTR SET CMDSTR=""
  SET CMDSTR=CMDSTR_"cd "_HFSPATH_" && "
  SET CMDSTR=CMDSTR_"node "_JSCRIPT_" "
  SET CMDSTR=CMDSTR_"--headless "
  SET CMDSTR=CMDSTR_"--ptlist="_OUTPATH_PTLSTFNAME_" "
  SET CMDSTR=CMDSTR_"--username="_CRED("USER")_" "
  SET CMDSTR=CMDSTR_"--password="_CRED("PASSWORD")_" "
  SET CMDSTR=CMDSTR_"--debug="_+$GET(DEBUG)_" "
  ;   
  ;"NOTE: this technique from here: https://gitlab.com/YottaDB/DB/YDB/-/blame/master/sr_unix/ydbenv.mpt#L398 from Bhaskar
  ;"      It allows sending commands to shell with long length
  NEW SHELL SET SHELL="ShellHandle"
  OPEN SHELL:(SHELL="/bin/sh":COMMAND="/bin/sh":stderr="errIO")::"pipe" 
  ;"//send command to shell
  USE SHELL
  WRITE CMDSTR,!
  WRITE /EOF  ;"flush buffer and shut off WRITE channel to shell.  
  ;
  ;"//Get any output (std out channel)
  NEW PIPEOUT,ERROUT,LINE,IDX 
  SET IDX=1
  FOR  DO  QUIT:$ZEOF  
  . READ LINE IF LINE=""&$ZEOF QUIT  
  . SET @MSGREF@("STD OUT",IDX)=LINE,IDX=IDX+1
  ;
  ;"//Get any error output (err out channel)
  SET IDX=1
  USE "errIO"
  FOR  DO  QUIT:$ZEOF  
  . READ LINE IF LINE=""&$ZEOF QUIT  
  . SET @MSGREF@("ERR OUT",IDX)=LINE,IDX=IDX+1
  CLOSE "errIO",SHELL
  USE TMGIO
  SET @MSGREF@("RESULT")="1^DONE" 
RSDN ;  
  QUIT  ;"<-- if run as background task, this will end the process
  ;
LOADNCLEAR(OUT,PATLIST,HFSPATH,OUTPATH,PTLSTFNAME) ;  
  ;"Now get back results from HFS
  KILL OUT
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(PATLIST(IDX)) QUIT:IDX'>0  DO
  . NEW LINE SET LINE=$GET(PATLIST(IDX)) QUIT:LINE=""
  . SET OUT(IDX)=LINE
  . NEW LNAME SET LNAME=$PIECE(LINE,"^",1)
  . NEW FNAME SET FNAME=$PIECE(LINE,"^",2)
  . NEW DOB SET DOB=$PIECE(LINE,"^",3)
  . NEW MONTH SET MONTH=$$RJ^XLFSTR($PIECE(DOB,"/",1),"0",2)
  . NEW DAY SET DAY=$$RJ^XLFSTR($PIECE(DOB,"/",2),"0",2)
  . NEW YR SET YR=$PIECE(DOB,"/",3)
  . NEW FILENAME SET FILENAME=LNAME_","_FNAME_","_MONTH_"_"_DAY_"_"_YR_".csv"
  . SET FILENAME=$TRANSLATE(FILENAME," ","_")
  . NEW DATA,OPTION SET OPTION("OVERFLOW")=1
  . NEW TEMP SET TEMP=$$HFS2ARR^TMGIOUT3(OUTPATH,FILENAME,"DATA",.OPTION)
  . IF +TEMP=0 SET OUT(IDX)="Error processing: "_FILENAME QUIT
  . NEW JDX SET JDX=0
  . FOR  SET JDX=$ORDER(DATA(JDX)) QUIT:JDX'>0  DO
  . . NEW ONERX SET ONERX=$GET(DATA(JDX)) QUIT:ONERX=""   
  . . NEW FMT SET FMT="DATE^DX^SOURCE^PHARMACY^MED"
  . . IF ONERX=FMT QUIT
  . . SET ONERX=$$STRIPSTR^TMGSTUT2(ONERX,"  ")  ;"remove all double whitespace  
  . . NEW RXARR DO PARSERX(ONERX,.RXARR)  ;"PARSE ONE RX ENTRY INTO ARRAY
  . . MERGE OUT(IDX,JDX)=RXARR
  . SET TEMP=$$DELFILE^TMGIOUTL(OUTPATH_FILENAME)  ;" returns 1 if success, 0 if failure -- BUT If deletion is blocked by OS, then 1 may be returns but file is not deleted.
  ;
  ;"Read in debug_run_log.txt and then delete it
  NEW LOGFNAME SET LOGFNAME="debug_run_log.txt"
  IF $$HFS2ARR^TMGIOUT3(HFSPATH,LOGFNAME,$NAME(MSG("LOG")))      ;"ignore result
  IF $$DELFILE^TMGIOUTL(HFSPATH_LOGFNAME)                        ;"ignore result
  ;
  ;"Read in status.text, and then delete it
  NEW STATFNAME SET STATFNAME="status.txt"
  IF $$HFS2ARR^TMGIOUT3(HFSPATH,STATFNAME,$NAME(MSG("STATUS")))  ;"ignore result
  IF $$DELFILE^TMGIOUTL(HFSPATH_STATFNAME)                       ;"ignore result
  ; 
  ;"Delete patient list
  SET TEMP=$$DELFILE^TMGIOUTL(OUTPATH_PTLSTFNAME)
  QUIT
  ;  
PARSERX(ONERX,OUT)  ;"PARSE ONE RX ENTRY INTO ARRAY
  ;"Input: ONERX -- one line as returned by puppeteer script.  E.g. 'ALBUTEROL HFA INH (200 PUFFS) 8.5GMSig: INHALE 1 TO 2 PUFFS BY MOUTH EVERY 6 HOURS AS NEEDED FOR WHEEZING QUANTITY 8- Days 25'
  ;"       OUT -- PASS BY FORMAT, AN OUT PARAMETER.  Format:  
  ;"             OUT("DATE")="3/3/2024"
  ;"             OUT("DX")="Acute upper respiratory infection, unspecified"
  ;"             OUT("SOURCE")="Pharmacy Reported Rx History"
  ;"             OUT("PHARMACY")="Walgreens"
  ;"             OUT("MEDICATION")="ALBUTEROL HFA INH (200 PUFFS) 8.5GM"
  ;"             OUT("MED DETAILS",1)="SIG=INHALE 1 TO 2 PUFFS BY MOUTH EVERY 6 HOURS AS NEEDED FOR WHEEZING"
  ;"             OUT("MED DETAILS",2)="QUANT=8"
  ;"             OUT("MED DETAILS",3)="DAYS=25"
  ;"             OUT("MED DETAILS",3)="NAME="                  <-- NOTE: not every entry will have all these key=value's
  ;"             OUT("MED DETAILS",3)="STRENGTH="
  ;"             OUT("MED DETAILS",3)="FORM="
  ;"             OUT("MED DETAILS",3)="DOSENUM="
  ;"             OUT("MED DETAILS",3)="DISP="
  ;"             OUT("MED DETAILS",3)="DOC="
  ;"             OUT("MED DETAILS",3)="PHARM="
  ;"             OUT("MED DETAILS",3)="REFILL="
  ;"             OUT("ZZRAW")="ALBUTEROL HFA INH (200 PUFFS) 8.5GMSig: INHALE 1 TO 2 PUFFS BY MOUTH EVERY 6 HOURS AS NEEDED FOR WHEEZING QUANTITY 8- Days 25"
  NEW ZZDEBUG SET ZZDEBUG=0   
  IF ZZDEBUG=1 DO
  . SET ONERX=$GET(^TMG("TMP","PARSERX^TMGKERN9"))
  ELSE  DO
  . SET ^TMG("TMP","PARSERX^TMGKERN9")=ONERX
  NEW TEMP,FMT SET FMT="DATE^DX^SOURCE^PHARMACY^MED"
  NEW IDX FOR IDX=1:1:5 DO
  . NEW KEY SET KEY=$PIECE(FMT,"^",IDX)
  . NEW VAL SET VAL=$PIECE(ONERX,"^",IDX)
  . IF KEY=VAL QUIT  ;"must be header line
  . IF KEY="MED" DO   ;"EXAMPLE: Medication: DULOXETINE 30MG DR CAP 30.000 Sig: QUANTITY 180 - Days 90 (more details...)
  . . SET VAL=$$REPLSTR^TMGSTUT3(VAL,"(more details...)","")
  . . SET VAL=$$REPLSTR^TMGSTUT3(VAL,"Medication: ","")
  . . SET OUT("ZZRAW")=VAL
  . . NEW QUANT DO SPLITB4^TMGSTUT2(.VAL,"QUANTITY",.QUANT)
  . . IF QUANT["-" DO
  . . . NEW ARR DO SPLIT2AR^TMGSTUT2(QUANT,"-",.ARR) KILL ARR("MAXNODE")
  . . . NEW JDX SET JDX=0
  . . . FOR  SET JDX=$ORDER(ARR(JDX)) QUIT:JDX'>0  DO
  . . . . NEW APART SET APART=$$TRIM^XLFSTR($GET(ARR(JDX))) QUIT:APART=""
  . . . . IF APART["QUANTITY" SET TEMP("QUANT")=$$TRIM^XLFSTR($PIECE(APART,"QUANTITY",2)) QUIT
  . . . . IF APART["DISPENSE" SET TEMP("DISP")=$$TRIM^XLFSTR($PIECE(APART,"DISPENSE",2)) QUIT
  . . . . IF APART["REFILL" SET TEMP("REFILL")=$$TRIM^XLFSTR($PIECE(APART,"REFILL",2)) QUIT
  . . . . IF APART["Days" DO  QUIT
  . . . . . IF APART["Doctor" DO
  . . . . . . NEW DOC DO SPLITB4^TMGSTUT2(.APART,"Doctor",.DOC)
  . . . . . . SET TEMP("DOC")=$$TRIM^XLFSTR(DOC) QUIT
  . . . . . ELSE  IF $LENGTH(APART," ")>2 DO
  . . . . . . NEW DOC SET DOC=$PIECE(APART," ",3,999)
  . . . . . . SET TEMP("DOC")=$$TRIM^XLFSTR(DOC)
  . . . . . . SET APART=$PIECE(APART," ",1,2)
  . . . . . SET TEMP("DAYS")=$$TRIM^XLFSTR($PIECE(APART,"Days",2)) QUIT
  . . . . ELSE  SET TEMP("PHARM")=APART QUIT
  . . ELSE  SET TEMP("QUANT")=QUANT  
  . . NEW SIG DO SPLITB4^TMGSTUT2(.VAL,"Sig:",.SIG)
  . . IF SIG["Sig:" SET SIG=$PIECE(SIG,"Sig:",2)
  . . SET TEMP("SIG")=$$TRIM^XLFSTR(SIG)
  . . SET VAL=$$TRIM^XLFSTR(VAL)
  . . SET OUT("MEDICATION")=VAL
  . . NEW PARENARR IF VAL["(" SET VAL=$$REMOVENCAP^TMGSTUT2(VAL,.PARENARR,"(")
  . . NEW ARR DO SPLIT2AR^TMGSTUT2(VAL," ",.ARR)
  . . NEW ARRIDX SET ARRIDX=+$GET(ARR("MAXNODE")) KILL ARR("MAXNODE")
  . . NEW DOSENUM SET DOSENUM=$GET(ARR(ARRIDX))
  . . IF $$ISNUM^TMGSTUT3(DOSENUM),DOSENUM["." DO
  . . . SET TEMP("DOSENUM")=DOSENUM
  . . . KILL ARR(ARRIDX)
  . . . SET ARRIDX=ARRIDX-1
  . . NEW FORM SET FORM=$GET(ARR(ARRIDX))
  . . IF "TAB,CAP,Capsule,Tablet"[FORM DO
  . . . SET TEMP("FORM")=FORM
  . . . KILL ARR(ARRIDX)
  . . SET ARRIDX=ARRIDX-1
  . . ;"NEW STRENGTH SET STRENGTH=$GET(ARR(ARRIDX)),ARRIDX=ARRIDX-1
  . . ;"SET TEMP("STRENGTH")=STRENGTH
  . . NEW MEDNAME SET MEDNAME=$$TRIM^XLFSTR($$ARR2STR^TMGSTUT2(.ARR," "))
  . . IF $DATA(PARENARR) SET MEDNAME=$$RESTORENCAPS^TMGSTUT2(MEDNAME,.PARENARR)
  . . SET OUT("MEDICATION")=MEDNAME
  . ELSE  DO
  . . SET OUT(KEY)=VAL
  SET IDX=1
  NEW LABEL
  FOR LABEL="NAME","STRENGTH","FORM","DOSENUM","SIG","QUANT","DISP","DAYS","DOC","PHARM","REFILL" DO
  . IF $DATA(TEMP(LABEL))=0 QUIT
  . SET OUT("MED DETAILS",IDX)=LABEL_"="_TEMP(LABEL),IDX=IDX+1
  QUIT
  ;
GETCRDNTL(OUT,NAME) ;"GET CREDENTIALS
  ;"Purpose: get SMS credentials from file 22724.1 (TMG CREDENTIALS)
  ;"Input: OUT -- PASS BY REFERENCE.  AN OUT PARAMETER.  Format:
  ;"         OUT("PASSWORD")=password value
  ;"         OUT("USER")=user value
  ;"         OUT("HFS PATH")=path
  ;"       NAME -- the value of the .01 field record to use.  
  ;"Result: 1 if OK, or -1^Message if error. 
  KILL OUT                                            
  SET NAME=$GET(NAME,"??")
  NEW RESULT SET RESULT="1^OK"
  NEW IEN SET IEN=+$ORDER(^TMG(22724.1,"B",NAME,0))
  NEW ZN SET ZN=$GET(^TMG(22724.1,IEN,0))
  SET OUT("PASSWORD")=$PIECE(ZN,"^",3)
  SET OUT("USER")=$PIECE(ZN,"^",5)
  NEW N5 SET N5=$GET(^TMG(22724.1,IEN,5))
  SET OUT("HFS PATH")=$PIECE(N5,"^",1)
  IF ZN="" DO  GOTO GCRDN
  . SET RESULT="-1^Record not found in 22724.1 for "_NAME 
  NEW IDX,NODE SET NODES="PASSWORD,USER,HFS PATH"
  FOR IDX=1:1:$LENGTH(NODES,",") DO  QUIT:RESULT'>0
  . NEW ANODE SET ANODE=$PIECE(NODES,",",IDX) QUIT:ANODE=""
  . IF $GET(OUT(ANODE))'="" QUIT  
  . SET RESULT="-1^"_ANODE_" not found in 22724.1 for IEN="_IEN 
GCRDN ;  
  QUIT RESULT
  ;
FILERXS(SRCREF)  ;"  
  ;"File: TMG MEDICATION ESCRIBE RECORDS                                 Branch: 1
  ;"REF  NODE;PIECE     FLD NUM  FIELD NAME
  ;"===============================================================================
  ;"  1  0;1                .01  NAME                               <-Pntr  [RP2']
  ;"     1;0                  1  ITEMS                          <-Mult [22733.41D]
  ;"  2   -0;1              .01   -DATE OF SCRIPT                              [D]
  ;"  3   -0;2              .02   -NAME OF MEDICATION                      [FJ160]
  ;"  4   -0;3              .03   -FORM                                     [FJ16]
  ;"  5   -0;4              .04   -STRENGTH                                 [FJ16]
  ;"  6   -0;5              .05   -QUANTITY                                  [FJ8]
  ;"  7   -0;6              .06   -DAYS                                      [FJ8]
  ;"  8   -0;7              .07   -REFILLS                                   [FJ8]
  ;"  9   -0;8              .08   -DISP                                      [FJ8]
  ;" 10   -0;9              .09   -DOSENUM                                   [FJ8]
  ;" 11   -1;1               .1   -DOC                                      [FJ16]
  ;" 12   -1;2              .11   -PHARM                                    [FJ16]
  ;" 13   -1;3              .12   -NAME                                     [FJ16]
  ;" 14   -2;1                1   -SIG                                     [FJ250]
  ;" 15   -3;1                2   -PHARMACY                                [FJ250]
  ;" 16   -4;1                3   -SOURCE                                  [FJ250]
  ;" 17   -5;1                4   -DX                                      [FJ250]
  ;"      -10;0              10   -RAW SCRIPT                      <-WP [22733.42]
  ;" 18    --0;1            .01    --RAW SCRIPT                               [Wx]
  ;" <> <> <>
  QUIT
  ;



  
  
  
BATCHMEDS0(OUT,PATLIST,MSG,OPTION) ;"DEPRECIATED
  ;"Purpose: call out to node.js / puppeteer and scrape Veradigm for meds for list of patients
  ;"INPUT: OUT -- PASS BY REFERENCE.  AN OUT PARAMETER.  FORMAT:
  ;"          OUT(IDX)=<PATIENT STRING, AS FOUND IN PATLIST>
  ;"          OUT(IDX)='Error processing: '_FILENAME   <--- if error loading patient result file.  
  ;"          OUT(IDX,#)=<RX ARRAY>  -- see format in PARSERX().  # is index, 1 for each med in patient med list. 
  ;"       PATLIST -- PASS BY REFERENCE.  FORMAT:
  ;"          PATLIST(IDX)='LNAME^FNAME^DOB^SDT^EDT^DFN'   
  ;"                note: IDX must be sequential numbers, e.g. 1,2,3... due to limitations in GTF^%ZISH. 1,3,5,7, or DFN's would NOT work
  ;"                      LNAME -- patient last name
  ;"                      FNAME -- patient first name.  NOTE: only first part used.  E.g. JOHN BOY --> only JOHN used
  ;"                      DOB -- patient DOB
  ;"                      SDT -- optional start of search date range.  
  ;"                      EDT -- optional end of search date range.  
  ;"                               Date format is mm/dd/yyyy (e.g. 5/12/2024)
  ;"                      DFN -- recommended to allow tying results back to patient
  ;"       MSG -- OPTIONAL.  PASS BY REFERENCE.  AN OUT PARAMETER.  FORMAT" 
  ;"           MSG("STD OUT",#) -- lines of output (if any), output by node to std out at command-line
  ;"           MSG("ERR OUT",#) -- lines of output (if any), output by node to error channel at command-line
  ;"           MSG("LOG",#)     -- lines of output (if any), debug_run_log.txt
  ;"           MSG("STATUS",#) -- lines of output (if any), status.txt file
  ;"       OPTION -- OPTIONAL -- PASS BY REFERENCE.  
  ;"           OPTION("DEBUG")=# -- default is 0.  Used to set debug run level for veridigm.js  
  ;"Result: 1^OK or -1^ErrorMessage
  NEW TMGIO SET TMGIO=$IO
  NEW TEMP,DEVSETUP,DEVSAVE DO DEV2ARR^TMGKERN1($IO,.DEVSAVE,,.DEVSETUP)
  NEW RESULT SET RESULT="1^OK"  ;"DEFAULT
  NEW CRED SET TEMP=$$GETCRDNTL(.CRED,"Veradigm")
  IF TEMP'>0 DO  GOTO BMDN
  . SET RESULT="-1^Error getting Veridigm credentials"
  NEW HFSPATH SET HFSPATH=CRED("HFS PATH")
  IF $EXTRACT(HFSPATH,$LENGTH(HFSPATH))'="/" SET HFSPATH=HFSPATH_"/"
  NEW OUTPATH SET OUTPATH=HFSPATH_"output/"
  NEW PTLSTFNAME SET PTLSTFNAME=$$UNIQUE^%ZISUTL("patients.csv")
  NEW ZZDEBUG SET ZZDEBUG=0
  IF ZZDEBUG=1 GOTO B2
  SET TEMP=$$ARR2HFS^TMGIOUT3("PATLIST",OUTPATH,PTLSTFNAME)   ;"RETURNS: 0 IF failure, 1 IF success
  IF TEMP'>0 DO  GOTO BMDN
  . SET RESULT="-1^Error saving "_HFSPATH_PTLSTFNAME
  ;      
  NEW CMDSTR SET CMDSTR=""
  SET CMDSTR=CMDSTR_"cd "_HFSPATH_" && "
  SET CMDSTR=CMDSTR_"node "_"veradigm.js "
  SET CMDSTR=CMDSTR_"--headless "
  SET CMDSTR=CMDSTR_"--ptlist="_OUTPATH_PTLSTFNAME_" "
  SET CMDSTR=CMDSTR_"--username="_CRED("USER")_" "
  SET CMDSTR=CMDSTR_"--password="_CRED("PASSWORD")_" "
  SET CMDSTR=CMDSTR_"--debug="_+$GET(OPTION("DEBUG"))_" "
  ;   
  ;"NOTE: this technique from here: https://gitlab.com/YottaDB/DB/YDB/-/blame/master/sr_unix/ydbenv.mpt#L398 from Bhaskar
  NEW SHELL SET SHELL="ShellHandle"
  OPEN SHELL:(SHELL="/bin/sh":COMMAND="/bin/sh":stderr="errIO")::"pipe" 
  ;"//send command to shell
  USE SHELL
  WRITE CMDSTR,!
  WRITE /EOF  ;"flush buffer and shut off WRITE channel to shell.  
  ;
  ;"//Get any output (std out channel)
  NEW PIPEOUT,ERROUT,LINE,IDX 
  SET IDX=1
  FOR  DO  QUIT:$ZEOF  
  . READ LINE IF LINE=""&$ZEOF QUIT  
  . SET MSG("STD OUT",IDX)=LINE,IDX=IDX+1
  ;
  ;"//Get any error output (err out channel)
  SET IDX=1
  USE "errIO"
  FOR  DO  QUIT:$ZEOF  
  . READ LINE IF LINE=""&$ZEOF QUIT  
  . SET MSG("ERR OUT",IDX)=LINE,IDX=IDX+1
  CLOSE "errIO",SHELL
  USE TMGIO
  ;
B2 ;"Now get back results from HFS
  KILL OUT
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(PATLIST(IDX)) QUIT:IDX'>0  DO
  . NEW LINE SET LINE=$GET(PATLIST(IDX)) QUIT:LINE=""
  . SET OUT(IDX)=LINE
  . NEW LNAME SET LNAME=$PIECE(LINE,"^",1)
  . NEW FNAME SET FNAME=$PIECE(LINE,"^",2)
  . NEW DOB SET DOB=$PIECE(LINE,"^",3)
  . NEW MONTH SET MONTH=$$RJ^XLFSTR($PIECE(DOB,"/",1),"0",2)
  . NEW DAY SET DAY=$$RJ^XLFSTR($PIECE(DOB,"/",2),"0",2)
  . NEW YR SET YR=$PIECE(DOB,"/",3)
  . NEW FILENAME SET FILENAME=LNAME_","_FNAME_","_MONTH_"_"_DAY_"_"_YR_".csv"
  . SET FILENAME=$TRANSLATE(FILENAME," ","_")
  . NEW DATA,OPTION SET OPTION("OVERFLOW")=1
  . NEW TEMP SET TEMP=$$HFS2ARR^TMGIOUT3(OUTPATH,FILENAME,"DATA",.OPTION)
  . IF +TEMP=0 SET OUT(IDX)="Error processing: "_FILENAME QUIT
  . NEW JDX SET JDX=0
  . FOR  SET JDX=$ORDER(DATA(JDX)) QUIT:JDX'>0  DO
  . . NEW ONERX SET ONERX=$GET(DATA(JDX)) QUIT:ONERX=""   
  . . NEW FMT SET FMT="DATE^DX^SOURCE^PHARMACY^MED"
  . . IF ONERX=FMT QUIT
  . . SET ONERX=$$STRIPSTR^TMGSTUT2(ONERX,"  ")  ;"remove all double whitespace  
  . . NEW RXARR DO PARSERX(ONERX,.RXARR)  ;"PARSE ONE RX ENTRY INTO ARRAY
  . . MERGE OUT(IDX,JDX)=RXARR
  . SET TEMP=$$DELFILE^TMGIOUTL(OUTPATH_FILENAME)  ;" returns 1 if success, 0 if failure -- BUT If deletion is blocked by OS, then 1 may be returns but file is not deleted.
  ;
  ;"Read in debug_run_log.txt and then delete it
  NEW LOGDATA,LOGFNAME SET LOGFNAME="debug_run_log.txt"
  NEW TEMP SET TEMP=$$HFS2ARR^TMGIOUT3(HFSPATH,LOGFNAME,"LOGDATA")
  MERGE MSG("LOG")=LOGDATA
  SET TEMP=$$DELFILE^TMGIOUTL(HFSPATH_LOGFNAME)
  ;
  ;"Read in status.text, and then delete it
  NEW STATDATA,STATFNAME SET STATFNAME="status.txt"
  NEW TEMP SET TEMP=$$HFS2ARR^TMGIOUT3(HFSPATH,STATFNAME,"STATDATA")
  MERGE MSG("STATUS")=STATDATA
  SET TEMP=$$DELFILE^TMGIOUTL(HFSPATH_STATFNAME)
  ;
  ;"Delete patient list
  SET TEMP=$$DELFILE^TMGIOUTL(OUTPATH_PTLSTFNAME)
  ;
BMDN ;
  DO RESTORDEV^TMGKERN1(.DEVSAVE,.DEVSETUP)
  QUIT RESULT
  ;
