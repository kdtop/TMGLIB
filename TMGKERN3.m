TMGKERN3        ;TMG/KST,JJIH/SMH - Terminal Emulation for CPRS Terminal; 7/26/11 3:01pm, 2/2/14
                ;;1.0;TMG-LIB;**1**;08/14/10;Build 1
        ;
        ;"TMG KERNEL FUNCTIONS -- 3
        ;"Primarily for running another instance of GTM.M inside
        ;"  a pipe, to allow for running session via RPC
        ;"
        ;"Kevin Toppenberg MD (minor mods by Sam Habiel PharmD)
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
        ;
        ; Summary of how this works (Sam needs this):
        ;  The CPRS proxy M process jobs off an M process at CTRLSGTM.
        ; CTRLSGTM starts another M process via a GT.M pipe. Then CTRLSGTM
        ; goes into an loop polling the other process for input and output.
        ;
        ;  Data is transferred between the jobbed process and the piped process
        ; through globals subscsripted by $J, to ensure that there will be
        ; no collision between different users.
        ;
        ;  CPRS uses an RPC to post data to the globals which CTRLSGTM polls.
        ; When it gets data in the globals, it posts that data to the piped
        ; process, then goes back to reading. If data comes back from the piped
        ; process, it is put into another global.
        ;
        ;  Every 1/2 second, CPRS polls the server for new data in the return
        ; global.
        ;
        ;"=======================================================================
        ;" Remote Procedures
        ;"=======================================================================
        ; TMG CONSOLE INITIATE            SVRINIT   TMGKERN3
        ; TMG CONSOLE SHUT DOWN           SVRHALT   TMGKERN3
        ; TMG CONSOLE SERVER POST         SVRPOST   TMGKERN3
        ; TMG CONSOLE SERVER QUERY        SVRQUERY  TMGKERN3
        ; TMG CONSOLE DISALLOW OPTION     DISMENU   TMGKERN3
        ;"=======================================================================
        ;" API -- Public Functions.
        ;"=======================================================================
        ;"SVRINIT: Initiates the SubGTM Process when CPRS starts. Jobs off
        ;  CTRLGTM.
        ;"SVRHALT: Asks Jobbed Process to Terminate using #BYE# and explicitly
        ;  mupip stops the piped process. Occurs when you close CPRS.
        ;"SVRPOST: Posts data to the piped process.
        ;"SVRQUERY: Queries data from the piped process.
        ;"DISMENU: Disables a menu option indicating it can't be used with
        ;  TMG CONSOLE.
        ;"RUN: Simulate CPRS running this console; very useful debugging tool!
        ;"=======================================================================
        ;" API -- Private Functions.
        ;"=======================================================================
        ;"CTRLSGTM(DUZ)  --Control Subprocess for GTM
        ;"
        ;"=======================================================================
        ;"Dependancies
        ;"=======================================================================
        ;"TMGKERNL
        ;"=======================================================================
        ;
DEBUGLOG(REF,MSG)        ; Private EP; Log Data in a private global.
        Q:'$$GET^XPAR("SYS","XWBDEBUG")
        NEW CT
        SET CT=$ORDER(@REF@(""),-1)+1
        SET @REF@(CT)="["_MSG_"]"
        QUIT
        ;
CTRLSGTM(TMGDUZ)         ; Private EP; Control Subprocess for GTM ; This is like the movie Inception!!!
        ;"Purpose: to launch a subprocess which runs GTM, with its IO
        ;"         redircted into this process.  The ultimate goal will
        ;"         be for process to be controlled remotely, via RPC link
        ;"Input: TMGDUZ -- PASS BY VALUE. The IEN of the current user
        ;"Output: ^TMG("TMP","SUBGTM",$J,"O",Line#) will store the output of the subprocess
        ;"        ^TMG("TMP","SUBGTM",$J,"O",Line#) will buffer the input to be sent to subprocss
        ;"NOTE: Special commands supported:
        ;"       #BYE# -- causes the process to disconnect and QUIT the subprocess.
        ;"       #OPTION#^1234 -- Causes menu OPTION of IEN=1234 (for example) to be
        ;"                     launched, unless disallowed in file 227012.
        ;
        ;
        KILL ^TMG("TMP","SUBGTM",$J)
        ;
        DO SETUPSUB(TMGDUZ) ;Establish DUZ array for this process
        SET $ZINTERRUPT="I $$JOBEXAM^ZU($ZPOSITION)" ; Set-up ZINT for this process.
        D SETNM^%ZOSV("CtSubProc "_TMGDUZ)                        ; Set-up Process Name
        ;
        ; Start GT.M SubProcess
        NEW GTM SET GTM="subGTM"                ; Pipe Name
        NEW GTMOUT,GTMIN,TMGCMD,LINECT
        SET TMGCMD=$GET(^%ZOSF("TMG SUB GTM"))
        IF TMGCMD="" DO  GOTO CTRDN
        . SET LINECT=+$ORDER(^TMG("TMP","SUBGTM",$J,"FROMSUB","@",-1))
        . SET ^TMG("TMP","SUBGTM",$J,"FROMSUB",LINECT+1)="Error: ^%ZOSF(""TMG SUB GTM"") not SET up.  Please ask IT Administrator to run SETUP^TMGKERN3"
        OPEN GTM:(COMMAND=TMGCMD:STREAM:NOWRAP)::"pipe"                ; Actual command to open
        USE GTM                                                                                                ; Read/Writes now go to pipe
        SET ^TMG("TMP","SUBGTM",$J,"SUB-$J")=$KEY                        ; $KEY is the PID/$J of the piped process.
        ;
        HANG .2 ; Wait until Process is Created
        ;
        NEW $ETRAP SET $ETRAP="CLOSE GTM DO ^%ZTER HALT"        ; Error Trap for This process
        ;
        ; Requisite stuff to make the process behave like a VISTA process...
        WRITE "DO SETUPSUB^TMGKERN3("_TMGDUZ_")",!  ;//smh - Setup variables INSIDE the pipe process
        WRITE "SET $ZINTERRUPT=""I $$JOBEXAM^ZU($ZPOSITION)""",! ;//smh mupip intrpt action
        WRITE "SET $ZPROMPT=""SUBGTM>""",!  ;"Set prompt  ;//smh - added !
        WRITE "S $ZT=""HALT:$EC["""",Z32,"""" D ^%ZTER HALT""",!  ;//smh default error trap; don't log IF broken pipe.
        WRITE "D SETNM^%ZOSV(""SubProc ""_DUZ)",!                        ; Set-up Process Name
        ;
        NEW LASTALIVE SET LASTALIVE=$H
        NEW DONE SET DONE=0
        FOR  DO  QUIT:$ZEOF!DONE
        . ;
        . NEW SECAGO SET SECAGO=$$HDIFF^XLFDT($H,LASTALIVE,2)
        . IF SECAGO>3600 SET DONE=1 QUIT  ;"Timeout after 1 hr of no keep-alive signal
        . ;
        . ; Read Loop from Subprocess. Reads and Reads into the "FROMSUB" node.
        . READ GTMOUT:0
        . IF (GTMOUT'="")&($ZA'=9) DO  QUIT  ;//smh --added QUIT--read all in a loop...
        . . IF GTMOUT="SUBGTM>" DO
        . . . SET ^TMG("TMP","SUBGTM",$J,"AT SUBGTM")=1
        . . . SET GTMOUT="#SUBGTM#"
        . . SET LINECT=+$ORDER(^TMG("TMP","SUBGTM",$J,"FROMSUB","@"),-1)+1
        . . SET ^TMG("TMP","SUBGTM",$J,"FROMSUB",LINECT)=GTMOUT  ;//smh - output global
        . . DO DEBUGLOG($name(^TMG("TMP","RPC","CTRLSGTM","GTMOUT")),GTMOUT) ;
        . ;
        . ; Read Loop from CPRS
        . HANG .1 ; Wait .1 seconds until CPRS gets us data.
        . ;
        . SET LINECT=+$ORDER(^TMG("TMP","SUBGTM",$J,"TOSUB",0))
        . SET GTMIN=$GET(^TMG("TMP","SUBGTM",$J,"TOSUB",LINECT))
        . IF GTMIN'="" DO DEBUGLOG($name(^TMG("TMP","RPC","CTRLSGTM","GTMIN")),GTMIN) ;
        . IF GTMIN'="" SET LASTALIVE=$H   ;"E.g. #ALIVE# would ensure still alive
        . IF GTMIN["#BYE#" SET DONE=1 QUIT
        . ELSE  IF GTMIN["#OPTION#" DO
        . . NEW IEN SET IEN=+$PIECE(GTMIN,"^",2) QUIT:(IEN'>0)
        . . DO DEBUGLOG($name(^TMG("TMP","RPC","CTRLSGTM","GTMIN")),"NOTE: IEN="_IEN)
        . . NEW DIS SET DIS=$$DISALLOW(IEN)
        . . DO DEBUGLOG($name(^TMG("TMP","RPC","CTRLSGTM","GTMIN")),"NOTE: $$DISALLOW="_DIS)
        . . NEW CANRUN SET CANRUN=($$ACCESS^XQCHK(DUZ,IEN)>0)
        . . DO DEBUGLOG($name(^TMG("TMP","RPC","CTRLSGTM","GTMIN")),"NOTE: $$CANRUN="_CANRUN)
        . . IF (DIS=1)!(CANRUN=0) QUIT  ;"Ignore invalid option requests
        . . ;"IF ($$DISALLOW^TMGRPC1C(IEN)=1)!($$CANRUN^TMGRPC1C(IEN)=0) DO  QUIT  ;"Ignore invalid option requests
        . . ;". SET LINECT=+$ORDER(^TMG("TMP","SUBGTM",$J,"FROMSUB","@"),-1)+1
        . . ;". SET ^TMG("TMP","SUBGTM",$J,"FROMSUB",LINECT)="#ERRMSG#^Option #"_IEN_" not allowed for user."
        . . NEW ZN SET ZN=$GET(^DIC(19,IEN,0)) QUIT:(ZN="")                ; Zero Node
        . . NEW TYPE SET TYPE=$PIECE(ZN,"^",4)
        . . DO DEBUGLOG($name(^TMG("TMP","RPC","CTRLSGTM","GTMIN")),"NOTE: TYPE=["_TYPE_"]")
        . . NEW ACTION SET ACTION=""
        . . IF (TYPE="A") SET ACTION=$GET(^DIC(19,IEN,20))
        . . IF (TYPE="R") DO
        . . . SET ACTION=$GET(^DIC(19,IEN,25)) QUIT:(ACTION="")
        . . . IF ACTION'["^" SET ACTION="^"_ACTION
        . . . SET ACTION="DO "_ACTION
        . . IF ACTION="" DO  QUIT
        . . . SET LINECT=+$ORDER(^TMG("TMP","SUBGTM",$J,"FROMSUB","@"),-1)+1
        . . . SET ^TMG("TMP","SUBGTM",$J,"FROMSUB",LINECT)="#ERROR#^Unable to find run code from OPTION #"_IEN
        . . DO DEBUGLOG($name(^TMG("TMP","RPC","CTRLSGTM","ACTION")),ACTION) ;
        . . WRITE ACTION,! ;//smh added !
        . . SET ^TMG("TMP","SUBGTM",$J,"AT SUBGTM")=0
        . ELSE  IF GTMIN["#CR#" WRITE !
        . ELSE  IF (+$GET(^TMG("TMP","SUBGTM",$J,"AT SUBGTM"))=0)&(GTMIN'="") DO
        . . WRITE GTMIN,!
        . KILL ^TMG("TMP","SUBGTM",$J,"TOSUB",LINECT)
        CLOSE GTM
        USE $P
        KILL ^TMG("TMP","SUBGTM",$J)
CTRDN        QUIT
        ;
        ;
SETUPSUB(TMGDUZ)        ;
        ;"Purpose: And entry point for subprocess to call to setup it's working environment.
        ;"Also used by CTRLSGTM process to SET up it's DUZ etc also.
        DO DT^DICRW
        DO DUZ^XUP(TMGDUZ)
        DO HOME^%ZIS  ; (Device 0 is configured to be P-OTHER80 right now; TMG can't DO ctrl chars).
        QUIT
        ;
        ;
MTRGTM(JOBNUM)         ;"Monitor subprocess controller -- DEBUGGING ONLY
        ;"
        NEW DONE SET DONE=0
        NEW FROMSUB,TOSUB
        FOR  DO  QUIT:DONE
        . NEW LINECT
        . FOR  DO  QUIT:(LINECT=0)    ;"Printout buffer.
        . . SET LINECT=+$ORDER(^TMG("TMP","SUBGTM",JOBNUM,"FROMSUB",0))
        . . IF LINECT=0 QUIT
        . . SET FROMSUB=$GET(^TMG("TMP","SUBGTM",JOBNUM,"FROMSUB",LINECT))
        . . KILL ^TMG("TMP","SUBGTM",JOBNUM,"FROMSUB",LINECT)
        . . WRITE FROMSUB,!
        . ;"Get user response
        . NEW USRINPUT READ USRINPUT:15 WRITE !
        . IF (USRINPUT="")!($TEST=0) QUIT
        . IF USRINPUT=$CHAR(27) DO   ;"ESC --> send #BYE# and QUIT
        . . SET USRINPUT="#BYE#"
        . IF USRINPUT="#BYE#" SET DONE=1
        . SET LINECT=+$GET(^TMG("TMP","SUBGTM",JOBNUM,"TOSUB",LINECT))+1
        . SET ^TMG("TMP","SUBGTM",JOBNUM,"TOSUB",LINECT)=USRINPUT
        . HANG 0.5  ;"//kt was 1
        QUIT
        ;
        ;
RUN        ;"Start it all running.   -- DEBUGGING ONLY
        JOB CTRLSGTM(DUZ):(OUTPUT="/dev/null":ERROR="/dev/null")
        DO MTRGTM($ZJOB)
        QUIT
        ;
SVRPOST(OUT,USRINPUT,JOBNUM)        ;
        ;"Purpose: RPC entry point to send user-submitted text to the GTM subprocess
        ;"Input:  OUT -- PASS BY REFERENCE.  An OUT PARAMETER.  See Output docs below
        ;"        USRINPUT -- The text to pass to the GTM subprocess
        ;"        JOBNUM -- OPTIONAL.  This is the job number of the subgtm controller.
        ;"NOTE: Special commands supported:
        ;"       #BYE# -- causes the process to disconnect and QUIT the subprocess.
        ;"       #ALIVE# -- A stay-alive signal to prevent GTM subprocess from timing out
        ;"                  Message is passed to CTRLSGTM and handled there
        ;"       #OPTION#^1234 -- Causes menu OPTION of IEN=1234 (for example) to be
        ;"                     launched, unless disallowed in file 227012.  This
        ;"                     will be checked at level of CTRLSGTM
        ;"Results: None
        ;"Output:  OUT(0)=1^Success   or -1^ErrorMessage
        ;
        ;"set ^TMG("TMP","RPC","SVRPOST")="["_$GET(USRINPUT)_"]"
        ;"do DEBUGLOG($name(^TMG("TMP","RPC","SVRPOST")),USRINPUT) ;
        ;
        KILL OUT
        IF '$DATA(JOBNUM) DO
        . SET JOBNUM=+$GET(^TMG("TMP","SUBGTM",$J,"CONTROLLER JOB"))
        IF $DATA(USRINPUT)#10=0 GOTO SPDN
        IF USRINPUT="" SET USRINPUT="#CR#"
        IF USRINPUT["#OPTION#" DO
        . SET ^TMG("TMP","SUBGTM",JOBNUM,"AT SUBGTM")=0
        . GOTO SP2
        IF USRINPUT["#BYE#" GOTO SP2
        IF $GET(^TMG("TMP","SUBGTM",JOBNUM,"AT SUBGTM"))=1 DO  GOTO SPDN
        . SET OUT(0)="-1^#NEED MENU#^Console not ready to accept text.  Select Menu option First."
SP2        SET OUT(0)="1^Success"
        NEW LINECT
        SET LINECT=+$GET(^TMG("TMP","SUBGTM",JOBNUM,"TOSUB","LINECT"))
        SET LINECT=LINECT+1
        SET ^TMG("TMP","SUBGTM",JOBNUM,"TOSUB",LINECT)=USRINPUT
SPDN        QUIT
        ;
SVRQUERY(OUT,JOBNUM)        ;
        ;"Purpose: RPC entry point to return any text output from the GTM subprocess
        ;"Input:  OUT -- PASS BY REFERENCE.  An OUT PARAMETER.  See Output docs below
        ;"         JOBNUM -- OPTIONAL -- PASS BY VALUE.  The $J of the subgtm controller
        ;"Results: None
        ;"Output:  OUT(0)=0^None  or 1^Success  or -1^ErrorMessage
        ;"         OUT(1)=TextFromServer
        ;"         OUT(2)=TextFromServer
        ;"         OUT(3)=TextFromServer
        ;"         ...
        ;"NOTE: Special message back from server implemented:
        ;"      (These messages are put out as text from Server)
        ;"      #SUBGTM#  -- This means that the subprocess is at a SUBGTM> prompt
        ;"                 and will now accept requests to launch a given OPTION.
        ;"                 This is specified by #OPTION# as documented in SVRPOST
        ;"     #ERROR#^Error Message
        NEW LINECT,FROMSUB,COUNT
        IF '$DATA(JOBNUM) DO
        . SET JOBNUM=+$GET(^TMG("TMP","SUBGTM",$J,"CONTROLLER JOB"))
        SET COUNT=1
        DO SVRPOST(,"#ALIVE#",JOBNUM) ;"Signal to keep subprocess alive.
        SET OUT(0)="0^None"
        FOR  DO  QUIT:(LINECT=0)    ;"Printout buffer.
        . SET LINECT=+$ORDER(^TMG("TMP","SUBGTM",JOBNUM,"FROMSUB",0))
        . IF LINECT=0 QUIT
        . SET FROMSUB=$GET(^TMG("TMP","SUBGTM",JOBNUM,"FROMSUB",LINECT))
        . KILL ^TMG("TMP","SUBGTM",JOBNUM,"FROMSUB",LINECT)
        . SET OUT(COUNT)=FROMSUB
        . SET COUNT=COUNT+1
        . SET OUT(0)="1^SUCCESS"
        ;
        QUIT
        ;
SVRINIT(OUT)        ;
        ;"Purpose: RPC entry point to turn on GTM subprocess
        ;"Input:  OUT -- PASS BY REFERENCE.  An OUT PARAMETER.  See Output docs below
        ;"Results: None
        ;"Output:  1^Success^SubGTMControllerJobNum  or -1^ErrorMessage
        ;"NOTE: the JOB number for the controller process will be stored at:
        ;"       ^TMG("TMP","SUBGTM",$J,"CONTROLLER JOB")=$ZJOB
        NEW RESULT,LASTLINE,DONE,COUNT
        DO CLEANJBS ;"clean up any dead jobs.
        SET OUT="1^Default Success"
        IF $GET(^%ZOSF("TMG SUB GTM"))="" DO  GOTO SIDN
        . SET OUT="-1^%ZOSF(""TMG SUB GTM"") not setup.  Please ask IT Administrator to DO SETUP^TMGKERN3."
        SET DUZ=+$GET(DUZ)
        DO
        . NEW $ETRAP SET $ETRAP="S OUT=""-1^""_$ZSTATUS_"" (""_$ECODE_"")"",$ETRAP="""",$ECODE="""""
        . JOB CTRLSGTM(DUZ):(OUTPUT="/dev/null":ERROR="/dev/null")
        IF +OUT'>0 GOTO SIDN
        SET ^TMG("TMP","SUBGTM",$J,"CONTROLLER JOB")=$ZJOB
        SET OUT="1^Success^"_$ZJOB
        SET COUNT=0,DONE=0
        HANG .5
        FOR  DO  QUIT:DONE=1
        . KILL RESULT
        . DO SVRQUERY(.RESULT,$ZJOB)
        . SET LASTLINE=$ORDER(RESULT("@"),-1)
        . IF $GET(RESULT(LASTLINE))["#SUBGTM#" DO  QUIT
        . . SET DONE=1
        . . SET ^TMG("TMP","SUBGTM",$ZJOB,"AT SUBGTM")=1
        . HANG .5
        . SET COUNT=COUNT+1
        . IF COUNT>50 DO
        . . SET DONE=1
        . . SET OUT="-1^TIMED OUT WAITING FOR #SUBGTM#^"_$ZJOB_"^"_RESULT(LASTLINE)
        IF +OUT=-1 GOTO SIDN
        ;===
        ; Next two lines commented out. //smh. Subcontroller initiates its own
        ; symbol table
        ; SET ^TMG("TMP","SUBGTM",$ZJOB,"AT SUBGTM")=0  ;"Force subcontroller to accept arbitrary code.
        ; DO SVRPOST(.RESULT,"DO SETUPSUB^TMGKERN3("_DUZ_")",$ZJOB)
        ; end //smh
        ;===
SIDN        QUIT
        ;
SVRHALT(OUT,JOBNUM)        ;
        ;"Purpose: To shut down the GTM subprocess
        ;"Input:  OUT -- PASS BY REFERENCE.  An OUT PARAMETER.  See Output docs below
        ;"        JOBNUM -- OPTIONAL.  This is the job number of the subgtm controller.
        ;"Results: None
        ;"Output:  1^Success  or -1^ErrorMessage
        ;"
        IF '$DATA(JOBNUM) DO
        . SET JOBNUM=+$GET(^TMG("TMP","SUBGTM",$J,"CONTROLLER JOB"))
        NEW LINECT,JOBS
        SET LINECT=+$GET(^TMG("TMP","SUBGTM",JOBNUM,"TOSUB","LINECT"))
        SET LINECT=LINECT+1
        SET ^TMG("TMP","SUBGTM",JOBNUM,"TOSUB",LINECT)="#BYE#"
        KILL ^TMG("TMP","SUBGTM",$J,"CONTROLLER JOB")
        HANG 1
        DO MJOBS^TMGKERNL(.JOBS)
        IF $DATA(JOBS(JOBNUM)) DO
        . DO KILLPID^TMGKERNL(JOBNUM)
        . HANG 1
        . KILL JOBS
        . DO MJOBS^TMGKERNL(.JOBS)
        . IF $D(JOBS(JOBNUM)) DO
        . . SET OUT(0)="-1^JOB "_JOBNUM_" COULD NOT BE TERMINATED."
        ELSE  SET OUT(0)="1^Success"
        QUIT
        ;
DISMENU(RESULT,OPTIONNUM)        
        ;"Purpose: To disable a menu option from the TMG-CPRS console.
        ;
        NEW TMGFDA,TMGIENS,TMGMSG,TMGIEN
        SET TMGIENS="+1,"
        SET OPTIONNUM=+$PIECE(OPTIONNUM,"^",2)
        IF OPTIONNUM'>0 DO  GOTO DMQUIT
        . SET RESULT="-1^NO OPTION NUMBER RECEIVED."
        SET TMGFDA(22712,TMGIENS,.01)=OPTIONNUM
        SET TMGFDA(22712,TMGIENS,.05)="N"
        DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO
        . SET RESULT="-1^Filing Error Occured: "_$GET(TMGMSG("DIERR",1,"TEXT",1))
        ELSE  DO
        . SET RESULT="1^SUCCESSFUL"
DMQUIT        QUIT
        ;
SETUP        ;"Purpose: To prepair the ^%ZOSF("TMG SUB GTM") node for use.
        ;"
        NEW TMGCMD,%,GTM,GTMOUT
SUL1           WRITE !,!
        WRITE "======================================================",!
        WRITE "=                                                    =",!
        WRITE "=  Setup ^%ZOSF(""TMG SUB GTM"") for GT.M Sub-Process  =",!
        WRITE "=                                                    =",!
        WRITE "======================================================",!
        WRITE !
        IF $GET(^%ZOSF("OS"))'["GT.M (Unix)" DO  QUIT
        . WRITE "This functionality is only available for GT.M (Unix) mumps.",!
        . WRITE "Aborting.",!,!
        WRITE "Please enter in the Linux/Unix command that would be entered",!
        WRITE "from a Linux command line prompt to launch GTM, leaving the user",!
        WRITE "at a mumps command prompt.  E.g. 'gtm'  or 'sh /home/john/runVistA'",!
        READ "Enter command (^ to abort): ",TMGCMD,!
        IF TMGCMD["^" WRITE "Aborting.",! QUIT
        WRITE !,"Test launching GT.M subprocess now, using Linux ",!
        WRITE "command: [",TMGCMD,"]"
        SET %=1 DO YN^DICN WRITE !
        SET GTM="subGTM"
        OPEN GTM:(COMMAND=TMGCMD:STREAM:NOWRAP)::"pipe"
        USE GTM
        NEW RESULT SET RESULT=-1
        ; WRITE "SET $ZPROMPT=""SUBGTM>"""  ;"Set prompt //smh old
        WRITE "SET $ZPROMPT=""SUBGTM>""",!  ;"Set prompt //smh new
        NEW DONE SET DONE=0
        IF +$DEVICE=1 DO
        . WRITE "Device error: ",$PIECE($DEVICE,",",2),!
        . SET DONE=1
        IF 'DONE FOR  DO  QUIT:$ZEOF!DONE
        . USE GTM READ GTMOUT:3
        . SET DONE=($TEST=0)
        . IF (GTMOUT'="")&($ZA'=9) do
        . . USE $P WRITE GTMOUT,!
        . . IF GTMOUT="SUBGTM>" SET DONE=1,RESULT=1
        CLOSE GTM
        USE $P
        SET %=2
        WRITE !,!,"Test run of GT.M subprocess "
        IF RESULT=1 DO
        . WRITE "SUCCESSFUL.  Ready for use.",!
        . SET ^%ZOSF("TMG SUB GTM")=TMGCMD
        ELSE  DO  IF %=1 GOTO SUL1
        . WRITE "FAILED.",!
        . WRITE "Try setting up again" DO YN^DICN WRITE !
        QUIT
        ;
CLEANJBS        ;
        ;"Purpose: to clean up nodes in ^TMG("TMP","SUBGTM",*) left by dead jobs.
        NEW JOBS
        DO MJOBS^TMGKERNL(.JOBS)
        IF $DATA(JOBS)=0 QUIT
        NEW J SET J=0
        FOR  SET J=$ORDER(^TMG("TMP","SUBGTM",J)) QUIT:(+J'>0)  DO
        . IF $DATA(JOBS(J)) QUIT  ;" job is active, so leave alone.
        . KILL ^TMG("TMP","SUBGTM",J)
        QUIT
DISALLOW(IEN19)        ;
        ;"Purpose: To determine IF menu option is on list of DISALLOWED menu options
        ;"         i.e. in file TMG CONSOLE SUBGTM OPTION SETTINGS (22712), option is not
        ;"         prohibited.
        ;"Input:  IEN19 -- IEN in file 19 (OPTION) file, to be checked.
        ;"Results: 1 IF NOT TO BE RUN. or 0 IF OK
        ;
        ; Note: Code in For loop is modified but untested! //smh
        NEW RESULT SET RESULT=0
        NEW IEN SET IEN=0
        FOR  SET IEN=$ORDER(^TMG(22712,"B",IEN19,IEN)) QUIT:(+IEN'>0)  DO
        . IF $PIECE($GET(^TMG(22712,IEN,3)),"^",1)'="Y" DO   ;"If no Y, then DON'T RUN
        . . NEW KEYIEN SET KEYIEN=+$PIECE($GET(^TMG(22712,IEN,0)),"^",2)
        . . IF KEYIEN>0 N KEYNAME S KEYNAME=$P(^DIC(19.1,KEYIEN,0),U)
        . . QUIT:$DATA(^XUSEC(KEYNAME,DUZ))   ;"If override key is present, then don't restrict
        . . SET RESULT=1
        QUIT RESULT
        ;
        ;
