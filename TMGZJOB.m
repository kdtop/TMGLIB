TMGZJOB  ;ISF/RWF - GT.M Job Exam ; 8/19/15 //kt
  ;;8.0;KERNEL;**349**;Jul 10, 1995;Build 29;WorldVistA 30-June-08
  ;";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;"NOTE: This code originally copied from ZJOB.m, with planned    ;
  ;"      extensive changes                                        ;
  ;"---------------------------------------------------------------;
  ;" Various edits between Wally, Dave Whitten, Bhaskar,           ;
  ;" Chris Richardson, K Toppenberg over a period of time.         ;
  ;";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;";
  ;"
  ;"Modified from FOIA VISTA,
  ;"Copyright 2008 WorldVistA.  Licensed under the terms of the GNU
  ;"General Public License See attached copy of the License.
  ;"
  ;"This program is free software; you can redistribute it and/or modify
  ;"it under the terms of the GNU General Public License as published by
  ;"the Free Software Foundation; either version 2 of the License, or
  ;"(at your option) any later version.
  ;"
  ;"This program is distributed in the hope that it will be useful,
  ;"but WITHOUT ANY WARRANTY; without even the implied warranty of
  ;"MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  ;"GNU General Public License for more details.
  ;"
  ;"You should have received a copy of the GNU General Public License along
  ;"with this program; if not, write to the Free Software Foundation, Inc.,
  ;"51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
  ;"
  GOTO CONSOLE
  ;
OLD ;  
  ;"PID is decimal job number.
  W !!,"Examine Another JOB Utility ",!
  W "---------------------------"
  I $ZV'["GT.M" W !,"This is for GT.M only" GOTO N
  N PID,HEXPID,ACTION
  N DTIME S DTIME=600
L S PID=$$RDJNUM
  G:PID="^" N
  G:PID'>0 L
  S ACTION="L"
M D DOIT(.ACTION) 
  G:ACTION="^" N
  S ACTION=$$ASKUSER()
  G:ACTION="^" L
  G M
N ; Single Exit
  W !,"Goodbye.",!
  Q
  ;
ASKUSER()   ; Ask for user input
  NEW ACTION
  R !,"Job Exam Action (? for help): L//",ACTION:DTIME
  S:'$T ACTION="^"
  S:ACTION="" ACTION="L"
  I ACTION["^" GOTO ASKDN
  ;Return first non-space, UPPER character
  S ACTION=$TR($E($TR(ACTION," ")),"klsv","KLSV")
ASKDN ;  
  Q ACTION
  ;
DOIT(ACTION)  ; Action Prompt
  I ACTION="K" D KILLPID(PID) S ACTION="S" ;FALLTHRU  ;"//kt 
  I ACTION="S" D ^TMGZSY  S ACTION="L" ;FALLTHRU
  I ACTION="L"!(ACTION="V") D DISPLAY(PID,ACTION) Q
  I ACTION="*" D  Q
  . W !,"Loading variables from process ",PID," into this process and then quitting."
  . D LOAD^ZJOB1
  . S ACTION="^" 
  ; All Else
  W:ACTION'="?" !,"Unknown Action received"
  ;ACTION["?"
  W !,"  Enter '^' to choose another JOB "
  W !,"  Enter 'L' to display status information about other Job"
  W !,"  Enter 'S' to display current System Status"
  W !,"  Enter 'V' to display local variables of other job"
  W !,"  Enter '*' to load other job's symbol table into current job and Q"
  W !,"  Enter 'K' to send a Kill Job command to other job"
  W !
  Q
  ;  
KILLPID(JN)  ;Send message to MUPIP to KILL Job  //kt added
  S JN=+$G(JN) Q:JN'>0
  W !,"Are you sure you want to KILL process #",JN,"? No// "
  N % R %:DTIME
  I "YES"'[$TR(%,"yes","YES") Q
  ZSYSTEM "mupip stop "_JN  ;"Launch command in linux OS
  Q
  ;  
RDJNUM()  ;
  N INP,PID
  S INP=""
  R !,"Enter JOB number (? for help): ",INP:DTIME S:'$T INP="^"
  S INP=$TR(INP,"hlsv","HLSV")
  I "^"[INP Q "^"  ;abend
  ;
  I INP["?" D  Q " "
  . W !,"  Enter 'S' to display current system status and job numbers"
  . W !,"  Enter '^' to exit"
  . W !,"  Enter <Job Number> to display information about another job"
  . ;"W !,"   enter JOB number in Hexidecimal or Decimal"
  . ;"W !,"   Enter Hexadecimal with a leading/trailing 'h' "
  . W !
  .Q
  I INP["S" D ^TMGZSY Q " " ;
  ;
  ; good hex or decimal number
  S PID=$TR(INP,"abcdefh","ABCDEFH")
  I $L($TR(PID,"0123456789ABCDEFH","")) D  Q " "
  . W !,"Invalid character in JOB number."
  .Q
  ;
  ;If in Hex, Convert PID to decimal
  I PID["H" S PID=$$DEC($TR(PID,"H")) ; ...and continue
  ; good job number but it is your own job.  Don't go there...
  I PID=$JOB D  Q " "
  . W !,"Can't EXAMINE your own process."
  .Q
  ;
  ; VA check to see if a GTM job exists.
  I $L($T(XUSCNT)),'$$CHECK^XUSCNT(PID) W !,"Not running thru VA kernel." ;decimal job
  ;
  ;W !,"JOB #",PID," does not exist"
  ;Q " " ; bad job number so re-ask
  Q PID
  ;
DISPLAY(JOB,ACTION)  ;
  ;"Display Job info, L is always the default.  No need to test for it.
  ;" The "L" header is part of the "V" Option
  NEW TEMP SET TEMP=$$GETVARS(JOB)  ;"Get reference to listing of variables from other job.  
  IF +TEMP=-1 DO  GOTO DISPDN
  . WRITE $PIECE(TEMP,"^",2),!
  NEW VREF SET VREF=TEMP
  DO DISPL ;"Show Header
  IF ACTION="V" D DISPV(VREF) ;"Show symbol table
DISPDN ;  
  Q
  ;
DISPL  ; ACTION="L" means single page info
  ; Show short job info
  ; Current Routine and Line Number  ;Current Line Executing
  D GETINFO
  S HEXJOB="" I $ZV["VMS" S HEXJOB=$$HEX(JOB)
  W !,"JOB #: "_JOB W:$L(HEXJOB) " ("_HEXJOB_")" W ?40,"Process Name: "_$G(^XUTL("XUSYS",JOB,"NM"))
  W !,"Device: "_$P($G(^XUTL("XUSYS",JOB,"JE","D",1))," ")
  W !,"Process State: "_PS W:$L(IMAGE) ?40,"IMAGE: "_IMAGE_" ("_INAME_")"
  W !,"JOB Type: "_JTYPE,?25,"CPU Time: "_CTIME,?50,"Login time: "_LTIME
  W !!,"Routine line: <"_$G(^XUTL("XUSYS",JOB,"INTERRUPT"))_">"
  W !,CODELINE
  Q
  ;
DISPV(VREF)  ;
  NEW C,I,S
  FOR S="Stack","Locks","Devices","Intrinsic Variables","Variables" DO
  . SET C=$EXTRACT(S,1),I=""
  . IF $D(^XUTL("XUSYS",JOB,"JE",C))=0 QUIT
  . WRITE !,"Section "_S,!
  . FOR  SET I=$O(^XUTL("XUSYS",JOB,"JE",C,I)) QUIT:I=""  DO
  . . WRITE $GET(^XUTL("XUSYS",JOB,"JE",C,I)),!
  Q
  ;  ==============
GETINFO  ; Identify the Target Process's state.
  ; Setup, process state > ps, Image name > iname, CPU time > ctime, Login time > ltime
  S (PS,INAME,CTIME,LTIME,JTYPE,IMAGE,CODELINE)=""
  S CODELINE=$G(^XUTL("XUSYS",JOB,"codeline"))
  I $zv["VMS" D VSTATE  Q
  ; Assume Unix as default
  D USTATE
  Q
  ;
VSTATE  ; VMS get Process state
  S TNAME=$ZGETJPI(JOB,"TERMINAL"),NM=$ZGETJPI(JOB,"prcnam")
  S JTYPE=$ZGETJPI(JOB,"jobtype"),PS=$ZGETJPI(JOB,"state")
  S LTIME=$$DATETIME($ZGETJPI(PID,"LOGINTIM")),CTIME=$$CPUTIME($ZGETJPI(JOB,"cputim"))
  Q
  ;
DATETIME(HOROLOG)  ;
  Q $ZDATE(HOROLOG,"DD-MON-YY 24:60:SS","Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec")
  ;
CPUTIME(S)  ; Calculate the VMS CPU time from first argument, S
  N T,SS,M,H,D
  S T=S#100,S=S\100 S:$L(T)=1 T="0"_T
  S SS=S#60,S=S\60 S:$L(SS)=1 SS="0"_SS
  S M=S#60,S=S\60 S:$L(M)=1 M="0"_M
  S H=S#24,D=S\24 S:$L(H)=1 H="0"_H
  Q D_" "_H_":"_M_":"_SS_"."_T
  ;
BLINDPID  ;
  N ZE S ZE=$ZS,$EC=""
  I ZE["NOPRIV" S NOPRIV=1
  Q
  ; MAY BE REDUNDANT OR WRONG
USTATE  ;UNIX Process state.
  N %FILE,%TEXT,U,%J,ZCMD,$ET,$ES
  S $ET="D UERR^ZJOB",STATE="",U="^"
  S %FILE="/tmp/_gtm_sy_"_$J_".tmp"
  ;S ZCMD="ps ef -C mumps >"_%FILE ;| grep "_JOB_">"_%FILE
  S ZCMD="ps eo pid,tty,stat,time,etime,cmd -C mumps >"_%FILE ;| grep "_JOB_">"_%FILE
  ;W !,ZCMD
  ZSYSTEM ZCMD
  O %FILE:(readonly)
  ; Get only line of text from temp file
  U %FILE
  F EXIT=0:0 R %TEXT Q:%TEXT=""  D  Q:EXIT
  . Q:+%TEXT'=JOB
  . S %TEXT=$$VPE(%TEXT," ",U) ; parse each line of the ps output
  . S TNAME=$P(%TEXT,U,2),PS=$P(%TEXT,U,3),CTIME=$P(%TEXT,U,4),LTIME=$P(%TEXT,U,5),JTYPE=$P(%TEXT,U,7)
  . S EXIT=1
  .Q
  ;
  U $P C %FILE:DELETE
  S PS=$S(PS="S":"hib",PS="D":"lef",PS="R":"run",1:PS)
  Q
  ;  ================
UERR  ;Error
  S $EC=""
  U $P W !,"Error: "_$ZS
  Q:$Q -9
  Q
  ;
HEX(D)  ;Decimal to Hex
  Q $$FUNC^%DH(D,8)
DEC(H)  ;Hex to Decimal
  Q $$FUNC^%HD(H)
  ;
VPE(%OLDSTR,%OLDDEL,%NEWDEL)  ; $PIECE extract based on variable length delimiter
  N %LEN,%PIECE,%NEWSTR
  S %STRING=$G(%STRING)
  S %OLDDEL=$G(%OLDDEL) I %OLDDEL="" S %OLDDEL=" "
  S %LEN=$L(%OLDDEL)
  ; each %OLDDEL-sized chunk of %OLDSTR that might be delimiter
  S %NEWDEL=$G(%NEWDEL) I %NEWDEL="" S %NEWDEL="^"
  ; each piece of the old string
  S %NEWSTR="" ; new reformatted string to return
  F  Q:%OLDSTR=""  D
  . S %PIECE=$P(%OLDSTR,%OLDDEL)
  . S $P(%OLDSTR,%OLDDEL)=""
  . S %NEWSTR=%NEWSTR_$S(%NEWSTR="":"",1:%NEWDEL)_%PIECE
  . F  Q:%OLDDEL'=$E(%OLDSTR,1,%LEN)  S $E(%OLDSTR,1,%LEN)=""
  .Q
  Q %NEWSTR
  ;
  ;"==========================================================================
  ;"==========================================================================
  ;
GETVARS(JOB,QUIET,TRIMVARS)  ;"Get reference to listing of variables from other job. 
  ;"INPUT: JOB --  Process ID (Job Number) to query
  ;"       QUIET -- if 1, then nothing to console
  ;"       TRIMVARS -- optional, default is 1
  NEW TEMP SET TEMP=$$INTRPT(JOB,"EXAM",.QUIET)  ;"Send the interupt
  NEW RESULT
  IF TEMP=0 DO  GOTO GVDN  
  . SET RESULT="-1^Unable to Examine JOB, please retry later" 
  SET RESULT=$NAME(^XUTL("XUSYS",JOB,"JE"))
  IF $GET(TRIMVARS,1)=1 DO   ;"Remove vars added to variable table in JOBEXAM^ZU
  . NEW IDX SET IDX=0 
  . FOR  SET IDX=$ORDER(@RESULT@("V",IDX)) QUIT:(IDX'>0)  DO
  . . NEW ENTRY SET ENTRY=$GET(@RESULT@("V",IDX))
  . . IF ENTRY["%ZPOS" DO  QUIT  
  . . . KILL @RESULT@("V",IDX)
  . . IF ENTRY["%reference" DO  ;"this is added to Var table in JOBEXAM^ZU
  . . . KILL @RESULT@("V",IDX)
GVDN ;  
  QUIT RESULT
  ;  
INTRPT(OTHERJOB,COMMAND,QUIET)  ;Send MUPIP intrpt
  ;"Input: OTHERJOB -- the job number (PID) of other job to send interrupt to
  ;"       COMMAND -- the command to pass to other job.  E.g. 'EXAM'
  NEW $ET,$ES S $ET="D IRTERR^ZJOB"  
  NEW RESULT SET RESULT=0  ;"default to failure.
  SET QUIET=+$GET(QUIET)
  NEW ZSYSCMD,ZPATH
  IF OTHERJOB=$JOB GOTO INTDN ;" shouldn't interrupt ourself
  ;"We need a LOCK to guarantee commands from two processes don't conflict
  ;"//kt NOTE: Why not just send command to ^XUTL("XUSYS","COMMAND",OTHERJOB) ?? 
  LOCK +^XUTL("XUSYS","COMMAND"):10 GOTO:'$T INTDN
  ;                          
  SET ^XUTL("XUSYS","COMMAND")=COMMAND
  SET ^XUTL("XUSYS","COMMAND",0)=$J_":"_$H
  KILL ^XUTL("XUSYS",OTHERJOB,"JE")
  KILL ^XUTL("XUSYS",OTHERJOB,"STATUS")  ;"//clear status.  Other job will set to 'DONE' when completed  
  ;
  SET ZSYSCMD="mupip intrpt "_OTHERJOB  ;" command to interrupt other job
  IF QUIET SET ZSYSCMD=ZSYSCMD_" 2> /dev/null"  ;" output to null
  SET ZPATH="$gtm_dist/" ;Unix path
  IF QUIET=0 WRITE !,"Sending intrp to job. Any error means you don't have the privilege to interrupt.",!
  ;"NOTE:  How does this work?
  ;"       Each job, when run from linux, has a parameter set up: gtm_zinterrupt='I $$JOBEXAM^ZU($ZPOSITION)'
  ;"       So went interrupt is sent, the other job will run: I $$JOBEXAM^ZU($ZPOSITION) 
  ;"       This will look at ^XUTL("XUSYS","COMMAND") for the command.  
  ;"         If CMD='EXAM' --> will effect: ZSHOW "*":^XUTL("XUSYS",$J,"JE")  <-- $J is the other job's number
  ;
  NEW STARTH SET STARTH=$H
  ZSYSTEM ZPATH_ZSYSCMD ; System Request
  NEW DONE SET DONE=0
  FOR  DO  QUIT:DONE
  . IF $GET(^XUTL("XUSYS",OTHERJOB,"STATUS"))="DONE" DO  QUIT  ;" <-- should be set by other job.
  . . SET DONE=1,RESULT=1 
  . . KILL ^XUTL("XUSYS",OTHERJOB,"STATUS")
  . NEW DELTASEC SET DELTASEC=$$HDIFF^XLFDT($H,STARTH,2)
  . SET DONE=(DELTASEC>2)  ;"TIMEOUT AFTER 2 SEC.    
  ;  
  KILL ^XUTL("XUSYS","COMMAND") ;"Cleanup
  LOCK -^XUTL("XUSYS","COMMAND")
INTDN ;  
  QUIT RESULT  ;"Report 1 if we received interrupt, otherwise 0
  ;  
CONSOLE  ;"provide console interface to ZJOB
  WRITE #  ;"clear screen
  NEW ARRAY,OPTION,TEMPCLR
  SET OPTION("COLUMNS","NUM")=3
  WRITE "Sending interrupts to other jobs to query information..."
  NEW DATA DO QUIET^TMGZSY(.DATA,1)
  WRITE !
  DO LOADDATA(.DATA,"ARRAY",.OPTION)  
  KILL DATA
  DO
  . SET OPTION("HEADER",1)=" - < EXAMINE JOB UTILITY > -"
  . SET OPTION("HEADER","COL",1)="Process ID"
  . SET OPTION("HEADER","COL",2)="Process INFO"
  . SET OPTION("HEADER","COL",3)="Details"
  . SET OPTION("FOOTER",1,1)="^ to exit"
  . ;"SET OPTION("ON SELECT")="HNDONSEL^TMGUSRIF"
  . ;"SET OPTION("ON CMD")="HNDONCMD^TMGUSRIF"
  . SET OPTION("ON CURSOR")="HNDONCURSOR^TMGZJOB"
  . SET OPTION("ON CURSOR DONE")="HNDONCSRDN^TMGZJOB"
  . SET OPTION("ON DRAW MAIN LINE")="HNDONDRAW^TMGZJOB"
  . SET OPTION("ON KEYPRESS")="HNDONKP^TMGZJOB"
  . ;
  . SET OPTION("COLORS","NORM")=$$COLORPAIR^TMGUSRI8("WHITE","BLUE",.TMPCLR) 
  . SET OPTION("COLORS","HIGH")=$$COLORPAIR^TMGUSRI8("BLACK","YELLOW",.TMPCLR) 
  . SET OPTION("COLORS","INACTIVE HIGH")=$$COLORPAIR^TMGUSRI8("WHITE","GREY",.TMPCLR) 
  . SET OPTION("COLORS","HEADER")=$$COLORPAIR^TMGUSRI8("BLACK","GREY",.TMPCLR)
  . SET OPTION("COLORS","FOOTER")=$$COLORPAIR^TMGUSRI8("BLACK","GREY",.TMPCLR)
  . SET OPTION("COLORS","TOP LINE")=$$COLORPAIR^TMGUSRI8("BLACK","GREY",.TMPCLR)
  . SET OPTION("COLORS","BOTTOM LINE")=$$COLORPAIR^TMGUSRI8("BLACK","GREY",.TMPCLR)
  . SET OPTION("COLORS","INDEX")=$$COLORPAIR^TMGUSRI8("BLACK","WHITE",.TMPCLR)
  . ; 
  . SET OPTION("COLUMNS",2,"COLORS","NORM")=$$COLORPAIR^TMGUSRI8("WHITE","CYAN",.TMPCLR)
  . SET OPTION("COLUMNS",2,"COLORS","HIGH")=$$COLORPAIR^TMGUSRI8("BLACK","YELLOW",.TMPCLR) 
  . SET OPTION("COLUMNS",2,"COLORS","INACTIVE HIGH")=$$COLORPAIR^TMGUSRI8("WHITE","GREY",.TMPCLR) 
  . SET OPTION("COLUMNS",2,"COLORS","INDEX")=$$COLORPAIR^TMGUSRI8("BLACK","WHITE",.TMPCLR)
  . ;
  . SET OPTION("COLUMNS",3,"COLORS","NORM")=$$COLORPAIR^TMGUSRI8("WHITE","BLUE",.TMPCLR)
  . SET OPTION("COLUMNS",3,"COLORS","HIGH")=$$COLORPAIR^TMGUSRI8("WHITE","GREY",.TMPCLR) 
  . SET OPTION("COLUMNS",3,"COLORS","HI-SEL")=$$COLORPAIR^TMGUSRI8("WHITE","MAGENTA",.TMPCLR) 
  . SET OPTION("COLUMNS",3,"COLORS","INDEX")=$$COLORPAIR^TMGUSRI8("BLACK","WHITE",.TMPCLR)
  . SET OPTION("COLUMNS",3,"COLORS","SELECTED")=$$COLORPAIR^TMGUSRI8("WHITE","RED",.TMPCLR)
  . ;
  . SET OPTION("COLUMNS",1,"WIDTH")=13
  . SET OPTION("COLUMNS",2,"WIDTH")=40
  . ;
  . SET OPTION("LR SCROLLING")=0
  . SET OPTION("SHOW INDEX")=0
  . ;
  . ;"BELOW IS GOOD FOR DEBUGGING
  . NEW ZZDEBUG SET ZZDEBUG=0
  . IF ZZDEBUG=1 DO                    
  . . SET OPTION("SCRN TOP OFFSET")=25
  . . SET OPTION("SCRN HEIGHT")=20
  . . SET OPTION("SCRN WIDTH")=100 
  ;
  DO SCROLLER^TMGUSRIF("ARRAY",.OPTION)
  QUIT
  ;
LOADDATA(DATA,ARRAYREF,OPTION) ;
  ;"INPUT: DATA -- data as output by QUIET^TMGZSY(.DATA,1). PASS BY REFERENCE.  Format:
  ;"         DATA(<INDEX#>,<SECTION>).  Sections are: 
  ;"              'CPU TIME'   e.g. DATA(<INDEX#>,'CPU TIME')='00:00:09'
  ;"              'DEV'        e.g. DATA(<INDEX#>,'DEV',<INDEX#>)=<info>
  ;"              'DEVICE'     e.g. DATA(<INDEX#>,'DEVICE')=''
  ;"              'ENV'        e.g. DATA(<INDEX#>,'VARS',<ParamName>)=<ParamValue>
  ;"              'MODE'       e.g. DATA(<INDEX#>,'MODE')='-direct'
  ;"              'PROCID'     e.g. DATA(<INDEX#>,'PROCID')='55661'
  ;"              'PROCNAME'   e.g. DATA(<INDEX#>,'PROCID')='Sub 55661'
  ;"              'PS'         e.g. DATA(<INDEX#>,'PS')='hib'
  ;"              'RTN'        e.g. DATA(<INDEX#>,'RTN')='GETTASK+3^%ZTMS1'
  ;"              'VARS'       e.g. DATA(<INDEX#>,'VARS',<INDEX#>)='varname=varvalue'                                 
  ;"        ARRAYREF  -- PASS BY NAME. Array to hold displayed data in scroller. 
  ;"        OPTION -- PASS BY REFERENCE. Array to hold options for scroller.       
  ;"RESULTS: none
  NEW XREF  ;"FORMAT:  XREF("PID",<PID>)=INDEX# IN ARRAY()
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(DATA(IDX)) QUIT:IDX'>0  DO
  . NEW PID SET PID=+$GET(DATA(IDX,"PROCID"))
  . ;"See if ARRAY already has PID
  . NEW TEMPI SET TEMPI=0
  . FOR  SET TEMPI=$ORDER(@ARRAYREF@(TEMPI)) QUIT:TEMPI'>0  DO  QUIT:TEMPI'>0
  . . IF $ORDER(@ARRAYREF@(TEMPI,""))'=PID QUIT
  . . SET XREF("PID",PID)=TEMPI
  . . SET TEMPI=0
  . IF $DATA(XREF("PID",PID))=0 SET XREF("PID",PID)=IDX  ;"not found, so used IDX from DATA(IDX)
  SET IDX=0  
  FOR  SET IDX=$ORDER(DATA(IDX)) QUIT:IDX'>0  DO
  . NEW TEMP MERGE TEMP=DATA(IDX)
  . NEW PID SET PID=+$GET(TEMP("PROCID"))
  . NEW ARRIDX SET ARRIDX=$GET(XREF("PID",PID)) 
  . IF ARRIDX="" SET ARRIDX=IDX
  . SET @ARRAYREF@(ARRIDX,PID)="Data for PID #"_PID
  . KILL XREF("NAME",ARRIDX)
  . NEW TEMPI SET TEMPI=""
  . FOR  SET TEMPI=$ORDER(@ARRAYREF@("COL",2,ARRIDX,"TXT",TEMPI)) QUIT:TEMPI'>0  DO
  . . NEW STR SET STR=$GET(@ARRAYREF@("COL",2,ARRIDX,"TXT",TEMPI)) QUIT:STR=""
  . . NEW NAME SET NAME=$PIECE(STR,":",1)
  . . SET XREF("NAME",ARRIDX,NAME)=TEMPI   ;"save xref if @ARRAYREF@ already had entry.  
  . NEW JDX SET JDX=1
  . NEW NAME FOR NAME="CPU TIME","DEV","DEVICE","MODE","PROCID","PROCNAME","PS","RTN","ENV","VARS" DO
  . . IF $GET(TEMP(NAME))="@" QUIT
  . . NEW NAMEIDX SET NAMEIDX=+$GET(XREF("NAME",ARRIDX,NAME))
  . . IF NAMEIDX=0 SET NAMEIDX=JDX,JDX=JDX+1
  . . SET @ARRAYREF@("COL",2,ARRIDX,"TXT",NAMEIDX)=NAME_": "_$GET(TEMP(NAME))
  . . SET OPTION("ZJOB ARR",ARRIDX,NAME)=$GET(TEMP(NAME))
  . . IF NAME="RTN" DO  QUIT   
  . . . NEW CMD SET CMD=$GET(TEMP("RTN")) QUIT:CMD=""
  . . . KILL OPTION("ZJOB ARR",ARRIDX,"RTN")
  . . . KILL @ARRAYREF@("COL",3,ARRIDX,NAMEIDX)
  . . . NEW ABSRTN,TEMP SET ABSRTN=$$CONVERTPOS^TMGMISC(CMD,"TEMP")
  . . . SET OPTION("ZJOB ARR",ARRIDX,"RTN","ABS RTN")=ABSRTN
  . . . SET ABSRTN=$PIECE(ABSRTN,"^",1) IF ABSRTN["+" SET ABSRTN=+$PIECE(ABSRTN,"+",2)
  . . . SET OPTION("ZJOB ARR",ARRIDX,"RTN","ABS INDEX")=ABSRTN
  . . . ;"restore later if this info is actually needed --> MERGE OPTION("ZJOB ARR","RTN","ROUTINE INFO")=TEMP
  . . . NEW RTN SET RTN=$PIECE(CMD,"^",2) QUIT:RTN=""
  . . . SET OPTION("ZJOB ARR",ARRIDX,"RTN","ROUTINE")=RTN
  . . . NEW LABEL SET LABEL=$PIECE(CMD,"^",1)
  . . . NEW OFFSET SET OFFSET=+$PIECE(LABEL,"+",2)
  . . . SET OPTION("ZJOB ARR",ARRIDX,"RTN","LABEL OFFSET")=OFFSET
  . . . SET LABEL=$PIECE(LABEL,"+",1)
  . . . SET OPTION("ZJOB ARR",ARRIDX,"RTN","LABEL")=LABEL
  . . . NEW RTNLINEIDX FOR RTNLINEIDX=1:1 DO  QUIT:RTNLINEIDX=0
  . . . . NEW LINE SET LINE=$TEXT(+RTNLINEIDX^@RTN)
  . . . . IF LINE="" SET RTNLINEIDX=0 QUIT
  . . . . SET @ARRAYREF@("COL",3,ARRIDX,NAMEIDX,"TXT",RTNLINEIDX)=LINE
  . . IF NAME="DEV" DO  QUIT
  . . . KILL OPTION("ZJOB ARR",ARRIDX,"DEV:")
  . . . KILL @ARRAYREF@("COL",3,ARRIDX,NAMEIDX)  
  . . . SET @ARRAYREF@("COL",2,ARRIDX,"TXT",NAMEIDX)="DEV:"
  . . . NEW KDX SET KDX=0
  . . . FOR  SET KDX=$ORDER(TEMP("DEV",KDX)) QUIT:KDX'>0  DO
  . . . . NEW VALUE SET VALUE=$$TRIM^XLFSTR($GET(TEMP("DEV",KDX)))
  . . . . SET @ARRAYREF@("COL",3,ARRIDX,NAMEIDX,"TXT",KDX)=VALUE
  . . . . SET OPTION("ZJOB ARR",ARRIDX,"DEV",KDX)=VALUE
  . . IF NAME="ENV" DO  QUIT
  . . . KILL OPTION("ZJOB ARR",ARRIDX,"ENV")
  . . . KILL @ARRAYREF@("COL",3,ARRIDX,NAMEIDX)
  . . . SET @ARRAYREF@("COL",2,ARRIDX,"TXT",NAMEIDX)="ENV:"
  . . . NEW PARAM SET PARAM="",KDX=1
  . . . FOR  SET PARAM=$ORDER(TEMP("ENV",PARAM)) QUIT:PARAM=""  DO
  . . . . NEW VALUE SET VALUE=$GET(TEMP("ENV",PARAM))
  . . . . SET @ARRAYREF@("COL",3,ARRIDX,NAMEIDX,"TXT",KDX)=PARAM_" = "_VALUE,KDX=KDX+1
  . . . . SET OPTION("ZJOB ARR",ARRIDX,"ENV",PARAM)=VALUE
  . . IF NAME="VARS" DO  QUIT
  . . . KILL @ARRAYREF@("COL",3,ARRIDX,NAMEIDX)
  . . . KILL OPTION("ZJOB ARR",ARRIDX,"VAR")  
  . . . SET @ARRAYREF@("COL",2,ARRIDX,"TXT",NAMEIDX)="VARS:"
  . . . NEW ENTRY SET ENTRY=0,KDX=1
  . . . FOR  SET ENTRY=$ORDER(TEMP("VARS",ENTRY)) QUIT:ENTRY'>0  DO
  . . . . NEW VALUE SET VALUE=$GET(TEMP("VARS",ENTRY))
  . . . . SET @ARRAYREF@("COL",3,ARRIDX,NAMEIDX,"TXT",KDX)=VALUE,KDX=KDX+1
  . . . . SET OPTION("ZJOB ARR",ARRIDX,"VAR",ENTRY)=VALUE
  QUIT
  ;
HNDONDRAW(TMGPSCRLARR,OPTION,INFO)  ;"Part of CONSOLE^TMGZJOB
  ;"Purpose: handle ON DRAW MAIN LINE event from SCROLLER
  ;"USED IN GLOBAL SCOPE:  HIGHLINE <-- array of selected line for each column used in TMGUSRIF.  Used READ ONLY
  ;"                       ACTIVECOL <-- Which column is active, meaning hold user cursor
  ;"Input: TMGPSCRLARR,OPTION,INFO -- see documentation in SCROLLER
  ;"       INFO has this:
  ;"         INFO("DRAW MAIN","TEXT")=TEXT
  ;"         INFO("DRAW MAIN","SELECTED")=SELECTED
  ;"         INFO("DRAW MAIN","ON HIGHLIGHT LINE")=ONHIGHLINE
  ;"         INFO("DRAW MAIN","TEXT COLOR")=TEXTCOLOR
  ;"         INFO("DRAW MAIN","SHOW INDEX")=SHOWIDX
  ;"         INFO("DRAW MAIN","DATA INDEX")=IDX
  ;"         INFO("DRAW MAIN","COLUMN NUM")=COLNUM    <-- READ ONLY
  ;
  NEW DRAWCOL SET DRAWCOL=+$GET(INFO("DRAW MAIN","COLUMN NUM"))
  NEW SEL1 SET SEL1=+$GET(HIGHLINE(1))
  NEW SEL2 SET SEL2=+$GET(HIGHLINE(2))
  IF $GET(INFO("DRAW MAIN","COLUMN NUM"))=1 DO
  . IF +$GET(ACTIVECOL)=1 QUIT  ;"Only proceed if drawing an inactive column
  . NEW DATAIDX SET DATAIDX=$GET(INFO("DRAW MAIN","DATA INDEX"))
  . IF DATAIDX'=SEL1 QUIT
  . SET INFO("DRAW MAIN","TEXT COLOR")="INACTIVE HIGH"
  ELSE  IF $GET(INFO("DRAW MAIN","COLUMN NUM"))=2 DO
  . IF +$GET(ACTIVECOL)=2 QUIT  ;"Only proceed if drawing an inactive column
  . NEW DATAIDX SET DATAIDX=$GET(INFO("DRAW MAIN","DATA INDEX"))
  . IF DATAIDX'=SEL2 QUIT
  . SET INFO("DRAW MAIN","TEXT COLOR")="INACTIVE HIGH"
  ELSE  IF $GET(INFO("DRAW MAIN","COLUMN NUM"))=3 DO
  . NEW SEL1TXT SET SEL1TXT=$GET(@TMGPSCRLARR@(SEL1))
  . NEW SEL2TXT SET SEL2TXT=$GET(@TMGPSCRLARR@("COL",2,SEL1,"TXT",SEL2))
  . IF $EXTRACT(SEL2TXT,1,4)'="RTN:" QUIT
  . NEW TEXT SET TEXT=$GET(INFO("DRAW MAIN","TEXT"))
  . NEW ARR MERGE ARR=OPTION("ZJOB ARR",SEL1)
  . KILL ARR("ENV")
  . NEW DATAIDX SET DATAIDX=$GET(INFO("DRAW MAIN","DATA INDEX"))
  . NEW RTNIDX SET RTNIDX=$GET(ARR("RTN","ABS INDEX"))
  . IF DATAIDX=RTNIDX DO
  . . NEW SELECTED,ONHIGHLINE,TEXTCOLOR
  . . SET SELECTED=1  ;"//$GET(INFO("DRAW MAIN","SELECTED"))
  . . SET ONHIGHLINE=$GET(INFO("DRAW MAIN","ON HIGHLIGHT LINE"))
  . . IF SELECTED SET TEXTCOLOR=$SELECT(ONHIGHLINE:"HI-SEL",1:"SELECTED")
  . . ELSE  SET TEXTCOLOR=$SELECT(ONHIGHLINE:"HIGH",1:"NORM")
  . . SET INFO("DRAW MAIN","TEXT COLOR")=TEXTCOLOR
  . . SET INFO("DRAW MAIN","SELECTED")=SELECTED
  QUIT
  ;
HNDONCURSOR(TMGPSCRLARR,OPTION,INFO)  ;"Part of CONSOLE^TMGZJOB
  ;"Purpose: handle ON CURSOR event from SCROLLER
  ;"Input: TMGPSCRLARR,OPTION,INFO -- see documentation in SCROLLER
  ;"       INFO has this:
  ;"          INFO("CURSOR")=<user direction key>, e.g. RIGHT, LEFT, etc
  ;"          INFO("USER INPUT")=INPUT
  ;"          INFO("CURRENT LINE","NUMBER")=number currently highlighted line
  ;"          INFO("CURRENT LINE","TEXT")=Text of currently highlighted line
  ;"          INFO("CURRENT LINE","RETURN")=return value of currently highlighted line
  ;"USED IN GLOBAL SCOPE:  HIGHLINE <-- array of selected line for each column used in TMGUSRIF.  
  NEW CURSOR SET CURSOR=$GET(INFO("CURSOR"))
  NEW ACTIVECOL SET ACTIVECOL=+$GET(OPTION("COLUMNS","ACTIVE"),1)
  NEW MAXCOLS SET MAXCOLS=+$GET(OPTION("COLUMNS","NUM"),1)
  NEW COL2TEXT SET COL2TEXT=$$COLTEXT(TMGPSCRLARR,.HIGHLINE,2)
  NEW VARSELECTED SET VARSELECTED=(COL2TEXT="VARS:")
  IF CURSOR="RIGHT" DO  
  . IF $GET(OPTION("LR SCROLLING"))=1 QUIT
  . IF ACTIVECOL>=MAXCOLS QUIT
  . SET ACTIVECOL=ACTIVECOL+1
  . SET TMGSCLRMSG="FULL"  
  . SET OPTION("COLUMNS","ACTIVE")=ACTIVECOL
  . SET INFO("CURSOR","HANDLED")=1
  . IF ACTIVECOL=3 DO
  . . DO SETF2STATUS(1)  ;"show F2 to enable
  . . NEW SEL1 SET SEL1=+$GET(HIGHLINE(1))  
  . . NEW ARR MERGE ARR=OPTION("ZJOB ARR",SEL1)  
  . . NEW RTNIDX SET RTNIDX=$GET(ARR("RTN","ABS INDEX"))
  . . SET HIGHLINE(3)=RTNIDX
  IF CURSOR="LEFT" DO  
  . IF $GET(OPTION("LR SCROLLING"))=1 QUIT
  . IF ACTIVECOL=1 QUIT
  . SET ACTIVECOL=ACTIVECOL-1
  . SET TMGSCLRMSG="FULL"  
  . SET OPTION("COLUMNS","ACTIVE")=ACTIVECOL
  . SET INFO("CURSOR","HANDLED")=1
  . IF ACTIVECOL<3 DO
  . . IF $$F2STATUS()>0 DO SETF2STATUS(-1)  ;"REMOVE F2 Scroll message
  . . IF $$F4STATUS()>0 DO SETF4STATUS(0)   ;"REMOVE F4 message  (May be restored in HNDONCSRDN)
  IF ACTIVECOL=3,("UP^DOWN^RIGHT"[CURSOR) DO  
  . NEW SHOWF4 SET SHOWF4=VARSELECTED
  . NEW COL3TEXT SET COL3TEXT=$$COLTEXT(TMGPSCRLARR,.HIGHLINE,3)
  . SET COL3TEXT=$PIECE(COL3TEXT,"=",1)
  . DO SETF4STATUS(SHOWF4,COL3TEXT)  ;"show F4 if appropriate  
  QUIT
  ;
HNDONCSRDN(TMGPSCRLARR,OPTION,INFO)  ;"Part of CONSOLE^TMGZJOB
  ;"Purpose: handle ON CURSOR DONE event from SCROLLER
  ;"Input: TMGPSCRLARR,OPTION,INFO -- see documentation in SCROLLER
  ;"       INFO has this:
  ;"          INFO("CURSOR")=<user direction key>
  ;"          INFO("USER INPUT")=INPUT
  ;"          INFO("CURRENT LINE","NUMBER")=number currently highlighted line
  ;"          INFO("CURRENT LINE","TEXT")=Text of currently highlighted line
  ;"          INFO("CURRENT LINE","RETURN")=return value of currently highlighted line
  ;"USED IN GLOBAL SCOPE:  HIGHLINE <-- array of selected line for each column used in TMGUSRIF.  Used READ ONLY
  NEW HL SET HL=HIGHLINE(1)
  NEW PID SET PID=$ORDER(@TMGPSCRLARR@(HL,0))
  SET OPTION("FOOTER",1,3)="[F3] REFRESH "_PID
  NEW COL2TEXT SET COL2TEXT=$$COLTEXT(TMGPSCRLARR,.HIGHLINE,2)
  NEW VARSELECTED SET VARSELECTED=(COL2TEXT="VARS:")
  NEW ACTIVECOL SET ACTIVECOL=+$GET(OPTION("COLUMNS","ACTIVE"),1)
  IF ACTIVECOL=2 DO
  . DO SETF4STATUS(VARSELECTED,"ALL VARS")  ;"show F4 if appropriate  
  QUIT
  ;
HNDONKP(TMGPSCRLARR,OPTION,INFO)  ;"Part of CONSOLE^TMGZJOB
  ;"Purpose: handle ON KEYPRESS event from SCROLLER
  ;"Input: TMGPSCRLARR,OPTION,INFO -- see documentation in SCROLLER
  ;"       INFO has this:
  ;"          INFO("CURRENT LINE","NUMBER")=number currently highlighted line
  ;"          INFO("CURRENT LINE","TEXT")=Text of currently highlighted line
  ;"          INFO("CURRENT LINE","RETURN")=return value of currently highlighted line
  ;"USED IN GLOBAL SCOPE:  HIGHLINE <-- array of selected line for each column used in TMGUSRIF.  Used READ ONLY
  NEW USR SET USR=$GET(INFO("USER INPUT"))
  IF USR="{F2}",$$F2STATUS()>0 DO
  . SET TMGSCLRMSG="FULL"
  . IF $GET(OPTION("LR SCROLLING"))=1 DO  ;"//LR scrolling initially ON
  . . SET OPTION("LR SCROLLING")=0   ;"turn off
  . . DO SETF2STATUS(1)  ;"show F2 to enable
  . ELSE  DO   ;"//LR scrolling initially ON
  . . SET OPTION("LR SCROLLING")=1   ;"turn ON
  . . DO SETF2STATUS(2)  ;"show F2 to disable
  IF USR="{F4}",$$F4STATUS()>0 DO
  . SET TMGSCLRMSG="FULL"
  . NEW VAR SET VAR=$$COLTEXT(TMGPSCRLARR,.HIGHLINE,3)
  . WRITE !,!,!,"To do: finish copy functionality in HNDONKP^TMGZJOB",!
  . WRITE "VAR: ",VAR,!
  . DO PRESS2GO^TMGUSRI2
  IF USR="{F3}" DO  ;"Refresh info on other job
  . NEW IDX SET IDX=$$FNIDX("F3") QUIT:IDX<0
  . NEW STR SET STR=$GET(OPTION("FOOTER",1,IDX)) QUIT:(STR'["REFRESH ")
  . NEW OTHERJOB SET OTHERJOB=+$PIECE(STR,"REFRESH ",2) QUIT:OTHERJOB'>0
  . NEW REF SET REF=$$GETVARS(OTHERJOB,1,0) QUIT:REF=""
  . NEW DATA MERGE DATA(1,"VARS")=@REF@("V")
  . NEW NAME FOR NAME="CPU TIME","DEV","DEVICE","MODE","PROCID","PROCNAME","PS","RTN","ENV" DO
  . . SET DATA(1,NAME)="@"  ;"default to NULL
  . SET DATA(1,"PROCID")=OTHERJOB
  . SET IDX=0 FOR  SET IDX=$ORDER(DATA(1,"VARS",IDX)) QUIT:(IDX'>0)  DO
  . . NEW ENTRY SET ENTRY=$GET(DATA(1,"VARS",IDX))
  . . IF ENTRY["%ZPOS" DO  QUIT  ;"this is added to Var table in JOBEXAM^ZU
  . . . SET ENTRY=$PIECE(ENTRY,"=",2)
  . . . SET ENTRY=$TRANSLATE(ENTRY,"""","")
  . . . SET DATA(1,"RTN")=ENTRY
  . . . KILL DATA(1,"VARS",IDX)
  . . IF ENTRY["%reference" DO  ;"this is added to Var table in JOBEXAM^ZU
  . . . KILL DATA(1,"VARS",IDX)
  . DO LOADDATA(.DATA,TMGPSCRLARR,.OPTION)
  . SET TMGSCLRMSG="FULL"  ;"full redraw
  QUIT
  ;
FNIDX(FN,FORCE)  ;"Return footer index holding FN message
  ;"INPUT: FN.. e.g. 'F1' or 'F2' or 'F3' etc.   NOT '[F1]' etc.
  ;"       FORCE:  if 1, then even if not found, index return to be next empty entry 
  NEW RESULT SET RESULT=-1
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(OPTION("FOOTER",1,IDX)) QUIT:(IDX'>0)!(RESULT>0)  DO
  . NEW STR SET STR=$GET(OPTION("FOOTER",1,IDX)) QUIT:STR=""
  . IF STR[("["_FN_"]") SET RESULT=IDX QUIT
  IF RESULT<0,$GET(FORCE)=1 DO
  . SET RESULT=$ORDER(OPTION("FOOTER",1,""),-1)+1
  QUIT RESULT
  ;
F2STATUS() ;
  ;"RESULT: 1 = [F2] is shown for ENABLE, and has text 'L,R scroll'
  ;"        2 = [F2] is shown for DISABLE
  ;"       -1 = [F2] NOT shown. 
  ;"USES OPTION IN GLOBAL SCOPE
  NEW RESULT SET RESULT=-1  ;"default
  NEW IDX SET IDX=$$FNIDX("F2")
  NEW STR SET STR=$GET(OPTION("FOOTER",1,IDX))
  IF (STR["F2"),(STR["L,R scroll") DO
  . IF STR["ENABLE" SET RESULT=1
  . IF STR["DISABLE" SET RESULT=2
  QUIT RESULT
  ;
SETF2STATUS(STATUS) ;
  ;"STATUS: 1 -> change to '[F2] ENABLE L,R scroll'
  ;"        2 -> change to '[F2] DISABLE L,R scroll'
  ;"       -1 -> delete entry
  ;"USES OPTION IN GLOBAL SCOPE
  SET STATUS=+$GET(STATUS)
  NEW IDX SET IDX=$$FNIDX("F2",1) QUIT:IDX<0
  IF STATUS=-1 KILL OPTION("FOOTER",1,IDX)
  ELSE  IF "1,2,"[STATUS DO
  . SET OPTION("FOOTER",1,IDX)="[F2] "_$SELECT(STATUS=1:"ENABLE",STATUS=2:"DISABLE")_" L,R scroll"
  DO SORTFOOTERS
  QUIT
  ;
F4STATUS() ;
  ;"RESULT: 1 = [F4] is shown and has text 'COPY 1 var to local'
  ;"       -1 = [F4] NOT shown. 
  ;"USES OPTION IN GLOBAL SCOPE
  NEW RESULT SET RESULT=-1  ;"default
  NEW IDX SET IDX=$$FNIDX("F4")
  NEW STR SET STR=$GET(OPTION("FOOTER",1,IDX))
  IF (STR["F4"),(STR["to local") DO
  . SET RESULT=1
  QUIT RESULT
  ;
SETF4STATUS(STATUS,VARNAME) ;
  ;"STATUS: 1 -> change to '[F4] COPY 1 var to local'
  ;"       0 -> delete entry
  ;"USES OPTION IN GLOBAL SCOPE
  SET STATUS=+$GET(STATUS)
  NEW IDX SET IDX=$$FNIDX("F4",1) QUIT:IDX<0
  IF STATUS=0 DO
  . KILL OPTION("FOOTER",1,IDX)
  ELSE  IF STATUS=1 DO
  . SET OPTION("FOOTER",1,IDX)="[F4] COPY '"_$GET(VARNAME)_"' to local"
  DO SORTFOOTERS
  QUIT
  ;  
COLTEXT(TMGPSCRLARR,HIGHLINE,COL) ;"Return currently highlighted text at column COL
  NEW RESULT SET RESULT=""
  NEW H1 SET H1=+$GET(HIGHLINE(1))
  SET COL=+$GET(COL)
  IF COL=1 DO  GOTO CTDN
  . SET RESULT=$ORDER(@TMGPSCRLARR@(H1,""))
  NEW H2 SET H2=+$GET(HIGHLINE(2))
  IF COL=2 DO  GOTO CTDN
  . SET RESULT=$GET(@TMGPSCRLARR@("COL",2,H1,"TXT",H2))
  NEW H3 SET H3=+$GET(HIGHLINE(3))
  IF COL=3 DO  GOTO CTDN
  . SET RESULT=$GET(@TMGPSCRLARR@("COL",3,H1,H2,"TXT",H3))
CTDN ;
  QUIT RESULT      
  ;
SORTFOOTERS  ;"sort the [Fn] entries so in order  
  ;"USES OPTION IN GLOBAL SCOPE
  ;"to do...
  QUIT