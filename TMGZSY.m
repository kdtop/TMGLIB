TMGZSY     ;ISF/RWF - GT.M/VA system status display ;8/15/07  10:39
     ;;8.0;KERNEL;**349**;Jul 10, 1995;Build 29
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;                                                              ;
     ;      Copyright 1989,2001 Sanchez Computer Associates, Inc.   ;
     ;                                                              ;
     ;      This source code contains the intellectual property     ;
     ;      of its copyright holder(s), and is made available       ;
     ;      under a license.  If you do not know the terms of       ;
     ;      the license, please stop and do not read further.       ;
     ;                                                              ;
     ;";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;" NOTE: modified from ZSY by K. Toppenberg to serve local needs ;
     ;"                                                               ;
     ;"                                                               ;
     ;";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;GT.M/VA %SY utility - status display
     ;
     ;From the top just show by PID
     NEW IMAGE,MODE
     WRITE !,"GT.M system status ",!  
     SET IMAGE=0,MODE=0 
     DO WORK(IMAGE,MODE)
     QUIT
     ;
QUIET(OUT,GETVARS) ;
     ;"INPUT: OUT -- PASS BY REFERENCE, AN OUT PARMETER.  Format:
     ;"       OUT(<INDEX#>,<SECTION>).  Sections are: 
     ;"            'CPU TIME'   e.g. OUT(<INDEX#>,'CPU TIME')='00:00:09'
     ;"            'DEV'        e.g. OUT(<INDEX#>,'DEV',<INDEX#>)=<info>
     ;"            'DEVICE'     e.g. OUT(<INDEX#>,'DEVICE')=''
     ;"            'ENV'        e.g. OUT(<INDEX#>,'VARS',<ParamName>)=<ParamValue>
     ;"            'MODE'       e.g. OUT(<INDEX#>,'MODE')='-direct'
     ;"            'PROCID'     e.g. OUT(<INDEX#>,'PROCID')='55661'
     ;"            'PROCNAME'   e.g. OUT(<INDEX#>,'PROCID')='Sub 55661'
     ;"            'PS'         e.g. OUT(<INDEX#>,'PS')='hib'
     ;"            'RTN'        e.g. OUT(<INDEX#>,'RTN')='GETTASK+3^%ZTMS1'
     ;"            'VARS'       e.g. OUT(<INDEX#>,'VARS',<INDEX#>)='varname=varvalue'
     ;"       GETVARS -- OPTIONAL.  If 1 then process variables also gotten.  
     NEW OPTION SET OPTION("QUIET")=1
     SET IMAGE=0,MODE=0 
     DO WORK(IMAGE,MODE,.OPTION)
     IF $GET(GETVARS)=1 DO
     . NEW IDX SET IDX=0
     . FOR  SET IDX=$ORDER(OPTION("OUTPUT",IDX)) QUIT:IDX'>0  DO
     . . NEW PID SET PID=$GET(OPTION("OUTPUT",IDX,"PROCID")) QUIT:PID'>0
     . . NEW REF SET REF=$$GETVARS^TMGZJOB(PID,1,1)
     . . IF +REF=-1 SET OPTION("OUTPUT",IDX,"VARS")=$PIECE(REF,"^",2) QUIT
     . . MERGE OPTION("OUTPUT",IDX,"VARS")=@REF@("V")
     MERGE OUT=OPTION("OUTPUT")
     QUIT
     ;
QUERY    ;
     N IMAGE,MODE,X
     S X=$$ASK W ! 
     I X=-1 Q
     S IMAGE=$P(X,"~",2),MODE=+X 
     D WORK(IMAGE,MODE)
     Q
IMAGE  ;
     N IMAGE,MODE
     S IMAGE=1,MODE=0 
     D WORK(IMAGE,MODE)
     Q
WORK(IMAGE,MODE,OPTION)  ;"Main driver, Will release lock
     ;"Input: IMAGE: 1-> sorted by image, 0-> sorted by PID
     ;"       MODE: 1->CTIME, 0->PID  (default = 0)
     ;"       OPTION: OPTIONAL.  
     ;"          OPTION("QUIET") = 1  <-- OPTIONAL. If set, then nothing put to console
     N NOPRIV,LOCK,PID,ACCESS,USERS,CTIME,GROUP,JTYPE,LTIME,MEMBER,PROCID
     N TNAME,UNAME,INAME,I,SORT,OLDPRIV,TAB
     N $ES,$ET,STATE,%PS,RTN,%OS,%T,SYSNAME,OLDINT,DONE 
     NEW QUIET SET QUIET=+$GET(OPTION("QUIET")) 
     LOCK +^XUTL("XUSYS","COMMAND"):1 I '$T G LW
     ;"Save $ZINTERRUPT, set new one
     ;"SET OLDINT=$ZINTERRUPT
     ;"SET $ZINTERRUPT="I $$JOBEXAM^ZU($ZPOSITION) S DONE=1"
     ;"%os = 1 for VMS, 0 = Linux.
     ;"SET %OS=$ZV["VMS"
     ;"SET $ET="D ERR^TMGZSY"
     ;"Clear old data
     SET ^XUTL("XUSYS","COMMAND")="Status"
     SET I=0 
     FOR  S I=$O(^XUTL("XUSYS",I)) Q:'I  K ^XUTL("XUSYS",I,"JE"),^("INTERUPT")
     SET (LOCK,NOPRIV,USERS)=0
     USE $P:CTRAP=$C(3)
     ;"//kt  I %OS S %T=0 D  I %T D EXIT Q
     ;"//kt  . S OLDPRIV=$ZSETPRV("SYSLCK,GROUP,WORLD")
     ;"//kt  . I '$ZPRIV("SYSLCK") S %T=1 W !,"You need SYSLCK privilege to run this program.",!
     ;"//kt  . Q
     ;Go get the data
     ;"//kt  I %OS D VMS(.USERS)
     ;"IF '%OS DO
     ;". D UNIX(.USERS,.SORT,.ACCESS)
     D UNIX(.USERS,.SORT,.ACCESS)
     ;"Now show the results
     IF 'QUIET DO
     . IF USERS D
     . . DO HEADER
     . . DO ISHOW(.SORT,.ACCESS,.STATE,.PROCID):IMAGE  
     . . DO USHOW(.SORT):'IMAGE
     . . W !,$J," -- This process (self)",!  ;//kt
     . . W !!,"Total ",USERS," other GT.M user",$S(USERS>1:"s.",1:"."),!   ;"//kt added 'other GT.M'
     . . Q
     . E  W !,"No current GT.M users.",!
     ELSE  DO
     . NEW TEMP DO GETDATA(.TEMP,.SORT,.ACCESS,.STATE,.PROCID)   ;"Put output into array, sorted by PID
     . MERGE OPTION("OUTPUT")=TEMP
     ;"//kt I NOPRIV W !,"Insufficient privileges to examine ",NOPRIV," process",$S(NOPRIV>1:"es.",1:"."),!
EXIT     ;
     L -^XUTL("XUSYS","COMMAND") ;Release lock and let others in
     ;"//kt  I %OS S:$D(OLDPRIV) OLDPRIV=$ZSETPRV(OLDPRIV)
     ;"IF $L($G(OLDINT)) S $ZINTERRUPT=OLDINT
     USE $P:CTRAP=""                           
     QUIT
     ;
ERR  ;
     USE $P 
     W !,$P($ZS,",",2,99),!
     D EXIT
     Q
     ;
LW   ;Lock wait
     W !,"Someone else is running the System status now."
     Q
     ;
  ;"//kt  VMS(USERS)     ;Collect VMS process info                         
  ;"//kt       S $ET="D VERR^ZSY"
  ;"//kt       S SYSNAME="SYSNAME"
  ;"//kt       S ACCESS(0)="Detach",ACCESS(1)="Network",ACCESS(2)="Batch",ACCESS(3)="Local",ACCESS(4)="Dialup",ACCESS(5)="Remote"
  ;"//kt       S STATE(5)="LEF",STATE(7)="HIB",STATE(12)="COM",STATE(14)="CUR"
  ;"//kt       S LOCK=$ZLKID(0)
  ;"//kt       I LOCK D  F  S LOCK=$ZLKID(1) Q:'LOCK  D
  ;"//kt       . I $EXTRACT($ZGETLKI(LOCK,"RESNAM"),1,6)="GTM$LM" S PID=$ZGETLKI(LOCK,"PID") D GETJOB(PID,.SORT) W "."
  ;"//kt       S USERS=USERS+NOPRIV
  ;"//kt       Q
     ;
HEADER     ;Display Header
     W # S ($X,$Y)=0
     S TAB(1)=9,TAB(2)=25,TAB(3)=29,TAB(4)=38,TAB(5)=57,TAB(6)=66
     W !,"GT.M Mumps users on ",$$DATETIME($H),!
     W !,"Proc. id",?TAB(1),"Proc. name",?TAB(2),"PS",?TAB(3),"Device",?TAB(4),"Routine",?TAB(5),"MODE",?TAB(6),"CPU time"
     W !,"--------",?TAB(1),"---------------",?TAB(2),"---",?TAB(3),"--------",?TAB(4),"--------",?TAB(5),"-------",?TAB(6)
     Q
GETDATA(OUT,SORT,ACCESS,STATE,PROCID)   ;"Put output into array, sorted by PID                      
     NEW SI,DI,X,EXIT,DEV,IDX
     SET SI="",EXIT=0,IDX=1
     FOR  SET SI=$ORDER(SORT(SI)) Q:SI=""!EXIT  F I=1:1:SORT(SI) D  Q:EXIT
     . SET X=SORT(SI,I)
     . SET TNAME=$P(X,"~",4)
     . SET PROCID=$P(X,"~",1)
     . SET JTYPE=$P(X,"~",5)
     . SET CTIME=$P(X,"~",6)
     . SET LTIME=$P(X,"~",7)
     . SET PS=$P(X,"~",3)
     . SET PID=$P(X,"~",8)
     . SET UNAME=$P(X,"~",2)
     . SET RTN=$G(^XUTL("XUSYS",PID,"INTERRUPT"))
     . SET OUT(IDX,"PROCID")=PROCID
     . SET OUT(IDX,"PROCNAME")=UNAME
     . SET OUT(IDX,"PS")=$G(STATE(PS),PS)
     . SET OUT(IDX,"DEVICE")=TNAME
     . SET OUT(IDX,"RTN")=RTN
     . SET OUT(IDX,"MODE")=ACCESS(JTYPE)
     . SET OUT(IDX,"CPU TIME")=CTIME
     . K DEV
     . F DI=1:1 Q:'$D(^XUTL("XUSYS",PID,"JE","D",DI))  DO
     . . S X=^(DI)
     . . S X=$P(X,":")_":" 
     . . I $TR(X,"_")'=TNAME S DEV(DI)=X
     . MERGE OUT(IDX,"DEV")=DEV
     . MERGE OUT(IDX,"ENV")=SORT(SI,"KV")
     . SET IDX=IDX+1
     QUIT
     ;
USHOW(SORT,ACCESS,STATE,PROCID)   ;Display job info, sorted by pid                         
     N SI,X,EXIT,DEV
     S SI="",EXIT=0
     F  S SI=$ORDER(SORT(SI)) Q:SI=""!EXIT  F I=1:1:SORT(SI) D  Q:EXIT
     . S X=SORT(SI,I)
     . S TNAME=$P(X,"~",4)
     . S PROCID=$P(X,"~",1)
     . S JTYPE=$P(X,"~",5)
     . S CTIME=$P(X,"~",6)
     . S LTIME=$P(X,"~",7)
     . S PS=$P(X,"~",3)
     . S PID=$P(X,"~",8)
     . S UNAME=$P(X,"~",2)
     . NEW AC SET AC=$GET(ACCESS(JTYPE))
     . S RTN=$G(^XUTL("XUSYS",PID,"INTERRUPT"))
     . W !,PROCID,?TAB(1),UNAME,?TAB(2),$G(STATE(PS),PS),?TAB(3),TNAME,?TAB(4),RTN,?TAB(5),AC,?TAB(6),$J(CTIME,6)
     . K DEV
     . F DI=1:1 Q:'$D(^XUTL("XUSYS",PID,"JE","D",DI))  S X=^(DI),X=$P(X,":")_":" I $TR(X,"_")'=TNAME S DEV(DI)=X
     . S DI=0 F  S DI=$O(DEV(DI)) Q:DI'>0  W !,?TAB(3),$E(DEV(DI),1,79-$X)
     . I $Y>22 D WAIT
     Q
ISHOW(SORT,ACCESS,STATE,PROCID)     ;Show process sorted by IMAGE
     NEW SI,X,INAME
     SET INAME="",EXIT=0
     FOR  SET INAME=$ORDER(SORT(INAME)) Q:INAME=""!EXIT  D
     . WRITE !,"IMAGE  : ",INAME 
     . SET SI=""
     . FOR  SET SI=$ORDER(SORT(INAME,SI)) Q:SI=""!EXIT  F I=1:1:SORT(INAME,SI) D  Q:EXIT
     . . S X=SORT(INAME,SI,I)
     . . S TNAME=$P(X,"~",4)
     . . S PROCID=$P(X,"~",1)
     . . S PS=$P(X,"~",3)
     . . S RTN=$P(X,"~",8)
     . . S JTYPE=$P(X,"~",5)
     . . S CTIME=$P(X,"~",6)
     . . S LTIME=$P(X,"~",7)
     . . S UNAME=$P(X,"~",2)
     . . NEW AC SET AC=$GET(ACCESS(JTYPE))
     . . S RTN=$G(^XUTL("XUSYS",RTN,"INTERRUPT"))
     . . W !,PROCID,?TAB(1),UNAME,?TAB(2),$G(STATE(PS),PS),?TAB(3),TNAME,?TAB(4),RTN,?TAB(5),AC,?TAB(6),CTIME
     . . I $Y>22 D WAIT
     . W !
     Q
     ;
WAIT     ;Page break
     N Y
     S Y=0 W !,"Press Return to continue '^' to stop: " R Y:300
     I $E(Y)="^" S EXIT=1
     E  D HEADER
     Q
     ;
  ;"//kt  GETJOB(PID,SORT)   ;Get data from a VMS job
  ;"//kt       N NM,SI,$ET,$ES
  ;"//kt       S $ET="G BLINDPID"
  ;"//kt       S PROCID=$$FUNC^%DH(PID,8),TNAME=$ZGETJPI(PID,"TERMINAL")
  ;"//kt       ZSYSTEM "@gtm$dist:mupip-intrpt.com "_PROCID ;"MUPIP INTRPT /ID="_procid
  ;"//kt       S NM=$ZGETJPI(PID,"PRCNAM")
  ;"//kt       S UNAME=$G(^XUTL("XUSYS",PID,"NM"),NM)
  ;"//kt       S JTYPE=$ZGETJPI(PID,"JOBTYPE"),PS=$ZGETJPI(PID,"STATE")
  ;"//kt       ;S RTN=PID ;$G(^XUTL("XUSYS",PID,"INTERRUPT"))
  ;"//kt       S LTIME=$$DATETIME($ZGETJPI(PID,"LOGINTIM")),CTIME=$$CPUTIME($ZGETJPI(PID,"CPUTIM"))
  ;"//kt       S SI=$S(MODE=1:CTIME,1:PID)
  ;"//kt       I IMAGE D
  ;"//kt       . S INAME=$ZGETJPI(PID,"IMAGNAME"),I=$GET(SORT(INAME,SI))+1,SORT(INAME,SI)=I
  ;"//kt       . S SORT(INAME,SI,I)=PROCID_"~"_UNAME_"~"_PS_"~"_TNAME_"~"_JTYPE_"~"_CTIME_"~"_LTIME_"~"_PID_"~"_INAME
  ;"//kt       E  S I=$GET(SORT(SI))+1,SORT(SI)=I,SORT(SI,I)=PROCID_"~"_UNAME_"~"_PS_"~"_TNAME_"~"_JTYPE_"~"_CTIME_"~"_LTIME_"~"_PID
  ;"//kt       S USERS=USERS+1
  ;"//kt       Q
     ;
DATETIME(HOROLOG)     ;
     Q $ZDATE(HOROLOG,"DD-MON-YY 24:60:SS","Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec")
     ;
  ;"//kt CPUTIME(S)     ;
  ;"//kt      N T,S,M,H,D
  ;"//kt      S T=S#100,S=S\100 S:$L(T)=1 T="0"_T
  ;"//kt      S S=S#60,S=S\60 S:$L(S)=1 S="0"_S
  ;"//kt      S M=S#60,S=S\60 S:$L(M)=1 M="0"_M
  ;"//kt      S H=S#24,D=S\24 S:$L(H)=1 H="0"_H
  ;"//kt      Q D_" "_H_":"_M_":"_S_"."_T
  ;"//kt      ;
  ;"//kt BLINDPID     ;
  ;"//kt      N ZE S ZE=$ZS,$EC=""
  ;"//kt      I ZE["NOPRIV" S NOPRIV=NOPRIV+1
  ;"//kt      Q
  ;"//kt  VERR     W !,"lock = ",LOCK,!
  ;"//kt       W !,$P(ZE,",",2,99),! U $P:CTRAP="" S:$D(OLDPRIV) OLDPRIV=$ZSETPRV(OLDPRIV)
  ;"//kt       Q
ASK()     ;Ask sort item
     N RES,X,GROUP
     S RES=0,GROUP=2
     W !,"1 pid",!,"2 cpu time",!,"3 IMAGE/pid",!,"4 IMAGE/cpu"
     F  R !,"1// ",X:600 S:X="" X=1 Q:X["^"  Q:(X>0)&(X<5)  W " not valid"
     Q:X["^" -1
     S X=X-1,RES=(X#GROUP)_"~"_(X\GROUP)
     Q RES
     ;
UNIX(USERS,SORT,ACCESS)   ;"PUG/TOAD - Kernel System Status Report for GT.M
     ;"Input: USERS
     ;"       SORT
     ;"       ACCESS
     ;"S $ZT="ZG "_$ZL_":UERR^ZSY"
     N IDX,%FILE,%LINE,%TEXT,%I,U,%J,STATE,$ET,$ES
     ;"S $ET="D UERR^TMGZSY"
     S %FILE="/tmp/_gtm_sy_"_$J_".tmp"
     ;"ZSYSTEM "ps ef -C mumps>"_%FILE
     ZSYSTEM "ps eo pid,tty,stat,time,cmd -C mumps>"_%FILE
     NEW HFSOPT SET HFSOPT("OVERFLOW")=1
     NEW RESULT,DATA SET RESULT=$$HFS2ARFP^TMGIOUT3(%FILE,"DATA",.HFSOPT)
     SET %I=$I,U="^"
     ;
     SET IDX=0 FOR  SET IDX=$ORDER(DATA(IDX)) QUIT:IDX'>0  DO
     . SET %TEXT=$GET(DATA(IDX))
     . SET %LINE=$$VPE(%TEXT," ",U) ; parse each line of the ps output
     . Q:$P(%LINE,U)="PID"  ; header line
     . Q:$P(%LINE,U)=$J  ;"//kt don't interrupt self. 
     . NEW SORTIDX
     . D JOBSET(%LINE,.SORT,.USERS,.ACCESS,.SORTIDX)
     . NEW CT,S1,S2,S3 SET S1=$PIECE(%TEXT,"=",1),S2=$PIECE(%TEXT,"=",2,99999)
     . SET CT=$LENGTH(S1," ")
     . SET S1=$PIECE(S1," ",CT)
     . SET S3=S1_"="_S2
     . NEW TEMP DO SPLITKVSTR(S3,.TEMP)
     . MERGE SORT(SORTIDX,"KV")=TEMP("KV")
     ;"  OPEN %FILE:(readonly)
     ;"  ;
     ;"  ; Get lines of text from temp file
     ;"  USE %FILE 
     ;"  FOR  R %TEXT Q:%TEXT=""  D
     ;"  . S %LINE=$$VPE(%TEXT," ",U) ; parse each line of the ps output
     ;"  . Q:$P(%LINE,U)="PID"  ; header line
     ;"  . Q:$P(%LINE,U)=$J  ;"//kt don't interrupt self. 
     ;"  . D JOBSET(%LINE,.SORT,.USERS,.ACCESS)
     ;"  ;
     ;"  USE %I 
     ;"  CLOSE %FILE:DELETE
     IF $$DELFILE^TMGIOUTL(%FILE)  ;"ignore result
     QUIT
     ;
UERR     ;Linux Error
     N ZE S ZE=$ZS,$EC="" U $P
     ZSHOW "*"
     Q  ;halt
     ;
JOBSET(%LINE,SORT,USERS,ACCESS,IDX)  ;"Get data from a Linux job
     ;"INPUT: $LINE --
     ;"       SORT -- PASS BY REFERENCE  
     ;"       USERS -- PASS BY REFERENCE  
     ;"       ACCESS -- PASS BY REFERENCE
     S (IMAGE,INAME,UNAME,PS,TNAME,JTYPE,CTIME,LTIME,RTN)=""
     SET IDX=-1
     S (%J,PID,PROCID)=$P(%LINE,U)  
     IF %J'>0 QUIT
     S TNAME=$P(%LINE,U,2) S:TNAME="?" TNAME="" ;" TTY, ? if none
     S PS=$P(%LINE,U,3) ;" process STATE               
     S PS=$S(PS="D":"lef",PS="R":"com",PS="S":"hib",1:PS)
     S CTIME=$P(%LINE,U,4) ;"cpu time
     S JTYPE=$P(%LINE,U,6)
     S ACCESS(JTYPE)=JTYPE
     ;
     ZSYSTEM "mupip intrpt "_%J_" 2> /dev/null"
     S UNAME=$G(^XUTL("XUSYS",%J,"NM"))
     S RTN="" ;" Routine, get at display time
     S SI=$S(MODE=0:PID,MODE=1:CTIME,1:PID)
     I IMAGE D
     . SET INAME="mumps"
     . SET I=$GET(SORT(INAME,SI))+1
     . SET SORT(INAME,SI)=I
     . S SORT(INAME,SI,I)=PROCID_"~"_UNAME_"~"_PS_"~"_TNAME_"~"_JTYPE_"~"_CTIME_"~"_LTIME_"~"_PID_"~"_INAME
     E  DO
     . SET I=$GET(SORT(SI))+1
     . SET SORT(SI)=I
     . SET SORT(SI,I)=PROCID_"~"_UNAME_"~"_PS_"~"_TNAME_"~"_JTYPE_"~"_CTIME_"~"_LTIME_"~"_PID
     S USERS=USERS+1
     SET IDX=SI
     Q
     ;
VPE(%OLDSTR,%OLDDEL,%NEWDEL)     ; $PIECE extract based on variable length delimiter
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
     Q %NEWSTR
     ;
INTRPT     ;List jobs that set INTRUPT.
     N J
     S J=0
     F  S J=$O(^XUTL("XUSYS",J)) Q:J'>0  S X=$G(^XUTL("XUSYS",J,"INTERRUPT")) I $L(X) W !,J,?10,X
     Q
     ;
SPLITKVSTR(STR,OUT)  ;"Split a string that is a long Key=Value array, separated by spaces
     NEW KEY,VAL,IDX SET IDX=1
     FOR  DO  QUIT:STR="" 
     . DO SPLIT1(.STR,.KEY,.VAL)
     . SET OUT(IDX)=KEY_"="_VAL
     . SET IDX=IDX+1
     . SET OUT("KV",KEY)=VAL
     QUIT
     ;
SPLIT1(STR,OUTKEY,OUTVAL)  ;"e.g.  KEY1=SOME LONG VALUE KEY2=SOME OTHER VALUE2 KEY3=YET ANOTHER VALUE3....
     SET OUTKEY=$PIECE(STR,"=",1)   ;"e.g. KEY1
     NEW CT,S1,AFTEREQ,BEFORENEXTEQ,AFTERNEXTEQ,NEXTKEY
     SET AFTEREQ=$PIECE(STR,"=",2,99999)  ;"e.g. SOME LONG VALUE KEY2=SOME OTHER VALUE2 KEY3=YET ANOTHER VALUE3....
     IF AFTEREQ["=" DO
     . SET BEFORENEXTEQ=$PIECE(AFTEREQ,"=",1)  ;"e.g.  SOME LONG VALUE KEY2  <-- note: last word is next key
     . SET AFTERNEXTEQ=$PIECE(AFTEREQ,"=",2,99999)  ;"e.g. SOME OTHER VALUE2 KEY3=YET ANOTHER VALUE3....
     . SET CT=$LENGTH(BEFORENEXTEQ," ")
     . NEW NEXTKEY SET NEXTKEY=$PIECE(BEFORENEXTEQ," ",CT)  ;"e.g. KEY2
     . SET OUTVAL=$PIECE(BEFORENEXTEQ," ",1,CT-1)  ;"E.G. SOME LONG VALUE 
     . SET STR=NEXTKEY_"="_AFTERNEXTEQ
     ELSE  DO  ;"no further key/vlue pairs
     . SET OUTVAL=AFTEREQ
     . SET STR=""
     IF STR="=" SET STR=""
     QUIT
     ;
     ;"=======================================================================================
     ;"=======================================================================================
     