TMGMKU ;TMG/kst/Custom version of ZTMKU ;03/25/06, 2/2/14
         ;;1.0;TMG-LIB;**1**;11/01/04

 ;"ZTMKU code -- NON-INTERACTIVE versions of standard code.
 ;"=============================================================================
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"
 ;"Purpose:
 ;"
 ;"This library will provide optional NON-INTERACTIVE versions of standard code.
 ;"
 ;"ZTMKU code
 ;"Apparent Callable points:
 ;"  (See below about optional "INFO" parameter)
 ;"        SSUB(NODE) ;Stop sub-managers
 ;"        SMAN(NODE) ;stop managers
 ;"        RUN(INFO) ;Remove Task Managers From WAIT State
 ;"        UPDATE(INFO) ;Have Managers Do an parameter Update
 ;"        WAIT(INFO) ;Put Task Managers In WAIT State
 ;"        STOP(INFO) ;Shut Down Task Managers
 ;"        QUERY ;Query Status Of A Task Manager
 ;"        NODES ;Return Task Manager Status Nodes
 ;"        LIVE ;Return Whether A Task Manager Is Live
 ;"        TABLE(INFO) ;Display Task Manager Table
 ;"        CLEAN(INFO) ;Cleanup Status Node
 ;"        PURGE(INFO) ;Purge the TASK list of running tasks.
 ;"        ZTM ;Return Number Of Live Task Managers
 ;"
 ;"Dependancies:
 ;"  IF TMGDEBUG defined, then requires TMGDEBUG.m
 ;"=============================================================================

ZTMKU ;SEA/RDS-Taskman: Option, ZTMWAIT/RUN/STOP ;11/04/99  15:05
 ;;8.0;KERNEL;**118,127,275**;Jul 10, 1995
 ;

 ;"K. Toppenberg's changes made November, 2004
 ;"
 ;"Input:
 ;"     Note: INFO variable is completely an OPTIONAL parameter.
 ;"                If not supplied, interactive mode used
 ;"        INFO("SILENT-OUTPUT") -- 1 = output is supressed.
 ;"        INFO("SILENT-INPUT") -- 1 = User-interactive input is supressed.
 ;"
 ;"        ** IF in SILENT-INPUT mode, THEN the following data should be supplied, IF the
 ;"                relevent function is being called.
 ;"     ----------------------
 ;"        INFO("CONTINUE") -- Should contain the answer the user would enter for question:
 ;"                Are you sure you want to stop TaskMan?
 ;"                Used in STOP^TMGMKU(INFO)
 ;"        INFO("SUBMANAGERS") -- Answer to: Should active submanagers shut down after finishing their current tasks?
 ;"                Used in STOP^TMGMKU(INFO)
 ;"Output:
 ;"        If in SILENT-OUTPUT mode, then output that would normally go to the screen, will be routed to this array
 ;"        NOTE: INFO SHOULD BE PASSED BY REFERENCE IF user wants this information passed back out.
 ;"        INFO("TEXT","LINES")=Number of output lines
 ;"        INFO("TEXT",1)= 1st output line
 ;"        INFO("TEXT",2)= 2nd output line, etc...
 ;
 ;
 Q

INIT
  IF $DATA(SILNTOUT)=0 KILL INFO("TEXT") ;//kt
  ;
  ;"Note: this establishes a variable with global-scope. ... And no one kills it...
  SET SILNTOUT=$GET(INFO("SILENT-OUTPUT"),0) ;//kt
  SET SILENTIN=$GET(INFO("SILENT-INPUT"),0) ;//KT
  ;
  QUIT


 ;
 ;"=============================================================================
SSUB(NODE) ;Stop sub-managers
 D SS(1,"SUB",NODE) Q
 ;"=============================================================================
SMAN(NODE) ;stop managers
 D SS(1,"MGR",NODE) Q
 ;
 ;"=============================================================================
SS(MD,GR,NODE) ;Set/clear STOP nodes.
 S GR=$G(GR,"MGR") S:"MGR_SUB_"'[GR GR="MGR"
 I MD=1 S ^%ZTSCH("STOP",GR,NODE)=$H D WS(0,GR)
 I MD=0 K ^%ZTSCH("STOP",GR,NODE)
 Q
 ;
 ;"=============================================================================
WS(MD,GR) ;Set/Clear Wait state
 S GR=$G(GR,"MGR") S:"MGR_SUB_"'[GR GR="MGR"
 I MD=1 S ^%ZTSCH("WAIT",GR)=$H ;set wait state
 I MD=0 K ^%ZTSCH("WAIT",GR) ;Clear wait
 Q
 ;
 ;"=============================================================================
GROUP(CALL) ;Do CALL for each node, use NODE as the parameter
 N J,ND,NODE
 F J=0:0 S J=$O(^%ZTSCH("STATUS",J)) Q:J=""  S ND=$G(^(J)),NODE=$P(ND,"^",3) D @CALL
 Q
 ;
 ;"=============================================================================
OPT(MD) ;Disable/Enable option prosessing
 I MD=1 S ^%ZTSCH("NO-OPTION")=""
 I MD=0 K ^%ZTSCH("NO-OPTION")
 Q
 ;
 ;"=============================================================================
RUN(INFO) ;Remove Task Managers From WAIT State
 D WS(0,"MGR"),WS(0,"SUB") K ^%ZTSCH("STOP")

 DO INIT
 DO OUTP^TMGQIO(SILNTOUT,"!","Done!","!")
 Q
 ;
 ;"=============================================================================
UPDATE(INFO) ;Have Managers Do an parameter Update
 K ^%ZTSCH("UPDATE")
 DO INIT
 DO OUTP^TMGQIO(SILNTOUT,"!","Done!","!")
 Q
 ;
 ;"=============================================================================
WAIT(INFO) ;Put Task Managers In WAIT State
 DO INIT
 D WS(1,"MGR")
 DO OUTP^TMGQIO(SILNTOUT,"!","TaskMan now in 'WAIT STATE'",$C(7),"!")
 D QSUB
 Q
 ;
 ;"=============================================================================
STOP(INFO) ;Shut Down Task Managers
 DO INIT
 N ZTX,ND,J
 DO INIT
 F  DO  Q:'$T!("^YESyesNOno"[ZTX)!(SILENTIN=1)
 . DO OUTP^TMGQIO(SILNTOUT,"!","!","Are you sure you want to stop TaskMan? NO// ")
 . IF $DATA(TMGDEBUG) DO DEBUGMSG^TMGDEBU4(.DBINDENT,"Starting Question Loop")
 . DO INP^TMGQIO(.ZTX,SILENTIN,$G(DTIME,60),$GET(INFO("CONTINUE")))
 . IF $GET(ZTX)="" SET ZTX="NO"
 . Q:'$T!("^YESyesNOno"[ZTX)!(SILENTIN=1)
 . IF ZTX'["?" DO OUTP^TMGQIO(SILNTOUT,$C(7))
 . DO OUTP^TMGQIO(SILNTOUT,"!","Answer YES to shut down all Task Managers on current the volume set.")
 IF $DATA(TMGDEBUG) DO DEBUGMSG^TMGDEBU4(.DBINDENT,"Processing input")
 I "YESyes"[ZTX DO
 . DO OUTP^TMGQIO(SILNTOUT,"!","Shutting down TaskMan.")
 . D GROUP("SMAN(NODE)")
 . ;"F J=0:0 S J=$O(^%ZTSCH("STATUS",J)) Q:J=""  S ND=$G(^(J)) D SMAN($P(ND,U,3))
 . ;"Q
 . D QSUB
 ELSE  DO
 . DO OUTP^TMGQIO(SILNTOUT,"!","TaskMan NOT shut down.")
 Q
 ;
 ;"=============================================================================
QSUB
 N ZTX,ND
 F  DO  Q:'$T!("^YESyesNOno"[ZTX)!(SILENTIN=1)
 . DO OUTP^TMGQIO(SILNTOUT,"!","!","Should active submanagers shut down after finishing their current tasks? NO// ")
 . IF $DATA(TMGDEBUG) DO DEBUGMSG^TMGDEBU4(.DBINDENT,"Auto answer=",$GET(INFO("SUBMANAGERS")))
 . DO INP^TMGQIO(.ZTX,SILENTIN,$S($D(DTIME)#2:DTIME,1:60),$GET(INFO("SUBMANAGERS")))
 . IF ZTX="" SET ZTX="NO"
 . Q:'$T!("^YESyesNOno"[ZTX)!(SILENTIN=1)
 . IF ZTX'["?" DO OUTP^TMGQIO(SILNTOUT,$C(7))
 . DO OUTP^TMGQIO(SILNTOUT,"!","Please answer YES or NO..")
 I "YESyes"[ZTX DO
 . DO GROUP("SSUB(NODE)")
 . DO OUTP^TMGQIO(SILNTOUT,"!","Okay!","!")
 Q
 ;
 ;"=============================================================================
QUERY ;Query Status Of A Task Manager
 Q:$D(%ZTX)[0  Q:%ZTX=""  S %ZTY=0
 I $D(^%ZTSCH("STATUS",%ZTX))#2 S %ZTY=^%ZTSCH("STATUS",%ZTX)
 K %ZTX Q
 ;
 ;"=============================================================================
NODES ;Return Task Manager Status Nodes
 S %ZTX="" F %ZTY=0:0 S %ZTX=$O(^%ZTSCH("STATUS",%ZTX)) Q:%ZTX=""  S %ZTY=%ZTY+1,%ZTY(%ZTY)=%ZTX
 K %ZTX Q
 ;
 ;"=============================================================================
LIVE ;Return Whether A Task Manager Is Live
 Q:$D(%ZTX)[0  Q:%ZTX=""  S %ZTY=0,U="^",%ZTX1=$H,%ZTX2=$P(%ZTX,U)
 S %ZTX3=%ZTX1-%ZTX2*86400+$P(%ZTX1,",",2)-$P(%ZTX2,",",2)
 I %ZTX3'<0 S %ZTY=$S($D(^%ZTSCH("RUN"))[0&(%ZTX'["WAIT"):0,%ZTX3<30:1,%ZTX3<120&(%ZTX["PAUSE"):1,1:0)
 K %ZTX,%ZTX1,%ZTX2,%ZTX3 Q
 ;
 ;"=============================================================================
TABLE(INFO) ;Display Task Manager Table
 DO INIT
 DO OUTP^TMGQIO(SILNTOUT,"!","NUMBER","?15","STATUS","?25","DESCRIPTION","?55","LAST UPDATED","?75","LIVE")
 DO OUTP^TMGQIO(SILNTOUT,"!","------","?15","------","?25","-----------","?55","------------","?75","----")
 D NODES S %ZTZ=%ZTY,%ZTZ1=0,U="^",%H=$H D YMD^%DTC S DT=X
 F %ZTI=1:1:%ZTZ DO
 . S %ZTX=%ZTY(%ZTI)
 . D QUERY
 . I %ZTY'=0 DO
 . . DO OUTP^TMGQIO(SILNTOUT,"!",%ZTY(%ZTI),"?15",$P(%ZTY,U,2),"?25",$P(%ZTY,U,3),"?55")
 . . S %ZTT=$P(%ZTY,U)
 . . D T
 . . S %ZTX=%ZTY
 . . D LIVE
 . . DO OUTP^TMGQIO(SILNTOUT,"?75",$S(%ZTY:"YES",1:"NO"))
 . . I %ZTY S %ZTZ1=%ZTZ1+1
 DO OUTP^TMGQIO(SILNTOUT,"!","?6","Total:",$J(%ZTZ,3),"!")
 DO OUTP^TMGQIO(SILNTOUT,"?6","Live :",$J(%ZTZ1,3))
 K %ZTI,%ZTT,%ZTY,%ZTZ
 Q
 ;
 ;
 ;"=============================================================================
CLEAN(INFO) ;Cleanup Status Node
 DO INIT
 K ^%ZTSCH("STATUS")
 DO OUTP^TMGQIO(SILNTOUT,"!","Done!","!")
 ;
 Q
 ;
 ;
 ;"=============================================================================
PURGE(INFO) ;Purge the TASK list of running tasks.
 DO INIT
 N TSK S TSK=0
 F  S TSK=$O(^%ZTSCH("TASK",TSK)) Q:TSK'>0  I '$D(^%ZTSCH("TASK",TSK,"P")) K ^%ZTSCH("TASK",TSK)
 DO OUTP^TMGQIO(SILNTOUT,"!","Done!","!")
 Q
 ;
 ;
 ;"=============================================================================
ZTM ;Return Number Of Live Task Managers
 D NODES S %ZTZ=%ZTY,%ZTZ1=0 F %ZTI=1:1:%ZTZ S %ZTX=%ZTY(%ZTI) D QUERY I %ZTY'=0 S %ZTX=%ZTY D LIVE I %ZTY S %ZTZ1=%ZTZ1+1
 S %ZTY=%ZTZ1 K %ZTI,%ZTZ,%ZTZ1 Q
 ;
 ;"=============================================================================
T ;Print Informal-format Conversion Of $H-format Date ; Input: %ZTT, DT.
 S %H=%ZTT
 D 7^%DTC
 DO OUTP^TMGQIO(SILNTOUT,$S(DT=X:"TODAY",DT+1=X:"TOMORROW",1:$E(X,4,5)_"/"_$E(X,6,7)_"/"_$E(X,2,3))_" AT ")
 S X=$P(%ZTT,",",2)\60
 S %H=X\60
 DO OUTP^TMGQIO(SILNTOUT,$E(%H+100,2,3)_":"_$E(X#60+100,2,3))
 K %,%D,%H,%M,%Y,X
 ;
 Q  ; Output: %ZTT, DT.
 ;
 ;"=============================================================================
