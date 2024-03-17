TMGKJOB ;TMG/kst/ GT.M Job Exam as API ;1/18/15
         ;;1.0;TMG-LIB;**1**;1/18/15
 ;"
 ;"TMG JOB EXAM
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"NOTE: Significant portions of this code was copied from ZJOB, which
 ;"  is also released under GPL version 2+       
 ;"
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"ZJOB(PID,ACTION,OUT) -- API service to ^ZJOB functionality
 ;"
 ;"=======================================================================
 ;" API -- Private Functions.
 ;"=======================================================================
 ;"
 ;"=======================================================================
 ;"Dependencies
 ;"=======================================================================
 ;"ZJOB
 ;"=======================================================================
 ;"
 ;"NOTE:  See also TMGZJOB for graphical ^ZJOB interface
 ;
ZJOB(PID,ACTION,OUT) ;
  ;"Purpose: To provide API service to ^ZJOB functionality
  ;"Input: PID -- $JOB number of target session to query
  ;"       ACTION -- 'L' for get status information of target session
  ;"                 'S' get global System Status  --> LATER
  ;"                 'V' get local variables of target session
  ;"                 'K' send KILL command to other job --> LATER
  ;"                 NOTE: '*' command should not be used.
  ;"       OUT -- PASS BY REFERENCE, AN OUT PARAMETER.  Format:
  ;"         OUT("Intrinsic Variables",count)='<name>=<value>'
  ;"         OUT("Locks",count)='<value>'
  ;"         OUT("Stack",count)='<value>'
  ;"         OUT("Devices",count)='<value>'
  ;"         OUT("Variables",count)='<name>=<value>'
  ;"         OUT("INFO","JOB #")=<value>
  ;"         OUT("INFO","Process Name")=<value>
  ;"         OUT("INFO","Device")=<value>
  ;"         OUT("INFO","Process State")=<value>
  ;"         OUT("INFO","IMAGE")=<value>
  ;"         OUT("INFO","JOB Type")=<value>
  ;"         OUT("INFO","CPU Time")=<value>
  ;"         OUT("INFO","Login time")=<value>
  ;"         OUT("INFO","Routine line")=<value>
  ;"Result: none
  SET PID=+$GET(PID)
  NEW JOB SET JOB=PID
  IF '$$INTRPT(PID) QUIT
  DO DISPL(PID,.OUT) ;Show Header
  IF ACTION="V" DO DISPV(PID,.OUT) ;Show symbol table
  QUIT
  ;"
DISPV(PID,OUT) ;
  ;"INPUT: PID -- $JOB number of target session to query
  ;"       OUT -- PASS BY REFERENCE, FORMAT
  ;"         OUT("Intrinsic Variables",count)='<name>=<value>'
  ;"         OUT("Locks",count)='<value>'
  ;"         OUT("Stack",count)='<value>'
  ;"         OUT("Devices",count)='<value>'
  ;"         OUT("Variables",count)='<name>=<value>'
  ;"Result: none
  NEW S
  FOR S="Stack","Locks","Devices","Intrinsic Variables","Variables" DO
  . MERGE OUT(S)=^XUTL("XUSYS",PID,"JE",$E(S))
  QUIT 
  ;"
DISPL(PID,OUT)  ;
  ;"INPUT: OUT -- PASS BY REFERENCE, FORMAT
  ;"        OUT("INFO",<param>)=value
  ;"Result: none
  DO GETINFO^ZJOB
  SET HEXJOB="" IF $ZV["VMS" SET HEXJOB=$$HEX^ZJOB(PID)
  SET OUT("INFO","JOB #")=PID_" ("_HEXJOB_")"
  SET OUT("INFO","Process Name")=$G(^XUTL("XUSYS",PID,"NM"))
  SET OUT("INFO","Device")=$P($G(^XUTL("XUSYS",PID,"JE","D",1))," ")
  SET OUT("INFO","Process State")=PS
  SET OUT("INFO","IMAGE")=IMAGE_" ("_INAME_")"
  SET OUT("INFO","JOB Type")=JTYPE
  SET OUT("INFO","CPU Time")=CTIME
  SET OUT("INFO","Login time")=LTIME
  SET OUT("INFO","Routine line")="<"_$G(^XUTL("XUSYS",PID,"INTERRUPT"))_">"
  QUIT  ;"
  ;"
INTRPT(JOB) ;Send MUPIP intrpt
 N $ET,$ES S $ET="D IRTERR^ZJOB"
 ; shouldn't interrupt ourself
 I JOB=$JOB Q 0
 ;We need a LOCK to guarantee commands from two processes don't conflict
 N X,OLDINTRPT,TMP,ZSYSCMD,ZPATH,%J
 L +^XUTL("XUSYS","COMMAND"):10 Q:'$T 0
 ;
 S ^XUTL("XUSYS","COMMAND")="EXAM",^("COMMAND",0)=$J_":"_$H
 K ^XUTL("XUSYS",JOB,"JE")
 S OLDINTRP=$ZINTERRUPT,%J=$J
 S TMP=0,$ZINTERRUPT="S TMP=1"
 ;
 I $ZV["VMS" S JOB=$$HEX^ZJOB(JOB),%J=$$HEX^ZJOB(%J)
 S ZSYSCMD="mupip intrpt "_JOB ; interrupt other job
 I $ZV["VMS" S ZPATH="@gtm$dist:"  ; VMS path
 ;E  S ZPATH="sudo $gtm_dist" ;$ztrnlnm("gtm_dist") ;Unix path
 E  S ZPATH="$gtm_dist/" ;Unix path
 ;"W !,"Send intrp to job. Any error means you don't have the privilege to intrpt.",!
 ZSYSTEM ZPATH_ZSYSCMD ; System Request
 ;Now send to self
 ;
 ;ZSYSTEM ZPATH_"mupip intrpt "_%J
 ; wait is too long 60>>30
 H 1 S TMP=1
 ; wait for interrupt, will set TMP=1
 ;F X=1:1:30 H 1 Q:TMP=1  ;ZINTERRUPT does not stop a HANG
 ; Restore old $ZINTERRPT
 S $ZINTERRUPT=OLDINTRP
 K ^XUTL("XUSYS","COMMAND") ;Cleanup
 L -^XUTL("XUSYS","COMMAND")
 Q TMP  ;Report if we received interrupt
 ;
