TMGIDE5 ;TMG/kst/GT/M debugger Interrupt handler code ;6/27/12, 2/2/14
         ;;1.0;TMG-LIB;**1**;03/23/09
 ;
 ;" TMG IDE Debugger Interrupt handler code
 ;"
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"------------------------------------------------------------
 ;"PUBLIC API
 ;"------------------------------------------------------------
 ;"PICKINTR -- show currently running jobs, and allow user to start debugging them
 ;"INTERUPT -- respond to mupip intrpt, assigning control to a remote process

 ;"------------------------------------------------------------
 ;"PRIVATE API       
 ;"------------------------------------------------------------
 ;"LaunchIntr(JobNum) -- create interrupt message to job, then start listening
 ;"         for requests for control from interrupted process
 ;"------------------------------------------------------------
 ;"Dependencies
 ;"   TMGIDE4
 ;"   TMGKERNL
 ;"   %ZISUTL
 ;"   TMGUSRIF
 ;                                      
PICKINTR   ;
  ;"Purpose: To show currently running jobs, and allow user to start
  ;"         debugging them (tapping into process currently running)
  ;"Called from TMGIDE
  ;
  NEW array
  DO MJOBS^TMGKERNL(.array)
  KILL array($J)  ;"don't show this process
  NEW Menu,UsrSlct
  NEW i,j SET i="",j=1
  FOR  SET i=$ORDER(array(i)) QUIT:(i="")  do
  . SET Menu(j)="Job "_$GET(array(i))_$CHAR(9)_i
  . SET j=j+1
  IF $DATA(Menu)=0 GOTO PIDN
  SET Menu(0)="Pick Job to Debug"
  ;
M1  ;
  SET UsrSlct=$$MENU^TMGUSRI2(.Menu,"^")
  ;                            
  IF UsrSlct="^" GOTO PIDN
  IF UsrSlct=0 SET UsrSlct="" GOTO M1
  IF UsrSlct=+UsrSlct DO LaunchIntr(UsrSlct)
  GOTO M1
  ;
PIDN ;
  QUIT
  ;
LaunchIntr(JobNum)  ;
  ;"Purpose: To create interrupt message to job, then start listening
  ;"         for requests for control from interrupted process
  IF +$GET(JobNum)'>0 QUIT
  SET tmgDbgRemoteJob=JobNum
  SET ^XUTL("XUSYS","TMG COMMAND")="INTRPT"
  zsystem "mupip intrpt "_JobNum
  DO Controller^TMGIDE3 ;"launch the controller
  SET ^XUTL("XUSYS","TMG COMMAND")=""
  QUIT
  ;
JOBEXAM(%ZPOS)  ;"Called from JOBEXAM^ZU as part of yottadb zinterrup. 
  NEW TMGCMD SET TMGCMD=$P($G(^XUTL("XUSYS","TMG COMMAND"))," ",1)
  KILL ^XUTL("XUSYS","TMG COMMAND")
  ;"SET $ZTRAP="",TMGDEBUG=1 ;"temp
  IF TMGCMD="XECUTE" DO
  . NEW $ETRAP SET $ETRAP="SET $ETRAP="""",$ECODE="""""
  . NEW TMGMCODE SET TMGMCODE=$P($G(^XUTL("XUSYS","TMG COMMAND"))," ",2,999)
  . XECUTE TMGMCODE
  ELSE  IF TMGCMD="INTRPT" DO INTERUPT^TMGIDE5 GOTO JEDN
  SET ^XUTL("XUSYS",$J,"STATUS")="DONE"  ;"//message to sender of interrupt that we are done.  //kt added 3/8/24
JEDN ;
  QUIT
  ;
INTERUPT  ;
  ;"Purpose: To respond to mupip interrupt for a process, turning control
  ;"         over to a remote control process
  ;"NOTE: This will be called by a modified version of JOBEXAM^ZU
  IF $GET(TMGIDEDEBUG) WRITE !,"Sending INQ to connect to remote controller..."
  NEW TMGR SET TMGR=$$MessageOut^TMGIDE4("INQ "_$J,30)
  IF TMGR="" GOTO Int2
  IF $GET(TMGIDEDEBUG) do
  . WRITE !
  . WRITE "*****************************************************",!
  . WRITE "* INTERRUPT RECEIVED.  Transferring control to      *",!
  . WRITE "* a remote controller.  That is process ",$$LJ^XLFSTR($PIECE(TMGR," ",2),5),?52,"*",!
  . WRITE "*                                                   *",!
  . WRITE "* Please switch to that process window for control. *",!
  . WRITE "*****************************************************",!
  SET TMGR=$$MessageOut^TMGIDE4("WRITE Notice: Controlling interrupted job #"_$J,,0)
  SET $ZSTEP="N tmgTrap S tmgTrap=$$STEPTRAP^TMGIDE4($ZPOS) ZSTEP:(tmgTrap=1) into ZSTEP:(tmgTrap=2) over ZSTEP:(tmgTrap=3) outof zcontinue"
  SET TMGStepMode="into"
  ZSTEP into QUIT
Int2 ;
  QUIT
  ;
