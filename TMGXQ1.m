TMGXQ1	; TMG - DRIVER FOR MENUMAN (PART 2) ;07/26/22
	;;8.0;KERNEL;**1,15,59,67,46,151,170,242,446**;Jul 26, 2022
 ;
 ;"TMG INTERFACE TO MENU MANAGER API FUNCTIONS
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 7/27/2022  Kevin S. Toppenberg MD
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
 ;"Private Functions
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"=======================================================================
 ;
ENTER(MENU,OPTION)  ;
  ;"Purpose: Enter Menu Manager 
  ;"Input:   MENU -- either a name of OPTION to enter, or IEN of OPTION
  ;"         OPTION -- Optional.  PASS BY REFERENCE.  
  ;"            OPTION("KEEP STACK")=1  If 1, then prior MENU stack is kept.
  ;"                                    NOTE: the menu system only returns here after
  ;"                                    all menus in stack have been exited.  
  ;"Result: none
  ;"Routine is copied and modified from /r/XQ1.m
  ;
  NEW KEEPSTACK SET KEEPSTACK=+$GET(OPTION("KEEP STACK"))
  IF 'KEEPSTACK DO  ;"backup stack and then kill it. 
  . KILL ^TMP("TMG ENTER TMGXQ1",$J)
  . MERGE ^TMP("TMG ENTER TMGXQ1",$J)=^XUTL("XQ",$J)
  . KILL ^XUTL("XQ",$J)
  . SET ^XUTL("XQ",$J,0)=$GET(^TMP("TMG ENTER TMGXQ1",$J,0))
  SET MENU=$GET(MENU)
  NEW IEN SET IEN=+MENU
  IF +MENU'=MENU,MENU'="" DO
  . SET DIC=19,DIC(0)="M",X=MENU DO ^DIC 
  . IF Y'<0 SET IEN=+Y
  IF +IEN'>0 QUIT
  SET (XQDIC,XQY)=+IEN
  KILL DIC,XQUR,Y,^VA(200,DUZ,202.1)  
  NEW TMGXUSCNOHALT SET TMGXUSCNOHALT=1  ;"used in custom version of XUSCLEAN.m
  NEW TMGXQ12QT SET TMGXQ12QT=1          ;"used in custom version of XQ12.m
  ;"Line below not needed if using custom XUSCLEAN and XQ12, but will leave in. 
  SET $P(XQXFLG,U,3)="XUP"
  DO INIT^XQ12
  DO
  . GOTO M^XQ  ;"<-- QUIT from here, will quit this DO block. 
  IF 'KEEPSTACK DO  ;"restore prior stack.  
  . KILL ^XUTL("XQ",$J)
  . MERGE ^XUTL("XQ",$J)=^TMP("TMG ENTER TMGXQ1",$J)
  . KILL ^TMP("TMG ENTER TMGXQ1",$J)
  QUIT
  ;