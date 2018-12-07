TMGKERN6 ;TMG/kst/FREECNT function ;12/8/14
         ;;1.0;TMG-LIB;**1**;12/5/14
 ;
 ;"TMG KERNEL FUNCTIONS
 ;"Code related to mumps database size.  Checking and adding....
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
%FREECNT(STORE,OUT)    ;GT.M %FREECNT utility - display database free blocks
        ;"Purpose: To display or store the free database blocks
        ;"Input: STORE -- boolean. 
        ;"              1 -will write information to the screen
        ;"              0 (default) -will store information to the variable assigned specified in OUT
        SET STORE=+$GET(STORE)
        SET OUT=$GET(OUT) 
        IF (STORE=0)&(OUT="") QUIT
        NEW RN,FN,FB,TB,%ZL
        IF '$DATA(%zdebug) NEW $ET SET $ET="zg "_$zl_":ERR^%FREECNT" USE $PRINCIPAL:(ctrap=$c(3):exc="zg "_$zl_":EXIT^%FREECNT")
        SET RN=$VIEW("GVFIRST")
        IF STORE=1 DO HEAD
        DO SHOW
        FOR  SET RN=$VIEW("gvnext",RN) QUIT:RN=""  DO SHOW
        DO EXIT
        QUIT
HEAD    ;
        WRITE "Region",?16,"Free",?25,"Total",?40,"Database file",!,"------",?16,"----",?25,"-----",?40,"-------------",!
        QUIT
SHOW    ;
        SET FN=$VIEW("GVFILE",RN),FB=$VIEW("FREEBLOCKS",RN),TB=$VIEW("TOTALBLOCKS",RN)
        IF STORE=1 WRITE RN,?12,$j(FB,8),?22,$j(TB,8)," (",$JUSTIFY(FB/TB*100.0,5,1),"%)",?40,FN,!
        ELSE  DO
        . SET @OUT@(RN,"FREE")=FB
        . SET @OUT@(RN,"TOTAL")=TB
        . SET @OUT@(RN,"PERCENT FREE")=$$TRIM^XLFSTR($JUSTIFY(FB/TB*100.0,5,1))
        . SET @OUT@(RN,"FILE")=FN
        QUIT
ERR     WRITE !,$PIECE($ZS,",",2,99),!
        SET $EC=""
        ; Warning: Fall-through
EXIT    USE $PRINCIPAL:(ctrap="":exc="")
        QUIT
        ;"
GETPCENT(PCENT,TOTBLOCKS)  ;"UPDATE PCENT & TOTBLOCKS (BOTH SHOULD BE SENT BY REFERENCE)
        NEW DBARRAY
        DO %FREECNT(0,"DBARRAY")
        NEW DB SET DB=""
        FOR  SET DB=$ORDER(DBARRAY(DB)) QUIT:DB=""  DO
        . SET PCENT=$GET(DBARRAY(DB,"PERCENT FREE"))
        . SET TOTBLOCKS=$GET(DBARRAY(DB,"TOTAL"))
        QUIT
        ;"
TMGFCENT
        ;"Purpose: To find free database blocks and create an alert if any
        ;"         are below 1%
        NEW DBARRAY
        DO %FREECNT(0,"DBARRAY")
        NEW DB SET DB=""
        FOR  SET DB=$ORDER(DBARRAY(DB)) QUIT:DB=""  DO
        . NEW PCENT SET PCENT=$GET(DBARRAY(DB,"PERCENT FREE"))
        . NEW TOTBLOCKS SET TOTBLOCKS=$GET(DBARRAY(DB,"TOTAL"))
        . IF PCENT>1 QUIT
        . SET XQADATA=DB_"^"_PCENT_"^"_TOTBLOCKS
        . DO SENDALERT(168,"DATABASE SIZE NEARING MAXIMUM BLOCKS",DB_" ONLY HAS "_PCENT_"% SPACE REMAINING. THERE ARE "_TOTBLOCKS_" TOTAL BLOCKS.","HNDLSIZ^TMGKERN6",XQADATA)
        QUIT
        ;"
HNDLSIZ
        ;"Purpose: Handle alerts, where free database blocks are nearly maximum allowed. 
        NEW DB,PCENT,TOTBLOCKS,DATA
        SET DATA=$GET(XQADATA)
        SET DB=$PIECE(DATA,"^",1)
        ;"SET PCENT=$PIECE(DATA,"^",2)
        ;"SET TOTBLOCKS=$PIECE(DATA,"^",3)
        DO GETPCENT(.PCENT,.TOTBLOCKS)
        WRITE !,"*****************************************************************************"
        WRITE !,DB_" ONLY HAS "_PCENT_"% SPACE REMAINING. THERE ARE "_TOTBLOCKS_" TOTAL BLOCKS.",!
        WRITE "*****************************************************************************",!
        NEW MENU,USERPICK,PERCENT
HDSZ1   KILL MENU
        SET MENU(0)="Select option for growing database"
        SET MENU(0,1)=DB_" HAS "_PCENT_"% SPACE REMAINING. "
        SET MENU(0,2)="THERE ARE "_TOTBLOCKS_" TOTAL BLOCKS."
        SET MENU(1)="Grow database by 10%"_$CHAR(9)_"GROW 10%"
        SET MENU(2)="Set size to 10% free blocks"_$CHAR(9)_"SET 10% FREE"
        SET MENU(3)="Enter a number of blocks to add"_$CHAR(9)_"ASK ADD"
        SET MENU(4)="Enter a % to grow database by"_$CHAR(9)_"ASK GROW %"
        SET MENU(5)="Enter a % to set for free space"_$CHAR(9)_"ASK % FREE"
        SET MENU(6)="Quit"_$CHAR(9)_"^"
        SET USERPICK=$$MENU^TMGUSRI2(.MENU,"^")
        IF USERPICK["^" GOTO HDSQ
        IF USERPICK="GROW 10%" DO
        . SET NUMBER=(TOTBLOCKS*.1)\1
        ELSE  IF USERPICK="SET 10% FREE" DO
        . SET PERCENT=(10-PCENT)*.01
        . SET NUMBER=TOTBLOCKS*PERCENT
        ELSE  IF USERPICK="ASK ADD" DO
        . READ !,"HOW MANY BLOCKS WOULD YOU LIKE TO ADD TO THIS DATABASE? >> ",NUMBER:$GET(DTIME,3600)
        . WRITE !
        . SET NUMBER=+$GET(NUMBER)        
        ELSE  IF USERPICK="ASK GROW %" DO 
        . READ !,"WHAT PERCENTAGE WOULD YOU LIKE TO GROW THIS DATABASE? >> ",PERCENT:$GET(DTIME,3600)
        . WRITE !
        . SET PERCENT=PERCENT*.01
        . SET NUMBER=TOTBLOCKS*PERCENT
        ELSE  IF USERPICK="ASK % FREE" DO 
        . READ !,"WHAT PERCENTAGE WOULD YOU LIKE THIS DATABASE TO HAVE FREE? >> ",PERCENT:$GET(DTIME,3600)
        . WRITE !
        . SET PERCENT=(PCENT-PERCENT)*.01
        . SET NUMBER=TOTBLOCKS*PERCENT
        IF NUMBER'>0 DO  
        . WRITE !,"NO SIZE INCREASE INDICATED."
        . WRITE "**NOTE: The size increase can be achieved manually by running: ",!
        . WRITE "  -> mupip extend -blocks=<# of blocks to increase> <Database name>",!
        ELSE  DO
        . DO DOADDBLK(NUMBER) ;"ADD BLOCKS TO DATABASE
        DO GETPCENT(.PCENT,.TOTBLOCKS)
        GOTO HDSZ1
HDSQ    ;        
        DO %FREECNT(1)   
        QUIT
        ;"
DOADDBLK(NUMBER) ;"ADD BLOCKS TO DATABASE
        NEW HOOKCMD SET HOOKCMD="mupip extend -blocks="_NUMBER_" "_DB
        ZSYSTEM HOOKCMD
        WRITE !   ;",$ZSYSTEM&255,!   ;"WRITE RESULT
        NEW OUT
        DO %FREECNT(1,.OUT)
        QUIT
        ;
SENDALERT(RECIPIENT,HEADING,MSG,HANDLER,DATA) ;"SEND ALERT
  ;"Input -- RECIPIENT -- IEN in NEW PERSON file
  ;"         HEADING -- The header message (show in CPRS)
  ;"         MSG -- The text to send
  ;"         HANDLER -- OPTIONAL.  Default is "HNDLERR^TMGSMS03"
  ;"         DATA
  NEW NOW SET NOW=+$TRANSLATE($H,",",".")
  FOR  QUIT:($DATA(^TMG("TMP","TMGKERN5",$J,NOW))=0)  SET NOW=NOW+0.00000001
  SET HANDLER=$GET(HANDLER,"HNDLERR^TMGSMS03")
  SET MSG=$GET(MSG) IF MSG="" GOTO SADN
  SET RECIPIENT=+$GET(RECIPIENT) IF (RECIPIENT'>0) GOTO SADN
  SET IENS=$GET(IENS)
  NEW XQA,XQAARCH,XQADATA,XQAFLG,XQAGUID,XQAID,XQAOPT,XQAROU,XQASUPV,XQASURO,XQATEXT
  SET XQADATA=DATA
  SET XQA(RECIPIENT)=""
  SET XQAID="TMG"
  SET XQAROU=HANDLER
  SET XQAMSG=HEADING
  SET ^TMG("TMP","TMGKERN5",$J,NOW,"MSG")=MSG
  NEW TEMP SET TEMP=$$SETUP1^XQALERT
SADN ;
  QUIT
    ;"
EXPBLOCK  ;"Expand block from Option
       WRITE !
       DO %FREECNT(1)
       NEW NUMBER
       READ !,"HOW MANY BLOCKS WOULD YOU LIKE TO ADD TO THIS DATABASE? >> ",NUMBER:$GET(DITIME,3600)
       SET NUMBER=+$GET(NUMBER)
       IF NUMBER'>0 WRITE !,"NO SIZE INCREASE INDICATED.",!,"**NOTE: The size increase can be achieved manually by running: ",!,"  -> mupip extend -blocks=<# of blocks to increase> <Database name>" QUIT
       WRITE !
       NEW HOOKCMD SET HOOKCMD="mupip extend -blocks="_NUMBER_" DEFAULT"
       ZSYSTEM HOOKCMD
       W !   ;",$ZSYSTEM&255,!   ;"WRITE RESULT
       DO %FREECNT(1)
       QUIT
       ;"