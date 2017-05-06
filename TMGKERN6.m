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
        SET PCENT=$PIECE(DATA,"^",2)
        SET TOTBLOCKS=$PIECE(DATA,"^",3)
        WRITE !,"*****************************************************************************"
        WRITE !,DB_" ONLY HAS "_PCENT_"% SPACE REMAINING. THERE ARE "_TOTBLOCKS_" TOTAL BLOCKS.",!
        WRITE "*****************************************************************************",!
        READ !,"HOW MANY BLOCKS WOULD YOU LIKE TO ADD TO THIS DATABASE? >> ",NUMBER:$GET(DTIME,3600)
        SET NUMBER=+$GET(NUMBER)
        IF NUMBER'>0 WRITE !,"NO SIZE INCREASE INDICATED.",!,"**NOTE: The size increase can be achieved manually by running: ",!,"  -> mupip extend -blocks=<# of blocks to increase> <Database name>" QUIT
        WRITE !
        NEW HOOKCMD SET HOOKCMD="mupip extend -blocks="_NUMBER_" "_DB
        ZSYSTEM HOOKCMD
        W !   ;",$ZSYSTEM&255,!   ;"WRITE RESULT
        DO %FREECNT(1)   
        QUIT
        ;"
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