TMGGI ;TMG/kst/ GT.M GI ;7/14/15
         ;;1.0;TMG-LIB;**1**;7/14/15 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                ;
;    Copyright 1991, 2006 Fidelity Information Services, Inc    ;
;                                ;
;    This source code contains the intellectual property    ;
;    of its copyright holder(s), and is made available    ;
;    under a license.  If you do not know the terms of    ;
;    the license, please stop and do not read further.    ;
;                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;
 ;//kt Modified for file input dialog.  Additional version: 7/14/15
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Changes Copyright (c) 7/14/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;
%GI ;service@greystone.com %GO;19920722 21:35;global input
    ;Load globals into database
    ;Possible enhancements:
    ;selection and/or exclusion by key list, range and/or wildcard
    ;optional confirmation by global name
    ;callable entry point
    ;
    WRITE !,"Global Input Utility",!
    DO PRESS2GO^TMGUSRI2
    IF '$DATA(%zdebug) NEW $et SET $et="zg "_$zl_":ERR^%GI" USE $P:(ctrap=$c(3):exc="zg "_$zl_":EXIT^TMGGI")
    NEW D,NUMGBLS,NUMNODES,SAVED,X,Y,%ZD,ZFORMAT,CTLCHARS
    NEW FSIZE SET FSIZE=0
    SET CTLCHARS="" FOR D=1:1:31,127 SET CTLCHARS=CTLCHARS_$CHAR(D)
    FOR  DO  QUIT:$LENGTH(%ZD)
    . ;"READ !,"Input device: <terminal>: ",%ZD,!
    . SET %ZD=$$GETFNAME^TMGIOUTL("Pick input file.")                 ;//kt
    . ;"IF '$LENGTH(%ZD) SET %ZD=$P QUIT
    . IF '$LENGTH(%ZD) SET %ZD="^"
    . IF %ZD="^" QUIT
    . NEW % SET %=1 WRITE "Ensure input file is in Unix format" DO YN^DICN WRITE !
    . IF %=-1 SET %ZD="^" QUIT
    . IF %=1 IF $$Dos2Unix^TMGKERNL(%ZD)
    . IF %ZD="?" DO  QUIT
    . . WRITE !!,"Select the device you want for input"
    . . WRITE !,"If you wish to exit enter a caret (^)",!
    . . SET %ZD=""
    . IF $zparse(%ZD)="" WRITE "  no such device" SET %ZD="" QUIT
    . SET FSIZE=$$FileSize^TMGKERNL(%ZD)
    . WRITE "Opening file ",%ZD," (",FSIZE," bytes) ..."
    . OPEN %ZD:(readonly:block=2048:record=2044:exception="GOTO EX"):0
    . WRITE !
    . IF '$T  WRITE !,%ZD," is not available" SET %ZD="" QUIT
    . QUIT
EX  . WRITE !,$P($ZS,",",2,999),! 
    . CLOSE %ZD SET %ZD=""
    QUIT:%ZD="^"
    WRITE !!
    SET SAVED="",(NUMGBLS,NUMNODES)=0
    USE %ZD:exception="GOTO EOF^TMGGI"
    READ X,Y USE $P WRITE !,X,!,Y,!!
    USE $P 
    ;"READ !,"OK <Yes>? ",X,!!
    ;"IF $LENGTH(X),$EXTRACT("NO",1,$LENGTH(X))=$TRANSLATE(X,"no","NO") GOTO EXIT
    NEW % SET %=1 WRITE "OK" DO YN^DICN WRITE !
    IF %'=1 GOTO EXIT
    SET ZFORMAT=Y["ZWR"
    NEW BYTES SET BYTES=0
    NEW STARTH SET STARTH=$H
    NEW LASTH SET LASTH=$H
    NEW DONE SET DONE=0
    FOR  DO  QUIT:DONE
    . IF (ZFORMAT) DO
    . . USE %ZD READ X  
    . . IF X="" SET DONE=1 QUIT
    . . SET BYTES=BYTES+$LENGTH(X)
    . . SET @X
    . . SET NUMNODES=NUMNODES+1
    . . SET X=$PIECE($PIECE(X,"="),"(")  
    . . IF X'=SAVED,X'="^" DO
    . . . SET NUMGBLS=NUMGBLS+1,SAVED=X
    . . . USE $P 
    . . . ;"WRITE:$X>70 ! 
    . . . ;"WRITE X,?$X\10+1*10
    . . . WRITE !,X,!
    . IF ('ZFORMAT) DO
    . . USE %ZD READ X,Y 
    . . SET BYTES=BYTES+$LENGTH(X)+$LENGTH(Y)
    . . IF "*"[$EXTRACT(X) SET DONE=1 QUIT 
    . . IF $TRANSLATE(X,CTLCHARS,"")'=X DO        ;"convert control chars to $C(X) exprs
    . . . NEW CHAR,CHARPOS,NEWX SET NEWX=""
    . . . FOR CHARPOS=1:1:$LENGTH(X) DO
    . . . . SET CHAR=$EXTRACT(X,CHARPOS)
    . . . . SET NEWX=NEWX_$SELECT(CTLCHARS[CHAR:"""_$C("_$ASCII(CHAR)_")_""",1:CHAR)
    . . . SET X=NEWX    ;"use fixed 'X'
    . . SET @X=Y
    . . SET NUMNODES=NUMNODES+1,X=$PIECE(X,"(")
    . . IF X'=SAVED,X'="^" DO
    . . . SET NUMGBLS=NUMGBLS+1,SAVED=X
    . . . USE $P 
    . . . ;"WRITE:$X>70 ! 
    . . . ;"WRITE X,?$X\10+1*10
    . . . WRITE !,X,!
    . IF $$HDIFF^XLFDT($H,LASTH,2)>5 DO  ;"Update progress Q 5 seconds
    . . SET LASTH=$H
    . . USE $P DO PROGBAR^TMGUSRI2(BYTES,"Progress",1,FSIZE,70,STARTH)
EOF USE $P
    WRITE !!,"Restored ",NUMNODES," node",$s(NUMNODES=1:"",1:"s")
    WRITE " in ",NUMGBLS," global",$s(NUMGBLS=1:".",1:"s.")
    CLOSE:%ZD'=$P %ZD USE $P:(ctrap="":exc="")
    QUIT
    ;
ERR USE $P WRITE !,$PIECE($zs,",",2,99),!
    ; Warning - Fall-though
    SET $ec=""
EXIT ;
    IF $DATA(%ZD),%ZD'=$P CLOSE %ZD
    USE $P:(ctrap="":exc="")
    QUIT
