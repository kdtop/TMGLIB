TMGRPCUT ;TMG/kst/RPC utility library ;5/14/12
         ;;1.0;TMG-LIB;**1**;5/14/12
 ;
 ;"TMG RPC UTILITY FUNCTIONS
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"ASKBRRPC ; --  Ask and then browse RPC information
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"BROWSRPC(IEN) -- display programming information about an RPC entry
 ;"DISP1(IEN,IDX,NAME) -- display information about a given parameter
 ;"RPCDESCR(IEN) --Show description for RPC entry
 ;"PAGEWP(TMGWP) --output WP array, with pause for page eveyr 10 pages.
 ;"DEBUGRPC(CODEREF,CODELINE,IEN,PARAMS) -- entry point to debug through RPC code
 ;"EDITPAR(IDX,PARAMS) -- edit the value of a parameters
 ;"RUNRPC(CODEREF,PARAMS) ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"      TMGUSRIF
 ;"      TMGVPE
 ;"      TMGDEBUG
 ;"      TMGIDE*
 ;"=======================================================================
 ;"=======================================================================
 ;
ASKBRRPC ;
        ;"Purpose: Ask and then browse RPC information
ABR1    WRITE !
        NEW DIC,X,Y
        SET DIC=8994,DIC(0)="MAEQ"
        DO ^DIC WRITE !
        IF +Y>0 DO BROWSRPC(+Y) GOTO ABR1
        QUIT
        ;
BROWSRPC(IEN) ;
        ;"Purpose: to display programming information about an RPC entry
        ;"Input: IEN -- record# in REMOTE PROCEDURE file (8994) to display
        ;"Results: none
        WRITE !,"-----REMOTE PROCEDURE CODE (RPC) EXPLORER-----",!,!
        SET IEN=+$GET(IEN) IF IEN'>0 GOTO BRWDN
        NEW MENU,USRPICK,I,TMGWP
        NEW ZN SET ZN=$GET(^XWB(8994,IEN,0))
        NEW RPCNAME SET RPCNAME=$PIECE(ZN,"^",1)
        NEW TAG SET TAG=$PIECE(ZN,"^",2)
        NEW ROUTINE SET ROUTINE=$PIECE(ZN,"^",3)
        NEW CODEREF SET CODEREF=TAG_"^"_ROUTINE
        NEW CODELINE SET CODELINE=$TEXT(@CODEREF)
        SET CODELINE=CODEREF_"("_$PIECE(CODELINE,"(",2,99)
        NEW COMMENT SET COMMENT=$$TRIM^XLFSTR($PIECE(CODELINE,";",2,99))
        SET CODELINE=$PIECE(CODELINE,";",1)
        ;"WRITE CODELINE,!
        NEW COUNT SET COUNT=1
        IF COMMENT'="" SET TMGWP(COUNT)=COMMENT SET COUNT=COUNT+1
        NEW DONE SET DONE=0
        FOR I=1:1 DO  QUIT:DONE   ;"Print out any additional comment lines
        . NEW CODEREF2 SET CODEREFS=TAG_"+"_I_"^"_ROUTINE
        . SET COMMENT=$$TRIM^XLFSTR($TEXT(@CODEREFS))
        . IF $EXTRACT(COMMENT,1)'=";" SET DONE=1 QUIT
        . SET COMMENT=$PIECE(COMMENT,";",2,99)
        . SET TMGWP(COUNT)=COMMENT SET COUNT=COUNT+1
        NEW PARAMS SET PARAMS=$PIECE($PIECE(CODELINE,"(",2),")",1)
        FOR I=1:1:$LENGTH(PARAMS,",") DO
        . SET PARAMS(I,"NAME")=$PIECE(PARAMS,",",I)
BRRPC1  KILL MENU
        SET MENU(0)="RPC NAME: "_RPCNAME_" (`"_IEN_")"
        SET MENU(0,1)="RPC CODE: "_CODELINE
        SET MENU(0,2)="Pick parameter to get more information"
        SET I=0
        FOR  SET I=$ORDER(PARAMS(I)) QUIT:+I'>0  DO
        . NEW NAME SET NAME=PARAMS(I,"NAME")
        . IF I=1 SET NAME=NAME_" (The RPC result parameter)"
        . SET MENU(I)="Parameter: "_NAME_$CHAR(9)_I
        SET I=$ORDER(MENU(""),-1)+1
        SET MENU(I)="View RPC Description"_$CHAR(9)_"RPC DESCRIPTION",I=I+1
        IF $DATA(TMGWP) SET MENU(I)="View code entry comments"_$CHAR(9)_"COMMENTS",I=I+1
        SET MENU(I)="Dump RPC record"_$CHAR(9)_"DUMP",I=I+1
        SET MENU(I)="View RPC code"_$CHAR(9)_"EDIT",I=I+1
        SET MENU(I)="Trace through RPC code"_$CHAR(9)_"DEBUG",I=I+1
        SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
        IF USRPICK="^" GOTO BRWDN
        IF USRPICK="RPC DESCRIPTION" DO  GOTO BRRPC1
        . DO RPCDESCR(IEN)
        ELSE  IF USRPICK="EDIT" DO
        . DO STARTVED^TMGVPE(TAG,ROUTINE,1)
        ELSE  IF USRPICK="DEBUG" DO
        . DO DEBUGRPC(CODEREF,CODELINE,IEN,.PARAMS)
        ELSE  IF USRPICK="DUMP" DO
        . WRITE !,!
        . DO DUMPREC^TMGDEBU3(8994,IEN_",")
        . DO PRESS2GO^TMGUSRI2
        ELSE  IF USRPICK="COMMENTS" DO
        . DO PAGEWP(.TMGWP)
        . DO PRESS2GO^TMGUSRI2
        ELSE  IF +USRPICK=USRPICK DO
        . NEW NAME SET NAME=$GET(PARAMS(USRPICK,"NAME"))
        . DO DISP1(IEN,USRPICK,NAME)
        GOTO BRRPC1
BRWDN   QUIT
        ;
DISP1(IEN,IDX,NAME) ;
        ;"Purpose: to display information about a given parameter
        ;"Input: IEN -- record# in REMOTE PROCEDURE file (8994) to display from
        ;"       IDX -- the index of the parameter.  1 for the 1st, 2 for the 2nd etc.
        ;"              Note: for RPC's, index=1 is always the return parameter
        ;"       NAME -- The name of the parameter
        SET IDX=$GET(IDX)
        SET IEN=+$GET(IEN)
        SET NAME=$GET(NAME)
        WRITE !,!,"Description of parameter '",NAME,"'",!
        WRITE "-----------------------------------------------------",!
        NEW TMGWP
        IF IDX'=1 GOTO DP2
        WRITE "RESULTS Parameter NAME: ",NAME,!
        WRITE "RESULTS Parameter TYPE: ",$$GET1^DIQ(8994,IEN_",",.04),!
        IF $$GET1^DIQ(8994,IEN_",",3,"","TMGWP")
        WRITE "RPC RESULT parameter DESCRIPTION: "
        DO PAGEWP(.TMGWP)
        GOTO DPDN
DP2
        SET IDX=IDX-1
        IF IDX'>0 GOTO DPDN
        NEW SUBIEN SET SUBIEN=+$ORDER(^XWB(8994,IEN,2,"PARAMSEQ",IDX,0))
        WRITE "Parameter NAME: ",NAME,!
        IF SUBIEN'>0 DO  GOTO DPDN
        . WRITE "Sorry, creator of RPC did not provide further information.",!
        . ;"WRITE "Can't find information for parameter '",IDX,"' in record #",IEN," in file 8994.",!
        . NEW % SET %=2
        . WRITE "Would you like to create NEW documentation for '",NAME,"'"
        . DO YN^DICN WRITE !
        . IF %'=1 QUIT
        . NEW TMGFDA,TMGMSG,TMGIEN,TMGIENS
        . SET TMGIENS="+1,"_IEN_","
        . SET TMGFDA(8994.02,TMGIENS,.01)=NAME
        . SET TMGFDA(8994.02,TMGIENS,.05)=IDX
        . DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) DO  QUIT
        . . WRITE $$GETERRST^TMGDEBU2(.TMGMSG)
        . SET SUBIEN=+$GET(TMGIEN(1)) IF SUBIEN'>0 DO  QUIT
        . . WRITE "Unable to find newly created record",!
        . NEW DIE SET DIE="^XWB(8994,"_IEN_",2,"
        . NEW DA SET DA=SUBIEN,DA(1)=IEN
        . NEW DR SET DR=".02;.03;.04;1"
        . DO ^DIE
        NEW IENS SET IENS=SUBIEN_","_IEN_","
        WRITE "Parameter TYPE: ",$$GET1^DIQ(8994.02,IENS,.02),!
        WRITE "Parameter REQUIRED: ",$$GET1^DIQ(8994.02,IENS,.04),!
        IF $$GET1^DIQ(8994.02,IENS,1,"","TMGWP")
        WRITE "Parameter DESCRIPTION: "
        DO PAGEWP(.TMGWP)
        ;
DPDN    DO PRESS2GO^TMGUSRI2
        WRITE !,!
        QUIT
        ;
RPCDESCR(IEN) ;"Show description for RPC entry
        NEW TMGWP
        IF $$GET1^DIQ(8994,IEN_",",1,"","TMGWP")
        DO PAGEWP(.TMGWP)
        DO PRESS2GO^TMGUSRI2
        WRITE !,!
        QUIT
        ;
PAGEWP(TMGWP) ;
        ;"Purpose: output WP array, with pause for page eveyr 10 pages.
        IF $DATA(TMGWP)'=0 DO
        . WRITE !
        . NEW CT SET CT=0
        . NEW I SET I=0
        . FOR  SET I=$ORDER(TMGWP(I)) QUIT:(I="")  DO
        . . WRITE ?5,TMGWP(I),!
        . . SET CT=CT+1
        . . IF CT#10=0 DO PRESS2GO^TMGUSRI2
        ELSE  WRITE "(none given)",!
        QUIT
        ;
DEBUGRPC(CODEREF,CODELINE,IEN,PARAMS) ;
        ;"Purpose: entry point to debug through RPC code
        ;"Input: CODEREF -- The tag and routine of RPC e.g. 'ENTRY^MYRPC1'
        ;"       CODELINE -- the code line to execute, e.g. 'ENTRY^MYRPC1(Y,A,B,C,D)'
        ;"       IEN -- the IEN of the RPC code
        ;"       PARAMS --PASS BY REFERENCE.  Expected format.  e.g.
        ;"              PARAMS(1,"NAME")="Y"
        ;"              PARAMS(2,"NAME")="A"
        ;"              PARAMS(3,"NAME")="B"
        ;"              PARAMS(4,"NAME")="C"
        ;"              PARAMS(5,"NAME")="D"
        ;"Results: none
        SET CODEREF=$GET(CODEREF) IF CODEREF="" GOTO DRPCDN
        NEW IDX,MENU,USRPICK,S
        SET IDX=1 FOR  SET IDX=$ORDER(PARAMS(IDX)) QUIT:(+IDX'>0)  DO
        . IF $GET(PARAMS(IDX,"TYPE"))'="" QUIT
        . NEW SUBIEN SET SUBIEN=+$ORDER(^XWB(8994,IEN,2,"PARAMSEQ",IDX-1,0))
        . NEW IENS SET IENS=SUBIEN_","_IEN_","
        . SET PARAMS(IDX,"TYPE")=$$GET1^DIQ(8994.02,IENS,.02)
        ;
        SET IDX=1 FOR  SET IDX=$ORDER(PARAMS(IDX)) QUIT:(+IDX'>0)  DO
        . IF $DATA(^TMG("TEMP",CODEREF,PARAMS(IDX,"NAME"),IDX,"VAL"))=0 DO
        . . SET ^TMG("TEMP",CODEREF,PARAMS(IDX,"NAME"),IDX,"VAL")=""
        . MERGE PARAMS(IDX,"VAL")=^TMG("TEMP",CODEREF,PARAMS(IDX,"NAME"),IDX,"VAL")
        ;
DRL0    KILL MENU
        SET MENU(0)="TRACE THROUGH "_CODEREF
        SET MENU(0,1)="Pick input parameter to edit"
        SET IDX=1 FOR  SET IDX=$ORDER(PARAMS(IDX)) QUIT:(+IDX'>0)  DO
        . NEW S SET S="Edit "_PARAMS(IDX,"NAME")
        . NEW DV SET DV=$DATA(PARAMS(IDX,"VAL"))
        . IF DV=0 SET S=S_" (Empty value)"
        . ELSE  IF DV=1 SET S=S_" (='"_PARAMS(IDX,"VAL")_"')"
        . ELSE  IF D'<10 SET S=S_" (Array value with data)"
        . SET MENU(IDX-1)=S
        SET IDX=$ORDER(MENU(""),-1)+1
        SET S="Debug into RPC" IF IDX>1 SET S=S_" with above parameters"
        SET MENU(IDX)=S_$CHAR(9)_"RUN"
DRL1    IF IDX>1 DO
        . SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
        ELSE  DO
        . SET USRPICK=$PIECE(MENU(1),$CHAR(9),2)
        IF USRPICK="^" GOTO DRPCDN
        IF USRPICK="RUN" DO
        . DO SAVEPAR(CODEREF,.PARAMS)
        . DO RUNRPC(CODEREF,.PARAMS) ;
        ELSE  IF +USRPICK=USRPICK DO  GOTO DRL0
        . DO EDITPAR(USRPICK+1,.PARAMS)
        . DO SAVEPAR(CODEREF,.PARAMS)
        IF IDX>1 GOTO DRL1
DRPCDN  QUIT
        ;
SAVEPAR(CODEREF,PARAMS) ;
        NEW IDX SET IDX=1 FOR  SET IDX=$ORDER(PARAMS(IDX)) QUIT:(+IDX'>0)  DO
        . KILL ^TMG("TEMP",CODEREF,PARAMS(IDX,"NAME"),IDX,"VAL")
        . MERGE ^TMG("TEMP",CODEREF,PARAMS(IDX,"NAME"),IDX,"VAL")=PARAMS(IDX,"VAL")
        QUIT
        ;
EDITPAR(IDX,PARAMS) ;
        ;"Purpose: To edit the value of a parameters
        NEW TYPE SET TYPE=$GET(PARAMS(IDX,"TYPE"))
        IF TYPE="" SET TYPE="LITERAL"
        IF TYPE'="LITERAL" DO  GOTO EPDN
        . WRITE "Editing of type ",TYPE," not yet supported",!
        . DO PRESS2GO^TMGUSRI2
        NEW VAL SET VAL=$GET(PARAMS(IDX,"VAL"))
        WRITE "Enter value for ",PARAMS(IDX,"NAME")," (^ to abort, @ to delete): ",VAL,"// "
        NEW NEWVAL READ NEWVAL:$GET(DTIME,3600),!
        IF NEWVAL="^" GOTO EPDN
        IF NEWVAL="@" SET NEWVAL="",VAL=""
        IF NEWVAL="" SET NEWVAL=VAL
        SET VAL=NEWVAL
        SET PARAMS(IDX,"VAL")=VAL
EPDN    QUIT
        ;
RUNRPC(CODEREF,PARAMS) ;
        NEW TMGVARS,VARNAME,TMGRESULT
        NEW RUNCODE SET RUNCODE=CODEREF_"("
        NEW IDX SET IDX=0 FOR  SET IDX=$ORDER(PARAMS(IDX)) QUIT:(+IDX'>0)  DO
        . KILL PARAMS(IDX,"POSTRUN-VAL")
        . SET VARNAME="%VPE"_PARAMS(IDX,"NAME")
        . SET TMGVARS(IDX,VARNAME)=""
        . MERGE @VARNAME=PARAMS(IDX,"VAL")
        . IF $EXTRACT(RUNCODE,$LENGTH(RUNCODE))'="(" SET RUNCODE=RUNCODE_","
        . SET RUNCODE=RUNCODE_"."_VARNAME
        SET RUNCODE="DO "_RUNCODE_")"
        DO DIRDEBUG^TMGIDE(RUNCODE)
        SET VARNAME="",TMGRESULT=""
        SET IDX=0 FOR  SET IDX=$ORDER(TMGVARS(IDX)) QUIT:(+IDX'>0)  DO
        . SET VARNAME=$ORDER(TMGVARS(IDX,""))
        . MERGE PARAMS(IDX,"POST-RUN-VAL")=@VARNAME
        . IF IDX=1 MERGE TMGRESULT=@VARNAME
        . KILL @VARNAME
        ;
        WRITE "RPC RESULT:",!
        WRITE "--------------------------------------",!
        DO ZWRITE^TMGZWR("TMGRESULT")
        WRITE "--------------------------------------",!
        DO PRESS2GO^TMGUSRI2
        QUIT


