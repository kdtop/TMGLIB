TMGRPCUT ;TMG/kst/RPC utility library ;5/14/12, 4/19/18
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
        IF +Y'>0 QUIT 
        ;"NEW ARR DO GETINFO(+Y,.ARR) ;"Get information about RPC record        
        ;"DO ENSURMETA(+Y) ;"Ensure metadata regaring parameters in 8994 has entry for each param
        ;"KILL ARR DO GETINFO(+Y,.ARR) ;"Get information about RPC record        
        DO BROWSRPC(+Y) GOTO ABR1
        QUIT
        ;
BROWSRPC(IEN) ;
        ;"Purpose: to display programming information about an RPC entry
        ;"Input: IEN -- record# in REMOTE PROCEDURE file (8994) to display
        ;"Results: none
        NEW MENU,USRPICK,IDX,TMGWP,INFO
        WRITE !,"-----REMOTE PROCEDURE CODE (RPC) EXPLORER-----",!,!
        SET IEN=+$GET(IEN) IF IEN'>0 GOTO BRWDN
        DO ENSURMETA(IEN)        
BRRPC1  KILL MENU,INFO
        DO GETINFO(IEN,.INFO) ;"Get information about RPC record     
        DO CHKFIXINFO(.INFO)         
        NEW RPCNAME SET RPCNAME=$GET(INFO("INFO","RPC NAME"))
        NEW CODELINE SET CODELINE=$GET(INFO("INFO","CODELINE"))
        NEW TAG SET TAG=$GET(INFO("INFO","TAG"))
        NEW ROUTINE SET ROUTINE=$GET(INFO("INFO","ROUTINE"))
        NEW CODEREF SET CODEREF=$GET(INFO("INFO","CODEREF"))
        SET MENU(0)="RPC NAME: "_RPCNAME_" (`"_IEN_")"
        SET MENU(0,1)="RPC CODE: "_CODELINE
        SET MENU(0,2)="Pick parameter to get more information"
        SET IDX=0
        FOR  SET IDX=$ORDER(INFO("CODE PARAMS",IDX)) QUIT:+IDX'>0  DO
        . NEW STR SET STR=$SELECT(IDX=1:"RESULT Param",1:"Parameter")_": "
        . NEW NAME SET NAME=$GET(INFO("CODE PARAMS",IDX,"NAME")) QUIT:NAME=""
        . NEW RPCNAME SET RPCNAME=$GET(INFO("CODE PARAMS",IDX,"RPC NAME"))
        . NEW SUBIEN SET SUBIEN=+$GET(INFO("CODE PARAMS",IDX,"RPC SUBIEN"))
        . IF IDX>1,RPCNAME'="",RPCNAME'=NAME SET NAME=NAME_" (aka "_RPCNAME_")"
        . NEW TYPE SET TYPE=$SELECT(IDX=1:$GET(INFO("RPC RESULT","TYPE")),1:$GET(INFO("RPC PARAM",SUBIEN,"TYPE")))
        . SET NAME=NAME_" (type: "_$PIECE(TYPE,"^",2)_")"
        . SET MENU(IDX)=STR_NAME_$CHAR(9)_"PARAM^"_IDX
        SET IDX=$ORDER(MENU(""),-1)+1
        SET MENU(IDX)="View RPC Description"_$CHAR(9)_"RPC DESCRIPTION",IDX=IDX+1
        IF $DATA(ARR("INFO","COMMENTS")) SET MENU(IDX)="View code entry comments"_$CHAR(9)_"COMMENTS",IDX=IDX+1
        SET MENU(IDX)="Dump RPC record"_$CHAR(9)_"DUMP",IDX=IDX+1
        SET MENU(IDX)="View RPC code"_$CHAR(9)_"VIEWCODE",IDX=IDX+1
        SET MENU(IDX)="Trace through RPC code"_$CHAR(9)_"DEBUG",IDX=IDX+1
        SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
        IF USRPICK="^" GOTO BRWDN
        IF USRPICK="RPC DESCRIPTION" DO  GOTO BRRPC1
        . DO RPCDESCR(IEN)
        ELSE  IF USRPICK="VIEWCODE" DO
        . DO STARTVED^TMGVPE(TAG,ROUTINE,1)
        ELSE  IF USRPICK="DEBUG" DO
        . DO DEBUGRPC2(.INFO)
        . ;"DO DEBUGRPC(CODEREF,CODELINE,IEN,.INFO)
        ELSE  IF USRPICK="DUMP" DO
        . WRITE !,!
        . DO DUMPREC^TMGDEBU3(8994,IEN_",")
        . DO PRESS2GO^TMGUSRI2
        ELSE  IF USRPICK="COMMENTS" DO
        . NEW TMGWP MERGE TMGWP=ARR("INFO","COMMENTS")
        . DO PAGEWP(.TMGWP)
        . DO PRESS2GO^TMGUSRI2
        ELSE  IF $PIECE(USRPICK,"^",1)="PARAM" DO
        . NEW IDX SET IDX=+$PIECE(USRPICK,"^",2)
        . NEW NAME SET NAME=$GET(INFO("CODE PARAMS",IDX,"NAME"))
        . DO EDIT1(IDX,.INFO)
        . ;"DO DISP1(IEN,IEN,NAME)
        GOTO BRRPC1
BRWDN   QUIT
        ;
GETINFO(IEN,ARR) ;"Get current information about RPC record (and from code itself)        
        ;"Input: IEN -- IEN in 8994
        ;"       ARR -- PASS BY REFERENCE, AN OUT PARAMETER.  Format below
        ;"       COMMENTARR -- PASS BY REFERENCE, AN OUT PARAMETER
        ;"       PARAMS  -- PASS BY REFERENCE, AN OUT PARAMETER
        ;"Output:  ARR is filled.  Format
        ;"         ARR("INFO","IEN")=IEN
        ;"         ARR("INFO","RPC NAME")=RPCNAME
        ;"         ARR("INFO","CODELINE")=CODELINE
        ;"         ARR("INFO","TAG")=$PIECE(ZN,"^",2)
        ;"         ARR("INFO","ROUTINE"=$PIECE(ZN,"^",3)
        ;"         ARR("INFO","CODEREF")=$PIECE(ZN,"^",2,3)
        ;"         ARR("INFO","COMMENTS",#)=LINE OF COMMENT
        ;"         ARR("CODE PARAMS",1,"RPC NAME")="<RETURN VALUE>"  1 is always the return value
        ;"         ARR("CODE PARAMS",<ORDER#>,"NAME")=NAME
        ;"         ARR("CODE PARAMS",<ORDER#>,"RPC NAME")=<NAME> OR "" if not provided in record
        ;"         ARR("CODE PARAMS",<ORDER#>,"RPC SUBIEN")=SUBIEN  if found
        ;"         ARR("CODE PARAMS",<ORDER#>,"TYPE")=type, same as ARR("RPC PARAM",<SUBIEN>,"TYPE"), if available 
        ;"         ARR("CODE PARAMS",<ORDER#>,"DEBUG VAL")=<prior value enterd when debugging>
        ;"         ARR("CODE PARAMS","B",NAME,<ORDER#>)=""  ;"a cross reference
        ;"         ARR("RPC PARAM",<SUBIEN>,"NAME")=RPC NAME
        ;"         ARR("RPC PARAM",<SUBIEN>,"TYPE")=CODE^VALUE  e.g. '1^LITERAL'
        ;"         ARR("RPC PARAM",<SUBIEN>,"MAX LEN")=# or ""
        ;"         ARR("RPC PARAM",<SUBIEN>,"REQ")=1 or 0 or ""   1=true, 0=false
        ;"         ARR("RPC PARAM",<SUBIEN>,"SEQ")=SEQ# or "" SEQUENCE number if provided. 
        ;"            NOTE: The first CODE PARAM is the return parameter.  So the user may have the first
        ;"                 input parameter be either 1 or 2 (or whatever the developer felt like putting in)
        ;"         ARR("RPC PARAM",SUBIEN,"NAME IN CODE")=<CODE PARAM>  Present only if found
        ;"         ARR("RPC PARAM","SEQ",<SEQ#>,<SUBIEN>)=""  -- a cross reference
        ;"         ARR("RPC PARAM",<SUBIEN>,"DESCR",#)=<line of description>  Only added if description found
        ;"         ARR("RPC RESULT","TYPE")=CODE^VALUE   e.g. '1^SINGLE VALUE'
        ;"         ARR("RPC RESULT","DESCR",#)=<line of description>  Only added if description found
        ;"         ARR("RPC RESULT","NAME IN CODE")=<VARIABLE NAME>
        ;"Result: none
        KILL ARR  ;"clear any prior
        SET IEN=+$GET(IEN) IF IEN'>0 GOTO BRWDN
        SET ARR("INFO","IEN")=IEN
        NEW ZN SET ZN=$GET(^XWB(8994,IEN,0))
        NEW RPCNAME SET RPCNAME=$PIECE(ZN,"^",1)
        SET ARR("INFO","RPC NAME")=RPCNAME
        NEW TAG SET TAG=$PIECE(ZN,"^",2)
        NEW ROUTINE SET ROUTINE=$PIECE(ZN,"^",3)
        NEW CODEREF SET CODEREF=TAG_"^"_ROUTINE
        NEW CODELINE SET CODELINE=$TEXT(@CODEREF)
        SET CODELINE=CODEREF_"("_$PIECE(CODELINE,"(",2,99)
        SET ARR("INFO","CODELINE")=CODELINE
        SET ARR("INFO","TAG")=$PIECE(ZN,"^",2)
        SET ARR("INFO","ROUTINE")=$PIECE(ZN,"^",3)
        SET ARR("INFO","CODEREF")=$PIECE(ZN,"^",2,3)
        NEW COMMENT SET COMMENT=$$TRIM^XLFSTR($PIECE(CODELINE,";",2,99))
        SET CODELINE=$PIECE(CODELINE,";",1)
        NEW COUNT SET COUNT=1
        IF COMMENT'="" SET ARR("INFO","COMMENTS",COUNT)=COMMENT,COUNT=COUNT+1
        NEW DONE SET DONE=0
        NEW IDX FOR IDX=1:1 DO  QUIT:DONE   ;"Print out any additional comment lines
        . NEW CODEREFS SET CODEREFS=TAG_"+"_IDX_"^"_ROUTINE
        . SET COMMENT=$$TRIM^XLFSTR($TEXT(@CODEREFS))
        . IF $EXTRACT(COMMENT,1)'=";" SET DONE=1 QUIT
        . SET COMMENT=$PIECE(COMMENT,";",2,99)
        . SET ARR("INFO","COMMENTS",COUNT)=COMMENT,COUNT=COUNT+1
        ;"Parse all the input parameter names from the mumps code 
        NEW PARAMS SET PARAMS=$PIECE($PIECE(CODELINE,"(",2),")",1)
        FOR IDX=1:1:$LENGTH(PARAMS,",") DO
        . NEW APARAM SET APARAM=$PIECE(PARAMS,",",IDX) QUIT:APARAM=""
        . SET ARR("CODE PARAMS",IDX,"NAME")=APARAM
        . SET ARR("CODE PARAMS","B",APARAM,IDX)=""  ;"a cross reference. IDX=1 is RESULT, IDX#2 = SEQ#1
        SET IDX=0
        NEW SETINFO SET SETINFO=$PIECE($GET(^DD(8994.02,.02,0)),"^",3)
        FOR IDX=1:1:$LENGTH(SETINFO,";") DO
        . NEW TEMP SET TEMP=$PIECE(SETINFO,";",IDX) QUIT:TEMP=""
        . NEW CODE SET CODE=$PIECE(TEMP,":",1)
        . NEW VALUE SET VALUE=$PIECE(TEMP,":",2)
        . SET SETINFO("BY CODE",CODE)=VALUE
        . SET SETINFO("BY VALUE",VALUE)=CODE
        ;"Next, parse all the entries in the REMOTE PROCEDURE file for each parameter
        NEW SUBIEN SET SUBIEN=0
        FOR  SET SUBIEN=$ORDER(^XWB(8994,IEN,2,SUBIEN)) QUIT:SUBIEN'>0  DO
        . NEW ZN SET ZN=$GET(^XWB(8994,IEN,2,SUBIEN,0))
        . SET ARR("RPC PARAM",SUBIEN,"NAME")=$PIECE(ZN,"^",1)
        . NEW CODE SET CODE=$PIECE(ZN,"^",2) IF CODE="" SET CODE="?"
        . NEW VALUE SET VALUE=$GET(SETINFO("BY CODE",CODE),"?")
        . SET ARR("RPC PARAM",SUBIEN,"TYPE")=CODE_"^"_VALUE
        . SET ARR("RPC PARAM",SUBIEN,"MAX LEN")=$PIECE(ZN,"^",3)
        . SET ARR("RPC PARAM",SUBIEN,"REQ")=$PIECE(ZN,"^",4)  ;"REQUIRED
        . NEW SEQ SET SEQ=+$PIECE(ZN,"^",5)
        . SET ARR("RPC PARAM",SUBIEN,"SEQ")=SEQ  ;"SEQUENCE
        . SET ARR("RPC PARAM","SEQ",SEQ,SUBIEN)=""  ;"CROSS REF
        . NEW JDX SET JDX=0
        . FOR  SET JDX=$ORDER(^XWB(8994,IEN,2,SUBIEN,1,JDX)) QUIT:JDX'>0  DO
        . . SET ARR("RPC PARAM",SUBIEN,"DESCR",JDX)=$GET(^XWB(8994,IEN,2,SUBIEN,1,JDX,0))
        ;"Cycle through 'RPC PARAMS' and link to any 'CODE PARAMS' of same name
        NEW SUBIEN SET SUBIEN=0
        FOR  SET SUBIEN=$ORDER(ARR("RPC PARAM",SUBIEN)) QUIT:SUBIEN'>0  DO
        . NEW RPCPARAM SET RPCPARAM=$GET(ARR("RPC PARAM",SUBIEN,"NAME"))
        . SET IDX=+$ORDER(ARR("CODE PARAMS","B",RPCPARAM,0)) QUIT:IDX'>0  ;"IDX=SEQ#+1  
        . NEW CODEPARAM SET CODEPARAM=$GET(ARR("CODE PARAMS",IDX,"NAME")) QUIT:CODEPARAM=""
        . SET ARR("RPC PARAM",SUBIEN,"NAME IN CODE")=CODEPARAM
        . SET ARR("CODE PARAMS",IDX,"RPC NAME")=RPCPARAM
        . SET ARR("CODE PARAMS",IDX,"RPC SUBIEN")=SUBIEN
        . SET ARR("CODE PARAMS",IDX,"TYPE")=$GET(ARR("RPC PARAM",SUBIEN,"TYPE"))
        . ;"----
        ;"Get any user preset values (if any) from prior debug runs        
        SET IDX=1 FOR  SET IDX=$ORDER(ARR("CODE PARAMS",IDX)) QUIT:(+IDX'>0)  DO
        . NEW NAME SET NAME=$GET(ARR("CODE PARAMS",IDX,"NAME")) QUIT:NAME=""
        . NEW REF SET REF=$NAME(^TMG("TEMP","RPC DEBUG",CODEREF,NAME))
        . IF $DATA(@REF@("VAL"))=0 SET @REF@("VAL")=""
        . MERGE ARR("CODE PARAMS",IDX,"DEBUG VAL")=@REF@("VAL")
        ;"Now result info
        KILL SETINFO SET SETINFO=$PIECE($GET(^DD(8994,.04,0)),"^",3)
        FOR IDX=1:1:$LENGTH(SETINFO,";") DO
        . NEW TEMP SET TEMP=$PIECE(SETINFO,";",IDX) QUIT:TEMP=""
        . NEW CODE SET CODE=$PIECE(TEMP,":",1)
        . NEW VALUE SET VALUE=$PIECE(TEMP,":",2)
        . SET SETINFO("BY CODE",CODE)=VALUE
        . SET SETINFO("BY VALUE",VALUE)=CODE
        NEW CODE SET CODE=$PIECE(ZN,"^",4) IF CODE="" SET CODE="?" 
        NEW VALUE SET VALUE=$GET(SETINFO("BY CODE",CODE),"?")
        SET ARR("RPC RESULT","TYPE")=CODE_"^"_VALUE        
        NEW JDX SET JDX=0
        FOR  SET JDX=$ORDER(^XWB(8994,IEN,3,JDX)) QUIT:JDX'>0  DO
        . SET ARR("RPC RESULT","DESCR",JDX)=$GET(^XWB(8994,IEN,3,JDX,0))
        SET ARR("RPC RESULT","NAME IN CODE")=$GET(ARR("CODE PARAMS",1,"NAME"))
        SET ARR("CODE PARAMS",1,"RPC NAME")="<RETURN VALUE>"
GIDN    QUIT
        ;
CHKFIXINFO(INFO) ;
        ;"Check SEQ info for bad, missing, duplicate entries
        NEW IEN SET IEN=+$GET(INFO("INFO","IEN"))
        NEW MENU,SUBIEN,IDX,JDX,USRPICK
        NEW BADSEQ
CFIL0   KILL BADSEQ
        NEW ASEQ SET ASEQ=""
        FOR  SET ASEQ=$ORDER(INFO("RPC PARAM","SEQ",ASEQ)) QUIT:ASEQ=""  DO
        . NEW CT SET CT=$$LISTCT^TMGMISC2($NAME(INFO("RPC PARAM","SEQ",ASEQ)))
        . IF CT=1 QUIT  ;"this is OK
        . NEW SUBIEN SET SUBIEN=0
        . FOR  SET SUBIEN=$ORDER(INFO("RPC PARAM","SEQ",ASEQ,SUBIEN)) QUIT:SUBIEN=""  DO
        . . SET BADSEQ(SUBIEN,ASEQ)=""
        IF $DATA(BADSEQ)=0 GOTO CFIDN        
CFIL1   KILL MENU SET IDX=1
        SET MENU(0)="BAD META DATA"
        SET MENU(0,0.1)="Pick parameter to FIX"
        SET MENU(0,0.2)="SOURCE CODE: "_$PIECE($GET(INFO("INFO","CODELINE")),";",1)
        SET JDX=1 FOR  SET JDX=$ORDER(INFO("CODE PARAMS",JDX)) QUIT:JDX'>0  DO
        . NEW NAME SET NAME=$GET(INFO("CODE PARAMS",JDX,"NAME"))
        . SET MENU(0,JDX)="     SEQ #"_(JDX-1)_": "_NAME
        SET SUBIEN="" FOR  SET SUBIEN=$ORDER(BADSEQ(SUBIEN)) QUIT:SUBIEN=""  DO
        . NEW ASEQ SET ASEQ=""
        . FOR  SET ASEQ=$ORDER(BADSEQ(SUBIEN,ASEQ)) QUIT:ASEQ=""  DO
        . . NEW NAME SET NAME=$GET(INFO("RPC PARAM",SUBIEN,"NAME"),"?")
        . . NEW STR SET STR="Record `"_SUBIEN_" ("_NAME_") had BAD/CONFLICTING SEQUENCE#: ["_ASEQ_"]"
        . . SET MENU(IDX)=STR_$CHAR(9)_"EDIT^"_SUBIEN_"^"_ASEQ
        . . SET IDX=IDX+1
        SET MENU(IDX)="Dump RPC record"_$CHAR(9)_"DUMP",IDX=IDX+1
        SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
        IF USRPICK="^" GOTO CFIDN
        IF $PIECE(USRPICK,"^",1)="EDIT" DO  GOTO CFIL0
        . NEW SUBIEN SET SUBIEN=$PIECE(USRPICK,"^",2)
        . DO EDITSUBIEN(SUBIEN,IEN,".01;.05",1)
        . DO GETINFO(IEN,.INFO)
        ELSE  IF USRPICK="DUMP" DO  GOTO CFIL1
        . WRITE !,!
        . DO DUMPREC^TMGDEBU3(8994,IEN_",")
        . DO PRESS2GO^TMGUSRI2
        GOTO CFIL1
CFIDN   QUIT;
        ;        
ENSURMETA(IEN) ;"Ensure metadata regaring parameters in 8994 has entry for each param
        ;"Input: IEN -- IEN in 8994
        SET IEN=+$GET(IEN) IF IEN'>0 GOTO ENSMTDN
        NEW ARR DO GETINFO(IEN,.ARR) ;"Get information about RPC record        
        NEW IDX SET IDX=1  ;"ignore param 1, It is the output/result parameter
        FOR  SET IDX=$ORDER(ARR("CODE PARAMS",IDX)) QUIT:IDX'>0  DO
        . NEW PARAMNAME SET PARAMNAME=$GET(ARR("CODE PARAMS",IDX,"NAME")) QUIT:PARAMNAME=""
        . NEW SUBIEN SET SUBIEN=+$GET(ARR("CODE PARAMS",IDX,"RPC SUBIEN"))
        . IF SUBIEN>0 QUIT  ;"already present
        . NEW TMGFDA,TMGIEN,TMGMSG
        . SET TMGFDA(8994.02,"+1,"_IEN_",",.01)=PARAMNAME
        . SET TMGFDA(8994.02,"+1,"_IEN_",",.05)=IDX-1
        . DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) DO  QUIT
        . . WRITE !,$$GETERRST^TMGDEBU2(.TMGMSG),!
        . . DO PRESS2GO^TMGUSRI2
ENSMTDN QUIT
        ;
 ;"DISP1(IEN,IDX,NAME) ;"DEPRECIATED
 ;"        ;"Purpose: to display information about a given parameter
 ;"        ;"Input: IEN -- record# in REMOTE PROCEDURE file (8994) to display from
 ;"        ;"       IDX -- the index of the parameter.  1 for the 1st, 2 for the 2nd etc.
 ;"        ;"              Note: for RPC's, index=1 is always the return parameter
 ;"        ;"       NAME -- The name of the parameter
 ;"        SET IDX=$GET(IDX)
 ;"        SET IEN=+$GET(IEN)
 ;"        SET NAME=$GET(NAME)
 ;"        WRITE !,!,"Description of parameter '",NAME,"'",!
 ;"        WRITE "-----------------------------------------------------",!
 ;"        NEW TMGWP
 ;"        IF IDX'=1 GOTO DP2
 ;"        WRITE "RESULTS Parameter NAME: ",NAME,!
 ;"        WRITE "RESULTS Parameter TYPE: ",$$GET1^DIQ(8994,IEN_",",.04),!
 ;"        IF $$GET1^DIQ(8994,IEN_",",3,"","TMGWP")
 ;"        WRITE "RPC RESULT parameter DESCRIPTION: "
 ;"        DO PAGEWP(.TMGWP)
 ;"        GOTO DPDN
 ;"DP2     SET IDX=IDX-1
 ;"        IF IDX'>0 GOTO DPDN
 ;"        NEW SUBIEN SET SUBIEN=+$ORDER(^XWB(8994,IEN,2,"PARAMSEQ",IDX,0))
 ;"        WRITE "Parameter NAME: ",NAME,!
 ;"        IF SUBIEN'>0 DO  GOTO DPDN
 ;"        . WRITE "Sorry, creator of RPC did not provide further information.",!
 ;"        . ;"WRITE "Can't find information for parameter '",IDX,"' in record #",IEN," in file 8994.",!
 ;"        . NEW % SET %=2
 ;"        . WRITE "Would you like to create NEW documentation for '",NAME,"'"
 ;"        . DO YN^DICN WRITE !
 ;"        . IF %'=1 QUIT
 ;"        . NEW TMGFDA,TMGMSG,TMGIEN,TMGIENS
 ;"        . SET TMGIENS="+1,"_IEN_","
 ;"        . SET TMGFDA(8994.02,TMGIENS,.01)=NAME
 ;"        . SET TMGFDA(8994.02,TMGIENS,.05)=IDX
 ;"        . DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
 ;"        . IF $DATA(TMGMSG("DIERR")) DO  QUIT
 ;"        . . WRITE $$GETERRST^TMGDEBU2(.TMGMSG)
 ;"        . SET SUBIEN=+$GET(TMGIEN(1)) IF SUBIEN'>0 DO  QUIT
 ;"        . . WRITE "Unable to find newly created record",!
 ;"        . NEW DIE SET DIE="^XWB(8994,"_IEN_",2,"
 ;"        . NEW DA SET DA=SUBIEN,DA(1)=IEN
 ;"        . NEW DR SET DR=".02;.03;.04;1"
 ;"        . DO ^DIE
 ;"        NEW IENS SET IENS=SUBIEN_","_IEN_","
 ;"        WRITE "Parameter TYPE: ",$$GET1^DIQ(8994.02,IENS,.02),!
 ;"        WRITE "Parameter REQUIRED: ",$$GET1^DIQ(8994.02,IENS,.04),!
 ;"        IF $$GET1^DIQ(8994.02,IENS,1,"","TMGWP")
 ;"        WRITE "Parameter DESCRIPTION: "
 ;"        DO PAGEWP(.TMGWP)
 ;"        ;
 ;"DPDN    DO PRESS2GO^TMGUSRI2
 ;"        WRITE !,!
 ;"        QUIT
 ;"        ;
EDIT1(IDX,INFO) ;"Display information about a given parameter, with option to edit
        ;"Input: IDX -- the index of the parameter.  1 for the 1st, 2 for the 2nd etc.
        ;"              Note: for RPC's, index=1 is always the return parameter
        ;"       INFO -- PASS BY REFERENCE.  As created by GETINFO() above
        ;"Result: none;
        SET IDX=+$GET(IDX)
        NEW IEN SET IEN=+$GET(INFO("INFO","IEN"))
        SET NAME=$GET(INFO("CODE PARAMS",IDX,"NAME"))
        WRITE !,!,"Description of parameter '",NAME,"'",!
        WRITE "-----------------------------------------------------",!
        NEW TMGWP
        IF IDX'=1 GOTO DP22
        ;"THIS IS FOR RETURN INFORMATION
        NEW TYPE SET TYPE=$GET(INFO("RPC RESULT","TYPE"))
        WRITE "RESULTS Parameter NAME: ",NAME,!
        WRITE "RESULTS Parameter TYPE: ",$PIECE(TYPE,"^",2),!
        IF $$GET1^DIQ(8994,IEN_",",3,"","TMGWP")
        WRITE "RPC RESULT parameter DESCRIPTION: "
        DO PAGEWP(.TMGWP)
        WRITE !
        SET %=2 WRITE "Edit information" DO YN^DICN WRITE !
        IF %'=1 GOTO DP2DN 
        NEW DIE SET DIE="^XWB(8994,"
        NEW DA SET DA=IEN
        NEW DR SET DR=".04;3"
        DO ^DIE WRITE !
        DO PRESS2GO^TMGUSRI2
        WRITE !,!        
        GOTO DP2DN
DP22     ;"THIS IS FOR REGULAR PARAMS (NOT THE RESULT)
        NEW SUBIEN SET SUBIEN=$GET(INFO("CODE PARAMS",IDX,"RPC SUBIEN"))
        DO EDITSUBIEN(SUBIEN,IEN) 
DP2DN   QUIT
        ;
EDITSUBIEN(SUBIEN,IEN,FIELDS,NOASK) ;
        SET FIELDS=$GET(FIELDS,".02;.03;.04;.05;1")
        DO DUMPREC^TMGDEBU3(8994.02,SUBIEN_","_IEN_",",1)
        WRITE !
        SET %=2 
        IF +$GET(NOASK)=1 SET %=1
        ELSE  WRITE "Edit information" DO YN^DICN WRITE !
        IF %'=1 QUIT
        DO EDITRPCPARAM(SUBIEN,IEN,FIELDS)
        DO PRESS2GO^TMGUSRI2
        WRITE !,!
        QUIT
        ;
EDITRPCPARAM(SUBIEN,IEN,FIELDS) ;
        NEW DIE SET DIE="^XWB(8994,"_IEN_",2,"
        NEW DA SET DA=SUBIEN,DA(1)=IEN
        NEW DR SET DR=FIELDS
        DO ^DIE
        WRITE !
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
 ;"DEBUGRPC(CODEREF,CODELINE,IEN,PARAMS) ;"DEPRECIATED
 ;"        ;"Purpose: entry point to debug through RPC code
 ;"        ;"Input: CODEREF -- The tag and routine of RPC e.g. 'ENTRY^MYRPC1'
 ;"        ;"       CODELINE -- the code line to execute, e.g. 'ENTRY^MYRPC1(Y,A,B,C,D)'
 ;"        ;"       IEN -- the IEN of the RPC code
 ;"        ;"       PARAMS --PASS BY REFERENCE.  Expected format.  e.g.
 ;"        ;"              PARAMS(1,"NAME")="Y"
 ;"        ;"              PARAMS(2,"NAME")="A"
 ;"        ;"              PARAMS(3,"NAME")="B"
 ;"        ;"              PARAMS(4,"NAME")="C"
 ;"        ;"              PARAMS(5,"NAME")="D"
 ;"        ;"Results: none
 ;"        SET CODEREF=$GET(CODEREF) IF CODEREF="" GOTO DRPCDN
 ;"        NEW IDX,MENU,USRPICK,S
 ;"        SET IDX=1 FOR  SET IDX=$ORDER(PARAMS(IDX)) QUIT:(+IDX'>0)  DO
 ;"        . IF $GET(PARAMS(IDX,"TYPE"))'="" QUIT
 ;"        . NEW SUBIEN SET SUBIEN=+$ORDER(^XWB(8994,IEN,2,"PARAMSEQ",IDX-1,0))
 ;"        . NEW IENS SET IENS=SUBIEN_","_IEN_","
 ;"        . SET PARAMS(IDX,"TYPE")=$$GET1^DIQ(8994.02,IENS,.02)
 ;"        ;
 ;"        SET IDX=1 FOR  SET IDX=$ORDER(PARAMS(IDX)) QUIT:(+IDX'>0)  DO
 ;"        . IF $DATA(^TMG("TEMP",CODEREF,PARAMS(IDX,"NAME"),IDX,"VAL"))=0 DO
 ;"        . . SET ^TMG("TEMP",CODEREF,PARAMS(IDX,"NAME"),IDX,"VAL")=""
 ;"        . MERGE PARAMS(IDX,"VAL")=^TMG("TEMP",CODEREF,PARAMS(IDX,"NAME"),IDX,"VAL")
 ;"        ;
 ;"DRL0    KILL MENU
 ;"        SET MENU(0)="TRACE THROUGH "_CODEREF
 ;"        SET MENU(0,1)="Pick input parameter to edit"
 ;"        SET IDX=1 FOR  SET IDX=$ORDER(PARAMS(IDX)) QUIT:(+IDX'>0)  DO
 ;"        . NEW S SET S="Edit "_PARAMS(IDX,"NAME")
 ;"        . NEW DV SET DV=$DATA(PARAMS(IDX,"VAL"))
 ;"        . IF DV=0 SET S=S_" (Empty value)"
 ;"        . ELSE  IF DV=1 SET S=S_" (='"_PARAMS(IDX,"VAL")_"')"
 ;"        . ELSE  IF DV'<10 SET S=S_" (Array value with data)"
 ;"        . SET MENU(IDX-1)=S
 ;"        SET IDX=$ORDER(MENU(""),-1)+1
 ;"        SET S="Debug into RPC" IF IDX>1 SET S=S_" with above parameters"
 ;"        SET MENU(IDX)=S_$CHAR(9)_"RUN"
 ;"DRL1    IF IDX>1 DO
 ;"        . SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
 ;"        ELSE  DO
 ;"        . SET USRPICK=$PIECE(MENU(1),$CHAR(9),2)
 ;"        IF USRPICK="^" GOTO DRPCDN
 ;"        IF USRPICK="RUN" DO
 ;"        . DO SAVEPAR(CODEREF,.PARAMS)
 ;"        . DO RUNRPC(CODEREF,.PARAMS) ;
 ;"        ELSE  IF +USRPICK=USRPICK DO  GOTO DRL0
 ;"        . DO EDITPAR(USRPICK+1,.PARAMS)
 ;"        . DO SAVEPAR(CODEREF,.PARAMS)
 ;"        IF IDX>1 GOTO DRL1
 ;"DRPCDN  QUIT
 ;"        ;
DEBUGRPC2(INFO) ;
        ;"Purpose: entry point to debug through RPC code
        ;"Input: INFO -- ARRAY. PASS BY REFERENCE.  Format as output by GETINFO() above
        ;"Results: none
        NEW CODEREF SET CODEREF=$GET(INFO("INFO","CODEREF")) IF CODEREF="" GOTO DRPC2DN
        NEW CODELINE SET CODELINE=$GET(INFO("INFO","CODELINE"))
        NEW IEN SET IEN=$GET(INFO("INFO","IEN"))
        NEW IDXMENU,USRPICK,S
        NEW ROUTINE SET ROUTINE=$GET(INFO("INFO","ROUTINE"))
        ;
DR2L0   KILL MENU
        SET MENU(0)="TRACE THROUGH "_CODEREF
        SET MENU(0,1)="Pick input parameter to edit"
        SET IDX=1 FOR  SET IDX=$ORDER(INFO("CODE PARAMS",IDX)) QUIT:(+IDX'>0)  DO
        . NEW S SET S="Edit "_INFO("CODE PARAMS",IDX,"NAME")        
        . NEW TYPE SET TYPE=$GET(INFO("CODE PARAMS",IDX,"TYPE"))
        . SET S=S_" (type: "_$PIECE(TYPE,"^",2)_")"
        . NEW DV SET DV=$DATA(INFO("CODE PARAMS",IDX,"DEBUG VAL"))
        . IF DV=0 SET S=S_" (Empty value)"
        . ELSE  IF DV=1 SET S=S_" (='"_INFO("CODE PARAMS",IDX,"DEBUG VAL")_"')"
        . ELSE  IF DV'<10 SET S=S_" (Array value with data)"
        . SET MENU(IDX-1)=S_$CHAR(9)_"PARAM^"_(IDX-1)
        . IF DV'<10 DO
        . . NEW NAME SET NAME=$GET(INFO("CODE PARAMS",IDX,"NAME"))
        . . NEW @NAME MERGE @NAME=INFO("CODE PARAMS",IDX,"DEBUG VAL")
        . . ;"NEW VAR MERGE VAR=INFO("CODE PARAMS",IDX,"DEBUG VAL")
        . . DO ZWR2ARR^TMGZWR(NAME,$NAME(MENU(IDX-1)))
        SET IDX=$ORDER(MENU(""),-1)+1
        SET S="Debug into RPC" IF IDX>1 SET S=S_" with above parameters"
        SET MENU(IDX)=S_$CHAR(9)_"RUN",IDX=IDX+1
        IF ROUTINE'="" SET MENU(IDX)="ZLINK "_ROUTINE_$CHAR(9)_"ZL" 
DR2L1   IF IDX>1 DO
        . SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
        ELSE  DO
        . SET USRPICK=$PIECE(MENU(1),$CHAR(9),2)
        IF USRPICK="^" GOTO DRPC2DN
        IF USRPICK="ZL" DO  GOTO DR2L0
        . ZLINK ROUTINE
        . WRITE !,"ZLINK COMPLETED.",!
        . DO PRESS2GO^TMGUSRI2
        IF USRPICK="RUN" DO
        . DO SAVEPAR2(.INFO)
        . DO RUNRPC2(.INFO) ;
        ELSE  IF $PIECE(USRPICK,"^",1)="PARAM" DO  GOTO DR2L0
        . SET USRPICK=$PIECE(USRPICK,"^",2)
        . DO EDITPAR2(USRPICK+1,.INFO)
        . DO SAVEPAR2(.INFO)
        IF IDX>1 GOTO DR2L1
DRPC2DN  QUIT
        ;
 ;"SAVEPAR(CODEREF,PARAMS) ;"DEPRECIATED
 ;"        NEW IDX SET IDX=1 FOR  SET IDX=$ORDER(PARAMS(IDX)) QUIT:(+IDX'>0)  DO
 ;"        . KILL ^TMG("TEMP",CODEREF,PARAMS(IDX,"NAME"),IDX,"VAL")
 ;"        . MERGE ^TMG("TEMP",CODEREF,PARAMS(IDX,"NAME"),IDX,"VAL")=PARAMS(IDX,"VAL")
 ;"        QUIT
        ;
SAVEPAR2(INFO) ;
        NEW CODEREF SET CODEREF=$GET(INFO("INFO","CODEREF")) IF CODEREF="" QUIT
        NEW IDX SET IDX=1 FOR  SET IDX=$ORDER(INFO("CODE PARAMS",IDX)) QUIT:(+IDX'>0)  DO
        . NEW NAME SET NAME=$GET(INFO("CODE PARAMS",IDX,"NAME")) QUIT:NAME=""
        . NEW REF SET REF=$NAME(^TMG("TEMP","RPC DEBUG",CODEREF,NAME))
        . KILL @REF@("VAL") MERGE @REF@("VAL")=INFO("CODE PARAMS",IDX,"DEBUG VAL")
        QUIT
        ;
 ;"EDITPAR(IDX,PARAMS) ;"DEPRECIATED
 ;"        ;"Purpose: To edit the value of a parameters
 ;"        NEW TYPE SET TYPE=$GET(PARAMS(IDX,"TYPE"))
 ;"        IF TYPE="" SET TYPE="LITERAL"        
 ;"        IF TYPE="LIST" DO  GOTO EP2DN
 ;"        . NEW CT SET CT=0
 ;"        . SET CT=CT+1,PREFIX(CT)="# Lines beginning with '#' are ignored."
 ;"        . SET CT=CT+1,PREFIX(CT)="# Instructions for entring LIST type data:"
 ;"        . SET CT=CT+1,PREFIX(CT)="# ----------------------------------------------------------"
 ;"        . SET CT=CT+1,PREFIX(CT)="# Enter list information, following the examples below."   
 ;"        . SET CT=CT+1,PREFIX(CT)="# Most RPC's will use simple numeric indices, but not always."   
 ;"        . SET CT=CT+1,PREFIX(CT)="# More complex inputs are possible, as shown below."   
 ;"        . SET CT=CT+1,PREFIX(CT)="# Generic name 'VAR' can be sustituted with any other name."   
 ;"        . SET CT=CT+1,PREFIX(CT)="# "   
 ;"        . SET CT=CT+1,PREFIX(CT)="# VAR(1)=1 "
 ;"        . SET CT=CT+1,PREFIX(CT)="# VAR(2)=""apple""  "
 ;"        . SET CT=CT+1,PREFIX(CT)="# VAR(""name"")=""John"" "
 ;"        . SET CT=CT+1,PREFIX(CT)="# VAR(""age"")=25 "
 ;"        . SET CT=CT+1,PREFIX(CT)="# VAR(""state"")=""MA"" "
 ;"        . SET CT=CT+1,PREFIX(CT)="# VAR(""state"",""city"")=""Boston""  "
 ;"        . SET CT=CT+1,PREFIX(CT)=" "
 ;"        . DO EDITARR2^TMGKERN8("TMGRPCVAR","pico",.PREFIX)
 ;"        . MERGE PARAMS(IDX,"VAL")=TMGRPCVAR
 ;"        ELSE  IF TYPE="LITERAL" DO  GOTO EPDN
 ;"        . NEW VAL SET VAL=$GET(PARAMS(IDX,"VAL"))
 ;"        . WRITE "Enter value for ",PARAMS(IDX,"NAME")," (^ to abort, @ to delete): ",VAL,"// "
 ;"        . NEW NEWVAL READ NEWVAL:$GET(DTIME,3600),!
 ;"        . IF NEWVAL="^" GOTO EPDN
 ;"        . IF NEWVAL="@" SET NEWVAL="",VAL=""
 ;"        . IF NEWVAL="" SET NEWVAL=VAL
 ;"        . SET VAL=NEWVAL
 ;"        . SET PARAMS(IDX,"VAL")=VAL
 ;"        ELSE  DO
 ;"        . WRITE "Editing of type ",TYPE," not yet supported",!
 ;"        . DO PRESS2GO^TMGUSRI2        
 ;"EPDN    QUIT
 ;"        ;
EDITPAR2(IDX,INFO) ;
        ;"Purpose: To edit the value of a parameters
        NEW TYPE SET TYPE=$PIECE($GET(INFO("CODE PARAMS",IDX,"TYPE")),"^",2)
        IF (TYPE="")!(TYPE="?") DO
        . NEW SUBIEN SET SUBIEN=$GET(INFO("CODE PARAMS",IDX,"RPC SUBIEN"))
        . DO DUMPREC^TMGDEBU3(8994.02,SUBIEN_","_IEN_",",1)
        . WRITE "================================",!
        . WRITE "NOTE: PARAMETER TYPE NOT DEFINED!",!
        . WRITE "(NOTE: for normal single values, ",!
        . WRITE "  PARAMETER TYPE should be set to 'LITERAL'",!
        . WRITE "---------------------------------",!
        . SET %=1 WRITE "Fix type now" DO YN^DICN WRITE !
        . IF %'=1 QUIT 
        . DO EDITRPCPARAM(SUBIEN,IEN,".02")
        . WRITE !,"(Finished editing)",!
        . WRITE "================================",!,!
        . KILL INFO DO GETINFO(IEN,.INFO),CHKFIXINFO(.INFO)         
        . SET TYPE=$PIECE($GET(INFO("CODE PARAMS",IDX,"TYPE")),"^",2)
        IF TYPE="LIST" DO  GOTO EP2DN
        . NEW CT SET CT=0
        . SET CT=CT+1,PREFIX(CT)="# Lines beginning with '#' are ignored."
        . SET CT=CT+1,PREFIX(CT)="# Instructions for entring LIST type data:"
        . SET CT=CT+1,PREFIX(CT)="# ----------------------------------------------------------"
        . SET CT=CT+1,PREFIX(CT)="# Enter list information, following the examples below."   
        . SET CT=CT+1,PREFIX(CT)="# Most RPC's will use simple numeric indices, but not always."   
        . SET CT=CT+1,PREFIX(CT)="# More complex inputs are possible, as shown below."   
        . SET CT=CT+1,PREFIX(CT)="# Generic name 'VAR' can be sustituted with any other name."   
        . SET CT=CT+1,PREFIX(CT)="# "   
        . SET CT=CT+1,PREFIX(CT)="# VAR(1)=1 "
        . SET CT=CT+1,PREFIX(CT)="# VAR(2)=""apple""  "
        . SET CT=CT+1,PREFIX(CT)="# VAR(""name"")=""John"" "
        . SET CT=CT+1,PREFIX(CT)="# VAR(""age"")=25 "
        . SET CT=CT+1,PREFIX(CT)="# VAR(""state"")=""MA"" "
        . SET CT=CT+1,PREFIX(CT)="# VAR(""state"",""city"")=""Boston""  "
        . SET CT=CT+1,PREFIX(CT)=" "
        . NEW NAME SET NAME=$GET(INFO("CODE PARAMS",IDX,"NAME"))
        . NEW @NAME MERGE @NAME=INFO("CODE PARAMS",IDX,"DEBUG VAL")
        . ;"NEW VAR MERGE VAR=INFO("CODE PARAMS",IDX,"DEBUG VAL")
        . DO EDITARR2^TMGKERN8(NAME,"pico",.PREFIX)
        . KILL INFO("CODE PARAMS",IDX,"DEBUG VAL")
        . MERGE INFO("CODE PARAMS",IDX,"DEBUG VAL")=@NAME
        ELSE  IF TYPE="LITERAL" DO  GOTO EP2DN
        . NEW VAL SET VAL=$GET(INFO("CODE PARAMS",IDX,"DEBUG VAL"))
        . WRITE "Enter value for ",INFO("CODE PARAMS",IDX,"NAME")," (^ to abort, @ to delete): ",VAL,"// "
        . NEW NEWVAL READ NEWVAL:$GET(DTIME,3600),!
        . IF NEWVAL="^" GOTO EP2DN
        . IF NEWVAL="@" SET NEWVAL="",VAL=""
        . IF NEWVAL="" SET NEWVAL=VAL
        . SET VAL=NEWVAL
        . SET INFO("CODE PARAMS",IDX,"DEBUG VAL")=VAL
        ELSE  DO
        . WRITE "Editing of type ",TYPE," not yet supported.  Consider adding to EDITPAR2^TMGRPCUT",!
        . DO PRESS2GO^TMGUSRI2        
EP2DN   QUIT
        ;
 ;"RUNRPC(CODEREF,PARAMS) ;"DEPRECIATED
 ;"        NEW TMGVARS,VARNAME,TMGRESULT
 ;"        NEW RUNCODE SET RUNCODE=CODEREF_"("
 ;"        NEW IDX SET IDX=0 FOR  SET IDX=$ORDER(PARAMS(IDX)) QUIT:(+IDX'>0)  DO
 ;"        . KILL PARAMS(IDX,"POSTRUN-VAL")
 ;"        . SET VARNAME="%VPE"_PARAMS(IDX,"NAME")
 ;"        . SET TMGVARS(IDX,VARNAME)=""
 ;"        . MERGE @VARNAME=PARAMS(IDX,"VAL")
 ;"        . IF $EXTRACT(RUNCODE,$LENGTH(RUNCODE))'="(" SET RUNCODE=RUNCODE_","
 ;"        . SET RUNCODE=RUNCODE_"."_VARNAME
 ;"        SET RUNCODE="DO "_RUNCODE_")"
 ;"        DO DIRDEBUG^TMGIDE(RUNCODE)
 ;"        SET VARNAME="",TMGRESULT=""
 ;"        SET IDX=0 FOR  SET IDX=$ORDER(TMGVARS(IDX)) QUIT:(+IDX'>0)  DO
 ;"        . SET VARNAME=$ORDER(TMGVARS(IDX,""))
 ;"        . MERGE PARAMS(IDX,"POST-RUN-VAL")=@VARNAME
 ;"        . IF IDX=1 MERGE TMGRESULT=@VARNAME
 ;"        . KILL @VARNAME
 ;"        ;
 ;"        WRITE "RPC RESULT:",!
 ;"        WRITE "--------------------------------------",!
 ;"        DO ZWRITE^TMGZWR("TMGRESULT")
 ;"        WRITE "--------------------------------------",!
 ;"        DO PRESS2GO^TMGUSRI2
 ;"        QUIT
 ;"        ;
RUNRPC2(INFO) ;
        NEW TMGVARS,VARNAME,TMGRESULT
        NEW RUNCODE SET RUNCODE=CODEREF_"("
        NEW IDX SET IDX=0 FOR  SET IDX=$ORDER(INFO("CODE PARAMS",IDX)) QUIT:(+IDX'>0)  DO
        . KILL INFO("CODE PARAMS",IDX,"POSTRUN-VAL")
        . SET VARNAME="%VPE"_INFO("CODE PARAMS",IDX,"NAME")
        . SET TMGVARS(IDX,VARNAME)=""
        . MERGE @VARNAME=INFO("CODE PARAMS",IDX,"DEBUG VAL")
        . IF $EXTRACT(RUNCODE,$LENGTH(RUNCODE))'="(" SET RUNCODE=RUNCODE_","
        . SET RUNCODE=RUNCODE_"."_VARNAME
        SET RUNCODE="DO "_RUNCODE_")"
        DO DIRDEBUG^TMGIDE(RUNCODE)
        SET VARNAME="",TMGRESULT=""
        SET IDX=0 FOR  SET IDX=$ORDER(TMGVARS(IDX)) QUIT:(+IDX'>0)  DO
        . SET VARNAME=$ORDER(TMGVARS(IDX,""))
        . MERGE INFO("CODE PARAMS",IDX,"POST-RUN-VAL")=@VARNAME
        . IF IDX=1 MERGE TMGRESULT=@VARNAME
        . KILL @VARNAME
        ;
        WRITE "RPC RESULT:",!
        WRITE "--------------------------------------",!
        DO ZWRITE^TMGZWR("TMGRESULT")
        WRITE "--------------------------------------",!
        DO PRESS2GO^TMGUSRI2
        QUIT
        ;