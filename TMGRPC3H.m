TMGRPC3H ;TMG/kst/Support Functions for GUI_Config ;12/1/10, 2/2/14
         ;;1.0;TMG-LIB;**1**;12/1/10
 ;
 ;"TMG RPC FUNCTIONS for a FM DESKTOP program
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
 ;" RPC -- Public Functions.
 ;"=======================================================================
 ;" <none>
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;"Dependencies:
 ;"  TMGRPC3* only
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;
GETFMDSV(TMGOUT,TMGPARAMS) ;
        ;"Purpose: Get a list of saved data views.  Those for user, and those shared by others
        ;"Input: TMGOUT -- an OUT PARAMETER, PASS BY REFERENCE.
        ;"       TMGPARAMS -- not used
        ;"Output: TMGOUT is filled as follows:
        ;"            TMGOUT(0)="1^Success" or "-1^Error Message"
        ;"            TMGOUT(1)=SELF or OTHERS^Shared(Y/N)^Name^Owner^DateCreated^IEN in 22714
        ;"            TMGOUT(2)=SELF or OTHERS^Shared(Y/N)^Name^Owner^DateCreated^IEN in 22714  ... etc
        ;"              Shared:  1 (if shared), or 0 IF owned by user
        ;"              Name:  Name of data view
        ;"              Owner: external format name of owner
        ;"              Date: external format of date of creation of view
        ;"Results: none
        NEW SELF,OTHERS,CT
        NEW TMGI SET TMGI=0
        FOR  SET TMGI=$ORDER(^TMG(22714,"C",+DUZ,TMGI)) QUIT:(+TMGI'>0)  DO
        . SET SELF(TMGI)=""
        SET TMGI=0
        FOR  SET TMGI=$ORDER(^TMG(22714,"AC","Y",TMGI)) QUIT:(+TMGI'>0)  DO
        . IF $DATA(SELF(TMGI)) QUIT
        . SET OTHERS(TMGI)=""
        SET TMGI=0
        SET CT=1
        FOR  SET TMGI=$ORDER(SELF(TMGI)) QUIT:(+TMGI'>0)  DO
        . SET TMGOUT(CT)="SELF^"_$$STROUT(TMGI)
        . SET CT=CT+1
        SET TMGI=0
        FOR  SET TMGI=$ORDER(OTHERS(TMGI)) QUIT:(+TMGI'>0)  DO
        . SET TMGOUT(CT)="OTHERS^"_$$STROUT(TMGI)
        . SET CT=CT+1
        IF CT>1 SET TMGOUT(0)="1^Success"
        ELSE  SET TMGOUT(0)="0^No entries found"
        QUIT
        ;
STROUT(IEN) ; Utility function for GETFMDSV
        NEW S
        NEW ZN SET ZN=$GET(^TMG(22714,IEN,0))
        SET S=$PIECE(ZN,"^",3)_"^"                    ;"Shared
        SET S=S_$PIECE(ZN,"^",1)_"^"                  ;"Name
        NEW P SET P=+$PIECE(ZN,"^",2)                 ;"Ptr to owner
        SET S=S_$PIECE($GET(^VA(200,P,0)),"^",1)_"^"  ;"Name of owner
        NEW Y SET Y=$PIECE(ZN,"^",4)                  ;"Date, internal format
        DO DD^%DT                                     ;"convert to external
        SET S=S_Y_"^"_IEN
        QUIT S
        ;
        ;
SAVFMDV(TMGRESULT,TMGPARAMS,INPUT) ;
        ;"Purpose: To save (or edit existing) FILEMAN DESKTOP View
        ;"Input: TMGOUT -- an OUT PARAMETER, PASS BY REFERENCE.
        ;"       TMGPARAMS -- IEN^Name^Shared  or +1^Name^Shared (for a NEW entry)
        ;"       INPUT -- PASS BY REFERENCE.  Stores WP data for 2 WP fields.
        ;"                   The lines will be sent contigously, but the two fields will
        ;"                   be separated by a specific tag, as below.
        ;"                   WP lines stored as follows:
        ;"                   INPUT(0)=1st line of Description
        ;"                   INPUT(1)=2nd line of Description
        ;"                   INPUT(2)=3rd line of Description
        ;"                   ...
        ;"                   INPUT(17)="{{START OF DATA}}"  <-- tag must match exactly
        ;"                   INPUT(18)=1st line of Data
        ;"                   INPUT(29)=2nd line of Data
        ;"                   ...
        ;"                   Note: IF Description or Data is not provided when sending
        ;"                         data for an existing IEN, then prior results will
        ;"                         be deleted.
        ;"Output: TMGOUT is filled as follows:
        ;"          TMGOUT(0)="1^Success" or "-1^Message"
        ;"Results: none
        NEW temp
        SET temp=0
        IF temp=1 do
        . KILL TMGPARAMS
        . KILL INPUT
        . MERGE TMGPARAMS=^TMG("TMP","RPC","SAVFMDV","TMGPARAMS")
        . MERGE INPUT=^TMG("TMP","RPC","SAVFMDV","INPUT")
        KILL ^TMG("TMP","RPC","SAVFMDV")
        MERGE ^TMG("TMP","RPC","SAVFMDV","TMGPARAMS")=TMGPARAMS
        MERGE ^TMG("TMP","RPC","SAVFMDV","INPUT")=INPUT
        NEW TMGIENS,NAME,SHARED,LINE
        NEW TMGI SET TMGI=-1
        NEW BDATA SET BDATA=0  ;"Boolean SET when data begins
        NEW COUNT SET COUNT=1  ;"Array counter
        NEW DESC,DATA
        SET TMGIENS=$PIECE(TMGPARAMS,"^",1)
        SET NAME=$PIECE(TMGPARAMS,"^",2)
        SET SHARED=$$UP^XLFSTR($PIECE(TMGPARAMS,"^",3))
        IF SHARED'="Y" SET SHARED="N"  ;"Ensure shared is either Y or N
        ;"PARSE DESC AND DATA INTO SEPARATE ARRAYS
        FOR  SET TMGI=$ORDER(INPUT(TMGI)) QUIT:(TMGI="")  DO
        . SET LINE=INPUT(TMGI)
        . IF LINE="{{START OF DATA}}" DO
        . . SET BDATA=1
        . . SET COUNT=0
        . ELSE  IF BDATA=1 DO
        . . SET DATA(COUNT)=LINE
        . ELSE  DO
        . . SET DESC(COUNT)=LINE
        . SET COUNT=COUNT+1
        ;"
        NEW TMGFDA KILL TMGFDA
        NEW TMGMSG
        ;"Test View Data and IENS
        IF +TMGIENS'>0 DO  GOTO FDAERR
        . SET TMGRESULT(0)="-1^No record number sent."
        IF DATA(1)="" DO  GOTO FDAERR
        . SET TMGRESULT(1)="-1^No view data received."
        IF TMGIENS="+1" SET TMGIENS="+1,"
        ;"
        ;"FILE DATA
        SET TMGFDA(22714,TMGIENS,.01)=NAME
        SET TMGFDA(22714,TMGIENS,1)="`"_DUZ
        SET TMGFDA(22714,TMGIENS,2)=SHARED
        SET TMGFDA(22714,TMGIENS,2.5)="NOW"
        NEW TMGIEN
        DO UPDATE^DIE("SE","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO FDAERR
        . SET TMGRESULT(0)="-1^See Fileman message"
        . SET TMGRESULT(1)=$$GETERSTR^TMGRPC3G(.TMGMSG)
        . DO ADDFDA^TMGRPC3G(.TMGFDA,.TMGRESULT,2)
        IF TMGIENS="+1," SET TMGIENS=$GET(TMGIEN(1))_","
        ;"
        ;"FILE WP ARRAYS
        NEW DESCPARAMS,DATAPARAMS
        SET DESCPARAMS="22714^3^"_TMGIENS
        SET DATAPARAMS="22714^4^"_TMGIENS
        DO PSTWPFLD^TMGRPC3E(.TMGRESULT,.DESCPARAMS,.DESC)
        DO PSTWPFLD^TMGRPC3E(.TMGRESULT,.DATAPARAMS,.DATA)
FDAERR  QUIT

