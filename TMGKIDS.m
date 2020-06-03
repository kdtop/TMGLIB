TMGKIDS ;TMG/kst/Code used for pre and post routines for KIDS build ;04/16/08, 2/2/14
         ;;1.0;TMG-LIB;**1**;04/16/08
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
POSTINST
        ;"Purpose: To provide a function that KIDS can call after installing patch.
        ;"This particular function will add custom RPC entries to the RPC field in
        ;"the OPTION field in OR CPRS GUI CHART.

D1      ;"Below is a data list, not simple comments
        ;;TMG ADD PATIENT
        ;;TMG AUTOSIGN TIU DOCUMENT
        ;;TMG BARCODE DECODE
        ;;TMG BARCODE ENCODE
        ;;TMG DOWNLOAD FILE
        ;;TMG DOWNLOAD FILE DROPBOX
        ;;TMG GET BLANK TIU DOCUMENT
        ;;TMG GET DFN
        ;;TMG GET IMAGE LONG DESCRIPTION
        ;;TMG GET PATIENT DEMOGRAPHICS
        ;;TMG SET PATIENT DEMOGRAPHICS
        ;;TMG UPLOAD FILE
        ;;TMG UPLOAD FILE DROPBOX
        ;;TMG CPRS GET URL LIST
        ;;--END OF LIST--

        NEW ienORCPRS,DIC,X,Y
        ;"set ienORCPRS= ... find in OPTION file.
        SET DIC=19  ;"OPTION file
        SET X="OR CPRS GUI CHART"
        DO ^DIC
        SET ienORCPRS=+$PIECE(Y,"^",1)
        IF ienORCPRS'>0 DO  GOTO PostDone
        . WRITE !,!,"Sorry, unable to locate OR CPRS GUI CHART in OPTION file.",!
        . WRITE "Unable to add TMG's RPC's to allowed list of RPC's for CPRS.",!

        NEW i,rpcName
        for i=1:1 DO  QUIT:(rpcName="")
        . SET rpcName=$text(D1+i^TMGKIDS)
        . SET rpcName=$PIECE(rpcName,";;",2)
        . IF rpcName="--END OF LIST--" SET rpcName=""
        . IF rpcName="" QUIT
        . DO AddRPC(ienORCPRS,rpcName)

PostDone
        QUIT



AddRPC(IENOption,RPCName)
        ;"Purpose: To add the RPC Name to the RPC subfile in the Option record,
        ;"         given by IENOption
        ;"Note: If IENRPC is already present, then it won't be added again.

        ;"See IF RPC is already present, to avoid duplication
        NEW DIC,TMGD0,X,Y
        SET TMGD0=IENOption
        SET X=RPCName
        SET DIC="^DIC(19,"_IENOption_",""RPC"","
        SET DIC(0)="MZ"
        DO ^DIC

        WRITE RPCName
        IF +Y'>0 do
        . ;"code to add RPC here.
        . NEW TMGFDA,TMGIEN,TMGMSG
        . SET TMGFDA(19.05,"+1,"_IENOption_",",.01)=RPCName
        . DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
        . DO SHOWDIER^TMGDEBU2(.TMGMSG)
        . WRITE ?30,"... Added as allowed RPC from CPRS",!
        ELSE  do
        . WRITE ?30,"... already present",!

        QUIT



