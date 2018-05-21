TMGRPC2 ;TMG/kst/RPC Functions for Install transfer ;01/07/08, 2/2/14
         ;;1.0;TMG-LIB;**1**;01/07/08

 ;"TMG RPC FUNCTIONS for transferring files from an OLD installation
 ;"  to a NEW installation.
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

 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================


 ;"=======================================================================
 ;"=======================================================================
 ;"Dependencies:


 ;"=======================================================================
 ;"=======================================================================


Connect()
        ;"Purpose: To establish M-2-M connection, from the OLD installation
        ;"         into the NEW installation.
        ;"Input: None (for now)
        ;"Results: 1 IF connected, 0 IF NOT connected

        NEW connected
        SET connected=$$CONNECT^XWBM2MC(9230,"127.0.0.1","1>orange;orange>1")
        QUIT connected


Disconnect()
        ;"Purpose: to shut down the M-2-M connection.
        ;"Input: None
        ;"Results: 1 IF closed, 0 IF close failed

        NEW closed
        SET closed=$$CLOSE^XWBM2MC
        QUIT closed




Test
        NEW IP,PORT,AV,CONTX,RPCN
        SET IP="127.0.0.1",PORT=9230,AV="1>orange;orange>1"
        SET CONTX=""
        SET RPCN="XWB M2M EXAMPLE LARRY"
        SET RPCN="XWB M2M EXAMPLE REF"
        D EN1^XWBM2MT(CONTX,RPCN,PORT,IP,AV)
        Q
        ;


Larry ;Example of passing Array (mult).

        IF $$Connect()=0 GOTO Ldone

        I '$$SETCONTX^XWBM2MC("XWB BROKER EXAMPLE") D  QUIT
        . DO HOME^%ZIS U IO W !,"Didn't get Context"

        ;"======================================================
        WRITE "run #1",!
        S XWBPARMS("PARAMS",1,"TYPE")="ARRAY"
        S XWBPARMS("PARAMS",1,"VALUE","Raul")="Programmer"
        S XWBPARMS("PARAMS",1,"VALUE","Susan")="Tech Writter"
        S XWBPARMS("PARAMS",1,"VALUE","Dan")="Project Mgr"

        I '$$CALLRPC^XWBM2MC("XWB M2M EXAMPLE LARRY","REQ",1) D
        . DO HOME^%ZIS U IO W !,"Could not run RPC."

        W !,"Result: "
        F I=1:1 Q:'$D(REQ(I))  W !,REQ(I)
        W !

        ;"======================================================
        WRITE "run #2",!
        S XWBPARMS("PARAMS",1,"TYPE")="ARRAY"
        S XWBPARMS("PARApArrayMS",1,"VALUE","Raul")="Programmer"
        S XWBPARMS("PARAMS",1,"VALUE","Susan")="Tech Writter"
        S XWBPARMS("PARAMS",1,"VALUE","Dan")="Project Mgr"
        I '$$CALLRPC^XWBM2MC("XWB M2M EXAMPLE LARRY","REQ",1) D
        . DO HOME^%ZIS U IO W !,"Could not run RPC."

        W !,"Result: "
        F I=1:1 Q:'$D(REQ(I))  W !,REQ(I)
        W !

        ;"======================================================
        WRITE "run #3",!
        S XWBPARMS("PARApArrayMS",1,"TYPE")="ARRAY"
        S XWBPARMS("PARAMS",1,"VALUE","Raul")="Programmer"
        S XWBPARMS("PARAMS",1,"VALUE","Susan")="Tech Writter"
        S XWBPARMS("PARAMS",1,"VALUE","Dan")="Project Mgr"
        I '$$CALLRPC^XWBM2MC("XWB M2M EXAMPLE LARRY","REQ",1) D
        . DO HOME^%ZIS U IO W !,"Could not run RPC."

        W !,"Result: "
        F I=1:1 Q:'$D(REQ(I))  W !,REQ(I)
        W !

        ;"======================================================
        WRITE "run #4",!
        S XWBPARMS("PARAMS",1,"TYPE")="ARRAY"
        S XWBPARMS("PARAMS",1,"VALUE","Raul")="Programmer"
        S XWBPARMS("PARAMS",1,"VALUE","Susan")="Tech Writter"
        S XWBPARMS("PARAMS",1,"VALUE","Dan")="Project Mgr"
        I '$$CALLRPC^XWBM2MC("XWB M2M EXAMPLE LARRY","REQ",1) D
        . DO HOME^%ZIS U IO W !,"Could not run RPC."

        W !,"Result: "
        F I=1:1 Q:'$D(REQ(I))  W !,REQ(I)
        W !

        IF $$Disconnect=0 do
        . WRITE "couldn't disconnect!",!
Ldone
        Q
