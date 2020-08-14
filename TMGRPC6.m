TMGRPC6 ;TMG/kst/RPC Functions for tmg-messenger ;09/17/09
         ;;1.0;TMG-LIB;**1**;09/17/09
 ;
 ;"TMG RPC FUNCTIONS for TMG-Messenger program
 ;
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
 ;"CHANNEL(TMGRESULT,INPUT) -- general purpose channel RPC from a GUI config program
 ;
 ;"=======================================================================
 ;"Dependencies:
 ;" TMGRPC6A
 ;"=======================================================================
 ;
CHANNEL(TMGRESULT,INPUT) ;
        ;"Purpose: This will be a general purpose channel RPC from a GUI Email program
        ;"         Called from RPC-- TMG MSGLINK CHANNEL;
        ;"Input: TMGRESULT -- this is an OUT parameter, and it is always passed by reference
        ;"       INPUT -- this will be array of data sent from the GUI client.  Defined below:
        ;"            INPUT("REQUEST")="cmd^params"  Valid values for "cmd" are:
        ;"              "GET USERS OF EMAIL ADDRESS"
        ;"                   params: email address.  e.g. SomeUser@gmail.com
        ;"                     e.g. INPUT("REQUEST")="GET USERS OF EMAIL ADDRESS^SomeUser@gmail.com"
        ;"              "GET MULT USERS EMAIL ADDRESSES"
        ;"                   params: patient IENs list separated by ';'  e.g.  1234;2345;234
        ;"                     e.g. INPUT("REQUEST")="GET USERS EMAIL ADDRESSES^1234;2345;234"
        ;"              "SET ONE USER EMAIL ADDRESS"
        ;"                   params: patient IEN^NewAddress^AltEmail1^AltEmail2^...
        ;"                     e.g. INPUT("REQUEST")="GET USER EMAIL ADDRESS^1234^MyMail@server.com"
        ;"              "SET USERS EMAIL ADDRESSES"
        ;"                   params: IEN1=NewAddress1;IEN2=NewAddress2;IEN3=NewAddress3 ....
        ;"              "FILE ENTRY SUBSET"
        ;"                   params: FileNum^ListStartValue^direction^MaxCount(optional, def=44)
        ;"              "GET IEN 8925 FOR IMAP UID"
        ;"                   params: UID  -- this is the ID for message returned by IMAP
        ;"              "SET IMAP UID FOR IEN 8925"
        ;"                   params: IEN8925^UID
        ;"              "KILL ASAVE"
        ;"                   params: IEN200^IEN8925
        ;"                       IEN200 -- the DUZ or user IEN in the NEW PERSON file
        ;"                       IEN8925 -- the IEN of the document in file 8925 for which ASAVE needs to be deleted.
        ;"              "KILL USER EMAIL ADDRESS"
        ;"                   params: PatientIEN^BadEmailAddress^bademail2^bademail3^...
        ;"              "ALTER USER EMAIL ADDRESS"
        ;"                   params: PatientIEN^OldAddressValue^NewAddressValue
        ;"              "GET DOC TITLE EMAIL IEN"
        ;"                   params: not used. Values ignored.
        ;"              "GET HIPPA CONSENT CODES"
        ;"                   params: PatientIEN^
        ;"              "SET HIPPA CONSENT CODES"
        ;"                   params: PatientIEN^Codes
        ;"                   Codes should be one of values below:
        ;"                            E - Email consented;
        ;"                           EC - email & cell msg consented;
        ;"                            C - just cell msg consented.
        ;"                            N - NOT CONSENTED

        ;"
        ;"Output: results of this function should be put into TMGRESULTS array.
        ;"        For cmd:
        ;"    "GET USERS OF EMAIL ADDRESS"
        ;"               TMGRESULT(0)="#Found^Success" or "-1^Message"
        ;"          e.g. 1^Success --> 1 match found
        ;"               2^Success --> 2 matches found
        ;"               0^Success --> no errors, but no matches found.
        ;"         TMGRESULT(1)=Name^DOB^IEN2
        ;"         TMGRESULT(2)=Name^DOB^IEN2
        ;"         etc ...
        ;"    "GET MULT USERS EMAIL ADDRESSES"
        ;"               TMGRESULT(0)="#Found^Success" or "-1^Message"
        ;"          e.g. 1^Success --> 1 match found
        ;"               2^Success --> 2 matches found
        ;"               0^Success --> no errors, but no matches found.
        ;"         TMGRESULT(1)=IEN1^EmailAddress
        ;"         TMGRESULT(2)=IEN1^;ALT;EmailAddress
        ;"         TMGRESULT(3)=IEN2^EmailAddress
        ;"         etc ...
        ;"    "SET USER EMAIL ADDRESS"
        ;"               TMGRESULT(0)="#Found^Success" or "-1^Message"
        ;"    "SET USERS EMAIL ADDRESSES"
        ;"               TMGRESULT(0)="#Found^Success" or "-1^Message"
        ;"    "FILE ENTRY SUBSET"
        ;"            TMGRESULT(0)="1^Success" or "-1^Message"
        ;"            TMGRESULT(1)=Value
        ;"            TMGRESULT(2)=Value
        ;"            etc ...
        ;"    "KILL ASAVE"
        ;"            TMGRESULT(0)="0^Success", or "-1^Message"
        ;"    "KILL USER EMAIL ADDRESS"
        ;"            TMGRESULT(0)="1^Success" or "-1^Message"
        ;"    "ALTER USER EMAIL ADDRESS"
        ;"            TMGRESULT(0)="1^Success" or "-1^Message"
        ;"    "GET DOC TITLE EMAIL IEN"
        ;"            TMGRESULT(0)="-1^Message" or TMG(0)=IEN^Name
        ;"    "GET HIPPA CONSENT CODES"
        ;"            TMGRESULT(0)="1^Codes" or TMG(0)=-1^Codes
        ;"            Codes will be one of values below:
        ;"                E - Email consented;
        ;"               EC - email & cell msg consented;
        ;"                C - just cell msg consented.
        ;"                N - NOT CONSENTED
        ;"    "SET HIPPA CONSENT CODES"
        ;"            TMGRESULT(0)="1^Success" or "-1^Message"
 ;
        ;"Result: none
 ;
        NEW TMGCOMMAND,TMGCOMMAND
        SET TMGCOMMAND=$$TRIM^XLFSTR($$UP^XLFSTR($PIECE($GET(INPUT("REQUEST")),"^",1)))
        SET TMGPARAMS=$PIECE($GET(INPUT("REQUEST")),"^",2,199)
        SET TMGRESULT(0)="-1^No command requested."  ;"default to error state.
        IF TMGCOMMAND="GET USERS OF EMAIL ADDRESS" DO
        . DO GETEMULT^TMGRPC6A(.TMGRESULT,TMGPARAMS)
        ELSE  IF TMGCOMMAND="GET MULT USERS EMAIL ADDRESSES" DO
        . DO GETUEMA^TMGRPC6A(.TMGRESULT,TMGPARAMS)
        ELSE  IF TMGCOMMAND="SET ONE USER EMAIL ADDRESS" DO
        . DO SETUEMA^TMGRPC6A(.TMGRESULT,TMGPARAMS)
        ELSE  IF TMGCOMMAND="SET USERS EMAIL ADDRESSES" DO
        . DO SETUSEM^TMGRPC6A(.TMGRESULT,TMGPARAMS)
        ELSE  IF TMGCOMMAND="FILE ENTRY SUBSET" DO
        . DO GFLSUBST^TMGRPC6A(.TMGRESULT,TMGPARAMS)
        ELSE  IF TMGCOMMAND="KILL ASAVE" DO
        . ;"doesn't exist, fix later --> DO KILASAVE^TMGRPC6A(.TMGRESULT,TMGPARAMS)
        ELSE  IF TMGCOMMAND="GET IEN 8925 FOR IMAP UID" DO
        . DO GETIEN8925^TMGRPC6A(.TMGRESULT,TMGPARAMS)
        ELSE  IF TMGCOMMAND="SET IMAP UID FOR IEN 8925" DO
        . DO SETUID^TMGRPC6A(.TMGRESULT,TMGPARAMS)
        ELSE  IF TMGCOMMAND="KILL USER EMAIL ADDRESS" DO
        . DO KILLUEMA^TMGRPC6A(.TMGRESULT,TMGPARAMS)
        ELSE  IF TMGCOMMAND="ALTER USER EMAIL ADDRESS" DO
        . DO ALTEREMA^TMGRPC6A(.TMGRESULT,TMGPARAMS)
        ELSE  IF TMGCOMMAND="GET DOC TITLE EMAIL IEN" DO
        . DO GETEMDOC^TMGRPC6A(.TMGRESULT,TMGPARAMS)
        ELSE  IF TMGCOMMAND="GET HIPPA CONSENT CODES" DO
        . DO GETCONSNT^TMGRPC6A(.TMGRESULT,TMGPARAMS)
        ELSE  IF TMGCOMMAND="SET HIPPA CONSENT CODES" DO
        . DO SETCONSNT^TMGRPC6A(.TMGRESULT,TMGPARAMS)
 ;
        QUIT
 ;
