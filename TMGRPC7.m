TMGRPC7 ;TMG/kst/RPC Functions for Fileman Desktop ;04/15/11
         ;;1.0;TMG-LIB;**1**;04/15/11
 ;
 ;"TMG RPC FUNCTIONS for a Fileman Desktop
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
 ;"CHANNEL(TMGRESULT,INPUT) -- general purpose channel RPC from a Fileman Desktop program
 ;
 ;"=======================================================================
 ;"Dependencies:
 ;"  TMGRPC7*, TMGFMUT*, TMGDICAT*
 ;
 ;"=======================================================================
 ;
CHANNEL(TMGRESULT,TMGINPUT) ;
        ;"Purpose: This will be a general purpose channel RPC from a Fileman Desktop program
        ;"Input: TMGRESULT -- this is an OUT parameter, and it is always passed by reference
        ;"       TMGINPUT -- this will be array of data sent from the GUI client.  Defined below:
        ;"            TMGINPUT("REQUEST")="cmd^params"  Valid values for "cmd" are:
        ;"              "ADD FM FIELD"
        ;"                   params: <empty>   Instead, input values are stored as follows:
        ;"                   TMGINPUT(1)="node1;node2;node3;...=value
        ;"                   TMGINPUT(2)="node1;node2;node3;...=value
        ;"                   TMGINPUT(3)="node1;node2;node3;...=value
        ;"                   Example:
        ;"                       "FILE=227111"
        ;"                       "FLD;NUM=13"
        ;"                       "FLD;NAME=FRIENDS"
        ;"                       "FLD;DATATYPE=7"
        ;"                       "FLD;STORE;SUBSCRIPT=3"
        ;"                       "FLD;STORE;PIECE=2"
        ;"                       "FLD;7;POINT TO WHAT FILE=PATIENT"
        ;"                   These nodes and values are parsed into an array, that
        ;"                   is taken as input array for ultimate function ADDFIELD^TMGDICATT(TMGINFO,TMGOUT)
        ;"                   Input documentation is lengthy, so see that function for details.
        ;"              "FIELD HAS DATA"
        ;"                   params: FileNum^FieldNum
        ;"              "DELETE FIELD"  <-- CAUTION!  No verify prior to delete.
        ;"                   params: FileNum^FieldNum
        ;"Output: results of this function should be put into TMGRESULTS array.
        ;"        For cmd:
        ;"          "ADD FM FIELD"
        ;"            TMGRESULT(0)="1^Success" or "-1^Message"
        ;"            TMGRESULT(1)=Additional message line (if any)
        ;"            TMGRESULT(2)=Additional message line (if any)
        ;"            etc ...
        ;"          "FIELD HAS DATA"
        ;"            TMGRESULT(0): 1=At least 1 field does has data.  0=No fields have data, or there was a problem.
 ;
        ;"Result: none
 ;
        NEW TMGXX SET TMGXX=0
        IF TMGXX=1 DO
        . KILL TMGINPUT
        . MERGE TMGINPUT=^TMG("TMP","RPC7","INPUT")
        ELSE  DO
        . KILL ^TMG("TMP","RPC7","INPUT")
        . MERGE ^TMG("TMP","RPC7","INPUT")=TMGINPUT
        KILL TMGRESULT
        NEW TMGCOMMAND,TMGCOMMAND
        SET TMGCOMMAND=$$TRIM^XLFSTR($$UP^XLFSTR($PIECE($GET(TMGINPUT("REQUEST")),"^",1)))
        SET TMGPARAMS=$$UP^XLFSTR($PIECE($GET(TMGINPUT("REQUEST")),"^",2,199))
        SET TMGRESULT(0)="-1^No command requested. Got: '"_$GET(INPUT("REQUEST"))_"'"  ;"default to error state.
        IF TMGCOMMAND="ADD FM FIELD" DO
        . KILL TMGINPUT("REQUEST")
        . DO ADDFLD^TMGRPC7A(.TMGRESULT,.TMGINPUT)
        ELSE  IF TMGCOMMAND="FIELD HAS DATA" DO
        . DO FLDHASDATA^TMGRPC7A(.TMGRESULT,TMGPARAMS)
        ELSE  IF TMGCOMMAND="DELETE FIELD" DO
        . DO DELFLD^TMGRPC7A(.TMGRESULT,TMGPARAMS)
        ;"
 ;
 ;
        QUIT
 ;
