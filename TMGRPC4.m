TMGRPC4 ;TMG/kst/RPC Functions for DxLink ;11/16/08, 2/2/14
         ;;1.0;TMG-LIB;**1**;11/16/08
 ;
 ;"TMG RPC FUNCTIONS for working with DxLink GUI application (for extracting
 ;"  PCE data, i.e. CPT codes, ICD9 codes etc.
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
 ;"CHANNEL(TMGRESULT,INPUT) -- general purpose channel RPC from a DxLink GUI program
 ;
 ;"=======================================================================
 ;"Dependencies:
 ;"  ...
 ;
 ;"=======================================================================
 ;
CHANNEL(TMGRESULT,INPUT) ;
        ;"Purpose: This will be a general purpose channel RPC from a DxLink program
        ;"Input: TMGRESULT -- this is an OUT parameter, and it is always passed by reference
        ;"       INPUT -- this will be array of data sent from the GUI client.  Defined below:
        ;"            <Stuff will go here>
        ;"            INPUT("REQUEST")="cmd^params"  Valid values for "cmd" are:
        ;"              "LOOKUP PATIENT"
        ;"                   params: LNAME,FNAME^DOB^SequelPMSAccount#
        ;"              "ENSURE VISIT"
        ;"                   params: DFN^Date@Time^DurationMins^Reason^Location^Provider^Comments
        ;"              "APPT LIST"
        ;"                   params: Date^Location^Provider(Optional)
        ;"Output: results of this function should be put into TMGRESULTS array.
        ;"        For cmd:
        ;"          "LOOKUP PATIENT"
        ;"            TMGRESULT(0)="1^Success" or "-1^Message"
        ;"            TMGRESULT(1)=DFN  (or 0 IF not found)
        ;"          "ENSURE VISIT"
        ;"            TMGRESULT(0)="1^Success" or "-1^Message"
        ;"            TMGRESULT(1)=IEN or (or 0 IF not found)
        ;"          "APPT LIST"
        ;"            TMGRESULT(0)="1^Success" or "-1^Message"
        ;"            TMGRESULT(1)=0 IF not found
        ;"            TMGRESULT(1)=DateTime^PatientName^DFN^DOB^SeqHRN^Location(Sequel ShortName)^CPTList^ICD9List
        ;"               CPTList format:  'Code#|CodeName;Code#|CodeName;Code#|CodeName...;'
        ;"               ICD9List format: 'Code#|CodeName;Code#|CodeName;Code#|CodeName...;'
        ;"Result: none
 ;
        NEW TMGCOMMAND,TMGCOMMAND
        SET TMGCOMMAND=$$TRIM^XLFSTR($$UP^XLFSTR($PIECE($GET(INPUT("REQUEST")),"^",1)))
        SET TMGPARAMS=$$UP^XLFSTR($PIECE($GET(INPUT("REQUEST")),"^",2,199))
        SET TMGRESULT(0)="-1^No valid command requested: '"_TMGCOMMAND_"'"  ;"default to error state.
        IF TMGCOMMAND="LOOKUP PATIENT" DO
        . DO LOOKUPAT^TMGRPC4B(.TMGRESULT,TMGPARAMS)
        ELSE  IF TMGCOMMAND="ENSURE VISIT" DO
        . DO ENSURVST^TMGRPC4B(.TMGRESULT,TMGPARAMS)
        ELSE  IF TMGCOMMAND="APPT LIST" DO
        . DO APPTLST^TMGRPC4B(.TMGRESULT,TMGPARAMS)
 ;
        QUIT
 ;
