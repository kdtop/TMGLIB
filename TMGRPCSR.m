TMGRPCSR ;TMG/kst/RPC entry points for Search API ; 6/4/10; 10/23/13, 2/2/14
        ;;1.0;TMG-LIB;**1**;05/25/10
        ;
 ;"RPC ENTRY POINTS FOR TMG FILEMAN SEARCH API
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
 ;"NOTE: this function depends on NEW version of LIST^DIC, from G. Timpson Patch
 ;"=======================================================================
 ;" RPC -- Public Functions.
 ;"=======================================================================
 ;"CHANNEL(TMGRESULT,INPUT) -- general purpose channel RPC from a GUI config program
 ;"LAUNCH(OUT,FILENUM,SRCHSTR) -- launch background search thread, return JOB #
 ;"STATUS(OUT,JOBNUM) --Return status of background job.
 ;"RESULTS(OUT,JOBNUM) -- Return results from background search job.
 ;"
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"
 ;"=======================================================================
 ;"=======================================================================
 ;"Dependencies:
 ;"  ^XLFSTR, ^TMGRPCS0, TMGSRCH1, TMGRPC1B
 ;"=======================================================================
 ;"=======================================================================
 ;
CHANNEL(TMGRESULT,INPUT) ;
        ;"Purpose: This will be a general purpose channel RPC from CPRS
        ;"Input: TMGRESULT -- this is an OUT parameter, and it is always passed by reference
        ;"       INPUT -- this will be array of data sent from the GUI client.  Defined below:
        ;"            <Stuff will go here>
        ;"            INPUT("REQUEST")="cmd^params"  Valid values for "cmd" are:
        ;"              "LAUNCH" -- Start background task for search
        ;"                   params: FileNumber^SearchString  <-- See docs for Search String in TMGSRCH.m
        ;"              "STATUS" --  Get status of background task
        ;"                   params: JobNumber
        ;"              "IEN LIST"   ; was RESULTS
        ;"                   params: JobNumber^Fields
        ;"                      NOTE: If Fields left blank, then NO FIELDS is assumed
        ;"              "IEN DETAILS" -- Get details of 1 IEN entry
        ;"                   params: JobNumber^IEN^[Fld#[;Fld[;Fld#[;...]]]]
        ;"                    JOBNUM -- The job number of task to query
        ;"                    IEN -- The searched-for IEN
        ;"                    Field(s) -- OPTIONAL.  Default is .01.  Format:
        ;"                      Fld#;Fld#;Fld#;... (any number of fields, separated by a ";")
        ;"                    NOTE: IF Fld is not specified, then a SET of hard-coded fields
        ;"                          will be returned, depending on what file# is encountered        
        ;"              "PREP SUBSET"
        ;"                   params: JobNumber^[Field[;FLD[;FLD...]]]
        ;"                              Field -- The desired field number(s).
        ;"                              OPTIONAL. DEFAULT is .01
        ;"                              If more than one supplied, then output is
        ;"                              concatinated.  Separate fieldnumbers with ';'
        ;"              "CLEAR" -- clear results from last search.
        ;"                   params: JobNumber
        ;"              "ALLOWED FILES ENTRY SUBSET"  -- get sublist of list .01 fields for allowed files (those pointing into FileNum)
        ;"                   params: FileNum^ListStartValue^direction^MaxCount(optional, def=44)^Simple
        ;"              "FIELD LIST SUBSET"  -- get sublist of fields names in file
        ;"                   params: FileNum^ListStartValue^direction^MaxCount(optional, def=44)^Simple
        ;"              "RESULTS LIST SUBSET"  -- get sublist of search results
        ;"                   params: JobNum^ListStartValue^direction^MaxCount(optional, def=44)
        ;"              =================================================================================
        ;"              == Calls for searching TIU DOCUMENTS                                           ==
        ;"              =================================================================================
        ;"              "PT DOCS SEARCH" -- launch a background search in documents for 1 patient
        ;"                   params: PatientEIN^SearchString
        ;"              "PT DOCS STATUS" -- Get status of background search
        ;"                   params : none
        ;"              "PT DOCS GET RESULTS" -- get result from background search
        ;"                   params : none
        ;"              "PT DOCS CLEAR" -- Tell background task to stop, and clear data array
        ;"                   params : none
        ;"              "PT DOCS STOP" -- Tell background task to stop searching
        ;"                   params : none
        ;"              "PT DOCS CHANGE SEARCH" -- tell background task to change search parameters
        ;"                   Note: this can be used to allow the search to begin while the
        ;"                         user is still entering the search terms.  If the NEW search is just an
        ;"                         extension to the prior search, then the prior search will be added on
        ;"                         rather than starting over.
        ;"                   params: PatientEIN^SearchString
        ;"              "PT DOCS PREP FOR SUBSET" -- Prep for Subset of List for TORCombobox
        ;"                   params : none
        ;"              "PT DOCS SUBSET OF RESULTS" -- Get a subset of list for TORCombobox
        ;"                   params : StartFrom^Direction^MaxCount
        ;"                      Direction and Maxcount are optional, def=1, 44 respectively
        ;"              =================================================================================
        ;"Output: results of this function should be put into TMGRESULTS array.
        ;"        For cmd:
        ;"          "LAUNCH"
        ;"            TMGRESULT(0)=1^JobNumber
        ;"          "STATUS"
        ;"            TMGRESULT(0)=1^%Done^Message.   <-- Will be '1^100^#DONE#' when task is done.
        ;"          "IEN LIST"
        ;"            TMGRESULT(0)=1 IF Success or -1^Message"
        ;"            TMGRESULT(1)=IEN^[Fld Value]  <-- Field value returned, IF requested
        ;"            TMGRESULT(2)=IEN^[Fld Value]
        ;"            etc ...
        ;"          "IEN DETAILS" -- Get details of 1 IEN entry
        ;"            TMGRESULT(0)=1 IF Success or -1^Message"
        ;"            TMGRESULT(1)=FileNum^IENInFile^FieldValue^FieldValue^FieldValue^...
        ;"                                 (a piece returned for each requested field)        
        ;"          "PREP SUBSET"
        ;"            TMGRESULT(0)=1^Success or -1^Message
        ;"          "CLEAR"
        ;"            TMGRESULT(0)=1^Success
        ;"          "ALLOWED FILES ENTRY SUBSET"
        ;"            TMGRESULT(0)="1^Success" or "-1^Message"
        ;"            TMGRESULT(1)=FileNum^FileName
        ;"            TMGRESULT(2)=FileNum^FileName
        ;"            etc ...
        ;"          "FIELD LIST SUBSET"
        ;"            TMGRESULT(0)="1^Success" or "-1^Message"
        ;"            TMGRESULT(1)=FLDNum^Name^Info
        ;"            TMGRESULT(2)=FLDNum^Name^Info
        ;"            etc ...
        ;"          "RESULTS LIST SUBSET"
        ;"            TMGRESULT(0)="1^Success" or "-1^Message"
        ;"            TMGRESULT(1)=IENNum^RequestedFieldNames
        ;"            TMGRESULT(2)=IENNum^RequestedFieldNames
        ;"            etc ...
        ;"          =================================================================================
        ;"          == Calls for searching TIU DOCUMENTS                                           ==
        ;"          =================================================================================
        ;"          "PT DOCS SEARCH"
        ;"              TMGRESULT(0)="1^Success", OR -1^ErrorMsg
        ;"          "PT DOCS STATUS"
        ;"              TMGRESULT(0)="1^Status" or -1^ErrorMessage
        ;"                 NOTe: will return 1^DONE when done with search.
        ;"          "PT DOCS GET RESULTS"
        ;"              TMGRESULT(0)=FoundCount^Success, or -1^Message
        ;"              TMGRESULT(1)=IEN1
        ;"              TMGRESULT(2)=IEN2 ... etc.
        ;"          "PT DOCS CLEAR"
        ;"              TMGRESULT(0)="1^Success
        ;"          "PT DOCS STOP"
        ;"              TMGRESULT(0)="1^Success
        ;"          "PT DOCS PREP FOR SUBSET"
        ;"              TMGRESULT(0)="1^Success", OR -1^ErrorMsg
        ;"          "PT DOCS SUBSET OF RESULTS"
        ;"              TMGRESULT(0)="1^Success" or "-1^Message"
        ;"              TMGRESULT(1)=IEN^ANoteIdentifier
        ;"              TMGRESULT(2)=IEN^ANoteIdentifier
        ;"          =================================================================================
        ;"Result: none
        ;
        NEW TMGCOMMAND,TMGCOMMAND
        SET TMGCOMMAND=$$TRIM^XLFSTR($$UP^XLFSTR($PIECE($GET(INPUT("REQUEST")),"^",1)))
        ;"//kt 10/15/13 original --> SET TMGPARAMS=$$UP^XLFSTR($PIECE($GET(INPUT("REQUEST")),"^",2,199))
        SET TMGPARAMS=$PIECE($GET(INPUT("REQUEST")),"^",2,199)
        NEW TMGDEBUGSRCH SET TMGDEBUGSRCH=0
        IF TMGDEBUGSRCH=1 DO
        . SET TMGCOMMAND=$GET(^TMG("TMP","RPC","TMGRPCSR","TMGCOMMAND"))
        . SET TMGPARAMS=$GET(^TMG("TMP","RPC","TMGRPCSR","TMGPARAMS"))
        ELSE  DO
        . KILL ^TMG("TMP","RPC","TMGRPCSR")
        . MERGE ^TMG("TMP","RPC","TMGRPCSR","TMGCOMMAND")=TMGCOMMAND
        . MERGE ^TMG("TMP","RPC","TMGRPCSR","TMGPARAMS")=TMGPARAMS
        ;
        SET TMGRESULT(0)="-1^No command requested."  ;"default to error state.
        IF TMGCOMMAND="LAUNCH" DO
        . DO LAUNCH^TMGRPCS0(.TMGRESULT,TMGPARAMS)
        IF TMGCOMMAND="STATUS" DO
        . DO STATUS^TMGRPCS0(.TMGRESULT,TMGPARAMS)
        ELSE  IF TMGCOMMAND="IEN LIST" DO
        . DO IENLIST^TMGRPCS0(.TMGRESULT,TMGPARAMS)
        ELSE  IF TMGCOMMAND="IEN DETAILS" DO
        . DO IENDETAL^TMGRPCS0(.TMGRESULT,TMGPARAMS)
        ELSE  IF TMGCOMMAND="RESULTS" DO
        . DO IENLIST^TMGRPCS0(.TMGRESULT,TMGPARAMS)
        ELSE  IF TMGCOMMAND="PREP SUBSET" DO
        . DO PREPSB^TMGRPCS0(.TMGRESULT,TMGPARAMS)
        ELSE  IF TMGCOMMAND="CLEAR" DO
        . DO CLEAR^TMGRPCS0(.TMGRESULT,TMGPARAMS)
        ELSE  IF TMGCOMMAND="ALLOWED FILES ENTRY SUBSET" DO
        . DO GETAFSUB^TMGSRCH1(.TMGRESULT,TMGPARAMS)
        ELSE  IF TMGCOMMAND="FIELD LIST SUBSET" DO
        . DO GETFLDSB^TMGSRCH1(.TMGRESULT,TMGPARAMS)
        ELSE  IF TMGCOMMAND="RESULTS LIST SUBSET" DO
        . DO GETRSLTSB^TMGRPCS0(.TMGRESULT,TMGPARAMS)
        ELSE  IF TMGCOMMAND="PT DOCS SEARCH" DO
        . DO PDSRCH^TMGRPCS1(.TMGRESULT,TMGPARAMS)
        ELSE  IF TMGCOMMAND="PT DOCS STATUS" DO
        . DO PDSTATUS^TMGRPCS1(.TMGRESULT,TMGPARAMS)
        ELSE  IF TMGCOMMAND="PT DOCS GET RESULTS" DO
        . DO PDRESULT^TMGRPCS1(.TMGRESULT,TMGPARAMS)
        ELSE  IF TMGCOMMAND="PT DOCS CLEAR" DO
        . DO PDCLEAR^TMGRPCS1(.TMGRESULT,TMGPARAMS)
        ELSE  IF TMGCOMMAND="PT DOCS STOP" DO
        . DO PDSTOP^TMGRPCS1(.TMGRESULT,TMGPARAMS)
        ELSE  IF TMGCOMMAND="PT DOCS CLEAR" DO
        . DO PDCLEAR^TMGRPCS1(.TMGRESULT,TMGPARAMS)
        ELSE  IF TMGCOMMAND="PT DOCS PREP FOR SUBSET" DO
        . DO PDPREPSS^TMGRPCS1(.TMGRESULT,TMGPARAMS)
        ELSE  IF TMGCOMMAND="PT DOCS SUBSET OF RESULTS" DO
        . DO PDGETSS^TMGRPCS1(.TMGRESULT,TMGPARAMS)
        ;
        QUIT
        ;
INSTALL ;
        ;"Purpose: to add the RPC's to the OPTION record OR CPRS GUI CHART
        DO ENSURE1^TMGRPC1B("TMG SEARCH CHANNEL")
        QUIT
 ;