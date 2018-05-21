TMGRPC3 ;TMG/kst/RPC Functions for GUI_Config ;10/22/14
         ;;1.0;TMG-LIB;**1**;08/31/08                  
 ;
 ;"TMG RPC FUNCTIONS for a GUI config program -- (AND CPRS uses this too)
 ;
 ;"Kevin Toppenberg MD
 ;"GNU Lessor General Public License (LGPL) applies
 ;"7/20/08
 ;
 ;"=======================================================================
 ;" RPC -- Public Functions.
 ;"=======================================================================
 ;"CHANNEL(TMGRESULT,INPUT) -- general purpose channel RPC from a GUI config program
 ;
 ;"=======================================================================
 ;"Dependencies:
 ;"  TMGRPC3* only
 ;
 ;"=======================================================================
 ;
CHANNEL(TMGRESULT,INPUT) ;
  ;"Purpose: This will be a general purpose channel RPC from a GUI config program
  ;"         Also used by FM Desktop program,
  ;"Input: TMGRESULT -- this is an OUT parameter, and it is always passed by reference
  ;"       INPUT -- this will be array of data sent from the GUI client.  Defined below:
  ;"   INPUT("REQUEST")="cmd^params"  Valid values for "cmd" are:
  ;"   "GET USER LIST"
  ;"        params: <empty> or NODISUSER
  ;"   "GET RECORDS LIST"  -- get list of all .01 fields for file.
  ;"        params: FileNumber  (e.g. 'GET RECORDS LIST^8989.3')
  ;"   "GET ONE USER"
  ;"        params: IEN (e.g. 'GET ONE USER^12345')
  ;"   "GET ONE RECORD"
  ;"        params: FileNum^IENS (e.g. 'GET ONE RECORD^200^73,')
  ;"   "GET ONE RECORD EI"  -- (external AND internal values)
  ;"        params: FileNum^IENS (e.g. 'GET ONE RECORD^200^73,')
  ;"   "GET ONE FIELD"
  ;"        params: FileNum^IENS^FldNum (e.g. 'GET ONE RECORD^200^73,^.05')
  ;"        Note: FldNum can be name or number or computed expression.
  ;"   "GET ONE WP FIELD"
  ;"        params: FileNum^Field^IENS^ (e.g. 'GET ONE WP FIELD^200^2^73,')
  ;"   "FILE ENTRY SUBSET"
  ;"        params: FileNum^ListStartValue^direction^[MaxCount]^[IENS]^[SCREEN]
  ;"   "SUBFILE ENTRY SUBSET"
  ;"        params: SubfileNum^IENS^ListStartValue^direction^[MaxCount]^[SCREEN]
  ;"   "GET SUB RECS LIST"  -- get all .01 sub record entries for a subfile
  ;"        params: SubFileNum^ParentIENS^Fields
  ;"               Fields is a fields string to pass to LIST^DIC
  ;"   "GET EXPANDED FILENAME"
  ;"        params: Filenum (or a File Name)
  ;"   "POST DATA"
  ;"        params: (not used)
  ;"        INPUT(0)=FileNum^IENS^FieldNum^FieldName^newValue^oldValue
  ;"        INPUT(1)=FileNum^IENS^FieldNum^FieldName^newValue^oldValue
  ;"        ...    (note: FieldName and oldValue are not used)
  ;"   "POST WP FIELD"
  ;"        params: FileNum^FieldNum^IENS (e.g. 'POST WP FIELD^200^2^73,')
  ;"        WP field itself is stored as follows:
  ;"        INPUT(0)=0TH line
  ;"        INPUT(1)=1st line
  ;"        INPUT(2)=2nd line
  ;"        ...
  ;"        Note: don't include INPUT("REQUEST") with text.
  ;"   "GET EMPTY ENTRY"  -- getting stub entries for subfiles typically
  ;"        params: file entry (file or subfile number)
  ;"   "GET CURRENT USER NAME" -- return name of DUZ (current) user
  ;"        (params: not used)
  ;"   "CLONE USER"
  ;"        params: SourceIENS^New.01Value
  ;"   "CLONE RECORD"
  ;"        params: FileNum^SourceIENS^New.01Value
  ;"   "GET HELP MSG"
  ;"        params     : FileNum^FieldNum^HelpType^IENS
  ;"   "IS WP FIELD"
  ;"        params: FileNum^FieldNum
  ;"   "GET ONE WP FIELD" -- retrieve on word processing (WP) field entry
  ;"        param -- FileNum^Field^IENS (e.g. 'GET ONE WP FIELD^200^2^73,')
  ;"   "REGISTER PATIENT"
  ;"        param -- FieldNum1^FieldValue1^FieldNum2^FieldValue2^FieldNum3^FieldValue3^...
  ;"   "GET FIELD INFO"
  ;"        param -- FileNum^Field
  ;"   "GET ALL FIELDS INFO"
  ;"        param -- FileNum
  ;"   "DATE TO FMDATE"
  ;"        param -- Date in external format
  ;"   "GET FMDESKTOP VIEWS"
  ;"        param -- not used
  ;"   "SAVE FMDESKTOP VIEW"
  ;"        param -- IEN(or +1)^Name^Shared
  ;"        Note: this will support sending TWO WP fields at once.
  ;"        The lines will be sent contigously, but the two fields will
  ;"        be separated by a specific tag, as below.
  ;"        WP lines stored as follows:
  ;"        INPUT(0)=1st line of Description
  ;"        INPUT(1)=2nd line of Description
  ;"        INPUT(2)=3rd line of Description
  ;"        ...
  ;"        INPUT(17)="{{START OF DATA}}"  <-- tag must match exactly
  ;"        INPUT(18)=1st line of Data
  ;"        INPUT(29)=2nd line of Data
  ;"        ...
  ;"        Note: IF Description or Data is not provided when sending
  ;"              data for an existing IEN, then prior results will
  ;"              be deleted.
  ;"   "GET REMINDER DIALOG INFO"  -- return info about file REMINDER DIALOG (#801.41)
  ;"        param -- not used
  ;"   "REMINDER DIALOG MOVE ELEMENT" -- Change ownership of a dialog element
  ;"        param -- ElementIEN^CurrentParentIEN^NewParentIEN^[NewSequenceNum] -- all IEN's are from file 801.41
  ;"   "GET REMINDER TAXONOMY INQUIRE" 
  ;"        param -- IEN from file 811.2
  ;"   "GET REMINDER TEST" 
  ;"        params: ReminderIEN^DFNorPatientName^AsOfDate(External format)^DisplayAllTerms(Y/N)
  ;"   "GET REMINDER DLG ITEM TREE ROOTS"
  ;"        param -- IEN from file 801.41
  ;"   "REMINDER DIALOG COPY TREE"
  ;"        param -- IEN^Namespace^AcceptList
  ;"           IEN - IEN in REMINDER DIALOG (801.41) file.
  ;"           NAMESPACE -- prefixed to .01 field (name).  
  ;"                   E.g. IF NAMESPACE="ZZ", and prior name was "VA-HTN", 
  ;"                   then NEW name will be "ZZ-VA-HTN"
  ;"           AcceptList -- semicolon delimited.  
  ;"                   List of component names spaces to accept
  ;"                   e.g. DG PH;ED;VA-HDL
  ;"                   If Reminder dialog subcomponents were to refer to a standard
  ;"                   library, and when copying a parent, these library items should
  ;"                   not also be copied, then these items should be noted in the
  ;"                   E.g. "DG PH" <-- will cause all subcomponents with
  ;"                          names starting with "DG PH" to not be copied
  ;"                        "ED" <-- all with name starting with "ED" not copied.
  ;"   "GET REMINDER DLG CHILD LIST" 
  ;"        param -- IEN 
  ;"                   IEN is from REMINDER DIALOG (801.41) file.
  ;"   "FIND ONE RECORD"  -- Wrapper for $$FIND1^DIC()
  ;"        params: File^IENS^Value^Flags^Indexes^Screen        
  ;"        See Fileman reference for details of input values.
  ;"        Note: Indexes should be delimited with ";", not "^" -- e.g. B;C;D 
  ;"              Screen code can not contain "^"
  ;"   "FIND RECORDS"  -- Wrapper for FIND^DIC()
  ;"        param: File^IENS^Value^Fields^Flags^Number^Indexes^Screen^Identifier        
  ;"        See Fileman reference for details of input values.
  ;"        Note: Indexes should be delimited with ";", not "^" -- e.g. B;C;D 
  ;"              Screen code can not contain "^"
  ;"   "GET CODE ROUTINE" -- Retrieve mumps code in Routine.        
  ;"        param -- RoutineName 
  ;"   "GET FUNC FINDING ARG SIGNATURE" -- returns function arguments signature
  ;"        param -- function finding name (name of record in 802.4)
  ;"        ...
  ;"   "ENQ" -- A Ping RPC, will just return ACK
  ;"        param -- none.
  ;"   "APPT DUE" -- Appointment due for patient? (As determined by text "followup" notation in notes
  ;"            params: DFN (Patient IEN) (e.g. 'APPT DUE^12345')
  ;"
  ;"Output: results of this function should be put into TMGRESULTS array.
  ;"   For cmd:
  ;"   "GET USER LIST"
  ;"     TMGRESULT(0)="1^Success" or "-1^Message"
  ;"     TMGRESULT(1)=Name^IEN^200^DISUSER   DISUSER will be 1 for "Y" or 0 for "N"
  ;"     TMGRESULT(2)=Name^IEN^200^DISUSER
  ;"     etc ...
  ;"   "GET RECORDS LIST"
  ;"     TMGRESULT(0)="1^Success" or "-1^Message"
  ;"     TMGRESULT(1)=.01Value^IEN^FileNum
  ;"     TMGRESULT(2)=.01Value^IEN^FileNum
  ;"     etc ...
  ;"   "GET ONE USER"
  ;"     TMGRESULT(0)="1^Success" or "-1^Message"
  ;"     TMGRESULT(1)=File^IENS^FieldNum^ExternalValue^DDInfo...
  ;"     TMGRESULT(2)=File^IENS^FieldNum^ExternalValue^DDInfo...
  ;"     etc ...
  ;"   "GET ONE RECORD"
  ;"     TMGRESULT(0)="1^Success" or "-1^Message"
  ;"     TMGRESULT(1)=File^IENS^FieldNum^ExternalValue^DDInfo...
  ;"     TMGRESULT(2)=File^IENS^FieldNum^ExternalValue^DDInfo...
  ;"     ...
  ;"     (note: the quotes (') shown below are NOT included.  Shown below to indicate that these ascii chars are sent)
  ;"     (the V nodes are only sent when a field of variable pointer type is sent back.)
  ;"     TMGRESULT(10)='INFO'^'DD'^FileNum^FieldNum^'V',1,0)&=&<DD ENTRY>
  ;"     TMGRESULT(12)='INFO'^'DD'^FileNum^FieldNum^'V',2,0)&=&<DD ENTRY>
  ;"     TMGRESULT(13)='INFO'^'DD'^FileNum^FieldNum^'V',3,0)&=&<DD ENTRY>
  ;"     ...
  ;"   "GET ONE RECORD EI"  -- (external AND internal values)
  ;"     TMGRESULT(0)="1^Success" or "-1^Message"
  ;"     TMGRESULT(1)=File^IENS^FieldNum^ExternalValue^InternalValue^DDInfo...
  ;"     TMGRESULT(2)=File^IENS^FieldNum^ExternalValue^InternalValue^DDInfo...
  ;"     ...
  ;"     (note: the quotes (') shown below are NOT included.  Shown below to indicate that these ascii chars are sent)
  ;"     (the V nodes are only sent when a field of variable pointer type is sent back.)
  ;"     TMGRESULT(10)='INFO'^'DD'^FileNum^FieldNum^'V',1,0)&=&<DD ENTRY>
  ;"     TMGRESULT(12)='INFO'^'DD'^FileNum^FieldNum^'V',2,0)&=&<DD ENTRY>
  ;"     TMGRESULT(13)='INFO'^'DD'^FileNum^FieldNum^'V',3,0)&=&<DD ENTRY>
  ;"     ...
  ;"   "GET ONE FIELD"
  ;"     TMGRESULT(0)="1^Success" or "-1^Message"
  ;"     TMGRESULT(1)=File^IENS^FieldNum^ExternalValue
  ;"   "GET ONE WP FIELD"
  ;"     TMGRESULT(0): "1^Success" or "-1^Message"
  ;"     TMGRESULT(1) will contain Fileman error, if any
  ;"     - or to return WP array -
  ;"     TMGRESULT(1)=1st line of text
  ;"     TMGRESULT(2)=2nd line of text
  ;"     etc..
  ;"   "FILE ENTRY SUBSET"
  ;"     TMGRESULT(0)="1^Success" or "-1^Message"
  ;"     TMGRESULT(1)=Value
  ;"     TMGRESULT(2)=Value
  ;"     etc ...
  ;"   "SUBFILE ENTRY SUBSET"
  ;"     TMGRESULT(0)="1^Success" or "-1^Message"
  ;"     TMGRESULT(1)=Value
  ;"     TMGRESULT(2)=Value
  ;"     etc ...
  ;"   "GET SUB RECS LIST"
  ;"     TMGRESULT(0)="1^Success^<Map>" or "-1^Message"
  ;"         <Map> gives the label name for each piece of the output.
  ;"              e.g. 'IEN^IX(1)^,01^2I^WID(WRITE)'
  ;"     TMGRESULT(1)=IEN^Value
  ;"     TMGRESULT(2)=IEN^Value
  ;"     ...
  ;"   "GET EXPANDED FILENAME"
  ;"     TMGRESULT(0)="1^Success" or "-1^Message"
  ;"     TMGRESULT(1)=GRANDPARENTFILENAME:PARENTILENAME:FILENAME^FILENUMBER
  ;"   "POST DATA"
  ;"     TMGRESULT(0)="1^Success" or "-1^Message"
  ;"     TMGRESULT(1)=Fileman message (long) (if generated)
  ;"     -or (if +1 etc values used)-
  ;"     TMGRESULT(1)=+5^1234  <--  results of IEN array returned (+5 converted to record 1234)
  ;"     TMGRESULT(2)=+3^2341  <--  results of IEN array returned (+3 converted to record 2341)
  ;"     ...
  ;"   "GET EMPTY ENTRY"
  ;"     TMGRESULT(0)="1^Success" or "-1^Message"
  ;"     TMGRESULT(1)=File^^FieldNum^^DDInfo...
  ;"     TMGRESULT(2)=File^^FieldNum^^DDInfo...
  ;"     etc ...
  ;"   "GET CURRENT USER NAME" -- return name of DUZ (current) user
  ;"     TMGRESULT(0)="1^Success^UserName" or "-1^Message"
  ;"   "CLONE USER"
  ;"     TMGRESULT(0)="1^Success^NewIENS" or "-1^Message"
  ;"     TMGRESULT(1)=Long Fileman message (if -1 error)
  ;"   "CLONE RECORD"
  ;"     TMGRESULT(0)="1^Success^NewIENS" or "-1^Message"
  ;"     TMGRESULT(1)=Fileman message (long) (if generated)
  ;"   "GET HELP MSG"
  ;"     TMGRESULT(0)="1^Success^NewIENS" or "-1^Message"
  ;"     TMGRESULT(1)=Fileman message (long) (if generated)
  ;"   "IS WP FIELD"
  ;"     TMGRESULT(0)="1^Success^YES/NO" or "-1^Message"
  ;"     "YES" IF is a WP subfile, otherwise "NO"
  ;"     TMGRESULT(1)=Fileman message (long) (if generated)
  ;"   "REGISTER PATIENT"
  ;"     TMGRESULT(0)="1^Success^NewIEN" or
  ;"                  "-1^Message"    Frank failure...  or
  ;"                  "0^BoolAndMessage^NewIEN".  Format:
  ;"                [Bool1;Bool2;Bool3;Bool4;Bool5*MessageText]  (e.g. '1;0;1;0;0*ErrorMsg')
  ;"                  (for Bool fields, 0=no or 1=yes)
  ;"                  Bool1 -- patient had previously been registered
  ;"                  Bool2 -- patient registered during this Fn
  ;"                  Bool3 -- problem filing data into non-identifier fields
  ;"                  Bool4 -- problem filing data into sub-file fields
  ;"                  Bool5 -- problem with filing HRN
  ;"     TMGRESULT(1)=Long Fileman message (if -1 error, or perhaps 0 code)
  ;"   "GET FIELD INFO"
  ;"     TMGRESULT(0)="1^Success^NewIENS" or "-1^Message"
  ;"     TMGRESULT(1)=Returns the 0 node from ^DD(File,Fieldnum,0)
  ;"   "GET ALL FIELDS INFO"
  ;"     TMGRESULT(0)="1^Success^NewIENS" or "-1^Message"
  ;"     TMGRESULT(1)=FileNum^FldNum^{Returns the 0 node from ^DD(File,Fieldnum,0)}
  ;"     TMGRESULT(2)=FileNum^FldNum^{Returns the 0 node from ^DD(File,Fieldnum,0)}  ... etc.
  ;"     NOTE: If the DD info contains a pointer to another file (or subfile or WP field), then that is also sent.
  ;"     e.g.TMGRESULT(3)=P2FILE^.01^DDInfo
  ;"   "DATE TO FMDATE"
  ;"     TMGRESULT(0)="-1^Error Message" or "1^FMDateTime"
  ;"   "GET FMDESKTOP SAVES"
  ;"     TMGRESULT(0)="1^Success" or "-1^Error Message"
  ;"     TMGRESULT(1)=SELF or OTHERS^Shared(Y/N)^Name^Owner^DateCreated^IEN in 22714
  ;"     TMGRESULT(2)=SELF or OTHERS^Shared(Y/N)^Name^Owner^DateCreated^IEN in 22714  ... etc
  ;"   "GET REMINDER DIALOG INFO"
  ;"     TMGRESULT(0)="1^Success" or "-1^Message"
  ;"     TMGRESULT(#)= (as below)
  ;"        piece#   Description
  ;"         1       IEN801.4
  ;"         2       Reminder dialog name
  ;"         ----- Below to match output from ORWCV START and ORWCV POL ----
  ;"         ----- only included IF there is a matching reminder definition ----
  ;"         3       IEN of linked REMINDER DEF (#811.9)    matches piece 1
  ;"         4       print name of REMINDER DEF             matches piece 2
  ;"         5-8     not used here                          matches piece 3-6
  ;"         9       Has linked active Rem Dlg (0 or 1)     matches piece 7
  ;"         10-12   not used here                          matches piece 8-10
  ;"         13      Additional check "DLGWIPE" (0 or 1)    matches piece 11
  ;"      e.g. TMGRESULT(1464)='663000719^VA-ALCOHOL F/U POS AUDIT-C^925^Positive AUDIT-C Needs Evaluation^^^^^1^^^^1'
  ;"   "REMINDER DIALOG MOVE ELEMENT"
  ;"     TMGRESULT(0)="1^Success^NewIENS" or "-1^Message"
  ;"        NewIENS is the IENS of the record *in the parent's elements list* (subfile 801.412)
  ;"   "REMINDER DIALOG COPY TREE"
  ;"     TMGRESULT(0)="1^Success^IENOfNewDialogTreeRoot^NewName" or "-1^Error Message"
  ;"   "GET REMINDER DLG CHILD LIST" 
  ;"     TMGRESULT(0)="1^Success^NewIENS" or "-1^Message"
  ;"     TMGRESULT(1)= <Child Element Name>
  ;"     TMGRESULT(2)= <Child Element Name>  etc.
  ;"   "GET REMINDER TAXONOMY INQUIRE" 
  ;"       param -- IEN from file 811.2
  ;"     TMGRESULT(0)="1^Success" or "-1^Error Message"
  ;"     TMGRESULT(1)=First line of report
  ;"     TMGRESULT(2)=Second line of report ...
  ;"   "GET REMINDER TEST" 
  ;"     TMGRESULT(0)="1^Success" or "-1^Error Message"
  ;"     TMGRESULT(1)=First line of report
  ;"     TMGRESULT(2)=Second line of report ...
  ;"   "GET REMINDER DLG ITEM TREE ROOTS"
  ;"     TMGRESULT(0)="1^Success" or "-1^Error Message"
  ;"     TMGRESULT(#)=IEN^Name
  ;"   "FIND ONE RECORD"  
  ;"     TMGRESULT(0)="1^FoundIEN" or "-1^Message"
  ;"   "FIND RECORDS" 
  ;"     TMGRESULT(0)="1^Success" or "-1^Error Message"
  ;"     TMGRESULT(#)=lines from output of FIND^DIC.  See documentation.  Example:
  ;"     TMGRESULT(1)="SRCH(0)=""51^*^0"""
  ;"     TMGRESULT(2)="SRCH(0,""MAP"")=""FID(1)"""
  ;"     TMGRESULT(3)="SRCH(1,1)=""ANRV STATE LIST""  .... etc.
  ;"   "GET CODE ROUTINE"
  ;"     TMGRESULT(0): "1^Success" or "-1^Message"
  ;"     TMGRESULT(1)=1st line of text
  ;"     TMGRESULT(2)=2nd line of text
  ;"   "GET FUNC FINDING ARG SIGNATURE" -- returns function arguments signature
  ;"     TMGRESULT(0): "1^Success" or "-1^Message"
  ;"     TMGRESULT(1)=<result>
  ;"       <result> format:  X^X^X^X^X^....
  ;"       <X> is one of following codes: F=FINDING, N=NUMBER, S=STRING
  ;"       Piece position of code matches argument position.
  ;"       e.g. 'F^N^S'  means 1st param is function, 2nd is number, 3rd is string.        
  ;"   "ENQ" -- A Ping RPC, will just return ACK
  ;"     TMGRESULT(0): "1^ACK" 
  ;"   "APPT DUE" -- Appointment due for patient?
  ;"      TMGRESULT(0)="0^<num_months>^months util appt." or 
  ;"                   "0^-1^Inactive" or
  ;"                   "1^<num_months>^months overdue" or           
  ;"                   "-1^Message"
  ;"      TMGRESULT(1)=RRGGBB, with each R,G,B being 2 digit hex number for color component
  ;"      TMGRESULT(2)=FMDATE-VISIT^FMDATE-F/U-DUE^Supporting narrative ...  
  ;"      TMGRESULT(#)=FMDATE-VISIT^FMDATE-F/U-DUE^Supporting narrative ...  
  ;"        NOTE: if follow-up date was "PRN" or "as previously schedule" etc, 
  ;"              then FMDATE-F/U-DUE value will be "1"
  ;"Result: none
  ;
  NEW DEBUG SET DEBUG=0 ;"Set to 1 at runtime to use stored input
  NEW TEMPDEBUG SET TEMPDEBUG=$GET(^TMG("TMP","TMGRPC3","DEBUGMSG"))
  IF DEBUG=0 DO
  . KILL ^TMG("TMP","RPC3")
  . MERGE ^TMG("TMP","RPC3","INPUT")=INPUT
  . SET ^TMG("TMP","RPC3","JOB")=$J
  . SET ^TMG("TMP","RPC3","IOT")=$GET(IOT)
  ELSE  DO
  . KILL INPUT MERGE INPUT=^TMG("TMP","RPC3","INPUT")
  KILL TMGRESULT
  ;"set LIVEDEBUG to 1 at runtime to trigger live debug listener, BEFORE GUI calls RPC
  NEW LIVEDEBUG SET LIVEDEBUG=0
  ;"Below is code for console process for live debugging GUI RPC's   
  IF LIVEDEBUG=1 DO  GOTO CH2
  . KILL ^TMG("TMP","TMGRPC3","LIVE DEBUG")
  . SET ^TMG("TMP","TMGRPC3","LIVE DEBUG")=1
  . FOR  DO  QUIT:$DATA(INPUT)
  . . KILL INPUT MERGE INPUT=^TMG("TMP","TMGRPC3","LIVE DEBUG","INPUT")
  . . IF $DATA(INPUT) QUIT
  . . HANG 1  ;"Wait for GUI to call CHANNEL and save INPUT info.
  ;"Below is code for process serving GUI to allow passing of debugging to
  ;"  a live console process, and getting results from there.
  IF $GET(^TMG("TMP","TMGRPC3","LIVE DEBUG"))=1 DO  GOTO CHDN  
  . KILL ^TMG("TMP","TMGRPC3","LIVE DEBUG","RESULT"),TMGRESULT
  . MERGE ^TMG("TMP","TMGRPC3","LIVE DEBUG","INPUT")=INPUT
  . FOR  DO  QUIT:$GET(^TMG("TMP","TMGRPC3","LIVE DEBUG","RESULT READY"))=1
  . . HANG 1  ;"Wait for results from other process 
  . MERGE TMGRESULT=^TMG("TMP","TMGRPC3","LIVE DEBUG","RESULT")
  . KILL ^TMG("TMP","TMGRPC3","LIVE DEBUG")
CH2;                        
  NEW TMGCOMMAND,TMGCOMMAND
  SET TMGCOMMAND=$$TRIM^XLFSTR($$UP^XLFSTR($PIECE($GET(INPUT("REQUEST")),"^",1)))
  SET TMGPARAMS=$$UP^XLFSTR($PIECE($GET(INPUT("REQUEST")),"^",2,199))
  NEW TMGORGPARAMS SET TMGORGPARAMS=$PIECE($GET(INPUT("REQUEST")),"^",2,199)
  SET TMGRESULT(0)="-1^No command requested. Got: '"_$GET(INPUT("REQUEST"))_"'"  ;"default to error state.
  IF TMGCOMMAND="GET USER LIST" DO  GOTO CHDN
  . DO GETUSRLT^TMGRPC3B(.TMGRESULT,TMGPARAMS)
  IF TMGCOMMAND="GET RECORDS LIST" DO  GOTO CHDN
  . DO GETRECLT^TMGRPC3B(.TMGRESULT,TMGPARAMS)
  ELSE  IF TMGCOMMAND="GET ONE USER" DO  GOTO CHDN
  . DO GET1USER^TMGRPC3B(.TMGRESULT,TMGPARAMS)
  ELSE  IF TMGCOMMAND="GET ONE RECORD" DO  GOTO CHDN
  . DO GET1REC^TMGRPC3B(.TMGRESULT,TMGPARAMS,"E")
  ELSE  IF TMGCOMMAND="GET ONE RECORD EI" DO  GOTO CHDN
  . DO GET1REC^TMGRPC3B(.TMGRESULT,TMGPARAMS,"EI")
  ELSE  IF TMGCOMMAND="GET ONE FIELD" DO  GOTO CHDN
  . DO GET1FLD^TMGRPC3B(.TMGRESULT,TMGPARAMS)
  ELSE  IF TMGCOMMAND="GET FIELD INFO" DO  GOTO CHDN
  . DO GETFLDIF^TMGRPC3B(.TMGRESULT,TMGPARAMS)
  ELSE  IF TMGCOMMAND="GET ALL FIELDS INFO" DO  GOTO CHDN
  . DO GETAFINF^TMGRPC3B(.TMGRESULT,TMGPARAMS)
  ELSE  IF TMGCOMMAND="FILE ENTRY SUBSET" DO  GOTO CHDN
  . DO GFLSUBST^TMGRPC3B(.TMGRESULT,TMGORGPARAMS)
  ELSE  IF TMGCOMMAND="SUBFILE ENTRY SUBSET" DO  GOTO CHDN
  . DO GSFSUBST^TMGRPC3B(.TMGRESULT,TMGPARAMS)
  ELSE  IF TMGCOMMAND="GET SUB RECS LIST" DO  GOTO CHDN
  . DO GETSRLST^TMGRPC3D(.TMGRESULT,TMGPARAMS)
  ELSE  IF TMGCOMMAND="POST DATA" DO  GOTO CHDN
  . KILL INPUT("REQUEST")
  . ;"KT 10/25/15 -- MERGE ^TMG("POST DATA","INPUT")=INPUT
  . IF TEMPDEBUG DO  QUIT        
  . . SET TMGRESULT="1^OK" 
  . DO POSTDATA^TMGRPC3C(.TMGRESULT,.INPUT)  
  ELSE  IF TMGCOMMAND="GET EMPTY ENTRY" DO  GOTO CHDN
  . DO GETEMPTY^TMGRPC3E(.TMGRESULT,TMGPARAMS)
  ELSE  IF TMGCOMMAND="GET CURRENT USER NAME" DO  GOTO CHDN
  . SET TMGRESULT(0)="1^Success^"_$PIECE($GET(^VA(200,DUZ,0)),"^",1)
  ELSE  IF TMGCOMMAND="CLONE RECORD" DO  GOTO CHDN
  . DO CLONEREC^TMGRPC3D(.TMGRESULT,TMGPARAMS)
  ELSE  IF TMGCOMMAND="CLONE USER" DO  GOTO CHDN
  . DO CLONEUSR^TMGRPC3D(.TMGRESULT,TMGPARAMS)
  ELSE  IF TMGCOMMAND="GET HELP MSG" DO  GOTO CHDN
  . DO GETHELPM^TMGRPC3E(.TMGRESULT,TMGPARAMS)
  ELSE  IF TMGCOMMAND="IS WP FIELD" DO  GOTO CHDN
  . DO GETIFWP^TMGRPC3E(.TMGRESULT,TMGPARAMS)
  ELSE  IF TMGCOMMAND="GET ONE WP FIELD" DO  GOTO CHDN
  . DO GETWPFLD^TMGRPC3E(.TMGRESULT,TMGPARAMS)
  ELSE  IF TMGCOMMAND="GET CODE ROUTINE" DO  GOTO CHDN
  . DO GETRCODE^TMGRPC3E(.TMGRESULT,TMGPARAMS)
  ELSE  IF TMGCOMMAND="DATE TO FMDATE" DO  GOTO CHDN
  . DO DTTOFMDT^TMGRPC3B(.TMGRESULT,TMGPARAMS)
  ELSE  IF TMGCOMMAND="GET EXPANDED FILENAME" DO  GOTO CHDN
  . DO EXPFNAME^TMGRPC3G(.TMGRESULT,TMGPARAMS)
  ELSE  IF TMGCOMMAND="POST WP FIELD" DO  GOTO CHDN
  . KILL INPUT("REQUEST")
  . DO PSTWPFLD^TMGRPC3E(.TMGRESULT,TMGPARAMS,.INPUT)
  ELSE  IF TMGCOMMAND="REGISTER PATIENT" DO  GOTO CHDN
  . DO REGPAT^TMGRPC3F(.TMGRESULT,TMGPARAMS)
  ELSE  IF TMGCOMMAND="GET FMDESKTOP VIEWS" DO  GOTO CHDN
  . DO GETFMDSV^TMGRPC3H(.TMGRESULT,TMGPARAMS)
  ELSE  IF TMGCOMMAND="SAVE FMDESKTOP VIEW" DO  GOTO CHDN
  . KILL INPUT("REQUEST")
  . DO SAVFMDV^TMGRPC3H(.TMGRESULT,TMGPARAMS,.INPUT)
  ELSE  IF TMGCOMMAND="GET REMINDER DIALOG INFO" DO  GOTO CHDN
  . DO GETDLGIF^TMGRPC3G(.TMGRESULT,TMGPARAMS)
  ELSE  IF TMGCOMMAND="REMINDER DIALOG MOVE ELEMENT" DO  GOTO CHDN
  . DO MOVDLGEL^TMGRPC3G(.TMGRESULT,TMGPARAMS)
  ELSE  IF TMGCOMMAND="GET REMINDER TAXONOMY INQUIRE" DO  GOTO CHDN
  . DO RPTPXRM^TMGRPC3G(.TMGRESULT,TMGPARAMS)
  ELSE  IF TMGCOMMAND="GET REMINDER TEST" DO  GOTO CHDN
  . DO TESTREM^TMGRPC3G(.TMGRESULT,TMGPARAMS)
  ELSE  IF TMGCOMMAND="GET REMINDER DLG ITEM TREE ROOTS" DO  GOTO CHDN
  . DO GDLGROOT^TMGRPC3G(.TMGRESULT,TMGPARAMS)
  ELSE  IF TMGCOMMAND="REMINDER DIALOG COPY TREE" DO  GOTO CHDN
  . DO DOCPYRMD^TMGRPC3G(.TMGRESULT,TMGPARAMS)
  ELSE  IF TMGCOMMAND="GET REMINDER DLG CHILD LIST" DO  GOTO CHDN
  . DO GETCHLST^TMGRPC3G(.TMGRESULT,TMGPARAMS)
  ELSE  IF TMGCOMMAND="FIND ONE RECORD" DO  GOTO CHDN
  . DO FIND1^TMGRPC3G(.TMGRESULT,TMGPARAMS)
  ELSE  IF TMGCOMMAND="FIND RECORDS" DO  GOTO CHDN
  . DO FIND^TMGRPC3G(.TMGRESULT,TMGPARAMS)
  ELSE  IF TMGCOMMAND="GET FUNC FINDING ARG SIGNATURE" DO  GOTO CHDN
  . DO GETFFNSG^TMGRPC3G(.TMGRESULT,TMGPARAMS) 
  ELSE  IF TMGCOMMAND="ENQ" DO  GOTO CHDN
  . SET TMGRESULT(0)="1^ACK" 
  ELSE  IF TMGCOMMAND="APPT DUE" DO  GOTO CHDN
  . DO RPCCKDUE^TMGTIU10(.TMGRESULT,TMGPARAMS)
  ;"
CHDN ;
  MERGE ^TMG("TMP","RPC3","RESULT")=TMGRESULT
  IF LIVEDEBUG DO
  . MERGE ^TMG("TMP","TMGRPC3","LIVE DEBUG","RESULT")=TMGRESULT
  . SET ^TMG("TMP","TMGRPC3","LIVE DEBUG","RESULT READY")=1
  . NEW TURNOFF SET TURNOFF=1  ;"can change at runtime to leave mode on.
  . IF TURNOFF=1 SET ^TMG("TMP","TMGRPC3","LIVE DEBUG")=0  
  ;"
  QUIT
  ;