TMGRPC1A ;TMG/kst-RPC Functions ;2/11/10, 6/19/10, 7/11/12, 2/2/14, 3/24/21
         ;;1.0;TMG-LIB;**1**;2/11/10
 ;
 ;"TMG RPC FUNCTIONS related to CPRS
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
 ;"SETINIVL(RESULT,SECTION,KEY,VALUE) ;Entry point for TMG INIFILE SET
 ;"GETINIVL(RESULT,SECTION,KEY,DEFAULT) ;Entry point for TMG INIFILE GET
 ;"SETLOG(RESULT,TMGDFN,DUZ,TYPE,IEN,TIME) ;Store a CPRS access log entry
 ;"GETLOG(RESULT,DUZ,SDATE,EDATE,TMGDFN,TYPE) ;Retrieve access log entries
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;"Dependencies:
 ;" DIC, TMGDEBUG
 ;"=======================================================================
 ;"=======================================================================
 ;
SETINIVL(RESULT,SECTION,KEY,VALUE) ;
        ;"SCOPE: Public
        ;"RPC that calls this: TMG INIFILE SET
        ;"Purpose: To provide an entry point for a RPC call from a client.  The client
        ;"         will use this instead of TIniFile object in Delphi.
        ;"         Note: Since all data are of type string in Mumps, this will work only with strings.
        ;"               and type casting will have to take place in client.
        ;"Input: RESULT  -- an OUT PARAMETER.  See output below.
        ;"       SECTION -- String of 'Section' to store setting in (corresponds to section in TIniFile)
        ;"       KEY     -- String of Key value.  (corresponds to Ident/Key in TIniFile)
        ;"       VALUE   -- String of Value to set
        ;"Note: Because this is a shared resource, it is expected that the client will use
        ;"      User.Name as the Section value.
        ;"      Also, any prior value will be overwritten.
        ;"Output: Will return RESULT="1^Success", or -1^Error Message"
        SET RESULT="1^Success"
        IF $GET(SECTION)="" SET RESULT="-1^No value passed for SECTION" QUIT
        IF $GET(KEY)="" SET RESULT="-1^No value passed for KEY" QUIT
        SET VALUE=$GET(VALUE)
        NEW X,Y,DIC,IEN,IEN2
        SET DIC=22710,DIC(0)="LM" ;"Find SECTION IF previously added.
        SET X=SECTION
        DO ^DIC SET IEN=+Y
        IF IEN'>0 DO  ;"For some reason LAYGO doesn't work when called by RPC
        . NEW TMGFDA,TMGIEN,TMGMSG
        . SET TMGFDA(22710,"+1,",.01)=SECTION
        . DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) DO  QUIT
        . . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        . SET IEN=+$GET(TMGIEN(1))
        IF +RESULT=-1 GOTO SIDN
        IF IEN'>0 SET RESULT="-1^Error establishing SECTION: ["_SECTION_"]" QUIT
        SET DA(1)=IEN,DIC(0)="LM",DIC="^TMG(22710,"_IEN_",1,"
        SET X=KEY
        DO ^DIC SET IEN2=+Y
        IF IEN2'>0 DO  ;"For some reason LAYGO sometimes doesn't work when called by RPC
        . NEW TMGFDA,TMGIEN,TMGMSG
        . SET TMGFDA(22710.01,"+1,"_IEN_",",.01)=KEY
        . DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) DO  QUIT
        . . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        . SET IEN2=+$GET(TMGIEN(1))
        IF +RESULT=-1 GOTO SIDN
        IF IEN2'>0 SET RESULT="-1^Error establishing KEY: ["_KEY_"]" QUIT
        NEW TMGFDA,TMGMSG
        IF VALUE="" SET VALUE="@"
        SET TMGFDA(22710.01,IEN2_","_IEN_",",1)=VALUE
        DO FILE^DIE("E","TMGFDA","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO SIDN
        . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
SIDN    QUIT
 ;
 ;
GETINIVL(RESULT,SECTION,KEY,DEFAULT) ;
        ;"SCOPE: Public
        ;"RPC that calls this: TMG INIFILE GET
        ;"Purpose: To provide an entry point for a RPC call from a client.  The client
        ;"         will use this instead of TIniFile object in Delphi.
        ;"         Note: Since all data are of type string in Mumps, this will work only with strings.
        ;"               and type casting will have to take place in client.
        ;"Input: RESULT  -- an OUT PARAMETER.  See output below.
        ;"       SECTION -- String of 'Section' to store setting in (corresponds to section in TIniFile)
        ;"       KEY     -- String of Key value.  (corresponds to Ident/Key in TIniFile)
        ;"       DEFAULT -- The value to be returned, IF no value found.
        ;"Note: Because this is a shared resource, it is expected that the client will use
        ;"      User.Name as the Section value.
        ;"      Also, any prior value will be overwritten.
        ;"Output: Will return RESULT="1^<Value>", or -1^Error Message"
        IF $GET(SECTION)="" SET RESULT="-1^No value passed for SECTION" QUIT
        IF $GET(KEY)="" SET RESULT="-1^No value passed for KEY" QUIT
        SET DEFAULT=$GET(DEFAULT)
        NEW VALUE SET VALUE=$$GETVALUE(SECTION,KEY)
        IF VALUE="-1^NOT FOUND" SET VALUE=$$GETVALUE("DEFAULT",KEY)
        IF VALUE="-1^NOT FOUND" SET VALUE=DEFAULT
        SET RESULT="1^"_VALUE
        QUIT
 ;
GETVALUE(SECTION,KEY)   ;
        ;"SCOPE: Private
        ;"Purpose: Returns value of section, key combination
        ;"Input: SECTION -- String of 'Section' to store setting in (corresponds to section in TIniFile)
        ;"       KEY     -- String of Key value.  (corresponds to Ident/Key in TIniFile)
        ;"Return: Either ini value or "-1^NOT FOUND"
        NEW X,Y,DIC,IEN,IEN2
        SET DIC=22710,X=SECTION
        DO ^DIC SET IEN=+Y
        IF IEN'>0 SET RESULT="-1^NOT FOUND" GOTO GVDN
        SET DA(1)=IEN,DIC="^TMG(22710,"_IEN_",1,"
        SET X=KEY
        DO ^DIC SET IEN2=+Y
        IF IEN2'>0 SET RESULT="-1^NOT FOUND" GOTO GVDN
        NEW VALUE SET VALUE=$GET(^TMG(22710,IEN,1,IEN2,1),DEFAULT)
        IF VALUE="" SET VALUE="-1^NOT FOUND"
        SET RESULT=VALUE
GVDN    QUIT RESULT
 ;
CONVERT
        ;"Purpose: A temp function to convert between the old storage method and the new.
        ;"Data was stored in: ^TMG("INIDATA",Section,Key,Vaue)
        NEW SECTION,KEY,VALUE
        SET SECTION=""
        FOR  SET SECTION=$ORDER(^TMG("INIDATA",SECTION)) QUIT:(SECTION="")  DO
        . SET KEY=""
        . FOR  SET KEY=$ORDER(^TMG("INIDATA",SECTION,KEY)) QUIT:(KEY="")  DO
        . . SET VALUE=$GET(^TMG("INIDATA",SECTION,KEY))
        . . NEW RESULT
        . . DO SETINIVL(.RESULT,SECTION,KEY,VALUE) ;
        . . IF +RESULT>0 KILL ^TMG("INIDATA",SECTION,KEY) QUIT
        . . WRITE "Error trying to store SECTION=",SECTION,"; KEY=",KEY,"; VALUE=",VALUE,!
        . . WRITE " -- ",$PIECE(RESULT,"^",2),!
        QUIT
 ;
INSTALL ;
        ;"Purpose: to add the RPC's to the OPTION record OR CPRS GUI CHART
        NEW DIC,X,Y,DA
        SET DIC="^DIC(19,",DIC(0)="M"
        SET X="OR CPRS GUI CHART"
        DO ^DIC
        IF +Y'>0 DO  QUIT
        . WRITE "ERROR.  Unable to find [OR CPRS GUI CHART] in file OPTION (#19)",!
        . NEW TEMP READ "Press [ENTER] to continue...",TEMP:($GET(DTIME,3600))
        . WRITE !
        SET DA(1)=+Y
        SET DIC=DIC_DA(1)_",""RPC"","
        SET DIC(0)="ML" ;"LAYGO --> add entry IF not found
        SET X="TMG INIFILE GET"
        DO ^DIC
        IF +Y'>0 DO
        . WRITE "ERROR.  Unable to add or find TMG INIFILE GET for subfile RPC in record",!
        . WRITE "OR CPRS GUI CHART in file OPTION (#19)",!
        . NEW TEMP READ "Press [ENTER] to continue...",TEMP:($GET(DTIME,3600))
        . WRITE !
        SET X="TMG INIFILE SET"
        DO ^DIC
        IF +Y'>0 DO
        . WRITE "ERROR.  Unable to add or find TMG INIFILE SET for subfile RPC in record",!
        . WRITE "OR CPRS GUI CHART in file OPTION (#19)",!
        . NEW TEMP READ "Press [ENTER] to continue...",TEMP:($GET(DTIME,3600))
        . WRITE !
        QUIT
 ;
SETLOG(RESULT,TMGDFN,DUZ,TYPE,IEN,DTIME) ;
        ;"Purpose: Store an access log entry, from action in CPRS, stored in file 22715
        ;"Input: RESULT -- the output of the function (single value)
        ;"       TMGDFN --the patient IEN (file 2)
        ;"       DUZ -- the user IEN (file 200)
        ;"       TYPE: this is the type of action, and should be acceptible for
        ;"              input into the EVENT TYPE field in file TMG CPRS PATIENT ACCESS LOG.
        ;"       IEN -- This will be used to fill the POINTER FIELD in
        ;"              the file TMG CPRS PATIENT ACCESS LOG.  My be null value
        ;"              IF TYPE does not refer to data that stores a pointer.
        ;"              E.g. IF TYPE is NOTE, then IEN will be IEN in file 8925.
        ;"       DTIME -- OPTIONAL.  Default is "NOW".  The date-time of the action.
        ;"Result: none
        ;"Output: RESULT variable will be 1^OK, or -1^Error Message
        NEW TMGDFA,TMGIEN,TMGIENS,TMGMSG
        SET RESULT="1^OK"
        SET TMGDFN=+$GET(TMGDFN)
        IF TMGDFN'>0 DO  GOTO SLDN
        . SET RESULT="-1^Value for DFN no provided"
        SET DUZ=+$GET(DUZ)
        IF DUZ'>0 DO  GOTO SLDN
        . SET RESULT="-1^Value for DUZ no provided"
        SET TYPE=$GET(TYPE)
        IF TYPE="" DO  GOTO SLDN
        . SET RESULT="-1^Value for TYPE no provided"
        SET IEN=+$GET(IEN)
        SET DTIME=$GET(DTIME,"NOW")
        IF DTIME'>0 DO
        . NEW %DT,X,Y SET %DT="ST"
        . SET X=DTIME
        . DO ^%DT
        . IF Y>0 SET DTIME=Y
        IF DTIME'>0 DO  GOTO SLDN
        . SET RESULT="-1^Invalid date-time for event.  Got: "_DTIME
        IF $DATA(^TMG(22715,TMGDFN))>0 GOTO SL2
        SET TMGIEN(1)=TMGDFN
        SET TMGFDA(22715,"+1,",.01)=TMGDFN
        DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO SLDN
        . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
SL2     SET TMGIENS="+1,"_DFN_","
        KILL TMGFDA,TMGIEN
        SET TMGFDA(22715.01,TMGIENS,.01)=DTIME
        SET TMGFDA(22715.01,TMGIENS,1)=TYPE
        IF IEN>0 SET TMGFDA(22715.01,TMGIENS,2)="`"_IEN
        DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO SLDN
        . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
SLDN    QUIT
        ;
GETLOG(RESULT,DUZ,SDATE,EDATE,TMGDFN,TYPE) ;
        ;"Purpose: Retrieve access log entries
        ;"Input: RESULT -- the output of the function (and array)
        ;"       DUZ -- the user IEN (file 200) that created event entries
        ;"       SDATE -- The start date-time of log range.  Should be a Fileman
        ;"                numeric date, or word such as "NOW" or "T-1" etc.
        ;"       EDATE -- Optional (Default=NOW). The end date-time of log range.
        ;"                Should be a Fileman numeric date, or word such as "NOW"
        ;"                or "T-1" etc.
        ;"       TMGDFN --Optional.  A patient IEN (file 2) to filter log entries by
        ;"       TYPE: Optional.  A Type to filter log entries by.
        ;"              Must be INTERNAL form of an entry in the EVENT TYPE
        ;"              field in file TMG CPRS PATIENT ACCESS LOG.
        ;"Result: none
        ;"Output: RESULT variable filled as follows:
        ;"             RESULT(0) = 1^OK, or -1^Error Message
        ;"             RESULT(#) = DFN^DUZ^TYPE^POINTER^DATETIME
        ;"             RESULT(#) = DFN^DUZ^TYPE^POINTER^DATETIME
        ;"             RESULT(#) = DFN^DUZ^TYPE^POINTER^DATETIME
        KILL RESULT
        SET RESULT(0)="1^OK"
        NEW RSLTCOUNT SET RSLTCOUNT=1
        SET DUZ=+$GET(DUZ)
        SET SDATE=$GET(SDATE)
        IF SDATE'>0 DO
        . NEW %DT,X,Y SET %DT="ST"
        . SET X=SDATE
        . DO ^%DT
        . IF Y>0 SET SDATE=Y
        IF SDATE'>0 DO  GOTO GLDN
        . SET RESULT(0)="-1^Valid start date-time not provided.  Got: "_SDATE
        SET EDATE=$GET(EDATE)
        IF EDATE="" DO
        . NEW %,%H,X
        . DO NOW^%DTC
        . SET EDATE=%
        IF EDATE'>0 DO  GOTO GLDN
        . SET RESULT(0)="-1^Valid end date-time not provided"
        SET TMGDFN=+$GET(TMGDFN)
        SET TYPE=$GET(TYPE)
        NEW DT SET DT=SDATE-0.000001 ;"backup 1 second to ensure none missed
        FOR  SET DT=$ORDER(^TMG(22715,"AD",DUZ,DT)) QUIT:(DT="")!(DT>EDATE)  DO
        . NEW IEN SET IEN=0
        . FOR  SET IEN=$ORDER(^TMG(22715,"AD",DUZ,DT,IEN)) QUIT:(IEN="")  DO
        . . NEW PAT SET PAT=$PIECE($GET(^TMG(22715,IEN,0)),"^",1)
        . . IF (TMGDFN>0),(PAT'=TMGDFN) QUIT  ;"wrong patient
        . . NEW SUBIEN SET SUBIEN=0
        . . FOR  SET SUBIEN=$ORDER(^TMG(22715,"AD",DUZ,DT,IEN,SUBIEN)) QUIT:(SUBIEN="")  DO
        . . . NEW ZN SET ZN=$GET(^TMG(22715,IEN,"LOG",SUBIEN,0))
        . . . IF (TYPE'=""),(TYPE'=$PIECE(ZN,"^",2)) QUIT  ;"wrong type
        . . . SET RESULT(RSLTCOUNT)=PAT_"^"_ZN
        . . . SET RSLTCOUNT=RSLTCOUNT+1
GLDN    QUIT

