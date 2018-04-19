TMGSEQL3 ;TMG/kst/Code to interface with SequelSystems PMS ;03/25/06, 2/2/14
         ;;1.0;TMG-LIB;**1**;09/01/05

 ;"TMG SEQUEL IMPORT UTILITY FUNCTIONS
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
 ;"RPTSSNCF  ;"i.e. Report SSN Conflict
 ;"RPTDOBER  ;"i.e. Report DOB Errors
 ;"CLEARALL
 ;"Schedule(Time,Routine,Descr) -- schedule a task at the given time, to run the specified routine


 ;"FIXERRORS --OLD
 ;"FixOneError(OneLine,OneErr,OneChLog) -- OLD
 ;"tempMakeAlerts

 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"ShowOneConflict(IEN,ErrMsg)
 ;"$$QuietClear(DUZ)


 ;"=======================================================================
 ;"DEPENDENCIES
 ;"TMGSEQL1
 ;"TMGSEQL2
 ;"TMGSTUTL
 ;"TMGGDFN
 ;"TMGDEBUG
 ;"=======================================================================
 ;"=======================================================================



tempMakeAlerts
        ;"Purpose: To creat alerts for all entries in file 22706
        ;"Input: none
        ;"Output: This will generate Alerts, sent to the current user (DUZ)
        ;"Result: none

        NEW i,OneLine

        SET i=$ORDER(^TMG(22706,0))
        IF +i>0 FOR  DO  QUIT:(+i'>0)
        . NEW ref,IEN
        . SET IEN=i
        . SET i=$ORDER(^TMG(22706,i))
        . SET ref="^TMG(22706,"_IEN_",2)"
        . SET OneLine=$$WPToStr^TMGSTUTL(ref)
        . SET Msg="Problem with upload of Sequel data"
        . DO MAKERALT^TMGSEQL2(IEN,DUZ,Msg)

        QUIT


FIXERRORS     ;"NOTE-- this is an OLD function, not being used
        ;"Purpose: To handles errors encountered during the ASKIMPORT function
        ;"Input: none
        ;"Output: the database is modified
        ;"Results: none

        NEW INDEX
        NEW ABORT SET ABORT=0
        NEW NewArray,newI
        NEW NewErrArray,NewChgLog
        NEW MaxCount
        NEW TMGRemSex
        SET TMGRemSex=$$GET1^DIQ(22711,"1,",6,"I") ;"6=PICK GENDER FROM NAME?

        NEW Ref SET Ref=$name(^TMP("TMG","SEQUELIMPORT","ERRORS"))
        SET MaxCount=$ORDER(@Ref@(""),-1)

        SET INDEX=$ORDER(@Ref@(""))
        IF INDEX'="" FOR  DO  QUIT:(+INDEX'>0)!(ABORT)
        . NEW result
        . NEW OneErrArray,OneChLog
        . MERGE OneErrArray=@Ref@(INDEX)
        . NEW OneLine SET OneLine=$GET(@Ref@(INDEX))
        . WRITE "(",INDEX,"/",MaxCount,") "
        . SET result=$$FixOneError(OneLine,.OneErrArray,.OneChLog)
        . IF result=-1 SET ABORT=1 QUIT
        . IF result>0 do
        . . ;"MERGE NewChgLog(INDEX)=OneChLog
        . . MERGE ^TMP("TMG","SEQUELIMPORT","CHANGES",$H,INDEX)=OneChLog
        . ELSE  do
        . . KILL @Ref@(INDEX)
        . . MERGE @Ref@(INDEX)=OneErrArray(1)
        . SET INDEX=$ORDER(@Ref@(INDEX))

        IF 1=0 do
        . SET newI=$GET(newI)+1
        . SET NewArray(newI)=OneLine
        . IF $$PROCESPT^TMGSEQL1(OneLine,.NewErrArray,.NewChgLog) do
        . . MERGE ^TMP("TMG","SEQUELIMPORT","CHANGES",$H)=NewChgLog
        . . IF $DATA(NewErrArray) do
        . . . MERGE ^TMP("TMG","SEQUELIMPORT","ERRORS")=NewErrArray
        . . . WRITE "Here is the info about adding that patient:",!
        . . . DO ZWRITE^TMGZWR("NewErrArray")
        . . . WRITE !!
        . . . KILL NewErrArray
        . . ;"WRITE "killing: ",Ref,"(",Idx,")",!
        . . KILL @Ref@(Idx)


        WRITE !,"Goodbye.",!
FEDone
        QUIT


FixOneError(OneLine,OneErr,OneChLog)  ;"NOTE-- this is an OLD function, not being used
        ;"Purpose: to Fix one filing error
        ;"Input: OneLine -- the original data line in CVS format.
        ;"       OneErr -- PASS BY REFERENCE
        ;"              coming in, it will pass the original error.
        ;"              passed back out, it will contain any NEW errors.
        ;"       OneChLog -- PASS BY REFERENCE
        ;"              This will contain messages about changes made.
        ;"       Note: uses var with global scipe: TMGRemSex
        ;"Result: 1 = error fixed
        ;"        0 = error NOT fixed
        ;"       -1 = aborted

        NEW ABORT SET ABORT=0
        NEW NewArray,newI
        NEW NewErrArray,NewChgLog
        NEW result SET result=0

        NEW Len SET Len=$LENGTH(OneLine)
        IF $EXTRACT(OneLine,Len)=$CHAR(13) SET OneLine=$EXTRACT(OneLine,1,Len-1)

        NEW Info MERGE Info=OneErr("INFO")
        NEW DIERR MERGE DIERR=OneErr("INFO","DIERR")
        IF OneLine="" GOTO FOEDone

        NEW LName,FNAME,DOB,SID
        SET LName=$PIECE(OneLine,",",3)
        SET FNAME=$PIECE(OneLine,",",4)
        SET DOB=$PIECE($PIECE(OneLine,",",17)," ",1)
        SET SID=$PIECE(OneLine,",",5)
        WRITE FNAME," ",LName," ("_DOB_"); #",SID,"): "
        NEW Prov SET Prov=$PIECE(OneLine,",",14)
        NEW skip SET skip=0
        NEW temp SET temp=""

        IF $$INVALPTN^TMGSEQL1(FNAME,LName) DO  GOTO FOEDone
        . WRITE !,"Skipping and deleting, because name is: ",FNAME," ",LName,!
        . SET result=0

        IF $$INVALPRV^TMGSEQL1(Prov) DO  GOTO FOEDone
        . WRITE !,"Skipping and deleting, because provider is: ",Prov,!
        . SET result=0

        IF ($GET(DIERR(1))=311)&($GET(Info(0))="PATIENT NOT IN DATABASE")&($GET(DIERR(1,"PARAM","FIELD"))=.02) do
        . SET temp=""
        . FOR  DO  QUIT:(temp'="")!(ABORT=1)
        . . SET skip=0
        . . IF TMGRemSex=1 SET temp=$$GETSEX^TMGSEQL2(FNAME)
        . . IF temp="" read "MALE/FEMALE? ?// ",temp:$GET(DTIME,3600)
        . . IF temp="" SET temp="?"
        . . SET temp=$$UP^XLFSTR(temp)
        . . IF temp="?" DO  QUIT
        . . . WRITE "Options:",!
        . . . WRITE "-----------------",!
        . . . WRITE "M   Name is MALE (and remember in future).",!
        . . . WRITE "F   Name is FEMALE (and remember in future).",!
        . . . WRITE "D   Show the data line from the other computer (Sequel)",!
        . . . ;"WRITE "S   Turn automatic selecting SEX based on first name: "
        . . . ;"WRITE $SELECT(TMGRemSex=1:"OFF",TMGRemSex=0:"ON"),!
        . . . WRITE "x   Skip this patient.",!
        . . . WRITE "Q   Query the database to see existing entries.",!
        . . . WRITE "^   Abort.",!
        . . . SET temp=""
        . . IF temp="Q" DO  QUIT
        . . . NEW DIC SET DIC=2
        . . . SET DIC(0)="AEQM"
        . . . DO ^DIC
        . . . SET temp=""
        . . IF temp="S" DO  QUIT
        . . . ;"set TMGRemSex='TMGRemSex
        . . IF temp="D" DO  QUIT
        . . . WRITE !,OneLine,!
        . . . SET temp=""
        . . IF ("MALE"[temp)&(temp'="FEMALE") DO  QUIT
        . . . WRITE "MALE",!
        . . . SET OneLine=OneLine_"^MALE"
        . . . IF TMGRemSex=1 do
        . . . . NEW temp
        . . . . SET temp=$$SETSEX^TMGSEQL2(FNAME,"MALE")
        . . ELSE  IF "FEMALE"[temp DO  QUIT
        . . . WRITE "FEMALE",!
        . . . SET OneLine=OneLine_"^FEMALE"
        . . . IF TMGRemSex=1 do
        . . . . NEW temp
        . . . . SET temp=$$SETSEX^TMGSEQL2(FNAME,"FEMALE")
        . . ELSE  IF temp="^" DO  QUIT
        . . . WRITE "aborting..",!
        . . . SET ABORT=1
        . . ELSE  DO  QUIT
        . . . WRITE "skip...",!
        . . . SET skip=1,temp="x"
        ELSE  do
        . WRITE "??",!
        . WRITE "Here is info array.  I don't know how to fix this:",!
        . DO ZWRITE^TMGZWR("Info")
        . SET temp="?"
        . FOR  DO  QUIT:(temp'="")!(ABORT=1)
        . . SET skip=0
        . . IF temp="?" DO  QUIT
        . . . WRITE "Options:",!
        . . . WRITE "-----------------",!
        . . . WRITE "D   Show the data line from the other computer (Sequel)",!
        . . . WRITE "E   Edit data line.",!
        . . . WRITE "x   Skip this patient.",!
        . . . WRITE "Q   Query the database to see existing entries.",!
        . . . WRITE "^   Abort.",!
        . . . SET temp=""
        . . ELSE  IF temp="Q" DO  QUIT
        . . . NEW DIC SET DIC=2
        . . . SET DIC(0)="AEQM"
        . . . DO ^DIC
        . . . SET temp=""
        . . ELSE  IF temp="S" do
        . . . SET TMGRemSex='TMGRemSex
        . . ELSE  IF temp="D" DO  QUIT
        . . . WRITE !,OneLine,!
        . . . SET temp=""
        . . ELSE  IF temp="E" do
        . . . NEW r,NewLine
        . . . SET r=$$EDT1LINE^TMGSEQL2(OneLine,NewLine)
        . . . IF r=1 SET OneLine=NewLine ;"NOTE: later I will save old line to keep from having to process each update cycle
        . . ELSE  IF temp="^" DO  QUIT
        . . . WRITE "aborting..",!
        . . . SET ABORT=1
        . . ELSE  DO  QUIT
        . . . WRITE "skip...",!
        . . . SET skip=1
        . . read !,"Enter Option: ?//",temp:$GET(DTIME,3600),!
        . . IF temp="" SET temp="?"
        . . SET temp=$$UP^XLFSTR(temp)

        IF skip=0 do
        . KILL OneErr
        . IF $$PROCESPT^TMGSEQL1(OneLine,.OneErr,.OneChLog) do
        . . IF $DATA(OneErr) do
        . . . WRITE "Here is the info about adding that patient:",!
        . . . DO ZWRITE^TMGZWR("OneErr")
        . . . WRITE !!
        . . ELSE  SET result=1

FOEDone
        IF ABORT SET result=-1
        QUIT result



RPTSSNCF  ;"i.e. Report SSN Conflict
        ;"Purpose: to output a report of all instances of conflicted SSNum's

        DO RptMsg("CONFLICTING SS-NUMBERS")
        QUIT


RPTDOBER  ;"i.e. Report DOB Errors
        ;"Purpose: to output a report of all instances of conflicted SSNum's

        DO RptMsg("DOB")
        QUIT


RptMsg(MatchMsg)  ;"i.e. Alerts with matching message
        ;"Purpose: to output a report of all instances of errors with matching message
        ;"input: MatchMsg -- A message of error to match for.
        ;"              e.g. CONFLICTING SS-NUMBERS

        SET %ZIS("A")="Enter output printer or device (^ to abort): "
        DO ^%ZIS
        IF POP DO  GOTO RpmDone
        . WRITE !,"Error selecting output printer or device. Aborting report.",!
        use IO

        NEW IEN,count
        SET count=0
        SET IEN=$ORDER(^TMG(22706,0))
        IF +IEN'=0 FOR  DO  QUIT:(+IEN'>0)
        . NEW Node0 SET Node0=$GET(^TMG(22706,IEN,0))
        . NEW SQLNum SET SQLNum=$PIECE(Node0,"^",1)
        . NEW Msg SET Msg=$PIECE(Node0,"^",2)
        . IF Msg[MatchMsg do
        . . DO ShowOneConflict(IEN,Msg)
        . . SET count=count+1
        . SET IEN=$ORDER(^TMG(22706,IEN))

        WRITE count," conflicts found."

        use IO(0)
        DO ^%ZISC

RpmDone
        WRITE !,"Goodbye.",!
        QUIT




ShowOneConflict(IEN,ErrMsg)
        ;"Purpose: to output one conflict
        ;"Input: IEN, the IEN from file 22706

        NEW OneLine,TMGWP,TMGMSG,PtInfo
        NEW sqSSNum,vSSNum

        NEW x SET x=$$GET1^DIQ(22706,IEN_",",2,"","TMGWP","TMGMSG")
        IF $DATA(TMGMSG("DIERR"))'=0 DO  GOTO SOCDone
        . NEW PriorErrorFound
        . DO SHOWDIER^TMGDEBU2(.TMGMSG,.PriorErrorFound)
        SET OneLine=$$WPToStr^TMGSTUTL("TMGWP","")
        IF $$ParseLine^TMGSEQL1B(OneLine,.PtInfo)=0 DO  GOTO SOCDone
        . WRITE "Error parsing Alert data into patient data.",!

        IF $GET(ErrMsg)="" GOTO SOCDone

        SET sqSSN=$PIECE(ErrMsg,"Sequel#=",2)
        SET sqSSN=$PIECE(sqSSN," ",1)
        SET vSSN=$PIECE(ErrMsg,"VistA#=",2)
        SET vSSN=$PIECE(vSSN," ",1)

        NEW vFullName
        DO  ;"get actual full name & DOB for VistA SSN
        . NEW vName,vDOB
        . NEW tempDFN SET tempDFN=$$SSNLKUP^TMGGDFN(vSSN)
        . NEW TMGMSG,TMGERR,IENS
        . SET IENS=+tempDFN_","
        . DO GETS^DIQ(2,IENS,".01;.03","E","TMGMSG","TMGERR")
        . IF $DATA(TMGERR("DIERR")) do
        . . NEW PriorErrorFound
        . . DO SHOWDIER^TMGDEBU2(.TMGMSG,.PriorErrorFound)
        . SET vName=$GET(TMGMSG(2,IENS,.01,"E"))
        . SET vDOB=$GET(TMGMSG(2,IENS,.03,"E"))
        . SET vFullName=vName_" ("_vDOB_")"

        WRITE sqSSN," is Sequel SSN for: ",$GET(PtInfo("FULL NAME2"))," phone: ",$GET(PtInfo("PHONE NUM")),!
        WRITE vSSN," is VistA SSN for:  ",$GET(vFullName),!
        WRITE !

SOCDone
        QUIT



CLEARALL
        ;"Purpose: Wrapper for QuietClear (which clears all entries in file 22706 and all alerts.)
        ;"Input: none, DUZ (in global scope) is used
        ;"Output: All entries in the file and all associated Alerts are deleted
        ;"Results: none

        NEW TMGLIST
        NEW i,count
        SET count=0

        WRITE !,"-==Error Deleater==-",!
        WRITE "This will delete all error alerts related to",!
        WRITE "importing demographics from Sequel system",!!

        SET count=$$QuietClear(DUZ)

        WRITE count," data import errors "
        IF count>0 WRITE "deleted.",!
        ELSE  WRITE "to delete.",!

        WRITE !,"Goodbye.",!

        QUIT


QuietClear(DUZ,ID)
        ;"Purpose: To clear all entries in file 22706 and all alerts.
        ;"Input: DUZ, the user to delete alerts for.
        ;"       ID -- Optional.  The Alert ID to look for.  Default='TMGSQLIMPORT'
        ;"Output: All entries in the file and all associated Alerts are deleted
        ;"Results: count of errors deleted.

        NEW TMGLIST
        NEW i,count
        SET count=0
        SET ID=$GET(ID,"TMGSQLIMPORT")

        DO USER^XQALERT("TMGLIST",DUZ)

        SET i=$ORDER(TMGLIST(""))
        IF i'="" FOR  DO  QUIT:(+i'>0)
        . NEW alertID,IEN,TMGDATA,line
        . SET line=$GET(TMGLIST(i))
        . SET i=$ORDER(TMGLIST(i))
        . SET alertID=$PIECE(line,"^",2)
        . IF $PIECE(alertID,";",1)'=ID QUIT
        . NEW XQAID
        . DO GETACT^XQALERT(alertID)  ;"loads XQADATA, XQAID
        . IF +XQADATA>0 do
        . . NEW TMGERR,result
        . . SET count=count+1
        . . ;"WRITE "Deleting from file 22706, IEN=",XQADATA,!
        . . SET result=$$DelIEN^TMGDBAPI(22706,XQADATA,.TMGERR)
        . . IF result=0 DO SHOWDIER^TMGDEBU2(.TMGERR)
        . . ELSE  do
        . . . ;"WRITE $PIECE(line,"^",1),!!
        . . . DO DELETE^XQALERT
        . ;"else  WRITE "?? XQADATA ??",!

        QUIT count



Schedule(Time,Routine,Descr)
        ;"Purpose: to schedule a task at the given time, to run the specified routine
        ;"Input:  Time: The time to run the task, in FileMan or $HOROLOG format
        ;"        Routine: the routine to run.   E.g. "TEST^TMGSEQL3"
        ;"        Descr: Task description (don't include package name)
        ;"Output: Will shedule the task with TaskMan
        ;"Result: returns the task number

        NEW result
        SET result=""

        ;"New all vars used by taskman scheduler, to ensure to use of unexpected values
        NEW ZTRTN,ZTDESC,ZTDTH,ZTIO,ZTUCI,ZTCPU
        NEW ZTPRI,ZTSAVE,ZTKIL,ZTSYNC

        SET ZTRTN=$GET(Routine)
        SET ZTDESC="TMG SEQUELIMPORTER "_$GET(Descr)
        SET ZTDTH=$GET(Time)
        SET ZTIO=""

        DO ^%ZTLOAD

        SET result=$GET(ZTSK)

SchDone
        QUIT result


SHOWTIME
        ;"Purpose: to show the last time that the import task was run
        ;"Input: none
        ;"Output: will WRITE to screen
        ;"Result: none

        NEW time

        WRITE !!,"SEQUEL BILLING SYSTEM DEMOGRAPHICS IMPORT",!
        WRITE "Last demographics import date/time was: "
        SET time=$$GET1^DIQ(22711,"1,","LAST IMPORT DATE","I")
        WRITE $$FMTE^XLFDT(time,"P"),!

        NEW task
        SET task=$$GET1^DIQ(22711,"1,","TASK FOR NEXT RUN","I")
        IF +task>0 do
        . WRITE "Next demographics import date/time is: "
        . SET time=$$GET1^DIQ(14.4,task_",","Scheduled Run Time ($H)")
        . WRITE $$HTE^XLFDT(time,"P"),!

        QUIT

