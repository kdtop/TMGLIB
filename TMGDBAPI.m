TMGDBAPI ;TMG/kst/Database API library ;8/10/11, 10/12/12, 2/2/14
         ;;1.0;TMG-LIB;**1**;07/12/05
 ;
 ;"TMG DATABASE API FUNCTIONS
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
 ;"$$GetNumField^TMGDBAPI(FILENumber,FIELDName)                  ;Convert Field Name to Field Number
 ;"$$GETFNUM(FILEName)  ;Convert File Name to File Number
 ;"$$GETFNAME^TMGDBAPI(FILENumber)                               ;Convert File Number to File Name
 ;"$$EXPFNAME^TMGDBAPI(FILENUM)                                  ;Convert file number info expanded name, PARENTNAME:SUBNAME:...
 ;"$$GetFldName^TMGDBAPI(FILE,FIELDNumber)                       ;Convert Field Number to Field Name
 ;"$$GetFldList^TMGDBAPI(FILE,pArray)                            ;Get list of all fields for a file.
 ;"FieldExists^TMGDBAPI(FILENumber,FIELD)
 ;"SetFieldInfo^TMGDBAPI(FILE,FIELD,Array)
 ;"GFLDINFO^TMGDBAP3(FILENumber,FIELD,VarOutP)
 ;"GetSubFileNumber^TMGDBAPI(FILENumber,FIELD)
 ;"$$IsSubFile^TMGDBAPI(FILE)
 ;"$$ISWPFLD(FILE,FLD)
 ;"GetSubFInfo^TMGDBAPI(SubFILENum,Array)
 ;"GetRecMatch^TMGDBAPI(Data,RecNumIEN)
 ;"CompRec^TMGDBAPI(FILENumber,dbRec,TestRec)
 ;"UploadData^TMGDBAPI(DaDIta,RecNumIEN)
 ;"ValueLookup^TMGDBAPI(Params)
 ;"FileUtility^TMGDBAPI(Params)
 ;"AddRec^TMGDBAPI(Data)
 ;"SetupFileNum^TMGDBAPI(Data)
 ;"RecFind^TMGDBAPI(Params)
 ;"FLDCOMP^TMGDBAPI(TestFIELD,dbFIELD,Type)
 ;"$$dbWrite^TMGDBAPI(FDA,Overwrite,TMGIDE,Flags,ErrArray)
 ;"$$DelIEN^TMGDBAPI(FILE,RecNumIEN,ErrArray)
 ;"$$WriteWP^TMGDBAPI(FILE,RecNumIEN,FIELD,Array)
 ;"$$ReadWP^TMGDBAPI(FILE,IENS,FIELD,Array)
 ;"$$ShowIfError^TMGDBAPI(TMGMsg,PriorErrorFund)
 ;"$$GetValidInput^TMGDBAPI(FILE,FIELD) -- Get a valid input for field in file, asking user
 ;"$$AskFIENS() -- pick a (sub)file number, then pick a record from that file.
 ;"$$AskIENS(FILENum) -- return IENS for FILE (or subfile) number
 ;"GetRef^TMGDBAPI(file,IENS,FIELD) -- to return the global reference for a given record
 ;"GetPtrsOUT^TMGDBAPI(FILENum,Info) -- get a list of pointers out from the file.
 ;"$$TrimFDA^TMGDBAPI(FDA,Quiet) -- Trim FDA of any data already present in the database
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"HackWrite(GlobalP,FILENumber,IENS,FIELDNum,Data)
 ;"HandleHacksArray(MsgArray)
 ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"TMGDEBUG
 ;"TMGUSRIF
 ;"TMGSTUTL
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;
        ;"FORMAT OF DATA ARRAY
 ;
        ;" cNull="(none)"
        ;" cRecNum="RECNUM"
        ;" cOutput="OUTVAR"
        ;" cGlobal="GLOBAL"
        ;" cEntries="Entries"
        ;" cFlags="FLAGS"
        ;" cParentIENS="ParentIENS"
 ;
        ;"The Data array will be filed with data. (An example)
        ;"                Data(0,"FILE")="1234.1" <-- "NEW PERSON" Note conversion
        ;"                Data(0,"FILE",cGlobal)="^DIC(200)"  <-- note, NOT "^DIC(200,"
        ;"                Data(0,cRecNum)=2  <-- only IF user-specified.
        ;"                Data(0,cEntries)=1
        ;"                Data(1,".01")="MyData1"
        ;"                Data(1,".01","MATCHVALUE")="MyData1"
        ;"                Data(1,".01",cFlags)=any flags given (only present IF user specified)
        ;"                Data(1,".02")="Bill"
        ;"                Data(1,".02","MATCHVALUE")="John"
        ;"                Data(1,".03")="MyData3"
        ;"                Data(1,".03",cFlags)=any flags given (only present IF user specified)
        ;"                Data(1,".04")="MyData4"
        ;"                Data(1,".06")="MyData5"  <-- note "NAME" was converted to ".06"
        ;"                Data(1,".07",0,cEntries)=2    <-- "ITEM" converted to ".07"
        ;"                Data(1,".07",0,cParentIENS)=",10033,"
        ;"                Data(1,".07",1,".01")="SubEntry1"
        ;"                Data(1,".07",1,".02")="SE1"
        ;"                Data(1,".07",1,".03")="'Some Info'"
        ;"                Data(1,".07",2,".01")="SubEntry2"
        ;"                Data(1,".07",2,".02")="SE2"
        ;"                Data(1,".07",2,".04",0,cEntries)=1    ;"TEXT converted to .04
        ;"                Data(1,".07",2,".04",0,cParentIENS)=",3,10033,"
        ;"                Data(1,".07",2,".04",1,".01")="JD"
        ;"                Data(1,".07",2,".04",1,".02")="DOE,JOHN"
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;
GetNumField(FILENumber,FIELDName) ;
        QUIT $$GTNUMFLD^TMGDBAP3(.FILENumber,.FIELDName)
        ;
GetFileFldNums(FILE,FIELD,FILENumber,FIELDNumber) ;"provide a generic shell to ensure that File and Field numbers are in place
        QUIT $$SETFFNUM^TMGDBAP3(.FILE,.FIELD,.FILENumber,.FIELDNumber)
        ;
FieldExists(FILENumber,FIELD) ;
        ;"PUBLIC FUNCTION
        ;"Purpose: To ensure that a field exists -- even IF hidden by security measures
        ;"Input: FILENumber: FILE to check
        ;"       FIELD: the field number (or name) to check
        ;"Result: 1 IF field exists, 0 IF doesn't, 2 IF exists but is hidden to user
        ;
        NEW RESULT,FIELDNumber
        IF +FIELD=0 SET FIELDNumber=$$GTNUMFLD^TMGDBAP3(FILENumber,FIELD)
        ELSE  SET FIELDNumber=FIELD
        ;
        SET RESULT=$$VFIELD^DILFD(FILENumber,FIELDNumber)
        IF RESULT=1 GOTO FExsDone
        ;
        ;"Try a low-level data dictionary eval to see IF really does exist, but is hidden
        IF $DATA(^DD(FILENumber,FIELDNumber,0))'=0 SET RESULT=2
        ;
FExsDone ;
        QUIT RESULT
        ;
GetSubFileNumber(FILENumber,FIELD) ;
        ;"PUBLIC FUNCTION
        ;"Purpose: If FIELDNumber is a 'multiple' field, then this function should return 'subfile'
        ;"                  number of the sub file.
        ;"Input:FILENumber-- the file number (or sub file number) that field exists in
        ;"        FIELD-- the field number (or name) in file to lookup
        ;"Result: Returns sub file number, or 0 IF not found or invalid
        ;
        NEW cOKToCont SET cOKToCont=1
        NEW cAbort SET cAbort=0
        IF $DATA(TMGDEBUG)#10=0 NEW TMGDEBUG SET TMGDEBUG=0
        ;
        NEW Info
        NEW RESULT SET RESULT=cAbort
        NEW Output
        ;
        ;"First, verify file (or subfile) exists
        IF $$VFILE^DILFD(FILENumber)=0 DO  GOTO GSFDone  ;"abort
        . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"File number '"_FILENumber_"' is not valid.")
        ;
        ;"Next, ensure FIELD exists in file
        IF $$FieldExists(FILENumber,FIELD)=0 DO  GOTO GSFDone ;"abort
        . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"FIELD number '"_FIELD_"' is not valid.")
        ;
        ;"Next, ensure field is a multiple and get field info.
        DO GFLDINFO^TMGDBAP3(FILENumber,FIELD,"Output")
        IF $DATA(Output("MULTIPLE-VALUED"))=0 DO  GOTO GSFDone ;"abort
        . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"FIELD '"_FIELD_"' in File '"_FILENumber_"' is not a subfile.")
        ;
        ;"Now actually get subfile number
        IF $DATA(Output("SPECIFIER"))=0 DO  GOTO GSFDone ;"abort
        . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Unable to find 'Specifier' (subfile number)")
        SET RESULT=+Output("SPECIFIER")
        ;
        ;"Now actually get subfile number
        ;"SET Info=$GET(^DD(FILENumber,FIELD,0),0)
        ;"IF Info=0 DO  GOTO GSFDone
        ;". DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Unable to get information from data dictionary.")
        ;"SET RESULT=+$PIECE(Info,"^",2)
        ;
GSFDone ;
        QUIT RESULT
        ;
ISWPFLD(TMGFILE,TMGFLD) ;
        ;"PUBLIC FUNCTION
        ;"Purpose: return IF field FLD is a WP field
        ;"Input: FILE -- file NUMBER
        ;"       FLD -- field NUMBER
        ;"Result: 1 IF WP field, 0 IF not
        NEW TMGRESULT SET TMGRESULT=0
        NEW TMGINFO SET TMGINFO=$PIECE($GET(^DD(TMGFILE,TMGFLD,0)),"^",2)
        IF +TMGINFO'=TMGINFO GOTO IWPDN
        NEW TMGSUBFILE SET TMGSUBFILE=+TMGINFO
        SET TMGINFO=$PIECE($GET(^DD(TMGSUBFILE,.01,0)),"^",2)
        SET TMGRESULT=(TMGINFO["W")
IWPDN   QUIT TMGRESULT
        ;
IsSubFile(FILE) ;
        QUIT $$ISSUBFIL^TMGDBAP3(FILE)
        ;
GetSubFInfo(SubFILENum,Array) ;
        ;"PUBLIC FUNCTION
        ;"Purpose: To take a subfile NUMBER, and return information about it.
        ;"Input: SubFILENum-- the sub file number
        ;"        Array -- PASS BY REFERENCE.  An array to receive results.
        ;"                      any preexisting data is deleted.
        ;"Output    Array is formated as follows:
        ;"                      Array("SUBFILE","NUMBER")=file number of this sub file.
        ;"                      Array("SUBFILE","NAME")=file name of this sub file.
        ;"                      Array("PARENT","NUMBER")=parent file number
        ;"                      Array("PARENT","NAME")=parent file name
        ;"                      Array("PARENT","GL")=global reference of parent, in open format<-- only valid IF parent isn't also a subfile
        ;"                      Array("FIELD IN PARENT","NUMBER")=field number of subfile in parent
        ;"                      Array("FIELD IN PARENT","NAME")=filed name of subfile in parent
        ;"                      Array("FIELD IN PARENT","LOC")=node and piece where subfile is stored
        ;"                      Array("FIELD IN PARENT","CODE")=code giving subfile's attributes.
        ;"Result: 1 IF found info, or 0 IF not found or invalid
        ;
        NEW RESULT SET RESULT=0
        IF '$GET(SubFILENum) GOTO GSPDone
        KILL Array
        SET Array("SUBFILE","NUMBER")=SubFILENum
        SET Array("SUBFILE","NAME")=$PIECE($GET(^DD(SubFILENum,0)),"^",1)
        NEW parent
        SET parent=+$GET(^DD(SubFILENum,0,"UP"))
        IF parent=0 GOTO GSPDone
        SET Array("PARENT","NUMBER")=parent
        SET Array("PARENT","NAME")=$ORDER(^DD(parent,0,"NM",""))
        SET Array("PARENT","GL")=$GET(^DIC(parent,0,"GL"))
        NEW i SET i=$ORDER(^DD(parent,""))
        FOR   DO  QUIT:(i="")!(RESULT=1)  ;"scan all fields for a match
        . QUIT:(i="")
        . NEW node,num
        . SET node=$GET(^DD(parent,i,0))
        . IF +$PIECE(node,"^",2)=SubFILENum DO  QUIT
        . . SET Array("FIELD IN PARENT","NUMBER")=i
        . . SET Array("FIELD IN PARENT","NAME")=$PIECE(node,"^",1)
        . . SET Array("FIELD IN PARENT","LOC")=$PIECE(node,"^",4)
        . . SET Array("FIELD IN PARENT","CODE")=$PIECE(node,"^",2)
        . . SET RESULT=1
        . SET i=$ORDER(^DD(parent,i))
        ;
GSPDone ;
        QUIT RESULT
        ;
GetFieldInfo(FILENumber,FIELD,VarOutP,InfoS) ;
        QUIT $$GFLDINFO^TMGDBAP3
        ;
HackWrite(GlobalP,FILENumber,IENS,FIELDNum,Data) ;
        ;"PUBLIC FUNCTION
        ;"Purpse: To force data into a field -- using low level 'hack' method
        ;"Input: GlobalP -- the NAME of the global to put this into, i.e. "^VA(200,"
        ;"       FILENumber- the file number
        ;"       IENS -- the standard API IENS
        ;"       FIELDNum the field to put this into
        ;"       Data -- the value to put in
        ;"Note:  This can be used to put a value of "@" into a field
        ;"Result: 1 IF ok to continue, 0=abort
        ;"!!!NOTICE:  This is a very low level means of accessing the database.
        ;"  The built in data verifiers, indexers etc etc will not be made aware of
        ;"  changes made to the database through this method. USE ONLY WITH CAUTION.
        ;
        NEW cOKToCont SET cOKToCont=1
        NEW cAbort SET cAbort=0
        IF $DATA(TMGDEBUG)#10=0 NEW TMGDEBUG SET TMGDEBUG=0
        ;
        NEW RESULT SET RESULT=cAbort
        ;
        IF '$DATA(GlobalP) GOTO HWDone
        IF '$DATA(FILENumber) GOTO HWDone
        IF '$DATA(IENS) GOTO HWDone
        IF '$DATA(FIELDNum) GOTO HWDone
        IF '$DATA(Data) GOTO HWDone
        ;
        NEW DDInfo
        NEW FIELDInfo
        NEW Index,Part
        NEW OldData
        NEW RecNum
        ;
        ;"Get info from data dictionary r.e. where actual fields are stored in files.
        SET DDInfo=$GET(^DD(FILENumber,FIELDNum,0))
        IF '$DATA(DDInfo) GOTO HWDone
        SET FIELDInfo=$PIECE(DDInfo,"^",4)
        IF '$DATA(FIELDInfo),(FIELDInfo="") GOTO HWDone
        SET Index=$PIECE(FIELDInfo,";",1)
        SET Part=$PIECE(FIELDInfo,";",2)
        ;
        ;"Convert global form of ^VA(200,  into ^VA(200)
        NEW Len
        SET Len=$LENGTH(GlobalP)
        IF $EXTRACT(GlobalP,Len)="," DO
        . SET $EXTRACT(GlobalP,Len)=")"
        ;
        SET RecNum=$PIECE(IENS,",",1)
        IF $PIECE(IENS,",",2)'="" DO  GOTO HWDone
        . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Hack writing to subfiles not supported")
        IF $DATA(@GlobalP@(RecNum,Index))=0 GOTO HWDone
        SET OldData=$PIECE(@GlobalP@(RecNum,Index),"^",Part)
        IF Data'=OldData DO
        . SET $PIECE(@GlobalP@(RecNum,Index),"^",Part)=Data
        . ;"Give Message
        . NEW Text
        . SET Text(0)="<!> Caution"
        . SET Text(1)="Yikes!"
        . SET Text(2)=" "
        . SET Text(3)="We just bypassed all safety measures, "
        . SET Text(4)="and wrote directly to the database."
        . SET Text(5)="Make sure you know what you are doing!!"
        . SET Text(6)=" "
        . SET Text(7)="File: "_FILENumber
        . SET Text(8)="FIELD: "_FIELDNum
        . SET Text(9)="Prior value: '"_OldData_"'"
        . SET Text(10)="New value: '"_Data_"'"
        . SET Text(11)=" "
        . SET Text(12)="(This was caused by using Flags='H' in"
        . SET Text(13)="the XML script.)"
        . DO PUARRAY^TMGUSRI2(5,45,.Text)
        ELSE  DO
        ;
HWDone ;
        QUIT
        ;
HandleHacksArray(MsgArray) ;
        ;"PUBLIC FUNCTION
        ;"Purpose: To cycle through an array of hackwrites and process each one.
        ;"Input: HacksArray.  Best IF passed by reference
        ;"        Expected format of array:
        ;"                MsgArray(cHack,0,cEntries)=Number of Entries
        ;"                MsgArray(cHack,n) = Global;FileNumber;IENS;FIELDNum;Data
        ;"                MsgArray(cHack,n,cFlags)=User specified Flags for field.
        ;"Output: database is changed
        ;"Result: 1 IF ok to continue, 0=abort
        ;"!!!NOTICE:  This is a very low level means of accessing the database.
        ;"  The built in data verifiers, indexers etc etc will not be made aware of
        ;"  changes made to the database through this method. USE ONLY WITH CAUTION.
        ;
        NEW cOKToCont SET cOKToCont=1
        NEW cAbort SET cAbort=0
        IF $DATA(TMGDEBUG)#10=0 NEW TMGDEBUG SET TMGDEBUG=0
        NEW cHack SET cHack="H"
        NEW cEntries SET cEntries="Entries"
        ;
        NEW RESULT SET RESULT=cOKToCont
        NEW index SET index=1
        NEW GlobalP,FILENum,IENS,FIELDNum,Data
        NEW s
        ;
        FOR index=1:1:$GET(MsgArray(cHack,0,cEntries)) DO  QUIT:(s="")!(RESULT=cAbort)
        . SET s=$GET(MsgArray(cHack,index)) IF s="" QUIT
        . SET GlobalP=$PIECE(s,";",1)
        . SET FILENum=$PIECE(s,";",2)
        . SET IENS=$PIECE(s,";",3)
        . SET FIELDNum=$PIECE(s,";",4)
        . SET Data=$PIECE(s,";",5)
        . SET RESULT=$$HackWrite(GlobalP,FILENum,IENS,FIELDNum,Data)
        ;
        QUIT RESULT
        ;
GetRecMatch(Data,RecNumIEN) ;
        ;"PUBLIC FUNCTION
        ;"Purpose: Take Data array from DoUpload, and search in database
        ;"         for a prior matching record
        ;"Input: Data - Data array will contain all the information that is to be uploaded
        ;"                FIELDs that should be specifically matched will have "MATCHTHIS" fields.
        ;"                A field may have a "MATCHTHIS" node meaning that the value
        ;"                  specified should be searched for.
        ;"                Or, rarely, one may want to specifically search for a different
        ;"                  search value.  This is stored in a "MATCHVALUE" node.  This
        ;"                  node is ignored IF "MATCHTHIS" node is present.
        ;"                The .01 field always is used for searching. If not present, then
        ;"                  a "MATCHTHIS" node is assumed.
        ;"                Example array:
        ;"                Data(0,"FILE")="1234.1" <-- "NEW PERSON" Note conversion
        ;"                Data(0,"ParentIENS")="1234," <-- optional
        ;"                Data(1,".01")="BILL"
        ;"                Data(1,".01","MATCHVALUE")="JOHN"   <-- optional search value
        ;"                Data(1,".01","MATCHTHIS")=1
        ;"                Data(1,".02")="Sue"
        ;"                Data(1,".03")="MyData3"
        ;"                Data(1,".03",cFlags)=any flags given (only present if user specified)
        ;"         RecNumIEN -- MUST PASS BY REFERENCE.  An OUT parameter to receive results
        ;"Output: Returns answer in RecNumIEN (record number in file) IF found, or 0 otherwise
        ;"Result: 1=OKToContinue, 0=Abort
        ;"Note:
        ;"  * Data in Multiple fields are NOT used for matching.
        ;"  * I am not going to support matching for subrecords (i.e. SubEntry stuff above)
        ;"  * If data passed is a subSET of a larger data group (i.e. when this function
        ;"    is called recursively to handle a subfile), then an entry will be placed
        ;"    in the Data(0,cParentIENS) that will specify the RecNumIEN of the parent record
        ;"    holding this subfile.
        ;
        NEW cOKToCont SET cOKToCont=1
        NEW cAbort SET cAbort=0
        IF $DATA(TMGDEBUG)#10=0 NEW TMGDEBUG SET TMGDEBUG=0
        NEW cParentIENS SET cParentIENS="ParentIENS"
        ;
        NEW FILENumber,FIELDNum
        SET RecNumIEN=0
        NEW IENS,FIELDs,Flags
        NEW MatchValue SET MatchValue=""
        NEW FIELDMatch SET FIELDMatch=""
        NEW ScreenCode
        NEW Matches,NumMatches
        NEW TMGMsg
        NEW RESULT SET RESULT=cOKToCont
        NEW index
        NEW SlimData   ;"Will hold just those fields that should be matched against
        NEW OneMatch
        ;
        SET IENS=$GET(Data(0,cParentIENS))
        IF IENS'="" IF $EXTRACT(IENS,1)'="," DO
        . SET IENS=","_IENS
        ;
        SET FIELDs="@"
        ;"Setup specifier to tell which fields to return info on
        NEW DONE SET DONE=0
        SET index=0
        FOR  SET index=$ORDER(Data(index)) QUIT:(index="")!DONE  DO
        . SET FIELDNum=""
        . FOR  SET FIELDNum=$ORDER(Data(index,FIELDNum)) QUIT:(+FIELDNum=0)  DO
        . . IF $GET(Data(index,FIELDNum,"MATCHTHIS"))=1 DO
        . . . SET FIELDMatch=$GET(Data(index,FIELDNum))
        . . ELSE  SET FIELDMatch=$GET(Data(index,FIELDNum,"MATCHVALUE"))
        . . IF FIELDNum=".01" DO
        . . . IF FIELDMatch="" SET FIELDMatch=$GET(Data(index,.01))
        . . . SET MatchValue=FIELDMatch
        . . IF FIELDMatch'="" DO
        . . . SET FIELDs=FIELDs_";"_FIELDNum
        . . . SET SlimData(FIELDNum)=FIELDMatch
        . . . SET FIELDMatch=""
        . SET DONE=1  ;"Force handling only 1 entry (i.e. #1), then QUIT after first cycle.

        SET FILENumber=$GET(Data(0,"FILE"))
        SET ScreenCode=""
        SET Flags=""
        ;
        ;"======================================================
        ;"Call FIND^DIC
        ;"======================================================
        ;"Params:
        ;"FILE,IENS,FIELDS,FLAGS,VALUE,NUMBER,INDEXES,SCREEN,IDENTIFIER,TARGET_ROOT,MSG_ROOTS
        DO FIND^DIC(FILENumber,$GET(IENS),FIELDs,Flags,MatchValue,"*",,ScreenCode,,"Matches","TMGMsg")
        ;"======================================================
        ;"======================================================
        ;
        IF $DATA(TMGMsg) DO
        . IF $DATA(TMGMsg("DIERR"))'=0 DO  QUIT
        . . DO SHOWDIER^TMGDEBU2(.TMGMsg,.PriorErrorFound)
        . . SET RESULT=cAbort
        IF RESULT=cAbort GOTO GRMQuit
        ;
        IF $DATA(Matches("DILIST"))=0 GOTO GRMQuit  ;"keep RecNumIEN default of 0
        SET NumMatches=$PIECE(Matches("DILIST",0),"^",1)
        IF NumMatches=0 GOTO GRMQuit  ;"keep RecNumIEN default of 0
        ;
        FOR index=1:1:NumMatches DO  QUIT:RecNumIEN'=0   ;"Note: FIRST match returned.
        . KILL OneMatch
        . MERGE OneMatch=Matches("DILIST","ID",index)
        . IF $$CompRec(FILENumber,.OneMatch,.SlimData) SET RecNumIEN=Matches("DILIST",2,index)
        ;
GRMQuit ;
        QUIT RESULT
        ;
CompRec(FILENumber,dbRec,TestRec) ;
        ;"PUBLIC FUNCTION
        ;"Purpose: To compare data from the database against a test match
        ;"Input: FILENumber: the file data is from
        ;"         dbRec, an array of data from the database in the following format
        ;"                dbRec(.01)="JOHNS,BILL"
        ;"                dbRec(.02)="MALE"
        ;"                dbRec(.03)="01/20/1957"
        ;"                dbRec(.07)="(123) 555-1212"
        ;"         TestRec, an array of data to test for match with, in same format
        ;"                as above.  Note: there may well be less entries in this array
        ;"                than in the dbRec
        ;"                TestRec(.01)="JOHNS,BILL"
        ;"                TestRec(.03)="01/20/1957"
        ;"Output: 1 IF all values in TestRec=dbRec. 0=conflict
        ;"        Note: values in dbRec that don't have a corresponding entry in TestRec
        ;"                are ignored.
        ;
        NEW cOKToCont SET cOKToCont=1
        NEW cAbort SET cAbort=0
        IF $DATA(TMGDEBUG)#10=0 NEW TMGDEBUG SET TMGDEBUG=0
        ;
        NEW RESULT SET RESULT=cOKToCont
        NEW index SET index=""
        NEW FIELDType,TMGFDA,TMGMsg
        NEW dbIDT,testIDT   ;" IDT = internal form of date/time
        ;
CRLoop ;
        SET index=$ORDER(TestRec(index))
        IF index="" GOTO CRDone
        IF $DATA(dbRec(index))=0 GOTO CRLoop
        KILL TMGFDA,TMGMsg
        DO FIELD^DID(FILENumber,index,,"TYPE","TMGFDA","TMGMsg")
        IF $GET(TMGFDA("TYPE"))="DATE/TIME" DO  GOTO CRDone:'RESULT
        . SET X=TestRec(index)
        . DO ^%DT   ;"convert date/time into internal format
        . SET testIDT=Y
        . SET X=dbRec(index)
        . DO ^%DT   ;"convert date/time into internal format
        . SET dbIDT=Y
        . IF testIDT'=dbIDT DO
        . . SET RESULT=cAbort
        ELSE  IF TestRec(index)'=dbRec(index) DO  GOTO CRDone   ;"Note: simple '=' compare
        . SET RESULT=cAbort
        GOTO CRLoop
CRDone  ;
        ;
        QUIT RESULT
        ;
UploadData(Data,RecNumIEN) ;
        ;"PUBLIC FUNCTION
        ;"Purpose: Do actual upload of Data, given in specific format
        ;"Note: This function may be called recursively by subfiles
        ;"Input: Data -- data in format show at TOP OF THIS FILE
        ;"            Note: If this function is being passed recursively, then the data
        ;"                passed is probably just a subpart that corresponds to the subfile
        ;"         RecNumIEN -- OPTIONAL pameter.  May be used to specify the
        ;"                record to force data into.  If passed by reference, then
        ;"                record number (IEN) where data was placed is passed back.
        ;"                Use of this parameter only makes sense when filing the highest
        ;"                level file.  (When filing subfiles recursively, then the parent
        ;"                record number is stored in (0,cParentIENS)=",10033," e.g.)
        ;"Output: Information will be put into global database, based on
        ;"          entries in Data.
        ;"          Record number (IEN) of record will be put into RecNumIEN (or 0 IF error)
        ;"Result: Returns success 1=OK to continue. 0=Abort
        ;
        NEW cOKToCont SET cOKToCont=1
        NEW cAbort SET cAbort=0
        IF $DATA(TMGDEBUG)#10=0 NEW TMGDEBUG SET TMGDEBUG=0
        NEW cEntries SET cEntries="Entries"
        ;
        NEW RESULT SET RESULT=cOKToCont
        NEW NumEntries
        NEW index
        ;
        SET RecNumIEN=$GET(RecNumIEN,0) ;"See IF user-specified IEN was given.
        ;
        IF RecNumIEN'=0 DO  GOTO UDDone:(RESULT=cAbort)
        . NEW Params,MyOutVar
        . SET Params("FILE")=$GET(Data(0,"FILE"))
        . SET Params(cRecNum)=RecNumIEN
        . SET Params(cFIELD)=".01"
        . SET Params(cOutput)="MyOutVar"
        . SET RESULT=$$ValueLookup(.Params)  ;"result=0 (cAbort) IF unsuccessful lookup
        . IF RESULT=cAbort DO
        . . IF $DATA(PriorErrorFound)=0 NEW PriorErrorFound
        . . NEW s SET s="Unable to overwrite data into record#"_RecNumIEN_" because that record does not already exist.\n"
        . . SET s=s_"Will try to put data into a NEW record, which may not be record#"_RecNumIEN
        . . DO SHOWERR^TMGDEBU2(.PriorErrorFound,s)
        . . SET RESULT=cOKToCont
        . . SET PriorErrorFound=0 ;"clear errors and continue program.
        . . SET RecNumIEN=0
        ;
        SET NumEntries=$GET(Data(0,cEntries))
        FOR index=1:1:NumEntries DO  QUIT:(RESULT=cAbort)
        . NEW tData      ;"Create a tData array that has only 1 entry in it.
        . MERGE tData(0)=Data(0)
        . SET tData(0,cEntries)=1
        . MERGE tData(1)=Data(index)
        . IF RecNumIEN=0 SET RESULT=$$GetRecMatch(.tData,.RecNumIEN)  ;"IF no prior record, returns 0
        . IF RESULT=cAbort QUIT  ;//kt added 1/6/05
        . ;
        . IF RecNumIEN=0 DO  QUIT:(RESULT=cAbort)
        . . NEW AddRecNum
        . . SET AddRecNum=$$AddRec(.tData)
        . . IF AddRecNum=0 DO  QUIT
        . . . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error adding a record.")
        . . . SET RESULT=cAbort
        . ELSE  DO  QUIT:(RESULT=cAbort)
        . . SET RESULT=$$OverwriteRec^TMGDBAP2(RecNumIEN,.tData)
        . . SET RecNumIEN=0 ;"We won't to file any more into that record num, force search next cycle.
        . . IF RESULT=cAbort DO  QUIT
        . . . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error modifying an existing record.")
        ;
UDDone  ;
        ;"IF (RESULT'=cAbort) SET RESULT=(RecNumIEN>0)
        QUIT RESULT
        ;
ValueLookup(Params)  ;
        ;"PUBLIC FUNCTION
        ;"Purpose: To look for a value of a given value in a given record in given file.
        ;"Input: Params -- an array loaded with expected parameters.  I.e.:
        ;"                Params("FILE")="NEW PERSON" in our example
        ;"                Params(cRecNum)="1" in example
        ;"                Params(cFIELD)=".01" in our example (could be Name of field)
        ;"                Params(cOutput)="MyVar"
        ;"Output: MyVar is loaded with data, i.e.:
        ;"                     MyVar("FILE")=200
        ;"                     MyVar(cGlobal)="^VA(200)"
        ;"                     MyVar(cGlobal,cOpen)="^VA(200,"
        ;"                   MyVar(cRecNum)=1
        ;"                     MyVar(cFIELD)=.01
        ;"                     MyVar(cValue)=xxx <-- the looked-up value
        ;"Returns: If should continue execution:  1=OK to continue.  0=unsuccessful lookup
        ;"Note: I am getting values by directly looking into database, rather than use
        ;"        the usual lookup commands. I am doing this so that there will be no
        ;"        'hidden' data, based on security etc.
        ;"        **I need to check, but this probably means that the data returned will be
        ;"        in INTERNAL FILEMAN FORMAT (i.e. time values are encoded etc.)
        ;
        NEW cOKToCont SET cOKToCont=1
        NEW cAbort SET cAbort=0
        IF $DATA(TMGDEBUG)#10=0 NEW TMGDEBUG SET TMGDEBUG=0
        NEW cFIELD SET cFIELD="FIELD"                                ;"FIELD"
        NEW cNull SET cNull="(none)"
        NEW cRecNum SET cRecNum="RECNUM"                        ;"RecNum
        NEW cOutput SET cOutput="OUTVAR"                        ;"OutVar"
        NEW cGlobal SET cGlobal="GLOBAL"
        NEW cValueLookup SET cValueLookup="LOOKUPFIELDVALUE"        ;"LookupFIELDValue"
        NEW cOpen SET cOpen="OPEN"
        ;
        NEW RESULT SET RESULT=cAbort
        ;
        NEW Data
        NEW DDInfo
        NEW FIELDInfo
        NEW Index,Part
        ;
        NEW FIELD SET FIELD=$GET(Params(cFIELD),cNull)
        NEW RecNum SET RecNum=$GET(Params(cRecNum),cNull)
        NEW OutVarP SET OutVarP=$GET(Params(cOutput),cNull)
        IF (RecNum=cNull),(OutVarP=cNull) GOTO DVLUDone
        KILL @OutVarP ;"--ensure old variables in output variable are removed.
        ;
        SET Data(0,"FILE")=$GET(Params("FILE"))
        SET RESULT=$$SetupFileNum(.Data)
        IF RESULT=cAbort GOTO DVLUDone
        NEW FILENum SET FILENum=$GET(Data(0,"FILE"),cNull)
        NEW GlobalP SET GlobalP=$GET(Data(0,"FILE",cGlobal),cNull)
        IF (FILENum=cNull),(GlobalP=cNull) GOTO DVLUDone
        NEW FIELDNum SET FIELDNum=$$GTNUMFLD^TMGDBAP3(FILENum,FIELD)
        IF FIELDNum=0 GOTO DVLUDone
        ;
        ;
        ;"Get info from data dictionary r.e. where actual fields are stored in files.
        SET DDInfo=$GET(^DD(FILENum,FIELDNum,0))
        IF $DATA(DDInfo)=0 GOTO HWDone
        SET FIELDInfo=$PIECE(DDInfo,"^",4)
        IF '$DATA(FIELDInfo),(FIELDInfo="") GOTO DVLUDone
        SET Index=$PIECE(FIELDInfo,";",1)
        SET Part=$PIECE(FIELDInfo,";",2)
        ;
        IF $DATA(@GlobalP@(RecNum,Index))=0 GOTO DVLUDone
        SET Data=$PIECE(@GlobalP@(RecNum,Index),"^",Part)
        ;
        KILL @OutVarP
        SET @OutVarP@("FILE")=FILENum
        SET @OutVarP@(cRecNum)=RecNum
        SET @OutVarP@(cFIELD)=FIELDNum
        SET @OutVarP@(cValue)=Data
        SET @OutVarP@(cGlobal)=GlobalP
        SET @OutVarP@(cGlobal,cOpen)=$GET(Data(0,"FILE",cGlobal,cOpen))
        ;
        SET RESULT=cOKToCont
        ;
DVLUDone ;
        QUIT RESULT
        ;
FileUtility(Params)  ;
        ;"PUBLIC FUNCTION
        ;"Purpose: To provide file access/manipulation utilities to script user
        ;"syntax:
        ;"   <FileUtility File="NEW PERSON" Fn="xxx" RecNum="1" FIELD=".01" OutVar"MyOutVar" Value="xx" >
        ;"Input: Params -- an array loaded with expected parameters.  I.e.:
        ;"                Params("FILE")="NEW PERSON" for example
        ;"                        File: The name of the file to act upon.
        ;"                        File may have subnodes (i.e. "NEW PERSON|ALIAS|TITLE")
        ;"                        **BUT**, any deletion or SET values will only work on top level (i.e. "NEW PERSON")
        ;"                Params(cFn)="info" or "delete", or "set"  [OPTIONAL]
        ;"                  Fn="delete"  If FIELD is not specified:
        ;"                                          Will cause record RecNum to be deleted.
        ;"                                          MyOutVar("DELETED")=RecNum of deleted record, or
        ;"                                        0 IF not found.
        ;"                                If FIELD IS specified:
        ;"                                        Will delete the value in field, in record RecNum
        ;"                                Note: delete is intended only for the highest-level records
        ;"                                        (i.e. not subfiels, or multiple fields)
        ;"                           Note: delete method uses ^DIK to delete the record
        ;"                  Fn="info"  Will just fill in info below.
        ;"                        If Fn not specified, this is default
        ;"                  Fn="set"  Will put Value into FIELD, in RecNum, in File (all required)
        ;"                Params(cRecNum)="1" for example
        ;"                        RecNum: [OPTIONAL] Specifies which record to act on.  If not
        ;"                                specified, then just file info is returned.
        ;"                Params(cFIELD)=".01" for example (could be Name of field)
        ;"                        FIELD: [OPTIONAL] Specifies which field to act on.
        ;"                Params(cOutput)="MyVar"
        ;"                        OutVar: Needed to get information back from function (but still Optional)
        ;"                        Gives name of variable to put info into.
        ;"Output: MyVar is loaded with data, i.e.
        ;"        i.e. MyOutVar("FILE")=Filenumber
        ;"             MyOutVar("FILE","FILE")=SubFilenumber <-- only IF subnodes input in File name (e.g."ALIAS")
        ;"             MyOutVar("FILE","FILE","FILE")=SubSubFilenumber <-- only IF subnodes input in File name (e.g."TITLE")
        ;"             MyOutVar("GLOBAL")="^VA(200)"
        ;"             MyOutVar("GLOBAL, OPEN")="^VA(200,"
        ;"             MyOutVar("RECNUM")=record number
        ;"             MyOutVar("FIELD")=Filenumber
        ;"             MyOutVar("VALUE")=xxxx <=== value of field (PRIOR TO deletion, IF deleted)
        ;"             MyOutVar("NEXTREC")=record number after RecNum, or "" IF none
        ;"             MyOutVar("PREVREC")=record number before RecNum, or "" IF none
        ;"             MyOutVar("FN")=the function executed
        ;"             MyOutVar("NUMRECS")=Number of records in file PRIOR to any deletions
        ;"             MyOutVar("FIRSTREC")=Rec number of first record in file
        ;"             MyOutVar("LASTREC")=Rec number of last record in file
        ;"Returns: If should continue execution:  1=OK to continue.  0=abort
        ;"Note: I am getting values by directly looking into database, rather than use
        ;"        the usual lookup commands. I am doing this so that there will be no
        ;"        'hidden' data, based on security etc.
        ;
        IF $DATA(TMGDEBUG)#10=0 NEW TMGDEBUG SET TMGDEBUG=0
        NEW cOKToCont SET cOKToCont=1
        NEW cAbort SET cAbort=0
        NEW cFIELD SET cFIELD="FIELD"                                ;"FIELD"
        NEW cNull SET cNull="(none)"
        NEW cRecNum SET cRecNum="RECNUM"                        ;"RecNum
        NEW cRecord SET cRecord="RECORD"                        ;"Record"
        NEW cOutput SET cOutput="OUTVAR"                        ;"OutVar"
        NEW cGlobal SET cGlobal="GLOBAL"
        NEW cValueLookup SET cValueLookup="LOOKUPFIELDVALUE"        ;"LookupFIELDValue"
        NEW cOpen SET cOpen="OPEN"
        NEW cInfo SET cInfo="INFO"                                ;"Info
        IF $DATA(cNodeDiv)#10=0 NEW cNodeDiv SET cNodeDiv="|"
        NEW cDelete SET cDelete="DELETE"                        ;"Delete
        NEW cNextRec SET cNextRec="NEXTREC"
        NEW cPrev SET cPrev="PREV"
        NEW cNumRecs SET cNumRecs="NUMRECS"
        NEW cFirstRec SET cFirstRec="FIRSTREC"
        NEW cLastRec SET cLastRec="LASTREC"
        ;
        NEW RESULT SET RESULT=cAbort
        ;
        NEW Data
        NEW DDInfo
        NEW FIELDInfo
        NEW Index,Part
        NEW DummyOut
        ;
        NEW OutVarP SET OutVarP=$GET(Params(cOutput),cNull)
        ;"IF (OutVarP=cNull) GOTO DFUTDone
        IF (OutVarP=cNull) DO
        . SET OutVarP="DummyOut"
        ;
        KILL @OutVarP ;"--ensure old variables in output variable are removed.
        ;
        NEW RecNum SET RecNum=$GET(Params(cRecNum))
        SET @OutVarP@(cRecNum)=RecNum
        ;
        NEW Fn SET Fn=$GET(Params(cFn),cInfo)
        SET Fn=$$UP^XLFSTR(Fn)
        ;
        NEW Value SET Value=$GET(Params(cValue))
        ;
        NEW FILEN SET FILEN=$GET(Params("FILE"))
        ;
        NEW SpliceArray
        IF FILEN[cNodeDiv DO    ;"Parse 'NEW PERSON|ALIAS|TITLE'  into 'NEW PERSON', 'ALIAS', 'TITLE'
        . DO CleaveToArray^TMGSTUTL(FILEN,cNodeDiv,.SpliceArray)
        . SET FILEN=$GET(SpliceArray(1))
        SET Data(0,"FILE")=FILEN
        SET RESULT=$$SetupFileNum(.Data) IF RESULT=cAbort GOTO DFUTDone
        NEW FILENum SET FILENum=$GET(Data(0,"FILE"),cNull)
        SET @OutVarP@("FILE")=FILENum
        ;
        NEW index SET index=2
        NEW GlobalP SET GlobalP=$name(@OutVarP@("FILE"))
        IF $DATA(SpliceArray(index)) DO
        . FOR index=index:1 DO  QUIT:index=""
        . . SET FILEN=SpliceArray(index)
        . . SET FILENum=$$GetSubFileNumber(FILENum,FILEN)
        . . IF +FILENum'=0 SET @GlobalP@("FILE")=FILENum
        . . SET GlobalP=$name(@GlobalP@("FILE"))
        . . SET index=$ORDER(SpliceArray(index))
        ;
        NEW GlobalP SET GlobalP=$GET(Data(0,"FILE",cGlobal),cNull)
        IF (FILENum=cNull),(GlobalP=cNull) GOTO DFUTDone
        SET @OutVarP@(cGlobal)=GlobalP
        SET @OutVarP@(cGlobal,cOpen)=$GET(Data(0,"FILE",cGlobal,cOpen))
        ;
        ;"If we've gotten this far, will consider the function a success
        SET RESULT=cOKToCont
        ;
        NEW FIELDN SET FIELDN=$GET(Params(cFIELD))
        NEW FIELDNum
        IF (+FIELDN=0)&(FIELDN'="") DO
        . SET FIELDNum=$$GTNUMFLD^TMGDBAP3(FILENum,FIELDN)
        ELSE  DO
        . IF FIELDN
        . SET FIELDNum=FIELDN
        SET @OutVarP@(cFIELD)=FIELDNum
        ;
        IF $DATA(@GlobalP@(0))=0 GOTO DFUTDone
        NEW NumRecs SET NumRecs=$PIECE(@GlobalP@(0),"^",4)
        NEW LastRec SET LastRec=$PIECE(@GlobalP@(0),"^",3)
        SET @OutVarP@(cNumRecs)=NumRecs
        SET @OutVarP@(cLastRec)=LastRec
        NEW RecI SET RecI=LastRec
        NEW PrevRec
        FOR  DO  QUIT:(RecI="")!(RecI=0)  ;"Scan backwards to find first record
        . SET PrevRec=$ORDER(@GlobalP@(RecI),-1)
        . IF (PrevRec="")!(PrevRec=0) DO
        . . SET @OutVarP@(cFirstRec)=RecI
        . SET RecI=PrevRec
        ;
        IF FIELDNum="" DO  GOTO DFUTDone
        . IF (Fn=cDelete)&(RecNum'="") DO
        . . SET DIK=$GET(Data(0,"FILE",cGlobal,cOpen))
        . . SET DA=RecNum
        . . DO ^DIK
        ;
        ;"Get info from data dictionary r.e. where actual fields are stored in files.
        SET DDInfo=$GET(^DD(FILENum,FIELDNum,0))
        IF '$DATA(DDInfo) GOTO HWDone
        SET FIELDInfo=$PIECE(DDInfo,"^",4)
        IF '$DATA(FIELDInfo),(FIELDInfo="") GOTO DFUTDone
        SET Index=$PIECE(FIELDInfo,";",1)
        SET Part=$PIECE(FIELDInfo,";",2)
        ;
        IF RecNum="" GOTO DFUTDone
        IF $DATA(@GlobalP@(RecNum,Index))=0 GOTO DFUTDone
        ;
        NEW Temp SET Temp=@GlobalP@(RecNum,Index)
        SET @OutVarP@(cValue)=$PIECE(Temp,"^",Part)
        KILL Temp
        SET @OutVarP@(cNextRec)=$ORDER(@GlobalP@(RecNum))
        SET @OutVarP@(cPrev)=$ORDER(@GlobalP@(RecNum),-1)
        ;
        IF Fn=cDelete DO
        .  SET $PIECE(@GlobalP@(RecNum,Index),"^",Part)=""
        ;
        IF Fn=cSet DO
        .  SET $PIECE(@GlobalP@(RecNum,Index),"^",Part)=Value
        ;
        SET RESULT=cOKToCont
        ;
DFUTDone ;
        QUIT RESULT
        ;
AddRec(Data)  ;
        ;"Purpose: Use info from data array to create a MINIMAL NEW record in database
        ;"                This record will have only it's .01 field, and any multiple
        ;"                subfiles will have only their .01 fields also.
        ;"Input: Data - Data array should be in format output from GetRInfo
        ;"Output: data base will be modified by adding record
        ;"Assumption: That a matching record does not already exist in database
        ;"Returns: RecNum of added record, or 0 IF error (0=abort)
        ;
 ;"NOTE!!! -- As I review this code, does it really return record number added???
        ;
        IF $DATA(TMGDEBUG)#10=0 NEW TMGDEBUG SET TMGDEBUG=0
        NEW cOKToCont SET cOKToCont=1
        NEW cAbort SET cAbort=0
        NEW cParentIENS SET cParentIENS="ParentIENS"
        ;
        NEW tmgFDA,TMGFDA  ;"Fileman Data Array
        NEW IENS ;"Internal Entry Number String
        NEW RecNum  ;"Internal number entry array
        NEW Flags
        NEW TMGMsg
        NEW FILENum
        NEW RESULT SET RESULT=cAbort
        NEW FDAIndex
        NEW MarkerArray
        NEW MsgArray
        ;
        SET IENS=$GET(Data(0,cParentIENS))
        ;
        NEW MarkNum SET MarkNum=0
        SET RESULT=$$SetupFDA^TMGDBAP2(.Data,.tmgFDA,IENS,"+",.MarkNum,.MsgArray)
        IF RESULT=cAbort GOTO SkRDone
        SET FILENum=$GET(Data(0,"FILE"),0)
        IF FILENum=0 SET RESULT=cAbort GOTO SkRDone
        ;
        ;
        SET FDAIndex=FILENum
        FOR  DO  QUIT:(FDAIndex="")!(RESULT=cAbort)
        . KILL TMGFDA
        . MERGE TMGFDA(FDAIndex)=tmgFDA(FDAIndex)
        . ;
        . SET Flags="E"  ;"E=External format values
        . ;
        . SET RESULT=$$ConvertFDA^TMGDBAP2(.TMGFDA,.MarkerArray)
        . IF TMGDEBUG DO ZWRITE^TMGZWR("TMGFDA") 
        . ;
        . ;"======================================================
        . ;"Call UPDATE^DIE
        . ;"======================================================
        . IF $DATA(TMGFDA)'=0 DO
        . . NEW $ETRAP SET $ETRAP="DO ErrTrp^TMGDBAPI"
        . . SET ^TMP("TMG",$J,"ErrorTrap")=RESULT
        . . SET ^TMP("TMG",$J,"Caller")="UPDATE^DIE"
        . . DO UPDATE^DIE(Flags,"TMGFDA","RecNum","TMGMsg")
        . . SET RESULT=^TMP("TMG",$J,"ErrorTrap")
        . . KILL ^TMP("TMG",$J,"ErrorTrap")
        . . KILL ^TMP("TMG",$J,"Caller")
        . ;"======================================================
        . ;"======================================================
        . ;
        . IF $DATA(RecNum) DO
        . . IF TMGDEBUG DO ZWRITE^TMGZWR("RecNum") 
        . . MERGE MarkerArray=RecNum
        . . IF RESULT=cAbort DO
        . . . NEW index
        . . . SET index=$ORDER(RecNum(""))
        . . . SET RESULT=$GET(RecNum(index))
        . ELSE  DO
        . ;
        . IF $DATA(TMGMsg("DIERR")) DO  QUIT
        . . DO SHOWDIER^TMGDEBU2(.TMGMsg,.PriorErrorFound)
        . . IF $DATA(RecNum(1)) DO
        . . . SET PriorErrorFound=0
        . . ELSE  DO
        . . . SET RESULT=cAbort
        . DO
        . . NEW tI SET tI=FDAIndex
        . . SET FDAIndex=$ORDER(tmgFDA(FDAIndex))
        . . KILL tmgFDA(tI)
        ;
        IF RESULT=cAbort DO  GOTO SkRDone
        ;
        SET RESULT=$$HandleHacksArray(.MsgArray)
        ;
        IF RESULT=cAbort GOTO SkRDone
        ;
SkRDone ;
        QUIT RESULT
        ;
        ;"=========================================================
        ;" Error trap routine
        ;"=========================================================
ErrTrp ;
        NEW cAbort SET cAbort=0
        SET $ETRAP="",$ECODE=""
        NEW Caller
        SET Caller=$GET(^TMP("TMG",$J,"Caller"),"?")
        DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error trapped. Caller was: ",Caller)
        IF $DATA(TMGMsg) DO SHOWDIER^TMGDEBU2(TMGMsg)
        SET ^TMP("TMG",$J,"ErrorTrap")=cAbort
        QUIT
        ;"=========================================================
        ;" End of Error trap routine
        ;"=========================================================
        ;
 ;"========================================================
 ;"The following routines were moved to shorten module length
        ;
GETFNAME(FILENumber)
        QUIT $$GETFNAME^TMGDBAP2(.FILENumber)
        ;
EXPFNAME(FILENUM)
        QUIT $$EXPFNAME^TMGDBAP2(.FILENUM)
        ;
GetFldName(FILE,FieldNumber)
        QUIT $$GetFldName^TMGDBAP2(.FILE,.FieldNumber)
        ;
GetFldList(FILE,pArray)
        QUIT $$GetFldList^TMGDBAP2(.FILE,.pArray)
        ;
SetupFileNum(Data)
        QUIT $$SetupFileNum^TMGDBAP2(.Data)
        ;
RecFind(Params)
        QUIT $$RecFind^TMGDBAP2(.Params)
        ;
FLDCOMP(TestField,dbField,Type)  ;"Field Compare
        QUIT $$FLDCOMP^TMGDBAP2(.TestField,.dbField,Type)
        ;
EnsureWrite(FILE,Field,IENS,Value,Flags,MsgArray)
        QUIT $$EnsureWrite^TMGDBAP2(.FILE,.Field,.IENS,.Value,.Flags,.MsgArray)
        ;
dbWrite(FDA,Overwrite,TMGIEN,Flags,ErrArray)
        QUIT $$dbWrite^TMGDBAP2(.FDA,.Overwrite,.TMGIEN,.Flags,.ErrArray)
        ;
DelIEN(FILE,RecNumIEN,ErrArray)
        QUIT $$DelIEN^TMGDBAP2(.FILE,.RecNumIEN,.ErrArray)
        ;
WriteWP(FILE,RecNumIEN,Field,TMGArray)
        QUIT $$WriteWP^TMGDBAP2(.FILE,.RecNumIEN,.Field,.TMGArray)
        ;
ReadWP(FILE,IENS,FIELD,Array)
        QUIT $$READWP^TMGDBAP3(.FILE,.IENS,.FIELD,.Array)
        ;
ShowIfError(TMGMsg,PriorErrorFound)
        QUIT $$ShowIfError^TMGDBAP2(.TMGMsg,.PriorErrorFound)
        ;
DataImport(Info,ProgressFN)
        QUIT $$DataImport^TMGDBAP2(.Info,.ProgressFN)
        ;
Set1(FILE,IEN,Field,Value,Flag)
        QUIT $$Set1^TMGDBAP2(.FILE,.IEN,.Field,.Value,.Flag)
        ;
GetValidInput(FILE,Field)
        QUIT $$GetValidInput^TMGDBAP2(.FILE,.Field)
        ;
AskFIENS()
        QUIT $$ASKFIENS^TMGDBAP3()
        ;
AskIENS(FILENum,IENS)
        QUIT $$ASKIENS^TMGDBAP3()
        ;
FIENS2Root(FIENS)  ;"depreciated
        QUIT $$FIENS2Root^TMGDBAP2(FIENS)
        ;
GetRef(file,IENS,field)
        QUIT $$GetRef^TMGDBAP2(file,IENS,field)
        ;
TrimFDA(FDA,Quiet)
        QUIT $$TrimFDA^TMGDBAP2(.FDA,.Quiet)
        ;
GetPtrsOUT(FILE,Info)
        QUIT $$GetPtrsOUT^TMGDBAP2(FILE,Info)
        ;
