TMGSELED ;TMG/kst/Group record selected editer ;03/25/06; 10/12/12, 2/2/14
         ;;1.0;TMG-LIB;**1**;01/25/07

 ;"TMG -- Group record selected editer
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
 ;"ASKSELED -- A record group selecter/editor, with asking user for options
 ;"ASK1ED -- A record editor
 ;"$$SELED(Options) -- entry point for group selecting and editing of records
 ;"        Options -- PASS BY REFERENCE.  Format:
 ;"              Options("FILE")=Filenumber^FileName
 ;"              Options("FIELDS","MAX NUM")=MaxDisplaySequenceNumber
 ;"              Options("FIELDS",DisplaySequence)=FldNum^FldName^Width
 ;"              Options("FIELDS",DisplaySequence)=FldNum^FldName^Width
 ;"              Options("FIELDS",DisplaySequence)=FldNum^FldName^Width
 ;"              Options("IEN LIST",IEN in FILE)=""
 ;"              Options("IEN LIST",IEN in FILE)=""
 ;"              Options("IEN LIST",IEN in FILE,"SEL")="" ;"<-- Optional. Makes preselected
 ;"              Note:  alternative Format
 ;"                Options("FIELDS",DisplaySequence)=FldNum:FldNum2:FldNum3^Width
 ;"                FldNum:FldNum2:FldNum3 means FldNum is ptr to file2, and
 ;"                      FldNum2 is in file2.  This value is a pointer to file3, and
 ;"                      FldNum3 is a value in file3
 ;"
 ;"$$EditRecs(pList,Options,LookupFn) -- get NEW values for fields in records
 ;"$$GetFields(Options) -- Interact with user to choose fields, and their display widths

 ;"=======================================================================
 ;" Private Functions.
 ;"=======================================================================
 ;"GetIENs(Options) -- Interact with user to choose IENs to be edited

 ;"GetFldVScreen(File,FieldNum,ScrnCode,pResults,Flags) -- get List of IENs in File matching ScreenCode
 ;"GetFldValue(File,FieldNum,Value,pResults) --get List of IENs in File with missing Field
 ;"FixValue(pList,FileNum,FieldNum) -- Ask user for a valid value & apply to all entries in pList



ASKSELED
        ;"Scope: PUBLIC
        ;"Purpose: A record group selecter/editor
        ;"Input: None
        ;"Output: Data in database may be edited.
        ;"Results: none

        WRITE !,"Group Select-and-Edit Routine",!
        WRITE "-------------------------------",!
        WRITE "Here are the steps we will go through . . .",!
        WRITE "Step #1. Pick FILE to browse",!
        WRITE "Step #2. Pick FIELDS to show when browsing",!
        WRITE "Step #3. Pick Records to browse from",!
        WRITE "Step #4. Select sepecific Records to edit",!
        WRITE "Step #5. Edit values in selected records",!
        WRITE "Loop back to Step #4",!

        NEW DIC,X,Y
        NEW FileNum,IEN
        NEW UseDefault SET UseDefault=1

        ;"Pick file to edit from
ASK1    SET DIC=1
        SET DIC(0)="AEQM"
        IF UseDefault DO   ;"leave the redundant DO loop, it protects $T, so second DO ^DIC isn't called
        . DO ^DICRW  ;" ^DICRW has default value of user's last response
        ELSE  DO ^DIC  ;"^DIC doesn't use a default value...
        WRITE !
        IF +Y'>0 WRITE ! GOTO ASKDone

        NEW Options
        SET Options("FILE")=Y
        IF $$GetFields(.Options)=0 GOTO ASKDone
        IF $$GetWidths(.Options)=0 GOTO ASKDone

ASK2    IF $$GetIENs(.Options)=0 GOTO ASKDone

        IF $$SELED(.Options)=2 GOTO ASK2

ASKDone
        QUIT


ASK1ED
        ;"Scope: PUBLIC
        ;"Purpose: A record editor
        ;"Input: None
        ;"Output: Data in database may be edited.
        ;"Results: none

        NEW DIC,X,Y
        NEW FileNum,IEN
        NEW UseDefault SET UseDefault=0

        ;"Pick file to edit from
AK1     KILL DIC
        SET DIC=1
        SET DIC(0)="AEQM"
        SET DIC("A")="Enter Name of File Containing Record to Edit: ^// "
        IF UseDefault DO   ;"leave the redundant DO loop, it protects $T, so second DO ^DIC isn't called
        . DO ^DICRW  ;" ^DICRW has default value of user's last response
        ELSE  DO ^DIC  ;"^DIC doesn't use a default value...
        WRITE !
        IF +Y'>0 WRITE ! GOTO AKDone

        NEW Options
        SET Options("FILE")=Y
        IF $$GetFields(.Options)=0 GOTO AKDone

AK2     KILL DIC
        SET DIC("A")="Enter Record in "_$PIECE(Y,"^",2)_" to Edit: ^// "
        SET DIC=+Y
        SET DIC(0)="AEQM"
        DO ^DIC
        IF Y=-1 GOTO AK1
        NEW list SET list(+Y)=""
        IF $$EditRecs("list",.Options)=1 GOTO AK2

AKDone
        QUIT


GetFields(Options)
        ;"Purpose: Interact with user to choose fields, and their display widths
        ;"Input: Options -- PASS BY REFERENCE, (used for input and as OUT PARAMETER)
        ;"                      Note: prior entries are NOT KILLED
        ;"              Options("FILE")=Filenumber^FileName
        ;"              Options("FILE")=Filenumber     <---- FileName will be filled in.
        ;"Output: Options is filled as follows:
        ;"              Options("FILE")=Filenumber^FileName   <-- left in from input
        ;"              Options("FIELDS","MAX NUM")=MaxDisplaySequenceNumber
        ;"              Options("FIELDS",DisplaySequence)=FldNum^FldName^Width
        ;"              Options("FIELDS",DisplaySequence)=FldNum^FldName^Width
        ;"              Options("FIELDS",DisplaySequence)=FldNum^FldName^Width
        ;"Results: 1=OK To continue, 0=abort

        NEW result SET result=1
        NEW DIC,X,Y
        NEW SeqNum SET SeqNum=1
        NEW Field

        NEW FNAME SET FNAME=$PIECE($GET(Options("FILE")),"^",2)
        NEW FileNum SET FileNum=+$GET(Options("FILE"))
        IF FileNum=0 SET result=0 GOTO GFDone
        IF FNAME="" do
        . SET FNAME=$$GETFNAME^TMGDBAP2(FileNum)
        . SET $PIECE(Options("FILE"),"^",2)=FNAME
        SET DIC="^DD("_FileNum_","
        SET DIC(0)="MEQ"
GFLoop
        WRITE "Enter "
        IF SeqNum=1 WRITE "first "
        ELSE  WRITE "next "
        WRITE "field to display/edit (^ to abort): "
        read Field:$GET(DTIME,3600)
        IF Field="^" SET result=0 GOTO GFDone
        IF Field="" GOTO GFDone
        IF Field[":" do
        . NEW i,CurFile,abort
        . NEW NewField SET NewField=""
        . NEW NewFldNames SET NewFldNames=""
        . SET CurFile=FileNum,abort=0
        . for i=1:1:$LENGTH(Field,":") DO  QUIT:(abort=1)
        . . NEW fld,DIC,X,Y
        . . SET fld=$PIECE(Field,":",i)
        . . SET DIC="^DD("_CurFile_","
        . . SET DIC(0)="MEQ"
        . . SET X=fld
        . . DO ^DIC
        . . IF Y=-1 SET abort=1 QUIT
        . . IF NewField'="" SET NewField=NewField_":"
        . . IF NewFldNames'="" SET NewFldNames=NewFldNames_":"
        . . SET NewField=NewField_+Y
        . . SET NewFldNames=NewFldNames_$PIECE(Y,"^",2)
        . . NEW FldInfo SET FldInfo=$PIECE($GET(^DD(CurFile,+Y,0)),"^",2)
        . . IF FldInfo["P" do
        . . . SET CurFile=+$PIECE(FldInfo,"P",2)
        . . . WRITE "->"
        . SET Field=NewField_"^"_NewFldNames
        . IF Field="^" SET Field=""
        . WRITE !
        ELSE  do
        . SET X=Field
        . DO ^DIC WRITE !
        . IF +Y>0 SET Field=Y
        . ;"NOTE: I need to ask for subfield IF PTR to another file.
        . ELSE  do
        . . ;"if Field'["?" WRITE "??",!
        . . SET Field=""
        IF Field="" GOTO GFLoop
        SET Options("FIELDS",SeqNum)=Field
        SET Options("FIELDS","MAX NUM")=SeqNum
        NEW % SET %=2
        WRITE "  DISPLAY only (i.e. don't allow edit)" DO YN^DICN WRITE !
        IF %=1 SET Options("FIELDS",SeqNum,"NO EDIT")=1
        IF %=-1 GOTO GFDone
        SET SeqNum=SeqNum+1
        GOTO GFLoop

GFDone
        WRITE !
        QUIT result


GetWidths(Options)
        ;"Purpose: Interact with user to choose adjust widths of displayed fields
        ;"Input: Options -- PASS BY REFERENCE, (used for input and as OUT PARAMETER)
        ;"                      Note: prior entries are NOT KILLED
        ;"              Options("FILE")=Filenumber^FileName
        ;"              Options("FIELDS","MAX NUM")=MaxDisplaySequenceNumber
        ;"              Options("FIELDS",DisplaySequence)=FldNum^FldName
        ;"              Options("FIELDS",DisplaySequence)=FldNum^FldName
        ;"              Options("FIELDS",DisplaySequence)=FldNum^FldName
        ;"Output: Options is filled as follows:
        ;"              Options("FILE")=Filenumber^FileName   <-- left in from input
        ;"              Options("FIELDS",DisplaySequence)=FldNum^FldName^Width
        ;"              Options("FIELDS",DisplaySequence)=FldNum^FldName^Width
        ;"              Options("FIELDS",DisplaySequence)=FldNum^FldName^Width
        ;"Results: 1=OK To continue, 0=abort

        ;"Note: Later I could rewrite this function to allow a more graphical
        ;"      resizing of the fields, by displaying the line with one field
        ;"      in reverse colors, indicating that it has been selected.  Then
        ;"      left-right would adjust size, and TAB would rotate to next field.

        NEW result SET result=1
        NEW LMargin SET LMargin=6
        NEW TMGMINW SET TMGMINW=3
        NEW FldCount SET FldCount=$GET(Options("FIELDS","MAX NUM"),0)
        IF FldCount=0 SET result=0 GOTO GWDone
        NEW ScrnWidth SET ScrnWidth=$GET(IOM,80)-LMargin-1  ;"leave room for selector numbers
        NEW tempW SET tempW=ScrnWidth\FldCount

        ;"Set default values
        NEW i for i=1:1:FldCount SET $PIECE(Options("FIELDS",i),"^",3)=tempW

        WRITE !,$$GetDispStr(.Options),!

        NEW %,i,Num,TMGW,Delta,MinW,TMGMAXW
        NEW SufferCol,SufferW
        NEW Menu,UsrSlct,MenuCount,MenuDflt
        SET MenuCount=1
        SET MenuDflt=1
        NEW DIR,FldName

        SET Menu(0)="Pick Option"
        for i=1:1:FldCount do
        . SET Menu(MenuCount)="Adjust ["_$PIECE(Options("FIELDS",i),"^",2)_"]"_$CHAR(9)_i
        . SET MenuCount=MenuCount+1
        SET Menu(MenuCount)="Enter ^ to abort"_$CHAR(9)_"^"

GWLoop
        SET %=2  ;"default to 'NO' the first time into loop.
        WRITE "Adjust column widths"
        DO YN^DICN WRITE !
        IF %=2 GOTO GWDone

        SET UsrSlct=$$MENU^TMGUSRI2(.Menu,MenuDflt,.MenuDflt)
        IF (UsrSlct="^")!(UsrSlct="") GOTO GWDone

        SET Num=+UsrSlct
        SET TMGW=$PIECE($GET(Options("FIELDS",Num)),"^",3)
        SET FldName=$PIECE($GET(Options("FIELDS",Num)),"^",2)

        ;"Determine which column will have compensatory changes as Column is changed
        SET SufferCol=FldCount
        IF Num<FldCount SET SufferCol=Num+1
        ELSE  IF Num>1 SET SufferCol=Num-1
        SET SufferW=$PIECE($GET(Options("FIELDS",SufferCol)),"^",3)

        SET TMGMAXW=ScrnWidth-((FldCount-1)*TMGMINW)  ;"min colum width is 3
        IF TMGMAXW<TMGMINW SET TMGMAXW=TMGMINW
        SET DIR(0)="N^"_(TMGMINW-TMGW)_":"_(SufferW-TMGMINW)_":0^K:(TMGW-X<TMGMINW)!(TMGW+X>TMGMAXW) X"
        SET DIR("A")="Enter amount to adjust "_FldName_" width by"
        SET DIR("B")=""

        WRITE $$GetDispStr(.Options)
        DO ^DIR WRITE !
        IF (Y="")!(Y["^") GOTO GWDone

        SET delta=+Y
        IF delta'=0 do
        . DO AdjCol(.Options,Num,delta)
        . DO AdjCol(.Options,SufferCol,-delta)

        ;"WRITE #
        WRITE $$GetDispStr(.Options),!

        GOTO GWLoop
GWDone
        QUIT result

AdjCol(Options,Num,Delta)
        ;"Purpose: To adust one column width
        ;"Input: Options -- PASS BY REFERENCE, (used for input and as OUT PARAMETER)
        ;"                      Note: prior entries are NOT KILLED
        ;"              Options("FIELDS",DisplaySequence)=FldNum^FldName
        ;"Output:Width for one column is changed.  No check for total width made
        ;"Results: none

        NEW W
        SET W=$PIECE($GET(Options("FIELDS",Num)),"^",3)
        SET W=W+Delta
        SET $PIECE(Options("FIELDS",Num),"^",3)=W
        QUIT


GetDispStr(Options)
        ;"Purpose: get a display representation of widths
        ;"Input: Options -- PASS BY REFERENCE
        ;"              Options("FIELDS","MAX NUM")=MaxDisplaySequenceNumber
        ;"              Options("FIELDS",DisplaySequence)=FldNum^FldName^Width
        ;"              Options("FIELDS",DisplaySequence)=FldNum^FldName^Width
        ;"              Options("FIELDS",DisplaySequence)=FldNum^FldName^Width
        ;"Results: returns a display string

        NEW outS SET $PIECE(outS," ",LMargin)=""
        ;"Display current widths
        for i=1:1:FldCount do
        . NEW W SET W=$PIECE(Options("FIELDS",i),"^",3)
        . NEW name SET name=$PIECE($GET(Options("FIELDS",i)),"^",2)
        . SET name=$EXTRACT(name,1,W-2)
        . SET name=$$LJ^XLFSTR(name,W-2,".") IF name="" SET name="!"
        . SET outS=outS_"["_name_"]"

        QUIT outS


GetIENs(Options)
        ;"Purpose: Interact with user to choose IENs to be edited
        ;"              User will be able to pick IENs from a SORT TEMPLATE, or
        ;"              a custom search.
        ;"Input: Options -- PASS BY REFERENCE, (used for input and as OUT PARAMETER)
        ;"                      Note: prior entries are NOT KILLED
        ;"              Options("FILE")=Filenumber^FileName
        ;"Output: Options is filled as follows:
        ;"              Options("FILE")=Filenumber^FileName   <-- left from input
        ;"              Options("IEN LIST",IEN in FILE)=""
        ;"              Options("IEN LIST",IEN in FILE)=""
        ;"Results: 1=OK To continue, 0=abort

        NEW Menu,UsrSlct
        NEW FileNum SET FileNum=$PIECE($GET(Options("FILE")),"^",1)
        NEW FileName SET FileName=$PIECE($GET(Options("FILE")),"^",2)
        NEW result SET result=1

        SET Menu(0)="Pick Records from "_FileName_" to Browse"
        SET Menu(1)="Choose a TEMPLATE from a former FILEMAN SEARCH"_$CHAR(9)_"TEMPLATE"
        SET Menu(2)="Browse ALL records"_$CHAR(9)_"ALL"
        SET Menu(3)="Browse records with a given Field VALUE"_$CHAR(9)_"SCREEN"
        SET Menu(4)="Enter ^ to abort"_$CHAR(9)_"^"
        ;"WRITE #
        SET UsrSlct=$$MENU^TMGUSRI2(.Menu,1)
        IF UsrSlct="^" SET result=0 GOTO GIDone
        IF UsrSlct=0 SET UsrSlct=""

        NEW abort SET abort=0
        IF UsrSlct="TEMPLATE" do
        . NEW DIC,Y
        . SET DIC=.401
        . SET DIC(0)="MAEQ"
TPLOOP  . WRITE "Select a TEMPLATE Containing Records for Browsing.",!
        . SET DIC("A")="Enter Template (^ to abort): "
        . DO ^DIC WRITE !
        . IF +Y'>0 SET abort=1 QUIT
        . NEW node SET node=$GET(^DIBT(+Y,0))
        . IF $PIECE(node,"^",4)'=FileNum DO  GOTO TPLOOP
        . . SET Y=0  ;"signal to try again
        . . NEW PriorErrorFound
        . . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error: That template doesn't contain records from "_FileName_". Please select another.")
        . . DO PRESS2GO^TMGUSRI2
        . IF (+Y>0)&($DATA(^DIBT(+Y,1))>1) do
        . . MERGE Options("IEN LIST")=^DIBT(+Y,1)

        ELSE  IF UsrSlct="ALL" do
        . DO GetFldValue(FileNum,.01,"ALL",$name(Options("IEN LIST")))

        ELSE  IF UsrSlct="SCREEN" do
        . NEW DIC,X,Y,DIR,FldNum,Value
        . SET DIC="^DD("_FileNum_","
        . SET DIC(0)="MAEQ"
        . SET DIC("A")="Enter FIELD to use for SCREEN: "
        . DO ^DIC WRITE !
        . IF Y=-1 QUIT
        . SET FldNum=+Y
        . SET DIR(0)=FileNum_","_FldNum
        . SET DIR("?",1)="Enter value to search for.  Records will be included"
        . SET DIR("?",2)="if the field chosed contains the value entered here."
        . SET DIR("?",3)="A @ may be entered to represent a NULL value for a field."
        . SET DIR("?",4)="For more complex searches, use Fileman search function,"
        . SET DIR("?",5)="store results in a template, and then chose that template"
        . SET DIR("?",6)="as the input source instead of choosing a screening value."
        . DO ^DIR WRITE !
        . IF X="@" SET Y="@"
        . IF Y="" QUIT
        . SET Value=$PIECE(Y,"^",1)
        . DO GetFldValue(FileNum,FldNum,Value,$name(Options("IEN LIST")))

        IF abort=1 SET result=0
GIDone
        QUIT result


GetFldVScreen(File,FieldNum,ScrnCode,pResults,Flags)
        ;"Purpose: get List of IENs in File with matching Field
        ;"Input: File -- the File to scan
        ;"       FieldNum -- the Field number to get from file
        ;"       ScrnCode -- Screening code to be executed....
        ;"          Format:  '$$MyFn^MyModule()', or
        ;"                   '(some test)'  such that the following is valid code:
        ;"                   SET @("flagToSkip="_ScrnCode)
        ;"              ---> If flagToSkip=1, then record is NOT selected
        ;"                   The following variables will be available for use:
        ;"                      File -- the File name or number
        ;"                      FieldNum -- the field number
        ;"                      IEN -- the IEN of the current record.
        ;"                      RecValue -- the current value of the field
        ;"       pResults -- PASS BY NAME, an OUT PARAMETER.
        ;"       Flags -- OPTIONAL.  Possible Flags
        ;"              "E" search for external forms (default is internal forms)
        ;"Output: @pResults is filled as following.  Note: prior results are not killed
        ;"              @pResults@(IEN)=""
        ;"              @pResults@(IEN)=""
        ;"Results: none

        NEW Itr,IEN,RecValue,FMFlag
        NEW abort SET abort=0
        SET FMFlag="I" IF $GET(Flags)["E" SET FMFlag=""

        SET RecValue=$$ItrFInit^TMGITR(File,.Itr,.IEN,FieldNum,,FMFlag)
        DO PrepProgress^TMGITR(.Itr,20,0,"IEN")
        FOR  DO  QUIT:(($$ItrFNext^TMGITR(.Itr,.IEN,.RecValue)="@@@@@@@@")!(+IEN=0))!abort
        . IF $$USRABORT^TMGUSRI2 SET abort=1 QUIT
        . NEW flagToSkip SET @("flagToSkip="_ScrnCode)
        . IF flagToSkip QUIT
        . SET @pResults@(IEN)=""
        DO ProgressDone^TMGITR(.Itr)

        QUIT


GetFldValue(File,FieldNum,Value,pResults,Flags)
        ;"Purpose: get List of IENs in File with matching Field
        ;"Input: File -- the File to scan
        ;"       FieldNum -- the Field number to get from file
        ;"       Value -- the value to compare against.  Poss Values
        ;"                VALUE:  IF field=VALUE, then record selected
        ;"                "@":    IF field=null (empty), then record selected
        ;"                "ALL":  all records are selected
        ;"       pResults -- PASS BY NAME, an OUT PARAMETER.
        ;"       Flags -- OPTIONAL.  Possible Flags
        ;"              "E" search for external forms (default is internal forms)
        ;"Output: @pResults is filled as following.  Note: prior results are not killed
        ;"              @pResults@(IEN)=""
        ;"              @pResults@(IEN)=""
        ;"Results: none


        NEW Itr,IEN,RecValue,FMFlag
        IF $GET(Value)="ALL" GOTO GFV3

GFV1    SET FMFlag="I" IF $GET(Flags)["E" SET FMFlag=""
        SET RecValue=$$ItrFInit^TMGITR(File,.Itr,.IEN,FieldNum,,FMFlag)
        DO PrepProgress^TMGITR(.Itr,20,0,"IEN")
        FOR  DO  QUIT:(($$ItrFNext^TMGITR(.Itr,.IEN,.RecValue)="@@@@@@@@")!(+IEN=0))
        . IF (RecValue=Value)!((Value="@")&(RecValue="")) do
        . . SET @pResults@(IEN)=""
        WRITE !
        GOTO GFVDone

GFV3    WRITE "Gathering ALL records...",!
        SET IEN=$$ItrInit^TMGITR(File,.Itr,.IEN)
        DO PrepProgress^TMGITR(.Itr,100,0,"IEN")
        FOR  DO  QUIT:($$ItrNext^TMGITR(.Itr,.IEN)="")
        . IF +IEN'=IEN QUIT
        . SET @pResults@(IEN)=""
        DO ProgressDone^TMGITR(.Itr)
GFVDone
        QUIT


SELED(Options)
        ;"Scope: PUBLIC
        ;"Purpose: the entry point for group selecting and editing of recrods
        ;"              Note: this can be used as an API entry point
        ;"Input: Options -- PASS BY REFERENCE
        ;"              Format:
        ;"              Options("FILE")=Filenumber^FileName
        ;"              Options("FIELDS","MAX NUM")=MaxDisplaySequenceNumber
        ;"              Options("FIELDS",DisplaySequence)=FldNum^FldName^Width
        ;"              Options("FIELDS",DisplaySequence)=FldNum^FldName^Width
        ;"              Options("FIELDS",DisplaySequence)=FldNum^FldName^Width
        ;"              Options("FIELDS",DisplaySequence,"LOOKUP FN") -- OPTIONAL
        ;"                      A function for looking up NEW values.
        ;"                      Must be in format like this:
        ;"                         Options("FIELDS",DisplaySequence,"LOOKUP FN")="$$MyFn^MyModule(File,FldNum)"
        ;"                      i.e. must be a function name.  Function may take passed
        ;"                      parameters 'File' and 'FldNum'
        ;"                      Default value="$$ValueLookup(File,FldNum)"
        ;"              Options("IEN LIST",IEN in FILE)=""
        ;"              Options("IEN LIST",IEN in FILE)=""
        ;"              Options("IEN LIST",IEN in FILE,"SEL")="" ;"<-- optional. Makes preselected
        ;"              Note:  alternative Format
        ;"                Options("FIELDS",DisplaySequence)=FldNum:FldNum2:FldNum3^Width
        ;"                FldNum:FldNum2:FldNum3 means FldNum is ptr to file2, and
        ;"                      FldNum2 is in file2.  This value is a pointer to file3, and
        ;"                      FldNum3 is a value in file3
        ;"Output: Data in database may be edited.
        ;"Results: 1=Normal exit, 2=Needs rescan and recall

        NEW result SET result=1
        NEW SelList,pList,pIENList
        SET pList=$name(SelList)
        SET pIENList=$name(Options("IEN LIST"))

        NEW Fields,Widths
        SET Fields="",Widths=""

        NEW File SET File=+$GET(Options("FILE"))
        IF File="" GOTO SEDone

        NEW i for i=1:1:$GET(Options("FIELDS","MAX NUM")) do
        . SET Fields=Fields_$PIECE($GET(Options("FIELDS",i)),"^",1)_";"
        . SET Widths=Widths_$PIECE($GET(Options("FIELDS",i)),"^",3)_";"

        NEW tempResult
        NEW pSaveArray ;"will store ref of stored display array --> faster
SLoop   KILL @pList

        ;"Later change this to allow custom order of sort fields.
        DO IENSELCT^TMGUSRI3(pIENList,pList,File,Fields,Widths,"Pick Records to Edit.  [ESC],[ESC] when done",Fields,.pSaveArray)
        NEW count SET count=$$LISTCT^TMGMISC2(pList)
        WRITE count," items selected.",!

        IF count>0 SET tempResult=$$EditRecs(pList,.Options)

        WRITE !,"Fix more"
        NEW % SET %=1
        IF count=0 SET %=2
        DO YN^DICN WRITE !
        IF %'=1 GOTO SEDone
        IF $DATA(@pList)=0 GOTO SLoop

        NEW needsRepack SET needsRepack=0
        WRITE "Removing fixed items from list.  Here are the old entries...",!
        IF $GET(pSaveArray)="" do
        . DO ListNot^TMGMISC(pIENList,pList)  ;"<-- probably a bug in this function
        ELSE  do
        . NEW Itr,IEN,DispLineNum
        . ;"DO ZWRITE^TMGZWR(pList)
        . SET IEN=$$ItrAInit^TMGITR(pList,.Itr)
        . IF IEN'="" FOR  DO  QUIT:($$ItrANext^TMGITR(.Itr,.IEN)="")
        . . SET DispLineNum=+$GET(@pList@(IEN))
        . . IF DispLineNum=0 QUIT
        . . NEW tempS
        . . SET tempS=$GET(@pSaveArray@(DispLineNum))
        . . SET tempS=$PIECE(tempS,$CHAR(9),2)
        . . WRITE "  --",tempS,!
        . . KILL @pSaveArray@(DispLineNum)
        . . SET needsRepack=1
        . WRITE !
        WRITE !
        ;"IMPORTANT NOTE:  It seems that that after deleting items in pSaveArray, the ordering
        ;"     gets out of sync, such that the display number is NOT the same as the index
        ;"     and the wrong references can be used!!!  Must renumber somehow...

        SET %=2
        WRITE "Rescan file (slow)"
        DO YN^DICN WRITE !
        IF %=1 SET result=2 GOTO SEDone
        IF %=-1 GOTO SEDone

        WRITE "Packing display list..."
        DO ListPack^TMGMISC(pSaveArray)
        WRITE !

        GOTO SLoop
SEDone
        QUIT result

EditRecs(pList,Options,LookupFn)
        ;"Purpose: To get NEW values for display fields in records
        ;"Input: pList -- PASS BY NAME.  A list of IENs to process
        ;"              @pList@(IEN)=IgnoredValue
        ;"              @pList@(IEN)=IgnoredValue
        ;"              @pList@(IEN)=IgnoredValue
        ;"       Options -- PASS BY REFERENCE.  Format:
        ;"              Options("FILE")=Filenumber^FileName
        ;"              Options("FIELDS","MAX NUM")=MaxDisplaySequenceNumber
        ;"              Options("FIELDS",DisplaySequence)=FldNum^FldName^Width <-- Width is ignored
        ;"              Options("FIELDS",DisplaySequence)=FldNum^FldName^Width <-- Width is ignored
        ;"              Options("FIELDS",DisplaySequence)=FldNum  <-- FldName OPTIONAL
        ;"              Options("FIELDS",DisplaySequence,"LOOKUP FN") -- OPTIONAL
        ;"                      A function for looking up NEW values.
        ;"                      Must be in format like this:
        ;"                         Options("FIELDS",DisplaySequence,"LOOKUP FN")="$$MyFn^MyModule(File,FldNum)"
        ;"                      i.e. must be a function name.  Function may take passed
        ;"                      parameters 'File' and 'FldNum'
        ;"                      Default value="$$ValueLookup(File,FldNum)"
        ;"              Options("FIELDS",DisplaySequence,"NO EDIT")=1 <-- indicates this field NOT to be edited.
        ;"              Note:  alternative Format
        ;"                Options("FIELDS",DisplaySequence)=FldNum:FldNum2:FldNum3^Width
        ;"                FldNum:FldNum2:FldNum3 means FldNum is ptr to file2, and
        ;"                      FldNum2 is in file2.  This value is a pointer to file3, and
        ;"                      FldNum3 is a value in file3
        ;"
        ;"Results: 1=OK to continue, 0 IF error

        NEW result SET result=0  ;"default to error
        NEW Menu,UsrSlct,MenuCount,FldCount,File
        NEW TMGFDA,TMGMSG
        SET FldCount=+$GET(Options("FIELDS","MAX NUM")) IF FldCount=0 GOTO GNVDone
        SET File=+$GET(Options("FILE")) IF File=0 GOTO GNVDone
        NEW LookupFn
        NEW DIR,FldNum,NewValue

GNVL1   KILL Menu
        SET Menu(0)="Pick Field to EDIT"
        SET MenuCount=1
        for i=1:1:FldCount do
        . NEW CommonValue,FieldNum,FieldName
        . IF $GET(Options("FIELDS",i,"NO EDIT"))=1 QUIT  ;"don't edit this field
        . SET FieldNum=$PIECE($GET(Options("FIELDS",i)),"^",1)
        . SET FieldName=$PIECE($GET(Options("FIELDS",i)),"^",2)
        . IF FieldName="" SET FieldName=$$GetFldName^TMGDBAPI(File,FieldNum)
        . SET CommonValue=$$GetCommonValue(File,FieldNum,pList)
        . SET Menu(MenuCount)=FieldName_": ["_CommonValue_"]"_$CHAR(9)_i
        . SET MenuCount=MenuCount+1
        ;"set Menu(MenuCount)="Enter ^ to abort"_$CHAR(9)_"^"

GNVL2
        SET UsrSlct=$$MENU^TMGUSRI2(.Menu)
        ;"if FldCount>1 do
        ;". SET UsrSlct=$$MENU^TMGUSRI2(.Menu)
        ;"else  SET UsrSlct=1 ;"If only 1 option, then auto-select
        IF (UsrSlct="^")!(UsrSlct="") GOTO GWDone

        SET LookupFn=$GET(Options("FIELDS",UsrSlct,"LOOKUP FN"),"$$ValueLookup(File,FldNum)")

        KILL DIR,NewValue
        SET FldNum=+$PIECE($GET(Options("FIELDS",UsrSlct)),"^",1)
        IF FldNum=0 GOTO GNVDone

        SET @("Y="_LookupFn)
        ;"WRITE !,"Enter NEW value for field below."
        ;"set DIR(0)=File_","_FldNum
        ;"do ^DIR WRITE !

        IF Y="" GOTO GNVL2
        IF Y="^" GOTO GNVDone
        SET NewValue=$PIECE(Y,"^",1)
        IF NewValue=+NewValue do
        . NEW array
        . DO GFLDINFO^TMGDBAP3(File,FldNum,"array")
        . IF $GET(array("SPECIFIER"))["S" QUIT  ;"check IF field is a SET, IF so, don't add ` mark
        . SET NewValue="`"_NewValue  ;"indicate that number is a pointer

        NEW Itr,IEN,Value,results
        SET result=1
        SET IEN=$$ItrAInit^TMGITR(pList,.Itr)
        IF IEN'="" FOR  DO  QUIT:($$ItrANext^TMGITR(.Itr,.IEN)="")
        . KILL TMGFDA,TMGMSG
        . SET TMGFDA(File,IEN_",",FldNum)=NewValue
        . DO FILE^DIE("EK","TMGFDA","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) do
        . . SET result=0
        . . NEW PriorErrorFound
        . . DO SHOWDIER^TMGDEBU2(.TMGMSG,.PriorErrorFound)

        GOTO GNVL1

GNVDone
        QUIT result


ValueLookup(File,FldNum)
        ;"Purpose: To interact with user and obtain a value for field in file
        ;"Input: File: A valid file number
        ;"       FldNum: A valid field number in File
        ;"Result: Returns value of user input.

        NEW DIR
        WRITE !,"Enter NEW value for field below."
        SET DIR(0)=File_","_FldNum
        DO ^DIR WRITE !
        IF X="@" SET Y=X
        QUIT Y


GetCommonValue(File,Field,pList,Flags)
        ;"Purpose: Return a value held by all records in pList, or "" IF mixed values
        ;"Input: File -- file number
        ;"       Field -- field number or 'num:num2:num3" etc
        ;"       Flags -- value to pass to GET1^DIQ during lookup
        ;"Output: returns a common value, or "" IF not common value

        NEW Itr,IEN,Value,abort,result
        SET abort=0,result=""

        NEW Itr,IEN,Value,abort
        SET abort=0
        SET IEN=$$ItrAInit^TMGITR(pList,.Itr)
        IF IEN'="" FOR  DO  QUIT:($$ItrANext^TMGITR(.Itr,.IEN)="")!(abort=1)
        . SET Value=$$GET1^DIQ(File,IEN_",",Field)
        . IF result="" SET result=Value
        . IF Value'=result SET result="<MIXED VALUES>",abort=1

        QUIT result



