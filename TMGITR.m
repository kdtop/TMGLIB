TMGITR ;TMG/kst/Array and Files Iterater code ;03/25/06, 2/2/14
         ;;1.0;TMG-LIB;**1**;08/12/06

 ;"TMG MISCELLANEOUS FUNCTIONS
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
 ;"firstIndex=$$ItrInit^TMGITR(File,.Iterater,[IENS],[direction],[PriorIndex]) -- SET up an iterater for a given fileman file
 ;"nextIndex=$$ItrNext^TMGITR(.Iterater,[.]CurIndex,[direction])

 ;"firstfieldValue=$$ItrFInit^TMGITR(File,.Iterater,.Index,[Field],[IENS],[Flags]) -- SET up an iterater for a given Fileman file, with FIELD return
 ;"nextFieldValue=$$ItrFNext^TMGITR(.Iterater,[.]CurIndex,.CurField,[direction]) -- return next $ORDER using iterater, returning FIELD

 ;"firstIndex=$$ItrAInit^TMGITR(pArray,.Iterater,[direction],[PriorIndex]) -- SET up an iterater for a given Array
 ;"nextIndex=$$ItrANext^TMGITR(.Iterater,[.]CurIndex,[direction]) -- return next $ORDER using iterater

 ;"PrepProgress^TMGITR(.Iterater,Interval,ByCt,pIndex)
 ;"ProgressDone^TMGITR(.Iterater)

 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"MakeRef(FileNum,IENS) -- make an global reference from a subfile

 ;"=======================================================================
 ;"DEPENDENCIES
 ;"      DIQ,DILF
 ;"=======================================================================
 ;"=======================================================================

 ;"Note: This code has not been tested/debugged with subfiles yet.


ItrInit(File,Iterater,IENS,Direction)
        ;"Purpose: To SET up an iterater for a given fileman file
        ;"Input: File -- name or number of a Fileman File
        ;"       Iterater -- PASS BY REFERENCE, an OUT PARAMETER.
        ;"              loaded with a reference that can be used with $ORDER
        ;"              e.g. Index=$ORDER(@Iterater@(Index))
        ;"              Iterater also stores other info as an array:
        ;"                Iterater("FILENUM")=FileNum
        ;"                Iterater("IENS"=IENS used to create iterater (if supplied)
        ;"                Iterater("COUNT")=number of records
        ;"       IENS -- OPTIONAL, IF File is a subfile, then must supply
        ;"              the IENS to specify its location, e.g.
        ;"              IEN,parent-IEN,grandparent-IEN,  etc.
        ;"              Function will add terminal ',' for user IF needed.
        ;"       Direction -- the Direction from "" to go for first record (-1 --> get last record)
        ;"Results: IEN of the first record in file, or "" IF error

        ;"Note: This is designed to work with Fileman files, with numeric
        ;"      nodes.  It is designed to NOT return alpha nodes (indices)

        KILL Iterater  ;"Clear any prior entries
        SET File=$GET(File)
        IF +File'=File SET File=$$GETFNUM^TMGDBAP3(File)
        NEW Index SET Index="" ;"default to error
        SET Iterater("FILENUM")=File
        SET Iterater("COUNT")=0
        SET Iterater("MAX")=0
        IF $GET(IENS)'="" do
        . IF $EXTRACT(IENS,$LENGTH(IENS))'="," SET IENS=IENS_","
        . SET Iterater("IENS")=IENS

        NEW ParentFile SET ParentFile=+$GET(^DD(File,0,"UP"))
        IF ParentFile=0 do
        . SET Iterater=$GET(^DIC(File,0,"GL"))
        . SET Iterater=$$CREF^DILF(Iterater)
        ELSE  SET Iterater=$$MakeRef(File,IENS)

        SET Direction=$GET(Direction,1)
        IF Iterater'="" do
        . SET Index=$ORDER(@Iterater@(0),Direction)
        . SET Iterater("COUNT")=$PIECE($GET(@Iterater@(0)),"^",4)
        . NEW index SET index=":"
        . FOR  SET index=$ORDER(@Iterater@(index),-1) QUIT:(+index>0)!(index="")
        . SET Iterater("MAX")=index

IIDone
        QUIT Index


ItrFInit(File,Iterater,Index,Field,IENS,Flags,Direction)
        ;"Purpose: To SET up an iterater for a given Fileman file, with FIELD return
        ;"Input: File -- name or number of a Fileman File
        ;"       Iterater -- PASS BY REFERENCE, an OUT PARAMETER.
        ;"              loaded with a reference that can be used with $ORDER
        ;"              e.g. Index=$ORDER(@Iterater@(Index))
        ;"              Iterater also stores other info as an array:
        ;"                Iterater("FILENUM")=FileNum
        ;"                Iterater("FIELD")=Field
        ;"                Iterater("FLAGS")=Flags
        ;"                Iterater("IENS"=IENS used to create iterater
        ;"       Index -- PASS BY REFERENCE, and OUT PARAMETER
        ;"              returns the first IEN in the file.
        ;"       Field -- optional.  Field Name or Number.  If supplied,
        ;"              value of field will be returned (rather than
        ;"       IENS -- optional, IF File is a subfile, then must supply
        ;"              the IENS to specify its location, e.g.
        ;"              NOTE: MUST end in ","
        ;"              IEN,parent-IEN,grandparent-IEN,  etc.
        ;"       Flags -- OPTIONAL -- Determines how value is returned.  Same Flags as used
        ;"              by GET1^DIQ.  "I"=Internal value returned (default is external form)
        ;"       Direction -- OPTIONAL -- the Direction from "" to go for first record (-1 --> get last record)
        ;"Results: Value of field for IEN of the first record in file, or "" IF error
        NEW result SET result=""
        SET IENS=$GET(IENS)
        SET Index=$$ItrInit(.File,.Iterater,.IENS,.Direction)
        SET Field=$GET(Field)
        IF +Field'=Field SET Field=$$GTNUMFLD^TMGDBAP3(.File,Field)
        SET Iterater("FIELD")=Field
        SET Iterater("FLAGS")=$GET(Flags)
        SET IENS=Index_","_IENS
        IF Index'="" SET result=$$GET1^DIQ(File,.IENS,.Field,.Flags)

        QUIT result

ItrAInit(pArray,Iterater,Direction,PriorIndex)
        ;"Purpose: To SET up an iterater for a given Array
        ;"Input: Array -- PASS BY NAME, the Array to be iterated.
        ;"       Iterater -- PASS BY REFERENCE, an OUT PARAMETER.
        ;"              loaded with a reference that can be used with $ORDER
        ;"              e.g. Index=$ORDER(@Iterater@(Index))
        ;"              Iterater also stores other info as an array:
        ;"                Iterater("COUNT")=number of top level nodes in the Array
        ;"       Direction -- OPTIONAL -- the Direction from "" (or PriorIndex) to go for first record (-1 --> get last record)
        ;"       PriorIndex -- OPTIONAL -- the prior index to start from.  Default=""
        ;"Results: first node in the Array, or "" IF error

        KILL Iterater ;"Clear any prior entries
        SET Iterater=pArray
        NEW Index SET Index="" ;"default to error
        IF $GET(pArray)="" GOTO IAIDone
        SET Direction=$GET(Direction,1)
        SET PriorIndex=$GET(PriorIndex,"")
        ;"Will count later, IF needed (avoid delay otherwise)
        ;"set Iterater("COUNT")=$$LISTCT^TMGMISC2(pArray)
        SET Iterater("COUNT")=0  ;"override later
        SET Iterater("MAX")=$ORDER(@Iterater@(":"),-1)
        SET Index=$ORDER(@Iterater@(PriorIndex),Direction)

IAIDone
        QUIT Index


MakeRef(FileNum,IENS)
        ;"Purpose: to make an global reference from a subfile
        ;"Input: FileNum -- must be filenumber
        ;"       IENS -- a standard Fileman IENS of subfile.  DON'T pass by reference
        ;"                      Array("SUBFILE","NUMBER")=file number of this sub file.
        ;"                      Array("SUBFILE","NAME")=file name of this sub file.
        ;"                      Array("PARENT","NUMBER")=parent file number
        ;"                      Array("PARENT","NAME")=parent file name
        ;"                      Array("PARENT","GL")=global reference of parent, in open format<-- only valid IF parent isn't also a subfile
        ;"                      Array("FIELD IN PARENT","NUMBER")=field number of subfile in parent
        ;"                      Array("FIELD IN PARENT","NAME")=filed name of subfile in parent
        ;"                      Array("FIELD IN PARENT","LOC")=node and piece where subfile is stored
        ;"                      Array("FIELD IN PARENT","CODE")=code giving subfile's attributes.
        ;"Result: returns reference

        NEW i
        NEW temp,IEN,parentFile
        NEW ref SET ref=""
        NEW Info

        for i=1:1 DO  QUIT:(FileNum=0)
        . ;"new NumIENs SET NumIENs=$LENGTH(IENS,",")
        . ;"set IEN=$PIECE(IENS,",",NumIENs)
        . ;"set IENS=$PIECE(IENS,",",1,NumIENs-1)
        . SET IEN=$PIECE(IENS,",",1)
        . SET IENS=$PIECE(IENS,",",2,999)
        . IF IEN'="" SET temp(i+1,"IEN")=IEN
        . IF $$GetSubFInfo^TMGDBAPI(FileNum,.Info)=0 SET FileNum=0 QUIT
        . SET FileNum=$GET(Info("PARENT","NUMBER"))
        . SET temp(i,"LOC IN PARENT")=$GET(Info("FIELD IN PARENT","LOC"))
        . SET temp(i+1,"REF")=$$CREF^DILF($GET(Info("PARENT","GL")))

        SET i=$ORDER(temp(""),-1)
        IF i'="" FOR  DO  QUIT:(i="")
        . IF $GET(temp(i,"REF"))'="" SET ref=temp(i,"REF")
        . NEW IEN SET IEN=$GET(temp(i,"IEN"))
        . NEW LOC SET LOC=$PIECE($GET(temp(i,"LOC IN PARENT")),";",1)
        . IF LOC'="" SET ref=$name(@ref@(LOC))
        . IF IEN'="" SET ref=$name(@ref@(IEN))
        . SET i=$ORDER(temp(i),-1)

        QUIT ref



ItrFNext(Iterater,CurIndex,CurField,direction)
        ;"Purpose: to return next $ORDER using iterater, returning FIELD
        ;"Input: Iterater -- PASS BY REFERENCE.  an iterater reference, as created by ItrInit
        ;"              Iterater also stores other info as an array:
        ;"                Iterater("FILENUM")=FileNum
        ;"                Iterater("FIELD")=Field
        ;"                Iterater("FLAGS")=Flags
        ;"                Iterater("IENS"=IENS used to create iterater
        ;"                Iterater("PROGRESS FN")=a PROGRESS FUNCTION <-- OPTIONAL
        ;"       CurIndex -- The current value of the index
        ;"                      IF PASSED BY REF, WILL BE CHANGED
        ;"       CurField -- OPTIONAL, PASS BY REFERENCE, an OUT PARAMETER -- not used to find next.
        ;"       direction -- OPTIONAL, 1 (default) for forward, -1 for backwards
        ;"Results: returns the next value by $ORDER, or "" IF none
        ;"NOTE: won't currently work for subfiles--would require passing a IENS

        SET CurIndex=$$ItrNext(.Iterater,.CurIndex,.direction)
        NEW File,Field,Flags
        SET CurField=""
        IF CurIndex'="" do
        . SET File=$GET(Iterater("FILENUM"))
        . SET Field=$GET(Iterater("FIELD"))
        . SET Flags=$GET(Iterater("FLAGS"))
        . SET CurField=$$GET1^DIQ(File,CurIndex,Field,Flags)

        QUIT CurField


ItrNext(Iterater,CurIndex,direction)
        ;"Purpose: to return next $ORDER using iterater
        ;"Input: Iterater -- and iterater reference, as created by ItrInit
        ;"                Iterater("PROGRESS FN")=a PROGRESS FUNCTION <-- OPTIONAL
        ;"       CurIndex -- The current value of the index
        ;"                      IF PASSED BY REF, WILL BE CHANGED
        ;"       direction -- OPTIONAL, 1 (default) for forward, -1 for backwards
        ;"Results: returns the next value by $ORDER, or "" IF none

        SET CurIndex=$ORDER(@Iterater@(CurIndex),$GET(direction,1))

        NEW ProgressFn SET ProgressFn=$GET(Iterater("PROGRESS FN"))
        IF ProgressFn'="" do
        . NEW $ETRAP SET $ETRAP="w ""??Progress function -- error trapped??"",!"
        . IF CurIndex="" DO ProgressDone(.Iterater)
        . ELSE  do
        . . SET Iterater("PROGRESS FN","CURRENT")=Iterater("PROGRESS FN","CURRENT")+1
        . . xecute ProgressFn

        QUIT CurIndex


ItrANext(Iterater,CurIndex,direction)
        ;"Purpose: to return next $ORDER using iterater
        ;"Input: Iterater -- and iterater reference, as created by ItrAInit
        ;"                Iterater("PROGRESS FN")=a PROGRESS FUNCTION <-- OPTIONAL
        ;"       CurIndex -- The current value of the index
        ;"                      IF PASSED BY REF, WILL BE CHANGED
        ;"       direction -- OPTIONAL, 1 (default) for forward, -1 for backwards
        ;"Results: returns the next value by $ORDER, or "" IF none

        QUIT $$ItrNext(.Iterater,.CurIndex,.direction)


PrepProgress(Iterater,Interval,ByCt,pIndex)
        ;"Purpose: to SET up code so that ItrNext can easily show a progress function
        ;"Input: Iterater -- PASS BY REFERENCE.  Array as SET up by ItrInit
        ;"       Interval -- OPTIONAL, default=10  The interval between showing progress bar
        ;"       ByCt -- OPTIONAL, default=1,
        ;"              IF 0: range is 0..MaxIEN,  index=IEN
        ;"              IF 1: range is 0..Number of Records, index=record counter
        ;"       pIndex -- IF ByCt=0, REQUIRED.  NAME OF 'IEN' variable

        NEW pCurrent,pTotal,pStartTime,PrgFn
        SET Interval=$GET(Interval,10)
        IF Interval=1 SET Interval=2  ;" X#1 is always 0, so would never show.
        SET ByCt=$GET(ByCt,1)
        SET Iterater("PROGRESS FN","BY-CT")=ByCt
        SET Iterater("PROGRESS FN","CURRENT")=0
        SET Iterater("PROGRESS FN","START TIME")=$H
        SET pStartTime=$name(Iterater("PROGRESS FN","START TIME"))
        IF ByCt=0 do
        . SET Iterater("PROGRESS FN","INDEX")=pIndex
        . NEW pMax SET pMax=$name(Iterater("MAX"))
        . SET PrgFn="if "_pIndex_"#"_Interval_"=1 "
        . SET PrgFn=PrgFn_"do PROGBAR^TMGUSRI2("_pIndex_",""Progress"",0,"_pMax_",,"_pStartTime_")"
        ELSE  do
        . SET pCurrent=$name(Iterater("PROGRESS FN","CURRENT"))
        . IF +$GET(Iterater("COUNT"))=0 do
        . . SET Iterater("COUNT")=$$LISTCT^TMGMISC2(Iterater)
        . SET pTotal=$name(Iterater("COUNT"))
        . SET PrgFn="if "_pCurrent_"#"_Interval_"=1 "
        . SET PrgFn=PrgFn_"do PROGBAR^TMGUSRI2("_pCurrent_",""Progress"",0,"_pTotal_",,"_pStartTime_")"

        SET Iterater("PROGRESS DONE FN")="do PROGBAR^TMGUSRI2(100,""Progress"",0,100)"
        SET Iterater("PROGRESS FN")=PrgFn

        QUIT


ProgressDone(Iterater)
        ;"Purpose: to allow user to call and ensure the progress bar is at 100% after
        ;"         loop is done.  This is needed because the Iterater code has no way of
        ;"         knowing what criteria will be used to determine when loop is complete.

        ;"new ProgressFn SET ProgressFn=$GET(Iterater("PROGRESS FN"))
        NEW ProgressFn SET ProgressFn=$GET(Iterater("PROGRESS DONE FN"))
        IF $GET(ProgressFn)'="" do
        . ;"new $ETRAP SET $ETRAP="w ""??Progress function -- error trapped??"",!"
        . ;"new ByCt SET ByCt=$GET(Iterater("PROGRESS FN","BY-CT"),1)
        . ;"if ByCt=0 do
        . ;". NEW pIndex SET pIndex=$GET(Iterater("PROGRESS FN","INDEX"))
        . ;". NEW max SET max=1
        . ;". IF pIndex'="" do
        . ;". . SET Iterater("MAX")=+$GET(@pIndex)
        . ;". . IF Iterater("MAX")'>0 SET Iterater("MAX")=1
        . ;"else  do
        . ;". SET Iterater("PROGRESS FN","CURRENT")=$GET(Iterater("COUNT"))
        . xecute ProgressFn
        WRITE !
        QUIT

 ;"============================================================
 ;"============================================================


Test
        ;"Purpose: test functionality and usability
        ;"         of plain iterater functions

        NEW Itr,IEN
        NEW abort SET abort=0
        SET IEN=$$ItrInit^TMGITR(22706.9,.Itr)
        DO PrepProgress^TMGITR(.Itr,20,0,"IEN")
        IF IEN'="" FOR  DO  QUIT:(+$$ItrNext^TMGITR(.Itr,.IEN)'>0)!abort
        . IF $$USRABORT^TMGUSRI2 SET abort=1 QUIT
        . ;"WRITE IEN,!
        . ;"other code here...
        DO ProgressDone^TMGITR(.Itr)

        QUIT


Test2
        ;"Purpose: test functionality and usability
        ;"         of iterater functions that return a given field

        NEW Itr,IEN,Name
        NEW abort SET abort=0
        SET Name=$$ItrFInit^TMGITR(22706.9,.Itr,.IEN,.05)
        FOR  DO  QUIT:(($$ItrFNext^TMGITR(.Itr,.IEN,.Name)="@@@")!(+IEN=0))!abort
        . IF $$USRABORT^TMGUSRI2 SET abort=1 QUIT
        . ;"WRITE Name,!
        . ;"other code here...
        DO ProgressDone^TMGITR(.Itr)

        QUIT


Test3
        ;"Purpose: test functionality and usability
        ;"         of iterater functions that work on an array

        NEW Itr,index
        NEW abort SET abort=0
        SET index=$$ItrAInit^TMGITR("^PSDRUG(""B"")",.Itr)
        DO PrepProgress^TMGITR(.Itr,20,1,"index")
        IF index'="" FOR  DO  QUIT:($$ItrANext^TMGITR(.Itr,.index)="")!abort
        . IF $$USRABORT^TMGUSRI2 SET abort=1 QUIT
        . ;"other code here...
        . ;"WRITE index,!
        DO ProgressDone^TMGITR(.Itr)

        QUIT

