TMGFMUT ;TMG/kst/Fileman utility functions ;03/25/06; 3/19/11, 2/2/14
   ;;1.0;TMG-LIB;**1**;07/12/05
  ;
  ;"TMG FILEMAN-UTILITY FUNCTIONS
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
  ;"$$PTRLINKS
  ;"$$FilePtrs(File,OutVarP)
  ;"DispArray(ArrayP,DispdList,indentDepth,MaxDepth)
  ;"ASKPTRIN
  ;"ASKMVPTR
  ;"QTMVPTR(Info,PFn) --quietly redirect pointers.
  ;"QTMMVPTR(Info,SHOWPROG) --quietly redirect multiple pointers at once.
  ;"$$PtrsIn(File,IEN,Array)
  ;"$$PtrsMIn(IENArray,Array,SHOWPROG)
  ;"$$PossPtrs(File,Array)
  ;"$$FMDate(DateStr) -- convert string to FM date, with extended syntax handing
  ;"SETWP(GREF,ARRAY) --directly SET a WP (word processor) field from array
  ;"$$RUNOPT(IEN,DUZ) -- Launch a menuman option
  ;"ASKGL2F --  ask user for global name and show all files using that global
  ;"GL2FIL(REF,ARRAY) -- return all files that uses the specified global
  ;"=======================================================================
  ;"PRIVATE API FUNCTIONS
  ;"=======================================================================
  ;"ScanFile(FInfo,IEN,Array)
  ;"ScanMFile(FInfoArray,IENArray,Array,SHOWPROG)
  ;"HandleSubFile(SearchValue,FileArray,Array,IENS,Ref)
  ;"HandleMSubFile(IENArray,FileArray,Array,IENS,Ref)
  ;"=======================================================================
  ;"DEPENDENCIES
  ;"=======================================================================
  ;"TMGDBAPI
  ;"=======================================================================
  ;
PTRLINKS ;
  ;"Purpose: To examine the Fileman data dictionary for a specified file
  ;"        Then tell any pointers out to other files.  If found, then display
  ;"        this 'dependency'.  Then follow trail to that file, and show it's
  ;"        'dependency'.  Trail will be followed up to N levels deep (set=6 here)
  ;"Results: 1=OKToContinue, 0=failure
  NEW File,Info,DispdList,RESULT
  WRITE "Display pointer dependencies between files.",!!
  read "Enter file name or number to explore (^ to abort): ",File:$GET(DTIME,3600),!
  IF File="^" GOTO PTDone
  SET RESULT=$$FilePtrs(File,"Info")
  IF RESULT=0 WRITE "Error.  Aborting. Sorry about that...",!! GOTO PTDone
  DO DispArray("Info",.DispdList,0,6)  ;"force max depth=6
PTDone ;
  QUIT RESULT
  ;                           
FilePtrs(File,OutVarP)
  ;"For File, create array listing those fields with pointers to other files
  ;"Input: File -- can be file name or number to explore
  ;"  OutVarP -- the name of array to put results into
  ;"Output: Values are put into @OutVarP  as follows:
  ;"      @OutVarP@(FileNum,"FILE NAME")=File Name
  ;"      @OutVarP@(FileNum,FieldNum)=Field Number
  ;"      @OutVarP@(FileNum,FieldNum,"FIELD NAME")=Field Name
  ;"      @OutVarP@(FileNum,FieldNum,"POINTS TO","GREF")=Open format global reference
  ;"      @OutVarP@(FileNum,FieldNum,"POINTS TO","FILE NAME")=File name pointed to
  ;"      @OutVarP@(FileNum,FieldNum,"POINTS TO","FILE NUMBER")=File number pointed to
  ;"      @OutVarP@(FileNum,FieldNum,"X GET")=Code to xecute to get value
  ;"     e.g. SET TMGVALUE=$PIECE($GET(^VA(200,TMGIEN,.11),"^",5))"
  ;"        note: TMGIEN is IEN to lookup, and result is in TMGVALUE
  ;"      @OutVarP@(FileNum,FieldNum,"X SET")=Code to xecute to SET value
  ;"     e.g. SET TMGVALUE=$PIECE(^VA(200,TMGIEN,.11),"^",5)=TMGVALUE"
  ;"      ** For subfiles ** ...
  ;"      @OutVarP@(FileNum,FieldNum,"SUBFILE",FileNum,FieldNum,"FIELD NAME")=Field Name
  ;"      @OutVarP@(FileNum,FieldNum,"SUBFILE",FileNum,FieldNum,"POINTS TO","GREF")=Open format global reference
  ;"      @OutVarP@(FileNum,FieldNum,"SUBFILE",FileNum,FieldNum,"POINTS TO","FILE NAME")=File name pointed to
  ;"      @OutVarP@(FileNum,FieldNum,"SUBFILE",FileNum,FieldNum,"POINTS TO","FILE NUMBER")=File number pointed to
  ;"      @OutVarP@(FileNum,FieldNum,"SUBFILE",FileNum,FieldNum,"X GET")=Code to xecute to get value
  ;"     e.g. SET TMGVALUE=$PIECE($GET(^VA(200,TMGIEN,TMGIEN(1),.11),"^",5))"
  ;"        note: TMGIEN is IEN to lookup, and result is in TMGVALUE
  ;"      @OutVarP@(FileNum,FieldNum,"X SET")=Code to xecute to SET value
  ;"     e.g. SET TMGVALUE=$PIECE(^VA(200,TMGIEN,TMGIEN(1),.11),"^",5)=TMGVALUE"
  ;"      ... etc.
  ;"Results: 1=OKToContinue, 0=failure
  NEW TMGptrArray,RESULT,index,FileNum,FileName
  SET RESULT=$$GetFldList^TMGDBAPI(.File,"TMGptrArray")
  IF RESULT=0 GOTO FPtrDone
  SET RESULT=($GET(OutVarP)'="")
  IF RESULT=0 GOTO FPtrDone
  IF +$GET(File)=0 DO
  . SET FileNum=$$GETFNUM^TMGDBAP3(.File)
  . SET FileName=$GET(File)
  ELSE  DO
  . SET FileNum=+File
  . SET FileName=$$GETFNAME^TMGDBAP2(FileNum)
  SET RESULT=(FileNum'=0)
  IF RESULT=0 GOTO FPtrDone
  ;
  SET index=$ORDER(TMGptrArray(""))
  FOR  DO  QUIT:(RESULT=0)!(index="")
  . NEW fieldnum,TMGFldInfo
  . SET fieldnum=index
  . IF +fieldnum=0 SET RESULT=0 QUIT
  . DO FIELD^DID(FileNum,fieldnum,,"POINTER;MULTIPLE-VALUED","TMGFldInfo","TMGMsg")
  . IF $DATA(TMGMsg) DO  SET RESULT=0 QUIT
  . . IF $GET(TMGDEBUG)>0 DO ZWRITE^TMGZWR("TMGMsg")
  . . IF $DATA(TMGMsg("DIERR"))'=0 DO  QUIT
  . . . DO SHOWDIER^TMGDEBU2(.TMGMsg,.PriorErrorFound)
  . IF $GET(TMGFldInfo("MULTIPLE-VALUED"))=1 DO
  . . ;" handle subfiles via a recursive call
  . . NEW subfile,subArrayP
  . . SET subfile=$$GetSubFileNumber^TMGDBAPI(FileNum,fieldnum)
  . . IF subfile=0 QUIT
  . . SET subArrayP=$name(@OutVarP@(FileNum,fieldnum,"SUBFILE"))
  . . ;"set subArrayP=OutVarP
  . . SET RESULT=$$FilePtrs(subfile,subArrayP)
  . IF $GET(TMGFldInfo("POINTER"))'="" DO
  . . IF +TMGFldInfo("POINTER")>0 QUIT  ;"screen out computed nodes.
  . . IF TMGFldInfo("POINTER")[":" QUIT  ;"screen out SET type fields
  . . NEW gref,node0
  . . SET gref=TMGFldInfo("POINTER")
  . . SET @OutVarP@(FileNum,"FILE NAME")=FileName
  . . SET @OutVarP@(FileNum,fieldnum,"FIELD NAME")=$$GetFldName^TMGDBAPI(FileNum,fieldnum)
  . . SET @OutVarP@(FileNum,fieldnum,"POINTS TO","GREF")=gref
  . . SET gref="^"_gref_"0)"
  . . ;"WRITE "index=",index," gref=",gref,!
  . . SET node0=$GET(@gref)
  . . SET @OutVarP@(FileNum,fieldnum,"POINTS TO","FILE NAME")=$PIECE(node0,"^",1)
  . . SET @OutVarP@(FileNum,fieldnum,"POINTS TO","FILE NUMBER")=+$PIECE(node0,"^",2)
  . . NEW DD SET DD=$GET(^DD(FileNum,fieldnum,0)) QUIT:(DD="")
  . . NEW nodepce SET nodepce=$PIECE(DD,"^",4) QUIT:(nodepce="")
  . . NEW node SET node=+$PIECE(nodepce,";",1) QUIT:(node="")
  . . NEW pce SET pce=+$PIECE(nodepce,";",2) QUIT:(pce'>0)
  . . NEW thisGL SET thisGL=$GET(^DIC(FileNum,0,"GL"))
  . . NEW getCode,setCode
  . . IF thisGL="" DO  QUIT:(thisGL="")
  . . . ;"Note: I am only going to support 1 sub level. More--> brain hurts!
  . . . NEW upNum SET upNum=$GET(^DD(FileNum,0,"UP"))
  . . . IF upNum="" QUIT
  . . . SET thisGL=$GET(^DIC(upNum,0,"GL"))
  . . . IF thisGL="" QUIT  ;"happens with sub-sub.. nodes.
  . . . SET getCode="SET TMGVALUE=$PIECE($GET("_thisGL_"TMGIEN,TMGIEN(1),"_node_")),""^"","_pce_")"
  . . . SET setCode="SET $PIECE("_thisGL_"TMGIEN,TMGIEN(1),"_node_"),""^"","_pce_")=TMGVALUE"
  . . ELSE  DO
  . . . SET getCode="SET TMGVALUE=$PIECE($GET("_thisGL_"TMGIEN,"_node_")),""^"","_pce_")"
  . . . SET setCode="SET $PIECE("_thisGL_"TMGIEN,"_node_"),""^"","_pce_")=TMGVALUE"
  . . SET @OutVarP@(FileNum,fieldnum,"X GET")=getCode
  . . SET @OutVarP@(FileNum,fieldnum,"X SET")=setCode
  . SET index=$ORDER(TMGptrArray(index))
FPtrDone
  QUIT RESULT
  ;
DispArray(ArrayP,DispdList,indentDepth,MaxDepth)
  ;"Purpose: Display array created by FilePtrs (see format there)
  ;"Input: ArrayP : name of array containing information
  ;"  DispdList : array (pass by reference) contining list of files already displayed
  ;"        DispdList("TIU DOCUMENT")=""
  ;"        DispdList("PATIENT")=""  etc.
  ;"  indentDepth : Number of indents deep this function is. Default=0
  ;"  MaxDepth : maximum number of indents deep allowed.

  NEW i,fieldnum,file,FileName
  SET indentDepth=+$GET(indentDepth,0)
  NEW indentS SET indentS=""
  for i=1:1:(indentDepth) s indentS=indentS_". "

  SET file=$ORDER(@ArrayP@(""))
  SET FileName=$GET(@ArrayP@(file,"FILE NAME"))
  SET DispdList(FileName)=""
  IF FileName'="" WRITE indentS,"FILE: ",FileName,!
  SET fieldnum=$ORDER(@ArrayP@(file,""))
  FOR  DO  QUIT:(+fieldnum=0)
  . IF +fieldnum=0 QUIT
  . NEW p2FNAME
  . SET p2FNAME=$GET(@ArrayP@(file,fieldnum,"POINTS TO","FILE NAME"))
  . WRITE indentS,"field: ",$GET(@ArrayP@(file,fieldnum,"FIELD NAME")),"--> file: ",p2FNAME
  . IF $DATA(DispdList(p2FNAME))=0 DO
  . . SET DispdList(p2FNAME)=""
  . . IF indentDepth<MaxDepth DO
  . . . NEW p2Array
  . . . IF $$FilePtrs(p2FNAME,"p2Array")=0 DO  QUIT
  . . . . WRITE " (?)",!
  . . . WRITE !
  . . . DO DispArray("p2Array",.DispdList,indentDepth+1,.MaxDepth)
  . . ELSE  WRITE " (...)",!
  . ELSE  DO
  . . WRITE " (above)",!
  . SET fieldnum=$ORDER(@ArrayP@(file,fieldnum))

  QUIT


ASKPTRIN
  ;"Purpose: An interface shell to PtrsIn.
  ;"      Will ask for name of a file, and then a record in that file.
  ;"      Will then show all pointers to that particular record.

  NEW File,IEN,Array,PFn,RESULT

  WRITE !!,"Pointer Scanner.",!
  WRITE "Will look for all pointers (references) to specified record.",!!
  SET DIC="^DIC("
  SET DIC(0)="MAQE"
  d ^DIC
  SET File=+Y
  IF File'>0 GOTO APTDone
  SET DIC=File
  DO ^DIC
  SET IEN=+Y
  IF IEN'>0 GOTO APTDone
  NEW TMGTIME SET TMGTIME=$H
  ;"set PFn="w TMGCODE,""  "",((TMGCUR/TMGTOTAL)*100)\1,""%"",!"
  SET PFn="do PROGBAR^TMGUSRI2(TMGCUR,""File: ""_$P(TMGCODE,""^"",1),1,TMGTOTAL)"
  WRITE !!,"Starting File Scan for instances of pointers (references) to this record.",!!
  SET RESULT=$$PtrsIn(File,IEN,.Array,PFn)
  IF RESULT=0 WRITE !,"There was some problem.  Sorry.",!! GOTO APTDone

  IF $DATA(Array) DO
  . WRITE !,"Done.  Here are results:",!
  . WRITE "Format is: ",!
  . WRITE "  Array(File#,IEN,0)=LastCount",!
  . WRITE "  Array(File#,IEN,count)=FullRef;piece;IENS;TopGlobalRef",!
  . WRITE "  Description of parts:",!
  . WRITE "  ----------------------",!
  . WRITE "  File# -- the file the found entry exists it (may be a subfile number)",!
  . WRITE "  IEN -- the record number in file",!
  . WRITE "    Note: IEN here is different from the IEN passed in as a parameter",!
  . WRITE "  FullRef -- the is the full reference to the found value.  e.g.",!
  . WRITE "    SET value=$PIECE(@FullRef,""^"",piece)",!
  . WRITE "  piece -- piece where value is stored in the node that is specified by FullRef",!
  . WRITE "  IENS -- this is provided only for matches in subfiles.  ",!
  . WRITE "       It is the IENS that may be used in database calls",!
  . WRITE "  TopGlobalRef -- this is the global reference for file.  If the match is in a",!
  . WRITE "      subfile, then this is the global reference of the parent file ",!
  . WRITE "      (or the highest grandparent file IF the parent file itself is",!
  . WRITE "      a subfile)",!
  . DO ZWRITE^TMGZWR("Array")
  . WRITE "---------------------------",!
  . NEW temp read "Press [ENTER] to continue.",temp:$GET(DTIME,3600),!
  ELSE  WRITE !,"No pointers to that record found.",!

APTDone
  QUIT

SCRLPTRIN
  ;"Purpose: An scrolling interface shell to PtrsIn.
  ;"      Will ask for name of a file, and then a record in that file.
  ;"      Will then show all pointers to that particular record.
  ;"      Will then allow one to trace along pointer path (in or out)

  NEW File,IEN,Array,PFn,RESULT
  NEW AFile,AIEN,ACount
  NEW ShowArray,ShowResults,Header,Count
  NEW PickStr,PickInfo,Abort,Menu,UsrSlct
  NEW DIC,X,Y
  WRITE !!,"Pointer Scanner/Browser.",!
  WRITE "Will look for all pointers (references) to specified record.",!!
  SET DIC="^DIC(",DIC(0)="MAQE" DO ^DIC SET File=+Y
  IF File'>0 GOTO SCPTDone
  SET DIC=File DO ^DIC SET IEN=+Y
  IF IEN'>0 GOTO SCPTDone
  NEW TMGTIME SET TMGTIME=$H
  SET PFn="do PROGBAR^TMGUSRI2(TMGCUR,""File: ""_$P(TMGCODE,""^"",1),1,TMGTOTAL)"
SCPT1   ;
  WRITE !!,"Scanning files for instances of pointers (references) to this record.",!!
  SET RESULT=$$PtrsIn(File,IEN,.Array,PFn)
  IF RESULT=0 DO  GOTO APTDone
  . WRITE !,"There was some problem.  Sorry.",!!
  . DO PRESS2GO^TMGUSRI2
  ;"Returned format is: ",!
  ;"  Array(File#,IEN,0)=LastCount",!
  ;"  Array(File#,IEN,count)=FullRef;piece;IENS;TopGlobalRef",!
  ;"  Description of parts:",!
  SET File=0,Abort=0
  FOR  SET File=$ORDER(Array(File)) QUIT:(+File'>0)  DO
  . NEW FNAME SET FNAME=$PIECE($GET(^DIC(File,0)),"^",1)
  . NEW IEN SET IEN=0
  . FOR  SET IEN=$ORDER(Array(File,IEN)) QUIT:(+IEN'>0)  DO
  . . NEW Value01 SET Value01=$$GET1^DIQ(File,IEN,.01)
  . . SET Count=0
  . . FOR  SET Count=$ORDER(Array(File,IEN,Count)) QUIT:(+Count'>0)  DO
  . . . NEW Str SET Str=FNAME_"; #"_IEN_"; "_Value01
  . . . IF Count>1 SET Str=Str_" ("_Count_")"
  . . . SET ShowArray(Str,File_"^"_IEN_"^"_Count)=""
  SET Header="Pick ONE (and only ONE) record to explore.  Press ESC ESC when done."
SCPT2  ;
  KILL ShowResults
  IF $GET(TMGPTCABORT)=1 GOTO SCPTDone
  DO SELECTR2^TMGUSRI3("ShowArray","ShowResults",Header)
  ;
  SET Count=$$LISTCT^TMGMISC2("ShowResults")
  IF Count>1 DO  GOTO SCPT2
  . WRITE "Please pick ONE (and only ONE) record to explore.",!
  . WRITE "You selected at least ",Count,!
  . WRITE "Enter ^ to QUIT",!
  . DO PRESS2GO^TMGUSRI2
  ;
  SET PickStr=""
  SET PickStr=$ORDER(ShowResults(PickStr))
  IF PickStr="" DO  GOTO SCPTDone
  . WRITE "No selected record.  Goodbye.",!
  . DO PRESS2GO^TMGUSRI2
  ;
  SET Count=$$LISTCT^TMGMISC2("ShowArray("_PickStr_")")
  IF Count>0 DO  GOTO SCPTDone
  . SET Abort=1
  . WRITE "Please pick ONE (and only ONE) record to explore.",!
  . WRITE "You selected at least ",Count,!
  . DO PRESS2GO^TMGUSRI2
  ;
  SET PickInfo=$ORDER(ShowResults(PickStr,""))
  SET AFile=$PIECE(PickInfo,"^",1)
  SET AIEN=$PIECE(PickInfo,"^",2)
  SET ACount=$PIECE(PickInfo,"^",3)
  ;
  SET Menu(0)="Pick Option."
  SET Menu(1)="Show info for this selected record"_$C(9)_"ShowInfo"
  SET Menu(2)="DUMP this record"_$C(9)_"DumpRec"
  SET Menu(3)="Show pointers INTO selected record"_$C(9)_"ShowPtrIN"
  SET Menu(4)="Browse to other records pointed OUT from this record."_$C(9)_"BrowseOUT"
MC1 ;
  WRITE #
  SET UsrSlct=$$MENU^TMGUSRI2(.Menu,"^")
  IF UsrSlct="^" DO  GOTO SCPTDone
  . WRITE "Goodbye.",!
  IF UsrSlct=0 SET UsrSlct=""
  IF UsrSlct="ShowInfo" DO  GOTO MC1
  . IF $DATA(Array(AFile,AIEN,ACount))=0 QUIT
  . DO ZWRITE^TMGZWR($NAME(Array(AFile,AIEN,ACount)))
  . DO PRESS2GO^TMGUSRI2
  IF UsrSlct="DumpRec" DO  GOTO MC1
  . DO DUMPREC^TMGDEBU3(AFile,AIEN,0)
  . DO PRESS2GO^TMGUSRI2
  IF UsrSlct="ShowPtrIN" DO  GOTO SCPT1
  . SET File=AFile
  . SET IEN=AIEN
  . SET Count=ACount
  IF UsrSlct="BrowseOUT" DO  GOTO MC1
  . DO Browse^TMGBROWS(AFile,AIEN,0)
  . DO PRESS2GO^TMGUSRI2
  GOTO MC1
SCPTDone ;
  QUIT
  ;
  ;
ASKMVPTR
  ;"Purpose: An interface shell toRedirect any pointer.
  ;"      Will ask for name of a file, and then a record in that file.
  ;"      Will then pass information to fileman function to move pointers.

  ;"Note: Example of array passed to P^DITP
  ;"        23510 is $J
  ;"        47 is IEN to be deleted in file 50 (stored at ^PSDRUG(*))
  ;"        1646 is IEN to be substituted for all 47's
  ;"
  ;"        First part of array is list of all files & fields that point to file
  ;"        ----------------
  ;"        ^UTILITY("DIT",23510,0,1)="727.819^67^P50'"
  ;"        ...
  ;"        ^UTILITY("DIT",23510,0,54)="801.43^.02^RV"
  ;"        ^UTILITY("DIT",23510,0,55)="810.31^.04^V"
  ;"        ^UTILITY("DIT",23510,0,56)="810.32^.01^V"
  ;"        ^UTILITY("DIT",23510,0,57)="811.52^.01^MVX"
  ;"        ^UTILITY("DIT",23510,0,58)="811.902^.01^MVX"
  ;"        ^UTILITY("DIT",23510,0,59)="9009032.4^.05^P50'"
  ;"
  ;"        Second part of array is list of changes that should be made.  Only 1 change shown here.
  ;"        ----------------
  ;"        ^UTILITY("DIT",23510,47)="1646;PSDRUG("
  ;"        ^UTILITY("DIT",23510,"47;PSDRUG(")="1646;PSDRUG("

  NEW File,fromIEN,toIEN,Array,PFn,RESULT
  NEW PossPtrs

  WRITE !,"Pointer Redirection Utility",!
  WRITE "Will change pointers to FROM to TO value",!

  KILL DIC
  SET DIC("A")="Select File holding records being pointed to: "
  SET DIC="^DIC("
  SET DIC(0)="MAQE"
  d ^DIC  ;"Get File to search
  SET File=+Y
  IF File'>0 GOTO AMPTDone

  ;"Get list of files/fields with pointers in
  SET RESULT=$$PossPtrs(File,.PossPtrs) IF RESULT=0 GOTO AMPTDone
  IF $DATA(PossPtrs)'>0 GOTO AMPTDone

  SET DIC=File
  SET DIC("A")="Select Original (i.e OLD) Record: "
  DO ^DIC  ;"get FROM record in File
  SET fromIEN=+Y
  IF fromIEN'>0 GOTO AMPTDone

  SET DIC("A")="Select New Record: "
  DO ^DIC  ;"get FROM record in File
  SET toIEN=+Y
  IF toIEN'>0 GOTO AMPTDone

  ;"set PFn="w TMGCODE,""  "",((TMGCUR/TMGTOTAL)*100)\1,""%"",!"
  ;"new TMGTIME SET TMGTIME=$H
  SET PFn="do PROGBAR^TMGUSRI2(TMGCUR,""Scanning File: ""_$P(TMGCODE,""^"",1),1,TMGTOTAL)"
  WRITE !!,"Starting File Scan for instances of pointers (references) to this record.",!!
  SET RESULT=$$PtrsIn(File,fromIEN,.Array,PFn) IF RESULT=0 GOTO AMPTDone

  ;" WRITE !,"Here are possible pointers in (file level)",!
  ;" IF $DATA(PossPtrs) DO ZWRITE^TMGZWR("PossPtrs")

  ;" WRITE !,"Here are actual pointers in",!
  ;" IF $DATA(Array) DO ZWRITE^TMGZWR("Array")

  ;"Now convert to FileMan Format.
  KILL ^UTILITY("DIT",$J)
  DO Prep4FM(.Array)

  IF $DATA(^UTILITY("DIT",$J)) DO
  . MERGE ^UTILITY("DIT",$J,0)=PossPtrs
  . ;"WRITE !,"here are results",!
  . ;" DO ZWRITE^TMGZWR($NAME(^UTILITY("DIT",$J)))
  . SET DIR(0)="Y",DIR("B")="YES"
  . SET DIR("A")="Ask Fileman to redirect pointers?"
  . SET DIR("?")="Enter YES IF you want Fileman to change all instances of the FROM record into the TO record."
  . DO ^DIR ;"get user response
  . IF +Y'=1 QUIT
  . WRITE "YES",!
  . DO PTS^DITP
  ELSE  DO
  . WRITE "No matches found...",!!

AMPTDone
  QUIT


QTMVPTR(Info,PFn)   ;"NOTE: this function hasn't been debugged/tested yet
  ;"Purpose: An interface to quietly redirect any pointer.
  ;"Input: Info, an array containing info for redirecting pointers.
  ;"        Format:   Note: File can be file name or number.
  ;"        Info(File,OldIEN)=newIEN
  ;"        Info(File,OldIEN)=newIEN1
  ;"        Info(File,OldIEN)=newIEN
  ;"      PFn: OPTIONAL, a progress function (must be a complete M expression)
  ;"Output: all pointers in linked files to OldIEN will be changed to newIEN
  ;"Results: none

  ;"Note: Example of array passed to P^DITP
  ;"        23510 is $J
  ;"        47 is IEN to be deleted in file 50 (stored at ^PSDRUG(*))
  ;"        1646 is IEN to be substituted for all 47's
  ;"
  ;"        First part of array is list of all files & fields that point to file
  ;"        ----------------
  ;"        ^UTILITY("DIT",23510,0,1)="727.819^67^P50'"
  ;"        ...
  ;"        ^UTILITY("DIT",23510,0,54)="801.43^.02^RV"
  ;"        ^UTILITY("DIT",23510,0,55)="810.31^.04^V"
  ;"        ^UTILITY("DIT",23510,0,56)="810.32^.01^V"
  ;"        ^UTILITY("DIT",23510,0,57)="811.52^.01^MVX"
  ;"        ^UTILITY("DIT",23510,0,58)="811.902^.01^MVX"
  ;"        ^UTILITY("DIT",23510,0,59)="9009032.4^.05^P50'"
  ;"
  ;"        Second part of array is list of changes that should be made.  Only 1 change shown here.
  ;"        ----------------
  ;"        ^UTILITY("DIT",23510,47)="1646;PSDRUG("
  ;"        ^UTILITY("DIT",23510,"47;PSDRUG(")="1646;PSDRUG("

  NEW File,Array,RESULT
  SET PFn=$GET(PFn)
  NEW Itr,File

  ;"Cycle through all files to be changed.
  SET File=$$ItrAInit^TMGITR("Info",.Itr)
  IF File'="" FOR  DO  QUIT:($$ItrANext^TMGITR(.Itr,.File)="")
  . NEW PossPtrs
  . IF +File'=File SET File=$$GETFNUM^TMGDBAP3(File)             ;Convert File Name to File Number
  . ;"Get list of files/fields with pointers in
  . SET RESULT=$$PossPtrs(File,.PossPtrs) IF RESULT=0 QUIT
  . IF $DATA(PossPtrs)'>0 QUIT
  . KILL ^UTILITY("DIT",$J)
  . NEW fromIEN,toIEN,fromItr
  . SET fromIEN=+$$ItrAInit^TMGITR($name(Info(File)),.fromItr)
  . NEW done2 SET done2=0
  . ;"Cycle through all records to be changed.
  . IF fromIEN'=0 FOR  DO  QUIT:(+$$ItrANext^TMGITR(.fromItr,.fromIEN)=0)!(done2=1)
  . . SET toIEN=$GET(Info(File,fromIEN))
  . . SET RESULT=$$PtrsIn(File,fromIEN,.Array,PFn) IF RESULT=0 SET done2=1
  . . DO Prep4FM(.Array)
  . IF $DATA(^UTILITY("DIT",$J))=0 QUIT
  . MERGE ^UTILITY("DIT",$J,0)=PossPtrs
  . DO PTS^DITP  ;"Note: call separately for each file specified.

QMPTDone
  QUIT


QTMMVPTR(Info,SHOWPROG)   ;"NOTE: this function hasn't been debugged/tested yet
  ;"Purpose: An interface to quietly redirect multiple pointer.
  ;"NOTE: This functions differes from QTMVPTR in that it can look for all IEN's
  ;"      for a given file at once, speeding database access.
  ;"Input: Info, an array containing info for redirecting pointers.
  ;"        Format:   Note: File can be file name or number.
  ;"        Info(File,OldIEN)=newIEN
  ;"        Info(File,OldIEN)=newIEN1
  ;"        Info(File,OldIEN)=newIEN
  ;"      SHOWPROG: IF 1, progress bar shown
  ;"Output: all pointers in linked files to OldIEN will be changed to newIEN
  ;"Results: none

  ;"Note: Example of array passed to P^DITP
  ;"        23510 is $J
  ;"        47 is IEN to be deleted in file 50 (stored at ^PSDRUG(*))
  ;"        1646 is IEN to be substituted for all 47's
  ;"
  ;"        First part of array is list of all files & fields that point to file
  ;"        ----------------
  ;"        ^UTILITY("DIT",23510,0,1)="727.819^67^P50'"
  ;"        ...
  ;"        ^UTILITY("DIT",23510,0,54)="801.43^.02^RV"
  ;"        ^UTILITY("DIT",23510,0,55)="810.31^.04^V"
  ;"        ^UTILITY("DIT",23510,0,56)="810.32^.01^V"
  ;"        ^UTILITY("DIT",23510,0,57)="811.52^.01^MVX"
  ;"        ^UTILITY("DIT",23510,0,58)="811.902^.01^MVX"
  ;"        ^UTILITY("DIT",23510,0,59)="9009032.4^.05^P50'"
  ;"
  ;"        Second part of array is list of changes that should be made.  Only 1 change shown here.
  ;"        ----------------
  ;"        ^UTILITY("DIT",23510,47)="1646;PSDRUG("
  ;"        ^UTILITY("DIT",23510,"47;PSDRUG(")="1646;PSDRUG("

  NEW ToFile,Array,RESULT
  SET PFn=$GET(PFn)
  NEW Itr

  ;"Cycle through all files to be changed.
  SET ToFile=$$ItrAInit^TMGITR("Info",.Itr)
  IF ToFile'="" FOR  DO  QUIT:($$ItrANext^TMGITR(.Itr,.ToFile)="")
  . NEW PossPtrs
  . IF +ToFile'=ToFile SET ToFile=$$GETFNUM^TMGDBAP3(ToFile)  ;"Convert File Name to File Number
  . ;"Get list of files/fields with pointers in
  . SET RESULT=$$PossPtrs(ToFile,.PossPtrs) IF RESULT=0 QUIT
  . IF $DATA(PossPtrs)'>0 QUIT
  . KILL ^UTILITY("DIT",$J)
  . ;"new fromIEN,toIEN,fromItr
  . ;"set fromIEN=+$$ItrAInit^TMGITR($name(Info(ToFile)),.fromItr)
  . NEW IENArray SET IENArray=ToFile
  . MERGE IENArray=Info(ToFile)
  . SET IENArray=ToFile
  . SET RESULT=$$PtrsMIn(.IENArray,.Array,.SHOWPROG)
  . NEW toFile2,toIEN,fromFile,fromIEN,Array2
  . SET toFile2=""
  . FOR  SET toFile2=$ORDER(Array(toFile2)) QUIT:(toFile2="")  DO
  . . SET toIEN=""
  . . FOR  SET toIEN=$ORDER(Array(toFile2,toIEN)) QUIT:(toIEN="")  DO
  . . . SET fromFile=""
  . . . FOR  SET fromFile=$ORDER(Array(toFile2,toIEN,fromFile)) QUIT:(fromFile="")  DO
  . . . . SET fromIEN=""
  . . . . FOR  SET fromIEN=$ORDER(Array(toFile2,toIEN,fromFile,fromIEN)) QUIT:(fromIEN="")  DO
  . . . . . MERGE Array2(fromFile,fromIEN)=Array(toFile2,toIEN,fromFile,fromIEN)
  . SET toFile2=""
  . FOR  SET toFile2=$ORDER(Array2(toFile2)) QUIT:(toFile2="")  DO
  . . DO MPrep4FM(toFile2,.Array2)
  . . IF $DATA(^UTILITY("DIT",$J))=0 QUIT
  . . MERGE ^UTILITY("DIT",$J,0)=PossPtrs
  . . DO PTS^DITP  ;"Note: call separately for each file specified.

QMMPTDone
  QUIT


Prep4FM(Array)
  ;"Purpose: to convert Array with redirection info into format for Fileman
  ;"Input: Array -- PASS BY REFERENCE.  An array as created by PtrsIn()
  ;"Output: Data will be put into ^UTILITY('DIT',$J)
  ;"Results: none

  ;"Now convert to FileMan Format.
  NEW iFile,iIEN,count,index,toRef
  SET iFile=$ORDER(Array(""))
  IF +iFile'=0 FOR  DO  QUIT:(+iFile=0)
  . SET iIEN=$ORDER(Array(iFile,""))
  . IF +iIEN'=0 FOR  DO  QUIT:(+iIEN=0)
  . . SET count=+$GET(Array(iFile,iIEN,0))
  . . for index=1:1:count DO
  . . . SET toRef=$PIECE($GET(Array(iFile,iIEN,count)),";",4)
  . . . SET toRef=$EXTRACT(toRef,2,999)
  . . . SET ^UTILITY("DIT",$J,fromIEN)=toIEN_";"_toRef
  . . . SET ^UTILITY("DIT",$J,""_fromIEN_";"_toRef_"")=""_toIEN_";"_toRef_""
  . . SET iIEN=$ORDER(Array(iFile,iIEN))
  . SET iFile=$ORDER(Array(iFile))

  QUIT


MPrep4FM(fromFile,Array)
  ;"Purpose: to convert Array with redirection info into format for Fileman
  ;"Input: fromFile -- the FromFileNum -- Note: should be called once for
  ;"  each File number
  ;"  Array -- PASS BY REFERENCE.  An array as created by PtrsMIn()
  ;"  Array(FromFile#,fromIEN,0)=LastCount
  ;"  Array(FromFile#,fromIEN,count)=FullRef;piece;IENS;TopGlobalRef
  ;"Output: Data will be put into ^UTILITY('DIT',$J)
  ;"Results: none

  ;"Now convert to FileMan Format.
  NEW fromIEN SET fromIEN=""                   
  FOR  SET fromIEN=$ORDER(Array(fromFile,fromIEN)) QUIT:(+fromIEN'>0)  DO
  . NEW count
  . SET count=+$GET(Array(fromFile,fromIEN,0))
  . NEW index for index=1:1:count DO
  . . NEW toRef
  . . SET toRef=$PIECE($GET(Array(fromFile,fromIEN,count)),";",4)
  . . SET toRef=$EXTRACT(toRef,2,999)
  . . SET ^UTILITY("DIT",$J,fromIEN)=toIEN_";"_toRef
  . . SET ^UTILITY("DIT",$J,""_fromIEN_";"_toRef_"")=""_toIEN_";"_toRef_""

  QUIT


PtrsIn(File,IEN,Array,PrgsFn)
  ;"SCOPE: PUBLIC
  ;"Purpose:  Create a list of  incoming pointers to a given record in given file
  ;"Input: File:    The file to investigate (Number or Name)
  ;"   IEN:    IEN of record to
  ;"   Array -- PASS BY REFERENCE.  An array to receive results back.
  ;"        any prexisting data in Array is killed before filling
  ;"   PrgsFn:   OPTIONAL -- <Progress Function Code>
  ;"            because this search process can be QUITe lengthy,
  ;"            an optional line of M code may be given here that will be executed
  ;"            before each file is scanned.  The following variables will be defined:
  ;"              TMGCODE -- will hold code of current file being scanned.
  ;"              TMGTOTAL -- will hold total number of records to scan
  ;"              TMGCUR -- will hold count of current record being scanned.
  ;"Output:  Array is filled with format as follows:
  ;"        Array(File#,IEN,0)=LastCount
  ;"        Array(File#,IEN,count)=FullRef;piece;IENS;TopGlobalRef
  ;"          Description of parts:
  ;"          ----------------------
  ;"          File# -- the file the found entry exists it (may be a subfile number)
  ;"          IEN -- the record number in file
  ;"            Note: IEN here is different from the IEN passed in as a parameter
  ;"          FullRef -- the is the full reference to the found value.  e.g.
  ;"            SET value=$PIECE(@FullRef,"^",piece)
  ;"          piece -- the piece where value is stored in the node that is specified by FullRef
  ;"          IENS -- this is provided only for matches in subfiles.  It is the IENS that may be used in database calls
  ;"          TopGlobalRef -- this is the global reference for file.  If the match is in a subfile, then
  ;"              this is the global reference of the parent file (or the highest grandparent file if
  ;"              the parent file itself is a subfile, etc.)
  ;"
  ;"Result: 1 IF results found, 0 IF error occurred.
  ;"NOTE: This function manually scans through potentially HUGE numbers of records-->BE PATIENT!
  KILL Array
  NEW RESULT SET RESULT=0
  NEW FileNum
  SET IEN=+$GET(IEN) IF IEN=0 GOTO FPIDone   ;"NOTE: IEN doesn't have to point to a valid record.
  IF $DATA(File)#10=0 GOTO FPIDone
  IF +File=0 SET FileNum=$$GETFNUM^TMGDBAP3(File)   ;"Convert File Name to File Number
  ELSE  SET FileNum=File
  IF +FileNum=0 GOTO FPIDone
  NEW PossArray,TMGCODE
  IF $$PossPtrs(File,.PossArray)=0 GOTO FPIDone
  ;
  ;"Count number of records to scan
  NEW TMGCUR SET TMGCUR=0
  NEW TMGTOTAL SET TMGTOTAL=0
  DO
  . NEW temp SET temp=""
  . FOR  SET temp=$ORDER(PossArray(temp)) QUIT:(temp="")  DO
  . . NEW code SET code=PossArray(temp)
  . . NEW REF SET REF=$GET(^DIC(+code,0,"GL"))
  . . NEW NumRecs
  . . IF REF="" SET NumRecs=10000 ;"some arbitrary guess of #recs in a subfile 
  . . ELSE  DO
  . . . SET REF=$$CREF^DILF(REF)  ;"convert open to closed format
  . . . SET NumRecs=+$PIECE($GET(@REF@(0)),"^",4)
  . . SET TMGTOTAL=TMGTOTAL+1,TMGTOTAL(TMGTOTAL)=NumRecs
  . SET temp=$ORDER(TMGTOTAL(""))
  . SET TMGTOTAL=1
  . IF temp'="" FOR  DO  QUIT:(temp="")
  . . SET TMGTOTAL=TMGTOTAL+TMGTOTAL(temp)
  . . SET temp=$ORDER(TMGTOTAL(temp))
  . IF TMGTOTAL=0 SET TMGTOTAL=1  ;"avoid div by zero issues.
  ;
  NEW count SET count=1
  NEW index SET index=$ORDER(PossArray(""))
  IF index'="" FOR  DO  QUIT:(index="")
  . SET TMGCUR=TMGCUR+TMGTOTAL(count)
  . SET count=count+1
  . SET TMGCODE=PossArray(index)
  . IF $GET(PrgsFn)'="" DO
  . . NEW $ETRAP SET $ETRAP="write ""(Invalid M Code!.  Error Trapped.)"" SET $ETRAP="""",$ecode="""""
  . . xecute PrgsFn
  . DO ScanFile(TMGCODE,IEN,.Array)
  . SET index=$ORDER(PossArray(index))

  SET RESULT=1
FPIDone
  QUIT RESULT


PtrsMIn(IENArray,Array,SHOWPROG)
  ;"SCOPE: PUBLIC
  ;"Purpose:  Create a list of  incoming pointers to an array of records in given file
  ;"NOTE: this function differes from PtrsIn because is allows multiple input IEN's
  ;"Input:  IENArray:   PASS BY REFERENCE.  Array of IENs of record in ToFile.  Format:
  ;"          IENArray=SourceFile#
  ;"          IENArray(IEN)=""
  ;"          IENArray(IEN)=""
  ;"   Array -- PASS BY REFERENCE.  An array to receive results back. Format below.
  ;"        any prexisting data in Array is killed before filling
  ;"   SHOWPROG: IF 1, progress bar shown
  ;"Output:  Array is filled with format as follows:
  ;"        Array(ToFile#,ToIEN,FromFile#,fromIEN,0)=LastCount
  ;"        Array(ToFile#,ToIEN,FromFile#,fromIEN,count)=FullRef;piece;IENS;TopGlobalRef
  ;"          Description of parts:
  ;"          ----------------------
  ;"          ToFile# -- the file containing the target IEN record
  ;"          ToIEN --the IEN in ToFile
  ;"          FromFile# -- the file the found entry exists it (may be a subfile number)
  ;"          fromIEN -- the record number in file
  ;"            Note: IEN here is different from the IEN passed in as a parameter
  ;"          FullRef -- the is the full reference to the found value.  e.g.
  ;"            SET value=$PIECE(@FullRef,"^",piece)
  ;"          piece -- the piece where value is stored in the node that is specified by FullRef
  ;"          IENS -- this is provided only for matches in subfiles.  It is the IENS that may be used in database calls
  ;"          TopGlobalRef -- this is the global reference for file.  If the match is in a subfile, then
  ;"              this is the global reference of the parent file (or the highest grandparent file if
  ;"              the parent file itself is a subfile, etc.)
  ;"
  ;"Result: 1 IF results found, 0 IF error occurred.
  ;"NOTE: This function manually scans through potentially HUGE numbers of records-->BE PATIENT!

  KILL Array
  NEW RESULT SET RESULT=0
  NEW FileNum
  SET ToFile=$GET(IENArray) IF ToFile="" GOTO FMPIDone
  IF +ToFile=0 SET FileNum=$$GETFNUM^TMGDBAP3(File)   ;"Convert File Name to File Number
  ELSE  SET FileNum=ToFile
  IF +FileNum=0 GOTO FMPIDone

  NEW PossArray
  IF $$PossPtrs(FileNum,.PossArray)=0 GOTO FMPIDone

  NEW FInfoArray
  NEW index SET index=""
  FOR  SET index=$ORDER(PossArray(index)) QUIT:(index="")  DO
  . NEW tempS SET tempS=$GET(PossArray(index))
  . NEW fromFile SET fromFile=$PIECE(tempS,"^",1)
  . NEW fromField SET fromField=$PIECE(tempS,"^",2)
  . NEW fldCode SET fldCode=$PIECE(tempS,"^",3)
  . SET FInfoArray(fromFile,fromField)=fldCode

  DO ScanMFile(.FInfoArray,.IENArray,.Array,.SHOWPROG)

  SET RESULT=1
FMPIDone
  QUIT RESULT


ScanFile(FInfo,IEN,Array)
  ;"SCOPE: PUBLIC
  ;"Purpose: To scan one file (from array setup by PossPtrs) for actual pointers to IEN
  ;"Input:  FInfo  : OtherFile#^Field#^FieldCode(piece#2 of 0 node of ^DD entry for field)
      ;"Examples of possible inputs follow:
        ;"50^62.05^*P50'"
        ;"695^.01^RP50'"
        ;"801.43^.02^RV"
        ;"810.31^.04^V"
        ;"811.902^.01^MVX"

  ;"NOTE: Idea for future enhancement: Allow FInfo to hold a list rather than just one value.
  ;"        This would be for instances where multiple fields in given record need to be searched
  ;"        This might speed up database access times.

  ;"   IEN  : the IEN that pointers should point to, to be considered a match.
  ;"   Array : PASS BY REFERENCE.  An array to receive results.    
  ;"Output:  Format of Array output:
  ;"        Array(File#,IEN,0)=LastCount
  ;"        Array(File#,IEN,count)=FullRef;piece;IENS;TopGlobalRef
  ;"          Description of parts:
  ;"          ----------------------
  ;"          File# -- the file the found entry exists it (may be a subfile number)
  ;"          IEN -- the record number in file
  ;"            Note: IEN here is different from the IEN passed in as a parameter
  ;"          FullRef -- the is the full reference to the found value.  e.g.
  ;"            SET value=$PIECE(@FullRef,"^",piece)
  ;"          piece -- the piece where value is stored in the node that is specified by FullRef
  ;"          IENS -- this is provided only for matches in subfiles.  It is the IENS that may be used in database calls
  ;"          TopGlobalRef -- this is the global reference for file.  If the match is in a subfile, then
  ;"              this is the global reference of the parent file (or the highest grandparent file if
  ;"              the parent file itself is a subfile, etc.)
  ;"
  ;"result : none

  NEW File SET File=$PIECE(FInfo,"^",1) IF File="" GOTO SFDone
  NEW Field SET Field=$PIECE(FInfo,"^",2) IF Field="" GOTO SFDone
  NEW Code SET Code=$PIECE(FInfo,"^",3) IF Code="" GOTO SFDone
  NEW count
  IF '((Code["P")!(Code["V")) GOTO SFDone
  NEW GREF
  NEW znode SET znode=$GET(^DD(File,Field,0))
  NEW loc SET loc=$PIECE(znode,"^",4)
  NEW node SET node=$PIECE(loc,";",1)
  NEW pce SET pce=$PIECE(loc,";",2)
  IF +$$IsSubFile^TMGDBAPI(File) DO
  . NEW FileArray,i,k,FNum,SubInfo
  . SET i=0
  . SET FileArray(0)=0
  . SET FileArray(i,"PARENT","LOC")=loc
  . SET FNum=File
  . FOR  DO  QUIT:(+FNum=0)  ;"setup array describing subfile's inheritence
  . . SET i=i+1
  . . SET FileArray(i)=FNum
  . . IF i=1 SET FileArray(0,"FILE")=FNum
  . . IF $$GetSubFInfo^TMGDBAPI(FNum,.SubInfo) DO
  . . . SET FileArray(i,"PARENT","LOC")=SubInfo("FIELD IN PARENT","LOC")
  . . . SET GREF=$GET(SubInfo("PARENT","GL")) ;"<-- only valid for highest ancestor
  . . ELSE  DO
  . . . SET (FileArray(0,"TOP GL"),FileArray(i,"PARENT","GL"))=$GET(^DIC(FNum,0,"GL"))
  . . SET FNum=$$IsSubFile^TMGDBAPI(FNum)
  . DO HandleSubFile(IEN,.FileArray,.Array)
  ELSE  DO
  . SET GREF=$GET(^DIC(File,0,"GL"))
  . NEW ORef SET ORef=GREF
  . SET GREF=$$CREF^DILF(GREF)  ;"convert open to closed format
  . NEW index SET index=$ORDER(@GREF@(0))
  . IF index'="" FOR  DO  QUIT:(index="")
  . . NEW value SET value=$GET(@GREF@(index,node))
  . . IF $PIECE(value,"^",pce)=IEN DO
  . . . SET Array(File,index,0)=1
  . . . SET Array(File,index,1)=$name(@GREF@(index,node))_";"_pce_";"_""_";"_ORef
  . . SET index=$ORDER(@GREF@(index))

SFDone
  QUIT


ScanMFile(FInfoArray,IENArray,Array,SHOWPROG)
  ;"SCOPE: PUBLIC
  ;"Purpose: To scan multiple file (from array setup by PossPtrs) for actual pointers to IENs
  ;"Input:  FInfoArray  : PASS BY REFERENCE.  Format:
  ;"        FInfoArray(OtherFile,Field)=FieldCode(piece#2 of 0 node of ^DD entry for field)
  ;"        Examples of possible inputs follow:
  ;"          FInfoArray(50,62.05)="*P50'"
  ;"          FInfoArray(695,.01)="RP50'"
  ;"          FInfoArray(801.43,.02)="RV"
  ;"          FInfoArray(810.31,.04)="V"
  ;"          FInfoArray(811.902,.01)="MVX"
  ;"   IENArray : PASS BY REFERENCE.  IEN's that pointers should point TO, to be considered a match.
  ;"          Format: IENArray=SourceFile
  ;"            IENArray(IEN)=""
  ;"            IENArray(IEN)=""
  ;"   Array : PASS BY REFERENCE.  AN OUT PARAMETER.  Format:
  ;"        Array(ToFile#,ToIEN,fromFile#,fromIEN,0)=LastCount
  ;"        Array(ToFile#,ToIEN,fromFile#,fromIEN,count)=FullRef;piece;IENS;TopGlobalRef
  ;"          Description of parts:
  ;"          ----------------------
  ;"          ToFile# -- the file containing the target IEN record
  ;"          ToIEN --the IEN in ToFile
  ;"          fromFile# -- the file the found entry exists it (may be a subfile number)
  ;"          fromIEN -- the record number in file
  ;"            Note: IEN here is different from the IEN passed in as a parameter
  ;"          FullRef -- the is the full reference to the found value.  e.g.
  ;"            SET value=$PIECE(@FullRef,"^",piece)
  ;"          piece -- the piece where value is stored in the node that is specified by FullRef
  ;"          IENS -- this is provided only for matches in subfiles.  It is the IENS that may be used in database calls
  ;"          TopGlobalRef -- this is the global reference for file.  If the match is in a subfile, then
  ;"              this is the global reference of the parent file (or the highest grandparent file if
  ;"              the parent file itself is a subfile, etc.)
  ;"   SHOWPROG: IF 1, progress bar shown
  ;"
  ;"result : none

  NEW ToFile SET ToFile=+$GET(IENArray)
  SET SHOWPROG=$GET(SHOWPROG,0)
  NEW abort SET abort=0
  SET fromFile=""
  FOR  SET fromFile=$ORDER(FInfoArray(fromFile)) QUIT:(fromFile="")!abort  DO
  . IF $$USRABORT^TMGUSRI2 SET abort=1 QUIT
  . WRITE !,"Processing File#: ",fromFile,!
  . NEW Field SET Field=""
  . FOR  SET Field=$ORDER(FInfoArray(fromFile,Field)) QUIT:(Field="")  DO
  . . WRITE "    Field#: ",Field,!
  . . NEW Code SET Code=$GET(FInfoArray(fromFile,Field)) IF Code="" QUIT
  . . NEW count
  . . IF '((Code["P")!(Code["V")) GOTO SFDone
  . . NEW GREF
  . . NEW znode SET znode=$GET(^DD(fromFile,Field,0))
  . . NEW loc SET loc=$PIECE(znode,"^",4)
  . . NEW node SET node=$PIECE(loc,";",1)
  . . NEW pce SET pce=$PIECE(loc,";",2)
  . . IF +$$IsSubFile^TMGDBAPI(fromFile) DO
  . . . NEW FileArray,i,k,FNum,SubInfo
  . . . SET i=0
  . . . SET FileArray(0)=0
  . . . SET FileArray(i,"PARENT","LOC")=loc
  . . . SET FNum=fromFile
  . . . FOR  DO  QUIT:(+FNum=0)  ;"setup array describing subfile's inheritence
  . . . . SET i=i+1
  . . . . SET FileArray(i)=FNum
  . . . . IF i=1 SET FileArray(0,"FILE")=FNum
  . . . . IF $$GetSubFInfo^TMGDBAPI(FNum,.SubInfo) DO
  . . . . . SET FileArray(i,"PARENT","LOC")=SubInfo("FIELD IN PARENT","LOC")
  . . . . . SET GREF=$GET(SubInfo("PARENT","GL")) ;"<-- only valid for highest ancestor
  . . . . ELSE  DO
  . . . . . SET (FileArray(0,"TOP GL"),FileArray(i,"PARENT","GL"))=$GET(^DIC(FNum,0,"GL"))
  . . . . SET FNum=$$IsSubFile^TMGDBAPI(FNum)
  . . . DO HandleMSubFile(.IENArray,.FileArray,.Array)
  . . ELSE  DO
  . . . SET GREF=$GET(^DIC(fromFile,0,"GL"))
  . . . NEW ORef SET ORef=GREF
  . . . SET GREF=$$CREF^DILF(GREF)  ;"convert open to closed format
  . . . NEW Itr,fromIEN
  . . . SET fromIEN=$$ItrAInit^TMGITR(GREF,.Itr)
  . . . IF SHOWPROG=1 DO PrepProgress^TMGITR(.Itr,20,1,"fromIEN")
  . . . IF fromIEN'="" FOR  DO  QUIT:($$ItrANext^TMGITR(.Itr,.fromIEN)="")!abort
  . . . . IF $$USRABORT^TMGUSRI2 SET abort=1 QUIT
  . . . . ;"FOR  SET fromIEN=$ORDER(@GREF@(fromIEN)) QUIT:(fromIEN="")  DO
  . . . . NEW valueS SET valueS=$GET(@GREF@(fromIEN,node))
  . . . . NEW ToIEN SET ToIEN=$PIECE(valueS,"^",pce)
  . . . . IF $DATA(IENArray(ToIEN))>0 DO
  . . . . . NEW lastCount SET lastCount=+$GET(Array(ToFile,ToIEN,fromFile,fromIEN,0))+1
  . . . . . SET Array(ToFile,ToIEN,fromFile,fromIEN,0)=lastCount
  . . . . . SET Array(ToFile,ToIEN,fromFile,fromIEN,lastCount)=$name(@GREF@(fromIEN,node))_";"_pce_";"_""_";"_ORef

SMFDone
  QUIT


HandleSubFile(SearchValue,FileArray,Array,IENS,Ref)
  ;"Purpose: To provide a means of recursively handling subfiles, searching for SearchValue.
  ;"Input:   SearchValue -- the value to be searched for, in INTERNAL format.
  ;"     File Array -- PASS BY REFERENCE  An array that describes the parent file numbers
  ;"             and storage locations. Example:
  ;"             FileArra(0,"TOP GL")="^XTV(8989.3,"
  ;"             FileArra(0,"FILE")=8989.33211
  ;"             FileArra(0)=0
  ;"             FileArra(0,"PARENT","LOC")="0;1" <-- for FileArray(0) node, stores node;piece
  ;"             FileArra(1)=8989.33211
  ;"             FileArra(1,"PARENT","LOC")="1;0"  <--- 1 is storage node
  ;"             FileArra(2)=8989.3321
  ;"             FileArra(2,"PARENT","LOC")="1;0" <--- 1 is storage node
  ;"             FileArra(3)=8989.332
  ;"             FileArra(3,"PARENT","LOC")="ABPKG;0" <--- "ABPKG" is storage node
  ;"             FileArra(4)=8989.3
  ;"             FileArra(4,"PARENT","GL")="^XTV(8989.3,"
  ;"     Array -- PASS BY REFERENCE.  An array the receives any search matches.
  ;"          Format is as follows
  ;"          Array(File#,IEN,0)=LastCount
  ;"          Array(File#,IEN,count)=FullRef;piece;IENS;TopGlobalRef
  ;"
  ;"      IENS -- OPTIONAL -- used by this function internally during recursive calls
  ;"      Ref -- OPTIONAL -- used by this function internally during recursive calls

  NEW index,s,IEN,CRef,pce,node
  SET index=$ORDER(FileArray(""),-1)
  SET s=$GET(FileArray(index,"PARENT","LOC"))
  SET node=$PIECE(s,";",1)
  SET pce=+$PIECE(s,";",2)
  IF s'="" DO
  . IF +node'=node SET node=""""_node_""""
  . SET s=node_","
  ELSE  DO
  . SET s=$GET(FileArray(index,"PARENT","GL"))
  . SET node=""
  SET Ref=$GET(Ref)_s
  IF Ref="" GOTO HSFDone
  SET CRef=$$CREF^DILF(Ref)
  NEW subFArray
  MERGE subFArray=FileArray
  KILL subFArray(index) ;"trim top entry from list/array
  IF index>0 DO
  . SET IEN=$ORDER(@CRef@(0))
  . IF +IEN>0 FOR  DO  QUIT:(+IEN=0)
  . . NEW subRef,subIENS
  . . SET subRef=Ref_IEN_","
  . . SET subIENS=IEN_","_$GET(IENS)
  . . DO HandleSubFile(SearchValue,.subFArray,.Array,.subIENS,subRef)
  . . SET IEN=$ORDER(@CRef@(IEN))
  ELSE  DO
  . IF (pce>0) DO  ;"Here is were the actual comparison to SearchValue occurs
  . . SET subRef=$$CREF^DILF(subRef)
  . . NEW p,t SET (p,t)=0
  . . FOR  SET t=$find(subRef,",",t) SET:(t>0) p=t QUIT:(t=0)  ;"find pos of last parameter
  . . ;"new ORef SET ORef=$EXTRACT(subRef,1,p-1)
  . . SET IEN=$PIECE($EXTRACT(subRef,p,99),")",1)
  . . NEW value SET value=$GET(@subRef@(node))
  . . SET value=$PIECE(value,"^",pce)
  . . SET value=$PIECE(value,";",1)  ;"I think VARIABLE pointers format is: IEN;file#
  . . IF value=SearchValue DO
  . . . NEW tFile SET tFile=$GET(FileArray(0,"FILE"),"?")
  . . . NEW count SET count=$GET(Array(tFile,IEN,0))+1
  . . . SET Array(tFile,IEN,0)=count
  . . . SET Array(tFile,IEN,count)=$name(@subRef@(node))_";"_pce_";"_""_$GET(IENS)_""_";"_$GET(FileArray(0,"TOP GL"))

HSFDone
  QUIT


HandleMSubFile(IENArray,FileArray,Array,IENS,Ref)
  ;"Purpose: To provide a means of recursively handling subfiles, searching for SearchValue.
  ;"Input:   IENArray : PASS BY REFERENCE.  IEN's to search for in INTERNAL format.
  ;"        Format: IENArray=SourceFile
  ;"          IENArray(IEN)=""
  ;"          IENArray(IEN)=""
  ;"   File Array -- PASS BY REFERENCE  An array that describes the parent file numbers
  ;"        and storage locations. Example:
  ;"        FileArray(0,"TOP GL")="^XTV(8989.3,"
  ;"        FileArray(0,"FILE")=8989.33211
  ;"        FileArray(0)=0
  ;"        FileArray(0,"PARENT","LOC")="0;1" <-- for FileArray(0) node, stores node;piece
  ;"        FileArray(1)=8989.33211
  ;"        FileArray(1,"PARENT","LOC")="1;0"  <--- 1 is storage node
  ;"        FileArray(2)=8989.3321
  ;"        FileArray(2,"PARENT","LOC")="1;0" <--- 1 is storage node
  ;"        FileArray(3)=8989.332
  ;"        FileArray(3,"PARENT","LOC")="ABPKG;0" <--- "ABPKG" is storage node
  ;"        FileArray(4)=8989.3
  ;"        FileArray(4,"PARENT","GL")="^XTV(8989.3,"
  ;"   Array : PASS BY REFERENCE.  AN OUT PARAMETER.  Format:
  ;"        Array(ToFile#,ToIEN,fromFile#,fromIEN,0)=LastCount
  ;"        Array(ToFile#,ToIEN,fromFile#,fromIEN,count)=FullRef;piece;IENS;TopGlobalRef
  ;"          Description of parts:
  ;"          ----------------------
  ;"          ToFile# -- the file containing the target IEN record
  ;"          ToIEN --the IEN in ToFile
  ;"          fromFile# -- the file the found entry exists it (may be a subfile number)
  ;"          fromIEN -- the record number in file
  ;"            Note: IEN here is different from the IEN passed in as a parameter
  ;"          FullRef -- the is the full reference to the found value.  e.g.
  ;"            SET value=$PIECE(@FullRef,"^",piece)
  ;"          piece -- the piece where value is stored in the node that is specified by FullRef
  ;"          IENS -- this is provided only for matches in subfiles.  It is the IENS that may be used in database calls
  ;"          TopGlobalRef -- this is the global reference for file.  If the match is in a subfile, then
  ;"              this is the global reference of the parent file (or the highest grandparent file if
  ;"              the parent file itself is a subfile, etc.)
  ;"
  ;"    IENS -- OPTIONAL -- used by this function internally during recursive calls
  ;"    Ref -- OPTIONAL -- used by this function internally during recursive calls

  NEW ToFile SET ToFile=$GET(IENArray)
  NEW index,s,IEN,CRef,pce,node
  SET index=$ORDER(FileArray(""),-1)
  SET s=$GET(FileArray(index,"PARENT","LOC"))
  SET node=$PIECE(s,";",1)
  SET pce=+$PIECE(s,";",2)
  IF s'="" DO
  . IF +node'=node SET node=""""_node_""""
  . SET s=node_","
  ELSE  DO
  . SET s=$GET(FileArray(index,"PARENT","GL"))
  . SET node=""
  SET Ref=$GET(Ref)_s
  IF Ref="" GOTO HSFDone
  SET CRef=$$CREF^DILF(Ref)
  NEW subFArray
  MERGE subFArray=FileArray
  KILL subFArray(index) ;"trim top entry from list/array
  IF index>0 DO
  . SET fromIEN=0
  . FOR  SET fromIEN=$ORDER(@CRef@(fromIEN)) QUIT:(+fromIEN=0)  DO
  . . NEW subRef,subIENS
  . . SET subRef=Ref_fromIEN_","
  . . SET subIENS=fromIEN_","_$GET(IENS)
  . . DO HandleMSubFile(.IENArray,.subFArray,.Array,.subIENS,subRef)
  ELSE  DO
  . IF (pce>0) DO  ;"Here is were the actual comparison to SearchValue occurs
  . . SET subRef=$$CREF^DILF(subRef)
  . . NEW p,t SET (p,t)=0
  . . FOR  SET t=$find(subRef,",",t) SET:(t>0) p=t QUIT:(t=0)  ;"find pos of last parameter
  . . ;"new ORef SET ORef=$EXTRACT(subRef,1,p-1)
  . . SET fromIEN=$PIECE($EXTRACT(subRef,p,99),")",1)
  . . NEW valueS SET valueS=$GET(@subRef@(node))
  . . SET valueS=$PIECE(valueS,"^",pce)
  . . NEW ToIEN SET ToIEN=$PIECE(valueS,";",1)  ;"I think VARIABLE pointers format is: IEN;file#
  . . IF $DATA(IENArray(ToIEN))>0 DO
  . . . NEW fromFile SET fromFile=$GET(FileArray(0,"FILE"),"?")
  . . . NEW count SET count=$GET(Array(ToFile,ToIEN,fromFile,fromIEN,0))+1
  . . . SET Array(ToFile,ToIEN,fromFile,fromIEN,0)=count
  . . . SET Array(ToFile,ToIEN,fromFile,fromIEN,count)=$name(@subRef@(node))_";"_pce_";"_""_$GET(IENS)_""_";"_$GET(FileArray(0,"TOP GL"))

HMSFDone
  QUIT


PossPtrs(File,Array)
  ;"SCOPE: PUBLIC
  ;"Purpose: to create a list of all possible pointers to a specified file, i.e. all other fields/fields
  ;"        that point to the specified file.
  ;"Input: File:    The file to investigate (Number or Name)
  ;"   Array -- PASS BY REFERENCE.  An array to receive results back.
  ;"        any prexisting data in Array is killed before filling
  ;"Output:  Array is filled with format as follows:
  ;"      Array(1)=OtherFile#^Field#^FieldCode(piece#2 of 0 node of ^DD entry for field)
  ;"      Array(2)=OtherFile#^Field#^FieldCode
  ;"Result: 1 IF results found, 0 IF error occurred.
                                                         
  KILL Array
  NEW RESULT SET RESULT=0
  NEW FileNum
  IF $DATA(File)#10=0 GOTO PPtrsDone
  IF +File=0 SET FileNum=$$GETFNUM^TMGDBAP3(File)   ;"Convert File Name to File Number
  ELSE  SET FileNum=File
  IF +FileNum=0 GOTO PPtrsDone

  NEW count SET count=1
  NEW PtrFile SET PtrFile=""
  FOR  SET PtrFile=$ORDER(^DD(FileNum,0,"PT",PtrFile)) QUIT:+PtrFile'>0  DO
  . NEW PtrField SET PtrField=""
  . FOR  SET PtrField=$ORDER(^DD(FileNum,0,"PT",PtrFile,PtrField)) QUIT:+PtrField'>0  DO
  . . NEW s SET s=PtrFile_"^"_PtrField
  . . SET s=s_"^"_$PIECE($GET(^DD(PtrFile,PtrField,0)),"^",2)
  . . SET Array(count)=s,count=count+1
  ;"NEW count SET count=1
  ;"NEW PtrFile SET PtrFile=$ORDER(^DD(FileNum,0,"PT",""))
  ;"IF PtrFile'="" FOR  DO  QUIT:(PtrFile="")
  ;". NEW PtrField SET PtrField=$ORDER(^DD(FileNum,0,"PT",PtrFile,""))
  ;". IF PtrField'="" FOR  DO  QUIT:(PtrField="")
  ;". . NEW s SET s=PtrFile_"^"_PtrField
  ;". . SET s=s_"^"_$PIECE($GET(^DD(PtrFile,PtrField,0)),"^",2)
  ;". . SET Array(count)=s
  ;". . SET count=count+1
  ;". . SET PtrField=$ORDER(^DD(FileNum,0,"PT",PtrFile,PtrField))
  ;". SET PtrFile=$ORDER(^DD(FileNum,0,"PT",PtrFile))
  SET RESULT=1
PPtrsDone
  QUIT RESULT


  ;"Note: Not fully debugged yet..."
SAFEKILL(Array,SHOWPROG)
  ;"Purpose: to safely KILL records, including removing any pointers TO them
  ;"input: pArray -- PASS BY REFERENCE.  Expected input Format:
  ;"        Array(File,IEN)=0
  ;"        Array(File,IEN)=0
  ;"      SHOWPROG: IF 1, progress bar shown
  ;"Output: all pointers in linked files to OldIEN will be changed to newIEN
  ;"Results: none

  DO QTMMVPTR(.Array,.SHOWPROG)
  QUIT


ASKKILL
  ;"Purpose: to interact with user and safely KILL records
  ;"Input: none.
  ;"Output: Records and pointers may be deleted
  ;"Results: none

  NEW DIC,File,X,Y
  NEW fromIEN,toIEN
  NEW delArray

  KILL DIC
  SET DIC("A")="Select file to delete from: "
  SET DIC="^DIC("
  SET DIC(0)="MAQE"
  d ^DIC  ;"Get File to search
  SET File=+Y
  IF File'>0 GOTO ASKKDone

  NEW Menu,UsrSlct
  SET Menu(0)="Pick Option for Selecting Record(s) to Safely Delete"
  SET Menu(1)="Manually pick Record(s)"_$CHAR(9)_"ManualPick"
  SET Menu(2)="Select a SET (aka SORT TEMPLATE) Contianing Many Records"_$CHAR(9)_"PickSet"

M1      WRITE #
  SET UsrSlct=$$MENU^TMGUSRI2(.Menu,"^")

  IF UsrSlct="ManualPick" GOTO ManualPick
  IF UsrSlct="PickSet" GOTO PickSet
  IF UsrSlct="^" GOTO ASKKDone
  IF UsrSlct=0 SET UsrSlct=""
  GOTO M1

ManualPick
  SET DIC=File
  SET DIC("A")="Select record to delete: "
  DO ^DIC  ;"get FROM record in File
  WRITE !
  SET fromIEN=+Y
  IF fromIEN'>0 GOTO ASKGo
  SET delArray(File,fromIEN)=0
  NEW % SET %=2
  WRITE "Pick another record" DO YN^DICN WRITE !
  IF %=1 GOTO ManualPick
  IF %=-1 GOTO ASKKDone
  GOTO ASKGo

PickSet NEW IENArray
  IF $$GETTPREC^TMGXMLUI(File,"IENArray","",1)=0 GOTO ASKKDone
  ;"Output: Data is put into pRecs like this: @pRecs@(IEN)=""

  NEW IEN SET IEN=""
  FOR  SET IEN=$ORDER(IENArray(IEN)) QUIT:(IEN="")  DO
  . SET delArray(File,IEN)=0

ASKGo
  IF $DATA(delArray)=0 GOTO ASKKDone

  ;"Get list of files/fields with pointers in
  SET RESULT=$$PossPtrs(File,.PossPtrs) IF RESULT=0 GOTO ASKKDone
  IF $DATA(PossPtrs)'>0 GOTO DelRecs

  DO SAFEKILL(.delArray,1)

DelRecs  ;"Now that pointers to records are deleted, it is safe to remove records themselves

  SET IEN=""
  NEW abort SET abort=0
  FOR  SET IEN=$ORDER(IENArray(IEN)) QUIT:(IEN="")!(abort=1)  DO
  . IF $$USRABORT^TMGUSRI2 SET abort=1 QUIT
  . NEW TMGFDA,TMGMSG
  . SET TMGFDA(File,IEN_",",.01)="@"
  . DO FILE^DIE("EK","TMGFDA","TMGMSG")
  . DO SHOWDIER^TMGDEBU2(.TMGMSG)

ASKKDone
  QUIT



VerifyPtrs(File,pArray,Verbose,AutoFix)
  ;"Purpose: to scan a file for pointers OUT that are bad/invalid
  ;"Input: File : file Name or Number to scan
  ;"       pArray : PASS BY NAME, an OUT PARAMETER.  Format:
  ;"    @pArray@(FileNum,IEN,FieldNum)=ValueOfBadPtr
  ;"    @pArray@(FileNum,IEN,FieldNum)=ValueOfBadPtr
  ;"       Verbose: OPTIONAL.  If 1, then errors immediately written out.
  ;"       AutoFix: OPTIONAL.  If 1, then bad pointers are deleted.
  ;"Results: None

  NEW PtrsOUT
  NEW pPtrsOUT SET pPtrsOUT="PtrsOUT"
  NEW fileNum
  IF +File=File SET fileNum=+File
  ELSE  SET fileNum=$$GETFNUM^TMGDBAP3(File)
  SET Verbose=+$GET(Verbose)
  SET AutoFix=+$GET(AutoFix)

  IF $$FilePtrs(fileNum,pPtrsOUT)=0 GOTO VPtrDone

  NEW Itr,Itr2,TMGIEN,fieldNum
  NEW TMGVALUE,code
  NEW abort SET abort=0
  NEW $ETRAP SET $ETRAP="set Y=""(Invalid M code!.  Error Trapped.)"" SET $ETRAP="""",$ecode="""""

  DO DoVerify(File,pArray,Verbose,AutoFix)  ;" Split out code to call it to call itself reentrantly

VPtrDone
  QUIT


DoVerify(fileNum,pArray,Verbose,AutoFix,IENS,pTMGIEN)
  ;"Purpose: Function allow VerifyPtrs to call reentrantly
  ;"Input: File : file Name or Number to scan
  ;"       pArray : PASS BY NAME, an OUT PARAMETER.  Format:
  ;"    @pArray@(FileNum,IEN,FieldNum)=ValueOfBadPtr
  ;"    @pArray@(FileNum,IEN,FieldNum)=ValueOfBadPtr
  ;"       Verbose: OPTIONAL.  If 1, then errors immediately written out.
  ;"       AutoFix: OPTIONAL.  If 1, then bad pointers are deleted.
  ;"       IENS: OPTIONAL.  If fileNum is a sub-file, then must supply
  ;"        to give location of subfile in parent file.
  ;"       pTMGIEN: "TMGIEN", or "TMGIEN(1)" etc.
  ;"Results: None
  ;"NOTICE: right now this MUST first be called from VerifyPtrs because
  ;"  I have not moved some NEW commandes etc from there to here.
  ;"  So this function depends on it's variables with global scope.

  SET IENS=$GET(IENS)
  SET pTMGIEN=$GET(pTMGIEN,"TMGIEN")
  SET @pTMGIEN=$$ItrInit^TMGITR(fileNum,.Itr,.IENS)
  IF IENS="" DO PrepProgress^TMGITR(.Itr,20,0,pTMGIEN)  ;" no bar for subfiles
  IF @pTMGIEN'="" FOR  DO  QUIT:(+$$ItrNext^TMGITR(.Itr,.@pTMGIEN)'>0)!abort
  . SET fieldNum=$$ItrAInit^TMGITR($name(@pPtrsOUT@(fileNum)),.Itr2)
  . IF fieldNum'="" FOR  DO  QUIT:(+$$ItrANext^TMGITR(.Itr2,.fieldNum)'>0)!abort
  . . IF (@pTMGIEN#10=0),$$USRABORT^TMGUSRI2 SET abort=1 QUIT
  . . ;"Line below handles subfiles
  . . IF $DATA(@pPtrsOUT@(fileNum,fieldNum,"SUBFILE")) DO  QUIT
  . . . NEW subFile SET subFile=$ORDER(@pPtrsOUT@(fileNum,fieldNum,"SUBFILE",""))
  . . . SET IENS=IENS_@pTMGIEN_","
  . . . DO DoVerify(subFile,$name(@pArray@("SUBFILE")),.Verbose,.AutoFix,IENS,$name(@pTMGIEN@(1)))
  . . ;"Otherwise, the usual case....
  . . SET code=$GET(PtrsOUT(fileNum,fieldNum,"X GET"))
  . . IF code="" QUIT
  . . xecute code
  . . IF TMGVALUE="" QUIT
  . . SET TMGVALUE=+TMGVALUE
  . . IF TMGVALUE'>0 DO  QUIT
  . . . SET @pArray@(fileNum,@pTMGIEN,fieldNum)=TMGVALUE
  . . . NEW setCode SET setCode=$GET(PtrsOUT(fileNum,fieldNum,"X SET")) QUIT:(setCode="")
  . . . NEW priorValue SET priorValue=TMGVALUE
  . . . SET TMGVALUE=""
  . . . IF 'AutoFix QUIT
  . . . xecute setCode
  . . . IF 'Verbose QUIT
  . . . WRITE !,"File=",fileNum,"; IEN=",@pTMGIEN,"; Field=",fieldNum,"; Bad Pointer value=[",priorValue,"]",!
  . . . WRITE "    fixed...",!
  . . ;"if (fileNum=2)&(TMGVALUE=777) DO  QUIT   ;"TEMP!!!!
  . . ;". SET code=$GET(PtrsOUT(fileNum,fieldNum,"X SET")) QUIT:(code="")
  . . ;". SET TMGVALUE=69
  . . ;". xecute code
  . . NEW PtToGREF SET PtToGREF="^"_$GET(PtrsOUT(fileNum,fieldNum,"POINTS TO","GREF"))
  . . IF PtToGREF="" DO  QUIT
  . . . SET @pArray@(fileNum,@pTMGIEN,fieldNum)="??No reference for pointed to file??"
  . . . IF 'Verbose QUIT
  . . . WRITE !,"File=",fileNum,"; IEN=",@pTMGIEN,"; Field=",fieldNum,"; Pointer value=[",TMGVALUE,"] but 'No reference for pointed to file (??)'",!
  . . SET PtToGREF=PtToGREF_TMGVALUE_")"
  . . IF $DATA(@PtToGREF)'>0 DO  QUIT
  . . . SET @pArray@(fileNum,@pTMGIEN,fieldNum)=TMGVALUE
  . . . NEW setCode SET setCode=$GET(PtrsOUT(fileNum,fieldNum,"X SET")) QUIT:(setCode="")
  . . . NEW priorValue SET priorValue=TMGVALUE
  . . . SET TMGVALUE=""
  . . . IF 'AutoFix QUIT
  . . . xecute setCode
  . . . IF 'Verbose QUIT
  . . . WRITE !,"File=",fileNum,"; IEN=",@pTMGIEN,"; Field=",fieldNum,"; Bad Pointer value=[",priorValue,"]",!
  . . . WRITE "    fixed...",!
  IF IENS="" DO ProgressDone^TMGITR(.Itr)
  QUIT


ASKVFYPT   ;"ASK VERIFY POINTERS
  ;"Ask user to pick file, then verify pointers for that file.
  ;
  WRITE "NOTICE: this function caused corruption of the database from",!
  WRITE "  deletion of pointers incorrectly.  Until this function",!
  WRITE "  (ASKVFYPT^TMGFMUT) is fixed, it may not be used.",!,!
  DO PRESS2GO^TMGUSRI2
  GOTO ASKDone
  NEW DIC,X,Y
  NEW FileNum,IEN
  NEW UseDefault SET UseDefault=1
  ;
  ;"Pick file to dump from
ASK1 ;
  SET DIC=1
  SET DIC(0)="AEQM"
  SET DIC("A")="SELECT FILE TO VERIFY POINTERS IN: "
  IF UseDefault DO   ;"leave the redundant DO loop, it protects $T, so second DO ^DIC isn't called
  . DO ^DICRW  ;" has default value of user's last response
  ELSE  DO ^DIC  ;doesn't have default value...
  WRITE !
  IF +Y'>0 WRITE ! GOTO ASKDone
  SET FileNum=+Y

  NEW BadPtrs
  NEW AutoFix,Verbose,%
  SET %=2
  WRITE "View details of scan" DO YN^DICN WRITE !
  IF %=-1 GOTO ASKDone
  SET Verbose=(%=1)

  SET %=2
  WRITE "Auto-delete bad pointers (i.e. 0 value, or pointers to empty records)"
  DO YN^DICN WRITE !
  IF %=-1 GOTO ASKDone
  SET AutoFix=(%=1)

  DO VerifyPtrs(FileNum,"BadPtrs",Verbose,AutoFix)

  IF $DATA(BadPtrs) DO
  . NEW % SET %=2
  . WRITE "View array of bad pointers" DO YN^DICN WRITE !
  . IF %'=1 QUIT
  . DO ZWRITE^TMGZWR("BadPtrs")
  ELSE  WRITE "No bad pointers.  Great!",!

  DO PRESS2GO^TMGUSRI2

ASKDone
  QUIT

GREP(FIELD,S)
  ;"The is a stub function, called by a Fileman Function (entry in file .5)
  NEW RESULT
  SET RESULT="X1="_$GET(FIELD)_" X2="_$GET(S)_" D0="_$GET(D0)_" DCC="_$GET(DCC)
  MERGE ^TMG("TMP","KILL","DIQGEY")=DIQGEY
  SET ^TMG("TMP","KILL","DA")=$GET(DA)
  SET ^TMG("TMP","KILL","DR")=$GET(DR)
  SET ^TMG("TMP","KILL","D0")=$GET(D0)
  SET ^TMG("TMP","KILL","DCC")=$GET(DCC)
  QUIT RESULT

GETAPPT(TMGIEN)
  QUIT 0

FMDate(DateStr)
  ;"Purpose: convert string to FM date, with extended syntax handling
  ;"Results: returns FM date, or -1 IF error
  NEW RESULT SET RESULT=-1
  ;"First try direct conversion
  NEW X,Y
  SET DateStr=$$TRIM^XLFSTR($GET(DateStr))
  IF DateStr="" GOTO FMDDone
  SET DateStr=$$UP^XLFSTR(DateStr)
  IF DateStr[" AT " SET DateStr=$$REPLSTR^TMGSTUT3(DateStr," AT ","@")
  IF DateStr["AM" SET DateStr=$$REPLSTR^TMGSTUT3(DateStr,"AM","")
  FOR  QUIT:(DateStr'["  ")  SET DateStr=$$REPLSTR^TMGSTUT3(DateStr,"  "," ")
  IF (DateStr'["@")&($LENGTH(DateStr," ")>3) DO
  . SET DateStr=$PIECE(DateStr," ",1,3)_"@"_$PIECE(DateStr," ",4,99)
  FOR  QUIT:(DateStr'["@ ")  SET DateStr=$$REPLSTR^TMGSTUT3(DateStr,"@ ","@")
  FOR  QUIT:(DateStr'[" @")  SET DateStr=$$REPLSTR^TMGSTUT3(DateStr," @","@")
  SET %DT="T",X=DateStr
  DO ^%DT
  IF Y>0 goto FMD2
  ;"If string has extra stuff, try seeing IF any one word is a date
  NEW l,idx SET l=$length(DateStr," ") IF l>1 for idx=1:1:l QUIT:Y>0  DO
  . SET X=$piece(DateStr," ",idx) 
  . DO ^%DT
FMD2    SET RESULT=Y
  
FMDDone QUIT RESULT
  ;
SETWP(GREF,ARRAY) ;
  ;"Purpose: To directly SET a WP (word processor) field from array
  ;"NOTE: It is better to use FILE^DIE or UPDATE^DIE whenever possible.
  ;"      This function DOES NOT TRIGGER ANY XREFS!
  ;"      Any prior entry in WP field is killed.
  ;"      This function DOES NOT check that GREF is proper.  It stores ARRAY
  ;"        at GREF regardless.  //old--> As long as *anything* is already there.
  ;"Input: GREF -- The reference to the header node, Open or closed format
  ;"         (e.g.  ^TMG(22702,99,1) in example below)
  ;"       ARRAY -- Holds WP information.  PASS BY REFERENCE.  Format as folows:
  ;"        ARRAY(n)=first line of array
  ;"        ARRAY(n+1)=second line of array
  ;"        ARRAY(n+2)=third line... etc.
  ;"Note:  The format of a WP field is as follows:
  ;"       e.g.    ^TMG(22702,99,1,0) = ^^4^4^3050118^
  ;"         ^TMG(22702,99,1,1,0) = Here is the first line of text
  ;"         ^TMG(22702,99,1,2,0) = And here is another line
  ;"         ^TMG(22702,99,1,3,0) =
  ;"         ^TMG(22702,99,1,4,0) = And here is a final line
  ;"  And the format of the 0 node is: ^^<line count>^<linecount>^<fmdate>^^
  ;"Result: -1^Message IF failure, 1 IF success
  ;"Assumptions: That GlobalP is a valid reference to a WP field
  ;
  NEW RESULT SET RESULT=1 ;"default to success
  IF $GET(GREF)="" DO  GOTO SETWDN
  . SET RESULT="-1^GREF not provided"
  SET GREF=$$CREF^DILF(GREF)
  ;"IF $DATA(GREF)=0 DO  GOTO SETWDN
  ;". SET RESULT="-1^GREF doesn't seem to point to existing WP field
  NEW WP
  NEW J SET J=0
  NEW I SET I=""
  FOR  SET I=$ORDER(ARRAY(I)) QUIT:(I="")  DO
  . SET J=J+1
  . SET WP(J,0)=ARRAY(I)
  ;"Now create a header node
  DO NOW^%DTC  ;"returns result in X
  SET WP(0)="^^"_J_"^"_J_"^"_X_"^^"
  ;"Now put WP into global reference.
  KILL @GREF
  MERGE @GREF=WP
SETWDN  QUIT RESULT
  ;
RUNOPT(TMGOPT,TMGDUZ) ;
  ;"Purpose: API to Launch a menuman option
  ;"Input: TMGIPT -- NAME of the option to run
  ;"      Or may be the IEN of record in OPTION file (#19) to launch
  ;"       DUZ -- Optional.  DUZ to run as.  Default is current DUZ.
  ;"NOTE: DUZ must be defined, meaning a log in should have occured PRIOR to call.
  ;"NOTE: If function fails due to invalid access error, then try rebuilding
  ;"      primary menu trees with option XQBUILDTREE
  ;"Results 1 if OK, or -1^Message IF error.
  SET TMGRESULT=1
  SET TMGOPT=$GET(TMGOPT)
  IF TMGOPT'=+TMGOPT DO
  . NEW DIC SET DIC=19,DIC(0)="M"
  . NEW X,Y SET X=TMGOPT
  . DO ^DIC
  . SET TMGOPT=Y
  NEW IEN19 SET IEN19=+TMGOPT
  IF (IEN19'>0)!($DATA(^DIC(19,IEN19,0))=0) DO  GOTO RNODN
  . SET TMGRESULT="-1^Valid Name or IEN for OPTION (#19) file not provided"
  NEW TMGZZDUZ
  IF $DATA(TMGDUZ),(+$G(TMGDUZ)>0) DO
  . IF $DATA(^VA(200,TMGDUZ,0))=0 DO  QUIT
  . . SET TMGRESULT="-1^DUZ provided is invalid.  No such user in the NEW PERSON file (#200)"
  . MERGE TMGZZDUZ=TMGDUZ
  . KILL DUZ
  . SET DUZ=TMGDUZ
  . DO DUZ^XUP(DUZ) ;"build DUZ array for specified user.
  IF TMGRESULT<0 GOTO RNODN
  ;"Ensure TMGDUZ allowed run menu.
  IF $GET(DUZ(0))="@" GOTO RNO5
  NEW TMGZZSEC SET TMGZZSEC=$$ACCESS^XQCHK(DUZ,IEN19)
  IF +TMGZZSEC'>0 DO  GOTO RNODN
  . IF +TMGZZSEC=0 DO  QUIT
  . . SET TMGRESULT="-1^No access found in any menu tree the user owns."
  . IF +TMGZZSEC=-1 DO  QUIT
  . . SET TMGRESULT="-1^DUZ provided is invalid.  No such user in the NEW PERSON file (#200)"
  . IF +TMGZZSEC=-2 DO  QUIT
  . . SET TMGRESULT="-1^User terminated or has no Access code"
  . IF +TMGZZSEC=-3 DO  QUIT
  . . SET TMGRESULT="-1^No such option found in the Option file (#19)"
RNO5    NEW XQXFLG SET XQXFLG="^^XUP"
  NEW XQY SET XQY=IEN19
  DO ^XQ
  ;"Restore original DUZ
  IF $DATA(TMGZZDUZ) DO
  . KILL DUZ
  . MERGE DUZ=TMGZZDUZ
RNODN   QUIT TMGRESULT
  ;
testRN  ;
  NEW DIC SET DIC=19,DIC(0)="MAEQ"
  DO ^DIC WRITE !
  IF +Y'>1 QUIT
  W $$RUNOPT(+Y)
  GOTO testRN
  ;
ASKGL2F ;
  ;"Purpose: ask user for global name and show all files using that global
  NEW REF,ARRAY
  WRITE !,"List Fileman File Names Using Global",!
AGL2F2  READ "ENTER GLOBAL NAME (e.g. '^TIU', or '^TIU(8925') (^ to QUIT): ",REF:$GET(DTIME,3600),!
  IF REF="" SET REF="^"
  IF REF="^" GOTO AG2FDN
  DO GL2FIL(REF,.ARRAY)
  IF $DATA(ARRAY) DO
  . DO ZWRITE^TMGZWR("ARRAY")
  ELSE  DO
  . WRITE "Sorry no Fileman files are stored in '",REF,"'",!
  DO PRESS2GO^TMGUSRI2
  GOTO AGL2F2
AG2FDN  QUIT
  ;
GL2FIL(REF,ARRAY) ;
  ;"Purpose: To return all files that uses the specified global
  ;"Input: Reference -- the name of the global, partial name or open format
  ;"          e.g. ^TIU(8925,  or ^TIU(  or ^TIU
  ;"    Note that the test will be IF Global_Name[REF
  ;"ARRAY: OUT parameter.  PASS BY REFERENCE
  ;"Output: Returns array filled with file numbers and globals.  E.g.
  ;"        ARRAY(FILENAME)="8925;^TIU(8925,"
  ;"        ARRAY(FILENAME)="8925.1;^TIU(8925,1"
  ;"Results: none
  ;"NOTE: does not return sub-file numbers
  SET REF=$GET(REF)
  KILL ARRAY
  NEW FILE SET FILE=0
  FOR  SET FILE=$ORDER(^DIC(FILE)) QUIT:(+FILE'>0)  DO
  . NEW GL SET GL=$GET(^DIC(FILE,0,"GL")) QUIT:(GL="")
  . IF GL'[REF QUIT
  . NEW NAME SET NAME=$PIECE($GET(^DIC(FILE,0)),"^",1)
  . SET ARRAY(NAME)=FILE_";"_GL
  QUIT

