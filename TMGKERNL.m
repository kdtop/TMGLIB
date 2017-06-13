TMGKERNL ;TMG/kst/OS Specific functions ;6/10/14, 3/6/15, 2/3/17
         ;;1.0;TMG-LIB;**1**;04/24/09
 ;
 ;"TMG KERNEL FUNCTIONS
 ;"I.e. functions that are OS specific.
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
 ;"$$Dos2Unix^TMGKERNL(FullNamePath)
 ;"$$Unix2Dos(FullNamePath)
 ;"$$ENSURDIR(DIR) -- ensure directory path exists  
 ;"$$EnsureDir(Dir) -- ensure directory path exists
 ;"$$MOVE^TMGKERNL(Source,Dest)
 ;"$$Copy^TMGKERNL(Source,Dest)
 ;"$$ISFILE(FPNAME)  ;-- FileExists() type file.  See also $$FILEXIST^TMGIOUTL(FilePathName)
 ;"$$ISDIR^TMGKERNL(Path)
 ;"$$CMDOK(CMDNAME) ;Check specified Linux command is available.
 ;"$$MKDIR(Dir) -- provide a shell for the Linux command 'mkdir'
 ;"$$RMDIR(Dir) -- provide a shell for the Linux command 'rmdir'
 ;"$$WGET(URL,OPTIONS,DIR) Provide a shell for the linux command 'wget'
 ;"$$Convert^TMGKERNL(FPathName,NewType) -- convert a graphic image to new type
 ;"$$XLTLANG(Phrase,langPair) -- execute a linux OS call to convert a phrase into another spoken language
 ;"(DEPRECIATED) $$GetPckList(PCKINIT,Array,NeedsRefresh,PckDirFNAME) -- launch special linux script to get patch file list from ftp.va.gov
 ;"$$DownloadFile^TMGKERNL(URL,DestDir) -- Interact with Linux to download a file with wget
 ;"$$EditArray^TMGKERNL(ARRAY,Editor) -- interact with Linux to edit array as file on the host file system
 ;"$$EditHFSFile^TMGKERNL(FilePathName,Editor) -- interact with Linux to edit a file on the host file system
 ;"$$EditArray^TMGKERNL(ARRAY,Editor) -- interact with Linux to edit array as file on the host file system
 ;"ZSAVE -- to save routine out to HFS
 ;"MAKEBAKF^TMGKERNL(FilePathName,NodeDiv)  ;Make Backup File if original exists
 ;"IOCapON -- redirect IO to a HFS file, so that it can be captured.
 ;"IOCapOFF(pOutArray) -- restore IO channel to that prior IOCapON was called, and return captured output in OutArray
 ;"KillPID(JobNum) -- send message to MUPIP to KILL Job
 ;"MJOBS(array) -- execute a linux OS call to get list of all 'mumps' jobs using: 'ps -C mumps'
 ;"$$GETSCRSZ(ROWS,COLS) --query the OS and get the dimensions of the terminal window.
 ;"KILLVARS(Mask) -- Selectively KILL variables from symbol table.
 ;"=======================================================================
 ;"Dependancies  TMGIOUTL
 ;"=======================================================================
 ;"=======================================================================
 ;
Dos2Unix(FullNamePath)  ;
  ;"Purpose: To execute the unix command Dos2Unix on filename path
  ;"FullNamePath: The filename to act on.
  ;"Result: 0 IF no error; >0 IF error
  ;"Notice!!!! The return code here is DIFFERENT from usual
  NEW RESULT SET RESULT=0
  IF $GET(FullNamePath)="" GOTO UDDone
  NEW spec SET spec(" ")="\ "
  SET FullNamePath=$$REPLACE^XLFSTR(FullNamePath,.spec)
  ;"new HOOKCMD SET HOOKCMD="dos2unix -q "_FullNamePath
  NEW HOOKCMD SET HOOKCMD="fromdos "_FullNamePath
  ZSYSTEM HOOKCMD
  SET RESULT=$ZSYSTEM&255  ;"get result of execution. (low byte only)
UDDone ;
  QUIT RESULT
  ;
Unix2Dos(FullNamePath)  ;
  ;"Purpose: To execute the unix command todos on filename path
  ;"FullNamePath: The filename to act on.
  ;"Result: 0 IF no error; >0 IF error
  ;"Notice!!!! The return code here is DIFFERENT from usual
  NEW RESULT SET RESULT=0
  IF $GET(FullNamePath)="" GOTO DUDone
  NEW spec SET spec(" ")="\ "
  SET FullNamePath=$$REPLACE^XLFSTR(FullNamePath,.spec)
  NEW HOOKCMD SET HOOKCMD="todos "_FullNamePath
  ZSYSTEM HOOKCMD
  SET RESULT=$ZSYSTEM&255  ;"get result of execution. (low byte only)
DUDone  ;
  QUIT RESULT
  ;
FileSize(FullNamePath)  ;
  ;"Purpose: To return the size of the file, in bytes.
  ;"Input:  FullNamePath: The filename to act on.
  ;"Result:  -1 if error, or returns size in bytes
  NEW RESULT SET RESULT=-1
  NEW p SET p="myTerm"
  OPEN p:(COMMAND="stat --format=%s "_FullNamePath:readonly)::"pipe"
  USE p
  NEW x READ x
  CLOSE p USE $P
  ;"WRITE "reply was :",x,!
  IF x'["cannot stat" SET RESULT=+x
  QUIT RESULT
  ;
FSTAT(OUT,FPNAME,PARAMS)  ; 
  ;"Purpose: Provide generic access to linux stat command.
  ;"Input:  OUT: Returns result from reading command output.  Format
  ;"          OUT(#)=<lines of text output from command"
  ;"        FPNAME: The path+filename to act on.
  ;"        PARAMS: (optional)  Contains command options, all in one long string.
  ;"           e.g. "--terse"   or "--printf=%n"
  ;"Result: none
  NEW CMD SET CMD="stat "_$GET(PARAMS)_" """_FPNAME_""""
  NEW P SET P="TEMP" OPEN P:(COMMAND=CMD:readonly)::"pipe" USE P
  KILL OUT NEW X,IDX SET IDX=1
  FOR   QUIT:$ZEOF  DO
  . READ X IF X=""&$ZEOF QUIT  
  . SET OUT(IDX)=X,IDX=IDX+1
  CLOSE P USE $P
  QUIT
  ;
CMDOK(CMDNAME) ;Check if specified Linux command is available.
  ;"Result: "1^OK" or "-1^Error Message"
  NEW OUT,TEMP
  SET TEMP=$$LINUXCMD("which "_CMDNAME_" 2>&1",.OUT)
  NEW TMGRESULT SET TMGRESULT="1^OK"
  FOR IDX=1:1 QUIT:'$DATA(OUT(IDX))!(+TMGRESULT'>0)  DO
  . IF $GET(OUT(IDX))'["no "_CMDNAME_" in" QUIT
  . SET TMGRESULT="-1^Linux command '"_CMDNAME_"' not found. ?Installed?"
  QUIT TMGRESULT        
  ;
LINUXCMD(CMD,OUT)  ;"Execute command on linux system, and return output
  ;"Input: CMD -- generic linux command to execute.
  ;"Result: "1^OK" or "-1^Error Message"
  NEW P SET P="TEMP" OPEN P:(COMMAND=CMD:readonly)::"pipe" USE P
  NEW X,IDX SET IDX=1
  FOR   QUIT:$ZEOF  DO
  . READ X IF X=""&$ZEOF QUIT
  . SET OUT(IDX)=X,IDX=IDX+1
  CLOSE P USE $P
  NEW TMGRESULT SET TMGRESULT="1^OK"
  ;"RESULT: 0 if no error; >0 if error
  NEW TEMP SET TEMP=$ZSYSTEM&255  ;"get result of execution. (low byte only)
  IF TEMP>0 SET TMGRESULT="-1^Linux error code returned: "_TEMP  
  QUIT TMGRESULT        
  ;  
ISFILE(FPNAME)  ;"Does file exist?  See also $$FILEXIST^TMGIOUTL(FilePathName)
  ;"Result: 1 if file exists, 0 if doesn't exist.  
  NEW TEMP DO FSTAT(.TEMP,FPNAME)
  NEW RESULT SET RESULT=($GET(TEMP(1))'["cannot stat")
  QUIT RESULT
  ;
ISDIR(Path,NodeDiv)  ;
  ;"Purpose: To determine if Path is a path to a directory
  ;"Input:  Path to test, e.g. "/home/user" or "/home/user/"
  ;"        NodeDiv: [OPTIONAL] -- the character that separates folders (e.g. "/")
  ;"                IF not supplied, then default value is "/"
  ;"Result:  1 filepath is actually a directory, 0 IF not
  ;"Note: NEW!  Will now return 1 if Path is a valid path to a directory, but there are no files in directory
  SET Path=$GET(Path)
  SET NodeDiv=$GET(NodeDiv,"/")
  IF $EXTRACT(Path,$LENGTH(Path))'=NodeDiv SET Path=Path_NodeDiv
  NEW p SET p="myTerm"
  OPEN p:(COMMAND="stat --format=%F "_Path:readonly)::"pipe"
  USE p
  NEW x READ x
  CLOSE p USE $P
  QUIT (x="directory")
  ;
ENSURDIR(DIR) ;
  QUIT $$EnsureDir(.DIR)
  ;
EnsureDir(Dir)  ;
  ;"Purpose: Ensure directory path exists
  ;"Input: Dir -- the directory to create
  ;"Result: 1 if OK, or -1^Message IF problem.
  NEW RESULT SET RESULT=1
  IF $$ISDIR(Dir) goto EnsDn        
  NEW i,tempDir
  for i=1:1:$length(Dir,"/") QUIT:(+RESULT=-1)  do
  . SET tempDir=$PIECE(Dir,"/",1,i)
  . IF $$ISDIR(tempDir) QUIT
  . IF $$MKDIR(tempDir)'=0 DO  QUIT  ;"0=no error
  . . SET RESULT="-1^Unable to create directory: "_tempDir
  . IF $$ISDIR(tempDir)'=1 DO  QUIT  ;"verify that created directory does exist.
  . . SET RESULT="-1^Failed to create directory: "_tempDir
EnsDn ;
  QUIT RESULT
  ;
MOVE(Source,Dest)  ;
  ;"Purpose to provide a shell for the Linux command 'mv'
  ;"      This can serve to move or rename a file
  ;"Note: a platform independant version of the this could be constructed later...
  ;"Result: 0 IF no error; >0 IF error
  ;"Notice!!!! The return code here is DIFFERENT from usual
  NEW HOOKCMD,RESULT
  NEW Srch
  SET Srch(" ")="\ "
  SET Source=$$REPLACE^XLFSTR(Source,.Srch)
  SET Dest=$$REPLACE^XLFSTR(Dest,.Srch)
  SET HOOKCMD="mv "_Source_" "_Dest
  ZSYSTEM HOOKCMD
  SET RESULT=$ZSYSTEM&255  ;"get result of execution. (low byte only)
  QUIT RESULT
  ;
Copy(Source,Dest)  ;
  ;"Purpose to provide a shell for the Linux command 'cp'
  ;"      This can serve to move or rename a file
  ;"Note: a platform independant version of the this could be constructed later...
  ;"Result: 0 IF no error; >0 IF error
  ;"Notice!!!! The return code here is DIFFERENT from usual
  ;
  NEW HOOKCMD,RESULT
  NEW Srch
  SET Srch(" ")="\ "
  SET Source=$$REPLACE^XLFSTR(Source,.Srch)
  SET Dest=$$REPLACE^XLFSTR(Dest,.Srch)
  SET HOOKCMD="cp "_Source_" "_Dest
  ZSYSTEM HOOKCMD
  SET RESULT=$ZSYSTEM&255  ;"get result of execution. (low byte only)
  QUIT RESULT
  ;
MKDIR(Dir)  ;
  ;"Purpose to provide a shell for the Linux command 'mkdir'
  ;"Note: a platform independant version of the this could be constructed later...
  ;"Result: 0 IF no error; >0 IF error
  ;"Notice!!!! The return code here is DIFFERENT from usual
  
  NEW HOOKCMD,RESULT
  NEW Srch SET Srch(" ")="\ "
  SET Dir=$$REPLACE^XLFSTR(Dir,.Srch)
  SET HOOKCMD="mkdir "_Dir
  ZSYSTEM HOOKCMD
  SET RESULT=$ZSYSTEM&255  ;"get result of execution. (low byte only)
  QUIT RESULT
  ;
RMDIR(Dir)  ;
  ;"Purpose to provide a shell for the Linux command 'rmdir'
  ;"Note: a platform independant version of the this could be constructed later...
  ;"Result: 0 IF no error; >0 IF error
  ;"Notice!!!! The return code here is DIFFERENT from usual
  NEW HOOKCMD,RESULT
  NEW Srch SET Srch(" ")="\ "
  SET Dir=$$REPLACE^XLFSTR(Dir,.Srch)
  SET HOOKCMD="rmdir "_Dir
  ZSYSTEM HOOKCMD
  SET RESULT=$ZSYSTEM&255  ;"get result of execution. (low byte only)
  QUIT RESULT
  ;
WGET(URL,OPTIONS,DIR)  ;
  ;"Purpose: Provide a shell for the linux command 'wget'
  ;"Input: URL -- the URL to download
  ;"       OPTIONS -- the text of the options to be passed to the wget command
  ;"                 see linux documentation for details.
  ;"       DIR -- the directory to download to.  The user must be able to
  ;"              cd into this directory.
  ;"Results: returns the result of the wget command.  0 = no problems
  ;"         NOTE: other return values defined in linux documentation.       
  NEW HOOKCMD,RESULT
  NEW Srch SET Srch(" ")="\ "
  SET DIR=$$REPLACE^XLFSTR(DIR,.Srch)
  SET HOOKCMD="cd "_DIR_" && wget"
  IF $GET(OPTIONS)'="" SET HOOKCMD=HOOKCMD_" "_OPTIONS
  SET HOOKCMD=HOOKCMD_" "_URL
  ZSYSTEM HOOKCMD
  SET RESULT=$ZSYSTEM&255  ;"get result of execution. (low byte only)
  QUIT RESULT
  ;
Convert(FPathName,NewType)  ;
  ;"Purpose: to convert a graphic image on the linux host to NEW type
  ;"         i.e. image.jpg --> image.png.  This is more than a simple renaming.
  ;"Input: FPathName -- full path, filename and extention.  E.g. "\tmp\image.jpg"
  ;"       NewType -- the NEW image type (without '.'),
  ;"                E.g. "jpg", or "JPG", or "TIFF", or "pcd" (NOT ".jpg" etc)
  ;"Output: New FPathName (with NEW extension) to NEW image file, or "" IF problem
  ;"
  ;"Note: If the conversion is successful, then the original image will be deleted
  ;"Note: This function depends on the ImageMagick graphic utility "convert" to be
  ;"      installed on the host linux system, and in the path so that it can be
  ;"      launched from any directory.
  NEW newFPathName SET newFPathName=""
  SET NewType=$GET(NewType)
  IF NewType="" GOTO ConvDone
  NEW FNAME,FPath,FileSpec
  DO SPLITFPN^TMGIOUTL(FPathName,.FPath,.FNAME,"/")
  SET FileSpec(FNAME)=""
  SET newFPathName=$PIECE(FPathName,".",1)_"."_NewType
  ;"Setup and launch linux command to execute convert
  NEW CmdStr
  SET CmdStr="convert "_FPathName_" "_newFPathName
  DO
  . ;"new $ETRAP,$ZTRAP
  . ;"set $ETRAP="S $ECODE="""""
  . ZSYSTEM CmdStr  ;"Launch command
  ;"get result of execution. (low byte only)  -- IF wanted
  NEW CmdResult SET CmdResult=$ZSYSTEM&255
  IF CmdResult'=0 DO  GOTO ConvDone
  . SET newFPathName=""
  ;"Delete old image file
  ;"**** temp!!!!! REMOVE COMMENTS LATER
  ;"new temp SET temp=$$DEL^%ZISH(FPath,"FileSpec")
ConvDone ;
  QUIT newFPathName
  ;
XLTLANG(Phrase,langPair)  ;
  ;"Purpose: To execute a linux OS call to convert a phrase into another
  ;"         spoken language
  ;"Input: Phrase -- The text to be translated.
  ;"       LangPair -- a language pair (as allowed by Google translater)
  ;"            for now, tested pairs are:
  ;"              "en-es" -- english  -> spanish
  ;"              "en-fr" -- english --> french
  ;"              "en-da" -- english --> ?
  ;"Result: The translated text, or "" IF error.
  ;"Note: This depends on the "tw" package be installed in the host OS
  ;"     I got this on 7/11/08 from: http://savannah.nongnu.org/projects/twandgtw/
  ;"Note: This is not working for some reason.....
  NEW RESULT SET RESULT=""
  SET langPair=$GET(langPair,"en-es")
  SET Phrase=$GET(Phrase,"?? Nothing Provided ??")
  NEW msgFNAME SET msgFNAME=$$UNIQUE^%ZISUTL("/tmp/TransLang.txt")
  ;"Setup and launch linux command to execute tw command
  NEW CmdStr
  SET CmdStr="tw translate.google.com."_langPair_" """_Phrase_""" > """_msgFNAME_""""
  ;"WRITE "About to execute zsystem command:",!,CmdStr,!
  ZSYSTEM CmdStr  ;"Launch command in linux OS
  ;"WRITE "Back from zsystem",!
  ;"get result of execution. (low byte only)  -- IF wanted
  NEW CmdResult SET CmdResult=$ZSYSTEM&255
  IF CmdResult'=0 GOTO TLDone
  NEW FNAME,FPath
  DO SPLITFPN^TMGIOUTL(msgFNAME,.FPath,.FNAME,"/")
  NEW RESULTArray
  IF $$FTG^%ZISH(FPath,FNAME,"RESULTArray(0)",1)=0 GOTO TLDone
  SET RESULT=$GET(RESULTArray(0))
TLDone  ;
  QUIT RESULT
  ;
TestTrans ;
  SET langPair=$GET(langPair,"en-es")
  SET Phrase=$GET(Phrase,"Hello friend")
  NEW msgFNAME SET msgFNAME=$$UNIQUE^%ZISUTL("/tmp/TransLang.txt")
  NEW CmdStr
  NEW qtChar SET qtChar="'"
  SET CmdStr="sh /var/local/OpenVistA_UserData/twlang.sh "_qtChar_langPair_qtChar_" "_qtChar_Phrase_qtChar_" "_msgFNAME
  WRITE "About to execute zsystem command:",!,CmdStr,!
  ZSYSTEM CmdStr  ;"Launch command in linux OS
  WRITE "Back from zsystem",!
  SET qtChar=""""
  SET CmdStr="sh /var/local/OpenVistA_UserData/twlang.sh "_qtChar_langPair_qtChar_" "_qtChar_Phrase_qtChar_" "_msgFNAME
  WRITE "About to execute zsystem command:",!,CmdStr,!
  ZSYSTEM CmdStr  ;"Launch command in linux OS
  WRITE "Back from zsystem",!
  QUIT
  ;
GetPckList(PCKINIT,Array,NeedsRefresh,PckDirFNAME)  ;"DEPRECIATED CODE
  QUIT $$GetPckList^TMGKERN4(.PCKINIT,.Array,.NeedsRefresh,.PckDirFNAME)
  ;
DownloadFile(URL,DestDir,Verbose,Timeout)  ;
  ;"Purpose: Interact with Linux to download a file with wget
  ;"Input: URL -- this is the URL of the file to be downloaded, as to be passed to wget
  ;"          IF the server is an FTP server, then URL should start with 'ftp://'
  ;"          NOTE: the URL will be enclosed in " ", so it may contain spaces etc,
  ;"               but should NOT have escaped characters, i.e. "Not\ this"
  ;"               Exception "April Fool'\''s Day" is proper
  ;"       DestDir -- this is the destination directory, on the HFS, where file should be stored
  ;"       Verbose -- OPTIONAL.  If 1, then output from wget is shown. Default is 0
  ;"       Timeout -- OPTIONAL.  Seconds of time to timeout.  Default=30 (units are SECONDS).
  ;"RESULT: 1 IF success, 0 IF failure
  NEW CmdStr,qFlag
  SET Timeout=+$GET(Timeout) IF Timeout'>0 SET Timeout=30
  ;"Setup and launch linux command to execute command
  IF +$GET(Verbose) SET qFlag=""
  ELSE  SET qFlag="--quiet "
  SET CmdStr="wget "_qFlag_"--timeout="_Timeout_" --directory-prefix="""_DestDir_""" """_URL_""""
  ZSYSTEM CmdStr  ;"Launch command in linux OS
  ;"get result of execution. (low byte only)
  NEW CmdResult SET CmdResult=$ZSYSTEM&255
  NEW RESULT SET RESULT=(CmdResult=0)
  QUIT RESULT
  ;
EditHFSFile(FilePathName,Editor)  ;
  ;"Purpose: interact with Linux to edit a file on the host file system
  ;"Input: FilePathName -- the full path of the file to edit.
  ;"       Editor -- name of editor to use.  Default="nano"
  ;"result: 1 IF success, 0 IF failure
  SET Editor=$get(Editor,"nano")
  ;"Setup and launch linux command to execute command
  NEW CmdStr SET CmdStr=Editor_" "_FilePathName
  ZSYSTEM CmdStr  ;"Launch command in linux OS
  ;"get result of execution. (low byte only)
  NEW CmdResult SET CmdResult=$ZSYSTEM&255
  NEW RESULT SET RESULT=(CmdResult=0)
  QUIT RESULT
  ;
EditArray(ARRAY,Editor) ;"interact with Linux to edit array as file on the host file system
  ;"Purpose: interact with Linux to edit array as a file on the host file system
  ;"Input: ARRAY - PASS BY REFERENCE.  Format: ARRAY(#)=<line of text>
  ;"       Editor -- name of editor to use.  Default="nano"
  ;"RESULT: 1 IF success, 0 IF failure
  NEW TMGRESULT
  NEW PATH SET PATH="/tmp/"
  NEW FNAME SET FNAME="M_ARRAY_EDIT_"_$J_".text"
  SET TMGRESULT=$$ARR2HFS^TMGIOUT3("ARRAY",PATH,FNAME)
  IF TMGRESULT'>0 GOTO EARDN
  SET TMGRESULT=$$EditHFSFile(PATH_FNAME,.Editor)
  IF TMGRESULT'>0 GOTO EARDN
  KILL ARRAY
  SET TMGRESULT=$$HFS2ARR^TMGIOUT3(PATH,FNAME,"ARRAY")
  IF TMGRESULT'>0 GOTO EARDN
  SET TMGRESULT=$$DELFILE^TMGIOUTL(PATH_FNAME)
EARDN ;
  QUIT TMGRESULT        
  ;        
ZSAVE   ;
  ;"Purpose: to save routine out to HFS
  ;"Input: globally scoped variable X should hold routine name
  ;
  ;"NOTE: this was moved out of ^DD("OS",19,"ZS")
  ;"Original line there was (all three lines were one long line)
  ;"N %I,%F,%S S %I=$I,%F=$P($P($ZRO,")"),"(",2)_"/"_X_".m" O %F:(NEWVERSION)
  ;"U %F X "S %S=0 F  S %S=$O(^UTILITY($J,0,%S)) Q:%S=""""  Q:'$D(^(%S))  S %=
  ;"^UTILITY($J,0,%S) I $E(%)'="";"" W %,!" C %F U %I
  ;"NOTE: The KIDS system seems to be using X ^%ZOSF("SAVE") instead of this.
  NEW %I,%F,%S
  NEW %  ;"//kt added -- not newing this caused problems in SAVE^DIKZ
  SET %I=$I
  NEW %DIR SET %DIR=$P($P($ZRO,")"),"(",2)
  SET %DIR=$PIECE(%DIR," ",$LENGTH(%DIR," "))
  SET %F=%DIR_"/"_X_".m"
  OPEN %F:(NEWVERSION)
  USE %F
  SET %S=0
  FOR  SET %S=$O(^UTILITY($J,0,%S)) Q:%S=""  Q:'$D(^(%S))  do
  . SET %=^UTILITY($J,0,%S)
  . IF $E(%)'=";" W %,!
  CLOSE %F
  USE %I
  QUIT
  ;
MAKEBAKF(FilePathName,NodeDiv,BackFName)  ;"Make Backup File if original exists
  ;"Purpose: to COPY existing File to File-ext_#.bak, creating a backup
  ;"         e.g. /tmp/dir1/FNAME.txt --> /tmp/dir1/FNAME-txt_1.bak
  ;"Input: FilePathName -- the name, e.g. /tmp/dir1/filename.txt
  ;"       NodeDiv -- OPTIONAL.  Default is "/"
  ;"              The node divider. "/" for unix, "\" for Microsoft
  ;"       BackFName -- OPTIONAL.  AN OUT PARAMETER.  PASS BY REFERENCE
  ;"              outputs the final backup filename, or "" if not created
  ;"RESULTs: none
  ;"Note: This assumes that the HFS supports filenames like FNAME-txt_1.bak,
  ;"      and length file name is not limited (e.g. not old 8.3 DOS style)
  ;"      Also, IF backup file, then number is incremented until a filename is found that doesn't exists
  ;"              e.g.  /tmp/dir1/FNAME-txt_1.bak
  ;"                    /tmp/dir1/FNAME-txt_2.bak
  ;"                    /tmp/dir1/FNAME-txt_3.bak
  SET NodeDiv=$GET(NodeDiv,"/")
  SET BackFName=""
  IF $$FILEXIST^TMGIOUTL(FilePathName) DO  ;"backup file IF it exists
  . NEW count SET count=0
  . NEW FNAME,FPath,done
  . DO SPLITFPN^TMGIOUTL(FilePathName,.FPath,.FNAME,NodeDiv)
  . FOR  DO  QUIT:done
  . . SET count=count+1
  . . SET BackFName=FNAME_"_"_count
  . . SET BackFName=FPath_$TRANSLATE(BackFName,".","-")_".bak"
  . . IF $$FILEXIST^TMGIOUTL(BackFName) DO  QUIT
  . . . SET done=0 
  . . . SET BackFName=""
  . . ELSE  do
  . . . SET done=1
  . . . IF $$Copy(FilePathName,BackFName)
  QUIT
  ;
IOCapON ;
  ;"Purpose: to redirect IO to a HFS file, so that it can be captured.
  ;"NOTE: CAUTION: If this is called, and then a routine asks for user input,
  ;"      then the program will appear to hang, because the message asking
  ;"      for input has gone to the output channel.
  SET TMGIOCAP=IO
  SET TMGIOCPT="/tmp/"
  SET TMGIOCFN="io-capture-"_$J_".txt"
  SET IO=TMGIOCPT_TMGIOCFN
  OPEN IO:(REWIND)
  USE IO
  QUIT
  ;
IOCapOFF(pOutArray)  ;
  ;"Purpose: To restore IO channel to that prior IOCapON was called, and return
  ;"        captured output in OutArray
  ;"NOTE: MUST call IOCapON prior to calling this function
  ;"Input: Globally-scoped TMGIOCAP is used.
  ;"       pOutArray -- PASS BY NAME, an OUT PARAMETER.  Prior contents are killed.
  ;"RESULTs: none
  CLOSE IO
  IF $GET(TMGIOCAP)="" USE $P GOTO IOCDone
  SET IO=TMGIOCAP
  USE IO
  IF $GET(pOutArray)="" GOTO IOCDone
  KILL @pOutArray
  IF ($GET(TMGIOCPT)="")!($GET(TMGIOCFN)="") GOTO IOCDone
  IF $$FTG^%ZISH(TMGIOCPT,TMGIOCFN,$name(@pOutArray@(0)),1)
  NEW TMGA SET TMGA(TMGIOCFN)=""
  IF $$DEL^%ZISH(TMGIOCPT,"TMGA")
IOCDone ;
  QUIT
  ;
KILLPID(JobNum)  ;
  DO KillPID(JobNum)
  QUIT
  ;
KillPID(JobNum)  ;
  ;"Purpose: send message to MUPIP to KILL Job
  NEW CmdStr SET CmdStr="mupip stop "_JobNum
  ZSYSTEM CmdStr  ;"Launch command in linux OS
  ;"do PRESS2GO^TMGUSRI2
  QUIT
  ;
TEST    ;
  NEW array
  NEW p SET p="temp"
  OPEN p:(COMMAND="ps -C mumps":readonly)::"pipe"
  USE p
  NEW LINEIN
  FOR  DO  QUIT:($zeof)
  . READ LINEIN
  . NEW ch FOR  DO  QUIT:(ch'=" ")
  . . SET ch=$EXTRACT(LINEIN,1,1)
  . . IF ch=" " SET LINEIN=$EXTRACT(LINEIN,2,40)
  . IF +LINEIN=0 QUIT
  . SET array(+LINEIN)=LINEIN
  CLOSE p
  USE $P
  DO ZWRITE^TMGZWR("array")
  QUIT
  ;
MJOBS(array)  ;
  ;"Purpose: To execute a linux OS call to get list of all 'mumps' jobs
  ;"         using: 'ps -C mumps'
  ;"Input: array -- PASS BY REFERNCE, an OUT PARAMETER.
  ;"Output: array is filled as follows:  (Prior data is killed)
  ;"         array(job#)=InfoLineFromOS
  ;"         array(job#)=InfoLineFromOS
  ;" e.g.    array(4483)=' 4883 pts/8   00:00:00 mumps'
  ;" e.g.    array(19308)='19308 ?       00:00:00 mumps'
  ;" e.g.    array(27454)='27454 pts/5   00:00:53 mumps'
  ;"Result: none
  NEW p SET p="temp"
  OPEN p:(COMMAND="ps -C mumps":readonly)::"pipe"
  USE p
  NEW LINEIN,ch
  FOR  DO  QUIT:($zeof)
  . READ LINEIN
  . FOR  DO  QUIT:(ch'=" ")
  . . SET ch=$EXTRACT(LINEIN,1,1) QUIT:(ch'=" ")
  . . SET LINEIN=$EXTRACT(LINEIN,2,40)
  . IF +LINEIN=0 QUIT
  . SET array(+LINEIN)=LINEIN
  CLOSE p
  ;"--The following block works at the TMG site.  Anyone can cut it out.
  OPEN p:(COMMAND="ps -C runAV":readonly)::"pipe"
  USE p
  NEW LINEIN,ch
  FOR  DO  QUIT:($zeof)
  . READ LINEIN
  . FOR  DO  QUIT:(ch'=" ")
  . . SET ch=$EXTRACT(LINEIN,1,1) QUIT:(ch'=" ")
  . . SET LINEIN=$EXTRACT(LINEIN,2,40)
  . IF +LINEIN=0 QUIT
  . SET array(+LINEIN)=LINEIN
  CLOSE p
  ;"---------------------------------------
  USE $P
  QUIT
  ;"====== old method below ==============
  KILL array
  NEW msgFNAME SET msgFNAME=$$UNIQUE^%ZISUTL("/tmp/mjobslist.txt")
  NEW CmdStr SET CmdStr="ps -C mumps > """_msgFNAME_""""
  ZSYSTEM CmdStr  ;"Launch command in linux OS
  ;
  ;"get result of execution. (low byte only)  -- IF wanted
  NEW CmdResult SET CmdResult=$ZSYSTEM&255
  IF CmdResult'=0 GOTO MJDone
  ;
  NEW FNAME,FPath
  DO SPLITFPN^TMGIOUTL(msgFNAME,.FPath,.FNAME,"/")
  NEW RESULTArray
  IF $$FTG^%ZISH(FPath,FNAME,"RESULTArray(0)",1)=0 GOTO TLDone
  ;
  ;"Delete temp info file
  NEW FileSpec SET FileSpec(FNAME)=""
  NEW temp SET temp=$$DEL^%ZISH(FPath,"FileSpec")
  ;
  ;"Format RESULTing array
  NEW i SET i=0
  FOR  SET i=$ORDER(RESULTArray(i)) QUIT:(i'>0)  do
  . NEW j SET j=$EXTRACT(RESULTArray(i),1,5)
  . NEW ch FOR  DO  QUIT:(ch'=" ")
  . . SET ch=$EXTRACT(j,1,1)
  . . IF ch=" " SET j=$EXTRACT(j,2,40)
  . SET array(+j)=RESULTArray(i)
  ;
MJDone ;
  QUIT
  ;
GETSCRSZ(ROWS,COLS)  ;
  ;"Purpose: To query the OS and get the dimensions of the terminal window
  ;"Input: ROWS,COLS -- Optional.  PASS BY REFERENCE.  Filled with results
  ;"Results: Row^Col  e.g. '24^80', or '24^60' as a default IF problem.
  ;"Note: thanks Bhaskar for figuring this out!
  SET ROWS=20,COLS=90 
  GOTO GSS2  ;"TEMP!!!
  ;"GOTO GSCRNSZ2+1 ;Sam's solution
  ;
GSS1 ;
  NEW p SET p="myTerm"
  NEW timeout SET timeout=1
  OPEN p:(COMMAND="stty -a -F "_$P_"|grep columns":readonly):timeout:"pipe"
  IF $TEST=0 SET (ROWS,COLS)=0 GOTO GSS2  ;"$T=0 IF timeout occurs during open command
  NEW x
  FOR  USE p READ x QUIT:($ZEOF)!(x["columns")
  CLOSE p USE $P
  SET COLS=+$PIECE(x,"columns ",2)-1
  SET ROWS=+$PIECE(x,"rows ",2)-1
GSS2 ;
  IF (COLS=0)&(ROWS=0) DO
  . SET COLS=100,ROWS=24
  ;
  NEW TROWS,TCOLS 
  IF $$GTMUMPW(.TROWS,.TCOLS)  ;"drop result
  IF TROWS<ROWS SET ROWS=TROWS ;"Use smaller of devices vs terminal window
  IF TCOLS<COLS SET COLS=TCOLS
  ;
  QUIT ROWS_"^"_COLS
  ;      
GSCRNSZ2(ROWS,COLS) ;
  ;"Modified from code posted by Sam Habiel,'...stolen from George Timson's %ZIS3.'
  ;"Purpose: query console device (the terminal window) and get current
  ;"         screen size, and set mumps environment to match.  Also will
  ;"         modify IOM and IOSL
  ;"NOTE: If INITKB^XGF() has been called to set up keyboard input escape code 
  ;"      processing, then it will capture result from QCUP^TMGTERM and make this
  ;"      function fail.  So user should call DO RESETKB^XGF to turn off XGF 
  ;"      escape key processing code prior to calling this function
  ;"Input: ROWS,COLS -- Optional.  PASS BY REFERENCE.  Filled with results
  ;"Result: '<Rows>^<Cols>', or '24^80' if problem
  NEW %I SET %I=$IO
  SET ROWS=24,COLS=80  ;"default
  DO 
  .  NEW X SET X=0 XECUTE ^%ZOSF("RM") ;"Disable wrapping.  ? if needed...
  .  DO VCUSAV2^TMGTERM      ;"Save cursor location
  .  DO ESSCR^TMGTERM        ;"Enable Screen Scrolling
  .  DO CUP^TMGTERM(999,999) ;"Set Cursor Position
  .  DO QCUP^TMGTERM         ;"Query Cursor Position  
  .  ;"The device is suppose to reply with: <ESC>[<ROW>;<COLUMN>R
  .  USE $P:(TERM="R":NOECHO)
  .  NEW ABORT SET ABORT=0
  .  NEW TEMP READ TEMP:1 SET ABORT=($T=0)  ;"If timesout, $T set to false
  .  USE $P:(TERM="":ECHO)
  .  DO VCULOAD2^TMGTERM   ;"Restore cursor location
  .  IF ABORT!(TEMP'?1C1"[".N1";".N) QUIT 
  .  SET COLS=+$P(TEMP,";",2)  ;"X value 
  .  SET ROWS=+$P(TEMP,"[",2)  ;"Y value
  .  USE $P:(TERM="":ECHO:WIDTH=COLS:LENGTH=ROWS)
  .  DO VCULOAD2^TMGTERM  ;"Note, both this AND one above are required for some reason 
  USE %I  ; restore state 
  SET IOM=ROWS,IOSL=COLS
  QUIT ROWS_"^"_COLS 
  ;
GTMUMPW(ROWS,COLS) ;" GET MUMPS WIDTH        
  ;"Purpose: To query the GT.M mumps environment and get the dimensions of
  ;"       the terminal device.  E.g. this would be the width where a mumps 
  ;"       WRITE would wrap on the screen.
  ;"Input: ROWS,COLS -- Optional.  PASS BY REFERENCE.  Filled with results
  ;"Results: Row^Col  e.g. '24^80', or '24^60' as a default IF problem.
  ;"Note: thanks Bhaskar for figuring this out!
  NEW TEMP ZSHOW "D":TEMP
  SET ROWS=24,COLS=60  ;"default values
  NEW DONE SET DONE=0
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(TEMP("D",IDX)) QUIT:(+IDX'>0)!(DONE=1)  DO
  . NEW STR SET STR=$GET(TEMP("D",IDX))
  . IF STR'["OPEN TERMINAL" QUIT
  . SET COLS=+$PIECE(STR,"WIDTH=",2)
  . SET ROWS=+$PIECE(STR,"LENG=",2)
  . SET DONE=1        
  QUIT ROWS_"^"_COLS
  ;
KILLVARS(zzMask) ;
  ;"Purpose: Selectively KILL variables from symbol table.
  ;"Input: Mask -- e.g. 'TMG*' -- will KILL all variables starting with TMG
  ;"              just '*' doesn't work.
  NEW zzName,zzTMGVars SET zzMask=$PIECE($GET(zzMask),"*",1)
  IF zzMask="" GOTO KVDN
  SET zzML=$LENGTH(zzMask)
  ZSHOW "V":zzTMGVars
  SET zzTMGVars=""
  FOR  SET zzTMGVars=$ORDER(zzTMGVars("V",zzTMGVars)) QUIT:(zzTMGVars="")  do
  . SET zzName=$PIECE($GET(zzTMGVars("V",zzTMGVars)),"=",1)
  . IF $EXTRACT(zzName,1,zzML)'=zzMask QUIT
  . KILL @zzName
KVDN ;
  QUIT
 ;
