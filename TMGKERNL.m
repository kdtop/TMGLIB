TMGKERNL ;TMG/kst/OS Specific functions ;3/8/18, 8/3/22
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
 ;"$$DOS2UNIX(FullNamePath)  --Wrapper to mixed-case function 
 ;"$$Dos2Unix^TMGKERNL(FullNamePath)
 ;"$$Unix2Dos(FullNamePath)
 ;"$$FileSize(FullNamePath)  -- return the size of the file, in bytes.
 ;"$$TIME(FORMAT)  --Get time from host OS
 ;"$$TIME2()  ;Get high resolution time
 ;"FSTAT(OUT,FPNAME,PARAMS) -- provide generic access to linux stat command.
 ;"$$FDIFF(OUT,FPNAME1,FPNAME2,PARAMS) -- generic access to linux diff command.
 ;"$$VIMDIFF(FPNAME1,FPNAME2,PARAMS) --FILES vimdiff command 
 ;"$$ADIFF(OUT,ARR1,ARR2,PARAMS)  -- use diff command on 2 arrays
 ;"$$VIMADIFF(ARR1,ARR2,PARAMS)  -- use vimdiff command on 2 arrays 
 ;"$$CMDOK(CMDNAME) ;Check specified Linux command is available.
 ;"$$LINUXCMD(CMD,OUT)  -- execute command on linux system, and return output
 ;"$$ISFILE(FPNAME)  ;-- FileExists() type file.  See also $$FILEXIST^TMGIOUTL(FilePathName)
 ;"$$ISDIR^TMGKERNL(Path) 
 ;"$$DIR2(PATH,OUT,OPTION) --Expanded directory lising, with parsing to array etc.  
 ;"$$ENSURDIR(DIR) -- ensure directory path exists  
 ;"$$EnsureDir(Dir) -- ensure directory path exists
 ;"$$MOVE^TMGKERNL(Source,Dest)
 ;"$$COPY(SRC,DEST) ;copy file
 ;"$$Copy^TMGKERNL(Source,Dest) ;copy file
 ;"$$MKDIR(Dir) -- provide a shell for the Linux command 'mkdir'
 ;"$$RMDIR(Dir) -- provide a shell for the Linux command 'rmdir'
 ;"$$RMFILE(PATHFILE) -- provide a shell for the Linux command 'rm' 
 ;"$$WGET(URL,OPTIONS,DIR) Provide a shell for the linux command 'wget'
 ;"$$Convert^TMGKERNL(FPathName,NewType) -- convert a graphic image to new type
 ;"$$THUMBNAIL(PATH,SOURCEFNAME,DESTFNAME,SIZE)  ;Create thumbnail of graphic image file
 ;"$$BLACKIMG(DESTPATHFNAME,SIZE) ;Make empty black image
 ;"$$XLTLANG(Phrase,langPair) -- execute a linux OS call to convert a phrase into another spoken language
 ;"$$DOWNLOADFILE(URL,DestDir,Verbose,Timeout)  -- Wrapper for mixed case function 
 ;"$$DownloadFile^TMGKERNL(URL,DestDir) -- Interact with Linux to download a file with wget
 ;"$$EDITARRAY(ARRAY,EDITOR)  -- interact with Linux to edit array as file on the host file system
 ;"$$EditArray^TMGKERNL(ARRAY,Editor) -- interact with Linux to edit array as file on the host file system
 ;"$$EDITHFSFILE(FILEPATHNAME,EDITOR) -- Wrapper for mixed case function.   
 ;"$$EditHFSFile^TMGKERNL(FilePathName,Editor) -- interact with Linux to edit a file on the host file system
 ;"ZSAVE -- to save routine out to HFS
 ;"MAKEBAKF^TMGKERNL(FilePathName,NodeDiv)  ;Make Backup File if original exists
 ;"IOCapON -- redirect IO to a HFS file, so that it can be captured.
 ;"IOCapOFF(pOutArray) -- restore IO channel to that prior IOCapON was called, and return captured output in OutArray
 ;"KillPID(JobNum) -- send message to MUPIP to KILL Job
 ;"MJOBS(array) -- execute a linux OS call to get list of all 'mumps' jobs using: 'ps -C mumps'
 ;"$$GETSCRSZ(ROWS,COLS) --query the OS and get the dimensions of the terminal window.
 ;"KILLVARS(Mask) -- Selectively KILL variables from symbol table.
 ;"GBLOUTGREP(FPATHNAME,FILTER,OUT) -- GLOBAL OUTPUT GREP  
 ;"KIDSGREP(FPATHNAME,FILTER,OUT)  -- Scan HFS .KIDS file and return strings based on filter
 ;"=======================================================================
 ;"Dependancies  TMGIOUTL, %webjson*
 ;"=======================================================================
 ;"=======================================================================
 ;
DOS2UNIX(FULLPATHNAME)  ;"Wrapper to mixed-case function
  QUIT $$Dos2Unix(.FULLPATHNAME)  
  ;
Dos2Unix(FullNamePath)  ;
  ;"Purpose: To execute the unix command Dos2Unix on filename path
  ;"FullNamePath: The filename to act on.
  ;"Result: 0 IF no error; >0 IF error
  ;"Notice!!!! The return code here is DIFFERENT from usual
  NEW RESULT SET RESULT=0
  IF $GET(FullNamePath)="" GOTO UDDone
  ;" NEW spec SET spec(" ")="\ "
  ;" SET FullNamePath=$$REPLACE^XLFSTR(FullNamePath,.spec)
  ;"new HOOKCMD SET HOOKCMD="dos2unix -q "_FullNamePath
  NEW HOOKCMD SET HOOKCMD="fromdos """_FullNamePath_""""
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
  ;"NEW spec SET spec(" ")="\ "
  ;"SET FullNamePath=$$REPLACE^XLFSTR(FullNamePath,.spec)
  NEW HOOKCMD SET HOOKCMD="todos """_FullNamePath_""""
  ZSYSTEM HOOKCMD
  SET RESULT=$ZSYSTEM&255  ;"get result of execution. (low byte only)
DUDone  ;
  QUIT RESULT
  ;
TIME(FORMAT)  ;Get time from host operating system timer -- DEPRECIATED, USE TIME2
  ;"SET FORMAT=$GET(FORMAT,"%T"".""%N")  ;"HH:MM:SS.nnnnnnnnn  (n=nanoseconds)
  SET FORMAT=$GET(FORMAT,"%s"".""%N")    ;"seconds since 1970 with nanoseconds
  NEW P SET P="MyTerm" OPEN P:(COMMAND="date +"_FORMAT:readonly)::"pipe"
  USE P NEW X READ X CLOSE P USE $P
  QUIT X
  ;
TIME2()  ;Get high resolution time
  ;"Per Bhaskar, below is faster...
  QUIT $ZUT/1000000
  ;
  ;"SET FORMAT=$GET(FORMAT,"%T"".""%N")  ;"HH:MM:SS.nnnnnnnnn  (n=nanoseconds)
  SET FORMAT=$GET(FORMAT,"%s"".""%N")    ;"seconds since 1970 with nanoseconds
  NEW P SET P="MyTerm" OPEN P:(COMMAND="date +"_FORMAT:readonly)::"pipe"
  USE P NEW X READ X CLOSE P USE $P
  QUIT X

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
FDIFF(OUT,FPNAME1,FPNAME2,PARAMS)  ;"FILEs diff command
  ;"Purpose: Provide generic access to linux diff command.
  ;"Input:  OUT: Returns result from reading command output.  Format
  ;"          OUT(#)=<lines of text output from command"
  ;"        FPNAME1: The first path+filename to compare
  ;"        FPNAME2: The second path+filename to compare
  ;"        PARAMS: (optional)  Contains command options, all in one long string.
  ;"           e.g. "--ignore-case"   or "-Z"   See Linux documentation for details
  ;"Result: 0 if results are the same, 1 if different, 2 if problem
  NEW CMD SET CMD="diff "_$GET(PARAMS)_" """_FPNAME1_""" """_FPNAME2_""""
  NEW P SET P="TEMP" OPEN P:(COMMAND=CMD:readonly)::"pipe" USE P
  KILL OUT NEW X,IDX SET IDX=1
  FOR   QUIT:$ZEOF  DO
  . READ X IF X=""&$ZEOF QUIT  
  . SET OUT(IDX)=X,IDX=IDX+1
  CLOSE P USE $P
  NEW RESULT SET RESULT=$ZSYSTEM&255  ;"get result of execution. (low byte only)
  QUIT RESULT  
  ;
VIMDIFF(FPNAME1,FPNAME2,PARAMS)  ;"FILES vimdiff command
  ;"Purpose: Provide generic access to linux diff command.
  ;"Input:  FPNAME1: The first path+filename to compare
  ;"        FPNAME2: The second path+filename to compare
  ;"        PARAMS: (optional)  Contains command options, all in one long string.
  ;"           e.g. "--ignore-case"   or "-Z"   See Linux documentation for details
  ;"Result: 1^OK, or -1^ErrMsg 
  WRITE #
  NEW L2 SET L2="REMEMBER!!\n"
  SET L2=L2_"(It can be hard to get out of vim!!)\n"
  SET L2=L2_"\n"
  SET L2=L2_"To exit, type <ESC>:qa<ENTER>\n"
  SET L2=L2_"\n"
  SET L2=L2_"Explaination:\n"
  SET L2=L2_"<ESC> ensures vim is set to COMMAND MODE\n"
  SET L2=L2_":q is for QUIT, and :qa is for quit ALL\n"
  SET L2=L2_"NOTE: the 'qa' must be **lower case** letters)"
  IF $GET(PARAMS)'="" SET PARAMS=PARAMS_" "
  DO POPUPBOX^TMGUSRI2("NOTICE! Launching vimdiff",L2)
  DO PRESS2GO^TMGUSRI2
  NEW CMD SET CMD="vimdiff "_$GET(PARAMS)_""""_FPNAME1_""" """_FPNAME2_""""
  ZSYSTEM CMD
  NEW RESULT SET RESULT=$ZSYSTEM&255  ;"get result of execution. (low byte only)
  IF RESULT=0 SET RESULT="1^OK"
  ELSE  SET RESULT="-1^Linux Error: "_RESULT
  QUIT RESULT  
  ;
ADIFF(OUT,ARR1,ARR2,PARAMS)  ;"use diff command on 2 arrays
  ;"Purpose: Provide generic access to linux diff command.
  ;"Input:  OUT: Returns result from reading command output.  Format
  ;"          OUT(#)=<lines of text output from command"
  ;"        ARR1: The first array to compare.  Format: ARR1(#)=line of text
  ;"        ARR2: The second array to compare  Format: ARR2(#)=line of text
  ;"        PARAMS: (optional)  Contains command options, all in one long string.
  ;"           e.g. "--ignore-case"   or "-Z"   See Linux documentation for details
  ;"Result: 0 if results are the same, 1 if different, 2 if problem, OR -1^Message  
  NEW TMGRESULT
  NEW PATH SET PATH="/tmp/"
  NEW FNAME1 SET FNAME1=$$UNIQUE^%ZISUTL("M_ARRAY_DIF_1.text")
  NEW FNAME2 SET FNAME2=$$UNIQUE^%ZISUTL("M_ARRAY_DIF_2.text")
  SET TMGRESULT=$$ARR2HFS^TMGIOUT3("ARR1",PATH,FNAME1)
  IF TMGRESULT'>0 GOTO DFDN
  SET TMGRESULT=$$ARR2HFS^TMGIOUT3("ARR2",PATH,FNAME2)
  IF TMGRESULT'>0 GOTO DFDN
  NEW TEMP SET TEMP=$$FDIFF(.OUT,PATH_FNAME1,PATH_FNAME2,.PARAMS)
  NEW TEMP2 SET TEMP2=$$DELFILE^TMGIOUTL(PATH_FNAME1)
  NEW TEMP3 SET TEMP3=$$DELFILE^TMGIOUTL(PATH_FNAME2)
  IF TEMP2'>0 SET TMGRESULT=TEMP2 GOTO DFDN 
  IF TEMP3'>0 SET TMGRESULT=TEMP3 GOTO DFDN
  SET TMGRESULT=TEMP  
DFDN  ;
  QUIT TMGRESULT
  ;  
VIMADIFF(ARR1,ARR2,PARAMS)  ;"use vimdiff command on 2 arrays
  ;"Purpose: Provide generic access to linux diff command.
  ;"Input:  ARR1: The first array to compare.  Format: ARR1(#)=line of text
  ;"        ARR2: The second array to compare  Format: ARR2(#)=line of text
  ;"        PARAMS: (optional)  Contains command options, all in one long string.
  ;"           e.g. "--ignore-case"   or "-Z"   See Linux documentation for details
  ;"Result: 1^OK, or -1^ErrMsg 
  NEW TMGRESULT,TEMP SET TMGRESULT="1^OK"
  NEW PATH SET PATH="/tmp/"
  NEW FNAME1 SET FNAME1=$$UNIQUE^%ZISUTL("M_ARRAY_DIF_1.txt")
  NEW FNAME2 SET FNAME2=$$UNIQUE^%ZISUTL("M_ARRAY_DIF_2.txt")
  SET TMGRESULT=$$ARR2HFS^TMGIOUT3("ARR1",PATH,FNAME1) IF TMGRESULT'>0 GOTO DFDN2
  SET TMGRESULT=$$ARR2HFS^TMGIOUT3("ARR2",PATH,FNAME2) IF TMGRESULT'>0 GOTO DFDN2
  NEW TEMP SET TEMP=$$VIMDIFF(PATH_FNAME1,PATH_FNAME2,.PARAMS)
  ;"Load data from files back into array to return to caller.
  KILL ARR1 SET TMGRESULT=$$HFS2ARR^TMGIOUT3(PATH,FNAME1,"ARR1") IF TMGRESULT'>0 GOTO DFDN2
  KILL ARR2 SET TMGRESULT=$$HFS2ARR^TMGIOUT3(PATH,FNAME2,"ARR2") IF TMGRESULT'>0 GOTO DFDN2
  NEW TEMP2 SET TEMP2=$$DELFILE^TMGIOUTL(PATH_FNAME1)
  NEW TEMP3 SET TEMP3=$$DELFILE^TMGIOUTL(PATH_FNAME2)
  IF TEMP2'>0 SET TMGRESULT=TEMP2 GOTO DFDN2
  IF TEMP3'>0 SET TMGRESULT=TEMP3 GOTO DFDN2
  SET TMGRESULT=TEMP  
DFDN2  ;
  QUIT TMGRESULT
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
LINUXCURL(OUT,URL,ARR,HEADERS,DATA) ;"Shell to linux curl command.  
  ;"NOTE: Will crash if resulting commend is > 255 chars  
  ;"Input: OUT -- PASS BY REFERENCE.   An array with output from curl command
  ;"       URL -- the URL to pass to curl
  ;"       ARR -- PASS BY REFERENCE.  Each ARR(x) element is appended to make parameters
  ;"       HEADERS --  PASS BY REFERENCE. OPTIONAL. Note: passed viat temp intermediate file on HFS
  ;"          Format:  HEADERS(#)=<header line>    <--  "-H" will be added for each line.  
  ;"       DATA -- PASS BY REFERENCE.  OPTIONAL. This DATA mumps variable will be converted
  ;"               to json, stored in intermediate file on host file system, and passed 
  ;"               to curl via --data-binary @... parameter 
  ;"Result: none. 
  NEW CMD SET CMD="curl "_URL_" --silent "
  NEW IDX SET IDX=""
  FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:IDX=""  DO
  . SET CMD=CMD_ARR(IDX)
  NEW DIR SET DIR="/tmp/"
  NEW DELARR
  NEW HEADERFPNAME SET HEADERFPNAME=""
  IF $DATA(HEADERS) DO  
  . SET HEADERFPNAME=$$UNIQUE^%ZISUTL(DIR_"curlHeaders.txt")
  . NEW FPATH,FNAME
  . DO SPLITFPN^TMGIOUTL(HEADERFPNAME,.FPATH,.FNAME) ;"split PathFileName into Path, Filename
  . NEW RESULT SET RESULT=$$ARR2HFS^TMGIOUT3("HEADERS",FPATH,FNAME)
  . IF RESULT'=1 QUIT
  . SET DELARR(FNAME)=""
  . SET CMD=CMD_" --header @"_HEADERFPNAME
  NEW DATAFPNAME SET DATAFPNAME=""
  IF $DATA(DATA) DO
  . SET DATAFPNAME=$$UNIQUE^%ZISUTL(DIR_"curlData.txt")
  . NEW FPATH,FNAME
  . DO SPLITFPN^TMGIOUTL(DATAFPNAME,.FPATH,.FNAME) ;"split PathFileName into Path, Filename
  . NEW TMGJSON DO ENCODE^%webjson("DATA","TMGJSON")
  . NEW RESULT SET RESULT=$$ARR2HFS^TMGIOUT3("TMGJSON",FPATH,FNAME)
  . IF RESULT'=1 QUIT
  . SET DELARR(FNAME)=""
  . SET CMD=CMD_" --data-binary @"_DATAFPNAME
  DO LINUXCMD(CMD,.OUT)
  IF $DATA(DELARR) DO
  . IF $$DEL^%ZISH(DIR,$NAME(DELARR))  ;"ignore possible errors. 
  QUIT
 ;
RANDOM(LOW,HI) ;"Return random number
  ;"Input: LOW -- OPTIONAL, low end of range for random number. Default = 0
  ;"       HI -- OPTIONAL, high end of range for random number.  Default = 1
  ;"Result: returns fractional random number in range, getting number from Linux 
  NEW TEMP
  SET LOW=+$GET(LOW)
  SET HI=+$GET(HI) IF HI=0 SET HI=1
  DO LINUXCMD("echo $RANDOM",.TEMP)
  NEW FRAC
  SET FRAC=TEMP(1)/32768
  NEW RESULT SET RESULT=(HI-LOW)*FRAC+LOW
  QUIT RESULT
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
  OPEN p:(COMMAND="stat --format=%F """_Path_"""":readonly)::"pipe"
  USE p
  NEW x READ x
  CLOSE p USE $P
  QUIT (x="directory")
  ;
DIR2(PATH,OUT,OPTION) ;"Expanded directory
  ;"INPUT: PATH -- HFS path to get directory from 
  ;"       OUT -- pass by reference, an OUT parameter.  See format below. 
  ;"       OPTION -  OPTIONAL
  ;"         OPTION("R") -- RECURSIVE (include sub-folders)
  ;"         OPTION("INCLUDE")= FNAME^PATH^FILE_TYPE^PERMISSIONS^HARD_LINKS^OWNER^GROUP^SIZE^FMDT
  ;"                          Each piece is a flag with values as follows:
  ;"                             0  (0b00) : don't include  (note: this will be ignored for FNAME piece)
  ;"                             1  (0b01) : include value in main array
  ;"                             2  (0b10) : include value in array index 
  ;"                             3  (0b11) : include value in both places 
  ;"                             e.g. 1^2^0^0^0^1^0^0^0  <-- E.g. return fNAME, path (also indexed) and OWNER
  ;"                          DEFAULT IS ALL 0's (except FNAME piece default is 1, 0 not allowed)
  ;"OUTPUT:  OUT:
  ;"Result: "1^OK" or "-1^Error Message"
  NEW CMD SET CMD="ls "_PATH_" -Al"
  NEW RESULT SET RESULT="1^OK"
  NEW TMP SET RESULT=$$LINUXCMD(CMD,.TMP)
  NEW IDX SET IDX=""
  FOR  SET IDX=$ORDER(TMP(IDX)) QUIT:IDX'>0  DO
  . NEW LINE SET LINE=$GET(TMP(IDX)) QUIT:LINE=""
  . IF $PIECE(LINE," ",1)="total" QUIT
  . IF $GET(OPTION("R")),$EXTRACT(LINE,1)="d" DO
  . . NEW TMP,TMPOPTION SET TMPOPTION("INCLUDE")="1"
  . . DO PARSE1DIRLN(LINE,PATH,.TMP,.TMPOPTION)
  . . NEW SUBPATH SET SUBPATH=$GET(TMP(1,"FNAME")) QUIT:SUBPATH=""
  . . SET SUBPATH=PATH_"/"_SUBPATH
  . . ;"WRITE SUBPATH,!
  . . KILL TMP IF $$DIR2(SUBPATH,.TMP,.OPTION)
  . . MERGE OUT=TMP
  . DO PARSE1DIRLN(LINE,PATH,.OUT,.OPTION)
  QUIT RESULT
  ;
PARSE1DIRLN(LINE,PATH,OUT,OPTION)  ;"private function for DIR2 above  
  ;"INPUT: LINE -- 1 line from a linux directory listing, from 'ls <path> -Al'
  ;"       OUT -- AN OUT PARAMETER.
  ;"       OPTION("INCLUDE")= FNAME^PATH^FILE_TYPE^PERMISSIONS^HARD_LINKS^OWNER^GROUP^SIZE^FMDT
  ;"                          Each piece is a flag with values as follows:
  ;"                             0  (0b00) : don't include  (note: this will be ignored for FNAME piece)
  ;"                             1  (0b01) : include value in main array
  ;"                             2  (0b10) : include value in array index 
  ;"                             3  (0b11) : include value in both places 
  ;"                             e.g. 1^2^0^0^0^1^0^0^0  <-- E.g. return fNAME, path (also indexed) and OWNER
  ;"                          DEFAULT IS ALL 0's (except FNAME piece default is 1, 0 not allowed)
  NEW SPLITOPTION SET SPLITOPTION("TRIM DIV")=1
  NEW TMP,IDX,JDX SET IDX=$GET(OUT("MAX"))+1 SET OUT("MAX")=IDX
  FOR JDX=1:1:8 DO
  . NEW PARTB DO CLEAVSTR^TMGSTUT2(.LINE," ",.PARTB,,.SPLITOPTION)
  . SET TMP(JDX)=LINE,LINE=PARTB
  SET FNAME=LINE  ;"this residual should be the filename, and it may contain spaces.  
  SET FNAME=$PIECE(FNAME," -> ",1)  ;"drop off arrow to actual file name if soft link.  
  ;
  NEW LABELS SET LABELS="FNAME^PATH^FILE_TYPE^PERMISSIONS^HARD_LINKS^OWNER^GROUP^SIZE^FMDT"
  NEW INCLUDE SET INCLUDE=$GET(OPTION("INCLUDE"))
  FOR JDX=1:1:9 SET INCLUDE(JDX)=$PIECE(INCLUDE,"^",JDX),LABELS(JDX)=$PIECE(LABELS,"^",JDX)
  IF INCLUDE(1)'>1 SET INCLUDE(1)=1
  ;  
  NEW DATA
  SET DATA(1)=FNAME
  SET DATA(2)=PATH
  SET DATA(3)=$EXTRACT($GET(TMP(1)),1)    ;"TYPE
  SET DATA(4)=$EXTRACT($GET(TMP(1)),2,10) ;"PERMISSIONS
  SET DATA(5)=$GET(TMP(2))                ;"HARDLINKS
  SET DATA(6)=$GET(TMP(3))                ;"OWNER
  SET DATA(7)=$GET(TMP(4))                ;"GROUP
  SET DATA(8)=$GET(TMP(5))                ;"SIZE
  DO
  . NEW MONTH     SET MONTH=$GET(TMP(6))  
  . NEW DAY       SET DAY=$GET(TMP(7))
  . NEW TIME      SET TIME=""
  . NEW YR        SET YR=$GET(TMP(8))
  . IF YR[":"     SET TIME=YR,YR=1700+$EXTRACT($$NOW^XLFDT,1,3)
  . NEW DTSTR SET DTSTR=MONTH_" "_DAY_" "_YR
  . IF TIME'="" SET DTSTR=DTSTR_" @"_TIME
  . NEW FMDT SET FMDT=$$EXT2FMDT^TMGDATE(DTSTR)
  . SET DATA(9)=FMDT
  ;
  FOR JDX=1:1:9 DO
  . NEW MAIN SET MAIN=INCLUDE(JDX)#2
  . NEW INDEX SET INDEX=(INCLUDE(JDX)/2)\1#2
  . IF MAIN SET OUT(IDX,LABELS(JDX))=DATA(JDX)
  . IF INDEX SET OUT(LABELS(JDX),DATA(JDX),IDX)=""
  QUIT
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
  ;"NEW Srch
  ;"SET Srch(" ")="\ "
  ;"SET Source=$$REPLACE^XLFSTR(Source,.Srch)
  ;"SET Dest=$$REPLACE^XLFSTR(Dest,.Srch)
  SET HOOKCMD="mv """_Source_""" """_Dest_""""
  ZSYSTEM HOOKCMD
  SET RESULT=$ZSYSTEM&255  ;"get result of execution. (low byte only)
  QUIT RESULT
  ;                         
COPY(SRC,DEST) ;
  QUIT $$Copy(.SRC,.DEST)
  ;
Copy(Source,Dest)  ;
  ;"Purpose to provide a shell for the Linux command 'cp'
  ;"      This can serve to move or rename a file
  ;"Note: a platform independant version of the this could be constructed later...
  ;"Result: 0 if no error; >0 if error
  ;"Notice!!!! The return code here is DIFFERENT from usual
  ;
  NEW HOOKCMD,RESULT
  ;"NEW Srch
  ;"SET Srch(" ")="\ "
  ;"SET Source=$$REPLACE^XLFSTR(Source,.Srch)
  ;"SET Dest=$$REPLACE^XLFSTR(Dest,.Srch)
  SET HOOKCMD="cp """_Source_""" """_Dest_""""
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
  ;"NEW Srch SET Srch(" ")="\ "
  ;"SET Dir=$$REPLACE^XLFSTR(Dir,.Srch)
  SET HOOKCMD="mkdir """_Dir_""""
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
  ;"NEW Srch SET Srch(" ")="\ "
  ;"SET Dir=$$REPLACE^XLFSTR(Dir,.Srch)
  SET HOOKCMD="rmdir """_Dir_""""
  ZSYSTEM HOOKCMD
  SET RESULT=$ZSYSTEM&255  ;"get result of execution. (low byte only)
  QUIT RESULT
  ;
RMFILE(PATHFILE) ;
  ;"Purpose to provide a shell for the Linux command 'rm'
  ;"Note: a platform independant version of the this could be constructed later...
  ;"Result: 0 IF no error; >0 IF error
  ;"Notice!!!! The return code here is DIFFERENT from usual
  NEW HOOKCMD,RESULT                                                                                                                                  
  ;" NEW Srch 
  ;" SET Srch(" ")="\ "
  ;" SET Srch("(")="\("
  ;" SET Srch(")")="\)"                                    
  ;" SET PATHFILE=$$REPLACE^XLFSTR(PATHFILE,.Srch)
  SET HOOKCMD="rm """_PATHFILE_""""
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
  ;" NEW Srch SET Srch(" ")="\ "
  ;" SET Srch(" ")="\ "
  ;" SET Srch("(")="\("
  ;" SET Srch(")")="\)"
  ;" SET DIR=$$REPLACE^XLFSTR(DIR,.Srch)
  ;" SET URL=$$REPLACE^XLFSTR(URL,.Srch)
  SET HOOKCMD="cd """_DIR_""" && wget"
  IF $GET(OPTIONS)'="" SET HOOKCMD=HOOKCMD_" "_OPTIONS
  SET HOOKCMD=HOOKCMD_" """_URL_""""
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
THUMBNAIL(PATH,SOURCEFNAME,DESTFNAME,SIZE)  ;
  ;"Purpose: to convert a graphic image on the linux host to a thumbnail
  ;"Input: SOURCEPATHFNAME -- full path, filename and extention of sourcefile
  ;"       DESTPATHFNAME -- full path, filename and extention of output file to be created
  ;"       SIZE =  width and height.  E.g. '256' OPTIONAL.  Default is 64
  ;"Output: "1^DESTPATHFNAME" to new image file, or "-1^Error Message" if problem
  ;"
  ;"Note: This function depends on the ImageMagick graphic utility "convert" to be
  ;"      installed on the host linux system, and in the path so that it can be
  ;"      launched from any directory.
  NEW TMGRESULT SET TMGRESULT=""
  SET PATH=$GET(PATH)
  IF PATH="" DO  GOTO THUMBDN
  . SET TMGRESULT="-1^No directory path provided"
  SET SOURCEFNAME=$GET(SOURCEFNAME)
  IF SOURCEFNAME="" DO  GOTO THUMBDN
  . SET TMGRESULT="-1^No source filename provided"
  SET DESTFNAME=$GET(DESTFNAME)
  IF DESTFNAME="" DO  GOTO THUMBDN
  . SET TMGRESULT="-1^No destination filename provided"
  SET SIZE=+$GET(SIZE) IF SIZE'>0 SET SIZE=64
  NEW FN1 SET FN1=PATH_SOURCEFNAME
  NEW FN2 SET FN2=PATH_DESTFNAME
  ;"Setup and launch linux command to execute convert
  NEW CMDSTR SET CMDSTR="convert "_FN1_" -thumbnail '"_SIZE_"X"_SIZE_"' "_FN2
  ZSYSTEM CMDSTR  ;"Launch command
  ;"get result of execution. (low byte only)  -- IF wanted
  NEW TEMP SET TEMP=$ZSYSTEM&255   ;"0 means no error
  IF TEMP'=0 DO  GOTO THUMBDN
  . SET TMGRESULT="-1^Error executing Linux convert command."
  SET TMGRESULT="1^"_FN2
THUMBDN ;
  QUIT TMGRESULT
  ;
BLACKIMG(DESTPATHFNAME,SIZE)  ;"Make empty black image
  ;"Purpose: to convert a graphic image on the linux host to a thumbnail
  ;"Input: DESTPATHFNAME -- full path, filename and extention of output file to be created
  ;"       SIZE =  width and height.  E.g. '256' OPTIONAL.  Default is 64
  ;"Output: "1^DESTPATHFNAME" to new image file, or "-1^Error Message" if problem
  ;"
  ;"Note: This function depends on the ImageMagick graphic utility "convert" to be
  ;"      installed on the host linux system, and in the path so that it can be
  ;"      launched from any directory.
  NEW TMGRESULT SET TMGRESULT=""
  SET DESTPATHFNAME=$GET(DESTPATHFNAME)
  IF DESTPATHFNAME="" DO  GOTO BLKDN
  . SET TMGRESULT="-1^No destination filename provided"
  SET SIZE=+$GET(SIZE) IF SIZE'>0 SET SIZE=64
  NEW CMDSTR SET CMDSTR="convert -size '"_SIZE_"X"_SIZE_"' "_DESTPATHFNAME
  ZSYSTEM CMDSTR  ;"Launch command
  ;"get result of execution. (low byte only)  -- IF wanted
  NEW TEMP SET TEMP=$ZSYSTEM&255   ;"0 means no error
  IF TEMP'=0 DO  GOTO THUMBDN
  . SET TMGRESULT="-1^Error executing Linux convert command."
  SET TMGRESULT="1^"_DESTPATHFNAME
BLKDN ;
  QUIT TMGRESULT
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
DOWNLOADFILE(URL,DestDir,Verbose,Timeout)  ;" Wrapper for mixed case function
  QUIT $$DownloadFile(.URL,.DestDir,.Verbose,.Timeout)  
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
EDITHFSFILE(FILEPATHNAME,EDITOR) ;"Wrapper for mixed case funciton
  QUIT $$EditHFSFile(.FILEPATHNAME,.EDITOR)  ;
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
EDITARRAY(ARRAY,EDITOR)  ;"interact with Linux to edit array as file on the host file system
  QUIT $$EditArray(.ARRAY,.EDITOR)
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
  NEW OPTION SET OPTION("OVERFLOW")=1  ;"turn overflow lines into possibly LONG array lines.  
  SET TMGRESULT=$$HFS2ARR^TMGIOUT3(PATH,FNAME,"ARRAY",.OPTION)
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
  ;"      Also, if backup file, then number is incremented until a filename is found that doesn't exists
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
  DO GSCRNSZ4(.ROWS,.COLS) 
  QUIT ROWS_"^"_COLS
  ;
GSCRNSZ1(ROWS,COLS) ;
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
  NEW TROWS,TCOLS 
  IF $$GTMUMPW(.TROWS,.TCOLS)  ;"drop result
  IF TROWS<ROWS SET ROWS=TROWS ;"Use smaller of devices vs terminal window
  IF TCOLS<COLS SET COLS=TCOLS
GSS1DN ;  
  QUIT ROWS_"^"_COLS
  ;
GSCRNSZ4(ROWS,COLS) ;
  NEW TEMP SET TEMP="0^0"
  ;"NEW X SET X="XVEMKY" X ^%ZOSF("TEST") IF $T SET TEMP=$$AUTOMARG^XVEMKY
  SET TEMP=$$AUTOMARG()
  SET COLS=+$PIECE(TEMP,"^",1)
  SET ROWS=+$PIECE(TEMP,"^",2)
  IF COLS'>0 SET COLS=80
  IF ROWS'>24 SET ROWS=24
  QUIT
  ;
AUTOMARG() ;"RETURNS IOM^IOSL IF IT CAN and resets terminal to those dimensions -- from George Timson's %ZIS3.
 ;"Taken from $$AUTOMARG^XVEMKY and stripped to just YottaDB, and modified. 
 NEW DEVSAV DO DEV2ARR^TMGKERN1($IO,.DEVSAV)
 IF $DATA(^%ZOSF("RM")) DO  ;"Setting WIDTH=0 to prevent wrapping
 . NEW X SET X=0 
 . XECUTE ^%ZOSF("RM") ;"  U $I:(WIDTH=$S(X<256:X,1:0):FILTER="ESCAPE")  
 NEW %I,%T,ESC,DIM,SUCCESS
 SET %I=$I,%T=$T                                  
 SET ESC=$C(27)        
 USE $P:(NOESCAPE:NOECHO:TERM="R") 
 WRITE ESC,"7"          ;"//kt <-- save current cursor position
 WRITE ESC,"[r"         ;"//kt <-- enable scrolling for entire screen
 WRITE ESC,"[999;999H"  ;"//kt <-- set cursor positon to row 999, column 9999
 WRITE ESC,"[6n"        ;"//kt <-- request a cursor positon response from the device
 READ DIM:1             ;"//kt <-- example result for DIM is $C(27)_"[51;165"  Format is Y;X.  Also, the terminal is really sending $C(27)_"[51;165R", but we have specified R as line terminator 
 SET SUCCESS=($T=1)     ;"If read timeout, $T=0
 WRITE ESC,"8"          ;"//kt <-- restore cursor position after a cursor pos save
 DO RESTORDEV^TMGKERN1(.DEVSAV)  ;"this effects a USE $IO  
 IF (SUCCESS=0)!($LENGTH($GET(DIM))=0)!(DIM?.APC) GOTO AMGDN 
 SET DIM=+$PIECE(DIM,";",2)_"^"_+$PIECE(DIM,"[",2)   ;"<-- change format to X^Y
 ;" resize terminal device params to match actual terminal window dimensions
 USE $P:(WIDTH=+$P(DIM,"^",1):LENGTH=+$P(DIM,"^",2))  ;"//kt <--- should this be $IO instead of $P?
AMGDN ; 
 USE %I 
 IF %T
 QUIT:$Q $S($G(DIM):DIM,1:"") 
 QUIT
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
GBLOUTGREP(FPATHNAME,FILTER,OUT) ;"GLOBAL OUTPUT GREP 
  ;"Purpose: Scan HFS file and return strings based on filter
  ;"Input:   FPATHNAME -- The full name of the path on the HFS to file to scan
  ;"         FILTER -- PASS BY REFERENCE.  An array of items to scan for.  Each
  ;"              element of array will be used as pattern match (e.g. string?pattern), or
  ;"              be used to check if string contains element.  
  ;"              And if found, then the matching line and the following line from file will be returned
  ;"              First node must be: "?", or "["
  ;"            e.g. FILTER("?","3N1""-""2N1""-""4N")   pattern match to ###-##-####
  ;"                 FILTER("?","9N")   pattern match to #######
  ;"                 FILTER("[","GEORGE")   Check if contains to #######
  ;"            NOTE: if any of the filter entries match then the line is saved, so they are effectively OR statements 
  ;"         OUT  -- PASS BY REFERENCE.  Format:
  ;"             OUT(#)=<line from file)   # will the original line number from source file.
  ;"Results: 1^OK, or -1^Linux Error
  NEW CMD SET CMD="cat """_FPATHNAME_""""
  NEW P SET P="TEMP" 
  OPEN P:(COMMAND=CMD:readonly)::"pipe" 
  USE P
  NEW KEEPNEXT SET KEEPNEXT=0
  NEW LINENUM SET LINENUM=0
  KILL OUT NEW X,IDX SET IDX=1
  FOR   QUIT:$ZEOF  DO
  . READ LINE IF LINE=""&$ZEOF QUIT
  . SET LINENUM=LINENUM+1
  . IF KEEPNEXT SET KEEPNEXT=0 SET OUT(LINENUM)=LINE QUIT
  . NEW MATCHES SET MATCHES=0
  . ;"NEW SUBCT SET SUBCT=0.1
  . NEW MATCHTYPE FOR MATCHTYPE="?","[" DO
  . . NEW PATTERN SET PATTERN=""
  . . FOR  SET PATTERN=$ORDER(FILTER(MATCHTYPE,PATTERN)) QUIT:(PATTERN="")!MATCHES  DO
  . . . IF MATCHTYPE="?" SET MATCHES=LINE?@PATTERN
  . . . IF MATCHTYPE="[" SET MATCHES=LINE[PATTERN
  . . . ;"NOTE: if line below wanted, will need to pass in an ALL variable to fill.  
  . . . ;"SET ALL(LINENUM+SUBCT)="Pattern match of "_MATCHTYPE_" "_PATTERN_" -> "_MATCHES,SUBCT=SUBCT+0.1
  . . . IF MATCHES=0 QUIT
  . . . SET KEEPNEXT=1
  . . . SET OUT(LINENUM)=LINE
  CLOSE P USE $P
  NEW TEMP SET TEMP=$ZSYSTEM&255  ;"get result of execution. (low byte only)
  NEW RESULT SET RESULT=$SELECT(TEMP=0:"1^OK",1:"-1^Linux error")
  QUIT RESULT  
  ;
