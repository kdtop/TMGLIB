TMGKERN4 ;TMG/KST - Patch server stuff; 8/23/12, 2/2/14, 3/3/17
        ;;1.0;TMG-LIB;**1**;08/14/10;Build 1
       ;
 ;"TMG KERNEL FUNCTIONS
 ;"This code is related to getting patches from a remote patch server
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
 ;"GETPATL(PCKINIT,ARRAY,REFRESH,DIRFN) -- query remote server to get patches available for download
 ;"GETWDIR(URL,PARRAY) -- get dirctory from a web server
 ;"WGETFIL(URL,PARRAY,PMSG) -- Get a text file via unix wget command, storing in @PARRAY
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;
GETPATL(PCKINIT,ARRAY,REFRESH,DIRFN) ;
        ;"Purpose: To query remote server to get patches available for download
        ;"Input: PCKINIT -- this is the namespace of the package to get patches for, e.g. 'DI' for fileman
        ;"       ARRAY -- PASS BY REFERENCE.  An OUT parameter.  Format:
        ;"              Format: Array(0)=<header line> <-- ignored (if sent back)
        ;"                      Array(1)=URLPath  e.g. 'server.org/pub/download/DG/DG_53_P481.KID'
        ;"                      Array(2)=URLPath  e.g. 'server.org/pub/download/DG/DG_53_P482.KID'
        ;"                      Array(3)=URLPath  e.g. 'server.org/pub/download/DG/DG_53_P483.KID'
        ;"                      ...
        ;"       REFRESH -- IF 0 then no refresh needed, just SET PckDirFNAME (but ensure file exists)
        ;"       DIRFN -- Optional. PASS BY REFERNCE, an OUT PARAMETER. Filled with HFS filename of file
        ;"Result : 1=success, 0=failure
        NEW RESULT SET RESULT=1 ;"success
        KILL TMGARR
        NEW ARRCT SET ARRCT=1
        IF $GET(PCKINIT)="" SET RESULT=0 GOTO GPLDN
        SET REFRESH=+$GET(REFRESH)
        ;
        ;"Results will be stored in /<dir>/Remote_Patches-dirFor-'PCKINIT'
        NEW FNAME,FPATH,DEFPATH
        SET FPATH=$GET(^TMG("KIDS","PATCH DIR"))
        IF (FPATH="")!($$ISDIR^TMGKERNL(FPATH)=0) DO
        . NEW MSG SET MSG="Please choose a file path for storing VA patches in."
        . SET FPATH=$$GETDIRNM^TMGIOUTL(MSG,.DEFPATH,"/","Pick directory")
        . IF $$ISDIR^TMGKERNL(FPATH)=0 SET FPATH="" QUIT
        . SET ^TMG("KIDS","PATCH DIR")=FPATH
        IF FPATH="" SET RESULT=0 GOTO GPLDN
        SET FPATH=$$MKTRALDV^TMGIOUTL(FPATH)
        SET FNAME=$$PATCHDIRNAME^TMGPAT4(PCKINIT) ;
        SET PCKDIRFNAME=FPATH_FNAME
        IF $$ISDIR^TMGKERNL(PCKDIRFNAME)=0 DO
        . IF $$MKDIR^TMGKERNL(PCKDIRFNAME)
        IF $$ISDIR^TMGKERNL(PCKDIRFNAME)=0 DO
        . NEW MSG SET MSG="Unable to create directory: "_PCKDIRFNAME
        . DO PROGBAR^TMGUSRI2("Error:",MSG)
        . DO PRESS2GO^TMGUSRI2
        . SET RESULT=0
        IF (REFRESH'>0)!(RESULT=0) GOTO GPLDN
        NEW URLLIST,URL,URLBASE
        SET URLBASE="mirrors.medsphere.org/pub/downloads.va.gov/files/FOIA/Software/"
        SET URLLIST(1)=URLBASE_"Patches_By_Application/"
        SET URLLIST(2)=URLBASE_"Patches_By_Application/COMBINED%20PATCH%20KIDS%20BUILDS/"
        SET URLBASE=URLBASE_"VISTA_FOIA_Historical_Files/VISTA_FOIA_RELEASES_BEFORE_2008/"
        SET URLLIST(3)=URLBASE
        SET URLLIST(4)=URLBASE_"CSV%20-%20Builds%20of%20combined%20patches/"
        NEW CT SET CT=0
        FOR  SET CT=$ORDER(URLLIST(CT)) QUIT:CT=""  DO
        . SET URL=$GET(URLLIST(CT)) QUIT:URL=""
        . NEW DIR,MATCHES
        . DO GETWDIR(URL,"DIR")
        . NEW ENTRY SET ENTRY=""
        . FOR  SET ENTRY=$ORDER(DIR(ENTRY)) QUIT:ENTRY=""  DO
        . . NEW STR SET STR=$GET(DIR(ENTRY)) QUIT:STR=""
        . . NEW NAME SET NAME=$PIECE(STR,"^",1)
        . . NEW DIV SET DIV="-"
        . . IF NAME'["-" SET DIV=" "
        . . NEW INIT SET INIT=$$TRIM^XLFSTR($PIECE(NAME,DIV,1))
        . . IF INIT'=PCKINIT QUIT
        . . SET MATCHES(STR)=""
        . IF $DATA(MATCHES)=0 QUIT
        . SET ENTRY=""
        . FOR  SET ENTRY=$ORDER(MATCHES(ENTRY)) QUIT:ENTRY=""  DO
        . . NEW ONEDIR,AURL
        . . SET AURL=$PIECE(ENTRY,"^",2)
        . . DO GETWDIR(AURL,"ONEDIR")
        . . NEW FILECT SET FILECT=""
        . . FOR  SET FILECT=$ORDER(ONEDIR(FILECT)) QUIT:FILECT=""  DO
        . . . NEW FILE SET FILE=$GET(ONEDIR(FILECT)) QUIT:FILE=""
        . . . NEW URL SET URL=$PIECE(FILE,"^",2)
        . . . SET ARRAY(ARRCT)=URL SET ARRCT=ARRCT+1
GPLDN   QUIT RESULT
        ;"
GETWDIR(URL,PARRAY) ;
        ;"Purpose: To get dirctory from a web server
        ;"Input:  URL -- e.g. 'mirrors.medsphere.org/pub'
        ;"        PARRAY -- NAME of location to store text.  Prior results killed
        ;"            Format:
        ;"            @PARRAY@(#)=DisplayName^FullURLForDownload
        ;"        PMSG -- OPTIONAL.  NAME OF ARRAY.  If present, then messages/errors from wget returned in array
        ;"Result: none
        ;"Note: this is tested against a generic Apache web server only
        KILL @PARRAY
        IF $EXTRACT(URL,$LENGTH(URL))'="/" SET URL=URL_"/"
        NEW TEMPARR
        NEW CT SET CT=1
        NEW OPENAREF SET OPENAREF="<a href="""
        NEW CLOSEAREF SET CLOSEAREF="</a>"
        DO WGETFIL(URL,"TEMPARR")
        NEW LN SET LN=0
        FOR  SET LN=$ORDER(TEMPARR(LN)) QUIT:LN=""  DO
        . NEW STR SET STR=$GET(TEMPARR(LN)) QUIT:STR=""
        . IF STR'[OPENAREF QUIT
        . SET STR=$PIECE(STR,OPENAREF,2,999)
        . SET STR=$PIECE(STR,CLOSEAREF,1)
        . NEW REF SET REF=$PIECE(STR,""">",1)
        . IF $EXTRACT(REF,1)="?" QUIT
        . ;"SET REF=$$REPLSTR^TMGSTUT3(REF,"%20","\ ")
        . NEW NAME SET NAME=$PIECE(STR,""">",2)
        . IF NAME="Parent Directory" QUIT
        . SET REF=URL_REF
        . SET @PARRAY@(CT)=NAME_"^"_REF SET CT=CT+1
        QUIT
        ;
WGETFIL(URL,PARRAY,PMSG) ;
        ;"Purpose: Get a text file via unix wget command, storing in @PARRAY
        ;"Input:  URL -- e.g. 'mirrors.medsphere.org/pub'
        ;"        PARRAY -- NAME of location to store text.  Prior results killed
        ;"        PMSG -- OPTIONAL.  NAME OF ARRAY.  If present, then messages/errors from wget returned in array
        ;"Output:  PARRAY is filled as follows:
        ;"           @PARRAY@(1)=<first line of text file>
        ;"           @PARRAY@(2)=<second line of text file>
        ;"         PMSG (if passed) filled with messages (same format as PARRAY)
        ;"NOTE: PMSG not working --> maybe fix later...
        ;"     I found that IF the ERR input channel was empty, then the process stopped,
        ;"     even IF there is still information on STDIN
        ;"Result: none
        KILL @PARRAY
        NEW ERRARRAY ;"used IF nothing in PMSG
        SET PMSG=$GET(PMSG,"ERRARRAY")
        NEW STR,ESTR SET ESTR=""
        NEW P1 SET P1="WGetPipe"
        NEW ERR SET ERR="ERRCHAN"
        OPEN P1:(COMMAND="wget -O - """_URL_"""":NOWRAP:STDERR=ERR)::"pipe"
        NEW DONE SET DONE=0
        NEW CT,ECT SET (CT,ECT)=1
        NEW SAVING SET SAVING=0
        FOR  DO  QUIT:($ZEOF)!($ZA'=0)!(+$DEVICE>0)!DONE
        . NEW $ETRAP SET $ETRAP="SET DONE=1 USE $P SET $ETRAP="""" SET $ECODE="""" "
         . ;"use $P WRITE "."
        . USE P1
        . READ STR:1
        . ;"USE ERR
        . ;"READ ESTR:0
        . USE $P
        . IF STR'="" DO
        . . USE $P ;"WRITE STR,!
        . . SET @PARRAY@(CT)=STR SET CT=CT+1
        . IF ESTR'="" DO
        . . USE $P ;"WRITE "-->",ESTR,!
        . . SET @PMSG@(ECT)=ESTR SET ECT=ECT+1
        CLOSE P1
        ;"WRITE "GOODBYE",!
        USE $P
        QUIT
        ;
GetPckList(PCKINIT,Array,NeedsRefresh,PckDirFNAME)   ;"DEPRECIATED CODE
        ;"Purpose: Call Linux, launching special script to get patch file list from ftp.va.gov
        ;"         This is a support function for automating the KIDS installation of patches.
        ;"Input: PCKINIT -- this is the namespace of the package to get patches for, e.g. 'DI' for fileman
        ;"       Array -- PASS BY REFERENCE.  An OUT parameter.  Format:
        ;"              Format: Array(0)=<header line> <-- ignored
        ;"                      Array(#)=URLPath  e.g. 'server.org/pub/download/DG/DG_53_P481.KID'
        ;"                      Array(#)=URLPath  e.g. 'server.org/pub/download/DG/DG_53_P482.KID'
        ;"                      Array(#)=URLPath  e.g. 'server.org/pub/download/DG/DG_53_P483.KID'
        ;"       NeedsRefresh -- IF 0 then no refresh needed, just SET PckDirFNAME (but ensure file exists)
        ;"       PckDirFNAME -- Optional. PASS BY REFERNCE, an OUT PARAMETER. Filled with HFS filename of file
        ;"Result : 1=success, 0=failure

        NEW result SET result=1  ;"success
        KILL Array
        IF $GET(PCKINIT)="" SET result=0 GOTO GPLDone
        ;
        ;"Results will be stored in /<dir>/ftp.va.gov-dirFor-'PCKINIT'
        NEW FNAME,FPath
        ;"Fix this.... check IF path exists.....
        SET FPath=$GET(^TMG("KIDS","PATCH DIR"))
        IF (FPath="")!($$ISDIR^TMGKERNL(FPath)=0) do
        . NEW Msg SET Msg="Please choose a file path for storing VA patches in."
        . SET FPath=$$GETDIRNM^TMGIOUTL(Msg,.DefPath,"/","Pick directory")
        IF FPath="" SET result=0 GOTO GPLDone
        SET FNAME="ftp.va.gov-dirFor-"_PCKINIT
        SET PckDirFNAME=FPath_FNAME
        IF ($GET(NeedsRefresh)'>0)&($$FILEXIST^TMGIOUTL(PckDirFNAME)) GOTO GPLDone
        ;
        NEW FPScript SET FPScript=$GET(^TMG("KIDS","VA FTP Script"))
        IF (FPScript'=""),($$FILEXIST^TMGIOUTL(FPScript)=0) do
        . KILL ^TMG("KIDS","VA FTP Script")
        . SET FPScript=""
        IF FPScript="" do
        . NEW msg SET msg="Linux script needed: vaftp_launcher.sh\n"
        . SET msg=msg_"Please browse to this script and select it after the pause."
        . SET FPScript=$$GETFNAME^TMGIOUTL(msg,"/","vaftp_launcher.sh")
        . IF $$FILEXIST^TMGIOUTL(FPScript) do
        . . SET ^TMG("KIDS","VA FTP Script")=FPScript
        . ELSE  do
        . . WRITE "ERROR: Choice of "_FPScript_" is invalid.  Aborting."
        . . SET FPScript=""
        IF FPScript="" SET result=0 GOTO GPLDone
        ;
        NEW CmdStr SET CmdStr=FPScript_" "_PCKINIT_" "_FPath
        zsystem CmdStr  ;"Launch command in linux OS
        ;
        ;"get result of execution. (low byte only)  -- IF wanted
        NEW CmdResult SET CmdResult=$ZSYSTEM&255
        IF CmdResult'=0 do
        . ;"Failed, so get log file instead of results
        . SET FNAME="ftp.va.gov_log"
        . SET result=1  ;"success
        ;
GPL2    ;"Get results file (or log file IF problem)
        IF $$FTG^%ZISH(FPath,FNAME,"Array(0)",1)=0 SET result=0 GOTO GPLDone
        ;
GPLDone QUIT result
        ;
