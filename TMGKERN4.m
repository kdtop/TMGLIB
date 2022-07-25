TMGKERN4 ;TMG/KST - Patch server stuff; 8/23/12,6/29/22
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
 ;"WEBBROWSE ;
 ;"GETPATL(PCKINIT,ARRAY,REFRESH,DIRFN) -- query remote server to get patches available for download
 ;"GETWDIR(URL,PARRAY) -- get dirctory from a web server
 ;"WGETFIL(URL,PARRAY,PMSG) -- Get a text file via unix wget command, storing in @PARRAY
 ;"SCAN() -- Scan web server holding all patches, and parse all into Fileman file
 ;"URLBASE() -- get name of web server that is mirror of VA's directories
 ;
 ;"=======================================================================
 ;" API -- Private Functions.
 ;"=======================================================================
 ;"PATCHBASE() -- get name of web server's /Patches_By_Application/                                       
 ;"HFSPATCHBASE() ;
 ;"GETURLS(URLLIST) ;
 ;"GetPckList(PCKINIT,ARR,NEEDSREFRESH,PCKGDIRFNAME) -- DEPRECIATED CODE
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;
WEBBROWSE() ;
  NEW OPTION
  SET OPTION("MSG")="Browsing files on a web server"
  SET OPTION("URL")="foia-vista.worldvista.org/Patches_By_Application/"
  SET OPTION("SELECT DIR")=0
  WRITE $$FBROWSE^TMGIOUT2(.OPTION)
  QUIT
  ;
URLBASE() ;
  ;" mirrors.medsphere.org/pub/downloads.va.gov/files/FOIA/Software/  <--- no longer valid URL
  ;" foia-vista.osehra.org/     <--- no longer valid URL        
  QUIT "foia-vista.worldvista.org/"
  ;  
PATCHBASE() ;                                      
  NEW BASE SET BASE=$$URLBASE()
  QUIT BASE_"Patches_By_Application/"                  
  ;
HFSPATCHBASE() ;
  QUIT "/opt/worldvista/EHR/kids"
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
  ;"       REFRESH -- IF 0 then no refresh needed, just SET PCKGDIRFNAME (but ensure file exists)
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
  NEW URLLIST,URL
  DO GETURLS(.URLLIST)
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
GPLDN ;
  QUIT RESULT
  ;
GETURLS(URLLIST) ;
  NEW URLBASE SET URLBASE=$$URLBASE()        
  SET URLLIST(1)=$$PATCHBASE()
  SET URLLIST(2)=$$PATCHBASE()_"COMBINED%20PATCH%20KIDS%20BUILDS/"
  SET URLBASE=$$URLBASE()_"VISTA_FOIA_Historical_Files/VISTA_FOIA_RELEASES_BEFORE_2008/"
  SET URLLIST(3)=URLBASE
  SET URLLIST(4)=URLBASE_"CSV%20-%20Builds%20of%20combined%20patches/"
  QUIT
  ;
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
  NEW HTMLSTR SET HTMLSTR=""
  NEW LN SET LN=0
  FOR  SET LN=$ORDER(TEMPARR(LN)) QUIT:LN=""  SET HTMLSTR=HTMLSTR_$GET(TEMPARR(LN))        
  FOR  QUIT:HTMLSTR=""  DO
  . NEW STRB
  . IF HTMLSTR'[OPENAREF SET HTMLSTR="" QUIT
  . NEW WORKSTR SET WORKSTR=$PIECE(HTMLSTR,OPENAREF,2,999)
  . SET WORKSTR=$PIECE(WORKSTR,CLOSEAREF,1)
  . SET HTMLSTR=$PIECE(HTMLSTR,CLOSEAREF,2,999)
  . NEW REF SET REF=$PIECE(WORKSTR,">",1)
  . SET REF=$PIECE(REF,"""",1)
  . IF $EXTRACT(REF,1)="?" QUIT
  . IF (REF="")!(REF="/") QUIT
  . NEW UREF SET UREF=$$UP^XLFSTR(REF)
  . IF (UREF["HTTP://")!(UREF["MAILTO:") QUIT
  . ;"SET REF=$$REPLSTR^TMGSTUT3(REF,"%20","\ ")
  . NEW NAME SET NAME=$PIECE(WORKSTR,""">",2)
  . IF NAME="Parent Directory" QUIT
  . IF $EXTRACT(NAME,1,4)="<img" QUIT
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
GetPckList(PCKINIT,ARR,NEEDSREFRESH,PCKGDIRFNAME)   ;"DEPRECIATED CODE
   ;"as of 6/13/22, I can't find anyone calling this code.  Can delete later...
  ;"Purpose: Call Linux, launching special script to get patch file list from ftp.va.gov
  ;"         This is a support function for automating the KIDS installation of patches.
  ;"Input: PCKINIT -- this is the namespace of the package to get patches for, e.g. 'DI' for fileman
  ;"       ARR -- PASS BY REFERENCE.  An OUT parameter.  Format:
  ;"              Format: ARR(0)=<header line> <-- ignored
  ;"                      ARR(#)=URLPath  e.g. 'server.org/pub/download/DG/DG_53_P481.KID'
  ;"                      ARR(#)=URLPath  e.g. 'server.org/pub/download/DG/DG_53_P482.KID'
  ;"                      ARR(#)=URLPath  e.g. 'server.org/pub/download/DG/DG_53_P483.KID'
  ;"       NEEDSREFRESH -- IF 0 then no refresh needed, just SET PCKGDIRFNAME (but ensure file exists)
  ;"       PCKGDIRFNAME -- Optional. PASS BY REFERNCE, an OUT PARAMETER. Filled with HFS filename of file
  ;"Result : 1=success, 0=failure

  NEW RESULT SET RESULT=1  ;"success                 
  KILL ARR
  IF $GET(PCKINIT)="" SET RESULT=0 GOTO GPLDNE
  ;
  ;"Results will be stored in /<dir>/ftp.va.gov-dirFor-'PCKINIT'
  NEW FNAME,FPATH
  ;"Fix this.... check IF path exists.....
  SET FPATH=$GET(^TMG("KIDS","PATCH DIR"))                
  IF (FPATH="")!($$ISDIR^TMGKERNL(FPATH)=0) do
  . NEW MSG SET MSG="Please choose a file path for storing VA patches in."
  . SET FPATH=$$GETDIRNM^TMGIOUTL(MSG,.DefPath,"/","Pick directory")
  IF FPATH="" SET RESULT=0 GOTO GPLDNE
  SET FNAME="ftp.va.gov-dirFor-"_PCKINIT
  SET PCKGDIRFNAME=FPATH_FNAME
  IF ($GET(NEEDSREFRESH)'>0)&($$FILEXIST^TMGIOUTL(PCKGDIRFNAME)) GOTO GPLDNE
  ;
  NEW FPSCRIPT SET FPSCRIPT=$GET(^TMG("KIDS","VA FTP Script"))
  IF (FPSCRIPT'=""),($$FILEXIST^TMGIOUTL(FPSCRIPT)=0) do
  . KILL ^TMG("KIDS","VA FTP Script")
  . SET FPSCRIPT=""                      
  IF FPSCRIPT="" DO
  . NEW MSG SET MSG="Linux script needed: vaftp_launcher.sh\n"
  . SET MSG=MSG_"Please browse to this script and select it after the pause."
  . SET FPSCRIPT=$$GETFNAME^TMGIOUTL(MSG,"/","vaftp_launcher.sh")
  . IF $$FILEXIST^TMGIOUTL(FPSCRIPT) DO
  . . SET ^TMG("KIDS","VA FTP Script")=FPSCRIPT
  . ELSE  DO
  . . WRITE "ERROR: Choice of "_FPSCRIPT_" is invalid.  Aborting."
  . . SET FPSCRIPT=""
  IF FPSCRIPT="" SET RESULT=0 GOTO GPLDNE
  ;
  NEW CMDSTR SET CMDSTR=FPSCRIPT_" "_PCKINIT_" "_FPATH
  ZSYSTEM CMDSTR  ;"Launch command in linux OS
  ;                                                     
  ;"get result of execution. (low byte only)  -- IF wanted
  NEW CMDRESULT SET CMDRESULT=$ZSYSTEM&255
  IF CMDRESULT'=0 DO     ;"Failed, so get log file instead of results
  . SET FNAME="ftp.va.gov_log"
  . SET RESULT=1  ;"success
GPL2    ;"Get results file (or log file IF problem)
  IF $$FTG^%ZISH(FPATH,FNAME,"ARR(0)",1)=0 SET RESULT=0 GOTO GPLDNE
GPLDNE ;
  QUIT RESULT
  ;
SCAN() ;" Scan web server holding all patches, and parse all into Fileman file
  DO SCAN^TMGPAT6
  QUIT 