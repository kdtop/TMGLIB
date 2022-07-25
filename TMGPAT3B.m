TMGPAT3B  ;TMG/kst/Patching tools ;7/30/22
         ;;1.0;TMG-LIB;**1**;7/30/22
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 7/303/22  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"The goal of this module is to provide an interface for rapid patch selection
 ;"  and installation.
 ;"
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"Private Functions
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;
 ;"NOTE: This module is a rewrite of TMGPAT3, using new resources
  ;
CONSOLE(USECACHE)  ;
  NEW ARRAY,OPTION
  SET OPTION("CURRENT PATH")="/"
  SET OPTION("SHOW PROGRESS")=1
  SET OPTION("USE CACHE")=+$GET(USECACHE)
  NEW DATA DO GETINFO^TMGPAT6(.DATA,.OPTION)
  MERGE OPTION("PATCH DATA")=DATA
  NEW BORDER SET BORDER="14^4"  ;"WHITE FG, BLUE BG
  SET OPTION("COLORS","NORM")="15^14"   ;"15=RED^14=GREY
  SET OPTION("COLORS","HEADER")=BORDER 
  SET OPTION("COLORS","TOP LINE")=BORDER
  SET OPTION("COLORS","FOOTER")=BORDER
  SET OPTION("COLORS","BOTTOM LINE")=BORDER
  ;"SET OPTION("COLORS","NORM")="15^15"  ;"RED ON WHITE
  SET OPTION("COLORS","READY")="7^9"  ;"WHITE ON GREEN
  SET OPTION("COLORS","NOT READY")="7^8" ;"WHITE ON RED
  SET OPTION("COLORS","PARTIAL")="7^9"  ;"  "14^10"  ;"WHITE ON YELLOW
  SET OPTION("COLORS","INSTALLED")="14^6"  ;"WHITE ON CYAN
  DO PREPAVAIL("ARRAY",.OPTION)
  SET OPTION("ON SELECT")="HNDONSEL^TMGPAT3B"
  SET OPTION("ON CMD")="HNDONCMD^TMGPAT3B"
  SET OPTION("ON KEYPRESS")="HNDONKP^TMGPAT3B"
  WRITE #
  DO SCROLLER^TMGUSRIF("ARRAY",.OPTION)
  QUIT
  ;
PREPAVAIL(PARRAY,OPTION)
  ;"Purpose: To prepair an array with patch status, for use with SCROLLER^TMGUSRIF
  ;"Input: PARRAY -- PASS BY NAME.  ARRAY to put info into.  Prior data is killed.
  ;"       OPTION -- PASS BY REFERENCE.  Prior data is NOT killed.  See SCROLLER^TMGUSRIF for details
  ;"                 OPTION("HIDE EMPTY")=0 OPTIONAL. Default is 0.  If 1 then, entries with no patches.
  ;"       OPTION("CUR DIR")="/"  E.g. "/", or "/YS/", or "/YS/5.01/"  DIR means directory, thinking of patches like a directory tree
  ;"Results: NONE
  KILL @PARRAY
  NEW SHOWINSTALLEDTOGGLE SET SHOWINSTALLEDTOGGLE=0
  NEW CT SET CT=1
  NEW CURDIR SET CURDIR=$GET(OPTION("CUR DIR"),"/")
  IF CURDIR="/" DO
  . NEW TOTALREADY SET TOTALREADY=0
  . NEW TOTALINSTALLED SET TOTALINSTALLED=0
  . NEW TOTALACTIONABLE SET TOTALACTIONABLE=0
  . NEW PCK SET PCK=""
  . FOR  SET PCK=$ORDER(OPTION("PATCH DATA",PCK)) QUIT:PCK=""  DO
  . . NEW DATA MERGE DATA=OPTION("PATCH DATA",PCK)
  . . NEW READY SET READY=+$GET(DATA("SUMMARY","INSTALLABLE"))
  . . SET TOTALREADY=TOTALREADY+READY
  . . NEW INSTALLED SET INSTALLED=+$GET(DATA("SUMMARY","INSTALLED"))
  . . SET TOTALINSTALLED=TOTALINSTALLED+INSTALLED
  . . NEW TOTAL SET TOTAL=+$GET(DATA("SUMMARY","TOTAL"))
  . . NEW ACTIONABLE SET ACTIONABLE=TOTAL-INSTALLED
  . . SET TOTALACTIONABLE=TOTALACTIONABLE+ACTIONABLE
  . . NEW LINE SET LINE=$$RJ^XLFSTR(PCK,4)_" - "_$$PCKNAME(PCK)
  . . SET LINE=$$LJ^XLFSTR(LINE,40)
  . . NEW COLOR SET COLOR="{{"_$SELECT(READY=TOTAL:"READY",READY>0:"PARTIAL",1:"BLOCKED")_"}}"
  . . NEW N1 SET N1=$$RJ^XLFSTR(READY,3)
  . . NEW N2 SET N2=$$RJ^XLFSTR(ACTIONABLE,3)
  . . SET LINE=LINE_"  "_COLOR_"("_N1_" of "_N2_" ready to install)"_"{{NORM}}"
  . . SET @PARRAY@(CT,LINE)="/"_PCK_"/",CT=CT+1
  . SET OPTION("HEADER",1)="TMG Patch Helper--  "_TOTALREADY_" of "_TOTALACTIONABLE_" Patches ready to be installed in all packages."
  ;
  IF $LENGTH(CURDIR,"/")=3 DO   ;"e.g. "/YS/"
  . NEW PCK SET PCK=$PIECE(CURDIR,"/",2)
  . NEW DATA MERGE DATA=OPTION("PATCH DATA",PCK)
  . NEW READY SET READY=$GET(DATA("SUMMARY","INSTALLABLE"))
  . NEW TOTAL SET TOTAL=$GET(DATA("SUMMARY","TOTAL"))
  . NEW INSTALLED SET INSTALLED=+$GET(DATA("SUMMARY","INSTALLED"))
  . NEW ACTIONABLE SET ACTIONABLE=TOTAL-INSTALLED  
  . NEW VER SET VER=""
  . FOR  SET VER=$ORDER(DATA(VER)) QUIT:VER'>0  DO
  . . NEW VERSTR SET VERSTR=VER IF $PIECE(VERSTR,".",2)="" SET $PIECE(VERSTR,".",2)="0"
  . . NEW LINE SET LINE=$$RJ^XLFSTR(PCK,4)_"- VERSION "_VERSTR
  . . SET LINE=$$LJ^XLFSTR(LINE,35)
  . . NEW COLOR SET COLOR="{{"_$SELECT(READY=TOTAL:"READY",READY>0:"PARTIAL",1:"BLOCKED")_"}}"
  . . NEW N1 SET N1=$$RJ^XLFSTR(READY,3)
  . . NEW N2 SET N2=$$RJ^XLFSTR(ACTIONABLE,3)
  . . SET LINE=LINE_"  "_COLOR_"("_N1_" of "_N2_" ready to install)"_"{{NORM}}"
  . . SET @PARRAY@(CT,LINE)="/"_PCK_"/"_VER_"/",CT=CT+1
  . SET OPTION("HEADER",1)="TMG Patch Helper-- Select VERSION of "_$$PCKNAME(PCK)_" to work on." 
  ;
  IF $LENGTH(CURDIR,"/")=4 DO   ;"e.g. "/YS/5.01/"
  . NEW PCK SET PCK=$PIECE(CURDIR,"/",2)
  . NEW VER SET VER=$PIECE(CURDIR,"/",3)
  . NEW VERSTR SET VERSTR=VER IF $PIECE(VERSTR,".",2)="" SET $PIECE(VERSTR,".",2)="0"
  . NEW SHOWINSTALLED SET SHOWINSTALLED=+$GET(OPTION("MODE","SHOW INSTALLED"))
  . NEW DONE SET DONE=0
  . FOR  DO  QUIT:DONE
  . . NEW SEQ SET SEQ=0
  . . FOR  SET SEQ=$ORDER(OPTION("PATCH DATA",PCK,VER,SEQ)) QUIT:SEQ'>0  DO
  . . . NEW DATA MERGE DATA=OPTION("PATCH DATA",PCK,VER,SEQ)
  . . . NEW NAME SET NAME=$GET(DATA("NAME"))
  . . . NEW STATUS SET STATUS=$GET(DATA("INSTALL","STATUS"))
  . . . NEW COLOR SET COLOR="{{"_STATUS_"}}"
  . . . NEW LINE SET LINE="SEQ# "_$$RJ^XLFSTR(SEQ,4)_" - "_NAME
  . . . SET LINE=$$LJ^XLFSTR(LINE,30)
  . . . NEW DESCR SET DESCR=""
  . . . IF (STATUS="INSTALLED") DO  QUIT:(SHOWINSTALLED=0)
  . . . . SET DESCR="(ALREADY INSTALLED)"
  . . . IF (STATUS="READY") DO
  . . . . SET DESCR="READY FOR INSTALLATION"
  . . . IF (STATUS="NOT READY") DO
  . . . . NEW DEPLINE SET DEPLINE=""
  . . . . NEW DEP SET DEP=""
  . . . . FOR  SET DEP=$ORDER(DATA("KIDS","DEPENDENCIES",DEP)) QUIT:DEP=""  DO
  . . . . . NEW INSTALLED SET INSTALLED=+$GET(DATA("KIDS","DEPENDENCIES",DEP))
  . . . . . IF INSTALLED QUIT
  . . . . . IF DEPLINE'="" SET DEPLINE=DEPLINE_", "
  . . . . . SET DEPLINE=DEPLINE_DEP
  . . . . SET DESCR="NEEDS: "_DEPLINE
  . . . SET LINE=LINE_" "_COLOR_$$LJ^XLFSTR(DESCR,40)_"{{NORM}}"
  . . . NEW VALUE SET VALUE=$SELECT(STATUS="READY":"OK",1:"BLOCKED")
  . . . SET VALUE=VALUE_"^"_CURDIR_NAME_"^"_PCK_"^"_VER_"^"_SEQ
  . . . SET @PARRAY@(CT,LINE)=VALUE,CT=CT+1
  . . IF (CT>1)!(SHOWINSTALLED) SET DONE=1
  . . SET SHOWINSTALLED=1 QUIT  ;"If no lines shown, then try showing installed.
  . SET SHOWINSTALLEDTOGGLE=1 
  . SET OPTION("HEADER",1)="TMG Patch Helper-- Select PATCH to install" 
  SET OPTION("FOOTER",1,1)="^ Exit"
  SET OPTION("FOOTER",1,2)="? Help"
  IF SHOWINSTALLEDTOGGLE DO
  . NEW SHOWINSTALLED SET SHOWINSTALLED=+$GET(OPTION("MODE","SHOW INSTALLED"))
  . NEW DISP SET DISP=$SELECT(SHOWINSTALLED:"Hide",1:"Show")_" Installed"
  . SET OPTION("FOOTER",1,3)="[F1] "_DISP
  ELSE  DO
  . KILL OPTION("FOOTER",1,3)
PAVDN  ;
  QUIT
  ;
PCKNAME(PCKINIT) ;"Return package full name        
  NEW PCKIEN SET PCKIEN=+$ORDER(^DIC(9.4,"C",PCKINIT,0))
  NEW PCKZN SET PCKZN=$GET(^DIC(9.4,PCKIEN,0),"<NEW PACKAGE>^?")
  QUIT $PIECE(PCKZN,"^",1)
  ;
HNDONKP(PARRAY,OPTION,INFO)
  ;"Purpose: handle ON SELECT event from SCROLLER^TMGUSRIF
  ;"Input: PARRAY,OPTION,INFO -- see documentation in SCROLLER^TMGUSRIF
  DO HNDONCMD(.PARRAY,.OPTION,.INFO)
  QUIT
  ;
HNDONCMD(PARRAY,OPTION,INFO)  ;
  ;"Purpose: handle ON SELECT event from Scroller
  ;"Input: PARRAY,OPTION,INFO -- see documentation in Scroller
  ;"       INFO has this:
  ;"          INFO("USER INPUT")=INPUT
  ;"          INFO("CURRENT LINE","NUMBER")=number currently highlighted line
  ;"          INFO("CURRENT LINE","TEXT")=Text of currently highlighted line
  ;"          INFO("CURRENT LINE","RETURN")=return value of currently highlighted line
  NEW INPUT SET INPUT=$$UP^XLFSTR($GET(INFO("USER INPUT")))
  NEW CMD SET CMD=$$UP^XLFSTR($GET(INFO("CMD")))
  IF INPUT["{F1}" DO
  . NEW SHOWINSTALLED SET SHOWINSTALLED=+$GET(OPTION("MODE","SHOW INSTALLED"))
  . SET OPTION("MODE","SHOW INSTALLED")='SHOWINSTALLED
  IF INPUT["{LEFT}" DO
  . NEW DIR SET DIR=OPTION("CUR DIR")
  . SET DIR=$$UPPATH^TMGIOUTL(DIR)
  . SET OPTION("CUR DIR")=DIR
  IF INPUT["{RIGHT}" DO
  . DO HNDONSEL(PARRAY,.OPTION,.INFO)        
  ELSE  IF INPUT="RESCAN" DO
  . NEW CACHE SET CACHE=+$GET(OPTION("USE CACHE"))
  . SET OPTION("USE CACHE")=0
  . NEW TEMP DO GETINFO^TMGPAT6(.TEMP,.OPTION)
  . KILL OPTION("PATCH DATA") MERGE OPTION("PATCH DATA")=TEMP
  . SET OPTION("USE CACHE")=CACHE
  ELSE  IF INPUT="?" DO
  . WRITE !,"Use UP and DOWN cursor keys to select package, then ENTER to work on.",!
  . WRITE "Enter 'NEWPACK' to install a NEW package.",!
  . WRITE "Enter 'RESCAN' to rescan the patch repository server",!
  . WRITE "Enter ^ at the ':' prompt to QUIT",!
  . DO PRESS2GO^TMGUSRI2
  ;" ELSE  IF INPUT'="" DO
  ;" . WRITE !,"Input ",$GET(INFO("USER INPUT"))," not recognized.",!
  ;" . DO PRESS2GO^TMGUSRI2
  ;
  DO PREPAVAIL(PARRAY,.OPTION)
  WRITE #
  QUIT
  ;
HNDONSEL(PARRAY,OPTION,INFO)
  ;"Purpose: handle ON SELECT event from SCROLLER^TMGUSRIF
  ;"Input: PARRAY,OPTION,INFO -- see documentation in SCROLLER^TMGUSRIF
  ;"       INFO has this:
  ;"          INFO("CURRENT LINE","NUMBER")=number currently highlighted line
  ;"          INFO("CURRENT LINE","TEXT")=Text of currently highlighted line
  ;"          INFO("CURRENT LINE","RETURN")=return value of currently highlighted line
  NEW RESULT SET RESULT="1^OK"
  NEW SELECTION SET SELECTION=$GET(INFO("CURRENT LINE","RETURN"))
  NEW L SET L=$LENGTH(SELECTION,"/")  ;"L = number of nodes in path with / delimiter
  IF (SELECTION'["^"),((L=3)!(L=4)) DO   ;"e.g. "/YS/" or "/YS/5.01/"
  . SET OPTION("CUR DIR")=SELECTION
  . DO PREPAVAIL(PARRAY,.OPTION)
  IF SELECTION["^" DO  GOTO HOSDN     ;"when final patch is picked, SELECTION contains "^" pieces
  . ;"FORMAT: [OK|BLOCKED]^<PATCH PATH>^<PCK>^<VER>^<SEQ>
  . NEW CMD SET CMD=$PIECE(SELECTION,"^",1)
  . NEW PATHNAME SET PATHNAME=$PIECE(SELECTION,"^",2)
  . NEW NAME DO SPLITFPN^TMGIOUTL(PATHNAME,,.NAME)
  . NEW PCK SET PCK=$PIECE(SELECTION,"^",3)
  . NEW VER SET VER=$PIECE(SELECTION,"^",4)
  . NEW SEQ SET SEQ=$PIECE(SELECTION,"^",5)
  . NEW DATA MERGE DATA=OPTION("PATCH DATA",PCK,VER,SEQ)
  . IF CMD="BLOCKED" DO  QUIT
  . . SET RESULT=$$HNDBLOCKED(NAME,.DATA) 
  . IF CMD="OK" DO  QUIT
  . . SET RESULT=$$HNDOK(NAME,.DATA,.OPTION)
  . . DO PREPAVAIL(PARRAY,.OPTION)  
  . WRITE "?? The line selected doesn't specify any command ??",!
  . DO PRESS2GO^TMGUSRI2
HOSDN ;
  ;"DO PRESS2GO^TMGUSRI2
  ;"WRITE #
  QUIT
  ;  
 ;"=======================================================================
 ;"=======================================================================
HNDBLOCKED(NAME,DATA)  ;
        
  QUIT
  ;
HNDOK(NAME,DATA,OPTION)  ;
  ;"Input: NAME -- Name of patch, e.g. "ABSV*4.0*45"
  ;"       DATA -- passed by reference.  Relevent data for patch.  E.g.
  ;"            DATA
  ;"            }~INFOTXT
  ;"            | }~4,3,1,
  ;"            | | }~CONTAINED PATCHES
  ;"            | | | }~"ABSV*4.0*45 SEQ #44" = 0
  ;"            | | }~"NAME" = ABSV-4_SEQ-44_PAT-45.txt
  ;"            | | }~"URL" = foia-vista.worldvista.org/Patches_By_Application/ABSV-Voluntary Service/ABSV-4_SEQ-44_PAT-45.txt
  ;"            | }~CONTAINED PATCHES
  ;"            |   }~"ABSV*4.0*45 SEQ #44" = 0
  ;"            }~INSTALL
  ;"            | }~"INSTALLABLE" = 1
  ;"            | }~"INSTALLED" = 0                    ATA
  ;"            | }~"PENDING DEPENDENCIES" = 0
  ;"            | }~"STATUS" = READY
  ;"            }~KIDS
  ;"            | }~3,3,1,
  ;"            | | }~CONTAINED PATCHES
  ;"            | | | }~"ABSV*4.0*45 SEQ #44" = 0
  ;"            | | }~"NAME" = ABSV-4_SEQ-44_PAT-45.kids
  ;"            | | }~"URL" = foia-vista.worldvista.org/Patches_By_Application/ABSV-Voluntary Service/ABSV-4_SEQ-44_PAT-45.kids
  ;"            | }~CONTAINED PATCHES
  ;"            | | }~"ABSV*4.0*45 SEQ #44" = 0
  ;"            | }~"PENDING DEPENDENCIES" = 0
  ;"            }~"NAME" = ABSV*4.0*45
  ;"            }~"PACKAGE" = ABSV
  ;"            }~"PATCH#" = 45
  ;"            }~"SEQ#" = 44
  ;"            }~"VER" = 4
  ;"            }~"VERSTR" = 4.0
  ;"        OPTION -- PASS BY REFERENCE.  NOTE: This contains all the patch data.
  ;"              If a patch is installed here, then OPTION("PATCH DATA") will be refereshed.   
  ;"RESULTS: none.
  ;
  NEW MENU,IDX,USRPICK,IENS,ADDED
  NEW INFOTEXTVIEWED SET INFOTEXTVIEWED=0
  NEW INFORMATIONALONLY SET INFORMATIONALONLY=1
  NEW HASTXT,HASKIDS SET (HASTXT,HASKIDS)=0
  NEW DONE SET DONE=0
HOL1 ;                    
  SET IDX=0  
  KILL MENU,ADDED SET MENU(IDX)="Select Option For Patch"
  SET MENU(IDX,1)=NAME
  SET IENS=""
  FOR  SET IENS=$ORDER(DATA("INFOTXT",IENS)) QUIT:IENS=""  DO
  . NEW URL SET URL=$GET(DATA("INFOTXT",IENS,"URL")) QUIT:URL=""
  . NEW TXTNAME DO SPLITFPN^TMGIOUTL(URL,,.TXTNAME) QUIT:TXTNAME=""
  . IF $DATA(ADDED(TXTNAME)) QUIT
  . SET IDX=IDX+1,MENU(IDX)="VIEW: "_TXTNAME_$CHAR(9)_"VIEW^"_URL_"^"_TXTNAME
  . SET ADDED(TXTNAME)=""
  . SET HASTXT=1
  SET IENS=""
  FOR  SET IENS=$ORDER(DATA("KIDS",IENS)) QUIT:IENS=""  DO
  . NEW URL SET URL=$GET(DATA("KIDS",IENS,"URL")) QUIT:URL=""
  . NEW KIDNAME DO SPLITFPN^TMGIOUTL(URL,,.KIDNAME) QUIT:KIDNAME=""
  . IF $DATA(ADDED(KIDNAME)) QUIT
  . SET IDX=IDX+1,MENU(IDX)="INSTALL: "_KIDNAME_$CHAR(9)_"INSTALL^"_URL_"^"_KIDNAME_"^"_IENS  
  . SET ADDED(KIDNAME)=""
  . SET HASKIDS=1
  SET IDX=IDX+1,MENU(IDX)="Display DATA for patch"_$CHAR(9)_"SHOW DATA"
  SET INFORMATIONALONLY=($GET(DATA("INFOTXT","INFORMATIONAL ONLY"))="YES")
  IF INFORMATIONALONLY!((HASKIDS=0)&INFOTEXTVIEWED) DO
  . NEW LINE SET LINE=""
  . IF INFORMATIONALONLY SET LINE="INFORMATIONAL ONLY. "
  . SET IDX=IDX+1,MENU(IDX)=LINE_"SET PATCH AS 'INSTALLED'"_$CHAR(9)_"PSEUDOPATCH"  
  ;"=======================================
  SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
  IF USRPICK="^" GOTO TMDN      
  NEW CMD SET CMD=$PIECE(USRPICK,"^",1)
  NEW URL SET URL=$PIECE(USRPICK,"^",2)
  NEW FNAME SET FNAME=$PIECE(USRPICK,"^",3)
  IF CMD="VIEW" DO  GOTO HOL1
  . NEW TEMP,INFO,MSG
  . MERGE TEMP=DATA KILL TEMP("KIDS")  ;"Prevent downloading potentially large KIDS file just to view TXT file. 
  . DO ENSRLOCL2^TMGPAT2B(.TEMP,.INFO,.MSG,.OPTION)
  . DO CHECKENSR(.INFO,"TEXT FILE",FNAME)      
  . IF $DATA(MSG) DO SHOWMSG^TMGPAT2B(.MSG) QUIT
  . IF $GET(INFO("TEXT FILE"))="" QUIT
  . IF $$EDITHFSFILE^TMGKERNL(INFO("TEXT FILE"))  ;"ignore result
  . SET INFOTEXTVIEWED=1
  IF CMD="INSTALL" DO  GOTO TMDN:DONE,HOL1
  . NEW TEMP,INFO,MSG
  . MERGE TEMP=DATA 
  . DO ENSRLOCL2^TMGPAT2B(.TEMP,.INFO,.MSG,.OPTION)    
  . ;"DO CHECKENSR(.INFO,"KID FILE",FNAME) 
  . IF $DATA(MSG) DO SHOWMSG^TMGPAT2B(.MSG) QUIT       
  . SET %=2 WRITE !,"Install patch: ",FNAME DO YN^DICN WRITE !
  . IF %'=1 QUIT
  . NEW INFO2 SET INFO2("PATH")=$GET(INFO("PATH")),INFO2("KID FILE")=FNAME
  . NEW TEMP,TXTFNAME SET TEMP=$GET(INFO("TEXT FILE")) DO SPLITFPN^TMGIOUTL(TEMP,,.TXTFNAME)
  . SET INFO2("TEXT FILE")=TXTFNAME
  . NEW OPT2,SUCCESS SET SUCCESS=$$GO^TMGPAT1(.OPT2,.INFO2,.MSG)
  . IF $DATA(MSG) DO SHOWMSG^TMGPAT2B(.MSG)
  . IF SUCCESS=0 QUIT
  . DO REFRESHPCK(PCK,.OPTION) ;"Refresh just PCK portion of patch data
  . ;"SET OPTION("CUR DIR")="/"  
  . SET DONE=1
  IF CMD="SHOW DATA" DO  GOTO HOL1
  . IF $DATA(DATA)="" QUIT
  . DO ARRDUMP^TMGMISC3("DATA")
  . DO PRESS2GO^TMGUSRI2
  IF CMD="PSEUDOPATCH" DO  GOTO TMDN:DONE,HOL1
  . NEW PCK SET PCK=$GET(DATA("PACKAGE"))
  . NEW VER SET VER=$GET(DATA("VER"))
  . NEW SEQ SET SEQ=$GET(DATA("SEQ#"))
  . NEW PNUM SET PNUM=$GET(DATA("PATCH#"))
  . IF (PCK="")!(VER="")!(SEQ="")!(PNUM="") DO  QUIT
  . . WRITE "Missing information: PCK=[",PCK,"], VER=[",VER,"], SEQ=[",SEQ,"], PNUM=[",PNUM,"]",!
  . . DO PRESS2GO^TMGUSRI2
  . SET SUCCESS=$$MAKEPATCHENTRY^TMGPAT2B(PCK,VER,SEQ,PNUM,.MSG)
  . IF $DATA(MSG) DO SHOWMSG^TMGPAT2B(.MSG)
  . IF SUCCESS=0 QUIT
  . DO REFRESHPCK(PCK,.OPTION) ;"Refresh just PCK portion of patch data
  . ;"SET OPTION("CUR DIR")="/"  
  . SET DONE=1
  GOTO HOL1
TMDN  ;
  QUIT
  ; 
REFRESHPCK(PCK,OPTION) ;"Refresh just PCK portion of patch data
  NEW PCKDATA DO GETPINFO^TMGPAT6(.PCKDATA,PCK)
  KILL OPTION("PATCH DATA",PCK)
  MERGE OPTION("PATCH DATA")=PCKDATA
  QUIT
  ;
CHECKENSR(INFO,TARGET,FNAME)  ;
  NEW RESULT SET RESULT=1     
  NEW PATH SET PATH=$GET(INFO("PATH"))
  NEW FPATHNAME SET FPATHNAME=PATH_FNAME
  IF $GET(INFO(TARGET))'=FPATHNAME DO                                      
  . NEW IDX,DONE SET (IDX,DONE)=0
  . FOR  SET IDX=$ORDER(INFO(TARGET,IDX)) QUIT:(IDX'>0)!DONE  DO
  . . NEW AFPATHNAME SET AFPATHNAME=$GET(INFO(TARGET,IDX)) QUIT:AFPATHNAME=""     
  . . IF AFPATHNAME'=FPATHNAME KILL INFO(TARGET,IDX) QUIT                        
  . . SET INFO(TARGET)=AFPATHNAME
  QUIT RESULT
