TMGIOUT2 ;TMG/kst/IO Utilities -- File browser ;05/16/09; ... 4/24/15
         ;;1.0;TMG-LIB;**1**;05/16/09
 ;
 ;"TMG IO UTILITIES
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"NOTE: THIS CODE IS NOW SACC COMPLIANT.  Keep it that way!  :-)  
 ;"7/22/15 (NOTE: prob too long now, needs recheck)
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"FBROWSE(OPTION,OUTPATH,OUTNAME) query the user to select a filename, either from local file system or web file server
 ;"CALLER(CODE) -- From call stack, return the location of the caller of the function
 ;"=======================================================================
 ;"Private API calls         
 ;"=======================================================================
 ;"LOADDIR(PARRAY,CURDIR,TMGMASK,OPTION) -- load CURDIR entries into PARRAY
 ;"HNDONSEL(PARRAY,OPTION,INFO) -- handle ON SELECT event from Scroller
 ;"HNDONCMD(PARRAY,OPTION,INFO) -- handle ON CMD event from Scroller
 ;"SHOWHELP -- show help for file browser
 ;
 ;"=======================================================================
 ;"Dependencies:
 ;"   TMGKERNL*, TMGIOUTL, TMGURSIF, %ZISH, XLFSTR
 ;"=======================================================================
 ;"=======================================================================
 ;
TEST ;
  NEW OPTION
  SET OPTION("MSG")="Hello there!"
  SET OPTION("PATH")="/home/"
  SET OPTION("SELECT DIR")=0
  WRITE $$FBROWSE(.OPTION)
  QUIT
  ;
TESTURL ;
  NEW OPTION
  SET OPTION("MSG")="Browsing files on a web server"
  SET OPTION("URL")="foia-vista.worldvista.org/Patches_By_Application/"
  SET OPTION("SELECT DIR")=0
  WRITE $$FBROWSE(.OPTION)
  QUIT
  ;  
FBROWSE(OPTION,OUTPATH,OUTNAME) ;
  ;"SCOPE: PUBLIC
  ;"Purpose: To query the user, to get a filename back
  ;"          Supplies optional directory listing.
  ;"Input: OPTION [OPTIONAL].  Format as follows.  All entries are optional
  ;"           OPTION("MSG") A message to show user prior to name prompt.
  ;"                         May contain "\n" character for line wrapping.
  ;"           OPTION("HEADER MSG",1) A message to show user in scroller header
  ;"           OPTION("HEADER MSG",2) a 2nd line message to show user in scroller header, etc...
  ;"           OPTION("PATH") Initial default PATH
  ;"           OPTION("URL") Initial web PATH <--- if provided, then "PATH" is ignored.  
  ;"           OPTION("NAME") Initial default filename
  ;"           OPTION("NodeDiv") The character that separates folders (e.g. "/")
  ;"                             If not supplied, then default value is "/"
  ;"           OPTION("MATCH","CASESPEC")=1" <-- means case specific.  Default is NON-specific
  ;"           OPTION("MATCH","*.m")="" -- e.g. use filter '*.m'
  ;"           OPTION("MATCH","*.txt")="" -- e.g. use filter '*.txt"
  ;"               NOTE: files matching ANY one of the specified matches allows display
  ;"           OPTION("PROMPT") A prompt for user to enter filename/directory name
  ;"           OPTION("SHOW HIDDEN")=1  Show files hidden (e.g. '.name')
  ;"           OPTION("SELECT DIR")=1  if 1 then mode is to select directories, not files
  ;"        OUTPATH: [OPTIONAL] Pass by reference, filled with selected PATH
  ;"        OUTNAME: [OPTIONAL] Pass by reference, filled with selected name
  ;"Result: returns user specified filename (with PATH), or "" IF aborted
  ;
  WRITE # ;"clear screen
  WRITE "Loading..."
  NEW SCRLFILES,DONE
  NEW SELDIR SET SELDIR=+$GET(OPTION("SELECT DIR"))
  NEW WIDTH SET WIDTH=70     
  NEW LINE SET $PIECE(LINE,"-",WIDTH-2)="-"
  NEW SPACES SET $PIECE(SPACES," ",WIDTH-2)=" "
  NEW HEADERLN SET HEADERLN=1
  SET OPTION("HEADER",HEADERLN)="+"_LINE_"+",HEADERLN=HEADERLN+1
  NEW BANNER SET BANNER="--== Please Select "_$SELECT(SELDIR:"Directory",1:"File")_" ==--"
  SET OPTION("HEADER",HEADERLN)="|"_$$CJ^XLFSTR(BANNER,WIDTH-2)_"|",HEADERLN=HEADERLN+1
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(OPTION("HEADER MSG",IDX)) QUIT:IDX'>0  DO
  . NEW LINE SET LINE=$GET(OPTION("HEADER MSG",IDX)) KILL OPTION("HEADER MSG",IDX)
  . NEW LEN SET LEN=$$NOCOLEN^TMGUSRIF(LINE)
  . IF LEN>(WIDTH-2) DO
  . . IF $$HASCOLOR^TMGUSRIF(LINE) SET LINE=$$STRIPCOLOR^TMGUSRIF(LINE) ;"strip any color tags to avoid breaking with trim
  . . SET LINE=$EXTRACT(LINE,1,WIDTH-5)_"..."
  . . SET LEN=$LENGTH(LINE)
  . NEW SPN,SPN1,SPN2
  . SET SPN=WIDTH-2-LEN
  . SET SPN1=SPN\2
  . SET SPN2=SPN-SPN1
  . SET LINE="|"_$EXTRACT(SPACES,1,SPN1)_LINE_$EXTRACT(SPACES,1,SPN2)_"|"
  . SET OPTION("HEADER",HEADERLN)=LINE,HEADERLN=HEADERLN+1
  SET OPTION("FOOTER",1)="Enter ? for help"
  DO DISPFILT(.OPTION)  ;"SET OPTION ARRAY TO PROPERLY DISPLAY CURRENT FILTERS  
  NEW FOOTIDX SET FOOTIDX=+$ORDER(OPTION("FOOTER",""),-1)+1
  IF $GET(OPTION("PROMPT"))'="" SET OPTION("FOOTER",FOOTIDX)=OPTION("PROMPT")
  SET OPTION("SCRN WIDTH")=WIDTH
  SET OPTION("ON SELECT")="HNDONSEL^TMGIOUT2" ;"code to call based on user input
  SET OPTION("ON CMD")="HNDONCMD^TMGIOUT2"    ;"code to execute for number entry
  SET OPTION("ON KEYPRESS")="HNDONKP^TMGIOUT2"    ;"code to execute commands and keypresses
  ;
  NEW MSG SET MSG=$GET(OPTION("MSG"))
  IF MSG'="" DO
  . DO POPUPBOX^TMGUSRI2("Message:",MSG)
  . DO PRESS2GO^TMGUSRI2
  ;
  NEW STACKCALLER SET STACKCALLER=$$CALLER()
  NEW NODEDIV SET NODEDIV=$GET(OPTION("NODEDIV"),"/")
  SET OPTION("NODEDIV")=NODEDIV ;" in case it wasn't there initially
  NEW CURDIR SET CURDIR=$GET(OPTION("URL"))
  IF CURDIR="" SET CURDIR=$GET(OPTION("PATH"))
  IF (CURDIR="")&($DATA(^TMG("TMP","SETTINGS","FBROWSE",STACKCALLER))) DO
  . SET CURDIR=$GET(^TMG("TMP","SETTINGS","FBROWSE",STACKCALLER))
  IF CURDIR="" SET CURDIR=NODEDIV
  SET CURDIR=$$MKTRALDV^TMGIOUTL(CURDIR,NODEDIV)
  IF $$ISDIR(CURDIR,.OPTION)=0 SET CURDIR=NODEDIV
  ;
  NEW TMGSELECT SET TMGSELECT=""
  NEW HEADERDIRSTR
  NEW HEADERTOP SET HEADERTOP=HEADERLN
L1 ;
  DO LOADDIR("SCRLFILES",CURDIR,.OPTION)
  SET HEADERDIRSTR="Current Dir: "_$$MKTRALDV^TMGIOUTL(CURDIR,NODEDIV)
  FOR  DO  QUIT:HEADERDIRSTR=""
  . IF $LENGTH(HEADERDIRSTR)'>(WIDTH-2) DO
  . . IF HEADERTOP=3 DO
  . . . SET OPTION("HEADER",HEADERLN)="|"_$$CJ^XLFSTR(HEADERDIRSTR,WIDTH-2)_"|"
  . . ELSE  DO
  . . . SET OPTION("HEADER",HEADERLN)="|"_$$LJ^XLFSTR(HEADERDIRSTR,WIDTH-2)_"|"
  . . SET HEADERDIRSTR=""
  . ELSE  DO
  . . SET OPTION("HEADER",HEADERLN)="|"_$EXTRACT(HEADERDIRSTR,1,(WIDTH-2))_"|"
  . . SET HEADERLN=HEADERLN+0.1
  . . SET HEADERDIRSTR=$EXTRACT(HEADERDIRSTR,WIDTH-1,$LENGTH(HEADERDIRSTR))
  SET TMGSELECT=""
  DO SCROLLER^TMGUSRIF("SCRLFILES",.OPTION) ;"Event handler should SET TMGSELECT
  IF TMGSELECT="" GOTO LQ
  IF SELDIR SET DONE=0 DO  GOTO:DONE LQ
  . NEW MENU,USRSLCT
  . SET MENU(0)="What DO you want to DO with this directory?"
  . SET MENU(1)="Choose "_TMGSELECT_" as selected directory"_$CHAR(9)_"DONE"
  . SET MENU(2)="Browse INTO it"_$CHAR(9)_"into"
  . WRITE !
  . SET USRSLCT=$$MENU^TMGUSRI2(.MENU,2)
  . WRITE #
  . IF USRSLCT="DONE" SET DONE=1
  IF $$ISDIR(TMGSELECT,.OPTION) SET CURDIR=TMGSELECT GOTO L1 ;"browse into directory
  DO SPLITFPN^TMGIOUTL(TMGSELECT,.OUTPATH,.OUTNAME,NODEDIV)
  ;
  SET ^TMG("TMP","SETTINGS","FBROWSE",STACKCALLER)=OUTPATH ;"store for future use.
LQ ;
  WRITE # ;"clear screen
  QUIT TMGSELECT
  ;
ISDIR(CURDIR,OPTION) ;
  NEW RESULT SET RESULT=""
  IF $DATA(OPTION("URL")) DO
  . NEW LASTCHAR SET LASTCHAR=$EXTRACT(CURDIR,$LENGTH(CURDIR))
  . SET RESULT=(LASTCHAR="/")
  ELSE  DO
  . SET RESULT=$$ISDIR^TMGKERNL(CURDIR)
  QUIT RESULT
  ;
GETDIR(CURDIR,PARR,OPTION) ;
  NEW RESULT SET RESULT=""
  IF $DATA(OPTION("URL")) DO
  . NEW TMGTEMP
  . DO GETWDIR^TMGKERN4(CURDIR,"TMGTEMP")
  . NEW INDEX SET INDEX=""
  . FOR  SET INDEX=$ORDER(TMGTEMP(INDEX)) QUIT:INDEX=""  DO
  . . NEW LINE SET LINE=$GET(TMGTEMP(INDEX)) QUIT:LINE=""
  . . SET LINE=$PIECE(LINE,"^",1)
  . . SET @PARR@(LINE)=""
  ELSE  DO
  . NEW TEMPMASK SET TEMPMASK("*")=""
  . IF $$LIST^%ZISH(CURDIR,"TEMPMASK",PARR)=0
  QUIT RESULT
  ;
LOADDIR(PARRAY,CURDIR,OPTION) ;
  ;"Purpose: load CURDIR entries into PARRAY
  ;"Input: PARRAY -- PASS BY NAME.  An OUT PARAMETER.  Filled in as follows
  ;"         @PARRAY@(1,DisplayText)=Return Text <-- note: must be numbered 1,2,3 etc.
  ;"         @PARRAY@(2,DisplayText)=Return Text
  ;"         @PARRAY@(3,DisplayText)=Return Text
  ;"       CURDIR -- the directory to get files from
  ;"       TMGMASK -- PASS BY REFERENCE.  The mask array (See FBROWSE)
  ;"       OPTION [OPTIONAL].  Format as follows.  All entries are optional
  ;"           OPTION("MATCH","CASESPEC")=1" <-- means case specific.  Default is NON-specific
  ;"           OPTION("MATCH","*.m")="" -- e.g. use filter '*.m'
  ;"           OPTION("MATCH","*.txt")="" -- e.g. use filter '*.txt"
  ;"               NOTE: Filters are combined by AND, i.e.  files matching one of the specified matches
  ;"           OPTION("NodeDiv") The character that separates folders (e.g. "/")
  ;"                             If not supplied, then default value is "/"
  ;"           OPTION("SHOW HIDDEN")=1  Show files hidden (e.g. '.name')
  ;"           OPTION("SELECT DIR")=1  IF 1 then mode is to select directories, not files
  ;"       NODEDIV -- The character that separates folders (e.g. "/")
  ;"       SHOWHIDDEN -- OPTIONAL. Default=0  If 1, then show hidden files
  ;"Results: none
  ;
  NEW TMGFILES,TEMPFILES
  NEW COUNT SET COUNT=1
  KILL @PARRAY
  NEW NODEDIV SET NODEDIV=$GET(OPTION("NODEDIV"),"/")
  SET NODEDIV=$GET(NODEDIV,"/")                                            
  SET SHOWHIDDEN=+$GET(OPTION("SHOW HIDDEN"))
  NEW SELDIR SET SELDIR=+$GET(OPTION("SELECT DIR"))
  SET CURDIR=$GET(CURDIR,NODEDIV)
  SET CURDIR=$$MKTRALDV^TMGIOUTL(CURDIR,NODEDIV)
  IF $$ISDIR(CURDIR,.OPTION)=0 GOTO LDDN
  ;"Note: Filter/Mask would apply to directory names too, so must
  ;"      ask for list of files with mask applied **AND** also with
  ;"      a mask of '*' to be sure to get directory names
  DO GETDIR(CURDIR,"TMGFILES",.OPTION)
  ;"NEW TEMPMASK SET TEMPMASK("*")=""
  ;"IF $$LIST^%ZISH(CURDIR,"TEMPMASK","TMGFILES")=0 GOTO LDDN
  NEW INDEX SET INDEX=""
  FOR  SET INDEX=$ORDER(TMGFILES(INDEX)) QUIT:(INDEX="")  DO
  . IF ($EXTRACT(INDEX,1)=".")&(SHOWHIDDEN=0) QUIT
  . NEW FNAME,FPNAME
  . SET FNAME=INDEX
  . SET FPNAME=CURDIR_FNAME
  . IF $$ISDIR(FPNAME,.OPTION) SET TEMPFILES("DIRS","<"_FNAME_">")=FPNAME
  . ELSE  SET TEMPFILES("FILES",FNAME)=FPNAME
  ;  
  ;"//kt 7/22/15 NOTE: the filter for $$LIST^%ZISH works for like this:
  ;"    E* or DI* to get files starting with E or DI respectively.  But I want
  ;"    it to work like this *.txt, will do it myself below
  ;";"Now get files again with user-supplied filter
  NEW TMGMASK MERGE TMGMASK=OPTION("MATCH")
  ;"IF $DATA(TMGMASK)=0 GOTO LD2  ;"use FILES node already created
  ;"KILL TEMPFILES("FILES")  ;"needs to be reloaded with mask applied.
  ;"IF $$LIST^%ZISH(CURDIR,"TMGMASK","TMGFILES")=0 GOTO LDDN
  ;"NEW INDEX SET INDEX=""                                                       
  ;"FOR  SET INDEX=$ORDER(TMGFILES(INDEX)) QUIT:(INDEX="")  DO
  ;". IF ($EXTRACT(INDEX,1)=".")&(SHOWHIDDEN=0) QUIT
  ;". NEW FNAME,FPNAME
  ;". SET FNAME=INDEX
  ;". SET FPNAME=CURDIR_FNAME
  ;". IF $GET(TEMPFILES("DIRS","<"_FNAME_">"))'="" QUIT
  ;". SET TEMPFILES("FILES",FNAME)=FPNAME
  ;";
  NEW AFILE SET AFILE=""
  IF $DATA(TMGMASK)=0 GOTO LD2
  FOR  SET AFILE=$ORDER(TEMPFILES("FILES",AFILE)) QUIT:AFILE=""  DO
  . IF $$ISMATCH(AFILE,.TMGMASK)=0 KILL TEMPFILES("FILES",AFILE)
LD2 ;
  SET INDEX=""
  IF CURDIR'=NODEDIV DO
  . SET @PARRAY@(COUNT,".. <UP>")=$$UPPATH^TMGIOUTL(CURDIR)
  . SET COUNT=COUNT+1
  FOR  SET INDEX=$ORDER(TEMPFILES("DIRS",INDEX)) QUIT:(INDEX="")  DO
  . SET @PARRAY@(COUNT,INDEX)=$GET(TEMPFILES("DIRS",INDEX))
  . SET COUNT=COUNT+1
  IF SELDIR=1 GOTO LDDN  ;"skip showing files.
  ;
  SET INDEX=""
  FOR  SET INDEX=$ORDER(TEMPFILES("FILES",INDEX)) QUIT:(INDEX="")  DO
  . SET @PARRAY@(COUNT,INDEX)=$GET(TEMPFILES("FILES",INDEX))
  . SET COUNT=COUNT+1
  ;
LDDN ;
  QUIT
  ;
ISMATCH(FNAME,FILTARR) ;"IS FNAME A MATCH AGAINST FILTER ARRAY?  
  ;"Input: FNAME -- the file name to check.
  ;"       FILTERARR -- PASS BY REFERENCE.  Format:
  ;"           FILTERARR("CASESPEC")=1" <-- means case specific.  Default is NON-specific
  ;"           FILTERARR("*.m")="" -- e.g. use filter '*.m'
  ;"           FILTERARR("*.txt")="" -- e.g. use filter '*.txt"
  ;"NOTE: result is TRUE if name matches against ANY of the supplied patterns
  ;"Result: 1 if match, or 0 if not
  NEW CASESPEC SET CASESPEC=+$GET(FILTERARR("CASESPEC"))
  IF CASESPEC'=1 SET FNAME=$$UP^XLFSTR(FNAME)
  NEW RESULT SET RESULT=0
  NEW AFILTER SET AFILTER=""
  FOR  SET AFILTER=$ORDER(FILTARR(AFILTER)) QUIT:(AFILTER="")!RESULT  DO
  . NEW FILT1 SET FILT1=AFILTER IF CASESPEC'=1 SET FILT1=$$UP^XLFSTR(FILT1)
  . SET RESULT=$$MATCH1(FNAME,FILT1)
  QUIT RESULT
  ;
MATCH1(FNAME,FILTER) ;"IS FNAME A MATCH AGAINST NAMED FILTER?
  ;"Input: FNAME -- the file name to check, already case-adjusted
  ;"       FILTER.  A filter name, e.g. "*.m" or "*.txt" or "DI*" or "TMG*.m"
  ;"Result: 1 if match, or 0 if not
  NEW RESULT SET RESULT=1 ;"default to success
  NEW PREMATCH SET PREMATCH=$PIECE(FILTER,"*",1) GOTO:PREMATCH="" M1L2
  SET RESULT=$$LMATCH^TMGSTUT3(FNAME,PREMATCH)
  GOTO:RESULT=0 M1DN  
M1L2 ;  
  NEW POSTMATCH SET POSTMATCH=$PIECE(FILTER,"*",2)
  SET RESULT=$$RMATCH^TMGSTUT3(FNAME,POSTMATCH)
M1DN ;
  QUIT RESULT

HNDONSEL(PARRAY,OPTION,INFO) ;
  ;"Purpose: handle ON SELECT event from Scroller
  ;"Input: PARRAY,OPTION,INFO -- see documentation in Scroller
  ;"       INFO has this:
  ;"          INFO("CURRENT LINE","NUMBER")=number currently highlighted line
  ;"          INFO("CURRENT LINE","TEXT")=Text of currently highlighted line
  ;"          INFO("CURRENT LINE","RETURN")=return value of currently highlighted line
  ;"Globally-scoped var used: TMGSELECT,TMGSCLRMSG
  NEW TEXT SET TEXT=$GET(INFO("CURRENT LINE","TEXT"))                         
  SET TMGSELECT=$GET(INFO("CURRENT LINE","RETURN"))
  SET TMGSCLRMSG="^"
  QUIT
  ;
HNDONCMD(PARRAY,OPTION,INFO) ;
  ;"Purpose: handle ON CMD event from Scroller
  ;"Input: PARRAY,OPTION,INFO -- see documentation in Scroller
  ;"       INFO has this:
  ;"          INFO("USER INPUT")=input
  ;"          INFO("CURRENT LINE","NUMBER")=number currently highlighted line
  ;"          INFO("CURRENT LINE","TEXT")=Text of currently highlighted line
  ;"          INFO("CURRENT LINE","RETURN")=return value of currently highlighted line
  NEW DONE SET DONE=0
  NEW RTN SET RTN=$GET(INFO("CURRENT LINE","RETURN"))
  NEW PATH SET PATH=RTN
  NEW USRINPUT SET USRINPUT=$GET(INFO("USER INPUT"))
  NEW UPUSRINPUT SET UPUSRINPUT=$$UP^XLFSTR(USRINPUT)
  SET ^TMP("HNDONCMD^TMGIOUT2",$J)=USRINPUT
  NEW CMD SET CMD=$$UP^XLFSTR($PIECE(USRINPUT," ",1))
  IF $EXTRACT(PATH,$LENGTH(PATH))'=NODEDIV DO
  . SET PATH=$$UPPATH^TMGIOUTL(PATH)  ;"Trim off filename
  IF CMD="CD" DO  GOTO:DONE HOCDN
  . NEW NEWDIR SET NEWDIR=$PIECE(USRINPUT," ",2)
  . IF NEWDIR=".." SET USRINPUT=".." QUIT
  . SET DONE=1
  . IF $EXTRACT(NEWDIR,1)'="/" SET NEWDIR=PATH_NEWDIR
  . IF $$ISDIR(NEWDIR,.OPTION)=0 DO  QUIT
  . . WRITE NEWDIR," is not a valid existing directory.",!
  . . DO PRESS2GO^TMGUSRI2
  . SET TMGSELECT=NEWDIR
  . SET TMGSCLRMSG="^"
  IF CMD="MKDIR" DO  GOTO:DONE HOCDN
  . NEW NEWDIR SET NEWDIR=$PIECE(USRINPUT," ",2)
  . SET DONE=1
  . IF $EXTRACT(NEWDIR,1)'="/" SET NEWDIR=PATH_NEWDIR
  . WRITE !,"Create NEW directory: ",NEWDIR
  . NEW % SET %=2
  . DO YN^DICN WRITE !
  . IF %=1 IF $$MKDIR^TMGKERNL(NEWDIR)
  . WRITE #
  . SET TMGSELECT=PATH
  . SET TMGSCLRMSG="^"
  IF CMD="RMDIR" DO  GOTO:DONE HOCDN
  . NEW NEWDIR SET NEWDIR=$PIECE(USRINPUT," ",2)
  . SET DONE=1
  . IF $EXTRACT(NEWDIR,1)'="/" SET NEWDIR=PATH_NEWDIR
  . WRITE !,"DELETE directory: ",NEWDIR
  . NEW % SET %=2
  . DO YN^DICN WRITE !
  . IF %=1 IF $$RMDIR^TMGKERNL(NEWDIR)
  . SET TMGSELECT=PATH
  . SET TMGSCLRMSG="^"
  . WRITE #
  IF (USRINPUT="{LEFT}")!(USRINPUT="..") DO  GOTO HOCDN
  . NEW NODEDIV SET NODEDIV=$GET(OPTION("NODEDIV"),"/") ;"extra info passed
  . SET TMGSELECT=$$UPPATH^TMGIOUTL(PATH)
  . SET TMGSCLRMSG="^"
  IF USRINPUT="{RIGHT}" DO  GOTO HOCDN
  . IF $$ISDIR(RTN,.OPTION)=0 QUIT
  . SET TMGSELECT=$GET(INFO("CURRENT LINE","RETURN"))
  . SET TMGSCLRMSG="^"
  ;"Later, I could put some stuff here to let the command line choose filters etc.
  ;"or perhaps jump to a given directory etc.  Perhaps later...
  IF USRINPUT["?" DO  GOTO HOCDN
  . DO SHOWHELP(.OPTION)
  IF UPUSRINPUT="FILTER" DO  GOTO HOCDN
  . DO EDITFILT(.OPTION,PARRAY)
  . WRITE #  ;"//CLEAR SCREEN
  ELSE  DO
  . NEW NEWNAME SET NEWNAME=PATH_USRINPUT
  . NEW % SET %=2
  . IF $$FILEXIST^TMGIOUTL(NEWNAME) SET %=1
  . ELSE  DO
  . . WRITE !,"Use NEW filename: ",NEWNAME
  . . DO YN^DICN WRITE !
  . . IF %'=1 WRITE #
  . IF %=1 DO
  . . SET TMGSELECT=NEWNAME
  . . SET TMGSCLRMSG="^"
HOCDN  ;
  QUIT
  ;
HNDONKP(PARRAY,OPTION,INFO) ;"HANDLE KEYPRESS
  ;"Purpose: handle ON KEYPRESS event from Scroller
  ;"Input: PARRAY,OPTION,INFO -- see documentation in Scroller
  ;"       INFO has this:
  ;"          INFO("USER INPUT")=input
  NEW USRINPUT SET USRINPUT=$GET(INFO("USER INPUT"))
  IF (USRINPUT="{LEFT}") DO HNDONCMD(PARRAY,.OPTION,.INFO)
  IF USRINPUT="{RIGHT}" DO HNDONCMD(PARRAY,.OPTION,.INFO) 
  QUIT
  ;  
SHOWHELP(OPTION) ;
  ;"Purpose: show help for file browser
  ;"Input: OPTION -- see documentation in Scroller
  WRITE !
  WRITE "Use [UP], [DOWN], [PgUp], or [PgDown] keys to scroll",!
  WRITE "Use [ENTER] to select file name",!
  WRITE "Use [ENTER] or [RIGHT] key to browse into a directory",!
  WRITE "Use [LEFT] key to back up one level",!
  WRITE "Use ^ to abort without selecting a file",!
  IF $GET(OPTION("SELECT DIR"))'=1 DO
  . WRITE "To create/select a NEW file, just type NEW name and [ENTER]",!
  WRITE "type: 'cd <DirName>' to change directory",!
  WRITE "type: 'MKDIR <DirName>' to create a NEW directory",!
  WRITE "type: 'RMDIR <DirName>' to DELETE a NEW directory",!
  WRITE "type: 'FILTER' to edit file name filter(s)",!
  DO PRESS2GO^TMGUSRI2
  WRITE #
  QUIT
  ;
CALLER(CODE) ;
  ;"Purpose: From call stack, return the location of the caller of the function
  ;"         Note this will not return the address of the function calling
  ;"         Caller, but instead, the address of the function before that
  ;"         in the stack.
  ;"         So a function (A) can call this routine to find out who called it (A).
  ;"Input: Code -- OPTIONAL.  PASS BY REFERANCE, AN OUT PARAMETER
  ;"                      Filled with line of calling code.
  SET CODE=$STACK($STACK-2,"MCODE")
  NEW RESULT SET RESULT=$STACK($STACK-2,"PLACE")
  IF RESULT="" SET RESULT="?"
  QUIT RESULT
  ;
EDITFILT(OPTION,PARRAY)  ;"EDIT FILENAME FILTERS (MATCHES)
  ;"Input: OPTION,PARRAY -- see documentation in Scroller
  ;"NOTE: only OPTION("MATCH", is used here   
  ;"           OPTION("MATCH","CASESPEC")=1" <-- means case specific.  Default is NON-specific
  ;"           OPTION("MATCH","*.m")="" -- e.g. use filter '*.m'
  ;"           OPTION("MATCH","*.txt")="" -- e.g. use filter '*.txt"
  ;"               NOTE: files matching ANY one of the specified matches allows display
  NEW MENU,IDX,USRPICK
  NEW CHANGED SET CHANGED=0
EFM1 ;  
  KILL MENU SET IDX=1
  SET MENU(0)="Pick Options for Filtering Displayed File Names"
  SET MENU(IDX)="ADD new file name filter"_$C(9)_"ADD",IDX=IDX+1
  IF $DATA(OPTION("MATCH")) DO
  . SET MENU(IDX)="SHOW current list of file name filters"_$C(9)_"SHOW",IDX=IDX+1
  . SET MENU(IDX)="REMOVE file name filter"_$C(9)_"REMOVE",IDX=IDX+1
  . IF +$GET(OPTION("MATCH","CASESPEC"))=1 DO
  . . SET MENU(IDX)="Set file name filters to NOT be case sensitive"_$C(9)_"NO-CS",IDX=IDX+1
  . ELSE  DO
  . . SET MENU(IDX)="Set file name filters to be CASE SENSITIVE"_$C(9)_"SET-CS",IDX=IDX+1
  SET MENU(IDX)="Done"_$C(9)_"^",IDX=IDX+1
  SET USRPICK=$$MENU^TMGUSRI2(.MENU)
  IF USRPICK="SHOW" DO  GOTO EFM1
  . NEW FILT,FOUND SET FOUND=0
  . SET FILT="" FOR  SET FILT=$ORDER(OPTION("MATCH",FILT)) QUIT:FILT=""  DO
  . . WRITE:(FILT'="CASESPEC") "     ",FILT,!
  . . SET FOUND=1
  . IF FOUND'=1 WRITE "     (none)",!
  . DO PRESS2GO^TMGUSRI2 WRITE !
  IF USRPICK="NO-CS" DO  GOTO EFM1
  . KILL OPTION("MATCH","CASESPC")
  . SET CHANGED=1
  IF USRPICK="SET-CS" DO  GOTO EFM1
  . SET OPTION("MATCH","CASESPEC")=1
  . SET CHANGED=1
  IF USRPICK="ADD" DO  GOTO EFM1
  . WRITE !,"Enter new file name filter: "
  . NEW TEMP READ TEMP:$GET(DTIME,2600),!
  . IF (TEMP="^")!(TEMP="") QUIT
  . SET OPTION("MATCH",TEMP)=""
  . SET CHANGED=1
  IF USRPICK="REMOVE" DO  GOTO EFM1
  . IF $$DELFILT(.OPTION) SET CHANGED=1
  . SET TMGSCLRMSG="^"  
EMDN ;
  IF CHANGED DO
  . DO LOADDIR(PARRAY,$GET(OPTION("PATH"),"/"),.OPTION)
  . DO DISPFILT(.OPTION)    
  QUIT
  ;
DELFILT(OPTION) ;"DELETE FILENAME FILTER
  ;"Results: 1 if something changed,or 0 if not. 
  NEW MENU,IDX,USRPICK
  NEW CHANGED SET CHANGED=0
DFM1 ;  
  KILL MENU SET IDX=1
  SET MENU(0)="Pick File Name Filter to DELETE"
  NEW FILT,FOUND SET FOUND=0
  SET FILT="" FOR  SET FILT=$ORDER(OPTION("MATCH",FILT)) QUIT:FILT=""  DO
  . SET MENU(IDX)=FILT_$C(9)_FILT,IDX=IDX+1
  SET MENU(IDX)="Done"_$C(9)_"^",IDX=IDX+1
  SET USRPICK=$$MENU^TMGUSRI2(.MENU)
  IF USRPICK="" SET USRPICK="^"
  IF USRPICK="^" GOTO DFMDN
  KILL OPTION("MATCH",USRPICK)
  SET CHANGED=1
  GOTO DFM1
DFMDN ;
  QUIT CHANGED
  ;
DISPFILT(OPTION)  ;"SET OPTION ARRAY TO PROPERLY DISPLAY CURRENT FILTERS
  NEW FILTNAME SET FILTNAME="Filter(s): "
  ;"Remove any prior entries with Filter(s) entries. 
  NEW IDX SET IDX=1
  FOR  SET IDX=$ORDER(OPTION("FOOTER",IDX)) QUIT:+IDX'>0  DO
  . IF $$LMATCH^TMGSTUT3($GET(OPTION("FOOTER",IDX)),FILTNAME)=0 QUIT
  . KILL OPTION("FOOTER",IDX)
  SET IDX=+$ORDER(OPTION("FOOTER",""),-1)+1
  ;"Add entries for filters...
  NEW WIDTH SET WIDTH=$GET(OPTION("SCRN WIDTH"),70)   
  NEW AFILTER SET AFILTER=""
  NEW FILTERSTR SET FILTERSTR=""
  FOR  SET AFILTER=$ORDER(OPTION("MATCH",AFILTER)) QUIT:AFILTER=""  DO
  . IF FILTERSTR="" SET FILTERSTR=FILTNAME
  . IF $LENGTH(FILTERSTR_AFILTER_" ")>WIDTH DO
  . . SET OPTION("FOOTER",IDX)=FILTERSTR,IDX=IDX+1
  . . SET FILTERSTR=FILTNAME
  . SET FILTERSTR=FILTERSTR_AFILTER_" "
  IF FILTERSTR'="" SET OPTION("FOOTER",IDX)=FILTERSTR
  QUIT
  ;
