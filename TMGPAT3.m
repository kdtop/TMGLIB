TMGPAT3  ;TMG/kst/Patching tools ;09/17/08, 2/2/14, 6/7/22
         ;;1.0;TMG-LIB;**1**;09/17/08
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
 ;"NEWPACK -- Install a NEW package from ftp server.
 ;"CONSOLE --show how many patches for a package are available and have not been installed yet
 ;"RESCAN -- show how many patches for a package are available and have not been installed yet
 ;"SHOWPLST -- Entry point for Show Applied Patches
 ;"ShowPATCHES(PCKINIT,VER) -- show installed patches, using scroll box.
 ;"EditNotes -- launch an editor for editing notes about patching.
 ;"CONINSTBRW -- Console for 'INSTALL' file browser.  
 ;
 ;"=======================================================================
 ;"Private Functions
 ;"=======================================================================
 ;"PREPAVAIL(PARRAY,OPTION) -- prepair an array with patch status, for use with SCROLLER^TMGUSRIF
 ;"HNDONSEL(PARRAY,OPTION,INFO) -- handle ON SELECT event from SCROLLER^TMGUSRIF
 ;"HNDONCMD(PARRAY,OPTION,INFO) -- handle ON SELECT event from Scroller
 ;"STORMSNG(PCKINIT,PARRAY) store the list of missing patches with the pending patches
 ;"DownPCK(PATCHNAME,OPTION,Msg) -- Given a package name, ensure all pending patches are local.
 ;"$$RPT1AVAL^TMGPAT3(PATCHNAME)
 ;"$$RPTAVAIL^TMGPAT3(PCKINIT)
 ;"SCAN4NEW(MAXDays,OPTION) -- scan all packages and determine how many patches are pending for each
 ;"SCAN41(PCKINIT,MAXDays,OPTION) -- scan ONE package and determine how many patches are pending
 ;"SCAN41A1VER(PCKINIT,VER,MAXDays,OPTION) -- scan ONE package and determine how many patches are pending
 ;"GETNEW(PCKINIT,VER,PARRAY,NEEDSREFRESH,OPTION) -- Get array of **just** patches still to be installed for a given package/version
 ;"GETAVAIL(PCKINIT,VER,PARRAY,NEEDSREFRESH,OPTION) -- return array of all patches for a given package/version
 ;"GETPLIST(PCKINIT,VER,PARRAY) -- get a list of applied patches, from PACKAGE file, into ARRAY
 ;"PREPPATCHLIST(PCKINIT,VER,pSHOWARRAY,ByPATCHNUM) -- prepair the patch list for display in scroll box.
 ;"HndOnPCmd(PARRAY,OPTION,INFO) -- handle ON SELECT event from Scroller
 ;"HndOnLstSel(PARRAY,OPTION,INFO) -- handle ON SELECT event from SCROLLER^TMGUSRIF for LIST PATCHES
 ;"SHOWAVAL -- Show data that tallies the available patches.
 ;"IncLineCt(lineCount,pageLen)
 ;"=======================================================================
 ;"=======================================================================
 ;
 ;"NOTE: This Module should be re-written.  Rather than store the data in the global ^TMG(...
 ;"      the Fileman file 22709 should be used.  As it is now, it is a duplication of organization.
 ;
NEWPACK  ;
        ;"Purpose: Install a NEW package from ftp server.
        ;
        NEW %,DIR,PCKINIT,VER,X,Y,Msg
        ;
        DO LOGO^TMGPAT1
        SET %=1
        WRITE "Install a NEW PACKAGE from patch repository server" DO YN^DICN WRITE !
        IF %'=1 GOTO NPDONE
        SET DIR(0)="F^2:4"
        SET DIR("A")="Enter PACKAGE prefix (? for help)"
        SET DIR("?")="Enter Namespace initials."
        SET DIR("?",1)="Enter namespace package prefix initials."
        SET DIR("?",2)="E.g. for Fileman, enter: DI"
        SET DIR("?",3)="Enter ^ to ABORT."
        DO ^DIR WRITE ! ;"RESULTs in X and Y
        IF Y="^" GOTO NPDONE
        SET PCKINIT=Y

        NEW ARRAY,RESULT
        WRITE "Fetching info from patch repository server..."
        ;"SET RESULT=$$GetPckList^TMGKERNL(PCKINIT,.ARRAY)  ;GetPckList is depreciated.   rewritten for newer code....
        SET RESULT=$$GETPATL^TMGKERN4(PCKINIT,.ARRAY)  ;"//kt 6/13/22

        WRITE "  DONE.",!
        IF RESULT=0 GOTO NPDONE
        NEW IEN9D4 SET IEN9D4=+$ORDER(^DIC(9.4,"C",PCKINIT,""))
        IF IEN9D4'>0 DO  GOTO NPDONE
        . DO ADDMSG^TMGPAT2("Can't find PACKAGE named '"_PCKINIT_"'",1,.Msg)
        ;
NPDONE  ;
        IF $$SHOWMSG^TMGPAT2(.Msg)
        WRITE "Goodbye.",!
        QUIT
        ;
 ;"====================================================================
        ;
CONSOLE  ;
        NEW ARRAY,OPTION
        DO PREPAVAIL("ARRAY",.OPTION)
        SET OPTION("FOOTER",1,1)="^ Exit"
        SET OPTION("FOOTER",1,2)="? Help"
        SET OPTION("FOOTER",1,3)="[F1] SHOW Compl"
        SET OPTION("FOOTER",1,4)="[F3] Hx/Avail"
        SET OPTION("FOOTER",1,5)="[F4] Downld Pak"
        SET OPTION("FOOTER",1,6)="[F5] Notes"
        SET OPTION("FOOTER",1,7)="[F6] Add Waiting"
        SET OPTION("ON SELECT")="HNDONSEL^TMGPAT3"
        SET OPTION("ON CMD")="HNDONCMD^TMGPAT3"
        SET OPTION("ON KEYPRESS")="HNDONKP^TMGPAT3"
        WRITE #
        DO SCROLLER^TMGUSRIF("ARRAY",.OPTION)
        QUIT
        ;
PREPAVAIL(PARRAY,OPTION)
        ;"Purpose: To prepair an array with patch status, for use with SCROLLER^TMGUSRIF
        ;"Input: PARRAY -- PASS BY NAME.  ARRAY to put info into.  Prior data is killed.
        ;"       OPTION -- PASS BY REFERENCE.  Prior data is NOT killed.  See SCROLLER^TMGUSRIF for details
        ;"                 OPTION("HIDE EMPTY")=0 OPTIONAL. Default is 0.  If 1 then, entries with no patches.
        ;"       Also-- Uses global variable...
        ;"          ^TMG("KIDS","PENDING PATCHES",PACKAGEInitials,Version)=Count
        ;"          ^TMG("KIDS","PENDING PATCHES",PACKAGEInitials,Version,"DATE REFRESHED")=Last date server checked.
        ;"          ^TMG("KIDS","PENDING PATCHES",PACKAGEInitials,Version,"PATCHES",######)=AAAA*NN.NN*NNNN SEQ #1234"
        ;"          ^TMG("KIDS","PENDING PATCHES",PACKAGEInitials,Version,"PATCHES",######)=AAAA*NN.NN*NNNN SEQ #1234"
        ;"          ^TMG("KIDS","PENDING PATCHES",PACKAGEInitials,"FULL NAME")=PACKAGE name
        ;"Results: NONE
        SET PARRAY=$GET(PARRAY) GOTO:(PARRAY="") pAvDONE
        KILL @PARRAY
        NEW Hinder,Blocked
        NEW ONELine,lineCt SET lineCt=1
        NEW PCKINIT SET PCKINIT=""
        NEW GRANDTOTAL SET GRANDTOTAL=0
        NEW hideEmpty SET hideEmpty=$GET(OPTION("HIDE EMPTY"),1)
        FOR  SET PCKINIT=$ORDER(^TMG("KIDS","PENDING PATCHES",PCKINIT)) QUIT:(PCKINIT="")  DO
        . NEW TOTAL SET TOTAL=0
        . NEW VER SET VER=""
        . NEW PACKAGENAME SET PACKAGENAME=$GET(^TMG("KIDS","PENDING PATCHES",PCKINIT,"FULL NAME"))
        . FOR  SET VER=$ORDER(^TMG("KIDS","PENDING PATCHES",PCKINIT,VER)) QUIT:(+VER'>0)  DO
        . . SET TOTAL=TOTAL+$GET(^TMG("KIDS","PENDING PATCHES",PCKINIT,VER))
        . SET GRANDTOTAL=GRANDTOTAL+TOTAL
        . IF (TOTAL=0)&(hideEmpty=1) QUIT
        . SET ONELine="("_PCKINIT_") "_PACKAGENAME_"  "
        . SET ONELine=$$LJ^XLFSTR($EXTRACT(ONELine,1,40),40)_"--> "_$$RJ^XLFSTR(TOTAL,3)_" patches. "
        . NEW temPARRAY,current,MAXVER
        . SET MAXVER=0,VER="",current=""
        . FOR  SET VER=$ORDER(^TMG("KIDS","PENDING PATCHES",PCKINIT,VER)) QUIT:(+VER'>0)  DO
        . . IF VER'>MAXVER QUIT
        . . NEW TEMP SET TEMP=$$GETLSTPK^TMGPAT1(PCKINIT,VER)
        . . IF TEMP'="" SET MAXVER=VER,current=TEMP
        . SET ONELine=ONELine_"Currently @ "_current
        . SET @PARRAY@(lineCt,ONELine)=$PIECE(current,"*",1,2)
        . SET lineCt=lineCt+1
        . NEW i SET i=""
        . FOR  SET i=$ORDER(^TMG("KIDS","PENDING PATCHES",PCKINIT,"WAITING FOR",i)) QUIT:(i="")  DO
        . . SET ONELine="    Waiting for "_i
        . . SET @PARRAY@(lineCt,ONELine)=$PIECE(i,"*",1,2)
        . . SET lineCt=lineCt+1
        . . NEW init SET init=$PIECE(i,"*",1)
        . . SET Hinder(init,PCKINIT)=""
        . . SET Blocked(PCKINIT)=1

        IF '$DATA(Hinder) GOTO pAV2

        NEW COUNT for COUNT=1:1:5 DO
        . SET init="" FOR  SET init=$ORDER(Hinder(init)) QUIT:(init="")  DO
        . . NEW init1,init2 SET init1=init,init2=""
        . . FOR  SET init2=$ORDER(Hinder(init,init2)) QUIT:(init2="")  DO
        . . . QUIT:(init2=init)
        . . . MERGE Hinder(init,init2)=Hinder(init2)

        SET @PARRAY@(lineCt,"--- SUMMARY ------------------------------")=""
        SET lineCt=lineCt+1
        NEW spaces SET $PIECE(spaces," ",20)=" "
        NEW ref SET ref="Hinder"
        NEW hideARRAY
        SET init=""
        FOR  SET ref=$query(@ref) QUIT:(ref="")  DO
        . IF $$OREF^DILF($query(@ref))[$$OREF^DILF(ref) QUIT
        . NEW COUNT,node,DONE SET DONE=0
        . for COUNT=1:1:$qlength(ref) DO  QUIT:DONE
        . . SET node=$qsubscript(ref,COUNT)
        . . SET ONELine=$SELECT((COUNT=1):"#",1:"")
        . . SET ONELine=ONELine_$EXTRACT(spaces,1,COUNT*3)_"Package "_node
        . . IF (COUNT=1)&($GET(Blocked(node))=1) SET DONE=1 QUIT
        . . IF COUNT=1 SET ONELine=ONELine_" is hindering..."
        . . ELSE  IF COUNT<$qlength(ref) SET ONELine=ONELine_", which is hindering..."
        . . IF $GET(hideARRAY(COUNT))=ONELine QUIT
        . . SET hideARRAY(COUNT)=ONELine
        . . SET @PARRAY@(lineCt,ONELine)="",lineCt=lineCt+1

pAV2    SET OPTION("HEADER",1)="TMG Patch Helper--  "_GRANDTOTAL_" Patches to be installed in all packages."

pAvDONE
        QUIT

HNDONKP(PARRAY,OPTION,INFO)
        ;"Purpose: handle ON SELECT event from SCROLLER^TMGUSRIF
        ;"Input: PARRAY,OPTION,INFO -- see documentation in SCROLLER^TMGUSRIF
        DO HNDONCMD(.PARRAY,.OPTION,.INFO)
        QUIT

HNDONSEL(PARRAY,OPTION,INFO)
        ;"Purpose: handle ON SELECT event from SCROLLER^TMGUSRIF
        ;"Input: PARRAY,OPTION,INFO -- see documentation in SCROLLER^TMGUSRIF
        ;"       INFO has this:
        ;"          INFO("CURRENT LINE","NUMBER")=number currently highlighted line
        ;"          INFO("CURRENT LINE","TEXT")=Text of currently highlighted line
        ;"          INFO("CURRENT LINE","RETURN")=return value of currently highlighted line

        NEW PATCHNAME,PCKINIT,VER
        SET PATCHNAME=$GET(INFO("CURRENT LINE","RETURN"))
        DO PRSEPATCHNAME^TMGPAT2(PATCHNAME,.PCKINIT,.VER)
        IF (PCKINIT="")&(VER="") DO  GOTO HOSDONE
        . WRITE "?? The line selected doesn't specify any command ??",!
        DO DONEXTPK^TMGPAT1(PCKINIT,VER)
        DO PREPAVAIL(PARRAY,.OPTION)
HOSDONE ;
        DO PRESS2GO^TMGUSRI2
        WRITE #
        QUIT
        ;
HndOnLstSel(PARRAY,OPTION,INFO)
        ;"Purpose: handle ON SELECT event from SCROLLER^TMGUSRIF
        ;"Input: PARRAY,OPTION,INFO -- see documentation in SCROLLER^TMGUSRIF
        ;"       INFO has this:
        ;"          INFO("CURRENT LINE","NUMBER")=number currently highlighted line
        ;"          INFO("CURRENT LINE","TEXT")=Text of currently highlighted line
        ;"          INFO("CURRENT LINE","RETURN")=return value of currently highlighted line
        NEW TEXT,PATCHNAME,PCKINIT,VER,STRB
        SET PATCHNAME=$$TRIM^XLFSTR($GET(INFO("CURRENT LINE","RETURN")))
        SET TEXT=$GET(INFO("CURRENT LINE","TEXT"))
        SET STRB=$$TRIM^XLFSTR($PIECE(TEXT,PATCHNAME,2))
        IF STRB["(available)" DO
        . WRITE !,"Install available patch: ",PATCHNAME
        . NEW % SET %=2 DO YN^DICN WRITE !
        . IF %=-1 SET TMGSCLRMSG="^" QUIT
        . IF %=2 QUIT
        . NEW IENS SET IENS=$$GETIENS^TMGPAT2(PATCHNAME)
        . IF IENS="" DO  QUIT
        . . ;"WRITE "Sorry.  Couldn't find this for some reason!"
        . . ;"DO PRESS2GO^TMGUSRI2
        . DO DOFIXMSG^TMGPAT1(IENS,"",1)
        . DO PREPPATCHLIST(TMGPCKI,TMGPVER,PARRAY,TMGSORT)
        . DO PRESS2GO^TMGUSRI2
        ;
        ELSE  DO
        . WRITE !,TEXT,!
        . DO PRESS2GO^TMGUSRI2
        WRITE #
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
        IF INPUT["F2" DO
        . SET OPTION("FOOTER",1,3)="[F1] SHOW compl"
        . SET OPTION("HIDE EMPTY")=1
        ELSE  IF INPUT="RESCAN" DO
        . DO RESCAN
        ELSE  IF INPUT["F1" DO
        . SET OPTION("FOOTER",1,3)="[F2] HIDE compl"
        . SET OPTION("HIDE EMPTY")=0
        ELSE  IF INPUT["F3" DO
        . NEW PATCHNAME SET PATCHNAME=$GET(INFO("CURRENT LINE","RETURN")) QUIT:(PATCHNAME="")
        . NEW PCKINIT,VER
        . DO PRSEPATCHNAME^TMGPAT2(PATCHNAME,.PCKINIT,.VER)
        . IF $$ShowPATCHES(PCKINIT,VER)
        ELSE  IF INPUT["F4" DO
        . NEW PATCHNAME SET PATCHNAME=$GET(INFO("CURRENT LINE","RETURN")) QUIT:(PATCHNAME="")
        . NEW OPTION SET OPTION("VERBOSE")=1
        . NEW % SET %=1
        . WRITE "Ensure that all pending patches for ",PATCHNAME," have been downloaded"
        . DO YN^DICN WRITE !
        . IF %'=1 QUIT
        . DO DownPCK(PATCHNAME,.OPTION)
        . DO PRESS2GO^TMGUSRI2
        ELSE  IF INPUT["F5" DO
        . DO EditNotes
        . DO PRESS2GO^TMGUSRI2
        ELSE  IF INPUT["F6" DO  ;"Add Waiting"
        . IF INFO("CURRENT LINE","TEXT")'["-->" DO  QUIT
        . . WRITE !,"Please first select containing '-->'",!
        . . DO PRESS2GO^TMGUSRI2
        . NEW PATCHNAME SET PATCHNAME=$GET(INFO("CURRENT LINE","RETURN")) QUIT:(PATCHNAME="")
        . NEW PCKINIT SET PCKINIT=$PIECE(PATCHNAME,"*",1)
        . NEW % SET %=1
        . WRITE !,"Manually add a 'Waiting For' entry for ",PATCHNAME
        . DO YN^DICN WRITE !
        . IF %'=1 QUIT
        . NEW DIR,DIRUT SET DIR(0)="F",DIR("A")="Enter what "_PATCHNAME_" is waiting for"
        . DO ^DIR WRITE ! IF $DATA(DIRUT) QUIT
        . DO AddMissing(PCKINIT,Y)
        . DO PRESS2GO^TMGUSRI2
        ELSE  IF INPUT="NEWPACK" DO
        . DO NEWPACK
        ELSE  IF INPUT="?" DO
        . WRITE !,"Use UP and DOWN cursor keys to select package, then ENTER to work on.",!
        . WRITE "Enter 'NEWPACK' to install a NEW package.",!
        . WRITE "Enter 'RESCAN' to rescan the patch repository server",!
        . WRITE "Enter ^ at the ':' prompt to QUIT",!
        . DO PRESS2GO^TMGUSRI2
        ELSE  IF INPUT'="" DO
        . WRITE !,"Input ",$GET(INFO("USER INPUT"))," not recognized.",!
        . DO PRESS2GO^TMGUSRI2
        ;
        DO PREPAVAIL(PARRAY,.OPTION)
        WRITE #
        QUIT
        ;
STORMSNG(PCKINIT,PARRAY)  ;"STORE MISSING
        ;"Purpose: to store the list of missing patches with the pending patches
        KILL ^TMG("KIDS","PENDING PATCHES",PCKINIT,"WAITING FOR")
        MERGE ^TMG("KIDS","PENDING PATCHES",PCKINIT,"WAITING FOR")=@PARRAY
        QUIT
        ;
AddMissing(PCKINIT,PATCHNAME)
        ;"Purpose: Add a missing patche to pending patches
        SET ^TMG("KIDS","PENDING PATCHES",PCKINIT,"WAITING FOR",PATCHNAME)=""
        QUIT
        ;
DownPCK(PATCHNAME,OPTION,Msg)
        ;"Purpose: given a patch name, ensure all pending patches are local.
        ;"Input: PATCHNAME -- patch name, e.g. ABC*1.0*123
        ;"       OPTION -- Optional.  PASS BY REFERENCE.
        ;"                  OPTION("VERBOSE")=1 --> puts output to console
        ;"       Msg -- PASS BY REFERANCE, an OUT PARAMETER
        ;"              Errors are stored in Msg("ERROR",x)=Message
        ;"                                   Msg("ERROR")=COUNT of last error
        ;"              Message are store in Msg(x)=Message
        ;"                                   Msg=COUNT of last message+1
        ;"Results: NONE
        NEW PCKINIT,VER,PATCHNUM,SEQNUM,INFO
        DO PRSEPATCHNAME^TMGPAT2(PATCHNAME,.PCKINIT,.VER,.PATCHNUM,.SEQNUM)
        DO SCAN41A1VER(PCKINIT,VER,90)
        NEW TOTAL SET TOTAL=+$GET(^TMG("KIDS","PENDING PATCHES",PCKINIT,VER))
        NEW COUNT SET COUNT=1
        NEW PATCH SET PATCH=""
        FOR  SET PATCH=$ORDER(^TMG("KIDS","PENDING PATCHES",PCKINIT,VER,"PATCHES",PATCH)) QUIT:(PATCH="")  DO
        . NEW PATCHNAME SET PATCHNAME=$GET(^TMG("KIDS","PENDING PATCHES",PCKINIT,VER,"PATCHES",PATCH))
        . IF $GET(OPTION("VERBOSE"))=1 WRITE COUNT,"/",TOTAL,".  ---- ",PATCHNAME," ----",!
        . NEW IENS SET IENS=$$GETIENS^TMGPAT2(PATCHNAME) QUIT:(IENS="")
        . IF $$ENSRLOCL^TMGPAT2(IENS,.INFO,.Msg,.OPTION,PCKINIT)=0 DO
        . . DO ADDMSG^TMGPAT2("Unable to download patch to local file system.",1,Msg)
        . SET COUNT=COUNT+1
        ;
        IF $GET(OPTION("VERBOSE"))=1 DO
        . IF $$SHOWMSG^TMGPAT2(.Msg)
        ;
        QUIT
        ;
RPT1AVAL(PATCHNAME)
        ;"Purpose: given a patch name (e.g. ABC*1.0*123), return pending patches.
        NEW PCKINIT,VER,PATCHNUM,SEQNUM
        NEW COUNT SET COUNT=-1
        DO PRSEPATCHNAME^TMGPAT2(PATCHNAME,.PCKINIT,.VER,.PATCHNUM,.SEQNUM)
        IF ($GET(PCKINIT)="")!($GET(VER)="") GOTO Rpt1DONE
        DO SCAN41A1VER(PCKINIT,VER,90)
        SET COUNT=+$GET(^TMG("KIDS","PENDING PATCHES",PCKINIT,VER))
Rpt1DONE ;
        QUIT COUNT
        ;
RPTAVAIL(PCKINIT)
        ;"Purpose: given a package (e.g. ABC), return pending patches.
        NEW TOTAL SET TOTAL=0
        NEW VER SET VER=""
        FOR  SET VER=$ORDER(^TMG("KIDS","PENDING PATCHES",PCKINIT,VER)) QUIT:(+VER'>0)  DO
        . SET TOTAL=TOTAL+$GET(^TMG("KIDS","PENDING PATCHES",PCKINIT,VER))
        QUIT TOTAL

RESCAN
        ;"Purpose: To show how many patches for a package are available and have not been installed yet
        WRITE !
        NEW DUOUT,DIR
        SET DIR("A")="Search ftp server IF data is older than __ days old? (SLOW!)"
        SET DIR("B")=90
        SET DIR(0)="N^0:999:0"
        DO ^DIR WRITE !
        NEW OPTION
        SET OPTION("VERBOSE")=0
        IF $GET(DUOUT) QUIT
        DO SCAN4NEW(+Y,.OPTION)
        ;"IF '$GET(DUOUT) DO SHOWAVAL
SADONE  QUIT



SCAN4NEW(MAXDays,OPTION)
        ;"Purpose: to scan all packages and determine how many patches are pending for each
        ;"Input: MAXDays -- the number of days that old days can be used.  If last refresh
        ;"                  was greater than this number, then patch repository server is queried again.
        ;"       OPTION -- Optional.  PASS BY REFERENCE.
        ;"                  OPTION("VERBOSE")=1 --> puts output to console
        ;"Output: Results will be stored:
        ;"          ^TMG("KIDS","PENDING PATCHES",PACKAGEInitials,Version)=Count
        ;"          ^TMG("KIDS","PENDING PATCHES",PACKAGEInitials,Version,"DATE REFRESHED")=Last date server checked.
        ;"          ^TMG("KIDS","PENDING PATCHES",PACKAGEInitials,Version,"PATCHES",######)=AAAA*NN.NN*NNNN SEQ #1234"
        ;"          ^TMG("KIDS","PENDING PATCHES",PACKAGEInitials,Version,"PATCHES",######)=AAAA*NN.NN*NNNN SEQ #1234"
        ;"          ^TMG("KIDS","PENDING PATCHES",PACKAGEInitials,"FULL NAME")=PACKAGE name
        ;"
        ;"Results: NONE

        ;"NOTE: This function should be re-written.  Rather than store the data in the global ^TMG(...
        ;"      the Fileman file 22709 should be used.  As it is now, it is a duplication of organization.
        SET MAXDays=+$GET(MAXDays)
        NEW PACKAGENAME SET PACKAGENAME=""
        FOR  SET PACKAGENAME=$ORDER(^DIC(9.4,"B",PACKAGENAME)) QUIT:(PACKAGENAME="")  DO
        . NEW IEN9D4 SET IEN9D4=+$ORDER(^DIC(9.4,"B",PACKAGENAME,"")) QUIT:(IEN9D4'>0)
        . NEW PCKINIT SET PCKINIT=$PIECE($GET(^DIC(9.4,IEN9D4,0)),"^",2) ;"0;2 = Package prefix
        . DO SCAN41(PCKINIT,MAXDays,.OPTION)
        QUIT
        ;
SCAN41(PCKINIT,MAXDays,OPTION)
        ;"Purpose: to scan ONE package and determine how many patches are pending
        ;"Input: PCKINIT -- Package Initials/prefix
        ;"       MAXDays -- the cutoff for when to requery the server
        ;"       OPTION -- PASS BY REFERENCE.
        ;"              OPTION("VERBOSE")=1 to be VERBOSE.
        ;"Output: Results will be stored:
        ;"          ^TMG("KIDS","PENDING PATCHES",PACKAGEInitials,Version)=Count
        ;"          ^TMG("KIDS","PENDING PATCHES",PACKAGEInitials,Version,"DATE REFRESHED")=Last date server checked.
        ;"          ^TMG("KIDS","PENDING PATCHES",PACKAGEInitials,Version,"PATCHES",######)=AAAA*NN.NN*NNNN SEQ #1234"
        ;"          ^TMG("KIDS","PENDING PATCHES",PACKAGEInitials,Version,"PATCHES",######)=AAAA*NN.NN*NNNN SEQ #1234"
        ;"          ^TMG("KIDS","PENDING PATCHES",PACKAGEInitials,"FULL NAME")=PACKAGE name
        ;"Results: NONE

        NEW IEN9D4 SET IEN9D4=+$ORDER(^DIC(9.4,"C",PCKINIT,""))
        NEW PACKAGENAME SET PACKAGENAME=""
        IF IEN9D4>0 SET PACKAGENAME=$PIECE($GET(^DIC(9.4,IEN9D4,0)),"^",1)
        IF $GET(OPTION("VERBOSE"))=1 WRITE "Checking Package: ",PACKAGENAME," (",PCKINIT,")...",!
        SET ^TMG("KIDS","PENDING PATCHES",PCKINIT,"FULL NAME")=PACKAGENAME
        ;"KILL ^TMG("KIDS","PENDING PATCHES",PCKINIT,"DATE REFRESHED") ;"force refresh
        SET VER=""
        FOR  SET VER=$ORDER(^DIC(9.4,IEN9D4,22,"B",VER)) QUIT:(VER="")  DO
        . DO SCAN41A1VER(PCKINIT,VER,MAXDays,.OPTION)

        QUIT


SCAN41A1VER(PCKINIT,VER,MAXDays,OPTION)
        ;"Purpose: to scan ONE package and determine how many patches are pending
        ;"Input: PCKINIT -- Package Initials/prefix
        ;"       VER -- The version of the Package
        ;"       MAXDays -- the cutoff for when to requery the server
        ;"       OPTION -- PASS BY REFERENCE.
        ;"              OPTION("VERBOSE")=1 to be VERBOSE.
        ;"Output: Results will be stored:
        ;"          ^TMG("KIDS","PENDING PATCHES",PACKAGEInitials,Version)=Count
        ;"          ^TMG("KIDS","PENDING PATCHES",PACKAGEInitials,Version,"DATE REFRESHED")=Last date server checked.
        ;"          ^TMG("KIDS","PENDING PATCHES",PACKAGEInitials,Version,"PATCHES",######)=AAAA*NN.NN*NNNN SEQ #1234"
        ;"          ^TMG("KIDS","PENDING PATCHES",PACKAGEInitials,Version,"PATCHES",######)=AAAA*NN.NN*NNNN SEQ #1234"
        ;"          ^TMG("KIDS","PENDING PATCHES",PACKAGEInitials,"FULL NAME")=PACKAGE name
        ;"Results: NONE
        ;"KILL ^TMG("KIDS","PENDING PATCHES",PCKINIT,"DATE REFRESHED") ;"force refresh
        IF $GET(PCKINIT)="" GOTO S41DONE
        IF $GET(VER)="" GOTO S41DONE
        SET MAXDays=+$GET(MAXDays,90)
        IF $GET(OPTION("VERBOSE"))=1 WRITE "  VER: ",VER,"  ",!
        NEW NEEDSREFRESH
        NEW LASTCHECK SET LASTCHECK=$GET(^TMG("KIDS","PENDING PATCHES",PCKINIT,"DATE REFRESHED"))
        IF LASTCHECK'="" DO
        . NEW X,Y,%DT,X1,X2
        . SET X=LASTCHECK,%DT="TS"
        . DO ^%DT ;"RESULT in Y
        . SET X=0
        . DO NOW^%DTC ;"returns date in X
        . SET X1=X,X2=Y
        . DO ^%DTC  ;"returns X=X1-X2
        . SET NEEDSREFRESH=(X>MAXDays)
        . IF NEEDSREFRESH QUIT
        . NEW % SET %=2
        . WRITE "Online patch repository was last checked ",X," days ago.",!
        . WRITE "Force recheck now"
        . DO YN^DICN
        . IF %=1 SET NEEDSREFRESH=1
        ELSE  SET NEEDSREFRESH=1
        NEW PARRAY SET PARRAY=$NAME(^TMG("KIDS","PENDING PATCHES",PCKINIT,VER,"PATCHES"))
        NEW COUNT SET COUNT=$$GETNEW(PCKINIT,VER,PARRAY,NEEDSREFRESH,.OPTION)
        IF $GET(OPTION("VERBOSE"))=1 WRITE "  ",COUNT," patches to be installed.",!
        SET ^TMG("KIDS","PENDING PATCHES",PCKINIT,VER)=COUNT
        IF NEEDSREFRESH DO
        . NEW %,%I,X,Y
        . DO NOW^%DTC SET Y=%
        . X ^DD("DD") ;"RESULT in Y
        . SET ^TMG("KIDS","PENDING PATCHES",PCKINIT,"DATE REFRESHED")=Y
S41DONE QUIT


GETNEW(PCKINIT,VER,PARRAY,NEEDSREFRESH,OPTION)
        ;"Purpose: Get array of **just** patches still to be installed for a given package/version
        ;"Input: PCKINIT -- this is the namespace of the package to get patches for, e.g. 'DI' for fileman
        ;"       VER -- the package version
        ;"       PARRAY -- PASS BY NAME. An OUT PARAMETER.  Format:
        ;"              @PARRAY@(######)=AAAA*NN.NN*NNNN SEQ #1234"
        ;"              @PARRAY@(######)=AAAA*NN.NN*NNNN SEQ #1234"
        ;"       NEEDSREFRESH -- 0 IF refreshing not needed (just ensure file exists)
        ;"       OPTION -- Optional.  PASS BY REFERENCE.
        ;"                   OPTION("VERBOSE")=1 --> puts output to console
        ;"Results: Number of patches still to be installed.

        NEW COUNT SET COUNT=0

        DO GETAVAIL(PCKINIT,VER,PARRAY,.NEEDSREFRESH,.OPTION)
        NEW LASTPCK SET LASTPCK=$$GETLSTPK^TMGPAT1(PCKINIT,VER)
        NEW lastSEQ SET lastSEQ=+$PIECE(LASTPCK,"SEQ #",2)
        IF lastSEQ="" KILL @PARRAY GOTO GNDONE
        NEW i SET i=""
        FOR  SET i=$ORDER(@PARRAY@(i)) QUIT:(i="")  DO
        . IF +i'>lastSEQ KILL @PARRAY@(i) QUIT
        . SET COUNT=COUNT+1
GNDONE
        QUIT COUNT



GETAVAIL(PCKINIT,VER,PARRAY,NEEDSREFRESH,OPTION)
        ;"Purpose: return array of all patches for a given package/version
        ;"Input: PCKINIT -- this is the namespace of the package to get patches for, e.g. 'DI' for fileman
        ;"       VER -- the package version
        ;"       PARRAY -- PASS BY NAME. An OUT PARAMETER.  Format:
        ;"              @PARRAY@(######)=AAAA*NN.NN*NNNN SEQ #1234"
        ;"              @PARRAY@(######)=AAAA*NN.NN*NNNN SEQ #1234"
        ;"       NEEDSREFRESH -- OPTIONAL.  0 IF refreshing not needed (just ensure file exists)
        ;"       OPTION -- Optional.  PASS BY REFERENCE.
        ;"                   OPTION("VERBOSE")=1 --> puts output to console
        ;"RESULTs: NONE

        KILL @PARRAY
        IF $$RefreshPackge^TMGPAT2(PCKINIT,.Msg,.NEEDSREFRESH,.OPTION)

        NEW IEN9D4 SET IEN9D4=+$ORDER(^DIC(9.4,"C",PCKINIT,""))
        IF IEN9D4'>0 GOTO GADONE
        NEW PCKIEN SET PCKIEN=+$ORDER(^TMG(22709,"B",IEN9D4,""))
        IF PCKIEN'>0 GOTO GADONE
        NEW VERIEN SET VERIEN=+$ORDER(^TMG(22709,PCKIEN,1,"B",VER,""))
        IF VERIEN'>0 SET VERIEN=+$ORDER(^TMG(22709,PCKIEN,1,"B",$PIECE(VER,".0",1),""))
        IF VERIEN'>0 GOTO GADONE

        NEW PATCHIEN,NEXTSEQ
        SET PATCHIEN=0
        FOR  SET PATCHIEN=$ORDER(^TMG(22709,PCKIEN,1,VERIEN,1,PATCHIEN)) QUIT:(PATCHIEN'>0)  DO
        . NEW NODE0 SET NODE0=$GET(^TMG(22709,PCKIEN,1,VERIEN,1,PATCHIEN,0)) QUIT:(NODE0="")
        . NEW PATCHNUM SET PATCHNUM=$PIECE(NODE0,"^",1)
        . NEW ONESEQNUM SET ONESEQNUM=$PIECE(NODE0,"^",2)
        . IF ONESEQNUM'>0 SET ONESEQNUM=0
        . SET @PARRAY@($$RJ^XLFSTR(ONESEQNUM,6,"0"))=PCKINIT_"*"_VER_"*"_PATCHNUM_" SEQ #"_ONESEQNUM

GADONE
        QUIT


TestPLIST  ;"temp function.
        NEW PCKINIT,VER,ARRAY
        DO GETPCKVER^TMGPAT1(.PCKINIT,.VER)
        IF VER="^" GOTO TPLDONE
        IF $$GETPLIST(PCKINIT,VER,"ARRAY")=0 GOTO TPLDONE
        DO ZWRITE^TMGZWR("ARRAY")
TPLDONE QUIT


GETPLIST(PCKINIT,VER,PARRAY)
        ;"Purpose: to get a list of applied patches, from PACKAGE file, into ARRAY
        ;"Input:
        ;"Input: PCKINIT -- this is the namespace of the package to get patches for, e.g. 'DI' for fileman
        ;"       VER -- the package version
        ;"       PARRAY -- PASS BY NAME. An OUT PARAMETER.  Format:
        ;"              @PARRAY@(OrderNUM)=PATCHNAME
        ;"              @PARRAY@(OrderNUM,"i",.01)=PATCHNAME
        ;"              either:
        ;"                @PARRAY@(OrderNUM,"i",.02)=Patch Date
        ;"                @PARRAY@(OrderNUM,"i",.03)=Applied By
        ;"              or:
        ;"                @PARRAY@(OrderNUM,"PENDING")="" (means available, but not yet installed)
        ;"Results: 1 if OK, 0 IF error
        ;
        NEW RESULT SET RESULT=1
        ;
        NEW IEN9D4,IEN9D49
        IF $$GETPVIEN^TMGPAT1(PCKINIT,VER,.IEN9D4,.IEN9D49)=0 SET RESULT=0 GOTO GPLDONE
        ;
        NEW ALREADYADDED
        NEW ORDERNUM SET ORDERNUM=1
        NEW PATCHIEN SET PATCHIEN=0
        FOR  SET PATCHIEN=$ORDER(^DIC(9.4,IEN9D4,22,IEN9D49,"PAH",PATCHIEN)) QUIT:(+PATCHIEN'>0)  DO
        . NEW STR SET STR=$PIECE($GET(^DIC(9.4,IEN9D4,22,IEN9D49,"PAH",PATCHIEN,0)),"^",1) ;"0;1=.01    --PATCH APPLICATION HISTORY
        . SET @PARRAY@(ORDERNUM)=STR
        . SET ALREADYADDED(+STR)=""
        . NEW IENS SET IENS=PATCHIEN_","_IEN9D49_","_IEN9D4_","
        . NEW TMGDATA,TMGMSG
        . DO GETS^DIQ(9.4901,IENS,".01;.02;.03","","TMGDATA","TMGMSG")
        . MERGE @PARRAY@(ORDERNUM,"i")=TMGDATA("9.4901",IENS)
        . SET ORDERNUM=ORDERNUM+1
        ;
        ;"Now add available patches to list.
        NEW AVAILLIST
        NEW AVALIDX SET AVALIDX=0
        DO GETAVAIL(PCKINIT,VER,"AVAILLIST")
        FOR  SET AVALIDX=$ORDER(AVAILLIST(AVALIDX)) QUIT:AVALIDX=""  DO
        . NEW STR SET STR=$GET(AVAILLIST(AVALIDX)) QUIT:STR=""
        . SET STR=$PIECE(STR,"*",3)
        . IF $DATA(ALREADYADDED(+STR)) QUIT
        . SET @PARRAY@(ORDERNUM)=STR
        . SET @PARRAY@(ORDERNUM,"PENDING")=""
        . SET @PARRAY@(ORDERNUM,"i",.01)=STR
        . SET ORDERNUM=ORDERNUM+1
        ;
        NEW PATCHDIGITS,SEQDIGITS SET (PATCHDIGITS,SEQDIGITS)=0
        ;"Find length of largest patch number and largest SEQ number
        SET ORDERNUM=0
        FOR  SET ORDERNUM=$ORDER(@PARRAY@(ORDERNUM)) QUIT:ORDERNUM=""  DO
        . NEW STR SET STR=$GET(@PARRAY@(ORDERNUM))
        . NEW PATCHNUM SET PATCHNUM=+$PIECE(STR," ",1)
        . NEW SEQNUM SET SEQNUM=+$PIECE(STR,"SEQ #",2)
        . IF $LENGTH(PATCHNUM)>PATCHDIGITS SET PATCHDIGITS=$LENGTH(PATCHNUM)
        . IF $LENGTH(SEQNUM)>SEQDIGITS SET SEQDIGITS=$LENGTH(SEQNUM)
        ;
        ;"Justify all patch number and SEQ numbers to same length
        SET ORDERNUM=0
        FOR  SET ORDERNUM=$ORDER(@PARRAY@(ORDERNUM)) QUIT:ORDERNUM=""  DO
        . NEW STR SET STR=$GET(@PARRAY@(ORDERNUM))
        . NEW PATCHNUM SET PATCHNUM=+$PIECE(STR," ",1)
        . NEW SEQNUM SET SEQNUM=+$PIECE(STR,"SEQ #",2)
        . IF ($LENGTH(PATCHNUM)'<PATCHDIGITS)&($LENGTH(SEQNUM)'<SEQDIGITS) QUIT
        . SET STR=$$RJ^XLFSTR(PATCHNUM,PATCHDIGITS,"0")_" SEQ #"_$$RJ^XLFSTR(SEQNUM,SEQDIGITS,"0")
        . SET @PARRAY@(ORDERNUM)=STR
        . SET @PARRAY@(ORDERNUM,"i",.01)=STR

GPLDONE QUIT RESULT

SHOWPLST
        ;"Purpose: query user for package and version, then show patches.
        NEW PCKINIT,VER,ARRAY
        DO GETPCKVER^TMGPAT1(.PCKINIT,.VER)
        IF VER="^" GOTO SLPDONE
        IF $$ShowPATCHES(PCKINIT,VER)
SLPDONE QUIT


ShowPATCHES(PCKINIT,VER)
        ;"Purpose: to show installed patches, using scroll box.
        ;"Input: PCKINIT -- this is the namespace of the package to get patches for, e.g. 'DI' for fileman
        ;"       VER -- the package version
        ;"Results: 1 if OK, 0 IF error
        NEW TMGPCKI SET TMGPCKI=PCKINIT  ;"used in HndOnPCmd^TMGPAT3
        NEW TMGPVER SET TMGPVER=VER      ;"used in HndOnPCmd^TMGPAT3
        NEW TMGSORT SET TMGSORT=1        ;"sort by index number.  Used in HndOnPCmd^TMGPAT3
        NEW TMGSARRAY
        SET TMGSARRAY(0)="SORT by IEN order"
        SET TMGSARRAY(1)="SORT by SEQ num"
        SET TMGSARRAY(2)="SORT by PATCH num"
        NEW OPTION,SHOWARRAY,RESULT
        SET OPTION("HEADER",1)="Applied patches for package "_PCKINIT_"  "_TMGSARRAY(TMGSORT)
        SET OPTION("FOOTER",1,1)="^ DONE"
        SET OPTION("FOOTER",1,2)="? Help"
        SET OPTION("FOOTER",1,2.5)="[ENTER] Command"
        SET OPTION("FOOTER",1,3)="[F1] "_TMGSARRAY((TMGSORT+1)#3)
        SET OPTION("FOOTER",1,4)="[F2] Fix Missing PATCH"
        SET OPTION("FOOTER",1,5)="[F3] Fix Missing SEQ"
        SET OPTION("FOOTER",1,6)="[F4] Browse Server"
        SET OPTION("ON CMD")="HndOnPCmd^TMGPAT3"
        SET OPTION("ON KEYPRESS")="HndOnPKP^TMGPAT3"
        SET OPTION("ON SELECT")="HndOnLstSel^TMGPAT3"
        SET OPTION("SHOW INDEX")=1
        ;
        DO PREPPATCHLIST(PCKINIT,VER,"SHOWARRAY",TMGSORT)
        IF $DATA(SHOWARRAY)=0 DO
        . DO ASKVER^TMGPAT1(PCKINIT,.VER)
        . DO PREPPATCHLIST(PCKINIT,VER,"SHOWARRAY",TMGSORT)
        ;
        WRITE #
        DO SCROLLER^TMGUSRIF("SHOWARRAY",.OPTION)
        ;
SPDONE  QUIT 1
        ;
PREPPATCHLIST(PCKINIT,VER,pSHOWARRAY,MODE)
        ;"Purpose: to prepair the patch list for display in scroll box.
        ;"Input: PCKINIT -- this is the namespace of the package to get patches for, e.g. 'DI' for fileman
        ;"       VER -- the package version
        ;"       pSHOWARRAY -- PASS BY NAME, an OUT PARAMATER
        ;"       MODE -- OPTIONAL.  0: Otherwise by IEN order
        ;"                          1: Otherwise by SEQ Num
        ;"                          2: Then sorted by patch number,

        SET ByPATCHNUM=+$GET(ByPATCHNUM)
        NEW INDEX SET INDEX=1
        NEW SHOWI SET SHOWI=1
        KILL @pSHOWARRAY
        NEW ARRAY,TEMPA
        IF $$GETPLIST(.PCKINIT,.VER,"ARRAY")=0 GOTO PPLDONE
        IF MODE=0 GOTO PPL2

        NEW NUM
        FOR  SET INDEX=$ORDER(ARRAY(INDEX)) QUIT:(INDEX="")  DO
        . IF MODE=2 SET NUM=+$PIECE(ARRAY(INDEX)," ",1)
        . ELSE  SET NUM=+$PIECE(ARRAY(INDEX),"SEQ #",2)
        . NEW s,PATCH
        . SET PATCH=PCKINIT_"*"_VER_"*"_$GET(ARRAY(INDEX))
        . SET s=$$LJ^XLFSTR(PATCH,25)
        . IF $DATA(ARRAY(INDEX,"PENDING"))=0 DO
        . . SET s=s_" Applied: "_$GET(ARRAY(INDEX,"i",".02"))_"  "
        . . SET s=s_" By: "_$GET(ARRAY(INDEX,"i",".03"))
        . ELSE  SET s=s_" (available)"
        . SET TEMPA(NUM)=s
        SET NUM=""
        FOR  SET NUM=$ORDER(TEMPA(NUM)) QUIT:(NUM="")  DO
        . NEW s SET s=$GET(TEMPA(NUM)) QUIT:(s="")
        . NEW RETURN SET RETURN=""
        . NEW TAG FOR TAG=" Applied"," (available)" DO
        . . IF s[TAG SET RETURN=$PIECE(s,TAG,1)
        . SET @pSHOWARRAY@(SHOWI,s)=RETURN
        . SET SHOWI=SHOWI+1
        GOTO PPLDONE

PPL2    FOR  SET INDEX=$ORDER(ARRAY(INDEX)) QUIT:(INDEX="")  DO
        . NEW s,PATCH
        . SET PATCH=PCKINIT_"*"_VER_"*"_$GET(ARRAY(INDEX))
        . SET s=$$LJ^XLFSTR(PATCH,25)
        . SET s=s_" Applied: "_$GET(ARRAY(INDEX,"i",".02"))_"  "
        . SET s=s_" By: "_$GET(ARRAY(INDEX,"i",".03"))
        . SET @pSHOWARRAY@(SHOWI,s)=PATCH
        . SET SHOWI=SHOWI+1

PPLDONE QUIT
        ;
HndOnPKP(PARRAY,OPTION,INFO) ;
        ;"Purpose: handle ON SELECT event from Scroller
        ;"Input: PARRAY,OPTION,INFO -- see documentation in Scroller
        ;"       INFO has this: INFO("USER INPUT")=INPUT
        ;"NOTE: uses global-scope vars SET up in ShowPATCHES:
        ;" TMGPCKI,TMGPVER,TMGSORT
        DO HndOnPCmd(.PARRAY,.OPTION,.INFO) ;
        QUIT
        ;
HndOnPCmd(PARRAY,OPTION,INFO) ;
        ;"Purpose: handle ON SELECT event from Scroller
        ;"Input: PARRAY,OPTION,INFO -- see documentation in Scroller
        ;"       INFO has this: INFO("USER INPUT")=INPUT
        ;"NOTE: uses global-scope vars SET up in ShowPATCHES:
        ;" TMGPCKI,TMGPVER,TMGSORT

        NEW INPUT SET INPUT=$$UP^XLFSTR($GET(INFO("USER INPUT")))
        IF INPUT["F1" DO
        . SET TMGSORT=(TMGSORT+1)#3
        . SET OPTION("HEADER",1)="Applied patches for package "_PCKINIT_"   "_TMGSARRAY(TMGSORT)
        . DO PREPPATCHLIST(TMGPCKI,TMGPVER,PARRAY,TMGSORT)
        . NEW NEXTSORT SET NEXTSORT=(TMGSORT+1)#3
        . SET OPTION("FOOTER",1,3)="[F1] "_TMGSARRAY(NEXTSORT)
        ELSE  IF INPUT["F2" DO
        . DO FXMSINIT^TMGPAT1(TMGPCKI,TMGPVER,2)
        . DO PREPPATCHLIST(TMGPCKI,TMGPVER,PARRAY,TMGSORT)
        . DO PRESS2GO^TMGUSRI2
        ELSE  IF INPUT["F3" DO
        . DO FXMSINIT^TMGPAT1(TMGPCKI,TMGPVER,1)
        . DO PREPPATCHLIST(TMGPCKI,TMGPVER,PARRAY,TMGSORT)
        . DO PRESS2GO^TMGUSRI2
        ELSE  IF INPUT["F4" DO
        . NEW OPTION
        . SET OPTION("URL")="foia-vista.worldvista.org/Patches_By_Application/"
        . SET OPTION("SELECT DIR")=0
        . WRITE $$FBROWSE^TMGIOUT2(.OPTION)
        . DO PRESS2GO^TMGUSRI2
        ELSE  IF INPUT="?" DO
        . WRITE !,"Use UP and DOWN cursor keys to scroll.",!
        . WRITE "Press F1 or F2 to change sorting",!
        . WRITE "  (If Function key doesn't work, try",!
        . WRITE "  typing in letters 'F1' or'F2' etc.)",!
        . WRITE "Enter ^ at the ':' prompt when DONE",!
        . DO PRESS2GO^TMGUSRI2
        ELSE  IF INPUT'="" DO
        . WRITE !,"Input ",$GET(INFO("USER INPUT"))," not recognized.",!
        . DO PRESS2GO^TMGUSRI2
        ;
        WRITE #
        QUIT
        ;
EditNotes  ;
        ;"Purpose: to launch an editor for editing notes about patching.
        NEW FPNAME SET FPNAME=$GET(^TMG("KIDS","PATCH DIR"),"/tmp/")_"Patch_Notes.txt"
        IF $$EditHFSFile^TMGKERNL(FPNAME)
        QUIT
        ;
        ;"====================================================================
        ;
CONINSTBRW ;  Console for "INSTALL" file browser.  
        NEW ARRAY,OPTION,TMGOPT2
        SET TMGOPT2("SHOW COMPLETED")=0         
        SET TMGOPT2("SORT BY")="NAME"
        DO PREPINSTARR("ARRAY",.OPTION,.TMGOPT2)
        SET OPTION("FOOTER",1,1)="^ Exit"
        SET OPTION("FOOTER",1,3)="[F1] Sort By NAME"
        SET OPTION("FOOTER",1,4)="[F2] Sort By STATUS"
        SET OPTION("FOOTER",1,5)="[F3] Toggle show COMPLETED"
        SET OPTION("ON SELECT")="HNDONSEL2^TMGPAT3"
        SET OPTION("ON CMD")="HNDONCMD2^TMGPAT3"
        SET OPTION("ON KEYPRESS")="HNDONKP2^TMGPAT3"
        WRITE #
        DO SCROLLER^TMGUSRIF("ARRAY",.OPTION)
        QUIT
        ;
HNDONSEL2(PARRAY,OPTION,INFO)  ;
        ;"Purpose: handle ON SELECT event from Scroller
        ;"Input: PARRAY,OPTION,INFO -- see documentation in Scroller
        ;"        INFO("USER INPUT")=INPUT
        ;"        INFO("CMD")=User full input command so far (being built up)
        ;
        NEW IEN SET IEN=$GET(INFO("CURRENT LINE","RETURN"))
        IF IEN'>0 QUIT
        DO MANAGEINST(IEN)
        WRITE #  ;"clear screen
        QUIT
        ;
HNDONCMD2(PARRAY,OPTION,INFO)  ;
        ;"Purpose: handle ON SELECT event from Scroller
        ;"Input: PARRAY,OPTION,INFO -- see documentation in Scroller
        ;"        INFO("USER INPUT")=INPUT
        ;"        INFO("CMD")=User full input command so far (being built up)
        ;
        QUIT
        ;
HNDONKP2(PARRAY,OPTION,INFO)  ;
        ;"Purpose: handle ON SELECT event from Scroller
        ;"Input: PARRAY,OPTION,INFO -- see documentation in Scroller
        ;"        INFO("USER INPUT")=INPUT
        ;"        INFO("CMD")=User full input command so far (being built up)
        ;"Uses TMGOPT2 in global scope. 
        ;
        NEW INPUT SET INPUT=$GET(INFO("USER INPUT"))
        NEW CMD SET CMD=$GET(INFO("CMD"))
        IF INPUT="{F1}" DO  GOTO HKP2DN
        . SET TMGOPT2("SORT BY")="NAME"
        . DO PREPINSTARR(PARRAY,.OPTION,.TMGOPT2)
        . SET INFO("USER INPUT")=""
        IF INPUT="{F2}" DO  GOTO HKP2DN
        . SET TMGOPT2("SORT BY")="STATUS"
        . DO PREPINSTARR(PARRAY,.OPTION,.TMGOPT2)
        . SET INFO("USER INPUT")=""
        IF INPUT="{F3}" DO  GOTO HKP2DN
        . SET TMGOPT2("SHOW COMPLETED")='$GET(TMGOPT2("SHOW COMPLETED")) 
        . DO PREPINSTARR(PARRAY,.OPTION,.TMGOPT2)
        . SET INFO("USER INPUT")=""
        ELSE  DO
        . SET TMGOPT2("MUST CONTAIN")=CMD            
        . DO PREPINSTARR(PARRAY,.OPTION,.TMGOPT2)
HKP2DN  ;        
        QUIT
        ;
PREPINSTARR(PARRAY,OPTION,OPT2)  ;"PREP INSTALL ARRAY
        ;"Purpose: To prepair an array with patch status, for use with SCROLLER^TMGUSRIF
        ;"Input: PARRAY -- PASS BY NAME.  ARRAY to put info into.  Prior data is killed.
        ;"       OPTION -- PASS BY REFERENCE.  Prior data is NOT killed.  See SCROLLER^TMGUSRIF for details
        ;"       OPT2 -- OPTIONAL.  Options for this function
        ;"         OPT2("SHOW LOADED")       Optional.  Default = 1.  If 1, then shown 
        ;"         OPT2("SHOW QUEUED")       Optional.  Default = 1.  If 1, then shown 
        ;"         OPT2("SHOW STARTED")      Optional.  Default = 1.  If 1, then shown 
        ;"         OPT2("SHOW COMPLETED")    Optional.  Default = 1.  If 1, then shown 
        ;"         OPT2("SHOW DEINSTALLED")  Optional.  Default = 1.  If 1, then shown 
        ;"         OPT2("SORT BY") Optional.  "NAME" -> sort by name;  "STATUS" -> sort by status, then name
        ;"         OPT2("PACKAGE")  Optional.  E.g. "DI".  If provided, then only patches from package shown.
        ;"         OPT2("MUST CONTAIN") Optional.  If provided, line must contain value to be shown. Case INSENSITIVE
        ;"                NOTE: if value contains " ", then each item (divided by space) is checked, and all must be present
        ;"Results: NONE    
        ;
        SET PARRAY=$GET(PARRAY) GOTO:(PARRAY="") PIADONE
        KILL @PARRAY
        NEW STATUS
        SET STATUS(0)="Loaded from Distribution"
        SET STATUS(1)="Queued for Install"
        SET STATUS(2)="Start of Install"
        SET STATUS(3)="Install Completed"
        SET STATUS(4)="De-Installed"
        NEW DESIRED
        IF $GET(OPT2("SHOW LOADED"),1)>0      SET DESIRED(0)=1
        IF $GET(OPT2("SHOW QUEUED"),1)>0      SET DESIRED(1)=1    
        IF $GET(OPT2("SHOW STARTED"),1)>0     SET DESIRED(2)=1
        IF $GET(OPT2("SHOW COMPLETED"),1)>0   SET DESIRED(3)=1
        IF $GET(OPT2("SHOW DEINSTALLED"),1)>0 SET DESIRED(4)=1
        NEW PKG SET PKG=$GET(OPT2("PACKAGE"),"*")
        NEW CONT SET CONT=$$UP^XLFSTR($GET(OPT2("MUST CONTAIN"),"*"))
        NEW CT SET CT=1
        NEW MODE SET MODE=$GET(OPT2("SORT BY"))
        IF MODE="STATUS" DO
        . NEW TEMP
        . NEW IEN SET IEN=0
        . FOR  SET IEN=$ORDER(^XPD(9.7,IEN)) QUIT:IEN'>0  DO
        . . NEW ZN SET ZN=$GET(^XPD(9.7,IEN,0))
        . . NEW ST SET ST=$PIECE(ZN,"^",9)
        . . IF $GET(DESIRED(ST))'>0 QUIT
        . . NEW NAME SET NAME=$PIECE(ZN,"^",1)
        . . IF (PKG'="*"),($PIECE(NAME,"*",1)'=PKG) QUIT
        . . SET TEMP(ST,NAME,IEN)=""
        . NEW ST SET ST=""
        . FOR  SET ST=$ORDER(TEMP(ST)) QUIT:ST=""  DO
        . . NEW NAME SET NAME=""
        . . FOR  SET NAME=$ORDER(TEMP(ST,NAME)) QUIT:NAME=""  DO
        . . . NEW IEN SET IEN=0
        . . . FOR  SET IEN=$ORDER(TEMP(ST,NAME,IEN)) QUIT:IEN'>0  DO
        . . . . NEW LINE SET LINE=$GET(STATUS(ST))_": "_NAME
        . . . . IF (CONT'="*"),($$LINECONT(LINE,CONT)=0) QUIT
        . . . . SET @PARRAY@(CT,LINE)=IEN,CT=CT+1
        ELSE  DO   ;"Sort by name.
        . NEW NAME SET NAME=""
        . FOR  SET NAME=$ORDER(^XPD(9.7,"B",NAME)) QUIT:NAME=""  DO
        . . NEW IEN SET IEN=0
        . . FOR  SET IEN=$ORDER(^XPD(9.7,"B",NAME,IEN)) QUIT:IEN'>0  DO
        . . . NEW ZN SET ZN=$GET(^XPD(9.7,IEN,0))
        . . . NEW NAME SET NAME=$PIECE(ZN,"^",1)
        . . . NEW ST SET ST=$PIECE(ZN,"^",9)
        . . . IF $GET(DESIRED(ST))'>0 QUIT
        . . . IF ST'="",(ST>=0),(ST<=4) SET ST=$GET(STATUS(ST))
        . . . IF (PKG'="*"),($PIECE(NAME,"*",1)'=PKG) QUIT
        . . . NEW LINE SET LINE=NAME_" ("_ST_")"
        . . . IF (CONT'="*"),($$LINECONT(LINE,CONT)=0) QUIT
        . . . SET @PARRAY@(CT,LINE)=IEN,CT=CT+1
        SET OPTION("HEADER",1)="TMG 'INSTALL' (#9.7) Fileman File Browser"        
PIADONE ;
        QUIT
        ;
LINECONT(LINE,CONT,DIV)  ;"Extended "Line Contains" function
        SET DIV=$GET(DIV," ")
        NEW UPLINE SET UPLINE=$$UP^XLFSTR(LINE)
        SET CONT=$$UP^XLFSTR(CONT)
        NEW RESULT SET RESULT=1  ;"default to success of contain
        NEW IDX FOR IDX=1:1:$LENGTH(CONT,DIV) QUIT:(RESULT=0)  DO
        . NEW PART SET PART=$PIECE(CONT,DIV,IDX)
        . IF UPLINE'[PART SET RESULT=0
        QUIT RESULT
        ;
MANAGEINST(IEN)  ;"Manage record from file 9.7 (INSTALL)
        ;"Input: IEN -- IEN in file 9.7
        NEW MENU,IDX,USRPICK
MIL1    NEW ZN SET ZN=$GET(^XPD(9.7,IEN,0))
        IF ZN="" GOTO MIDN
        NEW NAME SET NAME=$PIECE(ZN,"^",1)
        NEW ST SET ST=$PIECE(ZN,"^",9)
        NEW STS SET STS="" IF ST'="",(ST>=0),(ST<=4) SET STS=$GET(STATUS(ST)) 
        SET IDX=0  
        KILL MENU SET MENU(IDX)="Select Option For INSTALL (File #9.7) record"
        SET MENU(IDX,1)="INSTALL Name: '"_NAME_"'"
        SET IDX=IDX+1,MENU(IDX)="Display Record"_$CHAR(9)_"DUMP"        
	    NEW Y SET Y=+$G(IEN) 
	    IF Y>0,$D(^XPD(9.7,"ASP",Y,1,Y)),$D(^XTMP("XPDI",Y)) DO
	    . ;"NOTE: Screening code from EN1^XPDIU
        . SET IDX=IDX+1,MENU(IDX)="Uninstall Loaded INSTALL"_$CHAR(9)_"UNINSTALL"          
        IF ST=0,$D(^XPD(9.7,"ASP",Y,1,Y)),$D(^XTMP("XPDI",Y)) DO
        . ;"NOTE: Screening code from EN^XPDI
        . SET IDX=IDX+1,MENU(IDX)="Start Installation"_$CHAR(9)_"START"          
        IF "^Install Completed^De-Installed"'["^"_STS_"^" DO
        . SET IDX=IDX+1,MENU(IDX)="Restart Installation"_$CHAR(9)_"RESTART"  
        SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
        IF USRPICK="^" GOTO MIDN  
        IF USRPICK="DUMP" DO  GOTO MIL1
        . DO DUMPREC^TMGDEBU3(9.7,IEN,1)
        . DO PRESS2GO^TMGUSRI2
        IF USRPICK="UNINSTALL" DO  GOTO MIL1
        . DO EN1^XPDIU(IEN)
        . DO PRESS2GO^TMGUSRI2
        IF USRPICK="START" DO  GOTO MIL1
        . NEW TMGXPDI1 SET TMGXPDI1=IEN  ;"Will be used in global scope by LOOK^XPDI1
        . DO EN^XPDI
        . DO PRESS2GO^TMGUSRI2
        GOTO MIL1
MIDN    ;
        QUIT        