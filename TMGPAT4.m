TMGPAT4  ;TMG/kst/Patching tools ;09/22/08, 2/2/14, 8/4/2022
         ;;1.0;TMG-LIB;**1**;09/22/08
 ;
 ;"Kevin Toppenberg MD
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
 ;"ANALYZE(INFO,OPTION) -- look at a patch TXT file and extract useful information
 ;"ShowAnalysis(INFO,Msg) -- display to user analysis info in a meaningful way.
 ;
 ;"=======================================================================
 ;"Private Functions
 ;"=======================================================================
 ;"GETREQPATCHES(ARRAY,INFO) -- scan ARRAY, holding TXT file, and assemble list of required patches
 ;"GETCATEGORY(ARRAY,INFO) -- scan ARRAY, holding TXT file, and assemble category entries
 ;"GETFNAMEs(ARRAY,INFO) -- scan ARRAY, holding TXT file, and scavenge and .KID file names
 ;"GETMULTPATCHES(ARRAY,INFO,OPTION) -- scan ARRAY, holding TXT file, and matching multi-patch info
 ;"GETSEQ(PATCHNAME,MODE,OPTION,URL) -- For a given patch (e.g. DI*12*123), return the SEQ #
 ;"FindMultPATCH(PATCHNAME,PCKINIT,OPTION,URL) --
 ;"CHECKDELTA(INFO) -- compare the requirements in INFO vs existing system.
 ;"PANALYZE(INFO,OPTION) -- look at a patch KID file and extract useful information
 ;"PGETREQPATCHES(ARRAY,INFO) -- scan ARRAY, holding KID file, and assemble list of required patches
 ;"CheckLocal(PARRAY,OPTION) -- check a KIDS for conflict with local modifications.
 ;"CHKLOCLRTN(ROUTINE) -- see IF one routine has any local modifications.
 ;
 ;"=======================================================================
 ;
ANALYZE(INFO,OPTION)
        ;"Purpose: To look at a patch TXT file and extract useful information
        ;"Input:  INFO -- PASS BY REFERENCE, and IN and OUT PARAMETER.
        ;"          Input:      INFO("PATH") -- path on HFS of TXT file
        ;"                      INFO("TEXT FILE") -- FILENAME on HFS of TXT file
        ;"          Output:
        ;"                      INFO("SPECIFIED REQ",PATCHNAME)=""
        ;"                      INFO("SPECIFIED REQ",PATCHNAME)=""
        ;"                      INFO("MULTI-PATCH","FILENAME")=FileName
        ;"                      INFO("MULTI-PATCH","CONTAINS",PATCHNAME)=""
        ;"                      INFO("MULTI-PATCH","CONTAINS",PATCHNAME,"LATEST IN THIS PACKAGE")=lastPatch
        ;"       OPTION -- optional.  Pass by reference.
        ;"              OPTION("VERBOSE")=1, means messaages also written directly to output
        ;"
        ;"Results: NONE
        ;
        NEW FPATH,FNAME,ARRAY
        SET FPATH=$GET(INFO("PATH")) IF FPATH="" GOTO ADONE
        SET FNAME=$GET(INFO("TEXT FILE")) IF FNAME="" GOTO ADONE
        IF $$FTG^%ZISH(FPATH,FNAME,"ARRAY(0)",1)=0 GOTO ADONE
        DO ARRANALYZE(.ARRAY,.INFO,.OPTION)
ADONE   QUIT
        ;
ARRANALYZE(ARRAY,INFO,OPTION)        
        DO GETREQPATCHES(.ARRAY,.INFO)
        DO GETCATEGORY(.ARRAY,.INFO)
        DO GETMULTPATCHES(.ARRAY,.INFO,.OPTION)
        DO GETFNAMEs(.ARRAY,.INFO)
        DO GETSUBJECT(.ARRAY,.INFO)
        DO CHECKDELTA(.INFO)
        QUIT
        ;
PANALYZE(INFO,OPTION)
        ;"Purpose: To look at a patch KID file and extract useful information
        ;"Input:  INFO -- PASS BY REFERENCE, and IN and OUT PARAMETER.
        ;"          Input:      INFO("PATH") -- path on HFS of TXT file
        ;"                      INFO("KID FILE") -- FILENAME on HFS of KID file
        ;"          Output:     INFO("SPECIFIED REQ",PATCHNAME)=""
        ;"                      INFO("SPECIFIED REQ",PATCHNAME)=""
        ;"       OPTION -- optional.  Pass by reference.
        ;"              OPTION("VERBOSE")=1, means messaages also written directly to output
        ;"Results: NONE
        ;
        NEW FPATH,FNAME,ARRAY
        SET FPATH=$GET(INFO("PATH")) IF FPATH="" GOTO PADONE
        SET FNAME=$GET(INFO("KID FILE")) IF FNAME="" GOTO PADONE
        IF $$FTG^%ZISH(FPATH,FNAME,"ARRAY(0)",1)=0 GOTO PADONE
        DO PGETREQPATCHES(.ARRAY,.INFO)
        ;"do PCheckDelta(.INFO)
PADONE   QUIT
        ;
GETREQPATCHES(ARRAY,INFO)
        ;"Purpose: to scan ARRAY, holding TXT file, and assemble list of required patches
        ;"Input: ARRAY -- PASS BY REFERENCE -- holds TXT file
        ;"       INFO  -- PASS BY REFERENCE.  Data added as follows:
        ;"              INFO("SPECIFIED REQ",PATCHNAME)=""
        ;"Results: NONE

        NEW DONE SET DONE=0
        NEW FOUNDLIST SET FOUNDLIST=0
        NEW i SET i=""
        FOR  SET i=$ORDER(ARRAY(i)) QUIT:(i="")!DONE  DO
        . NEW s SET s=$$TRIM^XLFSTR($GET(ARRAY(i)))
        . SET s=$$STRIP^XLFSTR(s,$CHAR(10))
        . SET s=$$STRIP^XLFSTR(s,$CHAR(13))
        . IF (FOUNDLIST=1)&(s="") SET DONE=1 QUIT
        . IF s["Associated patches:" SET FOUNDLIST=1
        . IF FOUNDLIST DO
        . . NEW ONEPATCH
        . . SET ONEPATCH=$$TRIM^XLFSTR($PIECE($PIECE(s,")",2)," ",1))  ;"e.g.  (v)PX*1*29     <<= must be installed BEFORE `PX*1*121'
        . . IF ONEPATCH="" QUIT
        . . SET INFO("SPECIFIED REQ",ONEPATCH)=""
        QUIT

PGETREQPATCHES(ARRAY,INFO)
        ;"Purpose: to scan ARRAY, holding KID file, and assemble list of required patches
        ;"Input: ARRAY -- PASS BY REFERENCE -- holds TXT file
        ;"       INFO  -- PASS BY REFERENCE.  Data added as follows:
        ;"              INFO("SPECIFIED REQ",PATCHNAME)=""
        ;"Results: NONE
        ;
        NEW DONE SET DONE=0
        NEW FOUNDLIST SET FOUNDLIST=0
        NEW i SET i=""
        FOR  SET i=$ORDER(ARRAY(i)) QUIT:(i="")!DONE  DO
        . NEW s SET s=$$TRIM^XLFSTR($GET(ARRAY(i)))
        . SET s=$$STRIP^XLFSTR(s,$CHAR(10))
        . SET s=$$STRIP^XLFSTR(s,$CHAR(13))
        . IF s'["REQB" QUIT
        . IF (s["""REQB"",""B""")!(s["""REQB"",0") QUIT
        . SET i=$ORDER(ARRAY(i)) QUIT:(i="")
        . NEW ONEPATCH SET ONEPATCH=$PIECE($GET(ARRAY(i)),"^",1) QUIT:(ONEPATCH="")
        . IF $PIECE(ONEPATCH,",",1)="""RTN""" QUIT
        . IF $PIECE(ONEPATCH,",",1)="""KRN""" QUIT
        . SET INFO("SPECIFIED REQ",ONEPATCH)=""
        QUIT

GETCATEGORY(ARRAY,INFO)
        ;"Purpose: to scan ARRAY, holding TXT file, and assemble category entries
        ;"Input: ARRAY -- PASS BY REFERENCE -- holds TXT file
        ;"       INFO  -- PASS BY REFERENCE.  Data added as follows:
        ;"              INFO("PATCH CATEGORY",NAME)=""
        ;"Results: NONE

        NEW DONE SET DONE=0
        NEW FOUNDLIST SET FOUNDLIST=0
        NEW i SET i=""
        FOR  SET i=$ORDER(ARRAY(i)) QUIT:(i="")!DONE  DO
        . NEW s SET s=$$TRIM^XLFSTR($GET(ARRAY(i)))
        . SET s=$$STRIP^XLFSTR(s,$CHAR(10))
        . SET s=$$STRIP^XLFSTR(s,$CHAR(13))
        . IF (FOUNDLIST=1)&(s="") SET DONE=1 QUIT
        . IF s["Category:" SET FOUNDLIST=1
        . IF FOUNDLIST DO
        . . NEW ONEPATCH
        . . SET ONEPATCH=$$TRIM^XLFSTR($PIECE(s,"-",2))  ;"e.g.  - Routine
        . . IF ONEPATCH="" QUIT
        . . SET INFO("PATCH CATEGORY",ONEPATCH)=""
        QUIT

GETFNAMEs(ARRAY,INFO)
        ;"Purpose: to scan ARRAY, holding TXT file, and scavenge and .KID file names
        ;"Input: ARRAY -- PASS BY REFERENCE -- holds TXT file
        ;"       INFO  -- PASS BY REFERENCE.  Data added as follows:
        ;"              INFO("MISC KID FILES",NAME)=""
        ;"Results: NONE

        NEW i SET i=""
        FOR  SET i=$ORDER(ARRAY(i)) QUIT:(i="")  DO
        . NEW s SET s=$$TRIM^XLFSTR($GET(ARRAY(i)))
        . SET s=$$STRIP^XLFSTR(s,$CHAR(10))
        . SET s=$$STRIP^XLFSTR(s,$CHAR(13))
        . IF $$UP^XLFSTR(s)[".KID" DO   ;"NOTE: this system will only get 1 filename per line...
        . . NEW kidS
        . . IF s[".kid" SET kidS=".kid"
        . . ELSE  SET kidS=".KID"
        . . IF $PIECE(s,kidS,1)="" QUIT
        . . NEW FNAME SET FNAME=$$GETWORD^TMGSTUT3(s,$FIND(s,kidS)-1," "," ")
        . . IF $EXTRACT(FNAME,$LENGTH(FNAME))="." SET FNAME=$$TRIM^XLFSTR(FNAME,"R",".")
        . . IF (FNAME="") QUIT
        . . SET INFO("MISC KID FILES",FNAME)=""
        QUIT

GETSUBJECT(ARRAY,INFO)
        ;"Purpose: to scan ARRAY, holding TXT file, and scavenge SUBJECT line
        ;"Input: ARRAY -- PASS BY REFERENCE -- holds TXT file
        ;"       INFO  -- PASS BY REFERENCE.  Data added as follows:
        ;"              INFO("SUBJECT",NAME)=""
        ;"Results: NONE

        NEW i SET i=""
        NEW DONE SET DONE=0
        FOR  SET i=$ORDER(ARRAY(i)) QUIT:(i="")!DONE  DO
        . NEW s SET s=$$TRIM^XLFSTR($GET(ARRAY(i)))
        . SET s=$$STRIP^XLFSTR(s,$CHAR(10))
        . SET s=$$STRIP^XLFSTR(s,$CHAR(13))
        . IF s["Subject:" DO
        . . NEW subj SET subj=$PIECE(s,"Subject:",2)
        . . IF subj="" QUIT
        . . SET INFO("SUBJECT",subj)=""
        . . SET DONE=1
        QUIT

GETMULTPATCHES(ARRAY,INFO,OPTION)
        ;"Purpose: to scan ARRAY, holding TXT file, and matching multi-patch info
        ;"Input: ARRAY -- PASS BY REFERENCE -- holds TXT file
        ;"       INFO  -- PASS BY REFERENCE.  Data added as follows:
        ;"              INFO("MULTI-PATCH","FILENAME")=FileName
        ;"              INFO("MULTI-PATCH","CONTAINS",PATCHNAME)=""
        ;"       OPTION -- optional.  Pass by reference.
        ;"              OPTION("VERBOSE")=1, means messaages also written directly to output
        ;"Results: NONE

        NEW DONE SET DONE=0
        NEW FOUNDLIST,FOUNDSection SET (FOUNDLIST,FOUNDSection)=0
        NEW fileNAME SET fileNAME=""
        NEW i SET i=""
        FOR  SET i=$ORDER(ARRAY(i)) QUIT:(i="")!DONE  DO
        . NEW s SET s=$$TRIM^XLFSTR($GET(ARRAY(i)))
        . IF (FOUNDLIST=1)&(s="") SET DONE=1 QUIT
        . IF s["SOFTWARE AND DOCUMENTATION RETRIEVAL" DO
        . . SET FOUNDSection=1
        . IF FOUNDLIST DO  QUIT
        . . IF s["---" QUIT
        . . IF s="" SET DONE=1 QUIT
        . . NEW TEMPS SET TEMPS=$$TRIM^XLFSTR(s)
        . . NEW ONEPATCH
        . . SET ONEPATCH=$$GETWORD^TMGSTUT3(TEMPS,$LENGTH(TEMPS)-1)
        . . IF ONEPATCH="" QUIT
        . . NEW TEMPNAME SET TEMPNAME=$$GETSEQ(ONEPATCH,1)
        . . IF TEMPNAME="" SET ONEPATCH=ONEPATCH_" SEQ #???"
        . . ELSE  SET ONEPATCH=TEMPNAME
        . . SET INFO("MULTI-PATCH","CONTAINS",ONEPATCH)=""
        . IF FOUNDSection DO
        . . IF s["Patch(es)" SET FOUNDLIST=1 QUIT
        . . NEW TEMPS SET TEMPS=$$UP^XLFSTR(s)
        . . IF TEMPS[".KID" DO
        . . . NEW p SET p=$FIND(TEMPS,".KID")-1
        . . . SET TEMPS=$$GETWORD^TMGSTUT3(s,p) QUIT:TEMPS=""
        . . . FOR  QUIT:($$UP^XLFSTR($EXTRACT(TEMPS,$LENGTH(TEMPS)))="D")!(TEMPS="")  DO
        . . . . SET TEMPS=$EXTRACT(TEMPS,1,$LENGTH(TEMPS)-1)
        . . . SET INFO("MULTI-PATCH","FILENAME")=TEMPS

        QUIT


GETSEQ(PATCHNAME,MODE,OPTION,URL)
        ;"Purpose: For a given patch (e.g. DI*12*123), return name with the SEQ #
        ;"Input: PATCHNAME -- e.g. DI*12*123
        ;"       MODE -- OPTIONAL (0 is default) -- 0: search .KID & .TXT; 1: search .TXT files only, 2: search .KID only
        ;"       OPTION -- optional.  Pass by reference.
        ;"              OPTION("VERBOSE")=1, means messaages also written directly to output
        ;"       URL -- PASS BY REFERENCE, an OUT PARAMETER
        ;"Output: in addition to OUT PARAMETERS, the following global variables are SET:
        ;"  SET ^TMG("KIDS","PATCH NAMES",PCKINIT,VER,PATCHNUM,ONESEQNUM)=""
        ;"  SET ^TMG("KIDS","PATCH NAMES",PCKINIT,"COMBINED",RESULT)=URL
        ;"Results: returns patch name with seq #, e.g. 'DI*12*123 SEQ #123'
        ;"         or "" IF problem.

        NEW RESULT SET RESULT=""
        NEW SEQNUM,PCKINIT,VER,PATCHNUM,SEQNUM
        DO PRSEPATCHNAME^TMGPAT2(PATCHNAME,.PCKINIT,.VER,.PATCHNUM,.SEQNUM)
        IF PCKINIT="" GOTO GSQDN
        IF VER="" GOTO GSQDN
        IF PATCHNUM="" GOTO GSQDN
        IF +SEQNUM>0 GOTO GSQL1
        NEW IEN9D4 SET IEN9D4=+$ORDER(^DIC(9.4,"C",PCKINIT,""))
        NEW IENPCK SET IENPCK=+$ORDER(^TMG(22709,"B",IEN9D4,""))
        IF IENPCK'>0 GOTO GSQDN
        NEW IENVER SET IENVER=+$ORDER(^TMG(22709,IENPCK,1,"B",VER,""))
        IF IENVER'>0 GOTO GSQDN
        NEW IENPAT SET IENPAT=+$ORDER(^TMG(22709,IENPCK,1,IENVER,1,"B",PATCHNUM,""))
        IF IENPAT'>0 GOTO GSQDN
        NEW ZN SET ZN=$GET(^TMG(22709,IENPCK,1,IENVER,1,IENPAT,0))
        SET SEQNUM=$PIECE(ZN,"^",2)
GSQL1   SET RESULT=$$MAKEPATCHNAME^TMGPAT2(PCKINIT,VER,PATCHNUM,SEQNUM)
        GOTO GSQDN
GSqDONE IF RESULT="" DO
        . IF (PCKINIT="")!(VER="")!(PATCHNUM="") QUIT
        . SET ^TMG("KIDS","PATCH NAMES",PCKINIT,VER,PATCHNUM)="???"
GSQDN   QUIT RESULT

PATCHDIRNAME(PCKINIT) ;
        QUIT "Remote_Patches-dirFor-"_PCKINIT


FindMultPATCH(PATCHNAME,PCKINIT,OPTION,URL,INFO)
        ;"Purpose: Search through downloaded directory file for patch, and return URL
        ;"Input: PATCHNAME -- e.g. CSV_12_1234.KID
        ;"       PCKINIT -- the initials for the package.
        ;"       OPTION -- optional.  Pass by reference.
        ;"              OPTION("VERBOSE")=1, means messaages also written directly to output
        ;"       URL -- PASS BY REFERENCE, an OUT PARAMETER
        ;"       INFO -- PASS BY REFERENCE
        ;"              INFO("KID URL")=URL on server for KID file
        ;"              INFO("TEXT URL")=URL on server for TXT file
        ;"Results: 1 IF found, 0 IF not found or problem.

        NEW RESULT SET RESULT=0
        SET PATCHNAME=$GET(PATCHNAME) GOTO:(PATCHNAME="") FMPDONE
        NEW VERBOSE SET VERBOSE=$GET(OPTION("VERBOSE"))

        NEW FPATH,FNAME,ARRAY,ABORT
        SET ABORT=0
        SET FPATH=$GET(^TMG("KIDS","PATCH DIR"),"/tmp/")
        SET FNAME=$$PATCHDIRNAME(PCKINIT) ;
        IF '$$FILEXIST^TMGIOUTL(FPATH_FNAME) DO
        . IF VERBOSE DO
        . . WRITE "Finding Patch: ",PATCHNAME,!
        . . WRITE "Getting directory information from VA ftp server..."
        . ;"NOTE: GetPckList is depreciated.  Should be rewritten for newer code....
        . ;"IF $$GetPckList^TMGKERNL(PCKINIT,.ARRAY,1)=0 SET ABORT=1  ;GetPckList is depreciated.  rewritten for newer code....
        . IF $$GETPATL^TMGKERN4(PCKINIT,.ARRAY,1)=0 SET ABORT=1
        . IF VERBOSE WRITE " Done.",!
        ELSE  DO
        . IF $$FTG^%ZISH(FPATH,FNAME,"ARRAY(0)",1)=0 SET ABORT=1 QUIT
        IF ABORT GOTO FMPDONE

        NEW i SET i=0  ;"skip first line, a header line
        FOR  SET i=$ORDER(ARRAY(i)) QUIT:(i="")!(RESULT=1)  DO
        . NEW NAME,PATH,FULLNAMEPATH,TEMPNAME
        . SET FULLNAMEPATH=$GET(ARRAY(i)) QUIT:FULLNAMEPATH=""
        . DO SPLITFPN^TMGIOUTL(FULLNAMEPATH,.PATH,.NAME,"/")
        . IF NAME="" QUIT
        . IF $$UP^XLFSTR(NAME)=$$UP^XLFSTR(PATCHNAME) DO
        . . SET URL=FULLNAMEPATH
        . . SET RESULT=1

        IF RESULT=0 DO   ;"last try to come up with filename.
        . NEW serverPATH SET serverPATH=$GET(INFO("KID URL"))
        . IF serverPATH="" SET serverPATH=$GET(INFO("TEXT URL"))
        . SET serverPATH=$$PATHEXTR^TMGIOUTL(serverPATH)
        . SET serverPATH=$PIECE(serverPATH,"ftp://",2)
        . SET URL=serverPATH_PATCHNAME
        . SET RESULT=1  ;"this is not a sure 'find', more of a hopeful guess that it will be in same directory.

FMPDONE
        QUIT RESULT



CHECKDELTA(INFO)
        ;"Purpose: to compare the requirements in INFO (as created by ANALYZE) and
        ;"         determine differences in existing system.
        ;"Input: INFO.  PASS BY REFERENCE
        ;"          INFO("PATH") -- path on HFS of TXT file
        ;"          INFO("TEXT FILE") -- FILENAME on HFS of TXT file
        ;"          INFO("SPECIFIED REQ",PATCHNAME)=""
        ;"          INFO("MULTI-PATCH","FILENAME")=FileNAME
        ;"          INFO("MULTI-PATCH","CONTAINS",PATCHNAME)=""
        ;"          INFO("MULTI-PATCH","CONTAINS",PATCHNAME,"LATEST IN THIS PACKAGE")=lastPATCH
        ;"Output:
        ;"          INFO("SPECIFIED REQ",PATCHNAME)="OK, Installed." or "Still Needed"
        ;"          INFO("STILL NEEDED",reqPATCH)=""

        NEW reqPATCH SET reqPATCH=""
        FOR  SET reqPATCH=$ORDER(INFO("SPECIFIED REQ",reqPATCH)) QUIT:(reqPATCH="")  DO
        . NEW TEMPS
        . IF $$IsInstalled^TMGPAT2(reqPATCH) SET TEMPS="OK, Installed."
        . ELSE  DO
        . . SET TEMPS="Still Needed."
        . . NEW s2 SET s2=$$GETSEQ(reqPATCH) IF s2="" SET s2=reqPATCH
        . . SET INFO("STILL NEEDED",s2)=""
        . SET INFO("SPECIFIED REQ",reqPATCH)=TEMPS

        NEW PATCHNAME SET PATCHNAME=""
        FOR  SET PATCHNAME=$ORDER(INFO("MULTI-PATCH","CONTAINS",PATCHNAME)) QUIT:(PATCHNAME="")  DO
        . NEW PCKINIT SET PCKINIT=$PIECE(PATCHNAME,"*",1)
        . NEW VER SET VER=$PIECE(PATCHNAME,"*",2)
        . SET PATCHNUM=$PIECE($PIECE(PATCHNAME,"*",3)," ",1)
        . NEW SEQNUM SET SEQNUM=$PIECE(PATCHNAME,"SEQ #",2)
        . IF (PCKINIT="")!(VER="")!(PATCHNUM="") QUIT
        . NEW lastPATCH SET lastPATCH=$$GETLSTPK^TMGPAT1(PCKINIT,VER) ;"returns e.g. 'DI*22.0*140 SEQ# 123'
        . SET INFO("MULTI-PATCH","CONTAINS",PATCHNAME,"LATEST IN THIS PACKAGE")=lastPATCH
        . NEW lastSEQNUM SET lastSEQNUM=$PIECE(lastPATCH,"SEQ #",2)
        . IF lastSEQNUM<SEQNUM DO
        . . SET INFO("GAPPED PATCHES",PATCHNAME)="Currently at: "_lastPATCH

        QUIT

ShowAnalysis(INFO,Msg)
        ;"Purpose: To display analysis info in a meaningful way.
        ;"Input: INFO -- PASS BY REFERENCE.  INFO as created by ANALYZE(INFO,OPTION)
        ;"       Msg -- PASS BY REFERANCE, an OUT PARAMETER
        ;"              Errors are stored in Msg("ERROR",x)=Message
        ;"                                   Msg("ERROR")=COUNT of last error
        ;"              Message are store in Msg(x)=Message
        ;"                                   Msg=COUNT of last message+1

        NEW TEMPMsg,someReq
        MERGE TEMPMsg=Msg
        SET someReq=0
        DO AddMsg^TMGPAT2("According to INFO TXT file,",0,.TEMPMsg)
        DO AddMsg^TMGPAT2("Before this patch is applied, other patches should have been installed first:",0,.TEMPMsg)
        NEW i SET i=""
        FOR  SET i=$ORDER(INFO("SPECIFIED REQ",i)) QUIT:(i="")  DO
        . NEW s SET s=$$GETSEQ(i) IF s="" SET s=i
        . SET s=$$LJ^XLFSTR(s,25)
        . SET someReq=1
        . NEW PCKINIT SET PCKINIT=$PIECE(s,"*",1)
        . NEW VER SET VER=$PIECE(s,"*",2)
        . NEW LASTPCK SET LASTPCK=$$GETLSTPK^TMGPAT1(PCKINIT,VER)
        . NEW TEMPS SET TEMPS="    "_s_" <-- "_$GET(INFO("SPECIFIED REQ",i))
        . IF LASTPCK'="" SET TEMPS=TEMPS_" Current at: "_LASTPCK
        . DO AddMsg^TMGPAT2(TEMPS,0,.TEMPMsg)
        DO AddMsg^TMGPAT2(" ",0,.TEMPMsg)
        IF someReq=1 DO
        . KILL Msg MERGE Msg=TEMPMsg

        IF $GET(INFO("MULTI-PATCH","FILENAME"))="" GOTO SA2
        SET someReq=0
        DO AddMsg^TMGPAT2("This patch is a Multi-Patch.  It contains patches for the following packages: ",0,.Msg)
        KILL TEMPMsg MERGE TEMPMsg=Msg
        SET i=""
        FOR  SET i=$ORDER(INFO("MULTI-PATCH","CONTAINS",i)) QUIT:(i="")  DO
        . NEW curSys SET curSys=$GET(INFO("MULTI-PATCH","CONTAINS",i,"LATEST IN THIS PACKAGE"))
        . NEW s SET s=$$LJ^XLFSTR(i,25)
        . SET someReq=1
        . DO AddMsg^TMGPAT2("  "_s_" [This system is currently at: "_curSys_"]",0,.TEMPMsg)
        IF someReq=1 DO
        . KILL Msg MERGE Msg=TEMPMsg
        DO AddMsg^TMGPAT2("This multipatch is combined into file: "_$GET(INFO("MULTI-PATCH","FILENAME")),0,.Msg)
SA2
        NEW subj SET subj=$ORDER(INFO("SUBJECT",""))
        IF subj'="" DO
        . DO AddMsg^TMGPAT2("PATCH SUBJECT: "_subj,0,.Msg)

SADONE
        QUIT


CheckLocal(PARRAY,OPTION)
        ;"Purpose: to check a KIDS installation file, before it is applied, to ensure
        ;"         that it doesn't conflict with any local modifications.
        ;"         --The means of detecting local mods to be enhanced with time.
        ;"Input -- PARRAY -- PASS BY NAME.  This should be a reference to ^XTMP("XPDI",XPDA,
        ;"                   for the installation being considered.  Thus is should have this format:
        ;"                      @PARRAY@("BLD")
        ;"                      @PARRAY@("MBREQ")
        ;"                      @PARRAY@("PKG")
        ;"                      @PARRAY@("QUES")
        ;"                      @PARRAY@("RTN",RoutineNAME)  <--- the key part
        ;"         OPTION -- PASS BY REFERENCE
        ;"              OPTION("VERBOSE")=1 Optional, 1=show output
        ;"Results: 1 IF conflict, 0 IF OK.
        NEW RESULT SET RESULT=0
        NEW MATCHES
        NEW ROUTINE SET ROUTINE=""
        FOR  SET ROUTINE=$ORDER(@PARRAY@("RTN",ROUTINE)) QUIT:(ROUTINE="")  DO
        . NEW DETAIL
        . IF $$CHKLOCLRTN(ROUTINE,.DETAIL,.MATCHES)=1 DO
        . . SET RESULT=1
        . . IF $GET(OPTION("VERBOSE"))'=1 QUIT
        . . WRITE "WARNING: Importing routine ",ROUTINE," will OVERWRITE local changes!",!
        . . NEW IDX SET IDX=0
        . . FOR  SET IDX=$ORDER(DETAIL(IDX)) QUIT:IDX'>0  DO
        . . . WRITE "   Line #",IDX,".  ",$GET(DETAIL(IDX)),!
        QUIT RESULT


CHKLOCLRTN(ROUTINE,DETAIL,MATCHES)  ;"CHECK LOCAL ROUTINE
        ;"Purpose: to see IF one routine has any local modifications.
        ;"Input: ROUTINE -- the routine name  e.g. XUP
        ;"       DETAIL -- PASS BY REFERENCE, AN OUT PARAMETER.  Filled with lines with local tag.  
        ;"          DETAIL(<LINE#>)=<LINE WITH TAG>
        ;"       MATCHES -- pass by reference.  An array to hold data for matches, for faster repeat processing.  
        ;"Results: 1 if conflict, 0 if OK.
        NEW RESULT SET RESULT=0
        NEW LINE SET LINE=0
        IF $DATA(MATCHES)=0 DO
        . NEW DONE SET DONE=0
        . NEW IDX FOR IDX=1:1 DO  QUIT:(DONE)!(IDX>100)
        . . NEW TAG SET TAG=$TEXT(CKRTNDAT+IDX^TMGPAT4)
        . . IF TAG["<DONE>" SET DONE=1 QUIT
        . . SET TAG=$$UP^XLFSTR($PIECE(TAG,";;",2))
        . . SET MATCHES(TAG)=""
        NEW BLANKCT SET BLANKCT=0
        FOR  DO  QUIT:(BLANKCT>10)!RESULT
        . NEW REF SET REF="+"_LINE_"^"_ROUTINE
        . NEW STR SET STR=$$UP^XLFSTR($TEXT(@REF))
        . IF STR="" SET BLANKCT=BLANKCT+1
        . NEW ATAG SET ATAG=""
        . FOR  SET ATAG=$ORDER(MATCHES(ATAG)) DO  QUIT:ATAG=""
        . . IF ATAG="" QUIT
        . . IF STR[ATAG SET DETAIL(LINE)=$PIECE(STR,ATAG,1)_"****"_ATAG_"****"_$PIECE(STR,ATAG,2,99)
        . ;"SET RESULT=(STR["/kt")!(STR["/tmg")!(STR["/elh")!(STR["FOIA")
        . ;"IF RESULT SET DETAIL(LINE)=STR
        . SET LINE=LINE+1
        SET RESULT=($DATA(DETAIL)>0)
        QUIT RESULT
CKRTNDAT ;        
        ;;WorldVistA
        ;;/kt
        ;;/elh
        ;;tmg
        ;;FOIA
        ;;osehra
        ;;WVEHR
        ;;VEFA
        ;;<DONE>
        