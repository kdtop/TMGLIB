TMGPAT4  ;TMG/kst/Patching tools ;09/22/08, 2/2/14, 2/15/15
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
 ;"Analyze(Info,OPTION) -- look at a patch TXT file and extract useful information
 ;"ShowAnalysis(Info,Msg) -- display to user analysis info in a meaningful way.

 ;"=======================================================================
 ;"Private Functions
 ;"=======================================================================
 ;"GETReqPATCHES(ARRAY,Info) -- scan ARRAY, holding TXT file, and assemble list of required patches
 ;"GETCategory(ARRAY,Info) -- scan ARRAY, holding TXT file, and assemble category entries
 ;"GETFNAMEs(ARRAY,Info) -- scan ARRAY, holding TXT file, and scavenge and .KID file names
 ;"GETMultPATCHES(ARRAY,Info,OPTION) -- scan ARRAY, holding TXT file, and matching multi-patch info
 ;"GETSEQ(PATCHNAME,MODE,OPTION,URL) -- For a given patch (e.g. DI*12*123), return the SEQ #
 ;"FindMultPATCH(PATCHNAME,PCKINIT,OPTION,URL) --
 ;"CheckDelta(Info) -- compare the requirements in Info vs existing system.
 ;"PAnalyze(Info,OPTION) -- look at a patch KID file and extract useful information
 ;"PGETReqPATCHES(ARRAY,Info) -- scan ARRAY, holding KID file, and assemble list of required patches
 ;"CheckLocal(PARRAY,OPTION) -- check a KIDS for conflict with local modifications.
 ;"Chk1Routine(routine) -- see IF one routine has any local modifications.

 ;"=======================================================================

Analyze(Info,OPTION)
        ;"Purpose: To look at a patch TXT file and extract useful information
        ;"Input:  Info -- PASS BY REFERENCE, and IN and OUT PARAMETER.
        ;"          Input:      Info("PATH") -- path on HFS of TXT file
        ;"                      Info("TEXT FILE") -- FILENAME on HFS of TXT file
        ;"          Output:
        ;"                      Info("SPECIFIED REQ",PATCHNAME)=""
        ;"                      Info("SPECIFIED REQ",PATCHNAME)=""
        ;"                      Info("MULTI-PATCH","FILENAME")=FileName
        ;"                      Info("MULTI-PATCH","CONTAINS",PATCHNAME)=""
        ;"                      Info("MULTI-PATCH","CONTAINS",PATCHNAME,"LATEST IN THIS PACKAGE")=lastPatch
        ;"       OPTION -- optional.  Pass by reference.
        ;"              OPTION("VERBOSE")=1, means messaages also written directly to output
        ;"
        ;"Results: NONE

        NEW FPATH,FNAME,ARRAY
        SET FPATH=$GET(Info("PATH")) IF FPATH="" GOTO ADONE
        SET FNAME=$GET(Info("TEXT FILE")) IF FNAME="" GOTO ADONE
        IF $$FTG^%ZISH(FPATH,FNAME,"ARRAY(0)",1)=0 GOTO ADONE
        DO GETReqPATCHES(.ARRAY,.Info)
        DO GETCategory(.ARRAY,.Info)
        DO GETMultPATCHES(.ARRAY,.Info,.OPTION)
        DO GETFNAMEs(.ARRAY,.Info)
        DO GETSubject(.ARRAY,.Info)
        DO CheckDelta(.Info)
ADONE   QUIT


PAnalyze(Info,OPTION)
        ;"Purpose: To look at a patch KID file and extract useful information
        ;"Input:  Info -- PASS BY REFERENCE, and IN and OUT PARAMETER.
        ;"          Input:      Info("PATH") -- path on HFS of TXT file
        ;"                      Info("KID FILE") -- FILENAME on HFS of KID file
        ;"          Output:     Info("SPECIFIED REQ",PATCHNAME)=""
        ;"                      Info("SPECIFIED REQ",PATCHNAME)=""
        ;"       OPTION -- optional.  Pass by reference.
        ;"              OPTION("VERBOSE")=1, means messaages also written directly to output
        ;"Results: NONE

        NEW FPATH,FNAME,ARRAY
        SET FPATH=$GET(Info("PATH")) IF FPATH="" GOTO PADONE
        SET FNAME=$GET(Info("KID FILE")) IF FNAME="" GOTO PADONE
        IF $$FTG^%ZISH(FPATH,FNAME,"ARRAY(0)",1)=0 GOTO PADONE
        DO PGETReqPATCHES(.ARRAY,.Info)
        ;"do PCheckDelta(.Info)
PADONE   QUIT


GETReqPATCHES(ARRAY,Info)
        ;"Purpose: to scan ARRAY, holding TXT file, and assemble list of required patches
        ;"Input: ARRAY -- PASS BY REFERENCE -- holds TXT file
        ;"       Info  -- PASS BY REFERENCE.  Data added as follows:
        ;"              Info("SPECIFIED REQ",PATCHNAME)=""
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
        . . SET Info("SPECIFIED REQ",ONEPATCH)=""
        QUIT

PGETReqPATCHES(ARRAY,Info)
        ;"Purpose: to scan ARRAY, holding KID file, and assemble list of required patches
        ;"Input: ARRAY -- PASS BY REFERENCE -- holds TXT file
        ;"       Info  -- PASS BY REFERENCE.  Data added as follows:
        ;"              Info("SPECIFIED REQ",PATCHNAME)=""
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
        . SET Info("SPECIFIED REQ",ONEPATCH)=""
        QUIT

GETCategory(ARRAY,Info)
        ;"Purpose: to scan ARRAY, holding TXT file, and assemble category entries
        ;"Input: ARRAY -- PASS BY REFERENCE -- holds TXT file
        ;"       Info  -- PASS BY REFERENCE.  Data added as follows:
        ;"              Info("PATCH CATEGORY",NAME)=""
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
        . . SET Info("PATCH CATEGORY",ONEPATCH)=""
        QUIT

GETFNAMEs(ARRAY,Info)
        ;"Purpose: to scan ARRAY, holding TXT file, and scavenge and .KID file names
        ;"Input: ARRAY -- PASS BY REFERENCE -- holds TXT file
        ;"       Info  -- PASS BY REFERENCE.  Data added as follows:
        ;"              Info("MISC KID FILES",NAME)=""
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
        . . SET Info("MISC KID FILES",FNAME)=""
        QUIT

GETSubject(ARRAY,Info)
        ;"Purpose: to scan ARRAY, holding TXT file, and scavenge SUBJECT line
        ;"Input: ARRAY -- PASS BY REFERENCE -- holds TXT file
        ;"       Info  -- PASS BY REFERENCE.  Data added as follows:
        ;"              Info("SUBJECT",NAME)=""
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
        . . SET Info("SUBJECT",subj)=""
        . . SET DONE=1
        QUIT

GETMultPATCHES(ARRAY,Info,OPTION)
        ;"Purpose: to scan ARRAY, holding TXT file, and matching multi-patch info
        ;"Input: ARRAY -- PASS BY REFERENCE -- holds TXT file
        ;"       Info  -- PASS BY REFERENCE.  Data added as follows:
        ;"              Info("MULTI-PATCH","FILENAME")=FileName
        ;"              Info("MULTI-PATCH","CONTAINS",PATCHNAME)=""
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
        . . SET Info("MULTI-PATCH","CONTAINS",ONEPATCH)=""
        . IF FOUNDSection DO
        . . IF s["Patch(es)" SET FOUNDLIST=1 QUIT
        . . NEW TEMPS SET TEMPS=$$UP^XLFSTR(s)
        . . IF TEMPS[".KID" DO
        . . . NEW p SET p=$FIND(TEMPS,".KID")-1
        . . . SET TEMPS=$$GETWORD^TMGSTUT3(s,p) QUIT:TEMPS=""
        . . . FOR  QUIT:($$UP^XLFSTR($EXTRACT(TEMPS,$LENGTH(TEMPS)))="D")!(TEMPS="")  DO
        . . . . SET TEMPS=$EXTRACT(TEMPS,1,$LENGTH(TEMPS)-1)
        . . . SET Info("MULTI-PATCH","FILENAME")=TEMPS

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
        DO ParsePATCHNAME^TMGPAT2(PATCHNAME,.PCKINIT,.VER,.PATCHNUM,.SEQNUM)
        IF PCKINIT="" GOTO GSQDN
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
GSQL1   SET RESULT=$$ComposePATCHNAME^TMGPAT2(PCKINIT,VER,PATCHNUM,SEQNUM)
        GOTO GSQDN

        ;"=========== old code below =========================
        ;"NEW PCKINIT,VER,PATCHNUM
        ;"SET PATCHNAME=$GET(PATCHNAME)
        ;"SET PCKINIT=$PIECE(PATCHNAME,"*",1)
        ;"SET VER=$PIECE(PATCHNAME,"*",2)
        ;"SET PATCHNUM=$PIECE(PATCHNAME,"*",3)
        ;"IF (PCKINIT="")!(VER="")!(PATCHNUM="") GOTO GSqDONE
        ;"SET RESULT=$GET(^TMG("KIDS","PATCH NAMES",PCKINIT,VER,PATCHNUM)) ;"stored from prior search
        ;"IF RESULT'="" DO  GOTO GSqDONE
        ;". IF RESULT="???" SET RESULT="" QUIT
        ;". SET URL=$GET(^TMG("KIDS","PATCH NAMES",PCKINIT,"COMBINED",RESULT))
        ;"
        ;"NEW VERBOSE SET VERBOSE=$GET(OPTION("VERBOSE"))
        ;"SET MODE=+$GET(MODE)
        ;"
        ;"NEW FPATH,FNAME,ARRAY,ABORT
        ;"SET ABORT=0
        ;"SET FPATH=$GET(^TMG("KIDS","PATCH DIR"),"/tmp/")
        ;"SET FNAME=$$PATCHDIRNAME(PCKINIT) ;
        ;"IF '$$FILEXIST^TMGIOUTL(FPATH_FNAME) DO
        ;". IF VERBOSE DO
        ;". . WRITE "Finding Sequence # for ",PATCHNAME,!
        ;". . WRITE "Getting directory information from VA ftp server..."
        ;". IF $$GetPckList^TMGKERNL(PCKINIT,.ARRAY,1)=0 SET ABORT=1
        ;". IF VERBOSE WRITE " Done.",!
        ;"ELSE  DO
        ;". IF $$FTG^%ZISH(FPATH,FNAME,"ARRAY(0)",1)=0 SET ABORT=1 QUIT
        ;"IF ABORT GOTO GSqDONE
        ;"
        ;"NEW FOUND SET FOUND=0
        ;"NEW i SET i=0  ;"skip first line, a header line
        ;"FOR  SET i=$ORDER(ARRAY(i)) QUIT:(i="")!FOUND  DO
        ;". NEW NAME,PATH,FULLNAMEPATH,ONEVER,ONEPATCHNUM,ONESEQNUM
        ;". SET FULLNAMEPATH=$GET(ARRAY(i)) QUIT:FULLNAMEPATH=""
        ;". DO SPLITFPN^TMGIOUTL(FULLNAMEPATH,.PATH,.NAME,"/")
        ;". IF NAME="" QUIT
        ;". NEW TEMPNAME SET TEMPNAME=$$UP^XLFSTR(NAME)
        ;". IF MODE=0,(TEMPNAME'[".TXT")&((TEMPNAME'[".KID")) QUIT
        ;". ELSE  IF MODE=1,(TEMPNAME'[".TXT") QUIT
        ;". ELSE  IF MODE=2,(TEMPNAME'[".KID") QUIT
        ;". SET ONEVER=$PIECE($PIECE(TEMPNAME,"_",1),"-",2) QUIT:(ONEVER="")
        ;". IF ONEVER?.N1"P".N SET ONEVER=$TRANSLATE(VER,"P",".")
        ;". SET ONESEQNUM=$PIECE($PIECE(TEMPNAME,"_",2),"-",2) QUIT:(ONESEQNUM="")
        ;". SET ONEPATCHNUM=$PIECE($PIECE(TEMPNAME,"_",3),"-",2) QUIT:(ONEPATCHNUM="")
        ;". SET ONEPATCHNUM=$PIECE(ONEPATCHNUM,".",1) QUIT:(ONEPATCHNUM="")
        ;". IF ONEPATCHNUM=PATCHNUM DO
        ;". . SET RESULT=PATCHNAME_" SEQ #"_ONESEQNUM
        ;". . SET URL=FULLNAMEPATH
        ;". . SET FOUND=1
        ;". . SET ^TMG("KIDS","PATCH NAMES",PCKINIT,VER,PATCHNUM)=RESULT
        ;". . SET ^TMG("KIDS","PATCH NAMES",PCKINIT,"COMBINED",RESULT)=URL

GSqDONE
        IF RESULT="" DO
        . IF (PCKINIT="")!(VER="")!(PATCHNUM="") QUIT
        . SET ^TMG("KIDS","PATCH NAMES",PCKINIT,VER,PATCHNUM)="???"

GSQDN   QUIT RESULT

PATCHDIRNAME(PCKINIT) ;
        QUIT "Remote_Patches-dirFor-"_PCKINIT


FindMultPATCH(PATCHNAME,PCKINIT,OPTION,URL,Info)
        ;"Purpose: Search through downloaded directory file for patch, and return URL
        ;"Input: PATCHNAME -- e.g. CSV_12_1234.KID
        ;"       PCKINIT -- the initials for the package.
        ;"       OPTION -- optional.  Pass by reference.
        ;"              OPTION("VERBOSE")=1, means messaages also written directly to output
        ;"       URL -- PASS BY REFERENCE, an OUT PARAMETER
        ;"       Info -- PASS BY REFERENCE
        ;"              Info("KID URL")=URL on server for KID file
        ;"              Info("TEXT URL")=URL on server for TXT file
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
        . IF $$GetPckList^TMGKERNL(PCKINIT,.ARRAY,1)=0 SET ABORT=1
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
        . NEW serverPATH SET serverPATH=$GET(Info("KID URL"))
        . IF serverPATH="" SET serverPATH=$GET(Info("TEXT URL"))
        . SET serverPATH=$$PATHEXTR^TMGIOUTL(serverPATH)
        . SET serverPATH=$PIECE(serverPATH,"ftp://",2)
        . SET URL=serverPATH_PATCHNAME
        . SET RESULT=1  ;"this is not a sure 'find', more of a hopeful guess that it will be in same directory.

FMPDONE
        QUIT RESULT



CheckDelta(Info)
        ;"Purpose: to compare the requirements in Info (as created by Analyze) and
        ;"         determine differences in existing system.
        ;"Input: Info.  PASS BY REFERENCE
        ;"          Info("PATH") -- path on HFS of TXT file
        ;"          Info("TEXT FILE") -- FILENAME on HFS of TXT file
        ;"          Info("SPECIFIED REQ",PATCHNAME)=""
        ;"          Info("MULTI-PATCH","FILENAME")=FileNAME
        ;"          Info("MULTI-PATCH","CONTAINS",PATCHNAME)=""
        ;"          Info("MULTI-PATCH","CONTAINS",PATCHNAME,"LATEST IN THIS PACKAGE")=lastPATCH
        ;"Output:
        ;"          Info("SPECIFIED REQ",PATCHNAME)="OK, Installed." or "Still Needed"
        ;"          Info("STILL NEEDED",reqPATCH)=""

        NEW reqPATCH SET reqPATCH=""
        FOR  SET reqPATCH=$ORDER(Info("SPECIFIED REQ",reqPATCH)) QUIT:(reqPATCH="")  DO
        . NEW TEMPS
        . IF $$IsInstalled^TMGPAT2(reqPATCH) SET TEMPS="OK, Installed."
        . ELSE  DO
        . . SET TEMPS="Still Needed."
        . . NEW s2 SET s2=$$GETSEQ(reqPATCH) IF s2="" SET s2=reqPATCH
        . . SET Info("STILL NEEDED",s2)=""
        . SET Info("SPECIFIED REQ",reqPATCH)=TEMPS

        NEW PATCHNAME SET PATCHNAME=""
        FOR  SET PATCHNAME=$ORDER(Info("MULTI-PATCH","CONTAINS",PATCHNAME)) QUIT:(PATCHNAME="")  DO
        . NEW PCKINIT SET PCKINIT=$PIECE(PATCHNAME,"*",1)
        . NEW VER SET VER=$PIECE(PATCHNAME,"*",2)
        . SET PATCHNUM=$PIECE($PIECE(PATCHNAME,"*",3)," ",1)
        . NEW SEQNUM SET SEQNUM=$PIECE(PATCHNAME,"SEQ #",2)
        . IF (PCKINIT="")!(VER="")!(PATCHNUM="") QUIT
        . NEW lastPATCH SET lastPATCH=$$GETLSTPK^TMGPAT1(PCKINIT,VER) ;"returns e.g. 'DI*22.0*140 SEQ# 123'
        . SET Info("MULTI-PATCH","CONTAINS",PATCHNAME,"LATEST IN THIS PACKAGE")=lastPATCH
        . NEW lastSEQNUM SET lastSEQNUM=$PIECE(lastPATCH,"SEQ #",2)
        . IF lastSEQNUM<SEQNUM DO
        . . SET Info("GAPPED PATCHES",PATCHNAME)="Currently at: "_lastPATCH

        QUIT

ShowAnalysis(Info,Msg)
        ;"Purpose: To display analysis info in a meaningful way.
        ;"Input: Info -- PASS BY REFERENCE.  Info as created by Analyze(Info,OPTION)
        ;"       Msg -- PASS BY REFERANCE, an OUT PARAMETER
        ;"              Errors are stored in Msg("ERROR",x)=Message
        ;"                                   Msg("ERROR")=COUNT of last error
        ;"              Message are store in Msg(x)=Message
        ;"                                   Msg=COUNT of last message+1

        NEW TEMPMsg,someReq
        MERGE TEMPMsg=Msg
        SET someReq=0
        DO AddMsg^TMGPAT2("According to Info TXT file,",0,.TEMPMsg)
        DO AddMsg^TMGPAT2("Before this patch is applied, other patches should have been installed first:",0,.TEMPMsg)
        NEW i SET i=""
        FOR  SET i=$ORDER(Info("SPECIFIED REQ",i)) QUIT:(i="")  DO
        . NEW s SET s=$$GETSEQ(i) IF s="" SET s=i
        . SET s=$$LJ^XLFSTR(s,25)
        . SET someReq=1
        . NEW PCKINIT SET PCKINIT=$PIECE(s,"*",1)
        . NEW VER SET VER=$PIECE(s,"*",2)
        . NEW LASTPCK SET LASTPCK=$$GETLSTPK^TMGPAT1(PCKINIT,VER)
        . NEW TEMPS SET TEMPS="    "_s_" <-- "_$GET(Info("SPECIFIED REQ",i))
        . IF LASTPCK'="" SET TEMPS=TEMPS_" Current at: "_LASTPCK
        . DO AddMsg^TMGPAT2(TEMPS,0,.TEMPMsg)
        DO AddMsg^TMGPAT2(" ",0,.TEMPMsg)
        IF someReq=1 DO
        . KILL Msg MERGE Msg=TEMPMsg

        IF $GET(Info("MULTI-PATCH","FILENAME"))="" GOTO SA2
        SET someReq=0
        DO AddMsg^TMGPAT2("This patch is a Multi-Patch.  It contains patches for the following packages: ",0,.Msg)
        KILL TEMPMsg MERGE TEMPMsg=Msg
        SET i=""
        FOR  SET i=$ORDER(Info("MULTI-PATCH","CONTAINS",i)) QUIT:(i="")  DO
        . NEW curSys SET curSys=$GET(Info("MULTI-PATCH","CONTAINS",i,"LATEST IN THIS PACKAGE"))
        . NEW s SET s=$$LJ^XLFSTR(i,25)
        . SET someReq=1
        . DO AddMsg^TMGPAT2("  "_s_" [This system is currently at: "_curSys_"]",0,.TEMPMsg)
        IF someReq=1 DO
        . KILL Msg MERGE Msg=TEMPMsg
        DO AddMsg^TMGPAT2("This multipatch is combined into file: "_$GET(Info("MULTI-PATCH","FILENAME")),0,.Msg)
SA2
        NEW subj SET subj=$ORDER(Info("SUBJECT",""))
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
        NEW routine SET routine=""
        FOR  SET routine=$ORDER(@PARRAY@("RTN",routine)) QUIT:(routine="")  DO
        . IF $$Chk1Routine(routine)=1 DO
        . . SET RESULT=1
        . . IF $GET(OPTION("VERBOSE"))=1 WRITE "WARNING: Importing routine ",routine," will overWRITE local changes!",!
        QUIT RESULT


Chk1Routine(routine)
        ;"Purpose: to see IF one routine has any local modifications.
        ;"Input: routine -- the routine name  e.g. XUP
        ;"Results: 1 IF conflict, 0 IF OK.
        ;"NOTE: for now, the only test to be used will be the presence of the
        ;"      text '//kt'
        ;"      Later I want to use TMG VISTA FILE INFO, file 22708

        NEW RESULT SET RESULT=0

        NEW line SET line=0
        NEW blankCt SET blankCt=0
        FOR  DO  QUIT:(blankCt>10)!RESULT
        . NEW ref SET ref="+"_line_"^"_routine
        . NEW s SET s=$TEXT(@ref)
        . IF s="" SET blankCt=blankCt+1
        . SET RESULT=(s["//kt")
        . SET line=line+1
        QUIT RESULT
