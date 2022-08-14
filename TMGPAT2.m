TMGPAT2  ;TMG/kst/Patching tools Suport;09/17/08, 7/21/22
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
 ;"MAKFRESH(PCKINIT,Msg,PCKDIRFNAME) --ensure that the Package list of files avilable on server is fresh
 ;"RefreshPackge(PCKINIT,.Msg,NEEDSREFRESH,PCKDIRFNAME) -- query server for ONE package, and refresh info stored in TMG KIDS REMOTE PATCH SOURCE file
 ;"ISPNAME(NAME,VER,PATCHNUM,SEQNUM) --IS NAME A PATCH NAME?  IF SO, PARSE 
 ;"GETNEXTIENS(LastPATCH,NEXTPATCHNAME) -- return IENS in file TMG KIDS REMOTE PATCH SOURCE (22709)
 ;"GETIENS(PATCHNAME) -- Given patch name, return IENS in file TMG KIDS REMOTE PATCH SOURCE (22709)
 ;"GETIENS2(PATCHNAME) -- Given partial patch name, return IENS in file 22709.01
 ;"FORCEPAT -- All user to enter a patch entry
 ;"IsInstalled(PATCHNAME) -- return IF a given patch has already been installed.
 ;"PATCHESMATCH(PATCH1,PATCH2) ;--Return 0 or 1 depending if patches are the same (equivalent)
 ;"PRSEPATCHNAME(PATCHNAME,PCKINIT,VER,PATCHNUM,SEQNUM) -- parse a patch name into it's composit parts.
 ;"MAKEPATCHNAME(PCKINIT,VER,PATCHNUM,SEQNUM) -- the opposite of PRSEPATCHNAME: build up name from parts
 ;"ENSRLOCL(IENS,INFO,Msg,OPTION,PCKINIT) -- Ensure files downloaded from server and stored locally
 ;"DownloadPATCH(PATCHNAME,PROTOCOL,OPTION,Msg,INFO) -- Ensure that the Patch has been downloaded from server and stored locally
 ;"MakePATCHEntry(PATCHNAME,Msg) -- make pseudo-entries to show that something was processed.
 ;"ADDMSG(s,IsError,Msg) -- add a message to Msg ARRAY
 ;"SHOWMSG(Msg) -- display the message array
 ;
 ;"=======================================================================
 ;"Private Functions
 ;"=======================================================================
 ;"EmptyPACKAGE(IEN9D4,Msg) - delete info for Package in file TMG KIDS REMOTE PATCH SOURCE
 ;"LOADPCKG(IEN9D4,ARRAY,PROTOCOL,Msg,SOMEADDED) -- load info for Package info file TMG KIDS REMOTE PATCH SOURCE
 ;"LoadOne(PCKIEN,VER,PATCHNUM,SEQNUM,REMOTEURL,Msg) -- file ONE entry info TMG KIDS REMOTE PATCH SOURCE
 ;"ISSPNAME(NAME) -- determine if file on server is a specially formatted file.
 ;"ForceP2(PCKINIT,VER,PATCHNUM,SEQNUM) -- Hack WRITE a patch into Package file, based in componant parts. 
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;
MAKFRESH(PCKINIT,Msg,PCKDIRFNAME)
        ;"Purpose: to ensure that the Package list of files avilable on server is fresh
        ;"Input: PCKINIT -- this is the namespace of the package to get patches for, e.g. 'DI' for fileman
        ;"       Msg -- PASS BY REFERANCE, an OUT PARAMETER
        ;"              Errors are stored in Msg("ERROR",x)=Message
        ;"                                   Msg("ERROR")=COUNT of last error
        ;"              Message are store in Msg(x)=Message
        ;"                                   Msg=COUNT of last message+1
        ;"       PCKDIRFNAME -- Optional. PASS BY REFERNCE, an OUT PARAMETER. Filled with HFS filename of file
        ;"RESULTs: nONE

        NEW IEN9D4 SET IEN9D4=+$ORDER(^DIC(9.4,"C",PCKINIT,""))
        IF IEN9D4'>0 DO  GOTO ENSFDONE
        . DO ADDMSG("Can't find PACKAGE named '"_PCKINIT_"'",1,.Msg)
        NEW PCKIEN,lastFMDate
        SET PCKIEN=+$ORDER(^TMG(22709,"B",IEN9D4,""))
        IF PCKIEN>0 SET lastFMDate=$PIECE($GET(^TMG(22709,PCKIEN,2)),"^",1)
        ELSE  SET lastFMDate=""
        NEW %,X,X1,X2,NEEDSREFRESH
        SET NEEDSREFRESH=0
        IF lastFMDate'="" DO
        . DO NOW^%DTC ;"returns date in X
        . SET X1=lastFMDate,X2=X
        . DO ^%DTC
        . IF X>7 SET NEEDSREFRESH=1  ;"hard code in fresh IF > 7 days since last scan.
        ELSE  SET NEEDSREFRESH=1
        IF $$RefreshPackge(PCKINIT,.Msg,NEEDSREFRESH,.PCKDIRFNAME)
ENSFDONE
        QUIT

RefreshPackge(PCKINIT,Msg,NEEDSREFRESH,PCKDIRFNAME,OPTION)
        ;"Purpose: To query server for ONE package, and refresh info stored in TMG KIDS REMOTE PATCH SOURCE file
        ;"Input: PCKINIT -- this is the namespace of the package to get patches for, e.g. 'DI' for fileman
        ;"       Msg -- PASS BY REFERANCE, an OUT PARAMETER
        ;"              Errors are stored in Msg("ERROR",x)=Message
        ;"                                   Msg("ERROR")=COUNT of last error
        ;"              Message are store in Msg(x)=Message
        ;"                                   Msg=COUNT of last message+1
        ;"       NEEDSREFRESH -- OPTIONAL.  0 IF refreshing not needed (just return PCKDIRFNAME, but ensure file exists)
        ;"       PCKDIRFNAME -- Optional. PASS BY REFERANCE, an OUT PARAMETER. Filled with HFS filename of file
        ;"       OPTION -- Optional.  PASS BY REFERANCE.
        ;"              OPTION("VERBOSE")=1  puts out text to console. (1 is default value)
        ;"RESULT : 1=success, 0=failure

        NEW ARRAY,RESULT
        NEW VERBOSE SET VERBOSE=+$GET(OPTION("VERBOSE"),1)
        SET NEEDSREFRESH=+$GET(NEEDSREFRESH)
        IF NEEDSREFRESH,VERBOSE WRITE !,"Fetching list of available "_PCKINIT_" patches from patch repository server..."
        ;"SET RESULT=$$GetPckList^TMGKERNL(PCKINIT,.ARRAY,.NEEDSREFRESH,.PCKDIRFNAME)
        SET RESULT=$$GETPATL^TMGKERN4(PCKINIT,.ARRAY,.NEEDSREFRESH,.PCKDIRFNAME)
        IF NEEDSREFRESH,VERBOSE WRITE "  DONE.",!
        IF $GET(NEEDSREFRESH)'>0 GOTO RPDONE
        IF RESULT=0 GOTO RPDONE
        NEW IEN9D4 SET IEN9D4=+$ORDER(^DIC(9.4,"C",PCKINIT,""))
        IF IEN9D4'>0 DO  GOTO RPDONE
        . DO ADDMSG("Can't find PACKAGE named '"_PCKINIT_"'",1,.Msg)
        NEW SOMEADDED
        SET RESULT=$$LOADPCKG(IEN9D4,.ARRAY,"http://",.Msg,.SOMEADDED)
RPDONE
        QUIT RESULT


EmptyPACKAGE(IEN9D4,Msg)
        ;"Purpose: to delete info for Package in file TMG KIDS REMOTE PATCH SOURCE
        ;"Input: IEN9D4 -- IEN in 9.4 to get patches for
        ;"       Msg -- PASS BY REFERANCE, an OUT PARAMETER
        ;"              Errors are stored in Msg("ERROR",x)=Message
        ;"                                   Msg("ERROR")=COUNT of last error
        ;"              Message are store in Msg(x)=Message
        ;"                                   Msg=COUNT of last message+1

        NEW PCKIEN SET PCKIEN=+$ORDER(^TMG(22709,"B",IEN9D4,""))
        IF PCKIEN=0 GOTO EPDONE
        NEW TMGFDA,TMGMSG
        SET TMGFDA(22709,PCKIEN_",",.01)="@"
        DO FILE^DIE("EK","TMGFDA","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO
        . DO ADDMSG($$GETERRST^TMGDEBU2(.TMGMSG),1,.Msg)

EPDONE  QUIT


LOADPCKG(IEN9D4,ARRAY,PROTOCOL,Msg,SOMEADDED)
        ;"Purpose: to load info for Package info file TMG KIDS REMOTE PATCH SOURCE
        ;"Input:  IEN9D4 -- IEN in 9.4 to get patches for
        ;"       ARRAY -- This is file with available filepaths, as returned from GETPATL
        ;"              Format: ARRAY(0)=<header line> <-- ignored
        ;"                      ARRAY(#)=URLPATH  e.g. 'server.org/pub/download/DG/DG_53_P481.KID'
        ;"                      ARRAY(#)=URLPATH  e.g. 'server.org/pub/download/DG/DG_53_P482.KID'
        ;"                      ARRAY(#)=URLPATH  e.g. 'server.org/pub/download/DG/DG_53_P483.KID'
        ;"       PROTOCOL -- OPTIONAL.  Default is 'ftp://'
        ;"       Msg -- PASS BY REFERANCE, an OUT PARAMETER
        ;"              Errors are stored in Msg("ERROR",x)=Message
        ;"                                   Msg("ERROR")=COUNT of last error
        ;"              Message are store in Msg(x)=Message
        ;"                                   Msg=COUNT of last message+1
        ;"       SOMEADDED -- PASS BY REFERENCE, an OUT PARAMETER
        ;"              SET to 1 IF some added, otherwise 0
        ;"RESULT : 2=patch added, 1=no problems, 0=failure or error occured

        NEW RESULT SET RESULT=1
        SET SOMEADDED=0
        SET PROTOCOL=$GET(PROTOCOL,"http://")
        SET IEN9D4=+$GET(IEN9D4)
        IF IEN9D4'>0 DO  GOTO LPDONE
        . DO ADDMSG("Can't find record #"_IEN9D4_" in INSTALL file.",1,.Msg)
        NEW PCKIEN SET PCKIEN=+$ORDER(^TMG(22709,"B",IEN9D4,""))
        NEW TMGFDA,TMGMSG,TMGIEN,X,%
        DO NOW^%DTC ;"output in %
        IF PCKIEN>0 DO
        . SET TMGFDA(22709,PCKIEN_",",2)=%
        . DO FILE^DIE("K","TMGFDA","TMGMSG")
        ELSE  DO
        . SET TMGFDA(22709,"+1,",.01)=IEN9D4
        . SET TMGFDA(22709,"+1,",2)=%  ;"the time downloaded
        . DO UPDATE^DIE("K","TMGFDA","TMGIEN","TMGMSG")
        . SET PCKIEN=$GET(TMGIEN(1))
        IF $DATA(TMGMSG("DIERR")) DO
        . DO ADDMSG($$GETERRST^TMGDEBU2(.TMGMSG),1,.Msg)
        IF PCKIEN'>0 GOTO LPDONE

        NEW SPCLNAMES
        NEW i SET i=0  ;"skip first line, a header line
        FOR  SET i=$ORDER(ARRAY(i)) QUIT:(i="")  DO
        . NEW NAME,PATH,FULLNAMEPATH,VER,PATCHNUM,SEQNUM
        . SET FULLNAMEPATH=$GET(ARRAY(i)) QUIT:FULLNAMEPATH=""
        . DO SPLITFPN^TMGIOUTL(FULLNAMEPATH,.PATH,.NAME,"/")
        . IF (PATH="")!(NAME="") QUIT
        . NEW UNAME SET UNAME=$$UP^XLFSTR(NAME)
        . NEW VALID SET VALID=$$ISPNAME(NAME,.VER,.PATCHNUM,.SEQNUM) ;"IS NAME A PATCH NAME?  IF SO, PARSE
        . IF 'VALID,$$ISSPNAME(UNAME) DO  QUIT
        . . SET SPCLNAMES(NAME)=""
        . IF VER="" DO  QUIT
        . . DO ADDMSG("Unable to process file name: "_NAME_".  Couldn't determine version number.",1,.Msg)
        . ;"IF SEQNUM="" DO  QUIT   ;Removed because sometimes the sequence # comes from the TXT file, not the patch file.
        . ;". DO ADDMSG("Unable to process file name: "_NAME_".  Couldn't determine sequence number.",1,.Msg)
        . IF $GET(PATCHNUM)="" DO  QUIT
        . . DO ADDMSG("Unable to process file name: "_NAME_".  Couldn't determine patch number.",1,.Msg)
        . NEW TEMPRESULT
        . SET TEMPRESULT=$$LoadOne(PCKIEN,VER,PATCHNUM,SEQNUM,PROTOCOL_FULLNAMEPATH)
        . SET SPCLNAMES("ZZ",VER,PATCHNUM,SEQNUM)=FULLNAMEPATH
        . IF TEMPRESULT=2 SET SOMEADDED=1
        ;
        ;"Now process KID files that are pattern of NDF4P158.KID
        ;"Handle AFTER above, so can get sequence number that is typically given with the corresponding .TXT file
        NEW NAME SET NAME=""
        FOR  SET NAME=$ORDER(SPCLNAMES(NAME)) QUIT:NAME=""  DO
        . IF NAME="ZZ" QUIT
        . NEW PATH,FULLNAMEPATH,UNAME,VER,PATCHNUM,SEQNUM
        . NEW UNAME SET UNAME=$$UP^XLFSTR(NAME)
        . SET VER=$PIECE(UNAME,"P",1)
        . NEW DIGIT
        . FOR  Q:(VER="")  DO  Q:(+DIGIT=DIGIT)
        . . SET DIGIT=$EXTRACT(VER,1)
        . . Q:(+DIGIT=DIGIT)
        . . SET VER=$EXTRACT(VER,2,$LENGTH(VER))
        . SET VER=$TRANSLATE(VER,"_",".")
        . SET VER=+VER
        . SET PATCHNUM=+$PIECE(UNAME,"P",2)
        . SET SEQNUM=+$ORDER(SPCLNAMES("ZZ",VER,PATCHNUM,""))
        . IF SEQNUM'>0 QUIT
        . SET FULLNAMEPATH=$GET(SPCLNAMES("ZZ",VER,PATCHNUM,SEQNUM))
        . NEW TEMPNAME
        . DO SPLITFPN^TMGIOUTL(FULLNAMEPATH,.PATH,.TEMPNAME,"/")
        . IF (PATH="") QUIT
        . SET FULLNAMEPATH=PATH_NAME
        . NEW TEMPRESULT
        . SET TEMPRESULT=$$LoadOne(PCKIEN,VER,PATCHNUM,SEQNUM,PROTOCOL_FULLNAMEPATH)
        . IF TEMPRESULT=2 SET SOMEADDED=1

LPDONE
        QUIT RESULT
        ;
ISPNAME(NAME,VER,PATCHNUM,SEQNUM) ;"IS NAME A PATCH NAME?  IF SO, PARSE
        NEW RESULT SET RESULT=0
        SET (VER,PATCHNUM,SEQNUM)=""
        NEW UNAME SET UNAME=$$UP^XLFSTR(NAME)
        IF UNAME?2.4A1"_"1.4N1"_"0.1"P"1.N1".KID" DO  ;"e.g. DG_53_P481.KID or EAS_1_47.KID
        . SET VER=$PIECE($PIECE(NAME,"_",2),"_",1)
        . SET PATCHNUM=$PIECE($PIECE(NAME,"_",3),".",1)
        . IF PATCHNUM?1A.N SET PATCHNUM=$EXTRACT(PATCHNUM,2,99)
        . SET SEQNUM="" ;"signal for no seq number provided
        ELSE  IF UNAME?2.4A1"-"1.4N1"_SEQ-"1.4N1"_PAT-"1.4N1".KID" DO  ;"E.g. lex-2_seq-52_pat-56.kid
        . SET VER=$PIECE(UNAME,"_",1)
        . SET VER=$PIECE(VER,"-",2)
        . SET SEQNUM=$PIECE(UNAME,"_",2)
        . SET SEQNUM=$PIECE(SEQNUM,"-",2)
        . SET PATCHNUM=$PIECE(UNAME,"_",3)
        . SET PATCHNUM=$PIECE(PATCHNUM,".",1)
        . SET PATCHNUM=$PIECE(PATCHNUM,"-",2)
        ELSE  IF UNAME?2.4A1"_"1.4N1"_"1.4N1"_"1.4N1".KID"0.1"S" DO  ;"e.g. PXRM_2_0_54.KIDs
        . SET VER=+$PIECE(UNAME,"_",2)
        . SET SEQNUM=+$PIECE(UNAME,"_",3)
        . SET PATCHNUM=+$PIECE(UNAME,"_",4)
        ELSE  DO
        . SET VER=$PIECE($PIECE(NAME,"_",1),"-",2) QUIT:(VER="")
        . IF VER?.N1(1"p",1"P").N SET VER=$TRANSLATE(VER,"Pp","..")
        . SET SEQNUM=$PIECE($PIECE(NAME,"_",2),"-",2) QUIT:(SEQNUM="")
        . SET PATCHNUM=$PIECE($PIECE(NAME,"_",3),"-",2) QUIT:(PATCHNUM="")
        . SET PATCHNUM=$PIECE(PATCHNUM,".",1) QUIT:(PATCHNUM="")
        ;
        IF VER'="" DO
        . NEW VERIEN 
        . SET VERIEN=+$ORDER(^TMG(22709,PCKIEN,1,"C",VER,""))
        . IF VERIEN'>0 SET VERIEN=+$ORDER(^TMG(22709,PCKIEN,1,"B",VER,""))
        . ;"NOTE: this fails on first encounter of new package, because new version not added until LoadOne called
        . ;"... needs to have option here to add new if not found.  fix later.  
        . IF VERIEN'>0 DO
        . . NEW AVER SET AVER=""
        . . FOR  SET AVER=$ORDER(^TMG(22709,PCKIEN,1,"B",AVER)) QUIT:(AVER="")!(VERIEN>0)  DO
        . . . IF +VER=+AVER SET VERIEN=$ORDER(^TMG(22709,PCKIEN,1,"B",AVER,"")) QUIT
        . IF VERIEN=0 SET VER="" QUIT
        . SET VER=$PIECE($GET(^TMG(22709,PCKIEN,1,VERIEN,0)),"^",1)
        IF (VER'="")&(PATCHNUM'="") DO
        . SET RESULT=1        
        QUIT RESULT
        ;
ISSPNAME(NAME)  ;
        ;"Purpose: determine if file on server is a specially formatted file.
        NEW RESULT SET RESULT=0
        NEW EXT SET EXT=$PIECE(NAME,".",2)
        IF $EXTRACT(EXT,1,3)'="KID" GOTO ISPDN
        IF $EXTRACT(NAME,1,3)="NDF" SET RESULT=1 GOTO ISPDN
        IF $EXTRACT(NAME,1,3)="FEE" SET RESULT=1 GOTO ISPDN
ISPDN   QUIT RESULT


LoadOne(PCKIEN,VER,PATCHNUM,SEQNUM,REMOTEURL,Msg)
        ;"Purpose: to file ONE entry info TMG KIDS REMOTE PATCH SOURCE
        ;"         This doesn't actually get the file from the server, just store
        ;"         the directory info, for later retrieval
        ;"Input: PCKIEN -- the IEN in TMG KIDS REMOTE PATCH SOURCE for Package
        ;"       VER -- the version of the patch
        ;"       PATCHNUM -- the patch number
        ;"       SEQNUM -- the patch sequence number (the release sequence number) (if provided)
        ;"       REMOTEURL -- The PROTOCOL_REMOTEURL of the patch on the server
        ;"       Msg -- PASS BY REFERANCE, an OUT PARAMETER
        ;"              Errors are stored in Msg("ERROR",x)=Message
        ;"                                   Msg("ERROR")=COUNT of last error
        ;"              Message are store in Msg(x)=Message
        ;"                                   Msg=COUNT of last message+1
        ;"RESULT : 1=success, 0=failure

        NEW RESULT SET RESULT=0 ;"default to failure
        SET PCKIEN=+$GET(PCKIEN) IF PCKIEN'>0 GOTO LODONE
        SET VER=$GET(VER) IF VER="" GOTO LODONE
        IF VER'["." SET VER=VER_".0"
        SET PATCHNUM=$GET(PATCHNUM) IF PATCHNUM="" GOTO LODONE
        SET SEQNUM=$GET(SEQNUM) ;"OK IF no sequence number
        NEW VERIEN SET VERIEN=+$ORDER(^TMG(22709,PCKIEN,1,"B",VER,""))
        IF VERIEN'>0 DO
        . NEW TMGFDA,TMGIEN,TMGMSG
        . SET TMGFDA(22709.01,"+1,"_PCKIEN_",",.01)=VER
        . SET TMGFDA(22709.01,"+1,"_PCKIEN_",",.02)=$TRANSLATE(VER,".","") ;"synonym
        . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) DO
        . . DO ADDMSG($$GETERRST^TMGDEBU2(.TMGMSG),1,.Msg)
        . SET VERIEN=+$GET(TMGIEN(1))
        IF VERIEN'>0 GOTO LODONE
        NEW PATCHIEN SET PATCHIEN=+$ORDER(^TMG(22709,PCKIEN,1,VERIEN,1,"B",PATCHNUM,""))
        IF PATCHIEN'>0 DO
        . NEW TMGFDA,TMGIEN,TMGMSG
        . SET TMGFDA(22709.11,"+1,"_VERIEN_","_PCKIEN_",",.01)=PATCHNUM
        . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) DO
        . . DO ADDMSG($$GETERRST^TMGDEBU2(.TMGMSG),1,.Msg)
        . SET PATCHIEN=+$GET(TMGIEN(1))
        . SET RESULT=2  ;"something added
        IF PATCHIEN'>0 GOTO LODONE
        NEW TMGFDA,TMGMSG
        NEW Ext SET Ext=$PIECE(REMOTEURL,".",$LENGTH(REMOTEURL,"."))

        NEW spec
        SET spec("'")="'\''",spec("*")="'\*'",spec("&")="'\&'",spec("?")="'\?'"
        ;"SET spec("\ ")=" "
        SET spec("\ ")="%20"
        SET REMOTEURL=$$REPLACE^XLFSTR(REMOTEURL,.spec)

        NEW field
        IF $$UP^XLFSTR(Ext)="TXT" SET field=1.5
        ELSE  SET field=1
        SET TMGFDA(22709.11,PATCHIEN_","_VERIEN_","_PCKIEN_",",field)=REMOTEURL
        IF SEQNUM'="" SET TMGFDA(22709.11,PATCHIEN_","_VERIEN_","_PCKIEN_",",".02")=SEQNUM
        DO FILE^DIE("K","TMGFDA","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO
        . DO ADDMSG($$GETERRST^TMGDEBU2(.TMGMSG),1,.Msg)
        IF $DATA(TMGMSG("DIERR"))=0 DO
        . IF RESULT>0 QUIT
        . SET RESULT=1 ;"success
LODONE
        QUIT RESULT


GETNEXTIENS(LastPATCH,NEXTPATCHNAME)
        ;"Purpose: Given last patch name, return IENS in file TMG KIDS REMOTE PATCH SOURCE (22709)
        ;"         that points to record with information about next appropriate patch.
        ;"Input: LastPATCH -- expected Format: e.g.  DI*22.0*100 SEQ #123
        ;"       NEXTPATCHNAME -- PASS BY REFERENCE, an OUT PARAMETER
        ;"                              Will be filled with name of next patch.
        ;"Output: returns IENS to record in 22709.11, or "" IF problem.

        NEW RESULT SET RESULT=""
        SET NEXTPATCHNAME=""
        IF $GET(LastPATCH)="" GOTO GNPDONE

        NEW SEQNUM,PCKINIT,VER,PATCHNUM,SEQNUM
        DO PRSEPATCHNAME(LastPATCH,.PCKINIT,.VER,.PATCHNUM,.SEQNUM)
        IF SEQNUM="" GOTO GNPDONE

        NEW IEN9D4 SET IEN9D4=+$ORDER(^DIC(9.4,"C",PCKINIT,""))
        IF IEN9D4'>0 GOTO GNPDONE
        NEW PCKIEN SET PCKIEN=+$ORDER(^TMG(22709,"B",IEN9D4,""))
        IF PCKIEN'>0 GOTO GNPDONE
        NEW VERIEN SET VERIEN=+$ORDER(^TMG(22709,PCKIEN,1,"B",VER,""))
        IF VERIEN'>0 SET VERIEN=+$ORDER(^TMG(22709,PCKIEN,1,"B",$PIECE(VER,".0",1),""))
        IF VERIEN'>0 GOTO GNPDONE

        NEW PATCHIEN,NEXTSEQ
        SET NEXTSEQ=+$ORDER(^TMG(22709,PCKIEN,VERIEN,"SEQ",SEQNUM))
        IF NEXTSEQ'>0 GOTO GNPDONE
        SET PATCHIEN=+$ORDER(^TMG(22709,PCKIEN,VERIEN,"SEQ",NEXTSEQ,""))
        IF PATCHIEN'>0 GOTO GNPDONE
        SET RESULT=PATCHIEN_","_VERIEN_","_PCKIEN_","
        SET NEXTPATCHNAME=$PIECE(LastPATCH," ",1)
        NEW NODE0 SET NODE0=$GET(^TMG(22709,PCKIEN,1,VERIEN,1,PATCHIEN,0))
        NEW NEXTPATCHNUM SET NEXTPATCHNUM=$PIECE(NODE0,"^",1)
        SET $PIECE(NEXTPATCHNAME,"*",3)=NEXTPATCHNUM
        SET NEXTPATCHNAME=NEXTPATCHNAME_" SEQ #"_NEXTSEQ

GNPDONE
        QUIT RESULT


GETIENS(PATCHNAME)
        ;"Purpose: Given patch name, return IENS in file TMG KIDS REMOTE PATCH SOURCE (22709)
        ;"Input: PATCHNAME -- expected Format: e.g.  DI*22.0*100 SEQ #123
        ;"Output: returns IENS to record in 22709.11, or "" IF problem.

        NEW RESULT SET RESULT=""
        IF $GET(PATCHNAME)="" GOTO GIDONE
        NEW SEQNUM,PCKINIT,VER,PATCHNUM,SEQNUM
        NEW IEN9D4,PCKIEN,VERIEN,PATCHIEN
        DO PRSEPATCHNAME(PATCHNAME,.PCKINIT,.VER,.PATCHNUM,.SEQNUM) GOTO:(SEQNUM="") GIDONE
        SET IEN9D4=+$ORDER(^DIC(9.4,"C",PCKINIT,"")) GOTO:(IEN9D4'>0) GIDONE
        SET PCKIEN=+$ORDER(^TMG(22709,"B",IEN9D4,"")) GOTO:(PCKIEN'>0) GIDONE
        SET VERIEN=+$ORDER(^TMG(22709,PCKIEN,1,"B",VER,""))
        IF VERIEN'>0 SET VERIEN=+$ORDER(^TMG(22709,PCKIEN,1,"B",$PIECE(VER,".0",1),""))
        IF VERIEN'>0 GOTO GIDONE
        SET PATCHIEN=+$ORDER(^TMG(22709,PCKIEN,VERIEN,"SEQ",SEQNUM,"")) GOTO:(PATCHIEN'>0) GIDONE
        SET RESULT=PATCHIEN_","_VERIEN_","_PCKIEN_","
GIDONE
        QUIT RESULT


GETIENS2(PATCHNAME)
        ;"Purpose: Given partial patch name, return IENS in file 22709.01
        ;"         (i.e. just patch*ver)
        ;"Input: PATCHNAME -- expected Format: e.g.  DI*22.0*.... or DI*22.0
        ;"Output: returns IENS to record in 22709.01, or "" IF problem.

        NEW RESULT SET RESULT=""
        IF $GET(PATCHNAME)="" GOTO GI2DONE
        NEW SEQNUM,PCKINIT,VER,PATCHNUM,SEQNUM
        NEW IEN9D4,PCKIEN,VERIEN,PATCHIEN
        DO PRSEPATCHNAME(PATCHNAME,.PCKINIT,.VER,.PATCHNUM,.SEQNUM)
        IF (PCKINIT="")!(VER="") GOTO GI2DONE
        SET IEN9D4=+$ORDER(^DIC(9.4,"C",PCKINIT,"")) GOTO:(IEN9D4'>0) GI2DONE
        SET PCKIEN=+$ORDER(^TMG(22709,"B",IEN9D4,"")) GOTO:(PCKIEN'>0) GI2DONE
        SET VERIEN=+$ORDER(^TMG(22709,PCKIEN,1,"B",VER,""))
        IF VERIEN'>0 SET VERIEN=+$ORDER(^TMG(22709,PCKIEN,1,"B",$PIECE(VER,".0",1),""))
        IF VERIEN'>0 GOTO GI2DONE
        SET RESULT=VERIEN_","_PCKIEN_","
GI2DONE
        QUIT RESULT


ENSRLOCL(IENS,INFO,Msg,OPTION,PCKINIT)  ;"ENSURE LOCAL
        ;"Purpose: Ensure that the files have been downloaded from server and stored locally
        ;"Input: IENS --IENS in 22709.11
        ;"       INFO --PASS BY REFERENCE, an OUT PARAMETER.
        ;"       Msg -- PASS BY REFERANCE, an OUT PARAMETER
        ;"              Errors are stored in Msg("ERROR",x)=Message
        ;"                                   Msg("ERROR")=COUNT of last error
        ;"              Message are store in Msg(x)=Message
        ;"                                   Msg=COUNT of last message+1
        ;"       OPTION -- optional.  Pass by reference.
        ;"              OPTION("VERBOSE")=1, means messaages also written directly to output
        ;"       PCKINIT -- Package initials
        ;"Output: INFO will be filled as follows:
        ;"              INFO("PATH")=PATH in HFS
        ;"              INFO("KID FILE")=HFS FILENAME of .KID patch
        ;"              INFO("TEXT FILE")=HFS FILENAME of .TXT accompanying patch
        ;"              INFO("TEXT ONLY")=1 IF there is a text file, but no .KIDS file
        ;"              INFO("KID URL")=URL on server for KID file
        ;"              INFO("TEXT URL")=URL on server for TXT file
        ;"RESULTs: 1 if OK, 0 IF problem.

        NEW TMGMSG,TMGDATA,TMGFDA
        NEW VERBOSE SET VERBOSE=$GET(OPTION("VERBOSE"))
        NEW RESULT SET RESULT=1  ;"default to success
        KILL INFO
        IF $GET(IENS)="" GOTO ELDONE
        DO GETS^DIQ(22709.11,IENS,"1;1.5;2;3;4","","TMGDATA","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO ELDONE
        . NEW TEMPS SET TEMPS=$$GETERRST^TMGDEBU2(.TMGMSG)
        . DO ADDMSG(TEMPS,1,.Msg)
        . IF VERBOSE WRITE TEMPS,!
        NEW URL SET URL=$GET(TMGDATA(22709.11,IENS,1))
        SET INFO("KID URL")=URL
        NEW textURL SET textURL=$GET(TMGDATA(22709.11,IENS,1.5))
        SET INFO("TEXT URL")=textURL
        IF URL="" DO
        . IF textURL'="" SET INFO("TEXT ONLY")=1 QUIT
        . NEW TEMPS SET TEMPS="No URL found for KIDS patch or accompanying Info text file in FM File #22709.22, IENS="_IENS
        . DO ADDMSG(TEMPS,1,.Msg)
        . IF VERBOSE WRITE TEMPS,!

        NEW PATH SET PATH=$GET(TMGDATA(22709.11,IENS,2))
        IF PATH="" DO
        . SET PATH=$GET(^TMG("KIDS","PATCH DIR"),"/tmp/")
        . SET PATH=$$MKTRALDV^TMGIOUTL(PATH)
        . SET PATH=PATH_$$PATCHDIRNAME^TMGPAT4(PCKINIT)
        . SET PATH=$$MKTRALDV^TMGIOUTL(PATH)
        . SET TMGFDA(22709.11,IENS,2)=PATH
        SET INFO("PATH")=PATH
        NEW FILENAME SET FILENAME=$GET(TMGDATA(22709.11,IENS,3))
        NEW textFILENAME SET textFILENAME=$GET(TMGDATA(22709.11,IENS,4))

        IF (FILENAME'=""),$$FILEXIST^TMGIOUTL(PATH_FILENAME) DO
        . SET INFO("KID FILE")=FILENAME
        ELSE  IF (URL'="") DO
        . WRITE !
        . WRITE " -------------------------------------------------------------",!
        . WRITE "== DOWNLOAD ===================================================",!
        . IF VERBOSE WRITE "Downloading KID file from patch repository server..."
        . IF $$DownloadFile^TMGKERNL(URL,PATH,1)
        . WRITE !,"===============================================================",!
        . WRITE " -------------------------------------------------------------",!
        . SET FILENAME=$$FNEXTRCT^TMGIOUTL(URL)
        . IF $$FILEXIST^TMGIOUTL(PATH_FILENAME) DO
        . . IF $$Dos2Unix^TMGKERNL(PATH_FILENAME)
        . . SET TMGFDA(22709.11,IENS,3)=FILENAME
        . . SET INFO("KID FILE")=FILENAME
        . ELSE  SET RESULT=0
        . IF VERBOSE WRITE !

        IF (textFILENAME'=""),$$FILEXIST^TMGIOUTL(PATH_textFILENAME) DO
        . SET INFO("TEXT FILE")=textFILENAME
        ELSE  IF (textURL'="") DO
        . IF VERBOSE WRITE "Downloading TEXT file from patch repository server..."
        . WRITE !
        . WRITE " -------------------------------------------------------------",!
        . WRITE !,"== DOWNLOAD ===================================================",!
        . IF $$DownloadFile^TMGKERNL(textURL,PATH,1)
        . WRITE !,"===============================================================",!
        . WRITE " -------------------------------------------------------------",!
        . SET textFILENAME=$$FNEXTRCT^TMGIOUTL(textURL)
        . IF $$FILEXIST^TMGIOUTL(PATH_textFILENAME) DO
        . . SET TMGFDA(22709.11,IENS,4)=textFILENAME
        . . SET INFO("TEXT FILE")=textFILENAME
        . SET RESULT=0
        . IF VERBOSE WRITE !

        SET RESULT=1  ;"success
ELDONE
        KILL TMGMSG
        IF $DATA(TMGFDA) DO
        . DO FILE^DIE("","TMGFDA","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) DO
        . . DO ADDMSG($$GETERRST^TMGDEBU2(.TMGMSG),1,.Msg)
        . . SET RESULT=0
        . . IF VERBOSE WRITE Msg("ERROR",MsgI),!

        QUIT RESULT


DownloadPATCH(PATCHNAME,PROTOCOL,OPTION,Msg,INFO)
        ;"Purpose: Ensure that the Patch has been downloaded from server and stored locally
        ;"Input: PATCHNAME -- the name of the patch to get, e.g. ABC*12.34*1234 [SEQ #123]
        ;"       PROTOCOL -- OPTIONAL.  Default is 'ftp://'
        ;"       OPTION -- optional.  Pass by reference.
        ;"              OPTION("VERBOSE")=1, means messaages also written directly to output
        ;"       Msg -- PASS BY REFERANCE, an OUT PARAMETER
        ;"              Errors are stored in Msg("ERROR",x)=Message
        ;"                                   Msg("ERROR")=COUNT of last error
        ;"              Message are store in Msg(x)=Message
        ;"                                   Msg=COUNT of last message+1
        ;"       INFO -- PASS BY REFERENCE, an IN and OUT PARAMETER.
        ;"              INFO("PATH")=PATH in HFS
        ;"              INFO("KID FILE")=HFS FILENAME of .KID patch
        ;"              INFO("TEXT FILE")=HFS FILENAME of .TXT accompanying patch
        ;"              INFO("TEXT ONLY")=1 IF there is a text file, but no .KIDS file
        ;"              INFO("KID URL")=URL on server for KID file
        ;"              INFO("TEXT URL")=URL on server for TXT file
        ;"Output: INFO will be filled as follows:
        ;"              INFO("PATH")=PATH in HFS
        ;"              INFO("KID FILE")=HFS FILENAME of .KID patch
        ;"RESULTs: 1 if OK, 0 IF problem.

        NEW RESULT SET RESULT=1
        NEW SEQNUM,PCKINIT,VER,PATCHNUM,SEQNUM,PCKDIRFNAME,URL
        NEW VERBOSE SET VERBOSE=($GET(OPTION("VERBOSE"))=1)
        SET PROTOCOL=$GET(PROTOCOL,"ftp://")
        IF PATCHNAME?2.4N1"*"1.3N.(1"."1.4N)1"*"1.4N DO
        . DO PRSEPATCHNAME(PATCHNAME,.PCKINIT,.VER,.PATCHNUM,.SEQNUM)
        ELSE  IF PATCHNAME?1.4A1"_".E DO
        . SET PCKINIT=$PIECE(PATCHNAME,"_",1)
        ELSE  DO  GOTO:(RESULT=0) DLPDONE
        . NEW TEMPNAME SET TEMPNAME=$GET(INFO("TEXT FILE"))
        . IF TEMPNAME="" SET RESULT=0 QUIT
        . IF TEMPNAME?1.4A1"_".E DO
        . . SET PCKINIT=$PIECE(TEMPNAME,"_",1)
        . ELSE  IF TEMPNAME?1.4A1"-".E DO
        . . SET PCKINIT=$PIECE(TEMPNAME,"-",1)
        . IF $GET(PCKINIT)="" SET RESULT=0

        SET RESULT=$$RefreshPackge(PCKINIT,.Msg,0,.PCKDIRFNAME) GOTO:(RESULT=0) DLPDONE
        SET RESULT=$$FindMultPATCH^TMGPAT4(PATCHNAME,PCKINIT,.OPTION,.URL,.INFO) GOTO:(RESULT=0) DLPDONE

        NEW FILENAME,PATH
        SET PATH=$GET(^TMG("KIDS","PATCH DIR"),"/tmp/")
        SET INFO("PATH")=PATH
        SET FILENAME=$$FNEXTRCT^TMGIOUTL(URL)
        IF $$FILEXIST^TMGIOUTL(PATH_FILENAME) GOTO DLPDONE
        NEW spec SET spec("\ ")="%20"
        SET URL=$$REPLACE^XLFSTR(URL,.spec)
        WRITE !
        WRITE " -------------------------------------------------------------",!
        WRITE "== DOWNLOAD ===================================================",!
        IF VERBOSE WRITE "Downloading "_FILENAME_" from patch repository server...",!
        IF $$DownloadFile^TMGKERNL(PROTOCOL_URL,PATH,1)
        WRITE !,"===============================================================",!
        WRITE " -------------------------------------------------------------",!
        IF $$FILEXIST^TMGIOUTL(PATH_FILENAME)=0 SET RESULT=0 GOTO DLPDONE
        IF $$Dos2Unix^TMGKERNL(PATH_FILENAME)
        SET INFO("KID FILE")=FILENAME
DLPDONE
        QUIT RESULT

FORCEPAT
        ;"Purpose: All user to enter a patch entry

        NEW PCKINIT,VER,DIR,PATCHNUM,SEQNUM,NewPATCH
        DO GETPCKVER^TMGPAT1(.PCKINIT,.VER)
        SET DIR(0)="N",DIR("A")="Enter PATCH NUMBER"
        DO ^DIR WRITE !
        SET PATCHNUM=Y
        IF $GET(DIRUT) GOTO FPDONE
        SET DIR(0)="N",DIR("A")="Enter SEQUENCE NUMBER"
        DO ^DIR WRITE !
        SET SEQNUM=Y
        IF $GET(DIRUT) GOTO FPDONE
        DO ForceP2(.PCKINIT,.VER,.PATCHNUM,.SEQNUM)
        QUIT

ForceP2(PCKINIT,VER,PATCHNUM,SEQNUM)
        ;"Purpose: Hack WRITE a patch into Package file, based in componant parts.
        ;"RESULTs: NONE

        NEW NewPATCH SET NewPATCH=PCKINIT_"*"_VER_"*"_PATCHNUM_" SEQ #"_SEQNUM
        NEW DIR SET DIR(0)="Y"
        SET DIR("A")="HACK/FORCE an entry in the Package file for: "_NewPATCH_" (Y/N)"
        DO ^DIR WRITE !
        IF $GET(DIRUT)!(Y'=1) GOTO FPDONE
        NEW Msg
        IF $$MakePATCHEntry^TMGPAT2(NewPATCH,.Msg)
        IF $$SHOWMSG^TMGPAT2(.Msg)
FPDONE
        QUIT


MakePATCHEntry(PATCHNAME,Msg)
        ;"Purpose: For times when a patch was informational only, and there was
        ;"         no KIDS file to actually install, then this can make pseudo-entries
        ;"         to show that something was processed.
        ;"Input: PATCHNAME -- The name of the patch.  Eg: DI*22*123 SEQ #456"
        ;"       Msg -- PASS BY REFERANCE, an OUT PARAMETER
        ;"              Errors are stored in Msg("ERROR",x)=Message
        ;"                                   Msg("ERROR")=COUNT of last error
        ;"              Message are store in Msg(x)=Message
        ;"                                   Msg=COUNT of last message+1

        ;"RESULTs: 1 if OK, 0 IF error

        NEW %,X,RESULT
        NEW TMGMSG,TMGFDA,TMGIEN
        SET RESULT=1
        NEW justPATCH SET justPATCH=$PIECE(PATCHNAME," SEQ",1)
        NEW TEMPIEN SET TEMPIEN=+$ORDER(^XPD(9.7,"B",justPATCH,""))
        IF TEMPIEN>0 GOTO MPE2 ;"INSTALL entry already made

        DO NOW^%DTC
        SET TMGFDA(9.7,"+1,",.01)=justPATCH
        SET TMGFDA(9.7,"+1,",.02)=3                     ;"2 = status
        SET TMGFDA(9.7,"+1,",6)="Text_Only "_PATCHNAME  ;"6 = file comment
        SET TMGFDA(9.7,"+1,",9)=DUZ                     ;"9 = Installed by
        SET TMGFDA(9.7,"+1,",11)=%                      ;"11 = Install start time
        SET TMGFDA(9.7,"+1,",17)=%                      ;"17 = Install completion time
        DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO
        . DO ADDMSG($$GETERRST^TMGDEBU2(.TMGMSG),1,.Msg)
        . SET RESULT=0

MPE2    SET PCKINIT=$PIECE(PATCHNAME,"*",1)
        SET VER=$PIECE(PATCHNAME,"*",2)
        IF (PCKINIT="")!(VER="") SET RESULT=0 GOTO MPEDONE
        NEW IEN9D4,IEN9D49
        SET IEN9D4=+$ORDER(^DIC(9.4,"C",PCKINIT,""))
        IF IEN9D4'>0 SET RESULT=0 GOTO MPEDONE
        SET IEN9D49=+$ORDER(^DIC(9.4,IEN9D4,22,"B",VER,""))
        IF IEN9D49'>0 SET RESULT=0 GOTO MPEDONE

        NEW PATCHSEQ SET PATCHSEQ=$PIECE(PATCHNAME,"*",3)
        SET TEMPIEN=$ORDER(^DIC(9.4,IEN9D4,22,IEN9D49,"PAH","B",PATCHSEQ,""))
        IF TEMPIEN>0 DO  GOTO MPE3
        . SET TMGIEN(1)=TEMPIEN
        NEW IENS SET IENS="+1,"_IEN9D49_","_IEN9D4_","
        KILL TMGFDA,TMGMSG,TMGIEN
        SET TMGFDA(9.4901,IENS,.01)=PATCHSEQ ;".01=Patch Hx, e.g. 10 SEQ #10
        SET TMGFDA(9.4901,IENS,.02)="NOW"   ;".02=date applied
        SET TMGFDA(9.4901,IENS,.03)="`"_DUZ ;".03=Applied by
        DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO
        . DO ADDMSG($$GETERRST^TMGDEBU2(.TMGMSG),1,.Msg)
        . SET RESULT=0

MPE3    IF RESULT=0 GOTO MPEDONE
        NEW TMGWP
        KILL TMGFDA,TMGMSG
        SET TMGWP(1)="Patch was informational only.  No installed code etc."
        SET TMGWP(2)="This entry was created as a marker that information was processed."
        SET IENS=TMGIEN(1)_","_IEN9D49_","_IEN9D4_","
        DO WP^DIE(9.4901,IENS,1,"","TMGWP","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO
        . DO ADDMSG($$GETERRST^TMGDEBU2(.TMGMSG),1,.Msg)
        . SET RESULT=0

MPEDONE
        QUIT RESULT

IsInstalled(PATCHNAME)
        ;"Purpose: To return IF a given patch has already been installed.
        ;"Input: PATCHNAME -- format aaaa*nn.nn*mmm [SEQ #xxx]
        ;"RESULT: 1 IF installed, 0 IF not, or problem.

        NEW RESULT SET RESULT=0
        NEW PCKINIT,VER,PATCHNUM

        SET PATCHNAME=$PIECE(PATCHNAME," ",1)
        SET PATCHNAME=$$TrimRType^TMGSTUTL(PATCHNAME,"C") ;"trim any characters off end of patch name, e.g. 'ABC*5.5*123<=='
        SET PCKINIT=$PIECE(PATCHNAME,"*",1)
        SET VER=$PIECE(PATCHNAME,"*",2)
        SET PATCHNUM=$PIECE(PATCHNAME,"*",3)
        IF (PCKINIT="")!(VER="")!(PATCHNUM="") GOTO IIDONE

        NEW IEN9D4,IEN9D49
        SET IEN9D4=+$ORDER(^DIC(9.4,"C",PCKINIT,""))
        IF IEN9D4'>0 GOTO IIDONE
        SET IEN9D49=+$ORDER(^DIC(9.4,IEN9D4,22,"B",VER,""))
        IF IEN9D49'>0 DO
        . NEW ONEVER SET ONEVER=""
        . NEW FOUND SET FOUND=0
        . FOR  SET ONEVER=$ORDER(^DIC(9.4,IEN9D4,22,"B",ONEVER)) QUIT:(ONEVER="")!FOUND  DO
        . . NEW Int,ONEInt,Dec,ONEDec
        . . SET Int=$PIECE(VER,".",1),Dec=+$PIECE(VER,".",2)
        . . SET ONEInt=$PIECE(ONEVER,".",1),ONEDec=+$PIECE(ONEVER,".",2)
        . . IF (Int=ONEInt)&(Dec=ONEDec) DO
        . . . SET IEN9D49=+$ORDER(^DIC(9.4,IEN9D4,22,"B",ONEVER,""))
        . . . SET FOUND=1
        IF IEN9D49'>0 GOTO IIDONE

        GOTO:(IEN9D49'>0) IIDONE

        NEW i,array,DONE
        SET i="",DONE=0
        FOR  SET i=$ORDER(^DIC(9.4,IEN9D4,22,IEN9D49,"PAH","B",i)) QUIT:(i="")  DO
        . NEW ONEPATCHNUM SET ONEPATCHNUM=$PIECE(i," ",1)
        . IF ONEPATCHNUM=PATCHNUM DO
        . . SET RESULT=1,DONE=1
IIDONE  ;
        QUIT RESULT
        ;
PATCHESMATCH(PATCH1,PATCH2) ;"Return 0 or 1 depending if patches are the same (equivalent)
        NEW RESULT SET RESULT=(PATCH1=PATCH2)
        IF RESULT>0 GOTO PTMTDN
        NEW PCK1,VER1,PNUM1,SEQ1
        DO PRSEPATCHNAME(PATCH1,.PCK1,.VER1,.PNUM1,.SEQ1)
        NEW PCK2,VER2,PNUM2,SEQ2
        DO PRSEPATCHNAME(PATCH2,.PCK2,.VER2,.PNUM2,.SEQ2)
        ;"Now compare
        IF PCK1'=PCK2 GOTO PTMTDN
        IF +VER1'=+VER2,VER1>0 GOTO PTMTDN
        IF +PNUM1'=PNUM2,PNUM1>0 GOTO PTMTDN
        IF (+SEQ1=0)!(+SEQ2=0) SET RESULT=1 GOTO PTMTDN
        IF +SEQ1'=+SEQ2 GOTO PTMTDN
        SET RESULT=1
PTMTDN  QUIT RESULT        
        ;
PRSEPATCHNAME(PATCHNAME,PCKINIT,VER,PATCHNUM,SEQNUM)
        ;"Purpose: to parse a patch name (or filename) into it's composit parts.
        ;"Input: PATCHNAME -- the patch name to parse, e.g. ABC*12.34*1234 SEQ #123
        ;"       PCKINIT,VER,PATCHNUM,SEQNUM -- PASS BY REFERENCE, OUT PARAMETERS
        ;"RESULTs: NONE
        NEW DISCARD,TMGDIV SET TMGDIV="{!AN}"  ;"i.e. not alpha-numeric
        SET PATCHNAME=$TRANSLATE(PATCHNAME,"-","_")
        NEW UPNAME SET UPNAME=$$UP^XLFSTR(PATCHNAME)
        NEW EXT FOR EXT=".GBL",".KID",".TXT" IF UPNAME[EXT DO
        . SET PATCHNAME=$PIECE(UPNAME,EXT,1)
        NEW STR SET STR=PATCHNAME
        IF STR["SEQ" DO
        . SET SEQNUM=$$NUMAFTERLABEL^TMGSTUT3(STR,"SEQ")
        . NEW STRA SET STRA=$PIECE(STR,"SEQ",1)
        . NEW STRB SET STRB=$PIECE(STR,"SEQ",2)
        . NEW STRC SET STRC=$PIECE(STRB,SEQNUM,1)
        . NEW STRD SET STRD=$PIECE(STRB,SEQNUM,2)
        . SET STR=STRA_"_"_STRD
        ELSE  SET SEQNUM=""
        SET PCKINIT=STR
        SET TMGDIV="{!AN}" DO CLEAVSTR^TMGSTUT2(.PCKINIT,.TMGDIV,.DISCARD) ;"TMGDIV is changed to actual chars dividing
        SET STR=$EXTRACT(STR,$LENGTH(PCKINIT_TMGDIV)+1,$LENGTH(STR))
        SET TMGDIV="{!AN}",VER=STR DO CLEAVSTR^TMGSTUT2(.VER,.TMGDIV,.DISCARD) ;"TMGDIV is changed to actual chars dividing
        SET VER=$$UP^XLFSTR(VER) IF (VER?.N1"P".N) SET VER=$TRANSLATE(VER,"P",".")  ;"e.g. 5P3 -> 5.3
        ELSE  SET VER=$$NUMSTR^TMGSTUT3(STR)
        SET STR=$EXTRACT(STR,$LENGTH(VER)+1,$LENGTH(STR))
        SET TMGDIV="{!AN}",DISCARD=STR DO CLEAVSTR^TMGSTUT2(.DISCARD,.TMGDIV,.STR)
        IF STR["PAT" DO
        . SET PATCHNUM=$$NUMAFTERLABEL^TMGSTUT3(STR,"PAT")
        ELSE  DO
        . SET PATCHNUM=$$NUMSTR^TMGSTUT3(STR)
        QUIT
        ;
MAKEPATCHNAME(PCKINIT,VER,PATCHNUM,SEQNUM)
        ;"Purpose: the opposite of PRSEPATCHNAME: build up name from parts
        NEW RESULT SET RESULT=PCKINIT_"*"_VER_"*"_PATCHNUM
        IF $GET(SEQNUM)'="" SET RESULT=RESULT_" SEQ #"_SEQNUM
        QUIT RESULT
        ;
        ;
ADDMSG(s,IsError,Msg)
        ;"Purpose: to add a message to Msg ARRAY
        ;"Input: s  -- message.  May be a string, or an array (or both) in format of:
        ;"              s=A line
        ;"              s(1)=line 1
        ;"              s(2)=line 2
        ;"       IsError -- 1 IF is an error message
        ;"       Msg -- PASS BY REFERENCE, an OUT PARAMETER.
        ;"              Errors are stored in Msg("ERROR",x)=Message
        ;"                                   Msg("ERROR")=COUNT of last error
        ;"              Message are store in Msg(x)=Message
        ;"                                   Msg=COUNT of last message+1
        ;"RESULTs: nONE

        SET IsError=+$GET(IsError)
        NEW ONELine,subI SET subI=""
        SET ONELine=$GET(s)
        FOR  DO  SET subI=$ORDER(s(subI)) QUIT:(subI="")  SET ONELine=$GET(s(subI))
        . IF IsError DO
        . . NEW MsgI SET MsgI=$GET(Msg("ERROR"),0)+1
        . . SET Msg("ERROR",MsgI)=ONELine
        . . SET Msg("ERROR")=MsgI
        . ELSE  DO
        . . SET Msg=+$GET(Msg,1)
        . . SET Msg(Msg)=ONELine,Msg=Msg+1
        QUIT


SHOWMSG(Msg,NoPause)
        ;"Purpose: to display the message array
        ;"Input: Msg - PASS BY REFERENCE.  The message array to display.
        ;"       NoPause -- OPTIONAL.  If 1, then user not prompted to hit enter to cont.
        ;"RESULTs: 0 IF OK, 1 IF ERROR found in message array.
        NEW errorFound SET errorFound=0
        IF $DATA(Msg) DO
        . NEW i SET i=""
        . FOR  SET i=$ORDER(Msg(i)) QUIT:(+i'>0)  WRITE "  ",$GET(Msg(i)),!
        . IF $DATA(Msg("ERROR")) DO
        . . WRITE !!,"NOTE: ERRORS ENCOUNTERED:",!
        . . SET i=""
        . . FOR  SET i=$ORDER(Msg("ERROR",i)) QUIT:(+i'>0)  WRITE "  ",$GET(Msg("ERROR",i)),!
        . . SET errorFound=1
        . IF $GET(NoPause)'=1 DO PRESS2GO^TMGUSRI2

        QUIT errorFound