TMGPAT6  ;TMG/kst/Patching tools ;10/19/08, 7/14/22
         ;;1.0;TMG-LIB;**1**;10/19/08
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c)6/29/22  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"This code is related to interating with web server, which is a mirror of the 
 ;"  VA patch server, and to store the data into file 22709.1.  
 ;"The goal of this is to analyze each file, KIDS, TXT, ZIP, or even other types,
 ;"  and to note which patch they belong to, and to catelog each file.  This
 ;"  will hopefully put an end to a constant 'where's the actual patch file' search
 ;"  that I kept encountering.  
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"SCAN() - Scan web server holding all patches, and parse all into Fileman file
 ;"BROWSE() -- To browse file 22709.1
 ;"PICKENTRY(OPTION,OUTPATH,OUTNAME) --  browse file 22709.1 and pick entry
 ;"FIXMISSINGSEQ() --Fix KIDS or TXT entries with missing SEQ
 ;"FIXMISSINGINFOTXT() -- Cycle through all pairs (KIDS and INFO TEXT) for each patch
 ;"GETINFO(OUT,OPTION)  --Get info on ALL patches
 ;"GETPINFO(OUT,PCKINIT)  --Get info on patch, by Package
 ;"GETPVINFO(OUT,PCKINIT,VER)  --Get info on patch, by Package,Ver (PV)
 ;"GETPVPINFO(OUT,PCKINIT,VER,PATCHNUM)  ;Get info on patch, by Package,Ver,Patch# (PVP)
 ;"GETINFO1(OUT,IENS) --Get patch info from 1 record
 ;"GETURL(FILE,IENS)-- Get URL for stored entry
 ;"GETSRURL(IENS22709D121) --Get remote server URL corresponding to entry in subrecord 22709.121 
 ;
 ;"=======================================================================
 ;"Private Functions
 ;"=======================================================================
 ;"CYCLE(INFO) -- Process each web directory node, recursively calling self for subdirectories
 ;"INITIALIZE(INFO) --Setup INFO var 
 ;"ARRHASPATCH(ARR,PATCH) -- Determine if ARR, holding list of patches, has an entry matching PATCH  
 ;"GETUNPAIRED(OUT)  -- Make listing of all patches that are not a KIDS & INFOTEXT duo.
 ;"FIX1MISSINGINFO(DATA) -- Fix missing for 1 item
 ;"REF2IENS(REF) -- Get IENS from "C" or "D" index, given ref up to part BEFORE IENS
 ;"ADDDIR(INFO,IENS) --Add top level dir
 ;"ISDIR0(AFILE) --Decide if filename is a directory
 ;"ISZIP0(AFILE) --Decide if filename is of type ZIP
 ;"CHARACTERIZE(INFO,ISZIP,ISDIR,ISFILE,INZIPDIR,ISTEXT,ISKIDS,ISGBL) --Characterize file
 ;"ISDIR(INFO) --Decide if filename is a directory
 ;"ISZIP(INFO) --Decide if filename is of type ZIP
 ;"ISKIDS(INFO) --Decide if filename is of type KIDS
 ;"ISTEXT(INFO) --Decide if filename is of type TXT
 ;"ISGBL(INFO) -- Decide if filename is of type GBL
 ;"CHARACTERIZE(INFO,ISZIP,ISDIR,ISFILE,INZIPDIR,ISTEXT,ISKIDS,ISGBL) -- Characterize file
 ;"GETDIR(INFO,OUT) --Get directory listing, either from web server, or extract from pseudo-dir ZIP file
 ;"WGETFILE(INFO) --Download from web server to local HFS file
 ;"DELFILE(INFO) --Delete local file, as per name stored in INFO
 ;"LOADFILE(INFO,REF) --Download from web server to local HFS file AND copy head info into @REF
 ;"HEADFILE(LOCALFILE,REF,NUM) --Get first x lines of file into @REF
 ;"HANDLEFILE(INFO) --Take file entry and manage it: search, process, store
 ;"HANDLEDIR(INFO)  --HANDLE DIR (AND ALSO .ZIP AS PSEUDO-DIR) 
 ;"$$PROCESSFILE(INFO) --Take file entry and parse, depending on if KIDS etc. 
 ;"PARSETXT(INFO,ARR)  --Open KIDS info TEXT FILE and extract metadata
 ;"PARSEKIDS(INFO,ARR) --Open KIDS file and extract metadata
 ;"PARSEGBL(INFO,ARR)  -- Evaluate GBL file and extract metadata 
 ;"KIDSHASDPND(IENS)  --See if KIDS entry has listed dependencies.   
 ;"KEEPZIPENTRY(INFO)  --Return if a particular entry in a zip file should be kept (stored in Fileman file).
 ;"FINDFILE121(INFO,IENS)  --Search for prior record representing a file entry in file 22709.121
 ;"STOREFILE(INFO) --Store record representing file entry
 ;"VWRITE(INFO,STR,LF)  --Write string if in verbose mode
 ;"FIXDIRS() ;TEMP FIX
 ;"FIXDIRS2() ;TEMP FIX
 ;"KILL1(IEN)  --KILL IEN IN 22709.12
 ;"FINDSUBDIR(IENS,NAME,OUTIENS) --Search for subdir record representing directory
 ;"FINDDIR(INFO,IENS) --Search for prior record representing directory
 ;"STOREDIR(INFO) --STORE DIR INFO (AND ALSO .ZIP AS PSEUDO-DIR)
 ;"DELENTRY121(IENS)  --Delete entry in 22709.121
 ;"DELDIRENTRY(IENS)  --Delete entry in 22709.12, including all linked records.
 ;"DELSUBDIR(IENS,DELLINKED) --Delete a subdir entry in file 22709.122.
 ;"GETPATHIENS(PATH)  --Get IENS in 22709.12 for PATH
 ;"GETFMDIR(IENS,ARR)  --Get directory listing, from Fileman (FM) file 22709.12 
 ;"GETPATHDIR(PATH,ARR) --Get directory listing, from Fileman (FM) file 22709.1
 ;"GETPATCHDIR(PATH,ARR)  -- Get 'directory' listing (ACTUALLY PATCH LISTING) from Fileman (FM) file 22709.1  
 ;"LOADDIR(PARRAY,CURDIR,OPTION) -- load CURDIR entries into PARRAY
 ;"HNDONSEL(PARRAY,OPTION,INFO) -- handle ON SELECT event from Scroller
 ;"HNDONCMD(PARRAY,OPTION,INFO) -- handle ON CMD event from Scroller
 ;"HNDONKP(PARRAY,OPTION,INFO) -- handle ON KEYPRESS event from Scroller
 ;"TOGGLEDDISPLAYMODE(OPTION) -- Return toggle value for display mode
 ;"SHOWHELP(OPTION) -- show help for file browser
 ;"REF2IENS(REF) --Get IENS from "C" or "D" index, given ref up to part BEFORE IENS
 ;"FIXVER  --Fix versions saved at "2.0" instead of 2, etc.   
 ;"TESTGMD() ;TESTING 
 ;"LOADDIR(PARRAY,CURDIR,OPTION) -- load CURDIR entries into PARRAY
 ;
 ;"=======================================================================
 ;
 ;"======================================================================
 ;"======================================================================
 ;
SCAN()  ;"Entry point for scanning web patch server 
  ;"Purpose: Scan web server holding all patches, and parse all into Fileman file
  NEW INFO,RESULT 
  SET RESULT=$$INITIALIZE(.INFO)   IF RESULT'>0 GOTO SCDN
  SET RESULT=$$CYCLE(.INFO)        IF RESULT'>0 GOTO SCDN
  SET RESULT=$$FIXMISSINGSEQ()     IF RESULT'>0 GOTO SCDN
  SET RESULT=$$FIXMISSINGINFOTXT() IF RESULT'>0 GOTO SCDN
  NEW IEN SET IEN=INFO("IEN 22709.1")  ;"IEN in file 22709.1 for web server.
  NEW TMGFDA,TMGMSG SET TMGFDA(22709.1,IEN_",",.02)="NOW"
  DO FILE^DIE("E","TMGFDA","TMGMSG")  ;"Update last scan time to NOW
  IF $DATA(TMGMSG("DIERR")) DO  GOTO SCDN
  . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
SCDN ;
  IF RESULT'>0 DO 
  . WRITE !,$PIECE(RESULT,"^",2),!
  QUIT
  ;
INITIALIZE(INFO) ;"Setup INFO var 
  ;"Result: 1^OK, or -1^ErrMsg
  NEW RESULT SET RESULT="1^OK"
  NEW URLBASE SET URLBASE=$$URLBASE^TMGKERN4()
  NEW CURDIR SET CURDIR="Patches_By_Application/"
  NEW X,Y,DIC SET DIC(0)="ML",DIC=22709.1,DLAYGO=22709.1,X=URLBASE
  DO ^DIC
  IF +Y'>0 DO  GOTO INDN
  . SET RESULT="-1^Unable to find record for: "_URLBASE
  SET INFO("VERBOSE")=1           ;"For debugging.  later change to 0
  SET INFO("IEN 22709.1")=+Y      ;"IEN in file 22709.1 for web server.
  SET INFO("SERVER ROOT")=URLBASE ;"E.g. foia-vista.worldvista.org/
  SET INFO("PARENT PATH")=""      ;"path(s) of parent directory (if any), NOT including ROOT
  SET INFO("INDENT")=""          ;"indent spaces.
  SET INFO("WEB PARENT IENS")="" ;"The IENS of the multiple entry that represents the parent node
  ;"                              **on the web server**, not the parent in the Fileman file.
  SET INFO("NAME")=CURDIR
  SET INFO("CURRENT DIR")=""   ;"CHANGED BELOW
  NEW TEMP,IENS SET TEMP=$$FINDDIR(.INFO,.IENS) IF +TEMP'>0 SET RESULT=TEMP GOTO INDN
  IF IENS'>0 SET TEMP=$$ADDDIR(.INFO,.IENS) IF +TEMP'>0 SET RESULT=TEMP GOTO INDN
  IF IENS'>0 SET RESULT="-1^Unable to find top level directory: "_CURDIR  GOTO INDN
  SET INFO("IENS 22709.12 FOR TOP DIR")=IENS  ;" e.g. "1,1,"
  SET INFO("CURRENT DIR IENS")=IENS  ;" e.g. "1,1,"
  SET INFO("CURRENT DIR")=CURDIR  ;"Patches_By_Application/   DON'T include prior path.  Should have trailing "/"
  KILL INFO("NAME")
  SET INFO("FORCE RELOAD","ZIPS")=0
  SET INFO("FORCE RELOAD","KIDS")=0
  SET INFO("FORCE RELOAD","TXTS")=0
  SET INFO("FORCE RELOAD","GBL")=1
  SET INFO("QUIET WGET")=1
INDN ;  
  QUIT RESULT
  ;
CYCLE(INFO) ;"Process each web directory node, recursively calling self for subdirectories
  ;"Input: INFO -- pass by reference
  ;"       INFO("SERVER ROOT")=URLBASE
  ;"       INFO("IEN 22709.1")=IEN in file 22709 for web server.
  ;"       INFO("CURRENT DIR")=directory, e.g. "Patches_By_Application/"
  ;"       INFO("IENS 22709.12 FOR TOP DIR")="1,1," <-- top node. 
  ;"       INFO("INDENT")=indent spaces.
  ;"       INFO("PARENT PATH")=path of parent directory (if any), NOT including ROOT
  ;"       INFO("WEB PARENT IENS")="" The IENS of the multiple entry that represents the parent node
  ;"                                  **on the web server**, not the parent in the Fileman file.
  NEW FILES,RESULT SET RESULT="1^OK"
  NEW ISZIP,ISDIR,ISFILE,INZIPDIR,TEMP
  SET INFO("URL")=$GET(INFO("SERVER ROOT"))_$GET(INFO("PARENT PATH"))_$GET(INFO("CURRENT DIR"))
  SET INFO("PARENT PATH")=$GET(INFO("PARENT PATH"))_$GET(INFO("CURRENT DIR"))
  DO GETDIR(.INFO,.FILES)
  NEW AFILE SET AFILE=""
  FOR  SET AFILE=$ORDER(FILES(AFILE)) QUIT:(AFILE="")!(RESULT'>0)  DO
  . KILL INFO("KIDS PATCHES")
  . SET INFO("NAME")=AFILE
  . DO CHARACTERIZE(.INFO,.ISZIP,.ISDIR,.ISFILE,.INZIPDIR) 
  . IF ISFILE,ISZIP=0 SET RESULT=$$HANDLEFILE(.INFO) QUIT 
  . IF ISDIR,INZIPDIR QUIT  ;"Don't store zip subdirs as directory here
  . SET TEMP=$$HANDLEDIR(.INFO) IF TEMP'>0 SET RESULT=TEMP QUIT
  . NEW SUBIENS SET SUBIENS=$PIECE(TEMP,"^",2)
  . IF ISZIP,(SUBIENS>0),(+TEMP'=2) QUIT  ;"Dont recurse into ZIP "subdirectories", since already loaded.  
  . ;"SET UP DETAILS IN SUBINFO FOR RECURSION INTO SUBDIRECTORY
  . NEW SUBINFO MERGE SUBINFO=INFO
  . SET SUBINFO("INDENT")=$GET(INFO("INDENT"))_" . "
  . SET SUBINFO("CURRENT DIR")=AFILE
  . SET SUBINFO("CURRENT DIR IENS")=SUBIENS
  . SET SUBINFO("WEB PARENT IENS")=SUBIENS
  . SET SUBINFO("IN ZIP DIR")=ISZIP
  . SET SUBINFO("NAME")=""
  . SET RESULT=$$CYCLE(.SUBINFO)
  QUIT RESULT
  ;
  ;"=======================================================================
  ;"=======================================================================
  ;
FIXMISSINGSEQ() ;"Fix KIDS or TXT entries with missing SEQ
  ;"cycle through C reference.  Find all entries with 0 SEQ value
  ;"C index missing sequence number if KIDS file
  ;"E.G.
  ;" ^TMG(22709.1,"C","YS",5.01,0,60,1,25,104,1)=""   <-- IENS= 1,104,25,1,  patchnum=60
  ;" ^TMG(22709.1,"C","YS",5.01,0,60,1,166,163,1)=""
  ;" ^TMG(22709.1,"C","YS",      5.01,   0     ,60,   1,697,254,1)=""
  ;"                  PACKAGE, VERSION, SEQ   PATCH  REV IENS
  ;"Result: 0 if no fixes made, otherwise count of fixes made
  NEW FIXARR,PCK,VER,SEQ,PATCH
  NEW COUNT SET COUNT=0
  SET PCK=""
  FOR  SET PCK=$ORDER(^TMG(22709.1,"C",PCK)) QUIT:PCK=""  DO
  . SET VER=""
  . FOR  SET VER=$ORDER(^TMG(22709.1,"C",PCK,VER)) QUIT:VER=""  DO
  . . IF VER=0 QUIT
  . . NEW SEQARR
  . . SET SEQ=""
  . . FOR  SET SEQ=$ORDER(^TMG(22709.1,"C",PCK,VER,SEQ),-1) QUIT:SEQ=""  DO
  . . . SET PATCH=""
  . . . FOR  SET PATCH=$ORDER(^TMG(22709.1,"C",PCK,VER,SEQ,PATCH)) QUIT:PATCH=""  DO
  . . . . NEW REF SET REF=$NAME(^TMG(22709.1,"C",PCK,VER,SEQ,PATCH))
  . . . . NEW IENS SET IENS=$$REF2IENS(REF)
  . . . . IF SEQ>0 SET SEQARR(VER,PATCH)=SEQ_"^"_IENS QUIT  
  . . . . WRITE "FOUND: ",PCK,"*",VER,"*",PATCH," SEQ#",SEQ," in 22709.1211, IENS=",IENS,!
  . . . . IF $DATA(SEQARR(VER,PATCH)) DO
  . . . . . NEW DATA SET DATA=$GET(SEQARR(VER,PATCH))
  . . . . . NEW SEQNUM SET SEQNUM=$PIECE(DATA,"^",1)
  . . . . . NEW SOURCEIENS SET SOURCEIENS=$PIECE(DATA,"^",2)
  . . . . . WRITE "    SEQENCE SHOULD BE: ",SEQNUM,"!!",!
  . . . . . WRITE "    SOURCE DATA IN IENS: ",SOURCEIENS,!
  . . . . . DO DUMPREC^TMGDEBU3(22709.1211,SOURCEIENS,1)
  . . . . . NEW TMGFDA,TMGMSG 
  . . . . . SET TMGFDA(22709.1211,IENS,.04)=$SELECT(SEQNUM="":-1,1:+SEQNUM)
  . . . . . ZWR TMGFDA
  . . . . . DO FILE^DIE("E","TMGFDA","TMGMSG")
  . . . . . IF $DATA(TMGMSG("DIERR")) DO  QUIT
  . . . . . . WRITE $$GETERRST^TMGDEBU2(.TMGMSG),!
  . . . . . WRITE "Correction successfully stored",!
  . . . . . SET COUNT=COUNT+1
  QUIT COUNT
  ;
FIXMISSINGINFOTXT()  ;
  ;"Cycle through all pairs (KIDS and INFO TEXT) for each patch
  ;"If any of the KIDS contains multiple patches, then ensure the INFO TEXT also contains those. 
  ;"Result: 
  NEW RESULT SET RESULT="1^OK"
  NEW COUNT SET COUNT=0
  NEW ALLDATA,OPTION
  SET OPTION("SHOW PROGRESS")=1 
  SET OPTION("USE CACHE")=0
  DO GETINFO(.ALLDATA,.OPTION)
  NEW FIXARR,PCK,VER,SEQ,PATCH
  NEW COUNT SET COUNT=0
  SET PCK=""
  FOR  SET PCK=$ORDER(ALLDATA(PCK)) QUIT:PCK=""  DO
  . SET VER=""
  . FOR  SET VER=$ORDER(ALLDATA(PCK,VER)) QUIT:VER'>0  DO
  . . NEW SEQ SET SEQ=""
  . . FOR  SET SEQ=$ORDER(ALLDATA(PCK,VER,SEQ)) QUIT:SEQ'>0  DO
  . . . NEW HASTEXT SET HASTEXT=($DATA(ALLDATA(PCK,VER,SEQ,"INFOTXT"))>0)
  . . . IF HASTEXT=0 QUIT
  . . . NEW DATA MERGE DATA=ALLDATA(PCK,VER,SEQ)
  . . . NEW KIDSREF SET KIDSREF=$NAME(DATA("KIDS","CONTAINED PATCHES"))
  . . . NEW NUMKIDS SET NUMKIDS=$$LISTCT^TMGMISC(KIDSREF)
  . . . NEW INFOREF SET INFOREF=$NAME(DATA("INFOTXT","CONTAINED PATCHES"))
  . . . NEW NUMINFO SET NUMINFO=$$LISTCT^TMGMISC(INFOREF)
  . . . IF NUMKIDS'>NUMINFO QUIT
  . . . NEW THISPATCH SET THISPATCH=$GET(DATA("NAME"))
  . . . NEW TEXTPATCHES MERGE TEXTPATCHES=DATA("INFOTXT","CONTAINED PATCHES")
  . . . NEW TEXTIENS SET TEXTIENS="" 
  . . . FOR  SET TEXTIENS=$ORDER(DATA("INFOTXT",TEXTIENS)) QUIT:TEXTIENS'>0  DO  ;"THIS IS IENS FOR 22709.121
  . . . . NEW APATCH SET APATCH=""
  . . . . FOR  SET APATCH=$ORDER(DATA("KIDS","CONTAINED PATCHES",APATCH)) QUIT:APATCH=""  DO
  . . . . . IF $$ARRHASPATCH(.TEXTPATCHES,APATCH) QUIT
  . . . . . SET FIXARR(TEXTIENS,APATCH)="",COUNT=COUNT+1
  . . . . . WRITE "SHOULD ADD: ",APATCH," to INFO TEXT (",TEXTIENS,") contained patches",!
  ;"Now do actual fixes.
  WRITE COUNT," entries to fix.",!
  NEW IENS SET IENS=""  ;"THIS IS IENS FOR 22709.121
  FOR  SET IENS=$ORDER(FIXARR(IENS)) QUIT:(IENS="")!(RESULT'>0)  DO
  . NEW APATCH SET APATCH=""
  . FOR  SET APATCH=$ORDER(FIXARR(IENS,APATCH)) QUIT:(APATCH="")!(RESULT'>0)  DO
  . . NEW PCK,VER,PNUM,SEQ
  . . DO PRSEPATCHNAME^TMGPAT2(APATCH,.PCK,.VER,.PNUM,.SEQ)
  . . NEW TMGFDA,TMGMSG,TMGIEN,SUBIENS SET SUBIENS="+1,"_IENS
  . . SET TMGFDA(22709.1211,SUBIENS,.01)=PCK    ;"PACKAGE  
  . . SET TMGFDA(22709.1211,SUBIENS,.02)=+VER   ;"VERSION   '+' to prevent '2' and '2.0' from sorting separately
  . . SET TMGFDA(22709.1211,SUBIENS,.03)=PNUM   ;"PATCH NUM
  . . SET TMGFDA(22709.1211,SUBIENS,.04)=$SELECT(SEQ="":-1,1:+SEQ)   ;"SEQUENCE NUM
  . . SET TMGFDA(22709.1211,SUBIENS,.05)="Y"    ;"ADDED BY DEDUCTION
  . . DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG") ;"ADD RECORD
  . . IF $DATA(TMGMSG("DIERR")) DO  QUIT
  . . . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)  
  . . WRITE "DONE ADDING: ",APATCH," TO ",SUBIENS,!
  QUIT RESULT
  ;
ARRHASPATCH(ARR,PATCH) ;"Determine if ARR, holding list of patches, has an entry matching PATCH  
  ;"INPUT -- ARR.  Format ARR(<A PATCH NAME>)=""
  ;"Result: 0 if not present, 1 if present. 
  NEW RESULT SET RESULT=0
  NEW APATCH SET APATCH=""
  FOR  SET APATCH=$ORDER(ARR(APATCH)) QUIT:(APATCH="")!(RESULT=1)  DO
  . IF $$PATCHESMATCH^TMGPAT2(APATCH,PATCH) SET RESULT=1
  QUIT RESULT
  ;
GETUNPAIRED(OUT)  ;"Make listing of all patches that are not a KIDS & INFOTEXT duo.
  NEW ALLDATA,OPTION
  SET OPTION("SHOW PROGRESS")=1 
  SET OPTION("USE CACHE")=1
  DO GETINFO(.ALLDATA,.OPTION)
  NEW FIXARR,PCK,VER,SEQ,PATCH
  NEW COUNT SET COUNT=0
  SET PCK=""
  FOR  SET PCK=$ORDER(ALLDATA(PCK)) QUIT:PCK=""  DO
  . SET VER=""
  . FOR  SET VER=$ORDER(ALLDATA(PCK,VER)) QUIT:VER'>0  DO
  . . NEW SEQ SET SEQ=""
  . . FOR  SET SEQ=$ORDER(ALLDATA(PCK,VER,SEQ)) QUIT:SEQ'>0  DO
  . . . NEW HASKIDS SET HASKIDS=($DATA(ALLDATA(PCK,VER,SEQ,"KIDS"))>0)
  . . . NEW HASTEXT SET HASTEXT=($DATA(ALLDATA(PCK,VER,SEQ,"INFOTXT"))>0)
  . . . IF HASKIDS&HASTEXT QUIT
  . . . NEW DATA MERGE DATA=ALLDATA(PCK,VER,SEQ)
  . . . IF HASKIDS=0 WRITE "MISSING KIDS FILE INFORMATION",!
  . . . IF HASTEXT=0 WRITE "MISSING INFOTEXT INFORMATION",!
  . . . IF HASKIDS DO
  . . . . NEW REF SET REF=$NAME(DATA("KIDS","CONTAINED PATCHES"))
  . . . . NEW NUMCONTAINED SET NUMCONTAINED=$$LISTCT^TMGMISC(REF)
  . . . . IF NUMCONTAINED<2 QUIT
  . . . . ;"NEW RESULT SET RESULT=$$FIX1MISSINGINFO(.DATA) 
  . . . ZWR DATA(*)
  . . . WRITE "------------------------",!
  QUIT
  ;
FIX1MISSINGINFO(DATA)  ;"Fix missing for 1 item
  ;"Input: DATA -- PASS BY REFERENCE.  An array as output by GETPVPINFO()
  NEW APATCH SET APATCH=""
  FOR  SET APATCH=$ORDER(DATA("KIDS","CONTAINED PATCHES",APATCH)) QUIT:APATCH=""  DO
  . NEW APCK,AVER,APNUM
  . DO PRSEPATCHNAME^TMGPAT2(APATCH,.APCK,.AVER,.APNUM)
  . IF (APCK=$GET(DATA("PACKAGE")))&(+AVER=+$GET(DATA("VER")))&(APNUM=$GET(DATA("PATCH#"))) QUIT  ;"Looking at patch we already have info for
  . NEW OTHERDATA DO GETPVPINFO(.OTHERDATA,APCK,+AVER,APNUM)
  . NEW HASTEXT SET HASTEXT=($DATA(OTHERDATA("INFOTXT"))>0)
  . IF HASTEXT=0 QUIT
  . WRITE "LINKED PACKAGE FROM MULTIBUILD: ",APATCH," has a linked INFO TEXT file.",!
  . ;"Add the patch from DATA as a contained patch in the INFO TEXT file records from OTHERDATA
  QUIT
  ;
REF2IENS(REF) ;"Get IENS from "C" or "D" index, given ref up to part BEFORE IENS
  ;"Input: REF -- Closed form ref up to part BEFORE IENS. NOTE: changed if passed by reference
  ;"NOTE: This will only get 1st value followin REF                  
  ;"Result: an IENS 
  NEW OREF,L1 SET OREF=$$OREF^DILF(REF),L1=$LENGTH(OREF)
  NEW REF2,L2 SET REF2=$QUERY(@REF),L2=$LENGTH(REF2)
  NEW TEMPIENS SET TEMPIENS=$EXTRACT(REF2,L1+1,L2-1)_"," ;"reverse IENS format
  NEW DA DO DA^DILF(TEMPIENS,.DA)
  NEW IENS SET IENS=DA(3)_","_DA(2)_","_DA(1)_","_DA_","
  SET REF=REF2  ;"pass out active reference, if called with .REF
  QUIT IENS
  ;
  ;"=======================================================================
  ;"=======================================================================
  ;  
ADDDIR(INFO,IENS)  ;"Add top level dir
  ;"Input: INFO -- see above
  ;"       IENS -- an OUT parameter
  NEW TEMP,RESULT
  SET TEMP=$$STOREDIR(.INFO)
  IF +TEMP'=1 SET RESULT=TEMP GOTO ADDN
  SET IENS=$PIECE(TEMP,"^",2)
  NEW L SET L=$LENGTH(IENS,",")
  IF L=1 SET IEN=IENS_","
  IF L<3 SET IENS=IENS_+Y_","
  SET $PIECE(TEMP,"^",2)=IENS
  SET RESULT=TEMP
ADDN ;
  QUIT RESULT
  ;
ISDIR0(AFILE) ;;"Decide if filename is a directory
  NEW LASTCHAR SET LASTCHAR=$EXTRACT(AFILE,$LENGTH(AFILE))
  NEW RESULT SET RESULT=(LASTCHAR="/")
  QUIT RESULT
  ;
ISDIR(INFO) ;"Decide if filename in INFO is a directory
  QUIT $$ISDIR0($GET(INFO("NAME")))
  ;
ISZIP0(AFILE) ;"Decide if filename is of type ZIP
  NEW EXT SET EXT=$$UP^XLFSTR($PIECE(AFILE,".",$LENGTH(AFILE,".")))
  NEW RESULT SET RESULT=(EXT="ZIP")
  QUIT RESULT
  ;
ISZIP(INFO) ;"Decide if filename is of type ZIP
  QUIT $$ISZIP0($GET(INFO("NAME")))
  ;
ISKIDS(INFO) ;"Decide if filename is of type KIDS
  NEW AFILE SET AFILE=$GET(INFO("NAME"))
  NEW EXT SET EXT=$$UP^XLFSTR($PIECE(AFILE,".",$LENGTH(AFILE,".")))
  NEW RESULT SET RESULT=((EXT="KIDS")!(EXT="KID"))
  QUIT RESULT
  ;
ISTEXT(INFO) ;"Decide if filename is of type TXT
  NEW AFILE SET AFILE=$GET(INFO("NAME"))
  NEW EXT SET EXT=$$UP^XLFSTR($PIECE(AFILE,".",$LENGTH(AFILE,".")))
  NEW RESULT SET RESULT=((EXT="TXT")!(EXT="TEXT"))!($EXTRACT(EXT,1,3)="TXT")
  QUIT RESULT
  ;
ISGBL(INFO) ;"Decide if filename is of type GBL
  NEW AFILE SET AFILE=$GET(INFO("NAME"))
  NEW EXT SET EXT=$$UP^XLFSTR($PIECE(AFILE,".",$LENGTH(AFILE,".")))
  NEW RESULT SET RESULT=(EXT="GBL")!(EXT="GBLS")
  QUIT RESULT
  ;
CHARACTERIZE(INFO,ISZIP,ISDIR,ISFILE,INZIPDIR,ISTEXT,ISKIDS,ISGBL)  ;"Characterize file
  NEW NAME SET NAME=$GET(INFO("NAME"))
  IF NAME'=$GET(INFO("CHARACTERIZE","NAME")) DO
  . KILL INFO("CHARACTERIZE")
  . SET ISZIP=$$ISZIP(.INFO)
  . SET ISDIR=$$ISDIR(.INFO)
  . SET ISFILE=((ISZIP=1)!(ISDIR=0))
  . SET INZIPDIR=($GET(INFO("IN ZIP DIR"))=1)
  . SET ISTEXT=$$ISTEXT(.INFO)
  . SET ISKIDS=$$ISKIDS(.INFO)
  . SET ISGBL=$$ISGBL(.INFO)
  . SET INFO("CHARACTERIZE","ISZIP")=ISZIP
  . SET INFO("CHARACTERIZE","ISDIR")=ISDIR
  . SET INFO("CHARACTERIZE","ISFILE")=ISFILE
  . SET INFO("CHARACTERIZE","INZIPDIR")=INZIPDIR
  . SET INFO("CHARACTERIZE","ISTEXT")=ISTEXT
  . SET INFO("CHARACTERIZE","ISKIDS")=ISKIDS
  . SET INFO("CHARACTERIZE","ISGBL")=ISGBL
  . SET INFO("CHARACTERIZE","NAME")=NAME
  ELSE  DO
  . SET ISZIP=INFO("CHARACTERIZE","ISZIP")
  . SET ISDIR=INFO("CHARACTERIZE","ISDIR")
  . SET ISFILE=INFO("CHARACTERIZE","ISFILE")
  . SET INZIPDIR=INFO("CHARACTERIZE","INZIPDIR")
  . SET ISTEXT=INFO("CHARACTERIZE","ISTEXT")
  . SET ISKIDS=INFO("CHARACTERIZE","ISKIDS")
  . SET ISGBL=INFO("CHARACTERIZE","ISGBL")
  QUIT
  ;
GETDIR(INFO,OUT) ;"Get directory listing, either from web server, or extract from pseudo-dir ZIP file
  ;"Input: INFO See INFO details above
  ;"       OUT an out parameter
  ;"Results: none
  NEW URL SET URL=$GET(INFO("URL"))
  NEW RESULT SET RESULT=""
  NEW TMGTEMP
  NEW TEMPINFO SET TEMPINFO("NAME")=URL 
  NEW ISZIP SET ISZIP=$$ISZIP(.TEMPINFO)
  IF ISZIP DO
  . NEW TEMPDIR SET TEMPDIR="/tmp/"
  . NEW LOCALFILE SET LOCALFILE=$$WGETFILE(.INFO)
  . SET INFO("LOCAL FILE")=LOCALFILE
  . NEW DESTFILE SET DESTFILE=TEMPDIR_AFILE
  . IF $$LINUXCMD^TMGKERNL("zipinfo -1 "_LOCALFILE,.TMGTEMP)  
  . DO DELFILE(.INFO) ;"Delete local file, as per name stored in INFO
  . NEW INDEX SET INDEX=""
  . FOR  SET INDEX=$ORDER(TMGTEMP(INDEX)) QUIT:INDEX=""  DO
  . . SET TMGTEMP(INDEX)=$TRANSLATE($GET(TMGTEMP(INDEX)),"/","}")  ;"Flatten any subdirs in zip file entries
  ELSE  DO
  . DO VWRITE(.INFO,"(TO GETWDIR)")
  . DO GETWDIR^TMGKERN4(URL,"TMGTEMP")
  . DO VWRITE(.INFO,"(BACK FROM GETWDIR)",1)
  NEW INDEX SET INDEX=""
  FOR  SET INDEX=$ORDER(TMGTEMP(INDEX)) QUIT:INDEX=""  DO
  . NEW LINE SET LINE=$GET(TMGTEMP(INDEX)) QUIT:LINE=""
  . SET LINE=$PIECE(LINE,"^",1)
  . SET OUT(LINE)=""  
  QUIT
  ;
WGETFILE(INFO,QUIET) ;"Download from web server to local HFS file
  ;"Input: INFO See INFO details above
  ;"Results: local path+file name, OR -1^ErrMessage if problem. 
  NEW AFILE SET AFILE=$GET(INFO("NAME"))
  NEW URL SET URL=$GET(INFO("URL"))_AFILE
  NEW TEMPPATH,TEMPFN DO SPLITFPN^TMGIOUTL(URL,.TEMPPATH,.TEMPFN)
  IF AFILE="" SET AFILE=TEMPFN,INFO("NAME")=AFILE
  NEW TEMPDIR SET TEMPDIR="/tmp/"
  NEW OPTION SET OPTION=$SELECT($GET(INFO("QUIET WGET"))=1:"-q",1:"")
  ;"DO VWRITE(.INFO,"(CALLING WGET)")
  DO WGET^TMGKERNL("https://"_URL,OPTION,TEMPDIR)
  ;"DO VWRITE(.INFO,"(BACK FROM WGET)")
  NEW DESTFILE SET DESTFILE=TEMPDIR_AFILE
  NEW RESULT SET RESULT=DESTFILE
  IF $$ISFILE^TMGKERNL(DESTFILE)=0 DO  GOTO GFDN
  . SET RESULT="-1^Unable to download file:"_URL_" to "_DESTFILE
  IF $$Dos2Unix^TMGKERNL(DESTFILE) ;"ignore result
  SET INFO("LOCAL FILE")=DESTFILE
GFDN ;  
  QUIT DESTFILE
  ;
DELFILE(INFO) ;"Delete local file, as per name stored in INFO
  ;"Input: See INFO details above
  ;"Result: none
  NEW LOCALFILE SET LOCALFILE=$GET(INFO("LOCAL FILE"))
  IF $$RMFILE^TMGKERNL(LOCALFILE)=0 DO   ;"0 result means no Linux error
  . KILL INFO("LOCAL FILE")
  QUIT
  ;
LOADFILE(INFO,REF) ;"Download from web server to local HFS file AND copy head info into @REF
  ;"Input: See INFO details above
  ;"Result: -1^ERR MESSGAGE if failure, "1^OK" if success
  NEW RESULT SET RESULT=$$WGETFILE(.INFO) GOTO:(+RESULT=-1) LFDN
  NEW LOCALFILE SET LOCALFILE=RESULT
  NEW ISTEXT,ISKIDS,ISGBL DO CHARACTERIZE(.INFO,,,,,.ISTEXT,.ISKIDS,.ISGBL)
  IF ISTEXT!ISGBL DO  GOTO LFDN
  . SET RESULT=$$HEADFILE(LOCALFILE,REF,40)
  IF ISKIDS DO  GOTO LFDN
  . NEW FILTER,ARR
  . SET FILTER("[","**KIDS**")=""
  . SET FILTER("[","""REQB"",")=""   
  . NEW TEMP SET TEMP=$$GBLOUTGREP^TMGKERNL(LOCALFILE,.FILTER,.ARR)
  . MERGE @REF=ARR
  . SET RESULT="1^OK"
  ELSE  DO  GOTO LFDN
  . IF $$Dos2Unix^TMGKERNL(LOCALFILE)  ;"ignore results. 
  . NEW OPTION SET OPTION("OVERFLOW")=1
  . SET TEMP=$$HFS2ARFP^TMGIOUT3(LOCALFILE,REF,.OPTION)
  . IF TEMP=0 SET RESULT="-1^Unable to load ["_LOCALFILE_"]" QUIT
  . SET RESULT="1^OK"
LFDN ;  
  QUIT RESULT
  ;
HEADFILE(LOCALFILE,REF,NUM) ;"Get first x lines of file into @REF
  SET NUM=+$GET(NUM) IF NUM'>0 SET NUM=20
  NEW CMD SET CMD="head --lines="_NUM_" """_LOCALFILE_""""
  NEW TEMPARR
  ;"DO VWRITE(.INFO,"(CALLING HEADFILE)")  
  NEW RESULT SET RESULT=$$LINUXCMD^TMGKERNL(CMD,.TEMPARR)
  MERGE @REF=TEMPARR
  ;"DO VWRITE(.INFO,"(BACK FROM CALLING HEADFILE)")
  QUIT RESULT
  ;
HANDLEFILE(INFO) ;"Take file entry and manage it: search, process, store
  ;"Input: See INFO details above
  ;"Results : 1^IENS of prior record,  or 1^0 if not found, or -1^Error message.
  NEW ISZIP,ISDIR,ISFILE,INZIPDIR,ISTEXT,ISKIDS,ISGBL,RESULT,IENS,FOUND
  NEW SHOULDDELETE SET SHOULDDELETE=0
  DO CHARACTERIZE(.INFO,.ISZIP,.ISDIR,.ISFILE,.INZIPDIR,.ISTEXT,.ISKIDS,.ISGBL) 
  NEW RELOADKIDS SET RELOADKIDS=+$GET(INFO("FORCE RELOAD","KIDS"))
  NEW RELOADTXT SET RELOADTXT=+$GET(INFO("FORCE RELOAD","TXTS"))
  NEW RELOADGBL SET RELOADGBL=+$GET(INFO("FORCE RELOAD","GBL"))
  DO VWRITE(.INFO,$GET(INFO("INDENT"))_" FILE: "_$GET(INFO("NAME"))_" ")
  IF ISZIP,$$KEEPZIPENTRY(.INFO)=0 DO VWRITE(.INFO,"(NOT storing extra ZIP files)") GOTO HNDFDN
  SET RESULT=$$FINDFILE121(.INFO,.IENS),FOUND=(IENS>0) GOTO:(+RESULT'>0) HNDFDN
  IF 1=0,FOUND,(RELOADKIDS&ISKIDS) DO    ;"temp, delete block later....
  . IF $$KIDSHASDPND(IENS) SET RELOADKIDS=0 QUIT  ;"Don't reprocess if dependencies already noted.    
  IF FOUND,((RELOADTXT&ISTEXT)!(RELOADKIDS&ISKIDS)!(RELOADGBL&ISGBL)) DO  
  . SET SHOULDDELETE=1,RESULT="1^0",FOUND=0  ;"signal to add again
  IF FOUND DO VWRITE(.INFO,"(already added)") GOTO HNDFDN
  SET RESULT=$$PROCESSFILE(.INFO) IF RESULT'>0 DO  GOTO HNDFDN
  . DO VWRITE(.INFO,$PIECE(RESULT,"^",2),1)
  IF SHOULDDELETE DO  GOTO:(+RESULT'>0) HNDFDN
  . NEW TEMP SET TEMP=$$DELENTRY121(IENS) SET IENS=0
  . IF TEMP'>0 SET RESULT=TEMP QUIT
  . DO VWRITE(.INFO,"(PRIOR DELETED)")
  SET RESULT=$$STOREFILE(.INFO) GOTO:(+RESULT'>0) HNDFDN
  DO VWRITE(.INFO,"(ADDED)")
HNDFDN ;  
  DO VWRITE(.INFO,"",1) ;"linefeed
  QUIT RESULT
  ;
HANDLEDIR(INFO)  ;"HANDLE DIR (AND ALSO .ZIP AS PSEUDO-DIR)
  ;"Input: INFO -- see docs above.
  ;"Results : 1^IENS of prior record,  2^IENS of ADDED record, or 1^0 if not found, or -1^Error message.  
  ;"WRITE INDENT,NAME,!
  NEW RESULT,IENS,FOUND,ISZIP,ISDIR,ISFILE,INZIPDIR,ISTEXT,ISKIDS
  DO CHARACTERIZE(.INFO,.ISZIP,.ISDIR)
  NEW RELOADZIP SET RELOADZIP=+$GET(INFO("FORCE RELOAD","ZIPS"))
  IF ISZIP,ISDIR=0 SET INFO("NAME")=$GET(INFO("NAME"))_"/"
  DO VWRITE(.INFO,$GET(INFO("INDENT"))_$SELECT(ISZIP=1:" ZIP as",1:"")_" DIR: "_$GET(INFO("NAME"))_" ")
  SET RESULT=$$FINDDIR(.INFO,.IENS) SET FOUND=(IENS>0) GOTO:(+RESULT'>0) HNDDR
  IF ISZIP,FOUND,RELOADZIP,ISDIR=0 DO  GOTO:(+RESULT'>0) HNDDR  ;"FOR FIXING STORING MISTAKES   
  . NEW TEMP SET TEMP=$$DELDIRENTRY(IENS) IF TEMP'>0 SET RESULT=TEMP QUIT
  . SET RESULT="1^0",FOUND=0  ;"signal to add again
  . DO VWRITE(.INFO,"(PRIOR DELETED)") 
  IF FOUND DO VWRITE(.INFO,"(already added)") GOTO HNDDR
  SET RESULT=$$STOREDIR(.INFO) GOTO:(+RESULT'>0) HNDDR
  IF FOUND DO VWRITE(.INFO,"(ADDED)")
  IF +RESULT=1 SET $PIECE(RESULT,"^",1)=2  ;"signal that record was added.  
HNDDR ;  
  DO VWRITE(.INFO,"",1) ;"linefeed
  QUIT RESULT
  ;  
PROCESSFILE(INFO) ;"Take file entry and parse, depending on if KIDS etc. 
  ;"Input: See INFO details above
  ;"Result: 1^OK, or -1^ErrMessage
  NEW RESULT SET RESULT="1^OK"
  NEW ISZIP,ISDIR,ISFILE,INZIPDIR,ISTEXT,ISKIDS,ISGBL
  DO CHARACTERIZE(.INFO,.ISZIP,.ISDIR,.ISFILE,.INZIPDIR,.ISTEXT,.ISKIDS,.ISGBL) 
  IF (ISTEXT=0)&(ISKIDS=0)&(ISGBL=0)!INZIPDIR GOTO PFDN
  NEW TEMP,TMGTEMP,REF SET REF="TMGTEMP"
  NEW SKIPLOAD SET SKIPLOAD=(ISGBL)
  NEW TEMP SET TEMP=1
  IF 'SKIPLOAD SET TEMP=$$LOADFILE(.INFO,REF) ;"DOWNLOAD FILE into @REF  
  IF +TEMP=-1 SET RESULT=TEMP GOTO PFDN 
  IF ISTEXT DO PARSETXT(.INFO,.TMGTEMP)
  IF ISKIDS DO PARSEKIDS(.INFO,.TMGTEMP)
  IF ISGBL DO PARSEGBL(.INFO,.TMGTEMP)
  IF 'SKIPLOAD DO DELFILE(.INFO)  ;"Delete local file, as per name stored in INFO  
  KILL @REF  
PFDN ;  
  QUIT RESULT
  ;
PARSETXT(INFO,ARR)  ;"Evaluate info TEXT FILE (that is typically distributed with KIDS patches) and extract metadata
  ;"Input: See INFO details above
  ;"       ARR -- array holding contents of file.  
  ;"Output: INFO
  ;"          INFO("FILE TYPE")="I"  ;"Info Text
  ;"          INFO("INFORMATIONAL ONLY")=INFOONLY
  ;"          INFO("KIDS PATCHES")=1
  ;"          INFO("KIDS PATCHES",1)=APATCH
  ;"          INFO("KIDS PATCHES",1,"PACKAGE")=PCKINIT
  ;"          INFO("KIDS PATCHES",1,"VERSION")=VER
  ;"          INFO("KIDS PATCHES",1,"PATCH NUM")=PATCHNUM
  ;"          INFO("KIDS PATCHES",1,"SEQ NUM")=SEQNUM
  ;"          INFO("KIDS PATCHES",1,"SEQ NUM")=SEQNUM
  ;"Result: none
  NEW IDX SET IDX=0
  NEW DONE SET DONE=0
  NEW INFOONLY SET INFOONLY=""
  NEW APATCH SET APATCH=""
  NEW SEQNUM SET SEQNUM=""
  FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:(IDX'>0)!(IDX>20)!(DONE)  DO
  . NEW LINE SET LINE=$GET(ARR(IDX)) 
  . IF LINE["Designation:" SET APATCH=$$TRIM^XLFSTR($PIECE(LINE,"Designation:",2))
  . IF LINE["Category:" DO
  . . NEW IDX2 SET IDX2=+$ORDER(ARR(IDX))
  . . NEW NEXTLINE SET NEXTLINE=$GET(ARR(IDX2))
  . . IF NEXTLINE["Informational" SET INFOONLY="Y"
  . IF LINE["SEQ #" DO 
  . . SET SEQNUM=$$NUMSTR^TMGSTUT3($PIECE(LINE,"SEQ #",2))
  . IF (APATCH'="")&(SEQNUM'="") DO  QUIT
  . . SET APATCH=APATCH_" SEQ #"_SEQNUM
  . . ;"SET DONE=1
  IF APATCH'="" DO
  . SET INFO("FILE TYPE")="I"  ;"Info Text
  . SET INFO("INFORMATIONAL ONLY")=INFOONLY
  . DO PRSEPATCHNAME^TMGPAT2(APATCH,.PCKINIT,.VER,.PATCHNUM,.SEQNUM)
  . SET INFO("KIDS PATCHES",1)=APATCH
  . SET INFO("KIDS PATCHES",1,"PACKAGE")=PCKINIT
  . SET INFO("KIDS PATCHES",1,"VERSION")=VER
  . SET INFO("KIDS PATCHES",1,"PATCH NUM")=PATCHNUM
  . SET INFO("KIDS PATCHES",1,"SEQ NUM")=SEQNUM
  . SET INFO("KIDS PATCHES")=1
  . IF SEQNUM="",$GET(INFO("NAME"))["SEQ" DO
  . . SET APATCH=INFO("NAME")
  . . DO PRSEPATCHNAME^TMGPAT2(APATCH,.PCKINIT,.VER,.PATCHNUM,.SEQNUM)
  . . SET INFO("KIDS PATCHES",1,"SEQ NUM")=SEQNUM
  NEW TEMP
  DO ARRANALYZE^TMGPAT4(.TMGTEMP,.TEMP)
  ;" NEW TEMP,FPNAME,PATH,FNAME
  ;" SET FPNAME=$GET(INFO("LOCAL FILE")) QUIT:FPNAME=""
  ;" DO SPLITFPN^TMGIOUTL(FPNAME,.PATH,.FNAME)
  ;" SET TEMP("PATH")=PATH,TEMP("TEXT FILE")=FNAME
  ;" NEW OPTION SET OPTION("VERBOSE")=$GET(INFO("VERBOSE"))
  ;" DO ANALYZE^TMGPAT4(.TEMP,.OPTION)
  NEW COUNT SET COUNT=1
  SET APATCH=""
  FOR  SET APATCH=$ORDER(TEMP("SPECIFIED REQ",APATCH)) QUIT:APATCH=""  DO
  . NEW PCKINIT,VER,PATCHNUM,SEQNUM
  . DO PRSEPATCHNAME^TMGPAT2(APATCH,.PCKINIT,.VER,.PATCHNUM,.SEQNUM)
  . SET INFO("DEPENDENCIES",COUNT)=APATCH
  . SET INFO("DEPENDENCIES",COUNT,"PACKAGE")=PCKINIT
  . SET INFO("DEPENDENCIES",COUNT,"VERSION")=+VER
  . SET INFO("DEPENDENCIES",COUNT,"PATCH NUM")=PATCHNUM
  . SET INFO("DEPENDENCIES",COUNT,"SEQ NUM")=SEQNUM
  . SET INFO("DEPENDENCIES")=COUNT
  . SET COUNT=COUNT+1  
  QUIT
  ;
PARSEKIDS(INFO,ARR)  ;"Evaluate KIDS file and extract metadata
  ;"Input: See INFO details above
  ;"       ARR -- array holding contents of file.
  ;"Output: INFO
  ;"        INFO("DEPENDENCIES",REQIDX)=REQBLD
  ;"        INFO("DEPENDENCIES",REQIDX,"PACKAGE")=$P(REQBLD,"*",1)
  ;"        INFO("DEPENDENCIES",REQIDX,"VERSION")=+$P(REQBLD,"*",2)
  ;"        INFO("DEPENDENCIES",REQIDX,"PATCH NUM")=$P(REQBLD,"*",3)
  ;"        INFO("DEPENDENCIES",REQIDX,"SEQ NUM")=""   ;"NO SEQ NUM
  ;"        INFO("DEPENDENCIES")=REQCOUNT
  ;"        INFO("KIDS PATCHES",KDX)=APATCH
  ;"        INFO("KIDS PATCHES",KDX,"PACKAGE")=PCKINIT
  ;"        INFO("KIDS PATCHES",KDX,"VERSION")=VER
  ;"        INFO("KIDS PATCHES",KDX,"PATCH NUM")=PATCHNUM
  ;"        INFO("KIDS PATCHES",KDX,"SEQ NUM")=SEQNUM
  ;"        INFO("KIDS PATCHES")=KDX
  ;"        INFO("KIDS PATCHES",KDX,"SEQ NUM")=SEQNUM  
  ;"Result: none
  NEW IDX SET IDX=0
  NEW REQCOUNT,REQIDX SET REQCOUNT=0
  NEW DONE SET DONE=0
  FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:(IDX'>0)!(DONE)  DO
  . NEW LINE SET LINE=$GET(ARR(IDX))
  . IF LINE["REQB" DO
  . . SET REQIDX=+$PIECE(LINE,",",4)
  . . IF REQIDX'>0 QUIT
  . . NEW REQBLD SET REQBLD=$PIECE($GET(ARR(IDX+1)),"^",1)
  . . SET INFO("DEPENDENCIES",REQIDX)=REQBLD
  . . SET INFO("DEPENDENCIES",REQIDX,"PACKAGE")=$P(REQBLD,"*",1)
  . . SET INFO("DEPENDENCIES",REQIDX,"VERSION")=+$P(REQBLD,"*",2)
  . . SET INFO("DEPENDENCIES",REQIDX,"PATCH NUM")=$P(REQBLD,"*",3)
  . . SET INFO("DEPENDENCIES",REQIDX,"SEQ NUM")=""   ;"NO SEQ NUM
  . . SET REQCOUNT=REQCOUNT+1
  . . SET INFO("DEPENDENCIES")=REQCOUNT
  . IF LINE["**KIDS**" DO
  . . SET INFO("FILE TYPE")="K"
  . . SET LINE=$PIECE(LINE,"**KIDS**",2)
  . . NEW CH FOR  SET CH=$EXTRACT(LINE,1) QUIT:(": "'[CH)!(CH="")  S LINE=$EXTRACT(LINE,2,$LENGTH(LINE))
  . . NEW JDX FOR JDX=1:1:$LENGTH(LINE,"^") DO
  . . . NEW APATCH SET APATCH=$PIECE(LINE,"^",JDX) QUIT:APATCH=""
  . . . NEW KDX SET KDX=+$GET(INFO("KIDS PATCHES"))
  . . . NEW PCKINIT,VER,PATCHNUM,SEQNUM
  . . . DO PRSEPATCHNAME^TMGPAT2(APATCH,.PCKINIT,.VER,.PATCHNUM,.SEQNUM)
  . . . SET KDX=KDX+1
  . . . SET INFO("KIDS PATCHES",KDX)=APATCH
  . . . SET INFO("KIDS PATCHES",KDX,"PACKAGE")=PCKINIT
  . . . SET INFO("KIDS PATCHES",KDX,"VERSION")=VER
  . . . SET INFO("KIDS PATCHES",KDX,"PATCH NUM")=PATCHNUM
  . . . SET INFO("KIDS PATCHES",KDX,"SEQ NUM")=SEQNUM
  . . . SET INFO("KIDS PATCHES")=KDX
  . . . IF SEQNUM="",$GET(INFO("NAME"))["SEQ" DO
  . . . . SET APATCH=INFO("NAME")
  . . . . DO PRSEPATCHNAME^TMGPAT2(APATCH,.PCKINIT,.VER,.PATCHNUM,.SEQNUM)
  . . . . SET INFO("KIDS PATCHES",KDX,"SEQ NUM")=SEQNUM  
 QUIT
  ;
PARSEGBL(INFO,ARR)  ;"Evaluate GBL file and extract metadata
  ;"Input: See INFO details above
  ;"       ARR -- array holding contents of file.
  ;
  ;"RESULT: none
  NEW APATCH,PCKINIT,VER,PATCHNUM,SEQNUM
  SET APATCH=$GET(INFO("NAME"))
  DO PRSEPATCHNAME^TMGPAT2(APATCH,.PCKINIT,.VER,.PATCHNUM,.SEQNUM)  
  SET INFO("FILE TYPE")="G"  ;"GBL file
  SET INFO("KIDS PATCHES")=1
  SET INFO("KIDS PATCHES",1)=APATCH
  SET INFO("KIDS PATCHES",1,"PACKAGE")=PCKINIT
  SET INFO("KIDS PATCHES",1,"VERSION")=VER
  SET INFO("KIDS PATCHES",1,"PATCH NUM")=PATCHNUM
  SET INFO("KIDS PATCHES",1,"SEQ NUM")=SEQNUM
  QUIT
  ;
KIDSHASDPND(IENS)  ;"See if KIDS entry has listed dependencies.  
  ;"Input: IEN -- an IEN in file 22709.121
  ;"Result: 0 if no dependencies, or 1 if has them.  
  NEW DA DO DA^DILF(IENS,.DA)
  NEW REF SET REF=$$CREF^DILF($NAME(^TMG(22709.1,DA(2),1,DA(1),1,DA,2)))
  NEW RESULT SET RESULT=($DATA(@REF)>0)
  QUIT RESULT
  ;
KEEPZIPENTRY(INFO)  ;"Return if a particular entry in a zip file should be kept.
  ;"Input: See INFO details above
  ;"Result: 0 for false, 1 for true
  ;"
  NEW RESULT SET RESULT=$$ISKIDS(.INFO)
  NEW L SET L=$LENGTH($GET(INFO("NAME")),"/")
  IF L<3 SET RESULT=1
  QUIT RESULT
  ;
FINDFILE121(INFO,IENS)  ;"Search for prior record representing a file entry in 22709.121 
  ;"Input: See INFO details above
  ;"       IENS -- an OUT PARAMETER
  ;"Results : 1^IENS of prior record,  or 1^0 if not found, or -1^Error message.
  ;"        IENS is set to value, or 0 if not found.  
  NEW SERVERIEN SET SERVERIEN=$GET(INFO("IEN 22709.1"))
  NEW NAME SET NAME=$GET(INFO("NAME"))
  NEW TMGARR,TMGMSG,RESULT SET TMGRESULT="-1^error",IENS=0  ;"default to error state
  NEW DIRIENS SET DIRIENS=$GET(INFO("WEB PARENT IENS"))
  IF DIRIENS="" SET DIRIENS=$GET(INFO("IENS 22709.12 FOR TOP DIR"))  ;"Happens with a file entry in top directory of web server 
  DO FIND^DIC(22709.121,","_DIRIENS,"@;.01;","BE",NAME,"*",,,,"TMGARR","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO FDF121DN
  . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  SET RESULT="1^0"  ;"default to not found
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(TMGARR("DILIST",2,IDX)) QUIT:(IDX'>0)  DO
  . NEW SUBIEN SET SUBIEN=$GET(TMGARR("DILIST",2,IDX)) 
  . IF SUBIEN'>0 QUIT
  . NEW ANAME SET ANAME=$GET(TMGARR("DILIST","ID",IDX,.01))
  . IF ANAME'=NAME QUIT
  . SET IENS=SUBIEN_","_DIRIENS
  . SET RESULT="1^"_IENS
FDF121DN ;
  QUIT RESULT
  ;
STOREFILE(INFO)  ;"Store record representing file entry
  ;"Input: INFO -- see above:
  ;"Results : 1^IENS of added record,  or -1^Error message.  
  ;"Assumes entry is NEW (doesn't already exist).  Must check BEFORE CALLING THIS. 
  NEW RESULT SET RESULT="1^OK"  ;"default 
  NEW TMGFDA,TMGMSG,TMGIEN
  NEW SERVERIEN SET SERVERIEN=$GET(INFO("IEN 22709.1"))
  NEW NAME SET NAME=$GET(INFO("NAME"))
  NEW DIRIENS SET DIRIENS=$GET(INFO("WEB PARENT IENS"))
  IF DIRIENS="" SET DIRIENS=$GET(INFO("IENS 22709.12 FOR TOP DIR"))  ;"Happens with a file entry in top directory of web server 
  NEW IENS SET IENS="+1,"_DIRIENS
  ;"----------------------------------------------------
  ;" STORE RECORD IN 22709.121 SUB-SUBFILE  
  ;"----------------------------------------------------
  SET TMGFDA(22709.121,IENS,.01)=NAME   ;"NAME of file.  
  SET TMGFDA(22709.121,IENS,.02)=$GET(INFO("FILE TYPE"))          ;"TYPE of file. (a set)  
  SET TMGFDA(22709.121,IENS,.04)=$GET(INFO("INFORMATIONAL ONLY")) ;"INFORMATIONAL ONLY (a set)  
  DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")  ;"ADD RECORD
  IF $DATA(TMGMSG("DIERR")) DO  GOTO STFDN
  . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  NEW SUBIEN SET SUBIEN=$GET(TMGIEN(1))
  IF SUBIEN'>0 DO  GOTO STFDN
  . SET RESULT="-1^Unable to find added entry, in STOREFILE^TMGKERN4"
  SET $PIECE(IENS,",",1)=SUBIEN
  SET RESULT="1^"_SUBIEN_","_DIRIENS
  ;"----------------------------------------------------
  ;" STORE PACKAGE INFO IN 22709.1211 SUB-SUB-SUBFILE "PATCH INFO" (CONTAINED PATCHES)
  ;"----------------------------------------------------
  NEW SUBIENS
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(INFO("KIDS PATCHES",IDX)) QUIT:(IDX'>0)!(+RESULT=-1)  DO
  . SET SUBIENS="+1,"_IENS 
  . KILL TMGIEN
  . NEW PCKINIT SET PCKINIT=$GET(INFO("KIDS PATCHES",IDX,"PACKAGE")) QUIT:PCKINIT=""
  . NEW VER SET VER=$GET(INFO("KIDS PATCHES",IDX,"VERSION")) QUIT:VER=""
  . NEW PATCHNUM SET PATCHNUM=$GET(INFO("KIDS PATCHES",IDX,"PATCH NUM")) QUIT:PATCHNUM=""
  . NEW SEQ SET SEQ=$GET(INFO("KIDS PATCHES",IDX,"SEQ NUM"))  ;"optional, so don't quit if empty. 
  . SET TMGFDA(22709.1211,SUBIENS,.01)=PCKINIT    ;"PACKAGE  
  . SET TMGFDA(22709.1211,SUBIENS,.02)=+VER       ;"VERSION   '+' to prevent '2' and '2.0' from sorting separately
  . SET TMGFDA(22709.1211,SUBIENS,.03)=PATCHNUM   ;"PATCH NUM
  . SET TMGFDA(22709.1211,SUBIENS,.04)=$SELECT(SEQ="":-1,1:+SEQ)  ;"SEQUENCE NUM
  . DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG") ;"ADD RECORD
  . IF $DATA(TMGMSG("DIERR")) DO  QUIT
  . . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  . KILL INFO("KIDS PATCHES",IDX)
  KILL INFO("KIDS PATCHES")
  ;"----------------------------------------------------
  ;" STORE PACKAGE INFO IN 22709.1212 SUB-SUB-SUBFILE "DEPENDENCIES"
  ;"----------------------------------------------------
  NEW SUBIENS
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(INFO("DEPENDENCIES",IDX)) QUIT:(IDX'>0)!(+RESULT=-1)  DO
  . SET SUBIENS="+1,"_IENS 
  . KILL TMGIEN
  . NEW PCKINIT SET PCKINIT=$GET(INFO("DEPENDENCIES",IDX,"PACKAGE")) QUIT:PCKINIT=""
  . NEW VER SET VER=$GET(INFO("DEPENDENCIES",IDX,"VERSION")) QUIT:VER=""
  . NEW PATCHNUM SET PATCHNUM=$GET(INFO("DEPENDENCIES",IDX,"PATCH NUM")) QUIT:PATCHNUM=""
  . NEW SEQ SET SEQ=$GET(INFO("DEPENDENCIES",IDX,"SEQ NUM"))  ;"optional, so don't quit if empty. 
  . SET TMGFDA(22709.1212,SUBIENS,.01)=PCKINIT    ;"PACKAGE  
  . SET TMGFDA(22709.1212,SUBIENS,.02)=+VER       ;"VERSION   '+' to prevent '2' and '2.0' from sorting separately
  . SET TMGFDA(22709.1212,SUBIENS,.03)=PATCHNUM   ;"PATCH NUM
  . DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG") ;"ADD RECORD
  . IF $DATA(TMGMSG("DIERR")) DO  QUIT
  . . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  . KILL INFO("DEPENDENCIES",IDX)
  KILL INFO("DEPENDENCIES")
STFDN ;
  QUIT RESULT
  ;  
VWRITE(INFO,STR,LF)  ;"Write string if in verbose mode
  NEW VERBOSE SET VERBOSE=+$GET(INFO("VERBOSE"))
  IF VERBOSE'=1 QUIT
  WRITE $GET(STR)
  IF $GET(LF) WRITE !
  QUIT
  ;
FIXDIRS() ;TEMP FIX
  NEW ENTRY SET ENTRY=""
  FOR  SET ENTRY=$ORDER(^TMG(22709.1,1,1,"B",ENTRY)) QUIT:ENTRY=""  DO
  . IF $$ISZIP0(ENTRY)=0 QUIT
  . NEW IEN SET IEN=0
  . FOR  SET IEN=$ORDER(^TMG(22709.1,1,1,"B",ENTRY,IEN)) QUIT:IEN'>0  DO
  . . NEW IENS SET IENS=IEN_","_1
  . . WRITE ENTRY," <-- ",IENS
  . . NEW TEMP SET TEMP=$$DELDIRENTRY(IENS)
  . . IF TEMP'>0 DO  QUIT
  . . . WRITE !,TEMP,!
  . . WRITE " DELETED",!
  QUIT
  ;
FIXDIRS2() ;TEMP FIX
  NEW ENTRY SET ENTRY="OR_"
  FOR  SET ENTRY=$ORDER(^TMG(22709.1,1,1,"B",ENTRY)) QUIT:ENTRY=""  DO
  . IF $LENGTH(ENTRY)=30 QUIT  ;"for now, ignore truncated entries
  . NEW IEN SET IEN=0
  . NEW FOUND SET FOUND=0
  . FOR  SET IEN=$ORDER(^TMG(22709.1,1,1,"B",ENTRY,IEN)) QUIT:IEN'>0  DO
  . . IF FOUND=0 SET FOUND=IEN QUIT
  . . NEW IENS SET IENS=IEN_","_1
  . . WRITE ENTRY," <-- ",IENS," DUPLICATE",!
  . . WRITE ENTRY," <-- ",FOUND,",1, DUPLICATE",!
  . . ;"ZWR ^TMG(22709.1,1,1,"B",ENTRY,*)
  . . ;"ZWR ^TMG(22709.1,1,1,IEN,*)
  . . ;"NEW TEMP SET TEMP=$$DELDIRENTRY(IENS)
  . . ;"IF TEMP'>0 DO  QUIT
  . . ;". WRITE !,TEMP,!
  . . ;"WRITE " DELETED",!
  QUIT
  ;
KILL1(IEN)  ;"KILL IEN IN 22709.12
  NEW IENS SET IENS=IEN_",1,"
  NEW TEMP SET TEMP=$$DELDIRENTRY(IENS)
  IF TEMP'>0 WRITE TEMP,!
  ELSE  WRITE "DELETED #",IEN," and linked records",!
  QUIT
  ;
FINDSUBDIR(IENS,NAME,OUTIENS) ;"Search for subdir record representing directory
  ;"Input: IENS -- IENS for file 22091.12 (a directory entry record)
  ;"       NAME -- name of entry to find.  
  ;"       OUTIENS -- an OUT paremeter
  ;"Results : 1^IENS of prior record,  or 1^0 if not found, or -1^Error message.
  NEW RESULT SET RESULT="1^0",OUTIENS=0  ;"default to not found
  NEW TMGARR,TMGMSG
  DO FIND^DIC(22709.122,","_IENS,"@;.01;","BE",NAME,"*",,,,"TMGARR","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO FDSDRDN
  . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(TMGARR("DILIST",2,IDX)) QUIT:(IDX'>0)  DO
  . NEW SUBIEN SET SUBIEN=$GET(TMGARR("DILIST",2,IDX)) QUIT:SUBIEN'>0
  . SET OUTIENS=SUBIEN_","_IENS
  . SET RESULT="1^"_OUTIENS
FDSDRDN ;
  QUIT RESULT
  ;
FINDDIR(INFO,IENS) ;"Search for prior record representing directory
  ;"Input: INFO -- see docs above.
  ;"       IENS -- an OUT parameter
  ;"Results : 1^IENS of prior record,  or 1^0 if not found, or -1^Error message.
  NEW SERVERIEN SET SERVERIEN=$GET(INFO("IEN 22709.1"))
  NEW NAME SET NAME=$GET(INFO("NAME"))
  NEW DIR SET DIR=$GET(INFO("CURRENT DIR"))
  NEW RESULT SET RESULT="-1^error",IENS=0  ;"default to error state
  NEW PARENTPATH SET PARENTPATH=$GET(INFO("PARENT PATH"))
  NEW ROOT SET ROOT=$$URLBASE^TMGKERN4()
  IF ROOT[$PIECE(PARENTPATH,"/",1) SET PARENTPATH=$PIECE(PARENTPATH,"/",2,999)
  NEW TMGARR,TMGMSG
  DO FIND^DIC(22709.12,","_SERVERIEN_",","@;.01;.02","BE",NAME,"*",,,,"TMGARR","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO FDDRDN
  . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  SET RESULT="1^0"  ;"default to not found
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(TMGARR("DILIST",2,IDX)) QUIT:(IDX'>0)!($PIECE(RESULT,"^",2)>0)  DO
  . NEW SUBIEN SET SUBIEN=$GET(TMGARR("DILIST",2,IDX)) 
  . IF SUBIEN'>0 QUIT
  . NEW ANAME SET ANAME=$GET(TMGARR("DILIST","ID",IDX,.01))
  . IF ANAME'=NAME QUIT ;"ignore/disallow partial matches.  
  . NEW FLD SET FLD=$GET(TMGARR("DILIST","ID",IDX,.02))
  . IF (FLD'=DIR)&(FLD'=PARENTPATH) QUIT
  . SET IENS=SUBIEN_","_SERVERIEN_","
  . SET RESULT="1^"_IENS
FDDRDN ;
  QUIT RESULT
  ;
STOREDIR(INFO) ;"STORE DIR INFO (AND ALSO .ZIP AS PSEUDO-DIR)
  ;"Input: INFO -- see above:
  ;"       INFO("IEN 22709.1"))
  ;"       INFO("NAME"))
  ;"       INFO("PARENT PATH"))
  ;"       INFO("IENS 22709.12 FOR TOP DIR"))
  ;"Results : 1^IENS of added record,  or -1^Error message.  
  ;"Assumes entry is NEW (doesn't already exist).  Must check BEFORE CALLING THIS. 
  NEW RESULT SET RESULT="-1^error"  ;"default to error state
  NEW TMGFDA,TMGMSG,TMGIEN
  NEW SERVERIEN SET SERVERIEN=$GET(INFO("IEN 22709.1"))
  NEW NAME SET NAME=$GET(INFO("NAME"))
  NEW ISZIP SET ISZIP=$$ISZIP(.INFO)
  NEW PARENTPATH SET PARENTPATH=$GET(INFO("PARENT PATH"))
  NEW PARENTIENS SET PARENTIENS=$GET(INFO("CURRENT DIR IENS"))  ;"Current DIR here indicates the directory we are scanning through, not the DIR being added
  ;"----------------------------------------------------
  ;"FIRST, STORE RECORD IN TOP LEVEL OF 22709.12 SUBFILE  
  ;"----------------------------------------------------
  SET TMGFDA(22709.12,"+1,"_SERVERIEN_",",.01)=NAME        ;"NAME WITHOUT PATH
  SET TMGFDA(22709.12,"+1,"_SERVERIEN_",",.02)=PARENTPATH  ;"PARENT PATH
  SET TMGFDA(22709.12,"+1,"_SERVERIEN_",",.03)=PARENTIENS  ;"PARENT IENS (22709.12)  
  IF ISZIP DO
  . SET TMGFDA(22709.12,"+1,"_SERVERIEN_",",.04)="Y"         ;"IS ZIP?
  DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")  ;"ADD RECORD
  IF $DATA(TMGMSG("DIERR")) DO  GOTO STDRDN
  . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  NEW SUBIEN SET SUBIEN=$GET(TMGIEN(1))
  IF SUBIEN'>0 DO  GOTO STDRDN
  . SET RESULT="-1^Unable to find added entry, in STOREDIR^TMGKERN4" 
  ;"NEW THISIENS SET THISIENS=SUBIEN_","_PARENTIENS ;"SET $PIECE(THISIENS,",",1)=SUBIEN
  NEW THISIENS SET THISIENS=PARENTIENS SET $PIECE(THISIENS,",",1)=SUBIEN
  IF THISIENS'["," SET THISIENS=THISIENS_","
  SET RESULT="1^"_THISIENS
  IF PARENTIENS="" GOTO STDRDN
  ;"----------------------------------------------------
  ;"NEXT, STORE LINK AS CHILD OF PARENT DIRECTORY RECORD IN 22709.122 SUB-SUBFILE  
  ;"----------------------------------------------------
  KILL TMGFDA,TMGMSG,TMGIEN
  SET TMGFDA(22709.122,"+1,"_PARENTIENS,.01)=NAME       ;".01-NAME OF CHILD DIR
  SET TMGFDA(22709.122,"+1,"_PARENTIENS,.02)=THISIENS   ;".02-ENTRY IENS (22709.12)
  DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")   ;"ADD RECORD
  IF $DATA(TMGMSG("DIERR")) DO  GOTO STDRDN
  . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
STDRDN ;
  QUIT RESULT
  ; 
DELENTRY121(IENS)  ;"Delete entry in 22709.121
  NEW DA,DIK,INFO
  NEW NAME,PARENTNODEIENS,TEMP,SUBIENS
  NEW RESULT SET RESULT="1^OK"
  IF $LENGTH(IENS,",")'=4 DO  GOTO DEDN
  . SET RESULT="-1^IENS incorrect length: ["_IENS_"]"
  DO DA^DILF(IENS,.DA)
  SET DA=+IENS,DIK=$$OREF^DILF($NAME(^TMG(22709.1,DA(2),1,DA(1),1)))
  DO ^DIK ;"delete entry <IEN>, triggers cross references update  
  QUIT RESULT
  ;
DELDIRENTRY(IENS)  ;"Delete entry in 22709.12, including all linked records.
  NEW DA,DIK,INFO
  NEW NAME,PARENTNODEIENS,TEMP,SUBIENS
  NEW RESULT SET RESULT="1^OK"
  IF $LENGTH(IENS,",")>3 DO  GOTO DEDN
  . SET RESULT="-1^IENS too long: ["_IENS_"]"
  NEW IEN SET IEN=+$PIECE(IENS,",",1)
  NEW PARENTIEN SET PARENTIEN=+$PIECE(IENS,",",2)
  SET DA(1)=IEN,DA(2)=PARENTIEN
  ;"First, cycle through all SUBDIR entries, and delete those linked records and then entry itself.  
  SET DIK=$$OREF^DILF($NAME(^TMG(22709.1,DA(2),1,DA(1),2)))  ;"for file 22709.122, CHILD DIRECTORY
  NEW SUBIEN SET SUBIEN=0
  FOR  SET SUBIEN=$ORDER(^TMG(22709.1,PARENTIEN,1,IEN,2,SUBIEN)) QUIT:(SUBIEN'>0)!(RESULT'>0)  DO
  . NEW TEMPIENS SET TEMPIENS=SUBIEN_","_IENS
  . SET RESULT=$$DELSUBDIR(TEMPIENS,1)
  . IF RESULT'>0 QUIT
  . SET DA=SUBIEN DO ^DIK  ;"delete entry <SUBIEN>, triggers cross references update
  IF RESULT'>0 GOTO DEDN
  ;"Next, cycle through all FILE entries, and delete those linked records and then entry itself
  SET DIK=$$OREF^DILF($NAME(^TMG(22709.1,DA(2),1,DA(1),1)))  ;"for file 22709.121, FILE ENTRY
  NEW SUBIEN SET SUBIEN=0
  FOR  SET SUBIEN=$ORDER(^TMG(22709.1,PARENTIEN,1,IEN,1,SUBIEN)) QUIT:(SUBIEN'>0)!(RESULT'>0)  DO
  . ;"NEW NAME SET NAME=$GET(^TMG(22709.1,PARENTIEN,1,IEN,1,SUBIEN,0))
  . SET DA=SUBIEN DO ^DIK  ;"delete entry <SUBIEN>, triggers cross references update
  IF RESULT'>0 GOTO DEDN
  SET NAME=$PIECE($GET(^TMG(22709.1,PARENTIEN,1,IEN,0)),"^",1)
  SET PARENTNODEIENS=$PIECE($GET(^TMG(22709.1,PARENTIEN,1,IEN,3)),"^",1)
  SET TEMP=$$FINDSUBDIR(PARENTNODEIENS,NAME,.SUBIENS) IF TEMP'>0 SET RESULT=TEMP GOTO DEDN
  IF SUBIENS>0 SET RESULT=$$DELSUBDIR(SUBIENS,0) 
  SET DA=IEN,DA(1)=PARENTIEN
  SET DIK=$$OREF^DILF($NAME(^TMG(22709.1,DA(1),1)))
  DO ^DIK ;"delete entry <IEN>, triggers cross references update  
DEDN ;
  QUIT RESULT
  ;
DELSUBDIR(IENS,DELLINKED) ;"Delete a subdir entry in file 22709.122.  
  ;"Input: IENS -- IENS for 22709.122, e.g. "3,10,1,"
  ;"       DELLINKED -- optional.  If 1, then delete linked records
  ;"Result: 1^OK, or -1^ErrMsg
  NEW DA,DIK
  NEW RESULT SET RESULT="1^OK"
  SET DELLINKED=+$GET(DELLINKED)
  NEW PARENTIEN SET PARENTIEN=+$PIECE($GET(IENS),",",3)
  NEW IEN SET IEN=+$PIECE($GET(IENS),",",2)
  NEW SUBIEN SET SUBIEN=+$PIECE($GET(IENS),",",1)
  SET DA(1)=IEN,DA(2)=PARENTIEN
  SET DIK=$$OREF^DILF($NAME(^TMG(22709.1,DA(2),1,DA(1),2)))
  IF DELLINKED DO
  . NEW ZN SET ZN=$GET(^TMG(22709.1,PARENTIEN,1,IEN,2,SUBIEN,1))
  . NEW LINKEDIENS SET LINKEDIENS=$PIECE(ZN,"^",1)
  . SET RESULT=$$DELDIRENTRY(LINKEDIENS)  ;"Delete children entries
  IF RESULT'>0 GOTO DLSDRDN
  SET DA=SUBIEN DO ^DIK  ;"delete entry <SUBIEN>, triggers cross references update
DLSDRDN ;
  QUIT RESULT
  ;
TESTGMD() ;"TESTING 
  NEW PATH SET PATH="foia-vista.worldvista.org/Patches_By_Application/ORRC-CARE MANAGEMENT/"
  NEW ARR 
  DO GETPATHDIR(PATH,.ARR)
  ZWR ARR
  QUIT
  ;
GETPATHIENS(PATH)  ;"Get IENS in 22709.12 for PATH
  ;"Input: PATH -- a full path, without filename
  ;"Result: 1^<IENS for 22709.12>, or -1^ErrMessage
  NEW RESULT SET RESULT="-1^Not Found"
  NEW PATHARR DO PATH2ARR^TMGIOUTL(.PATH,.PATHARR)
  NEW IDX SET IDX=1
  NEW ROOT SET ROOT=$GET(PATHARR(IDX))  
  NEW X,Y,DIC SET DIC(0)="M",DIC=22709.1,X=ROOT DO ^DIC
  IF +Y'>0 DO  GOTO GMDDN
  . SET RESULT="-1^Unable to find record for: "_ROOT
  NEW INFO
  SET INFO("PARENT PATH")=ROOT
  SET INFO("IEN 22709.1")=+Y
  SET INFO("CURRENT DIR")=""
  NEW IENS SET IENS=+Y_","  
  FOR  SET IDX=$ORDER(PATHARR(IDX)) QUIT:IDX'>0  DO
  . NEW NODE SET NODE=$GET(PATHARR(IDX)) QUIT:NODE=""
  . SET INFO("NAME")=NODE
  . NEW TEMP SET TEMP=$$FINDDIR(.INFO)
  . SET RESULT=$PIECE(TEMP,"^",2)
  . SET INFO("PARENT PATH")=INFO("PARENT PATH")_NODE
  . SET INFO("CURRENT DIR")=NODE
GMDDN ;  
  QUIT RESULT
  ;
GETFMDIR(IENS,ARR)  ;"Get directory listing, from Fileman (FM) file 22709.12 
  ;"Input: IENS -- the IENS record indicator for file 22709.12
  ;"       ARR -- PASS BY REFERENCE.  AN OUT PARAMETER
  ;"Result:  1^OK, or -1^<ErrorMsg>
  ;"Output:  ARR is filles.  Format:
  ;"           ARR(#)="<dir names>"^^22709.122^IENS
  ;"           ARR(#)="<file names>"^22709.121^IENS
  NEW RESULT SET RESULT="1^OK"
  NEW TMGMSG,TMGARR
  DO GETS^DIQ(22709.12,IENS,"**","","TMGARR","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO GFMDDN
  . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  ;"ZWR TMGARR
  NEW IDX SET IDX=1
  FOR FILE=22709.122,22709.121 DO
  . NEW IENS SET IENS=""
  . FOR  SET IENS=$ORDER(TMGARR(FILE,IENS)) QUIT:IENS=""  DO
  . . NEW D01VALUE SET D01VALUE=""
  . . NEW D02VALUE SET D02VALUE=""
  . . NEW FLD SET FLD=""
  . . FOR  SET FLD=$ORDER(TMGARR(FILE,IENS,FLD)) QUIT:FLD=""  DO
  . . . NEW FLDNUM SET FLDNUM=+$PIECE(FLD," ",1)
  . . . NEW VALUE SET VALUE=$GET(TMGARR(FILE,IENS,FLD)) QUIT:VALUE=""
  . . . IF FLDNUM=.01 SET D01VALUE=VALUE
  . . . IF FLDNUM=.02 SET D02VALUE=VALUE
  . . IF FILE=22709.122 DO  ;"subfile for web-server subdirectories 
  . . . SET D01VALUE=$$MKTRALDV^TMGIOUTL(D01VALUE)
  . . . SET ARR(IDX)=D01VALUE_"^"_22709.12_"^"_D02VALUE,IDX=IDX+1  
  . . IF FILE=22709.121 DO ;"subfile for web-server file entries
  . . . SET ARR(IDX)=D01VALUE_"^"_22709.121_"^"_IENS,IDX=IDX+1  
GFMDDN ;  
  QUIT
  ;
GETPATHDIR(PATH,ARR)   ;"Get directory listing, from Fileman (FM) file 22709.1  
  ;"Input: PATH -- a full path, without filename
  ;"       ARR -- PASS BY REFERENCE.  AN OUT PARAMETER.
  ;"Result:  1^OK, or -1^<ErrorMsg>
  ;"Output:  ARR is filles.  Format:
  ;"           ARR(#)="<dir names>"^^22709.122^IENS
  ;"           ARR(#)="<file names>"^22709.121^IENS
  NEW IENS SET IENS=$$GETPATHIENS(.PATH)
  IF IENS'>0 SET RESULT=IENS GOTO GPDDN
  SET RESULT=$$GETFMDIR(IENS,.ARR)  
GPDDN ;
  QUIT RESULT
  ;
GETSRURL(IENS22709D121)  ;"Get remote server URL corresponding to entry in subrecord 22709.121
  ;"Input: IENS22709D121 -- IENS for subfile 22709.121
  NEW RESULT SET RESULT=$$GETURL(22709.121,IENS22709D121) ;"Get URL for stored entry
  QUIT RESULT
  ;
GETPATCHDIR(PATH,ARR)  ;"Get 'directory' listing (ACTUALLY PATCH LISTING) from Fileman (FM) file 22709.1  
  ;"Input: PATH -- a full path, without filename, E.G. <PCKG INIT>/<VER>/
  ;"       ARR -- PASS BY REFERENCE.  AN OUT PARAMETER.
  ;"Result:  1^OK, or -1^<ErrorMsg>
  ;"Output:  ARR is filles.  Format:
  ;"           ARR(#)="<package name>^9.4^/<package initials>/"
  ;"       or
  ;"           ARR(#)="<version number>^VER^/<package initials>"
  ;"       or
  ;"           ARR(#)="<dir names>"^22709.122^IENS
  ;"           ARR(#)="<file names>"^22709.121^IENS
  ;"NOTE: 'C' INDEX HAS THIS FORMAT:
  ;"  ^TMG(22709.1,"C",<PCKINIT>,<VER>,<SEQ#>,<PATCH#>,<WEB SERVER IEN>,<IEN 22709.12>,<IEN 22709.121>,<IEN 22709.1211>)=""
  NEW PATHARR DO PATH2ARR^TMGIOUTL(.PATH,.PATHARR)
  NEW INDEX SET INDEX=1
  NEW COUNT SET COUNT=$ORDER(PATHARR(""),-1)
  NEW PCKINIT,VER
  ;"SHOULD BE: 1 for '/' (browsing all patches), or
  ;"           2 for '/','DI'  (browsing all versions of 1 particular package), or
  ;"           3 for '/','DI','5.3' (browsing all patches for 1 package, 1 ver)
  IF COUNT=1 DO  ;"This is just top level, selecting between packages
  . SET PCKINIT=""
  . FOR  SET PCKINIT=$ORDER(^TMG(22709.1,"C",PCKINIT)) QUIT:PCKINIT=""  DO
  . . NEW PCKNAME SET PCKNAME=PCKINIT
  . . NEW IEN9D4 SET IEN9D4=+$ORDER(^DIC(9.4,"C",PCKINIT,0))
  . . IF IEN9D4>0 DO
  . . . NEW ZN SET ZN=$GET(^DIC(9.4,IEN9D4,0)) QUIT:ZN=""
  . . . SET PCKNAME=PCKNAME_" - ("_$PIECE(ZN,"^",1)_")"
  . . SET ARR(INDEX)=PCKNAME_"^9.4^/"_PCKINIT_"/",INDEX=INDEX+1
  IF COUNT'<2 SET PCKINIT=$PIECE($GET(PATHARR(2)),"/",1)
  IF COUNT=2 DO  ;"(browsing all versions of 1 particular package)
  . SET VER=""
  . FOR  SET VER=$ORDER(^TMG(22709.1,"C",PCKINIT,VER)) QUIT:VER=""  DO
  . . SET ARR(INDEX)=VER_"^VER^/"_PCKINIT_"/"_VER_"/",INDEX=INDEX+1  
  IF COUNT'<3 SET VER=$PIECE($GET(PATHARR(3)),"/",1)
  IF COUNT=3 DO ;"(browsing all patches for 1 package, 1 ver)
  . NEW SEQ SET SEQ=""
  . FOR  SET SEQ=$ORDER(^TMG(22709.1,"C",PCKINIT,VER,SEQ)) QUIT:SEQ=""  DO
  . . NEW PATCHNUM SET PATCHNUM=""
  . . FOR  SET PATCHNUM=$ORDER(^TMG(22709.1,"C",PCKINIT,VER,SEQ,PATCHNUM)) QUIT:PATCHNUM=""  DO
  . . . NEW TOPIEN SET TOPIEN=0
  . . . FOR  SET TOPIEN=$ORDER(^TMG(22709.1,"C",PCKINIT,VER,SEQ,PATCHNUM,TOPIEN)) QUIT:TOPIEN'>0  DO
  . . . . NEW IEN22709D12 SET IEN22709D12=0
  . . . . FOR  SET IEN22709D12=$ORDER(^TMG(22709.1,"C",PCKINIT,VER,SEQ,PATCHNUM,TOPIEN,IEN22709D12)) QUIT:IEN22709D12'>0  DO
  . . . . . NEW IEN22709D121 SET IEN22709D121=0
  . . . . . FOR  SET IEN22709D121=$ORDER(^TMG(22709.1,"C",PCKINIT,VER,SEQ,PATCHNUM,TOPIEN,IEN22709D12,IEN22709D121)) QUIT:IEN22709D121'>0  DO
  . . . . . . ;"NEW TOPIEN SET TOPIEN=1 ;"NOTE: I am assuming top level record=1. Will have to change if more than 1 web server tracked
  . . . . . . ;"NEW IEN22709D12 SET IEN22709D12=+$ORDER(^TMG(22709.1,"C",PCKINIT,VER,SEQ,PATCHNUM,TOPIEN,0))
  . . . . . . ;"NEW IEN22709D121 SET IEN22709D121=+$ORDER(^TMG(22709.1,"C",PCKINIT,VER,SEQ,PATCHNUM,TOPIEN,IEN22709D12,0))
  . . . . . . NEW NODE0 SET NODE0=$GET(^TMG(22709.1,TOPIEN,1,IEN22709D12,1,IEN22709D121,0))
  . . . . . . NEW SETDEF SET SETDEF=$PIECE($GET(^DD(22709.121,.02,0)),"^",3)
  . . . . . . NEW TYPE SET TYPE=$PIECE(NODE0,"^",2)
  . . . . . . NEW TYPENAME SET TYPENAME=""
  . . . . . . NEW IDX FOR IDX=1:1:$LENGTH(SETDEF,";") DO  QUIT:(TYPENAME'="")
  . . . . . . . NEW ASET SET ASET=$PIECE(SETDEF,";",IDX) QUIT:ASET=""
  . . . . . . . IF $PIECE(ASET,":",1)'=TYPE QUIT
  . . . . . . . SET TYPENAME=" ("_$PIECE(ASET,":",2)_")"
  . . . . . . NEW IENS SET IENS=IEN22709D121_","_IEN22709D12_","_TOPIEN_","
  . . . . . . NEW NAME SET NAME="SEQ# "_$$RJ^XLFSTR(SEQ,3,"0")_" "_PCKINIT_"*"_VER_"*"_PATCHNUM_TYPENAME 
  . . . . . . SET ARR(INDEX)=NAME_"^22709.121^"_IENS_"^"_$$GETURL(22709.121,IENS),INDEX=INDEX+1
  QUIT
  ;
FIXVER  ;"Fix versions saved at "2.0" instead of 2, etc.   
  ;"I think I have fixxed SCAN^TMGPAT6 so this won't be needed in the future. 
  NEW PCKINIT SET PCKINIT=""
  FOR  SET PCKINIT=$ORDER(^TMG(22709.1,"C",PCKINIT)) QUIT:PCKINIT=""  DO
  . NEW VER SET VER=""
  . FOR  SET VER=$ORDER(^TMG(22709.1,"C",PCKINIT,VER)) QUIT:VER=""  DO
  . . IF +VER=VER QUIT  ;"only worry about entries where this is NOT true
  . . NEW SEQ SET SEQ=""
  . . FOR  SET SEQ=$ORDER(^TMG(22709.1,"C",PCKINIT,VER,SEQ)) QUIT:SEQ=""  DO
  . . . NEW PATCHNUM SET PATCHNUM=""
  . . . FOR  SET PATCHNUM=$ORDER(^TMG(22709.1,"C",PCKINIT,VER,SEQ,PATCHNUM)) QUIT:PATCHNUM=""  DO
  . . . . NEW IEN22709D1 SET IEN22709D1=0
  . . . . FOR  SET IEN22709D1=$ORDER(^TMG(22709.1,"C",PCKINIT,VER,SEQ,PATCHNUM,IEN22709D1)) QUIT:IEN22709D1'>0  DO
  . . . . . NEW IEN22709D12 SET IEN22709D12=0
  . . . . . FOR  SET IEN22709D12=$ORDER(^TMG(22709.1,"C",PCKINIT,VER,SEQ,PATCHNUM,IEN22709D1,IEN22709D12)) QUIT:IEN22709D12'>0  DO
  . . . . . . NEW IEN22709D121 SET IEN22709D121=0
  . . . . . . FOR  SET IEN22709D121=$ORDER(^TMG(22709.1,"C",PCKINIT,VER,SEQ,PATCHNUM,IEN22709D1,IEN22709D12,IEN22709D121)) QUIT:IEN22709D121'>0  DO
  . . . . . . . NEW IEN22709D1211 SET IEN22709D1211=0
  . . . . . . . FOR  SET IEN22709D1211=$ORDER(^TMG(22709.1,"C",PCKINIT,VER,SEQ,PATCHNUM,IEN22709D1,IEN22709D12,IEN22709D121,IEN22709D1211)) QUIT:IEN22709D1211'>0  DO
  . . . . . . . . NEW TMGFDA,TMGMSG 
  . . . . . . . . NEW IENS SET IENS=IEN22709D1211_","_IEN22709D121_","_IEN22709D12_","_IEN22709D1_","
  . . . . . . . . SET TMGFDA(22709.1211,IENS,.02)=+VER
  . . . . . . . . DO FILE^DIE("E","TMGFDA","TMGMSG")
  . . . . . . . . IF $DATA(TMGMSG("DIERR")) DO  QUIT
  . . . . . . . . . WRITE $$GETERRST^TMGDEBU2(.TMGMSG),!
  . . . . . . . . WRITE "FIXED: ",PCKINIT," ",VER," IENS=",IENS,!
  QUIT
  ;
 ;"======================================================================
 ;"======================================================================
GETINFO(OUT,OPTION)  ;"Get info on ALL patches
  ;"Input: OUT -- PASS BY REFERENCE.  An OUT parameter.  Format as below.
  ;"       OPTION -- OPTIONAL
  ;"       OPTION("SHOW PROGRESS")=1  If 1, a progress bar will be shown on screen.  
  ;"       OPTION("USE CACHE")=1  If 1, then data from prior scan will be returned, if available
  ;"RESULT: none.  
  ;"OUTPUT: OUT is filled.  Prior data is NOT KILLED ...  Format:
  ;"        OUT(<PCKINIT>,<VER>,<SEQ#>,....info block, as per GETPVPINFO below. 
  ;
  NEW TRYCACHE SET TRYCACHE=+$GET(OPTION("USE CACHE"))  
  IF (TRYCACHE=0)!($DATA(^TMP($J,"CONSOLE^TMGPAT3B"))'>0) GOTO GI2
  MERGE OUT=^TMP($J,"CONSOLE^TMGPAT3B")
  GOTO GIFDN
GI2 ;   
  NEW SHOWPROG SET SHOWPROG=+$GET(OPTION("SHOW PROGRESS"))
  IF SHOWPROG WRITE !,!,"SCANNING PATCHES...",!  
  NEW STARTH SET STARTH=$H
  NEW IDX SET IDX=0
  NEW MAX SET MAX=0
  NEW PCK SET PCK=""
  FOR  SET PCK=$ORDER(^TMG(22709.1,"D",PCK)) QUIT:PCK=""  SET MAX=MAX+1  
  SET PCK="" FOR  SET PCK=$ORDER(^TMG(22709.1,"D",PCK)) QUIT:PCK=""  DO
  . IF SHOWPROG DO PROGBAR^TMGUSRI2(IDX,$$LJ^XLFSTR(PCK,4," "),1,MAX,60,STARTH)
  . DO GETPINFO(.OUT,PCK)
  . SET IDX=IDX+1
  KILL ^TMP($J,"CONSOLE^TMGPAT3B")
  MERGE ^TMP($J,"CONSOLE^TMGPAT3B")=OUT  
  IF SHOWPROG WRITE !
GIFDN ;
  QUIT
  ;
GETPINFO(OUT,PCKINIT)  ;"Get info on patch, by Package
  ;"Input: OUT -- PASS BY REFERENCE.  An OUT parameter.  Format as below. 
  ;"       PCKINIT -- e.g. "XU"
  ;"RESULT: none.  
  ;"OUTPUT: OUT is filled.  Prior data is NOT KILLED ...  Format:
  ;"        OUT(<PCKINIT>,<VER>,<SEQ#>,....info block, as per GETPVPINFO below. 
  NEW VER SET VER=""
  FOR  SET VER=$ORDER(^TMG(22709.1,"D",PCKINIT,VER)) QUIT:VER=""  DO
  . IF VER=0 QUIT
  . DO GETPVINFO(.OUT,PCKINIT,VER)
  SET VER=""
  NEW TOTAL SET TOTAL=0
  NEW READY SET READY=0
  NEW INSTALLED SET INSTALLED=0
  FOR  SET VER=$ORDER(OUT(PCKINIT,VER)) QUIT:VER'>0  DO
  . SET TOTAL=TOTAL+$GET(OUT(PCKINIT,VER,"SUMMARY","TOTAL"))
  . SET READY=READY+$GET(OUT(PCKINIT,VER,"SUMMARY","INSTALLABLE"))
  . SET INSTALLED=INSTALLED+$GET(OUT(PCKINIT,VER,"SUMMARY","INSTALLED"))
  SET OUT(PCKINIT,"SUMMARY","TOTAL")=TOTAL
  SET OUT(PCKINIT,"SUMMARY","INSTALLABLE")=READY  
  SET OUT(PCKINIT,"SUMMARY","INSTALLED")=INSTALLED  
  QUIT
  ;
GETPVINFO(OUT,PCKINIT,VER)  ;"Get info on patch, by Package,Ver (PV)
  ;"Input: OUT -- PASS BY REFERENCE.  An OUT parameter.  Format as below. 
  ;"       PCKINIT -- e.g. "XU"
  ;"       VER -- this is numeric version of package.  E.g. 8, not "8.0"
  ;"RESULT: none.  
  ;"OUTPUT: OUT is filled.  Prior data is NOT KILLED ...  Format:
  ;"        OUT(<PCKINIT>,<VER>,<SEQ#>,....info block, as per GETPVPINFO below. 
  ;"        OUT(<PCKINIT>,<VER>,<SEQ#>,"SEQ#")=Seq Num
  ;"        OUT(PCKINIT,VER,"SUMMARY","TOTAL")=TOTAL
  ;"        OUT(PCKINIT,VER,"SUMMARY","INSTALLABLE")=READY
  ;
  NEW VERSTR SET VERSTR=VER IF $PIECE(VERSTR,".",2)="" SET $PIECE(VERSTR,".",2)="0"
  NEW PATCHNUM SET PATCHNUM=""
  FOR  SET PATCHNUM=$ORDER(^TMG(22709.1,"D",PCKINIT,VER,PATCHNUM)) QUIT:PATCHNUM=""  DO
  . NEW DATA DO GETPVPINFO(.DATA,PCKINIT,VER,PATCHNUM)
  . NEW SEQ SET SEQ=+$GET(DATA("SEQ#"))
  . IF SEQ=0 SET SEQ=$ORDER(OUT(PCKINIT,VER,1),-1)+0.01
  . SET DATA("SEQ#")=SEQ
  . MERGE OUT(PCKINIT,VER,SEQ)=DATA  
  NEW TOTAL SET TOTAL=0
  NEW READY SET READY=0
  NEW INSTALLED SET INSTALLED=0
  NEW SEQ SET SEQ=0
  FOR  SET SEQ=$ORDER(OUT(PCKINIT,VER,SEQ)) QUIT:SEQ'>0  DO
  . SET TOTAL=TOTAL+1
  . SET READY=READY+$GET(OUT(PCKINIT,VER,SEQ,"INSTALL","INSTALLABLE"))
  . SET INSTALLED=INSTALLED+$GET(OUT(PCKINIT,VER,SEQ,"INSTALL","INSTALLED"))
  SET OUT(PCKINIT,VER,"SUMMARY","TOTAL")=TOTAL
  SET OUT(PCKINIT,VER,"SUMMARY","INSTALLABLE")=READY
  SET OUT(PCKINIT,VER,"SUMMARY","INSTALLED")=INSTALLED  
  QUIT
  ;
GETPVPINFO(OUT,PCKINIT,VER,PATCHNUM)  ;Get info on patch, by Package,Ver,Patch# (PVP)
  ;"Input: OUT -- PASS BY REFERENCE.  An OUT parameter.  Format as below. 
  ;"       PCKINIT -- e.g. "XU"
  ;"       VER -- this is numeric version of package.  E.g. 8, not "8.0"
  ;"       PATCHNUM -- the patch number to get info for.  
  ;"RESULT: none.  
  ;"OUTPUT:  OUT is filled.  Prior data is NOT KILLED ...  Format:  
  ;"        OUT
  ;"        }~"INFOTXT"
  ;"        | }~"14,4,1," = ""     <-- IENS FOR 22709.121   CAN BE MULTIPLE, esp if part of multipatch.  
  ;"        | | }~"NAME" = ACKQ-3_SEQ-15_PAT-16.TXT
  ;"        | | }~"URL" = foia-vista.worldvista.org/Patches_By_Application/ACKQ-QUASAR/ACKQ-3_SEQ-15_PAT-16.TXT
  ;"        | | }~CONTAINED PATCHES
  ;"        | | | }~"ACKQ*3.0*16" = ""
  ;"        | | }~DEPENDENCIES
  ;"        | | | }~"ACKQ*3.0*15" = ""
  ;"        | | | }~"ACKQ*3.0*17" = ""
  ;"        | }~CONTAINED PATCHES             <-- COMPOSIT OF ALL ABOVE INDIVIDUAL ARRS
  ;"        | | }~"ACKQ*3.0*16" = ""
  ;"        | }~DEPENDENCIES                  <-- COMPOSIT OF ALL ABOVE INDIVIDUAL ARRS
  ;"        | | }~"ACKQ*3.0*15" = ""
  ;"        | | }~"ACKQ*3.0*17" = ""
  ;"        }~"KIDS" 
  ;"        | }~"22,4,1," = ""      <-- IENS FOR 22709.121    COULD BE MULTIPLE (if present in more than one place in web server directory tree) 
  ;"        | | }~"NAME" = ACKQ-3_SEQ-15_PAT-16.KID
  ;"        | | }~"URL" = foia-vista.worldvista.org/Patches_By_Application/ACKQ-QUASAR/ACKQ-3_SEQ-15_PAT-16.KID\
  ;"        | | }~CONTAINED PATCHES  
  ;"        | | | }~"ACKQ*3.0*16" = ""
  ;"        | | }~DEPENDENCIES
  ;"        | | | }~"ACKQ*3.0*15" = ""
  ;"        | | | }~"ACKQ*3.0*17" = ""
  ;"        | }~CONTAINED PATCHES         <-- COMPOSIT OF ALL ABOVE INDIVIDUAL ARRS
  ;"        | | }~"ACKQ*3.0*16" = ""
  ;"        | }~DEPENDENCIES              <-- COMPOSIT OF ALL ABOVE INDIVIDUAL ARRS
  ;"        | | }~"ACKQ*3.0*15" = ""
  ;"        | | }~"ACKQ*3.0*17" = ""
  ;"        | }~"PENDING DEPENDENCIES")=0 or 1
  ;"        }~"NAME" = ACKQ*3.0*16
  ;"        }~"PACKAGE" = ACKQ
  ;"        }~"PATCH#" = 16
  ;"        }~"VER" = 3
  ;"        }~"VERSTR" = 3.0
  ;"          
  IF ($GET(PCKINIT)="")!(VER'>0) GOTO GIDN
  ;"removed becaus KMPD has a legitimate patch#0 --> IF PATCHNUM'>0 GOTO GIDN
  NEW VERSTR SET VERSTR=VER IF $PIECE(VERSTR,".",2)="" SET $PIECE(VERSTR,".",2)="0"
  SET OUT("PACKAGE")=PCKINIT
  SET OUT("PATCH#")=PATCHNUM
  SET OUT("VER")=VER                                                          
  SET OUT("VERSTR")=VERSTR  ;"i.e. 8.0 instead of 8
  NEW NAME SET NAME=$$MAKEPATCHNAME^TMGPAT2(PCKINIT,VERSTR,PATCHNUM)
  SET OUT("NAME")=NAME
  NEW INSTALLED SET INSTALLED=$$PATCH^XPDUTL(NAME)
  NEW REF SET REF=$NAME(^TMG(22709.1,"D",PCKINIT,VER,PATCHNUM))
  NEW BASEREF,L1,L2 SET BASEREF=$$OREF^DILF(REF),L1=$LENGTH(BASEREF)
  FOR  DO  QUIT:REF=""
  . SET REF=$QUERY(@REF),L2=$LENGTH(REF)
  . IF REF'[BASEREF SET REF="" QUIT
  . NEW TEMPIENS SET TEMPIENS=$EXTRACT(REF,L1+1,L2-1)_"," ;"reverse IENS format
  . NEW DA DO DA^DILF(TEMPIENS,.DA)
  . NEW IENS SET IENS=DA(3)_","_DA(2)_","_DA(1)_","_DA_","  ;"IENS for 22709.1211  e.g. 1,2,3,4,
  . DO GETINFO1(.OUT,IENS)
  ;"Now, combine all the dependencies and contained patches from multiple entries into unified entry
  ;"I want them to also be separated so I can see where a particular entry comes from    
  FOR TYPE="INFOTXT","KIDS" DO
  . NEW TARGET FOR TARGET="CONTAINED PATCHES","DEPENDENCIES" DO
  . . NEW SUBIENS SET SUBIENS=""
  . . FOR  SET SUBIENS=$ORDER(OUT(TYPE,SUBIENS)) QUIT:SUBIENS=""  DO
  . . . MERGE OUT(TYPE,TARGET)=OUT(TYPE,SUBIENS,TARGET)
  ;"Now see if any elements in CONTAINED PATCHES is also in DEPENDENCIES
  ;"This happens when a multipatch has, for example, 3 contained patches "A","B","C"
  ;"  Patch "C" can be noted to require "A", and thus a patch will ultimately here get
  ;"  markeD as requiring itself.  We have to remove such entries.
  FOR TYPE="INFOTXT","KIDS" DO
  . NEW CONTAINEDARR MERGE CONTAINEDARR=OUT(TYPE,"CONTAINED PATCHES")
  . NEW APATCH SET APATCH=""
  . FOR  SET APATCH=$ORDER(OUT(TYPE,"DEPENDENCIES",APATCH)) QUIT:APATCH=""  DO
  . . IF $$ARRHASPATCH(.CONTAINEDARR,APATCH)=0 QUIT
  . . NEW INSTALLED SET INSTALLED=+$GET(OUT(TYPE,"DEPENDENCIES",APATCH))
  . . ;"NEW REF SET REF=$NAME(OUT(TYPE,"PENDING DEPENDENCIES"))
  . . ;"IF INSTALLED=0,$GET(@REF)>0 SET @REF=@REF-1
  . . KILL OUT(TYPE,"DEPENDENCIES",APATCH)  
  ;"Now, after removing self-referential dependencies, determine final dependency status.
  NEW PENDINGDEPENDENCIES SET PENDINGDEPENDENCIES=0 
  NEW APATCH SET APATCH="",TYPE="KIDS"
  FOR  SET APATCH=$ORDER(OUT(TYPE,"DEPENDENCIES",APATCH)) QUIT:(APATCH="")!(PENDINGDEPENDENCIES)  DO
  . NEW INSTALLED SET INSTALLED=+$GET(OUT(TYPE,"DEPENDENCIES",APATCH))
  . SET PENDINGDEPENDENCIES=PENDINGDEPENDENCIES!(INSTALLED=0)
  SET OUT("KIDS","PENDING DEPENDENCIES")=PENDINGDEPENDENCIES
  SET OUT("INSTALL","INSTALLED")=INSTALLED
  SET OUT("INSTALL","PENDING DEPENDENCIES")=PENDINGDEPENDENCIES
  SET OUT("INSTALL","INSTALLABLE")=(PENDINGDEPENDENCIES=0)&(INSTALLED=0)
  SET OUT("INSTALL","STATUS")=$SELECT(INSTALLED:"INSTALLED",PENDINGDEPENDENCIES>0:"NOT READY",1:"READY")
GIDN ;
  QUIT
  ;
GETINFO1B(OUT,IENS) ;"Get patch info from 1 record
  ;"Input: OUT -- PASS BY REFERENCE.  An OUT parameter.  Format as below. 
  ;"       IENS -- this IENS for 22709.121. NOTE: This is a parent IENS for that used by GETINFO1
  ;"RESULT: none.  
  ;"OUTPUT:  OUT is filled as per GETINFO1() below.   
  DO GETINFO1(.OUT,"0,"_IENS)  ;"I can do this because GETINFO1() doesn't use the "0" part. 
  ;
GETINFO1(OUT,IENS) ;"Get patch info from 1 record
  ;"Input: OUT -- PASS BY REFERENCE.  An OUT parameter.  Format as below. 
  ;"       IENS -- this IENS for 22709.1211 OR 22709.1212
  ;"RESULT: none.  
  ;"OUTPUT:  OUT is filled.  Prior data is NOT KILLED ...  Format:  
  ;"        OUT
  ;"        }~"INFOTXT"
  ;"        | }~"14,4,1," = ""     <-- IENS FOR 22709.121   CAN BE MULTIPLE, esp if part of multipatch.  
  ;"        | | }~"NAME" = ACKQ-3_SEQ-15_PAT-16.TXT
  ;"        | | }~"URL" = foia-vista.worldvista.org/Patches_By_Application/ACKQ-QUASAR/ACKQ-3_SEQ-15_PAT-16.TXT
  ;"        | | }~CONTAINED PATCHES
  ;"        | | | }~"ACKQ*3.0*16" = ""
  ;"        | | }~DEPENDENCIES
  ;"        | | | }~"ACKQ*3.0*15" = ""
  ;"        | | | }~"ACKQ*3.0*17" = ""
  ;"      OR
  ;"        }~"KIDS" 
  ;"        | }~"22,4,1," = ""      <-- IENS FOR 22709.121    COULD BE MULTIPLE (if present in more than one place in web server directory tree) 
  ;"        | | }~"NAME" = ACKQ-3_SEQ-15_PAT-16.KID
  ;"        | | }~"URL" = foia-vista.worldvista.org/Patches_By_Application/ACKQ-QUASAR/ACKQ-3_SEQ-15_PAT-16.KID\
  ;"        | | }~CONTAINED PATCHES  
  ;"        | | | }~"ACKQ*3.0*16" = ""
  ;"        | | }~DEPENDENCIES
  ;"        | | | }~"ACKQ*3.0*15" = ""
  ;"        | | | }~"ACKQ*3.0*17" = ""
  ;"        | }~"PENDING DEPENDENCIES")=0 or 1
  ;"                                                                                                 
  NEW PARENTIENS SET PARENTIENS=$PIECE(IENS,",",2,5)
  NEW TMGDATA,TMGMSG
  DO GETS^DIQ(22709.121,PARENTIENS,"**","ER","TMGDATA","TMGMSG")
  NEW TYPE SET TYPE=$GET(TMGDATA(22709.121,PARENTIENS,"TYPE","E"),"?")
  NEW INFOONLY SET INFOONLY=$GET(TMGDATA(22709.121,PARENTIENS,"INFORMATIONAL ONLY","E"))
  IF INFOONLY'="" SET OUT(TYPE,"INFORMATIONAL ONLY")=INFOONLY
  NEW PENDINGDEPENDENCIES SET PENDINGDEPENDENCIES=0
  SET OUT(TYPE,PARENTIENS,"NAME")=$GET(TMGDATA(22709.121,PARENTIENS,"NAME OF FILE ENTRY","E"))
  SET OUT(TYPE,PARENTIENS,"URL")=$$GETSRURL(PARENTIENS)
  FOR SUBFILE=22709.1211,22709.1212 DO
  . NEW TARGET SET TARGET=$SELECT(SUBFILE=22709.1211:"CONTAINED PATCHES",SUBFILE=22709.1212:"DEPENDENCIES")
  . NEW SUBIENS SET SUBIENS=""
  . FOR  SET SUBIENS=$ORDER(TMGDATA(SUBFILE,SUBIENS)) QUIT:SUBIENS'>0  DO
  . . NEW PCK,VER,PN,SN
  . . SET PCK=$GET(TMGDATA(SUBFILE,SUBIENS,"PACKAGE","E"))
  . . SET VER=$GET(TMGDATA(SUBFILE,SUBIENS,"VERSION","E"))
  . . SET PN=$GET(TMGDATA(SUBFILE,SUBIENS,"PATCH NUM","E"))
  . . SET SN=$GET(TMGDATA(SUBFILE,SUBIENS,"SEQUENCE NUM","E"))
  . . NEW VERSTR SET VERSTR=VER IF $PIECE(VERSTR,".",2)="" SET $PIECE(VERSTR,".",2)="0"
  . . NEW NAME SET NAME=$$MAKEPATCHNAME^TMGPAT2(PCK,VERSTR,PN,SN)
  . . IF NAME="" QUIT
  . . NEW NAMENOSEQ SET NAMENOSEQ=$$MAKEPATCHNAME^TMGPAT2(PCK,VERSTR,PN)
  . . NEW INSTALLED SET INSTALLED=$$PATCH^XPDUTL(NAMENOSEQ)
  . . SET OUT(TYPE,PARENTIENS,TARGET,NAME)=INSTALLED
  . . IF TARGET="DEPENDENCIES" DO
  . . . SET PENDINGDEPENDENCIES=PENDINGDEPENDENCIES!(INSTALLED=0)
  . . IF PCK=$GET(OUT("PACKAGE")),VER=$GET(OUT("VER")),PN=$GET(OUT("PATCH#")) DO
  . . . IF TARGET="CONTAINED PATCHES",SN'="" SET OUT("SEQ#")=SN  
  IF TYPE="KIDS" DO
  . SET OUT(TYPE,"PENDING DEPENDENCIES")=PENDINGDEPENDENCIES
  QUIT
  ;
 ;"======================================================================
 ;"======================================================================
 ;"NOTE: Browsing code copied and modified from FBROWSE^TMGIOUT2 
 ;
BROWSE() ;"To browse file 22709.1
  NEW MENU,IDX,USRPICK,INFO
  NEW SELECTED,FILE,IENS,OPTION
B1 ;  
  ;"NEW OPTION SET OPTION("DISPLAY MODE")="PACKAGES"
  SET SELECTED=$$PICKENTRY(.OPTION)
  IF SELECTED="" GOTO BRDN
  NEW FULLPATHNAME SET FULLPATHNAME=$PIECE(SELECTED,"^",1)
  SET FILE=$PIECE(SELECTED,"^",2)
  SET IENS=$PIECE(SELECTED,"^",3)
  NEW PCKGPATH SET PCKGPATH=$PIECE(SELECTED,"^",4) ;"only if selected from package mode. 
  WRITE !,!,FULLPATHNAME,!
  WRITE "FILE#: ",FILE,!
  WRITE "IENS: ",IENS,!
  DO DUMPREC^TMGDEBU3(FILE,IENS,1)
  WRITE !
TML1 ;                    
  SET IDX=0  
  KILL MENU SET MENU(IDX)="Select Option For Record"
  SET IDX=IDX+1,MENU(IDX)="Download and view file"_$CHAR(9)_"VIEW^"_FULLPATHNAME
  SET IDX=IDX+1,MENU(IDX)="DELETE record"_$CHAR(9)_"DEL^"_FILE_"^"_IENS
  SET IDX=IDX+1,MENU(IDX)="REFRESH record"_$CHAR(9)_"REFRESH^"_FILE_"^"_IENS_"^"_FULLPATHNAME
  ;"SET IDX=IDX+1,MENU(IDX)="Something Else"_$CHAR(9)_"Opt2"
  SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
  IF USRPICK="^" GOTO B1
  IF USRPICK["VIEW^" DO  GOTO TML1
  . NEW INFO SET INFO("URL")=$PIECE(USRPICK,"^",2)
  . NEW LOCALFILE SET LOCALFILE=$$WGETFILE(.INFO)
  . IF $$EDITHFSFILE^TMGKERNL(LOCALFILE)  ;"ignore result
  . DO DELFILE(.INFO) ;"Delete local file, as per name stored in INFO
  SET %=0
  IF USRPICK["REFRESH^" DO  GOTO TML1:(%'=1),B1:(%=2),BRDN
  . IF FILE'=22709.121 DO  QUIT
  . . WRITE "Wrong file number",!
  . . DO PRESS2GO^TMGUSRI2
  . WRITE !,"This option will DELETE the record and RE-DOWNLOAD it. "
  . WRITE "Continue" DO YN^DICN WRITE !
  . IF %'=1 QUIT
  . NEW INFO DO INITIALIZE(.INFO)
  . NEW AFILE,APATH DO SPLITFPN^TMGIOUTL(FULLPATHNAME,.APATH,.AFILE)
  . SET INFO("NAME")=AFILE
  . SET INFO("URL")=APATH
  . NEW ISZIP,ISDIR,ISFILE,INZIPDIR,ISTEXT,ISKIDS,ISGBL
  . DO CHARACTERIZE(.INFO,.ISZIP,.ISDIR,.ISFILE,.INZIPDIR,.ISTEXT,.ISKIDS,ISGBL) 
  . IF ISFILE=0 DO  QUIT
  . . WRITE "WRONG FILE TYPE",!
  . . DO PRESS2GO^TMGUSRI2
  . IF ISKIDS SET INFO("FORCE RELOAD","KIDS")=1
  . IF ISTEXT SET INFO("FORCE RELOAD","TXTS")=1
  . IF ISGBL SET INFO("FORCE RELOAD","GBL")=1
  . NEW PARENTIENS SET PARENTIENS=$PIECE(IENS,",",2,9999)
  . SET INFO("WEB PARENT IENS")=PARENTIENS
  . NEW TEMP SET TEMP=$$HANDLEFILE(.INFO)  
  . WRITE !,"Done refreshing file",!
  . DO PRESS2GO^TMGUSRI2
  . SET %=2
  IF (USRPICK["DEL^") DO  GOTO TML1
  . NEW AFILE SET AFILE=$PIECE(USRPICK,"^",2)
  . IF AFILE'=22709.12 DO  QUIT
  . . WRITE !,"Wrong file number!"
  . . DO PRESS2GO^TMGUSRI2
  . NEW IENS SET IENS=$PIECE(USRPICK,"^",3)
  . IF %'=1 DO
  . . SET %=2
  . . WRITE !,"Are you sure you want delete this record (and linked records also)",!
  . . WRITE "This can't be undone.  Delete" DO YN^DICN
  . IF %'=1 QUIT
  . NEW TEMP SET TEMP=$$DELDIRENTRY(IENS)
  . IF TEMP'>0 DO  QUIT
  . . WRITE !,"ERROR: ",$PIECE(TEMP,"^",2),!
  . WRITE "DELETED.",!
  . DO PRESS2GO^TMGUSRI2
  GOTO B1
BRDN ;  
  QUIT
  ;
PICKENTRY(OPTION,OUTPATH,OUTNAME) ;
  ;"SCOPE: PUBLIC
  ;"Purpose: To browse file 22709.1 and pick entry
  ;"Input: OPTION [OPTIONAL].  Format as follows.  All entries are optional
  ;"           OPTION("MSG") A message to show user prior to name prompt.
  ;"                         May contain "\n" character for line wrapping.
  ;"           OPTION("HEADER MSG",1) A message to show user in scroller header
  ;"           OPTION("HEADER MSG",2) a 2nd line message to show user in scroller header, etc...
  ;"           OPTION("PATH") Initial default PATH
  ;"           OPTION("MATCH","CASESPEC")=1" <-- means case specific.  Default is NON-specific
  ;"           OPTION("MATCH","*.m")="" -- e.g. use filter '*.m'
  ;"           OPTION("MATCH","*.txt")="" -- e.g. use filter '*.txt"
  ;"               NOTE: files matching ANY one of the specified matches allows display
  ;"           OPTION("DISPLAY MODE") = "FILES" or "PACKAGES" Default is FILES
  ;"                  "FILES" --> Show files as found on web server
  ;"                  "PACKAGES" --> Show sorted entries by packages. 
  ;"        OUTPATH: [OPTIONAL] Pass by reference, filled with selected PATH
  ;"        OUTNAME: [OPTIONAL] Pass by reference, filled with selected name
  ;"Result: returns: SelectedPath+filename^FilemanSubfile#^IENS
  ;"        or       SelectedPath+filename^FilemanSubfile#^IENS^PackagePath  (if package selected)
  ;"        or       "" if aborted
  ;
  WRITE # ;"clear screen
  WRITE "Loading..."
  NEW SCRLFILES,DONE,BANNER
  NEW TMGSELECT,HEADERDIRSTR,HEADERTOP
  NEW SELDIR SET SELDIR=0
  NEW WIDTH SET WIDTH=70     
  NEW LINE SET $PIECE(LINE,"-",WIDTH-2)="-"
  NEW SPACES SET $PIECE(SPACES," ",WIDTH-2)=" "
  NEW HEADERLN SET HEADERLN=1
  NEW DISPLAYMODE SET DISPLAYMODE=$GET(OPTION("DISPLAY MODE"))
  IF DISPLAYMODE="" SET (DISPLAYMODE,OPTION("DISPLAY MODE"))="FILES"
  NEW FOOTIDX SET FOOTIDX=+$ORDER(OPTION("FOOTER",""),-1)+1
  IF $GET(OPTION("PROMPT"))'="" SET OPTION("FOOTER",FOOTIDX)=OPTION("PROMPT")
  SET OPTION("SCRN WIDTH")=WIDTH
  SET OPTION("ON SELECT")="HNDONSEL^TMGPAT6"  ;"code to call based on user input
  SET OPTION("ON CMD")="HNDONCMD^TMGPAT6"     ;"code to execute for number entry
  SET OPTION("ON KEYPRESS")="HNDONKP^TMGPAT6" ;"code to execute commands and keypresses
  IF $DATA(OPTION("COLORS"))=0 DO
  . ;"NOTE: DEMOCOLR^TMGTERM, COLORBOX^TMGTERM, ShowColorbox^TMGIDE6 can show color options.  
  . ;"Format is "FG^BG" 
  . NEW BORDER SET BORDER="14^08"  ;"WHITE FG, RED BG
  . SET OPTION("COLORS","NORM")="15^14"   ;"15=RED^14=GREY
  . SET OPTION("COLORS","HEADER")=BORDER 
  . SET OPTION("COLORS","TOP LINE")=BORDER
  . SET OPTION("COLORS","FOOTER")=BORDER
  . SET OPTION("COLORS","BOTTOM LINE")=BORDER
  ;
  NEW MSG SET MSG=$GET(OPTION("MSG"))
  IF MSG'="" DO
  . DO POPUPBOX^TMGUSRI2("Message:",MSG)
  . DO PRESS2GO^TMGUSRI2
  ;
  ;"NEW STACKCALLER SET STACKCALLER=$$CALLER^TMGIOUT2()
  NEW STACKCALLER SET STACKCALLER="" 
  NEW NODEDIV SET NODEDIV=$GET(OPTION("NODEDIV"),"/")
  SET OPTION("NODEDIV")=NODEDIV ;" in case it wasn't there initially
L0 ;
  ;"Refresh header and footer each time. 
  KILL OPTION("HEADER"),OPTION("FOOTER")
  SET OPTION("HEADER",HEADERLN)="+"_LINE_"+",HEADERLN=HEADERLN+1
  SET BANNER="--== Browse Metadata by "_DISPLAYMODE_" ==--"
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
  SET OPTION("FOOTER",1)="Enter ? for help | [F1] inspect | [F2] Toggle Mode to "_$$TOGGLEDDISPLAYMODE(.OPTION)
  DO DISPFILT^TMGIOUT2(.OPTION)  ;"SET OPTION ARRAY TO PROPERLY DISPLAY CURRENT FILTERS  
  ;"============
  NEW CURDIR SET CURDIR=$GET(OPTION("PATH"))
  ;"IF (CURDIR="")&($DATA(^TMG("TMP","SETTINGS","PATCHBROWSE",DISPLAYMODE,STACKCALLER))) DO
  ;". SET CURDIR=$GET(^TMG("TMP","SETTINGS","PATCHBROWSE",DISPLAYMODE,STACKCALLER))
  IF CURDIR="" DO
  . IF DISPLAYMODE="FILES" SET CURDIR=$$PATCHBASE^TMGKERN4() QUIT
  . IF DISPLAYMODE="PACKAGES" SET CURDIR="/"  
  SET CURDIR=$$MKTRALDV^TMGIOUTL(CURDIR,NODEDIV)
  IF $$ISDIR0(CURDIR)=0 SET CURDIR=$$PATCHBASE^TMGKERN4()
  ;
  SET TMGSELECT=""
  SET HEADERTOP=HEADERLN
L1 ;
  DO LOADDIR("SCRLFILES",CURDIR,.OPTION)
  SET HEADERDIRSTR="Current Dir: "_$$MKTRALDV^TMGIOUTL(CURDIR,NODEDIV)
  NEW ALINE SET ALINE=HEADERTOP-0.01
  FOR  SET ALINE=$ORDER(OPTION("HEADER",ALINE)) QUIT:ALINE'>0  DO
  . KILL OPTION("HEADER",ALINE)   ;"kill off options from prior loop. 
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
  ;"====================================================================
  DO SCROLLER^TMGUSRIF("SCRLFILES",.OPTION) 
  ;"NOTE: Event handler should SET TMGSELECT
  ;"     Expected format for TMGSELECT (set up by LOADDIR()) is:
  ;"         URL_path^file#^IENS 
  ;"     or  Patch_path^VER^IENS^URL_path  when dealing with patch versions
  ;"  HOWEVER: When browsing UP, towards root of directory tree, only the path may be returned.  
  ;"====================================================================
  IF $GET(OPTION("DISPLAY MODE","CHANGED"))=1 DO  GOTO L0
  . SET CURDIR=""
  . KILL OPTION("DISPLAY MODE","CHANGED")
  . SET DISPLAYMODE=$GET(OPTION("DISPLAY MODE"),"FILES")
  IF TMGSELECT="" GOTO LQ
  NEW SELENUM SET SELENUM=$SELECT(DISPLAYMODE="FILES":1,1:4)  ;"SELECTED ENTRY NUM: 1 or 4
  NEW SELECTEDENTRY
  IF DISPLAYMODE="PACKAGES" DO
  . SET SELECTEDENTRY=$PIECE(TMGSELECT,"^",4)
  . NEW TEMP SET TEMP=$PIECE(TMGSELECT,"^",1)
  . SET $PIECE(TMGSELECT,"^",1)=SELECTEDENTRY   ;"swap from piece position 4 -> 1
  . SET $PIECE(TMGSELECT,"^",4)=TEMP            ;"swap from piece position 1 -> 4
  ELSE  DO
  . SET SELECTEDENTRY=$PIECE(TMGSELECT,"^",1)
  NEW SELECTEDFILE SET SELECTEDFILE=$PIECE(TMGSELECT,"^",2)
  IF DISPLAYMODE="FILES" GOTO L3
L2 ;"FOR DISPLAY MODE: PACKAGES
  NEW ITEMDETAIL SET ITEMDETAIL=$PIECE(TMGSELECT,"^",3)
  ;"FILE = 9.4 when browsing top level package names
  IF "9.4,VER"[SELECTEDFILE SET CURDIR=ITEMDETAIL GOTO L1  ;"browse into directory for package
  ;
L3 ;"FOR DISPLAY MODE: FILES  
  NEW SELECTEDIENS SET SELECTEDIENS=$PIECE(TMGSELECT,"^",3)
  NEW ISZIP SET ISZIP=$$ISZIP0(SELECTEDENTRY)
  DO SPLITFPN^TMGIOUTL(SELECTEDENTRY,.OUTPATH,.OUTNAME,NODEDIV)
  IF SELDIR SET DONE=0 DO  GOTO:DONE LQ
  . NEW MENU,USRSLCT
  . SET MENU(0)="What DO you want to DO with this directory?"
  . SET MENU(1)="Select/Inspect "_OUTNAME_$CHAR(9)_"DONE"
  . SET MENU(2)="Browse INTO it"_$CHAR(9)_"into"
  . WRITE !
  . SET USRSLCT=$$MENU^TMGUSRI2(.MENU,2)
  . WRITE #
  . IF USRSLCT="DONE" SET DONE=1
  IF $$ISDIR0(SELECTEDENTRY) SET CURDIR=SELECTEDENTRY GOTO L1 ;"browse into directory
L5 ;
  ;"SET ^TMG("TMP","SETTINGS","PATCHBROWSE",DISPLAYMODE,STACKCALLER)=OUTPATH ;"store for future use.
LQ ;
  WRITE # ;"clear screen
  QUIT TMGSELECT
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
  ;"           OPTION("DISPLAY MODE") = "FILES" or "PACKAGES" Default is FILES
  ;"                  "FILES" --> Show files as found on web server
  ;"                  "PACKAGES" --> Show sorted entries by packages. 
  ;"       NODEDIV -- The character that separates folders (e.g. "/")
  ;"       SHOWHIDDEN -- OPTIONAL. Default=0  If 1, then show hidden files
  ;"Results: none
  NEW TMGFILES,TEMPFILES
  NEW COUNT SET COUNT=1
  KILL @PARRAY
  NEW NODEDIV SET NODEDIV=$GET(OPTION("NODEDIV"),"/")
  SET NODEDIV=$GET(NODEDIV,"/")                                            
  SET SHOWHIDDEN=+$GET(OPTION("SHOW HIDDEN"))
  NEW SELDIR SET SELDIR=+$GET(OPTION("SELECT DIR"))
  SET CURDIR=$GET(CURDIR,NODEDIV)
  SET CURDIR=$$MKTRALDV^TMGIOUTL(CURDIR,NODEDIV)
  NEW DISPLAYMODE SET DISPLAYMODE=$GET(OPTION("DISPLAY MODE"),"FILES")
  IF $$ISDIR0(CURDIR)=0 GOTO LDDN
  ;"Note: Filter/Mask would apply to directory names too, so must
  ;"      ask for list of files with mask applied **AND** also with
  ;"      a mask of '*' to be sure to get directory names  
  NEW TEMP 
  IF DISPLAYMODE="FILES" DO
  . SET TEMP=$$GETPATHDIR(CURDIR,.TMGFILES)
  ELSE  IF DISPLAYMODE="PACKAGES" DO
  . SET TEMP=$$GETPATCHDIR(CURDIR,.TMGFILES)
  NEW INDEX SET INDEX=""
  FOR  SET INDEX=$ORDER(TMGFILES(INDEX)) QUIT:(INDEX="")  DO
  . NEW VALUE SET VALUE=$GET(TMGFILES(INDEX)) QUIT:VALUE=""
  . NEW FNAME SET FNAME=$PIECE(VALUE,"^",1)
  . NEW FILE SET FILE=$PIECE(VALUE,"^",2)
  . NEW IENS SET IENS=$PIECE(VALUE,"^",3)
  . NEW FPNAME,PACKAGEURL 
  . IF DISPLAYMODE="PACKAGES",(IENS>0) DO
  . . SET PACKAGEURL=$PIECE(VALUE,"^",4)
  . . ;"SET FPNAME=$$GETURL(FILE,IENS)
  . . SET FPNAME=CURDIR_FNAME
  . ELSE  DO
  . . SET FPNAME=CURDIR_FNAME
  . . SET PACKAGEURL=""
  . IF FILE=22709.12 DO
  . . SET TEMPFILES("DIRS","<"_FNAME_">",FPNAME_"^"_FILE_"^"_IENS)=""
  . ELSE  DO
  . . SET TEMPFILES("FILES",FNAME,FPNAME_"^"_FILE_"^"_IENS_"^"_PACKAGEURL)=""
  ;  
  ;";"Now get files again with user-supplied filter
  NEW TMGMASK MERGE TMGMASK=OPTION("MATCH")
  NEW AFILE SET AFILE=""
  IF $DATA(TMGMASK)=0 GOTO LD2
  FOR  SET AFILE=$ORDER(TEMPFILES("FILES",AFILE)) QUIT:AFILE=""  DO
  . IF $$ISMATCH^TMGIOUT2(AFILE,.TMGMASK)=0 KILL TEMPFILES("FILES",AFILE)
LD2 ;
  NEW SHOWUP SET SHOWUP=1
  SET INDEX=""
  IF DISPLAYMODE="FILES",CURDIR=$$PATCHBASE^TMGKERN4() SET SHOWUP=0
  IF DISPLAYMODE="PACKAGES",CURDIR="/" SET SHOWUP=0
  IF SHOWUP DO
  . SET @PARRAY@(COUNT,".. <UP>")=$$UPPATH^TMGIOUTL(CURDIR),COUNT=COUNT+1
  SET AFILE=""
  FOR  SET AFILE=$ORDER(TEMPFILES("DIRS",AFILE)) QUIT:(AFILE="")  DO
  . NEW ARETURN SET ARETURN=""
  . FOR  SET ARETURN=$ORDER(TEMPFILES("DIRS",AFILE,ARETURN)) QUIT:ARETURN=""  DO
  . . SET @PARRAY@(COUNT,AFILE)=ARETURN,COUNT=COUNT+1
  IF SELDIR=1 GOTO LDDN  ;"skip showing files.
  SET AFILE=""
  FOR  SET AFILE=$ORDER(TEMPFILES("FILES",AFILE)) QUIT:(AFILE="")  DO
  . NEW ARETURN SET ARETURN=""
  . FOR  SET ARETURN=$ORDER(TEMPFILES("FILES",AFILE,ARETURN)) QUIT:ARETURN=""  DO
  . . SET @PARRAY@(COUNT,AFILE)=ARETURN,COUNT=COUNT+1
  ;
LDDN ;
  QUIT
  ;
GETURL(FILE,IENS) ;"Get URL for stored entry
  NEW RESULT SET RESULT=""
  NEW TMGDA DO DA^DILF(IENS,.TMGDA)
  IF FILE=22709.1 DO   ;"file TMG KIDS REMOTE DIRECTORY
  . NEW NODE0 SET NODE0=$GET(^TMG(22709.1,TMGDA,0))
  . NEW NAME SET NAME=$PIECE(NODE0,"^",1)
  . SET RESULT=NAME  
  IF FILE=22709.12 DO   ;"file DIRECTORY NAME
  . NEW NODE0 SET NODE0=$GET(^TMG(22709.1,TMGDA(1),1,TMGDA,0))
  . NEW NODE4 SET NODE4=$GET(^TMG(22709.1,TMGDA(1),1,TMGDA,4))
  . NEW NAME SET NAME=$PIECE(NODE0,"^",1)
  . NEW PARENTIENS SET PARENTIENS=$PIECE(IENS,",",2,999)
  . NEW PARENTFILE SET PARENTFILE=$$PARENTFILE^TMGDBAP3(FILE)
  . NEW PARENTURL SET PARENTURL=$$GETURL(PARENTFILE,PARENTIENS)
  . SET PARENTURL=PARENTURL_$PIECE(NODE4,"^",1)
  . SET RESULT=PARENTURL_NAME
  ELSE  IF FILE=22709.121 DO   ;"file FILE ENTRY
  . NEW NODE0 SET NODE0=$GET(^TMG(22709.1,TMGDA(2),1,TMGDA(1),1,TMGDA,0))
  . NEW NAME SET NAME=$PIECE(NODE0,"^",1)
  . NEW PARENTIENS SET PARENTIENS=$PIECE(IENS,",",2,999)
  . NEW PARENTFILE SET PARENTFILE=$$PARENTFILE^TMGDBAP3(FILE)
  . NEW PARENTURL SET PARENTURL=$$GETURL(PARENTFILE,PARENTIENS)
  . SET RESULT=PARENTURL_NAME
  ELSE  IF FILE=22709.122 DO   ;"file CHILD DIRECTORY
  . NEW NODE0 SET NODE0=$GET(^TMG(22709.1,TMGDA(2),1,TMGDA(1),2,TMGDA,0))
  . NEW NAME SET NAME=$PIECE(NODE0,"^",1)
  . NEW PARENTIENS SET PARENTIENS=$PIECE(IENS,",",2,999)
  . NEW PARENTFILE SET PARENTFILE=$$PARENTFILE^TMGDBAP3(FILE)
  . NEW PARENTURL SET PARENTURL=$$GETURL(PARENTFILE,PARENTIENS)
  . SET RESULT=PARENTURL_NAME
  QUIT RESULT
  ;
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
  ;"    NOTE: Uses CURDIR in global scope (OK if not defined)
  NEW DONE SET DONE=0
  NEW RTN SET RTN=$GET(INFO("CURRENT LINE","RETURN"))
  NEW PATH SET PATH=$PIECE(RTN,"^",1)
  NEW USRINPUT SET USRINPUT=$GET(INFO("USER INPUT"))
  NEW UPUSRINPUT SET UPUSRINPUT=$$UP^XLFSTR(USRINPUT)
  SET ^TMP("HNDONCMD^TMGIOUT2",$J)=USRINPUT
  NEW CMD SET CMD=$$UP^XLFSTR($PIECE(USRINPUT," ",1))
  NEW DISPLAYMODE SET DISPLAYMODE=$GET(OPTION("DISPLAY MODE"))
  ;"IF $EXTRACT(PATH,$LENGTH(PATH))'=NODEDIV DO
  ;". SET PATH=$$UPPATH^TMGIOUTL(PATH)  ;"Trim off filename
  IF CMD="CD" DO  GOTO:DONE HOCDN
  . NEW NEWDIR SET NEWDIR=$PIECE(USRINPUT," ",2)
  . IF NEWDIR=".." SET USRINPUT=".." QUIT
  . SET DONE=1
  . IF $EXTRACT(NEWDIR,1)'="/" SET NEWDIR=PATH_NEWDIR
  . IF $$ISDIR0(NEWDIR)=0 DO  QUIT
  . . WRITE NEWDIR," is not a valid existing directory.",!
  . . DO PRESS2GO^TMGUSRI2
  . NEW HIGHLINE SET HIGHLINE=+$GET(OPTION("BROWSE HX",NEWDIR))
  . IF HIGHLINE>0 SET OPTION("HIGHLINE")=HIGHLINE
  . ELSE  KILL OPTION("HIGHLINE")
  . SET TMGSELECT=NEWDIR
  . SET TMGSCLRMSG="^"
  IF (USRINPUT="{LEFT}")!(USRINPUT="..") DO  GOTO HOCDN
  . NEW NODEDIV SET NODEDIV=$GET(OPTION("NODEDIV"),"/") ;"extra info passed
  . ;"At this point Path is set to decend into a subdirectory or a file
  . SET PATH=$$UPPATH^TMGIOUTL(PATH,NODEDIV)
  . IF DISPLAYMODE="FILES",PATH=$$PATCHBASE^TMGKERN4 QUIT  ;"don't allow going UP from patchbase
  . ;"At this point Path is set to current directory
  . SET PATH=$$UPPATH^TMGIOUTL(PATH,NODEDIV)
  . ;"At this point PATH is set to parent directory
  . NEW HIGHLINE SET HIGHLINE=+$GET(OPTION("BROWSE HX",PATH))
  . IF HIGHLINE>0 SET OPTION("HIGHLINE")=HIGHLINE
  . ELSE  KILL OPTION("HIGHLINE")
  . SET TMGSELECT=PATH
  . IF DISPLAYMODE="PACKAGES" DO
  . . NEW PATHARR DO PATH2ARR^TMGIOUTL(.PATH,.PATHARR)
  . . NEW COUNT SET COUNT=$ORDER(PATHARR(""),-1)
  . . IF COUNT=2 SET TMGSELECT=PATH_"^VER^"_PATH
  . SET TMGSCLRMSG="^"
  IF USRINPUT="{RIGHT}" DO  GOTO HOCDN
  . IF DISPLAYMODE="FILES",$$ISDIR0(PATH)=0 QUIT
  . NEW RETURN SET RETURN=$GET(INFO("CURRENT LINE","RETURN"))
  . SET TMGSELECT=RETURN
  . SET TMGSCLRMSG="^"
  . NEW NEWDIR SET NEWDIR=$PIECE(RETURN,"^",1)
  . NEW HIGHLINE SET HIGHLINE=+$GET(OPTION("BROWSE HX",NEWDIR))
  . IF HIGHLINE>0 SET OPTION("HIGHLINE")=HIGHLINE
  . ELSE  KILL OPTION("HIGHLINE")
  . NEW DIR SET DIR=$GET(CURDIR) IF DIR="" SET DIR="/"
  . NEW SAVELINE SET SAVELINE=$GET(INFO("CURRENT LINE","NUMBER"))
  . SET OPTION("BROWSE HX",DIR)=SAVELINE
  ;"Later, I could put some stuff here to let the command line choose filters etc.
  ;"or perhaps jump to a given directory etc.  Perhaps later...
  IF USRINPUT["?" DO  GOTO HOCDN
  . DO SHOWHELP(.OPTION)
  IF UPUSRINPUT="FILTER" DO  GOTO HOCDN
  . DO EDITFILT^TMGIOUT2(.OPTION,PARRAY)
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
  IF "{LEFT},{RIGHT}"[USRINPUT DO HNDONCMD(PARRAY,.OPTION,.INFO)
  IF USRINPUT="{F1}" DO
  . SET SELDIR=1  ;"USED IN GLOBAL SCOPE
  . DO HNDONSEL(.PARRAY,.OPTION,.INFO) 
  IF USRINPUT="{F2}" DO
  . SET OPTION("DISPLAY MODE")=$$TOGGLEDDISPLAYMODE(.OPTION)
  . SET OPTION("DISPLAY MODE","CHANGED")=1
  . SET TMGSCLRMSG="^"
  QUIT
  ;  
TOGGLEDDISPLAYMODE(OPTION) ;" Return toggle value for display mode
  NEW RESULT
  NEW MODE SET MODE=$GET(OPTION("DISPLAY MODE"))
  NEW RESULT SET RESULT=$SELECT(MODE="FILES":"PACKAGES",1:"FILES")
  QUIT RESULT
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
  ;"WRITE "type: 'MKDIR <DirName>' to create a NEW directory",!
  ;"WRITE "type: 'RMDIR <DirName>' to DELETE a NEW directory",!
  WRITE "type: 'FILTER' to edit file name filter(s)",!
  DO PRESS2GO^TMGUSRI2
  WRITE #
  QUIT
  ;  