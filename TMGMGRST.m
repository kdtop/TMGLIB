TMGMGRST ;TMG/kst/Custom version of ZTMGRSET and ZOSFGUX ;03/25/06, 2/2/14, 3/1/15
         ;;1.0;TMG-LIB;**1**;11/01/04

 ;"ZTMGRSET(INFO) & ZOSFGUX  -- NON-INTERACTIVE versions of standard code.
 ;"=============================================================================
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"Purpose:
 ;"
 ;"This library will provide optional NON-INTERACTIVE versions of standard code.
 ;"
 ;"ZTMGRSET(INFO)
 ;"ZOSFGUX
 ;"
 ;"Dependancies:
 ;"  TMGQIO
 ;"  IF TMGDEBUG defined, then requires TMGDEBUG.m
 ;"=============================================================================

ZTMGRSET(INFO) ;SF/RWF,PUG/TOAD - SET UP THE MGR ACCOUNT FOR THE SYSTEM ;10/29/2003  10:19
 ;;8.0+;KERNEL;**34,36,69,94,121,127,136,191,275 (WorldVista Modified)**;JUL 10, 1995;
 ;";;8.0;KERNEL;**34,36,69,94,121,127,136,191,275**;JUL 10, 1995;
 ;"
 ;"K. Toppenberg's changes made November, 2004
 ;"
 ;"Input:
 ;"     Note: INFO variable is completely an OPTIONAL parameter.
 ;"                If not supplied, interactive mode used
 ;"        INFO("SILENT-OUTPUT") -- 1 = output is supressed.
 ;"        INFO("SILENT-INPUT") -- 1 = User-interactive input is supressed.
 ;"
 ;"        ** IF in SILENT-INPUT mode, THEN the following data should be supplied:
 ;"     ----------------------
 ;"        INFO("CONTINUE") -- Should contain the answer the user would enter for question:
 ;"                "THIS MAY NOT BE THE MANAGER UCI... continue anyway?"  (i.e. Y or N)
 ;"        INFO("OS") -- should have number that would be used to select OS to install (i.e. 1,2,3 etc.)
 ;"        INFO("RENAME") -- should have answer to "Rename fileman routines?" (i.e. Y or N)
 ;"        INFO("MGR-UCI,VOL") -- should have Managers UCI,VOL
 ;"        INFO("SIGNON-UCI,VOL") -- should have Sign-on UCI,VOL
 ;"        INFO("VOLUME-SET")--should have: NAME OF VOLUME SET (use same volume SET as for 'Production')
 ;"        INFO("TEMP") -- should have temp directory for system
 ;"Output:
 ;"        If in SILENT-OUTPUT mode, then output that would normally go to the screen, will be routed to this array
 ;"        NOTE: INFO SHOULD BE PASSED BY REFERENCE IF user wants this information passed back out.
 ;"        INFO("TEXT","LINES")=Number of output lines
 ;"        INFO("TEXT",1)= 1st output line
 ;"        INFO("TEXT",2)= 2nd output line, etc...
 ;
 ;

 IF '$DATA(TMGDBINDENT) NEW TMGDBINDENT SET TMGDBINDENT=0

 N %D,%S,I,OSMAX,U,X,X1,X2,Y,Z1,Z2,ZTOS,ZTMODE,SCR
 NEW ABORT SET ABORT=0  ;//kt
 NEW SILNTOUT SET SILNTOUT=$GET(INFO("SILENT-OUTPUT"),0) ;//kt
 NEW SILENTIN SET SILENTIN=$GET(INFO("SILENT-INPUT"),0) ;//KT
 KILL INFO("TEXT") ;//kt

 S ZTMODE=0
A
 DO OUTP^TMGQIO(SILNTOUT,"!","!","ZTMGRSET","!","Version ",$P($T(ZTMGRSET+1),";",3)," ",$P($T(ZTMGRSET+1),";",5))
 DO OUTP^TMGQIO(SILNTOUT,"!","!","HELLO! I'm here to help initialize the current account.")

 ;
 SET Y=0 ;//kt added
 I $D(^%ZOSF("UCI")) X ^%ZOSF("UCI")
 NEW CurUCI SET CurUCI=Y
 I CurUCI'["MG" DO  QUIT:(ABORT=1)
 . WRITE !,!,"CurUCI=",CurUCI,!
 . DO OUTP^TMGQIO(SILNTOUT,$C(7),"!","!","THIS MAY NOT BE THE MANAGER UCI.","!")
 . DO OUTP^TMGQIO(SILNTOUT," I think it is ",CurUCI,". Should I continue anyway? N//")
 . DO INP^TMGQIO(.X,SILENTIN,120,$GET(INFO("CONTINUE")))
 . IF "Yy"'[$E(X_"N") DO OUTP^TMGQIO(SILNTOUT,"QUITING.","!") SET ABORT=1 QUIT
 ;
 S ZTOS=$$OS()
 I ZTOS'>0 DO OUTP^TMGQIO(SILNTOUT,"!","Can't determine the OS type. Exiting ZTMGRSET.") QUIT
 ;
 I ZTMODE D  QUIT:(ABORT=1)
 . DO OUTP^TMGQIO(SILNTOUT,"!","!","Patch number to load: ")
 . DO INP^TMGQIO(.PCNM,SILENTIN,,$GET(INFO("PATCHNUM")))
 . IF (PCNM<1)!(PCNM>999) DO  QUIT
 . . DO OUTP^TMGQIO(SILNTOUT,"!","!","Need a Patch number to load. Exiting ZTMGRSET")
 . . SET ABORT=1
 . S SCR="I $P($T(+2^@X),"";"",5)?.E1P1"_$C(34)_PCNM_$C(34)_"1P.E"
 ;
 ;
 K ^%ZOSF("MASTER"),^("SIGNOFF") ;Remove old nodes.
 ;
DOIT
 DO OUTP^TMGQIO(SILNTOUT,"!","!","I will now rename a group of routines specific to your operating system.","!")
 D @ZTOS
 D ALL
 D GLOBALS:'ZTMODE
 ;
 DO OUTP^TMGQIO(SILNTOUT,"!","!","Completed ZTMGRSET^TMGMGRST.","!","So I guess this is 'Goodbye'.","!","!")
 ;
 Q
 ;
 ;==============================================================================================
 ;==============================================================================================
 ;
RELOAD ;Reload any patched routines
 N %D,%S,I,OSMAX,U,X,X1,X2,Y,Z1,Z2,ZTOS,ZTMODE,SCR
 S ZTMODE=1 G A
 Q
 ;
 ;==============================================================================================
 ;==============================================================================================
 ;
OS() ;Select the OS
 N Y,X1,X
 S U="^",SCR="I 1" F I=1:1:20 S X=$T(@I) Q:X=""  S OSMAX=I
B
 S Y=0,ZTOS=0 I $D(^%ZOSF("OS")) D
 . S X1=$P(^%ZOSF("OS"),U),ZTOS=$$OSNUM
 . DO OUTP^TMGQIO(SILNTOUT,"!","I think you are using ",X1)
 DO OUTP^TMGQIO(SILNTOUT,"!","Which MUMPS system should I install?","!")
 DO OUTP^TMGQIO(SILNTOUT,"!",0," = Abort;")
 F I=1:1:OSMAX DO OUTP^TMGQIO(SILNTOUT,"!",I," = ",$P($T(@I),";",3))
 DO OUTP^TMGQIO(SILNTOUT,"!","System: ")
 IF ZTOS DO OUTP^TMGQIO(SILNTOUT,ZTOS,"//")
 DO INP^TMGQIO(.X,SILENTIN,300,$GET(INFO("OS"),U))
 IF X="" S X=ZTOS
 IF (X=U)!(X=0) DO OUTP^TMGQIO(SILNTOUT,"!") SET X=0 GOTO OSQ
 I X<1!(X>OSMAX) DO OUTP^TMGQIO(SILNTOUT,"!","NOT A VALID OS CHOICE") GOTO B
OSQ
 QUIT X
 ;

OSNUM() ;Return the OS number
 N I,X1,X2,Y S Y=0,X1=$P($G(^%ZOSF("OS")),"^")
 F I=1:1 S X2=$T(@I) Q:X2=""  I X2[X1 S Y=I QUIT
 QUIT Y
 ;

ALL
 DO OUTP^TMGQIO(SILNTOUT,"!","!","Now to load routines common to all systems.")
 D TM,ETRAP,DEV,OTHER,FM
 I ZTOS=7!(ZTOS=8) D
 . S ^%ZE="D ^ZE"
 E  D  ;With ZLoad, ZSave, ZInsert
 . DO OUTP^TMGQIO(SILNTOUT,"!","Installing ^%Z editor")
 . D ^ZTEDIT
 I 'ZTMODE DO
 . DO OUTP^TMGQIO(SILNTOUT,"!","Setting ^%ZIS('C')")
 . K ^%ZIS("C")
 . S ^%ZIS("C")="G ^%ZISC"
 Q
 ;

TM ;Taskman
 S %S="ZTLOAD^ZTLOAD1^ZTLOAD2^ZTLOAD3^ZTLOAD4^ZTLOAD5^ZTLOAD6^ZTLOAD7"
 S %D="%ZTLOAD^%ZTLOAD1^%ZTLOAD2^%ZTLOAD3^%ZTLOAD4^%ZTLOAD5^%ZTLOAD6^%ZTLOAD7"
 D MOVE
 S %S="ZTM^ZTM0^ZTM1^ZTM2^ZTM3^ZTM4^ZTM5^ZTM6"
 S %D="%ZTM^%ZTM0^%ZTM1^%ZTM2^%ZTM3^%ZTM4^%ZTM5^%ZTM6"
 D MOVE
 S %S="ZTMS^ZTMS0^ZTMS1^ZTMS2^ZTMS3^ZTMS4^ZTMS5^ZTMS7^ZTMSH"
 ;I ZTOS=7!(ZTOS=8) S $P(%S,U,1)="ZTMSGTM"
 S %D="%ZTMS^%ZTMS0^%ZTMS1^%ZTMS2^%ZTMS3^%ZTMS4^%ZTMS5^%ZTMS7^%ZTMSH"
 D MOVE
 Q

FM ;Rename the FileMan routines
 I ZTMODE=1 GOTO FMQ  ;"Only ask on full install
 DO INP^TMGQIO(.X,SILENTIN,600,$GET(INFO("RENAME"),"N"),"!","!","Want to rename the FileMan routines: No//")
 GOTO:"Yy"'[$E(X_"N") FMQ
 S %S="DIDT^DIDTC^DIRCR",%D="%DT^%DTC^%RCR"
 D MOVE
FMQ
 QUIT
 ;
 ;
ETRAP ;Error Trap
 S %S="ZTER^ZTER1",%D="%ZTER^%ZTER1"
 D MOVE
 Q
 ;
 ;
OTHER
 S %S="ZTPP^ZTP1^ZTPTCH^ZTRDEL^ZTMOVE"
 S %D="%ZTPP^%ZTP1^%ZTPTCH^%ZTRDEL^%ZTMOVE"
 D MOVE
 Q
 ;
 ;
DEV
 S %S="ZIS^ZIS1^ZIS2^ZIS3^ZIS5^ZIS6^ZIS7^ZISC^ZISP^ZISS^ZISS1^ZISS2^ZISTCP^ZISUTL"
 S %D="%ZIS^%ZIS1^%ZIS2^%ZIS3^%ZIS5^%ZIS6^%ZIS7^%ZISC^%ZISP^%ZISS^%ZISS1^%ZISS2^%ZISTCP^%ZISUTL"
 D MOVE
 Q
 ;
 ;
RUM ;Build the routines for Capacity Management (CM)
 S %S=""
 I ZTOS=1 S %S="ZOSVKRV^ZOSVKSVE^ZOSVKSVS^ZOSVKSD" ;DSM
 I ZTOS=2 S %S="ZOSVKRM^ZOSVKSME^ZOSVKSMS^ZOSVKSD" ;MSM
 I ZTOS=3 S %S="ZOSVKRO^ZOSVKSOE^ZOSVKSOS^ZOSVKSD" ;OpenM
 I ZTOS=7!(ZTOS=8) S %S="ZOSVKRG^ZOSVKSGE^ZOSVKSGS^ZOSVKSD" ;GT.M
 S %D="%ZOSVKR^%ZOSVKSE^%ZOSVKSS^%ZOSVKSD"
 D MOVE
 Q
 ;
 ;
ZOSF(X) ;
 ;"Note: KT made change to this function.  It used to be that it would be
 ;"        called as DO ZOSF("FUNCTION").  Now it should be called like this:
 ;"        ZOSF("^FUNCTION").  The old fuction would automatically prefix
 ;"        all calls with a '^'.  I took this out so that calls to functions
 ;"        contained in this module are possible.
 ;
 X SCR
 I $T DO @(X)
 Q
 ;
 ;
1 ;;VAX DSM(V6), VAX DSM(V7)
 S %S="ZOSVVXD^ZTBKCVXD^ZIS4VXD^ZISFVXD^ZISHVXD^XUCIVXD^ZISETVXD"
 D DES,MOVE
 S %S="ZOSV2VXD^ZTMDCL",%D="%ZOSV2^%ZTMDCL"
 D MOVE,RUM,ZOSF("^ZOSFVXD")
 Q
 ;
 ;
2 ;;MSM-PC/PLUS, MSM for NT or UNIX
 DO OUTP^TMGQIO(SILNTOUT,"!","- Use autostart to DO ZTMB don't resave as STUSER.")
 S %S="ZOSVMSM^ZTBKCMSM^ZIS4MSM^ZISFMSM^ZISHMSM^XUCIMSM^ZISETMSM"
 D DES,MOVE
 S %S="ZOSV2MSM",%D="%ZOSV2"
 D MOVE,RUM,ZOSF("^ZOSFMSM")
 I $$VERSION^%ZOSV(1)["UNIX" S %S="ZISHMSU",%D="%ZISH" D MOVE
 Q
 ;
 ;
3 ;;OpenM for NT, Cache/NT, Cache/VMS
 S %S="ZOSVONT^^ZIS4ONT^ZISFONT^ZISHONT^XUCIONT"
 D DES,MOVE
 S %S="ZISTCPS",%D="%ZISTCPS"
 D MOVE,RUM,ZOSF("^ZOSFONT")
 Q
 ;
 ;
4 ;;Datatree, DTM-PC, DT-MAX
 S %S="ZOSVDTM^ZTBKCDTM^ZIS4DTM^ZISFDTM^ZISHDTM^XUCIDTM^ZISETDTM"
 D DES,MOVE
 S %S="ZOSV1DTM^ZTMB",%D="%ZOSV1^%ustart"
 D MOVE,ZOSF("^ZOSFDTM")
 Q
 ;
 ;
5 ;;MVX,ISM VAX
 S %S="ZOSVMSQ^ZTBKCMSQ^ZIS4MSQ^ZISFMSQ^ZISHMSQ^XUCIMSQ^ZISETMSQ"
 D DES,MOVE
 S %S="ZTMB",%D="ZSTU"
 D MOVE,ZOSF("^ZOSFMSQ")
 Q
 ;
 ;
6 ;;ISM (UNIX, Open VMS)
 S %S="ZOSVIS2^^ZIS4IS2^ZISFIS2^ZISHIS2^XUCIIS2^ZISETIS2"
 D DES,MOVE
 S %S="ZTMB",%D="ZSTU"
 D MOVE,ZOSF("^ZOSFIS2")
 Q
 ;
 ;
7 ;;GT.M (VMS)
 S %S="ZOSVGTM^ZTBKCGTM^ZIS4GTM^ZISFGTM^ZISHGTM^XUCIGTM^ZISETGTM"
 D DES,MOVE
 S %S="ZOSV2GTM^ZISTCPS",%D="%ZOSV2^%ZISTCPS"
 D MOVE,ZOSF("^ZOSFGTM")
 Q
 ;
 ;
8 ;;GT.M (Unix)
 S %S="ZOSVGUX^ZTBKCGUX^ZIS4GTM^ZISFGTM^ZISHGUX^XUCIGTM^ZISETGUX"
 ;S %S="ZOSVGUX^ZIS4GTM^ZISFGTM^ZISHGUX^XUCIGTM"  ;//kt removed 2 files that were missing
 D DES
 D MOVE
 S %S="ZOSV2GTM^ZISTCPS",%D="%ZOSV2^%ZISTCPS"
 D MOVE
 D ZOSF("ZOSFGUX")
 Q
 ;
 ;
10 ;;NOT SUPPORTED
 Q
 ;
 ;
MOVE ; rename % routines
 N %,X,Y
 F %=1:1:$L(%D,"^") D
 . S X=$P(%S,U,%) ; from
 . S Y=$P(%D,U,%) ; to
 . DO OUTP^TMGQIO(SILNTOUT,"!","Routine: ",X)
 . NEW INDENT SET INDENT=12-$LENGTH(X)
 . IF INDENT>0 DO OUTP^TMGQIO(SILNTOUT,"?"_INDENT)
 . DO OUTP^TMGQIO(SILNTOUT," --> ",Y)
 . SET INDENT=12-$LENGTH(Y)
 . DO OUTP^TMGQIO(SILNTOUT,"?"_INDENT)
 . Q:(X="")!(Y="")
 . I $TEXT(^@X)="" DO  QUIT
 . . DO OUTP^TMGQIO(SILNTOUT,"Missing")
 . X SCR
 . Q:'$T
 . IF $$COPY(X,Y)=0 DO
 . . DO OUTP^TMGQIO(SILNTOUT,"Loaded")
 . . ;"DO OUTP^TMGQIO(SILNTOUT,"?10","Saved as ",Y)
 . ELSE  DO
 . . DO OUTP^TMGQIO(SILNTOUT,"Missing (Failed Copy)")
 QUIT
 ;
 ;
COPY(FROM,TO) ;
 ;"Purpose: To copy file FROM to TO, getting directory path from $ZRO
 ;"Input: FROM-- a filename without path or '.m' extension
 ;"       TO-- a filename without path or '.m' extension
 ;"Result: 0: no error  1=error
 ;
 NEW RESULT SET RESULT=0
 I ZTOS'=7,ZTOS'=8 DO  GOTO CPQ
 . X "ZL @FROM ZS @TO"
 ;
 ;"For GT.M below
 ;"--------------
 ;
 N PATH,COPY
 SET FROM=$GET(FROM)_".m"
 SET TO=$TR($GET(TO),"%","_")_".m"
 S PATH=$$GETPATH(.FROM)
 IF PATH="" SET RESULT=1 GOTO CPQ  ;"QUIT 1
 IF $EXTRACT(PATH,$LENGTH(PATH))'="/" SET PATH=PATH_"/" ;"Ensure path ends in '/'.
 S COPY=$S(ZTOS=7:"COPY",1:"cp")
 ZSYSTEM COPY_" "_PATH_FROM_" "_PATH_TO
 SET RESULT=$ZSYSTEM
 ;
 ;
 ;
CPQ
 QUIT RESULT
 ;
GETPATH(FILE)
 ;"Note: This function is for GTM, which has a path sequence that may be searched for files.
 ;"Purpose: To take file, and look through file path to determine which path the file
 ;"        exists in.
 ;"        e.g. IF $ZRO="ObjDir1(SourceDir1 SourceDir2) ObjDir2(SourceDir3 SourceDir4)"
 ;"          then this function will look in SourceDir's 1..4 to see which one contains
 ;"          FILE.  Functions will return the appropriate SourceDir
 ;"Input:FILE: the filename to look for, with extension.  e.g. "XUP.m"
 ;"Result: Will return the source directory, e.g. /usr/local/OpenVistA/r
 ;
 ;"NOTE: see also RTNPATH^TMGKERN1 -- NEWER, TIGHTER CODE
 NEW LASTDIR SET LASTDIR=""
 NEW RESULT SET RESULT=""
 NEW PATH SET PATH=""
 ;
 FOR  DO  QUIT:(RESULT'="")!(LASTDIR="")
 . SET LASTDIR=$$R(LASTDIR)
 . IF LASTDIR="" QUIT
 . ;"DO OUTP^TMGQIO(SILNTOUT,"!","Looking in: ",LASTDIR)
 . SET PATH=LASTDIR
 . IF $$FEXISTS(PATH,FILE) DO
 . . SET RESULT=PATH
 ;
 QUIT RESULT
 ;
 ;
R(LASTDIR) ; routine directory for GT.M
 ;"Notice: The comments here only apply to GTM for Linux (#8).
 ;"                I don't have details about GT.M for VMS (#7) so I have not implemented
 ;"                cyclic directory evaluation.  LASTDIR will be ignored.
 ;"INPUT: LASTDIR - OPTIONAL.  This is the directory returned last time fuction called, to
 ;"                allow for cycling through all possible directories.
 ;"NOTE: The Syntax for $ZRO is as follows:
 ;"        ObjectDir1(SourceDir1) ObjectDir2(SourceDir1 SourceDir2 ...) ObjectDir3() ObjectDir4
 ;"        This shows elements are separated by spaces.
 ;"        Note that each element starts with the directory for .o files
 ;"        Each object directory has an optional (SourceDir) immediately following it
 ;"                IF (Dir) is present, it contains one or more source directories (separated by spaces)
 ;"                IF () is empty (i.e. "()") then no source directory is available.
 ;"                IF (Dir) is absent (i.e. ""), then object dir is used to search for source .m files
 ;"Result: will return the next directory, or "" IF none.
 ;"
 NEW RESULT SET RESULT=""
 SET LASTDIR=$GET(LASTDIR)
 ;"if LASTDIR'="" W "Will look for dir AFTER ",LASTDIR,!
 I ZTOS=7 DO
 . SET RESULT=$P($ZRO,",",1)
 IF ZTOS=8 DO  ;"GT.M for Linux
 . NEW SECTION
 . NEW PRIORFND SET PRIORFND=0
 . NEW ELEMENT SET ELEMENT=" "
 . NEW DIVPTS ;"Array to hold cut points of $ZRO. Setup in GETSECTN
 . SET DIVPTS("MAX")=0
 . FOR SECTION=1:1 DO  QUIT:(RESULT'="")!(SECTION>DIVPTS("MAX")+1)
 . . SET ELEMENT=$$GETSECTN($ZRO,SECTION,.DIVPTS) ;"gets 'ObjDir(SrceDir1 SrceDir2 ...)' etc.
 . . NEW SOURCES SET SOURCES=""
 . . IF (ELEMENT["(")&(ELEMENT[")") DO
 . . . SET SOURCES=$PIECE(ELEMENT,"(",2)
 . . . SET SOURCES=$PIECE(SOURCES,")",1) ;"Get just (..) part -- the source file paths.
 . . ELSE  DO
 . . . SET SOURCES=ELEMENT  ;"i.e. for ObjectDir [i.e. not ObjectDir()] format.
 . . IF (ELEMENT="")!(SOURCES="") QUIT
 . . NEW PART
 . . NEW PATH SET PATH=" "
 . . FOR PART=1:1 DO  QUIT:(RESULT'="")!(PATH="")
 . . . SET PATH=$PIECE(SOURCES," ",PART) ;"returns 'SourceDir1' etc.
 . . . IF PATH="" QUIT
 . . . IF (LASTDIR="")!(PRIORFND) SET RESULT=PATH
 . . . ELSE  IF PATH=LASTDIR SET PRIORFND=1
 ;
 ;"OLDER CODE
 ;". NEW temp
 ;". SET temp=$ZRO
 ;". IF $ZRO["(" DO
 ;". SET temp=$P($ZRO,"(",2)
 ;". SET temp=$P(temp,")",1)
 ;". SET RESULT=$P(temp," ",1)_"/"
 ;
 QUIT RESULT
 ;
 ;
GETSECTN(S,NUM,DIVPTS)
 ;"Purpose: To parse a string as follows:
 ;"        Expected format of S:
 ;"     ObjectDir(SourceDir1 SourceDir2 ...) ObjectDir2(SourceDir1 SourceDir2 ...) ...
 ;"  or ObjectDir ObjectDir2(SourceDir1 SourceDir2 ...) ObjectDir() ...  etc.
 ;"        --- so major sections are divided by spaces, with optional () with optional contents.
 ;"        --- there is no nesting of parentheses.
 ;"        If NUM=1, return ObjectDir(SourceDir1 SourceDir2 ...)
 ;"        If NUM=2, return ObjectDir2(SourceDir1 SourceDir2 ...)  etc.
 ;"        Notice: Spaces in ObjectDir name are NOT SUPPORTED
 ;"        Notice: If more than one space separates sections, will be treated as extra section
 ;"INPUT: S -- string as above
 ;"        NUM -- the section number to get (1..n)
 ;"        DIVPTS -- [OPTIONAL] PASS BY REFERENCE.  If empty, then will be filled
 ;"                with the indexes of the dividing spaces
 ;"                        e.g. DIVPTS(1)=12  DIVPTS(2)=25  DIVPTS(3)=41  DIVPTS("MAX")=3
 ;"                If not empty, then this will be used return the requested section.
 ;
 NEW RESULT SET RESULT=""
 NEW START SET START=0
 NEW END SET END=9999
 NEW PTIDX SET PTIDX=0
 NEW SECTION SET SECTION=0
 NEW MAXIDX
 ;
 SET S=$GET(S)
 SET NUM=$GET(NUM,0)
 ;
 ;Fill Array of division points IF empty
 IF $DATA(DIVPTS)'=11 DO
 . NEW INPAREN SET INPAREN=0
 . NEW I,CH
 . FOR I=1:1:$LENGTH(S) DO
 . . SET CH=$EXTRACT(S,I)
 . . IF CH="(" SET INPAREN=1 QUIT
 . . IF CH=")" SET INPAREN=0 QUIT
 . . IF (CH=" ")&(INPAREN=0) DO
 . . . SET PTIDX=PTIDX+1
 . . . SET DIVPTS(PTIDX)=I
 . . . SET DIVPTS("MAX")=PTIDX
 ;
 IF (NUM>0)&(NUM'>DIVPTS("MAX")+1) DO
 . SET PTIDX=$ORDER(DIVPTS(0))
 . ;"  1      2     3      <-- Section #'2
 . ;"xxxxx xxxxxx xxxxx    <-- sample S
 . ;"     ^      ^         <-- DIVPTS 1 & 2
 . IF NUM>1 SET START=DIVPTS(NUM-1)+1   ;"default START=0
 . IF NUM'>DIVPTS("MAX") SET END=DIVPTS(NUM)-1 ;"default END=9999
 . SET RESULT=$EXTRACT(S,START,END)
 ;
 QUIT RESULT
 ;
 ;
FEXISTS(PATH,FNAME)
 ;"Purpose: To determine IF file FNAME exists on HFS
 ;"Input: PATH: full path up to, but not including, filename. e.g. '/home/user/'
 ;"          FNAME: name of the file to open.  e.g. 'myfile.txt'
 ;"Result: 1=file exists, 0=file doesn't exist
 NEW RESULT SET RESULT=0
 IF ($DATA(PATH)'=0)!($DATA(FNAME)'=0) DO
 . NEW HANDLE SET HANDLE=""
 . DO OPEN^%ZISH(HANDLE,PATH,FNAME,"R") ;"Try to access file
 . IF POP=0 DO  ;"POP=0 means file opened, ergo file exists.
 . . SET RESULT=1
 . . DO CLOSE^%ZISH(HANDLE) ;"close file... we don't need it.
 QUIT RESULT
 ;
 ;
SPLITF(IN,PATH,FNAME,NODEDIV)
 ;"Purpose: To take a string with path and filename and
 ;"        cleave into a path string and a filename string
 ;"Input: IN: Initial string to parse.  e.g. /home/user1/somefile.txt
 ;"          PATH & FNAME: vars SHOULD BE PASSED BY REFERENCE -- to take out results
 ;"          The character used to divide nodes, e.g. '/' OPTIONAL .. defaults to '/'
 ;"Output:PATH: the path part of IN, e.g. '/home/user1/'
 ;"          FNAME: the filename part of IN, e.g. 'somefile.txt'
 SET NODEDIV=$GET(NODEDIV,"/")
 SET PATH=$GET(PATH)
 SET FNAME=$GET(IN)
 NEW DONE SET DONE=0
 FOR  DO  QUIT:(DONE=1)
 . IF FNAME[NODEDIV DO
 . . SET PATH=PATH_$PIECE(FNAME,NODEDIV,1)_NODEDIV
 . . SET FNAME=$PIECE(FNAME,NODEDIV,2,256)
 . ELSE  SET DONE=1
 QUIT
 ;
 ;
DES
 S %D="%ZOSV^%ZTBKC1^%ZIS4^%ZISF^%ZISH^%XUCI^ZISETUP"
 Q
 ;
 ;
GLOBALS ;Set node zero of file #3.05 & #3.07
 DO OUTP^TMGQIO(SILNTOUT,"!","!","Now, I will check your % globals.")
 DO OUTP^TMGQIO(SILNTOUT,"..........")
 F %="^%ZIS","^%ZISL","^%ZTER","^%ZUA" S:'$D(@%) @%=""
 S:$D(^%ZTSK(0))[0 ^%ZTSK(-1)=100,^%ZTSCH=""
 S Z1=$G(^%ZTSK(-1),-1),Z2=$G(^%ZTSK(0))
 I Z1'=$P(Z2,"^",3) S:Z1'>0 ^%ZTSK(-1)=+Z2 S ^%ZTSK(0)="TASK'S^14.4^"_^%ZTSK(-1)
 S:$D(^%ZUA(3.05,0))[0 ^%ZUA(3.05,0)="FAILED ACCESS ATTEMPTS LOG^3.05^^"
 S:$D(^%ZUA(3.07,0))[0 ^%ZUA(3.07,0)="PROGRAMMER MODE LOG^3.07^^"
 DO OUTP^TMGQIO(SILNTOUT,"... Done")
 Q
 ;
 ;
NAME() ;Setup the static names for this system
 ;"Input -- none
 ;"Result -- 0=normal exit  1=error
 ;
 ;"WRITE "IN CUSTOM NAME FUNCTION",!
 ;
 NEW RETRY SET RETRY=0
 NEW ABORT SET ABORT=0
 NEW RESULT SET RESULT=1
 ;
MGR

 IF ABORT=1 GOTO NMQ
 SET RETRY=0
 DO OUTP^TMGQIO(SILNTOUT,"!","!","ENTER NAME OF MANAGER'S UCI,VOLUME SET: "_^%ZOSF("MGR")_"// ")
 DO INP^TMGQIO(.X,SILENTIN,$G(DTIME,9999),$GET(INFO("MGR-UCI,VOL")))
 IF X="" SET X=^%ZOSF("MGR")
 IF X="^" DO OUTP^TMGQIO(SILNTOUT,"!","SKIPPING...") GOTO NMQ
 I X]"" DO  IF (RETRY=1) GOTO MGR
 . X ^("UCICHECK")
 . IF 0[Y DO
 . . SET RETRY=1
 . . IF SILENTIN=1 DO
 . . . DO OUTP^TMGQIO(SILNTOUT,"!","Invalid Manager's UCI,VOLUME SET")
 . . . SET ABORT=1
 S ^%ZOSF("MGR")=X
 ;
 ;
PROD
 IF ABORT=1 GOTO NMQ
 SET RETRY=0
 DO OUTP^TMGQIO(SILNTOUT,"!","ENTER PRODUCTION (SIGN-ON) UCI,VOLUME SET: "_^%ZOSF("PROD")_"// ")
 DO INP^TMGQIO(.X,SILENTIN,$S($G(DTIME):DTIME,1:9999),$GET(INFO("SIGNON-UCI,VOL")))
 IF X="" SET X=^%ZOSF("PROD")
 IF X="^" DO OUTP^TMGQIO(SILNTOUT,"!","SKIPPING...") GOTO NMQ
 I X]"" DO  IF (RETRY=1) GOTO PROD
 . X ^("UCICHECK")
 . IF 0[Y DO
 . . DO OUTP^TMGQIO(SILNTOUT,"!","Invalid Sign-On UCI,VOLUME SET","!")
 . . SET RETRY=1
 . . IF SILENTIN=1 SET ABORT=1
 S ^%ZOSF("PROD")=X
 ;
 ;
VOL
 IF ABORT=1 GOTO NMQ
 SET RETRY=0
 DO OUTP^TMGQIO(SILNTOUT,"!","ENTER NAME OF VOLUME SET (use same volume SET as for 'Production'): "_^%ZOSF("VOL")_"//")
 DO INP^TMGQIO(.X,SILENTIN,$G(DTIME,9999),$GET(INFO("VOLUME-SET")))
 IF X="" SET X=^%ZOSF("VOL")
 IF X="^" DO OUTP^TMGQIO(SILNTOUT,"!","SKIPPING...") GOTO NMQ
 I X]"" DO  IF (RETRY=1) GOTO VOL
 . IF (X'?3U)!(^%ZOSF("PROD")'[X) DO
 . . DO OUTP^TMGQIO(SILNTOUT,"MUST be 3 upper-case letters.")
 . . DO OUTP^TMGQIO(SILNTOUT,"Also, MUST be same Volume Set entered above.")
 . . SET RETRY=1
 . . IF SILENTIN=1 DO
 . . . DO OUTP^TMGQIO(SILNTOUT,"!","Invalid VOLUME SET")
 . . . SET ABORT=1
 SET ^%ZOSF("VOL")=X
 ;
 ;
 ;"KT copied/modified TMP section from ZOSFGUX (GT.M/Linux specific)
TMP ;Get the temp directory
 IF ABORT=1 GOTO NMQ
 IF $GET(ZTOS)=8 DO  GOTO TMP:(RETRY=1)
 . DO OUTP^TMGQIO(SILNTOUT,"!","Enter the temp directory for the system: '"_^%ZOSF("TMP")_"'//")
 . DO INP^TMGQIO(.X,SILENTIN,$S($G(DTIME):DTIME,1:9999),$GET(INFO("TEMP")))
 . IF X="" SET X=^%ZOSF("TMP")
 . IF SILENTIN=0 SET ABORT=1 QUIT
 . ELSE  DO  QUIT:(RETRY=1)!(ABORT=1)
 . . IF X="" SET ABORT=1 DO OUTP^TMGQIO(SILNTOUT,"SKIPPING...") QUIT
 . . IF X'?1"/".E SET RETRY=1 QUIT
 . S ^%ZOSF("TMP")=X
 . DO OUTP^TMGQIO(SILNTOUT,"!","^%ZOSF setup")

 DO OUTP^TMGQIO(SILNTOUT,"!")
 SET RESULT=0

NMQ
 QUIT RESULT
 ;
 ;
 ;"=====================================================================================
 ;"=====================================================================================
 ;"=====================================================================================
 ;"Note: ZOSFGUX used to be a separate file.  I included it here for modification.

ZOSFGUX ;SFISC/MVB,PUG/TOAD - ZOSF Table for GT.M for Unix ;10 Feb 2003 6:37 pm
 ;;8.0;KERNEL;**275**;Jul 10, 1995
 ;; for GT.M for Unix, version 4.3
 ;
 S %Y=1
 S DTIME=$G(DTIME,600)
 K ^%ZOSF("MASTER"),^%ZOSF("SIGNOFF")
 I $GET(^%ZOSF("VOL"))="" S ^%ZOSF("VOL")="ROU"
 ;"I '$D(^%ZOSF("VOL")) S ^%ZOSF("VOL")="ROU"
 K ZO
 F I="MGR","PROD","VOL","TMP" DO
 . IF $D(^%ZOSF(I)) SET ZO(I)=^%ZOSF(I)
 F I=1:2 DO  QUIT:Z=""
 . S Z=$P($TEXT(Z+I),";;",2)
 . Q:Z=""
 . S X=$P($TEXT(Z+1+I),";;",2,99)
 . IF Z="OS" S $P(^%ZOSF(Z),"^")=X
 . IF Z'="OS" S ^%ZOSF(Z)=$S($D(ZO(Z)):ZO(Z),1:X)
 ;
OS2 ;"was OS when this was a separate file.
 S ^%ZOSF("OS")="GT.M (Unix)^19"
 ;
 ;
 ;"I (KT) found the original code for Prod,Vol etc to be same as the NAME function in ZTMGRSET, so
 ;"  I'll just use the modifications already made there.  I will add the TMP part to NAME()
 IF $$NAME()=1 GOTO ZXQUIT  ;"Note, I'm not here making note error returned (doesn't DO anything)

ZXQUIT
 ;"WRITE "LEAVING CUSTOM ZOSF",!
 Q
 ;
 ;
Z ;
 ;;ACTJ
 ;;S Y=$$ACTJ^%ZOSV()
 ;;AVJ
 ;;S Y=$$AVJ^%ZOSV()
 ;;BRK
 ;;U $I:(CENABLE)
 ;;DEL
 ;;N %RD,%OD S %RD=$P($S($ZRO["(":$P($P($ZRO,"(",2),")"),1:$ZRO)," ")_"/",%OD=$S($ZRO["(":$P($ZRO,"(",1)_"/",1:%RD) ZSYSTEM "rm -f "_%RD_X_".m" ZSYSTEM "rm -f "_%OD_X_".o"
 ;;EOFF
 ;;U $I:(NOECHO)
 ;;EON
 ;;U $I:(ECHO)
 ;;EOT
 ;;S Y=$ZA\1024#2 ; <=====
 ;;ERRTN
 ;;^%ZTER
 ;;ETRP
 ;;Q
 ;;GD
 ;;G ^%GD
 ;;$INC
 ;;0
 ;;JOBPARAM
 ;;G JOBPAR^%ZOSV
 ;;LABOFF
 ;;U IO:(NOECHO) ; <=====
 ;;LOAD
 ;;D LOAD^%ZOSV2(X) ;S %N=0 F XCNP=XCNP+1:1 S %N=%N+1,%=$T(+%N^@X) Q:$L(%)=0  S @(DIF_XCNP_",0)")=%
 ;;LPC
 ;;S Y="" ; <=====
 ;;MAGTAPE
 ;;S %MT("BS")="*1",%MT("FS")="*2",%MT("WTM")="*3",%MT("WB")="*4",%MT("REW")="*5",%MT("RB")="*6",%MT("REL")="*7",%MT("WHL")="*8",%MT("WEL")="*9" ; <=====
 ;;MAXSIZ
 ;;Q
 ;;MGR
 ;;VAH,ROU
 ;;MTBOT
 ;;S Y=$ZA\32#2 ; <=====
 ;;MTERR
 ;;S Y=$ZA\32768#2 ; <=====
 ;;MTONLINE
 ;;S Y=$ZA\64#2 ; <=====
 ;;MTWPROT
 ;;S Y=$ZA\4#2 ; <=====
 ;;NBRK
 ;;U $I:(NOCENABLE)
 ;;NO-PASSALL
 ;;U $I:(NOPASSTHRU)
 ;;NO-TYPE-AHEAD
 ;;U $I:(NOTYPEAHEAD)
 ;;PASSALL
 ;;U $I:(PASSTHRU)
 ;;PRIINQ
 ;;S Y=$$PRIINQ^%ZOSV()
 ;;PRIORITY
 ;;QUIT  ;G PRIORITY^%ZOSV
 ;;PROD
 ;;VAH,ROU
 ;;PROGMODE
 ;;S Y=$$PROGMODE^%ZOSV()
 ;;RD
 ;;G ^%RD
 ;;RESJOB
 ;;Q:'$D(DUZ)  Q:'$D(^XUSEC("XUMGR",+DUZ))  N XQZ S XQZ="^FORCEX[MGR]" D DO^%XUCI ; <=====
 ;;RM
 ;;U $I:WIDTH=$S(X<256:X,1:0)
 ;;RSEL
 ;;K ^UTILITY($J) D ^%RSEL S X="" X "F  S X=$O(%ZR(X)) Q:X=""""  S ^UTILITY($J,X)=""""" K %ZR
 ;;RSUM
 ;;S Y=0 F %=1,3:1 S %1=$T(+%^@X),%3=$F(%1," ") Q:'%3  S %3=$S($E(%1,%3)'=";":$L(%1),$E(%1,%3+1)=";":$L(%1),1:%3-2) F %2=1:1:%3 S Y=$A(%1,%2)*%2+Y
 ;;SS
 ;;D ^ZSY
 ;;SAVE
 ;;D SAVE^%ZOSV2(X) ;N %I,%F S %I=$I,%F=$P($S($ZRO["(":$P($P($ZRO,"(",2),")"),1:$ZRO)," ")_"/"_X_".m" O %F:(NEWVERSION) U %F X "F  S XCN=$O(@(DIE_XCN_"")"")) Q:+XCN'=XCN  S %=@(DIE_XCN_"",0)"") Q:$E(%,1)=""$""  I $E(%)'="";"" W %,!" C %F U %I
 ;;SIZE
 ;;S Y=0 F I=1:1 S %=$T(+I) Q:%=""  S Y=Y+$L(%)+2 ; <=====
 ;;TEST
 ;;I X]"",$T(^@X)]""
 ;;TMK
 ;;S Y=$ZA\16384#2
 ;;TMP
 ;;/tmp/
 ;;TRAP
 ;;$ZT="G "_X
 ;;TRMOFF
 ;;U $I:(TERMINATOR="")
 ;;TRMON
 ;;U $I:(TERMINATOR=$C(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,127))
 ;;TRMRD
 ;;S Y=$A($ZB)
 ;;TYPE-AHEAD
 ;;U $I:(TYPEAHEAD)
 ;;UCI
 ;;S Y=^%ZOSF("PROD")
 ;;UCICHECK
 ;;S Y=1
 ;;UPPERCASE
 ;;S Y=$TR(X,"abcdefghijklmnopqrstuvwxyz","ABCDEFGHIJKLMNOPQRSTUVWXYZ")
 ;;XY
 ;;S $X=DX,$Y=DY ; <=====
 ;;VOL
 ;;ROU
 ;;ZD
 ;;S Y=$$HTE^XLFDT(X,2) I $L($P(Y,"/"))=1 S Y=0_Y
