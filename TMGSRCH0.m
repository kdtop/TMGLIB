TMGSRCH0 ;TMG/kst/Search API ;05/19/10  ; 6/4/10, 2/2/14
        ;;1.0;TMG-LIB;**1**;05/19/10
        ;
 ;"TMG FILEMAN SEARCH API
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"NOTE: this function depends on NEW version of LIST^DIC, from G. Timpson Patch
 ;"=======================================================================
 ;" RPC -- Public Functions.
 ;"=======================================================================
 ;"FMSRCH(OUT,FILENUM,COMPEXPR) --A wrapper for Fileman search call
 ;"ARRYSRCH(FILENUM,PRESULT,ARRAY) -- Process parsed array, doing search
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"USRPGFN(TMGPGFN,TMGSTAT) -- Do user Progress Function, IF any.
 ;"SAMEFILE(PARRAY,STARTNUM,CURFILE) --Return range of search terms that are all in the same Fileman file
 ;"COMPEXPR(FILENUM,PARRAY,STARTN,ENDN,SRCHFILE,FIELDS) -- prepair a FILEMAN COMPUTED EXPRSSION from elements in ARRAY
 ;"FIXCOMB(COMB) -- Fix COMBINER term
 ;"COMP1XP(PARRAY,FIELDS) -- prepair 1 FILEMAN COMPUTED EXPRSSION from elements in ARRAY
 ;"FIXSET(TMGRSLT,TARGETFILE,SRCHFILE,FLDS,TMGSET) -- Change output of FMSRCH into needed format.
 ;"RESOLV(FILE,FLDSTR,IEN,ERR) -- follow pointer path to final value.
 ;"DOCOMB(COMB,TMG1SET,PRESULT) -- combine TMG1SET with @PRESULT based on logical operation COMBiner
 ;"=======================================================================
 ;"=======================================================================
 ;"Dependencies:
 ;" DIC (custom version), TMGDEBUG, TMGMISC, TMGSTUTL
 ;"=======================================================================
 ;"=======================================================================
 ;
 ;
ARRYSRCH(FILENUM,PRESULT,TMGARRAY,TMGPGFN) ;
        ;"Purpose: Process parsed array, doing search on terms, and combining them.
        ;"Input:  FILENUM -- This is the target file
        ;"        PRESULT-- Pass by NAME.  AN OUT PARAMETER. (see output below)
        ;"        TMGARRAY -- Pass by reference.  Contains search terms.  Format
        ;"              TMGARRAY("FILE")=FileNumber  (This is target output file)
        ;"              TMGARRAY(index,"FLD")=Field to search
        ;"              TMGARRAY(index,"FNUMPTR")=FileNum:FLD[:FLD[:FLD...]]
        ;"              TMGARRAY(index,"SRCH")=Value to search for
        ;"              TMGARRAY(index,"LOGIC",num)=...
        ;"              TMGARRAY(index,"WP")=1 IF field is a WP field
        ;"              TMGARRAY(index,"COMP")=comparator  Allowed Comparators: =, '=, '<, '>, [, IN
        ;"              TMGARRAY(index,"SUBTERMS")=1 IF has subterms
        ;"              TMGARRAY(index,indexB,...)...
        ;"         TMGPGFN -- OPTIONAL. Mumps code that will be called periodically
        ;"                            to allow display of progress of slow searches.
        ;"                           Code may depend on the following variables:
        ;"                           TMGSTAT -- The most recent status text
        ;"                           TMGPCT -- a very gross estimate of % done (0-100%)
        ;"Output:  PRESULT is filled in.  Format:
        ;"             @PRESULT@(0)=-1^Error Message, IF needed
        ;"           -or-
        ;"             @PRESULT@(IEN)=""
        ;"             @PRESULT@(IEN)=""
        ;"Result: Returns number of matches found.
        NEW ENTRYNUM,ENDNUM,TEMP,TMGEXPR,TMGFLDS,TMGFILE,MAXNUM
        NEW CT
        KILL @PRESULT
        NEW ERR SET ERR=0
        NEW DONE SET DONE=0
        SET MAXNUM=+$ORDER(TMGARRAY("@"),-1)
        IF MAXNUM<1 SET MAXNUM=1 ;"Avoid any divide by zero error
        SET ENTRYNUM=1
        FOR  DO  QUIT:(DONE=1)!(+ERR=-1)
        . SET TEMP=$$SAMEFILE("TMGARRAY",ENTRYNUM)
        . SET ENDNUM=$PIECE(TEMP,"^",2)
        . IF ENDNUM<ENTRYNUM SET DONE=1 QUIT
        . SET TMGEXPR=$$COMPEXPR(FILENUM,"TMGARRAY",ENTRYNUM,ENDNUM,.TMGFILE,.TMGFLDS)
        . IF +TMGEXPR=-1 SET ERR=TMGEXPR QUIT
        . NEW COMB SET COMB=$PIECE($GET(TMGARRAY(ENTRYNUM,"LOGIC")),"^",2)
        . DO FIXCOMB(.COMB)
        . NEW TMGOUT,TMGOPT
        . IF TMGFLDS'="" SET TMGOPT("FIELDS")="@;"_+TMGFLDS_"I"
        . DO USRPGFN(.TMGPGFN,"Searching file #"_TMGFILE_" for: "_TMGEXPR_" ...")
        . SET CT=$$FMSRCH(TMGFILE,TMGEXPR,.TMGOUT,.TMGOPT)
        . IF $DATA(TMGOUT("ERR")) SET ERR="-1^FILEMAN ERROR^"_$GET(TMGOUT("ERR",0)) QUIT
        . DO USRPGFN(.TMGPGFN,"Organizing "_CT_" search results so far...")
        . NEW TMG1SET
        . SET ERR=$$FIXSET(.TMGOUT,FILENUM,TMGFILE,TMGFLDS,.TMG1SET)
        . IF +ERR=-1 QUIT
        . DO USRPGFN(.TMGPGFN,"Combining search term with net results...")
        . IF COMB="" MERGE @PRESULT=TMG1SET
        . ELSE  DO DOCOMB(COMB,.TMG1SET,PRESULT)
        . SET ENTRYNUM=ENDNUM+1
        IF +ERR=-1 DO
        . KILL @PRESULT
        . SET @PRESULT@(0)=ERR
        . SET CT=0
        ELSE  DO
        . SET TMGSTAT="Counting search results..."
        . DO USRPGFN(.TMGPGFN)
        . SET CT=$$LISTCT^TMGMISC(PRESULT)
        . SET CT=CT-1 ;"Remove count of "DETAILS" node
        QUIT CT
        ;
USRPGFN(TMGPGFN,TMGSTAT) ;"Do user Progress Function, IF any.
        IF $GET(TMGPGFN)'="" DO
        . NEW $ETRAP SET $ETRAP="S $ETRAP="""",$ECODE="""""
        . NEW TMGPCT SET TMGPCT=(((ENTRYNUM-1)/MAXNUM)*100)\1
        . XECUTE TMGPGFN  ;"Run user's progress function code
        QUIT
        ;
SAMEFILE(PARRAY,STARTNUM,CURFILE) ;
        ;"Purpose: Return range of search terms that are all in the same Fileman file
        ;"         OLD-> NOTE: IF WP field is encountered, this is kicked out as NOT
        ;"               in same file, to overcome LIST^DIC limitation. (REMOVED AFTER LIMITATION FIXED)
        ;"Input: PARRAY -- PASS BY NAME.  This is ARRAY as passed to DOSRCH
        ;"       STARTNUM -- OPTIONAL.  The index to start consideration of. Default=1
        ;"       CURFILE -- OPTIONAL.  Used when calling self reiteratively. Leave blank first time.
        ;"Result: StartIndex^EndIndex of entries dealing with same file.
        ;
        SET STARTNUM=$GET(STARTNUM,1)
        NEW RESULT SET RESULT=STARTNUM_"^-1"
        NEW I SET I=STARTNUM-1
        SET CURFILE=+$GET(CURFILE)
        NEW DONE SET DONE=0
        FOR  SET I=$ORDER(@PARRAY@(I)) QUIT:(+I'>0)!(DONE=1)  DO
        . NEW THISFNUM SET THISFNUM=+$GET(@PARRAY@(I,"FNUMPTR"))
        . IF $GET(@PARRAY@(I,"SUBTERMS"))=1 DO  QUIT:DONE=1
        . . SET THISFNUM=CURFILE
        . . NEW TEMP SET TEMP=$$SAMEFILE($NAME(@PARRAY@(I)),1,.THISFNUM)
        . . NEW NUM2 SET NUM2=$PIECE(TEMP,"^",2)
        . . IF NUM2=-1 SET DONE=1 QUIT
        . . IF +$ORDER(@PARRAY@(I,NUM2))>0 SET DONE=1
        . IF (CURFILE>0) DO  QUIT:DONE=1
        . . IF (THISFNUM'=CURFILE) SET DONE=1 QUIT
        . . ;"IF $GET(@PARRAY@(I,"WP"))=1 SET DONE=1 QUIT
        . SET CURFILE=THISFNUM
        . SET $PIECE(RESULT,"^",2)=I
        QUIT RESULT
        ;
COMPEXPR(FILENUM,PARRAY,STARTN,ENDN,SRCHFILE,FIELDS) ;
        ;"Purpose: to prepair a FILEMAN COMPUTED EXPRSSION from elements in ARRAY
        ;"Input: ARRAY -- Pass by reference.  Contains search terms.  Format
        ;"              @PARRAY@("FILE")=FileNumber  (This is target output file)
        ;"              @PARRAY@(index,"FLD")=Field to search
        ;"              @PARRAY@(index,"FNUMPTR")=FileNum:FLD[:FLD[:FLD...]]
        ;"              @PARRAY@(index,"SRCH")=Value to search for
        ;"              @PARRAY@(index,"COMP")=comparator  Allowed Comparators: =, '=, '<, '>, [, IN
        ;"              @PARRAY@(index,"SUBTERMS")=1 IF has subterms
        ;"       STARTN -- The starting index to consider
        ;"       ENDN -- the ending index to consider
        ;"       SRCHFILE --PASS BY REFERENCE.  This is the file to search for fields in
        ;"       FIELDS -- Pass by reference. This is the desired output fields.
        ;"Results: Will return a COMPUTED EXPRESSION, or -1^Message
        ;"
        NEW RESULT SET RESULT=""
        NEW I,CURFIL
        SET CURFIL=0
        FOR I=STARTN:1:ENDN DO  QUIT:(+RESULT=-1)
        . IF RESULT'="" DO
        . . NEW COMB SET COMB=$PIECE($GET(@PARRAY@(I,"LOGIC")),"^",2)
        . . DO FIXCOMB(.COMB)
        . . SET RESULT=RESULT_COMB
        . IF $GET(@PARRAY@(I,"SUBTERMS"))=1 DO  QUIT
        . . NEW ENUM SET ENUM=+$ORDER(@PARRAY@(I,"@"),-1)
        . . NEW TEMP SET TEMP=$$COMPEXPR(FILENUM,$NAME(@PARRAY@(I)),1,ENUM,.SRCHFILE,.FIELDS)
        . . IF +TEMP=-1 SET RESULT=TEMP
        . . SET RESULT=RESULT_TEMP
        . NEW PRIOREXP SET PRIOREXP=$GET(@PARRAY@(I,"FM COMP EXPR"))
        . IF PRIOREXP'="" SET RESULT=RESULT_PRIOREXP QUIT
        . NEW FNUMPTR SET FNUMPTR=$GET(@PARRAY@(I,"FNUMPTR"))
        . IF FNUMPTR="" DO  QUIT
        . . SET RESULT="-1^No FNUMPTR found in array.  Can't create computed expression"
        . IF CURFIL=0 SET CURFIL=+FNUMPTR
        . IF CURFIL'=+FNUMPTR DO  QUIT
        . . SET RESULT="-1^Can't make computed expression involving different files."
        . SET SRCHFILE=CURFIL
        . NEW EXPR SET EXPR=$$COMP1XP($NAME(@PARRAY@(I)),.FIELDS)
        . IF +EXPR=-1 SET RESULT=EXPR QUIT
        . SET @PARRAY@(I,"FM COMP EXPR")=EXPR
        . SET RESULT=RESULT_EXPR
        QUIT RESULT
        ;
FIXCOMB(COMB) ; "Fix COMBINER terms
        IF COMB="AND" SET COMB="&"
        ELSE  IF COMB="OR" SET COMB="!"
        ELSE  IF COMB="NOT" SET COMB="&'"
        QUIT
        ;
COMP1XP(PARRAY,FIELDS) ;
        ;"Purpose: to prepair ONE FILEMAN COMPUTED EXPRSSION from elements in ARRAY
        ;"Input: PARRAY -- Pass by NAME.  Contains search terms.  Format
        ;"              @PARRAY@("FLD")=Field to search
        ;"              @PARRAY@("FNUMPTR")=FileNum:FLD[:FLD[:FLD...]]
        ;"              @PARRAY@("SRCH")=Value to search for (or Value..Value2 IF IN comparator)
        ;"              @PARRAY@("COMP")=comparator  Allowed Comparators: =, '=, '<, '>, [, IN
        ;"       FIELDS -- Pass by reference. This is the desired output fields.
        ;"Results: Will return a COMPUTED EXPRESSION, or -1^Message
        ;
        NEW RESULT SET RESULT=""
        NEW FLD SET FLD=$GET(@PARRAY@("FLD"))
        IF +FLD=0 DO  GOTO CP1DN
        . SET RESULT="-1^No field number found"
        SET FIELDS=$PIECE($GET(@PARRAY@("FNUMPTR")),":",2,999)
        NEW COMP SET COMP=$GET(@PARRAY@("COMP"))
        IF COMP="" DO  GOTO CP1DN
        . SET RESULT="-1^No comparator found"
        NEW VALUE SET VALUE=$GET(@PARRAY@("SRCH"))
        IF VALUE="" DO  GOTO CP1DN
        . SET RESULT="-1^No value to search for found."
        IF COMP'="IN" DO
        . SET RESULT=$$FMT1COMP(FLD,COMP,VALUE)
        ELSE  DO  ;"Handle .01IN"5..10"
        . NEW COMP SET COMP(1)="'<",COMP(2)="'>"
        . SET RESULT="("
        . NEW IDX FOR IDX=1,2 DO  QUIT:+RESULT<0
        . . NEW VAL SET VAL=$PIECE(VALUE,"..",IDX)
        . . IF VAL="" SET RESULT="-1^Range values (e.g. V1..V2) not found for IN comparator." QUIT
        . . SET RESULT=RESULT_$$FMT1COMP(FLD,COMP(IDX),VAL)_$SELECT(IDX=1:"&",IDX=2:")")
        IF +RESULT=-1 GOTO CP1DN
        ;
CP1DN   QUIT RESULT
        ;
FMT1COMP(FLD,COMP,VALUE) ;"Compose 1 comparison expression
        NEW RESULT SET RESULT="(#"_FLD_COMP
        IF +VALUE'=VALUE SET VALUE=""""_VALUE_""""
        SET RESULT=RESULT_VALUE_")"
        QUIT RESULT
        ;
FMSRCH(TMGFILE,TMGCOMPEXPR,TMGOUT,TMGOPTION)  ;
        ;"Purpose: This is a wrapper for NEW Fileman search call LIST^DIC
        ;"Input: TMGFILE -- File name or number to search in.
        ;"         TMGFILE(0) -- If FILE refers to a subfile, then FILE(0) must be SET to
        ;"                  the IENS that identifies which subfile to search.
        ;"                  If supplied, then FILE should be PASSED BY REFERENCE
        ;"       TMGCOMPEXPR -- This is a FILEMAN COMPUTED EXPRESSION used for search.
        ;"       TMGOUT -- PASS BY REFERENCE.  an OUT PARAMETER.  Pre-existing data killed.
        ;"              This is array that will be filled with results.
        ;"                e.g. OUT(IEN)=IEN^FieldValue(s)
        ;"              If OPTION("BYROOT")=1, then OUT must hold the *name* of a variable to be filled.
        ;"                e.g. @OUT@(IEN)=IEN^FieldValue(s)
        ;"         TMGOUT("ERR") -- will be filled with error messages, IF encountered
        ;"       TMGOPTION -- (OPTIONAL) -- Used to past customizations to LIST^DIC.
        ;"          TMGOPTION("BYROOT") If 1, then TMGOUT holds name of variable to be filled with results.
        ;"           ** See details in documentation for LIST^DIC for items below **
        ;"          TMGOPTION("FIELDS") -- Optional.  Fields to return with each entry.
        ;"          TMGOPTION("FLAGS") -- Optional.  Default="PX"  Note: "X" will always be passed to LIST^DIC
        ;"          TMGOPTION("NUMBER") -- Optional. Max number of entries to return.  Default is "*" (all)
        ;"          TMGOPTION("FROM") -- Optional.  Index entry from which to begin the list.
        ;"          TMGOPTION("PART") -- Optional.  A partial match restriction.
        ;"          TMGOPTION("SCREEN") -- Optional.  Screening code to apply to each potential entry.
        ;"          TMGOPTION("ID") -- Optional.  Identifier: text to accompany each entry returned in the list.
        ;"Results: returns # of matches.
        NEW TMGRESULT SET TMGRESULT=0
        SET TMGFILE=$GET(TMGFILE)
        IF +TMGFILE'=TMGFILE DO
        . NEW X,Y,DIC
        . SET DIC=1,DIC(0)="M"
        . SET X=TMGFILE
        . DO ^DIC
        . SET TMGFILE=+Y
        NEW TMGIENS SET TMGIENS=$GET(FILE(0))
        NEW TMGFLDS SET TMGFLDS=$GET(TMGOPTION("FIELDS"),"@;")
        NEW TMGFLAGS SET TMGFLAGS=$GET(TMGOPTION("FLAGS"),"P")
        IF TMGFLAGS'["X" SET TMGFLAGS=TMGFLAGS_"X"
        NEW TMGMAX SET TMGMAX=$GET(TMGOPTION("NUMBER"),"*")
        NEW TMGFROM MERGE TMGFROM=TMGOPTION("FROM")
        NEW TMGPART MERGE TMGPART=TMGOPTION("PART")
        NEW TMGSCR SET TMGSCR=$GET(TMGOPTION("SCREEN"))
        NEW TMGID SET TMGID=$GET(TMGOPTION("ID"))
        SET TMGCOMPEXPR=$GET(TMGCOMPEXPR)
        NEW TMGRSLT,TMGMSG
        NEW TMGDB,TMGX SET TMGDB=0  ;"Can be changed when stepping through code.
        IF TMGDB=1 DO
        . SET TMGX="DO LIST^DIC("_TMGFILE_","
        . IF $GET(TMGIENS)'="" SET TMGX=TMGX_""""_TMGIENS_""""
        . SET TMGX=TMGX_","""_TMGFLDS_""","
        . SET TMGX=TMGX_""""_TMGFLAGS_""","""_TMGMAX_""","
        . IF $DATA(TMGFROM) SET TMGX=TMGX_".TMGFROM"
        . SET TMGX=TMGX_","
        . IF $DATA(TMGPART) SET TMGX=TMGX_".TMGPART"
        . SET TMGX=TMGX_","""_$$QTPROTCT^TMGSTUT3(TMGCOMPEXPR)_""","
        . IF $GET(TMGSCR)'="" SET TMGMAX=TMGMAX_""""_TMGSCR_""""
        . SET TMGX=TMGX_","
        . IF $GET(TMGID)'="" SET TMGMAX=TMGMAX_""""_TMGID_""""
        . SET TMGX=TMGX_","
        . SET TMGX=TMGX_"""TMGRSLT"",""TMGMSG"")"
        DO LIST^DIC(TMGFILE,TMGIENS,TMGFLDS,TMGFLAGS,TMGMAX,.TMGFROM,.TMGPART,TMGCOMPEXPR,TMGSCR,TMGID,"TMGRSLT","TMGMSG")
        NEW BYROOT SET BYROOT=+$GET(TMGOPTION("BYROOT"))
        NEW OUTROOT
        IF BYROOT SET OUTROOT=TMGOUT
        ELSE  SET OUTROOT="TMGOUT"
        KILL @OUTROOT
        IF $DATA(TMGMSG("DIERR")) DO  GOTO FMSDN
        . MERGE @OUTROOT@("ERR")=TMGMSG("DIERR") ;"copy in errors, IF any
        . SET TMGRESULT=0
        MERGE @OUTROOT@(0)=TMGRSLT("DILIST",0)
        NEW I SET I=0
        NEW IENPCE SET IENPCE=0
        FOR I=1:1:999 IF $PIECE(TMGRSLT("DILIST",0,"MAP"),"^",I)="IEN" SET IENPCE=I QUIT
        SET I=0 FOR  SET I=$ORDER(TMGRSLT("DILIST",I)) QUIT:(+I'>0)  DO
        . NEW VALUE SET VALUE=$GET(TMGRSLT("DILIST",I,0))
        . NEW IEN SET IEN=$PIECE(VALUE,"^",IENPCE)
        . SET @OUTROOT@(IEN)=VALUE
        MERGE @OUTROOT@("ID")=TMGRSLT("ID") ;"Copy in identifiers, IF any
        SET TMGRESULT=+$PIECE(TMGRSLT("DILIST",0),"^",1)
FMSDN   QUIT TMGRESULT
        ;
        ;
FIXSET(TMGIN,TARGETFN,SRCHFILE,FLDS,TMG1SET) ;
        ;"Purpose: Change output of FMSRCH into needed format.
        ;"         Note: FMSRCH() won't allow ouput fields in format of .02:.01:.1 etc.
        ;"Input: TMGIN -- PASS BY REFERENCE.  The results of FMSRCH.  Format:
        ;"                TMGIN(SrchFileIEN)=SrchFileIEN^FieldValue  <-- FieldValue is a pointer/IEN
        ;"       TARGETFN -- The this the target file number.
        ;"       SRCHFILE -- The file that the results are from.
        ;"       FLDS --  The desired fields.  e.g. .02,  or .02:.01 etc.
        ;"       TMG1SET -- PASS BY REFERENCE.  AN OUT PARAMETER.  Prior results killed
        ;"              TMG1SET(SrchFileIEN)=""
        ;"              TMG1SET(SrchFileIEN)=""
        ;"              TMG1SET("DETAILS",TargetFileIEN,SrchFileNum,SrchFileIEN)
        ;"              TMG1SET("DETAILS",TargetFileIEN,SrchFileNum,SrchFileIEN)
        ;"Results: 0 IF OK, or -1^Message IF error.
        KILL TMG1SET
        NEW RESULT SET RESULT=0
        NEW VALUE
        NEW ERR SET ERR=0
        NEW IEN SET IEN=0
        FOR  SET IEN=$ORDER(TMGIN(IEN)) QUIT:(+IEN'>0)!(+RESULT=-1)  DO
        . IF SRCHFILE'=TARGETFN DO
        . . SET VALUE=$PIECE($GET(TMGIN(IEN)),"^",2) QUIT:(+VALUE'>0)
        . . IF FLDS[":" SET VALUE=$$RESOLV(SRCHFILE,FLDS,VALUE,.ERR)
        . ELSE  DO
        . . SET VALUE=+$GET(TMGIN(IEN))
        . QUIT:(+VALUE'>0)
        . IF +ERR=-1 SET RESULT=ERR QUIT
        . SET TMG1SET(VALUE)=""
        . SET TMG1SET("DETAILS",VALUE,SRCHFILE,IEN)="" ;"<-- Value=IEN in target file, IEN=IEN in SRCHFILE
        QUIT RESULT
        ;
RESOLV(FILE,FLDSTR,IEN,ERR) ;"  NOTE: THIS NEEDS TO BE COMPILED.  INEFFECIENT TO DO EACH TIME.
        ;"Purpose: To follow pointer path to final value.
        ;"Input: FILE -- File that IEN is in.
        ;"       FLDSTR -- e.g. ".02:.01:10:.01"
        ;"       IEN -- This is the value in FILE of the first field in FLDSTR (e.g. ".02")
        ;"       ERR -- PASS BY REFERENCE.  AN OUT PARAMETER.  -1^Err Msg, IF any
        ;"Result: Returns resolved value (INTERNAL FORMAT)
        NEW P2FILE,INFO
        SET ERR=""
        NEW RESULT SET RESULT=""
        IF FLDSTR[":" DO  GOTO:(+ERR=-1) RLVDN
        . NEW ZNODE SET ZNODE=$GET(^DD(FILE,+FLDSTR,0))
        . IF ZNODE="" DO  QUIT
        . . SET ERR="-1^Can't find declaration in DD for File #"_FILE_", FLD #"_+FLDSTR
        . SET INFO=$PIECE(ZNODE,"^",2)
        . SET P2FILE=+$PIECE(INFO,"P",2)
        . IF P2FILE'>0 DO  QUIT
        . . SET ERR="-1^File #"_FILE_", FLD #"_+FLDSTR_" is not a pointer field."
        . NEW ROOT SET ROOT="^"_$PIECE(ZNODE,"^",3)_IEN_")"
        . NEW NEXTFLDS SET NEXTFLDS=$PIECE(FLDSTR,":",2,999)
        . SET ZNODE=$GET(^DD(P2FILE,+NEXTFLDS,0))
        . NEW NODE SET NODE=$PIECE($PIECE(ZNODE,"^",4),";",1)
        . NEW PCE SET PCE=$PIECE($PIECE(ZNODE,"^",4),";",2)
        . NEW NEXTIEN SET NEXTIEN=$PIECE($GET(@ROOT@(NODE)),"^",PCE)
        . SET RESULT=$$RESOLV(P2FILE,NEXTFLDS,NEXTIEN,.ERR)
        ELSE  SET RESULT=IEN
RLVDN   QUIT RESULT
        ;
DOCOMB(COMB,TMG1SET,PRESULT) ;
        ;"Purpose: combine TMG1SET with @PRESULT based on logical operation COMBiner
        ;"Input: COMB= &, !, &'
        ;"       TMG1SET -- PASS BY REFERENCE.
        ;"       PRESULT -- PASS BY NAME.
        IF COMB="!" MERGE @PRESULT=TMG1SET
        ELSE  IF COMB="&" DO
        . NEW TEMPSET
        . NEW I SET I=0
        . FOR  SET I=$ORDER(TMG1SET(I)) QUIT:(+I'>0)  DO
        . . IF $DATA(@PRESULT@(I))=0 QUIT
        . . SET TEMPSET(I)=""
        . . MERGE TEMPSET("DETAILS",I)=TMG1SET("DETAILS",I)
        . . MERGE TEMPSET("DETAILS",I)=@PRESULT@("DETAILS",I)
        . KILL @PRESULT MERGE @PRESULT=TEMPSET
        ELSE  IF COMB="&'" DO
        . NEW I SET I=0
        . FOR  SET I=$ORDER(TMG1SET(I)) QUIT:(+I'>0)  DO
        . . KILL @PRESULT@(I)  ;"Remove any entry in TMG1SET from @PRESULT@
        . KILL @PRESULT MERGE @PRESULT=TEMPSET
        QUIT
        ;
