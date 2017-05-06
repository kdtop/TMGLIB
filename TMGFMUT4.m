TMGFMUT4 ;TMG/kst/Fileman utility functions ;8/10/11, 2/2/14
         ;;1.0;TMG-LIB;**1**;8/10/11
 ;
 ;"TMG FILEMAN-UTILITY FUNCTIONS
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
 ;"MERGE(TMGTMPLFILE,TMGTMPLFLD,TMGTMPLIEN,TMGDATAFILE,TMGOUT) -- Fileman MERGE API
 ;"$$GETFNUM(TMGFILENAME) --Convert a file name into a file number
 ;"$$GETFLDNM(TMGFILENUM,TMGFLDNAME) -- Convert field name --> field number
 ;"$$ISWPFLD(TMGFILE,TMGFLD) --return IF field FLD is a WP field
 ;
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;"
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"XLFSTR, ORWRP*, DIWF
 ;"=======================================================================
TEST  ;
        NEW DIC,X,Y
        NEW TMGTMPLFILE,TMGTMPLFLD,TMGTMPLIEN,TMGDATAFILE,TMGOUT
        SET DIC=1,DIC(0)="MAEQ"
        SET DIC("A")="Select file with letter template: "
        DO ^DIC WRITE !
        IF +Y'>0 QUIT
        SET TMGTMPLFILE=+Y
        ;
        SET DIC="^DD("_+Y_","
        SET DIC("A")="Select the WP field containing the template text: "
        DO ^DIC WRITE !
        IF +Y'>0 QUIT
        SET TMGTMPLFLD=+Y
        ;
        SET DIC=TMGTMPLFILE
        SET DIC("A")="Select the record entry in file to use as template: "
        DO ^DIC WRITE !
        IF +Y'>0 QUIT
        SET TMGTMPLIEN=+Y
        ;
        SET DIC=1,DIC(0)="MAEQ"
        SET DIC("A")="Select data source file: "
        DO ^DIC WRITE !
        IF +Y'>0 QUIT
        SET TMGDATAFILE=+Y
        ;
        NEW TMGOUT
        IF $$MERGE(TMGTMPLFILE,TMGTMPLFLD,TMGTMPLIEN,TMGDATAFILE,.TMGOUT) ;
        QUIT
        ;
MERGE(TMGTMPLFILE,TMGTMPLFLD,TMGTMPLIEN,TMGDATAFILE,TMGOUT) ;
        ;"Purpose: To Merge into a template document, and return resulting text in aray
        ;"Input: TMGTMPLFILE -- This is the Fileman file (name or number) that contains
        ;"                    the template/boilerplate text.  File must contain a WP field.
        ;"                   For now, subfiles are not supported.
        ;"       TMGTMPLFLD -- Field (name or number) that contains template WP field.
        ;"                    The text in the WP field should contain Fileman codes
        ;"                    mixed in with other normal text.  See also Discussion below.
        ;"       TMGTMPLIEN --  Template IEN in TMGTFILE -- the particular record in file to use.
        ;"       TMGDATAFILE -- The Fileman file (name or number) that contains that data
        ;"                    souce for the MERGE.  If any Fileman codes make reference
        ;"                    to a field, they will be interpreted as coming from this
        ;"                    file
        ;"       TMGOUT --  An OUT parameter. PASS BY REFERENCE.  See Output below. Prior data killed.
        ;"Discussion: Example letter:
        ;"              |TOP| Dear Sir or Madame,
        ;"              |BLANK(4)|
        ;"              Thank you for your recent letter.....
        ;"            Information about allowed Fileman codes may be found here:
        ;"            Formatting codes:
        ;"                http://hardhats.org/fileman/u2/e3_intro.htm  (see Text Formatting in Word-processing Fields)
        ;"
        ;"            Computed expressions:
        ;"                http://hardhats.org/fileman/u2/ce_frm.htm
        ;"
        ;"            Fileman functions:
        ;"                http://hardhats.org/fileman/u2/fn_frm.htm
        ;"Output: TMGOUT -- prior data killed.
        ;"            TMGOUT(1)="1st line of text"
        ;"            TMGOUT(2)="2nd line of text"
        ;"            TMGOUT(3)="2nd line of text" ... etc.
        ;"Results : 1^OK, or -1^ErrorMessage
        NEW TMGRESULT SET TMGRESULT="1^OK"
        ;
        KILL TMGOUT
        NEW TMGTEMP SET TMGTEMP=$GET(TMGTMPLFILE)
        SET TMGTMPLFILE=$$GETFNUM(TMGTEMP) ;
        IF TMGTMPLFILE'>0 SET TMGERR="template source file" GOTO MRGERR
        ;
        SET TMGTEMP=$GET(TMGTMPLFLD)
        SET TMGTMPLFLD=$$GETFLDNM(TMGTMPLFILE,TMGTEMP)
        IF TMGTMPLFLD'>0 SET TMGERR="template field" GOTO MRGDN
        ;
        IF $$ISWPFLD(TMGTMPLFILE,TMGTMPLFLD)=0 DO  GOTO MRGDN
        . SET TMGRESULT="-1^Field #"_TMGTMPLFLD_" in File #"_TMGTMPLFILE_" is not a WP type field."
        ;
        SET TMGTEMP=$GET(TMGTMPLIEN)
        SET TMGTMPLIEN=+TMGTEMP
        IF TMGTMPLIEN'>0 SET TMGERR="template record number (IEN)" GOTO MRGDN
        ;
        SET TMGTEMP=$GET(TMGDATAFILE)
        SET TMGDATAFILE=$$GETFNUM(TMGTEMP) ;
        IF TMGDATAFILE'>0 SET TMGERR="data source file" GOTO MRGDN
        ;
        NEW TMGREF SET TMGREF=$GET(^DIC(TMGTMPLFILE,0,"GL"))
        IF TMGREF="" DO  GOTO MRGDN
        . SET TMGRESULT="-1^"_$NAME(^DIC(TMGTMPLFILE,0,"GL"))_" doesn't exist."
        NEW TMGNODE SET TMGNODE=$PIECE($GET(^DD(TMGTMPLFILE,TMGTMPLFLD,0)),"^",4)
        SET TMGNODE=+$PIECE(TMGNODE,";",1)
        IF TMGNODE'>0 DO  GOTO MRGDN
        . SET TMGRESULT="-1^Unable to locate storage location for Field #"_TMGTMPLFLD_" in File #"_TMGTMPLFILE
        SET TMGREF=TMGREF_TMGTMPLIEN_","_TMGNODE_","
        ;
        NEW IOP SET IOP="HOME"
        ;
        NEW DIWF,FR,TO,BY,DIR
        SET DIWF=TMGREF              ;"Source reference
        SET DIWF(1)=+TMGDATAFILE    ;"Data file#
        SET BY="NUMBER"              ;"sort by IEN number
        SET (FR,TO)=TMGTMPLIEN       ;"Print only 1 IEN
        ;
        ;DO EN2^DIWF
        NEW ROOT ;"Will hold destination array NAME upon return from START^ORWRP
        DO START^ORWRP(80,"EN2^DIWF")
        IF $GET(ROOT)="" DO  GOTO MRGDN
        . SET TMGRESULT="-1^Problem creating MERGE."
        MERGE TMGOUT=@ROOT
        KILL @ROOT
        GOTO MRGDN
MRGERR  SET TMGRESULT="-1^Invalid "_TMGERR_":  Got '"_TMGTEMP_"'."
MRGDN   QUIT TMGRESULT

GETFNUM(TMGFILENAME) ;
        ;"PUBLIC FUNCTION
        ;"Purpose: Convert a file name into a file number
        ;"Input: The name (or number) of a file
        ;"Result: The filenumber, or 0 IF not found.
        ;"Note: copied and modified from TMGDBAPI
        ;"NOTE: If input is a number, this is just returned as result, no check
        NEW TMGRESULT SET TMGRESULT=0
        SET TMGFILENAME=$$TRIM^XLFSTR($$UP^XLFSTR($GET(TMGFILENAME)))
        IF TMGFILENAME="" GOTO GFNMDN
        IF +TMGFILENAME=TMGFILENAME SET TMGRESULT=TMGFILENAME GOTO GFNMDN
        NEW DIC,X,Y
        SET DIC=1 ;"File 1=Global file reference (the file listing info for all files)
        SET DIC(0)="M"
        SET X=TMGFILENAME   ;"i.e. "AGENCY"
        DO ^DIC  ;"lookup filename  Result comes back in Y ... i.e. "4.11^AGENCY"
        SET TMGRESULT=$PIECE(Y,"^",1)
        IF TMGRESULT=-1 SET TMGRESULT=0
GFNMDN  QUIT TMGRESULT
        ;
GETFLDNM(TMGFILENUM,TMGFLDNAME)
        ;"PUBLIC FUNCTION
        ;"Purpose: Given file and the name of a field, this will return the field NUMBER
        ;"Input: TMGFILENUM.  Number of file, i.e. "4.11"
        ;"       TMGFLDNAME: the name (or number) of a field, i.e. "NAME"
        ;"              Spelling must exactly match.  If TMGFLDNAME is provided
        ;"              as a number, this number is simply returned, no checking.
        ;"Output: Returns field number, i.e. ".01" or 0 IF not found
        NEW TMGRESULT SET TMGRESULT=0
        SET TMGFILENUM=+$GET(TMGFILENUM)
        IF TMGFILENUM'>0 GOTO GFDNDN
        SET TMGFLDNAME=$$UP^XLFSTR($GET(TMGFLDNAME))
        IF TMGFLDNAME=+TMGFLDNAME SET TMGRESULT=TMGFLDNAME GOTO GFDNDN
        SET TMGRESULT=$$FLDNUM^DILFD(TMGFILENUM,TMGFLDNAME)
GFDNDN  QUIT TMGRESULT
        ;
ISWPFLD(TMGFILE,TMGFLD) ;
        ;"PUBLIC FUNCTION
        ;"Purpose: return IF field FLD is a WP field
        ;"Input: FILE -- file NUMBER
        ;"       FLD -- field NUMBER
        ;"Result: 1 IF WP field, 0 IF not
        NEW TMGRESULT SET TMGRESULT=0
        NEW TMGINFO SET TMGINFO=$PIECE($GET(^DD(TMGFILE,TMGFLD,0)),"^",2)
        IF +TMGINFO'=TMGINFO GOTO IWPDN
        NEW TMGSUBFILE SET TMGSUBFILE=+TMGINFO
        SET TMGINFO=$PIECE($GET(^DD(TMGSUBFILE,.01,0)),"^",2)
        SET TMGRESULT=(TMGINFO["W")
IWPDN   QUIT TMGRESULT











 ;"note: BELOW is scratch code that I put in while trying to figure out how to
 ;"      get mergine and resolving of fields working properly.

WP(DIRF,DIWL,DIWR,DIWPUT,DIWF)
        ;"Write out WP field (if any) stored at DIRF, or put it in DIWPUT array
        ;
        ;"|" in DIWF means that "|"-windows are not to be evaluated, but are to be printed as
        ;     they stand.
        ;"X" means eXactly line-for-line, with "||" printed as "||"
        ;"W" in DIWF means that formatted text will be written out to
        ;     the current device as it is assembled.
        ;"N" means NOWRAP-- text is assembled line-for-line
        ;"R" means text will be assembled Right-justified
        ;"D" means text will be double-spaced
        ;"L" means internal line numbers appear at the left margin
        ;"C" followed by a number will cause formatting of text in a column
        ;     width specified by the number.
        ;"I" followed by a number will cause text to be indented that number
        ;     of columns.
        ;"?" means that, IF user's terminal is available, "|"-windows that cannot
        ;     be evaluated will be asked from the user's terminal.
        ;"B" followed by number causes NEW page when output gets within that
        ;   number of lines from the bottom of the page (as defined by IOSL).
        ;
        N Z,A1,D,X,DIW,DIWT,DN,I,DIWI,DIWTC,DIWX
        K ^UTILITY($J,"W")
        ;"S DIWF=$E("W",'$D(DIWPUT))_"|"
        S:'$G(IOM) IOM=80 S:'$G(DIWR) DIWR=IOM S:'$G(DIWL) DIWL=1
        S A1=$P($G(@DIRF@(0)),U,3)
        SET D=0
        FOR  SET D=$O(@DIRF@(D)) QUIT:D>A1&A1!'D  DO  G QWP:$G(DN)=0
        . SET X=^(D,0)
        . DO DIWP ;"^DIWP
        IF $G(DIWPUT)]"" DO  Q 1
        . KILL @DIWPUT
        . MERGE @DIWPUT=^UTILITY($J,"W")
        D ^DIWW
QWP     I $G(DN)'=0 Q 1
        K DIOEND
        Q 0
        ;
DIWP
        ;
        ;DIWTC is a Boolean -- Are we printing out in LINE MODE?
        SET:'$L(X) X=" "
        SET DIWTC=X[($C(124)_"TAB")
        SET:'$D(DN) DN=1
LN      SET:'$D(DIWF) DIWF=""
        SET:'DIWTC DIWTC=DIWF["N"
        SET DIWX=X
        SET DIW=$C(124)
        SET I=$P(DIWF,"C",2)
        IF I SET DIWR=DIWL+I-1
        IF '$D(^UTILITY($J,"W",DIWL)) DO  G DIW
        . SET ^(DIWL)=1
        . KILL DIWFU,DIWFWU,DIWLL
        . DO DIWI
        . SET:'$D(DIWT) DIWT="5,10,15,20,25"
        SET I=^(DIWL),DIWI=^(DIWL,I,0)
        IF DIWI="" DO  GOTO Z
        . DO DIWI
        DO NEW:DIWTC
        ;
Z       SET Z=X?.P!DIWTC
        IF X?1" ".E!Z DO
        . SET DIWTC=1
        . DO NEW:DIWI]""
        . SET DIWTC=Z
DIW     ;"from RCR+5^DIWW
        IF DIWF["X" DO  GOTO D
        . SET DIWTC=1
        . SET X=DIWX,DIWX=""
        . DO C
        SET X=$P(DIWX,DIW,1)
        DO C:X]""
        SET X=$P(DIWX,DIW,1),DIWX=$P(DIWX,DIW,2,999)
        GOTO D:DIWX=""
        IF $D(DIWP),X'?.E1" " DO ST
        SET X=$P(DIWX,DIW,1)
        IF $P(X,"TAB",1)="" DO TAB GOTO N
        IF X="TOP" DO  GOTO N
        . DO PUT
        . SET ^("X")="S DIFF=1 X:$D(^UTILITY($J,1)) ^(1)"
        . DO NEW
        IF DIWF[DIW GOTO TMG1
        GOTO U:X="_"
        DO PUT
        DO RCR^DIWW
        GOTO N:$D(X)
TMG1    SET X=DIW_$P(DIWX,DIW,1)_DIW
        DO C
N       KILL X
        SET DIWX=$P(DIWX,DIW,2,99)
        IF DIWX]"" D ST:$D(DIWP) G DIW
D       KILL DIWP
        DO PUT
        DO PRE:DIWTC
        SET:DIWTC DIWI=""
        QUIT
        ;
ST      SET DIWI=$E(DIWI,1,$L(DIWI)-1)
        K DIWP
        QUIT
        ;
DIWI    SET DIWI=$J("",+$P(DIWF,"I",2))
        IF DIWF["L",$D(D)#2 S DIWLL=D
        QUIT
        ;
PUT     SET I=^UTILITY($J,"W",DIWL)
        SET ^(DIWL,I,0)=DIWI
        IF DIWF["L",$D(DIWLL) S ^("L")=DIWLL
        QUIT
L ;
        SET DIWTC=1
        GOTO LN
 ;
TAB     IF X="" SET X=DIW GOTO C
        SET J=$P(DIWT,",",DIWTC)
        SET DIWTC=DIWTC+1
        SET:X?3A1P.P.N.E J=$E(X,5,9)
        SET:J?1"""".E1"""" J=$E(J,2,$L(J)-1)
        IF J'>0 SET %=$P(DIWX,DIW,2) QUIT:%=""  SET J=$S(J<0:1-$L(%)-J,J="C":DIWR-DIWL-$L(%)\2,1:0)
        SET J=J-1-$L(DIWI) QUIT:J<1  SET X=$J("",J)
C       KILL DIWP
        IF DIWTC S DIWI=DIWI_X QUIT
B       SET Z=DIWR-DIWL+1-$L(DIWI)
        GOTO FULL:$F(X," ")-1>Z
        FOR %=Z:-1 I " "[$E(X,%) SET:$E(X,%+1)=" " %=%+1 QUIT
        SET Z=$E(X,1,%-1)
        SET X=$E(X,%+1,999)
        IF Z]"" S DIWI=DIWI_Z G S:X]"" S %=$E(Z,$L(Z)) S:%'=" " DIWI=DIWI_$J("",%="."+1),DIWP=1 Q
FULL    IF $P(DIWF,"I",2)'<$L(DIWI) DO
        . SET DIWI=DIWI_$P(X," ",1)
        . SET X=$P(X," ",2,999)
S       DO PUT
        DO NEW
        GOTO B:X]""
        QUIT
 ;
U       SET I=^UTILITY($J,"W",DIWL)
        IF $D(DIWFU) SET ^(DIWL,I,"U",$L(DIWI)+1)="" KILL DIWFU GOTO N
        SET ^(DIWL,I,"U",$L(DIWI)+1)=X
        SET DIWFU=1
        GOTO N
 ;
NEW     DO DIWI
PRE     SET I=^UTILITY($J,"W",DIWL)
        SET ^(DIWL)=I+1
        SET ^(DIWL,I+1,0)=""
        IF DIWF["D" DO
        . SET ^(0)=" "
        . SET ^UTILITY($J,"W",DIWL)=I+2
        . SET ^(DIWL,I+2,0)=""
        IF $D(DIWFU) SET ^("U",1+$P(DIWF,"I",2))="_"
        GOTO P:DIWF'["R"!DIWTC
        KILL %
        QUIT:'$D(^UTILITY($J,"W",DIWL,I,0))
        SET Y=^(0)
        SET %=$L(Y)
        FOR %=%:-1 Q:$A(Y,%)-32
        SET Y=$E(Y,1,%)
        SET J=DIWR-DIWL-%+1
        SET %X=0
        GOTO P:J<1
        FOR %=1:1 S %(%)=$P(Y," ",1),Y=$P(Y," ",2,999) G:Y="" PAD:%-1,P I $E(%(%),$L(%(%)))?.P S:%=1&(%(%)="") %=0,%X=%X+1 S:%&J J=J-1,%(%)=%(%)_" "
PAD     IF J FOR Y=%\2+1:1:%-1,%\2:-1 SET %(Y)=%(Y)_" ",J=J-1 G PAD:Y=1!'J
        SET Y=%(%) F %=%-1:-1:1 S Y=%(%)_" "_Y
        SET ^(0)=$J("",%X)_Y
        KILL %
P       IF DIWF'["W" QUIT
        GOTO NX^DIWW
        QUIT
