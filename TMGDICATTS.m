TMGDICATTS ;KS- INPUT VERIFICATION FOR TMGDICATT* ;6 JAN,2011, 2/2/14
         ;;1.0;TMG-LIB;**1**;1/6/11
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
CHKALL(TMGRESULT)  ;
        ;"Purpose: Check for input needed for all fields
        ;"Input: TMGRESULT -- PASS BY REFERENCE.  Result variable
        ;"Output: will SET some globally-scoped variables, SEE BELOW
        ;"        TMGMULT,TMGMANDIT,TMGHLP,TMGXHLP,TMGDESCR,,TMGSUBSCR,TMGPIECE
        ;"        TMGFILE,TMGFLDNUM,TMGFLDNAME,TMGFLDTYPE
        ;"        TMGMULTASK2,TMGSHOWADDNEW,TMGMULTSN
        ;"        Also, TMGRESULT will be SET IF there is an error.
        ;"Result: none
        ;
        NEW X,Y,DIC
        SET TMGFILE=$GET(TMGINFO("FILE"))
        IF +TMGFILE'>0 DO  GOTO CHKADN
        . SET TMGRESULT="-1^File number not provided.  Got '"_TMGFILE_"'"
        IF $DATA(^DIC(TMGFILE))=0 DO  GOTO CHKADN
        . SET TMGRESULT="-1^File #"_DIFILE_" not found in data dictionary."
        SET Y=TMGFILE_"^"_$PIECE($GET(^DD(TMGFILE,0)),"^",1)
        SET (TMGREF,DIC)=$GET(^DIC(TMGFILE,0,"GL"))
        ;"S DLAYGO=1 D D^DICRW Q:Y<0   ;"NOTE: DICRW can add a NEW file!
        ;"Above sets DIC=global reference of file; Y=FileNum^FileName, DIFILE=fileNum, DIAC=1
        IF $P($G(^DD(+Y,0,"DI")),U)["Y",($P(@(^DIC(+Y,0,"GL")_"0)"),U,4)) DO  GOTO CHKADN
        . SET TMGRESULT="-1^DATA DICTIONARY MODIFICATIONS ON ARCHIVE FILES ARE NOT ALLOWED"
        IF DIC="" DO  GOTO CHKADN
        . SET TMGRESULT="-1^Unable to find global reference for file."
        ;"I '$D(DIC) D DIE^DIB Q:'$D(DG)  S DIC=DG
        ;"S:$D(DIAX) DIAXDIC=+$P(@(DIC_"0)"),U,2)
        SET TMGFLDNUM=$GET(TMGINFO("FLD","NUM"))
        IF (+TMGFLDNUM'>0),(TMGEDIT'=1) DO  GOTO CHKADN
        . SET TMGRESULT="-1^Field number not provided.  Got '"_TMGFLDNUM_"'"
        SET TMGFLDNUM=+TMGFLDNUM
        SET TMGFLDNAME=$GET(TMGINFO("FLD","NAME"))
        IF (TMGFLDNAME=""),(TMGEDIT=1),(TMGFLDNUM>0) DO
        . SET TMGFLDNAME=TMGFLDNUM
        IF TMGFLDNAME="" DO  GOTO CHKADN
        . SET TMGRESULT="-1^Field name not provided."
        SET TMGFLDTYPE=$GET(TMGINFO("FLD","DATATYPE"))
        IF TMGFLDTYPE'="" DO
        . SET DIC(0)="I",DIC="^DOPT(""DICATT"","
        . SET X=TMGFLDTYPE
        . DO ^DIC
        . IF Y'>0 DO  QUIT
        . . SET TMGRESULT="-1^Invalid field data type.  Got '"_TMGFLDTYPE_"'"
        . SET TMGFLDTYPE=Y
        ELSE  IF $GET(TMGEDIT)'=1 DO
        . SET TMGRESULT="-1^Field data type not provided"
        IF TMGRESULT<1 GOTO CHKADN
        ;
        SET TMGMULT=$GET(TMGINFO("FLD","MULTIPLE?"),"NO")
        IF $$CHKYN(.TMGMULT)=0 DO  GOTO CHKADN
        . SET TMGRESULT="-1^Invalid answer for INPUT(""FLD"",""MULTIPLE?"").  Got '"_TMGMULT_"'"
        SET TMGMANDIT=$GET(TMGINFO("FLD","MANDITORY?"),"NO")
        IF $$CHKYN(.TMGMANDIT)=0 DO  GOTO CHKADN
        . SET TMGRESULT="-1^Invalid answer for INPUT(""FLD"",""MANDITORY?"").  Got '"_TMGMANDIT_"'"
        SET TMGHLP=$GET(TMGINFO("FLD","HELP PROMPT"))
        SET TMGXHLP=$GET(TMGINFO("FLD","XECUTABLE HELP"))
        KILL TMGDESCR MERGE TMGDESCR=TMGINFO("FLD","DESCRIPTION")
        IF TMGEDIT=1 GOTO CHKA3
        ;
        SET TMGSUBSCR=$GET(TMGINFO("FLD","STORE","SUBSCRIPT"))
        IF (TMGSUBSCR=""),(+TMGFLDTYPE'=6) DO  GOTO CHKADN   ;"6=Computed type
        . SET TMGRESULT="-1^No value supplied for INPUT(""FLD"",""SUBSCRIPT"")"
        IF (TMGSUBSCR?1P.E)!(TMGSUBSCR[",")!(TMGSUBSCR[":")!(TMGSUBSCR["=")!(TMGSUBSCR["""")!(TMGSUBSCR[";") DO  GOTO CHKADN
        . SET TMGRESULT="-1^Invalid value for SUBSCRIPT.  Can't contain [,:="";] characters.  Got '"_TMGSUBSCR_"'"
        IF $$CHKCHARS(.TMGSUBSCR,"SUBSCRIPT",",:="";^")=0 GOTO CHKADN
        IF (TMGSUBSCR?1P.E) DO  GOTO CHKADN
        . SET TMGRESULT="-1^Invalid value for SUBSCRIPT.  Got '"_TMGSUBSCR_"'"
        IF (TMGSUBSCR'?.ANP) DO  GOTO CHKADN
        . SET TMGRESULT="-1^Invalid value for SUBSCRIPT.  Control Characters are not allowed."
        SET PCE=$$TRIM^XLFSTR($GET(TMGINFO("FLD","STORE","PIECE")))
        IF (PCE="")&(TMGMULT["Y") SET PCE=1  ;".01 field of subfile always 0;1
        IF (PCE="")&((+TMGFLDTYPE=5)!(+TMGFLDTYPE=6)!(+TMGFLDTYPE=9)) DO  GOTO CHKA2  ;"5=WP,6=Computed,9=Mumps
        . SET PCE=0 ;"shouldn't be needed
        IF (PCE="") DO  GOTO CHKADN
        . SET TMGRESULT="-1^No value supplied for INPUT(""FLD"",""PIECE"")."
        IF (+PCE=PCE),(PCE'>0)!(PCE>240) DO  GOTO CHKADN
        . SET TMGRESULT="-1^Invalid NUMBER supplied for INPUT(""FLD"",""PIECE"").  Got '"_PCE_"'"
        IF (PCE'>0)!(PCE'<100)!(PCE\1'=PCE) DO  GOTO CHKADN
        . SET TMGRESULT="-1^Invalid NUMBER supplied for INPUT(""FLD"",""PIECE"").  Must be integer from 1-99. Got '"_PCE_"'"
CHKA2   SET TMGPIECE=PCE
CHKA3   SET TMGMULTASK2=$GET(TMGINFO("FLD","MULTIPLE?","ASK FOR ANOTHER?"),"NO")
        IF $$CHKYN(.TMGMULTASK2)=0 DO  GOTO CHKADN
        . SET TMGRESULT="-1^Invalid answer for INPUT(""FLD"",""MULTIPLE?"",""ASK FOR ANOTHER?"").  Got '"_TMGMULTASK2_"'"
        SET TMGSHOWADDNEW=$GET(TMGINFO("FLD","SEE ADDING NEW?"),"NO")
        IF $$CHKYN(.TMGSHOWADDNEW)=0 DO  GOTO CHKADN
        . SET TMGRESULT="-1^Invalid answer for INPUT(""FLD"",""MULTIPLE?"",""SEE ADDING NEW?"").  Got '"_TMGMULTASK2_"'"
        SET TMGMULTSN=$GET(TMGINFO("FLD","MULTIPLE?","SUB-DICT NUM")) ;"
CHKADN  QUIT
        ;
CHKYN(VAR) ;
        ;"Insure VAR is Y or N, or make error
        ;"Input: VAR -- variable to check.  PASS BY REFERENCE.
        ;"Result: 1 if OK, 0 IF error.
        IF VAR="YES" SET VAR="Y"
        ELSE  IF VAR="NO" SET VAR="N"
        IF (VAR'="Y")&(VAR'="N") QUIT 0
        QUIT 1
        ;
CHKCHARS(VAR,LABEL,INVAL) ;
        ;"Insure VAR doesn't containe invalid characters
        ;"Input: VAR -- variable to check.  PASS BY REFERENCE.
        ;"       LABEL, used to explain error.
        ;"       INVAL -- string with all characters that are NOT allowed
        ;"Result: 1 if OK, 0 IF error.
        ;"Output: IF error, then TMGRESULT is set
        NEW I FOR I=1:1:$L(INVAL) DO  IF +TMGRESULT<0 GOTO CHKADN
        . NEW CH SET CH=$EXTRACT(INVAL,I)
        . IF VAR'[CH QUIT
        . SET TMGRESULT="-1^Invalid value for "_LABEL_".  Can't contain '"_CH_"' character.  Got '"_VAR_"'"
        IF VAR'?.ANP DO
        . SET TMGRESULT="-1^Invalid value for "_LABEL_".  Can't contain control characters.  Got '"_VAR_"'"

        QUIT (TMGRESULT>0)
        ;
CHKSCRN(TMGRESULT) ;
        ;"Purpose: to check for input screening code.
        ;"Input: TMGRESULT -- PASS BY REFERENCE.  Result variable
        ;"Output: will SET some globally-scoped variables, SEE BELOW
        ;"    TMGIPSCRN,TMGIPSDESCR
        ;"        Also, TMGRESULT will be SET IF there is an error.
        ;"Result: NONE.  (TMGRESULT is changed IF any problems)
        SET TMGIPSCRN=$GET(TMGINFO("FLD","INPUT SCREEN CODE"))
        IF TMGIPSCRN="" SET RESULT=1 GOTO CHKSDN ;"OK
        NEW X,D
        SET X=TMGIPSCRN
        DO ^DIM  ;"kills X IF contains invalid mumps code
        IF '$DATA(X) DO  GOTO CHKSDN
        . SET TMGRESULT="-1^Invalid VA MUMPS code. Got: "_TMGIPSCRN
        IF DUZ(0)'="@" DO  GOTO CHKSDN
        . SET TMGRESULT="-1^User may not supply INPUT SCREEN CODE.  Missing programmer access code '@'."
        IF TMGIPSCRN'["DIC(""S"")" DO  GOTO CHKSDN
        . SET TMGRESULT="-1^Missing 'DIC(""S"")' in INPUT SCREEN CODE.  Got: "_TMGIPSCRN
        SET TMGIPSDESCR=$GET(TMGINFO("FLD","INPUT SCREEN DESCRIPTION"))
        IF TMGIPSDESCR="" DO  GOTO CHKSDN
        . SET TMGRESULT="-1^Input screen code provided, but REQUIRED accompanying description missing."
CHKSDN  QUIT
        ;
CHK1(TMGRESULT)    ;
        ;"Purpose: Check for input needed for data type 1
        ;"Input: TMGRESULT -- PASS BY REFERENCE.  Result variable
        ;"Output: will SET some globally-scoped variables, SEE BELOW
        ;"        TMGSDATE,TMGEDATE,TMGIMPDAT,TMGTIMEOK,TMGTIMREQ,TMGSECOK
        ;"        Also, TMGRESULT will be SET IF there is an error.
        ;"Result: none
        NEW %DT,X,Y
        SET %DT="T"
        SET TMGSDATE=$GET(TMGINFO("FLD",1,"EARLIEST DATE"))
        IF TMGSDATE'="" DO
        . SET X=TMGSDATE DO ^%DT
        . IF Y>0 SET TMGSDATE=Y QUIT
        . SET TMGRESULT="-1^Invalid date for INPUT(""FLD"",1,""EARLIEST DATE"").  Got '"_TMGSDATE_"'"
        IF TMGRESULT<0 GOTO CHK1DN
        SET TMGEDATE=$GET(TMGINFO("FLD",1,"LATEST DATE"))
        IF TMGEDATE'="" DO
        . SET X=TMGEDATE DO ^%DT
        . IF Y>0 SET TMGEDATE=Y QUIT
        . SET TMGRESULT="-1^Invalid date for INPUT(""FLD"",1,""LATEST DATE"").  Got '"_TMGEDATE_"'"
        IF TMGRESULT<0 GOTO CHK1DN
        SET TMGIMPDAT=$GET(TMGINFO("FLD",1,"IMPRECISE DATE OK"),"NO")
        IF $$CHKYN(.TMGIMPDAT)=0 DO  GOTO CHK1DN
        . SET TMGRESULT="-1^Invalid answer for INPUT(""FLD"",1,""IMPRECISE DATE OK"").  Got '"_TMGIMPDAT_"'"
        SET TMGTIMEOK=$GET(TMGINFO("FLD",1,"TIME ALLOWED"),"YES")
        IF $$CHKYN(.TMGTIMEOK)=0 DO  GOTO CHK1DN
        . SET TMGRESULT="-1^Invalid answer for INPUT(""FLD"",1,""TIME ALLOWED"").  Got '"_TMGTIMEOK_"'"
        SET TMGTIMREQ=$GET(TMGINFO("FLD",1,"TIME REQUIRED"),"NO")
        IF $$CHKYN(.TMGTIMREQ)=0 DO  GOTO CHK1DN
        . SET TMGRESULT="-1^Invalid answer for INPUT(""FLD"",1,""IMPRECISE DATE OK"").  Got '"_TMGTIMREQ_"'"
        SET TMGSECOK=$GET(TMGINFO("FLD",1,"SEC ALLOWED"),"YES")
        IF $$CHKYN(.TMGIMPDAT)=0 DO  GOTO CHK1DN
        . SET TMGRESULT="-1^Invalid answer for INPUT(""FLD"",""IMPRECISE DATE OK"").  Got '"_TMGSECOK_"'"
CHK1DN  QUIT
        ;
CHK2(TMGRESULT)    ;
        ;"Purpose: Check for input needed for data type 2
        ;"Input: TMGRESULT -- PASS BY REFERENCE.  Result variable
        ;"Output: will SET some globally-scoped variables, SEE BELOW
        ;"   TMGMINN,TMGMAXN,TMGDOLLAR,TMGDIGITS
        ;"        Also, TMGRESULT will be SET IF there is an error.
        ;"Result: none
        SET TMGMINN=$$TRIM^XLFSTR($GET(TMGINFO("FLD",2,"INCLUSIVE LOWER BOUND")))
        IF +TMGMINN'=TMGMINN DO  GOTO CHK2DN
        . SET TMGRESULT="-1^Invalid number for INPUT(""FLD"",2,""INCLUSIVE LOWER BOUND"").  Got '"_TMGMINN_"'"
        SET TMGMAXN=$$TRIM^XLFSTR($GET(TMGINFO("FLD",2,"INCLUSIVE UPPER BOUND")))
        IF +TMGMAXN'=TMGMAXN DO  GOTO CHK2DN
        . SET TMGRESULT="-1^Invalid number for INPUT(""FLD"",2,""INCLUSIVE UPPER BOUND"").  Got '"_TMGMAXN_"'"
        IF TMGMAXN<TMGMINN DO  GOTO CHK2DN
        . SET TMGRESULT="-1^Max number is less than Min amount.  Got UPPER="_TMGMAXN_", and LOWER="_TMGMINN
        SET TMGDOLLAR=$GET(TMGINFO("FLD",2,"DOLLAR AMOUNT?"),"NO")
        IF $$CHKYN(.TMGDOLLAR)=0 DO  GOTO CHK1DN
        . SET TMGRESULT="-1^Invalid answer for INPUT(""FLD"",2,""DOLLAR AMOUNT?"").  Got '"_TMGDOLLAR_"'"
        SET TMGDIGITS=+$GET(TMGINFO("FLD",2,"MAX NUMBER OF DIGITS"),"0")
CHK2DN  QUIT
        ;
CHK3(TMGRESULT)    ;
        ;"Purpose: Check for input needed for data type 3
        ;"Input: TMGRESULT -- PASS BY REFERENCE.  Result variable
        ;"Output: will SET some globally-scoped variables, SEE BELOW
        ;"      TMGSET
        ;"        Also, TMGRESULT will be SET IF there is an error.
        ;"Result: none
        DO CHKSCRN(.TMGRESULT)
        IF +TMGRESULT'>0 GOTO CHK3DN
        NEW CODE SET CODE=""
        FOR  SET CODE=$ORDER(TMGINFO("FLD",3,"SET",CODE)) QUIT:(CODE="")!(TMGRESULT<0)  DO
        . IF $$CHKCHARS(.CODE,"INTERNAL SET CODE",",:="";^?@")=0 QUIT
        . NEW NAME SET NAME=$GET(TMGINFO("FLD",3,"SET",CODE))
        . IF NAME="" DO  QUIT
        . . SET TMGRESULT="-1^No external value provided for internal SET code '"_CODE_"'"
        . IF $$CHKCHARS(.CODE,"EXTERNAL SET CODE NAME",",:="";^?@")=0 QUIT
        . SET TMGSET(CODE)=NAME
        IF $DATA(TMGSET)=0 DO
        . SET TMGRESULT="-1^No data for sets found in INPUT(""FLD"",3,*)"
CHK3DN  QUIT
        ;
CHK4(TMGRESULT)    ;
        ;"Purpose: Check for input needed for data type 4
        ;"Input: TMGRESULT -- PASS BY REFERENCE.  Result variable
        ;"Output: will SET some globally-scoped variables, SEE BELOW
        ;"        TMGMINL,TMGMAXL,TMGPATMATCH
        ;"        Also, TMGRESULT will be SET IF there is an error.
        ;"Result: none
        SET TMGMINL=$GET(TMGINFO("FLD",4,"MIN LEN"),1)
        SET TMGMAXL=$GET(TMGINFO("FLD",4,"MAX LEN"),32)
        SET TMGPATMATCH=$GET(TMGINFO("FLD",4,"PATTERN MATCH"))
        IF (TMGMINL<0)!(TMGMAXL>240) DO
        . SET TMGRESULT="-1^Invalid value for INPUT(""FLD"",""MAX LEN""). Should be 1-240."
        IF TMGMAXL<TMGMINL DO
        . SET TMERESULT="-1^Invalid input.  MAX ('"_TMGMAXL_"') must be greater than MIN ('"_TMGMINL_"')."
        QUIT
        ;
CHK5(TMGRESULT)    ;
        ;"Purpose: Check for input needed for data type 5
        ;"Input: TMGRESULT -- PASS BY REFERENCE.  Result variable
        ;"Output: will SET some globally-scoped variables, SEE BELOW
        ;"    TMGWPWRAP,TMGWPNOVAR
        ;"        Also, TMGRESULT will be SET IF there is an error.
        ;"Result: none
        SET TMGWPWRAP=$GET(TMGINFO("FLD",5,"WORD WRAP MODE"),"YES")
        IF $$CHKYN(.TMGWPWRAP)=0 DO  GOTO CHK5DN
        . SET TMGRESULT="-1^Invalid answer for INPUT(""FLD"",5,""WORD WRAP MODE"").  Got '"_TMGWPWRAP_"'"
        SET TMGWPNOVAR=$GET(TMGINFO("FLD",5,"IGNORE |"),"NO")
        IF $$CHKYN(.TMGWPNOVAR)=0 DO  GOTO CHK5DN
        . SET TMGRESULT="-1^Invalid answer for INPUT(""FLD"",5,""IGNORE |"").  Got '"_TMGWPNOVAR_"'"
CHK5DN  QUIT
        ;
CHK6(TMGRESULT)    ;
        ;"Purpose: Check for input needed for data type 6
        ;"Input: TMGRESULT -- PASS BY REFERENCE.  Result variable
        ;"Output: will SET some globally-scoped variables, SEE BELOW
        ;"        TMGCOMPEXP,TMGCOMPTYP.TMGCOMPND.TMGCOMPROU,TMGCOMPCFC
        ;"        Also, TMGRESULT will be SET IF there is an error.
        ;"Result: none
        ;
        SET TMGCOMPEXP=$GET(TMGINFO("FLD",6,"COMPUTED-FIELD EXPRESSION"))
        IF TMGCOMPEXP="" DO  GOTO CHK6DN
        . SET TMGRESULT="-1^Missing value for INPUT(""FLD"",6,""COMPUTED-FIELD EXPRESSION"")"
        SET TMGCOMPTYP=$GET(TMGINFO("FLD",6,"RESULT TYPE"))
        NEW I FOR I="S","N","B","D","m","p","mp" IF TMGCOMPTYP=I GOTO CHK6L1
        IF TMGCOMPTYP="STRING" SET TMGCOMPTYP="S" GOTO CHK6L1
        IF TMGCOMPTYP="NUMERIC" SET TMGCOMPTYP="N" GOTO CHK6L1
        IF TMGCOMPTYP="BOOLEAN" SET TMGCOMPTYP="B" GOTO CHK6L1
        IF TMGCOMPTYP="DATE" SET TMGCOMPTYP="D" GOTO CHK6L1
        IF TMGCOMPTYP="MULTIPLE" SET TMGCOMPTYP="m" GOTO CHK6L1
        IF TMGCOMPTYP="POINTER" SET TMGCOMPTYP="p" GOTO CHK6L1
        IF TMGCOMPTYP="MULTIPLE POINTER" SET TMGCOMPTYP="mp" GOTO CHK6L1
        ;"If get to here, then error
        SET TMGRESULT="-1^Invalid value for INPUT(""FLD"",6,""RESULT TYPE"").  Got '"_TMGCOMPTYP_"'"
        GOTO CHK6DN
CHK6L1  IF TMGCOMPTYP'="p" GOTO CHK6L2
        SET TMGCOMPP2=$GET(TMGINFO("FLD",6,"POINT TO WHAT FILE"))
        NEW DIC,X,Y
        SET DIC=1,DIC(0)="M"
        SET X=TMGCOMPP2
        DO ^DIC
        IF (Y<0)!($$OKFILE^DICOMPX(+Y,"W")'=1) DO  GOTO CHK6DN
        . SET TMGRESULT="-1^Unusable file in INPUT(""FLD"",6,""POINT TO WHAT FILE"").  Got '"_TMGCOMPP2_"'"
        SET TMGCOMPP2=Y
CHK6L2  IF TMGCOMPTYP'="N" GOTO CHK6L3
        SET TMGCOMPND=+$GET(TMGINFO("FLD",6,"NUM DIGITS"))\1
        IF (TMGCOMPND<0)!(TMGCOMPND>14) DO  GOTO CHK6DN
        . SET TMGRESULT="-1^Unusable file in INPUT(""FLD"",6,""NUM DIGITS"").  Must be 0-14. Got '"_TMGCOMPND_"'"
        SET TMGCOMPROU=$GET(TMGINFO("FLD",6,"ROUND TO NUM DIGITS"),"YES")
        IF $$CHKYN(.TMGCOMPROU)=0 DO  GOTO CHK6DN
        . SET TMGRESULT="-1^Invalid answer for INPUT(""FLD"",6,""ROUND TO NUM DIGITS"").  Got '"_TMGCOMPROU_"'"
        SET TMGCOMPCFC=$GET(TMGINFO("FLD",6,"CALC FROM SUMS OF COMPONENT FLDS")) ;"(default is "")
        IF (TMGCOMPCFC'=""),$$CHKYN(.TMGCOMPCFC)=0 DO  GOTO CHK6DN
        . SET TMGRESULT="-1^Invalid answer for INPUT(""FLD"",6,""CALC FROM SUMS OF COMPONENT FLDS"").  Got '"_TMGCOMPCFC_"'"
CHK6L3  IF TMGCOMPTYP'="S" GOTO CHK6L4
        SET TMGCOMPMXL=$GET(TMGINFO("FLD",6,"MAX LEN OUT"))
        IF TMGCOMPMXL="" GOTO CHK6L4
        IF +TMGCOMPMXL<1 DO  GOTO CHK6DN
        . SET TMGRESULT="-1^Invalid value in INPUT(""FLD"",6,""MAX LEN OUT""). Must be > 0.  Got '"_TMGCOMPMXL_"'"
        SET TMGCOMPMXL=TMGCOMPMXL\1
CHK6L4  ;
CHK6DN  QUIT
        ;
CHK7(TMGRESULT)    ;
        ;"Purpose: Check for input needed for data type 7  (POINTER TO A FILE)
        ;"Input: TMGRESULT -- PASS BY REFERENCE.  Result variable
        ;"Output: will SET some globally-scoped variables, SEE BELOW
        ;"        TMGP2F,TMGLAYGO
        ;"        Also, TMGRESULT will be SET IF there is an error.
        ;"Result: none
        ;
        DO CHKSCRN(.TMGRESULT)
        IF +TMGRESULT'>0 GOTO CHK3DN
        SET TMGP2F=$GET(TMGINFO("FLD",7,"POINT TO WHAT FILE"))
        NEW DIC,X,Y
        SET DIC=1,DIC(0)="MIZ"
        SET DIC("S")="I Y'=1.1 S DIFILE=+Y,DIAC=""RD"" D ^DIAC I %"
        SET X=TMGP2F
        DO ^DIC
        IF (Y<0) DO  GOTO CHK6DN
        . SET TMGRESULT="-1^Unusable file in INPUT(""FLD"",7,""POINT TO WHAT FILE"").  Got '"_TMGP2F_"'"
        MERGE TMGP2F=Y
        SET TMGLAYGO=$GET(TMGINFO("FLD",7,"LAYGO"),"NO")
        IF $$CHKYN(.TMGLAYGO)=0 DO  GOTO CHK7DN
        . SET TMGRESULT="-1^Invalid answer for INPUT(""FLD"",7,""LAYGO"").  Got '"_TMGLAYGO_"'"
CHK7DN  QUIT
        ;
CHK8(TMGRESULT)    ;
        ;"Purpose: Check for input needed for data type 8  (VARIABLE-POINTER)
        ;"Input: TMGRESULT -- PASS BY REFERENCE.  Result variable
        ;"Output: will SET some globally-scoped variables, SEE BELOW
        ;"    TMGVPTR(I,"PTR")=+Y  ;"File Number
        ;"    TMGVPTR(I,"MSG")=MSG
        ;"    TMGVPTR(I,"PREFIX")=PREFIX
        ;"    TMGVPTR(I,"SCRN")=SCRNCODE
        ;"    TMGVPTR(I,"SCRN DESCR")=DESCR
        ;"    TMGVPTR(I,"LAYGO")=LAYGO
        ;"        Also, TMGRESULT will be SET IF there is an error.
        ;"Result: none
        ;
        IF '$DATA(TMGINFO("FLD",8)) DO  GOTO CHK8DN
        . SET TMGRESULT="-1^No data provided for variable pointers in INPUT(""FLD"",8,*)"
        NEW I SET I=0
        FOR  SET I=$ORDER(TMGINFO("FLD",8,I)) QUIT:(I="")  DO  QUIT:(TMGRESULT<0)
        . NEW P2FILE SET P2FILE=$GET(TMGINFO("FLD",8,I,"POINT TO WHAT FILE"))
        . IF P2FILE="" DO  QUIT
        . . SET TMGRESULT="-1^Entry INPUT(""FLD"",8,"_I_",""POINT TO WHAT FILE"") is null."
        . NEW DIC,X,Y
        . SET DIC=1,DIC(0)="MI"
        . SET DIC("S")="I Y'=1.1 S DIFILE=+Y,DIAC=""RD"" D ^DIAC I %"
        . SET X=P2FILE
        . DO ^DIC
        . IF (Y<0) DO  QUIT
        . . SET TMGRESULT="-1^Unusable file in INPUT(""FLD"",8,"_I_",""POINT TO WHAT FILE"").  Got '"_P2FILE_"'"
        . SET TMGVPTR(I,"PTR")=+Y  ;"File Number
        . NEW LAYGO SET LAYGO=$GET(TMGINFO("FLD",8,I,"LAYGO"),"NO")
        . IF $$CHKYN(.LAYGO)=0 DO  QUIT
        . . SET TMGRESULT="-1^Invalid answer for INPUT(""FLD"",8,"_I_",""LAYGO"").  Got '"_LAYGO_"'"
        . NEW TMP SET TMP=$P($G(^DD(+Y,0,"DI")),U,2)
        . IF TMP["Y",LAYGO="Y" DO  QUIT
        . . SET TMGRESULT="-1^LAYGO not allowed for file "_P2FILE
        . SET TMGVPTR(I,"LAYGO")=$$LOW^XLFSTR(LAYGO)
        . NEW MSG SET MSG=$GET(TMGINFO("FLD",8,I,"MESSAGE"))
        . IF MSG="" DO  QUIT
        . . SET TMGRESULT="-1^Entry INPUT(""FLD"",8,"_I_",""MESSAGE"") is null."
        . SET TMGVPTR(I,"MSG")=MSG
        . NEW PREFIX SET PREFIX=$GET(TMGINFO("FLD",8,I,"PREFIX"))
        . IF PREFIX="" DO  QUIT
        . . SET TMGRESULT="-1^Entry INPUT(""FLD"",8,"_I_",""PREFIX"") is null."
        . SET TMGVPTR(I,"PREFIX")=PREFIX
        . NEW SCRNCODE SET SCRNCODE=$GET(TMGINFO("FLD",8,I,"INPUT SCREEN CODE"))
        . IF SCRNCODE="" QUIT
        . IF DUZ(0)'="@" DO  QUIT
        . . SET TMGRESULT="-1^User may not supply INPUT SCREEN CODE.  Missing programmer access code '@'."
        . SET X=SCRNCODE
        . DO ^DIM  ;"kills X IF contains invalid mumps code
        . IF '$DATA(X) DO  QUIT
        . . SET TMGRESULT="-1^Invalid VA MUMPS code in INPUT(""FLD"",8,"_I_",""INPUT SCREEN CODE"").  Got: "_SCRNCODE
        . IF SCRNCODE'["DIC(""S"")" DO  QUIT
        . . SET TMGRESULT="-1^Missing 'DIC(""S"")' in INPUT(""FLD"",8,"_I_",""INPUT SCREEN CODE"").  Got: "_SCRNCODE
        . SET TMGVPTR(I,"SCRN")=SCRNCODE
        . NEW DESCR SET DESCR=$GET(TMGINFO("FLD",8,I,"INPUT SCREEN DESCRIPTION"))
        . IF DESCR="" DO  QUIT
        . . SET TMGRESULT="-1^Input screen code provided, but REQUIRED accompanying description missing from INPUT(""FLD"",8,"_I_",""INPUT SCREEN DESCRIPTION"")"
        . SET TMGVPTR(I,"SCRN DESCR")=DESCR
CHK8DN  QUIT
        ;
ADDERR(MSG) ;
        ;"Set an output error message
        NEW I SET I=+$ORDER(TMGOUT(""),-1)+1
        SET TMGOUT(I)=MSG
        IF +TMGRESULT>0 SET TMGRESULT="-1^Failed: "_MSG
        QUIT
        ;

