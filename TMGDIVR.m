TMGDIVR ;GFT -- TMG VERSION OF VERIFY FIELD DIFLD, DATA DICTIONARY A;3FEB2006, 1/18/2011, 2/2/14
        ;;1.0;TMG-LIB;**1**;6/24/15
        ;;22.0;VA FileMan;**7,999,1004,1014,1015**;Mar 30, 1999
        ;
        ;"Original file header below
        ;"DIVR(A,DIFLD,DQI) ;GFT -- TMG VERSION OF VERIFY FIELD DIFLD, DATA DICTIONARY A;3FEB2006, 1/18/2011
        ;" ;;22.0;VA FileMan;**7,999,1004,1014,1015**;Mar 30, 1999
        ;
        ;"Original file (as above) is Public Domain.  Modifications
        ;"made herein are Copyright Kevin Toppenberg MD
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
        ;"======================================================================
TMGDIVR0(TMGOUT,A,DIFLD,DQI) ;
        ;"Entry point for mod'd file.
        ;"PURPOSE OF MODIFICATON:  Change output from IO device into variable, and make non-interactive
        ;"Input: TMGOUT -- PASS BY REFERENCE.  Filled with output text
        ;"          TMGOUT(0)=Output Text 0
        ;"          TMGOUT(1)=Output Text 1
        ;"          TMGOUT(2)=Output Text 2
        ;"          TMGOUT(3)=Output Text 3
        ;"       A -- (file number)
        ;"       DIFLD --
        ;"       DQI --
        ;"Result: none
        ;"        However, IF no problems found then following SET:
        ;"          TMGOUT("RESULT")="OK"
        ;
        ;"-------------------------------------------------------------------
        ;"I $DATA(DIVFIL)[0 N DIVDAT,DIVFIL,DIVMODE,DIVPG,POP D  G:$G(POP) Q^DIV
        ;". S DIVMODE="C"
        ;". D DEVSEL^DIV Q:$G(POP)
        ;". D INIT^DIV
        NEW W,I,J,V,DIVREQK,DIVTYPE,DIVTMP,DG,DIVRIX,T,TYP,E,DDC,DIVZ,DE,DR,P4,M,DIDANGL
        SET TYP=$PIECE($G(^DD(A,DIFLD,0)),U,2)
        IF TYP="" GOTO QX  ;"WAS QUIT
        DO IJ^DIUTL(A)
        SET V=$ORDER(J(""),-1)
        FOR T="N","D","P","S","V","F" QUIT:TYP[T
        FOR W="FREE TEXT","SET OF CODES","DATE","NUMERIC","POINTER","VARIABLE POINTER","K" I TYP[$E(W) S:W="K" T=W,W="MUMPS" QUIT
        IF TYP["C" GOTO QX  ;"WAS QUIT
        DO WRITE("--"_$$LABEL^DIALOGZ(A,DIFLD)_"--  ("_W_")"_$CHAR(10))
        SET W="DO WRITE^TMGDIVR(""ENTRY#"_$S(V:"'S",1:"")_"   "_$$LABEL^DIALOGZ(A,.01)_"   ERROR""_$CHAR(10))"
        DO LF
        ;"Q:$DATA(DIRUT)
        SET T=$E(T),DIVZ=$P(^DD(A,DIFLD,0),U,3),DDC=$P(^(0),U,5,999),DR=$P(^(0),U,2),P4=$P(^(0),U,4)
        SET DIVREQK=$DATA(^DD("KEY","F",A,DIFLD))>9
        IF $DATA(^DD("IX","F",A,DIFLD)) DO
        . SET DIVTYPE=T,T="INDEX",DIVROOT=$$FROOTDA^DIKCU(A)
        . DO LOADVER^DIVC(A,DIFLD,"DIVTMP")
        FOR %=0:0 S %=$O(^DD(A,DIFLD,1,%)) QUIT:%'>0  IF $DATA(^(%,1)) DO
        . NEW X SET X=$P(^(0),U,2,9) QUIT:X'?1.A
        . IF ^(2)?1"K ^".E1")",^(1)?1"S ^".E DO
        . . SET DG(%)="I $DATA("_$E(^(2),3,99)_"),"_$E(^(1),3,99)
        . . IF 'V SET DIVRIX(X)="" ;"Only looks at top-level X-refs
UNIQ    . . IF DR["U",DIFLD=.01,X="B" SET DDC="K % M %="_DIU_"""B"",X) K %(DA) K:$O(%(0)) X I $DATA(X) "_DDC
        IF T'="INDEX",'$DATA(^(+$O(^DD(A,DIFLD,1,0)),1)) G E
        IF T'="INDEX",'$DATA(DG) DO WRITE("(CANNOT CHECK")
        ELSE  DO WRITE("(CHECKING")
        DO WRITE(" CROSS-REFERENCE)")
        DO LF
        ;"I $DATA(DIRUT) Q:$DATA(DQI)  G Q
        IF $DATA(DG) DO
        . IF T="INDEX" SET E=DIVTYPE,DIVTYPE="IX"
        . ELSE  SET E=T,T="IX"
E       FOR Y=$F(DDC,"%DT="""):1 SET X=$E(DDC,Y) QUIT:""""[X  IF X="E" SET $E(DDC,Y)="" QUIT  ;Take out "E"
        IF DR["*" DO
        . SET DDC="Q"
        . IF '$DATA(^DD(A,DIFLD,12.1)) QUIT
        . XECUTE ^(12.1)
        . IF '$DATA(DIC("S")) QUIT
        . SET DDC(1)=DIC("S")
        . SET DDC="X DDC(1) E  K X"
        DO 0
        SET X=P4,Y=$P(X,";",2),X=$P(X,";")
        IF +X'=X SET X=""""_X_"""" IF Y="" SET DE=DE_"S X=DA D R" GOTO XEC
        SET DIDANGL="S X=$S($DATA(^(DA,"_X_")):$"_$S(Y:"P(^("_X_"),U,"_Y,1:"E(^("_X_"),"_$E(Y,2,9))_"),1:"""")"
        SET M=DIDANGL_" D R"
        IF $L(M)+$L(DE)>250 SET DE=DE_"X DE(1)",DE(1)=M
        ELSE  SET DE=DE_M
XEC     KILL DIC,M,Y
        XECUTE DE
 ;
DANGL   SET DIVRIX="A"
        FOR  SET DIVRIX=$O(DIVRIX(DIVRIX)) Q:DIVRIX=""  DO  ;"LOOK FOR BAD CROSS-REFERENCES
        . NEW IX,SN,SX,DA
        . SET IX=I(0)_""""_DIVRIX_""")",SN=$QL(IX)
        . KILL ^UTILITY("DIVRIX",$J)
        . FOR  S IX=$Q(@IX) Q:IX=""  Q:$QS(IX,SN)'=DIVRIX  D
        . . IF @IX]"" Q
        . . SET DA=$QS(IX,SN+2),SX=" CROSS-REF '"_$QS(IX,SN+1)_"'"
        . . IF '$DATA(@(I(0)_DA_")")) S M="DANGLING"_SX D X Q
        . . XECUTE DIDANGL
        . . IF $E($QS(IX,SN+1),1,30)'=$E(X,1,30) DO  QUIT
        . . . S M="WRONG"_SX DO X
        . . IF $DATA(^UTILITY("DIVRIX",$J,DA)) DO
        . . . S M="DUPLICATE"_SX DO X
        . . SET ^(DA)=""
        IF $DATA(DQI) GOTO QX  ;"QUIT
        IF '$DATA(M) DO
        . DO WRITE("  "_$CHAR(10)_"NO PROBLEMS")
        . SET TMGOUT("RESULT")="OK"
Q       SET M=$O(^UTILITY("DIVR",$J,0)),E=$O(^(M)),DK=J(0)
        ;"IF $DATA(ZTQUEUED) SET ZTREQ="@"
        ;"ELSE  I $T(^%ZISC)]"" D
        ;". D ^%ZISC
        ;"E  X $G(^%ZIS("C"))
        GOTO QX  ;"//kt skip saving options
        ;"IF 'E!$DATA(DIRUT)!$DATA(ZTQUEUED) GOTO QX
        ;"KILL DIBT,DISV
        ;"DO
        ;". NEW C,D,I,J,L,O,Q,S,D0,DDA,DICL,DIFLD,DIU0
        ;". W ! D S2^DIBT1 Q  ;STORE ENTRIES IN TEMPLATE??
        ;"SET DDC=0
        ;"I $DATA(DIRUT) GOTO Q2
        ;"IF Y<0 GOTO Q
        ;"FOR E=0:0 SET E=$O(^UTILITY("DIVR",$J,E)) Q:E=""  DO
        ;". SET DDC=DDC+1,^DIBT(+Y,1,E)=""
Q2      ;"IF DDC>0 SET ^DIBT(+Y,"QR")=DT_U_DDC
QX      K DIVINDEX,DIVKEY,DIVREQK,DIVROOT,DIVTMP,DIVTYPE
        K ^UTILITY("DIVR",$J),^UTILITY("DIVRIX",$J),DIRUT,DIROUT,DTOUT,DUOUT,DK,DQ,P,DR
        QUIT  ;"universal exit point from main routine.
 ;
R       Q:$DATA(DIRUT)
        I X?." " Q:DR'["R"&'DIVREQK  D  G X
        . I X="" S M="Missing"_$S(DIVREQK:" key value",1:"")
        . E  S M="Equals only 1 or more spaces"
        G @T
 ;
P       I @("$DATA(^"_DIVZ_"X,0))") S Y=X G F
        S M="No '"_X_"' in pointed-to File" G X
 ;
S       S Y=X X DDC I '$DATA(X) S M=""""_Y_""" fails screen" G X
        Q:";"_DIVZ[(";"_X_":")  S M=""""_X_""" not in Set" G X
 ;
D       S X=$$DATE^DIUTL(X) ;**
N ;
K ;
F       SET DQ=X
        IF X'?.ANP S M="Non-printing character" G X
        XECUTE DDC
        IF $DATA(X) QUIT
        SET M=""""_DQ_""" fails Input Transform"
X       I $O(^UTILITY("DIVR",$J,0))="" DO
        . XECUTE W
        S X=$S(V:DA(V),1:DA),^UTILITY("DIVR",$J,X)=""
        S X=V
        IF @(I(0)_"0)")
DA      IF 'X DO  QUIT
        . DO LF
        . Q:$DATA(DIRUT)
        . DO WRITE(DA)
        . DO PAD2(10)
        . DO WRITE($S($DATA(^(DA,0)):$E($P(^(0),U),1,30),1:DA))
        . DO PAD2(40)
        . DO WRITE($E(M,1,IOM-40))
        . IF V DO LF
        DO LF
        Q:$DATA(DIRUT)
        DO WRITE(DA(X))
        DO PAD2(10)
        DO WRITE($S($G(^(DA(X),0))]"":$P(^(0),U),1:"***NO ZERO NODE***"))
        SET X=X-1
        SET @("Y=$DATA(^("_I(V-X)_",0))")
        GOTO DA  ;"<--- does this cause endless loop??
 ;
 ;
0 ;
        SET Y=I(0),DE="",X=V
L       SET DA="DA"
        IF X SET DA=DA_"("_X_")"
        SET Y=Y_DA
        SET DE=DE_"F "_DA_"=0:0 "
        SET %="S "_DA_"=$O("_Y_"))"
        IF V>2 DO
        . SET DE(X+X)=%
        . SET DE=DE_"X DE("_(X+X)_")"
        ELSE  SET DE=DE_%
        SET DE=DE_" Q:"_DA_"'>0  S D"_(V-X)_"="_DA_" "
        ;";I X=1,DIFLD=.01 S DE=DE_"X P:$DATA(^(DA(1),"_I(V)_",0)) ",P="S $P(^(0),U,2)="""_$P(^DD(J(V-1),P,0),U,2)_Q
        SET X=X-1
        IF X'<0 DO  GOTO L
        . SET Y=Y_","_I(V-X)_","
        QUIT
 ;
 ;
IX      ;"F %=0:0 S %=$O(DG(%)) Q:+%'>0  X DG(%) I '$T S M=""""_X_""" not properly Cross-referenced" G X
        SET M=""
        FOR %=0:0 S %=$O(DG(%)) Q:+%'>0  DO  IF M'="" GOTO X
        . XECUTE DG(%)
        . I $T QUIT
        . SET M=""""_X_""" not properly Cross-referenced"
        GOTO @E
 ;
V       IF $P(X,";",2)'?1A.AN1"(".ANP,$P(X,";",2)'?1"%".AN1"(".ANP DO  GOTO X
        . SET M=""""_X_""""_" has the wrong format"

        SET M=$S($DATA(@(U_$P(X,";",2)_"0)")):^(0),1:"")
        IF '$DATA(^DD(A,DIFLD,"V","B",+$P(M,U,2))) DO  GOTO X
        . SET M=$P(M,U)_" FILE not in the DD"
        IF '$DATA(@(U_$P(X,";",2)_+X_",0)")) S M=U_$P(X,";",2)_+X_",0) does not exist" G X
        GOTO F
 ;
INDEX ;Check NEW indexes
        ;
        ;Set DIVINDEX(indexName,index#) = "" for indexes aren't set
        ;Set DIVKEY(file#,keyName,uiNumber) = "null" : IF key field is null
        ;                                     "uniq" : IF key is not unique
        KILL DIVKEY,DIINDEX
        DO VER^DIVC(A,DIVROOT,.DA,"DIVTMP",.DIVINDEX,.DIVKEY)
        ;
        ;If some indexes aren't SET properly, print index info
        I $DATA(DIVINDEX) D  K DIVINDEX Q:$DATA(DIRUT)
        . N DIVNAME,DIVNUM
        . S DIVNAME="" F  S DIVNAME=$O(DIVINDEX(DIVNAME)) Q:DIVNAME=""  D  Q:$DATA(DIRUT)
        .. S DIVNUM=0 F  S DIVNUM=$O(DIVINDEX(DIVNAME,DIVNUM)) Q:'DIVNUM  D  Q:$DATA(DIRUT)
        ... S M=""""_X_""": "_DIVNAME_" index (#"_DIVNUM_") not properly set"
        ... D IER
        ;
        ;If keys integrity is violated, print key info
        I $DATA(DIVKEY) D  K DIVKEY Q:$DATA(DIRUT)
        . N DIVFILE,DIVKNM,DIVPROB,DIVXRNM
        . S DIVFILE="" F  S DIVFILE=$O(DIVKEY(DIVFILE)) Q:DIVFILE=""  D  Q:$DATA(DIRUT)
        .. S DIVKNM="" F  S DIVKNM=$O(DIVKEY(DIVFILE,DIVKNM)) Q:DIVKNM=""  D  Q:$DATA(DIRUT)
        ... S DIVXRNM="" F  S DIVXRNM=$O(DIVKEY(DIVFILE,DIVKNM,DIVXRNM)) Q:DIVXRNM=""  D  Q:$DATA(DIRUT)
        .... S DIVPROB=DIVKEY(DIVFILE,DIVKNM,DIVXRNM)
        .... S M=""""_X_""": "_$S(DIVPROB="null":"Key values are missing.",1:"Key is not unique.")
        .... S M=M_" (File #"_DIVFILE_", Key "_DIVKNM_", Index "_DIVXRNM_")"
        .... D IER
        ;
        ;Continue with checking traditional xrefs (if any) and data type
        GOTO @DIVTYPE
        ;
IER     ;Print info about invalid indexes. (Modeled after DA subroutine above)
        NEW DIVTXT,DIVI,X
        ;
        ;Wrap message M to within 40 columns
        SET DIVTXT(0)=M
        DO WRAP^DIKCU2(.DIVTXT,40)
        ;
        ;If nothing was written yet, WRITE column headers
        IF $O(^UTILITY("DIVR",$J,0))="" XECUTE W
        ;
        ;Set ^UTILITY("DIVR",$J,topIen)="", X = level#, naked = top level root
        SET X=$S(V:DA(V),1:DA)
        SET ^UTILITY("DIVR",$J,X)=""
        SET X=V
        IF @(I(0)_"0)")
 ;
IER1    ;"If top level, WRITE record info and message
        IF 'X DO  QUIT
        . DO LF
        . Q:$DATA(DIRUT)
        . DO WRITE(DA)
        . DO PAD2(10)
        . DO WRITE($S($DATA(^(DA,0)):$P(^(0),U),1:DA))
        . FOR DIVI=0:1 Q:$DATA(DIVTXT(DIVI))[0  D  Q:$DATA(DIRUT)
        .. IF DIVI DO LF Q:$DATA(DIRUT)
        .. DO PAD2(40)
        .. DO WRITE(DIVTXT(DIVI))
        . IF V DO LF
        ;
        ;Else WRITE subrecord info, decrement level, SET naked = ^naked(node,0)
        DO LF
        QUIT:$DATA(DIRUT)
        DO WRITE(DA(X))
        DO PAD2(10)
        DO WRITE($P(^(DA(X),0),U))
        SET X=X-1
        SET @("Y=$DATA(^("_I(V-X)_",0))")
        GOTO IER1
        ;
LF      ;Issue a line feed or EOP read
        DO WRITE($CHAR(10))
        QUIT
        ;"----------------
        IF $Y+3<IOSL W ! Q
        ;
        N DINAKED
        S DINAKED=$$LGR^%ZOSV
        I IOST?1"C-".E D
        . N DIR,X,Y
        . S DIR(0)="E" W ! D ^DIR
        ;
        I '$DATA(DIRUT) D
        . I $DATA(ZTQUEUED),$$S^%ZTLOAD S (ZTSTOP,DIRUT)=1
        . E  W @IOF D HDR
        S:DINAKED]"" DINAKED=$S(DINAKED["""""":$O(@DINAKED),1:$DATA(@DINAKED))
        QUIT
 ;
HDR ;Print header
        NEW DIVTAB
        SET DIVPG=$G(DIVPG)+1
        DO WRITE("VERIFY FIELDS REPORT")
        ;
        SET DIVTAB=IOM-1-$L(DIVFIL)-$L(DIVDAT)-$L(DIVPG)
        IF DIVTAB>1 DO
        . DO LF
        . DO WRITE(DIVFIL_$J("",DIVTAB)_DIVDAT_DIVPG)
        ELSE  DO
        . DO LF
        . DO WRITE(DIVFIL_$CHAR(10)_$J("",IOM-1-$L(DIVDAT)-$L(DIVPG))_DIVDAT_DIVPG)
        DO LF
        DO WRITE($TR($J("",IOM-1)," ","-")_$CHAR(10))
        QUIT
        ;
PAD2(POS) ;
        ;"Purpose: pad current output line to length
        NEW CURLIN SET CURLIN=+$GET(TMGOUT)
        NEW TMPS SET TMPS=$GET(TMGOUT(CURLIN))
        FOR  QUIT:$LENGTH(TMPS)'<POS  SET TMPS=TMPS_" "
        SET TMGOUT(CURLIN)=TMPS
        QUIT
        ;
WRITE(NEWS) ;
        ;"Input: S -- text to ouput to var"
        ;
        NEW CURLIN SET CURLIN=+$GET(TMGOUT)
        NEW LF SET LF=NEWS[$CHAR(10)
        NEW PCS SET PCS=$LENGTH(NEWS,$CHAR(10))
        NEW I FOR I=1:1:PCS DO
        . NEW TMPS SET TMPS=$PIECE(NEWS,$CHAR(10),I)
        . SET TMGOUT(CURLIN)=$GET(TMGOUT(CURLIN))_TMPS
        . IF TMPS="",I=PCS QUIT
        . IF LF SET CURLIN=CURLIN+1
        SET TMGOUT=CURLIN
        QUIT
        ;
