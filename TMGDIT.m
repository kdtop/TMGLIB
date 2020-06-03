TMGDIT  ;SFISC/GFT-GET XFR ANSWERS ;4/6/94  13:03, 2/2/14
        ;;1.0;TMG-LIB;**1**;6/24/15
        ;;22.0;VA FileMan;;Mar 30, 1999
 ;"Copied from FM, for decompiling and modifications
 ;"Revised interface added at end.

 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;


0       S DIC="^DOPT(""DIT"","
        G OPT:$D(^DOPT("DIT",2))
        S ^(0)="TRANSFER OPTION^1.01"
        K ^("B")
        F X=1,2 S ^DOPT("DIT",X,0)=$P("TRANSFER^COMPARE/MERGE",U,X)_" FILE ENTRIES"
        S DIK=DIC
        D IXALL^DIK
OPT     W !!
        S DIC(0)="AEQZI"
        ;"//ktD ^DIC  ;"do menu asking for options --> Y=1^Transfer...
        S Y=1 ;"//kt added
        G Q:Y<0
        I +Y=2 D ^DITM K DIC G 0   ;"2 --> compare/MERGE 2 entries
        D Q  ;"cleanup local variables.
        S DLAYGO=1  ;"allow addition of NEW file, IF wanted
        D W^DICRW    ;"ask 'INPUT TO WHAT FILE?'  Y --> 200^NEW PERSON...
        G Q:$D(DTOUT)
        Q:Y<0
        S DFL=$P(Y,U,2)_": "
        I '$D(DIC) DO  Q:'$D(DG)  G FROM
        . D DIE^DIB
        . Q:'$D(DG)
        . S L=DG,Y=DLAYGO
        . K DG,DIE,DQ
        S DIC("B")=+Y   ;"save destination file number
        S L=DIC
        ;
FROM    S DMRG=1,DKP=1,(DDF(1),DDT(0))=+Y,DIC=1,DIC(0)="EQAZ",DIC("A")="TRANSFER FROM FILE: "
        S DIC("S")="S DIFILE=+Y,DIAC=""RD"" D ^DIAC I %"
        D ^DIC  ;"ask 'Transfer FROM which file?  Y --> 200^Newperson...
        K DIC
        G Q:Y<0
        G Q:'$D(^(0,"GL"))
        S DTO=^("GL")   ;"DTO = root of source file
        I DUZ(0)'="@",$S($D(^VA(200,DUZ,"FOF",+Y,0)):1,1:$D(^DIC(3,DUZ,"FOF",+Y,0))) G DTR:+$P(^(0),U,3),Q
        I DUZ(0)'="@",$D(^DIC(+Y,0,"DEL")) F X=1:1 G Q:X>$L(^("DEL")) Q:DUZ(0)[$E(^("DEL"),X)
DTR     D PTS   ;"get pointers into  ^UTILITY("DIT",$J,0,#)=File^Field^FieldDefInfo
        I +Y=DDF(1)
        G ^TMGDIT0  ;"prepare to transfer
        ;"
        ;"=============================================================
TWO     S (DTO(0),F)=L,L(+Y)=DDT(0),L=0,DDF(1)=+Y,DFR(1)=DTO_"D0,",DHIT=DLAYGO-(Y#1),%=0
        W !!
        K ^UTILITY("DITR",$J),A
        I DLAYGO-1 DO  G Q:%<1
        . W "DO YOU WANT TO TRANSFER THE '",$P(Y,U,2),"'",!
        . W "DATA DICTIONARY INTO YOUR NEW FILE"
        . D YN^DICN
        . QUIT:%<1
        . D ^DIT1:%=1

        K DITF,Y,B
        W !
        G Q:'$D(L)
        D MAP
        I '$D(DITF) W $C(7),"FILES DON'T MATCH!" G Q
        W:$X>40 ! W:'$D(A) "  WILL BE TRANSFERRED",!!
        S %=2,DMRG=0
        I @("$O("_DTO(0)_"0))>0") W !,"WANT TO MERGE TRANSFERRED ENTRIES WITH ONES ALREADY THERE" D YN^DICN G Q:%<1 I %=1 S DMRG=1
        S (DIK,DIC)=DTO,DTO=1,L="TRANSFER ENTRIES",FLDS="",DHD="@",%ZIS="F"
D       S %=0
        W !,"WANT EACH ENTRY TO BE DELETED AS IT'S TRANSFERRED"
        D YN^DICN
        S DHIT="S DI=99 D F^DITR"_$P(",^DIK",%,%=1)
        G Q:%<0
        I '% D F G D
        S DISTOP=0
        s DIOEND="S DIK=DTO(0),DIK(0)=""B"" D KL^DIT,IXALL^DIK,Q^DIT"
        D EN1^DIP
Q       ;
        K ^UTILITY("DITR",$J),^UTILITY("DIT",$J),DIT,DIC,DA,DB1,DFR,DIK,L,FLDS,DHIT,DISTOP,DIOEND,%ZIS
KL      K DIU,DIV,DIG,DIH,DLAYGO,DITF,DFN,DMRG,DTO,DTN,DDF,DTL,DFL,DDT,A,B,DKP,W,X,FLDS,Y,Z
        Q
        ;
MAP     ;"BUILD MAP OF FIELDS FROM 'FROM' TO 'TO' FILE
        N DFL
        S DFL=1
MAP2    ;"ENTRY POINT FROM ^DIT3
        K:L]"" L(L)
        S L=$O(L(0))
        Q:L']""
        F Y=0:0 S Y=$O(^DD(L,Y)) G MAP2:Y="",MAP2:'$D(^(Y,0)) S %=^(0) I $P(%,U,2)'["C" S DIC=$P(%,U,1),X=$O(^DD(L(L),"B",DIC,0)) I X>0,'^(X),$P(^DD(L(L),X,0),U,2)'["C" D T
        Q
        ;"
T       S Z=$P(^(0),U,4)
        S V=$P($P(^(0),U,2),U,Z[";0")
        S ^UTILITY("DITR",$J,L,Y)=$P(Z,";",2)_U_$P(Z,";",1)
        S:V ^(Y)=^(Y)_U_V,L(+$P(%,U,2))=+V
        I Z="0;1",DDF(DFL)=L
        S DITF=$P(%,U,4)
        Q:$D(A)
        W:$X ", "
        W:$L(DIC)+$X>66 !
        W "'"_DIC_"' FIELDS"
        Q
        ;
PTS     ;
        ;"Loads up ^UTILITY('DIT',$J) with pointers out (I think)
        ;"Input: Y as filenumber
        S DL=0
        F X=0:0 S X=$O(^DD(+Y,0,"PT",X)) Q:X'>0  do
        . F Z=.001:0 S Z=$O(^DD(+Y,0,"PT",X,Z)) Q:Z'>0  do
        . . I '$D(^DD(X,Z,0))#2 QUIT
        . . S %=^(0)
        . . I '(U_$P(%,U,3)=DTO!($D(^DD(X,Z,"V","B",+Y)))) QUIT
        . . I $P(%,U,2)'["I" do
        . . . S DL=DL+1
        . . . S ^UTILITY("DIT",$J,0,DL)=X_U_Z_U_$P(%,U,2)
        Q
        ;
F       W !?7,"(TYPE '^' TO FORGET THE WHOLE THING!)",!
        Q
        ;
        ;"=============================================================
        ;"=============================================================
TRNMRG(DIFLG,DIFFNO,DITFNO,DIFIEN,DITIEN)
        ;"Purpose: SILENT TRANSFER/MERGE OF SINGLE RECORDS IN FILE OR SUBFILE
        ;"Input:
        ;"      DIFLG  = FLAGS
        ;"      DIFFNO = TRANSFER 'FROM' FILE/SUBFILE NO. OR ROOT
        ;"      DITFNO = TRANSFER 'TO' FILE/SUBFILE NO.
        ;"      DIFIEN = TRANSFER 'FROM' IEN STRING
        ;"      DITIEN = TRANSFER 'TO' IEN STRING (PASS BY REFERENCE)
        ;"
        ;"//kt According to post from George Timpson, here is the
        ;"     definations for FLAGS:
        ;" "M"  Merge the two entries. --  If a field exists on the target
        ;"              record, DO not WRITE over it)
        ;" "O"  Overwrite the MERGE 'TO' entry. (if a field exists on the
        ;"      MERGE 'from' record, it overwrites the same field on the target record)
        ;" "A"  Add the NEW record.  This does not look for a match, but always
        ;"      adds the MERGE 'from' record as a NEW entry in the target file.
        ;" "R"  Replace.  (Used only for package installation).  In ^DIT
        ;"      and ^DIT3, this works the same as 'O'.  In replace mode, IF a match
        ;"      is found  between a record coming in with the installation and a
        ;"      record on the target system, the record on the target system is
        ;"      deleted, except for any locally developed fields.  We haven't yet
        ;"      worked out the details of how this will be done.  We may be setting
        ;"      a NEW internal flags to pass to ^DITR, based on this flag containing
        ;"      an "R".  It seems to me that ^DITR would be the place to delete the
        ;"      existing record, IF a match is found.
        ;" "X"  If this optional flag is included, the routine will run the SET
        ;"      cross-reference logic on the MERGEd 'TO' record, after it has been
        ;"      installed.
        ;"DIFFNO = TRANSFER 'FROM' FILE/SUBFILE NUMBER OR ROOT (OPTIONAL)
        ;"        If not passed, then value of DIFTNO is used.      
        ;"DITFNO = TRANSFER 'TO' FILE/SUBFILE NUMBER
        ;"DIFIEN = TRANSFER 'FROM' IEN STRING (IENS)
        ;"DITIEN = TRANSFER 'TO' IEN STRING (IENS) (PASS BY REFERENCE)
        ;"          To specify a specific destination, pass full IENS, e.g. "56," or "4,123,"
        ;"          To specify a NEW destination record, pass "+?n,", e.g. "+?1,"
        ;"            DITIEN will be changed to actual IEN record was store in
        ;"            DIFLG should be "A" when using "+?n," format
        G TRNMRG^DIT3

        ;"=============================================================
        ;"=============================================================

XFERREC(File,FromIEN,ToIEN)
        ;"Purpose:
        ;"Input: FromFile -- IEN of file of source

        NEW DIC
        D Q2  ;"cleanup local variables.
        SET File=+$GET(File)
        IF File'>0 QUIT
        SET ToIEN=+$GET(ToIEN)  ;"not supporting IENS right now.
        IF ToIEN'>0 QUIT
        SET FromIEN=+$GET(FromIEN)
        IF FromIEN'>0 QUIT
        S L=$GET(^DIC(File,0,"GL"))
        IF L="" QUIT

        S DMRG=1,DKP=1
        S (DDF(1),DDT(0))=File
        S DTO=L   ;"DTO = root of source file
        ;"D PTS   ;"get pointers into  ^UTILITY("DIT",$J,0,#)=File^Field^FieldDefInfo

        ;"=============================================

        K DIC
        S DIC=L
        SET Y=ToIEN ;"//kt

        S DFR=ToIEN
        S DTO(1)=L_ToIEN_","  ;"DTO is DESTINATION info array
        SET Y=FromIEN  ;"//kt

        S ^UTILITY("DIT",$J,FromIEN)=ToIEN_";"_$E(L,2,999)
        S DTO=0
        S (D0,DA)=+Y
        S DIK=L
        S DFR(1)=L_DA_","
        K DIC

GO      D GO^DITR  ;"Find fields to XRef

        D KL^TMGDIT
        S DA=ToIEN
        K DFR
        D IX1^DIK  ;"input: DIK (Root of file), DA (used as IEN)

Q2      G Q^TMGDIT
        ;
