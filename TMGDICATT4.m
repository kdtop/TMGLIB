TMGDICATT4 ;KST/SFISC/XAK- TMG VERSION OF DELETE A FIELD ;6 JAN,2011, 2/2/14
         ;;1.0;TMG-LIB;**1**;1/6/11
         ;
         ;"Original file header below
         ;DICATT4 ;SFISC/XAK-DELETE A FIELD ;12:39 PM  7 Mar 2002
         ;";;22.0;VA FileMan;**26,52,82,106**;Mar 30, 1999
         ;
         ;"Original file (as above) is Public Domain.  Modifications
         ;"made herein are Copyright Kevin Toppenberg MD Jan 6, 2011
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
DIEZ    SET DI=A,DA=D0
        DO
        . NEW ZTQUEUED SET ZTQUEUED=1 ;"allows silence
        . DO DIPZ^DIU0
        KILL ^DD(A,0,"ID",D0),^DD(A,0,"SP",D0)
EN      IF $O(@(I(0)_"0)"))>0 DO
        . NEW X,T,Y,Z,MUL
        . SET MUL=+$P(O,U,2)
        . SET %=1
        . SET Y=$P(O,U,4)
        . SET X=$P(Y,";")
        . SET Y=$P(Y,";",2)
        . SET Z=$S(+X=X:X,1:""""_X_"""")_")"
        . SET E="^("_Z
        . IF $O(^DD(A,"GL",X,""))="" DO  GOTO F
        . . SET T="K ^(M,"_Z
        . IF Y DO
        . . SET T="U_$P("_E_",U,"_(Y+1)_",999) K:"_E_"?.""^"" "_E
        . . SET:Y>1 T="$P("_E_",U,1,"_(Y-1)_")_U_"_T
        . ELSE  DO  Q:'X!'Y
        . . SET X=+$E(Y,2,4),Y=+$P(Y,",",2)
        . . QUIT:'X!'Y
        . . SET T="$E("_E_",1,"_(X-1)_")_$J("""","_(Y-X+1)_")_$E("_E_","_(Y+1)_",999)"
        . SET T="I $D(^(M,"_Z_")#2 S "_E_"="_T
F       . ;"IF '$D(DIU(0)) W $C(7),!,"OK TO DELETE '",$P(M,U),"' FIELDS IN THE EXISTING ENTRIES" D YN^DICN I %-1 D:'$D(DIU) DELXRF(A,D0) Q
        . IF '$D(DIU(0)) DO  QUIT:(%'=1)!(TMGRESULT<1)
        . . NEW TMGDEL,TMGDEL0 SET (TMGDEL0,TMGDEL)=$GET(TMGINFO("DELETE DATA"),"YES")
        . . IF $$CHKYN^TMGDICATTS(.TMGDEL)=0 DO  QUIT
        . . . SET TMGRESULT="-1^Invalid answer for INPUT(""DELETE DATA"").  Got '"_TMGDEL0_"'"
        . . ;"W $C(7),!,"OK TO DELETE '",$P(M,U),"' FIELDS IN THE EXISTING ENTRIES"
        . . ;"DO YN^DICN
        . . SET %=$SELECT(TMGDEL="Y":1,(1=1):2)
        . . IF %=1 QUIT
        . . IF '$D(DIU) DO DELXRF(A,D0)
KILLIX  . IF $D(DICATT4M) DO
        . . DO INDEX^DIKC(J(0),"","","","KiRW"_MUL)
        . . SET M=""
        . . FOR  SET M=$O(^DD(J(0),0,"IX",M)) QUIT:M=""  DO
        . . . IF $O(^(M,MUL,0)) KILL @(I(0)_""""_M_""")")
        . ELSE  DO
        . . DO:'$D(DIU) DELXRF(A,D0,1,J(0))
        . SET M="",X=DICL,Y=I(0)
        . IF $D(DQI) K @(I(0)_""""_DQI_""")")
L       . SET O="M"
        . IF X SET O=O_"("_X_")"
        . SET Y=Y_O
        . SET M=M_"F "_O_"=0:0 S "_O_"=$O("_Y_")) Q:"_O_"'>0  "
        . SET X=X-1
        . IF X+1 DO  GOTO L
        . . SET Y=Y_","_I(DICL-X)_","
        . SET M=M_"XECUTE T"
        . ;"SET M=M_$P(" W "".""",U,$S('$D(DIU(0)):1,DIU(0)["E":1,1:0))
        . XECUTE M ;"HERE'S THE LOOP WHERE WE KILL THE VALUES!
N       QUIT:$D(DIU)!$D(DICATT4M)
        GOTO N^TMGDICATT
 ;
NEW     ;"Delete the data in the multiple
        SET DICATT4M=$NA(^DD(A,D0))
        SET DICATT4M("SB")=$NA(^DD(A,"SB",+$P(O,U,2),D0))
        SET ^DD(A,D0,0)=O
        SET ^DD(A,"SB",+$P(O,U,2),D0)=""
        DO TMGDICATT4
        KILL @DICATT4M
        KILL @DICATT4M("SB")
        KILL DICATT4M
        ;
        ;"Kill the DD globals and go back to N^DICATT
        DO KDD
        GOTO N^TMGDICATT
 ;
VP      ;" VARIABLE POINTER
        SET DA(2)=DA(1),DA(1)=DA,DICATT=DA
        IF $D(DICS) S DICSS=DICS K DICS
V       NEW TMGI SET TMGI=""
        FOR  SET TMGI=$ORDER(TMGVPTR(TMGI)) QUIT:(TMGI="")!(TMGRESULT<0)  DO
        . NEW DIE,DIC,X,Y,DR
        . SET DA(2)=A
        . SET DA(1)=DICATT
        . SET DIC="^DD("_A_","_DICATT_",""V"","
        . SET DIC("P")=".12P"  ;"subfile number and specifier code
        . SET DIC(0)="MLI"
        . NEW TMGP2F SET TMGP2F=TMGVPTR(TMGI,"PTR")
        . SET X=TMGP2F
        . DO ^DIC  ;"Add NEW 'subfile' entry in "V" pseudofile.  If successful returns IEN^.01Field^1
        . IF Y<0 DO  QUIT
        . . SET TMGRESULT="-1^Problem adding VARIABLE-POINTER to: "_TMGVPTR(TMGI,"PTR")
        . SET DIE=DIC
        . KILL DIC
        . SET DA=+Y
        . SET Z="P"
        . SET DR=".02///"_TMGVPTR(TMGI,"MSG")_";"
        . SET DR=DR_".03///"_TMGI_";"   ;"Sequence #
        . SET DR=DR_".04///"_TMGVPTR(TMGI,"PREFIX")_";"
        . SET DR=DR_".06///"_TMGVPTR(TMGI,"LAYGO")_";"  ;""DI" node already screened for in CHK8^TMGDICATT
        . IF $DATA(TMGVPTR(TMGI,"SCRN")) DO
        . . SET DR=DR_".05///y;"
        . . SET DR=DR_"1///"_TMGVPTR(TMGI,"SCRN")_";"
        . . SET DR=DR_"2///"_TMGVPTR(TMGI,"SCRN DESCR")_";"
        . DO ^DIE ;"Store values.
        KILL TMGI
        GOTO V2
        ;
V0      ;"Older code from V0 up to v2, cut out and replaced with above.  Original label here was V
        SET DA(2)=A
        SET DA(1)=DICATT
        SET DIC="^DD("_A_","_DICATT_",""V"","
        SET DIC("P")=".12P"  ;"subfile number and specifier code
        SET DIC(0)="QEAMLI"
        SET DIC("W")="W:$S($D(^DIC(+^(0),0)):$P(^(0),U)'=$P(^DD(DA(2),DA(1),""V"",+Y,0),U,2),1:0) ?30,$P(^(0),U,2)"
        DO ^DIC
        SET DIE=DIC
        KILL DIC
        IF Y>0 DO
        . SET DA=+Y
        . SET Z="P"
        . SET DR=".01:.04;"  ;".01=VARIABLE-POINTER. ...  .04=PREFIX
        . NEW TMP SET TMP=$P($G(^DD(+$P(Y,U,2),0,"DI")),U,2)
        . SET DR=DR_$S(TMP["Y":".06///n",1:".06T")_";"  ;".06=LAYGO
        . SET DR=DR_"S:DUZ(0)'=""@"" Y=0;"
        . SET DR=DR_".05;"  ;".05=SHOULD ENTRIES BE SCREENED
        . SET DR=DR_"I ""n""[X K ^DD(DA(2),DA(1),""V"",DA,1),^(2) S Y=0;"
        . SET DR=DR_"S DIE(""NO^"")=""BACK"";"
        . SET DR=DR_"1;2;" ;"1=SCREEN  2=EXPLAINATION OF SCREEN.
        . IF $P(Y,U,3) SET DIE("NO^")=""
        IF Y'>0 GOTO V2
        DO ^DIE
        KILL DIE
        W !
        S:$D(DTOUT) DA=DICATT
        GOTO CHECK^DICATT:$D(DTOUT)
        GOTO V
        ;
V2      SET Z="V^",DIZ=Z,C="Q",L=18,DA=DICATT
        SET DA(1)=A
        IF $D(DICSS) SET DICS=DICSS
        KILL DICSS,DR,DIE,DA(2),DICATT
        IF $D(DTOUT)!(X=U) GOTO CHECK^TMGDICATT
        GOTO ^TMGDICATT1
        Q
 ;"HELP ;
        ;"W !?5,"Enter a MUMPS statement that sets DIC(""S"") to code that sets $T."
        ;"W !?5,"Those entries for which $T=1 will be selectable."
        ;"I Z?1"P".E D  Q
        ;". W !?5,"The naked reference will be at the zeroeth node of the pointed to"
        ;". W !?5,"file, e.g., ^DIZ(9999,Entry Number,0).  The internal entry number"
        ;". W !?5,"of the entry that is being processed in the pointed to file will be"
        ;". W !?5,"in the variable Y."
        ;"W !?5,"The variable Y will be equal to the internally-stored code of the item"
        ;"W !?5,"in the SET which is being processed."
       ;" Q
KDD ;
        I '$D(DIANC) S X=A F  S DIANC(X)="" Q:$D(^DD(X,0,"UP"))[0  S X=^("UP")
        S DQ=$O(DQ(0)),X=0 I DQ="" S DQ=-1 K DIANC Q
        D KIX(.DIANC,DQ)
        F  S X=$O(^DD(DQ,"SB",X)) Q:'X  S DQ(X)=0
        N DIFLD S DIFLD=0 F  S DIFLD=$O(^DD(DQ,DIFLD)) Q:'DIFLD  D
        . I $D(^DD(DQ,DIFLD,9.01)) S X=^(9.01),Y=DIFLD D KACOMP
        . D KTRB(.DIANC,DQ,DIFLD)
        . S X=$P($G(^DD(DQ,DIFLD,0)),U,2) I X'["P",X'["V" Q
        . I X["P" S X=+$P(X,"P",2) K:X ^DD(X,0,"PT",DQ,DIFLD) Q
        . F %=0:0 S %=$O(^DD(DQ,DIFLD,"V",%)) Q:'%  S X=+$G(^(%,0)) K:X ^DD(X,0,"PT",DQ,DIFLD)
        . Q
        K DQ(DQ),^DD(DQ),^DD("ACOMP",DQ),^DDA(DQ)
        S Y=0 F  S Y=$O(DIANC(Y)) Q:'Y  K ^DD(Y,"TRB",DQ)
        D DELXR(DQ)
        S Y=0 F  S Y=$O(^DIE("AF",DQ,Y)) Q:Y=""  S %=0 F  S %=$O(^DIE("AF",DQ,Y,0)) Q:%=""  K ^(%),^DIE(%,"ROU")
        S Y=0 F  S Y=$O(^DIPT("AF",DQ,Y)) G KDD:Y="" S %=0 F  S %=$O(^DIPT("AF",DQ,Y,0)) Q:%=""  K ^(%),^DIPT(%,"ROU")
 ;
KIX(DIANC,DIFIL) ;
        N F,NM
        S F=0 F  S F=$O(DIANC(F)) Q:'F  D
        . S NM="" F  S NM=$O(^DD(F,0,"IX",NM)) Q:NM=""  K:$D(^(NM,DIFIL)) ^(DIFIL)
        Q
KACOMP  N DA,I,% S DA(1)=DQ,DA=Y X ^DD(0,9.01,1,1,2) Q
 ;
KTRB(DIANC,DIFIL,DIFLD) ;Kill 5 node of triggered field
        ;Also KILL "TRB" nodes here IF triggered field is in another file
        N %,F,DITFLD,DITFIL,DIXR,DIXR0
        S DIXR=0
        F  S DIXR=$O(^DD(DIFIL,DIFLD,1,DIXR)) Q:'DIXR  S DIXR0=$G(^(DIXR,0)) D:$P(DIXR0,U,3)="TRIGGER"
        . S DITFIL=$P(DIXR0,U,4),DITFLD=$P(DIXR0,U,5) Q:'DITFIL!'DITFLD
        . S %=0
        . F  S %=$O(^DD(DITFIL,DITFLD,5,%)) Q:'%  I $P($G(^(%,0)),U,1,3)=(DIFIL_U_DIFLD_U_DIXR) D  Q
        .. K ^DD(DITFIL,DITFLD,5,%) Q:DITFIL=DIFIL!$D(DIANC(DITFIL))
        .. S F=DITFIL
        .. F  K ^DD(F,"TRB",DIFIL) S F=$G(^DD(F,0,"UP")) Q:'F!$D(DIANC(+F))
        Q
DELXR(DIFIL) ;Delete the Key and Index file entries for file DIFIL
        Q:'$G(DIFIL)
        N DA,DIK
        ;
        ;Kill keys on file DIFIL
        S DIK="^DD(""KEY"","
        S DA=0 F  S DA=$O(^DD("KEY","B",DIFIL,DA)) Q:'DA  D ^DIK
        ;
        ;Kill indexes on file DIFIL
        S DIK="^DD(""IX"","
        S DA=0 F  S DA=$O(^DD("IX","AC",DIFIL,DA)) Q:'DA  D ^DIK
        Q
 ;
DELXRF(DIFIL,DIFLD,DIFLG,DITOPFIL) ;Delete Keys and Indexes on field
        ;If DIFLG=1, also delete the Indexes from the data global.
        Q:'$G(DIFIL)!'$G(DIFLD)
        N DA,DIK
        ;
        ;Execute the KILL logic for all indexes defined on the field
        ;for all entries in the file.
        I $G(DIFLG) D
        . S:$G(DITOPFIL)="" DITOPFIL=$$FNO^DILIBF(DIFIL)
        . D:DITOPFIL INDEX^DIKC(DITOPFIL,"",DIFLD,"","RKW"_DIFIL)
        ;
        ;Kill keys on file/field
        S DIK="^DD(""KEY"","
        S DA=0 F  S DA=$O(^DD("KEY","F",DIFIL,DIFLD,DA)) Q:'DA  D ^DIK
        ;
        ;Kill indexes on file/field
        S DIK="^DD(""IX"","
        S DA=0 F  S DA=$O(^DD("IX","F",DIFIL,DIFLD,DA)) Q:'DA  D ^DIK
        Q
