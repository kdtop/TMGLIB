TMGDICATT22 ;KST/SFISC/GFT,XAK- TMG VERSION OF CREATE A SUBFILE ;6 JAN,2011, 2/2/14
         ;;1.0;TMG-LIB;**1**;1/6/11
         ;
         ;"Original file header below
         ;"DICATT22 ;SFISC/GFT-CREATE A SUBFILE ;28MAY2006
         ;";;22.0;VA FileMan;**42,52,89,999,1004,1024**;Mar 30, 1999
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
        IF V GOTO M
        IF P,$D(^DD(J(N-1),P,0)) DO
        . SET I=A_$E("I",$P(^(0),U,2)["I")
        . DO P
        IF O,DA=.01,'N DO
        . SET I=$P(@(I(0)_"0)"),U,2)
        . DO P
1 ;
        S %=$L(F)+$L(W)+$L(C)+$L(Z)
        I %>242 DO  G TYPE^TMGDICATT2
        . ;"W $C(7),!?5,"Field Definition is TOO LONG by ",%-242," characters!"
        . SET TMGRESULT="-1^Field Definition is TOO LONG by "_%-242_" characters!"
        IF T["P",$D(O)=11,+$P($P(O(1),U,2),"P",2)'=+$P(Z,"P",2) DO
        . S X=$P(O(1),U,2),DA(1)=A
        . XECUTE:$D(^DD(0,.2,1,3,2)) ^(2)
        SET ^DD(A,DA,0)=F_U_Z_U_W_U_C
        IF $P(Z,U)["K" SET ^(9)="@"
        DO SDIK
        DO I
        SET TMGDONE=1
        GOTO N^TMGDICATT
 ;
Q       W $C(7),!,"NUMBER MUST BE BETWEEN ",A," & ",%+1," AND NOT ALREADY IN USE"
M       S %=$P(A,"."),DE=%_"."_+$P(A,".",2)_DA
        I +DE'=DE!$D(^DD(DE)) F DE=A+.01:.01:%+.7,%+.7:.001:%+.9,%+.9:.0001 Q:DE>A&'$D(^DD(DE))
        I DUZ(0)'="@" GOTO M2
        ;"W !,"SUB-DICTIONARY NUMBER: "_DE_"// "
        IF TMGMULTSN="" SET TMGMULTSN=DE
        ;"READ DG:DTIME
        ;"SET:'$T DTOUT=1
        ;"GOTO:DG=U!'$T ^TMGDICATT2
        SET DG=TMGMULTSN
        SET:DG]"" DE=DG
        GOTO Q:+DE'=DE!(DE<A)
M2      GOTO Q:%+1'>DE!$D(^DD(DE))
        SET I=DE
        SET ^(I,0)=F_" SUB-FIELD^^.01^1"
        SET ^(0,"UP")=A
        SET ^("NM",F)=""
        SET %X="^DD("_A_","_DA_")"
        SET @%X@(0)=F_"^^^"_W
        DO P
        S W=$P(W,";")
        D SDIK
        SET:+W'=W W=""""_W_""""
        SET DICATT22=DA
        SET (N,DICL)=N+1
        SET I(N)=W
        SET J(N)=DE
        SET DA=.01
        SET ^DD(DE,DA,0)=F_U_Z_"^0;1^"_C
        SET %Y="^DD("_DE_",.01)"
VARPOINT I T["V" D
        . N I,FI,FD,P
        . S FI=$QS(%X,1)
        . SET FD=$QS(%X,2)
        . S I=0
        . F  S I=$O(@%X@("V",I)) Q:'I  DO
        . . S P=+$G(^(I,0))
        . . K:P ^DD(P,0,"PT",FI,FD)
        . M @%Y@("V")=@%X@("V")
        . K @%X@("V")
POINT   I T["P" F %=12,12.1 DO
        . I $D(@%X@(%)) DO
        . . S @%Y@(%)=@%X@(%)
        . . K @%X@(%)
        K %X,%Y
        I T'["W" D
        . SET ^DD(DE,DA,1,0)="^.1"
        . SET ^(1,0)=DE_"^B"
        . SET DIK=W_",""B"",$E(X,1,30),DA)"
        . F %=DICL-1:-1 SET DIK=I(%)_$E(",",1,%)_"DA("_(DICL-%)_"),"_DIK IF '% DO  QUIT
        . . SET ^(1)="S "_DIK_"="""""
        . . SET ^(2)="K "_DIK
        . . S:T["V" ^(3)="Required Index for Variable Pointer" Q
        DO SDIK
        DO I
        SET DICL=DICL-1
        D AUDIT(DA(1),.01,"N")
        SET DA=DICATT22
        K DICATT22 ;"AUDIT THE NEW .01 FIELD AT THE LOWER LEVEL
        SET TMGDONE=1
        GOTO N^TMGDICATT
 ;
AUDIT(DIFILE,DIFIELD,DITYPE) ;
        N DDA,DA,B0,A0
        SET DDA(1)=DIFILE
        SET DA=DIFIELD
        SET DDA=$G(DITYPE,"E")
        DO AUDT^DICATTA
        QUIT
 ;
 ;
 ;
I       I $P(O,U,2,99)'=$P(^DD(J(N),DA,0),U,2,99) DO
        . IF $D(M)#2 SET ^(3)=M
        . SET M(1)=0
        K DR,DG,DB,DQ,DQI,^DD(U,$J),^UTILITY("DIVR",$J)
EGP     ;K ^DD(DA(1),DA,.009) ; GET RID OF FOREIGN-LANGUAGE HELP MESSAGE WHEN THE BASIC ENGLISH ONE IS BEING RE-EDITED??
        SET DIE=DIK  ;"DIE=^DD(A,  DA=current record, DA(1)=field num
        ;"SET DR=$S(DUZ(0)="@":"3;4",1:3)_$P(";21",U,'O)
        SET DR=""
        IF TMGHLP'="" SET DR="3///"_TMGHLP
        IF (TMGXHLP'=""),DUZ(0)="@" DO
        . IF DR'="" SET DR=DR_";"
        . SET DR=DR_"4///"_TMGXHLP
        ;"IF 'O DO
        ;". IF DR'="" SET DR=DR_";"
        ;". SET DR=DR_"21"
        DO
        . NEW I,J,T
        . DO ^DIE
        IF ('O),$DATA(TMGDESCR) DO
        . NEW GREF SET GREF=DIE_DA_",21)"
        . NEW TMP SET TMP=$$SETWP(GREF,.TMGDESCR)
        . IF TMP<0 SET TMGRESULT=TMP
        ;
        IF T="W" K DE
        I $D(M)>9,O DO  ;It's not clear that we need these variables set, now we are calling DIVR^DIUTL 12/01
        . SET V=DICL
        . SET DR=$P(Z,U)
        . SET Z=$P(Z,U,2)
V       . NEW D0
        . SET DI=J(N)
        . NEW ZTQUEUED SET ZTQUEUED=1 ;"force silence
        . DO DIPZ^DIU0  ;"ouputs 1 blank line to console (!)
        . Q:$D(DTOUT)!'$D(DIZ)  ;"NEEDS 'DI' & 'DA'
        . DO DIVR(A,DA)  ;"DO DIVR^DIUTL(A,DA)
        K DR,M
        QUIT
 ;
 ;
P       F Y="S","D","P","A","V" DO
        . IF I[Y SET I=$P(I,Y)_$P(I,Y,2)_$P(I,Y,3)
        . IF T[Y SET I=I_Y
        SET ^(0)=$P(^(0),U)_U_I_U_$P(^(0),U,3,99)
        QUIT
 ;
SDIK    N %X
        SET DA(1)=J(DICL)
        SET DIK="^DD("_DA(1)_","
        IF O K ^DD(DA(1),"RQ",DA)
        ;"W !,"...."
        G IX1^DIK  ;"(QUIT occurs from there)
        ;
SETWP(GREF,ARRAY) ;
        ;"Purpose: To directly SET a WP (word processor) field from array
        ;"NOTE: It is better to use FILE^DIE or UPDATE^DIE whenever possible.
        ;"      This function DOES NOT TRIGGER ANY XREFS!
        ;"      Any prior entry in WP field is killed.
        ;"      This function DOES NOT check that GREF is proper.  It stores ARRAY
        ;"              at GREF regardless.  //old--> As long as *anything* is already there.
        ;"Input: GREF -- The reference to the header node, Open or closed format
        ;"               (e.g.  ^TMG(22702,99,1) in example below)
        ;"       ARRAY -- Holds WP information.  PASS BY REFERENCE.  Format as folows:
        ;"              ARRAY(n)=first line of array
        ;"              ARRAY(n+1)=second line of array
        ;"              ARRAY(n+2)=third line... etc.
        ;"Note:  The format of a WP field is as follows:
        ;"       e.g.    ^TMG(22702,99,1,0) = ^^4^4^3050118^
        ;"               ^TMG(22702,99,1,1,0) = Here is the first line of text
        ;"               ^TMG(22702,99,1,2,0) = And here is another line
        ;"               ^TMG(22702,99,1,3,0) =
        ;"               ^TMG(22702,99,1,4,0) = And here is a final line
        ;"  And the format of the 0 node is: ^^<line count>^<linecount>^<fmdate>^^
        ;"Result: -1^Message IF failure, 1 IF success
        ;"Assumptions: That GlobalP is a valid reference to a WP field
        ;
        NEW RESULT SET RESULT=1 ;"default to success
        IF $GET(GREF)="" DO  GOTO SETWDN
        . SET RESULT="-1^GREF not provided"
        SET GREF=$$CREF^DILF(GREF)
        ;"IF $DATA(GREF)=0 DO  GOTO SETWDN
        ;". SET RESULT="-1^GREF doesn't seem to point to existing WP field
        NEW WP
        NEW J SET J=0
        NEW I SET I=""
        FOR  SET I=$ORDER(ARRAY(I)) QUIT:(I="")  DO
        . SET J=J+1
        . SET WP(J,0)=ARRAY(I)
        ;"Now create a header node
        DO NOW^%DTC  ;"returns result in X
        SET WP(0)="^^"_J_"^"_J_"^"_X_"^^"
        ;"Now put WP into global reference.
        KILL @GREF
        MERGE @GREF=WP
SETWDN  QUIT RESULT
        ;
DIVR(DI,DIFLD) ;verify.  Copied from DIVR^DIUTL
        NEW DIVZ,S,A,DA,DICL,V,Z,DDC,DR,N,Y,I,J,Q,W,V,T,DQI
        KILL ^UTILITY("DIVR",$J),^DD(U,$J)
        DO IJ^DIUTL(DI)
        IF '$O(@(I(0)_"0)")) QUIT  ;File must have some entries!
        SET S=";",Q="""",V=$O(J(""),-1),A=DI,DA=DIFLD
        SET DR=$P(^DD(DI,DIFLD,0),U,2)
        SET Z=$P(^(0),U,3)
        SET $P(Y(0),U,4)=$P(^(0),U,4)
        SET DDC=$P(^(0),U,5,999)
        QUIT:DR["W"!(DR["C")
        FOR T="N","S","V","P","K","F" Q:DR[T
        NEW TMGNAME SET TMGNAME=$P(^(0),U)
        ;"WRITE !!,"SINCE YOU HAVE CHANGED THE FIELD DEFINITION,",!,"EXISTING '",$P(^(0),U),"' DATA WILL NOW BE CHECKED FOR INCONSISTENCIES",!,"OK"
        ;"SET %=1 D YN^DICN Q:%-1
        NEW DIVFIL SET DIVFIL=TMGFILE ;"??
        ;"D ^%ZIS Q:POP
        ;"U IO   WON'T WORK BECAUSE Q+3^DIVR ASKS TO STORE IN TEMPLATE
        DO
        . NEW TMGTMP
        . DO TMGDIVR0^TMGDIVR(.TMGTMP,DI,DIFLD)
        . IF $GET(TMGTMP("RESULT"))="OK" QUIT
        . SET TMGRESULT="-1^Error checking field data for NEW field type.  See message."
        . DO ADDERR^TMGDICATTS("SINCE THE FIELD DEFINITION CHANGED, EXISTING '"_TMGNAME_"' DATA")
        . DO ADDERR^TMGDICATTS("WAS CHECKED FOR INCONSISTENCIES, AND PROBLEMS WERE FOUND:")
        . DO ADDERR^TMGDICATTS("-------------------------------------------------------------")
        . NEW I SET I=""
        . FOR  SET I=$ORDER(TMGTMP(I)) QUIT:(+I'=I)  DO
        . . DO ADDERR^TMGDICATTS($GET(TMGTMP(I)))
        ;"D ^%ZISC
        QUIT
