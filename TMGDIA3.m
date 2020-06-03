TMGDIA3 ;TMG/kst/Custom version of DIA3 ;03/25/06, 2/2/14
        ;;1.0;TMG-LIB;**1**;01/01/06

 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;

DIA3    ;SFISC/GFT-UPDATE POINTERS, CHECK CODE IN INPUT STRING, CHECK FILE ACCESS ;9/7/94  09:57
        ;;22.0;VA FileMan;;Mar 30, 1999
        ;Per VHA Directive 10-93-142, this routine should not be modified.

        ;"*************************************************************************
        ;"* Custom version of Fileman code, for customization
        ;"* Also includes code from DITP.m
        ;"*************************************************************************

FIXPT(DIFLG,DIFILE,DIDELIEN,DIPTIEN)
        ;"Purpose:      DELETE OR REPOINT POINTERS
        ;"Note:         In V21, will just delete pointers.  Later, DIPTIEN will be record to repoint to.
        ;"Input: DIFLG="D" (delete) ;" ADDED "R" (replace) //kt
        ;"         DIFILE=File# previously pointed to
        ;"         DIDELIEN=Record# previously pointed to
        ;"         DIPTIEN=New pointed-to record(future)  ;"//KT fixed to DO now.
        ;"         ;"//e.g. IF wanting to replace all pointers to file#50, record#20 to record#40 (must be in file#50)
        ;"         ;"//   then DIFILE=50, DIDELIEN=20, DIPTIEN=40
        ;"Output:
        ;"Result: none

        ;"Note: sample of array passed to P^DITP
        ;"              23510 is $J
        ;"              47 is IEN to be deleted in file 50 (stored at ^PSDRUG(*))
        ;"              1646 is IEN to be substituted for all 47's
        ;"
        ;"              First part of array is list of all files & fields that point to file
        ;"              ----------------
        ;"              ^UTILITY("DIT",23510,0,1)="727.819^67^P50'"
        ;"              ...
        ;"              ^UTILITY("DIT",23510,0,54)="801.43^.02^RV"
        ;"              ^UTILITY("DIT",23510,0,55)="810.31^.04^V"
        ;"              ^UTILITY("DIT",23510,0,56)="810.32^.01^V"
        ;"              ^UTILITY("DIT",23510,0,57)="811.52^.01^MVX"
        ;"              ^UTILITY("DIT",23510,0,58)="811.902^.01^MVX"
        ;"              ^UTILITY("DIT",23510,0,59)="9009032.4^.05^P50'"
        ;"
        ;"              Second part of array is list of changes that should be made.  Only 1 change shown here.
        ;"              ----------------
        ;"              ^UTILITY("DIT",23510,47)="1646;PSDRUG("
        ;"              ^UTILITY("DIT",23510,"47;PSDRUG(")="1646;PSDRUG("

        NEW %X,%Y,X,Y
        ;"new DIPTIEN  ;//kt allow input value to be used.
        NEW DIFIXPT,DIFIXPTC,DIFIXPTH
        DO  IF $G(X)]"" DO BLD^DIALOG(201,X) QUIT   ;"BUILD FILEMAN DIALOG
        . SET X="DIFLG" QUIT:(($G(DIFLG)'="D")&($G(DIFLG)'="R"))  ;"//kt added "R"
        . SET X="DIDELIEN" Q:'$G(DIDELIEN)
        . SET X="DIFILE" Q:'$G(DIFILE)  Q:$G(^DIC(DIFILE,0,"GL"))=""
        . SET X="DIPTIEN"
        . IF (DIFLG="R"),$G(DIPTIEN) DO  QUIT:(Y="")
        . . SET Y=$GET(^DIC(DIFILE,0,"GL"))    ;"//kt changed ^DD to ^DIC
        . . QUIT:Y=""
        . . IF '$DATA(@(Y_DIPTIEN_",0)")) SET Y="" QUIT
        . KILL X
        . QUIT
        SET DIPTIEN=+$G(DIPTIEN)
        SET (DIFIXPT,DIFIXPTC)=1
        NEW %,BY,D,DHD,DHIT,DIA,DIC,DISTOP,DL,DR,DTO,FLDS,FR,IOP,L,TO,X,Y,Z
        KILL ^UTILITY("DIT",$J),^TMP("DIFIXPT",$J)
        SET (DIFILE,DIA("P"),Y)=+DIFILE
        SET (DIA,DTO)=^DIC(DIFILE,0,"GL")
        SET DIA(1)=DIDELIEN
        DO PTS^DIT
        SET ^UTILITY("DIT",$J,0)=0
        GOTO:$D(^(0))<9 QFIXPT
        SET ^UTILITY("DIT",$J,DIA(1))=DIPTIEN_";"_$E(DIA,2,999)
        SET ^UTILITY("DIT",$J,DIA(1)_";"_$E(DIA,2,999))=DIPTIEN_";"_$E(DIA,2,999)

        DO ZWRITE^TMGZWR($NAME(^UTILITY("DIT",$J)))
        ;"do P^DITP
        ;"do P

QFIXPT
        K ^UTILITY("DIT",$J),DIFLG,DIFILE,DIDELIEN,DIIOP,DIPTIEN
        QUIT
        ;

        ;"*************************************************************************
        ;"*  Code below from DITP.m
        ;"*************************************************************************

PTS  ;
        D WAIT^DICD
        KILL IOP
P      KILL DR,D,DL,X
        SET (BY,FR,TO)=""
        SET X=$O(^UTILITY("DIT",$J,0,0))
        IF X="" DO  QUIT  ;"<--- exit point from loop
        . K ^UTILITY("DIT",$J),DIA,DHD,DR,DISTOP,BY,TO,FR,FLDS,L
        SET Y=^(X)  ;"get value of entry e.g.  50^905^P50'X
        SET L=$P(Y,U,2)   ;"L= field#
        SET DL=1
        SET DL(1)=L
        SET DL(1)=DL(1)_"////^S X=$S($D(DE(DQ))[0:"""",$D(^UTILITY(""DIT"",$J,DE(DQ)))-1:"""",^(DE(DQ)):"
        SET DL(1)=DL(1)_$S($P(Y,U,3)'["V":"+",1:"")
        SET DL(1)=DL(1)_"^(DE(DQ)),1:""@"") I X]"""",$G(DIFIXPT)=1 D PTRPT^TMGDIA3"
        KILL ^(X)  ;"delete entry from top of list
        SET L=$P(^DD(+Y,L,0),U,4)  ;"+Y=File#, L=Field# --> L SET to 4th piece of data dictionary entry, e.g. '8;6'
        SET %=$P(L,";",2)   ;"e.g. %=6
        SET L=""""_$P(L,";",1)_""""   ;"e.g. L="8"
        SET DHD=$P(^(0),U)   ;"DHD--> header for EN1^DIP
        IF % SET %="$P(^("_L_"),U,"_%   ;"--> e.g. SET %='$P(^(8),U,8
        ELSE  SET %="$E(^("_L_"),"_+$E(%,2,9)_","_$P(%,",",2)
        SET L=L_")):"""","_%_")?."" "":"""",'$D(^UTILITY(""DIT"",$J,"_$S($P(Y,U,3)'["V":"+",1:"")_%_"))):"""",1:D"
UP    SET D(DL)=+Y   ;"+Y = File#
        SET %=+Y    ;"+Y = File#
        IF $D(^DD(%,0,"UP")) DO  GOTO UP
        . SET DL=DL+1
        . SET Y=^("UP")
        . SET (DL(DL),%)=$O(^DD(Y,"SB",%,0))_"///"
        . SET X(DL)=""""_$P($P(^DD(Y,+%,0),U,4),";")_""""
        . SET BY=+%_","_BY
        SET DHD=$O(^("NM",0))_" entries whose '"_DHD_"' pointers have been changed"
        IF '$D(^DIC(%,0,"GL")) GOTO P
        SET DIC=^("GL")
        SET Y="S X=$S('$D("_DIC_"D0,"
        for X=0:1:DL-1 do
        . SET DR(X+1,D(DL-X))=DL(DL-X)
        . IF X SET Y=Y_X(DL+1-X)_",D"_X_","
        SET DIA("P")=%
        SET %=$L(BY,",")
        IF %>2 SET BY=$P(BY,",",%-2)_",.01,"_BY
        SET BY=BY_Y_L_X_")"
        SET L=0
        SET FLDS=""
        SET DISTOP=0
        SET DHIT="G LOOP^DIA2"
        SET %ZIS=""
        DO EN1^DIP
        IF $G(DIFIXPT)=1  GOTO P
        SET IOP=$G(IO)
        GOTO P
        ;

PTRPT
        QUIT:'$G(DIFIXPTC)
        NEW I,J,X
        for I=1:1:DL do
        . SET J=""
        . FOR  SET J=$ORDER(DR(I,J)) QUIT:J=""  do
        . . IF DR(I,J)["///" do
        . . . SET X=$P($G(DR(I,J)),"///",1)
        . . . IF X]"" do
        . . . . NEW s
        . . . . SET s=^TMP("DIFIXPT",$J,DIFIXPTC)
        . . . . SET s=s_$S(I>1:" entry:"_$S(I=DL:$G(DA),1:$G(DA(DL-I))),1:"")
        . . . . SET s=s_$S(I=DL:"   field:",1:"   mult.fld:")
        . . . . SET s=s_X
        . . . . SET ^TMP("DIFIXPT",$J,DIFIXPTC)=s
        Q

