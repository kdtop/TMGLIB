TMGXPDR   ;TMG/kst/Altered version of XPDR ;03/25/06, 2/2/14, 2/2/14
         ;;1.0;TMG-LIB;**1**;7/25/05
 ;
 ;"TMGXPDR -- a custom version of XPDR
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
XPDR    ;SFISC/RSD - Routine File Edit ;09/17/96  10:05
        ;;8.0;KERNEL;**1,2,44**;Jul 10, 1995
        Q

UPDT        ;update routine file
        NEW DIR,DIRUT,XPD,XPDI,XPDJ
        NEW XPDN  ;"array of included (1 node) and excluded (0 node) namespaces
        NEW X,X1,Y,Y1,%
        NEW addCount SET addCount=0

        WRITE !!
        WRITE "** ROUTINE File Updater **",!
        WRITE "(Allows addition of selected routines to ROUTINE file)",!
        WRITE "-----------------------------------------------------------",!
        WRITE !
        WRITE "Enter namespace of routines to add (e.g. TIU), or",!
        WRITE "routines to exclude from addition (e.g. -TIU)",!!

        SET DIR(0)="FO^1:9^K:X'?.1""-""1U.7UNP X"
        SET DIR("A")="Routine Namespace ([ENTER] IF done)"
        SET DIR("?")="Enter 1 to 8 characters, preceed with ""-"" to exclude namespace"

        ;"XPDN(0=excluded names or 1=include names, namespace)=""
        FOR  DO  QUIT:$DATA(DIRUT)
        . DO ^DIR
        . QUIT:$DATA(DIRUT)
        . SET X=($EXTRACT(Y,$L(Y))="*")
        . SET %=($EXTRACT(Y)="-")
        . SET XPDN('%,$EXTRACT(Y,%+1,$LENGTH(Y)-X))=""

        IF ('$DATA(XPDN))!($DATA(DTOUT))!($DATA(DUOUT)) WRITE ! GOTO UPDTQ
        ;"QUIT:'$DATA(XPDN)!$DATA(DTOUT)!$DATA(DUOUT)
        WRITE !!,"NAMESPACE  INCLUDE",?35,"EXCLUDE",!,?11,"-------",?35,"-------"
        SET (X,Y)=""
        SET (X1,Y1)=1
        FOR  DO  WRITE !?11,X,?35,Y QUIT:'X1&'Y1
        . SET:X1 X=$O(XPDN(1,X)),X1=X]""
        . SET:Y1 Y=$O(XPDN(0,Y)),Y1=Y]""

        KILL DIR
        SET DIR(0)="Y"
        SET DIR("A")="OK to continue"
        SET DIR("B")="YES"
        DO ^DIR

        QUIT:'Y!$DATA(DIRUT)
        WRITE !
        SET DIR(0)="Y"
        SET DIR("A")="Want me to clean up the Routine File before updating"
        SET DIR("?")="YES means you want to go throught the Routine file and delete any routine name that no longer exists on the system."
        DO ^DIR

        QUIT:$DATA(DIRUT)
        DO WAIT^DICD
        WRITE !
        DO DELRTN:Y

        ;"----------------------------------------------------------------------------
        ;"Replacement code for below...
        NEW XPDArray
        MERGE XPDArray=XPDN(1)  ;"node 1=>included namespaces
        ;"ensure that all entries end with "*" (e.g. "TMG*" not "TMG")
        SET XPDI=$ORDER(XPDArray(""))
        IF XPDI'="" FOR  DO  QUIT:XPDI=""
        . NEW node SET node=XPDI
        . SET XPDI=$ORDER(XPDArray(node))
        . IF ($EXTRACT(node,$LENGTH(node))'="*") do
        . . KILL XPDArray(node)
        . . SET XPDArray(node_"*")=""

        DO NOINT^%RSEL("XPDArray")  ;"creates %ZR - an array of existing routines matching input request
        SET XPDJ=""
        FOR  DO  QUIT:XPDJ=""
        . SET XPDJ=$ORDER(%ZR(XPDJ))
        . IF XPDJ="" QUIT
        . IF $DATA(XPDN(0,XPDJ)) QUIT  ;"if name XPDJ is in the exclude list, skip
        . IF $ORDER(^DIC(9.8,"B",XPDJ,0)) QUIT  ;"if name XPDJ is already in Routine file, skip
        . ;"check IF XPDJ is refered in the EXCLUDED namespace by checking the subscript before XPDJ
        . SET %=$ORDER(XPDN(0,XPDJ),-1)
        . ;"if sub exist and $PIECE(XPDJ,sub)="" then it is part of the namespace, QUIT
        . IF ($LENGTH(%)>0)&($PIECE(XPDJ,%)="") QUIT
        . ;"Add routine to ROUTINE file
        . NEW XPD
        . SET XPD(9.8,"+1,",.01)=XPDJ
        . SET XPD(9.8,"+1,",1)="R"
        . DO ADD^DICA("","XPD")
        . WRITE "Added: ",XPDJ,!
        . SET addCount=addCount+1
UPDTQ
        WRITE "    ...Done.",!
        IF addCount=0 WRITE "ROUTINE file already up to date.  No additions needed.",!
        ELSE  WRITE addCount," entries added to ROUTINE file.",!
        WRITE "Leaving ROUTINE File Updater.",!
        QUIT

        ;"----------------------------------------------------------------------------

        ;"loop thru include list XPDN(1,*), i.e. included nodes-->requested namespaces
        ;"Goal: to consider each requested namespace...

        ;"Pseudocode:
        ;"          loop  (through all requested namespaces)
        ;"            XPDI = currently considered namespace
        ;"            loop (through all available routines--starting at XPDI)
        ;"              XPDJ is current routine name being considered -- from all available routines
        ;"              IF current routine name (XPDJ) is in exclude list, skip
        ;"              IF current routine name (XPDJ) is already in the ROUTINE file, then skip
        ;"              ... (to be completed)

        ;set XPDI=""
        ;FOR  DO  QUIT:XPDI=""
        ;. SET XPDI=$ORDER(XPDN(1,XPDI))
        ;. QUIT:XPDI=""
        ;. SET XPDJ=XPDI
        ;. IF '$DATA(^$routine(XPDJ)) QUIT
        ;. FOR  SET XPDJ=$ORDER(^$routine(XPDJ)) QUIT:(XPDJ="")!($PIECE(XPDJ,XPDI)]"")  do
        ;. . IF $DATA(XPDN(0,XPDJ)) QUIT  ;"if name XPDJ is in the exclude list, XPDN(0,XPDJ) QUIT
        ;. . IF $ORDER(^DIC(9.8,"B",XPDJ,0)) QUIT  ;"if name XPDJ is in Routine file, QUIT
        ;. . ;"check IF XPDJ is refered in the EXCLUDED namespace by checking the subscript before XPDJ
        ;. . SET %=$ORDER(XPDN(0,XPDJ),-1)
        ;. . ;"if sub exist and $PIECE(XPDJ,sub)="" then it is part of the namespace, QUIT
        ;. . IF ($LENGTH(%)>0)&($PIECE(XPDJ,%)="") QUIT  ;"e.g $PIECE("TMGTEST",
        ;. . NEW XPD
        ;. . SET XPD(9.8,"+1,",.01)=XPDJ
        ;. . SET XPD(9.8,"+1,",1)="R"
        ;. . DO ADD^DICA("","XPD")
        ;WRITE "    ...Done.",!
        ;QUIT

VER        ;verify Routine file
        N DIR,DIRUT,X,Y
        W !,"I will delete all entries in the ROUTINE file in which",!,"the Routine no longer exist on this system!",!
        S DIR(0)="Y",DIR("A")="OK to continue",DIR("B")="YES" D ^DIR
        Q:'Y!$D(DIRUT)  D DELRTN
        W "    ...Done.",!
        Q
DELRTN        ;delete routine file entries
        N DA,DIK,Y,count,max,delNum
        S DIK="^DIC(9.8,",DA=0,count=0,max=0,delNum=0
 ;"        F  S DA=$O(^DIC(9.8,DA)) Q:'DA  S Y=$G(^(DA,0)) I $P(Y,U,2)="R",$T(^@$P(Y,U))="" D ^DIK
        DO INIT^XPDID
        FOR  SET DA=$ORDER(^DIC(9.8,DA)) QUIT:'DA  SET max=max+1
        IF max=0 SET max=1
        SET XPDIDTOT=max
        DO TITLE^XPDID("Scanning for Entries to Remove...")
        SET DA=0
        WRITE !,"Starting search...",!
        FOR  SET DA=$ORDER(^DIC(9.8,DA)) QUIT:'DA  do
        . SET count=count+1
        . IF count#50=0 DO UPDATE^XPDID(count)
        . SET Y=$G(^(DA,0))
        . IF ($PIECE(Y,U,2)="R")&($text(^@$PIECE(Y,U))="") do
        . . WRITE "Removing: ",$PIECE(Y,U),!
        . . SET delNum=delNum+1
        . . DO ^DIK
        WRITE !
        IF delNum>0 do
        . NEW temp
        . WRITE "Done scanning.  ",delNum," Entries removed.",!
        . read "Please press [ENTER] to continue.",temp:$GET(DTIME,3600),!
        DO EXIT^XPDID()
        QUIT
PURGE        ;purge file
        N DA,DIK,DIR,DIRUT,X,XPD,XPDF,XPDI,XPDJ,XPDL,XPDN,XPDPG,XPDS,XPDUL,Y,Z
        S DIR("?")="Enter the file you want to purge the data from.",DIR(0)="SM^B:Build;I:Install;ALL:Build & Install",DIR("A")="Purge from what file(s)"
        D ^DIR Q:$D(DIRUT)
        S XPDF=$S(Y="I":9.7,1:9.6) S:Y="ALL" XPDF(1)=9.7
        K DIR S DIR("?")="Enter the number of Versions to keep in the file, for each package",DIR(0)="N^0:100:0",DIR("A")="Versions to Retain",DIR("B")=1
        D ^DIR Q:$D(DIRUT)  S XPDN=Y
        K DIR
        S DIR(0)="FO^3:30",DIR("?")="^D PURGEH^XPDR",DIR("A")="Package Name",DIR("B")="ALL"
        F  D ^DIR Q:$D(DIRUT)  S XPD(X)="" Q:X="ALL"  K DIR("B") S DIR("A")="Another Package Name"
        Q:'$D(XPD)
        ;if they want all, make sure all is the only one
        I $D(XPD("ALL")) K XPD S XPD("ALL")=""
        ;XPDF(1) is defined IF doing both files, DO purge twice
        K ^TMP($J) D PURGE1(XPDF),PURGE1($G(XPDF(1))):$D(XPDF(1))
        I '$D(^TMP($J)) W !!,"No match found" Q
        K XPD,DIR
        S DIR(0)="E",$P(XPDUL,"-",IOM)=""
        ;if ALL, reset XPDF to next file and Do, then reset back to 9.6
        D  I $D(XPDF(1)) D ^DIR I Y S XPDF=XPDF(1) D  S XPDF=9.6
        .S XPD="^TMP("_$J_","_XPDF,XPDS=XPD_",",XPD=XPD_")",XPDL=$L(XPDS),XPDPG=1,Y=1
        .W @IOF D HDR
        .;loop thru ^TMP($J,file,package) & show list, QUIT IF user "^"
        .F  S XPD=$Q(@XPD) Q:XPD=""!($E(XPD,1,XPDL)'=XPDS)  D  Q:'Y
        ..S Z=@XPD W $P(Z,"^"),$S($P(Z,"^",3):"  (duplicates)",1:""),! Q:$Y<(IOSL-4)
        ..D ^DIR Q:'Y
        ..S XPDPG=XPDPG+1 W @IOF D HDR
        S DIR(0)="Y",DIR("A")="OK to DELETE these entries",DIR("B")="NO"
        W !! D ^DIR
        I $D(DIRUT)!'Y W !!,"Nothing Purged" Q
        ;loop thru and delete
        D  I $D(XPDF(1)) S XPDF=XPDF(1) D
        .S DIK="^XPD("_XPDF_",",XPD="^TMP("_$J_","_XPDF,XPDS=XPD_",",XPD=XPD_")",XPDL=$L(XPDS)
        .F  S XPD=$Q(@XPD) Q:XPD=""!($E(XPD,1,XPDL)'=XPDS)  D
        ..S XPDI=@XPD F XPDJ=2:1 S DA=$P(XPDI,"^",XPDJ) Q:'DA  D ^DIK
        Q
        ;
PURGE1(XPDF)        ;XPDF=file #
        N XPDFL,XPDI,XPDJ,XPDP,XPDV,Y,Z
        W "."
        ;if All, loop thru B x-ref
        I $D(XPD("ALL")) D
        .S XPDI=""
        .F  S XPDI=$O(^XPD(XPDF,"B",XPDI)) Q:XPDI=""  D
        ..S X=$$PKG^XPDUTL(XPDI) D PURGE2(X)
        ..W "."
        E  S XPDI="" F  S XPDI=$O(XPD(XPDI)) Q:XPDI=""  D
        .D PURGE2(XPDI)
        .W "."
        ;loop thru each package, XPDP=package name
        S XPDP="" F  S XPDP=$O(^TMP($J,XPDF,XPDP)) Q:XPDP=""  D
        .S XPDV="",XPDL=XPDN
        .;the last is the most recent, XPDN = number to retain, XPDV=version
        .;XPDS=type (T/V/Z)
        .F  S XPDV=$O(^TMP($J,XPDF,XPDP,XPDV),-1),XPDS="" Q:'XPDV!'XPDL  F  S XPDS=$O(^TMP($J,XPDF,XPDP,XPDV,XPDS),-1) Q:XPDS=""!'XPDL  D
        ..S Y="" F  S Y=$O(^TMP($J,XPDF,XPDP,XPDV,XPDS,Y),-1) Q:Y=""!'XPDL  D
        ...I $D(^TMP($J,XPDF,XPDP,XPDV,XPDS,Y))#2 K ^(Y) S XPDL=XPDL-1 Q
        ...S Z="" F  S Z=$O(^TMP($J,XPDF,XPDP,XPDV,XPDS,Y,Z),-1) Q:Z=""!'XPDL  K ^(Z) S XPDL=XPDL-1
        Q
        ;
PURGE2(XPDX)        ;XPDX=package name
        ;XPDFL=1 this is not a patch, QUIT when we find a patch during loop
        S XPDS=XPDX,XPDL=$L(XPDX),XPDFL=XPDX'["*"
        ;loop and find matches
        D  F  S XPDS=$O(^XPD(XPDF,"B",XPDS)) Q:XPDS=""!($E(XPDS,1,XPDL)'=XPDX)!($S(XPDFL:XPDS["*",1:0))  D
        .S Y=$O(^XPD(XPDF,"B",XPDS,0)) Q:'Y
        .Q:'$D(^XPD(XPDF,Y,0))  S Z=^(0),Y=XPDS_"^"_Y
        .;can't delete Installs that status isn't 'Install Completed'
        .I XPDF=9.7 Q:$P(Z,U,9)<3
        .S XPDV=$$VER^XPDUTL(XPDS)
        .;TMP($J,file,package name,version,"*","T/V/Z",num,patch)=NAME^DA^duplicat DAs
        .I XPDS["*" D  Q
        ..I XPDV?1.2N1"."1.2N S ^TMP($J,XPDF,$$PKG^XPDUTL(XPDS),+XPDV,"*Z",0,+$P(XPDS,"*",3))=Y_$$DUP(XPDS,$P(Y,"^",2)) Q
        ..I XPDV["T" S ^TMP($J,XPDF,$$PKG^XPDUTL(XPDS),+XPDV,"*T",+$P(XPDV,"T",2),+$P(XPDS,"*",3))=Y_$$DUP(XPDS,$P(Y,"^",2)) Q
        ..I XPDV["V" S ^TMP($J,XPDF,$$PKG^XPDUTL(XPDS),+XPDV,"*V",+$P(XPDV,"V",2),+$P(XPDS,"*",3))=Y_$$DUP(XPDS,$P(Y,"^",2)) Q
        ..S ^TMP($J,XPDF,$$PKG^XPDUTL(XPDS),+XPDV,"*",+$P(XPDS,"*",3))=Y_$$DUP(XPDS,$P(Y,"^",2))
        .;TMP($J,file,package name,version,"Z",0)=NAME^DA^duplicate DAs
        .I XPDV?1.2N1"."1.2N S ^TMP($J,XPDF,$$PKG^XPDUTL(XPDS),+XPDV,"Z",0)=Y_$$DUP(XPDS,$P(Y,"^",2)) Q
        .;TMP($J,file,package name,version,"T/V",num)=NAME^DA^dup DAs
        .I XPDV["T" S ^TMP($J,XPDF,$$PKG^XPDUTL(XPDS),+XPDV,"T",+$P(XPDV,"T",2))=Y_$$DUP(XPDS,$P(Y,"^",2)) Q
        .I XPDV["V" S ^TMP($J,XPDF,$$PKG^XPDUTL(XPDS),+XPDV,"V",+$P(XPDV,"V",2))=Y_$$DUP(XPDS,$P(Y,"^",2)) Q
        Q
PURGEH        ;executable help from DIR call at PURGE+8
        W:$E(DIR("A"),1)="P" !,"Enter 'ALL' to purge all packages, or"
        W !,"Enter the name of the Package you want to Purge.",!," i.e. KERNEL 8.0  will purge version 8.0Tx and 8.0Vx",!,"      XU*8.0 will purge all patches for 8.0",!
        N DIR,X,Y
        S DIR(0)="Y",DIR("A")="Want to see the "_$S(XPDF=9.7:"Install File",$D(XPDF(1)):"Build & Install Files",1:"Build File")_" List",DIR("B")="Y"
        D ^DIR Q:'Y!$D(DIRUT)
        D PURGEH1("^XPD(9.6,"):XPDF=9.6,PURGEH1("^XPD(9.7,"):XPDF=9.7!$D(XPDF(1))
        Q
        ;
DUP(Z,Z1)        ;find duplicate, Z=NAME, Z1=last ien
        ;returns Y=DA^dup DA^dup DA...
        N Y S Y=""
        F  S Z1=$O(^XPD(XPDF,"B",Z,Z1)) Q:'Z1  S Y=Y_"^"_Z1
        Q Y
        ;
PURGEH1(DIC)        ;
        W !!,$S(DIC[9.6:"BUILD ",1:"INSTALL ")_"File"
        S DIC(0)="QE",X="??" D ^DIC
        Q
        ;
HDR        W !,"Package(s) in ",$S(XPDF=9.7:"INSTALL",1:"BUILD")," File, "
        I XPDN W "Retain last ",$S(XPDN=1:"version",1:XPDN_" versions")
        E  W "Don't retain any versions"
        W ?70,"PAGE ",XPDPG,!,XPDUL,!
        Q
