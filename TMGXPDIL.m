TMGXPDIL ;TMG/kst/Custom version of XPDIL ;09/17/08, 2/2/14, 2/2/14
         ;;1.0;TMG-LIB;**1**;09/17/08
 ;"Original header....
 ;"XPDIL   ;SFISC/RSD - load Distribution Global ;05/28/99  09:41
 ;"       ;;8.0;KERNEL;**15,44,58,68,108**;Jul 10, 1995
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
EN1(Option,Msg)
        ;"Purpose: Provide an API for KIDS load a distribution
        ;"Input: Option -- PASS BY REFERENCE.  Entries are required unless marked optional
        ;"              Option("HFSNAME")=FilePathNameOnHFS
        ;"              Option("FORCE CONT LOAD")=1 <-- IF not given, then load won't continue
        ;"              Option("DO ENV CHECK")=#  1=do check, 0=don't DO check
        ;"              Option("VERBOSE")=1 for output
        ;"       Msg -- PASS BY REFERANCE, an OUT PARAMETER
        ;"              Errors are stored in Msg("ERROR",x)=Message
        ;"                                   Msg("ERROR")=count of last error
        ;"              Message are store in Msg(x)=Message
        ;"                                   Msg=count of last message+1
        ;"Output: Option -- Option("INSTALL NAME")=Name to use to install package

        N POP,XPDA,XPDST,XPDIT,XPDT,XPDGP,XPDQUIT,XPDREQAB,XPDSKPE
        S:'$D(DT) DT=$$DT^XLFDT S:'$D(U) U="^"
        S XPDST=0
        SET Msg=+$GET(Msg,1)

        NEW temp SET temp=$$ST(.Option,.Msg)  ;"Load in patch
        IF (temp=0)!($G(XPDQUIT)) DO  GOTO EnDone
        . D ABRTALL^TMGXPDI(1,,.Msg)  ;"(not interactive)
        . DO AddMsg^TMGPAT2("**NOTHING LOADED**",1,.Msg)

        ;"XPDST= starting Build
        ;"XPDT("DA",ien)=seq # to install
        ;"XPDT("NM",build name)=seq #
        ;"XPDT(seq #)=ien^Build name
        ;"XPDT("GP",global)= 1-replace, 0-overwrite^ien
        ;"XPDGP=globals from a Global Package
        ;"XPDSKPE=1 don't run Environment Check^has question been asked
        S XPDIT=0,XPDSKPE="0^0"
        F  S XPDIT=$O(XPDT(XPDIT)) Q:'XPDIT  D  Q:'$D(XPDT)
        . S XPDA=+XPDT(XPDIT)
        . IF $$CheckLocal^TMGPAT4($name(^XTMP("XPDI",XPDA)),.Option)=1 do
        . . ;"if $GET(Option("VERBOSE"))'=1 QUIT
        . . WRITE "WARNING.  This code overwrites local mods!",!
        . . DO PRESS2GO^TMGUSRI2
        . ;"Check IF this Build has an Envir. Check
        . I $G(^XTMP("XPDI",XPDA,"PRE"))]"" D
        . . ;"Quit IF we already asked this question
        . . Q:$P(XPDSKPE,U,2)
        . . S $P(XPDSKPE,U,2)=1
        . . SET Y=$GET(Option("DO ENV CHECK"))
        . . IF Y'=1 SET XPDSKPE="1^1"
        . I $G(XPDQUIT) D ABRTALL^TMGXPDI(1,,.Msg) Q
        . D PKG^TMGXPDL1(XPDA,.Option,.Msg)

        ;"Global Package
        G:$D(XPDGP) ^XPDIGP
        I $D(XPDT),$D(^XPD(9.7,+XPDST,0)) do
        . DO AddMsg^TMGPAT2("Use INSTALL NAME: "_$P(^(0),U)_" to install this Distribution.",0,.Msg)
        . SET Option("INSTALL NAME")=$P(^(0),U)
EnDone
        Q


ST(Option,Msg)
        ;"Purpose: 'global input'
        ;"Input: Option -- PASS BY REFERENCE.  Entries are required unless marked optional
        ;"              Option("HFSNAME")=FilePathNameOnHFS
        ;"              Option("FORCE CONT LOAD")=1 <-- IF not given, then load won't continue
        ;"              Option("INTERACTIVE")=1 IF 1 then direct user input asked IF needed
        ;"       Msg -- PASS BY REFERENCE, an OUT PARAMETER.
        ;"              Errors are stored in Msg("ERROR",x)=Message
        ;"                                   Msg("ERROR")=count of last error
        ;"              Message are store in Msg(x)=Message
        ;"                                   Msg=count of last message+1
        ;"Results: 1=success, 0=error
        NEW result SET result=1
        N DIR,DIRUT,GR,IOP,X,Y,Z,%ZIS
        I '$D(^%ZIS(1,"B","HFS")) DO  GOTO STDone
        . DO AddMsg^TMGPAT2("You must have a device called 'HFS' in order to load a distribution!",1,.Msg)
        . S XPDQUIT=1
        D HOME^%ZIS
        SET Y=$GET(Option("HFSNAME"))
        IF (Y="")&($GET(Option("INTERACTIVE"))=1) do
        . SET DIR("A")="Enter a Host File",DIR("?")="Enter a filename and/or path to input Distribution."
        . SET Y=$$GETFNAME^TMGIOUTL(DIR("?"),"/tmp/","","","","",DIR("A")_": ")
        IF Y="" DO  GOTO STDone
        . DO AddMsg^TMGPAT2("No host file system filename provided!",1,.Msg)
        . S XPDQUIT=1
        S %ZIS="",%ZIS("HFSNAME")=Y,%ZIS("HFSMODE")="R",IOP="HFS"
        D ^%ZIS
        I POP do
        . DO AddMsg^TMGPAT2("Couldn't open file or HFS device!!",1,.Msg)
        . SET result=0
        ;"don't close device IF we have a global package, we need to bring in the globals now
        D GI(.Option,.Msg)  ;"Get file loaded in
        DO ^%ZISC:'$D(XPDGP)!$G(XPDQUIT)
STDone
        IF $GET(XPDQUIT)=1 SET result=0
        Q result


GI(Option,Msg)    ;"Get In
        ;"Purpose: Open file and load in.
        ;"Input: Option -- PASS BY REFERENCE.  Entries are required unless marked optional
        ;"              Option("HFSNAME")=FilePathNameOnHFS
        ;"              Option("FORCE CONT LOAD")=1 <-- IF not given, then load won't continue
        ;"       Msg -- PASS BY REFERENCE, an OUT PARAMETER.
        ;"              Errors are stored in Msg("ERROR",x)=Message
        ;"                                   Msg("ERROR")=count of last error
        ;"              Message are store in Msg(x)=Message
        ;"                                   Msg=count of last message+1

        N X,XPDSEQ,Y,Z
        U IO  ;"open KIDS text file for input
        DO DoRead(.X,1)
        DO DoRead(.Y,1)

        DO AddMsg^TMGPAT2(X,0,.Msg)
        DO AddMsg^TMGPAT2("Comment: "_Y,0,.Msg)
        S XPDST("H")=Y
        IF Y="Extracted from mail message" do
        . S XPDST("H1")=X
        ELSE  do
        . S XPDST("H1")=Y_"  ;Created on "_$P(X,"KIDS Distribution saved on ",2)
        ;"Z is the string of Builds in this file
        F X=1:1 DO  Q:Z=""
        . DO DoRead(.Z,1)
        . S Z=$P(Z,"**KIDS**",2,99)
        . Q:Z=""
        . S X(X)=Z
        U IO(0)
        I $G(X(1))="" DO  GOTO GIDone
        . DO AddMsg^TMGPAT2("This is not a Distribution HFS File!",1,.Msg)
        . S XPDQUIT=1

        ;"global package, SET XPDGP=flag;global^flag;global^...  flag=1 replace
        I $P(X(1),":")="GLOBALS" S XPDGP=$P(X(1),U,2,99),X(1)=$P(X(1),U)
        S XPDIT=0,X(1)=$P(X(1),":",2,99)

        DO AddMsg^TMGPAT2("This Distribution contains Transport Globals for the following Package(s):",0,.Msg)
        KILL XPDQUIT
        F X=1:1:X-1 DO  Q:$GET(XPDQUIT)=1
        . F Z=1:1 DO  Q:(Y="")!($GET(XPDQUIT)=1)
        . . S Y=$P(X(X),U,Z)
        . . Q:Y=""
        . . ;"can't install IF global exist, that means Build never finish install
        . . ;"INST will show name
        . . S XPDIT=XPDIT+1
        . . NEW temp SET temp=$$INST^TMGXPDL1(Y,.Option,.Msg)
        . . ;" //kt removed I temp=0 S XPDQUIT=1 Q
        IF $G(XPDQUIT) GOTO GIDone

        DO AddMsg^TMGPAT2("Distribution OK",0,.Msg)

        IF $D(XPDGP) DO DISP^TMGXPDIG(Msg)
        IF $GET(Option("FORCE CONT LOAD"))'=1 DO  GOTO GIDone
        . DO AddMsg^TMGPAT2("Option(""FORCE CONT LOAD"")=1 not found in passed options.",1,.Msg)
        . S XPDQUIT=1
        DO AddMsg^TMGPAT2("Loading Distribution...",0,.Msg)

        ;"reset expiration date to T+7 on transport global
        S ^XTMP("XPDI",0)=$$FMADD^XLFDT(DT,7)_U_DT
        ;"start reading the HFS again
        U IO
        DO DoRead(.X,0)
        DO DoRead(.Y,0)
        ;"R X:0,Y:0
        ;"the next read must be the INSTALL NAME
        I $D(XPDT("NM",Y))=0 DO  GOTO GIDone
        . DO AddMsg^TMGPAT2("Problem with load.  See messages.",1,.Msg)
        . S XPDQUIT=1
        I X'="**INSTALL NAME**" DO  GOTO GIDone
        . DO AddMsg^TMGPAT2("ERROR in HFS file format!",1,.Msg)
        . S XPDQUIT=1

        ;"XPDSEQ is the disk sequence number
        S %=XPDT("NM",Y)
        SET GR="^XTMP(""XPDI"","_+XPDT(%)_","
        SET XPDSEQ=1
        ;"X=global ref, Y=global value.
        F  DO DoRead(.X,0) Q:X="**END**"  D  I $D(DIRUT) S XPDQUIT=1 Q
        . DO DoRead(.Y,0)
        . I X="**INSTALL NAME**" D  Q
        . . S %=+$G(XPDT("NM",Y))
        . . SET GR=""  ;"//kt added  Allows ignoring parts of multipatch not needed
        . . ;"I '% S DIRUT=1 Q  ;"//kt
        . . I '% Q  ;"//kt
        . . S GR="^XTMP(""XPDI"","_+XPDT(%)_","
        . IF GR'="" S @(GR_X)=Y
        U IO(0)
GIDone
        Q

DoRead(S,timeOut)
        ;"Purpose: Do Read, but strip trailling #13 IF needed.
        ;"Input: S -- pass by reference, and OUT PARAMETER
        ;"       timeOut -- time out var to pass to READ command
        ;"Results: none
        read S:timeOut
        NEW l SET l=$LENGTH(S)
        NEW ch SET ch=$ASCII($EXTRACT(S,l))
        IF ch=13 SET S=$EXTRACT(S,1,l-1)
        QUIT


NEXTD   I ^%ZOSF("OS")'["MSM" U IO(0) W !!,"Error in disk, ABORTING load!!" S XPDQUIT=1 Q
        N DIR
        ;"close current device
        C IO U IO(0)
        S XPDSEQ=XPDSEQ+1,DIR(0)="E"
        S DIR("A")="Insert the next diskette, #"_XPDSEQ_", and Press the return key"
        S DIR("?")="This distribution is continued on another diskette"
        D ^DIR Q:$D(DIRUT)
        W "  OK",!
        ;MSM specific code to open HFS
        O @(""""_IO_""":"_IOPAR) U IO
        ;"R X:0,Y:0
        DO DoRead(.X,1)
        DO DoRead(.Y,1)

        ;"QUIT IF comments are not the same on each diskette
        G:Y'=XPDST("H") NEXTQ
        ;"QUIT IF not the expected sequence, Z is for the blank line
        ;"R Y:0,Z:0
        DO DoRead(.Y,1)
        DO DoRead(.Z,1)
        G:Y'=("**SEQ**:"_XPDSEQ) NEXTQ
        Q
        ;
NEXTQ   U IO(0) W !!,"This is NOT the correct diskette!!  The comment on this diskette is:",!,X,!!
        S XPDSEQ=XPDSEQ-1
        G NEXTD
        ;
NONE    W !!,"**NOTHING LOADED**",!
        Q


USER
        ;"Purpose: Ask user questions questions before running silent EN1^TMGXPDIL

        NEW Options
        NEW Option,Msg
        NEW DIR
        SET DIR("A")="Enter a Host File"
        SET DIR("?")="Enter a filename and/or path to input Distribution."
        IF $GET(TMGPATNM)'="" SET Y=TMGPATNM  ;"allow preset file name
        SET Option("HFSNAME")=$$GETFNAME^TMGIOUTL(DIR("?"),"/tmp/","","","","",DIR("A")_": ")

        NEW %
        SET %=2
        WRITE "Do Environmental check" DO YN^DICN WRITE !
        IF %=-1 GOTO UDone
        SET Option("DO ENV CHECK")=(%=1)

        SET %=1
        WRITE "Force continue load" DO YN^DICN WRITE !
        IF %=-1 GOTO UDone
        SET Option("FORCE CONT LOAD")=(%=1)

        DO EN1(.Option,.Msg)
        IF $DATA(Msg) DO ZWRITE^TMGZWR("Msg")

UDone
