TMGXPDIL ;TMG/kst/Custom version of XPDIL ;2/2/14, 8/9/22
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
EN1(OPTION,MSG)
  ;"Purpose: Provide an API for KIDS load a distribution
  ;"Input: OPTION -- PASS BY REFERENCE.  Entries are required unless marked optional
  ;"              OPTION("HFSNAME")=FilePathNameOnHFS
  ;"              OPTION("FORCE CONT LOAD")=1 <-- IF not given, then load won't continue
  ;"              OPTION("DO ENV CHECK")=#  1=do check, 0=don't DO check
  ;"              OPTION("VERBOSE")=1 for output
  ;"       MSG -- PASS BY REFERANCE, an OUT PARAMETER                            
  ;"              Errors are stored in MSG("ERROR",x)=Message
  ;"                                   MSG("ERROR")=count of last error
  ;"              Message are store in MSG(x)=Message
  ;"                                   MSG=count of last message+1
  ;"Output: OPTION -- OPTION("INSTALL NAME")=Name to use to install package
  ;
  NEW POP,XPDA,XPDST,XPDIT,XPDT,XPDGP,XPDQUIT,XPDREQAB,XPDSKPE
  SET:'$D(DT) DT=$$DT^XLFDT SET:'$D(U) U="^"
  SET XPDST=0
  SET MSG=+$GET(MSG,1)
  ;
  NEW TEMP SET TEMP=$$ST(.OPTION,.MSG)  ;"Load in patch
  IF (TEMP=0)!($G(XPDQUIT)) DO  GOTO ENDN
  . DO ABRTALL^TMGXPDI(1,,.MSG)  ;"(not interactive)
  . DO ADDMSG^TMGPAT2("**NOTHING LOADED**",1,.MSG)
  ;                    
  ;"XPDST= starting Build
  ;"XPDT("DA",ien)=seq # to install
  ;"XPDT("NM",build name)=seq #
  ;"XPDT(seq #)=ien^Build name
  ;"XPDT("GP",global)= 1-replace, 0-overwrite^ien
  ;"XPDGP=globals from a Global Package
  ;"XPDSKPE=1 don't run Environment Check^has question been asked
  SET XPDIT=0,XPDSKPE="0^0"
  FOR  SET XPDIT=$ORDER(XPDT(XPDIT)) QUIT:'XPDIT  D  QUIT:'$D(XPDT)
  . SET XPDA=+XPDT(XPDIT)
  . IF $$CHECKLOCAL^TMGPAT4($NAME(^XTMP("XPDI",XPDA)),.OPTION)=1 DO
  . . ;"if $GET(OPTION("VERBOSE"))'=1 QUIT
  . . WRITE "WARNING.  This code overwrites local mods!",!
  . . DO PRESS2GO^TMGUSRI2
  . ;"Check IF this Build has an Envir. Check
  . IF $G(^XTMP("XPDI",XPDA,"PRE"))]"" D
  . . ;"Quit IF we already asked this question
  . . QUIT:$PIECE(XPDSKPE,U,2)
  . . SET $PIECE(XPDSKPE,U,2)=1
  . . SET Y=$GET(OPTION("DO ENV CHECK"))
  . . IF Y'=1 SET XPDSKPE="1^1"
  . IF $G(XPDQUIT) D ABRTALL^TMGXPDI(1,,.MSG) Q
  . DO PKG^TMGXPDL1(XPDA,.OPTION,.MSG)
  ;
  ;"Global Package
  GOTO:$D(XPDGP) ^XPDIGP
  IF $DATA(XPDT),$DATA(^XPD(9.7,+XPDST,0)) DO
  . DO ADDMSG^TMGPAT2("Use INSTALL NAME: "_$PIECE(^(0),U)_" to install this Distribution.",0,.MSG)
  . SET OPTION("INSTALL NAME")=$PIECE(^(0),U)
ENDN ;
  QUIT
  ;
ST(OPTION,MSG)  ;   
  ;"Purpose: 'global input'
  ;"Input: OPTION -- PASS BY REFERENCE.  Entries are required unless marked optional
  ;"              OPTION("HFSNAME")=FilePathNameOnHFS
  ;"              OPTION("FORCE CONT LOAD")=1 <-- IF not given, then load won't continue
  ;"              OPTION("INTERACTIVE")=1 IF 1 then direct user input asked IF needed
  ;"              OPTION("Dos2Unix")=1.  Optional.  Default=1.  If 1, then Dos2Unix is run on file before opening.  
  ;"       MSG -- PASS BY REFERENCE, an OUT PARAMETER.
  ;"              Errors are stored in MSG("ERROR",x)=Message
  ;"                                   MSG("ERROR")=count of last error
  ;"              Message are store in MSG(x)=Message
  ;"                                   MSG=count of last message+1
  ;"RESULTs: 1=success, 0=error
  NEW RESULT SET RESULT=1        
  N DIR,DIRUT,GR,IOP,X,Y,Z,%ZIS
  IF '$D(^DD(3.5,0)) SET RESULT=$$OPEN(.OPTION,.MSG) GOTO STDN  
  IF '$D(^%ZIS(1,"B","HFS")) DO  GOTO STDN
  . DO ADDMSG^TMGPAT2("You must have a device called 'HFS' in order to load a distribution!",1,.MSG)
  . S XPDQUIT=1
  DO HOME^%ZIS
  SET Y=$GET(OPTION("HFSNAME"))
  IF (Y="")&($GET(OPTION("INTERACTIVE"))=1) DO
  . SET DIR("A")="Enter a Host File",DIR("?")="Enter a filename and/or path to input Distribution."
  . SET Y=$$GETFNAME^TMGIOUTL(DIR("?"),"/tmp/","","","","",DIR("A")_": ")
  IF Y="" DO  GOTO STDN
  . DO ADDMSG^TMGPAT2("No host file system filename provided!",1,.MSG)
  . SET XPDQUIT=1
  IF $GET(OPTION("Dos2Unix"),1)=1 DO
  . NEW TEMP SET TEMP=$$DOS2UNIX^TMGKERNL  
  . IF TEMP=0 QUIT  ;"success
  . SET MSG(MSG)="Error attempting Dos2Unix "_Y,MSG=MSG+1
  SET %ZIS="",%ZIS("HFSNAME")=Y,%ZIS("HFSMODE")="R",IOP="HFS"
  DO ^%ZIS
  IF POP DO
  . DO ADDMSG^TMGPAT2("Couldn't open file or HFS device!!",1,.MSG)
  . SET RESULT=0
  ;"don't close device IF we have a global package, we need to bring in the globals now
  DO GI(.OPTION,.MSG)  ;"Get file loaded in                                                            
  DO ^%ZISC:'$D(XPDGP)!$G(XPDQUIT)
STDN  ;       
  IF $GET(XPDQUIT)=1 SET RESULT=0
  Q RESULT
  ; 	
OPEN(OPTION,MSG) ;"use open command  -- ;Loader for case when no device file, e.g. during Virgin Install
  NEW RESULT SET RESULT=1        
  N IO,IOPAR,DIR,DIRUT,DTOUT,DUOUT
  S DIR(0)="F^1:79",DIR("A")="Device Name"
  S DIR("?",1)="Device Name is either the name of the HFS file or the name of the HFS Device.",DIR("?",2)="i.e.  for MSM enter  51",DIR("?")="      for DSM enter  DISK$USER::[ANONYMOUS]:KRN8.KID"
  D ^DIR I $D(DIRUT) S POP=1,RESULT=0 Q
  S IO=Y,DIR(0)="FO^1:79",DIR("A")="Device Parameters"
  S DIR("?",1)="Device Parameter is the Open parameter this M operating system needs to",DIR("?",2)="open the Device Name.",DIR("?",3)="i.e. for MSM enter  (""B:\KRN8.KID"":""R"")",DIR("?")="     for DSM enter  READONLY"
  D ^DIR I $D(DTOUT)!$D(DUOUT) S POP=1,RESULT=0 Q
  S IOPAR=Y
  XECUTE "O IO:"_IOPAR_":10" E  U $P W !,"Couldn't open ",IO S POP=1,RESULT=0 Q
  S IO(0)=$P
  D GI D ^%ZISC
  Q RESULT
  ;
GI(OPTION,MSG)    ;"Get In
  ;"Purpose: Open file and load in.
  ;"Input: OPTION -- PASS BY REFERENCE.  Entries are required unless marked optional
  ;"              OPTION("HFSNAME")=FilePathNameOnHFS
  ;"              OPTION("FORCE CONT LOAD")=1 <-- IF not given, then load won't continue
  ;"       MSG -- PASS BY REFERENCE, an OUT PARAMETER.
  ;"              Errors are stored in MSG("ERROR",x)=Message
  ;"                                   MSG("ERROR")=count of last error
  ;"              Message are store in MSG(x)=Message
  ;"                                   MSG=count of last message+1    
  ;"NOTE: Uses XPDQUIT,XPDST,XPDIT,XPDGP,XPDT in global scope
  ;"RESULT: NONE  
  NEW X,XPDSEQ,Y,Z
  USE IO  ;"open KIDS text file for input
  DO DOREAD(.X,1)
  DO DOREAD(.Y,1)
  ;
  DO ADDMSG^TMGPAT2(X,0,.MSG)
  DO ADDMSG^TMGPAT2("Comment: "_Y,0,.MSG)
  SET XPDST("H")=Y
  IF Y="Extracted from mail message" DO
  . SET XPDST("H1")=X
  ELSE  DO
  . SET XPDST("H1")=Y_"  ;Created on "_$PIECE(X,"KIDS Distribution saved on ",2)
  ;"Z is the string of Builds in this file
  FOR X=1:1 DO  QUIT:Z=""
  . DO DOREAD(.Z,1)
  . S Z=$PIECE(Z,"**KIDS**",2,99)
  . QUIT:Z=""
  . SET X(X)=Z
  USE IO(0)
  IF $GET(X(1))="" DO  GOTO GIDN
  . DO ADDMSG^TMGPAT2("This is not a Distribution HFS File!",1,.MSG)
  . SET XPDQUIT=1
  ;
  ;"global package, SET XPDGP=flag;global^flag;global^...  flag=1 replace
  IF $PIECE(X(1),":")="GLOBALS" SET XPDGP=$PIECE(X(1),U,2,99),X(1)=$PIECE(X(1),U)
  SET XPDIT=0,X(1)=$PIECE(X(1),":",2,99)
  ;
  DO ADDMSG^TMGPAT2("This Distribution contains Transport Globals for the following Package(s):",0,.MSG)
  KILL XPDQUIT
  FOR X=1:1:X-1 DO  QUIT:$GET(XPDQUIT)=1
  . FOR Z=1:1 DO  QUIT:(Y="")!($GET(XPDQUIT)=1)
  . . SET Y=$PIECE(X(X),U,Z)
  . . QUIT:Y=""
  . . ;"can't install IF global exist, that means Build never finish install
  . . ;"INST will show name
  . . SET XPDIT=XPDIT+1
  . . NEW TEMP SET TEMP=$$INST^TMGXPDL1(Y,.OPTION,.MSG)
  . . ;" //kt removed I TEMP=0 S XPDQUIT=1 Q
  IF $GET(XPDQUIT) GOTO GIDN
  ; 
  DO ADDMSG^TMGPAT2("Distribution OK",0,.MSG)
  ;
  IF $DATA(XPDGP) DO DISP^TMGXPDIG(MSG)
  IF $GET(OPTION("FORCE CONT LOAD"))'=1 DO  GOTO GIDN
  . DO ADDMSG^TMGPAT2("OPTION(""FORCE CONT LOAD"")=1 not found in passed options.",1,.MSG)
  . SET XPDQUIT=1
  DO ADDMSG^TMGPAT2("Loading Distribution...",0,.MSG)
  ;
  ;"reset expiration date to T+7 on transport global
  SET ^XTMP("XPDI",0)=$$FMADD^XLFDT(DT,7)_U_DT
  ;"start reading the HFS again
  USE IO
  DO DOREAD(.X,0)
  DO DOREAD(.Y,0)
  ;"R X:0,Y:0
  ;"the next read must be the INSTALL NAME
  IF $DATA(XPDT("NM",Y))=0 DO  GOTO GIDN
  . DO ADDMSG^TMGPAT2("Problem with load.  See messages.",1,.MSG)
  . SET XPDQUIT=1
  IF X'="**INSTALL NAME**" DO  GOTO GIDN
  . DO ADDMSG^TMGPAT2("ERROR in HFS file format!",1,.MSG)
  . SET XPDQUIT=1
  ;
  ;"XPDSEQ is the disk sequence number
  SET %=XPDT("NM",Y)     
  NEW GR  ;"//kt
  SET GR="^XTMP(""XPDI"","_+XPDT(%)_","
  SET XPDSEQ=1
  ;"X=global ref, Y=global value.
  FOR  DO DOREAD(.X,0) QUIT:X="**END**"  DO  IF $D(DIRUT) SET XPDQUIT=1 QUIT
  . DO DOREAD(.Y,0)
  . IF X="**INSTALL NAME**" D  Q
  . . SET %=+$G(XPDT("NM",Y))
  . . SET GR=""  ;"//kt added  Allows ignoring parts of multipatch not needed
  . . ;"I '% S DIRUT=1 Q  ;"//kt
  . . IF '% Q  ;"//kt
  . . SET GR="^XTMP(""XPDI"","_+XPDT(%)_","
  . IF GR'="" S @(GR_X)=Y
  USE IO(0)
GIDN    ;
  QUIT
  ;
DOREAD(S,TIMEOUT)
  ;"Purpose: Do Read, but strip trailling #13 IF needed.
  ;"Input: S -- pass by reference, and OUT PARAMETER
  ;"       TIMEOUT -- time out var to pass to READ command
  ;"RESULTs: none
  READ S:TIMEOUT
  NEW l SET l=$LENGTH(S)
  NEW CH SET CH=$ASCII($EXTRACT(S,l))
  IF CH=13 SET S=$EXTRACT(S,1,l-1)
  QUIT                                                     
  ;
  ;"NEXTD  ;"//kt note: I can't see that this block of code is used  -- delete later...
  ;"  IF ^%ZOSF("OS")'["MSM" DO  QUIT
  ;"  . USE IO(0) 
  ;"  . WRITE !!,"Error in disk, ABORTING load!!" 
  ;"  . SET XPDQUIT=1 
  ;"  N DIR
  ;"  ;"close current device
  ;"  C IO U IO(0)
  ;"  SET XPDSEQ=XPDSEQ+1,DIR(0)="E"
  ;"  SET DIR("A")="Insert the next diskette, #"_XPDSEQ_", and Press the return key"
  ;"  SET DIR("?")="This distribution is continued on another diskette"
  ;"  D ^DIR QUIT:$D(DIRUT)
  ;"  W "  OK",!
  ;"  ;MSM specific code to open HFS
  ;"  O @(""""_IO_""":"_IOPAR) U IO
  ;"  ;"R X:0,Y:0
  ;"  DO DOREAD(.X,1)
  ;"  DO DOREAD(.Y,1)
  ;"
  ;"  ;"QUIT IF comments are not the same on each diskette
  ;"  G:Y'=XPDST("H") NEXTQ
  ;"  ;"QUIT IF not the expected sequence, Z is for the blank line
  ;"  ;"R Y:0,Z:0
  ;"  DO DOREAD(.Y,1)
  ;"  DO DOREAD(.Z,1)
  ;"  G:Y'=("**SEQ**:"_XPDSEQ) NEXTQ
  ;"  Q
  ;"  ;
  ;"NEXTQ  ;
  ;"  U IO(0) W !!,"This is NOT the correct diskette!!  The comment on this diskette is:",!,X,!!
  ;"  SET XPDSEQ=XPDSEQ-1
  ;"  G NEXTD
  ;"  ;
  ;"NONE ;
  ;"  W !!,"**NOTHING LOADED**",!
  ;"  QUIT
  ;"  ;
USER
  ;"Purpose: Ask user questions questions before running silent EN1^TMGXPDIL
  NEW OPTION,MSG
  NEW DIR
  SET DIR("A")="Enter a Host File"
  SET DIR("?")="Enter a filename and/or path to input Distribution."
  IF $GET(TMGPATNM)'="" SET Y=TMGPATNM  ;"allow preset file name
  SET OPTION("HFSNAME")=$$GETFNAME^TMGIOUTL(DIR("?"),"/tmp/","","","","",DIR("A")_": ")
  ;"
  NEW % SET %=2
  WRITE "Do Environmental check" DO YN^DICN WRITE !
  IF %=-1 GOTO USDN
  SET OPTION("DO ENV CHECK")=(%=1)
  ;
  SET %=1
  WRITE "Force continue load" DO YN^DICN WRITE !
  IF %=-1 GOTO USDN
  SET OPTION("FORCE CONT LOAD")=(%=1)
  ;
  DO EN1(.OPTION,.MSG)
  IF $DATA(MSG) DO ZWRITE^TMGZWR("MSG")
  ;
USDN ;
  QUIT
