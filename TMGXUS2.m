TMGXUS2   ;TMG/kst/Altered version of XUS2 ;03/25/06, 2/2/14
         ;;1.0;TMG-LIB;**1**;12/23/05
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
XUS2    ;SF/RWF - TO CHECK OR RETURN USER ATTRIBUTES ;07/15/2003  12:20
        ;;8.0;KERNEL;**59,180,313**;Jul 10, 1995
        G XUS2^XUVERIFY ;All check or return user attributes moved to XUVERIFY
USER    G USER^XUVERIFY
EDIT    G EDIT^XUVERIFY
        Q
        ;
ACCED   ; ACCESS CODE EDIT from DD
        N DIR,DIR0,XUAUTO I "Nn"[$E(X,1) S X="" Q
        I "Yy"'[$E(X,1) K X Q
        S XUAUTO=($P($G(^XTV(8989.3,1,3)),U,1)="y"),XUH=""
AC1     D CLR,AUTO:XUAUTO,AASK:'XUAUTO G OUT:$D(DIRUT) D REASK G OUT:$D(DIRUT),AC1:'XUK D CLR,AST(XUH)
        G OUT
        ;
AASK    N X,XUU X ^%ZOSF("EOFF")
AASK1   W "Enter a NEW ACCESS CODE <Hidden>: " D GET Q:$D(DIRUT)
        I X="@" D DEL G:Y'=1 DIRUT S XUH="" Q
        ;"K. Toppenberg modified 11-19-04 to relax requirements
        I X[$C(34)!(X[";")!(X["^")!(X[":")!($L(X)>20)!($L(X)<5)!(X="MAIL-BOX") D CLR W *7,$$AVHLPTXT(1) D AHELP G AASK1
        ;"//kt I X[$C(34)!(X[";")!(X["^")!(X[":")!(X'?.UNP)!($L(X)>20)!($L(X)<6)!(X="MAIL-BOX") D CLR W *7,$$AVHLPTXT(1) D AHELP G AASK1
        ;"//kt I 'XUAUTO,((X?6.20A)!(X?6.20N)) D CLR W *7,"ACCESS CODE must be a mix of alpha and numerics.",! G AASK1
        S XUU=X,X=$$EN^XUSHSH(X),XUH=X,XMB(1)=$O(^VA(200,"A",XUH,0))
        I XMB(1),XMB(1)'=DA S XMB="XUS ACCESS CODE VIOLATION",XMB(1)=$P(^VA(200,XMB(1),0),"^"),XMDUN="Security" D ^XMB
        I $D(^VA(200,"AOLD",XUH))!$D(^VA(200,"A",XUH)) D CLR W *7,"This has been used previously as an ACCESS CODE.",! G AASK1
        Q
        ;
REASK   S XUK=1 Q:XUH=""  D CLR X ^%ZOSF("EOFF")
        F XUK=3:-1:1 W "Please re-type the NEW code to show that I have it right: " D GET G:$D(DIRUT) DIRUT D ^XUSHSH Q:(XUH=X)  D CLR W "This doesn't match.  Try again!",!,*7
        S:XUH'=X XUK=0
        Q
        ;
AST(XUH)        ;Change ACCESS CODE and index.
        W "OK, Access code has been changed!"
        ;S XUU=$P(^VA(200,DA,0),"^",3),$P(^VA(200,DA,0),"^",3)=XUH
        ;I XUU]"" F XUI=0:0 S X=XUU S XUI=$O(^DD(200,2,1,XUI)) Q:XUI'>0  X ^(XUI,2)
        ;I XUH]"" F XUI=0:0 S X=XUH S XUI=$O(^DD(200,2,1,XUI)) Q:XUI'>0  X ^(XUI,1)
        N FDA,IEN,ERR
        S IEN=DA_","
        S FDA(200,IEN,2)=XUH D FILE^DIE("","FDA","ERR")
        W !,"The VERIFY CODE has been deleted as a security measure.",!,"The user will have to enter a NEW one the next time they sign-on.",*7 D VST("",1)
        I $D(^XMB(3.7,DA,0))[0 S Y=DA D NEW^XM ;Make sure has a Mailbox
        Q
        ;
GET     ;Get the user input and convert case.
        S X=$$ACCEPT^XUS Q:X="@"  G:(X["^")!('$L(X)) DIRUT
        S X=$$UP^XLFSTR(X)
        Q
        ;
DIRUT   S DIRUT=1
        Q
        ;
CLR     I '$D(DDS) W ! Q
        N DX,DY
        D CLRMSG^DDS S DX=0,DY=DDSHBX+1 X IOXY
        Q
        ;
NEWCODE D REASK I XUK W !,"OK, remember this code for next time!"
        G OUT
        ;
CVC     ;From XUS1
        W !,"You must change your VERIFY CODE at this time." S DA=DUZ,X="Y"
VERED   ; VERIFY CODE EDIT From DD
        N DIR,DIR0 I "Nn"[$E(X,1) S X="" Q
        I "Yy"'[$E(X,1) K X Q
        S XUH=""
VC1     D CLR,VASK G OUT:$D(DIRUT) D REASK G OUT:$D(DIRUT),VC1:'XUK D CLR,VST(XUH,1)
        D CALL^XUSERP(DA,2)
        G OUT
        ;
VASK    N X,XUU X ^%ZOSF("EOFF") G:'$$CHKCUR() DIRUT D CLR
VASK1   W "Enter a NEW VERIFY CODE: " D GET Q:$D(DIRUT)
        I '$D(XUNC),(X="@") D DEL G:Y'=1 DIRUT S XUH="" Q
        D CLR S XUU=X,X=$$EN^XUSHSH(X),XUH=X,Y=$$VCHK(XUU,XUH) I +Y W *7,$P(Y,U,2,9),! D:+Y=1 VHELP G VASK1
        Q
        ;
VCHK(S,EC)      ;Call with String and Encripted versions
        ;Updated per VHA directive 6210 Strong Passwords
        ;"Kevin Toppenberg modified this 11-19-04 to relax password ("verify code") requirements.
        ;"  .. now it must just be length 8-20
        N PUNC,NA S PUNC="~`!@#$%&*()_-+=|\{}[]'<>,.?/"
        S NA("FILE")=200,NA("FIELD")=.01,NA("IENS")=DA_",",NA=$$HLNAME^XLFNAME(.NA)
        I ($L(S)<5)!($L(S)>20)!(S[";")!(S["^")!(S[":") Q "1^"_$$AVHLPTXT
        ;"//I ($L(S)<8)!($L(S)>20)!(S'?.UNP)!(S[";")!(S["^")!(S[":") Q "1^"_$$AVHLPTXT
        ;"//kt I (S?8.20A)!(S?8.20N)!(S?8.20P)!(S?8.20AN)!(S?8.20AP)!(S?8.20NP) Q "2^VERIFY CODE must be a mix of alpha and numerics and punctuation."
        ;"//kt I $D(^VA(200,DA,.1)),EC=$P(^(.1),U,2) Q "3^This code is the same as the current one."
        ;"//kt I $D(^VA(200,DA,"VOLD",EC)) Q "4^This has been used previously as the VERIFY CODE."
        ;"//kt I EC=$P(^VA(200,DA,0),U,3) Q "5^VERIFY CODE must be different than the ACCESS CODE."
        ;"//kt I S[$P(NA,"^")!(S[$P(NA,"^",2)) Q "6^Name cannot be part of code."
        Q 0
        ;
VST(XUH,%)      W:$L(XUH)&% !,"OK, Verify code has been changed!"
        ;S XUU=$P($G(^VA(200,DA,.1)),U,2) S $P(^VA(200,DA,.1),"^",1,2)=$H_"^"_XUH
        ;I XUU]"" F XUI=0:0 S X=XUU S XUI=$O(^DD(200,11,1,XUI)) Q:XUI'>0  X ^(XUI,2)
        ;I XUH]"" F XUI=0:0 S X=XUH S XUI=$O(^DD(200,11,1,XUI)) Q:XUI'>0  X ^(XUI,1)
        N FDA,IEN,ERR S IEN=DA_","
        S:XUH="" XUH="@" ;11.2 get triggerd
        S FDA(200,IEN,11)=XUH D FILE^DIE("","FDA","ERR")
        I $D(ERR) D ^%ZTER
        S:DA=DUZ DUZ("NEWCODE")=XUH Q
        ;
DEL     ;
        X ^%ZOSF("EON") W "@",*7 S DIR(0)="Y",DIR("A")="Sure you want to delete" D ^DIR I Y'=1 W:$X>55 !?9 W *7,"  <Nothing Deleted>"
        Q
        ;
AUTO    ;
        X ^%ZOSF("EON") F XUK=1:1:3 D GEN Q:(Y=1)!($D(DIRUT))
        K DIR
        Q
        ;
GEN     ;Generate a ACCESS code
        S XUU=$$AC^XUS4 S X=$$EN^XUSHSH(XUU),XUH=X I $D(^VA(200,"A",X))!$D(^VA(200,"AOLD",X)) G GEN
        D CLR W "The NEW ACCESS CODE is: ",XUU,"   This is ",XUK," of 3 tries."
YN      S Y=1 Q:XUK=3  S DIR(0)="YA",DIR("A")=" Do you want to keep this one? ",DIR("B")="YES",DIR("?",1)="If you don't like this code, we can auto-generate another.",DIR("?")="Remember you only get 3 tries!"
        D ^DIR Q:(Y=1)!$D(DIRUT)  D CLR W:XUK=2 "O.K. You'll have to keep the next one!",!
        Q
        ;
AHELP   S XUU=$$AC^XUS4 S X=$$EN^XUSHSH(XUU) I $D(^VA(200,"A",X))!$D(^VA(200,"AOLD",X)) G AHELP
        W !,"Here is an example of an acceptable Access Code: ",XUU,!
        Q
        ;
VHELP   S XUU=$$VC^XUS4 S X=$$EN^XUSHSH(XUU) I ($P($G(^VA(200,DA,0)),U,3)=X)!$D(^VA(200,DA,"VOLD",X)) G VHELP
        W !,"Here is an example of an acceptable Verify Code: ",XUU,!
        Q
        ;
OUT     ;
        K DUOUT S:$D(DIRUT) DUOUT=1
        X ^%ZOSF("EON") W !
        K DIR,DIRUT,XUKO,XUAUTO,XUU,XUH,XUK,XUI S X=""
        Q
        ;
CHKCUR()        ;Check user knows current code, Return 1 if OK to continue
        Q:DA'=DUZ 1 ;Only ask user
        Q:$P($G(^VA(200,DA,.1)),U,2)="" 1 ;Must have an old one
        S XUK=0 D CLR
CHK1    W "Please enter your CURRENT verify code: " D GET Q:$D(DIRUT) 0
        I $P(^VA(200,DA,.1),U,2)=$$EN^XUSHSH(X) Q 1
        D CLR W "Sorry that is not correct!",!
        S XUK=XUK+1 G:XUK<3 CHK1
        Q 0
        ;
BRCVC(XV1,XV2)  ;Broker change VC, return 0 IF good, '1^msg' IF bad.
        N XUU,XUH
        Q:$G(DUZ)'>0 "1^Bad DUZ" S DA=DUZ,XUH=$$EN^XUSHSH(XV2)
        I $P($G(^VA(200,DUZ,.1)),"^",2)'=$$EN^XUSHSH(XV1) Q "1^Sorry that isn't the correct current code"
        S Y=$$VCHK(XV2,XUH) Q:Y Y
        D VST(XUH,0),CALL^XUSERP(DA,2)
        Q 0
        ;
AVHLPTXT(%)     ;
        Q "Enter "_$S($G(%):"6-20",1:"8-20")_" characters mixed alphanumeric and punctuation (except '^', ';', ':')."
        ;
