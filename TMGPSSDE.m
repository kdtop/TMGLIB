TMGPSSDE ;TMG/kst/Custom version of PSSDEE ;03/25/06, 2/2/14
         ;;1.0;TMG-LIB;**1**;04/25/04
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
PSSDEE  ;BIR/WRT-MASTER DRUG ENTER/EDIT ROUTINE ;01/21/00
        ;;1.0;PHARMACY DATA MANAGEMENT;**3,5,15,16,20,22,28,32,34,33,38,57,47,68,61**;9/30/97

        ;"*****************************************************************
        ;"* Custom version of code by Kevin Toppenberg, MD
        ;"* to allow customization of the code.
        ;"*
        ;"*****************************************************************

        ;"Reference to REACT1^PSNOUT supported by DBIA #2080
        ;"Reference to $$UP^XLFSTR(X) supported by DBIA #10104
        ;"Reference to $$PSJDF^PSNAPIS(P1,P3) supported by DBIA #2531
        ;
BEGIN   SET PSSFLAG=0
        DO ^PSSDEE2  ;"kill vars
        SET PSSZ=1
        F PSSXX=1:1 DO  QUIT:PSSFLAG
        . KILL DA
        . DO ASK  ;" ask users all questions
DONE    DO ^PSSDEE2 ;" KILL vars
        KILL PSSFLAG
        QUIT
        ;
        ;"=================================================================
ASK     W !
        SET DIC="^PSDRUG("
        SET DIC(0)="QEALMNTV" ;"query/echo/ask/learn=OK/multIndex/IntNumOK/T->searchAllIndexes/verify
        SET DLAYGO=50  ;"force allowing adding record to file 50
        SET DIC("T")="" ;"present every match to the lookup value
        DO ^DIC
        KILL DIC
        IF Y<0 SET PSSFLAG=1 QUIT
        ;
        SET (FLG1,FLG2,FLG3,FLG4,FLG5,FLG6,FLG7,FLAG,FLGKY,FLGOI)=0
        KILL ^TMP($J,"ADD")
        KILL ^TMP($J,"SOL")
        ;
        SET DA=+Y
        SET DISPDRG=DA
        L +^PSDRUG(DISPDRG):0
        IF '$T W !,$C(7),"Another person is editing this one." QUIT
        SET PSSHUIDG=1
        SET PSSNEW=$P(Y,"^",3)
        DO USE
        DO NOPE
        DO COMMON
        DO DEA
        DO MF
        KILL PSSHUIDG
        DO DRG^PSSHUIDG(DISPDRG,PSSNEW)
        L -^PSDRUG(DISPDRG)
        KILL FLG3,PSSNEW
        QUIT
        ;
        ;"=================================================================
COMMON  SET DIE="^PSDRUG("
        SET DR="[PSSCOMMON]"
        DO ^DIE
        QUIT:$DATA(Y)!($DATA(DTOUT))
        W:'$DATA(Y) !,"PRICE PER DISPENSE UNIT: "
        S:'$DATA(^PSDRUG(DA,660)) $P(^PSDRUG(DA,660),"^",6)=""
        W:'$DATA(Y) $P(^PSDRUG(DA,660),"^",6)
        DO DEA
        DO CK
        DO ASKND
        DO OIKILL^PSSDEE1
        DO COMMON1
        QUIT
        ;
COMMON1 W !,"Just a reminder...you are editing ",$P(^PSDRUG(DISPDRG,0),"^"),"."
        SET (PSSVVDA,DA)=DISPDRG
        DO DOSN^PSSDOS
        SET DA=PSSVVDA
        KILL PSSVVDA
        DO USE
        DO APP
        DO ORDITM^PSSDEE1
        QUIT
        ;
CK      DO DSPY^PSSDEE1
        SET FLGNDF=0
        QUIT
        ;
ASKND   SET %=-1
        IF $DATA(^XUSEC("PSNMGR",DUZ)) do
        . DO MESSAGE^PSSDEE1
        . W !!,"Do you wish to match/rematch to NATIONAL DRUG file"
        . SET %=1
        . S:FLGMTH=1 %=2
        . DO YN^DICN
        IF %=0 W !,"If you answer ""yes"", you will attempt to match to NDF." G ASKND
        IF %=2 KILL X,Y QUIT
        IF %<0 KILL X,Y QUIT
        IF %=1 do
        . DO RSET^PSSDEE1
        . DO EN1^PSSUTIL(DISPDRG,1)
        . SET X="PSNOUT"
        . X ^%ZOSF("TEST")
        . IF  do
        . . DO REACT1^PSNOUT
        . . SET DA=DISPDRG
        . . IF $DATA(^PSDRUG(DA,"ND")),$P(^PSDRUG(DA,"ND"),"^",2)]"" DO ONE
        QUIT
        ;
ONE     SET PSNP=$G(^PSDRUG(DA,"I"))
        IF PSNP,PSNP<DT QUIT
        W !,"You have just VERIFIED this match and MERGED the entry."
        DO CKDF
        DO EN2^PSSUTIL(DISPDRG,1)
        S:'$DATA(OLDDF) OLDDF=""
        IF OLDDF'=NEWDF do
        . SET FLGNDF=1
        . DO WR
        QUIT
        ;
CKDF    SET NWND=^PSDRUG(DA,"ND")
        SET NWPC1=$P(NWND,"^",1)
        SET NWPC3=$P(NWND,"^",3)
        SET DA=NWPC1
        SET K=NWPC3
        SET X=$$PSJDF^PSNAPIS(DA,K)
        SET NEWDF=$P(X,"^",2)
        SET DA=DISPDRG
        N PSSK
        DO PKIND^PSSDDUT2
        QUIT
        ;
NOPE    SET ZAPFLG=0
        IF '$DATA(^PSDRUG(DA,"ND")),$DATA(^PSDRUG(DA,2)),$P(^PSDRUG(DA,2),"^",1)']"" DO DFNULL
        IF '$DATA(^PSDRUG(DA,"ND")),'$DATA(^PSDRUG(DA,2)) DO DFNULL
        IF $DATA(^PSDRUG(DA,"ND")),$P(^PSDRUG(DA,"ND"),"^",2)']"",$DATA(^PSDRUG(DA,2)),$P(^PSDRUG(DA,2),"^",1)']"" DO DFNULL
        QUIT
        ;
DFNULL  SET OLDDF=""
        SET ZAPFLG=1
        QUIT
        ;
ZAPIT   IF $DATA(ZAPFLG),ZAPFLG=1,FLGNDF=1,OLDDF'=NEWDF DO CKIV^PSSDEE1
        QUIT
        ;
APP     W !!,"MARK THIS DRUG AND EDIT IT FOR: "
        DO CHOOSE
        QUIT
        ;
CHOOSE  IF $DATA(^XUSEC("PSORPH",DUZ))!($DATA(^XUSEC("PSXCMOPMGR",DUZ))) W !,"O  - Outpatient" SET FLG1=1
        IF $DATA(^XUSEC("PSJU MGR",DUZ)) W !,"U  - Unit Dose" SET FLG2=1
        IF $DATA(^XUSEC("PSJI MGR",DUZ)) W !,"I  - IV" SET FLG3=1
        IF $DATA(^XUSEC("PSGWMGR",DUZ)) W !,"W  - Ward Stock" SET FLG4=1
        IF $DATA(^XUSEC("PSAMGR",DUZ))!($DATA(^XUSEC("PSA ORDERS",DUZ))) W !,"D  - Drug Accountability" SET FLG5=1
        IF $DATA(^XUSEC("PSDMGR",DUZ)) W !,"C  - Controlled Substances" SET FLG6=1
        IF $DATA(^XUSEC("PSORPH",DUZ)) W !,"X  - Non-VA Med" SET FLG7=1
        IF FLG1,FLG2,FLG3,FLG4,FLG5,FLG6 SET FLAG=1
        IF FLAG W !,"A  - ALL"
        W !
        IF 'FLG1,'FLG2,'FLG3,'FLG4,'FLG5,'FLG6,'FLG7 DO  QUIT
        . W !,"You DO not have the proper keys to continue. Sorry, this concludes your editing session.",!
        . SET FLGKY=1
        . KILL DIRUT,X
        IF FLGKY'=1 D
        . KILL DIR
        . SET DIR(0)="FO^1:30"
        . SET DIR("A")="Enter your choice(s) separated by commas "
        . F  DO ^DIR QUIT:$$CHECK($$UP^XLFSTR(X))
        . SET PSSANS=X
        . SET PSSANS=$$UP^XLFSTR(PSSANS)
        . DO BRANCH
        . DO BRANCH1
        QUIT
        ;
CHECK(X)        ;" Validates Application Use response
        N CHECK,I,C
        SET CHECK=1 IF X=""!(Y["^")!($DATA(DIRUT)) QUIT CHECK
        F I=1:1:$L(X,",") D
        . SET C=$P(X,",",I) W !?43,C," - "
        . IF C="O",FLG1 W "Outpatient" QUIT
        . IF C="U",FLG2 W "Unit Dose" QUIT
        . IF C="I",FLG3 W "IV" QUIT
        . IF C="W",FLG4 W "Ward Stock" QUIT
        . IF C="D",FLG5 W "Drug Accountability" QUIT
        . IF C="C",FLG6 W "Controlled Substances" QUIT
        . IF C="X",FLG7 W "Non-VA Med" QUIT
        . W "Invalid Entry",$C(7) SET CHECK=0
        QUIT CHECK
        ;
BRANCH  D:PSSANS["O" OP
        D:PSSANS["U" UD
        D:PSSANS["I" IV
        D:PSSANS["W" WS
        D:PSSANS["D" DACCT
        D:PSSANS["C" CS
        D:PSSANS["X" NVM
        QUIT
        ;
BRANCH1 IF FLAG,PSSANS["A" do
        . DO OP
        . DO UD
        . DO IV
        . DO WS
        . DO DACCT
        . DO CS
        . DO NVM
        QUIT
        ;
OP      IF FLG1 D
        . W !,"** You are NOW editing OUTPATIENT fields. **"
        . SET PSIUDA=DA
        . SET PSIUX="O^Outpatient Pharmacy"
        . DO ^PSSGIU
        . IF %=1 D
        . . SET DIE="^PSDRUG(",DR="[PSSOP]"
        . . DO ^DIE
        . . KILL DIR
        . . DO OPEI
        . . DO ASKCMOP
        . . SET X="PSOCLO1"
        . . X ^%ZOSF("TEST")
        . . IF  DO ASKCLOZ SET FLGOI=1
        IF FLG1 DO CKCMOP
        QUIT
        ;
CKCMOP  IF $P($G(^PSDRUG(DISPDRG,2)),"^",3)'["O" do
        . S:$DATA(^PSDRUG(DISPDRG,3)) $P(^PSDRUG(DISPDRG,3),"^",1)=0
        . K:$DATA(^PSDRUG("AQ",DISPDRG)) ^PSDRUG("AQ",DISPDRG)
        . SET DA=DISPDRG
        . DO ^PSSREF
        QUIT
        ;
UD      IF FLG2 do
        . W !,"** You are NOW editing UNIT DOSE fields. **"
        . SET PSIUDA=DA
        . SET PSIUX="U^Unit Dose"
        . DO ^PSSGIU
        . IF %=1 do
        . . SET DIE="^PSDRUG("
        . . SET DR="62.05;212.2"
        . . DO ^DIE
        . . SET DIE="^PSDRUG("
        . . SET DR="212"
        . . SET DR(2,50.0212)=".01;1"
        . . DO ^DIE
        . . SET FLGOI=1
        QUIT
        ;
IV      IF FLG3
        W !,"** You are NOW editing IV fields. **"
        S (PSIUDA,PSSDA)=DA
        SET PSIUX="I^IV"
        DO ^PSSGIU
        IF %=1 DO IV1 SET FLGOI=1
        QUIT
        ;
IV1     KILL PSSIVOUT ;"This variable controls the selection process loop.
        W !,"Edit Additives or Solutions: "
        KILL DIR
        SET DIR(0)="SO^A:ADDITIVES;S:SOLUTIONS;"
        DO ^DIR
        QUIT:$DATA(DIRUT)
        SET PSSASK=Y(0)
        D:PSSASK="ADDITIVES" ENA^PSSVIDRG
        D:PSSASK="SOLUTIONS" ENS^PSSVIDRG
        IF '$DATA(PSSIVOUT) G IV1
        KILL PSSIVOUT
        QUIT
        ;
WS      IF FLG4
        W !,"** You are NOW editing WARD STOCK fields. **"
        SET DIE="^PSDRUG("
        SET DR="300;301;302"
        DO ^DIE
        QUIT
        ;
DACCT   IF FLG5
        W !,"** You are NOW editing DRUG ACCOUNTABILITY fields. **"
        SET DIE="^PSDRUG("
        SET DR="441"
        DO ^DIE
        SET DIE="^PSDRUG("
        SET DR="9"
        SET DR(2,50.1)="1;2;400;401;402;403;404;405"
        DO ^DIE
        QUIT
        ;
CS      IF FLG6
        W !,"** You are NOW Marking/Unmarking for CONTROLLED SUBS. **"
        SET PSIUDA=DA
        SET PSIUX="N^Controlled Substances"
        DO ^PSSGIU
        QUIT
        ;
NVM     IF FLG7
        W !,"** You are NOW Marking/Unmarking for NON-VA MEDS. **"
        SET PSIUDA=DA
        SET PSIUX="X^Non-VA Med"
        DO ^PSSGIU
        QUIT
        ;
ASKCMOP IF $DATA(^XUSEC("PSXCMOPMGR",DUZ)) do
        . W !!,"Do you wish to mark to transmit to CMOP? "
        . KILL DIR
        . SET DIR(0)="Y"
        . SET DIR("?")="If you answer ""yes"", you will attempt to mark this drug to transmit to CMOP."
        DO ^DIR
        IF "Nn"[X KILL X,Y,DIRUT QUIT
        IF "Yy"[X do
        . SET PSXFL=0
        . DO TEXT^PSSMARK
        . H 7
        . N PSXUDA
        . S (PSXUM,PSXUDA)=DA
        . SET PSXLOC=$P(^PSDRUG(DA,0),"^")
        . SET PSXGOOD=0
        . SET PSXF=0
        . SET PSXBT=0
        . DO BLD^PSSMARK
        . DO PICK2^PSSMARK
        . SET DA=PSXUDA
        QUIT
        ;
ASKCLOZ W !!,"Do you wish to mark/unmark as a LAB MONITOR or CLOZAPINE DRUG? "
        KILL DIR
        SET DIR(0)="Y"
        SET DIR("?")="If you answer ""yes"", you will have the opportunity to edit LAB MONITOR or CLOZAPINE fields."
        DO ^DIR
        IF "Nn"[X KILL X,Y,DIRUT QUIT
        IF "Yy"[X SET NFLAG=0 DO MONCLOZ
        QUIT
        ;
MONCLOZ KILL PSSAST
        DO FLASH
        W !,"Mark/Unmark for Lab Monitor or Clozapine: "
        KILL DIR
        SET DIR(0)="S^L:LAB MONITOR;C:CLOZAPINE;"
        DO ^DIR
        QUIT:$DATA(DIRUT)
        SET PSSAST=Y(0)
        D:PSSAST="LAB MONITOR" ^PSSLAB
        D:PSSAST="CLOZAPINE" CLOZ
        QUIT
        ;
FLASH   KILL LMFLAG,CLFALG,WHICH
        SET WHICH=$P($G(^PSDRUG(DISPDRG,"CLOZ1")),"^")
        SET LMFLAG=0
        SET CLFLAG=0
        IF WHICH="PSOCLO1" SET CLFLAG=1
        IF WHICH'="PSOCLO1" S:WHICH'="" LMFLAG=1
        QUIT
        ;
CLOZ    QUIT:NFLAG
        QUIT:$DATA(DTOUT)
        QUIT:$DATA(DIRUT)
        QUIT:$DATA(DUOUT)
        W !,"** You are NOW editing CLOZAPINE fields. **"
        DO ^PSSCLDRG
        QUIT
        ;
USE     KILL PACK
        SET PACK=""
        S:$P($G(^PSDRUG(DISPDRG,"PSG")),"^",2)]"" PACK="W"
        IF $DATA(^PSDRUG(DISPDRG,2)) SET PACK=PACK_$P(^PSDRUG(DISPDRG,2),"^",3)
        IF PACK'="" D
        . W $C(7) N XX W !! F XX=1:1:79 W "*"
        . W !,"This entry is marked for the following PHARMACY packages: "
        . DO USE1
        QUIT
        ;
USE1    W:PACK["O" !," Outpatient"
        W:PACK["U" !," Unit Dose"
        W:PACK["I" !," IV"
        W:PACK["W" !," Ward Stock"
        W:PACK["D" !," Drug Accountability"
        W:PACK["N" !," Controlled Substances"
        W:PACK["X" !," Non-VA Med"
        W:'$DATA(PACK) !," NONE"
        IF PACK'["O",PACK'["U",PACK'["I",PACK'["W",PACK'["D",PACK'["N",PACK'["X" W !," NONE"
        QUIT
        ;
WR      IF ^XMB("NETNAME")'["CMOP-" do
        . IF OLDDF="" QUIT
        . W !,"The dosage form has changed from "_OLDDF_" to "_NEWDF_" due to",!
        . w "matching/rematching to NDF.",!
        . w "You will need to rematch to Orderable Item.",!
        QUIT
PRIMDRG IF $DATA(^PS(59.7,1,20)),$P(^PS(59.7,1,20),"^",1)=4!($P(^PS(59.7,1,20),"^",1)=4.5) do
        . IF $DATA(^PSDRUG(DISPDRG,2)) do
        . . SET VAR=$P(^PSDRUG(DISPDRG,2),"^",3)
        . . IF VAR["U"!(VAR["I") do
        . . . DO PRIM1
        QUIT
        ;
PRIM1   W !!,"You need to match this drug to ""PRIMARY DRUG"" file as well.",!
        SET DIE="^PSDRUG(",DR="64"
        SET DA=DISPDRG
        DO ^DIE
        KILL VAR
        QUIT
        ;
MF      IF $P($G(^PS(59.7,1,80)),"^",2)>1 IF $DATA(^PSDRUG(DISPDRG,2)) DO
        . SET PSSOR=$P(^PSDRUG(DISPDRG,2),"^",1)
        . IF PSSOR]"" DO
        . . DO EN^PSSPOIDT(PSSOR)
        . . DO EN2^PSSHL1(PSSOR,"MUP")
        QUIT
        ;
MFA     IF $P($G(^PS(59.7,1,80)),"^",2)>1 do
        . SET PSSOR=$P(^PS(52.6,ENTRY,0),"^",11)
        . SET PSSDD=$P(^PS(52.6,ENTRY,0),"^",2)
        . IF PSSOR]"" do
        . . DO EN^PSSPOIDT(PSSOR)
        . . DO EN2^PSSHL1(PSSOR,"MUP")
        . . DO MFDD
        QUIT
        ;
MFS     IF $P($G(^PS(59.7,1,80)),"^",2)>1 do
        . SET PSSOR=$P(^PS(52.7,ENTRY,0),"^",11)
        . SET PSSDD=$P(^PS(52.7,ENTRY,0),"^",2)
        . IF PSSOR]"" do
        . . DO EN^PSSPOIDT(PSSOR)
        . . DO EN2^PSSHL1(PSSOR,"MUP")
        . . DO MFDD
        QUIT
        ;
MFDD    IF $DATA(^PSDRUG(PSSDD,2)) do
        . SET PSSOR=$P(^PSDRUG(PSSDD,2),"^",1)
        . IF PSSOR]"" do
        . . DO EN^PSSPOIDT(PSSOR)
        . . DO EN2^PSSHL1(PSSOR,"MUP")
        QUIT
        ;
OPEI    IF $DATA(^PSDRUG(DISPDRG,"ND")),$P(^PSDRUG(DISPDRG,"ND"),"^",10)]"" do
        . SET DIE="^PSDRUG("
        . SET DR="28"
        . SET DA=DISPDRG
        . DO ^DIE
        QUIT
        ;
DEA     ;
        IF $P($G(^PSDRUG(DISPDRG,3)),"^")=1,($P(^PSDRUG(DISPDRG,0),"^",3)[1!($P(^(0),"^",3)[2)) DO DSH
        QUIT
        ;
DSH     W !!,"****************************************************************************"
        W !,"This entry contains a ""1"" or a ""2"" in the ""DEA, SPECIAL HDLG""",!
        w "field, therefore this item has been UNMARKED for CMOP transmission."
        W !,"****************************************************************************",!
        S $P(^PSDRUG(DISPDRG,3),"^")=0
        KILL ^PSDRUG("AQ",DISPDRG)
        SET DA=DISPDRG
        N %
        DO ^PSSREF
        QUIT
