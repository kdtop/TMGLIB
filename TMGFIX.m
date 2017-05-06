;(Scratch code to fix various specific problems over time.), 2/2/14
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
;"=====================================================================
;"================================================================

ENV     ;Establish Routine Environment
        N DDH,DIR,X,Y,ZTENV,ZTKEY,ZTNAME,ZTSK,XUTMUCI
        D ENV^XUTMUTL Q:'$D(ZTENV)
        ;
        NEW DIC,X,Y
        SET DIC=.401
        SET DIC(0)="MAEQ"
        DO ^DIC WRITE !
        IF +Y'>0 QUIT
        NEW templIEN SET tempLIEN=+Y
        NEW IEN SET IEN=""
        FOR  SET IEN=$ORDER(^DIBT(tempLIEN,1,IEN)) QUIT:IEN=""  do
        . DO KILLTSK(IEN)

        QUIT


KILLTSK(ZTSK)
        ;"W !
        ;"S XUTMT(0)="AL"
        ;"D ^XUTMT
        I 'ZTSK K ^TMP($J,"XUTMT") Q
        ;"I ZTSK["-"!(ZTSK[",") D ^XUTMD1 Q:$D(DTOUT)  G SELECT
        S XUTMT=ZTSK,XUTMT(0)="R3"
        D ^XUTMT
        ;
STATUS  ;Report On Status Of Task And Whether User May Delete It
        I $D(ZTSK(.11))#2,ZTSK(.11)="UNDEFINED",$O(ZTSK(.3))="" W !!?5,"That task is not defined.",$C(7) G SELECT
        I $D(ZTSK(.11))#2,ZTSK(.11)="UNDEFINED",$O(ZTSK(.3))="TASK",$O(ZTSK("TASK"))="" W !!?5,"That task is running and has no record." G SELECT
        I $D(ZTSK(.11))#2,ZTSK(.11)="UNDEFINED" W !!?5,"That task is scheduled but has no record." G CONFIRM:ZTKEY G SELECT
        ;
S5      I $D(ZTSK(.11))#2,$O(ZTSK(.3))="" W !!?5,"That task's record is incomplete." G CONFIRM:ZTKEY G SELECT
        I $D(ZTSK(.11))#2,$O(ZTSK(.3))="TASK",$O(ZTSK("TASK"))="" W !!?5,"That task is running and has an incomplete record." G SELECT
        I $D(ZTSK(.11))#2 W !!?5,"That task is scheduled, but has an incomplete record." G CONFIRM:ZTKEY G SELECT
        ;
S9      I $O(ZTSK(.3))="TASK",$O(ZTSK("TASK"))="" W !!?5,"That task is running." G SELECT
        I 'ZTKEY,$S($P(ZTSK(0),U,11)_","_$P(ZTSK(0),U,12)=XUTMUCI:DUZ'=$P(ZTSK(0),U,3),1:ZTNAME'=$P(ZTSK(0),U,10)) W !!?5,"You may only delete your own tasks." G SELECT
        ;
CONFIRM ;Prompt User To Confirm Unscheduling
        I $S($D(ZTSK(.11))[0:1,1:ZTSK(.11)'="UNDEFINED") W ! D EN^XUTMTP(ZTSK)
        ;"W !
        ;"K DIR
        ;"S DIR(0)="Y"
        ;"S DIR("A")="Are you sure you want to delete this task"
        ;"S DIR("B")="NO"
        ;"S DIR("?")="     Answer YES to delete the task."
        ;"D ^DIR
        ;"I 'Y W !!?5,"Tasks NOT deleted!"
        ;"I $D(DTOUT) W $C(7) Q
        ;"K DIR,DIRUT,DTOUT,DUOUT
        ;"I 'Y G SELECT
        ;
DELETE  ;Delete Task
        I $D(ZTSK(0))#2,ZTSK(0)["ZTSK^XQ1",$P(ZTSK(0),U,11)_","_$P(ZTSK(0),U,12)=XUTMUCI,$P(ZTSK(0),U,8)]"" D
        . F DA=0:0 S DA=$O(^DIC(19.2,DA)) Q:DA'>0  I $G(^DIC(19.2,DA,1))=ZTSK D
        . . N DIE S DIE="^DIC(19.2,",DR="2///@;12///@" D ^DIE Q
        . Q
        S XUTMT=ZTSK,XUTMT(0)="D"
        D ^XUTMT
        W !!?5,"Deleted!"
        G SELECT
        ;

SELECT
        QUIT

KILLTMPL
        NEW X,Y,DIC
        SET DIC=.401
        SET DIC(0)="MAEQ"
        DO ^DIC WRITE !
        IF +Y'>0 WRITE "goodbye.",! QUIT
        NEW TMPL SET TMPL=+Y
        NEW file SET file=$PIECE($GET(^DIBT(TMPL,0)),"^",4)
        IF file'=8925 DO  QUIT
        . WRITE "That file doesn't refer to file 8925.  That is all this function can work with!",!
        NEW % SET %=2
        WRITE "Delete all the records referred to in this sort template?" DO YN^DICN WRITE !
        IF %'=1 WRITE "goodbye.",! QUIT
        NEW MIN,MAX
        SET MIN=$ORDER(^DIBT(TMPL,1,0))
        SET MAX=$ORDER(^DIBT(TMPL,1,""),-1)
        NEW TMGCT SET TMGCT=0
        NEW STIME SET STIME=$H
        NEW TMGIEN SET TMGIEN=0
        FOR  SET TMGIEN=$ORDER(^DIBT(TMPL,1,TMGIEN)) QUIT:(+TMGIEN'>0)  do
        . ;"WRITE TMGIEN,! QUIT
        . NEW TMGFDA SET TMGFDA(8925,TMGIEN_",",.01)="@"
        . ;"new TMGFDA SET TMGFDA(8925,TMGIEN_",",.05)="COMPLETED"
        . NEW TMGMSG
        . DO FILE^DIE("E","TMGFDA","TMGMSG")
        . NEW RPTR SET RPTR=+$GET(^TMG("TMGSIPH","DOWNLOADED",8925,TMGIEN))
        . KILL ^TMG("TMGSIPH","DOWNLOADED",8925,TMGIEN)
        . KILL ^TMG("TMGSIPH","PT XLAT",8925,RPTR)
        . DO SHOWDIER^TMGDEBU2(.TMGMSG)
        . SET TMGCT=TMGCT+1
        . IF TMGCT>50 do
        . . DO PROGBAR^TMGUSRI2(TMGIEN,"Deleting records",MIN,MAX,70,STIME)
        . . SET TMGCT=0
        WRITE "goodbye.",!
        QUIT
        ;"
FIXXREF ;
        NEW FILENUM SET FILENUM=0
        FOR  SET FILENUM=$ORDER(^TMG("TMGSIPH","PT XLAT",FILENUM)) QUIT:(+FILENUM'>0)  DO
        . NEW RPTR SET RPTR=0
        . FOR  SET RPTR=$ORDER(^TMG("TMGSIPH","PT XLAT",FILENUM,RPTR)) QUIT:(+RPTR'>0)  DO
        . . NEW LPTR SET LPTR=+$GET(^TMG("TMGSIPH","PT XLAT",FILENUM,RPTR))
        . . IF LPTR'>0 WRITE "FILE ",FILENUM,",  REMOTE IEN=",RPTR," --> ?? LOCAL PTR",! QUIT
        . . IF $DATA(^TMG("TMGSIPH","DOWNLOADED",FILENUM,LPTR))=0 DO  QUIT
        . . . WRITE "FILE ",FILENUM,",  LOCAL IEN=",LPTR," --> Not downloaded??",!
        . . SET ^TMG("TMGSIPH","DOWNLOADED",FILENUM,LPTR)=RPTR
        . . WRITE "Set ",$NAME(^TMG("TMGSIPH","DOWNLOADED",FILENUM,LPTR)),"=",RPTR,!
        QUIT


 ;"    ; Note: ENTRY=DataPiece^PointedToFile^PointedToReference^IENDepth^[V]
 ;"    ; ONEREF will have multiple IEN entries IF IENDepth>1, e.g. '^SC(IEN,"S",IEN(2),1,IEN(3),"C")'
 ;"    ;        with order of IEN, IEN(2), IEN(3), ... etc.
FIXSUBFILES ;
        NEW FILENUM SET FILENUM=0
        NEW ABORT SET ABORT=0
        FOR  SET FILENUM=$ORDER(^TMG("TMGSIPH","DOWNLOADED",FILENUM)) QUIT:(+FILENUM'>0)!ABORT  DO
        . NEW TMP SET TMP=$$HASPTRSF^TMGFMUT2(FILENUM)
        . IF TMP DO
        . . WRITE "FILE ",FILENUM," has pointer subfiles.... probably needs fix.",!
        . . ;"IF $$DDOK^TMGSIPH1(JNUM,FILENUM)
        . . IF $$SETPTOUT^TMGSIPH1(FILENUM)
        . . NEW ONEREF SET ONEREF=""
        . . FOR  SET ONEREF=$ORDER(^TMG("TMGSIPH","DD",FILENUM,"PTR OUT",ONEREF)) QUIT:(ONEREF="")!ABORT  DO
        . . . ;"WRITE "ONEREF=",ONEREF,!
        . . . NEW ENTRY SET ENTRY=""
        . . . FOR  SET ENTRY=$ORDER(^TMG("TMGSIPH","DD",FILENUM,"PTR OUT",ONEREF,ENTRY)) QUIT:(ENTRY="")!ABORT  DO
        . . . . NEW IENDEPTH SET IENDEPTH=$PIECE(ENTRY,"^",4)
        . . . . IF IENDEPTH=1 QUIT
        . . . . NEW PCE SET PCE=+ENTRY
        . . . . ;"WRITE "  ENTRY=",ENTRY,!
        . . . . NEW IEN SET IEN=0
        . . . . NEW GREF SET GREF=^DIC(FILENUM,0,"GL") QUIT:(GREF="")
        . . . . NEW CGREF SET CGREF=$$CREF^DILF(GREF)
        . . . . FOR  SET IEN=$ORDER(@CGREF@(IEN)) QUIT:(+IEN'>0)!ABORT  DO
        . . . . . FOR  QUIT:($$IENCOMBO^TMGFMUT2(ONEREF,IENDEPTH,.IEN)'=1)!ABORT  DO
        . . . . . . SET ABORT=$$USRABORT^TMGUSRI2 QUIT:ABORT
        . . . . . . NEW TMPREF SET TMPREF=$NAME(@ONEREF)  ;"Puts IEN's from IEN array into name.
        . . . . . . NEW IENS SET IENS=$$GETIENS^TMGFMUT2(.IEN)
        . . . . . . IF $GET(^TMG("TMGSIPH","FIX",FILENUM,TMPREF))'="" QUIT  ;"Already fixed.
        . . . . . . NEW FROMFILE SET FROMFILE=$PIECE(ENTRY,"^",6)
        . . . . . . NEW PT SET PT=$PIECE($GET(@TMPREF),"^",PCE) ;"$$IENCOMBO sets up IEN(n).. needed for @REF
        . . . . . . NEW ISVIRT SET ISVIRT=($PIECE(ENTRY,"^",5)="V")
        . . . . . . NEW P2REF SET P2REF=$PIECE(ENTRY,"^",3)
        . . . . . . IF ISVIRT,$PIECE(PT,";",2)'=P2REF QUIT  ;"Loop to handle PTR with different ENTRY (V-Ptrs stored as IEN;OREF)
        . . . . . . SET PT=+PT QUIT:(PT'>0)
        . . . . . . NEW P2FILE SET P2FILE=$PIECE(ENTRY,"^",2)
        . . . . . . NEW FROMFLD SET FROMFLD=$PIECE(ENTRY,"^",7)
        . . . . . . NEW LPTR SET LPTR=$GET(^TMG("TMGSIPH","PT XLAT",P2FILE,PT),"??")
        . . . . . . WRITE "FILENUM: ",FILENUM," IENS=",IENS," ",TMPREF," --> PTR=",PT," in file: ",P2FILE," LPTR=",LPTR,!
        . . . . . . IF LPTR'="??" DO
        . . . . . . . IF (PT'=LPTR) SET $PIECE(@TMPREF,"^",PCE)=LPTR
        . . . . . . . SET ^TMG("TMGSIPH","FIX",FILENUM,TMPREF)=PT   ;"Store old value just in case...
        . . . . . . ELSE  DO
        . . . . . . . SET ^TMG("TMGSIPH","NEEDED RECORDS","PTOUT",P2FILE,PT,TMPREF,ENTRY)=""
        . . . . . . . ;"IF $$NEEDPTIN^TMGSIPH3(FILENUM) DO
        . . . . . . . ;". SET ^TMG("TMGSIPH","NEEDED RECORDS","PTIN",FILENUM,)=""
        . . . . . NEW TMPIEN SET TMPIEN=IEN KILL IEN SET IEN=TMPIEN ;"delete subnodes in array.
        DO PRESS2GO^TMGUSRI2
        QUIT


 ;"    ; Note: ENTRY=DataPiece^PointedToFile^PointedToReference^IENDepth^[V]
 ;"    ; ONEREF will have multiple IEN entries IF IENDepth>1, e.g. '^SC(IEN,"S",IEN(2),1,IEN(3),"C")'
 ;"    ;        with order of IEN, IEN(2), IEN(3), ... etc.
FIXDDSUBFILES ;
        NEW FILENUM SET FILENUM=0
        NEW ABORT SET ABORT=0
        FOR  SET FILENUM=$ORDER(^TMG("TMGSIPH","DOWNLOADED",FILENUM)) QUIT:(+FILENUM'>0)!ABORT  DO
        . ;"NEW TMP SET TMP=$$HASPTRSF^TMGFMUT2(FILENUM)
        . ;"IF TMP DO
        . WRITE "FILE ",FILENUM," has had DD reset.",!
        . IF $$SETPTOUT^TMGSIPH1(FILENUM)
        DO PRESS2GO^TMGUSRI2
        QUIT


FTIU
        NEW IEN SET IEN=0
        FOR  SET IEN=$O(^TIU(8925,IEN)) q:(+IEN'>0)  IF $D(^TIU(8925,IEN,"TEMP")) do
        . WRITE IEN
        . IF $D(^TIU(8925,IEN,"TEXT"))=0 DO  QUIT
        . . WRITE "NO TEXT"
        . . MERGE ^TIU(8925,IEN,"TEXT")=^TIU(8925,IEN,"TEMP")
        . . KILL ^TIU(8925,IEN,"TEMP")
        . . WRITE " -- FIXED",!
        . NEW SAME SET SAME=1
        . NEW j SET j=0
        . FOR  SET j=$o(^TIU(8925,IEN,"TEMP",j)) QUIT:(+j'>0)!(SAME=0)
        . . IF $G(^TIU(8925,IEN,"TEMP",j,0))'=$G(^TIU(8925,IEN,"TEXT",j,0)) SET SAME=0
        . WRITE " --> SAME=",SAME
        . IF SAME KILL ^TIU(8925,IEN,"TEMP") WRITE "  FIXED."
        . WRITE !
        QUIT

CKREC
         ;"^TMG("TMGSIPH","PT XLAT",FILENUM,RemoteIEN)=LocalIEN
        NEW DIC,X,Y
        SET DIC(0)="MAEQ"
        SET DIC=1
        DO ^DIC WRITE !
        IF +Y'>0 QUIT
        NEW ARRAY
        NEW OVERLAP
        SET OVERLAP=$$CKREC1F(+Y,.ARRAY)
        IF OVERLAP DO
        . WRITE "FILE #",+Y," has ",OVERLAP," overlapping records.",!
        QUIT


CKALLREC
        NEW FILENUM SET FILENUM=0
        NEW ARRAY
        FOR  SET FILENUM=$ORDER(^TMG("TMGSIPH","DOWNLOADED",FILENUM)) QUIT:(+FILENUM'>0)  DO
        . NEW OVERLAP
        . WRITE "Checking file #",FILENUM,"..."
        . SET OVERLAP=$$CKREC1F(FILENUM,.ARRAY)
        . WRITE "FILE #",FILENUM," has ",OVERLAP," overlapping records.",!
        MERGE ^TMG("TMGSIPH","OVERLAP")=ARRAY
        QUIT


CKREC1F(FILENUM,ARRAY)
        NEW CT SET CT=0
        NEW RPTR,LPTR
        SET RPTR=0
        FOR  SET RPTR=$ORDER(^TMG("TMGSIPH","PT XLAT",FILENUM,RPTR)) QUIT:(+RPTR'>0)  DO
        . SET LPTR=$GET(^TMG("TMGSIPH","PT XLAT",FILENUM,RPTR)) QUIT:LPTR=""
        . SET ARRAY(FILENUM,LPTR,RPTR)=""
        . SET CT=CT+1
        . IF CT#100=0 WRITE "." SET CT=0
        ;"Now delete all entries that are not doubled up.
        SET LPTR=0
        FOR  SET LPTR=$ORDER(ARRAY(FILENUM,LPTR)) QUIT:(LPTR="")  DO
        . NEW REF SET REF=$NAME(ARRAY(FILENUM,LPTR))
        . SET CT=$$LISTCT^TMGMISC2(REF)
        . IF CT=1 KILL @REF
        WRITE !
        QUIT $$LISTCT^TMGMISC2($NAME(ARRAY(FILENUM)))


SUMM
        NEW FILENUM SET FILENUM=0
        FOR  SET FILENUM=$ORDER(^TMG("TMGSIPH","OVERLAP",FILENUM)) QUIT:FILENUM=""  DO
        . NEW FNAME SET FNAME=$PIECE($GET(^DIC(FILENUM,0)),"^",1)
        . NEW REF SET REF=$NAME(^TMG("TMGSIPH","OVERLAP",FILENUM))
        . WRITE "FILE [",FNAME,"] has ",$$LISTCT^TMGMISC2(REF)," overlapping records",!
        QUIT


COMPRPC
        NEW DIC,X,Y
        NEW OPTION1,OPTION2
        SET DIC=19,DIC(0)="MAEQ"
        WRITE "First pick the OLDER entry to compaire",!
        DO ^DIC WRITE !
        IF Y=-1 GOTO CPRDN
        SET OPTION1=+Y
        WRITE !,"Now, pick the NEWER entry to compare",!
        DO ^DIC WRITE !
        IF Y=-1 GOTO CPRDN
        SET OPTION2=+Y
        NEW ARRAY1,ARRAY2
        NEW IEN
        SET IEN=0
        FOR  SET IEN=$ORDER(^DIC(19,OPTION1,"RPC",IEN)) QUIT:(+IEN'>0)  DO
        . NEW PRPC
        . SET PRPC=+$GET(^DIC(19,OPTION1,"RPC",IEN,0))
        . NEW NAME SET NAME=$PIECE($GET(^XWB(8994,PRPC,0)),"^",1)
        . IF NAME="" WRITE IEN," --> ??",!
        . ELSE  SET ARRAY1(NAME,IEN)=""

        SET IEN=0
        FOR  SET IEN=$ORDER(^DIC(19,OPTION2,"RPC",IEN)) QUIT:(+IEN'>0)  DO
        . NEW PRPC
        . SET PRPC=+$GET(^DIC(19,OPTION2,"RPC",IEN,0))
        . NEW NAME SET NAME=$PIECE($GET(^XWB(8994,PRPC,0)),"^",1)
        . IF NAME="" WRITE IEN," --> ??",!
        . ELSE  SET ARRAY2(NAME,IEN)=""

        NEW NAME SET NAME=""
        FOR  SET NAME=$ORDER(ARRAY1(NAME)) QUIT:(NAME="")  DO
        . IF $DATA(ARRAY2(NAME)) DO
        . . WRITE "Both have: ",NAME,!
        . . KILL ARRAY1(NAME),ARRAY2(NAME)

        NEW TEMP
        WRITE "OK.  Here are the entries in the OLDER option, not present in the NEW one.",!
        IF $DATA(ARRAY1) DO ZWRITE^TMGZWR("ARRAY1")
        ELSE  WRITE "(none)",!

        DO PRESS2GO^TMGUSRI2

        WRITE "OK.  Here are the entries in the NEWER option, not present in the old one.",!
        IF $DATA(ARRAY2) DO ZWRITE^TMGZWR("ARRAY2")
        ELSE  WRITE "(none)",!

        DO PRESS2GO^TMGUSRI2

CPRDN   WRITE "GOODBYE",!
        QUIT



FIXDRUG ;
        NEW IEN SET IEN=0
        FOR  SET IEN=$ORDER(^PSDRUG(IEN)) QUIT:(+IEN'>0)  do
        . IF $DATA(^PSDRUG(IEN,900))=0 QUIT
        . IF $DATA(^PSDRUG(IEN,900,0))'=0 WRITE "OK #"_IEN QUIT
        . WRITE "!"
        . NEW lastIEN SET lastIEN=$ORDER(^PSDRUG(IEN,900,""),-1)
        . SET ^PSDRUG(IEN,900,0)="^50.01A^"_lastIEN_"^"_lastIEN
        . WRITE !,"Fixed "_IEN
        QUIT
        
  ;"=====================================================
  ;"=====================================================
  ;" Keep Code in block below.  It took me about 5-6 hrs to write
  ;"=====================================================
  ;"=====================================================

COMMONREF(MODE,GENDER,GRAPH,ARRAY,pctl) ;
        ;"Purpose: Return array filled with data for percentile curves
        ;"Input: MODE -- 1 IF for age range 0-36 months, 2 IF age 2-20 yrs
        ;"       GENDER -- M OR F
        ;"       GRAPH -- Name of graph
        ;"       ARRAY -- PASS BY REFERENCE.  AN OUT PARAMETER.  PRIOR VALUES KILLED.
        ;"           ARRAY(%tile,Age)=x^y
        ;"Result: none
        NEW DIC,X,Y,L,M,S
        KILL ARRAY
        SET GENDER=$EXTRACT($$UP^XLFSTR(GENDER),1)
        IF (GENDER'="M")&(GENDER'="F") GOTO CMRFQT
        SET DIC=22713,DIC(0)="M",X=$GET(GRAPH)
        DO ^DIC
        IF +Y'>0 GOTO CMRFQT
        NEW IEN SET IEN=+Y
        NEW X0,XINC,XMAX
        IF MODE=1 SET X0=0,XINC=1,XMAX=36
        ELSE  SET X0=24,XINC=12,XMAX=240
        SET X=X0
        NEW ZARRAY,Z
        NEW ABORT SET ABORT=0
        FOR  DO  SET X=X+XINC QUIT:(X>XMAX)!ABORT        
        . IF +$$GETLMS^TMGGRC1(IEN,X,GENDER,.L,.M,.S)<0 SET ABORT=1 QUIT
        . NEW P SET P=pctl DO
        . . IF $GET(ZARRAY(P))="" SET ZARRAY(P)=$$PCTL2Z^TMGGRC1(P)
        . . SET Z=ZARRAY(P)        
        . . NEW VAL SET VAL=$$LMSZ2Y^TMGGRC1(L,M,S,Z)
        . . SET VAL=$JUSTIFY(VAL,0,2)
        . . SET ARRAY(P,X)=X_"^"_VAL
        ;
CMRFQT  QUIT
        ;

MAKEVS ;"Make fake vitals for a test patient.
        NEW DIC,X,Y,PCTL,AGE,GENDER,TMGERR,GRAPH,ARRAY,%
        SET DIC=2,DIC(0)="MEQ"
        SET DIC("A")="Enter name of **TEST** patient: "
        ;"SET X="ZZTEST,BABY"
        DO ^DIC WRITE !
        IF +Y'>0 QUIT
        SET DFN=+Y
        DO GETPAT^TMGGRC2(DFN,.AGE,.GENDER,.TMGERR)
        NEW DOB SET DOB=+$PIECE($GET(^DPT(DFN,0)),"^",3)
        IF $DATA(^GMR(120.5,"C",DFN)) DO  QUIT:(%=-1)
        . SET %=2
        . WRITE "Patient has other vitals already defined.  DELETE them all"
        . DO YN^DICN WRITE !
        . IF %'=1 QUIT
        . NEW DIK SET DIK="^GMR(120.5,"
        . NEW DA SET DA=0
        . FOR  SET DA=$ORDER(^GMR(120.5,"C",DFN,DA)) QUIT:(+DA'>0)  DO
        . . DO ^DIK
        . . WRITE "!"
        . WRITE !
        READ "Enter Percentile (9-95) :",PCTL:60,!
        KILL DIC
        NEW IEN
        FOR IEN=20,8,9 DO
        . ;"SET DIC=120.51,DIC(0)="MEQ"
        . ;"DO ^DIC WRITE !
        . ;"IF +Y'>0 QUIT
        . SET Y=IEN_"^"_$PIECE($GET(^GMRD(120.51,IEN,0)),"^",1)
        . WRITE !,$p(Y,"^",2)
        . DO MAKE1VS(DFN,GENDER,Y,PCTL) ;
        QUIT

MAKE1VS(DFN,GENDER,TYPE,PCTL) ;
        ;"TYPE -- IEN^Name from 120.51

        NEW GRAPH,UNIT,MULT,AGE
        NEW NAME SET NAME=$PIECE(TYPE,"^",2)
        IF NAME="CIRCUMFERENCE/GIRTH" DO
        . SET GRAPH(1)="HEAD CIRC BY AGE -- INFANT"
        . SET GRAPH(2)=""
        . SET UNIT="cm"
        . SET MULT=(1/2.54)
        ELSE  IF NAME="HEIGHT" DO
        . SET GRAPH(1)="LENGTH BY AGE -- INFANT"
        . SET GRAPH(2)="STATURE BY AGE"
        . SET UNIT="cm"
        . SET MULT=(1/2.54)
        ELSE  IF NAME="WEIGHT" DO
        . SET GRAPH(1)="WEIGHT BY AGE -- INFANT"
        . SET GRAPH(2)="WEIGHT BY AGE"
        . SET UNIT="kg"
        . SET MULT=2.2
        ELSE  DO  QUIT
        . WRITE "Unsupported graph.",!
        KILL DIC
        NEW ABORT SET ABORT=0
        FOR GRAPH=1,2 DO  QUIT:ABORT
        . SET DIC=22713,DIC(0)="M"
        . SET X=GRAPH(GRAPH) QUIT:X=""
        . DO ^DIC ;"WRITE !
        . IF +Y'>0 QUIT
        . ;"WRITE Y,!
        . SET GRAPH("N")=$PIECE(Y,"^",2)
        . DO COMMONREF(GRAPH,GENDER,GRAPH("N"),.ARRAY,PCTL) ;
        . IF $DATA(ARRAY)=0 QUIT
        . NEW AGE SET AGE=0
        . FOR  SET AGE=$ORDER(ARRAY(PCTL,AGE)) QUIT:(AGE="")!ABORT  DO
        . . NEW S SET S=ARRAY(PCTL,AGE)
        . . NEW DELTA SET DELTA=$P(S,"^",1)
        . . IF GRAPH=1,DELTA=36 QUIT
        . . IF GRAPH=2,DELTA=24 QUIT
        . . NEW VAL SET VAL=$P(S,"^",2)
        . . NEW NDATE,X1,X2,X,y
        . . SET X2=((DELTA*30.42)+.5)\1
        . . SET X1=DOB
        . . DO C^%DTC  ;"OUTPUT IN X
        . . SET Y=X DO DD^%DT  ;"OUPUT IN Y
        . . ;"WRITE "on DOB +",DELTA,": (",Y,")  ",VAL," ",UNIT,!
        . . SET ABORT=$$Add1VS(DFN,TYPE,VAL,Y,UNIT)
        QUIT

Add1VS(DFN,TYPE,VALUE,DATE,UNIT)
        ;"TYPE -- IEN^Name from 120.51
        ;"Result: 0 IF OK, 1 IF error
        SET VALUE=$JUSTIFY(VALUE,0,1)
        IF +$PIECE(VALUE,".",2)=0 SET VALUE=VALUE\1
        IF +TYPE=20 SET UNIT=$E(UNIT,1)
        SET VALUE=VALUE_UNIT
        WRITE $P(TYPE,"^",2),"  DATE: (",DATE,")  ",VALUE,!
        NEW RESULT SET RESULT=0
        NEW TMGFDA,TMGIEN,TMGMSG,AGE
        SET TMGFDA(120.5,"+1,",.01)=DATE_"@08:00"
        SET TMGFDA(120.5,"+1,",.02)="`"_DFN
        SET TMGFDA(120.5,"+1,",.03)="`"_+TYPE
        SET TMGFDA(120.5,"+1,",.04)="NOW"
        SET TMGFDA(120.5,"+1,",.05)="lo"
        SET TMGFDA(120.5,"+1,",.06)="`"_DUZ
        DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO A1VDN
        . DO SHOWDIER^TMGDEBU2(.TMGMSG)
        . SET RESULT=1
        NEW IEN SET IEN=TMGIEN(1)
        KILL TMGFDA
        SET TMGFDA(120.5,IEN_",",1.2)=VALUE
        DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO A1VDN
        . DO SHOWDIER^TMGDEBU2(.TMGMSG)
        . SET RESULT=1

A1VDN   QUIT RESULT

  ;"=====================================================
  ;"=====================================================


KILL
        NEW file SET file=0
        FOR  SET file=$ORDER(^DD(file)) QUIT:+file'>0  do
        . IF $PIECE($GET(^DD(file,.01,0)),"^",2)'["W" QUIT
        . NEW up SET up=+$GET(^DD(file,0,"UP"))
        . NEW upup SET upup=$GET(^DD(up,0,"UP"))
        . IF upup'="" QUIT
        . WRITE file," --> ",up
        . WRITE " (",$PIECE($GET(^DIC(up,0)),"^",1),")",!
        QUIT


GCDATA
        NEW I SET I=0
        FOR  SET I=$ORDER(^TMG(22713,I)) QUIT:I'>0  DO
        . NEW INDEX SET INDEX=0
        . FOR  SET INDEX=$ORDER(^TMG(22713,I,"D","B",INDEX)) QUIT:INDEX'>0  DO
        . . IF INDEX="0.99" DO
        . . . NEW ID SET ID=$ORDER(^TMG(22713,I,"D","B",INDEX,0))
        . . . SET ^TMG(22713,I,"D","B",.99,ID)=""
        . . . WRITE "WOULD KILL ^TMG(22713,"_I_",""D"",""B"",""0.99"")"
        QUIT


HL7DATA ;;0070,ABS,Abcess
        ;;0070,AMN,Amniotic fluid
        ;;0070,ASP,Aspirate
        ;;0070,BPH,Basophils
        ;;0070,BIFL,Bile fluid
        ;;0070,BLDA,Blood  arterial
        ;;0070,BBL,Blood bag
        ;;0070,BLDC,Blood  capillary
        ;;0070,BPU,Blood product unit
        ;;0070,BLDV,Blood  venous
        ;;0070,BON,Bone
        ;;0070,BRTH,Breath (use EXHLD)
        ;;0070,BRO,Bronchial
        ;;0070,BRN,Burn
        ;;0070,CALC,Calculus (=Stone)
        ;;0070,CDM,Cardiac muscle
        ;;0070,CNL,Cannula
        ;;0070,CTP,Catheter tip
        ;;0070,CSF,Cerebral spinal fluid
        ;;0070,CVM,Cervical mucus
        ;;0070,CVX,Cervix
        ;;0070,COL,Colostrum
        ;;0070,CBLD,Cord blood
        ;;0070,CNJT,Conjunctiva
        ;;0070,CUR,Curettage
        ;;0070,CYST,Cyst
        ;;0070,DIAF,Dialysis fluid
        ;;0070,DOSE,Dose med or substance
        ;;0070,DRN,Drain
        ;;0070,DUFL,Duodenal fluid
        ;;0070,EAR,Ear
        ;;0070,EARW,Ear wax (cerumen)
        ;;0070,ELT,Electrode
        ;;0070,ENDC,Endocardium
        ;;0070,ENDM,Endometrium
        ;;0070,EOS,Eosinophils
        ;;0070,RBC,Erythrocytes  HL7 Specifications for Electronic Laboratory-Based Reporting Page 59
        ;;0070,EYE,Eye
        ;;0070,EXHLD,Exhaled gas (=breath)
        ;;0070,FIB,Fibroblasts
        ;;0070,FLT,Filter
        ;;0070,FIST,Fistula
        ;;0070,FLU,Body fluid, unsp
        ;;0070,GAS,Gas
        ;;0070,GAST,Gastric fluid/contents
        ;;0070,GEN,Genital
        ;;0070,GENC,Genital cervix
        ;;0070,GENL,Genital lochia
        ;;0070,GENV,Genital vaginal
        ;;0070,HAR,Hair
        ;;0070,IHG,Inhaled Gas
        ;;0070,IT,Intubation tube
        ;;0070,ISLT,Isolate
        ;;0070,LAM,Lamella
        ;;0070,WBC,Leukocytes
        ;;0070,LN,Line
        ;;0070,LNA,Line arterial
        ;;0070,LNV,Line venous
        ;;0070,LIQ,Liquid NOS
        ;;0070,LYM,Lymphocytes
        ;;0070,MAC,Macrophages
        ;;0070,MAR,Marrow
        ;;0070,MEC,Meconium
        ;;0070,MBLD,Menstrual blood
        ;;0070,MLK,Milk
        ;;0070,MILK,Breast milk
        ;;0070,NAIL,Nail
        ;;0070,NOS,Nose (nasal passage)
        ;;0070,ORH,Other
        ;;0070,PAFL,Pancreatic fluid
        ;;0070,PAT,Patient
        ;;0070,PRT,Peritoneal fluid /ascites
        ;;0070,PLC,Placenta
        ;;0070,PLAS,Plasma
        ;;0070,PLB,Plasma bag
        ;;0070,PLR ,Pleural fluid (thoracentesis fld)
        ;;0070,PMN,Polymorphonuclear neutrophils
        ;;0070,PPP,Platelet poor plasma
        ;;0070,PRP,Platelet rich plasma
        ;;0070,PUS,Pus
        ;;0070,RT,Route of medicine
        ;;0070,SAL,Saliva
        ;;0070,SEM,Seminal fluid
        ;;0070,SER,Serum
        ;;0070,SKN,Skin
        ;;0070,SKM,Skeletal muscle
        ;;0070,SPRM,Spermatozoa
        ;;0070,SPT,Sputum
        ;;0070,SPTC,Sputum - coughed
        ;;0070,SPTT,Sputum - tracheal aspirate
        ;;0070,STON,Stone (use CALC)
        ;;0070,STL,Stool = Fecal
        ;;0070,SWT,Sweat
        ;;0070,SNV,Synovial fluid (Joint fluid)
        ;;0070,TEAR,Tears
        ;;0070,THRT,Throat
        ;;0070,THRB,Thrombocyte (platelet)
        ;;0070,TISS,Tissue
        ;;0070,TISG,Tissue gall bladder
        ;;0070,TLGI,Tissue large intestine
        ;;0070,TLNG,Tissue lung
        ;;0070,TISPL,Tissue placenta
        ;;0070,TSMI,Tissue small intestine
        ;;0070,TISU,Tissue ulcer
        ;;0070,TUB,Tube NOS
        ;;0070,ULC,Ulcer
        ;;0070,UMB,Umbilical blood
        ;;0070,UMED,Unknown medicine
        ;;0070,URTH,Urethra
        ;;0070,UR,Urine
        ;;0070,URC,Urine clean catch
        ;;0070,URT,Urine catheter
        ;;0070,URNS,Urine sediment
        ;;0070,USUB,Unknown substance
        ;;0070,VOM,Vomitus
        ;;0070,BLD,Whole blood
        ;;0070,BDY,Whole body
        ;;0070,WAT,Water
        ;;0070,WICK,Wick
        ;;0070,WND,Wound
        ;;0070,WNDA,Wound abscess
        ;;0070,WNDE,Wound exudate
        ;;0070,WNDD,Wound drainage
        ;;0070,XXX,To be specified in another part of the message
        ;;<DONE>

AddTabl
        NEW DIC,X,Y
        NEW TMGFDA,TMGMSG,TMGIEN,TMGIENS
        NEW I
        NEW DONE SET DONE=0
        FOR I=0:1 DO  QUIT:(DONE)
        . NEW LINE SET LINE=$TEXT(HL7DATA+I^TMGFIX)
        . SET LINE=$PIECE(LINE,";;",2)
        . IF LINE["<DONE>" SET DONE=1 QUIT
        . SET LINE=$PIECE(LINE,",",2,3)
        . NEW VAL,DESCR
        . SET VAL=$PIECE(LINE,",",1)
        . SET DESCR=$PIECE(LINE,",",2)
        . KILL TMGFDA,TMGMSG,TMGIEN
        . SET TMGFDA(22716,"+1,",.01)=VAL
        . SET TMGFDA(22716,"+1,",.02)=DESCR
        . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
        . DO SHOWDIER^TMGDEBU2(.TMGMSG)
        WRITE "GOODBYE.",!
        QUIT


HL7
        KILL
        NEW zv,zzjnum,zzi
        SET zzjnum=$ORDER(^TMG("TMP","TMGHL71",""),-1)
        IF zzjnum'>0 WRITE "No job found.  Bye.",! GOTO HL7DN
        MERGE zv=^TMG("TMP","TMGHL71",zzjnum,"VARS")
        for zzi=1:1 s zo=$GET(zv(zzi)) q:(zo="")  s @zo WRITE "Setting --> ",zo,!
        KILL zzjnum,zv,zzi
        GOTO MenuDone^TMGIDE
HL7DN   KILL zzjnum,zv,zzi
        QUIT

HL72
        ;"KILL
        NEW zv,zzi,zd
        NEW zzjnum SET zzjnum=""
        FOR  SET zzjnum=$ORDER(^TMG("TMP","TMGHL71",zzjnum)) QUIT:(zzjnum="")  do
        . IF $DATA(^TMG("TMP","TMGHL71",zzjnum,"VARS"))'=0 do
        . . KILL zv
        . . MERGE zv=^TMG("TMP","TMGHL71",zzjnum,"VARS")
        . . for zzi=1:1 s zo=$GET(zv(zzi)) q:(zo="")  s @zo WRITE "Setting --> ",zo,!
        . . SET zd(1)=1
        . IF $DATA(^TMG("TMP","TMGHL71",zzjnum,"VARS-2"))'=0 do
        . . KILL zv
        . . MERGE zv=^TMG("TMP","TMGHL71",zzjnum,"VARS-2")
        . . for zzi=1:1 s zo=$GET(zv(zzi)) q:(zo="")  s @zo WRITE "Setting --> ",zo,!
        . . MERGE ^LAH=^TMG("TMP","TMGHL71",zzjnum,"^LAH")
        . . SET zd(2)=1
        IF $DATA(zv)=0 WRITE "No data found.  Goodbye.",! QUIT
        KILL zzjnum,zv,zzi
        ;"GOTO MenuDone^TMGIDE
HL72DN  KILL zzjnum,zv,zzi
        QUIT

parentref(array,ptr) ;
        NEW ref SET ref=ptr_")"
        NEW parent
        FOR  SET parent=$ORDER(array("owned by",ptr,0)) QUIT:parent=""  DO  QUIT:parent=""
        . SET ref=parent_","_ref
        . IF parent'=ptr SET ptr=parent
        . ELSE  SET parent=""
        SET ref="array("_ref
        QUIT ref
        ;
 
HASFACTOR(SOURCEREF,FACTOR) ;"RETURN IF PATIENT HAS FACTOR
        ;"Purpose: Ensure patients have health factor etc stored for given dates
        ;"Input: REF -- format: @REF@(DFN,FMDT)=""        
        ;"       FACTOR -- format:  'HF.NOTAVETERAN
        ;"Result : 1 -- HAS FACTOR (ANY TIME) OR 0 IF NOT
        NEW TMGRESULT SET TMGRESULT=0
        NEW VFILE,XREF SET (VFILE,P2FILE,REF)=""
        NEW P2FILE SET P2FILE=0
        NEW PREFIX SET PREFIX=$PIECE(FACTOR,".",1)
        NEW FNAME SET FNAME=$PIECE(FACTOR,".",2)
        NEW DIC,X,Y,P2IEN SET DIC(0)="M"
        NEW VISITMSG,NOTSUPPORTED SET NOTSUPPORTED=0
        SET VISITMSG(2)="VST^VC^E"
        SET VISITMSG(3)="VST^OL^0^"_+$GET(DUZ(2))
        SET VISITMSG(4)="COM^1^@"
        IF PREFIX="IM" DO
        . SET VFILE=9000010.11,P2FILE=9999999.14,XREF="IMM"  ;"Immunization
        IF PREFIX="HF" DO
        . SET VFILE=9000010.23,P2FILE=9999999.64,XREF="HF"   ;"Health Factor        
        IF PREFIX="ICD9" DO
        . SET NOTSUPPORTED=0,TMGRESULT="-1^"_PREFIX_" not currently supported"  ;"finish support and remove later. 
        . SET VFILE=9000010.07,P2FILE=80,XREF="POV"        ;"Diagnosis
        IF PREFIX="ST" DO
        . SET NOTSUPPORTED=0,TMGRESULT="-1^"_PREFIX_" not currently supported"  ;"finish support and remove later. 
        . SET VFILE=9000010.12,P2FILE=9999999.28,XREF="SK"   ;"Skin Test
        IF PREFIX="EX" DO
        . SET NOTSUPPORTED=0,TMGRESULT="-1^"_PREFIX_" not currently supported"  ;"finish support and remove later. 
        . SET VFILE=9000010.13,P2FILE=9999999.15,XREF="XAM"  ;"Exam
        IF PREFIX="ED" DO
        . SET NOTSUPPORTED=0,TMGRESULT="-1^"_PREFIX_" not currently supported"  ;"finish support and remove later. 
        . SET VFILE=9000010.16,P2FILE=9999999.09,XREF="ED"   ;"Education Topic
        IF PREFIX="CPT" DO
        . SET NOTSUPPORTED=0,TMGRESULT="-1^"_PREFIX_" not currently supported"  ;"finish support and remove later. 
        . SET VFILE=9000010.18,P2FILE=81,XREF="CPT"         ;"Procedure
        IF (+P2FILE'>0)!(NOTSUPPORTED)!(XREF="") GOTO NSHDN
        SET DIC=P2FILE,X=FNAME DO ^DIC SET P2IEN=+Y
        IF +Y>0 SET FNAME=$P(Y,"^",2) 
        ELSE  DO  GOTO NSHDN
        . SET TMGRESULT="-1^Unable to find factor '"_FNAME_" in file "_P2FILE 
        SET XREF="^AUPNV"_XREF_"(""AA"",DFN,"_+P2IEN_")"  ;"all the V * have the same AA reference.
        SET TMGRESULT="1^Success"
        NEW DFN SET DFN=0
        FOR  SET DFN=$ORDER(@SOURCEREF@(DFN)) QUIT:(+DFN'>0)  DO
        . SET VISITMSG(5)="VST^PT^"_DFN
        . NEW FMDT SET FMDT=0
        . FOR  SET FMDT=$ORDER(@SOURCEREF@(DFN,FMDT)) QUIT:(+FMDT'>0)  DO
        . . SET VISITMSG(1)="HDR^0^^0;"_FMDT_";E"
        . . IF PREFIX="IM" SET VISITMSG(6)="IMM+^"_P2IEN_"^^"_FNAME_"^@^^@^0^^1"
        . . IF PREFIX="HF" SET VISITMSG(6)="HF+^"_P2IEN_"^^^^"_DUZ_"^^^^^"_FMDT_"^"
        . . IF PREFIX="ST" SET VISITMSG(6)="SK+^"    ;"//FINISH
        . . IF PREFIX="EX" SET VISITMSG(6)="XAM^"    ;"//FINISH
        . . IF PREFIX="ED" SET VISITMSG(6)="PED+^"   ;"//FINISH
        . . IF PREFIX="CPT" SET VISITMSG(6)="CPT+^"  ;"//FINISH          
        . . IF PREFIX="ICD9" SET VISITMSG(6)="POV+^" ;"//FINISH
        . . SET VISITMSG(7)="VST^DT^"_FMDT
        . . NEW FOUND SET FOUND=0
        . . NEW IDT SET IDT=""
        . . FOR  SET IDT=$ORDER(@XREF@(IDT)) QUIT:(+IDT'>0)!FOUND  DO
        . . . IF 9999999-IDT=FMDT SET FOUND=1 QUIT
        . . IF FOUND QUIT
        . . NEW OUT
        . . DO SAVE^ORWPCE(.OUT,.VISITMSG,0,+$GET(DUZ(2)))
NSHDN   QUIT TMGRESULT
        
 ;---------------------
SHOWLIPIDS(FLD63D04)
        NEW FLDNAME SET FLDNAME=$PIECE(^DD(63.04,FLD63D04,0),"^",1)
        NEW LRDFN SET LRDFN=0
        FOR  SET LRDFN=$ORDER(^LR(LRDFN)) QUIT:(+LRDFN'>0)  DO
        . NEW IEN2 SET IEN2=+$ORDER(^DPT("ATMGLR",LRDFN,0))
        . IF IEN2'>0 QUIT
        . NEW PTNAME SET PTNAME=$PIECE($GET(^DPT(IEN2,0)),"^",1)
        . NEW RT SET RT=0
        . FOR  SET RT=$ORDER(^LR(LRDFN,"CH",RT)) QUIT:(+RT'>0)  DO
        . . IF $DATA(^LR(LRDFN,"CH",RT,FLD63D04))=0 QUIT
        . . NEW VALUE SET VALUE=$PIECE(^LR(LRDFN,"CH",RT,FLD63D04),"^",1)
        . . WRITE "LRDFN=",LRDFN," ",PTNAME," -- ",FLDNAME," -- ",VALUE,!
        QUIT

ALLPT4LAB ;
        NEW DIC,X,Y,IEN60,DFN,ARRAY
        SET DIC=60,DIC(0)="MAEQ"
        DO ^DIC WRITE ! SET IEN60=+Y
        IF IEN60'>0 QUIT
        SET DFN=0
        FOR  SET DFN=$ORDER(^DPT(DFN)) QUIT:(DFN'>0)  DO
        . NEW TEMP
        . DO GETPLABS^TMGLRR01(DFN_"^2",IEN60,.TEMP)
        . IF $DATA(TEMP)=0 QUIT
        . MERGE ARRAY("NAMES")=TEMP(0) KILL TEMP(0)
        . MERGE ARRAY(DFN)=TEMP
        IF $DATA(ARRAY) DO ZWRITE^TMGZWR("ARRAY")
        QUIT
FIXSYN ;
        NEW SUBIEN SET SUBIEN=0
        FOR  SET SUBIEN=$ORDER(^LAB(60,1,5,SUBIEN)) Q:SUBIEN'>0  DO
        . NEW DIK,DA SET DIK="^LAB(60,1,5,",DA=SUBIEN,DA(1)=1 
        . NEW SYN SET SYN=$GET(^LAB(60,1,5,SUBIEN,0)) Q:SYN=""
        . WRITE SYN,!
        . WRITE "ACTION (K,I,M,R)? "
        . NEW ACTION READ ACTION,!
        . SET ACTION=$$UP^XLFSTR(ACTION)
        . IF ACTION="I" QUIT
        . IF ACTION="R" DO  QUIT
        . . SET SUBIEN=$ORDER(^LAB(60,1,5,SUBIEN),-1)
        . . SET SUBIEN=$ORDER(^LAB(60,1,5,SUBIEN),-1)
        . IF ACTION="K" DO  QUIT
        . . DO ^DIK
        . IF ACTION'="M" QUIT
        . ;"WRITE "PICK LAB TO MOVE SYNONYM TO",!
        . ;"DO PRESS2GO^TMGUSRI2
        . NEW IEN60 
FXSN1   . SET IEN60=$$SELLAB60^TMGHL70C(SYN)
        . IF IEN60'>0 DO  QUIT
        . . WRITE "No lab chosen, skipping synonym.  Re-run to try again.",!
        . . DO PRESS2GO^TMGUSRI2
        . IF +IEN60=1 DO  GOTO FXSN1
        . . WRITE "You have picked the same record that already holds synonym.  Try again",!
        . . DO PRESS2GO^TMGUSRI2
        . SET STORE=$PIECE($GET(^LAB(60,+IEN60,0)),"^",5)
        . IF STORE="" DO  GOTO FXSN1
        . . WRITE "That lab doesn't have active storage.  Try again.",!
        . . DO PRESS2GO^TMGUSRI2
        . NEW TEMP SET TEMP=$$ADD60SYN^TMGHL70B(+IEN60,SYN)
        . IF TEMP'>0 DO
        . . WRITE TEMP,! DO PRESS2GO^TMGUSRI2
        . DO ^DIK
        WRITE "ALL DONE, GOODBYE.",!
        DO PRESS2GO^TMGUSRI2
        
        
TESTSCRP(LIST,FIEVAL,RESULT) ;
        SET RESULT=1
        QUIT
        
z11     NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(^XUSEC(8994,IDX)) QUIT:+IDX'>0  DO
        . NEW USR SET USR=+$P($GET(^XUSEC(8994,IDX,0)),"^",2) QUIT:USR'>0
        . NEW DFN SET DFN=+$P($GET(^XUSEC(8994,IDX,100)),"^",1) QUIT:DFN'>0
        . SET ^XUSEC(8994,"USR",USR,IDX\1,DFN)=""
        QUIT
        ;
 ;--------------------------------------
TCHSRCH ;
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(^TMG(22702,IDX)) QUIT:(IDX'>0)  DO
        . NEW ANAME SET ANAME=$PIECE($GET(^TMG(22702,IDX,0)),"^",1) QUIT:ANAME=""
        . NEW TMGFDA,TMGMSG
        . WRITE IDX,". ",ANAME,!
        . SET TMGFDA(22702,IDX_",",1000)=ANAME
        . DO FILE^DIE("","TMGFDA","TMGMSG")
        . IF $DATA(TMGMSG)=0 QUIT
        . DO SHOWDIER^TMGDEBU2(.TMGMSG)
        QUIT
        ;"
FIXLAB  ;
        NEW LRDFN,FIXARRAY
        NEW % DO NOW^%DTC
        SET ^TMG("TMG LAB FIX XREF",%)="STARTED"
        SET LRDFN=0
        FOR  SET LRDFN=$ORDER(^LR(LRDFN)) QUIT:LRDFN'>0  DO
        . NEW DFN
        . SET DFN=$ORDER(^DPT("ATMGLR",LRDFN,0))
        . IF DFN'>0 DO  QUIT
        . . WRITE "LRDFN: ",LRDFN," DOES NOT HAVE AN DFN.",!
        . NEW NAME SET NAME=$PIECE($GET(^DPT(DFN,0)),"^",1)
        . WRITE "STORING FOR PATIENT ",NAME,"(",DFN,"-",LRDFN,")-----",!
        . NEW IDT SET IDT=0
        . FOR  SET IDT=$ORDER(^LR(LRDFN,"CH",IDT)) QUIT:IDT'>0  DO
        . . NEW DATE
        . . SET DATE=$PIECE($GET(^LR(LRDFN,"CH",IDT,0)),"^",1)
        . . NEW TMGFLD SET TMGFLD=0
        . . ;"QUIT  ;"TEMP...
        . . FOR  SET TMGFLD=$ORDER(^LR(LRDFN,"CH",IDT,TMGFLD)) QUIT:TMGFLD'>0  DO
        . . . NEW IEN60
        . . . SET IEN60=$PIECE($GET(^LR(LRDFN,"CH",IDT,TMGFLD)),"^",3)
        . . . SET IEN60=$PIECE(IEN60,"!",7)
        . . . IF IEN60'>0 QUIT  ;"WRITE "NO IEN60!",! QUIT
        . . . NEW NODE
        . . . SET NODE=LRDFN_";CH;"_IDT_";"_TMGFLD
        . . . WRITE "      ->",DATE,"- ",IEN60,!
        . . . ;"DO SLAB^LRPX(DFN,DATE,IEN60,NODE)
        . . . IF $$SLABNEEDED(DFN,DATE,IEN60,NODE) DO
        . . . . WRITE "FIX NEEDED HERE!",!
        . . . . SET FIXARRAY(DFN,DATE,IEN60,NODE)=1
        . . . . DO SLAB^LRPX(DFN,DATE,IEN60,NODE)
        DO NOW^%DTC
        SET ^TMG("TMG LAB FIX XREF",%)="FINISHED"        
        QUIT
        ;
SLABNEEDED(DFN,DATE,ITEM,NODE) ; from SLAB^LRPX
        ; SET index for lab data.
        NEW NEEDED SET NEEDED=1
        IF $DATA(^PXRMINDX(63,"PI",DFN,ITEM,DATE,NODE))=0 GOTO SLNDDN
        IF $DATA(^PXRMINDX(63,"IP",ITEM,DFN,DATE,NODE))=0 GOTO SLNDDN
        I ITEM=+ITEM SET NEEDED=0 GOTO SLNDDN
        IF $DATA(^PXRMINDX(63,"PDI",DFN,DATE,ITEM,NODE))=0 GOTO SLNDDN
        SET NEEDED=0
SLNDDN  Q NEEDED
 ;        
        
        

SCANDDS(OUT)  ;
  NEW FILE SET FILE=0
  FOR  SET FILE=$ORDER(^DD(FILE)) QUIT:(+FILE'>0)  DO
  . DO SCANDD1(FILE,.OUT)
  . ;WRITE "."
  WRITE !
  QUIT
  

SCANDD1(FILE,OUT) ;
  NEW FLD SET FLD=0
  FOR  SET FLD=$ORDER(^DD(FILE,FLD))  QUIT:(FLD'>0)  DO
  . NEW ZN SET ZN=$GET(^DD(FILE,FLD,0)) QUIT:ZN=""
  . SET DEF=$PIECE(ZN,"^",2) 
  . QUIT:DEF'["D"
  . NEW XFRM SET XFRM=$PIECE(ZN,"^",5,99)
  . QUIT:XFRM'["%DT"
  . NEW D SET D=$PIECE(XFRM,"%DT=",2) SET D=$PIECE(D," ",1)
  . SET D=$PIECE(D,"""",2) SET D=$PIECE(D,"""",1)
  . IF D="" DO
  . . WRITE !,"PROBLEM: FILE=",FILE," FLD=",FLD," CODE=",XFRM,!
  . SET OUT(FILE,FLD)=D
  QUIT

SCANDD2(FILE,OUT) ;
  NEW FLD SET FLD=0
  FOR  SET FLD=$ORDER(^DD(FILE,FLD))  QUIT:(FLD'>0)  DO
  . NEW ZN SET ZN=$GET(^DD(FILE,FLD,0)) QUIT:ZN=""
  . SET DEF=$PIECE(ZN,"^",2) 
  . QUIT:DEF'["D"
  . NEW XFRM SET XFRM=$PIECE(ZN,"^",5,99)
  . QUIT:XFRM'["%DT"
  . QUIT:((XFRM'["N ")&(XFRM'["NEW "))
  . WRITE !,"FILE=",FILE," FLD=",FLD," CODE=",XFRM,!
  . SET OUT(FILE,FLD)=XFRM
  QUIT
  
SCANDD3(FILE,OUT) ;
  NEW FLD SET FLD=0
  FOR  SET FLD=$ORDER(^DD(FILE,FLD))  QUIT:(FLD'>0)  DO
  . NEW ZN SET ZN=$GET(^DD(FILE,FLD,0)) QUIT:ZN=""
  . SET DEF=$PIECE(ZN,"^",2) 
  . QUIT:DEF'["J"
  . NEW TEMP SET TEMP=$PIECE(DEF,"J",2)
  . NEW NUM1 SET NUM1=+TEMP
  . NEW L SET L=$LENGTH(NUM1)
  . NEW NUM2 SET NUM2=$EXTRACT(TEMP,$LENGTH(NUM1)+1,999)
  . IF $EXTRACT(NUM2,1)'="," QUIT
  . SET NUM2=$EXTRACT(NUM2,2,999)
  . SET NUM2=+NUM2
  . SET OUT(FILE,FLD)="J^"_NUM1_"^"_NUM2
  QUIT  

TIUSCAN
  NEW TIUIEN SET TIUIEN=0
  NEW ABORT SET ABORT=0
  FOR  SET TIUIEN=$ORDER(^TIU(8925,TIUIEN)) QUIT:(+TIUIEN'>0)!(ABORT)  DO
  . NEW TMG SET TMG=$GET(^TIU(8925,TIUIEN,"TMG")) QUIT:TMG=""
  . NEW TEXT SET TEXT=$PIECE(TMG,"^",4)
  . IF TEXT["as previously scheduled" QUIT
  . NEW DT SET DT=$PIECE(TMG,"^",3) QUIT:DT=""
  . IF DT'=1 QUIT
  . IF $LENGTH(TEXT,"/")<2 QUIT
  . NEW % SET %=2
  . WRITE "REPROCESS: ",TEXT DO YN^DICN WRITE !
  . SET ABORT=(%=-1)
  . IF %'=1 QUIT
  . WRITE "IEN=",TIUIEN," DT=",DT," TEXT=",$PIECE(TMG,"^",4),!
  . NEW ZN SET ZN=$GET(^TIU(8925,TIUIEN,0))
  . NEW DFN SET DFN=+$PIECE(ZN,"^",2)  
  . NEW OUT,TEMP SET TEMP=$$SCANNOTE^TMGTIU10(DFN,TIUIEN,.OUT,1)
  . WRITE "RESULT=",TEMP,!
  . IF $DATA(OUT) DO ZWRITE^TMGZWR("OUT")
  QUIT
  ;
TIUSCAN2
  NEW TIUIEN SET TIUIEN=0
  FOR  SET TIUIEN=$ORDER(^TIU(8925,TIUIEN)) QUIT:(+TIUIEN'>0)  DO
  . NEW TMG SET TMG=$GET(^TIU(8925,TIUIEN,"TMG")) QUIT:TMG=""
  . NEW TEXT SET TEXT=$PIECE(TMG,"^",4)
  . IF TEXT'="?" QUIT
  . SET $PIECE(^TIU(8925,TIUIEN,"TMG"),"^",4)=""
  . WRITE "."
  WRITE !
QUIT  

FIXNAMECOMP ;
  NEW DA,DIK SET DIK="^VA(20,"
  NEW IEN SET IEN=0
  FOR  SET IEN=$ORDER(^VA(20,IEN)) QUIT:+IEN'>0  DO
  . NEW ZN SET ZN=$GET(^VA(20,IEN,0)) QUIT:ZN=""
  . NEW FROMFILE SET FROMFILE=+ZN
  . IF FROMFILE'=2 QUIT
  . NEW FROMFLD SET FROMFLD=$PIECE(ZN,"^",2)
  . IF FROMFLD'=.01 QUIT
  . NEW PTRIN SET PTRIN=$PIECE(ZN,"^",3)
  . IF $LENGTH(PTRIN,",")<3 QUIT
  . NEW DFN SET DFN=+PTRIN
  . NEW PATNAME SET PATNAME=$PIECE($GET(^DPT(DFN,0)),"^",1)
  . NEW PT2NC SET PT2NC=$PIECE($GET(^DPT(DFN,"NAME")),"^",1)
  . IF PT2NC=IEN QUIT
  . WRITE "KILL #",IEN,"  ",$GET(^VA(20,IEN,1))," <-- ",PTRIN,"  ",$GET(^DPT(+PTRIN,"NAME"))," ",PATNAME," -> ",PT2NC,!
  . SET DA=IEN DO ^DIK  
  QUIT
  
DOCSTRUCT
  N X,Y,DIC SET DIC=8925,DIC(0)="MAEQ"
  DO ^DIC W !
  QUIT:+Y'>0
  DO SHOWDSTR(+Y)
  QUIT
  ;  
SHOWDSTR(IEN,INDENT)
  SET INDENT=$GET(INDENT)
  W INDENT,IEN," -- ",$$GET1^DIQ(8925,IEN,.01),!
  W INDENT,"CHILDREN:",!
  N FOUND SET FOUND=0
  NEW SUBIEN SET SUBIEN=0
  FOR  SET SUBIEN=$ORDER(^TIU(8925,"DAD",IEN,SUBIEN)) QUIT:+SUBIEN'>0  DO
  . SET FOUND=1
  . DO SHOWDSTR(SUBIEN,INDENT_"  ")
  IF FOUND=0 W INDENT,"(NONE)",!
  QUIT
  ;"
FIXTOPIC   ;"Fix problems that were added on 8/1/12
  ;"Pull linked problems from 22719.5
  NEW PROBARRAY
  NEW IEN,SUBIEN,ZN,DATE
  SET IEN=0
  FOR  SET IEN=$ORDER(^TMG(22719.5,IEN)) QUIT:IEN'>0  DO
  . SET SUBIEN=0
  . FOR  SET SUBIEN=$ORDER(^TMG(22719.5,IEN,1,SUBIEN)) QUIT:SUBIEN'>0  DO
  . . SET ZN=$GET(^TMG(22719.5,IEN,1,SUBIEN,0))
  . . SET PROBARRAY($PIECE(ZN,"^",2))=""
  SET IEN=0
  FOR  SET IEN=$ORDER(^AUPNPROB(IEN)) QUIT:IEN'>0  DO
  . SET DATE=$PIECE($GET(^AUPNPROB(IEN,0)),"^",3)
  . ;"WRITE "CHECKING IEN ",IEN,!
  . IF $DATA(PROBARRAY(IEN)) QUIT
  . IF DATE=3120801 DO
  . . SET DFN=$PIECE($GET(^AUPNPROB(IEN,0)),"^",2)
  . . NEW TMGFDA SET TMGFDA(9000011,IEN_",",.02)="-"_DFN
  . . NEW TMGMSG
  . . ;"WRITE "****DELETING IT",!
  . . DO FILE^DIE("I","TMGFDA","TMGMSG")
  . . DO SHOWDIER^TMGDEBU2(.TMGMSG)
  QUIT
  ;"
RMALLERG   ;"REMOVE ALL ALLERGY TOPICS FROM 22719, IF FIRST SECTION
  NEW IDX SET IDX=0
  NEW NODEL,DEL
  SET (NODEL,DEL)=0
  NEW TMGFDA,TMGMSG
  FOR  SET IDX=$ORDER(^TMG(22719,"HPIALL","ALLERGIES",IDX)) QUIT:IDX'>0  DO
  . ;"WRITE "FOUND IDX: ",IDX,!
  . IF $DATA(^TMG(22719,"HPIALL","ALLERGIES",IDX,1)) DO
  . . NEW TMGFDA SET TMGFDA(22719.02,"1,"_IDX_",",.01)="@"
  . . NEW TMGMSG
  . . DO FILE^DIE("E","TMGFDA","TMGMSG")
  . . DO SHOWDIER^TMGDEBU2(.TMGMSG)
  . . WRITE "+"
  . . SET DEL=DEL+1
  . ELSE  DO
  . . WRITE "."
  . . SET NODEL=NODEL+1
  WRITE "==========TOTALS============",!
  WRITE " * DELETING: ",DEL,!
  WRITE " * NOT     : ",NODEL,!
  WRITE "============================",!
  QUIT
  ;"
  ;" --- 6/25/15
TESTTEXT
  WRITE !!
  NEW TEST SET TEST="AB "_$CHAR(9)_"CDE"
  DO ASCIILINE(TEST)
  WRITE "--------------",!
  NEW TMGI
  FOR TMGI=1:1:10 DO
  . NEW LINE SET LINE=$TEXT(+TMGI^DI)
  . WRITE LINE,!
  . DO ASCIILINE(LINE)
  QUIT
ASCIILINE(S) ;DUMP OUT FIRST FEW CHARACTERS OF INPUT AS ACII-CODE AND CHARACTER
  NEW TMGJ 
  FOR TMGJ=1:1:10 DO
  . NEW CH SET CH=$EXTRACT(S,TMGJ)
  . WRITE "[",$ASCII(CH),"]",CH
  WRITE !
  QUIT
;
FIXPATCH ;
  ;"Purpose:
  DO FIXPATCH^TMGPAT5
  QUIT;

TESTMCV ;  
   NEW FNAME,OUT,STATS,HILO
   ;SET FNAME=$$GETFNAME^TMGIOUTL("PICK FILE","/tmp/")
   SET FNAME="/tmp/hfs2.dat"
   IF FNAME="" QUIT
   NEW ARR
   NEW %DT SET %DT="PT"
   IF $$HFS2ARFP^TMGIOUT3(FNAME,"ARR")
   ;DO OPEN^%ZISH("OFILE","/tmp/","hfs3.csv","W")
   ;IF POP W "ERROR",! QUIT
   ;USE IO
   NEW LRDFN SET LRDFN=""
   NEW IDX SET IDX=""
   FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:+IDX'>0  DO
   . NEW LINE SET LINE=$GET(ARR(IDX)) QUIT:LINE=""
   . NEW PARTA,PARTB SET PARTA=$$TRIM^XLFSTR($EXTRACT(LINE,1,14))
   . IF PARTA'="" SET LRDFN=PARTA
   . IF LRDFN="" QUIT
   . SET PARTB=$$TRIM^XLFSTR($EXTRACT(LINE,15,999))
   . NEW DATE SET DATE=$PIECE(PARTB," ",1,2)
   . SET DATE=$PIECE(DATE,":",1,2)
   . NEW X,Y,FMDT SET X=DATE DO ^%DT SET FMDT=Y
   . IF FMDT'>0 QUIT
   . NEW RDT SET RDT=9999999-FMDT
   . NEW MCV SET MCV=$$TRIM^XLFSTR($PIECE(PARTB," ",3,999))
   . IF MCV="" QUIT
   . NEW DATA SET DATA=$GET(^LR(+LRDFN,"CH",RDT,388))
   . WRITE DATA,!
   . NEW LOC SET LOC=$P(DATA,"^",11)
   . IF LOC="" SET LOC="?"
   . ;WRITE "LOC=",LOC,!
   . NEW RANGE SET RANGE=$P(DATA,"^",5)
   . NEW LN SET LN=$PIECE(RANGE,"!",2)
   . NEW HN SET HN=$PIECE(RANGE,"!",3)
   . WRITE LRDFN,",",FMDT\1,",",MCV,",",LN,",",HN,",",LOC,!
   . SET OUT(LOC,MCV,LRDFN_"-"_FMDT)=""
   . SET STATS(LOC,MCV\1)=+$GET(STATS(LOC,MCV\1))+1
   . NEW YR SET YR=20_$EXTRACT(FMDT,2,3)
   . SET HILO(LOC,YR_":"_LN_"^"_HN)=""
   NEW LOC SET LOC=""
   FOR  SET LOC=$ORDER(STATS(LOC)) QUIT:LOC=""  DO
   . NEW MIN SET MIN=+$ORDER(STATS(LOC,""))
   . NEW MAX SET MAX=+$ORDER(STATS(LOC,""),-1)
   . NEW AMCV FOR AMCV=MIN:1:MAX DO
   . . ;WRITE LOC,",",AMCV,",",+$GET(STATS(LOC,AMCV)),!
   ;DO CLOSE^%ZISH("OFILE")
   ;ZWR STATS
   ;ZWR OUT
   ZWR HILO
   WRITE "goodbye.",!
   QUIT
  
  
FIXDD ;  
   NEW FNAME,OUT,STATS,HILO
   ;SET FNAME=$$GETFNAME^TMGIOUTL("PICK FILE","/tmp/")
   SET FNAME="/tmp/TMG-restore-DD.txt"
   IF FNAME="" QUIT
   NEW ARR
   IF $$HFS2ARFP^TMGIOUT3(FNAME,"ARR")
   ;ZWR ARR
   NEW IDX SET IDX=""
   FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:+IDX'>0  DO
   . NEW REF SET REF=$GET(ARR(IDX)) SET IDX=IDX+1
   . NEW REFA SET REFA=$TR($PIECE(REF,",",1),"""","")
   . ;"IF REFA'["^DD" QUIT
   . NEW REFB SET REFB=$PIECE(REF,",",3,999)
   . IF ($EXTRACT($PIECE(REF,",",2),1,3)="227")&(REFB["DT") QUIT
   . SET REF=REFA_"("_REFB
   . ;"if REF["22700001" QUIT
   . NEW VALUE SET VALUE=$GET(ARR(IDX))
   . NEW MYVAL SET MYVAL=$GET(@REF)
   . IF MYVAL=VALUE QUIT
   . IF MYVAL'="" WRITE !,"-",REF,"=",MYVAL,!
   . WRITE "+",REF,"=",VALUE,!
   . ;"SET ^TMG("TMP","^DD",REF)=VALUE
   . ;"SET @REF=VALUE
   
   QUIT
   

TEST1  ;
   NEW T,H,E,Y,A,R,V,S,M
   NEW VAR
   FOR T=0:1:9 DO
   . SET VAR("T")=T
   . SET VAR("USED",T)=1
   . FOR H=0:1:9 DO
   . . SET VAR("H")=H
   . . IF $GET(VAR("USED",H))=1 QUIT
   . . SET VAR("USED",H)=1
   . . FOR E=0:1:9 DO
   . . . SET VAR("E")=E
   . . . IF $GET(VAR("USED",E))=1 QUIT
   . . . SET VAR("USED",E)=1
   . . . FOR Y=0:1:9 DO
   . . . . SET VAR("Y")=Y
   . . . . IF $GET(VAR("USED",Y))=1 QUIT
   . . . . SET VAR("USED",Y)=1
   . . . . FOR A=0:1:9 DO
   . . . . . SET VAR("A")=A
   . . . . . IF $GET(VAR("USED",A))=1 QUIT
   . . . . . SET VAR("USED",A)=1
   . . . . . FOR R=0:1:9 DO
   . . . . . . SET VAR("R")=R
   . . . . . . IF $GET(VAR("USED",R))=1 QUIT
   . . . . . . SET VAR("USED",R)=1
   . . . . . . FOR V=0:1:9 DO
   . . . . . . . SET VAR("V")=V
   . . . . . . . IF $GET(VAR("USED",V))=1 QUIT
   . . . . . . . SET VAR("USED",V)=1
   . . . . . . . FOR S=0:1:9 DO
   . . . . . . . . SET VAR("S")=S
   . . . . . . . . IF $GET(VAR("USED",S))=1 QUIT
   . . . . . . . . SET VAR("USED",S)=1
   . . . . . . . . FOR M=0:1:9 DO
   . . . . . . . . . SET VAR("M")=M
   . . . . . . . . . IF $GET(VAR("USED",M))=1 QUIT
   . . . . . . . . . SET VAR("USED",M)=1
   . . . . . . . . . IF $$TESTCOMBO(.VAR)=1 DO
   . . . . . . . . . . NEW V2 MERGE V2=VAR KILL V2("USED")
   . . . . . . . . . . ;"ZWR V2(*)
   . . . . . . . . . . NEW IDX,STR SET (IDX,STR)="" 
   . . . . . . . . . . FOR  SET IDX=$ORDER(V2(IDX)) QUIT:IDX=""  DO
   . . . . . . . . . . . IF STR'="" SET STR=STR_", "
   . . . . . . . . . . . SET STR=STR_IDX_"="_$GET(V2(IDX))
   . . . . . . . . . . WRITE STR,!
   . . . . . . . . . . ;"WRITE "---------------",!
   . . . . . . . . . SET VAR("USED",M)=0
   . . . . . . . . SET VAR("USED",S)=0
   . . . . . . . SET VAR("USED",V)=0
   . . . . . . SET VAR("USED",R)=0
   . . . . . SET VAR("USED",A)=0
   . . . . SET VAR("USED",Y)=0
   . . . SET VAR("USED",E)=0
   . . SET VAR("USED",H)=0
   . SET VAR("USED",T)=0
   QUIT
   ;
TESTCOMBO(V)  ;
    ;"  T H E Y
    ;"    A R E
    ;"  V E R Y
    ;"---------
    ;"S M A R T
    ;
    NEW RESULT SET RESULT=0
    NEW COL,COLIDX,TEMP,CARRY
CL1 SET TEMP=V("Y")+V("E")+V("Y")
    SET CARRY=TEMP\10 SET TEMP=TEMP#10
    IF TEMP'=V("T") GOTO TESTDN
CL2 SET TEMP=CARRY+V("E")+V("R")+V("R")   
    SET CARRY=TEMP\10 SET TEMP=TEMP#10
    IF TEMP'=V("R") GOTO TESTDN
CL3 SET TEMP=CARRY+V("H")+V("A")+V("E")    
    SET CARRY=TEMP\10 SET TEMP=TEMP#10
    IF TEMP'=V("A") GOTO TESTDN
CL4 SET TEMP=CARRY+V("T")+V("V")    
    SET CARRY=TEMP\10 SET TEMP=TEMP#10
    IF TEMP'=V("M") GOTO TESTDN
CL5 SET TEMP=CARRY  
    IF TEMP'=V("S") GOTO TESTDN
    SET RESULT=1
TESTDN ;
   QUIT RESULT
   
   
Show(solution,expression) ;
 new arr,ch,digit
 for j=1:1:$length(expression) do
 . set ch=$extract(expression,j),digit=$extract(solution,j)
 . if ch'?1a quit
 . set arr(ch)=digit
 set ch="" for  set ch=$order(arr(ch)) quit:ch=""  write ch,"=",$get(arr(ch)),", "
 write !
 quit
 ;
PuzzleCall(digits) ;
 New try Set try=$Translate(expression,callletter,digits)
 If @try do
 . Set solution(try)=$Get(solution(try))+1
 . do Show(try,expression)
 ;"else  write "FAIL: ",try,!
 Quit
 ;
Permut(in,lead) ;
 New ii,letter,next
 SET lead=$get(lead)
 If in="" Do  Quit
 . Quit:lead=""
 . do PuzzleCall(lead) quit
 . Set Permut(lead)=$Get(Permut(lead))+1
 . Quit
 For ii=1:1:$Length(in) Do
 . Set letter=$Extract(in,ii)
 . set next=in
 . set $Extract(next,ii)=""
 . Do Permut(next,lead_letter)
 . Quit
 Quit
 ; 
Puzzle(expression)  ;" E.g.: Do Puzzle("SEND+MORE=MONEY") 
 New callletter,ii,letter,solution,ch set callletter=""
 For ii=1:1:$Length(expression) do
 . set ch=$extract(expression,ii) 
 . if ch?1a,callletter'[ch set callletter=callletter_ch
 Do Permut(1234567890,"")
 Quit
 ;
ITR
  NEW LEN SET LEN=3
  NEW CH,CARRY,TRY SET CARRY=0,TRY=""
  NEW DONE SET DONE=0
  FOR  DO  QUIT:DONE=1
  . NEW TEMP SET TEMP=""
  . NEW IDX FOR IDX=LEN:-1:1 DO 
  . . SET CH=$EXTRACT(TRY,IDX) IF CH="" SET CH=-1
  . . SET CH=CH+CARRY,CARRY=0
  . . FOR  DO  QUIT:TEMP'[CH
  . . . SET CH=CH+1 IF CH=10 SET CH=0,CARRY=1
  . . . IF CARRY=1,IDX=1 SET DONE=1
  . . SET TEMP=CH_TEMP
  . SET TRY=TEMP
  . WRITE TRY,!
  QUIT
  ;"
FXTOPIC2  ;"FIX TIU TOPIC FILE
  NEW IEN SET IEN=351581
  FOR  SET IEN=$ORDER(^TIU(8925,IEN)) QUIT:IEN'>0  DO
  . WRITE "FIXING IEN:",IEN,!
  . DO TRIGGER1^TMGC0Q04(IEN)
  WRITE "DONE",!
  QUIT
  ;"
FIX22729()
  WRITE "BEGINNING",!
  NEW DFN SET DFN=0
  FOR  SET DFN=$ORDER(^TIU(8925,"ZTMGPTDT",DFN)) QUIT:DFN'>0  DO
  . WRITE "WORKING ON DFN ",DFN,!
  . NEW DATE SET DATE=3140000
  . FOR  SET DATE=$ORDER(^TIU(8925,"ZTMGPTDT",DFN,DATE)) QUIT:DATE'>0  DO
  . . NEW TIUIEN SET TIUIEN=$ORDER(^TIU(8925,"ZTMGPTDT",DFN,DATE,0))
  . . WRITE "   - TIU IEN ",TIUIEN,!
  . . DO TABLDATA^TMGTIUT5(TIUIEN)
  WRITE "DONE",!
  QUIT
  ;"
FIX22731()
  WRITE "BEGINNING",!
  NEW TIUIEN SET TIUIEN=422643
  FOR  SET TIUIEN=$ORDER(^TIU(8925,TIUIEN)) QUIT:+TIUIEN'>0  DO
  . WRITE " -> ",TIUIEN,!
  . NEW DFN,TMGNODE,TMGDATE,TMGFOLLOWUP
  . SET DFN=+$P($GET(^TIU(8925,TIUIEN,0)),"^",2)
  . IF DFN'>0 QUIT
  . SET TMGNODE=$GET(^TIU(8925,TIUIEN,"TMG"))
  . SET TMGDATE=+$PIECE(TMGNODE,"^",3)
  . SET TMGFOLLOWUP=$PIECE(TMGNODE,"^",4)
  . IF TMGDATE>0 DO
  . . WRITE "    HAS A DATE OF ",TMGDATE,!
  . . WRITE "       -> ",TMGFOLLOWUP,!
  . . NEW TMGFDA,TMGIEN,TMGIENS,TMGMSG
  . . ;"DOES PT HAVE ENTRY ALREADY?
  . . IF $DATA(^TMG(22731,DFN))=0 DO
  . . . SET TMGIEN(1)=DFN
  . . . SET TMGFDA(22731,"+1,",.01)="`"_DFN
  . . . DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
  . . . IF $DATA(TMGMSG("DIERR")) DO SHOWDIER^TMGDEBU2(.TMGMSG)
  . . ELSE  DO
  . . . SET TMGIEN(1)=DFN
  . . ;"UPLOAD DATA
  . . KILL TMGFDA,TMGMSG
  . . NEW TMGIENS SET TMGIENS="+1,"_TMGIEN(1)_","
  . . SET TMGIEN(1)=TIUIEN
  . . SET TMGFDA(22731.01,TMGIENS,.01)=TIUIEN
  . . SET TMGFDA(22731.01,TMGIENS,.02)=TMGDATE
  . . SET TMGFDA(22731.01,TMGIENS,.03)=TMGFOLLOWUP
  . . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  . . IF $DATA(TMGMSG("DIERR")) DO SHOWDIER^TMGDEBU2(.TMGMSG)
  WRITE "DONE",!
  QUIT
  ;"  
TESTACTIVE ;
  NEW DFN SET DFN=0
  NEW CT SET CT=0
  FOR  SET DFN=$ORDER(^DPT(DFN)) QUIT:+DFN'>0  DO
  . IF $$ACTIVEPT^TMGPXR03(DFN,3) WRITE "+"
  . ELSE  WRITE "-"  
  QUIT
FIXFUDT()   ;"
  NEW DFN SET DFN=0
  FOR  SET DFN=$ORDER(^TMG(22731,DFN)) QUIT:DFN'>0  DO
  . NEW NAME SET NAME=$P($G(^DPT(DFN,0)),"^",1)
  . NEW IEN SET IEN=0
  . FOR  SET IEN=$ORDER(^TMG(22731,DFN,"DOC",IEN)) QUIT:IEN'>0  DO
  . . NEW ZN SET ZN=$GET(^TMG(22731,DFN,"DOC",IEN,0))
  . . NEW DATE SET DATE=$P(ZN,"^",2)
  . . IF DATE>4000000 DO
  . . . WRITE NAME," has a doc (",IEN,") with a date of ",DATE,!
  . . . NEW CORRECTDT,RESPONSE SET CORRECTDT=DATE-1000000
  . . . WRITE "      Set this to ",CORRECTDT,"? "
  . . . READ RESPONSE
  . . . SET RESPONSE=$$UP^XLFSTR(RESPONSE)
  . . . WRITE !
  . . . IF RESPONSE="Y" DO
  . . . . SET $P(^TMG(22731,DFN,"DOC",IEN,0),"^",2)=CORRECTDT
  QUIT
  ;"
SEC2STR(SEC) ;
  NEW SPM SET SPM=60 ;"sec/min
  NEW MPH SET MPH=60 ;"min/hr
  NEW HPD SET HPD=24 ;"hrs/day
  NEW DPY SET DPY=365 ;"days/yr
  NEW SPY SET SPY=SPM*MPH*HPD*DPY  ;" 31536000 seconds per year
  NEW SPD SET SPD=SPM*MPH*HPD      ;" 86400 seconds per day
  NEW SPH SET SPH=SPM*MPH          ;" 3600 seconds per hr
  NEW YR SET YR=SEC\SPY
  NEW REMAINDER SET REMAINDER=SEC#SPY
  NEW DAY SET DAY=REMAINDER\SPD
  SET REMAINDER=REMAINDER#SPD
  NEW HR SET HR=REMAINDER\SPH
  SET REMAINDER=REMAINDER#SPH
  NEW MIN SET MIN=REMAINDER\SPM
  SET REMAINDER=REMAINDER#SPM
  NEW SEC2 SET SEC2=REMAINDER
  NEW STR SET STR=""
  IF YR>0 SET STR=STR_YR_" yrs"
  IF DAY>0 DO
  . IF STR'="" SET STR=STR_", "
  . SET STR=STR_DAY_" days"  
  IF (HR>0) DO
  . IF STR'="" SET STR=STR_", "
  . SET STR=STR_HR_" hrs"
  IF (MIN>0) DO
  . IF STR'="" SET STR=STR_", "
  . SET STR=STR_MIN_" mins"
  IF (SEC2>0) DO
  . IF STR'="" SET STR=STR_", "
  . SET STR=STR_SEC2_" sec"
  QUIT STR
  ;
PARSEC(SEC) ;"Parse seconds
  NEW SPY,SPD,SPH,YR,DAY,HR,MIN,SEC2,RM,STR
  SET SPH=3600,SPD=SPH*24,SPY=SPD*365
  SET YR=SEC\SPY,RM=SEC#SPY,DAY=RM\SPD,RM=RM#SPD,HR=RM\SPH
  SET RM=RM#SPH,MIN=RM\60,RM=RM#60,SEC2=RM
  SET STR=YR_"^"_DAY_"^"_HR_"^"_MIN_"^"_SEC2
  QUIT STR
  ;  
S2STR(SEC) ;"Seconds to string
  NEW STR,PS,IDX,TAG,NM
  SET STR="",PS=$$PARSEC(SEC),IDX=1,TAG="yrs^days^hrs^mins^secs" 
  FOR IDX=1:1:5 SET NM=$P(PS,"^",IDX) SET:NM>0 STR=STR_$S(STR]"":", ",1:"")_NM_" "_$P(TAG,"^",IDX)
  QUIT STR
 ;
TSTS2ST  ;"TEST S2STR
  NEW SEC,IDX
  FOR SEC=1:41:32000000 WRITE SEC,"= ",$$S2STR(SEC),!
  QUIT
  ;
FIXBINDEX
  ;
  NEW NAME SET NAME=""
  FOR  SET NAME=$ORDER(^LAB(60,"B",NAME)) QUIT:NAME=""  DO
  . IF $LENGTH(NAME)'=30 QUIT
  . NEW IEN SET IEN=0
  . FOR  SET IEN=$ORDER(^LAB(60,"B",NAME,IEN)) QUIT:IEN'>0  DO
  . . NEW VALUE SET VALUE=$GET(^LAB(60,"B",NAME,IEN))
  . . IF VALUE="" QUIT
  . . WRITE "^LAB(60,""B"",",NAME,",",IEN,")=",VALUE,!
  . . IF $DATA(^LAB(60,IEN,5,"B",NAME)) QUIT
  . . KILL ^LAB(60,"B",NAME,IEN)
  QUIT
 

