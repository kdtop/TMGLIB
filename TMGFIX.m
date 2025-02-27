;(Scratch code to fix various specific problems over time.), 2/2/14, 3/24/21
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
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
        NEW TMGDFN SET TMGDFN=+Y
        DO GETPAT^TMGGRC2(TMGDFN,.AGE,.GENDER,.TMGERR)
        NEW DOB SET DOB=+$PIECE($GET(^DPT(TMGDFN,0)),"^",3)
        IF $DATA(^GMR(120.5,"C",TMGDFN)) DO  QUIT:(%=-1)
        . SET %=2
        . WRITE "Patient has other vitals already defined.  DELETE them all"
        . DO YN^DICN WRITE !
        . IF %'=1 QUIT
        . NEW DIK SET DIK="^GMR(120.5,"
        . NEW DA SET DA=0
        . FOR  SET DA=$ORDER(^GMR(120.5,"C",TMGDFN,DA)) QUIT:(+DA'>0)  DO
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
        . DO MAKE1VS(TMGDFN,GENDER,Y,PCTL) ;
        QUIT

MAKE1VS(TMGDFN,GENDER,TYPE,PCTL) ;
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
        . . SET ABORT=$$Add1VS(TMGDFN,TYPE,VAL,Y,UNIT)
        QUIT

Add1VS(TMGDFN,TYPE,VALUE,DATE,UNIT)
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
        SET TMGFDA(120.5,"+1,",.02)="`"_TMGDFN
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

TESTDIC
        NEW X,Y,DIC,ANSWER
        SET DIC=2,DIC(0)="MAEQ"
        D ^DIC
        WRITE !,Y
        ;"IF +Y>0 DO
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
        . FOR  SET j=$o(^TIU(8925,IEN,"TEMP",j)) QUIT:(+j'>0)!(SAME=0)  DO
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
        NEW TMGDFN SET TMGDFN=0
        FOR  SET TMGDFN=$ORDER(@SOURCEREF@(TMGDFN)) QUIT:(+TMGDFN'>0)  DO
        . SET VISITMSG(5)="VST^PT^"_TMGDFN
        . NEW FMDT SET FMDT=0
        . FOR  SET FMDT=$ORDER(@SOURCEREF@(TMGDFN,FMDT)) QUIT:(+FMDT'>0)  DO
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
FIXSUM ;
  NEW IEN SET IEN=0
  FOR  SET IEN=$ORDER(^TMG(22733.1,IEN)) QUIT:IEN'>0  DO
  . NEW LINE SET LINE=$GET(^TMG(22733.1,IEN,0))
  . NEW LINE2 SET LINE2=$$REPLSTR^TMGSTUT3(LINE,"DOSE","STRENGTH")  ;"REPLACE STRING
  . WRITE "-",LINE,!,"+",LINE2,!
  . SET ^TMG(22733.1,IEN,0)=LINE2
  QUIT
  ;
TESTPARSE ;
  NEW LINE SET LINE="<SPAN style=""FONT-SIZE: 14px; FONT-FAMILY: Arial,"" initial? text-decoration-color: initial; "
  SET LINE=LINE_"text-decoration-style: 0px; -webkit-text-stroke-width: normal; font-variant-caps: font-variant-ligatures: "
  SET LINE=LINE_"TEXT-INDENT: rgb(255,255,255); BACKGROUND-COLOR: LETTER-SPACING: !important; inline DISPLAY: 2; "
  SET LINE=LINE_"WIDOWS: ORPHANS: left; TEXT-ALIGN: FONT-STYLE: rgb(0,0,0); COLOR: FONT-WEIGHT: none; "
  SET LINE=LINE_"FLOAT: TEXT-TRANSFORM: WORD-SPACING: WHITE-SPACE: sans-serif; Helvetica, Neue?, Helvetica>"
  NEW ATTR,ERR
  DO parseElement^%zewdHTMLParser(LINE,.ATTR,.ERR,1)
  ZWR ATTR
  QUIT
  ;
TESTHFTABLE
  NEW TMGDFN,ARR,STR,OUIT SET TMGDFN=36735  ;"W.K.JEN."
  NEW TABLIEN SET TABLIEN=2
  NEW LNIEN SET LNIEN=17
  ;"WRITE $$GETTABLX^TMGTIUO6(TMGDFN,"HEALTH FACTORS",.ARR)
  WRITE $$GETITEM^TMGPXR02(TMGDFN,TABLIEN,LNIEN,9999,.OUT),!
  WRITE $$GETITEM^TMGTIUO8(TMGDFN,TABLIEN,LNIEN,9999,.OUT),!  
  QUIT
  
;"CHECK FOR DUPLICATE LAB TESTS BEING STORED IN SAME STORAGE FIELD
TESTDUPLABSTOR ;
  NEW DUPARR
  NEW IDX SET IDX=""
  FOR  SET IDX=$ORDER(^LAB(60,"C",IDX)) QUIT:IDX=""  DO
  . NEW LASTIDX SET LASTIDX=""
  . NEW JDX SET JDX=""
  . FOR  SET JDX=$ORDER(^LAB(60,"C",IDX,JDX))  QUIT:JDX'>0  DO
  . . IF LASTIDX'="" DO
  . . . MERGE DUPARR(IDX)=^LAB(60,"C",IDX)
  . . SET LASTIDX=JDX
  SET IDX=""
  FOR  SET IDX=$ORDER(DUPARR(IDX)) QUIT:IDX=""  DO
  . NEW JDX SET JDX=""
  . FOR  SET JDX=$ORDER(DUPARR(IDX,JDX)) QUIT:JDX=""  DO
  . . SET DUPARR(IDX,JDX)=$GET(^LAB(60,JDX,0))
  IF $DATA(DUPARR) ZWR DUPARR
  ELSE  WRITE !,"NONE",!
  ;

TESTNULL0(TONULL)
  WRITE "Text before trying to output to NULL",!
  WRITE "----------------------------------------",!
  IF TONULL DO
  . SET TONULL("HANDLE")="TMGHNDL1"
  . DO OPEN^%ZISUTL(TONULL("HANDLE"),"NULL")
  . IF POP>0 SET TONULL=0 QUIT  ;"Unable to open NULL device
  . USE IO
  FOR X=1:1:10 WRITE "X=",X,!
  IF TONULL DO CLOSE^%ZISUTL(TONULL("HANDLE"))  ;"Close NULL device if opened above. 
  WRITE "----------------------------------------",!
  WRITE "Text after trying to output to NULL",!
  QUIT
  ;

TESTNULL(TONULL)
  WRITE "Text before trying to output to NULL",!
  WRITE "----------------------------------------",!
  IF TONULL DO
  . SET TONULL("HANDLE")="TMGHNDL1"
  . SET IOP="NULL",%ZIS=""
  . DO ^%ZIS
  . ;"DO OPEN^%ZISUTL(TONULL("HANDLE"),"NULL")
  . IF POP>0 SET TONULL=0 QUIT  ;"Unable to open NULL device
  FOR X=1:1:10 WRITE "X=",X,!
  IF TONULL DO
  . DO CLOSE^%ZISUTL(TONULL("HANDLE"))  ;"Close NULL device if opened above. 
  WRITE "----------------------------------------",!
  WRITE "Text after trying to output to NULL",!
  QUIT
  ;

STRIP(STR) ;
 N I
 F I=1:1:$L(STR) D       
 . S X=$E(STR,I)         
 . I X="*" S STR=$P(STR,X,1)_$P(STR,X,2,99)
 Q STR                                     
 ;
  
STRIP2(STR) ;
 N I SET I=1
 F  Q:I>$L(STR)  DO
 . S X=$E(STR,I)         
 . I X'="*" S I=I+1 QUIT
 . S STR=$E(STR,1,I-1)_$E(STR,I+1,$L(STR))
 Q STR                                     
 ;

SHRDTESTPTS
 NEW PATNAME
 SET PATNAME="ZZ"
 FOR  SET PATNAME=$O(^DPT("B",PATNAME)) QUIT:PATNAME=""  DO
 . NEW TMGDFN SET TMGDFN=0
 . FOR  SET TMGDFN=$O(^DPT("B",PATNAME,TMGDFN)) QUIT:TMGDFN'>0  DO
 . . WRITE "SHREDDING ",PATNAME,!
 . . NEW DOB,SSN
 . . SET DOB=$P($G(^DPT(TMGDFN,0)),"^",3) W "->",DOB
 . . SET $P(^DPT(TMGDFN,0),"^",3)=DOB+10000 W "-",$P($G(^DPT(TMGDFN,0)),"^",3),!
 . . SET SSN=$P($G(^DPT(TMGDFN,0)),"^",9) W "->",SSN
 . . IF $$UP^XLFSTR(SSN)'["P" DO
 . . . SET $P(^DPT(TMGDFN,0),"^",9)=SSN+132097 
 . . W "-",$P($G(^DPT(TMGDFN,0)),"^",9),!
 QUIT
 ;" 
FIXMAG ;
  SET LOCIEN=2
  NEW ROOT SET ROOT=$GET(^MAG(2005.2,LOCIEN,22700))
  NEW TEMP,IEN SET IEN=0
  FOR  SET IEN=$ORDER(^MAG(2005,IEN)) QUIT:IEN'>0  DO
  . WRITE "."
  . IF IEN#100=0 WRITE !,IEN,!
  . NEW FNAME SET FNAME=$PIECE($GET(^MAG(2005,IEN,0)),"^",2)
  . NEW ABSFNAME SET ABSFNAME="TM"_$$RJ^XLFSTR(IEN,6,"0")_"."_"ABS"
  . DO
  . . NEW SHORTHASH SET SHORTHASH="TM00/"_$E(ABSFNAME,5,6)_"/"_$E(ABSFNAME,7,8)_"/"
  . . NEW SOURCEDIR SET SOURCEDIR=ROOT_SHORTHASH  
  . . NEW SOURCEFNAME SET SOURCEFNAME=SOURCEDIR_ABSFNAME
  . . SET TEMP=$$ISFILE^TMGKERNL(SOURCEFNAME) 
  . . IF TEMP=0 QUIT
  . . NEW NEWNAME SET NEWNAME="TMG"_$$RJ^XLFSTR(IEN,11,"0")_"."_"ABS"
  . . NEW DESTDIR SET DESTDIR=ROOT_$$DIRHASH^MAGFILEB(NEWNAME,2)
  . . NEW DESTFNAME SET DESTFNAME=DESTDIR_NEWNAME
  . . NEW TEMP SET TEMP=$$ENSURDIR^TMGKERNL(DESTDIR) IF TEMP'=1 WRITE TEMP,!
  . . SET TEMP=$$MOVE^TMGKERNL(SOURCEFNAME,DESTFNAME) IF TEMP'=0 WRITE "ERROR MOVING",!
  . . WRITE "MOVE: ",SOURCEFNAME," -> ",DESTFNAME,!
  . NEW ABS2FNAME SET ABS2FNAME="TM"_$$RJ^XLFSTR(IEN,12,"0")_"."_"ABS"
  . DO
  . . NEW HASH SET HASH=$$DIRHASH^MAGFILEB(ABS2FNAME,2)
  . . NEW SOURCEDIR SET SOURCEDIR=ROOT_HASH  
  . . NEW SOURCEFNAME SET SOURCEFNAME=SOURCEDIR_ABS2FNAME
  . . SET TEMP=$$ISFILE^TMGKERNL(SOURCEFNAME) 
  . . IF TEMP=0 QUIT
  . . NEW NEWNAME SET NEWNAME="TMG"_$$RJ^XLFSTR(IEN,11,"0")_"."_"ABS"
  . . NEW DESTDIR SET DESTDIR=ROOT_$$DIRHASH^MAGFILEB(NEWNAME,2)
  . . NEW DESTFNAME SET DESTFNAME=DESTDIR_NEWNAME
  . . NEW TEMP SET TEMP=$$ENSURDIR^TMGKERNL(DESTDIR) IF TEMP'=1 WRITE TEMP,!
  . . SET TEMP=$$MOVE^TMGKERNL(SOURCEFNAME,DESTFNAME) IF TEMP'=0 WRITE "ERROR MOVING",!
  . . WRITE "MOVE: ",SOURCEFNAME," -> ",DESTFNAME,!
  . IF FNAME?1"TMG"11N1"."3A DO  QUIT
  . . ;"WRITE FNAME,!
  . . NEW SHORTNAME SET SHORTNAME="TM0"_$E(FNAME,10,14)_"."_$P(FNAME,".",2)
  . . NEW SHORTHASH SET SHORTHASH="TM00/"_$E(FNAME,11,12)_"/"_$E(FNAME,13,14)_"/"
  . . NEW OLDFNAME SET OLDFNAME=ROOT_SHORTHASH_SHORTNAME
  . . SET TEMP=$$ISFILE^TMGKERNL(OLDFNAME)
  . . IF TEMP=0 QUIT
  . . WRITE OLDFNAME," STILL EXISTS!",!
  . . NEW DESTDIR SET DESTDIR=ROOT_$$DIRHASH^MAGFILEB(FNAME,2)
  . . NEW DESTFNAME SET DESTFNAME=DESTDIR_FNAME
  . . SET TEMP=$$ISFILE^TMGKERNL(DESTFNAME) 
  . . IF TEMP=0 QUIT
  . . WRITE DESTFNAME," EXISTS!",!
  . . NEW TEMP SET TEMP=$$DELFILE^TMGIOUTL(OLDFNAME)
  . . IF TEMP=1 W " --> DELETED: ",OLDFNAME,!
  . IF FNAME?1"TM0"11N1"."3A DO  QUIT
  . . NEW HASHDIR SET HASHDIR=$$DIRHASH^MAGFILEB(FNAME,2)
  . . NEW SOURCEDIR SET SOURCEDIR=ROOT_HASHDIR
  . . NEW SOURCEFNAME SET SOURCEFNAME=SOURCEDIR_FNAME
  . . NEW NEWNAME SET NEWNAME=FNAME SET $E(NEWNAME,3)="G"
  . . NEW DESTDIR SET DESTDIR=ROOT_$$DIRHASH^MAGFILEB(NEWNAME,2)
  . . NEW DESTFNAME SET DESTFNAME=DESTDIR_NEWNAME
  . . NEW TEMP SET TEMP=$$ENSURDIR^TMGKERNL(DESTDIR) IF TEMP'=1 WRITE TEMP,!
  . . SET TEMP=$$MOVE^TMGKERNL(SOURCEFNAME,DESTFNAME) IF TEMP'=0 WRITE "ERROR MOVING",!
  . . WRITE "MOVE: ",SOURCEFNAME," -> ",DESTFNAME,!
  . . NEW ABSFILE SET ABSFILE=SOURCEFNAME SET $P(ABSFILE,".",2)="ABS"
  . . NEW DESTABSFILE SET DESTABSFILE=DESTFNAME SET $P(DESTABSFILE,".",2)="ABS"
  . . SET TEMP=$$ISFILE^TMGKERNL(ABSFILE)
  . . IF TEMP=0 DO
  . . . SET TEMP=$$MOVE^TMGKERNL(ABSFILE,DESTABSFILE) IF TEMP'=0 WRITE "ERROR MOVING",!
  . . . WRITE "MOVE: ",ABSFILE," -> ",DESTABSFILE,!
  . . SET ^MAG(2005,"F",NEWNAME,IEN)=""
  . . KILL ^MAG(2005,"F",FNAME)
  . . SET $PIECE(^MAG(2005,IEN,0),"^",2)=NEWNAME
  . IF FNAME?1"TM"6N1"."3A DO  QUIT
  . . NEW HASH1 SET HASH1=$E(FNAME,1,4)_"/"_$E(FNAME,5,6)_"/"_$E(FNAME,7,8)
  . . NEW SOURCEFNAME SET SOURCEFNAME=ROOT_HASH1_"/"_FNAME
  . . IF '$$ISFILE^TMGKERNL(SOURCEFNAME) DO  QUIT
  . . . ;"WRITE IEN,": ",SOURCEFNAME," <-- NOT FOUND!",!
  . . NEW FNAME3 SET FNAME3="TMG0000000"_$E(FNAME,5,8)_"."_$P(FNAME,".",2)
  . . NEW DESTDIR SET DESTDIR=ROOT_$$DIRHASH^MAGFILEB(FNAME3,2)
  . . NEW TEMP SET TEMP=$$ENSURDIR^TMGKERNL(DESTDIR) IF TEMP'=1 WRITE TEMP,!
  . . NEW DESTFNAME SET DESTFNAME=DESTDIR_FNAME3
  . . SET TEMP=$$ISFILE^TMGKERNL(DESTFNAME)
  . . IF TEMP=0 DO  QUIT:(TEMP'=0) 
  . . . SET TEMP=$$Copy^TMGKERNL(SOURCEFNAME,DESTFNAME)
  . . . IF TEMP=0 WRITE "FILE COPIED",!
  . . . ELSE  WRITE "ERROR CODE: ",TEMP,!
  . . SET ^MAG(2005,"F",FNAME3,IEN)=""
  . . KILL ^MAG(2005,"F",FNAME)
  . . SET $PIECE(^MAG(2005,IEN,0),"^",2)=FNAME3
  . . WRITE IEN,": ","MOVED ",FNAME," -> ",FNAME3,!
  QUIT
  ;
FINDMEDS ;  ;"SCAN ALL PATIENTS AND SEE IF THEY HAVE ACTIVE MEDS.
  NEW ADFN SET ADFN=0
  NEW USER SET USER=168
  NEW VIEW SET VIEW=1
  NEW UPDATE SET UPDATE=1
  NEW CT SET CT=0
  FOR  SET ADFN=$ORDER(^DPT(ADFN)) QUIT:ADFN'>0  DO
  . NEW ARR
  . DO ACTIVE^ORWPS(.ARR,ADFN,USER,1,1) ; retrieve active inpatient & outpatient meds
  . KILL ARR(0)
  . SET CT=CT+1
  . ;"//IF CT#100 WRITE "."
  . IF $DATA(ARR)=0 QUIT
  . NEW ZN SET ZN=$GET(^DPT(ADFN,0))
  . WRITE !,"DFN=",ADFN," ",$PIECE(ZN,"^",1),!
  . ZWR ARR
  QUIT
  ;
FIXPATCH  ;
  DO FIXPATCH^TMGPAT5
  QUIT
  ;
   
FIX22719D2 ;
  NEW ADFN SET ADFN=0
  FOR  SET ADFN=$ORDER(^TMG(22719.2,ADFN)) QUIT:ADFN'>0  DO
  . NEW SAVEARR
  . NEW TOPICREC SET TOPICREC=0
  . FOR  SET TOPICREC=$ORDER(^TMG(22719.2,ADFN,1,TOPICREC)) QUIT:TOPICREC'>0  DO
  . . NEW DTREC SET DTREC=0
  . . FOR  SET DTREC=$ORDER(^TMG(22719.2,ADFN,1,TOPICREC,1,DTREC)) QUIT:DTREC'>0  DO
  . . . NEW ZN SET ZN=$GET(^TMG(22719.2,ADFN,1,TOPICREC,1,DTREC,0))
  . . . NEW DT SET DT=$PIECE(ZN,"^",1)
  . . . NEW IEN8925 SET IEN8925=$PIECE(ZN,"^",2)
  . . . IF IEN8925>0 QUIT  ;"already present
  . . . SET IEN8925=+$GET(SAVEARR(DT))
  . . . IF IEN8925'>0  SET IEN8925=+$$GETDOC(ADFN,DT)
  . . . IF IEN8925'>0 QUIT
  . . . SET $PIECE(ZN,"^",2)=IEN8925
  . . . SET ^TMG(22719.2,ADFN,1,TOPICREC,1,DTREC,0)=ZN  ;"DIRECT WRITE
  . . . SET SAVEARR(DT)=IEN8925
  . WRITE "."
  . ;"IF $DATA(SAVEARR) ZWR SAVEARR
  QUIT
  ;
GETDOC(ADFN,DT)  ;"CONVERT DFN + DT --> IEN8925
  NEW RESULT SET RESULT=+$ORDER(^TIU(8925,"ZTMGPTDT",ADFN,DT,0))
  QUIT RESULT
  ;
FIX22719D5 ;
  NEW IEN SET IEN=0
  FOR  SET IEN=$ORDER(^TMG(22719.5,IEN)) QUIT:IEN'>0  DO
  . NEW ZN SET ZN=$GET(^TMG(22719.5,IEN,0))
  . NEW DFN SET DFN=$PIECE(ZN,"^",1)
  . IF DFN=IEN QUIT
  . IF $DATA(^TMG(22719.5,DFN)) DO  QUIT
  . . WRITE !,"CAN'T MERGE INTO #"_DFN_", BECAUSE DATA ALREADY THERE.  SKIPPING",!
  . MERGE ^TMG(22719.5,DFN)=^TMG(22719.5,IEN) KILL ^TMG(22719.5,IEN)
  QUIT 
  ;"
TIUNOPAT
  ;"SCRATCH FUNCTION TO FIND NOTES WITHOUT PATIENTS ATTACHED TO THEM
  NEW TIUIEN SET TIUIEN=0
  NEW COUNT SET COUNT=0
  FOR  SET TIUIEN=$O(^TIU(8925,TIUIEN)) QUIT:TIUIEN'>0  DO
  . NEW ZN SET ZN=$G(^TIU(8925,TIUIEN,0))
  . NEW TMGDFN SET TMGDFN=+$P(ZN,"^",2)
  . IF TMGDFN'>0  DO
  . . NEW TITLEIEN SET TITLEIEN=+$P(ZN,"^",1)
  . . NEW TITLE SET TITLE=$P($G(^TIU(8925.1,TITLEIEN,0)),"^",1)
  . . NEW ENTRYDATE SET ENTRYDATE=$P($G(^TIU(8925,TIUIEN,12)),"^",1)
  . . WRITE "FOUND ONE NOTE: ",TIUIEN,!
  . . WRITE "         TITLE: ",TITLE,!
  . . WRITE "       ENTERED: ",$$EXTDATE^TMGDATE(ENTRYDATE,1),!
  . . SET COUNT=COUNT+1
  . . ;"DELETE THIS NOTE NOW
  . . NEW DIK SET DIK="^TIU(8925,",DA=TIUIEN
  . . D ^DIK
  WRITE "TOTAL RECORDS DELETED: ",COUNT,!
  QUIT
  ;"
MOVEICDS ;
  ;"This will be one-time code for moving data from 22719.5 LINK->ICD to LINK->ICDS (multiple)
  NEW PTIEN SET PTIEN=0
  FOR  SET PTIEN=$ORDER(^TMG(22719.5,PTIEN)) QUIT:PTIEN'>0  DO
  . NEW LINKIEN SET LINKIEN=0
  . FOR  SET LINKIEN=$ORDER(^TMG(22719.5,PTIEN,1,LINKIEN)) QUIT:LINKIEN'>0  DO
  . . NEW ZN SET ZN=$GET(^TMG(22719.5,PTIEN,1,LINKIEN,0))
  . . NEW IEN80 SET IEN80=+$PIECE(ZN,"^",3) QUIT:IEN80'>0
  . . NEW SUBSUBIEN SET SUBSUBIEN=$ORDER(^TMG(22719.5,PTIEN,1,LINKIEN,1,"B",IEN80,""))
  . . IF SUBSUBIEN>0 QUIT  ;"IEN80 already present
  . . NEW TMGFDA,TMGIEN,TMGMSG,IENS
  . . SET IENS="+1,"_LINKIEN_","_PTIEN_","
  . . SET TMGFDA(22719.511,IENS,.01)="`"_IEN80
  . . DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
  . . IF $DATA(TMGMSG) DO  QUIT
  . . . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)  
  . . SET SUBSUBIEN=+$GET(TMGIEN(1))
  . . IF SUBSUBIEN'>0 QUIT
  . . SET TMGFDA(22719.51,LINKIEN_","_PTIEN_",",.03)="@"
  . . DO FILE^DIE("E","TMGFDA","TMGMSG")
  . . IF $DATA(TMGMSG) DO  QUIT
  . . . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)  
  QUIT
  ;
LABFILE
  NEW IDX SET IDX=0
  FOR  SET IDX=$O(^TMG(22748,IDX)) QUIT:IDX'>0  DO
  . NEW SUBIDX SET SUBIDX=0
  . FOR  SET SUBIDX=$O(^TMG(22748,IDX,5,SUBIDX)) QUIT:SUBIDX'>0  DO  
  . . NEW FILESTR SET FILESTR=$G(^TMG(22748,IDX,5,SUBIDX,0))
  . . IF FILESTR["^" DO
  . . . NEW PATH,FILE SET PATH=$P(FILESTR,"^",1),FILE=$P(FILESTR,"^",2)
  . . . WRITE "FIXING ",FILESTR," TO BE ",PATH,FILE,!
  . . . SET ^TMG(22748,IDX,5,SUBIDX,0)=PATH_FILE
  . . ;"^TMG(22748,5674,5,1,0)="076/130/^24C201HKDCLAB.pdf"   
  QUIT
  ;"
IMMTEST
  NEW IDX SET IDX=0
  FOR  SET IDX=$O(^AUPNVIMM(IDX)) QUIT:IDX'>0  DO
  . NEW ZN SET ZN=$G(^AUPNVIMM(IDX,0))
  . ;"IF $P(ZN,"^",1)'=129 QUIT
  . IF $P(ZN,"^",2)'=75734 QUIT
  . WRITE "FOUND ONE ",IDX,!
  QUIT
