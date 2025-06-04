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
  ;"
TESTFIX  
  ;"NEW ADFN SET ADFN=75072
  NEW ADFN SET ADFN=36735  
  DO FIX22719D2(ADFN)
  QUIT
  ;
FIX22719D2(ADFN) ;
  ;
  NEW DATA DO TOPIC2DATA^TMGTOPIC(ADFN,.DATA)
  NEW COMPOSITE
  NEW ATOPIC SET ATOPIC=""
  FOR  SET ATOPIC=$ORDER(DATA("TOPIC",ATOPIC)) QUIT:ATOPIC=""  DO
  . NEW TOPICS
  . NEW ADT SET ADT=0
  . FOR  SET ADT=$ORDER(DATA("TOPIC",ATOPIC,ADT)) QUIT:ADT'>0  DO
  . . NEW CURARR MERGE CURARR=DATA("TOPIC",ATOPIC,ADT)
  . . NEW CURSTR SET CURSTR=$$ARR2STR^TMGSTUT2(.CURARR," ")
  . . SET TOPICS(ADT)=CURSTR
  . NEW SAVEDTOPICS MERGE SAVEDTOPICS=TOPICS
  . DO CLEANTOPICS(.TOPICS)
  . ;"ZWR SAVEDTOPICS WRITE ! ZWR TOPICS
  . MERGE COMPOSITE(ATOPIC)=TOPICS
  ;"TO DO... FILE RESULTS BACK INTO DATABASE, DELETING EMPTY ENTRIES
  ;"SAVE DFN TO ^TMG("TMP","FIX22719.2",<ADFN>)="" to keep record of those already done.
  ;"BEFORE DOING THIS, I NEED TO PREVENT FUTURE TOPIC FROM FILING INCORRECTLY.  
  QUIT
  ;
CLEANTOPICS(TOPICS)  ;
  NEW DIVS SET DIVS=" ,;:.!?-"
  NEW ADTLATER SET ADTLATER=""
  FOR  SET ADTLATER=$ORDER(TOPICS(ADTLATER),-1) QUIT:ADTLATER'>0  DO                                
  . NEW CURARR,CURTEXT SET CURTEXT=$GET(TOPICS(ADTLATER)) QUIT:CURTEXT=""
  . NEW ADTEARLIER SET ADTEARLIER=ADTLATER
  . FOR  SET ADTEARLIER=$ORDER(TOPICS(ADTEARLIER),-1) QUIT:(ADTEARLIER'>0)  DO             
  . . NEW PRIORARR,PRIORTEXT SET PRIORTEXT=$GET(TOPICS(ADTEARLIER)) QUIT:PRIORTEXT=""
  . . NEW MATCHES DO SUBSTRMATCH^TMGSTUT3(PRIORTEXT,CURTEXT,.MATCHES,.PRIORARR,.CURARR)
  . . NEW HASMATCH FOR  DO  QUIT:HASMATCH=0
  . . . SET HASMATCH=0
  . . . NEW ALEN SET ALEN=+$ORDER(MATCHES("LENIDX",""),-1) QUIT:ALEN'>0  
  . . . NEW MATCHIDX SET MATCHIDX=$ORDER(MATCHES("LENIDX",ALEN,0)) QUIT:MATCHIDX'>0
  . . . NEW AMATCH SET AMATCH=$GET(MATCHES(MATCHIDX)) QUIT:AMATCH=""
  . . . NEW MATCHLEN SET MATCHLEN=$PIECE(AMATCH,"^",3) QUIT:(MATCHLEN<=2)
  . . . SET HASMATCH=1 
  . . . NEW STARTPOS SET STARTPOS=+AMATCH
  . . . NEW LEN SET LEN=+$PIECE(AMATCH,"^",2)
  . . . NEW ENDPOS SET ENDPOS=+$PIECE(AMATCH,"^",4)
  . . . NEW PARTA SET PARTA=""
  . . . IF STARTPOS>0 SET PARTA=$EXTRACT(CURTEXT,1,STARTPOS-1)
  . . . NEW PARTB SET PARTB=$EXTRACT(CURTEXT,STARTPOS,ENDPOS)
  . . . NEW PARTC SET PARTC=""
  . . . IF LEN>0 SET PARTC=$EXTRACT(CURTEXT,ENDPOS+1,$LENGTH(CURTEXT))
  . . . SET CURTEXT=$SELECT(PARTA'="":PARTA_"...",1:"")_PARTC
  . . . NEW SCANPOS,SCANDONE SET SCANPOS=1,SCANDONE=0
  . . . FOR SCANPOS=1:1:$LENGTH(CURTEXT) DO  QUIT:SCANDONE
  . . . . NEW CH SET CH=$EXTRACT(CURTEXT,SCANPOS)
  . . . . SET SCANDONE=(((DIVS[CH)=0)&(CH'=""))
  . . . IF SCANPOS>1 DO
  . . . . IF SCANDONE=0 SET CURTEXT="" QUIT   ;"never found a non-div char.  
  . . . . SET CURTEXT=$EXTRACT(CURTEXT,SCANPOS,$LENGTH(CURTEXT))
  . . . SET TOPICS(ADTLATER)=CURTEXT
  . . . KILL CURARR,MATCHES 
  . . . DO SUBSTRMATCH^TMGSTUT3(PRIORTEXT,CURTEXT,.MATCHES,.PRIORARR,.CURARR)  
  NEW ADT SET ADT=0
  FOR  SET ADT=$ORDER(TOPICS(ADT)) QUIT:ADT'>0  DO
  . NEW CURARR,CURTEXT SET CURTEXT=$GET(TOPICS(ADT))
  . IF CURTEXT="" QUIT
  . NEW DONE SET DONE=0
  . FOR  DO  QUIT:DONE
  . . NEW CH SET CH=$EXTRACT(CURTEXT,1) 
  . . IF $ASCII(CH)=-1 SET DONE=1 QUIT
  . . IF DIVS'[CH SET DONE=1 QUIT
  . . SET CURTEXT=$EXTRACT(CURTEXT,2,$LENGTH(CURTEXT))
  . SET TOPICS(ADT)=CURTEXT
  QUIT

  
  
  