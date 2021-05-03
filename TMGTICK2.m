TMGTICK2 ;TMG/kst-Tickler Text Object Support Files;09/04/08, 2/2/14, 3/24/21
         ;;1.0;TMG-LIB;**1**;09/05/08
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"---------------------------------------------------------------------------
 ;"PUBLIC FUNCTIONS
 ;"---------------------------------------------------------------------------
 ;"GETMSG(DocIEN,WPArray) -- retrieve tickler message in document.
 ;"FLMSG(IEN) -- return the first line of the tickler message
 ;"SELTCKLS(SelArray) -- Browse tickler messages and return array of IEN's selected.
 ;"REUSER -- Allow browsing for a SET of Tickler files, and reassigning the target user
 ;"REDATE -- Allow browsing for a SET of Tickler files, and reassigning the due date
 ;"BROWSE -- Browse tickler messages.
 ;"$$SELTICKLERS(SelArray) -- Browse tickler messages and return array of IEN's selected.
 ;"CLEANDON -- remove tickler messages that have been completed, thus no longer needed.
 ;"CLEANDSC  -- remove tickler messages that have been discarded, thus no longer needed.
 ;"DispTicklers(IENArray) -- Display a list of tickler messages

 ;"---------------------------------------------------------------------------
 ;"PRIVATE FUNCTIONS
 ;"---------------------------------------------------------------------------
 ;"Dependencies:
 ;"  IENSELCT^TMGUSRI3
 ;"    --> SELECT^%ZVEMKT
 ;"    --> ItrAInit^TMGITR
 ;"  MENU^TMGUSRI2
 ;"  SHOWDIER^TMGDEBU2

FLMSG(IEN)
        ;"Purpose: To return the first line of the tickler message
        ;"NOTE: !!! DON'T REMOVE THIS FUNCTION.  It is called by the computed field,
        ;"      FIRST LINE OF MESSAGE (field #5) in file 22705.5 (TICKLER FILE MESSAGES)
        ;"Input: IEN: IEN in file 22705.5
        ;"Output: Returns first line, or "" IF null

        NEW result SET result=""
        NEW DocIEN SET DocIEN=+$PIECE($GET(^TMG(22705.5,IEN,0)),"^",4)
        IF DocIEN>0 do
        . NEW WPArray
        . NEW temp SET temp=$$GETMSG(DocIEN,.WPArray)
        . SET result=$GET(WPArray(1))
        QUIT result


GETMSG(DocIEN,WPArray)
        ;"Purpose: To retrieve the message for a tickler message in document.
        ;"Note: It is expected that the Tickler text structure will be:
        ;"
        ;"        ======= [TICKLER MESSGE] =======
        ;"        #DUE#: Put-DUE-DATE-here
        ;"        ================================
        ;"        Message: ...
        ;"
        ;"        ================================
        ;"
        ;"      And specifically, the key elements are:
        ;"        1. Entire Tickler starts with [TICKLER MESSGE]
        ;"        2. Message starts on line after ===========
        ;"        3. Messge ends with line with ===========
        ;"              If no closing =========== found, message extends to end of document
        ;"
        ;"Input: DocIEN -- IEN in 8925
        ;"       WPArray -- PASS BY REFERENCE, an OUT PARAMETER.  Returns message.  Format:
        ;"                  WPArray(1)='1st list'
        ;"                  WPArray(2)='2nd line' etc...
        ;"Result: 1 IF found, 0 IF not.

        NEW found,line SET (found,line)=0
        FOR  SET line=$ORDER(^TIU(8925,DocIEN,"TEXT",line)) QUIT:(+line'>0)!found  do
        . SET found=($GET(^TIU(8925,DocIEN,"TEXT",line,0))["[TICKLER MESSGE]")
        . IF (found=0)&($GET(^TIU(8925,DocIEN,"TEXT",line,0))["[TICKLER") DO
        . . ;"if TICKLER tag is broken between 2 lines   1/14/20
        . . SET found=1
        . . SET line=line+1
        . IF found do
        . . NEW done SET done=0
        . . NEW lineText SET lineText=""
        . . FOR  QUIT:done  SET line=$ORDER(^TIU(8925,DocIEN,"TEXT",line)) QUIT:(+line'>0)!done  do
        . . . SET done=$GET(^TIU(8925,DocIEN,"TEXT",line,0))["====="
        . . SET done=0
        . . NEW wpIndex SET wpIndex=1
        . . FOR  SET line=$ORDER(^TIU(8925,DocIEN,"TEXT",line)) QUIT:(+line'>0)!done  do
        . . . SET done=$GET(^TIU(8925,DocIEN,"TEXT",line,0))["====="
        . . . IF done QUIT
        . . . SET WPArray(wpIndex)=$GET(^TIU(8925,DocIEN,"TEXT",line,0))
        . . . SET wpIndex=wpIndex+1

        QUIT found


BROWSE
        ;"Purpose: To browse tickler messages
        ;"Results: none

        NEW SelArray,abort
        WRITE !
        SET abort=$$SELTICKLERS(.SelArray)
        IF abort GOTO BWDN
        IF $DATA(SelArray)=0 GOTO BWDN
        NEW % SET %=1
        WRITE "Review tickler messages for selected entries?" DO YN^DICN WRITE !
        IF %=-1 SET abort=1 GOTO SELTDONE
        IF %=1 DO DispTicklers(.SelArray)
        WRITE "Goodbye.",!
BWDN    QUIT


DELTICKL ;
        ;"Purpose: allow user to pick tickler message to delete.
        NEW SelArray
        WRITE !
        NEW % SET %=2
        WRITE "Select tickler messages to DELETE" DO YN^DICN WRITE !
        IF %'=1 GOTO DTDN
        SET abort=$$SELTICKLERS(.SelArray)
        IF abort GOTO DTDN
        IF $DATA(SelArray)=0 GOTO DTDN

        SET %=1
        WRITE "Review tickler messages for selected entries?" DO YN^DICN WRITE !
        IF %=-1 GOTO DTDN
        IF %=1 DO DispTicklers(.SelArray)

        SET %=2
        WRITE "Delete selected tickler messages" DO YN^DICN WRITE !
        IF %=-1 GOTO DTDN
        NEW DelCt SET DelCt=0
        IF %=1 do
        . SET DelCt=$$DELSET(.SelArray)
        . WRITE DelCt," tickler messages deleted.",!

        WRITE "Goodbye.",!
        DO PRESS2GO^TMGUSRI2
DTDN    QUIT



SELTICKLERS(SelArray)
        ;"Browse tickler messages and return array of IEN's selected.
        ;"Input: SelArray -- PASS BY REFERENCE.  An OUT ARRAY.
        ;"Output: SelArray is filled as follows:
        ;"          SelArray(IEN)=DispLineNumber
        ;"          SelArray(IEN)=DispLineNumber
        ;"Results: 1 IF aborted, otherwise 0

        NEW abort SET abort=0
        KILL SelArray
        WRITE !,"== TICKER MESSAGES BROWSER ==",!!
        NEW % SET %=2
        WRITE "View COMPLETED ticker messages " DO YN^DICN WRITE !
        IF %=-1 GOTO SELTDONE
        NEW HideCompl SET HideCompl=(%=2)
        SET %=2 WRITE "View DISCARDED ticker messages " DO YN^DICN WRITE !
        IF %=-1 GOTO SELTDONE
        NEW HideDiscarded SET HideDiscarded=(%=2)

        NEW Menu,usrChoice
        NEW LineCt SET LineCt=1
        SET Menu(0)="Pick Display Order for Selector"
        IF HideCompl do
        . SET Menu(LineCt)="User Name; Due Date; Patient Name"_$C(9)_"3;1;.01;2^20;15;20;10",LineCt=LineCt+1
        . SET Menu(LineCt)="Patient Name; User Name; Due Date"_$C(9)_".01;3;1;2^20;20;15;10",LineCt=LineCt+1
        . SET Menu(LineCt)="Due Date; Patient Name; User Name"_$C(9)_"1;.01;3;2^15;20;20;10",LineCt=LineCt+1
        . SET Menu(LineCt)="Note Date; Patient Name; User Name"_$C(9)_"4;.01;3;2^15;20;10;15",LineCt=LineCt+1
        ELSE  do
        . SET Menu(LineCt)="User Name; Status; Due Date; Patient Name"_$C(9)_"3;2;1;.01^20;10;15;20",LineCt=LineCt+1
        . SET Menu(LineCt)="Patient Name; Status; User Name; Due Date"_$C(9)_".01;2;3;1^20;10;20;15",LineCt=LineCt+1
        . SET Menu(LineCt)="Due Date; Patient Name; Status; User Name"_$C(9)_"1;.01;2;3^15;20;10;20",LineCt=LineCt+1
        . SET Menu(LineCt)="Note Date; Patient Name; Status; User Name"_$C(9)_"4;.01;2;3^15;20;10;15",LineCt=LineCt+1
        . SET Menu(LineCt)="Status; Due Date; Patient Name; User Name"_$C(9)_"2;1;.01;3^10;15;20;20",LineCt=LineCt+1

        SET usrChoice=$$MENU^TMGUSRI2(.Menu,3)
        IF usrChoice="^" GOTO SELTDONE

        NEW fields,widths
        SET fields=$PIECE(usrChoice,"^",1)
        SET widths=$PIECE(usrChoice,"^",2)

        NEW IENArray
        NEW IEN SET IEN=0
        FOR  SET IEN=$ORDER(^TMG(22705.5,IEN)) QUIT:(+IEN'>0)  do
        . NEW status
        . SET status=$PIECE($GET(^TMG(22705.5,IEN,0)),"^",3)
        . IF (status="C"),(HideCompl=1) QUIT
        . IF (status="O"),(HideDiscarded=1) QUIT   ;"FYI: 'O:ORPHAN' status renamed to 'O:DISCARDED'
        . SET IENArray(IEN)=""
        NEW Header SET Header="Pick Tickler Messages. Press <ESC><ESC> when done."
        DO IENSELCT^TMGUSRI3("IENArray","SelArray",22705.5,fields,widths,Header,fields)
SELTDONE
        QUIT abort


CLEANDON ;
        ;"Purpose: to remove tickler messages that have been completed, thus no longer needed.
        ;"Results: None
        WRITE !,"== CLEAN UP COMPLETED TICKER MESSAGES ==",!!
        NEW % SET %=2
        WRITE "DELETE all COMPLETED ticker messages " DO YN^DICN WRITE !
        IF %'=1 GOTO DELDONE
        DO GetStatusSet("C",.IENArray) ;
        NEW DelCt SET DelCt=$$DELSET(.IENArray)
        WRITE DelCt," completed tickler messages deleted.",!
DELDONE QUIT

CLEANOPH ;
CLEANDSC ;
        ;"Purpose: to remove tickler messages that have been discarded, thus no longer needed.
        ;"NOTE: An discarded note is created when a user launches a tickler object in a note, but
        ;"      then removes the text, so that the note does not actually have a tickler in it.
        ;"Results: None
        NEW abort SET abort=0
        New IENArray
        WRITE !,"== CLEAN UP DISCARDED TICKER MESSAGES ==",!!
        WRITE "Note: An DISCARDED ticker message occurs when a user launches",!
        WRITE "      the tickler text object from in CPRS, but then deletes",!
        WRITE "      it, so that the note does not actually have a tickler",!
        WRITE "      message in it.  There should be no harm in doing this.",!,!
        NEW % SET %=2
        WRITE "DELETE all DISCARDED ticker messages " DO YN^DICN WRITE !
        IF %'=1 GOTO ORPHDONE
        DO GetStatusSet("O",.IENArray) ;
        NEW DelCt SET DelCt=$$DELSET(.IENArray)
        WRITE DelCt," discarded tickler messages deleted.",!
        DO PRESS2GO^TMGUSRI2
ORPHDONE QUIT

GetStatusSet(Status,IENArray) ;
        ;"Purpose: return a SET of entries with given status.
        ;"Input:  Status -- the internal form of desired status.
        ;"        IENArray. PASS BY REFERENCE.  format as below.
        ;"          IENArray(IEN)=""
        ;"          IENArray(IEN)=""
        NEW IEN SET IEN=0
        FOR  SET IEN=$ORDER(^TMG(22705.5,IEN)) QUIT:(+IEN'>0)  do
        . NEW ThisStat SET ThisStat=$PIECE($GET(^TMG(22705.5,IEN,0)),"^",3)
        . IF (ThisStat=Status) SET IENArray(IEN)=""
        QUIT

DELSET(IENArray) ;
        ;"Purpose: To delete the specified Tickler Entries.
        ;"Input: IENArray. PASS BY REFERENCE.  format as below.
        ;"          IENArray(IEN)=""
        ;"          IENArray(IEN)=""
        ;"      NOTe: All included entries will be deleted with NO confirmation.
        ;"Results: returns number of deleted entries.
        ;
        NEW DIK SET DIK="^TMG(22705.5,"
        NEW DA
        NEW DelCt SET DelCt=0
        NEW IEN SET IEN=0
        FOR  SET IEN=$ORDER(IENArray(IEN)) QUIT:(+IEN'>0)  do
        . SET DA=IEN DO ^DIK
        . SET DelCt=DelCt+1
        QUIT DelCt

DispTicklers(IENArray)
        ;"Purpose: Display a list of tickler messages
        ;"Input: IENArray. PASS BY REFERENCE.  format:
        ;"          IENArray(IEN)=""
        ;"          IENArray(IEN)=""
        ;"Results: None

        NEW count SET count=0
        NEW abort SET abort=0
        NEW TklIEN SET TklIEN=""
        FOR  SET TklIEN=$ORDER(SelArray(TklIEN)) QUIT:(TklIEN="")!abort  do
        . SET count=count+1
        . WRITE "----------------------------------",!
        . WRITE "STATUS:   ",$$GET1^DIQ(22705.5,TklIEN,2),!
        . WRITE "DUE DATE: ",$$GET1^DIQ(22705.5,TklIEN,1),!
        . WRITE "PATIENT:  ",$$GET1^DIQ(22705.5,TklIEN,.01),!
        . WRITE "DOCUMENT: ",$$GET1^DIQ(22705.5,TklIEN,.05)," (#",$$GET1^DIQ(22705.5,TklIEN,.05,"I"),")",!
        . WRITE "DOC DATE: ",$$GET1^DIQ(22705.5,TklIEN,4),!
        . WRITE "USER:     ",$$GET1^DIQ(22705.5,TklIEN,3),!
        . WRITE "MESSAGE (1st line):",!," ",$$GET1^DIQ(22705.5,TklIEN,5),!
        . ;"if count#3=0 do
        . NEW temp read "Press Enter to Continue",temp:$GET(DTIME,3600),!
        . SET abort=(temp="^")

        IF count=0 WRITE "(No items to display.)",!
        WRITE !
        QUIT


REUSER  ;"Reassign Tickler File Recipient User
        ;"Purpose: to allow browsing for a SET of Tickler files, and reassigning the target user
        ;"Result: none

        NEW numErrors SET numErrors=0
        NEW NumProcessed SET NumProcessed=0

        WRITE !," -= REASSIGN RECIPIENT USER FOR TICKLER MESSAGES =-",!,!
        WRITE "You will next be able to select tickler messages to reassign.",!
        WRITE "Note: Only change tickler messages with a PENDING status.",!
        WRITE "      Changing others will have no effect.",!,!
        DO PRESS2GO^TMGUSRI2

        IF $$SELTICKLERS(.SelArray)=1 GOTO REUDONE

        IF $DATA(SelArray)=0 GOTO REUDONE
        NEW % SET %=2
        WRITE "Pick NEW recipient user for the selected tickler messages?"
        DO YN^DICN WRITE !
        IF %'=1 GOTO REUDONE

        NEW DIC SET DIC=200
        SET DIC(0)="MAEQ"
        SET DIC("A")="Select NEW RECIPIENT USER: "
        DO ^DIC WRITE !
        IF +Y'>0 GOTO REUDONE

        NEW IEN SET IEN=""
        FOR  SET IEN=$ORDER(SelArray(IEN)) QUIT:(IEN="")  do
        . SET NumProcessed=NumProcessed+1
        . NEW TMGFDA,TMGMSG
        . SET TMGFDA(22705.5,IEN_",",3)=+Y
        . DO FILE^DIE("","TMGFDA","TMGMSG")
        . IF $DATA(TMGMSG("DIERR"))>0 do
        . . DO SHOWDIER^TMGDEBU2(.TMGMSG)
        . . SET numErrors=numErrors+1
REUDONE
        WRITE !,NumProcessed," tickler message file entries processed.",!
        IF NumProcessed>0 WRITE numErrors," errors encountered.",!
        WRITE "Goodbye",!
        QUIT


REDATE  ;"Reassign Due Dates for Tickler File
        ;"Purpose: to allow browsing for a SET of Tickler files, and reassigning due date
        ;"Result: none

        WRITE !," -= REASSIGN DUE DATE FOR TICKLER MESSAGES =-",!,!
        WRITE "You will next be able to select tickler messages to change.",!
        WRITE "Note: Only change tickler messages with a PENDING status.",!
        WRITE "      Changing others will have no effect.",!,!
        DO PRESS2GO^TMGUSRI2

        IF $$SELTICKLERS(.SelArray)=1 GOTO REDDONE

        NEW numErrors SET numErrors=0
        NEW NumProcessed SET NumProcessed=0
        IF $DATA(SelArray)=0 GOTO REUDONE
        NEW % SET %=2
        WRITE "Pick NEW DUE DATE for the selected tickler messages?"
        DO YN^DICN WRITE !
        IF %'=1 GOTO REDDONE

        NEW DIR,X,Y
        SET DIR(0)="DO",DIR("A")="Enter NEW DUE DATE (^ to abort)"
        DO ^DIR WRITE !
        IF +Y'>0 GOTO REDDONE

        NEW IEN SET IEN=""
        FOR  SET IEN=$ORDER(SelArray(IEN)) QUIT:(IEN="")  do
        . SET NumProcessed=NumProcessed+1
        . NEW TMGFDA,TMGMSG
        . SET TMGFDA(22705.5,IEN_",",1)=+Y
        . DO FILE^DIE("","TMGFDA","TMGMSG")
        . IF $DATA(TMGMSG("DIERR"))>0 do
        . . DO SHOWDIER^TMGDEBU2(.TMGMSG)
        . . SET numErrors=numErrors+1

REDDONE
        WRITE !,NumProcessed," tickler message file entries processed.",!
        IF NumProcessed>0 WRITE numErrors," errors encountered.",!
        WRITE "Goodbye",!
        QUIT

FINDLOST ;
        ;"Purpose: perform *exhaustive* search of EVERY TIU document, and see
        ;"         IF there is any tickler message that was dropped.
        WRITE "==============================================",!
        WRITE "Scanner for lost Tickler messages",!
        WRITE "..............................................",!
        WRITE "Will scan every TIU DOCUMENT for Tickler Tag that is",!
        WRITE "not linked to a record in file 22705.5.",!
        WRITE "Press ESC (perhaps multiple times) to escape loop.",!
        WRITE "==============================================",!,!
        NEW %
        IF $DATA(^TMG("TMP","TICKLER","LOST")) DO  GOTO:(%=-1) FLDN
        . SET %=2
        . WRITE "KILL prior search records"
        . DO YN^DICN WRITE !
        . IF %=-1 QUIT
        . IF %=1 KILL ^TMG("TMP","TICKLER","LOST")
        NEW STIME SET STIME=$H
        NEW DONE SET DONE=0
        NEW TIUMAX SET TIUMAX=+$ORDER(^TIU(8925,"!"),-1)
        NEW TIUIEN SET TIUIEN=0
        FOR  SET TIUIEN=$ORDER(^TIU(8925,TIUIEN)) QUIT:(+TIUIEN'>0)!DONE  DO
        . IF TIUIEN#50=0 DO
        . . DO PROGBAR^TMGUSRI2(TIUIEN,"PROGRESS: "_TIUIEN,0,TIUMAX,60,STIME)
        . . IF $$USRABORT^TMGUSRI2("looking for lost Tickler messages") SET DONE=1
        . IF $DATA(^TMG(22705.5,"C",TIUIEN)) QUIT  ;"//already linked to tickler message
        . NEW DATESTR,USERIEN
        . IF $$HasTickler^TMGTICKL(TIUIEN,.DATESTR,.USERIEN)=0 QUIT
        . SET ^TMG("TMP","TICKLER","LOST",TIUIEN)=$GET(DATESTR)_"^"_$GET(USERIEN)
        DO PROGBAR^TMGUSRI2(TIUIEN,"COMPLETE",TIUMAX,TIUMAX,60,STIME)
        WRITE !,"Results:",!
FL2     IF $DATA(^TMG("TMP","TICKLER","LOST")) DO
        . DO ZWRITE^TMGZWR($NAME(^TMG("TMP","TICKLER","LOST")))
        . NEW % SET %=1
        . WRITE "Recover these lost ticklers" DO YN^DICN WRITE !
        . IF %'=1 QUIT
        . SET TIUIEN=0
        . FOR  SET TIUIEN=$ORDER(^TMG("TMP","TICKLER","LOST",TIUIEN)) QUIT:(+TIUIEN'>0)  DO
        . . NEW TMGDFN SET TMGDFN=+$PIECE($GET(^TIU(8925,TIUIEN,0)),"^",2)
        . . IF TMGDFN=0 DO  QUIT
        . . . WRITE !,"Unable to find a patient for TIU DOCUMENT #",TIUIEN,!
        . . NEW INFO SET INFO=$GET(^TMG("TMP","TICKLER","LOST",TIUIEN))
        . . NEW DUEDATE,DATESTR SET DATESTR=$PIECE(INFO,"^",1)
        . . IF DATESTR'=""  DO
        . . . NEW %DT,X,Y
        . . . SET %DT="T"
        . . . SET X=DATESTR DO ^%DT
        . . . IF Y=-1 SET DATESTR="" QUIT
        . . . SET DUEDATE=Y
        . . IF DATESTR="" DO
        . . . SET DUEDATE=$$NOW^XLFDT
        . . NEW USERIEN SET USERIEN=+$PIECE(INFO,"^",2)
        . . IF USERIEN'>0 SET USERIEN=DUZ
        . . NEW TMGFDA,TMGMSG,TMGIEN
        . . SET TMGFDA(22705.5,"+1,",.01)=TMGDFN
        . . SET TMGFDA(22705.5,"+1,",.05)=TIUIEN
        . . SET TMGFDA(22705.5,"+1,",2)="S"      ;"S=SIGNED
        . . SET TMGFDA(22705.5,"+1,",3)=USERIEN
        . . SET TMGFDA(22705.5,"+1,",1)=DUEDATE
        . . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
        . . IF $DATA(TMGMSG("DIERR"))=0 DO  QUIT  ;"no errors, so we are done here...
        . . . KILL ^TMG("TMP","TICKLER","LOST",TIUIEN)
        . . NEW ERRSTR SET ERRSTR=$$GETERRST^TMGDEBU2(.TMGMSG)
        . . WRITE !,ERRSTR,!
        ELSE  WRITE "None found.  Great!",!
        WRITE "Goodbye.",!
FLDN    QUIT