TMGDELPT        ;TMG/kst/Low-level Patient Deletion Utilities ;08/13/08, 2/2/14
                ;;1.0;TMG-LIB;**1**;08/12/08
        
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;

DelPt
        ;"Purpose: to try to safely delete a patient
        ;"NOTICE: Deleting could be a violation of laws regulating the maintanance
        ;"        of patient records.  I.e. IF you delete who a patient is, then
        ;"        you destroy all records associated with that patient.
        ;"        SO, only delete a patient IF you are sure that you what you are
        ;"        doing, and are sure that you are not causing perminant injury!

        NEW TMGDFN,DIC,X,Y,PtIEN
Loop1
        SET DIC=2,DIC(0)="MAEQ"
        DO ^DIC WRITE !
        IF +Y=-1 DO  GOTO DPDone
        . WRITE "Goodbye.",!
        SET PtIEN=+Y

        do DelOne(PtIEN,0)

        WRITE "Delete another patient (caution!)" DO YN^DICN WRITE !
        IF %=1 GOTO Loop1
        QUIT


DelOne(PtIEN,Quiet)
        ;"Purpose: to try to safely delete a patient
        ;"NOTICE: Deleting could be a violation of laws regulating the maintanance
        ;"        of patient records.  I.e. IF you delete who a patient is, then
        ;"        you destroy all records associated with that patient.
        ;"        SO, only delete a patient IF you are sure that you what you are
        ;"        doing, and are sure that you are not causing perminant injury!
        ;"Input: PtIEN -- the IEN in file 2
        ;"       Quiet -- OPTIONAL.  If 1, then user is not prompted.
        ;
        set Quiet=+$GET(Quiet,0)
        NEW PtrsIn
        NEW File,Field,A,GL,Q,ERR
        SET Qt="""",ERR=0,File=0
        FOR  SET File=$O(^DD(2,0,"PT",File)) QUIT:'File  do
        . SET Field=0
        . FOR  S Field=$O(^DD(2,0,"PT",File,Field)) QUIT:'Field  do
        . . SET K=0
        . . FOR  S K=$O(^DD(File,+Field,1,K)) QUIT:'K  DO
        . . . SET A=$G(^DD(File,+Field,1,K,0))
        . . . IF '$L($P(A,U,2)) QUIT
        . . . IF $L($P(A,U,3)) QUIT
        . . . SET GL=$G(^DIC(+File,0,"GL"))
        . . . QUIT:'$L(GL)
        . . . SET GL=GL_Qt_$P(A,U,2)_Qt_","_PtIEN_")"
        . . . IF $D(@GL) D
        . . . . NEW IEN SET IEN=$O(@GL@(""))
        . . . . SET PtrsIn(File,IEN)=""

        SET File="",Field=""
        FOR  SET File=$ORDER(PtrsIn(File)) QUIT:(File="")  do
        . SET IEN=""
        . FOR  SET IEN=$ORDER(PtrsIn(File,IEN)) QUIT:(IEN="")  do
        . . IF Quiet'=1 do
        . . . WRITE "Entry #",IEN," in file ",$PIECE($GET(^DIC(File,0)),"^",1)," (",File,") "
        . . . WRITE "points to this patient.",!
        . . NEW % SET %=2
        . . IF Quiet=1 SET %=1
        . . ELSE  WRITE "Delete this entry" DO YN^DICN WRITE !
        . . IF %'=1 QUIT
        . . NEW TMGFDA,TMGMSG
        . . SET TMGFDA(File,IEN_",",.01)="@"
        . . DO FILE^DIE("","TMGFDA","TMGMSG")
        . . DO SHOWDIER^TMGDEBU2(.TMGMSG)

        NEW % SET %=2
        if Quiet=1 SET %=1
        ELSE  WRITE "Delete ",$PIECE(Y,"^",2) DO YN^DICN WRITE !
        IF %'=1 GOTO DPDone

        NEW TMGFDA,TMGMSG
        SET TMGFDA(2,PtIEN_",",.01)="@"
        DO FILE^DIE("","TMGFDA","TMGMSG")
        DO SHOWDIER^TMGDEBU2(.TMGMSG)

DPDone
        QUIT



TempDel
        ;"Purpose: to delete all TMG-* named patients (testing only...)

        NEW IEN,name
        set name=""
        FOR  SET name=$ORDER(^DPT("B",name)) QUIT:(name="")  do
        . IF 'name["TMG-" QUIT
        . SET IEN=$ORDER(^DPT("B",name,""))
        . WRITE "Deleting: ",name,"..."
        . DO DelOne(IEN,1)
        . WRITE !

        QUIT