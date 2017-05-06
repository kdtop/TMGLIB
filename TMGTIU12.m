TMGTIU12 ;TMG/kst-TMG TIU NOTE COMPONENT UTILS ; 6/23/15
         ;;1.0;TMG-LIB;**1,17**;6/23/15
 ;
 ;"Kevin Toppenberg MD
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;"PUBLIC FUNCTIONS
 ;"=======================================================================
 ;"
 ;"UNSIGN(PARENTIEN8925)  ;"UNSIGN A NOTE/COMPONENT CLUSTER
 ;"RETRACT(PARENTIEN8925)  ;"REDACT A NOTE/COMPONENT CLUSTER
 ;"ENTRY  ;"Interactive point for changing a note's status
 ;"
 ;"=======================================================================
 ;"PRIVATE FUNCTIONS
 ;"=======================================================================
 ;" 
 ;"CHILDLST(PARENTIEN8925,OUT)  ;"GET LIST OF CHILDREN (RECURSIVE)
 ;"CHGSTAT(PARENTIEN8925,STATUS) ;"Change status of notes and children notes
 ;"GETSTAT(NAME)  ;"RETURNS THE IEN OF THE PROVIDED STATUS
 ;"
 ;"=======================================================================
 ;"Dependancies : TMGDEBUG 
 ;"=======================================================================
 ;
CHILDLST(PARENTIEN8925,OUT)  ;"GET LIST OF CHILDREN (RECURSIVE)
 ;"Input: PARENTIEN8925 - IEN of the parent note
 ;"       OUT(IEN8925)="" - # Will be the IEN of the child note
 NEW IEN SET IEN=0
 FOR  SET IEN=$ORDER(^TIU(8925,"DAD",PARENTIEN8925,IEN)) QUIT:IEN'>0  DO
 . DO CHILDLST(IEN,.OUT)
 . SET OUT(IEN)=""
 QUIT
 ;
UNSIGN(PARENTIEN8925)  ;"UNSIGN A NOTE/COMPONENT CLUSTER
 ;"Input: PARENTIEN8925 - IEN of the parent note
 ;"NEW RESPONSE
 ;"READ "This will unsign this note and all its children. Are you sure?(Y to continue)",RESPONSE
 ;"SET RESPONSE=$$UP^XLFSTR(RESPONSE)
 ;"IF RESPONSE="Y" 
 DO CHGSTAT(PARENTIEN8925,"UNSIGNED")
 ;"ELSE  WRITE "Note is unchanged.",!
 QUIT
 ;
RETRACT(PARENTIEN8925)  ;"REDACT A NOTE/COMPONENT CLUSTER
 ;Input: PARENTIEN8925 - IEN of the parent note
 ;"NEW RESPONSE
 ;"READ "This will permanently remove this note and all its children. Are you sure?(Y to continue)",RESPONSE
 ;"SET RESPONSE=$$UP^XLFSTR(RESPONSE)
 ;"IF RESPONSE="Y" DO 
 DO CHGSTAT(PARENTIEN8925,"RETRACTED") 
 ;"ELSE  WRITE "Note is unchanged.",!
 QUIT
 ;
ENTRY  ;"Interactive point for changing a note's status
 NEW PARENTIEN,DIC,Y,STATUSNAME
 SET DIC=8925
 SET DIC(0)="MAEQ"
 SET DIC("A")="Pick note to change status of: "
 SET DIC("S")="N P2 S P2=+$G(^TIU(8925,Y,0)) I $P($G(^TIU(8925.1,P2,0)),""^"",4)'=""CO"""
 DO ^DIC
 SET PARENTIEN=+$PIECE(Y,"^",1) 
 WRITE !
 IF PARENTIEN'>0 QUIT
 KILL DIC,Y
 SET DIC=8925.6
 SET DIC(0)="MAEQ"
 SET DIC("A")="Pick a new status: "
 SET DIC("S")="N NM S NM=$P($G(^TIU(8925.6,Y,0)),""^"",1) I (NM=""RETRACTED"")!(NM=""UNSIGNED"")"
 DO ^DIC
 SET STATUSNAME=$PIECE(Y,"^",2)
 IF STATUSNAME="COMPLETED" WRITE !,"I'm sorry. You cannot sign a note with this method. Please use CPRS" QUIT
 IF STATUSNAME="" QUIT
 DO CHGSTAT(PARENTIEN,STATUSNAME) 
 ;"NEW TMGMENU,RESPONSE
 ;"SET TMGMENU(0)="What would you like to do with this note?"
 ;"SET TMGMENU(1)="Unsign it and all its children"_$CHAR(9)_"Unsign"
 ;"SET TMGMENU(2)="Retract it and all its children"_$CHAR(9)_"Retract"
 ;"SET RESPONSE=$$MENU^TMGUSRI2(.TMGMENU,"^")
 ;"IF RESPONSE="Unsign" DO UNSIGN(.PARENTIEN)
 ;"IF RESPONSE="Retract" DO RETRACT(.PARENTIEN)
 QUIT
 ;"
CHGSTAT(PARENTIEN8925,STATUS) ;"Change status of notes and children notes
 ;"Input: PARENTIEN8925 - IEN of the parent note
 ;"       STATUS - The name of the status to set all notes to
 NEW ARRAY,IEN
 NEW STATUSIEN
 SET STATUSIEN=$$GETSTAT(STATUS)
 IF STATUSIEN'>0 WRITE "ERROR GETTING SIGNED IEN IN FILE 8925.6. CANNOT CONTINUE" QUIT
 NEW RESPONSE
 ;"DO YN^DICN
 WRITE !,"This will change this note and all its children.",!
 NEW % SET %=2 WRITE "Are you sure?(Y to continue)" DO YN^DICN WRITE !
 ;"SET RESPONSE=$$UP^XLFSTR(RESPONSE)
 ;"IF RESPONSE'="Y" WRITE !,"Note isn't changed." QUIT
 IF %'=1 WRITE !,"Note isn't changed." QUIT
 SET ARRAY(PARENTIEN8925)=""
 DO CHILDLST(PARENTIEN8925,.ARRAY)
 SET IEN=0
 WRITE !
 FOR  SET IEN=$ORDER(ARRAY(IEN)) QUIT:IEN'>0  DO
 . NEW TMGFDA SET TMGFDA(8925,IEN_",",.05)="`"_STATUSIEN
 . IF STATUS="UNSIGNED" DO
 . . SET TMGFDA(8925,IEN_",",1501)="@"
 . . SET TMGFDA(8925,IEN_",",1502)="@"
 . . SET TMGFDA(8925,IEN_",",1503)="@"
 . . SET TMGFDA(8925,IEN_",",1505)="@"
 . WRITE " - SETTING ",IEN," TO ",STATUS,!
 . NEW TMGMSG
 . DO FILE^DIE("E","TMGFDA","TMGMSG")
 . DO SHOWDIER^TMGDEBU2(.TMGMSG)
 WRITE "DONE",!
 QUIT
 ;"
GETSTAT(NAME)  ;"RETURNS THE IEN OF THE PROVIDED STATUS
 QUIT $ORDER(^TIU(8925.6,"B",NAME,0))
 ;"

 
