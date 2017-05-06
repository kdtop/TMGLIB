TMGOP1 ;TMG/kst/OPTION utility library ;5/14/12
         ;;1.0;TMG-LIB;**1**;5/14/12
 ;
 ;"TMG RPC UTILITY FUNCTIONS
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
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"ASKBROPT ; --  Ask and then browse OPTION information
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"BROWSOPT(IEN) -- display programming information about an RPC entry
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"      TMGUSRIF
 ;"      TMGDEBUG
 ;"=======================================================================
 ;"=======================================================================
 ;

ASKBROPT ;
        ;"Purpose: Ask and then browse OPTION information
ABR1    WRITE #,!,"-----OPTION EXPLORER-----",!,!
        WRITE "Pick option to explore:",!
        NEW DIC,X,Y
        SET DIC=19,DIC(0)="MAEQ"
        DO ^DIC WRITE !
        IF +Y>0 DO BROWSOPT(+Y) GOTO ABR1
        QUIT
        ;
BROWSOPT(IEN) ;BROWSE OPTION
        ;"Purpose: to display information about an OPTION entry
        ;"Input: IEN -- record# in OPTION (19)to display
        ;"Results: none
        WRITE #,!,"-----OPTION EXPLORER-----",!,!
        SET IEN=+$GET(IEN) IF IEN'>0 GOTO BRWDN
        NEW MENU,USRPICK,I
        NEW ZN SET ZN=$GET(^DIC(19,IEN,0))
        NEW OPTNAME SET OPTNAME=$PIECE(ZN,"^",1)
        NEW OPTTEXT SET OPTTEXT=$PIECE(ZN,"^",2)
        NEW TYPE SET TYPE=$PIECE(ZN,"^",4)
        NEW TYPENAME SET TYPENAME=$$GET1^DIQ(19,IEN,4)

BRRPC1  KILL MENU
        SET MENU(0)="OPTION: "_OPTTEXT_" ("_OPTNAME_", `"_IEN_")"
        SET I=1
        SET MENU(I)="Dump this OPTION record:"_OPTNAME_" ("_OPTNAME_")"_$CHAR(9)_"DUMP",I=I+1
        NEW PARENT SET PARENT=0
        FOR  SET PARENT=+$ORDER(^DIC(19,"AD",IEN,PARENT)) QUIT:(PARENT'>0)  DO
        . SET MENU(I)="<-- PARENT: "_$$GET1^DIQ(19,PARENT,1)_" ("_$$GET1^DIQ(19,PARENT,.01)_")"_$CHAR(9)_PARENT
        . SET I=I+1
        NEW CHILD SET CHILD=0
        IF TYPE="M" DO
        . NEW INFO
        . NEW CHILDIEN SET CHILDIEN=0
        . FOR  SET CHILDIEN=$ORDER(^DIC(19,IEN,10,CHILDIEN)) QUIT:(+CHILDIEN'>0)  DO
        . . NEW ZN SET ZN=$GET(^DIC(19,IEN,10,CHILDIEN,0))
        . . NEW OPTIEN SET OPTIEN=$PIECE(ZN,"^",1)
        . . NEW SHORT SET SHORT=$PIECE(ZN,"^",2)
        . . NEW MENUTEXT SET MENUTEXT=$$GET1^DIQ(19,OPTIEN,1)
        . . NEW MENUNAME SET MENUNAME=$$GET1^DIQ(19,OPTIEN,.01)
        . . NEW S SET S="View child --> "
        . . IF SHORT'="" SET S=S_SHORT_" - "
        . . SET S=S_MENUTEXT_" ("_MENUNAME_")"_$CHAR(9)_OPTIEN
        . . IF SHORT'="" SET INFO(SHORT)=S
        . . ELSE  IF MENUTEXT'="" SET INFO(MENUTEXT)=S
        . . ELSE  SET INFO(MENUNAME)=S
        . NEW ENTRY SET ENTRY=""
        . FOR  SET ENTRY=$ORDER(INFO(ENTRY)) QUIT:ENTRY=""  DO
        . . SET MENU(I)=$GET(INFO(ENTRY)) SET I=I+1
        WRITE #
        SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
        IF USRPICK="^" GOTO BRWDN
        ELSE  IF USRPICK="DUMP" DO
        . WRITE !,!
        . DO DUMPREC^TMGDEBU3(19,IEN_",")
        . DO PRESS2GO^TMGUSRI2
        ELSE  IF +USRPICK=USRPICK DO
        . DO BROWSOPT(USRPICK)
        GOTO BRRPC1
BRWDN   QUIT
        ;
