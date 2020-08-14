TMGDIS2 ;TMG/kst/Custom version of DIS2 ;03/25/06 ; 5/18/10 7:52am, 2/2/14
        ;;1.0;TMG-LIB;**1**;01/01/06
        ;"---- Prior header below ----------
        ;SFISC/GFT-SEARCH, TEMPLATES & COMPUTED FIELDS;4JUN2005
        ;;22.0;VA FileMan;**6,144**;Mar 30, 1999;Build 5
        ;
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
DIS2    ;
        ;"Purpose:
        ;"Input:  ...
        ;"Output:  TMGRESULT is set
        ;"Results: none
        KILL DISV
        ;"GOTO G:'DUZ
0       IF 1=0 D
        . N DIS,DIS0,DA,DC,DE,DJ,DL
        . D S3^DIBT1 Q
        IF 1=0 KILL DIRUT,DIROUT
        IF 1=0 IF $D(DTOUT)!($D(DUOUT)) GOTO Q
        ;"Get SORT TEMPLATE to store search into.
        IF 1=1 SET Y=$$PREPTMPL()
        IF +TMGRESULT=-1 GOTO TMGDONE  ;"Quit from there
        SET TMGSORTT=Y
        IF X="",'$D(DIAR) GOTO G
        IF Y<0,X=U GOTO TMGDONE  ;"WAS Q
        ;"IF Y<0 GOTO 0
        IF $D(DIARU),DIARU-Y=0 DO  GOTO TMGDONE  ;WAS 0
        . ;"WRITE $C(7),!,"Archivers must not store results in the default template"
        . SET TMGRESULT="-1^""Archivers must not store results in the default template"
        SET (DIARI,DISV)=+Y
        SET A=$D(^DIBT(DISV,"DL"))
        IF $D(DIS0)#2 SET ^("DL")=DIS0
        IF $D(DA)#2 SET ^("DA")=DA
        IF $D(DJ)#2 SET ^("DJ")=DJ
        IF $D(DIAR),'$D(DIARU) SET $P(^DIAR(1.11,DIARC,0),U,3)=DISV
        SET Z=-1
        SET DIS0="^DIBT(+Y,"
        FOR P="DIS","DA","DC","DE","DJ","DL" DO
        . SET %Y=DIS0_""""_P_""","
        . SET %X=P_"("
        . DO %XY^%RCR
        SET %X="^UTILITY($J,",%Y="^DIBT(DISV,""O"","
        SET @(%X_"0)=U")
        DO %XY^%RCR
G       NEW DISTXT
        SET %X="^UTILITY($J,"
        SET %Y="DISTXT("
        DO %XY^%RCR
        ;"WRITE !
        SET Y=DI
        DO Q
        SET DIC=Y
        ;Just QUIT.  Important screening code stored in SORT TEMPLATE in 'DIS' node
        GOTO TMGDONE  ;"//kt added
        ;
        IF $D(SF)!$D(L)&'$D(DIAR) GOTO EN1^DIP
        GOTO EN^DIP
        ;
        ;"==========================================
TEM     GOTO TEM^TMGDIS  ;"-- MOVED TO TMGDIS
COMP    GOTO COMP^TMGDIS  ;"-- MOVED TO TMGDIS
XA      GOTO XA^TMGDIS    ;"-- MOVED TO TMGDIS
COLON   GOTO COLON^TMGDIS ;"-- MOVED TO TMGDIS
Q       GOTO Q^TMGDIS     ;"-- MOVED TO TMGDIS
        ;"==========================================
        ;
 ;"X       KILL O(DC)
 ;"        GOTO X^TMGDIS
        ;
DIS     ;PUT SET LOGIC INTO DIS FOR SUBFILE
        SET %X=""
        FOR %Y=1:1 SET %X=$O(DIS(%X)) Q:'%X  DO
        . SET %=$S($D(DIAR(DIARF,%X)):DIAR(DIARF,%X),1:DIS(%X))
        . IF %["X DIS(" SET %=$P(%,"X DIS(")_"X DIFG("_DIARF_","_$P(%,"X DIS(",2)
        . SET ^DIAR(1.11,DIARC,"S",%Y,0)=%X
        . SET ^(1)=%
        IF %Y>1 DO
        . SET %Y=%Y-1
        . SET ^DIAR(1.11,DIARC,"S",0)="^1.1132^"_%Y_U_%Y
        GOTO DIS2 ;"QUIT will occur there.
        ;
PREPTMPL() ;
        ;"//kt added
        ;"Purpose: Return IEN of a SORT TEMPLATE ready for use.
        ;"Returns -1 IF problem, or IEN^NAME.  ALSO, X is SET to NAME (or "" IF unsuccessful)
        ;"Get SORT TEMPLATE to store search into.
        NEW TMGTMPL SET TMGTMPL=-1
        NEW Y SET Y=+$GET(INFO("SORT IEN"))
        IF (Y'>0)!($DATA(^DIBT(Y))=0) DO  ;"Get a NEW record
        . NEW DIC,X
        . SET DIC=.401,DIC(0)="L"
        . SET X="TMG SRCH "_$J
        . DO ^DIC ;"Create now, or get pre-existing
        . IF +Y'>0 DO  QUIT
        . . SET TMGRESULT="-1^Error getting SORT TEMPLATE for use."
        IF +Y>0,$DATA(^DIBT(+Y)) DO  ;"Edit existing record
        . NEW TMGFDA,TMGMSG,TMGIEN,TMGIENS,DA,DIE
        . SET TMGTMPL=Y
        . NEW I SET I=0
        . ;"Kill all but zero node of record
        . FOR  SET I=$ORDER(^DIBT(+Y,I)) QUIT:I=""  KILL ^DIBT(+Y,I)
        . NEW % DO NOW^%DTC
        . SET DIE=.401
        . SET DA=+Y
        . SET DR="2///"_%_";3///"_DUZ(0)_";4///"_+TMGFILE_";5///"_DUZ_";6///"_DUZ(0)
        . DO ^DIE
        . ;"SET IENS=+Y_","
        . ;"SET TMGFDA(.401,IENS,2)=%
        . ;"SET TMGFDA(.401,IENS,3)=DUZ(0)
        . ;"SET TMGFDA(.401,IENS,4)=+TMGFILE
        . ;"SET TMGFDA(.401,IENS,5)=DUZ
        . ;"SET TMGFDA(.401,IENS,6)=DUZ(0)
        . ;"Set back NEW field data
        . ;"DO FILE^DIE("K","TMGFDA","TMGMSG")
        . ;"IF $DATA(TMGMSG("DIERROR")) DO  QUIT
        . ;". SET TMGRESULT="-1^Error editing SORT TEMPLATE: '"_$GET(TMGMSG("DIERR",1,"TEXT",1))_"'"
        . ;". SET Y=-1
        SET X=$PIECE(TMGTMPL,U,2)
        QUIT TMGTMPL
        ;
TMGDONE QUIT
        ;
DELTEMPL(TMGIEN) ;
        ;"Purpose: To delete the SORT TEMPLATE in TMGIEN
        ;"Input: TMGIEN -- the IEN in file .401 to be deleted
        ;"Results: 1 IF success, -1 IF failure
        NEW DIE,DA,DR
        SET DIE=.401  ;DIE="^DIBT("  ;"FILE .401
        SET DA=TMGIEN
        SET DR=".01///@"
        DO ^DIE
        QUIT ($DATA(DA)=0)
        ;
