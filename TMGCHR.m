TMGCHR ;TMG/kst/Custom version of CHRISTEN ;03/25/06, 2/2/14
         ;;1.0;TMG-LIB;**1**;11/01/04

 ;"CHRISTEN(INFO)   This library will provide optional NON-INTERACTIVE versions of standard code.

 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;

 ;"=============================================================================
 ;"Kevin Toppenberg, MD  11-04
 ;"
 ;"Purpose:
 ;"
 ;"This library will provide optional NON-INTERACTIVE versions of standard code.
 ;"
 ;"CHRISTEN(INFO)
 ;"
 ;"Dependancies:
 ;"  TMGQIO
 ;"=============================================================================

MUDCHR        ;ISC-SF/GMB-Christen Site ;04/17/2002  11:48
        ;;8.0;MailMan;;Jun 28, 2002
        ; Entry points used by MailMan options (not covered by DBIA):
        ; CHRISTEN      XMCHRIS - Edit MailMan Site Parameters
 ;"
 ;"K. Toppenberg's changes made November, 2004
 ;"
 ;"Input:
 ;"     Note: INFO variable is completely an OPTIONAL parameter.
 ;"                If not supplied, interactive mode used
 ;"        INFO("SILENT-OUTPUT") -- 1 = output is supressed.
 ;"        INFO("SILENT-INPUT") -- 1 = User-interactive input is supressed.
 ;"
 ;"        ** IF in SILENT-INPUT mode, THEN the following data should be supplied:
 ;"     ----------------------
 ;"        INFO("DOMAIN") -- Answer for 'DOMAIN' to edit-- should be an existing domain
 ;"        INFO("PARENT") -- Answer for 'PARENT' domain question
 ;"        INFO("TIMEZONE") -- Answer for 'TIME ZONE' question
 ;"        INFO("CONTINUE") -- Answer for "Are you sure you want to change the name of this facility"
 ;"Output:
 ;"        If in SILENT-OUTPUT mode, then output that would normally go to the screen, will be routed to this array
 ;"        NOTE: INFO SHOULD BE PASSED BY REFERENCE IF user wants this information passed back out.
 ;"        INFO("TEXT","LINES")=Number of output lines
 ;"        INFO("TEXT",1)= 1st output line
 ;"        INFO("TEXT",2)= 2nd output line, etc...
 ;
 ;
CHRISTEN(INFO)        ;Set up/Change MailMan Site Parameters
        ;
        NEW SILNTOUT SET SILNTOUT=$GET(INFO("SILENT-OUTPUT"),0) ;//kt
        NEW SILENTIN SET SILENTIN=$GET(INFO("SILENT-INPUT"),0) ;//KT
        KILL INFO("TEXT") ;//kt

        N XMREC,XMABORT
        S XMABORT=0
        S XMREC=$G(^XMB(1,1,0)) I '+XMREC,$O(^XMB(1,0)) G E
        I XMREC="" D
        . D INIT
        E  D
        . D CHANGE
        Q:XMABORT
        D PARENT
        D SCRIPT
        G Q
        ;
        ;
        ;======================================================================
INIT        ; Initial Christening
        N DIC,DIE,Y,DA,XMFDA
        ;
        S DIC=4.2
        IF SILENTIN=1 DO
        . S DIC(0)="EM"
        . SET X=$GET(INFO("DOMAIN"))
        ELSE  DO
        . S DIC(0)="AEMQ"
        D ^DIC
        I Y<1 S XMABORT=1 D E1 Q
        S XMFDA(4.3,"+1,",.01)=+Y
        D UPDATE^DIE("","XMFDA")
        K DIC,Y
        DO InputParent
        ;"if SILENTIN>0 DO     ;"Note: Fields 3=PARENT, 1=TIME ZONE
        ;". SET DR="3///"_$GET(INFO("PARENT"),"FORUM.VA.GOV")   ;"3 '/'s means force the data in
        ;". SET DR=DR_";1///"_$GET(INFO("PARENT"),"EST")
        ;"else  do
        ;". S DR="3//FORUM.VA.GOV;1//EST"    ;"2 '/'s means ask user, with default suggestion.
        ;"S DIE=4.3  ;"MAILMAN SITE PARAMETERS
        ;"S DA=1     ;"Record#/IEN = 1
        ;"D ^DIE     ;"Input selected data elements to a given record. (only for existing records)
        I $D(Y) S XMABORT=1 D E1
        Q
        ;
        ;
        ;=======================================================================
CHANGE        ;
        N XMSITE,DIE,DA,DR,DIC,X,Yi
        IF $D(^XMB("NETNAME")) SET XMSITE=^XMB("NETNAME")
        ELSE  IF $D(^XMB("NAME")) SET XMSITE=^XMB("NAME")
        ELSE  IF $D(^DIC(4.2,+XMREC,0)) SET XMSITE=$P(^(0),U)
        ELSE  SET XMSITE=XMREC
        I $$SURE(XMSITE)=0 S XMABORT=1 Q  ; Are you sure?
        S DIC=4.2
        IF SILENTIN=0 DO
        . S DIC(0)="AEMQ"
        . S DIC("B")=$S($D(^DIC(4.2,+XMREC,0)):$P(^(0),U),1:XMSITE)
        ELSE  DO
        . SET DIC(0)="EM"
        . SET DIC("B")=""
        . SET X=$GET(INFO("DOMAIN"))
        D ^DIC
        I Y=-1 S XMABORT=1 Q
        I XMSITE'=$P(Y,U,2) D
        . I +Y=^XMB("NUM") D
        . . ; The domain name in file 4.2 has been changed.
        . . ; The pointer to file 4.2 has stayed the same.
        . . ; The filer won't fire the xrefs, so we need to DO it manually
        . . S (^XMB("NETNAME"),^XMB("NAME"))=$P(Y,U,2)
        . E  D
        . . N XMFDA
        . . S XMFDA(4.3,"1,",.01)=+Y
        . . D FILE^DIE("","XMFDA")
        . DO OUTP^TMGQIO(SILNTOUT,"!","!","The domain name for this facility is now: ",^XMB("NETNAME"))
        E  D
        . DO OUTP^TMGQIO(SILNTOUT,"!","!","The domain name for this facility remains: ",^XMB("NETNAME"))
        K DIC,Y
        DO InputParent
        ;"S DR="3//FORUM.VA.GOV;1//EST"
        ;"S DIE=4.3,DA=1
        ;"D ^DIE
        Q
        ;
        ;
        ;=======================================================================
InputParent
        IF SILENTIN>0 DO     ;"Note: Fields 3=PARENT, 1=TIME ZONE
        . SET DR="3///"_$GET(INFO("PARENT"),"FORUM.VA.GOV")   ;"3 '/'s means force the data in
        . SET DR=DR_";1///"_$GET(INFO("PARENT"),"EST")
        ELSE  do
        . S DR="3//FORUM.VA.GOV;1//EST"    ;"2 '/'s means ask user, with default suggestion.
        S DIE=4.3  ;"MAILMAN SITE PARAMETERS
        S DA=1     ;"Record#/IEN = 1
        D ^DIE     ;"Input selected data elements to a given record. (only for existing records)
        QUIT
        ;
        ;
        ;=======================================================================
SURE(XMSITE)        ;  Function returns 1 IF sure; 0 IF not
        N DIR,X,Y
        N RESULT SET RESULT=0 ;        Default to not sure
        ;
        DO OUTP^TMGQIO(SILNTOUT,"!","!","         * * * *  WARNING  * * * *","!","!")
        DO OUTP^TMGQIO(SILNTOUT,"You are about to change the domain name of this facility","!")
        DO OUTP^TMGQIO(SILNTOUT,"in the MailMan Site Parameters file.","!")
        DO OUTP^TMGQIO(SILNTOUT,"Currently, this facility is named: ",XMSITE,"!","!")
        DO OUTP^TMGQIO(SILNTOUT,"You must be extremely sure before you proceed!","!")
        DO OUTP^TMGQIO(SILENTIN,"Are you sure you want to change the name of this facility? NO//")
        DO INP^TMGQIO(.X,SILENTIN,120,$GET(INFO("CONTINUE")))
        IF X="" SET X="NO"
        IF "Yy"[$E(X_"N") SET RESULT=1  ;Yes, I'm sure!
        Q RESULT
        ;
        ;
PARENT        ;
        N XMPARENT
        S XMPARENT=+$G(^XMB("PARENT"))
        I XMPARENT S XMPARENT=$S($D(^DIC(4.2,XMPARENT,0)):$P(^(0),U),1:0)
        I XMPARENT'=0 D
        . DO OUTP^TMGQIO(SILNTOUT,"!","!",XMPARENT," has been initialized as your 'parent' domain.")
        . DO OUTP^TMGQIO(SILNTOUT,"!","(Forum is usually the parent domain, unless this is a subordinate domain.)")
        . DO OUTP^TMGQIO(SILNTOUT,"!","!","You may edit the MailMan Site Parameter file to change your parent domain.")
        E  D
        . DO OUTP^TMGQIO(SILNTOUT,"!","!",$C(7),"*** YOUR PARENT DOMAIN HAS NOT BEEN INITIALIZED !!! ***")
        . DO OUTP^TMGQIO(SILNTOUT,"!","!","You MUST edit the MailMan Site Parameter file to ENTER your parent domain.")
        Q
        ;
        ;
SCRIPT        ;RESET AUSTIN SCRIPT
        ;G SCRIPT^XMYPDOM
        DO OUTP^TMGQIO(SILNTOUT,"!","!","We will not initialize your transmission scripts.")
        Q
        ;
        ;
        ;=======================================================================
Q        DO OUTP^TMGQIO(SILNTOUT,"!","!","Use the 'Subroutine editor' option under network management menu to add your")
        DO OUTP^TMGQIO(SILNTOUT,"!","site passwords to the MINIENGINE script, and the 'Edit a script' option")
        DO OUTP^TMGQIO(SILNTOUT,"!","to edit any domain scripts that you choose to.")
        ;D ^XMYPDOM
        Q
        ;
        ;
        ;======================================================================
PMB        S Y=Y+1000
        S ^XMB(3.7,.5,2,+Y,1,0)=^TMP("XM",I,1,0)
        S ^XMB(3.7,.5,2,"B",$E($P(Y(0),U,1),1,30),+Y)=""
        S ^XMB(3.7,.5,2,+Y,0)=$P(Y(0),U)
        F J=0:0 DO  Q:J'>0
        . S J=$O(^TMP("XM",I,1,J))
        . Q:J'>0
        . S ^XMB(3.7,.5,2,+Y,1,J,0)=J
        . W "."
        Q
        ;
        ;
E        DO OUTP^TMGQIO(SILNTOUT,$C(7),"!","!")
        DO OUTP^TMGQIO(SILNTOUT,"There is a FILE INTEGRITY problem in your MailMan Site Parameters file","!")
        DO OUTP^TMGQIO(SILNTOUT,"There should only be one entry and that entry should be entry number 1.","!")
E1        DO OUTP^TMGQIO(SILNTOUT,$C(7),"!")
        DO OUTP^TMGQIO(SILNTOUT,"Your MailMan site parameters MUST be reviewed.","!")
EQ        DO OUTP^TMGQIO(SILNTOUT,"Then you can finish the INIT by executing POST^XMYPOST.","!","!")
        Q
E2        DO OUTP^TMGQIO(SILNTOUT,$C(7),"!","You DO not yet have an entry in your MailMan Site Parameters File","!")
        DO OUTP^TMGQIO(SILNTOUT,"Use FileMan to make an entry.","!")
        G EQ


