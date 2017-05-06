TMGVPE   ;TMG/kst/Simple VPE launcher ;03/25/06, 2/2/14
         ;;1.0;TMG-LIB;**1**;09/21/04
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 X ^%ZVEMS
 QUIT
 ;"--------------------------
 ;"Below is an decompiled version of ^%ZVEMS("E") -- for VPE editor
STARTVED(TAG,ROUTINE,EDITONLY)   ;
        ;"Purpose: this is a wrapper for launching VPE editor
        ;"Input: TAG -- the tag to start edit at. (Optional)
        ;"       ROUTINE -- routine (.e.g. the *.m file) to edit.  e.g. 'ORWCV'
        ;"       EDITONLY- Optional.  Default is 0.  If 1 then no saving allowed.
        QUIT:$G(DUZ)=""
        NEW FLAGSAVE,FLAGVPE,VEES
        NEW:$G(VEE("OS"))']"" VEE
        NEW %1,%2
        SET %2=$GET(TAG),%1=$GET(ROUTINE)
        DO VED4
        QUIT:'$D(^TMP("VEE","VRR",$J))
        IF $GET(EDITONLY)'=1 DO VED1
        KILL ^UTILITY($J)
        L  ;"unlock all
        QUIT
        ;
VED1    NEW %Y,VRRPGM,X
        D SAVE^%ZVEMRC(1)
        QUIT:$G(VRRPGM)']""
        DO VED2
        QUIT
        ;
VED2    NEW X
        SET X=VRRPGM
        X VEES("ZS")
        DO VED5
        QUIT
        ;
VED3    QUIT:$G(DUZ)>0
        SET ^TMP("VEE",$J,1)=$G(%1)
        SET ^(2)=$G(%2)
        DO ID^%ZVEMKU
        IF $G(VEESHL)="RUN" DO
        . SET %1=^TMP("VEE",$J,1)
        . SET %2=^(2)
        KILL ^TMP("VEE",$J)
        QUIT
        ;
VED4    SET $P(FLAGVPE,"^",4)="EDIT"
        DO PARAM^%ZVEMR($G(%1),$G(%2))
        QUIT
        ;
VED5    QUIT:VEE("OS")'=17&(VEE("OS")'=19)
        NEW LINK,PGM
        SET PGM=VRRPGM
        SET PGM=$TR(PGM,"%","_")
        SET LINK="ZLINK """_PGM_""""
        X LINK
        QUIT

