TMGTIUP2 ;TMG/kst-TMG TIU NOTE PARSING FUNCTIONS ; 4/11/17
         ;;1.0;TMG-LIB;**1,17**;4/11/17
 ;
 ;"Eddie Hagood
 ;"Kevin Toppenberg MD
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 4/11/17  Kevin S. Toppenberg MD
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
 ;"=======================================================================
 ;"PRIVATE FUNCTIONS
 ;"=======================================================================
 ;"
 ;"=======================================================================
 ;"Dependancies :  
 ;"=======================================================================
LASTTIU(DFN,SRCHTEXT)  ;
        ;"FIND LAST NOTE WITH HPI SECTION
        NEW TIUIEN SET TIUIEN=9999999
        NEW TIULASTOV SET TIULASTOV=0
        FOR  SET TIUIEN=$ORDER(^TIU(8925,"C",DFN,TIUIEN),-1) QUIT:(TIUIEN'>0)!(TIULASTOV'=0)  DO
        . NEW IDX SET IDX=0
        . IF $P($G(^TIU(8925,TIUIEN,0)),"^",5)'=7 QUIT ;"ONLY USE SIGNED DOCS
        . NEW TEXT SET TEXT=""
        . FOR  SET IDX=$ORDER(^TIU(8925,TIUIEN,"TEXT",IDX)) QUIT:IDX'>0  DO
        . . SET TEXT=TEXT_$GET(^TIU(8925,TIUIEN,"TEXT",IDX,0))
        . IF TEXT'["HISTORY OF PRESENT ILLNESS (HPI):" QUIT
        . SET TIULASTOV=TIUIEN
        QUIT TIULASTOV
        ; 
LASTHPI(DFN)  ;"Return the last HPI section, with processing, formatting etc.
        ;"FIND LAST NOTE WITH HPI SECTION
        NEW TMGHPI SET TMGHPI=""
        ;"NEW TIUIEN SET TIUIEN=9999999
        ;"NEW TIULASTOV SET TIULASTOV=0
        ;"FOR  SET TIUIEN=$ORDER(^TIU(8925,"C",DFN,TIUIEN),-1) QUIT:(TIUIEN'>0)!(TIULASTOV'=0)  DO
        ;". NEW IDX SET IDX=0
        ;". IF $P($G(^TIU(8925,TIUIEN,0)),"^",5)'=7 QUIT ;"ONLY USE SIGNED DOCS
        ;". NEW TEXT SET TEXT=""
        ;". FOR  SET IDX=$ORDER(^TIU(8925,TIUIEN,"TEXT",IDX)) QUIT:IDX'>0  DO
        ;". . SET TEXT=TEXT_$GET(^TIU(8925,TIUIEN,"TEXT",IDX,0))
        ;". IF TEXT'["HISTORY OF PRESENT ILLNESS (HPI):" QUIT
        ;". SET TIULASTOV=TIUIEN
        SET TIULASTOV=$$LASTTIU(DFN,"HISTORY OF PRESENT ILLNESS (HPI):")
        IF TIULASTOV=0 GOTO LHDN
        ;"
        ;"RUN PROCESS NOTE HERE
        NEW IDX,TIUARRAY,PROCESSEDARR SET IDX=0
        SET TIUARRAY("DFN")=DFN
        FOR  SET IDX=$ORDER(^TIU(8925,TIULASTOV,"TEXT",IDX)) QUIT:IDX'>0  DO
        . SET TIUARRAY("TEXT",IDX)=$GET(^TIU(8925,TIULASTOV,"TEXT",IDX,0))
        DO PROCESS^TMGTIUP3(.PROCESSEDARR,.TIUARRAY,1) ;
        ;"
        ;"EXTRACT HPI
        SET IDX=0
        FOR  SET IDX=$ORDER(PROCESSEDARR(IDX)) QUIT:IDX'>0  DO
        . SET TMGHPI=TMGHPI_$GET(PROCESSEDARR(IDX))
        ;"
        ;"GET GROUPING
        NEW AUTOGROUPING SET AUTOGROUPING=0
        NEW NUMOFGROUPS SET NUMOFGROUPS=0
        IF TMGHPI["[GROUP AUTO " DO
        . SET NUMOFGROUPS=+$P($P(TMGHPI,"[GROUP AUTO ",2),"]",1)
        . IF NUMOFGROUPS>0 SET AUTOGROUPING=1
        ;"
        SET TMGHPI=$P(TMGHPI,"<B>HISTORY OF PRESENT ILLNESS (HPI):</B>",2)
        IF TMGHPI["<STRONG>PAST MEDICAL HISTORY (PMH)" DO
        . SET TMGHPI=$P(TMGHPI,"<STRONG>PAST MEDICAL HISTORY (PMH)",1)
        ELSE  DO  
        . SET TMGHPI=$P(TMGHPI,"<B>PAST MEDICAL HISTORY (PMH)",1)
        ;"
        ;"Remove unwanted tags from note
        DO RMTAGS^TMGHTM1(.TMGHPI,"=== HPI ISSUES BELOW WERE NOT ADDRESSED TODAY ===")
        DO RMTAGS^TMGHTM1(.TMGHPI,"--&nbsp;[FOLLOWUP&nbsp;ITEMS]&nbsp;---------")
        DO RMTAGS^TMGHTM1(.TMGHPI,"-- [FOLLOWUP ITEMS] ---------")
        DO RPTAGS^TMGHTM1(.TMGHPI,"<LI>  <P>","<LI> ")
        DO RMTAGS^TMGHTM1(.TMGHPI,"<I>")
        DO RMTAGS^TMGHTM1(.TMGHPI,"<EM>")
        DO RMTAGS^TMGHTM1(.TMGHPI,"</I>")
        DO RMTAGS^TMGHTM1(.TMGHPI,"</EM>")
        ;"
        ;"Parse Items
        NEW SECTION,ITEMARRAY SET IDX=1
        NEW DELIMITER SET DELIMITER=$$NEXT(TMGHPI,"*","<LI>")
        ;"ORIGINAL LINE -> SET TMGHPI=$P(TMGHPI,"<LI>",2,999)
        SET TMGHPI=$P(TMGHPI,DELIMITER,2,999)
        NEW PREVFOUND SET PREVFOUND=0
        NEW GROUPING SET GROUPING=0
PARSE   IF (TMGHPI["<LI>")!(TMGHPI["*") DO
        . ;"SET TMGHPI=$P(TMGHPI,"<LI>",2,999)
        . NEW SECTION
        . SET DELIMITER=$$NEXT(TMGHPI,"<LI>","*")
        . ;"ORIGINAL LINE -> SET SECTION=$P(TMGHPI,"<LI>",1),TMGHPI=$P(TMGHPI,"<LI>",2,999)
        . SET SECTION=$P(TMGHPI,DELIMITER,1),TMGHPI=$P(TMGHPI,DELIMITER,2,999)
        . SET SECTION=$$TRIM^XLFSTR(SECTION)
        . DO RMTAGS^TMGHTM1(.SECTION,"</LI>")
        . WRITE SECTION,!,!
        . IF (SECTION="")!(SECTION="<P>")!(SECTION="<BR>")!(SECTION="<BR><BR>")!(SECTION="<BR></P>") DO
        . . ;Skip section
        . ELSE  DO 
        . . SET ^TMG("EDDIE","SECTION",IDX,"BEFORE")=SECTION
        . . SET SECTION=$$HTMLTRIM^TMGHTM1(SECTION,"LR")
        . . SET SECTION=$$BRKTRIM(SECTION)
        . . SET SECTION=$$ITALICS(SECTION)
        . . SET ^TMG("EDDIE","SECTION",IDX,"TRIMMED")=SECTION
        . . SET ITEMARRAY(IDX)=SECTION
        . . SET IDX=IDX+1
        . . SET ^TMP("EDDIE","SECTION",IDX)=SECTION
        . . IF $$UP^XLFSTR(SECTION)["PREVENT" SET PREVFOUND=1
        . . IF $$UP^XLFSTR(SECTION)["[GROUP" SET GROUPING=1
        . . IF $$UP^XLFSTR(SECTION)["(GROUP" SET GROUPING=1
        IF (TMGHPI["<LI>")!(TMGHPI["*") GOTO PARSE
        ;"WRITE "DONE WITH PARSING",!,!,!,!
        ;"WRITE TMGHPI,!,!
        SET DELIMITER=$$NEXT(TMGHPI,"</LI>","*")
        IF TMGHPI'="" DO
        . SET TMGHPI=$$HTMLTRIM^TMGHTM1(TMGHPI,"LR")
        . SET TMGHPI=$$BRKTRIM(TMGHPI)
        . SET ITEMARRAY(IDX)=$P(TMGHPI,DELIMITER,1)
        . SET ITEMARRAY(IDX)=$$ITALICS($G(ITEMARRAY(IDX)))
        . SET IDX=IDX+1
        . IF $$UP^XLFSTR(TMGHPI)["PREVENT" SET PREVFOUND=1
        IF PREVFOUND=0 DO  ;"if prevention section not found, add blank one
        . SET ITEMARRAY(IDX)="<U>Prevention</U>: (data needed)"
        . SET IDX=IDX+1
        ;"IF DELIMITER="*" SET TMGHPI="*"_TMGHPI
        KILL TMGHPI
        ;"
        NEW WARNING SET WARNING=0
        NEW TOPICS SET TOPICS=IDX-1
        IF TOPICS>10 SET WARNING=1
        ;"
        ;"Reassemble ordered list, removing undesired sections
        IF GROUPING=1 SET AUTOGROUPING=0  ;"IF ALREADY GROUPING, DON'T ATTEMPT TO AUTOGROUP
        SET IDX=0
        NEW BULLETS SET BULLETS=1
        IF +$GET(DUZ)=83 SET BULLETS=0
        IF (WARNING=1)&(GROUPING=0)&(+$GET(DUZ)'=83) DO
        . SET TMGHPI="{HTML:<B><FONT style=""BACKGROUND-COLOR:#ff0000"">}CONSIDER GROUPING. PATIENT HAS "_TOPICS_" TOPICS.{HTML:</B></FONT>}"
        ELSE  DO
        . SET TMGHPI=""
        IF BULLETS=1 DO
        . SET TMGHPI=TMGHPI_"<ul>"
        ELSE  DO
        . SET TMGHPI=TMGHPI_""
        FOR  SET IDX=$ORDER(ITEMARRAY(IDX)) QUIT:IDX'>0  DO
        . IF $GET(ITEMARRAY(IDX))["<U>ALLERGIES</U>" QUIT
        . IF BULLETS=1 DO
        . . IF AUTOGROUPING=1 DO
        . . . NEW BRKTAG SET BRKTAG=":"
        . . . IF $G(ITEMARRAY(IDX))["</U>:" SET BRKTAG="</U>:"
        . . . IF $G(ITEMARRAY(IDX))["</U> :" SET BRKTAG="</U> :"
        . . . SET TMGHPI=TMGHPI_"<LI>"_$$TRIM^XLFSTR($P($GET(ITEMARRAY(IDX)),BRKTAG,1))_BRKTAG_$$GROUP(IDX,TOPICS,NUMOFGROUPS)_$$TRIM^XLFSTR($P($GET(ITEMARRAY(IDX)),BRKTAG,2))_"</LI>"
        . . ELSE  DO
        . . . SET TMGHPI=TMGHPI_"<LI>"_$GET(ITEMARRAY(IDX))_"</LI>"
        . ELSE  DO
        . . SET TMGHPI=TMGHPI_"     * "_$GET(ITEMARRAY(IDX))_"<P>"
        IF BULLETS=1 SET TMGHPI=TMGHPI_"</UL>"
LHDN    QUIT TMGHPI
        ;
NEXT(STR,FIRST,SECOND) ;"
        ;"Purpose: Check string to determine which delimiter comes first and
        ;"         Return it
        NEW TMGRESULT
        NEW POS1 SET POS1=$F(STR,FIRST)
        NEW POS2 SET POS2=$F(STR,SECOND)
        IF (POS1=0)&(POS2>0) SET TMGRESULT=SECOND GOTO NXDN
        IF (POS2=0)&(POS1>0) SET TMGRESULT=FIRST GOTO NXDN
        IF POS1<POS2 DO
        . SET TMGRESULT=FIRST
        ELSE  DO
        . SET TMGRESULT=SECOND
NXDN    QUIT TMGRESULT
        ;
BRKTRIM(STR)  ;" 
        NEW TRIMMING 
TRIM    SET STR=$$TRIM^XLFSTR(STR)
        SET TRIMMING=0
        IF $E(STR,$L(STR)-2,$L(STR))="<P>" DO
        . SET STR=$E(STR,1,$L(STR)-3)
        . SET TRIMMING=1
        IF $E(STR,$L(STR)-3,$L(STR))="<BR>" DO
        . SET STR=$E(STR,1,$L(STR)-4)
        . SET TRIMMING=1
        IF $E(STR,$L(STR)-3,$L(STR))="</P>" DO
        . SET STR=$E(STR,1,$L(STR)-4)
        . SET TRIMMING=1
        IF $E(STR,$L(STR)-2,$L(STR))="..." DO
        . SET STR=$E(STR,1,$L(STR)-3)
        . SET TRIMMING=1      
        IF $E(STR,$L(STR)-6,$L(STR))="</FONT>" DO
        . SET STR=$E(STR,1,$L(STR)-7)
        . SET TRIMMING=1
        IF TRIMMING=1 GOTO TRIM    
BTDN    QUIT STR
        ;"
ENDTEXT(STR)  ;"RETURN THE END OF THE TEXT (ACCOUNTING FOR SPACES AND TAGS)
        NEW TMGRESULT SET TMGRESULT=$L(STR)
        NEW TRIMMING
SRCH    SET STR=$$TRIM^XLFSTR(STR)
        SET TRIMMING=0
        IF $E(STR,$L(STR)-2,$L(STR))="<P>" DO
        . SET STR=$E(STR,1,$L(STR)-3)
        . SET TRIMMING=1
        IF $E(STR,$L(STR)-3,$L(STR))="<BR>" DO
        . SET STR=$E(STR,1,$L(STR)-4)
        . SET TRIMMING=1
        IF $E(STR,$L(STR)-3,$L(STR))="</P>" DO
        . SET STR=$E(STR,1,$L(STR)-4)
        . SET TRIMMING=1
        IF $E(STR,$L(STR)-6,$L(STR))="</FONT>" DO
        . SET STR=$E(STR,1,$L(STR)-7)
        . SET TRIMMING=1
        SET TMGRESULT=$L(STR)
        IF TRIMMING=1 GOTO SRCH
        QUIT TMGRESULT
        ;"
ITALICS(SECTION)    ;"This will remove the italics and add one single
        ;"set of italics for the entire section before any table is found
        ;"Remove existing italics tags
        ;"DO RMTAGS^TMGHTM1(.SECTION,"<I>")
        ;"DO RMTAGS^TMGHTM1(.SECTION,"</I>")
        ;"DO RMTAGS^TMGHTM1(.SECTION,"<EM>")
        ;"DO RMTAGS^TMGHTM1(.SECTION,"</EM>")
        ;"Add end italics tags
        IF (SECTION["-- [")!(SECTION["--&nbsp;[") DO
        . NEW POS1,POS2,LEN
        . IF SECTION["-- [" DO
        . . SET POS2=$F(SECTION,"-- [")
        . . SET POS2=POS2-5
        . ELSE  DO
        . . SET POS2=$F(SECTION,"--&nbsp;[")
        . . SET POS2=POS2-10
        . NEW P1,P2 
        . SET P1=$E(SECTION,0,POS2)
        . SET POS1=$$ENDTEXT(P1)
        . SET P1=$E(SECTION,0,POS1)
        . SET P2=$E(SECTION,POS1+1,$L(SECTION))
        . IF $E(P1,$L(P1)-2,$L(P1))="..." DO
        . . SET P1=$E(P1,1,$L(P1)-3)
        . SET SECTION=P1_"</I>..."_P2_"<BR>"
        ELSE  DO
        . SET SECTION=SECTION_"</I>..."
        ;"Add beginning tags
        NEW POS SET POS=$F(SECTION,">:")
        IF POS'>0 DO
        . SET POS=$F(SECTION,":")
        IF POS>0 DO
        . NEW P1,P2 SET P1=$E(SECTION,0,POS-1),P2=$E(SECTION,POS,$L(SECTION))
        . SET SECTION=P1_"<I>"_P2        
        ELSE  DO
        . SET SECTION="<I>"_SECTION
        DO INLINE(.SECTION)
        QUIT SECTION
        ;"
INLINE(SECTION) ;"
        IF SECTION'["]]" GOTO ILDN
        SET SECTION=$P(SECTION,"[",1)_"</I>["_$P(SECTION,"[",2,999)
        SET SECTION=$P(SECTION,"]]",1)_"]]<I>"_$P(SECTION,"]]",2,999)
ILDN    QUIT SECTION
        ;"
GROUP(IDX,TOPICS,NUMOFGROUPS)
        IF NUMOFGROUPS>9 SET NUMOFGROUPS=9   ;"MAX ALLOWED
        NEW TMGRESULT SET TMGRESULT=""  ;""[GROUP A] "
        NEW NUMPERGRP SET NUMPERGRP=TOPICS/NUMOFGROUPS
        IF NUMPERGRP["." DO
        . NEW DEC SET DEC=$P(NUMOFGROUPS,".",2)
        . IF DEC>4 DO
        . . SET NUMPERGRP=NUMPERGRP+1
        NEW GRPNUMBER SET GRPNUMBER=$P(IDX/NUMPERGRP,".",1)+1
        IF GRPNUMBER>NUMOFGROUPS SET GRPNUMBER=NUMOFGROUPS
        SET TMGRESULT=" [GROUP "_$$GETLETTER(GRPNUMBER)_"] "
        QUIT TMGRESULT
        ;"
GETLETTER(GRPNUMBER)  ;"
        QUIT $TR(GRPNUMBER,"123456789","ABCDEFGHI")
        ;"

