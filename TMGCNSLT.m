TMGCNSLT ;ELH - Consult Routines; 4/30/2014
	;;1.0;CONSULT/REQUEST TRACKING;2/18/14;Build 7
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
HDR	; Header for consult - wedge
	;
	;get and format eligibility info
	;D BLD("HDR",1,1,0,"YEAH BOY")
	;Q
	N VAEL,VAPA,GMRCPEL,SUB,GMRCFROM
	N CVELIG,CVMARKER ;WAT
	D ELIG^VADPT
	D ADD^VADPT
	S GMRCPEL=$P(VAEL(1),U,2)
	;
	F SUB=0 D
	.N GMRCFLN
	.S GMRCFLN=$P($G(^DPT(GMRCDFN,0)),U,1)
	.;S CVELIG=$$CVEDT^DGCV(GMRCDFN) S:$P($G(CVELIG),U,3) CVELIG="CV ELIGIBLE" ;WAT
	.D BLD("HDR",SUB,1,0,$G(GMRCDVL))
	.D BLD("HDR",SUB,1,6,"MEDICAL RECORD")
	.D BLD("HDR",SUB,0,39,"|")
	.D BLD("HDR",SUB,0,45,"CONSULTATION REQUEST")
	.D BLD("HDR",SUB,1,0,$G(GMRCDVL))
	.D BLD("HDR",SUB,1,0,$G(GMRCFLN))
	.;D BLD("HDR",SUB,0,45,$G(GMRCPEL))
	.;ELH commented 6/26/18 D BLD("HDR",SUB,1,0,$G(GMRCSN))
	.;ELH changed to below lineD BLD("HDR",SUB,0,16,$$EXDT(GMRCDOB))
        .D BLD("HDR",SUB,1,0,$$EXDT(GMRCDOB))
	.;D BLD("HDR",SUB,0,45,$G(GMRCELIG))
	.;D:$G(CVELIG)["CV" BLD("HDR",SUB,1,45,$G(CVELIG))
	;
	;                                  ADDRESS LINES 1-3
	F GMRCX=1,2,3 D:$L(VAPA(GMRCX))
	.D BLD("HDR",0,1,0,VAPA(GMRCX))
	.;I GMRCX=1 D BLD("HDR",0,0,51,"Standard Form 513 (Rev 9-77)")
	;
	;         CITY              STATE                ZIP CODE
	S GMRCX=VAPA(4)_"   "_$P(VAPA(5),U,2)_"      "_VAPA(6)
	;
	I $L(VAPA(8)) S GMRCX=GMRCX_"      Phone: "_VAPA(8)   ; TELEPHONE (IF AVAILABLE)
	D BLD("HDR",SUB,2,0,$G(GMRCDVL))
	D BLD("HDR",SUB,3,0," ")
	;
	Q
	;
FTR(GMRCSG)	;Footer for consult, wedge
	;
	N GMRCRMBD,GMRCFAC1,GMRCLOC,GMRCX,SUB,VAIN,VAPA,VAERR
	;
	D ADD^VADPT,INP^VADPT
	;
	S (GMRCLOC,GMRCRMBD)=""
	S GMRCLOC=$P($G(VAIN(4)),U,2)
	S GMRCRMBD=$G(VAIN(5))
	S:'$L(GMRCLOC) GMRCLOC=$P($G(^SC(+$P($G(^GMR(123,+GMRCIFN,0)),U,4),0)),U,1)
	;No location, IFC - consulting site
	I '$L(GMRCLOC),$P(GMRCRD,U,23),$P($G(GMRCRD(12)),U,5)="F" D
	.I $P(GMRCRD,U,21) S GMRCLOC=$$GET1^DIQ(4,$P(GMRCRD,U,21),.01)
	.E  S GMRCLOC=$$GET1^DIQ(4,$P(GMRCRD,U,23),.01)
	S:'$L(GMRCLOC) GMRCLOC=GMRCUL
	;
	
	D BLD("FTR",0,1,0,$G(GMRCEQL))
	D BLD("FTR",1,1,0,$G(GMRCEQL))
	;
	I ($G(GMRCSG("GMRCSIGM"))="electronic") D  I 1
	.;D BLD("FTR",0,1,0,"SIGNATURE & TITLE: ")
	.;D BLD("FTR",0,0,20,$G(GMRCSG("GMRCSIG"))_" /es/")
	.;D BLD("FTR",0,0,54,"|")
	.D BLD("FTR",0,1,20,$G(GMRCSG("GMRCSIGT")))
	.;D BLD("FTR",0,0,54,"|DATE: "_$$EXDT($G(GMRCSG("GMRCSDT"))))
	E  D
	.;D BLD("FTR",0,1,0,"AUTHOR & TITLE: ")
	.D BLD("FTR",0,0,20,$G(GMRCSG("GMRCSIG")))
	.;D BLD("FTR",0,0,54,"|")
	.D BLD("FTR",0,1,20,$G(GMRCSG("GMRCSIGT")))
	.;D BLD("FTR",0,0,54,"|DATE: "_$$EXDT($G(GMRCSG("GMRCSDT"))))
	Q
	;
BLD(SUB,NDX,LINE,TAB,TEXT,RUNTIME)  ;FOR FOOTER
        DO BLD^GMRCP5B(.SUB,.NDX,.LINE,.TAB,.TEXT,.RUNTIME)
        QUIT
        ;
EXDT(X)   ;
        IF +X=X DO
        . DO EXDT^GMRCP5B(.X)
        QUIT X
        ;
ASSMBL(PAGELEN,PAGEWID) ;Original code in GMRCP5C
        ;This is called from PRNT^GMRCP5A. Used to 
	;format second page.
        N GMRCHDR,GMRCPG,SUB,GMRCPAGE,GMRCDVL
        ;
        S GMRCDVL="",$P(GMRCDVL,"-",PAGEWID+1)=""
        S ^TMP("GMRC",$J,"SF513")=$G(PAGELEN)
        S GMRCPG=1,GMRCHDR=""
        D CLRZONE(0)
        ;"D MERGE("HDR",0,1)
        ;"D MERGE("FTR",0,5)
        ;
        ;REQ    add reason for request segment
        ;
        D FORMAT("REQ",PAGELEN,PAGEWID,2)
        ;
        ;PDIAG  add provisional diagnosis segment
        ;
        ;D FORMAT("PDIAG",PAGELEN,PAGEWID,$$SIZE("PDIAG",1)+1)
        ;
        ;RES    add tiu results segment
        ;
        D FORMAT("RES",PAGELEN,PAGEWID,6)
        ;
        ;ADD    add addendum segment
        ;
        D FORMAT("ADD",PAGELEN,PAGEWID,4)
        ;
        ;SREP   add service report segment
        ;
        D FORMAT("SREP",PAGELEN,PAGEWID,5)
        ;
        ;COM    add comments segment
        ;
        D FORMAT("COM",PAGELEN,PAGEWID,5)
        ;
        I $D(GMRCPAGE(300000)) D OUTPUT(GMRCPG)
        ;
        Q
        ;
FORMAT(SUB,PAGELEN,PAGEWID,OFFSET) ;
        DO FORMAT^GMRCP5C(.SUB,.PAGELEN,.PAGEWID,.OFFSET)
        QUIT
        ;
MERGE(SUB,NDX,ZONE) ;
        DO MERGE^GMRCP5C(.SUB,.NDX,.ZONE)
        QUIT
	;
CLRZONE(ZONE) ;
        DO CLRZONE^GMRCP5C(.ZONE)
        QUIT
        ;
OUTPUT(GMRCPG) ;
        DO OUTPUT^GMRCP5C(.GMRCPG)
        QUIT
	;
CNSLTLNK(RESULT,IEN123,IEN8925,COMPLETE)  ;
        ;"Purpose: To link a consult with a TIU Note and complete (if
        ;"         requested)
        ;"Input: RESULT - Return variable
        ;"       IEN123 - IEN of consult
        ;"       IEN8925 - IEN of progress note
        ;"       COMPLETE - Boolean whether to complete (1 OR 0) DEFAULT IS 0
        NEW TMGFDA,TMGMSG,TMGIEN
        SET RESULT="1^SUCCESS"
        SET IEN123=+$GET(IEN123)
        IF IEN123'>0 SET RESULT="-1^NO CONSULT SENT" GOTO CLDN
        SET IEN8925=+$GET(IEN8925)
        IF IEN8925'>0 SET RESULT="-1^NO TIU NOTE SENT" GOTO CLDN
        SET COMPLETE=+$GET(COMPLETE)      
        SET TMGFDA(123.03,"+1,"_IEN123_",",.01)=IEN8925_";TIU(8925,"
        DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO CLDN
        . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        IF COMPLETE=0 GOTO CLDN
        KILL TMGFDA,TMGMSG,TMGIEN
        SET TMGFDA(123,IEN123_",",8)="COMPLETE"
        DO FILE^DIE("E","TMGFDA","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG) GOTO CLDN
        ;"complete order
        NEW IEN100 SET IEN100=+$P($G(^GMR(123,IEN123,0)),"^",3)
        IF IEN100'>0 GOTO CLDN
        KILL TMGFDA
        SET TMGFDA(100,IEN100_",",5)="COMPLETE"
        DO FILE^DIE("E","TMGFDA","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG) GOTO CLDN
CLDN    QUIT
        ;"
COMPORDR()  ;"
        ;"Purpose: To look at each consult, and if it is Completed... set
        ;"         the corresponding order to complete
        NEW COMPLIEN SET COMPLIEN=+$ORDER(^ORD(100.01,"B","COMPLETE",0))
        IF COMPLIEN'>0 WRITE "COMPLETED IEN NOT FOUND IN FILE 100.01",! GOTO CODN
        NEW IEN123 SET IEN123=0
        FOR  SET IEN123=$ORDER(^GMR(123,IEN123)) QUIT:+IEN123'>0  DO
        . IF +$P($G(^GMR(123,IEN123,0)),"^",12)'=COMPLIEN QUIT
        . WRITE "CONSULT ",IEN123," IS COMPLETED",!
        . NEW IEN100 SET IEN100=+$P($G(^GMR(123,IEN123,0)),"^",3)
        . IF IEN100'>0 QUIT
        . NEW STATUS SET STATUS=$P($G(^OR(100,IEN100,3)),"^",3)
        . IF STATUS=COMPLIEN QUIT
        . WRITE "          COMPLETING ",IEN100,!
        . NEW TMGFDA,TMGMSG
        . SET TMGFDA(100,IEN100_",",5)="COMPLETE"
        . DO FILE^DIE("E","TMGFDA","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) WRITE "-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
CODN    QUIT