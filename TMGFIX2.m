TMGFIX2 ;TMG/kst/Misc system fixes; 5/27/14, 6/15/16, 3/24/21
       ;;1.0;TMG-LIB;**1**;05/21/09
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
MEGA   ;Install multiple Post-KIDS patch routines
        ;//DEPRECIATED
       GOTO MEGA^TMGRPC1B
       ;
FIXDD2  ;" i.e. Fix DD for file 2
       ;"Purpose: To alter the input tranform for the SSNUM field in file 2
       ;
       NEW TMGXFRM SET TMGXFRM=$PIECE($GET(^DD(2,.09,0)),"^",5,999)
       SET ^TMG("BAK",2,.09,"XFRM",$H)=TMGXFRM
       NEW TMGXFRM2 SET TMGXFRM2=$PIECE($TEXT(TXT^TMGFIX2),";;",2,999)
       SET $PIECE(^DD(2,.09,0),"^",5,999)=TMGXFRM2
       ;"Now change field in file 2 to be not required
       DO URESTRCT(2,.09)  ;"SSN field
       DO URESTRCT(2,.301) ;"Service Connected?
       DO URESTRCT(2,1901) ;"Veteran Y/N?
       QUIT
       ;
       ;"DON'T remove the following line.  It is used...
TXT    ;;K:X[""""!($A(X)=45) X I $D(X) S:'$D(DPTX) DFN=DA D:'(($G(DA)="+")&(X["P")) SSN^DGRPDD1 Q
 ;
FIX8927FD08  ;" Fix DD for file 8927, field .08 (DIALOG) field.
       ;"changes set from 0:NO;1:YES;   --> 0:NO;1:YES;2:YES_EMBEDDED;
       NEW NODE SET NODE=$GET(^DD(8927,.08,0))
       IF NODE="" QUIT
       NEW P3 SET P3=$PIECE(NODE,"^",3)
       IF P3'["0:NO;1:YES" QUIT
       SET $PIECE(P3,";",3)="2:YES_EMBEDDED"
       IF $EXTRACT(P3,$LENGTH(P3))'=";" SET P3=P3_";"
       SET $PIECE(NODE,"^",3)=P3
       SET ^DD(8927,.08,0)=NODE
       NEW NOW SET NOW=$$NOW^XLFDT\1
       SET ^DD(8927,.08,"DT")=NOW
       NEW WPZN SET WPZN=$GET(^DD(8927,.08,21,0)) IF WPZN="" QUIT
       NEW FOUND SET FOUND=0
       NEW ADDTXT SET ADDTXT="TMG-CPRS mod: Added 2=YES_EMBEDDED for dialogs in HTML editor"
       NEW LINE SET LINE=0
       FOR  SET LINE=$ORDER(^DD(8927,.08,21,LINE)) QUIT:+LINE'>0  DO
       . NEW STR SET STR=$GET(^DD(8927,.08,LINE,0))
       . IF STR[ADDTXT SET FOUND=1
       IF FOUND QUIT
       SET LINE=$ORDER(^DD(8927,.08,21,""),-1)+1
       SET ^DD(8927,.08,21,LINE,0)=ADDTXT
       SET ^DD(8927,.08,21,0)="^^"_LINE_"^"_LINE_"^"_NOW_"^^"
       QUIT
       ;
URESTRCT(TMGFILE,TMGFIELD) ;"i.e. UNRESTRICT
       ;"Purpose: Remove R flag from DD entry
       ;
       NEW TMGDEF SET TMGDEF=$PIECE($GET(^DD(TMGFILE,TMGFIELD,0)),"^",2)
       IF TMGDEF["R" DO
       . SET ^TMG("BAK",TMGFILE,TMGFIELD,0,"Field Def",$H)=TMGDEF
       . SET TMGDEF=$TRANSLATE(TMGDEF,"R","")  ;"REMOVE R (RESTRICTED) FLAG
       . SET $PIECE(^DD(TMGFILE,TMGFIELD,0),"^",2)=TMGDEF
       QUIT
 ;
ENSURECPRS ;// DEPRECIATED.  SEE TMGRPC1B
NSURCPRS ;
        ;"Purpose: Ensure the OR CPRS GUI CHART   ... has all needed RPC's
        ;"
        NEW DIC,X,Y
        SET DIC=19,DIC(0)="M"
        SET X="OR CPRS GUI CHART"
        DO ^DIC
        IF +Y'>0 GOTO ENCDN
        NEW L SET L=1
        NEW DONE SET DONE=0
        FOR  DO  QUIT:DONE
        . NEW LINE,RPC
        . SET LINE=$TEXT(RPCLIST+L^TMGFIX2)
        . SET L=L+1
        . IF LINE["<DONE>" SET DONE=1 QUIT
        . SET RPC=$PIECE(LINE,";;""",2)
        . IF RPC="" SET DONE=1 QUIT
        . ;"WRITE RPC,!
        . IF $$ENSURE1(+Y,RPC)=1 SET DONE=1 QUIT
ENCDN   QUIT
        ;
ENSURE1(IEN,RPC) ;// DEPRECIATED.  SEE TMGRPC1B
        ;"Purpose: to ensure that the RPC is present.
        ;"Results: 0 IF OK, 1 IF error
        NEW RPCIEN,TMGERR
        NEW RESULT SET RESULT=0
        NEW IENS SET IENS=","_IEN_","
        SET RPCIEN=$$FIND1^DIC(19.05,IENS,"UX",RPC,,,"TMGERR")
        IF $DATA(TMGERR) DO  GOTO ENS1DN
        . DO ZWRITE^TMGZWR("TMGERR")
        . ;"SET RESULT=1
        IF RPCIEN>0 DO  GOTO ENS1DN
        . WRITE "."
        WRITE "MISSING: ",RPC,!
        NEW TMGFDA,TMGIEN
        SET TMGFDA(19.05,"+1,"_IEN_",",.01)=RPC
        DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGERR) DO  GOTO ENS1DN
        . DO ZWRITE^TMGZWR("TMGERR")
        . SET RESULT=1
        IF $DATA(TMGIEN) WRITE "ADDED: ",RPC,!
ENS1DN  QUIT RESULT
        ;
ASKDELP()  ;"
        ;"Purpose: Prompt user to delete a patient
        ;"Input: none
        ;"Output: none
        ;"Result: none
        ;"Get user fileman access code
        IF DUZ(0)'="@" DO  GOTO ADDN
        . WRITE "Cannot delete patients. Contact IT for assistance.",!
        WRITE "***********************************",!
        WRITE "****        CAUTION!!          ****",!
        WRITE "**** This function will delete ****",!
        WRITE "**** a patient and may leave   ****",!
        WRITE "**** dangling pointers for     ****",!
        WRITE "**** TIU Notes and other data  ****",!
        WRITE "***********************************",!
        NEW X,Y,DIC,ANSWER
        SET DIC=2,DIC(0)="MAEQ"
        D ^DIC
        WRITE !
        IF +Y'>0 GOTO ADDN
        WRITE "Permanantly delete patient? (Type ""YES"" to continue)",!
        READ ">",ANSWER:15,!
        IF ANSWER="YES" DO
        . WRITE "Deleting ",$PIECE(Y,"^",2),!
        . SET ^TMG("TMP","DELPAT")=1
        . DO DELPAT(+Y)
        . WRITE "Done",!
ADDN    QUIT
        ;"
DELPAT(TMGDFN)  ;" CAUTION!!!!
        ;"Purpose: Delete a patient without intervention
        ;"Input: TMGDFN - IEN of patient to delete
        ;"Output: None
        ;"Result: None
        NEW DIK,DA,PROPERCALL
        SET PROPERCALL=$GET(^TMG("TMP","DELPAT"))
        IF PROPERCALL'=1 QUIT
        SET DA=TMGDFN
        SET DIK="^AUPNPAT("
        DO ^DIK
        SET DIK="^DPT("
        DO ^DIK
        KILL ^TMG("TMP","DELPAT")
        QUIT
        ;"
        ;//DEPRECIATED.  SEE TMGRPC1B
RPCLIST ;  OR CPRS GUI CHART RPC's From Astronaut VistA
        ;;"GMRC LIST CONSULT REQUESTS
        ;;"MAGG PAT PHOTOS
        ;;"XWB GET VARIABLE VALUE
        ;;"TIU AUTHORIZATION
        ;;"TIU CAN CHANGE COSIGNER?
        ;;"TIU CREATE ADDENDUM RECORD
        ;;"TIU CREATE RECORD
        ;;"TIU DELETE RECORD
        ;;"TIU DETAILED DISPLAY
        ;;"TIU DOCUMENTS BY CONTEXT
        ;;"TIU GET ADDITIONAL SIGNERS
        ;;"TIU GET ALERT INFO
        ;;"TIU GET DOCUMENT PARAMETERS
        ;;"TIU GET DS TITLES
        ;;"TIU GET DS URGENCIES
        ;;"TIU GET PERSONAL PREFERENCES
        ;;"TIU GET PN TITLES
        ;;"TIU GET RECORD TEXT
        ;;"TIU IDENTIFY CONSULTS CLASS
        ;;"TIU IS THIS A CONSULT?
        ;;"TIU JUSTIFY DELETE?
        ;;"TIU LOAD BOILERPLATE TEXT
        ;;"TIU LOAD RECORD FOR EDIT
        ;;"TIU LOCK RECORD
        ;;"TIU LONG LIST CONSULT TITLES
        ;;"TIU LONG LIST OF TITLES
        ;;"TIU NOTES
        ;;"TIU NOTES 16 BIT
        ;;"TIU NOTES BY VISIT
        ;;"TIU PERSONAL TITLE LIST
        ;;"TIU PRINT RECORD
        ;;"TIU REQUIRES COSIGNATURE
        ;;"TIU SIGN RECORD
        ;;"TIU SUMMARIES
        ;;"TIU SUMMARIES BY VISIT
        ;;"TIU UNLOCK RECORD
        ;;"TIU UPDATE ADDITIONAL SIGNERS
        ;;"TIU UPDATE RECORD
        ;;"TIU WHICH SIGNATURE ACTION
        ;;"ORB DELETE ALERT
        ;;"ORB FOLLOW-UP ARRAY
        ;;"ORB FOLLOW-UP STRING
        ;;"ORB FOLLOW-UP TYPE
        ;;"ORB SORT METHOD
        ;;"ORK TRIGGER
        ;;"ORQ NULL LIST
        ;;"ORQOR DETAIL
        ;;"TIU TEMPLATE CHECK BOILERPLATE
        ;;"TIU TEMPLATE CREATE/MODIFY
        ;;"TIU TEMPLATE DELETE
        ;;"TIU TEMPLATE GETPROOT
        ;;"TIU TEMPLATE LISTOWNR
        ;;"TIU TEMPLATE SET ITEMS
        ;;"TIU GET LIST OF OBJECTS
        ;;"ORQOR LIST
        ;;"ORQORB SORT
        ;;"ORQPT ATTENDING/PRIMARY
        ;;"ORQPT CLINIC PATIENTS
        ;;"ORQPT CLINICS
        ;;"ORQPT DEFAULT LIST SOURCE
        ;;"ORQPT DEFAULT PATIENT LIST
        ;;"ORQPT PATIENT TEAM PROVIDERS
        ;;"ORQPT PROVIDER PATIENTS
        ;;"ORQPT PROVIDERS
        ;;"ORQPT SPECIALTIES
        ;;"ORQPT SPECIALTY PATIENTS
        ;;"ORQPT TEAM PATIENTS
        ;;"ORQPT TEAMS
        ;;"ORQPT WARD PATIENTS
        ;;"ORQPT WARDRMBED
        ;;"ORQPT WARDS
        ;;"ORQQAL DETAIL
        ;;"ORQQAL LIST
        ;;"ORQQAL LIST REPORT
        ;;"ORQQCN ADDCMT
        ;;"ORQQCN ADMIN COMPLETE
        ;;"ORQQCN DETAIL
        ;;"ORQQCN DISCONTINUE
        ;;"ORQQCN FIND CONSULT
        ;;"ORQQCN FORWARD
        ;;"ORQQCN GET CONSULT
        ;;"ORQQCN GET ORDER NUMBER
        ;;"ORQQCN GET PROC SVCS
        ;;"ORQQCN LIST
        ;;"ORQQCN LOAD FOR EDIT
        ;;"ORQQCN MED RESULTS
        ;;"ORQQCN PRINT SF513
        ;;"ORQQCN RECEIVE
        ;;"ORQQCN RESUBMIT
        ;;"ORQQCN SET ACT MENUS
        ;;"ORQQCN SHOW SF513
        ;;"ORQQCN SIGFIND
        ;;"ORQQCN STATUS
        ;;"ORQQCN SVCLIST
        ;;"ORQQCN SVCTREE
        ;;"ORQQCN URGENCIES
        ;;"ORQQCN2 GET CONTEXT
        ;;"ORQQCN2 SAVE CONTEXT
        ;;"ORQQLR DETAIL
        ;;"ORQQLR SEARCH RANGE INPT
        ;;"ORQQLR SEARCH RANGE OUTPT
        ;;"ORQQPL ADD SAVE
        ;;"ORQQPL AUDIT HIST
        ;;"ORQQPL CHECK DUP
        ;;"ORQQPL CLIN FILTER LIST
        ;;"ORQQPL CLIN SRCH
        ;;"ORQQPL DELETE
        ;;"ORQQPL DETAIL
        ;;"ORQQPL EDIT LOAD
        ;;"ORQQPL EDIT SAVE
        ;;"ORQQPL INACTIVATE
        ;;"ORQQPL INIT PT
        ;;"ORQQPL INIT USER
        ;;"ORQQPL LIST
        ;;"ORQQPL PROB COMMENTS
        ;;"ORQQPL PROBLEM LEX SEARCH
        ;;"ORQQPL PROBLEM LIST
        ;;"ORQQPL PROV FILTER LIST
        ;;"ORQQPL PROVIDER LIST
        ;;"ORQQPL REPLACE
        ;;"ORQQPL SAVEVIEW
        ;;"ORQQPL SERV FILTER LIST
        ;;"ORQQPL SRVC SRCH
        ;;"ORQQPL UPDATE
        ;;"ORQQPL USER PROB CATS
        ;;"ORQQPL USER PROB LIST
        ;;"ORQQPL VERIFY
        ;;"ORQQPP LIST
        ;;"ORQQPS DETAIL
        ;;"ORQQPS LIST
        ;;"ORQQPX REMINDER DETAIL
        ;;"ORQQPX REMINDERS LIST
        ;;"ORQQVI NOTEVIT
        ;;"ORQQVI VITALS
        ;;"ORQQVI VITALS FOR DATE RANGE
        ;;"ORQQVI1 DETAIL
        ;;"ORQQVI1 GRID
        ;;"ORQQVI2 VITALS HELP
        ;;"ORQQVI2 VITALS RATE CHECK
        ;;"ORQQVI2 VITALS VAL & STORE
        ;;"ORQQVI2 VITALS VALIDATE
        ;;"ORQQVI2 VITALS VALIDATE TYPE
        ;;"ORQQVS DETAIL NOTES
        ;;"ORQQVS DETAIL SUMMARY
        ;;"ORQQVS VISITS/APPTS
        ;;"ORQQXMB MAIL GROUPS
        ;;"ORQQXQA PATIENT
        ;;"ORQQXQA USER
        ;;"ORWCH LOADALL
        ;;"ORWCH LOADSIZ
        ;;"ORWCH SAVEALL
        ;;"ORWCH SAVESIZ
        ;;"ORWCS LIST OF CONSULT REPORTS
        ;;"ORWCS PRINT REPORT
        ;;"ORWCS REPORT TEXT
        ;;"ORWCV DTLVST
        ;;"ORWCV LAB
        ;;"ORWCV POLL
        ;;"ORWCV START
        ;;"ORWCV STOP
        ;;"ORWCV VST
        ;;"ORWD DEF
        ;;"ORWD DT
        ;;"ORWD FORMID
        ;;"ORWD GET4EDIT
        ;;"ORWD KEY
        ;;"ORWD OI
        ;;"ORWD PROVKEY
        ;;"ORWD SAVE
        ;;"ORWD SAVEACT
        ;;"ORWD SIGN
        ;;"ORWD VALIDACT
        ;;"ORWD1 PARAM
        ;;"ORWD1 PRINTGUI
        ;;"ORWD1 RVPRINT
        ;;"ORWD2 DEVINFO
        ;;"ORWD2 MANUAL
        ;;"ORWDAL32 ALLERGY MATCH
        ;;"ORWDAL32 DEF
        ;;"ORWDAL32 SYMPTOMS
        ;;"ORWDCN32 DEF
        ;;"ORWDCN32 ORDRMSG
        ;;"ORWDCN32 PROCEDURES
        ;;"ORWDCSLT DEF
        ;;"ORWDCSLT LOOK200
        ;;"ORWDFH ADDLATE
        ;;"ORWDFH ATTR
        ;;"ORWDFH CURISO
        ;;"ORWDFH DIETS
        ;;"ORWDFH FINDTYP
        ;;"ORWDFH ISOIEN
        ;;"ORWDFH ISOLIST
        ;;"ORWDFH PARAM
        ;;"ORWDFH QTY2CC
        ;;"ORWDFH TFPROD
        ;;"ORWDFH TXT
        ;;"ORWDGX LOAD
        ;;"ORWDGX VMDEF
        ;;"ORWDLR ABBSPEC
        ;;"ORWDLR ALLSAMP
        ;;"ORWDLR DEF
        ;;"ORWDLR LOAD
        ;;"ORWDLR OIPARAM
        ;;"ORWDLR STOP
        ;;"ORWDLR32 ABBSPEC
        ;;"ORWDLR32 ALLSAMP
        ;;"ORWDLR32 ALLSPEC
        ;;"ORWDLR32 DEF
        ;;"ORWDLR32 IC DEFAULT
        ;;"ORWDLR32 IC VALID
        ;;"ORWDLR32 IMMED COLLECT
        ;;"ORWDLR32 LAB COLL TIME
        ;;"ORWDLR32 LOAD
        ;;"ORWDLR32 MAXDAYS
        ;;"ORWDLR32 ONE SAMPLE
        ;;"ORWDLR32 ONE SPECIMEN
        ;;"ORWDLR32 STOP
        ;;"ORWDOR VMSLCT
        ;;"ORWDPS32 ALLROUTE
        ;;"ORWDPS32 AUTH
        ;;"ORWDPS32 DLGSLCT
        ;;"ORWDPS32 DOSES
        ;;"ORWDPS32 DRUGMSG
        ;;"ORWDPS32 FORMALT
        ;;"ORWDPS32 ISSPLY
        ;;"ORWDPS32 IVAMT
        ;;"ORWDPS32 MEDISIV
        ;;"ORWDPS32 OISLCT
        ;;"ORWDPS32 SCSTS
        ;;"ORWDPS32 VALQTY
        ;;"ORWDPS32 VALRATE
        ;;"ORWDPS32 VALSCH
        ;;"ORWDRA DEF
        ;;"ORWDRA32 APPROVAL
        ;;"ORWDRA32 DEF
        ;;"ORWDRA32 IMTYPSEL
        ;;"ORWDRA32 ISOLATN
        ;;"ORWDRA32 LOCTYPE
        ;;"ORWDRA32 PROCMSG
        ;;"ORWDRA32 RADSRC
        ;;"ORWDRA32 RAORDITM
        ;;"ORWDX AGAIN
        ;;"ORWDX DGRP
        ;;"ORWDX DISMSG
        ;;"ORWDX DLGDEF
        ;;"ORWDX DLGID
        ;;"ORWDX DLGQUIK
        ;;"ORWDX FORMID
        ;;"ORWDX LOADRSP
        ;;"ORWDX LOCK
        ;;"ORWDX MSG
        ;;"ORWDX ORDITM
        ;;"ORWDX SAVE
        ;;"ORWDX SEND
        ;;"ORWDX SENDP
        ;;"ORWDX UNLOCK
        ;;"ORWDX WRLST
        ;;"ORWDXA ALERT
        ;;"ORWDXA COMPLETE
        ;;"ORWDXA DC
        ;;"ORWDXA DCREASON
        ;;"ORWDXA DCREQIEN
        ;;"ORWDXA FLAG
        ;;"ORWDXA FLAGTXT
        ;;"ORWDXA HOLD
        ;;"ORWDXA UNFLAG
        ;;"ORWDXA UNHOLD
        ;;"ORWDXA VALID
        ;;"ORWDXA VERIFY
        ;;"ORWDXA WCGET
        ;;"ORWDXA WCPUT
        ;;"ORWDXC ACCEPT
        ;;"ORWDXC DELAY
        ;;"ORWDXC DELORD
        ;;"ORWDXC DISPLAY
        ;;"ORWDXC FILLID
        ;;"ORWDXC ON
        ;;"ORWDXC SAVECHK
        ;;"ORWDXC SESSION
        ;;"ORWDXM AUTOACK
        ;;"ORWDXM DLGNAME
        ;;"ORWDXM FORMID
        ;;"ORWDXM LOADSET
        ;;"ORWDXM MENU
        ;;"ORWDXM MSTYLE
        ;;"ORWDXM PROMPTS
        ;;"ORWDXM1 BLDQRSP
        ;;"ORWDXM2 CLRRCL
        ;;"ORWDXQ DLGNAME
        ;;"ORWDXQ DLGSAVE
        ;;"ORWDXQ GETQLST
        ;;"ORWDXQ GETQNAM
        ;;"ORWDXQ PUTQLST
        ;;"ORWDXQ PUTQNAM
        ;;"ORWDXR ISREL
        ;;"ORWDXR RENEW
        ;;"ORWDXR RNWFLDS
        ;;"ORWGEPT CLINRNG
        ;;"ORWLR CUMULATIVE REPORT
        ;;"ORWLR CUMULATIVE SECTION
        ;;"ORWLR REPORT LISTS
        ;;"ORWLRR ALLTESTS
        ;;"ORWLRR ATESTS
        ;;"ORWLRR ATG
        ;;"ORWLRR ATOMICS
        ;;"ORWLRR CHART
        ;;"ORWLRR CHEMTEST
        ;;"ORWLRR GRID
        ;;"ORWLRR INTERIM
        ;;"ORWLRR INTERIMG
        ;;"ORWLRR INTERIMS
        ;;"ORWLRR MICRO
        ;;"ORWLRR NEWOLD
        ;;"ORWLRR PARAM
        ;;"ORWLRR SPEC
        ;;"ORWLRR TG
        ;;"ORWLRR USERS
        ;;"ORWLRR UTGA
        ;;"ORWLRR UTGD
        ;;"ORWLRR UTGR
        ;;"ORWMC PATIENT PROCEDURES
        ;;"ORWOR RESULT
        ;;"ORWOR SHEETS
        ;;"ORWOR TSALL
        ;;"ORWORB AUTOUNFLAG ORDERS
        ;;"ORWORB FASTUSER
        ;;"ORWORB GET TIU ALERT INFO
        ;;"ORWORB GETDATA
        ;;"ORWORB KILL UNSIG ORDERS ALERT
        ;;"ORWORDG ALLTREE
        ;;"ORWORDG GRPSEQB
        ;;"ORWORDG IEN
        ;;"ORWORDG MAPSEQ
        ;;"ORWORDG REVSTS
        ;;"ORWORR AGET
        ;;"ORWORR GET
        ;;"ORWORR GET4LST
        ;;"ORWORR GETBYIFN
        ;;"ORWORR GETTXT
        ;;"ORWPCE ACTIVE PROV
        ;;"ORWPCE ACTPROB
        ;;"ORWPCE CPTREQD
        ;;"ORWPCE DELETE
        ;;"ORWPCE DIAG
        ;;"ORWPCE GET EDUCATION TOPICS
        ;;"ORWPCE GET EXAM TYPE
        ;;"ORWPCE GET HEALTH FACTORS TY
        ;;"ORWPCE GET IMMUNIZATION TYPE
        ;;"ORWPCE GET SET OF CODES
        ;;"ORWPCE GET SKIN TEST TYPE
        ;;"ORWPCE GET TREATMENT TYPE
        ;;"ORWPCE HF
        ;;"ORWPCE IMM
        ;;"ORWPCE LEX
        ;;"ORWPCE LEXCODE
        ;;"ORWPCE NOTEVSTR
        ;;"ORWPCE PCE4NOTE
        ;;"ORWPCE PED
        ;;"ORWPCE PROC
        ;;"ORWPCE SAVE
        ;;"ORWPCE SCDIS
        ;;"ORWPCE SCSEL
        ;;"ORWPCE SK
        ;;"ORWPCE TRT
        ;;"ORWPCE VISIT
        ;;"ORWPCE XAM
        ;;"ORWPS ACTIVE
        ;;"ORWPS COVER
        ;;"ORWPS DETAIL
        ;;"ORWPS1 NEWDLG
        ;;"ORWPS1 PICKUP
        ;;"ORWPS1 REFILL
        ;;"ORWPT ADMITLST
        ;;"ORWPT APPTLST
        ;;"ORWPT CLINRNG
        ;;"ORWPT CWAD
        ;;"ORWPT DFLTSRC
        ;;"ORWPT DIEDON
        ;;"ORWPT DISCHARGE
        ;;"ORWPT ENCTITL
        ;;"ORWPT FULLSSN
        ;;"ORWPT ID INFO
        ;;"ORWPT LAST5
        ;;"ORWPT LIST ALL
        ;;"ORWPT PTINQ
        ;;"ORWPT SAVDFLT
        ;;"ORWPT SELCHK
        ;;"ORWPT SELECT
        ;;"ORWPT SHARE
        ;;"ORWPT TOP
        ;;"ORWPT1 PCDETAIL
        ;;"ORWPT1 PRCARE
        ;;"ORWPT16 ADMITLST
        ;;"ORWPT16 APPTLST
        ;;"ORWPT16 DEMOG
        ;;"ORWPT16 GETVSIT
        ;;"ORWPT16 ID INFO
        ;;"ORWPT16 LIST ALL
        ;;"ORWPT16 LOOKUP
        ;;"ORWPT16 PSCNVT
        ;;"ORWRA DEFAULT EXAM SETTINGS
        ;;"ORWRA IMAGING EXAMS
        ;;"ORWRA PRINT REPORT
        ;;"ORWRA REPORT TEXT
        ;;"ORWRP PRINT LAB REPORTS
        ;;"ORWRP PRINT REPORT
        ;;"ORWRP REPORT LISTS
        ;;"ORWRP REPORT TEXT
        ;;"ORWRP1 LISTNUTR
        ;;"ORWRP16 REPORT LISTS
        ;;"ORWRP16 REPORT TEXT
        ;;"ORWTIU GET DCSUMM CONTEXT
        ;;"ORWTIU GET TIU CONTEXT
        ;;"ORWTIU SAVE DCSUMM CONTEXT
        ;;"ORWTIU SAVE TIU CONTEXT
        ;;"ORWU CLINLOC
        ;;"ORWU DEVICE
        ;;"ORWU DT
        ;;"ORWU EXTNAME
        ;;"ORWU GBLREF
        ;;"ORWU GENERIC
        ;;"ORWU HASKEY
        ;;"ORWU HOSPLOC
        ;;"ORWU INPLOC
        ;;"ORWU NEWPERS
        ;;"ORWU NPHASKEY
        ;;"ORWU PATCH
        ;;"ORWU TOOLMENU
        ;;"ORWU USERINFO
        ;;"ORWU VALDT
        ;;"ORWU VALIDSIG
        ;;"ORWU VERSRV
        ;;"ORWU16 DEVICE
        ;;"ORWU16 HOSPLOC
        ;;"ORWU16 NEWPERS
        ;;"ORWU16 USERINFO
        ;;"ORWU16 VALDT
        ;;"ORWU16 VALIDSIG
        ;;"ORWUH POPUP
        ;;"ORWUX SYMTAB
        ;;"ORWUXT LST
        ;;"ORWUXT REF
        ;;"ORWUXT VAL
        ;;"ORQQCN DEFAULT REQUEST REASON
        ;;"ORWDX LOCK ORDER
        ;;"ORWDX UNLOCK ORDER
        ;;"ORWDCN32 NEWDLG
        ;;"ORQQCN GET SERVICE IEN
        ;;"ORQQCN PROVDX
        ;;"TIU TEMPLATE ACCESS LEVEL
        ;;"TIU GET DOCUMENT TITLE
        ;;"ORWPT BYWARD
        ;;"ORQQPX GET HIST LOCATIONS
        ;;"ORQQPX NEW REMINDERS ACTIVE
        ;;"ORWPCE GET VISIT
        ;;"TIU GET REQUEST
        ;;"ORWORB KILL EXPIR MED ALERT
        ;;"DG CHK BS5 XREF ARRAY
        ;;"DG CHK BS5 XREF Y/N
        ;;"DG CHK PAT/DIV MEANS TEST
        ;;"DG SENSITIVE RECORD ACCESS
        ;;"DG SENSITIVE RECORD BULLETIN
        ;;"ORQQCN CANEDIT
        ;;"ORQQCN EDIT DEFAULT REASON
        ;;"ORQQCN SF513 WINDOWS PRINT
        ;;"ORWCIRN FACLIST
        ;;"ORWDLR32 GET LAB TIMES
        ;;"ORWPT LEGACY
        ;;"ORWRP GET DEFAULT PRINTER
        ;;"ORWRP PRINT WINDOWS REPORT
        ;;"ORWRP SAVE DEFAULT PRINTER
        ;;"ORWRP WINPRINT DEFAULT
        ;;"ORWRP WINPRINT LAB REPORTS
        ;;"ORWTIU WINPRINT NOTE
        ;;"ORWPCE GAFOK
        ;;"ORWPCE MHCLINIC
        ;;"ORWPCE LOADGAF
        ;;"ORWPCE SAVEGAF
        ;;"ORWPCE FORCE
        ;;"TIU GET DEFAULT PROVIDER
        ;;"TIU GET SITE PARAMETERS
        ;;"TIU IS USER A PROVIDER?
        ;;"ORWOR VWGET
        ;;"ORWOR VWSET
        ;;"ORWU PARAM
        ;;"ORWDOR LKSCRN
        ;;"ORWDOR VALNUM
        ;;"ORWDPS32 VALROUTE
        ;;"ORWORB UNSIG ORDERS FOLLOWUP
        ;;"ORWTIU GET LISTBOX ITEM
        ;;"ORWRP2 HS COMP FILES
        ;;"ORWRP2 HS COMPONENTS
        ;;"ORWRP2 HS FILE LOOKUP
        ;;"ORWRP2 HS REPORT TEXT
        ;;"ORWRP2 HS SUBITEMS
        ;;"ORWPCE HASCPT
        ;;"ORWPCE ASKPCE
        ;;"ORWPCE MHTESTOK
        ;;"ORWPCE GAFURL
        ;;"ORQQPXRM DIALOG PROMPTS
        ;;"ORQQPXRM EDUCATION SUBTOPICS
        ;;"ORQQPXRM EDUCATION SUMMARY
        ;;"ORQQPXRM EDUCATION TOPIC
        ;;"ORQQPXRM MENTAL HEALTH
        ;;"ORQQPXRM MENTAL HEALTH RESULTS
        ;;"ORQQPXRM MENTAL HEALTH SAVE
        ;;"ORQQPXRM PROGRESS NOTE HEADER
        ;;"ORQQPXRM REMINDER CATEGORIES
        ;;"ORQQPXRM REMINDER DETAIL
        ;;"ORQQPXRM REMINDER DIALOG
        ;;"ORQQPXRM REMINDER EVALUATION
        ;;"ORQQPXRM REMINDER INQUIRY
        ;;"ORQQPXRM REMINDER WEB
        ;;"ORQQPXRM REMINDERS APPLICABLE
        ;;"ORQQPXRM REMINDERS UNEVALUATED
        ;;"ORWLRR INFO
        ;;"TIU GET PRINT NAME
        ;;"TIU WAS THIS SAVED?
        ;;"ORWD1 COMLOC
        ;;"ORWD1 SIG4ANY
        ;;"ORWD1 SIG4ONE
        ;;"ORWOR UNSIGN
        ;;"ORWPT INPLOC
        ;;"ORQQCN2 GET PREREQUISITE
        ;;"ORQQCN2 SCHEDULE CONSULT
        ;;"YS GAF API
        ;;"TIU LONG LIST BOILERPLATED
        ;;"ORWDLR33 FUTURE LAB COLLECTS
        ;;"ORWRP PRINT REMOTE REPORT
        ;;"ORWRP PRINT WINDOWS REMOTE
        ;;"ORWRP PRINT LAB REMOTE
        ;;"ORWRP PRINT WINDOWS LAB REMOTE
        ;;"ORQQPXRM DIALOG ACTIVE
        ;;"ORWPCE MH TEST AUTHORIZED
        ;;"TIU GET BOILERPLATE
        ;;"ORWRP2 HS COMPONENT SUBS
        ;;"ORWCH SAVFONT
        ;;"ORWDLR33 LASTTIME
        ;;"ORWD1 SVONLY
        ;;"ORWPCE HASVISIT
        ;;"ORWPCE GETMOD
        ;;"ORWPCE CPTMODS
        ;;"XWB REMOTE CLEAR
        ;;"XWB REMOTE GETDATA
        ;;"XWB REMOTE RPC
        ;;"XWB REMOTE STATUS CHECK
        ;;"ORQQCN ASSIGNABLE MED RESULTS
        ;;"ORQQCN ATTACH MED RESULTS
        ;;"ORQQCN GET MED RESULT DETAILS
        ;;"ORQQCN REMOVABLE MED RESULTS
        ;;"ORQQCN REMOVE MED RESULTS
        ;;"ORQQCN SVC W/SYNONYMS
        ;;"ORWCV1 COVERSHEET LIST
        ;;"ORWORB KILL EXPIR OI ALERT
        ;;"ORWPCE GETSVC
        ;;"ORWRP LAB REPORT LISTS
        ;;"ORWTPN GETCLASS
        ;;"ORWTPN GETTC
        ;;"ORWTPO CSARNGD
        ;;"ORWTPO CSLABD
        ;;"ORWTPO GETTABS
        ;;"ORWTPP ADDLIST
        ;;"ORWTPP CHKSURR
        ;;"ORWTPP CLDAYS
        ;;"ORWTPP CLEARNOT
        ;;"ORWTPP CLRANGE
        ;;"ORWTPP CSARNG
        ;;"ORWTPP CSLAB
        ;;"ORWTPP DELLIST
        ;;"ORWTPP GETCOMBO
        ;;"ORWTPP GETCOS
        ;;"ORWTPP GETDCOS
        ;;"ORWTPP GETNOT
        ;;"ORWTPP GETNOTO
        ;;"ORWTPP GETOC
        ;;"ORWTPP GETOTHER
        ;;"ORWTPP GETREM
        ;;"ORWTPP GETSUB
        ;;"ORWTPP GETSURR
        ;;"ORWTPP GETTD
        ;;"ORWTPP GETTU
        ;;"ORWTPP LSDEF
        ;;"ORWTPP NEWLIST
        ;;"ORWTPP PLISTS
        ;;"ORWTPP PLTEAMS
        ;;"ORWTPP REMLIST
        ;;"ORWTPP SAVECD
        ;;"ORWTPP SAVECS
        ;;"ORWTPP SAVELIST
        ;;"ORWTPP SAVENOT
        ;;"ORWTPP SAVENOTO
        ;;"ORWTPP SAVEOC
        ;;"ORWTPP SAVEPLD
        ;;"ORWTPP SAVESURR
        ;;"ORWTPP SAVET
        ;;"ORWTPP SETCOMBO
        ;;"ORWTPP SETDCOS
        ;;"ORWTPP SETOTHER
        ;;"ORWTPP SETREM
        ;;"ORWTPP SETSUB
        ;;"ORWTPP SORTDEF
        ;;"ORWTPP TEAMS
        ;;"ORWTPT ATEAMS
        ;;"ORWTPT GETTEAM
        ;;"TIU TEMPLATE GET DEFAULTS
        ;;"TIU TEMPLATE GET DESCRIPTION
        ;;"TIU TEMPLATE SET DEFAULTS
        ;;"ORWTIU IDNOTES INSTALLED
        ;;"ORQQPX GET FOLDERS
        ;;"ORQQPX SET FOLDERS
        ;;"TIU FIELD CAN EDIT
        ;;"TIU FIELD DELETE
        ;;"TIU FIELD EXPORT
        ;;"TIU FIELD IMPORT
        ;;"TIU FIELD LIST
        ;;"TIU FIELD LOAD
        ;;"TIU FIELD LOAD BY IEN
        ;;"TIU FIELD LOCK
        ;;"TIU FIELD NAME IS UNIQUE
        ;;"TIU FIELD SAVE
        ;;"TIU FIELD UNLOCK
        ;;"ORWDPS1 CHK94
        ;;"ORWDPS1 ODSLCT
        ;;"ORWDPS1 SCHALL
        ;;"ORWDPS2 ADMIN
        ;;"ORWDPS2 DAY2QTY
        ;;"ORWDPS2 OISLCT
        ;;"ORWDPS2 REQST
        ;;"ORWDX DGNM
        ;;"ORWUL FV4DG
        ;;"ORWUL FVIDX
        ;;"ORWUL FVSUB
        ;;"ORWUL QV4DG
        ;;"ORWUL QVIDX
        ;;"ORWUL QVSUB
        ;;"ORWDPS1 DFLTSPLY
        ;;"PXRM REMINDER DIALOG (TIU)
        ;;"ORWPCE ANYTIME
        ;;"ORQQPX GET DEF LOCATIONS
        ;;"ORWTPP GETIMG
        ;;"ORWTPP SETIMG
        ;;"ORWTPO GETIMGD
        ;;"ORQQPX REM INSERT AT CURSOR
        ;;"TIU REMINDER DIALOGS
        ;;"TIU REM DLG OK AS TEMPLATE
        ;;"ORWDPS2 MAXREF
        ;;"ORWDPS2 SCHREQ
        ;;"ORWRP COLUMN HEADERS
        ;;"TIU FIELD DOLMTEXT
        ;;"TIU TEMPLATE PERSONAL OBJECTS
        ;;"ORWPCE AUTO VISIT TYPE SELECT
        ;;"ORWDPS2 QTY2DAY
        ;;"ORWU HAS OPTION ACCESS
        ;;"TIU TEMPLATE LOCK
        ;;"ORQQPX LVREMLST
        ;;"ORQQPX NEW COVER SHEET ACTIVE
        ;;"ORQQPX NEW COVER SHEET REMS
        ;;"ORQQPX SAVELVL
        ;;"PXRM REMINDER CATEGORY
        ;;"PXRM REMINDERS AND CATEGORIES
        ;;"TIU DIV AND CLASS INFO
        ;;"TIU TEMPLATE GETBOIL
        ;;"TIU TEMPLATE GETITEMS
        ;;"TIU TEMPLATE GETROOTS
        ;;"TIU TEMPLATE GETTEXT
        ;;"TIU TEMPLATE ISEDITOR
        ;;"TIU TEMPLATE UNLOCK
        ;;"TIU USER CLASS LONG LIST
        ;;"ORWPCE ALWAYS CHECKOUT
        ;;"ORWPCE GET EXCLUDED
        ;;"ORWDPS1 FORMALT
        ;;"ORQPT DEFAULT LIST SORT
        ;;"ORWDPS1 DOSEALT
        ;;"ORWTPR OCDESC
        ;;"ORWTPR NOTDESC
        ;;"ORWDPS1 FAILDEA
        ;;"ORQPT DEFAULT CLINIC DATE RANG
        ;;"ORWTIU CANLINK
        ;;"TIU ID ATTACH ENTRY
        ;;"TIU ID CAN ATTACH
        ;;"TIU ID CAN RECEIVE
        ;;"TIU ID DETACH ENTRY
        ;;"ORWCOM GETOBJS
        ;;"ORWCOM DETAILS
        ;;"ORWCOM PTOBJ
        ;;"TIU TEMPLATE GETLINK
        ;;"TIU TEMPLATE ALL TITLES
        ;;"ORWSR LIST
        ;;"ORWSR SHOW SURG TAB
        ;;"ORWSR GET SURG CONTEXT
        ;;"ORWSR SAVE SURG CONTEXT
        ;;"ORWSR ONECASE
        ;;"ORWSR SHOW OPTOP WHEN SIGNING
        ;;"ORWSR IS NON-OR PROCEDURE
        ;;"ORWSR CASELIST
        ;;"ORQQCN GET PROC IEN
        ;;"ORWRP PRINT V REPORT
        ;;"ORWRP3 EXPAND COLUMNS
        ;;"ORWTPD ACTDF
        ;;"ORWTPD DELDFLT
        ;;"ORWTPD GETDFLT
        ;;"ORWTPD RSDFLT
        ;;"ORWTPD SUDF
        ;;"ORWTPD SUINDV
        ;;"ORWTPD GETSETS
        ;;"ORWCOM ORDEROBJ
        ;;"ORWRP2 COMPABV
        ;;"ORWRP2 GETLKUP
        ;;"ORWRP2 SAVLKUP
        ;;"ORWRP2 COMPDISP
        ;;"ORWPCE ISCLINIC
        ;;"ORWCH SAVECOL
        ;;"ORWSR RPTLIST
        ;;"ORQQPXRM MST UPDATE
        ;;"ORWMC PATIENT PROCEDURES1
        ;;"ORWRA IMAGING EXAMS1
        ;;"ORWRA REPORT TEXT1
        ;;"ORWDPS4 CPINFO
        ;;"ORWDPS4 CPLST
        ;;"ORWORB KILL UNVER MEDS ALERT
        ;;"ORWORB KILL UNVER ORDERS ALERT
        ;;"ORWPCE HNCOK
        ;;"ORWPS MEDHIST
        ;;"TIU FIELD CHECK
        ;;"TIU FIELD LIST ADD
        ;;"TIU FIELD LIST IMPORT
        ;;"TIU SET DOCUMENT TEXT
        ;;"ORWDPS2 CHKPI
        ;;"ORWDXR GTORITM
        ;;"ORWDPS2 CHKGRP
        ;;"ORWDPS2 QOGRP
        ;;"ORWDXR GETPKG
        ;;"ORQPT MAKE RPL
        ;;"ORQPT READ RPL
        ;;"ORQPT KILL RPL
        ;;"ORWTIU GET SAVED CP FIELDS
        ;;"ORWDPS1 LOCPICK
        ;;"ORWPT LAST5 RPL
        ;;"ORWPT FULLSSN RPL
        ;;"ORWOR PKIUSE
        ;;"ORWOR1 SIG
        ;;"ORWOR1 CHKDIG
        ;;"ORWOR1 GETDTEXT
        ;;"ORWOR1 GETDSIG
        ;;"ORWTPD GETIMG
        ;;"OREVNTX1 PRMPTID
        ;;"ORECS01 CHKESSO
        ;;"ORECS01 VSITID
        ;;"OREVNTX LIST
        ;;"OREVNTX PAT
        ;;"OREVNTX1 GTEVT
        ;;"OREVNTX1 CPACT
        ;;"OREVNTX1 CURSPE
        ;;"OREVNTX1 CHGEVT
        ;;"OREVNTX1 DELPTEVT
        ;;"OREVNTX1 DFLTEVT
        ;;"OREVNTX ACTIVE
        ;;"OREVNTX1 PUTEVNT
        ;;"OREVNTX1 WRLSTED
        ;;"OREVNTX1 EVT
        ;;"OREVNTX1 NAME
        ;;"OREVNTX1 MATCH
        ;;"OREVNTX1 EMPTY
        ;;"OREVNTX1 EXISTS
        ;;"OREVNTX1 GTEVT1
        ;;"OREVNTX1 DIV
        ;;"OREVNTX1 DIV1
        ;;"OREVNTX1 LOC
        ;;"OREVNTX1 LOC1
        ;;"ORWDX SENDED
        ;;"OREVNTX1 GETDLG
        ;;"ORECS01 ECPRINT
        ;;"ORECS01 ECRPT
        ;;"OREVNTX1 ISDCOD
        ;;"OREVNTX1 SETDFLT
        ;;"TIU IS THIS A CLINPROC?
        ;;"TIU IDENTIFY CLINPROC CLASS
        ;;"TIU LONG LIST CLINPROC TITLES
        ;;"ORWDPS1 HASOIPI
        ;;"OREVNTX1 DEFLTS
        ;;"OREVNTX1 MULTS
        ;;"OREVNTX1 DONE
        ;;"OREVNTX1 PROMPT IDS
        ;;"ORWCIRN CHECKLINK
        ;;"XWB DIRECT RPC
        ;;"ORWDPS1 HASROUTE
        ;;"ORQQCN UNRESOLVED
        ;;"OREVNTX1 DELDFLT
        ;;"ORWCH LDFONT
        ;;"ORWU1 NAMECVT
        ;;"OREVNTX1 DFLTDLG
        ;;"ORWDPS5 LESAPI
        ;;"ORWDPS5 LESGRP
        ;;"OREVNTX1 TYPEXT
        ;;"ORWORR RGET
        ;;"OREVNTX1 AUTHMREL
        ;;"OREVNTX1 HAVEPRT
        ;;"OREVNTX1 CMEVTS
        ;;"OREVNTX1 ODPTEVID
        ;;"ORWOR PKISITE
        ;;"OREVNTX1 COMP
        ;;"OREVNTX1 ISHDORD
        ;;"ORWDXR ORCPLX
        ;;"OREVNTX1 ISPASS
        ;;"OREVNTX1 ISPASS1
        ;;"OREVNTX1 DLGIEN
        ;;"ORWDXR CANRN
        ;;"ORWDXR ISCPLX
        ;;"ORWDXA OFCPLX
        ;;"ORQQPX GET NOT PURPOSE
        ;;"ORWDPS1 IVDEA
        ;;"ORWDXR ISNOW
        ;;"ORRHCQ QRYITR
        ;;"OREVNTX1 GETSTS
        ;;"ORWU DEFAULT DIVISION
        ;;"ORWDXA ISACTOI
        ;;"ORECS01 SAVPATH
        ;;"ORWOR RESULT HISTORY
        ;;"XUS GET TOKEN
        ;;"ORQQPX IMMUN LIST
        ;;"XWB DEFERRED CLEARALL
        ;;"ORWOR1 SETDTEXT
        ;;"ORWOR1 GETDEA
        ;;"ORWOR1 GETDSCH
        ;;"ORWORB TEXT FOLLOWUP
        ;;"ORWU1 NEWLOC
        ;;"ORWPCE ACTIVE CODE
        ;;"ORQQPXRM GET WH LETTER TEXT
        ;;"ORQQPXRM GET WH LETTER TYPE
        ;;"ORQQPXRM GET WH PROC RESULT
        ;;"ORQQPXRM WOMEN HEALTH SAVE
        ;;"ORB FORWARD ALERT
        ;;"ORB RENEW ALERT
        ;;"ORPRF CLEAR
        ;;"ORPRF GETFLG
        ;;"ORPRF HASFLG
        ;;"ORWTPD GETOCM
        ;;"TIU ONE VISIT NOTE?
        ;;"VAFCTFU CONVERT ICN TO DFN
        ;;"ORIMO IMOLOC
        ;;"ORIMO IMOOD
        ;;"ORWDPS4 IPOD4OP
        ;;"ORWDPS4 UPDTDG
        ;;"TIU USER INACTIVE?
        ;;"ORWTPD PUTOCM
        ;;"ORWOR ACTION TEXT
        ;;"ORQQPXRM GEC DIALOG
        ;;"ORQQPXRM GET WH REPORT TEXT
        ;;"ORWDXR01 CANCHG
        ;;"ORWDXR01 SAVCHG
        ;;"TIU HAS AUTHOR SIGNED?
        ;;"ORQQPXRM CHECK REM VERSION
        ;;"ORQQPXRM GEC STATUS PROMPT
        ;;"ORWDAL32 SEND BULLETIN
        ;;"ORWDBA1 ORPKGTYP
        ;;"ORWDXR01 ISSPLY
        ;;"ORWDBA1 RCVORCI
        ;;"ORWPS REASON
        ;;"ORQQPXRM GEC FINISHED?
        ;;"ORWDXM3 ISUDQO
        ;;"ORWDBA1 SCLST
        ;;"ORWDXR01 OXDATA
        ;;"ORWDBA1 BASTATUS
        ;;"ORWORB SETSORT
        ;;"ORWORB GETSORT
        ;;"ORWOR EXPIRED
        ;;"ORECS01 GETDIV
        ;;"ORWTPD1 GETEFDAT
        ;;"ORWTPD1 GETEDATS
        ;;"ORWTPD1 PUTEDATS
        ;;"ORWTPD1 GETCSDEF
        ;;"ORWTPD1 GETCSRNG
        ;;"ORWTPD1 PUTCSRNG
        ;;"ORWTPD1 GETEAFL
        ;;"ORWDBA1 GETORDX
        ;;"ORWDBA3 HINTS
        ;;"ORWDAL32 LOAD FOR EDIT
        ;;"ORWDAL32 SAVE ALLERGY
        ;;"ORWDAL32 SITE PARAMS
        ;;"ORWPCE CXNOSHOW
        ;;"ORWDBA2 ADDPDL
        ;;"ORWDBA2 DELPDL
        ;;"ORWDBA2 GETDUDC
        ;;"ORWDBA2 GETPDL
        ;;"ORWDBA4 GETBAUSR
        ;;"ORWDBA4 GETTFCI
        ;;"ORWNSS NSSMSG
        ;;"ORWNSS QOSCH
        ;;"ORWNSS VALSCH
        ;;"ORWNSS CHKSCH
        ;;"ORWDPS4 ISUDIV
        ;;"ORWDPS32 AUTHNVA
        ;;"ORWTIU CHKTXT
        ;;"ORWDPS5 ISVTP
        ;;"TIU IS THIS A SURGERY?
        ;;"TIU IDENTIFY SURGERY CLASS
        ;;"TIU LONG LIST SURGERY TITLES
        ;;"TIU GET DOCUMENTS FOR REQUEST
        ;;"TIU SET ADMINISTRATIVE CLOSURE
        ;;"ORBCMA5 GETUDID
        ;;"ORIMO ISCLOC
        ;;"ORIMO ISIVQO
        ;;"ORWDBA7 GETIEN9
        ;;"ORWGN GNLOC
        ;;"ORWGN AUTHUSR
        ;;"ORVAA VAA
        ;;"ORWCIRN VISTAWEB
        ;;"ORWCIRN WEBCH
        ;;"ORWDAL32 CLINUSER
        ;;"ORWDBA7 ISWITCH
        ;;"ORWDFH CURRENT MEALS
        ;;"ORWDFH NFSLOC READY
        ;;"ORWDFH OPDIETS
        ;;"ORWMHV MHV
        ;;"ORWPCE1 NONCOUNT
        ;;"ORWPFSS IS PFSS ACTIVE?
        ;;"GMV EXTRACT REC
        ;;"GMV MARK ERROR
        ;;"ORWDXVB COMPORD
        ;;"ORWDXVB GETALL
        ;;"ORWDXVB RAW
        ;;"ORWDXVB RESULTS
        ;;"ORWDXVB STATALOW
        ;;"ORWGRPC ALLITEMS
        ;;"ORWGRPC CLASS
        ;;"ORWGRPC DATEITEM
        ;;"ORWGRPC DELVIEWS
        ;;"ORWGRPC DETAILS
        ;;"ORWGRPC GETDATES
        ;;"ORWGRPC GETPREF
        ;;"ORWGRPC GETVIEWS
        ;;"ORWGRPC ITEMDATA
        ;;"ORWGRPC ITEMS
        ;;"ORWGRPC LOOKUP
        ;;"ORWGRPC PUBLIC
        ;;"ORWGRPC RPTPARAM
        ;;"ORWGRPC SETPREF
        ;;"ORWGRPC SETVIEWS
        ;;"ORWGRPC TESTSPEC
        ;;"ORWGRPC TYPES
        ;;"TIU GET DOCUMENT STATUS
        ;;"TIU GET PRF ACTIONS
        ;;"TIU ISPRF
        ;;"TIU LINK TO FLAG
        ;;"ORWGRPC DETAIL
        ;;"ORWU VERSION
        ;;"GMV ALLERGY
        ;;"ORWCIRN WEBADDR
        ;;"ORWGRPC TAX
        ;;"GMV DLL VERSION
        ;;"ORWDX CHANGE
        ;;"GMV ADD VM
        ;;"GMV CONVERT DATE
        ;;"GMV GET CATEGORY IEN
        ;;"GMV GET CURRENT TIME
        ;;"GMV GET VITAL TYPE IEN
        ;;"GMV LATEST VM
        ;;"GMV MANAGER
        ;;"GMV PARAMETER
        ;;"GMV USER
        ;;"GMV VITALS/CAT/QUAL
        ;;"GMV V/M ALLDATA
        ;;"TIU GET LINKED PRF NOTES
        ;;"TIU GET PRF TITLE
        ;;"ORWDX1 PATWARD
        ;;"ORWRP4 HDR MODIFY
        ;;"ORWDX1 STCHANGE
        ;;"ORWDX1 DCREN
        ;;"ORQQPXRM MHV
        ;;"ORWGRPC GETSIZE
        ;;"ORWGRPC SETSIZE
        ;;"GMV LOCATION SELECT
        ;;"ORWCIRN AUTORDV
        ;;"ORPRF TRIGGER POPUP
        ;;"ORWCIRN HDRON
        ;;"MAG4 REMOTE IMPORT
        ;;"ORWPT ENHANCED PATLOOKUP
        ;;"ORWPT OTHER-RADIOBUTTONS
        ;;"TMG ADD PATIENT
        ;;"TMG AUTOSIGN TIU DOCUMENT
        ;;"TMG BARCODE DECODE
        ;;"TMG BARCODE ENCODE
        ;;"TMG DOWNLOAD FILE
        ;;"TMG DOWNLOAD FILE DROPBOX
        ;;"TMG GET BLANK TIU DOCUMENT
        ;;"TMG GET DFN
        ;;"TMG GET IMAGE LONG DESCRIPTION
        ;;"TMG GET PATIENT DEMOGRAPHICS
        ;;"TMG SET PATIENT DEMOGRAPHICS
        ;;"TMG UPLOAD FILE
        ;;"TMG UPLOAD FILE DROPBOX
        ;;"TMG CPRS GET URL LIST
        ;;"TMG INIFILE GET
        ;;"TMG INIFILE SET
        ;;"TMG CPRS ACCESS LOG RETRIEVE
        ;;"TMG CPRS ACCESS LOG STORE
        ;;"TMG CPRS GET AUDIT DETAIL
        ;;"TMG CPRS GET AUDIT PER USER
        ;;"TMG CPRS GET AUDIT PER PATIENT
        ;;"TMG CPRS GET DD HELP
        ;;"TMG CPRS GET INSTIT LIST
        ;;"TMG CPRS GET LAB COMPONENTS
        ;;"TMG CPRS GET LAB LIST
        ;;"TMG CPRS GET SPEC LIST
        ;;"TMG CPRS GET URL LIST
        ;;"TMG CPRS LAB DEF SPECIMEN GET
        ;;"TMG CPRS LAB DEF SPECIMEN SET
        ;;"TMG CPRS POST LAB VALUES  
        ;;"TMG CPRS PROCESS NOTE  
        ;;"MAGG PAT PHOTOS
        ;;"<DONE>
        QUIT
        ;
RMDUPROS ;//DEPRECIATED  -- MOVED TO RMDUSCNS^TMGTIUF1
        QUIT
FIXFUTMG ;
        NEW TIUIEN SET TIUIEN=0
        NEW TMGTEXT
        FOR  SET TIUIEN=$ORDER(^TIU(8925,TIUIEN)) QUIT:+TIUIEN'>0  DO
        . WRITE "CHECKING ",TIUIEN,!
        . SET TMGTEXT=$GET(^TIU(8925,TIUIEN,"TMG"))
        . IF TMGTEXT="^^-1^" DO
        . . WRITE " - TMGTEXT =",TMGTEXT,". KILLING.",!
        . . KILL ^TIU(8925,TIUIEN,"TMG")
        QUIT
        ;"
FIX1418
        NEW TIUIEN SET TIUIEN=0
        FOR  SET TIUIEN=$ORDER(^TIU(8925,"B",1418,TIUIEN)) QUIT:+TIUIEN'>0  DO
        . WRITE "SETTING ",TIUIEN,!
        . SET ^TIU(8925,"B",1408,TIUIEN)=""
        . SET $PIECE(^TIU(8925,TIUIEN,0),"^",1)=1408
        KILL ^TIU(8925,"B",1418)
        QUIT
        ;"
XFERPT()  ;"Tranfer 1 patient records to another patient
        ;"Purpose: This function is designed to transfer the images, tiu
        ;"         notes, and lab results to another account
        WRITE "*******************************************************",!
        WRITE "**                  WARNING!!!!                      **",!
        WRITE "** This function will completely transfer the TIU    **",!
        WRITE "** notes, images, and lab results from one patient   **",!
        WRITE "** account to another. Use this function with        **",!
        WRITE "** with caution as it cannot be undone.              **",!
        WRITE "*******************************************************",!!
        NEW RESPONSE
ANS     READ "DO YOU WANT TO CONTINUE? (Y/N) ",RESPONSE,!
        SET RESPONSE=$$UP^XLFSTR(RESPONSE)
        IF RESPONSE="N" GOTO XPDn
        IF RESPONSE'="Y"  WRITE "INVALID RESPONSE",! GOTO ANS
        NEW SOURCEPT,DESTPT
        ;"
        ;" Get SOURCE patient
        WRITE "SELECT SOURCE PATIENT"
        NEW DIC,X,Y
        SET DIC=2
        SET DIC(0)="MAEQ"
        DO ^DIC WRITE !
        IF +Y'>0 QUIT
        SET SOURCEPT=Y 
        ;"
        ;" Get DESTINATION patient
        WRITE "SELECT DESTINATION PATIENT"
        NEW DIC,X,Y
        SET DIC=2
        SET DIC(0)="MAEQ"
        DO ^DIC WRITE !
        IF +Y'>0 QUIT
        SET DESTPT=Y
        ;"
        ;" Verify
        SET RESPONSE=""
        WRITE !,"YOU ARE ABOUT TO TRANSFER ",$P(SOURCEPT,"^",2)," INTO ",$P(DESTPT,"^",2)
ANS2    READ ". CONTINUE? (Y/N) ",RESPONSE
        SET RESPONSE=$$UP^XLFSTR(RESPONSE)
        IF RESPONSE="N" GOTO XPDn
        IF RESPONSE'="Y" WRITE "INVALID RESPONSE",! GOTO ANS2
        ;"
        ;"Transfer
        NEW SOURCEIEN,DESTIEN,TMGFDA,TMGIEN,TMGMSG
        SET SOURCEIEN=$P(SOURCEPT,"^",1)
        SET DESTIEN=$P(DESTPT,"^",1)
        ;"
        ;"-> Images
        WRITE !,"TRANSFERRING IMAGES TO ",$P(DESTPT,"^",2),!
        NEW MAGIEN SET MAGIEN=0
        FOR  SET MAGIEN=$ORDER(^MAG(2005,"AC",SOURCEIEN,MAGIEN)) QUIT:MAGIEN'>0  DO
        . WRITE " ->Transferring image #",MAGIEN," to ",$P(DESTPT,"^",2),!
        . KILL TMGFDA,TMGMSG
        . SET TMGFDA(2005,MAGIEN_",",.01)=$P(DESTPT,"^",2)_" "_$P($GET(^DPT(DESTIEN,0)),"^",9)
        . SET TMGFDA(2005,MAGIEN_",",5)="`"_DESTIEN
        . DO FILE^DIE("E","TMGFDA","TMGMSG")
        . DO SHOWDIER^TMGDEBU2(.TMGMSG)
        ;"   -> TIU Notes
        WRITE !,"TRANSFERRING TIU NOTES TO ",$P(DESTPT,"^",2),!
        NEW TIUIEN SET TIUIEN=0
        FOR  SET TIUIEN=$ORDER(^TIU(8925,"C",SOURCEIEN,TIUIEN)) QUIT:TIUIEN'>0  DO 
        . WRITE " ->Transferring note #",TIUIEN," to ",$P(DESTPT,"^",2),!
        . KILL TMGFDA,TMGMSG
        . SET TMGFDA(8925,TIUIEN_",",.02)="`"_DESTIEN
        . DO FILE^DIE("E","TMGFDA","TMGMSG")
        . DO SHOWDIER^TMGDEBU2(.TMGMSG)
        ;"
        ;"   -> Lab results
        NEW SLRDFN,DLRDFN
        SET SLRDFN=+$GET(^DPT(SOURCEIEN,"LR"))
        IF SLRDFN'>0 WRITE "NO LAB ENTRIES TO MOVE",! GOTO CLEAN
        SET DLRDFN=+$GET(^DPT(DESTIEN,"LR"))
        IF DLRDFN'>0 WRITE "NO LAB DFN FOR ",$P(SOURCEPT,"^",2),! GOTO CLEAN
        WRITE !,"TRANSFERRING LAB RESULTS TO ",$P(DESTPT,"^",2),!
        MERGE ^LR(DLRDFN)=^LR(SLRDFN)
        ;"
        ;"Cleanup and request
CLEAN   WRITE !,"PATIENT TRANSFER IS NOW COMPLETE",!
        SET RESPONSE=""
ANS3    WRITE "WOULD YOU LIKE TO RENAME "_$P(SOURCEPT,"^",2)
        READ " TO A TEST PATIENT NAME? (Y/N) ",RESPONSE,!
        SET RESPONSE=$$UP^XLFSTR(RESPONSE)
        IF RESPONSE="N" WRITE "DONE." GOTO XPDn
        IF RESPONSE'="Y" WRITE "INVALID RESPONSE",! GOTO ANS3
        NEW TESTPTNAME SET TESTPTNAME="ZZTEST,"_$PIECE($PIECE(SOURCEPT,"^",2),",",2)
        NEW TESTPTSSN SET TESTPTSSN="123456789P"   ;"WILL THIS CAUSE AN ERROR? 
        WRITE "CHANGING ",$PIECE(SOURCEPT,"^",2)," TO ",TESTPTNAME,!
        KILL TMGFDA,TMGMSG
        SET TMGFDA(2,SOURCEIEN_",",.01)=TESTPTNAME
        SET TMGFDA(2,SOURCEIEN_",",.09)=TESTPTSSN
        DO FILE^DIE("E","TMGFDA","TMGMSG")
        DO SHOWDIER^TMGDEBU2(.TMGMSG)
        WRITE "DONE",!
XPDn    QUIT
        ;"
XFERTIU()    ;"TRANSFER ONE TIU NOTE FROM ONE PATIENT TO ANOTHER
        WRITE "*******************************************************",!
        WRITE "**                  WARNING!!!!                      **",!
        WRITE "** This function will completely transfer a TIU      **",!
        WRITE "** note from one patient to another.                 **",!
        WRITE "*******************************************************",!!
        NEW RESPONSE
ANS4    READ "DO YOU WANT TO CONTINUE? (Y/N) ",RESPONSE,!
        SET RESPONSE=$$UP^XLFSTR(RESPONSE)
        IF RESPONSE="N" GOTO XTIUDN
        IF RESPONSE'="Y"  WRITE "INVALID RESPONSE",! GOTO ANS4
        NEW SOURCENOTE,SOURCEPT,DESTPT
        ;"
        ;" Get SOURCE patient
        ;"WRITE "SELECT TIU NOTE"
        NEW DIC,X,Y
        SET DIC=8925
        SET DIC(0)="AMEQ"
        SET DIC("A")="ENTER PATIENT TO FIND NOTES: "
        DO ^DIC WRITE !
        IF +Y'>0 QUIT
        SET SOURCENOTE=Y
        WRITE "IEN ",Y,!
        ;"
        ;" Get DESTINATION patient
        WRITE "SELECT DESTINATION PATIENT"
        NEW DIC,X,Y
        SET DIC=2
        SET DIC(0)="MAEQ"
        DO ^DIC WRITE !
        IF +Y'>0 QUIT
        SET DESTPT=Y
        ;"
        ;" Verify
        SET RESPONSE=""
        WRITE !,"YOU ARE ABOUT TO TRANSFER THIS NOTE INTO ",$P(DESTPT,"^",2)
ANS5    READ ". CONTINUE? (Y/N) ",RESPONSE
        SET RESPONSE=$$UP^XLFSTR(RESPONSE)
        IF RESPONSE="N" GOTO XTIUDN
        IF RESPONSE'="Y" WRITE "INVALID RESPONSE",! GOTO ANS5
        ;"
        ;"Transfer
        NEW SOURCEIEN,DESTIEN,TMGFDA,TMGIEN,TMGMSG
        SET SOURCEIEN=$P(SOURCENOTE,"^",1)
        SET DESTIEN=$P(DESTPT,"^",1)
        ;"
        ;"-> Images
        WRITE !,"TRANSFERRING IMAGES TO ",$P(DESTPT,"^",2),!
        NEW IMGDA SET IMGDA=0
        FOR  SET IMGDA=$O(^TIU(8925.91,"ADI",SOURCEIEN,IMGDA)) QUIT:+IMGDA'>0  DO
        . WRITE " ->Transferring image #",IMGDA," to ",$P(DESTPT,"^",2),!
        . KILL TMGFDA,TMGMSG
        . SET TMGFDA(2005,IMGDA_",",.01)=$P(DESTPT,"^",2)_" "_$P($GET(^DPT(DESTIEN,0)),"^",9)
        . SET TMGFDA(2005,IMGDA_",",5)="`"_DESTIEN
        . DO FILE^DIE("E","TMGFDA","TMGMSG")
        . DO SHOWDIER^TMGDEBU2(.TMGMSG)
        ;"
        ;"-> TIU Notes
        WRITE !,"TRANSFERRING TIU NOTE TO ",$P(DESTPT,"^",2),!
        KILL TMGFDA,TMGMSG
        SET TMGFDA(8925,SOURCEIEN_",",.02)="`"_DESTIEN
        DO FILE^DIE("E","TMGFDA","TMGMSG")
        DO SHOWDIER^TMGDEBU2(.TMGMSG)
        ;"
XTIUDN        
        QUIT
        ;"
FIX36329
        NEW DATE,ARRAY SET DATE=0
        FOR  SET DATE=$ORDER(^OR(100,"AC","36329;DPT(",DATE)) QUIT:DATE'>0  DO
        . ;WRITE DATE,!
        . NEW IEN SET IEN=0
        . FOR  SET IEN=$ORDER(^OR(100,"AC","36329;DPT(",DATE,IEN)) QUIT:IEN'>0  DO
        . . ;WRITE " ",IEN,!
        . . NEW ANS,STATUS SET STATUS=$PIECE($GET(^OR(100,IEN,3)),"^",3)
        . . ;WRITE STATUS,!
        . . IF STATUS'=6 QUIT
        . . WRITE "CLEAR THIS ONE? ",$P($G(^OR(100,IEN,3)),"^",1),"  "
        . . READ ANS
        . . IF ANS="Y" DO
        . . . WRITE !,"WILL CLEAR",!
        . . . SET $P(^OR(100,IEN,3),"^",3)=1
        . . . SET ARRAY(IEN)=""
        . . ELSE  DO
        . . . WRITE !,"WON'T CLEAR",!
        QUIT
        ;"
FIXLAB  GOTO FIXLAB^TMGHL7U5   ;"Code moved 10/11/21 //kt
        ;
COMPORDR()  ;"This function completes any orders that are older than 18 months old
        NEW ORDERIEN SET ORDERIEN=0
        NEW ACTIVEIEN SET ACTIVEIEN=+$ORDER(^ORD(100.01,"B","ACTIVE",0))
        IF ACTIVEIEN'>0 WRITE "NO ACTIVE STATUS FOUND" QUIT
        NEW COMPLETEIEN SET COMPLETEIEN=+$ORDER(^ORD(100.01,"B","COMPLETE",0))
        IF COMPLETEIEN'>0 WRITE "NO COMPLETE STATUS FOUND" QUIT
        NEW ORDTYPE SET ORDTYPE="49;ORD(101.41,"
        FOR  SET ORDERIEN=$O(^OR(100,ORDERIEN)) QUIT:ORDERIEN'>0  DO
        . NEW STATUS SET STATUS=$PIECE($GET(^OR(100,ORDERIEN,3)),"^",3)
        . IF STATUS'=ACTIVEIEN QUIT
        . NEW THISTYPE SET THISTYPE=$P($G(^OR(100,ORDERIEN,0)),"^",5)
        . IF THISTYPE'=ORDTYPE QUIT
        . NEW STARTDATE
        . SET STARTDATE=$P($G(^OR(100,ORDERIEN,0)),"^",8)
        . NEW DAYSDIFF SET DAYSDIFF=$$DAYSDIFF^TMGDATE(STARTDATE,$$TODAY^TMGDATE)
        . IF DAYSDIFF<540 QUIT
        . NEW ORDDFN SET ORDDFN=+$PIECE($GET(^OR(100,ORDERIEN,0)),"^",2)
        . W "PATIENT: ",$P($G(^DPT(ORDDFN,0)),"^",1)
        . WRITE "  -  COMPLETING IEN ",ORDERIEN," WITH A DATE OF ",$$EXTDATE^TMGDATE(STARTDATE),!
        . SET ^TMG("COMPORDR","COMPLETED",ORDERIEN)=STATUS  ;"SAVE DATA HERE
        . SET $PIECE(^OR(100,ORDERIEN,3),"^",3)=COMPLETEIEN
        QUIT
        ;"
TIUNOPAT   ;"This function will find all TIU Documents that don't have
           ;"   patients assigned to them, separated by whether they have
           ;"   text or not
        NEW TIUIEN,COUNTWDAT,COUNTWODAT SET (TIUIEN,COUNTWDAT,COUNTWODAT)=0
        NEW TIUARRAYWDAT,TIUARRAYWODAT
        FOR  SET TIUIEN=$O(^TIU(8925,TIUIEN)) QUIT:TIUIEN'>0  DO
        . NEW PATIENT SET PATIENT=+$P($G(^TIU(8925,TIUIEN,0)),"^",2)       
        . IF PATIENT>0 QUIT
        . IF $D(^TIU(8925,TIUIEN,"TEXT")) DO  ;"these have text
        . . SET TIUARRAYWDAT(TIUIEN)=""
        . . SET COUNTWDAT=COUNTWDAT+1
        . . ZWR ^TIU(8925,TIUIEN,*)
        . ELSE  DO                            ;"these do not
        . . SET TIUARRAYWODAT(TIUIEN)=""
        . . SET COUNTWODAT=COUNTWODAT+1
        . . ZWR ^TIU(8925,TIUIEN,*)
        W COUNTWDAT_" DOCUMENTS FOUND WITHOUT PATIENTS - WITH NOTE TEXT",!
        W COUNTWODAT_" DOCUMENTS FOUND WITHOUT PATIENTS - WITH NO NOTE TEXT",!
        ;"
        ;"NOW PREPARE TO DELETE THESE NOTES
        SET TIUIEN=0
        FOR  SET TIUIEN=$O(TIUARRAYWDAT(TIUIEN)) QUIT:TIUIEN'>0  DO
        . NEW TMGFDA SET TMGFDA(8925,TIUIEN_",",.01)="@"
        . NEW TMGMSG
        . WRITE "DELETING ",TIUIEN,!
        . DO FILE^DIE("E","TMGFDA","TMGMSG")
        SET TIUIEN=0
        FOR  SET TIUIEN=$O(TIUARRAYWODAT(TIUIEN)) QUIT:TIUIEN'>0  DO
        . NEW TMGFDA SET TMGFDA(8925,TIUIEN_",",.01)="@"
        . NEW TMGMSG
        . WRITE "DELETING ",TIUIEN,!
        . DO FILE^DIE("E","TMGFDA","TMGMSG")
        QUIT
        ;"
TEST
       NEW IEN SET IEN=0
       NEW COUNT SET COUNT=0
       FOR  SET IEN=$O(^TIU(8925,IEN)) QUIT:COUNT>9  DO
       . SET COUNT=COUNT+1
       . IF COUNT=1 DO
       . . WRITE "WE HAVE A 1 HERE"
       . ELSE  IF COUNT=2 DO
       . . WRITE "WE HAVE A 2 HERE"
       . ELSE  DO
       . . WRITE "WE HAVE OTHER STUFF HERE"
       QUIT
