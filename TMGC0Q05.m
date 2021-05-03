TMGC0Q05 ;TMG/kst/TMG meanful use util code ;8/15/12, 2/2/14, 3/24/21
         ;;1.0;TMG-LIB;**1**;8/15/12
 ;
 ;"TMG C0Q FUNCTIONS
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
 ;"VIEWTTLS  --Browse / View TITLES and associated records.
 ;"GETLSTT(POUT,SDT,EDT) -- Generate a patient-document list for TOBACCO discussions
 ;"GETLSTO(POUT,SDT,EDT) -- Generate a patient-document list for OBESITY discussions
 ;"GETALIST(TESTFN,POUT,SDT,EDT) -- GET A PATIENT LIST (BY IEN FUNCTION)
 ;"GETXLIST(TESTFN,POUT,SDT,EDT) -- GET A XREF PATIENT LIST (BY TOPIC NAME FUNCTION)
 ;"INLST(TMGDFN,TESTFN,SDT,EDT) --Determine IF patient passes test
 ;"INLSTTOB(TMGDFN,SDT,EDT) --GET IF PATIENT IS IN 'LIST' OF TOBACCO DISCUSSIONS
 ;"INLSTOBE(TMGDFN,SDT,EDT) --GET IF PATIENT IS IN 'LIST' OF OBESITY DISCUSSIONS
 ;"INLSTHTN(TMGDFN,SDT,EDT) -- GET IF PATIENT IS IN 'LIST' OF HTN DISCUSSIONS
 ;"INLSTDM(TMGDFN,SDT,EDT) -- GET IF PATIENT IS IN 'LIST' OF DM DISCUSSIONS
 ;"FIXC0Q -- Used when transitioning from MU12 to MU13. Sets up NEW C0Q PATIENT LISTS, AND C0Q QUALITY MEASURE etc.
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"SHOW1(RECIEN) --Show 1 record from file 22719
 ;"ISTOBBAC(SECTION,TITLE) -- TEST FN to generate list for tobacco discussions
 ;"ISWT(SECTION,TITLE) -- TEST FN to generate list for weight discussions
 ;"ISHTN(SECTION,TITLE) -- TEST FN to generate list for hypertension discussions
 ;"ISDM(SECTION,TITLE) -- TEST FN to generate list for diabetes discussions
 ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"=======================================================================
 ;
 ;"=======================================================================
VIEWTTLS  ;"VIEW TITLES
        NEW TMGPICK,TMGRESULT,LIST2,SORTBY
        NEW TITLE,%,MENU
        WRITE !,!,"PROGRESS NOTE TOPIC VIEWER",!
        SET MENU(0)="Pick section to browse"
        SET MENU(1)="HISTORY OF PRESENT ILLNESS"_$CHAR(9)_"HPI"
        SET MENU(2)="ASSESSMENT & PLAN"_$CHAR(9)_"AP"
        NEW SECTION SET SECTION=$$MENU^TMGUSRI2(.MENU)
        IF SECTION="^" GOTO VDN
VT1     WRITE !,"Prepairing...."
        SET TITLE="" FOR  SET TITLE=$ORDER(^TMG(22719,SECTION,TITLE)) QUIT:TITLE=""  DO
        . SET TMGPICK(TITLE,0)=""
        KILL TMGRESULT
        DO SELECTR2^TMGUSRI3("TMGPICK","TMGRESULT","Pick title(s) to explore <Esc><Esc> when done.")
        KILL MENU
        SET MENU(0)="Select grouping of display"
        SET MENU(1)="Patient Name, Topic Title"_$CHAR(9)_"1"
        SET MENU(2)="Topic Title, Patient Name"_$CHAR(9)_"2"
        KILL SORTBY SET SORTBY=$$MENU^TMGUSRI2(.MENU)
        IF SORTBY="^" GOTO VDN
        KILL TMGPICK
        KILL LIST2
        SET TITLE=""
        FOR  SET TITLE=$ORDER(TMGRESULT(TITLE)) QUIT:TITLE=""  DO
        . NEW IEN SET IEN=0
        . FOR  SET IEN=$ORDER(^TMG(22719,SECTION,TITLE,IEN)) QUIT:(+IEN'>0)  DO
        . . NEW TIUIEN SET TIUIEN=$PIECE($GET(^TMG(22719,IEN,0)),"^",1)
        . . NEW TMGDFN SET TMGDFN=$PIECE($GET(^TIU(8925,TIUIEN,0)),"^",2)
        . . NEW PTNAME SET PTNAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
        . . NEW DISPSTR
        . . IF SORTBY=1 SET DISPSTR=PTNAME_" -- "_TITLE
        . . ELSE  SET DISPSTR=TITLE_" -- "_PTNAME
        . . SET TMGPICK(DISPSTR,0)=IEN_"^"_PTNAME
        . . SET LIST2(DISPSTR)=IEN_"^"_PTNAME
        KILL TMGRES
        IF $DATA(TMGPICK) DO
        . IF $$LISTCT^TMGMISC2("TMGPICK")=1 DO
        . . MERGE TMGRESULT=TMGPICK
        . ELSE  DO
        . . KILL TMGRESULT
        . . DO SELECTR2^TMGUSRI3("TMGPICK","TMGRESULT","Pick title(s) to explore <Esc><Esc> when done.")
        IF $DATA(TMGRESULT) DO
        . KILL TMGPICK
        . NEW TEMPTTL SET TEMPTTL=""
        . FOR  SET TEMPTTL=$ORDER(TMGRESULT(TEMPTTL)) QUIT:TEMPTTL=""  DO
        . . NEW IEN SET IEN=0
        . . FOR  SET IEN=$ORDER(LIST2(TEMPTTL,IEN)) QUIT:(+IEN'>0)  DO
        . . . NEW TIUIEN SET TIUIEN=$PIECE($GET(^TMG(22719,IEN,0)),"^",1)
        . . . NEW Y SET Y=$PIECE($GET(^TMG(22719,IEN,0)),"^",7)
        . . . DO DD^%DT NEW DT SET DT=Y
        . . . NEW NOTETTL SET NOTETTL=$$GET1^DIQ(8925,TIUIEN,.01)
        . . . NEW TMGDFN SET TMGDFN=$PIECE($GET(^TIU(8925,TIUIEN,0)),"^",2)
        . . . NEW PTNAME SET PTNAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
        . . . SET TMGPICK(PTNAME_" -- "_DT_" --  "_NOTETTL)=IEN
        IF $DATA(TMGPICK) DO
        . IF $$LISTCT^TMGMISC2("TMGPICK")=1 DO
        . . MERGE TMGRESULT=TMGPICK
        . ELSE  DO
        . . KILL TMGRESULT
        . . DO SELECTR2^TMGUSRI3("TMGPICK","TMGRESULT","Pick title(s) to explore <Esc><Esc> when done.")
        IF $DATA(TMGRESULT) DO
        . DO ZWRITE^TMGZWR("TMGRESULT")
        . NEW DONE SET DONE=0
        . NEW STR SET STR=""
        . FOR  SET STR=$ORDER(TMGRESULT(STR)) QUIT:(STR="")!DONE  DO
        . . NEW S2 SET S2=$GET(TMGRESULT(STR))
        . . NEW RECIEN SET RECIEN=+S2 QUIT:S2'>0
        . . NEW TIUIEN SET TIUIEN=$PIECE($GET(^TMG(22719,RECIEN,0)),"^",1)
        . . WRITE !,$$GET1^DIQ(8925,TIUIEN_",",.02),!
        . . WRITE $$GET1^DIQ(8925,TIUIEN_",",.01),"  ",$$GET1^DIQ(8925,TIUIEN_",",.07)," (#",TIUIEN,")",!
        . . DO DUMPREC^TMGDEBU3(22719,RECIEN)
        . . DO PRESS2GO^TMGUSRI2
        . . NEW % SET %=2
        . . WRITE "View full document" DO YN^DICN WRITE !
        . . IF %=-1 SET DONE=1 QUIT
        . . IF %=1 DO
        . . . DO DUMPREC^TMGDEBU3(8925,TIUIEN)
        . . . DO PRESS2GO^TMGUSRI2
        . . SET %=2
        . . WRITE "Re-parse document" DO YN^DICN WRITE !
        . . IF %=-1 SET DONE=1 QUIT
        . . IF %=1 DO
        . . . DO TRIGGER1^TMGC0Q04(TIUIEN)
        SET %=2
        WRITE "View more" DO YN^DICN WRITE !
        IF %=1 GOTO VT1
VDN     QUIT
        ;
SHOW1(RECIEN) ;"
        ;"Purpose:  Show 1 record from file 22719
        ;"Input: RECIEN -- IEN in 22719
        ;"FINISH!!!! C
        NEW TIUIEN SET TIUIEN=$PIECE($GET(^TMG(22719,RECIEN,0)),"^",1)
        WRITE !,$$GET1^DIQ(8925,TIUIEN_",",.02),!
        WRITE $$GET1^DIQ(8925,TIUIEN_",",.01),"  ",$$GET1^DIQ(8925,TIUIEN_",",.07)," (#",TIUIEN,")",!
        ;
        QUIT
        ;
GETALIST(TESTFN,POUT,SDT,EDT) ;"GET A PATIENT LIST (BY IEN FUNCTION)
        ;"Purpose: Get a patient list, based on TESTFN
        ;"Input: TESTFN -- This should be mumps code in the following  format:
        ;"            e.g. 'MYTEST^MYROUTINE'
        ;"            This function will be called like this:
        ;"               'SET TEST=$$MYTEST^MYROUTINE(RECIEN)'
        ;"            --Test should return 1 IF to include in patient list
        ;"            --Function must accept variable(s), as shown above
        ;"                RECIEN is the IEN in file 22719 being considered
        ;"       POUT -- PASS BY NAME. An OUT PARAMETER.  PRIOR DATA KILLED.
        ;"             Format:  @POUT@(TMGDFN,TIUIEN)=""
        ;"              TMGDFN=IEN IN PATIENT FILE
        ;"              TIUIEN=IEN IN FILE 8925
        ;"      SDT -- OPTIONAL. DATE IN FILEMAN FORMAT.  If provided, then
        ;"              entry only included IF linked document field .07 (EPISODE
        ;"              BEGIN DATE/TIME) IS >= SDT
        ;"      EDT -- OPTIONAL. DATE IN FILEMAN FORMAT.  If provided, then
        ;"              entry only included IF linked document field .07 (EPISODE
        ;"              BEGIN DATE/TIME) IS <= SDT
        ;"NOTE: this function differs from GETXLIST, in that this cycles through
        ;"       all records, whereas GETXLIST cycles through HPI and AP cross
        ;"       references
        ;"Results: none
        KILL @POUT
        SET SDT=+$GET(SDT)
        SET EDT=+$GET(EDT)
        NEW RECIEN SET RECIEN=0
        FOR  SET RECIEN=$ORDER(^TMG(22719,RECIEN)) QUIT:(+RECIEN'>0)  DO
        . NEW ERR SET ERR=0
        . NEW $ETRAP SET $ETRAP="SET $ETRAP="""",$ECODE="""",ERR=1"
        . NEW TEST
        . NEW FN SET FN="SET TEST=$$"_TESTFN_"("_RECIEN_")"
        . XECUTE FN
        . QUIT:ERR=1
        . IF TEST=0 QUIT
        . NEW TIUIEN SET TIUIEN=+$PIECE($GET(^TMG(22719,RECIEN,0)),"^",1)
        . QUIT:TIUIEN'>0
        . NEW DT SET DT=$PIECE($GET(^TIU(8925,TIUIEN,0)),"^",7)
        . IF (SDT>0),(DT<SDT) QUIT
        . IF (EDT>0),(DT>EDT) QUIT
        . NEW TMGDFN SET TMGDFN=$PIECE($GET(^TIU(8925,TIUIEN,0)),"^",2)
        . SET @POUT@(TMGDFN,TIUIEN)=""
        QUIT
        ;
GETXLIST(TESTFN,POUT,SDT,EDT) ;"GET A XREF PATIENT LIST (BY TOPIC NAME FUNCTION)
        ;"Purpose: Get a patient list, based on TESTFN
        ;"Input: TESTFN -- This should be mumps code in the following format:
        ;"            e.g. 'MYTEST2^MYROUTINE'
        ;"            This function will be called like this:
        ;"               'SET TEST=$$MYTEST2^MYROUTINE(SECTION,TOPICNAME)'
        ;"            --Test should return 1 IF to include in patient list
        ;"            --Function must accept variable(s), as shown above
        ;"                SECTION: will be "HPI" or "AP"
        ;"                TOPICNAME: the topic name to accept or reject
        ;"       POUT -- PASS BY NAME. An OUT PARAMETER.  PRIOR DATA KILLED.
        ;"             Format:  @POUT@(TMGDFN,TIUIEN)=""
        ;"              TMGDFN=IEN IN PATIENT FILE
        ;"              TIUIEN=IEN IN FILE 8925
        ;"      SDT -- OPTIONAL. DATE IN FILEMAN FORMAT.  If provided, then
        ;"              entry only included IF linked document field .07 (EPISODE
        ;"              BEGIN DATE/TIME) IS >= SDT
        ;"      EDT -- OPTIONAL. DATE IN FILEMAN FORMAT.  If provided, then
        ;"              entry only included IF linked document field .07 (EPISODE
        ;"              BEGIN DATE/TIME) IS <= SDT
        ;"NOTE: this function differs from GETALIST, in that this cycles through
        ;"       HPI and AP cross references, whereas GETALIST cycles through all
        ;"       records
        ;"Results: none
        NEW SECTION
        SET SDT=+$GET(SDT)
        SET EDT=+$GET(EDT)
        KILL @POUT
        FOR SECTION="HPI","AP" DO
        . NEW TITLE SET TITLE=""
        . FOR  SET TITLE=$ORDER(^TMG(22719,SECTION,TITLE)) QUIT:(TITLE="")  DO
        . . NEW ERR SET ERR=0
        . . NEW $ETRAP SET $ETRAP="SET $ETRAP="""",$ECODE="""",ERR=1"
        . . NEW TEST
        . . NEW TMGFN SET TMGFN="SET TEST=$$"_TESTFN_"("""_SECTION_""","""_TITLE_""")"
        . . XECUTE TMGFN
        . . QUIT:ERR=1
        . . IF TEST=0 QUIT
        . . NEW RECIEN SET RECIEN=0
        . . FOR  SET RECIEN=$ORDER(^TMG(22719,SECTION,TITLE,RECIEN)) QUIT:(+RECIEN'>0)  DO
        . . . NEW TIUIEN SET TIUIEN=+$PIECE($GET(^TMG(22719,RECIEN,0)),"^",1)
        . . . QUIT:TIUIEN'>0
        . . . NEW DT SET DT=$PIECE($GET(^TIU(8925,TIUIEN,0)),"^",7)
        . . . IF (SDT>0),(DT<SDT) QUIT
        . . . IF (EDT>0),(DT>EDT) QUIT
        . . . NEW TMGDFN SET TMGDFN=$PIECE($GET(^TIU(8925,TIUIEN,0)),"^",2)
        . . . SET @POUT@(TMGDFN,TIUIEN)=""
        QUIT
        ;
GETLSTT(POUT,SDT,EDT)  ;"GET LIST OF TOBACCO DISCUSSIONS
        ;"Purpose: Generate a patient-document list for TOBACCO discussions
        ;"       POUT -- PASS BY NAME. An OUT PARAMETER.  PRIOR DATA KILLED.
        ;"             Format:  @POUT@(TMGDFN,TIUIEN)=""
        ;"              TMGDFN=IEN IN PATIENT FILE
        ;"              TIUIEN=IEN IN FILE 8925
        ;"      SDT -- OPTIONAL. DATE IN FILEMAN FORMAT.  If provided, then
        ;"              entry only included IF linked document field .07 (EPISODE
        ;"              BEGIN DATE/TIME) IS >= SDT
        ;"      EDT -- OPTIONAL. DATE IN FILEMAN FORMAT.  If provided, then
        ;"              entry only included IF linked document field .07 (EPISODE
        ;"              BEGIN DATE/TIME) IS <= SDT
        ;"Results: none
        DO GETXLIST("ISTOBBAC^TMGC0Q05",POUT,.SDT,.EDT)
        QUIT
        ;
GETLSTO(POUT,SDT,EDT)  ;"GET LIST OF OBESITY DISCUSSIONS
        ;"Purpose: Generate a patient-document list for OBESITY discussions
        ;"       POUT -- PASS BY NAME. An OUT PARAMETER.  PRIOR DATA KILLED.
        ;"             Format:  @POUT@(TMGDFN,TIUIEN)=""
        ;"              TMGDFN=IEN IN PATIENT FILE
        ;"              TIUIEN=IEN IN FILE 8925
        ;"      SDT -- OPTIONAL. DATE IN FILEMAN FORMAT.  If provided, then
        ;"              entry only included IF linked document field .07 (EPISODE
        ;"              BEGIN DATE/TIME) IS >= SDT
        ;"      EDT -- OPTIONAL. DATE IN FILEMAN FORMAT.  If provided, then
        ;"              entry only included IF linked document field .07 (EPISODE
        ;"              BEGIN DATE/TIME) IS <= SDT
        ;"Results: none
        DO GETXLIST("ISWT^TMGC0Q05",POUT,.SDT,.EDT)
        QUIT
        ;
INLST(TMGDFN,TESTFN,SDT,EDT) ;"Determine IF patient passes test
        ;"Purpose: Cycle through entries in TMG TIU DOCUMENT TOPICS, and check for entry matches
        ;"Input: TESTFN -- This should be mumps code in the following format:
        ;"            e.g. 'MYTEST2^MYROUTINE'
        ;"            This function will be called like this:
        ;"               'SET TEST=$$MYTEST2^MYROUTINE(SECTION,TOPICNAME)'
        ;"            --Test should return 1 IF to include in patient list
        ;"            --Function must accept variable(s), as shown above
        ;"                SECTION: will be "HPI" or "AP"
        ;"                TOPICNAME: the topic name to accept or reject
        ;"      SDT -- OPTIONAL. DATE IN FILEMAN FORMAT.  If provided, then
        ;"              entry only included IF linked document field .07 (EPISODE
        ;"              BEGIN DATE/TIME) IS >= SDT
        ;"      EDT -- OPTIONAL. DATE IN FILEMAN FORMAT.  If provided, then
        ;"              entry only included IF linked document field .07 (EPISODE
        ;"              BEGIN DATE/TIME) IS <= SDT
        ;"Results: 1 IF patient passes test, 0 IF fails
        ;
        SET SDT=+$GET(SDT)
        SET EDT=+$GET(EDT)
        SET TMGDFN=$GET(TMGDFN)
        NEW FOUND SET FOUND=0
        NEW RECIEN SET RECIEN=0
        FOR  SET RECIEN=$ORDER(^TMG(22719,"DFN",TMGDFN,RECIEN)) QUIT:(+RECIEN'>0)!FOUND  DO
        . NEW TIUIEN SET TIUIEN=+$PIECE($GET(^TMG(22719,RECIEN,0)),"^",1)
        . QUIT:TIUIEN'>0
        . NEW DT SET DT=$PIECE($GET(^TIU(8925,TIUIEN,0)),"^",7)
        . IF (SDT>0),(DT<SDT) QUIT
        . IF (EDT>0),(DT>EDT) QUIT
        . NEW SUBNODE,SECTION
        . FOR SUBNODE=2,3 DO  QUIT:FOUND
        . . SET SECTION=$SELECT(SUBNODE=2:"HPI",1:"AP")
        . . NEW TITLE SET TITLE=""
        . . FOR  SET TITLE=$ORDER(^TMG(22719,RECIEN,SUBNODE,"B",TITLE)) QUIT:(TITLE="")!FOUND  DO
        . . . NEW ERR SET ERR=0
        . . . NEW $ETRAP SET $ETRAP="SET $ETRAP="""",$ECODE="""",ERR=1"
        . . . NEW TEST
        . . . NEW TMGFN SET TMGFN="SET FOUND=$$"_TESTFN_"("""_SECTION_""","""_TITLE_""")"
        . . . XECUTE TMGFN
        . . . IF ERR=1 SET FOUND=0
        QUIT FOUND
        ;
        ;"------------------------------------------------------------
        ;"------------------------------------------------------------
INLSTTOB(TMGDFN,SDT,EDT) ;"GET IF PATIENT IS IN 'LIST' OF TOBACCO DISCUSSIONS
        ;"Purpose: Rather than create a list and then query list, I can use
        ;"         cross reference to test patient directly.
        ;"Results: 1 IF patient in list (passes test), 0 otherwise
        QUIT $$INLST(TMGDFN,"ISTOBBAC^TMGC0Q05",.SDT,.EDT)
        ;
INLSTOBE(TMGDFN,SDT,EDT) ;"GET IF PATIENT IS IN 'LIST' OF OBESITY DISCUSSIONS
        ;"Purpose: Rather than create a list and then query list, I can use
        ;"         cross reference to test patient directly.
        ;"Results: 1 IF patient in list (passes test), 0 otherwise
        QUIT $$INLST(TMGDFN,"ISWT^TMGC0Q05",.SDT,.EDT)
        ;
INLSTHTN(TMGDFN,SDT,EDT) ;"GET IF PATIENT IS IN 'LIST' OF HTN DISCUSSIONS
        ;"Purpose: Rather than create a list and then query list, I can use
        ;"         cross reference to test patient directly.
        ;"Results: 1 IF patient in list (passes test), 0 otherwise
        QUIT $$INLST(TMGDFN,"ISHTN^TMGC0Q05",.SDT,.EDT)
        ;
INLSTDM(TMGDFN,SDT,EDT) ;"GET IF PATIENT IS IN 'LIST' OF DM DISCUSSIONS
        ;"NOTE:  See also PTHASDM^TMGPXR01
        ;"Purpose: Rather than create a list and then query list, I can use
        ;"         cross reference to test patient directly.
        ;"Results: 1 IF patient in list (passes test), 0 otherwise
        QUIT $$INLST(TMGDFN,"ISDM^TMGC0Q05",.SDT,.EDT)
        ;
        ;"------------------------------------------------------------
        ;"------------------------------------------------------------
ISTOBBAC(SECTION,TITLE) ;"TEST FN to generate list for tobacco discussions
        ;"Input:  SECTION: will be "HPI" or "AP" -- NOT USED
        ;"        TITLE: the topic name to accept or reject
        ;"Results: 1 IF to include, 0 to exclude
        NEW RESULT SET RESULT=1
        IF TITLE["TOBACCO" GOTO ITDN
        IF TITLE["SMOKING" GOTO ITDN
        SET RESULT=0
ITDN    QUIT RESULT
        ;
ISWT(SECTION,TITLE) ;"TEST FN to generate list for weight discussions
        ;"Input:  SECTION: will be "HPI" or "AP" -- NOT USED
        ;"        TITLE: the topic name to accept or reject
        ;"Results: 1 IF to include, 0 to exclude
        NEW RESULT SET RESULT=1
        IF TITLE["WEIGHT" GOTO IWDN
        IF TITLE["OBESITY" GOTO IWDN
        SET RESULT=0
IWDN    QUIT RESULT
        ;
ISHTN(SECTION,TITLE) ;"TEST FN to generate list for hypertension discussions
        ;"Input:  SECTION: will be "HPI" or "AP" -- NOT USED
        ;"        TITLE: the topic name to accept or reject
        ;"Results: 1 IF to include, 0 to exclude
        NEW RESULT SET RESULT=1
        IF TITLE["BLOOD PRESSURE" GOTO IHTN
        IF TITLE["HTN" GOTO IHTN
        IF TITLE["401." GOTO IHTN  ;"401.*
        IF TITLE["HYPERTENSION" GOTO IHTN
        SET RESULT=0
IHTN   QUIT RESULT
        ;
ISDM(SECTION,TITLE) ;"TEST FN to generate list for diabetes discussions
        ;"Input:  SECTION: will be "HPI" or "AP" -- NOT USED
        ;"        TITLE: the topic name to accept or reject
        ;"Results: 1 IF to include, 0 to exclude
        NEW RESULT SET RESULT=1
        IF TITLE["DIABET" GOTO IDMDN  ;"DIABETES, DIABETIC
        IF TITLE["250." GOTO IDMDN  ;"250.**
        IF $EXTRACT(TITLE,1,2)="DM" GOTO IDMDN  ;"DM I, DM II, DM., DM-1, DM-2 etc.
        SET RESULT=0
IDMDN   QUIT RESULT
        ;        
  ;"---------------------------------------------------
FIXC0Q ;
        ;"Purpose: Used when transitioning from MU12 to MU13.
        ;"    Sets up NEW C0Q PATIENT LISTS, AND C0Q QUALITY MEASURE etc.
        DO FIXC0Q1("MU12","MU13")
        QUIT
        ;
FIXC0Q1(OLDKEY,NEWKEY) ;
        NEW INIT 
        FOR INIT="kst","mdt" DO
        . NEW TAG SET TAG=INIT_" "_OLDKEY
        . NEW IEN SET IEN=0
        . FOR  SET IEN=$ORDER(^C0Q(101,IEN)) QUIT:(+IEN'>0)  DO
        . . SET ZN=$GET(^C0Q(101,IEN,0)) QUIT:ZN=""
        . . IF ZN'[TAG QUIT
        . . NEW NEWNAME SET NEWNAME=INIT_" "_NEWKEY_$PIECE(ZN,TAG,2)
        . . NEW NEWIEN SET NEWIEN=+$$GETIEN(101,NEWNAME)
        . . IF NEWIEN'>0 DO
        . . . NEW TMGFDA,TMGIEN,TMGMSG
        . . . SET TMGFDA(1130580001.101,"+1,",.01)=$PIECE(ZN,"^",1)
        . . . DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
        . . . IF $DATA(TMGMSG) DO ZWRITE^TMGZWR("TMGMSG") QUIT
        . . . SET NEWIEN=+$GET(TMGIEN(1))
        . . . IF NEWIEN'>0 DO  QUIT
        . . . . WRITE "CAN'T FIND NEW IEN!! ABORTING.",!
        . . IF NEWIEN'>0 QUIT
        . . SET ^C0Q(101,NEWIEN,5)=^C0Q(101,IEN,5)
        . . SET ^C0Q(101,NEWIEN,7)=^C0Q(101,IEN,7)
        . . SET $PIECE(^C0Q(101,NEWIEN,7),"^",6)=NEWKEY
        . . ;
        . . NEW N7 SET N7=$GET(^C0Q(101,NEWIEN,7))
        . . NEW NUMIEN SET NUMIEN=$PIECE(N7,"^",2)
        . . NEW NUMNAME SET NUMNAME=$PIECE($GET(^C0Q(301,NUMIEN,0)),"^",1)
        . . IF NUMNAME[OLDKEY DO
        . . . WRITE NUMNAME,!
        . . . NEW NEWNAME SET NEWNAME=$PIECE(NUMNAME,OLDKEY,1)_NEWKEY_$PIECE(NUMNAME,OLDKEY,2)
        . . . WRITE "--> ",NEWNAME,!
        . . . NEW IEN2 SET IEN2=+$ORDER(^C0Q(301,"B",$E(NEWNAME,1,30),0))
        . . . WRITE IEN2,!
        . . . IF IEN2>0 SET $PIECE(^C0Q(101,IEN,7),"^",2)=IEN2 QUIT
        . . . NEW TMGFDA,TMGIEN,TMGMSG
        . . . SET TMGFDA(1130580001.301,"+1,",.01)=NEWNAME
        . . . SET TMGFDA(1130580001.301,"+1,",999)=NEWNAME
        . . . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
        . . . IF $DATA(TMGMSG) DO ZWRITE^TMGZWR("TMGMSG") QUIT
        . . . SET IEN2=+$GET(TMGIEN(1))
        . . . IF IEN2'>0 DO  QUIT
        . . . . WRITE "CAN'T FIND NEWLY ADDED RECORD!!",!
        . . . IF IEN2>0 SET $PIECE(^C0Q(101,IEN,7),"^",3)=IEN2 QUIT
        . . NEW DENIEN SET DENIEN=$PIECE(N7,"^",3)
        . . NEW DENNAME SET DENNAME=$PIECE($GET(^C0Q(301,DENIEN,0)),"^",1)
        . . IF DENNAME[OLDKEY DO
        . . . WRITE DENNAME,!
        . . . NEW NEWNAME SET NEWNAME=$PIECE(DENNAME,OLDKEY,1)_NEWKEY_$PIECE(DENNAME,OLDKEY,2)
        . . . WRITE "--> ",NEWNAME,!
        . . . NEW IEN2 SET IEN2=+$ORDER(^C0Q(301,"B",$E(NEWNAME,1,30),0))
        . . . WRITE IEN2,!
        . . . IF IEN2>0 SET $PIECE(^C0Q(101,IEN,7),"^",3)=IEN2 QUIT
        . . . NEW TMGFDA,TMGIEN,TMGMSG
        . . . SET TMGFDA(1130580001.301,"+1,",.01)=NEWNAME
        . . . SET TMGFDA(1130580001.301,"+1,",999)=NEWNAME
        . . . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
        . . . IF $DATA(TMGMSG) DO ZWRITE^TMGZWR("TMGMSG") QUIT
        . . . SET IEN2=+$GET(TMGIEN(1))
        . . . IF IEN2'>0 DO  QUIT
        . . . . WRITE "CAN'T FIND NEWLY ADDED RECORD!!",!
        . . . IF IEN2>0 SET $PIECE(^C0Q(101,IEN,7),"^",3)=IEN2 QUIT
        . ;"now fix the C0Q MEASUREMENT SET
        . SET IEN=$$GTQMSIEN(INIT,OLDKEY)
        . IF IEN'>0 QUIT
        . NEW IEN2 SET IEN2=$$GTQMSIEN(INIT,NEWKEY)
        . IF IEN2'>0 QUIT
        . NEW MSRSUBIEN SET MSRSUBIEN=0
        . FOR  SET MSRSUBIEN=$ORDER(^C0Q(201,IEN,5,MSRSUBIEN)) QUIT:MSRSUBIEN'>0  DO
        . . NEW MSRIEN SET MSRIEN=+$PIECE($GET(^C0Q(201,IEN,5,MSRSUBIEN,0)),"^",1)
        . . IF MSRIEN'>0 QUIT
        . . NEW OLDNAME SET OLDNAME=$PIECE($GET(^C0Q(101,MSRIEN,0)),"^",1)
        . . WRITE OLDNAME,!
        . . NEW NEWNAME SET NEWNAME=$PIECE(OLDNAME,OLDKEY,1)_NEWKEY_$PIECE(OLDNAME,OLDKEY,2)
        . . WRITE "--> ",NEWNAME,!
        . . NEW NEWMSRIEN SET NEWMSRIEN=$$GETIEN(101,NEWNAME)
        . . IF NEWMSRIEN'>0 DO  QUIT
        . . . WRITE "ERROR: CANT'T FIND QUALITY MEASURE '",NEWNAME,"'... WHY???",!
        . . ;"Now see IF already stored in NEW QUALITY MEASURE SET.
        . . NEW NEWMSRSUBIEN SET NEWMSRSUBIEN=$ORDER(^C0Q(201,IEN2,5,"B",NEWMSRIEN,0))
        . . IF NEWMSRSUBIEN>0 QUIT  ;"ALREADY PRESENT
        . . NEW TMGFDA,TMGIEN,TMGMSG
        . . SET TMGFDA(1130580001.2011,"+1,"_IEN2_",",.01)=NEWMSRIEN
        . . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
        . . IF $DATA(TMGMSG) DO ZWRITE^TMGZWR("TMGMSG")
        QUIT
        ;
GTQMSIEN(INIT,KEY) ;
        NEW RESULT SET RESULT=0
        NEW IEN SET IEN=0
        FOR  SET IEN=$ORDER(^C0Q(201,IEN)) QUIT:(+IEN'>0)!(RESULT>0)  DO
        . NEW ANAME SET ANAME=$PIECE($GET(^C0Q(201,IEN,0)),"^",1)
        . IF (ANAME'[INIT)!(ANAME'[KEY) QUIT
        . SET RESULT=IEN
        IF RESULT'>0 DO
        . WRITE "UNABLE TO FIND C0Q QUALITY MEASURE SET FOR '",INIT,"' AND '",KEY,"'",! 
        QUIT RESULT
        ;
GETIEN(NUM,NAME) ;
        NEW RESULT SET RESULT=0
        NEW IEN SET IEN=0
        FOR  SET IEN=$ORDER(^C0Q(NUM,"B",$E(NAME,1,30),IEN)) QUIT:(+IEN'>0)!(RESULT>0)  DO
        . NEW ANAME SET ANAME=$PIECE($GET(^C0Q(NUM,IEN,0)),"^",1)
        . IF ANAME=NAME SET RESULT=IEN QUIT
        IF RESULT>0 GOTO GIEDN
        SET IEN=0
        FOR  SET IEN=$ORDER(^C0Q(NUM,IEN)) QUIT:(+IEN'>0)!(RESULT>0)  DO
        . NEW ANAME SET ANAME=$PIECE($GET(^C0Q(NUM,IEN,0)),"^",1)
        . IF ANAME=NAME SET RESULT=IEN QUIT
GIEDN   QUIT RESULT        
        ; 