TMGRPC3D ;TMG/kst/Support Functions for GUI_Config ;6/23/11, 2/2/14
         ;;1.0;TMG-LIB;**1**;08/31/08
 ;
 ;"TMG RPC FUNCTIONS for a GUI config program
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
 ;" RPC -- Public Functions.
 ;"=======================================================================
 ;" <none>
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"QTERMUSR(XUDA) -- quietly inactive a given user
 ;"QTREAUSR(TMGDA) -- launch quiet reactivation code
 ;"GETSRLST(TMGOUT,TMGPARAMS) -- Get all .01 sub record entries for a subfile
 ;"CLONEUSR(TMGOUT,TMGPARAMS) -- replicate a record into a NEW record, including all subrecs.
 ;"CLONEREC(TMGOUT,TMGPARAMS) --- replicate a record into a NEW record, including all subrecs.
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;"Dependencies:
 ;"  TMGRPC3* only
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;
QTERMUSR(XUDA) ;"QUITELY TERMINATE USER
        ;"Purpose: to quietly inactive a given user
        ;"         I traced through menu option XUSERDEACT --> XUSTERM to create below
        ;"Input: XUDA -- the IEN in 200 of user to inactivate
        ;"Note:  based on code from XUSTERM
        ;"Results: 0 = OK, or 1^message IF error
        ;
        NEW TMGRESULT SET TMGRESULT=0
        NEW TMGFDA,TMGMSG,XUDT,DIC
        SET TMGFDA(200,+XUDA_",",9.2)="NOW"
        DO FILE^DIE("EK","TMGFDA","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO QTUDONE
        . SET TMGRESULT="1^"_$$GETERSTR^TMGRPC3G(.TMGMSG)_";"_$GET(TMGOUT(1))
        SET XUDT=$P(^VA(200,+XUDA,0),"^",11)  ;" field 9.2 termination date
        DO GET^XUSTERM  ;"load up info about mail, mailboxes, keys   ? needed ?
        SET ZTDTH=XUDT  ;"Task Start time. VA FileMan or $HOROLOG format
        SET ZTRTN="DQ1^XUSTERM1"  ;"routine to be fired
        SET ZTDESC="DEACTIVATE USER"  ;"description
        SET ZTSAVE("XUDA")=""    ;"save variable XUDA for use in task
        SET ZTIO=""    ;"no IO device needed
        DO ^%ZTLOAD  ;"que a taskman task
QTUDONE ;
        QUIT TMGRESULT
        ;
        ;
QTREAUSR(TMGDA) ;"QUITELY REACTIVE USER
        ;"Purpose: launch quiet reactivation code
        ;"         I traced through menu option XUSERREACT --> REACT^XUSERNEW to create below
        ;"Input: TMGDA -- the IEN in 200 to be reactivated
        ;"Results: 0 IF OK, or 1^ErrMsg
        ;"NOTE: when user is deleated through VistA code, the access and verify codes
        ;"      are deleted.  This function does NOT replace these.
        ;
        NEW TMGRESULT SET TMGRESULT=0
        NEW TMGFDA,TMGMSG,DIC,Y
        SET TMGFDA(200,TMGDA_",",9.2)="@" ;"Clear the Termination date
        DO FILE^DIE("EK","TMGFDA","TMGMSG")      ;"Post data
        IF $DATA(TMGMSG("DIERR")) DO  GOTO QRUDONE
        . SET TMGRESULT="1^"_$$GETERSTR^TMGRPC3G(.TMGMSG)
        ;
        KILL XMZ   ;" IF null, then user can access old mail.
        SET Y=TMGDA ;"set user to work on.
        DO NEW^XM  ;"mailman driver -- create a mailbox for user Y
        ;
        IF +$PIECE($GET(^VA(200,TMGDA,201)),"^",1)'>0 DO  ;"201;1 = PRIMARY MENU OPTION (#201)
        . SET TMGFDA(200,TMGDA_",",201)="EVE"  ;"set default primary menu to EVE (high level!)
        . DO FILE^DIE("EK","TMGFDA","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) DO
        . . SET TMGRESULT="1^"_$$GETERSTR^TMGRPC3G(.TMGMSG)
        IF +TMGRESULT>0 GOTO QRUDONE
        ;
        KILL XMDT,XMM,XMZ
        DO REACT^XQ84(TMGDA) ;"See IF this user's menu trees need to be rebuilt
QRUDONE ;
        QUIT TMGRESULT
        ;
        ;
GETSRLST(TMGOUT,TMGPARAMS) ;"GET SUB-RECS LIST
        ;"Purpose: Get all .01 sub record entries for a subfile
        ;"Input: TMGOUT -- an OUT PARAMETER, PASS BY REFERENCE.
        ;"       TMGPARAMS -- SubFileNum^ParentIENS^[FIELDS]^[IDENTIFIER]
        ;"              SubFileNum - Subfile Number to get list from.
        ;"              ParentIENS -- IENS for parent to get list from (e.g. "73,"
        ;"              FIELDS -- OPTIONAL, a Fields string to pass to LIST^DIC, for getting back values
        ;"              IDENTIFIER -- OPTIONAL, a string of mumps code, as per documentation at https://hardhats.org/fileman/pm/db_dic_l.htm
        ;"                         If this provided, it will overwrite default returned field value, and FIELDS param will be of no effect. 
        ;"                         IMPORTANT NOTE!!:  In identifier code, every '^' must be replaced with '%'.  (Because TMGPARAM is already using ^ as delimiter)
        ;"                                        Example.  Instead of passing 'DO GETSUBRECID^TMGTIUTE(1,2,3)', must pass 'DO GETSUBRECID%TMGTIUTE(1,2,3)'
        ;"                                        Also, every '%' will be converted to '^' before passing to LIST^DIC
        ;"Output: TMGOUT is filled as follows:
        ;"          TMGOUT(0)="1^Success^<Map>" or "-1^Message"
        ;"                <Map> gives the label name for each piece of the output.
        ;"                     e.g. 'IEN^IX(1)^,01^2I^WID(WRITE)'
        ;"          TMGOUT(1)=IEN^Value^... (return depends of fields requested.  Default is .01)
        ;"          TMGOUT(2)=IEN^Value^...
        ;"          ...
        ;"Results: none
        ;
        SET TMGOUT(0)="1^Success"
        NEW TMGSUBFILE SET TMGSUBFILE=+$PIECE(TMGPARAMS,"^",1)
        IF TMGSUBFILE'>0 DO  GOTO GSRLDONE
        . SET TMGOUT(0)="-1^No Subfile number supplied"
        NEW TMGPARENTIENS SET TMGPARENTIENS=$PIECE(TMGPARAMS,"^",2)
        IF TMGPARENTIENS="" DO  GOTO GSRLDONE
        . SET TMGOUT(0)="-1^No Parent IENS supplied"
        NEW TMGIENS SET TMGIENS=","_TMGPARENTIENS
        NEW TMGFLDS SET TMGFLDS=$PIECE(TMGPARAMS,"^",3)
        NEW TMGIDFY SET TMGIDFY=$TRANSLATE($PIECE(TMGPARAMS,"^",4),"%","^")        
        NEW TMGMSG,TMGERR
        ;
        DO LIST^DIC(TMGSUBFILE,TMGIENS,TMGFLDS,"PU","*",,,,,TMGIDFY,"TMGMSG","TMGERR")
        ;
        IF $DATA(TMGMSG("DIERR")) DO  GOTO GSRLDONE
        . SET TMGOUT(0)="-1^See Fileman message"
        . SET TMGOUT(1)=$$GETERSTR^TMGRPC3G(.TMGMSG)
        . SET TMGOUT(2)="Actual call was:"
        . SET TMGOUT(3)="DO LIST^DIC("""_TMGSUBFILE_""","""_TMGIENS_""","""_TMGFLDS_""",""PU"",""*"",,,,,"""_TMGIDFY_""",""TMGMSG"",""TMGERR"")"
        ;
        NEW TMGCOUNT SET TMGCOUNT=1
        NEW TMGI SET TMGI=0
        FOR  SET TMGI=$ORDER(TMGMSG("DILIST",TMGI)) QUIT:(+TMGI'>0)  DO
        . NEW TMGONEENTRY SET TMGONEENTRY=$GET(TMGMSG("DILIST",TMGI,0))
        . IF TMGONEENTRY="" QUIT
        . NEW TMGIEN SET TMGIEN=$PIECE(TMGONEENTRY,"^",1)
        . SET $PIECE(TMGONEENTRY,"^",1)=TMGIEN_","_TMGPARENTIENS
        . IF TMGIDFY'="" DO
        . . NEW P3 SET P3=$PIECE(TMGONEENTRY,"^",3) QUIT:P3=""
        . . SET $PIECE(TMGONEENTRY,"^",2,3)=P3
        . SET TMGOUT(TMGCOUNT)=TMGONEENTRY
        . SET TMGCOUNT=TMGCOUNT+1
        NEW MAP SET MAP=$GET(TMGMSG("DILIST",0,"MAP"))
        SET TMGOUT(0)=TMGOUT(0)_"^"_MAP
        ;
GSRLDONE ;
        QUIT
        ;
CLONEUSR(TMGOUT,TMGPARAMS) ;
        ;"Purpose: to replicate a record into a NEW record, including all
        ; subrecs.
        ;"         Implementation of "CLONE RECORD" request
        ;"Input: TMGOUT -- an OUT PARAMETER, PASS BY REFERENCE.
        ;"       TMGPARAMS -- SourceIENS^New.01Value
        ;"          FileNum - filename file that is to be worked in
        ;"          SourceIENS -- source IENS for NEW record, (i.e. '123,'
        ;not '123')
        ;"          New.01Value -- NEW value to be put into the .01 field
        ;(to distinguish it from the old record)
        ;"Output: TMGOUT is filled as follows:
        ;"          TMGOUT(0)="1^Success^NewIENS" or "-1^Message"
        ;"          TMGOUT(1)=Long Fileman message (if -1 error)
        ;
        ;LOCAL GOW 2/4/2012 SECONDARY OPTION COPY
        N SOURCEIEN
        S SOURCEIEN=$P(TMGPARAMS,"^",1)
        ;END LOCAL MOD
        SET TMGPARAMS="200^"_TMGPARAMS
        DO CLONEREC(.TMGOUT,TMGPARAMS)
        NEW TMGFDA,TMGMSG
        IF +TMGOUT(0)'=1 GOTO CUDONE
        NEW TMGNEWIEN
        SET TMGNEWIEN=+$PIECE(TMGOUT(0),"^",3)
        IF TMGNEWIEN="" DO  GOTO CUDONE
        . SET TMGOUT(0)="-1^Can't find IEN of added record"
        ;
        ;
        ;"Set NEW user to inactive (needs editing before it is active)
        NEW TMGDATA
        SET TMGDATA(0)="200^"_TMGNEWIEN_",^7^^YES"
        DO POSTDATA^TMGRPC3C(.TMGOUT,.TMGDATA,"E")
        IF +TMGOUT(0)=1 SET $PIECE(TMGOUT(0),"^",3)=TMGNEWIEN
        ;
        ;
        ;LOCAL GOW 2/4/12 SECONDARY OPTION COPY
        ;I $E(TMGOUT(1),1,1)="+" D
        ;.SET $PIECE(TMGOUT(0),"^",3)=$PIECE(TMGOUT(1),"^",2)
        ;.S TMGNEWIEN=$PIECE(TMGOUT(1),"^",2)
        ;TRANSFER SECONDARY OPTIONS SUBFILE ENTRIES
        H 5
        D EN1^TMGRPC3I(SOURCEIEN,TMGNEWIEN)
        ;END LOCAL MODS
        ;
        ;
CUDONE  ;
        QUIT
        ;
CLONEREC(TMGOUT,TMGPARAMS) ;
        ;"Purpose: to replicate a record into a NEW record, including all subrecs (to be implemented).
        ;"         Implementation of "CLONE RECORD" request
        ;"Input: TMGOUT -- an OUT PARAMETER, PASS BY REFERENCE.
        ;"       TMGPARAMS -- FileNum^SourceIENS^New.01Value
        ;"              FileNum - filename file that is to be worked in
        ;"              SourceIENS -- the source IENS for the NEW record, (i.e. 123,  not 123)
        ;"              New.01Value -- the NEW value to be put into the .01 field (to distinguish it from the old record)
        ;"Output: TMGOUT is filled as follows:
        ;"          TMGOUT(0)="1^Success^NewIENS" or "-1^Message"
        ;"          TMGOUT(1)=Long Fileman message (if -1 error)
        ;"NOTE: This hasn't been tested on subfiles yet...
        ;
        NEW TMGFILE,TMGIENS,TMGNEW01VALUE
        SET TMGOUT(0)="-1^Error"  ;"default to error
        ;
        SET TMGFILE=+$PIECE(TMGPARAMS,"^",1)
        IF TMGFILE'>0 DO  GOTO CRDONE
        . SET TMGOUT(0)="-1^No file number supplied"
        SET TMGIENS=$PIECE(TMGPARAMS,"^",2)
        IF TMGIENS="" DO  GOTO CRDONE
        . SET TMGOUT(0)="-1^IENS not supplied"
        IF +TMGIENS=TMGIENS DO
        . IF TMGIENS'["," SET TMGIENS=TMGIENS_","
        SET TMGNEW01VALUE=$PIECE(TMGPARAMS,"^",3)
        IF TMGNEW01VALUE="" DO  GOTO CRDONE
        . SET TMGOUT(0)="-1^No NEW value supplied for .01 value"
        ;
        NEW TMGARRAY,TMGMSG,TMGTEMP
        NEW TMGREF SET TMGREF="TMGARRAY"
        ;
        DO STUBNEWR^TMGRPC3E(TMGFILE,TMGNEW01VALUE,.TMGOUT)
        IF +TMGOUT(0)'=1 DO  GOTO CRDONE
        . SET $PIECE(TMGOUT(0),"^",2)="Unable to stub in NEW record. "_$PIECE(TMGOUT(0),"^",2)
        NEW TMGNEWIEN SET TMGNEWIEN=$PIECE(TMGOUT(0),"^",3)
        ;"Write "The IENS of the NEW record is: ",TMGNEWIEN,! ;"TEMP!!!
        ;
        ;"Note: Using a "X" flag is VERY slow, because it reindexes
        ;"      ALL XREFS for ALL entries in file (e.g. 60 seconds)
        ;"DO TRNMRG^TMGDIT("M",TMGFILE,TMGFILE,TMGIENS,TMGNEWIEN_",")  ;"DO actual record copy.
        DO TRNMRG^DIT("M",TMGFILE,TMGFILE,TMGIENS,TMGNEWIEN_",")  ;"do actual record copy.
        NEW DA,DIK
        SET DIK=$GET(^DIC(TMGFILE,0,"GL"))
        SET DA=TMGNEWIEN
        IF (+DA>0)&(DIK'="") DO IX1^DIK  ;"index just the NEW record
        ;
        ;"OK IF we got this far (can't check error from TRNMRG)
        SET TMGOUT(0)="1^Success^"_TMGNEWIEN
        ;
        ;"Now copy selected subfile records
        ;
CRDONE  QUIT
        ;
COPYSREC(PARENTFNUM,PARENTFLD,SRCIEN,DESTIEN) ;
        ;"Purpose: to copy all subfile file records in a parent record, and copy
        ;"         into a NEW **BLANK** parent record
        ;"NOTE: This function should NOT be used with any destination record
        ;"      that already has subfile records.  They will be deleted with
        ;"      direct global kills. This could leave dangling xref pointers
        ;"NOTE: This function has not been written to work with sub-sub-files.
        ;"      It will only work with first-level subfiles.
        ;"Input: PARENTFNUM -- this is NOT the subfile number.  It is the file
        ;"                   number of the PARENT file.
        ;"       PARENTFLD: this is the field (in the parent file) that
        ;"                     represents the subfile
        ;"       SRCIEN: This is the IEN of the *parent* record, in the parent file
        ;"       DESTIEN: This is the IEN of the destination record, in the parent
        ;"                file. This record must already exist.  This should be
        ;"                an empty record.   See NOTE above.
        ;
        ;"TO BE COMPLETED....
        ;
        QUIT