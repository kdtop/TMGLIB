TMGSRVP        ; SLC/JER - RPCs for CREATE & UPDATE ;11/01/03, 2/2/14, 3/24/21
        ;;1.0;TEXT INTEGRATION UTILITIES;**1,7,19,28,47,89,104,100,115,109,167,113,112,175**;Jun 20, 1997
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;
 ;"Note: Code copied from TIUSRVP for local modifications.
 ;
MAKE(SUCCESS,TMGDFN,TITLE,VDT,VLOC,VSIT,TIUX,VSTR,SUPPRESS,NOASF)        ;" New Document
        ;" SUCCESS = (by ref) TIU DOCUMENT # (PTR to 8925)
        ;"         = 0^Explanatory message IF no SUCCESS
        ;" TMGDFN     = Patient (#2)
        ;" TITLE   = TIU Document Definition (#8925.1)
        ;" [VDT]   = Date(/Time) of Visit
        ;" [VLOC]  = Visit Location (HOSPITAL LOCATION)
        ;" [VSIT]  = Visit file ien (#9000010)
        ;" [VSTR]  = Visit string (i.e., VLOC;VDT;VTYPE)
        ;" [NOASF] = IF 1=Do Not Set ASAVE cross-reference
        ;" TIUX    = (by ref) array containing field data and document body
        ;
        N TIU,TIUDA,LDT,NEWREC
        S SUCCESS=0
        I +$G(VSIT) S VSTR=$$VSTRBLD(+VSIT)
        I $L($G(VSTR)) D
        . S VDT=$S(+$G(VDT):+$G(VDT),1:$P(VSTR,";",2))
        . S LDT=$S(+$G(VDT):$$FMADD^XLFDT(VDT,"","",1),1:"")
        . S VLOC=$S(+$G(VLOC):+$G(VLOC),1:$P(VSTR,";"))
        . ; If note is for Ward Location, call MAIN^TIUMOVE
        . I $P($G(^SC(+VLOC,0)),U,3)="W" D  Q
   . . DO MAIN^TIUMOVE(.TIU,TMGDFN,"",VDT,LDT,1,"LAST",0,+VLOC)
        . ;" Otherwise, call PATVADPT^TIULV
        . D PATVADPT^TIULV(.TIU,TMGDFN,"",VSTR)
        I '+$G(VSIT),'$L($G(VSTR)),+$G(VDT),+$G(VLOC) D
        . S VDT=$G(VDT),LDT=$S(+$G(VDT):$$FMADD^XLFDT(VDT,"","",1),1:"")
        . ;" If note is for Ward Location, call MAIN^TIUMOVE
        . I $P($G(^SC(+VLOC,0)),U,3)="W" D  Q
   . . DO MAIN^TIUMOVE(.TIU,TMGDFN,"",VDT,LDT,1,"LAST",0,+VLOC)
        . ;" Otherwise, call MAIN^TIUVSIT
        . D MAIN^TIUVSIT(.TIU,TMGDFN,"",VDT,LDT,"LAST",0,VLOC)
        I '+$G(TIU("VSTR")) D
        . D EVENT^TIUSRVP1(.TIU,TMGDFN)
        S TIU("INST")=$$DIVISION^TIULC1(+TIU("LOC"))
        I $S($D(TIU)'>9:1,+$G(TMGDFN)'>0:1,1:0) S SUCCESS="0^"_$$EZBLD^DIALOG(89250001) Q
        ;
        S TIUDA=$$GETREC(TMGDFN,.TIU,TITLE,.NEWREC)
        I +TIUDA'>0 S SUCCESS="0^"_$$EZBLD^DIALOG(89250002) Q
        S SUCCESS=+TIUDA
        ;
        D STUFREC^TIUSRVP1(+TIUDA,.TIUX,TMGDFN,,TITLE,.TIU)
        S:'+$G(NOASF) ^TIU(8925,"ASAVE",DUZ,TIUDA)=""
        K ^TIU(8925,+TIUDA,"TEMP")
        M ^TIU(8925,+TIUDA,"TEMP")=TIUX("TEXT") K TIUX("TEXT")
        D SETXT0(TIUDA)
        D FILE(.SUCCESS,+TIUDA,.TIUX,+$G(SUPPRESS))
        I +SUCCESS'>0 D DIK^TIURB2(TIUDA) Q
        I +$O(^TIU(8925,+TIUDA,"TEMP",0)) D MERGTEXT^TIUEDI1(+TIUDA,.TIU)
        I +$G(TIU("STOP")) D DEFER^TIUVSIT(TIUDA,TIU("STOP")) I 1
        E  D QUE^TIUPXAP1
        I '+$G(SUPPRESS) D
        . D RELEASE^TIUT(TIUDA,1)
        . D UPDTIRT^TIUDIRT(.TIU,TIUDA)
        K ^TIU(8925,+TIUDA,"TEMP")
        Q
 ;
VSTRBLD(VSIT)        ;" Given Visit ien, build Visit-Descriptor String
        N TIUY,VSIT0,VLOC,VDT,VSVCAT
        S VSIT0=$G(^AUPNVSIT(+VSIT,0)),VDT=+$P(VSIT0,U),VLOC=+$P(VSIT0,U,22)
        S VSVCAT=$P(VSIT0,U,7)
        S TIUY=VLOC_";"_VDT_";"_VSVCAT
        Q TIUY
 ;
SETXT0(TIUDA)        ;" Set root node of "TEMP" WP-field
        N TIUC,TIUI S (TIUC,TIUI)=0
        F  S TIUI=$O(^TIU(8925,TIUDA,"TEMP",TIUI)) Q:+TIUI'>0  D
        . S:$D(^TIU(8925,TIUDA,"TEMP",TIUI,0)) TIUC=TIUC+1
        S ^TIU(8925,TIUDA,"TEMP",0)="^^"_TIUC_U_TIUC_U_DT_"^^"
        Q
 ;
MAKEADD(TIUDADD,TIUDA,TIUX,SUPPRESS)        ;" Create addendum
   ;"Input: TIUADD -- OUT Parameter, for results
   ;"       TIUDA -- IEN of the parent document in file 8925
   ;"       TIUX -- Local input array containing the data to be filed for the
   ;"               addendum record, formatted as below.  It should look something like this:
   ;"          TIUX(.02)=45678
   ;"          TIUX(1301)=2960703.104556
   ;"          TIUX(1302)=293764
   ;"          TIUX("TEXT",1,0)="The patient is a 70 year old WHITE MALE, who presented to the ONCOLOGY CLINIC"
   ;"          TIUX("TEXT",2,0)="On JULY 3, 1996@10:00 AM, with the chief complaint of NECK PAIN..."
   ;"       SUPPRESS
   ;"Output: TIUADD returns IEN of created record, or error msg--
   ;"         -1^Could not create addendum.
   ;"          0^<some message>
 ;
        N DIE,DR,DA,DIC,X,Y,DLAYGO,TIUATYP,TIUCAN,TIUFPRIV,TIU
   S TIUFPRIV=1
        S TIUCAN=$$CANDO^TIULP(TIUDA,"MAKE ADDENDUM")
        I TIUCAN'>0 DO  Q
   . S TIUDADD="0^You may not MAKE AN ADDENDUM for this "
   . S TIUDADD=TIUDADD_$$STATUS^TIULC(TIUDA)_" "
   . S TIUDADD=TIUDADD_$$PNAME^TIULC1(+$G(^TIU(8925,+TIUDA,0)))
        S TIUATYP=+$$WHATITLE^TIUPUTU("ADDENDUM")
        S (DIC,DLAYGO)=8925,DIC(0)="L",X=""""_"`"_TIUATYP_""""
        D ^DIC
        S TIUDADD=+Y
        I +Y'>0 S TIUDADD=TIUDADD_"^Could not create addendum." Q
        D GETTIU^TIULD(.TIU,TIUDA)
        S TIU("DOCTYP")=TIUATYP_U_$$PNAME^TIULC1(TIUATYP)
        D STUFREC^TIUSRVP1(+TIUDADD,.TIUX,TMGDFN,+$G(TIUDA),TIUATYP,.TIU)
        K ^TIU(8925,+TIUDADD,"TEMP")
        M ^TIU(8925,+TIUDADD,"TEMP")=TIUX("TEXT") K TIUX("TEXT")
        D SETXT0(+TIUDADD)
        D FILE(.SUCCESS,+TIUDADD,.TIUX,+$G(SUPPRESS))
        I +SUCCESS'>0 D DIK^TIURB2(TIUDADD) Q
        I +$O(^TIU(8925,+TIUDADD,"TEMP",0)) D MERGTEXT^TIUEDI1(+TIUDADD,.TIU)
        I '+$G(SUPPRESS) D RELEASE^TIUT(+TIUDADD,1)
        K ^TIU(8925,+TIUDADD,"TEMP")
        Q
 ;
UPDATE(SUCCESS,TIUDA,TIUX,SUPPRESS)        ; Update existing Document
        N TIU,TIUI,TIUC,TIUD0,TIUD12,TIUD15,TIUCPF,TITLE
        I $S(+$G(TIUDA)'>0:1,'$D(^TIU(8925,+TIUDA,0)):1,1:0) D  Q
        . S SUCCESS="0^ Cannot update a non-existent document..."
        I +$P($G(^TIU(8925,+TIUDA,0)),U,5)>6 D  Q
        . S SUCCESS="0^ TIU Document #"_TIUDA_" is already signed..."
        I $D(TIUX("TEXT")) D
        . K ^TIU(8925,+TIUDA,"TEMP")
        . M ^TIU(8925,+TIUDA,"TEMP")=TIUX("TEXT")
        . S (TIUC,TIUI)=0
        . F  S TIUI=$O(^TIU(8925,+TIUDA,"TEMP",TIUI)) Q:+TIUI'>0  D
        . . S TIUC=TIUC+1
        . I +TIUC>0 S ^TIU(8925,+TIUDA,"TEMP",0)="^^"_TIUC_U_TIUC_U_DT_"^^"
        . K TIUX("TEXT")
        I +$O(TIUX(""))'>0 S:+$G(SUPPRESS) SUCCESS=+TIUDA Q
        S TIUD0=$G(^TIU(8925,TIUDA,0)),TIUD12=$G(^(12)),TITLE=+TIUD0
        ;Set a flag to indicate whether or not a Title is a member of the
        ;Clinical Procedures Class (1=Yes and 0=No)
        S TIUCPF=+$$ISA^TIULX(TITLE,+$$CLASS^TIUCP)
        D SETCOS(TIUDA,.TIUX,TIUD0,TIUD12)
        ; Title changed? Refile DC
        I +$G(TIUX(.01))>0,(+$G(TIUX(.01))'=+TIUD0) D
        . S TIUX(.04)=$$DOCCLASS^TIULC1(+$G(TIUX(.01)))
        D FILE(.SUCCESS,+TIUDA,.TIUX,+$G(SUPPRESS),TIUCPF)
        I +SUCCESS'>0 K ^TIU(8925,+TIUDA,"TEMP") Q
        D GETTIU^TIULD(.TIU,TIUDA)
        I $D(^TIU(8925,+TIUDA,"TEMP")) D
        . K ^TIU(8925,+TIUDA,"TEXT")
        . D MERGTEXT^TIUEDI1(+TIUDA,.TIU)
        . K ^TIU(8925,+TIUDA,"TEMP")
        . S:'+$G(SUCCESS) SUCCESS=+TIUDA
        ; If signed, re-file /ES/
        S TIUD15=$G(^TIU(8925,+TIUDA,15))
        I +TIUD15 D
        . N TIUBY,DR,DIE,DA,X,Y S TIUBY=$P(TIUD15,U,2) Q:+TIUBY'>0
        . S DR="1503///^S X=$$SIGNAME^TIULS("_TIUBY_");1504///^S X=$$SIGTITL^TIULS("_TIUBY_")"
        . S DA=TIUDA,DIE=8925 D ^DIE
        ; send alerts
        I '+$G(SUPPRESS) D
        . I +$P(TIUD0,U,5)<5,'$D(TIUX(.05)) D UPDSTAT(TIUDA,+$G(TIUD0))
        . D SEND^TIUALRT(TIUDA),SENDID^TIUALRT1(TIUDA):+$G(^TIU(8925,+TIUDA,21))
        . D UPDTIRT^TIUDIRT(.TIU,TIUDA)
        Q
 ;
SETCOS(TIUDA,TIUX,TIUD0,TIUD12)        ; SET cosig req
        N TIUDAD,TIUEXS,TIUNCS,TIUEXCS,TIURCS,TIUATT,TIUTTL,TIUDAD0
        S TIUEXS=$S(+$G(TIUX(1202)):+$G(TIUX(1202)),1:$P(TIUD12,U,4))
        S TIUNCS=$S(+$G(TIUX(1208)):+$G(TIUX(1208)),+$G(TIUX(1209)):+$G(TIUX(1209)),1:0)
        I TIUNCS S TIUX(1506)=$S(TIUNCS=TIUEXS:0,1:1) G SETCOSX
        S TIUEXCS=$P(TIUD12,U,8),TIUATT=$P(TIUD12,U,9)
        S TIUDAD=+$P(TIUD0,U,6),TIUDAD0=$G(^TIU(8925,+TIUDAD,0))
        I +$$ISDS^TIULX($S(+TIUDAD:+TIUDAD0,1:+TIUD0)) D  G SETCOSX
        . S TIUX(1506)=$S(TIUEXS=TIUEXCS:0,1:1)
        S TIUTTL=$S(+$G(TIUX(.01)):+$G(TIUX(.01)),1:+TIUD0)
        S TIUX(1506)=+$$REQCOSIG^TIULP(TIUTTL,TIUDA,TIUEXS)
SETCOSX        S:'TIUX(1506) TIUX(1208)="@"
        Q
 ;
UPDSTAT(DA,TITLE)        ; Update status on commit
        N DR,DIE S DR=".05////"_$$STATUS^TIUSRVP1(DA,0,TITLE)
        I '+$P($G(^TIU(8925,DA,13)),U,4) S DR=DR_";1304////^S X=$$NOW^XLFDT"
        S DIE=8925
        D ^DIE
        Q
 ;
GETREC(TMGDFN,TIU,TITLE,TIUNEW)        ; Get/create document record
        N DA,DIC,DIE,DLAYGO,DR,X,Y,TIUDPRM,TIUFPRIV,TIUHIT,TIUSCAT
        S (TIUHIT,DA)=0,TIUFPRIV=1
        S (DIC,DLAYGO)=8925,DIC(0)="FL"
        S X=""""_"`"_+TITLE_"""" D ^DIC K DIC("S")
        I +Y'>0 Q Y_U_" Insufficient data to create a NEW record."
        S DA=+Y,TIUNEW=+$P(Y,U,3)
        N DIE,DR,TIUVISIT S DIE=8925
        S TIUVISIT=$S(+$G(TIU("VISIT")):+$G(TIU("VISIT")),1:"")
        S TIUSCAT=$S(+$L($P($G(TIU("CAT")),U)):$P($G(TIU("CAT")),U),+$L($P($G(TIU("VSTR")),";",3)):$P($G(TIU("VSTR")),";",3),1:"")
        S DR=".04////"_$$DOCCLASS^TIULC1(+$P(Y,U,2))_";.13////"_TIUSCAT_";1205////"_$P($G(TIU("LOC")),U)_";1211////"_$P($G(TIU("VLOC")),U)_";1212////"_$P($G(TIU("INST")),U)
        D ^DIE
        Q +$G(DA)
 ;
FILE(SUCCESS,TIUDA,TIUX,SUPPRESS,TIUCPF)        ; Call FM Filer & commit
   ;"Purpose:
   ;"Input: SUCCESS -- Out paramater
   ;"       TIUDA -- IEN
   ;"       TIUX -- Array with text to file
   ;"       SUPPRESS -- IF 1 then suppress addditional processing
   ;"       TIUCPF
        N FDA,FDARR,IENS,FLAGS,TIUMSG,TIUCMMTX
        S IENS=""""_TIUDA_",""",FDARR="FDA(8925,"_IENS_")",FLAGS=""
        I +$G(TIUX(1202)) S TIUX(1204)=+$G(TIUX(1202))
        I +$G(TIUX(1209)) S TIUX(1208)=+$G(TIUX(1209))
        ;"If the document is a member of the Clinical Procedures Class, SET the
        ;"Entered By field to the Author/Dictator field
        I $G(TIUCPF),+$G(TIUX(1202)) S TIUX(1302)=+$G(TIUX(1202))
        M @FDARR=TIUX
        D FILE^DIE(FLAGS,"FDA","TIUMSG") ; File record
        I $D(TIUMSG)>9 S SUCCESS=0_U_$G(TIUMSG("DIERR",1,"TEXT",1)) Q
        S SUCCESS=TIUDA
        I '+$G(SUPPRESS) D
        . N DA
        . S DA=TIUDA
        . S TIUCMMTX=$$COMMIT^TIULC1(+$G(^TIU(8925,+TIUDA,0)))
        . I TIUCMMTX]"" X TIUCMMTX
        . K ^TIU(8925,"ASAVE",DUZ,TIUDA)
        Q
 ;
SIGN(ERR,TIUDA,TIUX)        ; API for /es/
        N X,TIUACT,TIUSIGN,TIUD0,TIUD12,TIUSTAT,SIGNER,COSIGNER,VALID,XTRASGNR
        N TIUES S ERR=0
        S TIUD0=$G(^TIU(8925,+TIUDA,0)),TIUD12=$G(^TIU(8925,+TIUDA,12))
        S SIGNER=$P(TIUD12,U,4),COSIGNER=$P(TIUD12,U,8)
        I (DUZ'=SIGNER),(DUZ'=COSIGNER) S XTRASGNR=+$O(^TIU(8925.7,"AE",+TIUDA,+DUZ,0))
        S TIUSTAT=+$P(TIUD0,U,5)
        S TIUACT=$S(TIUSTAT'>5:"SIGNATURE",+$G(XTRASGNR):"SIGNATURE",1:"COSIGNATURE")
        S TIUSIGN=$$CANDO^TIULP(TIUDA,TIUACT)
        I +TIUSIGN'>0 S ERR="89250004^"_$P(TIUSIGN,U,2) Q
        S VALID=$$VALIDATE($$DECRYP^XUSRB1(TIUX))
        I +VALID'>0 S ERR="89250005^"_$$EZBLD^DIALOG(89250005) Q
        S TIUES=1_U_$P($G(^VA(200,+DUZ,20)),U,2,3)
        I '+$G(XTRASGNR) D ES^TIURS(TIUDA,TIUES)
        I +$G(XTRASGNR) D ADDSIG^TIURS1(TIUDA,XTRASGNR)
        I +$G(^TIU(8925,TIUDA,21)),(TIUACT="SIGNATURE") D AUDLINK^TIUGR1(TIUDA,"a",+$G(^(21)))
        Q
 ;
VALIDATE(X)        ; Validate /es/-code
        N TIUY S TIUY=0
        D HASH^XUSHSHP I X]"",(X=$P($G(^VA(200,+DUZ,20)),U,4)) S TIUY=1
        Q TIUY
 ;
DELETE(ERR,TIUDA,TIURSN,OVRRIDE)        ; delete document
        N TIUDEL,TIUD0 S ERR=0
        I '+$G(OVRRIDE) D  Q:+$G(TIUDEL)'>0
        . S TIUDEL=$$CANDO^TIULP(TIUDA,"DELETE RECORD")
        . I TIUDEL'>0 S ERR="89250003^"_$$EZBLD^DIALOG(89250003)
        S TIUD0=$G(^TIU(8925,+TIUDA,0))
        I +$P(TIUD0,U,5)'<6 D  Q
        . S TIURSN=$G(TIURSN,"A")
        . D DELTEXT^TIURB2(TIUDA,TIURSN)
        D DIK^TIURB2(TIUDA)
        D DELAUDIT^TIUEDI1(TIUDA)
        Q
 ;
LOCK(ERR,TIUDA)        ; Bid for lock on a TIU Document record
        L +^TIU(8925,+TIUDA):1 I  S ERR=0
        E  S ERR="1^ Another session has this record locked."
        Q
 ;
UNLOCK(ERR,TIUDA)        ; Decrement Lock on a TIU Document record
        L -^TIU(8925,+TIUDA) S ERR=0
        Q
