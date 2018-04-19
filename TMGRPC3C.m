TMGRPC3C ;TMG/kst/Support Functions for GUI_Config ;08/31/08, 2/2/14
         ;;1.0;TMG-LIB;**1**;08/12/09
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
 ;"POSTDATA(TMGOUT,TMGDATA) -- Post Changes to database via Fileman
 ;
 ;"=======================================================================
 ;"Dependencies:
 ;"  TMGRPC3* only
 ;
 ;"=======================================================================
 ;
POSTDATA(TMGOUT,TMGDATA,TMGFLAG) ;
        ;"Post Changes to database via Fileman
        ;"Input: TMGOUT -- an OUT PARAMETER, PASS BY REFERENCE.
        ;"       TMGDATA -- Entries to be filed.  Format:
        ;"              TMGDATA(0)=FileNum^IENS^FieldNum^FieldName^newValue^oldValue
        ;"              TMGDATA(1)=FileNum^IENS^FieldNum^FieldName^newValue^oldValue
        ;"              TMGDATA(2)=FileNum^IENS^FieldNum^FieldName^newValue^oldValue
        ;"              ...
        ;"              Note: FieldName, oldValue pieces are not used (optional)
        ;"       TMGFLAG -- OPTIONAL, "E" is default.  Flag passed to FILE^DIE or UPDATE^DIE
        ;"                 should be "E" or "I"
        ;"Output: TMGOUT is filled as follows:
        ;"          TMGOUT(0)="1^Success" or "-1^Short Message"
        ;"          TMGOUT(1)=Fileman message (long)
        ;"          -or IF NEW records added:
        ;"          TMGOUT(1)=5^1234   ,c<--  results of IEN array returned (+5 converted to record 1234)
        ;"          TMGOUT(2)=3^2341   <--  results of IEN array returned (+3 converted to record 2341)
        ;"          ...
        ;"Results: none
 ;
        NEW TMGAVC SET TMGAVC=0  ;"TMGAVC=AccessVerifyCode. Default to no change.
        NEW TMGINACTUSER,TMGREACTUSER
        SET TMGFLAG=$GET(TMGFLAG,"E")
        NEW TMGI SET TMGI=""
        NEW TMGFDA,TMGNEWFDA,TMGMSG,TMGIEN,DIC
        NEW TMGOLDDUZ  ;"elh - to save previous fiel access level
        NEW TMGINTFDA
        FOR  SET TMGI=$ORDER(TMGDATA(TMGI)) QUIT:(TMGI="")  DO
        . NEW TMGFILE,TMGIENS,TMGFIELD,TMGVALUE,TMGONEENTRY
        . NEW TMGINTVALUE SET TMGINTVALUE=0
        . NEW TMGCONTINUE SET TMGCONTINUE=0
        . SET TMGONEENTRY=$GET(TMGDATA(TMGI))
        . SET TMGFILE=$PIECE(TMGONEENTRY,"^",1)
        . SET TMGIENS=$PIECE(TMGONEENTRY,"^",2)
        . SET TMGFIELD=$PIECE(TMGONEENTRY,"^",3)
        . SET TMGVALUE=$PIECE(TMGONEENTRY,"^",5)
        . IF TMGFILE=200 DO
        . . IF TMGFIELD=2 DO  ;"2 = Access Code
        . . . SET TMGOLDDUZ=DUZ(0)  ;"elh   Save File Access Level
        . . . SET DUZ(0)="^"    ;"elh   Set proper WRITE access level
        . . . IF TMGIENS["+" DO  QUIT
        . . . . SET TMGCONTINUE=1
        . . . . SET TMGOUT(1)="Can't SET Access code when first adding NEW user.  Try again."
        . . . ;"NOTICE: currently this code DOes NOT force code of certain length etc.
        . . . ;"S Y=$$VCHK(XV2,XUH) Q:Y Y  ;check for valid verify code, returns 0 (for OK), or 1^msg
        . . . SET TMGVALUE=$$UP^XLFSTR(TMGVALUE)  ;"access code must be upper case   elh
        . . . SET TMGVALUE=$$EN^XUSHSH(TMGVALUE)  ;"access code is supposed to be hashed first
        . . . SET TMGAVC=1  ;"signal change
        . . . SET TMGAVC("DA")=+TMGIENS
        . . . SET TMGINTVALUE=1
        . . ELSE  IF TMGFIELD=11 DO  ;"11 = Verify Code
        . . . SET TMGOLDDUZ=DUZ(0) ;" elh   Save File Access Level
        . . . SET DUZ(0)="^"  ;" elh   Set proper WRITE access level
        . . . IF TMGIENS["+" DO  QUIT
        . . . . SET TMGCONTINUE=1
        . . . . SET TMGOUT(1)="Can't SET Verify code when first adding NEW user.  Try again."
        . . . ;"NOTICE: currently this code does NOT force code of certain length etc.
        . . . ;"S Y=$$VCHK(XV2,XUH) Q:Y Y  ;check for valid verify code, returns 0 (for OK), or 1^msg
        . . . SET TMGVALUE=$$UP^XLFSTR(TMGVALUE) ;"verify code must be upper case   elh
        . . . SET TMGVALUE=$$EN^XUSHSH(TMGVALUE)  ;"verify code is supposed to be hashed first
        . . . SET TMGAVC=1  ;"signal change
        . . . SET TMGAVC("DA")=+TMGIENS
        . . . SET TMGINTVALUE=1
        . . ELSE  IF TMGFIELD=7 DO  ;"7 = DISUSER  Value should be 'YES' or 'NO'
        . . . IF TMGIENS["+" DO  QUIT
        . . . . SET TMGCONTINUE=1
        . . . . SET TMGOUT(1)="Can't SET DISUSER code when first adding NEW user.  Try again."
        . . . IF TMGVALUE="YES" DO
        . . . . SET TMGFDA(200,TMGIENS,9.2)="NOW" ;"add 9.2 = termination date
        . . . . SET TMGINACTUSER("DA")=+TMGIENS
        . . . IF (TMGVALUE="NO")!(TMGVALUE="@") DO
        . . . . SET TMGFDA(200,TMGIENS,9.2)="@" ;"delete 9.2 = termination date
        . . . . SET TMGFDA(200,TMGIENS,9.4)="@" ;"delete 9.4 = termination reason
        . . . . SET TMGREACTUSER("DA")=+TMGIENS
        . . ELSE  IF TMGFIELD=3 DO  ;"3 = FILE MANAGER ACCESS CODE  (i.e. @ etc)
        . . . IF TMGVALUE'["^" DO
        . . . . SET $PIECE(^VA(200,+TMGIENS,0),"^",4)=TMGVALUE  ;"force value in with low-level write
        . . . . SET TMGCONTINUE=1
        . IF TMGCONTINUE QUIT
        . IF TMGIENS["+" DO
        . . SET TMGNEWFDA(TMGFILE,TMGIENS,TMGFIELD)=TMGVALUE
        . ELSE  DO
        . . IF TMGINTVALUE=1 SET TMGINTFDA(TMGFILE,TMGIENS,TMGFIELD)=TMGVALUE
        . . ELSE  SET TMGFDA(TMGFILE,TMGIENS,TMGFIELD)=TMGVALUE
        . IF $DATA(TMGOLDDUZ) DO      ;"elh  reset file access
        . . SET DUZ(0)=TMGOLDDUZ
        . . KILL TMGOLDDUZ
 ;
        SET TMGOUT(0)="1^Success"  ;"default to success
        IF $DATA(TMGFDA) DO
        . DO FILE^DIE(TMGFLAG_"K","TMGFDA","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO
        . SET TMGOUT(0)="-1^See Fileman message re posting"
        . SET TMGOUT(1)=$$GETERSTR^TMGRPC3G(.TMGMSG)_";"_$GET(TMGOUT(1))
        . KILL TMGMSG
 ;
        IF $DATA(TMGINTFDA) DO  ;"process FDA with INTERNAL values
        . DO FILE^DIE("K","TMGINTFDA","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO
        . SET TMGOUT(0)="-1^See Fileman message re posting"
        . SET TMGOUT(1)=$$GETERSTR^TMGRPC3G(.TMGMSG)_";"_$GET(TMGOUT(1))
        . KILL TMGMSG
 ;
        IF $DATA(TMGNEWFDA) DO
        . DO UPDATE^DIE(TMGFLAG_"K","TMGNEWFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO
        . SET TMGOUT(0)="-1^See Fileman message re posting"
        . SET TMGOUT(1)=$$GETERSTR^TMGRPC3G(.TMGMSG)_";"_$GET(TMGOUT(1))
        . KILL TMGMSG
 ;
        IF ($PIECE(TMGOUT(0),"^",1)=1)&($DATA(TMGIEN)) DO
        . NEW TMGCOUNT SET TMGCOUNT=1
        . NEW TMGI SET TMGI=""
        . FOR  SET TMGI=$ORDER(TMGIEN(TMGI)) QUIT:(TMGI="")  DO
        . . SET TMGOUT(TMGCOUNT)=TMGI_"^"_$GET(TMGIEN(TMGI))
        . . SET TMGCOUNT=TMGCOUNT+1
 ;
        ;"If Access code / Verify code changed, then finish some follow up business
        ;"  as per BRCVC^XUS2<--CVC^XUSRB<--called by RPC 'XUS CVC'
        IF TMGAVC DO
        . NEW DA SET DA=TMGAVC("DA")
        . DO CALL^XUSERP(DA,2)  ;"Call for Kernel Create, **Update**, Disuser or Terminate events
        . ;"The above sets Taskman job -->DEQUE^XUSERP--> -->  D HL7^ALPBGEN
 ;
        IF $DATA(TMGINACTUSER) DO  ;"Finish up inactivating user with VistA code
        . NEW TMGRESULT
        . SET TMGRESULT=$$QTERMUSR^TMGRPC3D(TMGINACTUSER("DA"))  ;"finish termination of user code
        . IF +TMGRESULT'=0 DO
        . . SET TMGOUT(0)="-1^See Fileman message re terminating user"
        . . SET TMGOUT(1)=$GET(TMGOUT(1))_";"_TMGRESULT
 ;
        IF $DATA(TMGREACTUSER) DO  ;"finish up reactivating user with VistA code
        . NEW TMGRESULT
        . SET TMGRESULT=$$QTREAUSR^TMGRPC3D(TMGREACTUSER("DA")) ;"launch quiet reactivation code
        . IF +TMGRESULT'=0 DO
        . . SET TMGOUT(0)="-1^See Fileman message re reactivating user"
        . . SET TMGOUT(1)=$GET(TMGOUT(1))_";"_TMGRESULT
 ;
        QUIT
