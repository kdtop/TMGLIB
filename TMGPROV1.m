TMGPROV1 ;TMG/kst-TMG PROVIDER FUNCTION ; 9/7/2018
         ;;1.0;TMG-LIB;**1,17**;9/7/2018
 ;
 ;"Eddie Hagood
 ;"Kevin Toppenberg MD
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 9/7/2018  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;"PUBLIC FUNCTIONS
 ;"=======================================================================
 ;"GETPROV(TMGRESULT,TMGDFN,TMGDUZ) -- Get the patient's provider (RPC: TMG CPRS GET CURRENT PROVIDER)
 ;"
 ;"=======================================================================
 ;"PRIVATE FUNCTIONS
 ;"=======================================================================
 ;"FINDPCP(TMGDFN)  ;"Find patient's PCP based on last 100 office notes.
 ;"AUTHRCNT(TMGDFN,AUTHORARR) ;"Find last 100 notes for patient, and get total for each author
 ;"
 ;"=======================================================================
 ;"Dependancies :
 ;"=======================================================================
GETPROV(TMGRESULT,TMGDFN,TMGDUZ) ;"-- Get the patient's current provider (RPC: TMG CPRS GET CURRENT PROVIDER)
 ;"Purpose: This function will return the patient's provider to be set in the
 ;"         encounter object and displayed in the pnlVisit of frmFrame.
 ;"Input: TMGRESULT - (RPC Output) IEN of user (New Person file)
 ;"       TMGDFN - IEN of patient (Patient file)
 ;"       TMGDUZ - IEN of current user (New Person file)
 ;"Output: TMGRESULT should be the IEN of the provider
 ;"Result: none
 SET TMGDFN=+$G(TMGDFN),TMGDUZ=+$G(TMGDUZ)
 IF (TMGDFN'>0)!(TMGDUZ'>0) DO  GOTO GPDN
 . SET TMGRESULT=DUZ
 ;"Here we will determine if a PCP was determined previously
 ;"  and if so, use if it is was set in the last 30 days 3/3/20
 NEW SAVEDPCP,SAVEDDATE
 SET SAVEDPCP=+$P($G(^DPT(TMGDFN,"TMGPCP")),"^",1)
 SET SAVEDDATE=+$P($G(^DPT(TMGDFN,"TMGPCP")),"^",2) 
 IF (SAVEDPCP>0)&(SAVEDDATE>$$ADDDAYS^TMGDATE(-30)) DO  GOTO GPDN
 . SET TMGRESULT=SAVEDPCP
 SET TMGRESULT=$$FINDPCP(TMGDFN)
 IF TMGRESULT="-1" DO
 . SET TMGRESULT=TMGDUZ
 ELSE  DO
 . ;"save the provider here  3/3/20
 . NEW TMGFDA,TMGMSG
 . SET TMGFDA(2,TMGDFN_",",22707)="`"_TMGRESULT
 . SET TMGFDA(2,TMGDFN_",",22707.1)=$$TODAY^TMGDATE
 . DO FILE^DIE("E","TMGFDA","TMGMSG")
GPDN
 QUIT
 ;"
FINDPCP(TMGDFN)  ;"Find patient's PCP based on last X office notes.
 NEW PCPIEN 
 NEW AUTHORARR
 DO AUTHRCNT(TMGDFN,.AUTHORARR,100)  ;"Get count of last 100
 SET PCPIEN=+$G(AUTHORARR("PCENT",+$O(AUTHORARR("PCENT",101),-1)))
 IF PCPIEN'>0 SET PCPIEN="-1" 
 KILL AUTHORARR
 ;"NOTE: Since last 10 always overrides evaluation of 100, perhaps 100 shouldn't be
 ;"      done.... tweak logic later...
 DO AUTHRCNT(TMGDFN,.AUTHORARR,10)  ;"Get count of last 10
 NEW PCPIEN2,PCENT
 SET PCENT=+$O(AUTHORARR("PCENT",101.99),-1)
 SET PCPIEN2=+$G(AUTHORARR("PCENT",PCENT))
 ;"SET PCPIEN2=+$G(AUTHORARR("PCENT",+$O(AUTHORARR("PCENT","A"),-1)))
 IF PCPIEN2'>0 SET PCPIEN2="-1"
 IF PCPIEN2'=PCPIEN SET PCPIEN=PCPIEN2
 QUIT PCPIEN
 ;"
AUTHRCNT(TMGDFN,AUTHORARR,TARGETCT) ;"Find last 100 notes for patient, and get total for each author
 ;"Output: AUTHORARR(DUZ,"COUNT")=TOTAL NUM OF NOTES
 ;"        AUTHORARR("PERCENT",DUZ)=% of notes
 ;"        AUTHORARR("PCENT",%)=DUZ  <- will be used in FINDPCP
 NEW OVCOUNT SET OVCOUNT=0
 NEW OVIEN,PCEIEN,TMIEN  ;"WE ARE HARD CODING THESE IENS FOR USE IN TESTING FOR NOW 11/12/18
 SET OVIEN=1408,PCEIEN=1402,TMIEN=2012
 SET TARGETCT=$GET(TARGETCT,100)
 NEW TIUIEN SET TIUIEN=9999999
 FOR  SET TIUIEN=$O(^TIU(8925,"C",TMGDFN,TIUIEN),-1) QUIT:(TIUIEN'>0)!(OVCOUNT>TARGETCT)  DO
 . NEW ZN SET ZN=$G(^TIU(8925,TIUIEN,0))
 . NEW TIUTYPE SET TIUTYPE=$P(ZN,"^",1)
 . ;"NEW TIUHLIGHT SET TIUHLIGHT=$G(^TIU(8925.1,TIUTYPE,"TMGH"))
 . ;"IF TIUHLIGHT'="Y" QUIT
 . ;"NO LONGER TEST HIGHLIGHT. NOW TEST FOR ONLY OV AND PCE
 . ;"4/4/19 test date. if older than 5 years don't use
 . NEW DATE SET DATE=$P(ZN,"^",7)
 . IF $$DAYSDIFF^TMGDATE(DATE,$$TODAY^TMGDATE)>1825 QUIT
 . ;"END 4/4/19 CHANGE
 . IF (TIUTYPE'=OVIEN)&(TIUTYPE'=PCEIEN)&(TIUTYPE'=TMIEN) QUIT
 . SET OVCOUNT=OVCOUNT+1
 . NEW AUTHORIEN SET AUTHORIEN=$P($G(^TIU(8925,TIUIEN,12)),"^",2)
 . SET AUTHORARR(AUTHORIEN,"COUNT")=+$G(AUTHORARR(AUTHORIEN,"COUNT"))+1
 . ;"NEW PCENT SET PCENT=($G(AUTHORARR(AUTHORIEN,"COUNT"))*100)/OVCOUNT
 . ;"SET AUTHORARR("PERCENT",AUTHORIEN)=PCENT
 NEW AUTHORIEN SET AUTHORIEN=0
 FOR  SET AUTHORIEN=$O(AUTHORARR(AUTHORIEN)) QUIT:+AUTHORIEN'>0  DO
 . NEW COUNT SET COUNT=$G(AUTHORARR(AUTHORIEN,"COUNT"))
 . NEW PCENT SET PCENT=($G(AUTHORARR(AUTHORIEN,"COUNT"))*100)/OVCOUNT
 . SET AUTHORARR("PERCENT",AUTHORIEN)=PCENT
 SET AUTHORIEN=0
 FOR  SET AUTHORIEN=$O(AUTHORARR("PERCENT",AUTHORIEN)) QUIT:AUTHORIEN'>0  DO
 . NEW PCENT SET PCENT=$J($G(AUTHORARR("PERCENT",AUTHORIEN)),"",0) 
 . SET AUTHORARR("PCENT",PCENT)=AUTHORIEN 
 QUIT
 ;"
TESTPROV
 NEW X,Y,DIC SET DIC(0)="MAEQ",DIC=2
 DO ^DIC QUIT:+Y'>0
 NEW RESULT
 DO GETPROV(.RESULT,+Y,DUZ)
 WRITE !,!,"Patient ",$P($G(^DPT(+Y,0)),"^",1)," returns with ",$P($G(^VA(200,RESULT,0)),"^",1)
 WRITE " as their PCP",!
 QUIT
 ;"
GETPROVN(TMGRESULT,TMGDFN,TMGDUZ) ;"-- Get the patient's current provider (RPC: TMG SCANNER GET CURRENT PROVIDER)
 ;"Purpose: This function will return the patient's provider to be set in
 ;the
 ;"         encounter object and displayed in the pnlVisit of frmFrame.
 ;"Input: TMGRESULT - (RPC Output) IEN of user (New Person file)
 ;"       TMGDFN - IEN of patient (Patient file)
 ;"       TMGDUZ - IEN of current user (New Person file)
 ;"Output: TMGRESULT should be the IEN of the provider
 ;"Result: none
 SET TMGDFN=+$G(TMGDFN),TMGDUZ=+$G(TMGDUZ)
 IF (TMGDFN'>0)!(TMGDUZ'>0) DO  GOTO GPNDN
 . SET TMGRESULT=0
 ;"Here we will determine if a PCP was determined previously
 ;"  and if so, use if it is was set in the last 30 days 3/3/20
 NEW SAVEDPCP,SAVEDDATE
 SET SAVEDPCP=+$P($G(^DPT(TMGDFN,"TMGPCP")),"^",1)
 SET SAVEDDATE=+$P($G(^DPT(TMGDFN,"TMGPCP")),"^",2)
 IF (SAVEDPCP>0)&(SAVEDDATE>$$ADDDAYS^TMGDATE(-30)) DO  GOTO GPNDN
 . SET TMGRESULT=SAVEDPCP
 SET TMGRESULT=$$FINDPCP(TMGDFN)
 IF TMGRESULT="-1" DO
 . SET TMGRESULT=0
 ELSE  DO
 . ;"save the provider here  3/3/20
 . NEW TMGFDA,TMGMSG
 . SET TMGFDA(2,TMGDFN_",",22707)="`"_TMGRESULT
 . SET TMGFDA(2,TMGDFN_",",22707.1)=$$TODAY^TMGDATE
 . DO FILE^DIE("E","TMGFDA","TMGMSG")
GPNDN
 IF TMGRESULT>0 DO
 . SET TMGRESULT=$P($G(^VA(200,TMGRESULT,0)),"^",1)
 ELSE  DO
 . SET TMGRESULT="NO PCP DEFINED"
 QUIT
 ;"
