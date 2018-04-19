TMGRPC4B ;TMG/kst/Support Functions for DxLink ;11/16/08, 2/2/14
         ;;1.0;TMG-LIB;**1**;11/16/08
 ;
 ;"TMG RPC FUNCTIONS for a DxLinkprogram
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
 ;"LOOKUPAT(TMGOUT,TMGPARAMS) -- find a patient that is already registered, using exact search
 ;"ENSURVST(TMGOUT,TMGPARAMS)--ensure that a Visit entry exists for appt info
 ;
 ;"=======================================================================
 ;"Dependencies:
 ;"  TMGRPC3*,TMGRPC4*
 ;"  TMGGDFN
 ;
 ;"=======================================================================
 ;
LOOKUPAT(TMGOUT,TMGPARAMS) ;"LOOKUP PATIENT
        ;"Purpose: To find a patient that is already registered, using exact search
        ;"Input: TMGOUT -- an OUT PARAMETER, PASS BY REFERENCE.
        ;"       TMGPARAMS -- LNAME,FNAME^DOB^SequelPMSAccount#
        ;"Output: TMGOUT is filled as follows:
        ;"        TMGOUT(0)="1^Success" or "-1^Message"
        ;"        TMGOUT(1)=DFN  (or 0 IF not found)
        ;"Results: None
 ;
        NEW TMGA,TMGDFN
        SET TMGA(.01)=$PIECE(TMGPARAMS,"^",1)
        IF TMGA(.01)[", " DO
        . NEW SPEC SET SPEC(", ")=","
        . SET TMGA(.01)=$$REPLACE^XLFSTR(TMGA(.01),.SPEC)
        SET TMGA(.03)=$PIECE(TMGPARAMS,"^",2)
        SET TMGA(22701)=$PIECE(TMGPARAMS,"^",3)
        SET TMGOUT(1)=$$GETDFN2^TMGGDFN(.TMGA,0)
        IF TMGOUT(1)>0 SET TMGOUT(0)="1^Success"
        ELSE  SET TEMGOUT(0)="-1^Patient not found: "_TMGPARAMS
        ;
        QUIT
 ;
ENSURVST(TMGOUT,TMGPARAMS) ;"ENSURE VISIT
        ;"Purpose: To ensure that a Visit entry exists for appt info
        ;"Input: TMGOUT -- an OUT PARAMETER, PASS BY REFERENCE.
        ;"       TMGPARAMS -- DFN^Date@Time^DurationMins^Reason^Location^Provider^Comments
        ;"         Notes: DFN -- patient IEN
        ;"                Date@Time -- time of appt
        ;"                DurationMinutes -- duration of appt.
        ;"                Reason -- text reason for purpose of appt.
        ;"                Location -- clinic name as stored in Fileman
        ;"                Provider -- Sequel ShortName for appt provider
        ;"                Comments -- misc free text.
        ;"Output: TMGOUT is filled as follows:
        ;"        TMGOUT(0)="1^Success" or "-1^Message"
        ;"        TMGOUT(1)=IEN   (or 0 IF not found)
        ;"Results: None
        ;"Note: I have added a custom XRef on the HOSPITAL LOCATION file, and a
        ;"      NEW field (22700, PMS NAME), that allows the location to be
        ;"      looked up by the name provided by the PMS.
        ;"      ** If this were to be used in another site, this would need to be
        ;"         addressed.  A value would need to be put into that 22700 field etc.
        ;
        SET TMGOUT(0)="1^Success"   ;"set default result
        SET TMGOUT(1)=0


        IF $DATA(^TMG("TMP","Killthis","ENSURVST")) DO  GOTO T2
        . SET TMGPARAMS=$GET(^TMG("TMP","Killthis","ENSURVST"))

        MERGE ^TMG("TMP","Killthis","ENSURVST")=TMGPARAMS
        GOTO EVSTDONE
 ;

T2
        NEW TMGDFN SET TMGDFN=+$PIECE(TMGPARAMS,"^",1)
        NEW TMGVDT SET TMGVDT=$PIECE(TMGPARAMS,"^",2)
        NEW TMGDUR SET TMGDUR=$PIECE(TMGPARAMS,"^",3)
        NEW TMGRSN SET TMGRSN=$PIECE(TMGPARAMS,"^",4)
        ;"Note: TMGLOC holds a Sequl Shortname.  Depends on added 'TMG' xref in file 4
        NEW TMGLOC SET TMGLOC=$PIECE(TMGPARAMS,"^",5)
        ;"NEW TMGDOC SET TMGDOC=$PIECE(TMGPARAMS,"^",6)
        NEW TMGCOM SET TMGCOM=$PIECE(TMGPARAMS,"^",7)

 ;
        IF TMGDFN'>0 DO  GOTO EVSTDONE
        . SET TMGOUT(0)="-1^Patient DFN > 0 not specified: "_TMGPARAMS
        NEW TMGDFNIH SET TMGDFNIH=TMGDFN ;"IEN's same in file 9000001 <--> 2
 ;
        ;"new TMGLIEN,DIC,X,Y
        ;"set DIC=4,DIC(0)="M",X=TMGLOC
        ;"do ^DIC

 ;
        SET TMGVDT=$TRANSLATE(TMGVDT," ","")
        NEW TMGFMDT,TMGMSG
        DO DT^DILF("R",TMGVDT,.TMGFMDT,,"TMGMSG")
        IF (+$GET(TMGFMDT)'>0)!$DATA(TMGMSG) DO  GOTO EVSTDONE
        . SET TMGOUT(0)="-1^Invalid Date/Time: "_TMGVDT
 ;
        ;"IF TMGDOC="" DO  GOTO EVSTDONE
        ;". SET TMGOUT(0)="-1^No provider specified: "_TMGPARAMS
        ;"NEW TMGIEN2 SET TMGIEN2=$ORDER(^VA(200,"TMG",TMGDOC,""))
        ;"IF TMGIEN2'>0 DO  GOTO EVSTDONE
        ;". SET TMGOUT(0)="-1^Unable to convert Sequel shortname '"_TMGDOC_"' to a VistA provider name"
        ;"SET TMGDOC=$PIECE($GET(^VA(200,TMGIEN2,0)),"^",1)
 ;
        NEW TMGFDA,TMGIEN,TMGMSG,TMGIENS
        ;"Look for existing visit
        NEW TMGI SET TMGI=""
        NEW TMGDONE SET TMGDONE=0
        FOR  SET TMGI=$ORDER(^AUPNVSIT("C",TMGDFNIH,TMGI)) QUIT:(+TMGI'>0)!TMGDONE  DO
        . NEW VDT SET VDT=$PIECE($GET(^AUPNVSIT(TMGI,0)),"^",1) ;"0;1=VISIT DATE/TIME
        . NEW X1,X2,X
        . SET X1=VDT,X2=TMGFMDT
        . DO ^%DTC  ;"Return difference in days between dates: X=X1-X2
        . IF X=0 DO  QUIT  ;"Later could DO a more strict compare, i.e. same TIME
        . . SET TMGDONE=1
        IF TMGI>0 DO
        . SET TMGIEN=TMGI
        . SET TMGIENS=TMGIEN_","
        ELSE  DO
        . SET TMGIEN=0
        . SET TMGIENS="+1,"
 ;
        SET TMGFDA(9000010,TMGIENS,.01)=TMGVDT       ;".01-VISIT/ADMIT DATE&TIME
        ;"SET TMGFDA(9000010,TMGIENS,.02)="NOW"        ;".02-DATE VISIT CREATED
        SET TMGFDA(9000010,TMGIENS,.03)="OTHER"      ;".03-TYPE
        SET TMGFDA(9000010,TMGIENS,.05)="`"_TMGDFNIH ;".05-PATIENT NAME
        SET TMGFDA(9000010,TMGIENS,.06)=TMGLOC       ;".06-LOC. OF ENCOUNTER
        SET TMGFDA(9000010,TMGIENS,.07)="AMBULATORY" ;".07-SERVICE CATEGORY
        SET TMGFDA(9000010,TMGIENS,.09)=1            ;".09-DEPENDENT ENTRY COUNT
        ;"SET TMGFDA(9000010,TMGIENS,.13)="NOW"        ;".13-DATE LAST MODIFIED
        SET TMGFDA(9000010,TMGIENS,.22)=TMGLOC       ;".22-HOSPITAL LOCATION
        SET TMGFDA(9000010,TMGIENS,.23)="`"_DUZ      ;".23-CREATED BY USER
        SET TMGFDA(9000010,TMGIENS,.24)="TMG RPC CONTEXT DXLINK" ;".24-OPTION USED TO CREATE
        SET TMGFDA(9000010,TMGIENS,15002)="OUT"      ;"15002-PATIENT STATUS IN/OUT
        SET TMGFDA(9000010,TMGIENS,15003)="PRIMARY"  ;"15003-ENCOUNTER TYPE
        SET TMGFDA(9000010,TMGIENS,81202)="TMG"      ;"81202-PACKAGE (or should value be 'PCE'?)
        SET TMGFDA(9000010,TMGIENS,81203)="TEXT INTEGRATION UTILITIES"
        ;"?? Add field:  15001-VISIT ID : 10GJ-TEST  <-- added by Visit Tracking
 ;
        IF TMGIEN=0 DO
        . ;"Add record and return a pointer to it.
        . KILL TMGIEN
        . DO UPDATE^DIE("ES","TMGFDA","TMGIEN","TMGMSG")
        . SET TMGOUT(1)=+$GET(TMGIEN(1))
        ELSE  DO
        . KILL TMGFDA(9000010,TMGIENS,.05)   ;"FM says: 'Can't be edited'
        . KILL TMGFDA(9000010,TMGIENS,81203) ;"FM says: 'Can't be edited'
        . ;"Store values provided in existing record
        . DO FILE^DIE("E","TMGFDA","TMGMSG")
        . SET TMGOUT(1)=TMGIEN
        IF $DATA(TMGMSG("DIERR")) DO
        . SET TMGOUT(0)="-1^See Fileman message"
        . SET TMGOUT(1)=$$GETERSTR^TMGRPC3G(.TMGMSG)
 ;
EVSTDONE ;
        QUIT
 ;
 ;
APPTLST(TMGOUT,TMGPARAMS) ;"APPT LIST
        ;"Purpose: Return a list of appts for given date.
        ;"Input: TMGOUT -- an OUT PARAMETER, PASS BY REFERENCE.
        ;"       TMGPARAMS -- Date^Location(Optional)
        ;"         Notes: Date -- date to look for dates.  In external (user-input) format
        ;"                Location -- clinic name as stored in Fileman
        ;"Output: TMGOUT is filled as follows:
        ;"        TMGOUT(0)="1^Success" or "-1^Message"
        ;"        TMGOUT(1)=(0 IF none found)
        ;"        TMGOUT(1)=DateTime^PatientName^DFN^DOB^SeqHRN^Location(Sequel ShortName)^CPTList^ICD9List
        ;"              CPTList format:  'Code#|CodeName;Code#|CodeName;Code#|CodeName...;'
        ;"              ICD9List format: 'Code#|CodeName;Code#|CodeName;Code#|CodeName...;'
        ;"Results: None
        ;"Note: I have added a custom XRef on the HOSPITAL LOCATION file, and a
        ;"      NEW field (22700, PMS NAME), that allows the location to be
        ;"      looked up by the name provided by the PMS.
        ;"      ** If this were to be used in another site, this would need to be
        ;"         addressed.  A value would need to be put into that 22700 field etc.
        ;
        SET TMGOUT(0)="1^Success"   ;"set default result
        SET TMGOUT(1)=0
 ;
        NEW TMGVDT SET TMGVDT=$PIECE(TMGPARAMS,"^",1)
        NEW TMGLOC SET TMGLOC=$PIECE(TMGPARAMS,"^",2)
 ;
        NEW TMGLIEN SET TMGLIEN=0
        IF TMGLOC="" GOTO AL2
        NEW X,Y,DIC
        SET DIC=44,DIC(0)="M",X=TMGLOC
        DO ^DIC
        IF Y'>0 DO  GOTO APLDONE
        . SET TMGOUT(0)="-1^Invalid location name: '"_TMGLOC_"'"
        SET TMGLIEN=+Y
AL2 ;
        SET TMGVDT=$TRANSLATE(TMGVDT," ","")
        NEW TMGFMDT,TMGMSG
        DO DT^DILF("X",TMGVDT,.TMGFMDT,,"TMGMSG")
        IF (+$GET(TMGFMDT)'>0)!$DATA(TMGMSG) DO  GOTO APLDONE
        . SET TMGOUT(0)="-1^Invalid Date/Time: "_TMGVDT
 ;
        NEW TMGARRAY,TMGS
        NEW TMGCOUNT SET TMGCOUNT=1
        NEW TMG1DT SET TMG1DT=TMGFMDT
        FOR  SET TMG1DT=$ORDER(^AUPNVSIT("B",TMG1DT)) Q:(TMG1DT'>0)!(TMG1DT>(TMGFMDT+1))  DO
        . NEW IEN SET IEN=""
        . FOR  SET IEN=$ORDER(^AUPNVSIT("B",TMG1DT,IEN)) Q:(IEN'>0)  DO
        . . NEW LOC SET LOC=+$P($G(^AUPNVSIT(IEN,0)),U,22)
        . . IF (TMGLIEN>0)&(LOC'=TMGLIEN) QUIT
        . . NEW DFN SET DFN=+$P($G(^AUPNVSIT(IEN,0)),U,5) Q:(DFN'>0)
        . . NEW Y SET Y=+$P($G(^AUPNVSIT(IEN,0)),U,1)
        . . DO DD^%DT
        . . SET TMGS=Y_"^"
        . . SET TMGS=TMGS_$P($G(^DPT(DFN,0)),U,1)_"^"_DFN_"^"
        . . SET Y=$P($G(^DPT(DFN,0)),U,3) DO DD^%DT SET TMGS=TMGS_Y_"^"
        . . NEW SHRN SET SHRN=$P($G(^DPT(DFN,"TMG")),U,2)
        . . SET TMGS=TMGS_SHRN_"^"
        . . SET TMGS=TMGS_$PIECE($GET(^SC(LOC,"TMG")),U,1)_"^" ;"Custom field 22700
        . . SET TMGS=TMGS_$$CPTLIST(IEN)_"^"
        . . SET TMGS=TMGS_$$ICDLIST(IEN)_"^"
        . . SET TMGOUT(TMGCOUNT)=TMGS
        . . SET TMGCOUNT=TMGCOUNT+1
 ;
APLDONE ;
        QUIT
 ;
CPTLIST(VSTIEN) ;
        ;"Purpose: To return a list of CPT's associated with given visit.
        ;"Input: VSTIEN -- IEN in VISIT file (9000010)
        ;"Results: 'Code#|CodeName;Code#|CodeName;Code#|CodeName...;' , or '' IF none found
        NEW RESULT SET RESULT=""
        NEW IEN SET IEN=""
        FOR  SET IEN=$ORDER(^AUPNVCPT("AD",VSTIEN,IEN)) QUIT:(IEN="")  DO
        . NEW CPTIEN SET CPTIEN=$P($G(^AUPNVCPT(IEN,0)),U,1)  ;"0;1=CPT Name
        . NEW CODESTR SET CODESTR=$P($G(^ICPT(CPTIEN,0)),U,1)
        . NEW DESCR SET DESCR=$P($G(^ICPT(CPTIEN,0)),U,2)
        . IF (CODESTR="")&(DESCR="") QUIT
        . SET RESULT=RESULT_CODESTR_"|"_DESCR_";"
        QUIT RESULT
 ;
ICDLIST(VSTIEN) ;
        ;"Purpose: To return a list of ICD9 codes associated with given visit.
        ;"Input: VSTIEN -- IEN in VISIT file (9000010)
        ;"Results: 'Code#|CodeName;Code#|CodeName;Code#|CodeName...;' , or '' IF none found
        NEW RESULT SET RESULT=""
        NEW IEN SET IEN=""
        FOR  SET IEN=$ORDER(^AUPNVPOV("AD",VSTIEN,IEN)) QUIT:(IEN="")  DO
        . NEW ICDIEN SET ICDIEN=$P($G(^AUPNVPOV(IEN,0)),U,1)  ;"0;1=POV
        . NEW CODESTR SET CODESTR=$P($G(^ICD9(ICDIEN,0)),U,1)
        . NEW DESCR SET DESCR=$P($G(^ICD9(ICDIEN,0)),U,3)
        . IF (CODESTR="")&(DESCR="") QUIT
        . SET RESULT=RESULT_CODESTR_"|"_DESCR_";"
        QUIT RESULT
