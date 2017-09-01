TMGXQAL   ;TMG/ELH-RPCS FOR ALERT PROCESSES ; 3/2/17
         ;;1.0;TMG-LIB;**1,17**;3/2/17
         ;"
INFRMALT(TMGRESULT,DUZ,MESSAGE);
    ;"Purpose: This function creates an informational alert.
    ;"Input: TMGRESULT - Output:1^Success or -1^Error message
    ;"                   NOTE: SETUP^XQALERT doesn't return an error.
    ;"       DUZ - DFN of recipient
    ;"       MESSAGE - Alert message
    SET TMGRESULT="1^Success"  ;"Default
    NEW XQA,XQAARCH,XQADATA,XQAFLG,XQAGUID,XQAID,XQAMSG,XQAOPT,XQAROU,XQASUPV,XQASURO,XQATEXT,XQALERR
    SET XQA(DUZ)="" ; recipient is user `161
    SET XQAMSG=MESSAGE
    DO SETUP^XQALERT
    QUIT
    ;"
USIGNALT
    ;"Purpose: Ensure all unsigned notes have proper alerts for authors
    ;"         *This should be ran as a nightly task
    NEW UNSIGNEDARR
    ;"Get unsigned notes
    DO UNSIGNED^TMGTIUOJ(.UNSIGNEDARR)
    NEW NOTEIEN,PATIENT,AUTHOR
    SET NOTEIEN=0
    FOR  SET NOTEIEN=$ORDER(UNSIGNEDARR(NOTEIEN)) QUIT:NOTEIEN'>0  DO
    . SET PATIENT=+$ORDER(UNSIGNEDARR(NOTEIEN,0))
    . SET AUTHOR=+$ORDER(UNSIGNEDARR(NOTEIEN,PATIENT,0))
    . IF $$ALTEXIST(AUTHOR,NOTEIEN)=0 do
    . . DO SEND^TIUALRT(NOTEIEN)
    . . ;"WRITE "Note ",NOTEIEN," "
    . . ;"WRITE $PIECE($GET(^VA(200,AUTHOR,0)),"^",1)," - "
    . . ;"WRITE $PIECE($GET(^DPT(PATIENT,0)),"^",1)," needs an AUTHOR alert",!
    ;"Get note with unsigned co-signatures
    KILL UNSIGNEDARR
    DO ADDLSIGN^TMGTIUOJ(.UNSIGNEDARR)
    NEW NOTEIEN,PATIENT,AUTHOR
    SET NOTEIEN=0
    FOR  SET NOTEIEN=$ORDER(UNSIGNEDARR(NOTEIEN)) QUIT:NOTEIEN'>0  DO
    . SET PATIENT=+$ORDER(UNSIGNEDARR(NOTEIEN,0))
    . SET AUTHOR=+$ORDER(UNSIGNEDARR(NOTEIEN,PATIENT,0))
    . IF $$ALTEXIST(AUTHOR,NOTEIEN)=0 do
    . . DO SEND^TIUALRT(NOTEIEN)
    . . ;"WRITE "Note ",NOTEIEN," "
    . . ;"WRITE $PIECE($GET(^VA(200,AUTHOR,0)),"^",1)," - "
    . . ;"WRITE $PIECE($GET(^DPT(PATIENT,0)),"^",1)," needs an ADD'L SIGNER alert",!
    QUIT
    ;"
ALTEXIST(DUZ,TIUIEN)  ;"
    ;"Purpose: to determine if the user has an alert for the note
    ;"Result: 1 if alert exists, 0 if alert needs to be created
    NEW TMGRESULT SET TMGRESULT=0
    NEW ALERTDT SET ALERTDT=0
    FOR  SET ALERTDT=$ORDER(^XTV(8992,DUZ,"XQA",ALERTDT)) QUIT:ALERTDT'>0  DO
    . NEW THISTIU SET THISTIU=+$PIECE($GET(^XTV(8992,DUZ,"XQA",ALERTDT,1)),"^",1)   
    . IF THISTIU=TIUIEN SET TMGRESULT=1
    QUIT TMGRESULT
    ;"
PRINTRPT
ALERTRPT  ;"This is a report to display all alerts that are over 2 weeks
          ;"old
    NEW DFN SET DFN=0
    NEW DAYS SET DAYS="-14"
    NEW CUTOFFDATE SET CUTOFFDATE=$$ADDDAYS^TMGDATE(DAYS)
    NEW RETARRAY
    FOR  SET DFN=$O(^XTV(8992,DFN)) QUIT:DFN'>0  DO
    . NEW DATE SET DATE=0
    . FOR  SET DATE=$O(^XTV(8992,DFN,"XQA",DATE)) QUIT:DATE'>0  DO
    . . IF DATE>CUTOFFDATE QUIT
    . . NEW ZN SET ZN=$G(^XTV(8992,DFN,"XQA",DATE,0))
    . . SET RETARRAY(DFN,DATE)=$P(ZN,"^",3)
    . . ;"WRITE $$EXTDATE^TMGDATE(DATE)," - ",$P(ZN,"^",3),!
    IF '$D(RETARRAY) GOTO ARDN
    WRITE !
    WRITE "************************************************************",!
    WRITE "              ALL USER ALERTS OLDER THAN ",DAYS," OLD",!
    WRITE "                      Printed: ",$$TODAY^TMGDATE(1),!
    WRITE "           Please deliver this report to the OFFICE MANAGER",!
    WRITE "************************************************************",!
    WRITE "                                            (From TMGXQAL.m)",!!
    SET DFN=0
    FOR  SET DFN=$O(RETARRAY(DFN)) QUIT:DFN'>0  DO
    . WRITE "==== USER: ",$P($G(^VA(200,DFN,0)),"^",1)," ========",!
    . SET DATE=0
    . FOR  SET DATE=$O(RETARRAY(DFN,DATE)) QUIT:DATE'>0  DO
    . . WRITE $$EXTDATE^TMGDATE(DATE)," - ",$G(RETARRAY(DFN,DATE)),!
    . WRITE !!
ARDN
    QUIT