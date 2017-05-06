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