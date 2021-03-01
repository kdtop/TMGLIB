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
    . IF PATIENT'>0 QUIT
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
CNSLTALT
    ;"Purpose: Ensure all unsigned orders have proper alerts for authors
    ;"        *This should be ran as a nightly task
    NEW UNRLSDARR
    ;"Get unreleased orders
    DO ORDERS(.UNRLSDARR)
    NEW ORDIEN SET ORDIEN=0
    FOR  SET ORDIEN=$O(UNRLSDARR(ORDIEN)) QUIT:ORDIEN'>0  DO
    . NEW PROV SET PROV=+$G(UNRLSDARR(ORDIEN))
    . IF PROV'>0 QUIT
    . IF $$ORALTEXS(PROV,ORDIEN)=1 QUIT
    . WRITE "NEED ALERT FOR ",$P($G(^VA(200,PROV,0)),"^",1)," FOR ALERT ",ORDIEN,!
    . WRITE $G(^OR(100,ORDIEN,0)),!
    . ;"QUIT
    . NEW XQADATA,XQAID,XQAROU,XQA,XQAMSG,RESULT,XQADFN
    . SET XQA(PROV)=""
    . NEW DFN SET DFN=$P($P($G(^OR(100,ORDIEN,0)),"^",2),";",1)
    . NEW NAME SET NAME=$P($G(^DPT(DFN,0)),"^",1)
    . NEW DATE SET DATE=$P($G(^OR(100,ORDIEN,0)),"^",7)
    . SET XQAMSG=NAME_" (X1234): Order requires electronic signature"
    . SET XQADATA=ORDIEN_"@"
    . SET XQAID="OR,"_DFN_",12;"_PROV_";"_DATE
    . SET XQADFN=DFN
    . SET XQAROU="ESORD^ORB3FUP1"
    . SET RESULT=$$SETUP1^XQALERT
    QUIT
    ;"
ORDERS(RETARRAY)  
    NEW IDX SET IDX=300000
    NEW UNSIGNED SET UNSIGNED=11
    FOR  SET IDX=$O(^OR(100,IDX)) QUIT:IDX'>0  DO
    . NEW X0,X3,X8
    . SET X0=$G(^OR(100,IDX,0)),X3=$G(^OR(100,IDX,3)),X8=$G(^OR(100,IDX,8,1,0))
    . NEW STATUS,PROVIDER
    . SET STATUS=$P(X3,"^",3)
    . IF STATUS=UNSIGNED DO
    . . SET PROVIDER=+$P(X8,"^",5)
    . . IF PROVIDER'>0 SET PROVIDER=+$P(X8,"^",3)
    . . SET RETARRAY(IDX)=PROVIDER
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
ORALTEXS(DUZ,ORDIEN)  ;" DOES THE ORDER ALERT EXIST?
    ;"Purpose: to determine if the user has an alert for the note
    ;"Result: 1 if alert exists, 0 if alert needs to be created
    NEW TMGRESULT SET TMGRESULT=0
    NEW ALERTDT SET ALERTDT=0
    FOR  SET ALERTDT=$ORDER(^XTV(8992,DUZ,"XQA",ALERTDT)) QUIT:ALERTDT'>0  DO
    . NEW THISORDER SET THISORDER=+$PIECE($GET(^XTV(8992,DUZ,"XQA",ALERTDT,1)),"@",1)
    . IF THISORDER=ORDIEN SET TMGRESULT=1
    QUIT TMGRESULT
    ;"
PRINTRPT
ALERTRPT  ;"This is a report to display all alerts that are over 5 days
          ;"old
    NEW DFN SET DFN=0
    NEW DAYS SET DAYS="-5"
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
    WRITE "              ALL USER ALERTS OLDER THAN ",DAYS," DAYS OLD",!
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