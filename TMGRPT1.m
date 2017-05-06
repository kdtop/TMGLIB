TMGRPT1 ;TMG/kst/Misc Reports;8/14/13, 2/2/14, 10/12/16
         ;;1.0;TMG-LIB;**1**;05/01/09

 ;"TMG MISCELLANEOUS FUNCTIONS
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
 ;"ASKDEVRPT -- interactive entry point for report, asking device.
 ;"DEVMAMRPT -- interactive entry point for report, asking device.
 ;"ASKBILL --  interactive entry point for billing 
 ;"MAMMORPT --Show report of outstanding consults and extract schedule date from them
 ;"SETCONSULTTEMP -- Set Consult report templates to the Consult template IF it doesn't exist
 ;"DEVINRRPT -- interactive entry point for INR report, asking device.
 ;"COUMARPT -- report of patients who haven't had INR checks in 45+ days
 ;"CNSLTRPT(RECORDS,MAKENOTES) -- report of outstanding consults and extract schedule
 ;"ASKCNRPT --  interactive entry point for Consult report, asking for device 
 ;"DEVCNRPT -- NON-interactive entry point for Consult report 
 ;"MRNOTE(RESULT,DFN,DOS,SPECIALTY) -- Automatically create TIU note for sent records and sign
 ;"ASKMRRPT -- interactive entry point for MR report
 ;"ALLINRS -- diaplay all patient's PT/INR TIU Notes for the past year for verification of Sequel charges
 ;"PRTPAINR -- Non-interactive entry point for Pain report
 ;"PAINRPT -- report for patients due for UDS and Pain Contracts
 ;"PREVNAR -- report for patients needing Prevnar-13
 ;"CSDBPRT -- Non-interactive entry point for CSMD report,
 ;"CSDBRPT -- CSMD report.  
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"GETINRS  
 ;"GETDBDUE(TMGRESULT,BEGDT,ENDDT) -- Returns an array patients due for CSDB review
 ;
 ;"=======================================================================
 ;
ASKDEVRPT
       ;"Purpose: Provide an interactive entry point for report, asking device.
       NEW %ZIS
       SET %ZIS("A")="Enter Output Device: "
       SET %ZIS("B")="HOME"
       DO ^%ZIS  ;"standard device call
       IF POP DO  GOTO DMRDn
       . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output.  Aborting.")
       use IO
       DO MAMMORPT
       DO ^%ZISC  ;" Close the output device
       DO PRESS2GO^TMGUSRI2
DMRDn  QUIT

DEVMAMRPT
       ;"Purpose: Provide an interactive entry point for report, asking device.
       NEW %ZIS,IOP
       SET IOP="S121-LAUGHLIN-LASER"
       DO ^%ZIS  ;"standard device call
       IF POP DO  GOTO AMRDn
       . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output.  Aborting.")
       use IO
       DO MAMMORPT
       DO ^%ZISC  ;" Close the output device
AMRDn  QUIT

ASKBILL
       ;"Purpose: Provide interactive entry point for billing
       NEW %ZIS
       SET %ZIS("A")="Enter Output Device: "
       SET %ZIS("B")="HOME"
       DO ^%ZIS  ;"standard device call
       IF POP DO  GOTO ABDn
       . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output. Aborting.")
       USE IO
       NEW OUT
       DO BILLRPC^TMGRPT2(.OUT)
       NEW COUNT SET COUNT=0
       WRITE "BILLING ITEMS REPORT",!
       FOR  SET COUNT=$ORDER(OUT(COUNT)) QUIT:COUNT'>0  DO
       . WRITE $GET(OUT(COUNT)),!
       DO ^%ZISC  ;" Close the output device
ABDn  QUIT

MAMMORPT
       ;"Purpose: Show report of outstanding consults and extract schedule date from them.
       ;"Results: None, but report created.
       NEW MammoIEN SET MammoIEN=+$ORDER(^GMR(123.5,"B","MAMMOGRAM",""))
       IF MammoIEN'>0 DO  GOTO MRPTDn
       . WRITE "Can't locate record for MAMMOGRAM report.  Aborting.",!
       NEW ComplIEN SET ComplIEN=+$ORDER(^ORD(100.01,"B","COMPLETE",""))
       IF ComplIEN'>0 DO  GOTO MRPTDn
       . WRITE "Can't find record for COMPLETE status.  Aborting.",!
       NEW DCIEN SET DCIEN=+$ORDER(^ORD(100.01,"B","DISCONTINUED",""))
       NEW X,Y DO NOW^%DTC NEW NowDate SET NowDate=X
       ;
       WRITE !
       WRITE "************************************************************",!
       WRITE "              Outstanding mammograms report",!
       WRITE "                     " SET Y=X DO DD^%DT WRITE Y,!
       WRITE "          Please deliver this report to the nurse",!
       WRITE "************************************************************",!
       WRITE "                                            (From TMGRPT1.m)",!!
       NEW idx SET idx=""
       NEW matches,IENLIST
       FOR  SET idx=+$ORDER(^GMR(123,"C",MammoIEN,idx)) QUIT:(idx'>0)  do
       . NEW s
       . NEW znode SET znode=$GET(^GMR(123,idx,0))
       . NEW status SET status=$PIECE(znode,"^",12)
       . IF status=ComplIEN QUIT
       . IF status=DCIEN QUIT
       . NEW Y SET Y=$PIECE(znode,"^",7)  ;"date of request
       . DO DD^%DT SET s=Y
       . NEW PtIEN SET PtIEN=+$PIECE(znode,"^",2)
       . IF PtIEN'=0 do
       . . SET s=s_"^"_$PIECE($GET(^DPT(PtIEN,0)),"^",1)
       . ELSE  do
       . . SET s=s_"^"_"?? Patient Name not found.  Record # "_idx_" in file #123"
       . ;"Now scan for appt scheduled date
       . NEW idxWP SET idxWP=0
       . NEW found SET found=0
       . FOR  SET idxWP=+$ORDER(^GMR(123,idx,20,idxWP)) QUIT:(idxWP'>0)!found  do
       . . NEW line SET line=$GET(^GMR(123,idx,20,idxWP,0)) QUIT:line=""
       . . IF line'["Scheduled Appointment:" QUIT
       . . SET found=1
       . . NEW apptDate SET apptDate=$PIECE(line,"Scheduled Appointment:",2)
       . . SET Y=$$FMDate^TMGFMUT(apptDate)
       . . NEW FMDate SET FMDate=Y
       . . IF Y>0 do
       . . . DO DD^%DT  ;"standardize date
       . . ELSE  do
       . . . SET Y=apptDate
       . . . SET FMDate=NowDate ;Assume Due Now If Can't Resolve Date
       . . SET s=s_"^"_Y
       . . SET matches(FMDate,s)=""
       . . SET IENLIST(PtIEN)=""
       ;
       NEW future SET future=0
       NEW dueDate SET dueDate=""
       FOR  SET dueDate=$ORDER(matches(dueDate),1) QUIT:(dueDate="")  do
       . IF (dueDate>NowDate)&(future=0) do
       . . SET future=1
       . . WRITE "-----------------------------------------------------------------------------",!
       . NEW s SET s=""
       . FOR  SET s=$Order(matches(dueDate,s)) QUIT:s=""  do
       . . WRITE "Due: ",$p(s,"^",3),?25,$p(s,"^",2),?50,"Made on visit: ",$p($p(s,"^",1),"@",1),!
       ;" 
       QUIT   ;"DON'T PRINT THE FOLLOWING FOR NOW. REMOVE QUIT LATER WHEN READY
       ;"Print people who are overdue for mammograms below
       WRITE !
       WRITE "************************************************************",!
       WRITE "              Unscheduled  mammograms",!
       WRITE "************************************************************",!,!
       NEW DFN,UNSCHEDULEARR SET DFN=0
       NEW TODAY,X,Y DO NOW^%DTC SET TODAY=X
       FOR  SET DFN=$ORDER(^DPT(DFN)) QUIT:DFN'>0  DO
       . IF $$ACTIVEPT^TMGPXR03(DFN)'=1 QUIT
       . NEW AGE K VADM SET AGE=$$AGE^TIULO(DFN)
       . IF +AGE>75 QUIT
       . ;"GET LAST VISIT DATE
       . NEW LASTDT,VSTIEN SET LASTDT=0,VSTIEN=0
       . FOR  SET VSTIEN=$ORDER(^TMG(22723,DFN,1,VSTIEN)) QUIT:VSTIEN'>0  DO
       . . NEW VSTDATE SET VSTDATE=$P($G(^TMG(22723,DFN,1,VSTIEN,0)),"^",1)
       . . IF VSTDATE>TODAY QUIT
       . . IF LASTDT<VSTDATE SET LASTDT=VSTDATE
       . IF LASTDT=0 QUIT
       . SET Y=LASTDT X ^DD("DD") SET LASTDT=Y
       . NEW REMRESULT SET REMRESULT=$$DOREM^TMGPXR03(DFN,224,5,3170110)
       . IF REMRESULT["DUE NOW" DO
       . . IF $D(IENLIST(DFN)) DO
       . . . ;"skip  SET UNSCHEDULEARR($P($G(^DPT(DFN,0)),"^",1))="SCHEDULED AS ABOVE"
       . . ELSE  DO
       . . . SET UNSCHEDULEARR($P($G(^DPT(DFN,0)),"^",1))=$P(REMRESULT,"^",2,999)_"^"_LASTDT
       ;"
       NEW NAME SET NAME=""
       FOR  SET NAME=$ORDER(UNSCHEDULEARR(NAME)) QUIT:NAME=""  DO
       . NEW S SET S=$GET(UNSCHEDULEARR(NAME))
       . IF S["SCHEDULED" DO
       . . WRITE NAME,?25,S,!
       . ELSE  DO
       . . WRITE NAME,?20,"Last mammo: ",$P($P(S,"^",2),"@",1),?45,"Due: ",$P($P(S,"^",1),"@",1),?63,"Last vst: ",$P($P(S,"^",3),"@",1),!
MRPTDn QUIT

SETCONSULTTEMP
        ;"This function will SET the template for each existing consult to
        ;"the consult template IF no entry is found.
        ;"
        ;"The purpose is that by default, only one link can exist for each
        ;"template (file 8927-field .19). This creates the need for a
        ;"separate template to be maintained for each Consult. By running this,
        ;"multiple Consults can use a single template.
        ;"
        NEW CONSULTS SET CONSULTS=""
        NEW IEN SET IEN=0
        NEW CONSULTIEN
        set CONSULTIEN=+$ORDER(^TIU(8927,"B","CONSULT",""))
        FOR  SET CONSULTS=$ORDER(^GMR(123.5,"B",CONSULTS)) QUIT:(CONSULTS="")  do
        . SET IEN=$ORDER(^GMR(123.5,"B",CONSULTS,""))
        . NEW REF SET REF=0
        . SET REF=$ORDER(^TIU(8927,"AL",IEN_";GMR(123.5,",""))
        . IF REF'>0 SET ^TIU(8927,"AL",IEN_";GMR(123.5,",CONSULTIEN)=""
        . WRITE CONSULTS,!
        write "DONE"
        QUIT
        ;
DEVINRRPT
       ;"Purpose: Provide an interactive entry point for report, asking device.
       NEW %ZIS,IOP
       SET IOP="S121-LAUGHLIN-LASER"
       DO ^%ZIS  ;"standard device call
       IF POP DO  GOTO INRDn
       . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output.  Aborting.")
       use IO
       DO COUMARPT
       DO ^%ZISC  ;" Close the output device
INRDn  QUIT
COUMARPT ;
        ;"Purpose: Show report of patients who haven't had INR checks in 45+ days
        ;"Results: None, but report created.
        NEW COUMADINIEN SET COUMADINIEN=+$ORDER(^TIU(8927,"B","CarePlan<VEFA>-Coumadin",""))
        IF COUMADINIEN'>0 DO  GOTO INRPTDn
        . WRITE "Can't find TIU Template for CarePlan<VEFA>-Coumadin.   Aborting.",!
        NEW X,Y DO NOW^%DTC NEW NOWDATE SET NOWDATE=X
        ;
        WRITE !
        WRITE "************************************************************",!
        WRITE "                  Overdue INR checks (45+ days)",!
        WRITE "                     " SET Y=X DO DD^%DT WRITE Y,!
        WRITE "          Please deliver this report to the nurse",!
        WRITE "************************************************************",!
        WRITE "                                            (From TMGRPT1.m)",!!
        NEW CPIEN SET CPIEN=""
        NEW DFN,TIUIEN,TIUDATE,PTNAME,ACTIVE
        NEW X1,X2
        FOR  SET CPIEN=+$ORDER(^VEFA(19009,"C",COUMADINIEN,CPIEN)) QUIT:(CPIEN'>0)  DO
        . SET DFN=+$PIECE($GET(^VEFA(19009,CPIEN,0)),"^",1)
        . SET TIUIEN=$ORDER(^VEFA(19009,CPIEN,3,"B",""),-1)
        . SET TIUDATE=$$GET1^DIQ(8925,TIUIEN,.07,"I")
        . SET ACTIVE=$PIECE($GET(^VEFA(19009,CPIEN,0)),"^",5)
        . SET X1=NOWDATE
        . SET X2=TIUDATE
        . D ^%DTC
        . IF (X<180)&(X>45)&(ACTIVE="Y") DO
        . . SET Y=TIUDATE
        . . DO DD^%DT
        . . SET PTNAME=$PIECE($GET(^DPT(DFN,0)),"^",1)
        . . IF ($PIECE(PTNAME,",",1)'["ZZTEST")&($PIECE(PTNAME,",",1)'["TEST ")&($PIECE(PTNAME,",",1)'="TEST")  DO
        . . . WRITE PTNAME,"(",DFN,")",!
        . . . WRITE "            Last Check: ",Y," (",X/7\1," WKS AGO)",!
        . . . WRITE "     ------------- ",!
        WRITE !,"****************************END*****************************",!
INRPTDn QUIT
        ;
CNSLTRPT(RECORDS,MAKENOTES) ;
       ;"Purpose: Show report of outstanding consults and extract schedule
       ; date from them. (All except mammograms)
       ;"Results: None, but report created.
       SET MAKENOTES=+$GET(MAKENOTES)
       SET RECORDS=$GET(RECORDS)
       NEW MammoIEN SET MammoIEN=+$ORDER(^GMR(123.5,"B","MAMMOGRAM",""))
       IF MammoIEN'>0 DO  GOTO CNSTDn
       . WRITE "Can't locate record for MAMMOGRAM report.  Aborting.",!
       NEW ComplIEN SET ComplIEN=+$ORDER(^ORD(100.01,"B","COMPLETE",""))
       IF ComplIEN'>0 DO  GOTO CNSTDn
       . WRITE "Can't find record for COMPLETE status.  Aborting.",!
       NEW DCIEN SET DCIEN=+$ORDER(^ORD(100.01,"B","DISCONTINUED",""))
       NEW CANCELIEN SET CANCELIEN=+$ORDER(^ORD(100.01,"B","CANCELLED",""))
       NEW X,Y DO NOW^%DTC NEW NowDate SET NowDate=X
       ;
       WRITE !
       WRITE "************************************************************",!
       IF RECORDS>0 DO
       . WRITE "              Upcoming records to be sent",!
       ELSE  DO
       . WRITE "              Outstanding consults report",!
       WRITE "                     " SET Y=X DO DD^%DT WRITE Y,!
       IF RECORDS>0 DO
       . WRITE "        Please deliver this report to medical records",!
       ELSE  DO
       . WRITE "          Please deliver this report to the nurse",!
       WRITE "************************************************************",!
       WRITE "                                            (From TMGRPT1.m)",!!
       NEW idx SET idx=""
       NEW IEN SET IEN=0
       NEW matches
       NEW ORDERTYPE
       FOR  SET IEN=+$ORDER(^GMR(123,"C",IEN)) QUIT:(IEN'>0)  DO
       . IF IEN=MammoIEN QUIT
       . SET ORDERTYPE=$PIECE($GET(^GMR(123.5,IEN,0)),"^",1)
       . FOR  SET idx=+$ORDER(^GMR(123,"C",IEN,idx)) QUIT:(idx'>0)  do
       . . NEW s
       . . NEW znode SET znode=$GET(^GMR(123,idx,0))
       . . NEW status SET status=$PIECE(znode,"^",12)
       . . IF status=ComplIEN QUIT
       . . IF status=DCIEN QUIT
       . . IF status=CANCELIEN QUIT
       . . NEW Y SET Y=$PIECE(znode,"^",7)  ;"date of request
       . . DO DD^%DT SET s=Y
       . . NEW PtIEN SET PtIEN=+$PIECE(znode,"^",2)
       . . IF PtIEN'=0 do
       . . . SET s=s_"^"_$PIECE($GET(^DPT(PtIEN,0)),"^",1)_"-"_PtIEN
       . . ELSE  do
       . . . SET s=s_"^"_"?? Patient Name not found.  Record # "_idx_" in file #123"
       . . ;"Now scan for appt scheduled date
       . . NEW idxWP SET idxWP=0
       . . NEW found SET found=0
       . . FOR  SET idxWP=+$ORDER(^GMR(123,idx,20,idxWP)) QUIT:(idxWP'>0)!found  do
       . . . NEW line SET line=$GET(^GMR(123,idx,20,idxWP,0)) QUIT:line=""
       . . . IF line'["An appointment has been scheduled" QUIT
       . . . SET found=1
       . . . SET line=$GET(^GMR(123,idx,20,idxWP+1,0))
       . . . NEW apptDate SET apptDate=$PIECE(line,"*",2)
       . . . SET Y=$$FMDate^TMGFMUT(apptDate)
       . . . NEW FMDate SET FMDate=Y
       . . . IF Y>0 do
       . . . . DO DD^%DT  ;"standardize date
       . . . ELSE  do
       . . . . SET Y=apptDate
       . . . . SET FMDate=NowDate ;Assume Due Now If Can't Resolve Date
       . . . SET s=s_"^"_Y_"^"_ORDERTYPE
       . . . SET matches(FMDate,s)=""
       ;
       NEW future SET future=0
       NEW dueDate,RESULT,SENT,QUESTION,endDate
       ;"IF RECORDS>0 SET dueDate=NowDate 
       ;"ELSE  SET dueDate=""
       ;
       NEW X1,X2,X
       SET X1=NowDate,X2=1
       DO C^%DTC
       IF RECORDS>0 SET dueDate=X
       ELSE  SET dueDate=""
       SET X1=X,X2=6
       DO C^%DTC
       SET endDate=X
       FOR  SET dueDate=$ORDER(matches(dueDate),1) QUIT:(dueDate="")  do
       . IF (RECORDS>0)&(dueDate>endDate) QUIT
       . IF (dueDate>NowDate)&(future=0) do
       . . SET future=1
       . . WRITE "-----------------------------------------------------------------------------",!
       . NEW s SET s=""
       . FOR  SET s=$Order(matches(dueDate,s)) QUIT:s=""  do
       . . ;WRITE s,!
       . . IF RECORDS>0 WRITE "[ ]"
       . . WRITE $P(s,"^",4),?18,"Appt Date: ",$p(s,"^",3),?50,$p($p(s,"^",2),"-",1),?57,! ;""Made: ",$p($p(s,"^",1),"@",1),!
       . . IF MAKENOTES=1 DO
       . . . ;"WRITE "HAVE NOTES BEEN SENT FOR "_$p($p(s,"^",2),"-",1)_"? (Y/N)"
       . . . ;"READ SENT
       . . . ;"WRITE !
       . . . ;"IF SENT="Y" 
       . . . DO MRNOTE(.RESULT,$p($p(s,"^",2),"-",2),$p(s,"^",3),$p(s,"^",4))
CNSTDn QUIT
	;
MRNOTE(RESULT,DFN,DOS,SPECIALTY)  ;
       ;"Purpose: Automatically create TIU note for sent records and sign
       NEW DOCIEN
       SET DFN=+$GET(DFN),DOS=$GET(DOS)
       SET RESULT="-1^ERROR"
       IF DFN'>0 GOTO MNDN
       IF DOS=0 GOTO MNDN
       DO BLANKTIU^TMGRPC1(.RESULT,DFN,"HAGOOD,EDDIE",6,DOS,"RECORDS SENT")
       IF RESULT>0 DO SEND^TIUALRT(RESULT)
       GOTO MNDN    ;"FOR NOW JUST CREATE THE NOTE AND LEAVE IT THERE
       SET DOCIEN=RESULT
       IF DOCIEN'>0 GOTO MNDN
       ;"DO LOCK^TIUSRVP(.RESULT,DOCIEN)
       NEW TEXT
       SET TEXT("TEXT",1,0)="RECORDS SENT FOR "_SPECIALTY_" VISIT"
       SET TEXT("TEXT",2,0)="{RECORDS SENT}"
       D UPDATE^TIUSRVP(.RESULT,DOCIEN,.TEXT,1)
       ;"D UNLOCK^TIUSRVP(.RESULT,DOCIEN)
       GOTO MNDN
       IF RESULT'>0 GOTO MNDN
       NEW TMGFDA,TMGMSG
       SET TMGFDA(8925,DOCIEN_",",.05)="COMPLETED"      ;"field .05 = STATUS
       SET TMGFDA(8925,DOCIEN_",",1501)="NOW"           ;"field 1501 = Signed date
       SET TMGFDA(8925,DOCIEN_",",1502)="`150"   ;"field 1502 = signed by
       SET TMGFDA(8925,DOCIEN_",",1503)="HAGOOD,EDDIE"      ;"field 1503 = Signature block name
       ;"SET TMGFDA(8925,DOCIEN_",",1504)=
       SET TMGFDA(8925,DOCIEN_",",1505)="C"  ;C=Chart   ;"field 1505 = Signature mode
       DO FILE^DIE("E","TMGFDA","TMGMSG")
       IF $DATA(TMGMSG("DIERR")) DO  GOTO MNDN
       . DO ZWRITE^TMGZWR("TMGMSG(""DIERR"")")
       . MERGE RESULT("DIERR")=TMGMSG("DIERR")
        ;
       ;"D AUTOSIGN^TMGRPC1(.RESULT,DOCIEN)
       DO SEND^TIUALRT(DOCIEN)
MNDN   QUIT
       ;"
ASKCNRPT  ;
       ;"Purpose: Provide an interactive entry point for report, asking for device.
       NEW %ZIS
       SET %ZIS("A")="Enter Output Device: "
       SET %ZIS("B")="HOME"
       DO ^%ZIS  ;"standard device call
       IF POP DO  GOTO CNRDn
       . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output. Aborting.")
       use IO
       DO CNSLTRPT
       DO ^%ZISC  ;" Close the output device
       DO PRESS2GO^TMGUSRI2
CNRDn  QUIT
       ;
DEVCNRPT  ;
       ;"Purpose: Provide an NON-interactive entry point for report
       ; device.
       NEW %ZIS,IOP
       SET IOP="S121-LAUGHLIN-LASER"
       DO ^%ZIS  ;"standard device call
       IF POP DO  GOTO DCNDn
       . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output. Aborting.")
       use IO
       DO CNSLTRPT
       ;"DO ^%ZISC  ;" Close the output device
DCNDn  QUIT
       ;
ASKMRRPT  ;
       ;"Purpose: Provide an interactive entry point for report,
       NEW MAKENOTES
       READ "Would you like to create notes for these visits? (Y/N)",MAKENOTES:$GET(DTIME,3600),!
       IF MAKENOTES="Y" SET MAKENOTES=1
       ELSE  SET MAKENOTES=0
       NEW %ZIS
       SET %ZIS("A")="Enter Output Device: "
       SET %ZIS("B")="HOME"
       DO ^%ZIS  ;"standard device call
       IF POP DO  GOTO MRRDn
       . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output.  Aborting.")
       use IO
       DO CNSLTRPT(1,MAKENOTES)
       DO ^%ZISC  ;" Close the output device
       DO PRESS2GO^TMGUSRI2
MRRDn  QUIT
       ;"
ALLINRS ;
       ;"Purpose: This report will diaplay all patient's PT/INR TIU Notes
       ;"         for the past year for verification of Sequel charges
       NEW %ZIS
       SET %ZIS("A")="Enter Output Device: "
       SET %ZIS("B")="HOME"
       DO ^%ZIS  ;"standard device call
       IF POP DO  GOTO AIDn
       . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output. Aborting.")
       use IO
       DO GETINRS
       DO ^%ZISC  ;" Close the output device
       ;"DO PRESS2GO^TMGUSRI2
AIDn   QUIT
       ;"
GETINRS  ;
       NEW DOCTYPE,TIUIEN,DFN,TIUDATE,INRIEN,NOWDATE,YEARAGODT
       NEW RESULTARR,X,X1,X2
       DO NOW^%DTC SET NOWDATE=X
       SET X1=NOWDATE,X2=-365
       DO C^%DTC SET YEARAGODT=X              
       SET INRIEN=1457,TIUIEN=0
       FOR  SET TIUIEN=$ORDER(^TIU(8925,"B",INRIEN,TIUIEN)) QUIT:TIUIEN'>0  DO
       . NEW ZN SET ZN=$GET(^TIU(8925,TIUIEN,0))
       . SET TIUDATE=$PIECE(ZN,"^",7)
       . SET DFN=$PIECE(ZN,"^",2)
       . IF TIUDATE>YEARAGODT DO
       . . SET RESULTARR(DFN,TIUDATE)=""
       SET DFN=0
       FOR  SET DFN=$ORDER(RESULTARR(DFN)) QUIT:DFN'>0  DO
       . SET TIUDATE=0
       . NEW PATIENTNAME
       . SET PATIENTNAME=$PIECE($GET(^DPT(DFN,0)),"^",1)
       . WRITE "===== ",PATIENTNAME," ====",!
       . FOR  SET TIUDATE=$ORDER(RESULTARR(DFN,TIUDATE)) QUIT:TIUDATE'>0  DO
       . . NEW Y SET Y=TIUDATE
       . . DO DD^%DT
       . . WRITE "  ->",Y,!
       QUIT
       ;"
PRTPAINR  ;"
       ;"Purpose: Provide an Non-interactive entry point for Pain report,
       NEW %ZIS
       SET %ZIS("A")="Enter Output Device: "
       SET IOP="S121-LAUGHLIN-LASER"
       DO ^%ZIS  ;"standard device call
       IF POP DO  GOTO PPDn
       . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output.  Aborting.")
       use IO
       DO PAINRPT
       DO ^%ZISC  ;" Close the output device
       ;"DO PRESS2GO^TMGUSRI2
PPDn   QUIT
       ;"
PAINRPT   ;"
       ;"Purpose: Print report for patients due for UDS and Pain Contracts
       NEW RESULT
       NEW X,Y DO NOW^%DTC NEW NowDate SET NowDate=X
       NEW REMARR
       SET REMARR(232)="PAIN CONTRACT DUE"
       SET REMARR(233)="DRUG SCREEN DUE"
       DO APPTREMS^TMGPXR03(.RESULT,.REMARR,NowDate)
       IF RESULT(0)=0 GOTO PRDn
       WRITE !
       WRITE "************************************************************",!
       WRITE "              Controlled substance reminders due today",!
       WRITE "                            " SET Y=NowDate DO DD^%DT WRITE Y,!
       WRITE "               Please deliver this report to the NURSE",!
       WRITE "************************************************************",!
       WRITE "                                            (From TMGRPT1.m)",!!
       NEW REMIEN SET REMIEN=0
       FOR  SET REMIEN=$ORDER(RESULT(REMIEN)) QUIT:REMIEN'>0  DO
       . NEW REMDISP SET REMDISP=$PIECE($GET(^PXD(811.9,REMIEN,0)),"^",3)
       . WRITE "============= PATIENTS DUE FOR ",REMDISP," ============",!
       . NEW DATE SET DATE=0
       . FOR  SET DATE=$ORDER(RESULT(REMIEN,DATE)) QUIT:DATE'>0  DO
       . . NEW DFN SET DFN=0
       . . FOR  SET DFN=$ORDER(RESULT(REMIEN,DATE,DFN)) QUIT:DFN'>0  DO
       . . . NEW NAME SET NAME=$PIECE($GET(^DPT(DFN,0)),"^",1)
       . . . SET Y=DATE DO DD^%DT
       . . . WRITE "  - ",NAME,?40,Y,!
       . WRITE !!
PRDn   QUIT
       ;
PREVNAR ;
       ;"Purpose: Print report for patients needing Prevnar-13
       ;
       QUIT    
       ;"
CSDBPRT  ;
       ;"Purpose: Provide an Non-interactive entry point for CSMD report,
       NEW %ZIS
       SET %ZIS("A")="Enter Output Device: "
       SET IOP="S121-LAUGHLIN-LASER"
       DO ^%ZIS  ;"standard device call
       IF POP DO  GOTO CSPDn
       . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output. Aborting.")
       use IO
       DO CSDBRPT
       DO ^%ZISC  ;" Close the output device
       ;"DO PRESS2GO^TMGUSRI2
CSPDn  QUIT
       ;"
CSDBRPT   ;"CSMD report. 
       NEW DUEARRAY
       NEW X,Y DO NOW^%DTC NEW NowDate SET NowDate=X
       DO GETDBDUE(.DUERESULT,NowDate)
       IF DUERESULT(0)=0 GOTO CSDn
       WRITE !
       WRITE "****************************************************************",!
       WRITE "              Controlled Substance DB Review due today",!
       WRITE "                            " SET Y=NowDate DO DD^%DT WRITE Y,!
       WRITE "               Please deliver this report to MEDICAL RECORDS",!
       WRITE "****************************************************************",!
       WRITE "                                            (From TMGRPT1.m)",!!
       NEW DATE SET DATE=0
       WRITE "          NAME",?28,"  DOB",?45," APPT DATE",!
       FOR  SET DATE=$ORDER(DUERESULT(DATE)) QUIT:DATE'>0  DO
       . NEW DFN SET DFN=0
       . FOR  SET DFN=$ORDER(DUERESULT(DATE,DFN)) QUIT:DFN'>0  DO
       . . NEW NAME SET NAME=$PIECE($GET(^DPT(DFN,0)),"^",1)
       . . NEW DOB SET DOB=$PIECE($GET(^DPT(DFN,0)),"^",3)
       . . SET Y=DOB DO DD^%DT SET DOB=Y
       . . NEW APPT SET Y=DATE DO DD^%DT SET APPT=Y
       . . WRITE "[ ] ",NAME,?28,"(",DOB,")",?45,APPT,!
CSDn   QUIT
       ;"
GETDBDUE(TMGRESULT,BEGDT,ENDDT)  ;"
       ;"Purpose: Returns an array of every patient who is scheduled between
       ;"         dates sent and is due for CSDB review
       KILL TMGRESULT
       NEW PTARRAY,COUNT
       SET BEGDT=$GET(BEGDT),ENDDT=+$GET(ENDDT),COUNT=0
       IF ENDDT'>0 SET ENDDT=BEGDT
       DO GETSCHED^TMGPXR03(.PTARRAY,BEGDT,ENDDT)
       NEW DFN SET DFN=0
       FOR  SET DFN=$ORDER(PTARRAY(DFN)) QUIT:DFN'>0  DO
       . NEW MEDARRAY,OUT
       . DO MEDLIST^TMGTIUOJ(.OUT,DFN,.MEDARRAY)
       . IF $GET(MEDARRAY("KEY-VALUE","*CSM-DATABASE REVIEW","LINE"))["NEEDS UPDATE" DO
       . . SET COUNT=COUNT+1
       . . SET TMGRESULT($ORDER(PTARRAY(DFN,0)),DFN)=""
       SET TMGRESULT(0)=COUNT
       QUIT
       ;"
DEEMEDPT()  ;"
       NEW %ZIS
       SET %ZIS("A")="Enter Output Device: "
       SET IOP="S121-LAUGHLIN-LASER"
       DO ^%ZIS  ;"standard device call
       IF POP DO  GOTO DEEDN
       . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output. Aborting.")
       use IO
       DO NOW^%DTC
       WRITE !
       WRITE "****************************************************************",!
       WRITE "     Dr. Dee, ICD-10s for 2016 Medicare patients",!
       WRITE "                " SET Y=X DO DD^%DT WRITE Y,!
       WRITE "****************************************************************",!
       WRITE "                                            (From TMGRPT1.m)",!!
       NEW IDX SET IDX=0
       NEW PTARRAY
       FOR  SET IDX=$ORDER(^TIU(8925,"CA",83,IDX)) QUIT:IDX'>0  DO
       . NEW ZN,DFN,DATE,INS,INSIDX,INCLUDE,NOTEIEN
       . SET ZN=$GET(^TIU(8925,IDX,0))
       . SET DATE=$P(ZN,"^",7)
       . ;"WRITE DATE,!
       . IF DATE<3160101 QUIT
       . SET NOTEIEN=$P(ZN,"^",1)
       . ;"WRITE NOTEIEN,!
       . IF (NOTEIEN'=1408)&(NOTEIEN'=1399)&(NOTEIEN'=1402)&(NOTEIEN'=1983)&(NOTEIEN'=6)&(NOTEIEN'=7) QUIT
       . SET DFN=$P(ZN,"^",2)
       . SET INSIDX=0,INCLUDE=0
       . FOR  SET INSIDX=$ORDER(^DPT(DFN,.312,INSIDX)) QUIT:INSIDX'>0  DO
       . . NEW INSIEN SET INSIEN=$GET(^DPT(DFN,.312,INSIDX,0))
       . . IF INSIEN=3 SET INCLUDE=1
       . IF INCLUDE=0 QUIT
       . NEW NAME SET NAME=$P($G(^DPT(DFN,0)),"^",1)
       . NEW ICDIEN SET ICDIEN=0
       . FOR  SET ICDIEN=$ORDER(^AUPNVPOV("C",DFN,ICDIEN)) QUIT:+ICDIEN'>0  DO
       . . NEW ZN SET ZN=$GET(^AUPNVPOV(ICDIEN,0)) QUIT:ZN=""
       . . NEW VSTIEN SET VSTIEN=+$PIECE(ZN,"^",3) QUIT:VSTIEN'>0
       . . NEW VSTZN SET VSTZN=$GET(^AUPNVSIT(VSTIEN,0)) QUIT:VSTZN=""
       . . NEW VSTDT SET VSTDT=$PIECE(VSTZN,"^",1)
       . . IF (VSTDT<3160101)!(VSTDT>3161231) QUIT  ;"Past this line, ICD is within daterange
       . . ;" BELOW WAS ICDIEN, AND WHILE IT WAS NEWED WAS CONFUSING WITH ABOVE VARIABLE  12/28/16
       . . NEW VICDIEN SET VICDIEN=+$PIECE(ZN,"^",1) QUIT:VICDIEN'>0
       . . NEW ICDZN SET ICDZN=$GET(^ICD9(VICDIEN,0)) QUIT:ICDZN=""
       . . NEW ADT SET ADT=$ORDER(^ICD9(VICDIEN,68,"B",""),-1) QUIT:ADT'>0
       . . NEW PTR SET PTR=$ORDER(^ICD9(VICDIEN,68,"B",ADT,0)) QUIT:PTR'>0
       . . NEW DESCR SET DESCR=$GET(^ICD9(VICDIEN,68,PTR,1))
       . . NEW STR SET STR=$PIECE(ICDZN,"^",1)_" -- "_DESCR
       . . SET PTARRAY(NAME,STR)=""
       . . ;"SET PTARRAY(NAME,DATE)=""
       NEW NAME,DATESTR SET NAME=""
       FOR  SET NAME=$ORDER(PTARRAY(NAME)) QUIT:NAME=""  DO
       . WRITE NAME,!
       . NEW ICD,ICDSTR SET ICD="",ICDSTR=""
       . FOR  SET ICD=$ORDER(PTARRAY(NAME,ICD)) QUIT:ICD=""  DO
       . . W ICD,!
       . . QUIT
       . . IF ICDSTR="" DO
       . . . SET ICDSTR=ICD
       . . ELSE  DO
       . . . SET ICDSTR=ICDSTR_", "_ICD
       . W !
       . ;"WRITE "  ",ICDSTR,!,!
DEEDN  DO ^%ZISC  ;" Close the output device
       DO PRESS2GO^TMGUSRI2
       QUIT
       ;"
PT2BINAC()   ;" Print a list of patients who should be inactivated
       NEW %ZIS
       SET %ZIS("A")="Enter Output Device: "
       SET IOP="S121-LAUGHLIN-LASER"
       DO ^%ZIS  ;"standard device call
       IF POP DO  GOTO DEEDN
       . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output. Aborting.")
       use IO
       DO NOW^%DTC
       WRITE !
       WRITE "****************************************************************",!
       WRITE "   Patients set as Active, but haven't been scheduled since 2014",!
       WRITE "                " SET Y=X DO DD^%DT WRITE Y,!
       WRITE "****************************************************************",!
       WRITE "                                        (From TMGRPT1.m)",!!
       WRITE " ",!
       WRITE "PATIENT",?25,"LAST OV NOTE",?47,"DISCHARGE ?",!
       NEW DFN,PTARRAY,NOVISITARRAY SET DFN=0
       FOR  SET DFN=$ORDER(^DPT(DFN)) QUIT:DFN'>0  DO
       . IF $$ACTIVEPT^TMGPXR03(DFN)'=1 QUIT
       . ;"GET LAST VISIT DATE
       . NEW LASTDT,VSTIEN SET LASTDT=0,VSTIEN=0
       . FOR  SET VSTIEN=$ORDER(^TMG(22723,DFN,1,VSTIEN)) QUIT:VSTIEN'>0  DO
       . . NEW VSTDATE SET VSTDATE=$P($G(^TMG(22723,DFN,1,VSTIEN,0)),"^",1)
       . . IF LASTDT<VSTDATE SET LASTDT=VSTDATE
       . IF LASTDT>0 QUIT
       . SET PTARRAY($P($G(^DPT(DFN,0)),"^",1))=DFN
       NEW NAME SET NAME=""
       FOR  SET NAME=$ORDER(PTARRAY(NAME)) QUIT:NAME=""  DO
       . NEW NOTEDATE,LASTON SET NOTEDATE=9999999,LASTON=0
       . SET DFN=$GET(PTARRAY(NAME))
       . FOR  SET NOTEDATE=$ORDER(^TIU(8925,"APTP",DFN,NOTEDATE),-1) QUIT:(NOTEDATE'>0)!(LASTON>0)  DO
       . . NEW TIUIEN SET TIUIEN=$ORDER(^TIU(8925,"APTP",DFN,NOTEDATE,0))
       . . NEW DOCTYPE SET DOCTYPE=$P($G(^TIU(8925,TIUIEN,0)),"^",1)
       . . NEW OFFNOTE SET OFFNOTE=$G(^TIU(8925.1,DOCTYPE,"TMGH"))
       . . IF OFFNOTE="Y" SET LASTON=NOTEDATE
       . NEW Y SET Y=LASTON X ^DD("DD") SET LASTON=Y
       . IF LASTON=0 DO    ;"SET LASTON="*NO OFFICE NOTE FOUND*"
       . . SET NOVISITARRAY(NAME)=""
       . ELSE  DO
       . . WRITE NAME,?25,$P(LASTON,"@",1),?50,"[  ]",!
       SET NAME=""
       WRITE !,"======== NO OFFICE NOTES FOUND FOR THE FOLLOWING =========",!
       FOR  SET NAME=$ORDER(NOVISITARRAY(NAME)) QUIT:NAME=""  DO
       . WRITE NAME,?50,"[  ]",!
       DO ^%ZISC  ;" Close the output device 
       QUIT
