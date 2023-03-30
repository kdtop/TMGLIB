TMGRPT1 ;TMG/kst/Misc Reports;8/14/13, 2/2/14, 10/12/16, 5/8/17, 3/24/21
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
 ;"MRNOTE(RESULT,TMGDFN,DOS,SPECIALTY) -- Automatically create TIU note for sent records and sign
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
       IF MammoIEN'>0 GOTO MRPTDn
       NEW BoneDesIEN SET BoneDesIEN=+$ORDER(^GMR(123.5,"B","BONE DENSITY",""))
       IF BoneDesIEN'>0 DO  GOTO MRPTDn       
       . WRITE "Can't locate record for MAMMOGRAM report.  Aborting.",!
       NEW CONSARR SET CONSARR(MammoIEN)="",CONSARR(BoneDesIEN)=""
       NEW ComplIEN SET ComplIEN=+$ORDER(^ORD(100.01,"B","COMPLETE",""))
       IF ComplIEN'>0 DO  GOTO MRPTDn
       . WRITE "Can't find record for COMPLETE status.  Aborting.",!
       NEW DCIEN SET DCIEN=+$ORDER(^ORD(100.01,"B","DISCONTINUED",""))
       NEW X,Y DO NOW^%DTC NEW NowDate SET NowDate=X
       ;
       ;"GOTO UNSCH
       WRITE !
       WRITE "************************************************************",!
       WRITE "          Outstanding mammograms/bone density report",!
       WRITE "                     " SET Y=X DO DD^%DT WRITE Y,!
       WRITE "          Please deliver this report to the nurse",!
       WRITE "************************************************************",!
       WRITE "                                            (From TMGRPT1.m)",!!
       NEW idx SET idx=""
       NEW matches,IENLIST
       NEW CONSIEN SET CONSIEN=0
       FOR  SET CONSIEN=$O(CONSARR(CONSIEN)) QUIT:CONSIEN'>0  DO
       . FOR  SET idx=+$ORDER(^GMR(123,"C",CONSIEN,idx)) QUIT:(idx'>0)  do
       . . NEW s
       . . NEW znode SET znode=$GET(^GMR(123,idx,0))
       . . NEW status SET status=$PIECE(znode,"^",12)
       . . IF status=ComplIEN QUIT
       . . IF status=DCIEN QUIT
       . . NEW Y SET Y=$PIECE(znode,"^",7)  ;"date of request
       . . DO DD^%DT SET s=Y
       . . NEW PtIEN SET PtIEN=+$PIECE(znode,"^",2)
       . . IF PtIEN'=0 do
       . . . SET s=s_"^"_$PIECE($GET(^DPT(PtIEN,0)),"^",1)_" ("_$$EXTDATE^TMGDATE($PIECE($GET(^DPT(PtIEN,0)),"^",3))_")"
       . . ELSE  do
       . . . SET s=s_"^"_"?? Patient Name not found.  Record # "_idx_" in file #123"
       . . ;"Now scan for appt scheduled date
       . . NEW idxWP SET idxWP=0
       . . NEW found SET found=0
       . . FOR  SET idxWP=+$ORDER(^GMR(123,idx,20,idxWP)) QUIT:(idxWP'>0)!found  do
       . . . NEW line SET line=$GET(^GMR(123,idx,20,idxWP,0)) QUIT:line=""
       . . . IF line'["Scheduled Appointment:" QUIT
       . . . SET found=1
       . . . NEW apptDate SET apptDate=$PIECE(line,"Scheduled Appointment:",2)
       . . . SET Y=$$FMDate^TMGFMUT(apptDate)
       . . . NEW FMDate SET FMDate=Y
       . . . IF Y>0 do
       . . . . DO DD^%DT  ;"standardize date
       . . . ELSE  do
       . . . . SET Y=apptDate
       . . . . SET FMDate=NowDate ;Assume Due Now If Can't Resolve Date
       . . . SET s=s_"^"_Y
       . . . SET matches(CONSIEN,FMDate,s)=""
       . . . SET IENLIST(PtIEN)=""
       . . . ;"new code
       . . . NEW SUBX SET SUBX=1
       . . . NEW SUBIEN SET SUBIEN=0
       . . . FOR  SET SUBIEN=$O(^GMR(123,idx,50,SUBIEN)) QUIT:SUBIEN'>0  DO
       . . . . NEW RESCHIEN SET RESCHIEN=2230
       . . . . NEW TIUIEN SET TIUIEN=$G(^GMR(123,idx,50,SUBIEN,0))
       . . . . IF TIUIEN["TIU" DO
       . . . . . SET TIUIEN=+TIUIEN
       . . . . . IF $P($G(^TIU(8925,TIUIEN,0)),"^",1)'=RESCHIEN QUIT
       . . . . . NEW TEXTLINE SET TEXTLINE=0
       . . . . . FOR  SET TEXTLINE=$O(^TIU(8925,TIUIEN,"TEXT",TEXTLINE)) QUIT:TEXTLINE'>0  DO
       . . . . . . NEW LINE SET LINE=$G(^TIU(8925,TIUIEN,"TEXT",TEXTLINE,0))
       . . . . . . IF LINE["New date is" DO
       . . . . . . . NEW NEWDATE SET NEWDATE=$P($P(LINE,"<I>",2),"</B",1)
       . . . . . . . SET matches(CONSIEN,FMDate,s,SUBX)="RESCHEDULED FOR: "_NEWDATE
       . . . . . . . SET SUBX=SUBX+1
       ;
       NEW future SET future=0
       NEW dueDate SET dueDate=""
       SET CONSIEN=0
       FOR  SET CONSIEN=$O(CONSARR(CONSIEN)) QUIT:CONSIEN'>0  DO
       . WRITE "==============> ",$P($G(^GMR(123.5,CONSIEN,0)),"^",1)," <==============",!
       . FOR  SET dueDate=$ORDER(matches(CONSIEN,dueDate),1) QUIT:(dueDate="")  do
       . . IF (dueDate>NowDate)&(future=0) do
       . . . SET future=1
       . . . WRITE "-----------------------------------------------------------------------------",!
       . . NEW s SET s=""
       . . FOR  SET s=$Order(matches(CONSIEN,dueDate,s)) QUIT:s=""  do
       . . . WRITE "Due: ",$p(s,"^",3),?25,$p(s,"^",2),?60,"Made on visit: ",$p($p(s,"^",1),"@",1),!
       . . . NEW SUBIDX SET SUBIDX=0
       . . . FOR  SET SUBIDX=$Order(matches(CONSIEN,dueDate,s,SUBIDX)) QUIT:SUBIDX'>0  DO
       . . . . WRITE ?18,"========>",$G(matches(CONSIEN,dueDate,s,SUBIDX)),!
       . WRITE !
       ;" 
       QUIT   ;"DON'T PRINT THE FOLLOWING FOR NOW. REMOVE QUIT LATER WHEN READY
UNSCH
       ;"Print people who are overdue for mammograms below
       WRITE !
       WRITE "************************************************************",!
       WRITE "              Unscheduled  mammograms",!
       WRITE "************************************************************",!,!
       NEW TMGDFN,UNSCHEDULEARR SET TMGDFN=0
       NEW TODAY,X,Y DO NOW^%DTC SET TODAY=X
       FOR  SET TMGDFN=$ORDER(^DPT(TMGDFN)) QUIT:TMGDFN'>0  DO
       . IF $$ACTIVEPT^TMGPXR03(TMGDFN)'=1 QUIT
       . NEW AGE K VADM SET AGE=$$AGE^TIULO(TMGDFN)
       . IF +AGE>75 QUIT
       . IF +AGE<64 QUIT
       . ;"GET LAST VISIT DATE
       . NEW LASTDT,VSTIEN SET LASTDT=0,VSTIEN=0
       . FOR  SET VSTIEN=$ORDER(^TMG(22723,TMGDFN,1,VSTIEN)) QUIT:VSTIEN'>0  DO
       . . NEW VSTDATE SET VSTDATE=$P($G(^TMG(22723,TMGDFN,1,VSTIEN,0)),"^",1)
       . . IF VSTDATE>TODAY QUIT
       . . IF LASTDT<VSTDATE SET LASTDT=VSTDATE
       . IF LASTDT=0 QUIT
       . SET Y=LASTDT X ^DD("DD") SET LASTDT=Y
       . NEW REMRESULT SET REMRESULT=$$DOREM^TMGPXR03(TMGDFN,224,5,$$TODAY^TMGDATE)
       . IF REMRESULT["DUE NOW" DO
       . . IF $D(IENLIST(TMGDFN)) DO
       . . . ;"skip  SET UNSCHEDULEARR($P($G(^DPT(TMGDFN,0)),"^",1))="SCHEDULED AS ABOVE"
       . . ELSE  DO
       . . . SET UNSCHEDULEARR($P($G(^DPT(TMGDFN,0)),"^",1))=$P(REMRESULT,"^",2,999)_"^"_LASTDT
       ;"
       NEW NAME SET NAME=""
       FOR  SET NAME=$ORDER(UNSCHEDULEARR(NAME)) QUIT:NAME=""  DO
       . NEW S SET S=$GET(UNSCHEDULEARR(NAME))
       . IF S["SCHEDULED" DO
       . . WRITE NAME,?25,S,!,!,!
       . ELSE  DO
       . . WRITE NAME,?20,"Last mammo: ",$P($P(S,"^",2),"@",1),?45,"Due: ",$P($P(S,"^",1),"@",1),?63,"Last vst: ",$P($P(S,"^",3),"@",1),!,!,!
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
        NEW TMGDFN,TIUIEN,TIUDATE,PTNAME,ACTIVE
        NEW X1,X2
        WRITE "--------------------- OLD SYSTEM ---------------------",!
        FOR  SET CPIEN=+$ORDER(^VEFA(19009,"C",COUMADINIEN,CPIEN)) QUIT:(CPIEN'>0)  DO
        . SET TMGDFN=+$PIECE($GET(^VEFA(19009,CPIEN,0)),"^",1)
        . SET TIUIEN=$ORDER(^VEFA(19009,CPIEN,3,"B",""),-1)
        . SET TIUDATE=$$GET1^DIQ(8925,TIUIEN,.07,"I")
        . SET ACTIVE=$PIECE($GET(^VEFA(19009,CPIEN,0)),"^",5)
        . SET X1=NOWDATE
        . SET X2=TIUDATE
        . D ^%DTC
        . IF (X<180)&(X>45)&(ACTIVE="Y") DO
        . . SET Y=TIUDATE
        . . DO DD^%DT
        . . SET PTNAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
        . . IF ($PIECE(PTNAME,",",1)'["ZZTEST")&($PIECE(PTNAME,",",1)'["TEST ")&($PIECE(PTNAME,",",1)'="TEST")  DO
        . . . WRITE PTNAME,"(",TMGDFN,")",!
        . . . WRITE "            Last Check: ",Y," (",X/7\1," WKS AGO)",!
        . . . WRITE "     ------------- ",!
        WRITE !
        DO NEWCOUMA
        WRITE !,"****************************END*****************************",!
INRPTDn QUIT
        ;
NEWCOUMA ;
       NEW DATE SET DATE=$$TODAY^TMGDATE
       WRITE "--------------------- NEW SYSTEM ---------------------",!
       FOR  SET DATE=$O(^ORAM(103,"L",DATE),-1) QUIT:DATE'>0  DO
       . NEW TMGDFN SET TMGDFN=0
       . FOR  SET TMGDFN=$O(^ORAM(103,"L",DATE,TMGDFN)) QUIT:TMGDFN'>0  DO
       . . IF $$ACTIVEPT^TMGPXR03(TMGDFN)'=1 QUIT
       . . NEW NAME SET NAME=$P($G(^DPT(TMGDFN,0)),"^",1)
       . . IF NAME["ZZ" QUIT
       . . WRITE NAME,"(",TMGDFN,")",!
       . . WRITE "            Was Due On ",$$EXTDATE^TMGDATE(DATE),!
       . . WRITE "     ------------- ",!
       NEW TMGDFN SET TMGDFN=0
       FOR  SET TMGDFN=$O(^ORAM(103,TMGDFN)) QUIT:TMGDFN'>0  DO
       . NEW LASTDATE 
       . SET LASTDATE=$O(^ORAM(103,TMGDFN,3,"B",9999999),-1)
       . NEW DAYSDIFF SET DAYSDIFF=$$DAYSDIFF^TMGDATE($$TODAY^TMGDATE,LASTDATE)
       . IF DAYSDIFF>44 DO
       . . IF $$ACTIVEPT^TMGPXR03(TMGDFN)'=1 QUIT
       . . IF $$UP^XLFSTR($P($G(^ORAM(103,TMGDFN,0)),"^",7))="COMPLETED" QUIT
       . . NEW NAME SET NAME=$P($G(^DPT(TMGDFN,0)),"^",1)
       . . IF NAME["ZZ" QUIT
       . . WRITE NAME,"(",TMGDFN,")",!
       . . WRITE "            Was Last Done On ",$$EXTDATE^TMGDATE(LASTDATE),"(",DAYSDIFF," ago)",!
       . . WRITE "     ------------- ",!
       QUIT
       ;"
CNSLTRPT(RECORDS,MAKENOTES) ;
       ;"Purpose: Show report of outstanding consults and extract schedule
       ; date from them. (All except mammograms)
       ;"Results: None, but report created.
       SET MAKENOTES=+$GET(MAKENOTES)
       SET RECORDS=+$GET(RECORDS)
       NEW MammoIEN SET MammoIEN=+$ORDER(^GMR(123.5,"B","MAMMOGRAM",""))
       NEW BoneDensIEN SET BoneDensIEN=+$ORDER(^GMR(123.5,"B","BONE DENSITY",""))
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
       . IF IEN=BoneDensIEN QUIT
       . SET ORDERTYPE=$PIECE($GET(^GMR(123.5,IEN,0)),"^",1)
       . IF (RECORDS=0)&(ORDERTYPE["PSYCHIATRY") QUIT
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
       . . IF s["ZZ" QUIT
       . . ;"Now scan for appt scheduled date
       . . NEW idxWP SET idxWP=0
       . . NEW found SET found=0
       . . FOR  SET idxWP=+$ORDER(^GMR(123,idx,20,idxWP)) QUIT:(idxWP'>0)!found  do
      . . . NEW line SET line=$GET(^GMR(123,idx,20,idxWP,0)) QUIT:line=""
       . . . IF line'["An appointment has been scheduled" QUIT
       . . . SET found=1
       . . . SET line=$GET(^GMR(123,idx,20,idxWP+1,0))
       . . . NEW apptDate
       . . . if line["*" do
       . . . . SET apptDate=$PIECE(line,"*",2)
       . . . else  do
       . . . . set apptDate=$$TRIM^XLFSTR(line)
       . . . SET apptDate=$$UP^XLFSTR(apptDate)
       . . . IF apptDate["DAY " DO
       . . . . SET apptDate=$P(apptDate,"DAY ",2)
       . . . IF apptDate[" at " do
       . . . . SET apptDate=$p(apptDate," at ",1)_"@"_$p(apptDate," at ",2)
       . . . IF $P(apptDate,"@",2)="" SET apptDate=$P(apptDate,"@",1)
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
       SET endDate=X+.999999
       ;"SET NowDate=3221212
       ;"SET endDate=3221004
       FOR  SET dueDate=$ORDER(matches(dueDate),1) QUIT:(dueDate="")  do
       . IF (RECORDS>0)&(dueDate>endDate) QUIT
       . IF (RECORDS=0)&(future=1) QUIT
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
MRNOTE(RESULT,TMGDFN,DOS,SPECIALTY)  ;
       ;"Purpose: Automatically create TIU note for sent records and sign
       NEW DOCIEN
       SET TMGDFN=+$GET(TMGDFN),DOS=$GET(DOS)
       SET RESULT="-1^ERROR"
       IF TMGDFN'>0 GOTO MNDN
       IF DOS=0 GOTO MNDN
       DO BLANKTIU^TMGRPC1(.RESULT,TMGDFN,"HAGOOD,EDDIE",6,DOS,"RECORDS SENT")
       IF RESULT>0 DO SEND^TIUALRT(RESULT)
       SET DOCIEN=RESULT
       IF DOCIEN'>0 GOTO MNDN
       ;"DO LOCK^TIUSRVP(.RESULT,DOCIEN)
       NEW TEXT
       SET TEXT("TEXT",1,0)="RECORDS SENT FOR "_SPECIALTY_" VISIT"
       SET TEXT("TEXT",2,0)="{RECORDS SENT}"
       SET TEXT("HDR")="1^1"
       ;
       NEW RESULT DO SETTEXT^TMGTIUS1(.RESULT,DOCIEN,.TEXT,0)
       DO SEND^TIUALRT(DOCIEN)
       ;"D UPDATE^TIUSRVP(.RESULT,DOCIEN,.TEXT,1)
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
       DO ^%ZISC  ;" Close the output device
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
       NEW DOCTYPE,TIUIEN,TMGDFN,TIUDATE,INRIEN,NOWDATE,YEARAGODT
       NEW RESULTARR,X,X1,X2
       DO NOW^%DTC SET NOWDATE=X
       SET X1=NOWDATE,X2=-365
       DO C^%DTC SET YEARAGODT=X              
       SET INRIEN=1457,TIUIEN=0
       FOR  SET TIUIEN=$ORDER(^TIU(8925,"B",INRIEN,TIUIEN)) QUIT:TIUIEN'>0  DO
       . NEW ZN SET ZN=$GET(^TIU(8925,TIUIEN,0))
       . SET TIUDATE=$PIECE(ZN,"^",7)
       . SET TMGDFN=$PIECE(ZN,"^",2)
       . IF TIUDATE>YEARAGODT DO
       . . SET RESULTARR(TMGDFN,TIUDATE)=""
       SET TMGDFN=0
       FOR  SET TMGDFN=$ORDER(RESULTARR(TMGDFN)) QUIT:TMGDFN'>0  DO
       . SET TIUDATE=0
       . NEW PATIENTNAME
       . SET PATIENTNAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
       . WRITE "===== ",PATIENTNAME," ====",!
       . FOR  SET TIUDATE=$ORDER(RESULTARR(TMGDFN,TIUDATE)) QUIT:TIUDATE'>0  DO
       . . NEW Y SET Y=TIUDATE
       . . DO DD^%DT
       . . WRITE "  ->",Y,!
       QUIT
       ;"
PRTPAINR  ;"
       ;"Purpose: Provide an Non-interactive entry point for Pain report,
       ;"Get patient arrays
       NEW CSPTRESULT,REMRESULT  
       NEW X,Y DO NOW^%DTC NEW NOWDATE SET NOWDATE=X
       ;"DON'T PRINT ON WEDNESDAY,SATURDAY,SUNDAY
       NEW DOW SET DOW=$$DOW^XLFDT(NOWDATE)
       NEW DAYSTR SET DAYSTR="WED,SUN,SAT"
       IF DAYSTR[DOW GOTO PPDN  
       ;"
       NEW BDATE,EDATE 
       ;"Note: we are going to start printing reports in advance,
       ;"      so this will try to determine the next open date
       ;"      If returns 0, quit
       ;"NOTE: 5/7/18 - Changing report to print for today only
       ;"NEW NEXTDATE SET NEXTDATE=$$NEXTDATE^TMGPXR03(NOWDATE,3)
       NEW NEXTDATE SET NEXTDATE=$$TODAY^TMGDATE
       IF NEXTDATE'>0 GOTO PPDN
       SET BDATE=NEXTDATE,EDATE=NEXTDATE
       ;"SET BDATE=3200331,EDATE=3200331
       DO GETPRPT(.CSPTRESULT,.REMRESULT,BDATE,EDATE)
       NEW DUEARRAY
       DO GETDBDUE(.DUERESULT,BDATE)
       ;"
       ;"If data is found in either one, proceed with printing else quit
       IF ('$D(CSPTRESULT))&(REMRESULT(0)=0) GOTO PPDN 
       ;"
       NEW %ZIS
       SET %ZIS("A")="Enter Output Device: "
       SET IOP="S121-LAUGHLIN-LASER"
       DO ^%ZIS  ;"standard device call
       IF POP DO  GOTO PPDN
       . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output.  Aborting.")
       use IO
       DO PAINRPT(.CSPTRESULT,.REMRESULT,BDATE,EDATE,.DUERESULT)
       DO ^%ZISC  ;" Close the output device
       ;"DO PRESS2GO^TMGUSRI2
PPDN   QUIT
       ;"
GETPRPT(CSPTRESULT,REMRESULT,BDATE,EDATE)
       ;"Get CS Patients
       DO GETCSPAT^TMGPXR03(.CSPTRESULT,BDATE,EDATE)
       ;"Get patients with reminders due
       NEW REMARR
       SET REMARR(265)="CSDB REVIEW"
       SET REMARR(232)="PAIN CONTRACT DUE"
       SET REMARR(233)="DRUG SCREEN DUE"
       SET REMARR(266)="EKG DUE"
       SET REMARR(272)="MAMMOGRAM DUE"
       SET REMARR(228)="BONE DENSITY DUE"
       SET REMARR(242)="EYE EXAM DUE"
       DO APPTREMS^TMGPXR03(.REMRESULT,.REMARR,BDATE,EDATE)
       QUIT
       ;" 
PAINICD(TMGDFN)
       NEW ICD10,TMGTABLEARR,TMGTABLE SET ICD10="NOT ENTERED"
       SET TMGTABLE=$$GETTABLX^TMGTIUO6(+$G(TMGDFN),"[PAIN MANAGEMENT]",.TMGTABLEARR)
       IF $DATA(TMGTABLEARR) DO
       . NEW ICDDATA SET ICDDATA=$$UP^XLFSTR($GET(TMGTABLEARR("KEY-VALUE","ICD-10 Treatment")))
       . IF ICDDATA'="" SET ICD10=ICDDATA 
       QUIT ICD10
       ;"
PAINRPT(CSPTRESULT,REMRESULT,BDATE,EDATE,CSDBRESULT)   ;"
       ;"Purpose: Print report for patients due for UDS and Pain Contracts
       ;"Print heading
       NEW X,Y DO NOW^%DTC NEW NOWDATE SET NOWDATE=X
       ;"Get date range
       NEW DTRANGE
       SET Y=BDATE DO DD^%DT SET DTRANGE=Y
       IF BDATE'=EDATE DO
       . SET Y=EDATE DO DD^%DT SET DTRANGE=DTRANGE_"-"_Y
       WRITE !
       WRITE "************************************************************",!
       WRITE "              Nursing reminders due ",DTRANGE,!
       WRITE "                      Printed: " SET Y=NOWDATE DO DD^%DT WRITE Y,!
       WRITE "               Please deliver this report to the NURSE",!
       WRITE "************************************************************",!
       WRITE "                                            (From TMGRPT1.m)",!!
       ;"
       ;"Print CS Patients, if found in array
       IF $D(CSPTRESULT) DO
       . NEW DATE SET DATE=0
       . WRITE "============= PATIENTS SCHEDULED, ON CONTROLLED SUBSTANCES ============",!
       . FOR  SET DATE=$ORDER(CSPTRESULT(DATE)) QUIT:DATE'>0  DO
       . . NEW TMGDFN SET TMGDFN=0
       . . FOR  SET TMGDFN=$ORDER(CSPTRESULT(DATE,TMGDFN)) QUIT:TMGDFN'>0  DO
       . . . NEW NAME SET NAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
       . . . NEW ICD10 SET ICD10="ICD-10: "_$$PAINICD(TMGDFN)
       . . . SET Y=DATE DO DD^%DT
       . . . WRITE "  - ",NAME,?30,Y,?55,ICD10,!
       . WRITE !,!
       ;"
       ;"Print CSDB patient due
       ;"WRITE "============= PATIENTS DUE FOR CSDB REVIEW  ============",!
       ;"IF +$G(CSDBRESULT(0))>0 DO
       ;". NEW DATE SET DATE=0
       ;". FOR  SET DATE=$ORDER(DUERESULT(DATE)) QUIT:DATE'>0  DO
       ;". . NEW TMGDFN SET TMGDFN=0
       ;". . FOR  SET TMGDFN=$ORDER(DUERESULT(DATE,TMGDFN)) QUIT:TMGDFN'>0  DO
       ;". . . NEW NAME SET NAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
       ;". . . NEW DOB SET DOB=$PIECE($GET(^DPT(TMGDFN,0)),"^",3)
       ;". . . SET Y=DOB DO DD^%DT SET DOB=Y
       ;". . . NEW APPT SET Y=DATE DO DD^%DT SET APPT=Y
       ;". . . WRITE "  - ",NAME," (",DOB,")",?40,APPT,!
       ;". WRITE !
       ;"
       ;"Print reminders due, if found in array
       IF REMRESULT(0)=0 GOTO PRTDN
       NEW REMIEN SET REMIEN=0
       FOR  SET REMIEN=$ORDER(REMRESULT(REMIEN)) QUIT:REMIEN'>0  DO
       . NEW REMDISP SET REMDISP=$PIECE($GET(^PXD(811.9,REMIEN,0)),"^",3)
       . WRITE "============= PATIENTS DUE FOR ",REMDISP," ============",!
       . NEW DATE SET DATE=0
       . FOR  SET DATE=$ORDER(REMRESULT(REMIEN,DATE)) QUIT:DATE'>0  DO
       . . NEW TMGDFN SET TMGDFN=0
       . . FOR  SET TMGDFN=$ORDER(REMRESULT(REMIEN,DATE,TMGDFN)) QUIT:TMGDFN'>0  DO
       . . . NEW NAME SET NAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
       . . . NEW DOB SET DOB=$PIECE($GET(^DPT(TMGDFN,0)),"^",3)
       . . . SET Y=DOB DO DD^%DT SET DOB=Y
       . . . SET Y=DATE DO DD^%DT
       . . . WRITE "  - ",NAME," (",DOB,")",?40,Y,!
       . . . IF REMIEN=272 DO
       . . . . NEW SCHEDULED SET SCHEDULED=$$MAMSCHED(TMGDFN)
       . . . . IF SCHEDULED'="" WRITE "       ["_SCHEDULED,"]",!
       . WRITE !!
       ;"
       ;"Check TSH values for patients. If one is H or L, add to list
       WRITE "============= PATIENTS WITH OUT OF RANGE TSH ==================",!
       NEW PTARRAY,DATE
       DO GETSCHED^TMGPXR03(.PTARRAY,BDATE,EDATE)
       NEW TMGDFN SET TMGDFN=0
       FOR  SET TMGDFN=$ORDER(PTARRAY(TMGDFN)) QUIT:TMGDFN'>0  DO
       . NEW RESULTS,TESTRESULT
       . DO GETVALS^TMGLRR01(TMGDFN_"^2",110,.RESULTS)  ;"TSH. THIS CAN BE EXPANDED TO OTHER TESTS THOUGH
       . NEW TESTNAME,DATE SET TESTNAME=1
       . FOR  SET TESTNAME=$O(RESULTS(TESTNAME)) QUIT:+TESTNAME'>0  DO
       . . SET DATE=9999999
       . . SET DATE=$O(RESULTS(TESTNAME,DATE),-1)
       . . SET TESTRESULT=$G(RESULTS(TESTNAME,DATE))
       . . IF (TESTRESULT["H")!(TESTRESULT["L") DO
       . . . NEW NAME SET NAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
       . . . SET Y=DATE DO DD^%DT
       . . . WRITE "  - ",NAME,?30,"had a TSH of ",TESTRESULT," on ",$P(Y,"@",1),!
       WRITE !,!
       ;"
       ;"Check for allergy lists that haven't been accessed
       WRITE "========= PATIENTS WITH UNACCESSED ALLERGY LISTS==============",!
       SET TMGDFN=0
       FOR  SET TMGDFN=$ORDER(PTARRAY(TMGDFN)) QUIT:TMGDFN'>0  DO
       . NEW ALLERGIES SET ALLERGIES=$$ALLERGY^TMGTIUO3(TMGDFN)
       . IF (ALLERGIES["NEEDS ALLERGY ASSESSMENT")!(ALLERGIES="") DO
       . . NEW NAME,DOB,Y SET DOB=$PIECE($GET(^DPT(TMGDFN,0)),"^",3)
       . . SET NAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
       . . SET Y=DOB DO DD^%DT SET DOB=Y
       . . WRITE "  - ",NAME," (",DOB,")",!
PRTDN  QUIT
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
       . NEW TMGDFN SET TMGDFN=0
       . FOR  SET TMGDFN=$ORDER(DUERESULT(DATE,TMGDFN)) QUIT:TMGDFN'>0  DO
       . . NEW NAME SET NAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
       . . NEW DOB SET DOB=$PIECE($GET(^DPT(TMGDFN,0)),"^",3)
       . . SET Y=DOB DO DD^%DT SET DOB=Y
       . . NEW APPT SET Y=DATE DO DD^%DT SET APPT=Y
       . . WRITE "[ ] ",NAME,?28,"(",DOB,")",?45,APPT,!
       WRITE !
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
       NEW TMGDFN SET TMGDFN=0
       FOR  SET TMGDFN=$ORDER(PTARRAY(TMGDFN)) QUIT:TMGDFN'>0  DO
       . NEW MEDARRAY,OUT
       . DO MEDLIST^TMGTIUOJ(.OUT,TMGDFN,.MEDARRAY)
       . ;"Use old method for now since it includes the "NEEDS UPDATE" tag. 
       . ;"In future, research below method to find out why new way doesn't
       . ;"pull it, then use below method instead
       . ;"DO MEDARR^TMGTIUOJ(.OUT,TMGDFN,.MEDARRAY)   ;"//kt 5/7/18 
       . IF $GET(MEDARRAY("KEY-VALUE","*CSM-DATABASE REVIEW","LINE"))["NEEDS UPDATE" DO
       . . SET COUNT=COUNT+1
       . . SET TMGRESULT($ORDER(PTARRAY(TMGDFN,0)),TMGDFN)=""
       SET TMGRESULT(0)=COUNT
       QUIT
       ;"
LABS2DAY  ;"Patients scheduled for today that need blood work drawn today
       NEW PTARRAY,TODAY,OUTARR,DATE
       SET TODAY=$$TODAY^TMGDATE
       DO GETSCHED^TMGPXR03(.PTARRAY,TODAY,TODAY)
       NEW TMGDFN SET TMGDFN=0
       FOR  SET TMGDFN=$O(PTARRAY(TMGDFN)) QUIT:TMGDFN'>0  DO
       . SET DATE=""
       . FOR  SET DATE=$O(PTARRAY(TMGDFN,DATE)) QUIT:DATE=""  DO
       . . NEW COMMENT SET COMMENT=$P($G(PTARRAY(TMGDFN,DATE)),"^",3)
       . . IF COMMENT["LABS AT NEXT APPOINTMENT" DO
       . . . SET OUTARR(DATE,TMGDFN)=COMMENT
       IF $D(OUTARR) DO
       . NEW %ZIS
       . SET %ZIS("A")="Enter Output Device: "
       . SET IOP="S121-LAUGHLIN-LASER"
       . DO ^%ZIS  ;"standard device call
       . IF POP DO  GOTO DEEDN
       . . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output. Aborting.")
       . use IO
       . WRITE !
       . WRITE "************************************************************",!
       . WRITE "         Patients who needs labs at their appt today",!
       . WRITE "                       ",$$EXTDATE^TMGDATE(TODAY,1),!
       . WRITE "          Please deliver this report to reception",!
       . WRITE "************************************************************",!
       . WRITE "                                            (From TMGRPT1.m)",!!
       . SET DATE=""
       . FOR  SET DATE=$O(OUTARR(DATE)) QUIT:DATE=""  DO
       . . SET TMGDFN=0
       . . FOR  SET TMGDFN=$O(OUTARR(DATE,TMGDFN)) QUIT:TMGDFN'>0  DO
       . . . NEW HOUR,MINS
       . . . SET HOUR=$E($P(DATE,".",2),1,2),MINS=$E($P(DATE,".",2),3,4)
       . . . IF $L(HOUR)=1 SET HOUR=HOUR_"0"
       . . . IF $L(MINS)=0 SET MINS="00"
       . . . IF $L(MINS)=1 SET MINS=MINS_"0"
       . . . WRITE HOUR,":",MINS," -> ",$P($G(^DPT(TMGDFN,0)),"^",1)," ",$G(OUTARR(DATE,TMGDFN)),!
       . DO ^%ZISC 
       QUIT
       ;"  
MISSROS(CHKDATE)   ;"TODAY'S OFFICE NOTES MISSING ROS
       NEW TIUARR,DATE,TIUIEN
       IF +$G(CHKDATE)'>0 SET CHKDATE=$$TODAY^TMGDATE
       SET TIUIEN=0
       ;"SET CHKDATE=3230323
       SET DATE=CHKDATE
       FOR  SET DATE=$O(^TIU(8925,"D",DATE)) QUIT:DATE=""  DO
       . SET TIUIEN=0
       . FOR  SET TIUIEN=$O(^TIU(8925,"D",DATE,TIUIEN)) QUIT:(TIUIEN'>0)!(DATE'[CHKDATE)  DO
       . . NEW NOTETYPE SET NOTETYPE=$P($G(^TIU(8925,TIUIEN,0)),"^",1)
       . . NEW OFFNOTE SET OFFNOTE=$P($G(^TIU(8925.1,NOTETYPE,"TMGH")),"^",1)
       . . IF OFFNOTE="Y" SET TIUARR(TIUIEN)=""
       NEW MISSEDARR,TODAY SET TODAY=$$TODAY^TMGDATE
       SET TIUIEN=0
       FOR  SET TIUIEN=$O(TIUARR(TIUIEN)) QUIT:TIUIEN'>0  DO
       . NEW ROSFOUND SET ROSFOUND=0
       . NEW TMGDFN SET TMGDFN=$P($G(^TIU(8925,TIUIEN,0)),"^",2)
       . NEW DATETIME SET DATETIME=CHKDATE
       . FOR  SET DATETIME=$O(^TIU(8925,"ZTMGPTDT",TMGDFN,DATETIME)) QUIT:(DATETIME'>0)!($P(DATETIME,".",1)'=CHKDATE)  DO
       . . NEW THISTIU SET THISTIU=0
       . . FOR  SET THISTIU=$O(^TIU(8925,"ZTMGPTDT",TMGDFN,DATETIME,THISTIU)) QUIT:THISTIU'>0  DO
       . . . NEW NOTETYPE SET NOTETYPE=$P($G(^TIU(8925,THISTIU,0)),"^",1)
       . . . IF NOTETYPE=1424 SET ROSFOUND=1
       . IF ROSFOUND=0 SET MISSEDARR(TMGDFN,TIUIEN)=""
       IF $D(MISSEDARR) DO
       . NEW %ZIS
       . SET %ZIS("A")="Enter Output Device: "
       . SET IOP="S121-LAUGHLIN-LASER"
       . DO ^%ZIS  ;"standard device call
       . IF POP DO  GOTO DEEDN
       . . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output. Aborting.")
       . use IO
       . WRITE !
       . WRITE "************************************************************",!
       . WRITE "         OFFICE NOTES WITHOUT ROS SCANNED INTO SYSTEM",!
       . WRITE "                       ",$$EXTDATE^TMGDATE(TODAY,1),!
       . WRITE "          Please deliver this report to Eddie",!
       . WRITE "************************************************************",!
       . WRITE "                                            (From TMGRPT1.m)",!!
       . NEW TMGDFN SET TMGDFN=0
       . FOR  SET TMGDFN=$O(MISSEDARR(TMGDFN)) QUIT:TMGDFN'>0  DO
       . . WRITE $P($G(^DPT(TMGDFN,0)),"^",1)," -> "
       . . SET TIUIEN=0
       . . FOR  SET TIUIEN=$O(MISSEDARR(TMGDFN,TIUIEN)) QUIT:TIUIEN'>0  DO
       . . . W TIUIEN,","
       . . W !
       . DO ^%ZISC 
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
       . NEW ZN,TMGDFN,DATE,INS,INSIDX,INCLUDE,NOTEIEN
       . SET ZN=$GET(^TIU(8925,IDX,0))
       . SET DATE=$P(ZN,"^",7)
       . ;"WRITE DATE,!
       . IF DATE<3160101 QUIT
       . SET NOTEIEN=$P(ZN,"^",1)
       . ;"WRITE NOTEIEN,!
       . IF (NOTEIEN'=1408)&(NOTEIEN'=1399)&(NOTEIEN'=1402)&(NOTEIEN'=1983)&(NOTEIEN'=6)&(NOTEIEN'=7) QUIT
       . SET TMGDFN=$P(ZN,"^",2)
       . SET INSIDX=0,INCLUDE=0
       . FOR  SET INSIDX=$ORDER(^DPT(TMGDFN,.312,INSIDX)) QUIT:INSIDX'>0  DO
       . . NEW INSIEN SET INSIEN=$GET(^DPT(TMGDFN,.312,INSIDX,0))
       . . IF INSIEN=3 SET INCLUDE=1
       . IF INCLUDE=0 QUIT
       . NEW NAME SET NAME=$P($G(^DPT(TMGDFN,0)),"^",1)
       . NEW ICDIEN SET ICDIEN=0
       . FOR  SET ICDIEN=$ORDER(^AUPNVPOV("C",TMGDFN,ICDIEN)) QUIT:+ICDIEN'>0  DO
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
       NEW TMGDFN,PTARRAY,NOVISITARRAY SET TMGDFN=0
       FOR  SET TMGDFN=$ORDER(^DPT(TMGDFN)) QUIT:TMGDFN'>0  DO
       . IF $$ACTIVEPT^TMGPXR03(TMGDFN)'=1 QUIT
       . ;"GET LAST VISIT DATE
       . NEW LASTDT,VSTIEN SET LASTDT=0,VSTIEN=0
       . FOR  SET VSTIEN=$ORDER(^TMG(22723,TMGDFN,1,VSTIEN)) QUIT:VSTIEN'>0  DO
       . . NEW VSTDATE SET VSTDATE=$P($G(^TMG(22723,TMGDFN,1,VSTIEN,0)),"^",1)
       . . IF LASTDT<VSTDATE SET LASTDT=VSTDATE
       . IF LASTDT>0 QUIT
       . SET PTARRAY($P($G(^DPT(TMGDFN,0)),"^",1))=TMGDFN
       NEW NAME SET NAME=""
       FOR  SET NAME=$ORDER(PTARRAY(NAME)) QUIT:NAME=""  DO
       . NEW NOTEDATE,LASTON SET NOTEDATE=9999999,LASTON=0
       . SET TMGDFN=$GET(PTARRAY(NAME))
       . FOR  SET NOTEDATE=$ORDER(^TIU(8925,"APTP",TMGDFN,NOTEDATE),-1) QUIT:(NOTEDATE'>0)!(LASTON>0)  DO
       . . NEW TIUIEN SET TIUIEN=$ORDER(^TIU(8925,"APTP",TMGDFN,NOTEDATE,0))
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
       ;"
PRINTRPT(MUMPSCODE)  ;"
       ;"AUTO PRINT A REPORT TO THE PRINTER
       ;"MUMPSCODE WILL BE CALLED AFTER PRINTER IS SET UP
       NEW %ZIS
       SET %ZIS("A")="Enter Output Device: "
       SET IOP="S121-LAUGHLIN-LASER"
       DO ^%ZIS  ;"standard device call
       IF POP DO  GOTO PRDN
       . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output. Aborting.")
       use IO
       X MUMPSCODE
       DO ^%ZISC
PRDN   QUIT
       ;"
EKGSDUE()  ;"SCRATCH FUNCTION
       NEW %ZIS
       SET %ZIS("A")="Enter Output Device: "
       SET IOP="S121-LAUGHLIN-LASER"
       DO ^%ZIS  ;"standard device call
       IF POP DO  GOTO EKGDN
       . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output. Aborting.")
       use IO
       DO NOW^%DTC
       WRITE !
       WRITE "****************************************************************",!
       WRITE "         Patients who have nursing reminders due today",!
       WRITE "                " SET Y=X DO DD^%DT WRITE Y,!
       WRITE "****************************************************************",!
       WRITE "                                        (From TMGRPT1.m)",!!
       WRITE " ",!
       ;"Get patients with reminders due
       NEW REMARR
       SET REMARR(266)="EKG DUE"
       SET REMARR(272)="MAMMOGRAM DUE"
       SET REMARR(228)="BONE DENSITY DUE"
       SET REMARR(242)="EYE EXAM DUE"
       DO APPTREMS^TMGPXR03(.REMRESULT,.REMARR,$$TODAY^TMGDATE,3180131)
       IF REMRESULT(0)=0 GOTO EKGDN
       NEW REMIEN SET REMIEN=0
       FOR  SET REMIEN=$ORDER(REMRESULT(REMIEN)) QUIT:REMIEN'>0  DO
       . NEW REMDISP SET REMDISP=$PIECE($GET(^PXD(811.9,REMIEN,0)),"^",3)
       . WRITE "============= PATIENTS DUE FOR ",REMDISP," ============",!
       . NEW DATE SET DATE=0
       . FOR  SET DATE=$ORDER(REMRESULT(REMIEN,DATE)) QUIT:DATE'>0  DO
       . . NEW TMGDFN SET TMGDFN=0
       . . FOR  SET TMGDFN=$ORDER(REMRESULT(REMIEN,DATE,TMGDFN)) QUIT:TMGDFN'>0  DO
       . . . NEW NAME SET NAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
       . . . SET Y=DATE DO DD^%DT
       . . . WRITE "  - ",NAME,?40,Y,!
       . WRITE !!
EKGDN
       DO ^%ZISC  ;" Close the output device
       QUIT
       ;"
P23NEWRM  ;"SCRATCH FUNCTION WHICH CAN BE DELETED LATER   ELH    2/6/18
       NEW REMARR
       SET REMARR(257)="NEW PNEUMOCOCCAL REMINDER"
       DO APPTREMS^TMGPXR03(.REMRESULT,.REMARR,$$TODAY^TMGDATE,3180430)
       IF REMRESULT(0)=0 QUIT
       NEW %ZIS
       SET %ZIS("A")="Enter Output Device: "
       SET IOP="S121-LAUGHLIN-LASER"
       DO ^%ZIS  ;"standard device call
       IF POP DO  GOTO EKGDN
       . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output. Aborting.")
       use IO
       NEW REMIEN SET REMIEN=0
       FOR  SET REMIEN=$ORDER(REMRESULT(REMIEN)) QUIT:REMIEN'>0  DO
       . NEW REMDISP SET REMDISP=$PIECE($GET(^PXD(811.9,REMIEN,0)),"^",3)
       . WRITE "============= PATIENTS DUE FOR ",REMDISP," ============",!
       . NEW DATE SET DATE=0
       . FOR  SET DATE=$ORDER(REMRESULT(REMIEN,DATE)) QUIT:DATE'>0  DO
       . . NEW TMGDFN SET TMGDFN=0
       . . FOR  SET TMGDFN=$ORDER(REMRESULT(REMIEN,DATE,TMGDFN)) QUIT:TMGDFN'>0  DO
       . . . NEW AGE K VADM SET AGE=$$AGE^TIULO(TMGDFN)
       . . . IF AGE>64 QUIT
       . . . NEW SEQL SET SEQL=$P($G(^DPT(TMGDFN,"TMG")),"^",2)
       . . . NEW NAME SET NAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)_" ("_SEQL_")"
       . . . SET Y=DATE DO DD^%DT
       . . . WRITE "  - ",NAME,?40,Y,?65,AGE,!
       . WRITE !!
       DO ^%ZISC  ;" Close the output device
       QUIT
       ;"
MAMSCHED(TMGDFN)  ;"IS MAMMO SCHEDULED
       NEW RESULT SET RESULT=""
       NEW MammoIEN SET MammoIEN=+$ORDER(^GMR(123.5,"B","MAMMOGRAM",""))
       IF MammoIEN'>0 DO  GOTO MRPTDn
       . WRITE "Can't locate record for MAMMOGRAM report.  Aborting.",!
       NEW ComplIEN SET ComplIEN=+$ORDER(^ORD(100.01,"B","COMPLETE",""))
       IF ComplIEN'>0 DO  GOTO MRPTDn
       . WRITE "Can't find record for COMPLETE status.  Aborting.",!
       NEW DCIEN SET DCIEN=+$ORDER(^ORD(100.01,"B","DISCONTINUED",""))
       NEW X,Y DO NOW^%DTC NEW NowDate SET NowDate=X
       ;
       NEW idx SET idx=""
       NEW matches,IENLIST
       FOR  SET idx=+$ORDER(^GMR(123,"F",TMGDFN,idx)) QUIT:(idx'>0)  do
       . NEW TYPE
       . NEW znode SET znode=$GET(^GMR(123,idx,0))
       . SET TYPE=$P(znode,"^",5)
       . IF TYPE'=MammoIEN QUIT
       . NEW status SET status=$PIECE(znode,"^",12)
       . IF status=ComplIEN QUIT
       . IF status=DCIEN QUIT
       . NEW Y SET Y=$PIECE(znode,"^",7)  ;"date of request
       . DO DD^%DT SET RESULT="Mammogram is scheduled for "_Y
       ;. ;"Now scan for appt scheduled date
       ;. NEW idxWP SET idxWP=0
       ;. NEW found SET found=0
       ;. FOR  SET idxWP=+$ORDER(^GMR(123,idx,20,idxWP)) QUIT:(idxWP'>0)!found  do
       ;. . NEW line SET line=$GET(^GMR(123,idx,20,idxWP,0)) QUIT:line=""
       ;. . IF line'["Scheduled Appointment:" QUIT
       ;. . SET found=1
       ;. . NEW apptDate SET apptDate=$PIECE(line,"Scheduled Appointment:",2)
       ;. . SET Y=$$FMDate^TMGFMUT(apptDate)
       ;. . NEW FMDate SET FMDate=Y
       ;. . IF Y>0 do
       ;. . . DO DD^%DT  ;"standardize date
       ;. . ELSE  do
       ;. . . SET Y=apptDate
       ;. . . SET FMDate=NowDate ;Assume Due Now If Can't Resolve Date
       ;. . SET s=s_"^"_Y
       QUIT RESULT
       ;"
FLUAGE  ;" REPORT TO DETERMINE AGE OF PATIENTS WHO GOT FLU VACCINATIONS
       NEW IDX SET IDX=0
       NEW TOTALCNT,OVER65CNT
       SET TOTALCNT=0,OVER65CNT=0
       FOR  SET IDX=$O(^AUPNVCPT("B",90686,IDX)) QUIT:IDX'>0  DO
       . NEW VISIT,DATE,TMGDFN
       . SET VISIT=$P($G(^AUPNVCPT(IDX,0)),"^",3)
       . SET DATE=$P($G(^AUPNVSIT(VISIT,0)),"^",1)
       . IF DATE<3200501 QUIT
       . SET TOTALCNT=TOTALCNT+1
       . SET TMGDFN=$P($G(^AUPNVCPT(IDX,0)),"^",2)
       . NEW AGE K VADM SET AGE=+$$AGE^TIULO(TMGDFN)
       . IF AGE<65 QUIT
       . W AGE," INCLUDED",!
       . SET OVER65CNT=OVER65CNT+1
       W TOTALCNT," TOTAL FLU VACCINATIONS",!,OVER65CNT," WERE OVER 65",!
       QUIT
       ;"
NEWAPPT  ;"single use function that can be deleted if no longer needed
       ;"prints all patients who had a new patient appointment between the 
       ;"below date ranges
       NEW %ZIS,IOP
       SET IOP="S121-LAUGHLIN-LASER"
       DO ^%ZIS  ;"standard device call
       IF POP DO  GOTO INRDn
       . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output.  Aborting.")
       use IO       
       NEW BDT,EDT SET BDT=3210109,EDT=3210410
       WRITE "*****************************************************************************************",!
       WRITE "****      NEW PATIENT APPOINTMENTS BETWEEN ",$$EXTDATE^TMGDATE(BDT,1)," AND ",$$EXTDATE^TMGDATE(EDT,1),!
       WRITE "*****************************************************************************************",!
       NEW THISDT SET THISDT=BDT
       FOR  SET THISDT=$O(^TMG(22723,"DT",THISDT)) QUIT:(THISDT'>0)!(THISDT>EDT)  DO
       . NEW TMGDFN SET TMGDFN=0
       . FOR  SET TMGDFN=$O(^TMG(22723,"DT",THISDT,TMGDFN)) QUIT:(TMGDFN'>0)  DO
       . . NEW IDX SET IDX=0
       . . FOR  SET IDX=$O(^TMG(22723,"DT",THISDT,TMGDFN,IDX)) QUIT:(IDX'>0)  DO
       . . . NEW ZN SET ZN=$G(^TMG(22723,TMGDFN,1,IDX,0))
       . . . NEW REASON SET REASON=$P(ZN,"^",4)
       . . . IF REASON'["NEW" QUIT
       . . . NEW STATUS SET STATUS=$P(ZN,"^",7)
       . . . IF STATUS="C" QUIT
       . . . WRITE $P($G(^DPT(TMGDFN,0)),"^",1)," (",$$EXTDATE^TMGDATE($P($G(^DPT(TMGDFN,0)),"^",3),1),") VISIT ON ",$$EXTDATE^TMGDATE(THISDT,1),!
       DO ^%ZISC  ;" Close the output device
       QUIT
       ;"
CNSLTRPT2() ;
       ;"Purpose: Show report of outstanding consults and extract schedule
       ; date from them. (All except mammograms)
       ;"Results: None, but report created.
       SET RECORDS=+$GET(RECORDS)
       NEW MammoIEN SET MammoIEN=+$ORDER(^GMR(123.5,"B","MAMMOGRAM",""))
       NEW BoneDensIEN SET BoneDensIEN=+$ORDER(^GMR(123.5,"B","BONE DENSITY",""))
       IF MammoIEN'>0 DO  GOTO CNSTDn
       . WRITE "Can't locate record for MAMMOGRAM report.  Aborting.",!
       NEW ComplIEN SET ComplIEN=+$ORDER(^ORD(100.01,"B","COMPLETE",""))
       IF ComplIEN'>0 DO  GOTO CNSTDn
       . WRITE "Can't find record for COMPLETE status.  Aborting.",!
       NEW DCIEN SET DCIEN=+$ORDER(^ORD(100.01,"B","DISCONTINUED",""))
       NEW CANCELIEN SET CANCELIEN=+$ORDER(^ORD(100.01,"B","CANCELLED",""))
       NEW X,Y DO NOW^%DTC NEW NowDate SET NowDate=X
       NEW %ZIS
       SET %ZIS("A")="Enter Output Device: "
       SET IOP="S121-LAUGHLIN-LASER"
       DO ^%ZIS  ;"standard device call
       IF POP DO  GOTO CSPDn
       . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output. Aborting.")
       use IO
       ;
       WRITE !
       WRITE "************************************************************",!
       WRITE "              Outstanding consults report",!
       WRITE "                     " SET Y=X DO DD^%DT WRITE Y,!
       WRITE "          Please deliver this report to the nurse",!
       WRITE "************************************************************",!
       WRITE "                                            (From TMGRPT1.m)",!!
       NEW idx SET idx=""
       NEW IEN SET IEN=0
       NEW matches
       NEW ORDERTYPE
       FOR  SET IEN=+$ORDER(^GMR(123,"C",IEN)) QUIT:(IEN'>0)  DO
       . IF IEN=MammoIEN QUIT
       . IF IEN=BoneDensIEN QUIT
       . SET ORDERTYPE=$PIECE($GET(^GMR(123.5,IEN,0)),"^",1)
       . IF (RECORDS=0)&(ORDERTYPE["PSYCHIATRY") QUIT
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
       . . IF s["ZZ" QUIT
       . . ;"Now scan for appt scheduled date
       . . NEW idxWP SET idxWP=0
       . . NEW found SET found=0
       . . FOR  SET idxWP=+$ORDER(^GMR(123,idx,20,idxWP)) QUIT:(idxWP'>0)!found  do
       . . . NEW line SET line=$GET(^GMR(123,idx,20,idxWP,0)) QUIT:line=""
       . . . IF line'["An appointment has been scheduled" QUIT
       . . . SET found=1
       . . . SET line=$GET(^GMR(123,idx,20,idxWP+1,0))
       . . . NEW apptDate
       . . . if line["*" do
       . . . . SET apptDate=$PIECE(line,"*",2)
       . . . else  do
       . . . . set apptDate=$$TRIM^XLFSTR(line)
       . . . SET apptDate=$$UP^XLFSTR(apptDate)
       . . . IF apptDate["DAY " DO
       . . . . SET apptDate=$P(apptDate,"DAY ",2)
       . . . IF apptDate[" at " do
       . . . . SET apptDate=$p(apptDate," at ",1)_"@"_$p(apptDate," at ",2)
       . . . IF $P(apptDate,"@",2)="" SET apptDate=$P(apptDate,"@",1)
       . . . SET Y=$$FMDate^TMGFMUT(apptDate)
       . . . NEW FMDate SET FMDate=Y
       . . . IF Y>0 do
       . . . . DO DD^%DT  ;"standardize date
       . . . ELSE  do
       . . . . SET Y=apptDate
       . . . . SET FMDate=NowDate ;Assume Due Now If Can't Resolve Date
       . . . SET s=s_"^"_Y_"^"_ORDERTYPE
       . . . SET matches(ORDERTYPE,FMDate,s)=""
       . . . ;"
       . . . NEW SUBX SET SUBX=1
       . . . NEW SUBIEN SET SUBIEN=0
       . . . FOR  SET SUBIEN=$O(^GMR(123,idx,50,SUBIEN)) QUIT:SUBIEN'>0  DO
       . . . . NEW RESCHIEN SET RESCHIEN=2230
       . . . . NEW TIUIEN SET TIUIEN=$G(^GMR(123,idx,50,SUBIEN,0))
       . . . . IF TIUIEN["TIU" DO
       . . . . . SET TIUIEN=+TIUIEN
       . . . . . IF $P($G(^TIU(8925,TIUIEN,0)),"^",1)'=RESCHIEN QUIT
       . . . . . NEW TEXTLINE SET TEXTLINE=0
       . . . . . FOR  SET TEXTLINE=$O(^TIU(8925,TIUIEN,"TEXT",TEXTLINE)) QUIT:TEXTLINE'>0  DO
       . . . . . . NEW LINE SET LINE=$G(^TIU(8925,TIUIEN,"TEXT",TEXTLINE,0))
       . . . . . . IF LINE["New date is" DO
       . . . . . . . NEW NEWDATE SET NEWDATE=$P($P(LINE,"<I>",2),"</B",1)
       . . . . . . . SET matches(ORDERTYPE,FMDate,s,SUBX)="RESCHEDULED FOR: "_NEWDATE
       . . . . . . . SET SUBX=SUBX+1
       . . . ;"CHECK FOR UNSIGNED NOTES
       . . . NEW UNSIGNED SET UNSIGNED=""
       . . . NEW TIUIEN SET TIUIEN=0
       . . . FOR  SET TIUIEN=$O(^TIU(8925,"C",PtIEN,TIUIEN)) QUIT:TIUIEN'>0  DO
       . . . . NEW STATUS SET STATUS=$P($G(^TIU(8925,TIUIEN,0)),"^",5)
       . . . . IF STATUS=5 DO
       . . . . . NEW TITLE SET TITLE=$P($G(^TIU(8925,TIUIEN,0)),"^",1)
       . . . . . SET TITLE=$P($G(^TIU(8925.1,TITLE,0)),"^",1)
       . . . . . SET TITLE=$P(TITLE," ",1)
       . . . . . ;"IF TITLE["INSURANCE" QUIT
       . . . . . ;"IF TITLE["HL7" QUIT
       . . . . . ;"IF TITLE["HOSPITAL" QUIT
       . . . . . ;"IF TITLE["MAMMO" QUIT
       . . . . . SET TITLE=$E(TITLE,1,7)
       . . . . . IF ORDERTYPE'[TITLE QUIT
       . . . . . IF UNSIGNED[TITLE QUIT
       . . . . . IF UNSIGNED'="" SET UNSIGNED=UNSIGNED_","
       . . . . . SET UNSIGNED=UNSIGNED_$E(TITLE,1,7)
       . . . IF UNSIGNED'="" DO
       . . . . SET matches(ORDERTYPE,FMDate,s,SUBX)="UNSIGNED NOTES: "_UNSIGNED
       . . . . SET SUBX=SUBX+1
       ;"
       NEW future SET future=0
       NEW dueDate,RESULT,SENT,QUESTION,endDate
       NEW X1,X2,X
       SET X1=NowDate,X2=1
       DO C^%DTC
       IF RECORDS>0 SET dueDate=X
       ELSE  SET dueDate=""
       SET X1=X,X2=7
       DO C^%DTC
       SET endDate=X+.999999
       ;"SET NowDate=3181231
       ;"SET endDate=3190107
       SET ORDERTYPE=""
       FOR  SET ORDERTYPE=$ORDER(matches(ORDERTYPE)) QUIT:(ORDERTYPE="")  DO
       . SET dueDate=""
       . FOR  SET dueDate=$ORDER(matches(ORDERTYPE,dueDate),1) QUIT:(dueDate="")  do
       . . IF (dueDate>NowDate) QUIT
       . . NEW s SET s=""
       . . FOR  SET s=$Order(matches(ORDERTYPE,dueDate,s)) QUIT:s=""  do
       . . . WRITE $P(s,"^",4),?18,"Appt Date: ",$p(s,"^",3),?50,$p($p(s,"^",2),"-",1),?57,! ;""Made: ",$p($p(s,"^",1),"@",1),!
       . . . NEW SUBIDX SET SUBIDX=0
       . . . FOR  SET SUBIDX=$Order(matches(ORDERTYPE,dueDate,s,SUBIDX)) QUIT:SUBIDX'>0  DO
       . . . . WRITE ?18,"========>",$G(matches(ORDERTYPE,dueDate,s,SUBIDX)),!
CNST2Dn 
       DO ^%ZISC  ;" Close the output device    
       QUIT
	;
CHKSCHED()  ;"  Check the next 4 weeks to see if that date has less that 10 patients. If so print report listing dates
       NEW BEGDT,ENDDT
       NEW X,Y DO NOW^%DTC SET BEGDT=X
       SET ENDDT=$$ADDDAYS^TMGDATE(31)
       NEW ARRAY,SCHEDULE
       DO GETSCHED^TMGPXR03(.ARRAY,BEGDT,ENDDT,"A")  ;"GET ACTIVE 
       NEW DFN,DATETIME
       SET DFN=0
       FOR  SET DFN=$O(ARRAY(DFN)) QUIT:DFN'>0  DO
       . SET DATETIME=0
       . FOR  SET DATETIME=$O(ARRAY(DFN,DATETIME)) QUIT:DATETIME=""  DO
       . . NEW DAY SET DAY=$P(DATETIME,".",1)
       . . SET SCHEDULE(DAY)=+$G(SCHEDULE(DAY))+1
       NEW WARNDAYS
       SET DATETIME=0
       FOR  SET DATETIME=$O(SCHEDULE(DATETIME)) QUIT:DATETIME'>0  DO
       . IF $G(SCHEDULE(DATETIME))<11 SET WARNDAYS(DATETIME)=$G(SCHEDULE(DATETIME))
       IF '$D(WARNDAYS) QUIT
       NEW %ZIS
       SET %ZIS("A")="Enter Output Device: "
       SET IOP="S121-LAUGHLIN-LASER"
       DO ^%ZIS  ;"standard device call
       IF POP DO  GOTO CSPDn
       . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output. Aborting.")
       use IO
       SET DATETIME=0
       WRITE !
       WRITE "***************************************************************",!
       WRITE "   Office days in the next 4 weeks with less than 11 patients",!
       WRITE "                   " SET Y=X DO DD^%DT WRITE Y,!
       WRITE "           Please deliver this report to the reception",!
       WRITE "***************************************************************",!
       WRITE "                                            (From TMGRPT1.m)",!!
       FOR  SET DATETIME=$O(WARNDAYS(DATETIME)) QUIT:DATETIME'>0  DO
       . WRITE $$EXTDATE^TMGDATE(DATETIME)," has ",$G(WARNDAYS(DATETIME))," patients",!
       DO ^%ZISC  ;" Close the output device  
       QUIT
       ;"
TEST
   DO TEST1
   QUIT
   ;"
TEST1
   QUIT "YES"
   ;"
       


























