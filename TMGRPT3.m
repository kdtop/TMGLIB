TMGRPT3 ;TMG/kst TMG REPORTS ; 9/7/15    
        ;;1.0;TMG-LIB;**1**;9/7/15
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 9/7/15  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
PTINQ(DFN)  ;"Replacement for DGINQB^ORCXPND1 patient inquiry.
  WRITE "<HTML>",!
  WRITE "<H2>Patient Demographics</H2>",!
  NEW IDX,ARR,FLDS SET FLDS=""
  ;"Format FLDS(Seq#)=Template
  ;"    Piece# 1 -- Field name
  ;"           2 -- DisplayName
  ;"           3 -- boolShowIfEmpty.  1= Show if empty. Default 0
  ;"           4 -- Parse routine.  A field parser function
  DO  
  . SET FLDS(1)=".01^NAME^1"
  . SET FLDS(2)=".02^SEX^1"
  . SET FLDS(3)=".03^DATE OF BIRTH^1"
  . SET FLDS(4)=".033^AGE^1^PARSYR"
  . SET FLDS(5)=".09^SSN^1^PARSSSN"
  . SET FLDS(6)=".05^MARITAL STATUS"  
  . SET FLDS(7)=".111^ADDRESS^1"
  . SET FLDS(8)=".112^ADDRESS"
  . SET FLDS(9)=".113^ADDRESS"
  . SET FLDS(10)=".114^CITY^1"
  . SET FLDS(11)=".115^STATE^1"
  . SET FLDS(12)=".116^ZIP CODE^1"
  . SET FLDS(13)=".131^HOME PHONE^1^PARSPHON"
  . SET FLDS(14)=".134^CELL PHONE^1^PARSPHON"
  . SET FLDS(15)=".1041^PHYSICIAN" 
  SET IDX=0 FOR  SET IDX=$ORDER(FLDS(IDX)) QUIT:+IDX'>0  DO
  . SET FLDS=FLDS_$P($G(FLDS(IDX)),"^",1)_";"
  NEW ARR
  DO GETINFO(DFN,.ARR,FLDS) ;"Get needed patient info
  IF $DATA(ARR("ERR")) DO  GOTO PTCH
  . WRITE "<B>ERROR</B><BR>",!
  . SET IDX=0 FOR  SET IDX=$ORDER(ARR("ERR",IDX)) QUIT:IDX=""  DO  
  . . WRITE $GET(ARR("ERR",IDX)),"<BR>",!
  WRITE "<TABLE BORDER=0>",!
  SET IDX=0 FOR  SET IDX=$ORDER(FLDS(IDX)) QUIT:+IDX'>0  DO
  . NEW TEMP SET TEMP=$GET(FLDS(IDX))
  . NEW FLDNUM SET FLDNUM=$PIECE(TEMP,"^",1) QUIT:FLDNUM=""
  . NEW DISPNAME SET DISPNAME=$PIECE(TEMP,"^",2) QUIT:DISPNAME=""
  . NEW SHOWEMPTY SET SHOWEMPTY=+$PIECE(TEMP,"^",3)
  . NEW PARSRTN SET PARSRTN=$PIECE(TEMP,"^",4)
  . NEW VALUE SET VALUE=$GET(ARR(FLDNUM))
  . IF VALUE="",SHOWEMPTY'=1 QUIT
  . IF PARSRTN'="" DO
  . . NEW CODE SET CODE="SET VALUE=$$"_PARSRTN_"(VALUE)"
  . . XECUTE CODE
  . WRITE "<TR><TD align=right><B>",DISPNAME,"</B></TD><TD>&nbsp;&nbsp;",VALUE,"</TD></TR>",!
  ;"Add insurances
  NEW INSIDX,INSIEN,INSCOUNT SET (INSCOUNT,INSIDX)=0
  FOR  SET INSIDX=$ORDER(^DPT(DFN,.312,INSIDX)) QUIT:INSIDX'>0  DO
  . SET INSIEN=$P($GET(^DPT(DFN,.312,INSIDX,0)),"^",1)
  . NEW INSNAME SET INSNAME=$PIECE($GET(^DIC(36,INSIEN,0)),"^",1)
  . SET INSCOUNT=INSCOUNT+1
  . NEW COB SET COB=+$P($G(^DPT(DFN,.312,INSIDX,0)),"^",20)
  . IF COB'>0 SET COB="#"
  . WRITE "<TR><TD align=right><B>INSURANCE ",COB,"<B></TD><TD>&nbsp;&nbsp;",INSNAME,"</TD></TR>",!
  IF INSCOUNT=0 DO
  . WRITE "<TR><TD align=right><B>INSURANCE<B></TD><TD>&nbsp;&nbsp;No insurances found</TD></TR>",!
  ;"Add next appointment
  NEW DATE,X,Y
  DO NOW^%DTC SET DATE=X
  NEW APPTCOUNT SET APPTCOUNT=0
  FOR  SET DATE=$ORDER(^TMG(22723,DFN,1,"B",DATE)) QUIT:DATE'>0  DO
  . NEW IDX SET IDX=$ORDER(^TMG(22723,DFN,1,"B",DATE,0))
  . IF $P($G(^TMG(22723,DFN,1,IDX,0)),"^",7)'="A" QUIT
  . NEW REASON SET REASON=$P($G(^TMG(22723,DFN,1,IDX,0)),"^",4)
  . SET Y=DATE
  . DO DD^%DT
  . WRITE "<TR><TD align=right><B>UPCOMING APPT<B></TD><TD>&nbsp;&nbsp;",Y,"&nbsp;&nbsp;",REASON,"</TD></TR>",!
  . SET APPTCOUNT=APPTCOUNT+1
  IF APPTCOUNT=0 WRITE "<TR><TD align=right><B>UPCOMING APPT<B></TD><TD>&nbsp;&nbsp;No upcoming appts found</TD></TR>",!
  WRITE "</TABLE>",!
  WRITE "<HR>",!
  IF $DATA(ARR("NAR")) DO  GOTO PTCH
  . WRITE "<H3>NOTES</H3>",!
  . SET IDX=0 FOR  SET IDX=$ORDER(ARR("NAR",IDX)) QUIT:IDX=""  DO  
  . . WRITE $GET(ARR("NAR",IDX)),!
PTCH ;  
  WRITE "</HTML>",!
  
  ;" WRITE "<TABLE BORDER=0><TR><TD>",!
  ;" WRITE "NAME</TD><TD>SSN</TD><TD>DOB</TD></TR></TABLE>",!
  ;" WRITE "<TABLE><TR><TD>",!
  ;" WRITE "ADDRESS</TD><TD>100 FAKE STREET<BR>GREENEVILLE</TD></TR><TR><TD>",!
  ;" WRITE "PHONE #</TD></TR><TR><TD>",!
  ;" WRITE "CELL #</TD></TR><TR><TD>",!
  ;" WRITE "OTHER</TD></TR>",!
  ;" WRITE "</TABLE></HTML>",!
  QUIT
  ;
NEXTAPPT(DFN)  ;"Return date of next appt
  NEW TMGRESULT SET TMGRESULT="NO UPCOMING APPT"
  
  QUIT TMGRESULT
  ;"
PARSPHON(VALUE) ;"PARSE PHONE NUMBER
  NEW RESULT SET RESULT=VALUE
  IF VALUE?7N DO
  . SET RESULT=$EXTRACT(VALUE,1,3)_"-"_$EXTRACT(VALUE,4,7) 
  IF VALUE?10N DO
  . SET RESULT="("_$EXTRACT(VALUE,1,3)_") "_$EXTRACT(VALUE,4,6)_"-"_$EXTRACT(VALUE,7,10) 
  QUIT RESULT
  ;
PARSSSN(VALUE) ;"PARSE SS NUMBER
  NEW RESULT SET RESULT=VALUE
  IF VALUE?9N DO
  . SET RESULT=$EXTRACT(VALUE,1,3)_"-"_$EXTRACT(VALUE,4,5)_"-"_$EXTRACT(VALUE,6,9) 
  QUIT RESULT
  ;
PARSYR(VALUE) ;"PARSE YR NUMBER
  NEW RESULT SET RESULT=VALUE
  IF VALUE'="" SET RESULT=RESULT_" yrs" 
  QUIT RESULT
  ;
GETINFO(DFN,ARR,FLDS) ;"Get needed patient info
  NEW TMGOUT,TMGMSG
  DO GETS^DIQ(2,DFN_",",FLDS,"","TMGOUT","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO
  . SET ARR("ERR")=$$GETERRST^TMGDEBU2(.TMGMSG)
  ELSE  DO
  . MERGE ARR=TMGOUT(2,DFN_",")
  . NEW IDX SET IDX=0
  . FOR  SET IDX=$ORDER(^DPT(DFN,"TMGNAR",IDX)) QUIT:IDX'>0  DO
  . . SET ARR("NAR",IDX)=$GET(^DPT(DFN,"TMGNAR",IDX,0))  
  QUIT
  ;  
  ;  
  D EN^DGRPD ; MAS Patient Inquiry
  K CPRSGUI
  ;  
  S DGCMOR="UNSPECIFIED",DGMPI=$G(^DPT(+DFN,"MPI"))
  S DGLOCATN=$$FIND1^DIC(4,"","MX","`"_+$P(DGMPI,U,3)),DGLOCATN=$S(+DGLOCATN>0:$P($$NS^XUAF4(DGLOCATN),U),1:"NOT LISTED")
  I $D(DGMPI),$D(DGLOCATN) S DGCMOR=$P(DGLOCATN,"^")
  ;END MPI/PD CHANGE
  K DGRPOUT,DGHOW S DGABBRV=$S($D(^DG(43,1,0)):+$P(^(0),"^",38),1:0),DGRPU="UNSPECIFIED" D DEM^VADPT,HDR^DGRPD1 F I=0,.11,.13,.121,.122,.31,.32,.36,.361,.141,.3 S DGRP(I)=$S($D(^DPT(DFN,I)):^(I),1:"")
  S DGAD=.11,(DGA1,DGA2)=1 D A^DGRPU S DGTMPAD=0 I $P(DGRP(.121),"^",9)="Y" S DGTMPAD=$S('$P(DGRP(.121),"^",8):1,$P(DGRP(.121),"^",8)'<DT:1,1:0) I DGTMPAD S DGAD=.121,DGA1=1,DGA2=2 D A^DGRPU
  W ?1,"Address: ",$S($D(DGA(1)):DGA(1),1:"NONE ON FILE"),?40,"Temporary: ",$S($D(DGA(2)):DGA(2),1:"NO TEMPORARY ADDRESS")
  S I=2 F I1=0:0 S I=$O(DGA(I)) Q:I=""  W:(I#2)!($X>50) !?9 W:'(I#2) ?48 W DGA(I)
  S DGCC=+$P(DGRP(.11),U,7),DGST=+$P(DGRP(.11),U,5),DGCC=$S($D(^DIC(5,DGST,1,DGCC,0)):$E($P(^(0),U,1),1,20)_$S($P(^(0),U,3)]"":" ("_$P(^(0),U,3)_")",1:""),1:DGRPU)
  N DGCNTRY,DGFORGN S DGCNTRY=$P(DGRP(.11),"^",10),DGFORGN=$$FORIEN^DGADDUTL(DGCNTRY) I 'DGFORGN W !?2,"County: ",DGCC
  S X="NOT APPLICABLE" I DGTMPAD S Y=$P(DGRP(.121),U,7) X:Y]"" ^DD("DD") S X=$S(Y]"":Y,1:DGRPU)_"-",Y=$P(DGRP(.121),U,8) X:Y]"" ^DD("DD") S X=X_$S(Y]"":Y,1:DGRPU)
  N DGSKIP S DGSKIP=$S(DGFORGN:"!,?42,""From/To: """,1:"?42,""From/To: """) ;WorldVistA Change ;04/03/2010
  W @DGSKIP,X,!?3,"Phone: ",$S($P(DGRP(.13),U,1)]"":$P(DGRP(.13),U,1),1:DGRPU),?44,"Phone: ",$S('DGTMPAD:X,$P(DGRP(.121),U,10)]"":$P(DGRP(.121),U,10),1:DGRPU) K DGTMPADW
  W !?2,"Office: ",$S($P(DGRP(.13),U,2)]"":$P(DGRP(.13),U,2),1:DGRPU)
  W !?4,"Cell: ",$S($P(DGRP(.13),U,4)]"":$P(DGRP(.13),U,4),1:DGRPU)
  W !?2,"E-mail: ",$S($P(DGRP(.13),U,3)]"":$P(DGRP(.13),U,3),1:DGRPU)
  W !,"Bad Addr: ",$$EXTERNAL^DILFD(2,.121,"",$$BADADR^DGUTL3(+DFN))
  S ORDOC=$$OUTPTPR^SDUTL3(DFN)
  S ORTEAM=$$OUTPTTM^SDUTL3(DFN)
  S ORMHP=$$START^SCMCMHTC(DFN) ;Retrieve Mental Health Provider
  S ORINP=$G(^DPT(DFN,.104))
  S ORATP=$G(^DPT(DFN,.1041))
  S ORASS=$P($$OUTPTAP^SDUTL3(DFN,DT),U,2)
  I ORDOC!ORTEAM!ORMHP!ORINP!ORATP  D
  . W !!,"Primary Care Information:"
  . I ORDOC W !,"Primary Practitioner: ",$P(ORDOC,"^",2)
  . I ORTEAM W !,"Primary Care Team:    ",$P(ORTEAM,"^",2)
  . I $$INPT^ORWPT1(DFN) D
  . . I ORATP W !,"Attending Physician:  ",$P($G(^VA(200,+ORATP,0)),U)
  . . I ORINP W !,"Inpatient Provider:   ",$P($G(^VA(200,+ORINP,0)),U)
  . I $L(ORASS) W !,"Associate Provider:   ",ORASS
  . I ORMHP D
  .. W !!,"MH Treatment Information:"
  .. W !,"MH Treatment Coord:   ",$E($P(ORMHP,"^",2),1,28) D
  ... W ?52,"Position: ",$E($P(ORMHP,"^",3),1,18)
  .. W !,"MH Treatment Team:    ",$E($P(ORMHP,"^",5),1,56)
  W !!,"Health Insurance Information:"
  D DISP^DGIBDSP  ;DBIA #4408
  W !!,"Service Connection/Rated Disabilities:"
  D DIS^DGRPDB
  F CONTACT="N","S" D
  .S VAOA("A")=$S(CONTACT="N":"",1:3)
  .D OAD^VADPT ;   Get NOK Information
  .I VAOA(9)]"" D
  .. W !!,$S(CONTACT="N":"Next of Kin Information:",1:"Secondary Next of Kin Information:")
  .. W !,"Name:  ",VAOA(9)                          ;     NOK Name
  .. I VAOA(10)]"" W " (",VAOA(10),")"              ;     Relationship
  .. I VAOA(1)]"" W !?7,VAOA(1)                     ;     Address Line 1
  .. I VAOA(2)]"" W !?7,VAOA(2)                     ;     Line 2
  .. I VAOA(3)]"" W !?7,VAOA(3)                     ;     Line 3
  .. I VAOA(4)]"" D
  .. . W !?7,VAOA(4)                                ;     City
  .. . I VAOA(5)]"" W ", "_$P(VAOA(5),"^",2)        ;     State
  .. . W "  ",$P(VAOA(11),"^",2)                    ;     Zip+4
  .. I VAOA(8)]"" W !!?7,"Phone number:  ",VAOA(8)  ;     Phone
  .. I CONTACT="N",$P($G(^DPT(DFN,.21)),U,11)]"" W !?7,"Work phone number:  ",$P(^DPT(DFN,.21),U,11)
  .. I CONTACT="S",$P($G(^DPT(DFN,.211)),U,11)]"" W !?7,"Work phone number:  ",$P(^DPT(DFN,.211),U,11)
  D KVAR^VADPT
  Q
  ;"
APPTRECS()  ;"
  ;"Purpose: To generate a report with records needed for today's visit
  NEW %ZIS
  SET %ZIS("A")="Enter Output Device: "
  SET IOP="S121-LAUGHLIN-LASER"
  DO ^%ZIS  ;"standard device call
  IF POP DO  GOTO ARDN
  . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output. Aborting.")
  use IO
  ;"
  NEW APPTARRAY,HEADER,LINES
  SET HEADER=0
  NEW SDT,EDT
  SET SDT=$$TODAY^TMGDATE+0.00001
  SET EDT=$$TODAY^TMGDATE+0.999999
  DO APPT4DT^TMGSMS05(SDT,EDT,.APPTARRAY,1)
  ;"
  NEW DATE,DFN,LINE SET DATE=0,LINE=1
  FOR  SET DATE=$ORDER(APPTARRAY("DT",DATE)) QUIT:DATE'>0  DO
  . SET DFN=0
  . FOR  SET DFN=$ORDER(APPTARRAY("DT",DATE,DFN)) QUIT:DFN'>0  DO
  . . NEW REASON,STATUS,DOB
  . . KILL LINES
  . . SET LINE=1
  . . SET STATUS=""
  . . SET REASON=$G(APPTARRAY(DATE,DFN,"REASON"))
  . . IF REASON="FU ER" SET STATUS="NEED ER RECORDS"
  . . IF REASON="FU HOSP" SET STATUS="NEED HOSPITAL RECORDS"
  . . IF REASON="NEW PAT" SET STATUS="NEED PREVIOUS PHYSICIAN RECORDS"
  . . NEW AGE K VADM SET AGE=+$$AGE^TIULO(DFN)
  . . IF AGE<18 DO
  . . . IF (REASON="PHYSICAL")!(REASON="1 YR CHECK")!(REASON="SPORTS PE")!(REASON="WELL CHILD")!(REASON="WELL CPE") SET STATUS="PRINT NEW VACCINE REPORT"
  . . IF STATUS'="" DO
  . . . SET LINES(LINE)=STATUS
  . . . SET LINE=LINE+1
  . . DO CONSULTS(DFN,.LINES,.LINE)
  . . IF '$D(LINES) QUIT
  . . IF HEADER=0 DO
  . . . WRITE !
  . . . WRITE "****************************************************************",!
  . . . WRITE "              MEDICAL RECORDS NEEDED FOR TODAY'S SCHEDULE",!
  . . . WRITE "                            " WRITE $$TODAY^TMGDATE(1),!
  . . . WRITE "               Please deliver this report to MEDICAL RECORDS",!
  . . . WRITE "****************************************************************",!
  . . . WRITE "                                            (From TMGRPT1.m)",!!
  . . . SET HEADER=1
  . . SET DOB=$$EXTDATE^TMGDATE($P($G(^DPT(DFN,0)),"^",3))
  . . WRITE "[ ] ",$G(APPTARRAY(DATE,DFN,"NAME")),?28,"(",DOB,")",?45,$$EXTDATE^TMGDATE(DATE),!
  . . SET LINE=0
  . . FOR  SET LINE=$ORDER(LINES(LINE)) QUIT:LINE'>0  DO
  . . . WRITE "        -> ",$G(LINES(LINE)),!
  . . WRITE !
ARDN
  DO ^%ZISC  ;" Close the output device
  QUIT
  ;"
CONSULTS(DFN,ARRAY,X)  ;"
  ;"Purpose: write a string of text for each overdue consult for a patient
  NEW IDX SET IDX=0
  NEW COMPIEN SET COMPIEN=+$ORDER(^ORD(100.01,"B","COMPLETE",""))
  NEW DCIEN SET DCIEN=+$ORDER(^ORD(100.01,"B","DISCONTINUED",""))
  NEW CANCELIEN SET CANCELIEN=+$ORDER(^ORD(100.01,"B","CANCELLED",""))
  ;"
  FOR  SET IDX=$ORDER(^GMR(123,"F",DFN,IDX)) QUIT:IDX'>0  DO
  . NEW ZNODE SET ZNODE=$GET(^GMR(123,IDX,0))
  . NEW STATUS SET STATUS=$PIECE(ZNODE,"^",12)
  . IF (STATUS=COMPIEN)!(STATUS=DCIEN)!(STATUS=CANCELIEN) QUIT
  . NEW ORDERTYPE SET ORDERTYPE=$PIECE($GET(^GMR(123.5,$P(ZNODE,"^",5),0)),"^",1)
  . NEW DUEDATE SET DUEDATE=$$GETDUE(IDX)
  . IF $$TODAY^TMGDATE>DUEDATE DO
  . . SET Y=DUEDATE D DD^%DT
  . . SET ARRAY(X)=ORDERTYPE_" CONSULT WAS SCHEDULED FOR "_Y
  . . SET X=X+1
  QUIT
  ;"
GETDUE(idx) ;"
  ;"Purpose: Get the due date for a consult
  NEW idxWP SET idxWP=0
  NEW found SET found=0
  NEW Y SET Y=0
  FOR  SET idxWP=+$ORDER(^GMR(123,idx,20,idxWP)) QUIT:(idxWP'>0)!found  do
  . NEW line SET line=$GET(^GMR(123,idx,20,idxWP,0)) QUIT:line=""
  . IF line'["An appointment has been scheduled" QUIT
  . SET found=1
  . SET line=$GET(^GMR(123,idx,20,idxWP+1,0))
  . NEW apptDate
  . if line["*" do
  . . SET apptDate=$PIECE(line,"*",2)
  . else  do
  . . set apptDate=$$TRIM^XLFSTR(line)
  . IF apptDate[" at " do
  . . SET apptDate=$p(apptDate," at ",1)_"@"_$p(apptDate," at ",2)
  . SET apptDate=$$UP^XLFSTR(apptDate)
  . IF apptDate["DAY" SET apptDate=$P(apptDate,"DAY ",2)
  . SET Y=$$FMDate^TMGFMUT(apptDate)
  ;". IF Y>0 do
  ;". . DO DD^%DT  ;"standardize date
  ;". ELSE  do
  ;". . SET Y=apptDate
  IF found=0 SET Y=9999999  ;"DATE NOT FOUND, DON'T INCLUDE
  QUIT Y
  ;"
HL7QUERY(ROOT,ORDFN,ID,ORALPHA,OROMEGA,ORDTRNG,REMOTE,ORMAX,ORFHIE) ; --Query to Health Summary Reports
 ;"NOTES: I am perpetually confused by the Report definitions and cannot seem to find 
 ;"       a Reports manual. I copied the definition for the Current Orders report and 
 ;"       repurposed some of the fields. The format for the output is:
 ;"S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="1^Family Phys of Greeneville;777"  <- Leave this in item one
 ;"S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="2^>>"  <- all of item 2 will be the HL7 message text
 ;"S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="2^HL7 MESSAGE LINE 1"
 ;"S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="2^."
 ;"S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="3^ADMISSION"  <-  Type of message
 ;"S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="4^04/06/2020 09:05"  <- Date of message
 ;"S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="5^Greeneville Community Hospital East"  <- Sending Facility
 NEW OUT,ORDBEG,ORDEND,OREXT
 NEW ARRTYPES
 DO LOADTYPS^TMGHL76A(.ARRTYPES)
 NEW IDX SET IDX=0
 NEW THISDT SET THISDT=ORALPHA
 FOR  SET THISDT=$O(^TMG(22720.5,ORDFN,1,"B",THISDT)) QUIT:(THISDT="")!(THISDT>OROMEGA)  DO
 . NEW IEN22720 SET IEN22720=0
 . FOR  SET IEN22720=$O(^TMG(22720.5,ORDFN,1,"B",THISDT,IEN22720)) QUIT:IEN22720'>0  DO
 . . NEW DTADDED,IEN772,IEN773,FOLDER,FILE,ZN,HL7MSGARR,HOSPLOC,VSTTYPE,ADTTYPE,MSH,PATHFNAME
 . . S ZN=$G(^TMG(22720.5,ORDFN,1,IEN22720,0))
 . . SET DTADDED=$P(ZN,"^",6),FOLDER=$P(ZN,"^",2),FILE=$P(ZN,"^",3)
 . . SET IEN772=+$P(ZN,"^",4),IEN773=+$P(ZN,"^",5)
 . . IF FOLDER'["BalladADTHL7" QUIT  ;"ONLY ADT MESSAGES
 . . ;"IF IEN773'>0 QUIT
 . . ;"GET MESSAGE DETAILS
 . . SET PATHFNAME=FOLDER_FILE
 . . DO GETHL7MSG(PATHFNAME,.HL7MSGARR,.HOSPLOC,.VSTTYPE,.MSH)
 . . SET ADTTYPE=$P(MSH,"|",9)
 . . IF ADTTYPE'["ADT" QUIT
 . . SET ADTTYPE=$P(ADTTYPE,"^",2)
 . . IF (ADTTYPE'="A01")&(ADTTYPE'="A03") QUIT
 . . ;"GET ACTUAL MESSAGE
 . . 
 . . S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="1^Family Phys of Greeneville;777"
 . . S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="2^File Path: "_FOLDER
 . . S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="2^File Name: "_FILE
 . . S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="2^=========================================================================================="
 . . S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="2^"
 . . NEW HL7IDX SET HL7IDX=0
 . . FOR  SET HL7IDX=$O(HL7MSGARR(HL7IDX)) QUIT:HL7IDX'>0  DO
 . . . S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="2^"_$G(HL7MSGARR(HL7IDX))
 . . S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="2^=========================================================================================="
 . . S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="3^"_ADTTYPE_"-"_$G(ARRTYPES(ADTTYPE))
 . . S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="4^"_$$EXTDATE^TMGDATE(THISDT)
 . . S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="5^"_$P(HOSPLOC,"^",1)
 . . S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="6^"_VSTTYPE
 . . S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="7^"_$$EXTDATE^TMGDATE(DTADDED)
 S ROOT=$NA(^TMP("HL7DATA",$J))
 QUIT
 ;S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="6^[+]"
 ;"S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="7^339273;1"
 S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="1^Family Phys of Greeneville;777"
 S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="2^>>"
 S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="2^NON-FASTING LABS. LABS ASAP. "
 S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="2^."
 S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="2^ TESTS ORDERED:  CBC-Platelet With Diff.. "
 S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="2^."
 S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="2^ DIAG: Low B12 - E53.8."
 S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="3^DISCHARGE"
 S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="4^03/03/2020 17:49"
 S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="5^Greeneville Community Hospital East"
 ;S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="6^[+]"
 ;"S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="7^338867;1"
 S ROOT=$NA(^TMP("HL7DATA",$J))
 Q
  ;"
GETHL7MSG(PATHFNAME,HL7MSGARR,HOSPLOC,VSTTYPE,MSH)  ;"FETCH THE HL7 MSG AND STORE IN HL7MSGARR
 NEW IDX,OUTIDX,RESULT SET IDX=0,OUTIDX=1,MSH=""
 SET RESULT=$$LOADHL7^TMGHL7U2(PATHFNAME,.HL7MSGARR,.MSH)
 ;"FOR  SET IDX=$O(^HL(772,IEN772,"IN",IDX)) QUIT:IDX'>0  DO
 FOR  SET IDX=$O(HL7MSGARR(IDX)) QUIT:IDX'>0  DO
 . ;"NEW LINE SET LINE=$G(^HL(772,IEN772,"IN",IDX,0))
 . NEW LINE SET LINE=$G(HL7MSGARR(IDX))
 . NEW DONE SET DONE=0
 . IF LINE["MSH" DO
 . . SET MSH=LINE
 . IF LINE["PD1" DO
 . . SET HOSPLOC=$P(LINE,"|",4)
 . IF LINE["PV1" DO
 . . SET VSTTYPE=$P(LINE,"|",3) 
 . SET HL7MSGARR(OUTIDX)=LINE
 . SET OUTIDX=OUTIDX+1
 . ;"FOR  QUIT:DONE=1  DO
 . ;". IF $L(LINE)>120 DO
 . ;". . SET HL7MSGARR(OUTIDX)=$E(LINE,0,120)
 . ;". . SET LINE="       ->"_$E(LINE,121,$L(LINE))
 . ;". ELSE  DO
 . ;". . SET HL7MSGARR(OUTIDX)=LINE
 . ;". . SET DONE=1
 . ;". SET OUTIDX=OUTIDX+1
 Q
 ;"
HL7EXT   ; I don't know the function of this. 
        N ORVP
        S ORVP=DFN_";DPT("
        I '$D(^OR(100,"AC",ORVP)) Q
        D EN^ORQ1(ORVP,,2,,ORDBEG,ORDEND,1) ; current orders. ORLIST is set in ORQ1
        Q
  ;"
HL7(ROOT,ORALPHA,OROMEGA,ORMAX,ORDBEG,ORDEND,OREXT)     ; I don't know the function of this. 
 NEW IDX SET IDX=0
 S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="1^Family Phys of Greeneville;777"
 S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="2^>>"
 S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="2^NON-FASTING LABS. LABS TODAY. "
 S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="2^."
 S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="2^ TESTS ORDERED:  B12. "
 S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="2^."
 S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="2^ DIAG: DM-2 - E11.9."
 S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="3^ADMISSION"
 S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="4^04/06/2020 09:05"
 S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="5^Greeneville Community Hospital East"
 ;S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="6^[+]"
 S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="7^339273;1"
 S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="1^Family Phys of Greeneville;777"
 S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="2^>>"
 S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="2^NON-FASTING LABS. LABS ASAP. "
 S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="2^."
 S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="2^ TESTS ORDERED:  CBC-Platelet With Diff.. "
 S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="2^."
 S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="2^ DIAG: Low B12 - E53.8."
 S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="3^DISCHARGE"
 S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="4^03/03/2020 17:49"
 S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="5^Greeneville Community Hospital East"
 ;S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="6^[+]"
 S IDX=IDX+1,^TMP("HL7DATA",$J,IDX)="7^338867;1"
 S ROOT=$NA(^TMP("HL7DATA",$J))
 Q
  ;"
LABPULL  ;"
  ;"Get schedule for today and print all entries who have had lab results
  NEW RPTDATE SET RPTDATE=$$TODAY^TMGDATE
  ;"NEW RPTDATE SET RPTDATE=3191224
  NEW ACTORDERS
  NEW WHEAD SET WHEAD=0
  NEW DT SET DT=RPTDATE
  FOR  SET DT=$O(^TMG(22723,"DT",DT)) QUIT:(DT="")!(DT'[RPTDATE)  DO
  . NEW DFN SET DFN=0
  . FOR  SET DFN=$O(^TMG(22723,"DT",DT,DFN)) QUIT:DFN'>0  DO
  . . NEW IEN SET IEN=+$O(^TMG(22723,"DT",DT,DFN,0))
  . . NEW ZN SET ZN=$G(^TMG(22723,DFN,1,IEN,0))
  . . NEW STATUS SET STATUS=$P(ZN,"^",7)
  . . ;"WRITE ZN,!
  . . IF STATUS="C" QUIT
  . . NEW REASON SET REASON=$P(ZN,"^",4)
  . . IF REASON="PROTIME" QUIT
  . . NEW LABDATES DO GETLABDT(DFN,.LABDATES)
  . . IF '$D(LABDATES) QUIT
  . . IF WHEAD=0 DO
  . . . DO HEADING(RPTDATE)
  . . . SET WHEAD=1
  . . NEW NAME SET NAME=$P($G(^DPT(DFN,0)),"^",1)
  . . NEW TIME SET TIME=$P(DT,".",2)
  . . NEW HOUR,MINS
  . . SET HOUR=$E(TIME,1,2),MINS=$E(TIME,3,4)
  . . IF $L(HOUR)=1 SET HOUR=HOUR_"0"
  . . IF $L(MINS)=0 SET MINS="00"
  . . IF $L(MINS)=1 SET MINS=MINS_"0"
  . . SET TIME=HOUR_":"_MINS
  . . WRITE NAME_" has appt at "_TIME_". Lab dates:",!
  . . NEW FMDATE SET FMDATE=0
  . . FOR  SET FMDATE=$O(LABDATES(FMDATE)) QUIT:FMDATE'>0  DO
  . . . WRITE "  [ ] ",$$EXTDATE^TMGDATE(FMDATE),!
  . . WRITE !
  . . DO GETAORD(DFN,.ACTORDERS)
  ;"
  ;"THE NEXT PORTION WILL CHECK ALL PATIENTS TO SEE IF THEY HAVE
  ;"        ACTIVE LAB ORDERS
  IF $D(ACTORDERS) DO
  . WRITE !,"====================================================================================",!
  . WRITE "======= THE BELOW DATA IS FOR EDDIE",!
  . NEW NAME SET NAME=""
  . FOR  SET NAME=$O(ACTORDERS(NAME)) QUIT:NAME=""  DO
  . . NEW DATE SET DATE=0
  . . WRITE !,!,NAME," HAS ACTIVE LAB ORDERS FOR:",!
  . . FOR  SET DATE=$O(ACTORDERS(NAME,DATE)) QUIT:DATE'>0  DO
  . . . WRITE $$EXTDATE^TMGDATE(DATE,1)," "
  QUIT
  ;"
HEADING(RPTDATE)  ;"
  WRITE "====================================================================================",!
  WRITE "======= LAB RESULT PULL REPORT FOR ",$$EXTDATE^TMGDATE(RPTDATE,1),!
  WRITE "=======",!
  WRITE "======= Instructions: For each patient, check to see if the results",!
  WRITE "=======               are in the patient's chart. ",!
  WRITE "=======",!
  WRITE "=======               If they are not try and find them at nurse's station or in",!
  WRITE "=======               the office. ",!
  WRITE "=======",!
  WRITE "=======               If they are found and have been signed by the physician",!
  WRITE "=======               file them in the chart as normal",!
  WRITE "=======                  ",!
  WRITE "=======               If they are not signed off on, place in the",!
  WRITE "=======               chart, sideways and sticking out. ",!
  WRITE "=======",!
  WRITE "=======               If they cannot be located, give missing to office manager.",!
  WRITE "=======                                                (report in TMGRPT3)",!
  WRITE "===================================================================================",!,!,!
  QUIT
  ;"
GETAORD(DFN,RESULTARR)
  NEW DFNSTR SET DFNSTR=DFN_";DPT("
  NEW RDATE SET RDATE=0
  NEW ACTIVESTATUS SET ACTIVESTATUS=6
  NEW RCUTOFF SET RCUTOFF=9999999-$$ADDDAYS^TMGDATE("-365")
  FOR  SET RDATE=$O(^OR(100,"AR",DFNSTR,RDATE)) QUIT:(RDATE'>0)!(RDATE>RCUTOFF)  DO
  . NEW ORDIEN SET ORDIEN=0
  . FOR  SET ORDIEN=$O(^OR(100,"AR",DFNSTR,RDATE,ORDIEN)) QUIT:ORDIEN'>0  DO
  . . NEW DATE,STATUS,TEXT
  . . SET DATE=$P($G(^OR(100,ORDIEN,0)),"^",8)
  . . SET STATUS=$P($G(^OR(100,ORDIEN,3)),"^",3)
  . . IF STATUS'=ACTIVESTATUS QUIT
  . . NEW ORDLINE SET ORDLINE=0
  . . SET TEXT=""
  . . FOR  SET ORDLINE=$O(^OR(100,ORDIEN,8,1,.1,ORDLINE)) QUIT:ORDLINE'>0  DO
  . . . SET TEXT=TEXT_$G(^OR(100,ORDIEN,8,1,.1,ORDLINE,0))
  . . SET TEXT=$$UP^XLFSTR(TEXT)
  . . ;"WRITE TEXT,!
  . . IF (TEXT'["LABS")&(TEXT'["TESTS ORDERED:") QUIT
  . . SET RESULTARR($P($G(^DPT(DFN,0)),"^",1),DATE)=""
  QUIT
  ;"
GETLABDT(DFN,RESULTARR)
  NEW LRDFN SET LRDFN=+$P($G(^DPT(DFN,"LR")),"^",1)
  IF LRDFN'>0 GOTO GLDDN
  NEW RTODAY SET RTODAY=$$LABDTNOW()
  NEW CUTOFFDT SET CUTOFFDT=$$LCUTDT(30)
  NEW THISDATE SET THISDATE=CUTOFFDT
  FOR  SET THISDATE=$O(^LR(LRDFN,"CH",THISDATE),-1) QUIT:(THISDATE="")  DO
  . NEW DAY SET DAY=$P(THISDATE,".",1)
  . IF DAY'>0 QUIT
  . SET RESULTARR($$LDTTOFM(DAY))=""
GLDDN
  QUIT
  ;"
LABDTNOW()
  QUIT 9999999-$$TODAY^TMGDATE
  ;"
LCUTDT(NUMDAYS)
  NEW DATE SET DATE=$$ADDDAYS^TMGDATE(-NUMDAYS) 
  QUIT 9999999-DATE 
  ;"
LDTTOFM(LDATE)
  QUIT 9999999-LDATE 
  ;"
TIMESTR(DFN)
   NEW TMGRESULT SET TMGRESULT=""
   NEW INSIDX,INSIEN SET INSIDX=0
   FOR  SET INSIDX=$O(^DPT(DFN,.312,INSIDX)) QUIT:INSIDX'>0  DO
   . SET INSIEN=$P($G(^DPT(DFN,.312,INSIDX,0)),"^",1)
   . ;"IF INSIEN=3 SET TMGRESULT="MEDICARE. "
   . ;"IF INSIEN=36 SET TMGRESULT="AARP. "
   . ;"IF INSIEN=13 SET TMGRESULT="AARP. "
   . IF INSIEN'=12 DO
   . . SET TMGRESULT=$P($G(^DIC(36,INSIEN,0)),"^",1)
   IF TMGRESULT'="" SET TMGRESULT="PATIENT HAS "_TMGRESULT_". "
   QUIT TMGRESULT
  ;"
BILLTIME(DFN)  ;"
   NEW TMGRESULT SET TMGRESULT=$$TIMESTR(DFN)
   IF TMGRESULT'="" DO
   . SET TMGRESULT="<B><FONT style=""BACKGROUND-COLOR:#ff0000"">[^^!"_TMGRESULT_"IF THIS IS A TELEPHONE VISIT BILL FOR TIME.!^^]</B></FONT>"
   QUIT TMGRESULT
   ;"
TIMEWARN(DFN)  ;"
   NEW TMGRESULT SET TMGRESULT=$$TIMESTR(DFN)
   IF TMGRESULT'="" DO
   . SET TMGRESULT="<B><FONT style=""BACKGROUND-COLOR:#ff0000"">[^^!"_TMGRESULT_"IF THIS IS A TELEPHONE VISIT TURN ON TIMER.!^^]</B></FONT>"
   QUIT TMGRESULT
   ;"
PATSTR(TMGRESULT,DFN)  ;"RETURN THE PATIENT INFO STRING TO BE INSERTED INTO THE MESSENGER
   SET DFN=+$G(DFN)
   IF DFN'>0 DO  GOTO PSDN
   . SET TMGRESULT=""
   NEW NAME,DOB,PHONE,SEQNUM SET (NAME,DOB,PHONE,SEQNUM)=""
   SET NAME=$P($G(^DPT(DFN,0)),"^",1)
   SET DOB=$P($G(^DPT(DFN,0)),"^",3)
   SET DOB=$$EXTDATE^TMGDATE(DOB,1)
   SET PHONE=$P($G(^DPT(DFN,.13)),"^",1)
   IF PHONE="" SET PHONE=$P($G(^DPT(DFN,.13)),"^",4)
   SET PHONE=$$FMTPHONE(PHONE)
   SET SEQNUM=$P($G(^DPT(DFN,"TMG")),"^",2)
   SET TMGRESULT=NAME_" ("_DOB_" , "_PHONE_" , "_SEQNUM_")"
PSDN
   QUIT TMGRESULT
   ;"
FMTPHONE(PHONE)
   NEW TMGRESULT SET TMGRESULT=PHONE
   IF $L(PHONE)=10 DO
   . SET TMGRESULT="("_$E(PHONE,1,3)_")"_$E(PHONE,4,6)_"-"_$E(PHONE,7,10)
   ELSE  IF $L(PHONE)=11 DO
   . SET TMGRESULT="("_$E(PHONE,2,4)_")"_$E(PHONE,5,7)_"-"_$E(PHONE,8,11)   	   
   QUIT TMGRESULT
   