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
  . SET INSIEN=$GET(^DPT(DFN,.312,INSIDX,0))
  . NEW INSNAME SET INSNAME=$PIECE($GET(^DIC(36,INSIEN,0)),"^",1)
  . SET INSCOUNT=INSCOUNT+1
  . WRITE "<TR><TD align=right><B>INSURANCE ",INSCOUNT,"<B></TD><TD>&nbsp;&nbsp;",INSNAME,"</TD></TR>",!
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
  NEW DT,DFN,LINE SET DT=0,LINE=1
  FOR  SET DT=$ORDER(APPTARRAY("DT",DT)) QUIT:DT'>0  DO
  . SET DFN=0
  . FOR  SET DFN=$ORDER(APPTARRAY("DT",DT,DFN)) QUIT:DFN'>0  DO
  . . NEW REASON,STATUS,DOB
  . . KILL LINES
  . . SET LINE=1
  . . SET STATUS=""
  . . SET REASON=$G(APPTARRAY(DT,DFN,"REASON"))
  . . IF REASON="FU ER" SET STATUS="NEED ER RECORDS"
  . . IF REASON="FU HOSP" SET STATUS="NEED HOSPITAL RECORDS"
  . . IF REASON="NEW PAT" SET STATUS="NEED PREVIOUS PHYSICIAN RECORDS"
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
  . . WRITE "[ ] ",$G(APPTARRAY(DT,DFN,"NAME")),?28,"(",DOB,")",?45,$$EXTDATE^TMGDATE(DT),!
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
  . SET Y=$$FMDate^TMGFMUT(apptDate)
  ;". IF Y>0 do
  ;". . DO DD^%DT  ;"standardize date
  ;". ELSE  do
  ;". . SET Y=apptDate
  QUIT Y
  ;"
GET
  