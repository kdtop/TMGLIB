TMGGRC2C	      ;TMG/kst-Work with Growth Chart Data ;10/5/10 ; 9/27/11 9:41am
	       ;;1.0;TMG-LIB;**1,17**;10/5/10;Build 38
	      ;
	      ;"Code for working with pediatric growth chart data.
	      ;"This helps generate javascript code to pass back to WebBrowser
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
	      ;"PRIVATE API FUNCTIONS
	      ;"=======================================================================
	      ;"PCTILE(GRAPHTYP,AGEYR,GENDER,VALUE,V2) -- Return percentile of value for graph
	      ;"CVTMETRC(VITIEN,DATA) -- convert English measurements to metric
	      ;"GETDATES(DOB,MONMIN,MAXMON,STARTDT,ENDDT) --Convert DOB + age range into absolute starting and ending date
	      ;"ADDRPT -- Entry point not taking result variable, for KIDS
	      ;"ADDRPT0(RESULT)  -- install (add) the TMG GROWTH CHART MENU to ORWRP REPORT LIST.
	      ;"=======================================================================
	      ;
PCTILE(GRAPHTYP,AGEYR,GENDER,VALUE,V2)	;
	      ;"Purpose: Return percentile of value for graph
	      NEW RESULT SET RESULT=""
	      IF GRAPHTYP["-LN" SET RESULT=$$LENPCTL^TMGGRC1(AGEYR,GENDER,VALUE) GOTO PCTDN
	      IF GRAPHTYP["-WT4" SET RESULT=$$WTLENPCT^TMGGRC1(AGEYR,GENDER,VALUE,V2) GOTO PCTDN
	      IF GRAPHTYP["-WT" SET RESULT=$$WTPCTL^TMGGRC1(AGEYR,GENDER,VALUE) GOTO PCTDN
	      IF GRAPHTYP["-HC" SET RESULT=$$HCPCTL^TMGGRC1(AGEYR,GENDER,VALUE) GOTO PCTDN
	      IF GRAPHTYP["-BMI" SET RESULT=$$BMIPCTL^TMGGRC1(AGEYR,GENDER,VALUE) GOTO PCTDN
	      IF GRAPHTYP["-HT" SET RESULT=$$LENPCTL^TMGGRC1(AGEYR,GENDER,VALUE) GOTO PCTDN
	      IF GRAPHTYP["WHO-BA" SET RESULT=$$WHBAPCTL^TMGGRC1(AGEYR,GENDER,VALUE) GOTO PCTDN
	      IF GRAPHTYP["WHO-HA" SET RESULT=$$WHHAPCTL^TMGGRC1(AGEYR,GENDER,VALUE) GOTO PCTDN
	      IF GRAPHTYP["WHO-WA" SET RESULT=$$WHWAPCTL^TMGGRC1(AGEYR,GENDER,VALUE) GOTO PCTDN
	      IF GRAPHTYP["WHO-HC" SET RESULT=$$WHHCPCTL^TMGGRC1(AGEYR,GENDER,VALUE) GOTO PCTDN
	      IF GRAPHTYP["WHO-WL" SET RESULT=$$WHWLPCTL^TMGGRC1(AGEYR,GENDER,VALUE,V2) GOTO PCTDN
	      IF GRAPHTYP["WHO-WH" SET RESULT=$$WHWSPCTL^TMGGRC1(AGEYR,GENDER,VALUE,V2) GOTO PCTDN
PCTDN	 QUIT RESULT
	      ;
ZSCORE(GRAPHTYP,AGEYR,GENDER,VALUE,V2)	
	             ;"Purpose: Return z-score of value for graph
	             NEW TMGRESULT SET TMGRESULT=""
	             IF GRAPHTYP["ZWO-BA" SET TMGRESULT=$$WHBAPCTL^TMGGRC1(AGEYR,GENDER,VALUE,0,1) GOTO ZCDN
	             IF GRAPHTYP["ZWO-HA" SET TMGRESULT=$$WHHAPCTL^TMGGRC1(AGEYR,GENDER,VALUE,0,1) GOTO ZCDN
	             IF GRAPHTYP["ZWO-WA" SET TMGRESULT=$$WHWAPCTL^TMGGRC1(AGEYR,GENDER,VALUE,0,1) GOTO ZCDN
	             IF GRAPHTYP["ZWO-WL" SET TMGRESULT=$$WHWLPCTL^TMGGRC1(AGEYR,GENDER,VALUE,V2,0,1) GOTO ZCDN
	             IF GRAPHTYP["ZWO-WH" SET TMGRESULT=$$WHWSPCTL^TMGGRC1(AGEYR,GENDER,VALUE,V2,0,1) GOTO ZCDN
	             IF GRAPHTYP["ZWO-HC" SET TMGRESULT=$$WHHCPCTL^TMGGRC1(AGEYR,GENDER,VALUE,0,1) GOTO ZCDN
ZCDN	         QUIT TMGRESULT              
	      ;
CVTMETRC(VITIEN,DATA)	;"CONVERT METRIC
	      ;"Purpose: To convert us measurements to metric
	      ;"Input: VITIEN -- The IEN in 120.51 specifying which vital type to return (e.g. HEIGHT, vs WEIGHT etc.
	      ;"          There is no BMI vital type. If vital type is BMI then VITIEN will equal BMI.
	      ;"       DATA -- In US measurement.
	      ;"        If VITIEN is BMI, then DATA is expected to equal WEIGHT(lbs)^HEIGHT(in)
	      ;"Result: Data converted into metric measurement units
	      ;"Output: None
	      NEW VITTYPE,RESULT
	      NEW HEIGHT,WEIGHT
	      SET RESULT=DATA
	      IF DATA=0  GOTO CMETQT
	      IF +VITIEN=VITIEN DO
	      . SET VITTYPE=$PIECE($GET(^GMRD(120.51,VITIEN,0)),"^",2)
	      ELSE  DO
	      . SET WEIGHT=$PIECE(DATA,"^",1)
	      . SET HEIGHT=$PIECE(DATA,"^",2)
	      . SET VITTYPE=VITIEN
	      ;
	      IF VITTYPE="HT" DO
	      . SET RESULT=DATA*2.54   ;"in to cm
	      . SET RESULT=$JUSTIFY(RESULT,0,0)  ;"round to nearest integer
	      ELSE  IF VITTYPE="WT" DO
	      . SET RESULT=DATA*0.45359237   ;"lbs to kg
	       ; . SET RESULT=$JUSTIFY(RESULT,0,1)  ;"round to nearest tenth - smh
	      ELSE  IF VITTYPE="CG" DO
	      . SET RESULT=DATA*2.54   ;"in to cm
	      . SET RESULT=$JUSTIFY(RESULT,0,1)  ;"round to nearest tenth
	      ELSE  IF VITTYPE="BMI" DO
	      . ;"NOTE:  BMI= weight(kg) / height(m) sq.
	      . IF HEIGHT'>0 SET RESULT=0 QUIT
	      . SET RESULT=(WEIGHT*0.4545)/((HEIGHT*0.0254)**2)
	      . SET RESULT=$JUSTIFY(RESULT,0,1)  ;"round to nearest tenth
	      ELSE  IF VITTYPE="W4LS" DO
	      . SET RESULT=+$JUSTIFY(HEIGHT*2.54,0,1)_"^"_+$JUSTIFY(WEIGHT*0.4545,0,1)
CMETQT	QUIT RESULT
	      ;
	      ;
GETDATES(DOB,MONMIN,MAXMON,STARTDT,ENDDT)	       ;
	      ;"Purpose: Convert DOB + age range into absolute starting and ending date
	      ;"Input: DOB -- The patient's date of birth, in FMDate format
	      ;"       MONMIN -- the patient's age (months) at the beginning of desired date range
	      ;"       MONMAX -- the patient's age (months) at the end of desired date range
	      ;"       STARTDT -- PASS BY REFERENCE, AN OUT PARAMETERS
	      ;"       ENDDT -- PASS BY REFERENCE, AN OUT PARAMETERS
	      ;"Result: none
	      ;"Output: STARTDT AND ENDDT are filled with beginning and ending dates, in FMDate format
	      ;"
	      NEW X1,X2,X
	      ;"Calc Start Date
	      SET X1=DOB
	      SET X2=MONMIN*30  ;"Convert to num of days
	      D C^%DTC
	      SET STARTDT=X
	      ;Calc End Date
	      SET X1=DOB
	      SET X2=MAXMON*30  ;"Convert to num of days
	      D C^%DTC
	      SET ENDDT=X
	      QUIT
	      ;
	      ;
	      ;"===================================================================
	      ;"===================================================================
ADDRPT	        ;
	      ;"Purpose: Entry point not taking result variable, for KIDS
	      NEW RESULT
	      DO ADDRPT0(.RESULT)
	      QUIT
	      ;
	      ;
ADDRPT0(RESULT)	          ;" //elh
	      ;"Purpose: To install (add) the TMG GROWTH CHART MENU to ORWRP REPORT LIST.
	      ;"         This will make the reports show up in CPRS
	      ;"         The heirarchy is: System(SYS),User,Package(PKG),Division(DIV). To ensure that the
	      ;"         Menu is displayed, the entries will be tested top down until the highest
	      ;"         heirarchy is found, excluding User. The entry will be placed there.
	      ;"Input: None
	      ;"Output: None
	      ;"Result: 1^Successful or -1^Error Message
	      NEW PKG SET PKG="DIC(9.4,"
	      NEW SYS SET SYS="DIC(4.2,"
	      NEW DIV SET DIV="DIC(4,"
	      NEW DONE SET DONE=0
	      NEW RPTIEN SET RPTIEN=+$ORDER(^XTV(8989.51,"B","ORWRP REPORT LIST",0))
	      NEW FOUND SET FOUND=""
	      NEW HIERARCHY
	      ;"
	      ;"GET THE HIERARCHY STRUCTURE
	      NEW I SET I=0
	      FOR  SET I=$ORDER(^XTV(8989.51,RPTIEN,30,I)) QUIT:+I'>0  DO
	      . SET HIERARCHY(0)=$GET(^XTV(8989.51,RPTIEN,30,I,0))
	      . SET HIERARCHY($PIECE(HIERARCHY(0),"^",1))=$PIECE(HIERARCHY(0),"^",2)
	      ;"
	      ;"SORT THROUGH THE HIERARCHY AND PLACE THE TOP TWO IN HIERARCHY(1) AND HIERARCHY(2) RESPECTIVELY
	      SET I=0
	      NEW COUNTER SET COUNTER=1
	      NEW FILE
	      FOR  SET I=$ORDER(HIERARCHY(I)) QUIT:+I'>0  DO
	      . SET FILE=HIERARCHY(I)
	      . IF FILE'=200 DO    ;"EXCLUDE THE USER REPOET MENU
	      . . IF FILE[4.2 DO  ;"SYSTEM
	      . . . SET HIERARCHY(COUNTER)=SYS_"^SYS"
	      . . . SET COUNTER=COUNTER+1
	      . . ELSE  IF FILE[9.4 DO  ;"PACKAGE
	      . . . SET HIERARCHY(COUNTER)=PKG_"^PKG"
	      . . . SET COUNTER=COUNTER+1
	      . . ELSE  IF FILE[4 DO  ;"DIVISION
	      . . . SET HIERARCHY(COUNTER)=DIV_"^DIV"
	      . . . SET COUNTER=COUNTER+1
	      ;"
	      ;"TEST FOR HIGHEST USED HIERARCHY
	      NEW DATA SET DATA=0
	      NEW DATASTRING
	      FOR  SET DATA=$ORDER(^XTV(8989.5,"AC",RPTIEN,DATA)) QUIT:(+DATA'>0)!DONE  DO
	      . IF DATA[$PIECE(HIERARCHY(1),"^",1) DO
	      . . SET FOUND=$PIECE(HIERARCHY(1),"^",2)
	      . . SET DATASTRING=DATA
	      . . SET DONE=1
	      . ELSE  IF DATA[$PIECE(HIERARCHY(2),"^",1) DO
	      . . SET FOUND=$PIECE(HIERARCHY(2),"^",2)
	      . . SET DATASTRING=DATA
	      . ELSE  DO
	      . . SET DATASTRING=DATA
	      IF FOUND="" SET FOUND=$PIECE(HIERARCHY(3),"^",2)
	      ;"
	      ;"FIND THE FIRST AVAILABLE SEQUENCE
	      SET COUNTER=0
	      SET DONE=0
	      NEW FIRSTSEQ SET FIRSTSEQ=0
	      FOR  SET COUNTER=$ORDER(^XTV(8989.5,"AC",RPTIEN,DATASTRING,COUNTER)) QUIT:(+COUNTER'>0)!DONE  DO
	      . SET FIRSTSEQ=FIRSTSEQ+1
	      . IF COUNTER'=FIRSTSEQ SET DONE=1
	      ;"
	      ;"CHECK TO SEE IF REPORTS ALREADY EXIST
	      NEW CDCIEN,WHOIEN,ZWHOIEN,CURRENTIEN,CDCEXISTS,WHOEXISTS,ZWHOEXISTS,TEMP
	      SET CDCIEN=$ORDER(^ORD(101.24,"B","TMG GROWTH CHARTS",0))
	      SET WHOIEN=$ORDER(^ORD(101.24,"B","TMG WHO GROWTH CHARTS",0))
	             SET ZWHOIEN=$ORDER(^ORD(101.24,"B","TMG WHO Z-SCORE GROWTH CHARTS",0))
	      SET COUNTER=0,CDCEXISTS=0,WHOEXISTS=0,ZWHOEXISTS=0
	      FOR  SET COUNTER=$ORDER(^XTV(8989.5,"AC",RPTIEN,DATASTRING,COUNTER)) QUIT:(+COUNTER'>0)  DO
	      . SET CURRENTIEN=+$GET(^XTV(8989.5,"AC",RPTIEN,DATASTRING,COUNTER))
	      . IF CURRENTIEN=CDCIEN DO
	      . . SET CDCEXISTS=1
	             . IF CURRENTIEN=WHOIEN DO
	             . . SET WHOEXISTS=1
	             . IF CURRENTIEN=ZWHOIEN DO
	             . . SET ZWHOEXISTS=1
	      IF CDCEXISTS SET TEMP="-1^CDC REPORT EXISTS" GOTO IN2
	      ;"
	      ;"ATTEMPT ADDITIONS AND RETURN RESULTS
	      NEW ERROR
	      D EN^XPAR(FOUND,"ORWRP REPORT LIST",FIRSTSEQ,"TMG GROWTH CHARTS",.ERROR)
	      IF ERROR=0 DO
	      . SET TEMP=1
	      ELSE  DO
	      . SET TEMP="-1^"_$PIECE(ERROR,"^",2)
	      ;"
	      ;"FIND THE SECOND AVAILABLE SEQUENCE
	      ;"SET DONE=0
	      ;"SET COUNTER=0
IN2	      IF WHOEXISTS SET ERROR="-1^WHO REPORT EXISTS" GOTO IN3
	             NEW SECONDSEQ SET SECONDSEQ=0
	      ;"FOR  SET COUNTER=$ORDER(^XTV(8989.5,"AC",RPTIEN,DATASTRING,COUNTER)) QUIT:(+COUNTER'>0)!DONE  DO
	      ;". SET SECONDSEQ=SECONDSEQ+1
	      ;". IF COUNTER'=SECONDSEQ SET DONE=1
	      ;"
	      ;"ATTEMPT SECOND ADDITION
	      ;"D EN^XPAR(FOUND,"ORWRP REPORT LIST",SECONDSEQ,"TMG WHO GROWTH CHARTS",.ERROR)
	      SET SECONDSEQ=FIRSTSEQ_".1"
	      DO EN^XPAR(FOUND,"ORWRP REPORT LIST",SECONDSEQ,"TMG WHO GROWTH CHARTS",.ERROR)
	             ;"ATTEMPT ASSIGNING WHO Z-SCORE
IN3	          NEW ZWHOERROR
	             IF ZWHOEXISTS SET ZWHOERROR="-1^ZWHO REPORT EXISTS" GOTO ARPTDN
	             NEW THIRDSEQ SET THIRDSEQ=FIRSTSEQ_".2"
	             DO EN^XPAR(FOUND,"ORWRP REPORT LIST",THIRDSEQ,"TMG WHO Z-SCORE GROWTH CHARTS",.ZWHOERROR)
ARPTDN	      IF (ERROR=0)&(TEMP=1)&(ZWHOERROR=0) DO
	      . SET RESULT="1^SUCCESSFUL"
	      ELSE  DO
	      . SET RESULT="-1^"
	      . IF TEMP'=1 DO
	      . . SET RESULT=RESULT_"CDC-"_$PIECE(TEMP,"^",2)
	      . IF ERROR'=0 DO
	      . . SET RESULT=RESULT_"**WHO-"_$PIECE(ERROR,"^",2)
	             . IF ZWHOERROR'=0 DO
	             . . SET RESULT=RESULT_"**ZWHO-"_$PIECE(ZWHOERROR,"^",2)
	             QUIT
