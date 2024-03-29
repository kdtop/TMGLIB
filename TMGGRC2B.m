TMGGRC2B	      ;TMG/kst-Work with Growth Chart Data ;10/5/10 ; 9/27/11, 3/24/21
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
 ;"SETGRAPH(ROOT,AGEYR,GENDER,GRAPHTYP,TMGERR) --Setup labels, range etc for graph.
 ;"SETLINES(ROOT,TMGDFN,DOB,AGEYR,GENDER,GRAPHTYP,TMGERR) -- send out data for REFERENCE lines.
 ;"SETRLINE(ROOT,AGEYR,GENDER,GRAPHTYP,TMGERR) -- send out data for REFERENCE lines.
 ;"SETPLINE(ROOT,TMGDFN,DOB,AGEYR,GENDER,GRAPHTYP,TMGERR) -- send out data for PATIENT data line.
 ;"GETDATA(TMGDFN,VITIEN,DOB,STARTDT,ENDDT,GRAPHTYP,ARRAY) --populate ARRAY with desired vitals data for date range
 ;"CVTMETRC(VITIEN,DATA) -- convert English measurements to metric
 ;"GETDATES(DOB,MONMIN,MAXMON,STARTDT,ENDDT) --Convert DOB + age range into absolute starting and ending date
 ;"=======================================================================
 ;
SETLINES(ROOT,TMGDFN,DOB,AGEYR,GENDER,GRAPHTYP,TMGERR)	       ;
	      ;"Purpose: to send out data for REFERENCE lines.
	      ;"Input: ROOT -- Pass by NAME.  This is where output goes
	      ;"       TMGDFN -- Patient DFN
	      ;"       DOB -- Patient DOB
	      ;"       AGEYR -- Patient age in *YEARS*
	      ;"       GENDER -- Patient SEX -- Should be 'M' OR 'F'
	      ;"       GRAPHTYP -- Graph Type, See above for documentation
	      ;"      TMGERR -- PASS BY REFERENCE.  An OUT Parameter.
	      ;"Results: None
	      ;"Output: @ROOT is filled
	      DO STRTLINE^TMGGRC3F(.ROOT)    ;
	      DO SETRLINE(ROOT,AGE,GENDER,GRAPHTYP,.TMGERR)  QUIT:($D(TMGERR))
	      DO SETPLINE(ROOT,TMGDFN,DOB,AGE,GENDER,GRAPHTYP,.TMGERR)  QUIT:($D(TMGERR))
	      DO ENDLINES^TMGGRC3F(ROOT)    ;
	      QUIT
	      ;
SETRLINE(ROOT,AGEYR,GENDER,GRAPHTYP,TMGERR)	       ;
	      ;"Purpose: to send out data for REFERENCE lines.
	      ;"Input: ROOT -- Pass by NAME.  This is where output goes
	      ;"       AGEYR -- Patient age in *YEARS*
	      ;"       GENDER -- Patient SEX -- Should be 'M' OR 'F'
	      ;"       GRAPHTYP -- Graph Type, See above for documentation
	      ;"      TMGERR -- PASS BY REFERENCE.  An OUT Parameter.
	      ;"Results: None
	      ;"Output: @ROOT is filled
	      NEW ARRAY
	      SET GRAPHTYP=$GET(GRAPHTYP)
	      ;"Get normal curves values -- ARRAY(%tile,Age)=x^y
	      IF GRAPHTYP["INF-" DO
	      . IF GRAPHTYP="INF-LN" DO LENREF^TMGGRC1(1,GENDER,.ARRAY) QUIT
	      . IF GRAPHTYP="INF-WT" DO WTREF^TMGGRC1(1,GENDER,.ARRAY) QUIT
	      . IF GRAPHTYP="INF-HC" DO HCREF^TMGGRC1(1,GENDER,.ARRAY) QUIT
	      . IF GRAPHTYP="INF-BMI" DO BMIREF^TMGGRC1(1,GENDER,.ARRAY,"5,10,25,50,60,75,80,85,90,95") QUIT
	      . IF GRAPHTYP="INF-WT4L" DO WTLENREF^TMGGRC1(1,GENDER,.ARRAY,"5,10,25,50,75,80,90,95") QUIT
	      ELSE  IF (GRAPHTYP["WHO")!(GRAPHTYP["ZWO") DO
	      . NEW MODE
	      . IF $PIECE(GRAPHTYP,"-",3)="BT" SET MODE=10
	      . ELSE  IF $PIECE(GRAPHTYP,"-",3)="62" SET MODE=7
	      . ELSE  IF ($PIECE(GRAPHTYP,"-",3)="B2")!(GRAPHTYP="WHO-WL")!(GRAPHTYP="ZWO-WH") SET MODE=4
	      . ELSE  IF ($PIECE(GRAPHTYP,"-",3)="25")!(GRAPHTYP="WHO-WH")!(GRAPHTYP="ZWO-WH") SET MODE=5
	      . ELSE  IF $PIECE(GRAPHTYP,"-",3)="B6" SET MODE=6
	      . ELSE  IF $PIECE(GRAPHTYP,"-",3)="510" SET MODE=8
	      . ELSE  IF $PIECE(GRAPHTYP,"-",3)="519" SET MODE=9
	      . ELSE  SET MODE=3
	      . IF GRAPHTYP["ZWO-BA" DO WHBMIREF^TMGGRC1(MODE_"^Z",GENDER,.ARRAY,"-3,-2,-1,0,1,2,3") QUIT
	      . IF GRAPHTYP["ZWO-HA" DO WHHAREF^TMGGRC1(MODE_"^Z",GENDER,.ARRAY,"-3,-2,-1,0,1,2,3") QUIT
	      . IF GRAPHTYP["ZWO-WA" DO WHWAREF^TMGGRC1(MODE_"^Z",GENDER,.ARRAY,"-3,-2,-1,0,1,2,3") QUIT
	      . IF GRAPHTYP["ZWO-WL" DO WHWLREF^TMGGRC1(MODE_"^Z",GENDER,.ARRAY,"-3,-2,-1,0,1,2,3") QUIT
	      . IF GRAPHTYP["ZWO-WH" DO WHWLREF^TMGGRC1(MODE_"^Z",GENDER,.ARRAY,"-3,-2,-1,0,1,2,3") QUIT
	             . IF GRAPHTYP["ZWO-HC" DO WHHCREF^TMGGRC1(MODE_"^Z",GENDER,.ARRAY,"-3,-2,-1,0,1,2,3") QUIT
	      . IF GRAPHTYP["WHO-BA" DO WHBMIREF^TMGGRC1(MODE,GENDER,.ARRAY,"3,15,50,85,97") QUIT
	      . IF GRAPHTYP["WHO-HA" DO WHHAREF^TMGGRC1(MODE,GENDER,.ARRAY,"3,15,50,85,97") QUIT
	      . IF GRAPHTYP["WHO-WA" DO WHWAREF^TMGGRC1(MODE,GENDER,.ARRAY,"3,15,50,85,97") QUIT
	      . IF GRAPHTYP["WHO-HC" DO WHHCREF^TMGGRC1(MODE,GENDER,.ARRAY,"3,15,50,85,97") QUIT
	      . IF GRAPHTYP["WHO-WL" DO WHWLREF^TMGGRC1(MODE,GENDER,.ARRAY,"3,15,50,85,97") QUIT
	      . IF GRAPHTYP["WHO-WH" DO WHWSREF^TMGGRC1(MODE,GENDER,.ARRAY,"3,15,50,85,97") QUIT
	      ELSE  DO
	      . IF GRAPHTYP="CH-HT" DO LENREF^TMGGRC1(2,GENDER,.ARRAY) QUIT
	      . IF GRAPHTYP="CH-WT" DO WTREF^TMGGRC1(2,GENDER,.ARRAY,"5,10,25,50,60,75,80,85,90,95") QUIT
	      . IF GRAPHTYP="CH-BMI" DO BMIREF^TMGGRC1(2,GENDER,.ARRAY,"5,10,25,50,60,75,80,85,90,95") QUIT
	      . IF GRAPHTYP="CH-WT4S" DO WTLENREF^TMGGRC1(2,GENDER,.ARRAY) QUIT
	      . IF GRAPHTYP="CH-WT4S" DO WTLENREF^TMGGRC1(2,GENDER,.ARRAY) QUIT
	      ;
	      NEW PCTL SET PCTL=-4  ;"To account for z-score values
	      NEW LABLX SET LABLX=""
	      ;"Loop through each normal line (e.g. 5th %tile, then 10th%tile etc...
	      ;"-----------------------ehs change---------------------
	      NEW IILINE,REFLINE                              ;declare new variable
	      FOR IILINE=1:1 SET PCTL=$ORDER(ARRAY(PCTL)) QUIT:(PCTL="")  DO  ;make IILINE counter to count the number of lines
	      . NEW LINE
	      . IF LABLX="" DO
	      . . NEW J FOR J=1:1:2 SET LABLX=$ORDER(ARRAY(PCTL,LABLX),-1)
	      . NEW X SET X=""  ;"X Usually stores months, except in Weight For Length/Stature graphs
	      . FOR  SET X=$ORDER(ARRAY(PCTL,X)) QUIT:(X="")  DO
	      . . NEW VAL SET VAL=ARRAY(PCTL,X)
	      . . IF X=LABLX DO
	      . . . IF GRAPHTYP'["ZWO" DO
	      . . . . SET VAL=VAL_"^"_$$SETSUFIX^TMGGRCU(PCTL)
	      . . . ELSE  DO
	      . . . . SET VAL=VAL_"^"_PCTL
	      . . NEW XVAL SET XVAL=X
	      . . IF GRAPHTYP["CH-" DO
	      . . . SET XVAL=+$JUSTIFY(X/12,0,1)
	      . . . SET $PIECE(VAL,"^",1)=XVAL
	      . . SET LINE(XVAL)=VAL
	      . ;"Add one reference/normal line
	   . SET REFLINE=1                    ;set the value of REFLINE to be 1
	   . IF (IILINE=4&(GRAPHTYP["CH-"!(GRAPHTYP["INF-")))!(IILINE=3&(GRAPHTYP["WHO-"))!(IILINE=4&(GRAPHTYP["ZWO")) SET REFLINE=2  ;the if statment that detect the 50% tile line in all graphs
	   . DO ADDLINE^TMGGRC3F(.ROOT,.LINE,REFLINE)  ;" Add Data Set for a reference Line
	      QUIT
	      ;"----------------------------------------------------end ehs change----------------------------------------
SETPLINE(ROOT,TMGDFN,DOB,AGEYR,GENDER,GRAPHTYP,TMGERR)	       ;
	      ;"Purpose: to send out data for PATIENT data line.
	      ;"Input: ROOT -- Pass by NAME.  This is where output goes
	      ;"       TMGDFN -- Patient DFN
	      ;"       DOB -- Patient DOB
	      ;"       AGEYR -- Patient age in *YEARS*
	      ;"       GENDER -- Patient SEX -- Should be 'M' OR 'F'
	      ;"       GRAPHTYP -- Graph Type, See above for documentation
	      ;"       TMGERR -- PASS BY REFERENCE.  An OUT Parameter.
	      ;"Results: None
	      ;"Output: @ROOT is filled
	      ;
	      NEW STARTDT,ENDDT,ARRAY,VITIEN,RESULT
	      SET GRAPHTYP=$GET(GRAPHTYP)
	      IF GRAPHTYP["INF-" DO
	      . DO GETDATES^TMGGRC2C(DOB,0,36,.STARTDT,.ENDDT) ;
	      ELSE  IF (GRAPHTYP["WHO")!(GRAPHTYP["ZWO") DO
	      . IF $PIECE(GRAPHTYP,"-",3)="BT" DO GETDATES^TMGGRC2C(DOB,0,3.25,.STARTDT,.ENDDT) ;
	      . ELSE  IF $PIECE(GRAPHTYP,"-",3)="62" DO GETDATES^TMGGRC2C(DOB,6,24,.STARTDT,.ENDDT) ;
	      . ELSE  IF $PIECE(GRAPHTYP,"-",3)="B2" DO GETDATES^TMGGRC2C(DOB,0,24,.STARTDT,.ENDDT) ;
	      . ELSE  IF $PIECE(GRAPHTYP,"-",3)="25" DO GETDATES^TMGGRC2C(DOB,24,60,.STARTDT,.ENDDT) ;
	      . ELSE  IF $PIECE(GRAPHTYP,"-",3)="B6" DO GETDATES^TMGGRC2C(DOB,0,7,.STARTDT,.ENDDT) ;
	      . ELSE  IF $PIECE(GRAPHTYP,"-",3)="B5" DO GETDATES^TMGGRC2C(DOB,0,60,.STARTDT,.ENDDT) ;
	      . ELSE  IF $PIECE(GRAPHTYP,"-",3)="519" DO GETDATES^TMGGRC2C(DOB,60,228,.STARTDT,.ENDDT) ;
	      . ELSE  IF $PIECE(GRAPHTYP,"-",3)="510" DO GETDATES^TMGGRC2C(DOB,60,120,.STARTDT,.ENDDT) ;
	      . ELSE  IF (GRAPHTYP["WHO-WH")!(GRAPHTYP["ZWO-WH") DO GETDATES^TMGGRC2C(DOB,24,60,.STARTDT,.ENDDT) ;
	      . ELSE  IF (GRAPHTYP["WHO-WL")!(GRAPHTYP["ZWO-WL") DO GETDATES^TMGGRC2C(DOB,0,24,.STARTDT,.ENDDT) ;
	      ELSE  DO
	      . DO GETDATES^TMGGRC2C(DOB,24,240,.STARTDT,.ENDDT) ;
	      ;
	      IF (GRAPHTYP["-HT")!(GRAPHTYP["WHO-HA")!(GRAPHTYP["-LN")!(GRAPHTYP["ZWO-HA") DO
	      . SET VITIEN=+$ORDER(^GMRD(120.51,"B","HEIGHT",0))
	      ELSE  IF (GRAPHTYP["-HC")!(GRAPHTYP["WHO-HC")!(GRAPHTYP["ZWO-HC") DO
	      . SET VITIEN=+$ORDER(^GMRD(120.51,"B","CIRCUMFERENCE/GIRTH",0))
	      ELSE  IF (GRAPHTYP["-WT")!(GRAPHTYP["WHO-WA")!(GRAPHTYP["ZWO-WA") DO
	      . SET VITIEN=+$ORDER(^GMRD(120.51,"B","WEIGHT",0))
	      ELSE  IF (GRAPHTYP["-BMI")!(GRAPHTYP["WHO-BA")!(GRAPHTYP["ZWO-BA") DO
	      . SET VITIEN="BMI"
	      ELSE  SET VITIEN=0
	      SET RESULT=$$GETDATA(TMGDFN,VITIEN,DOB,.STARTDT,.ENDDT,GRAPHTYP,.ARRAY) ;
	      ;"NEW MODE SET MODE=$SELECT(GRAPHTYP["-WT4":1,(1=1):2)
	      DO ADDLINE^TMGGRC3F(.ROOT,.ARRAY,0)  ;" Add PATIENT Data Set
	      DO ADDDESC^TMGGRC3F(.ROOT,GRAPHTYP) ;"Add Description
	      DO ADDTABLE^TMGGRC3F(.ROOT,.ARRAY,GRAPHTYP)  ;" Add Table of Patient Data Set
	      QUIT
	      ;
GETDATA(TMGDFN,VITIEN,DOB,STARTDT,ENDDT,GRAPHTYP,ARRAY)	       ;
	      ;"Purpose: To populate ARRAY with desired vitals data for date range
	      ;"Input: TMGDFN -- The IEN in PATIENT file for patient
	      ;"       VITIEN -- The IEN in 120.51 specifying which vital type to return (e.g. HEIGHT, vs WEIGHT etc.
	      ;"          There is no BMI vital type. If vital type is BMI then VITIEN will equal BMI.
	      ;"       DOB -- Patient's DOB -- in FMDate format
	      ;"       STARTDT -- Start of desired date range, in FMDate format
	      ;"       ENDDT -- End of desired date range, in FMDate format
	      ;"       GRAPHTYP -- Graph Type, See above for documentation
	      ;"       ARRAY -- PASS BY REFERENCE, AN OUT PARAMETER.  See format below.
	      ;"Result: RESULT: -1^MESSAGE  OR 1^MESSAGE
	      ;"Output: ARRAY filled as follows.  Format:
	      ;"           NOTE: AGE is in months if GRAPHTYP["INF-", otherwise it is in years
	      ;"           ARRAY(Index#)=Age^ValueInMetric^^DateOfValue^^%tile
	      ;"              or  ARRAY(Index#)=HeightOrStatureInCm^WeightInKg^^DateOfValue^Age^%tile  if GRAPHTYP["-WT4"
	      ;"NOTE: Vitals are store in English format, and needs to be converted to metric
	      ;"NOTE: If desired vital type is BMI, then it will have to be calculated here, I think...
	      ;
	      ;"Use index --> ^PXRMINDX(120.5,"PI",TMGDFN,MEASUREMENT_IEN,DATE/TIME,IEN)
	      ;" e.g. ^PXRMINDX(120.5,"PI",TMGDFN,VITIEN,DATE/TIME,IENofStoredVitals
	      NEW THISDT SET THISDT=0
	      NEW IDX SET IDX=1
	      KILL ARRAY
	      NEW GENDER SET GENDER=$PIECE($GET(^DPT(TMGDFN,0)),"^",2)
	      SET RESULT="1^SUCCESSFUL"
	      IF (VITIEN="BMI")!(GRAPHTYP["-WT4")!(GRAPHTYP["WHO-BA")!(GRAPHTYP["WHO-WL")!(GRAPHTYP["WHO-WH")!(GRAPHTYP["ZWO-WH")!(GRAPHTYP["ZWO-BA")!(GRAPHTYP["ZWO-WL") DO
	      . NEW HTTYPE,WTTYPE
	      . SET HTTYPE=+$ORDER(^GMRD(120.51,"B","HEIGHT",0))
	      . SET WTTYPE=+$ORDER(^GMRD(120.51,"B","WEIGHT",0))
	      . FOR  SET THISDT=$ORDER(^PXRMINDX(120.5,"PI",TMGDFN,WTTYPE,THISDT)) QUIT:(+THISDT'>0)!(+RESULT=-1)  DO
	      . . NEW THISDTEX  DO
	      . . . NEW Y SET Y=THISDT\1  ;"Trim off Time
	      . . . X ^DD("DD")  ;"Y output as: e.g. "JUL 20,1969"
	      . . . SET THISDTEX=Y
	      . . NEW HTIEN,HTDATE,HEIGHT
	      . . SET HTDATE=+$ORDER(^PXRMINDX(120.5,"PI",TMGDFN,HTTYPE,THISDT\1)) QUIT:HTDATE'>0
	      . . IF (HTDATE\1)'=(THISDT\1) QUIT
	      . . SET HTIEN=+$ORDER(^PXRMINDX(120.5,"PI",TMGDFN,HTTYPE,HTDATE,0))
	      . . SET HEIGHT=$PIECE($GET(^GMR(120.5,HTIEN,0)),"^",8)  ;"English (not metric) units here
	      . . IF HEIGHT'>0 DO  QUIT
	      . . . SET RESULT="-1^NO HEIGHT RECORDED TO CALCULATE BMI"
	      . . IF (THISDT'<STARTDT)&(THISDT'>ENDDT) DO
	      . . . NEW RECIEN SET RECIEN=0
	      . . . FOR  SET RECIEN=+$ORDER(^PXRMINDX(120.5,"PI",TMGDFN,WTTYPE,THISDT,RECIEN)) QUIT:(+RECIEN'>0)  DO
	      . . . . NEW X1,X2,METDATA
	      . . . . SET X2=DOB
	      . . . . SET X1=$PIECE($GET(^GMR(120.5,RECIEN,0)),"^",1)
	      . . . . DO ^%DTC  ;"OUTPUT X=X1-X2, in days
	      . . . . NEW AGE SET AGE=X  ;Days
	      . . . . NEW AGEYR SET AGEYR=X/365    ;"+$JUSTIFY(X/365,0,1) ;"in years
	      . . . . IF GRAPHTYP["CH-" SET AGE=AGEYR
	      . . . . ELSE  SET AGE=X/30     ;"+$JUSTIFY(X/30,0,1) ;"in months
	      . . . . NEW L SET L=+$ORDER(^PXRMINDX(120.5,"PI",TMGDFN,WTTYPE,THISDT,0))
	      . . . . NEW WEIGHT SET WEIGHT=$PIECE($GET(^GMR(120.5,L,0)),"^",8) ;"English units
	      . . . . NEW PCTL SET PCTL=""
	      . . . . NEW ZSCORE SET ZSCORE=""
	      . . . . IF (GRAPHTYP["-WT4")!(GRAPHTYP["WHO-WL")!(GRAPHTYP["WHO-WH")!(GRAPHTYP["ZWO-WL")!(GRAPHTYP["ZWO-WH") DO
	      . . . . . SET METDATA=$$CVTMETRC^TMGGRC2C("W4LS",WEIGHT_"^"_HEIGHT)  ;"returns 'Ht^Wt'
	      . . . . . IF GRAPHTYP["ZWO" SET PCTL=$$ZSCORE^TMGGRC2C(GRAPHTYP,AGEYR,GENDER,WEIGHT/2.2,HEIGHT*2.54)
	      . . . . . ELSE  SET PCTL=$$PCTILE^TMGGRC2C(GRAPHTYP,AGEYR,GENDER,WEIGHT/2.2,HEIGHT*2.54)
	      . . . . . SET ARRAY(IDX)=METDATA_"^^"_THISDTEX_"^"_AGE_"^"_PCTL     ;"HeightOrStatureInCm^WeightInKg^^DateOfValue^Age^%tile
	      . . . . ELSE  IF GRAPHTYP["ZWO-BA" DO
	      . . . . . SET METDATA=$$CVTMETRC^TMGGRC2C("BMI",WEIGHT_"^"_HEIGHT)
	      . . . . . SET ZSCORE=$$ZSCORE^TMGGRC2C(GRAPHTYP,AGEYR,GENDER,METDATA)
	      . . . . . SET ARRAY(IDX)=AGE_"^"_METDATA_"^^"_THISDTEX_"^^"_ZSCORE
	      . . . . ELSE  DO  ;"BMI
	      . . . . . SET METDATA=$$CVTMETRC^TMGGRC2C("BMI",WEIGHT_"^"_HEIGHT)
	      . . . . . SET PCTL=$$PCTILE^TMGGRC2C(GRAPHTYP,AGEYR,GENDER,METDATA)
	      . . . . . SET ARRAY(IDX)=AGE_"^"_METDATA_"^^"_THISDTEX_"^^"_PCTL    ;"Age^ValueInMetric^^DateOfValue^^%tile
	      . . . . SET IDX=IDX+1
	      ELSE  DO
	      . FOR  SET THISDT=$ORDER(^PXRMINDX(120.5,"PI",TMGDFN,VITIEN,THISDT)) QUIT:(+THISDT'>0)  DO
	      . . IF (THISDT'<STARTDT)&(THISDT'>ENDDT) DO
	      . . . NEW THISDTEX  DO
	      . . . . NEW Y SET Y=THISDT\1  ;"Trim off Time
	      . . . . X ^DD("DD")  ;"Y output as: e.g. "JUL 20,1969"
	      . . . . SET THISDTEX=Y
	      . . . NEW RECIEN SET RECIEN=0
	      . . . FOR  SET RECIEN=+$ORDER(^PXRMINDX(120.5,"PI",TMGDFN,VITIEN,THISDT,RECIEN)) QUIT:(+RECIEN'>0)  DO
	      . . . . NEW X1,X2,METDATA
	      . . . . SET X2=DOB
	      . . . . SET X1=$PIECE($GET(^GMR(120.5,RECIEN,0)),"^",1)
	      . . . . DO ^%DTC
	      . . . . NEW AGE SET AGE=X  ;Days
	      . . . . NEW AGEYR SET AGEYR=X/365 ;"+$JUSTIFY(X/365,0,1) ;"in years
	      . . . . IF GRAPHTYP["CH-" SET AGE=AGEYR
	      . . . . ELSE  SET AGE=X/30 ;"+$JUSTIFY(X/30,0,1) ;"in months
	      . . . . SET METDATA=$$CVTMETRC^TMGGRC2C(VITIEN,$PIECE($GET(^GMR(120.5,RECIEN,0)),"^",8))
	      . . . . NEW VALUE
	      . . . . IF GRAPHTYP["ZWO" SET VALUE=$$ZSCORE^TMGGRC2C(GRAPHTYP,AGEYR,GENDER,METDATA)
	      . . . . ELSE  SET VALUE=$$PCTILE^TMGGRC2C(GRAPHTYP,AGEYR,GENDER,METDATA)
	      . . . . SET ARRAY(IDX)=AGE_"^"_METDATA_"^^"_THISDTEX_"^^"_VALUE        ;"Age^ValueInMetric^^DateOfValue^^%tile
	      . . . . SET IDX=IDX+1
	      MERGE ^EDDIE("ARRAY")=ARRAY
	      QUIT RESULT
	      ;
