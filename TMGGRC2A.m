TMGGRC2A	     ;TMG/kst-Work with Growth Chart Data ;10/5/10 ; 9/27/11, 3/24/21
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
	      ;"TMGCOMGR(ROOT,GRAPHTYP) --Common graphic entry point
	      ;"SETITEM(ROOT,X) -- set output into ROOT
	      ;"ADDSECT(LABEL,MODULES,ROOT) -- send out section of code, from LABEL until EOF found
	      ;"GETPAT(TMGDFN,AGE,GENDER,DOB,TMGERR)  -- ensure Patient variables are setup.
	      ;"=======================================================================
	      ;
TMGCOMGR(ROOT,GRAPHTYP)	       ;
	      ;"Purpose: Entry point, as called from CPRS REPORT system
	      ;"Input: ROOT -- Pass by NAME.  This is where output goes
	      ;"       GRAPHTYP -- Graph Type.  Legend:
	      ;"              CH-HT  (1) --    Height percentile for age (child)
	      ;"              INF-HC  (2) --   Head Circumference percentile for age
	      ;"              CH-WT  (3) --    Weight percentile for age (child)
	      ;"              CH-BMI (4) --    BMI percentile for age (child)
	      ;"              INF-WT4L (5) -   Weight percentile for Length (infant)
	      ;"              CH-WT4S (5.5)-   Weight percentile for Stature (child)
	      ;"              INF-BMI (6) --   BMI percentile for age (infant)
	      ;"              INF-WT  (7) --   Weight percentile for age (infant)
	      ;"              INF-LN  (8) --   Length percentile for age (infant)
	      ;"               WHO-BA  (9) --  WHO BMI by Age
	      ;"               WHO-HA  (10) -- WHO Height by Age
	      ;"               WHO-WA  (11) -- WHO Weight by Age
	      ;"               WHO-HC  (12) -- WHO Head Circumference by Age
	      ;"               WHO-WL  (13) -- WHO Weight for Length
	      ;"               WHO-WS  (14) -- WHO Weight for Stature
	      ;"Results: None
	      ;"Output: @ROOT is filled
	      ;
	      NEW TMGERR,AGE,GENDER,TMGERR,DOB
	      DO GETPAT(.TMGDFN,.AGE,.GENDER,.DOB,.TMGERR)
	      DO CHECKTYP(.GRAPHTYP,.TMGERR) GOTO:($D(TMGERR)) TGERR
	      DO ADDSECT("TOP","TMGGRC3A",.ROOT,.GENDER)
	      DO ADDSECT("GLIB","TMGGRC3A,TMGGRC3B,TMGGRC3C,TMGGRC3D,TMGGRC3E",.ROOT)
	      DO ADDSECT("SCRIPT","TMGGRC3F",.ROOT)
	      DO SETGRAPH(ROOT,AGE,GENDER,GRAPHTYP,.TMGERR) GOTO:($D(TMGERR)) TGERR
	      DO SETLINES^TMGGRC2B(ROOT,TMGDFN,DOB,AGE,GENDER,GRAPHTYP,.TMGERR) GOTO:($D(TMGERR)) TGERR
	      GOTO TGDN
TGERR	 DO SETITEM(ROOT,.TMGERR)
TGDN	  DO ADDSECT("ENDING","TMGGRC3F",.ROOT)
	      QUIT
	      ;
CHECKTYP(GRAPHTYP,TMGERR)	       ;
	      SET GRAPHTYP=$GET(GRAPHTYP)
	      IF ("INF;CH;WHO;ZWO"'[$P(GRAPHTYP,"-",1))!("HT;HC;WT;BMI;WT4L;WT4S;LN;BA;HA;WA;WL;WS;WH"'[$P(GRAPHTYP,";",2)) DO
	      . SET TMGERR="<bold>Error</bold>: Invalid chart type request: '"_GRAPHTYP_"'"
	      QUIT
	      ;
SETITEM(ROOT,X)	       ; -- set item in list
	      SET @ROOT@($ORDER(@ROOT@(9999),-1)+1)=X
	      QUIT
	      ;
ADDSECT(LABEL,MODULES,ROOT,GENDER)	       ;
	      ;"Purpose: To send out section of code, from LABEL until EOF found
	      ;"Input: LABEL -- e.g. "L1", "L2" etc.  Must match labels in TMGGRC3*
	      ;"       MODULES -- one or more routine names.  If multiple, separate by commas
	      ;"               e.g. 'TMGGRC3A,TMGGRC3B'
	      NEW I,LINE,REF
	      NEW DONE SET DONE=0
	      NEW MODNUM SET MODNUM=$LENGTH(MODULES,",")
	      NEW MODI,MODL
	      FOR MODI=1:1:MODNUM DO  QUIT:DONE
	      . SET MODL=$$TRIM^XLFSTR($PIECE(MODULES,",",MODI))
	      . NEW CONT SET CONT=0
	      . FOR I=1:1 QUIT:DONE!CONT  DO
	      . . SET REF=LABEL_"+"_I_"^"_MODL
	      . . SET LINE=$TEXT(@REF)
	      . . ;"SET LINE=$TEXT(@LABEL+I^TMGGRC3)
	      . . IF LINE="" SET DONE=1 QUIT
	      . . SET LINE=$PIECE(LINE,";;",2,99)
	      . . IF LINE["GENDERCOLOR" DO
	      . . . SET LINE=$PIECE(LINE,"GENDERCOLOR",1)_$$GETCOLOR($GET(GENDER))_$PIECE(LINE,"GENDERCOLOR",2)
	      . . IF LINE["GETLCOLOR" DO
	      . . . SET LINE=$PIECE(LINE,"GETLCOLOR",1)_$$GETLCOLOR()_$PIECE(LINE,"GETLCOLOR",2)
	      . . IF $$TRIM^XLFSTR(LINE)="EOF" SET DONE=1 QUIT
	      . . IF $$TRIM^XLFSTR(LINE)="CONTINUED" SET CONT=1 QUIT
	      . . DO SETITEM(.ROOT,LINE)
	      QUIT
	      ;
GETLCOLOR()	
	             ;"Return line color
	             NEW TMGRESULT SET TMGRESULT="red"  ;"Default line color
	             ;"NEW COLOR SET COLOR=$PIECE($GET(^TMG(22700001,1,2)),"^",1)
                     NEW COLOR SET COLOR=$PIECE($GET(^TMG(22738,1,2)),"^",1)
	             IF COLOR'="" SET TMGRESULT=COLOR
	             QUIT TMGRESULT
	             ;
GETCOLOR(GENDER)	
	             ;"Return hex gender color
	             NEW TMGRESULT SET TMGRESULT="#FFFFFF" ;"Default is white
	             NEW PIECE,COLOR
	             IF GENDER="" GOTO GCDN
	             ELSE  IF GENDER["F" DO
	             . SET PIECE=3
	             . SET TMGRESULT="#FFC0CB"  ;" Default pink
	             ELSE  DO
	             . SET PIECE=2
	             . SET TMGRESULT="#87CEEB"  ;"Default blue
	             ;"SET COLOR=$PIECE($GET(^TMG(22700001,1,2)),"^",PIECE)
                     SET COLOR=$PIECE($GET(^TMG(22738,1,2)),"^",PIECE)
	             IF COLOR'="" SET TMGRESULT=COLOR
GCDN	         QUIT TMGRESULT
	             ;
GETPAT(TMGDFN,AGE,GENDER,DOB,TMGERR)	        ;
	      ;"Purpose: To ensure Patient variables are setup.
	      ;"Input: TMGDFN -- The patient to get info from
	      ;"       AGE -- PASS BY REFERENCE, AN OUT PARAMETER
	      ;"       GENDER -- PASS BY REFERENCE, AN OUT PARAMETER
	      ;"       DOB -- PASS BY REFERENCE, AN OUT PARAMETER
	      ;"       TMGERR -- PASS BY REFERENCE, AN OUT PARAMETER
	      ;"Result: None
	      IF +$GET(TMGDFN)'>0 DO  QUIT
	      . SET TMGERR="<bold>Error</bold>: Patient ID 'DFN' not defined.  Contact administrator."
	      SET AGE=+$$GET1^DIQ(2,TMGDFN_",",.033) ;"returns calculated field, INTEGER yrs
	      SET DOB=$PIECE($GET(^DPT(TMGDFN,0)),"^",3)
	      IF AGE<18 DO
	      . NEW %,X,X1,X2,%Y
	      . DO NOW^%DTC
	      . SET X1=X ;"now
	      . SET X2=DOB
	      . DO ^%DTC  ;"result out in X (days delta)
	      . IF %Y=0 QUIT  ;"dates are unworkable
	      . SET AGE=+$JUSTIFY(X/365,0,4)
	      SET GENDER=$PIECE($GET(^DPT(TMGDFN,0)),"^",2)
	      IF GENDER="" DO  QUIT
	      . SET TMGERR="<bold>Error</bold>: Patient SEX not defined."
	      QUIT
	      ;
SETGRAPH(ROOT,AGEYR,GENDER,GRAPHTYP,TMGERR)	       ;
	      ;"Purpose: Setup labels, range etc for graph.  This will customize
	      ;"         The graph based on patients age--which will determine which
	      ;"         which graph should be displayed.
	      ;"Input: ROOT -- Pass by NAME.  This is where output goes
	      ;"       AGEYR -- Patient age in *YEARS*
	      ;"       GENDER -- Patient SEX -- Should be 'M' OR 'F'
	      ;"       GRAPHTYP -- Graph Type, See above for documentation
	      ;"       TMGERR -- PASS BY REFERENCE.  An OUT Parameter.
	      ;"Results: None
	      ;"Output: @ROOT is filled
	      NEW TITLE,XMIN,XMAX,XTITLE,YMIN,YMAX,YTITLE,XINC,YINC
	      SET XINC=1,YINC=5
	      SET GRAPHTYP=$GET(GRAPHTYP)
	      ;
	      SET TITLE="PHYSICAL GROWTH PERCENTILES"
	      IF GRAPHTYP["INF-" DO
	      . SET XMIN=0,XMAX=36
	      . SET XTITLE="AGE (MONTHS)"
	      ELSE  DO
	      . SET XMIN=1,XMAX=22
	      . SET XTITLE="AGE (YEARS)"
	      ;
	      IF GRAPHTYP="INF-LN" DO
	      . SET TITLE="LENGTH  "_TITLE
	      . SET YTITLE="LENGTH (cm)"
	      . SET YMIN=40,YMAX=110
	      ELSE  IF GRAPHTYP="INF-WT" DO
	      . SET TITLE="WEIGHT  "_TITLE
	      . SET YTITLE="WEIGHT (kg)"
	      . SET YMIN=2,YMAX=18
	      . SET XMIN=0,XMAX=40
	      ELSE  IF GRAPHTYP="INF-HC" DO
	      . SET TITLE="HEAD CIRCUMFERENCE  "_TITLE
	      . SET YMIN=30,YMAX=55
	      . SET YTITLE="HEAD CIRC (cm)"
	      . SET XMIN=0,XMAX=40
	      ELSE  IF GRAPHTYP="INF-BMI" DO
	      . SET TITLE="BODY MASS INDEX  "_TITLE
	      . SET YTITLE="BMI (kg/m^2)"
	      . SET YMIN=1,YMAX=20
	      ELSE  IF GRAPHTYP="INF-WT4L" DO
	      . SET TITLE="WEIGHT FOR LENGTH  "_TITLE
	      . SET YTITLE="WEIGHT (kg)"
	      . SET YMIN=10,YMAX=19
	      . SET XTITLE="LENGTH (cm)"
	      . SET XMIN=80,XMAX=100
	      . SET XINC=1,YINC=1
	      ELSE  IF GRAPHTYP="CH-HT" DO
	      . SET TITLE="STATURE  "_TITLE
	      . SET YTITLE="STATURE (cm)"
	      . SET YMIN=70,YMAX=200
	      ELSE  IF GRAPHTYP="CH-WT" DO
	      . SET TITLE="WEIGHT  "_TITLE
	      . SET YTITLE="WEIGHT (kg)"
	      . SET YMIN=10,YMAX=100
	      ELSE  IF GRAPHTYP="CH-BMI" DO
	      . SET TITLE="BODY MASS INDEX  "_TITLE
	      . SET YTITLE="BMI (kg/m^2)"
	      . SET YMIN=13,YMAX=35
	      ELSE  IF GRAPHTYP="CH-WT4S" DO
	      . SET TITLE="WEIGHT FOR STATURE  "_TITLE
	      . SET YTITLE="WEIGHT (kg)"
	      . SET YMIN=0,YMAX=20
	      . SET XTITLE="STATURE (cm)"
	      . SET XMIN=60,XMAX=150
	      ELSE  IF GRAPHTYP["WHO-BA" DO
	      . IF $PIECE(GRAPHTYP,"-",3)="B2" DO
	      . . SET TITLE="BMI-for-Age (Birth to 2 Years)"
	      . . SET YMIN=1,YMAX=21
	      . . SET XMIN=0,XMAX=25
	      . ELSE  IF $PIECE(GRAPHTYP,"-",3)="B5" DO
	      . . SET TITLE="BMI-for-Age (Birth to 5 Years)"
	      . . SET YMIN=10,YMAX=21
	      . . SET XMIN=0,XMAX=61
	      . . SET XINC=2,YINC=1
	      . ELSE  IF $PIECE(GRAPHTYP,"-",3)="519" DO
	      . . SET TITLE="BMI-for-Age (5 Years to 19 Years)"
	      . . SET YMIN=10,YMAX=32
	      . . SET XMIN=60,XMAX=229
	      . . SET XINC=6,YINC=2
	      . ELSE  DO
	      . . SET TITLE="BMI-for-Age (2 Years to 5 Years)"
	      . . SET YMIN=12,YMAX=20
	      . . SET XMIN=23,XMAX=61
	      . . SET XINC=2,YINC=1
	      . SET YTITLE="BMI (kg/m^2)"
	      . SET XTITLE="AGE (MONTHS)"
	      ELSE  IF GRAPHTYP["WHO-HA" DO
	      . IF $PIECE(GRAPHTYP,"-",3)="B6" DO
	      . . SET TITLE="Length-for-Age (Birth to 6 Months)"
	      . . SET YMIN=45,YMAX=76
	      . . SET XMIN=0,XMAX=6
	      . . SET XINC=1,YINC=5
	      . ELSE  IF $PIECE(GRAPHTYP,"-",3)="B2" DO
	      . . SET TITLE="Length-for-Age (Birth to 2 Years)"
	      . . SET YMIN=45,YMAX=95
	      . . SET XMIN=0,XMAX=25
	      . . SET XINC=1,YINC=5
	      . ELSE  IF $PIECE(GRAPHTYP,"-",3)="62" DO
	      . . SET TITLE="Length-for-Age (6 Months to 2 Years)"
	      . . SET YMIN=60,YMAX=95
	      . . SET XMIN=6,XMAX=25
	      . SET XINC=1,YINC=5
	      . ELSE  IF $PIECE(GRAPHTYP,"-",3)="25" DO
	      . . SET TITLE="Height-for-Age (2 Years to 5 Years)"
	      . . SET YMIN=75,YMAX=120
	      . . SET XMIN=23,XMAX=61
	      . . SET XINC=2,YINC=5
	      . ELSE  IF $PIECE(GRAPHTYP,"-",3)="519" DO
	      . . SET TITLE="Height-for-Age (5 Years to 19 Years)"
	      . . SET YMIN=90,YMAX=200
	      . . SET XMIN=60,XMAX=229
	      . . SET XINC=6,YINC=10
	      . ELSE  DO
	      . . SET TITLE="Length/Height-for-Age (Birth to 5 Years)"
	      . . SET YMIN=40,YMAX=125
	      . . SET XMIN=0,XMAX=61
	      . . SET XINC=2,YINC=5
	      . SET YTITLE="LENGTH (cm)"
	      . SET XTITLE="AGE (MONTHS)"
	      ELSE  IF GRAPHTYP["WHO-WA" DO
	      . IF $PIECE(GRAPHTYP,"-",3)="B6" DO
	      . . SET TITLE="Weight-for-Age (Birth to 6 Months)"
	      . . SET YMIN=0,YMAX=10
	      . . SET XMIN=0,XMAX=6
	      . . SET XINC=1,YINC=1
	      . ELSE  IF $PIECE(GRAPHTYP,"-",3)="B2" DO
	      . . SET TITLE="Weight-for-Age (Birth to 2 Years)"
	      . . SET YMIN=0,YMAX=16
	      . . SET XMIN=0,XMAX=25
	      . . SET XINC=1,YINC=1
	      . ELSE  IF $PIECE(GRAPHTYP,"-",3)="62" DO
	      . . SET TITLE="Weight-for-Age (6 Months to 2 Years)"
	      . . SET YMIN=5,YMAX=16
	      . . SET XMIN=6,XMAX=25
	      . . SET XINC=1,YINC=1
	      . ELSE  IF $PIECE(GRAPHTYP,"-",3)="25" DO
	      . . SET TITLE="Weight-for-Age (2 Years to 5 Years)"
	      . . SET YMIN=8,YMAX=26
	      . . SET XMIN=23,XMAX=61
	      . . SET XINC=2,YINC=1
	      . ELSE  IF $PIECE(GRAPHTYP,"-",3)="510" DO
	      . . SET TITLE="Weight-for-Age (5 Years to 10 Years)"
	      . . SET YMIN=10,YMAX=70
	      . . SET XMIN=60,XMAX=121
	      . . SET XINC=3,YINC=5
	      . ELSE  DO
	      . . SET TITLE="Weight-for-Age (Birth to 5 Years)"
	      . . SET YMIN=0,YMAX=25
	      . . SET XMIN=0,XMAX=61
	      . . SET XINC=2,YINC=2
	      . SET YTITLE="WEIGHT (kg)"
	      . SET XTITLE="AGE (MONTHS)"
	      ELSE  IF GRAPHTYP["WHO-HC" DO
	      . IF $PIECE(GRAPHTYP,"-",3)="BT" DO
	      . . SET TITLE="Head circumference-for-Age (Birth to 13 Weeks)"
	      . . SET YMIN=31,YMAX=43
	      . . SET XMIN=0,XMAX=3.25
	      . . SET XINC=.25,YINC=1
	      . ELSE  IF $PIECE(GRAPHTYP,"-",3)="B2" DO
	      . . SET TITLE="Head circumference-for-Age (Birth to 2 Years)"
	      . . SET YMIN=31,YMAX=51
	      . . SET XMIN=0,XMAX=25
	      . . SET XINC=2,YINC=1
	      . ELSE  DO
	      . . SET TITLE="Head circumference-for-Age (Birth to 5 Years)"
	      . . SET YMIN=31,YMAX=53
	      . . SET XMIN=0,XMAX=61
	      . . SET XINC=2,YINC=2
	      . SET YTITLE="HEAD CIRC (cm)"
	      . SET XTITLE="AGE (MONTHS)"
	      ELSE  IF GRAPHTYP["WHO-WL" DO
	      . SET TITLE="Weight-for-length (Birth to 2 Years)"
	      . SET YTITLE="WEIGHT (kg)"
	      . ;"SET YMIN=0,YMAX=20
	      . SET XTITLE="LENGTH (cm)"
	      . ;"SET XMIN=60,XMAX=150
	      . ;"SET XMIN=0,XMAX=60
	      . SET YMIN=1,YMAX=25
	      . SET XMIN=45,XMAX=112
	      . SET XINC=5,YINC=2
	      ELSE  IF GRAPHTYP["WHO-WH" DO
	      . SET TITLE="Weight for height (2 to 5 Years)"
	      . SET YTITLE="WEIGHT (kg)"
	      . SET YMIN=5,YMAX=29
	      . SET XTITLE="HEIGHT (cm)"
	      . SET XMIN=65,XMAX=130
	      . SET XINC=5,YINC=2
	      . ;"SET XTITLE="AGE (MONTHS)"
	      ELSE  IF GRAPHTYP["ZWO-HA" DO
	      . IF $PIECE(GRAPHTYP,"-",3)="B6" DO
	      . . SET TITLE="Length-for-Age  Birth to 6 Months (z-scores)"
	      . . SET YMIN=40,YMAX=75
	      . . SET XMIN=0,XMAX=6
	      . . SET XINC=1,YINC=5
	      . . SET XTITLE="Age (completed months)"
	      . . SET YTITLE="Length (cm)"
	      . ELSE  IF $PIECE(GRAPHTYP,"-",3)="62" DO
	      . . SET TITLE="Length-for-Age  6 Months to 2 Years (z-scores)"
	      . . SET YMIN=55,YMAX=100
	      . . SET XMIN=6,XMAX=25
	      . . SET XINC=1,YINC=5
	      . . SET XTITLE="Age (completed months)"
	      . . SET YTITLE="Length (cm)"
	      . ELSE  IF $PIECE(GRAPHTYP,"-",3)="B2" DO
	      . . SET TITLE="Length-for-Age  Birth to 2 Years (z-scores)"
	      . . SET YMIN=40,YMAX=100
	      . . SET XMIN=0,XMAX=26
	      . . SET XINC=1,YINC=5
	      . . SET XTITLE="Age (completed months)"
	      . . SET YTITLE="Length (cm)"
	      . ELSE  IF $PIECE(GRAPHTYP,"-",3)="25" DO
	      . . SET TITLE="Height-for-Age  2 to 5 Years (z-scores)"
	      . . SET YMIN=35,YMAX=125
	      . . SET XMIN=24,XMAX=62
	      . . SET XINC=2,YINC=5
	      . . SET XTITLE="Age (completed months)"
	      . . SET YTITLE="Height (cm)"
	      . ELSE  IF $PIECE(GRAPHTYP,"-",3)="B5" DO
	      . . SET TITLE="Length/height-for-Age  Birth to 5 Years (z-scores)"
	      . . SET YMIN=40,YMAX=125
	      . . SET XMIN=0,XMAX=61
	      . . SET XINC=2,YINC=5
	      . . SET XTITLE="Age (completed months)"
	      . . SET YTITLE="Length/Height (cm)"
              . ELSE  IF $PIECE(GRAPHTYP,"-",3)="519" DO
	      . . SET TITLE="Height-for-Age  5 Years to 19 Years (z-scores)"
	      . . SET YMIN=90,YMAX=200
	      . . SET XMIN=60,XMAX=229
	      . . SET XINC=6,YINC=10
	      . . SET XTITLE="Age (completed months)"
	      . . SET YTITLE="Length/Height (cm)"
	      ELSE  IF GRAPHTYP["ZWO-WA" DO
	      . IF $PIECE(GRAPHTYP,"-",3)="B6" DO
	      . . SET TITLE="Weight-for-Age  Birth to 6 Months (z-scores)"
	      . . SET YMIN=1,YMAX=11
	      . . SET XMIN=0,XMAX=6
	      . . SET XINC=1,YINC=1
	      . . SET XTITLE="Age (completed months)"
	      . . SET YTITLE="Weight (kg)"
	      . ELSE  IF $PIECE(GRAPHTYP,"-",3)="62" DO
	      . . SET TITLE="Weight-for-Age  6 Months TO 2 Years (z-scores)"
	      . . SET YMIN=5,YMAX=18
	      . . SET XMIN=6,XMAX=26
	      . . SET XINC=1,YINC=1
	      . . SET XTITLE="Age (completed months)"
	      . . SET YTITLE="Weight (kg)"
	      . ELSE  IF $PIECE(GRAPHTYP,"-",3)="B2" DO
	      . . SET TITLE="Weight-for-Age  Birth to 2 Years (z-scores)"
	      . . SET YMIN=1,YMAX=18
	      . . SET XMIN=0,XMAX=26
	      . . SET XINC=1,YINC=1
	      . . SET XTITLE="Age (completed months)"
	      . . SET YTITLE="Weight (kg)"
	      . ELSE  IF $PIECE(GRAPHTYP,"-",3)="25" DO
	      . . SET TITLE="Weight-for-Age  2 to 5 Years (z-scores)"
	      . . SET YMIN=7,YMAX=30
	      . . SET XMIN=24,XMAX=60
	      . . SET XINC=1,YINC=1
	      . . SET XTITLE="Age (completed months)"
	      . . SET YTITLE="Weight (kg)"
	      . ELSE  IF $PIECE(GRAPHTYP,"-",3)="B5" DO
	      . . SET TITLE="Weight-for-Age  Birth to 5 Years (z-scores)"
	      . . SET YMIN=1,YMAX=30
	      . . SET XMIN=0,XMAX=60
	      . . SET XINC=2,YINC=2
	      . . SET XTITLE="Age (completed months)"
	      . . SET YTITLE="Weight (kg)"
              . ELSE  IF $PIECE(GRAPHTYP,"-",3)="510" DO
	      . . SET TITLE="Weight-for-Age  5 Years to 10 Years (z-scores)"
	      . . SET YMIN=10,YMAX=70
	      . . SET XMIN=60,XMAX=121
	      . . SET XINC=3,YINC=5
	      . . SET XTITLE="Age (completed months)"
	      . . SET YTITLE="Weight (kg)"
	      ELSE  IF GRAPHTYP["ZWO-BA" DO
	      . IF $PIECE(GRAPHTYP,"-",3)="B2" DO
	      . . SET TITLE="BMI-for-Age  Birth to 2 Years (z-scores)"
	      . . SET YMIN=9,YMAX=23
	      . . SET XMIN=0,XMAX=26
	      . . SET XINC=1,YINC=1
	      . . SET XTITLE="Age (completed months)"
	      . . SET YTITLE="BMI (kg/m2)"
	      . ELSE  IF $PIECE(GRAPHTYP,"-",3)="25" DO
	      . . SET TITLE="BMI-for-Age  2 to 5 Years (z-scores)"
	      . . SET YMIN=11,YMAX=22
	      . . SET XMIN=24,XMAX=60
	      . . SET XINC=2,YINC=1
	      . . SET XTITLE="Age (completed months)"
	      . . SET YTITLE="BMI (kg/m2)"
	      . ELSE  IF $PIECE(GRAPHTYP,"-",3)="B5" DO
	      . . SET TITLE="BMI-for-Age  Birth to 5 Years (z-scores)"
	      . . SET YMIN=9,YMAX=23
	      . . SET XMIN=0,XMAX=60
	      . . SET XINC=2,YINC=1
	      . . SET XTITLE="Age (completed months)"
	      . . SET YTITLE="BMI (kg/m2)"
              . ELSE  IF $PIECE(GRAPHTYP,"-",3)="519" DO
	      . . SET TITLE="BMI-for-Age  5 Years to 19 Years (z-scores)"
	      . . SET YMIN=10,YMAX=37
	      . . SET XMIN=60,XMAX=229
	      . . SET XINC=6,YINC=2
	      . . SET XTITLE="Age (completed months)"
	      . . SET YTITLE="BMI (kg/m2)"
	      ELSE  IF GRAPHTYP["ZWO-WL" DO
	      . SET TITLE="Weight-for-length  Birth to 2 Years (z-scores)"
	      . SET YMIN=1,YMAX=25
	      . SET XMIN=45,XMAX=112
	      . SET XINC=5,YINC=2
	      . SET XTITLE="Length (cm)"
	      . SET YTITLE="Weight (kg)"
	      ELSE  IF GRAPHTYP["ZWO-WH" DO
	      . SET TITLE="Weight-for-height  2 to 5 Years (z-scores)"
	      . SET YMIN=5,YMAX=32
	      . SET XMIN=65,XMAX=122
	      . SET XINC=5,YINC=2
	      . SET XTITLE="Height (cm)"
	      . SET YTITLE="Weight (kg)"
	             ELSE  IF GRAPHTYP["ZWO-HC" DO
	      . IF $PIECE(GRAPHTYP,"-",3)="BT" DO
	      . . SET TITLE="Head circ-for-Age  Birth to 13 Weeks  (z-scores)"
	      . . SET YMIN=31,YMAX=43
	      . . SET XMIN=0,XMAX=3.25
	      . . SET XINC=.25,YINC=1
	      . ELSE  IF $PIECE(GRAPHTYP,"-",3)="B2" DO
	      . . SET TITLE="Head circ-for-Age  Birth to 2 Years  (z-scores)"
	      . . SET YMIN=31,YMAX=55
	      . . SET XMIN=0,XMAX=25
	      . . SET XINC=2,YINC=1
	      . ELSE  DO
	      . . SET TITLE="Head circ-for-Age  Birth to 5 Years  (z-scores)"
	      . . SET YMIN=31,YMAX=55
	      . . SET XMIN=0,XMAX=61
	      . . SET XINC=2,YINC=2
	      . SET YTITLE="HEAD CIRC (cm)"
	      . SET XTITLE="AGE (MONTHS)"
	      ;
	      NEW GENDERTITLE
	      IF GENDER="F" SET GENDERTITLE="GIRLS "
	      ELSE  SET GENDERTITLE="BOYS "
	      IF ($PIECE(GRAPHTYP,"-",1)["WHO")!($PIECE(GRAPHTYP,"-",1)["ZWO") SET TITLE=TITLE_" -- "_GENDERTITLE_" -- WHO DATA"
	      ELSE  SET TITLE=GENDERTITLE_TITLE
	      ;
	      NEW SOURCE,ACCESSDT
	      IF (GRAPHTYP["WHO")!(GRAPHTYP["ZWO") DO
	      . SET SOURCE="http://www.who.int/childgrowth/standards/en/"
	      . SET ACCESSDT="2/1/2011"
	      ELSE  DO
	      . SET SOURCE="http://www.cdc.gov/growthcharts/percentile_data_files.htm"
	      . SET ACCESSDT="10/21/2010"
	      DO SETGRAPH^TMGGRC3F(.ROOT,TITLE,XMIN,XMAX,XTITLE,YMIN,YMAX,YTITLE,XINC,YINC,SOURCE,ACCESSDT,GRAPHTYP) ;
	         ;
	         ; JS to draw separation line below; not drawn here. Actual call in SetupGraph produced in TMGGRC3.
	      IF (GRAPHTYP="WHO-HA-B5")!(GRAPHTYP="ZWO-HA-B5") DO ADDWHOL1^TMGGRC3F(.ROOT)  ; JS to Draw line to separate height/length (doesn't actually draw line)
	         IF (GRAPHTYP="WHO-BA-B5")!(GRAPHTYP="ZWO-BA-B5") DO ADDWHOL2^TMGGRC3F(.ROOT)  ; JS to Draw line to separate height/length (doesn't actually draw line)
	      QUIT
	      ;
