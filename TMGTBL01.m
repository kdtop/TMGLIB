TMGTIUOJ ;TMG/kst-Text objects for use in CPRS ; 2/2/14, 3/30/15
         ;;1.0;TMG-LIB;**1,17**;03/25/06
 ;
 ;"Kevin Toppenberg MD
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
 ;"TMG text objects
 ;"
 ;"These are bits of code that return text to be included in progress notes etc.
 ;"They are called when the user puts text like this in a note:
 ;"     ... Mrs. Jone's vitals today are |VITALS|, measured in the office...
 ;"     'VITALS' would be a TIU TEXT OBJECT, managed through menu option
 ;"     TIUFJ CREATE OBJECTS MGR
 ;
 ;"=======================================================================
 ;"PUBLIC FUNCTIONS
 ;"=======================================================================
 ;"
 ;"=======================================================================
 ;"PRIVATE FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"Dependancies :  TMGSTUT2 TMGTIUO3 TMGTIUO4 TMGTIUO6
 ;"                TIUL01 XLFDT TIULO XLFSTR
 ;"=======================================================================
 ;
ONEVITAL(DFN,TIU,TYPE,HTMLWRAP)    ;
        ;"Purpose: From GETVITALS, except only returns a single vital specified by TYPE
        ;"Input: DFN -- the patient's unique ID (record#)
        ;"       TIU -- this is an array created by TIU system that
        ;"              contains information about the document being
        ;"              edited/created.  I believe it has this structure:
        ;"                  TIU("VSTR") = LOC;VDT;VTYP
        ;"                  TIU("VISIT") = Visit File IFN^date?
        ;"                  TIU("STOP") = mark to defer workload
        ;"                  TIU("TYPE")=1^title DA^title Name  i.e.:  1^128^OFFICE VISIT^OFFICE VISIT
        ;"                  TIU("SVC")=service, e.g. 'FAMILY PRACTICE'
        ;"                  TIU("EDT")=TIUEDT^DateStr  = event begin time: FMDate^DateStr
        ;"                  TIU("LDT")=TIULDT^DateStr  = event end time: FMDate^DateStr
        ;"                  TIU("VSTR")=LOC;VDT;VTYP  e.g. 'x;x;OFFICE VISIT'
        ;"                  TIU("VISIT")=Visit File IFN
        ;"                  TIU("LOC")=TIULOC
        ;"                  TIU("VLOC")=TIULOC
        ;"                  TIU("STOP")=0  ;0=FALSE, don't worry about stop codes.
        ;"      TYPE -- "Wt" - Returns the weight value
        ;"              "Ht" - Returns the height value
        ;"              "BMI" - Returns the BMI value
        ;"              "BMI-CMT" - Returns the BMI with comment
        ;"              "HC" - Returns the Head Circumference value
        ;"              "TEMP","BP","Pulse" -- usual values
        ;"              "POX" -- Return POx% values
        ;"              "ALL" - (Default value) Return all values
        ;"              or Combination, e.g. "WT,HT,HC"
        ;"      HTMLWRAP -- (Optional) 1 if abnormal values are to be
        ;"                  wrapped in HTML colors
        ;"Results: String with value units and percentile (if available) and date IF not current date
        ;"Output: returns RESULT
        SET HTMLWRAP=+$G(HTMLWRAP)
        ;"SET HTMLWRAP=1    ;"TURN OFF FOR NOW
        NEW DEBUG SET DEBUG=0
        IF DEBUG=1 DO
        . MERGE TIU=^TMG("TMP","RPC","VITALS^TMGTIUOJ","TIU")
        . MERGE DFN=^TMG("TMP","RPC","VITALS^TMGTIUOJ","DFN")
        KILL ^TMG("TMP","RPC","VITALS^TMGTIUOJ")
        MERGE ^TMG("TMP","RPC","VITALS^TMGTIUOJ","TIU")=TIU
        MERGE ^TMG("TMP","RPC","VITALS^TMGTIUOJ","DFN")=DFN
        SET TYPE=$GET(TYPE,"ALL")
        NEW RESULT SET RESULT=""
        NEW CURDT SET CURDT=""
        NEW NOTEDT SET NOTEDT=""
        NEW INDENTSTR SET INDENTSTR="          "
        SET DFN=+$GET(DFN)
        NEW GENDER SET GENDER=$PIECE($GET(^DPT(DFN,0)),"^",2)
        IF $DATA(TIU) DO
        . SET NOTEDT=$$VISDATE^TIULO1(.TIU) ;"Get date of current note (in MM/DD/YY HR:MIN)
        . SET NOTEDT=$PIECE(NOTEDT," ",1)   ;"Drop time
        ELSE  DO
        . SET NOTEDT=$$NOW^XLFDT()\1
        NEW X SET X=$$DT^XLFDT()
        IF X'=NOTEDT\1 DO
        . SET CURDT=$$FMTE^XLFDT(X,"5D") ;"Outputs MM/DD/YYYY
        . ;"SET RESULT=RESULT_"("_CURDT_") "
        ELSE  SET CURDT=NOTEDT
        ;
        NEW PTAGE SET PTAGE=$$PTAGE^TMGTIUO3(DFN,NOTEDT) ;
        NEW WT SET WT=$$WEIGHT^TIULO(DFN)
        NEW HT SET HT=$$HEIGHT^TIULO(DFN)
        ;
        NEW FORCESHOW SET FORCESHOW=$SELECT(TYPE="ALL":0,(1=1):1)
        NEW OLDEXCLD
        ;
        ;"note: Maybe I will later change the calls below to use GETVITLS^TMGGMRV1
        IF (TYPE["TEMP")!(TYPE="ALL") DO
        . NEW TEMP SET TEMP=$$TEMP^TIULO(DFN)
        . IF HTMLWRAP=1 DO
        . . SET TEMP=$$WRPVITAL(TEMP,"T",PTAGE)
        . DO ADDVITAL^TMGTIUO3(.RESULT,TEMP,"T",.CURDT,.NOTEDT,FORCESHOW,,.OLDEXCLD)
        IF (TYPE["BP")!(TYPE="ALL") DO
        . NEW BP SET BP=$$BP^TIULO(DFN)
        . IF HTMLWRAP=1 DO
        . . SET BP=$$WRPVITAL(BP,"BP",PTAGE)
        . DO ADDVITAL^TMGTIUO3(.RESULT,BP,"BP",.CURDT,.NOTEDT,FORCESHOW,,.OLDEXCLD)
        IF (TYPE["RESP")!(TYPE="ALL") DO
        . DO ADDVITAL^TMGTIUO3(.RESULT,$$RESP^TIULO(DFN),"R",.CURDT,.NOTEDT,FORCESHOW,,.OLDEXCLD)
        IF (TYPE["Pulse")!(TYPE="ALL") DO
        . NEW PULSE SET PULSE=$$PULSE^TIULO(DFN)
        . IF HTMLWRAP=1 DO
        . . SET PULSE=$$WRPVITAL(PULSE,"Pulse",PTAGE)
        . DO ADDVITAL^TMGTIUO3(.RESULT,PULSE,"P",.CURDT,.NOTEDT,FORCESHOW,,.OLDEXCLD)
        IF (TYPE["POX")!(TYPE="ALL") DO
        . NEW POX SET POX=$$DOVITALS^TIULO(DFN,"PO2")
        . IF HTMLWRAP=1 DO
        . . SET POX=$$WRPVITAL(POX,"POX",PTAGE)
        . DO ADDVITAL^TMGTIUO3(.RESULT,POX,"POx",.CURDT,.NOTEDT,FORCESHOW,,.OLDEXCLD)
        IF (TYPE["WT")!(TYPE="ALL") DO
        . DO ADDVITAL^TMGTIUO3(.RESULT,WT,"Wt",.CURDT,.NOTEDT,1,PTAGE,.OLDEXCLD)
        . IF (PTAGE<18)&($GET(OLDEXCLD("Wt"))=0) DO ADDPCTLE^TMGTIUO3(.RESULT,"Wt",WT,PTAGE,GENDER)
        IF (TYPE["HT")!(TYPE="ALL") DO
        . NEW INCHHT SET INCHHT=$$FORMATHT^TMGTIUO3(HT,PTAGE)
        . DO ADDVITAL^TMGTIUO3(.RESULT,INCHHT,"Ht",.CURDT,.NOTEDT,1,PTAGE,.OLDEXCLD)
        . IF (PTAGE<18)&($GET(OLDEXCLD("Ht"))=0) DO ADDPCTLE^TMGTIUO3(.RESULT,"Ht",HT,PTAGE,GENDER)
        IF TYPE["HC" DO
        . NEW HC,HCSTR SET HCSTR=$$HC^TMGTIUO3(DFN,.HC)
        . DO ADDVITAL^TMGTIUO3(.RESULT,HC,"HC",.CURDT,.NOTEDT,1,PTAGE,.OLDEXCLD)
        . IF PTAGE<18 DO ADDPCTLE^TMGTIUO3(.RESULT,"HC",HC,PTAGE,GENDER)
        . IF RESULT="" SET RESULT="N/A"
        IF (TYPE["BMI")!(TYPE="ALL") DO
        . IF ($GET(OLDEXCLD("Ht"))=1)!($GET(OLDEXCLD("Wt"))=1) QUIT
        . NEW BMISTR,BMI,IDEALWTS
        . SET BMISTR=$$BMI^TMGTIUO4(PTAGE,HT,WT,.BMI,.IDEALWTS) QUIT:BMI=0  ;"Sets BMISTR and BMI
        . IF HTMLWRAP=1 DO
        . . SET BMISTR=$$WRPVITAL(BMISTR,"BMI",PTAGE)
        . DO ADDVITAL^TMGTIUO3(.RESULT,BMISTR,"BMI",.CURDT,.NOTEDT,1,PTAGE,.OLDEXCLD)
        . IF PTAGE<18 DO ADDPCTLE^TMGTIUO3(.RESULT,"BMI",BMI,PTAGE,GENDER)
        . IF (PTAGE>17)&((TYPE="ALL")!(TYPE["CMT")) SET RESULT=RESULT_$$BMICOMNT^TMGTIUO4(BMI,PTAGE,IDEALWTS) ;"BMI COMMENT
        IF (TYPE["WT")!(TYPE="ALL") DO  ;"Wt done in two parts
        . IF $GET(OLDEXCLD("Wt"))=1 QUIT
        . NEW WTDELTA SET WTDELTA=$$WTDELTA^TMGTIUO4(DFN,.TIU,0,HTMLWRAP)
        . IF WTDELTA="" QUIT
        . SET RESULT=RESULT_"; "_WTDELTA
        ;
        IF RESULT="" DO
        . ;"SET RESULT="[See vital-signs documented in chart]"
        . SET RESULT="[Current vital signs not available]"  ;"changed 12/1/20
        ELSE  DO
        . NEW OUTARRAY,I
        . NEW R2 SET R2=""
        . IF $$SPLITLN^TMGSTUT2(RESULT,.OUTARRAY,80,0,10,";")  ;"width changed from 60 to 80  8/5/19
        . SET I=0 FOR  SET I=$ORDER(OUTARRAY(I)) QUIT:(+I'>0)  DO
        . . IF R2'="" SET R2=R2_";"_$CHAR(13,10)
        . . SET R2=R2_$$REPLWRAP(OUTARRAY(I))
        . SET RESULT=$$TRIM^XLFSTR(R2,"l")
        
        ;""<B><FONT style=""BACKGROUND-COLOR:"_COLOR_""">"_$P(TMGRESULT," ",1)_"</B></FONT> "_$P(TMGRESULT," ",2,9999)
        QUIT RESULT
        ;
REPLWRAP(LINE)
        SET LINE=$$REPLSTR^TMGSTUT3(LINE,"+++","<B><FONT style=""BACKGROUND-COLOR:")
        SET LINE=$$REPLSTR^TMGSTUT3(LINE,"---",""">")
        SET LINE=$$REPLSTR^TMGSTUT3(LINE,"@@@","</B></FONT>")
        QUIT LINE 
        ;"
YELLOW()  ;"HTML COLOR
        QUIT "#ffff99"
        ;"
RED()     ;"HTML COLOR
        QUIT "#ff4d4d"
        ;"
WRPVITAL(VALUE,VITAL,PTAGE)  ;"This function will take a vital and wrap in color if abnormal
        NEW TMGRESULT SET TMGRESULT=VALUE
        NEW COLOR SET COLOR=""
        NEW VALUE1 SET VALUE1=$P(VALUE," ",1)
        IF VITAL="T" DO
        . IF VALUE1>100 SET COLOR=$$YELLOW()
        . IF VALUE1>100.4 SET COLOR=$$RED()
        ELSE  IF VITAL="BP" DO
        . NEW BPGOAL SET BPGOAL=$SELECT(PTAGE<60:"140/90",1:"150/90")
        . NEW SYSGOAL SET SYSGOAL=$PIECE(BPGOAL,"/",1)
        . NEW DIASGOAL SET DIASGOAL=$PIECE(BPGOAL,"/",2)
        . NEW SYS,DIA SET SYS=$P(VALUE1,"/",1),DIA=$P(VALUE1,"/",2)
        . IF (SYS>SYSGOAL)!(SYS<100)!(DIA>DIASGOAL)!(DIA<60) SET COLOR=$$YELLOW()
        . IF (SYS>(SYSGOAL+20))!(DIA>(DIASGOAL+20)) SET COLOR=$$RED()
        ELSE  IF VITAL="Pulse" DO
        . NEW PULSE SET PULSE=+$P(VALUE," ",1)
        . IF (VALUE1>100)!(VALUE1<60) SET COLOR=$$YELLOW()
        . IF (VALUE1>120)!(VALUE1<50) SET COLOR=$$RED()
        ELSE  IF VITAL="POX" DO
        . IF VALUE1<94 SET COLOR=$$YELLOW()
        . IF VALUE1<90 SET COLOR=$$RED()
        ELSE  IF VITAL="BMI" DO
        . IF VALUE1<18.5 SET COLOR=$$YELLOW()
        IF COLOR'="" DO
        . ;"NEW RESULT SET RESULT="<B><FONT style=""BACKGROUND-COLOR:"_COLOR_""">"_$P(TMGRESULT," ",1)_"</B></FONT> "_$P(TMGRESULT," ",2,9999)
        . NEW RESULT SET RESULT="+++"_COLOR_"---"_$P(TMGRESULT," ",1)_"@@@ "_$P(TMGRESULT," ",2,9999)
        . SET TMGRESULT=RESULT
        QUIT TMGRESULT
        ;"
GETLMAMO(DFN) ;"Return date of last mammogram
        NEW TMGRESULT SET TMGRESULT=$$GETTABLN^TMGPXR02(DFN,"HEALTH FACTORS","Mammogram")
        IF TMGRESULT="" SET TMGRESULT="NONE REPORTED" GOTO GMAMDN
        NEW DATES SET DATES=$PIECE(TMGRESULT,"Mammogram =",2)_" [HF]"
        IF DATES=" [HF]" SET DATES=$PIECE(TMGRESULT,"Mammogram :",2)_" [T]"
        IF DATES=" [T]" SET DATES="DATES NOT FOUND. DATA IS: "_TMGRESULT
        SET TMGRESULT=DATES        
        NEW GRPARR,NEWARR
        DO GETHFGRP(DFN,802,.GRPARR)
        IF $DATA(GRPARR) DO
        . NEW NAME,DATE
        . SET NAME=""
        . FOR  SET NAME=$ORDER(GRPARR(NAME)) QUIT:NAME=""  DO
        . . SET DATE=0
        . . FOR  SET DATE=$ORDER(GRPARR(NAME,DATE)) QUIT:DATE'>0  DO
        . . . SET NEWARR(DATE,NAME)=""
        . SET DATE=$ORDER(NEWARR(9999999),-1)
        . SET NAME=$ORDER(NEWARR(DATE,""))
        . NEW Y
        . SET Y=DATE X ^DD("DD")
        . SET TMGRESULT=TMGRESULT_$C(13,10)_"F/U HF DATA: "_NAME_" on "_Y
        ;"
        ;"GET NOTE TITLES
        NEW MAMMOARRAY,NOTEDATE,COUNT
        SET NOTEDATE=9999999,COUNT=0
        DO TIUDATES^TMGPXR01(DFN,"MAMMOGRAM REPORT",.MAMMOARRAY)
        IF $DATA(MAMMOARRAY) SET TMGRESULT=TMGRESULT_$C(13,10)_"MAMMOGRAM NOTE DATES : "
        FOR  SET NOTEDATE=$ORDER(MAMMOARRAY(NOTEDATE),-1) QUIT:(NOTEDATE'>0)!(COUNT>3)  DO
        . NEW Y
        . SET Y=NOTEDATE
        . X ^DD("DD")
        . SET TMGRESULT=TMGRESULT_$PIECE(Y,"@",1)_" "
        ;"GET RAD PROCEDURE DATES
        ;"CHECK ^RADPT( FOR IENs 708 AND 792
        ;"Pull data from bone density radiology studies
        NEW RADFN SET RADFN=+$$ENSRADFN^TMGRAU01(.DFN)
        NEW TESTDATES SET TESTDATES=""
        NEW TESTCOUNT SET TESTCOUNT=0
        IF RADFN>0 DO
        . NEW RADDT SET RADDT=0
        . FOR  SET RADDT=$O(^RADPT(RADFN,"DT",RADDT)) QUIT:(RADDT'>0)!(TESTCOUNT>3)  DO
        . . NEW IEN70D03 SET IEN70D03=0
        . . FOR  SET IEN70D03=$O(^RADPT(RADFN,"DT",RADDT,"P",IEN70D03)) QUIT:IEN70D03'>0  DO
        . . . NEW ZN SET ZN=$GET(^RADPT(RADFN,"DT",RADDT,"P",IEN70D03,0))
        . . . NEW PROCIEN SET PROCIEN=$PIECE(ZN,"^",2)
        . . . NEW PROCNAME SET PROCNAME=$P($G(^RAMIS(71,PROCIEN,0)),"^",1)
        . . . ;"IF (PROCIEN=708)!(PROCIEN=792) DO  ;"MAMMO DIAGNOSTIC BILATERAL INCL CAD or MAMMO SCREEN BIL W IMP W TOMO INCL CAD
        . . . IF PROCNAME["MAMMO" DO
        . . . . SET TESTCOUNT=TESTCOUNT+1
        . . . . NEW FMDATE SET FMDATE=9999999-$P(RADDT,".",1)
        . . . . IF TESTDATES'="" SET TESTDATES=TESTDATES_", "
        . . . . SET TESTDATES=TESTDATES_$$EXTDATE^TMGDATE(FMDATE,1)
        IF TESTDATES'="" SET TMGRESULT=TMGRESULT_$C(13,10)_"  Mammogram done on: "_TESTDATES_" [RAD]"
GMAMDN  QUIT TMGRESULT
        ;
GETLCOLN(DFN) ;"Return date of last colonoscopy
        NEW TMGRESULT SET TMGRESULT=$$GETTABLN^TMGPXR02(DFN,"HEALTH FACTORS","Colonoscopy")
        IF TMGRESULT="" SET TMGRESULT="Last Colonoscopy: NONE REPORTED" GOTO GCOLDN
        NEW DATES SET DATES=$PIECE(TMGRESULT,"Colonoscopy =",2)_" [HF]"
        IF DATES=" [HF]" SET DATES=$PIECE(TMGRESULT,"Colonoscopy :",2)_" [T]"
        IF DATES=" [T]" SET DATES="DATES NOT FOUND. DATA IS: "_TMGRESULT
        SET TMGRESULT="Last Colonoscopy: "_DATES        
        ;"Get FU dates
GCOLDN  
        NEW GRPARR,NEWARR
        DO GETHFGRP(DFN,803,.GRPARR)
        IF $DATA(GRPARR) DO
        . NEW NAME,DATE
        . SET NAME=""
        . FOR  SET NAME=$ORDER(GRPARR(NAME)) QUIT:NAME=""  DO
        . . SET DATE=0
        . . FOR  SET DATE=$ORDER(GRPARR(NAME,DATE)) QUIT:DATE'>0  DO
        . . . SET NEWARR(DATE,NAME)=""
        . SET DATE=$ORDER(NEWARR(9999999),-1)
        . SET NAME=$ORDER(NEWARR(DATE,""))
        . NEW Y
        . SET Y=DATE X ^DD("DD")
        . SET TMGRESULT=TMGRESULT_$C(13,10)_"F/U HF DATA: "_NAME_" (Relative to most recent colonoscopy)"
        ;"Get FOBT dates
        DO GETOCCLT(.TMGRESULT,DFN)        
        DO FOBTNOTE(.TMGRESULT,DFN)
        ;"Get cologuard note
        NEW COLOARRAY,NOTEDATE,COUNT
        SET NOTEDATE=9999999,COUNT=0
        DO TIUDATES^TMGPXR01(DFN,"COLOGUARD RESULT (IMAGE)",.COLOARRAY)
        IF $DATA(COLOARRAY) SET TMGRESULT=TMGRESULT_$C(13,10)_"COLOGUARD NOTE DATES : "
        FOR  SET NOTEDATE=$ORDER(COLOARRAY(NOTEDATE),-1) QUIT:(NOTEDATE'>0)!(COUNT>3)  DO
        . NEW Y
        . SET Y=NOTEDATE
        . X ^DD("DD")
        . SET TMGRESULT=TMGRESULT_$PIECE(Y,"@",1)_" "
        ;"
        ;"If over 75, include message
        NEW PTAGE SET PTAGE=$$PTAGE^TMGTIUO3(DFN,"")  ;"
        IF PTAGE>75 DO
        . SET TMGRESULT=TMGRESULT_$C(13,10)_$C(13,10)
        . SET TMGRESULT=TMGRESULT_"** NOTE: THIS PATIENT IS OVER 75. CONSIDER DISCONTINUING. .**"
        QUIT TMGRESULT
        ;
GETOCCLT(TMGRESULT,DFN) ;"Return dates of last iFOBTs and FOBT
        ;"NEW RESULT SET RESULT=$$GETTABLN^TMGPXR02(DFN,"HEALTH FACTORS","FOBT Negative HF")
        ;"IF RESULT'="" SET TMGRESULT=TMGRESULT_$C(13,10)_RESULT        
        ;"SET RESULT=$$GETTABLN^TMGPXR02(DFN,"HEALTH FACTORS","FOBT Positive HF")
        ;"IF RESULT'="" SET TMGRESULT=TMGRESULT_$C(13,10)_RESULT
        ;"SET RESULT=$$GETTABLN^TMGPXR02(DFN,"HEALTH FACTORS","iFOBT Negative HF")
        ;"IF RESULT'="" SET TMGRESULT=TMGRESULT_$C(13,10)_RESULT
        ;"SET RESULT=$$GETTABLN^TMGPXR02(DFN,"HEALTH FACTORS","iFOBT Positive HF")
        ;"IF RESULT'="" SET TMGRESULT=TMGRESULT_$C(13,10)_RESULT
        NEW RESULT,RESULTARR
        SET RESULT=$$GETTABL1^TMGTIUO6(DFN,"[STUDIES]",.RESULTARR)
        SET RESULT=$GET(RESULTARR("KEY-VALUE","IFOBT"))   ;"_" [T]"
        IF RESULT'="" SET TMGRESULT=TMGRESULT_$C(13,10)_"iFOBT= "_RESULT_" [T]"_$C(13,10)_"NOTE: iFOBT satisfies on a calendar year basis only."
        QUIT
        ;"
FOBTNOTE(TMGRESULT,DFN)  ;"Return a note if FOBT was done this year w/ result
        NEW THISRESULT SET THISRESULT=""
        NEW HFARRAY,HFIEN SET HFIEN=0
        SET HFARRAY(784)="iFOBT was Negative on:"
        SET HFARRAY(787)="iFOBT was POSITIVE on:"
        SET HFARRAY(783)="FOBT was Negative on:"
        SET HFARRAY(786)="FOBT was POSITIVE on:"
        SET HFARRAY(2627)="Cologuard was POSITIVE on:"
        SET HFARRAY(2628)="Cologuard was Negative on:"
        NEW CUTOFFDT SET CUTOFFDT=$$FIRSTYR^TMGDATE
        FOR  SET HFIEN=$O(HFARRAY(HFIEN)) QUIT:HFIEN'>0  DO        
        . NEW DATE SET DATE=9999999
        . NEW DONE SET DONE=0
        . FOR  SET DATE=$ORDER(^AUPNVHF("AA",DFN,HFIEN,DATE),-1) QUIT:(DATE'>0)!(DONE=1)  DO        
        . . NEW FMDATE SET FMDATE=9999999-DATE
        . . IF FMDATE<CUTOFFDT DO  QUIT
        . . . SET DONE=1
        . . IF THISRESULT'="" SET THISRESULT=THISRESULT_","
        . . SET THISRESULT=THISRESULT_$G(HFARRAY(HFIEN))_$$EXTDATE^TMGDATE(FMDATE,1)_" [HF]"
        ;"
        NEW LRDFN SET LRDFN=+$GET(^DPT(DFN,"LR"))
        IF LRDFN'>0 QUIT TMGRESULT
        NEW LABNUM1,LABNUM2
        SET LABNUM1=69489,LABNUM2=5602
        NEW DATEIDX SET DATEIDX=0
        FOR  SET DATEIDX=$ORDER(^LR(LRDFN,"CH",DATEIDX)) QUIT:(DATEIDX'>0)  DO
        . IF ('$DATA(^LR(LRDFN,"CH",DATEIDX,LABNUM1)))&('$DATA(^LR(LRDFN,"CH",DATEIDX,LABNUM2))) QUIT
        . NEW TEMPDATE SET TEMPDATE=9999999-DATEIDX
        . IF TEMPDATE<CUTOFFDT QUIT
        . NEW TEMP
        . DO LABTOARR(.TEMP,LRDFN,DATEIDX)
        . IF $D(TEMP(LABNUM1)) DO
        . . IF THISRESULT'="" SET THISRESULT=THISRESULT_","
        . . SET THISRESULT=THISRESULT_"iFOBT was "_$G(TEMP(LABNUM1))_" on "_$G(TEMP(0))_" [LAB]"
        . IF $D(TEMP(LABNUM2)) DO
        . . IF THISRESULT'="" SET THISRESULT=THISRESULT_","
        . . SET THISRESULT=THISRESULT_"iFOBT was "_$G(TEMP(LABNUM2))_" on "_$G(TEMP(0))_" [LAB]"
        ;"
        IF THISRESULT'="" SET TMGRESULT=TMGRESULT_$C(13,10)_"****NOTE: "_THISRESULT       
        QUIT
        ;"
GETLADIR(DFN) ;"Return date of last advance directives        
        NEW TMGRESULT,RESULTARR
        SET TMGRESULT=$$GETTABLN^TMGPXR02(DFN,"HEALTH FACTORS","Advance Directives")
        IF TMGRESULT="" DO  GOTO GADDN
        . SET TMGRESULT=$$GETTABL1^TMGTIUO6(DFN,"[STUDIES]",.RESULTARR)
        . SET TMGRESULT=$GET(RESULTARR("KEY-VALUE","ADVANCE DIRECTIVES"))_" [T]"
        . IF TMGRESULT=" [T]" SET TMGRESULT="NO DATA FOUND"      
        NEW DATES SET DATES=$PIECE(TMGRESULT,"Advance Directives =",2)_" [HF]"
        IF DATES=" [HF]" SET DATES=$PIECE(TMGRESULT,"Advance Directives :",2)
        IF DATES="" SET DATES="DATES NOT FOUND. DATA IS: "_TMGRESULT
        ELSE  SET DATES=DATES_" "
        SET TMGRESULT=DATES
GADDN   QUIT TMGRESULT
        ;
GETLBONE(DFN) ;"Return date of bone density
        NEW PREFIX SET PREFIX=""
        NEW TMGRESULT SET TMGRESULT=$$GETTABLN^TMGPXR02(DFN,"HEALTH FACTORS","Bone Density")
        IF TMGRESULT="" DO  GOTO GBDDN
        . NEW RESULTARR 
        . SET TMGRESULT=$$GETTABL1^TMGTIUO6(DFN,"[STUDIES]",.RESULTARR)
        . SET TMGRESULT=$GET(RESULTARR("KEY-VALUE","BONE DENSITY"))_" [T]"
        . SET PREFIX="STUDIES: "
        . IF TMGRESULT="" SET TMGRESULT="NONE REPORTED"
        NEW DATES SET DATES=$PIECE(TMGRESULT,"Bone Density =",2)
        IF DATES="" SET DATES=$PIECE(TMGRESULT,"Bone Density :",2)
        IF DATES="" SET DATES="DATES NOT FOUND. DATA IS: "_TMGRESULT
        ELSE  SET DATES=PREFIX_DATES_" [HF]"
        SET TMGRESULT=DATES        
GBDDN   
        ;"Pull data from bone density radiology studies
        NEW RADFN SET RADFN=+$$ENSRADFN^TMGRAU01(.DFN)
        NEW TESTDATES SET TESTDATES=""
        IF RADFN>0 DO
        . NEW RADDT SET RADDT=0
        . FOR  SET RADDT=$O(^RADPT(RADFN,"DT",RADDT)) QUIT:RADDT'>0  DO
        . . NEW IEN70D03 SET IEN70D03=0
        . . FOR  SET IEN70D03=$O(^RADPT(RADFN,"DT",RADDT,"P",IEN70D03)) QUIT:IEN70D03'>0  DO
        . . . NEW ZN SET ZN=$GET(^RADPT(RADFN,"DT",RADDT,"P",IEN70D03,0))
        . . . NEW PROCIEN SET PROCIEN=$PIECE(ZN,"^",2)
        . . . IF PROCIEN=536 DO  ;"BONE DENSITY
        . . . . NEW FMDATE SET FMDATE=9999999-$P(RADDT,".",1)
        . . . . IF TESTDATES'="" SET TESTDATES=TESTDATES_", "
        . . . . SET TESTDATES=TESTDATES_$$EXTDATE^TMGDATE(FMDATE,1)
        IF TESTDATES'="" SET TMGRESULT=TMGRESULT_$C(13,10)_"  Bone Density studies done on: "_TESTDATES
        QUIT TMGRESULT
        ;
GETLTSH(DFN)  ;"Return last lab data for TSH
        NEW TMGRESULT,RESULTARR
        SET TMGRESULT=$$GETTABL1^TMGTIUO6(DFN,"[STUDIES]",.RESULTARR)
        SET TMGRESULT=$GET(RESULTARR("KEY-VALUE","TSH"))_" [T]"
        IF TMGRESULT=" [T]" SET TMGRESULT="NO DATA FOUND"
        QUIT TMGRESULT
        ;"
GETLB12(DFN)  ;"Return last lab data for Vitamin B-12
        NEW TMGRESULT,RESULTARR
        SET TMGRESULT=$$GETTABL1^TMGTIUO6(DFN,"[STUDIES]",.RESULTARR)
        SET TMGRESULT=$GET(RESULTARR("KEY-VALUE","VIT-B12"))_" [T]"
        IF TMGRESULT=" [T]" SET TMGRESULT="NO DATA FOUND"
        QUIT TMGRESULT
        ;"
GETLTOBA(DFN)  ;"Return last tobacco status
        NEW RESULTS,RESULTARR,TMGRESULT
        SET RESULTS=$$GETTABL1^TMGTIUO6(DFN,"[SOCIAL HX]",.RESULTARR)
        SET TMGRESULT="Social Hx: "_$GET(RESULTARR("KEY-VALUE","TOBACCO"))_"<br>"
        KILL RESULTARR
        DO GETHFGRP(DFN,765,.RESULTARR)
        NEW NAME,DATE,COUNTER,EDATE,Y
        SET COUNTER=0
        SET NAME=""
        FOR  SET NAME=$ORDER(RESULTARR(NAME)) QUIT:NAME=""  DO
        . SET DATE=0
        . SET COUNTER=COUNTER+1
        . SET TMGRESULT=TMGRESULT_"     "_NAME_": "
        . FOR  SET DATE=$ORDER(RESULTARR(NAME,DATE)) QUIT:DATE'>0  DO
        . . SET Y=DATE
        . . X ^DD("DD")
        . . SET TMGRESULT=TMGRESULT_" "_Y
        . SET TMGRESULT=TMGRESULT_"<br>"
        QUIT TMGRESULT
        ;"
GETLEYEE(TMGDFN) ;"Return last diabetic eye exam
        NEW RESULTS,RESULTARR,TMGRESULT,TEMPLINE
        SET TMGRESULT=""
        SET RESULTS=$$GETTABL1^TMGTIUO6(TMGDFN,"[DIABETIC STUDIES]",.RESULTARR)
        SET TEMPLINE=$GET(RESULTARR("KEY-VALUE","DIABETIC EYE EXAM"))
        IF TEMPLINE'="" SET TMGRESULT="PRIOR : "_TEMPLINE_"<br>"
        SET TEMPLINE=$GET(RESULTARR("KEY-VALUE","EYE EXAM"))
        IF TEMPLINE'="" SET TMGRESULT=TMGRESULT_"HF = "_TEMPLINE_"<br>"
        NEW EYEEARRAY,NOTEDATE,COUNT
        SET NOTEDATE=9999999,COUNT=0
        DO TIUDATES^TMGPXR01(TMGDFN,"OPHTHO / OPTO / EYE CONSULTANT NOTE (IMAGE)",.EYEEARRAY)
        IF $DATA(EYEEARRAY) SET TMGRESULT=TMGRESULT_"CONSULT NOTE DATES : "
        FOR  SET NOTEDATE=$ORDER(EYEEARRAY(NOTEDATE),-1) QUIT:(NOTEDATE'>0)!(COUNT>3)  DO                
        . NEW Y 
        . SET Y=NOTEDATE
        . X ^DD("DD")
        . SET TMGRESULT=TMGRESULT_$PIECE(Y,"@",1)_" "
        IF TMGRESULT="" SET TMGRESULT="NONE FOUND"
        SET TMGRESULT="<br>"_TMGRESULT_"<br>"
        QUIT TMGRESULT
        ;"
GETLFTEX(TMGDFN) ;"Return last diabetic foot exam
        NEW RESULTS,RESULTARR,TMGRESULT,TEMPLINE
        SET TMGRESULT=""
        SET RESULTS=$$GETTABL1^TMGTIUO6(TMGDFN,"[DIABETIC STUDIES]",.RESULTARR)
        SET TEMPLINE=$GET(RESULTARR("KEY-VALUE","DIABETIC FOOT EXAM"))
        IF TEMPLINE'="" SET TMGRESULT="PRIOR : "_TEMPLINE_"<br>"
        SET TEMPLINE=$GET(RESULTARR("KEY-VALUE","FOOT EXAM"))
        IF TEMPLINE'="" SET TMGRESULT=TMGRESULT_"HF = "_TEMPLINE_"<br>"
        IF TMGRESULT="" SET TMGRESULT="NONE FOUND"
        SET TMGRESULT="<br>"_TMGRESULT_"<br>"
        QUIT TMGRESULT
        ;"
GETLGLAS(DFN)  ;"Return last glaucoma screening
        NEW PREFIX SET PREFIX=""
        NEW TMGRESULT SET TMGRESULT=$$GETTABLN^TMGPXR02(DFN,"HEALTH FACTORS","Glaucoma Screening")
        IF TMGRESULT="" DO  GOTO GGSDN
        . NEW RESULTARR
        . SET TMGRESULT=$$GETTABL1^TMGTIUO6(DFN,"[STUDIES]",.RESULTARR)
        . SET TMGRESULT=$GET(RESULTARR("KEY-VALUE","Glaucoma Eye Exam"))_" [T]"
        . SET PREFIX="STUDIES: "
        . IF TMGRESULT=" [T]" SET TMGRESULT="NONE REPORTED"
        NEW DATES SET DATES=$PIECE(TMGRESULT,"Glaucoma Screening =",2)_" [HF]"
        IF DATES=" [HF]" SET DATES=$PIECE(TMGRESULT,"Glaucoma Eye Exam :",2)_" [T]"
        IF DATES=" [T]" SET DATES="DATES NOT FOUND. DATA IS: "_TMGRESULT
        ELSE  SET DATES=PREFIX_DATES_" [HF]"
        SET TMGRESULT=DATES
        NEW EYEEARRAY,NOTEDATE,COUNT
        SET NOTEDATE=9999999,COUNT=0
        DO TIUDATES^TMGPXR01(DFN,"OPHTHO / OPTO / EYE CONSULTANT NOTE (IMAGE)",.EYEEARRAY)
        IF $DATA(EYEEARRAY) SET TMGRESULT=TMGRESULT_"<br>CONSULT NOTE DATES : "
        FOR  SET NOTEDATE=$ORDER(EYEEARRAY(NOTEDATE),-1) QUIT:(NOTEDATE'>0)!(COUNT>3)  DO
        . NEW Y
        . SET Y=NOTEDATE
        . X ^DD("DD")
        . SET TMGRESULT=TMGRESULT_$PIECE(Y,"@",1)_" "
GGSDN   QUIT TMGRESULT
        ;"        
GETLPAP(DFN)  ;"Return
        NEW TMGRESULT SET TMGRESULT="TEST STUFF HERE"
        QUIT TMGRESULT
        ;"
ADGIVEN(DFN)  ;"Return the health factor date for when the last CP papers were given/declined
        NEW TMGRESULT SET TMGRESULT=""
        SET DFN=$GET(DFN) IF DFN'>0 QUIT TMGRESULT
        NEW HFARRAY
        SET HFARRAY("TMG ADV CP PAPERS GIVEN")=""
        SET HFARRAY("TMG ADV CP DISCUSSED - NOT DESIRED")=""
        SET HFARRAY("TMG ADV SURROGATE DISCUSSED - NOT NAMED")=""
        SET HFARRAY("TMG ADV SURROGATE DOCUMENTED")=""
        NEW HFNAME SET HFNAME=""
        FOR  SET HFNAME=$ORDER(HFARRAY(HFNAME)) QUIT:HFNAME=""  DO
        . NEW HFIEN SET HFIEN=$ORDER(^AUTTHF("B",HFNAME,0))
        . IF HFIEN'>0 QUIT
        . NEW DATE SET DATE=9999999
        . NEW VHFIEN,COMMENT
        . SET COMMENT=""
        . NEW LATEST SET LATEST=0
        . FOR  SET DATE=$ORDER(^AUPNVHF("AA",DFN,HFIEN,DATE),-1) QUIT:DATE'>0  DO
        . . SET LATEST=DATE
        . . SET COMMENT=$$GETHFCOM(+$ORDER(^AUPNVHF("AA",DFN,HFIEN,DATE,0)))
        . . IF COMMENT'="" SET COMMENT=$C(13,10)_"         ("_COMMENT_")"
        . IF LATEST'=0 DO
        . . SET LATEST=9999999-LATEST
        . . ;"NEW Y SET Y=LATEST D DD^%DT
        . . SET LATEST=$E(LATEST,4,5)_"/"_$E(LATEST,6,7)_"/"_($E(LATEST,1,3)+1700) 
        . . IF TMGRESULT="" DO
        . . . SET TMGRESULT=HFNAME_" "_LATEST_" "_COMMENT_" [HF]"
        . . ELSE  DO
        . . . SET TMGRESULT=TMGRESULT_$C(13,10)_HFNAME_" "_LATEST_" "_COMMENT_" [HF]"
        IF TMGRESULT="" SET TMGRESULT="ADV DIR, PAPERS GIVEN: NO HF FOUND."
        QUIT TMGRESULT
        ;"
GETLLAB(DFN,LABNUM,NUM,DTONLY)   ;"Return the last urine culture
        ;"ADDING DTONLY, IF 1 WILL ONLY RETURN THE DATE ONLY
        SET DTONLY=+$G(DTONLY)
        NEW TMGRESULT SET TMGRESULT=""
        SET DFN=$GET(DFN) IF DFN'>0 QUIT TMGRESULT
        NEW LRDFN SET LRDFN=+$GET(^DPT(DFN,"LR"))
        IF LRDFN'>0 QUIT TMGRESULT
        SET LABNUM=+$GET(LABNUM)
        IF LABNUM'>0 QUIT TMGRESULT
        SET NUM=+$GET(NUM)
        IF NUM'>0 SET NUM=1
        NEW DATEIDX SET DATEIDX=0
        NEW CURCOUNT SET CURCOUNT=0
        NEW FOUND SET FOUND=0
        NEW ARR,TOTARR
        FOR  SET DATEIDX=$ORDER(^LR(LRDFN,"CH",DATEIDX)) QUIT:(DATEIDX'>0)!(FOUND=1)  DO
        . IF ('$DATA(^LR(LRDFN,"CH",DATEIDX,LABNUM)))&('$DATA(^LR(LRDFN,"CH",DATEIDX,69038))) QUIT  ;"ADDED 69038 FOR NOW 3/21/21
        . SET CURCOUNT=CURCOUNT+1
        . NEW TEMP
        . DO LABTOARR(.TEMP,LRDFN,DATEIDX)
        . IF CURCOUNT=NUM DO
        . . ;"SET FOUND=1
        . . MERGE ARR=TEMP
        . . ;"DO LABTOARR(.ARR,LRDFN,DATEIDX)
        . MERGE TOTARR(DATEIDX)=TEMP
        ;"zwr ARR
        IF $DATA(ARR) DO
        . IF DTONLY=1 DO
        . . NEW LASTDT SET LASTDT=0
        . . NEW COUNT SET COUNT=0
        . . SET DATEIDX=0
        . . FOR  SET DATEIDX=$O(TOTARR(DATEIDX)) QUIT:(DATEIDX'>0)!(COUNT=NUM)  DO
        . . . SET COUNT=COUNT+1
        . . . IF $G(TOTARR(DATEIDX,0))=LASTDT QUIT
        . . . SET LASTDT=$G(TOTARR(DATEIDX,0))
        . . . IF TMGRESULT="" SET TMGRESULT=$G(TOTARR(DATEIDX,0))
        . . . ELSE  SET TMGRESULT=TMGRESULT_", "_$G(TOTARR(DATEIDX,0))
        . ELSE  DO
        . . DO LARR2TBL(.TMGRESULT,.ARR)
        ;"write TMGRESULT
        QUIT TMGRESULT
        ;"
GETPTICK(DFN)  ;"Return
        NEW TMGRESULT SET TMGRESULT=""
        NEW TICKLIEN,ARRAY SET TICKLIEN=0
	    NEW DATA,TEMPARR,TIU,STATUS,USER,DUE,IDX,TIUDATE,TIUNAME
	    FOR  SET TICKLIEN=$O(^TMG(22705.5,"B",DFN,TICKLIEN)) QUIT:TICKLIEN'>0  DO
	    . NEW ZN SET ZN=$G(^TMG(22705.5,TICKLIEN,0))
	    . SET TIU=$P(ZN,"^",4),STATUS=$P(ZN,"^",3),USER=$P(ZN,"^",5),DUE=$P(ZN,"^",2)
	    . IF +$G(TIU)'>0 QUIT
	    . SET TIUDATE=$$EXTDATE^TMGDATE($P($G(^TIU(8925,TIU,0)),"^",7))
	    . SET TIUNAME=$P($G(^TIU(8925.1,$P($G(^TIU(8925,TIU,0)),"^",1),0)),"^",1)
	    . NEW OVERDUE SET OVERDUE=$$OVERDUE^TMGRPT2(STATUS,DUE)
	    . NEW MESSAGE SET MESSAGE=$$FLMSG^TMGTICK2(TICKLIEN)
	    . IF MESSAGE="" SET MESSAGE="----NO MESSAGE FOUND----"
	    . IF STATUS'="S" QUIT
	    . IF TMGRESULT="" DO
	    . . SET TMGRESULT="---- PENDING TICKLERS ----"
	    . IF OVERDUE=1 SET OVERDUE="<B><FONT style=""BACKGROUND-COLOR:"_$$RED()_""">OVERDUE</B></FONT> " ELSE  SET OVERDUE=""
	    . SET TMGRESULT=TMGRESULT_"<BR>"_OVERDUE_"Due on: "_$P($$EXTDATE^TMGDATE(DUE),"@",1)_"; Set on: "_$P(TIUDATE,"@",1)_"; "_MESSAGE
	    . ;"SET TEMPARR(DUE,TIU,OVERDUE)=$$TSTATUS(STATUS)_"^"_$P($G(^VA(200,USER,0)),"^",1)_"^"_MESSAGE_"^"_TIUDATE_"^"_TIUNAME
	    ;SET DUE=9999999,IDX=1
	    ;FOR  SET DUE=$O(TEMPARR(DUE),-1) QUIT:DUE'>0  DO
	    ;. SET TIU=0
	    ;. FOR  SET TIU=$O(TEMPARR(DUE,TIU)) QUIT:TIU'>0  DO
	    ;. . NEW OVERDUE SET OVERDUE=$O(TEMPARR(DUE,TIU,-1))
	    ;. . IF OVERDUE>0 DO
	    ;. . . SET DATA(IDX)="<b>"_$$EXTDATE^TMGDATE(DUE)_"^<b>"_$G(TEMPARR(DUE,TIU,OVERDUE))_"</b>"
	    ;. . ELSE  DO
	    ;. . . SET DATA(IDX)=$$EXTDATE^TMGDATE(DUE)_"^"_$G(TEMPARR(DUE,TIU,OVERDUE))
	    ;. . SET IDX=IDX+1
        QUIT TMGRESULT
        ;"
LABTOARR(RESULT,LRDFN,DATEIDX)  ;"Return lab results for a given date in array
        NEW DATE SET DATE=9999999-$PIECE(DATEIDX,".",1)
        SET DATE=$E(DATE,4,5)_"/"_$E(DATE,6,7)_"/"_($E(DATE,1,3)+1700)
        SET RESULT(0)=DATE
        NEW LABIDX SET LABIDX=1
        FOR  SET LABIDX=$ORDER(^LR(LRDFN,"CH",DATEIDX,LABIDX)) QUIT:LABIDX'>0  DO
        . SET RESULT(LABIDX)=$PIECE($GET(^LR(LRDFN,"CH",DATEIDX,LABIDX)),"^",1)
        NEW COMIDX SET COMIDX=0
        FOR  SET COMIDX=$ORDER(^LR(LRDFN,"CH",DATEIDX,1,COMIDX)) QUIT:COMIDX'>0  DO
        . SET RESULT("COMMENT",COMIDX)=$GET(^LR(LRDFN,"CH",DATEIDX,1,COMIDX,0))
        QUIT
        ;"
LARR2TBL(OUT,ARR)   ;"Take lab results in an array and convert to HTML table
        NEW IDX SET IDX=0
        NEW CELLCOLOR,CUR
        SET CUR=0
        SET CELLCOLOR(0)="#f5f5dc"
        SET CELLCOLOR(1)="#c4e3ed"
        SET OUT="<HTML><HEAD><font face='Consolas'><table border=1px><tr bgcolor=#d3d3d3><th colspan=""2"">LAB RESULTS FOR: "_$GET(ARR(0))_"</th></tr>"
        FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:+IDX'>0  DO
        . NEW LABIDX SET LABIDX=+$ORDER(^LAB(60,"C","CH;"_IDX_";1",0))
        . IF LABIDX'>0 QUIT
        . NEW LABNAME SET LABNAME=$PIECE($GET(^LAB(60,LABIDX,0)),"^",1)
        . SET OUT=OUT_"<tr bgcolor="_$GET(CELLCOLOR(CUR))_"><td>"_LABNAME_"</td><td>"_$GET(ARR(IDX))_"</td></tr>"
        . IF CUR=0 DO
        . . SET CUR=1
        . ELSE  DO
        . . SET CUR=0
        NEW COMIDX SET COMIDX=0
        SET OUT=OUT_"<tr bgcolor=#f0ffff><td colspan=""2"">"
        FOR  SET COMIDX=$ORDER(ARR("COMMENT",COMIDX)) QUIT:COMIDX'>0  DO
        . SET OUT=OUT_$GET(ARR("COMMENT",COMIDX))_"<br>"
        SET OUT=OUT_"</td></tr></table></font></head></html>"
        QUIT
        ;"
ALLHFTBL(DFN)  ;"Return an HTML table containing all health factors
        NEW ARR
        DO TMGHFACT^TMGRPT2("ARR",DFN,0,0,0)
        NEW TMGRESULT SET TMGRESULT=""
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:IDX'>0  DO
        . SET TMGRESULT=TMGRESULT_$GET(ARR(IDX))
        QUIT TMGRESULT
        ;"  
GETHFGRP(DFN,HFGROUPIEN,TMGRESULTARR)  ;"Return health factors for a patient by a given hf group
        ;"Purpose:
        ;"Input:
        ;"Output: 
        NEW HFIEN,DATE,HFNAME
        SET HFIEN=0
        FOR  SET HFIEN=$ORDER(^AUTTHF("AC",HFGROUPIEN,HFIEN)) QUIT:HFIEN'>0  DO
        . SET DATE=0
        . SET HFNAME=$PIECE($GET(^AUTTHF(HFIEN,0)),"^",1)
        . FOR  SET DATE=$ORDER(^AUPNVHF("AA",DFN,HFIEN,DATE)) QUIT:DATE'>0  DO
        . . SET TMGRESULTARR(HFNAME,9999999-DATE)=""
        QUIT
        ;"
GETHFCOM(VHFIEN)  ;"RETURN A VISIT'S HEALTH FACTOR COMMENT OR BLANK
        QUIT $GET(^AUPNVHF(VHFIEN,811))
        ;"
