TMGRX003 ;TMG/kst/Patient medication code; 08/25/17
       ;;1.0;TMG-LIB;**1**;08/25/17
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 08/25/17  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"EXTERNAL(ARR)  -- TURN MEDICATION ARRAY INTO OUTPUT STRING  
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"GETBRAND(IEN22733)  -- GET BRANDNAME
 ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"=======================================================================
 ;"Uses:  
 ;"=======================================================================
 ;        
EXTERNAL(ARR)  ;"TURN MEDICATION ARRAY INTO OUTPUT STRING  
  ;"Input: ARR -- PASS BY REFERENCE.  
  ;"         This is array as output by PARSELN^TMGRX001.  Format:
  ;"         ARR("ORIG")=The original unparsed line.  
  ;"         ARR("IEN22733")=<IEN 22733>
  ;"         ARR("PREFACE")= e.g. HOLD, HOLDING, OFF
  ;"         ARR("OTC")=1  <-- if specified as OTC
  ;"         ARR("WORKING")=The input line, with preface removed and arrows fixed  
  ;"         ARR("MEDICATION","INPUT NAME")=<NAME OF RX>  <-- last Rx name encountered on line (input name)
  ;"         ARR("MEDICATION","GENERIC")=<NAME OF RX> <-- Generic name as found on input line (if provided)  
  ;"         ARR("MEDICATION","GENERIC","DATABASE") <-- Generic name based on IEN22733:.01 field
  ;"         ARR("MEDICATION","GENERIC ALIAS")=SOURCE <-- Generic alias name, as found on input line (if provided)
  ;"         ARR("MEDICATION","GENERIC ALIAS","PREFERRED")=<PREFERRED GENERIC ALIAS> from database  
  ;"         ARR("MEDICATION","GENERIC ABBRV")=SOURCE <-- Generic abbreviation, as found on input line (if provided)
  ;"         ARR("MEDICATION","GENERIC ABBRV","PREFERRED")=<preferred generic abbreviation> from database  
  ;"         ARR("MEDICATION","BRAND")=<NAME OF RX> <-- Brand name, as found on input line (if provided)
  ;"         ARR("MEDICATION","BRAND","PREFERRED")=<Preferred brand name from database>  
  ;"         ARR("DOSE")=<DOSE>  <-- dose as found on input line (if provided)
  ;"         ARR("DOSE","DATABASE")=<registered dose strength for last IENS found> <-- if found.   
  ;"         ARR("DOSE","IENS",<IENS IN 22733.03>)=<FORM>^<Strength>^<Preferred Alias>  <-- may be multiple. 
  ;"         ARR("UNITS")=E.G. "MG"  <-- units as found on input line (if provided)
  ;"         ARR("UNITS","IEN50.607")=#  <-- this is UNITS IEN (if units provided)
  ;"         ARR("UNITS","DATABASE")=<units> from database  
  ;"         ARR("UNITS","PREFERRED")=<PREFERRED UNITS>-- if form determined
  ;"         ARR("FORM")=<FORM> <-- form as found on input line (if provided)
  ;"         ARR("FORM","SUBIEN")=<#>  <-- 
  ;"         ARR("FORM","DATABASE")=<FORM> from database
  ;"         ARR("MODIFIER")=<modifier> <-- modifier as found in input line (if provided)  
  ;"         ARR("MODIFIER","PREFERRED")=<PREFERRED MODIFIER, IN SAME GROUP AS MOD>  (if form determined)  
  ;"         ARR("SIG")="AS NEEDED"
  ;"         ARR("NOTE")=<any notes>  DELIMITED BY "<---" OR '>---" ...
  ;"RESULT: Returns string of external form of Rx. 
  NEW RESULT SET RESULT=""
  NEW FAILURE SET FAILURE=0
  NEW IEN22733 SET IEN22733=+$GET(ARR("IEN22733")) GOTO:IEN22733'>0 A2SER  
  NEW PRINTMODE SET PRINTMODE=$PIECE($GET(^TMG(22733,IEN22733,"1.5")),"^",3)
  IF PRINTMODE="" SET PRINTMODE="GE"
  NEW MODIFIERMODE SET MODIFIERMODE=$PIECE($GET(^TMG(22733,IEN22733,"1.5")),"^",5)
  NEW DOSEIENS SET DOSEIENS=$ORDER(ARR("DOSE","IENS",""))
  IF DOSEIENS'="",$ORDER(ARR("DOSE","IENS",DOSEIENS))'="" SET DOSEIENS=""  ;"multiple DOSE IENS's present
  IF DOSEIENS="" SET DOSEIENS=$$MOD2FRM^TMGRX004(.ARR)
  NEW FORMIEN SET FORMIEN=+$PIECE(DOSEIENS,",",2)
  NEW NODE1D1 SET NODE1D1=$GET(^TMG(22733,IEN22733,1.1))
  NEW NODE1D2 SET NODE1D2=$GET(^TMG(22733,IEN22733,1.2))
  DO   ;"MED NAME  (PARTS A, B)
  . NEW CONTROLLED SET CONTROLLED=($PIECE($GET(^TMG(22733,IEN22733,"1.5")),"^",2)="Y")
  . NEW PRTAMODE SET PRTAMODE=$PIECE(NODE1D1,"^",1)
  . NEW PRINTA SET PRINTA=$$GETRXNAM(.ARR,PRTAMODE)
  . NEW CAPA SET CAPA=$PIECE(NODE1D2,"^",1) 
  . IF CONTROLLED SET CAPA="UC"  ;"FORCE UPPER CASE
  . SET PRINTA=$$CAPBYMOD(PRINTA,CAPA)
  . IF PRINTA'="" SET RESULT=RESULT_PRINTA_" "  
  . ;---
  . NEW PRTBMODE SET PRTBMODE=$PIECE(NODE1D1,"^",2)
  . NEW PRINTB SET PRINTB=$$GETRXNAM(.ARR,PRTBMODE) 
  . NEW CAPB SET CAPB=$PIECE(NODE1D2,"^",2)
  . IF CONTROLLED SET CAPB="UC"  ;"FORCE UPPER CASE
  . SET PRINTB=$$CAPBYMOD(PRINTB,CAPB)  
  . IF PRINTB'="" SET RESULT=RESULT_"("_PRINTB_") "  
  DO  ;"MODIFIER  (PART C)     
  . NEW PRTCMODE SET PRTCMODE=$PIECE(NODE1D1,"^",3)
  . NEW PRINTC SET PRINTC=$$GETMODFY(.ARR,PRTCMODE)  ;"GET MODIFIER, BASED ON MODE
  . IF PRINTC="" QUIT
  . NEW CAPC SET CAPC=$PIECE(NODE1D2,"^",3) 
  . SET PRINTC=$$CAPBYMOD(PRINTC,CAPC)  
  . SET RESULT=RESULT_PRINTC_" "
  DO  ;"FORM  (PART D)
  . NEW PRTDMODE SET PRTDMODE=$PIECE(NODE1D1,"^",4)
  . NEW PRINTD SET PRINTD=$$GETFORM(.ARR,PRTDMODE)  ;"GET RX FORM, BASED ON MODE
  . IF PRINTD="" QUIT
  . NEW CAPD SET CAPD=$PIECE(NODE1D2,"^",4)  
  . SET PRINTD=$$CAPBYMOD(PRINTD,CAPD)  
  . SET RESULT=RESULT_PRINTD_" "
  DO  ;"DOSE  (PART E)
  . NEW PRTEMODE SET PRTEMODE=$PIECE(NODE1D1,"^",5)
  . NEW PRINTE SET PRINTE=$$GETDOSE(.ARR,PRTEMODE)  ;"GET DOSE, BASED ON MODE
  . IF PRINTE="" QUIT
  . NEW CAPE SET CAPE=$PIECE(NODE1D2,"^",5)  
  . SET PRINTE=$$CAPBYMOD(PRINTE,CAPE)  
  . SET RESULT=RESULT_PRINTE_" "
  DO  ;"UNITS (PART F)
  . NEW PRTFMODE SET PRTFMODE=$PIECE(NODE1D1,"^",6)
  . NEW PRINTF SET PRINTF=$$GETUNITS(.ARR,PRTFMODE)  ;"GET UNITS, BASED ON MODE
  . IF PRINTF="" QUIT
  . NEW CAPF SET CAPF=$PIECE(NODE1D2,"^",6)  
  . SET PRINTF=$$CAPBYMOD(PRINTF,CAPF)  
  . IF PRINTF="%" SET RESULT=$$TRIM^XLFSTR(RESULT)
  . SET RESULT=RESULT_PRINTF_" "
  DO  ;"SIG
  . NEW SIG SET SIG=$GET(ARR("SIG"))
  . IF SIG'="" SET RESULT=$$TRIM^XLFSTR(RESULT)_"; "_SIG_" "
  DO  ;"PREFACE -- MOVING TO END IF NEEDED
  . NEW PREFACE SET PREFACE=$$TRIM^XLFSTR($GET(ARR("PREFACE")))
  . IF $LENGTH(PREFACE)>15 DO  QUIT  ;"put long preface comments into NOTES spot
  . . NEW NOTE SET NOTE=$GET(ARR("NOTE"))
  . . IF NOTE'="" SET NOTE=NOTE_"; "
  . . SET ARR("NOTE")=NOTE_PREFACE
  . . SET ARR("PREFACE")=""
  DO  ;"NOTES
  . NEW NOTE SET NOTE=$GET(ARR("NOTE"))
  . IF NOTE'="" SET RESULT=RESULT_"<-- "_NOTE_" "
  SET ARR("RESULT_BEFORE_PREFACE")=RESULT
  DO  ;"OTC
  . IF $GET(ARR("OTC"))'=1 QUIT
  . SET RESULT="OTC "_RESULT
  . SET ARR("RESULT_BEFORE_PREFACE","PREFACE")="OTC "
  DO  ;"PREFACE
  . NEW PREFACE SET PREFACE=$$TRIM^XLFSTR($GET(ARR("PREFACE")))
  . IF PREFACE="" QUIT
  . SET RESULT=PREFACE_" "_RESULT
  . SET ARR("RESULT_BEFORE_PREFACE","PREFACE")=PREFACE_" "_$GET(ARR("RESULT_BEFORE_PREFACE","PREFACE"))
  ;
  GOTO A2SDN
A2SER  ;
  SET RESULT=$GET(ARR("ORIG"))
  IF RESULT="" SET RESULT="????"
  ELSE  SET RESULT="'"_RESULT_"'"
A2SDN ;
  SET ARR("RESULT")=RESULT
  QUIT RESULT
  ;
GETRXNAM(ARR,MODE)  ;"GET RX NAME, BASED ON MODE.
  ;"Input: ARR -- PASS BY REFERENCE.  
  ;"         This is array as output by PARSELN^TMGRX001.  Format (PARTIAL):
  ;"         ARR("MEDICATION","INPUT NAME")=<NAME OF RX>  <-- last Rx name encountered on line (input name)
  ;"         ARR("MEDICATION","GENERIC")=<NAME OF RX> <-- Generic name as found on input line (if provided)  
  ;"         ARR("MEDICATION","GENERIC","DATABASE") <-- Generic name based on IEN22733:.01 field
  ;"         ARR("MEDICATION","GENERIC ALIAS")=SOURCE <-- Generic alias name, as found on input line (if provided)
  ;"         ARR("MEDICATION","GENERIC ALIAS","PREFERRED")=<PREFERRED GENERIC ALIAS> from database  
  ;"         ARR("MEDICATION","GENERIC ABBRV")=SOURCE <-- Generic abbreviation, as found on input line (if provided)
  ;"         ARR("MEDICATION","GENERIC ABBRV","PREFERRED")=<preferred generic abbreviation> from database  
  ;"         ARR("MEDICATION","BRAND")=<NAME OF RX> <-- Brand name, as found on input line (if provided)
  ;"         ARR("MEDICATION","BRAND","PREFERRED")=<Preferred brand name from database>  
  NEW RESULT SET RESULT=""
  IF (MODE="GE")!(MODE="") DO   ;"GENERIC NAME
  . SET RESULT=$GET(ARR("MEDICATION","GENERIC","DATABASE"))
  ELSE  IF (MODE="GAL") DO      ;"PREFERRED GENERIC ALIAS
  . SET RESULT=$GET(ARR("MEDICATION","GENERIC ALIAS","PREFERRED"))
  ELSE  IF (MODE="GAB") DO      ;"PREFERRED GENERIC ABBREVIATION
  . SET RESULT=$GET(ARR("MEDICATION","GENERIC ABBRV","PREFERRED"))
  ELSE  IF (MODE="GBI") DO      ;"GIVEN MATCHED BRAND NAME, IF BRAND GIVEN
  . SET RESULT=$GET(ARR("MEDICATION","BRAND"))  
  ELSE  IF (MODE="PBI") DO      ;"PREFERRED BRAND NAME, IF BRAND GIVEN
  . NEW ABRAND SET ABRAND=$GET(ARR("MEDICATION","BRAND"))
  . IF ABRAND="" QUIT  ;"LEAVE RESULT EMPTY  
  . SET RESULT=$GET(ARR("MEDICATION","BRAND","PREFERRED"))
  ELSE  IF (MODE="PBA") DO  ;"PREFERRED BRAND ALWAYS (IF REGISTERED)
  . SET RESULT=$GET(ARR("MEDICATION","BRAND","PREFERRED"))
  ELSE  IF (MODE="NO") DO
  . SET RESULT=""
  QUIT RESULT               
  ;
GETMODFY(ARR,MODE)  ;"GET MODIFIER, BASED ON MODE
  ;"Input: ARR -- PASS BY REFERENCE.  
  ;"   ARR("MODIFIER")=<modifier> <-- modifier as found in input line (if provided)  
  ;"   ARR("MODIFIER","PREFERRED")=<PREFERRED MODIFIER, IN SAME GROUP AS MOD>  (if form determined)  
  NEW RESULT SET RESULT=""
  IF MODE="NO" QUIT ""                ;"NEVER SHOW
  ELSE  IF MODE="GMI" DO              ;"GIVEN MATCHED MODIFIER, IF PROVIDED
  . SET RESULT=$GET(ARR("MODIFIER"))
  ELSE  IF MODE="PMI" DO              ;"PREFERRED MODIFIER, IF A MODIFIER PROVIDED
  . SET RESULT=$GET(ARR("MODIFIER")) IF RESULT="" QUIT
  . SET RESULT=$GET(ARR("MODIFIER","PREFERRED")) 
  ELSE  IF MODE="PMA" DO              ;"PREFERRED MODIFIER ALWAYS
  . SET RESULT=$GET(ARR("MODIFIER","PREFERRED")) 
  QUIT RESULT
  ;  
GETFORM(ARR,MODE)  ;"GET RX FORM, BASED ON MODE
  ;"Input: ARR -- PASS BY REFERENCE.  
  ;"   ARR("FORM")=<FORM> <-- form as found on input line (if provided)
  ;"   ARR("FORM","SUBIEN")=<#>  <--
  ;"   ARR("FORM","DATABASE")=<FORM> from database
  ;"   ARR("FORM","PREFERRED")=<preferred form alias> from database    
  NEW RESULT SET RESULT=""
  IF MODE="NO" QUIT ""               ;"NEVER SHOW
  ELSE  IF MODE="RF" DO              ;"REGISTERED FORM
  . SET RESULT=$GET(ARR("FORM","DATABASE"))
  ELSE  IF MODE="GF" DO              ;"GIVEN FORM
  . SET RESULT=$GET(ARR("FORM"))
  ELSE  IF MODE="PF" DO              ;"PREFERRED FORM ALIAS
  . SET RESULT=$GET(ARR("FORM","PREFERRED")) 
  QUIT RESULT
  ;
GETDOSE(ARR,MODE)  ;"GET DOSE, BASED ON MODE
  ;"  ARR("DOSE","IENS",<IENS>)=<DOSE_NAME> , e.g. TAB
  ;"  ARR("DOSE")=<DOSE>  <-- dose as found on input line (if provided)
  ;"  ARR("DOSE","DATABASE")=<registered dose strength for last IENS found> <-- if found.   
  ;"  ARR("DOSE","IENS",<IENS IN 22733.03>)=<FORM>^<Strength>^<Preferred Alias>  <-- may be multiple.
  ;-------------------
  ;"DO  ;"DOSE
  ;". NEW DOSE SET DOSE=$GET(ARR("DOSE","DATABASE")) 
  ;". IF DOSE="" SET DOSE=$GET(ARR("DOSE")) 
  ;". NEW OPTIONAL SET OPTIONAL=$PIECE($GET(^TMG(22733,IEN22733,1.5)),"^",4)
  ;". IF DOSE="",OPTIONAL="Y" QUIT
  ;". IF DOSE="" SET DOSE="_?_dose"
  ;". SET RESULT=RESULT_DOSE_" "
  ;-------------------
  NEW RESULT SET RESULT=""
  IF MODE="NO" QUIT ""     ;"NEVER SHOW   
  ELSE  IF MODE="RDI" DO   ;"REGISTERED DOSE, IF AN INPUT DOSE WAS GIVEN
  . NEW ADOSE SET ADOSE=$GET(ARR("DOSE")) IF ADOSE="" QUIT
  . SET RESULT=$GET(ARR("DOSE","DATABASE"))
  ELSE  IF MODE="RDA" DO   ;"REGISTERED DOSE ALWAYS (_?_ IF NOT FOUND)
  . SET RESULT=$GET(ARR("DOSE","DATABASE"))
  . IF RESULT="" SET RESULT="_?_dose"
  ELSE  IF MODE="PDAI" DO  ;"PREFERRED DOSE ALIAS, IF AN INPUT DOSE GIVEN
  . NEW ADOSE SET ADOSE=$GET(ARR("DOSE")) IF ADOSE="" QUIT
  . NEW IENS SET IENS=$ORDER(ARR("DOSE","IENS","")) QUIT:IENS=""
  . NEW TEMP SET TEMP=$GET(ARR("DOSE","IENS",IENS))
  . SET RESULT=$PIECE(TEMP,"^",3)
  ELSE  IF MODE="PDAA" DO  ;"PREFERRED DOSE ALIAS ALWAYS (_?_) IF NOT FOUND
  . SET RESULT="_?_dose"
  . NEW IENS SET IENS=$ORDER(ARR("DOSE","IENS","")) QUIT:IENS=""
  . NEW TEMP SET TEMP=$GET(ARR("DOSE","IENS",IENS))
  . IF $PIECE(TEMP,"^",3)="" QUIT
  . SET RESULT=$PIECE(TEMP,"^",3)                      
  QUIT RESULT
  ;
GETUNITS(ARR,MODE)  ;"GET UNITS, BY MODE
  ;"  ARR("UNITS")=E.G. "MG"  <-- units as found on input line (if provided)
  ;"  ARR("UNITS","IEN50.607")=#  <-- this is UNITS IEN (if units provided)
  ;"  ARR("UNITS","DATABASE")=<units> from database  
  ;"  ARR("UNITS","PREFERRED")=<PREFERRED UNITS>-- if form determined
  ;--------------------
  ;" DO  ;"UNITS
  ;" . NEW UNITS SET UNITS=""
  ;" . NEW UNITIEN SET UNITIEN=+$GET(ARR("IEN50.607"))
  ;" . SET UNITS=$PIECE($GET(^PS(50.607,UNITIEN,0)),"^",1)
  ;" . IF UNITS="" DO
  ;" . . NEW IENS SET IENS=$ORDER(ARR("DOSE","IENS",""))
  ;" . . QUIT:IENS=""
  ;" . . SET UNITS=$$GET1^DIQ(22733.03,IENS,.02)
  ;" . IF UNITS="" DO
  ;" . . NEW RAWUNITS SET RAWUNITS=$GET(ARR("UNITS"))
  ;" . . IF RAWUNITS'="" SET UNITS="["_$GET(ARR("UNITS"))_"]"  
  ;" . IF 1=0,UNITS="" DO   ;"<--- REMOVE 1=0, TO FORCE UNITS PROMPT
  ;" . . SET UNITS="_?_units"
  ;" . IF UNITS="" QUIT
  ;" . IF UNITS="%" DO
  ;" . . SET RESULT=$$TRIM^XLFSTR(RESULT)_UNITS_" "
  ;" . ELSE  SET RESULT=RESULT_$$LOW^XLFSTR(UNITS)_" "
  ;--------------------
  NEW RESULT SET RESULT=""
  IF MODE="NO" QUIT ""       ;"NEVER SHOW 
  ELSE  IF MODE="RU" DO      ;"REGISTERED UNITS, ALWAYS SHOW
  . SET RESULT=$GET(ARR("UNITS","DATABASE"))
  ELSE  IF MODE="RUI" DO     ;"REGISTERED UNITS, IF UNITS IN INPUT
  . NEW AUNIT SET AUNIT=$GET(ARR("UNITS")) QUIT:AUNIT=""
  . SET RESULT=$GET(ARR("UNITS","DATABASE"))
  ELSE  IF MODE="PUA" DO     ;"PREFERRED UNIT ALIAS
  . SET RESULT=$GET(ARR("UNITS","PREFERRED"))
  QUIT RESULT
  ;
CAPBYMOD(STR,MODE)  ;"CAPITALIZE STR BY MODE
  NEW RESULT SET RESULT=$GET(STR)
  SET MODE=$GET(MODE)
  IF MODE="LC" DO
  . SET RESULT=$$LOW^XLFSTR(RESULT)
  ELSE  IF MODE="UC" DO
  . SET RESULT=$$UP^XLFSTR(RESULT)
  ELSE  IF MODE="FU" DO
  . SET RESULT=$$CAP1STAL^TMGSTUT2(RESULT,$$DIVSET^TMGRX004())
  ELSE  IF (MODE="")!(MODE="REG") DO
  . ;"leave capitalization unchanged.  
  QUIT RESULT
  ;
  ;  
  ;"GETBRAND(IEN22733)  ;"GET BRANDNAME
  ;"  NEW RESULT SET RESULT=""
  ;"  NEW IDX SET IDX=0
  ;"  NEW BRANDS
  ;"  NEW ABRAND SET ABRAND=""
  ;"  FOR  SET ABRAND=$ORDER(^TMG(22733,IEN22733,1,"B",ABRAND)) QUIT:ABRAND=""  DO
  ;"  . SET IDX=IDX+1,BRANDS(IDX)=ABRAND
  ;"  IF IDX>1 DO
  ;"  . NEW SUBIEN SET SUBIEN=0
  ;"  . FOR  SET SUBIEN=$ORDER(^TMG(22733,IEN22733,1,SUBIEN)) QUIT:(SUBIEN'>0)!(RESULT'="")  DO
  ;"  . . NEW ZN SET ZN=$GET(^TMG(22733,IEN22733,1,SUBIEN,0))
  ;"  . . IF $PIECE(ZN,"^",2)="Y" SET RESULT=$PIECE(ZN,"^",1) 
  ;"  . IF RESULT="" SET RESULT=$GET(BRANDS(1)) 
  ;"  ELSE  SET RESULT=$GET(BRANDS(1))
  ;"  QUIT RESULT
  ;"  ;
  ;"GETPMOD2(IEN22733,FORMIEN,ONLYPREF) ;"GET PREFERRED MOD, FOR GIVEN FORMIEN
  ;"GETPMOD2(IEN22733,FORMIEN) ;"GET PREFERRED MOD, FOR GIVEN FORMIEN
  ;"  NEW ROOT SET ROOT=$NAME(^TMG(22733,IEN22733,2,FORMIEN,2))
  ;"  QUIT $$GETPREF^TMGRX004(ROOT)
  ;"NEW MOD SET MOD=""
  ;"NEW ONLYPREF SET ONLYPREF=$GET(ONLYPREF,1)
  ;"NEW AMODIEN SET AMODIEN=0
  ;"FOR  SET AMODIEN=$ORDER(^TMG(22733,IEN22733,2,FORMIEN,2,AMODIEN)) QUIT:(AMODIEN'>0)!(MOD'="")  DO
  ;". NEW ZN SET ZN=$GET(^TMG(22733,IEN22733,2,FORMIEN,2,AMODIEN,0))
  ;". IF ONLYPREF,$PIECE(ZN,"^",2)'="Y" QUIT
  ;". SET MOD=$PIECE(ZN,"^",1)
  ;"QUIT MOD
  ;
  ;"GETMOD(IEN22733,FORMIEN)  ;"GET MODIFIER FOR FORMIEN
  ;"  NEW MOD SET MOD=$$GETPMOD2(IEN22733,FORMIEN) ;"get one marked as preferred
  ;"  IF MOD="" SET MOD=$$GETPMOD2(IEN22733,FORMIEN,0) ;"get first avail modifier
  ;"  QUIT MOD
  ;
  ;"EXTERNAL(ARR)  ;"TURN MEDICATION ARRAY INTO OUTPUT STRING  -- DELETE LATER  
  ;"  ;"Input: ARR -- PASS BY REFERENCE.  
  ;"  ;"         This is array as output by PARSELN^TMGRX001.  Format:
  ;"  ;"         ARR("ORIG")=The original unparsed line.  
  ;"  ;"         ARR("NOTE")=<any notes>  DELIMITED BY "<---" OR '>---" ...
  ;"  ;"         ARR("MEDICATION","INPUT NAME")=<NAME OF RX>
  ;"  ;"         ARR("MEDICATION","GENERIC")=<NAME OF RX>
  ;"  ;"         ARR("MEDICATION","BRAND")=<NAME OF RX>
  ;"  ;"         ARR("FORM")=<name of form from original line>  (if provided)
  ;"  ;"         ARR("MODIFIER")="ER"
  ;"  ;"         ARR("SIG")="{{ROUTE}} {{FREQ}} PRN HEADACHES"                                      
  ;"  ;"         ARR("SIG","FREQ" = BIDlp): step OVER// SHOW ARR                                       
  ;"  ;"         ARR("SIG","ROUTE" = PO
  ;"  ;"         ARR("DOSE")=<dose from original line>
  ;"  ;"         ARR("DOSE","DATABASE")=<standardized dose from database>
  ;"  ;"         ARR("DOSE","IENS",<IENS>)=<DOSE_NAME> , e.g. TAB
  ;"  ;"         ARR("OTC")=1  <-- if specified as OTC
  ;"  ;"         ARR("UNITS")=E.G. "MG"
  ;"  ;"         ARR("IEN50.607")=#  
  ;"  ;"         ARR("PREFACE")= e.g. HOLD, HOLDING, OFF
  ;"  ;"         ARR("IEN22733")=<IEN 22733>
  ;"  NEW RESULT SET RESULT=""
  ;"  NEW FAILURE SET FAILURE=0
  ;"  NEW IEN22733 SET IEN22733=+$GET(ARR("IEN22733")) GOTO:IEN22733'>0 A2SER  
  ;"  NEW PRINTMODE SET PRINTMODE=$PIECE($GET(^TMG(22733,IEN22733,"1.5")),"^",3)
  ;"  IF PRINTMODE="" SET PRINTMODE="GE"
  ;"  NEW MODIFIERMODE SET MODIFIERMODE=$PIECE($GET(^TMG(22733,IEN22733,"1.5")),"^",5)
  ;"  NEW DOSEIENS SET DOSEIENS=$ORDER(ARR("DOSE","IENS",""))
  ;"  IF DOSEIENS'="",$ORDER(ARR("DOSE","IENS",DOSEIENS))'="" SET DOSEIENS=""  ;"multiple DOSE IENS's present
  ;"  IF DOSEIENS="" SET DOSEIENS=$$MOD2FRM^TMGRX004(.ARR)
  ;"  NEW FORMIEN SET FORMIEN=+$PIECE(DOSEIENS,",",2)
  ;"  DO   ;"MED NAME
  ;"  . NEW CONTROLLED SET CONTROLLED=($PIECE($GET(^TMG(22733,IEN22733,"1.5")),"^",2)="Y")
  ;"  . ;"NOTE: MODES AS FOLLOWS
  ;"  . ;"  GE  -- GENERIC ONLY
  ;"  . ;"  BR  -- BRAND ONLY
  ;"  . ;"  BO  -- BOTH GENERIC AND BRAND
  ;"  . ;"  GBI -- GENERIC & BRAND (IF PROVIDED)
  ;"  . ;"  BGI -- BRAND & GENERIC (IF PROVIDED)
  ;"  . IF "^GE^BO^GBI^BGI^GEA^"[("^"_PRINTMODE_"^") DO  QUIT:FAILURE  ;"Generic name
  ;"  . . NEW RXNAME SET RXNAME=""
  ;"  . . ;"TO DO -- IMPLEMENT BGI (SHOW GENERIC ONLY IF PROVIDED IN INPUT)
  ;"  . . SET RXNAME=$GET(ARR("MEDICATION","GENERIC","DATABASE"))
  ;"  . . IF RXNAME="" SET RXNAME=$GET(ARR("MEDICATION","GENERIC"))
  ;"  . . IF RXNAME="" SET RXNAME=$PIECE($GET(^TMG(22733,IEN22733,0)),"^",1)
  ;"  . . IF RXNAME="" SET RXNAME="["_$GET(ARR("MEDICATION","INPUT NAME"))_"]"  ;"<-- shouldn't happen
  ;"  . . IF RXNAME="" SET FAILURE=1 QUIT
  ;"  . . IF CONTROLLED SET RXNAME=$$UP^XLFSTR(RXNAME)
  ;"  . . ELSE  SET RXNAME=$$LOW^XLFSTR(RXNAME)
  ;"  . . SET RESULT=RESULT_RXNAME_" "
  ;"  . IF "^BR^BO^GBI^BGI^"[("^"_PRINTMODE_"^") DO  QUIT:FAILURE  ;"Brand name
  ;"  . . NEW RXNAME2 SET RXNAME2="" 
  ;"  . . IF RXNAME2="" SET RXNAME2=$GET(ARR("MEDICATION","BRAND"))
  ;"  . . IF RXNAME2="",PRINTMODE="GBI" QUIT
  ;"  . . IF RXNAME2="" SET RXNAME2=$$GETPRBRD^TMGRX004(IEN22733)  ;"GET PREFERRED BRAND NAME //$$GETBRAND(IEN22733)
  ;"  . . IF RXNAME2="" SET RXNAME2="["_$GET(ARR("MEDICATION","INPUT NAME"))_"]"  ;"<-- shouldn't happen
  ;"  . . IF RXNAME2="" SET FAILURE=1 QUIT
  ;"  . . IF CONTROLLED SET RXNAME2=$$UP^XLFSTR(RXNAME2)
  ;"  . . ELSE  SET RXNAME2=$$CAP1STAL^TMGSTUT2(RXNAME2,$$DIVSET^TMGRX004())
  ;"  . . IF PRINTMODE'="BR" SET RXNAME2="("_RXNAME2_")"
  ;"  . . SET RESULT=RESULT_RXNAME2_" "
  ;"  . IF "^GEA^"[("^"_PRINTMODE_"^") DO  QUIT:FAILURE  ;"Abbreviation
  ;"  . . NEW RXNAME3 SET RXNAME3=$$GETPRGB^TMGRX004(IEN22733)  ;"GET PREFERRED ABBREVIATION FOR RX.  
  ;"  . . IF RXNAME3="" QUIT
  ;"  . . SET RESULT=RESULT_"("_RXNAME3_") "
  ;"  IF FAILURE GOTO A2SER
  ;"  DO  ;"MODIFIER
  ;"  . NEW FORMZN SET FORMZN=$GET(^TMG(22733,IEN22733,2,FORMIEN,0))
  ;"  . NEW MODFYPRMODE SET MODFYPRMODE=$PIECE(FORMZN,"^",3)
  ;"  . NEW SWITCHMODFY SET SWITCHMODFY=$PIECE(FORMZN,"^",4)    
  ;"  . ;"NOTE: MODIFIER PRINT MODES:
  ;"  . ;"  ALW      ALWAYS SHOW MODIFIER  <-- can't show if none defined
  ;"  . ;"  IF       SHOW IF FOUND IN INPUT
  ;"  . ;"  NO       NEVER SHOW
  ;"  . NEW MOD SET MOD=$GET(ARR("MODIFIER"))
  ;"  . IF MODFYPRMODE="NO" QUIT
  ;"  . IF MODFYPRMODE="ALW",MOD="" SET MOD=$$GETMOD(IEN22733,FORMIEN)
  ;"  . IF SWITCHMODFY="Y" SET MOD=$$GETPMOD2^TMGRX004(IEN22733,FORMIEN)
  ;"  . IF MOD="" QUIT
  ;"  . SET RESULT=RESULT_MOD_" "
  ;"  DO  ;"DOSE
  ;"  . NEW DOSE SET DOSE=$GET(ARR("DOSE","DATABASE")) 
  ;"  . IF DOSE="" SET DOSE=$GET(ARR("DOSE")) 
  ;"  . NEW OPTIONAL SET OPTIONAL=$PIECE($GET(^TMG(22733,IEN22733,1.5)),"^",4)
  ;"  . IF DOSE="",OPTIONAL="Y" QUIT
  ;"  . IF DOSE="" SET DOSE="_?_dose"
  ;"  . SET RESULT=RESULT_DOSE_" "
  ;"  DO  ;"UNITS
  ;"  . NEW UNITS SET UNITS=""
  ;"  . NEW UNITIEN SET UNITIEN=+$GET(ARR("IEN50.607"))
  ;"  . SET UNITS=$PIECE($GET(^PS(50.607,UNITIEN,0)),"^",1)
  ;"  . IF UNITS="" DO
  ;"  . . NEW IENS SET IENS=$ORDER(ARR("DOSE","IENS",""))
  ;"  . . QUIT:IENS=""
  ;"  . . SET UNITS=$$GET1^DIQ(22733.03,IENS,.02)
  ;"  . IF UNITS="" DO
  ;"  . . NEW RAWUNITS SET RAWUNITS=$GET(ARR("UNITS"))
  ;"  . . IF RAWUNITS'="" SET UNITS="["_$GET(ARR("UNITS"))_"]"  
  ;"  . IF 1=0,UNITS="" DO   ;"<--- REMOVE 1=0, TO FORCE UNITS PROMPT
  ;"  . . SET UNITS="_?_units"
  ;"  . IF UNITS="" QUIT
  ;"  . IF UNITS="%" DO
  ;"  . . SET RESULT=$$TRIM^XLFSTR(RESULT)_UNITS_" "
  ;"  . ELSE  SET RESULT=RESULT_$$LOW^XLFSTR(UNITS)_" "
  ;"  DO  ;"SIG
  ;"  . NEW SIG MERGE SIG=ARR("SIG")
  ;"  . SET SIG=$$TRIM^XLFSTR($$FIXSIG(.SIG))
  ;"  . ;"SET SIG=$$TRIMDIVS^TMGRX001(SIG,"L") ;"//trim off any leading divider characters
  ;"  . IF SIG'="" SET RESULT=$$TRIM^XLFSTR(RESULT)_"; "_SIG_" "
  ;"  DO  ;"PREFACE
  ;"  . NEW PREFACE SET PREFACE=$$TRIM^XLFSTR($GET(ARR("PREFACE")))
  ;"  . IF $LENGTH(PREFACE)>15 DO  QUIT  ;"put long preface comments into NOTES spot
  ;"  . . NEW NOTE SET NOTE=$GET(ARR("NOTE"))
  ;"  . . IF NOTE'="" SET NOTE=NOTE_"; "
  ;"  . . SET ARR("NOTE")=NOTE_PREFACE
  ;"  . . SET ARR("PREFACE")=""
  ;"  DO  ;"NOTES
  ;"  . NEW NOTE SET NOTE=$GET(ARR("NOTE"))
  ;"  . IF NOTE'="" SET RESULT=RESULT_"<-- "_NOTE_" "
  ;"  SET ARR("RESULT_BEFORE_PREFACE")=RESULT
  ;"  DO  ;"OTC
  ;"  . IF $GET(ARR("OTC"))'=1 QUIT
  ;"  . SET RESULT="OTC "_RESULT
  ;"  . SET ARR("RESULT_BEFORE_PREFACE","PREFACE")="OTC "
  ;"  DO  ;"PREFACE
  ;"  . NEW PREFACE SET PREFACE=$$TRIM^XLFSTR($GET(ARR("PREFACE")))
  ;"  . IF PREFACE="" QUIT
  ;"  . SET RESULT=PREFACE_" "_RESULT_PREFACE
  ;"  . SET ARR("RESULT_BEFORE_PREFACE","PREFACE")=PREFACE_" "_$GET(ARR("RESULT_BEFORE_PREFACE","PREFACE"))
  ;"  ;
  ;"  GOTO A2SDN
  ;"A2SER  ;
  ;"  SET RESULT=$GET(ARR("ORIG"))
  ;"  IF RESULT="" SET RESULT="????"
  ;"  ELSE  SET RESULT="'"_RESULT_"'"
  ;"A2SDN ;
  ;"  SET ARR("RESULT")=RESULT
  ;"  QUIT RESULT
  ;"  ;


