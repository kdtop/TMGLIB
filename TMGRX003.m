TMGRX003 ;TMG/kst/Patient medication code; 08/25/17, 9/23/17
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
  ;"         ARR("STRENGTH")=<STRENGTH>  <-- strength as found on input line (if provided)
  ;"         ARR("STRENGTH","DATABASE")=<registered strength strength for last IENS found> <-- if found.   
  ;"         ARR("STRENGTH","IENS",<IENS IN 22733.03>)=<FORM>^<Strength>^<Preferred Alias>  <-- may be multiple. 
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
  NEW STRENGTHIENS SET STRENGTHIENS=$ORDER(ARR("STRENGTH","IENS",""))
  IF STRENGTHIENS'="",$ORDER(ARR("STRENGTH","IENS",STRENGTHIENS))'="" SET STRENGTHIENS=""  ;"multiple STRENGTH IENS's present
  IF STRENGTHIENS="" SET STRENGTHIENS=$$MOD2FRM^TMGRX004(.ARR)
  NEW FORMIEN SET FORMIEN=+$PIECE(STRENGTHIENS,",",2)
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
  DO  ;"STRENGTH  (PART E)
  . NEW PRTEMODE SET PRTEMODE=$PIECE(NODE1D1,"^",5)
  . NEW PRINTE SET PRINTE=$$GETSTRNT(.ARR,PRTEMODE)  ;"GET STRENGTH, BASED ON MODE
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
  ;"ELSE  SET RESULT="'"_RESULT_"'"  ;"<--- RESTORE IF WANT ORIGINAL LINE IN QUOTES
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
GETSTRNT(ARR,MODE)  ;"GET STRENGTH, BASED ON MODE
  ;"  ARR("STRENGTH")=<STRENGTH>  <-- strength as found on input line (if provided)
  ;"  ARR("STRENGTH","DATABASE")=<registered strength strength for last IENS found> <-- if found.   
  ;"  ARR("STRENGTH","IENS",<IENS IN 22733.03>)=<FORM>^<Strength>^<Preferred Alias>  <-- may be multiple.
  ;-------------------
  NEW RESULT SET RESULT=""
  IF MODE="NO" QUIT ""     ;"NEVER SHOW   
  ELSE  IF MODE="RDI" DO   ;"REGISTERED STRENGTH, IF AN INPUT STRENGTH WAS GIVEN
  . NEW ASTRENGTH SET ASTRENGTH=$GET(ARR("STRENGTH")) IF ASTRENGTH="" QUIT
  . SET RESULT=$GET(ARR("STRENGTH","DATABASE"))
  ELSE  IF MODE="RDA" DO   ;"REGISTERED STRENGTH ALWAYS (_?_ IF NOT FOUND)
  . SET RESULT=$GET(ARR("STRENGTH","DATABASE"))
  . IF RESULT="" SET RESULT="_?_strength"
  ELSE  IF MODE="PDAI" DO  ;"PREFERRED STRENGTH ALIAS, IF AN INPUT STRENGTH GIVEN
  . NEW ASTRENGTH SET ASTRENGTH=$GET(ARR("STRENGTH")) IF ASTRENGTH="" QUIT
  . NEW IENS SET IENS=$ORDER(ARR("STRENGTH","IENS","")) QUIT:IENS=""
  . NEW TEMP SET TEMP=$GET(ARR("STRENGTH","IENS",IENS))
  . SET RESULT=$PIECE(TEMP,"^",3)
  ELSE  IF MODE="PDAA" DO  ;"PREFERRED STRENGTH ALIAS ALWAYS (_?_) IF NOT FOUND
  . SET RESULT="_?_strength"
  . NEW IENS SET IENS=$ORDER(ARR("STRENGTH","IENS","")) QUIT:IENS=""
  . NEW TEMP SET TEMP=$GET(ARR("STRENGTH","IENS",IENS))
  . IF $PIECE(TEMP,"^",3)="" QUIT
  . SET RESULT=$PIECE(TEMP,"^",3)    
  ELSE  IF MODE="GMD" DO  ;"GIVEM MATCHED STRENGTH
  . SET RESULT=$GET(ARR("STRENGTH"))
  QUIT RESULT
  ;
GETUNITS(ARR,MODE)  ;"GET UNITS, BY MODE
  ;"  ARR("UNITS")=E.G. "MG"  <-- units as found on input line (if provided)
  ;"  ARR("UNITS","IEN50.607")=#  <-- this is UNITS IEN (if units provided)
  ;"  ARR("UNITS","DATABASE")=<units> from database  
  ;"  ARR("UNITS","PREFERRED")=<PREFERRED UNITS>-- if form determined
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
  ELSE  IF MODE="GMU" DO     ;"GIVEN MATCHED UNITS.
  . SET RESULT=$GET(ARR("UNITS"))
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