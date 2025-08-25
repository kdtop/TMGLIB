TMGRX001 ;TMG/kst/Patient medication code; 08/23/17, 3/24/21
       ;;1.0;TMG-LIB;**1**;08/23/17
 ;
 ;"Code for parsing medication string into array
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 08/23/17  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"T2 ; "TEST THE PARSED MED LIST FOR A PATIENT 
 ;"PARSEARR(OUT,ARR)  ;"PARSE A MED LIST  
 ;"PARSELN(OUT,LINE,TRAIN,OPTION) ;"PARSE ONE MED LINE
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"PARS2SUM(OUT,SUMRY,TRAIN,OPTION)  ;"PARSE TO SUMMARY
 ;"GETSUMRY(ARR,LINE,IEN22733,PARSESIG)  ;"GET A SUMMARY OF LINE
 ;"VERFYSUM(SUMRY,TRAIN,LINE,OPTION)  ;
 ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"=======================================================================
 ;"Uses:  
 ;"=======================================================================
 ;        
TEST ; "TEST THE PARSED MED LIST FOR A PATIENT 
  NEW X,Y,DIC SET DIC=2,DIC(0)="MAEQ"
TL1 ;  
  DO ^DIC WRITE !
  IF +Y'>0 QUIT
  NEW TMGDFN SET TMGDFN=+Y
  NEW ARR,TEMP
  DO MEDARR^TMGTIUOJ(.TEMP,TMGDFN,.ARR)
  WRITE !,"--this is before parsing  -------------",!
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:IDX'>0  DO
  . WRITE $GET(ARR(IDX)),!
  NEW OPTION,%
  SET %=1 WRITE !,"Parse medication as if for patient to read" DO YN^DICN WRITE !
  IF %=-1 QUIT
  IF %=1 SET OPTION("FOR PATIENTS")=1
  NEW ARR2 DO PARSEARR(.ARR2,.ARR,.OPTION)
  WRITE !,"--Below is parsed med list -------------",!
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(ARR2(IDX)) QUIT:IDX'>0  DO
  . WRITE $GET(ARR2(IDX)),!
  ;"WRITE !,"--Appearance in FOLLOWING runs -------------",!
  ;"NEW ARR3 DO PARSEARR(.ARR3,.ARR2)
  ;"NEW IDX SET IDX=0
  ;"FOR  SET IDX=$ORDER(ARR3(IDX)) QUIT:IDX'>0  DO
  ;". WRITE $GET(ARR3(IDX)),!  
  SET %=1 WRITE !,"Try another patient" DO YN^DICN WRITE !
  IF %=1 GOTO TL1
  QUIT
  ;
PARSEARR(OUT,ARR,OPTION)  ;"PARSE A MED LIST  
  NEW OLD,IDX,JDX SET (IDX,JDX)=0 
  FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:IDX'>0  DO
  . NEW LINE,LINE2 SET LINE=$$TRIM^XLFSTR($GET(ARR(IDX))),LINE2=LINE 
  . IF LINE["[OLD ENTRY]" QUIT
  . SET OUT(IDX)=LINE
  . IF LINE["[MEDICATION" QUIT
  . IF $EXTRACT(LINE,1)'="*" DO
  . . NEW TEMP DO PARSELN(.TEMP,LINE,,.OPTION)
  . . SET LINE2=$$TRIM^XLFSTR($$EXTERNAL^TMGRX003(.TEMP,.OPTION))
  . NEW DIV SET DIV=$$NEXTCH^TMGSTUT3(LINE2,0,":","=")
  . IF DIV'="" DO
  . . NEW KEY SET KEY=$$TRIM^XLFSTR($PIECE(LINE2,DIV,1))
  . . NEW VALUE SET VALUE=$$TRIM^XLFSTR($PIECE(LINE2,DIV,2,99))
  . . SET OUT("KEY-VALUE",KEY)=VALUE
  . . SET OUT("KEY-VALUE",KEY,"LINE")=LINE2
  . SET OUT(IDX)=LINE2
  . IF LINE2'=LINE DO
  . . SET LINE="[OLD ENTRY] "_LINE
  . . SET JDX=JDX+1,OLD(JDX)=LINE
  . . NEW DIV SET DIV=$$NEXTCH^TMGSTUT3(LINE,0,":","=") 
  . . QUIT:DIV=""
  . . NEW KEY SET KEY=$$TRIM^XLFSTR($PIECE(LINE,DIV,1))
  . . NEW VALUE SET VALUE=$$TRIM^XLFSTR($PIECE(LINE,DIV,2,99))
  . . SET OUT("KEY-VALUE",KEY)=VALUE
  . . SET OUT("KEY-VALUE",KEY,"LINE")=LINE2
  SET IDX=$ORDER(OUT("@"),-1)
  SET JDX=0
  FOR  SET JDX=$ORDER(OLD(JDX)) QUIT:JDX'>0  DO
  . NEW LINE SET LINE=$GET(OLD(JDX)) QUIT:LINE=""
  . SET IDX=IDX+1,OUT(IDX)=LINE
  QUIT
  ;
PARSELN(OUT,LINE,TRAIN,OPTION) ;"PARSE ONE MED LINE
  ;"Input: OUT -- PASS BY REFERENCE.  AN OUT PARAMETER.  Format:
  ;"         OUT("ORIG")=The original unparsed line.  
  ;"         OUT("IEN22733")=<IEN 22733>
  ;"         OUT("PREFACE")= e.g. HOLD, HOLDING, OFF
  ;"         OUT("OTC")=1  <-- if specified as OTC
  ;"         OUT("WORKING")=The input line, with preface removed and arrows fixed  
  ;"         OUT("MEDICATION","INPUT NAME")=<NAME OF RX>  <-- last Rx name encountered on line (input name)
  ;"         OUT("MEDICATION","GENERIC")=<NAME OF RX> <-- Generic name as found on input line (if provided)  
  ;"         OUT("MEDICATION","GENERIC","DATABASE") <-- Generic name based on IEN22733:.01 field
  ;"         OUT("MEDICATION","GENERIC ALIAS")=SOURCE <-- Generic alias name, as found on input line (if provided)
  ;"         OUT("MEDICATION","GENERIC ALIAS","PREFERRED")=<PREFERRED GENERIC ALIAS> from database  
  ;"         OUT("MEDICATION","GENERIC ABBRV")=SOURCE <-- Generic abbreviation, as found on input line (if provided)
  ;"         OUT("MEDICATION","GENERIC ABBRV","PREFERRED")=<preferred generic abbreviation> from database  
  ;"         OUT("MEDICATION","BRAND")=<NAME OF RX> <-- Brand name, as found on input line (if provided)
  ;"         OUT("MEDICATION","BRAND","PREFERRED")=<Preferred brand name from database>  
  ;"         OUT("STRENGTH")=<STRENGTH>  <-- strength as found on input line (if provided)
  ;"         OUT("STRENGTH","DATABASE")=<registered strength strength for last IENS found> <-- if found.   
  ;"         OUT("STRENGTH","IENS",<IENS IN 22733.03>)=<FORM>^<Strength>^<Preferred Alias>  <-- may be multiple. 
  ;"         OUT("UNITS")=E.G. "MG"  <-- units as found on input line (if provided)
  ;"         OUT("UNITS","IEN50.607")=#  <-- this is UNITS IEN (if units provided)
  ;"         OUT("UNITS","DATABASE")=<units> from database  
  ;"         OUT("UNITS","PREFERRED")=<PREFERRED UNITS>-- if form determined
  ;"         OUT("FORM")=<FORM> <-- form as found on input line (if provided)
  ;"         OUT("FORM","SUBIEN")=<#>  
  ;"         OUT("FORM","DATABASE")=<FORM> from database
  ;"         OUT("FORM","PREFERRED")=<preferred form alias> from database  
  ;"         OUT("MODIFIER")=<modifier> <-- modifier as found in input line (if provided)  
  ;"         OUT("MODIFIER","PREFERRED")=<PREFERRED MODIFIER, IN SAME GROUP AS MOD>  (if form determined)  
  ;"         OUT("SIG")="AS NEEDED"
  ;"         OUT("NOTE")=<any notes>  DELIMITED BY "<---" OR ">---" or "NOTE:" ...
  ;"         OUT("PARSED")=<ARRAY> as output by GETSUMRY
  ;"       LINE -- a string of text representing one medication line from med table.
  ;"       TRAIN -- OPTIONAL.  If 1 then interactive process if needed. 
  ;"                           2 is ALWAYS interactive
  ;"        OPTION -- optional.  
  ;"          OPTION("FOR PATIENTS")=1
  ;"Result: None  
  SET ^TMP("PARSELN^TMGRX001",$J)=LINE
  NEW SUMRY,RESULT
  DO FIXLINE(.OUT,.LINE,.OPTION)  ;"FIX LINE
  SET RESULT=$$PARS2SUM(.OUT,.SUMRY,.TRAIN,.OPTION)
  MERGE OUT("PARSED")=SUMRY
  IF '((RESULT>0)!(RESULT["MISSING STRENGTH")) GOTO PRSLNDN
  DO XTRCTDAT(.OUT,.SUMRY,.OPTION) ;"Extract data based on summry
  DO FIXINFO(.OUT,.OPTION)  ;"FIX INFORMATION ARRAY    
PRSLNDN  
  QUIT
  ;
FIXLINE(OUT,LINE,OPTION)  ;"FIX LINE
  ;"Input:  OUT
  ;"        LINE  -- pass by reference
  ;"        OPTION -- optional.  
  ;"          OPTION("FOR PATIENTS")=1
  ;"NOTE: Later we will also fix spelling in FIXSPLNG^TMGRX004(IEN22733,.LINE), but we have to get IEN first (done later)
  NEW FORPTS SET FORPTS=($GET(OPTION("FOR PATIENTS"))=1)
  DO FIXARRWS^TMGRX004(.LINE)       ;"FIX ARROWS
  SET OUT("ORIG")=LINE
  IF FORPTS DO 
  . ;"DO FIXRTARROW^TMGRX004(.LINE)  ;"Change '-->' to words 'CHANGE TO'
  . DO DELAUTOADD^TMGRX004(.LINE)  ;" Remove '[Auto added AUG 02, 2024]'
  SET LINE=$$UP^XLFSTR(LINE)
  DO XTRCTNTE^TMGRX004(.OUT,.LINE)  ;"EXTRACT NOTE, IF ANY
  DO FIXDBLSP^TMGRX004(.LINE)       ;"FIX DOUBLE SPACES
  DO CHKPREFX^TMGRX004(.OUT,.LINE,.OPTION)  ;"CHECK AND REMOVE PREFIXES (INCLUDING OTC)
  IF LINE["_?_STRENGTH" SET LINE=$$REPLSTR^TMGSTUT3(LINE,"_?_STRENGTH","")  
  SET OUT("WORKING")=LINE
  QUIT
  ;
FIXINFO(OUT,OPTION)  ;"FIX INFORMATION ARRAY
  ;"Input: OUT -- PASS BY REFERENCE, AN IN AND OUT PARAMETER
  ;"RESULT: None  
  NEW FORPTS SET FORPTS=($GET(OPTION("FOR PATIENTS"))=1)
  NEW STRENGTHIENS SET STRENGTHIENS=$ORDER(OUT("STRENGTH","IENS",""))
  IF STRENGTHIENS'="",$ORDER(ARR("STRENGTH","IENS",STRENGTHIENS))'="" DO  ;"multiple STRENGTH IENS's present
  . SET STRENGTHIENS=$$MOD2FRM^TMGRX004(.OUT)
  . KILL ARR("STRENGTH","IENS") QUIT:STRENGTHIENS=""
  . NEW FORMIENS SET FORMIENS=$PIECE(STRENGTHIENS,",",2,3)_","
  . SET ARR("STRENGTH","IENS",STRENGTHIENS)=$$GET1^DIQ(22733.02,FORMIENS,.01)
  IF $GET(OUT("FORM","SUBIEN"))="",(STRENGTHIENS'="") DO
  . SET OUT("FORM","SUBIEN")=+$PIECE(STRENGTHIENS,",",2)  
  ; 
  NEW IEN22733 SET IEN22733=+$GET(OUT("IEN22733"))
  IF ($GET(OUT("MEDICATION","GENERIC","DATABASE"))=""),(IEN22733>0) DO
  . SET OUT("MEDICATION","GENERIC","DATABASE")=$PIECE($GET(^TMG(22733,IEN22733,0)),"^",1)
  ;
  SET OUT("MEDICATION","GENERIC ALIAS","PREFERRED")=$$GETPRGA^TMGRX004(IEN22733)  ;"GET PREFERRED GENERIC ALIAS
  SET OUT("MEDICATION","GENERIC ABBRV","PREFERRED")=$$GETPRGB^TMGRX004(IEN22733)  ;"GET PREFERRED ABBREVIATION FOR RX.  
  IF $GET(OUT("MEDICATION","BRAND","PREFERRED"))="" DO
  . SET OUT("MEDICATION","BRAND","PREFERRED")=$$GETPRBRD^TMGRX004(IEN22733)  ;"GET PREFERRED BRAND NAME
  ;
  NEW FORMIEN SET FORMIEN=+$GET(OUT("FORM","SUBIEN"))
  IF FORMIEN'>0 DO   ;"E.g. EpiPen PRN  -- only one form, so form not specified
  . NEW FORMS,IEN SET (FORMS,IEN)=0
  . FOR  SET IEN=$ORDER(^TMG(22733,IEN22733,2,IEN)) QUIT:IEN'>0  DO
  . . SET FORMS=+$GET(FORMS)+1,FORMS(FORMS)=IEN
  . IF FORMS=1 SET FORMIEN=FORMS(1)  ;"<--- later, consider picking first even if there is more than one form defined
  NEW IEN50D607 SET IEN50D607=+$GET(OUT("UNITS","IEN50.607"))
  IF (IEN50D607'>0),(STRENGTHIENS'="") DO 
  . SET IEN50D607=+$$GET1^DIQ(22733.03,STRENGTHIENS,.02,"I")
  . SET OUT("UNITS","IEN50.607")=IEN50D607
  IF $GET(OUT("UNITS","DATABASE"))="" DO
  . SET OUT("UNITS","DATABASE")=$PIECE($GET(^PS(50.607,IEN50D607,0)),"^",1)
  IF $GET(OUT("UNITS","PREFERRED"))="" DO
  . SET OUT("UNITS","PREFERRED")=$$GETPRUNT^TMGRX004(IEN22733,FORMIEN,+STRENGTHIENS,0) ;"GET PREFERRED UNITS FOR FORM
  ;  
  SET OUT("FORM","DATABASE")=$$GET1^DIQ(22733.02,FORMIEN_","_IEN22733_",",.01)
  SET OUT("FORM","PREFERRED")=$$GETPRFAL^TMGRX004(IEN22733,FORMIEN) ;"GET PREFERRED FORM ALIAS
  ; 
  IF ($GET(OUT("MODIFIER","PREFERRED"))=""),(FORMIEN>0) DO
  . SET OUT("MODIFIER","PREFERRED")=$$GETPMOD2^TMGRX004(IEN22733,FORMIEN) 
  ;
  NEW SIG SET SIG=$GET(OUT("SIG"))
  NEW FORPT SET FORPT=($GET(OPTION("FOR PATIENTS"))=1)  
  IF SIG'="" DO  ;"Get back non-UPPERCASE sig
  . NEW ORIG SET ORIG=$GET(OUT("ORIG"))  ;"This is mixed case 
  . NEW POS SET POS=$$POS^TMGSTUT3(SIG,$$UP^XLFSTR(ORIG)) 
  . IF POS>0 SET SIG=$EXTRACT(ORIG,POS,POS+$LENGTH(SIG)-1)
  . IF FORPT SET SIG=$$UP^XLFSTR(SIG)
  . SET SIG=$$FIXSIG(SIG) ;"FIX UP THE SIG PART OF LINE
  . SET OUT("SIG")=SIG  
  ;
  NEW NOTES SET NOTES=$GET(OUT("NOTE"))
  IF NOTES'="" DO  ;"Get back non-UPPERCASE notes
  . NEW ORIG SET ORIG=$GET(OUT("ORIG"))  ;"This is mixed case 
  . NEW POS SET POS=$$POS^TMGSTUT3(NOTES,$$UP^XLFSTR(ORIG)) QUIT:POS'>0
  . SET NOTES=$EXTRACT(ORIG,POS,POS+$LENGTH(NOTES))
  . SET OUT("NOTE")=NOTES
  ; 
  NEW CLARR DO GTCLARR^TMGRXU01(.CLARR,IEN22733) MERGE OUT("DRUG CLASS")=CLARR
  QUIT
  ;
  ;  
XTRCTDAT(OUT,SUMRY,OPTION) ;"Extract data based on summary
  ;"Input: OUT -- PASS BY REFERENCE.  AN OUT PARAMETER.  Format:
  ;"         OUT("IEN22733")=<IEN 22733>
  ;"         OUT("PREFACE")= e.g. HOLD, HOLDING, OFF
  ;"         OUT("OTC")=1  <-- if specified as OTC
  ;"         OUT("WORKING")=The input line, with preface removed and arrows fixed  
  ;"         OUT("MEDICATION","INPUT NAME")=<NAME OF RX>  <-- last Rx name encountered on line (input name)
  ;"         OUT("MEDICATION","GENERIC")=<NAME OF RX> <-- Generic name as found on input line (if provided)  
  ;"         OUT("MEDICATION","GENERIC","DATABASE") <-- Generic name based on IEN22733:.01 field
  ;"         OUT("MEDICATION","GENERIC ALIAS")=SOURCE <-- Generic alias name, as found on input line (if provided)  
  ;"         OUT("MEDICATION","GENERIC ABBRV")=SOURCE <-- Generic abbreviation, as found on input line (if provided)
  ;"         OUT("MEDICATION","BRAND")=<NAME OF RX> <-- Brand name, as found on input line (if provided)
  ;"         OUT("MEDICATION","BRAND","PREFERRED")=<Preferred brand name from database> (if brand provided)    
  ;"         OUT("STRENGTH","IENS",<IENS>)=<STRENGTH_NAME> , e.g. TAB
  ;"         OUT("STRENGTH")=<STRENGTH>  <-- strength as found on input line (if provided)
  ;"         OUT("STRENGTH","DATABASE")=<registered strength strength for last IENS found> <-- if found.   
  ;"         OUT("STRENGTH","IENS",<IENS IN 22733.03>)=<FORM>^<Strength>^<Preferred Alias>  <-- may be multiple. 
  ;"         OUT("UNITS")=E.G. "MG"  <-- units as found on input line (if provided)
  ;"         OUT("UNITS","IEN50.607")=#  <-- this is UNITS IEN (if units provided)
  ;"         OUT("FORM")=<FORM> <-- form as found on input line (if provided)
  ;"         OUT("FORM","SUBIEN")=<#>  <-- 
  ;"         OUT("MODIFIER")=<modifier> <-- modifier as found in input line (if provided)  
  ;"         OUT("MODIFIER","PREFERRED")=<PREFERRED MODIFIER, IN SAME GROUP AS MOD>  (if modifier provided)  
  ;"         OUT("SIG")="AS NEEDED"
  ;"      SUMRY -- Source data structure
  ;"      OPTION -- OPTIONAL.  Contains options. 
  ;"Result: None
  NEW IEN22733 SET IEN22733=+$GET(OUT("IEN22733"))
  IF IEN22733>0 SET OUT("MEDICATION","GENERIC","DATABASE")=$$GET1^DIQ(22733,IEN22733_",",.01)
  NEW LINE SET LINE=OUT("WORKING")
  NEW DONE SET DONE=0
  NEW LOSTWORDS
  NEW DELTAFOUND SET DELTAFOUND=0
  NEW STRENGTHINFO
  NEW MATCHSUMRY SET MATCHSUMRY=$GET(SUMRY("MATCH"))
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(SUMRY(IDX)) QUIT:(IDX'>0)!(DONE)  DO
  . NEW WORD MERGE WORD=SUMRY(IDX) SET WORD=$GET(WORD)
  . NEW SOURCE SET SOURCE=$GET(WORD("SOURCE"))
  . NEW DBSOURCE MERGE DBSOURCE=WORD("DBSOURCE")
  . NEW HANDLED SET HANDLED=0
  . IF WORD="{{DRUG_GENERIC}}" DO  QUIT:HANDLED
  . . IF $$CHKWORD(.MATCHSUMRY,.WORD,.HANDLED,.OUT,DELTAFOUND,.OPTION)=0 QUIT
  . . IF $GET(OUT("MEDICATION","GENERIC"))'="" QUIT  ;"ONLY USE 1ST VALUE 
  . . SET OUT("MEDICATION","GENERIC")=SOURCE
  . . IF SOURCE'="" SET OUT("MEDICATION","INPUT NAME")=SOURCE
  . IF WORD="{{DRUG_ALIAS}}" DO  QUIT:HANDLED
  . . IF $$CHKWORD(.MATCHSUMRY,.WORD,.HANDLED,.OUT,DELTAFOUND,.OPTION)=0 QUIT
  . . IF $GET(OUT("MEDICATION","GENERIC ALIAS"))'="" QUIT  ;"ONLY USE 1ST VALUE 
  . . SET OUT("MEDICATION","GENERIC ALIAS")=SOURCE   
  . . IF SOURCE'="" SET OUT("MEDICATION","INPUT NAME")=SOURCE  ;"<-- last Rx name encountered on input line
  . IF WORD="{{DRUG_ABBREV}}" DO  QUIT:HANDLED
  . . IF $$CHKWORD(.MATCHSUMRY,.WORD,.HANDLED,.OUT,DELTAFOUND,.OPTION)=0 QUIT
  . . IF $GET(OUT("MEDICATION","GENERIC ABBRV"))'="" QUIT  ;"ONLY USE 1ST VALUE 
  . . SET OUT("MEDICATION","GENERIC ABBRV")=SOURCE
  . . IF SOURCE'="" SET OUT("MEDICATION","INPUT NAME")=SOURCE
  . IF WORD="{{DRUG_BRAND}}" DO  QUIT:HANDLED
  . . IF $$CHKWORD(.MATCHSUMRY,.WORD,.HANDLED,.OUT,DELTAFOUND,.OPTION)=0 QUIT
  . . IF $GET(OUT("MEDICATION","BRAND"))'="" QUIT  ;"ONLY USE 1ST VALUE 
  . . SET OUT("MEDICATION","BRAND")=SOURCE
  . . SET OUT("MEDICATION","BRAND","PREFERRED")=$$GETPRBRD^TMGRX004(IEN22733)  ;"GET PREFERRED BRAND NAME    
  . . IF SOURCE'="" SET OUT("MEDICATION","INPUT NAME")=SOURCE
  . IF (WORD="{{STRENGTH}}")!(WORD="{{STRENGTH_ALIAS}}") DO  QUIT:HANDLED
  . . MERGE STRENGTHINFO(IDX)=WORD
  . . IF $$CHKWORD(.MATCHSUMRY,.WORD,.HANDLED,.OUT,DELTAFOUND,.OPTION)=0 QUIT
  . . NEW DUPL,DUPOK SET DUPL=($GET(OUT("STRENGTH"))'=""),DUPOK=0
  . . IF DUPL SET DUPOK=$$GETALLOWMULT^TMGRX004(.DBSOURCE)
  . . IF DELTAFOUND=0,DUPL,(DUPOK=0) QUIT  ;"ONLY USE 1ST VALUE 
  . . IF DELTAFOUND DO
  . . . SET OUT("STRENGTH")=$GET(OUT("STRENGTH"))_" CHANGED TO "_SOURCE
  . . ELSE  DO
  . . . IF DUPL,DUPOK DO
  . . . . NEW TEMPDIV SET TEMPDIV=", "
  . . . . NEW PRIORIDX SET PRIORIDX=$ORDER(STRENGTHINFO(IDX),-1)
  . . . . IF IDX-PRIORIDX=2 DO
  . . . . . IF $GET(SUMRY(IDX-1))="",$GET(SUMRY(IDX-1,"DIV"))'="" DO
  . . . . . . SET TEMPDIV=SUMRY(IDX-1,"DIV")
  . . . . SET SOURCE=OUT("STRENGTH")_TEMPDIV_SOURCE
  . . . SET OUT("STRENGTH")=SOURCE
  . . ;"SET OUT("SIG")=$GET(WORD("DIV"))_$$ARR2LN3^TMGRX004("SUMRY",IDX+.1,.OPTION)
  . . DO MTCHSTRT^TMGRX004(.OUT,.WORD)  ;"TRY TO MATCH STRENGTHS TO DATABASE. 
  . IF WORD="{{MODIFIER}}" DO  QUIT:HANDLED
  . . IF $$CHKWORD(.MATCHSUMRY,.WORD,.HANDLED,.OUT,DELTAFOUND,.OPTION)=0 QUIT
  . . IF $GET(OUT("MODIFIER"))'="" QUIT  ;"ONLY USE 1ST VALUE 
  . . SET OUT("MODIFIER")=SOURCE  
  . . SET OUT("MODIFIER","PREFERRED")=$$GETPMOD1^TMGRX004(IEN22733,SOURCE)  ;"GET PREFERRED MODIFIER, IN SAME GROUP AS MOD  
  . IF (WORD="{{FORM}}")!(WORD="{{FORM_ALIAS}}") DO  QUIT:HANDLED
  . . IF $$CHKWORD(.MATCHSUMRY,.WORD,.HANDLED,.OUT,DELTAFOUND,.OPTION)=0 QUIT
  . . IF $GET(OUT("FORM"))'="" QUIT  ;"ONLY USE 1ST VALUE 
  . . SET OUT("FORM")=SOURCE
  . . SET OUT("FORM","SUBIEN")=+$$GTFRMIEN^TMGRX004(IEN22733,SOURCE)  ;"GET FORM SUBIEN BASED ON INPUT FORM
  . IF (WORD="{{UNIT}}")!(WORD="{{UNIT_ALIAS}}") DO  QUIT:HANDLED
  . . IF $$CHKWORD(.MATCHSUMRY,.WORD,.HANDLED,.OUT,DELTAFOUND,.OPTION)=0 QUIT
  . . IF $GET(OUT("UNITS"))'="" QUIT  ;"ONLY USE 1ST VALUE 
  . . SET OUT("UNITS")=SOURCE
  . . SET OUT("UNITS","IEN50.607")=$GET(WORD("UNIT IEN"))
  . . DO MATCHUNT^TMGRX004(.OUT,.WORD)
  . IF WORD="{{ROUTE}}" DO  QUIT:HANDLED
  . . IF $$CHKWORD(.MATCHSUMRY,.WORD,.HANDLED,.OUT,DELTAFOUND,.OPTION)=0 QUIT
  . . IF $GET(OUT("SIG","ROUT"))'="" QUIT  ;"ONLY USE 1ST VALUE 
  . . SET OUT("SIG","ROUTE")=SOURCE
  . IF WORD="{{FREQ}}" DO  QUIT:HANDLED
  . . IF $$CHKWORD(.MATCHSUMRY,.WORD,.HANDLED,.OUT,DELTAFOUND,.OPTION)=0 QUIT
  . . IF $GET(OUT("SIG","FREG"))'="" QUIT  ;"ONLY USE 1ST VALUE 
  . . SET OUT("SIG","FREQ")=SOURCE
  . IF WORD="{{DELTA}}" DO  QUIT:HANDLED
  . . ;"Manage Delta in dose
  . . ;"Example: BUMETANIDE 2 MG PO DAILY --> 2 IN AM AND 1 MG (1/2 TAB) IN THE AFTERNOON
  . . ;"        This is a change in SIG, not a change in dose.  But how to distinguish??
  . . ;"       I am going to use test of only 1 number (strength) found after delta
  . . NEW STRENGTHCT SET STRENGTHCT=0
  . . NEW JDX SET JDX=IDX
  . . FOR  SET JDX=$ORDER(SUMRY(JDX)) QUIT:(JDX'>0)  DO
  . . . NEW WORD2 SET WORD2=$GET(SUMRY(JDX))
  . . . IF (WORD2="{{STRENGTH}}")!(WORD2="{{STRENGTH_ALIAS}}") SET STRENGTHCT=STRENGTHCT+1
  . . SET HANDLED=1
  . . IF STRENGTHCT'=1 QUIT
  . . SET DELTAFOUND=1
  . IF WORD'["{{" DO  QUIT:HANDLED
  . . NEW JDX SET JDX=+$GET(LOSTWORDS)+1
  . . MERGE LOSTWORDS(JDX)=WORD
  . . SET LOSTWORDS=JDX
  . ELSE  DO  QUIT:HANDLED  ;"match, remove, and ignore other items. 
  . . IF $$CHKWORD(.MATCHSUMRY,.WORD,.HANDLED,.OUT,DELTAFOUND,.OPTION)=0 QUIT
  SET OUT("SIG")=$$TRIMDIVS($GET(OUT("SIG")),"L")
  SET OUT("PREFACE")=$$TRIMDIVS($GET(OUT("PREFACE")),"R")
  QUIT
  ;
CHKWORD(MATCHSUMRY,WORD,HANDLED,OUT,DELTAFOUND,OPTION) ;"Check if WORD is still part of match summary, convert WORD otherwise. 
  NEW FORPTS SET FORPTS=($GET(OPTION("FOR PATIENTS"))=1)
  NEW RESULT SET RESULT=0
  IF ($PIECE(MATCHSUMRY," ",1)'=WORD),DELTAFOUND=0 DO  GOTO CKWDN
  . SET WORD=$GET(WORD("SOURCE"))
  IF DELTAFOUND=0 DO
  . ;"NOTE: When DELTAFOUND, then often have extra Rx info found at end of SIG, so will treat as if back in Rx match
  . NEW SIG SET SIG=$GET(WORD("DIV"))_$$ARR2LN3^TMGRX004("SUMRY",IDX+.1,.OPTION)
  . IF FORPTS DO
  . . DO FIXRTARROW^TMGRX004(.SIG)
  . SET OUT("SIG")=SIG
  SET MATCHSUMRY=$PIECE(MATCHSUMRY," ",2,999)
  SET HANDLED=1
  SET RESULT=1
CKWDN ;  
  QUIT RESULT
  ;
PARS2SUM(INFO,SUMRY,TRAIN,OPTION)  ;"PARSE TO SUMMARY
  ;"INPUT:  INFO -- PASS BY REFERENCE.  Contains many objects. 
  ;"             Must hold INFO("WORKING")=LINE
  ;"        SUMRY -- PASS BY REFERENCE.  AN OUT PARAMETER.  See GETSUMRY
  ;"       TRAIN -- OPTIONAL.  If 1 then interactive process if needed. 
  ;"                           2 is ALWAYS interactive
  ;"        OPTION -- optional.  
  ;"          OPTION("FOR PATIENTS")=1
  ;"RESULT: 1 if successful, 0 if not
  NEW RESULT,LINE SET LINE=INFO("WORKING")
  SET INFO("IEN22733")=$$GETMDIEN^TMGRX004(LINE)  ;"GET MEDICATION IEN (IEN22733) FROM LINE
  IF INFO("IEN22733")'>0 SET RESULT=0 GOTO P2SDN  
  SET OPTION("PARSESIG")=1
  DO GETSUMRY(.SUMRY,LINE,INFO("IEN22733"),.OPTION)  ;"GET A SUMMARY OF LINE
  DO RMVPREFX(.INFO,.SUMRY)  ;"REMOVE PREFIX FROM SUMMARY ARRAY
  SET RESULT=$$VERFYSUM(.SUMRY,+$GET(TRAIN),INFO("WORKING"),.OPTION)
P2SDN ;  
  QUIT RESULT
  ;
GETSUMRY(SUMRY,LINE,IEN22733,OPTION)  ;"GET A SUMMARY OF LINE
  ;"INPUT:  SUMRY -- PASS BY REFERENCE.  AN OUT PARAMETER.  FORMAT:    
  ;"           SUMRY={{DRUG_BRAND}} {{STRENGTH_ALIAS}} {{UNIT_ALIAS}} ONCE {{FREQ}}                     
  ;"           }~1 = {{DRUG_BRAND}}                                                            
  ;"           | }~"DIV" = " "                                                                 
  ;"           | }~"SOURCE" = ANORO ELLIPTA                                                    
  ;"           }~2 = {{STRENGTH_ALIAS}}                                                            
  ;"           | }~"DIV" = " "                                                                 
  ;"           | }~"SOURCE" = 62.5MG/25                                                        
  ;"           }~3 = {{UNIT_ALIAS}}                                                            
  ;"           | }~DBSOURCE                                                                    
  ;"           | | }~"22733.32^.01^1,2,1,215," = ""                                            
  ;"           | }~"DIV" = " "                                                                 
  ;"           | }~"SOURCE" = MG                       
  ;"           | }~"UNIT IEN" = 20                                                      
  ;"           }~4 = ONCE                    
  ;"           | }~"DIV" = " "                    
  ;"           }~5 = {{FREQ}}                    
  ;"           | }~"DIV" = " "                    
  ;"           | }~"SOURCE" = DAILY                    
  ;"           }~"ORIG" = "ANORO ELLIPTA 62.5MG/25 MG ONCE DAILY " 
  ;"        LINE  -- THE SOURCE LINE TO PROCESS
  ;"        IEN22733 -- THE IEN OF THE DRUG TO WORK WITH
  ;"        PARSESIG -- 0 OR 1, if 1 then parsing in sig is done.  
  ;"        OPTION -- optional.  
  ;"          OPTION("FOR PATIENTS")=1
  ;"          OPTION("PARSESIG")=1
  ;"RESULT: none
  NEW DICT DO GETDICT^TMGRX004(.DICT,IEN22733)
  DO FIXSPLNG^TMGRX004(IEN22733,.LINE)
  SET SUMRY("ORIG")=LINE
  NEW WORD,LINEARR,REFNUM SET REFNUM=1
  DO LINE2ARR^TMGRX004(.LINEARR,LINE,.DICT,.OPTION) ;"Parse LINE into LINEARR
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(LINEARR("WORD",IDX)) QUIT:IDX=""  DO
  . KILL WORD MERGE WORD=LINEARR("WORD",IDX) SET WORD=$GET(WORD)
  . NEW TYPE SET TYPE=$GET(WORD("TYPE"))
  . IF TYPE="" SET TYPE=$$FINDTYPE^TMGRX004(.WORD,.DICT,.OPTION,$NAME(LINEARR("WORD")),.IDX) ;"FIND A TYPE FOR WORD
  . IF TYPE="" QUIT
  . SET LINEARR("WORD",IDX)="{{"_TYPE_"}}"
  . SET LINEARR("WORD",IDX,"SOURCE")=WORD
  MERGE SUMRY=LINEARR("WORD")
  SET SUMRY=$$ARR2LINE^TMGRX004(.LINEARR)
  QUIT 
  ;
RMVPREFX(OUT,SUMRY)  ;"REMOVE PREFACE FROM SUMARY ARRAY
  ;"NOTE: Everything that comes before first {{xxx}} code should be put into OUT("PREFIX")
  NEW PREARR
  NEW IDX,DONE SET (IDX,DONE)=0
  FOR  SET IDX=$ORDER(SUMRY(IDX)) QUIT:(IDX'>0)!DONE  DO
  . NEW WORD SET WORD=$GET(SUMRY(IDX))
  . IF $EXTRACT(WORD,1,2)="{{" SET DONE=1 QUIT
  . MERGE PREARR(IDX)=SUMRY(IDX)
  . KILL SUMRY(IDX)
  NEW PREFIX SET PREFIX=$$ARR2LN2^TMGRX004("PREARR")
  IF $GET(OUT("PREFACE"))'="" SET OUT("PREFACE")=OUT("PREFACE")_" "
  SET OUT("PREFACE")=$GET(OUT("PREFACE"))_PREFIX  
  QUIT
  ;  
VERFYSUM(SUMRY,TRAIN,LINE,OPTION)  ;
  ;"INPUT: SUMRY -- PASS BY REFERENCE.  The array, as created by PARS2SUM
  ;"       TRAIN -- OPTIONAL.  If 1 then interactive process if needed. 
  ;"                           2 is ALWAYS interactive
  ;"       LINE -- OPTIONAL IF TRAIN NOT 1. REQUIRED IF TRAIN=1.  Line being parsed
  ;"RESULT: 1 if successful, 0 if not, -1 if ABORT
  NEW RESULT SET RESULT=0
  NEW MATCH,VALUE SET VALUE=""
  NEW DONE SET DONE=0
  NEW TEMPSUMRY MERGE TEMPSUMRY=SUMRY
  NEW STRENGTHSEEN SET STRENGTHSEEN=""
  NEW SHOWNMATCHLEN SET SHOWNMATCHLEN=0 
  NEW EDITED SET EDITED=0
  DO NORMLSUM(.TEMPSUMRY,.OPTION)  ;"NORMALIZE SUMMARY into standard text output
VL1 ;  
  SET MATCH=$$LONGESTM(TEMPSUMRY)  ;"GET LONGEST MATCH FROM 22733.1  
  NEW MATCHARR DO SPLIT2AR^TMGSTUT2(MATCH," ",.MATCHARR)
  SET RESULT=(MATCH'="")
  IF $GET(TRAIN)'>0 GOTO VFPTDN
  IF (TEMPSUMRY=MATCH),(TRAIN<2) GOTO VFPTDN
  IF EDITED WRITE ! GOTO VL2  
  WRITE !,"====================================================",!
  WRITE "TRAINING MODE: ON.  Verifying when no exact matches.",!
  WRITE "ORIG: ",LINE,!
  DO SHOWSUMRY(.TEMPSUMRY,.MATCHARR) 
VL2 ;  
  NEW HASSTRENGTH SET HASSTRENGTH=((TEMPSUMRY["{{STRENGTH}}")!(TEMPSUMRY["{{STRENGTH_ALIAS}}"))
  IF 'HASSTRENGTH DO  GOTO:(+RESULT=-1) VFPTDN
  . IF STRENGTHSEEN'="" SET %=STRENGTHSEEN  ;"prevent asking same question twice after loopback.  
  . ELSE  DO
  . . WRITE !,"NOTE: strength was not parsed.  Do you see strength in ORIG"   
  . . SET %=2 DO YN^DICN WRITE !
  . . SET STRENGTHSEEN=%
  . IF %=1 SET RESULT="-1^MISSING STRENGTH",MATCH=TEMPSUMRY
  . IF %=-1 SET RESULT="-1^ABORT"
  . WRITE ! DO SHOWSUMRY(.TEMPSUMRY,.MATCHARR) 
  SET %=0 
  IF EDITED,($LENGTH(MATCH)=SHOWNMATCHLEN),(VALUE'="") GOTO VL3
  IF MATCH'="",($LENGTH(MATCH)>SHOWNMATCHLEN) DO
  . NEW DIVCOL SET DIVCOL=$LENGTH(MATCH)+9
  . WRITE ?DIVCOL,"|",!
  . NEW STR SET STR="<--Rx match part-->"
  . WRITE ?(DIVCOL-$LENGTH(STR)),STR,"|<--SIG part-->",!
  . WRITE ?DIVCOL,"|",!
  . WRITE "MATCH:   ",MATCH,!,!
  . SET SHOWNMATCHLEN=$LENGTH(MATCH)
  . WRITE "This MATCH would have been used if not in training mode.",!,!
  . WRITE "NOTE: the Match is NOT supposed include: Instructions, purpose,",!
  . WRITE "      notes, route, or frequency.",!
  . WRITE "      SHOULD be just to match the RX, NOT how it is to be used.",!
  . WRITE "      SHOULD have Rx name, form, stregth, modifiers etc.",!,!
  . SET %=1 
  . WRITE "Would MATCH have been OK to identify Med but NOT SIG? Enter NO if insufficient or needs editing.",!
  . WRITE "Is MATCH OK" DO YN^DICN WRITE !
  IF %=-1 SET RESULT="-1^ABORT" GOTO VFPTDN
  IF %=1 GOTO VFPTDN
  SET %=2 
  WRITE "Does SUMMARY above have any parsing errors. Having extra parts is OK.  Enter YES if needs edit.",! 
  WRITE "Any SUMMARY ERRORS" DO YN^DICN WRITE !
  IF %=-1 SET RESULT=-1 GOTO VFPTDN
  IF %=1 SET RESULT=0 GOTO VFPTDN               
  WRITE !,"DELETE invalid or non-universal part of summary",!
  WRITE "EDIT: " SET VALUE=$$EDITBOX^TMGUSRI6(TEMPSUMRY,120) WRITE !
  SET VALUE=$$TRIM^XLFSTR(VALUE)
  IF VALUE'=TEMPSUMRY SET TEMPSUMRY=VALUE,EDITED=1 GOTO VL1
VL3 ;  
  SET RESULT=$$SAVESUMRY(VALUE)
VFPTDN ;  
  SET SUMRY("MATCH")=MATCH
  QUIT RESULT
  ;
SAVESUMRY(VALUE)  ;
  NEW RESULT SET RESULT=0
  NEW IEN SET IEN=0
  NEW FOUND SET FOUND=0
  FOR  SET IEN=$ORDER(^TMG(22733.1,IEN)) QUIT:(IEN'>0)!FOUND  DO
  . NEW ZN SET ZN=$GET(^TMG(22733.1,IEN,0)) QUIT:ZN=""
  . SET FOUND=($PIECE(ZN,"^",1)=VALUE)
  IF FOUND DO  GOTO SSDN
  . SET RESULT=1
  . WRITE !,"Patterns has been found before, and is already saved.",!
  . DO PRESS2GO^TMGUSRI2
  WRITE !,"Pattern has NOT been found before.",!
  NEW % SET %=2 
  WRITE "SAVE: ",VALUE,":" DO YN^DICN WRITE !
  SET RESULT=$SELECT(%=-1:-1,%=2:0,1:1)
  IF %'=1 GOTO SSDN
  NEW TMGFDA,TMGIEN,TMGMSG
  SET TMGFDA(22733.1,"+1,",.01)=VALUE
  DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO VFPTDN
  . WRITE "ERROR:",!,$$GETERRST^TMGDEBU2(.TMGMSG)
  SET RESULT=1
SSDN ;
  QUIT RESULT

SHOWSUMRY(SUMRY,MATCHARR)  ;
  NEW IDX,JDX,PASTMATCH SET IDX=0,JDX=1,PASTMATCH=0
  FOR  SET IDX=$ORDER(SUMRY(IDX)) QUIT:IDX'>0  DO
  . NEW WORD SET WORD=$GET(SUMRY(IDX)) 
  . IF WORD'["{{" QUIT
  . IF PASTMATCH=1 DO
  . . SET PASTMATCH=0
  . . WRITE "------Start of SIG part---------",!
  . IF $GET(MATCHARR(JDX))=WORD SET JDX=JDX+1
  . WRITE WORD," <-- ",$GET(TEMPSUMRY(IDX,"SOURCE"),"?"),!
  . IF JDX>$GET(MATCHARR("MAXNODE")) SET PASTMATCH=1,JDX=0
  WRITE "SUMMARY: ",TEMPSUMRY,!
  QUIT
  ;
NORMLSUM(SUMRY,OPTION)  ;"NORMALIZE SUMMARY
  NEW KILLIDX SET KILLIDX=0
  NEW IDX SET IDX=0
  ;"Kill all array members that are not {{xxx}} type words
  FOR  SET IDX=$ORDER(SUMRY(IDX)) QUIT:(IDX'>0)!(KILLIDX>0)  DO
  . NEW WORD SET WORD=$GET(SUMRY(IDX))
  . IF (WORD'["{{")&(WORD'="") KILL SUMRY(IDX)
  . ;"IF KILLIDX=0,($$ISROUTE^TMGRX004(WORD)!$$ISFREQ^TMGRX004(WORD,.SUMRY,.IDX)) DO
  . ;". ;"SET KILLIDX=IDX  <-- disable because of 'VITAMIN D PO 50,000 IU EVERY OTHER WEEK.'
  ;"Kill anything after FREQ or ROUTE entries, as those should be part of SIG
  IF KILLIDX>0 SET IDX=KILLIDX-0.1 FOR  SET IDX=$ORDER(SUMRY(IDX)) QUIT:IDX'>0  DO
  . KILL SUMRY(IDX)  
  SET IDX=0 FOR  SET IDX=$ORDER(SUMRY(IDX)) QUIT:(IDX'>0)  SET SUMRY(IDX,"DIV")=" "    
  SET SUMRY=$$TRIM^XLFSTR($$ARR2LN2^TMGRX004("SUMRY"))
  DO FIXDBLSP^TMGRX004(.SUMRY) ;"REMOVE DOUBLE SPACES
  QUIT
  ;  
GETLMCHA(OUT,STR)  ;"GET ARRAY OF LEFT MATCHES
  NEW FIRSTWORD SET FIRSTWORD=$PIECE(STR," ",1)
  NEW DONE SET DONE=0
  NEW ASUMRY SET ASUMRY=$EXTRACT(FIRSTWORD,1,$LENGTH(FIRSTWORD)-1)_"Z"
  FOR  SET ASUMRY=$ORDER(^TMG(22733.1,"B",ASUMRY)) QUIT:(ASUMRY="")!DONE  DO
  . IF $PIECE(ASUMRY," ",1)'=FIRSTWORD SET DONE=1 QUIT
  . IF $$LMATCH^TMGSTUT3(STR,ASUMRY)=0 QUIT
  . NEW IEN SET IEN=0
  . FOR  SET IEN=$ORDER(^TMG(22733.1,"B",ASUMRY,IEN)) QUIT:IEN'>0  DO
  . . NEW FULLSUM SET FULLSUM=$PIECE($GET(^TMG(22733.1,IEN,0)),"^",1)
  . . IF $$LMATCH^TMGSTUT3(STR,FULLSUM)=0 QUIT
  . . SET OUT($LENGTH(FULLSUM),FULLSUM)=""  ;"but keep searching for a longer match
  QUIT
  ;
LONGESTM(STR)  ;"GET LONGEST MATCH FROM 22733.1  
  NEW MATCH
  DO GETLMCHA(.MATCH,STR)
  NEW MAXLEN SET MAXLEN=$ORDER(MATCH(""),-1)
  SET MATCH=$ORDER(MATCH(MAXLEN,""))
  QUIT MATCH
  ;
TRIMDIVS(STR,MODE)  ;"TRIM DIVIDER CHARACTERS FROM START (L) OR END (R) OF STRING
  NEW RESULT SET RESULT=STR
  NEW DIVSET SET DIVSET=$$SUBDVSET^TMGRX004()
  SET RESULT=$$TRIM^XLFSTR(RESULT) IF RESULT="" GOTO TRDVDN
  SET MODE=$GET(MODE)
  NEW DONE SET DONE=0
  IF MODE["R" DO
  . FOR  DO  QUIT:DONE    ;"//trim off any trailing divider characters
  . . NEW LASTCH SET LASTCH=$EXTRACT(RESULT,$LENGTH(RESULT)) IF LASTCH="" SET DONE=1 QUIT
  . . SET DONE=(DIVSET'[LASTCH)
  . . QUIT:DONE
  . . SET RESULT=$EXTRACT(RESULT,1,$LENGTH(RESULT)-1)
  . . SET RESULT=$$TRIM^XLFSTR(RESULT)
  . . IF RESULT="" SET DONE=1
  IF MODE["L" DO
  . FOR  DO  QUIT:DONE    ;"//trim off any leading divider characters
  . . IF RESULT="" SET DONE=1 QUIT
  . . NEW FIRSTCH SET FIRSTCH=$EXTRACT(RESULT,1)
  . . SET DONE=(DIVSET'[FIRSTCH)
  . . QUIT:DONE
  . . SET RESULT=$EXTRACT(RESULT,2,$LENGTH(RESULT))
  . . SET RESULT=$$TRIM^XLFSTR(RESULT)
  . . IF RESULT="" SET DONE=1
TRDVDN  ;
  QUIT RESULT
  ;  
FIXSIG(SIG) ;"FIX UP THE SIG PART OF LINE
  SET SIG=$GET(SIG)
  NEW ARR DO LINE2ARR^TMGRX004(.ARR,SIG)
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(ARR("WORD",IDX)) QUIT:IDX'>0  DO
  . NEW WORD SET WORD=$GET(ARR("WORD",IDX))
  . IF WORD'["{{" QUIT
  . SET WORD=$PIECE($PIECE(WORD,"}}",1),"{{",2)
  . NEW NEWWORD SET NEWWORD=$GET(SIG(WORD))
  . IF NEWWORD="" QUIT
  . SET ARR("WORD",IDX)=NEWWORD
  SET SIG=$$ARR2LINE^TMGRX004(.ARR)
  QUIT SIG
  ;    
