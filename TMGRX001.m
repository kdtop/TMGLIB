TMGRX001 ;TMG/kst/Patient medication code; 08/23/17
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
 ;"PARSELN(OUT,LINE,TRAIN) ;"PARSE ONE MED LINE
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"PARS2SUM(OUT,SUMRY,TRAIN)  ;"PARSE TO SUMMARY
 ;"GETSUMRY(ARR,LINE,IEN22733,PARSESIG)  ;"GET A SUMMARY OF LINE
 ;"VERFYSUM(SUMRY,TRAIN,LINE)  ;
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
  NEW DFN SET DFN=+Y
  NEW ARR,TEMP
  DO MEDLIST^TMGTIUOJ(.TEMP,DFN,.ARR)
  WRITE !,"--this is before parsing  -------------",!
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:IDX'>0  DO
  . WRITE $GET(ARR(IDX)),!  
  NEW ARR2 DO PARSEARR(.ARR2,.ARR)
  WRITE !,"--Below is parsed med list -------------",!
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(ARR2(IDX)) QUIT:IDX'>0  DO
  . WRITE $GET(ARR2(IDX)),!
  WRITE !,"--Appearance in FOLLOWING runs -------------",!
  NEW ARR3 DO PARSEARR(.ARR3,.ARR2)
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(ARR3(IDX)) QUIT:IDX'>0  DO
  . WRITE $GET(ARR3(IDX)),!  
  SET %=1 WRITE !,"Try another patient" DO YN^DICN WRITE !
  IF %=1 GOTO TL1
  QUIT
  ;
PARSEARR(OUT,ARR)  ;"PARSE A MED LIST  
  NEW OLD,IDX,JDX SET (IDX,JDX)=0 
  FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:IDX'>0  DO
  . NEW LINE,LINE2 SET LINE=$$TRIM^XLFSTR($GET(ARR(IDX))),LINE2=LINE 
  . IF LINE["[OLD ENTRY]" QUIT
  . SET OUT(IDX)=LINE
  . IF LINE["[MEDICATION" QUIT
  . IF $EXTRACT(LINE,1)'="*" DO
  . . NEW TEMP DO PARSELN(.TEMP,LINE)
  . . SET LINE2=$$TRIM^XLFSTR($$EXTERNAL^TMGRX003(.TEMP))
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
PARSELN(OUT,LINE,TRAIN) ;"PARSE ONE MED LINE
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
  ;"         OUT("NOTE")=<any notes>  DELIMITED BY "<---" OR '>---" ...
  ;"       LINE -- a string of text representing one medication line from med table.
  ;"       TRAIN -- OPTIONAL.  If 1 then user is queried at command-line if needed during training
  ;"Result: None  
  SET ^TMP("PARSELN^TMGRX001",$J)=LINE
  NEW SUMRY,RESULT
  DO FIXLINE(.OUT,.LINE)  ;"FIX LINE
  SET RESULT=$$PARS2SUM(.OUT,.SUMRY,.TRAIN)
  IF (RESULT>0)!(RESULT["MISSING STRENGTH") DO
  . DO XTRCTDAT(.OUT,.SUMRY) ;"Extract data based on summry
  . DO FIXINFO(.OUT)  ;"FIX INFORMATION ARRAY    
PRSLNDN  
  QUIT
  ;
FIXLINE(OUT,LINE)  ;"FIX LINE
  DO FIXARRWS^TMGRX004(.LINE)       ;"FIX ARROWS
  SET OUT("ORIG")=LINE
  SET LINE=$$UP^XLFSTR(LINE)
  DO XTRCTNTE^TMGRX004(.OUT,.LINE)  ;"EXTRACT NOTE, IF ANY
  DO FIXDBLSP^TMGRX004(.LINE)       ;"FIX DOUBLE SPACES
  DO CHKPREFX^TMGRX004(.OUT,.LINE)  ;"CHECK AND REMOVE PREFIXES (INCLUDING OTC)
  IF LINE["_?_STRENGTH" SET LINE=$$REPLSTR^TMGSTUT3(LINE,"_?_STRENGTH","")  
  SET OUT("WORKING")=LINE
  QUIT
  ;
FIXINFO(OUT)  ;"FIX INFORMATION ARRAY
  ;"Input: OUT -- PASS BY REFERENCE, AN IN AND OUT PARAMETER
  ;"RESULT: None  
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
  IF SIG'="" DO  ;"Get back non-UPPERCASE sig
  . NEW ORIG SET ORIG=$GET(OUT("ORIG"))  ;"This is mixed case 
  . NEW POS SET POS=$$POS^TMGSTUT3(SIG,$$UP^XLFSTR(ORIG)) QUIT:POS'>0
  . SET SIG=$EXTRACT(ORIG,POS,POS+$LENGTH(SIG)-1)
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
  QUIT
  ;  
XTRCTDAT(OUT,SUMRY) ;"Extract data based on summary
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
  ;"      SUMRY --
  ;"Result: None
  NEW IEN22733 SET IEN22733=+$GET(OUT("IEN22733"))
  IF IEN22733>0 SET OUT("MEDICATION","GENERIC","DATABASE")=$$GET1^DIQ(22733,IEN22733_",",.01)
  NEW LINE SET LINE=OUT("WORKING")
  NEW DONE SET DONE=0
  NEW LOSTWORDS
  NEW MATCHSUMRY SET MATCHSUMRY=$GET(SUMRY("MATCH"))
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(SUMRY(IDX)) QUIT:(IDX'>0)!(DONE)  DO
  . NEW WORD MERGE WORD=SUMRY(IDX) SET WORD=$GET(WORD)
  . NEW SOURCE SET SOURCE=$GET(WORD("SOURCE"))
  . IF WORD'["{{" DO  QUIT
  . . NEW JDX SET JDX=+$GET(LOSTWORDS)+1
  . . MERGE LOSTWORDS(JDX)=WORD
  . . SET LOSTWORDS=JDX
  . . ;"NEW KDX SET KDX=IDX-1,DONE=1
  . . ;"FOR  SET KDX=$ORDER(SUMRY(KDX)) QUIT:(KDX'>0)!(DONE=0)  DO
  . . ;". SET:($GET(SUMRY(KDX))["{{") DONE=0
  . ELSE  IF WORD="{{DRUG_GENERIC}}" DO  QUIT
  . . IF MATCHSUMRY'[WORD QUIT
  . . SET OUT("SIG")=$GET(WORD("DIV"))_$$ARR2LN3^TMGRX004("SUMRY",IDX+.1)
  . . IF $GET(OUT("MEDICATION","GENERIC"))'="" QUIT  ;"ONLY USE 1ST VALUE 
  . . SET OUT("MEDICATION","GENERIC")=SOURCE
  . . IF SOURCE'="" SET OUT("MEDICATION","INPUT NAME")=SOURCE
  . ELSE  IF WORD="{{DRUG_ALIAS}}" DO  QUIT
  . . IF MATCHSUMRY'[WORD QUIT
  . . SET OUT("SIG")=$GET(WORD("DIV"))_$$ARR2LN3^TMGRX004("SUMRY",IDX+.1)
  . . IF $GET(OUT("MEDICATION","GENERIC ALIAS"))'="" QUIT  ;"ONLY USE 1ST VALUE 
  . . SET OUT("MEDICATION","GENERIC ALIAS")=SOURCE   
  . . IF SOURCE'="" SET OUT("MEDICATION","INPUT NAME")=SOURCE  ;"<-- last Rx name encountered on input line
  . ELSE  IF WORD="{{DRUG_ABBREV}}" DO  QUIT
  . . IF MATCHSUMRY'[WORD QUIT
  . . SET OUT("SIG")=$GET(WORD("DIV"))_$$ARR2LN3^TMGRX004("SUMRY",IDX+.1)
  . . IF $GET(OUT("MEDICATION","GENERIC ABBRV"))'="" QUIT  ;"ONLY USE 1ST VALUE 
  . . SET OUT("MEDICATION","GENERIC ABBRV")=SOURCE
  . . IF SOURCE'="" SET OUT("MEDICATION","INPUT NAME")=SOURCE
  . ELSE  IF WORD="{{DRUG_BRAND}}" DO  QUIT
  . . IF MATCHSUMRY'[WORD QUIT
  . . SET OUT("SIG")=$GET(WORD("DIV"))_$$ARR2LN3^TMGRX004("SUMRY",IDX+.1)
  . . IF $GET(OUT("MEDICATION","BRAND"))'="" QUIT  ;"ONLY USE 1ST VALUE 
  . . SET OUT("MEDICATION","BRAND")=SOURCE
  . . SET OUT("MEDICATION","BRAND","PREFERRED")=$$GETPRBRD^TMGRX004(IEN22733)  ;"GET PREFERRED BRAND NAME    
  . . IF SOURCE'="" SET OUT("MEDICATION","INPUT NAME")=SOURCE
  . ELSE  IF (WORD="{{STRENGTH}}")!(WORD="{{STRENGTH_ALIAS}}") DO  QUIT
  . . IF MATCHSUMRY'[WORD QUIT
  . . SET OUT("SIG")=$GET(WORD("DIV"))_$$ARR2LN3^TMGRX004("SUMRY",IDX+.1)
  . . IF $GET(OUT("STRENGTH"))'="" QUIT  ;"ONLY USE 1ST VALUE 
  . . SET OUT("STRENGTH")=SOURCE
  . . SET OUT("SIG")=$GET(WORD("DIV"))_$$ARR2LN3^TMGRX004("SUMRY",IDX+.1)
  . . DO MTCHSTRT^TMGRX004(.OUT,.WORD)  ;"TRY TO MATCH STRENGTHS TO DATABASE. 
  . ELSE  IF WORD="{{MODIFIER}}" DO  QUIT
  . . IF MATCHSUMRY'[WORD QUIT
  . . SET OUT("SIG")=$GET(WORD("DIV"))_$$ARR2LN3^TMGRX004("SUMRY",IDX+.1)
  . . IF $GET(OUT("MODIFIER"))'="" QUIT  ;"ONLY USE 1ST VALUE 
  . . SET OUT("MODIFIER")=SOURCE  
  . . SET OUT("MODIFIER","PREFERRED")=$$GETPMOD1^TMGRX004(IEN22733,SOURCE)  ;"GET PREFERRED MODIFIER, IN SAME GROUP AS MOD  
  . ELSE  IF (WORD="{{FORM}}")!(WORD="{{FORM_ALIAS}}") DO  QUIT
  . . IF MATCHSUMRY'[WORD QUIT
  . . SET OUT("SIG")=$GET(WORD("DIV"))_$$ARR2LN3^TMGRX004("SUMRY",IDX+.1)
  . . IF $GET(OUT("FORM"))'="" QUIT  ;"ONLY USE 1ST VALUE 
  . . SET OUT("FORM")=SOURCE
  . . SET OUT("FORM","SUBIEN")=+$$GTFRMIEN^TMGRX004(IEN22733,SOURCE)  ;"GET FORM SUBIEN BASED ON INPUT FORM
  . ELSE  IF (WORD="{{UNIT}}")!(WORD="{{UNIT_ALIAS}}") DO  QUIT
  . . IF MATCHSUMRY'[WORD QUIT
  . . SET OUT("SIG")=$GET(WORD("DIV"))_$$ARR2LN3^TMGRX004("SUMRY",IDX+.1)
  . . IF $GET(OUT("UNITS"))'="" QUIT  ;"ONLY USE 1ST VALUE 
  . . SET OUT("UNITS")=SOURCE
  . . SET OUT("UNITS","IEN50.607")=$GET(WORD("UNIT IEN"))
  . . DO MATCHUNT^TMGRX004(.OUT,.WORD)
  . ELSE  IF WORD="{{ROUTE}}" DO
  . . IF MATCHSUMRY'[WORD QUIT
  . . IF $GET(OUT("SIG","ROUT"))'="" QUIT  ;"ONLY USE 1ST VALUE 
  . . SET OUT("SIG","ROUTE")=SOURCE
  . ELSE  IF WORD="{{FREQ}}" DO
  . . IF MATCHSUMRY'[WORD QUIT
  . . IF $GET(OUT("SIG","FREG"))'="" QUIT  ;"ONLY USE 1ST VALUE 
  . . SET OUT("SIG","FREQ")=SOURCE
  SET OUT("SIG")=$$TRIMDIVS($GET(OUT("SIG")),"L")
  SET OUT("PREFACE")=$$TRIMDIVS($GET(OUT("PREFACE")),"R")
  QUIT
  ;
PARS2SUM(OUT,SUMRY,TRAIN)  ;"PARSE TO SUMMARY
  ;"INPUT:  OUT -- PASS BY REFERENCE.  Contains many objects. 
  ;"             Must hold OUT("WORKING")=LINE
  ;"        TRAIN -- OPTIONAL.  If 1 then interactive process
  ;"RESULT: 1 if successful, 0 if not
  NEW RESULT,LINE SET LINE=OUT("WORKING")
  SET OUT("IEN22733")=$$GETMDIEN^TMGRX004(LINE)  ;"GET MEDICATION IEN (IEN22733) FROM LINE
  IF OUT("IEN22733")'>0 SET RESULT=0 GOTO P2SDN  
  DO GETSUMRY(.SUMRY,LINE,OUT("IEN22733"),1)  ;"GET A SUMMARY OF LINE
  DO RMVPREFX(.OUT,.SUMRY)  ;"REMOVE PREFIX FROM SUMMARY ARRAY
  SET RESULT=$$VERFYSUM(.SUMRY,+$GET(TRAIN),OUT("WORKING"))
P2SDN ;  
  QUIT RESULT
  ;
GETSUMRY(SUMRY,LINE,IEN22733,PARSESIG)  ;"GET A SUMMARY OF LINE
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
  ;"        LINE  -- THE LINE TO PASS
  ;"        IEN22733 -- THE IEN OF THE DRUG TO WORK WITH
  ;"        PARSESIG -- 0 OR 1, if 1 then parsing in sig is done.  
  ;"RESULT: none
  NEW DICT DO GETDICT^TMGRX004(.DICT,IEN22733)
  DO FIXSPLNG^TMGRX004(IEN22733,.LINE)
  SET SUMRY("ORIG")=LINE
  NEW WORD,LINEARR,REFNUM SET REFNUM=1
  DO LINE2ARR^TMGRX004(.LINEARR,LINE,.DICT) ;"Parse LINE into LINEARR
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(LINEARR("WORD",IDX)) QUIT:IDX=""  DO
  . KILL WORD MERGE WORD=LINEARR("WORD",IDX) SET WORD=$GET(WORD)
  . NEW TYPE SET TYPE=$GET(WORD("TYPE"))
  . IF TYPE="" SET TYPE=$$FINDTYPE^TMGRX004(WORD,.DICT,.PARSESIG) ;"FIND A TYPE FOR WORD
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
VERFYSUM(SUMRY,TRAIN,LINE)  ;
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
  DO NORMLSUM(.TEMPSUMRY)  ;"NORMALIZE SUMMARY into standard text output
VL1 ;  
  SET MATCH=$$LONGESTM(TEMPSUMRY)  ;"GET LONGEST MATCH FROM 22733.1  
  SET RESULT=(MATCH'="")
  IF $GET(TRAIN)'>0 GOTO VFPTDN
  IF (TEMPSUMRY=MATCH),(TRAIN<2) GOTO VFPTDN
  ;"NEW I FOR I=1:1:16 WRITE "*",!  ;"<--- DELETE LATER
  WRITE "TRAINING MODE: ON.  Verifying when no exact matches.",!
  WRITE "ORIG: ",LINE,!
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(TEMPSUMRY(IDX)) QUIT:IDX'>0  DO
  . NEW AWORD SET AWORD=$GET(TEMPSUMRY(IDX)) 
  . IF AWORD'["{{" QUIT
  . WRITE AWORD," <-- ",$GET(TEMPSUMRY(IDX,"SOURCE"),"?"),!
  WRITE "SUMMARY: ",TEMPSUMRY,!
  NEW HASSTRENGTH SET HASSTRENGTH=((TEMPSUMRY["{{STRENGTH}}")!(TEMPSUMRY["{{STRENGTH_ALIAS}}"))
  IF 'HASSTRENGTH DO  GOTO:(+RESULT=-1) VFPTDN
  . IF STRENGTHSEEN'="" SET %=STRENGTHSEEN  ;"prevent asking same question twice after loopback.  
  . ELSE  DO
  . . WRITE "NOTE: strength was not parsed.  Do you see strength in ORIG"   
  . . SET %=2 DO YN^DICN WRITE !
  . . SET STRENGTHSEEN=%
  . IF %=1 SET RESULT="-1^MISSING STRENGTH",MATCH=TEMPSUMRY
  . IF %=-1 SET RESULT="-1^ABORT"                 
  SET %=0 
  IF EDITED,($LENGTH(MATCH)=SHOWNMATCHLEN),(VALUE'="") GOTO VL3
  IF MATCH'="",($LENGTH(MATCH)>SHOWNMATCHLEN) DO
  . WRITE "MATCH: ",MATCH,!
  . SET SHOWNMATCHLEN=$LENGTH(MATCH)
  . WRITE "MATCH would have been used if not in training mode.",!
  . SET %=1 WRITE "Would MATCH have been OK to identify Med (but NOT SIG)" DO YN^DICN WRITE !
  IF %=-1 SET RESULT="-1^ABORT" GOTO VFPTDN
  IF %=1 GOTO VFPTDN
  SET %=2 WRITE "Does SUMMARY above have any parsing errors (extra parts OK)" DO YN^DICN WRITE !
  IF %=-1 SET RESULT=-1 GOTO VFPTDN
  IF %=1 SET RESULT=0 GOTO VFPTDN               
  WRITE !,"DELETE invalid or non-universal part of summary",!
  WRITE "EDIT: " SET VALUE=$$EDITBOX^TMGUSRI6(TEMPSUMRY,70)
  SET VALUE=$$TRIM^XLFSTR(VALUE)
  IF VALUE'=TEMPSUMRY SET TEMPSUMRY=VALUE,EDITED=1 GOTO VL1
VL3 ;  
  SET %=2 WRITE !,!,"SAVE: ",VALUE,":" DO YN^DICN WRITE !
  IF %=-1 SET RESULT=-1 GOTO VFPTDN
  IF %=2 GOTO VFPTDN
  NEW TMGFDA,TMGIEN,TMGMSG
  SET TMGFDA(22733.1,"+1,",.01)=VALUE
  DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO VFPTDN
  . WRITE "ERROR:",!,$$GETERRST^TMGDEBU2(.TMGMSG)
  SET RESULT=1
  ;
VFPTDN ;  
  SET SUMRY("MATCH")=MATCH
  QUIT RESULT
  ;
NORMLSUM(SUMRY)  ;"NORMALIZE SUMMARY
  NEW KILLIDX SET KILLIDX=0
  NEW IDX SET IDX=0
  ;"Kill all array members that are not {{xxx}} type words
  FOR  SET IDX=$ORDER(SUMRY(IDX)) QUIT:(IDX'>0)!(KILLIDX>0)  DO
  . NEW WORD SET WORD=$GET(SUMRY(IDX))
  . IF (WORD'["{{")&(WORD'="") KILL SUMRY(IDX)
  . IF KILLIDX=0,($$ISROUTE^TMGRX004(WORD)!$$ISFREQ^TMGRX004(WORD)) DO
  . . ;"SET KILLIDX=IDX  <-- disable because of 'VITAMIN D PO 50,000 IU EVERY OTHER WEEK.'
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
  . IF $$LMATCH^TMGSTUT3(STR,ASUMRY)=0 DO  QUIT
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
  ;"NOTE: If wanted, could covert "PO" to "By mouth" etc if wanted
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
