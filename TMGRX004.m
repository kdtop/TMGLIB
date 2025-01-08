TMGRX004 ;TMG/kst/Patient medication code; 08/30/17
       ;;1.0;TMG-LIB;**1**;08/30/17
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 08/30/17  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"LINE2ARR(ARR,LINE)  -- PARSE LINE INTO AN ARRAY THAT COULD BE REASSEMBLED
 ;"ARR2LINE(ARR,STARTIDX)  -- REASSEMBLE ARRAY, AS CREATED BY LINE2ARR
 ;"ARR2LN2(REF,STARTIDX)   -- REASSEMBLE ARRAY
 ;"ARR2LN3(REF,STARTIDX) -- RESSEMBLE ARRY SUBSTITUTING {{XXX}} WITH SOURCE
 ;"FINDTYPE(WORD,DICT,PARSESIG) -- FIND A TYPE FOR WORD, IF POSSIBLE. 
 ;"GETDICT(ARR,IEN22733)  -- GET A LISTING OF ALL WORDS ASSOCIATED WITH RX
 ;"ADD2ARR(ARR,WORD,TYPE,FILE,FLD,IENS)  --ADD TO ARRAY
 ;"ADDRFDCT(ARR,REF,IEN22733,TYPE)  -- ADD DICT REF REF  
 ;"ADDRFDT2(ARR,FILE,IENS,TYPE)  ;"ADD DICT REF REF, VERSION 2
 ;"CHKPREFX(OUT,LINE)  -- CHECK AND REMOVE PREFIXES (INCLUDING OTC)
 ;"ISPREFIX(OUT,WORD)  -- HANDLE PREFIXES
 ;"FIXARRWS(LINE) -- FIX ARROWs, change alternate forms into standard form  
 ;"FIXDBLSP(LINE)  --FIX DOUBLE SPACES
 ;"XTRCTNTE(OUT,LINE)  -- EXTRACT NOTE
 ;"ISROUTE(WORD)  -- IS WORD A ROUTE?
 ;"ISFREQ(WORD)  -- IS WORD A FREQUENCY?
 ;"ISMODIFR(IEN22733,WORD)  -- IS WORD A RX NAME MODIFIER?  E.g. 'CD'
 ;"ISFORM(WORD)  --IS WORD A FORM (E.G. TAB)
 ;"WORDINFO(WORD,DICT)  -- characterize word
 ;"GETNXWRD(LINE,WORD,DICT)  -- GET NEXT WORD
 ;"DIVSET()  --Standard characters to divide up words.
 ;"SUBDVSET()  ;
 ;"PEEKNXWD(LINE,WORD,IDX)  -- GET NEXT WORD, just look, doesn't remove from line.
 ;"PUTBKWRD(LINE,WORD)  -- PUT BACK WORD
 ;"WORDLEN(LINE,DICT)  -- return length of line, E.G. 'hello-there/world you' as 4 words
 ;"STRIPWD(WORD) -- STRIP PUNCTUATION
 ;"GETMDIEN(LINE)  --GET MEDICATION IEN (IEN22733) FROM LINE
 ;"MEDIEN(OUT,NAME,LINE)  -- 
 ;"MEDIEN2(OUT,LINE,WORD)  --GET MEDICATION IEN, ALT VERSION
 ;"MDI2ADD(OUTREF,REF) -- ADD ARRAY INDEX TO OUT OUT ARRAY
 ;"MAXLMTCH(REFARR,STR)  -- findest longest entry in @REFARR, LMATCH'ing with STR
 ;"MTCHSTRT(OUT,WORD)  -- TRY TO MATCH STRENGTHS TO DATABASE.  
 ;"MOD2FRM(ARR)  -- Get correct IENS (which specifies correct form), based on matching modifiers
 ;"GETPSRA(IEN22733,FORMIEN,STRIEN)  -- GET PREFERRED STRENGTH ALIAS
 ;"GETPRBRD(IEN22733)  -- GET PREFERRED BRAND NAME
 ;"GETPRGA(IEN22733)  -- GET PREFERRED GENERIC ALIAS
 ;"GETPRGB(IEN22733)  -- GET PREFERRED GENERIC ABBREVIATION
 ;"GETPRUNT(IEN22733,FORMIEN,STRIEN) -- GET PREFERRED UNITS FOR FORM, STRENGTH
 ;"GETPRFAL(IEN22733,FORMIEN) -- GET PREFERRED FORM ALIAS
 ;"GETPREF(ROOT,GETANY)  -- GET PREFERRED ENTRY (VALUE IN PIECE 1, PREF IN PIECE 2)
 ;"GETPMOD1(IEN22733,MOD)  -- GET PREFERRED MODIFIER, IN SAME GROUP AS MOD
 ;"GETPMOD2(IEN22733,FORMIEN) -- GET PREFERRED MOD, FOR GIVEN FORMIEN
 ;"GTFRMIEN(IEN22733,FORM)  -- GET FORM SUBIEN BASED ON INPUT FORM
 ;"MATCHFRM(IEN22733,OUT,FORM)  -- GET PREFERRED ALIAS FORM, GIVEN INPUT FORM -- DELETE THIS  
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"=======================================================================
 ;"Uses:  
 ;"=======================================================================
 ;        
LINE2ARR(ARR,LINE,DICT)  ;"PARSE LINE INTO AN ARRAY THAT COULD BE REASSEMBLED
  ;"INPUT:  ARR -- PASS BY REFERENCE.  AN OUT PARAMETER.  format
  ;"        Example for input LINE of 'ANORO ELLIPTA 62.5MG/25 MG ONCE DAILY'
  ;"        ARR                                                                         
  ;"        }~WORD                                                                          
  ;"          }~1 = ANORO ELLIPTA                                                           
  ;"          | }~DBSOURCE                                                                  
  ;"          | | }~"22733.01^.01^1,215," = ""                                              
  ;"          | }~"DIV" = " "                                                               
  ;"          | }~"NUM" = ""                                                                
  ;"          | }~"S2" = ANORO ELLIPTA                                                      
  ;"          | }~"TYPE" = DRUG_BRAND                                                       
  ;"          | }~"UNIT IEN" = ""                                                           
  ;"          }~2 = 62.5MG/25                                                               
  ;"          | }~DBSOURCE                      
  ;"          | | }~"22733.31^.01^1,2,1,215," = ""                                          
  ;"          | }~"DIV" = " "                    
  ;"          | }~"NUM" = 62.5                    
  ;"          | }~"S2" = MG/25                    
  ;"          | }~"TYPE" = STRENGTH_ALIAS                    
  ;"          | }~"UNIT IEN" = ""                    
  ;"          }~3 = MG                    
  ;"          | }~DBSOURCE                    
  ;"          | | }~"22733.32^.01^1,2,1,215," = ""                    
  ;"          | }~"DIV" = " "                    
  ;"          | }~"NUM" = ""                    
  ;"          | }~"S2" = MG                    
  ;"          | }~"TYPE" = UNIT_ALIAS                    
  ;"          | }~"UNIT IEN" = 20                    
  ;"          }~4 = ONCE                    
  ;"          | }~"DIV" = " "                    
  ;"          | }~"NUM" = ""                    
  ;"          | }~"S2" = ONCE                    
  ;"          | }~"UNIT IEN" = ""                    
  ;"          }~5 = DAILY                    
  ;"            }~"DIV" = " "                    
  ;"            }~"NUM" = ""                    
  ;"            }~"S2" = DAILY                    
  ;"            }~"UNIT IEN" = ""   
  ;"        LINE -- string to parse, e.g. 'LORTAB 5/325 1 PO QDAY'
  ;"        DICT -- the array of words for Rx, as made by GETDICT^TMGRX004.  OPTIONAL
  NEW WORD,IDX,CT,DIV SET CT=0
  FOR  QUIT:LINE=""  DO
  . DO GETNXW1(.LINE,.WORD,.DICT)
  . SET CT=CT+1 MERGE ARR("WORD",CT)=WORD
  QUIT
  ;
ARR2LINE(ARR,STARTIDX,TRYSUB)  ;"REASSEMBLE ARRAY, AS CREATED BY LINE2ARR
  ;"INPUT:  ARR -- PASS BY REFERENCE.  ARRAY AS CREATED BY LINE2ARR
  ;"        STARTIDX -- OPTIONAL.  DEFAULT=0.
  ;"        TRYSUB -- OPTIONAL.  IF 1, then try to substitute {{xxx}} words with source.  
  SET STARTIDX=+$GET(STARTIDX)
  SET TRYSUB=+$GET(TRYSUB)
  NEW RESULT SET RESULT=""
  NEW IDX SET IDX=STARTIDX-0.1
  FOR  SET IDX=$ORDER(ARR("WORD",IDX)) QUIT:IDX'>0  DO
  . NEW WORD MERGE WORD=ARR("WORD",IDX)
  . IF (WORD["{{")&(WORD["}}"),(TRYSUB=1) DO
  . . NEW SRC SET SRC=$GET(ARR("WORD",IDX,"SOURCE"))
  . . IF SRC'="" SET WORD=SRC
  . SET RESULT=RESULT_$GET(WORD)_$GET(WORD("DIV"))
  QUIT RESULT
  ;
ARR2LN2(REF,STARTIDX)   ;"REASSEMBLE ARRAY
  NEW TEMP MERGE TEMP("WORD")=@REF
  QUIT $$ARR2LINE(.TEMP,.STARTIDX)
  ;
ARR2LN3(REF,STARTIDX)  ;"RESSEMBLE ARRY SUBSTITING {{XXX}} WITH SOURCE
  NEW TEMP MERGE TEMP("WORD")=@REF
  QUIT $$ARR2LINE(.TEMP,.STARTIDX,1)
  ;
FINDTYPE(WORD,DICT,PARSESIG) ;"FIND A TYPE FOR WORD, IF POSSIBLE. 
  NEW TYPE SET TYPE=""
  SET PARSESIG=$GET(PARSESIG)
  NEW STRIPWORD SET STRIPWORD=$$STRIPWD(WORD)
  IF $DATA(DICT(STRIPWORD))>0 DO
  . SET TYPE=$GET(DICT(STRIPWORD))
  . SET FRACT=($GET(DICT(STRIPWORD,"FRACT"))=1)  ;"<-----  ??   
  ELSE  DO
  . DO WORDINFO(.STRIPWORD,.DICT)
  . IF $GET(STRIPWORD("TIME"))'="" SET TYPE="TIME" QUIT
  . IF $GET(STRIPWORD("NUM"))'="" SET TYPE="NUM" QUIT
  . IF $GET(STRIPWORD("DATE"))'="" SET TYPE="DATE" QUIT
  . IF $$ISFORM(WORD) SET TYPE="FORM" QUIT
  . IF PARSESIG,$$ISROUTE(WORD) SET TYPE="ROUTE" QUIT
  . IF PARSESIG,$$ISFREQ(WORD) SET TYPE="FREQ" QUIT
  QUIT TYPE
  ;
GETDICT(ARR,IEN22733)  ;"GET A LISTING OF ALL WORDS ASSOCIATED WITH RX
  ;"INPUT:  ARR -- PASS BY REFERENCE, AN OUT PARAMETER.  FORMAT:
  ;"          ARR(120)="STRENGTH"
  ;"          ARR("24 HR")="MODIFIER"
  ;"          ARR("CARDIZEM")="BRAND"
  ;"          ARR("CD")="MODIFIER"
  ;"          ARR("DILTAZEM")="ALIAS"
  ;"          ARR("DILTIAZEM")="GENERIC"
  ;"          ARR("ER")="MODIFIER"
  ;"          ARR("LA")="MODIFIER"
  ;"          ARR("SR")="MODIFIER"
  ;"          ARR("XR")="MODIFIER"
  ;"          ARR("ZZIDX","ALIAS","DILTAZEM")="ALIAS"
  ;"          ARR("ZZIDX","BRAND","CARDIZEM")="BRAND"
  ;"          ARR("ZZIDX","STRENGTH",120)="STRENGTH"
  ;"          ARR("ZZIDX","GENERIC","DILTIAZEM")="GENERIC"
  ;"          ARR("ZZIDX","MODIFIER","24 HR")="MODIFIER"
  ;"          ARR("ZZIDX","MODIFIER","CD")="MODIFIER"
  ;"          ARR("ZZIDX","MODIFIER","ER")="MODIFIER"
  ;"          ARR("ZZIDX","MODIFIER","LA")="MODIFIER"
  ;"          ARR("ZZIDX","MODIFIER","SR")="MODIFIER"
  ;"          ARR("ZZIDX","MODIFIER","XR")="MODIFIER"
  ;"          ARR("ZZSOURCE",<WORD>,'<FILE>^<FLD>^<IENS>')=""
  ;"IEN22733 -- IEN to check
  ;"RESULTS -- NONE
  NEW WORD,TYPE,REF
  NEW FILES SET FILES=22733
  SET FILES("GENERIC ALIASES")=22733.022
  SET FILES("GENERIC ABBREVS")=22733.04
  SET FILES("COMBO INGREDS")=22733.05
  SET FILES("BRAND NAMES")=22733.01
  SET FILES("MEDICATION FORMS")=22733.02
  SET FILES("FORM ALIASES")=22733.055
  SET FILES("STRENGTHS")=22733.03
  SET FILES("STRENGTH ALIASES")=22733.31
  SET FILES("UNIT ALIASES")=22733.32
  SET FILES("NAME MODIFIERS")=22733.22
  ;
  SET ARR("ZZIEN",IEN22733)=""
  SET WORD=$PIECE($GET(^TMG(22733,IEN22733,0)),"^",1) 
  IF WORD'="" DO ADD2ARR(.ARR,WORD,"DRUG_GENERIC",22733,.01,IEN22733)
  DO ADDRFDT2(.ARR,FILES("GENERIC ALIASES"),IEN22733,"DRUG_ALIAS")  
  DO ADDRFDT2(.ARR,FILES("BRAND NAMES"),IEN22733,"DRUG_BRAND")  
  DO ADDRFDT2(.ARR,FILES("GENERIC ABBREVS"),IEN22733,"DRUG_ABBREV")  
  NEW SUBIEN SET SUBIEN=0
  FOR  SET SUBIEN=$ORDER(^TMG(22733,IEN22733,2,SUBIEN)) QUIT:SUBIEN'>0  DO  ;"DOSAGE FORMS, #22733.02
  . NEW IENS SET IENS=SUBIEN_","_IEN22733_","
  . NEW FORMIEN SET FORMIEN=+$PIECE($GET(^TMG(22733,IEN22733,2,SUBIEN,0)),"^",1)
  . IF FORMIEN>0 DO
  . . NEW FORM SET FORM=$PIECE($GET(^PS(50.606,FORMIEN,0)),"^",1)
  . . QUIT:FORM=""
  . . DO ADD2ARR(.ARR,FORM,"FORM",FILES("MEDICATION FORMS"),.01,IENS)
  . . IF ",CAP,CAPS,"[(","_FORM_",") DO ADD2ARR(.ARR,"CAPSULE","FORM",FILES("MEDICATION FORMS"),.01,IENS)
  . . IF ",TAB,TABS,"[(","_FORM_",") DO ADD2ARR(.ARR,"TABLET","FORM",FILES("MEDICATION FORMS"),.01,IENS)
  . DO ADDRFDT2(.ARR,FILES("FORM ALIASES"),IENS,"FORM_ALIAS")    
  . NEW STRENGTHIEN SET STRENGTHIEN=0
  . FOR  SET STRENGTHIEN=$ORDER(^TMG(22733,IEN22733,2,SUBIEN,1,STRENGTHIEN)) QUIT:STRENGTHIEN'>0  DO
  . . NEW SIENS SET SIENS=STRENGTHIEN_","_SUBIEN_","_IEN22733_","
  . . NEW UNITIEN SET UNITIEN=+$PIECE($GET(^TMG(22733,IEN22733,2,SUBIEN,1,STRENGTHIEN,0)),"^",2)  ;"STRENGTHS, #22733.03
  . . NEW UNIT SET UNIT=$PIECE($GET(^PS(50.607,UNITIEN,0)),"^",1)
  . . IF UNIT'="" DO  ;"check this next line.... Unit and strength??
  . . . DO ADD2ARR(.ARR,UNIT,"UNIT",FILES("STRENGTHS"),.02,SIENS)
  . . DO ADDRFDT2(.ARR,FILES("STRENGTH ALIASES"),SIENS,"STRENGTH_ALIAS")    
  . . DO ADDRFDT2(.ARR,FILES("UNIT ALIASES"),SIENS,"UNIT_ALIAS")    
  . DO ADDRFDT2(.ARR,FILES("STRENGTHS"),IENS,"STRENGTH")    
  . DO ADDRFDT2(.ARR,FILES("NAME MODIFIERS"),IENS,"MODIFIER")    
  ;
  ;"NOTE: I AM NOT DOING MATCHING BASED ON INGREDIENTS ANY MORE...
  ;";"CHECK IF THIS DRUG HAS INGREDIENTS, IF SO INCLUDE THEM
  ;"NEW IEN SET IEN=0
  ;"FOR  SET IEN=$ORDER(^TMG(22733,IEN22733,.05,"B",IEN)) QUIT:IEN'>0  DO
  ;". IF $DATA(ARR("ZZIEN",IEN)) QUIT  ;"AVOID RECURSIVE ENDLESS LOOPS
  ;". NEW TEMPARR DO GETDICT(.TEMPARR,IEN)
  ;". ;"DELETE PARTS
  ;". NEW ASTRENGTH SET ASTRENGTH=""
  ;". FOR  SET ASTRENGTH=$ORDER(TEMPARR("ZZIDX","STRENGTH",ASTRENGTH)) QUIT:ASTRENGTH=""  DO 
  ;". . KILL TEMPARR(ASTRENGTH)
  ;". KILL TEMPARR("ZZIDX","STRENGTH")
  ;". MERGE ARR=TEMPARR
  QUIT
  ;
ADD2ARR(ARR,WORD,TYPE,FILE,FLD,IENS)  ;"ADD TO ARRAY
  SET ARR(WORD)=TYPE
  SET ARR("ZZIDX",TYPE,WORD)=""
  SET ARR("ZZLEN",$LENGTH(WORD),WORD)=TYPE
  ;"SAVE SOURCE INFO BELOW
  SET FILE=+$GET(FILE),FLD=+$GET(FLD),IENS=$GET(IENS)
  IF IENS'="",$EXTRACT(IENS,$LENGTH(IENS))'="," SET IENS=IENS_","
  IF FILE>0,IENS'="" SET ARR("ZZSOURCE",WORD,FILE_"^"_FLD_"^"_IENS)=""
  QUIT
  ;
ADDRFDCT(ARR,REF,IEN22733,TYPE)  ;"ADD DICT REF REF  
  NEW TEMP,WORD,TEMPWORD
  SET WORD="" 
  FOR  SET WORD=$ORDER(@REF@(WORD)) QUIT:WORD=""  DO
  . SET TEMP(WORD)=TYPE
  . SET TEMP("ZZLEN",$LENGTH(WORD),WORD)=TYPE
  . IF WORD["/" DO
  . . SET TEMPWORD=$$REPLSTR^TMGSTUT3(WORD,"/","-")
  . . SET TEMP(TEMPWORD)=TYPE
  . . SET TEMP("ZZLEN",$LENGTH(TEMPWORD),TEMPWORD)=TYPE
  . IF WORD["-" DO
  . . SET TEMPWORD=$$REPLSTR^TMGSTUT3(WORD,"-","/")
  . . SET TEMP(TEMPWORD)=TYPE
  . . SET TEMP("ZZLEN",$LENGTH(TEMPWORD),TEMPWORD)=TYPE
  MERGE ARR("ZZIDX",TYPE)=TEMP
  MERGE ARR=TEMP
  QUIT
  ;                   
ADDRFDT2(ARR,FILE,IENS,TYPE)  ;"ADD DICT REF REF, VERSION 2
  NEW FLD SET FLD=.01
  NEW IDX SET IDX="B"  ;"Note: would have to extend code if need to work with other than .01 field
  NEW ISSF SET ISSF=$$ISSUBFIL^TMGDBAP3(FILE)
  NEW IENS2 SET IENS2=$SELECT(ISSF:""""_IDX_""","_IENS,1:IENS)
  NEW REF SET REF=$$FILE2REF^TMGDBAP3(FILE,IENS2)
  IF 'ISSF SET REF=REF_""""_IDX_""","      
  NEW ROOTREF SET ROOTREF=$PIECE($$OREF^DILF(REF),",",1,$LENGTH(REF,",")-2)
  SET REF=$$CREF^DILF(REF)   
  SET ROOTREF=$$CREF^DILF(ROOTREF)
  NEW TEMP,WORD SET WORD="" 
  FOR  SET WORD=$ORDER(@REF@(WORD)) QUIT:WORD=""  DO
  . NEW SUBIEN SET SUBIEN=0
  . FOR  SET SUBIEN=$ORDER(@REF@(WORD,SUBIEN)) QUIT:SUBIEN'>0  DO
  . . NEW FULLWORD SET FULLWORD=$PIECE($GET(@ROOTREF@(SUBIEN,0)),"^",1)    
  . . ;"NEW IENS3 SET IENS3=+$ORDER(@REF@(WORD,""))_","_IENS
  . . NEW IENS3 SET IENS3=SUBIEN_","_IENS
  . . DO ADD2ARR(.TEMP,FULLWORD,TYPE,FILE,FLD,IENS3)
  . . IF FULLWORD["/" DO ADD2ARR(.TEMP,$$REPLSTR^TMGSTUT3(FULLWORD,"/","-"),TYPE,FILE,FLD,IENS3)
  . . IF FULLWORD["-" DO ADD2ARR(.TEMP,$$REPLSTR^TMGSTUT3(FULLWORD,"-","/"),TYPE,FILE,FLD,IENS3)
  . . IF $$LMATCH^TMGSTUT3(TYPE,"STRENGTH")>0 DO 
  . . . IF $EXTRACT(FULLWORD,1)="." DO ADD2ARR(.TEMP,"0"_FULLWORD,TYPE,FILE,FLD,IENS3)  ;"auto add 0.5 if given .5
  . . . IF $EXTRACT(FULLWORD,1,2)="0." DO ADD2ARR(.TEMP,$EXTRACT(FULLWORD,2,$LENGTH(FULLWORD)),TYPE,FILE,FLD,IENS3)  ;"auto add .5 if given 0.5
  MERGE ARR("ZZIDX",TYPE)=TEMP
  MERGE ARR=TEMP
  QUIT
  ;
CHKPREFX(OUT,LINE)  ;"CHECK AND REMOVE PREFIXES (INCLUDING OTC)
  ;"NOTE: during summary process, all words before medication name are added to  
  ;"      prefix in GETMDIEN() <-- OLD, prefix to be removed elsewhere. 
  IF LINE["OTC" DO
  . NEW REPLSTR FOR REPLSTR="OTC "," OTC","(OTC)","(OTC "," OTC)" DO
  . . IF LINE'[REPLSTR QUIT
  . . SET LINE=$$REPLSTR^TMGSTUT3(LINE,REPLSTR,"") 
  . . SET OUT("OTC")=1
  NEW WORD DO GETNXWRD(.LINE,.WORD)  ;"GET NEXT WORD
  IF '$$ISPREFIX(.OUT,.WORD) DO
  . DO PUTBKWRD(.LINE,.WORD)
  QUIT
  ;  
ISPREFIX(OUT,WORD)  ;"HANDLE PREFIXES
  SET WORD=$GET(WORD)
  NEW RESULT SET RESULT=0
  IF ",ADD,ADDED,START,STOP,STOPPED,OFF,HOLD,HOLDING,RESTART,?,"[(","_WORD_",") DO  
  . SET RESULT=1
  . SET OUT("PREFACE")=WORD
  . SET WORD=""
  QUIT RESULT
  ;
FIXARRWS(LINE) ;"FIX ARROWs, change alternate forms into standard form  
  IF LINE[">-----" SET LINE=$$REPLSTR^TMGSTUT3(LINE,">-----","<--")  
  IF LINE[">----" SET LINE=$$REPLSTR^TMGSTUT3(LINE,">----","<--")  
  IF LINE[">---" SET LINE=$$REPLSTR^TMGSTUT3(LINE,">---","<--")  
  IF LINE[">--" SET LINE=$$REPLSTR^TMGSTUT3(LINE,">--","<--")  
  IF LINE["<-----" SET LINE=$$REPLSTR^TMGSTUT3(LINE,"<-----","<--")  
  IF LINE["<----" SET LINE=$$REPLSTR^TMGSTUT3(LINE,"<----","<--")  
  IF LINE["<---" SET LINE=$$REPLSTR^TMGSTUT3(LINE,"<---","<--")  
  IF LINE["----->" SET LINE=$$REPLSTR^TMGSTUT3(LINE,"----->","-->")  
  IF LINE["---->" SET LINE=$$REPLSTR^TMGSTUT3(LINE,"---->","-->")  
  IF LINE["--->" SET LINE=$$REPLSTR^TMGSTUT3(LINE,"--->","-->")
  QUIT
  ;
FIXDBLSP(LINE)  ;"FIX DOUBLE SPACES
  FOR  QUIT:LINE'["  "  DO
  . SET LINE=$$REPLSTR^TMGSTUT3(LINE,"  "," ")
  QUIT
  ;
XTRCTNTE(OUT,LINE)  ;"EXTRACT NOTE
  QUIT:LINE'["<--" 
  SET OUT("NOTE")=$$TRIM^XLFSTR($PIECE(LINE,"<--",2,99))
  SET LINE=$PIECE(LINE,"<--",1)
  QUIT
  ;
ISROUTE(WORD)  ;"IS WORD A ROUTE?
  SET WORD=$GET(WORD)
  NEW RESULT SET RESULT=0
  IF (WORD'=""),(",PO,PR,"[WORD) DO
  . SET RESULT=1 
  IF WORD="{{ROUTE}}" SET RESULT=1
  QUIT RESULT
  ;
ISFREQ(WORD)  ;"IS WORD A FREQUENCY?
  SET WORD=$GET(WORD)
  NEW RESULT SET RESULT=0
  IF (WORD'=""),(",QAM,QPM,QDAY,DAILY,BID,TID,QID,QHS,"[(","_WORD_",")) DO
  . SET RESULT=1 
  IF WORD="{{FREQ}}" SET RESULT=1
  QUIT RESULT
  ;
  ;"ISUNIT(WORD) ;"IS WORD A UNIT?
  ;"  SET WORD=$GET(WORD) IF WORD="" SET WORD="ZZNULL"
  ;"  NEW RESULT SET RESULT=($ORDER(^PS(50.607,"B",WORD,""))>0
  ;"  QUIT RESULT
  ;"  ;
ISMODIFR(IEN22733,WORD)  ;"IS WORD A RX NAME MODIFIER?  E.g. 'CD'
  SET WORD=$GET(WORD)
  NEW TMGRESULT SET TMGRESULT=0
  NEW IEN22733D02 SET IEN22733D02=0
  FOR  SET IEN22733D02=$ORDER(^TMG(22733,IEN22733,2,IEN22733D02)) QUIT:(IEN22733D02'>0)!(TMGRESULT=1)  DO
  . NEW MOD SET MOD=""
  . FOR  SET MOD=$ORDER(^TMG(22733,IEN22733,2,IEN22733D02,2,"B",MOD)) QUIT:(MOD="")!(TMGRESULT=1)  DO
  . . IF MOD=WORD SET TMGRESULT=1
  QUIT TMGRESULT
  ;
ISFORM(WORD)  ;"IS WORD A FORM (E.G. TAB)
  SET WORD=$GET(WORD)
  NEW RESULT SET RESULT=0
  IF (WORD'=""),",TAB,TABLET,"[(","_WORD_",") DO
  . SET RESULT=1
  QUIT RESULT
  ;
WORDINFO(WORD,DICT)  ;"characterize word
  ;"Input: WORD, PASS BY REFERENCE, AND IN AND OUT PARAMETER.  FORMAT
  ;"           WORD = <word to test>
  ;"           WORD("NUM")=# if word starts with number
  ;"           WORD("S2")=
  ;"           WORD("UNIT IEN")=
  ;"       DICT -- OPTIONAL.  ARRAY OF DICTIONARY WORDS. 
  NEW SAVE SET SAVE=WORD
  NEW DONE SET DONE=0
  IF 1=0 DO  ;"<-- removing block.  I don't remember why used, but was causing problems.  Turned OMEGA 3,6,9 --> OMEGA 369 (which didn't match)
  . NEW SPEC,EXTRA FOR EXTRA=",",";" IF WORD[EXTRA SET SPEC(EXTRA)=""
  . IF $DATA(SPEC) SET WORD=$$REPLACE^XLFSTR(WORD,.SPEC)
  NEW NUM SET NUM=""
  IF $DATA(DICT(WORD)) DO    ;"If 24HR is dictionary word, then don't parse to number etc.  
  . SET WORD("TYPE")=$GET(DICT(WORD))
  . MERGE WORD("DBSOURCE")=DICT("ZZSOURCE",WORD)
  IF WORD?1.4N1"/"1.4N1"/"1.4N DO   ;"e.g. #/#/#  as a date
  . SET WORD("DATE")=WORD,WORD=""
  DO
  . NEW PARTB SET NUM=$$NUMSTR^TMGSTUT3(WORD,.PARTB),WORD=PARTB
  ;"ELSE  IF WORD?0.1(1"+",1"-").NP1"/".NP DO   ;"Fractions, e.g. 1/2  or -0.4/.4
  ;". SET NUM=WORD,WORD=""
  ;"ELSE  FOR  QUIT:DONE!(WORD="")  DO
  ;". NEW CH SET CH=$EXTRACT(WORD,1)
  ;". IF "-.1234567890,"'[CH SET DONE=1 QUIT
  ;". SET WORD=$EXTRACT(WORD,2,999)
  ;". SET NUM=NUM_CH
  ;". IF NUM="0",($EXTRACT(WORD,1)'=".") SET NUM="0 "
  IF NUM["," SET NUM=$TRANSLATE(NUM,",","")
  SET WORD("NUM")=NUM
  SET WORD("S2")=WORD
  NEW TEMPWORD SET TEMPWORD=$$STRIPWD(WORD)
  NEW UNITIEN SET UNITIEN=0
  IF TEMPWORD'="",($DATA(DICT(WORD))!$DATA(DICT(TEMPWORD))) DO
  . SET UNITIEN=+$ORDER(^PS(50.607,"B",TEMPWORD,0))
  IF UNITIEN>0 SET WORD("UNIT IEN")=UNITIEN 
  ;"CHECK FOR TIME PATTERN, E.G. 8AM
  IF (",AM,PM,"[WORD("S2"))&(WORD("S2")'="") DO
  . NEW TIME SET TIME=WORD("NUM") SET:TIME'="" TIME=TIME_" "
  . SET TIME=TIME_WORD("S2")
  . SET WORD("TIME")=TIME
  SET WORD=SAVE
WIDN ;  
  QUIT
  ;  
GETNXW1(LINE,WORD,DICT)  ;"GET NEXT WORD, first pass
  ;"INPUT:  LINE -- The line to get next word from.  MODIFIED IF PASSED BY REFERENCE.  
  ;"        WORD-- PASS BY REFERENCE.  MODIFIED AS OUT PARAMETER.  
  ;"        DICT -- OPTIONAL.  Array of dictionary words for Rx, as made by GETDICT^TMGRX004
  ;"Results: none.
  KILL WORD
  NEW ALEN SET ALEN=9999
  FOR  SET ALEN=$ORDER(DICT("ZZLEN",ALEN),-1) QUIT:(ALEN'>0)!($DATA(WORD)>0)  DO
  . NEW AWORD SET AWORD=""
  . FOR  SET AWORD=$ORDER(DICT("ZZLEN",ALEN,AWORD)) QUIT:(AWORD="")!($DATA(WORD)>0)  DO
  . . NEW DIV SET DIV=""
  . . IF $$LMATCH^TMGSTUT3(LINE,AWORD)=0 QUIT
  . . NEW NEXTCH SET NEXTCH=$EXTRACT(LINE,$LENGTH(AWORD)+1)
  . . IF NEXTCH=".",$$ISNUM^TMGSTUT3(AWORD),$$NUMSTR^TMGSTUT3(LINE)'=AWORD QUIT  ;"prevent '2' from matching with '2.5' in line
  . . IF NEXTCH="/",LINE?1.2N1"/"1.2N1"/"1.4N.E QUIT  ;"DON'T MATCH DATES during dictionary match. (##/##/####)
  . . SET WORD=AWORD,LINE=$PIECE(LINE,AWORD,2,999)
  . . IF NEXTCH'="",$$DIVSET()[NEXTCH DO
  . . . SET DIV=NEXTCH,LINE=$EXTRACT(LINE,2,$LENGTH(LINE))
  . . SET WORD("DIV")=DIV
  . . DO WORDINFO(.WORD,.DICT)  ;"characterize word
  IF $DATA(WORD)=0 DO GETNXWRD(.LINE,.WORD,.DICT)
  QUIT
  ;
GETNXWRD(LINE,WORD,DICT)  ;"GET NEXT WORD, older method
  ;"INPUT:  LINE -- The line to get next word from.  MODIFIED IF PASSED BY REFERENCE.  
  ;"        WORD-- PASS BY REFERENCE.  MODIFIED AS OUT PARAMETER.  
  ;"        DICT -- OPTIONAL.  Array of dictionary words for Rx, as made by GETDICT^TMGRX004
  ;"Results: none.
  NEW DIVSET SET DIVSET=$$DIVSET()
  NEW TEMP,LEN,ST,IDX KILL WORD SET WORD=""
  SET LINE=$GET(LINE) QUIT:LINE=""
  NEW DIV
  SET WORD=$$NEXTWORD^TMGSTUT3(LINE,DIVSET,.DIV)
  NEW ISNUM SET ISNUM=$$ISNUM^TMGSTUT3(WORD)
  IF WORD="",DIV="." SET ISNUM=($$NUMSTR^TMGSTUT3(LINE)'="")  ;"handle .005%
  IF DIV="/",ISNUM,LINE?1.2N1"/"1.2N1"/"1.4N.E DO  ;"match dates at start of line: ##/##/####aaaaaaaaaa
  . SET WORD=$PIECE(LINE,"/",1,2)_"/"_+$PIECE(LINE,"/",3)
  . SET LINE=$PIECE(LINE,WORD,2,999),DIV=$EXTRACT(LINE,1)
  ELSE  IF "^/^,^.^"[("^"_DIV_"^"),ISNUM DO
  . NEW PARTB,CH
  . SET WORD=$$NUMSTR^TMGSTUT3(LINE,.PARTB),LINE=PARTB,DIV=""        
  . SET CH=$EXTRACT(LINE)
  . IF (CH'=""),DIVSET[CH SET DIV=CH,LINE=$EXTRACT(LINE,2,$LENGTH(LINE))
  ELSE  DO  ;"OLDER METHOD
  . SET LINE=$PIECE(LINE,DIV,2,99)
  . IF (DIV'=""),("/,"[DIV),ISNUM DO  
  . . ;"Look for fractions (e.g. 1/2), or date (e.g. #/#/#), or 1,000 pattern
  . . NEW NEXTSPCWORD SET NEXTSPCWORD=$PIECE(LINE," ",1)
  . . IF (+NEXTSPCWORD=NEXTSPCWORD)!(NEXTSPCWORD?1.4N1"/"1.4N) DO  QUIT
  . . . SET WORD=WORD_"/"_NEXTSPCWORD
  . . . SET LINE=$PIECE(LINE," ",2,99),DIV=" "
  . . IF DIV="/",NEXTSPCWORD?.N1"-".N DO    ;"Look for 1/2-1 pattern
  . . . SET WORD=WORD_"/"_$PIECE(NEXTSPCWORD,"-",1)
  . . . SET LINE=$PIECE(LINE," ",2,99)
  . . . SET LINE="-"_$PIECE(NEXTSPCWORD,"-",2,99)_" "_LINE,DIV=""
  . . IF NEXTSPCWORD?.N DO  QUIT
  . . . SET WORD=WORD_NEXTSPCWORD
  . . . SET LINE=$PIECE(LINE," ",2,99),DIV=" "
  . IF (DIV="."),$$ISNUM^TMGSTUT3(WORD) DO  ;"Look for decimals, e.g. 7.4
  . . NEW NEXTWORD DO PEEKNXWD(LINE,.NEXTWORD)  ;"GET NEXT WORD, just look, doesn't remove from line.
  . . IF NEXTWORD'?.N.E QUIT
  . . NEW PARTA,PARTB SET PARTA=+NEXTWORD 
  . . SET PARTB=$EXTRACT(NEXTWORD,$LENGTH(PARTA)+1,$LENGTH(NEXTWORD))
  . . SET LINE=PARTB_LINE,DIV=""    
  . . ;"IF (+NEXTWORD'>0)&($EXTRACT(NEXTWORD,1)'="0") QUIT
  . . ;"NEW TEMP DO GETNXWRD(.LINE,.TEMP)
  . . ;"SET WORD=WORD_"."_TEMP,DIV=$GET(TEMP("DIV"))
  SET WORD("DIV")=DIV
  DO WORDINFO(.WORD,.DICT)  ;"characterize word
  IF ($GET(WORD("NUM"))'=""),($GET(WORD("S2"))'="") DO
  . SET LINE=WORD("S2")_WORD("DIV")_LINE    ;"if we got 81mg, keep 81 and put mg back onto LINE
  . SET WORD("S2")=""
  . NEW TEMP SET TEMP=WORD("NUM")
  . KILL WORD SET WORD=TEMP,WORD("DIV")=" "
  . DO WORDINFO(.WORD,.DICT)
  . ;"SET WORD=WORD("NUM"),WORD("UNIT IEN")="",WORD("DIV")=" "
  QUIT  
  ;  
DIVSET()  ;"Standard characters to divide up words.
  QUIT " ,/-;:().+"
  ;
SUBDVSET()  ;
  QUIT " ,/;:()."   ;"//used for trimming chars from preface and sig, don't trim "+" or ""-"
  ;
PEEKNXWD(LINE,WORD,IDX)  ;"GET NEXT WORD, just look, doesn't remove from line.
  ;"INPUT: LINE -- line to look at 
  ;"       WORD -- PASS BY REFERNCE, AN OUT PARAMETER
  ;"       IDX -- OPTIONAL, DEFAULT is 1.  If 2, for example, then 2nd next word returned
  ;"Result: none 
  NEW TEMP SET TEMP=LINE
  KILL WORD SET WORD=""
  SET IDX=+$GET(IDX,1) IF IDX'>0 SET IDX=1
  NEW CT FOR CT=1:1:IDX DO
  . DO GETNXWRD(.TEMP,.WORD)
  QUIT
  ;
PUTBKWRD(LINE,WORD)  ;"PUT BACK WORD
  NEW DIV SET DIV=$GET(WORD("DIV"))
  ;"IF ("/-"[DIV)&(DIV'="")&($EXTRACT(LINE,1)=DIV) SET DIV=""
  SET LINE=WORD_DIV_LINE
  QUIT
  ;  
WORDLEN(LINE,DICT)  ;"return length of line, E.G. 'hello-there/world you' as 4 words
  ;"LINE -- The line to count
  ;"DICT -- optional array of dictionary words for Rx.  
  NEW TEMPLINE SET TEMPLINE=LINE
  NEW TEMPWORD
  NEW CT SET CT=0
  FOR  QUIT:(TEMPLINE="")  DO
  . DO GETNXWRD(.TEMPLINE,.TEMPWORD,.DICT) SET CT=CT+1
  QUIT CT
  ;   
STRIPWD(WORD) ;"STRIP PUNCTUATION
  NEW RESULT SET RESULT=WORD
  IF (RESULT?.N1".".N) DO  ;"e.g. "7.5"
  . SET RESULT=$TRANSLATE(WORD,",;:'","")
  ELSE  DO  ;"NOTE: this will fail if input was "7.5.", because it will --> 75
  . SET RESULT=$TRANSLATE(WORD,",.;:'","")
  QUIT RESULT
  ;
;"GETMDIEN(OUT,LINE)  ;"GET MEDICATION IEN (IEN22733) FROM LINE
GETMDIEN(LINE)  ;"GET MEDICATION IEN (IEN22733) FROM LINE
  ;"Input: delete later --> OUT -- PASS BY REFERENCE.  The array that is being filled with information
  ;"       LINE -- the input line containing line from drug table.
  ;"               NOTE:  If there are words that appear before medication name,
  ;"                    then they are put into OUT("PREFACE").  But otherwise
  ;"                    the line is not altered.  The medication name is not removed.
  ;"   NOTE2: I have changed the flow so that I only want this function
  ;"         to identify the IEN.  I don't want it to modify line
  ;"        or OUT.  So the caller of this function will be ignoring
  ;"        changes to LINE OR OUT.
  ;"Result : IEN22733
  NEW WORD,IDX
  NEW LEN SET LEN=$$WORDLEN(LINE)
  ;"NEW STRENGTHPREFIX SET STRENGTHPREFIX=""   ;"To handle situations like:  81 MG ASA
  NEW IEN22733 SET IEN22733=0
  FOR I=1:1:LEN DO  QUIT:IEN22733>0
  . KILL WORD
  . SET IEN22733=$$MEDIEN2(.OUT,.LINE,.WORD)  ;"//first try direct pattern match with pre-entered names and aliases.    
  . IF IEN22733>0 QUIT
  . KILL WORD DO GETNXWRD(.LINE,.WORD)
  . SET IEN22733=$$MEDIEN(,.WORD,.LINE)   ;"//next try older method.  Would work on LISINOPRIL/ HCTZ based on ingredients
  . IF IEN22733>0 QUIT
  . ;"//--- No Match -------------------------
  . ;"IF (WORD("NUM")>0)!(WORD("UNIT IEN")>0) DO
  . ;". SET STRENGTHPREFIX=STRENGTHPREFIX_WORD_" "
  . ELSE  DO
  . . ;"IF $$TRIM^XLFSTR(WORD)="" QUIT  <-- no, WORD might be "", but still have DIV="("
  . . ;"NEW PRE SET PRE=$GET(OUT("PREFACE"))
  . . ;"NEW LASTCH SET LASTCH=$EXTRACT(PRE,$LENGTH(PRE))
  . . ;"IF PRE'="",($$DIVSET()'[LASTCH) SET PRE=PRE_" "
  . . ;"SET OUT("PREFACE")=PRE_WORD_$GET(WORD("DIV"))
  . ;"SET OUT("WORKING")=LINE
  ;"SET OUT("IEN22733")=IEN22733
  ;"DO PUTBKWRD^TMGRX004(.LINE,.WORD)
  ;"IF STRENGTHPREFIX'="" SET LINE=STRENGTHPREFIX_LINE
  ;"SET OUT("WORKING")=LINE
  QUIT IEN22733
  ; 
MEDIEN(OUT,NAME,LINE)  ;"GET MEDICATION IEN  -- older method
  ;"Input: OUT -- Used when calling self recursively. 
  ;"       NAME -- 
  ;"       LINE -- 
  ;"Result: IEN22733
  NEW IEN22733 SET IEN22733=0
  IF $GET(NAME)="" GOTO MDINDN
  SET LINE=$GET(LINE)
  NEW ORIGLINE SET ORIGLINE=NAME_NAME("DIV")_LINE
  NEW NEXTWORD SET NEXTWORD=""
  NEW DIV SET DIV=$GET(NAME("DIV"))
  NEW LEN SET LEN=$$WORDLEN(LINE)
  NEW PEEKIDX FOR PEEKIDX=1:1:LEN DO  QUIT:(NEXTWORD'="")
  . DO PEEKNXWD(.LINE,.NEXTWORD,PEEKIDX)
  . IF NEXTWORD="" DO
  . . SET DIV=$GET(NEXTWORD("DIV"))  ;"HANDLE LISINOPRIL /HCTZ <-- LISINOPRILS'S DIV IS "", "/" is with NEXT char
  . . SET NAME("DIV")="" ;"remove space following first word
  SET OUT("TYPE")="?"
  NEW DUALNAME SET DUALNAME=NAME_DIV_NEXTWORD  ;"Prevent TOPROL from prematurely matching, missing TOPROL XL
  NEW FMIDX FOR FMIDX="B","B2","BBV","BRAND" QUIT:IEN22733>0  DO
  . SET IEN22733=$ORDER(^TMG(22733,FMIDX,DUALNAME,0))
  . IF IEN22733'>0 QUIT
  . IF (FMIDX="B")!(FMIDX="B2")!(FMIDX="BBV") DO  QUIT  ;"B2 IS GENERIC ALIASES
  . . SET OUT("TYPE")="GENERIC"
  . . IF (FMIDX'="B") SET NAME=$PIECE($GET(^TMG(22733,IEN22733,0)),"^",1) ;"change alias generic/ABBV to proper generic name.  
  . IF FMIDX="BRAND" DO  QUIT
  . . SET OUT("TYPE")="BRAND"
  . . ;"Later, I could change brand alias (possibly mispelled), to preferred brand name  
  FOR FMIDX="B","B2","BBV","BRAND" QUIT:IEN22733>0  DO
  . SET IEN22733=$ORDER(^TMG(22733,FMIDX,NAME,0))
  . IF IEN22733'>0 QUIT
  . IF (FMIDX="B")!(FMIDX="B2")!(FMIDX="BBV") DO  QUIT  ;"B2 IS GENERIC ALIASES
  . . SET OUT("TYPE")="GENERIC"
  . . IF FMIDX'="B" SET NAME=$PIECE($GET(^TMG(22733,IEN22733,0)),"^",1) ;"change alias generic/ABBV to proper generic name.  
  . IF FMIDX="BRAND" DO  QUIT
  . . SET OUT("TYPE")="BRAND"
  . . ;"Later, I could change brand alias (possibly mispelled), to preferred brand name
  SET IEN22733=+IEN22733
  IF IEN22733'>0 DO  ;"LOOK FOR PARTIAL MATCHES, E.G. 'VITAMIN D3'
  . NEW DONE SET DONE=0
  . FOR FMIDX="B","B2","BBV","BRAND" QUIT:IEN22733>0  DO
  . . NEW TEMP SET TEMP=NAME
  . . FOR  SET TEMP=$ORDER(^TMG(22733,FMIDX,TEMP)) QUIT:(TEMP="")!(IEN22733>0)!DONE  DO
  . . . IF $$LMATCH^TMGSTUT3(TEMP,NAME)=0 QUIT
  . . . IF ORIGLINE'[TEMP QUIT
  . . . NEW TEMPWORD SET TEMPWORD=NAME
  . . . NEW IDX FOR IDX=1:1:$LENGTH(LINE," ") DO
  . . . . SET TEMPWORD=TEMPWORD_" "_$PIECE(LINE," ",IDX)
  . . . . IF TEMP'[TEMPWORD SET IDX=999 QUIT
  . . . . IF TEMPWORD=TEMP DO  QUIT
  . . . . . SET IEN22733=$ORDER(^TMG(22733,FMIDX,TEMP,0))
  . . . . . SET IDX=999
  . . . . . SET NAME=TEMPWORD,NAME("DIV")=" "
  . . . . . SET LINE=$$TRIM^XLFSTR($EXTRACT(ORIGLINE,$LENGTH(NAME)+1,$LENGTH(ORIGLINE)))
  . . . . . ;"redo nextword
  . . . . . NEW LEN SET LEN=$$WORDLEN(LINE)
  . . . . . NEW PEEKIDX FOR PEEKIDX=1:1:LEN DO  QUIT:(NEXTWORD'="")
  . . . . . . DO PEEKNXWD(.LINE,.NEXTWORD,PEEKIDX)
  . . . . IF IEN22733>0 DO
  . . . . . IF (FMIDX="B")!(FMIDX="B2")!(FMIDX="BBV") SET OUT("GENERIC")=NAME
  . . . . . ELSE  IF FMIDX="BRAND" SET OUT("BRAND")=NAME
  . SET IEN22733=+IEN22733
  ;"IF IEN22733>0 DO  ;"See if next word is a modifier, e.g. GLUCOTROL XL
  ;". IF $$ISMODIFR(IEN22733,.NEXTWORD) DO  ;"IS RX NAME MODIFIER?
  ;". . SET OUT("NAME MODIFIER")=NEXTWORD
  ;". . DO GETNXWRD(.LINE,.NEXTWORD)  ;"REMOVE NEXTWORD FROM LINE
  ;"ADDED FEATURE, HANDLE COMBO DRUGS, LIKE LISINOPRIL / HCTZ
  IF (IEN22733>0),($GET(NEXTWORD)'=""),(DIV'=""),(",/,-,"[DIV)  DO
  . NEW RXCT SET RXCT=1        
  . NEW TEMPRX MERGE TEMPRX=NEXTWORD
  . NEW DRUGS SET DRUGS(RXCT)=IEN22733  ;"DRUGS array holds IEN's
  . NEW DONE SET DONE=0
  . ;"LOAD ALL COMBO RX INGREDIENTS INTO DRUGS ARRAY
  . FOR  DO  QUIT:DONE!(TEMPRX="")  DO
  . . NEW ARRM,TEMPIEN SET TEMPIEN=$$MEDIEN(.ARR,.TEMPRX)
  . . IF TEMPIEN'>0 SET DONE=1 QUIT
  . . SET RXCT=RXCT+1,DRUGS(RXCT)=TEMPIEN
  . . NEW DIV SET DIV=$GET(TEMPRX("DIV"))
  . . IF (DIV="")!(",/,-,"'[DIV) SET DONE=1 QUIT
  . . SET PEEKIDX=PEEKIDX+1
  . . DO PEEKNXWD(LINE,.TEMPRX,PEEKIDX) 
  . ;"Find array of Rx's containing ingredients in DRUGS
  . NEW IDXARR,IDX SET IDX=0
  . FOR  SET IDX=$ORDER(DRUGS(IDX)) QUIT:IDX'>0  DO
  . . NEW IEN SET IEN=$GET(DRUGS(IDX)) QUIT:IEN'>0
  . . NEW SUBIEN SET SUBIEN=0
  . . FOR  SET SUBIEN=$ORDER(^TMG(22733,"AINGRED",IEN,SUBIEN)) QUIT:SUBIEN'>0  DO
  . . . SET IDXARR(SUBIEN)=$GET(IDXARR(SUBIEN))+1
  . . . SET IDXARR(SUBIEN,IEN)=""
  . ;"NOW SEE WHICH RX'S contain the specified ingredients
  . NEW IEN SET IEN=0 SET DONE=0  
  . FOR  SET IEN=$ORDER(IDXARR(IEN)) QUIT:(IEN'>0)!DONE  DO 
  . . IF $GET(IDXARR(IEN))'=RXCT QUIT
  . . ;"Will just use first found match.  Shouldn't be more than one match...
  . . SET IEN22733=IEN
  . . FOR IDX=1:1:PEEKIDX DO
  . . . DO GETNXWRD(.LINE,.NEXTWORD)  ;"cut off Rx words that we have already considered.
MDINDN ;  
  QUIT IEN22733
  ;
MEDXR() ;"GET STORAGE LOCATION OF MEDICATION XREF
  ;"QUIT $NAME(^TMP("MEDIEN2^TMGRX004",$J))
  QUIT $NAME(^TMG(22733,"MED_SPECIAL"))
  ;
KILLXR ;"Entry point for cross Fileman reference
  ;"This will kill special array, and then it will be rebuild when next needed.  
  NEW REF SET REF=$$MEDXR()
  KILL @REF
  QUIT
  ;
BLDMEDXR  ;"BUILD MEDICATION XREF
  ;"Setup special formated version of XRef's
  NEW REF SET REF=$$MEDXR()
  KILL @REF  
  NEW OREF SET OREF=$$OREF^DILF(REF)
  FOR IDX="B","B2","BBV","BRAND" DO MDI2ADD(OREF_""""_IDX_""",","^TMG(22733,"""_IDX_""",")
  SET @REF=$$NOW^XLFDT
  QUIT
  ;
ENSURMXR  ;"ENSURE MEDICATION XREF
  NEW REF SET REF=$$MEDXR()
  ;"IF $DATA(@REF) DO  ;"See if special formated version of XRef's already exists
  ;". NEW LASTUPDATE SET LASTUPDATE=+$GET(@REF)
  ;". NEW DELTA SET DELTA=$$FMDIFF^XLFDT($$NOW^XLFDT,LASTUPDATE,2)  ;"results in seconds.
  ;". IF DELTA>60*5 DO
  ;". . KILL @REF  ;"delete if data is more than _5_ minutes old.  
  IF $DATA(@REF)=0 DO BLDMEDXR ;"Setup special formated version of XRef's
  QUIT
  ;
MEDIEN2(OUT,LINE,WORD)  ;"GET MEDICATION IEN, ALT VERSION
  ;"Input: OUT -- PASS BY REFERENCE.  The array that is being filled with information
  ;"       LINE -- the input line containing line from drug table.
  ;"       WORD -- PASS BY REFERENCE.  Filled with Rx name, if found.  
  ;"Result : IEN22733
  SET WORD=""
  NEW REF,OREF SET REF=$$MEDXR(),OREF=$$OREF^DILF(REF)
  NEW IDX,LONGEST SET LONGEST=0
  DO ENSURMXR
  NEW TEMPARR,IEN22733 SET IEN22733=0
  FOR IDX="B","B2","BBV","BRAND" DO
  . NEW LONGEST SET LONGEST=$$MAXLMTCH(OREF_""""_IDX_""",",LINE)  ;"Get longest match. 
  . NEW LEN SET LEN=$LENGTH(LONGEST) QUIT:LEN=0
  . IF LEN=28 DO  ;"Match is limited to 28 chars by technical reasons, So look for longer match with actual value. 
  . . NEW PRIORSTR SET PRIORSTR=$$SUBASCII^TMGSTUT3(LONGEST)
  . . NEW NAME SET NAME=PRIORSTR
  . . NEW DONE SET DONE=0
  . . FOR  SET NAME=$ORDER(^TMG(22733,IDX,NAME)) QUIT:(NAME="")!DONE  DO
  . . . IF $$LMATCH^TMGSTUT3(LINE,NAME)=0 SET DONE=1 QUIT
  . . . SET IEN22733=0
  . . . FOR  SET IEN22733=$ORDER(^TMG(22733,IDX,NAME,IEN22733)) QUIT:IEN22733'>0  DO
  . . . . NEW IENS SET IENS=IEN22733_","
  . . . . IF "^B2^BBV^BRAND^"[("^"_IDX_"^") SET IENS=+$ORDER(^TMG(22733,IDX,NAME,IEN22733,0))_","_IENS
  . . . . NEW FILE SET FILE=0
  . . . . IF IDX="B" SET FILE=22733
  . . . . ELSE  IF IDX="B2" SET FILE=22733.022
  . . . . ELSE  IF IDX="BBV" SET FILE=22733.04
  . . . . ELSE  IF IDX="BRAND" SET FILE=22733.01
  . . . . NEW FULLNAME SET FULLNAME=$$GET1^DIQ(FILE,IENS,.01)
  . . . . IF $$LMATCH^TMGSTUT3(LINE,FULLNAME)=0 QUIT
  . . . . SET TEMPARR($LENGTH(FULLNAME),FULLNAME)=IEN22733_"^"_IDX
  . ELSE  DO
  . . NEW FOLLOWCH SET FOLLOWCH=$EXTRACT(LINE,$LENGTH(LONGEST)+1)  
  . . IF FOLLOWCH'="",$$DIVSET()'[FOLLOWCH QUIT ;"Prevent "ACET" (an alias for acetaminophen) with matching with ACETAZOLAMIDE.
  . . SET IEN22733=+$ORDER(^TMG(22733,IDX,LONGEST,0)) 
  . . QUIT:IEN22733'>0 
  . . SET TEMPARR($LENGTH(LONGEST),LONGEST)=IEN22733_"^"_IDX
  NEW MAXLEN SET MAXLEN=+$ORDER(TEMPARR(""),-1)
  SET WORD=$ORDER(TEMPARR(MAXLEN,""))
  NEW VALUE SET VALUE=$GET(TEMPARR(MAXLEN,WORD))
  SET IEN22733=+VALUE,IDX=$PIECE(VALUE,"^",2)
  IF IEN22733>0 DO
  . IF IDX'="","^B^BBV^B2^"[("^"_IDX_"^") SET OUT("TYPE")="GENERIC"  ;",OUT("GENERIC")=WORD
  . IF IDX'="","^BRAND^"[("^"_IDX_"^") SET OUT("TYPE")="BRAND"   ;",OUT("BRAND")=WORD
  . SET LINE=$EXTRACT(LINE,$LENGTH(WORD)+1,$LENGTH(LINE))
  QUIT IEN22733
  ;
MDI2ADD(OUTREF,REF) ;"ADD ARRAY INDEX TO OUT OUT ARRAY
  SET REF=$$CREF^DILF(REF)
  IF OUTREF'["(" SET OUTREF=OUTREF_"("
  SET OUTREF=$$OREF^DILF(OUTREF)
  NEW NAME SET NAME=""
  FOR  SET NAME=$ORDER(@REF@(NAME)) QUIT:NAME=""  DO
  . NEW LEN SET LEN=$LENGTH(NAME)
  . IF LEN>28 SET LEN=28  ;"APPARENTLY THERE IS A LIMIT TO NUMBER OF ALLOWED SUBSCRIPTS
  . NEW ADDREF SET ADDREF=""
  . NEW JDX FOR JDX=1:1:LEN DO
  . . SET ADDREF=ADDREF_""""_$EXTRACT(NAME,JDX)_""","
  . . NEW TEMPREF SET TEMPREF=OUTREF_ADDREF
  . . SET TEMPREF=$$CREF^DILF(TEMPREF)
  . . NEW VALUE SET VALUE=$GET(@TEMPREF) 
  . . IF VALUE="",JDX=LEN SET VALUE=1
  . . SET @TEMPREF=VALUE
  QUIT
  ;
MAXLMTCH(REFARR,STR)  ;"findest longest entry in @REFARR, LMATCH'ing with STR
  ;"INPUT:  REFARR -- PASS BY NAME.  Expected format: 
  ;"          @REFARR@("A","P","P","L","E")=1
  ;"          @REFARR@("A","P","P","L","Y")=1
  ;"          @REFARR@("A","P","P","A","R","T")=1
  ;"          @REFARR@("A","P","P","A","R","T","M","E","N","T")=1  <-- 1 indicates a complete word
  ;"        STR -- The string to test.
  ;"Result: "" if not found, else returns REF for longest match
  ;"------------------
  NEW RESULT SET RESULT=""
  SET REFARR=$$OREF^DILF(REFARR)
  NEW DATA,ADDREF SET ADDREF=""
  NEW VALUE SET VALUE=""
  NEW MATCHES
  NEW LEN SET LEN=$LENGTH(STR)
  IF LEN>28 SET LEN=28  ;"THERE IS A LIMIT TO NUMBER OF ALLOWED SUBSCRIPTS
  NEW JDX FOR JDX=1:1:LEN DO  QUIT:(DATA=0)  
  . NEW CH SET CH=$EXTRACT(STR,JDX) IF CH="""" SET CH=""""""
  . SET ADDREF=ADDREF_""""_CH_""","
  . NEW CREF SET CREF=$$CREF^DILF(REFARR_ADDREF)
  . SET DATA=$DATA(@CREF)
  . IF DATA>0 SET RESULT=ADDREF
  . IF DATA#10=1 DO   ;"Can have entries like 'TYLENOL' and 'TYLENOL PM', so keep searching
  . . SET VALUE=$GET(@CREF)
  . . IF VALUE=1 SET MATCHES($LENGTH(RESULT),RESULT)=""
  NEW MAXLEN SET MAXLEN=$ORDER(MATCHES(""),-1)
  SET RESULT=$ORDER(MATCHES(MAXLEN,""))
  NEW TEMP SET TEMP=RESULT,RESULT=""
  SET LEN=$LENGTH(TEMP,",")
  FOR JDX=1:1:LEN DO   ;"COLLAPSE RESULT (CONTAINING ADDREF) BACK INTO A STRING
  . NEW LETTER SET LETTER=$PIECE(TEMP,",",1),TEMP=$PIECE(TEMP,",",2,LEN)
  . SET RESULT=RESULT_$EXTRACT(LETTER,2)  
  QUIT RESULT
  ;
MATCHUNT(OUT,WORD)  ;"TRY TO MATCH UNITS TO DATABASE.
  NEW IEN22733 SET IEN22733=+$GET(OUT("IEN22733")) IF IEN22733'>0 QUIT
  NEW ASRC SET ASRC=""
  FOR  SET ASRC=$ORDER(WORD("DBSOURCE",ASRC)) QUIT:ASRC=""  DO
  . NEW FILE SET FILE=+ASRC 
  . NEW FLD SET FLD=$PIECE(ASRC,"^",2)
  . NEW IENS SET IENS=$PIECE(ASRC,"^",3)
  . IF FILE=22733.32 DO
  . . SET FILE=22733.03,FLD=.02
  . . SET IENS=$PIECE(IENS,",",2,99)
  . IF FILE'=22733.03 QUIT
  . NEW FORMIEN SET FORMIEN=$PIECE(IENS,",",2)
  . NEW STRIEN SET STRIEN=+IENS
  . NEW UNITIEN SET UNITIEN=$$GET1^DIQ(22733.03,IENS,.02,"I")
  . SET OUT("UNITS","IEN50.607")=UNITIEN
  . SET OUT("UNITS","DATABASE")=$PIECE($GET(^PS(50.607,UNITIEN,0)),"^",1)
  . SET OUT("UNITS","PREFERRED")=$$GETPRUNT(IEN22733,FORMIEN,STRIEN,0)
  QUIT
  ;
MTCHSTRT(OUT,WORD)  ;"TRY TO MATCH STRENGTHS TO DATABASE.  
  ;"Input:  OUT -- pass by refrence.  An IN and OUT parameter.  
  ;"          Input below:
  ;"            OUT("IEN22733") -- the matched drug record in 22733
  ;"            OUT("STRENGTH")=<STRENGTH>  <-- strength as found on input line. REQUIRED
  ;"          Output below: 
  ;"            OUT("STRENGTH","IENS",<IENS IN 22733.03>)=<FORM>^<Strength>^<Preferred Alias>  <-- may be multiple. 
  ;"            OUT("STRENGTH","DATABASE")=<registered strength strength for last IENS found> <-- if found.
  ;"         WORD -- Array that contains information about strength word.  May contain info as follows
  ;"            WORD("DBSOURCE",'<FILE#>^<FLD#>^<IENS>')=""  <-- might be multiple
  ;"Results: none
  NEW IEN22733 SET IEN22733=+$GET(OUT("IEN22733")) IF IEN22733'>0 QUIT
  NEW ASRC SET ASRC=""
  FOR  SET ASRC=$ORDER(WORD("DBSOURCE",ASRC)) QUIT:ASRC=""  DO
  . NEW FILE SET FILE=+ASRC 
  . NEW IENS SET IENS=$PIECE(ASRC,"^",3)
  . IF FILE=22733.31 DO
  . . SET FILE=22733.03
  . . SET IENS=$PIECE(IENS,",",2,99)
  . IF FILE'=22733.03 QUIT
  . NEW FLD SET FLD=$PIECE(ASRC,"^",2)
  . NEW FORMIEN SET FORMIEN=$PIECE(IENS,",",2)
  . NEW STRIEN SET STRIEN=+IENS
  . NEW PARENTIENS SET PARENTIENS=$PIECE(IENS,",",2,99)
  . NEW FORM SET FORM=$$GET1^DIQ(22733.02,PARENTIENS,.01)
  . NEW STRENGTH SET STRENGTH=$$GET1^DIQ(22733.03,IENS,.01)
  . NEW STRALIAS SET STRALIAS=$$GETPSRA(IEN22733,FORMIEN,STRIEN)  ;"GET PREFERRED STRENGTH ALIAS
  . SET OUT("STRENGTH","IENS",IENS)=FORM_"^"_STRENGTH_"^"_STRALIAS
  . SET OUT("STRENGTH","DATABASE")=STRENGTH
  IF $DATA(OUT("STRENGTH","IENS")) GOTO MDSEDN
  NEW STRENGTH SET STRENGTH=$GET(OUT("STRENGTH")) IF STRENGTH="" QUIT
  NEW STRIPSTRENGTH SET STRIPSTRENGTH=$TRANSLATE(STRENGTH,",","")
  NEW IENS SET IENS=""
  NEW FORMIEN SET FORMIEN=0
  FOR  SET FORMIEN=$ORDER(^TMG(22733,IEN22733,2,FORMIEN)) QUIT:FORMIEN'>0  DO
  . NEW IEN50D606 SET IEN50D606=$PIECE($GET(^TMG(22733,IEN22733,2,FORMIEN,0)),"^",1) 
  . NEW FORM SET FORM=$$GET1^DIQ(50.606,IEN50D606,.01)
  . NEW STRENGTHIEN SET STRENGTHIEN=0
  . FOR  SET STRENGTHIEN=$ORDER(^TMG(22733,IEN22733,2,FORMIEN,1,STRENGTHIEN)) QUIT:STRENGTHIEN'>0  DO
  . . NEW ASTRENGTH SET ASTRENGTH=$PIECE($GET(^TMG(22733,IEN22733,2,FORMIEN,1,STRENGTHIEN,0)),"^",1)
  . . IF (ASTRENGTH=STRENGTH)!(ASTRENGTH=STRIPSTRENGTH) DO  QUIT
  . . . SET IENS=STRENGTHIEN_","_FORMIEN_","_IEN22733_","
  . . . SET OUT("STRENGTH","IENS",IENS)=FORM_"^"_$$GET1^DIQ(22733.03,IENS,.01)_"^"_$$GETPSRA(IEN22733,FORMIEN,STRENGTHIEN)  ;"GET PREFERRED STRENGTH ALIAS   
  . . IF ASTRENGTH["/" DO
  . . . NEW TEMPSTRENGTH SET TEMPSTRENGTH=$$REPLSTR^TMGSTUT3(ASTRENGTH,"/","-")
  . . . IF (TEMPSTRENGTH=STRENGTH)!(TEMPSTRENGTH=STRIPSTRENGTH) DO  QUIT
  . . . . SET IENS=STRENGTHIEN_","_FORMIEN_","_IEN22733_","
  . . . . SET OUT("STRENGTH","IENS",IENS)=FORM_"^"_$$GET1^DIQ(22733.03,IENS,.01)_"^"_$$GETPSRA(IEN22733,FORMIEN,STRENGTHIEN)  
  . . IF ASTRENGTH["-" DO
  . . . NEW TEMPSTRENGTH SET TEMPSTRENGTH=$$REPLSTR^TMGSTUT3(ASTRENGTH,"-","/-")
  . . . IF (TEMPSTRENGTH=STRENGTH)!(TEMPSTRENGTH=STRIPSTRENGTH) DO  QUIT
  . . . . SET IENS=STRENGTHIEN_","_FORMIEN_","_IEN22733_","
  . . . . SET OUT("STRENGTH","IENS",IENS)=FORM_"^"_$$GET1^DIQ(22733.03,IENS,.01)_"^"_$$GETPSRA(IEN22733,FORMIEN,STRENGTHIEN)  
  . . NEW AIEN SET AIEN=0
  . . FOR  SET AIEN=$ORDER(^TMG(22733,IEN22733,2,FORMIEN,1,STRENGTHIEN,1,AIEN)) QUIT:AIEN'>0  DO  ;"STRENGTH ALIASES
  . . . SET ASTRENGTH=$PIECE($GET(^TMG(22733,IEN22733,2,FORMIEN,1,STRENGTHIEN,1,AIEN,0)),"^",1)
  . . . IF (ASTRENGTH=STRENGTH)!(ASTRENGTH=STRIPSTRENGTH) DO  QUIT
  . . . . SET IENS=STRENGTHIEN_","_FORMIEN_","_IEN22733_","
  . . . . SET OUT("STRENGTH","IENS",IENS)=FORM_"^"_$$GET1^DIQ(22733.03,IENS,.01)_"^"_$$GETPSRA(IEN22733,FORMIEN,STRENGTHIEN)  
  IF IENS'="" DO
  . SET OUT("STRENGTH","DATABASE")=$$GET1^DIQ(22733.03,IENS,.01)
MDSEDN ;  
  QUIT
  ;  
MOD2FRM(ARR)  ;"Get correct IENS (which specifies correct form), based on matching modifiers
  ;"E.g. if there is a 1,000 mcg strength for both PO and IM, then use presence of modifier IM to pick correct IM form
  ;"  ... Likewise, if there is NO modifier specified, then pick form that does not
  ;"     have any modifiers.  
  ;"Input: ARR -- The array as created by PARSELN^TMGRX001
  ;"RESULT: Returns correct IENS, or none if can't match.  
  NEW RESULT SET RESULT=""
  NEW IEN22733 SET IEN22733=+$GET(ARR("IEN22733")) GOTO:IEN22733'>0 M2FRDN  
  NEW MOD SET MOD=$GET(ARR("MODIFIER")) 
  IF MOD="" SET MOD="{{NONE}}"
  ;"IF MOD="" GOTO M2FRDN
  NEW MODSARR
  NEW IENSARR MERGE IENSARR=ARR("STRENGTH","IENS")  
  NEW IENS SET IENS=""
  FOR  SET IENS=$ORDER(IENSARR(IENS)) QUIT:(IENS="")!(RESULT'="")  DO
  . NEW SUBIEN SET SUBIEN=+$PIECE(IENS,",",2) QUIT:SUBIEN'>0
  . NEW ARR
  . DO ADDRFDCT^TMGRX004(.ARR,$NAME(^TMG(22733,IEN22733,2,SUBIEN,2,"B")),IEN22733,"MODIFIER")
  . IF $DATA(ARR)=0 SET ARR("{{NONE}}")="MODIFIER"
  . MERGE MODSARR(IENS)=ARR
  . ;"IF $DATA(ARR(MOD))=0 QUIT
  . ;"SET RESULT=IENS
  ;
  SET IENS=""
  FOR  SET IENS=$ORDER(MODSARR(IENS)) QUIT:(IENS="")!(RESULT'="")  DO
  . IF $GET(MODSARR(IENS,MOD))'="MODIFIER" QUIT
  . SET RESULT=IENS
  ;"IF RESULT="" KILL IENSARR
  ;"IF $DATA(IENSARR)=0 DO
  ;". NEW SUBIEN SET SUBIEN=0
  ;". FOR  SET SUBIEN=$ORDER(^TMG(22733,IEN22733,2,SUBIEN)) QUIT:(SUBIEN'>0)!(RESULT'="")  DO
  ;". . NEW ARR
  ;". . DO ADDRFDCT^TMGRX004(.ARR,$NAME(^TMG(22733,IEN22733,2,SUBIEN,2,"B")),IEN22733,"MODIFIER")
  ;". . IF $DATA(ARR(MOD))=0 QUIT
  ;". . NEW IENS SET IENS=+$ORDER(^TMG(22733,IEN22733,2,SUBIEN,2,"B",MOD,0))_","_SUBIEN_","_IEN22733_","
  ;". . SET RESULT=IENS   
M2FRDN ;  
  QUIT RESULT
  ;  
GETPSRA(IEN22733,FORMIEN,STRIEN,GETANY)  ;"GET PREFERRED STRENGTH ALIAS
  NEW ROOT SET ROOT=$NAME(^TMG(22733,IEN22733,2,FORMIEN,1,STRIEN))
  QUIT $$GETPREF(ROOT,.GETANY)
  ;
GETPRBRD(IEN22733,GETANY)  ;"GET PREFERRED BRAND NAME
  NEW ROOT SET ROOT=$NAME(^TMG(22733,IEN22733,1))
  QUIT $$GETPREF(ROOT,.GETANY)
  ;      
GETPRGA(IEN22733,GETANY)  ;"GET PREFERRED GENERIC ALIAS
  NEW ROOT SET ROOT=$NAME(^TMG(22733,IEN22733,.02))
  QUIT $$GETPREF(ROOT,.GETANY)
  ;  
GETPRGB(IEN22733,GETANY)  ;"GET PREFERRED GENERIC ABBREVIATION
  NEW ROOT SET ROOT=$NAME(^TMG(22733,IEN22733,3))
  QUIT $$GETPREF(ROOT,.GETANY)
  ;                   
GETPRUNT(IEN22733,FORMIEN,STRIEN,GETANY) ;"GET PREFERRED UNITS FOR FORM, STRENGTH
  NEW ROOT SET ROOT=$NAME(^TMG(22733,IEN22733,2,FORMIEN,1,STRIEN,2))
  NEW RESULT SET RESULT=$$GETPREF(ROOT,.GETANY)
  IF RESULT="" DO
  . NEW IENS SET IENS=STRIEN_","_FORMIEN_","_IEN22733_","
  . SET RESULT=$$GET1^DIQ(22733.03,IENS,.02)
  QUIT RESULT
  ;       
GETPRFAL(IEN22733,FORMIEN,GETANY) ;"GET PREFERRED FORM ALIAS
  NEW ROOT SET ROOT=$NAME(^TMG(22733,IEN22733,2,FORMIEN,.05))
  QUIT $$GETPREF(ROOT,.GETANY)
  ;
GETPREF(ROOT,GETANY)  ;"GET PREFERRED ENTRY (VALUE IN PIECE 1, PREF IN PIECE 2)
  ;"INPUT: ROOT -- an closed reference of subfile to search
  ;"       GETANY -- optional.  Default = 1.  
  ;"               If 1, then something will be returned, even if none are marked PREFERRED
  SET GETANY=$GET(GETANY,1)
  NEW RESULT,ANY SET (RESULT,ANY)=""
  NEW SUBIEN SET SUBIEN=0
  FOR  SET SUBIEN=$ORDER(@ROOT@(SUBIEN)) QUIT:(SUBIEN'>0)!(RESULT'="")  DO
  . NEW ZN SET ZN=$GET(@ROOT@(SUBIEN,0)) QUIT:ZN=""
  . SET ANY=$PIECE(ZN,"^",1)    
  . IF $PIECE(ZN,"^",2)="Y" SET RESULT=ANY
  IF RESULT="",(GETANY=1) SET RESULT=ANY
  QUIT RESULT         
  ;
GETPMOD1(IEN22733,MOD)  ;"GET PREFERRED MODIFIER, IN SAME GROUP AS MOD
  ;"First find which grouping (form, strength etc) that MOD belongs to
  ;"  then see if any in that group is specified to be preferred.
  ;"NOTE: this assumes that a modifier will be unique for a form.  
  NEW FOUNDMODIEN SET FOUNDMODIEN=0
  NEW NEWMOD SET NEWMOD=MOD  ;"Default to original value.  
  NEW FORMIEN SET FORMIEN=0           
  FOR  SET FORMIEN=$ORDER(^TMG(22733,IEN22733,2,FORMIEN)) QUIT:(FORMIEN'>0)!(FOUNDMODIEN>0)  DO
  . NEW AMODIEN SET AMODIEN=0
  . FOR  SET AMODIEN=$ORDER(^TMG(22733,IEN22733,2,FORMIEN,2,AMODIEN)) QUIT:(AMODIEN'>0)!(FOUNDMODIEN>0)  DO
  . . NEW ZN SET ZN=$GET(^TMG(22733,IEN22733,2,FORMIEN,2,AMODIEN,0))
  . . IF $PIECE(ZN,"^",1)'=MOD QUIT
  . . SET FOUNDMODIEN=AMODIEN
  . IF FOUNDMODIEN=0 QUIT  
  . ;"Now that we are in a matching group, figure which is marked as preferred
  . SET NEWMOD=$$GETPMOD2(IEN22733,FORMIEN) ;"GET PREFERRED MOD
  QUIT NEWMOD
  ;  
GETPMOD2(IEN22733,FORMIEN,GETANY) ;"GET PREFERRED MOD, FOR GIVEN FORMIEN
  NEW ROOT SET ROOT=$NAME(^TMG(22733,IEN22733,2,FORMIEN,2))
  QUIT $$GETPREF(ROOT,.GETANY)
  ;
GTFRMIEN(IEN22733,FORM)  ;"GET FORM SUBIEN BASED ON INPUT FORM
  NEW RESULT SET RESULT=0
  NEW SUBIEN SET SUBIEN=0
  FOR  SET SUBIEN=$ORDER(^TMG(22733,IEN22733,2,SUBIEN)) QUIT:(SUBIEN'>0)!(RESULT>0)  DO
  . NEW IEN50D606 SET IEN50D606=$PIECE($GET(^TMG(22733,IEN22733,2,SUBIEN,0)),"^",1)
  . NEW AFORM SET AFORM=$PIECE($GET(^PS(50.606,IEN50D606,0)),"^",1)
  . IF AFORM=FORM SET RESULT=SUBIEN
  . IF $DATA(^TMG(22733,IEN22733,2,SUBIEN,.05,"B",FORM))>0 SET RESULT=SUBIEN  
GTFMINDN ;
  QUIT RESULT
  ;
MATCHFRM(IEN22733,OUT,FORM)  ;"GET PREFERRED ALIAS FORM, GIVEN INPUT FORM -- DELETE THIS
  ;"INPUT: IEN22733
  ;"       OUT -- PASS BY REFERENCE.  AN OUT PARAMETER.
  ;"          OUT("FORM","SUBIEN")=#
  ;"          OUT("FORM","DATABASE")=#
  ;"RESULTS: none
  NEW SUBIEN SET SUBIEN=+$$GTFRMIEN(IEN22733,FORM)  ;"GET FORM SUBIEN BASED ON INPUT FORM
  SET OUT("FORM","SUBIEN")=SUBIEN
  NEW IEN50D606 SET IEN50D606=$PIECE($GET(^TMG(22733,IEN22733,2,SUBIEN,0)),"^",1)
  NEW AFORM SET AFORM=$PIECE($GET(^PS(50.606,IEN50D606,0)),"^",1)
  SET OUT("FORM","DATABASE")=AFORM
  SET OUT("FORM","PREFERRED")=$$GETPRFAL(IEN22733,SUBIEN) ;"GET PREFERRED FORM ALIAS
  QUIT
  ;
FIXSPLNG(IEN22733,LINE)  ;"FIX SPELLING
  ;"Input: IEN22733 -- IEN of Rx
  ;"       LINE -- PASS BY REFERENCE.  Spelling problems are searched and replaced
  NEW IEN SET IEN=0
  FOR  SET IEN=$ORDER(^TMG(22733,IEN22733,.07,IEN)) QUIT:IEN'>0  DO
  . NEW ZN SET ZN=$GET(^TMG(22733,IEN22733,.07,IEN,0))
  . NEW BAD SET BAD=$PIECE(ZN,"^",1) QUIT:BAD=""
  . NEW GOOD SET GOOD=$PIECE(ZN,"^",2)
  . IF LINE'[BAD QUIT
  . SET LINE=$$REPLSTR^TMGSTUT3(LINE,BAD,GOOD)
  QUIT
  ;