TMGCPTU1 ;TMG/kst-HL7 Handling CPTs; 4/26/19
              ;;1.0;TMG-LIB;**1**; 4/26/19
  ;
  ;"Handling CPT 
  ;
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;"Copyright (c) 4/26/19  Kevin S. Toppenberg MD
  ;"
  ;"This file is part of the TMG LIBRARY, and may only be used in accordence
  ;" to license terms outlined in separate file TMGLICNS.m, which should
  ;" always be distributed with this file.
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;
  ;"=======================================================================
  ;"=======================================================================
  ;" API -- Public Functions.
  ;"=======================================================================
  ;"ADDCPT -- Interactive method to add a new CPT
  ;"
  ;"=======================================================================
  ;" API - Private Functions
  ;"=======================================================================
  ;"=======================================================================
  ;"Dependancies
  ;"=======================================================================
  ;"
  ;"=======================================================================
  ;"==============================================================
  ;
ADDCPT  ;"Interactive method to add a new CPT
  NEW CPT,CPTNAME,CPTDESC
  NEW TMGFDA,TMGMSG,TMGIEN
GETCPT ;
  READ !,"ENTER CPT TO ADD OR EDIT (^ to abort): ",CPT WRITE !
  IF CPT["^" QUIT
  IF $LENGTH(CPT)'=5 WRITE "CPT code should be exactly 5 characters",! GOTO GETCPT
  SET CPT=$$UP^XLFSTR(CPT)
  SET TMGIEN=+$ORDER(^ICPT("B",CPT,0))
  IF (+CPT=CPT),(TMGIEN=0),($DATA(^ICPT(+CPT))>0) DO  GOTO ACDN
  . WRITE "ERROR: Record found at IEN=",+CPT,", but not in index.  Aborting",!
  IF TMGIEN>0 DO  GOTO ACEDIT
  . WRITE "THIS ENTRY ALREADY EXISTS. OPENING FOR EDIT NOW.",!
  ;  
  IF +CPT'=CPT DO
  . ;"NOTE: If putting in custom CPT codes at an arbitrary IEN, then these could be 
  . ;"      potentially be clobbered with future CPT patch from VA.  So will put
  . ;"      these as very large IEN numbers: 227,000,000 - 228,000,000
  . SET TMGIEN=+$ORDER(^ICPT(228000000),-1)
  . IF TMGIEN<227000000 SET TMGIEN=227000000
  . ELSE  SET TMGIEN=TMGIEN+1
  ELSE  DO
  . SET TMGIEN=+CPT
  ;
  READ "ENTER CPT SHORT NAME: ",CPTDESC WRITE !
  IF (CPTDESC="")!(CPTDESC["^") WRITE !,"Restarting...",! GOTO GETCPT
  SET TMGIEN(1)=TMGIEN
  SET TMGFDA(81,"+1,",.01)=CPT
  SET TMGFDA(81,"+1,",2)=CPTDESC
  DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG) DO  GOTO ACDN
  . DO SHOWDIER^TMGDEBU2(.TMGMSG)
  WRITE !,!,"STUB ENTRY CREATED, BUT ADDITIONAL FIELDS STILL NEEDED...",!
  ;
ACEDIT
  NEW FIELDS SET FIELDS=".01;2;3;6;8;50;60;61;62"
  NEW NUMFLDS SET NUMFLDS=$LENGTH(FIELDS,";")
  NEW DA,DIE,DR SET DA=TMGIEN,DIE=81,DR=".01;2;3;6;8;50;60;61;62"
  NEW IDX,TMGOUT
  FOR IDX=1:1:NUMFLDS DO
  . NEW AFLD SET AFLD=$PIECE(FIELDS,";",IDX) QUIT:AFLD=""
  . DO FIELD^DID(81,AFLD,"","MULTIPLE-VALUED;LABEL","TMGOUT","TMGMSG")
  . NEW LABEL SET LABEL=$GET(TMGOUT("LABEL"))
  . IF $GET(TMGOUT("MULTIPLE-VALUED"))=1 DO
  . . WRITE !,"EDIT ",LABEL,": <MULTIPLE-VALUED>"
  . SET DR=AFLD
  . DO ^DIE 
ACDN
  WRITE !,"Goodbye",!
  QUIT
  ;"
CPTACTIVE(IEN81) ;"Is CPT code (passed as IEN) active currently?
  ;"Alternative option, figure out how to use STATCHK^ICPTAPIU
  NEW RESULT
  NEW ZN SET ZN=$GET(^ICPT(+IEN81,0))
  SET RESULT=($PIECE(ZN,"^",4)'=1)
  QUIT RESULT
  ;
CPTACTV2(CPTCODE) ;"Is CPT code (pass by actual CPT code) active currently?
  NEW IEN81 SET IEN81=+$ORDER(^ICPT("B",CPTCODE,""))
  IF IEN81'>0 QUIT 0
  QUIT $$CPTACTIVE(IEN81)
  ;
TESTSRCHCPT ;
  NEW TERMS
TSC1 ;  
  WRITE !,"Enter SEARCH TERMS: " READ TERMS WRITE !
  IF (TERMS="")!(TERMS["^") QUIT
  NEW ARR DO SEARCHCPT(.ARR,TERMS)
  NEW IEN SET IEN=0
  FOR  SET IEN=$ORDER(ARR(IEN)) QUIT:IEN'>0  DO
  . NEW ZN SET ZN=$GET(^ICPT(IEN,0))
  . WRITE $PIECE(ZN,"^",1)," - ",$PIECE(ZN,"^",2),!
  GOTO TSC1  
  QUIT
  ;    
TESTSRCHCPT2 ;
  NEW TERMS SET TERMS="SKIN EXCI"
  NEW ARR DO SEARCHCPT(.ARR,TERMS)
  NEW IEN SET IEN=0
  FOR  SET IEN=$ORDER(ARR(IEN)) QUIT:IEN'>0  DO
  . NEW ZN SET ZN=$GET(^ICPT(IEN,0))
  . WRITE $PIECE(ZN,"^",1)," - ",$PIECE(ZN,"^",2),!
  QUIT
  ;    
SEARCHCPT(OUT,PHRASE,OPTION)  ;"Search CPT index based on PHRASE
  ;"INPUT:  OUT -- PASS BY REFERENCE, AN OUT PARAMETER.  Format:
  ;"           OUT(IEN)=""
  ;"        PHRASE -- String of 1 or more parts to look up.  Words may be PARTIAL words
  ;"        OPTION -- OPTIONAL.  PASS BY REFERENCE.  
  ;"          OPTION("FLD") -- Field to search in.  Should be .01 (CPT CODE), or 2 (for SHORT NAME) or 50 (DESCRIPTION) or "*".  Default is "*"
  ;"          OPTION("INCL INACTIVE")=1 if found, results will be return that are INACTIVE.  
  ;"          OPTION("AS OF DT")=<FMDT>  Return if FMDT is >=ACTIVE DATE AND <INACTIVEDT
  SET PHRASE=$GET(PHRASE)
  NEW WORDS,SKIPWORDS
  NEW IDX FOR IDX=1:1:$LENGTH(PHRASE," ") DO
  . NEW AWORD SET AWORD=$PIECE(PHRASE," ",IDX) 
  . SET AWORD=$$UP^XLFSTR(AWORD)
  . IF $LENGTH(AWORD)<3 SET SKIPWORDS(AWORD)="" QUIT
  . IF ($LENGTH(AWORD)<3)!(",THEN,FROM,OTHER,THAN,WITH,THEIR,SOME,THIS,"[(","_AWORD_",")) DO  QUIT
  . . SET SKIPWORDS(AWORD)="" QUIT   ;"not included in KWIC index.  
  . SET WORDS(AWORD)=""
  NEW FLD SET FLD=$GET(OPTION("FLD"),"*") IF FLD="*" SET FLD=".01,2,50"
  NEW ALL,AFLD,IDX FOR IDX=1:1:$LENGTH(FLD,",") SET AFLD=$PIECE(FLD,",",IDX) IF FLD>0 DO
  . ;"NOTE: for field 50, I could use xref "C", which is ~same as TMGKDESC, except it has number entries trimmed out
  . NEW XREF SET XREF=$SELECT(AFLD=.01:"B",AFLD=2:"TMGKSN",AFLD=50:"TMGKDESC",1:"") QUIT:XREF=""
  . NEW TEMP DO SRCHXREF(.TEMP,.WORDS,XREF)
  . MERGE ALL(AFLD)=TEMP
  DO ORSUBLIST(.ALL)  ;"OR sublists. Example, DESCRIPTION (50) might contain 'FACE', but not found in NAME.  I want EITHER
  ;"NOTE: if needed, could filter further by checking for inclusion of SKIPWORDS.
  ;"FILTER FOR INACTIVE
  NEW ASOF SET ASOF=$GET(OPTION("AS OF DT"))
  NEW INCINACTIVE SET INCINACTIVE=($GET(OPTION("INCL INACTIVE"))=1)
  NEW IEN SET IEN=0
  FOR  SET IEN=$ORDER(ALL(IEN)) QUIT:IEN'>0  DO
  . NEW ZN SET ZN=$GET(ICPT(IEN,0)) QUIT ZN=""
  . IF ASOF>0  DO  QUIT
  . . NEW ACTIVEDT SET ACTIVEDT=$PIECE(ZN,"^",8)
  . . NEW INACTIVEDT SET INACTIVEDT=$PIECE(ZN,"^",8)
  . . IF (ASOF<ACTIVEDT)!(ASOF>=INACTIVEDT) KILL ALL(IEN)  ;"ASOF not in[ACTIVE .. INACTIVE] range, so kill.  
  . IF INCINACTIVE=1 QUIT  
  . NEW INACTIVE SET INACTIVE=$PIECE(ZN,"^",4) QUIT:INACTIVE=0
  . KILL ALL(IEN)  ;"entry is inactive, so kill    
  KILL OUT MERGE OUT=ALL    
  QUIT
  ;
SRCHXREF(OUT,WORDS,XREF)  ;
  NEW AWORD SET AWORD=""
  FOR  SET AWORD=$ORDER(WORDS(AWORD)) QUIT:AWORD=""  DO
  . NEW ANENTRY SET ANENTRY=$$SUBASCII^TMGSTUT3(AWORD)
  . NEW DONE SET DONE=0
  . FOR  SET ANENTRY=$ORDER(^ICPT(XREF,ANENTRY)) QUIT:DONE  DO
  . . IF $EXTRACT(ANENTRY,1,$LENGTH(AWORD))=AWORD DO  QUIT
  . . . IF XREF="TMGKDESC" DO
  . . . . NEW IEN SET IEN=0 FOR  SET IEN=$ORDER(^ICPT(XREF,ANENTRY,IEN)) QUIT:IEN'>0  DO
  . . . . . SET OUT(AWORD,IEN)=""  ;"drop subien
  . . . ELSE  DO
  . . . . MERGE OUT(AWORD)=^ICPT(XREF,ANENTRY)   ;"<--- LIST OF IEN'S
  . . IF (ANENTRY="")!(ANENTRY]AWORD)  SET DONE=1 QUIT
  DO ANDSUBLIST(.OUT) ;"AND the various sublists into matching for ALL WORDS
  QUIT  
  ;
ANDSUBLIST(ARR)  ;"AND sublists
  ;"AND the various sub array lists into just those found in ALL sublists
  ;"INPUT -- ARR  format:  ARR(<WORD>,IEN)=""
  NEW RESULT
  NEW CURARR,WORDA SET WORDA=""
  FOR  SET WORDA=$ORDER(ARR(WORDA)) QUIT:WORDA=""  MERGE CURARR=ARR(WORDA)    
  SET WORDA=""
  FOR  SET WORDA=$ORDER(ARR(WORDA)) QUIT:WORDA=""  DO
  . DO LISTAND^TMGMISC("CURARR",$NAME(ARR(WORDA)),"CURARR")
  KILL ARR MERGE ARR=CURARR
  QUIT  
  ;
ORSUBLIST(ARR)  ;"OR sublists
  ;"AND the various sub array lists into just those found in ALL sublists
  ;"INPUT -- ARR  format:  ARR(<WORD>,IEN)=""
  NEW RESULT
  NEW CURARR,WORDA SET WORDA=""
  FOR  SET WORDA=$ORDER(ARR(WORDA)) QUIT:WORDA=""  MERGE CURARR=ARR(WORDA)    
  KILL ARR MERGE ARR=CURARR
  QUIT  
  ;

