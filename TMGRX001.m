TMGRX001 ;TMG/kst/Patient medication code; 08/23/17
       ;;1.0;TMG-LIB;**1**;08/23/17
 ;
 ;
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
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"=======================================================================
 ;"Uses:  
 ;"=======================================================================
 ;        
T1 ;
  NEW TIUIEN SET TIUIEN="@"
  FOR  SET TIUIEN=$ORDER(^TIU(8925,TIUIEN),-1) QUIT:TIUIEN'>0  DO
  . NEW DFN SET DFN=$PIECE($GET(^TIU(8925,TIUIEN,0)),"^",2) QUIT:DFN'>0
  . NEW ARR,TEMP
  . DO MEDLIST^TMGTIUOJ(.TEMP,DFN,.ARR)
  . NEW ARR2 DO PARSEARR(.ARR2,.ARR)
  . ZWR ARR2
  QUIT  
  ;
T2 ;
  NEW X,Y,DIC SET DIC=2,DIC(0)="MAEQ"
  DO ^DIC WRITE !
  IF +Y'>0 QUIT
  NEW DFN SET DFN=+Y
  NEW ARR,TEMP
  DO MEDLIST^TMGTIUOJ(.TEMP,DFN,.ARR)
  NEW ARR2 DO PARSEARR(.ARR2,.ARR)
  ZWR ARR2
  QUIT
  ;
PARSEARR(OUT,ARR)  ;"PARSE A MED LIST  
  NEW IDX SET IDX=0 
  FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:IDX'>0  DO
  . NEW LINE SET LINE=$GET(ARR(IDX)) 
  . IF $EXTRACT(LINE,1)="*" QUIT
  . IF LINE["[MEDICATION" QUIT
  . NEW TEMP
  . SET ARR(IDX)=$$PARSELN(.TEMP,LINE) 
  QUIT
  ;
PARSELN(OUT,LINE) ;"PARSE ONE MED LINE
  ;"Input: OUT -- PASS BY REFERENCE.  AN OUT PARAMETER.  Format:
  ;"         OUT("NOTE")=<any notes>
  ;"         OUT("MED NAME")=<NAME OF RX>
  ;"         OUT("GENERIC")=<NAME OF RX>
  ;"         OUT("BRAND")=<NAME OF RX>
  ;"         OUT("OTC")=1  <-- if specified as OTC
  ;"         OUT("DOSE")=#
  ;"         OUT("UNITS")=E.G. "MG"
  ;"         OUT("IEN50.607")=#  
  ;"         OUT("PREFACE")= e.g. HOLD, HOLDING, OFF
  ;"         OUT("IEN22733")=<IEN 22733>
  ;"       LINE -- a string of text representing one medication line from med table. 
  SET LINE=$$UP^XLFSTR(LINE)
  IF LINE[">--" SET LINE=$$REPLSTR^TMGSTUT3(LINE,">--","<--")  
  IF LINE["<--" DO
  . SET OUT("NOTE")=$$TRIM^XLFSTR($PIECE(LINE,"<--",2,99))
  . SET LINE=$PIECE(LINE,"<--",1)
  IF LINE["OTC " SET LINE=$$REPLSTR^TMGSTUT3(LINE,"OTC ","") SET OUT("OTC")=1
  NEW FOUND,ABORT SET (FOUND,ABORT)=0
  SET FOUND("MED NAME")=0
  SET FOUND("DOSE")=0
  NEW IEN22733 SET IEN22733=0  
  FOR  QUIT:(LINE="")!ABORT  DO
  . NEW WORD DO GETNXWRD(.LINE,.WORD)  ;"GET NEXT WORD 
  . SET OUT("SIG")=LINE
  . IF WORD="" QUIT
  . IF (WORD'=""),FOUND("MED NAME")=0 DO  
  . . IF $$ISPREFIX(.OUT,.WORD) QUIT  ;"HANDLE PREFIXES
  . . SET IEN22733=$$MEDIEN(.OUT,WORD,.LINE) QUIT:IEN22733'>0
  . . SET OUT("IEN22733")=IEN22733
  . . SET OUT(OUT("TYPE"))=WORD KILL OUT("TYPE")
  . . SET OUT("MED NAME")=WORD,WORD=""
  . . SET FOUND("MED NAME")=1
  . . DO CHK2NDNM(.OUT,.LINE)  ;"CHECK FOR 2ND MED NAME: MedName (MedName2)  or  MedName / MedName2  pattern
  . IF (WORD'=""),FOUND("DOSE")=0 DO    
  . . IF $GET(WORD("NUM"))'="" DO
  . . . NEW NEXTWORD DO GETNXWRD(.LINE,.NEXTWORD)  ;"GET NEXT WORD
  . . . IF +$GET(NEXTWORD("UNIT IEN"))>0 DO
  . . . . SET OUT("DOSE")=WORD("NUM"),WORD=""
  . . . . SET OUT("UNITS")=NEXTWORD("S2")
  . . . . SET OUT("IEN50.607")=NEXTWORD("UNIT IEN")
  . . . . SET FOUND("DOSE")=1    ;"Must have numeric dose AND units to be considerred found
  . . . ELSE  DO PUTBKWRD(.LINE,NEXTWORD)  ;"PUT BACK WORD  
  . IF (IEN22733>0)&(FOUND("DOSE")>0) DO  ;"TRY TO MATCH DOSES TO DATABASE.
  . . DO MATCHDSE(.OUT,IEN22733)  ;"TRY TO MATCH DOSES TO DATABASE.  
  . IF WORD'="" DO    ;"If word not used above, then done with parsing  
  . . DO PUTBKWRD(.LINE,WORD)
  . . SET ABORT=1 
  . SET OUT("SIG")=LINE
  QUIT
  ;
CHK2NDNM(OUT,LINE)  ;"CHECK FOR 2ND MED NAME: MedName (MedName2)  or  MedName / MedName2  pattern
  NEW MED2NAME SET MED2NAME=""
  NEW NEXTWORD DO GETNXWRD(.LINE,.NEXTWORD)   
  NEW RESTORENAME SET RESTORENAME=NEXTWORD
  IF NEXTWORD="/" DO
  . DO GETNXWRD(.LINE,.MED2NAME)
  . SET RESTORENAME="/ "_MED2NAME
  ELSE  IF ($EXTRACT(NEXTWORD,1)="(")&($EXTRACT(NEXTWORD,$LENGTH(NEXTWORD))=")") DO
  . SET MED2NAME=$EXTRACT(NEXTWORD,2,$LENGTH(NEXTWORD)-1)
  IF MED2NAME'="" DO
  . NEW TEMPOUT,IEN,LINE2 
  . SET LINE2=MED2NAME,IEN=$$MEDIEN(.TEMPOUT,.MED2NAME,.LINE2)
  . IF IEN'>0 DO  QUIT
  . . DO PUTBKWRD(.LINE,RESTORENAME)  ;"PUT BACK WORD
  . SET OUT(TEMPOUT("TYPE"))=MED2NAME KILL OUT("TYPE")
  ELSE  DO PUTBKWRD(.LINE,RESTORENAME)  ;"PUT BACK WORD
  QUIT
  ;
GETNXWRD(LINE,WORD)  ;"GET NEXT WORD
  NEW TEMP,LEN,ST,IDX
  SET WORD=$PIECE(LINE," ",1),LINE=$PIECE(LINE," ",2,999) 
  IF (WORD["/"),(WORD'="/") DO  ;"Split LISINOPRIL/HCTZ into LISINOPRIL / HCTZ 
  . SET TEMP=""
  . SET LEN=$LENGTH(WORD,"/") IF LEN>2 QUIT
  . FOR IDX=1:1:2 SET ST(IDX)=$$TRIM^XLFSTR($PIECE(WORD,"/",IDX))
  . IF (+ST(1)=ST(1))!(+ST(2)=ST(2)) QUIT ;"ignore 1/2 or 2/day pattern
  . IF ST(1)'="" SET WORD=ST(1),LINE="/ "_ST(2)_" "_LINE 
  . ELSE  SET WORD="/",LINE=ST(2)_" "_LINE
  DO WORDINFO(.WORD)  ;"characterize word
  IF ($GET(WORD("NUM"))'=""),($GET(WORD("S2"))'="") DO
  . SET LINE=WORD("S2")_" "_LINE    ;"if we got 81mg, keep 81 and put mg back onto LINE
  . SET WORD("S2")=""  
  QUIT
  ;
PUTBKWRD(LINE,WORD)  ;"PUT BACK WORD
  SET LINE=WORD_" "_LINE
  QUIT
  ;
MEDIEN(OUT,NAME,LINE)  ;"
  NEW IEN22733 SET IEN22733=0
  SET OUT("TYPE")="?"
  NEW FMIDX FOR FMIDX="B","B2","BRAND" QUIT:IEN22733>0  DO
  . SET IEN22733=$ORDER(^TMG(22733,FMIDX,NAME,0))
  . IF IEN22733'>0 QUIT
  . IF (FMIDX="B")!(FMIDX="B2") DO  QUIT
  . . SET OUT("TYPE")="GENERIC"
  . . IF FMIDX="B2" SET NAME=$PIECE($GET(^TMG(22733,IEN22733,0)),"^",1) ;"change alias generic to proper generic name.  
  . IF FMIDX="BRAND" DO  QUIT
  . . SET OUT("TYPE")="BRAND"
  . . ;"Later, I could change brand alias (possibly mispelled), to preferred brand name  
  IF IEN22733'>0 DO  ;"LOOK FOR PARTIAL MATCHES, E.G. VITAMIN D3
  . NEW DONE SET DONE=0
  . FOR FMIDX="B","B2","BRAND" QUIT:IEN22733>0  DO
  . . NEW TEMP SET TEMP=NAME
  . . FOR  SET TEMP=$ORDER(^TMG(22733,FMIDX,TEMP)) QUIT:(NAME="")!(IEN22733>0)!DONE  DO
  . . . IF $EXTRACT(TEMP,1,$LENGTH(NAME))'=NAME SET DONE=1 QUIT
  . . . IF LINE'[TEMP QUIT
  . . . NEW TEMPWORD SET TEMPWORD=NAME
  . . . NEW IDX FOR IDX=1:1:$LENGTH(LINE," ") DO
  . . . . SET TEMPWORD=TEMPWORD_" "_$PIECE(LINE," ",IDX)
  . . . . IF TEMP'[TEMPWORD SET IDX=999 QUIT
  . . . . IF TEMPWORD=TEMP DO  QUIT
  . . . . . SET IEN22733=$ORDER(^TMG(22733,FMIDX,TEMP,0))
  . . . . . SET IDX=999,NAME=TEMPWORD
  . . . . IF IEN22733>0 DO
  . . . . . IF (FMIDX="B")!(FMIDX="B2") SET OUT("GENERIC")=NAME
  . . . . . ELSE  IF FMIDX="BRAND" SET OUT("BRAND")=NAME
  QUIT IEN22733
  ;
WORDINFO(WORD)  ;"characterize word
  ;"Input: WORD, PASS BY REFERENCE, AND IN AND OUT PARAMETER.  FORMAT
  ;"           WORD = <word to test>
  ;"           WORD("NUM")=# if word starts with number
  NEW SAVE SET SAVE=WORD
  NEW DONE SET DONE=0
  NEW SPEC,EXTRA FOR EXTRA=",",";" IF WORD[EXTRA SET SPEC(EXTRA)=""
  IF $DATA(SPEC) SET WORD=$$REPLACE^XLFSTR(WORD,.SPEC)
  NEW NUM SET NUM=""
  IF WORD?0.1(1"+",1"-").NP1"/".NP DO   ;"Fractions, e.g. 1/2  or -0.4/.4
  . SET NUM=WORD,WORD=""
  ELSE  FOR  QUIT:DONE!(WORD="")  DO
  . NEW CH SET CH=$EXTRACT(WORD,1)
  . IF "-.1234567890"'[CH SET DONE=1 QUIT
  . SET WORD=$EXTRACT(WORD,2,999)
  . SET NUM=NUM_CH
  . IF NUM="0" SET NUM="0 "
  SET WORD("NUM")=NUM
  SET WORD("S2")=WORD
  SET WORD("UNIT IEN")=$SELECT(WORD="":0,1:$ORDER(^PS(50.607,"B",WORD,0))) 
  SET WORD=SAVE
  QUIT
  ;
MATCHDSE(OUT,IEN2273)  ;"TRY TO MATCH DOSES TO DATABASE.  
  NEW FORMIEN SET FORMIEN=0
  FOR  SET FORMIEN=$ORDER(^TMG(22733,IEN22733,2,FORMIEN)) QUIT:FORMIEN'>0  DO
  . NEW IEN50D606 SET IEN50D606=$PIECE($GET(^TMG(22733,IEN22733,2,FORMIEN,0)),"^",1) 
  . NEW FORM SET FORM=$$GET1^DIQ(50.606,IEN50D606,.01)
  . NEW DOSEIEN SET DOSEIEN=0
  . FOR  SET DOSEIEN=$ORDER(^TMG(22733,IEN22733,2,FORMIEN,1,DOSEIEN)) QUIT:DOSEIEN'>0  DO
  . . NEW ADOSE SET ADOSE=$PIECE($GET(^TMG(22733,IEN22733,2,FORMIEN,1,DOSEIEN,0)),"^",1)
  . . IF ADOSE'=OUT("DOSE") QUIT
  . . SET OUT("DOSE IENS",DOSEIEN_","_FORMIEN_","_IEN22733_",")=FORM
  QUIT
  ;
ISPREFIX(OUT,WORD)  ;"HANDLE PREFIXES
  NEW RESULT SET RESULT=0
  IF ",ADD,ADDED,START,STOP,STOPPED,OFF,HOLD,HOLDING,OTC"[WORD DO  
  . SET RESULT=1
  . IF WORD="OTC" SET OUT("OTC")=1
  . ELSE  SET OUT("PREFACE")=WORD
  . SET WORD=""
  QUIT RESULT
  ;
  
