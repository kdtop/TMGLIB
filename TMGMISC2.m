TMGMISC2 ;TMG/kst/sacc-COMPLIANT Misc utility library 1/6/17
         ;;1.0;TMG-LIB;**1**;7/23/12
 ;"
 ;"TMG MISCELLANEOUS FUNCTIONS
 ;"SACC-Compliant version
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"NOTE: This will contain SACC-compliant versions of code from TMGMISC
 ;"      If routine is not found here, the please migrate and update the
 ;"      code to be compliant.
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"LISTCT(PARRAY) -- Count the number of entries in an array
 ;"$$DTFORMAT(FMDATE,FORMAT) -- FORMAT fileman dates
 ;"PARSEC(SEC) --Parse seconds
 ;"S2STR(SEC) --Seconds to string 
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"DOTOKEN(FMDATE,TOKEN,OUTPUT,ARRAY)
 ;"=======================================================================
 ;"DEPENDENCIES: ^XLFDT
 ;"=======================================================================
 ;"
LISTCT(PARRAY) ;" SAAC complient entry point.
  ;"SCOPE: PUBLIC
  ;"Purpose: to count the number of entries in an array
  ;"Input: PARRAY -- PASS BY NAME.  pointer to (name of) array to test.
  ;"OUTPUT: the number of entries at highest level
  ;"      e.g.  ARRAY("TELEPHONE")=1234
  ;"            ARRAY("CAR")=4764
  ;"            ARRAY("DOG")=5213
  ;"            ARRAY("DOG","COLLAR")=5213  <-- not highest level,not counted.
  ;"        The above array would have a count of 3
  ;"Results: returns count, or count up to point of any error
  NEW IDX SET IDX=""
  NEW RESULT SET RESULT=0
  DO
  . NEW $ETRAP SET $ETRAP="WRITE ""?? Error Trapped ??"",! SET $ECODE="""" QUIT"
  . FOR  SET IDX=$ORDER(@PARRAY@(IDX)) QUIT:IDX=""  SET RESULT=RESULT+1
  QUIT RESULT
  ;
DTFORMAT(FMDATE,FORMAT,ARRAY) ;
  ;"SCOPE: PUBLIC
  ;"Purpose: to allow custom formating of fileman dates in to text equivalents
  ;"Note: Sometime, compare this function to $$DATE^TIULS ... 
  ;"Input:   FMDATE -- this is the date to work on, in Fileman Format
  ;"         FORMAT -- a formating string with codes as follows.
  ;"           yy -- 2 digit year
  ;"           yyyy --  4 digit year
  ;"           m - month number without a leading 0.
  ;"           mm -- 2 digit month number (01-12)
  ;"           mmm - abreviated months (Jan,Feb,Mar etc.)
  ;"           mmmm -- full names of months (January,February,March etc)
  ;"           d -- the number of the day of the month (1-31) without a leading 0
  ;"           dd -- 2 digit number of the day of the month
  ;"           w -- the numeric day of the week (0-6)
  ;"           ww -- abreviated day of week (Mon,Tue,Wed)
  ;"           www -- day of week (Monday,Tuesday,Wednesday)
  ;"           h -- the number of the hour without a leading 0 (1-23) 24-hr clock mode
  ;"           hh -- 2 digit number of the hour.  24-hr clock mode
  ;"           H -- the number of the hour without a leading 0 (1-12) 12-hr clock mode
  ;"           HH -- 2 digit number of the hour.  12-hr clock mode
  ;"           # -- will display 'am' for hours 1-12 and 'pm' for hours 13-24
  ;"           M - the number of minutes with out a leading 0
  ;"           MM -- a 2 digit display of minutes
  ;"           s - the number of seconds without a leading 0
  ;"           ss -- a 2 digit display of number of seconds.
  ;"           allowed punctuation symbols--   ' ' : , / @ .;- (space, colon, comma, forward slash, at symbol,semicolon,period,hyphen)
  ;"           'text' is included as is, even IF it is same as a formatting code
  ;"           Other unexpected text will be ignored
  ;"         
  ;"           If a date value of 0 is found for a code, that code is ignored (except for min/sec)
  ;"         
  ;"           Examples:  with FMDATE=3050215.183000  (i.e. Feb 5, 2005 @ 18:30  0 sec)
  ;"           "mmmm d,yyyy" --> "February 5,2005"
  ;"           "mm d,yyyy" --> "Feb 5,2005"
  ;"           "'Exactly' H:MM # 'on' mm/dd/yy" --> "Exactly 6:30 pm on 02/05/05"
  ;"           "mm/dd/yyyy" --> "02/05/2005"
  ;"
  ;"         ARRAY -- OPTIONAL, IF supplied, SHOULD BE PASSED BY REFERENCE
  ;"              The array will be filled with data as follows:
  ;"              ARRAY(TOKEN)=value for that token  (ignores codes such as '/',':' ect)
  ;"OUTPUT: Text of date, as specified by above
  NEW RESULT SET RESULT=""
  NEW TOKEN SET TOKEN=""
  NEW LASTTOKEN SET LASTTOKEN=""
  NEW CH SET CH=""
  NEW LASTCH SET LASTCH=""
  NEW INSTR SET INSTR=0
  NEW DONE SET DONE=0
  NEW IDX
  IF $GET(FORMAT)="" GOTO FDTDONE
  IF +$GET(FMDATE)=0 GOTO FDTDONE
  FOR IDX=1:1:$LENGTH(FORMAT) DO  QUIT:DONE
  . SET LASTCH=CH
  . SET CH=$EXTRACT(FORMAT,IDX)   ;"get next char of format string.
  . IF (CH'=LASTCH)&(LASTCH'="")&(INSTR=0) DO DOTOKEN(FMDATE,.TOKEN,.RESULT,.ARRAY)
  . SET TOKEN=TOKEN_CH
  . IF CH="'" DO  QUIT
  . . IF INSTR DO DOTOKEN(FMDATE,.TOKEN,.RESULT)
  . . SET INSTR='INSTR  ;"toggle In-String mode
  . IF (IDX=$LENGTH(FORMAT)) DO DOTOKEN(FMDATE,.TOKEN,.RESULT,.ARRAY)
FDTDONE ;
  QUIT RESULT
  ;
DOTOKEN(FMDATE,TOKEN,OUTPUT,ARRAY) ;
  ;"SCOPE: PRIVATE
  ;"Purpose: To take tokens and build output following rules specified by DTFormat
  ;"Input: FMDATE -- the date to work with
  ;"          TOKEN -- SHOULD BE PASSED BY REFERENCE.  The code as oulined in DTFormat
  ;"          OUTPUT -- SHOULD BE PASSED BY REFERENCE. The cumulative output
  ;"          ARRAY -- OPTIONAL, IF supplied, SHOULD BE PASSED BY REFERENCE
  ;"              The array will be filled with data as follows:
  ;"              ARRAY(TOKEN)=value for that token  (ignores codes such as '/')
  ;"Result: none
  IF $EXTRACT(TOKEN,1,1)="'" DO  GOTO PTDONE
  . NEW STR SET STR=$EXTRACT(TOKEN,2,$LENGTH(TOKEN)-1)
  . SET OUTPUT=OUTPUT_STR
  IF ($LENGTH(TOKEN)=1)&(" .:/;,-@"[TOKEN) SET OUTPUT=OUTPUT_TOKEN GOTO PTDONE
  ;
  IF "yyyy"[TOKEN DO  GOTO PTDONE
  . NEW YEAR SET YEAR=+$EXTRACT(FMDATE,1,3)
  . IF YEAR=0 QUIT
  . IF TOKEN="yy" DO
  . . SET YEAR=+$EXTRACT(FMDATE,2,3)
  . . IF YEAR<10 SET YEAR="0"_YEAR
  . ELSE  DO
  . . SET YEAR=YEAR+1700
  . SET OUTPUT=OUTPUT_YEAR
  . SET ARRAY(TOKEN)=YEAR
  ;
  IF "mmmm"[TOKEN DO  GOTO PTDONE
  . NEW MONTH SET MONTH=+$EXTRACT(FMDATE,4,5)
  . IF MONTH=0 QUIT
  . IF TOKEN="mm" DO
  . . IF MONTH<10 SET MONTH="0"_MONTH
  . ELSE  IF TOKEN="mmm" DO
  . . SET MONTH=$PIECE("Jan^Feb^Mar^Apr^May^Jun^Jul^Aug^Sept^Oct^Nov^Dec","^",MONTH)
  . ELSE  IF TOKEN="mmmm" DO
  . . SET MONTH=$PIECE("January^February^March^April^May^June^July^August^September^October^November^December","^",MONTH)
  . SET OUTPUT=OUTPUT_MONTH
  . SET ARRAY(TOKEN)=MONTH
  ;
  IF "dd"[TOKEN DO  GOTO PTDONE
  . NEW DAY SET DAY=+$EXTRACT(FMDATE,6,7)
  . IF TOKEN="dd" DO
  . . IF DAY<10 SET DAY="0"_DAY
  . SET OUTPUT=OUTPUT_DAY
  . SET ARRAY(TOKEN)=DAY
  ;
  IF "www"[TOKEN DO  GOTO PTDONE
  . NEW DOW SET DOW=$$DOW^XLFDT(FMDATE,1)
  . IF (DOW<0)!(DOW>6) QUIT
  . NEW DOWS SET DOWS=$PIECE("Sun^Mon^Tue^Wed^Thur^Fri^Sat","^",DOW+1)
  . IF TOKEN="ww" SET DOW=DOWS
  . IF TOKEN="www" SET DOW=DOWS_"day"
  . SET OUTPUT=OUTPUT_DOW
  . SET ARRAY(TOKEN)=DOW
  ;
  IF "hh"[TOKEN DO  GOTO PTDONE
  . NEW HOUR SET HOUR=+$EXTRACT(FMDATE,9,10)
  . IF HOUR=0 QUIT
  . IF TOKEN="hh",(HOUR<10) SET HOUR="0"_HOUR
  . SET OUTPUT=OUTPUT_HOUR
  . SET ARRAY(TOKEN)=HOUR
  ;
  IF "HH"[TOKEN DO  GOTO PTDONE
  . NEW HOUR SET HOUR=+$EXTRACT(FMDATE,9,10)
  . IF HOUR=0 QUIT
  . IF HOUR>12 SET HOUR=HOUR-12
  . IF TOKEN="HH",(HOUR<10) SET HOUR="0"_HOUR
  . SET OUTPUT=OUTPUT_HOUR
  . SET ARRAY(TOKEN)=HOUR
  ;
  IF TOKEN="#" DO  GOTO PTDONE
  . NEW HOUR SET HOUR=+$EXTRACT(FMDATE,9,10)
  . IF HOUR=0 QUIT
  . NEW CODE SET CODE=$SELECT(HOUR>12:"pm",1:"am")
  . SET OUTPUT=OUTPUT_CODE
  . SET ARRAY(TOKEN)=CODE
  ;
  IF "MM"[TOKEN DO  GOTO PTDONE
  . NEW MIN SET MIN=+$EXTRACT(FMDATE,11,12)
  . IF TOKEN="MM",(MIN<10) SET MIN="0"_MIN
  . SET OUTPUT=OUTPUT_MIN
  . SET ARRAY(TOKEN)=MIN
  ;
  IF "ss"[TOKEN DO  GOTO PTDONE
  . NEW SEC SET SEC=+$EXTRACT(FMDATE,13,14)
  . IF TOKEN="ss",(SEC<10) SET SEC="0"_SEC
  . SET OUTPUT=OUTPUT_SEC
  . SET ARRAY(TOKEN)=SEC
  ;
PTDONE  ;
  SET TOKEN=""
  QUIT
  ;
TMPSAVE(ARR,LABEL,SAVET)  ;"TEMP STORE ARR IN ^XTMP, KILLING PRIOR
  ;"Input: ARR -- PASS BY REFERENCE.  ARRAY TO STORE
  ;"       LABEL -- Unique name for storage
  ;"       SAVET -- <num>S  or <num>M or <num>H  or <num>D
  ;"              This is the duration of the save time, in 
  ;"               *S*econds, *M*inutes, *H*ours, *D*ays
  ;"Result: NONE
  DO TMPSAVRF("ARR",.LABEL,.SAVET)
  QUIT 
  ;
TMPSAVRF(REF,LABEL,SAVET)  ;"TEMP STORE ARR IN ^XTMP, KILLING PRIOR
  ;"Input: REF -- PASS BY NAME.  ARRAY TO STORE
  ;"       LABEL -- Unique name for storage
  ;"       SAVET -- <num>S  or <num>M or <num>H  or <num>D
  ;"              This is the duration of the save time, in 
  ;"               *S*econds, *M*inutes, *H*ours, *D*ays
  ;"Result: NONE
  NEW XTMPREF SET XTMPREF=$$GETMPREF(LABEL)          
  IF $DATA(@XTMPREF) KILL @XTMPREF
  NEW NOW SET NOW=$$NOW^XLFDT
  SET SAVET=$$UP^XLFSTR($GET(SAVET)) IF SAVET="" SET SAVET="1D"
  NEW SEC,MIN,HR,DAY SET (SEC,MIN,HR,DAY)=0
  IF SAVET["S" SET SEC=+SAVET
  IF SAVET["M" SET MIN=+SAVET
  IF SAVET["H" SET HR=+SAVET
  IF SAVET["D" SET DAY=+SAVET
  NEW KILLT SET KILLT=$$FMADD^XLFDT(NOW,DAY,HR,MIN,SEC)
  SET @XTMPREF@(0)=KILLT_"^"_NOW
  MERGE @XTMPREF@("D")=@REF
  QUIT
  ;
GETMPREF(LABEL)  ;"GET REF IN ^XTMP FOR LABEL         
  SET LABEL=$GET(LABEL,"??-"+$H)
  NEW XTMPREF SET XTMPREF=$NAME(^XTMP("TMG-"_LABEL))
  QUIT XTMPREF
  ; 
PARSEC(SEC) ;"Parse seconds
  NEW SPY,SPD,SPH,YR,DAY,HR,MIN,SEC2,RM,STR
  SET SPH=3600,SPD=SPH*24,SPY=SPD*365
  SET YR=SEC\SPY,RM=SEC#SPY,DAY=RM\SPD,RM=RM#SPD,HR=RM\SPH
  SET RM=RM#SPH,MIN=RM\60,RM=RM#60,SEC2=RM
  SET STR=YR_"^"_DAY_"^"_HR_"^"_MIN_"^"_SEC2
  QUIT STR
  ;  
S2STR(SEC) ;"Seconds to string
  NEW STR,PS,IDX,TAG,NM
  SET STR="",PS=$$PARSEC(SEC),IDX=1,TAG="yrs^days^hrs^mins^secs" 
  FOR IDX=1:1:5 SET NM=$P(PS,"^",IDX) SET:NM>0 STR=STR_$S(STR]"":", ",1:"")_NM_" "_$P(TAG,"^",IDX)
  QUIT STR
 ;  
INITPFIL(GROUP)  ;"Initialize timing profile, by deleting old data
  NEW ROOT SET ROOT=$NAME(^TMG("TMP","TIMER^TMGMISC2",GROUP))
  KILL @ROOT
  QUIT
  ;
TIMEPFIL(GROUP,ITEM,TOGGLE) ;"Turn a timer on or off. 
  ;"Input: GROUP -- A user-determined name to describe a grouping for items
  ;"       ITEM -- A user-determined name to describe item
  ;"       TOGGLE -- should be 1 to turn on, or 0 to turn off
  ;"Discussion: When toggled on, this will record start time.  When turned off,
  ;"            this will store elapsed time.  Storage is in ^TMG("TMP","TIMER",<GROUP>,<ITEM>)
  ;"Result: none
  SET GROUP=$GET(GROUP,"??GROUP")
  SET ITEM=$GET(ITEM,"??ITEM")
  NEW ROOT SET ROOT=$NAME(^TMG("TMP","TIMER^TMGMISC2",GROUP))
  SET TOGGLE=+$GET(TOGGLE)
  IF TOGGLE DO
  . SET @ROOT@(ITEM)="START^"_$$TIME2^TMGKERNL
  ELSE  DO
  . NEW PRIOR SET PRIOR=$GET(@ROOT@(ITEM))
  . NEW STARTTIME SET STARTTIME=+$PIECE(PRIOR,"^",2)
  . NEW CURRENT SET CURRENT=$$TIME2^TMGKERNL
  . NEW DELTA SET DELTA=CURRENT-STARTTIME
  . IF STARTTIME=0 DO
  . . SET @ROOT@(ITEM)="END^"_$$TIME2^TMGKERNL
  . ELSE  DO
  . . SET @ROOT@(ITEM)="ELAPSED^"_DELTA
  . . SET @ROOT@("TIME_INDEX",DELTA,ITEM)=""
  QUIT
  ;
SHOWRPT(GROUP)  ;"show report of timer
  NEW OUT DO TIMERRPT(.OUT,GROUP)
  IF $DATA(OUT) ZWR OUT
  QUIT
  ;
TIMERPT(OUT,GROUP)  ;"Get report of timer
  SET GROUP=$GET(GROUP,"??GROUP")
  SET ITEM=$GET(ITEM,"??ITEM")
  NEW ROOT SET ROOT=$NAME(^TMG("TMP","TIMER^TMGMISC2",GROUP))
  MERGE OUT(GROUP)=^TMG("TMP","TIMER^TMGMISC2",GROUP)
  QUIT
  ;