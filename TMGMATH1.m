TMGMATH1 ;TMG/kst/Misc math utility library ;6/15/23
         ;;1.0;TMG-LIB;**1**;07/12/05
 ;         
 ;"TMG MISCELLANEOUS MATH FUNCTIONS
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/15/2023  Kevin S. Toppenberg MD
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
GCD(A,B)  ;"Calculate greated common denominator (or highest common factors) 
  IF A=0 QUIT B
  QUIT $$GCD(B#A,A)
  ;
LCM(A,B)  ;"calculate least common multiple of two numbers  
  NEW RESULT
  SET RESULT=(A*B)/$$GCD(A,B)
  QUIT RESULT
  ;
PARSEFRACT(FRACT,OUT)  ;"Parse fraction
  ;"INPUT: FRACT -- format:  NUMERATOR/DENOMINATOR.  e.g. "12/32" --> "3/8", or "4a/16b" --> "1a/4b"
  ;"OUTPUT: OUT  -- Format
  ;"            OUT="3/8" <-- example, fraction in lowest common form
  ;"            OUT("DECIMAL")=.375 <-- example, decimal form of 3/8
  ;"            OUT("ORIG")=<orignal fraction representation>
  ;"            OUT("NUM")=<NUMERATOR> reduced to lowest form. Numeric only (excludes any variable name)
  ;"            OUT("DEN")=<DENOMINATOR> reduced to lowest form. Numeric only (excludes any variable name)
  ;"            OUT("VARS","NUM")="" or <Any alpha chars from numerator>
  ;"            OUT("VARS","DEN")="" or <Any alpha chars from denominator>
  NEW NUM,DEN,D,OUTNUM,OUTDEN,VARNUM,VARDEN 
  KILL OUT
  SET OUT("ORIG")=FRACT
  SET NUM=$PIECE(FRACT,"/",1)
  SET DEN=$PIECE(FRACT,"/",2)
  IF DEN="" SET DEN=1
  IF +DEN=0 SET OUT="NaN" QUIT
  SET NUM=+$$NUMSTR^TMGSTUT3(NUM,.VARNUM)
  SET DEN=+$$NUMSTR^TMGSTUT3(DEN,.VARDEN)
  SET D=$$GCD(NUM,DEN)
  SET OUT("VARS","NUM")=$GET(VARNUM)
  SET OUT("VARS","DEN")=$GET(VARDEN)
  SET OUTNUM=NUM\D  
  SET OUTDEN=DEN\D
  DO CHECKSIGN(.OUTNUM,.OUTDEN)
  SET OUT("NUM")=OUTNUM
  SET OUT("DEN")=OUTDEN
  IF (OUTDEN_VARDEN)="1" DO
  . SET OUT=OUTNUM_VARNUM
  ELSE  DO
  . SET OUT=OUTNUM_VARNUM_"/"_OUTDEN_VARDEN
  NEW TEMP XECUTE "SET TEMP="_OUTNUM_"/"_OUTDEN
  SET OUT("DECIMAL")=TEMP
  QUIT
  ;
CHECKSIGN(NUM,DEN) ;"Change -2/-4 --> 2/4   or 1/-4 -> -1/4
  IF (NUM>0)&(DEN>0) QUIT
  IF (DEN<0) DO  QUIT
  . SET NUM=NUM*-1,DEN=DEN*-1
  QUIT
  ;
REDUCEFRACT(FRACT)  ;"Reduce fraction to lowest form
  ;"INPUT: FRACT -- format:  NUMERATOR/DENOMINATOR.  e.g. "12/32" --> "3/8", or "4a/16b" --> "1a/4b"
  NEW ARR DO PARSEFRACT(FRACT,.ARR)
  QUIT ARR
  ;
MULTFRACT(FRACT1,FRACT2,OUT) ;"Multiple 2 numbers in fraction form
  NEW ARR1,ARR2,D,OUTNUM,OUTDEN
  KILL OUT
  DO PARSEFRACT(FRACT1,.ARR1)
  DO PARSEFRACT(FRACT2,.ARR2)
  IF (ARR1="NAN")!(ARR2="NaN") SET OUT="NaN" QUIT
  NEW NUM SET NUM=ARR1("NUM")*ARR2("NUM")
  NEW DEN SET DEN=ARR1("DEN")*ARR2("DEN")
  NEW VARNUM SET VARNUM=$GET(ARR1("VARS","NUM"))_$GET(ARR2("VARS","NUM"))
  NEW VARDEN SET VARDEN=$GET(ARR1("VARS","DEN"))_$GET(ARR2("VARS","DEN"))
  SET D=$$GCD(NUM,DEN)
  SET OUTNUM=NUM\D  
  SET OUTDEN=DEN\D
  DO CHECKSIGN(.OUTNUM,.OUTDEN)
  SET OUT("NUM")=OUTNUM
  SET OUT("DEN")=OUTDEN
  SET OUT("VARS","NUM")=$GET(VARNUM)
  SET OUT("VARS","DEN")=$GET(VARDEN)
  IF (OUTDEN_VARDEN)="1" DO
  . SET OUT=OUTNUM_VARNUM
  ELSE  DO
  . SET OUT=OUTNUM_VARNUM_"/"_OUTDEN_VARDEN
  NEW TEMP XECUTE "SET TEMP="_OUTNUM_"/"_OUTDEN
  SET OUT("DECIMAL")=TEMP
  QUIT
  ;
INVERTFRACT(FRACT,OUT)  ;"Invert, e.g. 5/8 --> 8/5"
  ;"Input: FRACT
  ;"OUTPUT: OUT  -- Format
  ;"            OUT="3/8" <-- example, fraction in lowest common form
  ;"            OUT("DECIMAL")=.375 <-- example, decimal form of 3/8
  ;"            OUT("NUM")=<NUMERATOR> reduced to lowest form. Numeric only (excludes any variable name)
  ;"            OUT("DEN")=<DENOMINATOR> reduced to lowest form. Numeric only (excludes any variable name)
  ;"            OUT("VARS","NUM")="" or <Any alpha chars from numerator>
  ;"            OUT("VARS","DEN")="" or <Any alpha chars from denominator>
  KILL OUT
  NEW ARR DO PARSEFRACT(FRACT,.ARR)
  NEW OUTNUM SET OUTNUM=ARR("DEN")
  NEW OUTDEN SET OUTDEN=ARR("NUM")
  IF +OUTDEN=0 SET OUT="NaN" QUIT
  DO CHECKSIGN(.OUTNUM,.OUTDEN)
  SET OUT("NUM")=OUTNUM
  SET OUT("DEN")=OUTDEN
  NEW VARNUM SET VARNUM=$GET(ARR("VARS","DEN"))
  NEW VARDEN SET VARDEN=$GET(ARR("VARS","NUM"))
  SET OUT("VARS","NUM")=VARNUM
  SET OUT("VARS","DEN")=VARDEN
  IF (OUTDEN_VARDEN)="1" DO
  . SET OUT=OUTNUM_VARNUM
  ELSE  DO
  . SET OUT=OUTNUM_VARNUM_"/"_OUTDEN_VARDEN
  NEW TEMP XECUTE "SET TEMP="_OUTNUM_"/"_OUTDEN
  SET OUT("DECIMAL")=TEMP
  QUIT
  ;
DIVFRACT(FRACT1,FRACT2,OUT) ;"Return Fract1/Fract2
  NEW ARR1,ARR2,INVARR2
  KILL OUT
  DO PARSEFRACT(FRACT2,.ARR2)
  DO INVERTFRACT(.FRACT2,.INVARR2)  
  IF (ARR2="NAN")!(INVARR2="NaN") SET OUT="NaN" QUIT
  DO MULTFRACT(.FRACT1,.INVARR2,.OUT) ;"Multiple 2 numbers in fraction form
  QUIT
  ;
ADDSUBFRACT(FRACT1,FRACT2,OUT,SIGN) ;"Add or subtract two numbers in fractional form.  E.G. 13/16 + 7/8
  SET SIGN=$GET(SIGN,1)
  KILL OUT SET OUT="NaN"
  NEW ARR1 DO PARSEFRACT(.FRACT1,.ARR1)
  NEW ARR2 DO PARSEFRACT(.FRACT2,.ARR2)
  IF (ARR1="NAN")!(ARR2="NaN") QUIT
  NEW NUM1 SET NUM1=ARR1("NUM")
  NEW DEN1 SET DEN1=ARR1("DEN")
  NEW NUM2 SET NUM2=ARR2("NUM")
  NEW DEN2 SET DEN2=ARR2("DEN")  
  NEW VARNUM1 SET VARNUM1=$GET(ARR1("VARS","NUM"))
  NEW VARDEN1 SET VARDEN1=$GET(ARR1("VARS","DEN"))
  NEW VARNUM2 SET VARNUM2=$GET(ARR2("VARS","NUM"))
  NEW VARDEN2 SET VARDEN2=$GET(ARR2("VARS","DEN"))
  IF VARNUM1'=VARNUM2 QUIT
  IF VARDEN1'=VARDEN2 QUIT
  NEW LCD SET LCD=$$LCM(DEN1,DEN2)
  SET NUM1=NUM1*(LCD/DEN1)
  SET NUM2=NUM2*(LCD/DEN2)*SIGN
  NEW OUTNUM SET OUTNUM=NUM1+NUM2
  NEW OUTDEN SET OUTDEN=LCD
  DO PARSEFRACT(OUTNUM_VARNUM1_"/"_OUTDEN_VARDEN1,.OUT)
  QUIT  
  ;
ADDFRACT(FRACT1,FRACT2,OUT) ;"Add two numbers in fractional form.  E.G. 13/16 + 7/8
  DO ADDSUBFRACT(.FRACT1,.FRACT2,.OUT,1)
  QUIT
  ;
SUBFRACT(FRACT1,FRACT2,OUT) ;"Sutract two numbers in fractional form.  E.G. 13/16 + 7/8
  DO ADDSUBFRACT(.FRACT1,.FRACT2,.OUT,-1)
  QUIT
  ;
ACTFRACT(FRACT1,FRACT2,OPERATOR,OUT,SHOW) ;" Perform operation on fract1 and fract 2
  ;"INPUT: FRACT -- format:  NUMERATOR/DENOMINATOR.  e.g. "12/32" --> "3/8", or "4a/16b" --> "1a/4b"
  ;"OUTPUT: OUT  -- Format
  ;"            OUT="3/8" <-- example, fraction in lowest common form
  ;"            OUT("DECIMAL")=.375 <-- example, decimal form of 3/8
  ;"            OUT("NUM")=<NUMERATOR> reduced to lowest form. Numeric only (excludes any variable name)
  ;"            OUT("DEN")=<DENOMINATOR> reduced to lowest form. Numeric only (excludes any variable name)
  ;"            OUT("VARS","NUM")="" or <Any alpha chars from numerator>
  ;"            OUT("VARS","DEN")="" or <Any alpha chars from denominator>
  ;
  SET OUT="NaN"
  IF OPERATOR="+" DO  
  . DO ADDFRACT(.FRACT1,.FRACT2,.OUT)
  ELSE  IF OPERATOR="-" DO  
  . DO SUBFRACT(.FRACT1,.FRACT2,.OUT)
  ELSE  IF OPERATOR="*" DO  
  . DO MULTFRACT(.FRACT1,.FRACT2,.OUT)
  ELSE  IF OPERATOR="/" DO  
  . DO DIVFRACT(.FRACT1,.FRACT2,.OUT)
  IF $GET(SHOW)=1 DO
  . WRITE !,FRACT1," ",OPERATOR," ",FRACT2," --> ",OUT,!
  . IF $DATA(OUT)>1 ZWR OUT
  QUIT OUT
  ;
TESTFRACT(OUT) ;
  NEW FRACT1 SET FRACT1="7/5"
  NEW FRACT2 SET FRACT2="19/32"
  DO ACTFRACT(FRACT1,FRACT2,"+",.OUT,1)
  DO ACTFRACT(FRACT1,FRACT2,"-",.OUT,1)
  DO ACTFRACT(FRACT1,FRACT2,"*",.OUT,1)
  DO ACTFRACT(FRACT1,FRACT2,"/",.OUT,1)
  QUIT