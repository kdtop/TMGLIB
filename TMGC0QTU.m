TMGC0QTU ;TMG/kst/TMG CPT2 utilities ;10/17/17
         ;;1.0;TMG-LIB;**1**;10/17/17
EXCLUDE(DFN) ;"
  ;"This function determines whether patient should be excluded based on
  ;insurance
  ;"Result: 1 to exclude, 0 to include
  NEW TMGRESULT SET TMGRESULT=1  ;"EXCLUDE BY DEFAULT
  NEW AGE K VADM SET AGE=+$$AGE^TIULO(DFN)
  IF AGE>64.9 SET TMGRESULT=0 GOTO EXDN
  NEW INS2INC
  SET INS2INC=",3,4,5,6,13,17,36,15," ;"IENS OF ACCEPTABLE INS IN #36. USING LEADING AND TRAILING COMMAS FOR EASE OF MATCHING
  ;"SET INS2INC=",4,"
  NEW AGERELATED SET AGERELATED=",17,15,4,"
  ;"SET INS2INC=",5,"
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(^DPT(DFN,.312,IDX)) QUIT:IDX'>0  DO
  . NEW INSIEN SET INSIEN=","_$P($GET(^DPT(DFN,.312,IDX,0)),"^",1)_","
  . IF INS2INC'[INSIEN QUIT
  . ;"NEW AGE K VADM SET AGE=$$AGE^TIULO(DFN)
  . ;"IF AGE<65 QUIT
  . IF (AGERELATED[INSIEN)&(AGE<65) QUIT
  . SET TMGRESULT=0
EXDN
  QUIT TMGRESULT
  ;"
