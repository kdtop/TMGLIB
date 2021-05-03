TMGTIUO7 ;TMG/kst-Text objects for use in CPRS ; 7/20/12, 2/2/14, 3/24/21
         ;;1.0;TMG-LIB;**1,17**;7/20/12
 ;
 ;"Kevin Toppenberg MD
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;" This is spill over code from TMGTIUOJ, to make that file size smaller.
 ;"
 ;"=======================================================================
 ;"PUBLIC FUNCTIONS
 ;"=======================================================================
 ;"PARSEA1C(STR,ARRAY,MSG)  --Parse HgbA1c values (but may be usable for other situations)
 ;"PARSEPVX(STR,ARRAY,MSG) --Parse pneumovax entries in data table. 
 ;"
 ;"=======================================================================
 ;"PRIVATE FUNCTIONS
 ;"=======================================================================
 ;"PARSE1(STR)-- convert entry like '8.2 on 8/31/12'  or '5.7 12/30/11'

 ;"=======================================================================
 ;"Dependancies : 
 ;"=======================================================================
PARSTABL(TABLEARRAY) ;"Parse table array to enhance data extraction
        ;"Input: TABLEARRAY -- This is the array of the table, as created by 
        ;"                     $$GETTABLX(TMGDFN,LABEL,.TABLEARRAY)
        ;"Output: TABLEARRAY is modified to parsed values, as follows.  
        ;"        TABLEARRAY('KEY-VALUE',<label>,"PARSED",Index#)=Value^FMDate^SourceText
        ;"                  index is in date order, with #1 being most recent. 
        NEW STR SET STR=""
        FOR  SET STR=$ORDER(TABLEARRAY("KEY-VALUE",STR)) QUIT:STR=""  DO
        . NEW LINE SET LINE=$GET(TABLEARRAY("KEY-VALUE",STR))
        . NEW TEMP,MSG
        . DO PARSEDAT(LINE,.TEMP,.MSG)
        . IF +MSG<0 QUIT
        . MERGE TABLEARRAY("KEY-VALUE",STR,"PARSED")=TEMP
        QUIT
        ;
PARSEA1C(STR,ARRAY,MSG)  ;"Parse HgbA1c values (but may be usable for other situations)
        ;"Purpose: Parse string of values into ordered array of values
        ;"E.g. inputs  6.0 on 04/19/11 <--   6.5 on 01/19/11 <-- 7.1 on 11/15/10 <-- 6.7; 6.5; 7.0;
        ;"             9.2 (home) 8/23/12; 4/17/12 9.0; 2/9/12 9.9 (home kit)
        ;"NOTE: the ';' will have top priority dividing elements
        ;"      next are arrows: "<--" or "-->"
        ;"Input: STR -- the string of values to parse
        ;"       ARRAY -- AN OUT PARAMETER.  PASS BY REFERENCE.  PRIOR VALUES KILLED.
        ;"       MSG -- AN OUT PARAMETER.  PASS BY REFERENCE. PRIOR VALUES KILLED
        ;"Output: ARRAY filled as follows:
        ;"          ARRAY(Idx#)='Value^FMDATE^Orig. Str Segment
        ;"          Note: Idx# should be in decreasing date order (newest is #1)
        ;"          Note: FMDATE may be empty.
        ;"        MSG -- -1^Message IF problem.
        ;"Result: none 
        DO PARSEDAT(.STR,.ARRAY,.MSG)  ;"Parse data values
        QUIT
        ;
PARSEDAT(STR,ARRAY,MSG)  ;"Parse data values 
        ;"Purpose: Parse string of values into ordered array of values
        ;"E.g. inputs  6.0 on 04/19/11 <--  6.5 on 01/19/11 <-- 7.1 on 11/15/10 <-- 6.7; 6.5; 7.0;
        ;"             9.2 (home) 8/23/12; 4/17/12 9.0; 2/9/12 9.9 (home kit)
        ;"NOTE: the ';' will have top priority dividing elements
        ;"      next are arrows: "<--" or "-->"
        ;"Input: STR -- the string of values to parse
        ;"       ARRAY -- AN OUT PARAMETER.  PASS BY REFERENCE.  PRIOR VALUES KILLED.
        ;"       MSG -- AN OUT PARAMETER.  PASS BY REFERENCE. PRIOR VALUES KILLED
        ;"Output: ARRAY filled as follows:
        ;"          ARRAY(Idx#)='Value^FMDATE^Orig. Str Segment
        ;"          Note: Idx# should be in decreasing date order (newest is #1)
        ;"          Note: FMDATE may be empty.
        ;"        MSG -- -1^Message IF problem.
        ;"Result: none 
        NEW TEMPARR,IDX,DIR,ARROW
        KILL ARRAY
        SET MSG=""
        IF (STR["<--")&(STR["-->") DO  
        . SET MSG="-1^String contains '<--' and '-->', so can't ensure order of values"        
        IF STR["-->" SET DIR=-1,ARROW="-->"
        ELSE  SET DIR=1,ARROW="<--"
        IF STR[";" DO SPLIT2AR^TMGSTUT2(STR,";",.TEMPARR)
        IF +$GET(TEMPARR("MAXNODE"))=0 SET TEMPARR(1)=STR
        SET IDX=0
        FOR  SET IDX=$ORDER(TEMPARR(IDX)) QUIT:(+IDX'>0)  DO
        . NEW S2 SET S2=$GET(TEMPARR(IDX)) QUIT:S2=""
        . IF S2[ARROW DO
        . . NEW ARR2
        . . DO SPLIT2AR^TMGSTUT2(S2,ARROW,.ARR2)
        . . IF +$GET(ARR2("MAXNODE"))=0 QUIT
        . . KILL TEMPARR(IDX)
        . . NEW IDX2 SET IDX2=""
        . . FOR  SET IDX2=$ORDER(ARR2(IDX2),DIR) QUIT:(+IDX2'>0)  DO
        . . . SET IDX=IDX+0.001
        . . . SET TEMPARR(IDX)=$GET(ARR2(IDX2))
        KILL TEMPARR("MAXNODE")
        NEW CT SET CT=1
        SET IDX=""
        FOR  SET IDX=$ORDER(TEMPARR(IDX)) QUIT:(+IDX'>0)  DO
        . NEW S2 SET S2=$$TRIM^XLFSTR($GET(TEMPARR(IDX)))
        . SET S2=$$PARSE1(S2)
        . IF S2="" KILL TEMPARR(IDX) QUIT
        . SET ARRAY(CT)=S2 SET CT=CT+1
PVDN    QUIT
        ;        
PARSE1(STR) ;
        ;"Purpose: convert entry like '8.2 on 8/31/12'  or '5.7 12/30/11'
        NEW FMDATE SET FMDATE=-1
        NEW VAL SET VAL=""
        IF $LENGTH(STR," ")'>1,$$ISNUM^TMGSTUT3(STR) SET VAL=STR GOTO P1DN
        NEW TEMPARR
        DO SPLIT2AR^TMGSTUT2(STR," ",.TEMPARR)
        NEW IDX SET IDX=""
        FOR  SET IDX=$ORDER(TEMPARR(IDX)) QUIT:(+IDX'>0)  DO
        . NEW S2 SET S2=$GET(TEMPARR(IDX))
        . SET S2=$$TRIM^XLFSTR(S2) QUIT:S2=""
        . NEW ISNUM SET ISNUM=$$ISNUM^TMGSTUT3(S2)
        . IF ISNUM,(VAL="") SET VAL=S2 QUIT
        . IF FMDATE=-1 SET FMDATE=$$FMDATE(S2)
P1DN    IF FMDATE=-1 SET FMDATE=""
        QUIT VAL_"^"_FMDATE_"^"_STR  
        ;
        ;"===========================================================
        ;"===========================================================
        ;
PARSEPVX(STR,ARRAY,MSG) ;"Parse pneumovax entries in data table. 
        ;"Purpose: Parse string of values into ordered array of values
        ;"E.g. inputs  2008 PER PATIENT AT LAUGHLIN
        ;"             8/5/10 AT AGE 65
        ;"             DECLINES 10/20/08, 1/19/2010, 1/18,2011
        ;"Input: STR -- the string of values to parse
        ;"       ARRAY -- AN OUT PARAMETER.  PASS BY REFERENCE.  PRIOR VALUES KILLED.
        ;"       MSG -- AN OUT PARAMETER.  PASS BY REFERENCE. PRIOR VALUES KILLED
        ;"Output: ARRAY filled as follows.  Prior data killed
        ;"          ARRAY(FMDATE,VALUE)=""
        ;"                value will be "Y" -- yes (given or prior history of as of date)
        ;"                              "O" -- ORDERED
        ;"                              "D" -- DECLINED
        ;"                              "S" -- SCRIPT
        ;"                              "R" -- REFUSED
        ;"          ARRAY("REACTION")=symptoms (if any)
        ;"        MSG -- -1^Message IF problem.
        ;"Result: none
        ;"Step #1, separate line into different parts, into array
        
        NEW S1 SET S1=""
        NEW LASTWORD,TEMPARRAY
        NEW REACTION SET REACTION=""
        KILL ARRAY
        NEW FIRSTTIME SET FIRSTTIME=1
        FOR  QUIT:STR=""  DO
        . NEW STRB
        . SET LASTWORD=S1
        . SET S1=$$NEXTPART(STR,.STRB)
        . SET S1=$TRANSLATE(S1,"""","") ;"convert "2007" --> 2007
        . SET S1=$$UP^XLFSTR(S1)
        . NEW CODE SET CODE=$$KYWDCODE(S1,LASTWORD)
        . IF CODE="#RXN#" DO  QUIT
        . . SET REACTION=$$TRIM^XLFSTR($PIECE(STR,"REACTION:",2))
        . . SET STR=""
        . IF CODE'="" DO  QUIT
        . . SET LASTWORD=S1
        . . SET STR=$$PROCCESS(STRB,.TEMPARRAY,CODE,.LASTWORD)
        . NEW JUSTNUM SET JUSTNUM=$$JUSTNUM(S1)
        . IF FIRSTTIME,JUSTNUM'="" DO  QUIT
        . . SET LASTWORD=JUSTNUM
        . . SET STR=$$PROCCESS(STR,.TEMPARRAY,"Y",.LASTWORD)
        . SET STR=STRB
        . ;"SET FIRSTTIME=0
        NEW IDX SET IDX=""
        FOR  SET IDX=$ORDER(TEMPARRAY(IDX)) QUIT:IDX=""  DO
        . SET STR=$GET(TEMPARRAY(IDX)) QUIT:STR=""
        . NEW FMDATE SET FMDATE=$PIECE(STR,"^",1) QUIT:FMDATE=""
        . NEW VAL SET VAL=$PIECE(STR,"^",2) QUIT:VAL=""
        . SET ARRAY(FMDATE,VAL)=""
        IF REACTION'="" SET ARRAY("REACTION")=REACTION
        QUIT
        ;
KYWDCODE(S1,LASTWORD) ;"RETURN CODE FOR KEYWORD, IF FOUND
        NEW RESULT SET RESULT=""
        IF S1["ORDER" SET RESULT="O" GOTO KWDN
        IF S1["RECOM" SET RESULT="S" GOTO KWDN
        IF S1["SCRIPT" SET RESULT="S" GOTO KWDN
        IF S1["DECLIN" SET RESULT="D" GOTO KWDN
        IF S1["REFUS" SET RESULT="R" GOTO KWDN
        IF (S1["GIVE")&(LASTWORD'["SCRIPT") SET RESULT="Y" GOTO KWDN
        IF S1["ABOUT" SET RESULT="Y" GOTO KWDN
        IF S1["AROUND" SET RESULT="Y" GOTO KWDN
        IF S1["DONE" SET RESULT="Y" GOTO KWDN
        IF S1["REACTION:" SET RESULT="#RXN#" GOTO KWDN
        IF (S1="ON")&(LASTWORD="") SET RESULT="Y" GOTO KWDN
        IF (S1="IN")&(LASTWORD="") SET RESULT="Y" GOTO KWDN
KWDN    QUIT RESULT
        ;
PROCCESS(STR,ARRAY,CODE,LASTWORD)  ;" Process ORDERED DATE,DATE,DATE,DATE  Stop when another word encountered
        NEW DONE SET DONE=0
        NEW S1 SET S1=LASTWORD
        NEW RESULT SET RESULT=""
        FOR  QUIT:DONE  DO
        . IF STR="" SET DONE=1,RESULT="" QUIT
        . NEW STRB
        . SET LASTWORD=S1
        . NEW FMDATE SET FMDATE=-1
        . SET S1=$$NEXTPART(STR,.STRB)
        . SET S1=$$TRIM^XLFSTR(S1)
        . SET S1=$TRANSLATE(S1,"~""","") ;"convert "2007" or ~2007 --> 2007
        . IF $LENGTH(S1)>3 SET FMDATE=$$FMDATE(S1)
        . IF FMDATE>0 DO  QUIT
        . . NEW IDX SET IDX=+$ORDER(ARRAY(""),-1)+1
        . . SET ARRAY(IDX)=FMDATE_"^"_CODE
        . . SET STR=STRB
        . NEW TEMPCODE SET TEMPCODE=$$KYWDCODE(S1,LASTWORD)
        . IF TEMPCODE'=""  SET DONE=1,RESULT=STR QUIT
        . SET STR=STRB
        QUIT RESULT
        ;        
NEXTPART(STR,STRB,DIVCHS) ;
        SET DIVCHS=$GET(DIVCHS,"-,;. ()")
        NEW RESULT SET RESULT=""
        NEW J FOR J=1:1:$LENGTH(STR) QUIT:RESULT'=""  DO
        . NEW CH SET CH=$E(STR,J)
        . IF (DIVCHS'[CH)&(J<$LENGTH(STR)) QUIT
        . IF DIVCHS'[CH SET J=J+1
        . SET RESULT=$EXTRACT(STR,1,J-1)
        . NEW J2 FOR J2=J+1:1:$LENGTH(STR) QUIT:(DIVCHS'[$E(STR,J2))
        . SET STRB=$EXTRACT(STR,J2,999)
        QUIT RESULT
        ;
FMDATE(STR) ;"STR --> FMDATE
        NEW X,Y,%DT SET %DT="P"
        SET X=STR DO ^%DT
        QUIT Y
        ;
JUSTNUM(STR,EXTRACH) ;"Strip all non-numeric chars (but allow any Extra characters specified in EXTRACH)
        NEW J,CH,RESULT SET RESULT=""
        FOR J=1:1:$L(STR) SET CH=$E(STR,J) SET:("1234567890.~/"[CH)!($GET(EXTRACH)[CH) RESULT=RESULT_$E(STR,J)
ISNMDN  QUIT RESULT
        ;
