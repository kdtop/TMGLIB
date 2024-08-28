TMGZWR  ;TMG/kst/Non-propriatary code for ZWRITE; 3/5/13, 2/2/14
         ;;1.0;TMG-LIB;**1**;3/5/13
 ;
 ;"This code was written by Sam Habiel 
 ;"posted on public Hardhats forum on 8/2/12.
 ;"Reformatted her for coding consistence with TMG library.
 ; 
 ;"=======================================================================
 ;" RPC -- Public Functions.
 ;"=======================================================================
 ;"ZWRITE(NAME) -- Replacement for ZWRITE ; Public Proc
 ;"ZWR2ARR(NAME,OUTREF) --Output ZWR output into an array
 ;
 ;"RCC(NA) -- Replace Control Chars in NA with $C( ), Returns encoded string;
 ;"FORMAT(V) -- Add quotes, replace control characters IF necessary; 
 ;"CCC(S) -- Test IF S Contains a Control Character or $C(255); 
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ; 
 ;"=======================================================================
 ;"=======================================================================
 ;"Dependencies: (none)
 ;"=======================================================================
 ;"=======================================================================
 ;
ZWRITE(NAME) ;
  ;"Replacement for ZWRITE ; Public Proc 
  ;"Pass NAME by name as a closed reference. 
  ;"lvn and gvn are both supported. 
  ;" : syntax is not supported (yet)
  ;
  NEW L SET L=$LENGTH(NAME) ;" Name length 
  ;
  ;"If last sub is *, remove it and close the ref 
  IF $EXTRACT(NAME,L-2,L)=",*)" SET NAME=$EXTRACT(NAME,1,L-3)_")"
  ;
  ;" Get last Subscript upon which we can't loop further  
  NEW ORIGLAST SET ORIGLAST=$QSUBSCRIPT(NAME,$QLENGTH(NAME))
  ;
  ;" Number of subscripts in the original name 
  NEW ORIGQL SET ORIGQL=$QLENGTH(NAME)
  ;
  ;" Write base if it exists 
  IF $DATA(@NAME)#2 W NAME,"=",$$FORMAT(@NAME),!
  ;" $QUERY through the name. 
  ;" Stop when we are out. 
  ;" Stop when the last subscript of the original name isn't the same as 
  ;" the last subscript of the Name. 
  FOR  SET NAME=$QUERY(@NAME) Q:(NAME="")!($QSUBSCRIPT(NAME,ORIGQL)'=ORIGLAST)  DO
  . W NAME,"=",$$FORMAT(@NAME),!
  QUIT 
  ;
ZWR2ARR(NAME,OUTREF,STARTIDX) ;"//kt added this function
  ;"Purpose: Output ZWR output into an array
  ;"Input: NAME --   PASS BY NAME.  Should be a A closed reference.
  ;"       OUTREF -  PASS BY NAME.  Name of array to put output into.
  ;"       STARTIDX -- OPTIONAL.  Starting index #.  Default=1
  ;"Output:  @OUTREF@(1)=First line
  ;"         @OUTREF@(2)=2nd line... etc. 
  ;"Results: None
  ;"Note: lvn and gvn are both supported. 
  ;"      ':' syntax is not supported (yet)
  ;
  NEW L SET L=$LENGTH(NAME) ;" Name length 
  IF $EXTRACT(NAME,L-2,L)=",*)" SET NAME=$EXTRACT(NAME,1,L-3)_")"
  NEW ORIGLAST SET ORIGLAST=$QSUBSCRIPT(NAME,$QLENGTH(NAME))
  NEW ORIGQL SET ORIGQL=$QLENGTH(NAME)
  SET STARTIDX=+$GET(STARTIDX) IF STARTIDX'>0 SET STARTIDX=1
  NEW IDX SET IDX=STARTIDX
  IF $DATA(@NAME)#2 DO
  . SET @OUTREF@(IDX)=NAME_"="_$$FORMAT(@NAME)
  . SET IDX=IDX+1
  FOR  SET NAME=$QUERY(@NAME) Q:(NAME="")!($QSUBSCRIPT(NAME,ORIGQL)'=ORIGLAST)  DO
  . SET @OUTREF@(IDX)=NAME_"="_$$FORMAT(@NAME)
  . SET IDX=IDX+1
  QUIT 
  ;
FORMAT(V) ;
  ;"Add quotes, replace control characters if necessary; 
  ;"Public Function
  ;
  ;"If numeric, nothing to do. 
  ;"If no encoding required, then return as quoted string. 
  ;"Otherwise, return as an expression with $C()'s and strings. 
  IF +V=V QUIT V                    ;"If numeric, just return the value. 
  NEW QT SET QT=""""                ;"Quote 
  IF $FIND(V,QT) DO                 ;"Check IF V contains any Quotes 
  . SET P=0                         ;"Position pointer into V 
  . FOR  SET P=$F(V,QT,P) QUIT:'P  DO  ;"find next " 
  . . SET $E(V,P-1)=QT_QT           ;"double each " 
  . . SET P=P+1                     ;"skip over NEW " 
  IF $$CCC(V) DO  QUIT V            ;"If control character is present DO this and QUIT 
  . SET V=$$RCC(QT_V_QT)            ;"Replace control characters in "V" 
  . SET:$EXTRACT(V,1,3)="""""_" $EXTRACT(V,1,3)="" ;" Replace doubled up quotes at start 
  . SET L=$L(V) SET:$EXTRACT(V,L-2,L)="_""""" $EXTRACT(V,L-2,L)="" ;" Replace doubled up quotes at end 
  QUIT QT_V_QT                      ;"If no control characters, QUIT with "V" 
  ; 
CCC(S) ;
  ;"Test IF S Contains a Control Character or $C(255); 
  ;"Public Function
  QUIT:S?.E1C.E 1
  QUIT:$FIND(S,$CHAR(255)) 1
  QUIT 0
  ;
RCC(NA) ;
  ;"Replace control chars in NA with $C( ). 
  ;"Returns encoded string; 
  ;"Public Function 
  QUIT:'$$CCC(NA) NA                               ;"No embedded ctrl chars 
  NEW OUT SET OUT=""                               ;"Holds output name 
  NEW CC SET CC=0                                  ;"Count ctrl chars in $C( 
  NEW C                                            ;"Temp hold each char 
  FOR I=1:1:$LENGTH(NA) SET C=$EXTRACT(NA,I) DO    ;"For each char C in NA 
  . IF (C'?1C),(CC'=255) DO  SET OUT=OUT_C QUIT    ;"Not a ctrl char 
  . . IF CC SET OUT=OUT_")_""",CC=0                ;"Close up $C(... IF one is open 
  . IF CC DO 
  . . IF CC=256 SET OUT=OUT_")_$CHAR("_$ASCII(C),CC=0  ;"Max args in one $C( 
  . . ELSE  SET OUT=OUT_","_$ASCII(C)              ;"Add next ctrl char to $C( 
  . ELSE  SET OUT=OUT_"""_$C("_$ASCII(C) 
  . SET CC=CC+1
  . QUIT
  QUIT OUT
  ;