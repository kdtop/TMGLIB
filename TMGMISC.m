TMGMISC ;TMG/kst/Misc utility library ;03/25/06; 7/31/15, 3/24/21
	;;1.0;TMG-LIB;**1**;07/12/05
	;
	;"TMG MISCELLANEOUS FUNCTIONS
	;
	;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
	;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
	;"
	;"This file is part of the TMG LIBRARY, and may only be used in accordence
	;" to license terms outlined in separate file TMGLICNS.m, which should
	;" always be distributed with this file.;
	;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
	;
	;"=======================================================================
	;" API -- Public Functions.;
	;"=======================================================================
	;"STARTRPC -- Start up RPCBroker on port 9210
	;"STOPRPC -- Stop RPCBroker on port 9210
	;"STOPTSKM -- Stop TaskMan non-interactively
	;"EDITPT(AddOK)
	;"GetPersonClass(PersonClass,ProviderType,Specialty)
	;"$$DocLines(IEN,Chars) -- Count number of lines and chars in a 8925 WP field
	;"$$WPChars(Ptr)
	;"$$RoundUp(n)
	;"$$RoundDn(n)
	;"$$Round(n)
	;"$$InList(Value,ArrayP) -- return IF Value is in an array.;
	;"$$ListCt(pArray)
	;"$$LISTCT(pArray) -- same as $$ListCt(pArray)
	;"$$NodeCt(pArray) -- count all the nodes in an array
	;"$$IndexOf(pArray,value)
	;"ListPack(pArray,StartNum,IncValue)
	;"ListAdd(pArray,index,value)
	;"ListAnd(pArray1,pArray2,pResult)
	;"ListNot(pArray1,pArray2,pResult)
	;"$$DTFormat(FMDATE,format) -- format fileman dates
	;"$$COMPDOB(DOB1,DOB2) -- compare two dates
	;"BrowseBy(CompArray,ByTag) -- Allow a user to interact with dynamic text tree
	;"$$COMPNAME(Name1,Name2) -- compare two names
	;"$$FRMTNAME(NAME,CUTTITLE) ;
	;"$$FormatName(Name,CutTitle)
	;"$$HEXCHR(V) -- Take one BYTE and return HEX Values
	;"$$HEXCHR2(n,digits) -- convert a number (of arbitrary length) to HEX digits
	;"$$HEX2DEC(HEX) -- convert a string like this $10 to decimal number (e.g. 16)
	;"$$DEC2BIN(INT,DIGITS) -- INTEGER TO BINARY STRING
	;"$$BIN2DEC(BINSTR) -- BINARY STRING TO DECIMAL
	;"$$BIN2HEX(BINSTR) -- BINARY STRING TO HEX
	;"$$GETUTF8(CODEPT,OPTION) -- GET BYTE SEQUENCE FOR UNICODE CODEPOINT
	;"$$OR(a,b)   ; perform a bitwise OR on operands a and b
	;"ParsePos(pos,label,offSET,routine,dmod)
	;"ScanMod(Module,pArray)
	;"ConvertPos(Pos,pArray)
	;"COMPARRAY(PARR1,PARR2) -- upper case wrapper for CompArray(pArray1,pArray2)
	;"CompArray(pArray1,pArray2) return IF two arrays are identical
	;"$$CompABArray(pArrayA,pArrayB,pOutArray) -- FULL compare of two arrays, return diffArray
	;"$$IterTemplate(Template,Prior)
	;"$$NumPieces(s,delim,maxPoss) -- return number of pieces in string
	;"$$LastPiece(s,delim,maxPoss) -- return the last piece of a string
	;"$$ParseLast(s,remainS,delim,maxPoss) -- return the last piece AND the first part of the string
	;"$$Trim1Node(pRef) -- To shorten a reference by one node.;
	;"BROWSEASK --  ask user for the name of an array, then display nodes
	;"BRWSASK2 -- Improved... Ask user for the name of an array, then display nodes
	;"BROWSENODES(current,Order,paginate,countNodes) -- display nodes of specified array
	;"BRWSNOD2(curRef,Order,countNodes) -- display nodes of specified array, using Scroll box
	;"ShowNodes(pArray,order,paginate,countNodes) -- display all the nodes of the given array
	;"ShowNod2(pArray,order,countNodes) -- display all the nodes of the given array, using Scroll box
	;"$$IsNumeric(value) -- determine IF value is pure numeric.;
	;"$$ClipDDigits(Num,digits) -- clip number to specified number of digits
	;"LaunchScreenman(File,FormIEN,RecIEN,Page) -- launching point screenman form
	;"$$NumSigChs --determine how many characters are signficant in a variable name
	;"MkMultList(input,List) -- create a list of entries, given a string containing a list of entries.;
	;"MkRangeList(Num,EndNum,List) -- create a list of entries, given a starting and ending number
	;"UNHASH(X) -- Unhasher of XUSHSH hashing routine  (kst 4-21-05)
	;"RAND(LO,HI) -- Random Range
	;"$$DELTA(INITVAL,DELTA,LO,HI) -- Change INITVAL by DELTA, but clip at LO,HI (if provided)
	;"INBOUNDS(NUM,LOW,HI) --Return number, within bounds
	;"MAX(NUM1,NUM2) --Return greater of 2 numbers
	;"MIN(NUM1,NUM2) --Return lessor of 2 numbers
	;"MERGESN(SRC,DEST) -- Merge SUBNODES of SRC (but not value of SRC) into DEST
	;"=======================================================================
	;"PRIVATE API FUNCTIONS
	;"=======================================================================
	;"GetPersonClass(PersonClass,ProviderType,Specialty)
	;"$$IsSuffix(s)
	;"$$IsTitle(s)
	;"ShowBy(CompArray,ByTag,aOpen,bOpen,cOpen)
	;"CtTemplate(Template) -- return the Count of IEN's stored in a SORT TEMPLATE
	;"=======================================================================
	;"DEPENDENCIES
	;"      TMGDBAPI
	;"      TMGIOUTL
	;"      TMGDEBUG
	;"      TMGSTUTL
	;"=======================================================================
	;"=======================================================================
	;
STARTRPC ;
	;" -- Start up RPCBroker on port 9210
	WRITE "Starting RPC Broker on port 9210",!
	DO STRT^XWBTCP(9210)
	WRITE !
	QUIT
	;
STOPRPC ;
	;" -- Stop RPC Broker on port 9210
	WRITE "Stopping RPC Broker on port 9210",!
	DO STOP^XWBTCP(9210)
	WRITE !
	QUIT
	;
STOPTSKM  ;
	;"-- Shut Down Task Managers non-interactively
	;" Taken from STOP^ZTMKU
	;
	WRITE !,"Shutting down TaskMan and submanagers."
	DO GROUP^ZTMKU("SMAN^ZTMKU(NODE)")
	DO GROUP^ZTMKU("SSUB^ZTMKU(NODE)")
	WRITE !,"Okay!",!
	QUIT
	;
EDITPT(TMGADDOK) ;
	;"Purpose: To ask for a patient name, and then allow editing
	;"Input: TMGADDOK: IF 1, then adding NEW patients is allowed
	;"Result: none
	;
	DO LO^DGUTL
	SET DGCLPR=""
	NEW DGDIV SET DGDIV=$$PRIM^VASITE
	;
	IF DGDIV>0 SET %ZIS("B")=$PIECE($GET(^DG(40.8,+DGDIV,"DEV")),U,1)
	;
	KILL %ZIS("B")
	IF '$DATA(DGIO),$PIECE(^DG(43,1,0),U,30) DO
	. SET %ZIS="N",IOP="HOME"
	. DO ^%ZIS
	;
A  ;
	NEW TMGDFN SET TMGDFN=$GET(DFN)
	DO ENDREG^DGREG(TMGDFN)
	DO  IF (Y<0) GOTO EDITDONE
	. WRITE !!
	. IF $GET(TMGADDOK)=1 DO
	. . SET DIC=2,DIC(0)="ALEQM"
	. . SET DLAYGO=2
	. ELSE  DO
	. . SET DIC=2,DIC(0)="AEQM"
	. . SET DLAYGO=0
	. KILL DIC("S")
	. DO ^DIC
	. KILL DLAYGO
	. IF Y<0 QUIT
	. SET (TMGDFN,DA)=+Y
	. SET DGNEW=$P(Y,"^",3)
	. NEW Y
	. DO PAUSE^DG10
	. DO BEGINREG^DGREG(TMGDFN)
	. IF DGNEW DO NEW^DGRP
	;
	IF +$GET(DGNEW) DO
	. ;" query CMOR for Patient Record Flag Assignments IF NEW patient and
	. ;" display results.;
	. IF $$PRFQRY^DGPFAPI(TMGDFN) DO DISPPRF^DGPFAPI(TMGDFN)
	;
	SET (DGFC,CURR)=0
	SET DA=TMGDFN
	SET DGFC="^1"
	SET VET=$SELECT($DATA(^DPT(TMGDFN,"VET")):^("VET")'="Y",1:0)
	;
	SET %ZIS="N",IOP="HOME"
	DO ^%ZIS
	SET DGELVER=0
	;"DO EN^DGRPD
	;"IF $DATA(DGRPOUT) DO  GOTO A
	;". DO ENDREG^DGREG($G(TMGDFN))
	;". DO HL7A08^VAFCDD01
	;". KILL TMGDFN,DGRPOUT
	;
	;"DO HINQ^DG10
	IF $D(^DIC(195.4,1,"UP")) IF ^("UP") DO ADM^RTQ3
	;
	DO REG^IVMCQ($G(TMGDFN))  ;" send financial query
	;
	SET DGRPV=0
	DO EN1^DGRP
	;
EDITDONE ;
	IF $PIECE($GET(^VA(200,DUZ,"TMG")),"^",1)="C" DO
	. WRITE @IOF,!  ;"clear screen IF SETtings call for this.;
	;
	QUIT
	;
GetPersonClass(PersonClass,ProviderType,Specialty)    ;
	;"Purpose: To look through the PERSON CLASS file and find matching record
	;"Input -- PersonClass -- a value to match against the .01 field (PROVIDER TYPE)
	;"        Behavioral Health and Social Service
	;"        Chiropractors
	;"        Dental Service
	;"        Dietary and Nutritional Service
	;"        EMERGEncy Medical Service
	;"        Eye and Vision Services
	;"        Nursing Service
	;"        Nursing Service Related
	;"        Physicians (M.D. and D.O.)
	;"        etc.;
	;"      -- ProviderType -- a value to match against the 1 field (CLASSIFICATION)
	;"        Physician/Osteopath
	;"        Resident, Allopathic (includes Interns, Residents, Fellows)
	;"        Psychologist
	;"        Neuropsychologist
	;"        etc.;
	;"      -- Specialty -- a value to match against the 2 field (AREA OF SPECIALIZATION)
	;"Output -- (via results)
	;"Result -- Returns record number in PERSON CLASS file, OR 0 IF not found
	NEW RecNum,Params
	SET Params(0,"FILE")="PERSON CLASS"
	SET Params(".01")=$GET(PersonClass)
	SET Params("1")=$GET(ProviderType)
	SET Params("2")=$GET(Specialty)
	SET RecNum=$$RecFind^TMGDBAPI(.Params)
GPCDone ;
	QUIT RecNum
	;
	;
DocLines(IEN,Chars) ;
	;"Purpose: To count the number of lines and characters in a WP field
	;"         Initially it is targeted at entries in TIU DOCUMENT file.;
	;"Input:  IEN -- the record number in TIU DOCUMENT to count
	;"        Chars -- and OUT parameter. PASS BY REFERENCE
	;"Results: Returns number of lines, (with 1 decimal value)
	;"         Also will return character count in Chars, IF passed by reference
	;"NOte: This uses the Characters per line parameter value stored in
	;"      field .03 of TIU PARAMETERS (in ^TIU(8925.99))
	NEW CharsPerLine
	NEW LineCount SET LineCount=0
	SET Chars=0
	SET CharsPerLine=+$PIECE($GET(^TIU(8925.99,1,0)),"^",3)
	IF $GET(TMGDEBUG)>0 DO DEBUGMSG^TMGDEBU4(.TMGDBINDENT,"CharsPerLine=",CharsPerLine)
	;
	SET WPPtr=$name(^TIU(8925,IEN,"TEXT"))
	SET Chars=$$WPChars(WPPtr)
	IF $GET(TMGDEBUG)>0 DO DEBUGMSG^TMGDEBU4(.TMGDBINDENT,"Chars=",Chars)
	;
	IF CharsPerLine'=0 DO
	. SET LineCount=(((Chars/CharsPerLine)*10)\1)/10
	. ;"NEW IntLC,LC,Delta
	. ;"SET LC=Chars\CharsPerLine
	. ;"SET IntLC=Chars\CharsPerLine  ;" \ is integer divide
	. ;"SET Delta=(LC-IntLC)*10
	. ;"if Delta>4 SET IntLC=IntLC+1  ;"Round to closest integer value.;
	. ;"SET LineCount=IntLC
	;
	IF $GET(TMGDEBUG)>0 DO DEBUGMSG^TMGDEBU4(.TMGDBINDENT,"LineCount=",LineCount)
	;
	QUIT LineCount
	;
WPChars(Ptr)  ;
	;"Purpose: To count the number of characters in the WP field
	;"  pointed to by the name stored in Ptr
	;"Results: Returns number of characters, including spaces
	NEW index
	NEW Chars SET Chars=0
	;
	IF $GET(TMGDEBUG)>0 DO DEBUGMSG^TMGDEBU4(.TMGDBINDENT,"Ptr=",Ptr)
	SET index=$ORDER(@Ptr@(0))
	FOR  DO  QUIT:(index="")
	. IF index="" QUIT
	. IF $GET(TMGDEBUG)>0 DO DEBUGMSG^TMGDEBU4(.TMGDBINDENT,"index='",index,"'")
	. ;"NEW s SET s=$GET(@Ptr@(index,0)) WRITE "s=",s,!
	. SET Chars=Chars+$LENGTH($GET(@Ptr@(index,0)))
	. SET index=$ORDER(@Ptr@(index))
	;
	QUIT Chars
	;
RoundUp(n) ;
	;"SCOPE: PUBLIC
	;"Purpose: find the next greatest integer after decimal value of n (round up)
	;"  1.1 --> 2
	;"  1.0 --> 1
	;"  -2.8 --> 2
	;"input: n -- decimal or integer value
	;"output an integer, rounded up.;
	NEW result
	SET result=n\1
	IF result<n SET result=result+1
	QUIT result
	;
RoundDn(n)  ;
	;"SCOPE: PUBLIC
	;"Purpose: To round the  decimal value of n downward (towards 0)
	;"  1.4 --> 1
	;"  -2.2 --> -2
	;"input: n -- decimal or integer value
	;"output an integer, rounded down.;
	NEW result
	SET result=n\1
	QUIT result
	;
Round(n)  ;
	;"SCOPE: PUBLIC
	;"Purpose: find the nearest integer from decimal value of n
	;"  for values 0.0-0.49 --> 0
	;"  for values 0.5-0.99 --> 1
	;"input: n -- decimal or integer value
	;"output an integer, rounded to nearest integer
	NEW result SET result=n
	NEW decimal
	;
	SET decimal=+(n-(n\1))
	IF decimal<0.5 DO
	. SET result=$$RoundDn(n)
	ELSE  DO
	. SET result=$$RoundUp(n)
	;
	QUIT result
	;
InList(Value,ArrayP)  ;
	;"SCOPE: PUBLIC
	;"Purpose: To return IF Value is in an array. Match must be exact (i.e. '=')
	;"Input: Value -- the value to test for. Should not be an array
	;"   ArrayP -- the name of the array.  e.g. ArrayP="MyArray(""Title"")"
	;"Format of Array:  It may be in one of two possible formats:
	;"    1. MyArray("Title")=Value,   or
	;"    2. MyArray("Title")="*"  <-- a signal that multiple values are given
	;"      MyArray("Title",1)=Value1
	;"      MyArray("Title",2)=Value2
	;"      The '1','2', etc may anything
	;"Results: 1 IF Value is in list, 0 IF not
	NEW result SET result=0
	NEW index
	IF ($GET(ArrayP)'="")&($DATA(Value)=1) DO
	. IF @ArrayP'="*" SET result=(@ArrayP=$GET(Value)) QUIT
	. SET index=$ORDER(@ArrayP@("")) QUIT:(index="")
	. FOR  DO  QUIT:(index="")!(result=1)
	. . IF @ArrayP@(index)=Value SET result=1 QUIT
	. . SET index=$ORDER(@ArrayP@(index))
	;
ILDone ;
	QUIT result
	;
LISTCT(pArray) ;" SAAC complient entry point.;
	QUIT $$LISTCT^TMGMISC2(pArray)
ListCt(pArray)
	QUIT $$LISTCT^TMGMISC2(pArray)
	;
NodeCt(pArray)
	;"SCOPE: PUBLIC
	;"Purpose: to count all the nodes in an array
	;"Input: pArray -- PASS BY NAME.  pointer to (name of) array to test.;
	;"Output: the number of entries at highest level
	;"      e.g.  Array("TELEPHONE")=1234
	;"      Array("CAR")=4764
	;"      Array("DOG")=5213
	;"      Array("DOG","COLLAR")=5213  <-- IS counted
	;"  The above array would have a count of 4
	;"Results: returns count, or count up to point of any error
	NEW result SET result=0
	FOR  SET pArray=$query(@pArray),result=result+1 QUIT:(pArray="")
	QUIT result
	;
IndexOf(pArray,value)
	;"SCOPE: PUBLIC:
	;"Purpose: To search through an array of keys and values, and return 1st index (i.e. key) of value
	;"Input: pArray -- NAME OF array to search, format:
	;"    @pArray@(key1)=value1
	;"    @pArray@(key2)=value2
	;"    @pArray@(key3)=value3
	;"       value -- the value to search for
	;"Results: will return key for first found (based on $ORDER sequence),or "" IF not found
	NEW result SET result=""
	NEW i SET i=""
	NEW done SET done=0
	FOR  SET i=$ORDER(@pArray@(i)) QUIT:(i="")!(done=1)  DO
	. IF $GET(@pArray@(i))=value SET result=i,done=1
	;
	QUIT result
	;
ListPack(pArray,StartNum,IncValue)  ;
	;"SCOPE: PUBLIC
	;"Purpose: to take an array with numeric ordering and pack values.;
	;"      e.g. Array(3)="dog"
	;"     Array(5)="cat"
	;"     Array(75)="goat"
	;"      Will be pack as follows:
	;"     Array(1)="dog"
	;"     Array(2)="cat"
	;"     Array(3)="goat"
	;"Input: pArray -- pointer to (NAME OF) array to pack.;
	;"       StartNum -- OPTIONAL, default=1.  Value to start numbering at
	;"       IncValue -- OPTIONAL, default=1.  Amount to add to index value each time
	;"Output: array will be altered
	;"Results: none.;
	;"Notes: It is assumed that all of the indices are numeric
	;"       Nodes that are ALPHA (non-numeric) will be KILLED!!
	;"       If nodes have subnodes, they will be preserved.;
	NEW TMGlpArray
	NEW i
	NEW count SET count=$GET(StartNum,1)
	SET i=$ORDER(@pArray@(""))
	IF +i=i FOR  DO  QUIT:(+i'=i)
	. MERGE TMGlpArray(count)=@pArray@(i)
	. SET count=count+$GET(IncValue,1)
	. SET i=$ORDER(@pArray@(i))
	KILL @pArray
	MERGE @pArray=TMGlpArray
	QUIT
	;
ListTrim(pArray,startIndex,endIndex,CountName)  ;
	;"SCOPE: PUBLIC
	;"Purpose: Take a list with numeric (integer) ordering, and trim (KILL) entry
	;"   items startIndex...endIndex
	;"Input: pArray -- PASS BY NAME.  The array to trim
	;"       startIndex -- the first index item to KILL.  Default=1
	;"       endIndex -- the last index item to KILL. Default=1
	;"       CountName -- OPTIONAL.  The name of a node that includes the
	;"      final count of remaining nodes.  Default is "COUNT"
	;"Output:  Array items will be KILLed. Also, a node with the resulting count
	;"   of remaining items will be created, with name of CountName.  e.g.;
	;"   INPUT:  startIndex=1, endIndex=4
	;"         @pArray@(2)="grape"
	;"         @pArray@(3)="orange"
	;"         @pArray@(5)="apple"
	;"         @pArray@(7)="pear"
	;"         @pArray@(9)="peach"
	;"
	;"   OUTPUT:
	;"         @pArray@(5)="apple"
	;"         @pArray@(7)="pear"
	;"         @pArray@(9)="peach"
	;"         @pArray@("COUNT")=3
	SET startIndex=$GET(startIndex,1)
	SET endIndex=$GET(endIndex,1)
	SET CountName=$GET(CountName,"COUNT")
	KILL @pArray@(CountName)
	NEW i FOR i=startIndex:1:endIndex KILL @pArray@(i)
	DO ListPack(pArray)
	SET @pArray@(CountName)=$$ListCt(pArray)
	QUIT
	;
ListAdd(pArray,index,value) ;
	;"SCOPE: PUBLIC
	;"Purpose: To take a simple list and add to end of ist
	;"      e.g. Array("Apple")=75
	;"      Array("Pear")=19
	;"
	;"  DO ListAdd("Array","Grape",12)   -->
	;"
	;"      e.g. Array("Apple")=75
	;"      Array("Pear")=19
	;"      Array("Grape")=12
	;"Note: function creation aborted, because there is no intrinsic ordering in arrays.  I.e. the above would actually
	;"      be in this order, as returned by $ORDER():
	;"      e.g. Array("Apple")=75
	;"      Array("Grape")=12  <-- "G" comes before "P" alphabetically
	;"      Array("Pear")=19
	;"I'll leave this here as a reminder to myself next time.;
	QUIT
	;
LISTAND(REF1,REF2,OUTREF) ;"WRAPPER
	DO ListAnd(REF1,REF2,OUTREF)
	QUIT
	;
ListAnd(pArray1,pArray2,pResult)  ;
	;"Purpose: To take two lists, and create a third list that has only those entries that
	;"      exist in Array1 AND Array2
	;"Input: pArray1 : NAME OF array for list 1
	;"       pArray2 : NAME OF array for list 2
	;"       pResult : NAME OF array to results -- any preexisting entries will be KILLed
	;"Note: only TOP LEVEL nodes are considered, and *value* for pArray1 use for combined value
	;"E.g. of Use
	;"      @pArray1@("cat")="feline"
	;"      @pArray1@("dog")="canine"
	;"      @pArray1@("horse")="equinine"
	;"      @pArray1@("bird")="avian"
	;"      @pArray1@("bird","weight")=12  <--- will be ignored, not a top level node
	;"
	;"      @pArray2@("hog")="porcine"
	;"      @pArray2@("horse")="equinine"
	;"      @pArray2@("cow")="bovine"
	;"      @pArray2@("bird")="flier"  <----- note different value for key="bird"
	;"
	;"      resulting list:
	;"      @pResult@("horse")="equinine"
	;"      @pResult@("bird")="avian"  <-- note value from pArray1 used.;
	NEW Result
	NEW i SET i=$ORDER(@pArray1@(""))
	IF i'="" FOR  DO  QUIT:(i="")
	. IF $DATA(@pArray2@(i))#10 DO
	. . SET Result(i)=$GET(@pArray1@(i))
	. SET i=$ORDER(@pArray1@(i))
	;
	KILL @pResult
	MERGE @pResult=Result
	;
	QUIT
	;
ListNot(pArray1,pArray2,Verbose)  ;
	;"Purpose: To take two lists, and remove all entries from list 2 from list 1
	;"      exist in Array1 NOT Array2
	;"Input: pArray1 : NAME OF array for list 1
	;"       pArray2 : NAME OF array for list 2
	;"       Verbose: OPTIONAL.  IF 1 then verbose output, progress bar etc.;
	;"Note: only TOP LEVEL nodes are considered, and
	;"       *value* for pArray1 use for combined value
	;"E.g. of Use
	;"     list 1:
	;"     @pArray1@("cat")="feline"
	;"     @pArray1@("dog")="canine"
	;"     @pArray1@("horse")="equinine"
	;"     @pArray1@("bird")="avian"
	;"     @pArray1@("bird","weight")=12  <--- will be ignored, not a top level node
	;"
	;"     list 2:
	;"     @pArray1@("cat")="feline"
	;"     @pArray1@("horse")="equinine"
	;"
	;"     resulting list:
	;"     @pArray1@("dog")="canine"
	;"     @pArray1@("bird")="avian"
	;"     @pArray1@("bird","weight")=12
	;"
	NEW Itr,index
	SET index=$$ItrAInit^TMGITR(pArray2,.Itr)
	IF Verbose=1 DO PrepProgress^TMGITR(.Itr,20,1,"index")
	IF index'="" FOR  DO  QUIT:($$ItrANext^TMGITR(.Itr,.index)="")
	. KILL @pArray1@(i)
	QUIT
	;
	;
	;"Note: Sometime, compare this function to $$DATE^TIULS ... I didn't know about this function before!
DTFormat(FMDATE,format,Array)
	QUIT $$DTFORMAT^TMGMISC2(.FMDATE,.format,.Array)
	;
COMPDOB(DOB1,DOB2)  ;
	;"Purpose: to compare two DOB and return IF they match, or are similar
	;"Input: DOB1,DOB2 -- the two values to compare (in external format)
	;"Result: 0 - no similarity or equality
	;"  0.25  - doubt similarity
	;"  0.50  - possible similarity
	;"  0.75  - probable similarity
	;"  1 - exact match
	;"Note: I made this function because during lookups, I would get failures with data such as:
	;"      WILLIAM,JOHN G JR  05-21-60
	;"      WILLIAM,JOHN G JR  05-11-60   <-- date differs by one digit.;
	;"Rules for comparision
	;"      IF dates differ by 1 digit --> score of 0.75
	;"      IF dates differ by an absolute difference of < 1 months   --> 0.75
	;"      IF dates differ by an absolute difference of < 6 months   --> 0.50
	;"      IF dates differ by an absolute difference of < 1 year   --> 0.25
	;"      IF dates differ by 2 digits --> 0.25
	NEW DT1,DT2
	NEW result SET result=0
	NEW %DT
	SET X=DOB1 DO ^%DT SET DT1=Y   ;"convert into internal format to avoid format snafu's
	SET X=DOB2 DO ^%DT SET DT2=Y
	NEW DT1array,DT2array
	NEW temp
	IF DT1=DT2 SET result=1 GOTO CDOBDone
	;
	SET temp=$$DTFormat^TMGMISC(DT1,"mm/dd/yy",.DT1array) ;"parse date parts into array.;
	SET temp=$$DTFormat^TMGMISC(DT2,"mm/dd/yy",.DT2array)
	;
	;"Compare digits
	NEW NumDif SET NumDif=0
	NEW dg1,dg2
	;
	SET dg1=$EXTRACT($GET(DT1array("dd")),1,1)  SET dg2=$EXTRACT($GET(DT2array("dd")),1,1)
	IF dg1'=dg2 SET NumDif=NumDif+1
	SET dg1=$EXTRACT($GET(DT1array("dd")),2,2)  SET dg2=$EXTRACT($GET(DT2array("dd")),2,2)
	IF dg1'=dg2 SET NumDif=NumDif+1
	;
	SET dg1=$EXTRACT($GET(DT1array("mm")),1,1)  SET dg2=$EXTRACT($GET(DT2array("mm")),1,1)
	IF dg1'=dg2 SET NumDif=NumDif+1
	SET dg1=$EXTRACT($GET(DT1array("mm")),2,2)  SET dg2=$EXTRACT($GET(DT2array("mm")),2,2)
	IF dg1'=dg2 SET NumDif=NumDif+1
	;
	SET dg1=$EXTRACT($GET(DT1array("yy")),1,1)  SET dg2=$EXTRACT($GET(DT2array("yy")),1,1)
	IF dg1'=dg2 SET NumDif=NumDif+1
	SET dg1=$EXTRACT($GET(DT1array("yy")),2,2)  SET dg2=$EXTRACT($GET(DT2array("yy")),2,2)
	IF dg1'=dg2 SET NumDif=NumDif+1
	;
	IF NumDif=1 SET result=0.75 GOTO CDOBDone
	IF NumDif=2 SET result=0.50
	;
	;"Compare absolute date
	NEW H1,H2,DateDif
	SET H1=$$FMTH^XLFDT(DT1,1)
	SET H2=$$FMTH^XLFDT(DT2,1)
	SET DateDif=$$HDIFF^XLFDT(H1,H2,1) ;"1=results in 'days'
	IF $$HDIFF^XLFDT(H2,H1)>DateDif SET DateDif=$$HDIFF^XLFDT(H2,H1)
	;
	NEW score SET score=0
	IF DateDif<30 SET score=0.75
	IF DateDif<(30*6) SET score=0.50
	IF DateDif<365 SET score=0.25
	;
	IF score>result SET result=score
	;
CDOBDone  ;
	QUIT result
	;
BrowseBy(CompArray,ByTag)  ;
	;"Purpose: Allow a user to interact with dynamic text tree
	;"        that will open and close nodes.;
	;"Input:  CompArray -- array to browse.  Should be in this format
	;"          CompArray("opening tag",a,b,c,d)
	;"         ByTag -- the name to use in for "opening tag")
	NEW aOpen SET aOpen=0
	NEW bOpen SET bOpen=0
	NEW cOpen SET cOpen=0
	;
	NEW done SET done=0
	NEW input
	;
	FOR  DO  QUIT:(done=1)
	. DO ShowBy(.CompArray,ByTag,aOpen,bOpen,cOpen)
	. read "Enter option:",input:$GET(DTIME,3600),!
	. IF input="" SET input=0
	. IF +input>0 DO
	. . IF aOpen=0 DO
	. . . SET aOpen=input,bOpen=0,cOpen=0
	. . ELSE  IF bOpen=0 DO
	. . . SET bOpen=input,cOpen=0
	. . ELSE  IF cOpen=0 SET cOpen=input
	. ELSE  IF input=0 DO
	. . IF cOpen'=0 SET cOpen=0 QUIT
	. . IF bOpen'=0 SET bOpen=0 QUIT
	. . SET aOpen=0
	. ELSE  IF input="^" SET done=1
	;
	QUIT
	;
ShowBy(CompArray,ByTag,aOpen,bOpen,cOpen)  ;
	NEW a,b,c,d
	NEW acount SET acount=0
	NEW bcount SET bcount=0
	NEW ccount SET ccount=0
	NEW dcount SET dcount=0
	;
	WRITE #
	;
	SET a=$ORDER(CompArray(ByTag,""))
	IF a'="" FOR  DO  QUIT:(a="")
	. SET acount=acount+1
	. NEW nexta SET nexta=$ORDER(CompArray(ByTag,a))
	. NEW Aindent
	. IF (aOpen=0) DO
	. . IF acount<10 WRITE "0"
	. . WRITE acount,". "
	. ELSE  WRITE "... "
	. WRITE a,!
	. SET b=$ORDER(CompArray(ByTag,a,""))
	. IF (aOpen=acount)&(b'="") FOR  DO  QUIT:(b="")
	. . SET bcount=bcount+1
	. . NEW nextb SET nextb=$ORDER(CompArray(ByTag,a,b))
	. . NEW Bindent
	. . WRITE "    +--"
	. . IF (bOpen=0) DO
	. . . IF bcount<10 WRITE "0"
	. . . WRITE bcount,". "
	. . ELSE  WRITE "... "
	. . WRITE b,!
	. . IF nextb'="" SET Aindent="    |  "
	. . ELSE  SET Aindent="       "
	. . SET c=$ORDER(CompArray(ByTag,a,b,""))
	. . IF (bOpen=bcount)&(c'="") FOR  DO  QUIT:(c="")
	. . . SET ccount=ccount+1
	. . . NEW nextc SET nextc=$ORDER(CompArray(ByTag,a,b,c))
	. . . IF nextc'="" SET Bindent="    |  "
	. . . ELSE  SET Bindent="       "
	. . . WRITE Aindent,"    +--"
	. . . IF (cOpen=0) DO
	. . . . IF ccount<10 WRITE "0"
	. . . . WRITE ccount,". "
	. . . ELSE  WRITE "... "
	. . . WRITE c,!
	. . . SET d=$ORDER(CompArray(ByTag,a,b,c,""))
	. . . IF (cOpen=ccount)&(d'="") FOR  DO  QUIT:(d="")
	. . . . SET dcount=dcount+1
	. . . . WRITE Aindent,Bindent,"    +-- "
	. . . . IF dcount<10 WRITE "0"
	. . . . WRITE dcount,". "
	. . . . WRITE d,!
	. . . . SET d=$ORDER(CompArray(ByTag,a,b,c,d))
	. . . SET c=nextc
	. . SET b=nextb
	. SET a=nexta
SBDone
	QUIT
	;
COMPNAME(Name1,Name2)  ;
	;"Purpose: To compare two names, to see IF they are the name, or compatible.;
	;"        e.g. WILLIAMS,J BILL   vs. WILLAMS,JOHN BILL,  vs. WILLIAMS,JOHN B
	;"Input: Two names to compare
	;"Result:  0 --   IF entries conflict
	;"   0.5 -- IF entries are consistent (i.e. in example above)
	;"   1 --   IF entries completely match
	;"Note: This function WILL IGNORE a suffix.  This is because
	;"        WILLIAM,BILL    5-1-1950
	;"        WILLIAM,BILL SR 5-1-1950
	;"      would be considered the same person (the date is the determining factor)
	;"Rules: Last names must completely match or --> 0
	;"       If name is exactly the same, then --> 1
	;"       Initial must be same as first letters in name (e.g. N vs. NEWTON) --> 0.5
	;
	NEW result SET result=1
	;
	NEW NArray1,NArray2,TMGMsg
	;
	SET Name1=$$FormatName(Name1,1) ;"should convert to standard format.;
	SET Name2=$$FormatName(Name2,1)
	;
	DO STDNAME^XLFNAME(.Name1,"C",.TMGMsg)
	DO STDNAME^XLFNAME(.Name1,"C",.TMGMsg) ;"Doing a second time will ensure Array not in initial format.;
	;
	DO STDNAME^XLFNAME(.Name2,"C",.TMGMsg)
	DO STDNAME^XLFNAME(.Name2,"C",.TMGMsg) ;"Doing a second time will ensure Array not in initial format.;
	;
	IF Name1=Name2 SET result=1 GOTO CompNDone
	IF Name1("FAMILY")'=Name2("FAMILY") DO  GOTO:(result=0) CompNDone
	. IF $$EN^XUA4A71(Name1("FAMILY"))'=$$EN^XUA4A71(Name2("FAMILY")) SET result=0  ;"check soundex equality
	;
	IF Name1("GIVEN")'=Name2("GIVEN") DO
	. IF $$EN^XUA4A71(Name1("GIVEN"))=$$EN^XUA4A71(Name2("GIVEN")) QUIT   ;"check soundex equality
	. NEW n1,n2
	. SET n1=Name1("GIVEN")
	. SET n2=Name2("GIVEN")
	. IF $LENGTH(n2)<$LENGTH(n1) DO   ;"ensure length n2>n1
	. . NEW temp SET temp=n2
	. . SET n2=n1,n1=temp
	. IF $EXTRACT(n2,1,$LENGTH(n1))=n1 SET result=0.5
	. ELSE  SET result=0
	IF result=0 GOTO CompNDone
	;
	IF Name1("MIDDLE")'=Name2("MIDDLE") DO
	. IF $$EN^XUA4A71(Name1("MIDDLE"))=$$EN^XUA4A71(Name2("MIDDLE")) QUIT   ;"check soundex equality
	. NEW n1,n2
	. SET n1=Name1("MIDDLE")
	. SET n2=Name2("MIDDLE")
	. IF $LENGTH(n2)<$LENGTH(n1) DO   ;"ensure length n2>n1
	. . NEW temp SET temp=n2
	. . SET n2=n1,n1=temp
	. IF $EXTRACT(n2,1,$LENGTH(n1))=n1 SET result=0.5
	. ELSE  SET result=0
	IF result=0 GOTO CompNDone
	;
CompNDone
	QUIT result
	;
FRMTNAME(NAME,CUTTITLE) ;
	QUIT $$FormatName(.NAME,.CUTTITLE)
	;
FormatName(Name,CutTitle)
	;"Purpose:  To ensure patient name is properly formated.;
	;"  i.e. John G. Doe --> DOE,JOHN G
	;"       John G. Doe III --> DOE,JOHN G III
	;"       John G. Doe,III --> DOE,JOHN G III
	;"     Doe,  John G --> DOE,JOHN G
	;"       Doe,John g.,III,  phd  --> DOE,JOHN G III PHD
	;"Input: Name -- the name to be reformated
	;"  CutTitle -- OPTIONAL -- IF 1, then titles (e.g. MD, PhD etc) will be cut
	;"Results: returns properly formated name
	;"Note: If Name is passed by reference, it will be changed
	;"  Also, NO lookup is done in database to ensure name exists
	;
	;"Note: this function malfunctioned on a patient with name like this:
	;"      JOHN A VAN DER BON --> BON,JOHN A VAN DER (should be VAN DER BON,JOHN A)
	;"      I don't have a quick for this right now...;
	;"Also, Sue St. Clair --> CLAIR,SUE ST  this is also wrong.;
	;
	;"FYI: DO STDNAME^XLFNAME(.NAME,FLAGS,.ERRARRAY) can also DO standardization,
	;"      and also parse to component parts.  It specifically address the St. Clair issue.;
	;
	NEW NameArray
	NEW MaxNode
	NEW Suffix SET Suffix=""
	NEW i,s,lname
	NEW fname SET fname=""
	NEW result SET result=""
	IF $DATA(Name)#10=0 GOTO FormatNDone
	;
	IF $GET(TMGDEBUG)>0 DO DEBUGMSG^TMGDEBU4(.TMGDBINDENT,"Person's name initially is: '",Name,"'")
	SET Name=$TRANSLATE(Name,"*.","")  ;"cleans off any *'s or .'s from initials etc.;
	IF Name[", " DO
	. NEW s1,s2
	. SET s1=$PIECE(Name,", ",1)
	. SET s2=$PIECE(Name,", ",2)
	. IF $$IsTitle($$UP^XLFSTR(s2))&($GET(CutTitle)=1) DO
	. . SET Name=s1
	. ELSE  DO
	. . SET Name=s1_","_s2
	. ;"SET Name=$TRANSLATE(Name,", ",",") ;"Convert 'Doe, John'  into 'Doe,John'
	SET Name=$$UP^XLFSTR(Name)  ;"convert to upper case
	IF $GET(TMGDEBUG)>0 DO DEBUGMSG^TMGDEBU4(.TMGDBINDENT,"After translations, name is: '",Name,"'")
	SET result=$$FORMAT^TMGDPTN1(Name,3,30) ;"Convert to 'internal' format
	IF $GET(TMGDEBUG)>0 DO DEBUGMSG^TMGDEBU4(.TMGDBINDENT,"After $$FORMAT^DPTNAME, name is: '",result,"'")
	;
	;"Now, test IF FORMAT^DPTNAME caused empty name, i.e.;
	;"   John G Doe --> ""  (it wanted Doe,John G)
	SET lname=$PIECE(result,",",2)
	IF $$IsTitle(lname)&($GET(CutTitle)=1) DO     ;"trim off title IF not wanted.;
	. SET result=$PIECE(result,",",1)
	. SET lname=""
	IF $$IsSuffix(lname)=1 DO
	. ;"Here we have 'JOHN DOE,III' --> must be changed to 'DOE,JOHN III'
	. SET Name=$TRANSLATE(Name,","," ") ;"First change 'JOHN DOE,III' --> 'JOHN DOE III'
	. SET result=""  ;"signal need to rearrange letters.;
	IF (result="")&(Name'[",") DO
	. SET s=Name
	. DO CleaveToArray^TMGSTUTL(s," ",.NameArray,1)
	. SET MaxNode=+$GET(NameArray("MAXNODE"))
	. IF MaxNode=0 QUIT
	. IF $GET(CutTitle)=1 DO
	. . IF $$IsTitle(NameArray(MaxNode)) DO
	. . . KILL NameArray(MaxNode)
	. . . SET MaxNode=MaxNode-1
	. . . SET NameArray("MAXNODE")=MaxNode
	. SET lname=NameArray(MaxNode)
	. IF ($$IsSuffix(lname)=1)!($$IsTitle(lname)) DO
	. . ;"Change JOHN G DOE III --> JOHN G III DOE (order change in array)
	. . SET lname=NameArray(MaxNode-1)  ;"i.e. DOE
	. . SET Suffix=NameArray(MaxNode)   ;"i.e. III
	. . SET NameArray(MaxNode)=lname
	. . SET NameArray(MaxNode-1)=Suffix
	. SET result=lname_","
	. FOR i=1:1:MaxNode-1 DO
	. . SET result=result_NameArray(i)_" "
	;
	;"convert potential 'DOE,JOHN G,III, PHD' --> 'DOE,JOHN G III PHD'
	SET lname=$PIECE(result,",",1)
	SET fname=$PIECE(result,",",2,99)
	SET fname=$TRANSLATE(fname,","," ")
	SET result=lname_","_fname
	;
	SET result=$$Trim^TMGSTUTL(result)
	;
	;"One last run through, after all custom alterations made.;
	;"convert potential 'DOE,JOHN G III    PHD' --> 'DOE,JOHN G III PHD'
	SET result=$$FORMAT^TMGDPTN1(result,3,30) ;"Convert to 'internal' format
	;
FormatNDone
	QUIT result
	;
IsSuffix(s)
	;"Purpose: to return whether s is a suffix (i.e. I,II,Jr.,Sr. etc.)
	;"Input: s : the string to check
	;"Result 0 IF NOT a suffix, 1 IF IS a suffix.;
	;
	NEW result SET result=0
	;
	IF (s="I")!(s="II")!(s="III")!(s="JR")!(s="SR") SET result=1
	;
	QUIT result
	;
IsTitle(s)
	;"Purpose: to return whether s is a title (i.e. MD,PHD,JD,DDS etc.)
	;"Input: s : the string to check
	;"Result 0 IF NOT a suffix, 1 IF IS a suffix.;
	;
	NEW result SET result=0
	;
	IF (s="MD")!(s="PHD")!(s="JD")!(s="DDS") SET result=1
	IF (s="FNP")!(s="GNP")!(s="NP")!(s="PA") SET result=1
	IF (s="RN")!(s="LPN") SET result=1
	;
	QUIT result
	;
HEXCHR(V) ;
	;"Scope: PUBLIC
	;"Take one BYTE and return HEX Values
	;"(from Chris Richardson -- thanks!)
	NEW NV,B1,B2
	SET NV="0123456789ABCDEF"
	SET B1=(V#16)+1  ; "0 to 15 becomes 1 to 16
	SET B2=(V\16)+1
	QUIT $E(NV,B2)_$E(NV,B1)
	;
HEXCHR2(n,digits)
	;"SCOPE: PUBLIC
	;"Purpose: convert n to hex characters
	;"Input: n -- the number to convert
	;"   digits: (optional) number of digits in output.  Leading 0's padded to
	;"          front of answer to SET number of digits.;
	;"          e.g. IF answer is "A", then
	;"          2 -> mandates at least 2 digits ("0A")
	;"          3->3 digits ("00A")
	;"Note: This function is not as fast as HEXCHR(V)
	;
	NEW lo,ch
	NEW result SET result=""
	SET digits=$GET(digits,1)
	;
	FOR  DO  QUIT:(n=0)
	. SET lo=n#16
	. IF (lo<10) SET ch=+lo
	. ELSE  SET ch=$CHAR(55+lo)
	. SET result=ch_result
	. SET n=n\16
	;
	IF $LENGTH(result)<digits DO
	. NEW i FOR i=1:1:digits-$LENGTH(result) DO
	. . SET result="0"_result
	QUIT result
	;
HEX2DEC(HEX) ;"Hexidecimal to decimal number
	;"INPUT: HEX -- string containing hexidecimal numbers.  Can be upper or lower case. Optional "$" prefix
	;"Result: Decimal equivalent number.  If invalid input, then -1
	SET HEX=$$UP^XLFSTR($GET(HEX))
	IF $EXTRACT(HEX,1)="$" SET HEX=$PIECE(HEX,"$",2)
	NEW RESULT SET RESULT=0
	NEW L SET L=$LENGTH(HEX)
	NEW STPOS,DIGITPOS
	FOR STPOS=L:-1:1 DO  QUIT:RESULT<0
	. SET DIGITPOS=L-STPOS
	. NEW DIGIT SET DIGIT=$EXTRACT(HEX,STPOS)
	. NEW VAL SET VAL=$FIND("0123456789ABCDEF",DIGIT)-2
	. IF VAL<0 SET RESULT=-1 QUIT
	. SET RESULT=RESULT+(VAL*(16**DIGITPOS))
	QUIT RESULT
	;
DEC2BIN(INT,DIGITS) ;"INTEGER TO BINARY STRING
	;"Purpose: Return a string representing input INT.;
	;"Input:  INT -- integer to convert into binary string.  NOTE: any decimals are dropped.;
	;"  DIGITS -- OPTIONAL.  If provided, and if length of conversion
	;"      would be less than digits, then 0's are prepended.;
	;
	SET INT=$GET(INT,0)\1  ;" trimm off decimals
	SET DIGITS=+$GET(DIGITS)
	NEW RESULT SET RESULT=""
	NEW REMAINDER
	FOR  DO  QUIT:(INT=0)
	. SET REMAINDER=INT#2
	. SET RESULT=REMAINDER_RESULT
	. SET INT=INT\2
	NEW L SET L=$LENGTH(RESULT)
	NEW IDX FOR  QUIT:$LENGTH(RESULT)'<DIGITS  SET RESULT="0"_RESULT
	QUIT RESULT
	;
BIN2DEC(BINSTR) ;"BINARY STRING TO DECIMAL
	;"Purpose: Return decimal value for binary string
	;"Input: BINSTR -- string of 0's and 1's, with least signficant digit on left
	;"NOTE: If BINSTR contains chars other than 0 or 1, then result of 0 is returned
	;
	NEW RESULT SET RESULT=0
	NEW BINDIGIT
	NEW L SET L=$LENGTH(BINSTR)
	NEW ABORT SET ABORT=0
	NEW POS FOR POS=L:-1:1 DO  QUIT:ABORT
	. SET BINDIGIT=L-POS
	. NEW DIGIT SET DIGIT=$EXTRACT(BINSTR,POS)
	. IF "01"'[DIGIT SET ABORT=1 QUIT
	. SET RESULT=RESULT+(DIGIT*(2**BINDIGIT))
	QUIT RESULT
	;
BIN2HEX(BINSTR) ;"BINARY STRING TO HEX
	;"Purpose: Return hex string value for binary string
	;"Input: BINSTR -- string of 0's and 1's, with least signficant digit on left
	;"NOTE: If BINSTR contains chars other than 0 or 1, then result of 0 is returned
	;
	NEW RESULT SET RESULT=""
	NEW FRAG,L,ST SET ST=$GET(BINSTR)
	FOR  DO  QUIT:ST=""
	. SET L=$LENGTH(ST)
	. IF L>4 DO
	. . SET FRAG=$EXTRACT(ST,L-3,L)
	. . SET ST=$EXTRACT(ST,1,L-4)
	. ELSE  DO
	. . SET FRAG=ST
	. . SET ST=""
	. NEW VAL SET VAL=$$BIN2DEC(FRAG)
	. NEW HEX SET HEX=$EXTRACT("0123456789ABCDEF",VAL+1)
	. SET RESULT=HEX_RESULT
	QUIT RESULT
	;
HEX2BIN(HEXSTR,DIGITS) ;"HEX STRING TO BINARY STRING
	;"Input: HEXSTR -- hexadecimal string.  May have optional `$` at beginning
	;"  DIGITS -- OPTIONAL.  If provided, and if length of conversion
	;"      would be less than digits, then 0's are prepended.;
	;"         If length of result > digits, then any leading 0's will be trimmed to match if possible
	SET HEXSTR=$$UP^XLFSTR($GET(HEXSTR))
	IF $EXTRACT(HEXSTR,1)="$" SET HEXSTR=$PIECE(HEXSTR,"$",2)
	NEW ABORT,VAL,DIGIT,RESULT SET RESULT="",ABORT=0
	NEW P FOR P=1:1:$LENGTH(HEXSTR) DO  QUIT:ABORT
	. SET DIGIT=$EXTRACT(HEXSTR,P)
	. SET VAL=$FIND("0123456789ABCDEF",DIGIT)-2
	. IF VAL<0 SET ABORT=1,RESULT="" QUIT
	. SET RESULT=RESULT_$$DEC2BIN(VAL,4)
	NEW L SET L=$LENGTH(RESULT)
	NEW DIGITS SET DIGITS=+$GET(DIGITS)
	IF L<DIGITS SET RESULT=$$RJ^XLFSTR(RESULT,DIGITS,"0") GOTO H2BDN
	IF L=DIGITS GOTO H2BDN
	IF (DIGITS=0)!(L>DIGITS) DO
	. NEW DONE SET DONE=0
	. FOR  DO  QUIT:DONE
	. . NEW CH SET CH=$EXTRACT(RESULT,1)
	. . IF CH'="0" SET DONE=1 QUIT
	. . SET RESULT=$EXTRACT(RESULT,2,$LENGTH(RESULT))
H2BDN ;
	QUIT RESULT
	;
GETUTF8(CODEPT,OPTION) ;"GET BYTE SEQUENCE FOR UNICODE CODEPOINT
	;"Input: CODEPT -- This is the codepoint (i.e. unicode character number) for input. DECIMAL
	;"        Optionally, CODEPT may hold HEX number if prefixed with "$", e.g. "$20AC"
	;"       OPTION -- OPTIONAL.;
	;"    OPTION("HEX")=1, DEFAULT IS 0.  If 1, then HEX values returned
	;"    OPTION("HEX","PREFIX")='<prefix string>'  DEFAULT IS "$"
	;"Output: Returns 1-4 bytes as delimited string, e.g. "226,148,140", with
	;   sequence-to-be sent being left-to-right
	;"NOTE: if CODEPT>1114111 ($10FFFF) then "" is returned
	NEW RESULT SET RESULT=""
	SET CODEPT=$GET(CODEPT)
	IF $EXTRACT(CODEPT,1)="$" SET CODEPT=$$HEX2DEC(CODEPT)
	SET CODEPT=+CODEPT
	IF CODEPT<128 DO  GOTO GUDN   ;"0-127 --> single byte encoding
	. SET RESULT=$$DEC2BIN(CODEPT,8)
	IF CODEPT<2048 DO  GOTO GUDN  ;"128-2047 --> 2 byte encoding, storing 11 bits
	. NEW BINCODE SET BINCODE=$$DEC2BIN(CODEPT,11)
	. NEW BYTE1,BYTE2
	. SET BYTE1="110"_$$BINLTRIM(.BINCODE,5) ;"5 bits of codepoint put into byte #1
	. SET BYTE2="10"_$$BINLTRIM(.BINCODE,6)  ;"6 bits of codepoint put into byte #2
	. SET RESULT=BYTE1_","_BYTE2
	IF CODEPT<65536 DO  GOTO GUDN  ;"2048-65535 --> 3 byte encoding, storing 16 bits
	. NEW BINCODE SET BINCODE=$$DEC2BIN(CODEPT,16)
	. NEW BYTE1,BYTE2,BYTE3
	. SET BYTE1="1110"_$$BINLTRIM(.BINCODE,4) ;"4 bits of codepoint put into byte #1
	. SET BYTE2="10"_$$BINLTRIM(.BINCODE,6)   ;"6 bits of codepoint put into byte #2
	. SET BYTE3="10"_$$BINLTRIM(.BINCODE,6)   ;"6 bits of codepoint put into byte #3
	. SET RESULT=BYTE1_","_BYTE2_","_BYTE3
	IF CODEPT<1114112 DO  GOTO GUDN  ;"65536-1114111 --> 4 byte encoding, storing 21 bits
	. NEW BINCODE SET BINCODE=$$DEC2BIN(CODEPT,21)
	. NEW BYTE1,BYTE2,BYTE3,BYTE4
	. SET BYTE1="11110"_$$BINLTRIM(.BINCODE,3) ;"3 bits of codepoint put into byte #1
	. SET BYTE2="10"_$$BINLTRIM(.BINCODE,6)    ;"6 bits of codepoint put into byte #2
	. SET BYTE3="10"_$$BINLTRIM(.BINCODE,6)    ;"6 bits of codepoint put into byte #3
	. SET BYTE4="10"_$$BINLTRIM(.BINCODE,6)    ;"6 bits of codepoint put into byte #4
	. SET RESULT=BYTE1_","_BYTE2_","_BYTE3_","_BYTE4
GUDN ;
	SET RETURNHEX=+$GET(OPTION("HEX"))
	NEW HEXPREFIX IF RETURNHEX SET HEXPREFIX=$GET(OPTION("HEX","PREFIX"),"$")
	NEW IDX FOR IDX=1:1:$LENGTH(RESULT,",") DO
	. NEW VAL SET VAL=$PIECE(RESULT,",",IDX) QUIT:VAL=""
	. IF RETURNHEX DO
	. . SET VAL=HEXPREFIX_$$BIN2HEX(VAL)
	. ELSE  DO
	. . SET VAL=$$BIN2DEC(VAL)
	. SET $PIECE(RESULT,",",IDX)=VAL
	QUIT RESULT
	;
BINLTRIM(BINSTR,DIGITS) ;"Left trim certain number of bits
	;"INPUT: BINSTR -- PASS BY REFERENCE.  Binary string.  Assumed to be properly formed
	;"       DIGITS -- number of digits to remove from LEFT of string, leaving BINSTR shorter
	;"NOTE: If DIGITS is > length of BINSTR, then return will be as many as possible.;
	NEW RESULT SET RESULT=$EXTRACT(BINSTR,1,DIGITS)
	SET BINSTR=$EXTRACT(BINSTR,DIGITS+1,$LENGTH(BINSTR))
	QUIT RESULT
	;
OR(A,B) ;
	;"Scope: PUBLIC
	;"Purpose: to perform a bitwise OR on operands a and b
	;"Input:  A,B - integer values (not binary strings)
	NEW RESULT SET RESULT=0
	NEW MULT SET MULT=1
	FOR  DO  QUIT:(A'>0)&(B'>0)
	. SET RESULT=RESULT+(((A#2)!(B#2))*MULT)
	. SET A=A\2,B=B\2,MULT=MULT*2
	QUIT RESULT
	;
ParsePos(pos,label,offSET,routine,dmod)
	;"Purpose: to convert a pos string (e.g. X+2^ROUTINE$DMOD) into componant parts
	;"Input: pos -- the string, as example above
	;"   label -- OUT PARAM, PASS BY REF, would return "x"
	;"   offSET  -- OUT PARAM, PASS BY REF, would return "+2"
	;"   routine -- OUT PARAM, PASS BY REF, would return "ROUTINE"
	;"   dmod -- OUT PARAM, PASS BY REF, would return "DMOD"
	;"Results: none
	;"Note: results are shortened to 8 characters.;
	;
	NEW s
	SET s=$GET(pos)
	SET dmod=$PIECE(s,"$",1) ;"e.g. X+2^ROUTINE$DMOD-->X+2^ROUTINE
	SET routine=$PIECE(s,"^",2)
	;"SET routine=$EXTRACT(routine,1,8)   //kt removed 3/1/08, allow length over 8 chars
	SET label=$PIECE(s,"^",1)
	SET offSET=$PIECE(label,"+",2)
	SET label=$PIECE(label,"+",1)
	;"SET label=$EXTRACT(label,1,8)   //kt removed 3/1/08, allow length over 8 chars
	QUIT
	;
ScanMod(Module,pArray)  ;
	;"Purpose: To scan a module and find all the labels/entry points/Entry points
	;"Input: Module -- The name of the module, like "XGF" (not "XGF.m" or "^XGF")
	;"   pArray -- pointer to (NAME OF) array Will be filled like this
	;"        pArray(1,"TAG")="Label1"
	;"        pArray(1,"OFFSET")=1
	;"        pArray(2,"TAG")="Label2"
	;"        pArray(2,"OFFSET")=9
	;"        pArray(3,"TAG")="Label3"  etc.;
	;"        pArray(3,"OFFSET")=15
	;"        pArray("Label1")=1
	;"        pArray("Label2")=2
	;"        pArray("Label3")=3
	;"
	;"        NOTE: there seems to be a problem IF the passed pArray value is "pArray",
	;"          so use another name.;
	;"
	;"Output: Results are put into array
	;"Result: none
	NEW smIdx SET smIdx=1
	NEW LabelNum SET LabelNum=0
	NEW smLine SET smLine=""
	IF $GET(Module)="" GOTO SMDone
	;"look for a var with global scope to see how how many characters are significant to GT.M
	IF $GET(tmgZBSigNameLen)="" DO
	. SET tmgZBSigNameLen=$$NumSigChs^TMGMISC()
	;
	NEW $ETRAP SET $ETRAP="write !,""Error Trapped in ScanMod^TMGMISC"",! SET $ETRAP="""",$ECODE="""",ABORT=1"
	NEW ABORT SET ABORT=0
	FOR  DO  QUIT:(smLine="")!(ABORT)
	. NEW smCh
	. SET smLine=$text(+smIdx^@Module)
	. IF smLine="" QUIT
	. SET smLine=$$REPLSTR^TMGSTUT3(smLine,$Char(9),"  ") ;"replace tabs for 8 spaces
	. SET smCh=$EXTRACT(smLine,1)
	. IF (smCh'=" ")&(smCh'=";") DO
	. . NEW label
	. . SET label=$PIECE(smLine," ",1)
	. . SET label=$PIECE(label,"(",1)  ;"MyFunct(X,Y) --> MyFunct
	. . SET label=$EXTRACT(label,1,tmgZBSigNameLen)
	. . SET LabelNum=LabelNum+1
	. . SET @pArray@(LabelNum,"TAG")=label
	. . SET @pArray@(LabelNum,"OFFSET")=smIdx
	. . SET @pArray@(label)=LabelNum
	. SET smIdx=smIdx+1
	;
SMDone  ;
	QUIT
	;
RelConvertPos(Pos,ViewOffset,pArray)   ;
	;"Purpose: to convert a positioning line from one that is relative to
	;"        the start of the file to one that is relative to the
	;"        last tag/label
	;"        e.g. +32^MYFUNCT --> START+8^MYFUNCT
	;"    I.e. this function in the OPPOSITE of ConvertPos
	;"Input: Pos -- a position, as returned from $ZPOS
	;"       ViewOffset -- the offset from the Pos to get pos for
	;"       pArray -- pointer to (name of).  Array holding  holding tag offsets
	;"       see Description in ConvertPos()
	;"Result: returns the NEW position line, relative to the start of the last tag/label
	;
	;"WRITE !,"Here in RelConvertPos.  Pos=",Pos," ViewOffset=",ViewOffset,!
	NEW zbRelPos,zbLabel,zbOffset,zbRoutine
	DO ParsePos^TMGMISC(Pos,.zbLabel,.zbOffset,.zbRoutine)
	SET zbRelPos=zbLabel_"+"_+(zbOffset+ViewOffset)_"^"_zbRoutine
	NEW zbTemp SET zbTemp=zbRelPos
	;"5/27/07 I don't know why following line was here. Removing.;
	;"It was breaking the setting of breakpoints.  I wonder if I have now
	;"broken conditional breakpoints...  Figure that out later...;
	;"SET zbRelPos=$$ConvertPos^TMGMISC(zbRelPos,pArray)
	IF zbRelPos="" DO
	. WRITE "Before ConvertPos, zbRelPos=",zbTemp,!
	. WRITE "Afterwards, zbRelPos=""""",!
	;"WRITE "Done RelConvertPos.  Result=",zbRelPos,!
	QUIT zbRelPos
	;
CONVERTPOS(POS,PARRAY) ;"Uppercase case wrapper.;
	QUIT $$ConvertPos(.POS,.PARRAY) ;h
	;
ConvertPos(Pos,pArray)
	;"Purpose: to convert a text positioning line from one that is relative to the last tag/label, into
	;"        one that is relative to the start of the file
	;"        e.g. START+8^MYFUNCT --> +32^MYFUNCT
	;"Input: Pos -- a position, as returned from $ZPOS
	;"  pArray -- pointer to (name of).  Array holding  holding tag offSETs
	;"        pArray will be in this format:
	;"        pArray("ModuleA",1,"TAG")="ALabel1"
	;"        pArray("ModuleA",1,"OFFSET")=1
	;"        pArray("ModuleA",2,"TAG")="ALabel2"
	;"        pArray("ModuleA",2,"OFFSET")=9
	;"        pArray("ModuleA","Label1")=1
	;"        pArray("ModuleA","Label2")=2
	;"        pArray("ModuleA","Label3")=3
	;"        pArray("ModuleB",1,"TAG")="BLabel1"
	;"        pArray("ModuleB",1,"OFFSET")=4
	;"        pArray("ModuleB",2,"TAG")="BLabel2"
	;"        pArray("ModuleB",2,"OFFSET")=23
	;"        pArray("ModuleB","Label1")=1
	;"        pArray("ModuleB","Label2")=2
	;"        pArray("ModuleB","Label3")=3
	;"      NOTE: -- IF array passed is empty, then this function will call ScanModule to fill it
	;"Result: returns the NEW position line, relative to the start of the file/module
	;"
	NEW cpS
	NEW cpResult SET cpResult=""
	NEW cpRoutine,cpLabel,cpOffSET
	;
	SET cpS=$PIECE(Pos,"$",1)  ;"e.g. X+2^ROUTINE$DMOD-->X+2^ROUTINE
	IF cpS="" DO  GOTO CPDone
	. WRITE "Parse error: Nothing before $ in",cpS,!
	;
	SET cpRoutine=$PIECE(cpS,"^",2)
	IF cpRoutine="" DO  GOTO CPDone
	. WRITE "Parse error:  No routine specified in: ",cpS,!
	;
	SET cpS=$PIECE(cpS,"^",1)
	SET cpOffSET=+$PIECE(cpS,"+",2)
	;"if cpOffSET="" SET cpOffSET=1
	;"ELSE  SET cpOffSET=+cpOffSET
	SET cpLabel=$PIECE(cpS,"+",1)
	IF cpLabel="" set cpLabel="+1"  ;"//kt 6/8/16
	IF $DATA(@pArray@(cpRoutine))=0 DO
	. NEW p2Array SET p2Array=$name(@pArray@(cpRoutine))
	. DO ScanMod(cpRoutine,p2Array)
	;
	NEW cpIdx SET cpIdx=+$GET(@pArray@(cpRoutine,cpLabel))
	IF cpIdx=0 DO  GOTO CPDone
	. ;"WRITE "Parse error: Can't find ",cpRoutine,",",cpLabel," in stored source code.",!
	NEW cpGOffSET SET cpGOffSET=@pArray@(cpRoutine,cpIdx,"OFFSET")
	SET cpResult="+"_+(cpGOffSET+cpOffSET)_"^"_cpRoutine
	;
CPDone  ;
	QUIT cpResult
	;
COMPARRAY(PARR1,PARR2)  ;"upper case wrapper for CompArray(pArray1,pArray2)
	QUIT $$CompArray(PARR1,PARR2)
	;
CompArray(pArray1,pArray2)  ;"Are two arrays identical?
	;"Purpose: To return IF two arrays are identical
	;"      Equality means that all nodes and values are present and equal
	;"Input: Array1 -- PASS BY NAME.  The *name of* the first array to be compared
	;"       Array1 -- PASS BY NAME.  The *name of* the second array to be compared
	;"Output: 1 if two are identical, 0 if not
	NEW result SET result=1
	NEW index1,index2
	SET index1=$ORDER(@pArray1@(""))
	SET index2=$ORDER(@pArray2@(""))
	IF (index1="")!(index2="") SET result=0 GOTO CADone
	FOR  DO  QUIT:(result=0)!(index1="")!(index2="")
	. IF index1'=index2 SET result=0 QUIT
	. IF $GET(@pArray1@(index1))'=$GET(@pArray2@(index2)) SET result=0 QUIT
	. IF ($DATA(@pArray1@(index1))'<10)!($DATA(@pArray2@(index2))'<10) DO
	. . SET result=$$CompArray($name(@pArray1@(index1)),$name(@pArray2@(index2)))
	. SET index1=$ORDER(@pArray1@(index1))
	. SET index2=$ORDER(@pArray2@(index2))
	. IF index1'=index2 SET result=0 QUIT  ;"//kt 11/14/24
CADone QUIT result
	;
	;
IterTemplate(Template,Prior)
	;"Purpose: To iterate through a SORT TEMPLATE (i.e. provide record numbers held in the template
	;"    one at a time.  For each time this function is called, one record number (IEN) is returned.;
	;"Input: Template:  the IEN of an entry from file SORT TEMPLATE (file# .401)
	;"       Prior -- OPTIONAL (default is to return first record), an IEN as returned from this
	;"          function during the last call.;
	;"Result: Returns the next record found in list, occuring after Prior, or -1 IF error or not found
	;"  Returns "" IF end of list (no next record)
	;
	;"Example of use:  This will list all records held in SORT TEMPLATE record# 809
	;"  SET IEN=""
	;"  FOR  s IEN=$$IterTemplate^TMGMISC(809,IEN) w IEN,! q:(+IEN'>0)
	;
	SET Prior=$GET(Prior)
	SET result=-1
	IF +$GET(Template)'>0 GOTO ItTDone
	;
	SET result=$ORDER(^DIBT(Template,1,Prior))
	;
ItTDone QUIT result
	;
CtTemplate(Template)
	;"Purpose: To return the Count of IEN's stored in a SORT TEMPLATE
	;"Input: Template:  the IEN of an entry from file SORT TEMPLATE (file# .401)
	;"Result: Returns the count of records held
	;
	NEW name SET name=$name(^DIBT(Template,1))
	QUIT $$ListCt(name)
	;
NumPieces(s,delim,maxPoss)
	;"Purpose: to return the number of pieces in s, using delim as a delimiter
	;"Input: s -- the string to test
	;"       delim -- OPTIONAL -- the delimiter (e.g. ',' or ';' or ' ' etc), default=" "
	;"       maxPoss -- OPTIONAL the maximum number of possible pieces, default=32
	;"        the function counts DOWN from this number, so IF s has more than default, must specify
	;"Result: Returns the number of pieces
	;"        e.g. 'this is a test', space delimiter --> returns 4
	;"Note:  ("this is a test",";") --> 1
	;"       ("",";") --> 0
	;
	;"NOTICE!!!
	;"After writing this function, I was told that $LENGTH(s,delim) will DO this.;
	;" I will leave this here as a reminder, but it probably shouldn't be used....;
	QUIT $LENGTH(s,$GET(delim," "))
	;
	NEW i,result SET result=0
	IF $GET(s)="" GOTO NPsDone
	SET delim=$GET(delim," ")
	SET maxPoss=+$GET(maxPoss,32)
	;
	FOR result=maxPoss:-1:1 QUIT:($PIECE(s,delim,result)'="")
	;
	QUIT result
	;
LastPiece(s,delim,maxPoss)
	;"Purpose: to return the last piece of a string
	;"Input: s -- the string to use
	;"       delim -- OPTIONAL -- the delimiter (e.g. ',' or ';' or ' ' etc), default=" "
	;"       maxPoss -- OPTIONAL the maximum number of possible pieces, default=32 (see NumPieces function)
	;"Results : returns the LAST piece in the string
	;
	NEW result SET result=""
	IF $GET(s)="" GOTO LPDone
	SET delim=$GET(delim," ")
	NEW n
	SET n=$LENGTH(s,delim)
	SET result=$PIECE(s,delim,n)
	;
LPDone
	QUIT result
	;
ParseLast(s,remainS,delim,maxPoss)
	;"Purpose: to return the last piece of a string, AND return the first part of the string in remainS
	;"Input: s -- the string to use
	;"       remainS -- an OUT parameter.  PASS BY REFERENCE.  Returns the part of the string up to result
	;"       delim -- OPTIONAL -- the delimiter (e.g. ',' or ';' or ' ' etc), default=" "
	;"       maxPoss -- OPTIONAL the maximum number of possible pieces, default=32 (see NumPieces function)
	;"Results : returns the LAST piece in the string
	;
	NEW result SET result=""
	NEW tempS SET tempS=s  ;"in case s passed by reference, and remainS=s (i.e. w $$ParseLast(s,.s)
	SET remainS=""
	SET delim=$GET(delim," ")
	;
	IF $GET(tempS)="" GOTO PLDone
	NEW n
	SET n=$LENGTH(s,delim)
	SET result=$PIECE(tempS,delim,n)
	IF n>1 SET remainS=$PIECE(tempS,delim,1,n-1)
	;
PLDone
	QUIT result
	;
NPsDone
	QUIT result
	;
Trim1Node(pRef)
	;"Purpose: To shorten a reference by one node.;
	;"   e.g. "Array(567,2342,123)" --> "Array(567,2342)"
	;"Input: pRef -- the NAME OF an array.;
	;"Result: will return shortened reference, or "" IF problem
	;"  If no nodes to trim, just array name will be returnes.;
	;
	NEW result SET result=pRef
	IF pRef="" GOTO T1NDone
	;
	IF $qlength(pRef)>0 SET result=$name(@pRef,$qlength(pRef)-1)
	GOTO T1NDone
	;
	;"Below is an old way I came up with (not as effecient!)
	;"NOT USED.;
	SET result=$qsubscript(pRef,0)
	;
	NEW numNodes,i
	SET numNodes=$qlength(pRef)
	FOR i=1:1:(numNodes-1) DO
	. NEW node SET node=$qsubscript(pRef,i)
	. SET result=$name(@result@(node))
	;
T1NDone
	QUIT result
	;
BROWSEASK
	;"Purpose: to ask user for the name of an array, then display nodes
	;
	NEW current
	NEW order SET order=1 ;"default = forward display.;
	NEW paginate SET paginate=0 ;"no pagination
	NEW countNodes SET countNodes=0 ;"no counting
	WRITE !
	read "Enter name of array (or File number) to display nodes in: ",current:$GET(DTIME,3600),!
	IF +current=current DO
	. SET current=$GET(^DIC(+current,0,"GL"))
	. IF current="" WRITE "File number not found. Quitting.",! QUIT
	. WRITE "Browsing array: ",current,!
	IF current="" SET current="^"
	IF current="^" GOTO BADone
	;
	NEW % SET %=2 ;" default= NO
	WRITE "Display in REVERSE order? "
	DO YN^DICN WRITE !
	IF %=1 SET order=-1
	IF %=-1 GOTO BADone
	;
	SET %=2
	WRITE "Pause after each page? "
	DO YN^DICN WRITE !
	IF %=1 SET paginate=1
	IF %=-1 GOTO BADone
	;
	SET %=2
	WRITE "Show number of subnodes? "
	DO YN^DICN WRITE !
	IF %=1 SET countNodes=1
	IF %=-1 GOTO BADone
	;
	DO BROWSENODES(current,order,paginate,countNodes)
BADone
	QUIT
	;
BROWSENODES(current,Order,paginate,countNodes)
	;"Purpose: to display nodes of specified array
	;"Input: Current -- The reference to display
	;"       order -- OPTIONAL, default=1; 1 for forward, -1 for backwards order
	;"       paginate -- OPTIONAL, default=0;  0=no pagination, 1=pause after each page
	;"       countNodes -- OPTIONAL, default=0; 1=show number of child nodes.;
	;
	NEW parent,child
	SET parent=""
	SET order=$GET(order,1)
	SET paginate=$GET(paginate,0)
	SET countNodes=$GET(countNodes,0)
	;
	NEW len SET len=$LENGTH(current)
	NEW lastChar SET lastChar=$EXTRACT(current,len)
	IF lastChar'=")" DO
	. IF current'["(" QUIT
	. IF lastChar="," SET current=$EXTRACT(current,1,len-1)
	. IF lastChar="(" SET current=$EXTRACT(current,1,len-1) QUIT
	. SET current=current_")"
	;
BNLoop
	IF current="" GOTO BNDone
	SET child=$$ShowNodes(current,order,paginate,countNodes)
	IF child'="" DO
	. SET parent(child)=current
	. SET current=child
	ELSE  SET current=$GET(parent(current))
	GOTO BNLoop
BNDone
	QUIT
	;
ShowNodes(pArray,order,paginate,countNodes)
	;"Purpose: To display all the nodes of the given array
	;"Input: pArray -- NAME OF array to display
	;"       order -- OPTIONAL, default=1; 1 for forward, -1 for backwards order
	;"       paginate -- OPTIONAL, default=0;  0=no pagination, 1=pause after each page
	;"       countNodes -- OPTIONAL, default=0; 1=show number of child nodes.;
	;"Results: returns NAME OF next node to display (or "" IF none)
	;
	NEW TMGi
	NEW count SET count=1
	NEW Answers
	NEW someShown SET someShown=0
	NEW abort SET abort=0
	SET paginate=$GET(paginate,0)
	NEW pageCount SET pageCount=0
	NEW pageLen SET pageLen=20
	SET countNodes=$GET(countNodes,0)
	;
	WRITE pArray,!
	SET TMGi=$ORDER(@pArray@(""),order)
	IF TMGi'="" FOR  DO  QUIT:(TMGi="")!(abort=1)
	. WRITE count,".  +--[",TMGi,"]"
	. IF countNodes=1 WRITE "(",$$ListCt($name(@pArray@(TMGi))),")"
	. WRITE "=",$EXTRACT($GET(@pArray@(TMGi)),1,40),!
	. SET someShown=1
	. SET Answers(count)=$name(@pArray@(TMGi))
	. SET count=count+1
	. NEW temp read *temp:0
	. IF temp'=-1 SET abort=1
	. SET pageCount=pageCount+1
	. IF (paginate=1)&(pageCount>pageLen) DO
	. . NEW temp
	. . read "Press [ENTER] to continue (^ to stop list)...",temp:$GET(DTIME,3600),!
	. . IF temp="^" SET abort=1
	. . SET pageCount=0
	. SET TMGi=$ORDER(@pArray@(TMGi),order)
	;
	IF someShown=0 WRITE "   (no data)",!
	WRITE !,"Enter # to browse (^ to backup): ^//"
	NEW temp read temp:$GET(DTIME,3600),!
	;
	NEW result SET result=$GET(Answers(temp))
	;
	QUIT result
	;
BRWSASK2
	;"Purpose: Improved... Ask user for the name of an array, then display nodes
	;
	NEW current
	NEW order SET order=1 ;"default = forward display.;
	NEW countNodes SET countNodes=0 ;"no counting
	WRITE !
	read "Enter name of array (or File number) to display nodes in: ",current:$GET(DTIME,3600),!
	IF +current=current DO
	. SET current=$GET(^DIC(+current,0,"GL"))
	. IF current="" WRITE "File number not found. Quitting.",! QUIT
	. WRITE "Browsing array: ",current,!
	IF current="" SET current="^"
	IF current="^" GOTO BA2Done
	;
	NEW % SET %=2 ;" default= NO
	WRITE "Display in REVERSE order? " DO YN^DICN WRITE !
	IF %=1 SET order=-1
	IF %=-1 GOTO BA2Done
	;
	SET %=2
	WRITE "Show number of subnodes? " DO YN^DICN WRITE !
	IF %=1 SET countNodes=1
	IF %=-1 GOTO BA2Done
	;
	DO BRWSNOD2(current,order,countNodes)
BA2Done
	QUIT
	;
	;" ==============================================================
	;" ==============================================================
	;
	;"BRWSNOD2(curRef,Order,countNodes)
	;"  ;"Purpose: to display nodes of specified array
	;"  ;"Input: curRef -- The reference to display
	;"  ;"       order -- OPTIONAL, default=1; 1 for forward, -1 for backwards order
	;"  ;"       paginate -- OPTIONAL, default=0;  0=no pagination, 1=pause after each page
	;"  ;"       countNodes -- OPTIONAL, default=0; 1=show number of child nodes.;
	;"  SET curRef=$$CREF^DILF(curRef)
	;"  IF curRef="" GOTO BN2Done
	;"  NEW TMGBRWORDER SET TMGBRWORDER=$GET(order,1)
	;"  NEW TMGBRWCN SET TMGBRWCN=$GET(countNodes,0)
	;"  IF $$ShowNod2(curRef,TMGBRWORDER,TMGBRWCN)
	;"BN2Done QUIT
	;"  ;
	;"ShowNod2(REF,ORDER,countNodes)
	;"  ;"Purpose: To display all the nodes of the given array
	;"  ;"   UPDATED function to use Scroller box.;
	;"  ;"Input: REF -- NAME OF array to display
	;"  ;"       ORDER -- OPTIONAL, default=1; 1 for forward, -1 for backwards order
	;"  ;"       countNodes -- OPTIONAL, default=0; 1=show number of child nodes.;
	;"  ;"Results: returns NAME OF next node to display (or "" IF none)
	;"  ;
	;"  NEW TMGi,OPTION
	;"  NEW TMGDATA,dispI SET dispI=1
	;"  SET ORDER=$GET(ORDER,1)
	;"  SET countNodes=$GET(countNodes,0)
	;"  ;
	;"  DO LOADREF(REF,"TMGDATA",ORDER) ;"put data into TMGDATA(#)
	;"  ;
	;"  SET OPTION("HEADER",1)="Data for "_REF
	;"  SET OPTION("FOOTER",1,1)="? Help"
	;"  SET OPTION("FOOTER",1,2)="LEFT Backup"
	;"  SET OPTION("FOOTER",1,3)="RIGHT Browse IN"
	;"  SET OPTION("ON SELECT")="HNDONSEL^TMGMISC"
	;"  SET OPTION("ON CMD")="HNDONCMD^TMGMISC"
	;"  SET OPTION("ON CURSOR")="HNDONCSR^TMGMISC"
	;"  SET OPTION("UNICODE LINES")=1
	;"  SET OPTION("UNICODE LINES","OPTION","THICK")=1
	;"  ;
	;"  DO ADDNICECOLORS^TMGUSRIF(.OPTION,3)
	;"  ;
	;"  ;"BELOW IS GOOD FOR DEBUGGING
	;"  NEW ZZDEBUG SET ZZDEBUG=1
	;"  IF ZZDEBUG=1 DO
	;"  . SET OPTION("SCRN TOP OFFSET")=22
	;"  . SET OPTION("SCRN HEIGHT")=20
	;"  . SET OPTION("SCRN WIDTH")=130
	;"  ;
	;"  WRITE #
	;"  DO SCROLLER^TMGUSRIF("TMGDATA",.OPTION)
	;"  QUIT REF
	;"  ;
	;"LOADREF(REF,OUTREF,ORDER) ;"Put REF(#) info @OUTREF@(<NUM),STR)
	;"  NEW IDX,JDX SET IDX="",JDX=1
	;"  SET ORDER=$GET(ORDER,1)
	;"  SET COUNTNODES=$GET(COUNTNODES,0)
	;"  ;
	;"  FOR  SET IDX=$ORDER(@REF@(IDX),ORDER) QUIT:(IDX="")  DO
	;"  . NEW STR SET STR=" +---["_IDX_"]"
	;"  . IF COUNTNODES=1 SET STR=STR_"("_$$ListCt($name(@REF@(IDX)))_")"
	;"  . NEW STR2 SET STR2=$EXTRACT($GET(@REF@(IDX)),1,40)
	;"  . IF STR2'="" SET STR=STR_"="_STR2
	;"  . IF $DATA(@REF@(IDX))>9 SET STR=STR_"   ..."
	;"  . NEW SUBREF SET SUBREF=$NAME(@REF@(IDX))
	;"  . SET @OUTREF@(JDX,STR)=SUBREF_$CHAR(9)_SUBREF,JDX=JDX+1
	;"  IF $DATA(@OUTREF)=0 SET @OUTREF@(JDX,"<NO DATA>")="",JDX=JDX+1
	;"  QUIT
	;"  ;
	;"HNDONSEL(pArray,Option,Info) ;
	;"  ;"Purpose: handle ON SELECT event from SCROLLER^TMGUSRIF, launched by ShowNod2
	;"  ;"Input: pArray,Option,Info -- see documentation in SCROLLER^TMGUSRIF
	;"  ;"       Info has this:
	;"  ;"    Info("CURRENT LINE","NUMBER")=number currently highlighted line
	;"  ;"    Info("CURRENT LINE","TEXT")=Text of currently highlighted line
	;"  ;"    Info("CURRENT LINE","RETURN")=return value of currently highlighted line
	;"  ;
	;"  NEW ref SET ref=$GET(Info("CURRENT LINE","RETURN"))
	;"  IF ref'="" IF $$ShowNod2(ref,TMGBRWORDER,TMGBRWCN)
	;"  QUIT
	;"  ;
	;"HNDONCMD(pArray,Option,Info) ;
	;"  ;"Purpose: handle ON SELECT event from Scroller, launched by ShowNod2
	;"  ;"Input: pArray,Option,Info -- see documentation in Scroller
	;"  ;"       Info has this:
	;"  ;"    Info("USER INPUT")=input
	;"  ;"    Info("CURRENT LINE","NUMBER")=number currently highlighted line
	;"  ;"    Info("CURRENT LINE","TEXT")=Text of currently highlighted line
	;"  ;"    Info("CURRENT LINE","RETURN")=return value of currently highlighted line
	;"  ;"results: none (required to have none)
	;"  NEW input SET input=$$UP^XLFSTR($GET(Info("USER INPUT")))
	;"  IF input="?" DO
	;"  . WRITE !,"Use UP and DOWN cursor keys to select global node",!
	;"  . WRITE "LEFT will back up, and RIGHT or ENTER will browse node",!
	;"  . WRITE "^ at the ':' prompt will cause a back up of one level",!
	;"  . DO PRESS2GO^TMGUSRI2
	;"  ELSE  IF input'="" DO
	;"  . WRITE !,"Input ",$GET(Info("USER INPUT"))," not recognized.",!
	;"  . DO PRESS2GO^TMGUSRI2
	;"  ;
	;"  WRITE #
	;"  QUIT
	;"  ;
	;"HNDONCSR(pArray,Option,Info) ;
	;"  ;"Purpose: handle ON SELECT event from Scroller, launched by ShowNod2
	;"  ;"Input: pArray,Option,Info -- see documentation in Scroller
	;"  ;"       Info has this:
	;"  ;"    Info("USER INPUT")=input
	;"  ;"    Info("CURRENT LINE","NUMBER")=number currently highlighted line
	;"  ;"    Info("CURRENT LINE","TEXT")=Text of currently highlighted line
	;"  ;"    Info("CURRENT LINE","RETURN")=return value of currently highlighted line
	;"  ;"       TMGSCLRMSG,TMGBRWORDER,TMGBRWCN - globally scoped variables that are used.;
	;"  ;"results: none (required to have none)
	;"  ;
	;"  NEW INPUT
	;"  NEW CURSOR SET CURSOR=$GET(Info("CURSOR"))
	;"  SET INPUT=$$UP^XLFSTR($GET(Info("USER INPUT")))
	;"  IF INPUT?1"{".A1"}" SET INPUT=$PIECE($PIECE(INPUT,"{",2),"}",1)
	;"  IF CURSOR="" SET CURSOR=INPUT
	;"  IF CURSOR["LEFT" DO
	;"  . SET TMGSCLRMSG="^"
	;"  . SET Info("CURSOR","HANDLED")=1
	;"  ELSE  IF CURSOR["RIGHT" DO
	;"  . NEW ref SET ref=$GET(Info("CURRENT LINE","RETURN"))
	;"  . IF ref'="" IF $$ShowNod2(ref,TMGBRWORDER,TMGBRWCN)
	;"  . SET Info("CURSOR","HANDLED")=1
	;"  QUIT
	;"  ;
	;" ==============================================================
	;" ==============================================================
	;
TESTBROWSE ;
	DO BRWSNOD3^TMGMISC($NAME(^TIU(8925)))
	QUIT
	;
BRWSNOD2(CURREF,ORDER,COUNTNODES)
	;"Purpose: to display nodes of specified array
	;"Input: curRef -- The reference to display
	;"       order -- OPTIONAL, default=1; 1 for forward, -1 for backwards order
	;"       paginate -- OPTIONAL, default=0;  0=no pagination, 1=pause after each page
	;"       countNodes -- OPTIONAL, default=0; 1=show number of child nodes.;
	SET CURREF=$$CREF^DILF(CURREF) IF CURREF="" GOTO BN3DN
	NEW TMGBRWORDER SET TMGBRWORDER=$GET(ORDER,1)
	NEW TMGBRWCN SET TMGBRWCN=$GET(COUNTNODES,0)
	IF $$SHOWNODE3(CURREF,TMGBRWORDER,TMGBRWCN)
BN3DN QUIT
	;
SHOWNODE3(REF,ORDER,COUNTNODES)
	;"Purpose: To display all the nodes of the given array
	;"   UPDATED function to use Scroller box.;
	;"Input: REF -- NAME OF array to display
	;"       ORDER -- OPTIONAL, default=1; 1 for forward, -1 for backwards order
	;"       COUNTNODES -- OPTIONAL, default=0; 1=show number of child nodes.;
	;"Results: returns NAME OF next node to display (or "" IF none)
	;
	NEW OPTION
	SET OPTION("HEADER",1)="Data for "_REF
	SET OPTION("FOOTER",1,1)="? Help"
	SET OPTION("FOOTER",1,2)="CURSOR to browse"
	SET OPTION("FOOTER",1,3)="F1 column GROW"
	SET OPTION("FOOTER",1,4)="F2 column SHRINK"
	SET OPTION("FOOTER",1,5)="F3 DROP right column"
	SET OPTION("ON SELECT")="HNDONSEL3^TMGMISC"
	SET OPTION("ON CMD")="HNDONCMD3^TMGMISC"
	SET OPTION("ON CURSOR")="HNDONCSR3^TMGMISC"
	SET OPTION("ON NEED DATA")="HNDONNEEDDATA^TMGMISC"
	SET OPTION("ON KEYPRESS")="HNDONKEYPRESS^TMGMISC"
	SET OPTION("UNICODE LINES")=1
	SET OPTION("UNICODE LINES","OPTION","THICK")=1
	SET OPTION("USER-DATA","BROWSE REF",1)=REF
	SET OPTION("USER-DATA","COUNT NODES")=+$GET(COUNTNODES)
	SET OPTION("USER-DATA","ORDER")=+$GET(ORDER,1)
	;
	DO ADDNICECOLORS^TMGUSRIF(.OPTION,3)
	;
	;"BELOW IS GOOD FOR DEBUGGING
	NEW ZZDEBUG SET ZZDEBUG=0
	IF ZZDEBUG=1 DO
	. SET OPTION("SCRN TOP OFFSET")=22
	. SET OPTION("SCRN HEIGHT")=20
	. SET OPTION("SCRN WIDTH")=130
	;
	WRITE #
	DO SCROLLER^TMGUSRIF("TMGDATA",.OPTION)
	QUIT REF
	;
HNDONNEEDDATA(TMGPSCRLARR,OPTION,INFO)  ;"Part of SHOWNODE3
	;"  INFO("DATA","SELECTED",<COL#>)=<Selected Index of line>  I.e. what line is selected in column 1, 2, 3, etc.;
	;"  INFO("DATA","SELECTED",<COL#>,"TEXT")=<Text of selected line>
	;"          NOTE: <COL#> will be 1..<CUR_COL>-1  I.e. columns to the LEFT of current column.;
	;"  INFO("DATA","CUR COLUMN")=<COL#> for which data is being needed
	;"  INFO("DATA","CUR COLUMN","START INDEX")=<LINE#> for beginning of lines of data whicg is needed
	;"  INFO("DATA","CUR COLUMN","END INDEX")=<LINE#> for end of lines of data which is needed
	;"  INFO("DATA","HANDLED")=1 if event handler has returned data.  If 0 then data will be obtained from legacy methods
	;"  INFO("DATA","TEXT",<LINE#>)=<DISPLAY TEXT>  <--- event handler returns data for display here.    ;"
	NEW CURCOL SET CURCOL=$GET(INFO("DATA","CUR COLUMN")) QUIT:CURCOL'>0
	NEW COUNTNODES SET COUNTNODES=+$GET(OPTION("USER-DATA","COUNT NODES"))
	NEW ORDER SET ORDER=+$GET(OPTION("USER-DATA","ORDER"),1)
	NEW DATAREF SET DATAREF=""
	IF CURCOL=1 DO
	. SET DATAREF=$GET(OPTION("USER-DATA","BROWSE REF",1))
	ELSE  DO  QUIT:DATAREF=""
	. NEW HL MERGE HL=INFO("VIEWSTATE","HIGHLINE",CURCOL-1)
	. SET DATAREF=$GET(HL("TEXT","RETURN"))
	IF DATAREF="" QUIT
	NEW STARTIDX SET STARTIDX=INFO("DATA","CUR COLUMN","START INDEX")
	NEW ENDIDX SET ENDIDX=INFO("DATA","CUR COLUMN","END INDEX")
	NEW TEMP DO LOADREF3(DATAREF,STARTIDX-1,(ENDIDX-STARTIDX+1),"TEMP",STARTIDX,ORDER,COUNTNODES)
	NEW IDX FOR IDX=STARTIDX:1:ENDIDX DO
	. NEW LINE SET LINE=$GET(TEMP(IDX))
	. NEW RTN SET RTN=$GET(TEMP(IDX,"RETURN"))
	. SET INFO("DATA","TEXT",IDX)=LINE
	. IF RTN'="" SET INFO("DATA","TEXT",IDX,"RETURN")=RTN
	SET INFO("DATA","HANDLED")=1
	QUIT
	;
LOADREF3(NAME,OFFSETIDX,COUNT,OUTREF,STARTIDX,ORDER,COUNTNODES) ;"Put REF(#) info @OUTREF@(<NUM>)=STR
	;"Input: NAME --   PASS BY NAME.  Should be a a closed reference.;
	;"       OFFSETIDX -- OPTIONAL.  DEFAULT is 0
	;"          If > 0 then this is how many lines to skip over.;
	;"       COUNT -- NUMBER OF LINES TO RETURN.;
	;"       OUTREF -  PASS BY NAME.  Name of array to put output into.;
	;"       STARTIDX -- OPTIONAL.  Starting index #.  Default=1
	;"Output:  @OUTREF@(1)=First line
	;"         @OUTREF@(2)=2nd line... etc.;
	NEW JDX SET JDX=1
	SET ORDER=$GET(ORDER,1)
	SET COUNTNODES=$GET(COUNTNODES,0)
	SET OFFSETIDX=+$GET(OFFSETIDX)
	SET COUNT=+$GET(COUNT,10)
	SET STARTIDX=+$GET(STARTIDX) IF STARTIDX'>0 SET STARTIDX=1
	NEW IDX SET IDX=STARTIDX
	DO  ;"GET TOP LEVEL NODE FIRST...;
	. IF OFFSETIDX>0 SET OFFSETIDX=OFFSETIDX-1 QUIT
	. IF COUNT<1 QUIT
	. NEW VAL SET VAL=$GET(@NAME)
	. SET @OUTREF@(IDX)=NAME_"="_$$FORMAT^TMGZWR(VAL),IDX=IDX+1,COUNT=COUNT-1
	;"NOW CHILDREN NODES.;
	NEW REFIDX SET REFIDX=""
	FOR  SET REFIDX=$ORDER(@NAME@(REFIDX),ORDER) QUIT:(REFIDX="")!(COUNT<1)  DO
	. IF OFFSETIDX>0 SET OFFSETIDX=OFFSETIDX-1 QUIT
	. NEW STR SET STR=" +---["_REFIDX_"]"
	. IF COUNTNODES=1 SET STR=STR_"("_$$ListCt($NAME(@NAME@(REFIDX)))_")"
	. NEW VAL SET VAL=$GET(@NAME@(REFIDX))
	. NEW STR2 SET STR2=$$FORMAT^TMGZWR(VAL)
	. ;"NEW STR2 SET STR2=$EXTRACT($GET(@NAME@(REFIDX)),1,40)
	. IF STR2'="" SET STR=STR_"="_STR2
	. IF $DATA(@NAME@(REFIDX))>9 SET STR=STR_"   ..."
	. SET @OUTREF@(IDX)=STR
	. SET @OUTREF@(IDX,"RETURN")=$NAME(@NAME@(REFIDX))
	. SET IDX=IDX+1,COUNT=COUNT-1
	IF $DATA(@OUTREF)=0 SET @OUTREF@(IDX)="<NO DATA>"
	QUIT
	;
HNDONSEL3(TMGPSCRLARR,OPTION,INFO) ;
	;"Purpose: handle ON SELECT event from SCROLLER^TMGUSRIF, launched by ShowNod2
	SET INFO("CURSOR")="RIGHT"
	DO HNDONCSR3(.TMGPSCRLARR,.OPTION,.INFO)
	QUIT
	;
HNDONCMD3(pArray,Option,Info) ;
	;"Purpose: handle ON SELECT event from Scroller, launched by ShowNod2
	;"Input: pArray,Option,Info -- see documentation in Scroller
	;"       Info has this:
	;"    Info("USER INPUT")=input
	;"    Info("CURRENT LINE","NUMBER")=number currently highlighted line
	;"    Info("CURRENT LINE","TEXT")=Text of currently highlighted line
	;"    Info("CURRENT LINE","RETURN")=return value of currently highlighted line
	;"results: none (required to have none)
	NEW input SET input=$$UP^XLFSTR($GET(Info("USER INPUT")))
	IF input="?" DO
	. WRITE !,"Use UP and DOWN cursor keys to select global node",!
	. WRITE "LEFT will back up, and RIGHT or ENTER will browse node",!
	. WRITE "^ at the ':' prompt to exit",!
	. DO PRESS2GO^TMGUSRI2
	ELSE  IF input'="" DO
	. WRITE !,"Input ",$GET(Info("USER INPUT"))," not recognized.",!
	. DO PRESS2GO^TMGUSRI2
	;
	WRITE #
	QUIT
	;
HNDONCSR3(TMGPSCRLARR,OPTION,INFO) ;
	;"Purpose: handle ON CURSOR event from Scroller, launched by ShowNod3
	;"         Event is fired when a cursor key is pressed, before the scroller moves the highlighted line.;
	;"Input: TMGPSCRLARR,OPTION,INFO -- see documentation in SCROLLER
	;"       INFO has this:
	;"          INFO("CURSOR")=Cursor Key pressed
	;"          INFO("CURSOR","HANDLED")=1 <-- this is what called code should set if it handled
	;"                          the cursor event. This will prevent scroller from acting on it.;
	NEW CURSOR SET CURSOR=$GET(INFO("CURSOR"))
	IF "LEFT,RIGHT"'[CURSOR QUIT
	NEW ACTIVECOL SET ACTIVECOL=+$GET(OPTION("COLUMNS","ACTIVE"),1)
	NEW MAXCOLS SET MAXCOLS=+$GET(OPTION("COLUMNS","NUM"),1)
	IF CURSOR="RIGHT" DO
	. IF ACTIVECOL>=MAXCOLS DO  ;"SET UP A NEW COLUMN.;
	. . SET CMDIDX=$$ADDDOFN(.INFO,"ADD COLUMN(1)")   ;"1 --> AUTO SET COL WIDTHS
	. . NEW IDX SET IDX=$SELECT((ACTIVECOL+1)#2=0:2,1:1)  ;"If To-be-added column number will be EVEN --> 2, otherwise 1
	. . IF (ACTIVECOL+1)=2 QUIT  ;"COL 2 has colors set up in ADDNICECOLORS, called in DEMODYNCOLS
	. . NEW DATAREF SET DATAREF=$NAME(INFO("DO FUNCTION",CMDIDX,"DATA"))
	. . IF IDX=1 MERGE @DATAREF@("COLORS")=OPTION("COLORS")
	. . ELSE  MERGE @DATAREF@("COLORS")=OPTION("COLUMNS",IDX,"COLORS")
	. ELSE  SET ACTIVECOL=ACTIVECOL+1
	IF CURSOR="LEFT" DO
	. IF ACTIVECOL>1 SET ACTIVECOL=ACTIVECOL-1
	SET TMGSCLRMSG="FULL"
	SET OPTION("COLUMNS","ACTIVE")=ACTIVECOL
	SET INFO("CURSOR","HANDLED")=1
	QUIT
	;
ADDDOFN(INFO,NAME) ;"ADD DO FUNCTION
	NEW IDX SET IDX=$ORDER(INFO("DO FUNCTION",""),-1)+1
	SET INFO("DO FUNCTION",IDX)=NAME
	QUIT IDX
	;
HNDONKEYPRESS(TMGPSCRLARR,OPTION,INFO) ;"HANDLE On Keypress
	;"                  INFO("USER INPUT")=Key pressed
	;"                  INFO("CMD")=User full input command so far (being built up)
	;
	NEW INPUT SET INPUT=$GET(INFO("USER INPUT"))
	NEW MAXCOLS SET MAXCOLS=+$GET(OPTION("COLUMNS","NUM"),1)
	IF INPUT="{F1}" DO
	. IF $$ADDDOFN(.INFO,"GROW COLUMN")   ;"ignore result
	IF INPUT="{F2}" DO
	. IF $$ADDDOFN(.INFO,"SHRINK COLUMN")   ;"ignore result
	IF INPUT="{F3}",MAXCOLS>1 DO
	. IF $$ADDDOFN(.INFO,"DROP COLUMN")   ;"ignore result
	QUIT
	;
	;" ==============================================================
	;" ==============================================================
	;
SHOWVAR(AREF)  ;"
	IF AREF["$" DO  QUIT
	. WRITE AREF,"='"
	. NEW CODE SET CODE="do DEBUGWRITE^TMGIDE2C(1,"_AREF_")"
	. NEW $ETRAP SET $ETRAP="WRITE ""(Invalid M Code!.  Error Trapped.)"",! SET $ETRAP="""",$ecode="""""
	. XECUTE CODE
	. WRITE "'    "
	;
	SET AREF=$$CREF^DILF(AREF)  ;"convert open to closed format
	NEW TEMP DO ARRDUMP^TMGMISC3(AREF,,,"TEMP")
	NEW ARRAY,IDX,JDX SET IDX=0,JDX=1
	FOR  SET IDX=$ORDER(TEMP(IDX)) QUIT:IDX'>0  SET ARRAY(JDX,$GET(TEMP(IDX)))="",JDX=JDX+1
	KILL TEMP
	NEW OPTION
	SET OPTION("HEADER",1)=" - Display of ["_AREF_"] - "
	SET OPTION("FOOTER",1)="Enter ^ to exit"
	SET OPTION("ON SELECT")="HNDONSEL^TMGMISC"
	DO ADDNICECOLORS^TMGUSRIF(.OPTION,1)
	;
	;"BELOW IS GOOD FOR DEBUGGING
	NEW ZZDEBUG SET ZZDEBUG=0
	IF ZZDEBUG=1 DO
	. SET OPTION("SCRN TOP OFFSET")=22
	. SET OPTION("SCRN HEIGHT")=20
	. SET OPTION("SCRN WIDTH")=130
	;
	DO SCROLLER^TMGUSRIF("ARRAY",.OPTION)
	QUIT
	;
HNDONSEL(TMGPSCRLARR,OPTION,INFO)  ;"Part of SHOWVAR -- Handle ON SELECT event from SCROLLER
	SET TMGSCLRMSG="^"
	QUIT
	;" ==============================================================
	;" ==============================================================
	;
ZWRVAR(AREF) ;"
	IF AREF["$" DO  QUIT
	. NEW CODE
	. NEW $ETRAP SET $ETRAP="WRITE ""(Invalid M Code!.  Error Trapped.)"",! SET $ETRAP="""",$ecode="""""
	. WRITE AREF,"='"
	. SET CODE="do DEBUGWRITE^TMGIDE2C(1,"_AREF_")"
	. XECUTE CODE
	. WRITE "'    "
	;
	SET AREF=$$CREF^DILF(AREF)  ;"convert open to closed format
	NEW TEMP
	DO ZWR2ARR^TMGZWR(AREF,"TEMP")  ;"ZWRITE @AREF
	NEW ARRAY,IDX,JDX SET IDX=0,JDX=1
	FOR  SET IDX=$ORDER(TEMP(IDX)) QUIT:IDX'>0  SET ARRAY(JDX,$GET(TEMP(IDX)))="",JDX=JDX+1
	KILL TEMP
	NEW OPTION
	SET OPTION("HEADER",1)=" - ZWRITE of ["_AREF_"] - "
	SET OPTION("FOOTER",1)="Enter ^ to exit"
	SET OPTION("ON SELECT")="HNDONSEL^TMGMISC"
	DO ADDNICECOLORS^TMGUSRIF(.OPTION,1)
	;
	;"BELOW IS GOOD FOR DEBUGGING
	NEW ZZDEBUG SET ZZDEBUG=0
	IF ZZDEBUG=1 DO
	. SET OPTION("SCRN TOP OFFSET")=22
	. SET OPTION("SCRN HEIGHT")=20
	. SET OPTION("SCRN WIDTH")=130
	;
	DO SCROLLER^TMGUSRIF("ARRAY",.OPTION)
	QUIT
	;
	;" ==============================================================
	;" ==============================================================
	;
IsNumeric(value)
	;"Purpose: to determine IF value is pure numeric.;
	;"Note: This will be a more involved test than simply: IF +value=value, because
	;"      +"00001" is not the same as "1" or 1.  Also +"123abc"--> 123, but is not pure numeric
	;
	;"NOTE: 8/11/13 -- SEE ALSO $$ISNUM^TMGSTUT3.;
	SET value=$$Trim^TMGSTUTL(value)  ;" trim whitespace
	SET value=$$TrimL^TMGSTUTL(value,"0") ;"trim leading zeros
	QUIT (value=+value)
	;
ClipDDigits(Num,digits)
	;"Purpose: to clip number to specified number of decimal digits
	;"   e.g. 1234.9876543 --> 1234.9876  IF digits=4
	;"Input: Num -- the number to process
	;"       digits -- the number of allowed decimal digits after the decimal point
	;"Result: returns the number clipped to the specified number of decimals
	;"      note: this is a CLIP, not a ROUND function
	;
	NEW result SET result=Num
	NEW decimals SET decimals=$EXTRACT($PIECE(Num,".",2),1,digits)
	SET result=$PIECE(Num,".",1)
	IF decimals'="" SET result=result_"."_decimals
CDgDone
	QUIT result
	;
Diff(File,IENS1,IENS2,Result)
	;"Purpose: to determine how two records differ in a given file
	;"Input: File -- file name or number of file containing records to be compared
	;"       IENS1 -- the IEN (or IENS IF file is a subfile) of the first record to be compared
	;"       IENS2 -- the IEN (or IENS IF file is a subfile) of the second record to be compared
	;"       Result -- PASS BE REFERENCE, and OUT PARAMETER
	;"        Format of output Result array.  Will only hold differences
	;"        e.g. Result(FieldNum,"EXTRA",1)=valueOfField
	;"        e.g. Result(FieldNum,"EXTRA",2)=valueOfField
	;"        e.g. Result(FieldNum,"CONFLICT",1)=valueOfField
	;"        e.g. Result(FieldNum,"CONFLICT",2)=valueOfField
	;"        e.g. Result(FieldNum,"FIELD NAMES")=FieldName
	;"Note: this will consider only the first 1024 characters of  WP fields
	;"Note: For now, multiples (subfiles) will be IGNORED
	;
	NEW fileNum SET fileNum=+$GET(File)
	IF fileNum=0 SET fileNum=$$GETFNUM^TMGDBAP3(.File)
	NEW subFileNum
	;
	NEW field SET field=$ORDER(^DD(fileNum,0))
	IF +field>0 FOR  DO  QUIT:(+field'>0)
	. SET subFileNum=+$PIECE($GET(^DD(fileNum,field,0)),"^",2) ;"get subfile number, or 0 IF not subfile
	. IF subFileNum>0 DO  ;"finish later...;
	. . ;"Here I need to somehow cycle through each record of the subfile and compare THOSE
	. . NEW subResult
	. . DO DiffSubFile(subFileNum,.IENS1,.IENS2,.subResult) ;"null function for now
	. . ;"do some MERGE between Result and subResult
	. ELSE  DO Diff1Field(fileNum,field,.IENS1,.IENS2,.Result)
	. SET field=$ORDER(^DD(fileNum,field))
	;
	QUIT
	;
Diff1Field(File,Field,IENS1,IEN2,Result)
	;"Purpose: to determine how two records differ for one given field
	;"Input: File -- file NUMBER of file containing records to be compared
	;"       Field -- Field NUMBER to be evaluated
	;"       IENS1 -- the IEN (or IENS IF file is a subfile) of the first record to be compared
	;"       IENS2 -- the IEN (or IENS IF file is a subfile) of the second record to be compared
	;"       Result -- PASS BE REFERENCE, and OUT PARAMETER
	;"        Format of output Result array.  Will only hold differences
	;"        e.g. Result(FieldNum,"EXTRA",1)=valueOfField
	;"        e.g. Result(FieldNum,"EXTRA",2)=valueOfField
	;"        e.g. Result(FieldNum,"CONFLICT",1)=valueOfField
	;"        e.g. Result(FieldNum,"CONFLICT",2)=valueOfField
	;"        e.g. Result(FieldNum,"FIELD NAMES")=FieldName
	;"Results: none (data returned in Result out parameter)
	;"Note: only first 1023 characters of a WP field will be compared
	;
	NEW value1,value2,TMGWP1,TMGWP2
	NEW fieldName SET fieldName=$PIECE($GET(^DD(File,Field,0)),"^",1)
	;
	SET value1=$$GET1^DIQ(File,IENS1,Field,"","TMGWP1")
	SET value2=$$GET1^DIQ(File,IENS2,Field,"","TMGWP2")
	;
	IF $DATA(TMGWP1)!$DATA(TMGWP2) DO
	. SET value1=$$WPToStr^TMGSTUTL("TMGWP1"," ",1023)  ;"Turn first 1023 characters into one long string
	. SET value2=$$WPToStr^TMGSTUTL("TMGWP2"," ",1023)  ;"Turn first 1023 characters into one long string
	;
	IF value1=value2 GOTO D1FDone ;"default is no conflict
	IF (value2="")&(value1'="") DO
	. SET Result(Field,"EXTRA",1)=value1
	. SET Result(Field,"FIELD NAME")=fieldName
	IF (value1="")&(value2'="") DO
	. SET Result(Field,"EXTRA",2)=value2
	. SET Result(Field,"FIELD NAME")=fieldName
	IF (value1'="")&(value2'="") DO
	. SET Result(Field,"CONFLICT",1)=value1
	. SET Result(Field,"CONFLICT",2)=value2
	. SET Result(Field,"FIELD NAME")=fieldName
	;
D1FDone
	QUIT
	;
DiffSubFile(SubFile,IENS1,IENS2,Result)
	;
	QUIT
	;
Array2XML(pArray,pResult,indent)
	;"Purpose: to convert an array into XML format
	;"Input: pArray -- the NAME OF the array to convert (array can be any format)
	;"       pResult -- the NAME OF the output array.;
	;"        format:
	;"    Result(0)="<?xml version='1.0'?>"
	;"    Result(1)="<Node id="Node Name">Node Value</Node>
	;"    Result(2)="  <Node id="Node Name">Node Value</Node>
	;"    Result(3)="  <Node id="Node Name">Node Value</Node>
	;"    Result(4)="  <Node id="Node Name">Node Value    ;"<--- start subnode
	;"    Result(5)="    <Node id="Node Name">Node Value</Node>
	;"    Result(6)="    <Node id="Node Name">Node Value</Node>
	;"    Result(7)="  </Node>          ;"<---- end subnode
	;"    Result(8)="  <Node id="Node Name">Node Value</Node>
	;"       indent -- OPTIONAL.  IF 1, then subnodes have whitespace indent for pretty viewing
	;"Output: pResult is filled
	;"Result: none.;
	;"Note: example call  DO Array2XML("MyArray","MyOutput",1)
	;
	KILL @pResult
	SET @pResult@(0)=0
	IF $GET(indent)=1 SET indent=""
	ELSE  SET indent=-1
	DO A2XNode(pArray,pResult,.indent)
	SET @pResult@(0)=$$XMLHDR^MXMLUTL
	;
	QUIT
	;
A2XNode(pArray,pResult,indent)
	;"Purpose: To DO the output for Array2XML
	;"Input: pArray - the NAME OF the array to convert
	;"       pResult - the NAME OF the output array.;
	;"        Format to be as described in Array2XML, which one exception: Result(0)=MaxLine
	;"       indent -- OPTIONAL.  IF numeric value, then subnodes WON't whitespace indent for pretty viewing
	;"            otherwise, indent is string holding space to indent
	;"Result: none
	;
	NEW i,s
	SET indent=$GET(indent)
	SET i=$ORDER(@pArray@(""))
	IF i'="" FOR  DO  QUIT:(i="")
	. SET s="" IF indent'=-1 SET s=indent
	. SET s=s_"<Node id="""_i_""">"_$GET(@pArray@(i))
	. SET s=$$SYMENC^MXMLUTL(s)
	. IF $DATA(@pArray@(i))>1 DO
	. . SET @pResult@(0)=+$GET(@pResult@(0))+1  ;"Increment maxline
	. . SET @pResult@(@pResult@(0))=s
	. . NEW subIndent SET subIndent=-1
	. . IF indent'=-1 SET subIndent=indent_"  "
	. . DO A2XNode($name(@pArray@(i)),pResult,subIndent)
	. . SET s="" IF indent'=-1 SET s=indent
	. . SET s=s_"</Node>"
	. ELSE  DO
	. . SET s=s_"</Node>"
	. SET @pResult@(0)=+$GET(@pResult@(0))+1  ;"Increment maxline
	. SET @pResult@(@pResult@(0))=s
	. SET i=$ORDER(@pArray@(i))
	;
	QUIT
	;
Up(pArray)
	;"Purpose: Return a NAME of an array that is one level 'up' from the
	;"   the current array.  This really means one node shorter.;
	;"   e.g. '^MyVar('plant','tree','apple tree')' --> '^MyVar('plant','tree')'
	;"Results: returns shorten array as above, or "" IF error
	;
	NEW result SET result=""
	IF $GET(pArray)="" GOTO UpDone
	SET result=$qsubscript(pArray,0)
	NEW i
	FOR i=1:1:$qlength(pArray)-1 DO
	. SET result=$name(@result@($qsubscript(pArray,i)))
	;
UpDone  QUIT result
	;
LaunchScreenman(File,FormIEN,RecIEN,Page)
	;"Purpose: to provide a programatic launching point for displaying a
	;"   screenman form for editing a record
	;"Input: File -- the IEN of file to be edited
	;"       FormIEN -- the IEN in file FORM (.403)
	;"       RecIEN -- the IEN in File to edit
	;"       Page -- OPTIONAL, default=1.  The starting page of form.;
	;"Note: Form should be compiled before calling the function.  This can be
	;"      achieved by running the form once from ^DDSRUN (or viat Fileman menu)
	;
	NEW DDSFILE SET DDSFILE=File
	NEW DDSRUNDR SET DDSRUNDR=FormIEN
	NEW DDSPAGE SET DDSPAGE=+$GET(Page,1)
	NEW DA SET DA=RecIEN
	;
	DO REC+9^DDSRUN  ;"this goes against SAC conventions.;
	;
	QUIT
	;
NumSigChs()
	;"Purpose: To determine how many characters are signficant in a variable name
	;"   I.e. older versions of GT.M had only the first 8 characters as
	;"   significant.  Newer versions allow more characters to be significant.;
	;
	NEW pVar1,pVar2,i
	SET pVar1="zb",i=2
	NEW done SET done=0
	FOR  DO  QUIT:done
	. SET i=i+1
	. SET pVar2=pVar1_"b"
	. SET pVar1=pVar1_"a"
	. NEW @pVar2,@pVar1
	. SET @pVar1=7
	. IF $GET(@pVar2)=@pVar1 SET done=1
	;
	QUIT (i-1)
	;
SrchReplace(File,Field,Caption)
	;"Purpose: To DO a text-based search and replace in all record of
	;"   specified file, in the text of the specified file.;
	;"   Note: this does not work with pointer fields.  It would
	;"   fail to find the matching text in the pointer value and ignore it.;
	;"   It does not support subfiles.;
	;"Input: File -- the file name or number to work with.;
	;"       Field -- the field name or number to work with
	;"       Caption -- OPTIONAL.  A descriptive text of action.;
	;"Output: Data in records will be changed via Fileman and errors (if found)
	;"  will be written to console.;
	;"Results: none.;
	;
	IF $GET(File)="" GOTO SRDone
	IF $GET(Field)="" GOTO SRDone
	NEW OKToCont SET OKToCont=1
	IF +Field'=Field SET OKToCont=$$SETFFNUM^TMGDBAP3(File,Field,.File,.Field)
	IF OKToCont=0 GOTO SRDone
	;
	IF $GET(Caption)'="" DO
	. WRITE !,!,Caption,!
	. WRITE "----------------------------------------------------",!!
	;
	NEW searchS,replaceS,%
SR1
	WRITE "Enter characters/words to SEARCH for (^ to abort): "
	read searchS:$GET(DTIME,3600),!
	IF (searchS="")!(searchS="^") GOTO SRDone
	WRITE "REPLACE with (^ to abort): "
	read replaceS:$GET(DTIME,3600),!
	IF (replaceS="^") GOTO SRDone
	WRITE "'",searchS,"'-->'",replaceS,"'",!
	SET %=1
	WRITE "OK" DO YN^DICN WRITE !
	IF %=1 GOTO SR2
	IF %=-1 GOTO SRDone
	GOTO SR1
	;
SR2
	NEW Itr,IEN,CurValue,abort,count
	NEW ref SET ref=$GET(^DIC(File,0,"GL"))
	SET ref=$$CREF^DILF(ref)
	IF ref="" GOTO SRDone
	NEW node SET node=$PIECE($GET(^DD(File,Field,0)),"^",4)
	NEW piece SET piece=$PIECE(node,";",2)
	SET node=$PIECE(node,";",1)
	;
	SET abort=0,count=0
	SET IEN=$$ItrInit^TMGITR(File,.Itr)
	DO PrepProgress^TMGITR(.Itr,20,0,"IEN")
	IF IEN'="" FOR  DO  QUIT:($$ItrNext^TMGITR(.Itr,.IEN)'>0)!abort
	. IF $$USRABORT^TMGUSRI2() SET abort=1 QUIT
	. SET CurValue=$PIECE($GET(@ref@(IEN,node)),"^",piece)
	. IF CurValue'[searchS QUIT
SR3     . NEW NEWValue SET NEWValue=$$REPLSTR^TMGSTUT3(CurValue,searchS,replaceS)
	. NEW TMGFDA,TMGMSG
	. SET TMGFDA(File,IEN_",",Field)=NEWValue
	. DO FILE^DIE("K","TMGFDA","TMGMSG")
	. DO SHOWDIER^TMGDEBU2(.TMGMSG)
	. SET count=count+1
	DO ProgressDone^TMGITR(.Itr)
	;
	WRITE count," records changed",!
	DO PRESS2GO^TMGUSRI2
	;
SRDone
	QUIT
	;
MkMultList(input,List)
	;"Purpose: To create a list of entries, given a string containing a list of entries.;
	;"Input: input -- a string of user input.  E.g.: '345,3,12678,78-85,2' or '78-93' or '15'
	;"       List -- PASS BY REFERENCE.  An OUT PARAMETER.;
	;"Output: List will be filled as follows:
	;"        List(Entry number)=""
	;"        List(Entry number)=""
	;"        List(Entry number)=""
	;"Result: 1 IF values found, 0 none found, or error encountered
	;
	NEW result SET result=0
	;
	NEW i
	FOR i=1:1:$LENGTH(input,",") DO
	. NEW value SET value=$PIECE(input,",",i)
	. IF +value=value DO
	. . SET List(value)=""
	. . SET result=1
	. ELSE  IF value["-" DO
	. . NEW n1,n2
	. . SET n1=+$PIECE(value,"-",1)
	. . SET n2=+$PIECE(value,"-",2)
	. . SET result=$$MkRangeList(n1,n2,.List)
	;
	QUIT result
	;
MkRangeList(Num,EndNum,List)
	;"Purpose: To create a list of entries, given a starting and ending number
	;"Input: Num -- the start entry number
	;"       EndNum -- OPTIONAL, the last entry number (if supplied then all values
	;"        between Num and Endnum will be added to list
	;"       List -- PASS BY REFERENCE.  An OUT PARAMETER.;
	;"Output: List will be filled as follows:
	;"        List(Entry number)=""
	;"        List(Entry number)=""
	;"        List(Entry number)=""
	;"Result: 1 IF value input found, otherwise 0
	;
	NEW result SET result=0
	SET EndNum=$GET(EndNum,Num)
	IF (+Num'=Num)!(+EndNum'=EndNum) GOTO MkRLDone
	;
	NEW i
	FOR i=Num:1:EndNum DO
	. SET List(i)=""
	. SET result=1
	;
MkRLDone
	QUIT result
	;
Flags(Var,Flag,Mode)
	;"Purpose: To SET,delete,or toggle a flag stored in Var
	;"Input: Var -- PASS BY REFERENCE.  The variable holding the flags
	;"       Flag -- a single character flag to be stored in Var
	;"       Mode: should be: 'SET','DEL',or 'TOGGLE'.  Default is 'SET'
	;"Results: none
	;
	SET Flag=$GET(Flag,"SET")
	SET Var=$GET(Var)
	IF $GET(Mode)="TOGGLE" DO
	. IF Var[Flag SET Mode="DEL"
	. ELSE  SET Mode="SET"
	IF $GET(Mode)="SET" DO
	. IF Var[Flag QUIT
	. SET Var=Var_Flag
	IF $GET(Mode)="DEL" DO
	. IF Var'[Flag QUIT
	. SET Var=$PIECE(Var,Flag,1)_$PIECE(Var,Flag,2)
	;
	QUIT
	;
CompABArray(pArrayA,pArrayB,pExtraB,pMissingB,pDiff,ProgressFn,IncVar)
	;"Purpose: To compare two arrays, A & B, and return results in OutArray
	;"   that specifies how ArrayB differs from ArrayA
	;"Input: pArrayA -- PASS BY NAME. Baseline array to be compared against
	;"       pArrayB -- PASS BY NAME. Array to be compare against ArrayA
	;"       pExtraB -- PASS BY NAME. An OUT PARAMETER.  Array of extra info from B
	;"          OPTIONAL.  If not provided, then data not filled.;
	;"       pMissingB -- PASS BY NAME. An OUT PARAMETER.  Array of missing info
	;"          OPTIONAL.  If not provided, then data not filled.;
	;"       pDiff -- PASS BY NAME. An OUT PARAMETER.  Output as below.;
	;"          OPTIONAL.  If not provided, then data not filled.;
	;"    @pOutArray@("A",node,node,node,...)=different value
	;"    @pOutArray@("B",node,node,node,...)=different value
	;"       ProgressFn -- OPTIONAL -- M code to exec as a progress indicator
	;"       IncVar -- OPTIONAL -- a counter that can be referenced by ProgressFn
	;"Results: 0=OK, 1=aborted
	;
	NEW indexA,indexB
	;
	SET IncVar=+$GET(IncVar)
	SET ProgressFn=$GET(ProgressFn)
	SET pExtraB=$GET(pExtraB)
	SET pMissingB=$GET(pMissingB)
	SET pdiff=$GET(pDiff)
	NEW abort SET abort=0
	NEW Compared
	;
	SET indexA=""
	FOR  SET indexA=$ORDER(@pArrayA@(indexA)) QUIT:(indexA="")!abort  DO
	. SET IncVar=IncVar+1
	. IF (IncVar#10=1),(ProgressFn'="") DO  QUIT:(abort)
	. . NEW $ETRAP SET $ETRAP="SET $ETRAP="""",$ecode="""""
	. . xecute ProgressFn
	. . WRITE !,pArrayA,"(",indexA,")  ",!  DO CUU^TMGTERM(2)  ;"temp
	. . IF $$USRABORT^TMGUSRI2() SET abort=1 QUIT
	. IF $DATA(@pArrayB@(indexA))=0 DO  QUIT
	. . IF (pMissingB'="") MERGE @pMissingB@(pArrayA,indexA)=@pArrayA@(indexA)
	. NEW s1,s2
	. SET s1=$GET(@pArrayA@(indexA))
	. SET s2=$GET(@pArrayB@(indexA))
	. IF s1'=s2 DO
	. . IF pDiff="" QUIT
	. . IF $$TRIM^XLFSTR(s1)=$$TRIM^XLFSTR(s2) QUIT
	. . SET @pDiff@("A",pArrayA,indexA)=s1
	. . SET @pDiff@("B",pArrayA,indexA)=s2
	. SET abort=$$CompABArray($name(@pArrayA@(indexA)),$name(@pArrayB@(indexA)),.pExtraB,.pMissingB,.pDiff,.ProgressFn,.IncVar)
	. SET Compared($name(@pArrayA@(indexA)),$name(@pArrayB@(indexA)))=1
	;
	NEW temp SET temp=1
	SET indexB=""
	FOR  SET indexB=$ORDER(@pArrayB@(indexB)) QUIT:(indexB="")!abort  DO
	. SET temp=temp+1
	. IF (temp#10=1) DO  QUIT:(abort)
	. . WRITE !,pArrayA,"(",indexB,")  ",!  DO CUU^TMGTERM(2)  ;"temp
	. . IF $$USRABORT^TMGUSRI2() SET abort=1 QUIT
	. IF $DATA(@pArrayA@(indexB))=0 DO  QUIT
	. . IF (pExtraB'="") MERGE @pExtraB@(pArrayA,indexB)=@pArrayB@(indexB)
	. IF $GET(Compared($name(@pArrayA@(indexB)),$name(@pArrayB@(indexB))))=1 DO  QUIT  ;"already checked
	. . NEW temp
	. SET abort=$$CompABArray($name(@pArrayA@(indexB)),$name(@pArrayB@(indexB)),.pExtraB,.pMissingB,.pDiff)
	;
	QUIT abort
	;
FixArray(ref)
	;"Purpose: Convert an array like this:
	;"  @ref@("^DD(2,.362)",21,1,0)  --> @ref@("^DD",2,.362,21,1,0)
	;"  @ref@("^DD(2,.362)",21,2,0)  --> @ref@("^DD",2,.362,21,2,0)
	;"  @ref@("^DD(2,.362)",23,0)  --> @ref@("^DD",2,.362,23,0)
	;"  @ref@("^DD(2,.362)",23,1,0)  --> @ref@("^DD",2,.362,23,1,0)
	;"  @ref@("^DD(2,0,""IX"")","ACFL2",2,.312)  --> @ref@("^DD",2,0,"IX","ACFL2",2,.312)
	;"  @ref@("^DD(2,0,""IX"")","AEXP",2,.351)  --> @ref@("^DD",2,0,"IX","AEXP",2,.351)
	;"  @ref@("^DD(2,0,""IX"")","TMGS",2,22701)  --> @ref@("^DD",2,0,"IX","TMGS",2,22701)
	;"  @ref@("^DD(2,0,""PT"")",228.1,.02)  --> @ref@("^DD",2,0,"PT",228.1,.02)
	;"  @ref@("^DD(2,0,""PT"")",228.2,.02)  --> @ref@("^DD",2,0,"PT",228.2,.02)
	;"  @ref@("^DD(2,0,""PT"")",19620.92,.08)  --> @ref@("^DD",2,0,"PT",19620.92,.08)
	;"  @ref@("^DD(2,0,""PT"",115)",.01)  --> @ref@("^DD",2,0,"PT",115,.01)
	;"Input: ref -- PASS BY NAME
	;"Output: contents of @ref are converted as above.;
	;"Results: none
	;
	NEW origRef SET origRef=ref
	NEW output,s1,i
	FOR  SET ref=$query(@ref) QUIT:(ref="")  DO
	. SET s1=$qsubscript(ref,1)
	. NEW NEWRef SET NEWRef="output"
	. NEW startI SET startI=1
	. IF s1["(" DO
	. . SET startI=2
	. . SET NEWRef=NEWRef_"("""_$qs(s1,0)_""")"
	. . IF $qlength(s1)>1 FOR i=1:1:$qlength(s1) DO
	. . . SET NEWRef=$name(@NEWRef@($qsubscript(s1,i)))
	. FOR i=startI:1:$qlength(ref) DO
	. . NEW s3 SET s3=$qsubscript(ref,i)
	. . SET NEWRef=$name(@NEWRef@(s3))
	. MERGE @NEWRef=@ref
	;
	KILL @origRef
	MERGE @origRef=output  ;"put changes back into original array
	;
	QUIT
	;
Caller(Code) ;
	;"DEPRECIATED
	QUIT $$CALLER^TMGIOUT2
	;
UNHASH(X) ;" //Written 4/21/05.  Moved from XUSHSH 8/10/10
	;"Purpose: Unhasher of XUSHSH hashing routine
	;
	NEW result SET result=""
	NEW temp SET temp=""
	NEW digit SET digit=""
	NEW i
	;
	FOR i=1:1:$LENGTH(X) DO
	. SET digit=digit_$EXTRACT(X,i)
	. IF (+digit>31) DO
	. . SET temp=temp_$CHAR(digit)
	. . SET digit=""
	;
	FOR i=$LENGTH(temp):-1:1 DO
	. IF i#2 DO
	. . SET result=$EXTRACT(temp,1)_result      ;"get 1st char
	. . SET temp=$EXTRACT(temp,2,$LENGTH(temp))  ;"trim off 1st char
	. ELSE  DO
	. . SET result=$EXTRACT(temp,$LENGTH(temp))_result ;"get last char
	. . SET temp=$EXTRACT(temp,1,$LENGTH(temp)-1)      ;"trim last char
	;
	QUIT result
	;
TMGFLDS  ;
	;Purpose: To Show All Fields That Are TMG Namespaced
	NEW FILEIEN SET FILEIEN=0
	NEW FIELDNAME,DONE,PREFILE
	NEW %ZIS,IOP
	SET IOP="S121-LAUGHLIN-LASER"
	DO ^%ZIS  ;"standard device call
	IF POP DO  GOTO AMRDn
	. WRITE "ERROR OPENING DEVICE",!
	USE IO
	;
	WRITE "**********************************************************",!
	WRITE "********* LIST OF ALL TMG NAMESPACED FIELDS  *************",!
	WRITE "**********************************************************",!
	WRITE "",!
	FOR  SET FILEIEN=$ORDER(^DD(FILEIEN)) QUIT:(FILEIEN="")  DO
	. SET FIELDNAME="TMF"
	. SET PREFILE=0
	. SET DONE=0
	. FOR  SET FIELDNAME=$ORDER(^DD(FILEIEN,"B",FIELDNAME)) QUIT:(DONE)  DO
	. . IF FIELDNAME["TMG" DO
	. . . IF PREFILE'=FILEIEN DO
	. . . . WRITE "FILE: ",$ORDER(^DD(FILEIEN,0,"NM","")),"(",FILEIEN,")",!
	. . . . SET PREFILE=FILEIEN
	. . . WRITE "  FIELD: ",FIELDNAME,"(",$ORDER(^DD(FILEIEN,"B",FIELDNAME,0)),")",!
	. . ELSE  DO
	. . . SET DONE=1
	DO ^%ZISC ;"CLOSE THE OUTPUT DEVICE
AMRDn  QUIT
	;
GETALLMN(OUT) ;
	NEW AFILE SET AFILE=0
	FOR  SET AFILE=$ORDER(^DD(AFILE)) QUIT:+AFILE'>0  DO
	. DO GETMNEM(AFILE,.OUT)
	QUIT
	;
GETMNEM(FILE,OUT) ;  ;"Get list of mnemonix indexes for a file
	;"Purpose
	;"Input:  FILE -- file NUMBER to check
	;"  OUT -- PASS BY REFERENCE, AN OUT PARAMETER.  Format:
	;"       OUT(<File#>,<Field#InParent>,<Field#InSubfile>)=SubFile#
	;"Result: none.  OUT gets output
	NEW ASUBFILE SET ASUBFILE=0
	FOR  SET ASUBFILE=$ORDER(^DD(FILE,"SB",ASUBFILE)) QUIT:(+ASUBFILE'>0)  DO
	. NEW FLDINPARENT SET FLDINPARENT=+$ORDER(^DD(FILE,"SB",ASUBFILE,""))
	. NEW AFLD SET AFLD=0
	. FOR  SET AFLD=$ORDER(^DD(ASUBFILE,"IX",AFLD)) QUIT:(+AFLD'>0)  DO
	. . NEW IX SET IX=0
	. . FOR  SET IX=$ORDER(^DD(ASUBFILE,AFLD,1,IX)) QUIT:(+IX'>0)  DO
	. . . NEW ZN SET ZN=$GET(^DD(ASUBFILE,AFLD,1,IX,0))
	. . . IF $PIECE(ZN,"^",3)'["MNEMONIC" QUIT
	. . . SET OUT(FILE,FLDINPARENT,AFLD)=ASUBFILE
	. DO GETMNEM(ASUBFILE,.OUT)
	QUIT
	;
RAND(LO,HI) ;"Random Range
	QUIT $RANDOM(HI-LO+1)+LO   ;"E.G. 7,18 -> 18-7+1 = 12.  $R(12)->0..11, 0+7=7 11+7=18
	;
DELTA(INITVAL,DELTA,LO,HI) ;"Change INITVAL by DELTA, but clip at LO,HI (if provided)
	NEW RESULT SET RESULT=+$GET(INITVAL)+$GET(DELTA)
	IF $DATA(LO)#10>0,+LO=LO DO
	. IF RESULT<LO SET RESULT=LO
	IF $DATA(HI)#10>0,+HI=HI DO
	. IF RESULT>HI SET RESULT=HI
	QUIT RESULT
	;
INBOUNDS(NUM,LOW,HI) ;"Return number, within bounds
	IF LOW>HI NEW ZZ SET ZZ=HI,HI=LOW,LOW=ZZ
	NEW RESULT SET RESULT=$SELECT(NUM<LOW:LOW,NUM>HI:HI,1:NUM)
	QUIT RESULT
	;
MAX(NUM1,NUM2) ;"Return greater of 2 numbers
	NEW RESULT SET RESULT=$SELECT(NUM2>NUM1:NUM2,1:NUM1)
	QUIT RESULT
	;
MIN(NUM1,NUM2) ;"Return lessor of 2 numbers
	NEW RESULT SET RESULT=$SELECT(NUM2<NUM1:NUM2,1:NUM1)
	QUIT RESULT
	;
MERGESN(SRC,DEST) ;"Merge SUBNODES of SRC (but not value of SRC) into DEST
	NEW IDX SET IDX=""
	FOR  SET IDX=$ORDER(SRC(IDX)) QUIT:IDX=""  DO
	. MERGE DEST(IDX)=SRC(IDX)
	QUIT