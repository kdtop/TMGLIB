TMGRXU01 ;TMG/kst/Patient medication utility code; 04/15/18
       ;;1.0;TMG-LIB;**1**;04/15/18
 ;
 ;"CODE for dealing with drug CLASSes
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 04/15/18  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"TEST1  -- Test SelClass
 ;"TEST2  -- Test using PICKCLAS
 ;
 ;"PICKCLAS()   --Allow user to dynamically search and pick drug CLASS
 ;"SHOWCLSS  -- Show Classes
 ;"SELCLASS(ARR,ASKSUB) -- Allow user to browse ARR and select drug CLASS
 ;"SELFROM(REF)  -- Allow user to browse ARR and select drug CLASS
 ;"GTCLSSES(ARR) -- get an array back the shows the heirarchy of all VA DRUG CLASSes
 ;"GTCLHEIR(CLASSIEN,ARR)  -- get an array back the shows the heirarchy of one VA DRUG CLASS
 ;"FIXCLSES  -- Fix entries in the VA DRUG CLASS file where records are not properly linked into the heirarchy. 
 ;"GETINFO(IEN,ARR) -- fill record from VA DRUG CLASS file into a usable array
 ;"SRCHCLASS()  -- use Fileman to search for a drug CLASS
 ;"SRCHITMS(INPUT,ITEMS)  --Search through ITEMS array for INPUT, and return index number if found
 ;"GTRXCLASS()  -- Interact with user to pick a  medication, and return class from that med
 ;"GTCLSTR(IEN22733)  -- Get string showing drug class info.
 ;"RXSTR(IEN22733)  -- Get string showing drug class(es) and generic name
 ;"CLASSNAM(IEN50D605)  -- GET CLASS NAME FROM IEN
 ;"ISCLASS(CLASSIEN,REFCLASSIEN,ENCOUNTERED) -- Is CLASSIEN = REFCLASSIEN, or is one of it's children?
 ;"ISCLSS2(CLASSARR,REFCLASSIEN) --Does CLASSARR containt  REFCLASSIEN, or is one of it's children? 
 ;"GTCLARR(OUT,IEN22733) -- Get drug class information from one med record in 22733
 ;"GVPCLASS(OUT,IEN50D68) -- GET DRUG CLASSES FROM VA PRODUCT FILE
 ;"ADDCLASS(IEN50D605,IEN22733) -- Add a class to record in 22733
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"KILLINTR(ARR) -- One of the drug CLASSes is AA000, INTRODUCTION.  This will KILL this
 ;"FIX1CLASS(IEN) -- fix the parent entry of one erroneous CLASS, in the VA DRUG CLASS heirarchy.
 ;"PREPCLAR(OUT,ARR,INDENT) -- PREP ARRAY WITH DRUG CLASS INFO
 ;"GETCLASS(OUT,RXINFO) -- Extract drug class(es) based on RXINFO
 
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"=======================================================================
 ;"Uses:  
 ;"=======================================================================
  ;
TEST1   ;"Test SelClass
  NEW ARR,IEN
  DO GTCLSSES(.ARR)
  DO KILLINTR(.ARR)
  SET IEN=$$SELCLASS(.ARR,1)
  WRITE "IEN=",IEN,!
  QUIT
  ;   
TEST2  ;"Test selecting a class  
  WRITE $$PICKCLAS()
  QUIT 
  ;
TEST3 ;"Test getting a class from an existing medication  
  WRITE $$GTRXCLASS()  ;"Interact with user to pick a  medication, and return class from that med
  QUIT
  ;
SHOWCLSS  ;"Show Classes
  ;"Purpose: to display all the drug CLASSes, in a heirarchy.
  ;
  NEW ARR
  DO GTCLSSES(.ARR)
  DO ARRDUMP^TMGMISC3("ARR")
  QUIT
  ;
  ;"----------------------
  ;
GTCLSSES(ARR)  ;"Get Classes
  ;"Purpose: To get an array back the shows the heirarchy of all VA DRUG CLASSes
  ;"       ARR -- PASS BY REFERENCE, and OUT PARAMETER
  ;"Output: ARR will be filled as follows:
  ;"     ARR(GGF-IEN)=e.g. ""AD000^ANTIDOTES,DETERRENTS AND POISON CONTROL"
  ;"     ARR(GGF-IEN,GF-IEN)=e.g. "AD100^ALCOHOL DETERRENTS"
  ;"     ARR(GGF-IEN,GF-IEN,F-IEN)=e.g. "AD150^ALCOHOL DETERRENTS -- GENERAL TYPE 1"
  ;"     ARR(GGF-IEN,GF-IEN,F-IEN,IEN)=e.g. "AD152^ALCOHOL DETERRENTS -- GENERAL TYPE 1b"
  ;"  Note: prior entries in ARR are NOT killed.
  ;"Results: none
  ; 
  NEW IEN
  SET IEN=$ORDER(^PS(50.605,0))
  IF +IEN>0 FOR  DO  QUIT:(+IEN'>0)
  . DO GTCLHEIR(IEN,.ARR)
  . SET IEN=$ORDER(^PS(50.605,IEN))
  QUIT
  ;
KILLINTR(ARR)  ;
  ;"Purpose: One of the drug CLASSes is AA000, INTRODUCTION.  This will KILL this
  ;"        entry from the ARR
  ;"Input: ARR -- ARR, as created by GTCLSSES
  NEW IEN SET IEN=$ORDER(ARR(""))
  IF IEN'="" FOR  DO  QUIT:(IEN="")
  . NEW TEMP SET TEMP=IEN
  . SET IEN=$ORDER(ARR(IEN))
  . IF $PIECE(ARR(TEMP),"^",1)="AA000" KILL ARR(TEMP)
  QUIT
  ;             
GTCLHEIR(CLASSIEN,ARR)     ;"GetClHeirarchy
  ;"Purpose: To get an array back the shows the heirarchy of one VA DRUG CLASS
  ;"Input: CLASSIEN -- the IEN in file VA DRUG CLASS (50.605)
  ;"       ARR -- PASS BY REFERENCE, and OUT PARAMETER
  ;"Output: ARR will be filled as follows:
  ;"     ARR(GGF-IEN)=e.g. ""AD000^ANTIDOTES,DETERRENTS AND POISON CONTROL"
  ;"     ARR(GGF-IEN,GF-IEN)=e.g. "AD100^ALCOHOL DETERRENTS"
  ;"     ARR(GGF-IEN,GF-IEN,F-IEN)=e.g. "AD150^ALCOHOL DETERRENTS -- GENERAL TYPE 1"
  ;"     ARR(GGF-IEN,GF-IEN,F-IEN,IEN)=e.g. "AD152^ALCOHOL DETERRENTS -- GENERAL TYPE 1b"
  ;"  Note: prior entries in ARR are NOT killed.
  ;"Results: none
  NEW PARENTCLASS,INDENT
  NEW RESULTARR
  IF (+CLASSIEN'=0) FOR  DO  QUIT:(+CLASSIEN=0)
  . NEW TEMPARR
  . IF $DATA(RESULTARR) do
  . . NEW TEMP MERGE TEMP=RESULTARR
  . . KILL RESULTARR
  . . MERGE RESULTARR(CLASSIEN)=TEMP
  . NEW CURNODE,CODE,NAME,CODENUM
  . SET CURNODE=$GET(^PS(50.605,CLASSIEN,0))
  . SET CODE=$PIECE(CURNODE,"^",1)
  . SET CODENUM=+$EXTRACT(CODE,3,5)
  . SET NAME=$PIECE(CURNODE,"^",2)
  . SET TEMPARR(CLASSIEN)=CODE_"^"_NAME
  . SET PARENTCLASS=$PIECE(CURNODE,"^",3)
  . IF PARENTCLASS=CLASSIEN SET PARENTCLASS=0  ;"I found at least one circular ref.
  . IF (PARENTCLASS=0)&(CODENUM'=0) do
  . . WRITE IEN,":  ",NAME," appears broken: ",CODE," Will fix...",!
  . . DO FIX1CLASS(IEN)
  . SET CLASSIEN=PARENTCLASS
  . MERGE RESULTARR=TEMPARR
  MERGE ARR=RESULTARR
  QUIT
  ;
FIXCLSES  ;"FixClasses
  ;"Purpose: I have found a few instances in the VA DRUG CLASS file where records are
  ;"   not properly linked into the heirarchy.  They either give themselves as
  ;"   their own parents, or list no parent, though one should be present.
  ;"   If any such entries exist, this function will fix them.
  NEW IEN SET IEN=$ORDER(^PS(50.605,0))
  IF +IEN>0 FOR  DO  QUIT:(+IEN'>0)
  . NEW CURNODE,CODE,CODENUM,NAME
  . SET CURNODE=$GET(^PS(50.605,IEN,0))
  . SET CODE=$PIECE(CURNODE,"^",1)
  . SET CODENUM=+$EXTRACT(CODE,3,5)
  . SET NAME=$PIECE(CURNODE,"^",2)
  . SET PARENTCLASS=+$PIECE(CURNODE,"^",3)
  . IF PARENTCLASS=IEN SET PARENTCLASS=0
  . IF (PARENTCLASS=0)&(CODENUM'=0) do
  . . WRITE IEN,":  ",NAME," appears broken: ",CODE," Will fix...",!
  . . IF $GET(TMGFIX)=1 DO FIX1CLASS(IEN)
  . SET IEN=$ORDER(^PS(50.605,IEN))
  QUIT
  ;
FIX1CLASS(IEN)  ;
  ;"Purpose: To fix the parent entry of one erroneous CLASS, in the VA DRUG CLASS heirarchy.
  ;"Input: IEN -- the record number in VA DRUG CLASS to fix
  ;"Output: the database will be changed
  ;"Results: none.
  NEW CURNODE,CODE,CODENUM,PARENTCODE
  NEW PARENTCLASS,NEWPARENTCLASS
  SET CURNODE=$GET(^PS(50.605,IEN,0))
  SET CODE=$PIECE(CURNODE,"^",1)
  SET PARENTCLASS=+$PIECE(CURNODE,"^",3)
  SET PARENTCODE=$EXTRACT(CODE,1,2)_"000"
  SET NEWPARENTCLASS=+$ORDER(^PS(50.605,"B",PARENTCODE,""))
  IF NEWPARENTCLASS'=0 DO
  . SET $PIECE(^PS(50.605,IEN,0),"^",3)=NEWPARENTCLASS
  QUIT
  ;
GETINFO(IEN,ARR)  ;
  ;"Purpose: to fill record from VA DRUG CLASS file into a usable array
  ;"Input: IEN -- the IEN from VA DRUG CLASS file to get INFO for
  ;"       ARR -- PASS BY REFERENCE, to be filled in with data.  Old data is KILLED.
  ;"Output: ARR is filled with data:
  ;"        ARR("NAME")=NAME
  ;"        ARR("CODE")=code
  ;"        ARR("PARENT IEN")=parent IEN
  ;"Result: none
  NEW CURNODE
  KILL ARR
  SET CURNODE=$GET(^PS(50.605,IEN,0))
  SET ARR("CODE")=$PIECE(CURNODE,"^",1)
  SET ARR("NAME")=$PIECE(CURNODE,"^",2)
  SET ARR("PARENT IEN")=+$PIECE(CURNODE,"^",3)
  QUIT
  ;
SELCLASS(ARR,ASKSUB)  ;
  ;"Purpose: Allow user to browse ARR and select drug CLASS
  ;"Input: ARR -- An ARR containing Drug Class INFO, as created by GTCLSSES()
  ;"       ASKSUB -- OPTIONAL.  If 1, user is asked if they want to browse sub-CLASS (auto otherwise)
  ;"Results: Returns IEN of selected CLASS, or 0 IF not selected
  NEW IEN,DONE
  SET DONE=0
  SET ASKSUB=$GET(ASKSUB,0) ;"default=automatic browse of subCLASSes
  NEW REF SET REF=$NAME(ARR)
  FOR  DO  QUIT:(DONE=1)
  . SET IEN=$$SELFROM(REF)
  . IF IEN=0 DO  QUIT
  . . IF $QLENGTH(REF)>0 DO
  . . . SET REF=$NAME(@REF,$QLENGTH(REF)-1)
  . . ELSE  SET DONE=1
  . NEW SKIPSUB SET SKIPSUB=0
  . IF (ASKSUB=1)&($DATA(ARR(IEN))>1) DO
  . . NEW %
  . . WRITE "Browse sub-categories"
  . . SET %=1 DO YN^DICN WRITE !
  . . IF %'=1 SET SKIPSUB=1
  . IF ($DATA(ARR(IEN))>1)&(SKIPSUB=0) SET REF=$NAME(@REF@(IEN))
  . ELSE  DO
  . . NEW INFO,%
  . . DO GETINFO(IEN,.INFO)
  . . WRITE "Select: ",INFO("NAME")
  . . SET %=1 DO YN^DICN WRITE !
  . . IF %=1 SET DONE=1
  QUIT IEN
  ;
SRCHCLASS()  ;
  ;"Purpose: to use Fileman to search for a drug CLASS
  ;"Results: Returns IEN of selected CLASS, or 0 IF not selected
  NEW DIC,X,Y
  SET DIC=50.605
  SET DIC(0)="AEQM"
  SET DIC("A")="Enter a DRUG CLASS to search for // "
  DO ^DIC WRITE !
  NEW RESULT SET RESULT=0
  IF +Y>0 SET RESULT=+Y
  QUIT RESULT
  ;
SELFROM(REF)   ;
  ;"Purpose: Allow user to browse ARR and select drug CLASS
  ;"Input: REF -- NAME OF part of array to browse, containing Drug Class INFO
  ;"Results: Returns IEN of selected CLASS, or 0 IF not selected
  NEW TEMPLIST,ITEMS,ANSWERS,NAME
  NEW I,COUNT
  NEW RESULT SET RESULT=0
  SET IDX=""
  FOR  SET IDX=$ORDER(@REF@(IDX)) QUIT:(+IDX'>0)  DO
  . SET NAME=$PIECE($GET(@REF@(IDX)),"^",2) QUIT:(NAME="")
  . NEW CLASS SET CLASS=$PIECE($GET(@REF@(IDX)),"^",1) QUIT:(CLASS="")
  . SET TEMPLIST(NAME)=IDX
  . SET TEMPLIST(NAME,CLASS)=""
  ;
  SET COUNT=1
  SET NAME=$ORDER(TEMPLIST(""))
  IF NAME'="" FOR  DO  QUIT:(NAME="")
  . SET ITEMS(COUNT)=NAME
  . SET ITEMS(COUNT,"CLASS")=$ORDER(TEMPLIST(NAME,""))
  . SET ANSWERS(COUNT)=$GET(TEMPLIST(NAME))
  . SET COUNT=COUNT+1
  . SET NAME=$ORDER(TEMPLIST(NAME))
  ;
  NEW DONE SET DONE=0
  FOR  DO  QUIT:(DONE=1)
  . NEW NAME SET NAME=$PIECE($GET(@REF),"^",2)
  . IF NAME="" SET NAME="Major Drug Classes"
  . WRITE !,"Select from one of these ",NAME,!
  . SET IDX=$ORDER(ITEMS(0))
  . IF +IDX>0 FOR  DO  QUIT:(+IDX'>0)
  . . WRITE IDX,".  "
  . . NEW CLASS SET CLASS=$GET(ITEMS(IDX,"CLASS"))
  . . IF CLASS'="" WRITE CLASS,": "
  . . WRITE ITEMS(IDX),!
  . . SET IDX=$ORDER(ITEMS(IDX))
  . WRITE !,"Enter # of Drug Class to Pick (^ to Backup, S to Search): ^// "
  . NEW INPUT
  . READ INPUT:$GET(DTIME,3600),!
  . SET INPUT=$$UP^XLFSTR(INPUT)
  . IF INPUT="" SET INPUT="^"
  . IF INPUT="S" DO  QUIT:(DONE=1)
  . . NEW USRIEN SET USRIEN=$$SRCHCLASS
  . . IF USRIEN>0 SET RESULT=USRIEN,DONE=1
  . IF INPUT="" SET INPUT="^"
  . IF INPUT="^" SET DONE=1 QUIT
  . IF +INPUT=INPUT DO
  . . SET RESULT=ANSWERS(INPUT)
  . . SET DONE=1
  . ELSE  DO
  . . NEW TEMP SET TEMP=$$SRCHITMS(INPUT,.ITEMS)
  . . IF +TEMP>0 SET RESULT=ANSWERS(TEMP),DONE=1
  . . ELSE  WRITE "Invalid INPUT.  Please try again.",!
  QUIT RESULT
  ;
SRCHITMS(INPUT,ITEMS)  ;SrchItems
  ;"Purpose: to Search through ITEMS array for INPUT, and return index number IF found
  ;"Input:  INPUT -- the user INPUT -- may be a partial match for the NAME.
  ;"  ITEMS -- PASS BY REFERENCE -- Input array, as created in SELFROM()
  ;"        ITEMS(1)=value
  ;"        ITEMS(2)=value
  ;"        ITEMS(3)=value
  ;"Result: returns index of the FIRST match
  NEW RESULT SET RESULT=""
  NEW DONE SET DONE=0
  NEW VALUE
  SET INPUT=$$UP^XLFSTR($GET(INPUT))
  NEW IDX SET IDX=$ORDER(ITEMS(""))
  IF IDX'="" FOR  DO  QUIT:(IDX="")!(DONE=1)
  . SET VALUE=$GET(ITEMS(IDX))
  . SET VALUE=$EXTRACT(VALUE,1,$LENGTH(INPUT))
  . IF INPUT=VALUE SET RESULT=IDX,DONE=1
  . SET IDX=$ORDER(ITEMS(IDX))
  QUIT RESULT
  ;
  ;"=====================================================
PICKCLAS()   ;"Allow user to dynamically search and pick drug CLASS
  ;"Result: IEN^<CODE>^NAME if selected, otherwise ""
  NEW MENU,IDX,LISTCT
  NEW RESULT SET RESULT=""
  NEW DEFAULT SET DEFAULT="^"
PCKL1 ;          
  SET IDX=0  
  KILL MENU SET MENU(IDX)="Select Option For Picking or Viewing Drug CLASSES"
  SET IDX=IDX+1,MENU(IDX)="Search drug class names"_$CHAR(9)_"SRCH"
  SET IDX=IDX+1,MENU(IDX)="Pick an existing VA PRODUCT for linked drug class"_$CHAR(9)_"PICK"
  IF RESULT'="" SET IDX=IDX+1,MENU(IDX)="USE class: "_$PIECE(RESULT,"^",2,3)_$CHAR(9)_"DONE",DEFAULT=IDX
  ELSE  SET DEFAULT="^"
  SET USRPICK=$$MENU^TMGUSRI2(.MENU,DEFAULT)
  IF USRPICK="^" SET RESULT="" GOTO PCKCLDN  
  IF USRPICK="SRCH" DO  GOTO PCKL1
  . SET RESULT=$$PICKCLA1()
  IF USRPICK="PICK" DO  GOTO PCKL1
  . SET RESULT=$$PICKCLA2()
  IF USRPICK="DONE" GOTO PCKCLDN
  GOTO PCKL1
PCKCLDN ;
  QUIT RESULT
  ;
PICKCLA1()   ;"Allow user to dynamically search and pick drug CLASS
  ;"Result: IEN^<CODE>^NAME if selected, otherwise ""
  NEW ARR DO PREPCLA1(.ARR)
  NEW OPTION SET OPTION("NCS")=1,OPTION("HEADER")="Select Drug Class"
  NEW RESULT SET RESULT=$$LISTSEL^TMGUSRI4("ARR",.OPTION)
  WRITE #
  QUIT RESULT
  ;
PREPCLA1(OUT,ARR,INDENT)  ;"PREP ARRAY WITH DRUG CLASS INFO
  ;"INPUT: OUT -- pass by reference.  An OUT parameter. Format:
  ;"         OUT(<Display Text>)=IEN50.605
  ;"       ARR -- OPTIONAL.  IF EMPTY, THEN ALL CLASSES WILL BE USED
  ;"       INDENT -- OPTIONAL.  LENGTH OF INDENT.  USED WHEN CALLING SELF RECURSIVELY.
  ;"Result: IEN^<CODE>^NAME if selected, otherwise ""
  IF $DATA(ARR)=0 DO GTCLSSES(.ARR)  ;"Format: ARR(IEN)=<Code>^<Name>
  SET INDENT=+$GET(INDENT)
  NEW INDENTSTR SET INDENTSTR=$EXTRACT("                             ",1,INDENT)
  NEW IEN SET IEN=0
  FOR  SET IEN=$ORDER(ARR(IEN)) QUIT:IEN'>0  DO
  . NEW LINE SET LINE=$GET(ARR(IEN)) QUIT:LINE=""
  . NEW CT SET CT=$ORDER(OUT(""),-1)+1
  . NEW CODE SET CODE=$PIECE(LINE,"^",1)
  . NEW NAME SET NAME=$PIECE(LINE,"^",2)
  . NEW SHOWLINE SET SHOWLINE=$$RJ^XLFSTR(CT,3,0)_" "_INDENTSTR
  . SET SHOWLINE=SHOWLINE_CODE_": "_NAME
  . SET OUT(SHOWLINE)=IEN_"^"_LINE
  . IF $DATA(ARR(IEN))>1 DO
  . . NEW TEMP MERGE TEMP=ARR(IEN)
  . . DO PREPCLA1(.OUT,.TEMP,INDENT+2)
  QUIT
  ;
PICKCLA2()  ;"Allow user to dynamically search for a VA PRODUCT file entry, that has desired drug CLASS
  ;"Result: IEN^<CODE>^NAME if selected, otherwise ""  
  NEW RESULT SET RESULT=""
  NEW ARR DO PREPCLA2(.ARR)
  IF $DATA(ARR)=0 GOTO PCL2DN
  NEW OPTION SET OPTION("NCS")=1,OPTION("HEADER")="Select Drug With Desired DRUG CLASS"
  NEW IEN50D68 SET IEN50D68=$$LISTSEL^TMGUSRI4("ARR",.OPTION)
  IF IEN50D68'>0 GOTO PCL2DN
  NEW TEMP DO GVPCLASS(.TEMP,IEN50D68)
  NEW IENV50D605 SET IEN50D605=+$ORDER(TEMP("CLASS",0))
  NEW NAME SET NAME=$GET(TEMP("CLASS",IEN50D605))
  SET RESULT=IEN50D605_"^"_NAME
PCL2DN ;  
  QUIT RESULT
  ;
PREPCLA2(OUT)  ;"PREP ARRAY WITH VA PRODUCT DRUG INFO
  ;"INPUT: OUT -- pass by reference.  An OUT parameter. Format:
  ;"         OUT(<Display Text>)=IEN50.68
  NEW IEN50D68 SET IEN50D68=0
  FOR  SET IEN50D68=$ORDER(^PSNDF(50.68,IEN50D68)) QUIT:IEN50D68'>0  DO
  . NEW ZN SET ZN=$GET(^PSNDF(50.68,IEN50D68,0))
  . NEW NAME SET NAME=$PIECE(ZN,"^",1) QUIT:NAME=""
  . NEW PTR SET PTR=+$PIECE(ZN,"^",2)
  . NEW GENERIC SET GENERIC=$PIECE($GET(^PSNDF(50.6,PTR,0)),"^",1)
  . NEW LINE SET LINE=NAME
  . IF (GENERIC'=""),(LINE'[GENERIC) SET LINE=LINE_" / "_GENERIC
  . SET OUT(LINE)=IEN50D68
  QUIT
  ;  
GTRXCLASS()  ;"Interact with user to pick a  medication, and return class from that med
  NEW TMGRESULT SET TMGRESULT=""
  WRITE #
  WRITE !,"Prepairing to choose medication with desired class..."
  NEW IEN50D68 SET IEN50D68=+$$RECSEL^TMGUSRI4(50.68)
  IF IEN50D68'>0 GOTO GRXCDN
  NEW IEN50D605 SET IEN50D605=$PIECE($GET(^PSNDF(50.68,IEN50D68,3)),"^",1)
  NEW ZN SET ZN=$GET(^PS(50.605,IEN50D605,0))  ;"e.g. AD000^ANTIDOTES,DETERRENTS AND POISON CONTROL^^0
  SET TMGRESULT=IEN50D605_"^"_$PIECE(ZN,"^",1,2)
GRXCDN ;  
  WRITE #
  QUIT TMGRESULT
  ;
GETCLASS(OUT,RXINFO) ;"Extract drug class(es) based on RXINFO
  ;"Input: OUT -- PASS BY REFERENCE.  Format: 
  ;"          OUT("LINE",<original Rx table line>,IEN50.605)=""
  ;"          OUT("CLASS",IEN50.605)=<CLASS_NAME>^<External Description>  (VA drug class file)
  ;"          OUT("CLASS",IEN50.605,<original Rx table line>)=""
  ;"       RXINFO -- PASS BY REFERENCE.  Format: See PARSELN^TMGRX001
  ;"           RXINFO("ORIG")
  ;"           RXINFO("IEN22733")
  ;"           ... other stuff not used
  ;"Result: none
  NEW LINE SET LINE=$GET(RXINFO("ORIG")) QUIT:LINE=""
  NEW IEN22733 SET IEN22733=+$GET(RXINFO("IEN22733"))  QUIT:IEN22733'>0
  ;"TO DO, CHANGE TO USE RXINFO("DRUG CLASS")...
  NEW ARR DO GTCLARR(.ARR,IEN22733)
  NEW IEN50D605 SET IEN50D605=0
  FOR  SET IEN50D605=$ORDER(ARR(IEN50D605)) QUIT:IEN50D605'>0  DO
  . NEW NAME SET NAME=$GET(ARR(IEN50D605)) QUIT:NAME=""
  . SET OUT("CLASS",IEN50D605)=NAME
  . SET OUT("CLASS",IEN50D605,LINE)=""
  . SET OUT("LINE",LINE,IEN50D605)=""
  ;
  ;"OLD- ;"Get classes from LINKED DRUG CLASSES field multiple 
  ;"OLD- NEW SUBIEN SET SUBIEN=0
  ;"OLD- FOR  SET SUBIEN=$ORDER(^TMG(22733,IEN22733,4,SUBIEN))  QUIT:SUBIEN'>0  DO
  ;"OLD- . NEW IEN50D605 SET IEN50D605=+$PIECE($GET(^TMG(22733,IEN22733,4,SUBIEN,0)),"^",1)
  ;"OLD- . IF IEN50D605'>0 QUIT
  ;"OLD- . NEW NAME SET NAME=$GET(OUT(IEN50D605)) 
  ;"OLD- . IF NAME="" SET NAME=$$CLASSNAM(IEN50D605)  QUIT:NAME=""
  ;"OLD- . SET OUT("CLASS",IEN50D605)=NAME
  ;"OLD- . SET OUT("CLASS",IEN50D605,LINE)=""  
  ;"OLD- . SET OUT("LINE",LINE,IEN50D605)=""
  ;"OLD- ;"Next, get classes linked VA PRODUCT entries in the MEDICATION FORMS field multiple
  ;"OLD- SET SUBIEN=0
  ;"OLD- FOR  SET SUBIEN=$ORDER(^TMG(22733,IEN22733,2,SUBIEN))  QUIT:SUBIEN'>0  DO
  ;"OLD- . NEW SUBSUBIEN SET SUBSUBIEN=0
  ;"OLD- . FOR  SET SUBSUBIEN=$ORDER(^TMG(22733,IEN22733,2,SUBIEN,1,SUBSUBIEN))  QUIT:SUBSUBIEN'>0  DO
  ;"OLD- . . NEW ZN SET ZN=$GET(^TMG(22733,IEN22733,2,SUBIEN,1,SUBSUBIEN,0))
  ;"OLD- . . NEW IEN50D68 SET IEN50D68=+$PIECE(ZN,"^",3)  QUIT:IEN50D68'>0
  ;"OLD- . . NEW TEMP DO GVPCLASS(.TEMP,IEN50D68)
  ;"OLD- . . NEW IEN50D605 SET IEN50D605=0
  ;"OLD- . . FOR  SET IEN50D605=$ORDER(TEMP("CLASS",IEN50D605)) QUIT:IEN50D605'>0  DO
  ;"OLD- . . . NEW NAME SET NAME=$GET(TEMP("CLASS",IEN50D605)) QUIT:NAME="" 
  ;"OLD- . . . SET OUT("CLASS",IEN50D605)=NAME
  ;"OLD- . . . SET OUT("CLASS",IEN50D605,LINE)=""  
  ;"OLD- . . . SET OUT("LINE",LINE,IEN50D605)=""
  ;
  QUIT
  ;
GTCLARR(OUT,IEN22733)  ;"Get drug class information from one med record in 22733
  ;"Input: OUT -- PASS BY REFERENCE.  AN OUT PARAMETER.  FORMAT:
  ;"            OUT(IEN50.605)= <CODE>^<DISPLAY NAME>
  ;"       IEN22733 -- IEN IN 22733
  ;"Result: none
  ;
  ;"Get classes from LINKED DRUG CLASSES field multiple 
  NEW SUBIEN SET SUBIEN=0
  FOR  SET SUBIEN=$ORDER(^TMG(22733,IEN22733,4,SUBIEN))  QUIT:SUBIEN'>0  DO
  . NEW IEN50D605 SET IEN50D605=+$PIECE($GET(^TMG(22733,IEN22733,4,SUBIEN,0)),"^",1)
  . IF IEN50D605'>0 QUIT
  . NEW NAME SET NAME=$GET(OUT(IEN50D605)) 
  . IF NAME="" SET NAME=$$CLASSNAM(IEN50D605)  QUIT:NAME=""
  . SET OUT(IEN50D605)=NAME
  ;"Next, get classes linked VA PRODUCT entries in the MEDICATION FORMS field multiple
  SET SUBIEN=0
  FOR  SET SUBIEN=$ORDER(^TMG(22733,IEN22733,2,SUBIEN))  QUIT:SUBIEN'>0  DO
  . NEW SUBSUBIEN SET SUBSUBIEN=0
  . FOR  SET SUBSUBIEN=$ORDER(^TMG(22733,IEN22733,2,SUBIEN,1,SUBSUBIEN))  QUIT:SUBSUBIEN'>0  DO
  . . NEW ZN SET ZN=$GET(^TMG(22733,IEN22733,2,SUBIEN,1,SUBSUBIEN,0))
  . . NEW IEN50D68 SET IEN50D68=+$PIECE(ZN,"^",3)  QUIT:IEN50D68'>0
  . . NEW TEMP DO GVPCLASS(.TEMP,IEN50D68)
  . . NEW IEN50D605 SET IEN50D605=0
  . . FOR  SET IEN50D605=$ORDER(TEMP("CLASS",IEN50D605)) QUIT:IEN50D605'>0  DO
  . . . NEW NAME SET NAME=$GET(TEMP("CLASS",IEN50D605)) QUIT:NAME="" 
  . . . SET OUT(IEN50D605)=NAME
  QUIT
  ;
GTCLSTR(IEN22733)  ;"get string showing drug class info.
  NEW ARR DO GTCLARR(.ARR,IEN22733)
  NEW STR SET STR=""
  NEW IEN50D605 SET IEN50D605=0
  FOR  SET IEN50D605=$ORDER(ARR(IEN50D605)) QUIT:IEN50D605'>0  DO
  . NEW NAME SET NAME=$GET(ARR(IEN50D605)) QUIT:NAME=""
  . IF STR'="" SET STR=STR_","
  . SET STR=STR_$PIECE(NAME,"^",1)
  IF STR="" SET STR="     "
  SET STR="["_STR_"]"
  QUIT STR
  ;
RXSTR(IEN22733) ;"Get string showing drug class(es) and generic name
  NEW CLSTR SET CLSTR=$$GTCLSTR(IEN22733)
  NEW NAME SET NAME=$PIECE($GET(^TMG(22733,IEN22733,0)),"^",1)
  NEW RESULT SET RESULT=CLSTR_" "_NAME
  QUIT RESULT
  ;  
CLASSNAM(IEN50D605)  ;"GET CLASS NAME FROM IEN
  ;"RESULT: <CODE>^<DISPLAY NAME>
  NEW NAME SET NAME=$PIECE($GET(^PS(50.605,IEN50D605,0)),"^",1,2) 
  IF NAME="^" SET NAME=""
  QUIT NAME
  ;
ISCLASS(CLASSIEN,REFCLASSIEN,ENCOUNTERED)  ;"Is CLASSIEN = REFCLASSIEN, or is one of it's children?
  ;"Input: CLASSIEN -- the IEN to test
  ;"       REFCLASSIEN -- the IEN to see if CLASSIEN equals, or is a child of
  ;"       ENOUNTERED -- OPTIONAL.  An array to prevent endless loops, used when calling self recursively
  ;"Result: 0 or 1
  NEW RESULT SET RESULT=0
  IF CLASSIEN=REFCLASSIEN SET RESULT=1 GOTO ICLDN
  SET ENCOUNTERED(CLASSIEN)=1
  NEW PARENTIEN SET PARENTIEN=$PIECE($GET(^PS(50.605,CLASSIEN,0)),"^",3)
  IF PARENTIEN'>0 GOTO ICLDN
  IF $DATA(ENCOUNTERED(PARENTIEN)) GOTO ICLDN  ;"Abort potential endless loop
  SET RESULT=$$ISCLASS(PARENTIEN,REFCLASSIEN,.ENCOUNTERED)
ICLDN ;
  QUIT RESULT
  ;
ISCLSS2(CLASSARR,REFCLASSIEN) ;"Does CLASSARR containt  REFCLASSIEN, or is one of it's children?
  ;"Result: 0 or 1
  NEW RESULT SET RESULT=0
  NEW IEN50D605 SET IEN50D605=0
  FOR  SET IEN50D605=$ORDER(CLASSARR(IEN50D605)) QUIT:(IEN50D605'>0)!(RESULT=1)  DO
  . SET RESULT=$$ISCLASS(IEN50D605,REFCLASSIEN)
  QUIT RESULT
  ;
GVPCLASS(OUT,IEN50D68)  ;"GET DRUG CLASSES FROM SPECIFIED VA PRODUCT FILE RECORD  
  ;"Input: OUT -- PASS BY REFERENCE. Format:
  ;"          OUT("CLASS",IEN50D605)=NAME
  ;"       IEN50D608 -- IEN 50.608
  ;"Result: none
  ;"Get PRIMARY VA DRUG CLASS
  NEW IEN50D605 SET IEN50D605=+$PIECE($GET(^PSNDF(50.68,IEN50D68,3)),"^",1)
  IF IEN50D605>0 DO
  . NEW NAME SET NAME=$GET(OUT(IEN50D605)) 
  . IF NAME="" SET NAME=$$CLASSNAM(IEN50D605) QUIT:NAME=""
  . SET OUT("CLASS",IEN50D605)=NAME
  ;"Get SECONDARY DRUG CLASS
  NEW SUBIEN SET SUBIEN=0
  FOR  SET SUBIEN=$ORDER(^PSNDF(50.68,IEN50D68,4,SUBIEN)) QUIT:SUBIEN'>0  DO
  . SET IEN50D605=+$PIECE($GET(^PSNDF(50.68,IEN50D68,4,SUBIEN,0)),"^",1)
  . IF IEN50D605'>0 QUIT
  . NEW NAME SET NAME=$GET(OUT(IEN50D605)) 
  . IF NAME="" SET NAME=$$CLASSNAM(IEN50D605) QUIT:NAME=""
  QUIT
  ;    
ADDCLASS(IEN50D605,IEN22733) ;"Add a class to record in 22733
  ;"Result: 1^OK, or -1^ErrorMessage
  NEW TMGRESULT SET TMGRESULT="1^OK"
  SET IEN50D605=+$GET(IEN50D605)
  IF IEN50D605'>0 DO  GOTO ACLDN
  . SET TMGRESULT="-1^Valid IEN50D605 not provided as input"  
  SET IEN22733=+$GET(IEN22733)
  IF IEN22733'>0 DO  GOTO ACLDN
  . SET TMGRESULT="-1^Valid IEN22733 not provided as input"
  NEW TEMP DO GTCLARR(.TEMP,IEN22733)  ;"Get drug class information from one med record in 22733
  IF $DATA(TEMP(IEN50D605)) GOTO ACLDN  ;"Class already present, so don't add
  NEW TMGFDA,TMGIEN,TMGMSG
  SET TMGFDA(22733.06,"+1,"_IEN22733_",",.01)=IEN50D605
  DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO ACLDN
  . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)  
ACLDN ;
  QUIT TMGRESULT
  