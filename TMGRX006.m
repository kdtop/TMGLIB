TMGRX006 ;TMG/kst/Patient medication code; 04/13/18, 3/24/21
       ;;1.0;TMG-LIB;**1**;04/13/18
 ;
 ;"Code for dealing with drug classes from medication list
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 04/13/18  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"CONSOLE -- A console for testing / demo functions
 ;"TEST1  -- TEST THE PARSED MED LIST FOR A PATIENT 
 ;"TEST2(TMGDFN)-- TEST SHOWING CLASSES OF ALL MEDS OF A GIVEN PATIENT
 ;"GETCLARX(OUT,CLASSIENARR,TMGDFN) --  Get a list of patients most recent medications, belonging to specified VA DRUG classes  
 ;"GETCLSRX(OUT,IEN,TMGDFN) -- Get a list of patients most recent medications, belonging to 1 specified VA DRUG class  
 ;"HNDLCLSS(LIST) -- HANDLE CLASSES -- called from menu in CONSOLE^TMGRX002
 ;"EDTTBLCL(IEN22708) --Interact with user and edit RELATED MED CLASSES field in TMG TIU PXRM TABLE (22708) file
 ;"PKEDTBLC() -- Interact with user, picking  TMG TIU PXRM TABLE, then editing RELATED MED CLASSES field 
 ;" 
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"PARSEARR(OUT,ARR)  -- PARSE A MED LIST  
 ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"=======================================================================
 ;"Uses:  
 ;"=======================================================================
 ;    
CONSOLE  ;
  NEW IDX,MENU
CSL1 ;          
  SET IDX=0  
  KILL MENU SET MENU(IDX)="Select Option For Testing / Demo of Drug Classes"
  SET IDX=IDX+1,MENU(IDX)="TEST parsing med list for 1 patient"_$CHAR(9)_"TEST1"
  SET IDX=IDX+1,MENU(IDX)="SHOW meds matching classes for a given patient"_$CHAR(9)_"TEST3"
  SET IDX=IDX+1,MENU(IDX)="BROWSE classes"_$CHAR(9)_"CLASSBROWSE"
  SET IDX=IDX+1,MENU(IDX)="SHOW members of a class"_$CHAR(9)_"SHOWMEMBERS"
  SET IDX=IDX+1,MENU(IDX)="TABLE edit of RELATED MED CLASSES"_$CHAR(9)_"TABLECLASSES"
  SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
  IF USRPICK="^" GOTO CSDN  
  IF USRPICK="CLASSBROWSE" DO  GOTO CSL1
  . IF $$PICKCLAS^TMGRXU01  ;"IGNORE PICKED CLASS    
  IF USRPICK="SHOWMEMBERS" DO  GOTO CSL1
  . DO SHOWCMBR() ;"SHOW MEMBERS OF VA DRUG CLASS (including any children classes)
  IF USRPICK="TABLECLASSES" DO  GOTO CSL1
  . DO PKEDTBLC()  ;"Pick and edit table classes
  IF USRPICK="TEST1" DO  GOTO CSL1
  . DO TEST1
  IF USRPICK="TEST3" DO  GOTO CSL1
  . DO TEST3
  GOTO CSL1
CSDN  ;
  QUIT
  ;
TEST1 ; "TEST THE PARSED MED LIST FOR A PATIENT 
  NEW X,Y,DIC
TL1 ;  
  SET (X,Y)="",DIC=2,DIC(0)="MAEQ"
  DO ^DIC WRITE !
  IF +Y'>0 QUIT
  NEW TMGDFN SET TMGDFN=+Y
  DO TEST2(TMGDFN)
  SET %=1 WRITE !,"Try another patient" DO YN^DICN WRITE !
  IF %=1 GOTO TL1
  QUIT
  ;
TEST2(TMGDFN) ;"TEST SHOWING CLASSES OF ALL MEDS OF A GIVEN PATIENT
  NEW ARR,TEMP
  ;"DO MEDLIST^TMGTIUOJ(.TEMP,TMGDFN,.ARR)
  DO MEDARR^TMGTIUOJ(.TEMP,TMGDFN,.ARR)
  NEW OUT
  DO PARSEARR(.OUT,.ARR) 
  IF $DATA(OUT) ZWR OUT
  QUIT
  ;
TEST3()  ;"Test picking patient, and desired class, and getting back matches  
  NEW X,Y,DIC,TMGDFN,IEN50D605,OUT
TL3 ;  
  KILL OUT
  SET (X,Y)="",DIC=2,DIC(0)="MAEQ"
  DO ^DIC WRITE !
  IF +Y'>0 QUIT
  SET TMGDFN=+Y
  SET IEN50D605=$$PICKCLAS^TMGRXU01()
  IF IEN50D605'>0 QUIT
  DO GETCLSRX(.OUT,+IEN50D605,TMGDFN)
  IF $DATA(OUT) DO
  . ZWR OUT
  ELSE  WRITE "NONE.",!
  WRITE !
  SET %=1 WRITE !,"Try again" DO YN^DICN WRITE !
  IF %=1 GOTO TL3
  QUIT
  ;
  ;"============================================
GETCLARX(OUT,CLASSIENARR,TMGDFN,OPTION) ;"Get a list of patients most recent medications, belonging to specified VA DRUG classes  
  ;"INPUT: OUT -- PASS BY REFERENCE.  Prior entries are not killed. 
  ;"          OUT("LINE",<original Rx table line>,IEN50.605)=""
  ;"          OUT("CLASS",IEN50.605)=<CLASS_NAME>^<External Description>  (VA drug class file)
  ;"          OUT("CLASS",IEN50.605,<original Rx table line>)=""
  ;"       CLASSIENARR -- PASS BY REFERENCE.  An array of IEN's in 50.605 (VA DRUG CLASS).  
  ;"            Format: CLASSIENARR(IEN50D605)=""
  ;"                    CLASSIENARR(IEN50D605)=""
  ;"       TMGDFN -- patient IEN
  ;"       OPTION("USEOLDMETHOD")=1 if old method wanted.  
  ;"Result: none
  NEW USEOLDMETHOD SET USEOLDMETHOD=+$GET(OPTION("USEOLDMETHOD"))  ;"//kt 5/6/18
  NEW IEN50D605 SET IEN50D605=0
  FOR  SET IEN50D605=$ORDER(CLASSIENARR(IEN50D605)) QUIT:IEN50D605'>0  DO
  . IF USEOLDMETHOD DO
  . . DO GETCLSRX0(.OUT,IEN50D605,TMGDFN)  ;"//kt 5/6/18
  . ELSE  DO
  . . DO GETCLSRX(.OUT,IEN50D605,TMGDFN)  ;"//kt 5/6/18  
  QUIT
  ;
GETCLSRX(OUT,CLASSIEN,TMGDFN) ;"Get a list of patients most recent medications, belonging to 1 specified VA DRUG class  
  ;"INPUT: OUT -- PASS BY REFERENCE.  Prior entries are not killed. 
  ;"          OUT("LINE",<original Rx table line>,IEN50.605)=""
  ;"          OUT("CLASS",IEN50.605)=<CLASS_NAME>^<External Description>  (VA drug class file)
  ;"          OUT("CLASS",IEN50.605,<original Rx table line>)=""
  ;"       CLASSIEN -- IEN in 50.605 (VA DRUG CLASS) of desired matching class
  ;"       TMGDFN -- patient IEN
  ;"Result: none
  NEW ACLASS SET ACLASS=0
  FOR  SET ACLASS=$ORDER(^TMG(22733.2,TMGDFN,"RX","ACLASS",ACLASS)) QUIT:ACLASS'>0  DO
  . IF $$ISCLASS^TMGRXU01(ACLASS,CLASSIEN)=0 QUIT
  . NEW CLASSNAME SET CLASSNAME=$PIECE($GET(^PS(50.605,ACLASS,0)),"^",1,2)
  . SET OUT("CLASS",ACLASS)=CLASSNAME
  . NEW SUBIEN SET SUBIEN=0
  . FOR  SET SUBIEN=$ORDER(^TMG(22733.2,TMGDFN,"RX","ACLASS",ACLASS,SUBIEN)) QUIT:SUBIEN'>0  DO
  . . NEW ZN SET ZN=$GET(^TMG(22733.2,TMGDFN,"RX",SUBIEN,0))
  . . NEW LINE SET LINE=$PIECE(ZN,"^",2)
  . . SET OUT("LINE",LINE,ACLASS)=""
  . . SET OUT("CLASS",ACLASS,LINE)=""
  QUIT
  ;
GETCLSRX0(OUT,CLASSIEN,TMGDFN)  ;"DEPRECIATED //kt 5/6/18
  ;"Get a list of patients most recent medications, belonging to 1 specified VA DRUG class  
  ;"INPUT: OUT -- PASS BY REFERENCE.  Prior entries are not killed. 
  ;"          OUT("LINE",<original Rx table line>,IEN50.605)=""
  ;"          OUT("CLASS",IEN50.605)=<CLASS_NAME>^<External Description>  (VA drug class file)
  ;"          OUT("CLASS",IEN50.605,<original Rx table line>)=""
  ;"       CLASSIEN -- IEN in 50.605 (VA DRUG CLASS) of desired matching class
  ;"       TMGDFN -- patient IEN
  ;"Result: none
  NEW ARR,PARSEDARR,TEMP
  NEW REF SET REF=$NAME(^TMP("TMG CLASS RXs",$J,TMGDFN))
  IF $DATA(@REF)>0 DO
  . NEW DH SET DH=+$GET(@REF@("$H"))
  . NEW SEC SET SEC=$$HDIFF^XLFDT($H,DH,3)  ;"NUMBER OF SECONDS AGO DATA WAS STORED 
  . IF SEC>300 KILL @REF
  IF $DATA(@REF)>0 DO
  . MERGE PARSEDARR=@REF@("ARR")
  ELSE  DO
  . ;"DO MEDLIST^TMGTIUOJ(.TEMP,TMGDFN,.ARR)
  . DO MEDARR^TMGTIUOJ(.TEMP,TMGDFN,.ARR)
  . DO PARSEARR(.PARSEDARR,.ARR)
  . KILL @REF 
  . MERGE @REF@("ARR")=PARSEDARR
  . SET @REF@("$H")=$H
  NEW ONEIEN SET ONEIEN=0
  FOR  SET ONEIEN=$ORDER(PARSEDARR("CLASS",ONEIEN)) QUIT:ONEIEN'>0  DO
  . IF $$ISCLASS^TMGRXU01(ONEIEN,CLASSIEN)=0 QUIT
  . NEW LINE SET LINE=""
  . FOR  SET LINE=$ORDER(PARSEDARR("CLASS",ONEIEN,LINE)) QUIT:LINE=""  DO
  . . SET OUT("LINE",LINE,ONEIEN)=""
  . . SET OUT("CLASS",ONEIEN,LINE)=""
  QUIT
  ;  
PARSEARR(OUT,ARR)  ;"PARSE A MED LIST  
  ;"INPUT: OUT -- PASS BY REFERENCE
  ;"          OUT("LINE",<original Rx table line>,IEN50.605)=""
  ;"          OUT("CLASS",IEN50.605)=<CLASS_NAME>^<External Description>  (VA drug class file)
  ;"          OUT("CLASS",IEN50.605,<original Rx table line>)=""
  ;"       ARR -- PASS BY REFERENCE. Format: Med list array as output by MEDARR^TMGTIUOJ(), 3rd param
  ;"          ARR(#)=<line from medication table>
  ;"          ARR("KEY-VALUE","KEY")=<value>
  ;"
  NEW OLD,IDX,JDX SET (IDX,JDX)=0 
  FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:IDX'>0  DO
  . NEW LINE SET LINE=$$TRIM^XLFSTR($GET(ARR(IDX))) 
  . IF LINE["[OLD ENTRY]" QUIT
  . IF LINE["[MEDICATION" QUIT
  . IF LINE["[FINAL MEDICATION" QUIT
  . IF $EXTRACT(LINE,1)="*" QUIT
  . NEW RXINFO DO PARSELN^TMGRX001(.RXINFO,LINE) ;"<-- if this is too slow, could perhaps make similar function that returns less info
  . DO GETCLASS^TMGRXU01(.OUT,.RXINFO)
  QUIT
  ;
  ;"============================================
HNDLCLSS(LIST) ;"HANDLE CLASSES
  ;"Designed to be called from menu in CONSOLE^TMGRX002
  NEW MASTERREF,DONEREF,DELREF DO GETREFS^TMGRX002(.MASTERREF,.DONEREF,.DELREF) 
  NEW MENU,IDX,LISTCT
  NEW PICK DO GETSLL4(.PICK,.LIST) ;"Prep array for selection  
HCSL1 ;          
  SET IDX=0  
  SET LISTCT=$SELECT($DATA(LIST)=0:0,1:$$LISTCT^TMGMISC2("LIST"))
  KILL MENU SET MENU(IDX)="Select Option For Managing Drug CLASSES"
  SET MENU(IDX,1)="Current RX LIST has "_LISTCT_" entries"
  SET IDX=IDX+1,MENU(IDX)="VIEW current drug class mapping"_$CHAR(9)_"VIEW"
  SET IDX=IDX+1,MENU(IDX)="ADD drug class to med(s)"_$CHAR(9)_"ADD"
  SET IDX=IDX+1,MENU(IDX)="EDIT drug class of med(s)"_$CHAR(9)_"EDIT"  
  SET IDX=IDX+1,MENU(IDX)="REMOVE entries already assigned a class"_$CHAR(9)_"REMOVE"
  SET IDX=IDX+1,MENU(IDX)="BROWSE classes"_$CHAR(9)_"CLASSBROWSE"
  SET IDX=IDX+1,MENU(IDX)="SHOW members of a class"_$CHAR(9)_"SHOWMEMBERS"
  SET IDX=IDX+1,MENU(IDX)="TABLE edit of RELATED MED CLASSES"_$CHAR(9)_"TABLECLASSES"
  SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
  IF USRPICK="^" GOTO HCSDN  
  IF USRPICK="VIEW" DO  GOTO HCSL1
  . DO VWCLLIST(.LIST,.PICK) ;"View medications in list, showing drug classes.   
  IF USRPICK="ADD" DO  GOTO HCSL1  
  . DO HNDCLADD(.LIST,.PICK) 
  IF USRPICK="EDIT" DO  GOTO HCSL1
  . DO HNDLEDIT(.LIST,.PICK)
  IF USRPICK="REMOVE" DO  GOTO HCSL1
  . DO HNDLREMV(.LIST,.PICK) 
  IF USRPICK="CLASSBROWSE" DO  GOTO HCSL1
  . IF $$PICKCLAS^TMGRXU01  ;"IGNORE PICKED CLASS    
  IF USRPICK="SHOWMEMBERS" DO  GOTO HCSL1
  . DO SHOWCMBR() ;"SHOW MEMBERS OF VA DRUG CLASS (including any children classes)
  IF USRPICK="TABLECLASSES" DO  GOTO HCSL1
  . DO PKEDTBLC()  ;"Pick and edit table classes
  GOTO HCSL1
HCSDN  ;
  QUIT
  ;
HNDCLADD(LIST,PICK) ;  
  NEW ADDARR,ARR2 
  DO SELRX4(.ADDARR,.LIST,.PICK)
  IF $DATA(ADDARR)=0 WRITE !,"None Selected.",! QUIT
  WRITE !,"Next, pick a VA DRUG CLASS to apply to selected medications",!
  DO PRESS2GO^TMGUSRI2
  NEW CLASS SET CLASS=$$PICKCLAS^TMGRXU01  
  IF +CLASS'>0 WRITE !,"None Selected.",! QUIT
  NEW IDX SET IDX=""
  FOR  SET IDX=$ORDER(ADDARR(IDX)) QUIT:IDX=""  DO
  . NEW NAME SET NAME=$GET(ADDARR(IDX)) QUIT:NAME=""  
  . SET ARR2($PIECE(NAME,"^",1))=$PIECE(NAME,"^",2)
  WRITE !,"Selected Medication(s)",!
  WRITE "-----------------------",!
  DO VIEWLST^TMGRX002(.ARR2)
  WRITE !,"*ADD* [",$PIECE(CLASS,"^",2),"] ("_$PIECE(CLASS,"^",3)_") class to med(s)",!
  WRITE "Continue" 
  SET %=2 DO YN^DICN WRITE ! IF %'=1 QUIT
  SET IDX=""
  FOR  SET IDX=$ORDER(ADDARR(IDX)) QUIT:IDX=""  DO
  . NEW NAME SET NAME=$GET(ADDARR(IDX)) QUIT:NAME=""  
  . NEW IEN22733 SET IEN22733=$PIECE(NAME,"^",2)
  . NEW TEMP SET TEMP=$$ADDCLASS^TMGRXU01(+CLASS,IEN22733)
  . IF +TEMP'>0 DO  QUIT
  . . WRITE !,$PIECE(TEMP,"^",2),!
  . . DO PRESS2GO^TMGUSRI2  
  KILL PICK  ;"will force repopulating
  QUIT
  ;
HNDLEDIT(LIST,PICK) ;
  NEW ARR,ARR2 DO SELRX4(.ARR,.LIST,.PICK)
  IF $DATA(ARR)=0 WRITE !,"None Selected.",! QUIT
  WRITE !,"NOTE: ",!,"This utility can not edit classes derived from linked VA PRODUCT entries.",!
  NEW IDX SET IDX=""
  FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:IDX=""  DO
  . NEW NAME SET NAME=$GET(ARR(IDX)) QUIT:NAME=""  
  . NEW IEN22733 SET IEN22733=+$PIECE(NAME,"^",2) QUIT:IEN22733'>0
  . WRITE !,"EDITING [",$$GET1^DIQ(22733,IEN22733,.01),"], field #4 (LINKED VA DRUG CLASS):",!
  . DO EDITFLDS^TMGRX005(IEN22733,4)
  DO PRESS2GO^TMGUSRI2   
  QUIT
  ;
VWCLLIST(LIST,PICK) ;"View medications in list, showing drug classes.   
  IF $DATA(PICK)=0 DO GETSLL4(.PICK,.LIST) ;"Prep array for selection
  NEW SOMESHOWED SET SOMESHOWED=0
  NEW NAME SET NAME=""
  FOR  SET NAME=$ORDER(PICK(NAME)) QUIT:NAME=""  DO
  . WRITE NAME,!
  . SET SOMESHOWED=1
  ;" NEW NAME SET NAME=""
  ;" FOR  SET NAME=$ORDER(LIST(NAME)) QUIT:NAME=""  DO
  ;" . NEW RXINFO DO PARSELN^TMGRX001(.RXINFO,NAME)
  ;" . NEW IEN22733 SET IEN22733=$GET(RXINFO("IEN22733")) QUIT:IEN22733'>0
  ;" . NEW CLSTR SET CLSTR=$$GTCLSTR^TMGRXU01(IEN22733)
  ;" . WRITE CLSTR," ",NAME," (`",IEN22733,")",!   
  ;" . SET SOMESHOWED=1
  IF SOMESHOWED=0 WRITE !,"(None)",!
  DO PRESS2GO^TMGUSRI2
  QUIT  
  ;  
SELRX4(OUT,LIST,PICK) ;"Pick medication lines, showing drug class
  ;"OUT -- FORMAT:  OUT(MedLin^IEN22733)=""
  IF $DATA(PICK)=0 DO GETSLL4(.PICK,.LIST) ;"Prep array for selection  
  KILL OUT  
  DO SELECTOR^TMGUSRI3("PICK","OUT","Select medication(s). <ESC><ESC> to exit.")
  QUIT
  ;
GETSLL4(OUT,LIST) ;"Prep array for selection
  ;"OUT FORMAT:  OUT(DISPLAY NAME)=MedLine^IEN22733
  SET LISTCT=$SELECT($DATA(LIST)=0:0,1:$$LISTCT^TMGMISC2("LIST"))
  NEW MIN SET MIN=1
  NEW MAX SET MAX=LISTCT
  NEW STARTH SET STARTH=$H
  NEW CT SET CT=0  
  NEW NAME SET NAME=""
  FOR  SET NAME=$ORDER(LIST(NAME)) QUIT:NAME=""  DO
  . SET CT=CT+1 IF CT#10=0 DO PROGBAR^TMGUSRI2(CT,"Progress",1,MAX,60,STARTH)
  . NEW RXINFO DO PARSELN^TMGRX001(.RXINFO,NAME)
  . NEW IEN22733 SET IEN22733=$GET(RXINFO("IEN22733")) QUIT:IEN22733'>0
  . NEW CLSTR SET CLSTR=$$GTCLSTR^TMGRXU01(IEN22733)
  . SET OUT(CLSTR_" "_NAME)=NAME_"^"_IEN22733   
  QUIT
  ; 
HNDLREMV(LIST,PICK) ;"Handle remove entries already assigned a drug class  
  IF $DATA(PICK)=0 DO GETSLL4(.PICK,.LIST) ;"Prep array for selection
  NEW CT SET CT=0
  NEW NAME SET NAME=""
  FOR  SET NAME=$ORDER(PICK(NAME)) QUIT:NAME=""  DO
  . IF $EXTRACT(NAME,1,2)="[ " QUIT
  . NEW RX SET RX=$PIECE($GET(PICK(NAME)),"^",1)
  . KILL LIST(RX)
  . KILL PICK(NAME)
  . SET CT=CT+1
  WRITE "Deleted ",CT," entries.",!
  DO PRESS2GO^TMGUSRI2
  QUIT  
  ;
SHOWCMBR() ;"SHOW MEMBERS OF VA DRUG CLASS (including any children classes)
  ;"ASK FOR CLASS
  NEW IEN50D605 SET IEN50D605=$$PICKCLAS^TMGRXU01 
  IF IEN50D605'>0 QUIT
  NEW ARR DO GETCMBR(.ARR,+IEN50D605)   ;"GET MEMBERS OF VA DRUG CLASS
  NEW SOMESHOWN SET SOMESHOWN=0
  NEW NAME SET NAME=""
  FOR  SET NAME=$ORDER(ARR(NAME)) QUIT:NAME=""  DO
  . NEW IEN22733 SET IEN22733=$GET(ARR(NAME))
  . WRITE NAME," ('",IEN22733,")",!
  . SET SOMESHOWN=1
  IF SOMESHOWN=0 WRITE "(none)",!
  QUIT
  ;
GETCMBR(OUT,IEN50D605)   ;"GET MEMBERS OF VA DRUG CLASS (including any children classes)
  ;"SCAN EACH ENTRY IN 22733 AND GET CLASS LIST FROM THAT ENTRY
  ;"  SEE IF EACH ENTRY IN CLASS LIST MATCHS IEN50D605
  ;"  IF SO, THEN ADD TO OUT DIRECTOR
  ;"INPUT: OUT -- PASS BY REFERENCE, AN OUT PARAMETER.  FORMAT:
  ;"         OUT(DISPLAY_TEXT)=IEN22733
  NEW IEN22733 SET IEN22733=0
  FOR  SET IEN22733=$ORDER(^TMG(22733,IEN22733)) QUIT:IEN22733'>0  DO
  . NEW CLASSARR DO GTCLARR^TMGRXU01(.CLASSARR,IEN22733) ;"Format: ARR(IEN50.605)= <CODE>^<DISPLAY NAME>
  . IF $$ISCLSS2^TMGRXU01(.CLASSARR,IEN50D605)'=1 QUIT
  . NEW LINE SET LINE=$$RXSTR^TMGRXU01(IEN22733)
  . SET OUT(LINE)=IEN22733
  QUIT
  ;
PKEDTBLC()  ;"Pick and edit table classes
  NEW X,Y,DIC SET DIC=22708,DIC(0)="MAEQ"
  DO ^DIC WRITE !
  IF Y>0 DO EDTTBLCL(+Y)
  QUIT
  ;
EDTTBLCL(IEN22708)  ;"Interact with user and edit RELATED MED CLASSES field in TMG TIU PXRM TABLE (22708) file
  ;"Designed to be called from menu in CONSOLE^TMGRX002
  NEW MENU,IDX,SUBIEN
  SET IEN22708=$GET(IEN22708) QUIT:IEN22708'>0
ETBL1 ;          
  SET IDX=0  
  KILL MENU SET MENU(IDX)="Select Option For Editing TMG TIU PXRM TABLE"
  SET MENU(IDX,1)=" -- "_$$GET1^DIQ(22708,IEN22708,.01)
  SET SUBIEN=0
  FOR  SET SUBIEN=$ORDER(^TMG(22708,IEN22708,5,SUBIEN)) QUIT:SUBIEN'>0  DO
  . NEW IEN50D605 SET IEN50D605=$PIECE($GET(^TMG(22708,IEN22708,5,SUBIEN,0)),"^",1) QUIT:IEN50D605'>0
  . NEW NAME SET NAME=$$CLASSNAM^TMGRXU01(IEN50D605)
  . SET NAME="("_$PIECE(NAME,"^",1)_") "_$PIECE(NAME,"^",2)
  . SET IDX=IDX+1,MENU(IDX)="REMOVE class: "_NAME_$CHAR(9)_"DEL^"_SUBIEN_"^"_IEN50D605   
  SET IDX=IDX+1,MENU(IDX)="ADD class to table"_$CHAR(9)_"ADD"
  SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
  IF USRPICK="^" GOTO ETBDN  
  IF USRPICK["DEL^" DO  GOTO ETBL1
  . NEW SUBIEN SET SUBIEN=$PIECE(USRPICK,"^",2) QUIT:SUBIEN'>0
  . NEW DA SET DA=SUBIEN,DA(1)=IEN22708
  . NEW DIK SET DIK="^TMG(22708,"_DA(1)_",5,"
  . DO ^DIK 
  IF USRPICK="ADD" DO  GOTO ETBL1
  . NEW IEN50D605 SET IEN50D605=+$$PICKCLAS^TMGRXU01 QUIT:IEN50D605'>0
  . IF $DATA(^TMG(22708,IEN22708,5,"B",IEN50D605)) DO  QUIT
  . . WRITE !,"Already present! Try again.",! 
  . . DO PRESS2GO^TMGUSRI2
  . NEW TMGFDA,TMGIEN,TMGMSG
  . SET TMGFDA(22708.06,"+1,"_IEN22708_",",.01)=IEN50D605
  . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  . IF $DATA(TMGMSG("DIERR")) DO  QUIT
  . . WRITE !,"ERROR:",!,$$GETERRST^TMGDEBU2(.TMGMSG),!  
  . . DO PRESS2GO^TMGUSRI2    
  GOTO ETBL1
ETBDN ;
  QUIT
  ;
  