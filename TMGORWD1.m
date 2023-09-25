TMGORWD1 ;TMG/kst/ExtraOrderPrint functions 9/1/2022
         ;;1.0;TMG-LIB;**1**;09/01/2022
CHK4FAX(PRTLIST) ;
    ;"This is called from PRINTGUI^ORWD1. It checks the text of the order and if the word
    ;"   "FAX" is found, it sends an alert to Eddie to verify that the lab results
    ;"    are set to be faxed to the mentioned office/doctor
    NEW TMGTEST SET TMGTEST=0
    IF TMGTEST=1 DO
    . MERGE PRTLIST=^TMG("TMGORWD1")
    ELSE  DO
    . MERGE ^TMG("TMGORWD1")=PRTLIST
    NEW I
    SET I=0
    FOR  SET I=$O(PRTLIST(I)) QUIT:I'>0  DO
    . NEW ORDIEN SET ORDIEN=+$G(PRTLIST(I))
    . IF ORDIEN'>0 QUIT
    . NEW ORDERARR
    . DO TEXT^ORQ12(.ORDERARR,ORDIEN,"")
    . NEW LINEIDX SET LINEIDX=0
    . NEW ORDERTEXT SET ORDERTEXT=""
    . FOR  SET LINEIDX=$O(ORDERARR(LINEIDX)) QUIT:LINEIDX'>0  DO
    . . SET ORDERTEXT=ORDERTEXT_$$UP^XLFSTR(ORDERARR(LINEIDX))
    . IF ORDERTEXT["FAX" DO
    . . NEW TMGDFN SET TMGDFN=+$P($G(^OR(100,ORDIEN,0)),"^",2)
    . . NEW MSG SET MSG=$P($G(^DPT(TMGDFN,0)),"^",1)_" HAD A LAB ORDER PRINTED THAT NEEDS TO BE FAXED"
    . . NEW ALRTMSG DO INFRMALT^TMGXQAL(.ALRTMSG,150,MSG)    
    QUIT
    ;"
WEDGE(ORDER,ARR,WIDTH) ;   
  ;"This is called from ORDTEXT^ORCSAVE1()
  ;"Purpose: * This function will look for special compilation code stored
  ;"           in a linked order dialog.  This code linked by custom, site-specific
  ;"           field 22700, TMG ORDER TEXT COMPILER TAG, 
  ;"           and field 22701 TMG ORDER TEXT COMPILER RTN  
  ;"         * The linked-to compiler function must accept same number of paramaters
  ;"           as example code TMGORTX^TMGORWD1()
  ;"         * The code must compile the text of the order and save into the 
  ;"           ORDER TEXT field (#.1) of the subfile ORDER ACTIONS (FLD #8, subfile#100.008)
  ;"           in the ORDER file.
  ;"         * If this wedge is not able to meet above requirements, it should 
  ;"           return a 0 result value.
  ;"
  ;"Inputs: ORDER -- This is '<IEN100>;<IEN100.008>' -- ORDER IEN's
  ;"        ARR -- Array.  Passed in.  format:         
  ;"           Note: this array initially generated from CPRS during RPC call
  ;"                It is filled out in DO GETDLG1^ORCD(ORDIALOG)
  ;"           ARR=IEN101.41 of top-level dialog   
  ;"           ARR(COMPIEN101.41)=#^^
  ;"           ARR(COMPIEN101.41,0)=field defination information
  ;"           ARR(COMPIEN101.41,1)=User data (store values), internal format.
  ;"           ARR("WP",COMPIEN101.41,1,#,0)=<LINE OF TEXT>  <-- if WP field above
  ;"           ARR("ORTS")=0
  ;"        WIDTH -- max width for output
  ;" NOTE: ORDEA is optionally used in GLOBAL SCOPE downstream-- as does ORDTEXT^ORCSAVE1
  ;"Result: 1^OK if TMG handled, and standard VistA code can be bypassed
  ;"        0 if not handled.
  ;"       -1^Error message
  NEW RESULT SET RESULT=0
	NEW IEN100 SET IEN100=+$GET(ORDER)
	NEW IEN100D008 SET IEN100D008=+$PIECE(ORDER,";",2)
	;"Get linked OR DIALOG (if any)
	NEW IEN101D41 SET IEN101D41=$PIECE($GET(^OR(100,IEN100,3)),"^",4)  ;"Variable pointer field
	SET IEN101D41=$SELECT(IEN101D41[";ORD(101.41":+IEN101D41,1:0) ;"Make null if not to 101.41
	IF IEN101D41=0 GOTO WDN
	;"See if OR DIALOG has data for custom fields 22700, 22701
	NEW TMG SET TMG=$GET(^ORD(101.41,IEN101D41,22700)) IF TMG="" GOTO WDN
	NEW TAG,ROUTINE SET TAG=$PIECE(TMG,"^",1),ROUTINE=$PIECE(TMG,"^",2) IF (TAG="")!(ROUTINE="") GOTO WDN
	;"Call custom code
	NEW CODE SET CODE="SET RESULT=$$"_TAG_"^"_ROUTINE_"("_IEN100_","_IEN100D008_",.ARR,"_WIDTH_")"
	XECUTE CODE
WDN ;                                                        
  QUIT RESULT
  ;
GETDLGINFO(ARR,OUT,IEN101D41) ;"Load info about order dialog
  ;"Input:   ARR -- ARRAY, Format as per ARR in TMGORTX.  
  ;"            ARR=IEN101.41 of top-level dialog
  ;"            ARR(COMPIEN101.41)=#^^
  ;"            ARR(COMPIEN101.41,0)=field defination information
  ;"            ARR(COMPIEN101.41,<instance#>)=User data (store values), internal format.
  ;"            ARR(COMPIEN101.41,<instance#>,<KeyName>)=<Value>
  ;"            ARR("WP",COMPIEN101.41,1,#,0)=<LINE OF TEXT>  <-- if WP field above
  ;"            ARR("ORTS")=0
  ;"            ARR("TYPE",<type>,<COMPIEN101.41><instance>)=""
  ;"         OUT -- OUTPUT OF FUNCTION.  e.g. 
  ;"           OUT("COMMENT",1,0)="This is an order comment"
  ;"           OUT("COMMENT",2,0)="Make sure to collect specimen!"
  ;"           OUT("Dx","Anemia - D64.9")=""
  ;"           OUT("Dx","DM-2 - E11.9")=""
  ;"           OUT("Dx","Fatigue - R53.82")=""
  ;"           OUT("Dx","Gout - M10.9")=""
  ;"           OUT("Dx","OtherDx1")=""
  ;"           OUT("Dx","OtherDx2")=""
  ;"           OUT("Dx","OtherDx3")=""
  ;"           OUT("Dx","OtherDx4")=""
  ;"           OUT("Dx","Preventative - Z00.00")=""
  ;"           OUT("Dx","Prostate/BPH - N40.0")=""
  ;"           OUT("Dx","Well Child Preventative - Z00.129")=""
  ;"           OUT("LabTiming","ASAP")=""
  ;"           OUT("OrderFlags","Sick Patient")=""
  ;"           OUT("OrderFlags","Standing Order")=""
  ;"           OUT("OrderOptions","AutoSign Order")=""
  ;"           OUT("OrderOptions","Ballad Order")=""
  ;"           OUT("OrderOptions","Outside Order")=""
  ;"           OUT("OrderOptions","Prompt To Print")=""
  ;"           OUT("OrderingProvider","As Ordered")=""
  ;"           OUT("Proc","CBC-Platelet With Diff.")=""
  ;"           OUT("Proc","CMP")=""
  ;"           OUT("Proc","Folic Acid")=""
  ;"           OUT("Proc","Lipids")=""
  ;"           OUT("Proc","OtherProc")=""
  ;"           OUT("Proc","TIBC")=""
  ;"         IEN101D41 -- IEN of dialog being processed. 
  NEW INSTARR  ;"STORES LAST USED INSTANCE NUMBER...
  NEW LOCALARR MERGE LOCALARR=ARR
  NEW TYPEINFO
  SET TYPEINFO("?")="None"
  SET TYPEINFO("D")="Dialog"
  SET TYPEINFO("I")="Dx"
  SET TYPEINFO("L")="Proc"
  SET TYPEINFO("B")="Bundle"
  SET TYPEINFO("P")="PageGroup"
  SET TYPEINFO("O")="OrderingProvider"
  SET TYPEINFO("T")="LabTiming"
  SET TYPEINFO("F")="OrderFlags"
  SET TYPEINFO("N")="OrderOptions"
  SET TYPEINFO("X")="ItemData"
  SET TYPEINFO("E")="TextFld"
  SET TYPEINFO("W")="WPFld"  
  ;
  ;"Gather instance info and metadata from order dialog being processed
  NEW ASEQ SET ASEQ=0
  FOR  SET ASEQ=$ORDER(^ORD(101.41,IEN101D41,10,"B",ASEQ)) QUIT:ASEQ'>0  DO
  . NEW IDX SET IDX=0
  . FOR  SET IDX=$ORDER(^ORD(101.41,IEN101D41,10,"B",ASEQ,IDX)) QUIT:IDX'>0  DO
  . . NEW N0 SET N0=$GET(^ORD(101.41,IEN101D41,10,IDX,0))
  . . NEW N1 SET N1=$GET(^ORD(101.41,IEN101D41,10,IDX,1))
  . . NEW ELEMENTP SET ELEMENTP=+$PIECE(N0,"^",2)
  . . NEW NAME SET NAME=$PIECE(N0,"^",4)
  . . SET NAME=$$RESTORENAME^TMGLRU2(NAME)
  . . NEW TEMP,DATA SET DATA=$PIECE(N1,"^",1)
  . . IF $EXTRACT(DATA,1)'="~" QUIT
  . . SET DATA=$EXTRACT(DATA,2,$LENGTH(DATA))
  . . NEW JDX FOR JDX=1:1:$LENGTH(DATA,";") DO
  . . . NEW PART SET PART=$PIECE(DATA,";",JDX) QUIT:PART=""
  . . . NEW KEY SET KEY=$PIECE(PART,"=",1)
  . . . NEW VALUE SET VALUE=$PIECE(PART,"=",2)
  . . . SET TEMP(KEY)=VALUE
  . . NEW INSTANCE SET INSTANCE=+$GET(INSTARR(ELEMENTP))+1 
  . . SET INSTARR(ELEMENTP)=INSTANCE
  . . SET INSTARR("I",ELEMENTP,INSTANCE)=NAME
  . . MERGE INSTARR("DATA",ELEMENTP,INSTANCE)=TEMP
  ;
  NEW ELEMENTP SET ELEMENTP=0
  FOR  SET ELEMENTP=$ORDER(ARR(ELEMENTP)) QUIT:ELEMENTP'>0  DO
  . NEW INSTANCE SET INSTANCE=0
  . FOR  SET INSTANCE=$ORDER(ARR(ELEMENTP,INSTANCE)) QUIT:INSTANCE'>0  DO
  . . SET NAME=$GET(INSTARR("I",ELEMENTP,INSTANCE))
  . . MERGE LOCALARR(ELEMENTP,INSTANCE)=INSTARR("DATA",ELEMENTP,INSTANCE)
  . . NEW TYPE SET TYPE=$GET(LOCALARR(ELEMENTP,INSTANCE,"TYPE"))
  . . SET LOCALARR(ELEMENTP,INSTANCE,"NAME")=NAME
  . . NEW TYPENAME SET TYPENAME=$GET(TYPEINFO(TYPE),"??")
  . . SET LOCALARR(ELEMENTP,INSTANCE,"TYPENAME")=TYPENAME  
  . . NEW IVALUE SET IVALUE=$GET(ARR(ELEMENTP,INSTANCE))
  . . IF "^LabTiming^OrderingProvider^"[TYPENAME DO
  . . . NEW ITEMS SET ITEMS=$GET(LOCALARR(ELEMENTP,INSTANCE,"ITEMS")) QUIT:ITEMS=""
  . . . NEW IVALUE SET IVALUE=$GET(LOCALARR(ELEMENTP,INSTANCE)) QUIT:IVALUE="" 
  . . . NEW IEN22751 SET IEN22751=$PIECE(ITEMS,",",IVALUE+1)
  . . . NEW EVALUE SET EVALUE=$PIECE($GET(^TMG(22751,IEN22751,0)),"^",1)
  . . . SET LOCALARR(ELEMENTP,INSTANCE,"VALUE-E")=EVALUE  
  . . . SET OUT(TYPENAME,EVALUE)=""
  . . IF "^Dx^Proc^OrderFlags^OrderOptions"[TYPENAME DO
  . . . IF $DATA(LOCALARR(ELEMENTP,INSTANCE,"ITEMS")) QUIT ;"Ignore parents of children entries.  
  . . . NEW EVALUE SET EVALUE=$SELECT(IVALUE=1:NAME,1:"")
  . . . NEW IVALUE2 SET IVALUE2=$SELECT(IVALUE=1:"SELECTED",1:"")
  . . . SET LOCALARR(ELEMENTP,INSTANCE,"VALUE-E")=EVALUE    
  . . . SET LOCALARR(ELEMENTP,INSTANCE,"VALUE-I")=IVALUE2    
  . . . SET OUT(TYPENAME,EVALUE)=""
  . . IF TYPENAME="TextFld" DO
  . . . NEW KIND SET KIND=$PIECE(NAME," ",3,99)
  . . . NEW IVALUE SET IVALUE=$GET(LOCALARR(ELEMENTP,INSTANCE)) QUIT:IVALUE="" 
  . . . NEW EVALUE SET EVALUE=IVALUE
  . . . SET LOCALARR(ELEMENTP,INSTANCE,"VALUE-I")=IVALUE  
  . . . SET LOCALARR(ELEMENTP,INSTANCE,"VALUE-E")=EVALUE  
  . . . NEW TYPE2 SET TYPE2=$SELECT(KIND["DX":"Dx",KIND["OTHER TIME":"LabTimeOther",1:"Proc")
  . . . SET OUT(TYPE2,EVALUE)=""
  . . IF TYPENAME="WPFld" DO
  . . . NEW WPARR MERGE WPARR=LOCALARR("WP",ELEMENTP,INSTANCE)
  . . . MERGE OUT("COMMENT")=WPARR
  QUIT
  ;
TMGORTX(IEN100,IEN100D008,ARR,WIDTH) ;"Replacement function for ORTX^ORCSAVE1
  ;"Inputs:  IEN100 -- IEN in file 100   
  ;"         IEN100D008 -- IEN is subfile 100.008 (ORDER ACTIONS subfile)
  ;"         ARR -- Array.  Passed in.  format:  
  ;"            Note: this array initially generated from CPRS during RPC call
  ;"                 It is filled out in DO GETDLG1^ORCD(ORDIALOG)
  ;"            ARR=IEN101.41 of top-level dialog
  ;"            ARR(COMPIEN101.41)=#^^
  ;"            ARR(COMPIEN101.41,0)=field defination information
  ;"            ARR(COMPIEN101.41,<instance#>)=User data (store values), internal format.
  ;"            ARR("WP",COMPIEN101.41,1,#,0)=<LINE OF TEXT>  <-- if WP field above
  ;"            ARR("ORTS")=0
  ;"         WIDTH -- max width for output
  ;" NOTE: ORDEA is optionally used in GLOBAL SCOPE-- as does ORDTEXT^ORCSAVE1, which this function replaces.
  ;"Output: Order is compiled and put into ORDER TEXT field (#.1) of the 
  ;"        subfile ORDER ACTIONS (FLD #8, subfile#100.008) in the ORDER file.
  ;"        Additional info may also be put into field #.2 (EXTERNAL TEXT)
  ;"Result: 1^OK, or -1^Error message
  ;"  NOTE: All result items should have a preceeding space character to indicate new line between
  ;"        each item
  NEW RESULT SET RESULT="1^OK"
  NEW TMGDEBUG SET TMGDEBUG=0
  IF TMGDEBUG=1 DO
  . KILL IEN100,IEN100D008,ARR,WIDTH
  . MERGE IEN100=^TMG("TMP","TMGORTX^TMGORWD1","IEN100") 
  . MERGE IEN100D008=^TMG("TMP","TMGORTX^TMGORWD1","IEN100D008") 
  . MERGE ARR=^TMG("TMP","TMGORTX^TMGORWD1","ARR") 
  . MERGE WIDTH=^TMG("TMP","TMGORTX^TMGORWD1","WIDTH")   
  ELSE  DO
  . KILL ^TMG("TMP","TMGORTX^TMGORWD1")
  . MERGE ^TMG("TMP","TMGORTX^TMGORWD1","IEN100")=IEN100 
  . MERGE ^TMG("TMP","TMGORTX^TMGORWD1","IEN100D008")=IEN100D008 
  . MERGE ^TMG("TMP","TMGORTX^TMGORWD1","ARR")=ARR 
  . MERGE ^TMG("TMP","TMGORTX^TMGORWD1","WIDTH")=WIDTH 
  ;
  NEW IEN101D41 SET IEN101D41=+$GET(ARR)
  NEW INFO
  DO GETDLGINFO(.ARR,.INFO,IEN101D41) ;"Load info about order dialog
  ;
  NEW ORDERPROVIDER,LABTIMING,SICKPT,STANDINGORDER,FASTING
  NEW BALLADORDER,OUTSIDEORDER,PRINTPROMPT,AUTOSIGN,FORHOSPITAL
  SET ORDERPROVIDER=$ORDER(INFO("OrderingProvider",""))
  SET LABTIMING=$ORDER(INFO("LabTiming",""))
  IF (LABTIMING["Other Time")&($D(INFO("LabTimeOther"))) DO
  . SET LABTIMING=LABTIMING_": "_$ORDER(INFO("LabTimeOther",""))
  SET SICKPT=$DATA(INFO("OrderFlags","Sick Patient"))>0
  SET STANDINGORDER=$DATA(INFO("OrderFlags","Standing Order"))>0
  SET FASTING=$DATA(INFO("OrderFlags","Fasting"))>0
  SET BALLADORDER=$DATA(INFO("OrderOptions","Ballad Order"))>0
  SET OUTSIDEORDER=$DATA(INFO("OrderOptions","Outside Order"))>0
  SET PRINTPROMPT=$DATA(INFO("OrderOptions","Prompt To Print"))>0
  SET AUTOSIGN=$DATA(INFO("OrderOptions","AutoSign Order"))>0
  SET FORHOSPITAL=(BALLADORDER!OUTSIDEORDER)
  ;  
  NEW EMPTYLINE SET EMPTYLINE="  "   ;" was " ."
  ;"Assemble beginning section
  NEW TMGARR,STR
  IF SICKPT DO
  . DO ADDWRAPARR^TMGSTUT2(.TMGARR,WIDTH,1," SICK PATIENT")
  . DO ADDWRAPARR^TMGSTUT2(.TMGARR,WIDTH,1,EMPTYLINE)
  SET STR=" "_$SELECT(FASTING:"FASTING",1:"NON-FASTING")_" LABS "_$$UP^XLFSTR(LABTIMING)
  IF STANDINGORDER SET STR=STR_" - STANDING ORDER"
  DO ADDWRAPARR^TMGSTUT2(.TMGARR,WIDTH,1,STR)
  DO ADDWRAPARR^TMGSTUT2(.TMGARR,WIDTH,1,EMPTYLINE)
  ;  
  ;"Check for GHP components... if so remove TSH, CBC, and CMP... then add GHP
  NEW D1 SET D1=$DATA(INFO("PROC","TSH"))>0
  NEW D2 SET D2=$DATA(INFO("PROC","CMP"))>0
  NEW D3 SET D3=$DATA(INFO("PROC","CBC-Platelet With Diff."))>0
  IF (D1&D2&D3) DO
  . KILL INFO("PROC","TSH")
  . KILL INFO("PROC","CMP")
  . KILL INFO("PROC","CBC-Platelet With Diff.")
  . SET INFO("PROC","General Health 80050 (CBC w/ diff & CMP & TSH)")=""
  ;
  ;"Add tests ordered
  SET STR=""
  NEW APROC SET APROC=""
  FOR  SET APROC=$ORDER(INFO("Proc",APROC)) QUIT:APROC=""  DO
  . IF STR'="" SET STR=STR_", "
  . SET STR=STR_APROC
  IF STR="" SET STR="(NONE)"
  DO ADDWRAPARR^TMGSTUT2(.TMGARR,WIDTH,1," TESTS ORDERED: "_STR)
  DO ADDWRAPARR^TMGSTUT2(.TMGARR,WIDTH,1,EMPTYLINE)
  ;
  ;"Add diagnoses
  SET STR=""
  NEW ADX SET ADX=""
  FOR  SET ADX=$ORDER(INFO("Dx",ADX)) QUIT:ADX=""  DO
  . IF STR'="" SET STR=STR_", "
  . SET STR=STR_ADX
  IF STR="" SET STR="(NONE)"
  DO ADDWRAPARR^TMGSTUT2(.TMGARR,WIDTH,1," DIAG: "_STR)
  DO ADDWRAPARR^TMGSTUT2(.TMGARR,WIDTH,1,EMPTYLINE)
  ;
  ;"Add Comments
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(INFO("COMMENT",IDX)) QUIT:IDX'>0  DO
  . DO ADDWRAPARR^TMGSTUT2(.TMGARR,WIDTH,1," "_$GET(INFO("COMMENT",IDX,0)))
  ;  
  ;"Add Ordering Provider
  IF ORDERPROVIDER'="As Ordered" DO
  . DO ADDWRAPARR^TMGSTUT2(.TMGARR,WIDTH,1,EMPTYLINE)
  . SET STR=" !! ORDERING PROVIDER IS "_$$UP^XLFSTR(ORDERPROVIDER)_" TOPPENBERG. !!"
  . DO ADDWRAPARR^TMGSTUT2(.TMGARR,WIDTH,1,STR)
  ;  
  ;
  ;"SET TMGARR(1)=" TEST1"
  ;"SET TMGARR(2)=" TEST2"               
  ;"SET TMGARR(3)=" TEST3-1"
  ;
  ;"--- FILE TMGARR INTO FILEMAN FIELDS ----
  NEW TMGIENS SET TMGIENS=IEN100D008_","_IEN100_","
  DO WP^DIE(100.008,TMGIENS,.1,"K","TMGARR","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO TODN
  . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  ;
  ;"Below code copied and modified from ORDTEXT^ORCSAVE1
	IF $EXTRACT($GET(ORDEA))'=2 GOTO TODN  ;PKI Drug Schedule - in future may allow 2-5
	SET ORSET=0
	DO DIGTEXT^ORCSAVE1(IEN100,ORDEA)  ;"modifies ORSET as output
	KILL TMGARR MERGE TMGARR=ORSET
  DO WP^DIE(100.008,TMGIENS,.2,"K","TMGARR","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO TODN
  . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  ;
TODN ;  
	QUIT RESULT
  ;