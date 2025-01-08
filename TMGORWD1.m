TMGORWD1 ;TMG/kst/ExtraOrderPrint functions 9/1/2022, 11/10/24
         ;;1.0;TMG-LIB;**1**;09/01/2022
  ;
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;"Copyright (c) 11/10/24  Kevin S. Toppenberg MD
  ;"
  ;"This file is part of the TMG LIBRARY, and may only be used in accordence
  ;" to license terms outlined in separate file TMGLICNS.m, which should   
  ;" always be distributed with this file.                              
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;
  ;"=======================================================================
  ;"PUBLIX FUNCTIONS
  ;"=======================================================================  
  ;"CHK4FAX(PRTLIST) -- called from PRINTGUI^ORWD1. Checks text of order and if "FAX" is found, sends an alert to Eddie
  ;"WEDGE(ORDER,ARR,WIDTH) -- called from ORDTEXT^ORCSAVE1() to effext custom saver in Fields 22700/22701
  ;"TMGORTX(IEN100,IEN100D008,ARR,WIDTH) --Replacement function for ORTX^ORCSAVE1.  Called via WEDGE
  ;"INSTNUM(DA)  -- Return computed field TMG INSTANCE # (22700) for subfile ITEMS in 101.41 (ORDER DIALOG)
  ;"CMPDTEXT(DA) --Return computed value for FIELD 22700 IN RESPONSES SUBFILE IN ORDER FILE (100)  RETURNS DISPLAY TEXT.
  ;"CMPHLPMG(DA) --Return computed value for FIELD 22701 IN RESPONSES SUBFILE IN ORDER FILE (100)  RETURNS HELP MESSAGE
  ;
  ;"=======================================================================
  ;"PRIVATE API FUNCTIONS
  ;"=======================================================================
  ;"GETDLGINFO(ARR,OUT,IEN101D41) ;"Load info about order dialog
  ;"GET1INSTINFO(IEN101D41,ITEMIEN,INSTNUM,FLDS,FLAGS,OUTREF,MSGREF) --Get info for 1 type:instance element. Utility function for possible future use.  
  ;"TESTG1II ;
  ;"GETINFO(DA,FLD) ;"Return computed value for FIELD 22700 or 22701 IN RESPONSES SUBFILE IN ORDER FILE (100) 
  ;"=======================================================================
  ;           
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
  ;"           Field 22700 (TMG ORDER TEXT COMPILER TAG),  
  ;"           and Field 22701 (TMG ORDER TEXT COMPILER RTN)
  ;"           in File 101.41 (ORDER DIALOG)
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
EXAMINE100(IEN100,OUT)  ;"Dump value of file 100 for TMG stored lab dialog responses
  SET IEN100=+$GET(IEN100) IF IEN100'>0 GOTO E100DN 
  NEW ZN SET ZN=$GET(^OR(100,IEN100,0))
  NEW IEN101D41 SET IEN101D41=$PIECE(ZN,"^",5)  ;"0;5 = ORDER DIALOG  e.g. 16021;ORD(101.41,
  IF $PIECE(IEN101D41,";",2)'="ORD(101.41," GOTO E100DN
  SET IEN101D41=+IEN101D41   ;"E.g. 16021
  NEW ARR SET ARR=IEN101D41
  NEW SUBIEN SET SUBIEN=0
  FOR  SET SUBIEN=$ORDER(^OR(100,IEN100,4.5,SUBIEN)) QUIT:SUBIEN'>0  DO
  . ;"FINISH LATER...  I would love to not have to recreate how to make ARR to pass to GETDLGINFO....
E100DN ;
  QUIT
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
  ;"            E.G. 
  ;"                ARR=16021
  ;"                ARR(16022)="116^^"    <-- TMG LAB ORDER Y/N (`16022 in #101.41)
  ;"                ARR(16022,0)="Y"
  ;"                ARR(16022,9)=1
  ;"                ARR(16022,14)=1      
  ;"                ARR(16024)="29^^"     <-- TMG LAB COMMON DX ENTRY (`16024 in #101.41)
  ;"                ARR(16024,0)="Y"
  ;"                ARR(16024,9)=1
  ;"                ARR(16026)="92^^"     <-- TMG LAB DISPLAY GROUP (`16026 in #101.41)
  ;"                ARR(16026,0)="F"
  ;"                ARR(16027)="97^^"     <-- TMG LAB BUNDLE ENTRY (`16027 in #101.41)
  ;"                ARR(16027,0)="F"
  ;"                ARR(16032)="102^^"    <-- TMG LAB LISTBOX GROUP (`16032 in #101.41) 
  ;"                ARR(16032,0)="N"     
  ;"                ARR(16032,1)=0
  ;"                ARR(16032,2)=0
  ;"                ARR(16033)="107^^"    <-- TMG LAB ITEM DATA (`16033 in #101.41)
  ;"                ARR(16033,0)="Y"
  ;"                ARR(16034)="124^^"    <-- TMG LAB TEXT FIELD (`16034 in #101.41)
  ;"                ARR(16034,0)="F"
  ;"                ARR(16035)="121^^"    <-- TMG LAB WP FIELD (`16035 in #101.41)
  ;"                ARR(16035,0)="W"
  ;"                ARR("ORTS")=0
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
  NEW ARR22751
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
  . . . IF (KEY="IEN"),(+VALUE>0) DO
  . . . . SET ARR22751(VALUE)=$$GET1^DIQ(22751,+VALUE,.01)
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
  . . . NEW LINKNAME SET LINKNAME="LINK TEST TO DX"
  . . . NEW TYPE2 SET TYPE2=$SELECT(KIND=LINKNAME:"Linked",KIND["DX":"Dx",KIND["OTHER TIME":"LabTimeOther",1:"Proc")
  . . . IF KIND=LINKNAME DO 
  . . . . NEW IDX FOR IDX=1:1:$LENGTH(EVALUE,",") DO
  . . . . . NEW ASET SET ASET=$PIECE(EVALUE,",",IDX) QUIT:ASET=""  DO
  . . . . . . NEW PROCIEN SET PROCIEN=+$PIECE(ASET,":",1) QUIT:PROCIEN'>0
  . . . . . . NEW DXIEN SET DXIEN=+$PIECE(ASET,":",2) QUIT:DXIEN'>0
  . . . . . . NEW PROCNAME SET PROCNAME=$GET(ARR22751(PROCIEN)) QUIT:PROCNAME=""
  . . . . . . NEW DXNAME SET DXNAME=$GET(ARR22751(DXIEN)) QUIT:DXNAME=""
  . . . . . . NEW DXICDPTR SET DXICDPTR=$PIECE($GET(^TMG(22751,DXIEN,2)),"^",1)
  . . . . . . SET OUT(TYPE2,PROCNAME)="" ;"PROCIEN
  . . . . . . SET OUT(TYPE2,PROCNAME,DXNAME)=$$GET1^DIQ(80,DXICDPTR,.01) ;"DXIEN
  . . . ELSE  SET OUT(TYPE2,EVALUE)=""
  . . IF TYPENAME="WPFld" DO
  . . . NEW WPARR MERGE WPARR=LOCALARR("WP",ELEMENTP,INSTANCE)
  . . . MERGE OUT("COMMENT")=WPARR
  QUIT
  ;
TMGORTX(IEN100,IEN100D008,ARR,WIDTH) ;"Replacement function for ORTX^ORCSAVE1
  ;"Q: How does this function get called? 
  ;"A: ORDTEXT^ORCSAVE1 normally calls ORTX^ORCSAVE1.  But we have a wedge that calls WEDGE^TMGORWD1,
  ;"   and this routine (located above), looks in field 22700/22701 for location of
  ;"   a replacement function.  Currently in the TMG LAB ORDER DIALOG ELEMENTS record in 101.41,
  ;"   we have a link here to this TMGORTX^TMGORWD1. 
  ;"   ORDTEXT^ORCSAVE1 is called during the normal VistA order saving process, I believe.
  ;"
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
  ;" -- first any linked Proc->Dx entries. 
  NEW APROC SET APROC=""
  FOR  SET APROC=$ORDER(INFO("Linked",APROC)) QUIT:APROC=""  DO
  . KILL INFO("Proc",APROC) ;"prevent listing twice.  
  . IF STR'="" SET STR=STR_", "
  . NEW DXSTR SET DXSTR=""
  . NEW ADX SET ADX=""
  . FOR  SET ADX=$ORDER(INFO("Linked",APROC,ADX)) QUIT:ADX=""  DO
  . . NEW ICD SET ICD=$GET(INFO("Linked",APROC,ADX))
  . . IF DXSTR'="" SET DXSTR=DXSTR_", "
  . . SET DXSTR=DXSTR_ADX_" - "_ICD
  . IF DXSTR'="" DO
  . . SET STR=STR_APROC_"(Dx: "_DXSTR_")"
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
INSTNUM(DA)  ;"Return computed field TMG INSTANCE # (22700) for subfile ITEMS in 101.41 (ORDER DIALOG)
  NEW RESULT SET RESULT=""
  NEW SELFIEN SET SELFIEN=+$GET(DA) IF SELFIEN'>0 GOTO INDN
  NEW IEN101D41 SET IEN101D41=+$GET(DA(1)) IF IEN101D41'>0 GOTO INDN
  NEW SEQARR
  ;"CYCLE THROUGH "B" INDEX OF SUBFILE, COUNTING INSTANCES, UNTIL SELF FOUND
  NEW DONE SET DONE=0
  NEW ASEQ SET ASEQ=0
  FOR  SET ASEQ=$ORDER(^ORD(101.41,IEN101D41,10,"B",ASEQ)) QUIT:(ASEQ'>0)!(DONE)  DO
  . NEW SUBIEN SET SUBIEN=0
  . FOR  SET SUBIEN=$ORDER(^ORD(101.41,IEN101D41,10,"B",ASEQ,SUBIEN)) QUIT:(SUBIEN'>0)!(DONE)  DO  ;"NOTE: Should only be 1 SUBIEN for 1 SEQ#, but I'll be safe
  . . NEW ZN SET ZN=$GET(^ORD(101.41,IEN101D41,10,SUBIEN,0))
  . . NEW ITEMPTR SET ITEMPTR=+$PIECE(ZN,"^",2) QUIT:ITEMPTR'>0
  . . NEW INSTNUM SET INSTNUM=+$GET(SEQARR(ITEMPTR))+1
  . . SET SEQARR(ITEMPTR)=INSTNUM
  . . IF SUBIEN'=SELFIEN QUIT
  . . SET RESULT=INSTNUM,DONE=1
INDN ;  
  QUIT RESULT
  ;
GET1INSTINFO(IEN101D41,ITEMIEN,INSTNUM,FLDS,FLAGS,OUTREF,MSGREF) ;"Get info for 1 type:instance element.
  ;"Utility function for possible future use.  
  ;"INPUT: IEN101D41 -- IEN IN 101.41 of dialog holding all the items making up the visible dialog
  ;"       ITEMIEN   -- IEN in 101.41 of the 1 dialog element (e.g. Y/N box)
  ;"       INSTNUM   -- NUMBER, indicating it's instance number the parent dialog's ITEMS subfile, by sequence
  ;"       FLDS  -- The fields to return for desired subrecord to pass to GETS^DIQ, default is "*"
  ;"       FLAGS -- Flags to pass to GETS^DIQ
  ;"       OUTREF -- Name of array to receive output
  ;"       MSGREF -- Namm of array to recieve messages from GETS^DIQ
  ;"CYCLE THROUGH "B" INDEX OF SUBFILE, COUNTING INSTANCES
  SET IEN101D41=+$GET(IEN101D41) IF IEN101D41'>0 GOTO G1IIDN
  SET ITEMIEN=+$GET(ITEMIEN) IF ITEMIEN'>0 GOTO G1IIDN
  SET INSTNUM=+$GET(INSTNUM) IF INSTNUM'>0 GOTO G1IIDN
  SET FLDS=$GET(FLDS,"*")
  NEW SEQARR
  ;"CYCLE THROUGH "B" INDEX OF SUBFILE, COUNTING INSTANCES, UNTIL INSTNUM FOUND
  NEW DONE SET DONE=0
  NEW ASEQ SET ASEQ=0
  FOR  SET ASEQ=$ORDER(^ORD(101.41,IEN101D41,10,"B",ASEQ)) QUIT:(ASEQ'>0)!(DONE)  DO
  . NEW SUBIEN SET SUBIEN=0
  . FOR  SET SUBIEN=$ORDER(^ORD(101.41,IEN101D41,10,"B",ASEQ,SUBIEN)) QUIT:(SUBIEN'>0)!(DONE)  DO  ;"NOTE: Should only be 1 SUBIEN for 1 SEQ#, but I'll be safe
  . . NEW ZN SET ZN=$GET(^ORD(101.41,IEN101D41,10,SUBIEN,0))
  . . NEW ITEMPTR SET ITEMPTR=+$PIECE(ZN,"^",2) QUIT:(ITEMPTR'>0)!(ITEMPTR'=ITEMIEN)
  . . NEW CURINSTNUM SET CURINSTNUM=+$GET(SEQARR(ITEMPTR))+1
  . . SET SEQARR(ITEMPTR)=CURINSTNUM
  . . IF CURINSTNUM'=INSTNUM QUIT
  . . NEW IENS SET IENS=SUBIEN_","_IEN101D41_","
  . . DO GETS^DIQ(101.412,IENS,.FLDS,.FLAGS,OUTREF,MSGREF)
  . . SET DONE=1
G1IIDN ;  
  QUIT
  ;
TESTG1II ;
  NEW ARR,ERR
  DO GET1INSTINFO(16021,16022,58,"*","EIN","ARR","ERR")
  IF $DATA(ARR) ZWR ARR
  IF $DATA(ERR) ZWR ERR
  QUIT
  ;
CMPDTEXT(DA) ;"Return computed value for FIELD 22700 IN RESPONSES SUBFILE IN ORDER FILE (100)  RETURNS DISPLAY TEXT.
  NEW RESULT SET RESULT=$$GETINFO(.DA,4)
  QUIT RESULT
  ;
CMPHLPMG(DA)  ;"Return computed value for FIELD 22701 IN RESPONSES SUBFILE IN ORDER FILE (100)  RETURNS HELP MESSAGE
  NEW RESULT SET RESULT=$$GETINFO(.DA,11)
  QUIT RESULT
  ;
GETINFO(DA,FLD) ;"Return computed value for FIELD 22700 or 22701 IN RESPONSES SUBFILE IN ORDER FILE (100) 
  ;"NOTE: Sometimes when this is called from Fileman, DA is not define properly,
  ;"      So then uses D0, D1 in GLOBAL SCOP.  
  NEW RESULT SET RESULT=""
  IF +$GET(DA(1))'>0 DO
  . SET DA(1)=$GET(D0)
  . SET DA=D1
  NEW SUBIEN SET SUBIEN=+$GET(DA) IF SUBIEN'>0 GOTO CFDN
  NEW IEN100 SET IEN100=+$GET(DA(1)) IF IEN100'>0 GOTO CFDN
  ;"GET IEN101.41  ensure dialog is actually 101.41
  NEW ZN SET ZN=$GET(^OR(100,IEN100,0))
  NEW IEN101D41 SET IEN101D41=$PIECE(ZN,"^",5)
  IF $PIECE(IEN101D41,";",2)'="ORD(101.41," GOTO CFDN
  SET IEN101D41=+IEN101D41  ;"stip to piece #1
  SET ZN=$GET(^OR(100,IEN100,4.5,SUBIEN,0)) IF ZN="" GOTO CFDN
  NEW ITEMPTR SET ITEMPTR=+$PIECE(ZN,"^",2) IF ITEMPTR'>0 GOTO CFDN
  NEW INSTNUM SET INSTNUM=+$PIECE(ZN,"^",3) IF INSTNUM'>0 GOTO INSTNUM
  NEW TMGARR,TMGERR  
  DO GET1INSTINFO(IEN101D41,ITEMPTR,INSTNUM,FLD,"E","TMGARR","TMGERR") ;"Get info for 1 type:instance element.
  NEW IENS SET IENS=$ORDER(TMGARR(101.412,"")) IF IENS="" GOTO CFDN
  NEW VAL SET VAL=$GET(TMGARR(101.412,IENS,FLD,"E"))
  SET RESULT=VAL
CFDN ;  
  QUIT RESULT
  ;
CHKPRINT(TMGRESULT,TMGDFN,ORDERIEN)  ;"  
  ;"CHECK THE LAB ORDER BEFORE PRINTING TO SEE IF IT NEEDS TO WARN THE USER
  ;"SET TMGRESULT(0)=""
  NEW RESULTIDX SET RESULTIDX=0
  NEW TMGTEST SET TMGTEST=0
  IF TMGTEST=1 DO
  . SET TMGDFN=$G(^TMG("CHKPRINT","TMGDFN"))
  . SET ORDERIEN=$G(^TMG("CHKPRINT","ORDERIEN"))
  ELSE  DO
  . SET ^TMG("CHKPRINT","TMGDFN")=TMGDFN
  . SET ^TMG("CHKPRINT","ORDERIEN")=ORDERIEN
  ;"
  ;"CHECK ORDER TEXT AND SET APPROPRIATE VARIABLES
  ;"The variables here will be:
  ;"   NEXTAPPTLABS - Will be 1 if the order is for the Next Appt
  ;"   TODAYLABS - Will be 1 if the order is to be done today
  ;"   ADDONLABS - Will be 1 if the order is to be added to existing blood work
  NEW ORDERTEXT 
  DO TEXT^ORQ12(.ORDERTEXT,$P(ORDERIEN,";",1))
  NEW NEXTAPPTLABS SET NEXTAPPTLABS=0
  NEW TODAYLABS SET TODAYLABS=0
  NEW ADDONLABS SET ADDONLABS=0
  NEW ORDIDX SET ORDIDX=0
  FOR  SET ORDIDX=$O(ORDERTEXT(ORDIDX)) QUIT:ORDIDX'>0  DO
  . NEW LINE SET LINE=$$UP^XLFSTR($G(ORDERTEXT(ORDIDX)))
  . IF LINE["NEXT APPT" SET NEXTAPPTLABS=1
  . IF LINE["TODAY" SET TODAYLABS=1
  . IF LINE["USE LAB BLOOD" SET ADDONLABS=1
  ;"
  ;"CHECK THE PATIENT TO SEE WHEN THE NEXT APPT IS
  ;"The variables here will be:
  ;"   TODAYAPPT - Will be a date if the patient has an appt today or ""
  ;"   APPTSOON - Will be a date if the patient has an appt in the next 14 days or ""
  NEW TODAYAPPT,APPTSOON,NEXTAPPT
  SET (TODAYAPPT,APPTSOON,NEXTAPPT)=0
  NEW DATE,X,Y
  DO NOW^%DTC SET DATE=X
  NEW TODAY SET TODAY=DATE
  NEW SOONDT SET SOONDT=$$ADDDAYS^TMGDATE(14)
  FOR  SET DATE=$ORDER(^TMG(22723,TMGDFN,1,"B",DATE)) QUIT:DATE'>0  DO
  . NEW THISDAY SET THISDAY=$P(DATE,".",1)
  . NEW IDX SET IDX=$ORDER(^TMG(22723,TMGDFN,1,"B",DATE,0))
  . IF $P($G(^TMG(22723,TMGDFN,1,IDX,0)),"^",7)="C" QUIT
  . IF THISDAY=TODAY SET TODAYAPPT=1
  . IF THISDAY<SOONDT SET APPTSOON=1
  . IF THISDAY>TODAY SET NEXTAPPT=THISDAY
  ;"
  NEW MESSAGE SET MESSAGE=""
  IF (APPTSOON=1)&(NEXTAPPTLABS=0) SET MESSAGE="Patient has appt in the next 14 days. Order isn't before next appt, though."
  IF (NEXTAPPTLABS=1)&(APPTSOON=0) SET MESSAGE="This order is for next appt, but the next appt isn't until: "_$$EXTDATE^TMGDATE(NEXTAPPT,1)  	  
  ;"
  IF MESSAGE'="" DO
  . SET TMGRESULT(RESULTIDX)=MESSAGE,RESULTIDX=RESULTIDX+1
  . SET TMGRESULT(RESULTIDX)="",RESULTIDX=RESULTIDX+1
  . SET TMGRESULT(RESULTIDX)="Do you still want to print this order?",RESULTIDX=RESULTIDX+1
  . ;"SET TMGRESULT(RESULTIDX)="ARE YOU SURE YOU WANT TO PRINT THIS ORDER????",RESULTIDX=RESULTIDX+1
  . ;"SET TMGRESULT(RESULTIDX)="JUST DON'T BE STUPID ABOUT IT.",RESULTIDX=RESULTIDX+1
  QUIT
  ;"