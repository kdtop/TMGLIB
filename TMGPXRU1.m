TMGPXRU1 ;TMG/kst/TMG Reminder Utilities ;5/8/13, 2/2/14, 1/15/17
         ;;1.0;TMG-LIB;**1**;5/8/13
 ;
 ;"TMG REMINDER FUNCTIONS
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"NSUREHAS(SOURCEREF,FACTOR) -- Ensure patients have health factor etc stored for given dates
 ;"IFHAS(DFN,FACTOR) -- Determine IF patient has health factor, and IF found, return date of most recent finding.
 ;"GETHFDT(DFN,HFNAME,HFARRAY) ; -- Return most recent date of provided HF 
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"SETUPFLS(FACTOR,VFILE,P2IEN,XREF) --SET UP FOR FILES
 ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"
 ;"=======================================================================
 ;
NSUREHAS(SOURCEREF,FACTOR) ;"ENSURE PATIENT HAS HEALTH FACTOR(S)
        ;"Purpose: Ensure list of patients have health factor etc stored for given dates
        ;"Input: REF -- format: @REF@(DFN,FMDT)=""        
        ;"       FACTOR -- format:  'HF.NOTAVETERAN
        ;"Result : 1^Success, or -1^Message IF error.
        NEW TMGRESULT SET TMGRESULT="-1^Unknown error"
        NEW VFILE,XREF SET (VFILE,XREF)=""
        NEW P2FILE,P2IEN SET (P2FILE,P2IEN)=0
        NEW PREFIX SET PREFIX=$PIECE(FACTOR,".",1)
        NEW FNAME SET FNAME=$PIECE(FACTOR,".",2)
        ;"NEW DIC,X,Y,P2IEN SET DIC(0)="M"
        NEW VISITMSG,NOTSUPPORTED
        SET TMGRESULT=$$SETUPFLS^TMGPXRU1(FACTOR,.VFILE,.P2IEN,.XREF) ;
        IF +TMGRESULT<0 GOTO NSHDN        
        SET VISITMSG(2)="VST^VC^E"
        SET VISITMSG(3)="VST^OL^0^"_+$GET(DUZ(2))
        SET VISITMSG(4)="COM^1^@"
        SET TMGRESULT="1^Success"
        NEW DFN SET DFN=0
        FOR  SET DFN=$ORDER(@SOURCEREF@(DFN)) QUIT:(+DFN'>0)  DO
        . SET VISITMSG(5)="VST^PT^"_DFN
        . NEW FMDT SET FMDT=0
        . FOR  SET FMDT=$ORDER(@SOURCEREF@(DFN,FMDT)) QUIT:(+FMDT'>0)  DO
        . . SET VISITMSG(1)="HDR^0^^0;"_FMDT_";E"
        . . IF PREFIX="IM" SET VISITMSG(6)="IMM+^"_P2IEN_"^^"_FNAME_"^@^^@^0^^1"
        . . IF PREFIX="HF" SET VISITMSG(6)="HF+^"_P2IEN_"^^^^"_DUZ_"^^^^^"_FMDT_"^"
        . . IF PREFIX="ST" SET VISITMSG(6)="SK+^"    ;"//FINISH
        . . IF PREFIX="EX" SET VISITMSG(6)="XAM^"    ;"//FINISH
        . . IF PREFIX="ED" SET VISITMSG(6)="PED+^"   ;"//FINISH
        . . IF PREFIX="CPT" SET VISITMSG(6)="CPT+^"  ;"//FINISH          
        . . IF PREFIX="ICD9" SET VISITMSG(6)="POV+^" ;"//FINISH
        . . SET VISITMSG(7)="VST^DT^"_FMDT
        . . NEW FOUND SET FOUND=0
        . . NEW IDT SET IDT=""
        . . FOR  SET IDT=$ORDER(@XREF@(IDT)) QUIT:(+IDT'>0)!FOUND  DO
        . . . IF 9999999-IDT=FMDT SET FOUND=1 QUIT
        . . IF FOUND QUIT
        . . NEW OUT
        . . DO SAVE^ORWPCE(.OUT,.VISITMSG,0,+$GET(DUZ(2)))
NSHDN   QUIT TMGRESULT
        ; 
IFHAS(DFN,FACTOR) ;"RETURN IF PATIENT HAS FACTOR
        ;"Purpose: See if 1 patient has health factor at all. 
        ;"Input: DFN -- Patient IEN        
        ;"       FACTOR -- format:  'HF.NOTAVETERAN
        ;"Result : 1^Found^Date, or 0^Not found, or -1^Message if error.
        NEW TMGRESULT SET TMGRESULT="-1^Unknown error"
        NEW P2IEN,VFILE,XREF SET (P2IEN,VFILE,XREF)=""
        SET TMGRESULT=$$SETUPFLS(FACTOR,.VFILE,.P2IEN,.XREF) ;"<-- XREF is output, with DFN node
        IF +TMGRESULT<0 GOTO IHSDN        
        SET TMGRESULT="0^Not Found"        
        NEW IDT SET IDT=$ORDER(@XREF@(""))
        IF IDT'="" SET TMGRESULT="1^Success^"_(9999999-IDT)        
IHSDN   QUIT TMGRESULT
        ;
SETUPFLS(FACTOR,VFILE,P2IEN,XREF) ;"SET UP VARIABLES FOR FILES, for given FACTOR
        ;"PRIVATE
        ;"Input: FACTOR -- format:  'HF.NOTAVETERAN
        ;"       VFILE -- AN OUT PARAMETER.  PASS BY REFRENCE. FM File for factor
        ;"       P2IEN -- AN OUT PARAMETER.  PASS BY REFRENCE. FM File pointed to
        ;"       XREF -- AN OUT PARAMETER.  PASS BY REFRENCE. A cross reference for factor that can be looped through
        NEW TMGRESULT SET TMGRESULT="1^Success"
        NEW P2FILE,NOTSUPPORTED
        SET (VFILE,P2FILE,XREF)="" SET P2FILE=0
        NEW PREFIX SET PREFIX=$PIECE(FACTOR,".",1)
        NEW FNAME SET FNAME=$PIECE(FACTOR,".",2)
        NEW DIC,X,Y SET DIC(0)="M"
        SET NOTSUPPORTED=0
        IF PREFIX="IM" DO
        . SET VFILE=9000010.11,P2FILE=9999999.14,XREF="IMM"  ;"Immunization
        IF PREFIX="HF" DO
        . SET VFILE=9000010.23,P2FILE=9999999.64,XREF="HF"   ;"Health Factor        
        IF PREFIX="ICD9" DO
        . SET NOTSUPPORTED=0,TMGRESULT="-1^"_PREFIX_" not currently supported"  ;"finish support and remove later. 
        . SET VFILE=9000010.07,P2FILE=80,XREF="POV"        ;"Diagnosis
        IF PREFIX="ST" DO
        . SET NOTSUPPORTED=0,TMGRESULT="-1^"_PREFIX_" not currently supported"  ;"finish support and remove later. 
        . SET VFILE=9000010.12,P2FILE=9999999.28,XREF="SK"   ;"Skin Test
        IF PREFIX="EX" DO
        . SET NOTSUPPORTED=0,TMGRESULT="-1^"_PREFIX_" not currently supported"  ;"finish support and remove later. 
        . SET VFILE=9000010.13,P2FILE=9999999.15,XREF="XAM"  ;"Exam
        IF PREFIX="ED" DO
        . SET NOTSUPPORTED=0,TMGRESULT="-1^"_PREFIX_" not currently supported"  ;"finish support and remove later. 
        . SET VFILE=9000010.16,P2FILE=9999999.09,XREF="ED"   ;"Education Topic
        IF PREFIX="CPT" DO
        . SET NOTSUPPORTED=0,TMGRESULT="-1^"_PREFIX_" not currently supported"  ;"finish support and remove later. 
        . SET VFILE=9000010.18,P2FILE=81,XREF="CPT"         ;"Procedure
        IF +TMGRESULT<0 GOTO SUFSDN
        IF (+P2FILE'>0)!(NOTSUPPORTED)!(XREF="") DO  GOTO SUFSDN
        . SET TMGRESULT="-1^Unknown error"
        SET DIC=P2FILE,X=FNAME 
        DO ^DIC SET P2IEN=+Y
        IF +Y>0 SET FNAME=$P(Y,"^",2) 
        ELSE  DO  GOTO SUFSDN
        . SET TMGRESULT="-1^Unable to find factor '"_FNAME_" in file "_P2FILE 
        SET XREF="^AUPNV"_XREF_"(""AA"",DFN,"_+P2IEN_")"  ;"all the V * have the same AA reference.
SUFSDN  QUIT TMGRESULT
        ;        
GETHFDT(DFN,HFNAME,HFARRAY,OPTION) ;
        ;"Purpose: Find and return the most recent date of a health factor for a patient
        ;"Input: DFN - Patient's IEN
        ;"       HFName - Name of health factor as defined in 
        ;"                file 9999999.64 Health Factors
        ;"       HFARRAY optional - AN OUT PARAMETER. includes all health
        ;"               factor dates  e.g. ARRAY(FMDate)=IEN in 9999999.64
        ;"       OPTION -- OPTIONAL.  PASS BY REFERENCE.  //kt added 1/15/17
        ;"             OPTION("SDT")=FMDT -- if present, HF's prior to SDT are ignored
        ;"             OPTION("EDT")=FMDT -- if present, HF's after SDT are ignored
        ;"Result: FMDate of last occurance or -1^Error Message or 0 if not found
        ;"        Also, HFARRAY is filled with ALL found matching instances 
        NEW TMGRESULT,HFDATE,TEMPIEN,TEMPHFIEN
        KILL HFARRAY
        NEW SDT SET SDT=+$GET(OPTION("SDT"))
        NEW EDT SET EDT=+$GET(OPTION("EDT")) IF EDT'>0 SET EDT=9999999
        NEW HFIEN SET HFIEN=$$GETHFIEN(HFNAME),TEMPIEN=0
        IF HFIEN'>0 SET TMGRESULT="-1^HF NAME NOT FOUND" GOTO GHDDN
        FOR  SET TEMPIEN=$ORDER(^AUPNVHF("C",DFN,TEMPIEN)) QUIT:TEMPIEN'>0  DO
        . IF HFIEN'=$PIECE($GET(^AUPNVHF(TEMPIEN,0)),"^",1) QUIT
        . SET HFDATE=$PIECE($GET(^AUPNVHF(TEMPIEN,12)),"^",1)
        . IF HFDATE'>0 DO
        . . NEW HFVISIT SET HFVISIT=$PIECE($GET(^AUPNVHF(TEMPIEN,0)),"^",3)
        . . SET HFDATE=$PIECE($GET(^AUPNVSIT(HFVISIT,0)),"^",1)
        . IF (HFDATE<SDT)!(HFDATE>EDT) QUIT   ;"//kt 1/15/17
        . SET HFDATE=$PIECE(HFDATE,".",1) ;"Remove time   7/28/16
        . SET HFARRAY(HFDATE)=TEMPIEN
        SET TMGRESULT=+$ORDER(HFARRAY(9999999),-1)     
GHDDN   QUIT TMGRESULT
        ;
GETHFIEN(HFNAME) ;
        ;"Purpose: To find IEN for a health factor
        ;"Input: HFNAME - Name of health factor
        ;"Result: IEN of health factor or -1
        NEW TMGRESULT,HFIEN 
        SET TMGRESULT="-1",HFIEN=0
        SET HFIEN=$ORDER(^AUTTHF("B",HFNAME,HFIEN))
        IF HFIEN>0 SET TMGRESULT=HFIEN
        QUIT HFIEN
        ;