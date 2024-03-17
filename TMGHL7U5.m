TMGHL7U5 ;TMG/kst-HL7 utility functions ; 10/11/21
              ;;1.0;TMG-LIB;**1**;3/6/18
 ;
 ;"TMG HL7 UTILITY FUNCTIONS 
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 10/11/21  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;                                           
 ;"=======================================================================
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"FIXLAB  -- Called from OPTION: TMG LAB FIX XREF (via scheduled Taskman task)
 ;
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"=======================================================================
 ;
CHKLRDFN   ;"Check to see if any LRDFN's point to invalid patient records
        NEW LRDFN SET LRDFN=0
        FOR  SET LRDFN=$ORDER(^LR(LRDFN)) QUIT:LRDFN'>0  DO
        . NEW TMGDFN SET TMGDFN=+$ORDER(^DPT("ATMGLR",LRDFN,0))
        . IF TMGDFN=0 DO
        . . WRITE "ERROR!! ",LRDFN," IS BLANK",!
        . NEW NAME SET NAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
        . IF NAME="" DO
        . . WRITE "ERROR WITH LRDFN ",LRDFN,!
        . . WRITE "      IT POINTS TO TMGDFN ",TMGDFN," WHICH IS EMPTY",!
        . NEW LASTDFN SET LASTDFN=$G(^TMG("CHKLRDFN",$J,LRDFN))
        . IF LASTDFN'=TMGDFN DO
        . . WRITE "ERROR!! ",LRDFN," POINTED TO ",LASTDFN," BUT NOW POINTS TO ",TMGDFN,!
        . ;"SET ^TMG("CHKLRDFN",$J,LRDFN)=TMGDFN
        QUIT
        ;"
FIXLAB(VERBOSE)  ;"  Called from OPTION: TMG LAB FIX XREF (via scheduled Taskman task)        
        SET VERBOSE=$GET(VERBOSE,0)
        NEW % DO NOW^%DTC
        SET ^TMG("TMG LAB FIX XREF",%)="STARTED"
        NEW LRDFN SET LRDFN=0
        FOR  SET LRDFN=$ORDER(^LR(LRDFN)) QUIT:LRDFN'>0  DO
        . DO FIX1PTL(LRDFN,VERBOSE)
        DO NOW^%DTC
        SET ^TMG("TMG LAB FIX XREF",%)="FINISHED"        
        QUIT
        ;
FIX1PTL(LRDFN,VERBOSE) ;"Fix lab for one patient
        ;"Input: LRDFN -- the lab patient index number
        ;"       VERBOSE -- optional.  If 1, then info written to output.
        ;"Result: none
        SET VERBOSE=$GET(VERBOSE,1)
        NEW FIXARRAY
        NEW TMGDFN SET TMGDFN=$ORDER(^DPT("ATMGLR",LRDFN,0))
        IF TMGDFN'>0 DO  QUIT
        . IF VERBOSE WRITE "LRDFN: ",LRDFN," DOES NOT HAVE AN DFN.",!
        NEW NAME SET NAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
        IF VERBOSE WRITE "CHECKING LABS FOR PATIENT: ",NAME,"(",TMGDFN,"-",LRDFN,")-----",!
        NEW IDT SET IDT=0
        FOR  SET IDT=$ORDER(^LR(LRDFN,"CH",IDT)) QUIT:IDT'>0  DO
        . NEW DATE
        . SET DATE=$PIECE($GET(^LR(LRDFN,"CH",IDT,0)),"^",1)
        . IF DATE="" QUIT  ;"CAN'T PROCEED IF LAB DOESN'T HAVE DATE
        . NEW TMGFLD SET TMGFLD=0
        . FOR  SET TMGFLD=$ORDER(^LR(LRDFN,"CH",IDT,TMGFLD)) QUIT:TMGFLD'>0  DO
        . . NEW IEN60 SET IEN60=$PIECE($GET(^LR(LRDFN,"CH",IDT,TMGFLD)),"^",3)
        . . SET IEN60=$PIECE(IEN60,"!",7)
        . . IF IEN60'>0 QUIT  
        . . NEW NODE SET NODE=LRDFN_";CH;"_IDT_";"_TMGFLD
        . . IF VERBOSE WRITE "      ->",$$FMTE^XLFDT(DATE,"2PZ")," - ",$PIECE($GET(^LAB(60,IEN60,0)),"^",1)," (IEN60: '",IEN60,")",!
        . . IF $GET(FIXARRAY(TMGDFN,DATE,IEN60,NODE))=1 QUIT  ;"already processed. 
        . . IF $$SLABNEEDED(TMGDFN,DATE,IEN60,NODE) DO
        . . . IF VERBOSE WRITE "FIX NEEDED HERE!",!
        . . . SET FIXARRAY(TMGDFN,DATE,IEN60,NODE)=1
        . . . DO SLAB^LRPX(TMGDFN,DATE,IEN60,NODE)
        QUIT
        ;
SLABNEEDED(TMGDFN,DATE,ITEM,NODE) ; copied and modified from SLAB^LRPX
        ; SET index for lab data.
        ;"Input: TMGDFN -- PATIENT IEN  (not LRDFN)
        ;"       DATE -- DT of labs, in FM format
        ;"       ITEM -- IEN60
        ;"       NODE -- i.e.  LRDFN_";CH;"_LRIDT_";"_LRDN
        NEW NEEDED SET NEEDED=1
        IF $DATA(^PXRMINDX(63,"PI",TMGDFN,ITEM,DATE,NODE))=0 GOTO SLNDDN
        IF $DATA(^PXRMINDX(63,"IP",ITEM,TMGDFN,DATE,NODE))=0 GOTO SLNDDN
        IF ITEM=+ITEM SET NEEDED=0 GOTO SLNDDN
        IF $DATA(^PXRMINDX(63,"PDI",TMGDFN,DATE,ITEM,NODE))=0 GOTO SLNDDN
        SET NEEDED=0
SLNDDN  QUIT NEEDED
        ;"
TEST    ;"
        NEW TMGDFN SET TMGDFN=0
        FOR  SET TMGDFN=$O(^DPT(TMGDFN)) QUIT:TMGDFN'>0  DO
        . DO SPECALRT(TMGDFN)
        QUIT
        ;"
SPECALRT(TMGDFN)  ;"DETERMINE IF LAB RESULTS NEED TO BE SENT TO SPECIALIST
        NEW IDX
        SET IDX=0
        NEW MSGPRE,MSGPOST
        FOR  SET IDX=$O(^TMG(22757,IDX)) QUIT:IDX'>0  DO
        . NEW ZN SET ZN=$G(^TMG(22757,IDX,0))
        . NEW TITLEIEN,RANGE,BDT,SPECIALTY
        . SET TITLEIEN=$P(ZN,"^",1)
        . SET RANGE=$P(ZN,"^",2)
        . SET SPECIALTY=$P(ZN,"^",3)
        . SET RANGE=RANGE*365
        . SET RANGE="-"_RANGE
        . SET BDT=$$ADDDAYS^TMGDATE(RANGE)  ;"_".000000"
        . NEW SENDALERT SET SENDALERT=0
        . FOR  SET BDT=$O(^TIU(8925,"ZTMGPTDT",TMGDFN,BDT)) QUIT:(BDT="")!(SENDALERT=1)  DO
        . . NEW TIUIEN SET TIUIEN=0
        . . FOR  SET TIUIEN=$O(^TIU(8925,"ZTMGPTDT",TMGDFN,BDT,TIUIEN)) QUIT:(TIUIEN'>0)!(SENDALERT=1)  DO
        . . . NEW THISTITLE
        . . . SET THISTITLE=$P($G(^TIU(8925,TIUIEN,0)),"^",1)
	    . . . IF THISTITLE=TITLEIEN SET SENDALERT=1
	    . IF SENDALERT=1 DO
	    . . NEW ALERTRESULT,ALERTMSG
	    . . SET ALERTMSG=$P($G(^DPT(TMGDFN,0)),"^",1)_" NEEDS LAB RESULTS SENT TO "_SPECIALTY
	    . . IF $$ALRTEXST(150,ALERTMSG)=0 DO INFRMALT^TMGXQAL(.ALERTRESULT,150,ALERTMSG)
        QUIT
        ;"
ALRTEXST(TMGDUZ,TEXT)  ;"
        ;"Returns 0 or 1 (whether the alert exists or not)
        NEW TMGRESULT SET TMGRESULT=0
        NEW MSGDT SET MSGDT=""
        FOR  SET MSGDT=$O(^XTV(8992,TMGDUZ,"XQA",MSGDT)) QUIT:MSGDT=""  DO
        . NEW MESSAGE SET MESSAGE=$P($G(^XTV(8992,TMGDUZ,"XQA",MSGDT,0)),"^",3)
        . IF MESSAGE=TEXT SET TMGRESULT=1
        QUIT TMGRESULT
        ;"
