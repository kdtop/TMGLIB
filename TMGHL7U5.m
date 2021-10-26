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