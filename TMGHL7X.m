TMGHL7X ;TMG/kst-HL7 transformation engine processing ;07/28/13, 2/2/14
              ;;1.0;TMG-LIB;**1**;03/28/11
 ;
 ;"TMG HL7 TRANSFORMATION FUNCTIONS
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
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"CYCLEXFMSG(TMGENV,TMGHL7MSG) -- Cycle through stages of transformation, PRE, FINAL etc.
 ;"XFMSG(TMGENV,TMGHL7MSG) -- take parsed message array, and apply transforms
 ;
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;"XFSEG(IEN22720,IENSEG,TMGSEGN,TMGHL7MSG,TMGU) -- take parsed message segment, and apply transforms
 ;"XFFLD(IEN22720,IENSEG,TMGSEGN,TMGFLDN,TMGHL7MSG,TMGU) -- take parsed message field, and apply transforms
 ;"XFCOMP(IEN22720,IENSEG,TMGSEGN,TMGFLDN,TMGCOMPN,TMGHL7MSG,TMGU) --take parsed message array, and apply transforms
 ;"XFSCMP(IEN22720,IENSEG,TMGSEGN,TMGFLDN,TMGCOMPN,TMGSCMPN,TMGHL7MSG,TMGU) --take parsed message array, and apply transforms
 ;"XECCODE(TMGCODREF,TMGVALUE,TMGNUM) -- launch user code
 ;"STRIPSN(TMGSEGN,TMGHL7MSG,TMGU) --return segment, with first piece with segment name ('OBX') removed.
 ;"STORSTPD(S,TMGSEGN,TMGHL7MSG,TMGU) --Store a stripped segment (i.e. without it's first piece with name, e.g. 'OBX')
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"TMGSTUTL, all the HL*, LA* code that the HL processing path normally calls.
 ;"=======================================================================
 ;
CYCLEXFMSG(TMGENV,TMGHL7MSG) ;
        ;"Purpose: Cycle through stages of transformation, PRE, FINAL etc.
        ;"Input: TMGENV -- PASS BY REFERENCE.  The lab environment array.  
        ;"       TMGHL7MSG -- PASS BY REFERENCE.  Format -- See PARSEMSG^TMGHL7X2
        ;"Result: 1 if OK, or -1^message IF problem.
        NEW TMGRESULT SET TMGRESULT=1        
        NEW IEN22720 SET IEN22720=TMGENV("IEN 22720")  ;"IEN in file TMG HL7 MESSAGE TRANSFORM SETTINGS
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(^TMG(22720,IEN22720,15,"B",IDX)) QUIT:(IDX'>0)!(+TMGRESULT'>0)  DO
        . NEW SUBIEN SET SUBIEN=$ORDER(^TMG(22720,IEN22720,15,"B",IDX,0)) QUIT:SUBIEN'>0
        . NEW ZN SET ZN=$GET(^TMG(22720,IEN22720,15,SUBIEN,0))
        . NEW TMGSTAGE SET TMGSTAGE=$PIECE(ZN,"^",2) QUIT:TMGSTAGE=""
        . SET TMGHL7MSG("STAGE")=TMGSTAGE
        . SET TMGRESULT=$$XFMSG(.TMGENV,.TMGHL7MSG) 
        QUIT TMGRESULT
        ;
XFMSG(TMGENV,TMGHL7MSG) ;"
        ;"Purpose: To take parsed message array, and applied transforms
        ;"Input: TMGENV -- PASS BY REFERENCE.  The lab environment array.  
        ;"       TMGHL7MSG -- PASS BY REFERENCE.  Format -- See PARSEMSG^TMGHL7X2
        ;"Result: 1 if OK, or -1^message IF problem.
        NEW TMGERR SET TMGERR=""
        NEW IEN22720 SET IEN22720=TMGENV("IEN 22720")  ;"IEN in file TMG HL7 MESSAGE TRANSFORM SETTINGS
        NEW TMGRESULT SET TMGRESULT=1
        IF $GET(HLREC("SAN"))="" SET HLREC("SAN")=$PIECE($GET(^TMG(22720,IEN22720,0)),"^",1)
        NEW TMGU MERGE TMGU=TMGENV("TMGU")
        ;"------------ Entire message prerun code ------------
        NEW TMGCODREF SET TMGCODREF=$NAME(^TMG(22720,IEN22720,10))
        NEW TMGCODE SET TMGCODE=$GET(@TMGCODREF)
        IF TMGCODE'="" DO
        . SET TMGRESULT=$$XECCODE(TMGCODE)
        . IF TMGRESULT=1 DO REFRESHM^TMGHL7X2(.TMGHL7MSG,.TMGU)
        IF TMGRESULT<0 DO  GOTO XFMP  
        . ;"Even though error, still run postrun code, because it might be needed 
        . ;"to clean up globally-scoped vars used by transform code
        . SET TMGERR=TMGRESULT
        ;"------------ Code for each segment ---------------
        NEW TMGORD SET TMGORD=0
        FOR  SET TMGORD=$ORDER(TMGHL7MSG("PO",TMGORD)) QUIT:(+TMGORD'>0)!(TMGRESULT<0)  DO
        . SET TMGSEGN=TMGHL7MSG("PO",TMGORD)
        . NEW SEGNAME SET SEGNAME=$GET(TMGHL7MSG(TMGSEGN,"SEG")) QUIT:SEGNAME=""
        . NEW IENSEG SET IENSEG=+$ORDER(^TMG(22720,IEN22720,11,"B",SEGNAME,0))
        . IF IENSEG'>0 QUIT
        . SET TMGRESULT=$$XFSEG(IEN22720,IENSEG,TMGSEGN,.TMGHL7MSG,.TMGU) ;"transform segments (and from there all subnodes)
        IF TMGRESULT<0 DO  GOTO XFMP
        . ;"Even though error, still run postrun code, because it might be needed 
        . ;"to clean up globally-scoped vars
        . SET TMGERR=TMGRESULT
XFMP    ;"------------ Entire message postrun code ------------
        NEW TMGCODREF SET TMGCODREF=$NAME(^TMG(22720,IEN22720,12))
        SET TMGCODE=$GET(@TMGCODREF)
        IF TMGCODE'="" DO
        . SET TMGRESULT=$$XECCODE(TMGCODE)
        . IF TMGRESULT=1 DO REFRESHM^TMGHL7X2(.TMGHL7MSG,.TMGU)
XFDN    IF TMGERR'="" SET TMGRESULT=TMGERR
        QUIT TMGRESULT
        ;
XFSEG(IEN22720,IENSEG,TMGSEGN,TMGHL7MSG,TMGU) ;
        ;"Purpose: To take parsed message array, and applied transforms
        ;"Input: IEN22720 -- IEN in file TMG HL7 MESSAGE TRANSFORM SETTINGS
        ;"       IENSEG -- the subrecord in file 22720 to handle TMGSEGN
        ;"       TMGSEGN -- The segment number being handled.
        ;"       TMGHL7MSG -- PASS BY REFERENCE.  Format -- See PARSEMSG above.
        ;"       TMGU -- Array with divisor characters
        ;"Result: 1 if OK, or -1^message IF problem.
        NEW TMGRESULT SET TMGRESULT=1
        NEW TMGVALUE,TMGOLDVAL
        ;"------------ Entire seg, prerun code ------------
        SET (TMGVALUE,TMGOLDVAL)=$$STRIPSN(TMGSEGN,.TMGHL7MSG,.TMGU) ;"first piece with segment name (e.g. 'OBX') removed.
        NEW TMGCODREF SET TMGCODREF=$NAME(^TMG(22720,IEN22720,11,IENSEG,10))
        NEW TMGCODE SET TMGCODE=$GET(@TMGCODREF)
        IF TMGCODE'="" DO
        . SET TMGRESULT=$$XECCODE(TMGCODE,.TMGVALUE,TMGSEGN)
        . IF (TMGRESULT=1),(TMGVALUE'=TMGOLDVAL) DO STORSTPD(TMGVALUE,TMGSEGN,.TMGHL7MSG,.TMGU) ;"Refreshed in procedure
        IF TMGRESULT<0 GOTO XFSDN
        ;"------------ Code for each field ---------------
        IF $ORDER(^TMG(22720,IEN22720,11,IENSEG,0))'>0 GOTO XFS2  ;"No entries for field transforms. 
        NEW TMGFLDN SET TMGFLDN=0
        FOR  SET TMGFLDN=$ORDER(TMGHL7MSG(TMGSEGN,TMGFLDN)) QUIT:(+TMGFLDN'>0)!(TMGRESULT<0)  DO
        . IF $DATA(^TMG(22720,IEN22720,11,IENSEG,11,TMGFLDN))=0 QUIT
        . SET TMGRESULT=$$XFFLD(IEN22720,IENSEG,TMGSEGN,TMGFLDN,.TMGHL7MSG,.TMGU)
        IF TMGRESULT<0 GOTO XFSDN
        ;"------------ Entire seg, postrun code ------------
XFS2    SET (TMGVALUE,TMGOLDVAL)=$$STRIPSN(TMGSEGN,.TMGHL7MSG,.TMGU) ;"first piece with segment name (e.g. 'OBX') removed.
        SET TMGCODREF=$NAME(^TMG(22720,IEN22720,11,IENSEG,12))
        SET TMGCODE=$GET(@TMGCODREF)
        IF TMGCODE'="" DO
        . SET TMGRESULT=$$XECCODE(TMGCODE,.TMGVALUE,TMGSEGN)
        . IF (TMGRESULT=1),(TMGVALUE'=TMGOLDVAL) DO
        . . DO STORSTPD(TMGVALUE,TMGSEGN,.TMGHL7MSG,.TMGU) ;"Refreshed in procedure
        . . DO REFRESHM^TMGHL7X2(.TMGHL7MSG,.TMGU,TMGSEGN)
XFSDN   QUIT TMGRESULT
        ;
XFFLD(IEN22720,IENSEG,TMGSEGN,TMGFLDN,TMGHL7MSG,TMGU) ;
        ;"Purpose: To take parsed message array, and apply transforms
        ;"Input: IEN22720 -- IEN in file TMG HL7 MESSAGE TRANSFORM SETTINGS
        ;"       IENSEG -- the subrecord in file 22720 to handle TMGSEGN
        ;"       TMGSEGN -- the segment number being handled
        ;"       TMGFLDN -- The field number being handled.
        ;"       TMGHL7MSG -- PASS BY REFERENCE.  Format -- See PARSEMSG above.
        ;"       TMGU -- Array with divisor characters
        ;"Result: 1 if OK, or -1^message IF problem, or 0 IF nothing done
        NEW TMGRESULT SET TMGRESULT=1
        NEW TMGVALUE,TMGOLDVAL
        ;"------------ Entire field, prerun code ------------
        SET (TMGVALUE,TMGOLDVAL)=$GET(TMGHL7MSG(TMGSEGN,TMGFLDN))
        NEW TMGCODREF SET TMGCODREF=$NAME(^TMG(22720,IEN22720,11,IENSEG,11,TMGFLDN,10))
        NEW TMGCODE SET TMGCODE=$GET(@TMGCODREF)
        IF TMGCODE'="" DO
        . SET TMGRESULT=$$XECCODE(TMGCODE,.TMGVALUE,TMGFLDN)
        . IF (TMGRESULT=1),(TMGVALUE'=TMGOLDVAL) DO
        . . SET TMGHL7MSG(TMGSEGN,TMGFLDN)=TMGVALUE
        . . DO REFRESHM^TMGHL7X2(.TMGHL7MSG,.TMGU,TMGSEGN,TMGFLDN)
        IF TMGRESULT<0 GOTO XFFDN
        ;"------------ Code for each component ---------------
        IF $ORDER(^TMG(22720,IEN22720,11,IENSEG,11,TMGFLDN,0))'>0 GOTO XFF2  ;"No entries for component transforms. 
        NEW TMGCOMPN SET TMGCOMPN=0
        FOR  SET TMGCOMPN=$ORDER(TMGHL7MSG(TMGSEGN,TMGFLDN,TMGCOMPN)) QUIT:(+TMGCOMPN'>0)!(TMGRESULT<0)  DO
        . SET TMGRESULT=$$XFCOMP(IEN22720,IENSEG,TMGSEGN,TMGFLDN,TMGCOMPN,.TMGHL7MSG,.TMGU)
        IF TMGRESULT<0 GOTO XFFDN
        ;"------------ Entire seg, postrun code ------------
XFF2    SET (TMGVALUE,TMGOLDVAL)=$GET(TMGHL7MSG(TMGSEGN,TMGFLDN))
        NEW TMGCODREF SET TMGCODREF=$NAME(^TMG(22720,IEN22720,11,IENSEG,11,TMGFLDN,12))
        SET TMGCODE=$GET(@TMGCODREF)
        IF TMGCODE'="" DO
        . SET TMGRESULT=$$XECCODE(TMGCODE,.TMGVALUE,TMGFLDN)
        . IF (TMGRESULT=1),(TMGVALUE'=TMGOLDVAL) DO
        . . SET TMGHL7MSG(TMGSEGN,TMGFLDN)=TMGVALUE
        . . DO REFRESHM^TMGHL7X2(.TMGHL7MSG,.TMGU,TMGSEGN,TMGFLDN)
XFFDN   QUIT TMGRESULT
        ;
XFCOMP(IEN22720,IENSEG,TMGSEGN,TMGFLDN,TMGCOMPN,TMGHL7MSG,TMGU) ;
        ;"Purpose: To take parsed message array, and applied transforms
        ;"Input: IEN22720 -- IEN in file TMG HL7 MESSAGE TRANSFORM SETTINGS
        ;"       IENSEG -- the subrecord in file 22720 to handle TMGSEGN
        ;"       TMGSEGN -- the segment number being handled
        ;"       TMGFLDN -- The field number being handled.
        ;"       TMGCOMPN -- the number of the component being handled.
        ;"       TMGHL7MSG -- PASS BY REFERENCE.  Format -- See PARSEMSG above.
        ;"       TMGU -- Array with divisor characters
        ;"Result: 1 if OK, or -1^message IF problem.
        NEW TMGRESULT SET TMGRESULT=1
        NEW TMGVALUE,TMGOLDVAL
        ;"------------ Entire component, prerun code ------------
        SET (TMGVALUE,TMGOLDVAL)=$GET(TMGHL7MSG(TMGSEGN,TMGFLDN,TMGCOMPN))
        NEW TMGCODREF SET TMGCODREF=$NAME(^TMG(22720,IEN22720,11,IENSEG,11,TMGFLDN,11,TMGCOMPN,10))
        NEW TMGCODE SET TMGCODE=$GET(@TMGCODREF)
        IF TMGCODE'="" DO
        . SET TMGRESULT=$$XECCODE(TMGCODE,.TMGVALUE,TMGCOMPN)
        . IF (TMGRESULT=1),(TMGVALUE'=TMGOLDVAL) DO
        . . SET TMGHL7MSG(TMGSEGN,TMGFLDN,TMGCOMPN)=TMGVALUE
        . . DO REFRESHM^TMGHL7X2(.TMGHL7MSG,.TMGU,TMGSEGN,TMGFLDN,TMGCOMPN)
        IF TMGRESULT<0 GOTO XFCDN
        ;"------------ Code for each sub-component ---------------
        IF $ORDER(^TMG(22720,IEN22720,11,IENSEG,11,TMGFLDN,11,TMGCOMPN,0))'>0 GOTO XFC2  ;"No entries for component transforms.         
        NEW TMGSCMPN SET TMGSCMPN=0
        FOR  SET TMGSCMPN=$ORDER(TMGHL7MSG(TMGSEGN,TMGCOMPN,TMGSCMPN)) QUIT:(+TMGSCMPN'>0)!(TMGRESULT<0)  DO
        . SET TMGRESULT=$$XFSCMP(IEN22720,IENSEG,TMGSEGN,TMGFLDN,TMGCOMPN,TMGSCMPN,.TMGHL7MSG,.TMGU)
        IF TMGRESULT<0 GOTO XFCDN
        ;"------------ Entire component, postrun code ------------
XFC2    SET TMGVALUE=$GET(TMGHL7MSG(TMGSEGN,TMGFLDN,TMGCOMPN))
        NEW TMGCODREF SET TMGCODREF=$NAME(^TMG(22720,IEN22720,11,IENSEG,11,TMGFLDN,11,TMGCOMPN,12))
        SET TMGCODE=$GET(@TMGCODREF)
        IF TMGCODE'="" DO
        . SET TMGRESULT=$$XECCODE(TMGCODE,.TMGVALUE,TMGCOMPN)
        . IF (TMGRESULT=1),(TMGVALUE'=TMGOLDVAL) DO
        . . SET TMGHL7MSG(TMGSEGN,TMGFLDN,TMGCOMPN)=TMGVALUE
        . . DO REFRESHM^TMGHL7X2(.TMGHL7MSG,.TMGU,TMGSEGN,TMGFLDN,TMGCOMPN)
        ;
XFCDN   QUIT TMGRESULT
        ;
XFSCMP(IEN22720,IENSEG,TMGSEGN,TMGFLDN,TMGCOMPN,TMGSCMPN,TMGHL7MSG,TMGU) ;
        ;"Purpose: To take parsed message array, and applied transforms
        ;"Input: IEN22720 -- IEN in file TMG HL7 MESSAGE TRANSFORM SETTINGS
        ;"       IENSEG -- the subrecord in file 22720 to handle TMGSEGN
        ;"       TMGSEGN -- the segment number being handled
        ;"       TMGFLDN -- The field number being handled.
        ;"       TMGCOMPN -- the number of the component being handled.
        ;"       TMGSCMPN -- the number of the sub-component being handled.
        ;"       TMGHL7MSG -- PASS BY REFERENCE.  Format -- See PARSEMSG above.
        ;"       TMGU -- Array with divisor characters
        ;"Result: 1 if OK, or -1^message IF problem.
        ;"------------ Entire sub-component run code ------------
        NEW TMGVALUE,TMGOLDVAL
        SET (TMGVALUE,TMGOLDVAL)=$GET(TMGHL7MSG(TMGSEGN,TMGFLDN,TMGCOMPN,TMGSCMPN))
        NEW TMGCODREF SET TMGCODREF=$NAME(^TMG(22720,IEN22720,11,IENSEG,11,TMGFLDN,11,TMGCOMPN,11,TMGSCMPN,10))        
        NEW TMGCODE SET TMGCODE=$GET(@TMGCODREF)
        IF TMGCODE'="" DO
        . SET TMGRESULT=$$XECCODE(TMGCODE,.TMGVALUE,TMGSCMPN)
        . IF (TMGRESULT=1),(TMGVALUE'=TMGOLDVAL) DO
        . . SET TMGHL7MSG(TMGSEGN,TMGFLDN,TMGCOMPN,TMGSCMPN)=TMGVALUE
        . . DO REFRESHM^TMGHL7X2(.TMGHL7MSG,.TMGU,TMGSEGN,TMGFLDN,TMGCOMPN,TMGSCMPN)
        QUIT TMGRESULT
        ;
XECCODE(TMGCODE,TMGVALUE,TMGNUM) ;
        ;"Purpose: launch user code
        ;"Results: 1 if OK, 0 IF nothing done, -1^Message IF error
        NEW TMGRESULT SET TMGRESULT=0
        SET TMGVALUE=$GET(TMGVALUE) ;"Make available for called code
        SET TMGNUM=$GET(TMGNUM) ;"Make available for called code
        ;"NEW TMGCODE SET TMGCODE=$GET(@TMGCODREF)
        ;"IF TMGCODE="" GOTO XCDDN
        SET TMGRESULT=1
        NEW TMGXERR
        DO
        . ;"NEW $ETRAP
        . ;"SET $ETRAP="SET TMGXERR=""Error trapped executing code: '""_TMGCODE_""' "
        . ; SET $ETRAP=$ETRAP_"$ZSTATUS=""_$ZSTATUS_""; $ECODE=""_$ECODE "
        . ;"SET $ETRAP=$ETRAP_"SET $ETRAP="""",$ECODE="""" "
        . ;"DO TMGLOG^HLCSTCP1("In XECODE^TMGHL7X: about to execute code: ["_TMGCODE_"]")
        . XECUTE TMGCODE
        . ;"DO TMGLOG^HLCSTCP1("In XECODE^TMGHL7X: Safe return from: ["_TMGCODE_"]")
        IF $DATA(TMGXERR) SET TMGRESULT="-1^"_TMGXERR
XCDDN   QUIT TMGRESULT
        ;
STRIPSN(TMGSEGN,TMGHL7MSG,TMGU) ;
        ;"Purpose: to return segment, with first piece with segment name ('OBX') removed.
        NEW TMGRESULT SET TMGRESULT=$GET(TMGHL7MSG(TMGSEGN))
        SET TMGRESULT=$PIECE(TMGRESULT,TMGU(1),2,999)
        QUIT TMGRESULT
        ;
STORSTPD(S,TMGSEGN,TMGHL7MSG,TMGU) ;
        ;"Purpose: Store a stripped segment (i.e. without it's first piece with name, e.g. 'OBX')
        ;"         back into place, using name in the SEG node.  If SEG node not found, then
        ;"         function will exit without storing.
        ;"NOTE: This function WILL refresh the array, parsing changes into subnodes.
        NEW SEGNAME SET SEGNAME=$GET(TMGHL7MSG(TMGSEGN,"SEG")) GOTO:(SEGNAME="") STSPD
        SET TMGHL7MSG(TMGSEGN)=SEGNAME_TMGU(1)_S
        DO REFRESHM^TMGHL7X2(.TMGHL7MSG,.TMGU,TMGSEGN)
STSPD   QUIT
