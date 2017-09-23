TMGUSRI3 ;TMG/kst/SACC-compliant USER INTERFACE API FUNCTIONS ;10/8/12, 2/2/14
         ;;1.0;TMG-LIB;**1,17**;07/17/12
 ;
 ;"TMG USER INTERFACE API FUNCTIONS
 ;"SACC-Compliant version
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"NOTE: This will contain SACC-compliant versions of code from TMGUSRIF
 ;"      If routine is not found here, the please migrate the code
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"IENSELCT(PIENARRAY,PRESULTS,FILE,FIELDS,WIDTHS,HEADER,SORTFIELDS,SAVEARRAY) -- allow selecting records from an IEN array
 ;"SELECTOR(PARRAY,PRESULTS,HEADER) -- Use VPE Selector code to select from an array
 ;"SELECTR2(PARRAY,PRESULTS,HEADER) -- Use VPE Selector code to select from an array
 ;
 ;"=======================================================================
 ;"Private Functions
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"DEPENDENCIES:  TMGDEBU2, TMGMISC2, %ZVEMKT (VPE Environment) 
 ;"=======================================================================
 ;
 ;"Note: Rules of use for VPE Selector...
 ;
 ;"  REF must=^TMP("VEE",$J)
 ;"  Each line should be in this format:
 ;"      @REF@(number)=ReturnValue_$CHAR(9)_DisplayValue
 ;"      @REF@(number)=ReturnValue_$CHAR(9)_DisplayValue
 ;"      @REF@(number)=ReturnValue_$CHAR(9)_DisplayValue
 ;"      Note: IF DisplayValue is to be divided into colums, then
 ;"            use | character to separate
 ;"  Results come back in:
 ;"      ^TMP("VPE","SELECT",$J,number)=ReturnValue_$CHAR(9)_DisplayValue
 ;"      ^TMP("VPE","SELECT",$J,number)=ReturnValue_$CHAR(9)_DisplayValue
 ;"      ^TMP("VPE","SELECT",$J,number)=ReturnValue_$CHAR(9)_DisplayValue
 ;"  To preselect entries, provide an array like this:
 ;"      array(number)=""  <-- number is same number as above, shows selected
 ;"      array(number)=""
 ;"      array(number)=""
 ;"      pass array by name:  SELECT^%ZVEMKT(REF,,"array")
 ;"  D SELECT^%ZVEMKT(REF,NUMBERLINES,ADDNEW,"PRESELARRAY")
 ;
 ;"=======================================================================
 ;
 ;"Was --> IENSelector(PIENARRAY,PRESULTS,FILE,FIELDS,WIDTHS,HEADER,SORTFIELDS,SAVEARRAY)
IENSELCT(PIENARRAY,PRESULTS,FILE,FIELDS,WIDTHS,HEADER,SORTFIELDS,SAVEARRAY) ;
        ;"Purpose: to allow selecting records from an IEN array
        ;"Input: PIENARRAY, PASS BY NAME.  An array of IENS to select from
        ;"       format:
        ;"              @PIENARRAY@(IEN)=""
        ;"              @PIENARRAY@(IEN)=""
        ;"              @PIENARRAY@(IEN,"SEL")="" ;"<-- Optional marker to have this preselected
        ;"       PRESULTS -- NAME OF array to have results returned in
        ;"              ** Note: Prior contents of array WILL be KILLED first
        ;"              Format of returned array:  Only those valuse that user selected will
        ;"              be aded to list
        ;"              @PRESULTS@(IEN)=DisplayLineNumber
        ;"              @PRESULTS@(IEN)=DisplayLineNumber
        ;"       FILE: The file number that IEN's are from.
        ;"       FIELDS: OPTIONAL. The Field(s) that should be shown for record. .01 is Default
        ;"              Fields may also be a ';' delimited list of Fields, e.g. ".01;.02;1".
        ;"       WIDTHS: Optional.  The widths of the columns to display Fields in.
        ;"              Format: e.g. "10;12;24" for three colums of widths:
        ;"                 Sequence must match sequence given in Fields
        ;"              Default is to evenly space colums
        ;"       HEADER -- OPTIONAL -- A header text to show.
        ;"       SORTFIELDS -- OPTIONAL -- Provide sorting fields
        ;"              Format: 'FldNum1;FldNum2;FldNum3...'
        ;"       SAVEARRAY -- OPTIONAL -- PASS BY REFERENCE,
        ;"              This variable will be filled with the NAME of the array
        ;"              used for displaying the array.  The FIRST time this function
        ;"              is called, this variable should = "".  On SUBSEQUENT calls,
        ;"              IF this variable holds the name of a variable (a reference), then
        ;"              that array will be used, rather than taking the time to create
        ;"              the display array again. Format of array:
        ;"              @SAVEARRAY(LineNumber)=IEN_$C(9)_Field1_"|"_Field2...
        ;"              @SAVEARRAY(LineNumber)=IEN_$C(9)_Field1_"|"_Field2...
        ;"              Note: The LineNumber is the same number as the DisplayLineNumber
        ;"                              returned in @PRESULTS@(IEN)=DisplayLineNUmber
        ;"Results: none
        IF $GET(PRESULTS)'="" KILL @PRESULTS
        NEW PRESELARRAY,REF
        IF $GET(SAVEARRAY)="" DO
        . SET REF=$NAME(^TMP("VEE",$J))
        . KILL @REF
        . SET SAVEARRAY=REF
        ELSE  DO  GOTO IS1  ;"Skip recreating array IF SAVEARRAY holds reference
        . SET REF=SAVEARRAY
        NEW REF2 SET REF2=$NAME(^TMG("TMP",$J,"IEN-SELECT"))
        KILL @REF2
        IF $GET(HEADER)'="" SET @REF@("HD")=HEADER
        SET IOM=$GET(IOM,80)
        SET FIELDS=$GET(FIELDS,".01")
        SET WIDTHS=$GET(WIDTHS)
        NEW SORT SET SORT=($DATA(SORTFIELDS)'=0)
        SET FILE=$GET(FILE)
        ;"Setup FLDARRAY.  Format:
        ;"      FLDARRAY=number of colums
        ;"      FLDARRAY(Sequence#)=field;fieldWidth
        NEW FLDARRAY,IDX
        SET FLDARRAY=0
        NEW WREMAIN SET WREMAIN=IOM
        FOR IDX=1:1:$LENGTH(FIELDS,";") DO
        . NEW FLD,W
        . SET FLD=$PIECE(FIELDS,";",IDX)
        . IF FLD="" QUIT
        . SET W=+$PIECE(WIDTHS,";",IDX)
        . IF W=0 DO
        . . IF FLDARRAY>0 SET W=IOM/FLDARRAY
        . . ELSE  SET W=20 ;"some arbitrary number
        . IF W>WREMAIN SET W=WREMAIN  ;"this isn't perfect
        . SET WREMAIN=WREMAIN-W
        . IF WREMAIN<1 SET WREMAIN=1
        . SET FLDARRAY(IDX)=FLD_";"_W
        . SET FLDARRAY=FLDARRAY+1
        ;
        NEW ITR,IEN,NAME,PRIORERRORFOUND
        NEW ABORT SET ABORT=0
        NEW ORDER SET ORDER=1
        NEW IENPRESELECTED
        WRITE "Prepairing list to display..."
        ;"SET IEN=$$ItrAInit^TMGITR(PIENARRAY,.ITR)
        ;"DO PrepProgress^TMGITR(.ITR,100,0,"IEN")
        WRITE !
        ;"IF IEN'="" FOR  DO  QUIT:($$ItrANext^TMGITR(.ITR,.IEN)="")!(ABORT=1)
        SET IEN=0
        FOR  SET IEN=$ORDER(@PIENARRAY@(IEN)) QUIT:(+IEN'>0)!(ABORT=1)  DO
        . NEW TMGOUT,TMGMSG,IENS,SHOWS
        . SET SHOWS=""
        . SET IENS=IEN_","
        . NEW TEMPFIELDS
        . SET IENPRESELECTED=($DATA(@PIENARRAY@(IEN,"SEL"))>0)
        . NEW IDX FOR IDX=1:1:FLDARRAY DO
        . . WRITE "."
        . . IF SHOWS'="" SET SHOWS=SHOWS_"|"
        . . NEW FLD,TEMPS
        . . SET FLD=$PIECE(FLDARRAY(IDX),";",1)
        . . SET TEMPS=$$GET1^DIQ(FILE,IENS,FLD,,"TMGOUT","TMGMSG")
        . . IF $PIECE($GET(^DD(FILE,FLD,0)),"^",2)["D" DO  ;"format dates for sorting IF in column 1
        . . . NEW %DT,X,Y
        . . . SET %DT="TS"
        . . . SET X=TEMPS
        . . . DO ^%DT ;"X in, Y out
        . . . SET TEMPS=$$DTFORMAT^TMGMISC2(Y,"yyyy mm/dd")  ;"make dates sort numerically
        . . IF $DATA(TMGMSG("DIERR")) DO  SET ABORT=1 QUIT
        . . . DO SHOWDIER^TMGDEBU2(.TMGMSG,.PRIORERRORFOUND)
        . . NEW W SET W=$PIECE(FLDARRAY(IDX),";",2)
        . . SET TEMPS=$EXTRACT(TEMPS,1,W)
        . . IF SORT SET TEMPFIELDS(FLD)=TEMPS
        . . SET SHOWS=SHOWS_$$LJ^XLFSTR(TEMPS,W," ")
        . IF SORT=0 DO
        . . SET @REF@(ORDER)=IEN_$CHAR(9)_SHOWS
        . . IF IENPRESELECTED SET PRESELARRAY(ORDER)=""
        . . SET ORDER=ORDER+1
        . ELSE  DO
        . . NEW TEMPREF SET TEMPREF=REF2
        . . FOR IDX=1:1:$LENGTH(SORTFIELDS,";") DO
        . . . NEW ONEFLD SET ONEFLD=$PIECE(SORTFIELDS,";",IDX)
        . . . NEW F SET F=$GET(TEMPFIELDS(ONEFLD))
        . . . IF F="" QUIT
        . . . SET TEMPREF=$NAME(@TEMPREF@(F))
        . . SET @TEMPREF@(IEN)=IEN_$CHAR(9)_SHOWS
        . . IF IENPRESELECTED SET @TEMPREF@(IEN,"SEL")=""
        . . ;"Sets up sorted variable as follows:
        . . ;"  @TEMPREF@(sortFLD1,sortFLD2,sortFLD3,IEN)='IEN_$CHAR(9)_SHOWS'
        . . ;"  @TEMPREF@(sortFLD1,sortFLD2,sortFLD3,IEN)='IEN_$CHAR(9)_SHOWS'
        ;"DO ProgressDone^TMGITR(.ITR)
        WRITE !
        ;
        IF ABORT=1 GOTO ISDONE
        ;
IES1    IF SORT=1 DO
        . WRITE "Sorting... "
        . SET ORDER=1
        . NEW TEMPREF2 SET TEMPREF2=REF2
        . NEW SHOWS,NUMNODES,DONE
        . SET DONE=0
        . FOR  DO  QUIT:(TEMPREF2="")!(DONE=1)
        . . SET TEMPREF2=$QUERY(@TEMPREF2)
        . . IF (TEMPREF2="") QUIT
        . . IF $QSUBSCRIPT(TEMPREF2,$QLENGTH(TEMPREF2))="SEL" DO  QUIT
        . . . SET PRESELARRAY(ORDER-1)=""
        . . IF (TEMPREF2'[$$OREF^DILF(REF2)) SET DONE=1 QUIT
        . . SET SHOWS=$GET(@TEMPREF2)
        . . SET @REF@(ORDER)=SHOWS
        . . SET ORDER=ORDER+1
IS1     ;
        NEW NUMBERLINES SET NUMBERLINES=0  ;"1--> number each line
        NEW ADDNEW SET ADDNEW=0 ;"1-> Allow adding NEW entry
        USE $I:(NOESCAPE)  ;"//kt 9/19/17
        WRITE "Passing off to selector..."
        DO SELECT^%ZVEMKT(REF,NUMBERLINES,ADDNEW,"PRESELARRAY")
        ;
        ;"Format results
        ;"NEW ITR2,INDEX
        ;"SET INDEX=$$ItrAInit^TMGITR($NAME(^TMP("VPE","SELECT",$J)),.ITR2)
        ;"IF INDEX'="" FOR  DO  QUIT:($$ItrANext^TMGITR(.ITR2,.INDEX)="")
        NEW INDEX SET INDEX=""
        FOR  SET INDEX=$ORDER(^TMP("VPE","SELECT",$J,INDEX)) QUIT:(INDEX="")  DO
        . NEW STR SET STR=$PIECE($GET(^TMP("VPE","SELECT",$J,INDEX)),$CHAR(9),1)
        . SET @PRESULTS@(STR)=INDEX
        KILL ^TMP("VPE","SELECT",$J)
        IF $GET(REF2) KILL @REF2  ;"i.e. ^TMG("TMP",$J,"IEN-SELECT")
        ;
ISDONE  QUIT
        ;
        ;
 ;"Selector(PARRAY,PRESULTS,HEADER)
SELECTOR(PARRAY,PRESULTS,HEADER) ;
        ;"Purpose: Interface with VPE Selector code to select from an array
        ;"Input: PARRAY -- NAME OF array holding items to be selected from
        ;"            Expected format:
        ;"              @PARRAY@("Display Choice Words")=ReturnValue  <-- ReturnValue is optional
        ;"              @PARRAY@("Display Choice Words")=ReturnValue
        ;"              @PARRAY@("Display Choice Words","SEL")="" <-- optional preselection indicator
        ;"       PRESULTS -- NAME OF array to have results returned in
        ;"              ** Note: Prior contents of array will NOT be KILLED first
        ;"              Format of returned array:  Only those valuse that user selected will be returned
        ;"              @PRESULTS@("Display Choice Words")=ReturnValue  <-- ReturnValue is optional
        ;"              @PRESULTS@("Display Choice Words")=ReturnValue
        ;"       HEADER -- OPTIONAL -- A header text to show.
        ;"Results: None
        NEW REF SET REF=$NAME(^TMP("VEE",$J))
        KILL @REF
        IF $GET(PARRAY)="" GOTO SELDONE
        IF $GET(PRESULTS)="" GOTO SELDONE
        NEW PRESELARRAY
        ;"First SET up array of options
        NEW RTNVALUE
        NEW ORDER SET ORDER=1
        NEW DISPWORDS SET DISPWORDS=""
        FOR  SET DISPWORDS=$ORDER(@PARRAY@(DISPWORDS)) QUIT:(DISPWORDS="")  DO
        . SET RTNVALUE=$GET(@PARRAY@(DISPWORDS),"<NONE>")
        . SET @REF@(ORDER)=RTNVALUE_$CHAR(9)_$EXTRACT(DISPWORDS,1,$GET(IOM,80))
        . IF $DATA(@PARRAY@(DISPWORDS,"SEL")) SET PRESELARRAY(ORDER)="" ;"mark as preselected
        . SET ORDER=ORDER+1
        IF $GET(HEADER)'="" SET @REF@("HD")=HEADER
        NEW NUMBERLINES SET NUMBERLINES=0  ;"1--> number each line
        NEW ADDNEW SET ADDNEW=0 ;"1-> Allow adding NEW entry
        ;
        DO SELECT^%ZVEMKT(REF,NUMBERLINES,ADDNEW,"PRESELARRAY")
        ;
        ;"Format selected options.
        ;"NEW INDEX SET INDEX=$ORDER(^TMP("VPE","SELECT",$J,""))
        ;"IF INDEX'="" FOR  DO  QUIT:(INDEX="")
        ;
        NEW INDEX SET INDEX=""
        FOR  SET INDEX=$ORDER(^TMP("VPE","SELECT",$J,INDEX)) QUIT:(INDEX="")  DO
        . NEW STR,STR1,STR2
        . SET STR=$GET(^TMP("VPE","SELECT",$J,INDEX))
        . SET STR1=$PIECE(STR,$CHAR(9),1)
        . SET STR2=$PIECE(STR,$CHAR(9),2)
        . SET @PRESULTS@(STR2)=STR1
        ;
        KILL ^TMP("VPE","SELECT",$J)
        KILL @REF
SELDONE QUIT
        ;
SELECTR2(PARRAY,PRESULTS,HEADER) ;
        ;"Purpose: Interface with VPE Selector code to select from an array
        ;"      Note: This allows a different format of input.  In Selector() above,
        ;"            it is NOT possible to have two similar Display Words with
        ;"            different return values.  E.g. two drugs with LISINOPRIL, but
        ;"            different IEN return values.  This fn allows this
        ;"Input: PARRAY -- NAME OF array holding items to be selected from
        ;"            Expected format:
        ;"              @PARRAY@("Display Choice Words",ReturnValue)="" <-- return value IS required
        ;"              @PARRAY@("Display Choice Words",ReturnValue)=""
        ;"              @PARRAY@("Display Choice Words",ReturnValue,"SEL")="" <-- optional preselection indicator
        ;"       PRESULTS -- NAME OF array to have results returned in
        ;"              ** Note: Prior contents of array will NOT be KILLED first
        ;"              Format of returned array:  Only those values that user selected will be returned
        ;"              @PRESULTS@("Display Choice Words",ReturnValue)=""
        ;"              @PRESULTS@("Display Choice Words",ReturnValue)=""
        ;"       HEADER -- OPTIONAL -- A header text to show.
        ;"Results: none
        NEW REF SET REF=$NAME(^TMP("VEE",$J))
        KILL @REF
        IF $GET(PARRAY)="" GOTO SL2DONE
        IF $GET(PRESULTS)="" GOTO SL2DONE
        NEW PRESELARRAY
        ;"First SET up array of options
        NEW DISPWORDS,RTNVALUE
        NEW ORDER SET ORDER=1
        SET DISPWORDS=""
        FOR  SET DISPWORDS=$ORDER(@PARRAY@(DISPWORDS)) QUIT:(DISPWORDS="")  DO
        . SET RTNVALUE=""
        . FOR  SET RTNVALUE=$ORDER(@PARRAY@(DISPWORDS,RTNVALUE)) QUIT:(RTNVALUE="")  DO
        . . SET @REF@(ORDER)=RTNVALUE_$CHAR(9)_$EXTRACT(DISPWORDS,1,$GET(IOM,80))
        . . IF $DATA(@PARRAY@(DISPWORDS,RTNVALUE,"SEL")) SET PRESELARRAY(ORDER)="" ;"mark as preselected
        . . SET ORDER=ORDER+1
        IF $GET(HEADER)'="" SET @REF@("HD")=HEADER
        NEW NUMBERLINES SET NUMBERLINES=0  ;"1--> number each line
        NEW ADDNEW SET ADDNEW=0 ;"1-> Allow adding NEW entry
        ;
        ;"Note: Rules of use:
        ;"  REF must=^TMP("VEE",$J)
        ;"  Each line should be in this format:
        ;"      @REF@(number)=ReturnValue_$CHAR(9)_DisplayValue
        ;"      @REF@(number)=ReturnValue_$CHAR(9)_DisplayValue
        ;"      @REF@(number)=ReturnValue_$CHAR(9)_DisplayValue
        ;"      Note: IF DisplayValue is to be divided into colums, then
        ;"            use | character to separate
        ;"  Results come back in:
        ;"      ^TMP("VPE","SELECT",$J,number)=ReturnValue_$CHAR(9)_DisplayValue
        ;"      ^TMP("VPE","SELECT",$J,number)=ReturnValue_$CHAR(9)_DisplayValue
        ;"      ^TMP("VPE","SELECT",$J,number)=ReturnValue_$CHAR(9)_DisplayValue
        ;"  To preselect entries, provide an array like this:
        ;"      array(number)=""  <-- number is same number as above, shows selected
        ;"      array(number)=""
        ;"      array(number)=""
        ;"      pass array by name:  SELECT^%ZVEMKT(REF,,"array")     
        USE $P  ;"//kt 9/19/17
        DO SELECT^%ZVEMKT(REF,NUMBERLINES,ADDNEW,"PRESELARRAY")
        ;
        ;"Format selected options.
        NEW INDEX SET INDEX=$ORDER(^TMP("VPE","SELECT",$J,""))
        IF INDEX'="" FOR  DO  QUIT:(INDEX="")
        . NEW STR,STR1,STR2
        . SET STR=$GET(^TMP("VPE","SELECT",$J,INDEX))
        . SET STR1=$PIECE(STR,$CHAR(9),1)
        . SET STR2=$PIECE(STR,$CHAR(9),2)
        . SET @PRESULTS@(STR2,STR1)=""
        . SET INDEX=$ORDER(^TMP("VPE","SELECT",$J,INDEX))
        ;
        KILL ^TMP("VPE","SELECT",$J)
        KILL @REF
        ;
SL2DONE QUIT
        ;
