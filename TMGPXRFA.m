TMGPXRFA ;TMG/kst/TMG Reminder Computed Function Findings Stuff ;10/5/13, 2/2/14
         ;;1.0;TMG-LIB;**1**;5/21/13
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
 ;"PUBLIC FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"PRIVATE FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"Dependancies : 
 ;"=======================================================================
 ;
COLNSCOP(LIST,FIEVAL,RESULT) ;"SET FOLLOWUP FREQUENCY FOR COLONOSCOPY.
        ;"Purpose: Entry point for COMPUTED FUNCTION FINDING
        ;"Handler for: file 811.925 (in 811.9), IENS 2,223, 'Colonoscopy Followup Interval Setter'
        ;"Input: LIST -- PASS BY REFERENCE.  Format
        ;"          LIST(0)=number of parameters specified for this function
        ;"          LIST(1)="<something>"  <-- the first parameter
        ;"          LIST(2)="<something>"  <-- the second parameter
        ;"       FIEVAL -- PASS BY REFERENCE.  Format:
        ;"         FIEVAL(#,"FINDING")=vptr to finding, e.g. "19;AUTTIMM("
        ;"         FIEVAL(#,#2)=1 or 0  (I think)
        ;"         FIEVAL("SEX")=# 1 or 0  (I think)
        ;"       RESULT -- OUT PARAMENTER.  Result for the function -- 0 or 1 
        ;"              RESULT = 0 means interval setter is inactive
        ;"              RESULT = 1 means interval setter is ACTIVE
        ;"This will be called from SCRIPT^TMGPXRF0
        ;"NOTE: Some globally-scoped variable may be depended upon
        ;"      PXRMITEM is defined, and is IEN of reminder definition being processed.
        ;"      FFN = the subIEN (field 25,FUNCTION FINDINGS).  Format: "FF#", e.g. "FF1" for subIEN=1
        ;"      DEFARR array contains the reminder definition raw field values etc.
        ;"      PXRMPDEM array contains patient demographics
        ;"      TODAY -- The date of the the evaluation
        ;"NOTE: To temporarily change the MIN AGE (fld #13), MAX AGE (fld #14), 
        ;"      or REMINDER FREQUENCY (fld #15) fields, then alter as follows:
        ;"         DEFARR(25,FFN,0), pieces 2, 3, and 4 respectively.
        ;"         This doesn't change definition stored in Fileman file, just 
        ;"         the array for this run
        ;"         FREQ (piece 4) must be nD, nM, or nY for days, months, years.
        NEW ZZDEBUG SET ZZDEBUG=0
        IF ZZDEBUG=0 DO SAVEPARM^TMGPXRF1
        IF ZZDEBUG=1 DO RESTPARM^TMGPXRF1
        ;"----------------------------------------------------------------
        SET RESULT=0 ;"Default of FALSE turns off this function, won't SET the reminder frequency. 
        DO INDEX^TMGPXRF1(.FIEVAL)
        NEW MRD SET MRD=$$MRDFN^TMGPXRF1(.FIEVAL,"TMG COLONOSCOPY COMPLETED")
        IF MRD=0 GOTO CLNSDN  ;"leaves frequency at baseline   
        NEW FACTORS DO LOADHFAR("TMG COLONOSCOPY FU",.FACTORS)
        NEW NAME SET NAME=$$MRFNDN^TMGPXRF1(.FIEVAL,.FACTORS) ;"Name of most recent finding.
        IF NAME="" GOTO CLNSDN  ;"leaves frequency at baseline
        NEW YR,MO,DAY DO INTRVLST^TMGPXRF1(NAME,4,.YR,.MO,.DAY) ;"FU interval from string
        NEW FREQ SET FREQ=$SELECT((YR>0):YR_"Y",(MO>0):MO_"M",(DAY>0):DAY_"D",1:"")
        SET $PIECE(DEFARR(25,FFN,0),"^",2)=50  ;"MIN age of 50 for routine colonoscopies
        SET $PIECE(DEFARR(25,FFN,0),"^",3)=75  ;"MAX age of 75 for routine colonoscopies
        SET $PIECE(DEFARR(25,FFN,0),"^",4)=FREQ
        SET DEFARR(25,FFN,1,1,0)="Patient marked for follow up "_FREQ_" after last colonoscopy"
        SET RESULT=1 ;"Enables this finding to SET the reminder frequency. 
CLNSDN  KILL FIEVAL("B"),FIEVAL("TMG NAME")
        QUIT
        ;
MAMMO(LIST,FIEVAL,RESULT) ;"SET FOLLOWUP FREQUENCY FOR MAMMOGRAM
        ;"Purpose: Entry point for COMPUTED FUNCTION FINDING
        ;"Handler for: file 811.925 (in 811.9), IENS 2,223, 'Mammogram Followup Interval Setter'
        ;"Input: same as for COLNSCOP^TMGPXRFA (see above) 
        NEW ZZDEBUG SET ZZDEBUG=0
        IF ZZDEBUG=0 DO SAVEPARM^TMGPXRF1
        IF ZZDEBUG=1 DO RESTPARM^TMGPXRF1
        ;"----------------------------------------------------------------
        SET RESULT=0 ;"Default of FALSE turns off this function, won't SET the reminder frequency. 
        DO INDEX^TMGPXRF1(.FIEVAL)
        NEW MRD SET MRD=$$MRDFN^TMGPXRF1(.FIEVAL,"TMG MAMMOGRAM/IMAGING DONE")
        IF MRD=0 GOTO MAMDN  ;"leaves frequency at baseline   
        NEW FACTORS DO LOADHFAR("TMG MAMMOGRAM/IMAGING FU",.FACTORS)
        NEW NAME SET NAME=$$MRFNDN^TMGPXRF1(.FIEVAL,.FACTORS) ;"Name of most recent finding.
        IF NAME="" GOTO MAMDN  ;"leaves frequency at baseline
        NEW YR,MO,DAY DO INTRVLST^TMGPXRF1(NAME,4,.YR,.MO,.DAY) ;"FU interval from string
        NEW FREQ SET FREQ=$SELECT((YR>0):YR_"Y",(MO>0):MO_"M",(DAY>0):DAY_"D",1:"")
        SET $PIECE(DEFARR(25,FFN,0),"^",2)=40  ;"MIN age of 40 for routine mammograms
        SET $PIECE(DEFARR(25,FFN,0),"^",3)=99  ;"MAX age of 99 for routine mammograms
        SET $PIECE(DEFARR(25,FFN,0),"^",4)=FREQ
        SET DEFARR(25,FFN,1,1,0)="Patient marked for follow up "_FREQ_" after last mammogram"
        SET RESULT=1 ;"Enables this finding to SET the reminder frequency. 
MAMDN   KILL FIEVAL("B"),FIEVAL("TMG NAME")
        QUIT
        ;        
LOADHFAR(PREFIX,ARR) ;
        ;"Purpose: Load array with up all health factors that start with PREFIX
        NEW PRE2 SET PRE2=$EXTRACT(PREFIX,1,$LENGTH(PREFIX)-1)
        SET PRE2=PRE2_$CHAR($ASCII($EXTRACT(PREFIX,$LENGTH(PREFIX)))-1)
        NEW NAME SET NAME=PRE2
        NEW DONE SET DONE=0
        FOR  SET NAME=$ORDER(^AUTTHF("B",NAME)) QUIT:(DONE=1)!(NAME="")  DO
        . IF $EXTRACT(NAME,1,$LENGTH(PREFIX))'=PREFIX SET DONE=1 QUIT
        . NEW IDX SET IDX=0 FOR  SET IDX=$ORDER(^AUTTHF("B",NAME,IDX)) Q:IDX'>0  DO
        . . NEW STR SET STR=$PIECE($GET(^AUTTHF(IDX,0)),"^",1) QUIT:STR=""
        . . SET ARR(STR)=""
        QUIT
        ;
