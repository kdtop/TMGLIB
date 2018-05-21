TMGPXRF1 ;TMG/kst/TMG Reminder Computed Function Findings Utility Stuff ;10/5/13
         ;;1.0;TMG-LIB;**1**;5/21/13
 ;
 ;"TMG REMINDER COMPUTED FUNCTION FINDINGS UTILITIES
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
SAVEPARM ;"SAVE REMINDER ENGINE PARAMS
         NEW TMGSTACK
         KILL ^TMG("TMP","SAVEPARM^TMGPXRF1")
         MERGE ^TMG("TMP","SAVEPARM^TMGPXRF1","LIST")=LIST
         MERGE ^TMG("TMP","SAVEPARM^TMGPXRF1","FIEVAL")=FIEVAL          
         MERGE ^TMG("TMP","SAVEPARM^TMGPXRF1","PXRMITEM")=PXRMITEM          
         MERGE ^TMG("TMP","SAVEPARM^TMGPXRF1","FFN")=FFN
         MERGE ^TMG("TMP","SAVEPARM^TMGPXRF1","DEFARR")=DEFARR
         MERGE ^TMG("TMP","SAVEPARM^TMGPXRF1","PXRMPDEM")=PXRMPDEM
         MERGE ^TMG("TMP","SAVEPARM^TMGPXRF1","TODAY")=TODAY
         QUIT
         ;
RESTPARM ;" RESTORE REMINDER ENGINE PARMS
         KILL LIST MERGE LIST=^TMG("TMP","SAVEPARM^TMGPXRF1","LIST")
         KILL FIEVAL MERGE FIEVAL=^TMG("TMP","SAVEPARM^TMGPXRF1","FIEVAL")          
         KILL PXRMITEM MERGE PXRMITEM=^TMG("TMP","SAVEPARM^TMGPXRF1","PXRMITEM")          
         KILL FFN MERGE FFN=^TMG("TMP","SAVEPARM^TMGPXRF1","FFN")
         KILL DEFARR MERGE DEFARR=^TMG("TMP","SAVEPARM^TMGPXRF1","DEFARR")
         KILL PXRMPDEM MERGE PXRMPDEM=^TMG("TMP","SAVEPARM^TMGPXRF1","PXRMPDEM")                  
         KILL TODAY MERGE TODAY=^TMG("TMP","SAVEPARM^TMGPXRF1","TODAY")
         QUIT
         ;
INDEX(FIEVAL) ;"SET UP INDEXES IN FIEVAL ARRAY
        ;"Input: FIEVAL -- Findings set up by Reminder engine.
        ;"                 See SCRIPT^TMGPXRF0 for format
        NEW IDX SET IDX=""
        FOR  SET IDX=$ORDER(FIEVAL(IDX)) QUIT:IDX'>0  DO
        . NEW STR SET STR=$GET(FIEVAL(IDX,"FINDING")) QUIT:STR=""
        . NEW IEN SET IEN=+STR
        . NEW REF SET REF="^"_$PIECE(STR,";",2)_IEN_",0)"
        . NEW ZN SET ZN=$GET(@REF) QUIT:ZN=""
        . NEW NAME SET NAME=$PIECE(ZN,"^",1)
        . SET FIEVAL("B",NAME,IDX)=""
        . SET FIEVAL("TMG NAME",IDX)=NAME
        QUIT
        ;
MRFIND(FIEVAL,FNAMESAR) ;"MOST RECENT FINDING IN ARRAY, RETURNING INDEX
        ;"Input: FIEVAL -- PASSED BY REFERENCE.  Findings set up by Reminder engine.
        ;"       FNAMESAR -- PASSED BY REFERENCE.  Format:
        ;"           FNAMESAR(<FINDING NAME>)=""
        ;"Result: returns ITEM INDEX NUMBER of finding with most recent date., 
        ;"        or 0 if none
        NEW TEMPARR,DATE
        IF $DATA(FIEVAL("B"))=0 DO INDEX(.FIEVAL)
        NEW TMGRESULT SET TMGRESULT=-1
        NEW NAME SET NAME=""
        FOR  SET NAME=$ORDER(FNAMESAR(NAME)) QUIT:NAME=""  DO
        . NEW IDX SET IDX=+$ORDER(FIEVAL("B",NAME,0)) 
        . QUIT:IDX'>0
        . SET DATE=$GET(FIEVAL(IDX,"DATE")) 
        . QUIT:DATE=""
        . SET TEMPARR(DATE,IDX)=""
        SET DATE=$ORDER(TEMPARR(""),-1)
        SET TMGRESULT=+$ORDER(TEMPARR(DATE,0))
MRFDDN  QUIT TMGRESULT
        ;
MRFNDN(FIEVAL,FNAMESAR) ;"MOST RECENT FINDING IN ARRAY, RETURNING NAME
        ;"Input: FIEVAL -- PASSED BY REFERENCE.  Findings set up by Reminder engine.
        ;"       FNAMESAR -- PASSED BY REFERENCE.  Format:
        ;"           FNAMESAR(<FINDING NAME>)=""
        ;"Result: returns NAME of finding with most recent date., 
        ;"        or "" if none
        NEW TMGRESULT SET TMGRESULT=""        
        NEW IDX SET IDX=$$MRFIND^TMGPXRF1(.FIEVAL,.FNAMESAR)
        IF IDX>0 SET TMGRESULT=$$GETIXNAM^TMGPXRF1(.FIEVAL,IDX)
        QUIT TMGRESULT
        ;
INTRVLST(NAME,PNUM,YR,MO,DAY) ;"INTERVAL FROM STRING
        ;"Purpose: Turn a name with follow up intervals into variable results
        ;"Input: NAME -- Factor name.  Must be in format of :
        ;"               xx xx xx xx 5 YR xxx xxx
        ;"               I.e. number must be one piece, and month / yr as next piece
        ;"               Only 1st letter of 2nd piece is checked. (Not case sensitive)
        ;"                 Y=year, M=month, D=day
        ;"       PNUM -- The piece of the number, using space (' ') as divisor
        ;"       DAY,HR,MIN,SEC -- PASS BY REFERENCE.  OUT PARAMETERS.  
        ;"Result: None, OUT PARAMETERS are filled. 
        NEW TMGRESULT SET TMGRESULT=0
        SET PNUM=+$GET(PNUM)
        NEW NUM SET NUM=+$PIECE(NAME," ",PNUM)
        NEW UNIT SET UNIT=$EXTRACT($$UP^XLFSTR($PIECE(NAME," ",PNUM+1)),1)
        SET (YR,MO,DAY)=0
        IF UNIT="Y" SET YR=NUM
        ELSE  IF UNIT="M" SET MO=NUM
        ELSE  IF UNIT="D" SET DAY=NUM
FUITDN  QUIT        
        ;
GETFIDX(FIEVAL,NAME) ;"Get factor name index
        IF $DATA(FIEVAL("B"))=0 DO INDEX(.FIEVAL)
        QUIT +$ORDER(FIEVAL("B",$GET(NAME),0))
        ;
GETIXNAM(FIEVAL,IDX) ;"Get name of factor at index position
        IF $DATA(FIEVAL("B"))=0 DO INDEX(.FIEVAL)
        QUIT $GET(FIEVAL("TMG NAME",IDX))
        ;
MRDFN(FIEVAL,NAME) ;"Return most recent date for factor name
        IF $DATA(FIEVAL("B"))=0 DO INDEX(.FIEVAL)
        NEW LIST SET LIST(0)=1
        SET LIST(1)=+$ORDER(FIEVAL("B",$GET(NAME),0))
        NEW TMGRESULT DO MRD^PXRMFF0(.LIST,.FIEVAL,.TMGRESULT)
        QUIT TMGRESULT
        ;
