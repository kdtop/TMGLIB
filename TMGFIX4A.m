TMGFIX4A ;TMG/kst/Fixes for converting old labs ; 9/25/13, 2/2/14
         ;;1.0;TMG-LIB;**1**;9/25/13
 ;
 ;"FIXES related to TMG TIU PXRM TABLES
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
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ; 
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"   TMGSTUT2, TMGSTUT3
 ;"=======================================================================
 ;"
 ;"=======================================================================
 ;
;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"DOSCAN -- Fix patient's TOBACCO entry in the text table.
 ;"FIXEKG(DFN) -- 
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"GETHDR(TEXT) --LOAD UP HEADER ARRAY
 ;"MAKENOTE(VALUE,YN,DFN,TXTARRAY) -- compose a note with information for NEW table.
 ; 
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"=======================================================================
 ;"   C0QMU12, TMGSTUT2, TMGUSRI2, TMGMISC2, TMGC0Q*, TMGPXRU1
 ;"=======================================================================
 ;
DOSCAN  ;"Purpose: To centralized EKG's into one place: the Studies table.
        ;"         Currently they are found in the HTN, DM, and sometimes STUDIES table
        ;"
        KILL ^TMG("TMP","TOBACCO")
        NEW STIME SET STIME=$H
        NEW DONE SET DONE=0
        NEW DFNMAX SET DFNMAX=$ORDER(^DPT("!"),-1)
        NEW DFN SET DFN=0
        FOR  SET DFN=$ORDER(^DPT(DFN)) QUIT:(+DFN'>0)  DO
        . IF DFN#5=0 DO
        . . DO PROGBAR^TMGUSRI2(DFN,"PROGRESS: "_DFN,0,DFNMAX,60,STIME)
        . . IF $$USRABORT^TMGUSRI2("scanning patients") SET DONE=1
        . DO FIXEKG(DFN)
        QUIT
        ;
FIXEKG(DFN) ;
        ;
        NEW DM2ARR,HTNARR,STUDIESARR,TEMPS
        NEW EKGVAL SET EKGVAL=""
        NEW ONLYSTUDIESTABLE SET ONLYSTUDIESTABLE=1
        SET TEMPS=$$GETTABLX^TMGTIUO6(DFN,"HYPERTENSION",.HTNARR)
        SET TEMPS=$$TRIM^XLFSTR($GET(HTNARR("KEY-VALUE","EKG")))
        IF "<NO DATA>."[$$UP^XLFSTR(TEMPS) SET TEMPS=""
        IF TEMPS'="" DO
        . SET EKGVAL=TEMPS
        . SET ONLYSTUDIESTABLE=0
        SET TEMPS=$$GETTABLX^TMGTIUO6(DFN,"DIABETIC STUDIES",.DM2ARR)
        SET TEMPS=$$TRIM^XLFSTR($GET(DM2ARR("KEY-VALUE","EKG")))
        IF "<NO DATA>."[$$UP^XLFSTR(TEMPS) SET TEMPS=""
        IF EKGVAL[TEMPS SET TEMPS=""
        IF TEMPS'="" DO
        . IF EKGVAL'="" SET EKGVAL=EKGVAL_"; "
        . SET EKGVAL=EKGVAL_TEMPS
        . SET ONLYSTUDIESTABLE=0
        SET TEMPS=$$GETTABLX^TMGTIUO6(DFN,"STUDIES",.STUDIESARR)
        SET TEMPS=$$TRIM^XLFSTR($GET(STUDIESARR("KEY-VALUE","EKG")))
        IF "<NO DATA>."[$$UP^XLFSTR(TEMPS) SET TEMPS=""
        IF EKGVAL[TEMPS SET TEMPS=""
        IF TEMPS'="" DO
        . IF EKGVAL'="" SET EKGVAL=EKGVAL_"; "
        . SET EKGVAL=EKGVAL_TEMPS
        IF (EKGVAL'="")&(ONLYSTUDIESTABLE=0) DO
        . WRITE $P(^DPT(DFN,0),"^",1)," -- "
        . WRITE "EKG: ",EKGVAL,"                              ",!
        . NEW TXTARR
        . DO MAKENOTE(.STUDIESARR,EKGVAL,.TXTARRAY)  ;
        . NEW FORCE SET FORCE=1
        . NEW DIDSAVE SET DIDSAVE=$$SAVENOTE^TMGFIX3(DFN,.TXTARRAY,FORCE)
        QUIT
        ;
HEADER  ;
        ;;"  Standardizing notation of EKG studies...
        ;;"  Patient's prior data was reformatted into table below.
        ;;"
        ;;"<END>
        ;
GETHDR(TEXT,LABEL) ;"LOAD UP HEADER ARRAY
        ;"Results: TEXT is loaded as follows:
        ;"     TEXT(#,0)=<line text>
        NEW DONE SET DONE=0
        KILL TEXT
        NEW CT SET CT=1
        NEW I FOR I=1:1 DO  QUIT:DONE
        . NEW S SET S=$TEXT(@LABEL+I)
        . IF S="" SET DONE=1 QUIT
        . SET S=$PIECE(S,";;""",2)
        . IF S="<END>" SET DONE=1 QUIT
        . SET TEXT(CT,0)=S
        . SET CT=CT+1
        QUIT
        ;
MAKENOTE(STUDIESARR,VALUE,TXTARRAY)  ;
        ;"Purpose: to compose a note with information for NEW table.
        ;"Input: VALUE -- PASS BY REFERENCE, to get NEW value back out.
        KILL TXTARRAY
        DO GETHDR(.TXTARRAY,"HEADER")
        NEW KEY SET KEY="EKG"
        NEW SPACES SET SPACES="     "
        SET STUDIESARR("KEY-VALUE",$$UP^XLFSTR(KEY))=VALUE
        SET STUDIESARR("KEY-VALUE",$$UP^XLFSTR(KEY),"LINE")=SPACES_KEY_": "_VALUE
        NEW STR SET STR=$$ARRAY2ST^TMGTIUO4(.STUDIESARR,SPACES)
        SET STR=SPACES_"-- [STUDIES] ---------"_$CHAR(13,10)_STR
        NEW CT SET CT=+$ORDER(TXTARRAY(""),-1)+1
        NEW ARR DO SPLIT2AR^TMGSTUT2(STR,$CHAR(13)_$CHAR(10),.ARR,CT)
        SET CT=0 FOR  SET CT=$ORDER(ARR(CT)) QUIT:+CT'>0  DO
        . SET TXTARRAY(CT,0)=$GET(ARR(CT))
        NEW CT SET CT=+$ORDER(TXTARRAY(""),-1)+1
        NEW I FOR I=1:1:3 DO
        . SET CT=CT+1
        . SET TXTARRAY(CT,0)=" "
MNDN    QUIT
        ;
CHECKRAD  ;" TEMP REPORT TO CHECK THE RAD DAY CASE POINTERS FOR ERRORS
        ;"Purpose: Provide an interactive entry point for report, asking device.
        NEW %ZIS,IOP
        SET IOP="S121-LAUGHLIN-LASER"
        DO ^%ZIS  ;"standard device call
        IF POP DO  QUIT
        . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output.  Aborting.")
        use IO
        NEW DFN SET DFN=0
        FOR  SET DFN=$O(^RADPT("B",DFN)) QUIT:DFN'>0  DO
        . NEW DISPLAYNAME SET DISPLAYNAME=0
        . NEW DT SET DT=9999999
        . FOR  SET DT=$O(^RADPT(DFN,"DT",DT),-1) QUIT:DT'>0  DO
        . . NEW IDX SET IDX=0
        . . FOR  SET IDX=$O(^RADPT(DFN,"DT",DT,"P",IDX)) QUIT:IDX'>0  DO
        . . . NEW IEN73
        . . . SET IEN73=+$P($G(^RADPT(DFN,"DT",DT,"P",IDX,0)),"^",17)
        . . . IF IEN73'>0 QUIT
        . . . ;"TEST THE PATIENT FOR THE POINTED TO REPORT
        . . . NEW TEMPDFN SET TEMPDFN=$P($G(^RARPT(IEN73,0)),"^",2)
        . . . IF TEMPDFN'=DFN DO
        . . . . IF DISPLAYNAME=0 DO
        . . . . . WRITE "==",$P($G(^DPT(DFN,0)),"^",1),!
        . . . . . SET DISPLAYNAME=1
        . . . . WRITE "      !! DAYCASE # ",$P($G(^RARPT(IEN73,0)),"^",1)
        . . . . WRITE " ASSIGNED TO ",$P($G(^DPT(TEMPDFN,0)),"^",1),!
        . . . . WRITE "         *DATE: ",$$EXTDATE^TMGDATE(9999999-$P(DT,".",1),1),". FILE 73 IEN: ",IEN73,!
        . IF DISPLAYNAME=1 WRITE !
        DO ^%ZISC  ;" Close the output device
        QUIT
        ;"