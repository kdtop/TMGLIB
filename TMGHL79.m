TMGHL79 ;TMG/kst-HL7 transformation engine processing ;6/15/2026
              ;;1.0;TMG-LIB;**1**;09/20/13
 ;
 ;"TMG HL7 TRANSFORMATION FUNCTIONS
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 3/27/2019  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"NOTE: this is code for working with labs from **[LabCorp lab]**
 ;"      FYI -- Pathgroup code is in TMGHL73
 ;"             Laughlin code is in TMGHL74
 ;"             Laughlin RADIOLOGY is in TMGHL74R
 ;"             Quest code is in TMGHL75
 ;"             common code is in TMGHL72
 ;"             GCHE LAB code is TMGHL76
 ;"             GCHE RADIOLOGY code is TMGHL76R
 ;"             LabCorp Web code is TMGHL78
 ;"             LabCorp is TMGHL79
 ;"=======================================================================
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"TEST  -- Pick file and manually send through filing process.   
 ;"BATCH -- Launch processing through all files in folder for laughlin lab
 ;"
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;"XMSG    -- Process entire message before processing segments
 ;"XMSH15  -- Process MSH segment, FLD 15
 ;"XMSH16  -- Process MSH segment, FLD 16
 ;"PID     -- transform the PID segment, esp SSN
 ;"XORC1   -- Process empty ORC message, field 1
 ;"XORC12  -- Process empty ORC message, field 12
 ;"XORC13  -- Process empty ORC message, field 13
 ;"OBR     -- setup for OBR fields.
 ;"OBR4    -- To transform the OBR segment, field 4
 ;"OBR15   -- Transform Secimen source
 ;"OBR16   -- Transform Ordering provider.
 ;"OBX3    -- transform the OBX segment, field 3 -- Observation Identifier
 ;"OBX5    -- transform the OBX segment, field 5 -- Observation value
 ;"OBX15   -- transform the OBX segment, field 15 ---- Producer's ID
 ;"OBX16   -- transform the OBX segment, field 16 ---- Responsibile Observer
 ;"OBX18   -- transform the OBX segment, field 18 ---- Equipment Identifier (EI)
 ;"NTE3    -- transform the NTE segment, field 3
 ;"XFTEST(FLDVAL,TMGU) -- convert test code into value acceptable to VistA
 ;"SUORL   -- Setup TMGINFO("ORL"), TMGINFO("LOC"), TMGINFO("INSTNAME")
 ;" 
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"TMGSTUTL, all the HL*, LA* code that the HL processing path normally calls.
 ;"=======================================================================
 ;
TEST  ;"Pick file and manually send through filing process.
        NEW OPTION SET OPTION("NO MOVE")=1
        SET OPTION("NO ALERT")=0
        SET LABCORPDISPNAME=1
        DO TEST^TMGHL71("/mnt/WinServer/LabCorp",.OPTION)
        KILL LABCORPDISPNAME
        QUIT
        ;
BATCH   ;"Launch processing through all files in folder.
        SET LABCORPDISPNAME=1
        DO HLDIRIN^TMGHL71("/mnt/WinServer/LabCorp",1000,10)
        KILL LABCORPDISPNAME
        QUIT
        ;
        ;"---------------------------------------------------------------
        ;"===============================================================
        ;"|  Below are the call-back functions to handle transformation |
        ;"|  hooks, called by the XFMSG^TMGHL7X engine                  |
        ;"===============================================================
        ;"---------------------------------------------------------------
        ;
LOCDATA2ARR(OBRARR,ARRTOADD,LASTNTE,LASTOBX)
        NEW LOCNAME SET LOCNAME=""
        NEW COUNT SET COUNT=0
        NEW LASTSEG SET LASTSEG=LASTNTE
        IF LASTSEG'>0 SET LASTSEG=LASTOBX
        FOR  SET LOCNAME=$O(OBRARR(LOCNAME)) QUIT:LOCNAME=""  DO
        . SET COUNT=COUNT+1
        . NEW TESTNAME,TESTS SET (TESTS,TESTNAME)=""
        . FOR  SET TESTNAME=$O(OBRARR(LOCNAME,TESTNAME)) QUIT:TESTNAME=""  DO
        . . IF TESTS'="" SET TESTS=TESTS_", "
        . . SET TESTS=TESTS_TESTNAME
        . ;"SET TESTS=""    ;"7/14/26 - Store these as blank for now.
        . SET ARRTOADD(LASTSEG,LOCNAME)=TESTS
        ;"8/12/26 - I was using LASTNTE above and below. I had defined LASTSEG but never used. I think that was what I intended here.
        SET ARRTOADD(LASTSEG,"COUNT")=COUNT
        QUIT
        ;"
SPEC2ARR(OBRARR,SPECARR,LASTNTE,LASTOBX)
        NEW COUNT SET COUNT=0
        NEW LASTSEG SET LASTSEG=LASTNTE
        IF LASTSEG'>0 SET LASTSEG=LASTOBX
        NEW SPECIDX SET SPECIDX=0
        ;"FOR  SET SPECIDX=$O(SPECARR(SPECIDX)) QUIT:SPECIDX'>0  DO
        
        
        QUIT
        ;"
ONEFLAG(FLAG)  ;"GET FLAG NAME
        IF FLAG="L" QUIT "Low"   ;"Below Low Normal"
        IF FLAG="H" QUIT "High"  ;"Above High Normal"
        IF FLAG="LL" QUIT "Alert Low"
        IF FLAG="HH" QUIT "Alert High"
        IF FLAG="<" QUIT "Panic Low"
        IF FLAG=">" QUIT "Panic High"
        IF FLAG="A" QUIT "Abnormal"
        IF FLAG="AA" QUIT "Critical Abnormal"
        IF FLAG="S" QUIT "Susceptible"
        IF FLAG="R" QUIT "Resistant"
        IF FLAG="I" QUIT "Intermediate"
        IF FLAG="NEG" QUIT "Negative"
        IF FLAG="POS" QUIT "Positive"
        QUIT ""
FLAGSTR(FLAGARR)  ;"Purpose: to build a list of all the flags contained within this lab set
        NEW STR SET STR=""
        NEW FLAG SET FLAG=""
        FOR  SET FLAG=$O(FLAGARR(FLAG)) QUIT:FLAG=""  DO
        . IF STR'="" SET STR=STR_", "
        . SET STR=STR_""""_FLAG_""""_" = "_$$ONEFLAG(FLAG)
        SET STR="Key: "_STR
        QUIT STR
        ;"
HNDLZPS ;"Purpose: Handle ZPS segments, storing testing locations in NTE segment
        ;"First load ZPS segment data into LOCARR
        NEW LOCARR,ZPSSEG
        NEW FLAGARR
        SET ZPSSEG=0
        FOR  SET ZPSSEG=$O(TMGHL7MSG("B","ZPS",ZPSSEG)) QUIT:ZPSSEG'>0  DO
        . ;"NEW SEG SET SEG=$G(TMGHL7MSG(ZPSSEG))
        . NEW LOCID SET LOCID=$G(TMGHL7MSG(ZPSSEG,2))
        . NEW LOCATION,SECONDLINE SET LOCATION=$G(TMGHL7MSG(ZPSSEG,3))_":",SECONDLINE=""
        . ;"NEW LOCATION SET LOCATION=$P(SEG,TMGU(1),4)_" "_$P(SEG,TMGU(1),5)
        . ;"SET LOCATION=$TR(LOCATION,"^"," ")
        . ;"NEW LOCSTR SET LOCSTR=$G(TMGHL7MSG(ZPSSEG,3))
        . NEW ADDRIDX SET ADDRIDX=0
        . NEW ZIP SET ZIP=""
        . FOR  SET ADDRIDX=$O(TMGHL7MSG(ZPSSEG,4,ADDRIDX)) QUIT:ADDRIDX'>0  DO
        . . NEW ITEM SET ITEM=$G(TMGHL7MSG(ZPSSEG,4,ADDRIDX))
        . . IF ITEM="" QUIT
        . . IF ADDRIDX=5 DO
        . . . SET ITEM=$$STR2ZIP^TMGSTUT3(ITEM)
        . . . IF $L(LOCATION)>60 DO
        . . . . SET ZIP=ITEM_". ",ITEM=""
        . . NEW DELIM SET DELIM=" "
        . . IF ADDRIDX=4 SET DELIM=", "
        . . IF ADDRIDX<3 DO
        . . . SET LOCATION=LOCATION_DELIM_ITEM
        . . ELSE  DO
        . . . SET SECONDLINE=SECONDLINE_DELIM_ITEM
        . NEW THIRDLINE SET THIRDLINE=""
        . SET SECONDLINE=SECONDLINE_" "_ZIP_$$STR2PHONE^TMGSTUT3($G(TMGHL7MSG(ZPSSEG,5)))
        . NEW MEDDIRECTOR SET MEDDIRECTOR=$G(TMGHL7MSG(ZPSSEG,7))
        . SET MEDDIRECTOR=$P(MEDDIRECTOR,"^",3)_" "_$P(MEDDIRECTOR,"^",4)_" "_$P(MEDDIRECTOR,"^",2)_", "_$P(MEDDIRECTOR,"^",1)
        . SET THIRDLINE=THIRDLINE_" Medical Director: "_MEDDIRECTOR
        . SET LOCARR(LOCID)=LOCID_" = "_LOCATION_"^"_SECONDLINE_"^"_THIRDLINE
        ;"
        ;"Now look at the SPM segments for sent data   8/21/26
        NEW SPMIDX,SPMARR
        SET SPMIDX=0
        FOR  SET SPMIDX=$O(TMGHL7MSG("B","SPM",SPMIDX)) QUIT:SPMIDX'>0  DO
        . NEW SEG SET SEG=$G(TMGHL7MSG(SPMIDX))
        . NEW SEGNUM SET SEGNUM=$P(SEG,TMGU(1),2)
        . NEW SEGSITE SET SEGSITE=$P(SEG,TMGU(1),9)
        . SET SEGSITE=$$TITLE^XLFSTR($P(SEGSITE,TMGU(2),2))
        . NEW SEGPLACE SET SEGPLACE=$P(SEG,TMGU(1),10)
        . SET SEGPLACE=$$TITLE^XLFSTR($P(SEGPLACE,TMGU(2),2))_", "_$$TITLE^XLFSTR($P(SEGPLACE,TMGU(2),4))
        . NEW SEGCOLL SET SEGCOLL=$P(SEG,TMGU(1),18)
        . IF SEGCOLL'="" SET SEGCOLL=$$HL72FMDT^TMGHL7U3(SEGCOLL)
        . IF SEGCOLL'="" SET SEGCOLL=$$EXTDATE^TMGDATE(SEGCOLL)
        . SET TMGHL7MSG("LABCORP","SPECIMEN",SEGNUM)=SEGSITE_"^"_SEGPLACE_"^"_SEGCOLL
        ;"
        ;"Now cycle through TMGHL7MSG, checking each segment. 
        ;"When we find an OBR, we will store 
        NEW IDX SET IDX=0
        NEW INSIDEOBR SET INSIDEOBR=0
        NEW INSIDENTE SET INSIDENTE=0
        NEW LASTNTE SET LASTNTE=0
        NEW LASTOBX SET LASTOBX=0
        NEW LASTORC SET LASTORC=0
        NEW ARRTOADD,OBRARR
        FOR  SET IDX=$O(TMGHL7MSG(IDX)) QUIT:IDX'>0  DO
        . NEW SEGMENT SET SEGMENT=$G(TMGHL7MSG(IDX,"SEG"))
        . ;" Handle new OBR
        . IF SEGMENT="OBR" DO
        . . SET INSIDEOBR=1
        . . SET LASTOBR=IDX
        . . IF $D(OBRARR) DO  ;"WE HAVE LOCATION DATA TO ADD TO NTE
        . . . DO LOCDATA2ARR(.OBRARR,.ARRTOADD,LASTNTE,LASTORC) 
        . . . SET ARRTOADD(LASTNTE,"FLAGS")=$$FLAGSTR(.FLAGARR)
        . . . SET LASTNTE=0,LASTOBX=0,LASTORC=0  ;"SAFETY MEASURE
        . . . KILL OBRARR,FLAGARR
        . ;"
        . ;" Handle new OBX
        . IF SEGMENT="OBX" DO
        . . SET LASTOBX=IDX
        . . NEW ONELOC SET ONELOC=$G(TMGHL7MSG(IDX,15))
        . . SET ONELOC=$G(LOCARR(ONELOC))
        . . SET OBRARR(ONELOC,$G(TMGHL7MSG(IDX,3,2)))=""
        . . SET FLAGARR($G(TMGHL7MSG(IDX,8)))=""
        . ;"
        . IF SEGMENT="ORC" SET LASTORC=IDX
        . ;" Handle new NTE
        . IF SEGMENT="NTE" DO
        . . SET LASTNTE=IDX       
        ;"
        IF $D(OBRARR) DO  ;"WE HAVE LOCATION DATA TO ADD TO NTE
        . DO LOCDATA2ARR(.OBRARR,.ARRTOADD,LASTNTE,LASTORC)
        ;"
        NEW NTEIDX SET NTEIDX=9999
        FOR  SET NTEIDX=$O(ARRTOADD(NTEIDX),-1) QUIT:NTEIDX'>0  DO
        . NEW TEMPARR,IDX SET IDX=0
        . NEW TEMPLOC SET TEMPLOC=""
        . NEW COUNT SET COUNT=$G(ARRTOADD(NTEIDX,"COUNT"))
        . SET TEMPARR($I(IDX))="===================================================="
        . IF COUNT>1 DO
        . . SET TEMPARR($I(IDX))="Performing Locations: "
        . ELSE  DO
        . . SET TEMPARR($I(IDX))="Performing Location: "
        . FOR  SET TEMPLOC=$O(ARRTOADD(NTEIDX,TEMPLOC)) QUIT:TEMPLOC=""  DO
        . . IF TEMPLOC="COUNT" QUIT
        . . IF TEMPLOC="FLAGS" QUIT
        . . IF COUNT>1 DO
        . . . ;"SET TEMPARR($I(IDX))=" "_TEMPLOC_":"
        . . . SET TEMPARR($I(IDX))=" "_$P(TEMPLOC,"^",1)
        . . . SET TEMPARR($I(IDX))="     "_$P(TEMPLOC,"^",2)
        . . . QUIT  ;" DON'T LIST TESTS  7/14/26
        . . . NEW TESTS SET TESTS=$G(ARRTOADD(NTEIDX,TEMPLOC))
        . . . IF $L(TESTS)>70 DO
        . . . . NEW TESTARR DO SPLIT70(TESTS,.TESTARR)
        . . . . NEW I SET I=0
        . . . . FOR  SET I=$O(TESTARR(I)) QUIT:I'>0  DO
        . . . . . SET TEMPARR($I(IDX))="     "_$G(TESTARR(I))
        . . . ELSE  DO
        . . . . SET TEMPARR($I(IDX))="    "_TESTS
        . . ELSE  DO
        . . . SET TEMPARR($I(IDX))=" "_$P(TEMPLOC,"^",1)
        . . . SET TEMPARR($I(IDX))="      "_$P(TEMPLOC,"^",2)
        . . . SET TEMPARR($I(IDX))="      "_$P(TEMPLOC,"^",3)
        . ;"IF $D(SPMARR) DO    ;"  added 8/21/26
        . ;". SET TEMPARR($I(IDX))="===================================================="
        . ;". NEW SPMIDX SET SPMIDX=0
        . ;". FOR  SET SPMIDX=$O(SPMARR(SPMIDX)) QUIT:SPMIDX'>0  DO
        . ;". . NEW ONESPEC SET ONESPEC=$G(SPMARR(SPMIDX))
        . ;". . SET TEMPARR($I(IDX))="Specimen "_SPMIDX_": "_$P(ONESPEC,"^",1)_", "_$P(ONESPEC,"^",2)
        . ;". . SET TEMPARR($I(IDX))="   Collection Date/Time: "_$P(ONESPEC,"^",3)
        . ;SET TEMPARR($I(IDX))="===================================================="
        . ;NEW FLAGSTR SET FLAGSTR=$G(ARRTOADD(NTEIDX,"FLAGS"))
        . ;IF $L(FLAGSTR)>70 DO 
        . ;. NEW FGARR DO SPLIT70(FLAGSTR,.FGARR)
        . ;. NEW FIDX SET FIDX=0
        . ;. FOR  SET FIDX=$O(FGARR(FIDX)) QUIT:FIDX'>0  DO
        . ;. . SET TEMPARR($I(IDX))=$G(FGARR(FIDX))
        . ;ELSE  DO
        . ;. SET TEMPARR($I(IDX))=FLAGSTR
        . DO INSRTNTE^TMGHL72(.TEMPARR,.TMGHL7MSG,.TMGU,NTEIDX)
        QUIT
        ;"
TROBX11(CODE)  ;"Purpose: Translate the code sent in OBX-11, Called from OBX11^TMGHL79 7/23/26
        NEW OUTCODE 
        SET CODE=$G(CODE),OUTCODE="?? - No code for: "_CODE
        IF CODE="" SET OUTCODE="No code provided."
        IF CODE="C" SET OUTCODE="Corrected"
        IF CODE="D" SET OUTCODE="Deleted"
        IF CODE="F" SET OUTCODE="Final"
        IF CODE="I" SET OUTCODE="Incomplete"
        IF CODE="P" SET OUTCODE="Preliminary"
        IF CODE="R" SET OUTCODE="Results not verified"
        IF CODE="S" SET OUTCODE="Partial results"
        IF CODE="U" SET OUTCODE="Results status change"
        IF CODE="W" SET OUTCODE="Wrong"
        IF CODE="X" SET OUTCODE="Test Not Performed"
        QUIT OUTCODE
        ;"
SPLIT70(STR,ARR) ;
        ; Input: STR = comma-delimited string
        ; Output: ARR(1..n) = lines <= 50 chars
        K ARR
        NEW IDX,PIECE,LINE
        SET IDX=1,LINE=""
        FOR  QUIT:STR=""  DO
        . SET PIECE=$P(STR,",",1)
        . SET STR=$P(STR,",",2,999)
        . ; Trim leading/trailing spaces
        . FOR  QUIT:$E(PIECE,1)'=" "  SET PIECE=$E(PIECE,2,$L(PIECE))
        . FOR  QUIT:$E(PIECE,$L(PIECE))'=" "  SET PIECE=$E(PIECE,1,$L(PIECE)-1)
        . IF LINE="" DO  QUIT
        . . SET LINE=PIECE
        . IF $L(LINE)+2+$L(PIECE)'>70 DO  QUIT
        . . SET LINE=LINE_", "_PIECE
        . SET ARR(IDX)=LINE
        . SET IDX=IDX+1
        . SET LINE=PIECE
        IF LINE'="" SET ARR(IDX)=LINE
        QUIT        
        ;"
TRSTATUS(STATUS)  ;" CONVERT TO PROPER STATUS
        IF STATUS="F" QUIT "FINAL"
        IF STATUS="P" QUIT "PRELIMINARY"
        QUIT ""
        ;"
TRFAST(FASTING)  ;" CONVERT TO PROPER FASTING
        IF FASTING="Y" QUIT "YES"
        IF FASTING="N" QUIT "NO"
        QUIT ""
HNDLPID ;"Purpose: This will get PID 18.6 and PID 18.7 (Status of Specimen and Fasting) then will either add a new NTE or append to the current PID
        NEW TEMPARR
        NEW PIDIDX SET PIDIDX=$O(TMGHL7MSG("B","PID",0))
        NEW STATUS,FASTING
        SET STATUS=$$TRSTATUS($G(TMGHL7MSG(PIDIDX,18,6)))
        SET FASTING=$$TRFAST($G(TMGHL7MSG(PIDIDX,18,7)))
        SET TEMPARR(1)="SPECIMEN STATUS: "_STATUS_".  FASTING: "_FASTING
        ;"IF $D(TMGHL7MSG("B","NTE",PIDIDX+1)) DO
        DO INSRTNTE^TMGHL72(.TEMPARR,.TMGHL7MSG,.TMGU,PIDIDX)
        QUIT
        ;"
HNDLOBR ;"Purpose: Check all OBRs elements to see if they are children of other OBR
        ;"//kt //codex 2026-10-02 ;"To begin with, we have to create a listing of all OBR tests.
        ;"//kt //codex 2026-10-02 ;"   This cannot be done top down, because some reflex tests may themselve reflex and overwrite the parent
        ;"//kt //codex 2026-10-02 NEW PARENTARR,CHILDARR
        ;"//kt //codex 2026-10-02 NEW ONESEGN SET ONESEGN=0
        ;"//kt //codex 2026-10-02 FOR  SET ONESEGN=$O(TMGHL7MSG("B","OBR",ONESEGN)) QUIT:ONESEGN'>0  DO
        ;"//kt //codex 2026-10-02 . NEW TESTNUM SET TESTNUM=$P($G(TMGHL7MSG(ONESEGN,4)),TMGU(2),1)
        ;"//kt //codex 2026-10-02 . SET PARENTARR(TESTNUM)=$G(TMGHL7MSG(ONESEGN,4))
        ;"//kt //codex 2026-10-02 . NEW CODE SET CODE=$G(TMGHL7MSG(ONESEGN,11))
        ;"//kt //codex 2026-10-02 . IF CODE="G" SET CHILDARR(ONESEGN)=$G(TMGHL7MSG(ONESEGN,29))
        ;"//kt //codex 2026-10-02 ;"
        ;"//kt //codex 2026-10-02 ;"Now go through each child node, replacing the test with the parent
        ;"//kt //codex 2026-10-02 SET ONESEGN=0
        ;"//kt //codex 2026-10-02 FOR  SET ONESEGN=$O(CHILDARR(ONESEGN)) QUIT:ONESEGN'>0  DO
        ;"//kt //codex 2026-10-02 . NEW PARENT SET PARENT=$G(CHILDARR(ONESEGN))
        ;"//kt //codex 2026-10-02 . IF PARENT'="" SET PARENT=$G(PARENTARR(PARENT))
        ;"//kt //codex 2026-10-02 . IF PARENT'="" SET $P(TMGHL7MSG(ONESEGN),TMGU(1),5)=PARENT   ;"  USE  SETPCE^TMGHL7X2
        ;"//kt //codex 2026-10-02 QUIT
        ;"//kt //codex 2026-10-02 ;"
        ;"//kt //codex prior active HNDLOBR block 2026-10-17 --
        ;"NEW CHILDOBR SET CHILDOBR=0 
        ;"FOR  SET CHILDOBR=$ORDER(TMGHL7MSG("B","OBR",CHILDOBR)) QUIT:CHILDOBR'>0  DO  
        ;". NEW ACTIONCODE SET ACTIONCODE=$GET(TMGHL7MSG(CHILDOBR,11)) QUIT:ACTIONCODE'["G"  
        ;". NEW PARENTOBRID SET PARENTOBRID=$PIECE($GET(TMGHL7MSG(CHILDOBR,29)),TMGU(2),1) QUIT:PARENTOBRID=""  
        ;". NEW PARENTOBR,SEARCH SET PARENTOBR=0,SEARCH=CHILDOBR  
        ;". FOR  SET SEARCH=$ORDER(TMGHL7MSG(SEARCH),-1) QUIT:(SEARCH'>0)!(PARENTOBR>0)  DO  
        ;". . IF $GET(TMGHL7MSG(SEARCH,"SEG"))'="OBR" QUIT  
        ;". . IF $GET(TMGHL7MSG(SEARCH,4,1))=PARENTOBRID SET PARENTOBR=SEARCH  
        ;". QUIT:PARENTOBR'>0  
        ;". NEW PARENTOBXID SET PARENTOBXID=$PIECE($GET(TMGHL7MSG(CHILDOBR,26)),TMGU(2),1) QUIT:PARENTOBXID=""  
        ;". NEW PARENTOBX SET PARENTOBX=0,SEARCH=CHILDOBR  
        ;". FOR  SET SEARCH=$ORDER(TMGHL7MSG(SEARCH),-1) QUIT:(SEARCH'>0)!(SEARCH<PARENTOBR)!(PARENTOBX>0)  DO  
        ;". . IF $GET(TMGHL7MSG(SEARCH,"SEG"))'="OBX" QUIT  
        ;". . IF $GET(TMGHL7MSG(SEARCH,3,1))=PARENTOBXID SET PARENTOBX=SEARCH  
        ;". QUIT:PARENTOBX'>0  
        ;". NEW CURSEGN,STOP,MOVED SET CURSEGN=CHILDOBR,STOP=0,MOVED=0  
        ;". FOR  SET CURSEGN=$ORDER(TMGHL7MSG(CURSEGN)) QUIT:(CURSEGN'>0)!STOP  DO  
        ;". . NEW SEGTYPE SET SEGTYPE=$GET(TMGHL7MSG(CURSEGN,"SEG"))  
        ;". . IF (SEGTYPE="ORC")!(SEGTYPE="OBR") SET STOP=1 QUIT  
        ;". . IF SEGTYPE'="OBX" QUIT  
        ;". . NEW TESTNAME SET TESTNAME=$GET(TMGHL7MSG(CURSEGN,3,2))  
        ;". . IF $EXTRACT(TESTNAME,1,7)'="Result " QUIT  
        ;". . NEW RESULTNUM SET RESULTNUM=$EXTRACT(TESTNAME,8,$LENGTH(TESTNAME))  
        ;". . IF RESULTNUM'?1N.N QUIT  
        ;". . NEW NTEARR,NTESEGS,NTESEGN,NOTEDONE,NTECT SET NTESEGN=CURSEGN,NOTEDONE=0,NTECT=0  
        ;". . FOR  SET NTESEGN=$ORDER(TMGHL7MSG(NTESEGN)) QUIT:(NTESEGN'>0)!NOTEDONE  DO  
        ;". . . IF $GET(TMGHL7MSG(NTESEGN,"SEG"))'="NTE" SET NOTEDONE=1 QUIT  
        ;". . . SET NTECT=NTECT+1,NTEARR(NTECT)=$GET(TMGHL7MSG(NTESEGN,3)),NTESEGS(NTESEGN)=""  
        ;". . IF NTECT'>0 QUIT  
        ;". . NEW TMGRESULT SET TMGRESULT=$$INSRTNTE^TMGHL72(.NTEARR,.TMGHL7MSG,.TMGU,PARENTOBX) QUIT:+TMGRESULT'>0  
        ;". . NEW KILLSEGN SET KILLSEGN=0  
        ;". . FOR  SET KILLSEGN=$ORDER(NTESEGS(KILLSEGN)) QUIT:KILLSEGN'>0  DO KILLSEG79(KILLSEGN)  
        ;". . DO KILLSEG79(CURSEGN)  
        ;". . SET MOVED=1  
        ;". IF MOVED DO  
        ;". . NEW REMAINING,SCAN,SCANSTOP SET REMAINING=0,SCAN=CHILDOBR,SCANSTOP=0  
        ;". . FOR  SET SCAN=$ORDER(TMGHL7MSG(SCAN)) QUIT:(SCAN'>0)!SCANSTOP  DO  
        ;". . . NEW SEGTYPE SET SEGTYPE=$GET(TMGHL7MSG(SCAN,"SEG"))  
        ;". . . IF (SEGTYPE="ORC")!(SEGTYPE="OBR") SET SCANSTOP=1 QUIT  
        ;". . . IF SEGTYPE="OBX" SET REMAINING=1  
        ;". . IF REMAINING=0 SET TMGHL7MSG("IGNORE","OBR",CHILDOBR)=1  
        ;"QUIT  
        ;"//kt //codex begin mod 2026-10-17 --
        ;"EXAMPLE HL7 causing problem to be solve. Result is spread over severy OBR's
        ;"     MSH|^~\&|1100|BN|FAMPHYGV|BN018719|202609141636||ORU|0077|P|2.3
        ;"     
        ;"     ORC|RE|20704918866^LAB|25264673560^LAB||||||202609090000|||1093726085^TOPPENBER^K^^^^^N
        ;"     OBR|1|20704918866^LAB|25264673560^LAB|008144^Stool Culture^L|||202609091033||||||SRC:ST                     SRC:ST|202609090000|ST     CD- 20704918866|||20704918866||20704918866||202609141636|||F
        ;"     OBX|1|ST|008722^Salmonella/Shigella Screen^L||Final report|||||N|F|20031013||202609121301|01|||||||||||||MIC
        ;"     
        ;"     ORC|RE|20704918866^LAB|25264673560^LAB||||||202609090000|||1093726085^TOPPENBER^K^^^^^N
        ;"     OBR|2|20704918866^LAB|25264673560^LAB|997871^Result^L|||202609091033||||G|||202609090000||||20704918866||20704918866||202609141636|||F|008722|||008144
        ;"     OBX|1|TX|997141^Result 1^L||Comment|||||N|F|||202609121301|01|||||||||||||MIC
        ;"     NTE|1|L|No Salmonella or Shigella recovered.
        ;"     
        ;"     ORC|RE|20704918866^LAB|25264673560^LAB||||||202609090000|||1093726085^TOPPENBER^K^^^^^N
        ;"     OBR|3|20704918866^LAB|25264673560^LAB|008144^Stool Culture^L|||202609091033|||||||202609090000|ST     CD- 20704918866|||20704918866||20704918866||202609141636|||F
        ;"     OBX|1|ST|180141^Campylobacter Culture^L||Final report|||||N|F|20031013||202609141517|01|||||||||||||MIC
        ;"     
        ;"     ORC|RE|20704918866^LAB|25264673560^LAB||||||202609090000|||1093726085^TOPPENBER^K^^^^^N
        ;"     OBR|4|20704918866^LAB|25264673560^LAB|080172^Result^L|||202609091033||||G|||202609090000||||20704918866||20704918866||202609141636|||F|180141|||008144
        ;"     OBX|1|TX|080173^Result 1^L||Comment|||||N|F|||202609141517|01|||||||||||||MIC
        ;"     NTE|1|L|No Campylobacter species isolated.
        ;"     
        ;"     ORC|RE|20704918866^LAB|25264673560^LAB||||||202609090000|||1093726085^TOPPENBER^K^^^^^N
        ;"     OBR|5|20704918866^LAB|25264673560^LAB|008144^Stool Culture^L|||202609091033|||||||202609090000|ST     CD- 20704918866|||20704918866||20704918866||202609141636|||F
        ;"     OBX|1|ST|180935^E coli Shiga Toxin EIA^L||Negative||Negative|||N|F|||202609120436|01|||||||||||||MIC
        ;"     
        ;"     ORC|RE|20704918866^LAB|25264673560^LAB||||||202609090000|||1093726085^TOPPENBER^K^^^^^N
        ;"     OBR|6|20704918866^LAB|25264673560^LAB|PDFReport1^PDF Report^L|||202609091033|||||||202609090000||||20704918866||20704918866||202609141636|||F
        ;"     OBX|1|ED|PDFReport1^PDF Report1^L|000000|LCLS^Image^PDF^Base64^EMBEDDED DOCUMENT||||||F||||01
        ;" 
        ;"Goal is to change above into this:
        ;"     MSH|^~\&|1100|BN|FAMPHYGV|BN018719|202609141636||ORU|0077|P|2.3
        ;"     
        ;"     ORC|RE|20704918866^LAB|25264673560^LAB||||||202609090000|||1093726085^TOPPENBER^K^^^^^N
        ;"     OBR|1|20704918866^LAB|25264673560^LAB|008144^Stool Culture^L|||202609091033||||||SRC:ST                     SRC:ST|202609090000|ST     CD- 20704918866|||20704918866||20704918866||202609141636|||F
        ;"     OBX|1|ST|008722^Salmonella/Shigella Screen^L||Final report|||||N|F|20031013||202609121301|01|||||||||||||MIC
        ;"     NTE|1|L|No Salmonella or Shigella recovered.
        ;"     OBX|2|ST|180141^Campylobacter Culture^L||Final report|||||N|F|20031013||202609141517|01|||||||||||||MIC
        ;"     NTE|1|L|No Campylobacter species isolated.
        ;"     
        ;"     ORC|RE|20704918866^LAB|25264673560^LAB||||||202609090000|||1093726085^TOPPENBER^K^^^^^N
        ;"     OBR|2|20704918866^LAB|25264673560^LAB|008144^Stool Culture^L|||202609091033|||||||202609090000|ST     CD- 20704918866|||20704918866||20704918866||202609141636|||F
        ;"     OBX|1|ST|180935^E coli Shiga Toxin EIA^L||Negative||Negative|||N|F|||202609120436|01|||||||||||||MIC
        ;"     
        ;"What changes do: 
        ;" - locate generated child OBRs via OBR-26/29;
        ;" - fold substantive Result N values into parent NTEs (Organism: Klebsiella pneumoniae);
        ;" - transfer child-group OBX/NTE content into the parent package;
        ;" - remove absorbed OBR/ORC boundaries;
        ;" - merge only root OBRs that share a generated-child culture structure, leaving an unrelated OBR such as the Shiga-toxin group separate.
        NEW CHILDOBR,CULTROOT SET CHILDOBR=0
        FOR  SET CHILDOBR=$ORDER(TMGHL7MSG("B","OBR",CHILDOBR)) QUIT:CHILDOBR'>0  DO
        . NEW ACTIONCODE SET ACTIONCODE=$GET(TMGHL7MSG(CHILDOBR,11)) QUIT:ACTIONCODE'["G"
        . NEW PARENTOBR,PARENTOBRID,PARENTOBX,PARENTOBXID
        . SET PARENTOBRID=$PIECE($GET(TMGHL7MSG(CHILDOBR,29)),TMGU(2),1) QUIT:PARENTOBRID=""
        . SET PARENTOBR=$$PARENTOBR79(CHILDOBR,PARENTOBRID) QUIT:PARENTOBR'>0
        . SET PARENTOBXID=$PIECE($GET(TMGHL7MSG(CHILDOBR,26)),TMGU(2),1) QUIT:PARENTOBXID=""
        . SET PARENTOBX=$$PARENTOBX79(CHILDOBR,PARENTOBR,PARENTOBXID) QUIT:PARENTOBX'>0
        . SET CULTROOT(PARENTOBR)=$$CULTKEY79(PARENTOBR)
        . DO FOLDRES79(CHILDOBR,PARENTOBR,PARENTOBX)
        . DO KILLPREORC79(CHILDOBR)
        . DO MOVEBLOCK79(CHILDOBR,PARENTOBR)
        . DO KILLSEG79(CHILDOBR)
        NEW ROOT,KEY,CANON SET ROOT=0
        FOR  SET ROOT=$ORDER(CULTROOT(ROOT)) QUIT:ROOT'>0  DO
        . SET KEY=$GET(CULTROOT(ROOT)) QUIT:KEY=""
        . IF $GET(CANON(KEY))'>0 SET CANON(KEY)=ROOT QUIT
        . DO KILLPREORC79(ROOT)
        . DO MOVEBLOCK79(ROOT,CANON(KEY))
        . DO KILLSEG79(ROOT)
        ;"//kt //codex end mod 2026-10-17 --
        QUIT
        ;
PARENTOBR79(CHILDOBR,PARENTOBRID) ;"Return nearest preceding OBR with the requested test ID.
        ;"//kt //codex added entire function 2026-10-17
        NEW PARENTOBR,SEARCH SET PARENTOBR=0,SEARCH=CHILDOBR
        FOR  SET SEARCH=$ORDER(TMGHL7MSG(SEARCH),-1) QUIT:(SEARCH'>0)!(PARENTOBR>0)  DO
        . IF $GET(TMGHL7MSG(SEARCH,"SEG"))'="OBR" QUIT
        . IF $GET(TMGHL7MSG(SEARCH,4,1))=PARENTOBRID SET PARENTOBR=SEARCH
        QUIT PARENTOBR
        ;
PARENTOBX79(CHILDOBR,PARENTOBR,PARENTOBXID) ;"Return the parent OBX identified by OBR-26.
        ;"//kt //codex added entire function 2026-10-17
        NEW PARENTOBX,SEARCH SET PARENTOBX=0,SEARCH=CHILDOBR
        FOR  SET SEARCH=$ORDER(TMGHL7MSG(SEARCH),-1) QUIT:(SEARCH'>0)!(SEARCH<PARENTOBR)!(PARENTOBX>0)  DO
        . IF $GET(TMGHL7MSG(SEARCH,"SEG"))'="OBX" QUIT
        . IF $GET(TMGHL7MSG(SEARCH,3,1))=PARENTOBXID SET PARENTOBX=SEARCH
        QUIT PARENTOBX
        ;
FOLDRES79(CHILDOBR,PARENTOBR,PARENTOBX) ;"Move generated Result N content into its linked parent OBX notes.
        ;"//kt //codex added entire function 2026-10-17
        NEW CURSEGN,STOP SET CURSEGN=CHILDOBR,STOP=0
        FOR  SET CURSEGN=$ORDER(TMGHL7MSG(CURSEGN)) QUIT:(CURSEGN'>0)!STOP  DO
        . NEW SEGTYPE SET SEGTYPE=$GET(TMGHL7MSG(CURSEGN,"SEG"))
        . IF (SEGTYPE="ORC")!(SEGTYPE="OBR") SET STOP=1 QUIT
        . IF SEGTYPE'="OBX" QUIT
        . NEW TESTNAME SET TESTNAME=$GET(TMGHL7MSG(CURSEGN,3,2))
        . IF $EXTRACT(TESTNAME,1,7)'="Result " QUIT
        . NEW RESULTNUM SET RESULTNUM=$EXTRACT(TESTNAME,8,$LENGTH(TESTNAME)) QUIT:RESULTNUM'?1N.N
        . NEW NTEARR,NTESEGS,NTESEGN,NOTEDONE,NTECT SET NTECT=0,NTESEGN=CURSEGN,NOTEDONE=0
        . NEW VALUE SET VALUE=$GET(TMGHL7MSG(CURSEGN,5))
        . IF (VALUE'="")&(VALUE'="Comment")&(VALUE'="Final report") DO
        . . NEW LABEL SET LABEL="Result: "
        . . IF $GET(TMGHL7MSG(PARENTOBR,4,2))["Culture" SET LABEL="Organism: "
        . . SET NTECT=NTECT+1,NTEARR(NTECT)=LABEL_VALUE
        . FOR  SET NTESEGN=$ORDER(TMGHL7MSG(NTESEGN)) QUIT:(NTESEGN'>0)!NOTEDONE  DO
        . . IF $GET(TMGHL7MSG(NTESEGN,"SEG"))'="NTE" SET NOTEDONE=1 QUIT
        . . SET NTECT=NTECT+1,NTEARR(NTECT)=$GET(TMGHL7MSG(NTESEGN,3)),NTESEGS(NTESEGN)=""
        . IF NTECT>0 NEW TMGRESULT SET TMGRESULT=$$INSRTNTE^TMGHL72(.NTEARR,.TMGHL7MSG,.TMGU,PARENTOBX)
        . NEW KILLSEGN SET KILLSEGN=0
        . FOR  SET KILLSEGN=$ORDER(NTESEGS(KILLSEGN)) QUIT:KILLSEGN'>0  DO KILLSEG79(KILLSEGN)
        . DO KILLSEG79(CURSEGN)
        QUIT
        ;
MOVEBLOCK79(FROMOBR,TOOBR) ;"Move all OBX/NTE segments from one OBR group to another.
        ;"//kt //codex added entire function 2026-10-17
        NEW MOVEARR,KILLARR,SEGN,STOP,COUNT SET SEGN=FROMOBR,STOP=0,COUNT=0
        FOR  SET SEGN=$ORDER(TMGHL7MSG(SEGN)) QUIT:(SEGN'>0)!STOP  DO
        . NEW SEGTYPE SET SEGTYPE=$GET(TMGHL7MSG(SEGN,"SEG"))
        . IF SEGTYPE="OBR" SET STOP=1 QUIT
        . IF (SEGTYPE'="OBX")&(SEGTYPE'="NTE") QUIT
        . SET COUNT=COUNT+1,MOVEARR(COUNT)=$GET(TMGHL7MSG(SEGN)),KILLARR(SEGN)=""
        QUIT:COUNT'>0
        NEW LASTSEG SET LASTSEG=$$LASTSEG79(TOOBR)
        NEW TMGRESULT SET TMGRESULT=$$INSRTSEG^TMGHL72(.MOVEARR,.TMGHL7MSG,.TMGU,LASTSEG)
        QUIT:+TMGRESULT'>0
        SET SEGN=0
        FOR  SET SEGN=$ORDER(KILLARR(SEGN)) QUIT:SEGN'>0  DO KILLSEG79(SEGN)
        QUIT
        ;
LASTSEG79(OBRSEGN) ;"Return the final segment in one OBR group, before the next OBR.
        ;"//kt //codex added entire function 2026-10-17
        NEW LASTSEG,SEGN,STOP SET LASTSEG=OBRSEGN,SEGN=OBRSEGN,STOP=0
        FOR  SET SEGN=$ORDER(TMGHL7MSG(SEGN)) QUIT:(SEGN'>0)!STOP  DO
        . IF $GET(TMGHL7MSG(SEGN,"SEG"))="OBR" SET STOP=1 QUIT
        . SET LASTSEG=SEGN
        QUIT LASTSEG
        ;
KILLPREORC79(OBRSEGN) ;"Remove the ORC immediately preceding an absorbed OBR group.
        ;"//kt //codex added entire function 2026-10-17
        NEW PRIOR SET PRIOR=$ORDER(TMGHL7MSG(OBRSEGN),-1)
        IF $GET(TMGHL7MSG(PRIOR,"SEG"))="ORC" DO KILLSEG79(PRIOR)
        QUIT
        ;
CULTKEY79(OBRSEGN) ;"Return a key for LabCorp culture roots eligible to share a filing package.
        ;"//kt //codex added entire function 2026-10-17
        NEW KEY,SEP SET SEP=$CHAR(30)
        SET KEY=$GET(TMGHL7MSG(OBRSEGN,2))_SEP_$GET(TMGHL7MSG(OBRSEGN,3))_SEP_$GET(TMGHL7MSG(OBRSEGN,4,1))_SEP_$GET(TMGHL7MSG(OBRSEGN,7))
        QUIT KEY
        ;
KILLSEG79(SEGN) ;"Purpose: Delete one segment and all indexes that refer to it.  ;"//kt //codex 2026-10-02
        NEW SEGTYPE SET SEGTYPE=$GET(TMGHL7MSG(SEGN,"SEG"))  
        IF SEGTYPE'="" KILL TMGHL7MSG("B",SEGTYPE,SEGN)  
        KILL TMGHL7MSG("ORDER",SEGN),TMGHL7MSG("RESULT",SEGN)  
        NEW POINDEX SET POINDEX=""  
        FOR  SET POINDEX=$ORDER(TMGHL7MSG("PO",POINDEX)) QUIT:POINDEX=""  DO  
        . IF $GET(TMGHL7MSG("PO",POINDEX))=SEGN KILL TMGHL7MSG("PO",POINDEX)  
        KILL TMGHL7MSG(SEGN)  
        QUIT  
        ;
REMVPDF ;"Purpose: This routine will remove and PDFs contains in OBXs of TMGHL7MSG (globally scoped)
        NEW IDX SET IDX=0
        FOR  SET IDX=$O(TMGHL7MSG("B","OBX",IDX)) QUIT:IDX'>0  DO
        . NEW LINE SET LINE=$G(TMGHL7MSG(IDX))
        . IF $P(LINE,TMGU(1),4)["PDFReport1" DO
        . . KILL TMGHL7MSG(IDX)
        . . KILL TMGHL7MSG("B","OBX",IDX)
        . . KILL TMGHL7MSG("PO",IDX)
        QUIT
        ;"
MSG    ;"Purpose: Process entire message before processing segments
        ;"  EDDIE - Here I can scan the entire message to create whatever data structures I want
        ;' TMGHL7MSG - USE INSRTNTE^TMGHL72
        ;"IF TMGSTAGE="FINAL" DO HNDLPID
        IF TMGSTAGE="PRE" DO 
        . DO HNDLOBR
        . DO REMVPDF
        IF TMGSTAGE="FINAL" DO HNDLZPS        
        DO MSG^TMGHL74
        QUIT
        ;
MSG2    ;"Purpose: Process entire message after processing segments
        DO MSG2^TMGHL74 
        IF TMGSTAGE="FINAL" DO
        . KILL TMGALTPID
        . KILL TMGORCNPI
        KILL TMGIGNOREOBR
        QUIT
        ;
MSH3    ;"Purpose: Process MSH segment, FLD 4 (Sending Application)
        QUIT
        ;
MSH4  ;"Purpose: Process MSH segment, FLD 4 (Sending Facility)
        IF $GET(TMGHL7MSG("STAGE"))="PRE" DO  QUIT
        . SET TMGVALUE="LABCORP"
        . DO XMSH4^TMGHL72
        QUIT
        ;
MSH15  ;"Purpose: Process MSH segment, FLD 15
        DO XMSH15^TMGHL72
        QUIT
        ;
MSH16  ;"Purpose: Process MSH segment, FLD 16
        DO XMSH16^TMGHL72 
        QUIT
        ;
PID     ;"Purpose: To transform the PID segment, esp SSN
        IF TMGSTAGE="PRE" DO
        . NEW PIDSEG SET PIDSEG=$O(TMGHL7MSG("B","PID",0))
        . ;"SET TMGALTPID=$G(TMGHL7MSG(PIDSEG,2))_";  Alt Lab Patient ID: "_$G(TMGHL7MSG(PIDSEG,4))
        . SET TMGALTPID=$G(TMGHL7MSG(PIDSEG,4))
        DO PID^TMGHL72
        QUIT
        ;
PV18    ;"Purpose: Process entire PV1-8 segment
        DO PV18^TMGHL74
        QUIT
        ;
ORC1   ;"Purpose: Process empty ORC message, field 1
        DO XORC1^TMGHL72
        QUIT
        ;
ORC2   ;"Purpose: Process empty ORC message, field 1
        DO ORC2^TMGHL72
        QUIT
        ;
ORC12  ;"Purpose: Process empty ORC message, field 12
        ;"Note for block below:   //kt 7/24/26
        ;"  Fileman couldn't find unique match for TOPPENBERG,M because both Matthew and Marcia Toppenberg are in system
        IF TMGVALUE["^TOPPENBER^M" DO   
        . SET TMGVALUE="^TOPPENBERG^MARCIA DEE"
        . ;"Ensure TMGHL7MSG changed and refreshed before calling XORC12^TMGHL72, 
        . ;"   which sets up TMGINFO("PROV") preferentially from first OBR.16
        . DO SETPCE^TMGHL7X2(TMGVALUE,.TMGHL7MSG,.TMGU,"OBR",16)   
        . DO SETPCE^TMGHL7X2(TMGVALUE,.TMGHL7MSG,.TMGU,TMGSEGN,TMGFLDN)  ;"Ensure TMGHL7MSG changed before calling XORC12^TMGHL72 
        IF TMGSTAGE="PRE" DO
        . SET TMGORCNPI=$P(TMGVALUE,"^",1)
        DO XORC12^TMGHL72
        QUIT
        ;
ORC13  ;"Purpose: Process empty ORC message, field 13
        DO ORC13^TMGHL74
        QUIT
        ;
OBRPARNM(PARENTTESTID) ;"Purpose: If child test, find the parent test name
        ;"PARENTTESTID SHOULD BE THE ID OF THE PARENT IN ANOTHER OBR
        ;"Note, uses TMGHL7MSG, which should be globally scoped
        NEW PARENTOUT SET PARENTOUT=""
        NEW SEGNUM SET SEGNUM=0
        FOR  SET SEGNUM=$O(TMGHL7MSG("B","OBR",SEGNUM)) QUIT:SEGNUM'>0  DO
        . IF PARENTTESTID=$G(TMGHL7MSG(4,"ORDER","PREMAP","TESTID")) DO
        . . SET PARENTOUT=$G(TMGHL7MSG(4,"ORDER","PREMAP","TEST"))
        QUIT PARENTOUT
        ;"
OBR     ;"Purppse: setup for OBR fields.
        ;"Uses TMGHL7MSG,TMGSEGN,TMGU in global scope
        IF $GET(TMGHL7MSG("STAGE"))="PRE" DO  QUIT
        . NEW TEMP SET TEMP=$$HNDUPOBX^TMGHL72(.TMGHL7MSG,TMGSEGN,.TMGU)
        . IF TEMP<0 SET TMGXERR=$PIECE(TEMP,"^",2,99) 
        ;"
        NEW OBRCOMMENT SET OBRCOMMENT(1)=$G(TMGHL7MSG(TMGSEGN,13))
        IF $G(OBRCOMMENT(1))'="" DO
        . DO INSRTNTE^TMGHL72(.OBRCOMMENT,.TMGHL7MSG,.TMGU,TMGSEGN)
        DO OBR^TMGHL72        
        QUIT
        ;
OBR4    ;"Purpose: To transform the OBR segment, field 4
        SET TMGLASTOBR4=TMGVALUE  ;"this will be later killed in MSG2^TMGHL76
        SET TMGLASTOBX3=""        ;"Rest since going into different order (OBR)
        SET TMGOBXCOUNT=0
        IF $GET(TMGHL7MSG("STAGE"))="PRE" QUIT
        ;"
        DO OBR4^TMGHL72
        QUIT
        ;
OBR15   ;"Transform Secimen source
        ;"SET TMGINFO("ORL")="LABC"
        DO SUORL^TMGHL72
        DO OBR15^TMGHL73
        QUIT
        ;
OBR16   ;"Transform Ordering provider.
        IF TMGVALUE["^PROVIDER^HISTORICAL^" DO
        . SET TMGHL7MSG("IGNORE","OBR",TMGSEGN)=1
        DO OBR16^TMGHL72
        QUIT
        ;
OBRDN   ;"Purpose: setup for OBR fields, called *after* fields, subfields etc are processed
        ;"This allows putting information about the ordered test(s) into the comment section
        ;"Uses globally scoped vars: TMGSEGN, TMGDD
        NEW LABCORPFASTING  ;"WILL BE USED IN GLOBAL SCOPE BY OBRDN^TMGHL74 
        NEW PIDIDX SET PIDIDX=$O(TMGHL7MSG("B","PID",0))
        ;"NEW STATUS,FASTING
        ;"SET STATUS=$$TRSTATUS($G(TMGHL7MSG(PIDIDX,18,6)))
        SET LABCORPFASTING=$$TRFAST($G(TMGHL7MSG(PIDIDX,18,7)))
        NEW LABCORPACCTN SET LABCORPACCTN=$G(TMGHL7MSG(PIDIDX,18,1))
        NEW USEPREMAP SET USEPREMAP=1
        ;" LAB STATUS
        NEW PIDIDX SET PIDIDX=$O(TMGHL7MSG("B","PID",0))
        SET STATUS=$$TRSTATUS($G(TMGHL7MSG(PIDIDX,18,6)))
        NEW LABCORPSTATUS SET LABCORPSTATUS=STATUS
        NEW LABCORPVOLUME SET LABCORPVOLUME=$P($G(TMGHL7MSG(TMGSEGN)),TMGU(1),10)
        ;"
        DO OBRDN^TMGHL74
        QUIT
        ;
OBX     ;"Purpose: To transform the entire OBX segment -- Observation Identifier
        ;"Input: Uses globally scoped vars: TMGHL7MSG, TMGU, TMGVALUE, 
        ;"       TMGSEGN, TMGINFO, TMGENV
        IF TMGVALUE["PDF" DO
        . NEW TEST
        DO OBX^TMGHL72
        QUIT
        ;
OBX3    ;"Purpose: To transform the OBX segment, field 3 -- Observation Identifier
        ;"Input: Uses globally scoped vars: TMGHL7MSG, TMGU, TMGVALUE, IEN62D4,
        ;"       TMGSEGN, TMGINFO, TMGENV
        ;"Example TMGVALUE -- 'CRE^CREATININE'
        ;"Test for special result name
        NEW TESTNAME SET TESTNAME=$P(TMGVALUE,TMGU(2),2)
        ;"IF (TESTNAME=".")!(TESTNAME="RESULT 1") DO
        IF (TMGSTAGE="FINAL")&(TESTNAME=".") SET $P(TMGVALUE,TMGU(2),2)="Comment"
        ;"//kt //codex begin mod 2026-10-17 --
        ;"The Result 1 branch changed only local TMGVALUE.  OBX3^TMGHL72 maps
        ;"the parsed field, so this did not affect storage selection.  Generated
        ;"Result N content is now folded into its parent by HNDLOBR.
        ;"IF TESTNAME="RESULT 1" DO
        ;". NEW TEMPNAME SET TEMPNAME=$P(TMGVALUE,TMGU(2),5)
        ;". IF TEMPNAME'=-"" SET $P(TMGVALUE,TMGU(2),2)=TEMPNAME
        ;"//kt //codex end mod 2026-10-17 --
        SET TMGOBXCOUNT=$GET(TMGOBXCOUNT)+1
        DO OBX3^TMGHL72
        SET TMGLASTOBX3=TMGVALUE
        QUIT
        ;
OBX5    ;"Purpose: To transform the OBX segment, field 5 -- Observation value
        IF TMGVALUE="" DO       ;"7/13/26
        . IF $D(TMGHL7MSG("B","NTE",TMGSEGN+1)) DO  QUIT
        . . SET TMGVALUE="(See note below)"
        . SET TMGVALUE=" "
        DO OBX5^TMGHL72        
        QUIT
        ;
OBX11   ;"Purpose: To transform the OBX segment, field 11 -- Observ Result Status
        IF TMGSTAGE="FINAL" SET TMGVALUE=$$TROBX11(TMGVALUE)
        QUIT
        ;"
OBX15   ;"Purpose: To transform the OBX segment, field 15 ---- Producer's ID
        IF TMGSTAGE="FINAL" DO       ;"DO OBX15^TMGHL72
        . NEW LOC SET LOC=$G(TMGINFO("ORL"))
        . SET TMGVALUE=TMGVALUE_"^"_$P(LOC,"^",2)_"^"_$P(LOC,"^",3)_"^"_$P(LOC,"^",4) 
        QUIT
        ;
OBX16   ;"Purpose: To transform the OBX segment, field 16 ---- Responsibile Observer
        DO OBX16^TMGHL72
        QUIT
        ;
OBX18   ;"Purpose: To transform the OBX segment, field 18 ---- Equipment Identifier (EI)
        DO OBX18^TMGHL72
        QUIT
        ;
NTE3    ;"Purpose: To transform the NTE segment, field 3 (the comments)
        ;"Note: This handles NTE's after OBX's.  
        ;"      NTE's after OBR's are handled in OBRDN
        SET TMGLABCORPNAME=1   ;"//kt NOTE: killed below. 
        DO NTE3^TMGHL73     
        KILL TMGLABCORPNAME
        QUIT
        ;
SUORL   ;"Purpose: Setup TMGINFO("ORL") and TMGINFO("LOC") and TMGINFO("INSTNAME")
        DO SUORL^TMGHL72
        QUIT
        ;
TRTEST(TESTNUM,TESTNAME)  ;"Purpose: translate test name
        NEW TMGRESULT SET TMGRESULT=TESTNAME
        IF TMGRESULT["RESULT" QUIT TESTNAME
        ;"IF TMGRESULT["MICROSCOPIC OBSERVATION" QUIT "MICROSCOPIC:"
        ;"NEW TEMPNAME SET TEMPNAME=$$UP^XLFSTR(TESTNAME)
        NEW FOUND SET FOUND=0
        NEW SUBITEM SET SUBITEM=0
        FOR  SET SUBITEM=$O(^LAB(60,TESTNUM,5,SUBITEM)) QUIT:(SUBITEM'>0)!(FOUND=1)  DO
        . NEW SYN SET SYN=$G(^LAB(60,TESTNUM,5,SUBITEM,0))
        . IF SYN["LC-" DO
        . . NEW TEXT SET TEXT=$P(SYN,"LC-",2)
        . . IF +TEXT=TEXT QUIT   ;"Don't use numeric results
        . . IF (TMGRESULT["MICROSCOPIC OBSERVATION")&(TEXT=".") QUIT  
        . . IF ($E(TEXT,1,1)'=1)&($E(TEXT,1,1)'=0) DO
        . . . SET TMGRESULT=$$UP^XLFSTR(TEXT)
        . . . SET FOUND=1
        IF FOUND=0 DO
        . IF TESTNAME["MONOS" SET TMGRESULT="MONOCYTES"
        . IF TESTNAME["AST" SET TMGRESULT="AST (SGOT)"
        . IF TESTNAME["GRANULOCYTES, IMMATURE" SET TMGRESULT="IMMATURE GRANULOCYTES"
        ;"
        QUIT TMGRESULT
        ;"
        
