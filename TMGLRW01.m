TMGLRW01 ;TMG/kst-Entry point for writing to LAB DATA file ; 5/23/17
              ;;1.0;TMG-LIB;**1**;06/20/13
 ;
 ;"TMG LAB RESULTS STORAGE API
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
 ;"FILEMSG(IEN62D4,TMGHL7MSG) -- FILE HL7 MESSAGE INTO LAB DATA FILE (63)
 ;"LRWRITE(DFN,ARRAY,LABTYPE,FLAGS,ALERTS) -- Store data in LAB DATA (^LR), file# 63
 ;"PARSXTRA(REF,DATA,DIV1,DIV2) ;"Parse lab dataline back into XTRA array
 ;"
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;"GETINFO(TMGHL7MSG,MSGINFO) -- PARSE HEADER INFO INTO USABLE ARRAY
 ;"FILESUBM(INFO,MSGARR,DATESUSED,ALERTS) -- FILE SUB HL7 MESSAGE
 ;"PREPINFO(INFO,FILEARR) -- Fill in FILEARR with relevant parts of INFO
 ;"PREPOBR(INFO,OBRARR,FILEARR,DATESUSED) -- Prepair array for LRWRITE
 ;"UNIQUEDT(FMDT,DATESUSED) -- Slightly change date such that date is unique
 ;"STORNOTE(SUBFILE,IENS,NTEARR) ;
 ;"PREPOBX(INFO,ARR,FILEARR) -- prepare one OBX result
 ;"COMPLXTR(REF,DIV) -- COMPILE XTRA ARRAY
 ;"GETPREFX(INFO,NLTCODE) -- Get Prefix for notes. 
 ;"NLT2IEN(INFO,NLTCODE,IEN62D41) -- CONVERT NLT TO IEN62D41
 ;"DEPRECIATED -- FILESARR(DFN,FNUM,IENS,ARRAY,FLAGS,TMGRESULT) -- Prepare user input for passing into Fileman
 ;"FILE1ARR(LRDFN,FNUM,IENS,ARRAY,FLAGS) -- File user input with Fileman
 ;"SETUPFDA(FNUM,IEN,PARENTIENS,SRCARRAY,TMGFDA) -- Setup for FDA for fileman
 ;"NEWLRDFN(DFN,MSG) -- Make NEW LAB DATA store record (IEN is LRDFN)
 ;"SUBFNUM(FILE,FIELD) -- Determine IF field is a subfile
 ;"PRIORLDT(TMGHL7MSG,DATESUSED,MSGINFO) -- PRIOR LAB DATES
 ;"DELPRIOR(FILEARR,DATESUSED,INFO)  -- DELETE PRIOR FILINGS 
 ;"DELLAB(LRDFN,DT,FLD)  -- DELETE 1 LAB 
 ;"TEST -- test filing.
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"  TMGDEBU2
 ;"=======================================================================
 ;
FILEMSG(TMGENV,TMGHL7MSG) ;"FILE HL7 MESSAGE INTO LAB DATA FILE (63)
        ;"Input: TMGENV -- PASS BY REFERENCE.  Lab environment
        ;"           TMGENV("PREFIX") -- e.g. "LMH"
        ;"           TMGENV("IEN 68.2") -- IEN in LOAD/WORK LIST (holds orderable items)
        ;"           TMGENV("IEN 62.4") -- IEN in AUTO INSTRUMENT (holds resultable items)
        ;"           TMGENV("TMGU",1)=FS
        ;"           TMGENV("TMGU",2)=$EXTRACT(ECH,1)
        ;"           TMGENV("TMGU",3)=$EXTRACT(ECH,4)
        ;"           TMGENV(<other entries>)= etc.              
        ;"       TMGHL7MSG -- array as created by PARSMSG2^TMGHL7X2
        ;"       TMGU -- PASS BY REFERENCE.  Array containing piece divider chars for HL7 message
        ;"Result: 1 if OK, or -1^Error Msg IF any
        NEW TMGLRDEBUG SET TMGLRDEBUG=0
        IF TMGLRDEBUG=1 DO
        . KILL TMGHL7MSG
        . MERGE TMGHL7MSG=^TMG("TMP","TMGHL7MSG")   ;"TO DO - REMOVE
        . SET IEN62D4=204 ;"TO DO - REMOVE
        NEW TMGRESULT SET TMGRESULT=1
        NEW MSGINFO,DATESUSED
        NEW NTEARR,FILEARR
        NEW TMGU MERGE TMGU=TMGENV("TMGU")
        SET IEN62D4=+$GET(TMGENV("IEN 62.4"))
        IF IEN62D4'>0 DO  GOTO FMGDN
        . SET TMGRESULT="-1^In FILEMSG.TMGLRW01: No IEN for file 62.4 found."
        SET MSGINFO("IEN 62.4")=IEN62D4
        SET TMGRESULT=$$GETINFO(.TMGHL7MSG,.MSGINFO)
        IF +TMGRESULT<0 GOTO FMGDN
        DO PRIORLDT(.TMGHL7MSG,.DATESUSED,.MSGINFO) ;"Fill used prior lab dates
        ;        
        NEW IDX SET IDX=0
        NEW DONE SET DONE=0
        NEW HDRNTEARR
        FOR  SET IDX=$ORDER(TMGHL7MSG(IDX)) QUIT:(+IDX'>0)!DONE  DO
        . NEW STR SET STR=$GET(TMGHL7MSG(IDX))
        . NEW SEGTYPE SET SEGTYPE=$GET(TMGHL7MSG(IDX,"SEG"))
        . IF SEGTYPE="OBR" SET DONE=1 QUIT
        . IF SEGTYPE="NTE" DO
        . . SET HDRNTEARR(IDX)=STR
        ;
        NEW ALERTS
        NEW OBRIDX SET OBRIDX=0
        FOR  SET OBRIDX=$ORDER(TMGHL7MSG("B","OBR",OBRIDX)) QUIT:(+OBRIDX'>0)!(+TMGRESULT<0)  DO
        . NEW SUBMSGARR
        . SET TMGRESULT=$$GETSUBMSG(.TMGHL7MSG,OBRIDX,.SUBMSGARR)
        . IF +TMGRESULT<0 QUIT
        . SET TMGRESULT=$$FILESUBM(.MSGINFO,.SUBMSGARR,.DATESUSED,.ALERTS)
        . IF +TMGRESULT<0 QUIT
        . NEW SUBFILE SET SUBFILE=$PIECE($PIECE(TMGRESULT,"^",2),":",1)
        . NEW IENS SET IENS=$PIECE($PIECE(TMGRESULT,"^",2),":",2)
        . ;"Gather notes in this sub-message.         
        . NEW IDX SET IDX=0
        . NEW FORTESTMSG,NTEARR
        . FOR  SET IDX=$ORDER(SUBMSGARR(IDX)) QUIT:(+IDX'>0)  DO
        . . NEW STR SET STR=$GET(SUBMSGARR(IDX))
        . . NEW SEGTYPE SET SEGTYPE=$GET(SUBMSGARR(IDX,"SEG"))
        . . IF SEGTYPE="OBX" DO
        . . . NEW NLT SET NLT=$GET(SUBMSGARR(IDX,3,1))
        . . . NEW FOR1TEST SET FOR1TEST=$$GETPREFX(.INFO,NLT)
        . . . IF $$TRIM^XLFSTR(FOR1TEST)="" QUIT
        . . . SET FORTESTMSG(IDX)=FOR1TEST
        . . IF SEGTYPE="NTE" DO
        . . . SET NTEARR(IDX)=$GET(SUBMSGARR(IDX))
        . . . MERGE NTEARR=FORTESTMSG
        . . . KILL FORTESTMSG
        . . IF SEGTYPE="OBR" SET NTEARR(IDX)="NTE",$PIECE(NTEARR(IDX),TMGU(1),4)=" "  ;"e.g. 'NTE||| '
        . MERGE NTEARR=HDRNTEARR
        . SET TMGRESULT=$$STORNOTE(SUBFILE,IENS,.NTEARR)
        IF +TMGRESULT<0 GOTO FMGDN
        ;"Next, send alert that lab has been filed and is available for review.
        NEW PROV SET PROV=$GET(MSGINFO("PROV IEN")) IF PROV'>0 GOTO FMGDN
        NEW ALERTSTR,STR SET (ALERTSTR,STR)=""
        FOR  SET STR=$ORDER(ALERTS(STR)) QUIT:(STR="")  DO
        . IF +$PIECE(STR,"^",3)'<+$PIECE(ALERTSTR,"^",3) SET ALERTSTR=STR
        IF ALERTSTR="" GOTO FMGDN
        NEW DFN SET DFN=$PIECE(ALERTSTR,"^",1)
        NEW DT SET DT=$PIECE(ALERTSTR,"^",2)
        NEW LEVEL SET LEVEL=$PIECE(ALERTSTR,"^",3)        
        NEW NODE SET NODE=$PIECE(ALERTSTR,"^",4)
        NEW SUPPRESS SET SUPPRESS=1  ;"Don't send if duplicate
        NEW TEMP SET TEMP=$$ALERT^TMGLRWU2(PROV,DFN,DT,LEVEL,NODE,SUPPRESS)  ;"Send the alert
        IF +TEMP'>0 SET TMGRESULT=TEMP
FMGDN   QUIT TMGRESULT
        ;
GETSUBMSG(TMGHL7MSG,OBRIDX,SUBMSGARR) ;
        ;"Purpose: Return array with specified OBR index and all NTE and OBX 
        ;"         segments that follow, until end of array encountered, or 
        ;"         next OBR segment encountered.
        ;"NOTE: uses TMGU by global scope (contains piece divider chars)
        ;"Result: 1 IF ok, OR -1^Error Msg IF any
        MERGE SUBMSGARR(OBRIDX)=TMGHL7MSG(OBRIDX)
        SET TMGRESULT=1
        NEW OBRFOUND SET OBRFOUND=0
        FOR  SET OBRIDX=$ORDER(TMGHL7MSG(OBRIDX)) QUIT:(+OBRIDX'>0)!(OBRFOUND=1)  DO
        . NEW STR SET STR=$GET(TMGHL7MSG(OBRIDX))
        . NEW SEGTYPE SET SEGTYPE=$PIECE(STR,TMGU(1),1)           
        . IF SEGTYPE="OBR" SET OBRFOUND=1 QUIT
        . MERGE SUBMSGARR(OBRIDX)=TMGHL7MSG(OBRIDX)
        QUIT TMGRESULT
        ;
GETINFO(TMGHL7MSG,MSGINFO)  ;"PARSE HEADER INFO INTO USABLE ARRAY
        ;"Input: TMGHL7MSG.  PASS BY REFERENCE.  Parsed HL7 message array
        ;"       MSGINFO.  PASS BY REFERENCE.  AN OUT PARAMETER
        ;"Output: MSGINFO is filled. 
        ;"Result: 1 if OK, or -1^Error Msg IF any
        NEW TMGRESULT SET TMGRESULT=1
        NEW IDX SET IDX=0
        NEW SEGMENT,DATA
        FOR  SET IDX=$ORDER(TMGHL7MSG(IDX)) QUIT:(+IDX'>0)!(+TMGRESULT'>0)  DO
        . SET DATA=$GET(TMGHL7MSG(IDX))
        . SET SEGMENT=$PIECE(DATA,TMGU(1),1)
        . IF SEGMENT="MSH" DO   ;"Process header with sender information
        . . SET MSGINFO("LAB IEN4")=$GET(TMGHL7MSG(IDX,4,2))_"^"_$GET(TMGHL7MSG(IDX,4,1))
        . IF SEGMENT="PID" DO   ;"Process patient segment
        . . SET MSGINFO("NAME")=$GET(TMGHL7MSG(IDX,5,1))_","_$GET(TMGHL7MSG(IDX,5,2))
        . . NEW DOB SET DOB=$GET(TMGHL7MSG(IDX,7))
        . . SET MSGINFO("DOB")=$E(DOB,5,6)_"-"_$E(DOB,7,8)_"-"_$E(DOB,3,4)  
        . . SET MSGINFO("SEX")=$GET(TMGHL7MSG(IDX,8))
        . . SET MSGINFO("SSNUM")=$TRANSLATE($GET(TMGHL7MSG(IDX,19)),"-","")
        . . SET MSGINFO("DFN")=$$GETDFN^TMGGDFN(.MSGINFO,0)
        . . IF $PIECE(MSGINFO("DFN"),"^",1)<1 DO  QUIT
        . . . SET TMGRESULT="-1^Patient not found in system: "_MSGINFO("NAME")_" ("_MSGINFO("DOB")_"). Lookup routine says: "
        . . . SET TMGRESULT=TMGRESULT_$PIECE(MSGINFO("DFN"),"^",2)  
        . IF SEGMENT="PV1" DO   ;"Process provider information
        . . IF $DATA(MSGINFO("PROV IEN")) QUIT
        . . SET MSGINFO("NPI")=$GET(TMGHL7MSG(IDX,8,1))
        . . SET MSGINFO("PROV IEN")=$ORDER(^VA(200,"ANPI",MSGINFO("NPI"),0))
        . . IF MSGINFO("PROV IEN")'>0 SET TMGRESULT="-1^PROVIDER NPI '"_MSGINFO("NPI")_"' NOT FOUND" 
        . IF SEGMENT="ORC" DO  ;"Process order information
        . . SET MSGINFO("LOCATION")=$GET(TMGHL7MSG(IDX,13,1))
        . . SET MSGINFO("LOCATION IEN")=$GET(TMGHL7MSG(IDX,13,2))
        . . SET MSGINFO("ORDER UID")=$GET(TMGHL7MSG(IDX,3))
        . . IF ($GET(MSGINFO("PROV IEN"))=""),$GET(TMGHL7MSG(IDX,12,13))="TMGDUZ" DO
        . . . SET MSGINFO("PROV IEN")=+$GET(TMGHL7MSG(IDX,12,1))
        NEW TMGDFN SET TMGDFN=$GET(MSGINFO("DFN"))
        NEW LRDFN SET LRDFN=+$PIECE($GET(^DPT(TMGDFN,"LR")),"^",1)
        SET MSGINFO("LRDFN")=LRDFN
GINFDN  QUIT TMGRESULT
        ;
FILESUBM(INFO,MSGARR,DATESUSED,ALERTS) ;"FILE SUB HL7 MESSAGE
        ;"Input: INFO -- Array containing information needed for filing. 
        ;"       MSGARR -- Array with message to be filed.  Format similar to 
        ;"                 TMGHL7MSG, but only header segments, and ONE SET of
        ;"                 results returned, i.e. OBR and resulting OBX and NTE segments
        ;"       DATESUSED --PASS BY REFERENCE.  Array of other date/times already found for other OBR segments.
        ;"             format: DATESUSED(FMDT)=""   <-- was DATESUSED(FMDT_" ")=""
        ;"                     DATESUSED(FMDT,LabNum)=""   <-- was DATESUSED(FMDT_" ",LabNum)=""
        ;"                     DATESUSED("B",LabNum,FMDT)=""  <-- DATESUSED("B",LabNum,FMDT_" ")=""
        ;"       ALERTS -- AN OUT PARAMETER.  PASS BY REFERENCE.  FORMAT:
        ;"             ALERTS("DFN^FMDT^LEVEL^NODE")=""
        ;"Result: 1^File#:IENS if OK, or -1^Error Msg IF any
        NEW TMGRESULT SET TMGRESULT=1
        NEW NTEARR,IENS,OBRARR,FILEARR
        SET TMGRESULT=$$PREPINFO(.INFO,.FILEARR)
        NEW OBRIDX SET OBRIDX=$ORDER(MSGARR(0))
        IF (OBRIDX="")!($PIECE($GET(MSGARR(OBRIDX)),TMGU(1),1)'="OBR") DO  GOTO FSMDN
        . SET TMGRESULT="-1^OBR not found in MSGARR in FILESUBM^TMGLRW01"
        MERGE OBRARR=MSGARR(OBRIDX)
        ;"Collect array of labs to be placed in DT, for used to avoid mixing
        ;"  this set with different labs from a different OBR+OBX set.
        NEW OBSDATETIME SET OBSDATETIME=+$$HL72FMDT^TMGHL7U3($GET(OBRARR(7)))
        NEW NEWDATES SET NEWDATES(OBSDATETIME)=""
        NEW OBXIDX SET OBXIDX=0
        FOR  SET OBXIDX=$ORDER(MSGARR(OBXIDX)) QUIT:(+OBXIDX'>0)!(+TMGRESULT'>0)  DO
        . NEW STOREFLD SET STOREFLD=$PIECE($GET(MSGARR(OBXIDX,"RESULT","STORAGE")),"^",2) QUIT:STOREFLD=""
        . SET NEWDATES(OBSDATETIME,STOREFLD)=""
        . SET NEWDATES("B",STOREFLD,OBSDATETIME)=""
        NEW IDX SET IDX=0
        SET TMGRESULT=$$PREPOBR(.INFO,.OBRARR,.FILEARR,.DATESUSED,.NEWDATES)   
        NEW OBXIDX SET OBXIDX=0
        FOR  SET OBXIDX=$ORDER(MSGARR(OBXIDX)) QUIT:(+OBXIDX'>0)!(+TMGRESULT'>0)  DO
        . NEW STR SET STR=$GET(MSGARR(OBXIDX))
        . NEW SEGTYPE SET SEGTYPE=$GET(MSGARR(OBXIDX,"SEG"))
        . IF SEGTYPE'="OBX" QUIT
        . NEW ARR MERGE ARR=MSGARR(OBXIDX)
        . SET TMGRESULT=$$PREPOBX(.INFO,.ARR,.FILEARR)
        ;"NOTICE: At this point were have data to be filed in FILEARR
        ;"    and records of data already file in database in DATESUSED("B").
        ;"    If an HL7 message is processed twice, or a repeat is sent in, then
        ;"    the prior process will have changed the Date-time slightly (adding a 
        ;"    few seconds or minutes) such that it will here be file a SECOND time
        ;"    leading to duplicate lab displaying in CPRS.
        ;"  SO, I will delete prior entries before filing the new ones...
        SET TMGRESULT=$$DELPRIOR(.FILEARR,.DATESUSED,.INFO)
        IF +TMGRESULT'>0 GOTO FSMDN
        SET TMGRESULT=$$LRWRITE(INFO("DFN"),.FILEARR,"CH","ES",.ALERTS)
FSMDN   QUIT TMGRESULT
        ;
PREPINFO(INFO,FILEARR)
        ;"Purpose: to fill in FILEARR with relevant parts of INFO
        ;"Result: 1 if OK, or -1^Error Msg IF any
        ;"TO DO - DECIDE IF THIS IS NECESSARY - ALL WE NEEDED WAS DFN
        NEW TMGRESULT SET TMGRESULT=1
        QUIT TMGRESULT
        ;
PREPOBR(INFO,OBRARR,FILEARR,DATESUSED,NEWDATES) ;"prepair array for LRWRITE from OBR
        ;"Input: INFO -- PASS BY REFERENCE.  Array containing information needed for filing. 
        ;"       OBRARR -- PASS BY REFERENCE.  Array (a subset from TMGHL7MSG) containing parsed OBR segment
        ;"       FILEARR - PASS BY REFERENCE.  An IN & OUT PARAMETER.  Format: FILEARR(FLD#)=STORE_VALUE
        ;"       DATESUSED --PASS BY REFERENCE.  Array of other date/times already found for other other OBR segments.  
        ;"                     This will be used to give each OBR (and corresponding OBX segs) a unique grouping.
        ;"       NEWDATES -- PASS BY REFERENCE.  Array of dates and and tests to be filed in 
        ;"                     this grouping, in same format as DATESUSED.  Can be used
        ;"                     to avoid mixing this group with any other OBR+OBX grouping.
        ;"Result: 1 if OK, or -1^Error Msg if any
        NEW TEMPDATE
        NEW TMGRESULT SET TMGRESULT=1
        ;"TO DO - VERIFY DATES
        NEW OBSDATETIME SET OBSDATETIME=$GET(OBRARR(7))  ;"7= Observation Date/Time
        SET OBSDATETIME=+$$HL72FMDT^TMGHL7U3(OBSDATETIME)
        NEW USEUNQ SET USEUNQ=1
        IF USEUNQ=1 SET OBSDATETIME=$$UNIQUEDT(OBSDATETIME,.DATESUSED,.NEWDATES) ;
        SET FILEARR(.01)=OBSDATETIME  ;"<-- DATE/TIME SPECIMEN TAKEN      was: -- $E(TEMPDATE,5,6)_"/"_$E(TEMPDATE,7,8)_"/"_$E(TEMPDATE,1,4) 
        SET FILEARR(.02)=0  ;"DATE INEXACT 0=NO
        NEW RECEIVEDDATETIME SET RECEIVEDDATETIME=$GET(OBRARR(14))  ;"14 = Specimen Received Date/Time
        SET RECEIVEDDATETIME=$$HL72FMDT^TMGHL7U3(RECEIVEDDATETIME)  ;"This doesn't need to be made unique...
        SET FILEARR(.03)=RECEIVEDDATETIME ;"<-- DATE REPORT COMPLETED    was: -- $E(TEMPDATE,5,6)_"/"_$E(TEMPDATE,7,8)_"/"_$E(TEMPDATE,1,4)
        SET FILEARR(.04)="`"_DUZ
        SET FILEARR(.05)="`"_$GET(OBRARR(15,4))  ;"specimen
        SET FILEARR(.112)="`"_INFO("LOCATION IEN")  ;"originally SET up in XORC13^TMGHL73
        ;
        SET INFO("ORDER NLT")=$PIECE($GET(OBRARR("ORDER","IEN64")),"^",2)
        GOTO POBRDN ;"<--- remove later
        NEW SPECIEN61 SET SPECIEN61=+$GET(OBRARR(15,4))
        IF SPECIEN61>0 SET FILEARR("CUSTOM",5,1)=SPECIEN61
        NEW ORDERNLT SET ORDERNLT=$PIECE($GET(OBRARR("ORDER","IEN64")),"^",2)
        SET FILEARR("CUSTOM",3,1)=ORDERNLT
POBRDN  QUIT TMGRESULT
        ;
UNIQUEDT(FMDT,DATESUSED,NEWDATES) ;
        ;"Purpose: To slightly change date such that date is unique
        ;"Input: FMDT -- Fileman date/time
        ;"       DATESUSED -- PASS BY REFERENCE.  An array of dates that should NOT be duplicated.  Format:
        ;"          DATESUSED(FMDT)=""  <-- was DATESUSED(FMDT_" ")=""
        ;"             NOTE: the reason I stopped using FMDT_" " was that FMDT was 
        ;"                  sometimes being derived from 9999999-RDT.  And since 
        ;"                 the RDT was in cannonical form, then the FMDT was also
        ;"                in cannonical form.  And by storing FMDT_" ", it was 
        ;"               setting up situation where a date (from a different source)
        ;"              with a time ending in "0" was different, and thus not
        ;"              properly detected as already being present.  
        ;"       NEWDATES -- PASS BY REFERENCE.  Array of NEW dates and and tests to be filed in 
        ;"                     this grouping, in same format as DATESUSED.  Can be used
        ;"                     to avoid mixing this group with any other OBR+OBX grouping.
        ;"                     NOTE: I didn't end up needing this array, but I left here
        ;"                     in cased it might be needed in the future. 
        ;"Result: A unique FM date/time
        ;"Output: DATESUSED will be modified, such that NEW unique date is added to array.
        NEW TMGRESULT 
        IF $$SUPPTIME^TMGHL7U2()=1 SET TMGRESULT=FMDT GOTO UDTDN  ;"ELH 2/10/15        
        ;"FOR  QUIT:($DATA(DATESUSED(FMDT_" "))=0)  DO
        FOR  QUIT:($DATA(DATESUSED(FMDT))=0)  DO
        . SET FMDT=$$FMADD^XLFDT(FMDT,0,0,0,1)
        SET TMGRESULT=FMDT
        ;"SET DATESUSED(FMDT_" ")=""        
        SET DATESUSED(FMDT)=""        
UDTDN   QUIT TMGRESULT
        ;
STORNOTE(SUBFILE,IENS,NTEARR,WANTDUPL) ;
        ;"Result: 1 if OK, or -1^Error Msg IF any
        ;"INPUT: SUBFILE -- Subfile number.  Must be 63.04
        ;"       IENS -- IENS to store into file 63.04
        ;"       NTEARR -- array of notes.  Format expected:
        ;"         NTEARR(#)="aaa|bbb|ccc|<text of message>"  aaa,bbb,ccc are discarded, can be null
        ;"       WANTDUPL -- OPTIONAL.  default=0.  If 1 then duplicate messages
        ;"                 will be stored.  Otherwise note only stored IF not already present.
        ;"NOTE: May use TMGU by global scope, but IF not present "|" used as $piece div char.
        ;"Result: 1 if OK, or -1^Message IF problem
        NEW TMGRESULT SET TMGRESULT=1
        IF SUBFILE'=63.04 DO  GOTO STNTDN
        . SET TMGRESULT=-"-1^In STORNOTE.TMGLRW01 Can't store notes into subfile: '"_SUBFILE_"'"
        NEW DIV SET DIV=$GET(TMGU(1),"|")        
        NEW ADDIENS SET ADDIENS="+1,"_IENS
        NEW WRAPWIDTH SET WRAPWIDTH=90  ;"//65
        ;"Do word wrapping, if needed.        
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(NTEARR(IDX)) QUIT:(+IDX'>0)!(+TMGRESULT'>0)  DO
        . NEW MSG SET MSG=$GET(NTEARR(IDX))
        . SET MSG=$PIECE(MSG,DIV,4) 
        . IF $LENGTH(MSG)>(WRAPWIDTH+2) DO   ;"67
        . . NEW WRAPMSG SET WRAPMSG=$EXTRACT(MSG,WRAPWIDTH+1,$LENGTH(MSG))
        . . SET $PIECE(NTEARR(IDX+0.1),DIV,4)="  - "_WRAPMSG
        . . SET MSG=$EXTRACT(MSG,1,WRAPWIDTH)_" -"
        . . SET $PIECE(NTEARR(IDX),DIV,4)=MSG
        IF $GET(WANTDUPL)=1 GOTO SN2
        ;"Check IF message is already present
        NEW IEN,SUBIEN SET SUBIEN=+IENS,IEN=$PIECE(IENS,",",2)
        NEW NTIDX SET NTIDX=+$ORDER(NTEARR(0))
        NEW MATCH SET MATCH=0
        SET IDX=0
        FOR  SET IDX=$ORDER(^LR(IEN,"CH",SUBIEN,1,IDX)) QUIT:(+IDX'>0)!(MATCH=-1)!(NTIDX="")  DO
        . NEW CMT SET CMT=$GET(^LR(IEN,"CH",SUBIEN,1,IDX,0))
        . FOR  QUIT:$EXTRACT(CMT,$LENGTH(CMT))'=" "  S CMT=$EXTRACT(CMT,1,$LENGTH(CMT)-1)  ;"RTRIM space
        . NEW MSG SET MSG=$PIECE($GET(NTEARR(NTIDX)),DIV,4)
        . FOR  QUIT:$EXTRACT(MSG,$LENGTH(MSG))'=" "  S MSG=$EXTRACT(MSG,1,$LENGTH(MSG)-1)  ;"RTRIM space
        . IF MATCH=0 DO
        . . IF MSG'=CMT QUIT
        . . SET MATCH=1
        . . SET NTIDX=$ORDER(NTEARR(NTIDX))        
        . ELSE  IF MATCH=1 DO
        . . IF CMT=MSG DO
        . . . SET NTIDX=$ORDER(NTEARR(NTIDX))
        . . ELSE  SET MATCH=0  ;"WAS -1
        IF MATCH GOTO STNTDN  ;"Note already present. 
SN2     SET IDX=0
        FOR  SET IDX=$ORDER(NTEARR(IDX)) QUIT:(+IDX'>0)!(+TMGRESULT'>0)  DO
        . NEW MSG SET MSG=$GET(NTEARR(IDX))
        . SET MSG=$PIECE(MSG,DIV,4) 
        . IF MSG="" QUIT
        . NEW TMGFDA,TMGIEN,TMGMSG
        . SET TMGFDA(63.041,ADDIENS,.01)=MSG
        . DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) DO  GOTO STNTDN 
        . . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
STNTDN  QUIT TMGRESULT
        
PREPNTE(NTEARR,FILEARR) ;"prep NTE   "//DEPRECIATED
        ;"Result: 1 if OK, or -1^Error Msg IF any
        NEW TMGRESULT SET TMGRESULT=1
        NEW IDX SET IDX=0
        NEW MSG
        SET FILEARR(.99)="SUBFILE"
        FOR  SET IDX=$ORDER(NTEARR(IDX)) QUIT:(+IDX'>0)  DO
        . SET MSG=$GET(NTEARR(IDX))
        . SET MSG=$PIECE(MSG,TMGU(1),4)
        . SET FILEARR(.99,IDX,.01)=MSG
        . ;"SET FILEARR(.99,IDX,1)="`"_DUZ
        QUIT TMGRESULT
        ;
PREPOBX(INFO,ARR,FILEARR) ;"prepare one OBX result
        ;"Input: INFO -- PASS BY REFERENCE.  An array containing needed info.
        ;"       ARR -- PASS BY REFERENCE.  Array containg the parsed OBX segment to prepair
        ;"       FILEARR -- PASS BY REFERENCE. AN OUT PARAMETER. 
        ;"Result: 1 if OK, or -1^Error Msg IF any
        ;"
        NEW TMGRESULT SET TMGRESULT=1
        NEW NLT SET NLT=$GET(ARR(3,1))
        NEW FLD SET FLD=+$PIECE($GET(ARR("RESULT","STORAGE")),"^",2)
        IF FLD'>0 DO  GOTO POXDN
        . SET TMGRESULT="-1^Storage location not found in OBX RESULT STORAGE node for test: "_$PIECE($GET(ARR(3)),"^",1,2)
        NEW VALUE SET VALUE=$GET(ARR(5))
        ;"Removed below.  This might cause input transform to fail after
        ;"  my code has screened for this.  Instead, changed in XFORM for OBX5
        ;"IF VALUE="" SET VALUE=" "  ;"Labs won't be shown in CPRS if value="", but will if value=" " 
        SET FILEARR(FLD)=VALUE
        ;
        NEW IEN60 SET IEN60=$GET(ARR("RESULT","IEN60"))
        NEW IEN4 SET IEN4=$GET(INFO("LAB IEN4")) IF IEN4'="" SET IEN4=+IEN4
        NEW UNITS SET UNITS=$GET(ARR(6))
        NEW REFRANGE SET REFRANGE=$GET(ARR(7))
        NEW REFLO SET REFLO=$PIECE(REFRANGE,"-",1)
        NEW REFHI SET REFHI=$PIECE(REFRANGE,"-",2)
        NEW ABNFLAG SET ABNFLAG=$GET(ARR(8)) 
        NEW LABNAME SET LABNAME=$GET(ARR(15,1))
        NEW OBSDATETIME SET OBSDATETIME=$$HL72FMDT^TMGHL7U3($GET(ARR(14)))
        NEW NLTRESULT SET NLTRESULT=$PIECE($GET(ARR("RESULT","IEN64")),"^",2)
        NEW NLTORDER SET NLTORDER=$GET(INFO("ORDER NLT"))
        IF ABNFLAG'="" SET FILEARR("FLAGS",ABNFLAG)=""
        ;
        SET FILEARR(.12)=2  ;"NEW PERSON CONVERSION -- affects RPC from CPRS
        ;
        SET FILEARR(FLD,"XTRA",1)=VALUE        ;"Test value
        SET FILEARR(FLD,"XTRA",2)=ABNFLAG      ;"Flag (H for High, L for Low, HH for very high)
        SET FILEARR(FLD,"XTRA",3)="!"          ;"(marker of subpieces divider)
        SET FILEARR(FLD,"XTRA",3,1)=NLTORDER   ;"National Lab Code
        SET FILEARR(FLD,"XTRA",3,2)=NLTRESULT  ;"Result national Lab code (can have a suffix)
        SET FILEARR(FLD,"XTRA",3,3)=""         ;"LOINC without checksum character"
        SET FILEARR(FLD,"XTRA",3,4)="0000"     ;"Workload Suffix"
        SET FILEARR(FLD,"XTRA",3,5)=""         ;"(not used)
        SET FILEARR(FLD,"XTRA",3,6)=""         ;"(not used)
        SET FILEARR(FLD,"XTRA",3,7)=IEN60      ;"IEN Pointer to File 60 ^LAB(60,
        SET FILEARR(FLD,"XTRA",4)=DUZ          ;"Verifying Technologist, pointer to file 200
        SET FILEARR(FLD,"XTRA",5)="!"          ;"(marker of subpieces divider)
        SET FILEARR(FLD,"XTRA",5,1)=""         ;"Site/Specimen, IEN pointer to file 61 ^LAB(61
        SET FILEARR(FLD,"XTRA",5,2)=REFLO      ;"Reference Low
        SET FILEARR(FLD,"XTRA",5,3)=REFHI      ;"Reference High
        SET FILEARR(FLD,"XTRA",5,4)=""         ;"Critical Low
        SET FILEARR(FLD,"XTRA",5,5)=""         ;"Critical High
        SET FILEARR(FLD,"XTRA",5,6)=""         ;"(not used)
        SET FILEARR(FLD,"XTRA",5,7)=UNITS      ;"Units
        SET FILEARR(FLD,"XTRA",5,8)=""         ;"Type of Delta Check
        SET FILEARR(FLD,"XTRA",5,9)=""         ;"Delta Value
        SET FILEARR(FLD,"XTRA",5,10)=""        ;"Default Value
        SET FILEARR(FLD,"XTRA",5,11)=""        ;"Therapeutic Low
        SET FILEARR(FLD,"XTRA",5,11)=""        ;"Therapeutic High
        SET FILEARR(FLD,"XTRA",6)=OBSDATETIME  ;"(not used) <-- but example holds standard FM date. 
        SET FILEARR(FLD,"XTRA",7)=""           ;"(not used)
        SET FILEARR(FLD,"XTRA",8)=""           ;"(not used)
        SET FILEARR(FLD,"XTRA",9)=IEN4         ;"Pointer to file 4, Institution performing the test
        SET FILEARR(FLD,"XTRA",10)=""          ;"HL7 Equipment Entity Identifier -- EEI
        SET FILEARR(FLD,"XTRA",11)=LABNAME     ;"(not used) <-- example holds "PATHGROUP"
        ;
        SET FILEARR(FLD,"XTRA")=$$COMPLXTR($NAME(FILEARR(FLD,"XTRA")))
        ;        
POXDN   QUIT TMGRESULT
        ;
COMPLXTR(REF,DIV) ; 
        ;"Input: REF -- PASS BY NAME.  XTRA array to compile.
        ;"       DIV -- OPTIONAL, divider marker. DEFAULT=^
        ;"NOTE: a reverse of this function can be achieved via PARSXTRA^TMGLRR02
        SET DIV=$GET(DIV,"^")
        NEW PCE SET PCE=0
        NEW TMGRESULT SET TMGRESULT=""
        FOR  SET PCE=$ORDER(@REF@(PCE)) QUIT:(+PCE'>0)  DO
        . NEW VAL SET VAL=$GET(@REF@(PCE))
        . IF VAL="!" SET VAL=$$COMPLXTR($NAME(@REF@(PCE)),"!")
        . SET $PIECE(TMGRESULT,DIV,PCE)=VAL
        QUIT TMGRESULT
        ;
GETPREFX(INFO,NLTCODE) ;
        ;"Input:INFO -- PASS BY REFERENCE.  An array containing needed info.  Specifically INFO('IEN 62.4')
        ;"      NLTCODE -- NLT code, e.g. 82341.0000
        ;"Result: Prefix phrase for notes, IF any, or "" IF none found or IF problem.
        NEW TMGRESULT SET TMGRESULT=""
        NEW IEN62D4 SET IEN62D4=+$GET(INFO("IEN 62.4"))
        NEW IEN62D41
        IF $$NLT2IEN(.INFO,NLTCODE,.IEN62D41)'>0 GOTO GPFXDN
        SET $PIECE(TMGRESULT,TMGU(1),4)=$PIECE($GET(^LAB(62.4,IEN62D4,3,IEN62D41,2)),"^",8)
GPFXDN  QUIT TMGRESULT        

NLT2IEN(INFO,NLTCODE,IEN62D41) ;"CONVERT NLT TO IEN62D41
        ;"Input:INFO -- PASS BY REFERENCE.  An array containing needed info.  Specifically INFO('IEN 62.4')
        ;"      NLTCODE -- NLT code, e.g. 82341.0000
        ;"      IEN62D41 -- PASS BY REFERENCE, AN OUT PARAMETER.  See output. 
        ;"Result: 1 if OK, or -1^Error Msg IF any
        ;"Output: IEN62D41 is filled with IEN IF found, or 0 IF not. 
        NEW TMGRESULT SET TMGRESULT=1
        NEW IEN62D4 SET IEN62D4=+$GET(INFO("IEN 62.4"))
        SET IEN62D41=+$ORDER(^LAB(62.4,IEN62D4,3,"AC",NLTCODE,0))
        IF IEN62D41'>0 DO  
        . SET TMGRESULT="-1^Unable to find NLT code '"_NLTCODE_"' in file #62.4, record #"_IEN62D4_"."
        QUIT TMGRESULT
        ;
LRWRITE(DFN,ARRAY,LABTYPE,FLAGS,ALERTS) ;"Store data in LAB DATA (^LR), file# 63
        ;"Input: DFN -- IEN in PATIENT file
        ;"       ARRAY -- PASS BY REFERENCE.  Format:
        ;"         ARRAY(<Field Name or Number>)=<Value>
        ;"         ARRAY(<Field Name or Number>,"XTRA")=<string to SET into global>
        ;"         ARRAY("FLAGS",FLD)=FLAG
        ;"       LABTYPE -- [Optional] Default is "CH"
        ;"               -- "CH" for Chem, Hem, Tox, RIA, Ser etc.
        ;"               -- "MI" for Microbiology labs  <-- NOT FULLY SUPPORTED YET
        ;"       FLAGS -- Flags to be passed to UPDATE^DIE when filing data
        ;"       ALERTS -- AN OUT PARAMETER.  PASS BY REFERENCE.  FORMAT:
        ;"             ALERTS("ALERT","DFN^FMDT^LEVEL^NODE")=""
        ;"NOTE: IF record already exists for specified DT, then data will be written
        ;"      into that record, rather than adding a NEW record.
        ;"Results: 1^File#:IENS,   or -1^Message  -- IF error
        ;
        NEW TMGRESULT SET TMGRESULT="1^OK"
        SET LABTYPE=$GET(LABTYPE,"CH")
        NEW SUBFILE
        IF LABTYPE="CH" SET SUBFILE=63.04
        ELSE  IF LABTYPE="MI" SET SUBFILE=63.05
        ELSE  DO  GOTO LRWDN
        . SET TMGRESULT="-1^Invalid LAB TYPE.  Expected 'CH' or 'MI'.  Got ["_LABTYPE_"]"
        NEW LRDFN SET LRDFN=+$PIECE($GET(^DPT(DFN,"LR")),"^",1) ;"Patient identifier for lab package.
        IF LRDFN'>0 SET LRDFN=$$NEWLRDFN(DFN,.TMGRESULT) ;"CREATE NEW LRDFN RECORD
        IF +TMGRESULT'>0 GOTO LRWDN
        NEW IENS SET IENS=LRDFN_","
        SET TMGRESULT=$$FILE1ARR(LRDFN,SUBFILE,IENS,.ARRAY,FLAGS) 
        IF +TMGRESULT'>0 GOTO LRWDN
        NEW DT SET DT=$GET(ARRAY(.01)) IF DT'>0 GOTO LRWDN
        NEW LEVEL SET LEVEL=1  ;"1->normal 2->abnormal, 3->Critical. 
        NEW AFLG SET AFLG="" FOR  SET AFLG=$ORDER(ARRAY("FLAGS",AFLG)) QUIT:(AFLG="")  DO  ;"//Note: equivalent mapping in CPRS is in TfrmLabs.GetVisibleLabs
        . NEW ALVL SET ALVL=0
        . IF (AFLG="H")!(AFLG="L") SET ALVL=2
        . IF (AFLG="HH")!(AFLG="LL")!(AFLG="C")!(AFLG["*") SET ALVL=3
        . IF ALVL>LEVEL SET LEVEL=ALVL
        SET ALERTS(DFN_"^"_DT_"^"_LEVEL_"^"_LABTYPE)=""
LRWDN   QUIT TMGRESULT        
        ;
 ;"FILESARR(DFN,FNUM,IENS,ARRAY,FLAGS,TMGRESULT) ;// DEPRECIATED ;"FILE SUB ENTRIES
 ;"        ;"Purpose: To take user input array and prepair for passing into Fileman
 ;"        ;"         -- returning only subfile entries. 
 ;"        ;"Input: DFN -- IEN in PATIENT file 
 ;"        ;"       FNUM -- the file/subfile number that contains fields
 ;"        ;"       IENS -- the IENS for record number containing fields
 ;"        ;"       ARRAY -- Array as passed into LRWRITE.  See docs there. 
 ;"        ;"       FLAGS -- Flags to be passed to UPDATE^DIE when filing data
 ;"        ;"       TMGRESULT -- PASS BY REFERENCE.  Altered to -1^<Message> IF error or problem
 ;"        NEW FIELD SET FIELD=""
 ;"        FOR  SET FIELD=$ORDER(ARRAY(FIELD)) QUIT:(FIELD="")!(+TMGRESULT'>0)  DO
 ;"        . NEW VALUE SET VALUE=$GET(ARRAY(FIELD))
 ;"        . IF VALUE'="SUBFILE" QUIT
 ;"        . NEW FLDNUM SET FLDNUM=+FIELD
 ;"        . IF FLDNUM'>0 DO  QUIT:(+TMGRESULT'>0)
 ;"        . . SET FLDNUM=$$GTNUMFLD^TMGDBAP3(FNUM,FIELD)
 ;"        . . IF FLDNUM'>0 DO  QUIT
 ;"        . . . SET TMGRESULT="-1^Unable to turn field ename '"_FIELD_"' info valid field number."
 ;"        . NEW SUBFNUM SET SUBFNUM=$$SUBFNUM(FNUM,FLDNUM) 
 ;"        . IF SUBFNUM=0 QUIT
 ;"        . NEW IDX SET IDX=0
 ;"        . FOR  SET IDX=$ORDER(ARRAY(FIELD,IDX)) QUIT:(IDX="")!(+TMGRESULT'>0)  DO
 ;"        . . NEW TEMPARR MERGE TEMPARR=ARRAY(FIELD,IDX)
 ;"        . . SET TMGRESULT=$$FILE1ARR(SUBFNUM,IENS,.TEMPARR,FLAGS)
 ;"        . . IF +TMGRESULT'>0 QUIT
 ;"PADN    QUIT        
 ;"        ;
FILE1ARR(LRDFN,FNUM,IENS,ARRAY,FLAGS) ;
        ;"Purpose: To take user input array and file with Fileman
        ;"Input: LRDFN -- Lab Identifier
        ;"       FNUM -- the file/subfile number that contains fields
        ;"       IENS -- IENS of record holding fields
        ;"       ARRAY -- PASS BY REFERENCE.  Format:
        ;"         ARRAY(<Field Name or Number>)=<Value>
        ;"         ARRAY(<Field Name or Number>,"XTRA")=<string to SET into global>
        ;"         ARRAY("FLAGS",FLD)=FLAG
        ;"       FLAGS -- Flags to be passed to UPDATE^DIE when filing data 
        ;"NOTE: IF record already exists for specified DT, then data will be written
        ;"      into that record, rather than adding a NEW record.
        ;"Result: 1^FileNum:IENS or -1^<Message> IF error or problem
        NEW TMGRESULT SET TMGRESULT=1
        NEW TMGFDA,TMGIEN,TMGMSG
        NEW FIELD SET FIELD=""
        NEW FLDD01 SET FLDD01=$GET(ARRAY(.01))
        NEW INVDT SET INVDT=9999999-FLDD01
        NEW NODE SET NODE=$SELECT(FNUM=63.04:"CH",FNAUM=63.05:"MI",1:"X")
        NEW SUBIEN SET SUBIEN=INVDT
        IF $DATA(^LR(+IENS,NODE,INVDT,0))=0 SET SUBIEN="+1"
        SET TMGRESULT=$$SETUPFDA(FNUM,SUBIEN,IENS,.ARRAY,.TMGFDA) 
        GOTO:(TMGRESULT'>0) FADN
        NEW TMGFDASAVE MERGE TMGFDASAVE=TMGFDA
        IF SUBIEN="+1" DO    ;"Create NEW record.                
        . DO UPDATE^DIE(FLAGS,"TMGFDA","TMGIEN","TMGMSG")
        . SET SUBIEN=+$GET(TMGIEN(1))
        ELSE  DO           ;"Add to EXISTING record
        . DO FILE^DIE(FLAGS,"TMGFDA","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO FADN  
        . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        ;"WRITE TO LABS INDEX X-REF ^PXRMINDX FOR REMINDER SYSTEM
        NEW TMGIEN SET TMGIEN=0
        FOR  SET TMGIEN=$ORDER(TMGFDASAVE(FNUM,TMGIEN)) QUIT:TMGIEN'>0  DO
        . NEW TMGFLD SET TMGFLD=.01
        . FOR  SET TMGFLD=$ORDER(TMGFDASAVE(FNUM,TMGIEN,TMGFLD)) QUIT:TMGFLD'>0  DO
        . . NEW NEWNODE SET NEWNODE=LRDFN_";"_NODE_";"_INVDT_";"_TMGFLD
        . . NEW IEN60 SET IEN60=$GET(ARRAY(TMGFLD,"XTRA",3,7))
        . . IF IEN60'>0 QUIT
        . . DO SLAB^LRPX(DFN,FLDD01,IEN60,NEWNODE)
        ;"-------------------------------------
        ;"IF +TMGRESULT>0 GOTO FAR2 ;"no problem
        ;"NEW PREEXIST SET PREEXIST="'"_SUBIEN_","_IENS_"' already exists"
        ;"IF TMGRESULT'[PREEXIST GOTO FADN
        ;"NOTE: Code block below, ending at FAR2, written before change to FILE^DIE made above.  
        ;"      Will leave in, but shouldn't be needed.
        ;"Since record already exists, try overwrite of existing record with current values
        ;"KILL TMGMSG,TMGFDA
        ;"SET TMGRESULT=$$SETUPFDA(FNUM,SUBIEN,IENS,.ARRAY,.TMGFDA) 
        ;"IF +TMGRESULT'>0 GOTO FADN
        ;"DO FILE^DIE(FLAGS,"TMGFDA","TMGMSG")
        ;"IF $DATA(TMGMSG("DIERR")) DO  GOTO FADN  
        ;". SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)        
        ;"-------------------------------------
FAR2    IF SUBIEN'>0 DO  GOTO FADN
        . SET TMGRESULT="-1^Unable to determine record number of newly added lab data"
        NEW SUBIENS SET SUBIENS=SUBIEN_","_IENS
        ;"DO FILESARR(FNUM,SUBIENS,.ARRAY,FLAGS,.TMGRESULT) ;"FILE SUB ENTRIES
        ;"Now handle non-fileman-compatible information that lab package uses (messy, but they SET it up this way)
        IF NODE'="CH" GOTO FAR3
        SET FIELD=0
        FOR  SET FIELD=$ORDER(ARRAY(FIELD)) QUIT:(+FIELD'>0)  DO
        . NEW HARDSTR SET HARDSTR=$GET(ARRAY(FIELD,"XTRA"))
        . IF HARDSTR="" QUIT
        . SET ^LR(+IENS,"CH",SUBIEN,FIELD)=HARDSTR  ;"NOTE, ONLY CHEM... FIELD SUPPORTED.               
FAR3    SET TMGRESULT="1^"_FNUM_":"_SUBIENS
FADN    QUIT TMGRESULT
        ;
SETUPFDA(FNUM,IEN,PARENTIENS,SRCARRAY,TMGFDA) ;
        ;
        NEW TMGRESULT SET TMGRESULT=1
        NEW TEMPIENS SET TEMPIENS=IEN_","_PARENTIENS
        NEW TMGIEN,TMGMSG KILL TMGFDA
        FOR  SET FIELD=$ORDER(SRCARRAY(FIELD)) QUIT:(+FIELD'>0)!(+TMGRESULT'>0)  DO
        . NEW VALUE SET VALUE=$GET(SRCARRAY(FIELD))
        . IF VALUE="SUBFILE" QUIT
        . NEW FLDNUM SET FLDNUM=+FIELD
        . IF FLDNUM'>0 DO  QUIT:(+TMGRESULT'>0)
        . . SET FLDNUM=$$GTNUMFLD^TMGDBAP3(FNUM,FIELD)
        . . IF FLDNUM'>0 DO  QUIT
        . . . SET TMGRESULT="-1^Unable to turn field ename '"_FIELD_"' info valid field number."
        . IF $$SUBFNUM(FNUM,FLDNUM)>0 QUIT
        . IF (FNUM=63.04)&(FLDNUM=".01")&(IEN'["+") QUIT  ;"avoid FM error regarding DINUM field.
        . SET TMGFDA(FNUM,TEMPIENS,FLDNUM)=VALUE      
        IF FNUM=63.04,$DATA(TMGFDA(FNUM,TEMPIENS,.05))=0 DO  
        . SET TMGRESULT="-1^Missing required .05 field (SPECIMEN TYPE) for 'CH' type entry."  
        . ;"NOTE: Fileman filing would succeed, but trigger CPRS error if missing
        QUIT TMGRESULT
        ;
NEWLRDFN(DFN,MSG) ;"Make NEW LAB DATA store record (IEN is LRDFN)
        ;"INPUT: DFN -- IEN in PATIENT file
        ;"       MSG -- PASS BY REFERENCE.  Altered to -1^<Message> IF error or problem
        ;"Results: returns LRDFN (IEN in lab data package)
        SET DFN=+$GET(DFN) IF DFN'>0 DO  GOTO NLRDDN
        . SET MSG="-1^Numeric DFN not provided."
        NEW LRDFN 
        SET LRDFN=+$GET(^DPT(DFN,"LR"))
        IF LRDFN>0 DO  GOTO NLRDDN
        . SET MSG="-1^Patient# "_DFN_" already has lab ID number."
        SET LRDFN=$ORDER(^LR("@"),-1)+1  ;"add NEW record right after last entry
        IF LRDFN'>0 SET LRDFN=1
        NEW TMGFDA,TMGIEN,TMGMSG
        SET TMGFDA(63,"+1,",.01)=LRDFN
        SET TMGFDA(63,"+1,",.02)=2  ;"2=PATIENT file
        SET TMGFDA(63,"+1,",.03)=DFN
        SET TMGIEN(1)=LRDFN
        DO UPDATE^DIE("S","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO NLRDDN
        . SET MSG="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        ;"NOTE: For SET command below, this is done because field 63's input  
        ;"      transform would have to be changed to allow Fileman to store value.
        SET ^DPT(DFN,"LR")=LRDFN 
        NEW DIK SET DIK="^DPT(",DIK(1)=63  ;"Fld 63 = LABORATORY REFERENCE field.
        NEW DA SET DA=DFN
        DO EN1^DIK  ;"Execute SET logic of XRef's for field 63 for this entry. 
NLRDDN  QUIT LRDFN
        ;
SUBFNUM(FILE,FIELD) ;
        ;"Purpose: Determine if field is a subfile
        ;"Result: 1 if multiple valued, or 0 if not.
        NEW INFO,TMGMSG
        NEW SUBFILE SET SUBFILE=+$PIECE($GET(^DD(FILE,FIELD,0)),"^",2)
        ;"DO FIELD^DID(FILE,FIELD,,"MULTIPLE-VALUED","INFO","TMGMSG")
        ;"IF $DATA(TMGMSG("DIERR"))'=0 DO
        ;". SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        ;"QUIT ($GET(INFO("MULTIPLE-VALUED"))=1)
        QUIT SUBFILE
        ;
PRIORLDT(TMGHL7MSG,DATESUSED,MSGINFO) ;"PRIOR LAB DATES
        ;"Purpose: Fill DATESUSED with dates already filed in database on same day
        ;"Input: TMGHL7MSG -- PASS BY REFERNCE.  Array as created by PARSMSG2^TMGHL7X2
        ;"       DATESUSED --PASS BY REFERENCE.  OUT PARAMETER. Array of other date/times already found.
        ;"          DATESUSED(FMDT)="" <-- was DATESUSED(FMDT_" ")=""
        ;"       MSGINFO.  PASS BY REFERENCE.  Array as created by GETINFO()
        ;"Results: none
        NEW LRDFN SET LRDFN=+$GET(MSGINFO("LRDFN"))
        NEW TEMPARR
        NEW OBRIDX SET OBRIDX=0
        FOR   SET OBRIDX=$ORDER(TMGHL7MSG("B","OBR",OBRIDX)) QUIT:(+OBRIDX'>0)!(+TMGRESULT<0)   DO
        . NEW DT SET DT=$GET(TMGHL7MSG(OBRIDX,7)) QUIT:DT=""
        . SET DT=$$HL72FMDT^TMGHL7U3(DT)
        . SET TEMPARR(DT\1)=""
        NEW DT SET DT=0
        FOR  SET DT=$ORDER(TEMPARR(DT)) QUIT:DT=""  DO
        . NEW RDT SET RDT=9999999-DT
        . NEW LABRDT SET LABRDT=RDT
        . FOR  SET LABRDT=$ORDER(^LR(LRDFN,"CH",LABRDT),-1) QUIT:(+LABRDT'>0)!(LABRDT<(RDT-1))  DO
        . . NEW FMDT SET FMDT=9999999-LABRDT
        . . ;"SET DATESUSED(FMDT_" ")=""
        . . SET DATESUSED(FMDT)=""
        . . NEW ALAB SET ALAB=0 FOR  SET ALAB=$ORDER(^LR(LRDFN,"CH",LABRDT,ALAB)) QUIT:+ALAB'>0  DO
        . . . ;"SET DATESUSED(FMDT_" ",ALAB)=""
        . . . SET DATESUSED(FMDT,ALAB)=""
        . . . ;"SET DATESUSED("B",ALAB,FMDT_" ")=""
        . . . SET DATESUSED("B",ALAB,FMDT)=""
        QUIT
        ;
DELPRIOR(FILEARR,DATESUSED,INFO)  ;"DELETE PRIOR FILINGS
        ;"Input: FILEARR - PASS BY REFERENCE.  Array of new filings to be made, as created by FILESUBM()
        ;"       DATESUSED --PASS BY REFERENCE.  Array of other date/times already found
        ;"             format: DATESUSED(FMDT)=""     <-- was DATESUSED(FMDT_" ")=""
        ;"                     DATESUSED(FMDT,LabNum)=""  <-- was DATESUSED(FMDT_" ",LabNum)=""
        ;"                     DATESUSED("B",LabNum,FMDT)=""  <-- was DATESUSED("B",LabNum,FMDT_" ")=""
        ;"       INFO -- Array containing information needed for filing. 
        ;"Result: 1^OK , or  -1^Message
        NEW TMGRESULT SET TMGRESULT="1^OK"
        NEW DT SET DT=+$GET(FILEARR(.01)) IF DT'>0 GOTO DLPDN
        FOR  QUIT:$LENGTH(DT)>11  SET DT=DT_"0"
        NEW SD1 SET SD1=$EXTRACT(DT,1,12)   ;"YYYMMDD.HHMM__
        SET SD1=$$LJ^XLFSTR(SD1,12,"0")  ;"add trailing 0's to achive 12 characters.        
        NEW LRDFN SET LRDFN=+$GET(INFO("LRDFN")) IF LRDFN'>0 GOTO DLPDN
        NEW FLD SET FLD=1
        FOR  SET FLD=$ORDER(FILEARR(FLD)) QUIT:(+FLD'>0)!(+TMGRESULT'>0)  DO
        . IF $DATA(DATESUSED("B",FLD))=0 QUIT
        . NEW OLDDT SET OLDDT=$ORDER(DATESUSED("B",FLD,"")) QUIT:OLDDT=""  ;"OLDDT here is cardinal number (no trailing 0's)
        . SET OLDDT=$$LJ^XLFSTR(OLDDT,12,"0")  ;"add trailing 0's to achive 12 characters.
        . IF $EXTRACT(OLDDT,1,12)'=SD1 QUIT
        . SET TMGRESULT=$$DELLAB(LRDFN,+OLDDT,FLD) QUIT:+TMGRESULT'>0
        . ;"KILL DATESUSED("B",FLD,OLDDT_" "),DATESUSED(OLDDT_" ",FLD)
        . KILL DATESUSED("B",FLD,OLDDT),DATESUSED(OLDDT,FLD)
DLPDN   QUIT TMGRESULT
        ;
DELLAB(LRDFN,DT,FLD)  ;"DELETE 1 LAB
        ;"IMPLEMENT
        NEW TMGFDA,TMGMSG,TMGRESULT SET TMGRESULT="1^OK"
        NEW RDT SET RDT=9999999-DT
        SET TMGFDA(63.04,RDT_","_LRDFN_",",FLD)="@"
        DO FILE^DIE("EK","TMGFDA","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO DLBDN
        . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        ;"Now kill all the field values that are non-standard Fileman 
        KILL ^LR(LRDFN,"CH",RDT,FLD) ;"originally put her by SET in FILE1ARR()
DLBDN   QUIT TMGRESULT
TEST ;
        NEW DFN SET DFN=70685  ;"PATIENT=TEST,KILLME
        NEW LABS
        SET LABS(.01)="NOW-1D"
        SET LABS(.02)=1
        SET LABS(.03)="NOW"
        SET LABS(.04)="`"_DUZ             
        SET LABS(.05)="SERUM"
        ;"SET LABS(.1)="`"_provider
        SET LABS(.112)="`71"  ;LAUGHLIN HOSPITAL
        ;"NOTE: I think the code above doesn't support SUBFILE anymore. 
        ;"SET LABS(.99)="SUBFILE"  ;"<--- COMMENT field
        ;"SET LABS(.99,1,.01)="AUTOFILE test"
        ;"SET LABS(.99,1,1)="`"_DUZ
        ;"SET LABS(.99,2,.01)="AUTOFILE test #2"
        ;"SET LABS(.99,2,1)="`"_DUZ
        SET LABS("GLUCOSE")=96
        SET LABS("SODIUM")=132
        SET LABS("POTASSIUM")=5.0
        NEW RESULT
        SET RESULT=$$LRWRITE(DFN,.LABS,"CH","E") 
        WRITE !,RESULT,!
        QUIT
