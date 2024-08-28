TMGTIUT6 ;TMG/kst-TIU-related code ; 5/7/16, 3/24/21; 8/8/24
   ;;1.0;TMG-LIB;**1**;5/7/16
  ;
  ;"
  ;"--This code is for parsing a TIU DOCUMENT and extracting the paragraph titles
  ;"  for determing the TOPICS discussed at a given visit.
  ;"--Results are stored in file 22719.
  ;"--There is a function here for parsing all documents at one time.  But the
  ;"  ultimate use is SET up such that a POST-SIGNATURE hook from TIU calls code
  ;"  here after a document is completed.
  ;
  ;"NOTE: This code was moved from TMGC0Q04 on 8/8/24.
  ;
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;"Copyright (c) 8/8/24  Kevin S. Toppenberg MD
  ;"
  ;"This file is part of the TMG LIBRARY, and may only be used in accordence
  ;" to license terms outlined in separate file TMGLICNS.m, which should 
  ;" always be distributed with this file.
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;
  ;"=======================================================================
  ;" RPC -- Public Functions. 
  ;"=======================================================================  
  ;"GETTOPICS(OUT,IEN8925)  -- Retrieve topics with added info from file 22719 that corresponds to IEN8925
  ;"ADDLSIGN(TIUIEN)  -- TRIGGER HANDLER.  Add an addl signer when needed  
  ;"SUMMALL -- process all TIU documents and summarize each one
  ;"ASKSUMM1 --Ask user for 1 document, and summarize it.  
  ;"SUMM1(TIUIEN,OPTION)  Summarize 1 TIU document, with provided options
  ;"TRIGALL --process all TIU documents and trigger each one
  ;"ASKTRIG -- Ask for TIU IEN and TRIGGER for that one document.
  ;"TEST1  -- Ask for TIU IEN and SUMM that one document.
  ;"  
  ;"=======================================================================
  ;"PRIVATE API FUNCTIONS
  ;"=======================================================================
  ;"CHK6CIT(TIUIEN)  -- CHECK THE NOTE FOR 6CIT TEST
  ;"ADD6CIT  ;"
  ;"CHECKDT(TIUIEN) --DEPRECIATED
  ;
  ;"=======================================================================
  ;"DEPENDENCIES: 
  ;"=======================================================================
  ;
ASKTRIG ;"Ask for TIU IEN and TRIGGER that one document.
  NEW DIC,X,Y
  SET DIC=8925,DIC(0)="MAEQ"
  DO ^DIC WRITE !
  QUIT:+Y'>0
  DO HNDLFRGND^TMGTIUT5(+Y)
  QUIT
  ;
TRIGALL ;
  ;"Purpose: process all TIU documents and trigger each one
  WRITE "==============================================",!
  WRITE "Office visit parser",!
  WRITE "..............................................",!
  WRITE "Will scan every TIU DOCUMENT and summarize",!
  WRITE "paragraph titles found in HPI and A&P sections.",!
  WRITE "Press ESC (perhaps multiple times) to escape loop.",!
  WRITE "==============================================",!,!
  NEW TMGOVTITLES,ARRAY
  NEW STIME SET STIME=$H
  NEW DONE SET DONE=0       
  NEW TIUMAX SET TIUMAX=+$ORDER(^TIU(8925,"!"),-1)
  NEW TIUIEN SET TIUIEN=0
  FOR  SET TIUIEN=$ORDER(^TIU(8925,TIUIEN)) QUIT:(+TIUIEN'>0)!DONE  DO
  . IF TIUIEN#5=0 DO
  . . DO PROGBAR^TMGUSRI2(TIUIEN,"PROGRESS: "_TIUIEN,0,TIUMAX,60,STIME)
  . . IF $$USRABORT^TMGUSRI2("parsing notes") SET DONE=1
  . DO HNDLFRGND^TMGTIUT5(TIUIEN,1)
  DO PROGBAR^TMGUSRI2(TIUIEN,"Done.",TIUMAX,TIUMAX,60,STIME)
  QUIT
  ;
  ;"SUMNOTE(TIUIEN,ARRAY) ;"Purpose: To take a given note in file 8925, and parse HPI and A&P into array
  ;"        DO SUMNOTE^TMGTIUP1(.TIUIEN,.ARRAY)
  ;"        QUIT
  ;"        ;  
CHK6CIT(TIUIEN)  ;"CHECK THE NOTE FOR 6CIT TEST
  ;"IF FOUND, GET THE RESULT AND SAVE A HF WITH THE
  ;"RESULT AS A COMMENT
  NEW IDX SET IDX=0
  NEW FOUND SET FOUND=0
  NEW SCORE SET SCORE=-1
  NEW TMGRESULT SET TMGRESULT=""
  FOR  SET IDX=$O(^TIU(8925,TIUIEN,"TEXT",IDX)) QUIT:(IDX'>0)!(SCORE'=-1)  DO
  . NEW LINE SET LINE=$G(^TIU(8925,TIUIEN,"TEXT",IDX,0))
  . SET LINE=LINE_$G(^TIU(8925,TIUIEN,"TEXT",IDX+1,0)) ;"GET CURRENT + NEXT LINE, IN CASE OF TRUNCATION   4/15/21
  . IF LINE["6CIT - Kingshill Version 2000" SET FOUND=1
  . IF FOUND=1 DO
  . . IF LINE["6CIT score = " DO
  . . . SET SCORE=$P(LINE,"/",1)
  . . . SET SCORE=+$P(SCORE,"6CIT score = ",2)
  IF SCORE'=-1 DO
  . ;"HERE WE ARE GOING TO ADD THE HF
  . ;"WRITE "    **ADDING 6CIT HF",!  ;"REMOVE THIS AFTERWARD
  . NEW TIUDATE,PROVIEN,PROVNAME,TMGDFN
  . SET TMGDFN=$P($G(^TIU(8925,TIUIEN,0)),"^",2)
  . SET PROVIEN=$P($G(^TIU(8925,TIUIEN,12)),"^",2)
  . SET TIUDATE=$P($G(^TIU(8925,TIUIEN,0)),"^",7)
  . SET PROVNAME=$P($G(^VA(200,PROVIEN,0)),"^",1)
  . NEW ARRDATA	
  . SET ARRDATA(1)="HDR^0^^6;"_TIUDATE_";A"
  . SET ARRDATA(2)="VST^DT^"_TIUDATE
  . SET ARRDATA(3)="VST^PT^"_TMGDFN
  . SET ARRDATA(4)="VST^HL^6"
  . SET ARRDATA(5)="VST^VC^A"
  . SET ARRDATA(6)="PRV^"_PROVIEN_"^^^"_PROVNAME_"^1"
  . SET ARRDATA(7)="HF+^2703^^TMG 6CIT DONE^@^^^^^1^"
  . SET ARRDATA(8)="COM^1^"_SCORE
  . DO SAVE^ORWPCE(.TMGRESULT,.ARRDATA,0,6)
  QUIT
  ;
ADD6CIT  ;"Check CHK6CIT for ALL documents.  
  NEW TIUIEN SET TIUIEN=0
  FOR  SET TIUIEN=$O(^TIU(8925,TIUIEN)) QUIT:+TIUIEN'>0  DO
  . WRITE "CHECKING TIUIEN: ",TIUIEN,!
  . DO CHK6CIT(TIUIEN)
  QUIT
  ;  
  ;"=============================================================
GETTOPICS(OUT,IEN8925)  ;"Retrieve topics with added info from file 22719 that corresponds to IEN8925
  ;"Input: OUT --  OUT PARAMETER.  See format below
  ;"       IEN8925 -- TIU DOCUMENT 8925 to retrieve extracted info for
  ;"Result: none, but OUT may be filled.  Format.  (designed to work with GETHPI^TMGTIUP2)
  ;"      NOTE: OUT may be returned empty if nothing found.  
  ;"      OUT("DATE")=FMDT date
  ;"      OUT("TOPIC",<topic name>)=<text of topic>
  SET IEN8925=+$GET(IEN8925)
  IF $DATA(^TMG(22719,IEN8925))'>0 GOTO GTDN   ;"NOTE: IENs in 22719 are 1:1 with file 8925, i.e. DINUMd
  IF $DATA(^TIU(8925,IEN8925))'>0 GOTO GTDN    ;"Ensure note still exists.  
  IF $$GET1^DIQ(8925,IEN8925,.05)'="COMPLETED" GOTO GTDN  ;"Ensure not retracted, or unsigned etc.  
  NEW ADT SET ADT=$$GET1^DIQ(22719,IEN8925,.03,"I")
  NEW EXTDT SET EXTDT=$$FMTE^XLFDT(ADT,"2D")
  NEW SUBIEN SET SUBIEN=0
  FOR  SET SUBIEN=$ORDER(^TMG(22719,IEN8925,2,SUBIEN)) QUIT:SUBIEN'>0  DO
  . NEW ZN SET ZN=$GET(^TMG(22719,IEN8925,2,SUBIEN,0))
  . NEW TOPICNAME SET TOPICNAME=$PIECE(ZN,"^",1)
  . NEW WP MERGE WP=^TMG(22719,IEN8925,2,SUBIEN,2)
  . NEW TEXT SET TEXT=""
  . NEW IDX SET IDX=0 
  . FOR  SET IDX=$ORDER(WP(IDX)) QUIT:IDX'>0  DO
  . . NEW LINE SET LINE=$GET(WP(IDX,0)) QUIT:LINE=""
  . . IF TEXT'="",$EXTRACT(TEXT,$LENGTH(TEXT))'=" " SET TEXT=TEXT_" "
  . . SET TEXT=TEXT_LINE
  . SET OUT("TOPIC",TOPICNAME)=EXTDT_": "_TEXT
GTDN ;
  QUIT
  ;
TESTGT ;
  NEW OUT DO GETTOPICS(.OUT,800352) ZWR OUT
  QUIT
  ;
  ;"=============================================================
SUMMALL ;
  ;"Purpose: process all TIU documents and summarize each one
  WRITE "==============================================",!
  WRITE "Office visit parser",!
  WRITE "..............................................",!
  WRITE "Will scan every TIU DOCUMENT and summarize",!
  WRITE "paragraph titles found in HPI sections.",!
  WRITE "Press ESC (perhaps multiple times) to escape loop.",!
  WRITE "==============================================",!,!
  NEW TMGOVTITLES,ARRAY
  NEW STIME SET STIME=$H
  NEW DONE SET DONE=0
  NEW OPTION
  SET OPTION("SKIP AUTOADD","PREVENTION")=1
  SET OPTION("SKIP AUTOADD","SOCIAL")=1
  SET OPTION("SKIP AUTOADD","CONTRACEPTION")=1
  SET OPTION("THREADS")=1
  SET OPTION("SKIP REWRITE")=1   
  NEW TIUMAX SET TIUMAX=+$ORDER(^TIU(8925,"!"),-1)
  NEW TIUIEN SET TIUIEN=0
  SET TIUIEN=+$GET(^TMG("TMP","SUMMALL^TMGTIUT6"))  ;"temp, remove later....
  FOR  SET TIUIEN=$ORDER(^TIU(8925,TIUIEN)) QUIT:(+TIUIEN'>0)!DONE  DO
  . IF $DATA(^TIU(8925,TIUIEN,0))=0 DO  QUIT
  . . WRITE !,"MADNESS!  ",TIUIEN,!
  . IF TIUIEN#5=0 DO
  . . DO PROGBAR^TMGUSRI2(TIUIEN,"PROGRESS: "_TIUIEN,0,TIUMAX,60,STIME)
  . . IF $$USRABORT^TMGUSRI2("parsing notes") SET DONE=1
  . DO SUMM1(TIUIEN,.OPTION)
  . SET ^TMG("TMP","SUMMALL^TMGTIUT6")=TIUIEN
  DO PROGBAR^TMGUSRI2(TIUIEN,"Done.",TIUMAX,TIUMAX,60,STIME)
  QUIT
  ;
ASKSUMM1 ;"Ask user for 1 document, and summarize it.  
  NEW OPTION
  SET OPTION("SKIP AUTOADD","PREVENTION")=1
  SET OPTION("SKIP AUTOADD","SOCIAL")=1
  SET OPTION("SKIP AUTOADD","CONTRACEPTION")=1
  SET OPTION("THREADS")=1
  NEW DIC,X,Y
  SET DIC=8925,DIC(0)="MAEQ"
  DO ^DIC WRITE !
  QUIT:+Y'>0
  DO SUMM1(+Y,.OPTION)
  QUIT
  ;
SUMM1(TIUIEN,OPTION)  ;"Summarize 1 TIU document, with provided options
  NEW ARRAY,PROVIEN
  NEW ONLYOV SET ONLYOV=1
  SET PROVIEN=+$PIECE($GET(^TIU(8925,TIUIEN,12)),"^",2)
  IF (ONLYOV=1),($$ISOFFVST^TMGC0QT1(TIUIEN)=0) GOTO SUMM1DN
  IF (PROVIEN'=168)&(PROVIEN'=83) GOTO SUMM1DN  ;"<-- HARD CODED PROVIDER IEN'S
  DO SUMNOTE^TMGTIUP1(TIUIEN,.ARRAY,.OPTION)
  DO FILE1^TMGTIUT5(TIUIEN,"ARRAY",.QUIET,.OPTION)  
  DO FILE1B^TMGTIUT5(TIUIEN,.ARRAY,.QUIET,.OPTION)  
SUMM1DN ;
  QUIT
  ;  
TEST1  ;"Ask for TIU IEN and SUMM that one document.
  NEW DIC,X,Y
  SET DIC=8925,DIC(0)="MAEQ"
  DO ^DIC WRITE !
  QUIT:+Y'>0
  DO SUMNOTE^TMGTIUP1(+Y,.ARRAY)
  KILL ARRAY(+Y,"FULL")
  ZWR ARRAY
  QUIT
  ;  
