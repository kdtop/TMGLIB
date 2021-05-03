TMGRX007 ;TMG/kst/Patient medication listing code; 05/16/18, 3/24/21
       ;;1.0;TMG-LIB;**1**;05/04/18
 ;
 ;"Code for dealing with saving patients medication list
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 05/04/18  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"CONSOLE 
 ;"PICKSCAN -- PICK PATIENT, THEN SCAN THEM
 ;"SCANALL  -- SCAN ALL PATIENTS...
 ;"SCANPT(TMGDFN) -- SCAN 1 PATIENT  
 ;"PICKVIEW  -- PICK PATIENT, THEN VIEW RECORD
 ;"VIEW1(TMGDFN) -- VIEW RECORD FOR 1 PATIENT
 ;"HANDLTIU(IEN8925)  -- Evaluate a TIU DOCUMENT for medication table, and save if found
 ;"SAVETABL(TMGDFN,ARR,OPTION)  -- SAVE MEDICATION TABLE.
 ;"SCANDOC(IEN8925,ARR)  -- Scan document for MEDICATION TABLE or FINAL MEDICATION TABLE, extract it if found 
 ;"GETRXTBL(OUTARR,OUTDT,TMGDFN)  --GET MED TABLE FOR PATIENT (doesn't include title name)
 ;" 
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"=======================================================================
 ;"Uses:  
 ;"=======================================================================
 ; 
CONSOLE ;
  NEW MENU,IDX
CONL1 ;                    
  SET IDX=0  
  KILL MENU SET MENU(IDX)="Select Option For Managing Patient Medication Tables"
  SET IDX=IDX+1,MENU(IDX)="Pick patient to rescan"_$CHAR(9)_"RESCAN_1"
  ;"SET IDX=IDX+1,MENU(IDX)="Pick patient to rescan, using MEDAARR^TMGTIUOJ"_$CHAR(9)_"RESCAN_2"
  ;"SET IDX=IDX+1,MENU(IDX)="Pick patient to rescan, COMPARING two methods"_$CHAR(9)_"RESCAN_3"
  SET IDX=IDX+1,MENU(IDX)="Rescan ALL patients"_$CHAR(9)_"RESCAN_ALL"
  ;"SET IDX=IDX+1,MENU(IDX)="Rescan ALL patients, using MEDAARR^TMGTIUOJ"_$CHAR(9)_"RESCAN_ALL2"
  SET IDX=IDX+1,MENU(IDX)="View one patient's record"_$CHAR(9)_"VIEW1"  
  SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
  IF USRPICK="^" GOTO CONDN  
  IF USRPICK="RESCAN_1" DO  GOTO CONL1
  . DO PICKSCAN(1)
  ;"IF USRPICK="RESCAN_2" DO  GOTO CONL1
  ;". DO PICKSCAN(2)
  ;"IF USRPICK="RESCAN_3" DO  GOTO CONL1
  ;". DO PICKSCAN(3)
  IF USRPICK["RESCAN_ALL" DO  GOTO CONL1
  . WRITE !,"NOTE: Should NOT be done while others might be using info",!
  . WRITE "RESCAN ALL PATIENTS, DELETING PRIOR INFO (Y/N)"
  . DO YN^DICN WRITE !,!
  . IF %'=1 QUIT
  . IF USRPICK="RESCAN_ALL" DO SCANALL(0)
  . ;"IF USRPICK="RESCAN_ALL2" DO SCANALL(1)
  IF USRPICK="VIEW1" DO  GOTO CONL1
  . DO PICKVIEW   
  GOTO CONL1
CONDN  ;
  QUIT
  ;
PICKSCAN(MODE) ;"PICK PATIENT, THEN SCAN THEM
  NEW DIC,X,Y,TMGDFN
  SET DIC=2,DIC(0)="MAEQ" DO ^DIC WRITE ! SET TMGDFN=+Y  
  IF TMGDFN>0 DO
  . IF MODE=1 DO SCANPT(TMGDFN)
  . ;"IF MODE=2 DO SCANPT2(TMGDFN) ;"<--- depreciated
  . ;"IF MODE=3 DO SCANPT3(TMGDFN) ;"<--- depreciated
  WRITE !,"RESCAN COMPLETE.",!
  DO PRESS2GO^TMGUSRI2
  QUIT
  ;
SCANALL(MODE)  ;"SCAN ALL PATIENTS...
  NEW TMGDFN SET TMGDFN=0
  NEW MAXDFN SET MAXDFN=$ORDER(^DPT("@"),-1)
  NEW STARTH SET STARTH=$H
  SET MODE=+$GET(MODE)
  FOR  SET TMGDFN=$ORDER(^DPT(TMGDFN)) QUIT:TMGDFN'>0  DO
  . IF TMGDFN#10=0 DO PROGBAR^TMGUSRI2(TMGDFN,"DFN: "_TMGDFN,1,MAXDFN,60,STARTH)
  . IF MODE=0 DO SCANPT(TMGDFN)    
  . ;"IF MODE=1 DO SCANPT2(TMGDFN)  ;"<--- depreciated
  QUIT
  ;
SCANPT(TMGDFN) ;"SCAN 1 PATIENT  -- This method is same as used by POST-SIGNATURE code
  ;"WRITE !,"This scans all patients, and gets last note with table information.",!
  NEW TABLENAME SET TABLENAME="[FINAL MEDICATIONS]"
  NEW NOTES IF $$LASTNOTE^TMGTIUO5(TMGDFN,.NOTES,TABLENAME)=0 QUIT ;"GET LAST NOTE POINTED TO IN 22729
  NEW DT SET DT=$ORDER(NOTES(""),-1) IF DT'>0 QUIT
  NEW IEN8925 SET IEN8925=$ORDER(NOTES(DT,0)) IF IEN8925'>0 QUIT  
  NEW TEMP SET TEMP=$$HANDLTIU(IEN8925)
  IF TEMP'>0 WRITE !,$PIECE(TEMP,"^",2),!
  QUIT
  ;  
  ;"SCANPT2(TMGDFN) ;"SCAN 1 PATIENT --  This method just gets patients MED ARRAY  <--- depreciated
  ;"  ;"WRITE !,"This scans all patients, assembling med table from one or more notes.",!
  ;"  SET OPTION("USEOLDMETHOD")=1  ;"<-- needed to ensure everything pulled together to current date
  ;"  NEW MEDARR DO MEDARR^TMGTIUOJ(0,TMGDFN,.MEDARR,,.OPTION)
  ;"  KILL MEDARR(0)
  ;"  IF $DATA(MEDARR)=0 QUIT
  ;"  NEW TMGRESULT SET TMGRESULT=$$SAVETABL(TMGDFN,.MEDARR)  ;"SAVE MEDICATION TABLE.
  ;"  IF TMGRESULT'>0 WRITE !,!,"ERROR: ",$PIECE(TMGRESULT,"^",2),!
  ;"  QUIT
  ;                                                                  
  ;"SCANPT3(TMGDFN) ;"SCAN 1 PATIENT with 2 methods, and comparing, NO SAVING  <--- depreciated
  ;"  NEW TABLENAME SET TABLENAME="[FINAL MEDICATIONS]"
  ;"  NEW NOTES IF $$LASTNOTE^TMGTIUO5(TMGDFN,.NOTES,TABLENAME)=0 QUIT ;"GET LAST NOTE POINTED TO IN 22729
  ;"  NEW DT SET DT=$ORDER(NOTES(""),-1) IF DT'>0 DO  QUIT
  ;"  . WRITE !,"Unable to find a note to scan",! 
  ;"  NEW IEN8925 SET IEN8925=$ORDER(NOTES(DT,0)) 
  ;"  IF IEN8925'>0 DO  QUIT
  ;"  . WRITE !,"Unable to find a note to scan",! 
  ;"  NEW ARR1 IF $$SCANDOC(IEN8925,.ARR1)  ;"ignore result
  ;"  SET OPTION("USEOLDMETHOD")=1  ;"<-- needed to ensure everything pulled together to current date
  ;"  NEW ARR2 DO MEDARR^TMGTIUOJ(0,TMGDFN,.ARR2,,.OPTION)
  ;"  NEW RESIDUAL1,RESIDUAL2,COMMON
  ;"  DO COMMON^TMGMISC3(.ARR1,.ARR2,.RESIDUAL1,.RESIDUAL2,.COMMON)
  ;"  QUIT
  ;  
PICKVIEW  ;"PICK PATIENT, THEN VIEW RECORD
  NEW DIC,X,Y,TMGDFN
  SET DIC=2,DIC(0)="MAEQ" DO ^DIC WRITE ! SET TMGDFN=+Y  
  IF TMGDFN>0 DO VIEW1(TMGDFN)
  QUIT
  ;
VIEW1(TMGDFN) ;"VIEW RECORD FOR 1 PATIENT
  NEW OPTION SET OPTION("NO LOOP")=1
  DO ASKDUMP^TMGDEBU3(22733.2,TMGDFN,.OPTION)
  QUIT
  ;
HANDLTIU(IEN8925)  ;"Evaluate a TIU DOCUMENT for medication table, and save if found
  ;"This is called by a POST-SIGNATURE trigger <-- TRIGJOB^TMGTIUT5 <-- TRIG1^TMGTIUT5  
  ;"Note: if both a MEDICATION TABLE and a  FINAL MEDICATION TABLE is found, the final one will be saved
  ;"INPUT -- IEN8925 -- IEN in file 8925, TIU DOCUMENT. May not contain med list
  ;"RESULT: 1^OK, o r-1^Error message if problem. 
  NEW TMGRESULT SET TMGRESULT="1^OK"
  NEW TMGDFN SET TMGDFN=$PIECE($GET(^TIU(8925,IEN8925,0)),"^",2) GOTO:TMGDFN'>0 HANDLDN
  NEW ARR IF $$SCANDOC(IEN8925,.ARR)=0 GOTO HANDLDN
  SET TMGRESULT=$$SAVETABL(TMGDFN,.ARR)  ;"SAVE MEDICATION TABLE. 
HANDLDN ;
  QUIT TMGRESULT
  ;
SAVETABL(TMGDFN,ARR,OPTION)  ;"SAVE MEDICATION TABLE.
  ;"INPUT: TMGDFN  -- Patient IEN
  ;"       ARR  -- Input.  Format:
  ;"          ARR(#)=<line from table>    <--- this is every line from medication table
  ;"                  NOTE: typically does NOT include title and '*' lines 
  ;"          ARR("KEY-VALUE",<KEY>)=<VALUE>  <-- KEY and VALUE are separated by "=", <KEY> is UPPERCASE
  ;"          ARR("KEY-VALUE",<KEY>,"LINE")=<original line from medication table)
  ;"       OPTION -- Optional.  
  ;"          OPTION("SAVE RX PARSE")=1 (or 0)  DEFAULT IS 0  if 1, raw parsing info is saved.  
  ;"RESULT: 1^OK, or -1^message if error
  NEW TMGRESULT SET TMGRESULT="1^OK"
  NEW SAVEPARSE SET SAVEPARSE=$GET(OPTION("SAVE RX PARSE"),0)
  IF $DATA(^TMG(22733.2,TMGDFN))'>0 DO  GOTO:(TMGRESULT'>0) SAVTBLDN
  . NEW TMGFDA,TMGIEN,TMGMSG
  . SET TMGIEN(1)=TMGDFN
  . SET TMGFDA(22733.2,"+1,",.01)=TMGDFN
  . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  . IF $DATA(TMGMSG("DIERR")) DO  QUIT
  . . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  ;
  NEW REF SET REF=$NAME(^TMG(22733.2,TMGDFN,"TABLE"))
  NEW KEYVAL 
  ;"WHEN ERX SCRIPTS ARE BEING SAVED, THE KEY-VALUE ISN'T POPULATED. 
  ;"WE WILL CHECK TO ENSURE THAT WE ONLY KILL OFF KV AND RESET IF
  ;"IF IT IS FOUND  3/1/19
  IF $D(ARR("KEY-VALUE")) DO
  . MERGE KEYVAL=ARR("KEY-VALUE") 
  . KILL ARR("KEY-VALUE")  
  DO ARRAY2WP^TMGSTUT2("ARR",REF)  ;" <-- saves to field 2, TABLE RAW
  ;
  DO
  . NEW TMGFDA,TMGMSG
  . SET TMGFDA(22733.2,TMGDFN_",",.02)=$$NOW^XLFDT
  . DO FILE^DIE("","TMGFDA","TMGMSG")
  . IF $DATA(TMGMSG("DIERR")) DO  QUIT
  . . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  IF TMGRESULT'>0 GOTO SAVTBLDN
  ;
  ;"-- Now add entries for meds. 
  NEW DIK SET DIK="^TMG(22733.2,"_TMGDFN_",""RX"","
  NEW DA SET DA=0,DA(1)=TMGDFN
  FOR  SET DA=$ORDER(^TMG(22733.2,TMGDFN,"RX",DA)) QUIT:DA'>0  DO ^DIK  ;"kill prior entries. 
  NEW IDX SET IDX=0   
  FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:(IDX'>0)!(TMGRESULT'>0)  DO  
  . NEW LINE SET LINE=$$TRIM^XLFSTR($GET(ARR(IDX)))
  . ;"ADD LINE TO MEDICATIONS SUBFILE.  
  . IF LINE["[OLD ENTRY]" QUIT
  . IF LINE["[MEDICATION" QUIT
  . IF LINE["[FINAL MEDICATION" QUIT
  . IF $EXTRACT(LINE,1)="*" QUIT
  . IF LINE["^" SET LINE=$TRANSLATE("^","~")  
  . NEW RXINFO DO PARSELN^TMGRX001(.RXINFO,LINE) 
  . ;"SAVE INFO TO SUBFILE FIELD 1 (PARSING RAW INFO)
  . NEW TMGFDA,TMGIEN,TMGMSG,TMGIENS
  . SET TMGIENS="+1,"_TMGDFN_","
  . SET TMGFDA(22733.21,TMGIENS,.01)=+$GET(RXINFO("IEN22733"))
  . SET TMGFDA(22733.21,TMGIENS,.02)=LINE
  . NEW STRENGTH SET STRENGTH=$GET(RXINFO("STRENGTH"))
  . IF STRENGTH'="" SET TMGFDA(22733.21,TMGIENS,.03)=STRENGTH
  . NEW CLASS SET CLASS=+$ORDER(RXINFO("DRUG CLASS",""))
  . IF CLASS>0 SET TMGFDA(22733.21,TMGIENS,.04)=CLASS
  . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  . IF $DATA(TMGMSG("DIERR")) DO  QUIT
  . . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  . IF SAVEPARSE DO
  . . NEW SUBIEN SET SUBIEN=$GET(TMGIEN(1))
  . . IF SUBIEN'>0 DO  QUIT
  . . . SET TMGRESULT="-1^Unable to locate added record"
  . . NEW TEXT DO ZWR2ARR^TMGZWR("RXINFO","TEXT") ;"//kt added this function
  . . SET REF=$NAME(^TMG(22733.2,TMGDFN,"RX",SUBIEN,"PARSED"))
  . . DO ARRAY2WP^TMGSTUT2("TEXT",REF)
  ;
  ;"-- Now store KEY-VALUE pairs
  IF '$D(KEYVAL) GOTO SAVTBLDN   ;"DON'T RESAVE IS KVs WERE SENT  3/1/19
  NEW DIK SET DIK="^TMG(22733.2,"_TMGDFN_",""KV"","
  NEW DA SET DA=0,DA(1)=TMGDFN
  FOR  SET DA=$ORDER(^TMG(22733.2,TMGDFN,"KV",DA)) QUIT:DA'>0  DO ^DIK  ;"kill prior entries. 
  NEW KEY SET KEY=""   
  FOR  SET KEY=$ORDER(KEYVAL(KEY)) QUIT:(KEY="")!(TMGRESULT'>0)  DO
  . NEW VALUE SET VALUE=$$TRIM^XLFSTR($GET(KEYVAL(KEY)))  
  . NEW LINE SET LINE=$$TRIM^XLFSTR($GET(KEYVAL(KEY,"LINE")))
  . IF VALUE["^" SET VALUE=$TRANSLATE("^","~")  
  . IF LINE["^" SET LINE=$TRANSLATE("^","~")  
  . NEW TMGFDA,TMGIEN,TMGMSG,TMGIENS
  . SET TMGIENS="+1,"_TMGDFN_","
  . SET TMGFDA(22733.33,TMGIENS,.01)=KEY
  . SET TMGFDA(22733.33,TMGIENS,.02)=VALUE
  . SET TMGFDA(22733.33,TMGIENS,1)=LINE
  . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  . IF $DATA(TMGMSG("DIERR")) DO  QUIT
  . . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  ;
SAVTBLDN  
  QUIT TMGRESULT
  ;
SCANDOC(IEN8925,ARR)  ;"Scan document for MEDICATION TABLE or FINAL MEDICATION TABLE, extract it if found 
  KILL ARR
  NEW TABLENAME,TEMP
  NEW TMGDFN SET TMGDFN=$PIECE($GET(^TIU(8925,IEN8925,0)),"^",2) GOTO:TMGDFN'>0 SCDCDN
  NEW FOUNDTABLE SET FOUNDTABLE=""
  FOR TABLENAME="[FINAL MEDICATIONS]","[MEDICATIONS]" DO  QUIT:$DATA(TEMP)  
  . DO XTRCTSPC^TMGTIUO5(IEN8925,TABLENAME,"BLANK_LINE",.TEMP)
  . IF $DATA(TEMP) SET FOUNDTABLE=TABLENAME    
  NEW ERXARR DO ERX1TIU^TMGTIUO5(IEN8925,.ERXARR)
  IF $DATA(TEMP)=0,+$GET(ERXARR(0))>0 DO
  . DO GETRXTBL^TMGRX007(.TEMP,.DT,TMGDFN)  ;"doesn't return table name.
  . SET FOUNDTABLE="[MEDICATIONS]"
  IF $DATA(TEMP),+$G(ERXARR(0))>0 DO
  . NEW CT SET CT=+$ORDER(TEMP(9999),-1) ;"Changed from TEMP("") which returned 0 elh  3/1/19
  . NEW IDX SET IDX=0
  . FOR  SET IDX=$ORDER(ERXARR(IDX)) QUIT:IDX'>0  DO
  . . NEW LINE SET LINE=$GET(ERXARR(IDX)) QUIT:LINE=""
  . . SET CT=CT+1,TEMP(CT)=LINE
  IF $DATA(TEMP)=0 GOTO SCDCDN
  DO SORTMEDS^TMGTIUO5(.TEMP) 
  NEW IDX,CT SET (IDX,CT)=0
  FOR  SET IDX=$ORDER(TEMP(IDX)) QUIT:IDX'>0  DO
  . NEW LINE SET LINE=$$TRIM^XLFSTR($GET(TEMP(IDX))) QUIT:LINE=""
  . SET LINE=$$FIXABREV^TMGTIUO6(LINE)  ;"Fix any abbreviations.  
  . SET CT=CT+1,ARR(CT)=LINE
  MERGE ARR("KEY-VALUE")=TEMP("KEY-VALUE")  ;"12/13/18 the above loop doesn't include these values
  DO SORTMARR^TMGTIUO5(.ARR) ;"splits to KEY-VALUE pairs etc.  
SCDCDN ;  
  QUIT ($DATA(ARR)>0)
  ;
GETRXTBL(OUTARR,OUTDT,TMGDFN)  ;"GET MED TABLE FOR PATIENT from file 22733.2.  
  ;"INPUT:  OUTARR -- PASS BY REFERENCE. AN OUT PARAMETER.  Format:
  ;"          OUTARR(#)=<entry lines from table>
  ;"          OUTARR("KEY-VALUE",<KEY>)=<VALUE>  <-- KEY and VALUE are separated by "=", <KEY> is UPPERCASE
  ;"          OUTARR("KEY-VALUE",<KEY>,"LINE")=<original line from medication table)
  ;"          OUTARR("KEY-VALUE","SOURCE-DATE")=FMDT  (same as OUTDT)  
  ;"        OUTDT -- PASS BY REFERENCE. AN OUT PARAMETER.  Date of source of med list
  ;"        TMGDFN -- patient IEN
  ;"NOTE: Doesn't include title name
  ;"Result: none
  SET TMGDFN=+$GET(TMGDFN)
  SET OUTDT=$PIECE($GET(^TMG(22733.2,TMGDFN,0)),"^",2)
  IF $ORDER(^TMG(22733.2,TMGDFN,"KV",0))'>0 DO
  . DO SCANPT^TMGRX007(TMGDFN)  ;"//kt 10/16/18 For some reason, sometimes this is missing, so recreate 
  KILL OUTARR
  SET OUTARR("KEY-VALUE","SOURCE-DATE")=OUTDT
  NEW CT SET CT=0
  NEW SUBIEN SET SUBIEN=0
  FOR  SET SUBIEN=$ORDER(^TMG(22733.2,TMGDFN,"TABLE",SUBIEN)) QUIT:SUBIEN'>0  DO
  . NEW LINE SET LINE=$GET(^TMG(22733.2,TMGDFN,"TABLE",SUBIEN,0))
  . IF LINE["[FINAL MEDICATION" QUIT
  . IF LINE["[MEDICATION" QUIT
  . SET CT=CT+1,OUTARR(CT)=LINE
  ;"GET KEY-VALUES
  ;"Previously used the B index, but the labels were truncated
  NEW IDX SET IDX=0
  FOR  SET IDX=$O(^TMG(22733.2,TMGDFN,"KV",IDX)) QUIT:+IDX'>0  DO
  . NEW VALUE,LINE,KEY
  . SET KEY=$P($G(^TMG(22733.2,TMGDFN,"KV",IDX,0)),"^",1)
  . SET VALUE=$P($G(^TMG(22733.2,TMGDFN,"KV",IDX,0)),"^",2)
  . SET LINE=$G(^TMG(22733.2,TMGDFN,"KV",IDX,1))
  . SET OUTARR("KEY-VALUE",KEY)=VALUE
  . SET OUTARR("KEY-VALUE",KEY,"LINE")=LINE
  QUIT
  ;
