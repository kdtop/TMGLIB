TMGTIUO5 ;TMG/kst-Text objects for use in CPRS ; 3/12/15, 1/15/17
         ;;1.0;TMG-LIB;**1,17**;7/20/12
 ;
 ;"Kevin Toppenberg MD
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;" This is spill over code from TMGTIUOJ, to make that file size smaller.
 ;"
 ;"=======================================================================
 ;"PUBLIC FUNCTIONS
 ;"=======================================================================
 ;"GNOTELST(DFN,LIST,INCDAYS,OPTIONS) --Return a list of notes for patient in given time span
 ;"LASTNOTE(DFN,LIST,STARTMARKERS,OPTION) -- GET LAST NOTE POINTED TO IN 22729 
 ;"XTRCTSPC(IEN8925,STARTMARKERS,ENDMARKERS,ARRAY,SPACES) -- scan the REPORT TEXT field in given document and return ...
 ;"XTRCTREF(REF,STARTMARKERS,ENDMARKERS,ARRAY,SPACES) -- EXTRACT SPECIAL, FROM REFERENCE. 
 ;"SORTMARR(ARR) -- SPLIT MED ARRAY UP INTO LINES, AND KEY-VALUE PAIRS.  
 ;"EMEDSARR(DFN,MEDARRAY)  -- add any eRx scripts found in a note to the provided array. 
 ;"ERX1TIU(TIUIEN,MEDARRAY) -- add any eRx scripts found specified note to the provided array. 
 ;"NOTETEXT(TIUIEN) -- returns the text of a note for a given IEN
 ;"
 ;"=======================================================================
 ;"PRIVATE FUNCTIONS
 ;"=======================================================================
 ;"MERGEIN(PARTARRAY,MASTERARRAY) --combine PARTARRAY into MasterARRAY.
 ;"SETKEYVL(LINE,ARR) --SET KE-VALUE
 ;"BEGTAG()
 ;"ENDTAG()
 ;"MEDTAG()
 ;"FINALMED()
 ;"ERXSUFFX()
 ;"CHK1ERX(TIUIEN,MEDARRAY,ADDED,FINALMED)  ;
 ;"NOTEMEDS(TIUIEN,NOTETEXT,MEDARRAY,ADDED)  -- GET EPRESCRIBED MEDS OUT OF A STRING
 ;"ERXNOTES(DFN,MEDARRAY,ADDED) -- FIND THE LAST NOTE WITH MED LIST AS WELL AS ERX DATA
 ;"FRMTMED(THISMED)  -- FORMAT MEDICATION LINE
 ;"SORTMEDS(MEDARRAY) -- sort the medications if any new ones were added
 ;"
 ;"=======================================================================
 ;"Dependancies : %DTC TMGTHM1 XLFSTR
 ;"=======================================================================
GNOTELST(DFN,LIST,INCDAYS,OPTIONS) ;"GET NOTE LIST
  ;"Purpose: Return a list of notes for patient in given time span
  ;"Input: DFN -- IEN in PATIENT file (the patient record number)
  ;"       LIST -- PASS BY REFERENCE, an OUT PARAMETER. (Format below)
  ;"       INCDAYS -- Number of DAYS to search in.
  ;"              E.g. 4 --> get notes from last 4 days
  ;"       OPTIONS -- PASS BY REFERENCE.  Optional variable.  Format
  ;"            OPTIONS("ALL NOTES")=1  If found then ALL notes for patient
  ;"                   will be searched.  If not found, then default is to
  ;"                   only search notes with COMPLETED status.  
  ;"            OPTIONS("DT")=FMDT <-- if present, then ignore notes AFTER this date.  
  ;"Output: LIST format:
  ;"              LIST(FMTimeOfNote,IEN8925)=""
  ;"              LIST(FMTimeOfNote,IEN8925)=""
  ;"              LIST(FMTimeOfNote,IEN8925)=""
  ;"        If no notes found, then array is left blank.  Prior entries KILLED
  ;"Results: none
  ;"5/8/13 -- modified such that only COMPLETED notes are returned.
  ;"3/12/15 -- modified such that ALL notes can be returned optionally.
  ;"1/15/17 -- modified such that notes AFTER specified DT are ignored.  
  KILL LIST
  SET DFN=+$GET(DFN)
  IF DFN'>0 GOTO GNLDONE
  SET INCDAYS=+$GET(INCDAYS)
  NEW X,NOWDT DO NOW^%DTC SET NOWDT=X
  NEW PTRCOMPL SET PTRCOMPL=$ORDER(^TIU(8925.6,"B","COMPLETED",0))
  NEW USEALLNOTES SET USEALLNOTES=+$GET(OPTIONS("ALL NOTES"))
  ;"NEW TEMP MERGE TEMP=^TIU(8925,"C",DFN)
  NEW EDT SET EDT=+$GET(OPTIONS("DT")) IF EDT=0 SET EDT=9999999  ;"//KT 1/15/17
  NEW IEN SET IEN=""
  FOR  SET IEN=$ORDER(^TIU(8925,"C",DFN,IEN)) QUIT:(+IEN'>0)  DO        
  . NEW X,X1,X2,%Y,ZNODE,STARTDATE,NODE13
  . SET ZNODE=$GET(^TIU(8925,IEN,0))
  . SET NODE13=$GET(^TIU(8925,IEN,13))
  . IF (USEALLNOTES=0),($PIECE(ZNODE,"^",5)'=PTRCOMPL) QUIT  ;"0;5=STATUS field.  Only allow COMPLETED notes.
  . SET X1=NOWDT
  . ;" ORIGINAL LINE SET (STARTDATE,X2)=$PIECE(ZNODE,"^",7) ;"STARTDATE
  . SET (STARTDATE,X2)=$PIECE(NODE13,"^",1) ;"REF DATE
  . IF STARTDATE>EDT QUIT  ;"//kt 1/15/17
  . DO ^%DTC ;"calculate X=X1-X2.  Returns #days between
  . IF X>INCDAYS QUIT
  . SET LIST(STARTDATE,IEN)=""
GNLDONE ;
  QUIT
  ;
LASTNOTE(DFN,LIST,STARTMARKERS,OPTION) ;"GET LAST NOTE POINTED TO IN 22729
  ;"Purpose: Return the note referenced in 22729
  ;"Input: DFN - Patient IEN
  ;"       LIST - Return array
  ;"       STARTMARKERS - Beginning table marker
  ;"       OPTIONS -- PASS BY REFERENCE.  Optional variable.  Format
  ;"            OPTION("DT")=FMDT <-- if present, then data AFTER given DT is ignored
  ;"Output: LIST(NOTEDTTIME,TIUIEN)="" ;"should just be one note or blank
  ;"Result: 0 for no note, 1 for note found
  NEW TMGRESULT SET TMGRESULT=0 ;"Result to failure
  ;"Get tableien        
  NEW TABLEIEN,TABLENAME
  IF STARTMARKERS["[" DO
  . SET TABLENAME=$PIECE(STARTMARKERS,"[",2)
  . SET TABLENAME=$PIECE(TABLENAME,"]",1)
  ELSE  DO                        
  . SET TABLENAME=STARTMARKERS
  SET TABLEIEN=+$ORDER(^TMG(22708,"B",TABLENAME,0))
  IF TABLEIEN'>0 GOTO LNDN
  ;"
  ;"We are suffering from missing items in the followup table
  ;"I have yet to recreate the issue, so as a stop gap measure
  ;"if the table is the fu table (#31) I am going to set the Option
  ;"to check all notes as this function bubbles back up.
  ;"This will supercede the  entry in 22729, for that table only
  ;"until I figure what is happening
  ;"     ELH    12/19/19
  ;"
  IF TABLEIEN=31 SET OPTION("ALL NOTES")=1  
  ;"
  ;"Find if table is referenced in 22729
  NEW IDX SET IDX=+$ORDER(^TMG(22729,"C",DFN,TABLEIEN,0))
  IF IDX'>0 GOTO LNDN
  NEW TIUIEN SET TIUIEN=+$PIECE($GET(^TMG(22729,IDX,0)),"^",3)
  IF TIUIEN'>0 GOTO LNDN
  ;"Make sure table is found within note text
  NEW NOTETEXT,TEXTIDX,NOTEDT
  SET NOTEDT=$PIECE($GET(^TIU(8925,TIUIEN,0)),"^",7)
  NEW ASOFDT SET ASOFDT=+$GET(OPTION("DT"))
  IF ASOFDT>0,NOTEDT>ASOFDT GOTO LNDN  ;"skip if note is after AS-OF date
  SET TEXTIDX=0,NOTETEXT=""
  FOR  SET TEXTIDX=$ORDER(^TIU(8925,TIUIEN,"TEXT",TEXTIDX)) QUIT:TEXTIDX'>0  DO
  . SET NOTETEXT=NOTETEXT_$GET(^TIU(8925,TIUIEN,"TEXT",TEXTIDX,0))
  IF NOTETEXT[STARTMARKERS DO
  . SET LIST(NOTEDT,TIUIEN)=""
  . SET TMGRESULT=1
LNDN ;
  QUIT TMGRESULT
  ;
XTRCTSPC(IEN8925,STARTMARKERS,ENDMARKERS,ARRAY,SPACES) ;"EXTRACT SPECIAL
  ;"Purpose: To scan the REPORT TEXT field in given document and return
  ;"         paragraph of text that is started by STARTMARKERS, and ended by ENDMARKERS.
  ;"         I.E. Search for a line that contains MARKERS.  Return that line and
  ;"         all following lines until line found with ENDMARKERS, or
  ;"         end of text.
  ;"Input: IEN8925 -- IEN in file 8925 (TIU DOCUMENT)
  ;"       STARTMARKERS -- the string to search for that indicates start of block
  ;"       ENDMARKERS -- the string to search for that indicates the end of block.
  ;"              NOTE: IF ENDMARKERS="BLANK_LINE", then search is
  ;"              ended when a blank line is encountered.
  ;"              ALSO, will end if </P> or </TD> or </TR> FOUND
  ;"       ARRAY -- PASS BY REFERENCE, an OUT PARAMETER.  Prior values killed.
  ;"              Format:  ARRAY(0)=MaxLineCount
  ;"                       ARRAY(1)="Text line 1"
  ;"                       ARRAY(2)="Text line 2" ...
  ;"       SPACES -- OPTIONAL.  Pass by reference. AN OUT PARAMETER.
  ;"                      Fill with the space length that found tables are indented with.
  ;"Result: 1 IF data found, otherwise 0
  ;
  NEW RESULT SET RESULT=0
  NEW ISHTML
  KILL ARRAY
  SET SPACES=""
  SET IEN8925=+$GET(IEN8925)
  IF IEN8925'>0 GOTO ESDONE
  IF $DATA(^TIU(8925,IEN8925,"TEXT"))'>0 GOTO ESDONE
  IF $GET(STARTMARKERS)="" GOTO ESDONE
  IF $GET(ENDMARKERS)="" GOTO ESDONE
  NEW REF SET REF=$NAME(^TIU(8925,IEN8925,"TEXT"))
  ;"to do ... just call XTRCTREF() below, but needs to be tested.  
  NEW TEMPARRAY
  NEW ISHTML SET ISHTML=$$ISHTML^TMGHTM1(IEN8925)
  IF ISHTML DO
  . MERGE TEMPARRAY=^TIU(8925,IEN8925,"TEXT")
  . DO HTML2TXT^TMGHTM1(.TEMPARRAY)
  . SET REF="TEMPARRAY"
  NEW LINE,I,BLOCKFOUND,DONE
  SET LINE=0,I=0,BLOCKFOUND=0,DONE=0
  IF STARTMARKERS'["[" SET STARTMARKERS="["_STARTMARKERS_"]"
  FOR  SET LINE=$ORDER(@REF@(LINE)) QUIT:(LINE="")!DONE  DO
  . NEW LINES SET LINES=$GET(@REF@(LINE,0))
  . IF (BLOCKFOUND=0) DO  QUIT  ;"don't include header line with output
  . . IF LINES[STARTMARKERS DO
  . . . SET BLOCKFOUND=1
  . . . FOR  QUIT:$EXTRACT(LINES,1)'=" "  DO
  . . . . SET SPACES=SPACES_" "
  . . . . SET LINES=$EXTRACT(LINES,2,$LENGTH(LINES))
  . IF (BLOCKFOUND=1) DO
  . . SET I=I+1,ARRAY(0)=I
  . . NEW S2 SET S2=$$TRIM^XLFSTR(LINES)
  . . SET S2=$$TRIM^XLFSTR(S2,"LR",$CHAR(9))
  . . SET ARRAY(I)=LINES
  . . IF S2="" SET ARRAY(I)=S2
  . . SET RESULT=1
  . . IF (ENDMARKERS="BLANK_LINE")&(S2="") SET BLOCKFOUND=0,DONE=1 QUIT
  . . IF LINES[ENDMARKERS SET BLOCKFOUND=0,DONE=1 QUIT  ;"include line with END marker
  IF 'ISHTML SET SPACES=$CHAR(9) ;"(TAB char)
  ;"ELSE  SET SPACES="  "  ;"<--- NOTE: This forces ALL html tables to be indented this amount.
ESDONE ;
  QUIT RESULT
  ;
XTRCTREF(REF,STARTMARKERS,ENDMARKERS,ARRAY,SPACES) ;"EXTRACT SPECIAL, FROM REFERENCE. 
  ;"Purpose: To scan a WP field or array and return
  ;"         paragraph of text that is started by STARTMARKERS, and ended by ENDMARKERS.
  ;"         I.e. Search for a line that contains MARKERS.  Return that line and
  ;"         all following lines until line found with ENDMARKERS, or
  ;"         end of text.
  ;"Input: REF -- NAME (reference) of array holding text.  Expected format:
  ;"         @REF@(#,0)=<line of text>  <-- used preferentially
  ;"         @REF@(#)=<line of text>  <-- used if @REF@(#,0) empty  
  ;"       STARTMARKERS -- the string to search for that indicates start of block
  ;"       ENDMARKERS -- the string to search for that indicates the end of block.
  ;"              NOTE: IF ENDMARKERS="BLANK_LINE", then search is
  ;"              ended when a blank line is encountered.
  ;"              ALSO, will end if </P> or </TD> or </TR> FOUND
  ;"       ARRAY -- PASS BY REFERENCE, an OUT PARAMETER.  Prior values killed.
  ;"              Format:  ARRAY(0)=MaxLineCount
  ;"                       ARRAY(1)="Text line 1"
  ;"                       ARRAY(2)="Text line 2" ...
  ;"       SPACES -- OPTIONAL.  Pass by reference. AN OUT PARAMETER.
  ;"                      Fill with the space length that found tables are indented with.
  ;"Result: 1 if data found, otherwise 0
  ;
  NEW RESULT SET RESULT=0
  NEW ISHTML,TEMPARRAY
  KILL ARRAY
  SET SPACES=""
  NEW IDX SET IDX=$ORDER(@REF@(0))
  NEW ZNMODE SET ZNMODE=($DATA(@REF@(IDX,0))>0)
  IF $GET(STARTMARKERS)="" GOTO XRDONE
  IF $GET(ENDMARKERS)="" GOTO XRDONE
  NEW ISHTML SET ISHTML=$$ISHTMREF^TMGHTM1(REF,ZNMODE)
  IF ISHTML DO
  . NEW HEADERZN SET HEADERZN=$GET(@REF@(0))  ;"//i.e. header of WP subfile
  . IF ZNMODE DO
  . . NEW TEMPARRAY MERGE TEMPARRAY=@REF
  . . DO HTML2TXT^TMGHTM1(.TEMPARRAY)
  . . KILL @REF MERGE @REF=TEMPARRAY
  . ELSE  DO
  . . NEW TEMPARRAY 
  . . NEW IDX SET IDX=0
  . . FOR  SET IDX=$ORDER(@REF@(IDX)) QUIT:IDX'>0  SET TEMPARRAY(IDX,0)=$GET(@REF@(IDX))
  . . DO HTML2TXT^TMGHTM1(.TEMPARRAY)
  . . SET IDX=0 KILL @REF
  . . FOR  SET IDX=$ORDER(TEMPARRAY(IDX)) QUIT:IDX'>0  SET @REF@(IDX)=$GET(TEMPARRAY(IDX,0))
  . SET @REF@(0)=HEADERZN
  NEW LINE,I,BLOCKFOUND,DONE SET (LINE,I,BLOCKFOUND,DONE)=0
  IF STARTMARKERS'["[" SET STARTMARKERS="["_STARTMARKERS_"]"
  FOR  SET LINE=$ORDER(@REF@(LINE)) QUIT:(LINE="")!DONE  DO
  . NEW LINES 
  . IF ZNMODE SET LINES=$GET(@REF@(LINE,0))
  . ELSE  SET LINES=$GET(@REF@(LINE))
  . IF (BLOCKFOUND=0) DO  QUIT  ;"don't include header line with output
  . . IF LINES[STARTMARKERS DO
  . . . SET BLOCKFOUND=1
  . . . FOR  QUIT:$EXTRACT(LINES,1)'=" "  DO
  . . . . SET SPACES=SPACES_" "
  . . . . SET LINES=$EXTRACT(LINES,2,$LENGTH(LINES))
  . IF (BLOCKFOUND=1) DO
  . . SET I=I+1,ARRAY(0)=I
  . . NEW S2 SET S2=$$TRIM^XLFSTR(LINES)
  . . SET S2=$$TRIM^XLFSTR(S2,"LR",$CHAR(9))
  . . SET ARRAY(I)=LINES
  . . IF S2="" SET ARRAY(I)=S2
  . . SET RESULT=1
  . . IF (ENDMARKERS="BLANK_LINE")&(S2="") SET BLOCKFOUND=0,DONE=1 QUIT
  . . IF LINES[ENDMARKERS SET BLOCKFOUND=0,DONE=1 QUIT  ;"include line with END marker
  IF 'ISHTML SET SPACES=$CHAR(9) ;"(TAB char)
  ;"ELSE  SET SPACES="  "  ;"<--- NOTE: This forces ALL html tables to be indented this amount.
XRDONE ;
  QUIT RESULT
  ;
  
MERGEIN(PARTARRAY,MASTERARRAY) ;"MERGE INTO
  ;"Purpose: to combine PARTARRAY into MasterARRAY.
  ;"Input: PARTARRAY -- PASS BY REFERENCE
  ;"       MASTERARRAY -- PASS BY REFERENCE
  ;"Note:  Arrays are combine in a 'transparent' manner such that newer entries
  ;"       will overwrite older entries only for identical values.  For example:
  ;"         -- BLOCK --   <--- MasterArray
  ;"             TSH = 1.56
  ;"             LDL = 140
  ;"         -- END BLOCK --
  ;"       
  ;"         -- BLOCK --   <--- PARTArray
  ;"             LDL = 150
  ;"         -- END BLOCK --
  ;"       
  ;"        The above two blocks will result in this final array
  ;"             -- BLOCK --
  ;"                 TSH = 1.56
  ;"                 LDL = 150   <--- this value overwrote older entry
  ;"             -- END BLOCK --
  ;"        
  ;"         In this mode, only data that is in a LABEL <--> VALUE format
  ;"            will be checked for newer vs older entries.  All other
  ;"            lines will simply be included in one large summation block.
  ;"         And the allowed format for LABEL <--> VALUE will be:
  ;"                 Label = value      or
  ;"                 Label : value
  ;"
  ;"Output: MASTERARRAY will be filled as follows:
  ;"         MASTERARRAY(Seq#)="text line"
  ;"         MASTERARRAY(Seq#)="text line"
  ;"         MASTERARRAY("KEY-VALUE",KEYName)=VALUE
  ;"         MASTERARRAY("KEY-VALUE",KEYName,"LINE")=original line
  ;
  NEW SORTARRAY
  NEW LINENUM SET LINENUM=0
  FOR  SET LINENUM=$ORDER(PARTARRAY(LINENUM)) QUIT:(+LINENUM'>0)  DO
  . NEW LINE SET LINE=$GET(PARTARRAY(LINENUM))
  . IF (LINE["=")!(LINE[":") DO
  . . DO SETKEYVL(LINE,.MASTERARRAY)  
  . . ;" NEW KEY,SHORTKEY,VALUE,PIVOT,P1,P2
  . . ;" SET P1=$FIND(LINE,"="),P2=$FIND(LINE,":")
  . . ;" IF P1>0 DO
  . . ;" . IF (P2>0)&(P2<P1) SET PIVOT=":"
  . . ;" . ELSE  SET PIVOT="="
  . . ;" ELSE  SET PIVOT=":"
  . . ;" SET KEY=$PIECE(LINE,PIVOT,1)
  . . ;" SET SHORTKEY=$TRANSLATE(KEY,$CHAR(9)," ")
  . . ;" SET SHORTKEY=$$UP^XLFSTR($$TRIM^XLFSTR(SHORTKEY))
  . . ;" SET VALUE=$PIECE(LINE,PIVOT,2,999)
  . . ;" SET VALUE=$$TRIM^XLFSTR(VALUE) ;"//kt 7/14/12
  . . ;" IF VALUE="" DO
  . . ;" . SET VALUE="   "  ;Removed final period
  . . ;" . SET LINE=LINE_"   ."
  . . ;" SET MASTERARRAY("KEY-VALUE",SHORTKEY)=VALUE
  . . ;" SET MASTERARRAY("KEY-VALUE",SHORTKEY,"LINE")=LINE
  . ELSE  DO
  . . IF LINE="" QUIT
  . . SET LINE=$$TRIM^XLFSTR(LINE)  ;"//kt 7/14/12
  . . SET SORTARRAY($$UP^XLFSTR(LINE))=LINE ;"//kt 7/14/12
  . . ;"SET ARRAY(LINE)=""
  NEW ONELINE SET ONELINE=""
  NEW CT SET CT=+$ORDER(MASTERARRAY("@"),-1)
  FOR  SET ONELINE=$ORDER(SORTARRAY(ONELINE)) Q:ONELINE=""  DO
  . NEW LINE SET LINE=$GET(SORTARRAY(ONELINE))
  . SET CT=CT+1,MASTERARRAY(CT)=LINE
  QUIT
  ;
SORTMARR(ARR)  ;"SPLIT MED ARRAY UP INTO LINES, AND KEY-VALUE PAIRS.  
  ;"Input: ARR -- PASS BY REFERENCE.  Format: ARR(#)=<line>
  NEW SORTARRAY,SOURCEARR MERGE SOURCEARR=ARR KILL ARR
  NEW LINENUM SET LINENUM=0
  FOR  SET LINENUM=$ORDER(SOURCEARR(LINENUM)) QUIT:(+LINENUM'>0)  DO
  . NEW LINE SET LINE=$GET(SOURCEARR(LINENUM))  
  . SET LINE=$$TRIM^XLFSTR(LINE) QUIT:LINE=""
  . ;"IF (LINE["=")!(LINE[":") DO
  . ;"Changed the IF so key-values wouldn't have false positives
  . IF $E($$TRIM^XLFSTR(LINE))="*" DO
  . . DO SETKEYVL(LINE,.ARR)  
  . ELSE  DO
  . . SET SORTARRAY($$UP^XLFSTR(LINE))=LINE 
  NEW ONELINE SET ONELINE=""
  NEW CT SET CT=0
  FOR  SET ONELINE=$ORDER(SORTARRAY(ONELINE)) Q:ONELINE=""  DO
  . NEW LINE SET LINE=$GET(SORTARRAY(ONELINE))
  . SET CT=CT+1,ARR(CT)=LINE
  QUIT
  ;
SETKEYVL(LINE,ARR)  ;"SET KE-VALUE
  ;"NOTE!!  ASSUMES (LINE["=")!(LINE[":")
  NEW KEY,SHORTKEY,VALUE,PIVOT,P1,P2
  SET P1=$FIND(LINE,"="),P2=$FIND(LINE,":")
  IF P1>0 DO
  . IF (P2>0)&(P2<P1) SET PIVOT=":"
  . ELSE  SET PIVOT="="
  ELSE  SET PIVOT=":"
  SET KEY=$PIECE(LINE,PIVOT,1)
  SET SHORTKEY=$TRANSLATE(KEY,$CHAR(9)," ")
  SET SHORTKEY=$$UP^XLFSTR($$TRIM^XLFSTR(SHORTKEY))
  SET VALUE=$PIECE(LINE,PIVOT,2,999)
  SET VALUE=$$TRIM^XLFSTR(VALUE) ;"//kt 7/14/12
  IF VALUE="" DO
  . SET VALUE="   "  ;Removed final period
  . SET LINE=LINE_"   ."
  SET ARR("KEY-VALUE",SHORTKEY)=VALUE
  SET ARR("KEY-VALUE",SHORTKEY,"LINE")=LINE
  QUIT
  ;
  ;"====================================
  ;"== CHECK NOTES FOR ERX MEDICATIONS
  ;"====================================
BEGTAG()
  QUIT "{E-SCRIBE}"
  ;"
ENDTAG()
  QUIT "{/E-SCRIBE}"
  ;"
MEDTAG()
  QUIT "MEDICATION:"
  ;"
FINALMED()
  QUIT "[FINAL MEDICATIONS]"
  ;"
ERXSUFFX()
  QUIT " [Auto added "_$$TODAY^TMGDATE(1)_"] "
  ;"

EMEDSARR(DFN,MEDARRAY)  ;"
  ;"Purpose: This function will add any eRx scripts found in a note to the
  ;"         provided array. 
  ;"Input: DFN - Patient's IEN
  ;"       MEDARRAY - Meds will be added to the end of the medication array
  NEW ADDED SET ADDED=0
  DO ERXNOTES(DFN,.MEDARRAY,.ADDED)
  IF ADDED=1 DO SORTMEDS(.MEDARRAY)
  QUIT
  ;"  
ERX1TIU(TIUIEN,MEDARRAY) ;"
  ;"Purpose: This function will add any eRx scripts found specified note to the
  ;"         provided array. 
  ;"Input: TIUIEN -- IEN IN 8925
  ;"       MEDARRAY - Meds will be added to the end of the medication array
  NEW ADDED SET ADDED=0
  NEW CNT SET CNT=$ORDER(MEDARRAY(9999),-1)
  SET MEDARRAY(0)=CNT
  DO CHK1ERX(TIUIEN,.MEDARRAY,.ADDED)
  IF ADDED=1 DO SORTMEDS(.MEDARRAY)
  QUIT
  ;
ERXNOTES(DFN,MEDARRAY,ADDED) ;"FIND THE LAST NOTE WITH MED LIST AS WELL AS ERX DATA
  NEW TIUDATE SET TIUDATE=9999999
  NEW DONE
  NEW CNT SET CNT=$ORDER(MEDARRAY(9999),-1)
  SET MEDARRAY(0)=CNT
  SET DONE=0
  ;"FIND NOTES
  FOR  SET TIUDATE=$ORDER(^TIU(8925,"ZTMGPTDT",DFN,TIUDATE),-1) QUIT:(TIUDATE'>0)!(DONE=1)  DO
  . NEW TIUIEN,NOTETEXT
  . SET TIUIEN=0
  . FOR  SET TIUIEN=$ORDER(^TIU(8925,"ZTMGPTDT",DFN,TIUDATE,TIUIEN)) QUIT:(TIUIEN'>0)!(DONE=1)  DO
  . . DO CHK1ERX(TIUIEN,.MEDARRAY,.ADDED,.DONE)  ;"//kt
  . . ;" SET NOTETEXT=$$NOTETEXT(TIUIEN)
  . . ;" IF NOTETEXT[$$FINALMED SET DONE=1
  . . ;" SET NOTETEXT=$$HTML2TXS^TMGHTM1(NOTETEXT)
  . . ;" SET NOTETEXT=$$UP^XLFSTR(NOTETEXT)
  . . ;" IF NOTETEXT'[$$BEGTAG QUIT
  . . ;" DO NOTEMEDS(TIUIEN,NOTETEXT,.MEDARRAY,.ADDED)
  QUIT
  ;"
CHK1ERX(TIUIEN,MEDARRAY,ADDED,FINALMED)  ;"GET MED LIST FROM 1 TIU NOTE
  NEW NOTETEXT SET NOTETEXT=$$NOTETEXT(TIUIEN)
  SET FINALMED=(NOTETEXT[$$FINALMED)
  SET NOTETEXT=$$HTML2TXS^TMGHTM1(NOTETEXT)
  SET NOTETEXT=$$UP^XLFSTR(NOTETEXT)
  IF NOTETEXT'[$$BEGTAG QUIT
  DO NOTEMEDS(.TIUIEN,.NOTETEXT,.MEDARRAY,.ADDED)
  QUIT
  ;
NOTEMEDS(TIUIEN,NOTETEXT,MEDARRAY,ADDED)  ;"GET EPRESCRIBED MEDS OUT OF A STRING
  NEW RESULT SET RESULT=""
  ;"
  FOR  QUIT:NOTETEXT'[$$BEGTAG  DO
  . NEW TEMPTEXT
  . SET NOTETEXT=$P(NOTETEXT,$$BEGTAG,2,999) 
  . SET TEMPTEXT=$P(NOTETEXT,$$ENDTAG,1)
  . SET NOTETEXT=$P(NOTETEXT,$$ENDTAG,2,999)
  . ;"
  . ;"LOOP THROUGH TO GET ALL MEDS FOUND
  . FOR  QUIT:TEMPTEXT'[$$MEDTAG  DO
  . . SET TEMPTEXT=$P(TEMPTEXT,$$MEDTAG,2,999)
  . . NEW THISMED SET THISMED=$P(TEMPTEXT,$$MEDTAG,1)
  . . NEW COUNT SET COUNT=$G(MEDARRAY(0))+1
  . . SET MEDARRAY(COUNT)=$$FRMTMED(THISMED)
  . . SET MEDARRAY(0)=COUNT
  . . SET ADDED=1
ERXDN
  QUIT RESULT
  ;"
FRMTMED(THISMED,ADDLINEBREAK)  ;"FORMAT MEDICATION LINE
  NEW MEDSTR SET MEDSTR=""
  SET THISMED=$P(THISMED,"- REFILL",1)
  SET THISMED=$$TRIM^XLFSTR($P(THISMED,"SIG: TAKE",1))_";"_$$TRIM^XLFSTR($P(THISMED,"SIG: TAKE",2))
  SET THISMED=$$TRIM^XLFSTR($P(THISMED,"SIG:",1))_";"_$$TRIM^XLFSTR($P(THISMED,"SIG:",2))
  SET THISMED=$P(THISMED,"QUANTITY",1)
  SET MEDSTR=MEDSTR_$$SENTENCE^XLFSTR(THISMED)_$$ERXSUFFX
  IF +$G(ADDLINEBREAK)>0 SET MEDSTR=MEDSTR_"<BR>"
  QUIT MEDSTR
  ;"
SORTMEDS(MEDARRAY)
  ;"Purpose: This will sort the medications if any new ones were added
  NEW IDX SET IDX=0
  NEW SOURCEARR MERGE SOURCEARR=MEDARRAY
  NEW TEMPARR
  FOR  SET IDX=$O(MEDARRAY(IDX)) QUIT:IDX'>0  DO
  . NEW LINE SET LINE=$$TRIM^XLFSTR($$UP^XLFSTR($G(MEDARRAY(IDX))))
  . QUIT:LINE=""
  . SET TEMPARR(LINE)=IDX
  NEW KEYVALUE MERGE KEYVALUE=MEDARRAY("KEY-VALUE")
  KILL MEDARRAY  ;"//kt 5/9/18
  SET IDX=1
  NEW MED SET MED=""
  FOR  SET MED=$O(TEMPARR(MED)) QUIT:MED=""  DO
  . NEW SOURCELINE SET SOURCELINE=$G(TEMPARR(MED))
  . SET MEDARRAY(IDX)=$GET(SOURCEARR(SOURCELINE))
  . SET MEDARRAY(0)=IDX  ;"ADDED 6/12/18
  . SET IDX=IDX+1
  MERGE MEDARRAY("KEY-VALUE")=KEYVALUE
  QUIT
  ;"
  ;"====================================
  ;"== END OF ERX MED FUNCTIONS
  ;"====================================
  ;"
NOTETEXT(TIUIEN) ;"
  ;"Purpose: this function returns the text of a note for a given IEN
  NEW LINE SET LINE=0
  NEW NOTETEXT SET NOTETEXT=""
  FOR  SET LINE=$ORDER(^TIU(8925,TIUIEN,"TEXT",LINE)) QUIT:+LINE'>0  DO
  . NEW ONELINE SET ONELINE=$GET(^TIU(8925,TIUIEN,"TEXT",LINE,0))
  . SET NOTETEXT=NOTETEXT_ONELINE
  QUIT NOTETEXT
  ;"
  