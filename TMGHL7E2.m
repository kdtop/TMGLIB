TMGHL7E2 ;TMG/kst-HL7 Processing Error/Alert handling; 11/18/16, 3/24/21, 5/10/21
              ;;1.0;TMG-LIB;**1**; 11/18/16
  ;
  ;"TMG HL7 Error/Alert handling for lab messages
  ;
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;"Copyright (c) 11/18/16  Kevin S. Toppenberg MD
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
  ;"HNDLERR -- Handler for alert created during **Radiology** HL7 filing system.
  ;"SETALERT(ERRTEXT,AMSG,IEN772,IEN773,AMODE,TMGENV) --set up alerts for error handling of Rad filer problems. 
  ;"
  ;"=======================================================================
  ;" API - Private Functions
  ;"=======================================================================
  ;"=======================================================================
  ;"Dependancies
  ;"=======================================================================
  ;"TMGSTUTL, all the HL*, LA* code that the HL processing path normally calls.
  ;"=======================================================================
  ;"==============================================================
  ;
SETALERT(ERRTEXT,AMSG,IEN772,IEN773,AMODE,TMGENV) ;    
 ;"NOTE: for now, I am not using AMODE.  Overwritting locally anyway, to RAD
  DO SETALERT^TMGHL7E(.ERRTEXT,.AMSG,.IEN772,.IEN773,"RAD",.TMGENV)  ;"note: 5/10/21 -- I am merging error handlers back together.   
  QUIT
  ;
ERRLABEL(TMGHL7MSG)  ;
  NEW RESULT SET RESULT="Error during Rad filer process."
  NEW NAMEDOB SET NAMEDOB=$$GETNMDOB^TMGHL7U3(.TMGHL7MSG)
  IF NAMEDOB'="" SET RESULT=RESULT_" ["_NAMEDOB_"]"
  QUIT RESULT
  ;
HNDLERR ;
  ;"  ;NOTE: I found that the radiology handler code was getting further out of 
  ;"         sync with bug fixes compared to the main lab handler code.  So
  ;"         I am going to just try to get them merged back together.
  ;"         Will handle slight differences in the system by specifying a "RAD" mode.
  DO HNDLERR^TMGHL7E("RAD")
  QUIT
  ;
FIXCPTMAP(TMGERROR,INDENT) ;
  ;"E.g. TMGERROR:
  ;"    Fileman says: 'The value '`845' for field PROCEDURE in EXAMINATIONS SUB-FIELD in REGISTERED EXAMS SUB-FIELD in file RAD/NUC MED PATIENT is not valid. Details: [3]=`845  [FIELD]=2  [FILE]=70.03
  NEW TEMPS SET TEMPS=$PIECE(TMGERROR,"Details:",2)
  NEW IEN71 SET IEN71=+$PIECE($PIECE(TEMPS,"[FIELD]=",1),"]=`",2)
  NEW FIELD SET FIELD=+$PIECE(TEMPS,"[FIELD]=",2)
  NEW FILE SET FILE=+$PIECE(TEMPS,"[FILE]=",2)
  IF (FILE'=70.03)!(FIELD'=2) DO  QUIT
  . WRITE !,"Unable to automatically fix this problem.",!
  . DO PRESS2GO^TMGHL7E2
  NEW ZN SET ZN=$GET(^RAMIS(71,IEN71,0))
  NEW IEN81 SET IEN81=$PIECE(ZN,"^",9)
  NEW PROCNAME SET PROCNAME=$PIECE(ZN,"^",1)
  IF $DATA(TMGMSG("DIERR")) DO  QUIT
  . WRITE "Error: ",$$GETERRST^TMGDEBU2(.TMGMSG),!
  IF $$CPTACTIVE^TMGCPTU1(IEN81)=1 DO  QUIT
  . WRITE "CPT is already active!  Try fixing another way.",!
  . DO PRESS2GO^TMGHL7E2  
  NEW TEMP SET TEMP=$$SELCPT(PROCNAME)
  NEW CPTIEN SET CPTIEN=+$GET(TEMP)
  IF CPTIEN>0 DO
  . NEW TMGFDA,TMGIEN,TMGMSG
  . SET TMGFDA(71,IEN71_",",9)=CPTIEN
  . DO FILE^DIE("E","TMGFDA","TMGMSG")
  . DO SHOWDIER^TMGDEBU2(.TMGMSG)
  ;  
  QUIT
  ;
FIXCPT(TMGERROR,INDENT) ;
  NEW TMP SET TMP=$PIECE($PIECE(TMGERROR,",",2),"#",2)
  NEW IEN71 SET IEN71=+$$TRIM^XLFSTR(TMP)
  IF IEN71'>0 DO  GOTO FCPDN
  . WRITE !,"Unable to extract IEN from message.  Got ["_TMGERROR_"]",!
  . DO PRESS2GO^TMGUSRI2
  ;"Test to see if field 9 has already been fixed
  NEW CURCPT SET CURCPT=$$GET1^DIQ(71,IEN71,9)        
  IF CURCPT'="" DO  GOTO FCPDN
  . WRITE !,"This record seems to have already been fixed and set to ",CURCPT,! 
  . DO PRESS2GO^TMGUSRI2
  WRITE !,"Below is current record",!
  DO DUMPREC^TMGDEBU3(71,IEN71)
  DO PRESS2GO^TMGUSRI2
  NEW TESTNAME SET TESTNAME=$$GET1^DIQ(71,IEN71,.01)
  NEW TEMP SET TEMP=$$SELCPT(TESTNAME)
  NEW CPTIEN SET CPTIEN=+$G(TEMP)
  IF CPTIEN>0 DO
  . NEW TMGFDA,TMGIEN,TMGMSG
  . SET TMGFDA(71,IEN71_",",9)=CPTIEN
  . DO FILE^DIE("E","TMGFDA","TMGMSG")
  . DO SHOWDIER^TMGDEBU2(.TMGMSG)
  ;
FCPDN  ;
  QUIT        
  ;
SELCPT(TESTNAME)  ;"interact with user to pick CPT code.   
  ;"INPUT: TESTNAME -- the name of a test that user will search for match for
  ;"Result: IEN_81^CPT Code^Description,  or -1 if aborted or error.  
  NEW SELOPT,Y
  SET SELOPT("NCS")=1 ;"not case sensitive
  SET SELOPT("HEADER",1)="Please selected a record from the CPT CODE file"
  SET SELOPT("HEADER",2)="that matches: {{RED}}"_TESTNAME_"{{NORM}}"
  SET SELOPT("HEADER",3)="{{BOLD}}NOTE: If no match is found, press ^ for chance to add NEW entry{{NORM}}"
  SET SELOPT("COLORS","NORM")="7^4"
  SET SELOPT("COLORS","BOLD")="14^4"
  SET SELOPT("COLORS","RED")="14^1"
  SET SELOPT("COLORS","FOOTER")="14^6"
  SET SELOPT("SCRN WIDTH")=70
  SET SELOPT("SCRN HEIGHT")=20
  SET SELOPT("REC NAME SETUP CODE")="$$SETUPCPTLST^TMGHL7E2(IEN)"
  SET SELOPT("ON CHANGING")="HNDLCPT1^TMGHL7E2"
  SET SELOPT("FILTER CODE")="$$FILTERCPT^TMGHL7E2(NAME,IEN,.OPTION)"
  SET Y=$$RECSEL^TMGUSRI4(81,,.SELOPT) WRITE #
  IF +Y'>0 GOTO SLCPTDN
  NEW ZN SET ZN=$GET(^ICPT(+Y,0))
  NEW CPT SET CPT=$PIECE(ZN,"^",1)
  NEW DESCR SET DESCR=$PIECE(ZN,"^",2)
  WRITE "Use CPT '",CPT,"' (",DESCR,")",!,"for test '",TESTNAME,"'?"
  NEW % SET %=1 DO YN^DICN WRITE !
  IF %=1 SET Y=+Y_"^"_CPT_"^"_DESCR
  ELSE  SET Y=-1
SLCPTDN ;
  QUIT Y
  ;
FILTERCPT(NAME,IEN,OPTION) ;"This is a callback function
   ;"If function returns 1, then element is filtered (i.e. NOT included)
   QUIT '$$CPTACTIVE^TMGCPTU1(IEN)
   ;
SETUPCPTLST(IEN)  ;"This is a callback function
  NEW ZN SET ZN=$GET(^ICPT(IEN,0))
  NEW TMGRESULT SET TMGRESULT=$PIECE(ZN,"^",1)_" - "_$PIECE(ZN,"^",2)
  QUIT TMGRESULT
  ;
HNDLCPT1(PARRAY,OPTION,INFO) ;
  ;"Purpose: Handle ON CHANGING event from Scroller, from LISTSEL
  ;"Input: PARRAY -- PASSED BY NAME.  This is array that is displayed.  See SCROLLER^TMGUSRIF for documentation
  ;"       OPTION -- -- PASSED BY REFERENCE.  This is OPTION array.  See SCROLLER^TMGUSRIF for documentation
  ;"       INFO -- PASSED BY REFERENCE.  An Array with releventy information.
  ;"          INFO("CURRENT LINE","NUMBER")=number currently highlighted line
  ;"          INFO("CURRENT LINE","TEXT")=Text of currently highlighted line
  ;"          INFO("CURRENT LINE","RETURN")=return value of currently highlighted line
  ;"    Referenced globally scoped variables --
  ;"        TMGSCLRMSG,TMGRESULT
  ;"Result: NONE
  ;"Output: May affect globally-scoped variable TMGSCLRMSG to communicate back to Scroller
  ;"        May affect globally-scoped variable TMGRESULT
  SET TMGRESULT=$GET(INFO("CURRENT LINE","RETURN"))
  DO VCUSAV2^TMGTERM
  NEW YPOS SET YPOS=$GET(OPTION("SCRN HEIGHT"))+1
  NEW XMAX SET XMAX=$GET(OPTION("SCRN WIDTH"))
  DO CUP^TMGTERM(1,YPOS)
  NEW IEN SET IEN=+$GET(INFO("NEXT LINE","RETURN"))
  NEW LINE SET LINE=$$LJ^XLFSTR("=",XMAX,"=")
  WRITE LINE
  NEW LINECT SET LINECT=0
  NEW ARRAY MERGE ARRAY=^ICPT(IEN,"D") KILL ARRAY(0)
  DO WordWrapArray^TMGSTUTL(.ARRAY,XMAX-1)
  NEW SUBIEN SET SUBIEN=0
  FOR  SET SUBIEN=$ORDER(ARRAY(SUBIEN)) QUIT:(SUBIEN'>0)!(LINECT>10)  DO
  . NEW S SET S=$GET(ARRAY(SUBIEN,0))
  . SET S=$$LJ^XLFSTR(S,XMAX-1)_"|"
  . WRITE !,S
  . SET LINECT=LINECT+1
  FOR  QUIT:LINECT>10  DO
  . SET LINECT=LINECT+1     
  . NEW S SET S=$$LJ^XLFSTR(" ",XMAX-1)_"|"
  . WRITE !,S
  WRITE !,LINE
  DO VCULOAD2^TMGTERM
  QUIT
  ;
PREHANDLE(TMGERROR) ;
  ;"RESULT: 1 if handled automatically or 0 otherwise.
  NEW TMGRESULT SET TMGRESULT=0
  NEW PTNOTFOUND SET PTNOTFOUND=$$PTNOTFOUND^TMGHL7E(TMGERROR) 
  ;"NEW PTNOTFOUND SET PTNOTFOUND=(TMGERROR["Patient not found in system:")
  IF PTNOTFOUND,$$IGNORPT^TMGHL7E(TMGERROR) DO
  . DO CLEANUP2^TMGHL7E(TMGJOBN,TMGTIME) 
  . SET TMGRESULT=1
  IF TMGERROR[" Missing CPT: File# 71" DO
  . ;"TEST HERE IF CPT STILL MISSING (NOT ALREADY FIXED)
  . ;"EXAMPLE ERROR MESSAGE:  Missing CPT: File# 71, record# 707, field# 9, does not contain CPT code.  Please fix.
  . NEW IEN SET IEN=$PIECE(TMGERROR,"record# ",2),IEN=$PIECE(IEN,",",1)
  . NEW FLD SET FLD=$PIECE(TMGERROR,"field# ",2),FLD=$PIECE(FLD,",",1)
  . ;"FINISH...
  QUIT TMGRESULT
  ;