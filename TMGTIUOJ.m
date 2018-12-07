TMGTIUOJ ;TMG/kst-Text objects for use in CPRS ; 3/30/15, 1/13/17
         ;;1.0;TMG-LIB;**1,17**;03/25/06
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
 ;"=======================================================================
 ;"TMG text objects
 ;"
 ;"These are bits of code that return text to be included in progress notes etc.
 ;"They are called when the user puts text like this in a note:
 ;"     ... Mrs. Jone's vitals today are |VITALS|, measured in the office...
 ;"     'VITALS' would be a TIU TEXT OBJECT, managed through menu option
 ;"     TIUFJ CREATE OBJECTS MGR
 ;
 ;"=======================================================================
 ;"PUBLIC FUNCTIONS
 ;"=======================================================================
 ;"$$VITALS(DFN,.TIU)
 ;"$$ONEVITAL(DFN,.TIU,TYPE)
 ;"$$NICENAME(DFN)
 ;"$$FNAME(DFN)
 ;"$$MNAME(DFN)
 ;"$$LNAME(DFN)
 ;"$$PHONENUM(DFN)
 ;"$$GETTABLX(DFN,LABEL,OUT,ARRAY) -- return a table compiled from prior notes.
 ;"TESTTABL 
 ;"$$MEDLIST(RESULT,DFN)  -- RPC Entry point (TMG GET MED LIST).  Return a patient's med list
 ;"MEDARR(RESULT,DFN,ARRAY,DT) -- Get MED LIST ARRAY
 ;"$$ALLERGY(DFN) -- Get allergy list to populate TIU Object |TMG ALLERGY LIST|
 ;"$$PTPRPRO(DFN) -- Returns patient's personal pronoun
 ;"$$PTPOPRO(DFN) -- Returns patient's possessive pronoun
 ;"$$GETREACT(IEN120D8,COMMENTS,NOTHTML) --  Return either signs/symptoms (if COMMENTS=0) or comments (COMMENTS=1)
 ;"$$DETALRGY(DFN) -- FIND AND RETURN DETAILED ALLERGY INFORMATION 
 ;"$$ROSALRGY(TMGRESULT,DFN) --FIND AND RETURN DETAILED ALLERGY INFORMATION FOR THE ROS FORM 
 ;"$$WEIGHT(DFN,TIU)  ;
 ;"$$BMI(DFN,BMI,IDEALWTS,USEWTDT,WHY) --Return BMI
 ;"$$TMGVISDT(TIU)  --Visit date
 ;"$$WTTREND(DFN,TIU) --return text showing patient's trend in change of weight.
 ;"$$MEANFLUS(DFN) -- Return text showing missing items for meaningful use.
 ;"$$GETLMAMO(DFN) --Return date of last mammogram
 ;"$$GETLCOLN(DFN) --Return date of last colonoscopy
 ;"$$GETLADIR(DFN) --Return date of last advance directives        
 ;"$$GETLBONE(DFN) --Return date of bone density
 ;"$$PTPRPRO(DFN) --Returns patient's personal pronoun
 ;"$$PTPOPRO(DFN) --Returns patient's possessive pronoun
 ;"$$GETLTSH(DFN)  --Return last lab data for TSH
 ;"$$GETLB12(DFN)  --Return last lab data for Vitamin B-12
 ;"$$GETLVITD(DFN)  --Return last lab data for Vitamin D
 ;"$$GETLTOBA(DFN)  --Return last tobacco status
 ;"$$GETLEYEE(TMGDFN) --Return last diabetic eye exam
 ;"$$GETLFTEX(TMGDFN) --Return last diabetic foot exam
 ;"$$GETLGLAS(DFN)  --Return last glaucoma screening
 ;"$$GETHFGRP(DFN,HFGROUPIEN,TMGRESULTARR)  -- Return health factors for a patient by a given hf group
 ;"$$GETTIUOJ(DFN,NAME) -- return tiu text object for patient
 ;"$$GETLURIN(DFN,NUM) -- Return the last urine with comments
 ;"=======================================================================
 ;"PRIVATE FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"Dependancies :  TMGSTUT2 TMGTIUO3 TMGTIUO4 TMGTIUO6
 ;"                TIUL01 XLFDT TIULO XLFSTR
 ;"=======================================================================
 ;
ALLERGY(DFN) ; ALLERGY LIST
  QUIT $$ALLERGY^TMGTIUO3(.DFN)
  ;
GETREACT(IEN120D8,COMMENTS,NOTHTML)  ;
  QUIT $$GETREACT^TMGTIUO3(.IEN120D8,.COMMENTS,.NOTHTML)  
  ;
DETALRGY(DFN)  ;
  QUIT $$DETALRGY^TMGTIUO3(.DFN) 
  ;
ROSALRGY(TMGRESULT,DFN)  ;
  QUIT $$ROSALRGY^TMGTIUO3(.TMGRESULT,.DFN)  
  ;
VITALS(DFN,TIU) ;
  ;"Purpose: Return a composite Vitals string
  ;"Input: DFN -- the patient's unique ID (record#)
  ;"       TIU -- See documentation below.
  ;"Output: returns RESULT
  ;"note: Consider using $$GETVITLS^TMGGMRV1 in the future...
  QUIT $$ONEVITAL(.DFN,.TIU,"ALL")
  ;                                                  
WEIGHT(DFN,TIU)  ;         
  QUIT $$WEIGHT^TMGTIUO3(.DFN,.TIU)
  ;
ONEVITAL(DFN,TIU,TYPE)    ;
  QUIT $$ONEVITAL^TMGTBL01(.DFN,.TIU,.TYPE)
  ;
BMI(DFN,BMI,IDEALWTS,USEWTDT,WHY) ;"Return BMI
  QUIT $$BMI^TMGTIUO3(.DFN,.BMI,.IDEALWTS,.USEWTDT,.WHY) ;"Return BMI
  ;
TMGVISDT(TIU)  ;" Visit date
  QUIT $$TMGVISDT^TMGTIUO3(.TIU)
  ;
FNAME(DFN)  ;
  QUIT $$FNAME^TMGTIUO3(.DFN)
  ;
MNAME(DFN) ;
  QUIT $$MNAME^TMGTIUO3(.DFN)
  ;
LNAME(DFN) ;
  QUIT $$LNAME^TMGTIUO3(.DFN)
  ;
NICENAME(DFN) ;
  QUIT $$NICENAME^TMGTIUO3(.DFN)
  ;
PHONENUM(DFN) ;
  QUIT $$PHONENUM^TMGTIUO3(.DFN)
  ;
WTTREND(DFN,TIU) ;"Purpose: return text showing patient's trend in change of weight.
  QUIT $$WTTREND^TMGTIUO4(DFN,.TIU)
  ;
GETTABLX(DFN,LABEL,ARRAY,OPTION) ;"Purpose: A call point for TIU objects, to return a table comprised from prior notes.
  QUIT $$GETTABLX^TMGTIUO6(.DFN,.LABEL,.ARRAY,.OPTION)
  ;
TESTTABL  ;
  NEW DIC,X,Y,DFN,IEN22708,TABLENAME
  SET DIC=2,DIC(0)="MAEQ" DO ^DIC WRITE ! SET DFN=+Y IF DFN'>0 QUIT
  SET DIC=22708 DO ^DIC WRITE ! SET IEN22708=+Y,TABLENAME=$PIECE(Y,"^",2) IF IEN22708'>0 QUIT
  NEW ARRAY,RESULT
  DO INITPFIL^TMGMISC2("GETITEM^TMGTIUO8")
  SET RESULT=$$GETTABLX(DFN,TABLENAME,.ARRAY)
  WRITE "Table output:",!,RESULT,!
  WRITE "Associated output array:",!
  ZWRITE ARRAY
  WRITE !
  DO SHOWRPT^TMGMISC2("GETITEM^TMGTIUO8")
  QUIT
  ;  
TESTMEDLIST ;
  NEW DIC,X,Y,DFN,STR,ARR
  SET DIC=2,DIC(0)="MAEQ" DO ^DIC WRITE ! SET DFN=+Y IF DFN'>0 QUIT
  DO MEDLIST(.STR,DFN,.ARR)
  WRITE STR,!
  QUIT
  ;
TESTTIUOBJ ; ;"Interact with user to select a patient and TIU TEXT OBJECT and test it.
  NEW DIC,X,Y,DFN,STR,ARR,IEN8925D1
  SET DIC=2,DIC(0)="MAEQ" DO ^DIC WRITE ! SET DFN=+Y IF DFN'>0 DO  QUIT
  . WRITE "No patient selected.  Aborting.",! 
  SET DIC=8925.1 DO ^DIC WRITE ! SET IEN8925D1=+Y IF IEN8925D1'>0 DO  QUIT
  . WRITE "No TIU TEXT OBJECT selected.  Aborting.",! 
  NEW CODE SET CODE=$GET(^TIU(8925.1,IEN8925D1,9)) IF CODE="" DO  QUIT
  . WRITE "The selected item does not have an OBJECT METHOD.  Aborting.",!
  DO
  . NEW $ETRAP SET $ETRAP="write ""(Invalid M Code!.  Error Trapped.)"",! SET $ETRAP="""",$ECODE="""" "
  . XECUTE CODE
  . WRITE "OUTPUT:",!,$GET(X),!
  QUIT
  ;
MEDLIST(RESULT,DFN,ARRAY,DT,OPTION)  ;"Purpose: RPC (TMG GET MED LIST) to return a patient's med list
  ;"NOTE!!: It is not effecient to call this just to get ARRAY.  
  ;"        Use MEDARR() below instead!
  ;"SET OPTION("USEOLDMETHOD")=1  ;"TEMP!, REMOVE LATER.
  DO GTMEDLST^TMGTIUO8(.RESULT,.DFN,.ARRAY,.DT,.OPTION)
  QUIT
  ;
MEDARR(RESULT,DFN,ARRAY,DT,OPTION) ;"Get MED LIST ARRAY
  SET RESULT=1  ;"Does nothing.  I included to keep function signature same as MEDLIST()
  IF $GET(DT)>0 SET OPTION("DT")=DT
  ;"SET OPTION("USEOLDMETHOD")=1  ;"TEMP!, REMOVE LATER.
  DO PRIORRXT^TMGTIUO8(DFN,48,.ARRAY,1,.OPTION)
  QUIT
  ;
MEANFLUS(DFN) ;"Return text showing missing items for meaningful use.
  QUIT $$MEANFLOJ^TMGC0Q06(DFN)
  ;
GETLMAMO(DFN) ;"Return date of last mammogram
  QUIT $$GETLMAMO^TMGTBL01(.DFN)
  ;
GETLCOLN(DFN) ;"Return date of last colonoscopy
  QUIT $$GETLCOLN^TMGTBL01(.DFN)
  ;
GETLADIR(DFN) ;"Return date of last advance directives        
  QUIT $$GETLADIR^TMGTBL01(.DFN)
  ;
GETLBONE(DFN) ;"Return date of bone density
  QUIT $$GETLBONE^TMGTBL01(.DFN)
  ;
PTPRPRO(DFN)  ;Returns patient's personal pronoun
  QUIT $$PTPRPRO^TMGTIUO3(DFN) 
  ;
PTPOPRO(DFN)  ;Returns patient's possessive pronoun
  QUIT $$PTPOPRO^TMGTIUO3(DFN)  
  ;
GETLTSH(DFN)  ;"Return last lab data for TSH
  QUIT $$GETLTSH^TMGTBL01(.DFN)
  ;
GETLB12(DFN)  ;"Return last lab data for Vitamin B-12
  QUIT $$GETLB12^TMGTBL01(.DFN)
  ;
GETIVITD(DFN)  ;"Return Vit-D level for inline table
  QUIT $PIECE($$GETLVITD(DFN),"[T]",1)
  ;
GETIMAG(DFN)   ;"Return Magnesium level for inline table
  QUIT "NOT FINISHED"
  ;
GETLVITD(DFN)  ;"Return last lab data for Vitamin D
  QUIT $$GETLVITD^TMGTIUO3(.DFN)  ;"Return last lab data for Vitamin D
  ;
GETLTOBA(DFN)  ;"Return last tobacco status
  QUIT $$GETLTOBA^TMGTBL01(.DFN)
  ;
GETLEYEE(TMGDFN) ;"Return last diabetic eye exam
  QUIT $$GETLEYEE^TMGTBL01(.TMGDFN)
  ;
GETLFTEX(TMGDFN) ;"Return last diabetic foot exam
  QUIT $$GETLFTEX^TMGTBL01(.TMGDFN)
  ;
GETLGLAS(DFN)  ;"Return last glaucoma screening
  QUIT $$GETLGLAS^TMGTBL01(.DFN)
  ;"
GETHFGRP(DFN,HFGROUPIEN,TMGRESULTARR)  ;"Return health factors for a patient by a given hf group
  DO GETHFGRP^TMGTBL01(.DFN,.HFGROUPIEN,.TMGRESULTARR)
  QUIT
  ;        
GETTIUOJ(DFN,NAME) ;" return tiu text object for patient
  NEW RESULT SET RESULT=$$BOIL^TIUSRVD("|"_NAME_"|")
  QUIT RESULT
  ;
ADGIVEN(DFN)  ;"Return the health factor date for when the last CP papers were given
  QUIT $$ADGIVEN^TMGTBL01(.DFN)
  ;
GETLLAB(DFN,LABNUM,NUM,DTONLY)   ;"Return the last urine culture
  QUIT $$GETLLAB^TMGTBL01(.DFN,.LABNUM,.NUM,.DTONLY)
  ;
ALLHFTBL(DFN)  ;"Return an HTML table containing all health factors
  QUIT $$ALLHFTBL^TMGTBL01(.DFN)
  ;
FUITEMS(DFN)  ;"Return the followup table if data is contained
  QUIT $$FUITEMS^TMGTIUO3(.DFN)  
  ;
LASTHPI(DFN)  ;"Return the last HPI section.  Called by TIU TEXT OBJECT 'TMG LAST HPI'
  QUIT $$LASTHPI^TMGTIUP2(.DFN)  ;"Moved to TMGTIUP2 to minimize file size 
  ;
ADMINDOC(TMGRESULT);
  NEW IEN SET IEN=0
  FOR  SET IEN=$ORDER(^TMG(22737,"B",IEN)) QUIT:IEN'>0  DO
  . NEW NAME SET NAME=$PIECE($GET(^TIU(8925.1,IEN,0)),"^",1)
  . SET TMGRESULT(IEN)=NAME_";"
  QUIT
  ;
XTRAFORM(TMGRESULT,DFN)  ;
  QUIT $$XTRAFORM^TMGTIUO3(.TMGRESULT,.DFN) 
  ;
NEEDPSA(TMGRESULT,DFN)  ;
  QUIT $$NEEDPSA^TMGTIUO3(.TMGRESULT,.DFN)  
  ;
GETSTATS(STATUS,TMGRESULT) ;
  QUIT $$GETSTATS^TMGTIUO3(.STATUS,.TMGRESULT) 
  ;
UNSIGNED(TMGRESULT)  ;
  QUIT $$UNSIGNED^TMGTIUO3(.TMGRESULT)  
  ;
ADDLSIGN(TMGRESULT)  ;
  QUIT $$ADDLSIGN^TMGTIUOJ(.TMGRESULT)  
  ;
LASTOPTH(DFN)  ;
  QUIT $$LASTOPTH^TMGTIUO3(.DFN) 
  ;      