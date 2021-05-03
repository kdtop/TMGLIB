TMGTIUOJ ;TMG/kst-Text objects for use in CPRS ; 3/30/15, 1/13/17, 3/28/21
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
 ;"$$VITALS(TMGDFN,.TIU)
 ;"$$ONEVITAL(TMGDFN,.TIU,TYPE)
 ;"$$NICENAME(TMGDFN)
 ;"$$FNAME(TMGDFN)
 ;"$$MNAME(TMGDFN)
 ;"$$LNAME(TMGDFN)
 ;"$$PHONENUM(TMGDFN)
 ;"$$GETTABLX(TMGDFN,LABEL,OUT,ARRAY) -- return a table compiled from prior notes.
 ;"TESTTABL 
 ;"$$MEDLIST(RESULT,TMGDFN)  -- RPC Entry point (TMG GET MED LIST).  Return a patient's med list
 ;"MEDARR(RESULT,TMGDFN,ARRAY,DT) -- Get MED LIST ARRAY
 ;"$$ALLERGY(TMGDFN) -- Get allergy list to populate TIU Object |TMG ALLERGY LIST|
 ;"$$PTPRPRO(TMGDFN) -- Returns patient's personal pronoun
 ;"$$PTPOPRO(TMGDFN) -- Returns patient's possessive pronoun
 ;"$$GETREACT(IEN120D8,COMMENTS,NOTHTML) --  Return either signs/symptoms (if COMMENTS=0) or comments (COMMENTS=1)
 ;"$$DETALRGY(TMGDFN) -- FIND AND RETURN DETAILED ALLERGY INFORMATION 
 ;"$$ROSALRGY(TMGRESULT,TMGDFN) --FIND AND RETURN DETAILED ALLERGY INFORMATION FOR THE ROS FORM 
 ;"$$WEIGHT(TMGDFN,TIU)  ;
 ;"$$BMI(TMGDFN,BMI,IDEALWTS,USEWTDT,WHY) --Return BMI
 ;"$$TMGVISDT(TIU)  --Visit date
 ;"$$WTTREND(TMGDFN,TIU) --return text showing patient's trend in change of weight.
 ;"$$MEANFLUS(TMGDFN) -- Return text showing missing items for meaningful use.
 ;"$$GETLMAMO(TMGDFN) --Return date of last mammogram
 ;"$$GETLCOLN(TMGDFN) --Return date of last colonoscopy
 ;"$$GETLADIR(TMGDFN) --Return date of last advance directives        
 ;"$$GETLBONE(TMGDFN) --Return date of bone density
 ;"$$PTPRPRO(TMGDFN) --Returns patient's personal pronoun
 ;"$$PTPOPRO(TMGDFN) --Returns patient's possessive pronoun
 ;"$$GETLTSH(TMGDFN)  --Return last lab data for TSH
 ;"$$GETLB12(TMGDFN)  --Return last lab data for Vitamin B-12
 ;"$$GETLVITD(TMGDFN)  --Return last lab data for Vitamin D
 ;"$$GETLTOBA(TMGDFN)  --Return last tobacco status
 ;"$$GETLEYEE(TMGDFN) --Return last diabetic eye exam
 ;"$$GETLFTEX(TMGDFN) --Return last diabetic foot exam
 ;"$$GETLGLAS(TMGDFN)  --Return last glaucoma screening
 ;"$$GETHFGRP(TMGDFN,HFGROUPIEN,TMGRESULTARR)  -- Return health factors for a patient by a given hf group
 ;"$$GETTIUOJ(TMGDFN,NAME) -- return tiu text object for patient
 ;"$$GETLURIN(TMGDFN,NUM) -- Return the last urine with comments
 ;"=======================================================================
 ;"PRIVATE FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"Dependancies :  TMGSTUT2 TMGTIUO3 TMGTIUO4 TMGTIUO6
 ;"                TIUL01 XLFDT TIULO XLFSTR
 ;"=======================================================================
 ;
ALLERGY(TMGDFN) ; ALLERGY LIST
  QUIT $$ALLERGY^TMGTIUO3(.TMGDFN)
  ;
GETREACT(IEN120D8,COMMENTS,NOTHTML)  ;
  QUIT $$GETREACT^TMGTIUO3(.IEN120D8,.COMMENTS,.NOTHTML)  
  ;
DETALRGY(TMGDFN)  ;
  QUIT $$DETALRGY^TMGTIUO3(.TMGDFN) 
  ;
ROSALRGY(TMGRESULT,TMGDFN)  ;
  QUIT $$ROSALRGY^TMGTIUO3(.TMGRESULT,.TMGDFN)  
  ;
VITALS(TMGDFN,TIU) ;
  ;"Purpose: Return a composite Vitals string
  ;"Input: TMGDFN -- the patient's unique ID (record#)
  ;"       TIU -- See documentation below.
  ;"Output: returns RESULT
  ;"note: Consider using $$GETVITLS^TMGGMRV1 in the future...
  QUIT $$ONEVITAL(.TMGDFN,.TIU,"ALL",1)
  ;                                                  
WEIGHT(TMGDFN,TIU)  ;         
  QUIT $$WEIGHT^TMGTIUO3(.TMGDFN,.TIU)
  ;
ONEVITAL(TMGDFN,TIU,TYPE,HTMLWRAP)    ;
  SET HTMLWRAP=+$G(HTMLWRAP)
  QUIT $$ONEVITAL^TMGTBL01(.TMGDFN,.TIU,.TYPE,HTMLWRAP)
  ;
BMI(TMGDFN,BMI,IDEALWTS,USEWTDT,WHY) ;"Return BMI
  QUIT $$BMI^TMGTIUO3(.TMGDFN,.BMI,.IDEALWTS,.USEWTDT,.WHY) ;"Return BMI
  ;
TMGVISDT(TIU)  ;" Visit date
  QUIT $$TMGVISDT^TMGTIUO3(.TIU)
  ;
FNAME(TMGDFN)  ;
  QUIT $$FNAME^TMGTIUO3(.TMGDFN)
  ;
MNAME(TMGDFN) ;
  QUIT $$MNAME^TMGTIUO3(.TMGDFN)
  ;
LNAME(TMGDFN) ;
  QUIT $$LNAME^TMGTIUO3(.TMGDFN)
  ;
NICENAME(TMGDFN) ;
  QUIT $$NICENAME^TMGTIUO3(.TMGDFN)
  ;
PHONENUM(TMGDFN) ;
  QUIT $$PHONENUM^TMGTIUO3(.TMGDFN)
  ;
WTTREND(TMGDFN,TIU) ;"Purpose: return text showing patient's trend in change of weight.
  QUIT $$WTTREND^TMGTIUO4(TMGDFN,.TIU)
  ;
GETTABLX(TMGDFN,LABEL,ARRAY,OPTION) ;"Purpose: A call point for TIU objects, to return a table comprised from prior notes.
  QUIT $$GETTABLX^TMGTIUO6(.TMGDFN,.LABEL,.ARRAY,.OPTION)
  ;
TESTTABL  ;
  NEW DIC,X,Y,TMGDFN,IEN22708,TABLENAME
  SET DIC=2,DIC(0)="MAEQ" DO ^DIC WRITE ! SET TMGDFN=+Y IF TMGDFN'>0 QUIT
  SET DIC=22708 DO ^DIC WRITE ! SET IEN22708=+Y,TABLENAME=$PIECE(Y,"^",2) IF IEN22708'>0 QUIT
  NEW ARRAY,RESULT
  DO INITPFIL^TMGMISC2("GETITEM^TMGTIUO8")
  SET RESULT=$$GETTABLX(TMGDFN,TABLENAME,.ARRAY)
  WRITE "Table output:",!,RESULT,!
  WRITE "Associated output array:",!
  ZWRITE ARRAY
  WRITE !
  DO SHOWRPT^TMGMISC2("GETITEM^TMGTIUO8")
  QUIT
  ;  
TESTMEDLIST ;
  NEW DIC,X,Y,TMGDFN,STR,ARR
  SET DIC=2,DIC(0)="MAEQ" DO ^DIC WRITE ! SET TMGDFN=+Y IF TMGDFN'>0 QUIT
  DO MEDLIST(.STR,TMGDFN,.ARR)
  WRITE STR,!
  QUIT
  ;
TESTTIUOBJ ; ;"Interact with user to select a patient and TIU TEXT OBJECT and test it.
  NEW DIC,X,Y,TMGDFN,STR,ARR,IEN8925D1
  SET DIC=2,DIC(0)="MAEQ" DO ^DIC WRITE ! SET TMGDFN=+Y IF TMGDFN'>0 DO  QUIT
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
PRVPROBS(TMGDFN)  ;"
  NEW TMGRESULT 
  SET TMGRESULT=$$GETTABLX^TMGTIUOJ(+$G(TMGDFN),"[FOLLOWUP ITEMS]")
  SET TMGRESULT=$$REPLSTR^TMGSTUT3(TMGRESULT,$C(13,10),"<br>")
  QUIT TMGRESULT
  ;"
MEDLIST(RESULT,TMGDFN,ARRAY,DT,OPTION)  ;"Purpose: RPC (TMG GET MED LIST) to return a patient's med list
  ;"NOTE!!: It is not effecient to call this just to get ARRAY.  
  ;"        Use MEDARR() below instead!
  ;"SET OPTION("USEOLDMETHOD")=1  ;"TEMP!, REMOVE LATER.
  DO GTMEDLST^TMGTIUO8(.RESULT,.TMGDFN,.ARRAY,.DT,.OPTION)
  QUIT
  ;
MEDARR(RESULT,TMGDFN,ARRAY,DT,OPTION) ;"Get MED LIST ARRAY
  SET RESULT=1  ;"Does nothing.  I included to keep function signature same as MEDLIST()
  IF $GET(DT)>0 SET OPTION("DT")=DT
  ;"SET OPTION("USEOLDMETHOD")=1  ;"TEMP!, REMOVE LATER.
  DO PRIORRXT^TMGTIUO8(TMGDFN,48,.ARRAY,1,.OPTION)
  QUIT
  ;
MEANFLUS(TMGDFN) ;"Return text showing missing items for meaningful use.
  QUIT $$MEANFLOJ^TMGC0Q06(TMGDFN)
  ;
GETLMAMO(TMGDFN) ;"Return date of last mammogram
  QUIT $$GETLMAMO^TMGTBL01(.TMGDFN)
  ;
GETLCOLN(TMGDFN) ;"Return date of last colonoscopy
  QUIT $$GETLCOLN^TMGTBL01(.TMGDFN)
  ;
GETLADIR(TMGDFN) ;"Return date of last advance directives        
  QUIT $$GETLADIR^TMGTBL01(.TMGDFN)
  ;
GETLBONE(TMGDFN) ;"Return date of bone density
  QUIT $$GETLBONE^TMGTBL01(.TMGDFN)
  ;
PTPRPRO(TMGDFN)  ;Returns patient's personal pronoun
  QUIT $$PTPRPRO^TMGTIUO3(TMGDFN) 
  ;
PTPOPRO(TMGDFN)  ;Returns patient's possessive pronoun
  QUIT $$PTPOPRO^TMGTIUO3(TMGDFN)  
  ;
GETLTSH(TMGDFN)  ;"Return last lab data for TSH
  QUIT $$GETLTSH^TMGTBL01(.TMGDFN)
  ;
GETLB12(TMGDFN)  ;"Return last lab data for Vitamin B-12
  QUIT $$GETLB12^TMGTBL01(.TMGDFN)
  ;
GETIVITD(TMGDFN)  ;"Return Vit-D level for inline table
  QUIT $PIECE($$GETLVITD(TMGDFN),"[T]",1)
  ;
GETIMAG(TMGDFN)   ;"Return Magnesium level for inline table
  QUIT "NOT FINISHED"
  ;
GETLVITD(TMGDFN)  ;"Return last lab data for Vitamin D
  QUIT $$GETLVITD^TMGTIUO3(.TMGDFN)  ;"Return last lab data for Vitamin D
  ;
GETLTOBA(TMGDFN)  ;"Return last tobacco status
  QUIT $$GETLTOBA^TMGTBL01(.TMGDFN)
  ;
GETLEYEE(TMGDFN) ;"Return last diabetic eye exam
  QUIT $$GETLEYEE^TMGTBL01(.TMGDFN)
  ;
GETLFTEX(TMGDFN) ;"Return last diabetic foot exam
  QUIT $$GETLFTEX^TMGTBL01(.TMGDFN)
  ;
GETLGLAS(TMGDFN)  ;"Return last glaucoma screening
  QUIT $$GETLGLAS^TMGTBL01(.TMGDFN)
  ;"
GETHFGRP(TMGDFN,HFGROUPIEN,TMGRESULTARR)  ;"Return health factors for a patient by a given hf group
  DO GETHFGRP^TMGTBL01(.TMGDFN,.HFGROUPIEN,.TMGRESULTARR)
  QUIT
  ;        
GETTIUOJ(TMGDFN,NAME) ;" return tiu text object for patient
  NEW RESULT SET RESULT=$$BOIL^TIUSRVD("|"_NAME_"|")
  QUIT RESULT
  ;
ADGIVEN(TMGDFN)  ;"Return the health factor date for when the last CP papers were given
  QUIT $$ADGIVEN^TMGTBL01(.TMGDFN)
  ;
GETLLAB(TMGDFN,LABNUM,NUM,DTONLY)   ;"Return the last urine culture
  QUIT $$GETLLAB^TMGTBL01(.TMGDFN,.LABNUM,.NUM,.DTONLY)
  ;
ALLHFTBL(TMGDFN)  ;"Return an HTML table containing all health factors
  QUIT $$ALLHFTBL^TMGTBL01(.TMGDFN)
  ;
FUITEMS(TMGDFN)  ;"Return the followup table if data is contained
  QUIT $$FUITEMS^TMGTIUO3(.TMGDFN)  
  ;
LASTHPI(TMGDFN,AWV)  ;"Return the last HPI section.  Called by TIU TEXT OBJECT 'TMG LAST HPI'
  SET AWV=+$G(AWV)
  QUIT $$LASTHPI^TMGTIUP2(.TMGDFN,AWV)  ;"Moved to TMGTIUP2 to minimize file size 
  ;
ADMINDOC(TMGRESULT);
  NEW IEN SET IEN=0
  FOR  SET IEN=$ORDER(^TMG(22737,"B",IEN)) QUIT:IEN'>0  DO
  . NEW NAME SET NAME=$PIECE($GET(^TIU(8925.1,IEN,0)),"^",3)
  . SET TMGRESULT(IEN)=NAME_";"
  QUIT
  ;
XTRAFORM(TMGRESULT,TMGDFN)  ;
  QUIT $$XTRAFORM^TMGTIUO3(.TMGRESULT,.TMGDFN) 
  ;
NEEDPSA(TMGRESULT,TMGDFN)  ;
  QUIT $$NEEDPSA^TMGTIUO3(.TMGRESULT,.TMGDFN)  
  ;
GETSTATS(STATUS,TMGRESULT) ;
  QUIT $$GETSTATS^TMGTIUO3(.STATUS,.TMGRESULT) 
  ;
UNSIGNED(TMGRESULT)  ;
  QUIT $$UNSIGNED^TMGTIUO3(.TMGRESULT)  
  ;
ADDLSIGN(TMGRESULT)  ;
  QUIT $$ADDLSIGN^TMGTIUO3(.TMGRESULT)  
  ;
LASTOPTH(TMGDFN)  ;
  QUIT $$LASTOPTH^TMGTIUO3(.TMGDFN) 
  ;      
ADVNOTES(TMGDFN)  ;"
  NEW TMGRESULT SET TMGRESULT=""
  NEW ADVARRAY,NOTEDATE,COUNT
  SET NOTEDATE=9999999,COUNT=0
  DO TIUDATES^TMGPXR01(TMGDFN,"ADVANCE DIRECTIVE (IMAGE)",.ADVARRAY)
  DO TIUDATES^TMGPXR01(TMGDFN,"ADVANCE DIRECTIVE",.ADVARRAY)
  IF $DATA(ADVARRAY) DO
  . SET TMGRESULT=TMGRESULT_"ADVANCE DIR NOTE DATES : "
  . FOR  SET NOTEDATE=$ORDER(ADVARRAY(NOTEDATE),-1) QUIT:(NOTEDATE'>0)!(COUNT>3)  DO
  . . NEW Y SET Y=NOTEDATE
  . . X ^DD("DD")
  . . SET TMGRESULT=TMGRESULT_$PIECE(Y,"@",1)_" "
  ELSE  DO
  . SET TMGRESULT="NO ADVANCE DIRECTIVE NOTES FOUND"
  QUIT TMGRESULT
  ;"
  