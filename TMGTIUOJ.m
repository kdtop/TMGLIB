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
 ;"$$MEDLIST(RESULT,DFN)  -- RPC Entry point (TMG GET MED LIST).  Return a patient's med list
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
        ;"Purpose: Get allergy list to populate TIU Object |TMG ALLERGY LIST|
        ;"Input: DFN
        ;"Result: Comma delimited list of allergies, 
        ;"        or ***NEEDS ALLERGY ASSESSMENT*** (if no allergy assessment was found)
        ;"        or Patient has answered NKA (if No Known Allergies was returned)
        ;"
        NEW RESULT SET RESULT=""
        NEW ALRGYL,ALDESC,ALCNT
        ;"Get allergy list
        DO LIST^ORQQAL(.ALRGYL,DFN)
        SET ALCNT=""
        ;"Format allergy list into string
        FOR  SET ALCNT=$O(ALRGYL(ALCNT)) QUIT:ALCNT=""  DO
        . SET ALDESC=$P(ALRGYL(ALCNT),"^",2) ;allergy description
        . IF RESULT="" SET RESULT=ALDESC
        . ELSE  SET RESULT=RESULT_", "_ALDESC
        ;"
        ;"If no assessment done, alert provider
        IF (RESULT["No Allergy Assessment")!(RESULT="") SET RESULT="***NEEDS ALLERGY ASSESSMENT***"
        ;"If NKA, rephrase
        IF RESULT["No Known Allergies" SET RESULT="Patient has answered NKA"
        QUIT RESULT
        ;
GETREACT(IEN120D8,COMMENTS,NOTHTML)  ;
        ;" Purpose: Return either signs/symptoms (if COMMENTS=0) or comments (COMMENTS=1)
        NEW REACTS,REACTIEN,SYMPIEN,SYMPNAME,LINENUM,COMMENT,HTMLBTAG,HTMLETAG,PREFIX
        SET NOTHTML=$GET(NOTHTML,0)
        IF NOTHTML=0 DO
        . SET HTMLBTAG="{HTML:<I>}"
        . SET HTMLETAG="{HTML:</I>}"
        . SET PREFIX="Signs/Sym: "
        ELSE  DO
        . SET HTMLBTAG=""
        . SET HTMLETAG=""
        . SET PREFIX=""
        SET REACTS="",REACTIEN=0
        IF IEN120D8="" QUIT REACTS
        IF COMMENTS=0 DO
        . FOR  SET REACTIEN=$ORDER(^GMR(120.8,IEN120D8,10,REACTIEN)) QUIT:REACTIEN'>0  DO
        . . SET SYMPIEN=$PIECE($GET(^GMR(120.8,IEN120D8,10,REACTIEN,0)),"^",1)
        . . SET SYMPNAME=$PIECE($GET(^GMRD(120.83,SYMPIEN,0)),"^",1)
        . . IF REACTS="" SET REACTS=PREFIX_SYMPNAME
        . . ELSE  SET REACTS=REACTS_","_SYMPNAME
        ELSE  DO
        . SET LINENUM=0
        . SET COMMENT=""
        . FOR  SET LINENUM=$ORDER(^GMR(120.8,IEN120D8,26,1,2,LINENUM)) QUIT:LINENUM'>0  DO 
        . . SET COMMENT=$GET(^GMR(120.8,IEN120D8,26,1,2,LINENUM,0))
        . . IF REACTS="" SET REACTS=PREFIX_HTMLBTAG_COMMENT
        . . ELSE  SET REACTS=REACTS_","_COMMENT
        . IF REACTS'="" SET REACTS=REACTS_HTMLETAG
        QUIT REACTS
        ;
DETALRGY(DFN)  ;
        ;" PURPOSE: FIND AND RETURN DETAILED ALLERGY INFORMATION 
        NEW RESULT,IEN120D8,ALRGYARR,LINE,IEN,Y,REACTIONS
        SET IEN120D8=0,RESULT="{HTML:<BR>}"
        SET IEN=0
        FOR  SET IEN120D8=$ORDER(^GMR(120.8,"B",DFN,IEN120D8)) QUIT:IEN120D8'>0  DO
        . ;WRITE $GET(^GMR(120.8,IEN120D8,0)),!
        . IF $D(^GMR(120.8,IEN120D8,"ER")) QUIT  ;"Exclude if Entered In Error
        . SET LINE=$GET(^GMR(120.8,IEN120D8,0))
        . SET Y=$P(LINE,"^",4)  ;date
        . X ^DD("DD")
        . SET REACTIONS=$$GETREACT(IEN120D8,0)
        . IF REACTIONS="" DO
        . . SET REACTIONS=$$GETREACT(IEN120D8,1)
        . SET RESULT=RESULT_"{HTML:<B>}"_$P(LINE,"^",2)_"{HTML:</B><FONT SIZE=""-1"">} (Entered: "_$P(Y,"@",1)_"){HTML:</FONT>}"_REACTIONS_"{HTML:<BR>}"   ;"$CHAR(13)_$CHAR(10)
        . SET IEN=IEN+1
        IF RESULT="{HTML:<BR>}" SET RESULT="No Known Allergies"
        QUIT RESULT
        ;
ROSALRGY(TMGRESULT,DFN)  ;
        ;" PURPOSE: FIND AND RETURN DETAILED ALLERGY INFORMATION
        ;"          FOR THE ROS FORM, THROUGH "TMG GET ROS ALLERGY LIST" RPC 
        NEW IEN120D8,ALRGYARR,LINE,IEN,Y,REACTIONS
        SET IEN120D8=0,RESULT=""
        FOR IEN=1:1:7 SET ALRGYARR(IEN)=","
        SET IEN=0
        FOR  SET IEN120D8=$ORDER(^GMR(120.8,"B",DFN,IEN120D8)) QUIT:IEN120D8'>0  DO
        . IF $D(^GMR(120.8,IEN120D8,"ER")) QUIT  ;"Exclude if Entered In Error
        . SET LINE=$GET(^GMR(120.8,IEN120D8,0))
        . SET LINE=$TR(LINE,",",";")
        . SET REACTIONS=$$GETREACT(IEN120D8,0,1)
        . IF REACTIONS="" DO
        . . SET REACTIONS=$$GETREACT(IEN120D8,1,1)
        . SET REACTIONS=$TR(REACTIONS,",",";")
        . SET IEN=IEN+1
        . IF IEN=8 SET ALRGYARR(7)="(MORE... SEE COMPLETE LIST),"
        . ELSE  IF IEN>8 QUIT
        . ELSE  SET ALRGYARR(IEN)=$P(LINE,"^",2)_","_REACTIONS
        SET IEN=0,TMGRESULT=""
        FOR  SET IEN=$ORDER(ALRGYARR(IEN)) QUIT:IEN'>0  DO
        . SET LINE=$GET(ALRGYARR(IEN))
        . SET TMGRESULT=TMGRESULT_LINE
        . IF IEN<7 SET TMGRESULT=TMGRESULT_","
        IF TMGRESULT="" SET TMGRESULT=",No Known Allergies,,,,,,,,,,,,"
        QUIT
        ;
VITALS(DFN,TIU) ;
        ;"Purpose: Return a composite Vitals string
        ;"Input: DFN -- the patient's unique ID (record#)
        ;"       TIU -- See documentation below.
        ;"Output: returns RESULT
        ;
        ;"note: Consider using $$GETVITLS^TMGGMRV1 in the future...
        QUIT $$ONEVITAL(.DFN,.TIU,"ALL")
        ;
WEIGHT(DFN,TIU)  ;
        ;"Purpose: Return a string of the weight values
        ;"Input: DFN -- the patient's unique ID (record#)
        ;"       TIU -- See documentation below.
        ;"Output: returns RESULT
        NEW STRING
        SET STRING=$$ONEVITAL(.DFN,.TIU,"WT")_" "_$$ONEVITAL(.DFN,.TIU,"BMI-CMT")
        IF STRING["Wt " SET STRING=$PIECE(STRING,"Wt ",2,999)
        SET STRING="Wt "_STRING
        SET STRING=$TR(STRING,$C(13,10))
        SET STRING=$$REPLSTR^TMGSTUT3(STRING,"         ","")
        SET STRING=$$REPLSTR^TMGSTUT3(STRING,"[See vital-signs documented in chart]","")
        ;"FINISH PARSING STRING
        SET ^TMP("EDDIE","WEIGHT")=STRING
        QUIT STRING
        ;"    
ONEVITAL(DFN,TIU,TYPE)    ;
        QUIT $$ONEVITAL^TMGTBL01(.DFN,.TIU,.TYPE)
        ;
BMI(DFN,BMI,IDEALWTS,USEWTDT,WHY) ;"Return BMI
        ;"Input: DFN--PATIENT IEN
        ;"       BMI  -- OPTIONAL, AN OUT PARAMETER.  Numeric BMI
        ;"       IDEALWTS -- OPTIONAL, AN OUT PARAMETER.  Ideal weight range-- Format : MinWt^MaxWt^PtWt
        ;"       USEWTDT -- OPTIONAL.  DEFAULT=0.  IF 1, then use WT date rather than oldest date
        ;"                     Also, IF 1, then WT date is used to calculate patient age, which
        ;"                     is used to determine ideal weight range.
        ;"       WHY -- OPTIONAL.  PASS BY REFERENCE.  AN OUT PARAMETER
        ;"               If BMI is 0, APPENDS string with reason
        ;"Result: BMI string. Format:  'BMI (date of oldest measurement)'
        SET WHY=$GET(WHY)
        NEW WT SET WT=$$WEIGHT^TIULO(DFN)
        IF WT="" SET WHY=WHY_$$NURSEPRE^TMGC0QT1()_"No recorded weight found.] " ;"elh 7/12/13
        NEW HT SET HT=$$HEIGHT^TIULO(DFN)
        IF HT="" SET WHY=WHY_$$NURSEPRE^TMGC0QT1()_"No recorded height found.] " ;"elh 7/12/13
        NEW ONDT,TEMPWT
        IF (+$GET(USEWTDT)=1)&(WT'="") SET TEMPWT=$$PARSEWT^TMGTIUO4(WT,.ONDT)
        ELSE  SET ONDT=$$NOW^XLFDT\1
        NEW PTAGE SET PTAGE=$$PTAGE^TMGTIUO3(DFN,ONDT) ;
        QUIT $$BMI^TMGTIUO4(PTAGE,HT,WT,.BMI,.IDEALWTS,.USEWTDT)
        ;
TMGVISDT(TIU)  ;" Visit date
        QUIT $$TMGVISDT^TMGTIUO3(.TIU)
        ;
FNAME(DFN)  ;
        QUIT $$FNAME^TMGTIUO3(DFN)
        ;
MNAME(DFN) ;
        QUIT $$MNAME^TMGTIUO3(DFN)
        ;
LNAME(DFN) ;
        QUIT $$LNAME^TMGTIUO3(DFN)
        ;
NICENAME(DFN) ;
        QUIT $$NICENAME^TMGTIUO3(DFN)
        ;
PHONENUM(DFN) ;
        QUIT $$PHONENUM^TMGTIUO3(DFN)
        ;
WTTREND(DFN,TIU) ;"Purpose: return text showing patient's trend in change of weight.
        QUIT $$WTTREND^TMGTIUO4(DFN,.TIU)
        ;
GETTABLX(DFN,LABEL,ARRAY,OPTION) ;"Purpose: A call point for TIU objects, to return a table comprised from prior notes.
        ;"Input ARRAY is optional.  Supply to get back array of table.
        QUIT $$GETTABLX^TMGTIUO6(.DFN,.LABEL,.ARRAY,.OPTION)
        ;
MEDLIST(RESULT,DFN,ARRAY,DT)  ;"Purpose: RPC (TMG GET MED LIST) to return a patient's med list
        ;"Input ARRAY is optional.  Supply to get back array of table. Not used by RPC call.
        ;"      DT -- OPTIONAL.  If supplied, then get med list AS OF the specified FM DT
        NEW OPTION IF $DATA(DT)#10 SET OPTION("DT")=DT
        SET RESULT=$$GETTABLX(DFN,"MEDICATIONS",.ARRAY,.OPTION) 
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
        NEW GENDER,TMGRESULT
        SET TMGRESULT=""
        SET GENDER=$$SEX^TIULO(DFN)
        IF GENDER="FEMALE" SET TMGRESULT="she"
        ELSE  SET TMGRESULT="he"
        QUIT TMGRESULT
        ;
PTPOPRO(DFN)  ;Returns patient's possessive pronoun
        NEW GENDER,TMGRESULT
        SET TMGRESULT=""
        SET GENDER=$$SEX^TIULO(DFN)
        IF GENDER="FEMALE" SET TMGRESULT="her"
        ELSE  SET TMGRESULT="his"
        QUIT TMGRESULT
        ;
GETLTSH(DFN)  ;"Return last lab data for TSH
        QUIT $$GETLTSH^TMGTBL01(.DFN)
        ;"
GETLB12(DFN)  ;"Return last lab data for Vitamin B-12
        QUIT $$GETLB12^TMGTBL01(.DFN)
        ;"
GETIVITD(DFN)  ;"Return Vit-D level for inline table
        QUIT $PIECE($$GETLVITD(DFN),"[T]",1)
        ;"
GETIMAG(DFN)   ;"Return Magnesium level for inline table
        QUIT "NOT FINISHED"
        ;"
GETLVITD(DFN)  ;"Return last lab data for Vitamin D
        NEW TMGRESULT,RESULTARR
        SET TMGRESULT=$$GETTABL1^TMGTIUO6(DFN,"[STUDIES]",.RESULTARR)
        SET TMGRESULT=$GET(RESULTARR("KEY-VALUE","VIT-D"))_" [T]"
        IF TMGRESULT=" [T]" SET TMGRESULT="NO DATA FOUND"
        QUIT TMGRESULT
        ;"
GETLTOBA(DFN)  ;"Return last tobacco status
        QUIT $$GETLTOBA^TMGTBL01(.DFN)
        ;"
GETLEYEE(TMGDFN) ;"Return last diabetic eye exam
        QUIT $$GETLEYEE^TMGTBL01(.TMGDFN)
        ;"
GETLFTEX(TMGDFN) ;"Return last diabetic foot exam
        QUIT $$GETLFTEX^TMGTBL01(.TMGDFN)
        ;"
GETLGLAS(DFN)  ;"Return last glaucoma screening
        QUIT $$GETLGLAS^TMGTBL01(DFN)
        ;"
GETHFGRP(DFN,HFGROUPIEN,TMGRESULTARR)  ;"Return health factors for a patient by a given hf group
        DO GETHFGRP^TMGTBL01(.DFN,.HFGROUPIEN,.TMGRESULTARR)
        QUIT
        ;"        
GETTIUOJ(DFN,NAME) ;" return tiu text object for patient
        NEW RESULT SET RESULT=$$BOIL^TIUSRVD("|"_NAME_"|")
        QUIT RESULT
        ;
ADGIVEN(DFN)  ;"Return the health factor date for when the last CP papers were given
        QUIT $$ADGIVEN^TMGTBL01(.DFN)
        ;"
GETLLAB(DFN,LABNUM,NUM)   ;"Return the last urine culture
        QUIT $$GETLLAB^TMGTBL01(.DFN,.LABNUM,.NUM)
        ;"
ALLHFTBL(DFN)  ;"Return an HTML table containing all health factors
        QUIT $$ALLHFTBL^TMGTBL01(DFN)
        ;"
FUITEMS(DFN)  ;"Return the followup table if data is contained
        NEW X
        S X=$$GETTABLX^TMGTIUOJ(+$G(DFN),"[FOLLOWUP ITEMS]")      
        NEW TEMP SET TEMP=$P(X,$C(13,10),2)
        SET TEMP=$$TRIM^XLFSTR(TEMP)
        IF TEMP="" SET X=TEMP
        QUIT X
        ;"
LASTHPI(DFN)  ;"Return the last HPI section
        QUIT $$LASTHPI^TMGTIUP2(DFN)  ;"Moved to TMGTIUP2 to minimize file size 
        ;
ADMINDOC(TMGRESULT);
        NEW IEN SET IEN=0
        FOR  SET IEN=$ORDER(^TMG(22737,"B",IEN)) QUIT:IEN'>0  DO
        . NEW NAME SET NAME=$PIECE($GET(^TIU(8925.1,IEN,0)),"^",1)
        . SET TMGRESULT(IEN)=NAME_";"
        QUIT
        ;"
XTRAFORM(TMGRESULT,DFN)  ;"
        ;"Purpose: This function will take the DFN and return an array
        ;"         of all documents that the user should have printed for
        ;"         them.
        ;"Input: TMGRESULT -- RPC output
        ;"       TMGRESULT(0)=0 or # of documents due
        ;"       TMGRESULT(#)=Name  <- This name MUST correspond with the
        ;"                       name of the CSV as well as the DOC files
        SET TMGRESULT(0)=0
        IF $$NEEDPSA(.RESULT,DFN) DO
        . SET TMGRESULT(0)=$GET(TMGRESULT(0))+1
        . NEW IDX SET IDX=$GET(TMGRESULT(0))
        . SET TMGRESULT(IDX)="PSA"
        QUIT
        ;"
NEEDPSA(TMGRESULT,DFN)  ;"
        ;"Purpose: To determine if the patient needs a PSA handout
        SET TMGRESULT=0
        NEW X DO NOW^%DTC
        NEW REMRESULT SET REMRESULT=$$DOREM^TMGPXR03(DFN,263,5,X)
        IF REMRESULT["DUE NOW" SET TMGRESULT=1
        QUIT TMGRESULT
        ;"
GETSTATS(STATUS,TMGRESULT) ;"
        ;"Purpose: Finds all TIU notes with a given status
        ;"Input: STATUS - IEN of the status to search for
        ;"       TMGRESULT(Return array) - TMGRESULT(IEN,PATIENT IEN,AUTHOR IEN)=""
        SET STATUS=+$GET(STATUS)
        IF STATUS<1 QUIT
        NEW NOTEIEN SET NOTEIEN=0
        NEW THISSTATUS
        FOR  SET NOTEIEN=$ORDER(^TIU(8925,NOTEIEN)) QUIT:NOTEIEN'>0  DO
        . SET THISSTATUS=$PIECE($GET(^TIU(8925,NOTEIEN,0)),"^",5)
        . IF THISSTATUS=STATUS DO
        . . NEW AUTHOR SET AUTHOR=+$PIECE($GET(^TIU(8925,NOTEIEN,12)),"^",2)
        . . NEW PATIENT SET PATIENT=+$PIECE($GET(^TIU(8925,NOTEIEN,0)),"^",2)
        . . SET TMGRESULT(NOTEIEN,PATIENT,AUTHOR)=""
        QUIT
        ;"
UNSIGNED(TMGRESULT)  ;"
        ;"Purpose: find all unsigned notes
        ;"Input: TMGRESULT(Return array) - as above
        DO GETSTATS(5,.TMGRESULT)
        QUIT
        ;"
ADDLSIGN(TMGRESULT)  ;"
        ;"Purpose: To find all notes where the add'l signer hasn't signed
        ;"Input: TMGRESULT(Return array) - as above
        NEW IEN SET IEN=0
        FOR  SET IEN=$ORDER(^TIU(8925.7,IEN)) QUIT:IEN'>0  DO
        . NEW SIGNDT SET SIGNDT=+$PIECE($GET(^TIU(8925.7,IEN,0)),"^",4)
        . IF SIGNDT'>0 DO
        . . NEW NOTEIEN,EXPECTEDIEN,PATIENT
        . . SET NOTEIEN=+$PIECE($GET(^TIU(8925.7,IEN,0)),"^",1)
        . . IF +$PIECE($GET(^TIU(8925,NOTEIEN,0)),"^",5)=15 QUIT  ;"Don't include retracted notes
        . . SET EXPECTEDIEN=+$PIECE($GET(^TIU(8925.7,IEN,0)),"^",3)
        . . SET PATIENT=+$PIECE($GET(^TIU(8925,NOTEIEN,0)),"^",2)
        . . SET TMGRESULT(NOTEIEN,PATIENT,EXPECTEDIEN)=""
        . . ;"WRITE $PIECE($GET(^VA(200,EXPECTEDIEN,0)),"^",1)," NEEDS TO SIGN ",NOTEIEN,!
        QUIT
        ;"
LASTOPTH(DFN)  ;"
        ;"Purpose: To return the patient's last opthalmology note titles
        NEW TMGRESULT
        NEW EYEEARRAY,NOTEDATE,COUNT
        SET NOTEDATE=9999999,COUNT=0
        DO TIUDATES^TMGPXR01(TMGDFN,"OPHTHO / OPTO / EYE CONSULTANT NOTE (IMAGE)",.EYEEARRAY)
        IF $DATA(EYEEARRAY) DO 
        . SET TMGRESULT=TMGRESULT_"OPHTHO NOTE DATES : "
        . FOR  SET NOTEDATE=$ORDER(EYEEARRAY(NOTEDATE),-1) QUIT:(NOTEDATE'>0)!(COUNT>3)  DO
        . . NEW Y
        . . SET Y=NOTEDATE
        . . X ^DD("DD")
        . . SET TMGRESULT=TMGRESULT_$PIECE(Y,"@",1)_" "
        ELSE  DO
        . SET TMGRESULT="NO OPHTHALMOLOGY NOTES FOUND"
        QUIT TMGRESULT
        ;"