TMGTIUO3 ;TMG/kst-Text objects for use in CPRS ; 10/19/16
         ;;1.0;TMG-LIB;**1,17**;10/24/10
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
 ;"$$AGEONDAT(DFN,REFDATE)  -- Returns age on a given date
 ;"VITARR(OUT,DFN,SDT,EDT,VERBOSE)  --GET ARRAY OF VITALS
 ;
 ;"=======================================================================
 ;"PRIVATE FUNCTIONS
 ;"=======================================================================
 ;"ADDVITAL(RESULT,STR,LABEL,CURDT,NOTEDT,FORCESHOW) -- format and add a vital set, wrapping line IF needed.
 ;"ADDPCTLE(RESULT,TYPE,VALUE,PTAGE,GENDER) --add percentile of vital measurement, IF possible.
 ;"FORMATVT(RESULT,STR,LABEL,CURDT,NOTEDT) -- remove redundant text in formating Vitals
 ;"DELIFOLD(LABEL,AGE,VALUE) --remove vitals that re too old to be relevent. e.g. pulse from 2 months ago.
 ;"REMOVEDT(STR,DT) --remove a date-Time string, and return in DT
 ;"$$REMOVTIM(DT) --remove the time from a date/time string
 ;"$$FORMATHT(HEIGHTSTR,PTAGE) remove centimeters from patient's height for adults
 ;"DATEDELT(REFDATE,DT) --  determine the number of days between REFDATE and DT
 ;"TMGVISDT(TIU)  Return a string for date of visit
 ;"PTAGE(DFN,NOTEDT) --return patient's AGE (in years) on date of note
 ;"HC(DFN,HC) --  Return formated head circumference reading
 ;"LASTHC(DFN) --Return the patient's last head circumference
 ;"FNAME(DFN) -- Return Patient's first name
 ;"MNAME(DFN) -- Return Patient's middle name(s)
 ;"LNAME(DFN)-- Return Patient's last name
 ;"NICENAME(DFN) -- Return Patient's name format: Firstname Middlename Lastname
 ;"PHONENUM(DFN) -- return the patient's phone number
 ;"BMI(DFN,BMI,IDEALWTS,USEWTDT,WHY) -- Return BMI
 ;"WEIGHT(DFN,TIU) -- Return a string of the weight values 
 ;"ALLERGY(DFN) ; ALLERGY LIST -- Get allergy list to populate TIU Object |TMG ALLERGY LIST|
 ;"GETREACT(IEN120D8,COMMENTS,NOTHTML)  -- Return either signs/symptoms (if COMMENTS=0) or comments (COMMENTS=1)
 ;"DETALRGY(DFN) -- FIND AND RETURN DETAILED ALLERGY INFORMATION
 ;"ROSALRGY(TMGRESULT,DFN) -- RETURN DETAILED ALLERGY INFORMATION FOR THE ROS FORM, THROUGH "TMG GET ROS ALLERGY LIST" RPC
 ;"PTPRPRO(DFN)  ;Returns patient's personal pronoun
 ;"PTPOPRO(DFN)  ;Returns patient's possessive pronoun 
 ;"GETLVITD(DFN) ;Return last lab data for Vitamin D
 ;"FUITEMS(DFN)  ;Return the followup table if data is contained 
 ;"XTRAFORM(TMGRESULT,DFN) -- return all documents that the user should have printed for them.
 ;"NEEDPSA(TMGRESULT,DFN) -- determine if the patient needs a PSA handout
 ;"GETSTATS(STATUS,TMGRESULT) -- Finds all TIU notes with a given status
 ;"UNSIGNED(TMGRESULT)  - find all unsigned notes 
 ;"ADDLSIGN(TMGRESULT)  -- find all notes where the add'l signer hasn't signed
 ;"LASTOPTH(DFN) -- return the patient's last opthalmology note titles 
 ;"ENSURE(ARRAY,KEY,PIVOT,VALUE) --add one (empty) entry, IF a value for this doesn't already exist.
 ;"=======================================================================
 ;"Dependancies : TMGGRC1, XLFSTR, ^%DT, XLFDT, TIULS, %DTC, DIQ TMGGRC2 TMGSTUT2,
 ;"=======================================================================
 ;
ADDVITAL(RESULT,STR,LABEL,CURDT,NOTEDT,FORCESHOW,PTAGE,EXCLUDEARR) ;
  ;"Purpose: To format and add a vital set, wrapping line if needed.
  ;"Input: RESULT -- PASS BY REFERENCE .. the cumulative string
  ;"         STR -- the string value result to add
  ;"         LABEL -- the text label
  ;"              Labels used: T, BP, R, P, Wt, Ht, HC, BMI,
  ;"         CURDT -- the last DT string shown
  ;"         NOTEDT -- [optional] DT string of date of note
  ;"                        If provided, then the date of the vital sign must equal NOTEDT, or
  ;"                        "" is returned (Unless FORCESHOW=1)
  ;"         FORCESHOW -- [optional] 1: Will force a return result, IF otherwise wouldn't be shown
  ;"         PTAGE -- [optional], default is 99-- age in YRS of patient
  ;"         EXCLUDEARR -- [optional].  PASS BY REFERENCE.  If a particular
  ;"              vital is too old for inclusion, then this is array is filled
  ;"                e.g. EXCLUDEARR("T")=1
  ;"Results: none (changes are passed back in result)
  NEW TEMPS SET TEMPS=""
  DO FORMATVT(.TEMPS,.STR,.LABEL,.CURDT,.NOTEDT,.FORCESHOW,.PTAGE,.EXCLUDEARR)
  IF (TEMPS'="")&(RESULT'="") SET RESULT=RESULT_"; "
  SET RESULT=RESULT_TEMPS
  QUIT
  ;
ADDPCTLE(RESULT,TYPE,VALUE,PTAGE,GENDER) ;
  ;"Purpose: To add percentile of vital measurement, IF possible.
  ;"Input:   RESULT -- PASS BY REFERENCE .. the cumulative string
  ;"         TYPE -- the type of percentile to add: 'Ht','Wt','HC', 'WtLen'
  ;"         VALUE -- the value of the vital sign, in metric
  ;"         PTAGE -- The patients age *In Years*
  ;"         gender -- must by 'M' or 'F'
  ;"Results: none (changes are passed back in RESULT)
  SET TYPE=$GET(TYPE)
  SET VALUE=$GET(VALUE)
  NEW TEMPS SET TEMPS=""
  ;
  IF TYPE="Ht" DO
  . SET VALUE=$PIECE($PIECE(VALUE,"[",2),"]",1)
  . SET VALUE=+VALUE
  . SET TEMPS=$$LENPCTL^TMGGRC1(.PTAGE,.GENDER,VALUE)
  ELSE  IF TYPE="Wt" DO
  . SET VALUE=$PIECE($PIECE(VALUE,"[",2),"]",1)
  . SET VALUE=+VALUE
  . SET TEMPS=$$WTPCTL^TMGGRC1(.PTAGE,.GENDER,VALUE)
  ELSE  IF TYPE="BMI" DO
  . SET TEMPS=$$BMIPCTL^TMGGRC1(.PTAGE,.GENDER,VALUE)
  ELSE  IF TYPE="HC" DO
  . SET TEMPS=$$HCPCTL^TMGGRC1(.PTAGE,.GENDER,VALUE)
  ELSE  IF TYPE="WtLen" DO
  . SET TEMPS=$$WTLENPCT^TMGGRC1(.PTAGE,.GENDER,VALUE)
  ;
  ;"SET RESULT=$$ADDWRAP^TMGSTUTL(RESULT,TEMPS,60,INDENTSTR)
  IF (TEMPS'="")&(RESULT'="") SET RESULT=RESULT_", "
  SET RESULT=RESULT_TEMPS ;
  QUIT
  ;
FORMATVT(OUTS,STR,LABEL,CURDT,NOTEDT,FORCESHOW,PTAGE,EXCLUDEARR) ;"Format Vitals
  ;"Note: Consider using $$GETVITLS^TMGGMRV1 in the future to avoid need for this function...
  ;"Purpose: To remove redundant text in formating Vitals
  ;"Input:   OUTS -- PASS BY REFERENCE .. the cumulative string
  ;"         STR -- the string value result to add
  ;"         LABEL -- the text label
  ;"              Labels used: T, BP, R, P, Wt, Ht, HC, BMI,
  ;"         CURDT -- the last DT string shown
  ;"         NOTEDT -- [optional] DT string of date of note
  ;"                        If provided, then the date of the vital sign must equal NOTEDT, or
  ;"                        "" is returned (Unless FORCESHOW=1)
  ;"         FORCESHOW -- [optional] 1: Will force a return RESULT, IF otherwise wouldn't be shown
  ;"                      NOTE: this is older system.  New system excludes
  ;"                      various vitals at various cutoffs. FORCESHOW doesn't override this.
  ;"         PTAGE -- [optional], default is 99-- age in YRS of patient
  ;"         EXCLUDEARR -- [optional].  PASS BY REFERENCE.  If a particular
  ;"              vital is too old for inclusion, then this is array is filled
  ;"                e.g. EXCLUDEARR("T")=1
  ;"Results: non (changes are passed back in RESULT)
  SET OUTS=$GET(OUTS)
  NEW RESULT SET RESULT=""
  SET PTAGE=+$GET(PTAGE,99)
  IF $GET(STR)'="" DO
  . NEW VITALDT SET VITALDT=""
  . NEW DELTA
  . SET STR=$$REMOVEDT(STR,.VITALDT)
  . SET VITALDT=$$REMOVTIM(VITALDT)
  . SET DELTA=$$DATEDELT(.NOTEDT,.VITALDT) QUIT:(DELTA<0)  ;"Returns #days
  . IF (DELTA>0)&($GET(NOTEDT)'="")&($GET(FORCESHOW)'=1) QUIT  ;"If NOTEDT specified, don't allow delta>0
  . IF (OUTS'="")&($EXTRACT(OUTS,$LENGTH(OUTS))'=$CHAR(9)) SET RESULT=RESULT_", "
  . IF CURDT'=VITALDT DO
  . . SET CURDT=VITALDT
  . . IF (DELTA>0)&(VITALDT'="") DO
  . . . IF (PTAGE>18)!(DELTA<32) DO  QUIT
  . . . . IF $$DELIFOLD(LABEL,DELTA,.STR)=0 DO  QUIT
  . . . . . SET RESULT=""
  . . . . . SET EXCLUDEARR(LABEL)=1
  . . . . SET RESULT=RESULT_"("_VITALDT_") "
  . . . ELSE  DO
  . . . . NEW AGESTR,AGEATVIT SET AGEATVIT=$$AGEONDAT(DFN,VITALDT)
  . . . . IF AGEATVIT<2 SET AGESTR=$JUSTIFY(AGEATVIT/12,0,0)_" mo"
  . . . . ELSE  SET AGESTR=$JUSTIFY(AGEATVIT,0,1)_" yr"
  . . . . SET RESULT=RESULT_"("_VITALDT_" @ "_AGESTR_") "
  . IF STR'="" SET RESULT=RESULT_LABEL_" "_STR
  SET RESULT=$$TRIM^XLFSTR(RESULT)
  SET OUTS=OUTS_RESULT
FVDONE  ;
  QUIT
  ;
DELIFOLD(LABEL,AGE,VALUE) ;"DELETE IF OLD
  ;"Purpose: To remove vitals that re too old to be relevent. e.g. pulse from 2 months ago.
  ;"         The relevent age will depend on the type of vital measurement
  ;"Input: LABEL: the vitals label, used here as a TYPE
  ;"              Labels used: T, BP, R, P, Wt, Ht, HC, BMI,
  ;"       AGE:age of vital measurement, in DAYS
  ;"       VALUE -- PASS BY REFERENCE.  Will be SET to "" IF too old
  ;"Result: 1 if OK to show, 0 IF too old.
  NEW CUTOFF
  SET CUTOFF("T")=2      ;"2 DAYS
  SET CUTOFF("BP")=14    ;"1 WEEK
  SET CUTOFF("R")=2      ;"2 DAYS
  SET CUTOFF("P")=2      ;"2 DAYS
  SET CUTOFF("Wt")=14    ;"2 WEEKS
  SET CUTOFF("Ht")=9999  ;"(infinite)
  SET CUTOFF("HC")=180   ;"6 MONTH
  SET CUTOFF("BMI")=9999 ;"(infinite)
  NEW RESULT SET RESULT=1
  NEW ALLOWEDAGE SET ALLOWEDAGE=$GET(CUTOFF(LABEL),9999)
  IF AGE>ALLOWEDAGE DO
  . SET VALUE=""
  . SET RESULT=0
  QUIT RESULT
  ;
REMOVEDT(STR,DT)  ;
  ;"Purpose: to remove a date-Time string, and return in DT
  ;"    i.e. turn this:
  ;"        127/56 (12/25/04 16:50)
  ;"    into these:
  ;"        '127/56'   and   '12/25/04 16:50'
  ;"Input:  STR -- a string as above
  ;"       DT -- [Optional] an OUT parameter... must PASS BY REFERENCE
  ;"RESULT: returns input string with (date-time) removed
  ;"        Date-Time is returned in DT IF passed by reference.
  ;
  NEW RESULT SET RESULT=$GET(STR)
  IF RESULT="" GOTO RDTDONE
  SET RESULT=$PIECE(STR,"(",1)
  SET RESULT=$$TRIM^XLFSTR(RESULT)
  SET DT=$PIECE(STR,"(",2)
  SET DT=$PIECE(DT,")",1)
  SET DT=$$TRIM^XLFSTR(DT)
RDTDONE ;
  QUIT RESULT
  ;
REMOVTIM(DT) ;
  ;"Purpose: to remove the time from a date/time string
  ;"Input: DT -- the date/time string, i.e. '2/24/05 16:50'
  ;"RESULT: returns just the date, i.e. '2/25/05'
  NEW RESULT SET RESULT=$PIECE(DT," ",1)
  QUIT RESULT
  ;
FORMATHT(HEIGHTSTR,PTAGE)  ;
  ;"Purpose: to remove centimeters from patient's height for adults
  ;"Input: Ht, a height string, e.g. '74 in [154 cm] (1/1/1990)'
  ;"       PTAGE, patient's age in years
  ;"Result: returns patient height, with [154 cm] removed, IF age > 16
  NEW RESULT SET RESULT=$GET(HEIGHTSTR)
  IF $GET(PTAGE)'<16 DO
  . SET RESULT=$PIECE(HEIGHTSTR,"[",1)
  . IF HEIGHTSTR["(" SET RESULT=RESULT_"("_$PIECE(HEIGHTSTR,"(",2,99)
  QUIT RESULT
  ;
DATEDELT(REFDATE,DT) ;
  ;"Purpose: To determine the number of days between REFDATE and DT
  ;"                i.e. How many days DT was before REFDATE.
  ;"Input:REFDATE -- a reference/baseline date/time string
  ;"                IF not supplied, Current date/time used as default.
  ;"        DT -- a date/time string (i.e. '12/25/04 16:50')
  ;"Result: Return number of days between DT and REFDATE
  ;"        Positive numbers used when DT occured before current date
  ;"        i.e. RESULT=REFDATE-DT
  NEW INTREFDATE,INTDT  ;internal format of dates
  NEW RESULT SET RESULT=0
  SET X=DT DO ^%DT SET INTDT=Y         ;"Convert date into internal
  IF $GET(REFDATE)="" SET INTREFDATE=$$DT^XLFDT
  ELSE  SET X=REFDATE DO ^%DT SET INTREFDATE=Y   ;"Convert date into internal
  SET RESULT=$$FMDIFF^XLFDT(INTREFDATE,INTDT)
  QUIT RESULT
  ;
TMGVISDT(TIU)  ;" Visit date
  ;"Purpose: Return a string for date of visit
  ;"Note: This is based on the function VISDATE^TIULO1(TIU)
  ;"        However, that function seemed to return the appointment date associated
  ;"                with a note, rather than the specified date of the note
  ;"        Also, this will return date only--not time.
  ;"Input: TIU -- this is an array created by TIU system.  See documentation above.
  ;"Output: returns RESULT
  ;
  N TIUX,TIUY
  NEW RESULT
  IF $GET(TIU("VISIT"))'="" DO
  . SET RESULT=$PIECE(TIU("VISIT"),U,2)
  ELSE  IF $GET(TIU("VSTR"))'="" DO
  . SET RESULT=$PIECE(TIU("VSTR"),";",2)
  ELSE  DO
  . SET RESULT="(Visit Date Unknown)"
  ;
  IF +RESULT>0 DO
  . SET RESULT=$$DATE^TIULS(RESULT,"MM/DD/YY HR:MIN")
  . SET RESULT=$PIECE(RESULT," ",1)  ;"cut off time.
VDDONE  ;
  QUIT RESULT
  ;
AGEONDAT(DFN,REFDATE) ;
  ;"Purpose: return patient age on given date, in years
  ;"Input: DFN -- Patient's IEN
  ;"       REFDATE -- Date of reference, in FMDATE, or external form.
  ;"Output: age, in YEARS.
  NEW DOB SET DOB=$PIECE($GET(^DPT(DFN,0)),"^",3)
  NEW RESULT SET RESULT=0
  SET REFDATE=$GET(REFDATE)
  IF +REFDATE'=REFDATE DO
  . NEW %DT,X,Y
  . SET X=REFDATE DO ^%DT SET REFDATE=Y
  IF REFDATE'>0 GOTO AODDN
  NEW X1 SET X1=REFDATE
  NEW X2 SET X2=DOB
  DO ^%DTC  ;"RETURNS X=X1-X2, in days
  SET RESULT=X/365
  IF RESULT>17 SET RESULT=RESULT\1 GOTO AODDN
  IF RESULT>2 SET RESULT=+$JUSTIFY(X/365,0,1) GOTO AODDN
  SET RESULT=+$JUSTIFY(X/365,0,2) GOTO AODDN
AODDN   ;
  QUIT RESULT
  ;
PTAGE(DFN,NOTEDT) ;
  ;"Purpose: return patient's AGE (in years) on date of note (or current
  ;"         date IF date of note is empty)
  ;"Input: DFN -- Patient IEN
  ;"       NOTEDT -- Date of Note
  ;"Output: results in years.
  NEW PTAGE SET PTAGE=$$AGEONDAT(DFN,NOTEDT)
  IF PTAGE=0 DO
  . IF NOTEDT="" DO
  . . NEW X DO NOW^%DTC
  . . SET PTAGE=$$AGEONDAT(DFN,X)
  . ELSE  DO
  . . SET PTAGE=+$$GET1^DIQ(2,DFN_",",.033) ;"returns INTEGER yrs
  . . IF PTAGE>17 QUIT
  . . NEW DOB SET DOB=$PIECE($GET(^DPT(DFN,0)),"^",3)
  . . NEW %,X,X1,X2,%Y
  . . DO NOW^%DTC
  . . SET X1=X ;"now
  . . SET X2=DOB
  . . DO ^%DTC  ;"RESULT out in X (days delta)
  . . IF %Y=0 QUIT  ;"dates are unworkable
  . . SET PTAGE=$JUSTIFY(X/365,0,4)
  QUIT PTAGE
  ;
HC(DFN,HC) ;
  ;"Purpose: Return formatedd head circumference reading
  ;"Input: DFN -- The patient's IEN
  ;"       HC -- PASS BY REFERENCE, an OUT PARAMETER.  Returns value in centimeters (cm)
  ;"Result: Head circumference string, e.g. 123 cm (1/1/1980), or "" IF invalid
  NEW RESULT SET RESULT="",HC=""
  NEW HEADCIR SET HEADCIR=$$LASTHC(DFN)
  IF HEADCIR="" GOTO HCDN
  SET HC=$PIECE(HEADCIR,"^",3)
  NEW DATEOFHC SET DATEOFHC=$PIECE(HEADCIR,"^",1)
  SET RESULT=$PIECE(HEADCIR,"^",2)_" in ["_HC_"cm] ("_$$FMTE^XLFDT(DATEOFHC,"5D")_")" ;"OUTPUT MM/DD/YYYY
HCDN  ;
  QUIT RESULT
  ;
LASTHC(DFN)   ;
  ;"Purpose: Return the patient's last head circumference
  ;"NOTE: this assumes that head circumference is store in CIRC/GIRTH vital type.
  ;"Input: DFN -- Patient's DFN
  ;"Output: none
  ;"Results: FMDATE^VALUE(in)^VALUE(cm) or "" IF invalid
  ;"note: Consider using $$GETVITLS^TMGGMRV1 in the future...
  NEW RESULT
  NEW THISDT SET THISDT=9999999
  NEW VITIEN SET VITIEN=+$ORDER(^GMRD(120.51,"B","CIRCUMFERENCE/GIRTH",0))
  SET THISDT=$ORDER(^PXRMINDX(120.5,"PI",DFN,VITIEN,THISDT),-1)
  IF THISDT'>0 SET RESULT="" GOTO GHCDN
  NEW RECIEN SET RECIEN=0
  SET RECIEN=+$ORDER(^PXRMINDX(120.5,"PI",DFN,VITIEN,THISDT,RECIEN))
  NEW VALUE SET VALUE=$PIECE($GET(^GMR(120.5,RECIEN,0)),"^",8)
  IF VALUE'>0 SET RESULT=""
  ELSE  DO
  . NEW METRICVAL SET METRICVAL=$$CVTMETRC^TMGGRC2C("CG",VALUE)
  . SET THISDT=THISDT\1  ;"Trim off Time
  . SET RESULT=THISDT_"^"_VALUE_"^"_$JUSTIFY(METRICVAL,0,1)
GHCDN  ;
  QUIT RESULT
  ;
FNAME(DFN)  ;
  ;"Purpose: Return Patient's first name
  ;"Input: DFN -- the patient's unique ID (record#)
  ;"Output: returns RESULT
  ;"SEE ALSO TMGGDFNU.m
  NEW NAME SET NAME=$PIECE($GET(^DPT(DFN,0)),"^",1)
  SET NAME=$PIECE(NAME,",",2)
  SET NAME=$PIECE(NAME," ",1)
  SET NAME=$$CAPWORDS^TMGSTUT2(NAME)
  QUIT NAME
  ;
MNAME(DFN) ;
  ;"Purpose: Return Patient's middle name(s)
  ;"Input: DFN -- the patient's unique ID (record#)
  ;"Output: returns RESULT
  ;"SEE ALSO TMGGDFNU.m
  NEW NAME SET NAME=$PIECE($GET(^DPT(DFN,0)),"^",1)
  SET NAME=$PIECE(NAME,",",2)
  SET NAME=$PIECE(NAME," ",2,100)
  SET NAME=$$CAPWORDS^TMGSTUT2(NAME)
  QUIT NAME
  ;
LNAME(DFN) ;
  ;"Purpose: Return Patient's last name
  ;"Input: DFN -- the patient's unique ID (record#)
  ;"Output: returns RESULT
  ;"SEE ALSO TMGGDFNU.m
  NEW NAME SET NAME=$PIECE($GET(^DPT(DFN,0)),"^",1)
  SET NAME=$PIECE(NAME,",",1)
  SET NAME=$$CAPWORDS^TMGSTUT2(NAME)
  QUIT NAME
  ;
NICENAME(DFN) ;
  ;"Purpose: Return Patient's name format: Firstname Middlename Lastname
  ;"                      only the first letter of each name capitalized.
  ;"Input: DFN -- the patient's unique ID (record#)
  ;"Output: returns RESULT
  NEW NAME SET NAME=$PIECE($GET(^DPT(DFN,0)),"^",1)
  SET NAME=$PIECE(NAME,",",2)_" "_$PIECE(NAME,",",1) ;"put first NAME first
  SET NAME=$$CAPWORDS^TMGSTUT2(NAME)
  QUIT NAME
  ;
PHONENUM(DFN) ;
  ;"Purpose: to return the patient's phone number
  ;"Input: DFN -- the patient's unique ID (record#)
  ;"Output: returns RESULT
  NEW RESULT SET RESULT=""
  SET DFN=+$GET(DFN)
  IF DFN=0 GOTO PNDONE
  SET RESULT=$$GET1^DIQ(2,DFN_",",.131)
  SET RESULT=$TRANSLATE(RESULT," ","")
  IF $LENGTH(RESULT)=10 DO
  . NEW TEMP SET TEMP=RESULT
  . SET RESULT="("_$EXTRACT(RESULT,1,3)_") "_$EXTRACT(RESULT,4,6)_"-"_$EXTRACT(RESULT,7,10)
  IF $LENGTH(RESULT)=7 DO
  . NEW TEMP SET TEMP=RESULT
  . SET RESULT=$EXTRACT(RESULT,1,3)_"-"_$EXTRACT(RESULT,4,7)
PNDONE  ;
  QUIT RESULT
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
WEIGHT(DFN,TIU)  ;         
  ;"Purpose: Return a string of the weight values
  ;"Input: DFN -- the patient's unique ID (record#)
  ;"       TIU -- See documentation below.
  ;"Output: returns RESULT
  NEW STRING
  SET STRING=$$ONEVITAL^TMGTBL01(.DFN,.TIU,"WT")_" "_$$ONEVITAL^TMGTBL01(.DFN,.TIU,"BMI-CMT")
  IF STRING["Wt " SET STRING=$PIECE(STRING,"Wt ",2,999)
  SET STRING="Wt "_STRING
  SET STRING=$TR(STRING,$C(13,10))
  SET STRING=$$REPLSTR^TMGSTUT3(STRING,"         ","")
  SET STRING=$$REPLSTR^TMGSTUT3(STRING,"[See vital-signs documented in chart]","")
  ;"FINISH PARSING STRING
  SET ^TMP("EDDIE","WEIGHT")=STRING
  QUIT STRING
  ;"      
WTONLY(DFN,TIU)
  ;"Purpose: return a string of weight values with dates. Limit at 3
  NEW TMGRESULT SET TMGRESULT="" 
  NEW TIUVIT,TIUVT,TIUVDT,TIUVDA,TIUY,VDT,TIUI,TIUCWRAP,TIUMAXW,TIUVITC
  NEW TIUVCNT,TIUVCNT2,TIUVDONE,TIUVDATE,TIUY1,TIUVTEMP,CONV
  NEW LVDT,COUNT,MAX SET LVDT=0
  SET COUNT=0,MAX=3
  SET TIUVDONE=0
  SET TIUVITC="WT"
  DO VITALS^TIULO(.TIUVIT,DFN,TIUVITC,"","",10)
  SET (TIUVDT,TIUVDONE,TIUVCNT)=0
  FOR  SET TIUVDT=$O(TIUVIT(TIUVITC,TIUVDT)) QUIT:+TIUVDT'>0!TIUVDONE  DO
  . SET TIUVDA=0
  . FOR  SET TIUVDA=$O(TIUVIT(TIUVITC,TIUVDT,TIUVDA)) Q:+TIUVDA'>0!TIUVDONE  DO
  . . SET TIUVDATE=TIUVDT,TIUVCNT=TIUVCNT+1
  . . SET TIUVTEMP=$G(TIUVIT(TIUVITC,TIUVDT,TIUVDA))
  . . SET VDT=$$DATE^TIULS($P(TIUVTEMP,U,1),"MM/DD/CCYY")
  . . IF VDT=LVDT QUIT
  . . SET LVDT=VDT
  . . SET TIUY=$P(TIUVTEMP,U,8)
  . . QUIT:+TIUY'>0
  . . SET COUNT=COUNT+1
  . . IF COUNT=MAX SET TIUVDONE=1
  . . SET CONV=$J((+TIUY/2.2),3,1)
  . . SET TIUY=TIUY_" lb "
  . . SET TIUY=TIUY_"("_VDT_")"
  . . IF TMGRESULT'="" SET TMGRESULT=TMGRESULT_", "
  . . SET TMGRESULT=TMGRESULT_TIUY
  SET TMGRESULT="Weight = "_TMGRESULT
  QUIT TMGRESULT
  ;"
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
GETREACT(IEN120D8,NOTHTML,TRIM)  ;
  ;" Purpose: NOTE: changed to pull both signs/symp AND Comments  8/2/18
  ;"          IF TRIM=1, TRUNCATE TO 40 CHARACTERS
  NEW REACTS,REACTIEN,SYMPIEN,SYMPNAME,LINENUM,COMMENT,HTMLBTAG,HTMLETAG,PREFIX
  SET TRIM=+$G(TRIM)
  SET NOTHTML=$GET(NOTHTML,0)
  IF NOTHTML=0 DO
  . SET HTMLBTAG="{HTML:<I>}"
  . SET HTMLETAG="{HTML:</I>}"
  . SET RPREFIX=" Signs/Sym: "
  . SET CPREFIX="Comments: "
  ELSE  DO
  . SET HTMLBTAG=""
  . SET HTMLETAG=""
  . SET RPREFIX=" Signs/Sym: "
  . SET CPREFIX="Comments: "
  SET REACTS="",REACTIEN=0
  IF IEN120D8="" QUIT REACTS
  FOR  SET REACTIEN=$ORDER(^GMR(120.8,IEN120D8,10,REACTIEN)) QUIT:REACTIEN'>0  DO
  . SET SYMPIEN=$PIECE($GET(^GMR(120.8,IEN120D8,10,REACTIEN,0)),"^",1)
  . SET SYMPNAME=$PIECE($GET(^GMRD(120.83,SYMPIEN,0)),"^",1)
  . IF REACTS="" SET REACTS=RPREFIX_SYMPNAME
  . ELSE  SET REACTS=REACTS_","_SYMPNAME
  SET LINENUM=0
  SET COMMENT=""
  NEW COMMENTSTR SET COMMENTSTR=""
  FOR  SET LINENUM=$ORDER(^GMR(120.8,IEN120D8,26,1,2,LINENUM)) QUIT:LINENUM'>0  DO 
  . SET COMMENT=$GET(^GMR(120.8,IEN120D8,26,1,2,LINENUM,0))
  . IF COMMENTSTR="" SET COMMENTSTR=CPREFIX_HTMLBTAG_COMMENT
  . ELSE  SET COMMENTSTR=COMMENTSTR_","_COMMENT
  IF COMMENTSTR'="" SET COMMENTSTR=COMMENTSTR_HTMLETAG
  SET REACTS=REACTS_" "_COMMENTSTR
  IF (TRIM=1)&($L(REACTS)>39) SET REACTS=$E(REACTS,1,35)_"..."
  QUIT REACTS
  ;
DETALRGY(DFN)  ;
  ;" PURPOSE: FIND AND RETURN DETAILED ALLERGY INFORMATION 
  ;"Check for No Assessment
  NEW ALLSTR SET ALLSTR=$$ALLERGY(DFN)
  IF ALLSTR["NEEDS ALLERGY ASSESSMENT" DO  QUIT RESULT
  . SET RESULT="NEEDS ALLERGY ASSESSMENT"
  . SET RESULT="{HTML:<FONT style=""BACKGROUND-COLOR:#ff0000"">}"_RESULT_"{HTML:</FONT>}"
  NEW RESULT,IEN120D8,ALRGYARR,LINE,IEN,Y,REACTIONS
  SET IEN120D8=0,RESULT="{HTML:<BR>}"
  SET IEN=0                     
  FOR  SET IEN120D8=$ORDER(^GMR(120.8,"B",DFN,IEN120D8)) QUIT:IEN120D8'>0  DO
  . ;WRITE $GET(^GMR(120.8,IEN120D8,0)),!
  . IF $D(^GMR(120.8,IEN120D8,"ER")) QUIT  ;"Exclude if Entered In Error
  . SET LINE=$GET(^GMR(120.8,IEN120D8,0))
  . SET Y=$P(LINE,"^",4)  ;date
  . X ^DD("DD")
  . SET REACTIONS=$$GETREACT(IEN120D8)
  . SET RESULT=RESULT_"{HTML:<B>}"_$P(LINE,"^",2)_"{HTML:</B><FONT SIZE=""-1"">} (Entered: "_$P(Y,"@",1)_"){HTML:</FONT>}"_REACTIONS_"{HTML:<BR>}"   ;"$CHAR(13)_$CHAR(10)
  . SET IEN=IEN+1
  IF RESULT="{HTML:<BR>}" SET RESULT="No Known Allergies"
  QUIT RESULT
  ;
DETALRGY2(DFN)  ;
  ;" PURPOSE: FIND AND RETURN DETAILED ALLERGY INFORMATION 
  ;"Check for No Assessment
  NEW ALLSTR SET ALLSTR=$$ALLERGY(DFN)
  IF ALLSTR["NEEDS ALLERGY ASSESSMENT" DO  QUIT RESULT
  . SET RESULT="NEEDS ALLERGY ASSESSMENT"
  . SET RESULT="<FONT style=""BACKGROUND-COLOR:#ff0000"">"_RESULT_"</FONT>"
  NEW RESULT,IEN120D8,ALRGYARR,LINE,IEN,Y,REACTIONS
  SET IEN120D8=0,RESULT="<P><B>ALLERGIES:</B><BR>"
  SET IEN=0                     
  FOR  SET IEN120D8=$ORDER(^GMR(120.8,"B",DFN,IEN120D8)) QUIT:IEN120D8'>0  DO
  . ;WRITE $GET(^GMR(120.8,IEN120D8,0)),!
  . IF $D(^GMR(120.8,IEN120D8,"ER")) QUIT  ;"Exclude if Entered In Error
  . SET LINE=$GET(^GMR(120.8,IEN120D8,0))
  . SET Y=$P(LINE,"^",4)  ;date
  . X ^DD("DD")
  . SET REACTIONS=$$GETREACT(IEN120D8,1)
  . SET RESULT=RESULT_"<B>"_$P(LINE,"^",2)_"</B><FONT SIZE=""-1""> (Entered: "_$P(Y,"@",1)_")</FONT>"_REACTIONS_"<BR>"   ;"$CHAR(13)_$CHAR(10)
  . SET IEN=IEN+1
  IF RESULT="<BR>" SET RESULT="No Known Allergies"
  QUIT RESULT
  ;
ROSALRGY(TMGRESULT,DFN)  ;
  ;" PURPOSE: FIND AND RETURN DETAILED ALLERGY INFORMATION
  ;"          FOR THE ROS FORM, THROUGH "TMG GET ROS ALLERGY LIST" RPC 
  NEW IEN120D8,ALRGYARR,LINE,IEN,Y,REACTIONS
  SET IEN120D8=0,RESULT=""
  NEW GMRARXN
  D EN1^GMRAOR1(DFN,"GMRARXN")
  IF $G(GMRARXN)="" S TMGRESULT="No Allergy Assessment,,,,,,,,,,,,," GOTO RADN
  IF $G(GMRARXN)=0 S TMGRESULT="No Known Allergies,,,,,,,,,,,,," GOTO RADN
  FOR IEN=1:1:7 SET ALRGYARR(IEN)=","
  SET IEN=0
  FOR  SET IEN120D8=$ORDER(^GMR(120.8,"B",DFN,IEN120D8)) QUIT:IEN120D8'>0  DO
  . IF $D(^GMR(120.8,IEN120D8,"ER")) QUIT  ;"Exclude if Entered In Error
  . SET LINE=$GET(^GMR(120.8,IEN120D8,0))
  . SET LINE=$TR(LINE,",",";")
  . SET LINE=$TR(LINE,""""," ")
  . IF $L(LINE)>38 SET LINE=$E(LINE,1,38)_"..."
  . SET REACTIONS=$$GETREACT(IEN120D8,1,1)
  . ;"IF REACTIONS="" DO
  . ;". SET REACTIONS=$$GETREACT(IEN120D8,1,1)
  . SET REACTIONS=$TR(REACTIONS,",",";")
  . SET REACTIONS=$TR(REACTIONS,""""," ")
  . SET REACTIONS=$$SENTENCE^XLFSTR(REACTIONS)  ;"10/8/18
  . SET IEN=IEN+1
  . IF IEN=8 SET ALRGYARR(7)="(MORE... SEE COMPLETE LIST),"
  . ELSE  IF IEN>8 QUIT
  . ELSE  SET ALRGYARR(IEN)=$P(LINE,"^",2)_","_REACTIONS
  SET IEN=0,TMGRESULT=""
  FOR  SET IEN=$ORDER(ALRGYARR(IEN)) QUIT:IEN'>0  DO
  . SET LINE=$GET(ALRGYARR(IEN))
  . SET TMGRESULT=TMGRESULT_LINE
  . IF IEN<7 SET TMGRESULT=TMGRESULT_","
  IF TMGRESULT=",,,,,,,,,,,,," SET TMGRESULT=",No Known Allergies,,,,,,,,,,,,"
RADN
  QUIT                                        
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
GETLVITD(DFN)  ;"Return last lab data for Vitamin D
  NEW TMGRESULT,RESULTARR
  SET TMGRESULT=$$GETTABL1^TMGTIUO6(DFN,"[STUDIES]",.RESULTARR)
  SET TMGRESULT=$GET(RESULTARR("KEY-VALUE","VIT-D"))_" [T]"
  IF TMGRESULT=" [T]" SET TMGRESULT="NO DATA FOUND"
  QUIT TMGRESULT
  ;"  
FUITEMS(DFN)  ;"Return the followup table if data is contained
  NEW X
  S X=$$GETTABLX^TMGTIUO6(+$G(DFN),"[FOLLOWUP ITEMS]")      
  NEW TEMP SET TEMP=$P(X,$C(13,10),2)
  SET TEMP=$$TRIM^XLFSTR(TEMP)
  IF TEMP="" SET X=TEMP
  IF TEMP'="" SET TEMP="<FONT style=""BACKGROUND-COLOR:#ff0000"">}"_TEMP_"{HTML:</FONT>}"
  QUIT X
  ;"                             
FUITEMS2(TMGRESULT,DFN)  ;"Return the followup table if data is contained, for RPC: TMG GET FOLLOWUP ITEMS
  NEW FUITEMS SET FUITEMS=$$GETTABLX^TMGTIUO6(+$G(DFN),"[FOLLOWUP ITEMS]")
  SET TMGRESULT=""
  NEW IDX SET IDX=0
  FOR IDX=0:1:6  DO
  . IF TMGRESULT'="" SET TMGRESULT=TMGRESULT_","
  . NEW ITEM SET ITEM=$P(FUITEMS,$C(13,10),1)
  . IF ITEM="" SET ITEM=FUITEMS
  . SET FUITEMS=$P(FUITEMS,$C(13,10),2,999)
  . SET ITEM=$$TRIM^XLFSTR(ITEM)
  . SET ITEM=$TR(ITEM,",",".")
  . SET TMGRESULT=TMGRESULT_ITEM
  ;FOR  QUIT:FUITEMS'[$C(13,10)  DO
  ;. SET TMGRESULT(IDX)=$P(FUITEMS,$C(13,10),1)
  ;. SET TMGRESULT(IDX)=$$TRIM^XLFSTR($G(TMGRESULT(IDX)))
  ;. SET TMGRESULT(IDX)=$TR($G(TMGRESULT(IDX)),",",".")
  ;. SET IDX=IDX+1
  ;. SET FUITEMS=$P(FUITEMS,$C(13,10),2,999)
  ;IF FUITEMS'="" DO
  ;. SET TMGRESULT(IDX)=FUITEMS
  ;. SET TMGRESULT(IDX)=$$TRIM^XLFSTR($G(TMGRESULT(IDX)))
  ;. SET TMGRESULT(IDX)=$TR($G(TMGRESULT(IDX)),",",".")
  ;IF TMGRESULT="" SET TMGRESULT(0)="NO FOLLOW UP ITEMS FOUND"
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
  ;          
NEEDPSA(TMGRESULT,DFN)  ;"
  ;"Purpose: To determine if the patient needs a PSA handout
  SET TMGRESULT=0
  NEW X DO NOW^%DTC       
  NEW REMRESULT SET REMRESULT=$$DOREM^TMGPXR03(DFN,263,5,X)
  IF REMRESULT["DUE NOW" SET TMGRESULT=1
  QUIT TMGRESULT
  ;"
NEEDPHQ9(TMGRESULT,DFN,APPTDATE)  ;"
  ;"Purpose: To determine if the patient needs a PHQ-9 handout
  SET TMGRESULT=$$REMDUE(DFN,APPTDATE,277)
  ;"NEW DATE SET DATE=$$INTDATE^TMGDATE(APPTDATE)
  ;"IF DATE'>0 DO
  ;". NEW X DO NOW^%DTC
  ;". SET DATE=X
  ;"NEW REMRESULT SET REMRESULT=$$DOREM^TMGPXR03(DFN,277,5,DATE)
  ;"IF REMRESULT["DUE NOW" SET TMGRESULT=1
  QUIT
  ;"
NEEDMCOG(TMGRESULT,DFN,APPTDATE)  ;"
  ;"Purpose: To determine if the patient needs a mini cog handout
  SET TMGRESULT=$$REMDUE(DFN,APPTDATE,278)
  ;"NEW DATE SET DATE=$$INTDATE^TMGDATE(APPTDATE)
  ;"IF DATE'>0 DO
  ;". NEW X DO NOW^%DTC
  ;". SET DATE=X
  ;"NEW REMRESULT SET REMRESULT=$$DOREM^TMGPXR03(DFN,278,5,DATE)
  ;"IF REMRESULT["DUE NOW" SET TMGRESULT=1
  QUIT
  ;"
NEEDFALL(TMGRESULT,DFN,APPTDATE)  ;"
  ;"Purpose: To determine if the patient needs a fall risk handout
  SET TMGRESULT=$$REMDUE(DFN,APPTDATE,279)
  QUIT 
  ;"
REMDUE(DFN,DATE,REMIEN)  ;"
  ;"Purpose: To determine if the given reminder is due for the patient on
  ;"         date sent
  NEW TMGRESULT SET TMGRESULT=0
  NEW TESTDATE SET TESTDATE=$$INTDATE^TMGDATE(APPTDATE)
  IF TESTDATE'>0 DO
  . NEW X DO NOW^%DTC
  . SET TESTDATE=X
  NEW REMRESULT SET REMRESULT=$$DOREM^TMGPXR03(DFN,REMIEN,5,TESTDATE)
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
  . . IF AUTHOR'>0 QUIT
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
  . . NEW Y SET Y=NOTEDATE
  . . X ^DD("DD")
  . . SET TMGRESULT=TMGRESULT_$PIECE(Y,"@",1)_" "
  ELSE  DO
  . SET TMGRESULT="NO OPHTHALMOLOGY NOTES FOUND"
  QUIT TMGRESULT
  ;"            
 ;"-------------------------------------------------------------
 ;"-------------------------------------------------------------
ENSURE(ARRAY,KEY,PIVOT,VALUE) ;
  ;"Purpose: to add one (empty) entry, IF a value for this doesn't already exist.
  ;"Input: ARRAY.  Format as follows:
  ;"          ARRAY("text line")=""
  ;"          ARRAY("text line")=""
  ;"          ARRAY("KEY-VALUE",KEYNAME)=VALUE
  ;"          ARRAY("KEY-VALUE",KEYNAME,"LINE")=original line
  ;"       KEY -- the name of the study
  ;"       PIVOT -- ":", or "="  OPTIONAL.  Default = ":"
  ;"       VALUE -- the description of the needed value.  OPTIONAL.
  ;"              default value = '<no data>'
  SET PIVOT=$GET(PIVOT,":")
  SET VALUE=$GET(VALUE)
  SET KEY=$GET(KEY)
  IF VALUE=""!(VALUE=".") SET VALUE="<NO DATA>"
  IF KEY="" GOTO AIADONE
  NEW UPKEY SET UPKEY=$$UP^XLFSTR(KEY)
  IF $DATA(ARRAY("KEY-VALUE",UPKEY))>0 GOTO AIADONE
  SET ARRAY("KEY-VALUE",UPKEY)=VALUE
  NEW LINE SET LINE="        "_KEY_" "_PIVOT_" "_VALUE
  SET ARRAY("KEY-VALUE",UPKEY,"LINE")=LINE
AIADONE  ;
  QUIT
  ;
REMOVE(ARRAY,KEY,PIVOT) ;
  ;"Purpose: to remove one entry from table. 
  ;"Input: ARRAY.  Format.  As per ENSURE^TMGTIUO3
  ;"       KEY -- the name of the study
  ;"       PIVOT -- ":", or "="  OPTIONAL.  Default = ":"
  SET PIVOT=$GET(PIVOT,":"),KEY=$GET(KEY)
  IF KEY="" GOTO RMDN
  KILL ARRAY("KEY-VALUE",$$UP^XLFSTR(KEY))
RMDN  ;
  QUIT
  ;
SETBYPXR(DFN,ARRAY,KEY,PIVOT,TABLENAME,ITEMNAME)  ;"Set by TMG TIU PXRM TABLE
  ;"Input: DFN -- IEN in PATIENT file
  ;"Input: ARRAY.  Format as follows:
  ;"          ARRAY("text line")=""
  ;"          ARRAY("text line")=""
  ;"          ARRAY("KEY-VALUE",KEYNAME)=VALUE
  ;"          ARRAY("KEY-VALUE",KEYNAME,"LINE")=original line
  ;"       KEY -- the name of the study
  ;"       PIVOT -- ":", or "="  OPTIONAL.  Default = ":"
  ;"       TABLENAME -- NAME of table to pull from (TMG TIU PXRM TABLE, #22708)
  ;"       ITEMNAME -- ITEM name in table
  SET KEY=$GET(KEY) IF KEY="" GOTO SBPRDN
  SET PIVOT=$GET(PIVOT,":")
  NEW VALUE SET VALUE=$$GETTABLN^TMGPXR02(.DFN,.TABLENAME,.ITEMNAME)
  SET VALUE=$$TRIM^XLFSTR($PIECE(VALUE,":",2))
  IF VALUE=""!(VALUE=".") SET VALUE="NO DATA"
  SET VALUE="["_VALUE_"]"
  NEW UPKEY SET UPKEY=$$UP^XLFSTR(KEY)
  SET ARRAY("KEY-VALUE",UPKEY)=VALUE
  ;"NEW LINE SET LINE="        "_KEY_" "_PIVOT_" "_VALUE
  NEW LINE SET LINE=KEY_" "_PIVOT_" "_VALUE
  SET ARRAY("KEY-VALUE",UPKEY,"LINE")=LINE
SBPRDN  ;
  QUIT
  ;
VITARR(OUT,DFN,SDT,EDT,VERBOSE)  ;"GET ARRAY OF VITALS
  ;"Purpose: get array of vitals data for date range.  
  ;"Input: OUT -- PASS BY REFERENCE, AN OUT PARAMETER.  Format:
  ;"           OUT(DFN)=PATIENT NAME
  ;"           OUT(DFN,DT,VTIEN)=VALUE^VITAL NAME
  ;"           OUT(DFN,DT,VTIEN,0)=ZERO NODE OF RECORD
  ;"           OUT(DFN,"V",VTIEN,DT)=VALUE^VIT NAME
  ;"           OUT("V",VTIEN)=VITAL NAME
  ;"       DFN -- patient IEN
  ;"       SDT -- Starting date of range, FM format, default is 0
  ;"       EDT -- Starting date of range, FM format, default is 9999999
  ;"       VERBOSE -- if 1 then more record info is returned
  ;"note: Compare to $$GETVITLS^TMGGMRV1 in the future...
  SET DFN=+$GET(DFN) SET SDT=+$GET(SDT) SET EDT=+$GET(EDT) 
  IF EDT=0 SET EDT=9999999
  NEW RSDT SET RSDT=9999999-SDT
  NEW REDT SET REDT=9999999-EDT
  SET VERBOSE=$GET(VERBOSE)
  SET OUT(DFN)=$PIECE($GET(^DPT(DFN,0)),"^",1)
  NEW VITIEN SET VITIEN=0
  FOR  SET VITIEN=$ORDER(^GMR(120.5,"AA",DFN,VITIEN)) QUIT:(+VITIEN'>0)  DO
  . NEW VITNAME SET VITNAME=$PIECE($GET(^GMRD(120.51,VITIEN,0)),"^",1)
  . SET OUT("V",VITIEN)=VITNAME
  . NEW ARDT SET ARDT=REDT-0.00000001
  . FOR  SET ARDT=$ORDER(^GMR(120.5,"AA",DFN,VITIEN,ARDT)) QUIT:(+ARDT'>0)!(+ARDT>RSDT)  DO
  . . NEW ADT SET ADT=9999999-ARDT
  . . NEW IEN SET IEN=0
  . . FOR  SET IEN=$ORDER(^GMR(120.5,"AA",DFN,VITIEN,ARDT,IEN)) QUIT:+IEN'>0  DO
  . . . NEW ZN SET ZN=$GET(^GMR(120.5,IEN,0))
  . . . NEW N2 SET N2=$GET(^GMR(120.5,IEN,2))
  . . . IF $PIECE(N2,"^",1)="1" QUIT  ;"VITAL WAS ENTERED IN ERROR
  . . . SET OUT(DFN,ADT,VITIEN)=$PIECE(ZN,"^",8)_"^"_VITNAME
  . . . SET OUT(DFN,"V",VITIEN,ADT)=$PIECE(ZN,"^",8)_"^"_VITNAME
  . . . IF VITIEN=1,VITNAME="BLOOD PRESSURE" DO
  . . . . NEW VAL SET VAL=$PIECE(ZN,"^",8)
  . . . . SET OUT(DFN,ADT,1.1)=$PIECE(VAL,"/",1)_"^SYSTOLIC"
  . . . . SET OUT(DFN,ADT,1.2)=$PIECE(VAL,"/",2)_"^DIASTOLIC"
  . . . . SET OUT(DFN,"V",1.1,ADT)=$PIECE(VAL,"/",1)_"^SYSTOLIC"
  . . . . SET OUT(DFN,"V",1.2,ADT)=$PIECE(VAL,"/",2)_"^DIASTOLIC"
  . . . . SET OUT("V",1.1)="SYSTOLIC"
  . . . . SET OUT("V",1.2)="DIASTOLIC"
  . . . IF VERBOSE DO
  . . . . SET OUT(DFN,ADT,VITIEN,0)=ZN
  . . . . SET OUT(DFN,ADT,VITIEN,2)=N2
  QUIT
  ;