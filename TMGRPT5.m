TMGRPT5  ;TMG/kst TMG REPORTS  ;04/30/20, 3/24/21
         ;;1.0;TMG-LIB;**1**;04/30/20
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 04/30/20  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;
 ;"NOTE: 7/20/20, changed table width from 900 to 700 because the note font
 ;"      size was being decreased to accomodate the wide tables
HRAFORM(TMGDFN,MODE)
 ;"Purpose: This creates an HTML table for the Health Risk Assessment
 ;"         If MODE=0, then the table contains all questions with the patient's answers
 ;"         If MODE=1, then the table contains only those questions with answers that need
 ;"                    to be addressed by the physician
 NEW TMGRESULT
 SET MODE=+$G(MODE)
 NEW TOPICIEN,AWVLIST,HFARRAY
 NEW IDX SET IDX=0
 SET TOPICIEN=0
 ;"
 ;"Get all questions
 DO GTQUESTS(.AWVLIST)
 ;"
 ;"Get last date's health factors
 NEW DATE
 DO GETPTHFS(.HFARRAY,TMGDFN,.DATE)
 ;"
 ;"Cycle through the topics
 IF MODE=0 DO
 . SET TMGRESULT="<TABLE BORDER=1 width=""700"" ID=""tmg_risk_assessment""><CAPTION><B>HEALTH RISK ASSESSMENT QUESTIONNAIRE<BR>COMPLETED ON: "_$$EXTDATE^TMGDATE(DATE)_"</B><BR></CAPTION>"
 ELSE  DO
 . SET TMGRESULT="<TABLE BORDER=1 width=""700"" ID=""tmg_risk_assessment_physician_review"">"   ;""<TABLE BORDER=1 width=""700""><CAPTION><B>ANSWERS TO REVIEW<BR>COMPLETED ON: "_$$EXTDATE^TMGDATE(DATE)_"</B><BR></CAPTION>"
 . SET TMGRESULT=TMGRESULT_"<tr style=""background-color:"_$$COLOR("TOPIC")_"""><td>Question<br><B>Answer</B></td><td width=""350"">Physician's Comments</td></tr>"
 FOR  SET TOPICIEN=$O(AWVLIST(TOPICIEN)) QUIT:TOPICIEN'>0  DO
 . NEW TOPIC SET TOPIC=$G(AWVLIST(TOPICIEN,0))
 . IF MODE=0 DO  ;"SKIP FOR MODE 1 AND ONLY ADD BELOW IF NOT ALREADY ENTERED
 . . SET TMGRESULT=TMGRESULT_"<tr><th colspan=""2"" style=""background-color:"_$$COLOR("TOPIC")_""" >"_TOPIC_"</th></tr>"
 . NEW TOPICADDED SET TOPICADDED=0
 . NEW QUESTIEN SET QUESTIEN=0
 . ;"
 . ;"Cycle through the questions
 . FOR  SET QUESTIEN=$O(AWVLIST(TOPICIEN,QUESTIEN)) QUIT:QUESTIEN'>0  DO
 . . NEW QUESTION SET QUESTION=$G(AWVLIST(TOPICIEN,QUESTIEN,0))
 . . NEW QUESTIONLINE  ;"Keep track of one line 
 . . ;"
 . . ;"Now cycle through the answers.
 . . NEW ANSIEN SET ANSIEN=0
 . . NEW FOUND SET FOUND=0
 . . NEW HFIEN,COMMENT,COLOR,TEXT,DATA,ANSWER
 . . SET (COMMENT,COLOR,TEXT,DATA,ANSWER)=""
 . . ;"
 . . ;"Use FOUND to determine if question wasn't answered, or if multiple answers are found
 . . FOR  SET ANSIEN=$O(AWVLIST(TOPICIEN,QUESTIEN,ANSIEN)) QUIT:ANSIEN'>0  DO
 . . . SET DATA=$G(AWVLIST(TOPICIEN,QUESTIEN,ANSIEN))
 . . . SET HFIEN=$P(DATA,"^",1),COMMENT=+$P(DATA,"^",2)
 . . . SET COLOR=$P(DATA,"^",3),TEXT=$P(DATA,"^",4)
 . . . ;"
 . . . ;"If there is not a HF found for the patient, then quit out
 . . . IF '$D(HFARRAY(HFIEN)) QUIT  
 . . . ;"
 . . . ;"Set the question cell value
 . . . IF FOUND=1 DO
 . . . . SET QUESTION="       (continued)"
 . . . ;"
 . . . ;"Set the answer cell value
 . . . IF COMMENT=1 DO
 . . . . SET ANSWER=$P($G(HFARRAY(HFIEN)),"^",2)
 . . . ELSE  DO
 . . . . SET ANSWER=TEXT
 . . . SET FOUND=1
 . . . IF MODE=1 DO  ;"IF MODE 1 AND ANSWER HAS A COLOR THEN WE WANT IT REPORTED FOR PHYSICIAN REVIEW
 . . . . IF COLOR'="" DO
 . . . . . IF TOPICADDED=0 DO
 . . . . . . SET TMGRESULT=TMGRESULT_"<tr><th colspan=""2"" style=""background-color:"_$$COLOR("TOPIC")_""" >"_TOPIC_"</th></tr>"
 . . . . . . SET TOPICADDED=1
 . . . . . SET QUESTION=QUESTION_"<BR><B>"_ANSWER_"</B>"
 . . . . . DO SET1ROW(.TMGRESULT,QUESTION," .","")
 . . . ELSE  DO
 . . . . DO SET1ROW(.TMGRESULT,QUESTION,ANSWER,COLOR)
 . . ;"
 . . ;"If FOUND wasn't set, then write the row with NO RESPONSE FOUND
 . . ;"IF MODE=1 QUIT  ;"DON'T WRITE MISSING ANSWERS. WE MAY CHANGE THIS
 . . IF FOUND=0 DO
 . . . SET COLOR=""
 . . . IF MODE=1 SET COLOR="SEVERE"
 . . . SET ANSWER="NO RESPONSE FOUND"
 . . . IF DUZ'=168 DO
 . . . . DO SET1ROW(.TMGRESULT,QUESTION,ANSWER,COLOR)
 SET TMGRESULT=TMGRESULT_"</TABLE>"
 SET TMGRESULT=TMGRESULT_"<BR>PATIENT GIVEN COPY OF THIS PLAN"
 QUIT TMGRESULT
 ;"
SET1ROW(TMGRESULT,QUESTION,ANSWER,COLORTYPE)
 ;"Purpose: This will set the HTML tags for one row in the table, adding it to TMGRESULT
 NEW LINE,ENDTAG
 IF COLORTYPE'="" SET COLORTYPE=" style=""font-weight:bold;background-color:"_$$COLOR(COLORTYPE)_""""
 SET LINE="<tr"_COLORTYPE_"><td>"_QUESTION_"</td><td>"_ANSWER_"</td></tr>"
 SET TMGRESULT=TMGRESULT_LINE
 QUIT 
 ;"
GETPTHFS(HFARRAY,TMGDFN,LDATE)
 ;"Purpose: Return all health factors with the "TMG AWV" prefix
 ;"Format: HFARRAY(HFIEN)=HF Name^HF Comment^HF Date
 ;"
 NEW IEN SET IEN=0
 NEW DATE,TEMPARR,EXTDATE,HFIEN,TEMPARRAY
 SET LDATE=0
 ;"
 ;"Loop through all the patient's Health Factors, Gathering all AWV HFs in a temp array, indexed by date
 FOR  SET IEN=$ORDER(^AUPNVHF("C",TMGDFN,IEN)) QUIT:IEN'>0  DO
 . SET HFIEN=$PIECE($GET(^AUPNVHF(IEN,0)),"^",1)
 . IF HFIEN'>0 QUIT
 . NEW DATEIEN SET DATEIEN=$PIECE($GET(^AUPNVHF(IEN,0)),"^",3)
 . SET DATE=$PIECE($GET(^AUPNVSIT(DATEIEN,0)),"^",1)
 . IF DATE="" QUIT
 . ;"
 . ;"Use the TMG AWV QUESTIONNAIRE COMPLETED as the Date Completed, keeping track of the latest one
 . IF HFIEN=2548 DO   
 . . IF DATE>LDATE SET LDATE=$P(DATE,".",1)
 . ;"
 . NEW HFNAME SET HFNAME=$PIECE($GET(^AUTTHF(HFIEN,0)),"^",1)
 . IF HFNAME="" SET HFNAME="No name found for IEN: "_HFIEN
 . ;"FOR  QUIT:'$DATA(TEMPARR(DATE))  SET DATE=DATE+0.000001
 . IF HFNAME'["TMG AWV" QUIT
 . NEW HFCOMM SET HFCOMM=$GET(^AUPNVHF(IEN,811))
 . IF HFCOMM="" SET HFCOMM=" "
 . SET TEMPARRAY(DATE,HFIEN)=HFNAME_"^"_HFCOMM_"^"_DATE
 ;"
 ;"Loop through temp array, storing only the ones with dates matching last TMG AWV QUESTIONNAIRE COMPLETED
 NEW ONEDATE SET ONEDATE=LDATE
 FOR  SET ONEDATE=$O(TEMPARRAY(ONEDATE)) QUIT:(ONEDATE'>0)!(ONEDATE'[LDATE)  DO
 . SET HFIEN=0
 . FOR  SET HFIEN=$O(TEMPARRAY(ONEDATE,HFIEN)) QUIT:HFIEN'>0  DO
 . . SET HFARRAY(HFIEN)=$G(TEMPARRAY(ONEDATE,HFIEN))
 QUIT
 ;"
GTQUESTS(ARRAY)
  ;"Purpose: This creates an array for the Annual Wellness Visit questionnaire
  ;"Format:  ARRAY(#,0) = Section Name
  ;"         ARRAY(#,#,0) = Question Text
  ;"         ARRAY(#,#,#) = HF IEN^Comment Needed (0 or 1)^Color for answer^Text of answer
 NEW IDX SET IDX=0
 SET ARRAY(1,0)="Physical Activity"
   SET ARRAY(1,1,0)="Do you exercise regularly"
     SET ARRAY(1,1,1)="2714^0^^Yes"
     SET ARRAY(1,1,2)="2715^0^^No, but I am active"
     SET ARRAY(1,1,3)="2716^0^^No, due to physical limitations"
     SET ARRAY(1,1,4)="2717^0^^No"
   
   ;"SET ARRAY(1,1,0)="In the past 7 days, how many days did you exercise?"
   ;"  SET ARRAY(1,1,1)="2448^1^^"
     
   ;"SET ARRAY(1,2,0)="On days when you exercised, for how long did you exercise (in minutes) per day?"
   ;"  SET ARRAY(1,2,1)="2449^1^^"
   ;"  SET ARRAY(1,2,2)="2450^0^^Does not apply"
     
   ;"SET ARRAY(1,3,0)="How intense was your typical exercise?"
   ;"  SET ARRAY(1,3,1)="2424^0^^Light (like stretching or slow walking)"
   ;"  SET ARRAY(1,3,2)="2425^0^^Moderate (like brisk walking)"
   ;"  SET ARRAY(1,3,3)="2426^0^^Heavy (like jogging or swiming)"
   ;"  SET ARRAY(1,3,4)="2427^0^^Very heavy (like fast running or stair climbing)"
   ;"  SET ARRAY(1,3,5)="2428^0^MODERATE^I am currently not exercising"

 SET ARRAY(2,0)="Tobacco/Nicotine Use"
   SET ARRAY(2,1,0)="In the last 30 days, have you used tobacco? Smoked:"
     SET ARRAY(2,1,1)="2431^0^MODERATE^Yes"
     SET ARRAY(2,1,2)="2432^0^^No"
     
  SET ARRAY(2,2,0)="Vaped or used an e-cigarette?"
     SET ARRAY(2,2,1)="2719^0^MODERATE^Yes"
     SET ARRAY(2,2,2)="2721^0^^No"     

   SET ARRAY(2,3,0)="Used a smokeless tobacco product:"
     SET ARRAY(2,3,1)="2433^0^MODERATE^Yes"
     SET ARRAY(2,3,2)="2434^0^^No"
     
   SET ARRAY(2,4,0)="If Yes to either, Would you be interested in quitting tobacco use within the next month?"
     SET ARRAY(2,4,1)="2436^0^MODERATE^Yes"
     SET ARRAY(2,4,2)="2437^0^^No"
     SET ARRAY(2,4,2)="2718^0^^N/A"

 SET ARRAY(3,0)="Alcohol Use"
   SET ARRAY(3,1,0)="In the past 7 days, on how many days did you drink alcohol?"
     SET ARRAY(3,1,1)="2451^1^^"

   SET ARRAY(3,2,0)="On days when you drank alcohol, how often did you have alcoholic drinks on one occasion? (5 or more for men, 4 or more for women and those men and women 65 years old or over)" 
     SET ARRAY(3,2,1)="2438^0^^Never"
     SET ARRAY(3,2,2)="2439^0^^Once during the week"
     SET ARRAY(3,2,3)="2440^0^MODERATE^2-3 times during the week"
     SET ARRAY(3,2,4)="2452^0^SEVERE^More than 3 times during the week"
     
   SET ARRAY(3,3,0)="Do you ever drive after drinking, or ride with a driver who has been drinking?"
     SET ARRAY(3,3,1)="2446^0^SEVERE^Yes"
     SET ARRAY(3,3,2)="2447^0^^No"   

 ;"SET ARRAY(4,0)="Nutrition"
 ;"  SET ARRAY(4,1,0)="In the past 7 days, how many servings of fruits and vegetables did you typically eat each day? (1 serving = 1 cup of fresh vegetables, 1/2 cup of cooked vegetables, or 1 medium piece of fruit. 1 cup = size of a baseball.)"
 ;"    SET ARRAY(4,1,1)="2542^1^^"
     
 ;"  SET ARRAY(4,2,0)="In the past 7 days, how many servings of high fiber or whole grain foods did you typically eat each day? (1 serving = 1 slice of 100% whole wheat bread, 1 cup of whole-grain or high-fiber ready-to-eat cereal, 1/2 cup of cooked cereal such as oatmeal, or 1/2 cup of cooked brown rice or whole wheat pasta.)"
 ;"    SET ARRAY(4,2,1)="2543^1^^"
   
 ;"  SET ARRAY(4,3,0)="In the past 7 days, how many servings of fried or high-fat foods did you typically eat each day? (Examples include fried chicken, fried fish, bacon, French fries, potato chips, corn chips, doughnuts, creamy salad dressings, and foods made with whole milk, cream, cheese, or mayonnaise.)"
 ;"    SET ARRAY(4,3,1)="2546^1^^"
   
 ;"  SET ARRAY(4,4,0)="In the past 7 days, how many sugar-sweetened (not diet) beverages did you typically consume each day?"
 ;"    SET ARRAY(4,4,1)="2547^1^^"
   
 SET ARRAY(5,0)="Seat Belt Use"
   SET ARRAY(5,1,0)="Do you always fasten your seat belt when you are in a car?"
     SET ARRAY(5,1,1)="2453^0^^Yes"
     SET ARRAY(5,1,2)="2454^0^MODERATE^No"    

 ;"SET ARRAY(6,0)="Depression"
 ;"  SET ARRAY(6,1,0)="In the past 2 weeks, how often have you felt down, depressed, or hopeless?"
 ;"    SET ARRAY(6,1,1)="2469^0^SEVERE^Almost all of the time"
 ;"    SET ARRAY(6,1,2)="2470^0^MODERATE^Most of the time"
 ;"    SET ARRAY(6,1,3)="2471^0^MILD^Some of the time"
 ;"    SET ARRAY(6,1,4)="2472^0^^Almost never"
   
 ;"  SET ARRAY(6,2,0)="In the past 2 weeks, how often have you felt little interest or pleasure in doing things?"
 ;"    SET ARRAY(6,2,1)="2461^0^SEVERE^Almost all of the time"
 ;"    SET ARRAY(6,2,2)="2462^0^MODERATE^Most of the time"
 ;"    SET ARRAY(6,2,3)="2463^0^MILD^Some of the time"
 ;"    SET ARRAY(6,2,4)="2464^0^^Almost never"   

 ;"  SET ARRAY(6,3,0)="Have your feelings caused you distress or interfered with your ability to get along socially with family or friends?" 
 ;"    SET ARRAY(6,3,1)="2465^0^SEVERE^Yes"
 ;"    SET ARRAY(6,3,2)="2468^0^^No"   

 ;"SET ARRAY(7,0)="Anxiety"
 ;"  SET ARRAY(7,1,0)="In the past 2 weeks, how often have you felt nervous, anxious, or on edge?"
 ;"    SET ARRAY(7,1,1)="2473^0^SEVERE^Almost all of the time"
 ;"    SET ARRAY(7,1,2)="2478^0^MODERATE^Most of the time"
 ;"    SET ARRAY(7,1,3)="2479^0^MILD^Some of the time"
 ;"    SET ARRAY(7,1,4)="2480^0^^Almost never"     

 ;"  SET ARRAY(7,2,0)="In the past 2 weeks, how often were you not able to stop worrying or control your worrying?"
 ;"    SET ARRAY(7,2,1)="2455^0^SEVERE^Almost all of the time"
 ;"    SET ARRAY(7,2,2)="2458^0^MODERATE^Most of the time"
 ;"    SET ARRAY(7,2,3)="2459^0^MILD^Some of the time"
 ;"    SET ARRAY(7,2,4)="2460^0^^Almost never"  

 ;"SET ARRAY(8,0)="High Stress"
 ;"  SET ARRAY(8,1,0)="How often is stress a problem for you in handling such things as: –Your health? –Your finances? –Your family or social relationships? –Your work?" 
 ;"    SET ARRAY(8,1,1)="2481^0^^Never or rarely"
 ;"    SET ARRAY(8,1,2)="2486^0^MILD^Sometimes"
 ;"    SET ARRAY(8,1,3)="2487^0^MODERATE^Often"
 ;"    SET ARRAY(8,1,4)="2488^0^SEVERE^Always"     

 ;"SET ARRAY(9,0)="Social/Emotional Support"
 ;"  SET ARRAY(9,1,0)="How often do you get the social and emotional support you need:"
 ;"    SET ARRAY(9,1,1)="2489^0^^Always"
 ;"    SET ARRAY(9,1,2)="2490^0^^Usually"
 ;"    SET ARRAY(9,1,3)="2495^0^MILD^Sometimes"
 ;"    SET ARRAY(9,1,4)="2496^0^MODERATE^Rarely"
 ;"    SET ARRAY(9,1,5)="2497^0^SEVERE^Never" 

 ;"SET ARRAY(10,0)="Pain"
 ;"  SET ARRAY(10,1,0)="In the past 7 days, how much pain have you felt?"
 ;"    SET ARRAY(10,1,1)="2498^0^^None"
 ;"    SET ARRAY(10,1,2)="2499^0^MODERATE^Some"
 ;"    SET ARRAY(10,1,3)="2504^0^SEVERE^A lot"   

 ;"SET ARRAY(11,0)="General Health"
 ;"  SET ARRAY(11,1,0)="In general, would you say your health is"
 ;"    SET ARRAY(11,1,1)="2505^0^^Excellent"
 ;"    SET ARRAY(11,1,2)="2506^0^^Very Good"
 ;"    SET ARRAY(11,1,3)="2507^0^^Good"
 ;"    SET ARRAY(11,1,4)="2508^0^MODERATE^Fair"
 ;"    SET ARRAY(11,1,5)="2511^0^SEVERE^Poor"    

 ;"  SET ARRAY(11,2,0)="How would you describe the condition of your mouth and teeth—including false teeth or dentures?"
 ;"    SET ARRAY(11,2,1)="2513^0^^Excellent"
 ;"    SET ARRAY(11,2,2)="2514^0^^Very Good"
 ;"    SET ARRAY(11,2,3)="2516^0^^Good"
 ;"    SET ARRAY(11,2,4)="2517^0^MODERATE^Fair"
 ;"    SET ARRAY(11,2,5)="2518^0^SEVERE^Poor"

 SET ARRAY(12,0)="Activities of Daily Living"
   SET ARRAY(12,1,0)="In the past 7 days, did you need help from others to perform everyday activities such as eating, getting dressed, grooming, bathing, walking, or using the toilet?"
     SET ARRAY(12,1,1)="2519^0^SEVERE^Yes"
     SET ARRAY(12,1,2)="2524^0^^No"

 SET ARRAY(13,0)="Instrumental Activities of Daily Living"
   SET ARRAY(13,1,0)="In the past 7 days, did you need help from others to take care of things such as laundry and housekeeping, banking, shopping, using the telephone, food preparation, transportation, or taking your own medications?"
     SET ARRAY(13,1,1)="2525^0^SEVERE^Yes"
     SET ARRAY(13,1,2)="2526^0^^No"

 SET ARRAY(14,0)="Sleep"
   SET ARRAY(14,1,0)="Each night, how many hours of sleep do you usually get?"
     SET ARRAY(14,1,1)="2530^1^^"

   SET ARRAY(14,2,0)="Do you snore or has anyone told you that you snore?"
     SET ARRAY(14,2,1)="2532^0^MODERATE^Yes"
     SET ARRAY(14,2,2)="2533^0^^No"
     SET ARRAY(14,2,3)="2762^0^^Yes, but benefitting from CPAP"

   SET ARRAY(14,3,0)="In the past 7 days, how often have you felt sleepy during the daytime?"
     SET ARRAY(14,3,1)="2534^0^SEVERE^Always"
     SET ARRAY(14,3,2)="2535^0^MODERATE^Usually"
     SET ARRAY(14,3,3)="2537^0^^Sometimes"
     SET ARRAY(14,3,4)="2538^0^^Rarely"
     SET ARRAY(14,3,5)="2539^0^^Never"
     SET ARRAY(14,3,6)="5777^0^^Yes, but benefitting from CPAP"
     
 SET ARRAY(15,0)="Fall Risk"
   SET ARRAY(15,1,0)="Any falls in the past year?"
     SET ARRAY(15,1,1)="2550^0^MILD^Yes"
     SET ARRAY(15,1,2)="2551^0^^No"
     
   SET ARRAY(15,2,0)="Do you have any worries about falling or feel unsteady when standing or walking?"
     SET ARRAY(15,2,1)="2553^0^MODERATE^Yes"
     SET ARRAY(15,2,2)="2555^0^^No"   
     
   SET ARRAY(15,3,0)="Do you have problems with your vision that affect your ability to safely walk?"
     SET ARRAY(15,3,1)="2557^0^MODERATE^Yes"
     SET ARRAY(15,3,2)="2558^0^^No"     
   
   SET ARRAY(15,4,0)="Observed gait"
     SET ARRAY(15,4,1)="2561^0^^Normal"
     SET ARRAY(15,4,2)="2562^0^SEVERE^Mildly impaired"
     SET ARRAY(15,4,3)="2563^0^SEVERE^Moderately impaired"
     SET ARRAY(15,4,4)="2564^0^SEVERE^Non-Ambulatory"
     
   SET ARRAY(15,5,0)="Do you use any assistance devices? (Check all that apply)"
     SET ARRAY(15,5,1)="2567^0^^None"
     SET ARRAY(15,5,2)="2565^0^^Cane"
     SET ARRAY(15,5,3)="2580^0^^Walker"
     SET ARRAY(15,5,4)="2584^0^^Rollator"
     SET ARRAY(15,5,5)="2585^0^^Wheel Chair"
     SET ARRAY(15,5,6)="2587^0^^Standby assist"
     SET ARRAY(15,5,7)="2591^0^^Grab Bar (Bathroom)"
     
   SET ARRAY(15,6,0)="Have you reviewed your home for safety hazards (adequate lighting, loose rugs)?"
     SET ARRAY(15,6,1)="2593^0^^Yes"
     SET ARRAY(15,6,2)="2594^0^MODERATE^No"
     
 SET ARRAY(16,0)="Hearing"
   SET ARRAY(16,1,0)="Have you noticed any hearing difficulties?"
     SET ARRAY(16,1,1)="2750^0^SEVERE^Yes"
     SET ARRAY(16,1,2)="2751^0^^No"
     SET ARRAY(16,1,3)="5760^0^^Yes, but corrected with hearing aids"
     
 SET ARRAY(17,0)="Pain"
   SET ARRAY(17,1,0)="Have you had significant pain in the last week?"
     SET ARRAY(17,1,1)="5761^0^^None"
     SET ARRAY(17,1,2)="5762^0^SEVERE^Mild"
     SET ARRAY(17,1,3)="5763^0^SEVERE^Moderate"
     SET ARRAY(17,1,4)="5764^0^SEVERE^Severe"
     
 QUIT
 ;"
COLOR(COLORWORD)  ;"
 ;"IF COLORWORD="SEVERE" QUIT "lightpink"
 ;"IF COLORWORD="MODERATE" QUIT "lightyellow"
 ;"IF COLORWORD="MILD" QUIT "lightcyan"
 ;"IF COLORWORD="TOPIC" QUIT "#99CCFF"
 IF COLORWORD="SEVERE" QUIT "#FF8080"
 IF COLORWORD="MODERATE" QUIT "#FFB3B3"
 IF COLORWORD="MILD" QUIT "#FFE6E6"
 IF COLORWORD="TOPIC" QUIT "#CCDDFF"
 QUIT ""
 ;"
HFFORREM(HFLIST)  ;"HF TO USE FOR COMPLETED REMINDERS
 SET HFLIST(223)="TMG COLONOSCOPY COMPLETED"
 ;"SET HFLIST(0)="NOT USED CURRENTLY"
 QUIT
 ;"
HELHRPT(TMGDFN) 
   ;"Purpose: Entry point, as called from CPRS REPORT system
  ;"Input: ROOT -- Pass by NAME.  This is where output goes
  ;"       TMGDFN -- Patient DFN ; ICN for foriegn sites
  ;"       ID --
  ;"       ALPHA -- Start date (lieu of DTRANGE)
  ;"       OMEGA -- End date (lieu of DTRANGE)
  ;"       DTRANGE -- # days back from today
  ;"       REMOTE --
  ;"       MAX    --
  ;"       ORFHIE --
  ;"Result: None.  Output goes into @ROOT
  ;"NEW THHEAD SET THHEAD="<TH style=""background-color:"_$$COLOR("TOPIC")_""">"
  NEW HD SET HD="<TABLE BORDER=3><CAPTION><B>PERSONALIZED PREVENTION PLAN OF SERVICE</B></CAPTION><TR style=""background-color:"_$$COLOR("TOPIC")_"""><TH>ITEM</TH>"
  SET HD=HD_"<TH>STATUS</TH><TH>LAST DONE</TH><TH>DUE DATE</TH><TH>FREQUENCY</TH></TR>"
  ;"SET HD=HD_"<TH>STATUS</TH></TR>"
  NEW SUFFIXARR DO GETSUFFIX(TMGDFN,.SUFFIXARR)
  NEW REMIEN,REMLIST SET REMIEN=0
  DO AWVREMS^TMGRPT2(.REMLIST)
  NEW HFFORREMS DO HFFORREM(.HFFORREMS)
  FOR  SET REMIEN=$O(REMLIST(REMIEN)) QUIT:REMIEN'>0  DO
  . NEW AGE SET AGE=$$AGE^TIULO(TMGDFN)
  . IF (REMIEN=223)&(AGE>75) QUIT  ;"SPECIAL RULE FOR COLONOSCOPY ONLY    4/13/21
  . NEW REMRESULT
  . SET REMRESULT=$$DOREM^TMGPXR03(TMGDFN,REMIEN,5,$$TODAY^TMGDATE,1)
  . NEW STATUS,DUE,DONE,FREQ,REMNAME
  . SET REMNAME=$P($G(REMLIST(REMIEN)),"^",1),FREQ=$P($G(REMLIST(REMIEN)),"^",2)
  . SET STATUS=$P(REMRESULT,"^",1),DONE=$P($P(REMRESULT,"^",3),"@",1),DUE=$P($P(REMRESULT,"^",2),"@",1)
  . NEW STATUSTOKEEP SET STATUSTOKEEP="DUE NOW^DUE SOON^RESOLVED^DONE"
  . IF STATUSTOKEEP'[STATUS QUIT
  . IF (STATUS="RESOLVED")!(STATUS="DONE") SET STATUS="Up To Date"
  . NEW HFDONE SET HFDONE=0
  . IF $D(HFFORREMS(REMIEN)) DO   
  . . NEW HFNAME SET HFNAME=$GET(HFFORREMS(REMIEN))
  . . IF HFNAME="" QUIT
  . . NEW HF,OPTION SET HFDONE=$$GETHFDT^TMGPXRU1(TMGDFN,HFNAME,.HFARRAY,.OPTION)
  . . IF +HFDONE>0 DO
  . . . SET DONE=$$EXTDATE^TMGDATE(HFDONE,1)
  . . ELSE  DO
  . . . IF DONE'="" DO
  . . . . SET DONE="NO RECORD OF BEING DONE<BR>REMINDER COMPLETED ON "_DONE
  . . . ELSE  DO
  . . . . SET DONE="NO RECORD OF BEING DONE"
  . IF DONE="" SET DONE="NO RECORD OF BEING DONE"
  . IF DUE="" DO 
  . . SET DUE="NO DUE DATE"
  . IF REMIEN=33 DO
  . . SET DUE="09/01/"_$P(DUE,"/",3)  ;"FORCE FLU REMINDER TO BE DUE ON Sept 1 of the year
  . IF REMIEN=231 DO    ;"Adv care plan, set to 1D follow up to force each time, so we need to set STATUS properly
  . . IF (DUE="None")&(STATUS="DUE NOW") DO
  . . . SET STATUS="Up To Date"
  . . . NEW NEWDUE SET NEWDUE=$P($G(REMRESULT),"^",3)
  . . . SET NEWDUE=$$INTDATE^TMGDATE($P(NEWDUE,"@",1))
  . . . IF NEWDUE>0 DO
  . . . . SET DUE=$$ADDDAYS^TMGDATE(730,NEWDUE)
  . . . . SET DUE=$$EXTDATE^TMGDATE(DUE,2)
  . IF REMIEN=298 DO    ;"Low dose CT, set to 1D follow up to force each time, so we need to set STATUS properly
  . . IF (DUE="None")&(STATUS="DUE NOW") DO
  . . . SET STATUS="Up To Date"
  . . . NEW NEWDUE SET NEWDUE=$P($G(REMRESULT),"^",3)
  . . . SET NEWDUE=$$INTDATE^TMGDATE($P(NEWDUE,"@",1))
  . . . IF NEWDUE>0 DO
  . . . . SET DUE=$$ADDDAYS^TMGDATE(365,NEWDUE)
  . . . . IF DUE<$$TODAY^TMGDATE DO
  . . . . . SET STATUS="DUE NOW"
  . . . . SET DUE=$$EXTDATE^TMGDATE(DUE,2)
  . IF REMIEN=224 SET REMNAME=REMNAME_$G(SUFFIXARR("MAMMO"))
  . IF REMIEN=228 SET REMNAME=REMNAME_$G(SUFFIXARR("BONEDENSITY"))
  . IF REMIEN=225 SET REMNAME=REMNAME_$G(SUFFIXARR("AAA"))
  . IF REMIEN=33 SET REMNAME=REMNAME_$G(SUFFIXARR("FLU"))
  . IF REMIEN=298 SET REMNAME=REMNAME_$G(SUFFIXARR("LOWDOSECT"))
  . IF REMIEN=241 SET REMNAME=REMNAME_$G(SUFFIXARR("P13"))
  . IF REMIEN=257 SET REMNAME=REMNAME_$G(SUFFIXARR("P23"))
  . IF REMIEN=220 SET REMNAME=REMNAME_$G(SUFFIXARR("FECAL"))
  . IF REMIEN=223 SET REMNAME=REMNAME_$G(SUFFIXARR("COLON"))
  . IF REMIEN=300 SET REMNAME=REMNAME_$G(SUFFIXARR("SHINGRIX"))
  . IF FREQ="" SET FREQ="NO FREQUENCY DEFINED"
  . SET TMGRESULT(REMIEN)=REMNAME_"^"_STATUS_"^"_DONE_"^"_DUE_"^"_FREQ
  . ;"SET TMGRESULT(REMIEN)=$G(REMLIST(REMIEN))_"^"_STATUS
  NEW TMGOUT
  DO SETHTML(.TMGOUT,.TMGRESULT,HD,5)
  ;"DO SETHTML(.TMGOUT,.TMGRESULT,HD,2)
  ;"KILL TMGRESULT
  ;"SET TMGRESULT=TMGOUT
  QUIT TMGOUT
  ;"
GETSUFFIX(TMGDFN,SUFFIXARR)  ;"ADDS ANY NEEDED SUFFIXES TO THE ARRAY TO BE PULLED AND INSERTED INTO THE HEALTH REPORT
  NEW VHFIEN SET VHFIEN=0
  NEW TODAY SET TODAY=$$TODAY^TMGDATE
  FOR  SET VHFIEN=$O(^AUPNVHF("C",TMGDFN,VHFIEN)) QUIT:VHFIEN'>0  DO
  . NEW HFIEN,VDATE
  . SET VDATE=$P($G(^AUPNVHF(VHFIEN,0)),"^",3),HFIEN=$P($G(^AUPNVHF(VHFIEN,0)),"^",1)
  . SET VDATE=$P($G(^AUPNVSIT(VDATE,0)),"^",1),VDATE=$P(VDATE,".",1)
  . IF TODAY'=VDATE QUIT
  . IF HFIEN=730 SET SUFFIXARR("MAMMO")=" (Ordered today)"
  . IF HFIEN=811 SET SUFFIXARR("BONEDENSITY")=" (Ordered today)"
  . IF HFIEN=2705 SET SUFFIXARR("FECAL")=" (Ordered today)"
  . IF HFIEN=798 SET SUFFIXARR("FECAL")=" (Ordered today)"
  . IF HFIEN=800 SET SUFFIXARR("FECAL")=" (Ordered today)"
  . IF HFIEN=801 SET SUFFIXARR("FECAL")=" (Ordered today)"
  . IF HFIEN=730 SET SUFFIXARR("MAMMO")=" (Ordered today)"
  . IF HFIEN=844 SET SUFFIXARR("AAA")=" (Ordered today)"
  . IF HFIEN=2746 SET SUFFIXARR("FLU")=" (Ordered today)"
  . IF HFIEN=2749 SET SUFFIXARR("P23")=" (Ordered today)"
  . IF HFIEN=2748 SET SUFFIXARR("P13")=" (Ordered today)"
  . IF HFIEN=2747 SET SUFFIXARR("SHINGRIX")=" (Ordered today)"
  . IF HFIEN=762 SET SUFFIXARR("COLON")=" (Ordered today)"
  . IF HFIEN=864 SET SUFFIXARR("LOWDOSECT")="(Ordered today)"
  QUIT
  ;"
BIOTBL(TMGDFN)  ;"
  ;"Purpose: This table returns the patients most recent BP, Lipids, Glucose, Weight
  NEW TMGRESULT,TMGOUT SET TMGRESULT=""
  NEW OUT
  NEW HD SET HD="<TABLE BORDER=3><CAPTION><B>MOST RECENT BIOMETRIC MEASURES</B></CAPTION><TR style=""background-color:"_$$COLOR("TOPIC")_"""><TH>MEASUREMENT</TH>"
  SET HD=HD_"<TH>VALUE</TH><TH>DATE</TH></TR>"     ;"<TH>RESULT</TH></TR>"  
  ;"BP
  NEW BP,BPDATE SET BP=$$TREND^TMGGMRV1(TMGDFN,"T","BP",1,"",1) ;" <-Get last BP with date
  SET BPDATE=$P($P(BP,"(",2),")",1),BP=$P(BP," ",1)
  SET TMGRESULT(1)="BP^"_BP_"^"_BPDATE    ;"_"^OK"
  ;"LIPIDS
  NEW CHOL SET CHOL=$$GETLLAB(TMGDFN,183)
  SET TMGRESULT(2)="Cholesterol^"_$P(CHOL,"^",1)_"^"_$P(CHOL,"^",2)  ;"_"^GOOD"
  ;"GLUCOSE
  NEW GLU SET GLU=$$GETLLAB(TMGDFN,175)
  SET TMGRESULT(3)="Glucose^"_$P(GLU,"^",1)_"^"_$P(GLU,"^",2)  ;"^GREAT"
  ;"WEIGHT
  NEW WT,WTDATE SET WT=$$TREND^TMGGMRV1(TMGDFN,"T","WT",1,"",1) ;" <-Get last BP with date
  SET WTDATE=$P($P(WT,"(",2),")",1),WT=$P(WT," ",1)
  SET TMGRESULT(4)="Weight^"_WT_"^"_WTDATE  ;"_"^OK"
  DO SETHTML(.TMGOUT,.TMGRESULT,HD,3)
  QUIT TMGOUT
  ;"
GETLLAB(TMGDFN,LABNUM)  ;"RETURN LAST VALUE AND DATE
  NEW TMGRESULT,OUT SET TMGRESULT="-1^NO RESULT"
  DO GETVALS^TMGLRR01(TMGDFN_"^2",LABNUM,.OUT)
  NEW LABSTR SET LABSTR=LABNUM
  SET LABSTR=$O(OUT(LABSTR))
  NEW DATE SET DATE=$O(OUT(LABSTR,9999999),-1)
  IF DATE'>0 GOTO GLDN
  SET TMGRESULT=$G(OUT(LABSTR,DATE))_"^"_$$EXTDATE^TMGDATE(DATE,1)  
GLDN
  QUIT TMGRESULT
  ;"
SETHTML(ROOT,RESULTS,HEADING,COLNUMS)  ;
  ;"Input: ROOT -- AN OUT PARAMETER 
  ;"          ROOT(1)= HEADING
  ;"          ROOT(2)=one long string with HTML codes.
  ;"          ROOT(3)=END OF TABLE                
  ;"       RESULTS -- INPUT DATA.  Pass by reference.  Format:  
  ;"            RESULT(#)=<COL1>^<COL2)^<COL3>
  ;"       HEADING -- Column titles, carot deliminated
  ;"             <Title1>^<Title2>^<Title3>
  ;"       COLNUM -- number of colums
  ;"Results -- none
  NEW END SET END=3
  MERGE ^EDDIE("TMGRPT2")=RESULTS
  NEW DATA
  SET ROOT=""  ;""<TABLE BORDER=1 width=""700"">"  ;"<CAPTION><B>"_TITLE_"</CAPTION></B>"
  SET DATA=HEADING
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(RESULTS(IDX)) QUIT:IDX'>0  DO
  . IF $DATA(RESULTS(IDX,"HEADING")) DO
  . . SET DATA=DATA_"<TR bgcolor=#c4e3ed align=""center"">"
  . ELSE  DO
  . . SET DATA=DATA_"<TR>"
  . NEW PIECE
  . FOR PIECE=1:1:COLNUMS  DO
  . . SET DATA=DATA_"<TD>"_$PIECE($GET(RESULTS(IDX)),"^",PIECE)_"</TD>"
  . ;SET DATA=DATA_"<TD>"_$GET(RESULTS(IDX))_"</TD>"
  . SET DATA=DATA_"</TR>"
  . SET END=END+1
  SET ROOT=ROOT_DATA
  SET ROOT=ROOT_"</TABLE>"
  QUIT   
  ;"
DRVSPACE()  ;"
  ;" Check used drive space and alert if low
  NEW CMD,OUT SET CMD="df -h"
  NEW RESULT SET RESULT=$$LINUXCMD^TMGKERNL(CMD,.OUT)
  ;"
  ;" READ COMMAND RESULTS AND STORE IN ARRAY TO BE ANALYZED
  NEW IDX SET IDX=0
  NEW HEADERS,DATA
  FOR  SET IDX=$O(OUT(IDX)) QUIT:IDX'>0  DO
  . NEW DRIVE
  . NEW LINE SET LINE=$G(OUT(IDX))
  . NEW THISPIECE SET THISPIECE=0
  . NEW NUMPIECES,I
  . SET NUMPIECES=$L(LINE," ")
  . FOR I=1:1:NUMPIECES DO
  . . NEW PIECE SET PIECE=$P(LINE," ",I)
  . . IF PIECE'="" DO
  . . . SET THISPIECE=THISPIECE+1
  . . . IF IDX=1 DO
  . . . . SET HEADERS(THISPIECE)=PIECE
  . . . ELSE  DO
  . . . . IF THISPIECE=1 DO
  . . . . . SET DRIVE=PIECE
  . . . . ELSE  DO
  . . . . . SET DATA(DRIVE,$GET(HEADERS(THISPIECE)))=PIECE
  ;"
  NEW DRIVE SET DRIVE=""
  FOR  SET DRIVE=$O(DATA(DRIVE)) QUIT:DRIVE=""  DO
  . NEW AVAIL SET AVAIL=+$G(DATA(DRIVE,"Use%"))
  . IF AVAIL<95 QUIT
  . NEW ALERT,ALRTRESULT SET ALERT="DRIVE "_DRIVE_" is running low on space. "_$G(DATA(DRIVE,"Use%"))_" Remaining."
  . DO INFRMALT^TMGXQAL(.ALRTRESULT,150,ALERT)
  QUIT
  ;"
  