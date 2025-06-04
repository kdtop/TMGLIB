TMGMISC4        ;TMG/kst/Misc utility librar ;9/29/22
                ;;1.0;TMG-LIB;**1**;9/29/22
 ;
 ;"TMG CPRS GARBLING / SCRAMBLING FUNCTIONS, FOR HIDING PATIENT PHI
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 9/29/22  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;" RPC -- Public Functions.
 ;"=======================================================================
 ;" <none>
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;" ;               
 ;"=======================================================================
 ;"Dependencies:
 ;" ;
 ;"=======================================================================
 ;
SETGARBLE(VALUE) ;
  ;"USES DUZ IN GLOBAL SCOPE, IF AVAILABLE.  
  IF $GET(DUZ)>0 DO
  . SET ^TMG("TMP","GARBLE",DUZ)=VALUE
  ELSE  DO
  . SET ^TMG("TMP","GARBLE")=VALUE
  QUIT
  ;
SHOULDGARBLE(ADUZ) ;
  NEW RESULT SET RESULT=0
  IF $GET(ADUZ)>0 DO
  . SET RESULT=($GET(^TMG("TMP","GARBLE",ADUZ))=1)
  ELSE  IF $GET(DUZ)>0 DO
  . SET RESULT=($GET(^TMG("TMP","GARBLE",DUZ))=1)
  ELSE  DO
  . SET RESULT=($GET(^TMG("TMP","GARBLE"))=1)
  QUIT RESULT
  ;                                                
GARBLERESULTS(ARR)  ;
  ;"Purpose: Garble the outputs.  This will be used when demonstrating CPRS, while hiding patient names
  ;"Initially, this is called from LISTALL^TMGHRPC2
  NEW ARR2
  NEW CT SET CT=""
  FOR  SET CT=$ORDER(ARR(CT)) QUIT:CT=""  DO
  . NEW ENTRY SET ENTRY=$GET(ARR(CT))
  . NEW VAL SET VAL=$PIECE(ENTRY,U,2)
  . SET VAL=$$GARBLENAME(VAL)
  . SET $PIECE(ENTRY,U,2)=VAL
  . SET $PIECE(ENTRY,U,6)=VAL
  . SET ARR2(CT)=ENTRY
  KILL ARR MERGE ARR=ARR2
  QUIT
  ;
GARBLENAME(VAL)  ;
  NEW IDX FOR IDX=1:1:$LENGTH(VAL," ") DO           
  . NEW PART SET PART=$PIECE(VAL," ",IDX)
  . IF $$ISNUM^TMGSTUT3(PART)=1 QUIT
  . NEW JDX FOR JDX=1:1:$LENGTH(PART,",") DO
  . . NEW SUBPART SET SUBPART=$PIECE(PART,",",JDX)
  . . NEW NORM SET NORM=$EXTRACT(SUBPART,1,2)  ;"KEEP FIRST 2 CHARS UNGARBLED. 
  . . NEW GARB SET GARB=$EXTRACT(SUBPART,3,$LENGTH(SUBPART))
  . . SET GARB=$$GARBLESTR(GARB,"L")
  . . SET $PIECE(PART,",",JDX)=NORM_GARB
  . SET $PIECE(VAL," ",IDX)=PART
  QUIT VAL
  ;        
RANDOMDOB() ;"Make random DOB in FM date format
  NEW DOB 
  SET DOB="1"_$$RJ^XLFSTR($RANDOM(100),2,0)  ;"YEAR:L 1800-1899
  SET DOB=DOB_$$RJ^XLFSTR(($RANDOM(12)+1),2,0) ;"MONTH
  SET DOB=DOB_$$RJ^XLFSTR(($RANDOM(28)+1),2,0) ;"DAY
  QUIT DOB
  ;
GARBLESTR(STR,FLAGS) ;"Garble string to random characters, keeping length the same
  QUIT $$RANDSTR^TMGSTUT3($LENGTH(STR),.FLAGS)
  ;  
GARBLEPTNAME(REF,ADFN) ;"Ensure document array doesn't include patients name or DOB or other PHI
  ;"Input: REF -- Global Reference.  format @REF(#)=<document text>
  ;"       ADFN -- PATIENT IEN
  NEW TMGDEBUG SET TMGDEBUG=0
  IF TMGDEBUG=1 DO
  . KILL @REF MERGE @REF=^TMG("TMP","DEBUG","GARBLEPTNAME^TMGMISC4","@REF")
  . SET ADFN=+$GET(^TMG("TMP","DEBUG","GARBLEPTNAME^TMGMISC4","ADFN"))
  ELSE  DO
  . KILL ^TMG("TMP","DEBUG","GARBLEPTNAME^TMGMISC4","@REF")
  . MERGE ^TMG("TMP","DEBUG","GARBLEPTNAME^TMGMISC4","@REF")=@REF
  . SET ^TMG("TMP","DEBUG","GARBLEPTNAME^TMGMISC4","ADFN")=ADFN
  IF $GET(REF)="" QUIT
  NEW ARR MERGE ARR=@REF
  NEW CHANGED SET CHANGED=0
  NEW NAME SET NAME=$PIECE($GET(^DPT(ADFN,0)),"^",1)
  SET NAME=$TRANSLATE(NAME,"-,","  ")
  NEW MATCH,REPLACE DO SETUPMATCH(.MATCH,.REPLACE,ADFN)    
  NEW MATCHNUM SET MATCHNUM=$ORDER(MATCH(""),-1)        
  NEW KVMATCH,KVREPLACE DO SETUPKVMATCH(.KVMATCH,.KVREPLACE,ADFN)    
  NEW KVMATCHNUM SET KVMATCHNUM=$ORDER(KVMATCH(""),-1)        
  NEW ZEROBASED SET ZEROBASED=0
  NEW REDACTING SET REDACTING=0
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:IDX'>0  DO
  . NEW LINE,OLDLINE
  . IF ZEROBASED SET LINE=$GET(ARR(IDX,0)) 
  . ELSE  DO
  . . SET LINE=$GET(ARR(IDX))
  . . IF LINE="",$DATA(ARR(IDX,0))>0 SET LINE=$GET(ARR(IDX,0)),ZEROBASED=1  
  . QUIT:LINE=""
  . SET OLDLINE=LINE
  . IF REDACTING DO
  . . IF (LINE'["<LI>")&(LINE'["<li>") SET LINE="#" QUIT
  . . SET LINE="<li>"_$PIECE(LINE,"<li>",2,999)
  . . SET REDACTING=0
  . IF (0=1),LINE["Social",(LINE["<LI>")!(LINE["<li>") DO
  . . NEW PARTA,PARTB 
  . . SET PARTA=$PIECE(LINE,"SOCIAL",1)_"SOCIAL"
  . . SET PARTB=$PIECE(LINE,"SOCIAL",2)
  . . SET LINE=PARTA_$PIECE(PARTB,"<li>",2,999)
  . . SET REDACTING=1
  . NEW PART FOR PART=1:1:MATCHNUM DO
  . . NEW ONEMATCH SET ONEMATCH=MATCH(PART) QUIT:(ONEMATCH="")!(ONEMATCH=" ")
  . . FOR  QUIT:(LINE'[ONEMATCH)  DO
  . . . NEW PARTA SET PARTA=$PIECE(LINE,ONEMATCH,1)
  . . . NEW PARTB SET PARTB=$PIECE(LINE,ONEMATCH,2,999)
  . . . SET LINE=PARTA_REPLACE(PART)_PARTB
  . FOR PART=1:1:KVMATCHNUM DO   ;"E.g. Acct: 12343456456
  . . NEW ONEMATCH SET ONEMATCH=KVMATCH(PART) QUIT:(ONEMATCH="")!(ONEMATCH=" ")
  . . NEW GOODA,GOODB SET GOODA="",GOODB=LINE
  . . NEW DONE SET DONE=0
  . . FOR  QUIT:(GOODB'[ONEMATCH)!DONE  DO
  . . . NEW PARTA SET PARTA=$PIECE(LINE,ONEMATCH,1)
  . . . NEW PARTB SET PARTB=$PIECE(LINE,ONEMATCH,2,999)
  . . . NEW REPL SET REPL=KVREPLACE(PART)  ;"e.g. '#6' meaning 6 digit long number
  . . . IF (REPL?1"#"1.N)=0 SET DONE=1 QUIT
  . . . SET REPL=$EXTRACT(REPL,2,$LENGTH(REPL))  ;"strip '#', e.g. to '6'
  . . . NEW RNDNUM,JDX SET RNDNUM=0 FOR JDX=1:1:+REPL SET RNDNUM=RNDNUM*10+$RANDOM(9)  ;"make number X digits long
  . . . NEW PRIORNUM SET PRIORNUM=$$NUMSTR^TMGSTUT3(PARTB,.PARTB)
  . . . SET GOODA=PARTA_ONEMATCH_RNDNUM_"r"
  . . . SET GOODB=PARTB
  . . SET LINE=GOODA_GOODB
  . IF LINE'=OLDLINE DO
  . . IF ZEROBASED SET ARR(IDX,0)=LINE 
  . . ELSE  SET ARR(IDX)=LINE
  . . SET CHANGED=1
  IF CHANGED DO
  . KILL @REF
  . MERGE @REF=ARR
  QUIT
  ;                                                                
SETUPKVMATCH(MATCH,REPLACE,ADFN) ;"Set up Key-Value match
  NEW MATCHNUM SET MATCHNUM=0
  SET MATCHNUM=MATCHNUM+1
  SET MATCH(MATCHNUM)="Lab Accession Number: "
  SET REPLACE(MATCHNUM)="#9"
  SET MATCHNUM=MATCHNUM+1
  SET MATCH(MATCHNUM)="Acct #"
  SET REPLACE(MATCHNUM)="#11"  
  SET MATCHNUM=MATCHNUM+1
  SET MATCH(MATCHNUM)="Lab Patient ID: "
  SET REPLACE(MATCHNUM)="#7"
  QUIT
  ;
SETUPMATCH(MATCH,REPLACE,ADFN) ;" Setup up match and replace arrays
  NEW MATCHNUM SET MATCHNUM=0
  NEW NAME SET NAME=$PIECE($GET(^DPT(ADFN,0)),"^",1)
  NEW ARR SET ARR(NAME)=""
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(^DPT(ADFN,.01,IDX)) QUIT:+IDX'>0  DO
  . NEW ALIAS SET ALIAS=$PIECE($GET(^DPT(ADFN,.01,IDX,0)),"^",1)
  . SET ARR(ALIAS)=""  
  NEW ANAME SET ANAME=""
  FOR  SET ANAME=$ORDER(ARR(ANAME)) QUIT:ANAME=""  DO
  . NEW DIF FOR DIV=","," " DO
  . . NEW PART FOR PART=1:1:$LENGTH(ANAME,DIV) DO
  . . . NEW ANAMEPART SET ANAMEPART=$PIECE(ANAME,DIV,PART)
  . . . NEW GARBNP SET GARBNP=$$GARBLENAME(ANAMEPART)
  . . . IF $LENGTH(ANAMEPART)<3 QUIT
  . . . NEW CASE FOR CASE=1:1:3 DO
  . . . . SET MATCHNUM=MATCHNUM+1
  . . . . IF CASE=1 SET MATCH(MATCHNUM)=$$UP^XLFSTR(ANAMEPART)
  . . . . ELSE  IF CASE=2 SET MATCH(MATCHNUM)=$$LOW^XLFSTR(ANAMEPART)
  . . . . ELSE  IF CASE=3 SET MATCH(MATCHNUM)=$$SENTENCE^XLFSTR(ANAMEPART)
  . . . . SET REPLACE(MATCHNUM)=GARBNP
  NEW DOB SET DOB=$PIECE($GET(^DPT(ADFN,0)),"^",3)
  NEW RANDDOB SET RANDDOB=$$RANDOMDOB()
  NEW EDOB SET EDOB=$$FMTE^XLFDT(DOB,"2D")
  SET MATCHNUM=MATCHNUM+1
  SET MATCH(MATCHNUM)=EDOB
  SET REPLACE(MATCHNUM)=$$FMTE^XLFDT(RANDDOB,"2D")
  SET EDOB=$$FMTE^XLFDT(DOB,"5D")
  SET MATCHNUM=MATCHNUM+1
  SET MATCH(MATCHNUM)=EDOB
  SET REPLACE(MATCHNUM)=$$FMTE^XLFDT(RANDDOB,"5D")
  NEW ERANDDOB SET ERANDDOB=$$FMTE^XLFDT(RANDDOB)
  NEW EDOB SET EDOB=$$FMTE^XLFDT(DOB)
  SET EDOB=$TRANSLATE(EDOB,",","")
  SET ERANDDOB=$TRANSLATE(ERANDDOB,",","")
  FOR PART=1:1:$LENGTH(EDOB," ") DO
  . NEW APART SET APART=$PIECE(EDOB," ",PART)
  . NEW ARANDPART SET ARANDPART=$PIECE(ERANDDOB," ",PART)
  . SET MATCHNUM=MATCHNUM+1
  . SET MATCH(MATCHNUM)=APART
  . SET REPLACE(MATCHNUM)=ARANDPART
  SET EDOB=$$FMTE^XLFDT(DOB,"2D")
  NEW AGE SET AGE=$$GET1^DIQ(2,ADFN,.033)
  SET MATCHNUM=MATCHNUM+1
  SET MATCH(MATCHNUM)=AGE
  SET REPLACE(MATCHNUM)=$RANDOM(99)
  NEW PHONENUM SET PHONENUM=$$PHONENUM^TMGTIUOJ(ADFN)
  SET MATCHNUM=MATCHNUM+1
  SET MATCH(MATCHNUM)=PHONENUM
  SET REPLACE(MATCHNUM)="(###) ###-####"        
  NEW AREACODE SET AREACODE=+$PIECE(PHONENUM,"(",2)
  IF AREACODE>0 DO
  . SET MATCHNUM=MATCHNUM+1
  . SET MATCH(MATCHNUM)=AREACODE
  . SET REPLACE(MATCHNUM)="###"
  NEW FIRST3 SET FIRST3=+$PIECE(PHONENUM," ",2)
  IF FIRST3>0 DO
  . SET MATCHNUM=MATCHNUM+1
  . SET MATCH(MATCHNUM)=FIRST3
  . SET REPLACE(MATCHNUM)="###"
  NEW LAST4 SET LAST4=+$PIECE(PHONENUM,"-",2)
  IF LAST4>0 DO
  . SET MATCHNUM=MATCHNUM+1
  . SET MATCH(MATCHNUM)=LAST4
  . SET REPLACE(MATCHNUM)="####"
  NEW SSN SET SSN=$PIECE($GET(^DPT(ADFN,0)),"^",9) SET SSN=$TR(SSN,"-","")
  SET MATCHNUM=MATCHNUM+1
  SET MATCH(MATCHNUM)=SSN
  SET REPLACE(MATCHNUM)="#########"
  SET SSN=$EXTRACT(SSN,1,3)_"-"_$EXTRACT(SSN,4,5)_"-"_$EXTRACT(SSN,6,9)
  SET MATCHNUM=MATCHNUM+1
  SET MATCH(MATCHNUM)=SSN
  SET REPLACE(MATCHNUM)="###-##-####"
  ;"Ensure that replacement value is different from match value.  
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(MATCH(IDX)) QUIT:IDX'>0  DO
  . FOR  QUIT:($GET(REPLACE(IDX))'[$GET(MATCH(IDX)))  DO
  . . SET REPLACE(IDX)=$RANDOM(1000)
  QUIT
  ;
GARBLEHPI(ITEMARRAY)  ;  ;"CALLED FROM GETHPI^TMGTIUP2
  ;"Input: ITEMSARRAY -- see format in PARSEARR^TMGTIUP2
  NEW ITEM SET ITEM=0
  FOR  SET ITEM=$ORDER(ITEMARRAY("TEXT",ITEM)) QUIT:ITEM'>0  DO
  . NEW SUB SET SUB=0
  . FOR  SET SUB=$ORDER(ITEMARRAY("TEXT",ITEM,SUB)) QUIT:SUB'>0  DO
  . . NEW NARR SET NARR=$GET(ITEMARRAY("TEXT",ITEM,SUB)) QUIT:NARR=""
  . . IF "[GROUP],[TABLE]"[NARR QUIT
  . . SET NARR=$$GARBLEPARA(NARR)
  . . SET ITEMARRAY("TEXT",ITEM,SUB)=NARR
  QUIT
  ;
GARBLEPARA(STR)  ;"GARBLE PARAGRAPH  
  SET STR=$$REPLSTR^TMGSTUT3(STR,"<"," <")
  SET STR=$$REPLSTR^TMGSTUT3(STR,">","> ")
  NEW OPTION SET OPTION("TRIM DIV")=1
  NEW ARR DO SPLIT2AR^TMGSTUT2(STR," ",.ARR,1,.OPTION)
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:IDX'>0  DO
  . NEW WORD SET WORD=$GET(ARR(IDX))
  . IF WORD="" KILL ARR(IDX) QUIT
  . NEW ISDATE SET ISDATE=WORD?1.2N1"/"1.2N1"/"2.4N0.1":"                         
  . IF ISDATE QUIT
  . NEW ISTAG SET ISTAG=WORD?1(1"<",1"[")1.10A1(1">",1"]")
  . IF ISTAG QUIT
  . SET WORD=$$GARBLESTR(WORD,"L") 
  . SET ARR(IDX)=WORD
  NEW RESULT SET RESULT=$$ARR2STR^TMGSTUT2(.ARR," ")
  QUIT RESULT
  ;  
GARBLEALERT(S) ;" Garble alert.  Called here from DOIT1^XQALERT1
  ;"example S3221001.200001^OR,76030,22;168;3221001.200001^AAAAAA,AA (A9999): Imaging Results,Non Critical: EXTENDED WEAR HOLTER ^^R^^RPTRAD2^ORB3FUP2^^1^^^0^0^0
  NEW RESULT SET RESULT=S
  NEW P3 SET P3=$PIECE(RESULT,"^",3)
  NEW P2A SET P2A=$PIECE($PIECE(RESULT,"^",2),";",1)
  IF P2A["NO-ID" GOTO GBALTDN
  IF P2A["TMG-HL7" DO  GOTO GBALTSET
  . NEW P3A SET P3A=$PIECE(P3,"[",1)
  . NEW P3B SET P3B=$PIECE(P3,"]",2)
  . SET P3=P3A_P3B
  IF $EXTRACT(P3,1,3)="FYI" GOTO GBALTDN
  IF "TIU,OR,"[$EXTRACT(P2A,1,3) DO  GOTO GBALTSET
  . NEW P3A SET P3A=$PIECE(P3,":",1)
  . NEW P3B SET P3B=$PIECE(P3,":",2)
  . NEW NAME SET NAME=$PIECE(P3A,"(",1)
  . NEW LNAME SET LNAME=$PIECE(NAME,",",1)
  . NEW FNAME SET FNAME=$PIECE(NAME,",",2)
  . SET NAME=$EXTRACT(LNAME,1,2)_","_$EXTRACT(FNAME,1,2)
  . SET P3=NAME_" :"_P3B
  ELSE  DO
  . SET P3=$$GARBLESTR(PT,"UL")      
GBALTSET ;  
  SET $PIECE(RESULT,"^",3)=P3
GBALTDN ;  
  QUIT RESULT
  ;
LOADFAKENOTEREF(REF) ;
  NEW OUT DO LOADFAKENOTE(.OUT)
  MERGE @REF=OUT
  QUIT
  ;
LOADFAKENOTE(OUT) ;
  NEW IDX,DONE SET DONE=0
  FOR IDX=1:1 DO  QUIT:DONE
  . NEW LINE SET LINE=$TEXT(FAKENOTE+IDX)
  . SET LINE=$PIECE(LINE,";;",2,99)
  . IF LINE="<END>" SET DONE=1 QUIT
  . SET OUT(IDX)=LINE
  QUIT
  ;
FAKENOTE  
 ;;<HEAD>
 ;;<META content="text/html; charset=windows-1256" http-equiv=Content-Type>
 ;;<META name=GENERATOR content="MSHTML 6.00.6000.17107"></HEAD>
 ;;<BODY contentEditable=true>
 ;;<p>6/24/20&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(###) ###-#### 
 ;;<p><FONT size=6>
 ;;<p>Progress Note</FONT> 
 ;;<p><b>Identification:</b>Ygny,Xytmee R (DOB: MAY 17,1918) is a 85yr yr. FEMALE. <br><b>Reason for visit:</b> Scheduled visit for periodic followup of chronic problems. 
 ;;<p><STRONG><FONT style="BACKGROUND-COLOR: #ffff00">NOTE: Below is a FAKE note.&nbsp; Any apparent references to a person are made up.&nbsp; Date of birth, age, references to other people, etc, are all fictional&nbsp; </FONT></STRONG>
 ;;<p><b>FOLLOW-UP INFORMATION FROM PRIOR NOTES:</b>&nbsp;&nbsp;<br>&nbsp;&nbsp;0.1 months until appt. due. <br>&nbsp;&nbsp;Note on 10/29/20 --&gt; f/u due approx.: 10/29/20 or after. --&nbsp; &nbsp;"1 Month or sooner if needed;"<br><b>CHIEF COMPLAINT (CC):</b> Medical Recheck 
 ;;<p><STRONG><u>ALLERGIES</u>: </STRONG><br><b>PENICILLIN</b><FONT size=-1> (Entered: MAR 28,2008)</FONT> <br><b>CODEINE</b><FONT size=-1> (Entered: MAR 28,2008)</FONT> <br><b>GOLD COMPOUNDS,ANTIRHEUMATIC</b><FONT size=-1> (Entered: MAR 28,2008)</FONT> <br>
 ;;<p><FONT style="BACKGROUND-COLOR: #fffc00"></FONT>
 ;;<p>Note: Italicized text below is copied from prior notes for continuity<br><b>HISTORY OF PRESENT ILLNESS (HPI):</b><br>
 ;;<ul>
 ;;<li><u>Social</u>: [GROUP A,B] <i>things are about the same. Went up to NH recently to see fall colors. It was cold at night, but very pretty! .. 9/29/20: Went on a picnic in Deep Creek a couple weeks ago.&nbsp;&nbsp;It is on the way to ... near the dam</I>... 10/28/20: They are enjoying this lovely weather.<br>
 ;;<li><u>Bradycardia</u>: <i>Her initial pulse was reading 42.&nbsp;... pOx reading of 98 and pulse reading of 52.&nbsp;&nbsp;She seems to be doing OK on this.&nbsp;&nbsp;She is on diltiazem 360 mg for A-fib. &nbsp;&nbsp;I want them to monitor this and let me know if found &lt; 50</I>... 10/28/20: Some days it is low and other days it is normal. It usually runs in the low 60s<br>
 ;;<li><u>GI issues</u>: [GROUP B] <EM>She is not feeling well.&nbsp;... 8/18/2018&nbsp;&nbsp;She is doing much better.&nbsp;&nbsp; The confusion seemed to clear after she finished the UTI Rx.&nbsp;&nbsp; ... 9/29/20: Went to the hospital last week for&nbsp;&nbsp;abdominal pain and was discharged with Immodium.&nbsp;&nbsp;... was Dx'd with diverticulosis.&nbsp;&nbsp;They had follow up with GI...<br></EM>
 ;;<li><EM><u>Confusion</u>: [GROUP A,B] 04/20/2018 She has been confused several days this week.&nbsp; ... Doesn't know what day it is or where she is.&nbsp;&nbsp;She has been urinating frequently. No fever.&nbsp;&nbsp; ...&nbsp;She hasn't been out of the house in several weeks.&nbsp;&nbsp;Discussed options and son would like to treat empirically for bladder infection. She wants to avoid getting patient out of the house in light of coronavirus outbreak. If patient does not improve by Monday, she will call to schedule a time for bloodwork ... . Orders as below.&nbsp;... Call sooner if symptoms worsen, fever, vomiting, or other problems... <br></EM><br>-- [MEMORY/THINKING] ---------<br>B-12 = 760 on 4/16/2018 &lt;-- 505 on 8/26/2017 &lt;-- 400 on 01/18/2015<br>Meds-1 = donepezil (Aricept) 5 mg tablet;1 tablet at bedtime.<br>Mini Cog Results = JAN 14, 2000 (ABN)<br>TSH = 2.50 on 6/30/2018 &lt;-- 2.78 on 4/16/2018 &lt;-- 4.82H on 4/18/2019<br>
 ;;<li><EM><u>DM-2</u>: [GROUP B] 01/4/18:&nbsp;&nbsp; We had to start her on insulin due to high sugars.&nbsp;&nbsp;She has tolerated 10 U and BS's are still running high. Will increase to 15 U.&nbsp;&nbsp;.. 6/30/20:&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;... checking her BS periodically and haven't seen any high sugars... 8/4/20:&nbsp;The hospital put her back on insulin.&nbsp;&nbsp;She has been seeing BS's &gt; 200.&nbsp;&nbsp; Will continue at 5 units... 9/29/20: BS is good control in April ... 10/28/20: HA1c was good at last visit. Continue with current therapy.&nbsp; Will draw labs today.&nbsp;</EM> <br><br>-- [DIABETIC STUDIES] ---------<br>Eye Exam = 01/2019 "OK"; 10/2018 "Got new glasses. "; 10/2017 ""(-)""<br>Foot Exam = 2/14/2018; 01/4/2018; 10/24/2017 "Good circulation, but she can't sense a dia...<br>HgbA1c = 5.9 on 4/16/2018 &lt;-- 6.1H on 6/26/2019 &lt;-- 7.9H on 01/6/2018 &lt; -- 6.9H on 4/24/2018 <br>Creatinine = 1.50H on 9/4/2018 &lt;-- 1.40H on 8/01/2018 &lt;-- 1.40H on 8/26/2018<br>EGFR = 29 on 9/4/2018 &lt;-- 32 on 8/01/2018 &lt;-- 32 on 8/26/2018 <br>Urine Microalbumin = 118.0 on 6/30/2018 &lt;-- 39.0 on 8/2/2019 &lt;-- 16.9H on 8/24/2018 &lt;-- 11.2H on 8/16/2016 <br>Albumin / Cr Ratio = 2443H on 6/30/2018 &lt;-- 995H on 8/2/2019<br>CVD Risk = 17% in next 10 years<br>Complications : chronic kidney disease <br>Medication-1 = Lantus 100 UNIT/ML Solution Sig: inject 5 units sq daily <br>EKG = 8/01/15 multiple ventricular &amp; atrial premture complexes at Dr. J's office <br>
 ;;<li><u>Fall</u>: [GROUP B] <i>She fell and broke her left wrist.&nbsp;&nbsp;She broke "2 bones".&nbsp;&nbsp; ...&nbsp;8/2/20:&nbsp;&nbsp;No falls.&nbsp;The O2 tubing is still a concern... 6/30/20:&nbsp;&nbsp;... She slipped and bruised her hip.&nbsp;&nbsp; But no other falls... 8/18/20:&nbsp;&nbsp;&nbsp;&nbsp; She did have a slide to the ground, but no head injury. Will monitor</I>... <br>
 ;;<li><u>Hypothyroidism</u>: [GROUP B] <i>Data as below.&nbsp;10/24/17: At goal.&nbsp;Will continue same.... 4/24/18:&nbsp;&nbsp; Stable on Rx, will continue same.. 01/4/18:&nbsp;&nbsp;Stable... 8/2/20:&nbsp;&nbsp; Last data as below.&nbsp;I am going to increase her to 75 mcg... 04/16/2018 Levels are therapeutic with current dose of 75 mcg. Will continue... 6/30/20:&nbsp;&nbsp; Stable on Rx, will continue</I>... 10/28/20: No new data. Her level was good last time she was here. Continue with current therapy.&nbsp; <br><br>-- [THYROID] ---------<br>Regimen : <br>TSH = 2.50 on 6/30/2018 &lt;-- 2.78 on 4/16/2018 &lt;-- 4.82H on 4/18/2019<br>Free T4 = 0.95 on 4/16/2018<br>Medication-1 = levothyroxine sodium 75 mcg po Daily<br>
 ;;<li><u>Dyspepsia</u>: [GROUP B] <i>H/O Bleeding gastric ulcer... seen on EGD ... Doing well on Protonix....04/16/2018 no complaints on medication... 6/30/20: Still on Rx, doing Ok</I>... 10/28/20: Still on Rx. doing ok. Continue with current therapy.<br><br>-- [GI] ---------<br>Amylase = 77 on 4/2/2015<br>Bilirubin,Total = 0.4 on 9/4/2018 &lt;-- 0.3 on 8/01/2018 &lt;-- 0.5 on 8/26/2018<br>Colonoscopy = 8/8/2009; 2005<br>EGD = 2005 --&gt; (-), 6/18/09 -&gt; retention, 8/18/09; 6/01/10 --&gt; (-)<br>Hep C = Nonreactive on 6/30/2018<br>LIPASE = 68 on 4/2/2015<br>MEDS-1 = Ondansetron 4 mg tablet disintegrating;1 tab q 4 hr prn nausea/vomiting <br>MEDS-2 = STOPPED OTC alka seltzer plus po as needed<br>MEDS-3 = pantoprazole (Protonix) 40 mg po twice Daily<br>Physician : <br>
 ;;<li><u>Lipids</u>: [GROUP B] <i>data as below.&nbsp;&nbsp; Will follow along.&nbsp;&nbsp;They do a lot of fresh veggies and fruits.&nbsp;&nbsp;&nbsp;&nbsp;Will continue with diet control.... 04/16/2018 LDL 64 without medications currently... 6/30/20:&nbsp;&nbsp; With her age, I will leave her off statin.&nbsp;&nbsp;LDL is 64</I>... 10/28/20: No new data. Continue off therapy.<br><br>-- [LIPIDS] ---------<br>Total Cholesterol = 189 on 6/30/2018 &lt;-- 184 on 4/16/2018 &lt;-- 153 on 4/18/2019<br>HDL Cholesterol = 70 on 6/30/2018 &lt;-- 53L on 4/16/2018 &lt;-- 53 on 4/18/2019 <br>Non-HDL Cholesterol = 119 on 6/30/2018 &lt;-- 100 on 4/18/2019 &lt;-- 86 on 01/6/2018<br>LDL Cholesterol = 74 on 6/30/2018 &lt;-- 64 on 4/18/2019 &lt;-- 29 on 01/6/2018 <br>Triglycerides = 226H on 6/30/2018 &lt;-- 339H on 4/16/2018 &lt;-- 180H on 4/18/2019<br>Liver Enzymes (ALT) = 12 on 9/4/2018 &lt;-- 16 on 8/01/2018 &lt;-- 12 on 8/26/2018<br>ASCVD Positive = 2/14/2018<br>CVD Risk = 17% in next 10 years<br>Statin Intensity=N/A<br>Target statin tx group = moderate<br>
 ;;<li><u>Anxiety/Depression</u>: [GROUP B] <i>Stable on Rx, will continue.&nbsp;&nbsp;&nbsp;She feels she is doing well with this.&nbsp;....04/16/2018 fairly well controlled with medication currently... 6/30/20:&nbsp;&nbsp;&nbsp;&nbsp;She has been sleeping a lot of the time.&nbsp;&nbsp; Will have them HOLD the mirtazapine unless she gets back not sleeping</I>... 10/28/20: Hard to tell at times with memory impairment. &nbsp; Will follow.&nbsp; <br><br>-- [MOOD] ---------<br>Psychiatrist : None<br>Psychologist/Counselor : None<br>Medication-1 = buspirone (Buspar) 15 mg po twice Daily<br>Medication-2 = donepezil (Aricept) 5 mg tablet;1 tablet at bedtime.<br>History of Manic Symptoms : None<br>Suicidal Ideation : None<br>Dx : Mild depression.<br>
 ;;<li><u>Insomnia</u>: [GROUP A] <i>2/14/20: She sleeps fairly well, but still wakes up very early and gets confused upon doing this... Son's sister had to come and help and given him a break... 9/16/20:&nbsp;&nbsp; Sleeping better as above... 9/29/20: &nbsp;&nbsp; She gets up at times and wonders around at night</I>... 10/28/20: Sleeping is hit or miss. Some nights she sleeps all night and others not much. <br><br>-- [INSOMNIA] ---------<br>Medication-2 = buspirone (Buspar) 15 mg po twice Daily<br>
 ;;<li><u>HTN</u>: [GROUP A] <i>...2/14/20: B/P high on this visit, 166/51.&nbsp;But at her age, I am not going ot increase her dose... 04/16/2018 well controlled with current meds. Will continue... 9/16/20:&nbsp; &nbsp; They were at cardiology appt and ... wasn't happy with her BP being high.&nbsp;&nbsp;Sosent in script for diltiazem 300 mg.&nbsp;&nbsp;But she was already on 360 mg.&nbsp;&nbsp;It is now 144/102.&nbsp;She has been complaining of dizziness.&nbsp;&nbsp;I hate to crank up her BP higher at age&nbsp;xx yrs</I>... 10/28/20: BP is up today. Per her son it is up and down. It is checked twice a day by family.&nbsp; At&nbsp;her age, will not increase her dose.&nbsp; <br><br>-- [HYPERTENSION] ---------<br>BP = 164/71 (10/28/20) &lt;- 156/76 &lt;- 144/102 &lt;- 148/65<br>Sodium = 135L on 9/4/2018 &lt;-- 138 on 8/01/2018<br>Date of last electrolytes : 9/4/2018 &lt;-- 8/01/2018<br>Potassium = 4.6 on 9/4/2018 &lt;-- 4.1 on 8/01/2018 &lt;-- 3.2L on 8/26/2018<br>Creatinine = 1.50H on 9/4/2018 &lt;-- 1.40H on 8/01/2018 &lt;-- 1.40H on 8/26/2018<br>Urine Protein = &gt;=300 on 9/4/2018 &lt;-- 100 on 8/26/2018<br>Guideline BP Goal: &lt;140/90 (JNC8) (DM, CKD, Age: 95yr)<br>BP Control Status: ABOVE GOAL (SYS PRESSURE HIGH)<br>CVD Risk = 17% in next 10 years<br>Complications : cardiovascular disease (A-fib) . CKD-3B<br>EKG = 08/28/2018 (NOTE)<br>Medication-1 = Clonidine hcl 0.1 mg tablet;1 po q 8 hrs prn severe htn 180/100 <br>Medication-2 = Diltiazem (Tiazic) 360 mg po po Daily<br>Medication-3 = STOPPED furosemide (Lasix) 20 mg po Daily<br>Medication-4 = STOPPED metolazone (Zaroxolyn) 5 mg po twice Daily as needed<br>Medication-5 = isosorbide (Monoket) ER 30 mg po Daily<br>Medication-6 = potassium (Klor-Con) 10 mEq po 2 QAM and 3 at bedtime (50 mEq daily)<br>Medication-7 = spironolactone (Aldactone) 25 mg po Daily<br>
 ;;<li><u>Hypokalemia</u>: [GROUP A] <i>...is stable.&nbsp;She is on 50 mEq daily and spironolactone. Will need to keep close watch on this. ...04/18/2019 She is taking six potassium tablets daily, but most recent potassium was only 3.3.04/16/2018 normal potassium 4.6 on 4/16/2018</I>... 10/28/20: Continue with current therapy.<br><br>-- [POTASSIUM] ---------<br>Potassium = 4.6 on 9/4/2018 &lt;-- 4.1 on 8/01/2018 &lt;-- 3.2L on 8/26/2018<br>Creatinine = 1.50H on 9/4/2018 &lt;-- 1.40H on 8/01/2018 &lt;-- 1.40H on 8/26/2018<br>MEDS-1 = STOPPED furosemide (Lasix) 20 mg po Daily<br>MEDS-2 = STOPPED metolazone (Zaroxolyn) 5 mg po twice Daily as needed<br>MEDS-3 = potassium (Klor-Con) 10 mEq po 2 QAM and 3 at bedtime (50 mEq daily)<br>MEDS-4 = spironolactone (Aldactone) 25 mg po Daily<br>Magnesium = 2.0 on 8/01/2016<br>
 ;;<li><u>Anticoagulation</u>: [GROUP A] Taking for A-fib.&nbsp; <i>...she meets 2 of the following 3: age &gt; 80, wt &lt; 60 kg, Cr &gt; 1.5.&nbsp;&nbsp; ... Will continue with&nbsp;&nbsp;&nbsp;&nbsp;Eliquis 2.5 mg po BID Will continue same....04/18/2019 Taking Eliquis 2.5 mg po once daily currently... 2/14/20: Managed with Dr. Jim , will see him again June... 8/28/20:&nbsp;&nbsp; She has had A-fib in the past.&nbsp;&nbsp;I am trying to minimize her Rx's to see if she is having polypharmacy issues.&nbsp;&nbsp;&nbsp;&nbsp;Will have them hold this for now.&nbsp;&nbsp;Will continue her diltiazem, but monitor for low BP with her poor PO intake</I>... 10/28/20: Continue on anticoagulation.&nbsp; Will drop this paragraph and consider with A-fib below.&nbsp; <br>
 ;;<li><u>CKD</u>: [GROUP A] <i>04/18/2019 Creatinine 1.9 on 2/18/20 associated with CKD stage 4. She is drinking more fluid recently, but tolerating them.&nbsp;Will recheck CMP today... 10/8/20:&nbsp;&nbsp; We discussed option for her to see a nephrologist.&nbsp;&nbsp;&nbsp;&nbsp;They would prefer to not pursue this until it becomes a problems...04/16/2018 Her creatinine is improved slightly compared to previous</I>... 10/28/20: GFR and SCr is similar to previous. Continue to follow.<br><br>-- [CKD/RENAL] ---------<br>CKD STAGE = 4. (LAST EGFR=29 ON SEP 03, 2000@17:11:01)<br>CREATININE = 1.50H on 9/4/2018 &lt;-- 1.40H on 8/01/2018 &lt;-- 1.40H on 8/26/2018<br>EGFR = 29 on 9/4/2018 &lt;-- 32 on 8/01/2018 &lt;-- 32 on 8/26/2018<br>MAGNESIUM = 2.0 on 8/01/2016<br>POTASSIUM = 4.6 on 9/4/2018 &lt;-- 4.1 on 8/01/2018 &lt;-- 3.2L on 8/26/2018<br>Albumin / Cr Ratio = 2443H on 6/30/2018 &lt;-- 995H on 8/2/2019<br>Medication-1 = STOPPED furosemide (Lasix) 20 mg po Daily<br>Medication-2 = spironolactone (Aldactone) 25 mg po Daily<br>NEPHROLOGIST : <br>Urine Microalbumin = 118.0 on 6/30/2018 &lt;-- 39.0 on 8/2/2019 &lt;-- 16.9H on 8/24/2018 &lt;-- 11.2H on 8/16/2016<br>Urine Protein = &gt;=300 on 9/4/2018 &lt;-- 100 on 8/26/2018<br>
 ;;<li><u>A-fib</u>: [GROUP A] <i>04/18/2019 She continues to follow with Dr. Jim . Taking Eliquis once daily.&nbsp;&nbsp;No racing heart... 10/8/20:&nbsp;&nbsp;&nbsp;No problems with this... 2/14/20: No problems, she still keeps on with Dr. Jim. He will see him again in June.&nbsp;&nbsp;Will continue same... 04/16/2018 Taking medications as prescribed</I>... 10/28/20: Continue same therapy.&nbsp; No heart racing.&nbsp; <br><br>-- [Atrial Fibrillation] ---------<br>Cardiologist : Dr. Jim<br>Echocardiogram : <br>Medication-1 = Clonidine hcl 0.1 mg tablet;1 po q 8 hrs prn severe htn 180/100 <br>Medication-2 = Diltiazem (Tiazic) 360 mg po po Daily<br>Medication-3 = NTG 0.4 mg SL Q5 min for up to 3 doses prn chestpain<br>Medication-4 = STOPPED furosemide (Lasix) 20 mg po Daily<br>Medication-5 = STOPPED metolazone (Zaroxolyn) 5 mg po twice Daily as needed<br>Medication-6 = apixaban (Eliquis) 2.5 mg po twice Daily<br>Medication-7 = isosorbide (Monoket) ER 30 mg po Daily<br>Medication-8 = spironolactone (Aldactone) 25 mg po Daily<br>
 ;;<li><u>Cardiac/Chronic Angina</u>: [GROUP A] <i>She continues on Isosorbide MN ER. ... no change in her frequency or intensity of chest pain. ...&nbsp; Sees Dr. Jim once a year.&nbsp;&nbsp;Not having much chest pain.&nbsp;&nbsp;They carry nitro pills but she rarely has to use them.... 2/18/18:&nbsp;&nbsp; No change. Will continue same....8/24/18:&nbsp;&nbsp; Sees Dr. Jim&nbsp;&nbsp;Everything currently stable... 10/8/20:&nbsp;&nbsp; Stable, no chest pain... 2/14/20: Stable, follows with Dr. Jim</I>... 10/28/20: Stable. Follows with Dr. Jim. They last saw him in September. <br>
 ;;<li><u>Hypoxia</u>: [GROUP B] <i>Uses O2 full time now.&nbsp;&nbsp; Still using oxygen full time.... 10/24/17:&nbsp;&nbsp; Stable....&nbsp;&nbsp;Using O2 full time and benefiting...01/4/18:&nbsp;She has been on 2 L/min.&nbsp;&nbsp;Then her Son increased her to 3 when she was having some issues.&nbsp;&nbsp;I want them to go back to 2 when she gets stable... 8/2/20: She continues on O2, will continue... 04/16/2018 Using oxygen full time and benefitting from it... 6/30/20:&nbsp;&nbsp; Still using O2 at 2L</I>... 10/28/20:&nbsp; Continues to be 2L O2. Still getting benefit.&nbsp; Will continue&nbsp; Will drop and consider with dyspnea below. <br>
 ;;<li><u>Dyspnea</u>: [GROUP A] <i>DUE TO CONGESTIVE HEART FAILURE (CHF):&nbsp;... A-fib --&gt; net decrease in cardiac output;&nbsp;&nbsp;&nbsp;&nbsp; Continued diureses and negative fluid balance.&nbsp; Son has been tracking her weight with some new scales and they have been checking it every day.&nbsp;&nbsp; They know to let me know if her weight goes up by 5 lbs.&nbsp; &nbsp;&nbsp;&nbsp;She is on diuretics: lasix, spironolactone, and PRN metolazone.&nbsp;...2/18/18:&nbsp;She is taking less lasix now, as I requested because her creatinine was up.&nbsp;&nbsp; She has not had an increase in weight and her Son is monitoring this.&nbsp;&nbsp; If she starts gaining weight, I want them to let me know.... 8/24/18:&nbsp;&nbsp;has trouble breathing when it is hot... 04/18/2019 Weight is up slightly from dry weight, but very little pedal edema.&nbsp;She is wearing her oxygen. Will continue current meds and check potassium... 10/8/20: Doing OK. She has her O2 on... 2/14/20: Doing alright, she has her O2. She recently got a new tank... 04/16/2018 Her BNP is stable compared to previous years. I think her current wheezing/cough is probably due to infection</I>... 10/28/20: Doing ok on medication and oxygen.<br><br>-- [CHF] ---------<br>BNP = 296H on 4/16/2018 &lt;-- 299H on 8/10/2016 &lt;-- 313H on 8/8/2016<br>Cardiologist : Dr. Jim<br>Creatinine = 1.50H on 9/4/2018 &lt;-- 1.40H on 8/01/2018 &lt;-- 1.40H on 8/26/2018<br>Dry Weight : 158<br>Educated about sodium intake : 8/24/18 -- they use Mrs. Dash.<br>Last echocardiogram : <br>Medication-1 = Diltiazem (Tiazic) 360 mg po po Daily<br>Medication-2 = NTG 0.4 mg SL Q5 min for up to 3 doses prn chestpain<br>Medication-3 = STOPPED furosemide (Lasix) 20 mg po Daily<br>Medication-4 = STOPPED metolazone (Zaroxolyn) 5 mg po twice Daily as needed<br>Medication-5 = potassium (Klor-Con) 10 mEq po 2 QAM and 3 at bedtime (50 mEq daily)<br>Medication-6 = spironolactone (Aldactone) 25 mg po Daily<br>Pneumovax = 4/14/2008; 1991<br>Potassium = 4.6 on 9/4/2018 &lt;-- 4.1 on 8/01/2018 &lt;-- 3.2L on 8/26/2018<br>Weight = 161.4 lb (10/28/2018), 162 lb (10/29/2018), 162.4 lb (08/18/2018)<br>
 ;;<li><u>Pedal edema</u>: [GROUP A] <i>2/18/18:&nbsp;&nbsp;Related to CHF.&nbsp;&nbsp;8/24/18: Currently wearing support stockings which she says helps with the swelling... 04/18/2019 Very little swelling today... 10/8/20:&nbsp;&nbsp;STABLE... 2/14/20: Same as before.&nbsp;&nbsp; She has good compression hose on</I>... 10/28/20: Continues to be present but not an issue. They keep her feet up the best they can and she still has compression hose.<br><br>-- [EDEMA] ---------<br>ALBUMIN = 4.2 on 10/28/2018 &lt;-- 3.7 on 9/4/2018 &lt;-- 3.3L on 8/01/2018<br>BNP = 296H on 4/16/2018 &lt;-- 299H on 8/10/2016 &lt;-- 313H on 8/8/2016<br>CKD STAGE = 4. (LAST EGFR=27 ON OCT 27, 2000@13:22:06)<br>CREATININE = 1.58H on 10/28/2018 &lt;-- 1.50(H) on 9/4/2018 &lt;-- 1.40(H) on 8/01/2018<br>MEDS-1 = potassium (Klor-Con) 10 mEq po 2 QAM and 3 at bedtime (50 mEq daily)<br>MEDS-2 = spironolactone (Aldactone) 25 mg po Daily<br>POTASSIUM = 4.9 on 10/28/2018 &lt;-- 4.6 on 9/4/2018 &lt;-- 4.1 on 8/01/2018<br>SODIUM = 136 on 10/28/2018 &lt;-- 135L on 9/4/2018 &lt;-- 138 on 8/01/2018<br>
 ;;<li><u>Prevention</u>: [GROUP A,B]&nbsp;<br><br>&nbsp;&nbsp;Tobacco Use History Update:<br>&nbsp;&nbsp;&nbsp;&nbsp; -----------------------------------<br>&nbsp;&nbsp;&nbsp;&nbsp; Current Tobacco Use Status<br>&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;Tobacco usage is unchanged<br>&nbsp;&nbsp;&nbsp;&nbsp; Type of tobacco (if any):<br>&nbsp;&nbsp;Complete Annual Physical:<br>&nbsp;&nbsp;&nbsp;&nbsp; Complete physical not applicable.<br>&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;Comment: Will stop due to age.<br>&nbsp;&nbsp;HEMOGLOBIN A1C:<br>&nbsp;&nbsp;&nbsp;&nbsp; HEMOGLOBIN A1C ORDERED TODAY.<br>&nbsp;&nbsp;CONSIDER EGD FOR CHRONIC GERD:<br>&nbsp;&nbsp;&nbsp;&nbsp; EGD Considered<br></li></ul>
 ;;<p><br><b>PAST MEDICAL HISTORY (PMH)</b>: Patient's intake questionnaire documenting changes to past medical history was reviewed and discussed as needed. 
 ;;<p><b>REVIEW OF SYSTEMS:</b> Patient's ROS from questionaire was reviewed.Significant positive responses have been incorporated into HPI above.&nbsp;&nbsp;A scanned copy should be included in EMR 
 ;;<p>-- [MEDICATIONS] ---------<br>&nbsp;&nbsp;&nbsp;&nbsp; *Allergies sync'd with ERx on date = 10/28/20<br>&nbsp;&nbsp;&nbsp;&nbsp; *CSM-Database Review = 10/29/20 <br>&nbsp;&nbsp;&nbsp;&nbsp; *CSM Contract = 06/30/20 <br>&nbsp;&nbsp;&nbsp;&nbsp; *Reconciliation date = 10/28/20<br>&nbsp;&nbsp;&nbsp;&nbsp; *Source of information = verbal/brought<br>&nbsp;&nbsp;&nbsp;&nbsp; apixaban (Eliquis) 2.5 mg po twice Daily<br>&nbsp;&nbsp;&nbsp;&nbsp; buspirone (Buspar) 15 mg po twice Daily<br>&nbsp;&nbsp;&nbsp;&nbsp; <b><FONT style="BACKGROUND-COLOR: #ffff99">Clonidine hcl 0.1 mg tablet;1 po q 8 hrs prn severe htn 180/100</b></FONT><br>&nbsp;&nbsp;&nbsp;&nbsp; Diltiazem (Tiazic) 360 mg po po Daily<br>&nbsp;&nbsp;&nbsp;&nbsp; donepezil (Aricept) 5 mg tablet;1 tablet at bedtime.<br>&nbsp;&nbsp;&nbsp;&nbsp; STOPPED furosemide (Lasix) 20 mg po Daily<br>&nbsp;&nbsp;&nbsp;&nbsp; isosorbide (Monoket) ER 30 mg po Daily<br>&nbsp;&nbsp;&nbsp;&nbsp; Lantus 100 UNIT/ML Solution Sig: inject 5 units sq daily<br>&nbsp;&nbsp;&nbsp;&nbsp; levothyroxine sodium 75 mcg po Daily<br>&nbsp;&nbsp;&nbsp;&nbsp; STOPPED metolazone (Zaroxolyn) 5 mg po twice Daily as needed<br>&nbsp;&nbsp;&nbsp;&nbsp; NTG 0.4 mg SL Q5 min for up to 3 doses prn chestpain<br>&nbsp;&nbsp;&nbsp;&nbsp; STOPPED Nystatin 100000 unit/ml suspension;swish and swallow 5 cc ac and hs for 7 days<br>&nbsp;&nbsp;&nbsp;&nbsp; O2 2 L/min night-time<br>&nbsp;&nbsp;&nbsp;&nbsp; OFF Ipratropium-albuterol 0.5-2.5 (3) Mg/3ml solution;use 1 unit dose in nebulizer 4 times daily.<br>&nbsp;&nbsp;&nbsp;&nbsp; Ondansetron 4 mg tablet disintegrating;1 tab q 4 hr prn nausea/vomiting<br>&nbsp;&nbsp;&nbsp;&nbsp; STOPPED OTC alka seltzer plus po as needed<br>&nbsp;&nbsp;&nbsp;&nbsp; OTC MTV po Daily<br>&nbsp;&nbsp;&nbsp;&nbsp; OTC Tylenol extra strength po as needed<br>&nbsp;&nbsp;&nbsp;&nbsp; OTC Vitamin-D, 2,000 IU po Daily<br>&nbsp;&nbsp;&nbsp;&nbsp; OTC Zyrtec po Daily<br>&nbsp;&nbsp;&nbsp;&nbsp; pantoprazole (Protonix) 40 mg po twice Daily<br>&nbsp;&nbsp;&nbsp;&nbsp; potassium (Klor-Con) 10 mEq po 2 QAM and 3 at bedtime (50 mEq daily)<br>&nbsp;&nbsp;&nbsp;&nbsp; spironolactone (Aldactone) 25 mg po Daily</P>
 ;;<p>-- [PROBLEM LIST] ---------<br>&nbsp;&nbsp;&nbsp;&nbsp; ***NOTE: See HPI above for further problems<br>&nbsp;&nbsp;&nbsp;&nbsp; ASCVD: Severe 2 vessal dz on Cath.&nbsp;&nbsp;Med Management planned<br>&nbsp;&nbsp;&nbsp;&nbsp; Anxiety/Depression<br>&nbsp;&nbsp;&nbsp;&nbsp; Dyspepsia/GERD<br>&nbsp;&nbsp;&nbsp;&nbsp; HTN<br>&nbsp;&nbsp;&nbsp;&nbsp; Osteoarthritis<br>&nbsp;&nbsp;&nbsp;&nbsp; Osteoporosis </P>
 ;;<p>-- [SURGERIES] ---------<br>&nbsp;&nbsp;&nbsp;&nbsp; Back surgery, L4-L5 fusion<br>&nbsp;&nbsp;&nbsp;&nbsp; (B) hip replacement<br>&nbsp;&nbsp;&nbsp;&nbsp; Cholecystectomy<br>&nbsp;&nbsp;&nbsp;&nbsp; TAH with f/u BSO&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;C-section x 2
 ;;<p>-- [SOCIAL HX] ---------<br>&nbsp;&nbsp;&nbsp;&nbsp; EtOH : None<br>&nbsp;&nbsp;&nbsp;&nbsp; Tobacco : NO. NEVER<br>&nbsp;&nbsp;&nbsp;&nbsp; Son:&nbsp;JZ<br>&nbsp;&nbsp;&nbsp;&nbsp; Living arragements: Lives with Son&nbsp;&nbsp; 
 ;;<p>-- [FAMILY HX] ---------<br>&nbsp;&nbsp;&nbsp;&nbsp; Dad: died with COPD&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp; DM: son<br>&nbsp;&nbsp;&nbsp;&nbsp; Aunt: MI at 88 yrs&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; &nbsp; 
 ;;<p>-- [PROVIDER TEAM] ---------<br>&nbsp;&nbsp;&nbsp;&nbsp; Cardiologist: Dr.Jim&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp; GI:&nbsp;Mento<br>&nbsp;&nbsp;&nbsp;&nbsp; Hematology: Dr.&nbsp;Q<br>&nbsp;&nbsp;&nbsp;&nbsp; Ophthomology: Dr.&nbsp;UU<br>&nbsp;&nbsp;&nbsp;&nbsp; Rheumatologist: Dr&nbsp;LL&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; 
 ;;<p>-- [STUDIES] ---------<br>&nbsp;&nbsp;&nbsp;&nbsp; Advance Directives : Living will on chart.&nbsp;&nbsp; DNR<br>&nbsp;&nbsp;&nbsp;&nbsp; Colonoscopy : 2005 --&gt; (-), 8/8/09 -&gt; severe sigmoid diverticulosis<br>&nbsp;&nbsp;&nbsp;&nbsp; Creatinine = 1.50H on 9/4/2018 &lt;-- 1.40H on 8/01/2018 &lt;-- 1.40H on 8/26/2018<br>&nbsp;&nbsp;&nbsp;&nbsp; Hemoglobin = 11.2L on 9/4/2018 &lt;-- 11.0L on 8/01/2018<br>&nbsp;&nbsp;&nbsp;&nbsp; Magnesium = 2.0 on 8/01/2016<br>&nbsp;&nbsp;&nbsp;&nbsp; Potassium = 4.6 on 9/4/2018 &lt;-- 4.1 on 8/01/2018<br>&nbsp;&nbsp;&nbsp;&nbsp; TSH = 2.50 on 6/30/2018 &lt;-- 2.78 on 4/16/2018 &lt;-- 4.82H on 4/18/2019<br>&nbsp;&nbsp;&nbsp;&nbsp; Vit-B12 = 760 on 4/16/2018 &lt;-- 505 on 8/26/2017 &lt; -- 400 on 01/18/2015<br>&nbsp;&nbsp;&nbsp;&nbsp; Vit-D = 37.6 on 8/26/2017 &lt;-- 37.1 on 01/18/2015 &lt; -- 40.3 on 4/28/2015<br>&nbsp;&nbsp;&nbsp;&nbsp; BNP = 296H on 4/16/2018 &lt;-- 299H on 8/10/2016 &lt;-- 313H on 8/8/2016<br>&nbsp;&nbsp;&nbsp;&nbsp; BUN = 21H on 9/4/2018 &lt;-- 24H on 8/01/2018<br>&nbsp;&nbsp;&nbsp;&nbsp; Hep C = Nonreactive on 6/30/2018<br>&nbsp;&nbsp;&nbsp;&nbsp; HgbA1C = 5.9 on 4/16/2018 &lt;-- 6.1H on 6/26/2019 &lt; -- 7.9H on 01/6/2018 &lt;-- 6.9H on 4/24/2018<br>&nbsp;&nbsp;&nbsp;&nbsp; LDL = 74 on 6/30/2018 &lt;-- 64 on 4/18/2019 &lt;-- 29 on 01/6/2018<br>&nbsp;&nbsp;&nbsp;&nbsp; EGD: 2005 --&gt; (-), 6/18/09 -&gt; retention, 8/18/09; 6/01/10 --&gt; (-)<br>&nbsp;&nbsp;&nbsp;&nbsp; EKG: 8/01/15 multiple ventricular &amp; atrial premture complexes at Dr. Jim's office<br>&nbsp;&nbsp;&nbsp;&nbsp; Pap : (stopped due to TAH)&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; 
 ;;<p>-- [IMMUNIZATIONS] ---------<br>&nbsp;&nbsp;&nbsp;&nbsp; INFLUENZA = 9/29/2018; 10/8/2019; 10/2/2018<br>&nbsp;&nbsp;&nbsp;&nbsp; Prevnar = 6/26/2015<br>&nbsp;&nbsp;&nbsp;&nbsp; Pneumococcal-17 = 4/14/2008; 1991<br>&nbsp;&nbsp;&nbsp;&nbsp; Td&nbsp;&nbsp;= 01/01/2006<br>&nbsp;&nbsp;&nbsp;&nbsp; Tdap = 8/20/2013<br>&nbsp;&nbsp;&nbsp;&nbsp; Zostavax = 2/26/2014; refused 8/20/2013 
 ;;<p>-- [HEALTH FACTORS] ---------<br>&nbsp;&nbsp;&nbsp;&nbsp; Advance Directives = 01/4/2018 "ON chart, DNR"<br>&nbsp;&nbsp;&nbsp;&nbsp; Colonoscopy = 8/8/2009; 2005<br>&nbsp;&nbsp;&nbsp;&nbsp; EKG = 08/28/2018 (NOTE)<br>&nbsp;&nbsp;&nbsp;&nbsp; EKG HF = <br>&nbsp;&nbsp;&nbsp;&nbsp; Glaucoma Screening = 01/2019 "OK"<br>&nbsp;&nbsp;&nbsp;&nbsp; Mammogram = 4/14/2016 /IMAGING FU 1 YR; 4/26/2015 /IMAGING FU 1 YR<br>&nbsp;&nbsp;&nbsp;&nbsp; Mammogram - Not Applicable = 10/24/2017 "If she had breast cancer, she says she would not want to do anything about it.&nbsp;&nbsp;" 
 ;;<p><b>PHYSICAL EXAM (PE):</b>: 
 ;;<p>
 ;;<ul>
 ;;<li><u>Vitals</u>: T 51.4 F [36.3 C]; BP <b><FONT style="BACKGROUND-COLOR: #ffff99">164/71</b></FONT>; R 18; P 61; POx 99;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Wt 161.4 lb [73.4 kg]; (06/30/2018) Ht 61.5 in;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; BMI 30.1 (30-35 = "OBESITY CLASS I"); (Ideal Wt=99-134 lbs;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; 27.4 lbs over weight); ; Wt down .6 lbs. (161.4 &lt;== 162 prior) 
 ;;<li><u>General</u>: Well nourished, well developed no apparent distress. Alert and oriented to simple conversation.&nbsp; She has no idea of the year or day of week.&nbsp; She know she is in doctor's office. Appears good for age of&nbsp; 75yr 
 ;;<li><u>HEENT</u>: Head is normo-cephalic, atraumatic. Eyes: EOMI. Nose: Patent. Mouth: Moist mucous membranes, adequate dentition. 
 ;;<li><u>Neck</u>: Supple, FROM. No bruit, adenopathy, or thyromegally.&nbsp;&nbsp;No supraclavicular nodes 
 ;;<li><u>Heart </u>: Reg. Rate.&nbsp;&nbsp;Normal S2/S2 No murmurs, clicks, gallops, or rubs. 
 ;;<li><u>Lungs</u>: CTAB, no crackles or wheezes.&nbsp;&nbsp;Normal AP diameter. No increased work of breathing 
 ;;<li><u>Abdomen</u>: Normoactive bowel sounds. &nbsp;&nbsp;Palpation is unremarkable. Soft and benign. 
 ;;<li><u>Extremities</u> : Good peripheral perfusion. No significant edema at the ankles 
 ;;<li><u>Back</u> : Good symmetry. Non-tender. 
 ;;<li><u>Neuro</u>: CN II-XII intact (grossly). &nbsp;&nbsp;Gait is a bit slow.&nbsp; 
 ;;<li><u>Skin</u>: Partial skin exam (dressed) reveals no worrisome lesions 
 ;;<li><u>Affect</u>: Full, bright, appropriate. </li></ul>
 ;;<p><b>ASSESSMENT &amp; PLAN <br><FONT size=4>PATIENT VISIT SUMMARY</FONT>:</b> 
 ;;<p>
 ;;<ul>
 ;;<li><u>Multiple issues:</u> See listing of diagnoses and plans outlined above </li></ul>
 ;;<p>
 ;;<p>-- [FINAL MEDICATIONS] ---------<br>*Allergies sync'd with ERx on date : 10/28/20<br>*CSM-Database Review : 10/29/20 <br>*CSM Contract : 06/30/20 <br>*Reconciliation date : 10/28/20<br>*Source of information : verbal/brought<br>apixaban (Eliquis) 2.5 mg po twice Daily<br>buspirone (Buspar) 15 mg po twice Daily<br><b><FONT style="BACKGROUND-COLOR: #ffff99">Clonidine hcl 0.1 mg tablet;1 po q 8 hrs prn severe htn 180/100</b></FONT><br>Diltiazem (Tiazic) 360 mg po po Daily<br>donepezil (Aricept) 5 mg tablet;1 tablet at bedtime.<br>isosorbide (Monoket) ER 30 mg po Daily<br>Lantus 100 UNIT/ML Solution Sig: inject 5 units sq daily<br>levothyroxine sodium 75 mcg po Daily<br>NTG 0.4 mg SL Q5 min for up to 3 doses prn chestpain<br>O2 2 L/min night-time<br>OFF Ipratropium-albuterol 0.5-2.5 (3) Mg/3ml solution;use 1 unit dose in nebulizer 4 times daily.<br>Ondansetron 4 mg tablet disintegrating;1 tab q 4 hr prn nausea/vomiting<br>OTC MTV po Daily<br>OTC Tylenol extra strength po as needed<br>OTC Vitamin-D, 2,000 IU po Daily<br>OTC Zyrtec po Daily<br>pantoprazole (Protonix) 40 mg po twice Daily<br>potassium (Klor-Con) 10 mEq po 2 QAM and 3 at bedtime (50 mEq daily)<br>spironolactone (Aldactone) 25 mg po Daily<br>
 ;;<p><b>FOLLOW UP APPT:</b> 4 Months or sooner if needed; For Group A Problems 
 ;;<DIV name="*FOLLOWUP*"></DIV>
 ;;<p><br><br>
 ;;<TABLE cellSpacing=2 cellPadding=0 bgColor=#d3d3d3 border=0 TMGLABS="1">
 ;;<TBODY>
 ;;<TR bgColor=#f2f2f2>
 ;;<TD>LABS: NON-FASTING LABS. LABS TODAY.</TD></TR>
 ;;<TR bgColor=#f2f2f2>
 ;;<TD>TESTS ORDERED: Lipids, HgbA1c, General Health Panel (80050 - Panel includes TSH, CMP, &amp; CBC w/ Diff).</TD></TR>
 ;;<TR bgColor=#f2f2f2>
 ;;<TD>DIAG: Fatigue - R53.82. HTN - I10. DM-2 - E11.9. Lipids - E78.00.</TD></TR></TBODY></TABLE>
 ;;<p>
 ;;<DIV name="lab_target"></DIV>
 ;;<p><br><br><br>cc: patient 
 ;;<p><br>&nbsp;</P><pre><HR>/es/ Dr James Earl Watson
 ;;Family Physician
 ;;Signed: 06/24/2019 14:12
 ;;<HR>
 ;;</pre></BODY>
 ;;<END>