TMGPXR04 ;TMG/kst/TMG Reminder Dashboard;  8/5/24
         ;;1.0;TMG-LIB;**1**;8/5/24
 ;
 ;"TMG REMINDER FUNCTIONS
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 8/5/24  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;"PUBLIC FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"PRIVATE FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"Changes to database and data dictionary.  
 ;"=======================================================================
 ; 
 ;"Added User class entries: 
 ;"100  REMINDER HANDLING - CLINICAL                        REMCLIN
 ;"101  REMINDER HANDLING NONCLINICAL                       REMNONC
 ;
 ;"Modified Data Dictionary:
 ;" File: REMINDER DEFINITION                                            Branch: 1
 ;" REF  NODE;PIECE     FLD NUM  FIELD NAME
 ;" ===============================================================================
 ;"...
 ;"      TMG;0            22700  USER CLASS HANDLING           <-Mult [811.9005PA]
 ;" 111   -0;1              .01   -USER CLASS HANDLING           <-Pntr  [MP8930']
 ;"  <> <> <>
 ;"=======================================================================
 ;"=======================================================================
 ;"
GTPOPRPT(OUT,ADUZ,ALOC,PARAMS,SORT,INACTIVE,REFRESH,NUMRESULTS,DIRECTION,LASTDISPLAYED)  ;"RPC ENTRY POINT: TMG CPRS GET POP HEALTH REPORT
  ;"Input:  OUT --   PASS BY REFERENCE, AN OUT PARAMETER.  See Result below
  ;"        ADUZ --  IEN of user 
  ;"        ALOC --  IEN of location, needed for reminder evaluation.  
  ;"        PARAMS -- (optional)
  ;"          Format   MODE;Beginning Range;Ending Range
  ;"          MODE -- 
  ;"                If 0, then get patients with APPOINTMENT in date range between Beginning Range and Ending Range
  ;"                If 1, then get patients with last name between Beginning Range and Ending Range
  ;"                If 2, then get all patients   <- DEFAULT
  ;"        SORT -- (optional)
  ;"          If 0, by patient alphabetically
  ;"          If 1, by Reminder, by display order
  ;"        INACTIVE -- (optional)
  ;"          0 (default) = Only active patients
  ;"          1 = All patients including inactive ones
  ;"        REFRESH -- (optional)REFRESH
  
  ;"          0 = Use previously saved list (Previous list will be stored in ^TMP("POP HEALTH REPORT",$J,ADUZ))
  ;"          1 = = Process a new list (this will ignore DIRECTION and LASTDISPLAYED as the list will be fresh)
  ;"        NUMRESULTS -- (optional)
  ;"          If sent, limit the number of results to return
  ;"        DIRECTION -- (optional)
  ;"          If sent, 1 = BACKWARDS
  ;"                   2 = FORWARD
  ;"        LASTDISPLAYED -- (optional)
  ;"           If sent, this is the number of last results that had been sent
  ;" Notes: This RPC will first collect a bolus of patients, based on the PARAM criteria.
  ;"          It will then get a list of Reminders, based on the user (ADUZ)
  ;"          It will cycle through the patients, and test each reminder to see which are due
  ;"          Finally it will output a listing of each reminder due for each patient, in an HTML format for display in the Pop. Health tab of CPRS
  ;" Results : OUT(0) = Status^Message^Partial^FirstShown^LastShown^TotalResults
  ;"                    Status will be -1 (error) or 1 (success)
  ;"                    Message (if error this will contain the error message... if success this will say success)
  ;"                    Partial (0 means the entire list will be contained (Back and Forward buttons will be invisible), 1 means a Partial list was sent)
  ;"                    FirstShown - Number of first result shown
  ;"                    LastShown - Number of last result shown
  ;"                    TotalResults - Number of total results in set
  ;"SET OUT(0)="<html><body><table border=1>"
  NEW OUTIDX SET OUTIDX=2
  NEW MODE,PARAM1,PARAM2,PATARR,OPTION
  NEW PARTIAL,FIRST,LAST,TOTAL,DONE,STARTLIST
  SET OPTION("INCLUDE INACTIVE")=+$G(INACTIVE)
  SET MODE=$P(PARAMS,";",1),PARAM1=$P(PARAMS,";",2),PARAM2=$P(PARAMS,";",3)
  ;"
  ;"Get the patient list, based on the MODE
  NEW PTDUEARR  ;"THIS ARRAY WILL BE ORGANIZED (PTDUEARR("P",PATNAME,TMGDFN,REMDUE)="") and PTDUEARR("R",REMDUE,PATNAME,TMGDFN)="")
  IF REFRESH=0 DO  GOTO DISPREPORT
  . MERGE PTDUEARR=^TMP("POP HEALTH REPORT",$J,ADUZ)
  . IF DIRECTION=0 DO
  . . SET FIRST=1
  . . SET LASTDISPLAYED=NUMRESULTS
  . ELSE  IF DIRECTION=1 DO
  . . SET FIRST=(LASTDISPLAYED-NUMRESULTS)
  . . IF FIRST<1 SET FIRST=1
  . . SET LASTDISPLAYED=FIRST+(NUMRESULTS-1)
  . ELSE  DO
  . . SET FIRST=(LASTDISPLAYED+1)
  . . SET LASTDISPLAYED=FIRST+(NUMRESULTS-1)
  ELSE  DO
  . SET FIRST=1
  . SET LASTDISPLAYED=NUMRESULTS
  . KILL ^TMP("POP HEALTH REPORT",$J,ADUZ)
  . SET ^TMP("POP HEALTH REPORT",$J,ADUZ,"STATUS")="STARTING"
  DO GETPTBOLUS(.PATARR,ADUZ,ALOC,PARAM1,PARAM2,MODE,.OPTION)
  ;"
  ;"Get a listing of all CoverSheet reminders for the provided user
  NEW REMARR    ;"THIS ARRAY WILL CONTAIN ALL OF THE USER'S REMINDERS, TO TEST THE PATIENT'S LIST AGAINST
  DO GETRLIST(.REMARR,ADUZ,ALOC)
  ;"
  ;"Get each reminder for each patient, and return a listing of due reminders for each patient
  KILL ^TMP($J,"PATARR")
  KILL ^TMP($J,"REMARR")
  MERGE ^TMP($J,"PATARR")=PATARR
  MERGE ^TMP($J,"REMARR")=REMARR
  JOB JTESTPAT^TMGPXR04($J,ADUZ)
  ;"DO TESTPATS(.PTDUEARR,.PATARR,.REMARR)
  SET OUT(0)="1^SUCCESS^0^0^0^0"
  SET OUT(1)="<html><body>Please wait....</body></html>"
  QUIT    ;" DON'T RETURN ANYTHING AT THIS POINT
  ;"MERGE ^TMP("POP HEALTH REPORT",$J,ADUZ)=PTDUEARR
  ;"
DISPREPORT   ;"HERE IS WHERE WE WILL DISPLAY THE ACTUAL REPORT  
  ;"HERE WE WILL FORMAT THE OUTPUT FOR THE ARRAY
  NEW TMGNAME,REMIEN,TMGDFN
  SET TMGNAME="",REMIEN=0
  SET DONE=0,STARTLIST=0
  SET PARTIAL=0,LAST=0,TOTAL=0
  SET OUT(1)="<html><body><table border=1>"
  SET OUT(2)="",OUTIDX=OUTIDX+1
  IF SORT=0 DO
  . ;"SET OUT(OUTIDX)="SORTING 0 ON "_$J,OUTIDX=OUTIDX+1
  . FOR  SET TMGNAME=$O(PTDUEARR("P",TMGNAME)) QUIT:TMGNAME=""  DO
  . . ;"SET OUT(OUTIDX)=TMGNAME,OUTIDX=OUTIDX+1
  . . SET TMGDFN=0
  . . FOR  SET TMGDFN=$O(PTDUEARR("P",TMGNAME,TMGDFN)) QUIT:TMGDFN'>0  DO
  . . . SET REMIEN=0
  . . . SET TOTAL=TOTAL+1
  . . . IF TOTAL=FIRST SET STARTLIST=1
  . . . IF (STARTLIST=1)&(DONE=0) DO
  . . . . IF TOTAL=LASTDISPLAYED SET DONE=1
  . . . . SET LAST=TOTAL
  . . . . NEW REMSTR SET REMSTR=""
  . . . . FOR  SET REMIEN=$O(PTDUEARR("P",TMGNAME,TMGDFN,REMIEN)) QUIT:REMIEN'>0  DO
  . . . . . IF REMSTR'="" SET REMSTR=REMSTR_"<br>"
  . . . . . SET REMSTR=REMSTR_$P($G(^PXD(811.9,REMIEN,0)),"^",1)
  . . . . DO ADDONE(.OUT,.OUTIDX,TMGNAME,TMGDFN,REMSTR,SORT,0)
  ELSE  IF SORT=1 DO
  . ;"SET OUT(OUTIDX)="SORTING 1",OUTIDX=OUTIDX+1
  . FOR  SET REMIEN=$O(PTDUEARR("R",REMIEN)) QUIT:REMIEN'>0  DO
  . . SET TMGNAME=""
  . . FOR  SET TMGNAME=$O(PTDUEARR("R",REMIEN,TMGNAME)) QUIT:TMGNAME=""  DO
  . . . SET TMGDFN=0
  . . . FOR  SET TMGDFN=$O(PTDUEARR("R",REMIEN,TMGNAME,TMGDFN)) QUIT:TMGDFN'>0  DO
  . . . . SET TOTAL=TOTAL+1
  . . . . IF TOTAL=FIRST SET STARTLIST=1
  . . . . IF (STARTLIST=1)&(DONE=0) DO
  . . . . . ;"IF TOTAL=(LASTDISPLAYED+NUMRESULTS) SET LAST=TOTAL,LASTDISPLAYED=LASTDISPLAYED+1,DONE=1
  . . . . . IF TOTAL=LASTDISPLAYED SET DONE=1
  . . . . . SET LAST=TOTAL
  . . . . . DO ADDONE(.OUT,.OUTIDX,TMGNAME,TMGDFN,$P($G(^PXD(811.9,REMIEN,0)),"^",1),SORT,REMIEN)
  IF (FIRST'=1)!(TOTAL'=LAST) SET PARTIAL=1
  SET OUT(OUTIDX)="</table></body></html>"
  NEW STATUS SET STATUS=$G(^TMP("POP HEALTH REPORT",$J,ADUZ,"STATUS"))
  SET OUT(0)="1^SUCCESS^"_PARTIAL_"^"_FIRST_"^"_LAST_"^"_TOTAL_"^"_STATUS
  QUIT
  ;"
ADDONE(OUT,OUTIDX,PATNAME,TMGDFN,REMNAME,MODE,REMIEN)  ;"HERE IS WHERE WE WILL ENTER THE INFORMATION INTO THE HTML OUTPUT
  ;"MODE WILL BE 0=PATIENT FIRST
  ;"             1=REMINDER FIRST
  NEW OUTSTR SET OUTSTR=""
  IF MODE=0 DO
  . SET OUTSTR="<tr><td><a href="""_"DFN-"_TMGDFN_""" title="""_PATNAME_"""> "_PATNAME_"</a></td>"
  . SET OUTSTR=OUTSTR_"<td>"_REMNAME_"</td></tr>"
  ELSE  IF MODE=1 DO
  . SET OUTSTR="<tr><td><a href="""_"DFN-"_TMGDFN_"&REM-"_REMIEN_""" title="""_PATNAME_"""> "_REMNAME_"</td>"
  . SET OUTSTR=OUTSTR_"<td><a href="""_"DFN-"_TMGDFN_""" title="""_PATNAME_"""> "_PATNAME_"</a></td></tr>"
  SET OUT(OUTIDX)=OUTSTR,OUTIDX=OUTIDX+1
  QUIT
  ;"
GETPTBOLUS(OUT,ADUZ,ALOC,START,END,MODE,OPTION) ;"Get list of patients for Reminder Dashboard 
  ;"Input:  OUT --   PASS BY REFERENCE, AN OUT PARAMETER.  
  ;"                 Format: OUT(DFN)=""
  ;"        ADUZ --  IEN of user 
  ;"        ALOC --  IEN of location, needed for reminder evaluation.  
  ;"        START -- Start of range.  If date, should be FMDT
  ;"        END   -- End of range.    If date, should be FMDT
  ;"        MODE -- OPTIONAL
  ;"                If 0, then get patients with APPOINTMENT in date range
  ;"                If 1, then get patients with last name between START and END.  In this case, START and END are alphabetic
  ;"                If 2, then get all patients   <- DEFAULT
  ;"        OPTION -- Optional. 
  ;"            OPTION("INCLUDE INACTIVE")=1  -- include patients considered inactive
  ; 
  NEW TEMPARR
  NEW PTCOUNT SET PTCOUNT=0
  NEW INACTIVE SET INACTIVE=+$G(OPTION("INCLUDE INACTIVE"))
  IF MODE=0 DO   ;"This MODE is patients by Appointment date
  . DO GETSCHED^TMGPXR03(.TEMPARR,PARAM1,PARAM2)  ;"Results will be by DFN. Ready to return
  ELSE  IF MODE=1 DO  ;"This MODE is patients by last initial
  . NEW TMGDFN,NAME SET NAME=PARAM1  ;"BEGIN WITH THE BEGINNING LETTER
  . FOR  SET NAME=$O(^DPT("B",NAME)) QUIT:(NAME="")!($ASCII($E(NAME,1,1))>$ASCII(PARAM2))  DO
  . . SET TMGDFN=0
  . . FOR  SET TMGDFN=$O(^DPT("B",NAME,TMGDFN)) QUIT:TMGDFN'>0  DO
  . . . IF INACTIVE=1 DO
  . . . . SET TEMPARR(TMGDFN)=""
  . . . ELSE  IF $$ACTIVEPT^TMGPXR03(TMGDFN,3)=1 DO
  . . . . SET TEMPARR(TMGDFN)=""
  ELSE  IF MODE=2 DO  ;"This MODE is all patients
  . NEW TMGDFN SET TMGDFN=0
  . FOR  SET TMGDFN=$O(^DPT(TMGDFN)) QUIT:TMGDFN'>0  DO
  . . IF INACTIVE=1 DO
  . . . SET TEMPARR(TMGDFN)=""
  . . ELSE  IF $$ACTIVEPT^TMGPXR03(TMGDFN,3)=1 DO
  . . . SET TEMPARR(TMGDFN)=""
  MERGE OUT=TEMPARR
  QUIT
  ;
JTESTPAT(ParentJ,ADUZ)
  SET ^TMP("POP HEALTH REPORT",ParentJ,ADUZ,"STATUS")="JOB LAUNCHED"
  NEW PATARR,PTDUEARR,REMARR
  MERGE PATARR=^TMP(ParentJ,"PATARR")
  NEW PTCOUNT SET PTCOUNT=0
  NEW DFN SET DFN=0
  FOR  SET DFN=$O(PATARR(DFN)) QUIT:DFN'>0  DO
  . SET PTCOUNT=PTCOUNT+1
  SET PATARR("TOTAL COUNT")=PTCOUNT
  MERGE REMARR=^TMP(ParentJ,"REMARR")
  DO DT^DICRW  ;"ensure Fileman environment.
  ;" NEW U SET U="^"
  DO TESTPATS(.PTDUEARR,.PATARR,.REMARR,ParentJ)
  QUIT
  ;"
TESTPATS(PTDUEARR,PATARR,REMARR,ParentJ) ;"This function will test each patient against each Reminder
 NEW PATCOUNT SET PATCOUNT=$G(PATARR("TOTAL COUNT"))
 KILL PATARR("TOTAL COUNT")
 NEW CURPAT SET CURPAT=0
 SET ^TMP("POP HEALTH REPORT",ParentJ,ADUZ,"STATUS")="UPDATING"
 NEW TMGDFN SET TMGDFN=0
 FOR  SET TMGDFN=$O(PATARR(TMGDFN)) QUIT:TMGDFN'>0  DO
 . SET CURPAT=CURPAT+1
 . SET ^TMP("POP HEALTH REPORT",ParentJ,ADUZ,"STATUS")="RUNNING #"_CURPAT_" OF "_PATCOUNT
 . NEW PATNAME SET PATNAME=$P($G(^DPT(TMGDFN,0)),"^",1)
 . NEW REMIEN SET REMIEN=0
 . FOR  SET REMIEN=$O(REMARR(REMIEN)) QUIT:REMIEN'>0  DO
 . . ;"SET ^TMP("POP HEALTH REPORT",ParentJ,ADUZ,"STATUS")="TESTING "_REMIEN_" ON "_PATNAME
 . . NEW X DO NOW^%DTC       
 . . NEW REMRESULT SET REMRESULT=$$DOREM^TMGPXR03(TMGDFN,REMIEN,5,X)  ;"This will run the reminder and return "DUE NOW" if due
 . . ;"SET ^TMP("POP HEALTH REPORT",ParentJ,ADUZ,"STATUS")="FINISHED TEST "_REMIEN_" ON "_PATNAME
 . . ;"SET ^TMP("POP HEALTH REPORT",ParentJ,ADUZ,"REM STATUS")=REMRESULT
 . . IF REMRESULT["DUE NOW" DO
 . . . SET PTDUEARR("P",PATNAME,TMGDFN,REMIEN)=""   ;"P FOR PATIENT INDEX
 . . . SET PTDUEARR("R",REMIEN,PATNAME,TMGDFN)=""   ;"R FOR REMINDER INDEX
 . . . MERGE ^TMP("POP HEALTH REPORT",ParentJ,ADUZ)=PTDUEARR  ;"KEEP UPDATING THE GLOBAL
 SET ^TMP("POP HEALTH REPORT",ParentJ,ADUZ,"STATUS")="COMPLETED"
 QUIT  
  ;"=======================================================================================================================================================================================
  ;"======================THE FOLLOWING FUNCTIONS WERE ALL COPIED FROM ORQQPX AND CUSTOMIZED SO WE COULD USE A PROVIDED DUZ AS A PARAMETER=================================================
  ;"=======================================================================================================================================================================================
GETRLIST(OUT,ADUZ,ORLOC) ;Returns a list of all cover sheet reminders
  ;"  THIS IS COPIED FROM GETLIST^ORQQPX, so the DUZ could be a parameter rather than assumed
  N I,ORY
  D REMLIST(.ORY,ADUZ,$G(ORLOC))
  S I=0
  F  S I=$O(ORY(I)) Q:'I  D
  .S ORY(I)=$P(ORY(I),U,2)
  NEW REMIEN,OUTIDX,TEMPIDX
  SET TEMPIDX=0,OUTIDX=1
  FOR  SET TEMPIDX=$O(ORY(TEMPIDX)) QUIT:TEMPIDX'>0  DO
  . SET OUT($G(ORY(TEMPIDX)))="",OUTIDX=OUTIDX+1
  QUIT
  ;"
REMLIST(ORY,ADUZ,LOC)   ;"  THIS IS COPIED FROM REMLIST^ORQQPX, so the DUZ could be a parameter rather than assumed
  N SRV,I,J,ORLST,CODE,IDX,IEN,NEWP
  ;S SRV=$P($G(^VA(200,ADUZ,5)),U)
  S SRV=$$GET1^DIQ(200,ADUZ,29,"I")
  D NEWCVOK^ORQQPX(.NEWP)
  I 'NEWP D GETLST^XPAR(.ORY,"USR^LOC.`"_$G(LOC)_"^SRV.`"_+$G(SRV)_"^DIV^SYS^PKG","ORQQPX SEARCH ITEMS","Q",.ORERR) Q
  D REMACCUM(.ORLST,ADUZ,"PKG","Q",1000)
  D REMACCUM(.ORLST,ADUZ,"SYS","Q",2000)
  D REMACCUM(.ORLST,ADUZ,"DIV","Q",3000)
  I +SRV D REMACCUM^ORQQPX(.ORLST,"SRV.`"_+$G(SRV),"Q",4000)
  I +LOC D REMACCUM^ORQQPX(.ORLST,"LOC.`"_+$G(LOC),"Q",5000)
  D REMACCUM(.ORLST,ADUZ,"CLASS","Q",6000)
  D REMACCUM(.ORLST,ADUZ,"USR","Q",7000)
  S I=0
  F  S I=$O(ORLST(I)) Q:'I  D
  .S IDX=$P(ORLST(I),U,1)
  .F  Q:'$D(ORY(IDX))  S IDX=IDX+1
  .S CODE=$E($P(ORLST(I),U,2),2)
  .S IEN=$E($P(ORLST(I),U,2),3,999)
  .I CODE="R" D ADDREM^ORQQPX(.ORY,IDX,IEN)
  .I CODE="C" D ADDCAT^ORQQPX(.ORY,IDX,IEN)
  K ORY("B")
  QUIT
  ;"
REMACCUM(ORY,ADUZ,LVL,TYP,SORT,CLASS) ; Accumulates ORTMP into ORY ;"  THIS IS COPIED FROM REMACCUM^ORQQPX, so the DUZ could be a parameter rather than assumed
 ; Format of entries in ORQQPX COVER SHEET REMINDERS:
 ;   L:Lock;R:Remove;N:Normal / C:Category;R:Reminder / Cat or Rem IEN
 N IDX,I,J,K,M,FOUND,ORERR,ORTMP,FLAG,IEN
 N FFLAG,FIEN,OUT,P2,ADD,DOADD,CODE
 I LVL="CLASS" D  I 1
 .N ORLST,ORCLS,ORCLSPRM,ORWP
 .S ORCLSPRM="ORQQPX COVER SHEET REM CLASSES"
 .D GETLST^XPAR(.ORLST,"SYS",ORCLSPRM,"Q",.ORERR)
 .S I=0,M=0,CLASS=$G(CLASS)
 .F  S I=$O(ORLST(I)) Q:'I  D
 ..S ORCLS=$P(ORLST(I),U,1)
 ..I +CLASS S ADD=(ORCLS=+CLASS) I 1
 ..E  S ADD=$$ISA^USRLM(ADUZ,ORCLS,.ORERR)
 ..I +ADD D
 ...D GETWP^XPAR(.ORWP,"SYS",ORCLSPRM,ORCLS,.ORERR)
 ...S K=0
 ...F  S K=$O(ORWP(K)) Q:'K  D
 ....S M=M+1
 ....S J=$P(ORWP(K,0),";",1)
 ....S ORTMP(M)=J_U_$P(ORWP(K,0),";",2)
 E  D GETLST^XPAR(.ORTMP,LVL,"ORQQPX COVER SHEET REMINDERS",TYP,.ORERR)
 S I=0,IDX=$O(ORY(999999),-1)+1,ADD=(SORT="")
 F  S I=$O(ORTMP(I)) Q:'I  D
 .S (FOUND,J)=0,P2=$P(ORTMP(I),U,2)
 .S FLAG=$E(P2),IEN=$E(P2,2,999)
 .I ADD S DOADD=1
 .E  D
 ..S DOADD=0
 ..F  S J=$O(ORY(J)) Q:'J  D  Q:FOUND
 ...S P2=$P(ORY(J),U,2)
 ...S FIEN=$E(P2,2,999)
 ...I FIEN=IEN S FOUND=J,FFLAG=$E(P2)
 ..I FOUND D  I 1
 ...I FLAG="R",FFLAG'="L" K ORY(FOUND)
 ...I FLAG'=FFLAG,(FLAG_FFLAG)["L" S $E(P2)="L",$P(ORY(FOUND),U,2)=P2
 ..E  I (FLAG'="R") S DOADD=1
 .I DOADD D
 ..S OUT(IDX)=ORTMP(I)
 ..S $P(OUT(IDX),U)=$P(OUT(IDX),U)_SORT
 ..I SORT="" S OUT(IDX)=$$ADDNAME^ORQQPX(OUT(IDX))
 ..S IDX=IDX+1
 M ORY=OUT
 Q
 ;"

 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
GETREMLIST(OUT,ADUZ,ALOC,) ;"Get list of applicable reminders for user  
  ;"Input:  OUT -- PASS BY REFERENCE, AN OUT PARAMETER.  See Result below
  ;"        ADUZ -- User IEN to check reminders with.
  ;"        ALOC -- IEN of location, needed for reminder evaluation.  
  ;"Result: none.  But OUT set as below
  ;"        OUT(<ReminderIEN>)=<ReminderIEN>^<Reminder Display Name>
  NEW ARR DO ALL^PXRMRPCA(.ARR) ;"All active reminders.   ARR(#)=print name^ien
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:IDX'>0  DO
  . NEW ENTRY SET ENTRY=$GET(ARR(IDX)) QUIT:ENTRY=""
  . NEW IEN SET IEN=$PIECE(ENTRY,"^",2) QUIT:IEN'>0
  . NEW NAME SET NAME=$PIECE(ENTRY,"^",1) 
  . IF $$REMAPPLIES(IEN,ADUZ)=0 QUIT
  . SET OUT(IEN)=IEN_"^"_NAME  
  QUIT
  ;
REMAPPLIES(REMIEN,ADUZ,ADT,TMP)  ;"Does reminder apply to user?
  ;"Input:  REMIEN -- IEN 811.9, REMINDER DEFINITION
  ;"        ADUZ -- User IEN to check
  ;"        ADT -- OPTIONAL. FMDT.   Default is NOW
  ;"        TMP -- PASS BY REFERENCE.  An array to speed up processing by avoiding repeat lookups.  
  NEW TMGRESULT SET TMGRESULT=1  ;"Default to DOES APPLY
  NEW REMCLINICAL SET REMCLINICAL=$$HASCLASS(ADUZ,"REMINDER HANDLING CLINICAL",.ADT,.TMP)
  NEW REMCLERICAL SET REMCLERICAL=$$HASCLASS(ADUZ,"REMINDER HANDLING NONCLINICAL",.ADT,.TMP)
  NEW REMUSER SET REMUSER=((REMCLINICAL>0)!(REMCLERICAL>0))
  IF REMUSER DO
  . ;"Does reminder have class matching user?
  . IF REMCLINICAL
  . 
  QUIT TMGRESULT
  ; 
HASCLASS(ADUZ,CLASSNAME,ADT,TMP) ;
  ;"Input: ADUZ -- User IEN to check
  ;"       CLASSNAME -- name of class, e.g. 'REMINDER HANDLING - CLINICAL', or 'REMINDER HANDLING NONCLINICAL'
  ;"       ADT -- OPTIONAL. FMDT.   Default is NOW
  ;"       TMP -- PASS BY REFERENCE.  An array to speed up processing by avoiding repeat lookups.  
  ;"Result: 0 if user does not have active class entry
  ;"        ClassIEN if does have entry.  
  NEW TMGRESULT SET TMGRESULT=0  ;"default to NOT having class
  NEW CLASSIEN SET CLASSIEN=+$ORDER(TMP("B",CLASSNAME,0))
  IF CLASSIEN'>0 DO
  . SET CLASSIEN=+$ORDER(^USR(8930,"B",CLASSNAME,0)) QUIT:CLASSIEN'>0
  . SET TMP("B",CLASSNAME,CLASSIEN)=""
  IF CLASSIEN'>0 GOTO HCDN
  NEW SUBIEN SET SUBIEN=$ORDER(^VA(200,ADUZ,"USC1","B",CLASSIEN,0))
  IF SUBIEN'>0 GOTO HCDN
  NEW ZN SET ZN=$GET(^VA(200,ADUZ,"USC1",SUBIEN,0))
  SET ADT=+$GET(ADT) IF ADT'>0 SET ADT=$$NOW^XLFDT()
  NEW SDT SET SDT=+$PIECE(ZN,"^",2)
  NEW EDT SET EDT=+$PIECE(ZN,"^",3)
  IF SDT>0,SDT>ADT GOTO HCDN  ;"Start date is in future from ADT
  IF EDT>0,ADT>EDT GOTO HCDN  ;"ADT is past end date
  SET TMGRESULT=CLASSIEN  ;"If we got her, then patient has class.  
HCDN ;
  QUIT TMGRESULT
  ;
GETDUEBOLUS(OUT,ADUZ,ALOC,PATARR)  ;"
  ;"Input:  OUT -- PASS BY REFERENCE, AN OUT PARAMETER.  See Result below
  ;"        ADUZ -- User IEN to check reminders with.
  ;"        ALOC -- IEN of location, needed for reminder evaluation.  
  ;"        PATARR -- PASS BY REFEERENCE.  Format:
  ;"           PATARR(ADFN)=""
  ;"Result: none.  But OUT set as below
  ;"        OUT("XBYPT",ADFN)=<ReminderIEN>^<Reminder Display Name>
  ;"        OUT("XBYRMDR",<Reminder Display Name>,ADFN)=<ReminderIEN>^<Reminder Display Name>
  ;
  QUIT
  ;