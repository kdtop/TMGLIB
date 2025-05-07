TMGAPPT1 ;TMG/kst-Appointment Related Fns;11/08/08, 6/3/24
         ;;1.0;TMG-LIB;**1,17**;11/08/08
 ;
 ;"Kevin Toppenberg MD
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 05/22/2017  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"---------------------------------------------------------------------------
 ;"PUBLIC FUNCTIONS
 ;"---------------------------------------------------------------------------
 ;
 ;"PROAPPT(Y,PROVIDER,ORBDATE,OREDATE) -- RETURN LIST OF APPOINTMENTS FOR A PROVIDER    
 ;"PROVIDER(RESULT)  -- RETURN ALL PROVIDERS
 ;"PTSTATUS(TMGDFN,APPTDATE)                                   
 ;"CHKNOTES(TMGDFN)  
 ;"NOPATSEL(OUT,TMGDUZ) -- Called from RPC TMG CPRS NO PATIENT SELECTED
 ;"GETAPPT(OUT,PROVIDER,SDT,EDT,OPTION) -- RETURN LIST OF APPOINTMENTS
 ;
 ;"---------------------------------------------------------------------------
 ;"PRIVATE FUNCTIONS
 ;"---------------------------------------------------------------------------
 ;"TEST2 ;
 ;"TEST1 ;
 ;"NOPATSEL0(OUT,TMGDUZ)-- depreciated -- Called from RPC TMG CPRS NO PATIENT SELECTED
 ;
 ;"---------------------------------------------------------------------------
 ;
PROAPPT(Y,PROVIDER,ORBDATE,OREDATE)     ;" RETURN LIST OF APPOINTMENTS FOR A PROVIDER    
  ;"Input:  Y -- PASS BY REFERENCE, AN OUT PARAMETER.   Format:  
  ;"              Y(#)="<error message>"  OR
  ;"              Y(#)=DFN^PATIENTNAME_STATUS^PROVIDER^APPTDATE^(APPTTYPE)   STATUS is '+' or '*'
  ;"         E.g. Y(7)=12345^HAVARD,HAROLD^83^3240418.093^ (3 MO CHECK)
  ;"        PROVIDER -- DUZ, user IEN.  OPTIONAL.  If not provided, then all schedule for all providers returned.  
  ;"        ORBDATE -- BEGINNING DATE (FM format)
  ;"        OREDATE -- ENDING DATE (FM format)
  ;"Result: none. 
  ;
  ;"//kt 4/18/24  +$GET(PROVIDER)<1 SET Y(1)="^No provider identified" Q
  SET PROVIDER=+$GET(PROVIDER)  ;"//kt 4/18/24  
  SET Y(1)="^NO APPOINTMENTS FOUND"
  NEW ORI,APPTIEN,APPTDATE
  IF ORBDATE="" SET Y(1)="^No beginning date sent" Q
  IF OREDATE="" SET Y(1)="^No ending date sent" Q
  ;
  ; Convert ORBDATE, OREDATE to FM Date/Time:
  DO DT^DILF("T",ORBDATE,.ORBDATE,"","")
  DO DT^DILF("T",OREDATE,.OREDATE,"","")
  IF (ORBDATE=-1)!(OREDATE=-1) S Y(1)="^Error in date range." Q
  SET OREDATE=$PIECE(OREDATE,".")_.9999 ;
  SET ORI=0,APPTDATE=ORBDATE-0.000001
  FOR  SET APPTDATE=$ORDER(^TMG(22723,"DT",APPTDATE)) QUIT:(APPTDATE>OREDATE)!(APPTDATE'>0)  DO
  . NEW TMGDFN SET TMGDFN=0
  . FOR  SET TMGDFN=$ORDER(^TMG(22723,"DT",APPTDATE,TMGDFN)) QUIT:TMGDFN'>0  DO
  . . NEW APPTIEN SET APPTIEN=$ORDER(^TMG(22723,"DT",APPTDATE,TMGDFN,0))
  . . NEW PATNAME,ZN,DOCTOR,STATUS,APPTTYPE
  . . SET STATUS=$GET(^TMG(22723,"DT",APPTDATE,TMGDFN,APPTIEN))
  . . IF STATUS="C" QUIT   ;"C = cancelled
  . . ;"10/2/20 Per Dr. Dee, start including old appts now IF STATUS="O" QUIT  ;"9/15/20  don't include Old appts for now
  . . SET ZN=$GET(^TMG(22723,TMGDFN,1,APPTIEN,0))
  . . SET DOCTOR=$PIECE(ZN,"^",3)
  . . IF PROVIDER>0,DOCTOR'=PROVIDER QUIT
  . . SET APPTTYPE=$PIECE(ZN,"^",4)
  . . IF $$UP^XLFSTR(APPTTYPE)["INJ ONLY" QUIT
  . . SET PATNAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
  . . SET ORI=ORI+1
  . . NEW STATUS 
  . . IF ORBDATE<$$TODAY^TMGDATE DO
  . . . SET STATUS=""
  . . ELSE  DO
  . . . SET STATUS=$$PTSTATUS(TMGDFN,APPTDATE)
  . . SET Y(ORI)=TMGDFN_"^"_PATNAME_STATUS_"^"_PROVIDER_"^"_APPTDATE_"^ ("_APPTTYPE_")"
  IF PROVIDER=168 DO   ;"THIS CAN EVENTUALLY BE MADE A PARAMETER
  . ;"SORT entries to that completed entries are put to bottom of list.  
  . NEW TOPITEMS,BOTITEMS
  . NEW TOPIDX,BOTIDX
  . SET TOPIDX=0,BOTIDX=0
  . NEW IDX SET IDX=0
  . FOR  SET IDX=$O(Y(IDX)) QUIT:IDX'>0  DO
  . . IF $PIECE($GET(Y(IDX)),"^",2)["*" DO
  . . . SET BOTIDX=BOTIDX+1
  . . . SET BOTITEMS(BOTIDX)=$GET(Y(IDX))
  . . ELSE  DO
  . . . SET TOPIDX=TOPIDX+1
  . . . SET TOPITEMS(TOPIDX)=$GET(Y(IDX))
  . KILL Y
  . SET IDX=0,TOPIDX=0,BOTIDX=0
  . FOR  SET TOPIDX=$O(TOPITEMS(TOPIDX)) QUIT:TOPIDX'>0  DO
  . . SET IDX=IDX+1
  . . SET Y(IDX)=$GET(TOPITEMS(TOPIDX))
  . FOR  SET BOTIDX=$O(BOTITEMS(BOTIDX)) QUIT:BOTIDX'>0  DO
  . . SET IDX=IDX+1
  . . SET Y(IDX)=$GET(BOTITEMS(BOTIDX))
  IF $$SHOULDGARBLE^TMGMISC4() DO GARBLERESULTS^TMGMISC4(.Y)     ;"check for special mode to hide patient info during demos
  QUIT
  ;"
PROVIDER(RESULT)  ;"RETURN ALL PROVIDERS
  SET RESULT(1)="168^Toppenberg,Kevin S"
  SET RESULT(2)="83^Toppenberg,Marcia Dee"
  QUIT
  ;
PTSTATUS(TMGDFN,APPTDATE)   ;"                                
  ;"Purpose:  determine status of patient visit, specifically if note signed etc. 
  ;"         if so return notation, if not return nothing
  ;"Result:  * --> note for visit signed.  
  ;"         + --> note for visit unsigned.  
  NEW TMGRESULT SET TMGRESULT=""
  SET TMGDFN=+$GET(TMGDFN)
  IF TMGDFN'>0 GOTO PTDN
  SET APPTDATE=$PIECE(APPTDATE,".",1)
  ;"NEW PTDATA SET PTDATA=$$GETPDATA^TMGORQPT(TMGDFN,"NOT NEEDED",APPTDATE)
  ;"IF ($PIECE(PTDATA,",",7)'="")&($PIECE(PTDATA,",",8)'="") SET TMGRESULT=" *"
  ;"
  NEW NOTATION SET NOTATION=$$CHKNOTES(TMGDFN)
  IF NOTATION'="" SET TMGRESULT=NOTATION
PTDN ;
  QUIT TMGRESULT
  ;"
CHKNOTES(TMGDFN)  ;"
  ;"Result:  * --> note for visit signed 
  ;"         + --> note for visit unsigned.  
  NEW TMGRESULT SET TMGRESULT=""
  NEW NOTATION SET NOTATION=""
  NEW TIUIEN SET TIUIEN=0
  FOR  SET TIUIEN=$O(^TIU(8925,"C",TMGDFN,TIUIEN)) QUIT:TIUIEN'>0  DO
  . NEW ZN SET ZN=$GET(^TIU(8925,TIUIEN,0))
  . NEW TIUTYPE SET TIUTYPE=$P(ZN,"^",1)
  . NEW OFFICEVISIT SET OFFICEVISIT=$P($G(^TIU(8925.1,TIUTYPE,"TMGH")),"^",1)
  . NEW STATUS,DATE
  . SET STATUS=$PIECE(ZN,"^",5)
  . SET DATE=$PIECE($PIECE($GET(^TIU(8925,TIUIEN,12)),"^",1),".",1)
  . NEW AUTHOR SET AUTHOR=$PIECE($GET(^TIU(8925,TIUIEN,12)),"^",2)
  . ;"ADDING OFFICEVISIT TO BOTH IFS. REMOVED &(AUTHOR=DUZ) BELOW 
  . ;"IF (DATE=$$TODAY^TMGDATE)&(STATUS=5)&(AUTHOR=DUZ)&(OFFICEVISIT="Y") DO   ;"status 5 --> UNSIGNED
  . IF (DATE=$$TODAY^TMGDATE)&(STATUS=5)&(OFFICEVISIT="Y") DO   ;"status 5 --> UNSIGNED
  . . SET TMGRESULT=" +"
  . . ;"NEW LINE SET LINE=0
  . . ;"FOR  SET LINE=$O(^TIU(8925,TIUIEN,"TEXT",LINE)) QUIT:LINE'>0  DO
  . . ;". NEW TEXT SET TEXT=$GET(^TIU(8925,TIUIEN,"TEXT",LINE,0))
  . . ;". IF TEXT["=%=" SET NOTATION=" &"
  . . ;". IF TEXT["=&amp;=" SET NOTATION=" &"
  . ELSE  IF (DATE=$$TODAY^TMGDATE)&(STATUS=7)&(OFFICEVISIT="Y") DO   ;"status 7 --> COMPLETED
  . . SET TMGRESULT=" *"
  IF NOTATION'="" SET TMGRESULT=NOTATION
  QUIT TMGRESULT
  ;"
NOPATSEL0(OUT,TMGDUZ)  ;"Called from RPC "TMG CPRS NO PATIENT SELECTED"
  ;"  Returns the HTML to be displayed in wbNoPatientSelected TWebBrowser when
  ;"       No Patient Is Currently Selected
  ZLINK "TMGTEST"
  DO NOPATSEL^TMGTEST(.OUT,.TMGDUZ)
  QUIT
  ;
NOPATSEL(OUT,TMGDUZ)  ;"Called from RPC "TMG CPRS NO PATIENT SELECTED"
  ;"  Returns the HTML to be displayed in wbNoPatientSelected TWebBrowser when
  ;"       No Patient Is Currently Selected
  NEW INFO,OPTION
  NEW NOW SET NOW=$$NOW^XLFDT()
  NEW NOWSTR SET NOWSTR=$$FMTE^XLFDT(NOW,"2PMZ")
  SET TMGDUZ=$GET(TMGDUZ)
  IF (TMGDUZ'=168)&(TMGDUZ'=83) SET TMGDUZ=0
  DO GETAPPT^TMGAPPT1(.INFO,.TMGDUZ,NOW\1,NOW\1,.OPTION)
  NEW HEADERSTR SET HEADERSTR=""
  IF TMGDUZ'=0 SET HEADERSTR=" for "_$P($G(^VA(200,TMGDUZ,0)),"^",1)
  NEW I SET I=0
  SET I=I+1,OUT(I)="<!DOCTYPE html>"
  SET I=I+1,OUT(I)="<html>"
  SET I=I+1,OUT(I)="<head>"
  SET I=I+1,OUT(I)="<style>"
  SET I=I+1,OUT(I)="table {"
  SET I=I+1,OUT(I)="  font-family: arial, sans-serif; "
  SET I=I+1,OUT(I)="  border-collapse: collapse; "
  SET I=I+1,OUT(I)="  width: 100%; "
  SET I=I+1,OUT(I)="} "
  SET I=I+1,OUT(I)=""
  SET I=I+1,OUT(I)="td, th {"
  SET I=I+1,OUT(I)="  border: 1px solid #dddddd;  "
  SET I=I+1,OUT(I)="  text-align: center; "
  SET I=I+1,OUT(I)="  padding: 6px; "                             
  SET I=I+1,OUT(I)="}"
  SET I=I+1,OUT(I)=""
  SET I=I+1,OUT(I)="</style>   "                                           
  SET I=I+1,OUT(I)="</head>"                                               
  SET I=I+1,OUT(I)="<body> "                                               
  SET I=I+1,OUT(I)=" "                                                     
  ;"SET I=I+1,OUT(I)="<h2>Schedule"_HEADERSTR_"</h2> "                                    
  ;"SET I=I+1,OUT(I)="<h3>As of "_NOWSTR_"</h2> "     
  SET I=I+1,OUT(I)="<table><tr><td><h2>Schedule"_HEADERSTR_"</h2><h3><p>As of "_NOWSTR_"</h3></td><td>"
  DO GETSTATS(.OUT,.I,.INFO)
  SET I=I+1,OUT(I)="</td></tr></table>"
  SET I=I+1,OUT(I)=""                                                      
  SET I=I+1,OUT(I)="<table style=""font-size: 16px"">"                                               
  SET I=I+1,OUT(I)="  <tr> "                                               
  SET I=I+1,OUT(I)="    <th>Time</th> "                                    
  SET I=I+1,OUT(I)="    <th>Patient</th> "                                 
  SET I=I+1,OUT(I)="    <th>Reason</th>"
  SET I=I+1,OUT(I)="    <th>Visit Status</th>"
  SET I=I+1,OUT(I)="    <th>Note Status</th>"
  SET I=I+1,OUT(I)="    <th>Check Status</th>"
  SET I=I+1,OUT(I)="    <th>Check In</th>"
  SET I=I+1,OUT(I)="    <th>Check Out</th>"
  SET I=I+1,OUT(I)="  </tr> "                                              
  NEW JDX SET JDX=0    
  NEW LASTTIME SET LASTTIME=""
  FOR  SET JDX=$ORDER(INFO(JDX)) QUIT:JDX'>0  DO
  . NEW ENTRY SET ENTRY=$GET(INFO(JDX)) QUIT:ENTRY=""
  . ;"FORMAT: DFN^PATIENTNAME^STATUS^PROVIDER^APPTDATE^APPTTYPE^DTIN^DTOUT^APPTDELTA^TIMEHERE
  . NEW ADFN SET ADFN=$PIECE(ENTRY,"^",1)                             ;"Patient's DFN
  . NEW PTNAME SET PTNAME=$PIECE(ENTRY,"^",2)                         ;"Patient's name
  . IF PTNAME["NO APPOINTMENTS FOUND" DO  QUIT
  . . SET I=I+1,OUT(I)="<tr bgcolor=""#b3ffb3""><td colspan=""8""><b>NO APPOINTMENTS FOUND</b></td></tr>"
  . NEW LAST,FIRST
  . SET LAST=$E($P(PTNAME,",",1),1,3)
  . SET FIRST=$E($P(PTNAME,",",2),1,3)
  . NEW PTABBR SET PTABBR=LAST_","_FIRST
  . NEW NOTESTATUS SET NOTESTATUS=$PIECE(ENTRY,"^",3)                 ;"* --> signed note,  + --> unsigned note, or ''
  . NEW PROV SET PROV=$PIECE(ENTRY,"^",4)                             ;"IEN of provider.  
  . NEW APPTDT SET APPTDT=$PIECE(ENTRY,"^",5)                         ;"FMDT for appt
  . NEW APPTDTSTR SET APPTDTSTR=$PIECE($$FMTE^XLFDT(APPTDT,"2PMZ")," ",2,3)           ;"MM/DD/YY@HH:MM am/pm
  . IF (LASTTIME["am")&(APPTDTSTR["pm") DO
  . . SET I=I+1,OUT(I)="<tr bgcolor=""$000000"" height=""5""><td colspan=""8""></td><tr>"
  . SET LASTTIME=APPTDTSTR
  . NEW APPTTYPE SET APPTTYPE=$PIECE(ENTRY,"^",6)                     ;"e.g. 1 MO CHECK
  . NEW APPTCOLOR DO APPTCOLOR(.APPTCOLOR,APPTTYPE)
  . NEW DTIN SET DTIN=+$PIECE(ENTRY,"^",7)                            ;"FMDT for check in
  . NEW DTINSTR SET DTINSTR=$PIECE($$FMTE^XLFDT(DTIN,"2PMZ")," ",2,3)
  . NEW DTOUT1 SET DTOUT1=+$PIECE(ENTRY,"^",8)                        ;"FMDT for check out
  . NEW DTOUTSTR SET DTOUTSTR=$PIECE($$FMTE^XLFDT(DTOUT1,"2PMZ")," ",2,3)
  . NEW APPTDELTA SET APPTDELTA=$PIECE(ENTRY,"^",9)                   ;"'PENDING' if not checked in, 'DONE' if checked OUT, or #minutes before or after appt patient is
  . NEW TIMEHERE SET TIMEHERE=$PIECE(ENTRY,"^",10)                    ;"0 if patient not currently checked in.  Or #minutes checked in.
  . NEW NOTESTATUSSTR SET NOTESTATUSSTR=$SELECT(NOTESTATUS="+":"Unsigned Note",NOTESTATUS="*":"Complete",1:"")
  . NEW NOTESTATUSCLR SET NOTESTATUSCLR=$SELECT(NOTESTATUSSTR["Unsigned":" bgcolor=""#b3f0ff""",1:"")
  . NEW VSTATUS SET VSTATUS=APPTDELTA
  . NEW CHKSTATUS SET CHKSTATUS=""
  . IF +VSTATUS=VSTATUS DO
  . . IF VSTATUS<0 SET VSTATUS=-APPTDELTA_" min BEFORE appt." 
  . . ELSE  SET VSTATUS=APPTDELTA_" min since scheduled time" 
  . NEW COLOR SET COLOR=""
  . IF APPTDELTA="PENDING" DO
  . . SET CHKSTATUS="Pending"                      
  . . SET COLOR="#b3f0ff"
  . ELSE  IF DTIN=0 DO 
  . . SET CHKSTATUS="Not checked in"  
  . ELSE  IF DTOUT1>0 DO
  . . SET CHKSTATUS="Checked out"
  . . SET COLOR="#b3ffb3"
  . ELSE  DO
  . . SET CHKSTATUS="Checked in for "_TIMEHERE_" mins"
  . . IF TIMEHERE<31 DO
  . . . DO GETINIVL^TMGRPC1A(.COLOR,"DEFAULT","Appt Color 0-30","")
  . . ELSE  IF TIMEHERE<60 DO 
  . . . DO GETINIVL^TMGRPC1A(.COLOR,"DEFAULT","Appt Color 31-60","")
  . . ELSE  IF TIMEHERE>59 DO 
  . . . DO GETINIVL^TMGRPC1A(.COLOR,"DEFAULT","Appt Color 60+","")
  . . SET COLOR=$PIECE(COLOR,"^",2)
  . . SET COLOR="#"_$E(COLOR,6,7)_$E(COLOR,4,5)_$E(COLOR,2,3)
  . IF COLOR]"" SET COLOR="bgcolor="""_COLOR_""""
  . SET I=I+1,OUT(I)="  <tr "_COLOR_"> " 
  . SET I=I+1,OUT(I)="    <td>"_APPTDTSTR_"</td>"
  . SET I=I+1,OUT(I)="    <td><a href="""_"DFN-"_ADFN_""" title="""_PTNAME_"""> "_PTABBR_"</a></td>"
  . SET I=I+1,OUT(I)="    <td "_APPTCOLOR_">"_APPTTYPE_"</td>"
  . SET I=I+1,OUT(I)="    <td>"_VSTATUS_"</td>"
  . SET I=I+1,OUT(I)="    <td"_NOTESTATUSCLR_">"_NOTESTATUSSTR_"</td>"
  . SET I=I+1,OUT(I)="    <td>"_CHKSTATUS_"</td>"
  . SET I=I+1,OUT(I)="    <td>"_DTINSTR_"</td>"
  . SET I=I+1,OUT(I)="    <td>"_DTOUTSTR_"</td>"
  . SET I=I+1,OUT(I)="  </tr> "
  SET I=I+1,OUT(I)="</table> "
  SET I=I+1,OUT(I)=""
  SET I=I+1,OUT(I)="</body>"
  SET I=I+1,OUT(I)="</html> "
  QUIT
  ;  
APPTCOLOR(COLOR,APPTTYPE)  ;
  ;"This function will look at the APPTTYPE and determine if the type should have a background color.
  ;"  COLOR is the return variable and will either be returned blank or with the approproate color code
  SET COLOR=""
  IF APPTTYPE="AWV" SET COLOR="#FFBF80"
  IF APPTTYPE="LESION" SET COLOR="#FFBF80"
  IF APPTTYPE="PHYSICAL" SET COLOR="#FFBF80"
  IF APPTTYPE="1 YR CHECK" SET COLOR="#FFBF80"  
  IF COLOR'="" SET COLOR="bgcolor="""_COLOR_""""
  QUIT
  ;"
GETSTATS(TABLE,TABLEIDX,APPTARR)  ;
  ;"Purpose: This will analyze the Appointments sent in APPTARR
  ;"  and return the table HTML code in TABLE
  NEW JDX SET JDX=0   
  NEW STATTABLE
  NEW LASTTIME SET LASTTIME=""
  NEW NOAPPTGRP    ;"THESE ARE THE TYPES TO NOT GROUP AS GENERIC OFFICE VISITS
  SET NOAPPTGRP("PHYSICAL")=""
  SET NOAPPTGRP("AWV")=""
  SET NOAPPTGRP("LESION")=""
  SET NOAPPTGRP("UTI")=""
  SET NOAPPTGRP("PAIN")=""
  SET NOAPPTGRP("ACUTE")=""
  SET NOAPPTGRP("1 YR CHECK")=""
  NEW APPTTYPE 
  NEW TOTALCOUNT SET TOTALCOUNT=0
  FOR  SET JDX=$ORDER(INFO(JDX)) QUIT:JDX'>0  DO
  . NEW ENTRY SET ENTRY=$GET(INFO(JDX)) QUIT:ENTRY=""
  . SET APPTTYPE=$PIECE(ENTRY,"^",6)
  . IF ENTRY["NO APPOINTMENTS FOUND" QUIT
  . IF '$D(NOAPPTGRP(APPTTYPE)) SET APPTTYPE="OFFICE VISIT"
  . IF APPTTYPE="1 YR CHECK" SET APPTTYPE="PHYSICAL"
  . SET STATTABLE(APPTTYPE)=+$G(STATTABLE(APPTTYPE))+1
  . SET TOTALCOUNT=TOTALCOUNT+1
  SET APPTTYPE=""
  SET STATROW=""
  SET TABLEIDX=TABLEIDX+1,TABLE(TABLEIDX)="<table><caption><h3>VISIT STATISTICS</h3></caption><tr>"
  FOR  SET APPTTYPE=$O(STATTABLE(APPTTYPE)) QUIT:APPTTYPE=""  DO
  . SET TABLEIDX=TABLEIDX+1,TABLE(TABLEIDX)="<td>"_APPTTYPE_"</td>"
  . SET STATROW=STATROW_"<td>"_+$G(STATTABLE(APPTTYPE))_"</td>"
  SET TABLEIDX=TABLEIDX+1,TABLE(TABLEIDX)="<td><b>TOTAL</b></td>"
  SET STATROW=STATROW_"<td bgcolor=""#FFFFE0""><b>"_+$G(TOTALCOUNT)_"</b></td>"
  SET TABLEIDX=TABLEIDX+1,TABLE(TABLEIDX)="</tr><tr>"_STATROW_"</tr>"
  SET TABLEIDX=TABLEIDX+1,TABLE(TABLEIDX)="</table>"
  QUIT
  ;"  
TEST2 ;
  NEW TMPRESULT DO NOPATSEL(.TMPRESULT,150)
  ZWR TMPRESULT
  QUIT
  ;    
TEST1 ;
  NEW INFO
  DO GETAPPT(.INFO,83,3240418,3240418)
  IF $DATA(INFO) ZWR INFO
  QUIT 
  ;    
GETAPPT(OUT,PROVIDER,SDT,EDT,OPTION)     ;" RETURN LIST OF APPOINTMENTS
  ;"Input:  OUT -- PASS BY REFERENCE, AN OUT PARAMETER.   Format:  
  ;"              OUT(#)="<error message>"  OR
  ;"              OUT(#)=DFN^PATIENTNAME^STATUS^PROVIDER^APPTDATE^APPTTYPE^DTIN^DTOUT^APPTDELTA^TIMEHERE  
  ;"                         STATUS is '+' or '*'
  ;"                          * --> unsigned note
  ;"                          + --> note for visit signed.    
  ;"                         APPTDELTA -- PENDING if patient has not yet checked in.
  ;"                                      Or DONE if patient checked in and then checked OUT
  ;"                                      Or #minutes before or after appt patient is
  ;"                         TIMEHERE -- 0 if patient not currently checked in.  Or #minutes checked in.  
  ;"         E.g. OUT(7)=12345^HAVARD,HAROLD^83^3240418.093^ (3 MO CHECK)
  ;"        PROVIDER -- DUZ, user IEN.  OPTIONAL.  If not provided, then all schedule for all providers returned.  
  ;"        SDT -- BEGINNING DATE (FM format)  BEST TO PROVIDE DATE ONLY, NO TIME.  
  ;"        EDT -- ENDING DATE (FM format) BEST TO PROVIDE DATE ONLY, NO TIME.   --NOTE: Must be valid datetime.  E.g. 3240418.999999 will FAIL
  ;"        OPTION("SHOW CANCELLED")=1 OPTIONAL.  DEFAULT = 0.  If found, then cancelled appts are returned.  
  ;"        OPTION("SHOW OLD")=1 OPTIONAL.  DEFAULT=1. If found, then old appts are returned.
  ;"        OPTION("EXCLUDE REASONS") OPTIONAL.  DEFAULT='INJ ONLY^PROTIME'  Set to "" to return all.  
  ;"Result: none. 
  ;
  NEW TMGRESULT SET TMGRESULT="1^OK"
  SET PROVIDER=+$GET(PROVIDER)  ;"//kt 4/18/24  
  SET OUT(1)="^NO APPOINTMENTS FOUND"
  NEW ORI,APPTIEN,APPTDT
  SET SDT=+$GET(SDT)
  SET EDT=+$GET(EDT) IF EDT'>0 SET EDT=9999999
  IF $DATA(OPTION("EXCLUDE REASONS"))#10=0 SET OPTION("EXCLUDE REASONS")="INJ ONLY^PROTIME"
  NEW NOW SET NOW=$$NOW^XLFDT
  NEW TODAY SET TODAY=$$TODAY^TMGDATE
  ;"Ensure SDT, EDT in FM Date/Time:
  DO DT^DILF("T",SDT,.SDT,"","")
  DO DT^DILF("T",EDT,.EDT,"","")
  SET SDT=SDT+".0001"
  IF (SDT=-1)!(EDT=-1) SET TMGRESULT="-1^Error in date range." GOTO GADN  
  IF $PIECE(EDT,".",2)'>0 SET EDT=$PIECE(EDT,".",1)_.9999
  ;
  SET ORI=0,APPTDT=SDT-0.000001
  FOR  SET APPTDT=$ORDER(^TMG(22723,"DT",APPTDT)) QUIT:(APPTDT>EDT)!(APPTDT'>0)  DO
  . NEW TMGDFN SET TMGDFN=0
  . FOR  SET TMGDFN=$ORDER(^TMG(22723,"DT",APPTDT,TMGDFN)) QUIT:TMGDFN'>0  DO
  . . NEW APPTIEN SET APPTIEN=$ORDER(^TMG(22723,"DT",APPTDT,TMGDFN,0))
  . . NEW ZN,DOCTOR,STATUS,APPTTYPE
  . . SET STATUS=$GET(^TMG(22723,"DT",APPTDT,TMGDFN,APPTIEN))
  . . IF STATUS="C",$GET(OPTION("SHOW CANCELLED"))'=1 QUIT   ;"C = cancelled
  . . IF STATUS="0",$GET(OPTION("SHOW CANCELLED"),1)'=1 QUIT   ;"C = OLD
  . . SET ZN=$GET(^TMG(22723,TMGDFN,1,APPTIEN,0))
  . . SET DOCTOR=$PIECE(ZN,"^",3)
  . . IF PROVIDER>0,DOCTOR'=PROVIDER QUIT
  . . SET APPTTYPE=$PIECE(ZN,"^",4)
  . . IF OPTION("EXCLUDE REASONS")[$$UP^XLFSTR(APPTTYPE) QUIT
  . . NEW PATNAME SET PATNAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
  . . NEW DTCHECKIN,DTCHECKOUT,APPTDELTA,TIMEHERE
  . . SET DTCHECKIN=+$PIECE(ZN,"^",8)
  . . SET DTCHECKOUT=+$PIECE(ZN,"^",9)
  . . IF DTCHECKIN>0 DO  ;"Patient has checked in already. 
  . . . IF DTCHECKOUT>0 DO  ;"And patient has already checked out again.  
  . . . . SET APPTDELTA="DONE"
  . . . . SET TIMEHERE=0
  . . . ELSE  DO  ;"Patient still in office.  
  . . . . SET APPTDELTA=$$GETMINS(NOW,APPTDT)  ;"If < 0, then patient checked in BEFORE appt, if > 0 then minutes SINCE APPT TIME. 
  . . . . SET TIMEHERE=$$GETMINS(NOW,DTCHECKIN)
  . . ELSE  DO
  . . . SET APPTDELTA="PENDING"
  . . . SET TIMEHERE=0  
  . . SET ORI=ORI+1
  . . NEW STATUS SET STATUS=$SELECT(SDT<TODAY:"",1:$$TRIM^XLFSTR($$PTSTATUS(TMGDFN,APPTDT)))
  . . SET OUT(ORI)=TMGDFN_"^"_PATNAME_"^"_STATUS_"^"_PROVIDER_"^"_APPTDT_"^"_APPTTYPE_"^"_DTCHECKIN_"^"_DTCHECKOUT_"^"_APPTDELTA_"^"_TIMEHERE
GADN ;  
  IF $$SHOULDGARBLE^TMGMISC4() DO GARBLERESULTS^TMGMISC4(.OUT)     ;"check for special mode to hide patient info during demos
  QUIT TMGRESULT  
  ;
GETMINS(DT1,DT2)  ;
  NEW RESULT SET RESULT=$$FMDIFF^XLFDT(DT1,DT2,2)
  SET RESULT=RESULT\60  ;"convert sec to min
  QUIT RESULT 