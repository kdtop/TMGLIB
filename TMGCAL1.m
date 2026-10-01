TMGCAL1 ;TMG/kst-Appointment Related Fns;11/08/08, 6/3/24
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
 ;"BUILDCA(CALOBJ,IDX) 
 ;"
 ;======================================================================
 ; CSS
 ;======================================================================
CALCSS(CALOBJ,IDX) 
 ;S CALOBJ($I(IDX))="<style type='text/css'>"
 S CALOBJ($I(IDX))=".cal-container{"
 S CALOBJ($I(IDX))="display:table;"
 S CALOBJ($I(IDX))="width:100%;"
 S CALOBJ($I(IDX))="table-layout:fixed;"
 S CALOBJ($I(IDX))="}"
 S CALOBJ($I(IDX))=".cal-container-cell{"
 S CALOBJ($I(IDX))="display:table-cell;"
 S CALOBJ($I(IDX))="vertical-align:top;"
 S CALOBJ($I(IDX))="table-layout:fixed;"
 ;"S CALOBJ($I(IDX))="padding-right:25px;"
 S CALOBJ($I(IDX))="}"
 S CALOBJ($I(IDX))=".cal-box{"
 S CALOBJ($I(IDX))="width:250px;"
 S CALOBJ($I(IDX))="margin:0 auto;"
 S CALOBJ($I(IDX))="}"
 S CALOBJ($I(IDX))=".cal-title{"
 S CALOBJ($I(IDX))="text-align:center;"
 S CALOBJ($I(IDX))="font-weight:bold;"
 S CALOBJ($I(IDX))="font-size:13px;"
 S CALOBJ($I(IDX))="padding:4px;"
 S CALOBJ($I(IDX))="border:1px solid #999;"
 S CALOBJ($I(IDX))="border-bottom:0;"
 S CALOBJ($I(IDX))="}"
 S CALOBJ($I(IDX))=".cal-table{"
 S CALOBJ($I(IDX))="width:100%;"
 S CALOBJ($I(IDX))="table-layout:fixed;"
 S CALOBJ($I(IDX))="border-collapse:collapse;"
 S CALOBJ($I(IDX))="}"
 S CALOBJ($I(IDX))=".cal-day{"
 S CALOBJ($I(IDX))="height:43px;"
 S CALOBJ($I(IDX))="border:1px solid #999;"
 S CALOBJ($I(IDX))="text-align:center;"
 S CALOBJ($I(IDX))="vertical-align:top;"
 S CALOBJ($I(IDX))="padding:2px;"
 S CALOBJ($I(IDX))="width:20%;"
 S CALOBJ($I(IDX))="}"
 S CALOBJ($I(IDX))=".cal-date{"
 S CALOBJ($I(IDX))="font-size:10px;"
 S CALOBJ($I(IDX))="line-height:12px;"
 S CALOBJ($I(IDX))="}"
 S CALOBJ($I(IDX))=".cal-count{"
 S CALOBJ($I(IDX))="font-size:16px;"
 S CALOBJ($I(IDX))="font-weight:bold;"
 S CALOBJ($I(IDX))="line-height:20px;"
 S CALOBJ($I(IDX))="}"
 S CALOBJ($I(IDX))=".cal-today{"
 S CALOBJ($I(IDX))="border:2px solid #000;"
 S CALOBJ($I(IDX))="}"
 S CALOBJ($I(IDX))=".cal-main-title{"
 S CALOBJ($I(IDX))="font-weight:bold;"
 S CALOBJ($I(IDX))="font-size:16px;"
 S CALOBJ($I(IDX))="text-align:center;"
 S CALOBJ($I(IDX))="padding:4px 0 6px 0;"
 S CALOBJ($I(IDX))="}"
 S CALOBJ($I(IDX))=".cal-legend{"
 S CALOBJ($I(IDX))="text-align:center;"
 S CALOBJ($I(IDX))="font-size:11px;"
 S CALOBJ($I(IDX))="padding-top:6px;"
 S CALOBJ($I(IDX))="}"
 S CALOBJ($I(IDX))=".cal-legend-item{"
 S CALOBJ($I(IDX))="display:inline-block;"
 S CALOBJ($I(IDX))="margin-right:12px;"
 S CALOBJ($I(IDX))="}"
 S CALOBJ($I(IDX))=".cal-legend-color{"
 S CALOBJ($I(IDX))="display:inline-block;"
 S CALOBJ($I(IDX))="width:14px;"
 S CALOBJ($I(IDX))="height:14px;"
 S CALOBJ($I(IDX))="border:1px solid #999;"
 S CALOBJ($I(IDX))="vertical-align:middle;"
 S CALOBJ($I(IDX))="margin-right:3px;"
 S CALOBJ($I(IDX))="}"
 ;S CALOBJ($I(IDX))="</style>" 
 QUIT
 
BUILDCA(CALOBJ,IDX)
 ;======================================================================
 ; BUILD THREE COMPACT PATIENT SCHEDULING CALENDARS
 ;
 ; CALL:
 ;
 ;   K CALOBJ
 ;   S IDX=0
 ;   D BUILDCA^YOURROUTINE(.CALOBJ,.IDX)
 ;
 ; Every HTML fragment is stored separately:
 ;
 ;   S CALOBJ($I(IDX))="..."
 ;
 ; Calendar #1:
 ;   Current week + following 3 weeks
 ;
 ; Calendar #2:
 ;   Week before TODAY+1 month
 ;   Week containing TODAY+1 month
 ;   Following 2 weeks
 ;
 ; Calendar #3:
 ;   Week before TODAY+3 months
 ;   Week containing TODAY+3 months
 ;   Following 2 weeks
 ;
 ; Monday-Friday only.
 ;======================================================================

 N TODAY,TARGET,START

 S TODAY=$$DT^XLFDT


 ;======================================================================
 ; OUTER TABLE
 ;
 ; Using a real HTML table here rather than flexbox/inline-block.
 ; This should be much more reliable in the CPRS embedded browser.
 ;======================================================================
 S CALOBJ($I(IDX))="<div class='cal-main-title'>UPCOMING APPOINTMENT CALENDARS (ALL PROVIDERS)</div>"
 S CALOBJ($I(IDX))="<table class='cal-container' cellpadding='0' cellspacing='0' border='0'>"
 S CALOBJ($I(IDX))="<tr>"
 ;======================================================================
 ; CALENDAR #1
 ;======================================================================
 S CALOBJ($I(IDX))="<td class='cal-container-cell' valign='top'>"
 S START=$$GETMON(TODAY)
 D BUILDONE(.CALOBJ,.IDX,START,"CURRENT",TODAY)
 S CALOBJ($I(IDX))="</td>"
 ;======================================================================
 ; CALENDAR #2
 ;======================================================================
 S CALOBJ($I(IDX))="<td class='cal-container-cell' valign='top'>"
 S TARGET=$$ADDMONTH(TODAY,1)
 ; Move to Monday of target week, then back one week
 S START=$$FMADD^XLFDT($$GETMON(TARGET),-7)
 D BUILDONE(.CALOBJ,.IDX,START,"+1 MONTH",TODAY)
 S CALOBJ($I(IDX))="</td>"
 ;======================================================================
 ; CALENDAR #3
 ; +2 MONTHS
 ;======================================================================
 S CALOBJ($I(IDX))="<td class='cal-container-cell' valign='top'>"
 S TARGET=$$ADDMONTH(TODAY,2)
 S START=$$FMADD^XLFDT($$GETMON(TARGET),-7)
 D BUILDONE(.CALOBJ,.IDX,START,"+2 MONTHS",TODAY)
 S CALOBJ($I(IDX))="</td>"
 ;======================================================================
 ; CALENDAR #4
 ;======================================================================
 S CALOBJ($I(IDX))="<td class='cal-container-cell' valign='top'>"
 S TARGET=$$ADDMONTH(TODAY,3)
 ; Move to Monday of target week, then back one week
 S START=$$FMADD^XLFDT($$GETMON(TARGET),-7)
 D BUILDONE(.CALOBJ,.IDX,START,"+3 MONTHS",TODAY)
 S CALOBJ($I(IDX))="</td>"
 ;======================================================================
 ; END OUTER TABLE
 ;======================================================================
 S CALOBJ($I(IDX))="</tr>"
 S CALOBJ($I(IDX))="</table>"
 Q
 ;"
 ;======================================================================
 ; BUILD ONE CALENDAR
 ;======================================================================
BUILDONE(CALOBJ,IDX,START,TITLE,TODAY)
 N WEEK,DAY,DT,CNT,COLOR,CLASS
 ;----------------------------------------------------------------------
 ; Calendar title
 ;----------------------------------------------------------------------
 S CALOBJ($I(IDX))="<div class='cal-box'>"
 S CALOBJ($I(IDX))="<div class='cal-title'>"_TITLE_"</div>"
 ;----------------------------------------------------------------------
 ; Calendar table
 ;----------------------------------------------------------------------
 S CALOBJ($I(IDX))="<table class='cal-table' cellpadding='0' cellspacing='0'>"
 ;----------------------------------------------------------------------
 ; Four weeks
 ;----------------------------------------------------------------------
 F WEEK=0:1:3 D
 . S CALOBJ($I(IDX))="<tr>"
 . F DAY=0:1:4 D
 . . ;---------------------------------------------------------------
 . . ; IMPORTANT:
 . . ; Use FMADD^XLFDT for date arithmetic.
 . . ; Do NOT use START+(WEEK*7)+DAY because FileMan dates
 . . ; aren't normal sequential integers across months.
 . . ;---------------------------------------------------------------
 . . S DT=$$FMADD^XLFDT(START,(WEEK*7)+DAY)
 . . S CNT=$$PATIENTS(DT)
 . . S COLOR=$$DAYCOLOR(CNT,DT)
 . . S CLASS="cal-day"
 . . I DT=TODAY S CLASS="cal-day cal-today"
 . . ;---------------------------------------------------------------
 . . ; Notice the closing single quote after COLOR.
 . . ;---------------------------------------------------------------
 . . S CALOBJ($I(IDX))="<td class='"_CLASS_"' style='background-color:"_COLOR_"'>"
 . . S CALOBJ($I(IDX))="<div class='cal-date'>"_$$DATEFMT(DT)_"</div>"
 . . S CALOBJ($I(IDX))="<div class='cal-count'>"_CNT_"</div>"
 . . S CALOBJ($I(IDX))="</td>"
 . S CALOBJ($I(IDX))="</tr>"
 S CALOBJ($I(IDX))="</table>"
 S CALOBJ($I(IDX))="<div class='cal-legend'>"
 S CALOBJ($I(IDX))="<span class='cal-legend-item'>"
 S CALOBJ($I(IDX))="<span class='cal-legend-color' style='background-color:#F4CCCC;'></span>"
 S CALOBJ($I(IDX))="Under 10"
 S CALOBJ($I(IDX))="</span>"
 S CALOBJ($I(IDX))="<span class='cal-legend-item'>"
 S CALOBJ($I(IDX))="<span class='cal-legend-color' style='background-color:#FFF2CC;'></span>"
 S CALOBJ($I(IDX))="10-15"
 S CALOBJ($I(IDX))="</span>"
 S CALOBJ($I(IDX))="<span class='cal-legend-item'>"
 S CALOBJ($I(IDX))="<span class='cal-legend-color' style='background-color:#DFF0D8;'></span>"
 S CALOBJ($I(IDX))="16+"
 S CALOBJ($I(IDX))="</span>"
 S CALOBJ($I(IDX))="<span class='cal-legend-item'>"
 S CALOBJ($I(IDX))="<span class='cal-legend-color' style='background-color:#E7E7E7;'></span>"
 S CALOBJ($I(IDX))="Wednesday"
 S CALOBJ($I(IDX))="</span>"
 S CALOBJ($I(IDX))="</div>"
 S CALOBJ($I(IDX))="</div>"
 Q
 ;"
 ;======================================================================
 ; GETMON
 ;
 ; Return the Monday of the week containing DT.
 ;
 ; IMPORTANT:
 ; XLFDT DOW uses:
 ;
 ;   0 = Sunday
 ;   1 = Monday
 ;   2 = Tuesday
 ;   3 = Wednesday
 ;   4 = Thursday
 ;   5 = Friday
 ;   6 = Saturday
 ;======================================================================
GETMON(DT)
 N DOW
 S DOW=$$DOW^XLFDT(DT,1)
 ; 1 = Sunday
 ; 2 = Monday
 ; 3 = Tuesday
 ; 4 = Wednesday
 ; 5 = Thursday
 ; 6 = Friday
 ; 7 = Saturday
 ; Sunday: go back 6 days to Monday
 I DOW=0 Q $$FMADD^XLFDT(DT,-6)  ;"was DOW=1
 ; Monday-Friday/Saturday:
 ; Monday = 0 days back
 ; Tuesday = 1 day back
 ; ...
 Q $$FMADD^XLFDT(DT,-(DOW-1))
 ;"
 ;======================================================================
 ; ADD MONTH
 ;
 ; Add N calendar months while preserving the day when possible.
 ;
 ; Example:
 ;
 ;   9/4/26 + 1 month = 10/4/26
 ;
 ;   1/31/26 + 1 month = 2/28/26
 ;======================================================================
ADDMONTH(DT,N)
 N Y,M,D,TOTAL,NEWY,NEWM,LAST
 S Y=+$E(DT,1,3)
 S M=+$E(DT,4,5)
 S D=+$E(DT,6,7)
 S TOTAL=(M-1)+N
 S NEWY=Y+(TOTAL\12)
 S NEWM=(TOTAL#12)+1
 S LAST=$$LASTDAY(NEWY,NEWM)
 I D>LAST S D=LAST
 Q NEWY_$TR($J(NEWM,2)," ","0")_$TR($J(D,2)," ","0")
 ;"
 ;======================================================================
 ; LAST DAY OF MONTH
 ;======================================================================
LASTDAY(Y,M)
 N NEXT,LASTDATE
 I M=12 D
 . S NEXT=(Y+1)_"0101"
 E  D
 . S NEXT=Y_$TR($J(M+1,2)," ","0")_"01"
 S LASTDATE=$$FMADD^XLFDT(NEXT,-1)
 Q +$E(LASTDATE,6,7)
 ;"
 ;======================================================================
 ; FORMAT DATE
 ;
 ; FileMan date -> M/D/YY
 ;======================================================================
DATEFMT(DT)
 N M,D,Y
 S M=+$E(DT,4,5)
 S D=+$E(DT,6,7)
 S Y=$E(DT,2,3)
 Q M_"/"_D_"/"_Y
 ;"
 ;======================================================================
 ; PATIENT COUNT
 ;======================================================================
PATIENTS(DT)
 NEW TMGRESULT SET TMGRESULT=0
 NEW APPTDT SET APPTDT=DT-0.000001
 FOR  SET APPTDT=$ORDER(^TMG(22723,"DT",APPTDT)) QUIT:(APPTDT'[DT)!(APPTDT'>0)  DO
 . NEW TMGDFN SET TMGDFN=0
 . FOR  SET TMGDFN=$ORDER(^TMG(22723,"DT",APPTDT,TMGDFN)) QUIT:TMGDFN'>0  DO
 . . NEW APPTIEN SET APPTIEN=$ORDER(^TMG(22723,"DT",APPTDT,TMGDFN,0))
 . . NEW ZN,DOCTOR,STATUS,APPTTYPE
 . . SET STATUS=$GET(^TMG(22723,"DT",APPTDT,TMGDFN,APPTIEN))
 . . IF STATUS="C" QUIT   ;"C = cancelled
 . . IF STATUS="O" QUIT   ;"C = OLD
 . . SET TMGRESULT=TMGRESULT+1
 . . ;"SET ZN=$GET(^TMG(22723,TMGDFN,1,APPTIEN,0))
 . . ;"SET DOCTOR=$PIECE(ZN,"^",3)
 . . ;"IF PROVIDER>0,DOCTOR'=PROVIDER QUIT
 Q TMGRESULT
 ;"
 ;======================================================================
 ; DAY COLOR
 ;======================================================================
DAYCOLOR(CNT,DT)
 N DOW SET DOW=$$DOW^XLFDT(DT,1)
 IF DOW=3 Q "#E7E7E7"
 IF CNT<10 QUIT "#F4CCCC"
 IF CNT<15 QUIT "#FFF2CC"
 Q "#DFF0D8"
 ;"