TMGRAD01 ;TMG/kst Radiology Related Fns;8/24/2026
         ;;1.0;TMG-LIB;**1**;8/24/2026
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
 ;"---------------------------------------------------------------------------
 ;"PUBLIC FUNCTIONS
 ;"---------------------------------------------------------------------------
 ;
 ;"---------------------------------------------------------------------------
 ;"PRIVATE FUNCTIONS
 ;"---------------------------------------------------------------------------
 ;
 ;"---------------------------------------------------------------------------
 ;
RADEXAMS(TMGRESULT,TMGDFN)  ;"Called with RPC: TMG TIU RAD RXAMS
 ;"Purpose: This function returns a list of all radiology testing from both
 ;"         the radiology package as well as from TIU Notes 
 ;"Output: TMGRESULT(#)="IEN8925^TITLE^FMDT", WITH PIECE 20="TIU"
 ;"        TMGRESULT(#)="RAD^SITEID^EXAM ID^FMDT^PROCEDURE NAME^CASE NUMBER^STATUS^SEVERITY^?^EXAM STATUS^IMAGING LOCATION^TYPE^?^?"
 ;"                     WITH PIECE 20="RAD"
 NEW RADARR,TOTALARR
 ;"
 ;"Get all exams from the radiology package, storing in TOTALARR(FMDATE) 
 DO EXAMS1^ORWRA(.RADARR,TMGDFN)
 NEW RADIDX SET RADIDX=0
 FOR  SET RADIDX=$ORDER(@RADARR@(RADIDX)) QUIT:RADIDX'>0  DO
 . NEW LINE SET LINE=@RADARR@(RADIDX)
 . NEW THISDATE SET THISDATE=$P(LINE,"^",3)
 . SET $P(LINE,"^",20)="RAD"
 . DO ADDSTUDY(.TOTALARR,THISDATE,LINE)
 . ;"SET TOTALARR(THISDATE)="RAD^"_LINE
 ;"
 ;"Get TIU types that are set as Radiology TIU Notes
 NEW TIUARR,ONETIU SET ONETIU=0
 FOR  SET ONETIU=$O(^TMG(22736,"B",ONETIU)) QUIT:ONETIU'>0  DO
 . NEW TIUTITLE SET TIUTITLE=$P($G(^TIU(8925.1,ONETIU,0)),"^",1)
 . ;"Get all the patient's notes for this title
 . NEW TIUIEN SET TIUIEN=0
 . FOR  SET TIUIEN=$O(^TIU(8925,"C",TMGDFN,TIUIEN)) QUIT:TIUIEN'>0  DO
 . . NEW THISTITLE,REFDATE
 . . SET THISTITLE=$P($G(^TIU(8925,TIUIEN,0)),"^",1)
 . . IF THISTITLE'=ONETIU QUIT
 . . SET REFDATE=$P($G(^TIU(8925,TIUIEN,13)),"^",1)
 . . NEW LINE SET LINE=TIUIEN_"^"_TIUTITLE_"^"_REFDATE
 . . SET $P(LINE,"^",20)="TIU"
 . . DO ADDSTUDY(.TOTALARR,REFDATE,LINE)
 ;"
 ;"Now search through TOTALARR and compile it into TMGRESULT
 NEW OUTIDX SET OUTIDX=0
 NEW ONEDT SET ONEDT=9999999
 FOR  SET ONEDT=$O(TOTALARR(ONEDT),-1) QUIT:ONEDT=""  DO
 . SET TMGRESULT($I(OUTIDX))=$G(TOTALARR(ONEDT))
 ;"
 ;ZWR TOTALARR
 ;W !
 ;ZWR TMGRESULT
 QUIT
 ;"
ADDSTUDY(ARR,STUDYFMDATE,LINE)  ;"
 ;"Purpose: Add a new entry into ARR, which is date indexed. Check the
 ;"         provided date and if exists, add a second until it is a 
 ;"         unique entry.
 NEW FMDATE SET FMDATE=STUDYFMDATE-0.000001
 NEW DAY SET DAY=$P(STUDYFMDATE,".",1) ;"SAFE GUARD SO THIS DOESN'T RUN FOREVER. STOPS IF THE NEXT CALENDAR DAY WAS REACHED
 NEW ENTERED SET ENTERED=0
 FOR  SET FMDATE=FMDATE+0.000001 QUIT:(ENTERED=1)!(FMDATE'[DAY)  DO
 . IF '$D(ARR(FMDATE)) DO
 . . SET ARR(FMDATE)=LINE
 . . SET ENTERED=1
 QUIT