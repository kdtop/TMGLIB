TMGCHNG1 ;TMG/kst-Change Log Functions ; 10/15/24
         ;;1.0;TMG-LIB;**1**;10/15/24
 ;
 ;"TMG CHANGE LOG FUNCTIONS related to CPRS
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 10/10/2024  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
GTCHNGLG(TMGRESULT,TMGDUZ,CPRSVERSION) ;"
 ;"Purpose: This function will return the change log for CPRS.
 ;"Input: TMGDUZ - Current user
 ;"       CPRSVERSION -  will with be the current version used, or "ALL"
 ;"         If current version, it will only return the current version's
 ;"         changes (if the user hasn't seen them yet). If ALL, it will 
 ;"         return all the changes
 SET TMGRESULT(0)="-1^NONE"
 NEW RESULTARR
 SET TMGRESULT(1)="<html><body><table border=1>"
 NEW OUTIDX SET OUTIDX=2
 NEW VERSION,DATE,DESC,FOUND 
 SET VERSION="ZZZZ",FOUND=0
 FOR  SET VERSION=$O(^TMG(22760,"B",VERSION),-1) QUIT:VERSION=""  DO
 . ;"IF (VERSION'=CPRSVERSION)&(CPRSVERSION'="ALL") QUIT
 . ;"IF CPRSVERSION'="ALL" QUIT  ;"only do all for now
 . ;"TO FINISH: if getting version changes, check to see if user has already seen. If so quit. If not return the changes and add to the seen field.
 . NEW VERSIDX SET VERSIDX=$O(^TMG(22760,"B",VERSION,0))
 . IF (CPRSVERSION="ALL")!($$SEEN(TMGDUZ,VERSIDX)=0) DO
 . . NEW SUBIDX SET SUBIDX=0
 . . FOR  SET SUBIDX=$O(^TMG(22760,VERSIDX,1,SUBIDX)) QUIT:SUBIDX'>0  DO
 . . . SET FOUND=1
 . . . SET DATE=$P($G(^TMG(22760,VERSIDX,1,SUBIDX,0)),"^",1)
 . . . SET DESC=$P($G(^TMG(22760,VERSIDX,1,SUBIDX,0)),"^",2)
 . . . ;"SET RESULTARR(CPRSVERSION,SUBIDX,DATE)=DESC
 . . . SET TMGRESULT(OUTIDX)="<tr><td>"_VERSION_"</td><td>"_$$EXTDATE^TMGDATE(DATE,1)_"</td><td>"_DESC_"</td></tr>",OUTIDX=OUTIDX+1
 IF FOUND=1 DO
 . SET TMGRESULT(0)="1^CHANGES RETURNED"
 . SET TMGRESULT(OUTIDX)="</table></body></html>"
 ;"SET TMGRESULT(1)="<HTML><BODY>COMING SOON</BODY></HTML>"
GCLDN 
 QUIT
 ;"
SEEN(TMGDUZ,VERSIDX)  ;"HAS THE USER SEEN THIS? IF NOT RETURN 0 (AND SET TO SHOWN), IF SO RETURN 1
 NEW TMGRESULT SET TMGRESULT=0
 NEW ONEDUZ SET ONEDUZ=0
 FOR  SET ONEDUZ=$O(^TMG(22760,VERSIDX,2,"B",ONEDUZ)) QUIT:ONEDUZ'>0  DO
 . IF ONEDUZ=TMGDUZ SET TMGRESULT=1
 IF TMGRESULT=0 DO
 . ;"HERE SET THE USER AS SEEN
 . KILL TMGFDA,TMGIEN,TMGMSG
 . NEW IENS SET IENS="+1,"_VERSIDX_","
 . SET TMGFDA(22760.02,IENS,.01)=TMGDUZ
 . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
 . IF $DATA(TMGMSG) DO  QUIT TMGRESULT
 . . ;"IGNORE SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
 QUIT TMGRESULT