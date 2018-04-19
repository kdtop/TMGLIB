TMGPAT5  ;TMG/kst/Patching tools ;10/19/08
         ;;1.0;TMG-LIB;**1**;10/19/08
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
 ;" API -- Public Functions.
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"Private Functions
 ;"=======================================================================
 ; 
 ;"=======================================================================
 ;
SHOWTG ;"SHOW ENTRIES LOADED IN TRANSPORT GLOBAL
  NEW OUT
  WRITE !,!,"Distributions currently loaded into Transport Global",!
  WRITE "----------------------------------------------------",!
  IF $$LSTTG(.OUT)>0 DO
  . NEW NAME SET NAME=""
  . FOR  SET NAME=$ORDER(OUT(NAME)) QUIT:NAME=""  DO
  . . NEW IEN SET IEN=0
  . . FOR  SET IEN=$ORDER(OUT(NAME,IEN)) QUIT:+IEN'>0  DO
  . . . WRITE "#",IEN," ",NAME,!
  ELSE  DO
  . WRITE "(NONE)",!
  WRITE !
  DO PRESS2GO^TMGUSRI2
  QUIT
  ;
LSTTG(OUT) ;"LIST TRANSPORT GLOBALS LOADED
  ;"Input: -- OUT -- PASS BY REFERENCE.  AN OUT PARAMETER.  Format:
  ;
  ;"Result: Count of number found
  NEW CT SET CT=0
  NEW IEN9D7 SET IEN9D7=0
  FOR  SET IEN9D7=$ORDER(^XTMP("XPDI",IEN9D7)) QUIT:+IEN9D7'>0  DO
  . NEW NAME SET NAME=$PIECE($GET(^XPD(9.7,IEN9D7,0)),"^",1)
  . SET OUT(NAME,IEN9D7)="",CT=CT+1
  QUIT CT
 ;
FIXPATCH ;
  ;"Purpose: This fixes situation when a patch is applied without first
  ;"  changing the $ZRO
  ;"  This will chagned the routines into the /r folder (backing up prior
  ;"    files if they exist)
  NEW PDIR SET PDIR="/opt/worldvista/EHR/p/"
  NEW RDIR SET RDIR="/opt/worldvista/EHR/r/"
  NEW INIT
  FOR I=1:1:26 DO 
  . NEW INIT SET INIT=$CHAR(64+I)
  . NEW TMGFILTER SET TMGFILTER(INIT_"*")=""
  . NEW PLIST,RLIST
  . IF $$LIST^%ZISH(PDIR,"TMGFILTER","PLIST")
  . ;"IF $$LIST^%ZISH(RDIR,"TMGFILTER","RLIST")
  . NEW AFILE SET AFILE=""
  . FOR  SET AFILE=$ORDER(PLIST(AFILE)) QUIT:AFILE=""  DO
  . . NEW TYPE SET TYPE=$PIECE(AFILE,".",2)
  . . IF TYPE'="m" QUIT
  . . NEW NAMESPACE SET NAMESPACE=$EXTRACT(AFILE,1,3)
  . . IF NAMESPACE="TMG" QUIT
  . . IF $EXTRACT(AFILE,1,1)="Z" QUIT
  . . WRITE AFILE
  . . NEW PFILE SET PFILE=PDIR_AFILE
  . . NEW RFILE SET RFILE=RDIR_AFILE
  . . IF $$ISFILE^TMGKERNL(RFILE) DO
  . . . WRITE " <-- overlap",!
  . . . NEW BACKFNAME
  . . . DO MAKEBAKF^TMGKERNL(RFILE,,.BACKFNAME) 
  . . . IF BACKFNAME="" QUIT
  . . . WRITE "  Backed up ",RFILE," --> ",BACKFNAME,!
  . . . ;"NEW % SET %=2
  . . . ;"WRITE "  Delete ",RFILE DO YN^DICN WRITE !
  . . . ;"IF %'=1 QUIT
  . . . NEW TMGDEL SET TMGDEL(AFILE)=""
  . . . NEW TEMP SET TEMP=$$DEL^%ZISH(RDIR,"TMGDEL")
  . . . IF TEMP=0 WRITE "  Delete failed...",! QUIT
  . . . WRITE "  MOVING ",PFILE," --> ",RFILE,!
  . . . IF $$MOVE^TMGKERNL(PFILE,RFILE)
  . . ELSE  DO
  . . . WRITE !
  . . . NEW % SET %=2
  . . . WRITE "MOVING ",PFILE," --> ",RFILE,!
  . . . ;"DO YN^DICN WRITE !
  . . . ;"IF %'=1 QUIT
  . . . IF $$MOVE^TMGKERNL(PFILE,RFILE)
  QUIT  
  ;
