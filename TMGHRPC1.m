TMGHRPC1 ;TMG/elh/Support Functions for GUI_Config ;2/2/14, 5/7/18, 3/24/21  
         ;;1.0;TMG-LIB;**1**;10/20/09                              
 ;
 ;"TMG Function to change Access/Verify Code
 ;"When either an IEN, Access Code, or Name is Supplied
 ;"The IEN or name can be passed directly, the Access Code should be preceeded
 ;"by an underscore.
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
 ;" RPC -- Public Functions.
 ;"=======================================================================
 ;" <none>
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"CHANGEAVCODE(TMGOUT,TMGDATA) -- Post Changes to database via Fileman
 ;
 ;"=======================================================================
 ;"Dependencies:
 ;"  TMGRPC3* only
 ;
 ;"=======================================================================
 ;
CHANGEAVCODE(TMGUSER,TMGNEWAC,TMGNEWVC)
   ;"Post NEW access and verify codes to database via Fileman
   ;"Input: TMGUSER -- User name, IEN, or Access Code (preceeded by an underscore)
   ;"       TMGNEWAC -- New Access Code, Sent By User
   ;"       TMGNEWVC -- New Verify Code, Sent By User
   ;"Results: none
   ;
   NEW TMGAVC SET TMGAVC=0  ;"TMGAVC=AccessVerifyCode. Default to no change.
   NEW TMGINACTUSER,TMGREACTUSER
   NEW TMGI SET TMGI=""
   NEW TMGFDA,TMGNEWFDA,TMGMSG,TMGIEN,DIC
   ;
   SET TMGUSER=$TRANSLATE($GET(TMGUSER),"""","")
   IF +$GET(TMGUSER)>0 SET Y=+TMGUSER GOTO L2
   SET TMGUSER=$$UP^XLFSTR(TMGUSER)
   IF $EXTRACT(TMGUSER,1)="_" DO  GOTO L1
   . SET TMGUSER=$EXTRACT(TMGUSER,2,999)
   . SET TMGUSER=$$EN^XUSHSH(TMGUSER)   ;Hash Access Code
   . SET Y=$ORDER(^VA(200,"A",TMGUSER,""))
   ;
   IF TMGUSER="" SET Y=0
   ELSE  SET Y=+$ORDER(^VA(200,"B",TMGUSER,""))
   ;
L1 ;
   IF Y'>0 WRITE "CAN'T FIND THAT USER ("_TMGUSER_"-"_Y_")" GOTO L3
L2 ;
   ;"SET TMGOLDAC=$$EN^XUSHSH(TMGOLDAC)   ;Hash Access Code
   SET TMGNEWAC=$GET(TMGNEWAC) IF TMGNEWAC="" GOTO L3
   SET TMGNEWAC=$$UP^XLFSTR(TMGNEWAC)   ;Access Code Must Be Uppercase
   SET TMGNEWAC=$$EN^XUSHSH(TMGNEWAC)   ;Hash Access Code
   ;
   SET TMGNEWVC=$GET(TMGNEWVC) IF TMGNEWVC="" GOTO L3
   SET TMGNEWVC=$$UP^XLFSTR(TMGNEWVC) ;"verify code must be upper case   elh
   SET TMGNEWVC=$$EN^XUSHSH(TMGNEWVC)  ;"verify code is supposed to be hashed first
   SET TMGOLDAC=$PIECE(^VA(200,Y,0),"^",3)
   SET $PIECE(^VA(200,Y,0),"^",3)=TMGNEWAC
   IF TMGOLDAC'="" KILL ^VA(200,"A",TMGOLDAC)
   SET ^VA(200,"A",TMGNEWAC,Y)=+$H
   SET $PIECE(^VA(200,Y,.1),"^",2)=TMGNEWVC
   ;
L3 ;
   QUIT
   ;
TIUADDEN(TMGRESULT,TIUIEN)   ; 
   ; This RPC Returns all addendums that belong to given note IEN
   NEW IEN SET IEN=""
   KILL TMGRESULT
   NEW COUNT SET COUNT=0
   SET TIUIEN=+TIUIEN
   FOR  SET IEN=$ORDER(^TIU(8925,"DAD",TIUIEN,IEN)) QUIT:IEN'>0  DO
   . SET COUNT=COUNT+1
   . SET TMGRESULT(COUNT)=IEN
   IF COUNT>0 SET TMGRESULT(0)="1^ADDENDUM FOUND"
   ELSE  SET TMGRESULT(0)="-1^NO ADDENDUM FOUND"
   ;
   QUIT 
   ;
GETUNAME(DUZ)   ;
   ; This RPC returns the name (Last, First) for the DUZ provided
   NEW TMGRESULT
   SET TMGRESULT=$PIECE($GET(^VA(200,DUZ,0)),"^",1)
   QUIT TMGRESULT
   ;
TESTPI   ;
   ;"KILL THIS FUNCTION
   NEW IDX,IDX2,TEMPARRAY
   SET IDX=0,IDX2=0
   FOR  SET IDX=$ORDER(^PXRMINDX(63,"PI",IDX)) QUIT:IDX'>0  DO
   . FOR  SET IDX2=$ORDER(^PXRMINDX(63,"PI",IDX,IDX2)) QUIT:IDX2'>0  DO
   . . NEW DATE
   . . SET DATE=$ORDER(^PXRMINDX(63,"PI",IDX,IDX2,0))
   . . IF DATE>3131201 SET TEMPARRAY(DATE)=""  ;WRITE "FOUND CURRENT DATE =",DATE,!
   IF $DATA(TEMPARRAY) DO ZWRITE^TMGZWR("TEMPARRAY")
   QUIT
   ;
PAINMEDS(TMGDFN,TEST,DATE,DATA,TEXT) ;
   ;"Purpose: Determine IF patient is on pain medication
   ;"Input: TMGDFN -- the patient IEN
   ;"       TEST -- AN OUT PARAMETER.  The logical value of the test:
   ;"          1=true, 0=false
   ;"               Also an IN PARAMETER.  Any value for COMPUTED
   ;"FINDING PARAMETER will be passed in here.
   ;"       DATE -- AN OUT PARAMETER.  Date of finding.
   ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.
   ;"       TEXT -- Text to be display in the Clinical Maintenance
   ;"Output.  Optional.
   ;"Results: none
   DO PAINMEDS^TMGPXR01(.TMGDFN,.TEST,.DATE,.DATA,.TEXT) ;"//kt 5/7/18
   ;"//kt 5/7/18 REMOVED BELOW, REPLACED WITH ABOVE
   ;"NEW TMGMEDLIST,TMGMEDARRAY
   ;"NEW DBTAG SET DBTAG="*CSM-Database Review ="
   ;"DO MEDLIST^TMGTIUOJ(.TMGMEDLIST,.TMGDFN)
   ;"IF $DATA(TMGMEDLIST) DO
   ;". NEW DBDATE SET DBDATE=$PIECE(TMGMEDLIST,DBTAG,2)
   ;". NEW %DT,X,Y
   ;". SET %DT=""
   ;". SET DBDATE=$PIECE(DBDATE,$C(13),1)
   ;". SET DBDATE=$$TRIM^XLFSTR(DBDATE)
   ;". SET X=DBDATE
   ;". DO ^%DT
   ;". IF Y>0 DO
   ;". . SET TEST=1
   ;". . SET DATE=Y
   QUIT
   ;
DATPAINK(TMGDFN)   ;
   QUIT $$DATPAINK^TMGPXR01(.TMGDFN,1)
   ;"Code below was redundant. Can be deleted later if no problems arise.  ELH 5/8/18
   ;"Purpose: Return last date of pain contract,"DUE",or -1^Message
   ;"NEW TMGRESULT SET TMGRESULT="-1^NO DATE FOUND"
   ;"NEW CONTRACTTAG SET CONTRACTTAG="*CSM Contract ="
   ;"SET TMGDFN=$GET(TMGDFN)
   ;"IF TMGDFN'>0 DO  GOTO DNPC
   ;". SET TMGRESULT="-1^DFN NOT PROVIDED"
   ;"NEW TMGMEDLIST
   ;"DO MEDLIST^TMGTIUOJ(.TMGMEDLIST,.TMGDFN)
   ;"IF $DATA(TMGMEDLIST) DO
   ;". NEW CONTRACTDATE SET CONTRACTDATE=$PIECE(TMGMEDLIST,CONTRACTTAG,2)
   ;". SET CONTRACTDATE=$PIECE(CONTRACTDATE,$C(13),1)
   ;". SET CONTRACTDATE=$$TRIM^XLFSTR(CONTRACTDATE)
   ;". SET X=CONTRACTDATE
   ;". DO ^%DT
   ;". IF Y>0 DO
   ;". . SET TMGRESULT=Y
   ;". ELSE  DO
   ;". . SET TMGRESULT="DUE"
   ;"ELSE  DO
   ;". SET TMGRESULT="-1^MED LIST NOT FOUND"
   ;"DNPC ;
   ;"QUIT TMGRESULT
   ;
PAINDONE(TMGDFN,TEST,DATE,DATA,TEXT) ;
   DO PAINDONE^TMGPXR01(.TMGDFN,.TEST,.DATE,.DATA,.TEXT)
   QUIT
   ;"Code below was redundant. Can be deleted later if no problems arise.  ELH 5/8/18
   ;"Purpose: Return information about the date of birth for the
   ; patient
   ;"Input: TMGDFN -- the patient IEN
   ;"       TEST -- AN OUT PARAMETER.  The logical value of the test:
   ;1=true, 0=false
   ;"               Also an IN PARAMETER.  Any value for COMPUTED
   ;FINDING PARAMETER will be passed in here.
   ;"       DATE -- AN OUT PARAMETER.  Date of finding.
   ;"       DATA -- AN OUT PARAMETER.  PASSED BY REFERENCE.  See output
   ;format below
   ;"       TEXT -- Text to be display in the Clinical Maintenance
   ;output.  Optional.
   ;"Results: none
   ;"NEW CONTRACTDATE SET CONTRACTDATE=$$DATPAINK(.TMGDFN)
   ;"IF CONTRACTDATE="DUE" DO
   ;". SET TEST=0
   ;". SET DATE=0
   ;"ELSE  IF +CONTRACTDATE>0 DO
   ;". SET TEST=0
   ;". SET DATE=CONTRACTDATE
   ;"ELSE  DO
   ;". SET TEST=1
   ;". SET DATE=0
   ;"QUIT
   ;"
TMGFIELD(NAMESPACE,BEGNUM,ENDNUM)  ;
   ;"Purpose: To scan through all files and find any fields that
   ;"         are in the provided Numberspace or Namespace
   NEW FILEIEN,FILENAME,FIELDIEN,FIELDNAME,RESULTARR
   SET NAMESPACE=$GET(NAMESPACE),BEGNUM=+$GET(BEGNUM),ENDNUM=+$GET(ENDNUM)
   IF NAMESPACE="" SET NAMESPACE="TMG" ;"DEFAULT TO TMG
   IF BEGNUM=0 SET BEGNUM=22700   ;"DEFAULT TO TMG
   IF ENDNUM=0 SET ENDNUM=22799  ;"DEFAULT TO TMG
   SET BEGNUM=BEGNUM-1,ENDNUM=ENDNUM+1  ;"ADJUST
   SET FILEIEN=0
   WRITE "---------------BEGINNING FILE SEARCH---------------",!     
   FOR  SET FILEIEN=$ORDER(^DD(FILEIEN)) QUIT:FILEIEN'>0  DO
   . SET FILENAME=$ORDER(^DD(FILEIEN,0,"NM",0))
   . WRITE "  --CHECKING FILE ",FILENAME,!                   
   . IF FILENAME[NAMESPACE DO  QUIT
   . . WRITE "    **ENTIRE FILE ADDED",!
   . . SET RESULTARR("ENTIRE FILE",FILENAME)=FILEIEN
   . SET FIELDIEN=0
   . FOR  SET FIELDIEN=$ORDER(^DD(FILEIEN,FIELDIEN)) QUIT:+FIELDIEN'>0  DO
   . . SET FIELDNAME=$PIECE($GET(^DD(FILEIEN,FIELDIEN,0)),"^",1)
   . . WRITE "    --CHECKING FIELD ",FIELDNAME,!
   . . IF (FIELDNAME[NAMESPACE)!((FIELDIEN>BEGNUM)&(FIELDIEN<ENDNUM)) DO
   . . . WRITE "    **FIELD ",FIELDNAME," IS IN NAMESPACE",!
   . . . SET RESULTARR(FILENAME_" #"_FILEIEN,FIELDNAME)=FIELDIEN
   WRITE "--------------FILE SEARCH ENDED-------------------",!,!
   NEW %ZIS
   SET %ZIS("A")="Enter Output Device: "
   SET %ZIS("B")="HOME"
   DO ^%ZIS  ;"standard device call
   IF POP DO  GOTO TMGDn
   . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error opening output. Aborting.")
   USE IO
   WRITE " ----------------------------------------------_",!
   WRITE "| The following files and fields use the ",NAMESPACE,!
   WRITE "| namespace or are inside the ",BEGNUM,"-",ENDNUM,!
   WRITE "| numberspace.",!,!
   WRITE " ----------------------------------------------",!
   WRITE "      Routine called from TMGFIELD^TMGHRPC1",!! 
   NEW FILENAME,FIELDNAME
   SET FILENAME="ENTIRE FILE",FIELDNAME=""
   WRITE " -- ENTIRE FILES --",!
   FOR  SET FIELDNAME=$ORDER(RESULTARR(FILENAME,FIELDNAME)) QUIT:FIELDNAME=""  DO
   . WRITE "    ",FIELDNAME," #",$GET(RESULTARR(FILENAME,FIELDNAME)),!
   WRITE !        
   SET FILENAME=""
   WRITE " -- FIELDS --",!
   WRITE "    FILE NAME   FIELD",!
   FOR  SET FILENAME=$ORDER(RESULTARR(FILENAME)) QUIT:FILENAME=""  DO
   . IF FILENAME="ENTIRE FILE" QUIT
   . SET FIELDNAME=""
   . WRITE "    --",FILENAME,"--",!
   . FOR  SET FIELDNAME=$ORDER(RESULTARR(FILENAME,FIELDNAME)) QUIT:FIELDNAME=""  DO
   . . WRITE "                ->",FIELDNAME," #",$GET(RESULTARR(FILENAME,FIELDNAME)),!
   . WRITE !
   D ^%ZISC ;"Close the output device
TMGDn ;
   QUIT
   ;       