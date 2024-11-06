TMGHRPC2        ;TMG/elh/Support Functions for TMG_CPRS ;10/20/09; 11/6/10, 9/4/13, 2/2/14, 3/24/21
                ;;1.0;TMG-LIB;**1**;10/20/09;Build 3
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
 ;" ;               
 ;"=======================================================================
 ;"Dependencies:
 ;" ;
 ;"=======================================================================
 ;
LISTALL(Y,FROM,DIR,IGNOREINACTIVE)  ;" Return a bolus of patient names.  From is either Name or IEN^Name.
  ;"Entry point for RPC: ORWPT LIST ALL  <-- RPC changed for TMG site to use our custom lookup code.  
  ;"NOTE: This sets up global-scope var TMGIGNOREINACTIVE=1 IF IGNOREINACTIVE=1, for use in this module.  
  ;
  ;" MERGE ^TMG("TMP","RPC","LISTALL^TMGHRPC2","FROM")=FROM
  ;" MERGE ^TMG("TMP","RPC","LISTALL^TMGHRPC2","DIR")=DIR
  ;"IF $EXTRACT(FROM,1)="." DO INEXACT(.Y,FROM,.DIR) QUIT
  NEW TMGIGNOREINACTIVE SET TMGIGNOREINACTIVE=$GET(IGNOREINACTIVE)
  IF TMGIGNOREINACTIVE="" SET TMGIGNOREINACTIVE=0  ;"default to OFF.
  IF $$WEDGE^TMGHRPC2(.Y,FROM,.DIR) GOTO LADN
  IF FROM'="" DO INEXACT(.Y,FROM,.DIR) GOTO LADN  
  DO VALISTALL(.Y,.FROM,.DIR) GOTO LADN   ;"original --> DO LISTALL^ORWPT(.Y,.FROM,.DIR) GOTO LADN  //kt 6/25/24
LADN  ;          
  IF $$SHOULDGARBLE^TMGMISC4() DO GARBLERESULTS^TMGMISC4(.Y)     ;"check for special mode to hide patient info during demos
  QUIT      
  ;
VALISTALL(Y,FROM,DIR)	;" Return a bolus of patient names.  From is either Name or IEN^Name.
  ;"Copied and modified from LISTALL^ORWPT   
  ;"NOTE: TMGIGNOREINACTIVE is may be used in global scope here, or in descendant functions
  N I,IEN,CNT,FROMIEN,ORIDNAME S CNT=44,I=0,FROMIEN=0
  I $P(FROM,U,2)'="" S FROMIEN=$P(FROM,U,1),FROM=$O(^DPT("B",$P(FROM,U,2)),-DIR)
  F  S FROM=$O(^DPT("B",FROM),DIR) Q:FROM=""  D  Q:I=CNT
  . S IEN=FROMIEN,FROMIEN=0 F  S IEN=$O(^DPT("B",FROM,IEN)) Q:'IEN  D  Q:I=CNT
  . . IF $GET(TMGIGNOREINACTIVE),$$ACTIVEPT(IEN)=0 QUIT  ;"//kt 6/25/24 
  . . S ORIDNAME=$G(^DPT(IEN,0)) ; Get zero node name.
  . . ;" S X1=$G(^DPT(IEN,.1))_" "_$G(^DPT(IEN,.101))
  . . NEW NAME SET NAME=$P(ORIDNAME,U)
  . . ;"SET NAME=$EXTRACT(NAME,1)_" -- "_NAME
  . . S I=I+1 S Y(I)=IEN_U_FROM_U_U_U_U_NAME ;"    _"^"_X ; _"^"_X1  ;"   ("_X_")"
  Q
  ;
WEDGE(OUT,FROM,DIR)        ;
  ;"Purpose: Return a bolus of patient names, handling a leading date or
  ;"         phone number
  ;"Input: OUT --  Out parameter, pass by reference.
  ;"       FROM -- User specified string to search from.
  ;"                If in format of a date or phone #, then handled here.  
  ;"                Otherwise this function QUITs, and handling will
  ;"                occur elsewhere
  ;".               Input can be either Name or IEN^Name (but later not
  ;"                   handled here)
  ;"                Example of Input: '10/1/67 Too~'
  ;"                Example of Input: (phone number) <-- finish with exact example
  ;"                Note: CPRS decrements the terminal character of user
  ;"                   input, and adds a ~
  ;"       DIR -- should be 1 or -1
  ;"NOTE: TMGIGNOREINACTIVE is may be used in global scope here, or in descendant functions
  ;"Results: 1 IF handled, 0 IF not handled.
  ;"
  NEW TMGCH,TMGTEMP,TMGTNAME,TMGSUBIEN,TMGB,TMGABORT
  NEW I,IEN,CNT,FROMIEN,TMGNAME,TMGA,TMGANAME,TMGRESULT
  NEW %DT,X,Y
  SET CNT=44,I=0,TMGABORT=0
  SET DIR=$GET(DIR,1)
  SET TMGRESULT=0  ;"Default to failure
  SET TMGA=$PIECE(FROM," ",1)
  SET TMGB=$PIECE(FROM," ",2)
  IF $$ISPHONE(FROM) QUIT $$HANDLEPHONE(.OUT,.FROM,.DIR)
  IF $$ISFNAME(FROM) QUIT $$HNDLFNAME(.OUT,.FROM,.DIR)
  IF $$ISMONTH(TMGA) DO
  . NEW YEAR SET YEAR=$P(FROM," ",3)
  . IF (YEAR?2.4N) DO
  . . SET TMGA=$PIECE(FROM," ",1,3)
  . . SET TMGB=$PIECE(FROM," ",4)
  . ELSE  DO
  . . SET TMGA=$PIECE(FROM," ",1,2)
  . . SET TMGB=$PIECE(FROM," ",3)
  ELSE  DO
  . ;"Test for nN/nN/nnNN pattern
  . IF '(TMGA?1.2N1(1"-",1"/")1.2N1(1"-",1"/")2.4E0.1"~") SET TMGABORT=1
  IF TMGABORT GOTO WQ
  SET TMGA=$TRANSLATE(TMGA,"~","")
  IF (DIR=1),TMGB="" DO   ;"Reverse CPRS's inc/dec of terminal digit IF isolated date
  . SET TMGCH=$E(TMGA,$L(TMGA))
  . SET TMGCH=$CHAR($ASCII(TMGCH)+DIR)
  . SET TMGA=$E(TMGA,1,$L(TMGA)-1)_TMGCH
  SET %DT="P" ;"Assume past dates
  SET X=TMGA
  DO ^%DT  ;"convert external date to FM Date
  IF Y=-1 GOTO WQ
  SET IEN=0
  ;"Gather ALL patients with specified DOB, so can be sorted alphabetically
  FOR  SET IEN=$ORDER(^DPT("ADOB",Y,IEN)) QUIT:'IEN  DO
  . IF $GET(TMGIGNOREINACTIVE),$$ACTIVEPT(IEN)=0 QUIT  ;"//kt 6/25/24 
  . SET TMGNAME=$P($G(^DPT(IEN,0)),U,1) ; Get zero node name.
  . NEW TEMP SET TEMP=TMGA_" "_TMGNAME
  . SET TMGTEMP(TEMP,IEN_U_TEMP_U_U_U_U_TEMP)=""
  . SET TMGSUBIEN=0
  . FOR  SET TMGSUBIEN=$O(^DPT(IEN,.01,TMGSUBIEN)) QUIT:TMGSUBIEN=""  DO
  . . SET TMGANAME=$P($G(^DPT(IEN,.01,TMGSUBIEN,0)),U,1)
  . . NEW TEMP2 SET TEMP2=TMGA_" "_TMGANAME
  . . SET TMGTEMP(TEMP2,IEN_U_TEMP2_U_U_U_U_TEMP)=""
  ;
  KILL OUT        
  SET TMGTNAME=TMGA_" "_$$UP^XLFSTR($TRANSLATE(TMGB,"~",""))
  ;"Get sublist of patients starting at specified last name etc.
  FOR  SET TMGTNAME=$ORDER(TMGTEMP(TMGTNAME),DIR) QUIT:TMGTNAME=""  DO  QUIT:I=CNT
  . NEW ENTRY SET ENTRY=""
  . FOR  SET ENTRY=$O(TMGTEMP(TMGTNAME,ENTRY),DIR) QUIT:ENTRY=""  DO  QUIT:I=CNT
  . . SET I=I+1
  . . SET OUT(I)=ENTRY
  SET TMGRESULT=1
  GOTO WQ
  ;
WQ ;
  QUIT TMGRESULT
  ;
ISFNAME(S) ;"
  ;"Purpose: To determine whether the provided
  ;"         string is a first name only, which
  ;"         would have a leading comma and a name
  NEW TMGRESULT SET TMGRESULT=0
  SET S=$$TRIM^XLFSTR(S)
  IF S'["," QUIT TMGRESULT
  NEW LNAME,FNAME
  SET LNAME=$PIECE(S,",",1)
  SET FNAME=$PIECE(S,",",2)
  IF (LNAME="")&(FNAME'="") SET TMGRESULT=1
  QUIT TMGRESULT
  ;"
HNDLFNAME(OUT,FROM,DIR)  ;"
  ;"NOTE: TMGIGNOREINACTIVE is may be used in global scope here, or in descendant functions
  NEW TMGRESULT SET TMGRESULT=1
  NEW TMPNAME,FNAME,TMGDFN,I
  SET I=0
  SET TMPNAME=$$UP^XLFSTR($PIECE(FROM,",",2))
  SET TMPNAME=$TRANSLATE(TMPNAME,"~","")
  IF DIR=1 DO
  . NEW TMGCH SET TMGCH=$EXTRACT(TMPNAME,$LENGTH(TMPNAME))
  . SET TMGCH=$CHAR($ASCII(TMGCH)+DIR)
  . SET TMPNAME=$EXTRACT(TMPNAME,1,$LENGTH(TMPNAME)-1)_TMGCH
  SET FNAME=TMPNAME
  SET TMGDFN=0
  ;"GET ALL PATIENTS WITH FIRST NAME LISTED
  FOR  SET TMGDFN=$ORDER(^DPT("TMGFN",FNAME,TMGDFN)) QUIT:TMGDFN'>0  DO
  . IF $GET(TMGIGNOREINACTIVE),$$ACTIVEPT(TMGDFN)=0 QUIT  ;"//kt 6/25/24 
  . NEW NAME SET NAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
  . SET I=I+1
  . SET OUT(I)=TMGDFN_U_NAME_U_U_U_U_NAME
  ;"GET ALL OTHER PATIENTS WHO HAVE ADDITIONAL CHARS SUCH AS MIDDLE INITIALS
  FOR  SET FNAME=$ORDER(^DPT("TMGFN",FNAME)) QUIT:(FNAME'[TMPNAME)!(FNAME="")  DO
  . SET TMGDFN=0
  . FOR  SET TMGDFN=$ORDER(^DPT("TMGFN",FNAME,TMGDFN)) QUIT:TMGDFN'>0  DO
  . . NEW NAME SET NAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
  . . SET I=I+1
  . . SET OUT(I)=TMGDFN_U_NAME_U_U_U_U_NAME
  QUIT TMGRESULT
  ;
ISMONTH(S)        ;
  ;"Purpose: to determine IF S is a string specifying a month.
  ;"Input: S -- the string to test.  It is altered, so don't pass by reference
  ;"Results: 1 IF is a month name, or 0 IF not.
  SET S=$$UP^XLFSTR(S)
  SET S=$TRANSLATE(S,".","")
  IF S="JANUARY"!(S="JAN") QUIT 1
  IF S="FEBRUARY"!(S="FEB") QUIT 1
  IF S="MARCH"!(S="MAR") QUIT 1
  IF S="APRIL"!(S="APR") QUIT 1
  IF S="MAY" QUIT 1
  IF S="JUNE"!(S="JUN") QUIT 1
  IF S="JULY"!(S="JUL") QUIT 1
  IF S="AUGUST"!(S="AUG") QUIT 1
  IF S="SEPTEMBER"!(S="SEP")!(S="SEPT") QUIT 1
  IF S="OCTOBER"!(S="OCT") QUIT 1
  IF S="NOVEMBER"!(S="NOV") QUIT 1
  IF S="DECEMBER"!(S="DEC") QUIT 1
  QUIT 0
  ;
ISPHONE(S)    ;    
  ;"Purpose: to determine is S is a phone number.
  ;"Input: S -- the string to test.
  ;"Results: 1 IF a phone number, or 0 IF not.
  NEW RESULT SET RESULT=0
  ;
  ;"###[-, ]###[-, ]{1-4}#'s [any other chars]
  IF (S?3N0.1(1"-",1" ")3N1(1"-",1" ")1.4N.E) SET RESULT=1 GOTO IPHDN
  ;    
  ;IF (S?3N0.1(1"-",1" ")3.4N.E) SET RESULT=1 GOTO IPHDN
  IF $EXTRACT(S,1)="(" SET RESULT=1 GOTO IPHDN
  ;
  NEW S2 SET S2=$TRANSLATE(S,"()- ","")  ;"Strip formatting chars
  ;"[6-10]#'s
  IF S2?6.10N0.1"~" SET RESULT=1 GOTO IPHDN  ;"NOTE: '~' added by ORListBox to allow $ORDER() to find. 
IPHDN     ;          
  QUIT RESULT
  ;
INEXACT(OUT,FROM,DIR)    ;    
  ;"Purpose: To perform an inexact, old-style Fileman lookup on user input
  ;"Input: OUT --  Out parameter, pass by reference.
  ;"       FROM -- User specified string to search from.
  ;".               Input can be either Name or IEN^Name
  ;"                Example of Input: '.Smit,Joh~'
  ;"                Note: CPRS decrements the terminal character of user
  ;"                   input, and adds a ~
  ;"       DIR -- should be 1 or -1
  ;"NOTE: TMGIGNOREINACTIVE is may be used in global scope here, or in descendant functions
  ;"Results: NONE
  ;"
  NEW I,IEN,CNT,FROMIEN,TMGNAME
  SET CNT=44,I=0,FROMIEN=0
  SET DIR=$GET(DIR,1)
  NEW TMGCH,TMGTRIM SET TMGTRIM=""
  ;"Trim any leading '.' or ' '
  FOR  SET TMGCH=$EXTRACT(FROM,1) QUIT:(". "'[TMGCH)!(TMGCH="")!(TMGCH[" ")  DO
  . SET TMGTRIM=TMGTRIM_$EXTRACT(FROM,1)
  . SET FROM=$EXTRACT(FROM,2,999)
  IF $PIECE(FROM,U,2)'="" DO
  . SET FROM=$PIECE(FROM,U,2)
  . SET FROMIEN=$PIECE(FROM,U,1)
  NEW TMGSRCH SET TMGSRCH=$TRANSLATE(FROM,"~","")
  NEW TMGSRFROM SET TMGSRFROM=""
  IF TMGSRCH[" -- " DO
  . SET TMGSRFROM=$PIECE(TMGSRCH," -- ",2)
  . SET TMGSRCH=$PIECE(TMGSRCH," -- ",1)
  IF (DIR=1),(TMGSRFROM="") DO   ;"Reverse CPRS's inc/dec of terminal digit
  . SET TMGCH=$EXTRACT(TMGSRCH,$LENGTH(TMGSRCH))
  . SET TMGCH=$CHAR($ASCII(TMGCH)+DIR)
  . SET TMGSRCH=$EXTRACT(TMGSRCH,1,$LENGTH(TMGSRCH)-1)_TMGCH
  NEW TMGOUT,TMGMSG
  DO FIND^DIC(2,,"@;.01","PBC",TMGSRCH,"*",,,,"TMGOUT","TMGMSG")
  KILL OUT
  IF +TMGOUT("DILIST",0)'>0 GOTO INEDN    ;"QUIT  ;"No matches found.
  ;
  ;"Gather ALL matching patients so can be sorted alphabetically
  NEW TMGBYIEN,TMGTEMP
  NEW TMGIDX SET TMGIDX=0
  FOR  SET TMGIDX=$ORDER(TMGOUT("DILIST",TMGIDX)) QUIT:(TMGIDX="")  DO
  . SET TMGNAME=$PIECE($GET(TMGOUT("DILIST",TMGIDX,0)),U,2)
  . SET IEN=+$PIECE($GET(TMGOUT("DILIST",TMGIDX,0)),U,1)
  . IF $GET(TMGIGNOREINACTIVE),$$ACTIVEPT(IEN)=0 QUIT  ;"//kt 6/25/24 
  . NEW TEMP SET TEMP=TMGTRIM_TMGSRCH_" -- "_TMGNAME
  . SET TMGTEMP(TEMP,IEN_U_TEMP_U_U_U_U_TEMP)=""
  . IF FROMIEN>0 SET TMGBYIEN(IEN)=IEN_U_TEMP_U_U_U_U_TEMP
  ;
  KILL OUT
  IF $DATA(TMGBYIEN) DO
  . SET IEN=FROMIEN-DIR
  . FOR  SET IEN=$ORDER(TMGBYIEN(IEN),DIR) QUIT:(+IEN'>0)!(I=CNT)  DO
  . . SET I=I+1
  . . SET OUT(I)=$GET(TMGBYIEN(IEN))
  ELSE  DO
  . SET TMGTNAME=TMGTRIM_TMGSRCH_" -- "_TMGSRFROM
  . FOR  SET TMGTNAME=$ORDER(TMGTEMP(TMGTNAME),DIR) QUIT:TMGTNAME=""  DO  QUIT:I=CNT
  . . NEW ENTRY SET ENTRY=""
  . . FOR  SET ENTRY=$ORDER(TMGTEMP(TMGTNAME,ENTRY),DIR) QUIT:ENTRY=""  DO  QUIT:I=CNT
  . . . SET I=I+1
  . . . SET OUT(I)=ENTRY
  ;
  ;"Now expand found list up to full bolus size   //kt added 6/28/24
  NEW IEN,FROMIEN SET FROMIEN=0
  SET FROM=TMGSRCH
  IF 0=1,I<CNT FOR  SET FROM=$ORDER(^DPT("B",FROM),DIR) QUIT:FROM=""  DO  QUIT:I=CNT
  . SET IEN=FROMIEN,FROMIEN=0 
  . FOR  SET IEN=$ORDER(^DPT("B",FROM,IEN)) Q:'IEN  D  Q:I=CNT
  . . IF $GET(TMGIGNOREINACTIVE),$$ACTIVEPT(IEN)=0 QUIT  
  . . NEW ZN SET ZN=$GET(^DPT(IEN,0)) ; Get zero node name.
  . . SET TMGNAME=$PIECE(ZN,"^",1)
  . . SET I=I+1 
  . . SET OUT(I)=IEN_U_FROM_U_U_U_U_TMGNAME      
  ;            
INEDN
  ;"Here we will additionally search for nicknames (no matter if the above returned results or not
  NEW LNAME,FNAME
  SET LNAME=$P(TMGSRCH,",",1),FNAME=$P(TMGSRCH,",",2)
  K TMGOUT,TMGMSG
  DO FIND^DIC(2,,"@;.01","PBC",LNAME,"*",,,,"TMGOUT","TMGMSG")
  IF +TMGOUT("DILIST",0)'>0 QUIT  ;"No matches found.
  SET TMGIDX=0
  FOR  SET TMGIDX=$ORDER(TMGOUT("DILIST",TMGIDX)) QUIT:(TMGIDX="")  DO
  . SET TMGNAME=$PIECE($GET(TMGOUT("DILIST",TMGIDX,0)),U,2)
  . SET IEN=+$PIECE($GET(TMGOUT("DILIST",TMGIDX,0)),U,1)
  . NEW PERFNAME SET PERFNAME=$P($G(^DPT(IEN,.24)),"^",5)
  . IF PERFNAME[FNAME DO
  . . IF $GET(TMGIGNOREINACTIVE),$$ACTIVEPT(IEN)=0 QUIT
  . . ;"NEW ZN SET ZN=$GET(^DPT(IEN,0)) ; Get zero node name.
  . . NEW DUPLICATE SET DUPLICATE=$$CHKDUP(.OUT,IEN)
  . . IF DUPLICATE=1 QUIT  ;" DON'T ADD DUPLICATE
  . . SET I=I+1
  . . SET OUT(I)=IEN_U_TMGSRCH_" -- "_TMGNAME_" "_""""_$$UP^XLFSTR(PERFNAME)_""""_U_U_U_U_TMGNAME
  QUIT
  ;
CHKDUP(ARRAY,IEN) ;"CHECK THE SENT ARRAY FOR DUPLICATE DFN
  NEW TMGRESULT SET TMGRESULT=0
  NEW IDX SET IDX=0
  FOR  SET IDX=$O(ARRAY(IDX)) QUIT:IDX'>0  DO
  . NEW ONEIEN SET ONEIEN=$P($G(ARRAY(IDX)),"^",1)
  . IF ONEIEN=IEN SET TMGRESULT=1
  QUIT TMGRESULT
  ;"
LSTHIPAA(TMGOUT,TMGDFN,FORMAT)   ;
  ;"PURPOSE: To return date of last HIPAA scanned into CPRS
  ;"INPUT: TMGDFN - Patient's DFN
  ;"       FORMAT (OPTIONAL I or E) Return format of date (Internal(Default)/External)
  ;"RETURN - TMGOUT - Date in either internal or external format as defined by FORMAT OR -1^ERROR
  ;"RESULT - NONE
  NEW HIPAAIEN,TIUIEN,DATE
  SET HIPAAIEN=34    ;"IEN OF HIPAA IN TIU DOCUMENT
  SET TIUIEN=0,DATE=0
  SET TMGDFN=+$GET(TMGDFN)
  IF TMGDFN'>0 DO  GOTO LHDN
  . SET TMGOUT="-1^NO DFN RECEIVED"
  SET TMGOUT="-1^NO HIPAA AGREEMENT FOUND"
  SET FORMAT=$GET(FORMAT)
  IF (FORMAT'="E")&(FORMAT'="I") SET FORMAT="I"
  FOR  SET TIUIEN=$ORDER(^TIU(8925,"C",TMGDFN,TIUIEN)) QUIT:TIUIEN'>0  DO
  . NEW DOCIEN SET DOCIEN=$PIECE($GET(^TIU(8925,TIUIEN,0)),"^",1)
  . IF DOCIEN'=HIPAAIEN QUIT
  . NEW THISDATE SET THISDATE=$PIECE($GET(^TIU(8925,TIUIEN,0)),"^",7)
  . IF THISDATE>DATE SET DATE=THISDATE
  IF DATE'=0 DO
  . IF FORMAT="I" SET TMGOUT=DATE
  . ELSE  DO
  . . NEW Y SET Y=DATE
  . . D DD^%DT
  . . SET TMGOUT=Y
LHDN ;
  QUIT
  ;
NEWHIPAA(TMGOUT,TMGDFN)    ;
  ;"PURPOSE: TO DETERMINE IF A NEW HIPAA IS NEEDED. 
  ;"INPUT: TMGDFN - PATIENT'S DFN
  ;"OUTPUT: TMGOUT - "1^DUE"      IF NEW HIPAA IS NEEDED
  ;"                 "0^NOT DUE"  IF NEW HIPAA IS NOT NEEDED
  ;"                 -1^ERROR MESSAGE
  NEW MAXDAYSALLOW SET MAXDAYSALLOW=365
  SET TMGDFN=+$GET(TMGDFN)
  IF TMGDFN'>0 DO  GOTO NHDN
  . SET TMGOUT="-1^NO DFN"
  SET TMGOUT="1^DUE"   ;"DEFAULT TO NEEDED
  ;
  NEW X,TODAYSDATE
  DO NOW^%DTC
  SET TODAYSDATE=X
  ;
  NEW NOTEDATE
  DO LSTHIPAA(.NOTEDATE,TMGDFN,"I")
  IF $PIECE(NOTEDATE,"^",1)="-1" GOTO NHDN
  NEW DAYSDIFF ;"SET DAYSDIFF=TODAYSDATE-NOTEDATE
  ;"
  NEW X1,X2
  SET X1=TODAYSDATE
  SET X2=NOTEDATE
  D ^%DTC
  SET DAYSDIFF=X
  ;"
  IF DAYSDIFF<MAXDAYSALLOW SET TMGOUT="0^NOT DUE"
NHDN  ;
  QUIT          
  ;
HANDLEPHONE(OUT,FROM,DIR)    ;    
  ;"PURPOSE: To handle patient lookup by telephone
  ;"Gather ALL patients with specified DOB, so can be sorted alphabetically
  ;"Results: 1 IF handled, 0 IF not handled.
  ;"NOTE: TMGIGNOREINACTIVE is may be used in global scope here, or in descendant functions
  NEW TEMP1,TEMP2,TEMP3,TEMPNUM,IEN,TMGNAME,TMGTNAME,TMGCH
  SET FROM=$TRANSLATE(FROM,"~","")
  IF (DIR=1) DO   ;"Reverse CPRS's inc/dec of terminal digit IF isolated date
  . SET TMGCH=$E(FROM,$L(FROM))
  . SET TMGCH=$CHAR($ASCII(TMGCH)+DIR)
  . SET FROM=$E(FROM,1,$L(FROM)-1)_TMGCH
  ;"SET TEMPNUM=$TR(FROM,"(")
  ;"SET TEMPNUM=$TR(TEMPNUM," ")
  ;"SET TEMPNUM=$TR(TEMPNUM,"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz!@#$%^&*)-_=+[]{}<>,./?:;'\|")
  SET TEMPNUM=$$EXTRACTNUM^TMGSTUT3(FROM) ;"strip out any non-numeric characteers  
  SET TEMP1=$EXTRACT(TEMPNUM,1,3)
  SET TEMP2=$EXTRACT(TEMPNUM,4,6)
  SET TEMP3=$EXTRACT(TEMPNUM,7,99)
  ;
  NEW TMGIDX
  FOR TMGIDX="AZVWVOE","ATMGPHONEWORK","ATMGPHONETEMP","ATMGPHONECELL"  DO
  . SET TEMPNUM=TEMP1_TEMP2_TEMP3
  . DO DOLOOKUP(.TMGTEMP,TMGIDX,TEMPNUM,FROM)
  . IF $LENGTH(TEMPNUM)=7 DO  ;"user entered number without preceeding area code
  . . DO BRUTELOOKUP(.TMGTEMP,TMGIDX,TEMPNUM,FROM) ;"//kt 9/28/22
  . SET TEMPNUM=TEMP1_" "_TEMP2_" "_TEMP3
  . DO DOLOOKUP(.TMGTEMP,TMGIDX,TEMPNUM,FROM)
  . SET TEMPNUM=TEMP1_" "_TEMP2_TEMP3
  . DO DOLOOKUP(.TMGTEMP,TMGIDX,TEMPNUM,FROM)
  . SET TEMPNUM=TEMP1_TEMP2_" "_TEMP3
  . DO DOLOOKUP(.TMGTEMP,TMGIDX,TEMPNUM,FROM)
  ;
  KILL OUT
  SET TMGTNAME=FROM_" "_$$UP^XLFSTR($TRANSLATE(FROM,"~",""))
  ;"Get sublist of patients starting at specified last name etc.
  FOR  SET TMGTNAME=$ORDER(TMGTEMP(TMGTNAME),DIR) QUIT:TMGTNAME=""  DO  QUIT:I=CNT
  . NEW ENTRY SET ENTRY=""
  . FOR  SET ENTRY=$O(TMGTEMP(TMGTNAME,ENTRY),DIR) QUIT:ENTRY=""  DO  QUIT:I=CNT
  . . SET I=I+1
  . . SET OUT(I)=ENTRY
  QUIT 1
  ;
DOLOOKUP(TMGTEMP,INDEX,ITEM,FROM)  ;      
  ;"NOTE: TMGIGNOREINACTIVE is may be used in global scope here, or in descendant functions
  NEW TMGNAME,IEN
  SET IEN=0
  FOR  SET IEN=$ORDER(^DPT(INDEX,ITEM,IEN)) QUIT:'IEN  DO
  . IF $GET(TMGIGNOREINACTIVE),$$ACTIVEPT(IEN)=0 QUIT  ;"//kt 6/25/24 
  . SET TMGNAME=$P($G(^DPT(IEN,0)),U,1) ; Get zero node name.
  . NEW TEMP SET TEMP=FROM_" "_TMGNAME
  . SET TMGTEMP(TEMP,IEN_U_TEMP_U_U_U_U_TEMP)=""
  . SET TMGSUBIEN=0
  QUIT
  ;
BRUTELOOKUP(TMGTEMP,INDEX,ITEM,FROM) ;
  ;"NOTE: Because this looks at EVERY number in index, it could be SLOW!       
  ;"NOTE: TMGIGNOREINACTIVE is may be used in global scope here, or in descendant functions
  NEW TMGNAME,IEN
  SET IEN=0
  NEW ANUM SET ANUM=""
  FOR  SET ANUM=$ORDER(^DPT(INDEX,ANUM)) QUIT:'ANUM  DO
  . IF $TRANSLATE(ANUM," ","")'[ITEM QUIT
  . FOR  SET IEN=$ORDER(^DPT(INDEX,ANUM,IEN)) QUIT:'IEN  DO
  . . IF $GET(TMGIGNOREINACTIVE),$$ACTIVEPT(IEN)=0 QUIT  ;"//kt  6/25/24
  . . SET TMGNAME=$P($G(^DPT(IEN,0)),U,1) ; Get zero node name.
  . . NEW TEMP SET TEMP=FROM_" "_TMGNAME
  . . SET TMGTEMP(TEMP,IEN_U_TEMP_U_U_U_U_TEMP)=""
  . . SET TMGSUBIEN=0
  QUIT
  ;
ACTIVEPT(ADFN) ;"Is patient active.    //kt 6/25/24
  NEW RESULT SET RESULT=$$ACTIVEPT^TMGPXR03(ADFN,10)  ;"10 yr window for activity.
  QUIT RESULT
  ;
FIXINDEX        ;
  ;Check date of last run
  ;if < 5 mins exit
  ;loop through entry in Index
  ;check entry for proper format
  ;if improper format, then have fileman store NEW properly formatted number
  ;store date of last run
  ;
SETRPC(TMGMODULE)        ;
  ;"Purpose: Set module for RPC call.
  IF $GET(TMGMODULE)="" QUIT
  NEW TMGDATA,TMGMSG
  SET DIC(0)="B"
  SET DIC=8994
  SET X="ORWPT LIST ALL"
  DO ^DIC
  IF Y=-1 QUIT
  SET TMGDATA(8994,$P(Y,U,1)_",",".03")=TMGMODULE
  DO FILE^DIE("K","TMGDATA","TMGMSG")
  QUIT
  ;
INSTWEDG        ;
  ;"Purpose: ROUTES RPC FOR CPRS PATIENT LOOKUP FROM ORWPT TO TMGHRPC2, IF IT EXISTS
  DO SETRPC("TMGHRPC2") ;
  QUIT
  ;
DELWEDG        ;
  ;"Purpose: ROUTES RPC FOR CPRS PATIENT LOOKUP FROM ORWPT TO TMGHRPC2, IF IT EXISTS
  DO SETRPC("ORWPT") ;
  QUIT
  ;"
HTNRPT ;
  NEW TMGDFN SET TMGDFN=0
  NEW HASHTN,SDATE,EDATE
  DO GETDTS^TMGC0Q07(.SDATE,.EDATE)
  NEW NAME,HTNARRAY,DOB
  SET %ZIS("A")="Enter Output Device: "
  SET %ZIS("B")="HOME"
  NEW INITIO SET INITIO=IO
  DO ^%ZIS  ;"standard device call
  NEW TOHOME SET TOHOME=(IO=INITIO)
  IF POP GOTO POM
  USE IO
  FOR  SET TMGDFN=$ORDER(^DPT(TMGDFN)) QUIT:TMGDFN'>0  DO
  . IF $$INLSTHTN^TMGC0Q05(TMGDFN,SDATE,EDATE) DO
  . . SET NAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
  . . SET DOB=$PIECE($GET(^DPT(TMGDFN,0)),"^",3)
  . . SET HTNARRAY(NAME,DOB)=""
  WRITE "MEANINGFUL USE - PATIENT LIST  (HTN)",!,!
  SET NAME=""
  FOR  SET NAME=$ORDER(HTNARRAY(NAME)) QUIT:NAME=""  DO
  . SET DOB=0
  . FOR  SET DOB=$ORDER(HTNARRAY(NAME,DOB)) QUIT:DOB'>0  DO
  . . WRITE NAME," (",$$FMTE^XLFDT(DOB,"2D"),")",!
  DO ^%ZISC  ;" Close the output device
POM  ;
  QUIT
  ;
GETRACE(TMGRESULT,TMGDFN)  ;
  ;"Purpose: Return patients race (Single letter: W, B, etc...)
    
  QUIT        
