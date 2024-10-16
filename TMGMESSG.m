TMGMESSG ;TMG/elh-Routines designed to work with AHK Messenger ;1/16/23
              ;;1.0;TMG-LIB;**1**;1/16/23
 ;
 ;"TMG MESSENGER ROUTINES 
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 1/16/23  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"GETUSERS(TMGRESULTS) -- This routine returns all the users listed in the Network Messenger ini file
 ;"SENDMESSAGE(TMGRESULT,TO,FROM,MESSAGE) -- This routine will send a message to the Network Messenger
 ;"ORDERTO(TMGRESULT) -- This routine will return the user that needs to get the lab order message (as set in the LabOrder.ini file
 ;"GETMYNAM(TMGRESULT,IPADDRESS) -- Purpose: This routine will return the current user's name, based on IP Address
 ;"
INIFILENAME()  ;"Name of the INI File holding the user list
  QUIT "NetworkMessengerUsers.ini"
  ;"
LABORDERINI()  ;"Name of the lab order ini
  QUIT "LabOrder.ini"
  ;"
LABORDERLOC()  ;"Path of the lab order folder
  QUIT "/mnt/WinPublic/e.hagood"
  ;"
NETMESSLOC()  ;"Network folder path
  QUIT "/mnt/WinPublic/NetworkMessenger"
  ;"
GETMYNAM(TMGRESULT,IPADDRESS) ;"
  ;"Purpose: This routine will return the current user's name, based on IP Address
  SET TMGRESULT="-1^NO USER FOUND WITH IP ADDRESS "_IPADDRESS
  NEW USERSARR
  DO HFS2ARR^TMGIOUT3($$NETMESSLOC(),$$INIFILENAME(),"USERSARR")
  NEW IDX SET IDX=1 ;"IGNORE HEADER
  FOR  SET IDX=$O(USERSARR(IDX)) QUIT:IDX'>0  DO
  . NEW LINE,NAME,IP
  . SET LINE=$P($G(USERSARR(IDX)),$C(13),1)
  . SET NAME=$P(LINE,"=",1)
  . SET IP=$P(LINE,"=",2)
  . IF IP'=IPADDRESS QUIT
  . IF NAME="" QUIT
  . SET TMGRESULT="1^"_NAME_"^"_IP
  QUIT
  ;"
GETUSERS(TMGRESULT)  ;"
 ;"Purpose: This routine returns all the users listed in the Network Messenger ini file
 SET TMGRESULT(0)="1^SUCCESS"
 NEW USERSARR
 DO HFS2ARR^TMGIOUT3($$NETMESSLOC(),$$INIFILENAME(),"USERSARR")
 NEW IDX SET IDX=1 ;"IGNORE HEADER
 NEW OUTIDX SET OUTIDX=1
 FOR  SET IDX=$O(USERSARR(IDX)) QUIT:IDX'>0  DO
 . NEW LINE,NAME,IP
 . SET LINE=$P($G(USERSARR(IDX)),$C(13),1)
 . SET NAME=$P(LINE,"=",1)
 . SET IP=$P(LINE,"=",2)
 . IF (NAME="")!(IP="") QUIT
 . SET TMGRESULT(OUTIDX)=NAME_"^"_IP,OUTIDX=OUTIDX+1
 IF OUTIDX=1 SET TMGRESULT(0)="-1^NO USERS RETURNED"
 ZWR TMGRESULT
 QUIT
 ;"
SEND1MSG(TMGRESULT,USERTO,USERFROM,MESSAGE,NOTECREATED)  ;"
 ;"Purpose: This routine will send a message to the Network Messenger
 ;"Input: USERTO - Receiving user's name
 ;"       USERFROM - Sending user's name
 ;"       MESSAGE - ARRAY Message to be displayed in lines
 SET NOTECREATED=+$GET(NOTECREATED)
 SET TMGRESULT="1^SUCCESS"
 ;"ADD TIME TO Z-NODE!!
 NEW OUTFILENAME SET OUTFILENAME=$$UNIQUEFN(USERTO,USERFROM)
 IF $P(OUTFILENAME,"^",1)="-1" DO  QUIT
 . SET TMGRESULT=OUTFILENAME
 NEW SUFFIX SET SUFFIX=""
 IF NOTECREATED SET SUFFIX=$C(13,10)_"(A NOTE HAS BEEN CREATED IN CPRS FOR THIS MESSAGE)"
 SET MESSAGE(0)="["_$$MSGDATE_"] -- "_$G(MESSAGE(0))_SUFFIX
 DO AR2HFSFP^TMGIOUT3("MESSAGE",OUTFILENAME)
 QUIT
 ;"
UNIQUEFN(TO,FROM)  ;"
 ;"Purpose: This routine will return a unique filename (appending numbers 
 ;"         as needed until it is an unused filename)
 ;"Input: TO - Receiving user's name
 ;"       FROM - Sending user's name
 NEW FILENAME,COUNT,DONE
 SET FILENAME=$$NETMESSLOC()_"/"_TO_"-"_FROM_".mgr"
 IF $$ISFILE^TMGKERNL(FILENAME)=0 GOTO UFDN
 SET COUNT=0,DONE=0
 FOR  SET COUNT=COUNT+1 QUIT:DONE=1  DO
 . SET FILENAME=$$NETMESSLOC()_"/"_TO_"-"_FROM_"-"_COUNT_".mgr"
 . IF $$ISFILE^TMGKERNL(FILENAME)=0 SET DONE=1
 . IF COUNT>99 SET FILENAME="-1^ERROR FINDING UNIQUE FILENAME",DONE=1
UFDN
 QUIT FILENAME
 ;"
ORDERTO(TMGRESULT,TYPE)  ;"
 ;"Purpose: This routine will return the user that needs to get the
 ;"         lab order message (as set in the LabOrder.ini file
 ;"Input: Type is the user to return 1=Normal user to receive 
 ;"                                  2=Lab user for today or add-ons
 NEW LABINIARR,IDX
 SET TYPE=+$G(TYPE)
 IF TYPE'>0 DO  GOTO OTDN
 . SET TMGRESULT="-1^NO TYPE SENT"
 IF TYPE=2 DO  GOTO OTDN
 . SET TMGRESULT="1^Room 3"   ;"This user is hard coded as always the Lab Room
 SET TMGRESULT="-1^NO USER RETURNED",IDX=0
 DO HFS2ARR^TMGIOUT3($$LABORDERLOC(),$$LABORDERINI(),"LABINIARR")
 FOR  SET IDX=$O(LABINIARR(IDX)) QUIT:IDX'>0  DO
 . NEW LINE SET LINE=$G(LABINIARR(IDX))
 . IF LINE["MessageRecipient" DO
 . . NEW USER SET USER=$P(LINE,"MessageRecipient=",2)
 . . SET USER=$P(USER,$C(13),1)
 . . SET TMGRESULT="1^"_USER
OTDN
 QUIT
 ;"
FUMSGTO(TMGRESULT,TYPE)  ;"
 ;"Purpose: This routine will return the user that needs to get the
 ;"         office visit Follow up order message 
 ;"Input: Type is the user to return 1=Normal user to receive 
 ;"                                  2=<future expansion>
 NEW INIARR,IDX
 SET TYPE=+$G(TYPE)
 IF TYPE'>0 DO  GOTO OTDN
 . SET TMGRESULT="-1^NO TYPE SENT"
 SET TMGRESULT="-1^NO USER RETURNED",IDX=0
 DO HFS2ARR^TMGIOUT3($$LABORDERLOC(),$$LABORDERINI(),"INIARR")
 FOR  SET IDX=$O(INIARR(IDX)) QUIT:IDX'>0  DO
 . NEW LINE SET LINE=$G(INIARR(IDX))
 . IF LINE["MessageRecipient" DO
 . . NEW USER SET USER=$P(LINE,"MessageRecipient=",2)
 . . SET USER=$P(USER,$C(13),1)
 . . SET TMGRESULT="1^"_USER
 QUIT
 ;"
MSGDATE()  ;"
 ;"Purpose: Get the formatted date string for the message
 NEW DATE,%,Y DO NOW^%DTC
 SET Y=$$FMTE^XLFDT(%,"5Z")
 S DATE=$TR(Y,"@"," ")
 QUIT DATE
 ;"
GETMSGS(TMGRESULT,USER)  ;"
  ;"Purpose: Get one user's messages
  
  QUIT
  ;"
CPRSUSR(TMGRESULT,MGRNAME)  ;"RPC: TMG GET USER FROM MGR NAME   
  ;"This RPC will convert a user's MESSENGER NAME to their CPRS equavalant
  ;"This function needs to be properly fleshed out to be more robust, but due
  ;"  to time constraints I am hard coding values for now. I would imagine
  ;"  the best way to take care of this would be to add a new field
  ;"  to the NEW PERSON file to hold the Messenger name
  SET TMGRESULT="-1^CANNOT DETERMINE THE CPRS USER TO ASSIGN AS ADDL SIGNER. NO ADDL SIGNER CAN BE ADDED."
  SET MGRNAME=$$UP^XLFSTR(MGRNAME)
  IF MGRNAME="EDDIE" SET TMGRESULT="150^Hagood,Eddie L^- System Manager"
  IF MGRNAME="TAMMY" SET TMGRESULT="123^Hensley,Tammy G"
  IF MGRNAME="LINDSEY" SET TMGRESULT="1053^Garst,Lindsey"
  IF MGRNAME="SABRINA" SET TMGRESULT="259^Shipley,Sabrina^- CMA"
  IF MGRNAME="DRKEVIN" SET TMGRESULT="168^Toppenberg,Kevin S^- MD"
  IF MGRNAME="DRDEE" SET TMGRESULT="83^Toppenberg,Marcia Dee^- MD"
  QUIT
  ;"
  ;"===Code for browsing messenger logs ==============================
GETMSGLOGLST(LST)  ;"Get list of log files.
  ;"Input: LST -- PASS BY REFERENCE, an OUT PARAMETER.  Format
  ;"         LST("RAW",#,"FNAME")=<FILE_NAME_WITH_PATH>
  ;"         LST("DT",<FMDT>)=<FILE_NAME_WITH_PATH>
  ;"         LST("YR",<YEAR>,<MONTH>,<DAY>)=<FILE_NAME_WITH_PATH>
  ;"         LST("YR",<YEAR>,<MONTH>,<DAY>,"FMDT")=<FMDT OF FILE>
  ;"
  NEW PATH SET PATH=$$NETMESSLOC()  ;"Network folder path
  ;"NEW OPTION SET OPTION("MATCH","message*.log")=""
  ;"SET OPTION("NO UP")=1
  ;"SET OPTION("NO DIRS")=1
  ;"NEW LST DO LOADDIR^TMGIOUT2($NAME(LST("RAW")),PATH,.OPTION)  <-- SLOW!
  NEW OPTION SET OPTION("MATCH")="message*.log"
  NEW TMP DO DIR2^TMGKERNL(PATH,.TMP,.OPTION)
  MERGE LST("RAW")=TMP 
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(LST("RAW",IDX)) QUIT:IDX'>0  DO
  . ;"NEW FNAME SET FNAME=$ORDER(LST("RAW",IDX,""))
  . NEW FPNAME SET FPNAME=$GET(LST("RAW",IDX,"FNAME")) QUIT:FPNAME=""
  . NEW FMDT,EDT SET EDT=$PIECE($PIECE(FPNAME,"message",2),".",1)
  . SET %DT="P",X=EDT DO ^%DT SET FMDT=Y 
  . SET LST("DT",FMDT)=FPNAME
  . NEW YR,MONTH,DAY 
  . DO SPLITDT^TMGDATE(FMDT,.YR,.MONTH,.DAY)
  . SET LST("YR",YR,MONTH,DAY)=FPNAME
  . SET LST("YR",YR,MONTH,DAY,"FMDT")=FMDT
  QUIT
  ;
GETSELLST(LST,OUTLST,YR,MONTH,DAY)  ;"Get list of files based on selected month and years.  
  ;"Input: LST -- PASS BY REFERENCE.  Array as created by GETMSGLOGLST
  ;"         LST("RAW",#,"FNAME")=<FILE_NAME_WITH_PATH>
  ;"         LST("DT",<FMDT>)=<FILE_NAME_WITH_PATH>
  ;"         LST("YR",<YEAR>,<MONTH>,<DAY>)=<FILE_NAME_WITH_PATH>
  ;"       OUTLST -- PASS BY REFERENCE, AN OUT PARAMETER.  FORMAT:
  ;"          OUTLST(IDX,"FNAME")=<FILE NAME WITH PATH>
  ;"       YR -- # of year, or "ALLYRS"
  ;"       MONTH -- # of month, or "ALLMONTHS"
  ;"       DAY -- # of day in month, or "ALLDAYS"
  NEW IDX SET IDX=1
  KILL OUTLST
  IF YR="ALLYRS" DO  GOTO GSLDN
  . MERGE OUTLST=LST("RAW")
  IF MONTH="ALLMONTHS" DO  GOTO GSLDN
  . NEW AMONTH SET AMONTH=0
  . FOR  SET AMONTH=$ORDER(LST("YR",YR,AMONTH)) QUIT:AMONTH'>0  DO
  . . NEW ADAY SET ADAY=0
  . . FOR  SET ADAY=$ORDER(LST("YR",YR,AMONTH,ADAY)) QUIT:ADAY'>0  DO
  . . . NEW FPNAME SET FPNAME=$GET(LST("YR",YR,AMONTH,ADAY)) QUIT:FPNAME=""
  . . . SET OUTLST(IDX,"FNAME")=FPNAME,IDX=IDX+1
  IF DAY="ALLDAYS" DO  GOTO GSLDN
  . NEW ADAY SET ADAY=0
  . FOR  SET ADAY=$ORDER(LST("YR",YR,MONTH,ADAY)) QUIT:ADAY'>0  DO
  . . NEW FPNAME SET FPNAME=$GET(LST("YR",YR,MONTH,ADAY)) QUIT:FPNAME=""
  . . SET OUTLST(IDX,"FNAME")=FPNAME,IDX=IDX+1  
  NEW FPNAME SET FPNAME=$GET(LST("YR",YR,MONTH,DAY)) 
  IF FPNAME'="" SET OUTLST(IDX,"FNAME")=FPNAME,IDX=IDX+1  
GSLDN ;
  QUIT
  ;
PROCESSLOGS(OUT,LST) ;"Get all log files and process them
  ;"Input: OUT -- PASS BY REFERENCE, an OUT PARAMETER.  Format:
  ;"
  ;"       LST -- PASS BY REFERENCE.  Format:
  ;"          LST(#,"FNAME")=<FILE NAME WITH PATH>
  ;"Result: none
  NEW STARTH SET STARTH=$H
  NEW MAX SET MAX=$ORDER(LST(""),-1)  
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(LST(IDX)) QUIT:IDX'>0  DO
  . NEW FPNAME SET FPNAME=$GET(LST(IDX,"FNAME")) QUIT:FPNAME=""
  . DO PARSE1LOG(.OUT,FPNAME)
  . IF IDX#5=0 DO
  . . DO PROGBAR^TMGUSRI2(IDX,"%",1,MAX,60,STARTH)  
  QUIT
  ;  
PARSE1LOG(OUT,FPNAME) ;"Load and parse one .log file.  
  ;
  NEW OPTION SET OPTION("LINE-TERM")=$CHAR(13)
  NEW ARR DO HFS2ARFP^TMGIOUT3(FPNAME,"ARR",.OPTION)
  NEW PROCESSED
  NEW DONE SET DONE=0
  FOR  QUIT:DONE  DO
  . NEW RECARR DO GET1REC(.RECARR,.ARR,.PROCESSED)
  . SET DONE=($DATA(ARR)=0) QUIT:DONE
  . IF $DATA(RECARR)=0 DO  QUIT
  . . ;"NOTHING
  . NEW RESULT,TEMP SET RESULT=$$PARSE1REC(.TEMP,.RECARR)
  . IF RESULT'>0 DO  QUIT
  . . NEW IDX SET IDX=$ORDER(OUT("ERR",""),-1)+1
  . . SET OUT("ERR",IDX)=RESULT
  . DO MERGEREC(.OUT,.TEMP)
  QUIT
  ;
MERGEREC(ARR,ONEREC) ;
  ;"Input: ARR -- PASS BY REFERENCE, an IN and OUT parameter.  Format:
  ;"           ARR("BY USER",<NAME>,FMDT,#)=<TEXT>   <-- all messages to and from this user, i.e. from multiple users
  ;"           ARR("BY PARTY",<NAME1;NAME2>,FMDT,#)=<TEXT>   <-- all messages BETWEEN these two users
  ;"           ARR("ALL",FMDT,#)=<TEXT>  <-- all messages, but NOT system info messages.  
  ;"       ONEREC -- PASS BY REFERENCE.  Format:
  ;"           ONEREC("DT")=<EXTERNAL DATE & TIME>
  ;"           ONEREC("FMDT")=<FMDT>
  ;"           ONEREC("TO")=<NAME>
  ;"           ONEREC("FROM")=<NAME>
  ;"           ONEREC("PARTIES",<ANAME>)=""  <-- SHOULD HAVE 2 NAMES
  ;"           ONEREC("SYSTEM INFO")=1  <-- optional
  ;"           ONEREC("TEXT",#)=<LINE OF TEXT>
  ;"Result: 1^OK, or -1^Err Msg
  ;  
  NEW RESULT SET RESULT="1^OK"
  IF $GET(ONEREC("SYSTEM INFO"))=1 GOTO MRDN  ;"Ignore system messages for now.  
  NEW FMDT SET FMDT=$GET(ONEREC("FMDT"))  ;"note: Error should have already been triggered upstream if FMDT not set.
  NEW TO SET TO=$GET(ONEREC("TO"))
  NEW FROM SET FROM=$GET(ONEREC("FROM"))
  NEW PARTIES MERGE PARTIES=ONEREC("PARTIES")
  NEW PARTY1,PARTY2,PARTYNAME SET PARTY1=$ORDER(PARTIES("")),PARTY2=$ORDER(PARTIES(PARTY1)),PARTYNAME=PARTY1_";"_PARTY2
  NEW IDX,WHO,IDX2 FOR IDX=1:1:4 DO
  . NEW TEMP MERGE TEMP=ONEREC KILL TEMP("TEXT")
  . IF IDX=1 DO  QUIT
  . . MERGE ARR("BY USER",TO,FMDT)=ONEREC("TEXT")
  . . SET ARR("BY USER",TO,FMDT,.01)="FROM: "_FROM
  . . MERGE ARR("BY USER",TO,FMDT,"META")=TEMP
  . ELSE  IF IDX=2 DO  QUIT
  . . MERGE ARR("BY USER",FROM,FMDT)=ONEREC("TEXT")
  . . SET ARR("BY USER",FROM,FMDT,.01)="TO: "_TO
  . . MERGE ARR("BY USER",FROM,FMDT,"META")=TEMP
  . ELSE  IF IDX=3 DO  QUIT
  . . MERGE ARR("BY PARTY",PARTYNAME,FMDT)=ONEREC("TEXT")  
  . . SET ARR("BY PARTY",PARTYNAME,FMDT,.01)="PARTY: "_PARTYNAME
  . . MERGE ARR("BY PARTY",PARTYNAME,FMDT,"META")=TEMP
  . ELSE  IF IDX=4 DO  QUIT
  . . MERGE ARR("ALL",FMDT)=ONEREC("TEXT")
  . . SET ARR("ALL",FMDT,.01)="PARTY: "_PARTYNAME
  . . MERGE ARR("ALL",FMDT,"META")=TEMP
MRDN ;  
  QUIT RESULT
  ;
GET1REC(OUT,ARR,PROCESSED) ;"Get one record, with horizontal line dividing records
  NEW DONE SET DONE=0
  NEW IDX,JDX SET (IDX,JDX)=0
  FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:(IDX'>0)!DONE  DO
  . NEW LINE SET LINE=$GET(ARR(IDX))
  . SET PROCESSED(IDX)=LINE
  . KILL ARR(IDX) 
  . IF LINE="-------------------------------------------------------------------" SET DONE=1 QUIT
  . SET JDX=JDX+1,OUT(JDX)=LINE
  QUIT
  ;
PARSE1REC(OUT,ARR) ;"Parse one record from messanger log file.    
  ;"OUT format:
  ;"    OUT("DT")=<EXTERNAL DATE & TIME>
  ;"    OUT("FMDT")=<FMDT>
  ;"    OUT("TO")=<NAME>
  ;"    OUT("FROM")=<NAME>
  ;"    OUT("PARTIES",<NAME>)=""
  ;"    OUT("SYSTEM INFO")=1  <-- optional
  ;"    OUT("TEXT",#)=<LINE OF TEXT>
  ;"Result: 1^OK, or -1^Error
  NEW RESULT SET RESULT="1^OK"
  NEW TO,FROM
  NEW JDX SET JDX=1
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:IDX'>0  DO
  . NEW LINE SET LINE=$GET(ARR(IDX)) QUIT:LINE=""
  . IF LINE["SYSTEM INFORMATION" DO  QUIT
  . . SET OUT("SYSTEM INFO")=1
  . IF (LINE["TO:")&(LINE["FROM") DO  QUIT
  . . SET TO=$PIECE($PIECE(LINE,"TO:",2),"-",1)
  . . SET FROM=$PIECE($PIECE(LINE,"FROM:",2),"->",1)
  . IF $EXTRACT(LINE,1)=$CHAR(9) DO
  . . NEW ADT SET ADT=$PIECE($PIECE(LINE,"[",2),"]",1)
  . . SET LINE=$PIECE(LINE,"]",2)
  . . SET OUT("DT")=ADT
  . . SET ADT=$TRANSLATE(ADT," ","@")
  . . SET %DT="PTS",X=ADT DO ^%DT 
  . . SET OUT("FMDT")=Y
  . IF $GET(OUT("SYSTEM INFO"))=1,LINE["Displaying Message From" DO
  . . SET TO=$PIECE(LINE,"Displaying Message",1) SET TO=$$TRIM^XLFSTR(TO)
  . . SET FROM=$PIECE(LINE,"Message From",2) SET FROM=$$TRIM^XLFSTR(FROM)
  . SET OUT("TEXT",JDX)=LINE,JDX=JDX+1
  IF $DATA(TO) DO
  . SET OUT("PARTIES",TO)=""
  . SET OUT("TO")=TO
  IF $DATA(FROM) DO
  . SET OUT("PARTIES",FROM)=""
  . SET OUT("FROM")=FROM
  IF $DATA(OUT("FMDT"))=0 DO
  . SET RESULT="-1^FMDT of message not found"
  IF $GET(OUT("SYSTEM INFO"))=0,($DATA(OUT("TO"))=0)!($DATA(OUT("FROM"))=0) SET RESULT="-1^To/From not found"
  QUIT RESULT
  ;
BRWINYR(LST,YR) ;"Pick in Year  
  NEW MENU,IDX,USRPICK,MONTH
MY1 ;
  SET IDX=0
  IF YR="ALLYRS" SET MONTH="ALLMONTHS" GOTO MY2
  KILL MENU SET MENU(IDX)="Select Option For Browsing Messenger logs. "
  SET MONTH=0
  FOR  SET MONTH=$ORDER(LST("YR",YR,MONTH)) QUIT:MONTH'>0  DO
  . SET IDX=IDX+1,MENU(IDX)=$$MONTHNAME^TMGDATE(MONTH)_", "_YR_$CHAR(9)_MONTH
  SET IDX=IDX+1,MENU(IDX)="ALL months"_$CHAR(9)_"ALLMONTHS"
  SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
  IF USRPICK="^" GOTO MY1DN
  IF (+USRPICK'=USRPICK)&(USRPICK'="ALLMONTHS") GOTO MY1
  SET MONTH=USRPICK
MY2 ;
  DO BRWINMON(.LST,YR,MONTH)
  GOTO MY1
MY1DN  ;
  QUIT
  
BRWINMON(LST,YR,MONTH) ;"Pick in Year, Month  
  NEW MENU,IDX,USRPICK,DAY,SELLST,DATA
MM1 ;
  SET IDX=0
  IF YR="ALLYRS" SET DAY="ALLDAYS" GOTO MM2
  KILL MENU SET MENU(IDX)="Select Option For Browsing Messenger logs. "
  SET DAY=0
  FOR  SET DAY=$ORDER(LST("YR",YR,MONTH,DAY)) QUIT:DAY'>0  DO
  . NEW FMDT SET FMDT=$GET(LST("YR",YR,MONTH,DAY,"FMDT"))
  . SET IDX=IDX+1,MENU(IDX)="Browse "_$$MONTHNAME^TMGDATE(MONTH)_" "_DAY_", "_YR_" ("_$$DOW^XLFDT(FMDT)_")"_$CHAR(9)_DAY
  SET IDX=IDX+1,MENU(IDX)="ALL days"_$CHAR(9)_"ALLDAYS"
  SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
  IF USRPICK="^" GOTO MM1DN
  IF (+USRPICK'=USRPICK)&(USRPICK'="ALLDAYS") GOTO MM1
  SET DAY=USRPICK
MM2 ;
  DO GETSELLST(.LST,.SELLST,YR,MONTH,DAY)
  KILL DATA DO PROCESSLOGS(.DATA,.SELLST) 
  DO BRWWHO(.DATA)
  GOTO MM1
MM1DN  ;
  QUIT
  
BRWWHO(DATA) ;"Pick who to browse by  
  ;"Input: DATA -- PASS BY REFERENCE, an IN and OUT parameter.  Format:
  ;"          **NOTE**: At this point, DATA contains only those records selected before, by year, month, day (or ALL).
  ;"           DATA("BY USER",<NAME>,FMDT)=<RECORD ARRAY>   <-- all messages to and from this user, i.e. from multiple users
  ;"           DATA("BY PARTY",<NAME1;NAME2>,FMDT)=<RECORD ARRAY>   <-- all messages BETWEEN these two users
  ;"           DATA("ALL",FMDT)=<RECORD ARRAY>  <-- all messages, but NOT system info messages.  
  ;"           <REC ARRAY> format:
  ;"               <REC ARRAY>("DT")=<EXTERNAL DATE & TIME>
  ;"               <REC ARRAY>("FMDT")=<FMDT>
  ;"               <REC ARRAY>("TO")=<NAME>
  ;"               <REC ARRAY>("FROM")=<NAME>
  ;"               <REC ARRAY>("PARTIES",<ANAME>)=""  <-- SHOULD HAVE 2 NAMES
  ;"               <REC ARRAY>("SYSTEM INFO")=1  <-- optional
  ;"               <REC ARRAY>(#)=<TEXT LINE>
  ;                   
  NEW MENU,IDX,USRPICK
MW1 ;
  SET IDX=0  
  KILL MENU SET MENU(IDX)="Select Option For Browsing Messenger logs. "
  IF $DATA(DATA("BY USER")) SET IDX=IDX+1,MENU(IDX)="By User"_$CHAR(9)_"USER"
  IF $DATA(DATA("BY PARTY")) SET IDX=IDX+1,MENU(IDX)="By Parties"_$CHAR(9)_"PARTIES"
  IF $DATA(DATA("ALL")) SET IDX=IDX+1,MENU(IDX)="All"_$CHAR(9)_"ALL"
  SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
  IF USRPICK="^" GOTO MW1DN  
  IF USRPICK="USER" DO  GOTO MW1
  . DO BRWUSERS(.DATA)
  IF USRPICK="PARTIES" DO  GOTO MW1
  . DO BRWPARTIES(.DATA)
  IF USRPICK="ALL" DO  GOTO MW1
  . DO BRWALL(.DATA)
  GOTO MW1
MW1DN  ;
  QUIT
  ;
BRWUSERS(DATA) ;"Browse by users  
  NEW MENU,IDX,USRPICK
  NEW REF SET REF=""
MU1 ;
  SET IDX=0  
  KILL MENU SET MENU(IDX)="Select Option For Browsing Messenger logs. "
  NEW AUSER SET AUSER=""
  FOR  SET AUSER=$ORDER(DATA("BY USER",AUSER)) QUIT:AUSER=""  DO
  . SET IDX=IDX+1,MENU(IDX)=AUSER_$CHAR(9)_AUSER
  SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
  IF USRPICK="^" GOTO MU1DN
  SET REF=$NAME(DATA("BY USER",USRPICK))
  IF $DATA(@REF)=0 GOTO MU1
  DO BRWREF(.DATA,REF)
  GOTO MW1
MU1DN  ;
  QUIT
  ;
BRWPARTIES(DATA) ;"Browse by parties  
  NEW MENU,IDX,USRPICK
  NEW REF SET REF=""
MP1 ;
  SET IDX=0  
  KILL MENU SET MENU(IDX)="Select Option For Browsing Messenger logs. "
  NEW APARTY SET APARTY=""
  FOR  SET APARTY=$ORDER(DATA("BY PARTY",APARTY)) QUIT:APARTY=""  DO
  . SET IDX=IDX+1,MENU(IDX)=$PIECE(APARTY,";",1)_" <--> "_$PIECE(APARTY,";",2)_$CHAR(9)_APARTY
  SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
  IF USRPICK="^" GOTO MP1DN
  SET REF=$NAME(DATA("BY PARTY",USRPICK))
  IF $DATA(@REF)=0 GOTO MP1
  DO BRWREF(.DATA,REF)
  GOTO MP1
MP1DN  ;
  QUIT
  ;
BRWALL(DATA) ;"Browse all  
  NEW REF SET REF=$NAME(DATA("ALL"))
  DO BRWREF(.DATA,REF)
  QUIT
  ;
BRWREF(DATA,REF) ;
  NEW OUT,IDX,JDX SET JDX=1
  NEW LASTDT SET LASTDT=0
  NEW ADT SET ADT=0
  FOR  SET ADT=$ORDER(@REF@(ADT)) QUIT:ADT'>0  DO
  . IF (ADT\1)'=(LASTDT\1) DO
  . . SET LASTDT=ADT
  . . SET OUT(JDX)="--- "_$$FMTE^XLFDT(ADT\1)_"-----------------------------",JDX=JDX+1
  . NEW TEMP MERGE TEMP=@REF@(ADT)
  . NEW ARR DO REC2ARR(.TEMP,.ARR,(REF["""ALL"""))
  . SET IDX=0
  . FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:IDX'>0  DO
  . . SET OUT(JDX)=$GET(ARR(IDX)),JDX=JDX+1  
  ;"Now output the display.
  ;"Later can choose to put this to text editor
  WRITE !,"View in Editor " SET %=1 DO YN^DICN WRITE !
  IF %=-1 QUIT
  IF %=1 DO
  . DO EDITARRAY^TMGKERNL(.OUT,"joe")
  ELSE  DO
  . FOR  SET IDX=$ORDER(OUT(IDX)) QUIT:IDX'>0  DO
  . . WRITE $GET(OUT(IDX)),!  
  . DO PRESS2GO^TMGUSRI2
  QUIT
  ;
REC2ARR(ARR,OUT,ISALL) ;
  NEW TABSTR SET TABSTR="     "
  NEW JDX SET JDX=1
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:IDX'>0  DO
  . NEW EDT SET EDT=$GET(ARR("META","DT"))
  . NEW FMDT SET FMDT=$GET(ARR("META","FMDT"))
  . NEW LINE SET LINE=$GET(ARR(IDX))
  . IF IDX=.01 DO
  . . IF LINE["PARTY:" DO
  . . . SET LINE=$GET(ARR("META","FROM"))_": "
  . . . IF ISALL SET LINE=LINE_" [to "_$GET(ARR("META","TO"))_"]"
  . . SET OUT(JDX)=$PIECE($$FMTE^XLFDT(FMDT,"2MP")," ",2),JDX=JDX+1
  . . ;"SET OUT(JDX)=$PIECE(EDT," ",2),JDX=JDX+1
  . . ;"SET LINE=LINE_"           ("_$PIECE(EDT," ",2)_")"
  . ELSE  SET LINE=TABSTR_LINE
  . SET OUT(JDX)=LINE,JDX=JDX+1
  SET OUT(JDX)="",JDX=JDX+1
  QUIT
  ;
BROWSELOGS ;"
  NEW LST DO GETMSGLOGLST(.LST)  ;"Get list of log files.
  ;"Returns: LST -- Format:
  ;"         LST("RAW",#,"FNAME")=<FILE_NAME_WITH_PATH>
  ;"         LST("DT",<FMDT>)=<FILE_NAME_WITH_PATH>
  ;"         LST("YR",<YEAR>,<MONTH>,<DAY>)=<FILE_NAME_WITH_PATH>
  NEW MENU,IDX,USRPICK,YR
ML0 ;                    
  SET IDX=0  
  KILL MENU SET MENU(IDX)="Select Option For Browsing Messenger logs. Which Year?"
  SET YR=0 FOR  SET YR=$ORDER(LST("YR",YR)) QUIT:YR'>0  DO
  . SET IDX=IDX+1,MENU(IDX)="Browse "_YR_$CHAR(9)_YR
  SET IDX=IDX+1,MENU(IDX)="Browse ALL Years"_$CHAR(9)_"ALLYRS"
  ;"I could add date RANGE later.  
  SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
  IF USRPICK="^" GOTO ML0DN
  IF (+USRPICK'=USRPICK)&(USRPICK'="ALLYRS") GOTO ML0
  DO BRWINYR(.LST,USRPICK) 
  GOTO ML0
ML0DN  ;
  QUIT
  ;
