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
  SET TMGRESULT="-1^CANNOT DETERMINE THE CPRS USER TO ASSIGN AS ADDL SIGNER."
  SET MGRNAME=$$UP^XLFSTR(MGRNAME)
  IF MGRNAME="EDDIE" SET TMGRESULT="150^Hagood,Eddie L^- System Manager"
  IF MGRNAME="TAMMY" SET TMGRESULT="123^Hensley,Tammy G"
  IF MGRNAME="LINDSEY" SET TMGRESULT="1053^Garst,Lindsey"
  IF MGRNAME="SABRINA" SET TMGRESULT="259^Shipley,Sabrina^- CMA"
  IF MGRNAME="DRKEVIN" SET TMGRESULT="168^Toppenberg,Kevin S^- MD"
  IF MGRNAME="DRDEE" SET TMGRESULT="83^Toppenberg,Marcia Dee^- MD"
  QUIT
  ;"