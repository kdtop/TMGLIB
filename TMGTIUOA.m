TMGTIUOA ;TMG/kst-TIU OBJECTS ; 03/30/15
         ;;1.0;TMG-LIB;**1,17**;03/30/15
 ;"
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
 ;"=======================================================================
 ;"PUBLIC FUNCTIONS
 ;"=======================================================================
 ;"$$GETPTBP(TMGDFN) --Return PT'S BP
 ;"$$GETPTWT(TMGDFN) --Return pt's weight
 ;"
GETPTBP(TMGDFN)  ;"Return patient's blood pressure readings
 ;"Purpose: Return patient's BP readings as a TIU Object
 ;"
 NEW TMGRESULT SET TMGRESULT=$$TREND^TMGGMRV1(TMGDFN,"T","BP",4," <- ")
 QUIT TMGRESULT
 ;"
GETPULSE(TMGDFN)  ;"Return patient's pulse readings
 ;"Purpose: Return patient's pulse readings as a TIU Object
 ;"
 NEW TMGRESULT SET TMGRESULT=$$TREND^TMGGMRV1(TMGDFN,"T","P",4," <- ")
 QUIT TMGRESULT
 ;"
GETPOX(TMGDFN)  ;"Return patient's pulse oxymetry
 ;"Purpose: Return patient's POx readings as a TIU Object
 ;"
 NEW TMGRESULT SET TMGRESULT=$$TREND^TMGGMRV1(TMGDFN,"T","PO2",4," <- ")
 QUIT TMGRESULT
 ;"
GETPTWT(TMGDFN,TIU)  ;"Return patient's wt trend
 ;"Purpose: Return patient's weight trend as a TIU Object
 ;"
 NEW TMGRESULT 
 SET TMGRESULT=$$GETTIUOJ^TMGTIUOJ(.TMGDFN,"TMG PATIENT WEIGHT")
 QUIT TMGRESULT
 ;" 
IMGNOTES(TMGRESULT,BDATE,EDATE)   ;
 ;"SET TMGRESULT(0)="NAME^DATE^NOTE TITLE^STATUS^AUTHOR"
 NEW IDX SET IDX=0
 NEW %DT,X,Y
 SET BDATE=$GET(BDATE)
 IF BDATE="" SET BDATE="T"
 SET X=BDATE DO ^%DT SET BDATE=$PIECE(Y,".",1)
 SET EDATE=$GET(EDATE)
 IF EDATE="" SET EDATE="T"
 SET X=EDATE DO ^%DT SET EDATE=$PIECE(Y,".",1)_".999999"
 NEW DT,DONE,IEN,DOCIEN,TITLE,ARRAY
 SET DT=BDATE,DONE=0
 FOR  SET DT=$ORDER(^TIU(8925,"F",DT)) QUIT:(DT'>0)!(DONE=1)  DO
 . IF DT>EDATE SET DONE=1 QUIT
 . SET IEN=0
 . FOR  SET IEN=$ORDER(^TIU(8925,"F",DT,IEN)) QUIT:IEN'>0  DO
 . . NEW ZN SET ZN=$GET(^TIU(8925,IEN,0))
 . . SET DOCIEN=$PIECE(ZN,"^",1),TITLE=$$UP^XLFSTR($PIECE($GET(^TIU(8925.1,DOCIEN,0)),"^",1))
 . . IF (TITLE'["HOSPITAL")&(TITLE'["IMAGE") QUIT
 . . IF TITLE["PMH/ROS" QUIT
 . . IF TITLE["HIPAA" QUIT
 . . NEW NAME,DFN,STATUS,AUTHOR,DATE
 . . ;"GET PT NAME
 . . SET DFN=+$PIECE(ZN,"^",2)
 . . IF DFN'>0 QUIT
 . . SET NAME=$PIECE($GET(^DPT(DFN,0)),"^",1)
 . . ;"GET STATUS
 . . SET STATUS=$PIECE(ZN,"^",5)
 . . SET STATUS=$PIECE($GET(^TIU(8925.6,STATUS,0)),"^",1)
 . . ;"GET DATE
 . . NEW Y 
 . . S Y=$PIECE(ZN,"^",7) D DD^%DT 
 . . SET DATE=$PIECE(Y,"@",1)
 . . ;"GET AUTHOR
 . . SET AUTHOR=+$PIECE($GET(^TIU(8925,DOCIEN,12)),"^",2)
 . . SET AUTHOR=$PIECE($GET(^VA(200,AUTHOR,0)),"^",1)
 . . ;"STORE RESULTS
 . . SET ARRAY(NAME,DATE,IEN)=TITLE_"^"_AUTHOR_"^"_STATUS
 . . ;"SET TMGRESULT(IDX)=DATE_"-"_NAME_"-"_TITLE_"("_DATE_")"_"-"_STATUS_"-"_AUTHOR_"^"_IEN
 . . ;"SET IDX=IDX+1
 NEW NAME SET NAME=""
 SET IDX=0
 FOR  SET NAME=$ORDER(ARRAY(NAME)) QUIT:NAME=""  DO
 . NEW DATE SET DATE=""
 . FOR  SET DATE=$ORDER(ARRAY(NAME,DATE)) QUIT:DATE=""  DO
 . . NEW IEN SET IEN=0
 . . FOR  SET IEN=$ORDER(ARRAY(NAME,DATE,IEN)) QUIT:IEN'>0  DO
 . . . NEW DATA SET DATA=$GET(ARRAY(NAME,DATE,IEN))
 . . . SET TMGRESULT(IDX)=NAME_"^"_DATE_"^"_DATA_"^"_IEN
 . . . SET IDX=IDX+1 
 QUIT
 ;"
LASTSEEN(TMGRESULT,DFN,DUZ)  ;"
 ;"Return the date last seen by the provider sent
 SET TMGRESULT="Not seen yet"
 NEW DATE SET DATE=9999999
 NEW FOUND SET FOUND=0
 FOR  SET DATE=$O(^TIU(8925,"ZTMGPTDT",DFN,DATE),-1)  QUIT:(DATE'>0)!(FOUND=1)  DO
 . NEW IEN SET IEN=0
 . FOR  SET IEN=$O(^TIU(8925,"ZTMGPTDT",DFN,DATE,IEN)) QUIT:IEN'>0  DO
 . . NEW NOTETYPE SET NOTETYPE=$P($G(^TIU(8925,IEN,0)),"^",1)
 . . NEW AUTHOR SET AUTHOR=$P($G(^TIU(8925,IEN,12)),"^",2)
 . . IF AUTHOR'=DUZ QUIT
 . . NEW HILIGHT SET HILIGHT=$P($G(^TIU(8925.1,NOTETYPE,"TMGH")),"^",1)
 . . IF HILIGHT'="Y" QUIT
 . . SET FOUND=1
 . . SET TMGRESULT=$$EXTDATE^TMGDATE($P(DATE,".",1),1)
 QUIT
 ;"

 QUIT 
 ;"