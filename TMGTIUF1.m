TMGTIUF1 ;TMG/kst/TIU fixes;10/30/13
       ;;1.0;TMG-LIB;**1**;10/30/13
       ;
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
RMDUSCNS  ;
        ;"Purpose: To scan the TIU Document file's (8925) "B" Index for the following
        ;"         document types, provided that they are empty and unsigned (status 5)
        ;"         --ROS Image file types -- 1424
        ;"         --Acute visit -- 1399
        ;"         --HIPAA AGREEMENT -- 34
        ;"Input: none
        ;"Output: none
        ;"Result:none
        NEW STATUS
        NEW COUNT SET COUNT=+$GET(^TMG("TMP","RMDUSCNS",0))
        NEW DOCTYPEIEN FOR DOCTYPEIEN=1424,1399,34,1472 DO
        . NEW DOCNAME SET DOCNAME=$PIECE($GET(^TIU(8925.1,DOCTYPEIEN,0)),"^",1)
        . WRITE "CHECKING FOR TYPE '",DOCNAME,"' that are empty and unsigned.",!
        . NEW IEN SET IEN=0
        . FOR  SET IEN=$ORDER(^TIU(8925,"B",DOCTYPEIEN,IEN)) QUIT:+IEN'>0  DO
        . . SET STATUS=$PIECE($GET(^TIU(8925,IEN,0)),"^",5)
        . . IF STATUS'=5 QUIT
        . . IF $DATA(^TIU(8925,IEN,"TEXT")) DO  QUIT
        . . . WRITE DOCNAME," ",IEN," HAS TEXT -- SKIPPING",!
        . . IF $DATA(^TIU(8925,IEN,"TEMP")) DO  QUIT
        . . . WRITE DOCNAME," ",IEN," HAS TEXT -- SKIPPING",!
        . . WRITE DOCNAME," ",IEN," IS EMPTY AND UNSIGNED... DELETED",!
        . . SET ^TMG("TMP","RMDUPROS",IEN)=""
        . . NEW DIK S DIK="^TIU(8925,",DA=IEN
        . . SET COUNT=COUNT+1
        . . DO ^DIK
        SET ^TMG("TMP","RMDUSCNS",0)=COUNT
        QUIT
	;
VRFYDOCS(TMGRESULT,TIUIENARRAY) ;"
        ;"Purpose: When images are automatically uploaded to TIU Notes from
        ;"         CPRS, there are occassions when documents are created but
        ;"         for some reason they aren't completed and close to 100
        ;"         get created. We haven't been to determine the reason for
        ;"         this, since it happens so infrequently. To resolve this
        ;"         CPRS will keep track of all TIU notes created during each
        ;"         timer run and will then pass that array into this RPC.
        ;"         Then this function will, one at a time, examine each
        ;"         document. If the document has: no report text, no text in
        ;"         buffer, is unsigned, and has no images owned then it will
        ;"         be deleted.
        ;"Input: TIURESULT - output
        ;"       TIUIENARRAY - ARRAY(TIUIEN)=""
        ;"NOTE -- STORING ALL ACTIVITY IN ^TMG("TMP","VRFYDOCS^TMGTIUF1",TIUIEN)=RESULT
        ;"        THIS CAN (AND SHOULD) BE REMOVED AFTER A MONTH OR SO
        NEW TIUIEN,STATUS,COUNT,IMAGELST,ICOUNT,MAGIEN
        SET TIUIEN=0,COUNT=0
        KILL ^TMG("TMP","VRFYDOCS^TMGTIUF1","INPUT")
        MERGE ^TMG("TMP","VRFYDOCS^TMGTIUF1","INPUT")=TIUIENARRAY
        FOR  SET TIUIEN=$ORDER(TIUIENARRAY(TIUIEN)) QUIT:TIUIEN'>0  DO
        . SET STATUS=$PIECE($GET(^TIU(8925,TIUIEN,0)),"^",5)
        . IF STATUS'=5 DO  QUIT  ;"NOT UNSIGNED
        . . SET ^TMG("TMP","VRFYDOCS^TMGTIUF1",TIUIEN)="NOT DELETED - NOT UNSIGNED"
        . IF $DATA(^TIU(8925,TIUIEN,"TEXT")) DO  QUIT  ;"TEXT EXISTS FOR DOC
        . . SET ^TMG("TMP","VRFYDOCS^TMGTIUF1",TIUIEN)="NOT DELETED - TEXT EXISTS"
        . IF $DATA(^TIU(8925,TIUIEN,"TEMP")) DO  QUIT  ;"TEXT EXISTS IN BUFFER
        . . SET ^TMG("TMP","VRFYDOCS^TMGTIUF1",TIUIEN)="NOT DELETED - BUFFER EXISTS"
        . KILL IMAGELST
        . SET ICOUNT=0,MAGIEN=0
        . DO GETILST^TIUSRVPL(.IMAGELST,TIUIEN)
        . FOR  SET MAGIEN=$ORDER(IMAGELST(MAGIEN)) QUIT:MAGIEN'>0  DO
        . . SET ICOUNT=ICOUNT+1
        . IF ICOUNT>0 DO  QUIT  ;"IMAGES ARE LINKED
        . . SET ^TMG("TMP","VRFYDOCS^TMGTIUF1",TIUIEN)="NOT DELETED - "_ICOUNT_" IMAGES ARE LINKED"
        . SET ^TMG("TMP","VRFYDOCS^TMGTIUF1",TIUIEN)="DELETED"
        . NEW DIK S DIK="^TIU(8925,",DA=TIUIEN
        . SET COUNT=COUNT+1
        . DO ^DIK
        ;" elh  removed 7-9-15 IF COUNT>0 SET TMGRESULT="-1^"_COUNT_" DOCUMENTS WERE DELETED. CHECK ^TMG(TMP,VRFYDOCS,*)"
        ;"ELSE  
        SET TMGRESULT="1^NONE DELETED"
        KILL ^TMG("TMP","VRFYDOCS^TMGTIUF1","INPUT")  ;"//kt added 3/12/15
        QUIT
        ;"