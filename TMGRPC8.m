TMGRPC8 ;TMG/kst/RPC Functions  ;08/28/12, 2/2/14, 3/24/21
         ;;1.0;TMG-LIB;**1**;08/28/12
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;
NOTIFY(RESULT,LIST)   ;
        ;"This function saves the patient notifications from the Scanner program
        ;"INPUT   LIST is the array of patient data
        ;"        LIST[#] = DFN^SEQUELNUM^NOTIFY (YES/NO)^TYPE
        ;"             DFN used for lookup, IF DFN fails SequelNum used
        ;"             TYPE should be 1 letter to match SET for field .03
        ;"RESULT: RESULT equals 1 for success or -1^ERROR for failure
        ;"MERGE LIST=^TMG("TMP","NOTIFY^TMGRPC8")
        NEW TMGFDA,PTNUM
        NEW TMGIEN,TMGMSG
        NEW TMGERROR
        NEW TMGDFN,NOTIFIED,SEQUELNUM,TYPE
        NEW TMGRESULTARRAY SET TMGRESULTARRAY=1
        SET RESULT(0)=1
        SET PTNUM=0
        FOR  SET PTNUM=$ORDER(LIST(PTNUM)) QUIT:+PTNUM<1  DO
        . SET TMGDFN=+$PIECE(LIST(PTNUM),"^",1)
        . SET SEQUELNUM=+$PIECE(LIST(PTNUM),"^",2)
        . IF TMGDFN<1 SET TMGDFN=$$GETDFN(SEQUELNUM)
        . IF TMGDFN<1 QUIT
        . SET TYPE="T"
        . SET NOTIFIED=$PIECE(LIST(PTNUM),"^",3)
        . SET TMGERROR=0
        . IF $DATA(^TMG(22716,TMGDFN))=0 DO  QUIT:TMGERROR
        . . SET TMGFDA(22716,"+1,",.01)=TMGDFN
        . . SET TMGIEN(1)=TMGDFN
        . . D UPDATE^DIE(,"TMGFDA","TMGIEN","TMGMSG")
        . . IF $DATA(TMGMSG("DIERR")) DO
        . . . SET TMGRESULTARRAY(+TMGRESULTARRAY)=$$GETERRST^TMGDEBU2(.TMGMSG)
        . . . SET TMGRESULTARRAY=TMGRESULTARRAY+1
        . . . SET TMGERROR=1
        . KILL TMGFDA,TMGIEN
        . SET TMGFDA(22716.01,"+1,"_TMGDFN_",",.01)="NOW"
        . SET TMGFDA(22716.01,"+1,"_TMGDFN_",",.02)=NOTIFIED
        . SET TMGFDA(22716.01,"+1,"_TMGDFN_",",.03)=TYPE
        . D UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) DO
        . . SET TMGRESULTARRAY(+TMGRESULTARRAY)=$$GETERRST^TMGDEBU2(.TMGMSG)
        . . SET TMGRESULTARRAY=TMGRESULTARRAY+1
        . . SET TMGERROR=1
        IF $DATA(TMGRESULTARRAY(1)) DO
        . MERGE RESULT=TMGRESULTARRAY
        . SET RESULT(0)="-1^ERRORS FOUND"
        QUIT
        ;
GETDFN(SEQUELNUM)    ;
        NEW TMGDFN
        SET TMGDFN=+$ORDER(^DPT("TMGS",SEQUELNUM,""))
        QUIT TMGDFN
        ;"
TASKNOTE(TMGDFN)  ;" 
        NEW RESULT
        SET TMGDFN=+$G(TMGDFN)
        SET RESULT=$G(^DPT(TMGDFN,"TMGTASK"))
        QUIT RESULT
        ;"
