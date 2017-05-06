TMGDIS0 ;TMG/kst/Custom DIS0, non-interactive SEARCH, IF STATEMENT AND MULTIPLE COMBO'S ;5/13/10 ; 5/16/10 10:01pm, 2/2/14
     ;;1.0;TMG-LIB;**1**;01/01/06
     ;-----Prior header below -------------
     ;SFISC/GFT-SEARCH, IF STATEMENT AND MULTIPLE COMBO'S ;30JAN2005
     ;;22.0;VA FileMan;**144**;Mar 30, 1999;Build 5
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
DIS0(TMGINFO,TMGOUT,TMGBYROOT) ;
        ;"Purpose: Provide an API interface for the classic Fileman console search
        ;"Input: TMGINFO -- PASS BY REFERENCE.  This is pre-defined search terms.  Format:
        ;"           TMGINFO("FILE") -- File name or number to be used for search
        ;"           TMGINFO(1,...) -- Search condition 1 (corresponds to 'A' in console)
        ;"           TMGINFO(2,...) -- Search condition 2 (corresponds to 'B' in console)
        ;"           TMGINFO("LOGIC-IF")=(OPTIONAL) Logic string that would be normally
        ;"                                 entered at 'IF: ' prompt
        ;"                                 e.g. "1&2", or "A&B", or "AB" <--- all the same
        ;"                                 Default is logic string ANDing all search terms.
        ;"           TMGINFO("LOGIC-OR",1)=(OPTIONAL) Logic string that would be normally
        ;"                                 entered at 'OR: ' prompt
        ;"           TMGINFO("LOGIC-OR",#)=(OPTIONAL) Logic string that would be normally
        ;"                                 entered at 'OR: ' prompt.  #=2,3,4... For multiple
        ;"                                 lines of OR logic
        ;"                 NOTE: Fileman console labels search terms as "A","B","C",...
        ;"                       But the above numbering system uses "1","2","3",...
        ;"                       When entering in logic strings, one may use either letters
        ;"                       or numbers. A=1, B=2 etc.  Note that Fileman allows AB to
        ;"                       mean the same as A&B.  This is not possible with numbers.
        ;"           ...
        ;"           --DETAILS ON SEARCH CONDITION----
        ;"           TMGINFO(n,"FLD") -- The Fileman field name or number to seach in
        ;"           TMGINFO(n,"COND") -- The condition: "=,>,<,[,?,NULL"  Prefix ' or - to negate
        ;"           TMGINFO(n,"VALUE") -- the value to search for
        ;"       TMGOUT --An OUT PARAMETER.  Prior values killed.  Format:
        ;"          TMGOUT(FILENUM,IEN)=""
        ;"          TMGOUT(FILENUM,IEN)=""
        ;"          TMGOUT(FILENUM,IEN)=""
        ;"       TMGBYROOT -- (Optional)  If 1, then TMGOUT is treated as a variable NAME (root)
        ;"                             i.e. @TMGOUT@(FILENUM,IEN)=""
        ;"Globally-Scoped variables uses: O,DC,DA  (and probably others)
        ;"Results: 1 if OK, or -1^Error Message
        ;
     ;"WRITE !
     NEW TMGRESULT SET TMGRESULT=1 ;"Default to success
     NEW R,N,DL,DE,DJ  ;"WAS KILL initially
     NEW P,LOGIC,NLOG
     NEW DU
     SET O=0
     SET E=$DATA(DC(2))  ;"E>0 IF MORE THAN ONE SRCH TERM
     SET N="IF: A// "
     SET DE=$SELECT(E:"IF: ",1:N)
     NEW TMGLMODE SET TMGLMODE=1  ;"1="LOGIC-IF" 2=LOGIC-OR
     NEW TMGLORN SET TMGLORN=0  ;"Logic OR line number
     SET DL=0
     SET C=","
R    ;"WRITE !,DE
     KILL DV
     IF TMGLMODE=1 DO
     . SET X=$GET(INFO("LOGIC-IF"))
     . IF X'="" QUIT
     . NEW I SET I=0
     . FOR  SET I=$ORDER(INFO(I)) QUIT:+I'>0  SET X=X_$CHAR(I+64)
     . SET INFO("LOGIC-IF")=X
     ELSE  DO
     . SET TMGLORN=TMGLORN+1
     . SET X=$GET(INFO("LOGIC-OR",TMGLORN))
     IF X'="" GOTO R2
     ;"READ X:DTIME SET:'$T DTOUT=1 GOTO Q:X[U!'$T
     SET DV=1,DU=X
     GOTO 1:DL
     SET DQ="TYPE '^' TO EXIT"
     SET Y="^1^"
     SET DL=1
     ;"GOTO BAD:E
     IF E="" DO  GOTO TMGDONE
     . SET TMGRESULT="-1^Bad/absent logic string."
     DO ASKQ(.DC,.DV,.DU)
     GOTO L
     ;
R2   SET Y=U,P=0,DU="",D=""
     SET DL=DL+1
P    ;"PARSE LOGIC STRING
     SET LOGIC=X,LOGN=0
     FOR  QUIT:(LOGIC="")!(+TMGRESULT=-1)  DO
     . SET DV=0
     . IF +LOGIC>0 DO
     . . SET (DV,DQ)=+LOGIC
     . . SET LOGIC=$EXTRACT($LENGTH(DQ)+1,9999)
     . ELSE  DO
     . . SET DQ=$EXTRACT(LOGIC,1)
     . . SET LOGIC=$EXTRACT(LOGIC,2,9999)
     . . IF DQ?.A SET DV=$ASCII(DQ)-64
     . IF (DV>0)&($DATA(DC(DV))>0) DO  QUIT
     . . SET LOGN=LOGN+1
     . . DO ASKQ(.DC,.DV,.DU)
     . . SET TMGRESULT=$$CHK(DV)
     . IF "&+ "[DQ QUIT
     . IF ((DU="")&("'-"[DQ)) SET DU="'" QUIT
     . SET TMGRESULT="-1^Bad entry '"_DQ_"' found in logic phrase '"_X_"'"
     IF LOGN'>0 SET TMGRESULT="-1^No valid logic terms found in '"_X_"'"
     IF +TMGRESULT=-1 GOTO TMGDONE
     GOTO L
     ;
 ;"BAD  DO
 ;"     . IF DQ?."?" DO  QUIT
 ;"     . . DO BLD^DIALOG($SELECT($DATA(DC(2)):8004.2,1:8004.1))
 ;"     . . DO MSG^DIALOG("WH")   ;HELP depending on whether there is a CONDITION B
 ;"     . WRITE "   <",DQ,">??"
 ;"     WRITE !!
 ;"     KILL DJ(DL),DE(DL)
 ;"     SET DL=DL-1
 ;"     GOTO R
     ;
ASKQ(DC,DV,DU) ;"-------------
     NEW J,%,I
     SET J=DC(DV)
     SET %=J["?."" """
     SET I=J["^'"+(DU["'")#2
     IF J["W^" DO  QUIT
     . SET DV(DV)=$SELECT(I:2-%,1:%+%+1)
     . IF % SET DC(DV)=$EXTRACT(J,1,$LENGTH(J)-5)_"="""""
     IF $PIECE(J,U)[C SET DV(DV)=J?.E1",.01^".E&%+(I+%#2)
     QUIT
     ;
CHK(DV) ;Check search term
     ;"Result: 1 if OK, -1^ErrorMessage
     NEW %
     NEW RSLT SET RSLT=1 ;"Default to success
     SET %=$F(Y,U_DV)
     IF % DO  GOTO CKDN ;"Was BAD
     . SET %=$PIECE($EXTRACT(Y,%),U,1)'=DU
     . SET DQ=""""_DQ_""" AND """_$EXTRACT("'",%)_DQ_""" IS "_$PIECE("REDUNDANT^CONTRADICTORY",U,%+1)
     . SET RSLT="-1^"_DQ
     SET %=1
     SET Y=Y_DV_DU_U
     SET DU=""
     SET J=$PIECE(DC(DV),U,1)
     IF J'[C GOTO CKDN ;"WAS P
     FOR Z=2:1 IF $PIECE(J,C,Z,99)'[C SET J=$PIECE(J,C,1,Z-1)_C QUIT
     IF J=D DO
     . DO SAMEQ  ;"result in %
     . IF %=1 SET DJ(DL,DV)=DX(DV)
     SET D=J,DJ=DV
     ;"IF %>0 GOTO P
     IF %'>0 DO  GOTO CKDN
     . SET RSLT="-1^Error checking search term #"_DV
CKDN QUIT RSLT
 ;"Q    GOTO Q^DIS2
     ;
SAMEQ ;----
     IF (J<0),$PIECE(DY(-J),U,3)="" QUIT
     ;"NOTE!!!: Answer to question below FORCED TO BE 'YES' FOR NOW.  Later figure how to specify in INFO array
     ;"WRITE !?8,"CONDITION -"_$C(DV+64)_"- WILL APPLY TO THE SAME MULTIPLE AS CONDITION -"_$C(DJ+64)_"-",!?8,"...OK"
     ;"DO YN^DICN
     SET %=1 ;"FORCE 'YES' answer
     QUIT
     ;
     ;-----------------
L    SET P=O
     SET DL(DL)=Y
     SET DE="OR: "
     SET TMGLMODE=2 ;"OR mode
     FOR %=2:1 SET X=$PIECE(Y,U,%) QUIT:X=""  DO
     . SET O=O+1
     . NEW S SET S=$SELECT($DATA(DJ(DL,+X)):"  together with ",1:"   and ")
     . SET ^UTILITY($J,O,0)=$SELECT(%>2:S,O=1:"",1:" Or ")_$PIECE("not ",U,X["'")_O(+X)
     ;"WRITE:$X>18 !
     ;"WRITE "   "
     ;"FOR %=P+1:1 Q:'$DATA(^UTILITY($J,%,0))  DO
     ;". SET X=^(0)
     ;". IF $LENGTH(X)+$X>77 WRITE !?13
     ;". WRITE " "_$PIECE(X,U)
     ;". IF $PIECE(X,U,2)'="" WRITE " ("_$PIECE(X,U,2)_")"
     SET DV=0
DV   SET DV=$ORDER(DV(DV))
     IF DV="" SET DV=-1
     ;"GOTO:DV'>0 R:E,1
     IF (DV'>0)&E GOTO R  ;"Go back and ask for OR" logic phrase
     IF (DV'>0) GOTO 1
     IF $DATA(DJ(DL,DV)) GOTO DV
     SET I=$PIECE(DC(DV),U,1),D=DK,DN=0
     SET Y="DO YOU WANT THIS SEARCH SPECIFICATION TO BE CONSIDERED TRUE FOR CONDITION -"_$C(DV+64)_"-"
G    SET DN=DN+1
     SET P=$PIECE(I,C,1)
     SET I=$PIECE(I,C,2,99)
     IF P["W" GOTO W
     IF I="" GOTO DV
     IF P<0 DO  GOTO G:'$PIECE(J,U,3)
     . SET J=DY(-P)
     . SET D=+J
     . SET R=" '"_$PIECE(^DIC(D,0),U,1)_"' ENTRIES "
     ELSE  DO
     . SET D=+$PIECE(^DD(D,P,0),U,2),R=" '"_$ORDER(^DD(D,0,"NM",0))_"' MULTIPLES "
HOW  ;
     ;"NOTE!!! -- I am forcing answers to be the default ones for now.  Later figure out how to
     ;"           specify pre-defined answers in the INFO array
     ;"
     ;"WRITE !!,Y,!?8,"1) WHEN AT LEAST ONE OF THE"_R_"SATISFIES IT"
     ;"WRITE !?8,"2) WHEN ALL OF THE"_R_"SATISFY IT" SET X=2
     ;"IF DV(DV) DO
     ;". WRITE !?8,"3) WHEN ALL OF THE"_R_"SATISFY IT,",!?16,"OR WHEN THERE ARE NO"_R
     ;". SET X=3
     ;"WRITE !?4,"CHOOSE 1-"_X_": "
     IF DV(DV)>1 DO
     . ;"WRITE 3
     . SET %1=3
     ELSE  DO
     . ;"WRITE 1
     . SET %1=1
     ;"READ "// ",%:DTIME,!
     ;"SET:'$T DTOUT=1 SET:%="" %=%1
     SET %=%1  ;"//KT
     KILL %1
     ;"GOTO Q:%=U!'$T
     ;"GOTO HOW:%>X!'%
     IF %>1 DO
     . SET DE(DL,DV,DN)=%
     . SET O=O+1
     . SET ^UTILITY($J,O,0)="   for all"_R_$PIECE(", or when no"_R_"exist",U,%>2)
     GOTO G
     ;
W    IF DV(DV)-2 DO  GOTO DV
     . SET DE(DL,DV,DN)=DV(DV)
     ;"NOTE!!! I am setting the answer to the question below to the default value.
     ;"        Later figure out how to pass predefined answer in INFO array from programmer
     ;"WRITE !!,Y,!?7,"WHEN THERE IS NO '"_$PIECE(^DD(D,+P,0),U,1)_"' TEXT AT ALL"
     SET %=1
     ;"DO YN^DICN
     ;"GOTO Q:%<0
     ;"GOTO W:'%
     SET DE(DL,DV,DN)=4-%
     GOTO DV
     ;
1    KILL DX,Y ;"removed O from kill
     DO DIS1^TMGDIS1   ;"Sets TMGRESULT,  WAS GOTO ^DIS1
     GOTO TMGDONE
     ;
TMGDONE ;
     QUIT
