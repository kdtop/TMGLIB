TMGDIS  ;TMG/kst/Custom version of DIS ;03/25/06 ; 5/19/10 1:16pm, 2/2/14
        ;;1.0;TMG-LIB;**1**;01/01/06
        ;"-------Prior header below ---------------
        ;"SFISC/GFT-GATHER SEARCH CRITERIA ;05:52 PM  27 Mar 2002
        ;";22.0;VA FileMan;**6,97**;Mar 30, 1999
        ;"Purpose: to GATHER SEARCH CRITERIA
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
        ;"------Also includes code from DIS2, with header as below.
DIS2    ;SFISC/GFT-SEARCH, TEMPLATES & COMPUTED FIELDS;4JUN2005
        ;;22.0;VA FileMan;**6,144**;Mar 30, 1999;Build 5
        ;"
        ;"NOTE: The following code was just to the point of working when I found a better
        ;"      way to DO this via the NEW LIST^DIC.  So I am going to stop work on this code.
SRCH(TMGINFO,TMGOUT) ;
        ;"Purpose: Provide an API interface for the classic Fileman console search
        ;"Input: TMGINFO -- PASS BY REFERENCE.  This is pre-defined search terms.  Format:
        ;"           TMGINFO("FILE") -- File name or number to be used for search
        ;"                          If name is supplied, will be converted to IEN^NAME
        ;"           TMGINFO(1,...) -- Search condition 1 (corresponds to 'A' in console)
        ;"           TMGINFO(2,...) -- Search condition 2 (corresponds to 'B' in console)
        ;"           TMGINFO("LOGIC-IF")=(OPTIONAL) Logic string that would be normally
        ;"                                 entered at 'IF: ' prompt
        ;"                                 e.g. "1&2", "A&B", "A+B", or "AB" <--- all the same
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
        ;"           --------------------------
        ;"           TMGINFO("SORT IEN")=MyIEN (OPTIONAL) -- If provided, then IEN must point
        ;"                  to an existing SORT TEMPLATE that will be used to store the output
        ;"                  search into.  Any preexisting data in record will be deleated.
        ;"           --------------------------
        ;"           TMGINFO("PRE-SET",  -- (Optional) PASS BY REFERENCE.  If provided, then only
        ;"                  the IEN's provided will be used for further searching.  This will
        ;"                  allow this function to be call successively, further narrowing a
        ;"                  search.  The results of a prior run can be passed back in. Format:
        ;"                  TMGINFO("PRE-SET",Filenum,IEN)=""
        ;"                  TMGINFO("PRE-SET",Filenum,IEN)=""
        ;"                  -or-
        ;"                  TMGINFO("PRE-SET","ROOT",Filenum)=NameOfVariableHoldingSet.  Var must have format:
        ;"                      Varname(IEN)=""
        ;"           --------------------------
        ;"           TMGINFO("BYROOT")=1  (Optional)  If 1, then TMGOUT is treated as a variable NAME (root)
        ;"                             i.e. @TMGOUT@(FILENUM,IEN)=""
        ;"           --------------------------
        ;"           ...
        ;"           --DETAILS ON SEARCH CONDITION----
        ;"           TMGINFO(n,"FLD") -- The Fileman field name or number to seach in
        ;"           TMGINFO(n,"COND") -- The condition: "=,>,<,[,?,NULL"  Prefix ' or - to negate
        ;"           TMGINFO(n,"VALUE") -- the value to search for
        ;"         *Alternative Syntax*
        ;"           TMGINFO(n)=Fld^Cond^Value  If this is found, it will be used to fill in fields above.
        ;"       TMGOUT --An OUT PARAMETER.  Fill with results of search.  Prior values killed.  Format:
        ;"          TMGOUT(FILENUM,IEN)=""
        ;"          TMGOUT(FILENUM,IEN)=""
        ;"       or @TMGOUT@(FILENUM,IEN)=""  IF BYROOT=1 (see above)
        ;"Results: 1 if OK, or -1^Error Message
        ;
        NEW TMGSORTT SET TMGSORTT=0 ;"Will store IEN of SORT TEMPLATE used for output
        NEW TMGRESULT SET TMGRESULT=$$PREPTMPL(.TMGINFO)
        IF +TMGRESULT=-1 GOTO SRCHDN
        NEW TMGBYROOT SET TMGBYROOT=+$GET(TMGINFO("BYROOT"))
        IF TMGSORTT'>0 DO  GOTO SRCHDN
        . SET TMGRESULT="-1^Unable to prepair a SORT template for use."
        IF +TMGRESULT=-1 GOTO SRCHDN
        NEW TMGFILE SET TMGFILE=+$GET(TMGINFO("FILE"))
        NEW ROOT
        IF $DATA(TMGINFO("PRE-SET",TMGFILE)) DO
        . SET ROOT=$GET(TMGINFO("PRE-SET","ROOT",TMGFILE)) QUIT:ROOT'=""
        . SET ROOT=$NAME(TMGINFO("PRE-SET",TMGFILE))
        ELSE  DO
        . SET ROOT=$GET(^DIC(TMGFILE,0,"GL"))
        . IF ROOT="" SET TMGRESULT="-1^Unable to get global root for file '"_TMGFILE_"'"
        . SET ROOT=$$CREF^DILF(ROOT)
        NEW DIS
        MERGE DIS=^DIBT(+TMGSORTT,"DIS")
        IF $DATA(DIS(0))=0 DO  GOTO SRCHDN
        . SET TMGRESULT="-1^Unable to find screening code in SORT template"
        NEW RSLTROOT
        IF TMGBYROOT SET RSLTROOT=TMGOUT
        ELSE  SET RSLTROOT="TMGOUT"
        KILL @RSLTROOT
        NEW D0 SET D0=0  ;"D0 is IEN used in DIS code.
        FOR  SET D0=$ORDER(@ROOT@(D0)) QUIT:(+D0'>0)  DO
        . XECUTE DIS(0)
        . IF $TEST SET @RSLTROOT@(TMGFILE,D0)=""
SRCHDN  IF TMGSORTT>0 DO
        . IF TMGSORTT=$GET(TMGINFO("SORT IEN")) QUIT  ;"Don't delete IF specified by user
        . ;"IF $$DELTEMPL^TMGDIS2(TMGSORTT)>0 QUIT
        . ;"SET TMGRESULT="-1^Unable to delete SORT TEMPLATE #"_TMGSORTT
        QUIT TMGRESULT
        ;
        ;
PREPTMPL(TMGINFO)
        ;"PURPOSE: Prepair a SORT TEMPLATE that will be used for doing the actual search.
        ;"         Note: This code used to extend into DIP* code where the actual search would be done.
        ;"               But it has been repurposed.
        ;"Input: TMGINFO -- See documentation above.
        ;"Output: TMGSORTT should be SET to the IEN of the SORT TEMPLATE that contains the searching code.
        ;"Results: 1 if OK, or -1^Message IF error
        ;
        NEW DC ;"Variable DC stores coded search values
              ;"Example:
             ;"DC(1)="14,.01^=105" <-- field 14, sub field .01 '=' IEN 105 (in pointed to file)
            ;"DC(2)="14,2^=44"    <-- field 14, sub field 2 '=' IEN 44 (in pointed to file)
           ;"
          ;"Example
         ;"DC(1)="14,-1^[""ACETA"""  <-- field 14 is a multiple, '-' --> ?  1 is field '[' ACETA
        ;"DC(2)="14,-2^[""%"""      <-- field 14 is a multiple, '-' --> ?  2 is field '[' %
        ;"
        ;"Example
        ;"DC=6
        ;"DC(1) = 14,.01^=105   <-- field 14, sub field .01 '=' IEN 105 (in pointed to file)
        ;"DC(2) = 14,-2^["%"    <-- field 14 is a multiple, '-' --> ?  2 is field '[' %
        ;"              note field 2 is a pointer, so perhaps '-' means non-exact match
        ;"DC(3) = 14,1^["1"     <-- field 14 is a multiple, 1 is field '[' ACETA
        ;"              note field 1 is free text, so perhaps '-' not needed
        ;"DC(4) = 1^=211        <-- field 1 '=' IEN 211
        ;"DC(5) = .01^["A"      <-- field .01 '[' A
        ;"Values of O with above example
        ;"O=0
        ;"O(1) = VA PRODUCT ACTIVE INGREDIENTS EQUALS 105^ACETAMINOPHEN
        ;"O(2) = VA PRODUCT UNITS CONTAINS "%"
        ;"O(3) = VA PRODUCT STRENGTH CONTAINS "1"
        ;"O(4) = DOSAGE FORM EQUALS 211^BAG
        ;"O(5) = NAME CONTAINS "A"
        NEW DIS,%ZIS
        NEW O   ;"('Oh', not 'zero')  Stores file & field names and values to search FOR
               ;"Example:
              ;"O=0
             ;"O(1) = VA PRODUCT ACTIVE INGREDIENTS CONTAINS (case-insensitive) "ACETAMINOPHEN"
            ;"O(2) = VA PRODUCT ACTIVE INGREDIENTS CONTAINS (case-insensitive) "CAFF"
           ;"O(3) = VA GENERIC NAME CONTAINS "A"
          ;"Note:
         ;"  Each node (i.e. (1),(2) etc) contains a separate search item.
        ;"
        ;"Another example
        ;"O="EQUALS"
        ;"O(1)="VA PRODUCT ACTIVE INGREDIENTS EQUALS 105^ACETAMINOPHEN"
        ;"O(2)="VA PRODUCT UNITS EQUALS 44^%"
        ;"
        ;"Note:
        ;"  In above examples,
        ;"     O(1) --> VA PRODUCT is file name, ACTIVE INGREDIENTS is .01 field
        ;"              of ACTIVE INGREDIENTS multiple
        ;"              105 is IEN of ACETAMINOPHEN
        ;"              EQUALS is chosen comparator
        ;"     O(2)--> VA PRODUCT is file name, UNITS is field 2 of ACTIVE INGREDIENTS multiple
        ;"              44 is IEN of unit '%'
        ;"              EQUALS is chosen comparator
        ;"  The value in O (e.g. 'EQUALS') is later killed, so not used in actual search.

        NEW N,P,C,I,J,Q
        NEW R  ;"stores root of file being searched
        NEW E  ;"stores field type codes (piece 2 of 0 node)
        NEW Z  ;"pointers or SET data (piece 3 of 0 note)
        NEW DIC,X,Y
        NEW DL ;"DL=indent amount from left margin.
        NEW DC ;"DC=search element i.e. 1=A,2=B,3=C etc.
        NEW DU ;"DU = field number
        NEW DA,DI,DV,DX,DY,DTOUT,DK
        NEW DICMX,DICOMP
        NEW TMGSAVX
        NEW TMGRESULT SET TMGRESULT=1  ;"Default to success
        SET DIC=1
        SET X=+$GET(TMGINFO("FILE"))
        DO ^DIC
        IF Y=-1 DO  GOTO PREPDN
        . SET TMGRESULT="-1^File '"_X_"' is not valid."
        SET DIC=+Y
        SET TMGINFO("FILE")=Y
        NEW TMGFILE SET TMGFILE=Y
        DO  ;"Parse syntax of all in one line into separate fields
        . NEW I SET I=0
        . FOR  SET I=$ORDER(INFO(I)) QUIT:(+I'>0)!(+TMGRESULT=-1)  DO
        . . NEW S SET S=$GET(INFO(I)) QUIT:S=""
        . . NEW TEMPL SET TEMPL="FLD^COND^VALUE"
        . . NEW J FOR J=1:1:3 DO
        . . . NEW LABL SET LABL=$PIECE(TEMPL,"^",J)
        . . . NEW F1 SET F1=$PIECE(S,"^",J)
        . . . IF $DATA(INFO(I,LABL)),$GET(INFO(I,LABL))'=F1 DO  QUIT
        . . . . SET TMGRESULT="-1^Conflicting "_LABL_" information for term #"_I
        . . . SET INFO(I,LABL)=F1
        . . IF +TMGRESULT'=-1 SET INFO(I)=""
        IF +TMGRESULT=-1 GOTO PREPDN
EN      ;
        IF DIC SET DIC=$G(^DIC(DIC,0,"GL"))
        IF DIC="" DO  GOTO PREPDN
        . SET TMGRESULT="-1^File '"_TMGFILE_"' is not valid."
        KILL DI,DX,DY,I,J,DL,DC,DA,DTOUT,^UTILITY($J)
        IF '$DATA(@(DIC_"0)")) DO  GOTO PREPDN
        . SET TMGRESULT="-1^File '"_TMGFILE_"' is missing its global."
        SET (R,DI,I(0))=DIC
        SET DL=1  ;"DL=indent amount from left margin.
        SET DC=1  ;"DC=search element i.e. 1=A,2=B,3=C etc.
        SET DY=999
        SET N=0
        SET Q=""""
        SET DV=""
R       ;
        ;"SET J(N) and DK<--file NUMBER, R<--file NAME
        IF +R=R DO
        . SET (J(N),DK)=R
        . SET R=""
        ELSE  DO
        . SET @("(J(N),DK)=+$PIECE("_R_"0),U,2)")
        . SET R=$PIECE(^(0),U)
        ;
F       ;=== Get next field===
        IF DC>58 GOTO UP
        KILL X,DIC,P   ;"Note: newer version of code renames P to DISPOINT
        SET DIC(0)="Z"  ;"WAS EZ
        SET C=","
        SET DIC="^DD("_DK_C
        SET DIC("W")=""
        SET DIC("S")="IF $PIECE(^(0),U,2)'[""m"""_$SELECT($DATA(DICS):" "_DICS,1:""),DU=""
        SET X=$GET(TMGINFO(DC,"FLD"))
        IF X="" GOTO UP
        ;"IF X?1"[".E GOTO TEM  ;"I think this is for putting all on one line. REMOVED because it is user-interactive
        SET TMGSAVX=X
        DO ^DIC ;"search FOR field, based on user input.
        IF Y=-1 SET X=TMGSAVX
        IF Y'>0 GOTO COMP
        KILL P
        SET DE=Y(0)
        SET O(DC)=$PIECE(DE,U)  ;"Store first part of search term
        SET DU=+Y   ;"DU = field number
        SET Z=$PIECE(DE,U,3)  ;"pointers or SET data
        SET E=$PIECE(DE,U,2)  ;"field info codes, poss with subfile #
G       ;==== Get Condition =========
        KILL X,DIC
        SET DIC="^DOPT(""DIS"","  ;"file containing "equals","contains","greater than" etc.
        SET DIC(0)="Z"  ;"Was QEZ
        IF E["B" SET X="" GOTO OK ;"'B'->field is a BOOLEAN COMPUTED field, so skip
        IF +E=0 GOTO G2 ;"E=file info code starts with # IF subfile.  So skip IF not subfile
        SET N(DL)=N
        SET N=N+1
        SET DV(DL)=DV
        SET DL(DL)=DK
        SET DK=+E
        SET J(N)=DK
        SET X=$PIECE($PIECE(DE,U,4),";")  ;"4th piece of 0 node holds storage location
        SET I(N)=$SELECT(+X=X:X,1:""""_X_"""")
        SET Y(0)=^DD(DK,.01,0)
        SET DL=DL+1   ;"indent further
        IF $PIECE(Y(0),U,2)["W" DO  GOTO C  ;"was GOTO WP
        . SET DIC("S")="IF Y<3"
        . SET DU=+Y_"W"
        SET DV=DV_+Y_","
        GOTO F   ;"loop back to get more field info for subfile   FIX!!!  How is this pre-determined??
        ;
G2      SET X=$PIECE(E,"P",2)
        IF X,$DATA(^DIC(+X,0,"GL")) DO
        . ;Y will be FIELD lookup, unless it's COMPUTED EXPRESSION from ^DIS2
        . SET P=$SELECT(Y:+Y,1:-DC)_U_U_^("GL")
        IF E["P" DO
        . SET P=+Y_U_Y(0) ;"e.g. P=.02^PATIENT^P9000001'
        . SET X=+$PIECE(E,"P",2)
        . FOR  QUIT:'X  DO
        . . SET DA=$PIECE($G(^DD(X,.01,0)),U,2)
        . . IF DA["D" DO  QUIT
        . . . SET E="D"_E
        . . . SET X=""
        . . SET X=+$P(DA,"P",2)
        IF $DATA(P),Y>0 DO
        . SET X="(#"_+Y_")"
        . NEW SAVX SET SAVX=X
        . SET DA="DIS("""_$C(DC+64)_DL_""","
        . SET DICOMP=N
        . SET:$DATA(O(DC))[0 O(DC)=X
        . DO EN^DICOMP
        . IF $GET(X)="" DO  QUIT
        . . SET TMGRESULT="-1^Unable to process '"_SAVX
        . SET DA(DC)=X
        . SET DU=-DC
        . FOR %=0:0 SET %=$ORDER(X(%)) Q:'%  SET @(DA_%_")")=X(%)
        IF +TMGRESULT=-1 GOTO PREPDN
        ;
C       SET X=$GET(TMGINFO(DC,"COND")) ;"Get pre-defined user search condition
        IF X="" DO  GOTO PREPDN
        . SET TMGRESULT="-1^Search condition not specified for term #"_DC
        SET DN=$SELECT("'-"[$E(X):"'",1:"")  ;"IF NOT is specified then DN="'"
        SET X=$E(X,DN]""+1,99) ;"remove 'NOT' symbol, IF present
        DO ^DIC
        IF Y=-1 DO  GOTO PREPDN
        . SET TMGRESULT="-1^Search condition '"_X_"' is not valid."
C2      SET O=$PIECE("NOT ",U,DN]"")_$PIECE(Y,U,2)  ;"Store search condition in O
        IF +Y=1 DO  GOTO OK  ;"Handle NULL selected
        . SET X=DN_"?."" """
        . SET O(DC)=O(DC)_" "_O
        SET DQ=Y
        ;"At this point DQ should be one of following values:
        ;"1 for NULL,       2 for CONTAINS     3 for matches
        ;"4 for LESS THAN   5 for EQUALS       6 for GREATER THAN
        ;
        ;"====Get Search Term=================
        SET X=$GET(INFO(DC,"VALUE"))
        IF X="" DO  GOTO PREPDN
        . SET TMGRESULT="-1^No search value specified for term #"_DC
        ;
DT      ;"--Handle searches for DATES--
        IF (E'["D")!(DQ<4) GOTO PT
        SET %DT="T"  ;"was TE
        DO ^%DT
        IF Y<0 DO  GOTO PREPDN
        . SET TMGRESULT="-1^Invalid date value '"_X
        SET X=Y_U_X
        XECUTE ^DD("DD")
        SET Y=X_U_Y
        GOTO GOT
        ;
PT      ;"--POINTERS--
        IF ($DATA(P)=0)!(+DQ'=5) GOTO PT2
        ;"--Handle Pointer field EQUALS X value--
        KILL DIC,DIS($CHAR(DC+64)_DL)
        SET DIC=U_$PIECE(P,U,4)
        SET DIC(0)="M"  ;"was EMQ
        SET DU=+P
        DO ^DIC
        IF Y'>0 DO  GOTO PREPDN
        . SET TMGRESULT="-1^Search value '"_X_"' not found for search term #"_DC
        GOTO GOT
        ;
PT2     SET Y=X
        ;Line below allows looking for "^" in WP or $E-stored actual data
        IF (Y[U),($PIECE(DE,U,4)'[";E"),('$P($G(DE),U,2)),(E'["C") DO  GOTO PREPDN
        . SET TMGRESULT="-1^Search value '"_Y_"' should not contain '^'"
        IF +DQ'=3 GOTO PT3
        SET X="I X?"_Y
        SET TMGSAVX=X
        DO ^DIM
        IF $DATA(X)=0 DO  GOTO PREPDN
        . SET TMGRESULT="-1^Bad match expression: '"_TMGSAVX_"'"
        GOTO GOT
        ;
PT3     IF (DQ=4)!(DQ=6),(+Y'=Y) DO  GOTO PREPDN ;> or < have to be numeric
        . SET TMGRESULT="-1^Search value '"_Y_"' must be numeric to use comparator '"_O_"'"
        IF Y?."?" DO  GOTO PREPDN
        . SET TMGRESULT="-1^Bad search value '"_Y_"'"
        ;
SET     ;"--Handle set-type fields----
        IF E'["S" GOTO OTHR
        SET TMGSAVX=X
        DO
        . NEW D
        . SET X=1
        . IF (+DQ=5)!(Y["""") DO  KILL:(D="") X QUIT
        . . NEW DIR,DDER
        . . SET X=Y
        . . SET DIR(0)="S^"_Z
        . . SET DIR("V")=1
        . . DO ^DIR
        . . IF $G(DDER) DO  QUIT
        . . . SET D=""
        . . . SET TMGRESULT="-1^Error choosing '"_X_"' in SET '"_Z_"'"
        . . NEW DONE SET DONE=0
        . . FOR X=1:1 DO  QUIT:(D="")!DONE
        . . . SET D=$PIECE(Z,";",X) QUIT:D=""
        . . . IF Y=$PIECE(D,":") DO
        . . . . SET Y=""""_$$CONVQQ^DILIBF($P(D,":"))_"""^"_$P(D,":",2)
        . . . . SET DONE=1
        . NEW N,FND,C
        . SET Y=""""_Y_""""
        . SET N="DE"_DN_$E(" [?<=>",DQ)_Y
        . FOR X=1:1 DO  QUIT:(D="")
        . . SET D=$PIECE(Z,";",X)
        . . SET DE=$PIECE(D,":",2)
        . . IF D="" QUIT
        . . SET DIS(U,DC,$P(D,":"))=DE
        . . NEW MATCH SET MATCH=0
        . . IF @N SET MATCH=1  ;"Note: IF '(@N) QUIT <-- won't work
        . . IF 'MATCH QUIT
        . . SET FND="'"_DE_"'"
        . IF $D(FND)=0 KILL X QUIT
        IF +TMGRESULT=-1 GOTO PREPDN
        KILL DIS("XFORM",DC)
        IF $DATA(X)=0 DO  GOTO PREPDN
        . KILL DIS(U,DC)
        . SET TMGRESULT="-1^Search value '"_TMGSAVX_"' is invalid for SET type field."
        GOTO GOT
        ;
OTHR    IF Y?.E2A.E DO
        . SET DIS("XFORM",DC)="$$UP^DILIBF(;)"
        . SET Y=$$UP^DILIBF(Y)
        DO
        . N P,YY,C
        . SET C=""""
        . SET YY=C_$$CONVQQ^DILIBF($P(Y,U))
        . FOR P=2:1:$L(Y,U)  DO
        . . SET YY="("_YY_"""_$C(94)_"""_$$CONVQQ^DILIBF($P(Y,U,P)),C=C_")"
        . SET Y=YY_C
        ;
        ;===============================================
GOT     ;"At this point, Y should be search value
        SET X=DN_$EXTRACT(" [?<=>",DQ)_$P(Y,U)
        IF E["D" DO
        . IF ($PIECE(Y,U)'["."),$E(Y,6,7) DO
        . . SET %=$PIECE("^^^^ any time during^ the entire day",U,DQ)
        . . IF %']"" QUIT
        . . SET DIS("XFORM",DC)="$P(;,""."")"
        . . SET O=O_%
        . SET Y=$P(Y,U,3)_U_$P(Y,U,2)
        IF $GET(DIS("XFORM",DC))="$$UP^DILIBF(;)" SET O=O_" (case-insensitive)"
        SET O(DC)=O(DC)_" "_O_" "_Y
        ;
OK      SET DC(DC)=DV_DU_U_X
        SET %=DL-1_U_(N#100)
        IF DL>1,O(DC)'[R SET O(DC)=R_" "_O(DC)
        IF DU["W" SET %=DL-2_U_(N#100-1)
        SET DX(DC)=%
        SET DC=DC+1 ;"Incr logical part (i.e. 'A'->'B'->'C'->D)
        IF DC=27 SET DC=33
B       GOTO F:(DU'["W"&(DC<59))
        ;
        ;"==============
UP      IF (DC'>1)!(DL'<2) GOTO U2
        ;"Done with entering conditions.  Continue processing in ^TMGDIS0
        DO DIS0^TMGDIS0(.TMGINFO,.TMGOUT,.TMGBYROOT) ;"Sets TMGRESULT
        GOTO PREPDN
        ;
U2      SET DL=DL-1
        SET DV=DV(DL)
        SET DK=DL(DL)
        SET N=N(DL)
        SET R=$SELECT($DATA(R(DL)):R(DL),1:R)
        KILL R(DL)
        SET %=N
        FOR  DO  IF %<0 GOTO F   ;"go back and get more field information.
        . SET %=$ORDER(I(%))
        . IF %="" SET %=-1
        . IF %<0 QUIT
        . KILL I(%),J(%)
Q2      IF '$D(DIARU) DO  GOTO PREPDN
        . SET TMGRESULT="-1^No search terms found"
        ;"GOTO DIS2^TMGDIS2
        SET TMGRESULT=$$DIS2^TMGDIS2
        ;
        ;"==========================================
PREPDN  ;"Purpose: New common exit point for function
        DO Q ;"kill vars
        QUIT TMGRESULT
        ;
        ;
        ;--Code below from TMGDIS2----
        ;"==========================================
        ;SFISC/GFT-SEARCH, TEMPLATES & COMPUTED FIELDS;4JUN2005
        ;;22.0;VA FileMan;**6,144**;Mar 30, 1999;Build 5
        ;"==========================================
        ;
COMP    SET E=X  ;"e.g. X="(#.02)"
        SET DICMX="X DIS(DIXX)"
        SET DICOMP=N_"?"
        SET DQI="Y("
        SET DA="DIS("""_$C(DC+64)_DL_""","
        IF $D(O(DC))[0
        SET O(DC)=X
        IF X?.E1":" DO COLON GOTO R
        IF (X?.E1":.01"),($D(O(DC))[0) SET O(DC)=$E(X,1,$L(X)-4)
        DO EN^DICOMP ;"Eval computed expression.  Output in X
        DO XA
        IF $GET(X)="" DO  GOTO PREPDN^TMGDIS
        . SET TMGRESULT="-1^Unable to evaluate computed expression '"_E_"'"
        IF Y["m" DO  GOTO PREPDN^TMGDIS
        . SET TMGRESULT="-1^Found unexpected 'm' in '"_Y_"'"
        ;"GOTO X:'$D(X)
        ;"GOTO X:Y["m" ;IF Y["m" SET X=E_":" G COMP
        SET DA(DC)=X
        SET DU=-DC
        SET E=$E("B",Y["B")_$E("D",Y["D")
        IF Y["p" SET E="p"_+$P(Y,"p",2)
        GOTO G
        ;
COLON   ; NOTE: code reached only by DO call
        DO ^DICOMPW
        DO XA  ;"Setup DIS array
        IF $GET(X)="" DO  GOTO PREPDN^TMGDIS
        . SET TMGRESULT="-1^Unable to evaluate computed expression '"_E_"'"
        ;"G X:'$D(X)
        SET R(DL)=R
        SET N(DL)=N
        SET N=+Y
        SET DY=DY+1
        SET DV(DL)=DV
        SET DL(DL)=DK
        SET DL=DL+1
        SET DV=DV_-DY_C
        SET DY(DY)=DP_U_$S(Y["m":DC_"."_DL,1:"")_U_X
        SET R=U_$P(DP,U,2)
        KILL X
        QUIT
        ;
        ;"==========================================
XA      SET %=0
        FOR  DO  Q:%=""
        . SET %=$O(X(%))
        . Q:%=""
        . SET @(DA_%_")")=X(%)
        SET %=-1
        QUIT
        ;
Q ;
        KILL DIC,DA,DX,O,D,DC,DI,DK,DL,DQ,DU,DV
        KILL E,DE,DJ,N,P,Z,R,DY,DTOUT,DIRUT,DUOUT,DIROUT,^UTILITY($J)
        QUIT

TEM     ;"Note: code execution reached here by GOTO
        ;"Note: This code is user-interactive, so will not be used.
        KILL DIC
        SET X=$P($E(X,2,99),"]",1)
        SET DIC="^DIBT("
        SET DIC(0)="EQ"
        DO
        . NEW S SET S=$S($D(DIAR):"$P(^(0),U,8)",1:"'$P(^(0),U,8)")
        . SET DIC("S")="I "_S_",$P(^(0),U,4)=DK,$P(^(0),U,5)=DUZ!'$P(^(0),U,5),$D(^(""DIS""))"
        . SET DIC("W")="X ""FOR %=1:1 Q:'$D(^DIBT(Y,""""O"""",%,0))  "
        . SET DIC("W")=DIC("W")_"WRITE !?9 SET I=^(0) W:$L(I)+$X>79 !?9 WRITE I"""
        DO ^DIC
        KILL DIC
        IF Y<0 GOTO F
        SET P="DIS"
        SET Z=-1,%X="^DIBT(+Y,P,",%Y="DIS(" D %XY^%RCR
        SET %Y="^UTILITY($J,",P="O" D %XY^%RCR
        SET TMGRESULT=$$DIS2^TMGDIS2()   ;"G DIS2^TMGDIS2
        GOTO PREPDN
