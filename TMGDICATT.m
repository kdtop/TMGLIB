TMGDICATT ;KST/SFISC/GFT,XAK- TMG VERSION OF MODIFY FILE ATTR ;6 JAN,2011, 2/2/14
         ;;1.0;TMG-LIB;**1**;1/6/11
         ;
         ;"Original file header below
         ;"SFISC/GFT,XAK-MODIFY FILE ATTR ;4JUL2006
         ;";;22.0;VA FileMan;**7,82,1003,1004,1009,1023,1024**;Mar 30, 1999
         ;
         ;"Original file (as above) is Public Domain.  Modifications
         ;"made herein are Copyright Kevin Toppenberg MD Jan 6, 2011
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
 ;"$$ADDFIELD(TMGINFO,TMGOUT) ;-- API for Add    aspect of Fileman MODIFY FILE
 ;"$$DELFIELD(TMGINFO,TMGOUT) ;-- API for Delete aspect of Fileman MODIFY FILE
 ;"$$EDITFLD(TMGINFO,TMGOUT)  ;-- API for Edit   aspect of Fileman MODIFY FILE
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"$$ALTFIELD(TMGINFO,TMGOUT,TMGEDIT) ; -- provide a silent API for fileman function MODIFY FILE
 ;
 ;"=======================================================================
 ;
ADDFIELD(TMGINFO,TMGOUT) ;
        ;"Purpose: to provide a silent API for fileman function MODIFY FILE
        ;"         this will create a NEW file IF it doesn't already exist,
        ;"Input: TMGINFO - PASS BY REFERENCE.  Input information
        ;"             TMGINFO("FILE")=File number of file to edit.
        ;"             TMGINFO("FLD","NUM")= Field to add or edit.
        ;"             TMGINFO("FLD","NAME")= Field name IF added.
        ;"             TMGINFO("FLD","DATATYPE")= Name or number of data type for field to be added/edited:
        ;"                        1, or "DATE/TIME"
        ;"                        2, or "NUMERIC"
        ;"                        3, or "SET OF CODES"
        ;"                        4, or "FREE TEXT"
        ;"                        5, or "WORD-PROCESSING"
        ;"                        6, or "COMPUTED"
        ;"                        7, or "POINTER TO A FILE"
        ;"                        8, or "VARIABLE-POINTER"
        ;"                        9, or "MUMPS"
        ;"             TMGINFO("FLD","MULTIPLE?")="Y" OR "N" (Optional.  default is NO)  (Ignored for WP datatype)
        ;"             TMGINFO("FLD","MULTIPLE?","ASK FOR ANOTHER?")="Y" OR "N" (Optional.  default is NO)  //HAVING ENTERED OR EDITED ONE ;, SHOULD USER BE ASKED ANOTHER
        ;"             TMGINFO("FLD","MULTIPLE?","SUB-DICT NUM")=e.g. 227111.111 (Optional.  default is FM suggestion)
        ;"             TMGINFO("FLD","SEE ADDING NEW?")="Y" OR "N" (Optional.  default is NO)   // SHOULD USER SEE AN "ADDING A NEW ;?" MESSAGE FOR NEW ENTRIES
        ;"             TMGINFO("FLD","MANDITORY?")="Y" OR "N" (Optional.  default is NO)
        ;"             TMGINFO("FLD","HELP PROMPT")=(Optional.  default is Fileman generated)
        ;"             TMGINFO("FLD","XECUTABLE HELP")=(Optional.  default is none)
        ;"             TMGINFO("FLD","DESCRIPTION",0)=(Optional. Lines of descriptive text)
        ;"             TMGINFO("FLD","DESCRIPTION",1)=(Optional. Lines of descriptive text)
        ;"             TMGINFO("FLD","DESCRIPTION",2)=(Optional. Lines of descriptive text)
        ;"             TMGINFO("FLD","DESCRIPTION",etc)=(Optional. Lines of descriptive text)
        ;"             TMGINFO("FLD","STORE","SUBSCRIPT")=Subscript name/number for storage
        ;"             TMGINFO("FLD","STORE","PIECE")=Piece position for storage <-- not used IF DATATYPE=9 (MUMPS), and ? 5 (WP)
        ;"             NOTE: the following line should contain answers to questions:
        ;"                   ENTER A TRUTH-VALUED EXPRESSION WHICH MUST BE TRUE OF ANY ENTRY POINTED TO:
        ;"                   MUMPS CODE THAT WILL SET 'DIC(""S"")'
        ;"               NOTE: this is asked for only some data types: SETS,POINTER-TO-FILE
        ;"                   If no screen code provided, then not used.
        ;"                 TMGINFO("FLD","INPUT SCREEN CODE") -- holds mumps code that sets DIC("S").  OPTIONAL.
        ;"                 TMGINFO("FLD","INPUT SCREEN DESCRIPTION") -- Text to describe input screen.
        ;"             TMGINFO("FLD",DataTypeNum,*) -- Holds answers to required questions for various datatypes.
        ;"           For type of 1 (DATE/TIME):
        ;"             TMGINFO("FLD",1,"EARLIEST DATE")=OptionalEarliestDate (Optional) (external, or FM format)
        ;"             TMGINFO("FLD",1,"LATEST DATE")=OptionalLatestDate (Optional) (external, or FM format)
        ;"             TMGINFO("FLD",1,"IMPRECISE DATE OK")=OptionalLatestDate (Optional default is YES)
        ;"             TMGINFO("FLD",1,"TIME ALLOWED")="Y" OR "N" (Optional.  default is YES)
        ;"             TMGINFO("FLD",1,"TIME REQUIRED")="Y" OR "N" (Optional.  default is NO)
        ;"             TMGINFO("FLD",1,"SEC ALLOWED")="Y" OR "N" (Optional.  default is YES)
        ;"        ---For type of 2 (NUMERIC):
        ;"             TMGINFO("FLD",2,"INCLUSIVE LOWER BOUND")=RequiredNumber
        ;"             TMGINFO("FLD",2,"INCLUSIVE UPPER BOUND")=RequiredNumber
        ;"             TMGINFO("FLD",2,"DOLLAR AMOUNT?")="Y" OR "N" (Optional.  default is NO)
        ;"             TMGINFO("FLD",2,"MAX NUMBER OF DIGITS")=Number (Optional.  default is 0)
        ;"        ---For type of 3 (SET OF CODES):
        ;"             TMGINFO("FLD",3,"SET",<InternalCode>)=ExternalName
        ;"             e.g. TMGINFO("FLD",3,"SET","I")="Incomplete"
        ;"             e.g. TMGINFO("FLD",3,"SET","S")="Signed"
        ;"             e.g. TMGINFO("FLD",3,"SET","U")="Unsigned"
        ;"             e.g. TMGINFO("FLD",3,"SET","P")="Processed"
        ;"        ---For type of 4 (FREE TEX):
        ;"             TMGINFO("FLD",4,"MIN LEN")=NumberOfMinlength  Optional, default=1
        ;"             TMGINFO("FLD",4,"MAX LEN")=NumberOfMaxlength  Optional, default=32
        ;"             TMGINFO("FLD",4,"PATTERN MATCH") -- optional
        ;"        ---For type of 5 (WP):
        ;"            Below is answer for following FM question:
        ;"              SHALL THIS TEXT NORMALLY APPEAR IN WORD-WRAP MODE?
        ;"                ANSWER 'YES' IF THE INTERNALLY-STORED '"_F_"' TEXT"
        ;"                SHOULD NORMALLY BE PRINTED OUT IN FULL LINES, BREAKING AT WORD BOUNDARIES."
        ;"                ANSWER 'NO' IF THE INTERNAL TEXT SHOULD NORMALLY BE PRINTED OUT"
        ;"                LINE-FOR-LINE AS IT STANDS."
        ;"             TMGINFO("FLD",5,"WORD WRAP MODE")="Y" OR "N" (Optional.  default is YES)
        ;"            Below is answer for following FM question:
        ;"              SHALL ""|"" CHARACTERS IN THIS TEXT BE TREATED LIKE ANY OTHER CHARACTERS?"
        ;"              ANSWER 'YES' IF THE INTERNALLY-STORED '"_F_"' TEXT MAY HAVE ""|"" CHARACTERS"
        ;"              IN IT (SUCH AS HL7 MESSAGES) THAT NEED TO DISPLAY EXACTLY AS THEY ARE STORED."
        ;"              ANSWER 'NO' IF THE INTERNAL TEXT SHOULD NORMALLY BE PRINTED OUT WITH ANYTHING"
        ;"              THAT IS DELIMITED BY ""|"" CHARACTERS INTERPRETED AS VARIABLE TEXT."
        ;"             TMGINFO("FLD",5,"IGNORE |")="Y" OR "N" (Optional.  default is NO)
        ;"        ---For type of 6 (COMPUTED):
        ;"             TMGINFO("FLD",6,"COMPUTED-FIELD EXPRESSION")=Expression (required)
        ;"             TMGINFO("FLD",6,"RESULT TYPE")=<code>.  Required.  MUST be one of the following.
        ;"                      S or STRING
        ;"                      N or NUMERIC
        ;"                      B or BOOLEAN
        ;"                      D or DATE
        ;"                      m or MULTIPLE
        ;"                      p or POINTER
        ;"                      mp or MULTIPLE POINTER
        ;"             Used IF RESULT TYPE is 'S':
        ;"               Answer for: //Maximum number of character expected to be output."
        ;"                 TMGINFO("FLD",6,"MAX LEN OUT")=Number.  Optional.  Default is FM suggestion..
        ;"             Used IF RESULT TYPE is 'N':
        ;"               Answer for: //NUMBER OF FRACTIONAL DIGITS TO OUTPUT
        ;"                 TMGINFO("FLD",6,"NUM DIGITS")=Num0to14. (Optional. Default is 0)
        ;"               Answer for: SHOULD VALUE ALWAYS BE INTERNALLY ROUNDED TO <#> DECIMAL PLACES"
        ;"                 TMGINFO("FLD",6,"ROUND TO NUM DIGITS")="Y" OR "N" (Optional.  default is YES)
        ;"               Answer for: WHEN TOTALLING THIS FIELD, SHOULD THE SUM BE COMPUTED FROM THE SUMS OF THE COMPONENT FIELDS
        ;"                 TMGINFO("FLD",6,"CALC FROM SUMS OF COMPONENT FLDS")="Y" OR "N" (Optional.  default is FM suggestion)
        ;"             *Required* IF RESULT TYPE is 'p' or 'mp':
        ;"               TMGINFO("FLD",6,"POINT TO WHAT FILE")=fileNameorNumber.
        ;"        ---For type of 7 (POINTER TO A FILE):
        ;"               TMGINFO("FLD",7,"POINT TO WHAT FILE")=fileNameorNumber.
        ;"              Answer to question: SHOULD 'ADDING A NEW <file> FILE ENTRY' ("LAYGO")
        ;"                                  BE ALLOWED WHEN ANSWERING THE <field> QUESTION?
        ;"               TMGINFO("FLD",7,"LAYGO")="Y" OR "N" (Optional.  default is NO)
        ;"        ---For type of 8 (VARIABLE-POINTER):
        ;"              **For every file that field may point to, an array as below
        ;"                must be supplied.  I.e. n=1, then n=2, then n=3 etc.
        ;"                The node number (e.g. n=1 vs 2 etc) will be the sequence number of storage.
        ;"               TMGINFO("FLD",8,n,"POINT TO WHAT FILE")=fileNameorNumber. Required.
        ;"               TMGINFO("FLD",8,n,"MESSAGE")=TextOfMessage.  Required.
        ;"               TMGINFO("FLD",8,n,"PREFIX")=PrefixText.  Required.
        ;"               TMGINFO("FLD",8,n,"INPUT SCREEN CODE") -- holds mumps code that sets DIC("S").  OPTIONAL.
        ;"               TMGINFO("FLD",8,n,"INPUT SCREEN DESCRIPTION") -- Text to describe input screen.
        ;"               TMGINFO("FLD",8,n,"LAYGO")="Y" OR "N" (Optional.  default is NO)
        ;"        ---For type of 9 (MUMPS):
        ;"                No additional information is needed.
        ;"
        ;"       TMGOUT -- PASS BY REFERENCE.  Messages stored as follows
        ;"              TMGOUT(1)="first message line."
        ;"     Also uses TMGEDIT, IF defined.  (Globally-scoped var)
        ;"Result: 1^Success, or -1^ErrorMessage
        ;"
        QUIT $$ALTFIELD(.TMGINFO,.TMGOUT) ;
        ;
DELFIELD(TMGINFO,TMGOUT) ;
        ;"Purpose: to provide a silent API for fileman function Delete aspect of MODIFY FILE
        ;"Input: TMGINFO - PASS BY REFERENCE.  Input information.
        ;"              Parameters are the same as for ADDFIELD, with changes:        ;
        ;"             TMGINFO("FILE")=File number of file to edit.
        ;"             TMGINFO("FLD","NAME")= Existing field name to lookup for deleting.   <-- one or other required
        ;"             TMGINFO("FLD","NUM")= Existing field number to lookup for deleting.  <-- one or other required
        ;"             TMGINFO("DELETE DATA")="Y" OR "N" Optional (Default="Y").
        ;"                      If field is deleted, then data will also be deleted IF "Y"
        ;"       TMGOUT -- PASS BY REFERENCE.  Messages stored as follows
        ;"              TMGOUT(1)="first message line."
        ;"Result: 1^Success, or -1^ErrorMessage
        ;"
        SET TMGINFO("EFLD","NAME")="@"
        QUIT $$EDITFLD(.TMGINFO,.TMGOUT)
        ;
EDITFLD(TMGINFO,TMGOUT) ;
        ;"Purpose: to provide a silent API for fileman function Edit aspect of MODIFY FILE
        ;"Input: TMGINFO - PASS BY REFERENCE.  Input information.
        ;"              Parameters are the same as for ADDFIELD, with changes:        ;
        ;"             TMGINFO("FILE")=File number of file to edit.
        ;"             TMGINFO("FLD","NAME")= Existing field name to lookup for editing.
        ;"             ;
        ;"             --All of the following are OPTIONAL--
        ;"             **Information will be ignored IF not consistent with FM logical flow.
        ;"             TMGINFO("EFLD","NAME")=NewName
        ;"             TMGINFO("EFLD","TITLE")=NewTitle
        ;"             TMGINFO("EFLD","AUDIT"))=    should be 'Y','YES','N', or 'NO' IF provided
        ;"             TMGINFO("EFLD","AUDIT CONDITION"))=<Mumps Code that will SET $T to 1 for Audit to take place.>
        ;"             TMGINFO("EFLD","READ ACCESS"))=NewValue
        ;"             TMGINFO("EFLD","DELETE ACCESS"))=NewValue
        ;"             TMGINFO("EFLD","WRITE ACCESS"))=NewValue
        ;"             TMGINFO("EFLD","SOURCE"))=NewValue
        ;"             // not currently supported --> TMGINFO("EFLD","DESTINATION"))=
        ;"             // not currently supported --> TMGINFO("EFLD","GROUP"))=
        ;"             TMGINFO("EFLD","DESCRIPTION",0)=(Optional. Lines of descriptive text)
        ;"             TMGINFO("EFLD","DESCRIPTION",1)=(Optional. Lines of descriptive text)
        ;"             TMGINFO("EFLD","DESCRIPTION",2)=(Optional. Lines of descriptive text)
        ;"             TMGINFO("EFLD","DESCRIPTION",etc)=(Optional. Lines of descriptive text)
        ;"             TMGINFO("EFLD","TECHNICAL DESCRIPTION",0)=(Optional. Lines of descriptive text)
        ;"             TMGINFO("EFLD","TECHNICAL DESCRIPTION",1)=(Optional. Lines of descriptive text)
        ;"             TMGINFO("EFLD","TECHNICAL DESCRIPTION",2)=(Optional. Lines of descriptive text)
        ;"             TMGINFO("EFLD","TECHNICAL DESCRIPTION",etc)=(Optional. Lines of descriptive text)
        ;"
        ;"              TMGINFO("DELETE DATA")="Y" OR "N" Optional (Default="Y").
        ;"                      If field is deleted, then data will also be deleted IF "Y"
        ;"              TMGINFO("WRITE ACCESS") -- optional WRITE access for field delete
        ;"       TMGOUT -- PASS BY REFERENCE.  Messages stored as follows
        ;"              TMGOUT(1)="first message line."
        ;"NOTE: If field does not already exist, then this function will act
        ;"      the SAME as $$ADDFIELD(), and the input needed for that function
        ;"      must be defined.
        ;"Result: 1^Success, or -1^ErrorMessage
        ;
        NEW TMGRESULT SET TMGRESULT="1^Success"
        SET TMGINFO("EFLD",.01)=$GET(TMGINFO("EFLD","NAME"))
        SET TMGINFO("EFLD",.1)=$GET(TMGINFO("EFLD","TITLE"))
        SET TMGINFO("EFLD",1.1)=$GET(TMGINFO("EFLD","AUDIT"))
        IF TMGINFO("EFLD",1.1)'="" DO
        . NEW VAL,ORIG SET (VAL,ORIG)=TMGINFO("EFLD",1.1)
        . IF $$CHKYN^TMGDICATTS(.VAL)=0 DO  QUIT
        . . SET TMGRESULT="-1^Invalid value for INPUT(""FLD"",""AUDIT"").  Got '"_ORIG_"'"
        . SET TMGINFO("EFLD",1.1)=VAL
        IF TMGRESULT<0 GOTO EDDN
        SET TMGINFO("EFLD",1.2)=$GET(TMGINFO("EFLD","AUDIT CONDITION"))
        SET TMGINFO("EFLD",8)=$GET(TMGINFO("EFLD","READ ACCESS"))
        SET TMGINFO("EFLD",8.5)=$GET(TMGINFO("EFLD","DELETE ACCESS"))
        SET TMGINFO("EFLD",9)=$GET(TMGINFO("EFLD","WRITE ACCESS"))
        SET TMGINFO("EFLD",10)=$GET(TMGINFO("EFLD","SOURCE"))
        ;"// not currently supported --> SET TMGINFO("EFLD",11)=$GET(TMGINFO("EFLD","DESTINATION"))
        ;"// not currently supported --> SET TMGINFO("EFLD",20)=$GET(TMGINFO("EFLD","GROUP"))
        MERGE TMGINFO("EFLD",21)=TMGINFO("FLD","DESCRIPTION")
        MERGE TMGINFO("EFLD",23)=TMGINFO("FLD","TECHNICAL DESCRIPTION")
        ;
        SET TMGRESULT=$$ALTFIELD(.TMGINFO,.TMGOUT,1)
EDDN    QUIT TMGRESULT
        ;
        ;
ALTFIELD(TMGINFO,TMGOUT,TMGEDIT) ;
        ;"Purpose: to provide a silent API for fileman function MODIFY FILE
        ;"         this will create a NEW file IF it doesn't already exist,
        ;"         or it will edit the field IF TMGINFO("EFLD") data exists (see above)
        ;"Input: TMGINFO - PASS BY REFERENCE.  Input information
        ;"              See descriptions above.
        ;"       TMGOUT -- PASS BY REFERENCE.  Messages stored as follows
        ;"              TMGOUT(1)="first message line."
        ;"       TMGEDIT -- OPTIONAL.  Default is 0.  Set to 1 IF field is to be edited.
        ;"Result: 1^Success, or -1^ErrorMessage
EN0     ;
        SET TMGEDIT=+$GET(TMGEDIT)
        NEW X,Y,DIFILE,DIAC,DIC,DIE,DR,DA,P
        NEW TMGRESULT SET TMGRESULT="1^Success"
        NEW TMGMULT,TMGMANDIT,TMGHLP,TMGXHLP,TMGDESCR,TMGSUBSCR,TMGPIECE
        NEW TMGMINL,TMGMAXL,TMGPATMATCH
        NEW TMGFLDNUM,TMGFLDNAME,TMGFLDTYPE,TMGREF,TMGFILE,TMGMULTASK2
        NEW TMGMULTSN,TMGSHOWADDNEW,TMGSET,TMGSETCODE,TMGSETNM
        NEW TMGSDATE,TMGEDATE,TMGIMPDAT,TMGTIMEOK,TMGTIMREQ,TMGSECOK
        NEW TMGMINN,TMGMAXN,TMGDOLLAR,TMGDIGITS
        NEW TMGIPSCRN,TMGIPSDESCR
        NEW TMGWPWRAP,TMGWPNOVAR,TMGCOMPEXP,TMGCOMPTYP,TMGCOMPND,TMGCOMPROU
        NEW TMGCOMPCFC,TMGP2F,TMGLAYGO,TMGVPTR
        NEW TMGDONE SET TMGDONE=0
        DO CHKALL^TMGDICATTS(.TMGRESULT)
        IF +TMGRESULT'>0 GOTO TMGDONE
        SET DIC=TMGREF
        SET DIFILE=TMGFILE
EN ;
        KILL I
        SET Q="""",I(0)=DIC,S=";"
        SET B=+$P(@(DIC_"0)"),U,2)  ;"B=File number
B ;
        KILL DA,J,DIU0,DDA
        SET A=B,DICL=0,J(0)=B,DDA=""
        ;
M       ;"Main looping-back point
        IF (TMGDONE)!(+TMGRESULT'>0) GOTO TMGDONE
        I ($G(Z)["W"),(A-B) GOTO B
        ;"W !!!
        KILL O,DQ,DIC,DIE,DG,M
        IF $D(DTOUT) GOTO Q^DIB
        SET O=1,E=0
        SET DIC(0)="IZ"  ;"ALEQIZ"
        IF TMGEDIT'=1 SET DIC(0)=DIC(0)_"L"
        SET X=TMGFLDNAME
        SET DINUM=TMGFLDNUM
        SET DIC="^DD("_A_","
        IF $D(DICS) SET DIC("S")=DICS
        ;"S DIC("W")="S %=$P(^(0),U,2) I % W $P(""  (multiple)^  (word-processing)"",U,$P(^DD(+%,.01,0),U,2)[""W""+1)"
        IF $P(^DD(A,.01,0),U,2)["W" DO
        . S DIC(0)="AEQZ",DIC("B")=.01
        ELSE  I $D(DA),$D(^DD(A,DA,0)),'$P(^(0),U,2),$P(^(0),U,4)'?.P DO
        . SET E=DA
        DO ^DIC  ;"<-- will add NEW field (stub entry only), IF NEW one selected
        ;"sets DA=1 (field num?), Y(0)=FieldName (is the entire zero node of the selected entry)
        ;"set Y(0,0)=FieldName  (the external form of the .01 field)
        ;"X=FieldName, Y=FieldNum^FieldName^1 <-- 1=Newly added Rec
        IF $P(Y,U,3) SET DDA="N"  ;" "N" for 'new'(?)
        ELSE  IF $GET(TMGEDIT)'=1 DO  GOTO TMGDONE
        . SET TMGRESULT="-1^Field '"_TMGFLDNAME_"' already exists.  Can't create."
        IF Y'<0 GOTO SV
        IF (A-B) GOTO B
        SET TMGRESULT="-1^Unable to create field, or find existing field."
        DO Q^TMGDICATT2  ;"was a GOTO
        GOTO TMGDONE
        ;
SV      IF '$P(Y,U,3) DO  ;"Fld not just added.
        . SET DIU0=A
        . SET O(1)=$P(^DD(A,+Y,0),U,1,2)
        . SET O(2)=$S($D(^(.1)):$P(^(.1),U),1:"")
        . SET DDA="E"
        . DO SV^DICATTA
        SET DDA(1)=A  ;"A=FileNum
        SET DIAC="AUDIT",DIFILE=A
        DO ^DIAC  ;"file access determination (if user has access to file)
        SET O=+%
        K DIAC,DIFILE  ;"%=1 IF user has access, 0 IF not.
SKP     SET (D0,DA)=+Y  ;"+Y=field number
        SET DA(1)=A   ;"A=filenumber
        SET DIE=DIC
        SET M=Y(0)    ;"Y(0)=zero node of entry
        SET T=$P(M,U,2) ;"T = field characteristics string
        IF (T["C")!(T["W") SET O=0  ;"If computed or WP, then O=0, (don't DO ^DIE)
        ;"SET DR=""  ; moved below by //kt
        ;"IF (DUZ(0)="@")!'$F(T,"X") SET DR=DR_".01:.1;"
        ;";"SET DR=$P(".01:.1;",U,DUZ(0)="@"!'$F(T,"X"))
        ;"SET DR=DR_$P("1.1;",U,T'["C")
        ;"SET DR=DR_$S(DUZ(0)="@"&(T'["C"):"1.2;",1:"")
        ;"SET DR=DR_$S(T["C":"8;",1:"8:9;10:")
        ;"SET DR=DR_"11;20:29"
        ;";"Example DR: DR=.01:.1;1.1;1.2;8:9;10:11;20:29
        ;"S O=$S($P(Y,U,3):0,1:1_U_$P(M,U,2,99))
        IF $P(Y,U,3) SET O=0  ;"If record just added, O=0 (don't DO ^DIE)
        ELSE  SET O="1^"_$P(M,U,2,99)
        SET F=$P(M,U)  ;"Field NAME
        KILL DIC,DQI
        ;"S X=0 F  S X=$O(^DD(A,DA,1,X)) Q:X'>0  I +^(X,0)=B,$P(^(0),B,2)?1"^"1.A S DQI=$P(^(0),U,2)
        SET X=0
        FOR  SET X=$O(^DD(A,DA,1,X)) QUIT:X'>0  DO
        . IF +^(X,0)'=B QUIT
        . IF $P(^(0),B,2)?1"^"1.A SET DQI=$P(^(0),U,2)
        ;"S X=-1 I 'T D DIE:O  Q:$D(DTOUT)  S:'$D(DA) DDA="D" G TYPE^DICATT2:$D(DA),N:$P(O,U,4)?.P,^DICATT4
        SET X=-1
        IF T GOTO SKP2  ;"If field characteristics string starts with number, then DO different ^DIE
        ;"Setting DR moved down from above by //kt
        SET DR=""  ;"NOTE: DR SET up 'fields' to edit, using definitions from ^DD(0,*)
        IF $GET(TMGINFO("EFLD",.01))="@" DO  GOTO:(TMGRESULT<1) TMGDONE GOTO SKP0
        . SET DR=".01;"
        . SET TMGDONE=1
        . IF TMGINFO("FLD","NUM")=.01 DO  QUIT
        . . SET TMGRESULT="-1^Can not delete required .01 field.  Try editing instead."
        IF (DUZ(0)="@")!'$F(T,"X") DO
        . SET DR=DR_".01;"  ;".01=LABEL
        . SET DR=DR_".1;"   ;".1=TITLE
        IF T["C" DO
        . SET DR=DR_"8;" ;"8=READ ACCESS (OPTIONAL)
        ELSE  DO
        . SET DR=DR_"1.1;" ;"1.1=AUDIT
        . IF DUZ(0)="@" SET DR=DR_"1.2;"  ;"1.2=AUDIT CONDITION
        . SET DR=DR_"8;"   ;"8=READ ACCESS (OPTIONAL)
        . SET DR=DR_"8.5;" ;"8.5=DELETE ACCESS (OPTIONAL)
        . SET DR=DR_"9;"   ;"9=WRITE ACCESS (OPTIONAL)
        . SET DR=DR_"10;"  ;"10=SOURCE
        SET DR=DR_"11;"    ;"11=DESTINATION
        SET DR=DR_"20"     ;"20=GROUP
        ;"Example DR: DR=.01:.1;1.1;1.2;8:9;10:11;20:29
SKP0    DO STUFVALS(.DR)  ;"put values into DR string //kt
        IF O DO
        . NEW FLD FOR FLD=21,23 DO  ;"21=DESCRIPTION, 23=TECHNICAL DESCRIPTION
        . . DO STUFFWP(FLD) ;"will only stuff IF user-supplied data present.
        . IF TMGRESULT<0 QUIT
        . DO DIE  ;"Edit the rest of the fields
        SET DR=""
        IF $D(DTOUT)!(TMGRESULT<0) GOTO TMGDONE  ;"QUIT
        IF $D(DA) GOTO TYPE^TMGDICATT2
        ;
        SET DDA="D"
        IF $P(O,U,4)?.P GOTO N
        GOTO ^TMGDICATT4
        ;
SKP2    SET DR=".01;8;9;10:11;20:29"
        DO STUFVALS(.DR)  ;"put values into DR string //kt
        DO DIE
        IF '$D(DA) S DDA="D" S DQ(+T)=0 G NEW^TMGDICATT4
        SET X=$P($P(M,U,4),";")
        SET M=^DD(A,DA,0)
        SET E=$P(M,U)
        SET A=+T
        SET DICL=DICL+1
        SET J(DICL)=A
        SET Y=$E(Q,+X'=X)
        SET I(DICL)=Y_X_Y
        IF E'=F DO
        . S ^(0)=E_" SUB-FIELD^"_$P(^DD(A,0),U,2,9)
        . K ^(0,"NM")
        . S ^("NM",E)=""
        IF $P(M,U,2)["W" GOTO 5
        GOTO N
 ;
 ;
E       SET DE=^DD(A,E,0)
        WRITE $P(DE,U)   ;"<-- WHAT DOES THIS OUTPUT? AND WHEN?
        QUIT
 ;
P       SET DI=DIU0
        IF $DATA(O(1)) DO
        . IF '$DATA(DA) DO  QUIT
        . . S DA=D0 D DIPZ^DIU0
        . IF $DATA(^DD(DI,DA,0)),O(1)'=$P(^(0),U,1,2) DO  QUIT
        . . DO DIPZ^DIU0
        . IF $DATA(^(.1)),O(2)'=$P(^(.1),U) DO  QUIT
        . . DO DIPZ^DIU0
        KILL DIU0
        QUIT
 ;
N       IF DDA]"" DO AUDIT^DICATT22(DDA(1),D0,DDA)
        IF $D(DIU0) DO P
        SET DIZZ=$S(('O&$D(DIZ)):DIZ,1:$P(O,U,2,3))
        GOTO M
        ;
X       ;"W $C(7),"    '",F,"' DELETED!"
        DO ADDERR^TMGDICATTS("FIELD DEFINITION FOR '"_F_"' DELETED.")
        SET DDA=$S(DDA="":"D",1:"")
        SET DIK="^DD(A,",DA(1)=A
        DO ^DIK
        GOTO N
 ;
CHECK   ;"G:$P(^DD(A,DA,0),U,2)']"" X:$D(DTOUT) G NO^DICATT2
        IF ($P(^DD(A,DA,0),U,2)']""),$D(DTOUT) GOTO X
        GOTO NO^TMGDICATT2
 ;
DIE ;
        NEW I,J,DICATTED
        SET DICATTED=1
        NEW ZTQUEUED SET ZTQUEUED=1  ;"Will keep ^DIE silent
        DO ^DIE
        QUIT
 ;
TYPESWITCH  ;
0       ;"Switch for field type
        SET C=$P(O,U,5,99)
        GOTO @N
        ;"===============================
1       DO CHK1^TMGDICATTS(.TMGRESULT)             ;"DATE/TIME"
        IF +TMGRESULT'>0 GOTO TMGDONE
        GOTO 1^TMGDICATT0
        ;
2       DO CHK2^TMGDICATTS(.TMGRESULT)             ;"NUMERIC"
        IF +TMGRESULT'>0 GOTO TMGDONE
        GOTO 2^TMGDICATT0
        ;
3       DO CHK3^TMGDICATTS(.TMGRESULT)             ;"SET OF CODES"
        IF +TMGRESULT'>0 GOTO TMGDONE
        GOTO 3^TMGDICATT6
        ;
4       DO CHK4^TMGDICATTS(.TMGRESULT)             ;"FREE TEXT"
        IF +TMGRESULT'>0 GOTO TMGDONE
        GOTO 4^TMGDICATT6
        ;
5       SET W="0;1"                                ;"WORD-PROCESSING"
        SET (Z,DIZ)="W^",C="Q",V=1,L=1
        DO CHK5^TMGDICATTS(.TMGRESULT)
        IF +TMGRESULT'>0 GOTO TMGDONE
        IF O GOTO ^TMGDICATT2
        GOTO SUB^TMGDICATT1
        ;
6       DO CHK6^TMGDICATTS(.TMGRESULT)             ;"COMPUTED
        IF +TMGRESULT'>0 GOTO TMGDONE
        GOTO ^TMGDICATT3
        ;
7       DO CHK7^TMGDICATTS(.TMGRESULT)             ;"POINTER TO A FILE"
        IF +TMGRESULT'>0 GOTO TMGDONE
        GOTO 7^TMGDICATT5
        ;
8       DO CHK8^TMGDICATTS(TMGRESULT)                 ;"VARIABLE-POINTER"
        IF +TMGRESULT'>0 GOTO TMGDONE
        GOTO VP^TMGDICATT4
        ;
9       SET (Z,DIZ)="K^"                  ;"MUMPS"
        SET V=0,C="K:$L(X)>245 X D:$D(X) ^DIM",L=245
        S:$P(^DD(A,DA,0),U,4)]"" W=$P(^(0),U,4)
        IF O GOTO ^TMGDICATT2
        GOTO SUB^TMGDICATT1
 ;
STUFVALS(DR)  ;
        ;"Purpose: to stuff a DR string with automatic values, and remove any
        ;"         field that does not have an automatic value
        ;"       DR -- PASS BY REFERENCE.  The string to edit.
        ;"Results: none
        NEW TEMPDR SET TEMPDR=""
        NEW FLD,I
        FOR I=1:1:$LENGTH(DR,";") DO
        . SET FLD=$PIECE(DR,";",I)
        . IF FLD="" QUIT
        . DO ADD2DR(FLD,.TEMPDR)
        SET DR=TEMPDR
        QUIT
        ;
STUFFWP(FLD)  ;
        ;"Purpose: stuff in WP data into field, IF supplied.
        ;"Input: FLD -- the field to stuff
        ;"Also uses DIE, DA (globally scoped).  Can SET TMGRESULT
        IF +$GET(FLD)'>0 QUIT
        NEW GREF SET GREF=DIE_DA_","_FLD_")"
        NEW TMGDESCR MERGE TMGDESCR=TMGINFO("EFLD",FLD)
        IF $DATA(TMGDESCR)'>0 QUIT
        NEW TMP SET TMP=$$SETWP^TMGDICATT22(GREF,.TMGDESCR)
        IF TMP<0 SET TMGRESULT=TMP
        QUIT
        ;
ADD2DR(FLD,DR) ;
        ;"Purpse: Add to DR string IF FLD has a value
        ;"Input: FLD -- the field number, e.g. .01 or 8.5 etc
        ;"       DR -- PASS BY REFERENCE.  The string to build up.
        ;"Results: none
        IF $GET(FLD)="" QUIT
        NEW VALUE SET VALUE=$GET(TMGINFO("EFLD",FLD))
        IF VALUE="" QUIT
        NEW LASTCH SET LASTCH=$EXTRACT(DR,$L(DR))
        IF $GET(DR)'="",LASTCH'=";" SET DR=DR_";"
        SET DR=DR_FLD_"///"_VALUE
        QUIT
        ;
        ;
TMGDONE QUIT TMGRESULT

