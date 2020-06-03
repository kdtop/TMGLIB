TMGFIX5 ;TMG/kst/Download in integrate VistA Integration Agreements ;2/19/14
         ;;1.0;TMG-LIB;**1**;02/19/14
;
 ;"TMG Download in integrate VistA Integration Agreements
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
 ;" API -- Public Functions.
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"=======================================================================
 ;"Dependencies   
 ;"=======================================================================
 ;"
WGET ;"<-- I can't get this to work VA site rejects wget request.
        NEW URL
        NEW FILE SET FILE="ALL%20ACTIVE%20IA%20DESCRIPTIONS%20APRIL%202013.txt"
        NEW FILE2 SET FILE2=$$REPLSTR^TMGSTUT3(FILE,"%20"," ")
        SET URL="https://downloads.va.gov/files/FOIA/Software/Integration_Agreements/"   ;"//12/28/16 <-- link no longer good.
        ;"files found here 12/28/16 ... http://foia-vista.osehra.org/VistA_Integration_Agreement/
        SET URL=URL_FILE                  
        NEW DIR SET DIR="/tmp/VistA_IAs"
        IF $$ISDIR^TMGKERNL(DIR) GOTO RF2
        IF $$MKDIR^TMGKERNL(DIR)'=0 DO  GOTO RFDN
        . WRITE "Unable to create output director: ",DIR
RF2     NEW OPTION SET OPTION="--no-check-certificate"
        IF $$WGET^TMGKERNL(URL,OPTION,DIR)'=0 DO  GOTO RFDN
        . WRITE "Unable to fetch file from VA website:",!
        . WRITE URL,!
        QUIT                                                                 
        ;
        ;
        ;
LOAD   ;
        NEW DIR
        NEW FILE IF $$GETFNAME^TMGIOUTL("Pick text file with IA's","/mnt/WinPublic/vista/ICR",,"/",.DIR,.FILE)
        NEW ARR
        SET DIR=$$MKTRALDV^TMGIOUTL(DIR,"/")  
        DO POPUPBOX^TMGUSRI2("MESSAGE","Loading, please wait...")
        IF $$HFS2ARR^TMGIOUT3(DIR,FILE,"ARR")'=1 DO  GOTO RFDN
        . WRITE "Unable to open downloaded file:",!
        . WRITE "FILE=",FILE2,!
        . WRITE "DIR=",DIR,!
        ;"Trim off top header
        WRITE #
        ;"Cut off header -- if present.  Removed because not present in latest file.  
        ;"NEW LINE SET LINE=""
        ;"FOR  DO  QUIT:LINE["------------"
        ;". NEW IDX SET IDX=$ORDER(ARR(""))
        ;". SET LINE=$GET(ARR(IDX))
        ;". KILL ARR(IDX)
        ;
        ;"NOTICE: 12/28/16 The format of the file has changed. I would have to 
        ;"    rewrite the parser to make this all work again....
        NEW FLDS DO SETFLDS(.FLDS) ;"SET UP ARRAY OF FIELD NAMES
        NEW EOFTAG SET EOFTAG="                             ********************"
        ;
        NEW PART,DATA,OUT SET DATA=1
        NEW TEMP
        SET TMGZZ=0
        NEW TMGRESULT SET TMGRESULT=""
        NEW SUCCESSNUM                          
        NEW DONE SET DONE=0                        
        NEW PROGCT SET PROGCT=1
        FOR  DO  QUIT:(DATA=0)
        . SET DATA=$$GETNXTAR(.ARR,.PART) QUIT:(DATA=0)!(DONE)
        . DO PARSPART(.PART,.OUT) ;
        . SET PROGCT=PROGCT+1
        . IF PROGCT>1 DO
        . . NEW NUM SET NUM=+$GET(OUT("NUM"))
        . . DO PROGBAR^TMGUSRI2(NUM,NUM,-1,-1)
        . . SET PROGCT=0
        . ;"NEW USAGE SET USAGE=$GET(OUT("USAGE"))
        . ;"IF USAGE'="" SET TEMP("USAGE",USAGE)=""
        . ;"NEW STATUS SET STATUS=$GET(OUT("STATUS"))
        . ;"IF STATUS'="" SET TEMP("STATUS",STATUS)=""
        . ;"NEW DUR SET DUR=$GET(OUT("DURATION"))
        . ;"IF DUR'="" SET TEMP("DURATION",DUR)=""
        . ;"NEW TYPE SET TYPE=$GET(OUT("TYPE"))
        . ;"IF TYPE'="" SET TEMP("TYPE",TYPE)=""
        . NEW NUM SET NUM=$GET(OUT("NUM"))
        . IF NUM=5040 DO
        . . NEW TEMP SET TEMP=0
        . SET TMGRESULT=$$STOREARR(.OUT,.FLDS) ;" STORE ARRAY
        . IF +TMGRESULT<0 DO
        . . WRITE !
        . . WRITE "Last successful number was: ",SUCCESSNUM,!
        . . WRITE NUM,": ",TMGRESULT,!
        . . DO ZWRITE^TMGZWR("OUT")
        . . DO PRESS2GO^TMGUSRI2
        . . ;SET DONE=1
        . ELSE  SET SUCCESSNUM=NUM
        IF +TMGRESULT'<0 WRITE !,"SUCCESS!",!
RFDN    QUIT     
        ;
 ;"===============================================================
 ;"===============================================================
PAIR2RPC() ;
        ;"Purpose: For each entry that is of type Remote Procedure, then
        ;"         create pointer to the actual records and store in field# 10
        ;"Result: 1^OK, or -1^Message
        NEW TMGRESULT SET TMGRESULT="1^OK"
        NEW PROGCT SET PROGCT=0
        NEW MAXCT SET MAXCT=$ORDER(^TMG(22721,"@"),-1)
        NEW IEN SET IEN=0
        FOR  SET IEN=$ORDER(^TMG(22721,IEN)) QUIT:(+IEN'>0)!(+TMGRESULT'>0)  DO
        . SET PROGCT=PROGCT+1
        . IF PROGCT>25 DO
        . . NEW NUM SET NUM=+$GET(OUT("NUM"))
        . . DO PROGBAR^TMGUSRI2("NUM",IEN,1,MAXCT)
        . . SET PROGCT=0
        . NEW ZN SET ZN=$GET(^TMG(22721,IEN,0)) QUIT:ZN=""
        . NEW TYPE SET TYPE=$$UP^XLFSTR($PIECE(ZN,"^",12))
        . IF TYPE'="REMOTE PROCEDURE" QUIT
        . NEW NAME SET NAME=$PIECE(ZN,"^",1)
        . NEW DIC,X,Y 
        . SET DIC=8994,DIC(0)="M",X=NAME
        . DO ^DIC 
        . QUIT:+Y'>0
        . NEW TMGFDA,TMGMSG
        . SET TMGFDA(22721,IEN_",",10)=+Y
        . DO FILE^DIE("","TMGFDA","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) DO  QUIT
        . . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        QUIT TMGRESULT
        ;
 ;"===============================================================
 ;"===============================================================
 ;
GETNXTAR(ARR,PART)  ;
        ;"Purpose: cut off next grouping and return.
        ;"Input: ARR -- The master array with the entire file.  Will be trimmed.
        ;"       PART -- OUT PARAMETER.  Returns the next part.
        ;"          PART(#)=<TEXT>         
        ;"Results: 1 if OK, 0 if done.
        ;
        NEW TMGRESULT SET TMGRESULT=1
        NEW IDX,LINE SET LINE=""
        NEW JDX SET JDX=1
        KILL PART
        SET IDX=0
        FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:(LINE[EOFTAG)!($DATA(ARR)=0)  DO  
        . SET LINE=$GET(ARR(IDX)) KILL ARR(IDX)
        . IF LINE[EOFTAG QUIT
        . SET PART(JDX)=LINE,JDX=JDX+1
        ;"Trim leading blank lines
        SET JDX=0,LINE=""
        FOR  SET JDX=$ORDER(PART(JDX)) QUIT:(LINE'="")!($DATA(PART)=0)  DO
        . SET LINE=$$TRIM^XLFSTR($GET(PART(JDX)))
        . IF LINE="" KILL PART(JDX)
        ;"Trim trailing blank lines.
        SET JDX="",LINE=""
        FOR  SET JDX=$ORDER(PART(JDX),-1) QUIT:(LINE'="")!($DATA(PART)=0)  DO
        . SET LINE=$$TRIM^XLFSTR($GET(PART(JDX)))
        . IF LINE="" KILL PART(JDX)       
        IF $DATA(PART)=0 SET TMGRESULT=0
        QUIT TMGRESULT
        ;
PARSPART(PART,OUT) ;        
        ;"USES FLDS in global scope. 
        ;"Purpose: Convert text into array of formatted text.
        ;"Output: OUT if filled as follows:
        ;"        OUT("NAME")=<name>
        ;"        OUT("NUM")=<Agreement #>
        ;"        OUT("CUSTODIAL PACKAGE")=<name>
        ;"        OUT("SUBSCRIBING PACKAGE",#)=<name>
        ;"        OUT("SUBSCRIBING PACKAGE",#,"WP",#)=<line of text>
        ;"        OUT("USAGE")=<value>
        ;"        OUT("ENTERED")=<value>
        ;"        OUT("STATUS")=<value>
        ;"        OUT("EXPIRES")=<value>
        ;"        OUT("DURATION")=<value>
        ;"        OUT("VERSION")=<value>
        ;"        OUT("FILE")=<value>
        ;"        OUT("ROOT")=<value>
        ;"        OUT("TYPE")=<value>
        ;"        OUT("DESCRIPTION","WP",#)=<line of text>
        ;"        OUT("ROUTINE",#)=<name>
        ;"        OUT("ROUTINE",#,"COMPONENT",#)=<name>
        ;"        OUT("ROUTINE",#,"COMPONENT",#,"VARS",#)=<name>
        ;"        OUT("ROUTINE",#,"COMPONENT",#,"VARS",#,"TYPE")=
        ;"        OUT("ROUTINE",#,"COMPONENT",#,"VARS",#,"WP",#)=<line of text>
        ;"        OUT("ROUTINE",#,"COMPONENT",#,"WP",#)=<line of text>
        ;
        KILL OUT
        NEW LINE,IDX SET IDX=0
        SET IDX=$ORDER(PART(IDX)),LINE=$GET(PART(IDX))
        SET OUT("NUM")=+$$TRIM^XLFSTR(LINE)
        NEW DONE SET DONE=0
        SET IDX=IDX-1
        FOR  SET IDX=$ORDER(PART(IDX)) QUIT:(IDX="")!(DONE)  DO
        . SET LINE=$GET(PART(IDX))
        . NEW FOUND,TAG,VALUE
        . FOR  DO  QUIT:FOUND=0    ;"Line can have multiple tags in it....
        . . SET FOUND=$$GETFLD(LINE,.TAG,.VALUE) QUIT:FOUND=0
        . . SET LINE=VALUE
        . . NEW FLDI SET FLDI=$ORDER(FLDS("B",TAG,"")) QUIT:+FLDI'>0
        . . NEW ROUTINE SET ROUTINE=$GET(FLDS(FLDI,"R"),"STD")
        . . NEW F2,T2,V2
        . . SET F2=$$GETFLD(VALUE,.T2,.V2)
        . . IF F2=1 SET VALUE=$PIECE(VALUE,T2_":",1)
        . . SET VALUE=$$TRIM^XLFSTR(VALUE)
        . . DO @ROUTINE
        . . IF +IDX'>0 SET DONE=1
        ;
        QUIT
        ;
GETFLD(LINE,TAG,VALUE) ;
        ;"Purpose: to determine if line contains a tag.  If so then tag name
        ;"         is returned, and also the rest of the line (may containe other tags)
        ;"Input: LINE -- The line to scan
        ;"       TAG -- AN OUT PARAMETER.  The name of the tag found
        ;"       VALUE -- AN OUT PARAMETER.  The text right after the tag, to the end of the line.
        ;"       Uses FLDS by global scope.
        ;"Result: 1 if found, 0 if not found. 
        NEW RESULT SET RESULT=0
        SET (TAG,VALUE)=""
        NEW FLDI SET FLDI=0
        FOR  SET FLDI=$ORDER(FLDS(FLDI)) QUIT:(+FLDI'>0)!(RESULT=1)  DO
        . NEW FLDNAME SET FLDNAME=$GET(FLDS(FLDI))
        . QUIT:(LINE'[FLDNAME)
        . SET TAG=$$TRIM^XLFSTR($PIECE(FLDNAME,":",1))
        . SET VALUE=$$TRIM^XLFSTR($PIECE(LINE,FLDNAME,2))
        . SET RESULT=1
        QUIT RESULT
        ;
 ;"===============================================================
 ;"===============================================================
 ;
STOREARR(ARR,FLDS) ;" STORE ARRAY
        ;"Purpose: To store ARR into file 22721 -- TMG INTEGRATION AGREEMENTS
        ;"         NOTE: Any pre-existing record will be deleted.
        ;"Input:  PASS BY REFERENCE
        ;"        ARR("NAME")=<name>
        ;"        ARR("NUM")=<Agreement #>
        ;"        ARR("CUSTODIAL PACKAGE")=<name>
        ;"        ARR("SUBSCRIBING PACKAGE",#)=<name>
        ;"        ARR("SUBSCRIBING PACKAGE",#,"WP",#)=<line of text>
        ;"        ARR("USAGE")=<value>
        ;"        ARR("ENTERED")=<value>
        ;"        ARR("STATUS")=<value>
        ;"        ARR("EXPIRES")=<value>
        ;"        ARR("DURATION")=<value>
        ;"        ARR("VERSION")=<value>
        ;"        ARR("FILE")=<value>
        ;"        ARR("ROOT")=<value>
        ;"        ARR("TYPE")=<value>
        ;"        ARR("DESCRIPTION","WP",#)=<line of text>
        ;"        ARR("ROUTINE",#)=<name>
        ;"        ARR("ROUTINE",#,"COMPONENT",#)=<name>
        ;"        ARR("ROUTINE",#,"COMPONENT",#,"VARS",#)=<name>
        ;"        ARR("ROUTINE",#,"COMPONENT",#,"VARS",#,"TYPE")=
        ;"        ARR("ROUTINE",#,"COMPONENT",#,"VARS",#,"WP",#)=<line of text>
        ;"        ARR("ROUTINE",#,"COMPONENT",#,"WP",#)=<line of text>
        ;"FLDS -- PASS BY REFERENCE.  Array of fields, as set up by SETFLDS() 
        ;"Result: IEN^OK, or -1^Error Message 
        NEW NUM SET NUM=+$GET(ARR("NUM"))
        NEW TMGRESULT SET TMGRESULT=0
        NEW TMGIENS,TMGFDA,TMGIEN,TMGMSG        
        IF NUM'>0 DO  GOTO STRDN
        . SET TMGRESULT="-1^Number for record not found."
        ;"if record already exists, then delete prior one. 
        IF $DATA(^TMG(22721,NUM)) DO  GOTO:(+TMGRESULT<0) STRDN
        . SET TMGFDA(22721,NUM_",",.01)="@" 
        . DO FILE^DIE("E","TMGFDA","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) DO  QUIT
        . . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        ;
        SET TMGIENS="+1,"
        SET TMGIEN(1)=NUM
        NEW FLDNUM SET FLDNUM=0
        FOR  SET FLDNUM=$ORDER(FLDS("FLD",FLDNUM)) QUIT:+FLDNUM'>0  DO
        . NEW FLDNAME SET FLDNAME=$PIECE($GET(FLDS("FLD",FLDNUM)),"^",1)
        . NEW MULT SET MULT=$PIECE($GET(FLDS("FLD",FLDNUM)),"^",2) QUIT:MULT'=""
        . NEW VALUE SET VALUE=$GET(ARR(FLDNAME))
        . IF VALUE'="" SET TMGFDA(22721,TMGIENS,FLDNUM)=VALUE
        IF $DATA(TMGFDA)=0 GOTO STRDN
        DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO STRDN
        . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        NEW IEN SET IEN=+$GET(TMGIEN(1)) IF +IEN'>0 DO  GOTO STRDN
        . SET TMGRESULT="-1^Unable to determine IEN of added subrec for TMG INTEGRATION AGREEMENT."
        SET TMGRESULT=IEN_"^OK"
        ;
        ;"-- Next file Description --
        NEW REF SET REF=$NAME(ARR("DESCRIPTION","WP"))
        DO TRIMWP(REF) 
        IF $DATA(@REF)>0 DO 
        . DO WP^DIE(22721,IEN_",",2,"",REF,"TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) DO  
        . . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        IF +TMGRESULT<0 GOTO STRDN
        ;
        ;"-- Next file Multiple entries for Subscribers --
        NEW SUBARR MERGE SUBARR=ARR("SUBSCRIBING PACKAGE")
        SET TMGRESULT=$$STORESUB(.SUBARR,IEN_",")
        IF +TMGRESULT<0 GOTO STRDN
        ;
        ;"-- Next file Multiple entries for Routine --
        NEW RTNINFO MERGE RTNINFO=ARR("ROUTINE")
        SET TMGRESULT=$$STORERTN(.RTNINFO,IEN_",")
        IF +TMGRESULT<0 GOTO STRDN
        ;
STRDN   QUIT TMGRESULT        
        ;
STORESUB(ARR,IENS) ;
        ;"Purpose: Store SUBSCRIBERS field info. 
        ;"        ARR(#)=<name>
        ;"        ARR(#,"WP",#)=<line of text>
        NEW TMGRESULT SET TMGRESULT=0
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:(+IDX'>0)!(+TMGRESULT<0)  DO
        . NEW TMGFDA,TMGIEN,TMGMSG
        . NEW NAME SET NAME=$GET(ARR(IDX)) IF NAME="" SET NAME="<unspecified>"
        . SET TMGFDA(22721.01,"+1,"_NUM_",",.01)=NAME
        . DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) DO  QUIT
        . . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        . NEW IEN SET IEN=+$GET(TMGIEN(1)) IF +IEN'>0 DO  QUIT 
        . . SET TMGRESULT="-1^Unable to determine IEN of added subrec for SUBSCRIBING PACKAGE."
        . NEW REF SET REF=$NAME(ARR(IDX,"WP"))
        . DO TRIMWP(REF) 
        . IF $DATA(@REF)>0 DO
        . . KILL TMGMSG
        . . DO WP^DIE(22721.01,IEN_","_IENS,1,"",REF,"TMGMSG")
        . . IF $DATA(TMGMSG("DIERR")) DO  QUIT
        . . . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        QUIT TMGRESULT
        ;        
STORERTN(ARR,IENS) ;
        ;"Purpose: Store ROUTINE field info.
        ;"Input: ARR -- PASS BY REFERENCE.
        ;"        ARR(#)=<name>
        ;"        ARR(#,"COMPONENT",#)=<name>
        ;"        ARR(#,"COMPONENT",#,"VARS",#)=<name>
        ;"        ARR(#,"COMPONENT",#,"VARS",#,"TYPE")=
        ;"        ARR(#,"COMPONENT",#,"VARS",#,"WP",#)=<line of text>
        ;"        ARR(#,"COMPONENT",#,"WP",#)=<line of text>
        ;"       IEN -- the parent IENS to store info into
        ;"FLDS -- PASS BY REFERENCE.  Array of fields, as set up by SETFLDS() 
        ;"Result: IEN^OK, or -1^Error Message
        NEW TMGRESULT SET TMGRESULT=0
        SET IDX=0
        FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:(+IDX'>0)!(+TMGRESULT<0)  DO
        . NEW TMGFDA,TMGIEN,TMGMSG
        . NEW NAME SET NAME=$GET(ARR(IDX)) IF NAME="" SET NAME="<unspecified>"
        . SET TMGFDA(22721.03,"+1,"_IENS,.01)=NAME
        . DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) DO  QUIT
        . . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        . NEW IEN SET IEN=+$GET(TMGIEN(1)) IF +IEN'>0 DO  QUIT 
        . . SET TMGRESULT="-1^Unable to determine IEN of added subrec for ROUTINE."
        . NEW TMGIENS SET TMGIENS=IEN_","_IENS
        . NEW COMPARR MERGE COMPARR=ARR(IDX,"COMPONENT")
        . SET TMGRESULT=$$STORCOMP(.COMPARR,TMGIENS)
        . KILL ARR(IDX)
        QUIT TMGRESULT
        ;
STORCOMP(ARR,IENS) ;
        ;"Purpose: Store ROUTINE field info.
        ;"Input: ARR -- PASS BY REFERENCE.
        ;"        ARR(#)=<name>
        ;"        ARR(#,"VARS",#)=<name>
        ;"        ARR(#,"VARS",#,"TYPE")=
        ;"        ARR(#,"VARS",#,"WP",#)=<line of text>
        ;"        ARR(#,"WP",#)=<line of text>
        ;"       IENS -- the parent IENS to store info into
        ;"FLDS -- PASS BY REFERENCE.  Array of fields, as set up by SETFLDS() 
        ;"Result: IEN^OK, or -1^Error Message
        NEW TMGRESULT SET TMGRESULT=0
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:(+IDX'>0)!(+TMGRESULT<0)  DO        
        . NEW TMGFDA,TMGIEN,TMGMSG
        . NEW NAME SET NAME=$GET(ARR(IDX)) IF NAME="" SET NAME="<unspecified>"
        . SET TMGFDA(22721.31,"+1,"_IENS,.01)=NAME
        . DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) DO  QUIT
        . . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        . NEW IEN SET IEN=+$GET(TMGIEN(1)) IF +IEN'>0 DO  QUIT 
        . . SET TMGRESULT="-1^Unable to determine IEN of added subrec for COMPONENT."
        . ;
        . NEW REF SET REF=$NAME(ARR(IDX,"WP"))
        . DO TRIMWP(REF)         
        . IF $DATA(@REF)>0 DO  QUIT:(+TMGRESULT<0)
        . . KILL TMGMSG
        . . DO WP^DIE(22721.31,IEN_","_IENS,.02,"",REF,"TMGMSG")
        . . IF $DATA(TMGMSG("DIERR")) DO  QUIT
        . . . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        . ;
        . NEW VARSARR MERGE VARSARR=ARR(IDX,"VARS")
        . NEW TMGIENS SET TMGIENS=IEN_","_IENS
        . SET TMGRESULT=$$STORVARS(.VARSARR,TMGIENS)
        . KILL ARR(IDX)        
        QUIT TMGRESULT
        ;
STORVARS(ARR,IENS) ;
        ;"Purpose: Store ROUTINE field info.
        ;"Input: ARR -- PASS BY REFERENCE.
        ;"        ARR(#)=<name>
        ;"        ARR(#,"TYPE")=
        ;"        ARR(#,"WP",#)=<line of text>
        ;"       IENS -- the parent IENS to store info into
        ;"FLDS -- PASS BY REFERENCE.  Array of fields, as set up by SETFLDS() 
        ;"Result: IEN^OK, or -1^Error Message
        NEW TMGRESULT SET TMGRESULT=0
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:(+IDX'>0)!(+TMGRESULT<0)  DO
        . NEW TMGFDA,TMGIEN,TMGMSG
        . NEW NAME SET NAME=$GET(ARR(IDX)) IF NAME="" SET NAME="<unspecified>"
        . SET TMGFDA(22721.32,"+1,"_IENS,.01)=NAME
        . NEW TYPE SET TYPE=$GET(ARR(IDX,"TYPE"))
        . IF TYPE'="" SET TMGFDA(22721.32,"+1,"_IENS,.02)=TYPE
        . DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) DO  QUIT
        . . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        . NEW IEN SET IEN=+$GET(TMGIEN(1)) IF +IEN'>0 DO  QUIT 
        . . SET TMGRESULT="-1^Unable to determine IEN of added subrec for VARIABLES."
        . ;
        . NEW REF SET REF=$NAME(ARR(IDX,"WP"))
        . DO TRIMWP(REF)         
        . IF $DATA(@REF)>0 DO  QUIT:(+TMGRESULT<0)
        . . KILL TMGMSG
        . . DO WP^DIE(22721.32,IEN_","_IENS,1,"",REF,"TMGMSG")
        . . IF $DATA(TMGMSG("DIERR")) DO  QUIT
        . . . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        . KILL ARR(IDX)
        QUIT TMGRESULT
        ;
TRIMWP(REF) ;
        NEW DONE SET DONE=0
        NEW IDX SET IDX=""
        FOR  SET IDX=$ORDER(@REF@(IDX),-1) QUIT:(IDX="")!(DONE)  DO
        . SET DONE=($$TRIM^XLFSTR($GET(@REF@(IDX)))'="") QUIT:DONE
        . KILL @REF@(IDX)
        QUIT
 ;"===============================================================
 ;"===============================================================
        ;
 ;"===============================================================
 ;"For Field handlers, inputs as follows
 ;"Input (All values are by global scope)
 ;"      VALUE -- the value to file
 ;"      TAG -- The field name
 ;"      OUT -- Array for output.
 ;"      PART -- the Array with text
 ;"      IDX -- the index of the parsing line from PART
 ;"===============================================================
STD ;
        SET OUT(TAG)=VALUE
        QUIT
        ;
DESCR ;"DESCRIPTION
        NEW WP SET WPI=1
        NEW DONE SET DONE=0
        FOR  DO  QUIT:DONE  
        . SET IDX=$ORDER(PART(IDX)) QUIT:DONE
        . NEW LINE SET LINE=$GET(PART(IDX))
        . SET DONE=(LINE["ROUTINE:") IF DONE SET IDX=$ORDER(PART(IDX),-1) QUIT
        . SET WP(WPI)=LINE,WPI=WPI+1
        IF $DATA(WP) DO
        . MERGE OUT(TAG,"WP")=WP
        QUIT
SUBSCR ;":SUBSCRIBERS
        IF $$TRIM^XLFSTR(VALUE)="" SET OUT(TAG,1)="" GOTO SCRDN
        ;"NEW I2 SET I2=IDX
        NEW SUBN SET SUBN=1
        NEW LINE SET LINE=$GET(PART(IDX))
        NEW INDENT SET INDENT=$LENGTH($PIECE(LINE,VALUE,1))
        NEW DONE SET DONE=0
        FOR SUBN=1:1 DO  QUIT:DONE  
        . SET OUT(TAG,SUBN)=VALUE
        . NEW WP SET WPI=1
        . NEW DONE2 SET DONE2=0
        . FOR  SET IDX=$ORDER(PART(IDX)) QUIT:DONE2  DO
        . . SET LINE=$GET(PART(IDX))
        . . NEW WHITE SET WHITE=$$TRIM^XLFSTR($EXTRACT(LINE,1,INDENT))
        . . SET (DONE,DONE2)=(WHITE'="") IF DONE2 SET IDX=IDX-2 QUIT
        . . SET LINE=$EXTRACT(LINE,INDENT+1,$LENGTH(LINE))
        . . IF $EXTRACT(LINE,1)'=" " SET DONE2=1,VALUE=LINE QUIT
        . . SET LINE=$EXTRACT(LINE,4,$LENGTH(LINE))
        . . SET WP(WPI)=LINE,WPI=WPI+1
        . IF $DATA(WP) DO
        . . MERGE OUT(TAG,SUBN,"WP")=WP
SCRDN   QUIT
        
        ;
ROUTINE ;
        NEW RTNNUM SET RTNNUM=1
        IF $$TRIM^XLFSTR(VALUE)="" SET VALUE="<unspecified>"
        SET OUT(TAG,RTNNUM)=VALUE
        NEW SUBN SET SUBN=1
        SET IDX=$ORDER(PART(IDX)) IF +IDX'>0 GOTO RTNDN
        NEW LINE SET LINE=$GET(PART(IDX))
        NEW COMPNUM SET COMPNUM=0
        NEW DONE SET DONE=0
        FOR  QUIT:LINE'["COMPONENT:"  DO  QUIT:DONE  
        . SET COMPNUM=COMPNUM+1
        . SET VALUE=$$TRIM^XLFSTR($PIECE(LINE,"COMPONENT:",2))
        . SET OUT(TAG,RTNNUM,"COMPONENT",COMPNUM)=VALUE
        . DO COMPONENT(RTNNUM,COMPNUM,.LINE)
        . IF +IDX'>0 SET DONE=1
RTNDN   QUIT
        ;
COMPONENT(RTNNUM,COMPNUM,LINE) ;
        NEW VALUE SET VALUE=$$TRIM^XLFSTR($PIECE(LINE,"COMPONENT:",2))
        SET OUT(TAG,RTNNUM,"COMPONENT",COMPNUM)=VALUE
        SET IDX=$ORDER(PART(IDX)) IF +IDX'>0 GOTO CMPDN
        SET LINE=$GET(PART(IDX))
        NEW VARTAG SET VARTAG="VARIABLES:"
        IF LINE[VARTAG  DO
        . SET LINE=$PIECE(LINE,VARTAG,1)_"          "_$PIECE(LINE,VARTAG,2)
        . DO VARS(RTNNUM,COMPNUM,.LINE)
        IF +IDX'>0 GOTO CMPDN
        NEW WP,WPI SET WPI=1
        NEW DONE SET DONE=0
        FOR  DO  QUIT:(INDENT<18)!DONE
        . SET INDENT=$LENGTH(LINE)-$LENGTH($$TRIM^XLFSTR(LINE,"L")) 
        . IF $$TRIM^XLFSTR(LINE)="" SET INDENT=999
        . IF INDENT<18 SET DONE=1 QUIT
        . SET LINE=$$TRIM^XLFSTR(LINE)
        . SET WP(WPI)=LINE,WPI=WPI+1
        . SET IDX=$ORDER(PART(IDX)) IF +IDX'>0 SET DONE=1 QUIT
        . SET LINE=$GET(PART(IDX))
        IF $DATA(WP) MERGE OUT(TAG,RTNNUM,"COMPONENT",COMPNUM,"WP")=WP
CMPDN   QUIT
        ;
VARS(RTNNUM,COMPNUM,LINE) ;
        SET LINE=$GET(LINE)
        IF $$TRIM^XLFSTR(LINE)="" GOTO VARDN
        NEW INDENT
        NEW VARNUM SET VARNUM=1
        NEW DONE SET DONE=0
        FOR  DO  QUIT:DONE
        . SET INDENT=$LENGTH(LINE)-$LENGTH($$TRIM^XLFSTR(LINE,"L"))
        . IF INDENT'=15 SET DONE=1 QUIT
        . SET VARNUM=VARNUM+1
        . DO ONEVAR(RTNNUM,COMPNUM,VARNUM,.LINE)
        . IF +IDX'>0 SET DONE=1
VARDN   QUIT
        ;
ONEVAR(RTNNUM,COMPNUM,VARNUM,LINE) ;
        NEW NAME SET NAME=$$TRIM^XLFSTR(LINE,"L")
        SET NAME=$$TRIM^XLFSTR($PIECE(LINE,"Type:",1))
        NEW TYPE SET TYPE=$$TRIM^XLFSTR($PIECE(LINE,"Type:",2))
        SET OUT(TAG,RTNNUM,"COMPONENT",COMPNUM,"VARS",VARNUM)=NAME
        SET OUT(TAG,RTNNUM,"COMPONENT",COMPNUM,"VARS",VARNUM,"TYPE")=TYPE
        NEW WP,WPI SET WPI=1
        NEW DONE SET DONE=0
        FOR  SET IDX=$ORDER(PART(IDX)) DO  QUIT:DONE!(+IDX'>0)
        . SET LINE=$GET(PART(IDX))
        . IF LINE[EOFTAG SET DONE=1 QUIT
        . SET INDENT=$LENGTH(LINE)-$LENGTH($$TRIM^XLFSTR(LINE,"L"))
        . IF $$TRIM^XLFSTR(LINE)="" SET INDENT=999
        . IF INDENT<36 SET DONE=1 QUIT
        . SET WP(WPI)=$$TRIM^XLFSTR(LINE),WPI=WPI+1
        IF $DATA(WP) MERGE OUT(TAG,RTNNUM,"COMPONENT",COMPNUM,"VARS",VARNUM,"WP")=WP
        QUIT
        ;
 ;"===============================================================
 ;"===============================================================
        ;       
SETFLDS(ARR) ;"SET UP ARRAY OF FIELD NAMES
        ;;"NAME^.01
        ;;"CUSTODIAL PACKAGE^.02
        ;;"SUBSCRIBING PACKAGE^.03^M^SUBSCR
        ;;"USAGE^.04
        ;;"ENTERED^.05
        ;;"STATUS^.06
        ;;"EXPIRES^.07
        ;;"DURATION^.08
        ;;"VERSION^.09
        ;;"FILE^1
        ;;"ROOT^1.1
        ;;"DESCRIPTION^2^M^DESCR
        ;;"TYPE^1.2
        ;;"ROUTINE^3^M^ROUTINE
        ;;"<DONE>
        KILL ARR
        NEW JDX SET JDX=1
        NEW DONETAG SET DONETAG="<DONE>"
        NEW IDX,TAG SET TAG=""
        FOR IDX=1:1 DO  QUIT:TAG=DONETAG
        . SET TAG=$TEXT(SETFLDS+IDX^TMGFIX5)
        . SET TAG=$PIECE(TAG,";;""",2)
        . NEW FLDNUM SET FLDNUM=+$PIECE(TAG,"^",2)
        . NEW MULT SET MULT=$PIECE(TAG,"^",3)
        . NEW ROUTINE SET ROUTINE=$PIECE(TAG,"^",4)
        . SET TAG=$PIECE(TAG,"^",1)
        . IF TAG=DONETAG QUIT
        . SET ARR(JDX)=TAG_":"
        . SET ARR("B",TAG,JDX)=""
        . SET ARR("FLD",FLDNUM)=TAG_"^"_MULT
        . IF ROUTINE'="" SET ARR(JDX,"R")=ROUTINE
        . SET JDX=JDX+1
        QUIT
