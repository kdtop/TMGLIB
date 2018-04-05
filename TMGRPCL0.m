TMGRPCL0 ;TMG/kst/CPRS Lab Entry support routines ; 9/4/13, 2/2/14M 3/21/15
         ;;1.0;TMG-LIB;**1**;09/04/13
 ;
 ;"TMG CPRS LAB ENTRY ROUTINES
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
 ;"LISTALL(TMGOUT,FROM,DIR) -- Return a bolus of lab names.
 ;"LISTSPEC(TMGOUT,FROM,DIR) -- Return a SET of specimens from TOPOGRAPHY FIELD file.
 ;"CHLDINFO(TMGOUT,IEN) -- Get info regarding child complents for a lab, IF any. (RPC CALL) 
 ;"DFLTSPEC(TMGOUT,IEN60) --GET DEFAULT SPECIMEN (FILE 61) FOR GIVEN LAB TEST (IEN60) (RPC CALL)
 ;"GETGRPS(TMGOUT) -- GET LAB GROUPINGS
 ;"GRPLABS(TMGOUT,GRPIEN) -- GET LABS FOR ONE GROUP
 ;
 ;"=======================================================================
 ;" Private Functions.
 ;"=======================================================================
 ;"PREPIDX(STR,TMGOUT) -- PREP INDEX FOR LABS
 ;"PRPIDX2(STR,TMGOUT) -- PREP INDEX FOR SPECIMENS (from TOPOGRAPHY FIELD file)
 ;"DECODFRM(STR)  -- DECODE 'FROM' STRING, e.g. 'URIND~' --> 'URINE'
 ;"ACTVCOMP(IEN) -- Return IF lab has at least one component that has a specified storage location
 ;"ADDONE(IEN,UPSTR,NAME,STORE,TMGOUT) -- Add one item to idx list.
 ;"CHLDINFO(TMGOUT,IEN) -- Get info regarding child complents for a lab, IF any. (RPC CALL)
 ;"LISTINST(TMGOUT,FROM,DIR) -- Return a SET of Institutions
 ;"POSTLABS(TMGRESULT,INARRAY) -- Post lab results, as passed in from CPRS client
 ;"FD4IEN60(IEN60)  -- GET STORAGE FIELD FOR IEN60
 ;"
 ;"=======================================================================
 ;"Dependancies:
 ;"  TMGDEBUG
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;
LISTALL(TMGOUT,FROM,DIR)        ;" Return a bolus of lab names.  (RPC CALL)  
        ;"Purpose: Return a bolus of LAB names for TMG CPRS GET LAB LIST rpc call
        ;"Input: TMGOUT --  An OUT PARAMETER, PASS BY REFERENCE.  Format:
        ;"         TMGOUT(#)=IEN60^text^FullName^StorageInfo
        ;"                text format: '<user input> -- full name
        ;"                e.g. 'CBC -- LMH-CBC'  
        ;"       FROM -- User specified string to search from.
        ;"                Note: CPRS decrements the terminal character of user
        ;"                   input, and adds a ~
        ;"       DIR -- should be 1 or -1
        ;"Results: none 
        ;"
        NEW IDXARR DO PREPIDX(FROM,.IDXARR)
        NEW IDX SET IDX=0
        SET DIR=+$GET(DIR,1)
        NEW CNT SET CNT=44 ;"number of max items to return
        NEW ENTRY SET ENTRY=FROM
        FOR  SET ENTRY=$ORDER(IDXARR(ENTRY),DIR) QUIT:(ENTRY="")!(IDX>CNT)  DO
        . NEW IEN SET IEN=0
        . FOR  SET IEN=$ORDER(IDXARR(ENTRY,IEN)) QUIT:(IEN'>0)!(IDX>CNT)  DO
        . . NEW NAMEANDSTORE SET NAMEANDSTORE=$GET(IDXARR(ENTRY,IEN))  ;"this is 2 pieces
        . . SET IDX=IDX+1,TMGOUT(IDX)=IEN_"^"_ENTRY_"^"_NAMEANDSTORE
        QUIT
        ; 
PREPIDX(STR,TMGOUT) ;" PREP INDEX FOR LABS
        ;"Purpose: Create list of resultable labs for display in CPRS
        ;"         -This will be a list of every LABORATORY test record for
        ;"         which a storage location (data name) has been specified.
        ;"         -It will also include all panel tests which contain at
        ;"         least one component test that has a specified storage
        ;"         location.
        ;"         -This will include entries that *contain* STR, so that
        ;"        'LMH-CBC', for example, will be include in list with STR='CBC' 
        ;"        Also, If input has space, then check for each entry sepearately so, 
        ;"             e.g.  'URINE DRUG' AND 'DRUG,URINE' will both be found
        ;"INPUT: STR -- This is the user input so far
        ;"                Example of Input: 'CBB~'  <-- IF user in CPRS typed 'CBC'
        ;"                Note: CPRS decrements the terminal character of user
        ;"                   input, and adds a ~
        ;"       TMGOUT -- PASS BY REFERENCE.  AN OUT PARAMETER.  Format:
        ;"            TMGOUT(TEXT,IEN)=FullName^StorageInfo
        ;"                TEXT format: '<user input> -- <full name>'
        ;"                e.g. 'CBC -- LMH-CBC'
        NEW UPSTR SET UPSTR=$$DECODFRM(STR)  
        ;"---- 9/29/13 mod for multipart lookup
        NEW FILTARR
        NEW PNUM FOR PNUM=1:1:$LENGTH(UPSTR," ") DO
        . NEW FILTERPART SET FILTERPART=$PIECE(UPSTR," ",PNUM)
        . SET FILTERPART=$$UP^XLFSTR(FILTERPART)
        . IF FILTERPART="" SET FILTERPART=" "  
        . SET FILTARR(FILTERPART)=""
        ;"---- 9/29/13 end mod for multipart lookup
        NEW STORE SET STORE=""
        NEW IEN SET IEN=0
        FOR  SET IEN=$ORDER(^LAB(60,IEN)) QUIT:(+IEN'>0)  DO
        . NEW NAME SET NAME=$PIECE($GET(^LAB(60,IEN,0)),"^",1)
        . SET NAME=$$UP^XLFSTR(NAME)
        . SET STORE=$PIECE($GET(^LAB(60,IEN,0)),"^",5)
        . IF (STORE="")&($$ACTVCOMP(IEN)=0) QUIT
        . ;"---- 9/29/13 mod for multipart lookup
        . NEW FILTERPART SET FILTERPART=""
        . NEW OK SET OK=1
        . FOR  SET FILTERPART=$ORDER(FILTARR(FILTERPART)) QUIT:(FILTERPART="")!(OK=0)  DO
        . . SET OK=(NAME[FILTERPART)
        . . IF OK=0 QUIT
        . IF OK DO ADDONE(IEN,UPSTR,NAME,STORE,.TMGOUT)
        . ;"---- 9/29/13 end mod for multipart lookup
        . ;"//kt original --> IF NAME[UPSTR DO ADDONE(IEN,UPSTR,NAME,STORE,.TMGOUT)
        . NEW SYNIEN SET SYNIEN=0
        . FOR  SET SYNIEN=$ORDER(^LAB(60,IEN,5,SYNIEN)) QUIT:(SYNIEN'>0)  DO
        . . NEW SYNNAME SET SYNNAME=$PIECE($GET(^LAB(60,IEN,5,SYNIEN,0)),"^",1)
        . . SET SYNNAME=$$UP^XLFSTR(SYNNAME)
        . . ;"---- 9/29/13 mod for multipart lookup
        . . NEW FILTERPART SET FILTERPART=""
        . . NEW OK SET OK=1
        . . FOR  SET FILTERPART=$ORDER(FILTARR(FILTERPART)) QUIT:(FILTERPART="")!(OK=0)  DO
        . . . SET OK=(NAME[FILTERPART)
        . . . IF OK=0 QUIT
        . . IF 'OK QUIT
        . . ;"---- 9/29/13 end mod for multipart lookup
        . . ;"//kt original --> IF SYNNAME'[UPSTR QUIT
        . . DO ADDONE(IEN,UPSTR,SYNNAME_" {"_NAME_"}",STORE,.TMGOUT)
PIDN    QUIT
        ;
ADDONE(IEN,UPSTR,NAME,STORE,TMGOUT) ;"Add one item to idx list.
        NEW TEXT SET TEXT=UPSTR_" -- "_NAME
        SET TMGOUT(TEXT,IEN)=NAME_"^"_STORE
        QUIT
        ;
DECODFRM(STR)  ;"DECODE 'FROM' STRING, e.g. 'URIND~' --> 'URINE'
        NEW UPSTR SET UPSTR=$$UP^XLFSTR(STR)
        IF $EXTRACT(UPSTR,$LENGTH(UPSTR))="~" DO
        . SET UPSTR=$EXTRACT(UPSTR,1,$LENGTH(UPSTR)-1)
        . NEW LASTCH SET LASTCH=$EXTRACT(UPSTR,$LENGTH(UPSTR))
        . SET LASTCH=$CHAR($ASCII(LASTCH)+1)
        . SET $EXTRACT(UPSTR,$LENGTH(UPSTR))=LASTCH
        QUIT UPSTR
        ;
ACTVCOMP(IEN) ;"Return IF lab has at least one component that has a specified storage location
        ;"Input-- IEN = IEN60
        ;"Result: 1 IF has at least one component that has a specified storage location, or 0 IF not
        NEW ACTIVE SET ACTIVE=0
        NEW SUBIEN SET SUBIEN=0
        FOR  SET SUBIEN=$ORDER(^LAB(60,IEN,2,SUBIEN)) QUIT:(SUBIEN'>0)!ACTIVE  DO
        . NEW LABIEN SET LABIEN=+$PIECE($GET(^LAB(60,IEN,2,SUBIEN,0)),"^",1)
        . IF LABIEN'>0 QUIT
        . NEW STORE SET STORE=$PIECE($GET(^LAB(60,LABIEN,0)),"^",5)
        . IF $PIECE(STORE,";",1)'="CH" SET STORE=""  ;"I only want labs of type CHEM... (i.e. not micro or pathology etc)
        . SET ACTIVE=(STORE'="")
        QUIT ACTIVE
        ;
CHLDINFO(TMGOUT,IEN) ;"Get info regarding child complents for a lab, IF any. (RPC CALL) 
        ;"Input: TMGOUT --  Out parameter, pass by reference.  Format:
        ;"               TMGOUT(0)=<number values returned>
        ;"               TMGOUT(#)=ChildIEN60^^FullName^StorageInfo
        ;"       IEN -- IEN60 of parent panel lab. 
        ;"Results: none
        ;"NOTE: only components that have a specified storage location are returned.
        ;"        
        NEW IDX SET IDX=0
        NEW SUBIEN SET SUBIEN=0
        FOR  SET SUBIEN=$ORDER(^LAB(60,IEN,2,SUBIEN)) QUIT:(SUBIEN'>0)  DO
        . NEW LABIEN SET LABIEN=+$PIECE($GET(^LAB(60,IEN,2,SUBIEN,0)),"^",1)
        . IF LABIEN'>0 QUIT
        . NEW STORE SET STORE=$PIECE($GET(^LAB(60,LABIEN,0)),"^",5)
        . IF STORE="" QUIT
        . NEW NAME SET NAME=$PIECE($GET(^LAB(60,LABIEN,0)),"^",1)
        . SET IDX=IDX+1,TMGOUT(IDX)=LABIEN_"^^"_NAME_"^"_STORE
        SET TMGOUT(0)=IDX
        QUIT
        ;
LISTSPEC(TMGOUT,FROM,DIR)        ;" Return a SET of specimens Must come from TOPOGRAPHY FIELD file.
        ;"Input: TMGOUT=returned list, 
        ;"       FROM=text to $ORDER from, 
        ;"       DIR=$ORDER direction, 1 or -1
        NEW IDXARR DO PRPIDX2(FROM,.IDXARR)
        SET DIR=$GET(DIR,1)
        NEW IDX SET IDX=0
        NEW CNT SET CNT=44 ;"number of max items to return
        NEW ENTRY SET ENTRY=FROM
        FOR  SET ENTRY=$ORDER(IDXARR(ENTRY),DIR) QUIT:(ENTRY="")!(IDX>CNT)  DO
        . NEW IEN SET IEN=0
        . FOR  SET IEN=$ORDER(IDXARR(ENTRY,IEN)) QUIT:(IEN'>0)!(IDX>CNT)  DO
        . . NEW NAME SET NAME=$GET(IDXARR(ENTRY,IEN))
        . . SET IDX=IDX+1,TMGOUT(IDX)=IEN_"^"_ENTRY_"^"_NAME
        QUIT
        ;
PRPIDX2(STR,TMGOUT) ;" PREP INDEX FOR SPECIMENS (from TOPOGRAPHY FIELD file)
        ;"Purpose: Create list of specimens for display in CPRS
        ;"         -This will be a list of every TOPOGRAPHY record.
        ;"         -This will include entries that *contain* STR.
        ;"INPUT: STR -- This is the user input so far
        ;"                Example of Input: 'URIND~'  <-- IF user in CPRS typed 'URINE'
        ;"                Note: CPRS decrements the terminal character of user
        ;"                   input, and adds a ~
        ;"       TMGOUT -- PASS BY REFERENCE.  AN OUT PARAMETER.  Format:
        ;"            TMGOUT(TEXT,IEN)=FullName
        ;"                TEXT format: '<user input> -- <full name>'
        ;"                e.g. 'URINE-- 24 HR. URINE'
        ;"                e.g. 'URINE-- URINE'
        NEW UPSTR SET UPSTR=$$DECODFRM(STR)  ;
        NEW NAME SET NAME=""
        FOR  SET NAME=$ORDER(^LAB(61,"B",NAME)) QUIT:(NAME="")  DO
        . IF NAME'[UPSTR QUIT
        . NEW TEXT SET TEXT=UPSTR_" -- "_NAME
        . NEW IEN SET IEN=0
        . FOR  SET IEN=$ORDER(^LAB(61,"B",NAME,IEN)) QUIT:(IEN'>0)  DO
        . . SET TMGOUT(TEXT,IEN)=NAME
        QUIT
        ;
LISTINST(TMGOUT,FROM,DIR)        ;" Return a SET of Institutions
        ;"Input: TMGOUT=returned list, 
        ;"       FROM=text to $ORDER from, 
        ;"       DIR=$ORDER direction, 1 or -1
        SET DIR=$GET(DIR,1)
        NEW IDX SET IDX=0
        NEW CNT SET CNT=44 ;"number of max items to return
        NEW ENTRY SET ENTRY=FROM
        FOR  SET ENTRY=$ORDER(^DIC(4,"B",ENTRY),DIR) QUIT:(ENTRY="")!(IDX>CNT)  DO
        . NEW IEN SET IEN=0
        . FOR  SET IEN=$ORDER(^DIC(4,"B",ENTRY,IEN)) QUIT:(IEN'>0)!(IDX>CNT)  DO
        . . IF $PIECE($GET(^DIC(4,IEN,"TMG")),"^",2)="Y" QUIT  ;"skip TMG INACTIVE records.  No problem IF field not defined
        . . SET IDX=IDX+1,TMGOUT(IDX)=IEN_"^"_ENTRY
        QUIT
        ;
POSTLABS(TMGRESULT,INARRAY) ;"POST LABS (RPC CALL)
        ;"Purpose: Post lab results, as passed in from CPRS client
        ;"Input: TMGRESULT -- this is an OUT parameter, and it is always passed by reference
        ;"       INARRAY -- this will be array of data sent from the GUI client.  Defined below:
        ;"          INARRAY(#)=(line of information)
        ;"          Three sections will be sent (more could be added later):
        ;"            INARRAY(#)="<METADATA>"
        ;"              Lines after this tag will be meta data, as follows
        ;"              e.g. INARRAY(#)="DT_TAKEN = 3130906.202858"
        ;"       removed-->       e.g. INARRAY(#)="DT_COMPLETED = 3130908.203019"
        ;"              e.g. INARRAY(#)="PATIENT = 123^JONES,THOMAS
        ;"              e.g. INARRAY(#)="PROVIDER = 168^Toppenberg,Kevin S^- MD"
        ;"              e.g. INARRAY(#)="LOCATION = 69^Family Phys of Greeneville"  <-- IEN 4
        ;"              e.g. INARRAY(#)="SPECIMEN = 71^URINE -- URINE^URINE"
        ;"            INARRAY(#)="<VALUES>"
        ;"              Lines after this tag will be lab values.  
        ;"              Order of pieces is as follows:
        ;"                IEN60^LabValue^Flag^RefLo^RefHi^[LabDate]^[SpecimenName]^[SpecimenIEN61]^
        ;"              e.g. INARRAY(#)="1^12.4^^0^0"
        ;"              e.g. INARRAY(#)="3^12.5^^0^0"
        ;"              e.g. INARRAY(#)="4^32^^0^0"
        ;"              e.g. INARRAY(#)="9^180^^0^0^3130908^SERUM^71^"
        ;"            INARRAY(#)="<COMMENTS>"
        ;"              Lines after this tag will be comments
        ;"              e.g. INARRAY(#)="Performed at LabCorp."
        ;"
        ;"NOTE: The date information provided in the METADATA section is the default date.
        ;"      If a different date is provide for a given lab, then this OVERRIDES the default.
        ;"NOTE: For specimen, IF the IEN is not provided, then the NAME will be used
        ;"      for lookup.
        ;"RESULT: None, but TMGOUT var TMGRESULT(0) holds a result
        SET TMGRESULT(0)="1^OK"
        NEW LABDEBUG SET LABDEBUG=0
        IF LABDEBUG=1 DO  
        . KILL INARRAY
        . MERGE INARRAY=^TMG("TMP","RPC","POSTLABS")        
        ELSE  DO    ;"QUIT  ;!!!QUIT is temp!!!
        . KILL ^TMG("TMP","RPC","POSTLABS")
        . MERGE ^TMG("TMP","RPC","POSTLABS")=INARRAY
        ;
        NEW LABS,NOTES
        NEW IEN4 SET IEN4=""
        NEW LABINST SET LABINST=""
        NEW TEMPDFN SET TEMPDFN=0        
        NEW NLTORDER SET NLTORDER=""     ;"National Lab Code
        NEW NLTRESULT SET NLTRESULT=""   ;"Result national Lab code (can have a suffix)
        NEW GROUPLABDATE SET GROUPLABDATE=""
        NEW GROUPSPECIEN SET GROUPSPECIEN=0
        NEW MODE SET MODE=""
        NEW NOTEIDX SET NOTEIDX=0
        NEW IDX SET IDX=""
        FOR  SET IDX=$ORDER(INARRAY(IDX)) QUIT:(IDX="")!(+TMGRESULT(0)<0)  DO
        . NEW STR SET STR=$GET(INARRAY(IDX)) QUIT:(STR="")
        . IF $EXTRACT(STR,1)="<" DO  QUIT
        . . SET MODE=$PIECE(STR,"<",2)
        . . SET MODE=$PIECE(MODE,">",1)
        . IF MODE="METADATA" DO
        . . NEW TAG SET TAG=$$TRIM^XLFSTR($PIECE(STR,"=",1))
        . . NEW VALUE SET VALUE=$$TRIM^XLFSTR($PIECE(STR,"=",2,99))
        . . NEW FLD SET FLD=""
        . . IF TAG="PATIENT" SET TEMPDFN=+VALUE QUIT
        . . IF TAG="DT_TAKEN" DO
        . . . ;"SET FLD=.01              ;"DATE/TIME SPECIMEN COLLECTED FROM PATIENT
        . . . ;"SET OBSDATETIME=VALUE                   
        . . . SET GROUPLABDATE=VALUE
        . . ;"IF TAG="DT_COMPLETED" DO
        . . ;". SET FLD=.03                                     ;"DATE REPORT COMPLETED
        . . IF TAG="PROVIDER" SET FLD=.1,VALUE="`"_+VALUE     ;"REQUESTING PERSON
        . . IF TAG="LOCATION" DO
        . . . SET LABINST=$PIECE(VALUE,"^",2)
        . . . SET FLD=.112,IEN4=+VALUE,VALUE="`"_+VALUE       ;"ACCESSIONING INSTITUION
        . . IF TAG="SPECIMEN",(GROUPSPEC'>0) DO
        . . . ;"SET FLD=.05      ;"SPECIMEN TYPE
        . . . NEW TEMPIEN SET TEMPIEN=$$SPECIEN(VALUE)
        . . . IF TEMPIEN'>0 SET VALUE="" QUIT
        . . . SET VALUE="`"_TEMPIEN 
        . . . SET GROUPSPECIEN=TEMPIEN
        . . IF (TAG'=""),(VALUE'="") DO
        . . . SET LABS(0,FLD)=VALUE
        . IF MODE="VALUES" DO
        . . NEW IEN60 SET IEN60=$PIECE(STR,"^",1)
        . . NEW FLD SET FLD=$$FD4IEN60(IEN60)
        . . IF +FLD=-1 SET TMGRESULT(0)=FLD QUIT
        . . NEW VALUE SET VALUE=$PIECE(STR,"^",2)
        . . IF VALUE="" QUIT
        . . NEW LABDATE SET LABDATE=$PIECE(STR,"^",6)
        . . IF (LABDATE'=""),(GROUPLABDATE="") SET GROUPLABDATE=LABDATE
        . . IF (LABDATE=""),(GROUPLABDATE'="") SET LABDATE=GROUPLABDATE 
        . . NEW LABSPECNAME SET LABSPECNAME=$PIECE(STR,"^",7)
        . . NEW LABSPECIEN SET LABSPECIEN=+$PIECE(STR,"^",8)
        . . IF (LABSPECIEN>0),(GROUPSPECIEN'>0) SET GROUPSPECIEN=LABSPECIEN
        . . IF (LABSPECIEN'>0),(GROUPSPECIEN>0) SET LABSPECIEN=GROUPSPECIEN
        . . SET LABS(LABDATE,LABSPECIEN,FLD)=VALUE
        . . NEW ABNFLAG SET ABNFLAG=$PIECE(STR,"^",3)
        . . NEW REFLO SET REFLO=$PIECE(STR,"^",4)
        . . NEW REFHI SET REFHI=$PIECE(STR,"^",5)
        . . ;"NEW XREF SET XREF=$NAME(LABS(LABDATE,LABSPECIEN,FLD,"XTRA"))
        . . NEW XREF SET XREF="XREF"
        . . SET @XREF@(1)=VALUE          ;"Test value
        . . SET @XREF@(2)=ABNFLAG        ;"Flag (H for High, L for Low, HH for very high)
        . . SET @XREF@(3)="!"            ;"(marker of subpieces divider)
        . . SET @XREF@(3,1)=NLTORDER     ;"National Lab Code
        . . SET @XREF@(3,2)=NLTRESULT    ;"Result national Lab code (can have a suffix)
        . . SET @XREF@(3,3)=""           ;"LOINC without checksum character"
        . . SET @XREF@(3,4)="0000"       ;"Workload Suffix"
        . . SET @XREF@(3,5)=""           ;"(not used)
        . . SET @XREF@(3,6)=""           ;"(not used)
        . . SET @XREF@(3,7)=IEN60        ;"IEN Pointer to File 60 ^LAB(60,
        . . SET @XREF@(4)=DUZ            ;"Verifying Technologist, pointer to file 200
        . . SET @XREF@(5)="!"            ;"(marker of subpieces divider)
        . . SET @XREF@(5,1)=""           ;"Site/Specimen, IEN pointer to file 61 ^LAB(61
        . . SET @XREF@(5,2)=REFLO        ;"Reference Low
        . . SET @XREF@(5,3)=REFHI        ;"Reference High
        . . SET @XREF@(5,4)=""           ;"Critical Low
        . . SET @XREF@(5,5)=""           ;"Critical High
        . . SET @XREF@(5,6)=""           ;"(not used)
        . . SET @XREF@(5,7)=""           ;"Units
        . . SET @XREF@(5,8)=""           ;"Type of Delta Check
        . . SET @XREF@(5,9)=""           ;"Delta Value
        . . SET @XREF@(5,10)=""          ;"Default Value
        . . SET @XREF@(5,11)=""          ;"Therapeutic Low
        . . SET @XREF@(5,11)=""          ;"Therapeutic High
        . . SET @XREF@(6)=LABDATE        ;"(not used) <-- but example holds standard FM date. 
        . . SET @XREF@(7)=""             ;"(not used)
        . . SET @XREF@(8)=""             ;"(not used)
        . . SET @XREF@(9)=IEN4           ;"Pointer to file 4, Institution performing the test
        . . SET @XREF@(10)=""            ;"HL7 Equipment Entity Identifier -- EEI
        . . SET @XREF@(11)=LABINST       ;"(not used) <-- example holds "PATHGROUP"
        . . ;"SET @XREF=$$COMPLXTR^TMGLRW01(XREF)
        . . SET LABS(LABDATE,LABSPECIEN,FLD,"XTRA")=$$COMPLXTR^TMGLRW01(XREF)
        . IF MODE="COMMENTS" DO
        . . SET NOTEIDX=NOTEIDX+1,NOTES(NOTEIDX)="|||"_STR
        . . ;"SET ALTGROUPARRAY(IDX)=STR
        IF $DATA(LABS)=0 SET TMGRESULT(0)="-1^No data found to file."
        IF TEMPDFN'>0 SET TMGRESULT(0)="-1^Patient not specified"
        IF +TMGRESULT(0)<0 GOTO PLDN
        SET LABS(0,.04)="`"_DUZ  ;"VERIFY PERSON
        SET LABS(0,.12)=2  ;"NEW PERSON CONVERSION -- affects RPC from CPRS
        NEW ADATE SET ADATE=0
        FOR  SET ADATE=$ORDER(LABS(ADATE)) QUIT:(ADATE="")!(+TMGRESULT(0)'>0)  DO
        . NEW ASPEC SET ASPEC=""
        . FOR  SET ASPEC=$ORDER(LABS(ADATE,ASPEC)) QUIT:(ASPEC="")!(+TMGRESULT(0)'>0)  DO
        . . NEW ONELABSET MERGE ONELABSET=LABS(ADATE,ASPEC)        
        . . MERGE ONELABSET=LABS(0)
        . . SET ONELABSET(.01)=ADATE     ;"DATE/TIME SPECIMEN COLLECTED FROM PATIENT
        . . SET ONELABSET(.03)=ADATE     ;"DATE REPORT COMPLETED
        . . SET ONELABSET(.05)="`"_ASPEC     ;"SPECIMEN TYPE
        . . ;"CALL OUT TO FILE DATA.        
        . . ;"Returns 1^File#:IENS IF OK, or -1^Error Msg IF any
        . . NEW TEMP SET TEMP=$$LRWRITE^TMGLRW01(TEMPDFN,.ONELABSET,"CH","ES")
        . . IF TEMP<0 SET TMGRESULT(0)=TEMP QUIT
        . . SET TEMP=$PIECE(TEMP,"^",2)
        . . NEW SUBFILE SET SUBFILE=+TEMP
        . . NEW IENS SET IENS=$PIECE(TEMP,":",2)
        . . ;"CALL OUT TO STORE COMMENTS
        . . SET TEMP=$$STORNOTE^TMGLRW01(SUBFILE,IENS,.NOTES)
        . . IF TEMP<0 SET TMGRESULT(0)=TEMP
PLDN    QUIT
        ;
SPECIEN(VALUE) ;"
        ;"Input: VALUE --  IEN^DispName^InternalName, e.g.  71^URINE -- URINE^URINE"
        ;"               or  Name^IEN
        ;"NOTE: IF IEN is not provided, then Name is used for lookup.
        NEW TMGRESULT SET TMGRESULT=0
        IF +VALUE>0 SET TMGRESULT=+VALUE GOTO SPIENDN
        NEW P1 SET P1=$PIECE(VALUE,"^",1)
        NEW P2 SET P2=$PIECE(VALUE,"^",2)
        NEW P3 SET P3=$PIECE(VALUE,"^",3)
        IF +P2=P2 SET TMGRESULT=P2 GOTO SPIENDN
        NEW NAME SET NAME=""
        IF P3'="" SET NAME=P3
        ELSE  SET NAME=P1
        SET TMGRESULT=+$ORDER(^LAB(61,"B",NAME,0))
SPIENDN QUIT TMGRESULT
        ;
FD4IEN60(IEN60)  ;"GET STORAGE FIELD FOR IEN60
        ;"Result: Field#, or -1^Error IF problem. 
        NEW TMGRESULT
        NEW STORE SET STORE=$PIECE($GET(^LAB(60,IEN60,0)),"^",5)
        IF $PIECE(STORE,";",1)'="CH" SET STORE=""  ;"I only want labs of type CHEM... (i.e. not micro or pathology etc)
        IF STORE'="" DO
        . SET TMGRESULT=$PIECE(STORE,";",2)
        ELSE  DO
        . NEW NAME SET NAME=$PIECE($GET(^LAB(60,IEN60,0)),"^",1)
        . SET TMGRESULT="-1^Valid storage not found for lab "_NAME_" ("_IEN60_")"
        QUIT TMGRESULT
        ;
DFLTSPEC(TMGOUT,IEN60) ;"GET DEFAULT SPECIMEN (FILE 61) FOR GIVEN LAB TEST (IEN60) (RPC CALL)
        ;"NOTE: Depends on custom TMG DEFAULT TOPOGRAPHY (22701) field
        ;"      If not found, or no value stored, then default is #^BLOOD
        ;"Output:  TMGOUT(0)=IEN61^Name, or -1^Message.
        ;"Result: None.
        SET TMGOUT(0)="-1^Error"
        SET IEN60=+$GET(IEN60)
        IF IEN60'>0 DO  GOTO DFSPDN
        . SET TMGOUT(0)="-1^No lab # (IEN60) specified."
        NEW TMGN SET TMGN=$GET(^LAB(60,IEN60,"TMG"))
        NEW IEN61 SET IEN61=$PIECE(TMGN,"^",2)
        IF IEN61>0 DO  GOTO DFSPDN
        . NEW NAME SET NAME=$PIECE($GET(^LAB(61,IEN61,0)),"^",1)
        . SET TMGOUT(0)=IEN61_"^"_NAME
        ;"Not found, so return BLOOD
        SET IEN61=+$ORDER(^LAB(61,"B","BLOOD",0))
        IF IEN61'>0 DO  GOTO DFSPDN
        . SET TMGOUT(0)="-1^Unable to find entry in file 61 for 'BLOOD'"
        SET TMGOUT(0)=IEN61_"^BLOOD"        
DFSPDN  QUIT
        ;
SDEFSPEC(TMGOUT,IEN60,IEN61) ;"SET DEFAULT SPECIMEN (FILE 61) FOR GIVEN LAB TEST (IEN60) (RPC CALL)
        ;"NOTE: Depends on custom TMG DEFAULT TOPOGRAPHY (22701) field
        ;"      If field found, then error returned. 
        ;"NOTE: IF lab is a panel, then all child components are also changed.
        ;"Result: TMGOUT(0)=1^Success, or -1^Message.
        SET IEN60=+$GET(IEN60)
        IF IEN60'>0 DO  GOTO SDSPCDN
        . SET TMGOUT(0)="-1^No lab # (IEN60) specified."
        SET IEN61=+$GET(IEN61)
        IF IEN61'>0 DO  GOTO SDSPCDN
        . SET TMGOUT(0)="-1^No specimen # (IEN61) specified."
        IF $DATA(^DD(60,22701))=0 DO  GOTO SDSPCDN
        . SET TMGOUT(0)="-1^Custom field 22701 in file 60 not found.  TMG Patch applied??"
        NEW TMGFDA,TMGMSG
        SET TMGFDA(60,IEN60_",",22701)=IEN61
        DO FILE^DIE("","TMGFDA","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO SDSPCDN  
        . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        SET TMGOUT(0)="1^Success"
        NEW SUBIEN SET SUBIEN=0
        FOR  SET SUBIEN=$ORDER(^LAB(60,IEN60,2,SUBIEN)) QUIT:SUBIEN'>0  DO
        . NEW SUBIEN60 SET SUBIEN60=+$PIECE($GET(^LAB(60,IEN60,2,SUBIEN,0)),"^",1)
        . QUIT:SUBIEN60'>0
        . NEW TEMPOUT
        . DO SDEFSPEC(.TEMPOUT,SUBIEN60,IEN61)
        . IF +$GET(TEMPOUT(0))'=-1 QUIT
        . IF +TMGOUT(0)=1 SET TMGOUT(0)=TEMPOUT(0) QUIT
        . SET TMGOUT(0)="-1^"_$PIECE(TMGOUT(0),"^",2)_"; "_$PIECE(TEMPOUT(0),"^",2)
SDSPCDN QUIT        
        ;
GETGRPS(TMGOUT) ;-- GET LAB GROUPINGS FOR CPRS
        ;"TMGOUT(IEN)=NAME
        NEW NAME SET NAME=""
        NEW IDX SET IDX=1
        FOR  SET NAME=$O(^TMG(22734,"B",NAME)) QUIT:NAME=""  DO
        . NEW IEN SET IEN=$O(^TMG(22734,"B",NAME,0))
        . NEW USE SET USE=$P($G(^TMG(22734,IEN,0)),"^",2)
        . IF USE'="C" QUIT
        . SET TMGOUT(IDX)=IEN_"^"_NAME
        . SET IDX=IDX+1
        QUIT
        ;"
GRPLABS(TMGOUT,GRPIEN) ;-- GET LABS FOR ONE GROUP
        ;"TMGOUT(IDX)=IEN60
        NEW IDX,IEN60,STORE,NAME
        SET IDX=0
        FOR  SET IDX=$O(^TMG(22734,GRPIEN,1,IDX)) QUIT:IDX'>0  DO
        . SET IEN60=$G(^TMG(22734,GRPIEN,1,IDX,0))
        . SET NAME=$PIECE($GET(^LAB(60,IEN60,0)),"^",1)
        . SET NAME=$$UP^XLFSTR(NAME)
        . SET STORE=$PIECE($GET(^LAB(60,IEN60,0)),"^",5)
        . SET TMGOUT(IDX)=IEN60_"^"_NAME_"^"_NAME_"^"_STORE
        QUIT
        ;"