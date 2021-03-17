TMGXMLT2 ;TMG/kst/XML Utils ;11/4/14, 6/6/17
         ;;1.0;TMG-LIB;**1**;10/26/14
 ;
 ;"TMG XML EXPORT/IMPORT Tools / UTILITY FUNCTIONS
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
 ;"GETFNUM(FILENAME) --Convert a file name into a file number
 ;"GETFNAME(FILENUM) -- Convert a file number into a file name
 ;"FLDISSF(FILENUMBER,FIELD) -- IS FIELD SUBFILE? (FIELD IS SUBFILE)        
 ;"GFLDINFO(FILENUMBER,FIELD,POUT,INFOS) -- GET FIELD INFO
 ;"GTFLDNAM(FILE,FIELDNUM) --  Convert a field number into a field name
 ;"GTFLDLST(FILE,REFARR) --  Get list of all fields for a file.
 ;"SFREF(SUBFILENUM,TMGIEN,INFO,IDX) -- GET GLOBAL REFERENCE FOR SUBFILE.
 ;"TOPFILE(SUBFILENUM)  --Return top-most file for subfilenum
 ;"READWP(FILE,IENS,FIELD,ARRAY) -- provide a shell for reading a WP with error trap, error reporting
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================

 ;"=======================================================================
 ;"=======================================================================
GETFNUM(FILENAME) ;
        ;"Purpose: Convert a file name into a file number
        ;"Input: The name of a file
        ;"Result: The filenumber, or 0 if not found.
        NEW RESULT SET RESULT=0
        SET FILENAME=$GET(FILENAME)
        IF FILENAME="" GOTO GFNUMDN
        SET RESULT=+$ORDER(^DIC("B",FILENAME,"")) ;"QUICK, exact lookup.
        IF RESULT>0 GOTO GFNUMDN
        NEW DIC,X,Y SET DIC=1,DIC(0)="M"
        SET X=FILENAME   ;"i.e. "AGENCY"
        DO ^DIC  ;"lookup filename, slower.
        SET RESULT=$PIECE(Y,"^",1) ;"Result comes back in Y ... i.e. "4.11^AGENCY"
        IF RESULT=-1 SET RESULT=0
GFNUMDN QUIT RESULT
        ;
GETFNAME(FILENUM)  ;
        ;"Purpose: Convert a file number into a file name
        ;"Input: The number of a file
        ;"Result: The file name, or "" if not found.
        NEW RESULT SET RESULT=""
        SET FILENUM=+$GET(FILENUM)
        SET RESULT=$GET(^DIC(FILENUM,0))
        IF (RESULT="")&(FILENUM[".") SET RESULT=$GET(^DD(FILENUM,0))
        SET RESULT=$PIECE(RESULT,"^",1)
        QUIT RESULT        
        ;
FLDISSF(FILENUMBER,FIELD)  ;"IS FIELD SUBFILE? (FIELD IS SUBFILE)        
        ;"PUBLIC FUNCTION
        ;"Purpose: To get if FIELD is a subfile
        ;"Input: FILENUMBER: File or subfile number
        ;"       FIELD: FIELD name or number
        ;"Result: 0 if not subfile, otherwise, returns subfile number
        NEW TMGRESULT SET TMGRESULT=0
        NEW OUT DO GFLDINFO(FILENUMBER,FIELD,"OUT")
        IF $GET(OUT("MULTIPLE-VALUED"))=1 SET TMGRESULT=+$GET(OUT("SPECIFIER"))
        QUIT TMGRESULT
        ;
GFLDINFO(FILENUMBER,FIELD,POUT,INFOS) ;"GET FIELD INFO    ;"copy at GFLDINFO^TMGDBAP3
        ;"PUBLIC FUNCTION
        ;"Purpose: To get FIELD info,
        ;"Input: FILENUMBER: File or subfile number
        ;"         FIELD: FIELD name or number
        ;"         POUT -- the NAME of the variable to put result into.
        ;"         INFOS -- [OPTIONAL] -- additional attributes of field info to be looked up
        ;"                              (as allowed by FIELD^DID).  Multiple items should be
        ;"                              separated by a semicolon (';')
        ;"                              e.g. "TITLE;LABEL;POINTER"
        ;"Output: Data is put into POUT (any thing in POUT is erased first
        ;"        i.e. @POUT@("MULTIPLE-VALUED")=X
        ;"        i.e. @POUT@("SPECIFIER")=Y
        ;"        i.e. @POUT@("TYPE")=Z
        ;"        i.e. @POUT@("STORELOC")="0;1"   <-- not from  fileman output (i.e. extra info)
        ;"      (if additional attributes were specified, they will also be in array)
        ;"Result: none
        KILL @POUT  ;"erase any old information
        IF +FIELD=0 SET FIELD=$$FLDNUM^DILFD(FILENUMBER,FIELD)
        SET @POUT@("STORELOC")=$PIECE($GET(^DD(FILENUMBER,FIELD,0)),"^",4)
        NEW TMGMSG,ATTRIBS SET ATTRIBS="MULTIPLE-VALUED;SPECIFIER;TYPE"
        IF $DATA(INFOS) SET ATTRIBS=ATTRIBS_";"_INFOS
        ;"Next, check IF  field is a multiple and get field info.
        DO FIELD^DID(FILENUMBER,FIELD,,ATTRIBS,POUT,"TMGMSG")
        IF $DATA(TMGMSG),$DATA(TMGMSG("DIERR"))'=0 DO
        . DO SHOWDIER^TMGDEBU2(.TMGMSG)
        IF $GET(@POUT@("SPECIFIER"))="MV",$GET(@POUT@("TYPE"))="VARIABLE-POINTER" DO
        . NEW IDX SET IDX=0 
        . FOR  SET IDX=$ORDER(^DD(FILENUMBER,FIELD,"V",IDX)) QUIT:IDX'>0  DO
        . . NEW P2FILE SET P2FILE=+$GET(^DD(FILENUMBER,FIELD,"V",IDX,0)) QUIT:P2FILE'>0
        . . NEW P2FGL SET P2FGL=$GET(^DIC(P2FILE,0,"GL"))
        . . SET @POUT@("VARIABLE-POINTER",P2FILE)=P2FGL
        . . SET @POUT@("VARIABLE-POINTER","GL",P2FGL)=P2FILE
        QUIT
        ;        
GTFLDNAM(FILE,FIELDNUM)  ;"GET FIELD NAME
        ;"Purpose: Convert a field number into a field name
        ;"Input: FILE -- name or number of file
        ;"       FIELDNUM -- the number of the field to convert
        ;"Result: The field name, or "" if not found.
        SET FILE=$GET(FILE)
        IF +FILE'>0 SET FILE=$$GETFNUM(FILE)
        SET FIELDNUM=+$GET(FIELDNUM)
        NEW RESULT SET RESULT=$PIECE($GET(^DD(FILE,FIELDNUM,0)),"^",1)
        QUIT RESULT
        ;
GTFLDLST(FILE,REFARR)  ;"GET FIELD LIST
        ;"Purpose: Get list of all fields for a file.
        ;"Input: FILE -- File name or number to look query.  May be a sub file number
        ;"        REFARR -- pointer to (i.e. name of) array to put data into
        ;"                  Any preexisting data in REFARR will be killed.
        ;"Output: Array will be fille with info like this:
        ;"     example: Array(.01)="" <--- shows that field .01 exists
        ;"              Array(1)=""   <--- shows that field 1 exists
        ;"              Array(2)=""   <--- shows that field 2 exists
        ;"Results:  1=OK to continue.  0=error
        NEW RESULT SET RESULT=1
        SET FILE=$GET(FILE)
        IF +FILE'>0 SET FILE=$$GETFNUM(FILE)
        IF ($GET(FILE)="")!($GET(REFARR)="") SET RESULT=0 GOTO GFLDN
        KILL @REFARR
        NEW FLD SET FLD=0
        FOR  SET FLD=$ORDER(^DD(FILE,FLD)) QUIT:FLD'>0  DO
        . SET @REFARR@(FLD)=""
        IF $DATA(@REFARR)=0 SET RESULT=0
GFLDN   QUIT RESULT
        ;
SFREF(SUBFILENUM,TMGIEN,INFO)  ;" GET GLOBAL REFERENCE FOR SUBFILE.
        ;"Input: SUBFILENUM -- Subfile number to get reference for
        ;"       TMGIEN -- PASS BY REFERENCE, AN OUT PARAMETER. Format:
        ;"          e.g. TMGIEN=2  <-- highest IEN index used
        ;"          e.g. TMGIEN("REF",1)="^GMT(142,"
        ;"          e.g. TMGIEN("REF",2)="^GMT(142,TMGIEN(1),1,"
        ;"          e.g. TMGIEN("FILE")=142.01  
        ;"       INFO -- Array as made by GETPTROT^TMGXMLUI.  Format: 
        ;"          e.g. ARRAY(142,"POINTERS OUT",.06,200)=""
        ;"          e.g. ARRAY(142.01,"POINTERS OUT",1,142.1)=""
        ;"          e.g. ARRAY(142.2,"POINTERS OUT",.01,44)=""
        ;"          e.g. ARRAY(142.2,"POINTERS OUT",.02,3.5)=""
        ;"          e.g. ARRAY("SUBFILE",142.01,142,1)=""
        ;"          e.g. ARRAY("SUBFILE",142.14,142.01,4)=""
        ;"          e.g. ARRAY("SUBFILE",142.2,142,20)=""        
        ;"Results: Open format reference to subfile data.      
        ;"       e.g. SubFile 142.14 --> "^GMT(142,TMGIEN(1),1,TMGIEN(2),1,"
        SET TMGIEN=+$GET(TMGIEN,1)
        NEW TMGRESULT SET TMGRESULT=""
        NEW PARENTFNUM SET PARENTFNUM=+$ORDER(INFO("SUBFILE",SUBFILENUM,0)) 
        IF PARENTFNUM'>0 GOTO SFRFDN
        NEW PARENTFLD SET PARENTFLD=+$ORDER(INFO("SUBFILE",SUBFILENUM,PARENTFNUM,0))
        IF PARENTFLD'>0 GOTO SFRFDN
        NEW NODE SET NODE=$PIECE(^DD(PARENTFNUM,PARENTFLD,0),"^",4)
        NEW PCE SET PCE=$PIECE(NODE,";",2) SET NODE=$PIECE(NODE,";",1)
        IF +NODE'=NODE SET NODE=""""_NODE_""""
        NEW PARENTREF SET PARENTREF=$GET(^DIC(PARENTFNUM,0,"GL"))
        IF PARENTREF'="" DO  ;"parent is normal file
        . SET TMGIEN("REF",TMGIEN)=PARENTREF
        . SET TMGRESULT=PARENTREF_"TMGIEN("_TMGIEN_"),"_NODE_","
        . SET TMGIEN=TMGIEN+1
        . SET TMGIEN("REF",TMGIEN)=TMGRESULT
        ELSE  DO  ;"parent is a subfile
        . NEW REF SET REF=$$SFREF(PARENTFNUM,.TMGIEN,.INFO)
        . SET TMGIEN("REF",TMGIEN)=REF
        . SET TMGRESULT=REF_"TMGIEN("_TMGIEN_"),"_NODE_","
        . SET TMGIEN=TMGIEN+1
        . SET TMGIEN("REF",TMGIEN)=TMGRESULT
        SET TMGIEN("FILE")=SUBFILENUM
SFRFDN  QUIT TMGRESULT
        ;      
TOPFILE(SUBFILENUM)  ;"Return top-most file for subfilenum
        NEW TMGRESULT SET TMGRESULT=SUBFILENUM
        NEW PARENTFNUM SET PARENTFNUM=+$GET(^DD(SUBFILENUM,0,"UP"))
        IF PARENTFNUM>0 SET TMGRESULT=$$TOPFILE(PARENTFNUM)
        QUIT TMGRESULT
        ;        
READWP(FILE,IENS,FIELD,ARRAY) ;
        ;"Purpose: To provide a shell for reading a WP with error trap, error reporting
        ;"Input: FILE: a number or name
        ;"         IENS: a standard IENS (i.e.  "IEN,parent-IEN,grandparent-IEN,ggparent-IEN," etc.
        ;"              Note: can just pass a single IEN (without a terminal ",")
        ;"         FIELD: a field number
        ;"         ARRAY: The array to receive WP data.  PASS BY REFERENCE
        ;"                      returned In Fileman acceptible format.
        ;"                      ARRAY will be deleted before refilling
        ;"Results: 1^OK, or -1^Message
        NEW FILENUM,FLDNUM
        NEW TMGWP,TEMP,TMGMSG
        NEW RESULT SET RESULT="1^OK"
        IF $GET(IENS)="" DO  GOTO RWPDN
        . SET RESULT="-1^IENS not provided"
        IF $EXTRACT(IENS,$LENGTH(IENS))'="," SET IENS=IENS_","
        SET FILENUM=$GET(FILE) IF +FILENUM'>0 SET FILENUM=$$GETFNUM(FILE)
        IF FILENUM'>0 DO  GOTO RWPDN
        . SET RESULT="-1^Invalid FILE passed"
        SET FLDNUM=+$GET(FIELD)  ;"doesn't support field NAMES at this time
        SET TEMP=$$GET1^DIQ(FILENUM,IENS,FLDNUM,"","TMGWP","TMGMSG")
        IF $DATA(TMSETGMSG),$DATA(TMGMSG("DIERR"))'=0 DO 
        . DO SHOWDIER^TMGDEBU2(.TMGMSG)
        . SET RESULT=0
        IF RESULT=0 GOTO RWPDN
        KILL ARRAY MERGE ARRAY=TMGWP
RWPDN   QUIT RESULT
        ;        