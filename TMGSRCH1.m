TMGSRCH1 ;TMG/kst/Search API ; 6/4/10, 2/2/14
        ;;1.0;TMG-LIB;**1**;05/19/10
        ;
 ;"UTILITIES FOR TMG FILEMAN SEARCH API
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"NOTE: this function depends on NEW version of LIST^DIC, from G. Timpson Patch
 ;"=======================================================================
 ;" RPC -- Public Functions.
 ;"=======================================================================
 ;"FNPTR(FNUMPTR) -- Resolve a FNUMPTR, finding ultimate target file
 ;"PATHTO(FROMFILE,TOFILE,COUNT) -- Find a 'path' of fields that gets from file A -->B
 ;"FLDNUM(TMGFILE,TMGNAME) --Turn a field name into number, and change FILE to pointed-to-file
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"GETFLD(STR) -- Separate field name from comparator
 ;"FIXCOMP(COMP,ERR) --Standardize value comparators, e.g. <> becomes '=
 ;"FIXCOMB(COMB,ERR) --Standardize expression combiners, e.g. | becomes OR
 ;"STDDATE(TMGDATE,ERR) --Standardized date, or report error
 ;"=======================================================================
 ;"=======================================================================
 ;"Dependencies:
 ;" ^DIC, TMGSTUTL, XLFSTR, %DT, XLFDT
 ;"=======================================================================
 ;"=======================================================================
 ;
 ;
FNPTR(FNUMPTR) ;
        ;"Puprose: To resolve a FNUMPTR, finding ultimate target file
        ;"Input: FNUMPTR: Format: FNUM:FLDA[:FLDB[:FLDC...]] FNUM is filenumber that
        ;"          contain search field, and then fields used to point to *TARGET* FILENUM
        ;"Results: -1^Error message IF error, otherwise returns pointed to file
        NEW RESULT,FILE,FLD,I,DONE
        SET FILE=+$GET(FNUMPTR)
        SET RESULT=0
        SET DONE=0
        FOR I=2:1:999 DO  QUIT:(+RESULT=-1)!(DONE=1)
        . SET FLD=$PIECE(FNUMPTR,":",I)
        . IF FLD="" SET DONE=1 QUIT
        . IF $DATA(^DD(FILE,FLD,0))=0 DO  QUIT
        . . SET RESULT="-1^Field ["_FLD_"] was not found in file ["_FILE_"]"
        . NEW FLDTYPE SET FLDTYPE=$PIECE(^DD(+FILE,+FLD,0),"^",2)
        . IF FLDTYPE'["P" DO  QUIT
        . . SET RESULT="-1^Field ["_FLD_"] does not point to another file."
        . SET FILE=+$PIECE(FLDTYPE,"P",2)
        SET RESULT=FILE
        QUIT RESULT
 ;
PATHTO(FROMFILE,TOFILE,COUNT) ;
        ;"Purpose: to find a "path" of fields that gets from file A -->B (if possible)
        ;"        E.g. From TIU DOCUMENT to PATIENT would yield ".01:.01", meaning
        ;"        that the .01 field of TIU DOCUMENT-->IHS PATIENT, and
        ;"                 .01 field of IHS PATIEN-->PATIENT
        ;"Input: FROMFILE -- The starting file number
        ;"       TOFILE -- The target file number
        ;"       COUNT -- used when calling self reiteratively.  Leave blank on first call.
        ;"Note: This fill only allow the length of the path to be 3 links long.
        ;"      Also, the search is stopped after the first link is found.
        ;"      NOTE:  If the file link is changed to be longer than 3, then
        ;"      GETAFSUB() must also be changed
        ;"Results: Returns field link, e.g. ".01;2;.01"
        SET COUNT=+$GET(COUNT)
        NEW RESULT SET RESULT=""
        SET FROMFILE=+$GET(FROMFILE)
        NEW FLD SET FLD=0
        FOR  SET FLD=$ORDER(^DD(FROMFILE,FLD)) QUIT:(+FLD'>0)!(RESULT'="")  DO
        . NEW INFO SET INFO=$PIECE($GET(^DD(FROMFILE,FLD,0)),"^",2)
        . IF INFO'["P" QUIT
        . NEW PT SET PT=+$PIECE(INFO,"P",2)
        . IF PT=FROMFILE QUIT  ;"ignore pointers to self
        . IF PT=TOFILE SET RESULT=FROMFILE_":"_FLD_"->"_TOFILE QUIT
        . IF COUNT>2 QUIT
        . NEW TEMP SET TEMP=$$PATHTO(PT,TOFILE,COUNT+1)
        . IF TEMP'="" SET RESULT=FROMFILE_":"_FLD_"->"_TEMP
        IF COUNT=0 DO
        . NEW TEMP,I
        . SET TEMP=""
        . FOR I=1:1:$LENGTH(RESULT,"->") DO
        . . NEW PART SET PART=$PIECE(RESULT,"->",I)
        . . NEW PART2 SET PART2=$PIECE(PART,":",2)
        . . IF PART2="" QUIT
        . . IF TEMP'="" SET TEMP=TEMP_":"
        . . SET TEMP=TEMP_PART2
        . SET RESULT=FROMFILE_":"_TEMP_"^"_RESULT
        QUIT RESULT
        ;
        ;
GETFLD(STR) ;
        ;"Purpose: To separate field name from comparator
        ;"Input: STR -- PASS BY REFERENCE -- the string to pull field from
        ;"Results: returns extracted field.
        NEW FLD
        IF +STR>0 DO
        . SET FLD=+STR
        . SET STR=$PIECE(STR,FLD,2,999)
        ELSE  DO
        . IF $EXTRACT(STR,1)="""" DO
        . . SET FLD=$$MATCHXTR^TMGSTUT3(STR,"""",,,"""")
        . . IF FLD'="" SET STR=$EXTRACT(STR,$LENGTH(FLD)+3,9999)
        . ELSE  DO
        . . SET FLD=""
        . . NEW P FOR P=1:1:$LENGTH(STR) QUIT:"'<>=[:{"[$EXTRACT(STR,P)  DO
        . . . SET FLD=FLD_$EXTRACT(STR,P)
        . . IF FLD'="" SET STR=$EXTRACT(STR,$LENGTH(FLD)+1,9999)
        QUIT FLD
        ;
FLDNUM(TMGFILE,TMGNAME) ;
        ;"Purpose: To turn a field name into number, and change FILE to pointed-to-file
        ;"Input: TMGFILE -- PASS BY REFERENCE.  Input is current file.  Output is NEW pointed-to-file
        ;"       TMGNAME -- PASS BY REFERENCE.  The field name to look up. Name will be cleaned up.
        NEW DIC,X,Y SET Y=0
        IF TMGNAME="" SET TMGFILE=0 GOTO FLDNDN
        SET DIC="^DD("_+TMGFILE_","
        SET TMGNAME=$$TRIM^XLFSTR(TMGNAME,," ")
        SET TMGNAME=$$TRIM^XLFSTR(TMGNAME,,"""")
        SET X=TMGNAME
        DO ^DIC
        IF +Y'>0 GOTO FLDNDN
        NEW INFO SET INFO=$PIECE($GET(^DD(+TMGFILE,+Y,0)),"^",2)
        IF INFO'["P" SET TMGFILE=0 GOTO FLDNDN
        SET TMGFILE=+$PIECE(INFO,"P",2)
FLDNDN  QUIT +Y
        ;
FIXCOMP(COMP,ERR) ;
        ;"Purpose: to standardize value comparators, e.g. <> becomes '=
        NEW RESULT SET RESULT=""
        IF COMP="=" SET RESULT=COMP GOTO FCDN
        NEW COMPSAV SET COMPSAV=COMP
        SET COMP=$$UP^XLFSTR(COMP)
        IF (COMP="<>") SET COMP="'="
        ELSE  IF (COMP=">=") SET COMP="'<"
        ELSE  IF (COMP="<=") SET COMP="'>"
        ELSE  IF (COMP="{") SET COMP="IN"
        NEW NOT
        SET NOT=$EXTRACT(COMP,1) IF NOT="'" SET COMP=$EXTRACT(COMP,2,999)
        ELSE  SET NOT=""
        IF (COMP="=")!(COMP="[")!(COMP="IN")!(COMP="<")!(COMP=">") DO
        . SET RESULT=NOT_COMP
        ELSE  SET ERR="-1^'"_COMPSAV_"' is not a valid comparator."
FCDN    QUIT RESULT
        ;
FIXCOMB(COMB,ERR) ;
        ;"Purpose: to standardize expression combiners, e.g. | becomes OR
        NEW COMBSAV SET COMBSAV=COMB
        IF (COMB="|")!(COMB="||")!(COMB="!") SET COMB="OR"
        ELSE  IF (COMB="&")!(COMB="&&") SET COMB="AND"
        ELSE  IF (COMB="'")!(COMB="ANDNOT") SET COMB="NOT"
        IF (COMB'="AND")&(COMB'="OR")&(COMB'="NOT") SET COMB=""
        IF COMB="" SET ERR="-1^'"_COMBSAV_"' is not a valid SET combiner."
        QUIT COMB
        ;
STDDATE(TMGDATE,ERR) ;
        ;"Purpose: return a standardized date, or report error
        NEW X,Y,%DT
        NEW RESULT SET RESULT=""
        SET %DT="T"
        SET X=TMGDATE
        DO ^%DT
        IF Y=-1 SET ERR="-1^Invalid date: ["_X_"]"
        ELSE  DO
        . SET RESULT=Y
        . ;"SET RESULT=$$FMTE^XLFDT(Y,5)
        QUIT RESULT
        ;
GETAFSUB(TMGOUT,TMGPARAMS) ;"GET ALLOW FILES SUBSET
        ;"Purpose: For a given file to be searched, return sublist of allowed
        ;"         related files which can be used as search terms.  NOTE: only
        ;"         files that point back to the original search file are allowed.
        ;"         NOTE: This function will return not only files that point
        ;"           directly back to search file, but also files that point to
        ;"           other files that point to search file.  In fact, there can
        ;"           be a distance of 3 files between returned file and search file.
        ;"           If this allowed distance of 3 files is changed, then PATHTO()
        ;"           must also be changed.
        ;"         NOTE: Subfiles not currently supported
        ;"Input: TMGPARAMS -- FileNum^ListStartValue^direction^MaxCount(optional, def=44)^Simple
        ;"              FileNum -- this is the search file, results must point back to this
        ;"              ListStartValue -- OPTIONAL -- text to $ORDER() from
        ;"              Direction -- $ORDER(xx,Direction) direction (should be 1 or -1) -- OPTIONAL
        ;"              MaxCount -- OPTIONAL.  Default is 44 values returned.
        ;"              Simple -- OPTIONAL  Default is 0 (false).  If 1, then
        ;"                      a very limited list of files returned, with
        ;"                      more user-friendly pseudo names
        ;"Output: TMGRESULTS is filled as follows.
        ;"            TMGRESULT(0)="1^Success" or "-1^Message"
        ;"            TMGRESULT(1)=IEN^FileName
        ;"            TMGRESULT(2)=IEN^FileName
        ;"NOTE: Any files that don't have data are excluded.
        NEW TMGFILE SET TMGFILE=+$PIECE(TMGPARAMS,"^",1)
        IF TMGFILE'>0 DO  GOTO GAFSDN
        . SET TMGOUT(0)="-1^No file number supplied"
        NEW TMGFROM SET TMGFROM=$PIECE(TMGPARAMS,"^",2)
        NEW TMGDIR SET TMGDIR=$PIECE(TMGPARAMS,"^",3)
        IF TMGDIR'=-1 SET TMGDIR=1
        NEW TMGMAXCT SET TMGMAXCT=+$PIECE(TMGPARAMS,"^",4)
        IF TMGMAXCT=0 SET TMGMAXCT=44
        NEW TMGSIMPLE SET TMGSIMPLE=+$PIECE(TMGPARAMS,"^",5)
        ;
        IF (TMGFILE=2),(TMGSIMPLE=1) DO  GOTO GAFS0
        . SET TMGOUT(1)="2^1. PATIENT INFO"
        . SET TMGOUT(2)="8925^2. NOTES"
        . SET TMGOUT(3)="120.5^3. VITALS"
        . SET TMGOUT(4)="9000010^4. VISIT"
        . SET TMGOUT(5)="9000010.18^5. LINKED CPT CODE"
        ;
        NEW TMGREF SET TMGREF=$NAME(^TMP("TMGSRCH",$J,"ALLOWED FILES",TMGFILE))
        IF $DATA(@TMGREF)=0 DO
        . DO SETUPLS(TMGREF,TMGFILE)
        NEW TMGSTARTIEN SET TMGSTARTIEN=""
        NEW TMGI SET TMGI=0
        FOR  SET TMGFROM=$ORDER(@TMGREF@("B",TMGFROM),TMGDIR) QUIT:(TMGFROM="")!(TMGI'<TMGMAXCT)  DO
        . NEW TMGIEN SET TMGIEN=TMGSTARTIEN
        . FOR  SET TMGIEN=$ORDER(@TMGREF@("B",TMGFROM,TMGIEN),TMGDIR) QUIT:(+TMGIEN'>0)!(TMGI'<TMGMAXCT)  DO
        . . SET TMGI=TMGI+1
        . . ;"SET TMGOUT(TMGI)=TMGIEN_"^"_TMGFROM_"^"_$GET(@TMGREF@("B",TMGFROM,TMGIEN))
        . . SET TMGOUT(TMGI)=TMGIEN_"^"_TMGFROM
        ;
GAFS0   SET TMGOUT(0)="1^Success"
GAFSDN  MERGE ^TMG("TMP","RPC","GETAFSUB^TMGSRCH1")=TMGOUT
        QUIT
        ;
SETUPLS(POUT,FILENUM,CT) ;
        ;"Purpose: to return a list of pointers in to file
        ;"Input: POUT -- PASS BY NAME, An OUT PARAMETER
        ;"       FILE -- The file for which pointers IN should be added.
        ;"       CT -- This is used when passing self reiteratively. Leave blank first time.
        ;"NOTE: Any files that don't have data are excluded.
        SET CT=$GET(CT,1)
        NEW NAME
        SET NAME=$PIECE($GET(^DIC(FILENUM,0)),"^",1)
        IF NAME'="",$DATA(@POUT@("B",NAME,FILENUM))=0 DO
        . SET @POUT@("B",NAME,FILENUM)=""
        NEW AFILE SET AFILE=0
        FOR  SET AFILE=$ORDER(^DD(FILENUM,0,"PT",AFILE)) QUIT:(+AFILE'>0)  DO
        . SET NAME=$PIECE($GET(^DIC(AFILE,0)),"^",1) QUIT:NAME=""
        . SET GL=$GET(^DIC(AFILE,0,"GL")) QUIT:(GL="")
        . SET GL=GL_"0)" NEW INFO SET INFO=$GET(@GL)
        . NEW NUMRECS SET NUMRECS=+$PIECE(INFO,"^",4) QUIT:NUMRECS'>0
        . SET @POUT@("B",NAME,AFILE)=""
        . IF CT<3 DO SETUPLS(POUT,AFILE,CT+1)
        QUIT
        ;
GETFLDSB(TMGOUT,TMGPARAMS) ;
        ;"Purpose: Get FIELD list subset, for file
        ;"Input: TMGPARAMS -- FileNum^ListStartValue^direction^MaxCount(optional, def=44)^Simple
        ;"              FileNum -- this is the file to get fields in
        ;"              ListStartValue -- OPTIONAL -- text to $ORDER() from
        ;"              Direction -- $ORDER(xx,Direction) direction (should be 1 or -1) -- OPTIONAL
        ;"              MaxCount -- OPTIONAL.  Default is 44 values returned.
        ;"              Simple -- OPTIONAL  Default is 0 (false).  If 1, then
        ;"                      a very limited list of files returned, with
        ;"                      more user-friendly pseudo names
        ;"Output: TMGRESULTS is filled as follows.
        ;"            TMGRESULT(0)="1^Success" or "-1^Message"
        ;"            TMGRESULT(1)=FldNum^Name^InfoNodes2-4
        ;"            TMGRESULT(2)=FldNum^Name^InfoNodes2-4
        ;"NOTE: Any files that don't have data are excluded.  Subfiles also excluded
        NEW TMGFILE SET TMGFILE=+$PIECE(TMGPARAMS,"^",1)
        IF TMGFILE'>0 DO  GOTO GFSBDN
        . SET TMGOUT(0)="-1^No file number supplied"
        NEW TMGFROM SET TMGFROM=$PIECE(TMGPARAMS,"^",2)
        NEW TMGDIR SET TMGDIR=$PIECE(TMGPARAMS,"^",3)
        IF TMGDIR'=-1 SET TMGDIR=1
        NEW TMGMAXCT SET TMGMAXCT=+$PIECE(TMGPARAMS,"^",4)
        IF TMGMAXCT=0 SET TMGMAXCT=44
        NEW TMGSIMPLE SET TMGSIMPLE=+$PIECE(TMGPARAMS,"^",5)
        ;
        NEW TMGI SET TMGI=0
        NEW HANDLED SET HANDLED=0
        IF TMGSIMPLE DO
        . IF TMGFILE=2 DO           ;"2^PATIENT INFO"
        . . SET TMGI=TMGI+1 SET TMGOUT(TMGI)=".01^NAME"
        . . SET TMGI=TMGI+1 SET TMGOUT(TMGI)=".02^SEX"
        . . SET TMGI=TMGI+1 SET TMGOUT(TMGI)=".03^DATE OF BIRTH"
        . . SET TMGI=TMGI+1 SET TMGOUT(TMGI)=".033^AGE"
        . . SET TMGI=TMGI+1 SET TMGOUT(TMGI)=".05^MARITAL STATUS"
        . . SET TMGI=TMGI+1 SET TMGOUT(TMGI)=".07^OCCUPATION"
        . . SET TMGI=TMGI+1 SET TMGOUT(TMGI)=".09^SOCIAL SECURITY NUMBER"
        . . SET TMGI=TMGI+1 SET TMGOUT(TMGI)=".114^CITY"
        . . SET TMGI=TMGI+1 SET TMGOUT(TMGI)=".115^STATE"
        . . SET TMGI=TMGI+1 SET TMGOUT(TMGI)=".116^ZIP CODE"
        . . SET HANDLED=1
        . IF TMGFILE=8925 DO        ;"8925^NOTES"
        . . SET TMGI=TMGI+1 SET TMGOUT(TMGI)=".01^TYPE OF NOTE"
        . . SET TMGI=TMGI+1 SET TMGOUT(TMGI)=".05^STATUS"
        . . SET TMGI=TMGI+1 SET TMGOUT(TMGI)=".07^BEGINNING DATE"
        . . SET TMGI=TMGI+1 SET TMGOUT(TMGI)=".08^ENDING DATE"
        . . SET TMGI=TMGI+1 SET TMGOUT(TMGI)="2^NOTE TEXT"
        . . SET TMGI=TMGI+1 SET TMGOUT(TMGI)="1201^CREATION DATE"
        . . SET TMGI=TMGI+1 SET TMGOUT(TMGI)="1202^AUTHOR/DICTATOR"
        . . SET TMGI=TMGI+1 SET TMGOUT(TMGI)="1204^EXPECTED SIGNER"
        . . SET TMGI=TMGI+1 SET TMGOUT(TMGI)="1211^VISIT LOCATION"
        . . SET TMGI=TMGI+1 SET TMGOUT(TMGI)="1502^SIGNED BY"
        . . SET HANDLED=1
        . IF TMGFILE=120.5 DO       ;"120.5^VITALS"
        . . SET TMGI=TMGI+1 SET TMGOUT(TMGI)=".01^DATE/TIME TAKEN"
        . . SET TMGI=TMGI+1 SET TMGOUT(TMGI)=".03^VITAL TYPE"
        . . SET TMGI=TMGI+1 SET TMGOUT(TMGI)=".05^LOCATION"
        . . SET TMGI=TMGI+1 SET TMGOUT(TMGI)="1.2^VALUE"
        . . SET HANDLED=1
        . IF TMGFILE=9000010 DO     ;"9000010^VISIT"
        . . SET TMGI=TMGI+1 SET TMGOUT(TMGI)=".01^DATE/TIME"
        . . SET TMGI=TMGI+1 SET TMGOUT(TMGI)=".03^TYPE"
        . . SET TMGI=TMGI+1 SET TMGOUT(TMGI)=".22^LOCATION"
        . . SET HANDLED=1
        . IF TMGFILE=9000010.18 DO  ;"9000010.18^LINKED CPT CODE"
        . . SET TMGI=TMGI+1 SET TMGOUT(TMGI)=".01^CPT NAME"
        . . SET TMGI=TMGI+1 SET TMGOUT(TMGI)=".04^PROVIDER NARRATIVE"
        . . SET TMGI=TMGI+1 SET TMGOUT(TMGI)=".05^DIAGNOSIS"
        . . SET TMGI=TMGI+1 SET TMGOUT(TMGI)=".07^PRINCIPLE PROCEDURE"
        . . SET TMGI=TMGI+1 SET TMGOUT(TMGI)="1204^ENCOUNTER PROVIDER"
        . . SET TMGI=TMGI+1 SET TMGOUT(TMGI)="80201^CATEGORY"
        . . SET HANDLED=1
        IF HANDLED DO ADDINFO(TMGFILE,.TMGOUT) GOTO GFSB0
        ;
        NEW TMGREF SET TMGREF=$NAME(^DD(TMGFILE))
        FOR  SET TMGFROM=$ORDER(@TMGREF@("B",TMGFROM),TMGDIR) QUIT:(TMGFROM="")!(TMGI'<TMGMAXCT)  DO
        . NEW TMGFLD SET TMGFLD=""
        . FOR  SET TMGFLD=$ORDER(@TMGREF@("B",TMGFROM,TMGFLD),TMGDIR) QUIT:(+TMGFLD'>0)!(TMGI'<TMGMAXCT)  DO
        . . NEW INFO SET INFO=$PIECE($GET(^DD(TMGFILE,TMGFLD,0)),"^",2,4)
        . . IF +INFO>0,($$ISWPFLD^TMGDBAPI(TMGFILE,TMGFLD)=0) QUIT  ;"Don't return subfile fields (for now)
        . . SET TMGI=TMGI+1
        . . SET TMGOUT(TMGI)=TMGFLD_"^"_TMGFROM_"^"_INFO
        ;
GFSB0   SET TMGOUT(0)="1^Success"
GFSBDN  QUIT
        ;
ADDINFO(TMGFILE,TMGOUT) ;
        ;"Purpose: To add INFO to field entries, as created in GETFLDSB
        NEW I SET I=0
        FOR  SET I=$ORDER(TMGOUT(I)) QUIT:(+I'>0)  DO
        . NEW ENTRY SET ENTRY=$GET(TMGOUT(I)) QUIT:(ENTRY="")
        . NEW TMGFLD SET TMGFLD=+ENTRY
        . NEW INFO SET INFO=$PIECE($GET(^DD(TMGFILE,TMGFLD,0)),"^",2,4)
        . SET TMGOUT(I)=ENTRY_"^"_INFO
        QUIT
