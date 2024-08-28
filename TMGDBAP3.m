TMGDBAP3 ;TMG/kst/SACC-compliant DB APT utilities ;9/23/17, 7/7/22
         ;;1.0;TMG-LIB;**1,17**;07/12/05
  ;  
  ;"TMG DB API UTILITIES
  ;"SACC compliant versions of TMGDEBUG
  ;
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
  ;"
  ;"This file is part of the TMG LIBRARY, and may only be used in accordence
  ;" to license terms outlined in separate file TMGLICNS.m, which should 
  ;" always be distributed with this file.
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;
  ;"NOTE: This will contain SACC-compliant versions of code from TMGDBAPI
  ;"      If routine is not found here, the please migrate and update the
  ;"      code to be compliant.
  ;"    -- My need division into smaller files to pass ^XINDEX
  ;"=======================================================================
  ;" API -- Public Functions.
  ;"=======================================================================
  ;"ASKFIENS(FILE,IENS) --Ask user to pick a file number, then pick a record
  ;"ASKIENS(FILENUM,IENS) -- ask user to select a record in File indicated by FILENUM.
  ;"$$GETFNUM(FILENAME) -- Convert a file name into a file number
  ;"$$GTNUMFLD(FILENUMBER,FIELDNAME) -- Given file and the name of a field, this will return the field NUMBER
  ;"$$SETFFNUM(FILE,FIELD,FILENUMBER,FIELDNUMBER) -- ensure that FILE and FIELD numbers are in place
  ;"READWP(FILE,IENS,FIELD,ARRAY) --a wrapper for reading a WP with error trap, error reporting
  ;"$$GFLDINFO(FILENUMBER,FIELD,POUT,INFOS) -- GET FIELD INFO
  ;"$$ISSUBFIL(FILE) -- Returns if file is a subfile
  ;"$$HASREC(FILE,IENS) --Return if file (or subfile) has any records
  ;"$$REC1(BFILE,IENS) -- Return 1st IEN of file or subfile, or 0 if none
  ;"$$PARENTFILE(SUBFILE) -- return parent file, given subfile number  
  ;"$$SFUPFLD(SUBFILE) --'SUBFILE UP FIELD'.  Return field in parent that holds given subfile
  ;"SFUPARR(OUT,SUBFILE) --Return an array of subfile info up to root.
  ;
  ;"=======================================================================
  ;"Private API functions
  ;"=======================================================================
  ;"ASKSCRN -- Input transform for ASKFIENS
  ;"GETREFAR(FILENUM,ARRAY) -- Get ARRAY to use with ^DIC
  ;
  ;"=======================================================================
  ;"DEPENDENCIES: TMGDEBU2
  ;"=======================================================================
  ;
ASKFIENS(FILE,IENS)  ;
  ;"Purpose: Ask user to pick a file number, then pick a record
  ;"         from that file.  This supports selection of subfiles.
  ;"Input: FILE -- OPTIONAL. If specified, then taken as user's input
  ;"                        to query for file or subfile
  ;"       IENS: OPTIONAL.  Allows for supplying a partial IENS supplying a
  ;"                      partial path.  E.g. IF a full IENS to FILENUM
  ;"                    would be '2,3,4455,' and IF the IENS supplied is
  ;"                  '3,4455,' then only the missing IEN (in this case 2)
  ;"                 would be asked.
  ;"Results: format-- FILE^IENS, or ^ if abort
  NEW RESULT SET RESULT="^"
  NEW DIR,X,Y SET Y=0
  SET FILE=$GET(FILE) 
  IF FILE'="" DO
  . SET X=FILE 
  . DO ASKSCRN  ;"X is IN and OUT parameter
  . SET Y=X
  IF Y'>0 DO
  . SET DIR(0)="F"
  . SET DIR("A")="Select FILE (or SUBFILE)"
  . SET DIR("?")="Answer with FILE NUMBER or NAME, or SUBFILE NUMBER"
  . SET DIR("PRE")="D ASKSCRN^TMGDBAP3"
  . DO ^DIR
AFIL1 ;
  SET Y=+Y
  IF Y>0 SET RESULT=Y_"^"_$$ASKIENS(Y,.IENS)
  ;
  QUIT RESULT
  ;
ASKIENS(FILENUM,IENS) ;
  ;"Purpose: To ask user to select a record in File indicated by FILENUM.
  ;"         If FILENUM is a subfile number, then the user will be asked
  ;"         for records to drill down to desired record, and return values
  ;"         as an IENS.
  ;"Input: FILENUM: A file number or subfile number
  ;"       IENS: OPTIONAL.  Allows for supplying a partial IENS supplying a
  ;"                      partial path.  E.g. IF a full IENS to FILENUM
  ;"                    would be '2,3,4455,' and IF the IENS supplied is
  ;"                  '3,4455,' then only the missing IEN (in this case 2)
  ;"                 would be asked.
  ;"Results: Returns IENS.  format: IEN in file,IEN in parentfile,IEN in grandparentfile, ... ,
  ;"            Note: IENS will contain '?' IF there is a problem,
  ;"                  or "" IF FILENUM is invalid
  NEW ARRAY
  DO GETREFAR(FILENUM,.ARRAY)
  NEW RESULTIENS SET RESULTIENS=""
  SET IENS=$GET(IENS)
  IF (IENS'=""),(IENS'[",") SET IENS=IENS_","  ;"Convert plain IEN into IENS
  ;
  NEW DANUM SET DANUM=1
  NEW TMGDA,NUMIENS
  SET NUMIENS=$LENGTH(IENS,",")
  NEW IDX,ABORT SET IDX="",ABORT=0
  FOR  SET IDX=$ORDER(ARRAY(IDX),-1) QUIT:(IDX="")!ABORT  DO
  . NEW DIC,X,Y,DA
  . NEW TEMPIEN SET TEMPIEN=+$PIECE(IENS,",",NUMIENS-DANUM)
  . IF TEMPIEN'>0 DO
  . . SET DIC=$GET(ARRAY(IDX,"GL")),DIC(0)="AEQM"
  . . IF DIC'="" WRITE !,"Select entry in file# ",ARRAY(IDX,"FILE NUM")
  . . DO ^DIC WRITE !
  . ELSE  SET Y=TEMPIEN
  . IF +Y'>0 SET RESULTIENS="?,"_RESULTIENS,ABORT=1 QUIT
  . SET TMGDA(DANUM)=+Y,DANUM=DANUM+1
  . SET RESULTIENS=+Y_","_RESULTIENS
  ;
  WRITE "#: ",RESULTIENS,!
  QUIT RESULTIENS
  ;
ASKSCRN  ;
  ;"Purpose: an Input transform for ASKFIENS
  ;"Input: (global) X -- the user's response in ^DIR
  ;"       (global) DTOUT -- this will be defined IF the read timed out.
  ;"Output: If X is changed, it will be as if user entered in new X
  ;"        If X is killed, it will be as if user entered an illegal value.
  ;"Result: none
  IF $DATA(DTOUT) QUIT
  IF +X=X DO
  . IF $DATA(^DD(X,0))=0  KILL X QUIT
  . IF $DATA(^DIC(X,0)) WRITE " ",$PIECE(^DIC(X,0),"^",1)," " QUIT
  . ;"Here we deal with subfiles
  . NEW TEMP,IDX,FILENUM
  . SET FILENUM=X
  . SET X=""
  . FOR IDX=100:-1:0 DO  QUIT:(FILENUM=0)
  . . SET TEMP(IDX)=FILENUM
  . . SET X=X_FILENUM_","
  . . SET FILENUM=+$GET(^DD(FILENUM,0,"UP"))
  . NEW INDENT SET INDENT=5
  . NEW INDENTS SET $PIECE(INDENTS," ",75)=" "
  . WRITE !
  . SET IDX=""
  . FOR  SET IDX=$ORDER(TEMP(IDX)) QUIT:(IDX="")  DO
  . . SET FILENUM=+$GET(TEMP(IDX)) QUIT:(FILENUM=0)
  . . WRITE $EXTRACT(INDENTS,1,INDENT)
  . . IF $DATA(^DIC(FILENUM,0)) DO
  . . . WRITE $PIECE(^DIC(FILENUM,0),"^",1)," (FILE #",FILENUM,")",!
  . . ELSE  WRITE "+--SUBFILE# ",FILENUM,!
  . . SET INDENT=INDENT+3
  ELSE  DO  ;"check validity of FILE NAME
  . IF X="" QUIT
  . NEW FILENUM SET FILENUM=$ORDER(^DIC("B",X,""))
  . IF +FILENUM>0 SET X=+FILENUM_"," QUIT
  . SET FILENUM=$$GETFNUM(X)
  . IF +FILENUM>0 SET X=+FILENUM_"," QUIT
  . NEW DIC,Y
  . SET DIC=1 SET DIC(0)="EQM"
  . DO ^DIC w !
  . IF +Y>0 SET X=+Y QUIT
  . SET X=0
  ;
  IF $GET(X)="" SET X=0
  QUIT
  ;
GETFNUM(FILENAME) ;   ;"updated code avail in TMGXMLT2
  ;"Purpose: Convert a file name into a file number
  ;"Input: The name of a file
  ;"Result: The filenumber, or 0 IF not found.
  NEW RESULT SET RESULT=0
  IF $GET(FILENAME)="" GOTO GFNUMDN
  IF FILENAME=" " DO  GOTO GFNUMDN
  . DO SHOWERR^TMGDEBU2(,"No file specifier (either name or number) given!")
  . SET RESULT=0
  SET DIC=1  ;"File 1=Global file reference (the file listing info for all files)
  SET DIC(0)="M"
  SET X=FILENAME   ;"i.e. "AGENCY"
  DO ^DIC  ;"lookup filename  Result comes back in Y ... i.e. "4.11^AGENCY"
  SET RESULT=$PIECE(Y,"^",1)
  IF RESULT=-1 SET RESULT=0
GFNUMDN ;
  QUIT RESULT
  ;
GTNUMFLD(FILENUMBER,FIELDNAME) ;
  ;"PUBLIC FUNCTION
  ;"Purpose: Given file and the name of a field, this will return the field NUMBER
  ;"Input: FILENUMBER.  Number of file, i.e. "4.11"
  ;"       FIELDNAME: the name of a field, i.e. "NAME"  spelling must exactly match
  ;"Output: Returns field number, i.e. ".01" or 0 IF not found
  QUIT $$FLDNUM^DILFD(FILENUMBER,FIELDNAME)
  ;
SETFFNUM(FILE,FIELD,FILENUMBER,FIELDNUMBER) ;
  ;"Purpose: To provide a generic shell to ensure that FILE and FIELD numbers are in place
  ;"Input:     FILE -- FILE number or name
  ;"           FIELD -- field number or name
  ;"           FILENUMBER -- PASS BY REFERENCE -- an out parameter
  ;"           FIELDNUMBER -- PASS BY REFERENCE -- an out parameter
  ;"Results: 1 IF ok, otherwise 0 IF error
  ;"Output -- FILENUMBER and FIELDNUMBER are filled in.
  NEW RESULT SET RESULT=1
  SET FILENUMBER=+$GET(FILE)
  IF FILENUMBER=0 SET FILENUMBER=$$GETFNUM^TMGDBAP3(.FILE)
  IF FILENUMBER=0 DO  GOTO SFFNDN
  . SET RESULT=0
  . DO SHOWERR^TMGDEBU2(,"Can't convert file '"_$GET(FILE)_", to a number.")
  SET FIELDNUMBER=$GET(FIELD)
  IF +FIELDNUMBER=0 SET FIELDNUMBER=$$GTNUMFLD(FILENUMBER,.FIELD)
  IF FIELDNUMBER=0 DO  GOTO SFFNDN
  . SET RESULT=0
  . DO SHOWERR^TMGDEBU2(,"Can't convert field '"_$GET(FIELD)_", to a number.")
SFFNDN  QUIT RESULT
  ;
GETREFAR(FILENUM,ARRAY) ;
  ;"Purpose: To return an ARRAY containing global references that can
  ;"         be passed to ^DIC, for given file or subfile number
  ;"Input: FILENUM: A file number or subfile number
  ;"       ARRAY: PASS BY REFERENCE.  See format below
  ;"Results: none, but ARRAY is filled with result.  Format (example):
  ;"      ARRAY(1,"FILE NUM")=2.011  <--- sub sub file
  ;"      ARRAY(1,"GL")="^DPT(TMGDA(1),""DE"",TMGDA(2),""1"","
  ;"      ARRAY(2,"FILE NUM")=2.001  <---- sub file
  ;"      ARRAY(2,"GL")="^DPT(TMGDA(1),""DE"","
  ;"      ARRAY(3,"FILE NUM")=2  <---- parent file
  ;"      ARRAY(3,"GL")="^DPT("
  ;"Note: To use the references stored in "GL", then the IEN for
  ;"      each step should be stored in TMGDA(x)
  NEW IDX
  FOR IDX=1:1 QUIT:(+$GET(FILENUM)=0)  DO
  . SET ARRAY(IDX,"FILE NUM")=FILENUM
  . IF $DATA(^DD(FILENUM,0,"UP")) DO
  . . NEW PARENTFLNUM,FIELD
  . . SET PARENTFLNUM=+$GET(^DD(FILENUM,0,"UP"))
  . . IF PARENTFLNUM=0 QUIT  ;"really should be an abort
  . . SET FIELD=$ORDER(^DD(PARENTFLNUM,"SB",FILENUM,""))
  . . IF FIELD="" QUIT  ;"really should be an abort
  . . NEW NODE SET NODE=$PIECE($PIECE($GET(^DD(PARENTFLNUM,FIELD,0)),"^",4),";",1)
  . . SET ARRAY(IDX,"NODE IN PARENT")=NODE
  . ELSE  DO
  . . SET ARRAY(IDX,"GL")=$GET(^DIC(FILENUM,0,"GL"))
  . SET FILENUM=+$GET(^DD(FILENUM,0,"UP"))
  ;
  SET IDX="" SET IDX=$ORDER(ARRAY(IDX),-1)
  SET ARRAY(IDX,"REF")=$GET(ARRAY(IDX,"GL"))_"TMGDA(1),"
  NEW DANUM SET DANUM=2
  FOR  SET IDX=$ORDER(ARRAY(IDX),-1) QUIT:(IDX="")  DO
  . NEW REF
  . SET REF=$GET(ARRAY(IDX+1,"REF"))_""""_$GET(ARRAY(IDX,"NODE IN PARENT"))_""","
  . KILL ARRAY(IDX+1,"REF"),ARRAY(IDX,"NODE IN PARENT")
  . SET ARRAY(IDX,"GL")=REF
  . SET ARRAY(IDX,"REF")=REF_"TMGDA("_DANUM_"),"
  . SET DANUM=DANUM+1
  KILL ARRAY(1,"REF")
  QUIT
  ;
READWP(FILE,IENS,FIELD,ARRAY) ;
  ;"Purpose: To provide a shell for reading a WP with error trap, error reporting
  ;"Input: FILE: a number or name
  ;"         IENS: a standard IENS (i.e.  "IEN,parent-IEN,grandparent-IEN,ggparent-IEN," etc.
  ;"              Note: can just pass a single IEN (without a terminal ",")
  ;"         FIELD: a name or number
  ;"         ARRAY: The array to receive WP data.  PASS BY REFERENCE
  ;"                      returned In Fileman acceptible format.
  ;"                      ARRAY will be deleted before refilling
  ;"Results -- IF error occured
  ;"        1  IF no error
  ;"        0  IF error
  NEW FILENUM,FLDNUM
  NEW TMGWP,TEMP,TMGMSG
  NEW RESULT SET RESULT=1
  IF $GET(IENS)="" DO  GOTO RWPDN
  . DO SHOWERR^TMGDEBU2(,"Valid IENS not supplied.")
  IF $EXTRACT(IENS,$LENGTH(IENS))'="," SET IENS=IENS_","
  IF $$SETFFNUM^TMGDBAP3(.FILE,.FIELD,.FILENUM,.FLDNUM)=0 GOTO RWPDN
  SET TEMP=$$GET1^DIQ(FILENUM,IENS,FLDNUM,"","TMGWP","TMGMSG")
  IF $DATA(TMSETGMSG),$DATA(TMGMSG("DIERR"))'=0 DO 
  . DO SHOWDIER^TMGDEBU2(.TMGMSG)
  . SET RESULT=0
  IF RESULT=0 GOTO RWPDN
  KILL ARRAY MERGE ARRAY=TMGWP
RWPDN  ;
  QUIT RESULT
  ;
GFLDINFO(FILENUMBER,FIELD,POUT,INFOS) ;"GET FIELD INFO     ;"copy at GFLDINFO^TMGXMLT2
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
  IF +FIELD=0 SET FIELD=$$GTNUMFLD(FILENUMBER,FIELD)
  SET @POUT@("STORELOC")=$PIECE($GET(^DD(FILENUMBER,FIELD,0)),"^",4)
  NEW TMGMSG,ATTRIBS SET ATTRIBS="MULTIPLE-VALUED;SPECIFIER;TYPE"
  IF $DATA(INFOS) SET ATTRIBS=ATTRIBS_";"_INFOS
  ;"Next, check IF  field is a multiple and get field info.
  DO FIELD^DID(FILENUMBER,FIELD,,ATTRIBS,POUT,"TMGMSG")
  IF $DATA(TMGMSG),$DATA(TMGMSG("DIERR"))'=0 DO
  . DO SHOWDIER^TMGDEBU2(.TMGMSG)
  QUIT
  ;
ISSUBFIL(FILE) ;"IS SUBFILE?
  ;"Purpose: to return if file is actually a subfile
  ;"Input: FILE -- File name or number
  ;"Results: Parent file number^FIELD in Parent File, or 0 if not a subfile.
  IF +FILE'=FILE SET FILE=$$GETFNUM^TMGDBAP3(FILE)
  NEW RESULT SET RESULT=+$GET(^DD(FILE,0,"UP"))
  IF RESULT'>0 GOTO ISFDN
  ;"Now find which field this sub file is in its parent
  NEW FLDINPARENT SET FLDINPARENT=0
  NEW FIELD SET FIELD=0
  NEW DONE SET DONE=0
  ;"NOTE: I think the following could be rewritten to use the "SB" index instead of blind search
  FOR  SET FIELD=$ORDER(^DD(RESULT,FIELD)) QUIT:(+FIELD'>0)!(DONE=1)  DO
  . NEW FLDINFO SET FLDINFO=$PIECE($GET(^DD(RESULT,FIELD,0)),"^",2)
  . IF +FLDINFO=FILE SET FLDINPARENT=FIELD SET DONE=1
  IF FLDINPARENT>0 SET RESULT=RESULT_"^"_FLDINPARENT
ISFDN   QUIT RESULT
  ; 
HASREC(FILE,IENS) ;"FILE (or SUBFILE) HAS A RECS?
  ;"Return boolean result of whether subfile actually has any records
  QUIT ($$REC1(FILE,IENS)>0)
  ;
REC1(FILE,IENS) ;"RETURN 1ST IEN IN FILE OR SUBFILE, OR 0 IF NONE.
  NEW OROOT SET OROOT=$$GETGREF^TMGFMUT2(FILE,IENS) 
  IF OROOT="" QUIT 0
  NEW CROOT SET CROOT=$$CREF^DILF(OROOT)
  QUIT $ORDER(@CROOT@(0))
  ;
PARENTFILE(SUBFILE) ;"return parent file, given subfile number
  NEW PARENTFILE SET PARENTFILE=+$GET(^DD($GET(SUBFILE),0,"UP"))
  QUIT PARENTFILE
  ;
SFUPFLD(SUBFILE)  ;"'SUBFILE UP FIELD':  RETURN FIELD IN PARENT THAT HOLDS GIVEN SUBFILE.
  ;"Results: 0^0^0 if not found, 
  ;"         or <FIELD IN PARENT FILE>^<PARENT FILE>^<Storage subscript node in parent>
  ;"         or 0^0^.^<Storage of top level file>
  SET SUBFILE=+$GET(SUBFILE)
  NEW RESULT,PARENTFILE SET PARENTFILE=+$GET(^DD(SUBFILE,0,"UP"))
  IF PARENTFILE>0 DO
  . NEW FLD SET FLD=+$ORDER(^DD(PARENTFILE,"SB",SUBFILE,0))
  . NEW NODE SET NODE=$PIECE($PIECE($GET(^DD(PARENTFILE,FLD,0)),"^",4),";",1)
  . SET RESULT=FLD_"^"_PARENTFILE_"^"_NODE
  ELSE  DO  ;"Handle if 'SUBFILE' is actually a top level file.  
  . SET GL=$GET(^DIC(SUBFILE,0,"GL")) 
  . IF GL="" SET GL=0
  . SET RESULT="0^0^."_GL
  QUIT RESULT
  ;
SFUPARR(OUT,SUBFILE) ;"Return an array of subfile info up to root.
  ;"Input: SUBFILE -- file number of a subfile
  ;"       OUT -- pass by reference.  An OUT parameter.  Format:
  ;"          OUT=<passed in SUBFILE>
  ;"          OUT(1)=<FIELD IN PARENT FILE>^<PARENT FILE>^<Storage subscript node in parent>
  ;"          OUT(2)=<FIELD IN GrandPARENT FILE>^<GrandPARENT FILE>^<Storage subscript node in Grand-parent>
  ;"          OUT(3)=<FIELD IN GreatGrandPARENT FILE>^<GreatGrandPARENT FILE>^<Storage subscript node in GreatGrand-parent>
  ;"          OUT(4)= etc.
  ;"          E.G., if SUBFILE=22733.32
  ;"            OUT=22733.32
  ;"            OUT(1)="2^22733.03^2"
  ;"            OUT(2)="1^22733.02^1"
  ;"            OUT(3)="2^22733^2"
  ;"            OUT(4)="0^0^.^TMG(22733,"
  ;"Result: NONE
  NEW IDX SET IDX=0
  NEW AFILE SET AFILE=+$GET(SUBFILE),OUT=SUBFILE
  FOR  QUIT:AFILE'>0  SET IDX=IDX+1,OUT(IDX)=$$SFUPFLD(AFILE),AFILE=+$PIECE(OUT(IDX),"^",2)
  QUIT
  ;
FILE2REF(FILE,IENS)  ;"CONVERT A FILE/SUBFILE NUMBER AND IENS INTO A OPEN REF
  ;"INPUT: FILE -- the file or subfile number
  ;"       IENS -- the standard IENS of subfile.  E.g. '4,3,2,1'
  ;"                NOTE: '"B",3,2,1' is also allowed
  ;"EXAMPLE:     
  ;" FILE=22733.32, IENS='4,3,2,1' 
  ;" 22733.32 (STRENGTH ALIAS) parent is 22733.03 (STRENGTH), and it is field 1, storage node 2
  ;" 22733.03 (STRENGTH) parent is 22733.02 (DOSAGE FORM), and it is field 1, storage node 1
  ;" 22733.02 (DOSAGE FORM) parent is the IEN in the root of the file.  It is field 2, storage node 2
  ;" so
  ;"   IEN IN 22733.32=4  STORAGE NODE=2
  ;"   IEN IN 22733.03=3  STORAGE NODE=1
  ;"   IEN IN 22733.02=2  STORAGE NODE=2
  ;"   IEN IN 22733   =1  STORAGE NODE=_
  ;" 
  ;"  (below I am artificially putting the IEN's in brackets, e.g. [4], to show difference from storage nodes 
  ;"   --> ^TMG(22733,[1],2,[2],1,[3],2,[4],
  ;"RESULT: ^TMG(22733,1,2,2,1,3,2,4,
  NEW RESULT SET RESULT="",IENS=$GET(IENS),FILE=+$GET(FILE)
  FOR  QUIT:$EXTRACT(IENS,$LENGTH(IENS))'=","  SET IENS=$EXTRACT(IENS,1,$LENGTH(IENS)-1)
  NEW IDX FOR IDX=1:1:$LENGTH(IENS,",") SET IENS(IDX)=$PIECE(IENS,",",IDX)
  NEW STORE DO SFUPARR(.STORE,FILE)
  SET IDX=+$ORDER(IENS(""),-1)
  FOR  QUIT:IDX'>0  DO
  . NEW TEMP SET TEMP=$GET(STORE(IDX))
  . NEW LOC SET LOC=$PIECE(TEMP,"^",3) 
  . IF LOC="." SET LOC="^"_$PIECE(TEMP,"^",4)
  . IF $EXTRACT(LOC,$LENGTH(LOC))=","  SET LOC=$EXTRACT(LOC,1,$LENGTH(LOC)-1)
  . NEW IEN SET IEN=$GET(IENS(IDX))
  . SET RESULT=RESULT_LOC_","_IEN_","
  . SET IDX=IDX-1
  QUIT RESULT  
  ;
LISTFILES(OUT,SN,EN,OPTION)  ;"List Fileman files for number range.  
  ;"Input: OUT -- PASS BY REFERENCE.  FORMAT:
  ;"         OUT("A",NUMBER)=NAME^<PARENT FILE NUMBER IF ANY>
  ;"         OUT("B",NAME,NUMBER)=""
  ;"       SN -- option, default is 0.  Start of number range to return  (number is inclusive)
  ;"       EN -- option, default is last possible.  End of number range to return  (number is inclusive)
  ;"       OPTION -- optional.  PASS BY REFERENCE.  Format:
  ;"          OPTION("NO SUBFILES")=0 OR 1.  Default is P.  If 1, then subfiles not shown.  
  SET SN=+$GET(SN)
  SET EN=$GET(EN) IF EN'>0 SET EN=$ORDER(^DIC("@"),-1)
  NEW NOSUBFILE SET NOSUBFILE=+$GET(OPTION("NO SUBFILES"))
  NEW FNUM SET FNUM=SN-0.00000001
  FOR  SET FNUM=$ORDER(^DD(FNUM)) QUIT:(FNUM'>0)!(FNUM>EN)  DO
  . NEW NAME SET NAME=$ORDER(^DD(FNUM,0,"NM","")) IF NAME="" SET NAME="???"
  . NEW UP SET UP=+$GET(^DD(FNUM,0,"UP"))
  . IF UP>0,NOSUBFILE QUIT
  . SET OUT("A",FNUM)=NAME_$SELECT(UP>0:"^"_UP,1:"")
  . SET OUT("B",NAME,FNUM)=""
  . IF UP>0 SET OUT("SUBFILES",FNUM,NAME)="SUBFILE^"_UP
  QUIT
  ;
ASKLSTFL(SN,EN,OPTION) ;"Ask user for start and end of range, and display all files in range
  ;"INPUT: SN -- option, default is 0.  Start of number range to return  (number is inclusive)
  ;"       EN -- option, default is last possible.  End of number range to return  (number is inclusive)
  ;"       OPTION -- optional.  PASS BY REFERENCE.  Format:
  ;"          OPTION("NO SUBFILES")=0 OR 1.  Default is P.  If 1, then subfiles not shown.  
  WRITE !,"Display all Fileman files (including subfiles), by number, in range.",!
  SET SN=+$GET(SN),EN=+$GET(EN)
  IF SN'>0 WRITE "START NUMBER FOR RANGE (INCLUSIVE): " READ SN 
  IF (SN["^")!(+SN'>0) QUIT
  WRITE " --> using start number [",+SN,"]",!
  IF EN'>0 WRITE "END NUMBER FOR RANGE (INCLUSIVE): " READ EN 
  IF (EN["^")!(EN'>0) QUIT
  WRITE " --> using end number [",+EN,"]",!
  NEW LIST
  DO LISTFILES(.LIST,+SN,+EN,.OPTION)
  NEW FNUM SET FNUM=0
  FOR  SET FNUM=$ORDER(LIST("A",FNUM)) QUIT:FNUM'>0  DO
  . NEW VALUE SET VALUE=$GET(LIST("A",FNUM),"???")
  . WRITE FNUM,?15,$PIECE(VALUE,"^",1)
  . NEW PARENT SET PARENT=$PIECE(VALUE,"^",2) IF PARENT'=""  DO
  . . WRITE " --> in ",$$PARENTSTR(.LIST,PARENT)
  . WRITE !
  QUIT
  ;
PARENTSTR(LIST,NUM)  ;"Utility function for ASKLSTFL
  NEW VALUE SET VALUE=$GET(LIST("A",NUM))
  NEW RESULT SET RESULT=$PIECE(VALUE,"^",1)
  NEW UP SET UP=$PIECE(VALUE,"^",2)
  IF UP>0 SET RESULT=RESULT_" --> in "_$$PARENTSTR(.LIST,UP)
  QUIT RESULT
  ;
LSTTMGFILES ;
  WRITE !,!,"-- DISPLAY TMG FILES, BY NUMBER --",!
  SET %=2 WRITE "Include SUBFILES" DO YN^DICN WRITE !
  NEW OPTION SET OPTION("NO SUBFILES")=(%=2)
  
  NEW %ZIS
  SET %ZIS("A")="Enter Output Device: "
  SET %ZIS("B")="HOME"
  DO ^%ZIS  ;"standard device call
  IF POP DO  QUIT
  . DO SHOWERR^TMGDEBU2(,"Error opening output.  Aborting.")
  USE IO
  ;"Do the output
  DO ASKLSTFL(22700,22799.9999999,.OPTION) 
  ;" Close the output device
  DO ^%ZISC
  DO PRESS2GO^TMGUSRI2
  QUIT
  