TMGAI001 ;TMG/kst/Codex Specific functions ;10/11/26
         ;;1.0;TMG-LIB;**1**;4/11/22
 ;
 ;"TMG OPENAI Codex-relatedfunctions. 
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 10/11/26  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;" LIST(TMGDUZ,FILENUM,PART,TMGOUT,MAX) -- Return matching record IENs/names
 ;" DDFLDS(TMGDUZ,FILE) -- Display VPE's formatted data dictionary fields
 ;
 ;"=======================================================================
 ;" Private  Functions.
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;
INIT(TMGDUZ)  ;"Ensure Fileman environment.  
  DO DUZ^XUP(TMGDUZ)
  QUIT
  ;
DUMPREC(TMGDUZ,FILENUM,IENS,SHOWEMPTY)  ;"WRAPPER FOR DUMPCREC^TMGDEBU3
  ;"Purpose: to dump (display) a record, NOT using ^DII (Fileman's Inquire code)
  ;"Input: TMGDUZ -- The IEN of the user to run this function as.
  ;"       FILENUM -- the number of the file to dump from
  ;"       IENS -- the record number to display (or IENS: #,#,#,)
  ;"       SHOWEMPTY -- OPTIONAL;  IF 1 then empty fields will be displayed
  DO INIT(TMGDUZ)
  DO DUMPREC^TMGDEBU3(.FILENUM,.IENS,.SHOWEMPTY)
  QUIT
  ;
LIST(TMGDUZ,FILENUM,PART,TMGOUT,MAX)  ;"WRAPPER FOR LIST^DIC
  ;"Purpose: List records whose B-index value starts with PART.
  ;"Input: TMGDUZ -- The IEN of the user to run this function as.
  ;"       FILENUM -- The number of the file to search.
  ;"       PART -- Optional B-index prefix to match; matching is case-insensitive.
  ;"       TMGOUT -- PASS BY REFERENCE.  Prior data is killed.
  ;"       MAX -- Optional maximum candidates to return.  Default is 20.
  ;"Output: TMGOUT("DILIST",0)=Count^Maximum^MoreResults^Flags
  ;"        TMGOUT("DILIST",Sequence,0)=IEN^Name
  ;"        TMGOUT("DIERR",...)=FileMan error data, if any.
  KILL TMGOUT
  DO INIT(TMGDUZ)
  SET PART=$$UP^XLFSTR($GET(PART))
  SET MAX=+$GET(MAX) IF MAX'>0 SET MAX=20
  NEW TMGMSG
  DO LIST^DIC(FILENUM,"","@;.01","P",MAX,"",PART,"B","","","TMGOUT","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) MERGE TMGOUT("DIERR")=TMGMSG("DIERR")
  QUIT
  ;
DDFLDS(TMGDUZ,FILE)  ;"DISPLAY VPE'S FORMATTED DATA DICTIONARY FIELDS
  ;"Purpose: Display a FileMan file's fields in VPE's field-location layout.
  ;"Input: TMGDUZ -- The IEN of the user to run this function as.
  ;"       FILE -- The number or name of the file whose data dictionary is displayed.
  ;"Output: Writes the VPE field-location display to the current device.
  ;"        Includes descendant fields in multiple subfiles.
  DO INIT(TMGDUZ)
  NEW U SET U="^"
  SET FILE=$GET(FILE)
  IF FILE="" DO  QUIT
  . WRITE !,"No FileMan file name or number was supplied.",!
  NEW FILENUM
  IF +FILE=FILE SET FILENUM=FILE
  ELSE  DO
  . NEW DIC,X,Y
  . SET DIC=1,DIC(0)="M",X=FILE
  . DO ^DIC
  . SET FILENUM=+Y
  IF FILENUM'>0 DO  QUIT
  . WRITE !,"FileMan unique filename '",FILE,"' was not found.",!
  IF '$DATA(^DIC(FILENUM,0)) DO  QUIT
  . WRITE !,"FileMan file #",FILENUM," was not found.",!
  IF '$DATA(^DD(FILENUM,0)) DO  QUIT
  . WRITE !,"FileMan file #",FILENUM," has no data dictionary.",!
  NEW BAR,DASHES,FILE,FLAGP,FLAGE,FLAGQ,FLD,FLDNAM,II,LEV,NP,PIECE,RCNT
  NEW SPACE,SYM,VEESIZE,VEEX,ZDATA,ZGL,ZNAM,ZNUM
  SET ZNUM=FILENUM,ZNAM=$PIECE(^DIC(ZNUM,0),U),ZGL=$GET(^DIC(ZNUM,0,"GL"))
  SET FLAGP=1,FLAGQ=0,FLAGE=0,VEESIZE=999999
  WRITE !!,"File: ",ZNAM," (#",ZNUM,")  Global: ",ZGL,!
  DO HD^%ZVEMDL1
  SET (LEV,RCNT)=1,FILE(LEV)=ZNUM,FLD(LEV)=0
  DO START^%ZVEMDL,LOOP^%ZVEMDL
  QUIT
  ;
ZWR(REF)  ;"Do ZWRITE of ref
  ZWRITE @REF@(*)
  QUIT
  ;
