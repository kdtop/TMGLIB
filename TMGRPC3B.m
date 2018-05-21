TMGRPC3B ;TMG/kst/Support Functions for GUI_Config ;9/28/14
         ;;1.0;TMG-LIB;**1**;08/31/08
 ;
 ;"TMG RPC FUNCTIONS for a GUI config program
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
 ;" <none>
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"GETUSRLT(TMGOUT,TMGPARAMS) -- fill list with users on the system.
 ;"GETRECLT(TMGOUT,TMGPARAMS) -- fill list with records in file on the system
 ;"GET1USER(TMGOUT,TMGIEN) -- Get one user's record
 ;"GET1REC(TMGOUT,TMGPARAMS,FLAGS) -- get one record in file
 ;"GET1FLD((TMGOUT,TMGPARAMS) -- get one field from one record in file
 ;"XTRCTFLD(TMGOUT,TMGARRAY,TMGFLAG) -- convert output from GETS^DIQ into another format
 ;"GFLSUBST(TMGOUT,TMGPARAMS) -- return a subset of entries a file's .01 names
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;"Dependencies:
 ;"  TMGRPC3* only
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;
GETUSRLT(TMGOUT,TMGPARAMS) ;"GET USER LIST
        ;"Purpose: to fill list with users on the system.
        ;"Input: TMGOUT -- an OUT PARAMETER, PASS BY REFERENCE.
        ;"       TMGPARAMS -- either "" or "NODISUSER" IF not to return DISUSER=YES users
        ;"Output: TMGOUT is filled as follows:
        ;"          TMGOUT(0)="1^Success" or "-1^Message"
        ;"          TMGOUT(1)=Name^IEN^200^DISUSER  DISUSER will be 1 for "Y" or 0 for "N"
        ;"          TMGOUT(2)=Name^IEN^200^DISUSER
        ;"Results: none
 ;
        NEW TMGACTIVEONLY SET TMGACTIVEONLY=($GET(TMGPARAMS)="NODISUSER")
        NEW TMGINDEX SET TMGINDEX=1
        NEW TMGNAME SET TMGNAME=""
        FOR  SET TMGNAME=$ORDER(^VA(200,"B",TMGNAME)) QUIT:(TMGNAME="")  DO
        . NEW TMGIEN SET TMGIEN=""
        . FOR  SET TMGIEN=$ORDER(^VA(200,"B",TMGNAME,TMGIEN)) QUIT:(+TMGIEN'>0)  DO
        . . NEW TMGDISUSER SET TMGDISUSER=$PIECE($GET(^VA(200,TMGIEN,0)),"^",7)
        . . IF (TMGACTIVEONLY)&(TMGDISUSER) QUIT
        . . NEW TMGNAME SET TMGNAME=$PIECE($GET(^VA(200,TMGIEN,0)),"^",1)
        . . SET TMGOUT(TMGINDEX)=TMGNAME_"^"_TMGIEN_"^200^"_TMGDISUSER
        . . SET TMGINDEX=TMGINDEX+1
 ;
        SET TMGOUT(0)="1^Success"
 ;
        QUIT
 ;
GETRECLT(TMGOUT,TMGPARAMS) ;"GET RECS LIST
        ;"Purpose: to fill list with records in file on the system.
        ;"Input: TMGOUT -- an OUT PARAMETER, PASS BY REFERENCE.
        ;"       TMGPARAMS -- Filenumber
        ;"Output: TMGOUT is filled as follows:
        ;"          TMGOUT(0)="1^Success" or "-1^Message"
        ;"          TMGOUT(1)=.01Value^IEN^FileNum
        ;"          TMGOUT(2)=.01Value^IEN^FileNum
        ;"Results: none
 ;
        NEW TMGINDEX SET TMGINDEX=1
        NEW TMGNAME SET TMGNAME=""
        NEW TMGFNUM SET TMGFNUM=+$GET(TMGPARAMS)
        IF TMGFNUM'>0 DO  GOTO GRLDONE
        . SET TMGOUT(0)="-1^Valid file number not found"
        NEW TMGREF SET TMGREF=$GET(^DIC(TMGFNUM,0,"GL"))
        SET TMGREF=$$CREF^DILF(TMGREF)
        IF TMGREF="" DO  GOTO GRLDONE
        . SET TMGOUT(0)="-1^Unable to find global reference for file: "_TMGFNUM
        NEW TMGLOC,TMGPIECE
        SET TMGLOC=$PIECE(^DD(TMGFNUM,.01,0),"^",4)
        SET TMGPIECE=$PIECE(TMGLOC,";",2)
        SET TMGLOC=$PIECE(TMGLOC,";",1)
        FOR  SET TMGNAME=$ORDER(@TMGREF@("B",TMGNAME)) QUIT:(TMGNAME="")  DO
        . NEW TMGIEN SET TMGIEN=""
        . FOR  SET TMGIEN=$ORDER(@TMGREF@("B",TMGNAME,TMGIEN)) QUIT:(+TMGIEN'>0)  DO
        . . NEW TMGNAME SET TMGNAME=$PIECE($GET(@TMGREF@(TMGIEN,TMGLOC)),"^",TMGPIECE)
        . . SET TMGOUT(TMGINDEX)=TMGNAME_"^"_TMGIEN_"^"_TMGFNUM
        . . SET TMGINDEX=TMGINDEX+1
 ;
        SET TMGOUT(0)="1^Success"
GRLDONE ;
        QUIT
 ;
GET1USER(TMGOUT,TMGIEN) ;"GET ONE USER
        ;"Purpose: to get record of one user
        ;"Input: TMGOUT -- an OUT PARAMETER, PASS BY REFERENCE.
        ;"       TMGIEN -- the IEN in file 200 to get
        ;"Output: TMGOUT is filled as follows:
        ;"          TMGOUT(0)="1^Success" or "-1^Message"
        ;"          TMGOUT(1)=File^IENS^FieldNum^ExternalValue^DDInfo...
        ;"          TMGOUT(2)=File^IENS^FieldNum^ExternalValue^DDInfo...
        ;"Note: the fields to return are decided HERE
        ;"Results: none
 ;
        NEW TMGIENS SET TMGIENS=+$GET(TMGIEN)_","
        DO GET1REC(.TMGOUT,"200^"_TMGIENS)
        QUIT
 ;
 ;
GET1REC(TMGOUT,TMGPARAMS,FLAGS) ;
        ;"Purpose: to get one record in file
        ;"Input: TMGOUT -- an OUT PARAMETER, PASS BY REFERENCE.
        ;"       TMGPARAMS: File^IENS
        ;"         File -- the file or subfile to retrieve from
        ;"         IENS -- IF File is a subfile, then IENS should be full IENS to get (e.g. '2,103,')
        ;"                 IF File is not a subfile, then IENS can be just IEN or IEN_","
        ;"       FLAGS -- OPTIONAL.  Default = "E" for external value.  Options
        ;"                  "E" for external values returned, "I" for internal values returned
        ;"Output: TMGOUT is filled as follows:
        ;"          TMGOUT(0)="1^Success" or "-1^Message"
        ;"          TMGOUT(1)=File^IENS^FieldNum^ExternalValue^DDInfo...   <-- if FLAGS = "E"
        ;"          TMGOUT(2)=File^IENS^FieldNum^InternalValue^DDInfo...   <-- if FLAGS = "I"
        ;"          TMGOUT(2)=File^IENS^FieldNum^ExternalValue^InternalValue^DDInfo...    <-- if FLAGS = "IE" or "EI"
        ;"NOTE! -- If Flags call for "I" and "E", then the piece position of the DD info is shifted 1 place to right!
        ;"          ...
        ;"          (note: the quote char ' not included.  Shown below to indicate that these ascii chars are sent)
        ;"          (the V nodes are only sent when a field of variable pointer type is sent back.)
        ;"          TMGOUT(10)='INFO'^'DD'^FileNum^FieldNum^'V',1,0)&=&<DD ENTRY>
        ;"          TMGOUT(11)='INFO'^'DD'^FileNum^FieldNum^'V',2,0)&=&<DD ENTRY>
        ;"          TMGOUT(12)='INFO'^'DD'^FileNum^FieldNum^'V',3,0)&=&<DD ENTRY>
        ;"Note: the fields to return are decided HERE
        ;"Results: none
 ;
        SET TMGOUT(0)="1^Success"  ;"default to success
        SET FLAGS=$GET(FLAGS,"E")
        NEW TMGARRAY,TMGMSG
        SET TMGPARAMS=$GET(TMGPARAMS)
        SET ^TMG("TMP","RPC","GET1REC")=TMGPARAMS
        NEW TMGFILE SET TMGFILE=$PIECE(TMGPARAMS,"^",1)
        IF +TMGFILE'>0 DO  GOTO GORDONE
        . SET TMGOUT(0)="-1^No file number supplied"
        NEW TMGIENS SET TMGIENS=$PIECE(TMGPARAMS,"^",2)
        IF TMGIENS="" DO  GOTO GORDONE
        . SET TMGOUT(0)="-1^No IENS supplied"
        ;
        DO GETS^DIQ(TMGFILE,TMGIENS,"**","IE","TMGARRAY","TMGMSG")
        ;
        IF $DATA(TMGMSG("DIERR")) DO  GOTO GORDONE
        . SET TMGOUT(0)="-1^See Fileman message"
        . SET TMGOUT(1)=$$GETERSTR^TMGRPC3G(.TMGMSG)
        ;
        DO XTRCTFLD(.TMGOUT,.TMGARRAY,FLAGS)
        ;
GORDONE ;
        QUIT
        ;
GET1FLD(TMGOUT,TMGPARAMS) ; 
        ;"Purpose: to get one record in file
        ;"Input: TMGOUT -- an OUT PARAMETER, PASS BY REFERENCE.
        ;"       TMGPARAMS: File^IENS^FldNum
        ;"         File -- the file or subfile to retrieve from
        ;"         IENS -- IF File is a subfile, then IENS should be full IENS to get (e.g. '2,103,')
        ;"                 IF File is not a subfile, then IENS can be just IEN or IEN_","
        ;"         FldNum -- The Field number (or name or computed expression) to lookup.
        ;"Output: TMGOUT is filled as follows:
        ;"          TMGOUT(0)="1^Success" or "-1^Message"
        ;"          TMGOUT(1)=File^IENS^FieldNum^ExternalValue
        ;"Note: the fields to return are decided HERE
        ;"NOTE: If FldNum is a WP field, then nothing is returned.
        ;"Results: none
        ;
        SET TMGOUT(0)="1^Success"  ;"default to success
        NEW TMGARRAY,TMGMSG
        NEW TMGREF SET TMGREF="TMGARRAY"
        SET TMGPARAMS=$GET(TMGPARAMS)
        SET ^TMG("TMP","RPC","GET1FLD")=TMGPARAMS
        NEW TMGFILE SET TMGFILE=$PIECE(TMGPARAMS,"^",1)
        IF +TMGFILE'>0 DO  GOTO GORDONE
        . SET TMGOUT(0)="-1^No file number supplied"
        NEW TMGIENS SET TMGIENS=$PIECE(TMGPARAMS,"^",2)
        IF TMGIENS="" DO  GOTO GOFDONE
        . SET TMGOUT(0)="-1^No IENS supplied"
        NEW TMGFLD SET TMGFLD=$PIECE(TMGPARAMS,"^",3)
        ;
        NEW TMGVAL SET TMGVAL=$$GET1^DIQ(TMGFILE,TMGIENS,TMGFLD,"",TMGREF,"TMGMSG")
        ;
        IF $DATA(TMGMSG("DIERR")) DO  GOTO GORDONE
        . SET TMGOUT(0)="-1^See Fileman message"
        . SET TMGOUT(1)=$$GETERSTR^TMGRPC3G(.TMGMSG)
        ;
        SET TMGOUT(1)=TMGFILE_"^"_TMGIENS_"^"_TMGFLD_"^"_TMGVAL
        ;
GOFDONE QUIT
        ;
GETFLDIF(TMGOUT,TMGPARAMS)  ;"GET FIELD INFO  //kt 11/17/10
        ;"Purpose: to get data dictionary information about one field in file
        ;"Input: TMGOUT -- an OUT PARAMETER, PASS BY REFERENCE.
        ;"       TMGPARAMS: File^FldNum
        ;"         File -- the file or subfile to retrieve from
        ;"         FldNum -- The Field number (or name or computed expression) to lookup.
        ;"Output: TMGOUT is filled as follows:
        ;"          TMGOUT(0)="1^Success" or "-1^Message"
        ;"          TMGOUT(1)=(returns the 0 node from ^DD(File,Fieldnum,0)
        ;"Results: none
        ;
        NEW TMGFILE SET TMGFILE=$PIECE(TMGPARAMS,"^",1)
        IF +TMGFILE'>0 DO  GOTO GFIFDN
        . SET TMGOUT(0)="-1^No numeric file number supplied. Got: "_TMGFILE
        NEW TMGFLD SET TMGFLD=$PIECE(TMGPARAMS,"^",2)
        IF +TMGFLD'>0 DO  GOTO GFIFDN
        . SET TMGOUT(0)="-1^No numeric field number supplied. Got: "_TMGFLD
        IF $DATA(^DD(+TMGFILE,+TMGFLD,0))=0 DO  GOTO GFIFDN
        . SET TMGOUT(0)="-1^No data dictionary info found."
        SET TMGOUT(0)="1^Success"
        SET TMGOUT(1)=^DD(+TMGFILE,+TMGFLD,0)
        ;
GFIFDN  QUIT
 ;
GETAFINF(TMGOUT,TMGPARAMS)  ;"GET FIELD INFO  //elh 11/19/10
        ;"Purpose: to get data dictionary information about all fields in file
        ;"Input: TMGOUT -- an OUT PARAMETER, PASS BY REFERENCE.
        ;"       TMGPARAMS: File
        ;"         File -- the file or subfile to retrieve from
        ;"Output: TMGOUT is filled as follows:
        ;"          TMGOUT(0)="1^Success" or "-1^Message"
        ;"          TMGOUT(1)=FileNum^FieldNum^{0 node from ^DD(File,Fieldnum,0)}
        ;"          TMGOUT(2)=FileNum^FieldNum^{0 node from ^DD(File,Fieldnum,0)} ... etc.
        ;"Results: none
        ;
        NEW TMGFILE SET TMGFILE=$PIECE(TMGPARAMS,"^",1)
        IF +TMGFILE'>0 DO  GOTO GAFIFDN
        . SET TMGOUT(0)="-1^No numeric file number supplied. Got: "_TMGFILE
        SET TMGOUT(0)="1^Success"
        NEW TMGFIELD SET TMGFIELD=0
        NEW COUNTER SET COUNTER=1
        FOR  SET TMGFIELD=$ORDER(^DD(+TMGFILE,+TMGFIELD)) QUIT:(+TMGFIELD'>0)  DO
        . NEW DEF SET DEF=$GET(^DD(+TMGFILE,+TMGFIELD,0))
        . SET TMGOUT(COUNTER)=+TMGFILE_"^"_+TMGFIELD_"^"_DEF
        . SET COUNTER=COUNTER+1
        . ;"IF ($PIECE(DEF,"^",2)["P")!(+$PIECE(DEF,"^",2)=$PIECE(DEF,"^",2)) DO
        . NEW P2FILE SET P2FILE=+$PIECE(DEF,"P",2)
        . IF P2FILE=0 SET P2FILE=+$PIECE(DEF,"^",2)
        . IF P2FILE>0 DO
        . . SET DEF=$GET(^DD(P2FILE,.01,0))
        . . IF $GET(^DD(P2FILE,0,"UP"))=+TMGFILE DO
        . . . IF $PIECE(DEF,"^",2)["W" QUIT
        . . . SET $PIECE(DEF,"^",2)=$TRANSLATE($PIECE(DEF,"^",2),"P","p")
        . . . IF $PIECE(DEF,"^",2)'["p" DO
        . . . . SET $PIECE(DEF,"^",2)="p"_$PIECE(DEF,"^",2)
        . . SET TMGOUT(COUNTER)=P2FILE_"^.01^"_DEF
        . . SET COUNTER=COUNTER+1
        ;
GAFIFDN QUIT
        ;
DTTOFMDT(TMGOUT,TMGPARAMS)  ;"DATE TO FMDATE  //elh 11/22/10
        ;"Purpose: To convert an external date into a FM date
        ;"Input: TMGOUT -- an OUT PARAMETER, PASS BY REFERENCE.
        ;"       TMGPARAMS: Date
        ;"         Date -- the date to convert
        ;"Output: TMGOUT is filled as follows:
        ;"          TMGOUT(0)="-1^ERROR MESSAGE" or "1^FM DATE"
        ;"Results: none
        NEW X SET X=TMGPARAMS
        SET %DT="TX"
        D ^%DT
        IF Y=-1 DO
        . SET TMGOUT(0)="-1"
        ELSE  DO
        . SET TMGOUT(0)="1^"_Y
        QUIT
        ;
XTRCTFLD(TMGOUT,TMGARRAY,TMGFLAGS) ;"EXTRACT FIELDS
        ;"Purpose: convert output from GETS^DIQ into another format
        ;"Input: TMGOUT -- AN OUT PARAMETER.  Format:
        ;"         TMGOUT(#)=File#^IENS^Field#^ExternalValue^DDInformation....   <-- if TMGFLAGS="E"
        ;"         TMGOUT(#)=File#^IENS^Field#^InternalValue^DDInformation....   <-- if TMGFLAGS="I"
        ;"         TMGOUT(#)=File#^IENS^Field#^ExternalValue^InternalValue^DDInformation....    <-- if TMGFLAGS="EI" or "IE"
        ;"       TMGARRAY -- array as returned from Fileman
        ;"       FLAGS -- "I","E","IE", or "EI" --  "E" for external values returned, "I" for internal values returned
        ;"NOTE! -- If Flags call for "I" and "E", then the piece position of the DD info is shifted 1 place to right!
        ;"Results: none   
        NEW TMGINDEX SET TMGINDEX=1
        NEW TMGFILE,TMGFIELD,TMGIENS
        SET TMGFILE=""
        FOR  SET TMGFILE=$ORDER(TMGARRAY(TMGFILE)) QUIT:(TMGFILE="")  DO
        . SET TMGIENS=""
        . FOR  SET TMGIENS=$ORDER(TMGARRAY(TMGFILE,TMGIENS)) QUIT:(TMGIENS="")  DO
        . . SET TMGFIELD=0
        . . FOR  SET TMGFIELD=$ORDER(^DD(TMGFILE,TMGFIELD)) QUIT:(+TMGFIELD'>0)  DO
        . . . NEW AFLAG FOR AFLAG="I","E" DO   
        . . . . IF TMGFLAGS[AFLAG IF $GET(TMGARRAY(TMGFILE,TMGIENS,TMGFIELD,AFLAG))="" DO
        . . . . . SET TMGARRAY(TMGFILE,TMGIENS,TMGFIELD,AFLAG)=""
        . . SET TMGFIELD=""
        . . FOR  SET TMGFIELD=$ORDER(TMGARRAY(TMGFILE,TMGIENS,TMGFIELD)) QUIT:(TMGFIELD="")  DO
        . . . NEW TMGEVALUE SET TMGEVALUE=$GET(TMGARRAY(TMGFILE,TMGIENS,TMGFIELD,"E"))
        . . . NEW TMGIVALUE SET TMGIVALUE=$GET(TMGARRAY(TMGFILE,TMGIENS,TMGFIELD,"I"))
        . . . NEW TMGDDINFO SET TMGDDINFO=$PIECE($GET(^DD(TMGFILE,TMGFIELD,0)),"^",1,99)  ;"//change 4 --> 99 on 9/28/14
        . . . IF $PIECE(TMGDDINFO,"^",2)["V" DO  ;"Thanks to Rick Marshall for this part.  Add var ptr with prefix, e.g. INFLUENZA --> IM.INFLUENZA
        . . . . IF TMGIVALUE="" QUIT
        . . . . NEW TMGROOT SET TMGROOT="^"_$PIECE(TMGIVALUE,";",2)_"0)"
        . . . . NEW TMGVFNUM SET TMGVFNUM=+$PIECE($GET(@TMGROOT),"^",2) QUIT:(TMGVFNUM'>0)
        . . . . NEW TMGVPIEN SET TMGVPIEN=+$ORDER(^DD(TMGFILE,TMGFIELD,"V","B",TMGVFNUM,0)) QUIT:(TMGVPIEN'>0)
        . . . . NEW TMGPREFIX SET TMGPREFIX=$PIECE($GET(^DD(TMGFILE,TMGFIELD,"V",TMGVPIEN,0)),"^",4) QUIT:(TMGPREFIX="")
        . . . . SET TMGEVALUE=TMGPREFIX_"."_TMGEVALUE
        . . . IF $PIECE(TMGDDINFO,"^",2)["D" DO  ;"convert data format to one Delphi can use
        . . . . IF TMGFLAGS'["E" QUIT
        . . . . SET TMGEVALUE=$$FMTE^XLFDT(TMGIVALUE,5)
        . . . NEW TMGVALUE SET TMGVALUE=""
        . . . IF TMGFLAGS["E" SET TMGVALUE=TMGEVALUE
        . . . IF TMGFLAGS["I" DO
        . . . . IF TMGFLAGS["E" SET TMGVALUE=TMGVALUE_"^"
        . . . . SET TMGVALUE=TMGVALUE_TMGIVALUE
        . . . SET TMGOUT(TMGINDEX)=TMGFILE_"^"_TMGIENS_"^"_TMGFIELD_"^"_TMGVALUE
        . . . SET TMGOUT(TMGINDEX)=TMGOUT(TMGINDEX)_"^"_TMGDDINFO
        . . . SET TMGINDEX=TMGINDEX+1
        . . . IF $PIECE(TMGDDINFO,"^",2)["V" DO
        . . . . SET TMGINDEX=$$ADDVINFO(.TMGOUT,TMGFILE,TMGFIELD)
        . . . . ;"NEW TMGREF SET TMGREF=$NAME(^DD(TMGFILE,TMGFIELD,"V",0))
        . . . . ;"FOR  SET TMGREF=$QUERY(@TMGREF) QUIT:($QSUBSCRIPT(TMGREF,3)'="V")  DO
        . . . . ;". NEW TMGREFTAIL SET TMGREFTAIL=$PIECE(TMGREF,",",3,99)
        . . . . ;". NEW TMGDDVAL SET TMGDDVAL=$GET(@TMGREF)
        . . . . ;". SET TMGOUT(TMGINDEX)="INFO^DD^"_TMGFILE_"^"_TMGFIELD_"^"_TMGREFTAIL_"&=&"_TMGDDVAL
        . . . . ;". SET TMGINDEX=TMGINDEX+1
        ;
        QUIT
        ;
ADDVINFO(TMGARRAY,TMGFILE,TMGFIELD) ;
        ;"Purpose: to add DD info to output array for a variable pointer field.
        ;"Input: TMGARRAY -- An OUT PARAMETER.  Pass by reference.  Expected format:
        ;"          TMGARRAY(#)=value.
        ;"          Data from this function will be added at TMGARRAY(#+1)=...
        ;"      TMGFILE -- The file
        ;"      TMGFIELD -- The field.
        ;"Results: Returns next available index number, e.g. #+1
        ;"Output: TMGARRAY is changed.
        ;
        NEW TMGINDEX SET TMGINDEX=+$ORDER(TMGARRAY(""),-1)+1
        NEW TMGINITINDEX SET TMGINITINDEX=TMGINDEX
        NEW TMGREF SET TMGREF=$NAME(^DD(TMGFILE,TMGFIELD,"V",0))
        FOR  SET TMGREF=$QUERY(@TMGREF) QUIT:($QSUBSCRIPT(TMGREF,3)'="V")  DO
        . NEW TMGREFTAIL SET TMGREFTAIL=$PIECE(TMGREF,",",3,99)
        . NEW TMGDDVAL SET TMGDDVAL=$GET(@TMGREF)
        . SET TMGOUT(TMGINDEX)="INFO^DD^"_TMGFILE_"^"_TMGFIELD_"^"_TMGREFTAIL_"&=&"_TMGDDVAL
        . SET TMGINDEX=TMGINDEX+1
        QUIT TMGINDEX
  ;
GFLSUBST(TMGOUT,TMGPARAMS) ;"GET FILE SUBSET
        ;"Purpose: to return a subset of entries a file's .01 names
        ;"Input: TMGOUT -- an OUT PARAMETER, PASS BY REFERENCE.
        ;"       TMGPARAMS -- FileNum^StartFrom^Direction^maxCount^[IENS]^[SCREEN]
        ;"              FileNum  -- file number to traverse
        ;"              StartFrom -- text to $ORDER() from  -- OPTIONAL
        ;"              Direction -- $ORDER(xx,Direction) direction (should be 1 or -1) -- OPTIONAL
        ;"              maxCt -- OPTIONAL -- the max number of entries to return.
        ;"              IENS -- OPTIONAL -- only used IF FileNum is a subfile.  A standard IENS string
        ;"              SCREEN -- OPTIONAL -- 
        ;"                  Format#1: 'FldNum=value|FldNum=value|FldNum=...'
        ;"                    Each SET is a matching value.  Each record that is returned
        ;"                    will match value.  Multiple sets may be supplied, delimited by
        ;"                    "|" Each returned value will match ALL restrictions.
        ;"                     Only EXTERNAL value of field compared.
        ;"                    NOTE: 'Fldnum' is passed to $$GET1^DIQ.  So this should be
        ;"                        able to be field number, or name or a FM computed expression
        ;"                  Format#2:
        ;"                      [REMDEF FLAGS:abcdefg] -- Only passed for file 811.9
        ;"                      [REMDLG ITEM PARENT:####] -- only passed for file 801.41
        ;"                      [FROM FileNum:FieldNum] -- Generic description of the file:field needing pointer options
        ;"Output: TMGOUT is filled as follows:
        ;"          TMGOUT(0)="1^Success" or "-1^Message"
        ;"          TMGOUT(1)=IEN^Value
        ;"          TMGOUT(2)=IEN^Value
        ;"          ...
        ;"Results: none
        ;
        ;"KILL ^TMG("TMP","RPC3B")
        ;"MERGE ^TMG("TMP","RPC3B","TMGPARAMS")=TMGPARAMS
        NEW TMGTEMP1,TMGTEMP2  ;"scratch variables, available to screening processes.
        NEW TMGFILE SET TMGFILE=+$PIECE(TMGPARAMS,"^",1)
        IF TMGFILE'>0 DO  GOTO GFSDONE
        . SET TMGOUT(0)="-1^No file number supplied"
        NEW TMGFROM SET TMGFROM=$PIECE(TMGPARAMS,"^",2)
        SET TMGPARAMS=$$UP^XLFSTR(TMGPARAMS)
        NEW TMGDIR SET TMGDIR=$PIECE(TMGPARAMS,"^",3)
        IF TMGDIR'=-1 SET TMGDIR=1
        NEW TMGMAXCT SET TMGMAXCT=+$PIECE(TMGPARAMS,"^",4)
        IF TMGMAXCT=0 SET TMGMAXCT=44
        NEW TMGISPTR SET TMGISPTR=($PIECE($GET(^DD(TMGFILE,.01,0)),"^",2)["P")
        NEW TMGSTARTIEN SET TMGSTARTIEN=""
        IF TMGISPTR DO
        . IF $LENGTH(TMGFROM,";")>2 SET TMGSTARTIEN=+$PIECE(TMGFROM,";",2)
        . IF TMGFROM?1.N1";".E SET TMGFROM=+TMGFROM
        NEW TMGIENS  ;"//kt added
        SET TMGIENS=$PIECE(TMGPARAMS,"^",5)
        NEW TMGSCRN SET TMGSCRN=$PIECE(TMGPARAMS,"^",6)
        NEW TMGFNSCRN SET TMGFNSCRN=""
        ;
        IF $EXTRACT(TMGSCRN,1)="[" DO
        . SET TMGFNSCRN=$$GFSSCRN(TMGSCRN),TMGSCRN=""
        ELSE  FOR  QUIT:TMGSCRN=""  DO
        . NEW SA SET SA=$PIECE(TMGSCRN,"|",1)
        . SET TMGSCRN=$PIECE(TMGSCRN,"|",2,999)
        . NEW I SET I=+$ORDER(TMGSCRN(""),-1)+1
        . SET TMGSCRN(I,"FLD")=$PIECE(SA,"=",1)
        . SET TMGSCRN(I,"VAL")=$PIECE(SA,"=",2,999)
        ;
        ;"MERGE ^TMG("TMP","RPC3B","TMGSCRN")=TMGSCRN
        NEW TMGI SET TMGI=0
        ;"NEW TMGLAST SET TMGLAST=""
        ;"NEW prev SET prev=""
        ;"NEW TMGREF SET TMGREF=$GET(^DIC(TMGFILE,0,"GL"))
        NEW TMGREF SET TMGREF=$$GETGREF^TMGFMUT2(TMGFILE,TMGIENS) ;"IENS not used unless TMGFILE is a subfile
        SET TMGREF=$$CREF^DILF(TMGREF)  ;"convert open --> closed reference
        IF TMGREF="" DO  GOTO GFSDONE
        . SET TMGOUT(0)="-1^Unable to obtain global ref for file #"_TMGFILE
        ;
        FOR  SET TMGFROM=$ORDER(@TMGREF@("B",TMGFROM),TMGDIR) QUIT:(TMGFROM="")!(TMGI'<TMGMAXCT)  DO
        . NEW TMGIEN SET TMGIEN=TMGSTARTIEN
        . FOR  SET TMGIEN=$ORDER(@TMGREF@("B",TMGFROM,TMGIEN),TMGDIR) QUIT:(+TMGIEN'>0)!(TMGI'<TMGMAXCT)  DO
        . . NEW TMGIENS0
        . . IF TMGIENS="" SET TMGIENS0=TMGIEN_","
        . . ELSE  DO
        . . . SET TMGIENS0=TMGIENS
        . . . SET $PIECE(TMGIENS0,",",1)=TMGIEN
        . . NEW TMGFLDS SET TMGFLDS=".01"
        . . IF TMGFNSCRN'="" DO  QUIT:(TMGOK'=1)
        . . . XECUTE TMGFNSCRN
        . . . SET TMGOK=$T
        . . IF $DATA(TMGSCRN)>1 DO
        . . . NEW I SET I=0
        . . . FOR  SET I=$ORDER(TMGSCRN(I)) QUIT:(I'>0)  DO
        . . . . NEW FLD SET FLD=$GET(TMGSCRN(I,"FLD"))
        . . . . IF (FLD=.01)!(FLD="") QUIT
        . . . . SET TMGFLDS=TMGFLDS_";"_FLD
        . . NEW TMGARRAY,TMGMSG
        . . DO GETS^DIQ(TMGFILE,TMGIENS0,TMGFLDS,"E","TMGARRAY","TMGMSG")
        . . NEW TMGOK SET TMGOK=1
        . . IF $DATA(TMGSCRN)>1 DO  QUIT:(TMGOK'=1)
        . . . NEW I SET I=0
        . . . FOR  SET I=$ORDER(TMGSCRN(I)) QUIT:(I'>0)!(TMGOK'=1)  DO
        . . . . NEW FLD SET FLD=$GET(TMGSCRN(I,"FLD"))
        . . . . IF $$UP^XLFSTR($GET(TMGARRAY(TMGFILE,TMGIENS0,FLD,"E")))'=$GET(TMGSCRN(I,"VAL")) SET TMGOK=0 QUIT
        . . SET TMGI=TMGI+1
        . . SET TMGOUT(TMGI)=TMGIEN_"^"
        . . IF TMGISPTR SET TMGOUT(TMGI)=TMGOUT(TMGI)_TMGFROM_";"_TMGIEN_";"
        . . SET TMGOUT(TMGI)=TMGOUT(TMGI)_$GET(TMGARRAY(TMGFILE,TMGIENS0,.01,"E"))
        . . ;"SET TMGOUT(TMGI)=TMGOUT(TMGI)_$$GET1^DIQ(TMGFILE,TMGIENS0,.01)
        ;
        SET TMGOUT(0)="1^Success"
GFSDONE QUIT
        ;
GFSSCRN(TMGSCRN) ;"Convert special screens, called from GFLSUBST
        ;"Input: TMGSCRN -- text of screen messgae
        ;"Also may use TMGFILE in global scope -- the target Filenumber.
        ;"Result: returns like of code to execute for each TMGIEN, that sets $T
        NEW RESULT SET RESULT=""
        IF TMGSCRN["REMDEF FLAGS" DO
        . NEW FLGS SET FLGS=$PIECE(TMGSCRN,":",2),FLGS=$PIECE(FLGS,"]",1),TMGSCRN=""
        . SET RESULT="IF $$IFREMFLG^TMGRPC3G(TMGIEN,"""_FLGS_""")"
        ELSE  IF TMGSCRN["REMDLG ITEM PARENT" DO
        . NEW PARENTIEN SET PARENTIEN=+$PIECE(TMGSCRN,":",2),TMGSCRN=""
        . SET RESULT="IF $$IFRMDLGO^TMGRPC3G(TMGIEN,"_PARENTIEN_")"
        . SET TMGTEMP1(PARENTIEN)=""                  ;"NOTE: TMGTEMP1 was NEW'd in GFLSUBST.  
        . DO LOADPRNT^TMGRPC3G(PARENTIEN,.TMGTEMP1)   ;"      Will be used in IFRMDLGO^TMGRPC3G
        ELSE  IF TMGSCRN["[FROM " DO
        . NEW STR,CONTFNUM,CONTFLD ;"Contianing File:field that needs pointer options.
        . SET STR=$PIECE(TMGSCRN,"[FROM ",2)
        . SET CONTFNUM=+$PIECE(STR,":",1),CONTFLD=+$PIECE(STR,":",2)
        . IF ((CONTFNUM="801.41")&(CONTFLD="15"))!((CONTFNUM="801.4118")&(CONTFLD=".01")) DO
        . . NEW TARGETFNUM SET TARGETFNUM=+$GET(TMGFILE) QUIT:TARGETFNUM'>0
        . . NEW IDX SET IDX=0  ;"Get screen for variable pointer for FINDING ITEM in 801.41 (REMINDER DIALOG)
        . . FOR  SET IDX=$ORDER(^DD(801.41,15,"V",IDX)) QUIT:(+IDX'>0)!(RESULT'="")  DO
        . . . IF +$PIECE($GET(^DD(801.41,15,"V",IDX,0)),"^",1)'=TARGETFNUM QUIT
        . . . NEW ASCRN SET ASCRN=$GET(^DD(801.41,15,"V",IDX,1)) QUIT:(ASCRN="")
        . . . NEW DIC XECUTE ASCRN
        . . . IF $GET(DIC("S"))="" QUIT
        . . . SET RESULT="N Y S Y=TMGIEN "_DIC("S")
        QUIT RESULT
        ;
GSFSUBST(TMGOUT,TMGPARAMS) ;"GET SUBFILE SUBSET   //kt 11/24/10
        ;"Purpose: to return a subset of entries a file's .01 names
        ;"Input: TMGOUT -- an OUT PARAMETER, PASS BY REFERENCE.
        ;"       TMGPARAMS -- SubfileNum^IENS^StartFrom^Direction^maxCount^[SCREEN]
        ;"              FileNum - subfilename file to traverse
        ;"              IENS -- needed to locate particular subfile instance
        ;"              StartFrom -- text to $ORDER() from  -- OPTIONAL
        ;"              Direction -- $ORDER(xx,Direction) direction (should be 1 or -1) -- OPTIONAL
        ;"              maxCt -- OPTIONAL -- the max number of entries to return.
        ;"              SCREEN -- OPTIONAL -- see documentation in GFLSUBST
        ;"Output: TMGOUT is filled as follows:
        ;"          TMGOUT(0)="1^Success" or "-1^Message"
        ;"          TMGOUT(1)=IEN^Value
        ;"          TMGOUT(2)=IEN^Value
        ;"          ...
        ;"Results: none
        ;
        NEW P2
        MERGE ^TMG("TMP","RPC3B","TMGPARAMS")=TMGPARAMS
        SET P2=$P(TMGPARAMS,"^",1)_"^"_$P(TMGPARAMS,"^",3,5)_"^"_$P(TMGPARAMS,"^",2)_"^"_$P(TMGPARAMS,"^",6)
        SET ^EDDIE(2)=P2
        MERGE ^TMG("TMP","RPC3B","P2")=P2
        DO GFLSUBST(.TMGOUT,P2)
        QUIT
        ;
