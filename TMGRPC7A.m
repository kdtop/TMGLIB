TMGRPC7A ;TMG/kst/Support Functions for Fileman Desktop ;02/15/11, 2/2/14
         ;;1.0;TMG-LIB;**1**;02/15/11
 ;
 ;"TMG RPC FUNCTIONS for a Fileman Desktop program
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
 ;"=======================================================================
 ;" RPC -- Public Functions.
 ;"=======================================================================
 ;" <none>
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;"Dependencies:
 ;"
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;
ADDFLD(TMGRESULT,TMGINPUT) ;
        ;"Purpose: to parse input from RPC call into that ready for ADDFIELD^TMGDICATT, and handle return
        ;"Input: TMGRESULT -- an OUT PARAMETER, PASS BY REFERENCE.
        ;"       TMGINPUT -- holds the data to be used in creating field:
        ;"                   TMGINPUT(0)="node1;node2;node3;...=value
        ;"                   TMGINPUT(1)="node1;node2;node3;...=value
        ;"                   TMGINPUT(2)="node1;node2;node3;...=value
        ;"                   ...
        ;"                   These nodes and values are parsed into an array that
        ;"                   is taken as input array for ultimate function
        ;"                   ADDFIELD^TMGDICATT(TMGINFO,TMGRESULT).  Input
        ;"                   documentation is lengthy, so see that function for details.
        ;"Output:
        ;"       TMGRESULT(0): "1^Success" or "-1^Message"
        ;"       TMGRESULT(1) will contain Fileman error, IF any
        ;"       TMGRESULT(2) will contain second line of Fileman error, IF any
        ;"       ...
        ;"Results: None
        NEW TMGINFO,TMGMSG
        NEW TMGI SET TMGI=""
        FOR  SET TMGI=$ORDER(TMGINPUT(TMGI)) QUIT:(+TMGI'>0)  DO
        . NEW VAL,NODE,STR
        . NEW REF SET REF="TMGINFO"
        . SET STR=$GET(TMGINPUT(TMGI)) QUIT:(STR="")
        . SET VAL=$PIECE(STR,"=",2)
        . SET STR=$PIECE(STR,"=",1)
        . NEW TMGJ
        . FOR TMGJ=1:1:$LENGTH(STR,";") DO
        . . SET NODE=$PIECE(STR,";",TMGJ)
        . . IF NODE="" QUIT
        . . SET REF=$NAME(@REF@(NODE))
        . SET @REF=VAL
        SET TMGRESULT(0)=$$ADDFIELD^TMGDICATT(.TMGINFO,.TMGMSG)
        IF $DATA(TMGMSG) DO
        . NEW CT SET CT=1
        . SET TMGI=""
        . FOR  SET TMGI=$ORDER(TMGMSG(TMGI)) QUIT:(TMGI="")  DO
        . . SET TMGRESULT(CT)=$GET(TMGMSG(TMGI)),CT=CT+1
        ;
        QUIT
        ;
FLDHASDATA(TMGRESULT,TMGPARAMS) ;
        ;"Purpose: to return IF a field, in a given file, has data.
        ;"Input: TMGRESULT -- an OUT PARAMETER, PASS BY REFERENCE.
        ;"       TMGPARAMS: FileNum^FieldNum
        ;"         FileNum -- the file or subfile to check in
        ;"                      NOTE: IENS is not required for subfile because ALL possibilities are checked.
        ;"         FieldNum-- the field number to check
        ;"Output:
        ;"       TMGRESULT(0): 1=At least 1 field does has data.  0=No fields have data, or there was a problem.
        ;"Results: None
        NEW TMGFILE,TMGFLD
        SET TMGFILE=$PIECE(TMGPARAMS,"^",1)
        SET TMGFLD=$PIECE(TMGPARAMS,"^",2)
        SET TMGRESULT(0)=$$FLDHASDATA^TMGFMUT3(TMGFILE,TMGFLD)
        QUIT
        ;
DELFLD(TMGRESULT,TMGPARAMS) ;" <-- CAUTION.  NO VERIFY PRIOR TO DELETE
        ;"Purpose: to return IF a field, in a given file, has data.
        ;"Input: TMGRESULT -- an OUT PARAMETER, PASS BY REFERENCE.
        ;"       TMGPARAMS: FileNum^FieldNum
        ;"         FileNum -- the file holding field.
        ;"         FieldNum-- the field number to delete
        ;"Output:
        ;"       TMGRESULT(0): "1^Success" or "-1^Message"
        ;"       TMGRESULT(1) will contain message line, IF any
        ;"       TMGRESULT(2) will contain message line, IF any
        ;"       ...
        ;"Results: None
        NEW TMGFILE,TMGFLD,TMGINFO,TMGMSG
        SET TMGFILE=$PIECE(TMGPARAMS,"^",1)
        SET TMGFLD=$PIECE(TMGPARAMS,"^",2)
        SET TMGINFO("FILE")=TMGFILE
        SET TMGINFO("FLD","NUM")=TMGFLD
        SET TMGINFO("DELETE DATA")="Y"
        ;
        SET TMGRESULT(0)=$$DELFIELD^TMGDICATT(.TMGINFO,.TMGMSG)
        IF $DATA(TMGMSG) DO
        . NEW CT SET CT=1
        . SET TMGI=""
        . FOR  SET TMGI=$ORDER(TMGMSG(TMGI)) QUIT:(TMGI="")  DO
        . . SET TMGRESULT(CT)=$GET(TMGMSG(TMGI)),CT=CT+1
        QUIT
        ;
