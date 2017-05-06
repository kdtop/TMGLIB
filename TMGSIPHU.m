TMGSIPHU ;TMG/kst/SIPHON PROGRAM, FOR TRANSFERRING VISTA INSTANCES ;11/27/09, 2/2/14
         ;;1.0;TMG-LIB;**1**;11/27/09
 ;
 ;"TMG SIPHON PROGRAM, FOR TRANSFERRING VISTA INSTANCE
 ;"UTILITY FUNCTIONS
 ;"Kevin Toppenberg MD
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
 ;"ORDREF(REF) -- return a $ORDER on a reference
 ;"QLASTSUB(REF) -- Returns the LAST subscript of reference
 ;"QSUBS(REF,ENDNUM,STARTNUM) -- Return subscripts from START to END ***NOTE ORDER OF PARAMETERS.
 ;"QSETSUB(REF,POS,VALUE) -- Set the subscript in REF as position POS to be VALUE
 ;"GETREF0(FILENUM) -- Returns reference to 0 node for file.
 ;"GETNUMREC(FILENUM) -- Return the highest record number in given file.
 ;"STOREDATA(ARRAY) -- store data from array into local globals, making backup of overwritten records
 ;"IENOFARRAY(FILENUM,ARRAY,IENS) --return the IEN record number of the array.
 ;"APPENDIEN(FILENUM,IENS) --return an IEN number that is +1 from the last one in the file.
 ;"RLOCARRAY(FILENUM,NEWIEN,ARRAY,NARRAY)  --Relocate array (change IEN)
 ;"STOREDAS(FILENUM,IEN,ARRAY) -- Store data from array into local globals, making backup of
 ;"                            overwritten records.  AND ALSO translate record number to input-specified IEN
 ;"GETFLD(FILENUM,LOC,PCE) -Return field number cooresponding to File number, node, and piece.
 ;"
 ;"=======================================================================
 ;" API -- Private Functions.
 ;"=======================================================================
 ;"UNNEEDPTR(FILENUM,RPTR,LPTR,INOUT,TALLY) -- satisfy all the places that were wanting a remote record to be downloaded
 ;"ISDIFF(ARRAY) -- determine IF record stored in ARRAY is different from that stored in local ^Global
 ;"RECSHOW(FILENUM,RPTR,ARRAY) -- Show remote and local data, to allow user to see differences
 ;"GET01FIELD(FILENUM,ARRAY,RVALUE,LVALUE,IENS) -- Extract .01 field name from data array
 ;"GETTARGETIEN(FILENUM,ARRAY,TARGETIEN) --determine IF a local record should be overwritten with record from server.
 ;"                Ask user directly IF not able to automically determine.
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"TMGUSRIF
 ;"=======================================================================
 ;
ORDREF(REF)
        ;"Purpose: to return a $ORDER on a reference
        ;"              e.g.  ^TIU(8925,"")  --> returns ^TIU(8925,0)
        ;"                    ^TIU(8925)     --> returns ^TIU(8925.1)
        ;"NOTE: If there is no further nodes AT THE LEVEL OF THE LAST PARAMETER, then "" is returned.
        ;"      e.g.   A("Fruits","Citrus","Orange")
        ;"             A("Fruits","Citrus","Green")
        ;"             A("Fruits","Non-Citrus","Red","Hard")
        ;"             A("Fruits","Non-Citrus","Red","Soft")
        ;"             A("Fruits","Tropic","Yellow")
        ;"             A("Fruits","Tropic","Blue")
        ;"           In this example, $ORDREF(A("Fruits","Non-Citrus","Red","Soft")), would return ""
        ;"           This is difference from $QUERY, which would return A("Fruits","Tropic","Yellow")
        ;"Input --REF -- reference to a global.  Must be in Closed format
        ;"Results: Returns NEW reference.
        NEW RESULT,SUB
        SET SUB=$ORDER(@REF)
        IF SUB'="" DO
        . SET RESULT=REF
        . DO QSETSUB(.RESULT,$QLENGTH(REF),SUB)
        ELSE  SET RESULT=""
        QUIT RESULT
 ;
 ;
QLASTSUB(REF) ;
        ;"Returns the LAST subscript of reference
        ;"Input:  REF -- The reference to work on, e.g. ^TIU(8925,3,0)  MUST be in closed form
        QUIT $QSUBSCRIPT(REF,$QLENGTH(REF))
 ;
 ;
QSUBS(REF,ENDNUM,STARTNUM)  ;"***NOTE ORDER OF PARAMETERS.  IT IS 'BACKWARDS', so STARTNUM can be optional
        ;"Purpose: Return subscripts from START to END
        ;"Input:  REF -- The reference to work on, e.g. ^TIU(8925,3,0)  MUST be in closed form
        ;"        ENDNUM -- The ending subscript to return.
        ;"        STARTNUM -- The starting subscript to return.  OPTIONAL.  Default is 0
        ;"Returns the reference, in closed for.
        NEW I,RESULT SET RESULT=""
        SET STARTNUM=+$GET(STARTNUM)
        SET ENDNUM=+$GET(ENDNUM)
        IF ENDNUM>$QLENGTH(REF) SET ENDNUM=$QLENGTH(REF)
        FOR I=STARTNUM:1:ENDNUM DO
        . NEW ONENODE SET ONENODE=$QSUBSCRIPT(REF,I)
        . IF (+ONENODE'=ONENODE),(I>0) SET ONENODE=""""_ONENODE_""""
        . SET RESULT=RESULT_ONENODE
        . IF I=0 SET RESULT=RESULT_"("
        . ELSE  SET RESULT=RESULT_","
        SET RESULT=$$CREF^DILF(RESULT)
        IF (RESULT'["("),($EXTRACT(RESULT,$LENGTH(RESULT))=",") DO
        . SET RESULT=$EXTRACT(RESULT,1,$LENGTH(RESULT)-1)_")"
        QUIT RESULT
 ;
 ;
QSETSUB(REF,POS,VALUE) ;
        ;"Purpose: Set the subscript in REF as position POS to be VALUE
        ;"Input:  REF --  The reference to modify.  PASS BY REFERENCE
        ;"        POS -- The position of the subscript to change.  POS=1 means first subscript
        ;"        VALUE -- The NEW subscript number or name
        ;"Output: REF is modified
        ;"Results: none
        IF (POS>$QLENGTH(REF))!(POS<1) QUIT
        NEW REFA SET REFA=$$QSUBS(REF,POS-1)
        SET REFA=$$OREF^DILF(REFA)
        NEW REFB SET REFB=$$QSUBS(REF,999,POS+1)
        IF REFB="" SET REFB=")"
        ELSE  SET REFB=","_REFB
        IF (+VALUE'=VALUE),($EXTRACT(VALUE,1)'="""") SET VALUE=""""_VALUE_""""
        SET REF=REFA_VALUE_REFB
        QUIT
 ;
 ;
GETREF0(FILENUM)
        ;"Purpose: Returns reference to 0 node for file.
        ;"Input: FILENUM -- The fileman number of the file to return info for.
        ;"Result: RETURNS REF, OR "" IF problem.
        NEW REF SET REF=$GET(^DIC(FILENUM,0,"GL"))
        IF REF'="" SET REF=REF_"0)"
        QUIT REF
 ;
 ;
GETNUMREC(FILENUM)
        ;"Purpose: Return the highest record number in given file.
        ;"Input: FILENUM -- The fileman number of the file to return info for.
        ;"Results: returns number, or -1 IF problem.
        ;"WRITE "Here in GETNUMRECS",!
        NEW RESULT,REF,NODE
        SET RESULT=-1
        SET REF=$$GETREF0(FILENUM)
        IF REF'="" SET RESULT=$PIECE($GET(@REF),"^",4)
        IF RESULT="" SET RESULT=-1
        QUIT RESULT
 ;
 ;
STOREDATA(ARRAY)
        ;"Purpose: To store data from array into local globals, making backup of
        ;"         overwritten records
        ;"Input: ARRAY -- Pass by REFERENCE.  Format
        ;"          ARRAY(1)=ARef_"="
        ;"          ARRAY(2)="="_AValue
        ;"          ARRAY(3)=ARef_"="
        ;"          ARRAY(4)="="_AValue
        ;"          ...
        ;"Results: none
        NEW STIME SET STIME=$H
        NEW TMGI SET TMGI=1
        NEW TMGCT SET TMGCT=0
        NEW SHOWPROG SET SHOWPROG=0
        NEW SHOWREF SET SHOWREF=0
        NEW REF,VALUE
        FOR  DO  QUIT:(TMGI="")
        . SET REF=$GET(ARRAY(TMGI))
        . SET REF=$EXTRACT(REF,1,$LENGTH(REF)-1)
        . IF REF="" SET TMGI="" QUIT
        . SET TMGI=TMGI+1
        . SET VALUE=$GET(ARRAY(TMGI))
        . SET VALUE=$EXTRACT(VALUE,2,10000)
        . IF $DATA(@REF) DO
        . . MERGE ^TMG("TMGSIPH","OVERWRITTEN",REF)=@REF
        . . KILL @REF
        . SET @REF=VALUE
        . SET TMGI=$ORDER(ARRAY(TMGI))
        . SET TMGCT=TMGCT+1
        . IF (SHOWPROG=0),($$HDIFF^XLFDT($H,STIME,2)>15) DO  ;"Turn on progress bar after 15 seconds.
        . . SET SHOWPROG=1
        . . SET TMGMIN=$ORDER(ARRAY(0))
        . . SET TMGMAX=$ORDER(ARRAY(""),-1)
        . IF (SHOWPROG=1),(TMGCT>500) DO
        . . IF (SHOWREF=0),($$HDIFF^XLFDT($H,STIME,2)>120) DO  ;"Turn on showing referecences after 2 min.
        . . NEW SREF SET SREF=""
        . . IF SHOWREF DO
        . . . SET SREF=REF QUIT:($LENGTH(REF)'>20)
        . . . SET SREF=$EXTRACT(REF,1,17)_"..."
        . . DO PROGBAR^TMGUSRI2(TMGI,"Storing Data: "_SREF,TMGMIN,TMGMAX,70,STIME)
        . . SET TMGCT=0
        ;
        QUIT
 ;
 ;
IENOFARRAY(FILENUM,ARRAY,IENS) ;"
        ;"Purpose: return the IEN record number of the array.
        ;"Input: FILENUM -- The file number of the data passed in array.  MUST MATCH
        ;"       ARRAY -- Pass by REFERENCE.  Format
        ;"         ARRAY(1)=ARef_"="    <---- Expected to hold the .01 field.
        ;"         ARRAY(2)="="_AValue
        ;"         ARRAY(3)=ARef_"="
        ;"         ARRAY(4)="="_AValue
        ;"       IENS -- OPTIONAL (needed If FILENUM is a subfile) -- A standard IENS for subfile.
        ;"Result: IEN IF found, or 0 IF error.
        ;"        NOTE: Even IF FILENUM is a subfile, IEN is a single number, i.e. IEN of subrecord
        ;"              e.g. '3' not '3,23456,'
        ;"
        NEW RESULT SET RESULT=0
        SET FILENUM=+$GET(FILENUM) IF FILENUM'>0 GOTO IOADN
        ;"NEW GREF SET GREF=$GET(^DIC(FILENUM,0,"GL"))
        NEW GREF SET GREF=$$GETGREF^TMGFMUT2(FILENUM,.IENS) ;"IENS not used IF not subfile.
        NEW CGREF SET CGREF=$$CREF^DILF(GREF)
        IF GREF="" GOTO IOADN
        NEW GREFLEN SET GREFLEN=$QLENGTH(CGREF)
        NEW REF SET REF=$GET(ARRAY(1)) IF (REF="") GOTO IOADN
        SET REF=$EXTRACT(REF,1,$LENGTH(REF)-1) IF (REF="") GOTO IOADN
        IF $$QSUBS(REF,GREFLEN)'=CGREF GOTO IOADN
        SET RESULT=$QSUBSCRIPT(REF,GREFLEN+1)
IOADN   QUIT RESULT
 ;
 ;
APPENDIEN(FILENUM,IENS) ;
        ;"Purpose: to return an IEN number that is +1 from the last one in the file.
        ;"Return : the NEW IEN, or 0 IF problem
        NEW RESULT SET RESULT=0
        ;"NEW GREF SET GREF=$GET(^DIC(FILENUM,0,"GL")) IF GREF="" GOTO AIEDN
        NEW GREF SET GREF=$$GETGREF^TMGFMUT2(FILENUM,.IENS) ;"IENS not used IF not subfile.
        NEW CGREF SET CGREF=$$CREF^DILF(GREF)
        NEW LASTIEN SET LASTIEN="%"
        FOR  SET LASTIEN=$ORDER(@CGREF@(LASTIEN),-1) QUIT:(LASTIEN="")!(+LASTIEN=LASTIEN)
        SET RESULT=LASTIEN+1
        IF $GET(IENS)["," DO
        . SET $PIECE(IENS,",",1)=RESULT
        . SET RESULT=IENS
AIEDN  QUIT RESULT
 ;
 ;
RLOCARRAY(FILENUM,NEWIEN,ARRAY,NARRAY)  ;"Relocate array (change IEN)
        ;"Purpose: To take array, and change IEN values to NEWIEN
        ;"NOTE: It is assumed that ALL data in ARRAY represents ONE record (not multiple!)
        ;"      The array MAY contain cross-references data
        ;"Input: FILENUM -- The file (or subfile) number of the data passed in array.  MUST MATCH
        ;"       NEWIEN -- The IEN that the data in ARRAY should be changed to.
        ;"                 If FILENUM is a subfile, then NEWIEN should be in standard IENS format (e.g. '7,345,')
        ;"       ARRAY -- Pass by REFERENCE.  Format
        ;"         ARRAY(1)=ARef_"="
        ;"         ARRAY(2)="="_AValue
        ;"         ARRAY(3)=ARef_"="
        ;"         ARRAY(4)="="_AValue
        ;"         ...
        ;"       NARRAY -- PASS BY REFERENCE, an OUT PARAMETER.  Format same as ARRAY
        ;"         NARRAY(1)=ARef_"="
        ;"         NARRAY(2)="="_AValue
        ;"         ...
        ;"Results: 1 if OK, -1 IF error
        ;
        KILL NARRAY
        NEW RESULT SET RESULT=-1
        NEW SHOWPROG SET SHOWPROG=0
        NEW STIME SET STIME=$H
        SET FILENUM=+$GET(FILENUM) IF FILENUM'>0 GOTO RLAD
        SET NEWIEN=$GET(NEWIEN) IF +NEWIEN'>0 GOTO RLAD
        NEW GREF SET GREF=$$GETGREF^TMGFMUT2(FILENUM,NEWIEN)
        ;"NEW GREF SET GREF=$GET(^DIC(FILENUM,0,"GL"))
        NEW CGREF SET CGREF=$$CREF^DILF(GREF)
        IF GREF="" GOTO SDAD
        ;"Check to see that the ARRAY data is referenced to same place as FILENUM
        NEW GREFLEN SET GREFLEN=$QL(CGREF)
        NEW REF SET REF=$GET(ARRAY(1)) IF (REF="") GOTO RLAD
        SET REF=$EXTRACT(REF,1,$LENGTH(REF)-1) IF (REF="") GOTO RLAD
        IF $$QSUBS(REF,GREFLEN)'=CGREF GOTO RLAD
        NEW VALUE,RECNUM
        NEW OLDIEN SET OLDIEN=""
        NEW DONE SET DONE=0
        NEW TMGCT SET TMGCT=0
        NEW TMGI SET TMGI=0
        FOR  SET TMGI=$ORDER(ARRAY(TMGI)) QUIT:(TMGI="")!DONE  DO
        . SET REF=$GET(ARRAY(TMGI))
        . SET REF=$EXTRACT(REF,1,$LENGTH(REF)-1)
        . SET TMGI=TMGI+1
        . IF REF="" SET DONE=1 QUIT
        . SET REC=$QSUBSCRIPT(REF,GREFLEN+1) ;"Get IEN of ARRAY data
        . IF OLDIEN="",(+REC=REC) SET OLDIEN=REC
        . IF REC'=+NEWIEN DO
        . . IF (+REC=REC) DO  ;"Change record number in reference
        . . . SET REF=GREF_+NEWIEN_","_$$QSUBS(REF,99,GREFLEN+2)
        . . ELSE  DO  ;"Redirect XREF value.
        . . . NEW PT2 SET PT2=$QSUBSCRIPT(REF,$QLENGTH(REF))
        . . . IF PT2'=OLDIEN QUIT  ;"Unexpected format of xref
        . . . DO QSETSUB(.REF,$QLENGTH(REF),+NEWIEN) ;"Change pointer in last position.
        . SET VALUE=$EXTRACT($GET(ARRAY(TMGI)),2,10000)
        . SET NARRAY(TMGI-1)=REF_"="
        . SET NARRAY(TMGI)="="_VALUE
        . IF (SHOWPROG=0),($$HDIFF^XLFDT($H,STIME,2)>15) DO  ;"Turn on progress bar after 15 seconds.
        . . SET SHOWPROG=1
        . . SET TMGMIN=$ORDER(ARRAY(0))
        . . SET TMGMAX=$ORDER(ARRAY(""),-1)
        . SET TMGCT=TMGCT+1
        . IF (SHOWPROG=1),(TMGCT>500) DO
        . . DO PROGBAR^TMGUSRI2(TMGI,"Shifting Data: ",TMGMIN,TMGMAX,70,STIME)
        . . SET TMGCT=0
        SET RESULT=1
RLAD    QUIT RESULT
 ;
 ;
STOREDAS(FILENUM,IEN,ARRAY)  ;"'STORE DATA AS'
        ;"Purpose: To store data from array into local globals, making backup of
        ;"         overwritten records.  AND ALSO translate record number to input-specified IEN
        ;"NOTE: It is assumed that ALL data in ARRAY represents ONE record (not multiple!)
        ;"      The array MAY contain cross-references data
        ;"Input: FILENUM -- The file number of the data passed in array.  MUST MATCH
        ;"       IEN -- The IEN that the data in ARRAY should be changed to.
        ;"              If FILENUM is a subfile, then pass a standard IENS string in IEN
        ;"       ARRAY -- Pass by REFERENCE.  Format
        ;"         ARRAY(1)=ARef_"="
        ;"         ARRAY(2)="="_AValue
        ;"         ARRAY(3)=ARef_"="
        ;"         ARRAY(4)="="_AValue
        ;"         ...
        ;"Also -- Makes use of Globally-scoped variable TMGOWSAVE.  If =0, overwritten records are NOT saved
        ;"Results: 1 if OK, -1 IF error
        ;"NOTE: Subfile support not completed yet...
        NEW RESULT SET RESULT=-1
        NEW NARRAY
        NEW SHOWPROG SET SHOWPROG=0
        NEW SHOWREF SET SHOWREF=0
        NEW TMGCT SET TMGCT=0
        NEW STIME SET STIME=$H
        IF $$IENOFARRAY(FILENUM,.ARRAY,IEN)=+NEWIEN GOTO SDA2
        IF $$RLOCARRAY(FILENUM,NEWIEN,.ARRAY,.NARRAY)'=1 GOTO SDAD  ;"Relocate array (change IEN)
        KILL ARRAY MERGE ARRAY=NARRAY
SDA2    NEW TMGI SET TMGI=0
        NEW DONE SET DONE=0
        FOR  SET TMGI=$ORDER(ARRAY(TMGI)) QUIT:(TMGI="")!DONE  DO
        . SET REF=$GET(ARRAY(TMGI))
        . SET REF=$EXTRACT(REF,1,$LENGTH(REF)-1)
        . SET TMGI=TMGI+1
        . IF REF="" SET DONE=1 QUIT
        . NEW VALUE SET VALUE=$EXTRACT($GET(ARRAY(TMGI)),2,10000)
        . ;"WRITE REF,!
        . IF $DATA(@REF) DO
        . . IF +$GET(TMGOWSAVE)=0 QUIT
        . . MERGE ^TMG("TMGSIPH","OVERWRITTEN",REF)=@REF
        . . KILL @REF
        . SET @REF=VALUE
        . IF (SHOWPROG=0),($$HDIFF^XLFDT($H,STIME,2)>15) DO  ;"Turn on progress bar after 15 seconds.
        . . SET SHOWPROG=1
        . . SET TMGMIN=$ORDER(ARRAY(0))
        . . SET TMGMAX=$ORDER(ARRAY(""),-1)
        . SET TMGCT=TMGCT+1
        . IF (SHOWPROG=1),(TMGCT>500) DO
        . . IF (SHOWREF=0),($$HDIFF^XLFDT($H,STIME,2)>120) DO  ;"Turn on showing referecences after 2 min.
        . . NEW SREF SET SREF=""
        . . IF SHOWREF DO
        . . . SET SREF=REF QUIT:($LENGTH(REF)'>20)
        . . . SET SREF=$EXTRACT(REF,1,17)_"..."
        . . DO PROGBAR^TMGUSRI2(TMGI,"Storing Data: "_SREF,TMGMIN,TMGMAX,70,STIME)
        . . SET TMGCT=0
        SET RESULT=1
SDAD    QUIT RESULT
 ;
 ;
UNNEEDPTR(FILENUM,RPTR,LPTR,INOUT,TALLY) ;
        ;"Purpose: To satisfy all the places that were wanting a remote record to be downloaded
        ;"Input:  FILENUM -- the fileman number of file (or subfile) to get from remote server
        ;"                   If FILENUM is a subfile, then can be passed as just subfilenumber, OR
        ;"                   in format: SubFileNum{ParentFileNum...
        ;"        RPTR -- The IEN of the record that was wanted from the server.
        ;"                If dealing with subfiles, pass in standard IENS format (e.g. '7,2345,')
        ;"        LPTR -- OPTIONAL.  This can specify IF the desired REMOTE record has been
        ;"                stored at a different IEN locally.
        ;"                If dealing with subfiles, pass in standard IENS format (e.g. '7,2345,')
        ;"        INOUT -- OPTIONAL -- Default is "PTOUT".  Should be "PTIN" or "PTOUT"
        ;"        TALLY -- OPTIONAL.  PASS BY REFERENCE.  An array to keep progress stats.  Format:
        ;"                 TALLY("UNNEEDED RECORDS")=#
        ;"NOTE:  Gobal ^TMG("TMGSIPH","NEEDED RECORDS","PTOUT") used, with format as below:
        ;"             ^TMG("TMGSIPH","NEEDED RECORDS","PTOUT",FILENUM,RemotePointer,RefToNodeToBeCorrected,INFO)=""
        ;"                      INFO=DataPiece^PointedToFile^PointedToReference^IENDepth^[V]
        ;"       As pointers are resolved, the entries will be KILLED from the above global
        ;"Results: none
        ;"
        SET FILENUM=$GET(FILENUM) QUIT:(+FILENUM'>0)
        IF $$ISSUBFIL^TMGFMUT2(FILENUM),FILENUM'["{" DO
        . SET FILENUM=$$GETSPFN^TMGFMUT2(FILENUM)  ;"convert 123.02 --> '123.02{123'
        SET RPTR=$GET(RPTR)
        SET LPTR=$GET(LPTR)
        SET INOUT=$GET(INOUT) IF INOUT'="PTIN" SET INOUT="PTOUT"
        IF INOUT="PTIN" GOTO UN2
        NEW NODE SET NODE=""
        FOR  SET NODE=$ORDER(^TMG("TMGSIPH","NEEDED RECORDS","PTOUT",FILENUM,RPTR,NODE)) QUIT:(NODE="")  DO
        . NEW INFO SET INFO=""
        . FOR  SET INFO=$ORDER(^TMG("TMGSIPH","NEEDED RECORDS","PTOUT",FILENUM,RPTR,NODE,INFO)) QUIT:(INFO="")  DO
        . . NEW PCE SET PCE=+INFO
        . . NEW P2FILE SET P2FILE=$PIECE(INFO,"^",2)
        . . IF LPTR'=RPTR DO
        . . . IF $PIECE(INFO,"^",5)="V" SET LPTR=LPTR_";"_$PIECE(INFO,"^",3) ;"VPTR stored as 'IEN;OREF'
        . . . SET $PIECE(@NODE,"^",PCE)=LPTR
        . . IF 0=1 DO  ;"Build up map array to store history of connections.  DON'T USE.....
        . . . IF P2FILE=2 DO  ;"2=PATIENT file.
        . . . . SET ^TMG("TMGSIPH","MAP IN","F"_2,"F"_FILENUM,LPTR)=""
        . . . . SET ^TMG("TMGSIPH","MAP IN","XREF",FILENUM)=$NAME(^TMG("TMGSIPH","MAP IN","F"_2,"F"_FILENUM))
        . . . IF $DATA(^TMG("TMGSIPH","MAP IN","XREF","F"_P2FILE)) DO
        . . . . NEW REF SET REF=$GET(^TMG("TMGSIPH","MAP IN","XREF","F"_P2FILE))
        . . . . QUIT:(REF="")!($QLENGTH(REF)>15)
        . . . . SET @REF@("F"_FILENUM,LPTR)=""
        . . . . SET ^TMG("TMGSIPH","MAP IN","XREF","F"_FILENUM)=$NAME(@REF@("F"_FILENUM))
        . . KILL ^TMG("TMGSIPH","NEEDED RECORDS","PTOUT",FILENUM,RPTR,NODE,INFO)
        . . SET TALLY("UNNEEDED RECORDS")=+$GET(TALLY("UNNEEDED RECORDS"))+1
UN2     KILL ^TMG("TMGSIPH","NEEDED RECORDS",INOUT,FILENUM,RPTR)  ;"TEMP
        ;
        QUIT
 ;
 ;
ISDIFF(ARRAY) ;
        ;"Purpose:to determine IF record stored in ARRAY is different from that stored in local ^Global
        ;"Input: ARRAY -- Pass by REFERENCE.  This is actual remote record from server. Format:
        ;"         ARRAY(1)=ARef_"="
        ;"         ARRAY(2)="="_AValue
        ;"         ARRAY(3)=ARef_"="
        ;"         ARRAY(4)="="_AValue
        ;"Result: 0 -- no difference
        ;"        1 -- ARRAY has extra information
        ;"        2 -- ARRAY has conflicting information
        ;
        NEW RESULT SET RESULT=0
        NEW TMGI SET TMGI=0
        NEW STIME SET STIME=$H
        NEW SHOWPROG SET SHOWPROG=0
        NEW TMGMAX,TMGMIN
        NEW TMGCT SET TMGCT=0
        NEW REF,VALUE
        FOR  SET TMGI=$ORDER(ARRAY(TMGI)) QUIT:(TMGI="")!(RESULT=2)  DO
        . IF (SHOWPROG=0),($$HDIFF^XLFDT($H,STIME,2)>15) DO  ;"Turn on progress bar after 15 seconds.
        . . SET SHOWPROG=1
        . . SET TMGMIN=$ORDER(ARRAY(0))
        . . SET TMGMAX=$ORDER(ARRAY(""),-1)
        . IF (SHOWPROG=1),(TMGCT>500) DO
        . . DO PROGBAR^TMGUSRI2(TMGI,"Comparing server data to local ",TMGMIN,TMGMAX,70,STIME)
        . . SET TMGCT=0
        . SET REF=$GET(ARRAY(TMGI))
        . SET REF=$EXTRACT(REF,1,$LENGTH(REF)-1)
        . SET TMGI=TMGI+1
        . SET TMGCT=TMGCT+1
        . IF REF="" SET RESULT=2 QUIT
        . SET VALUE=$EXTRACT($GET(ARRAY(TMGI)),2,10000)
        . IF $DATA(@REF)=0 SET RESULT=1 ;"ARRAY has extra info
        . IF $GET(@REF)=VALUE QUIT
        . SET RESULT=2 ;"ARRAY conflicts with local value.
        QUIT RESULT
 ;
 ;
GETFLD(FILENUM,LOC,PCE)
        ;"Purpose: Return field number cooresponding to File number, node, and piece.
        ;"Input: FILENUM -- Fileman file number to work with.
        ;"       LOC -- the subscript location
        ;"       PCE -- the piece for the field in question
        ;"Results: field number^field name, or 0 IF not found
        NEW RESULT SET RESULT=0
        NEW FOUND SET FOUND=0
        NEW FLD SET FLD=0
        FOR  SET FLD=$ORDER(^DD(FILENUM,FLD)) QUIT:(+FLD'>0)!(FOUND=1)  DO
        . NEW INFO SET INFO=$PIECE($GET(^DD(FILENUM,FLD,0)),"^",4)
        . IF $PIECE(INFO,";",1)'=LOC QUIT
        . IF $PIECE(INFO,";",2)'=PCE QUIT
        . SET FOUND=1
        . SET RESULT=FLD_"^"_$PIECE($GET(^DD(FILENUM,FLD,0)),"^",1)
        QUIT RESULT
 ;
 ;
RECSHOW(FILENUM,RPTR,ARRAY) ;
        ;"Purpose: to show remote and local data, to allow user to see differences
        ;"Input: FILENUM -- Fileman file (or subfile) number to work with.
        ;"       RPTR -- The record number (IEN) on the server of the record downloaded.
        ;"               If FILENUM is a subfile, then pass RPTR in standard IENS format (e.g. '4,6787,')
        ;"       ARRAY -- Pass by REFERENCE.  This is actual remote record from server.
        ;"          Format as per OVERWRITE
        ;"
        WRITE "NOTE: ONLY DIFFERENCE WILL BE SHOWN",!,!
        WRITE "LEGEND: REFERENCE",!
        WRITE "  L -- Local data value",!
        WRITE "  R -- Remote data value",!!
        NEW LINECT SET LINECT=6
        NEW TMGI SET TMGI=0
        SET IOSL=$GET(IOSL,24)
        ;"NEW GREF SET GREF=$GET(^DIC(FILENUM,0,"GL")) QUIT:(GREF="")
        NEW GREF SET GREF=$$GETGREF^TMGFMUT2(FILENUM,RPTR) QUIT:(GREF="")
        NEW SL SET SL=$QLENGTH($$CREF^DILF(GREF))
        NEW REF,VALUE,LVALUE
        NEW DONE SET DONE=0
        FOR  SET TMGI=$ORDER(ARRAY(TMGI)) QUIT:(TMGI="")!(DONE=1)  DO
        . SET REF=$GET(ARRAY(TMGI))
        . SET REF=$EXTRACT(REF,1,$LENGTH(REF)-1)
        . SET TMGI=TMGI+1
        . IF REF="" SET DONE=1 QUIT
        . SET VALUE=$EXTRACT($GET(ARRAY(TMGI)),2,10000)
        . SET LVALUE=$GET(@REF)
        . IF LVALUE=VALUE QUIT
        . ;"Later, I will format raw nodes into readable fileman fields and values...
        . IF $QLENGTH(REF)=(SL+2) DO
        . . NEW LOC SET LOC=$QSUBSCRIPT(REF,SL+2)
        . . NEW PCE,FLD
        . . FOR PCE=1:1:$LENGTH(VALUE,"^") DO
        . . . NEW V1,LV1,EV1,ELV1,INFO
        . . . SET (EV1,V1)=$PIECE(VALUE,"^",PCE)
        . . . SET (ELV1,LV1)=$PIECE(LVALUE,"^",PCE)
        . . . IF V1=LV1 QUIT
        . . . SET FLD=$$GETFLD(FILENUM,LOC,PCE)
        . . . IF +FLD=0 WRITE "?? FIELD",! QUIT
        . . . IF $DATA(^DD(FILENUM,+FLD,2))#10=1 DO
        . . . . NEW XFRM SET XFRM=$GET(^DD(FILENUM,+FLD,2))
        . . . . IF XFRM="" QUIT
        . . . . NEW Y
        . . . . SET Y=V1 XECUTE XFRM SET EV1=Y
        . . . . SET Y=LV1 XECUTE XFRM SET ELV1=Y
        . . . WRITE "Field -- ",$PIECE(FLD,"^",2)," (",+FLD,"):",!
        . . . WRITE " L = ",ELV1,!
        . . . WRITE " R = ",EV1,!
        . . . SET LINECT=LINECT+3
        . . . IF LINECT>(IOSL-5) DO
        . . . . DO PRESS2GO^TMGUSRI2
        . . . . SET LINECT=0
        . ELSE  DO
        . . WRITE REF,!
        . . WRITE " L = ",$GET(@REF),!
        . . WRITE " R = ",VALUE,!
        . . SET LINECT=LINECT+3
        . . IF LINECT>(IOSL-5) DO
        . . . DO PRESS2GO^TMGUSRI2
        . . . SET LINECT=0
        ;
        IF LINECT>0 DO PRESS2GO^TMGUSRI2
        QUIT
 ;
 ;
GET01FIELD(FILENUM,ARRAY,RVALUE,LVALUE,IENS) ;
        ;"Purpose: Extract .01 field name from data array
        ;"Input: FILENUM -- Fileman file (of subfile) number to work with.
        ;"       ARRAY -- Pass by REFERENCE.  This is actual remote record from server.
        ;"          Format as per OVERWRITE
        ;"       RVALUE -- Pass by REFERENCE. An OUT PARAMETER.  Filled with .01 field from server
        ;"       LVALUE -- Pass by REFERENCE. An OUT PARAMETER   Filled with .01 field from local database
        ;"       IENS -- OPTIONAL.  Only needed IF FILENUM is a subfile.
        ;"Results: none
        ;"Output: RVALUE and LVALUE are filled with the INTERNAL values of the .01 field, or "" IF null
        ;"
        SET (RVALUE,LVALUE)=""
        ;"NEW GREF SET GREF=$GET(^DIC(FILENUM,0,"GL")) QUIT:(GREF="")
        NEW GREF SET GREF=$$GETGREF^TMGFMUT2(FILENUM,.IENS) QUIT:(GREF="")
        NEW SL SET SL=$QLENGTH($$CREF^DILF(GREF))
        NEW REF,RNODE,LNODE
        NEW DONE SET DONE=0
        NEW TMGI SET TMGI=0
        FOR  SET TMGI=$ORDER(ARRAY(TMGI)) QUIT:(TMGI="")!(DONE=1)  DO
        . SET REF=$GET(ARRAY(TMGI))
        . SET REF=$EXTRACT(REF,1,$LENGTH(REF)-1)
        . SET TMGI=TMGI+1
        . IF REF="" SET DONE=1 QUIT
        . SET RNODE=$EXTRACT($GET(ARRAY(TMGI)),2,10000)
        . SET LNODE=$GET(@REF)
        . ;"Later, I will format raw nodes into readable fileman fields and values...
        . IF $QLENGTH(REF)=(SL+2) DO
        . . NEW LOC SET LOC=$QSUBSCRIPT(REF,SL+2)
        . . IF LOC'=0 QUIT
        . . SET RVALUE=$PIECE(RNODE,"^",1)
        . . SET LVALUE=$PIECE(LNODE,"^",1)
        . . SET DONE=1
        ;
        QUIT
 ;
 ;
GETTARGETIEN(FILENUM,ARRAY,TARGETIEN)  ;
        ;"Purpose: To determine IF a local record should be overwritten with record from server.
        ;"         Ask user directly IF not able to automically determine.
        ;"Input: FILENUM -- Fileman file (or subfile) number to work with.
        ;"       ARRAY -- Pass by REFERENCE.  This is actual remote record from server. Format:
        ;"               ARRAY(1)=ARef_"="
        ;"               ARRAY(2)="="_AValue
        ;"               ARRAY(3)=ARef_"="
        ;"               ARRAY(4)="="_AValue
        ;"               NOTE: IEN of array doesn't match input TARGETIEN, then IEN of array will be changed to it.
        ;"       TARGETIEN -- Required.  PASS BY REFERENCE.  an IN & OUT PARAMETER.
        ;"               If FILENUM is a subfile, then pass TARGETIEN in standard IENS format.
        ;"               INPUT:  The initially planned location for storing the array
        ;"               OUTPUT:  This is the pointer of where the record should be stored locally
        ;"Result: "OVERWRITE" = OVERWRITE record currently stored at TARGETIEN
        ;"        "ABORT" = User abort or error occurred.
        ;"        "USELOCAL" = Dump server data, and just use record already at TARGETIEN
        ;"TARGETIEN pointer may be changed to NEW target record location.
        NEW Y,NARRAY,%
        NEW R01VALUE,L01VALUE
        NEW RESULT SET RESULT="OVERWRITE" ;"default to overwriting
        SET TARGETIEN=$GET(TARGETIEN)
        IF +TARGETIEN'>0 DO  GOTO OVWDN
        . SET RESULT="ABORT"
        SET FILENUM=+$GET(FILENUM)
        NEW RPTR SET RPTR=+$$IENOFARRAY(FILENUM,.ARRAY,TARGETIEN)
        IF TARGETIEN["," DO  ;"i.e. is an IENS
        . NEW TEMP SET TEMP=TARGETIEN
        . SET $PIECE(TEMP,",",1)=RPTR
        . SET RPTR=TEMP    ;"convert RPTR into an IENS
        IF +RPTR'>0 DO  GOTO OVWDN
        . SET RESULT="ABORT"
        IF $GET(^TMG("TMGSIPH",".01 VALUE",FILENUM,RPTR))="" DO
        . DO GET01FIELD(FILENUM,.ARRAY,.R01VALUE,,RPTR) ;"Extract .01 field name from data array, before relocated
        . SET ^TMG("TMGSIPH",".01 VALUE",FILENUM,RPTR)=R01VALUE ;"Needed elsewhere for faster processing of future records.
        IF TARGETIEN'=RPTR DO  GOTO:(RESULT="ABORT") OVWDN
        . NEW TEMP SET TEMP=$$RLOCARRAY(FILENUM,TARGETIEN,.ARRAY,.NARRAY)  ;"Relocate array (change IEN)
        . IF TEMP=-1 SET RESULT="ABORT" QUIT
        . KILL ARRAY
        . MERGE ARRAY=NARRAY
        NEW DIFF SET DIFF=$$ISDIFF(.ARRAY)  ;" 0=no diff, 1=ARRAY has extra info, 2=ARRAY has conflicting info
        IF DIFF=0 SET RESULT="USELOCAL" GOTO OVWDN
        IF DIFF=1 SET RESULT="OVERWRITE" GOTO OVWDN
        ;
        DO GET01FIELD(FILENUM,.ARRAY,.R01VALUE,.L01VALUE,RPTR) ;
        IF R01VALUE'=L01VALUE DO  GOTO OVWDN ;"If .01 values are different, so move TARGETIEN to NEW location
        . SET TARGETIEN=$$APPENDIEN(FILENUM,RPTR)  ;"RPTR not used unless dealing with subfile.
        . SET RESULT=$SELECT((TARGETIEN>0):"OVERWRITE",1:"ABORT")
        ;
        IF $GET(^DD(FILENUM,.01,0))["DINUM" SET RESULT="OVERWRITE" GOTO OVWDN ;"translation of pointer not allowed
        NEW MENU,USRSLCT
        SET USRSLCT=$GET(^TMG("TMGSIPH","CONFLICT HANDL",FILENUM))
        IF USRSLCT'="" GOTO OW3
        ;
OW2     WRITE #
        NEW FNAME SET FNAME=$$FILENAME^TMGFMUT2(FILENUM)
        KILL MENU
        SET MENU(0)="<<!!CONFLICT FOUND!!>> OVERWRITE LOCAL DATA IN FILE ["_FNAME_"] ?"
        SET MENU(1)="VIEW local and remote raw data"_$CHAR(9)_"View"
        SET MENU(2)="OVERWRITE local data."_$CHAR(9)_"Overwrite1"
        SET MENU(3)="Store record in NEW location."_$CHAR(9)_"ChangeIEN"
        SET MENU(4)="Use LOCAL data, not remote data from server."_$CHAR(9)_"UseLocal"
        SET MENU(5)="FIND a local record to use instead."_$CHAR(9)_"FindLocal"
        SET MENU(6)="Abort"_$CHAR(9)_"Abort"
        ;
        WRITE "File = ",FNAME,"; Record .01 field = "_R01VALUE,!
        SET USRSLCT=$$MENU^TMGUSRI2(.MENU,"")
        IF USRSLCT="^" SET USRSLCT="Abort"
        IF USRSLCT=0 SET USRSLCT=""
        IF USRSLCT="FindLocal" DO  GOTO:(+Y>0) OVWDN
        . NEW X,DIC
        . IF $$ISSUBFIL^TMGFMUT2(FILENUM) DO
        . . SET DIC=$$GETGREF^TMGFMUT2(FILENUM,TARGETIEN)
        . ELSE  SET DIC=FILENUM
        . SET DIC(0)="MAEQ"
        . DO ^DIC WRITE !
        . IF +Y'>0 QUIT
        . SET RESULT="OVERWRITE"
        . SET $PIECE(TARGETIEN,",",1)=+Y
        IF USRSLCT="Abort" SET RESULT="ABORT" GOTO OVWDN
        IF USRSLCT="View" DO RECSHOW(FILENUM,RPTR,.ARRAY) GOTO OW2
        SET %=2
        WRITE "ALWAYS DO this for file ["_FNAME_"]"
        DO YN^DICN WRITE !
        IF %=-1 SET RESULT="ABORT" GOTO OVWDN
        IF %=2 SET ^TMG("TMGSIPH","CONFLICT HANDL",FILENUM)=""
        ELSE  SET ^TMG("TMGSIPH","CONFLICT HANDL",FILENUM)=USRSLCT
OW3     IF USRSLCT="Overwrite1" DO  GOTO OVWDN
        . SET RESULT="OVERWRITE"
        IF USRSLCT="ChangeIEN" DO  GOTO OVWDN
        . SET TARGETIEN=$$APPENDIEN(FILENUM,RPTR)  ;"RPTR not used unless dealing with subfile.
        . SET RESULT=$SELECT((TARGETIEN>0):"OVERWRITE",1:"ABORT")
        IF USRSLCT="UseLocal" DO  GOTO OVWDN
        . SET RESULT="USELOCAL"
        GOTO OW2
        ;
OVWDN   QUIT RESULT
 ;