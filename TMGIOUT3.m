TMGIOUT3 ;TMG/kst/IO Utilities ;11/23/14; 12/27/14
         ;;1.0;TMG-LIB;**1**;2/2/14
 ;
 ;"TMG IO UTILITIES
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"NOTE: THIS CODE IS SACC COMPLIANT.  Keep it that way.  :-)
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"WP2HFS(GLOBALP,PATH,FILENAME) -- WRITE a WP field to a Host-File-System file
 ;"WP2HFSFP(GLOBALP,PATHFILENAME) --provide an interface to WP2HFS for cases when filename is not already separated from path
 ;"HFS2WP(PATH,FILENAME,GLOBALP) -- read a WP field from a Host-File-System file
 ;"HFS2WPFP(PATHFILENAME,GLOBALP) -- provide an interface to HFS2WP for cases when filename is not already separated from path
 ;"ARR2HFS(REF,PATH,FILENAME) -- WRITE an array to a Host-File-System file text file
 ;"AR2HFSFP(REF,PATHFILENAME) -- provide an interface to ARR2HFS for cases when filename is not already separated from path
 ;"HFS2ARR(PATH,FILENAME,REF,OPTION)  -- read a text array from a Host-File-System file
 ;"HFS2ARFP(PATHFILENAME,REF,OPTION) -- provide an interface to HFS2ARR for cases when filename is not already separated from path
 ;"=======================================================================
 ;"Dependancies
 ;"%ZISH
 ;"=======================================================================
 ;"=======================================================================
 ;
WP2HFS(GLOBALP,PATH,FILENAME) ;
        ;"Purpose: To WRITE a WP field to a Host-File-System file
        ;"Input: GLOBALP -- The reference to the header node (e.g.  ^TMG(22702,99,1) in example below)
        ;"         PATH: for the output file, the path up to, but not including, the filename
        ;"         FILENAME -- the filename to save to in the host file system. If file already exists, it will be overwritten.
        ;"Note:  The format of a WP field is as follows:
        ;"      e.g.    ^TMG(22702,99,1,0) = ^^4^4^3050118^
        ;"               ^TMG(22702,99,1,1,0) = Here is the first line of text
        ;"               ^TMG(22702,99,1,2,0) = And here is another line
        ;"               ^TMG(22702,99,1,3,0) =
        ;"               ^TMG(22702,99,1,4,0) = And here is a final line
        ;"  And the format of the 0 node is: ^^<line count>^<linecount>^<fmdate>^^
        ;"Result: 0 IF failure, 1 IF success
        ;"Assumptions: That GLOBALP is a valid reference to a WP field
        ;
        NEW RESULT SET RESULT=0 ;"default to failure
        IF $DATA(GLOBALP)&($DATA(PATH))&($DATA(FILENAME)) DO
        . NEW TMGWP
        . MERGE TMGWP=@GLOBALP
        . SET RESULT=$$GTF^%ZISH("TMGWP(1,0)",1,PATH,FILENAME)
        QUIT RESULT
        ;
WP2HFSFP(GLOBALP,PATHFILENAME) ;
        ;"Purpose: To provide an interface to WP2HFS for cases when filename is not already separated from path
        ;"Result: 0 IF failure, 1 IF success
        NEW PATH,FILENAME,RESULT
        DO SPLITFPN^TMGIOUTL(.PATHFILENAME,.PATH,.FILENAME)
        SET RESULT=$$WP2HFS(.GLOBALP,.PATH,.FILENAME)
        QUIT RESULT
        ;
HFS2WP(PATH,FILENAME,GLOBALP) ;
        ;"Purpose: To read a WP field from a Host-File-System file
        ;"Input: PATH: for the source  file, the path up to, but not including, the filename
        ;"       FILENAME -- the filename to load from the host file system. 
        ;"       GLOBALP -- The reference to the header node (e.g.  ^TMG(22702,99,1) in example below)
        ;"Note:  The format of a WP field is as follows:
        ;"       e.g.    ^TMG(22702,99,1,0) = ^^4^4^3050118^
        ;"               ^TMG(22702,99,1,1,0) = Here is the first line of text
        ;"               ^TMG(22702,99,1,2,0) = And here is another line
        ;"               ^TMG(22702,99,1,3,0) =
        ;"               ^TMG(22702,99,1,4,0) = And here is a final line
        ;"  And the format of the 0 node is: ^^<line count>^<linecount>^<fmdate>^^
        ;"Result: 0 IF failure, 1 IF success
        ;"Assumptions: That GLOBALP is a valid reference to a WP field
        NEW RESULT SET RESULT=0 ;"default to failure
        IF $DATA(GLOBALP)&($DATA(PATH))&($DATA(FILENAME)) DO
        . NEW TMGWP,WP
        . SET RESULT=$$FTG^%ZISH(PATH,FILENAME,"TMGWP(1,0)",1)
        . IF RESULT=0 QUIT
        . ;"Scan for overflow nodes, and integrate into main body
        . NEW IDX SET IDX=$ORDER(TMGWP(""))
        . IF IDX'="" FOR  DO  QUIT:(IDX="")
        . . IF $DATA(TMGWP(IDX,"OVF")) DO
        . . . NEW JDX SET JDX=$ORDER(TMGWP(IDX,"OVF",""))
        . . . IF JDX'="" FOR  DO  QUIT:(JDX="")
        . . . . NEW NUM SET NUM=IDX+(JDX/1000)
        . . . . SET TMGWP(NUM,0)=TMGWP(IDX,"OVF",JDX)
        . . . . SET JDX=$ORDER(TMGWP(IDX,"OVF",JDX))
        . . . KILL TMGWP(IDX,"OVF")
        . . SET IDX=$ORDER(TMGWP(IDX))
        . ;"Now copy into another variable, renumbering lines (in case there were overflow lines)
        . SET IDX=$ORDER(TMGWP(""))
        . SET JDX=0
        . IF IDX'="" FOR  DO  QUIT:(IDX="")
        . . SET JDX=JDX+1
        . . SET WP(JDX,0)=TMGWP(IDX,0)
        . . SET IDX=$ORDER(TMGWP(IDX))
        . ;"now create a header node
        . DO NOW^%DTC  ;"returns RESULT in X
        . SET WP(0)="^^"_JDX_"^"_JDX_"^"_X_"^^"
        . ;"now put WP into global reference.
        . KILL @GLOBALP
        . MERGE @GLOBALP=WP
        QUIT RESULT
        ;
HFS2WPFP(PATHFILENAME,GLOBALP) ;
        ;"Purpose: To provide an interface to HFS2WP for cases when filename is not already separated from path
        ;"Result: 0 IF failure, 1 IF success
        NEW PATH,FILENAME,RESULT
        DO SPLITFPN^TMGIOUTL(.PATHFILENAME,.PATH,.FILENAME)
        SET RESULT=$$HFS2WP(.PATH,.FILENAME,.GLOBALP)
        QUIT RESULT
        ;
ARR2HFS(REF,PATH,FILENAME) ;"Array to HFS
        ;"Purpose: To WRITE an array to a Host-File-System file text file
        ;"Input: REF -- The reference to source array
        ;"       PATH: for the output file, the path up to, but not including, the filename
        ;"       FILENAME -- the filename to save to in the host file system. If file already exists, it will be overwritten.
        ;"Note:  The format of a array is as follows:
        ;"      e.g.  @REF@(#)=<line of text>
        ;"Note2: Due to limitations in GTF^%ZISH, the index numbers MUST be
        ;"     sequential integers, e.g. 1,2,3,4.   1,3,5,7 would NOT work
        ;"Result: 0 IF failure, 1 IF success
        NEW RESULT SET RESULT=0 ;"default to failure
        NEW IDX SET IDX=$ORDER(@REF@(""))
        NEW REF2 SET REF2=$NAME(@REF@(IDX))
        SET RESULT=$$GTF^%ZISH(REF2,1,PATH,FILENAME)
        QUIT RESULT
        ;
AR2HFSFP(REF,PATHFILENAME) ; "Array to HFS via FilePath
        ;"Purpose: To provide an interface to ARR2HFS for cases when filename is not already separated from path
        ;"Result: 0 IF failure, 1 IF success
        NEW PATH,FILENAME,RESULT
        DO SPLITFPN^TMGIOUTL(.PATHFILENAME,.PATH,.FILENAME)
        SET RESULT=$$ARR2HFS(.REF,.PATH,.FILENAME)
        QUIT RESULT
        ;
HFS2ARR(PATH,FILENAME,REF,OPTION)  ;
        ;"Purpose: To read a text array from a Host-File-System file
        ;"Input: PATH: for the source file, the path up to, but not including, the filename
        ;"       FILENAME -- the filename of the source host file system. 
        ;"       REF -- The reference to the array to be filled with RESULTs.  Format:
        ;"              @REF@(#)=<line of text>"
        ;"       OPTION -- [OPTIONAL].  PASS BY REFERENCE
        ;"            OPTION("LINE-TERM")=<line_terminator_string>  e.g. $CHAR(13).  If passed normal parsing done.
        ;"            OPTION("OVERFLOW")=<mode>.  Mode options:
        ;"                        If 0, null or "" then overflow lines are turned into normal lines
        ;"                        If 1 then the overflow portion is concat'd to the orig line (making length>255)
        ;"                        If 2 then over lines are left with index values with decimal (e.g. 123.1)
        ;"NOTE: This will support loading lines of length 255*1000000 = ~255 MB
        ;"Result: 0 if failure, 1 if success      
        NEW TMGRESULT,%ZZWP
        SET TMGRESULT=$$FTG^%ZISH(PATH,FILENAME,"%ZZWP(1,0)",1)
        IF TMGRESULT=0 GOTO H2ARDN
        NEW LINETERM SET LINETERM=$GET(OPTION("LINE-TERM"))
        ;"Scan for overflow nodes, and integrate into main body
        IF LINETERM'="" GOTO H2AR2  ;"different processing.
        NEW IDX SET IDX=""
        FOR  SET IDX=$ORDER(%ZZWP(IDX)) QUIT:(IDX="")  DO
        . IF $DATA(%ZZWP(IDX,"OVF")) DO
        . . NEW JDX SET JDX=""
        . . FOR  SET JDX=$ORDER(%ZZWP(IDX,"OVF",JDX)) QUIT:(JDX="")  DO
        . . . SET %ZZWP(IDX+(JDX/1000000),0)=%ZZWP(IDX,"OVF",JDX)
        . . KILL %ZZWP(IDX,"OVF")
        NEW OVERFLOWMODE SET OVERFLOWMODE=+$GET(OPTION("OVERFLOW"))
        ;"Now copy into destination variable, renumbering lines, and handling overflow lines
        SET (IDX,JDX)=0
        FOR  SET IDX=$ORDER(%ZZWP(IDX)) QUIT:(IDX="")  DO
        . IF (OVERFLOWMODE'=0)&(IDX\1'=IDX) DO  QUIT
        . . IF OVERFLOWMODE=1 DO  QUIT
        . . . NEW PRIORIDX SET PRIORIDX=$ORDER(@REF@(""),-1)
        . . . NEW PRIORSTR SET PRIORSTR=$GET(@REF@(PRIORIDX))
        . . . NEW THISSTR SET THISSTR=$GET(%ZZWP(IDX,0)) 
        . . . SET @REF@(PRIORIDX)=PRIORSTR_THISSTR
        . . IF OVERFLOWMODE=2 DO  QUIT
        . . . NEW KDX SET KDX=JDX_"."_$PIECE(IDX,".",2)
        . . . SET @REF@(KDX)=%ZZWP(IDX,0)
        . ELSE  SET JDX=JDX+1,@REF@(JDX)=%ZZWP(IDX,0)
        . KILL %ZZWP(IDX,0)
        GOTO H2ARDN
H2AR2   ;" handle dividing up by custom line terminator character(s)  Added 8/30/13
        NEW TEMPREF SET TEMPREF="%ZZWP(0)"
        NEW TEMPARR,IDX SET IDX=0
        NEW RESIDUAL SET RESIDUAL=""
        FOR  SET TEMPREF=$QUERY(@TEMPREF) QUIT:(TEMPREF="")  DO
        . NEW S SET S=$GET(@TEMPREF) QUIT:S=""
        . SET S=RESIDUAL_S
        . SET RESIDUAL=""
        . NEW NUMPARTS SET NUMPARTS=$LENGTH(S,LINETERM)
        . NEW PNUM FOR PNUM=1:1:NUMPARTS DO
        . . NEW PART SET PART=$PIECE(S,LINETERM,PNUM)
        . . IF PNUM=NUMPARTS SET RESIDUAL=PART QUIT
        . . SET IDX=IDX+1,TEMPARR(IDX)=PART
        IF RESIDUAL'="" SET IDX=IDX+1,TEMPARR(IDX)=RESIDUAL
        MERGE @REF=TEMPARR
H2ARDN  QUIT TMGRESULT
        ;
HFS2ARFP(PATHFILENAME,REF,OPTION) ;" HFS file to Array, via FilePathName
        ;"Purpose: To provide an interface to HFS2ARR for cases when filename is not already separated from path
        ;"Input: PATHFILENAME: for the source file, the path+filename
        ;"       REF -- The reference to the array to be filled with RESULTs.  Format:
        ;"              @REF@(#)=<line of text>"
        ;"       OPTION -- [OPTIONAL].  PASS BY REFERENCE
        ;"            OPTION("LINE-TERM")=<line_terminator_string>  e.g. $CHAR(13).  If passed normal parsing done.
        ;"            OPTION("OVERFLOW")=<mode>.  Mode options:
        ;"                        If 0, null or "" then overflow lines are turned into normal lines
        ;"                        If 1 then the overflow portion is concat'd to the orig line (making length>255)
        ;"                        If 2 then over lines are left with index values with decimal (e.g. 123.1)
        ;"Result: 0 if failure, 1 if success
        NEW PATH,FILENAME,RESULT
        DO SPLITFPN^TMGIOUTL(.PATHFILENAME,.PATH,.FILENAME)
        SET RESULT=$$HFS2ARR(.PATH,.FILENAME,.REF,.OPTION)
        QUIT RESULT
        ;
