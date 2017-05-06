TMGTIUO2 ;TMG/TIU Text Object Expansion Fns;04/15/10, 2/2/14
         ;;1.0;TMG-LIB;**1,17**;04/15/10
 ;
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
 ;"PUBLIC FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"PRIVATE FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;
GETPTFLD(DFN,PARAM) ;
        ;"Purpose: This is the server-side code for the TIU TEXT OBJECT, which
        ;"      will allow the user to retrieve a field from the PATIENT file.
        ;"NOTE: This requires that patch TMG-CPRS-TEXTOBJ-PARAM*1.0*1 or later
        ;"      be installed, to allow passing in of parameters from the CPRS client.
        ;"Input: DFN -- This should be the IEN of the currently open patient
        ;"       Param -- Field(s)^Flags^FormatString.  Details below
        ;"            Field(s) -- required.  Options for input:
        ;"                -  A single field number or name
        ;"                -  A list of field numbers (or names), separated by semicolons
        ;"                -  A range of field numbers (or names), in the form M:N,
        ;"                         where M and N are the end points of the inclusive range.
        ;"                         All field numbers within this range are retrieved.
        ;"                -  A '*' for all fields at the top level (no sub-multiple record).
        ;"                -  A '**' for all fields including all fields and data in sub-multiple fields.
        ;"                -  Field number (or name) of a multiple followed by an * to indicate all
        ;"                         fields and records in the sub-multiple for that field.
        ;"                Invalid field names will be ignored
        ;"            Flags -- Optional.
        ;"                -  'F' -- include field name in results with value.  e.g. "AGE: 43" instead of just "43"
        ;"                -         This flag is ignored IF a FormatString is provided (see below)
        ;"                -  'S' -- Keep all data values on a single line, separated by ';'.
        ;"                -         If flag not provided, and multiple data fields are requested,
        ;"                -         then the default is that each data value will be separated by a
        ;"                -         CRLF [$C(13)_$C(10)]
        ;"                -         This flag is ignored IF a FormatString is provided (see below)
        ;"                -  'R' -- Resolve fields to NAMES, even IF a field NUMBER was used for input request
        ;"                -         Note: this will affect the sorting order of the output (see FormatString
        ;"                -         info below).  I.e. IF R not specified, and field NUMBERS are used for input,
        ;"                -         then results will be returned in numerical field number order by default.
        ;"                -         If R is specified, then field numbers are converted to field NAMES, and that
        ;"                -         is used to determine the order of output.
        ;"                -  'N' -- Don't return values for empty fields.  This is helpful IF ALL fields
        ;"                -         were requested via '*'
        ;"            FormatString -- A string to determine how results are passed back....
        ;"                NOTE: without a format string, results will be passed back in the order returned
        ;"                      by fileman.  I.e. IF user requested fields "SEX;.01;AGE", then Fileman will
        ;"                      place results into an array, which MUMPS will sort alphabetically, e.g.
        ;"                      .01, then AGE, then SEX.  If "*" fields are requested, it would be even
        ;"                      more complex.  A format string will allow the user to specify ORDER.
        ;"                Format: e.g. "Any arbitrary text %FieldNameOrNum% more text %FieldNameOrNum% ..."
        ;"                  (The goal was to follow the method used by printf in the c language.)
        ;"                  -  Any arbitrary text can be included.
        ;"                  -  Field numbers or names should be enclosed by the '%' character
        ;"                       These will be replaced with actual data values.
        ;"                  - '\n' can be included to specify line breaks
        ;"                  - '%%' will be used to show a '%' in the output text
        ;"                  - Invalid, or non-matching, field names/numbers will be ignored.
        ;"
        ;"Results: returns a string that will be sent back to CPRS, to be included in a text note
        ;"NOTE: I have chosen to make this function work with only file 2 (PATIENT FILE).  I think
        ;"      it could be a security violation IF any CPRS user was able to look at any arbitrary file.
        ;"
        ;"Examples of PARAM inputs:
        ;"    '.01'     -- returns .01 field, which is the patients NAME, e.g. "SMITH,JOHN A"
        ;"    'NAME'    -- returns same value as above, e.g. "SMITH,JOHN A"
        ;"    'NAME^F'  -- e.g result "NAME: SMITH,JOHN A"
        ;"    'NAME;SEX;AGE^F' --> "AGE: 34"_$C(13)_$C(10)_"NAME: SMITH,JOHN A"_$C(13)_$C(10)_"SEX: MALE"
        ;"    'NAME;SEX;AGE^S' --> "34; SMITH,JOHN A; MALE"
        ;"    'NAME;SEX;AGE^^"NAME: %NAME%, %AGE% yrs., %SEX%"' --> "NAME: SMITH,JOHN A, 34 YRS., MALE"
        ;"
        NEW TMGFLDS,TMGFLAGS
        NEW TMGFILE SET TMGFILE=2
        NEW RESULT SET RESULT=""
        SET PARAM=$GET(PARAM)
        SET TMGFLDS=$PIECE(PARAM,"^",1)
        IF TMGFLDS="" DO  GOTO GPDN
        . SET RESULT="ERROR: No input parameter.  Example of use: |TMG PATIENT FLD{AGE}|"
        SET DFN=$GET(DFN)
        IF +DFN'>0 DO  GOTO GPDN
        . SET RESULT="ERROR: Internal patient value DFN not defined.  Contact IRM"
        NEW TMGIENS SET TMGIENS=DFN_","
        SET TMGFLAGS=""
        NEW TMGTEMP SET TMGTEMP=$$UP^XLFSTR($PIECE(PARAM,"^",2))
        IF TMGTEMP["N" SET TMGFLAGS=TMGFLAGS_"N"
        IF TMGTEMP["F" SET TMGFLAGS=TMGFLAGS_"R"
        NEW TMGFORMAT SET TMGFORMAT=$PIECE(PARAM,"^",3)
        NEW TMGOUT,TMGMSG
        DO GETS^DIQ(TMGFILE,TMGIENS,TMGFLDS,TMGFLAGS,"TMGOUT","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO GPDN
        . SET RESULT=$$GETERRST^TMGDEBU2(.TMGMSG)
        NEW FLD,FLDNAME
        SET FLD=""
        IF TMGFORMAT="" DO
        . FOR  SET FLD=$ORDER(TMGOUT(TMGFILE,TMGIENS,FLD)) QUIT:(FLD="")  DO
        . . IF $DATA(TMGOUT(TMGFILE,TMGIENS,FLD,0)) QUIT  ;"For now, WP fields are not supported.  Could add later IF needed.
        . . NEW VALUE SET VALUE=$GET(TMGOUT(TMGFILE,TMGIENS,FLD))
        . . IF VALUE="",TMGTEMP["N" QUIT
        . . IF RESULT'="" DO
        . . . IF TMGTEMP["S" SET RESULT=RESULT_"; "
        . . . ELSE  SET RESULT=RESULT_$CHAR(13)_$CHAR(10)
        . . IF TMGTEMP["F" DO
        . . . IF FLD'=+FLD SET FLDNAME=FLD
        . . . ELSE  SET FLDNAME=$PIECE($GET(^DD(TMGFILE,FLD,0)),"^",1)
        . . . SET RESULT=RESULT_FLDNAME_": "
        . . SET RESULT=RESULT_VALUE
        ELSE  DO  ;"Handle format strings.
        . SET RESULT=TMGFORMAT
        . FOR  QUIT:(RESULT'["%")  DO
        . . NEW SUBA,SUBB
        . . SET SUBA=$PIECE(RESULT,"%",1)
        . . SET FLD=$PIECE(RESULT,"%",2)
        . . SET SUBB=$PIECE(RESULT,"%",3,999)
        . . NEW VALUE
        . . IF FLD="" SET VALUE="<@!@>"  ;"protect %%, later convert back to '%'
        . . ELSE  SET VALUE=$GET(TMGOUT(TMGFILE,TMGIENS,FLD))
        . . IF VALUE="" DO
        . . . IF FLD=+FLD DO
        . . . . SET FLD=$PIECE($GET(^DD(TMGFILE,FLD,0)),"^",1) ;"Convert # to name
        . . . ELSE  DO
        . . . . SET FLD=$ORDER(^DD(TMGFILE,"B",FLD,"")) ;"Convert name to #
        . . . IF FLD'="" SET VALUE=$GET(TMGOUT(TMGFILE,TMGIENS,FLD))
        . . . IF VALUE="" SET VALUE="??"
        . . SET RESULT=SUBA_VALUE_SUBB
        . NEW TMGALT
        . SET TMGALT("<@!@>")="%"
        . SET TMGALT("\n")=$CHAR(13)_$CHAR(10)
        . SET RESULT=$$REPLACE^XLFSTR(RESULT,.TMGALT)
        ;
GPDN    QUIT RESULT