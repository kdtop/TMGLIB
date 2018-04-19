TMGDEBU4 ;TMG/kst/Debug utilities ;10/26/14
         ;;1.0;TMG-LIB;**1**;10/26/14
 ;
 ;"TMG DEBUG UTILITIES
 ;"SACC-Compliant version 
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
 ;"DEBUGMSG(INDENT,MSG,A,B,C,D,E,F,G,H,I,J,K,L)  -- a debugging message output procedure
 ;"DBWRITE(INDENT,STR,NEWLN,IOFILE) -- WRITE debug output.  
 ;
 ;"=======================================================================
 ;"Private API functions
 ;"=======================================================================
 ;"DBINDENT(INDENT,FORCED) 
 ;
 ;"=======================================================================
 ;"DEPENDENCIES:
 ;"=======================================================================
 ;"=======================================================================
 ;
DEBUGMSG(INDENT,MSG,A,B,C,D,E,F,G,H,I,J,K,L)  ;
        ;"PUBLIC FUNCTION
        ;"Purpose: a debugging message output procedure
        ;"Input:Indent -- the value of indentation expected
        ;"      MSG -- a string or value to show as message
        ;"      A..L -- OPTIONAL.  Extra values to show.
        IF $GET(TMGDEBUG,0)=0 QUIT
        SET INDENT=+$GET(INDENT)
        SET MSG=$GET(MSG)
        SET MSG=MSG_$GET(A)_$GET(B)_$GET(C)_$GET(D)_$GET(E)_$GET(F)
        SET MSG=MSG_$GET(G)_$GET(H)_$GET(I)_$GET(J)_$GET(K)_$GET(L)
        DO DBINDENT(INDENT)
        DO DBWRITE(INDENT,.MSG,1)
        QUIT 
        ;
DBINDENT(INDENT,FORCED)  ;"DEBUG INDENT
        ;"PUBLIC FUNCTION
        ;"Purpose: to provide a unified indentation for debug messages
        ;"Input: INDENT = number of indentations
        ;"       Forced = 1 IF to indent regardless of DEBUG mode
        SET FORCED=+$GET(FORCED)
        IF (+$GET(TMGDEBUG)=0)&(Forced=0) QUIT
        NEW IDX FOR IDX=1:1:INDENT DO
        . IF FORCED DO DBWRITE(INDENT,"  ")
        . ELSE  DO DBWRITE(INDENT,". ")
        QUIT
        ;
DBWRITE(INDENT,STR,NEWLN,IOFILE)  ;
        ;"PUBLIC FUNCTION
        ;"Purpose: to WRITE debug output.  Having the proc separate will allow
        ;"        easier dump to file etc.
        ;"Input:INDENT, the amount of indentation expected for output.
        ;"      STR -- the text to WRITE
        ;"      NEWLN -- if 1, a carrage return (new line) is output after s
        ;"      IOFILE -- Optional, but if TMGDEBUG=2 or 3, should be valid mumps output file
        ;"TMGDEBUG is referenced in global scope.
        ;"Relevant TMGDEBUG values
        ;"        0, or undefined -- will abort output
        ;"        1 - Debug output to screen 
        ;"        2 or 3 - Debug output to file 
        SET TMGDEBUG=+$GET(TMGDEBUG) IF TMGDEBUG=0 QUIT
        IF (TMGDEBUG=2)!(TMGDEBUG=3),$DATA(IOFILE) USE IOFILE
        SET NEWLN=+$GET(NEWLN)
        NEW CH,CHARNUM,l,i
        NEW LEN SET LEN=$LENGTH(STR)
        NEW IDX FOR IDX=1:1:LEN do
        . SET CH=$EXTRACT(STR,IDX)
        . SET CHARNUM=$ASCII(CH)
        . IF (CHARNUM<32)&(CHARNUM'=13) WRITE "<",CHARNUM,">"
        . ELSE  WRITE CH
        IF NEWLN=1 WRITE !
        IF (TMGDEBUG=2)!(TMGDEBUG=3),$DATA(IOFILE) USE $P
        QUIT
        ;