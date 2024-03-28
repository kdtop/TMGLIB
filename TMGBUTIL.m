TMGBUTIL ;TMG/kst/Binary Global Data Utilities ;03/25/06, 2/2/14
         ;;1.0;TMG-LIB;**1**;08/20/05

 ;"TMG BINARY GLOBAL DATA UTILITY FUNCTIONS
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
 ;"DISPLAY(globalRef,incSubscr,offset,numLines,bytesPerLine)
 ;"BROWSE(globalRef,incSubscr)

 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================


 ;"=======================================================================
 ;"DEPENDENCIES
 ;"=======================================================================
 ;"Uses:   TMGBINF
 ;"          TMGMISC
 ;"          TMGSTUTL

 ;"=======================================================================

BROWSE(globalRef,incSubscr)
        ;"SCOPE: PUBLIC
        ;"Purpose: to browse a binary SET as hex codes

        NEW offset SET offset=0
        NEW input
        FOR  DO  QUIT:(offset="")
        . read "Offset to browse (? for help): ",input:$GET(DTIME,3600),!
        . IF input="?" WRITE "^ to abort,A=browse up, Z=browse down",! QUIT
        . IF input="^" SET offset="" QUIT
        . IF input="" SET input="Z"
        . IF "Aa"[input SET offset=offset-(8*16)
        . IF "Zz"[input SET offset=offset+(8*16)
        . IF $EXTRACT(input,1)="$" SET input=$$HEX2DEC^TMGMISC(input)
        . IF +input=input SET offset=input
        . IF +offset'=offset SET offset="" QUIT
        . DO DISPLAY(globalRef,incSubscr,offset,8,16)

        QUIT


DISPLAY(globalRef,incSubscr,offset,numLines,bytesPerLine)
        ;"SCOPE: PUBLIC
        ;"Purpose: to display a binary SET as hex codes
        ;"Input: gobalRef -- the reference of the beginning of the block (in closed form)
        ;"        incSubscr-- (required) Identifies the incrementing subscript level.  For example, IF you
        ;"                           pass ^TMP(115,1,1,0) as the global_ref parameter and pass 3 as the
        ;"                           inc_subscr parameter, $$BIN2GBL will increment the third subscript, such
        ;"                           as ^TMP(115,1,x), but will WRITE notes at the full global reference, such
        ;"                           as ^TMP(115,1,x,0).
        ;"        offset --       (OPTIONAL) the bytes offset from the beginning of the
        ;"                          block to start from. Default=0
        ;"        numLines -- (OPTIONAL) the number of lines to show.  Default=8
        ;"        bytesPerLine -- (OPTIONAL) the number of bytes to show per line Default=16
        ;"Output -- displays the hex bytes to the screen
        ;"Result -- none

        ;"Note: each line in the global ref is assumed to hold 512 bytes.

        NEW index,data
        NEW bytesNeeded
        NEW atEnd SET atEnd=0

        SET offset=$GET(offset,0)
        SET numLines=$GET(numLines,8)
        SET bytesPerLine=$GET(bytesPerLine,16)
        SET bytesNeeded=numLines*bytesPerLine
        SET index=offset\512

        IF index>0 SET globalRef=$$NEXTNODE^TMGBINF(globalRef,incSubscr,1,index)
        IF (globalRef="") GOTO DispDone
        SET data=$EXTRACT($GET(@globalRef),(offset#512)+1,512)

        FOR  QUIT:($LENGTH(data)'<bytesNeeded)!(atEnd>0)  do
        . SET globalRef=$$NEXTNODE^TMGBINF(globalRef,incSubscr,1,1)
        . IF (globalRef="") SET atEnd=1 QUIT
        . NEW oneLine SET oneLine=$GET(@globalRef)
        . IF oneLine="" SET atEnd=1 QUIT
        . SET data=data_$EXTRACT(oneLine,1,bytesNeeded-$LENGTH(data))

        ;"Now display data
        NEW dispLine
        NEW dispOffset SET dispOffset=offset
        FOR  QUIT:($LENGTH(data)=0)  do
        . SET dispLine=$EXTRACT(data,1,bytesPerLine)
        . SET data=$EXTRACT(data,bytesPerLine+1,bytesNeeded)
        . WRITE "$",$$HEXCHR2^TMGMISC(dispOffset,6),"  "
        . WRITE $$STRB2H^TMGSTUTL(dispLine,1),!
        . SET dispOffset=dispOffset+bytesPerLine


DispDone
        QUIT


