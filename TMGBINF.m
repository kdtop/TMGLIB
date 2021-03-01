TMGBINF ;TMG/kst/Binary <--> Global Functions ;11/23/12, 2/2/14
         ;;1.0;TMG-LIB;**1**;08/20/05

 ;"TMG BIN <-->GBL FUNCTIONS
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
 ;"$$BFTG(path,filename,globalRef,incSubscr,width) -- BINARY FILE TO GLOBAL
 ;"$$GTBF(globalRef,incSubscr,path,filename) -- GLOBAL TO BINARY FILE
 ;"CPYBG(srcGREF,srcIncSubscr,dstGREF,dstIncSubscr,width) -- COPY/RESIZE BINARY GLOBAL.
 ;
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"$$NEXTNODE(curRef,incSubscr)
 ;"$$READBG(GREF,incSubscr,pos,count,actualCount)  -- STREAM READ FROM BINARY GLOBAL
 ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"=======================================================================
 ;"Uses:  (No other units)
 ;
 ;"=======================================================================
 ;
DATAWRAP(DATASTR,REF,LEN) ;     
   ;"PURPOSE: Wrap a long datastring into an array of @REF, with each line LEN long
   ;"NOTE: It is assumed that @REF is empty.  
   ;"INPUT:  DATASTR -- This will be a very long string, perhaps megabytes long
   ;"        REF -- Reference of variable or Global to output to. in fully
   ;"               resolved (closed root) format.  This function does NOT KILL the global
   ;"               before writing to it.  e.g. "ZZZ", or "ZZZ(12)", or "^TMP(123)"
   ;"        LEN -- OPTIONAL.  Default is 60.  Length of each line of output array
   ;"OUTPUT:  @REF@(#)=1 cleaved line from DATASTR.
   ;"               # will be 1,2,3 ...
   NEW CUTLEN SET CUTLEN=+$GET(LEN,60) QUIT:CUTLEN'>0
   NEW IDX SET IDX=1
   NEW POS SET POS=1
   NEW STRLEN SET STRLEN=$LENGTH(DATASTR)
   FOR  QUIT:POS>STRLEN  DO
   . SET @REF@(IDX)=$EXTRACT(DATASTR,POS,POS+CUTLEN-1)
   . SET POS=POS+CUTLEN
   . SET IDX=IDX+1
   QUIT
   ;
BFTG(path,filename,globalRef,incSubscr,width)
  ;"SCOPE: PUBLIC
  ;"Purpose: To load a binary file from the host filesystem into a global, storing
  ;"              the composit bytes as raw binary data.
  ;"              You do not need to open the host file before making this call; it is opened
  ;"              and closed automatically                                     
  ;"Input: path --        (required) full path, up to but not including the filename
  ;"         filename --  (required) name of the file to open
  ;"         globalRef-- (required) Global reference to WRITE the host binary file to, in fully
  ;"                           resolved (closed root) format.  This function does NOT KILL the global
  ;"                           before writing to it.
  ;"                           Note:
  ;"                           At least one subscript must be numeric.  This will be the incrementing
  ;"                           subscript (i.e. the subscript that $$BIN2WP^TMGBINWP will increment
  ;"                           to store each NEW global node).  This subscript need not be the final
  ;"                           subscript.  For example, to load into a WORD PROCESSING field, the
  ;"                           incrementing node is the second-to-last subscript; the final subscript
  ;"                           is always zero.
  ;"        incSubscr-- (required) Identifies the incrementing subscript level.  For example, IF you
  ;"                           pass ^TMP(115,1,1,0) as the global_ref parameter and pass 3 as the
  ;"                           inc_subscr parameter, $$BIN2GBL will increment the third subscript, such
  ;"                           as ^TMP(115,1,x), but will WRITE notes at the full global reference, such
  ;"                           as ^TMP(115,1,x,0).
  ;"        width --        OPTIONAL -- the number of bytes to store per line. Default=512
  ;"                         *** NOTICE: width is not working properly.  For now, just don't supply a number.
  ;
  ;"Result: 1=success, 0=failure  <-- changed error result to 0^Message //kt 11/23/12  Also changed routines that call this procedure
  ;"
  ;"Example:
  ;"  WRITE $$BFTG(path,file,"^TMP(115,1,1,0)",3)                                                                 
  ;"  ^TMP(115,1,1,0)="04016785439093479334987689724398732490782..."
  ;"  ^TMP(115,1,2,0)="09834573467345092647823450982345858792346..."
  ;"  ^TMP(115,1,3,0)="90783492734987234098243908723459590823494..."
  ;"  ^TMP(115,1,4,0)="23489723450234097234980732402349955987284..."
  ;"  ^TMP(115,1,5,0)="0983457823450982734572349874234874"  <-- not padded with terminal zeros
  ;"In this example, only digits 0-9 are shown.  In reality each digit can be a byte with a value 0-255
  ;
  NEW result SET result=0  ;"default to failure
  NEW handle SET handle="TMGHANDLE"
  NEW abort SET abort=0
  NEW blockIn
  NEW $ETRAP
  IF $GET(globalRef)="" GOTO BFTGDone
  ;
  NEW curRef
  NEW tempRef SET tempRef="^TMP(""BFTG^TMGBINF"","_$J_",1)"
  ;"if user wants a width other than 512, will have to load into a temporary location,
  ;"and then copy over to final destination at requested with
  IF +$GET(width)>0 SET curRef=tempRef
  ELSE  SET curRef=globalRef
  ;
  SET filename=$GET(filename)
  IF filename="" GOTO BFTGDone
  ;
  SET path=$$DEFDIR^%ZISH($GET(path))
  ;
  ;"Note: Each line will 512 bytes long (512 is hard coded into OPEN^%ZISH)
  DO OPEN^%ZISH(handle,path,filename,"RB")  ;"B is a 512 block/binary mode
  IF POP DO  GOTO BFTGDone
  . ;"WRITE "Error opening file...",!
  . SET result="0^Unable to open file for binary read: "_path_filename
  ;"set $ETRAP="set abort=1,$ECODE="""" QUIT"
  use IO
  FOR  DO  QUIT:($ZEOF)!(abort=1)!(blockIn="")
  . SET $ETRAP="set abort=1,abort(1)=""Error during read: ""_$ECODE,$ECODE="""" QUIT"
  . read blockIn:10   ;"10 sec timeout
  . IF (blockIn="") QUIT
  . SET @curRef=blockIn
  . SET curRef=$$NEXTNODE(curRef,incSubscr)
  ;
  IF abort=1 do
  . ;"WRITE "Aborted...",!
  . SET result="0^"_$get(abort(1))
  IF (abort'=1) SET result=1 ;"SUCCESS
  DO CLOSE^%ZISH(handle)
  ;
  IF +$GET(width)>0 do
  . DO CPYBG(tempRef,3,globalRef,incSubscr,width)
  . KILL @tempRef
  ;
  ;
BFTGDone ;
  QUIT result
  ;
  ;
GTBF(globalRef,incSubscr,path,filename) ;
  ;"SCOPE: PUBLIC
  ;"Purpose: This function will WRITE the values of nodes of a global (at the subscript
  ;"              level you specify) to a host file, in a binary fashion.  If the host file already
  ;"              exists, it is truncated to length zero (0) before the copy.
  ;"              Each line of the global is written out, in a serial fashion based on the ordering
  ;"              of the subscripts, with no line terminators written between lines.
  ;"              You DO not need to open the host file before making this call; it is opened
  ;"              and closed $$GTBF^TMGBINF
  ;"Input:
  ;"         globalRef-- Global reference to WRITE the host binary file to, in fully resolved
  ;"                              (closed root) format.  This function does not KILL the global before
  ;"                              writing to it.  (required)
  ;"                           Note:
  ;"                           At least one subscript must be numeric.  This will be the incrementing
  ;"                           subscript (i.e. the subscript that $$BIN2WP^TMGBINWP will increment
  ;"                           to store each new global node).  This subscript need not be the final
  ;"                           subscript.  For example, to load into a WORD PROCESSING field, the
  ;"                           incrementing node is the second-to-last subscript; the final subscript
  ;"                           is always zero.
  ;"        incSubscr-- (required) Identifies the incrementing subscript level.  For example, IF you
  ;"                           pass ^TMP(115,1,1,0) as the global_ref parameter and pass 3 as the
  ;"                           inc_subscr parameter, $$BIN2GBL will increment the third subscript, such
  ;"                           as ^TMP(115,1,x), but will WRITE notes at the full global reference, such
  ;"                           as ^TMP(115,1,x,0).
  ;"         path --        full path, up to but not including the filename (required)
  ;"         filename --  name of the file to open (required)
  ;"Result: 1=success, 0=failure    <-- changed error result to 0^Message //kt 11/23/12  Also changed routines that call this procedure
  ;"
  ;"Example:
  ;"  WRITE $$GTBF(path,file,"^TMP(115,1,1,0)",3)
  ;"  ^TMP(115,1,1,0)="04016785439093479334987689724398732490782..."
  ;"  ^TMP(115,1,2,0)="09834573467345092647823450982345858792346..."
  ;"  ^TMP(115,1,3,0)="90783492734987234098243908723459590823494..."
  ;"  ^TMP(115,1,4,0)="23489723450234097234980732402349955987284..."
  ;"  ^TMP(115,1,5,0)="0983457823450982734572349874234874"
  ;"Each line would be sent to the output file in turn as a continuous data sequence.
  ;"In this example, only digits 0-9 are shown.  In reality each digit can be a byte with a value 0-255
  ;"
  ;
  NEW result SET result=0  ;"default to failure
  NEW handle SET handle="TMGHANDLE"
  NEW abort SET abort=0
  NEW blockOut
  NEW mustExist SET mustExist=1
  NEW $ETRAP
  NEW curRef SET curRef=globalRef
  ;
  SET path=$$DEFDIR^%ZISH($GET(path))
  DO OPEN^%ZISH(handle,path,filename,"W")
  IF POP DO  GOTO GTBFDone
  . SET result="0^Unable to open file for writing: "_path_filename
  ;"set $ETRAP="set abort=1,$ECODE="""" QUIT"
  SET $ETRAP="set abort=1,abort(1)=""Error during read: ""_$ECODE,$ECODE="""" QUIT"
  use IO
  FOR  DO  QUIT:(curRef="")!(abort=1)
  . SET blockOut=$GET(@curRef)
  . IF (blockOut'="") WRITE blockOut
  . SET $X=0 ;"prevent IO system from 'wrapping' (adding a linefeed)
  . SET curRef=$$NEXTNODE(curRef,incSubscr,mustExist)
  ;
  IF abort=1 do
  . ;"WRITE "Aborted...",!
  . SET result="0^"_$get(abort(1))
  IF (abort'=1) SET result=1 ;"SUCCESS
  DO CLOSE^%ZISH(handle)
  ;
GTBFDone ;
  QUIT result
  ;
  ;
NEXTNODE(curRef,incSubscr,mustExist,incAmount) ;
  ;"SCOPE: PUBLIC
  ;"Purpose: to take a global reference, and increment the node specified by incSubscr
  ;"Input:   curRef --    The reference to alter, e.g. '^TMP(115,1,4,0)'
  ;"           incSubscr--The node to alter, e.g.
  ;"                              1-->^TMG(x,1,4,0)    x would be incremented
  ;"                              2-->^TMG(115,x,4,0) x would be incremented
  ;"                              3-->^TMG(115,1,x,0) x would be incremented
  ;"                              4-->^TMG(115,1,4,x) x would be incremented
  ;"           mustExist-- (Option)  IF >0, then after incrementing, If resulting
  ;"                               reference doesn't exist then "" is returned.
  ;"           incAmount -- (Optional) the amount to increment by (default=1)
  ;"Note: The node that incSubscr references should be numeric (i.e. not a name)
  ;"      otherwise the alpha node will be treated as a 0
  ;"result: returns the NEW reference (or "" IF doesn't exist and mustExist>0)

  NEW i,result
  SET incAmount=$GET(incAmount,1)
  SET result=$qsubscript(curRef,0)_"("
  for i=1:1:$qlength(curRef) do
  . NEW node
  . IF i'=1 SET result=result_","
  . SET node=$qsubscript(curRef,i)
  . IF i=incSubscr SET node=node+incAmount
  . IF (node'=+node) SET node=""""_node_""""
  . SET result=result_node
  SET result=result_")"
  ;
  IF $GET(mustExist,0)>0 do
  . IF $DATA(@result)#10=0 SET result=""
  ;
  QUIT result
  ;
  ;
CPYBG(srcGREF,srcIncSubscr,dstGREF,dstIncSubscr,width) ;
        ;
  ;"*** NOTICE: THIS FUNCTION IS NOT WORKING PROPERLY, IT REPEATS THE DATA IN BLOCKS...***
  ;
  ;"Purpose: COPY/RESIZE BINARY GLOBAL.  This can be used to change the number of bytes
  ;"              stored on each line of a binary global array
  ;"Input:
  ;"      srcGREF--      Global reference of the SOURCE binary global array, in fully resolved
  ;"                              (closed root) format.
  ;"                           Note:
  ;"                           At least one subscript must be numeric.  This will be the incrementing
  ;"                           subscript (i.e. the subscript that $$BIN2WP^TMGBINWP will increment
  ;"                           to store each NEW global node).  This subscript need not be the final
  ;"                           subscript.  For example, to load into a WORD PROCESSING field, the
  ;"                           incrementing node is the second-to-last subscript; the final subscript
  ;"                           is always zero.
  ;"                           REQUIRED
  ;"      srcIncSubscr-- (required) Identifies the incrementing subscript level, for the source global
  ;"                           For example, IF you pass ^TMP(115,1,1,0) as the global_ref parameter and
  ;"                           pass 3 as the inc_subscr parameter, $$BIN2GBL will increment the third
  ;"                           subscript, such as ^TMP(115,1,x), but will WRITE notes at the full global
  ;"                           reference, such as ^TMP(115,1,x,0).
  ;"                           REQUIRED
  ;"      dstGREF--      Global reference of the DESTINATION binary global array, in fully resolved
  ;"                              (closed root) format.  The destination IS NOT KILLED prior to filling with
  ;"                           NEW data
  ;"                           Note:
  ;"                           At least one subscript must be numeric.  (same as note above)
  ;"                           REQUIRED
  ;"      dstIncSubscr-- (required) Identifies the incrementing subscript level, for the source global
  ;"                           For example, IF you pass ^TMP(115,1,1,0) as the global_ref parameter and
  ;"                           pass 3 as the inc_subscr parameter, $$BIN2GBL will increment the third
  ;"                           subscript, such as ^TMP(115,1,x), but will WRITE notes at the full global
  ;"                           reference, such as ^TMP(115,1,x,0).
  ;"                           REQUIRED
  ;"       width--          The number of bytes to store per line in the DESTINATION array.
  ;"                           REQUIRED
  ;"
  ;"Output: @dstGREF is filled with data
  ;"Result: None
  ;
  ;
 ;"*** NOTICE: THIS FUNCTION IS NOT WORKING PROPERLY, IT REPEATS THE DATA IN BLOCKS...***
  ;
  ;
  NEW readPos SET readPos=1
  NEW bytesRead
  IF $GET(srcGREF)="" GOTO CPYBGDone
  IF $GET(dstGREF)="" GOTO CPYBGDone
  IF $GET(srcIncSubscr)="" GOTO CPYBGDone
  IF $GET(dstIncSubscr)="" GOTO CPYBGDone
  IF $GET(width)="" GOTO CPYBGDone
  ;
  FOR  DO  QUIT:(bytesRead=0)
  . SET @dstGREF=$$READBG(srcGREF,srcIncSubscr,readPos,width,.bytesRead)
  . IF (bytesRead=0) KILL @dstGREF
  . SET readPos=readPos+bytesRead
  . SET dstGREF=$$NEXTNODE(dstGREF,dstIncSubscr,0,1)
  ;
CPYBGDone  ;
  QUIT
  ;
  ;
READBG(GREF,incSubscr,pos,count,actualCount)  ;
  ;"SCOPE: PUBLIC
  ;"Purpose: To read 'count' bytes from binary global '@srcGREF', starting at 'pos'
  ;"Input:
  ;"         GREF--      Global reference of the binary global array, in fully resolved
  ;"                              (closed root) format.
  ;"                           Note:
  ;"                           At least one subscript must be numeric.  This will be the incrementing
  ;"                           subscript (i.e. the subscript that $$BIN2WP^TMGBINWP will increment
  ;"                           to store each NEW global node).  This subscript need not be the final
  ;"                           subscript.  For example, to load into a WORD PROCESSING field, the
  ;"                           incrementing node is the second-to-last subscript; the final subscript
  ;"                           is always zero.
  ;"                           REQUIRED
  ;"      incSubscr-- (required) Identifies the incrementing subscript level, for the source global
  ;"                           For example, IF you pass ^TMP(115,1,1,0) as the global_ref parameter and
  ;"                           pass 3 as the inc_subscr parameter, $$BIN2GBL will increment the third
  ;"                           subscript, such as ^TMP(115,1,x), but will WRITE notes at the full global
  ;"                           reference, such as ^TMP(115,1,x,0).
  ;"                           REQUIRED
  ;"      Pos--             The position in the binary global to start reading from (0 is first byte), as if
  ;"                           entire global array is one long binary stream. E.g. the 913th byte might be
  ;"                           actually the 17th byte on the 14th line (if 64 bytes are stored per line).  But
  ;"                           this is handled transparently and the user need only specify byte #913 etc.
  ;"                        .  The reading will start at the appropriate point.
  ;"      count--           The number of bytes/characters to read.
  ;"  actualCount--      OPTIONAL.  An OUT PARAMETER -- PASS BY REFERENCE
  ;"                           This is filled with the actual number of bytes/characters successfully read.
  ;"Result: a string filled with requested number of bytes/characters
  ;
  NEW result SET result=""
  NEW countPerLine
  NEW goalLen SET goalLen=count
  NEW Line,p1
  NEW done
  IF $GET(GREF)="" GOTO ReadBGDone
  SET countPerLine=$LENGTH(@GREF)
  IF (countPerLine=0) GOTO ReadBGDone
  ;
  SET Line=pos\countPerLine
  SET p1=pos#countPerLine
  ;
  FOR  DO  QUIT:(done=1)!(count<1)
  . NEW curRef
  . SET done=0
  . SET curRef=$$NEXTNODE(GREF,incSubscr,1,Line)
  . IF curRef="" SET done=1 QUIT
  . SET result=result_$EXTRACT(@GREF,p1,p1+count-1)
  . SET count=goalLen-$LENGTH(result)
  . IF count<1 SET done=1 QUIT
  . SET Line=Line+1
  . SET p1=1
  ;
ReadBGDone ;
  SET actualCount=$LENGTH(result)
  QUIT result
  ;