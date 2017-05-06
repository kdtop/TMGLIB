TMGRPC1C ;TMG/kst-RPC Functions ;07/09/10, 9/19/10, 2/18/14
         ;;1.0;TMG-LIB;**1**;07/09/10
 ;
 ;"TMG RPC FUNCTIONS especially related to CPRS
 ;"  Including CPRS imaging.
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
 ;"GETDEFNL() -- return the default Network Location (file 2005.2) entry
 ;"GETLOCFPATH(FPATH,LOCIEN) -- get local (absolute) path for storing on host file system
 ;"GETDROPPATH(LOCIEN,DROPBOX) -- return path to local dropbox.
 ;"DOWNLOAD(GREF,FPATH,FNAMEW $$,LOCIEN)
 ;"UPLOAD(RESULT,FPATH,FNAME,LOCIEN,ARRAY)
 ;"DOWNDROP(RESULT,FPATH,FNAME,LOCIEN)  -- Download drop box file
 ;"UPLDDROP(RESULT,FPATH,FNAME,LOCIEN)  -- Upload Dropbox File
 ;"DELIMAGE(RESULT,IMGIEN,MODE,REASON) -- Delete or Retract Image
 ;"UNRETRACT(RESULT,TMGIEN) -- reverse retraction process from DELIMAGE above.
 ;"GETPWDS(RESULT,DUZ) -- Recover user's Verify Code, Access Code, and ESig
 ;"GETMENU(TMGOUT,NAME) --return a listing of menu by name, taylored for USER
 ;"$$LOCKED(TMGKEY) -- return IF current user (DUZ) is missing SECURITY KEY assigned to them
 ;"$$LOCKDIEN(KEYIEN) -- return IF current user (DUZ) is missing SECURITY KEY (BY IEN) assigned to them
 ;"$$DISALLOW(IEN19) -- determine IF menu option is on list of DISALLOWED menu options
 ;"$$CANRUN(IEN19,USER) -- determine IF menu OPTION is allowed for user to run.

 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"ENCODE(GREF,INCSUBSCR,ENCODEFN)
 ;"DECODE(GREF,INCSUBSCR,DECODEFN)
 ;"$$HEXCODER(INPUT)    ;encode the input string.  Currently using simple hex encoding/
 ;"$$B64CODER(INPUT)    ;encode the input string via UUENCODE (actually Base64)
 ;"$$B64DECODER(INPUT)  ;encode the input string via UUDECODE (actually Base64)
 ;"ENSUREDIV(FPATH,LOCIEN) ;Ensure that the path ends with an appropriate node divider.
 ;"GET1MNU(IEN) -- Get entry info for one option
 ;"$$CHKOPTN(IEN19,ALLOWEDMENUS) -- determine IF self/parent matches SET of allowed menus
 ;"=======================================================================
 ;"Dependancies:
 ;" DIK, TMGDEBUG, TMGKERNL, TMGBINF, TMGSTUTL, RGUTUU,
 ;"=======================================================================
 ;
GETDEFNL()
        ;"Purpose: to return the default Network Location (file 2005.2) entry
        ;"Input: None
        ;"Results: Returns IEN in file 2005.2,  or 1 IF some problem.
        ;
        NEW RESULT SET RESULT=1  ;"Default
        ;
        ;"First get default INSTITUTION, stored in KERNEL SYSTEM PARAMETERS file.
        NEW INSTPTR  SET INSTPTR=+$PIECE($GET(^XTV(8989.3,1,"XUS")),"^",17)  ;"Ptr to file $4 (Institution)
        IF INSTPTR'>0 GOTO GDFNDN
        ;
        ;"Now get IMAGING SITE PARAMETERS for Institution Name
        NEW IMGSPPTR SET IMGSPPTR=+$ORDER(^MAG(2006.1,"B",INSTPTR,0))
        IF IMGSPPTR'>0 GOTO GDFNDN
        ;
        ;"Now get NETWORK LOCATION stored in IMAGING SITE PARAMETERS record
        NEW LOCPTR SET LOCPTR=+$PIECE($GET(^MAG(2006.1,IMGSPPTR,0)),"^",3)
        IF LOCPTR>0 SET RESULT=LOCPTR
        ;
GDFNDN  QUIT RESULT
        ;
        ;
ENSUREDIV(FPATH,LOCIEN) ;
        ;"Purpose: Ensure that the path ends with an appropriate node divider.
        SET FPATH=$GET(FPATH,"/")
        SET LOCIEN=+$GET(LOCIEN) IF LOCIEN'>0 SET LOCIEN=$$GETDEFNL()
        ;
        ;"default is "/"    NOTE: CUSTOM FIELD
        NEW NODEDIV SET NODEDIV=$PIECE($GET(^MAG(2005.2,LOCIEN,22701),"/"),"^",1)
        ;
        NEW ENDCHAR SET ENDCHAR=$EXTRACT(FPATH,$LENGTH(FPATH))
        IF ENDCHAR'=NODEDIV SET FPATH=FPATH_NODEDIV
        QUIT FPATH
        ;
GETLOCFPATH(FPATH,LOCIEN) ;
        ;"Purpose: to get local (absolute) path for storing on host file system
        ;"Input: FPATH --      the file path up to, but not including, the filename
        ;"                     Use '/' to NOT specify any subdirectory
        ;"                     [optional] default is '/'
        ;"       LOCIEN--      [optional] -- the IEN from file 2005.2 (network location) to download from
        ;"                      NOTE: DEPRECIATED.  Should pass "" to allow code to lookup default
        ;"                      values stored in KERNEL SYSTEM PARAMETERS etc.
        ;"                      Note: For security reasons, all path requests will be considered relative to a root path.
        ;"                            e.g. IF user asks for /download/SomeFile.jpg, this function will retrieve:
        ;"                                        /var/local/Dir1/Dir2/download/SomeFile.jpg
        ;"                            This root path is found in custom field 22701 in file 2005.2
        ;"Returns: A path, that can be passed to KERNEL calls for HFS calls.
        ;"         NOTE: Result WILL end with a node divider
        ;"         NOTE: If resulting path did not exist, then an attempt will be made
        ;"               to create the directory.  
        ;
        SET LOCIEN=+$GET(LOCIEN) IF LOCIEN'>0 SET LOCIEN=$$GETDEFNL()
        ;
        ;"NOTE: CUSTOM FIELD
        NEW PATHROOT SET PATHROOT=$PIECE($GET(^MAG(2005.2,LOCIEN,22700)),"^",1)
        ;
        ;"default is "/"    NOTE: CUSTOM FIELD
        NEW NODEDIV SET NODEDIV=$PIECE($GET(^MAG(2005.2,LOCIEN,22701),"/"),"^",1)
        SET FPATH=$GET(FPATH,NODEDIV)
        IF NODEDIV="/" SET FPATH=$TRANSLATE(FPATH,"\",NODEDIV)
        ;
        NEW ENDROOT SET ENDROOT=$EXTRACT(PATHROOT,$LENGTH(PATHROOT))
        NEW STARTPATH SET STARTPATH=$EXTRACT(FPATH,1)
        ;
        IF (ENDROOT=NODEDIV)&(STARTPATH=NODEDIV) DO
        . SET FPATH=$EXTRACT(FPATH,2,1024)
        ELSE  IF (ENDROOT'=NODEDIV)&(STARTPATH'=NODEDIV) DO
        . SET PATHROOT=PATHROOT_NODEDIV

        SET FPATH=$$ENSUREDIV(PATHROOT_FPATH,LOCIEN)
        NEW MSG SET MSG=$$ENSURDIR^TMGKERNL(FPATH)
        IF +MSG=-1 SET FPATH=MSG GOTO GLFDN
        
GLFDN   QUIT FPATH
        ;
        ;
GETDROPPATH(LOCIEN,DROPBOX) ;
        ;"Purpose: return path to local dropbox.
        ;"Input: LOCIEN  -- the IEN from file 2005.2 (network location)
        ;"       DROPBOX -- PASS BY REFERENCE.  AN OUT PARAMETER.
        ;"Results: 1 if OK, -1 IF error
        SET LOCIEN=+$GET(LOCIEN)
        IF LOCIEN'>0 SET LOCIEN=$$GETDEFNL()
        NEW RESULT SET RESULT=1
        SET DROPBOX=$PIECE($GET(^MAG(2005.2,LOCIEN,22702)),"^",1)
        IF DROPBOX="" DO  GOTO GDPDN
        . SET RESULT=-1
        SET DROPBOX=$$ENSUREDIV(DROPBOX,LOCIEN)
GDPDN   QUIT RESULT
        ;
        ;
DOWNLOAD(GREF,FPATH,FNAME,LOCIEN)
        ;"SCOPE: Public
        ;"Purpose: To provide an entry point for a RPC call from a client.  The client
        ;"              will ask for a given file, and it will be passed back in the form
        ;"              of an array (in BASE64 ascii encoding)
        ;"Input: GREF --        OUT PARAM -- the array to pass the result back in (PASSED BY REFERENCE)
        ;"       FPATH --      the file path up to, but not including, the filename
        ;"                     Use '/' to NOT specify any subdirectory
        ;"                     [optional] default is '/'
        ;"       FNAME --     the name of the file to pass back
        ;"       LOCIEN--      [optional] -- the IEN from file 2005.2 (network location) to download from
        ;"                            NOTE: DEPRECIATED.  Should pass "" to allow code to lookup default
        ;"                            values stored in KERNEL SYSTEM PARAMETERS etc.
        ;"                            Note: For security reasons, all path requests will be considered relative to a root path.
        ;"                                    e.g. IF user asks for /download/SomeFile.jpg, this function will retrieve:
        ;"                                        /var/local/Dir1/Dir2/download/SomeFile.jpg
        ;"                                    This root path is found in custom field 22701 in file 2005.2
        ;"Output: results are passed out in @GREF
        ;"              @GREF@(0)=success;    1=success, 0=failure <-- Changed to 0^ErrorMessage 11/23/12
        ;"              @GREF@(1..xxx) = actual data
        ;
        NEW TMGUPDEBUG SET TMGUPDEBUG=0
        IF TMGUPDEBUG=0 DO
        . KILL ^TMG("TMP","RPC","DOWNLOAD^TMGRPC1C")
        . SET ^TMG("TMP","RPC","DOWNLOAD^TMGRPC1C","FPATH")=$GET(FPATH)
        . SET ^TMG("TMP","RPC","DOWNLOAD^TMGRPC1C","FNAME")=$GET(FNAME)
        . SET ^TMG("TMP","RPC","DOWNLOAD^TMGRPC1C","LOCIEN")=$GET(LOCIEN)
        ELSE  DO
        . SET FPATH=^TMG("TMP","RPC","DOWNLOAD^TMGRPC1C","FPATH")
        . SET FNAME=^TMG("TMP","RPC","DOWNLOAD^TMGRPC1C","FNAME")
        . SET LOCIEN=^TMG("TMP","RPC","DOWNLOAD^TMGRPC1C","LOCIEN")
        ;
        SET FNAME=$GET(FNAME)
        SET LOCIEN=+$GET(LOCIEN)
        IF LOCIEN'>0 SET LOCIEN=$$GETDEFNL()
        SET GREF="^TMP(""DOWNLOAD^TMGRPC1"","_$J_")"
        KILL @GREF
        NEW INITPATH 
DL1     SET INITPATH=FPATH
        SET FPATH=$$GETLOCFPATH($GET(FPATH),LOCIEN) ;
        IF +FPATH=-1 SET @GREF@(0)="0^"_$PIECE(FPATH,"^",2,99) GOTO DLDN
        NEW TEMP SET TEMP=$$BFTG^TMGBINF(.FPATH,.FNAME,$NAME(@GREF@(1)),3)
        IF +TEMP=0,(INITPATH'="") SET FPATH="" GOTO DL1  ;"If can't find in hashed directory, try in main images folder.
        SET @GREF@(0)=TEMP
        IF +TEMP=1 DO ENCODE($NAME(@GREF@(1)),3)
DLDN    QUIT
        ;
        ;
UPLOAD(RESULT,FPATH,FNAME,LOCIEN,ARRAY)
        ;"SCOPE: Public
        ;"RPC That calls this: TMG UPLOAD FILE
        ;"Purpose: To provide an entry point for a RPC call from a client.  The client
        ;"              will provide a file for upload (in BASE64 ascii encoding)
        ;"Input: GREF --    OUT PARAM -- the array to pass the result back in (PASSED BY REFERENCE)
        ;"       FPATH --   the file path up to, but not including, the filename
        ;"                  Use '/' to NOT specify any subdirectory
        ;"                     [optional] default is '/'
        ;"       FNAME --   the name of the file to pass back
        ;"       LOCIEN--   [optional] -- the IEN from file 2005.2 (network location) to upload to
        ;"                     NOTE: DEPRECIATED.  Should pass "" to allow code to lookup default
        ;"                     Note: For security reasons, all path requests will be considered relative to a root path.
        ;"                           e.g. IF user asks for /download/SomeFile.jpg, this function will retrieve:
        ;"                               /var/local/Dir1/Dir2/download/SomeFile.jpg
        ;"                           This root path is found in custom field 22701 in file 2005.2
        ;"       ARRAY --   the array that will hold the file, in BASE64 ascii encoding
        ;"Output: results are passed out in RESULT:  1^SuccessMessage   or 0^FailureMessage
        ;
        NEW TMGUPDEBUG SET TMGUPDEBUG=0
        IF TMGUPDEBUG=0 DO
        . KILL ^TMG("TMP","RPC","UPLOAD^TMGRPC1C")
        . SET ^TMG("TMP","RPC","UPLOAD^TMGRPC1C","FPATH")=$GET(FPATH)
        . SET ^TMG("TMP","RPC","UPLOAD^TMGRPC1C","FNAME")=$GET(FNAME)
        . SET ^TMG("TMP","RPC","UPLOAD^TMGRPC1C","LOCIEN")=$GET(LOCIEN)
        . MERGE ^TMG("TMP","RPC","UPLOAD^TMGRPC1C","ARRAY")=ARRAY
        ELSE  DO
        . SET FPATH=^TMG("TMP","RPC","UPLOAD^TMGRPC1C","FPATH")
        . SET FNAME=^TMG("TMP","RPC","UPLOAD^TMGRPC1C","FNAME")
        . SET LOCIEN=^TMG("TMP","RPC","UPLOAD^TMGRPC1C","LOCIEN")
        . KILL ARRAY
        . MERGE ARRAY=^TMG("TMP","RPC","UPLOAD^TMGRPC1C","ARRAY")
        ;        
        NEW RESLTMSG SET RESLTMSG="1^Successful Upload"
        ;
        IF $DATA(ARRAY)=0 SET RESLTMSG="0^No data received to upload" GOTO UPDN
        SET FNAME=$GET(FNAME)
        IF FNAME="" DO  GOTO UPDN
        . SET RESLTMSG="0^No file name received"
        ;
        SET LOCIEN=+$GET(LOCIEN) IF LOCIEN'>0 SET LOCIEN=$$GETDEFNL()
        SET FPATH=$$GETLOCFPATH($GET(FPATH),LOCIEN) ;
        IF +FPATH=-1 SET RESLTMSG="0^"_$PIECE(FPATH,"^",2,99) GOTO DLDN
        ;
        DO DECODE("ARRAY(0)",1)
        ;
        IF $$EnsureDir^TMGKERNL(FPATH)=0 DO  GOTO UPDN
        . SET RESLTMSG="-1^Unable to ensure file path to save location exist"
        NEW TEMP SET TEMP=$$GTBF^TMGBINF("ARRAY(0)",1,FPATH,FNAME)
        IF +TEMP=0 DO
        . SET RESLTMSG="0^Error while saving file. "_$PIECE(TEMP,"^",2,99)
        ;
        ;"SET ^TMG("TMP","RPC","UPLOAD^TMGRPC1","STEP")=3
        ;
UPDN    SET RESULT=RESLTMSG
        SET ^TMG("TMP","RPC","UPLOAD^TMGRPC1","RESULT")=RESULT
        QUIT
        ;
        ;
DOWNDROP(RESULT,FPATH,FNAME,LOCIEN)  ;"i.e. Download drop box file
        ;"SCOPE: Public
        ;"RPC That calls this: TMG DOWNLOAD FILE DROPBOX
        ;"Purpose: To provide an entry point for a RPC call from a client.  The client
        ;"         will request for the file to be placed into in a 'dropbox' file
        ;"         location that both the client and server can access.  File may be
        ;"         moved from there to its final destination by the client.
        ;"         This method alloows file-hiding ability on the server side.
        ;"Input: RESULT --    OUT PARAM -- the array to pass the result back in (PASSED BY REFERENCE)
        ;"       FPATH --   the file path up to, but not including, the filename.  This
        ;"                  is the path that the file is stored at (relative to a root path,
        ;"                  see comments below).  It is NOT the path of the dropbox.
        ;"                  Use '/' to NOT specify any subdirectory
        ;"                  [optional] default is '/'
        ;"       FNAME --   the name of the file to be uploaded.  Note: This is also the
        ;"                  name of the file to be put into the dropbox.  It is the
        ;"                  responsibility of the client to ensure that there is not already
        ;"                  a similarly named file in the dropbox before requesting a file
        ;"                  be put there.  It is the responsibility of the client to delete
        ;"                  the file from the drop box.
        ;"       LOCIEN--     [optional] -- the IEN from file 2005.2 (network location) to download from
        ;"                            NOTE: DEPRECIATED.  Should pass "" to allow code to lookup default
        ;"                            Note: For security reasons, all path requests will be considered relative to a root path.
        ;"                                    e.g. IF user asks for /download/SomeFile.jpg, this function will retrieve:
        ;"                                        /var/local/Dir1/Dir2/download/SomeFile.jpg
        ;"                                    This root path is found in custom field 22701 in file 2005.2
        ;"                       Also: dropbox location is obtained from custom field 22702 in file 2005.2
        ;"NOTE RE DROPBOX:
        ;"   This system is designed for a system where by the server and the client have a
        ;"   shared filesystem, but the directory paths will be different.  For example:
        ;"      Linux server has dropbox at: /mnt/WinServer/dropbox/
        ;"      Windows Client has access to dropbox at: V:\Dropbox\

        ;"Output: results are 1^Success^FileSize (in bytes), or 0^Error Message
        ;
        NEW DROPBOX,MOVERESULT,SRCHNAMEPATH
        ;
        NEW RESLTMSG SET RESLTMSG="1^Successful Download"
        ;
        SET FNAME=$GET(FNAME) IF FNAME="" DO  GOTO DNDBXDN
        . SET RESLTMSG="0^No file name received"
        ;
        SET FPATH=$$GETLOCFPATH(.FPATH,.LOCIEN) ;
        IF +FPATH=-1 SET RESLTMSG="0^"_$PIECE(FPATH,"^",2,99) GOTO DNDBXDN
        ;
        IF $$GETDROPPATH(.LOCIEN,.DROPBOX)=-1 DO  GOTO DNDBXDN
        . SET RESLTMSG="0^Dropbox location not configured in file 2005.2, IEN "_LOCIEN_", field 22702"
        ;
        SET SRCHNAMEPATH=FPATH_FNAME
        ;
        SET MOVERESULT=$$Copy^TMGKERNL(SRCHNAMEPATH,DROPBOX)
        IF MOVERESULT>0 DO
        . SET RESLTMSG="0^Move failed, returning OS error code: "_MOVERESULT
        ELSE  DO
        . SET RESLTMSG=RESLTMSG_"^"_$$FileSize^TMGKERNL(SRCHNAMEPATH)
        ;
DNDBXDN SET RESULT=RESLTMSG
        QUIT
        ;
        ;
UPLDDROP(RESULT,FPATH,FNAME,LOCIEN)  ;"i.e. Upload Dropbox File
        ;"SCOPE: Public
        ;"RPC That calls this: TMG UPLOAD FILE DROPBOX
        ;"Purpose: To provide an entry point for a RPC call from a client.  The client
        ;"         will put the file in a 'dropbox' file location that both the client
        ;"         and server can access.  File will be moved from there to its final
        ;"         destination.  This will provide file-hiding ability on the server side.
        ;"Input: RESULT --  OUT PARAM -- the array to pass the result back in (PASSED BY REFERENCE)
        ;"       FPATH --   the file path up to, but not including, the filename.  This
        ;"                  is the path to store the file at.  (relative to a root path,
        ;"                  see comments below).  It is NOT the path of the dropbox.
        ;"                  Use '/' to NOT specify any subdirectory
        ;"                  [optional] default is '/'
        ;"       FNAME --   the name of the file to be uploaded.  Note: This is also the
        ;"                  name of the file to be pulled from the dropbox.  It is the
        ;"                  responsibility of the client to ensure that there is not already
        ;"                  a similarly named file in the dropbox before depositing a file there.
        ;"                  The server will remove the file from the dropbox, unless there is
        ;"                  an error with the host OS (which will be returned as an error message)
        ;"       LOCIEN--   [optional] -- the IEN from file 2005.2 (network location) to upload to
        ;"                     NOTE: DEPRECIATED.  Should pass "" to allow code to lookup default
        ;"                     Note: For security reasons, all path requests will be considered relative to a root path.
        ;"                           e.g. IF user asks for /download/SomeFile.jpg, this function will retrieve:
        ;"                               /var/local/Dir1/Dir2/download/SomeFile.jpg
        ;"                           This root path is found in custom field 22700 in file 2005.2
        ;"                     Also: dropbox location is obtained from custom field 22702 in file 2005.2
        ;"NOTE RE DROPBOX:
        ;"   This system is designed for a system where by the server and the client have a
        ;"   shared filesystem, but the directory paths will be different.  For example:
        ;"      Linux server has dropbox at: /mnt/WinServer/dropbox/
        ;"      Windows Client has access to dropbox at: V:\Dropbox\
        ;
        ;"Output: results are passed out in RESULT:
        ;"      1^SuccessMessage   or 0^FailureMessage
        ;
        NEW SRCHNAMEPATH,DESTNMPATH,MOVERESULT
        NEW RESLTMSG SET RESLTMSG="1^Successful Upload"
        ;
        SET FNAME=$GET(FNAME)
        IF FNAME="" SET RESLTMSG="0^No file name received" GOTO UPDBXDN
        ;
        NEW DROPBOX
        IF $$GETDROPPATH(.LOCIEN,.DROPBOX)=-1 DO  GOTO UPDBXDN
        . SET RESLTMSG="0^Dropbox location not configured in file 2005.2, IEN "_LOCIEN_", field 22702"
        ;
        SET FPATH=$$GETLOCFPATH($GET(FPATH),LOCIEN) ;
        IF +FPATH=-1 SET RESLTMSG="0^"_$PIECE(FPATH,"^",2,99) GOTO UPDBXDN
        ;
        SET SRCHNAMEPATH=DROPBOX_FNAME
        SET DESTNMPATH=FPATH_FNAME
        ;
        SET MOVERESULT=$$MOVE^TMGKERNL(SRCHNAMEPATH,DESTNMPATH)
        IF MOVERESULT>0 DO
        . SET RESLTMSG="0^Move failed, returning OS error code: "_MOVERESULT
        ;
UPDBXDN SET RESULT=RESLTMSG
        QUIT
        ;
        ;
ENCODE(GREF,INCSUBSCR,ENCODEFN)
        ;"Purpose: ENCODE a  BINARY GLOBAL.
        ;"Input:
        ;"          GREF--      Global reference of the SOURCE binary global array, in fully resolved
        ;"                              (closed root) format.
        ;"                           Note:
        ;"                           At least one subscript must be numeric.  This will be the incrementing
        ;"                           subscript (i.e. the subscript that $$BIN2WP^TMGBINWP will increment
        ;"                           to store each NEW global node).  This subscript need not be the final
        ;"                           subscript.  For example, to load into a WORD PROCESSING field, the
        ;"                           incrementing node is the second-to-last subscript; the final subscript
        ;"                           is always zero.
        ;"                           REQUIRED
        ;"         INCSUBSCR-- (required) Identifies the incrementing subscript level, for the source global
        ;"                           For example, IF you pass ^TMP(115,1,1,0) as the global_ref parameter and
        ;"                           pass 3 as the inc_subscr parameter, $$BIN2GBL will increment the third
        ;"                           subscript, such as ^TMP(115,1,x), but will WRITE notes at the full global
        ;"                           reference, such as ^TMP(115,1,x,0).
        ;"                           REQUIRED
        ;"         ENCODEFN-   (OPTIONAL) the name of a function that will encode a line of data.
        ;"                            e.g. "CODER^ZZZCODER"  or "LOCALCODER".  The function should
        ;"                            take one input variable (the line of raw binary data), and return a converted
        ;"                            line.  e.g.
        ;"                                CODER(INPUT)
        ;"                                 ... ;"convert INPUT to RESULT
        ;"                                QUIT RESULT
        ;"                            default value is B64CODER^TMGRPC1
        ;"
        ;"Output: @GREF is converted to encoded data
        ;"Result: None
        ;
        IF $GET(GREF)="" GOTO ENCODEDN
        IF $GET(INCSUBSCR)="" GOTO ENCODEDN
        ;
        SET ENCODEFN=$GET(ENCODEFN,"B64CODER")
        ;
        NEW ENCODER
        SET ENCODER="SET TEMP=$$"_ENCODEFN_"(.TEMP)"
        ;
        FOR  DO  QUIT:(GREF="")
        . NEW TEMP
        . SET TEMP=$GET(@GREF)
        . IF TEMP="" SET GREF="" QUIT
        . XECUTE ENCODER  ;"i.e.  SET TEMP=$$encoder_Fn(.TEMP)
        . SET @GREF=TEMP
        . SET GREF=$$NEXTNODE^TMGBINF(GREF,INCSUBSCR,1,1)
        ;
ENCODEDN ;
        QUIT
        ;
        ;
HEXCODER(INPUT)
        ;"Purpose: to encode the input string.  Currently using simple hex encoding/
        QUIT $$STRB2H^TMGSTUTL(.INPUT,0,1)
        ;
        ;
B64CODER(INPUT)
        ;"Purpose: to encode the input string via UUENCODE (actually Base64)
        QUIT $$ENCODE^RGUTUU(.INPUT)
        ;
B64DECODER(INPUT)
        ;"Purpose: to encode the input string via UUENCODE (actually Base64)
        QUIT $$DECODE^RGUTUU(.INPUT)
        ;
        ;
DECODE(GREF,INCSUBSCR,DECODEFN)
        ;"Purpose: ENCODE a  BINARY GLOBAL.
        ;"Input:
        ;"          GREF--      Global reference of the SOURCE binary global array, in fully resolved
        ;"                              (closed root) format.
        ;"                           Note:
        ;"                           At least one subscript must be numeric.  This will be the incrementing
        ;"                           subscript (i.e. the subscript that $$BIN2WP^TMGBINWP will increment
        ;"                           to store each NEW global node).  This subscript need not be the final
        ;"                           subscript.  For example, to load into a WORD PROCESSING field, the
        ;"                           incrementing node is the second-to-last subscript; the final subscript
        ;"                           is always zero.
        ;"                           REQUIRED
        ;"         INCSUBSCR-- (required) Identifies the incrementing subscript level, for the source global
        ;"                           For example, IF you pass ^TMP(115,1,1,0) as the global_ref parameter and
        ;"                           pass 3 as the inc_subscr parameter, $$BIN2GBL will increment the third
        ;"                           subscript, such as ^TMP(115,1,x), but will WRITE notes at the full global
        ;"                           reference, such as ^TMP(115,1,x,0).
        ;"                           REQUIRED
        ;"         DECODEFN-   (OPTIONAL)  the name of a function that will decode a line of data.
        ;"                              e.g. "DECODER^ZZZCODER"  or "DECODER".  The function should take
        ;"                            one input variable (the line of encoded data), and return a decoded line.  e.g.
        ;"                                DECODER(INPUT)
        ;"                                 ... ;"convert INPUT to RESULT
        ;"                                QUIT RESULT
        ;"                            default value is B64DECODER^TMGRPC1
        ;"
        ;"Output: @GREF is converted to decoded data
        ;"Result: None
        ;
        IF $GET(GREF)="" GOTO DECODEDN
        IF $GET(INCSUBSCR)="" GOTO DECODEDN
        SET DECODEFN=$GET(DECODEFN,"B64DECODER")
        ;
        NEW DECODER
        SET DECODER="SET TEMP=$$"_DECODEFN_"(.TEMP)"
        ;
        FOR  DO  QUIT:(GREF="")
        . NEW TEMP
        . SET TEMP=$GET(@GREF)
        . IF TEMP="" SET GREF="" QUIT
        . XECUTE DECODER  ;"i.e.  SET TEMP=$$DECODERFn(.TEMP)
        . SET @GREF=TEMP
        . SET GREF=$$NEXTNODE^TMGBINF(GREF,INCSUBSCR,1,1)
        ;
DECODEDN QUIT
        ;
        ;
DELIMAGE(RESULT,TMGIEN,TMGMODE,TMGREASON) ;
        ;"Purpose: Provide functionality for deleting or retacting an image from CPRS
        ;"NOTE: MAGG IMAGE DELETE is not used because it does things like archive
        ;"      the images before deletion.  I don't have this system fully integrated
        ;"      In the future, that could possibly be used.
        ;"NOTE: This function DOES NOT CHECK PERMISSIONS for deleting the images.
        ;"      It is assumed that that has been doine PRIOR to calling this function.
        ;"NOTE: It mode is to retract (see below), then the image will not be
        ;"      actually be deleted.  It will just be marked as retracted and
        ;"      SET so that it doesn't appear in CPRS.
        ;"      --But IF mode is to delete, then the record in the IMAGE file
        ;"      will be deleted AND ALSO the actual image (with no backup.) This
        ;"      mode is for deletion before signing, and the image has not been
        ;"      formally entered into the record.
        ;"Input: RESULT -- an OUT Parameter. (See results below)
        ;"       TMGIEN -- the IEN in the IMAGE file (2005) to remove
        ;"       TMGMODE -- 0 for NONE <-- just exit and DO nothing
        ;"               1 for DELETE <-- delete record and image file
        ;"               2 for RETRACT <-- mark record as retracted, don't delete iamge file.
        ;"       TMGREASON -- String (10-60 chars) giving reason for deletion.
        ;"                 This is only used for mode RETRACT.
        ;"Output: RESULT="1^Success"   or "-1^Some Failure Message"  <-- SET up as SINGLE VALUE type in RPC BROKER
        ;
        SET RESULT="1^Success"  ;"Default to success
        SET TMGIEN=$GET(TMGIEN,0)
        IF +TMGIEN'>0 DO  GOTO DIDN
        . SET RESULT="-1^Invalid IEN: "_TMGIEN
        SET TMGIEN=+TMGIEN
        SET TMGMODE=+$GET(TMGMODE)
        IF TMGMODE=0 DO  GOTO DIDN
        . SET RESULT="1^Delete not done because mode=0"
        SET TMGREASON=$GET(TMGREASON,"(Not Specified)")
        NEW TMGPTR SET TMGPTR=+$PIECE($GET(^MAG(2005,TMGIEN,2)),"^",8) ;"2;8 ==> Field 18 = PARENT DATA FILE IMAGE POINTER
        IF TMGPTR'>0 DO  GOTO DIDN
        . SET RESULT="-1^FILE 2005, IEN "_TMGIEN_", Field 18 does not point to valid record in file 8925.91"
        NEW TMGTIUPTR SET TMGTIUPTR=+$PIECE($GET(^TIU(8925.91,TMGPTR,0)),"^",1) ;"0;1 ==> Field .01 = DOCUMENT (ptr to 8925)
        IF TMGMODE=1 DO  GOTO:(+RESULT'>0) DIDN   ;"Delete mode
        . NEW FNAME SET FNAME=$PIECE($GET(^MAG(2005,TMGIEN,0)),"^",2)
        . NEW TMGPATH SET TMGPATH=$$GETLOCFPATH()
        . IF +TMGPATH=-1 SET RESULT="0^"_$PIECE(TMGPATH,"^",2,99) QUIT
        . NEW TMGARRAY,DELRSLT
        . SET TMGARRAY(FNAME)=""
        . SET DELRSLT=$$DEL^%ZISH(TMGPATH,"TMGARRAY")
        . IF DELRSLT=0 DO  QUIT
        . . SET RESULT="-1^Unable to delete file: "_TMGPATH_FNAME
        . KILL TMGARRAY
        . NEW FNAME2 SET FNAME2=FNAME
        . SET $PIECE(FNAME2,",",$LENGTH(FNAME2,"."))="ABS"
        . SET TMGARRAY(FNAME2)=""
        . SET DELRSLT=$$DEL^%ZISH(TMGPATH,"TMGARRAY") ;"Ingnore results.  Thumbnail not always present
        . NEW DIK SET DIK="^MAG(2005,"
        . NEW DA SET DA=TMGIEN
        . DO ^DIK  ;"Kill Record in 2005
        ELSE  IF TMGMODE=2 DO  GOTO:(+RESULT'>0) DIDN  ;"Retract mode
        . NEW TMGFDA,TMGMSG,TMGIENS
        . SET TMGIENS=TMGIEN_","
        . SET TMGFDA(2005,TMGIENS,30)="`"_+DUZ
        . SET TMGFDA(2005,TMGIENS,30.1)="NOW"
        . SET TMGFDA(2005,TMGIENS,30.2)=TMGREASON
        . SET TMGFDA(2005,TMGIENS,18)="@"
        . ;"NOTE: Fld 17 already holds IEN of linked 8925 document
        . DO FILE^DIE("EKT","TMGFDA","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) DO
        . . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        DO  ;"Do this for both DELETE and RETRACT modes.
        . NEW DIK SET DIK="^TIU(8925.91,"
        . NEW DA SET DA=TMGPTR
        . DO ^DIK  ;"Kill record in 8925.91
        ;
DIDN    QUIT
        ;
UNRETRACT(RESULT,TMGIEN) ;
        ;"Purpose: to reverse retraction process from DELIMAGE above.
        ;"Input: RESULT -- an OUT Parameter. (See results below)
        ;"       TMGIEN -- the IEN in the IMAGE file (2005) to remove
        ;"Output: RESULT="1^Success"   or "-1^Some Failure Message"  <-- SET up as SINGLE VALUE type in RPC BROKER
        SET TMGIEN=$GET(TMGIEN)
        IF +TMGIEN'>0 DO  GOTO URDN
        . SET RESULT="-1^Invalid IEN supplied: "_TMGIEN
        SET TMGIEN=+TMGIEN
        NEW TIUPTR SET TIUPTR=+$PIECE($GET(^MAG(2005,TMGIEN,2)),"^",7)
        IF TIUPTR'>0 DO  GOTO URDN
        . SET RESULT="-1^Record 2005 doesn't hold link to TIU DOCUMENT in field 17"
        NEW TMGFDA,TMGFDA,TMGIENS
        ;"-- Recreate TIU EXTERNAL DATA LINK record
        KILL TMGFDA
        SET TMGIENS="+1,"
        SET TMGFDA(8925.91,TMGIENS,.01)=TIUPTR
        SET TMGFDA(8925.91,TMGIENS,.02)=TMGIEN
        DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO URDN
        . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        NEW TIUIMGPTR SET TIUIMGPTR=+$GET(TMGIEN(1))
        IF TIUIMGPTR'>0 DO  GOTO URDN
        . SET RESULT="-1^Unable to locate recreated TIU EXTERNAL DATA LINK record"
        ;"-- remove DELETED info from IMAGE record --
        NEW TMGFDA,TMGFDA,TMGIENS
        SET TMGIENS=TMGIEN_","
        SET TMGFDA(2005,TMGIENS,30)="@"
        SET TMGFDA(2005,TMGIENS,30.1)="@"
        SET TMGFDA(2005,TMGIENS,30.2)="@"
        SET TMGFDA(2005,TMGIENS,18)=TIUIMGPTR
        DO FILE^DIE("EKT","TMGFDA","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO URDN
        . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
URDN    QUIT
        ;
GETPWDS(RESULT,DUZ) ;
        ;"Purpose: Recover user's Verify Code, Access Code, and ESig
        ;"************************************************************************
        ;"NOTE: This function would allow a programmer to steal the identity.
        ;"      THIS IS A CRIME.  This function must only be used in a responsible
        ;"      manner.
        ;"************************************************************************
        ;"Input: RESULT -- PASS BY REFERENCE, AN OUT PARAMETER
        ;"       DUZ -- the IEN in the NEW PERSON file to recover information
        ;"Results: none
        ;"Output: RESULT(0)=1^OK, or -1^Error Message
        ;"        RESULT(1)=AccessCodeInPlainText
        ;"        RESULT(2)=VerifyCodeInPlainText
        ;"        RESULT(3)=ESigCodeInPlainText
        ;"        RESULT(4)=ProperUserWarning.
        NEW VERHASH,ACCHASH,ESIG
        SET DUZ=$GET(DUZ)
        IF +DUZ'>0 DO  QUIT
        . SET RESULT(0)="-1^Invalid DUZ passed.  Got: "_DUZ
        SET DUZ=+DUZ
        IF $DATA(^VA(200,DUZ))=0 DO  QUIT
        . SET RESULT(0)="-1^RecOrd in NEW PERSON file does not exist for #"_DUZ
        ;
        SET VERHASH=$PIECE($GET(^VA(200,DUZ,.1)),"^",2)
        SET ACCHASH=$PIECE($GET(^VA(200,DUZ,0)),"^",3)
        SET ESIG=$PIECE($GET(^VA(200,DUZ,20)),"^",4)

        SET RESULT(1)=$$UNHASH^TMGMISC(ACCHASH)
        SET RESULT(2)=$$UNHASH^TMGMISC(VERHASH)
        SET RESULT(3)=ESIG
        SET RESULT(4)="NOTE: It is a CRIME to use this information to steal a users identity!"
        SET RESULT(0)="1^OK"
        QUIT
        ;
GETRTMNU(TMGOUT) ;"
        ;"Purpose: To return a root menu (OPTIONS) for USER
        ;"Input:  TMGOUT -- PASS BY REFERENCE, AN PARAMETER.  Format as in GETMENU below
        NEW IEN
        SET IEN=+$PIECE($GET(^VA(200,+DUZ,201)),"^",1)
        IF IEN'>0 DO  GOTO GRMDN
        . SET TMGOUT(0)="-1^^^User does not have an assigned PRIMARY MENU OPTION"
        NEW NAME SET NAME=$PIECE($GET(^DIC(19,IEN,0)),"^",1)
        DO GETMENU(.TMGOUT,NAME)
        NEW SUBIEN SET SUBIEN=0
        FOR  SET SUBIEN=$ORDER(^VA(200,+DUZ,203,SUBIEN)) QUIT:(+SUBIEN'>0)  DO
        . NEW SECMENU SET SECMENU=$GET(^VA(200,+DUZ,203,SUBIEN,0))
        . SET NAME=$PIECE($GET(^DIC(19,SUBIEN,0)),"^",1)
        . NEW TEMP
        . DO GETMENU(.TEMP,NAME)
        . IF +TEMP(0)'=1 QUIT  ;"1=submenu
        . NEW LINECT SET LINECT=$ORDER(TMGOUT(""),-1)+1
        . NEW I SET I=0
        . FOR  SET I=$ORDER(TEMP(I)) QUIT:(+I'>0)  DO
        . . NEW LINE SET LINE=$GET(TEMP(I))
        . . IF (LINE="")!(+LINE'>0) QUIT
        . . SET TMGOUT(LINECT)=LINE SET LINECT=LINECT+1
GRMDN   QUIT
        ;
GETMENU(TMGOUT,NAME) ;
        ;"Purpose: To return a listing of menu by name
        ;"Input:  TMGOUT -- PASS BY REFERENC, AN PARAMETER.  Format in OUTPUT
        ;"        NAME -- SINGLE VALUE.  The name of the menu to get
        ;"Output: TMGOUT(0)= #^MenuText^NAME^Info
        ;"                      #=-1 IF error.  Info is error message
        ;"                      #=0 IF locked.  Info is locking key
        ;"                      #=1 IF submenu. Info not used
        ;"                      #=2 IF action.  Info IEN of OPTION to run.   //was mumps code to run action.
        ;"        TMGOUT(n)=T^Line 1 of Description.
        ;"        TMGOUT(n)=T^Line 2 of Description.
        ;"        TMGOUT(n)=T^Line 3 of Description.
        ;"        TMGOUT(n)=T^...
        ;"        TMGOUT(n)=#^MenuText^NAME^info
        ;"Results: none
        ;
        NEW IEN,DIC,X,Y,LOCKNAME
        NEW LINECT SET LINECT=1
        SET DIC=19,DIC(0)="X"
        SET X=$GET(NAME)
        IF X="" DO  GOTO GMDN
        . SET TMGOUT(0)="-1^Menu name not provided."
        DO ^DIC
        IF +Y'>0 DO  GOTO GMDN
        . SET TMGOUT(0)="-1^Unable to find menu option: ["_NAME_"]"
        SET IEN=+Y
        SET TMGOUT(0)=$$GET1MNU(IEN)
        NEW I SET I=0
        ;"Get Menu description
        FOR  SET I=$ORDER(^DIC(19,IEN,1,I)) QUIT:(+I'>0)  DO
        . SET TMGOUT(LINECT)="T^"_$GET(^DIC(19,IEN,1,I,0)),LINECT=LINECT+1
        IF +TMGOUT(0)=1 DO  ;"Get sub menu items
        . NEW TMPARRY
        . ;"NEW HASABBREV SET HASABBREV=0
        . SET I=0
        . FOR  SET I=$ORDER(^DIC(19,IEN,10,I)) QUIT:(+I'>0)  DO
        . . NEW ZN SET ZN=$GET(^DIC(19,IEN,10,I,0))
        . . NEW SUBIEN SET SUBIEN=+ZN
        . . NEW ABBREV SET ABBREV=$PIECE(ZN,"^",2)
        . . NEW ORDER SET ORDER=$PIECE(ZN,"^",3)
        . . NEW ONEENTRY SET ONEENTRY=$$GET1MNU(SUBIEN)
        . . IF +ONEENTRY=-1 QUIT  ;"Don't send back disabled menus
        . . NEW MENUTXT SET MENUTXT=$PIECE(ONEENTRY,"^",2)
        . . NEW SORTVALUE
        . . IF ORDER'="" SET SORTVALUE=$$RJ^XLFSTR(ORDER,5,"0") ;" E.g. change 5 --> 00005 so 5 and 50 don't sort together
        . . ELSE  IF ABBREV'="" DO
        . . . SET SORTVALUE="@^"_ABBREV
        . . . SET $PIECE(ONEENTRY,"^",2)=ABBREV_" -- "_MENUTXT
        . . ELSE  SET SORTVALUE=MENUTXT
        . . ;"SET HASABBREV=HASABBREV!(ABBREV'="")  ;"Keep track IF any entry has an abbreviation
        . . SET TMPARRY(SORTVALUE,MENUTXT)=ONEENTRY
        . SET I=0
        . FOR  SET I=$ORDER(TMPARRY(I)) QUIT:(I="")  DO
        . . SET NAME=""
        . . FOR  SET NAME=$ORDER(TMPARRY(I,NAME)) QUIT:(NAME="")  DO
        . . . NEW VAL SET VAL=$GET(TMPARRY(I,NAME))
        . . . ;"IF 'HASABBREV DO
        . . . ;". NEW NAME SET NAME=$$TRIM^XLFSTR($PIECE(VAL,"^",2))  ;"If there are no abbreviations, then remove all leading spaces.
        . . . ;". SET $PIECE(VAL,"^",2)=NAME
        . . . SET TMGOUT(LINECT)=VAL,LINECT=LINECT+1
GMDN    QUIT
        ;
GET1MNU(IEN,SORTVALUE,HASABBREV) ;
        ;"Purpose: Get entry info for one option
        ;"Input: IEN -- IEN in file 19
        ;"Results: #^MenuText^NAME^Info
        ;"            #=-1 IF error.  Info is error message
        ;"            #=0 IF locked.  Info is locking key
        ;"            #=1 IF submenu. Info not used
        ;"            #=2 IF action.  Info IEN of OPTION to run.   //was mumps code to run action.
        NEW LOCKNAME,ZN,RESULT
        SET RESULT="-1^Unknown error" ;"Default
        SET ZN=$GET(^DIC(19,IEN,0))
        SET NAME=$PIECE(ZN,"^",1)
        NEW MENUTXT SET MENUTXT=$PIECE(ZN,"^",2)
        SET LOCKNAME=$PIECE(ZN,"^",6)
        IF $$LOCKED(LOCKNAME) DO  GOTO G1MDN
        . SET RESULT="0^"_MENUTXT_"^"_NAME_"^Locked with key: "_LOCKNAME
        IF $$DISALLOW(IEN) DO  GOTO G1MDN
        . SET RESULT="0^"_MENUTXT_"^"_NAME_"^Disallowed in FM file 22712"
        NEW TYPE SET TYPE=$PIECE(ZN,"^",4)
        NEW LINECT SET LINECT=1
        IF (TYPE="M") DO  GOTO G1MDN
        . SET RESULT="1^"_MENUTXT_"^"_NAME_"^"
        ELSE  IF (TYPE="A") DO  GOTO G1MDN
        . NEW ACTION SET ACTION=$GET(^DIC(19,IEN,20))
        . IF ACTION="" DO  GOTO G1MDN
        . . SET RESULT="-1^"_MENUTXT_"^"_NAME_"^ACTION OPTION does not have an ENTRY ACTION"
        . ;"SET RESULT="2^"_MENUTXT_"^"_NAME_"^"_ACTION
        . SET RESULT="2^"_MENUTXT_"^"_NAME_"^"_IEN
        ELSE  IF (TYPE="R") DO  GOTO G1MDN
        . NEW ACTION SET ACTION=$GET(^DIC(19,IEN,25))
        . IF ACTION="" DO  GOTO G1MDN
        . . SET RESULT="-1^"_MENUTXT_"^"_NAME_"^RUN ROUTINE OPTION does not have a ROUTINE"
        . ELSE  SET ACTION="DO ^"_ACTION
        . ;"SET RESULT="2^"_MENUTXT_"^"_NAME_"^"_ACTION
        . SET RESULT="2^"_MENUTXT_"^"_NAME_"^"_IEN
        ELSE  DO  GOTO G1MDN
        . SET RESULT="-1^"_MENUTXT_"^"_NAME_"^OPTION is not of ACTION, RUN ROUTINE, or MENU type"
G1MDN   QUIT RESULT
        ;
        ;
LOCKED(TMGKEY) ;
        ;"Purpose: To return IF current user (DUZ) is missing SECURITY KEY assigned to them
        IF $GET(TMGKEY)="" QUIT 0
        NEW OUT
        DO HASKEY^ORWU(.OUT,TMGKEY)
        QUIT (OUT=0)
        ;
        ;
LOCKDIEN(KEYIEN) ;
        ;"Purpose: To return IF current user (DUZ) is missing SECURITY KEY (BY IEN) assigned to them
        NEW KEYNAME
        SET KEYNAME=$PIECE($GET(^DIC(19.1,KEYIEN,0)),"^",1)
        QUIT $$LOCKED(KEYNAME)
        ;
        ;
DISALLOW(IEN19) ;
        ;"Purpose: To determine IF menu option is on list of DISALLOWED menu options
        ;"         i.e. in file TMG CONSOLE SUBGTM OPTION SETTINGS (22712), option is not
        ;"         prohibited.
        ;"Input:  IEN19 -- IEN in file 19 (OPTION) file, to be checked.
        ;"Results: 1 IF NOT TO BE RUN. or 0 IF OK
        NEW RESULT SET RESULT=0
        NEW IEN SET IEN=0
        FOR  SET IEN=$ORDER(^TMG(22712,"B",IEN19,IEN)) QUIT:(+IEN'>0)  DO
        . IF $PIECE($GET(^TMG(22712,IEN,3)),"^",1)'="Y" DO   ;"If no Y, then DON'T RUN
        . . NEW KEYIEN SET KEYIEN=+$PIECE($GET(^TMG(22712,IEN,0)),"^",2)
        . . IF (KEYIEN>0)&'$$LOCKDIEN(KEYIEN) QUIT   ;"If override key is present, then don't restrict
        . . SET RESULT=1
        QUIT RESULT
        ;
        ;
CANRUN(IEN19,USER) ;
        ;"Purpose: To determine IF menu OPTION is allowed for user to run.  I.e.
        ;"         part of the allowed menu tree for user.
        ;"Input:  IEN19 -- IEN in file 19 (OPTION) file, to be checked.
        ;"        USER -- OPTIONAL.  The user to check option for.  Default is DUZ (current user)
        ;"Results: 1 IF MAY be run. or 0 IF not allowed
        ;
        NEW RESULT SET RESULT=1
        ;"do DEBUGLOG^TMGKERN3($name(^TMG("TMP","RPC","CTRLSGTM","GTMIN")),"NOTE: in CANRUN")
        SET USER=+$GET(USER)
        IF USER'>0 SET USER=+$GET(DUZ)
        ;"do DEBUGLOG^TMGKERN3($name(^TMG("TMP","RPC","CTRLSGTM","GTMIN")),"NOTE: USER="_USER)
        IF USER'>0 GOTO CRDN
        NEW ALLOWEDMENUS
        ;
        ;"Check Primary Menu
        SET ALLOWEDMENUS(+$GET(^VA(200,USER,201)))=""
        ;"Check Secondary Menus
        NEW IEN SET IEN=0
        FOR  SET IEN=+$ORDER(^VA(200,USER,203,IEN,0)) QUIT:(+IEN'>0)  DO
        . SET ALLOWEDMENUS(IEN)=""
        ;
        NEW DEPTH
        ;"do DEBUGLOG^TMGKERN3($name(^TMG("TMP","RPC","CTRLSGTM","GTMIN")),"NOTE: STARTING CHKOPTN")
        SET RESULT=$$CHKOPTN(IEN19,.ALLOWEDMENUS,.DEPTH)
CRDN    QUIT RESULT
        ;
CHKOPTN(IEN19,ALLOWEDMENUS,DEPTH)
        ;"Purpose: To determine IF 1 menu matches SET of allowed menus, or if
        ;"         any of it's parent menus match
        ;"Input: IEN19 -- the IEN in OPTION file to consider
        ;"       ALLOWEDMENUS -- An array of allowed menues.  Format:
        ;"              ALLOWEDMENUS(IEN)=""
        ;"       DEPTH -- PASS BY REFERENCE.  This is a variable to ensure there
        ;"                are no endless loops IF parent path is circular.
        ;"                If parent not found after 100 loops, then gives up.
        ;"Result: 1 IF match found, or 0 IF not.
        ;"do DEBUGLOG^TMGKERN3($name(^TMG("TMP","RPC","CTRLSGTM","GTMIN")),"NOTE: IN CHKOPTN.  IEN19="_IEN19)
        NEW RESULT SET RESULT=0
        SET DEPTH=+$GET(DEPTH)+1
        IF DEPTH>100 GOTO COMDN
        IF $DATA(ALLOWEDMENUS(IEN19)) SET RESULT=1 GOTO COMDN
        NEW IEN SET IEN=0
        FOR  SET IEN=$ORDER(^DIC(19,"AD",IEN19,IEN)) QUIT:(+IEN'>0)!RESULT  DO
        . SET RESULT=$$CHKOPTN(IEN,.ALLOWEDMENUS,.DEPTH)
COMDN   QUIT RESULT

