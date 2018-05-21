TMGRPC1G ;TMG/kst-RPC Functions ;9/22/15
         ;;1.0;TMG-LIB;**1**;09/28/10
 ;
 ;"TMG RPC FUNCTIONS especially related to CPRS
 ;"  Universal Images.
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
 ;"GETMIMGS(TMGOUT) - Returns all images in the 22701.5 file that are shared or owned by the user
 ;"ADDIMG(RESULT,NAME,IMAGEIEN,PRIVATE) - Adds an image to 22701.5 file
 ;"IMGINFO(TMGOUT,IMAGEIEN) - Returns an image's info  based on the MAGIEN
 ;"GETIMGWNAME(TMGOUT,NAME) -returns image info as stored in ^MAG(2005) from a universal image name
 ;"GETMAGIEN(TMGOUT,NAME) - returns the Mag IEN from a given Universal Image Name
 ;"DELMIMAGE(TMGOUT,IEN) - delete the Universal Image By IEN
 ;"MEDIAHTM(TMGRESULT,FILEARR) - wrap approproate files in HTML
 ;"
 ;"=======================================================================
 ;"Dependancies:
 ;" MAGGTII, DIE
 ;"=======================================================================
 ;
GETMIMGS(TMGOUT)  ;
        ;"Purpose: returns all images in the TMG IMAGE MULTI-USE (22701.5) FILE.
        ;"Input: TMGOUT(An out parameter)
        ;"Output: Array containing images.  Format: Code^Name^MAGIEN
        ;"         CODE: 0 IF shared or 1 IF private
        ;"         TMGOUT(0)=1^success, or -1^Error
        ;"         e.g. TMGOUT(1)=1^ImageName^1234
        ;"         e.g. TMGOUT(2)=0^Picture2^3456
        ;"Results: none
        ;
        NEW TMGI,IMAGE,LOCKED,OWNER,TEMP,RESULT
        SET RESULT="-1^NO IMAGES FOUND"
        SET TMGI=0
        FOR  SET TMGI=$ORDER(^TMG(22701.5,TMGI)) QUIT:(+TMGI'>0)  DO
        . SET IMAGE=$GET(^TMG(22701.5,TMGI,0))
        . SET LOCKED=$PIECE(IMAGE,"^",3)
        . IF LOCKED="Y" DO
        . . SET OWNER=$PIECE(IMAGE,"^",4)
        . . IF OWNER=$GET(DUZ) DO
        . . . SET TEMP("1^"_$PIECE(IMAGE,"^",1)_"^"_$PIECE(IMAGE,"^",2))=""
        . ELSE  DO
        . . SET TEMP("0^"_$PIECE(IMAGE,"^",1)_"^"_$PIECE(IMAGE,"^",2))=""
        SET TMGI=""
        NEW COUNT
        SET COUNT=1
        FOR  SET TMGI=$ORDER(TEMP(TMGI)) QUIT:(TMGI="")  DO
        . SET RESULT="1^SUCCESS"
        . SET TMGOUT(COUNT)=TMGI
        . SET COUNT=COUNT+1
        SET TMGOUT(0)=RESULT
        QUIT
        ;
ADDIMG(RESULT,NAME,IMAGEIEN,PRIVATE)   ;
        ;"Purpose: saves an image to the TMG IMAGE MULTI-USE (22701.5) FILE.
        ;"If PRIVATE is Y, the owner will be SET as DUZ
        ;"
        ;"Input: RESULT(An Out Parameter)
        ;"       NAME - Name of image to be added
        ;"       IMAGEIEN - MAGIEN to associate with NEW image
        ;"       PRIVATE - Y if image is private or blank
        ;"Results: none
        ;"Output: RESULT either "1^Successful" or "-1^message"
        ;
        NEW TMGFDA,TMGMSG,TMGIENS
        SET RESULT="1^SUCCESSFUL"
        SET DUZ=+$GET(DUZ)
        IF DUZ'>0 DO  GOTO Q1
        . SET RESULT="-1^DUZ NOT DEFINED"
        SET NAME=$GET(NAME)
        IF NAME="" DO  GOTO Q1
        . SET RESULT="-1^NAME NOT PROVIDED"
        SET PRIVATE=$$UP^XLFSTR($GET(PRIVATE))
        IF PRIVATE="Y" DO
        . NEW USERNAME SET USERNAME=$PIECE($GET(^VA(200,DUZ,0)),"^",1)
        . SET NAME=NAME_" [OWNED BY] "_USERNAME
        ;"IF NAME IN USE, SET ERROR
        IF $D(^TMG(22701.5,"B",$E(NAME,1,64))) DO  GOTO Q1
        . SET RESULT="-1^NAME IN USE"
        SET IMAGEIEN=$GET(IMAGEIEN)
        IF IMAGEIEN'>0 DO  GOTO Q1
        . SET RESULT="-1^Numeric IMAGEIEN not provide.  Got: ["_IMAGEIEN_"]"
        IF $D(^MAG(2005,IMAGEIEN))'>0 DO  GOTO Q1
        . SET RESULT="-1^IMAGE CANNOT BE LOCATED."
        SET TMGFDA(22701.5,"+1,",.01)=NAME
        SET TMGFDA(22701.5,"+1,",1)=IMAGEIEN
        SET TMGFDA(22701.5,"+1,",2)=PRIVATE
        SET TMGFDA(22701.5,"+1,",3)=DUZ
        DO UPDATE^DIE("","TMGFDA","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO Q1
        . SET TMGRESULT="-1^Filing Error Occured:"+$GET(TMGMSG("DIERR",1,"TEXT",1))
        ;"Change the IMAGE file such that it is not linked to a particular patient
        KILL TMGFDA
        SET TMGFDA(2005,IMAGEIEN_",",.01)="MULTI IMAGE: "_NAME
        SET TMGFDA(2005,IMAGEIEN_",",5)="@"
        DO FILE^DIE("E","TMGFDA","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO Q1
        . SET TMGRESULT="-1^Filing Error Occured:"+$GET(TMGMSG("DIERR",1,"TEXT",1))
Q1      QUIT
       ;
IMGINFO(TMGOUT,IMAGEIEN)    ;
        ;"Purpose: returns the image info for a given image IEN
        ;"Input: TMGOUT (An Out Parameter)
        ;"       IMAGEIEN - MAGIEN of image
        ;"Output: Image data as stored in ^MAG(2005)
        ;
        NEW MAGXX SET MAGXX=IMAGEIEN
        D INFO^MAGGTII
        IF $D(MAGFILE) DO
        . SET TMGOUT="1^"_MAGFILE
        ELSE  DO
        . SET TMGOUT="0^NO IMAGE DATA FOUND"
        ;"CHECK MAGGTII FOR POSSIBLE ERROR OUTPUTS TO TEST FOR
        QUIT
        ;
GETIMGWN(TMGOUT,NAME) ;
        ;"Purpose: returns image info as stored in ^MAG(2005) from a universal image name
        ;"Input: TMGOUT (An out parameter)
        ;"       NAME - Name as stored in ^TMG(22701.5)
        ;"Output: 1^Image information as stored in ^MAG(2005) or -1^message
        ;
        NEW MAGIEN
        DO GETMAGIEN(.MAGIEN,NAME)
        IF MAGIEN=0 DO  GOTO GINDN
        . SET TMGOUT="-1^NO IMAGE FOUND"
        DO IMGINFO(.TMGOUT,MAGIEN)
        IF +TMGOUT<1 DO  GOTO GINDN
        . SET TMGOUT="-1^NO IMAGE INFORMATION FOUND."
        ;"SET TMGOUT="1^"_TMGOUT
GINDN   QUIT
        ;
GETMAGIEN(TMGOUT,NAME) ;
        ;"Purpose: returns the Mag IEN from a given Universal Image Name
        ;"Input: TMGOUT (An out parameter)
        ;"       NAME - Name as stored in ^TMG(22701.5)
        ;"Results: MAGIEN of the universal image
        ;"Output: MAGIEN of the universal image or 0 IF not found
        ;
        NEW IEN S IEN=0
        SET IEN=+$ORDER(^TMG(22701.5,"B",NAME,IEN))
        SET TMGOUT=+$PIECE($GET(^TMG(22701.5,IEN,0)),"^",2)
        QUIT
        ;
DELMIMAGE(TMGOUT,IEN) ;
        ;"Purpose: delete the Universal Image By IEN
        ;"Input: TMGOUT (An out parameter)
        ;"       IEN - MAGIEN to be deleted from the universal image file (^TMG(22701.5)
        ;
        QUIT
        ;
MEDIAHTM(TMGRESULT,FILEARR) ;
        ;"Purpose: wrap the provided file(s) in HTML for display in a
        ;"         browser on the client side
        ;"         N+-OTE: This RPC is stubbed for handling multiple files
        ;"               but for the time being, only handles one.  
        ;"Input: TMGRESULT - Out variable
        ;"       FILEARR - Array of files
        ;"Output: TMGRESULT(0)= "-1^Error" or "0" (for no HTML wrapping)
        ;"                      or "1^HTML INCLUDED" (for HTML code to
        ;"                      following in result array)
        ;"Result: none
        SET TMGRESULT(0)=0
        NEW FILENAME,FILEEXT,LINENUM,IEN,EXTIEN,FOUND,TEMPIEN
        SET FOUND=0,TEMPIEN=0
        SET FILENAME=$GET(FILEARR(0))
        IF FILENAME="" GOTO MHDN
        SET FILEEXT=$$UP^XLFSTR($PIECE(FILENAME,".",2))
        SET EXTIEN=0
        FOR  SET TEMPIEN=$ORDER(^TMG(22722,TEMPIEN)) QUIT:(TEMPIEN'>0)!(FOUND)  DO
        . SET EXTIEN=$ORDER(^TMG(22722,TEMPIEN,1,"B",FILEEXT,0))
        . IF EXTIEN>0 SET FOUND=1,IEN=TEMPIEN,TMGRESULT(0)=1
        IF EXTIEN'>0 GOTO MHDN ;"Quit if ext not specified
        SET LINENUM=0
        NEW LINETEXT
        FOR  SET LINENUM=$ORDER(^TMG(22722,IEN,2,LINENUM)) QUIT:LINENUM'>0  DO
        . SET LINETEXT=$GET(^TMG(22722,IEN,2,LINENUM,0))
        . IF LINETEXT["|FILE#1|" DO
        . . SET LINETEXT=$P(LINETEXT,"|FILE#1|",1)_FILENAME_$P(LINETEXT,"|FILE#1|",2)
        . SET TMGRESULT(LINENUM)=LINETEXT
MHDN    QUIT
        ;"
