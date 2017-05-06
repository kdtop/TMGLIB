TMGKERN7 ;TMG/kst/Barcode Interface to Linux ;6/23/15
         ;;1.0;TMG-LIB;**1**;6/23/15
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
 ;"$$MAKEBC^TMGBARC(Message) -- shell to Linux, to create barcode image.
 ;"$$READBC^TMGBARC(FPathName) -- shell to Linux, to read barcode image.
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"Test
 ;"=======================================================================
 ;
MAKEBC(Message,Option)
        ;"Purpose: to shell out to Linux, and call dmtxwrite to create
        ;"         the barcode image.
        ;"Input: Message -- the text to be encode in the barcode
        ;"                 Note: the barcode function may limit what type of chars
        ;"                 can be put it, and how long. (explore more later...)
        ;"       Option -- PASS BY REFERENCE.  Array that may hold optional
        ;"                 settings, as follows:  Default value is "png"
        ;"                 Option("IMAGE TYPE")="jpg" <-- output is desired in .jpg format
        ;"Output: Returns filename, on host file system, of created image, or "" IF error
        ;"
        ;"NOTE: dmtxwrite must be installed on linux host.
        ;"      I found source code here:
        ;"      http://sourceforge.net/projects/libdmtx/
        ;"      After installing (./configure --> make --> make install etc), I
        ;"        copied dmtxread and dmtxwrite, which were found in the
        ;"        (installdir)/util/dmtxread/.libs and (installdir)/util/dmtxwrite/.libs
        ;"        folders, into a folder on the system path.  I chose /usr/bin/
        ;"      Also, to achieve compile of above, I had to install required libs.
        ;"      See notes included with source code.
        NEW result SET result=""
        NEW msgFNAME SET msgFNAME=$$UNIQUE^%ZISUTL("/tmp/msg.txt")
        NEW imageFNAME SET imageFNAME=$$UNIQUE^%ZISUTL("/tmp/barcode.png")
        NEW imageType SET imageType=$$LOW^XLFSTR($GET(OPTION("IMAGE TYPE"),"png"))
        SET ^TMG("TMP","MAKEBC^TMGKERN7",$H)="START"     

        ;"Write Message to host file .txt file
        NEW %ZIS,IOP,POP
        SET %ZIS("HFSNAME")=msgFNAME
        SET IOP="HFS"
        DO ^%ZIS  ;"standard device call
        IF POP GOTO MBCDone
        use IO
        WRITE Message
        DO ^%ZISC  ;"close device

        ;"Setup and launch linux command to execute dmtxwrite
        ;"Note: dmtxwrite only makes .png format images
        NEW CmdStr
        SET CmdStr="cat "_msgFNAME_" | dmtxwrite -o "_imageFNAME
        do
        . NEW $ETRAP,$ZTRAP
        . SET $ETRAP="S $ECODE="""""
        . zsystem CmdStr  ;"Launch command

        ;"get result of execution. (low byte only)  -- IF wanted
        NEW CmdResult
        SET CmdResult=$ZSYSTEM&255
        IF CmdResult'=0 GOTO MBCDone

        ;"No error, so successful

        ;"Convert to specified image type, IF needed
        IF imageType'="png" SET imageFNAME=$$Convert^TMGKERNL(imageFNAME,imageType)

        SET result=imageFNAME

        ;"Delete Message .txt file
        NEW FNAME,FPath,FileSpec
        DO SPLITFPN^TMGIOUTL(msgFNAME,.FPath,.FNAME,"/")
        SET FileSpec(FNAME)=""
        NEW temp SET temp=$$DEL^%ZISH(FPath,$name(FileSpec))
        SET ^TMG("TMP","MAKEBC^TMGKERN7",$H)="END"

MBCDone
        QUIT result


READBC(FPathName)
        ;"Purpose: to shell out to Linux, and call dmtxread to read a
        ;"         barcode image.
        ;"Input: FPathName -- valid host file name of image to be decoded.
        ;"Output: Returns message stored in barcode, or "" IF problem
        ;"
        ;"NOTE: dmtxread must be installed on linux host.
        ;"      I found source code here:
        ;"      http://sourceforge.net/projects/libdmtx/
        ;"      After installing (./configure --> make --> make install), I
        ;"        copied dmtxread and dmtxwrite, which were found in the
        ;"        (installdir)/util/dmtxread/.libs and (installdir)/util/dmtxwrite/.libs
        ;"        folders, into a folder on the system path.  I chose /usr/bin/
        ;"      Also, to achieve compile of above, I had to install required libs.
        ;"      See notes included with source code.

        NEW result SET result=""

        NEW msgFNAME SET msgFNAME=$$UNIQUE^%ZISUTL("/tmp/msg.txt")
        NEW FNAME,FPath,FileSpec
        DO SPLITFPN^TMGIOUTL(msgFNAME,.FPath,.FNAME,"/")
        SET FileSpec(FNAME)=""

        ;"Setup and launch linux command to execute dmtxwrite
        NEW CmdStr
        SET CmdStr="dmtxread -g 32 "_FPathName_" >> "_msgFNAME
        ;"Add a leading filler character to prevent possible read error
        ;"from an empty file.
        SET CmdStr="echo ""#"" > "_msgFNAME_";"_CmdStr
        do
        . zsystem CmdStr  ;"Launch command

        ;"get result of execution. (low byte only)  -- IF wanted
        NEW CmdResult SET CmdResult=$ZSYSTEM&255
        IF CmdResult'=0 GOTO RBCDone

        SET ^TMG("TMP","BARCODE","LOG")="5d"  ;"temp

        NEW resultArray
        IF $$FTG^%ZISH(FPath,FNAME,"resultArray(0)",1)=0 DO  GOTO RBCDone
        ;"First line should be just '#' (filler character)
        ;"Second line should hold answer
        SET result=$GET(resultArray(1))

        MERGE ^TMG("TMP","BARCODE","RESULT FILE")=resultArray

        ;"Read Message from host file .txt file
        ;"do OPEN^%ZISH("FILE1",FPath,FNAME,"R")
        ;"if POP GOTO RBCDone
        ;"do
        ;". use IO
        ;". read result
        ;"do CLOSE^%ZISH("FILE1")
        ;"new resultArray
        ;"do CleaveToArray^TMGSTUTL(result,$CHAR(10),.resultArray)
        ;"for now I am only going to pay attention to first line...
        ;"set result=$GET(resultArray(1))
        ;"set result=$EXTRACT(result,2,999) ;"remove 1st character which is filler '#'


        ;"Delete Message .txt file
        ;"TEMP!!! DELETE LATER...
        NEW temp SET temp=$$DEL^%ZISH(FPath,$name(FileSpec))

RBCDone
        QUIT result


Test
        NEW msg,file
        SET msg="This is a test message"
        WRITE "Creating barcode...",!
        SET file=$$MAKEBC(msg)
        WRITE "reading barcode....",!,!
        WRITE $$READBC(file),!

        ;"delete temp image file
        NEW FNAME,FPath,FileSpec
        DO SPLITFPN^TMGIOUTL(file,.FPath,.FNAME,"/")
        SET FileSpec(FNAME)=""
        NEW temp SET temp=$$DEL^%ZISH(FPath,"FileSpec")

        QUIT
