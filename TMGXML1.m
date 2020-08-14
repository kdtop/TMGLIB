TMGXML1 ;TMG/kst/XML Exporter -- Testing code ;10/26/14
         ;;1.0;TMG-LIB;**1**;07/09/05
 ;
 ;"This is a test file for working with XML Documents
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"
START ;
        ;"Kevin Toppenberg, MD  7-9-04
        ;"This is a test file for working with XML Documents
        ;
        NEW Y,PATH,FILE,GBLREF
        KILL ^TMP("KT",$J)
        ;
        SET PATH="/home/kdtop"
        ;
        WRITE "-----------------------",!
        READ "Enter FILENAME:",FILE:$GET(DTIME,3600)
        WRITE !
        IF FILE="^" QUIT
        IF FILE="" SET FILE="XMLSample#2.xml" WRITE "Using default: ",FILE,!
        ;
        SET GBLREF="^TMP(""KT"","_$J_",0)"
        SET Y=$$FTG^%ZISH(PATH,FILE,GBLREF,3)
        IF 'Y WRITE "error opening file.",! QUIT
        ;
        ;"do EN^MXMLTEST($NAME(^TMP("KT",$J)),"V")
        ;
        WRITE "---------------------------",!
        WRITE "Part #2",!,!
        ;
        NEW FILENAME
        SET FILENAME=PATH_"/"_FILE
        ;WRITE "FILENAME=",FILENAME,!
        ;
        NEW FNARRAY
        SET FNARRAY="Array of Callback Functions"
        SET FNARRAY("STARTDOCUMENT")="STARTDOC^TMGXML1"
        SET FNARRAY("ENDDOCUMENT")="ENDDOC^TMGXML1"
        SET FNARRAY("DOCTYPE")="DOCTYPE^TMGXML1"
        SET FNARRAY("STARTELEMENT")="STARTEL^TMGXML1"
        SET FNARRAY("ENDELEMENT")="ENDEL^TMGXML1"
        SET FNARRAY("CHARACTERS")="CHARS^TMGXML1"
        ;
        ;WRITE "Here is FNARRAY",!
        ;
        WRITE "Calling EN^MXMLPRSE",!
        DO EN^MXMLPRSE($NAME(^TMP("KT",$J)),.FNARRAY)
        ;do EN^MXMLPRSE(FILENAME,.FNARRAY)
        WRITE "Done calling EN^MXMLPRSE",!
        ;
        WRITE "---------------------------",!
        WRITE "Part #3",!,!
        ;
        NEW PARSEHANDLE
        WRITE "Calling EN^MXMLDOM",!
        SET PARSEHANDLE=$$EN^MXMLDOM($NAME(^TMP("KT",$J)),"V")
        WRITE "Done calling EN^MXMLDOM",!
        WRITE "Handle=",PARSEHANDLE,!
        ;
        DO SHOWNODE(1,0)
        DO LISTCHLD(1,1)
        ;
        DO DELETE^MXMLDOM(PARSEHANDLE)
        ;
        KILL ^TMP("KT",$J)
        WRITE "********************",!
        WRITE "Quiting normally",!
        ;
        QUIT
        ;
 ;"-------------------------------------------------------------
        ;
STARTDOC ;
        WRITE "##Starting Document Processing##",!
        QUIT
        ;
ENDDOC  ;
        WRITE "##End of Document Processing##",!
        QUIT
        ;
DOCTYPE(ROOT,PUBID,SYSID) ;
        WRITE "Doctype encountered.",!
        WRITE "ROOT=",ROOT,!
        WRITE "PUBID=",PUBID,!
        WRITE "SYSID=",SYSID,!
        QUIT
        ;
STARTEL(NAME,ATTRLIST)  ;"START ELEMENT
        WRITE "Attrib:"
        WRITE "Name=",NAME,!
        IF $DATA(ATTRLIST) DO
        . WRITE "AttrList:"
        . DO ZWRITE^TMGZWR("ATTRLIST")
        QUIT
        ;
ENDEL(NAME) ;
        WRITE "End Attrib:"
        WRITE NAME,!
        QUIT
        ;
CHARS(TEXT) ;
        WRITE "TEXT:",TEXT,!
        QUIT
        ;
 ;"-------------------------------------------------------------
        ;
LISTCHLD(NODE,INDENTNUM)  ;"LIST CHILDREN
        NEW CHILDNODE
        SET CHILDNODE=$$CHILD^MXMLDOM(PARSEHANDLE,NODE,0)
        IF CHILDNODE=0 QUIT
        ;
        NEW LOOP
        FOR LOOP=1:1 DO  IF CHILDNODE=0 QUIT
        . DO SHOWNODE(CHILDNODE,INDENTNUM)
        . DO LISTCHLD(CHILDNODE,INDENTNUM+1)
        . SET CHILDNODE=$$CHILD^MXMLDOM(PARSEHANDLE,NODE,CHILDNODE)
        QUIT
        ;
SHOWNODE(NODE,INDENTNUM) ;
        NEW NODETEXT
        NEW ATTRIBTEXT
        DO INDENT(INDENTNUM)
        WRITE $$NAME^MXMLDOM(PARSEHANDLE,NODE),!
        IF $$CMNT^MXMLDOM(PARSEHANDLE,NODE,$NAME(NODETEXT)) DO
        . DO INDENT(INDENTNUM)
        . WRITE "  Comment: ",NODETEXT(1),!
        IF $$TEXT^MXMLDOM(PARSEHANDLE,NODE,$NAME(NODETEXT)) DO
        . DO INDENT(INDENTNUM)
        . WRITE "  '",NODETEXT(1),"'",!
        SET ATTRIBTEXT=$$ATTRIB^MXMLDOM(PARSEHANDLE,NODE)
        IF $DATA(ATTRIBTEXT),ATTRIBTEXT'="" DO
        . DO INDENT(INDENTNUM)
        . WRITE "  Attrib: ",ATTRIBTEXT,"="
        . WRITE $$VALUE^MXMLDOM(PARSEHANDLE,NODE,ATTRIBTEXT),!
        QUIT
        ;
INDENT(INDENTNUM)  ;
        NEW IDX FOR IDX=1:1:INDENTNUM WRITE "  "
        QUIT
        ;
