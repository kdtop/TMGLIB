TMGXMLP  ;TMG/kst/Detail XML Parsing functions ;10/26/14
         ;;1.0;TMG-LIB;**1**;01/01/05
 ;
 ;"TMG DETAIL XML PARSE FUNCTIONS
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
 ;"DTLPARSE ;
 ;"LISTCHLD(NODE,INDENTNUM)
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"STARTDOC
 ;"ENDDOC
 ;"DOCTYPE(ROOT,PUBID,SYSID)
 ;"STARTEL(NAME,ATTRLIST)
 ;"ENDEL(NAME)
 ;"Chars(TEXT)
 ;"PARSEERR(ERR)
 ;"SHOWNODE(NODE,INDENTNUM)
 ;"INDENT(INDENTNUM)
 ;"=================================================================
 ;
DTLPARSE(REFARR)  ;"DETAIL PARSE
        ;"Purpose: To DO a detailed deconstruction of parse to detect errors.
        ;"Input: REFARR -- PASS BY NAME.  OPTIONAL.
        ;"       Default = $NAME(^TMP("TMG",$J))
        ;"Output: Puts info to debug stream
        ;"results: none
        NEW Y,PATH,FILE,GBLREF
        NEW DBINDENT SET DBINDENT=0  ;"//kt 10/26/24
        SET REFARR=$GET(REFARR,$NAME(^TMP("TMG",$J)))
        NEW FNARRAY SET FNARRAY="Array of Callback Functions"
        SET FNARRAY("ERROR")="PARSEERR^TMGXMLP"
        SET FNARRAY("STARTDOCUMENT")="STARTDOC^TMGXMLP"
        SET FNARRAY("ENDDOCUMENT")="ENDDOC^TMGXMLP"
        SET FNARRAY("DOCTYPE")="DOCTYPE^TMGXMLP"
        SET FNARRAY("STARTELEMENT")="STARTEL^TMGXMLP"
        SET FNARRAY("ENDELEMENT")="ENDEL^TMGXMLP"
        SET FNARRAY("CHARACTERS")="Chars^TMGXMLP"
        DO DEBUGMSG^TMGDEBU4(DBINDENT,"This part of the program will DO a detailed parse analysis...")
        DO DEBUGMSG^TMGDEBU4(DBINDENT,"Hopefully this will reveal the parsing error.")
        DO DEBUGMSG^TMGDEBU4(DBINDENT,"Here is loaded file that is being parsed:")
        NEW % SET %=1
        WRITE "View array containing XML data"
        DO YN^DICN WRITE !
        IF %=1 DO ZWRITE^TMGZWR(REFARR)
        IF %=-1 GOTO DTLPDN
        ;
        DO DEBUGMSG^TMGDEBU4(DBINDENT,"Calling EN^MXMLPRSE (a detailed parse assessment.)")
        DO DEBUGMSG^TMGDEBU4(DBINDENT,"As each element of the XML file is encountered, it will be listed.")
        DO EN^MXMLPRSE(REFARR,.FNARRAY,"V")
        DO DEBUGMSG^TMGDEBU4(DBINDENT,"Done calling EN^MXMLPRSE")
        DO DEBUGMSG^TMGDEBU4(DBINDENT,"---------------------------")
        ;
DTLPDN  QUIT
        ;
STARTDOC  ;
        DO DEBUGMSG^TMGDEBU4(DBINDENT,"---------------------")
        DO DEBUGMSG^TMGDEBU4(DBINDENT,"##Starting Document Processing##")
        QUIT
        ;
ENDDOC  ;
        DO DEBUGMSG^TMGDEBU4(DBINDENT,"---------------------")
        DO DEBUGMSG^TMGDEBU4(DBINDENT,"##End of Document Processing##")
        QUIT
        ;
DOCTYPE(ROOT,PUBID,SYSID)
        DO DEBUGMSG^TMGDEBU4(DBINDENT,"--------------------")
        DO DEBUGMSG^TMGDEBU4(DBINDENT,"Doctype encountered.")
        DO DEBUGMSG^TMGDEBU4(DBINDENT,"ROOT=",$GET(ROOT))
        DO DEBUGMSG^TMGDEBU4(DBINDENT,"PUBID=",$GET(PUBID))
        DO DEBUGMSG^TMGDEBU4(DBINDENT,"SYSID=",$GET(SYSID))
        QUIT
        ;
STARTEL(NAME,ATTRLIST)  ;
        DO DEBUGMSG^TMGDEBU4(DBINDENT,"---------------------")
        DO DEBUGMSG^TMGDEBU4(DBINDENT,"Start Element:")
        DO DEBUGMSG^TMGDEBU4(DBINDENT,"Name=",$GET(NAME))
        IF $DATA(ATTRLIST) DO
        . DO DEBUGMSG^TMGDEBU4(DBINDENT,"AttrList:")
        . DO ZWRITE^TMGZWR("ATTRLIST")
        QUIT
        ;
ENDEL(NAME)  ;
        DO DEBUGMSG^TMGDEBU4(DBINDENT,"---------------------")
        DO DEBUGMSG^TMGDEBU4(DBINDENT,"End Element:")
        DO DEBUGMSG^TMGDEBU4(DBINDENT,"Name=",$GET(NAME))
        QUIT

Chars(TEXT)
        DO DEBUGMSG^TMGDEBU4(DBINDENT,"---------------------")
        DO DEBUGMSG^TMGDEBU4(DBINDENT,"TEXT=",$GET(TEXT))
        QUIT
        ;
PARSEERR(ERR)  ;
        IF ($DATA(ERR)=0)!($DATA(ERR)=1) GOTO PEDN
        IF ERR("SEV")<2 GOTO PEDN  ;"Bypass all but error>=2
        ;
        NEW HEADER,TEXT,PTRLINE
        ;
        SET HEADER="<!> XML "
        IF ERR("SEV")=0 SET HEADER=HEADER_"Processing Warning"
        IF ERR("SEV")=1 SET HEADER=HEADER_"Validation Error"
        IF ERR("SEV")=2 SET HEADER=HEADER_"Conformance Error"
        ;
        SET TEXT="Document Line #"_ERR("LIN")_" Position #"_ERR("POS")_"\n"
        SET TEXT=TEXT_"'"_ERR("XML")_"'\n\n"
        ;
        SET PTRLINE=""
        NEW IDX FOR IDX=1:1:ERR("POS")-1 SET PTRLINE=PTRLINE_"."
        SET TEXT=TEXT_PTRLINE_"^"_"\n"
        ;
        SET PTRLINE=""
        FOR IDX=1:1:ERR("POS")-1 SET PTRLINE=PTRLINE_" "
        SET TEXT=TEXT_PTRLINE_"|"_"\n"
        ;
        IF ERR("MSG")'="" DO
        . SET TEXT=TEXT_ERR("MSG")_"\n"
        SET TEXT=TEXT_"\nErroneous token: '"_ERR("ARG")_"'\n"
        ;
        DO PROGBAR^TMGUSRI2(HEADER,TEXT,75)
        ;
        IF $DATA(REFARR) DO
        . DO ZWRITE^TMGZWR(REFARR,$GET(ERR("LIN")))
        ;
PEDN    QUIT
        ;
 ;"-------------------------------------------------------------
        ;
LISTCHLD(NODE,INDENTNUM) ;
        NEW CHILDNODE
        SET CHILDNODE=$$CHILD^MXMLDOM(ParseHandle,NODE,0)
        IF CHILDNODE=0 QUIT
        NEW IDX FOR IDX=1:1 DO  IF CHILDNODE=0 QUIT
        . DO SHOWNODE(CHILDNODE,INDENTNUM)
        . DO LISTCHLD(CHILDNODE,INDENTNUM+1)
        . SET CHILDNODE=$$CHILD^MXMLDOM(ParseHandle,NODE,CHILDNODE)
        QUIT
        ;
SHOWNODE(NODE,INDENTNUM)  ;
        NEW NODETEXT
        NEW ATTRIBTEXT
        ;
        DO INDENT(INDENTNUM)
        WRITE $$NAME^MXMLDOM(ParseHandle,NODE),!
        IF $$CMNT^MXMLDOM(ParseHandle,NODE,$NAME(NODETEXT)) DO
        . DO INDENT(INDENTNUM)
        . WRITE "  Comment: ",NODETEXT(1),!
        IF $$TEXT^MXMLDOM(ParseHandle,NODE,$NAME(NODETEXT)) DO
        . DO INDENT(INDENTNUM)
        . WRITE "  '",NODETEXT(1),"'",!
        SET ATTRIBTEXT=$$ATTRIB^MXMLDOM(ParseHandle,NODE)
        IF $DATA(ATTRIBTEXT),ATTRIBTEXT'="" DO
        . DO INDENT(INDENTNUM)
        . WRITE "  Attrib: ",ATTRIBTEXT,"="
        . WRITE $$VALUE^MXMLDOM(ParseHandle,NODE,ATTRIBTEXT),!
        ;
        QUIT
        ;
INDENT(INDENTNUM)  ;
        for i=1:1:INDENTNUM WRITE "  "
        QUIT
        ;
