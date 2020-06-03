TMGXMLT ;TMG/kst/XML Tools ;10/26/14
         ;;1.0;TMG-LIB;**1**;02/09/08
 ;
 ;"TMG XML EXPORT/IMPORT TOOL FUNCTIONS
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
 ;"LOADFILE^TMGXMLT(Path,FILENAME,Option)  -- Load XML file and parse
 ;"$$GDESCNOD^TMGXMLT(XMLHANDLE,ParentNODE,NAME,CHILDNODE) -- Find a node that matches Name that is a descendant of NODE
 ;"$$GSIBDSCN^TMGXMLT(XMLHANDLE,NODE,NAME) -- Find a node that matches Name that is a sibling of NODE
 ;"GETNNAME^TMGXMLT(XMLHANDLE,NODE) -- Get name of node indicated by Node handle
 ;"GETNTEXT^TMGXMLT(XMLHANDLE,NODE,TEXTARRAY) -- Get text associated with node
 ;"GET1NTXT^TMGXMLT(XMLHANDLE,NODE,TEXTARRAY) -- Get 1st line of text associated with node
 ;"GETJNTXT^TMGXMLT(XMLHANDLE,NODE,TEXTARRAY) -- Get all text of node, joined into 1 long string
 ;"GTATRVAL^TMGXMLT(XMLHANDLE,NODE,ATTRIB) - Get attrib value for ATTRIB
 ;"GETPARMS^TMGXMLT(XMLHANDLE,NODE,PARAMARRAY,SUBSCALLBACK) - Get all the attribs and values into a Parameter array
 ;"WRITEARR^TMGXMLT(REF,NODELABEL,ID,FLAGS,INDENTS,INCINDENT) -- Write out data dictionary file in XML format
 ;"READARR^TMGXMLT(XMLHANDLE,NODE,ARRAY) -- read an Array (as written by WRITEARR) from XML file back into Array
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"LOADPARM(XMLHANDLE,DBNAME,PARAMARRAY,COUNT,SUBSCALLBACK) common code to call from GETPARMS
 ;"SHOWXMND(NODENUM) show a parsed node
 ;"WRITNODE(REF,NODE,INDENTS,FLAGS) A reentrant function to WRITE out one node of the data dictionary.
 ;"READNODE(XMLHANDLE,PARENTNODE,CURREF)
 ;
 ;"=======================================================================
 ;"=======================================================================
 
LOADFILE(Path,FILENAME,Option)
        ;"Purpose: To load the file and check for XML validity
        ;"Input: Path -- path of file to load
        ;"       FILENAME, -- name of file to load
        ;"       Option -- OPTIONAL
        ;"Returns: 0 IF fails, otherwise XML file handle.
        ;"Note: EN^MXMLDOM can load the file directly... change this code later.
        NEW REFDEST SET REFDEST=$NAME(^TMG("TMP","XML_IMPORT",$J))
        NEW FILEHANDLE
        SET XMLHANDLE=0
        NEW REFDEST1 SET REFDEST1=$NAME(@REFDEST@(1))
        ;
        SET FILEHANDLE=$$FTG^%ZISH(Path,FILENAME,REFDEST1,$QLENGTH(REFDEST1))
        IF FILEHANDLE=0 DO  GOTO QLDN
        . NEW ERRFOUND
        . DO SHOWERR^TMGDEBU2(.ERRFOUND,"Error opening file. Path="_Path_", FILENAME="_FILENAME)
        DO HANDLOVF(REFDEST)
        ;
        WRITE "Parsing XML File.  Please wait.  Large files can take > 15 minutes . . ."
        SET XMLHANDLE=$$EN^MXMLDOM(REFDEST,"")
        WRITE !
        IF XMLHANDLE=0 DO
        . NEW ERRMSG,ERRFOUND
        . SET ERRMSG="Error parsing XML document.\n\n"
        . SET ERRMSG=ERRMSG_"Now analyzing XML file to determine problem...\n"
        . DO SHOWERR^TMGDEBU2(.ERRFOUND,ERRMSG)
        . DO DTLPARSE^TMGXMLP(REFDEST)
QLDN    KILL @REFDEST
        QUIT XMLHANDLE
        ;
HANDLOVF(REFDEST)  ;"handle overflow. 
        ;"Purpose: to try to handle overflow (OVF) nodes after loading file,
        ;"         If line length is too long because of leading (left side)
        ;"         space padding.
        NEW LINE SET LINE=""
        FOR  SET LINE=$ORDER(@REFDEST@(LINE)) QUIT:(LINE="")  DO
        . IF $DATA(@REFDEST@(LINE,"OVF"))>0 DO
        . . ;"WRITE "OVF on line ",LINE,!
        . . ;"do ZWRITE^TMGZWR(REFDEST,LINE)
        . . NEW S1,S2
        . . SET S1=$GET(@REFDEST@(LINE))
        . . SET S2=$GET(@REFDEST@(LINE,"OVF",1))  ;"NOTE:  <--- only handles 1 extra line.  Expand later?
        . . KILL @REFDEST@(LINE,"OVF",1)
        . . SET S1=$$TRIM^XLFSTR(S1)_S2
        . . ;"set S1=$$TrimL^TMGSTUTL(S1)_S2
        . . IF $LENGTH(S1)>255 DO  QUIT
        . . . ;"WRITE "Overflow Line Present.  LTrim was not enough...",!
        . . . SET S2=$PIECE(S1,">",2,999)
        . . . SET S1=$PIECE(S1,">",1)_">"
        . . . SET @REFDEST@(LINE)=S1
        . . . SET @REFDEST@(LINE_".5")=S2
        . . . IF $LENGTH(S2)>255 DO
        . . . . ;"WRITE "Overflow Line Present.  LTrim & line split was not enough..."
        . . . . ;"WRITE "Line ",LINE,".5 is ",$LENGTH(S2)," characters long.",!
        . . ELSE  DO
        . . . SET @REFDEST@(LINE)=S1
        ;
        QUIT
        ;
GDESCNOD(XMLHANDLE,PARENTNODE,NAME,CHILDNODE)  ;"GET DESCRIPTION NODE
        ;"Purpose: Find a node that matches Name that is a descendant of Node
        ;"Input: XMLHANDLE -- the handle, as created by $$EN^MXMLDOM
        ;"       PARENTNODE: a node handle specifying parent
        ;"       NAME: the name to search for
        ;"       CHILDNODE: OPTIONAL.  If provided, then result will follow
        ;"                  CHILDNODE (i.e. start search at CHILDNODE)
        ;"Note: If <Yellow> node is sought, NAME should be 'Yellow', not '<Yellow>''
        ;"Note: NAME to be searched for is NOT CASE SENSITIVE
        ;"Results: nodehandle, or 0 IF not found.
        SET CHILDNODE=+$GET(CHILDNODE)
GDNL    SET CHILDNODE=$$CHILD^MXMLDOM(XMLHANDLE,PARENTNODE,CHILDNODE)
        IF CHILDNODE=0 GOTO GDNQ
        IF $$GETNNAME(XMLHANDLE,CHILDNODE)=$$UP^XLFSTR(NAME) GOTO GDNQ
        GOTO GDNL
GDNQ    QUIT CHILDNODE
        ;
GSIBDSCN(XMLHANDLE,NODE,NAME)  ;"GET SIBLING DESCRIPTION NODE
        ;"Purpose: Find a node that matches Name, starting search among siblings with node
        ;"Input: XMLHANDLE -- the handle, as created by $$EN^MXMLDOM
        ;"       NODE: a node handle specifying the node to start searching from (AFTER)
        ;"       NAME: the name to search for
        ;"Note: If <Yellow> node is sought, NAME should be 'Yellow', not '<Yellow>''
        ;"Note: NAME comparison is NOT CASE SENSITIVE.
        ;"Results: nodehandle, or 0 IF not found.
        NEW SIBNODE SET SIBNODE=NODE
GSDNL   SET SIBNODE=$$SIBLING^MXMLDOM(XMLHANDLE,SIBNODE)
        IF $$GETNNAME(XMLHANDLE,CHILDNODE)=$$UP^XLFSTR(NAME) GOTO GSDNQ
        IF SIBNODE>0 GOTO GSDNL
GSDNQ   QUIT SIBNODE
        ;
GETNNAME(XMLHANDLE,NODE)  ;
        ;"Purpose: Get name of node indicated by Node handle
        ;"Input: XMLHANDLE -- the handle, as created by $$EN^MXMLDOM
        ;"       NODE: node handle
        ;"Output: returns name associated with node (in UPPERCASE)
        NEW RESULT SET RESULT=$$NAME^MXMLDOM(XMLHANDLE,NODE)
        SET RESULT=$$UP^XLFSTR(RESULT)
        QUIT RESULT
        ;
GETNTEXT(XMLHANDLE,NODE,TEXTARRAY)  ;
        ;"Purpose: Get text associated with node
        ;"Input: XMLHANDLE -- the handle, as created by $$EN^MXMLDOM
        ;"       NODE: node handle
        ;"       TEXTARRAY: a reference to global array to hold text array
        ;"Output: returns is text in Text is valid
        ;"Results: 1=value 0=not valid
        ;"Note: IF Text is not valid, Text is SET to " "
        NEW VALID SET VALID=$$TEXT^MXMLDOM(XMLHANDLE,NODE,$NAME(TEXTARRAY))
        IF 'VALID SET TEXTARRAY=" "
        QUIT VALID
        ;
GET1NTXT(XMLHANDLE,NODE,TEXTARRAY)  ;
        ;"Purpose: To get 1st line of text associated with node
        ;"      2/13/08 -- modified to 'Return 1 line of text' (all
        ;"Input: XMLHANDLE -- the handle, as created by $$EN^MXMLDOM
        ;"       NODE: node handle
        ;"       TEXTARRAY: [OPTIONAL] If given, should be passed by reference
        ;"                Will contain entire array, as passed back from XML functions
        ;"Output: returns text associated with node, or "" IF none found
        NEW RESULT
        IF $$GETNTEXT(XMLHANDLE,NODE,.TEXTARRAY)>0 DO
        . SET RESULT=$GET(TEXTARRAY(1))
        . SET RESULT=$$Trim^TMGSTUTL(RESULT)
        ELSE  DO
        . SET RESULT=""
        QUIT RESULT
        ;
GETJNTXT(XMLHANDLE,NODE,TEXTARRAY)   ;
        ;"Purpose: To get all text associated with node, joined into 1 long string
        ;"Input: XMLHANDLE -- the handle, as created by $$EN^MXMLDOM
        ;"       NODE: node handle
        ;"       TEXTARRAY: [OPTIONAL] If given, should be passed by reference
        ;"                Will contain entire array, as passed back from XML functions
        ;"Output: returns text associated with node, or "" IF none found
        NEW RESULT
        IF $$GETNTEXT(XMLHANDLE,NODE,.TEXTARRAY)>0 DO
        . SET RESULT=$$WPToStr^TMGSTUTL("TEXTARRAY","")
        . ;"set RESULT=$$Trim^TMGSTUTL(RESULT)
        ELSE  DO
        . SET RESULT=""
        QUIT RESULT
        ;
GET1LTEXT(XMLHANDLE,NODE,TEXTARRAY)  ;
        ;"Purpose: To get 1 line of text associated with node when nodes are in this format:
        ;"         <Rec label="xyz" >   <--- NODE points to this
        ;"           <LINE>1317</LINE>     <--- 1317 to be returned. <LINE> could be <AnyName>
        ;"         </Rec>
        ;"Input: XMLHANDLE -- the handle, as created by $$EN^MXMLDOM
        ;"       NODE: node handle
        ;"       TEXTARRAY: [OPTIONAL] If given, should be passed by reference
        ;"                Will contain entire array, as passed back from XML functions
        ;"Output: returns text associated with node, or "" IF none found
        NEW RESULT SET RESULT=""
        NEW NODELINE SET NODELINE=$$CHILD^MXMLDOM(XMLHANDLE,NODE)
        IF NODELINE>0 DO
        . NEW VALID SET VALID=$$TEXT^MXMLDOM(XMLHANDLE,NODELINE,"TEXTARRAY")
        . IF 'VALID QUIT
        . SET RESULT=$$TRIM^XLFSTR($GET(TEXTARRAY(1)))
        QUIT RESULT
        ;
GTATRVAL(XMLHANDLE,NODE,ATTRIB)  ;"GET ATTRIB VALUE
        ;"Purpose: Get attrib value for ATTRIB
        ;"Input: XMLHANDLE -- the handle, as created by $$EN^MXMLDOM
        ;"       NODE: node handle
        ;"       ATTRIB: name of attribute
        ;"Results: returns value associated with attribute, or " " IF not found
        NEW RESULT SET RESULT=" "
        NEW USERATTRIB
        IF ATTRIB=" " GOTO GAVDN
        ;"Note: because user-given attrib may be in lower case, I will have to
        ;"      first scan attribs to find right one.  Then get value
        SET ATTRIB=$$UP^XLFSTR(ATTRIB)
        SET USERATTRIB=$$ATTRIB^MXMLDOM(XMLHANDLE,NODE)
GAVL1   IF ATTRIB=$$UP^XLFSTR(USERATTRIB) GOTO GAVGET  
        SET USERATTRIB=$$ATTRIB^MXMLDOM(XMLHANDLE,NODE,USERATTRIB)
        IF $DATA(USERATTRIB)=0 GOTO GAVDN
        IF USERATTRIB="" GOTO GAVDN
        GOTO GAVL1
GAVGET  SET RESULT=$$VALUE^MXMLDOM(XMLHANDLE,NODE,USERATTRIB)
        IF $DATA(RESULT)=0 SET RESULT=" "
        IF RESULT="" SET RESULT=" "
GAVDN   QUIT RESULT
        ;
GETPARMS(XMLHANDLE,NODE,PARAMARRAY,SUBSCALLBACK)  ;
        ;"Purpose: To get all the attribs and values into a Parameter array
        ;"Input: XMLHANDLE -- the handle, as created by $$EN^MXMLDOM
        ;"       NODE -- the node to parse
        ;"       PARAMARRAY -- MUST BE PASSED BY REFERENCE  to accept values back
        ;"            When passed back, it will have this structure:
        ;"                PARAMARRAY(1,"Name")=<attrib name>
        ;"                PARAMARRAY(1,"Name","UpperCase")=<UPPER CASE OF attrib name>
        ;"                PARAMARRAY(1,"VALUE")=<value of attrib>
        ;"                PARAMARRAY(1,"VALUE","UpperCase")=<UPPER CASE OF value of attrib>
        ;"                PARAMARRAY(2,"Name")=<attrib name>
        ;"                PARAMARRAY(2,"Name","UpperCase")=<UPPER CASE OF attrib name>
        ;"                PARAMARRAY(2,"VALUE")=<value of attrib>
        ;"                PARAMARRAY(2,"VALUE","UpperCase")=<UPPER CASE OF value of attrib>
        ;"            e.g.:
        ;"                PARAMARRAY(1,"Name")="id"
        ;"                PARAMARRAY(1,"Name","UpperCase")="ID"
        ;"                PARAMARRAY(1,"VALUE")="office"
        ;"                PARAMARRAY(1,"VALUE","UpperCase")="OFFICE"
        ;"        ALSO I will additionally put the data in this format:
        ;"                PARAMARRAY("ID")="office"
        ;"                PARAMARRAY("ID","UpperCase")="OFFICE"
        ;"        ALSO I will add the text from the node into PARAMARRAY("TEXT")
        ;"            e.g. <Comment>
        ;"                  Some Text
        ;"                    And some more...
        ;"               </Comment>
        ;"                would result in:
        ;"                PARAMARRAY("TEXT",1)="Some Text"
        ;"                PARAMARRAY("TEXT",2)="And some more"
        ;"       SUBSCALLBACK -- name of function, used to call to see IF data subsition (i.e. turning
        ;"                a data value into something else).  OPTIONAL
        ;"                e.g. "CheckSubstituteData^TMGXINST".  Function must be declared
        ;"                in this format: CheckSubstituteData(value)
        ;"Result: 1=ok to continue,  0=ABORT
        NEW COUNT SET COUNT=1
        NEW VALUE
        NEW RESULT SET RESULT=1
        NEW ATTRIB SET ATTRIB=$$ATTRIB^MXMLDOM(XMLHANDLE,NODE)
        IF $DATA(ATTRIB)=0 GOTO GPARDN
        IF ATTRIB="" GOTO GPARDN
        SET RESULT=$$LOADPARM(XMLHANDLE,ATTRIB,.PARAMARRAY,COUNT,.SUBSCALLBACK)
        SET COUNT=COUNT+1
GPARL1  SET ATTRIB=$$ATTRIB^MXMLDOM(XMLHANDLE,NODE,ATTRIB)
        IF $DATA(ATTRIB)=0 GOTO GPARDN
        IF ATTRIB="" GOTO GPARDN
        SET RESULT=$$LOADPARM(XMLHANDLE,ATTRIB,.PARAMARRAY,COUNT,.SUBSCALLBACK)
        SET COUNT=COUNT+1
        GOTO GPARL1
GPARDN  QUIT RESULT
        ;
LOADPARM(XMLHANDLE,DBNAME,PARAMARRAY,COUNT,SUBSCALLBACK)  ;
        ;"Purpose: Provide common code to call from GETPARMS
        ;"Input: XMLHANDLE -- the handle, as created by $$EN^MXMLDOM
        ;"       NODE -- the node to parse
        ;"       PARAMARRAY -- MUST BE PASSED BY REFERENCE  to accept values back
        ;"       COUNT -- Current Count
        ;"       SUBSCALLBACK -- name of function, used to call to see IF data subsition (i.e. turning
        ;"                a data value into something else).  OPTIONAL
        ;"                e.g. "CheckSubstituteData^TMGXINST".  Function must be declared
        ;"                in this format: CheckSubstituteData(value)
        ;"RESULT: 1=ok to continue,  0=ABORT
        NEW VALUE
        NEW RESULT SET RESULT=1
        SET PARAMARRAY(COUNT,"Name")=DBNAME
        SET PARAMARRAY(COUNT,"Name","UpperCase")=$$UP^XLFSTR(DBNAME)
        SET VALUE=$$VALUE^MXMLDOM(XMLHANDLE,NODE,DBNAME)
        IF $GET(SUBSCALLBACK)'="" DO
        . NEW FN SET FN="SET RESULT=$$"_SUBSCALLBACK_"(.VALUE)"
        . XECUTE FN
        . IF RESULT=0 DO SHOWERR^TMGDEBU2(.ERRFOUND,"Error getting parameter substitution: "_DBNAME)
        SET PARAMARRAY(COUNT,"VALUE")=VALUE
        SET PARAMARRAY(COUNT,"VALUE","UpperCase")=$$UP^XLFSTR(VALUE)
        SET PARAMARRAY($$UP^XLFSTR(DBNAME))=VALUE
        SET PARAMARRAY($$UP^XLFSTR(DBNAME),"UpperCase")=$$UP^XLFSTR(VALUE)
        QUIT RESULT
        ;
SHOWXMND(NODENUM)  ;"SHOW XML NODE
        ;"Purpose: To show a parsed node
        NEW LINEI
        IF NODENUM'>0 DO  GOTO SXNDN
        DO ZWRITE^TMGZWR("^TMP(""MXMLDOM"","_$J_",1,"_NODENUM_")")
        IF $DATA(^TMP("MXMLDOM",$J,1,NODENUM))=0 DO  GOTO SXNDN
        . DO ZWRITE^TMGZWR("^TMP(""MXMLDOM"","_$J_",1,"_NODENUM_")")
        IF $DATA(^TMP("MXMLDOM",$J,1,NODENUM,"A")) DO
        . SET LINEI=$Order(^TMP("MXMLDOM",$J,1,NODENUM,"A",""))
        . FOR  DO  QUIT:(LINEI="")
        . . SET LINEI=$Order(^TMP("MXMLDOM",$J,1,NODENUM,"A",LINEI))
SXNDN   QUIT
        ;
WRITEARR(REF,NODELABEL,ID,FLAGS,INDENTS,INCINDENT,PROGRESSFN)  ;
        ;"Scope: PUBLIC
        ;"Purpose: to WRITE out an array in XML format
        ;"Input: REF -- the REF to WRITE out, e.g. $NAME(^DD(FILENUM))
        ;"       NODElabel -- the label of the node,
        ;"         e.g. DataDictionary, --> <DataDictionary>
        ;"       ID -- An id for <Label id=xxx>
        ;"       FLAGS -- OPTIONAL -- flags as declared above.  Only "i" used here
        ;"       INDENTS -- OPTIONAL -- current string to WRITE to indent line.
        ;"       INCINDENT -- OPTIONAL -- the amount of space to indent by, e.g. "  "
        ;"       PROGRESSFN -- OPTIONAL -- M code to exec as a progress indicator
        ;"Results: none
        SET INCINDENT=$GET(INCINDENT,"  ")
        SET INDENTS=$GET(INDENTS)
        IF $GET(FLAGS)["i" WRITE INDENTS
        WRITE "<",NODELABEL," id=""",$$SYMENC^MXMLUTL(ID),""">",!
        DO WRITNODE(REF,REF,INDENTS_INCINDENT,.FLAGS,.PROGRESSFN)
        IF $GET(FLAGS)["i" WRITE INDENTS
        WRITE "</",NODELABEL,">",!
        QUIT
        ;
WRITNODE(REF,NODE,INDENTS,FLAGS,PROGRESSFN,INCVAR)  ;
        ;"SCOPE: PRIVATE
        ;"Purpose: A reentrant function to WRITE out one node of the data dictionary.
        ;"Input: REF -- the NAME OF the full referenct to the current node to export (includes Node below)
        ;"       NODE -- the name of just the node to export
        ;"       INDENTS -- The OPTIONAL string to print to indent the text.
        ;"       FLAGS -- FLAGS as declared above.  Only "i" used here.
        ;"       PROGRESSFN -- OPTIONAL -- M code to exec as a progress indicator
        ;"       INCVAR -- OPTIONAL -- a counter that can be referenced by PROGRESSFN
        ;"NOTE: Uses GLOBAL SCOPED INCINDENT variable.  But setting this is OPTIONAL.
        ;"Results: none
        NEW RESULT SET RESULT=0
        SET INCINDENT=$GET(INCINDENT,"  ")
        SET INDENTS=$GET(INDENTS)
        SET INCVAR=+$GET(INCVAR)+1
        IF (INCVAR#10=1),($GET(PROGRESSFN)'="") DO
        . NEW $ETRAP SET $ETRAP="set $ETRAP="""",$ecode="""""
        . XECUTE PROGRESSFN
        NEW OUTS
        IF $GET(FLAGS)["i" WRITE INDENTS
        WRITE "<N id=""",$$SYMENC^MXMLUTL(NODE),""">"
        SET OUTS=$$SYMENC^MXMLUTL($GET(@REF))
        IF OUTS'="" WRITE OUTS
        ELSE  IF $DATA(@REF)#10=1 WRITE """"""
        ;"WRITE !
        NEW NEWLNWRITTEN SET NEWLNWRITTEN=0
        ;
        NEW SUB SET SUB=""
        FOR  SET SUB=$ORDER(@REF@(SUB)) QUIT:(SUB="")  DO
        . IF NEWLNWRITTEN=0 WRITE ! SET NEWLNWRITTEN=1
        . DO WRITNODE($NAME(@REF@(SUB)),SUB,INDENTS_INCINDENT,.FLAGS,.PROGRESSFN,.INCVAR)
        ;
        IF NEWLNWRITTEN,$GET(FLAGS)["i" WRITE INDENTS
        WRITE "</N>",!
        QUIT
        ;
READARR(XMLHANDLE,NODE,ARRAY,PROGRESSFN,INCVAR)  ;
        ;"Purpose: to read an Array (as written by WRITEARR) from XML file back into Array
        ;"Input: XMLHANDLE
        ;"       NODE -- the node number (as used by MXMLDOM code)
        ;"       ARRAY -- PASS BY REFERENCE, the array to get results back into.  Old values Killed
        ;"       PROGRESSFN -- OPTIONAL -- M code to exec as a progress indicator
        ;"       INCVAR -- OPTIONAL -- a counter that can be referenced by PROGRESSFN
        ;"Result: None
        KILL ARRAY
        NEW REFNODE SET REFNODE=$$CHILD^MXMLDOM(XMLHANDLE,NODE)
        IF REFNODE=0 GOTO RADN
        NEW ORIGREF SET ORIGREF=$$GTATRVAL(XMLHANDLE,REFNODE,"id")
        NEW TEMP SET TEMP=$$READNODE(XMLHANDLE,REFNODE,"ARRAY",.PROGRESSFN,.INCVAR)
RADN    QUIT
        ;
READNODE(XMLHANDLE,PARENTNODE,CURREF,PROGRESSFN,INCVAR) ;
        ;"Input: XMLHANDLE
        ;"       PARENTNODE -- the node number (as used by MXMLDOM code)
        ;"       CURREF -- PASS BY NAME.  Reference to array to get results back into.  Old values Killed
        ;"       PROGRESSFN -- OPTIONAL -- M code to exec as a progress indicator
        ;"       INCVAR -- OPTIONAL -- a counter that can be referenced by PROGRESSFN
        ;"Result: 1=Data found, 0=no data found
        SET INCVAR=+$GET(INCVAR)+1
        IF INCVAR#10=1 DO
        . IF $GET(PROGRESSFN)'="" DO
        . . NEW $ETRAP SET $ETRAP="set $ETRAP="""",$ecode="""""
        . . XECUTE PROGRESSFN
        . . WRITE !,PARENTNODE,"(",CURREF,")        ",!  DO CUU^TMGTERM(2)  ;"TEMP
        . . IF $$USRABORT^TMGUSRI2() SET ABORT=1 QUIT
        ;
        NEW RESULT SET RESULT=0
        NEW CURNODE SET CURNODE=0
        NEW ORIGREF SET ORIGREF=CURREF
        FOR  SET CURNODE=$$CHILD^MXMLDOM(XMLHANDLE,PARENTNODE,CURNODE) QUIT:(CURNODE=0)  DO
        . SET RESULT=1
        . NEW ARRAYNODE SET ARRAYNODE=$$GTATRVAL(XMLHANDLE,CURNODE,"id")
        . SET CURREF=$NAME(@ORIGREF@(ARRAYNODE))
        . NEW DATA
        . ;"BELOW ONLY READS 1 LINE PER NODE.. I THINK THIS IS OK...
        . IF $$TEXT^MXMLDOM(XMLHANDLE,CURNODE,"DATA") SET DATA=$GET(DATA(1))
        . SET DATA=$$TRIM^XLFSTR(DATA,"R") ;"I am afraid a L trim of WP docs would mess them up...
        . NEW TEMP SET TEMP=DATA IF TEMP=" " SET TEMP=""
        . IF DATA="""""" SET @CURREF=""
        . ELSE  IF (DATA'="")&(TEMP'="") SET @CURREF=DATA
        . ;"if TEMP'="" WRITE CURREF,"=[",DATA,"]",!   ;"TEMP
        . NEW SUBDATA SET SUBDATA=$$READNODE(XMLHANDLE,CURNODE,CURREF,.PROGRESSFN,.INCVAR)
        . IF (SUBDATA=0)&(DATA="") DO
        . . SET @CURREF=""
        . . ;"WRITE CURREF,"=[""""]",!   ;"TEMP
        QUIT RESULT
        ;
