TMGHTMR1 ;TMG/kst/HTML RPC API help guide; 2/15/14
         ;;1.0;TMG-LIB;**1**;02/15/14

 ;"TMG HTML EXPORT FUNCTION
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"NOTE: This code is intended as a one-off for outputing a reference
 ;"     for CPRS's RPC calls.   
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"DumpNtes(List,FPath,OUTARRAY)
 ;"MakeFNAME(IEN)
 ;"GetCONTEXTRecs(PRECS,CONTEXT)
 ;"W1RPCHLP(IEN,RPCNAME)
 ;"=======================================================================
 ;"Dependencies   (duplicates shown in parenthesies)
 ;"=======================================================================
 ;"^TMGMISC
 ;
MAKESITE(FPath,CONTEXT)
        ;"Purpose: To create an interlinked site with specified notes.
        ;"Input: FPath OPTIONAL -- IF not provided, user will be asked
        ;"              This is the directory where output is to be sent
        ;"      CONTEXT OPTIONAL -- IF not provided, user will be asked
        ;"              This is the name of the search/sort template holding
        ;"              a list of IENs to output
        ;"Output: files are written to file system
        ;"RESULT: none.


        IF $GET(FPath)="" do
        . WRITE !!,"This will create a reference document for CPRS RPC'sas an an interlinked website.",!!
        . read "Enter destination directory path: ",FPath:$GET(DTIME,3600),!
        IF FPath="^" GOTO MSDone

        ;"Create core index.htm
        ;"----------------------
        IF $$OpenIO(FPath,"index.htm")=0 DO  GOTO MSDone
        . WRITE "Error.  Aborting.",!
        NEW OFFSET
        FOR OFFSET=1:1  DO  QUIT:(s["{^}")
        . SET s=$PIECE($TEXT(IndexDat+OFFSET),";;",2)
        . QUIT:(s["{^}")
        . WRITE s,!
        DO ^%ZISC ;" Close the output device

        ;"Create core intro.htm
        ;"----------------------
        IF $$OpenIO(FPath,"intro.htm")=0 DO  GOTO MSDone
        . WRITE "Error.  Aborting.",!
        NEW OFFSET
        FOR OFFSET=1:1  DO  QUIT:(s["{^}")
        . SET s=$PIECE($TEXT(IntroDat+OFFSET),";;",2)
        . QUIT:(s["{^}")
        . WRITE s,!
        DO ^%ZISC ;" Close the output device

        ;"Create individual files with notes.
        ;"-----------------------------------
        NEW OUTARRAY
        DO WriteCONTEXT(.FPath,.CONTEXT,.OUTARRAY)

        ;"Create toc.htm-- the table of contents.
        ;"---------------------------------------
        IF $$OpenIO(FPath,"toc.htm")=0 DO  GOTO MSDone
        . WRITE "Error.  Aborting.",!
        DO MakeTOC(.OUTARRAY)
        DO ^%ZISC ;" Close the output device

        ;"Create style sheet left-style.css
        ;"----------------------
        IF $$OpenIO(FPath,"left-style.css")=0 DO  GOTO MSDone
        . WRITE "Error.  Aborting.",!
        NEW OFFSET
        FOR OFFSET=1:1  DO  QUIT:(s["{^}")
        . SET s=$PIECE($TEXT(LStyleDat+OFFSET),";;",2)
        . QUIT:(s["{^}")
        . WRITE s,!
        DO ^%ZISC ;" Close the output device

        ;"Create style sheet right-style.css
        ;"----------------------
        IF $$OpenIO(FPath,"right-style.css")=0 DO  GOTO MSDone
        . WRITE "Error.  Aborting.",!
        NEW OFFSET
        FOR OFFSET=1:1  DO  QUIT:(s["{^}")
        . SET s=$PIECE($TEXT(RStyleDat+OFFSET),";;",2)
        . QUIT:(s["{^}")
        . WRITE s,!
        DO ^%ZISC ;" Close the output device

        
MSDone
        WRITE "Good bye.",!!
        QUIT

WriteCONTEXT(FPath,CONTEXT,OUTARRAY)
        ;"Purpose: To WRITE out notes listed in CONTEXT to directory FPath
        ;"Input: FPath -- The name of the directory to put the output files to
        ;"      CONTEXT -- OPTIONAL -- the name of a search/sort template that contains
        ;"              list of IENS's to output
        ;"              If not supplied, user will be asked for name.
        ;"      OUTARRAY -- An OUT parameter.  PASS BY REFERENCE
        ;"              An array to receive results of names written.  See WriteList for format
        ;"Output: files are written to directory
        ;"RESULT: none

        NEW PrgsFn SET PrgsFn="DO PROGBAR^TMGUSRI2(TMGCUR,""Progress"",1,TMGMAX,,TMGSTART)"
        NEW LIST,COUNT
        SET COUNT=$$GetCONTEXTRecs("LIST",.CONTEXT)
        IF COUNT>0 DO WriteList(.LIST,FPath,.OUTARRAY,PrgsFn)

        NEW OTHERARR,TEMP
        SET COUNT=$$GetNonCPRSIARPCs(.LIST,.OTHERARR) ;
        IF COUNT>0 DO
        . WRITE !,COUNT," IA-allowed RPC's not in CPRS found",!
        . DO WriteList(.OTHERARR,FPath,.TEMP,PrgsFn)
        MERGE OUTARRAY("NON-CPRS")=TEMP
        
        QUIT


WriteList(List,FPath,OUTARRAY,PrgCallback)
        ;"Purpose: To WRITE out all notes given in List to separate files in FPath
        ;"Input: List -- PASS BY REFERENCE.  A list of IEN's from file 8994 that must be written.
        ;"              Format as follows:
        ;"              List(IEN1)=""
        ;"              List(IEN2)=""
        ;"              List(IEN3)=""
        ;"              List(IEN4)=""
        ;"              ...
        ;"      FPath -- The name of the directory that files should be written to
        ;"              e.g. "/tmp/output/"
        ;"      OUTARRAY -- An OUT parameter.  PASS BY REFERENCE
        ;"              An array to receive results of names written.  Format:
        ;"              OUTARRAY(<IEN>)=Filename1
        ;"              OUTARRAY(<IEN>,RPCNAME)=""
        ;"              OUTARRAY("B",RPCNAME,IEN1)=""
        ;"      PrgCallback: OPTIONAL -- IF supplied, then M code contained in this string
        ;"              will be xecuted periodically, to allow display of a progress bar etc.
        ;"              Note: the following variables with global scope will be declared and
        ;"                      available for use: TMGCUR (current count), TMGMAX (max count),
        ;"                      TMGSTART (the start time
        ;"
        ;"Output: A series of files will be written (or overwritten) to directory
        ;"      Each file will be a TIU DOCUMENT in .htm format.
        ;"      Filename format: lastname_firstname_title_datetime.htm
        ;"RESULT: none
        ;
        KILL OUTARRAY
        NEW TMGMAX SET TMGMAX=0
        NEW TMGSTART SET TMGSTART=$H
        NEW TMGCUR SET TMGCUR=0
        NEW IEN SET IEN="" 
        FOR  SET IEN=$ORDER(List(IEN)) QUIT:(+IEN'>0)  SET TMGMAX=TMGMAX+1
        NEW DELAY SET DELAY=0
        ;
        SET IEN=""
        FOR  SET IEN=$ORDER(List(IEN)) QUIT:(+IEN'>0)  DO
        . SET TMGCUR=TMGCUR+1
        . NEW RPCNAME
        . NEW FNAME SET FNAME=$$MakeFNAME(IEN,.RPCNAME)
        . IF $$OpenIO(FPath,FNAME)'=0 do
        . . DO W1RPCHLP(IEN,.RPCNAME)
        . . DO ^%ZISC ;" Close the output device
        . . SET OUTARRAY(IEN)=FNAME
        . . SET OUTARRAY(IEN,RPCNAME)=""
        . . SET OUTARRAY("B",RPCNAME,IEN)=""
        . IF (DELAY>30),$GET(PrgCallback)'="" DO  ;"update progress bar every 30 cycles
        . . NEW $ETRAP SET $ETRAP="write ""(Invalid M Code!.  Error Trapped.)"" SET $ETRAP="""",$ecode="""""
        . . XECUTE PrgCallback  ;"call the specified progress code.
        . . SET DELAY=0
        . ELSE  SET DELAY=DELAY+1
        QUIT
        ;
OpenIO(FPath,FNAME,NodeDiv)
        ;"Purpose: to open the IO channel such that all writes
        ;"      will go to the file specified.
        ;"Input: FPath -- the path to open file in
        ;"       FNAME -- the name of the file to open
        ;"       NodeDiv -- OPTIONAL (default is "/") -- the path delimiter for OS
        ;"result: 1=OK To continue, 0=error

        NEW RESULT SET RESULT=1
        NEW PFNAME SET PFNAME=FPath
        SET NodeDiv=$GET(NodeDiv,"/")
        NEW ch
        SET ch=$EXTRACT(PFNAME,$LENGTH(PFNAME))
        IF ch'=NodeDiv SET PFNAME=PFNAME_NodeDiv
        SET PFNAME=PFNAME_FNAME

        ;"Select IO channel
        SET %ZIS("HFSNAME")=PFNAME
        SET %ZIS="Q" ;"queing allowed
        SET %ZIS("HFSMODE")="W"  ;"WRITE mode
        SET IOP="HFS"

        DO ^%ZIS  ;"standard device call
        IF POP DO  GOTO OIODone
        . SET RESULT=0

        USE IO

OIODone
        QUIT RESULT


MakeFNAME(IEN,NAME)
        ;"Purpose: To create a filename from REMOTE PROCEDURE
        ;"Input -- IEN.  and IEN from file 8994
        ;"         NAME -- PASS BY REFERENCE, AN OUT PARAMETER.
        ;"RESULT -- the filename
        NEW RESULT SET RESULT=""
        SET NAME=$$GET1^DIQ(8994,IEN_",",.01)
        SET NAME=$TRANSLATE(NAME,"?","")
        SET NAME=$TRANSLATE(NAME,",","-")
        SET NAME=$TRANSLATE(NAME," ","_")
        SET RESULT=NAME_".html"
        QUIT RESULT


GetCONTEXTRecs(PRECS,CONTEXT) ;
        ;"Purpose: to ask user for a search/sort template to inport records from.
        ;"Input -- PRECS -- pointer to (i.e. name of) array to fill
        ;"                      will probably be passed with "Array(12345)"
        ;"         CONTEXT -- OPTIONAL.  Name of template to import.
        ;"              If not supplied, user will be asked for name
        ;"Output: Data is put into PRECS like this:
        ;"              @PRECS@(IEN1)=""  IEN is from file 8994
        ;"              @PRECS@(IEN2)=""
        ;"              @PRECS@(IEN3)=""
        ;"RESULT: Count of records imported
        ;"Note: uses global variable pHeader
        ;
        SET CONTEXT=$GET(CONTEXT,"OR CPRS GUI CHART")
        NEW COUNT SET COUNT=0
        NEW X,Y,DIC
        SET DIC=19,DIC(0)="M"
        SET X=CONTEXT
        DO ^DIC
        IF +Y'>0 GOTO GCTRDN
        NEW IDX SET IDX=0
        FOR  SET IDX=$ORDER(^DIC(19,+Y,"RPC",IDX)) QUIT:(+IDX'>0)  DO
        . NEW IEN SET IEN=$PIECE($GET(^DIC(19,+Y,"RPC",IDX,0)),"^",1)
        . SET @PRECS@(IEN)=""
        . SET COUNT=COUNT+1
        WRITE COUNT," RPC Records selected.",!
GCTRDN  QUIT COUNT
        ;
        ;
GetNonCPRSIARPCs(CPRSARR,OTHERARR) ;
        NEW IEN SET IEN=0
        NEW CT SET CT=0
        FOR  SET IEN=$ORDER(^TMG(22721,"AC",IEN)) QUIT:(+IEN'>0)  DO
        . IF $DATA(CPRSARR(IEN)) QUIT
        . SET OTHERARR(IEN)=""
        . SET CT=CT+1
        QUIT CT
        ;        
MakeTOC(OUTARRAY)
        ;"Purpose: To WRITE toc.htm (the table of contents side panel)
        ;"Input: IEN -- the IEN in file 8925 (TIU DOCUMENT)
        ;"       OUTARRAY -- PASS BY REFERENCE
        ;"              An array with file names written.  Format:
        ;"              OUTARRAY(<IEN>)=Filename1
        ;"              OUTARRAY(<IEN>,RPCNAME)=""
        ;"              OUTARRAY("B",RPCNAME,IEN1)=""
        ;"
        ;"RESULT: none:
        ;
        NEW OFFSET,STR
        FOR OFFSET=1:1  DO  QUIT:(STR["{^}")
        . SET STR=$PIECE($TEXT(TOCHdr+OFFSET),";;",2)
        . QUIT:(STR["{^}")
        . WRITE STR,!
        ;
        NEW RPC SET RPC=""
        FOR  SET RPC=$ORDER(OUTARRAY("B",RPC)) QUIT:(RPC="")  DO
        . NEW IEN SET IEN=$ORDER(OUTARRAY("B",RPC,"")) QUIT:(IEN="")
        . SET FNAME=$GET(OUTARRAY(IEN)) QUIT:FNAME=""
        . IF $ORDER(^TMG(22721,"AC",IEN,0))>0 DO  ;"See if is in TMG INTEGRATION AGREEMENTS file
        . . SET OUTARRAY("ICR",RPC,IEN)=""
        ;
        SET RPC=""
        FOR  SET RPC=$ORDER(OUTARRAY("NON-CPRS","B",RPC)) QUIT:(RPC="")  DO
        . NEW IEN SET IEN=$ORDER(OUTARRAY("NON-CPRS","B",RPC,"")) QUIT:(IEN="")
        . SET FNAME=$GET(OUTARRAY("NON-CPRS",IEN)) QUIT:FNAME=""
        . SET OUTARRAY("NON-CPRS ICR",RPC,IEN)=""
        . SET OUTARRAY(IEN)=FNAME
        . KILL OUTARRAY("NON-CPRS","B",RPC)
        ;
        WRITE "<a id=""Top"">",!
        WRITE "<a href=""#IA_CPRS_RPC"">IA-Allowed RPC's from CPRS</a><br>"
        WRITE "<a href=""#ALL_CPRS_RPC"">All CPRS RPC's</a><br>"
        WRITE "<a href=""#IA_NON_CPRS_RPC"">IA-Allowed RPC's <u>NOT</u> in CPRS</a><br>"
        WRITE "<p>",!        
        ;        
        WRITE "<a id=""IA_CPRS_RPC"">"
        WRITE "<strong>IA-Allowed RPC's from CPRS</strong></a> <a href=""#Top"">(Top)</a><p>",!
        WRITE "<ul>",!
        SET RPC=""
        FOR  SET RPC=$ORDER(OUTARRAY("ICR",RPC)) QUIT:(RPC="")  DO
        . NEW IEN SET IEN=$ORDER(OUTARRAY("ICR",RPC,"")) QUIT:(IEN="")
        . SET FNAME=$GET(OUTARRAY(IEN)) QUIT:FNAME=""
        . WRITE "<li><a href=""",FNAME,""" target=""main"">",RPC,"</a></li>",!
        WRITE "</ul>",!
        ;
        WRITE "<a id=""ALL_CPRS_RPC"">"
        WRITE "<strong>All CPRS RPC's</strong></a> <a href=""#Top"">(Top)</a><p>",!
        WRITE "<ul>",!
        SET RPC=""
        FOR  SET RPC=$ORDER(OUTARRAY("B",RPC)) QUIT:(RPC="")  DO
        . NEW IEN SET IEN=$ORDER(OUTARRAY("B",RPC,"")) QUIT:(IEN="")
        . SET FNAME=$GET(OUTARRAY(IEN)) QUIT:FNAME=""
        . NEW STAR SET STAR=""
        . IF $ORDER(OUTARRAY("ICR",RPC,0))>0 SET STAR="(*) "
        . WRITE "<li><a href=""",FNAME,""" target=""main"">",STAR_RPC,"</a></li>",!
        WRITE "</ul>",!
        ;
        WRITE "<a id=""IA_NON_CPRS_RPC"">"
        WRITE "<strong>IA-Allowed RPC's <u>NOT</u> in CPRS</strong></a> <a href=""#Top"">(Top)</a><p>",!
        WRITE "<ul>",!
        SET RPC=""
        FOR  SET RPC=$ORDER(OUTARRAY("NON-CPRS ICR",RPC)) QUIT:(RPC="")  DO
        . NEW IEN SET IEN=$ORDER(OUTARRAY("NON-CPRS ICR",RPC,"")) QUIT:(IEN="")
        . SET FNAME=$GET(OUTARRAY(IEN)) QUIT:FNAME=""
        . WRITE "<li><a href=""",FNAME,""" target=""main"">",RPC,"</a></li>",!
        WRITE "</ul>",!
        ;
        WRITE "</body",!
        WRITE "</html>",!
        QUIT
        ;
        ;
W1RPCHLP(IEN,RPCNAME)  ;"WRITE 1 RPC HELP
        ;"Purpose: To WRITE out a progress note in HTML format
        ;"Input: IEN -- the IEN in file 8994 (REMOTE PROCEDURE)
        ;"       RPCNAME -- OUT PARAMETER.  Passes back name of RPC
        ;"Output: The note (in complete HTML format) is written to current
        ;"      output device.
        ;"RESULT: none:
       ;"code below includes code modified from TMGRPCUT
 
        NEW OFFSET,s
        NEW IENS SET IENS=IEN_","
        ;
        FOR OFFSET=1:1  DO  QUIT:(s["{^}")
        . SET s=$PIECE($TEXT(NoteHdr+OFFSET),";;",2)
        . QUIT:(s["{^}")
        . WRITE s,!
        ;
        NEW MENU,USRPICK,I,TMGWP
        NEW ZN SET ZN=$GET(^XWB(8994,IEN,0))
        SET RPCNAME=$PIECE(ZN,"^",1)
        NEW TAG SET TAG=$PIECE(ZN,"^",2)
        NEW ROUTINE SET ROUTINE=$PIECE(ZN,"^",3)
        NEW CODEREF SET CODEREF=TAG_"^"_ROUTINE
        NEW CODELINE SET CODELINE=$TEXT(@CODEREF)
        SET CODELINE=CODEREF_"("_$PIECE(CODELINE,"(",2,99)
        NEW COMMENT SET COMMENT=$$TRIM^XLFSTR($PIECE(CODELINE,";",2,99))
        SET CODELINE=$PIECE(CODELINE,";",1)
        NEW COUNT SET COUNT=1
        IF COMMENT'="" SET TMGWP(COUNT)=COMMENT SET COUNT=COUNT+1

        WRITE "<h2><strong>",RPCNAME,"</strong></h2><p>",!  
        WRITE "<strong>Server Entry Point: </strong>"
        NEW URL SET URL="http://code.osehra.org/dox/Routine_"_ROUTINE_"_source.html"
        WRITE "<code><a href=""",URL,""">",CODELINE,"</a></code><p>"
        WRITE "<strong>Input Parameters: </strong>",!
        WRITE "<ul>",!
        ;
        NEW PARAMS SET PARAMS=$PIECE($PIECE(CODELINE,"(",2),")",1)
        FOR I=1:1:$LENGTH(PARAMS,",") DO
        . SET PARAMS(I,"NAME")=$PIECE(PARAMS,",",I)
        . NEW NAME SET NAME=PARAMS(I,"NAME")
        . NEW TEMPNAME SET TEMPNAME=NAME
        . IF I=1 SET TEMPNAME=TEMPNAME_" (The RPC result parameter)"
        . WRITE "<li><strong>"_TEMPNAME_"</strong></li>",!
        . DO DISP1(IEN,I,NAME)
        WRITE "</ul>",!
        ;
        NEW DONE SET DONE=0
        FOR I=1:1 DO  QUIT:DONE   ;"Print out any additional comment lines
        . NEW CODEREF2 SET CODEREFS=TAG_"+"_I_"^"_ROUTINE
        . SET COMMENT=$$TRIM^XLFSTR($TEXT(@CODEREFS))
        . IF $EXTRACT(COMMENT,1)'=";" SET DONE=1 QUIT
        . SET COMMENT=$PIECE(COMMENT,";",2,99)
        . SET TMGWP(COUNT)=COMMENT SET COUNT=COUNT+1
        IF $DATA(TMGWP) DO
        . WRITE "<p><strong>Code header comments:</strong><br> "
        . DO PAGEWP(.TMGWP) 
        . WRITE "<p>",!
        ;
        WRITE "<strong>RPC DESCRIPTION:</strong><br>  " 
        DO RPCDESCR(IEN)
        WRITE "<p>",!
        ;
        WRITE "</div>",!
        WRITE "</body",!
        WRITE "</html>",!
        ;
        QUIT
        ;
        ;
 ;"=======================================================================
 ;"code below modified from TMGRPCUT
        
DISP1(IEN,IDX,NAME) ;
        ;"Purpose: to display information about a given parameter
        ;"Input: IEN -- record# in REMOTE PROCEDURE file (8994) to display from
        ;"       IDX -- the index of the parameter.  1 for the 1st, 2 for the 2nd etc.
        ;"              Note: for RPC's, index=1 is always the return parameter
        ;"       NAME -- The name of the parameter
        SET IDX=$GET(IDX)
        SET IEN=+$GET(IEN)
        SET NAME=$GET(NAME)
        NEW TMGWP
        IF IDX'=1 GOTO DP2
        WRITE "<u>TYPE</u>: <samp>",$$GET1^DIQ(8994,IEN_",",.04),"</samp><br>",!
        IF $$GET1^DIQ(8994,IEN_",",3,"","TMGWP")
        WRITE "<u>DESCRIPTION</u>: ",!
        DO PAGEWP(.TMGWP)
        GOTO DPDN
DP2     SET IDX=IDX-1
        IF IDX'>0 GOTO DPDN
        NEW SUBIEN SET SUBIEN=+$ORDER(^XWB(8994,IEN,2,"PARAMSEQ",IDX,0))
        IF SUBIEN'>0 DO  GOTO DPDN
        . WRITE "<i><samp>(No info given)","</samp></i><br>",!
        . ;"WRITE "<i><samp>Sorry, creator of RPC did not provide further information.","</samp></i><br>",!
        NEW IENS SET IENS=SUBIEN_","_IEN_","
        WRITE "<u>TYPE</u>: <samp>",$$GET1^DIQ(8994.02,IENS,.02),"</samp><br>",!
        WRITE "<u>REQUIRED?</u>: <samp>",$$GET1^DIQ(8994.02,IENS,.04),"</samp><br>",!
        IF $$GET1^DIQ(8994.02,IENS,1,"","TMGWP")
        WRITE "<u>DESCRIPTION</u>: "
        DO PAGEWP(.TMGWP)
DPDN    WRITE "<br>",!
        QUIT
        ;
RPCDESCR(IEN) ;"Show description for RPC entry
        NEW TMGWP
        IF $$GET1^DIQ(8994,IEN_",",1,"","TMGWP")
        DO PAGEWP(.TMGWP)
        WRITE "<p>",!
        QUIT
        ;
PAGEWP(TMGWP) ;
        ;"Purpose: output WP array
        IF $DATA(TMGWP)'=0 DO
        . WRITE "<blockquote>"
        . NEW I SET I=0
        . FOR  SET I=$ORDER(TMGWP(I)) QUIT:(I="")  DO
        . . WRITE "<samp>",TMGWP(I),"</samp><br>",!
        . WRITE "</blockquote>",!
        ELSE  WRITE "<i><samp>(none given)</samp></i>",!
        QUIT
        ;        
        
  ;"=======================================================================
IndexDat
        ;;<html>
        ;; <head>
        ;;  <meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
        ;;  <meta name="Author" content="Kevin Toppenberg">
        ;;  <meta name="GENERATOR" content="jEdit">
        ;;  <title>Reference of CPRS RPC's</title>
        ;; </head>
        ;; <frameset cols="300,*">
        ;;  <frame name="toc" src="toc.htm" resize>
        ;;  <frame name="main" src="intro.htm">
        ;;  <noframes>
        ;;   <body>
        ;;   </body>
        ;;  </noframes>
        ;; </frameset>
        ;;</html>
        ;;
        ;;{^}  ;"Kevin's custom end-of-data symbol

IntroDat
        ;;<html>
        ;;<head>
        ;;  <meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
        ;;  <meta name="GENERATOR" content="Kate 2.1 on Linux">
        ;;  <title>Exported RPC Info</title>
        ;; </head>
        ;; <body bgcolor="#FFFFFF" link="#FFFF00" vlink="#800080">
        ;;  <p><strong>Please Select an RPC from List at Left</strong></p>
        ;; </body>
        ;;</html>
        ;;
        ;;{^}  ;"Kevin's custom end-of-data symbol


TOCHdr
        ;;<html>
        ;;<head>
        ;;  <meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
        ;;  <link rel="stylesheet" type="text/css" href="left-style.css">
        ;;  <title>List of Remote Procedure Calls (RPCs)</title>
        ;;</head>
        ;;<body link="#0000FF" vlink="#800080">
        ;;<p><a href="intro.htm" target="main">Introduction</a></p>
        ;;<p><font size="5"><b>Remote Procedure Calls</b></font></p>
        ;;
        ;;{^}  ;"Kevin's custom end-of-data symbol


NoteHdr
        ;;<html>
        ;;
        ;; <head>
        ;;  <meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
        ;;  <title>Introduction</title>
        ;;  <link rel="stylesheet" type="text/css" href="right-style.css">
        ;; </head>
        ;; <body>
        ;;<div id="PageDiv">
        ;;
        ;;{^}  ;"Kevin's custom end-of-data symbol

        
RStyleDat
        ;;body {
        ;;  background-color:#FAFAEE;
        ;;  background-image:url('http://www.mypublicwifi.com/images/serverclient.jpg');
        ;;  background-repeat:no-repeat;
        ;;  background-position:right top;
        ;;  background-size:20% auto;
        ;;}
        ;;H2 {
        ;;  text-shadow:4px 4px 5px #5066ff;  /*light blue */
        ;;}
        ;;samp {
        ;;  color:blue;
        ;;}
        ;;#PageDiv {
        ;;/*  height:800px;
        ;;  background:linear-gradient(Beige,White); 
        ;;*/
        ;;}
        ;;{^}  ;"Kevin's custom end-of-data symbol
        
LStyleDat
        ;;body {
        ;;  background-color:Beige;
        ;;}
        ;;        ;;
        ;;{^}  ;"Kevin's custom end-of-data symbol
        
       
