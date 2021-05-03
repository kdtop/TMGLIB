TMGHTMCH ;TMG/kst/HTML Mini-chart creator ;03/25/06, 2/2/14, 3/24/21
         ;;1.0;TMG-LIB;**1**;01/10/06

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
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================

 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"DumpNtes(List,FPath,OutArray)
 ;"MakeFNAME(IEN)
 ;"GetTemplateRecs(pRecs,Template)
 ;"Write1Note(IEN)


 ;"=======================================================================
 ;"Dependencies   (duplicates shown in parenthesies)
 ;"=======================================================================
 ;"^TMGMISC

MAKESITE(FPath,Template)
        ;"Purpose: To create an interlinked site with specified notes.
        ;"Input: FPath OPTIONAL -- IF not provided, user will be asked
        ;"              This is the directory where output is to be sent
        ;"      Input OPTIONAL -- IF not provided, user will be asked
        ;"              This is the name of the search/sort template holding
        ;"              a list of IENs to output
        ;"Output: files are written to file system
        ;"Result: none.


        IF $GET(FPath)="" do
        . WRITE !!,"This will export TIU DOCUMENT records to an interlinked website.",!!
        . read "Enter destination directory path: ",FPath:$GET(DTIME,3600),!
        IF FPath="^" GOTO MSDone

        ;"Create core index.htm
        ;"----------------------
        IF $$OpenIO(FPath,"index.htm")=0 DO  GOTO MSDone
        . WRITE "Error.  Aborting.",!
        NEW offset
        for offset=1:1  DO  QUIT:(s["{^}")
        . SET s=$PIECE($TEXT(IndexDat+offset),";;",2)
        . QUIT:(s["{^}")
        . WRITE s,!
        DO ^%ZISC ;" Close the output device

        ;"Create core intro.htm
        ;"----------------------
        IF $$OpenIO(FPath,"intro.htm")=0 DO  GOTO MSDone
        . WRITE "Error.  Aborting.",!
        NEW offset
        for offset=1:1  DO  QUIT:(s["{^}")
        . SET s=$PIECE($TEXT(IntroDat+offset),";;",2)
        . QUIT:(s["{^}")
        . WRITE s,!
        DO ^%ZISC ;" Close the output device

        ;"Create individual files with notes.
        ;"-----------------------------------
        NEW OutArray
        DO WriteTemplate(.FPath,.Template,.OutArray)


        ;"Create toc.htm-- the table of contents.
        ;"---------------------------------------
        IF $$OpenIO(FPath,"toc.htm")=0 DO  GOTO MSDone
        . WRITE "Error.  Aborting.",!
        DO MakeTOC(.OutArray)
        DO ^%ZISC ;" Close the output device

MSDone
        WRITE "Good bye.",!!
        QUIT

WriteTemplate(FPath,Template,OutArray)
        ;"Purpose: To WRITE out notes listed in Template to directory FPath
        ;"Input: FPath -- The name of the directory to put the output files to
        ;"      Template -- OPTIONAL -- the name of a search/sort template that contains
        ;"              list of IENS's to output
        ;"              If not supplied, user will be asked for name.
        ;"      OutArray -- An OUT parameter.  PASS BY REFERENCE
        ;"              An array to receive results of names written.  See WriteList for format
        ;"Output: files are written to directory
        ;"Result: none

        NEW List,count
        SET count=$$GetTemplateRecs("List",.Template)

        NEW PrgsFn SET PrgsFn="do PROGBAR^TMGUSRI2(TMGCUR,""Progress"",1,TMGMAX,,TMGSTART)"

        IF count>0 do
        . DO WriteList(.List,FPath,.OutArray,PrgsFn)

        QUIT


WriteList(List,FPath,OutArray,PrgCallback)
        ;"Purpose: To WRITE out all notes given in List to separate files in FPath
        ;"Input: List -- PASS BY REFERENCE.  A list of IEN's that must be written.
        ;"              Format as follows:
        ;"              List(IEN1)=""
        ;"              List(IEN2)=""
        ;"              List(IEN3)=""
        ;"              List(IEN4)=""
        ;"              ...
        ;"      FPath -- The name of the directory that files should be written to
        ;"              e.g. "/tmp/output/"
        ;"      OutArray -- An OUT parameter.  PASS BY REFERENCE
        ;"              An array to receive results of names written.  Format:
        ;"              OutArray(IEN1)=Filename1
        ;"              OutArray(IEN1,PatientNameAndDOB1)=""
        ;"              OutArray(IEN2)=Filename2
        ;"              OutArray(IEN2,PatientNameAndDOB2)=""
        ;"              OutArray(IEN3)=Filename3
        ;"              OutArray(IEN3,PatientNameAndDOB3)=""
        ;"              OutArray(IEN4)=Filename4
        ;"              OutArray(IEN4,PatientNameAndDOB4)=""
        ;"              OutArray("B",PatientNameAndDOB1,IEN1)=""
        ;"              OutArray("B",PatientNameAndDOB1,IEN1b)="" <-- IF more than one IEN per patient.
        ;"              OutArray("B",PatientNameAndDOB2,IEN2)=""
        ;"              OutArray("B",PatientNameAndDOB3,IEN3)=""
        ;"              OutArray("B",PatientNameAndDOB4,IEN4)=""
        ;"      PrgCallback: OPTIONAL -- IF supplied, then M code contained in this string
        ;"              will be xecuted periodically, to allow display of a progress bar etc.
        ;"              Note: the following variables with global scope will be declared and
        ;"                      available for use: TMGCUR (current count), TMGMAX (max count),
        ;"                      TMGSTART (the start time
        ;"
        ;"Output: A series of files will be written (or overwritten) to directory
        ;"      Each file will be a TIU DOCUMENT in .htm format.
        ;"      Filename format: lastname_firstname_title_datetime.htm
        ;"Result: none

        NEW ien
        KILL OutArray
        NEW TMGMAX SET TMGMAX=0
        NEW TMGSTART SET TMGSTART=$H
        NEW TMGCUR SET TMGCUR=0
        SET ien=$ORDER(List(""))
        IF ien'="" FOR  DO  QUIT:(ien="")
        . SET TMGMAX=TMGMAX+1
        . SET ien=$ORDER(List(ien))
        NEW delay SET delay=0

        SET ien=$ORDER(List(""))
        IF ien'="" FOR  DO  QUIT:(ien="")
        . SET TMGCUR=TMGCUR+1
        . NEW FNAME
        . SET FNAME=$$MakeFNAME(ien)
        . IF $$OpenIO(FPath,FNAME)'=0 do
        . . DO Write1Note(ien)
        . . DO ^%ZISC ;" Close the output device
        . . SET OutArray(ien)=FNAME
        . . NEW PtName,DOB,TMGDFN
        . . SET TMGDFN=$$GET1^DIQ(8925,ien_",",.02,"I")
        . . SET PtName=$$GET1^DIQ(2,TMGDFN_",",.01)
        . . SET DOB=$$GET1^DIQ(2,TMGDFN_",",.03)
        . . SET PtName=PtName_" "_DOB
        . . SET OutArray(ien,PtName)=""
        . . SET OutArray("B",PtName,ien)=""
        . IF (delay>30),$GET(PrgCallback)'="" DO  ;"update progress bar every 30 cycles
        . . NEW $ETRAP SET $ETRAP="write ""(Invalid M Code!.  Error Trapped.)"" SET $ETRAP="""",$ecode="""""
        . . xecute PrgCallback  ;"call the specified progress code.
        . . SET delay=0
        . ELSE  SET delay=delay+1
        . SET ien=$ORDER(List(ien))

        QUIT


OpenIO(FPath,FNAME,NodeDiv)
        ;"Purpose: to open the IO channel such that all writes
        ;"      will go to the file specified.
        ;"Input: FPath -- the path to open file in
        ;"       FNAME -- the name of the file to open
        ;"       NodeDiv -- OPTIONAL (default is "/") -- the path delimiter for OS
        ;"result: 1=OK To continue, 0=error

        NEW result SET result=1
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
        . SET result=0

        use IO

OIODone
        QUIT result


MakeFNAME(IEN)
        ;"Purpose: To create a filename from TIU DOCUMENT IEN
        ;"Input -- IEN.  and IEN from file 8925
        ;"Result -- the filename

        NEW result SET result=""
        NEW name,type,datetime

        SET name=$$GET1^DIQ(8925,IEN_",",.02)
        SET name=$TRANSLATE(name,",","_")
        SET name=$TRANSLATE(name," ","-")

        SET type=$$GET1^DIQ(8925,IEN_",",.01)
        SET type=$TRANSLATE(type," ","-")

        SET date=$$GET1^DIQ(8925,IEN_",",.07,"I")
        SET date=$$DTFormat^TMGMISC(date,"mm-dd-yyyy")

        SET result=name_"_"_type_"_"_date_".htm"
        QUIT result


GetTemplateRecs(pRecs,Template)
        ;"Purpose: to ask user for a search/sort template to inport records from.
        ;"Input -- pRecs -- pointer to (i.e. name of) array to fill
        ;"                      will probably be passed with "Array(12345)"
        ;"         Template -- OPTIONAL.  Name of template to import.
        ;"              If not supplied, user will be asked for name
        ;"Output: Data is put into pRecs like this:
        ;"              @pRecs@(IEN1)=""
        ;"              @pRecs@(IEN2)=""
        ;"              @pRecs@(IEN3)=""
        ;"Result: Count of records imported
        ;"Note: uses global variable pHeader

        NEW File SET File=8925
        NEW count SET count=0
        NEW Y
        IF $GET(pRecs)="" GOTO GTRDone

        FOR  DO  QUIT:((+Y>0)!(+Y=-1))
        . NEW DIC
        . SET DIC=.401
        . IF $GET(Template)'="" do
        . . SET X=Template
        . ELSE  do
        . . SET DIC(0)="AEQ"
        . . WRITE "Select a Template containing records for import. ",!
        . . WRITE "(? for list, ^ to QUIT) "
        . . SET DIC("A")="Enter Template: "
        . DO ^DIC
        . IF $GET(Template)="" WRITE !
        . IF +Y'>0 QUIT
        . NEW node SET node=$GET(^DIBT(+Y,0))
        . IF $PIECE(node,"^",4)'=File DO  QUIT
        . . SET Y=0  ;"signal to try again
        . . NEW PriorErrorFound
        . . WRITE "Error: That template doesn't contain a list of progress notes. Please select another.",!

        IF (+Y>0)&($DATA(^DIBT(+Y,1))>1) do
        . NEW index SET index=$ORDER(^DIBT(+Y,1,0))
        . IF index'="" FOR  DO  QUIT:(index="")
        . . SET @pRecs@(index)=""
        . . SET count=count+1
        . . SET index=$ORDER(^DIBT(+Y,1,index))

        IF $GET(Template)="" WRITE count," Records selected.",!

GTRDone
        QUIT count



Write1Note(IEN)
        ;"Purpose: To WRITE out a progress note in HTML format
        ;"Input: IEN -- the IEN in file 8925 (TIU DOCUMENT)
        ;"Output: The note (in complete HTML format) is written to current
        ;"      output device.
        ;"Result: none:

        NEW offset,s
        NEW IENS SET IENS=IEN_","

        for offset=1:1  DO  QUIT:(s["{^}")
        . SET s=$PIECE($TEXT(NoteHdr+offset),";;",2)
        . QUIT:(s["{^}")
        . WRITE s,!

        WRITE "<h2><strong>",$$GET1^DIQ(8925,IENS,".01"),"</strong></h2>",!  ;"Note type
        WRITE "<p><strong>Name: </strong>",$$GET1^DIQ(8925,IENS,".02"),"<br>",!    ;"patient name
        NEW Date SET Date=$$GET1^DIQ(8925,IENS,".07","I")
        SET Date=$$DTFormat^TMGMISC(Date,"mmmm d,yyyy")
        WRITE "<strong>Date: </strong>",Date,"</p>",!    ;"note date
        WRITE "<p><strong>Note: </strong></p>",!

        NEW TMGWP,x
        SET x=$$GET1^DIQ(8925,IENS,2,"","TMGWP")="WP"
        do
        . NEW i
        . WRITE "<p>"
        . SET i=$ORDER(TMGWP(""))
        . FOR  DO  QUIT:(i="")
        . . NEW line SET line=$GET(TMGWP(i))
        . . SET line=$$SYMENC^MXMLUTL(line)
        . . WRITE line,"<br>",!
        . . SET i=$ORDER(TMGWP(i))
        . WRITE "</p>",!

        WRITE "<p><strong><u>Note Detail</u>:</strong><br>",!
        WRITE "<font size=""2""><strong>Author</strong>: ",$$GET1^DIQ(8925,IENS,"1202"),"</font><br>",!
        WRITE "<font size=""2""><strong>Signature Date/Time</strong>: ",$$GET1^DIQ(8925,IENS,"1501"),"</font><br>",!
        WRITE "<font size=""2""><strong>Status</strong>: ",$$GET1^DIQ(8925,IENS,".05"),"</font><br>",!
        WRITE "<font size=""2""><strong>Location</strong>: ",$$GET1^DIQ(8925,IENS,"1211"),"</font><br>",!
        WRITE "<font size=""2""><strong>Transcriptionist</strong>: ",$$GET1^DIQ(8925,IENS,"1302"),"</font><br>",!
        WRITE "<font size=""2""><strong>Transcription Date/Time</strong>: ",$$GET1^DIQ(8925,IENS,"1201"),"</font><br>",!
        ;"WRITE "<font size=""2""><strong>Line count</strong>: ",$$GET1^DIQ(8925,IENS,".1"),"</font><br>",!
        ;"WRITE "<font size=""2""><strong>Character count</strong>: ",$$GET1^DIQ(8925,IENS,"22711"),"</font><br>",!
        WRITE "</p>",!

        WRITE "</body",!
        WRITE "</html>",!

        QUIT


MakeTOC(OutArray)
        ;"Purpose: To WRITE toc.htm (the table of contents side panel)
        ;"Input: IEN -- the IEN in file 8925 (TIU DOCUMENT)
        ;"Output: OutArray -- PASS BY REFERENCE
        ;"              An array with file names written.  Format:
        ;"              OutArray(IEN1)=Filename1
        ;"              OutArray(IEN1,PatientNameAndDOB1)=""
        ;"              OutArray(IEN2)=Filename2
        ;"              OutArray(IEN2,PatientNameAndDOB2)=""
        ;"              OutArray(IEN3)=Filename3
        ;"              OutArray(IEN3,PatientNameAndDOB3)=""
        ;"              OutArray(IEN4)=Filename4
        ;"              OutArray(IEN4,PatientNameAndDOB4)=""
        ;"              OutArray("B",PatientNameAndDOB1,IEN1)=""
        ;"              OutArray("B",PatientNameAndDOB1,IEN1b)="" <-- IF more than one IEN per patient.
        ;"              OutArray("B",PatientNameAndDOB2,IEN2)=""
        ;"              OutArray("B",PatientNameAndDOB3,IEN3)=""
        ;"              OutArray("B",PatientNameAndDOB4,IEN4)=""
        ;"
        ;"Result: none:

        NEW offset,s

        for offset=1:1  DO  QUIT:(s["{^}")
        . SET s=$PIECE($TEXT(TOCHdr+offset),";;",2)
        . QUIT:(s["{^}")
        . WRITE s,!

        ;"Write Patient name and DOB, then list of their notes , then repeat.
        NEW Patient
        SET Patient=$ORDER(OutArray("B",""))
        IF Patient'="" FOR  DO  QUIT:(Patient="")
        . WRITE "<p><strong>",Patient,"</strong></p>",!
        . WRITE "<ul>",!
        . NEW ien SET ien=$ORDER(OutArray("B",Patient,""))
        . IF ien'="" FOR  DO  QUIT:(ien="")
        . . NEW Type,Date,FNAME
        . . SET Type=$$GET1^DIQ(8925,ien_",",".01") ;"Note type
        . . SET Date=$$GET1^DIQ(8925,ien_",",".07","I") ;"note date
        . . SET Date=$$DTFormat^TMGMISC(Date,"mm/dd/yyyy")
        . . SET FNAME=OutArray(ien)
        . . WRITE "<li><a href=""",FNAME,""" target=""main"">"
        . . WRITE Type," -- ",Date
        . . WRITE "</a></li>",!
        . . SET ien=$ORDER(OutArray("B",Patient,ien))
        . SET Patient=$ORDER(OutArray("B",Patient))
        . WRITE "</ul>",!

        WRITE "</body",!
        WRITE "</html>",!

        QUIT



 ;"=======================================================================
IndexDat
        ;;<html>
        ;; <head>
        ;;  <meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
        ;;  <meta name="Author" content="Open Vista Team &amp; ed. by Kevin Toppenberg">
        ;;  <meta name="GENERATOR" content="Kate 2.1 on Linux">
        ;;  <title>Open VistA Exported Notes</title>
        ;; </head>
        ;; <frameset cols="200,*">
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
        ;;  <title>OpenVistA Exported Notes</title>
        ;; </head>
        ;; <body bgcolor="#FFFFFF" link="#FFFF00" vlink="#800080">
        ;;  <p><strong>Please Select A Patient from List at Left</strong></p>
        ;; </body>
        ;;</html>
        ;;
        ;;{^}  ;"Kevin's custom end-of-data symbol


TOCHdr
        ;;<html>
        ;;<head>
        ;;<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
        ;;<title>OpenVista List of Patients</title>
        ;;</head>
        ;;<body link="#0000FF" vlink="#800080">
        ;;<p><a href="intro.htm" target="main">Introduction</a></p>
        ;;<p><font size="5"><b>Patients</b></font></p>
        ;;
        ;;{^}  ;"Kevin's custom end-of-data symbol


NoteHdr
        ;;<html>
        ;;
        ;; <head>
        ;;  <meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
        ;;  <title>OpenVistA Introduction</title>
        ;; </head>
        ;; <body>
        ;
        ;;{^}  ;"Kevin's custom end-of-data symbol


