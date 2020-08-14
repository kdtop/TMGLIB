TMGTIUPR ;TMG/kst-Text objects for use in CPRS ; 7/26/2018
         ;;1.0;TMG-LIB;**1,17**;7/26/18
 ;
 ;"Kevin Toppenberg MD
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 7/26/2018  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"
 ;"=======================================================================
 ;"PUBLIC FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"PRIVATE FUNCTIONS
 ;"=======================================================================
MULTIPRT(TMGRESULT,LSTTOPRINT)  ;"
   ;"PURPOSE: This function is designed to compile all text from provided
   ;"         notes into one array, that the client can then pass to
   ;"         a TWebBrowser to print as one document with page breaks.
   SET TMGRESULT(0)="1^SUCCESSFUL"
   NEW TIUIEN SET TIUIEN=999999
   NEW TMGTEST SET TMGTEST=0
   SET BREAK="<p style=""page-break-before: always"">"
   IF TMGTEST=0 DO
   . MERGE ^TMP("TMG","MULTIPRT","LIST")=LSTTOPRINT
   ELSE  DO
   . MERGE LSTTOPRINT=^TMP("TMG","MULTIPRT","LIST")
   FOR  SET TIUIEN=$O(LSTTOPRINT(TIUIEN),-1) QUIT:TIUIEN'>0  DO
   . ;"SET TMGRESULT(TIUIEN)="GOT "_$G(LSTTOPRINT(TIUIEN))
   . NEW REF,NOTEARR
   . IF $$EXCLUDE(TIUIEN)=1 QUIT
   . DO TGET^TIUSRVR1(.REF,TIUIEN)
   . ;"DO PRINTW^ORWTIU(.REF,TIUIEN,2)
   . MERGE NOTEARR=@REF   
   . DO ADD(.TMGRESULT,.NOTEARR,BREAK)
   QUIT
   ;"
ADD(RESULT,NOTEARR,BREAK)  ;"
   ;"Purpose: This function adds contents of REF array to result, 
   ;"         after inserting "BREAK", which is the printing line
   ;"         break
   NEW OUTIDX SET OUTIDX=$O(RESULT(999999),-1)+1
   NEW IDX SET IDX=0
   IF OUTIDX>1 DO
   . SET RESULT(OUTIDX)=BREAK,OUTIDX=OUTIDX+1
   ;"NEW NOTEARR SET NOTEARR=@REF
   FOR  SET IDX=$O(NOTEARR(IDX)) QUIT:IDX'>0  DO
   . SET RESULT(OUTIDX)=$G(NOTEARR(IDX))
   . SET OUTIDX=OUTIDX+1
   QUIT
   ;"
EXCLUDE(TIUIEN)  ;"
   NEW TMGRESULT SET TMGRESULT=0
   ;"CHECK NOTE STATUS. ONLY INCLUDE SIGNED NOTES
   NEW STATUS SET STATUS=$P($G(^TIU(8925,TIUIEN,0)),"^",5)
   IF STATUS'=7 DO  GOTO EXDN
   . SET TMGRESULT=1
   ;"CHECK NOTE TO SEE IF USER CAN PRINT IT.
   NEW CANPRINT
   ;"SET CANPRINT=$S(TIULVL="DOC":$$CANDO^TIULP(TIUDA,"PRINT RECORD"),1:1)
   SET CANPRINT=$P($$CANDO^TIULP(TIUIEN,"PRINT RECORD"),"^",1)
   IF CANPRINT=0 DO  GOTO EXDN
   . SET TMGRESULT=1
EXDN
   QUIT TMGRESULT
   ;"
CHARTEXP(GREF,DFN,NOTESTOEXPORT,LABSTOEXPORT,RADTOEXPORT,COVERDATA)  ;"
   ;"This RPC will be used to export a patient's chart, in steps
   ;"Notes to achieve this.
   ;"INPUT: TMGRESULT - OUT. WILL BE EITHER 1^FILEPATH AND FILE NAME OF PDF
   ;"                               OR      -1^FAIL MESSAGE
   ;"       DFN - PATIENT IEN
   ;"       NOTESTOEXPORT - ARRAY CONTAINING NOTES TO INCLUDE
   ;"       LABSTOEXPORT - ARRAY CONTAINING LABS TO INCLUDE
   ;"       RADTOEXPORT - ARRAY CONTAINING RAD REPORTS TO INCLUDE
   ;"1) Create a folder using $J in /tmp. If one exists delete it
   ;"
   NEW TMGTEST SET TMGTEST=0
   IF TMGTEST=1 DO
   . SET DFN=$G(^TMG("TMGTIUPR","DFN"))
   . MERGE NOTESTOEXPORT=^TMG("TMGTIUPR","NOTESTOEXPORT")
   . MERGE LABSTOEXPORT=^TMG("TMGTIUPR","LABSTOEXPORT")
   . MERGE RADTOEXPORT=^TMG("TMGTIUPR","RADTOEXPORT")
   . MERGE COVERDATA=^TMG("TMGTIUPR","COVERDATA")
   ELSE  DO
   . SET ^TMG("TMGTIUPR","DFN")=DFN
   . MERGE ^TMG("TMGTIUPR","NOTESTOEXPORT")=NOTESTOEXPORT
   . MERGE ^TMG("TMGTIUPR","LABSTOEXPORT")=LABSTOEXPORT
   . MERGE ^TMG("TMGTIUPR","RADTOEXPORT")=RADTOEXPORT
   . MERGE ^TMG("TMGTIUPR","COVERDATA")=COVERDATA	   
   SET TMGRESULT="1^SUCCESSFUL"
   NEW FOLDERNAME,FILENAME
   SET FOLDERNAME="/tmp/EXPORT-"_$J
   SET GREF="^TMP(""DOWNLOAD^TMGRPC1"","_$J_")"
   K @GREF
   IF $$MKDIR^TMGKERNL(FOLDERNAME)'=0 DO  GOTO CEDN
   . SET @GREF@(0)="-1^COULD NOT CREATE FOLDER"
   NEW FILECOUNT SET FILECOUNT=1 ;"SET THE FILE COUNT AS WE WANT IT EXPORTED
   NEW NAME SET NAME=$P($G(^DPT(DFN,0)),"^",1)
   NEW DOB SET DOB=$$EXTDATE^TMGDATE($P($G(^DPT(DFN,0)),"^",3))
   ;"
   ;" ADDING A STEP HERE FOR A COVERSHEET
   ;"
     ;"GOTO RAD
   NEW COVERSHEET,COVERIDX
   SET COVERIDX=1
   DO SETDFLTHDR(.COVERSHEET,.COVERIDX,NAME,DOB)
   DO MAKECOVER(.COVERSHEET,.COVERIDX,.COVERDATA)
   SET FILENAME="0-"_1_"-COVER.html"
   DO EXPHTML(.COVERSHEET,FOLDERNAME,FILENAME)
   ;"
   ;"2) Cycle through each note. Inside the notes:
   ;"   2a) Replace image tags with absolute file paths
   ;"   2b) Add the tags for headers and footers
   ;"      <!-- FOOTER LEFT "foo" -->
   ;"      <!-- FOOTER CENTER "foo" -->
   ;"      <!-- FOOTER RIGHT "foo" -->
   ;"      <!-- HEADER LEFT "foo" -->
   ;"      <!-- HEADER CENTER "foo" -->
   ;"      <!-- HEADER RIGHT "foo" -->
   ;"          Also use $DATE, $PAGE, $PAGES, $TIME
   ;"   2c) Add the page break "<p style=""page-break-before: always"">" at the end of the note
   ;"   2d) Save the entire HTML note out as an file, incrementing the filename by one
   ;"
   NEW TIUIEN SET TIUIEN=999999

   NEW TMGTEST SET TMGTEST=0
   SET BREAK="<p style=""page-break-before: always"">"  ;"THIS MAY NOT BE NEEDED
   ;"
   FOR  SET TIUIEN=$O(NOTESTOEXPORT(TIUIEN),-1) QUIT:TIUIEN'>0  DO
   . WRITE TIUIEN,!
   . IF $$EXCLUDE(TIUIEN)=1 QUIT
   . NEW REF,NOTEARR
   . SET FILENAME="1-"_FILECOUNT_"-TIU"_TIUIEN_".html"  ;"1 is the order number, followed by filecount, then TIU number for reference
   . SET FILECOUNT=FILECOUNT+1
   . DO TGET^TIUSRVR1(.REF,TIUIEN)
   . MERGE NOTEARR=@REF
   . ;"NEW NOTETYPE SET NOTETYPE=$P($G(^TIU(8925,TIUIEN,0)),"^",1)
   . SET NOTEARR(1)=$G(NOTEARR)_"<!-- HEADER CENTER ""DOS: "_$$EXTDATE^TMGDATE($P($G(^TIU(8925,TIUIEN,13)),"^",1),1)_""" -->"
   . DO EXPHTML(.NOTEARR,FOLDERNAME,FILENAME)
   . ;"WRITE BREAK,!
  
   ;"3) Cycle through each lab, creating an HTML table
   ;"   3a) Add the page break "<p style=""page-break-before: always"">" at the end of the note
   ;"   3b) Save the entire HTML note out as an file, incrementing the filename by one
   ;"
   SET FILECOUNT=1
   NEW LABDATE SET LABDATE=9999999
   FOR  SET LABDATE=$O(LABSTOEXPORT(LABDATE),-1) QUIT:LABDATE'>0  DO
   . NEW SINGLEDT SET SINGLEDT(LABDATE)=""
   . NEW LABHTML DO GETREPRT^TMGRPCL1(.LABHTML,DFN,.SINGLEDT)
   . SET FILENAME="2-"_FILECOUNT_"-LABS.html",FILECOUNT=FILECOUNT+1
   . SET LABHTML(1)=$G(LABHTML)_"<!-- HEADER CENTER ""LAB RESULTS: "_$$EXTDATE^TMGDATE(LABDATE,1)_""" -->"
   . DO EXPHTML(.LABHTML,FOLDERNAME,FILENAME)
   . MERGE ^TMP("LABHTML",FILECOUNT)=LABHTML
   ;"4) Cycle though each Radiology report and print out an HTML table
   ;"   4a) Add the page break "<p style=""page-break-before: always"">" at the end of the note
   ;"   4b) Save the entire HTML note out as an file, incrementing the filename by one
   ;"
      
   SET FILECOUNT=1
   NEW RADREPORT SET RADREPORT=""
   FOR  SET RADREPORT=$O(RADTOEXPORT(RADREPORT)) QUIT:RADREPORT=""  DO
   . NEW ROOT,RADARRAY,RADHTML
   . DO RPT^ORWRP(.ROOT,DFN,"18:IMAGING (LOCAL ONLY)~","","",RADREPORT,0,0)
   . MERGE RADARRAY=@ROOT
   . NEW RADIDX SET RADIDX=0
   . NEW RADHTML
   . FOR  SET RADIDX=$O(RADARRAY(RADIDX)) QUIT:RADIDX'>0  DO
   . . SET RADHTML(RADIDX)=$G(RADARRAY(RADIDX,0))_"<br>"
   . SET RADHTML(1)="<HTML><BODY><TABLE BORDER=1 BGCOLOR=#ffffe6 style=""table-layout:fixed; width:800px""><tr><td><div style = ""width:700px; word-wrap: break-word""> <!-- HEADER CENTER ""IMAGING RESULTS"" --> "_$G(RADHTML(1))
   . SET RADHTML($O(RADHTML(9999999),-1))=",</td></tr></TABLE></BODY></HTML>"
   . SET FILENAME="3-"_FILECOUNT_"-IMAGING.html",FILECOUNT=FILECOUNT+1
   . DO EXPHTML(.RADHTML,FOLDERNAME,FILENAME)
   ;"
   ;"
   ;"5) Export the entire list of HTML files to PDF
   ;"   htmldoc --webpage /tmp/*.html --outfile /tmp/TestNote.pdf
   NEW DESTFOLDER SET DESTFOLDER="/opt/worldvista/EHR/images/"
   NEW FILENAME SET FILENAME="EXPORT-"_DFN_"-"_$J_".pdf"
   NEW LINUXCMD SET LINUXCMD="htmldoc --webpage "_FOLDERNAME_"/*.html --outfile "_DESTFOLDER_FILENAME
   NEW EXPRESULT
   SET EXPRESULT=$$LINUXCMD^TMGKERNL(LINUXCMD)
   IF $P(EXPRESULT,"^",1)'=1 DO  GOTO CEDN
   . SET @GREF@(0)=EXPRESULT
   ;". SET TMGRESULT="1^"DESTFOLDER_FILENAME
   ;"6) Finally return either the location of the PDF, or send it back through the RPC
   DO DOWNLOAD^TMGRPC1C(GREF,DESTFOLDER,FILENAME,"")
   ;"
   ;"7) Clean up
   NEW LINUXRESULT
   SET LINUXRESULT=$$LINUXCMD^TMGKERNL("rm "_FOLDERNAME_"/*.*")
   SET LINUXRESULT=$$LINUXCMD^TMGKERNL("rmdir "_FOLDERNAME)
   SET LINUXRESULT=$$LINUXCMD^TMGKERNL("rm "_DESTFOLDER_FILENAME)
CEDN   
   QUIT
   ;"
EXPHTML(ARRTOEXP,FOLDERNAME,FILENAME)  ;"
   NEW HANDLE,PATH
   SET HANDLE="TMGEXPORT"
   SET PATH=FOLDERNAME_"/"
   DO OPEN^%ZISH(HANDLE,PATH,FILENAME,"W")
   IF POP DO  GOTO EHDN
   . write "Unable to open file for writing: ",path,filename,!
   USE IO
   NEW IDX SET IDX=0
   FOR  SET IDX=$O(ARRTOEXP(IDX)) QUIT:IDX'>0  DO
   . IF $G(ARRTOEXP(IDX))["KTESIG" DO
   . . SET ARRTOEXP(IDX)=$P($G(ARRTOEXP(IDX)),"file:///$CPRSDIR$/Cache",1)_"/opt/worldvista/EHR/server-files"_$P($G(ARRTOEXP(IDX)),"file:///$CPRSDIR$/Cache",2)
   . IF $G(ARRTOEXP(IDX))["$CPRSDIR$" DO
   . . WRITE $$GETIPATH($G(ARRTOEXP(IDX))),!
   . ELSE  DO
   . . WRITE $G(ARRTOEXP(IDX)),!
   DO CLOSE^%ZISH(HANDLE)
EHDN
   QUIT
   ;"
MAKECOVER(COVERSHEET,COVERIDX,COVERDATA)
   SET COVERSHEET(COVERIDX)="<HTML><BODY>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="<TABLE style=""margin-left:auto;margin-right:auto;""><TR><TD>FAMILY PHYSICIANS OF GREENEVILLE PC.</TD></TR>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="<TR><TD>Laughlin Medical Building #1</TD></TR>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="<TR><TD>1410 Tusculum Blvd. Suite 2600</TD></TR>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="<TR><TD>Greeneville, TN. 37745</TD></TR>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="<TR><TD>VOICE: (423)787-7000    FAX: (423)787-7049</TD></TR></TABLE>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="<P>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="<TABLE><TR><TD>To: <u>"_$G(COVERDATA("TO"))_"</u></TD>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="<TD>Fax: <u>"_$G(COVERDATA("FAX"))_"</u></TD></TR>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="<TR><TD>From: <u>"_$P($G(^VA(200,DUZ,0)),"^",1)_"</u></TD>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="<TD>Fax: <u>(423)787-7000</u></TD></TR>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="<TR colspan=""2""><TD>Date: <u>"_$$TODAY^TMGDATE(1,1)_"</u></TD></TR>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="<TR colspan=""2""><TD>RE: <u>"_$G(COVERDATA("RE"))_"</u></TD></TR>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="<TR colspan=""2""><TD>Comments: <u>",COVERIDX=COVERIDX+1
   NEW I SET I=0 
   FOR  SET I=$O(COVERDATA("COMMENTS",I)) QUIT:I'>0  DO
   . SET COVERSHEET(COVERIDX)=$G(COVERDATA("COMMENTS",I))_" ",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="</u></TD></TR></TABLE>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="<P><P><P>CONFIDENTIALITY CLAUSE<P>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="The information contained in this facsimile message is legally privileged and confidential information<br>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="intended only for the use of the individual or entity named above. If the reader of this message is not<BR>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="the intended recipient, you are hereby notified that any dissemination, distribution or copying of the<br>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="telecopy is prohibited. If you have received this telecopy in error, please immediately notify us by<br>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="telephone.<p>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="                                                              Thank You<p></body></html>",COVERIDX=COVERIDX+1
   QUIT
   ;"
SETDFLTHDR(COVERSHEET,COVERIDX,NAME,DOB)
   SET COVERSHEET(COVERIDX)="<!-- FOOTER LEFT ""FAMILY PHYSICIANS OF GREENEVILLE"" -->",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="<!-- FOOTER CENTER "" "" -->",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="<!-- FOOTER RIGHT ""PAGE: $PAGE"" -->",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="<!-- HEADER CENTER """_""" -->",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="<!-- HEADER LEFT ""PATIENT : "_NAME_""" -->",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="<!-- HEADER RIGHT ""DOB: "_DOB_""" -->",COVERIDX=COVERIDX+1
   QUIT
   ;"
GETIPATH(LINE)
   ;WRITE LINE,!
   NEW DIVIDER SET DIVIDER=$P(LINE,"$CPRSDIR$",2)
   SET DIVIDER=$E(DIVIDER,1,1)
   NEW PATHTOREPLACE SET PATHTOREPLACE="$CPRSDIR$"_DIVIDER_"Cache"_DIVIDER
   NEW IMAGENAME SET IMAGENAME=$P(LINE,PATHTOREPLACE,2)
   ;WRITE IMAGENAME,!
   SET IMAGENAME=$P(IMAGENAME,"""",1)
   ;WRITE IMAGENAME,!,!
   NEW IMAGEPATH SET IMAGEPATH=$$IMAGEPATH(IMAGENAME)
   SET LINE=$P(LINE,PATHTOREPLACE,1)_IMAGEPATH_$P(LINE,PATHTOREPLACE,2,9999)
   IF LINE["$CPRSDIR$" SET LINE=$$GETIPATH(LINE)   ;"RECURSE INTO FUNCTION IF TAG STILL EXISTS
   QUIT LINE
   ;"
IMAGEPATH(FILENAME)
   NEW RESULT SET RESULT="/opt/worldvista/EHR/server-files/"
   IF FILENAME["KTESIG" SET RESULT="opt/worldvista/EHR/server-files/" QUIT RESULT
   NEW TEMP SET TEMP=$P(FILENAME,".",1)
   FOR  QUIT:$L(TEMP)<3  DO
   . IF TEMP["TMG0" DO
   . . SET RESULT=RESULT_"TMG0/"
   . . SET TEMP=$E(TEMP,5,$L(TEMP))
   . ELSE  DO
   . . NEW NEWFOLDER 
   . . SET NEWFOLDER=$E(TEMP,1,2),TEMP=$E(TEMP,3,$L(TEMP))
   . . SET RESULT=RESULT_NEWFOLDER_"/"
   ;"SET RESULT=RESULT_FILENAME Don't return filename
   QUIT RESULT
   ;"

