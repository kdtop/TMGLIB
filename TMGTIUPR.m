MATMGTIUPR ;TMG/kst-Text objects for use in CPRS ; 7/26/2018, 3/24/21
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
CHARTEXP(GREF,TMGDFN,NOTESTOEXPORT,LABSTOEXPORT,RADTOEXPORT,COVERDATA,SCANSTOEXPORT,EXTRATOEXPORT,ORDERSTOEXPORT)  ;"
   ;"Purpose: This RPC takes Notes, Labs, Imaging, Coversheet, Scans, and ExtraDocs from a patient's chart
   ;"         to export to a PDF, then returns the PDF through GREF
   ;"INPUT: GREF - OUT. WILL BE EITHER 1^FILEPATH AND FILE NAME OF PDF
   ;"                               OR      -1^FAIL MESSAGE
   ;"       TMGDFN - PATIENT IEN
   ;"       NOTESTOEXPORT - ARRAY CONTAINING NOTES TO INCLUDE
   ;"       LABSTOEXPORT - ARRAY CONTAINING LABS TO INCLUDE
   ;"       RADTOEXPORT - ARRAY CONTAINING RAD REPORTS TO INCLUDE
   ;"       COVERDATA - Containing Cover sheet data
   ;"                ("TO") - Recipient
   ;"                ("FAX") - Fax number
   ;"                ("RE") - RE
   ;"                ("COMMENTS",#) - ARRAY OF COMMENTS
   ;"                ("MODE") - #OF MODE 0=NONE,1=DEFAULT,2=UPLOADED ONE,3=CONSULTANT, 4=TEMPLATE
   ;"                ("UPLOADED") - IF MODE = 2, THIS WILL CONTAIN THE LOCAL LOCATION OF THE UPLOADED FILE
   ;"                ("CONSULTANT") - IF MODE = 3, THIS WILL CONTAIN THE CONSULTANT TO USE
   ;"                ("DEMOGRAPHICS") - EITHER 0 TO NOT, OR 1 TO INCLUDE A DEMO SHEET AS PAGE 2 OF THE EXPORT
   ;"                ("TEMPLATE") - IF MODE = 4, THIS WILL CONTAIN THE TEMPLATE TO USE FOR THE COVERSHEET
   ;"       SCANSTOEXPORT - Array containing the patient's scanned docs to include
   ;"       EXTRATOEXPORT - Array containing the outside files to include
   ;"
   ;"       EXPLANATION OF PROCESS:
   ;"          THIS TAKES MULTIPLE SOURCES AND STAGES THEM AS HTML DOCUMENTS INTO A CREATED TEMP FOLDER (/tmp/EXPORT-$J)
   ;"          THE FILES ARE NAME SPACED NUMERICALLY TO ENSURE THEY ARE IN THE PROPER ORDER WHEN EXPORTED
   ;"          THE FIRST CHARACTER IS THE SECTION NUMBER, THEN A LABEL, THEN AN INDEX
   ;"
   ;"         FILE LOCATIONS:
   ;"          TEMP LOCATION - /tmp/EXPORT-$J  (cleaned and removed at the end of the process)
   ;"          UPLOADED FILES LOCATION - /opt/worldvista/EHR/server-files   (each file deleted after it has been processed)
   ;"          COMPLETED PDF LOCATION - /opt/worldvista/EHR/images/  (file is deleted after stored in the global)
   ;"
   ;"         SECTIONS EXPLAINED:
   ;"           SECTION 0 - COVER SHEET
   ;"             THE FIRST THING
   ;"             COVER SHEETS HAVE 5 MODES (SENT IN COVERDATA("MODE")
   ;"               0 = NO COVERSHEET
   ;"               1 = DEFAULT COVER SHEET - CREATE COVERSHEET 
   ;"               2 = UPLOADED COVER SHEET - FILE IS A PDF. FILENAME SENT IN COVERDATA("UPLOADED")
   ;"                                          FILE IS EXPECTED TO BE UPLOADED TO /opt/worldvista/EHR/server-files
   ;"                                          FILE IS SPLIT INTO PAGES, EACH AS A PNG, AND STORED IN /tmp/EXPORT-$J/COVER*.PNG
   ;"                                          SENT IN PDF IS DELETED
   ;"               3 = CONSULTANT COVER SHEET - THE CONSULT TO USE IS COVERDATA("CONSULTANT")
   ;"                                            USE THE CONSULTANT FORM FOR THE PROVIDED CONSULTANT
   ;"               4 = TEMPLATE COVER SHEET - THE TEMPLATE TO USE IT IN COVERDATA("TEMPLATE")
   ;"                                          THIS WILL POINT TO AN ENTRY IN "TMG EXPORT COVER PAGES" FILE # 22744
   ;"              ONCE THE HTML IS CREATED FOR THE COVER IT WILL BE EXPORTED TO /tmp/EXPORT-$J 
   ;"              USING FILENAME "0-1-COVER.html"
   ;"            SECTION 1 - DEMOGRAPHICS
   ;"              IF COVERDATA("DEMOGRAPHICS") = 1 THEN THE DEMOGRAPHICS SHEET 
   ;"              WILL BE CREATED AND SAVED AS 1-1-DEMOGRAPHICS.html
   ;"            SECTION 2 - OFFICE NOTES
   ;"              USING THE ARRAY NOTESTOEXPORT (WHICH USES THE REF DATE AS THE INDEX), 
   ;"              EACH NOTE IS SAVED (IN DESCENDING ORDER)AS 
   ;"              2-"_FILECNTNAME_"-TIU"_TIUIEN_".html
   ;"            SECTION 3 - LAB RESULTS
   ;"              USING THE ARRAY LABSTOEXPORT (WHICH IS SET TO <FILEPATH>^<FILENAME>), 
   ;"              EACH RESULT PDF IS SAVED AS AN HTML FILE (IN DESCENDING ORDER)AS 
   ;"              "3-"_FILECNTNAME_"-LABS.html"
   ;"            SECTION 4 - RADIOLOGY RESULTS
   ;"              USING THE ARRAY RADTOEXPORT, 
   ;"              EACH RADIOLOGY REPORT IS EXPORTED AS 
   ;"              "4-"_FILECNTNAME_"-IMAGING.html"
   ;"            SECTION 5 - SCANNED DOCUMENTS
   ;"              USING THE SCANSTOEXPORT(FILENAME) ARRAY, EACH PDF LOCATED IN THE PATIENTS SCAN
   ;"              FOLDER IS SPLIT INTO INDIVIDUAL PNGS FOR EACH PAGE AND SAVED TO THE TEMP LOCATION
   ;"              USING THE PDFFILE NAME. THE HTML POINTING TO EACH PAGE IS CREATED AND SAVED AT
   ;"              "5-"_FILECNTNAME_"-SCAN.html"
   ;"            SECTION 6 - OTHER DOCUMENTS
   ;"              USING THE EXTRATOEXPORT(FILENAME) ARRAY, (IT IS EXPECTED THAT EACH FILE ALREADY 
   ;"              IS UPLOADED TO /opt/worldvista/EHR/server-files), EACH DOCUMENT IS SPLIT INTO
   ;"              INDIVIDUAL PNGS TO THE TEMP LOCATION USING THE FILE NAME. HTML IS CREATED POINTING
   ;"              TO EACH PAGE AND SAVED AT
   ;"              "6-"_FILECNTNAME_"-EXTRA.html"
   ;"              EACH PDF DOCUMENT IS DELETED AFTER BEING SPLIT, AS THE ARE NO LONGER NEEDED
   ;"            SECTION 7 - ORDERS
   ;"              USING THE ARRAY ORDERSTOEXPORT (WHICH USES ORDER IEN), 
   ;"              EACH ORDER IS SAVED AS
   ;"              "7-"_FILECNTNAME_"-ORDER.html"
   ;"              EACH PDF DOCUMENT IS DELETED AFTER BEING SPLIT, AS THE ARE NO LONGER NEEDED
   ;"         FINISHING THE EXPORT
   ;"            CREATE PDF
   ;"              ALL HTML DOCUMENTS ARE NOW CREATED AND LOCATED IN /tmp/EXPORT-$J
   ;"              USING THE LINUX COMMAND htmldoc --webpage "_FOLDERNAME_"/*.html --outfile "_DESTFOLDER_FILENAME 
   ;"              WHERE THE FOLDER IS "/opt/worldvista/EHR/images" AND THE FILENAME IS "EXPORT-"_DFN_"-"_$J_".pdf"
   ;"            SET PDF TO BE EXPORTED THROUGH RPC
   ;"              USING DOWNLOAD^TMGRPC1C THE PDF IS STORED IN THE GLOBAL FOR THE RPC RESULT
   ;"            CLEANUP
   ;"              NOW THAT THE RPC RESULT IS STORED IN THE GLOBAL, WE CAN CLEANUP ALL THE DIRECTORIES
   ;"              USING THE LINUX COMMAND rm, WE DELETE ALL THE CONTENTS OF /tmp/EXPORT-$J
   ;"              USING THE LINUX COMMAND rmdir, WE DELETE THE /tmp/EXPORT-$J FOLDER ITSELF
   ;"              USING THE LINUX COMMAND rm, WE DELETE THE PDF ITSELF        
   ;"          
   ;"**************************************************
   ;"    SAVE DATA FOR TESTING
   ;"    (CAN BE DELETED LATER)
   ;"**************************************************
   NEW TMGTEST SET TMGTEST=0
   IF TMGTEST=1 DO
   . SET TMGDFN=$G(^TMG("TMGTIUPR","DFN"))
   . MERGE NOTESTOEXPORT=^TMG("TMGTIUPR","NOTESTOEXPORT")
   . MERGE LABSTOEXPORT=^TMG("TMGTIUPR","LABSTOEXPORT")
   . MERGE RADTOEXPORT=^TMG("TMGTIUPR","RADTOEXPORT")
   . MERGE COVERDATA=^TMG("TMGTIUPR","COVERDATA")
   . MERGE SCANSTOEXPORT=^TMG("TMGTIUPR","SCANSTOEXPORT")
   . MERGE EXTRATOEXPORT=^TMG("TMGTIUPR","EXTRATOEXPORT")
   . MERGE ORDERSTOEXPORT=^TMG("TMGTIUPR","ORDERSTOEXPORT")
   ELSE  DO
   . KILL ^TMG("TMGTIUPR")
   . SET ^TMG("TMGTIUPR","DFN")=TMGDFN
   . MERGE ^TMG("TMGTIUPR","NOTESTOEXPORT")=NOTESTOEXPORT
   . MERGE ^TMG("TMGTIUPR","LABSTOEXPORT")=LABSTOEXPORT
   . MERGE ^TMG("TMGTIUPR","RADTOEXPORT")=RADTOEXPORT
   . MERGE ^TMG("TMGTIUPR","COVERDATA")=COVERDATA
   . MERGE ^TMG("TMGTIUPR","SCANSTOEXPORT")=SCANSTOEXPORT
   . MERGE ^TMG("TMGTIUPR","EXTRATOEXPORT")=EXTRATOEXPORT
   . MERGE ^TMG("TMGTIUPR","ORDERSTOEXPORT")=ORDERSTOEXPORT
   ;"
   ;"**************************************************
   ;"    CREATE THE WORKSPACE
   ;"**************************************************
   SET TMGRESULT="1^SUCCESSFUL"
   NEW FOLDERNAME,FILENAME
   SET FOLDERNAME="/tmp/EXPORT-"_$J
   SET GREF="^TMP(""DOWNLOAD^TMGRPC1"","_$J_")"
   K @GREF
   IF $$MKDIR^TMGKERNL(FOLDERNAME)'=0 DO  GOTO CEDN
   . SET @GREF@(0)="-1^COULD NOT CREATE FOLDER"
   ;"
   ;"**************************************************
   ;"    SETUP VARIABLES
   ;"**************************************************
   NEW FILECOUNT,FILECNTNAME SET FILECOUNT=1 ;"SET THE FILE COUNT AS WE WANT IT EXPORTED
   NEW NAME SET NAME=$P($G(^DPT(TMGDFN,0)),"^",1)
   NEW DOB SET DOB=$$EXTDATE^TMGDATE($P($G(^DPT(TMGDFN,0)),"^",3))
   ;"
   ;"**************************************************
   ;"    CREATE COVER SHEET - SECTION 0
   ;"**************************************************
   NEW CSMODE SET CSMODE=+$G(COVERDATA("MODE"))
   NEW COVERSHEET,COVERIDX
   SET COVERIDX=1  
   DO SETDFLTHDR(.COVERSHEET,.COVERIDX,NAME,DOB)
   IF CSMODE=0 DO        ;"ADD BLANK SHEET HERE
   . DO MAKEBCOVER(.COVERSHEET,.COVERIDX,.COVERDATA)
   ELSE  IF CSMODE=1 DO  ;"DEFAULT COVER
   . DO MAKECOVER(.COVERSHEET,.COVERIDX,.COVERDATA,NAME,DOB)
   ELSE  IF CSMODE=2 DO  ;"UPLOADED COVER
   . DO MAKEUPCOVER(.COVERSHEET,.COVERIDX,.COVERDATA,FOLDERNAME) 
   ELSE  IF CSMODE=3 DO  ;"CONSULT COVER
   . DO MAKECONCOVER(.COVERSHEET,.COVERIDX,.COVERDATA)
   ELSE  IF CSMODE=4 DO  ;"TEMPLATE COVER
   . DO MAKETEMCOVER(.COVERSHEET,.COVERIDX,.COVERDATA)
   SET FILENAME="0-"_1_"-COVER.html"
   DO EXPHTML(.COVERSHEET,FOLDERNAME,FILENAME,1)
   ;"
   ;"**************************************************
   ;"    DEMOGRAPHICS SHEET - SECTION 1 
   ;"**************************************************
   IF $G(COVERDATA("DEMOGRAPHICS"))'=0 DO
   . NEW DEMOSHEET
   . SET FILENAME="1-1-DEMOGRAPHICS.html"
   . DO DEMOGRAP(.DEMOSHEET,TMGDFN)
   . DO EXPHTML(.DEMOSHEET,FOLDERNAME,FILENAME,1)
   ;"
   ;"**************************************************
   ;"    TIU NOTES - SECTION 2
   ;"**************************************************
   NEW TIUDATE SET TIUDATE=9999999
   NEW TMGTEST SET TMGTEST=0
   SET BREAK="<p style=""page-break-before: always"">"  ;"THIS MAY NOT BE NEEDED
   ;"
   FOR  SET TIUDATE=$O(NOTESTOEXPORT(TIUDATE),-1) QUIT:TIUDATE'>0  DO
   . NEW TIUIEN SET TIUIEN=9999999
   . FOR  SET TIUIEN=$O(NOTESTOEXPORT(TIUDATE,TIUIEN),-1) QUIT:TIUIEN'>0  DO
   . . IF $$EXCLUDE(TIUIEN)=1 QUIT
   . . NEW ISHTML SET ISHTML=$$ISHTML^TMGHTM1(TIUIEN)
   . . NEW REF,NOTEARR
   . . SET FILECNTNAME=$$FMTFILEN(2,FILECOUNT)
   . . SET FILENAME="2-"_FILECNTNAME_"-TIU"_TIUIEN_".html"  ;"1 is the order number, followed by filecount, then TIU number for reference
   . . SET FILECOUNT=FILECOUNT+1
   . . DO TGET^TIUSRVR1(.REF,TIUIEN,"VIEW;A")
   . . MERGE NOTEARR=@REF
   . . ;"NEW NOTETYPE SET NOTETYPE=$P($G(^TIU(8925,TIUIEN,0)),"^",1)
   . . SET NOTEARR(1)=$G(NOTEARR)_"<!-- HEADER CENTER ""DOS: "_$$EXTDATE^TMGDATE($P($G(^TIU(8925,TIUIEN,13)),"^",1),1)_""" -->"
   . . ;"SET NOTEARR(1)=$G(NOTEARR)_"<!-- HEADER CENTER "" "_FILENAME_""" -->"
   . . DO EXPHTML(.NOTEARR,FOLDERNAME,FILENAME,ISHTML)
   . . ;"WRITE BREAK,!
   ;"
   ;"**************************************************
   ;"    LAB RESULTS - SECTION 3
   ;"**************************************************
   SET FILECOUNT=1
   NEW LABDATA SET LABDATA="ZZ"
   FOR  SET LABDATA=$O(LABSTOEXPORT(LABDATA),-1) QUIT:LABDATA=""  DO
   . ;"NEW THISLAB SET THISLAB=$G(
   . NEW LABHTML
   . NEW ROOTPATH SET ROOTPATH=$$GETROOTPATH^TMGLRPD1() ;
   . NEW FULLPATH SET FULLPATH=ROOTPATH_$P(LABDATA,"^",1)
   . NEW LABFILENAME SET LABFILENAME=$P(LABDATA,"^",2)
   . SET FILECNTNAME=$$FMTFILEN(3,FILECOUNT)
   . DO PDFTOHTML(.LABHTML,FULLPATH,LABFILENAME,FOLDERNAME,FILECNTNAME,0)
   . SET FILENAME="3-"_FILECNTNAME_"-LABS.html",FILECOUNT=FILECOUNT+1
   . DO EXPHTML(.LABHTML,FOLDERNAME,FILENAME,1)
   . ;"NEW LABHTML DO GETREPRT^TMGRPCL1(.LABHTML,TMGDFN,.SINGLEDT)
   . ;"SET FILECNTNAME=$$FMTFILEN(FILECOUNT)
   . ;"SET FILENAME="3-"_FILECNTNAME_"-LABS.html",FILECOUNT=FILECOUNT+1
   . ;"SET LABHTML(1)=$G(LABHTML)_"<!-- HEADER CENTER ""LAB RESULTS: "_$$EXTDATE^TMGDATE(LABDATE,1)_""" -->"
   . ;"DO EXPHTML(.LABHTML,FOLDERNAME,FILENAME,1)
   . ;"MERGE ^TMP("LABHTML",FILECOUNT)=LABHTML
   ;"
   ;"**************************************************
   ;"    RADIOLOGY REPORTS - SECTION 4
   ;"**************************************************
   SET FILECOUNT=1
   NEW RADREPORT SET RADREPORT=""
   FOR  SET RADREPORT=$O(RADTOEXPORT(RADREPORT)) QUIT:RADREPORT=""  DO
   . NEW ROOT,RADARRAY,RADHTML
   . DO RPT^ORWRP(.ROOT,TMGDFN,"18:IMAGING (LOCAL ONLY)~","","",RADREPORT,0,0)
   . MERGE RADARRAY=@ROOT
   . NEW RADIDX SET RADIDX=0
   . NEW RADHTML
   . FOR  SET RADIDX=$O(RADARRAY(RADIDX)) QUIT:RADIDX'>0  DO
   . . SET RADHTML(RADIDX)=$G(RADARRAY(RADIDX,0))_"<br>"
   . SET RADHTML(1)="<HTML><BODY><TABLE BORDER=1 BGCOLOR=#ffffe6 style=""table-layout:fixed; width:800px""><tr><td><div style = ""width:700px; word-wrap: break-word""> <!-- HEADER CENTER ""IMAGING RESULTS"" --> "_$G(RADHTML(1))
   . SET RADHTML($O(RADHTML(9999999),-1))=",</td></tr></TABLE></BODY></HTML>"
   . SET FILECNTNAME=$$FMTFILEN(4,FILECOUNT)   
   . SET FILENAME="4-"_FILECNTNAME_"-IMAGING.html",FILECOUNT=FILECOUNT+1
   . DO EXPHTML(.RADHTML,FOLDERNAME,FILENAME,1)
   ;"
   ;"**************************************************
   ;"    SCANNED DOCUMENTS - SECTION 5
   ;"**************************************************
   NEW SCANFILE 
   SET SCANFILE="",FILECOUNT=1
   FOR  SET SCANFILE=$O(SCANSTOEXPORT(SCANFILE)) QUIT:SCANFILE=""  DO
   . NEW SCANHTML
   . SET FILECNTNAME=$$FMTFILEN(5,FILECOUNT)
   . DO SCN2HTML(.SCANHTML,SCANFILE,FOLDERNAME,FILECNTNAME)
   . SET FILENAME="5-"_FILECNTNAME_"-SCAN.html",FILECOUNT=FILECOUNT+1
   . DO EXPHTML(.SCANHTML,FOLDERNAME,FILENAME,1)
   ;"   
   ;"**************************************************
   ;"    EXTRA DOCUMENTS  - SECTION 6
   ;"**************************************************
   ;"Note: for now this only handles PDFs. We can later expand to 
   ;"      handle more file types
   NEW EXTRAFILE 
   SET EXTRAFILE="",FILECOUNT=1
   FOR  SET EXTRAFILE=$O(EXTRATOEXPORT(EXTRAFILE)) QUIT:EXTRAFILE=""  DO
   . NEW EXTRAHTML
   . SET FILECNTNAME=$$FMTFILEN(6,FILECOUNT)
   . DO PDFTOHTML(.EXTRAHTML,"/opt/worldvista/EHR/server-files/",EXTRAFILE,FOLDERNAME,FILECNTNAME,1)
   . SET FILENAME="6-"_FILECNTNAME_"-EXTRA.html",FILECOUNT=FILECOUNT+1
   . DO EXPHTML(.EXTRAHTML,FOLDERNAME,FILENAME,1)
   ;"
   ;"**************************************************
   ;"    ORDERS - SECTION 7
   ;"**************************************************
   SET FILECOUNT=1
   NEW ORDERIEN SET ORDERIEN=0
   FOR  SET ORDERIEN=$O(ORDERSTOEXPORT(ORDERIEN)) QUIT:ORDERIEN=""  DO
   . NEW ORDERTEXT,THISOIDX
   . NEW IFN,ORTA
   . SET IFN=$P(ORDERIEN,";",1),ORTA=$P(ORDERIEN,";",2)
   . NEW ORDERZN,ORDERSTATUS SET ORDERZN=$G(^OR(100,IFN,8,ORTA,0))
   . SET ORDERSTATUS=$P(ORDERZN,"^",4)
   . NEW ORDERSIGNDT,ORDERSIGNER
   . SET ORDERSIGNDT=$P(ORDERZN,"^",6),ORDERSIGNER=$P(ORDERZN,"^",5)
   . NEW ORDACTION SET ORDACTION=$P(ORDERZN,"^",2)
   . IF ORDACTION'="NW" QUIT
   . SET THISORDIDX=2
   . NEW ORDERHTML
   . SET ORDERHTML(1)="<HTML><BODY><TABLE BORDER=1 BGCOLOR=#ffffe6 style=""table-layout:fixed; width:800px""><tr><td><div style = ""width:700px; word-wrap: break-word""> <!-- HEADER CENTER ""PHYSICIAN ORDER"" --> "
   . SET ORDERHTML(THISORDIDX)="<HR>",THISORDIDX=THISORDIDX+1
   . SET ORDERHTML(THISORDIDX)="              PHYSICIAN ORDER #"_IFN_"             <BR>",THISORDIDX=THISORDIDX+1
   . SET ORDERHTML(THISORDIDX)="<HR>",THISORDIDX=THISORDIDX+1
   . SET ORDERHTML(THISORDIDX)="           FAMILY PHYSICIANS OF GREENEVILLE         <BR>",THISORDIDX=THISORDIDX+1
   . SET ORDERHTML(THISORDIDX)="             1410 TUSCULUM BLVD SUITE 2600          <BR>",THISORDIDX=THISORDIDX+1
   . SET ORDERHTML(THISORDIDX)="                GREENEVILLE, TN 37745               <BR>",THISORDIDX=THISORDIDX+1
   . SET ORDERHTML(THISORDIDX)="       PHONE: 423-787-7000   FAX: 423-787-7049      <BR>",THISORDIDX=THISORDIDX+1
   . SET ORDERHTML(THISORDIDX)="<HR>>",THISORDIDX=THISORDIDX+1
   . DO TEXT^ORQ12(.ORDERTEXT,ORDERIEN,"")
   . NEW ORDERIDX SET ORDERIDX=0
   . FOR  SET ORDERIDX=$O(ORDERTEXT(ORDERIDX)) QUIT:ORDERIDX'>0  DO
   . . SET ORDERHTML(THISORDIDX)=$G(ORDERTEXT(ORDERIDX))_"<br>",THISORDIDX=THISORDIDX+1
   . SET ORDERHTML(THISORDIDX)="<HR>",THISORDIDX=THISORDIDX+1
   . IF ORDERSTATUS=2 DO
   . . SET ORDERHTML(THISORDIDX)="****  UNSIGNED  ****",THISORDIDX=THISORDIDX+1 
   . ELSE  DO
   . . SET ORDERHTML(THISORDIDX)="/es/"_$P($G(^VA(200,ORDERSIGNER,20)),"^",2)_"<BR>",THISORDIDX=THISORDIDX+1
   . . SET ORDERHTML(THISORDIDX)=$P($G(^VA(200,ORDERSIGNER,20)),"^",3)_"<BR>",THISORDIDX=THISORDIDX+1
   . . SET ORDERHTML(THISORDIDX)="Signed: "_$$EXTDATE^TMGDATE(ORDERSIGNDT),THISORDIDX=THISORDIDX+1
   . SET ORDERHTML(THISORDIDX)=",</td></tr></TABLE></BODY></HTML>"
   . SET FILECNTNAME=$$FMTFILEN(7,FILECOUNT)   
   . SET FILENAME="7-"_FILECNTNAME_"-ORDER.html",FILECOUNT=FILECOUNT+1
   . DO EXPHTML(.ORDERHTML,FOLDERNAME,FILENAME,1)
   ;"
   ;"**************************************************
   ;"    EXPORT THE HTML SET INTO ONE PDF
   ;"**************************************************
   NEW DESTFOLDER SET DESTFOLDER="/opt/worldvista/EHR/images/"
   NEW FILENAME SET FILENAME="EXPORT-"_TMGDFN_"-"_$J_".pdf"
   NEW LINUXCMD SET LINUXCMD="htmldoc --webpage "_FOLDERNAME_"/*.html --outfile "_DESTFOLDER_FILENAME
   NEW EXPRESULT
   SET EXPRESULT=$$LINUXCMD^TMGKERNL(LINUXCMD)
   IF $P(EXPRESULT,"^",1)'=1 DO  GOTO CEDN
   . SET @GREF@(0)=EXPRESULT
   ;"
   ;"**************************************************
   ;"    RETURN THE CREATED PDF TO THE CLIENT THROUGH
   ;"    THE RPC CALL
   ;"**************************************************  
   DO DOWNLOAD^TMGRPC1C(GREF,DESTFOLDER,FILENAME,"")
   ;"
   ;"**************************************************
   ;"    CLEANUP THE WORKSPACE
   ;"**************************************************
   NEW LINUXRESULT
   SET LINUXRESULT=$$LINUXCMD^TMGKERNL("rm "_FOLDERNAME_"/*.*")
   SET LINUXRESULT=$$LINUXCMD^TMGKERNL("rmdir "_FOLDERNAME)
   SET LINUXRESULT=$$LINUXCMD^TMGKERNL("rm "_DESTFOLDER_FILENAME)
   ;"
   ;"**************************************************
   ;"    CLEANUP THE WORKSPACE
   ;"**************************************************
   NEW STORERESULT
   SET STORERESULT=$$STORINFO(TMGDFN,.NOTESTOEXPORT,.LABSTOEXPORT,.RADTOEXPORT,.COVERDATA,.SCANSTOEXPORT,.ORDERSTOEXPORT)  ;"
CEDN   
   QUIT
   ;"
FMTFILEN(SECTION,FILECNTNAME)
   ;"Purpose: take the current file count and add leading zeros until it gets to a length of 5
   ;"         4/21/21 - Added section parameter, to keep all images separate
   FOR  QUIT:$L(FILECNTNAME)>3  DO  
   . SET FILECNTNAME="0"_FILECNTNAME 
   SET FILECNTNAME=SECTION_FILECNTNAME
   QUIT FILECNTNAME
   ;"
EXPHTML(ARRTOEXP,FOLDERNAME,FILENAME,ISHTML)  ;"
   ;"Purpose: convert the sent Array to an HTML document, stored at FOLDERNAME/FILENAME 
   ;"         if ISHTML=0, then add proper <BR> tags after each array element.
   NEW HANDLE,PATH
   NEW EOL SET EOL=""
   IF ISHTML=0 SET EOL="<br>"
   SET HANDLE="TMGEXPORT"
   SET PATH=FOLDERNAME
   IF $E(PATH,$L(PATH),$L(PATH))'="/" SET PATH=PATH_"/"
   DO OPEN^%ZISH(HANDLE,PATH,FILENAME,"W")
   IF POP DO  GOTO EHDN
   . ;"write "Unable to open file for writing: ",PATH,FILENAME,!
   USE IO
   NEW IDX SET IDX=0
   FOR  SET IDX=$O(ARRTOEXP(IDX)) QUIT:IDX'>0  DO
   . IF $G(ARRTOEXP(IDX))["KTESIG" DO
   . . SET ARRTOEXP(IDX)=$P($G(ARRTOEXP(IDX)),"file:///$CPRSDIR$/Cache",1)_"/opt/worldvista/EHR/server-files"_$P($G(ARRTOEXP(IDX)),"file:///$CPRSDIR$/Cache",2)
   . IF $G(ARRTOEXP(IDX))["$CPRSDIR$" DO
   . . WRITE $$GETIPATH($G(ARRTOEXP(IDX))),EOL,!
   . ELSE  DO
   . . WRITE $G(ARRTOEXP(IDX)),EOL,!
   DO CLOSE^%ZISH(HANDLE)
EHDN
   QUIT
   ;"
MAKECOVER(COVERSHEET,COVERIDX,COVERDATA,NAME,DOB)
   ;"Purpose: create the default coversheet
   SET COVERSHEET(COVERIDX)="<html><body>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="<B>FAMILY PHYSICIANS OF GREENEVILLE PC.</B><BR>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="Laughlin Medical Building #1<BR>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="1410 Tusculum Blvd. Suite 2600<BR>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="Greeneville, TN. 37745<BR>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="VOICE: (423)787-7000    FAX: (423)787-7049<BR><HR>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="<P><P>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="<TABLE width=""700""><TR><TD width=""350"">To: <u>"_$G(COVERDATA("TO"))_"</u></TD>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="<TD width=""350"">Fax: <u>"_$G(COVERDATA("FAX"))_"</u></TD></TR>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="<TR><TD>From: <u>"_$P($G(^VA(200,DUZ,0)),"^",1)_"</u></TD>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="<TD>Fax: <u>(423)787-7049</u></TD></TR>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="<TR colspan=""2""><TD>Patient: <u>"_NAME_" ("_DOB_")"_"</u></TD></TR>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="<TR colspan=""2""><TD>Date: <u>"_$$TODAY^TMGDATE(1,1)_"</u></TD></TR>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="<TR colspan=""2""><TD>RE: <u>"_$G(COVERDATA("RE"))_"</u></TD></TR>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="<TR colspan=""2""><TD>Comments: <u>",COVERIDX=COVERIDX+1
   NEW I SET I=0 
   FOR  SET I=$O(COVERDATA("COMMENTS",I)) QUIT:I'>0  DO
   . SET COVERSHEET(COVERIDX)=$G(COVERDATA("COMMENTS",I))_" ",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="</u></TD></TR></TABLE><BR><HR>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="<P><P><P>CONFIDENTIALITY CLAUSE<P>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="The information contained in this facsimile message is legally privileged and confidential information<br>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="intended only for the use of the individual or entity named above. If the reader of this message is not<BR>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="the intended recipient, you are hereby notified that any dissemination, distribution or copying of the<br>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="telecopy is prohibited. If you have received this telecopy in error, please immediately notify us by<br>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="telephone.<p>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="                                                              Thank You<p></body></html>",COVERIDX=COVERIDX+1
   QUIT
   ;"
MAKEBCOVER(COVERSHEET,COVERIDX,COVERDATA)
   ;"Purpose: Make a blank cover sheet
   ;"   Nothing to add. Place holder just in case we eventually want to force some form of cover
   QUIT
   ;"
MAKEUPCOVER(COVERSHEET,COVERIDX,COVERDATA,DESTFOLDER)
   ;"Purpose: Make a cover for uploaded file
   NEW PDFFILE SET PDFFILE="/opt/worldvista/EHR/server-files/"_$G(COVERDATA("UPLOADED"))
   NEW LINUXCMD SET LINUXCMD="pdftoppm """_PDFFILE_""" "_DESTFOLDER_"/COVER -png"
   NEW EXPRESULT
   SET EXPRESULT=$$LINUXCMD^TMGKERNL(LINUXCMD)
   NEW TEMPMASK,TMGFILES SET TEMPMASK("COVER*.png")=""
   IF $$LIST^%ZISH(DESTFOLDER,"TEMPMASK","TMGFILES")=0 QUIT
   NEW INDEX SET INDEX=""
   SET COVERSHEET(COVERIDX)="<HTML><BODY>",COVERIDX=COVERIDX+1
   FOR  SET INDEX=$ORDER(TMGFILES(INDEX)) QUIT:(INDEX="")  DO
   . NEW FNAME,FPNAME
   . SET FNAME=INDEX
   . SET FPNAME=DESTFOLDER_"/"_FNAME
   . SET COVERSHEET(COVERIDX)="<IMG src="""_FPNAME_""" width=640 height=836><p>",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="</BODY></HTML>"
   NEW LINUXRESULT
   SET LINUXRESULT=$$LINUXCMD^TMGKERNL("rm "_PDFFILE)
   QUIT
   ;"
MAKECONCOVER(COVERSHEET,COVERIDX,COVERDATA)
   ;"Purpose:; Make a consult cover sheet
   NEW REF
   DO DETAIL^ORQQCN(.REF,$G(COVERDATA("CONSULTANT")))
   MERGE COVERSHEET=@REF
   NEW IDX,FOUND,DONE
   SET IDX=0,FOUND=0,DONE=0
   ZWR COVERSHEET
   FOR  SET IDX=$ORDER(COVERSHEET(IDX)) QUIT:(IDX'>0)!(DONE=1)  DO
   . NEW LINE SET LINE=$G(COVERSHEET(IDX,0))
   . IF LINE["Reason For Request:" DO  QUIT
   . . SET FOUND=1
   . IF LINE["Patient Notified" SET DONE=1
   . IF FOUND=0 QUIT
   . SET COVERSHEET(COVERIDX)=LINE_"<br>",COVERIDX=COVERIDX+1
   QUIT
   ;"
MAKETEMCOVER(COVERSHEET,COVERIDX,COVERDATA)
   ;"Purpose: Make a cover sheet from template
   NEW TEMPLATEIEN SET TEMPLATEIEN=+$G(COVERDATA("TEMPLATE"))
   IF TEMPLATEIEN'>0 QUIT
   NEW PATIENTDATA DO GTPATDATA(.PATIENTDATA,0)
   NEW IDX SET IDX=0
   FOR  SET IDX=$O(^TMG(22744,TEMPLATEIEN,1,IDX)) QUIT:IDX'>0  DO
   . NEW LINE SET LINE=$G(^TMG(22744,TEMPLATEIEN,1,IDX,0))
   . SET LINE=$$CNVERTLN(LINE,.PATIENTDATA)
   . SET COVERSHEET(COVERIDX)=LINE,COVERIDX=COVERIDX+1
   QUIT
   ;"
GTPATDATA(PATARRAY,TMGDFN)
   SET PATARRAY("@@PATIENTNAME@@")="ZZTEST,PATIENT"
   QUIT
   ;"
CNVERTLN(LINE,PATARRAY)
   NEW LABEL SET LABEL=""
   FOR  SET LABEL=$O(PATARRAY(LABEL)) QUIT:LABEL=""  DO
   . IF LINE[LABEL DO
   . . SET LINE=$P(LINE,LABEL,1)_$G(PATARRAY(LABEL))_$P(LINE,LABEL,2)
   QUIT LINE
   ;"
SETDFLTHDR(COVERSHEET,COVERIDX,NAME,DOB)
   ;"Purpose: create the coversheet header, which will be persisitent throughout the document
   SET COVERSHEET(COVERIDX)="<!-- FOOTER LEFT ""FAMILY PHYSICIANS OF GREENEVILLE"" -->",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="<!-- FOOTER CENTER "" "" -->",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="<!-- FOOTER RIGHT ""PAGE: $PAGE"" -->",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="<!-- HEADER CENTER """_""" -->",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="<!-- HEADER LEFT ""PATIENT : "_NAME_""" -->",COVERIDX=COVERIDX+1
   SET COVERSHEET(COVERIDX)="<!-- HEADER RIGHT ""DOB: "_DOB_""" -->",COVERIDX=COVERIDX+1
   QUIT
   ;"
GETIPATH(LINE)
   ;"Purpose: take the LINE and resolve any image tags to the proper local location (recursive)
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
   ;"Purpose: take the FILENAME, and convert it into the correct local path
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
DEMOGRAP(DEMOSHEET,TMGDFN)
   ;"CREATE A DEMOGRAPHIC SHEET AND RETURN IT AS HTML IN THE DEMOSHEET ARRAY
   NEW IDX SET IDX=1
   SET DEMOSHEET(IDX)="<HTML><BODY>",IDX=IDX+1
   SET DEMOSHEET(IDX)="<TABLE width=""700"" CENTER><TR><TD style=""text-align: center;"">PATIENT DEMOGRAPHICS</TD></TR></TABLE>",IDX=IDX+1
   ;"
   ;"Create a master table to hold the other tables
   SET DEMOSHEET(IDX)="<TABLE width=""700""><TR><TD width=""350"">",IDX=IDX+1
   ;"
   ;"Create table for patient demo
   NEW ZN SET ZN=$G(^DPT(TMGDFN,0))
   NEW ADDRNODE SET ADDRNODE=$G(^DPT(TMGDFN,.11))
   NEW STATE SET STATE=$P(^DIC(5,$P(ADDRNODE,"^",5),0),"^",1)
   NEW ZIP SET ZIP=$P(ADDRNODE,"^",6)
   IF ZIP="" SET ZIP=$P(ADDRNODE,"^",12)
   ;"SET DEMOSHEET(IDX)="<TABLE BORDER=1>",IDX=IDX+1
   SET DEMOSHEET(IDX)="<TR><TD><B>PATIENT INFORMATION</B></TD></TR>",IDX=IDX+1
   SET DEMOSHEET(IDX)="<TR><TD>Account #:</TD><TD>"_$P($G(^DPT(TMGDFN,"TMG")),"^",2)_"</TD></TR>",IDX=IDX+1
   SET DEMOSHEET(IDX)="<TR><TD>First Name:</TD><TD>"_$P($P(ZN,"^",1),",",2)_"</TD></TR>",IDX=IDX+1
   SET DEMOSHEET(IDX)="<TR><TD>Last Name:</TD><TD>"_$P($P(ZN,"^",1),",",1)_"</TD></TR>",IDX=IDX+1
   SET DEMOSHEET(IDX)="<TR><TD>DOB:</TD><TD>"_$$EXTDATE^TMGDATE($P(ZN,"^",3),1)_"</TD></TR>",IDX=IDX+1
   SET DEMOSHEET(IDX)="<TR><TD>Sex:</TD><TD>"_$P(ZN,"^",2)_"</TD></TR>",IDX=IDX+1
   SET DEMOSHEET(IDX)="<TR><TD>SSN:</TD><TD>"_$P(ZN,"^",9)_"</TD></TR>",IDX=IDX+1
   SET DEMOSHEET(IDX)="<TR><TD>Address:</TD><TD>"_$P(ADDRNODE,"^",1)_"</TD></TR>",IDX=IDX+1
   SET DEMOSHEET(IDX)="<TR><TD>City:</TD><TD>"_$P(ADDRNODE,"^",4)_"</TD></TR>",IDX=IDX+1
   SET DEMOSHEET(IDX)="<TR><TD>State:</TD><TD>"_STATE_"</TD></TR>",IDX=IDX+1
   SET DEMOSHEET(IDX)="<TR><TD>Zip:</TD><TD>"_ZIP_"</TD></TR>",IDX=IDX+1
   SET DEMOSHEET(IDX)="<TR><TD>Home Phone:</TD><TD>"_$P($G(^DPT(TMGDFN,.13)),"^",1)_"</TD></TR>",IDX=IDX+1
   SET DEMOSHEET(IDX)="<TR><TD>Cell Phone:</TD><TD>"_$P($G(^DPT(TMGDFN,.13)),"^",4)_"</TD></TR>",IDX=IDX+1
   NEW PROV SET PROV=+$P($G(^DPT(TMGDFN,"TMGPCP")),"^",1)
   IF PROV>0 DO
   . SET DEMOSHEET(IDX)="<TR><TD>Primary Care Provider:</TD><TD>"_$P($G(^VA(200,PROV,0)),"^",1)_"</TD></TR>",IDX=IDX+1
   ;"SET DEMOSHEET(IDX)="</TABLE></TD><TD width=""350"">",IDX=IDX+1
   ;"
   ;"Create table to hold insurance information
   ;"SET DEMOSHEET(IDX)="<TABLE BORDER=1>",IDX=IDX+1
   NEW INSIDX,INSIEN,INSCOUNT SET (INSCOUNT,INSIDX)=0
   FOR  SET INSIDX=$ORDER(^DPT(TMGDFN,.312,INSIDX)) QUIT:INSIDX'>0  DO
   . SET INSIEN=$P($GET(^DPT(TMGDFN,.312,INSIDX,0)),"^",1)
   . NEW INSNAME SET INSNAME=$PIECE($GET(^DIC(36,INSIEN,0)),"^",1)
   . SET INSCOUNT=INSCOUNT+1
   . NEW COB SET COB=+$P($G(^DPT(TMGDFN,.312,INSIDX,0)),"^",20)
   . IF COB'>0 SET COB="#"
   . NEW INSID SET INSID=$G(^DPT(TMGDFN,.312,INSIDX,5))
   . SET DEMOSHEET(IDX)="<TR><TD><B>INSURANCE INFO</B></TD></TR>",IDX=IDX+1
   . SET DEMOSHEET(IDX)="<TR><TD>Insurance:</TD><TD>"_INSNAME_"</TD></TR>",IDX=IDX+1
   . SET DEMOSHEET(IDX)="<TR><TD>Insurance ID:</TD><TD>"_INSID_"</TD></TR>",IDX=IDX+1
   . SET DEMOSHEET(IDX)="</TABLE></BODY></HTML>"
   QUIT
   ;"
SCN2HTML(HTMLARRAY,PDFFILE,DESTFOLDER,FILECOUNT)
   ;"Purpose: Take a PDF file, split it into individual images and then
   ;"         create an HTML file to view all the images as separate pages
   NEW PDFPATH 
   SET PDFPATH="/mnt/WinPrivate/FPG Charts/"_$p(PDFFILE,"/filesystem/oldrecs/",2)
   NEW LINUXCMD SET LINUXCMD="pdftoppm """_PDFPATH_""" "_DESTFOLDER_"/SCANNEDFILE"_FILECOUNT_" -png"
   NEW EXPRESULT
   SET EXPRESULT=$$LINUXCMD^TMGKERNL(LINUXCMD)
   NEW TEMPMASK,TMGFILES SET TEMPMASK("SCANNEDFILE"_FILECOUNT_"*.png")=""
   IF $$LIST^%ZISH(DESTFOLDER,"TEMPMASK","TMGFILES")=0 QUIT
   NEW INDEX SET INDEX=""
   SET HTMLARRAY(1)="<!-- HEADER CENTER ""SCANNED DOCUMENT"" -->"
   NEW HTMLIDX SET HTMLIDX=2
   SET HTMLARRAY(HTMLIDX)="<HTML><BODY>",HTMLIDX=HTMLIDX+1
   FOR  SET INDEX=$ORDER(TMGFILES(INDEX)) QUIT:(INDEX="")  DO
   . NEW FNAME,FPNAME
   . SET FNAME=INDEX
   . SET FPNAME=DESTFOLDER_"/"_FNAME
   . SET HTMLARRAY(HTMLIDX)="<IMG src="""_FPNAME_""" width=640 height=836><p>",HTMLIDX=HTMLIDX+1
   SET HTMLARRAY(HTMLIDX)="</BODY></HTML>"
   QUIT
   ;"
PDFTOHTML(HTMLARRAY,PDFPATH,PDFFILE,DESTFOLDER,FILECOUNT,REMOVE)
   ;"Purpose: Take a PDF file, split it into individual images and then
   ;"         create an HTML file to view all the images as separate pages
   ;"    We won't assume the file path any longer. It must be sent now
   SET REMOVE=+$G(REMOVE)
   IF PDFPATH="" SET PDFPATH="/opt/worldvista/EHR/server-files/"
   SET PDFPATH=PDFPATH_PDFFILE
   NEW LINUXCMD SET LINUXCMD="pdftoppm """_PDFPATH_""" "_DESTFOLDER_"/PDFFILE"_FILECOUNT_" -png"
   NEW EXPRESULT
   SET EXPRESULT=$$LINUXCMD^TMGKERNL(LINUXCMD)
   NEW TEMPMASK,TMGFILES SET TEMPMASK("PDFFILE"_FILECOUNT_"*.png")=""
   IF $$LIST^%ZISH(DESTFOLDER,"TEMPMASK","TMGFILES")=0 QUIT
   NEW INDEX SET INDEX=""
   SET HTMLARRAY(1)="<!-- HEADER CENTER ""IMPORTED DOCUMENT"" -->"
   NEW HTMLIDX SET HTMLIDX=2
   SET HTMLARRAY(HTMLIDX)="<HTML><BODY>",HTMLIDX=HTMLIDX+1
   FOR  SET INDEX=$ORDER(TMGFILES(INDEX)) QUIT:(INDEX="")  DO
   . NEW FNAME,FPNAME
   . SET FNAME=INDEX
   . SET FPNAME=DESTFOLDER_"/"_FNAME
   . SET HTMLARRAY(HTMLIDX)="<IMG src="""_FPNAME_""" width=640 height=836><p>",HTMLIDX=HTMLIDX+1
   SET HTMLARRAY(HTMLIDX)="</BODY></HTML>"
   IF REMOVE=1 SET LINUXRESULT=$$LINUXCMD^TMGKERNL("rm "_PDFPATH)
   QUIT
   ;"
MERGEPDF(DESTFOLDER,FILEARRAY,RESULTFILE)  ;"

   QUIT
   ;"
GETTEMPL(TMGRESULTS)  ;"GET ALL TEMPLATES
   NEW IDX SET IDX=0
   FOR  SET IDX=$O(^TMG(22744,IDX)) QUIT:+$G(IDX)'>0  DO
   . SET TMGRESULTS(IDX)=IDX_"^"_$G(^TMG(22744,IDX,0))
   QUIT
   ;"
   ;"================================================================
   ;"  SAVE INFORMATION BELOW
   ;"================================================================
STORINFO(TMGDFN,NOTESTOEXPORT,LABSTOEXPORT,RADTOEXPORT,COVERDATA,SCANSTOEXPORT,ORDERSTOEXPORT)  ;"
   ;"THIS ROUTINE WILL SAVE THE EXPORT INFORMATION TO THE 
   ;"'TMG RECORDS SENT HISTORY' FILEMAN FILE #22748
   NEW TMGFDA,TMGMSG,TMGIEN
   NEW STORERESULT SET STORERESULT="1^SUCCESS"
   SET TMGFDA(22748,"+1,",.01)=TMGDFN
   SET TMGFDA(22748,"+1,",.02)=DUZ
   SET TMGFDA(22748,"+1,",.03)=$G(COVERDATA("TO"))
   SET TMGFDA(22748,"+1,",.04)=$G(COVERDATA("FAX"))
   SET TMGFDA(22748,"+1,",.05)=$G(COVERDATA("RE"))
   SET TMGFDA(22748,"+1,",.06)=$G(COVERDATA("COMMENTS"))
   SET TMGFDA(22748,"+1,",.07)=$G(COVERDATA("MODE"))
   SET TMGFDA(22748,"+1,",.08)=$G(COVERDATA("CONSULTANT"))
   SET TMGFDA(22748,"+1,",.09)=$G(COVERDATA("DEMOGRAPHICS"))
   NEW X DO NOW^%DTC
   SET TMGFDA(22748,"+1,",.1)=%
   DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
   IF $DATA(TMGMSG("DIERR")) DO  GOTO STINFODN
   . NEW MSG SET MSG=$$GETERRST^TMGDEBU2(.TMGMSG)
   . SET STORERESULT="-1^ERROR STORING DATA: "_MSG
   NEW TMGIENS SET TMGIENS="+1,"_$G(TMGIEN(1))_","
   ;"NOTES
   NEW TIUDATE SET TIUDATE=9999999
   FOR  SET TIUDATE=$O(NOTESTOEXPORT(TIUDATE),-1) QUIT:TIUDATE'>0  DO
   . NEW TIUIEN SET TIUIEN=9999999
   . FOR  SET TIUIEN=$O(NOTESTOEXPORT(TIUDATE,TIUIEN),-1) QUIT:TIUIEN'>0  DO
   . . KILL TMGMSG,TMGFDA,TMGIEN
   . . SET TMGFDA(22748.01,TMGIENS,.01)=TIUIEN
   . . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
   ;"LAB RESULTS
   NEW LABDATA SET LABDATA="ZZ"
   FOR  SET LABDATA=$O(LABSTOEXPORT(LABDATA),-1) QUIT:LABDATA=""  DO
   . KILL TMGMSG,TMGFDA,TMGIEN
   . SET TMGFDA(22748.02,TMGIENS,.01)=LABDATA
   . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
   ;"RAD REPORTS
   NEW RADREPORT SET RADREPORT=""
   FOR  SET RADREPORT=$O(RADTOEXPORT(RADREPORT)) QUIT:RADREPORT=""  DO
   . KILL TMGMSG,TMGFDA,TMGIEN
   . SET TMGFDA(22748.03,TMGIENS,.01)=RADREPORT
   . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
   ;"SCAN FILES
   NEW SCANFILE SET SCANFILE=""
   FOR  SET SCANFILE=$O(SCANSTOEXPORT(SCANFILE)) QUIT:SCANFILE=""  DO
   . KILL TMGMSG,TMGFDA,TMGIEN
   . SET TMGFDA(22748.04,TMGIENS,.01)=SCANFILE
   . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
   ;"ORDERS
   NEW ORDERIEN SET ORDERIEN=0
   FOR  SET ORDERIEN=$O(ORDERSTOEXPORT(ORDERIEN)) QUIT:ORDERIEN=""  DO
   . KILL TMGMSG,TMGFDA,TMGIEN
   . SET TMGFDA(22748.05,TMGIENS,.01)=ORDERIEN
   . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
STINFODN
   QUIT STORERESULT
   ;"
GTREPORT(ROOT,TMGDFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE) ;"SENT RECORDS REPORT
        ;"RETURN HTML REPORT OF TIME SPENT
        ;"Purpose: Entry point, as called from CPRS REPORT system
        ;"Input: ROOT -- Pass by NAME.  This is where output goes
        ;"       TMGDFN -- Patient DFN ; ICN for foriegn sites
        ;"       ID --
        ;"       ALPHA -- Start date (lieu of DTRANGE)
        ;"       OMEGA -- End date (lieu of DTRANGE)
        ;"       DTRANGE -- # days back from today
        ;"       REMOTE --
        ;"       MAX    --
        ;"       ORFHIE --
        ;"Result: None.  Output goes into @ROOT
        NEW TMGRESULT,RESULTIDX SET RESULTIDX=1
        NEW SDT,EDT
        SET SDT=+$G(ALPHA)
        SET EDT=+$G(OMEGA) IF EDT'>0 SET EDT="9999999"
        NEW HD SET HD="<TABLE BORDER=3><CAPTION><B>PATIENT RECORDS SENT REPORT</B><BR>"
        SET HD=HD_" FAMILY PHYSICIANS OF GREENEVILLE<BR>1410 TUSCULUM BLVD  STE. 2600 <BR>"
        SET HD=HD_" GREENEVILLE, TN 37745</CAPTION><TR><TH>DATE</TH>"
        SET HD=HD_"<TH>SENT TO</TH><TH>FAX #</TH><TH>USER</TH><TH>REGARDING</TH></TR>"
        NEW IDX SET IDX=9999999
        NEW TIMEIDX SET TIMEIDX=1
        FOR  SET IDX=$O(^TMG(22748,"B",TMGDFN,IDX),-1) QUIT:IDX'>0  DO
        . NEW N9 SET N9=$G(^TMG(22748,IDX,9))
        . NEW THISDATE SET THISDATE=$P(N9,"^",1)
        . IF (THISDATE<SDT)!(THISDATE>EDT) QUIT
        . NEW USER,TO,FAX,RE
        . SET USER=$P($G(^TMG(22748,IDX,0)),"^",2)
        . SET TO=$P($G(^TMG(22748,IDX,0)),"^",3)
        . SET FAX=$P($G(^TMG(22748,IDX,1)),"^",1)
        . SET RE=$P($G(^TMG(22748,IDX,2)),"^",1)
        . SET TMGRESULT(RESULTIDX)=$$EXTDATE^TMGDATE(THISDATE)_"^"_TO_"^"_FAX_"^"_$P($G(^VA(200,USER,0)),"^",1)_"^"_RE
        . SET RESULTIDX=RESULTIDX+1
        DO SETHTML^TMGRPT2(.ROOT,.TMGRESULT,"CHART EXPORT REPORT",HD,5)
        QUIT
        ;"   