TMGC0PH1  ;TMG/kst-ELABHL7 transformation engine processing ;10/21/15, 2/17/16, 5/9/17(for Ignacio)
        ;;1.0;TMG-LIB;**1**;10/25/15;Build 61
  ;
  ;"TMG HL7 TRANSFORMATION FUNCTIONS
  ;
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;"Copyright (c) 10/25/2015  Kevin S. Toppenberg MD
  ;"
  ;"This file is part of the TMG LIBRARY, and may only be used in accordence
  ;" to license terms outlined in separate file TMGLICNS.m, which should 
  ;" always be distributed with this file.
  ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
  ;
  ;"NOTE: this is code for working with labs from **[QUEST DIAGNOSTICS]**
  ;"           (specifically, the QUEST interface for Ignacio Valdes)
  ;"      FYI -- Pathgroup code is in TMGHL73
  ;"             Laughlin  code is in TMGHL74
  ;"             Quest     code is in TMGHL75
  ;"=======================================================================
  ;"=======================================================================
  ;" API -- Public Functions.
  ;"=======================================================================
  ;"TEST  -- Pick file and manually send through filing process.   
  ;"BATCH -- Launch processing through all files in folder for laughlin lab
  ;"
  ;"=======================================================================
  ;" API - Private Functions
  ;"=======================================================================
  ;"XMSG    -- Process entire message before processing segments
  ;"XMSH15  -- Process MSH segment, FLD 15
  ;"XMSH16  -- Process MSH segment, FLD 16
  ;"PID     -- transform the PID segment, esp SSN
  ;"XORC1   -- Process empty ORC message, field 1
  ;"XORC12  -- Process empty ORC message, field 12
  ;"XORC13  -- Process empty ORC message, field 13
  ;"OBR     -- setup for OBR fields.
  ;"OBR4    -- To transform the OBR segment, field 4
  ;"OBR15   -- Transform Secimen source
  ;"OBR16   -- Transform Ordering provider.
  ;"OBX3    -- transform the OBX segment, field 3 -- Observation Identifier
  ;"OBX5    -- transform the OBX segment, field 5 -- Observation value
  ;"OBX15   -- transform the OBX segment, field 15 ---- Producer's ID
  ;"OBX16   -- transform the OBX segment, field 16 ---- Responsibile Observer
  ;"OBX18   -- transform the OBX segment, field 18 ---- Equipment Identifier (EI)
  ;"NTE3    -- transform the NTE segment, field 3
  ;"XFTEST(FLDVAL,TMGU) -- convert test code into value acceptable to VistA
  ;"SUORL   -- Setup TMGINFO("ORL"), TMGINFO("LOC"), TMGINFO("INSTNAME")
  ;" 
  ;"=======================================================================
  ;"Dependancies
  ;"=======================================================================
  ;"TMGSTUTL, all the HL*, LA* code that the HL processing path normally calls.
  ;"=======================================================================
  ;
TEST   ;"Pick file and manually send through filing process.
  DO TEST^TMGHL71("/mnt/WinServer/")
  QUIT
  ;
BATCH    ;"Launch processing through all files in folder.
  NEW DIR SET DIR="/mnt/WinServer"
  NEW DONEPATH SET DONEPATH="/mnt/WinServer"
  NEW OPTION SET OPTION("AUTO REGISTER MODE")=1  ;"Auto register new labs
  DO HLDIRIN^TMGHL71(DIR,1000,10,DONEPATH,".hl7",.OPTION)
  QUIT
  ;
  ;"----------------------------------------------------------------------
  ;"----------------------------------------------------------------------
PROCESS1(ARR,HL7MSG,HTMLMSG,PROCMODE) ;"ENTRY POINT.  CALLED BY ELABPULL^TMGC0P01
  ;"Purpose: process one lab message array, as returned from NewCrop
  ;"Input: ARR -- PASS BY REFERENCE.  Format ARR(<property str>)=value
  ;"       HL7MSG -- A string comprising entire HL7 message
  ;"       HTMLMSG -- A string comprising entire HTML message to display lab.
  ;"       PROCMODE -- OPTIONAL.
  ;"            PROCMODE("PDF")=1 (default) if PDF should be generated for result
  ;"            PROCMODE("HL7")=1 (default) if HL7 message should be processed
  ;"            PROCMODE("ERR")=1 (default) if errors should generate alerts
  ;"NOTE: If error generated in this procedure, an ALERT will generated.
  ;"Result:  1^OK, OR -1^ErrorMessage
  NEW TMGRESULT SET TMGRESULT="1^OK"
  SET PROCMODE("PDF")=$GET(PROCMODE("PDF"),1)
  SET PROCMODE("HL7")=$GET(PROCMODE("HL7"),1)
  SET PROCMODE("ERR")=$GET(PROCMODE("ERR"),1)
  NEW IEN2005 SET IEN2005=0 
  NEW HL7MSGARR,TMGHL7MSG
  NEW DIV SET DIV="^"
  NEW CR,LF,CRLF,LFCR SET CR=$CHAR(13),LF=$CHAR(10),CRLF=CR_LF,LFCR=LF_CR
  SET DIV=$SELECT(HL7MSG[CRLF:CRLF,HL7MSG[LFCR:LFCR,HL7MSG[LF:LF,1:CR)
  ;IF HL7MSG[CRLF SET DIV=CRLF
  ;ELSE  IF HL7MSG[LFCR SET DIV=LFCR
  ;ELSE  IF HL7MSG[LF SET DIV=LF
  ;ELSE  IF HL7MSG[CR SET DIV=CR   
  DO SPLIT2AR^TMGSTUT2(HL7MSG,DIV,.HL7MSGARR) ;split to array
  ;"Below is just to ease getting DFN and DT from HL7 message.  Actual processing later
  NEW TMGU SET TMGRESULT=$$PRSEARRY^TMGHL7X2(,.HL7MSGARR,.TMGHL7MSG,.TMGU) 
  IF +TMGRESULT'>0 GOTO PR1ERR
  NEW PIDIDX SET PIDIDX=+$ORDER(TMGHL7MSG("B","PID",0))
  NEW TMGDFN SET TMGDFN=$GET(ARR("PatientMRN"))
  NEW TEMPDFN SET TEMPDFN=$GET(TMGHL7MSG(PIDIDX,2)) IF TEMPDFN'="" SET TMGDFN=TEMPDFN
  IF TMGDFN'>0 DO
  . NEW TMGINFO DO PID SET TMGDFN=+$GET(TMGINFO("DFN"))
  . IF TMGDFN'>0 DO  QUIT
  . . SET TMGRESULT="-1^Unable to determine DFN in PROCESS1^TMGHL75"
  IF TMGRESULT<0 GOTO PR1ERR
  NEW MSHIDX SET MSHIDX=+$ORDER(TMGHL7MSG("B","MSH",0))
  NEW DT SET DT=$$HL72FMDT^TMGHL7U3($GET(TMGHL7MSG(MSHIDX,7)))
  NEW IENS22735D01 SET IENS22735D01=0
PR1HL7 ;
  IF PROCMODE("HL7")'=1 GOTO PR1PDF
  SET TMGRESULT=$$HNDLHL7(.HL7MSGARR,TMGDFN,DT)  ;"File HL7 message.  
  IF TMGRESULT="1^DUPLICATE" SET TMGRESULT="1^OK" GOTO PR1DN ;"SKIP PROCESSING
  IF TMGRESULT<0 GOTO PR1ERR
  SET IENS22735D01=TMGRESULT 
PR1PDF ;
  IF PROCMODE("PDF")'=1 GOTO PR1ERR
  SET IEN2005=$$HTML2PDF(.HTMLMSG,TMGDFN,DT) ;Returns IEN2005^OK, OR -1^ErrorMessage
  IF +IEN2005'>0 SET TMGRESULT=IEN2005 GOTO PR1ERR
  IF +IENS22735D01>0 DO
  . NEW TMGFDA,TMGMSG
  . SET TMGFDA(22735.01,IENS22735D01,.06)=+IEN2005
  . DO FILE^DIE("K","TMGFDA","TMGMSG")
  . IF $DATA(TMGMSG("DIERR")) DO  QUIT
  . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  IF TMGRESULT<0 GOTO PR1ERR
  GOTO PR1DN  
PR1ERR  ; 
  IF PROCMODE("ERR")'=1 GOTO PR1DN
  DO SETALRT2^TMGHL7E(TMGRESULT)  
PR1DN ;
  QUIT TMGRESULT
  ;
HNDLHL7(HL7MSG,TMGDFN,DT) ;
  ;"HANDLE HL7MESSAGE, and make TMG HL7 MESSAGE INFO record  
  ;"NOTE: If a corrected HL7 message comes in, it should be somehow different, and thus
  ;"      will have a different MD5SUM value.  Only if MD5SUM is the exactly the same will
  ;"      the message be considered to be duplicate.
  ;"RESULT: IENS TO 22735.01 (e.g. "2,3456,") if OK, -1^Message if error, or 1^DUPLICATE if already processed
  NEW TMGRESULT SET TMGRESULT="1^OK"
  NEW TMGHL7MSG,TMGENV,OPTION
  NEW MD5SUM SET MD5SUM=$$MD5ARR^TMGKERN1("HL7MSG")
  IF $$ISDUPHL7(TMGDFN,MD5SUM) DO  GOTO HNDH7DN
  . SET TMGRESULT="1^DUPLICATE"
  NEW IEN772,IEN773
  SET OPTION("AUTO REGISTER MODE")=1
  SET OPTION("HL7 PURGE DT")="T+12M@0800"  ;"PURGE AFTER 10 YEARS
  SET TMGRESULT=$$MKHLMAR2^TMGHL7U2(.HL7MSG,.IEN772,.IEN773,.OPTION) ;"MAKE HL7 MESSAGE
  IF TMGRESULT<0 GOTO HNDH7DN
  NEW HLMTIEN SET HLMTIEN=IEN772
  NEW HLMTIENS SET HLMTIENS=IEN773
  NEW NOALERT SET NOALERT=1 ;"alerts handled above
  SET TMGRESULT=$$HL7IN^TMGHL71(NOALERT,.OPTION)  ;"returns OPTION("HL7 DATE")
  NEW FILERESULT SET FILERESULT=TMGRESULT
  NEW DT SET DT=$$HL72FMDT^TMGHL7U3($GET(OPTION("HL7 DATE"))) 
  IF DT'>0 DO  GOTO HNDH7DN
  . SET TMGRESULT="-1^Unable to get HL7 date/time in HNDLHL7^TMGHL75"
  SET TMGRESULT=$$MKMSGREC(TMGDFN,DT,IEN772,IEN773,MD5SUM,(FILERESULT>0))
  IF FILERESULT'>0 DO  
  . IF TMGRESULT>0 SET TMGRESULT=FILERESULT QUIT
  . SET TMGRESULT="-1^"_$PIECE(FILERESULT,"^",2,99)_" AND "_$PIECE(TMGRESULT,"^",2,99)
HNDH7DN ;
  QUIT TMGRESULT
  ;
ISDUPHL7(TMGDFN,MD5SUM)  ;"IS HL7 MESSAGE DUPLICATE? TESTING MD5SUM
  NEW TMGRESULT SET TMGRESULT=($DATA(^TMG(22735,"AMD5SUM",MD5SUM,TMGDFN))>0)
  QUIT TMGRESULT
  ;
MKMSGREC(TMGDFN,DT,IEN772,IEN773,MD5SUM,SUCCESS)  ;"MAKE HL7 MESSAGE INFO REC
  ;"INPUT: TMGDFN -- PATIENT IEN
  ;"RESULT IENS TO 22735.01 (e.g. "2,3456,"), or -1^message if problem
  NEW TMGFDA,TMGMSG,TMGIEN
  NEW TMGRESULT SET TMGRESULT="1^OK"
  IF +$GET(TMGDFN)'>0 DO  GOTO MKMRCDN 
  . SET TMGRESULT="-1^Invalid DFN in MKMSGREC^TMGHL75"
  IF $DATA(^TMG(22735,TMGDFN)) GOTO MKMRC2
  SET TMGFDA(22735,"+1,",.01)=TMGDFN
  SET TMGIEN(1)=TMGDFN
  DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO MKMRCDN   
  . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
MKMRC2 ;
  KILL TMGFDA,TMGIEN,TMGMSG
  SET SUCCESS=$SELECT(SUCCESS=1:"Y",1:"")
  NEW TMGIENS SET TMGIENS="+1,"_TMGDFN_","  
  SET TMGFDA(22735.01,TMGIENS,.01)=DT    
  SET TMGFDA(22735.01,TMGIENS,.02)=IEN773
  SET TMGFDA(22735.01,TMGIENS,.03)=IEN772
  SET TMGFDA(22735.01,TMGIENS,.04)=SUCCESS
  SET TMGFDA(22735.01,TMGIENS,.05)=MD5SUM 
  DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO MKMRCDN   
  . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  NEW SUBIEN SET SUBIEN=+$GET(TMGIEN(1))
  IF SUBIEN'>0 DO  GOTO MKMRCDN
  . SET TMGRESULT="-1^Unable to locate IEN of added record in MKMSGREC^TMGHL75"
  SET TMGRESULT=SUBIEN_","_TMGDFN_","
MKMRCDN ;
  QUIT TMGRESULT
  ;
HTML2PDF(HTMLMSG,TMGDFN,DT)  ;Save HTMLMsg as file and entry in IMAGE file.  
  ;"Input: HTMLMSG -- string containing HTML page.  
  ;"       TMGDFN=PATIENT DFN
  ;"       DT=DT OF STUDY, FM format.  
  ;"Result:  IEN2005^OK, OR -1^ErrorMessage
  NEW TEMPDIR SET TEMPDIR="/tmp/"
  NEW TMGRESULT SET TMGRESULT=$$MAKIMAGE(TMGDFN,DT)  ;"FILE 2005 (IMAGE) holds file metadata
  NEW IEN2005 SET IEN2005=+TMGRESULT IF IEN2005'>0 GOTO H2PDN
  NEW DESTPATH SET DESTPATH=$PIECE(TMGRESULT,"^",2)
  SET DESTPATH=$$GETLOCFPATH^TMGRPC1C(DESTPATH)
  NEW DESTFNAME SET DESTFNAME=$PIECE(TMGRESULT,"^",3)
  IF DESTFNAME="" DO  GOTO H2PDN
  . SET TMGRESULT="-1^No filename returned from MAKIMAGE, in HTML2PDF^TMGC0P04()"
  SET TMGRESULT=IEN2005_"^OK"
  NEW DEST SET DEST=DESTPATH_DESTFNAME
  NEW SRCFNAME SET SRCFNAME=$$UNIQUE^%ZISUTL("LABHTML")_".html"
  NEW TEMP
  SET TEMP=$$ST2HFS(.HTMLMSG,TEMPDIR,SRCFNAME) IF +TEMP'>0 SET TMGRESULT=TEMP GOTO H2PDN
  SET TEMP=$$MAKPDF(TEMPDIR_SRCFNAME,DEST) IF +TEMP'>0 SET TMGRESULT=TEMP GOTO H2PDN    
  NEW KIL SET KIL(SRCFNAME)="" SET TEMP=$$DEL^%ZISH(TEMPDIR,"KIL")
  IF TEMP'=1 DO  GOTO H2PDN
  . SET TMGRESULT="-1^Unable to delete temp file: "_TEMPDIR_SRCFNAME
H2PDN ;
  QUIT TMGRESULT
  ;
MAKPDF(SRCFNAME,DESTFNAME)  ;Render HFS html file into pdf file.  
  ;"Input: SRCFNAME -- filename, including path, of saved html file.    
  ;"       DESTFNAME -- filename, including path, of the pdf file that is to be created
  ;"NOTE: This routine depends on linux command wkhtmltopdf which must be
  ;"      installed in linux
  ;"Result: 1^OK, or -1^Error message (Linux error return code)
  NEW TMGRESULT SET TMGRESULT=$$CMDOK^TMGKERNL("wkhtmltopdf")  
  IF +TMGRESULT'>0 GOTO MKPDN
  NEW CMD SET CMD="wkhtmltopdf -q "_SRCFNAME_" "_DESTFNAME
  NEW OUT,TEMP SET TEMP=$$LINUXCMD^TMGKERNL(CMD,.OUT)
  IF +TEMP'>0 SET TMGRESULT=TEMP GOTO MKPDN
  IF $$ISFILE^TMGKERNL(DESTFNAME)=0 DO  QUIT
  . SET TMGRESULT="-1^Expected output file, "_DESTFNAME_", not found."
MKPDN ;
  QUIT TMGRESULT
  ;
MAKIMAGE(TMGDFN,DT)  ;"MAKE ENTRY IN FILE 2005 (IMAGE) TO HOLD LAB PDF IMAGE
  ;"Input: TMGDFN=PATIENT DFN
  ;"       DT=DT OF STUDY, FM format.  
  ;"Result:  IEN2005^FILE NAME (with full path), or -1^ErrorMessage
  NEW TMGARR  ;"NOTE: subscript names (e.g. 'magDFN') have no significance; 1st piece of value is key 
  SET TMGARR("NETLOCABS")="ABS^STUFFONLY"
  SET TMGARR("magDFN")="5^"_TMGDFN
  SET TMGARR("DATETIME")="7^NOW"            ;date/time image stored
  SET TMGARR("DATETIMEPROC")="15^"_DT       ;Date/Time of Procedure
  SET TMGARR("PROC")="6^LAB IMAGE"          ;text name of procedure, 1-10 chars
  SET TMGARR("DESC")="10^PDF file containing lab results"  ;image description  
  SET TMGARR("DUZ")=DUZ                     ;DUZ
  SET TMGARR("OBJTYPE")="3^1"                ;Object Type. 1= Still Image
  SET TMGARR("FileExt")="EXT^pdf"
  NEW OUT DO ADD^MAGGTIA(.OUT,.TMGARR)  ;"OUT is single variable, IEN2005^path^FILE NAME (with full path)
  SET OUT=$GET(OUT)
  IF +OUT'>0 SET OUT="-1^Unable to add IMAGE record in file# 2005 in MAKIMAGE^TMGC0P04"
  QUIT OUT
  ;
ST2HFS(STR,PATH,FILENAME)  ;"WRITE STRING TO HOST FILE SYSTEM
  ;"Result:  1^ok, OR -1^ErrorMessage
  NEW TMGRESULT SET TMGRESULT="1^OK"
  SET PATH=$GET(PATH),FILENAME=$GET(FILENAME)
  DO OPEN^%ZISH("FILE1",PATH,FILENAME,"W")
  IF POP DO  GOTO STHDN
  . SET TMGRESULT="-1^Unable to open file.  Path=["_PATH_"], Filename=["_FILENAME_"]"
  USE IO 
  WRITE STR
  DO CLOSE^%ZISH("FILE1")
STHDN ;
  QUIT TMGRESULT
  ;
  ;"+-----------------------------------------------------------------+
  ;"| =============================================================== |
  ;"| |  Below are the call-back functions to handle transformation | |
  ;"| |  hooks, called by the XFMSG^TMGHL7X engine                  | |
  ;"| |  These are stored in FILE 22720, TMG HL7 MESSAGE TRANSFORM -| | 
  ;"| |     SETTINGS                                                | |
  ;"| =============================================================== |
  ;"+-----------------------------------------------------------------+
  ;
MSG     ;"Purpose: Process entire message BEFORE processing segments
  ;"Input: Uses globally scoped vars: TMGHL7MSG, TMGU, HLREC
  ;
  ;"STARTING GLOBAL SCOPE OF VARS: TMGNTEADD, TMGDD -- to be killed in MSG2()
  KILL TMGNTEADD,TMGDD,TMGLASTOBXSEGN
  DO GETDD^TMGHL7X3(.TMGDD)
  DO KILLDNRS(.TMGHL7MSG)
  DO ENSURSEG^TMGHL7X2(.TMGHL7MSG,"NTE",.TMGU)
  ;"The line below was for handling debug values of providers during Quest QA process.
  ;"DO SUPROV  ;"Purpose: Setup TMGINFO("PROV") -- Ordering provider.        
  DO XMSG^TMGHL72 
  QUIT
  ;
MSG2     ;"Purpose: Process entire message AFTER processing segments
  KILL TMGNTEADD,TMGDD,TMGLASTOBXSEGN
  IF $GET(TMGHL7MSG("STAGE"))="PRE" QUIT
  DO XMSG2^TMGHL72  
  NEW ARR DO ADDTOARR(.ARR,$$DBLN())  ;"Add terminal ascii line to notes
  DO ADD2NTE(.ARR,.TMGHL7MSG,.TMGU)  
  QUIT
  ;
KILLDNRS(TMGHL7MSG)  ;"Scan an remove all OBX's with lab value of "DNR", and also associated NTE segs.
  NEW ASEG SET ASEGN=""
  FOR  SET ASEGN=$ORDER(TMGHL7MSG(ASEGN)) QUIT:+ASEGN'>0  DO
  . IF $GET(TMGHL7MSG(ASEGN,"SEG"))'="OBX" QUIT
  . IF $GET(TMGHL7MSG(ASEGN,5))'="DNR" QUIT
  . DO DELSEG(.TMGHL7MSG,ASEGN) 
  . ;"NOW KILL ANY NTE SEGMENTS THAT DIRECTLY FOLLOW THIS OBX
  . NEW SEGN2,DONE SET SEGN2=ASEGN,DONE=0
  . FOR  SET SEGN2=$ORDER(TMGHL7MSG(SEGN2)) QUIT:DONE!(+SEGN2'>0)  DO  
  . . IF $GET(TMGHL7MSG(SEGN2,"SEG"))'="NTE" SET DONE=1 QUIT
  . . IF $$ISFINALN(.TMGHL7MSG,SEGN2)=1 SET DONE=1 QUIT    
  . . DO DELSEG(.TMGHL7MSG,SEGN2)  
  QUIT
  ;
ISFINALN(TMGHL7MSG,SEGN)  ;"IS NTE BLOCK AT THE END OF THE MESSAGE?  I.E. NO FOLLOWING SEGMENTS?
  NEW TMGRESULT SET TMGRESULT=0
  NEW SEGN2,OTHER SET SEGN2=ASEGN,OTHER=0
  FOR  SET SEGN2=$ORDER(TMGHL7MSG(SEGN2)) QUIT:OTHER!(+SEGN2'>0)  DO
  . IF $GET(TMGHL7MSG(SEGN2,"SEG"))'="NTE" SET OTHER=1 QUIT
  QUIT (OTHER=0)
  ;
DELSEG(TMGHL7MSG,SEGN)  ;"DELETE SEGMENT, AND REFERENCES TO IT IN THE CROSS REFERENCES
  KILL TMGHL7MSG(SEGN),TMGHL7MSG("B","OBX",SEGN)   ;"KILL SEGMENT AND B INDEX ENTRY
  NEW APO SET APO=""
  FOR  SET APO=$ORDER(TMGHL7MSG("PO",APO)) QUIT:+APO'>0  DO   ;"KILL PROCESS ORDER ENTRIES
  . IF $GET(TMGHL7MSG("PO",APO))=SEGN KILL TMGHL7MSG("PO",APO)
  QUIT
  ;
DBLN()  ;"Return ascii text line
  QUIT "===================================================="
  ;
SHOWPART(ARR,INFO,NODE,PRINTNAME)   ;
  IF $$CHILDCT($NAME(INFO(NODE)))=1 DO
  . SET VALUE=$ORDER(INFO(NODE,""))
  . SET LINE=PRINTNAME_VALUE
  . DO ADDTOARR(.ARR,LINE)
  ELSE  DO
  . NEW VALUE SET VALUE=""
  . FOR  SET VALUE=$ORDER(INFO(NODE,VALUE)) QUIT:VALUE=""  DO
  . . NEW SHOWN SET SHOWN=0
  . . NEW TESTNAME SET TESTNAME=""
  . . FOR  SET TESTNAME=$ORDER(INFO(NODE,VALUE,TESTNAME)) QUIT:TESTNAME=""  DO
  . . . SET LINE=PRINTNAME_VALUE_" for test: "_TESTNAME
  . . . DO ADDTOARR(.ARR,LINE)
  . . . SET SHOWN=1
  . . IF SHOWN=0 DO
  . . . SET LINE=PRINTNAME_VALUE
  . . . DO ADDTOARR(.ARR,LINE)
  QUIT
  ;        
ADDTOARR(ARR,LINE)   ;
  NEW IDX SET IDX=$ORDER(ARR(""),-1)+1
  SET ARR(IDX)=LINE 
  QUIT
  ;
CHILDCT(REF)   ;"Return number of child nodes for ref
  NEW CT SET CT=0
  NEW IDX SET IDX=""
  FOR  SET IDX=$ORDER(@REF@(IDX)) QUIT:IDX=""  DO
  . SET CT=CT+1
  QUIT CT
  ;
MSH3     ;"Purpose: Process MSH segment, FLD 4 (Sending Application)
  ;"SET TMGVALUE="LA7V HOST QUEST"
  QUIT
  ;
MSH4   ;"Purpose: Process MSH segment, FLD 4 (Sending Facility)
  IF $GET(TMGHL7MSG("STAGE"))="PRE" QUIT
  NEW CODE SET CODE=$PIECE(TMGVALUE,TMGU(2),1)
  NEW IEN22720 SET IEN22720=+$ORDER(^TMG(22720,"D",CODE,0))
  IF IEN22720'>0 DO  QUIT
  . SET TMGXERR="IN MSH4^TMGHL75: Unable to find entry to match '"_CODE_"' in 'D' cross reference."
  NEW IEN4 SET IEN4=$PIECE($GET(^TMG(22720,IEN22720,0)),"^",2)
  IF IEN4'>0 DO  QUIT
  . SET TMGXERR="IN MSH4^TMGHL75: No institution (field #1)defined for record #"_IEN22720_" in file 22720"
  NEW ZN SET ZN=$GET(^DIC(4,IEN4,0))
  NEW INAME SET INAME=$PIECE(ZN,"^",1)
  SET TMGVALUE=INAME_TMGU(2)_IEN4
  ;"DO XMSH4^TMGHL72
  QUIT
  ;
MSH15   ;"Purpose: Process MSH segment, FLD 15 -- Accept Acknowledgment Type (ID)
  DO XMSH15^TMGHL72
  QUIT
  ;
MSH16   ;"Purpose: Process MSH segment, FLD 16 -- Application Acknowledgment Type (ID)
  DO XMSH16^TMGHL72 
  QUIT
  ;
PID      ;"Purpose: To transform the PID segment, ESP SSN
  IF $GET(TMGHL7MSG("STAGE"))="PRE" QUIT
  DO PID^TMGHL72
  QUIT
  ;
PID5     ;"Purpose: to transform patient name, if needed
  QUIT
  ;
PV18     ;"Purpose: Process entire PV1-8 segment
  ;"NOTE: Pathgroup doesn't send order information in OBR16, so will use
  ;"      information from here to fix that.
  ;"In Pathrgoup messages, PV1 comes before any OBR segments.
  NEW LNAME SET LNAME=$PIECE(TMGVALUE,TMGU(2),2)
  NEW FNAME SET FNAME=$PIECE(TMGVALUE,TMGU(2),3)
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(TMGHL7MSG(IDX)) QUIT:(+IDX'>0)  DO
  . NEW SEGTYPE SET SEGTYPE=$GET(TMGHL7MSG(IDX,"SEG")) QUIT:SEGTYPE=""
  . IF SEGTYPE="OBR" DO
  . . DO SETPCE^TMGHL7X2(TMGVALUE,.TMGHL7MSG,.TMGU,IDX,16)
  DO SETPCE^TMGHL7X2(TMGVALUE,.TMGHL7MSG,.TMGU,"ORC",12)
  QUIT
  ;
ORC     ;"Purpose: Process entire ORC message
  ;"Uses Globally scoped vars: TMGSEGN (set up in from TMGHL7X* code before calling here.)
  ;"     also TMGDD
  IF $DATA(TMGHL7MSG(TMGSEGN,13))=0 DO
  . SET TMGHL7MSG(TMGSEGN,13)="?"
  IF $GET(TMGINFO("PROV"))="" DO SUPROV^TMGHL72("ORC",12)
  ;
  QUIT
  ;
ORC1    ;"Purpose: Process ORC message, field 1
  DO XORC1^TMGHL72
  QUIT
  ;
ORC12   ;"Purpose: Process ORC message, field 12
  DO XORC12^TMGHL72
  QUIT
  ;
ORC13   ;"Purpose: Process ORC message, field 13
  DO XORC13^TMGHL72
  SET $PIECE(TMGVALUE,"^",1)="ASTRONAUT HARRRIS COUNTY NET"
  SET $PIECE(TMGVALUE,"^",2)="1" 
  QUIT
  ;
OBR      ;"Purpose: setup for OBR fields
  ;"Uses globally scoped vars: TMGHL7MSG,TMGSEGN,TMGU,TMGINFO
  ;"Creates globally scoped var TMGHL75OBRCOLDT, will be killed in OBRDN
  NEW ASEG MERGE ASEG=TMGHL7MSG(TMGSEGN)
  ;"If collection date/time not specified, use lab reception date/time
  IF $GET(ASEG(7))="",$GET(ASEG(14))'="" DO   ;"#7 = collection date-time, #14=received date-time 
  . NEW DT SET DT=ASEG(14)
  . ;"NO, see note below --> SET DT=$EXTRACT(DT,1,8)  ;"strip time and use only collection DATE
  . ;"NOTICE: If I don't keep time, then I might store duplicate messages in cases
  . ;"   where the lab sends a correction or final result, if the DT in not
  . ;"   sufficiently precise.  So I will use the full collection DT 
  . NEW FMDT SET FMDT=$$HL72FMDT^TMGHL7U3(DT)
  . NEW DATESUSED DO PRIORLDT^TMGLRW01(.TMGHL7MSG,.DATESUSED,.TMGINFO)
  . SET FMDT=$$UNIQUEDT^TMGLRW01(FMDT,.DATESUSED)
  . SET DT=$$FMDT2HL7^TMGHL7U3(FMDT)
  . DO SETPCE^TMGHL7X2(DT,.TMGHL7MSG,.TMGU,TMGSEGN,7)
  . SET TMGHL75OBRCOLDT=1  ;"this is a flag to make a note of the change in OBRDN
  DO OBR^TMGHL72
  QUIT
  ;
OBR4     ;"Purpose: To transform the OBR segment, field 4
  IF $GET(TMGHL7MSG("STAGE"))="PRE" QUIT
  DO OBR4^TMGHL72
  QUIT
  ;
OBR15    ;"Transform Secimen source
  ;"FYI -- Quest doesn't send a specimen source.  
  IF $GET(TMGHL7MSG("STAGE"))="PRE" QUIT
  DO OBR15^TMGHL73
  QUIT
  ;
OBR16    ;"Transform Ordering provider.
  DO OBR16^TMGHL72
  QUIT
  ;
OBRDN    ;"Purpose: setup for OBR fields, called *after* fields, subfields etc are processed
  ;"Uses globally scoped vars: TMGSEGN, TMGDD
  IF $GET(TMGHL7MSG("STAGE"))="PRE" QUIT
  NEW TEMP
  DO LABLDATA(.TEMP,.TMGHL7MSG,"OBR",TMGSEGN) ;
  ;
  NEW ORDINFO MERGE ORDINFO=TMGHL7MSG("ORDER",TMGSEGN)
  NEW TESTNAME SET TESTNAME=$PIECE($GET(TMGHL7MSG("ORDER",TMGSEGN,"IEN60")),"^",2)
  ;
  NEW INFO,PROV,PID  
  ;"NEW TESTNAME SET TESTNAME=$GET(TEMP("Universal Service ID"))
  ;"SET TESTNAME=$PIECE(TESTNAME,TMGU(2),2)
  NEW ONEACSN SET ONEACSN=$GET(TEMP("Filler Order Number"))
  NEW LABADDR SET LABADDR=$GET(TEMP("Filler Field 2"))
  ;"SET LABADDR=$PIECE(LABADDR,TMGU(2),2,999)
  ;"SET LABADDR=$$REPLSTR^TMGSTUT3(LABADDR,"^","; ")
  SET PROV=$GET(TEMP("Ordering Provider"))
  SET PROV=$PIECE(PROV,TMGU(2),2,3)
  SET PROV=$$TRIM^XLFSTR($TRANSLATE(PROV,"^"," "))
  IF PROV="" DO
  . NEW TEMP2 DO LABLDATA(.TEMP2,.TMGHL7MSG,"ORC") 
  . SET PROV=$GET(TEMP2("Ordering Provider"))
  . SET PROV=$PIECE(PROV,TMGU(2),2,3)
  . SET PROV=$TRANSLATE(PROV,"^"," ")        
  NEW OBSDT SET OBSDT=$GET(TEMP("Observation Date/Time"))
  SET OBSDT=$$HL72FMDT^TMGHL7U3(OBSDT)
  SET OBSDT=$$FMTE^XLFDT(OBSDT)
  NEW RECDT SET RECDT=$GET(TEMP("Specimen Received Date/Time"))
  SET RECDT=$$HL72FMDT^TMGHL7U3(RECDT)
  SET RECDT=$$FMTE^XLFDT(RECDT)        
  NEW RPTDT SET RPTDT=$GET(TEMP("Results Rpt/Status Chng - Date/Time"))
  SET RPTDT=$$HL72FMDT^TMGHL7U3(RPTDT)
  SET RPTDT=$$FMTE^XLFDT(RPTDT)
  NEW STATUS SET STATUS=$GET(TEMP("Result Status"))
  IF STATUS="F" SET STATUS="FINAL"
  IF STATUS="I" SET STATUS="INCOMPLETE/PRELIMINARY"
  IF STATUS="C" SET STATUS="CORRECTED"
  IF STATUS="P" SET STATUS="PRELIMINARY"
  IF STATUS="X" SET STATUS="TEST CANCELED"
  ;       
  NEW TEMP2 DO LABLDATA(.TEMP2,.TMGHL7MSG,"PID") ;
  NEW PID SET PID=$GET(TEMP2("Patient ID"))
  NEW GENDER SET GENDER=$GET(TEMP2("Sex"))
  IF GENDER="F" SET GENDER="FEMALE"
  IF GENDER="M" SET GENDER="MALE"
  NEW PTDOB SET PTDOB=$GET(TEMP2("Date/Time Of Birth"))
  SET PTDOB=$$HL72FMDT^TMGHL7U3(PTDOB)
  SET PTDOB=$$FMTE^XLFDT(PTDOB,"2D")
  NEW PTNAME SET PTNAME=$TRANSLATE($GET(TEMP2("Patient Name")),TMGU(2),",")
  NEW ACCTN SET ACCTN=$GET(TEMP2("Patient Account Number"))
  NEW PATIENT SET PATIENT=PTNAME_" ("_PTDOB_"), "_GENDER_", Acct #"_ACCTN
  ;
  NEW LINE,ARR,FLD,VALUE SET FLD=""   
  NEW INDENT SET INDENT="  "
  DO ADDTOARR(.ARR,$$DBLN())
  DO ADDTOARR(.ARR,"Test ordered: "_TESTNAME)
  DO ADDTOARR(.ARR,"Ordering Provider: "_PROV)
  DO ADDTOARR(.ARR,"Lab Accession Number: "_ONEACSN)
  DO ADDTOARR(.ARR,"Patient: "_PATIENT)
  DO ADDTOARR(.ARR,"Lab Patient ID: "_PID)
  SET LINE="Specimen Collection Date: "_OBSDT
  IF $GET(TMGHL75OBRCOLDT)=1 SET LINE=LINE_" <-- see *NOTE*"
  DO ADDTOARR(.ARR,LINE)
  IF $GET(TMGHL75OBRCOLDT)=1 DO
  . DO ADDTOARR(.ARR,"  *NOTE*: Collection date/time not provided.")    
  . DO ADDTOARR(.ARR,"          Using date/time lab RECEIVED instead.")    
  KILL TMGHL75OBRCOLDT
  DO ADDTOARR(.ARR,"Specimen Received Date: "_RECDT)
  DO ADDTOARR(.ARR,"Result Report Date: "_RPTDT)
  DO ADDTOARR(.ARR,"Result Status: "_STATUS)
  IF LABADDR'="" DO
  . DO ADDTOARR(.ARR,"Test Performed by: ")
  . DO ADDTOARR(.ARR,INDENT_$PIECE(LABADDR,TMGU(2),2))  ;"Addr1
  . DO ADDTOARR(.ARR,INDENT_$PIECE(LABADDR,TMGU(2),3))  ;"Addr2
  . SET LINE=$PIECE(LABADDR,TMGU(2),4)_", "             ;"City
  . SET LINE=LINE_$PIECE(LABADDR,TMGU(2),5)_" "         ;"State
  . SET LINE=LINE_$PIECE(LABADDR,TMGU(2),6)             ;"ZIP
  . DO ADDTOARR(.ARR,INDENT_LINE)  
  . IF $PIECE(LABADDR,TMGU(2),8)'="" DO  ;"2nd address found...
  . . DO ADDTOARR(.ARR,INDENT_$PIECE(LABADDR,TMGU(2),7))  ;"Addr1
  . . DO ADDTOARR(.ARR,INDENT_$PIECE(LABADDR,TMGU(2),8))  ;"Addr2
  . . SET LINE=$PIECE(LABADDR,TMGU(2),9)                  ;"City
  . . IF LINE'="" SET LINE=LINE_", "   
  . . SET LINE=LINE_$PIECE(LABADDR,TMGU(2),10)_" "        ;"State
  . . SET LINE=LINE_$PIECE(LABADDR,TMGU(2),11)            ;"ZIP
  . . DO ADDTOARR(.ARR,INDENT_LINE)
  . . DO ADDTOARR(.ARR,INDENT_$PIECE(LABADDR,TMGU(2),12))  ;"responsible physician
  . ELSE  DO  
  . . DO ADDTOARR(.ARR,INDENT_$PIECE(LABADDR,TMGU(2),7))  ;"responsible physician
  ELSE  DO
  . DO ADDTOARR(.ARR,"Test Performed by:  (not provided)")
  DO ADDTOARR(.ARR,$$DBLN())
  ;           
  DO INSRTNTE(.ARR,.TMGHL7MSG,.TMGU,TMGSEGN)  
  QUIT
  ;
  ;        
OBX  ;"Purpose: to transform the entire OBX segment before any fields are processed
  ;"Uses TMGSEGN, that is set up in from TMGHL7X* code before calling here. 
  SET TMGLASTOBXSEGN=TMGSEGN  ;"Will be killed in MSG2^TMHL75
  NEW ASEG MERGE ASEG=TMGHL7MSG(TMGSEGN)
        ;"Format of ASEG(3)=<TestCode>^<Test Text>^<Coding System>^<Alt Id>^<Alt Text>^<Alt coding system>
  NEW QALTID SET QALTID=$$TRIM^XLFSTR($GET(ASEG(3,4)))
  NEW QALTNAME SET QALTNAME=$$TRIM^XLFSTR($GET(ASEG(3,5)))
  IF QALTNAME="",QALTID'="" DO
  . SET QALTNAME="TEST "_QALTID
  . DO SETPCE^TMGHL7X2(QALTNAME,.TMGHL7MSG,.TMGU,TMGSEGN,3,5)
  QUIT
  ;
OBX3     ;"Purpose: To transform the OBX segment, field 3 -- Observation Identifier
  IF $GET(TMGHL7MSG("STAGE"))="PRE" QUIT
  DO OBX3^TMGHL72
  QUIT
  ;
OBX5     ;"Purpose: To transform the OBX segment, field 5 -- Observation value
  IF $GET(TMGHL7MSG("STAGE"))="PRE" QUIT
  IF TMGVALUE="TNP" SET TMGVALUE="(test not performed)"
  IF TMGVALUE="" DO
  . NEW NEXTSEGN SET NEXTSEGN=+$ORDER(TMGHL7MSG(TMGSEGN))
  . NEW NEXTSEG SET NEXTSEG=""
  . IF NEXTSEGN>0 SET NEXTSEG=$GET(TMGHL7MSG(NEXTSEGN,"SEG"))
  . IF NEXTSEG'="NTE" QUIT
  . SET TMGVALUE="(see in comments below)"
  . IF $$NUMNTEFO(TMGSEGN)'=1 QUIT
  . NEW ASEG MERGE ASEG=TMGHL7MSG(NEXTSEGN)
  . NEW ANTE SET ANTE=$GET(ASEG(3))
  . IF $LENGTH(ANTE)>60 QUIT
  . SET TMGVALUE=ANTE
  . KILL TMGHL7MSG(NEXTSEGN)
  IF TMGVALUE="" SET TMGVALUE=" "
  DO OBX5^TMGHL72
  QUIT
  ;
NUMNTEFO(SEGN)  ;"NUMBER OF NTE SEGMENTS THAT FOLLOW AFTER SEGN
  ;"Input: Uses globally scoped vars: TMGHL7MSG
  NEW TMGRESULT SET TMGRESULT=0
  NEW NEXTSEGN SET NEXTSEGN=SEGN
  NEW DONE SET DONE=0
  FOR  SET NEXTSEGN=+$ORDER(TMGHL7MSG(NEXTSEGN)) QUIT:DONE  DO
  . IF +NEXTSEGN'>0 SET DONE=1 QUIT
  . NEW NEXTSEG SET NEXTSEG=$GET(TMGHL7MSG(NEXTSEGN,"SEG"))
  . IF NEXTSEG'="NTE" SET DONE=1 QUIT
  . SET TMGRESULT=TMGRESULT+1
  QUIT TMGRESULT
  ;
OBX15    ;"Purpose: To transform the OBX segment, field 15 ---- Producer's ID
  DO OBX15^TMGHL72
  QUIT
  ;
OBX16    ;"Purpose: To transform the OBX segment, field 16 ---- Responsibile Observer
  DO OBX16^TMGHL72
  QUIT
  ;
OBX18    ;"Purpose: To transform the OBX segment, field 18 ---- Equipment Identifier (EI)
  DO OBX18^TMGHL72
  QUIT
  ;
NTE  ;"Transfor the entire NTE segment
  ;"Input: Uses globally scoped vars: TMGHL7MSG, TMGU, TMGSEGN
  NEW ASEG SET ASEG=$GET(TMGHL7MSG(TMGSEGN))
  IF $LENGTH(ASEG,TMGU(1))<4 DO
  . DO SETPCE^TMGHL7X2(" ",.TMGHL7MSG,.TMGU,TMGSEGN,3)
  QUIT
  ;
NTE3     ;"Purpose: To transform the NTE segment, field 3 (the comments)
  ;"Uses TMGSEGN, that is set up in from TMGHL7X* code before calling here. 
  IF $GET(TMGHL7MSG("STAGE"))="PRE" QUIT
  NEW NTELN SET NTELN=+$GET(TMGHL7MSG(TMGSEGN,1))
  NEW DIVLNLEN SET DIVLNLEN=50
  SET TMGLASTOBXSEGN=+$GET(TMGLASTOBXSEGN)
  IF TMGLASTOBXSEGN>0,(NTELN=1) DO
  . NEW TESTNAME SET TESTNAME=$PIECE($GET(TMGHL7MSG(TMGLASTOBXSEGN,"RESULT","IEN60")),"^",2)
  . NEW LINE SET LINE="~~~ Comment for: "_TESTNAME_" "
  . FOR  QUIT:$LENGTH(LINE)>DIVLNLEN  SET LINE=LINE_"~"
  . DO PREFIXNT(LINE,.TMGHL7MSG,.TMGU,TMGSEGN)
  DO NTE3^TMGHL72
  NEW NEXTSEGN SET NEXTSEGN=+$ORDER(TMGHL7MSG(TMGSEGN))
  NEW NEXTSEG SET NEXTSEG=""
  IF NEXTSEGN>0 SET NEXTSEG=$GET(TMGHL7MSG(NEXTSEGN,"SEG"))
  IF (NEXTSEG'=""),(NEXTSEG'="NTE"),(TMGLASTOBXSEGN>0) DO  
  . NEW ARR,LINE SET LINE=""
  . FOR  QUIT:$LENGTH(LINE)>DIVLNLEN  SET LINE=LINE_"~"
  . DO ADDTOARR(.ARR,LINE)
  . DO APPNDNTE(.ARR,.TMGHL7MSG,.TMGU,TMGSEGN)
  QUIT
  ;
SUORL    ;"Purpose: Setup TMGINFO("ORL") and TMGINFO("LOC") and TMGINFO("INSTNAME")
  DO SUORL^TMGHL72
  QUIT
  ;
LABLDATA(OUT,TMGHL7MSG,SEG,SEGN)  ;
  ;"Uses globally scoped vars: TMGDD
  NEW ASEG 
  SET SEGN=+$GET(SEGN)
  IF SEGN>0 MERGE ASEG=TMGHL7MSG(SEGN)
  ELSE  DO GETSEG^TMGHL7X2(.ASEG,.TMGHL7MSG,SEG)
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(ASEG(IDX)) QUIT:+IDX'>0  DO
  . NEW VALUE SET VALUE=$GET(ASEG(IDX)) QUIT:VALUE=""
  . NEW FLDNAME SET FLDNAME=$GET(TMGDD(SEG,IDX),"?")
  . SET OUT(FLDNAME)=VALUE
  QUIT
  ;
SUPROV   ;"Purpose: Setup TMGINFO("PROV") -- Ordering provider.
  ;"Input: Uses globally scoped vars: TMGHL7MSG, TMGU, HLREC
  ;"Output: Sets globally scoped variable TMGINFO("PROV")
  ;"Results: None.  TMGXERR SET IF error
  ;"NOTE: in XORC12^TMGHL72, call to SUPROV^TMGHL72 is avoided because
  ;"   at that point we will have already set up TMGINFO("PROV"), done below.  
  ;
  NEW ASEG DO GETSEG^TMGHL7X2(.ASEG,.TMGHL7MSG,"ORC")
  ;"below is for getting debug messages from Quest....
  ;"------------------------------------------------------
  ;"NEW SWAPPROV SET SWAPPROV=0
  NEW SSN,FNAME,LNAME
  SET SSN=$GET(ASEG(12,1)),LNAME=$GET(ASEG(12,2)),FNAME=$GET(ASEG(12,3))
  ;"IF (SSN="1234567893")&(LNAME="COLMENAR")&(FNAME="ANTONIO") SET SWAPPROV=1
  ;"IF (SSN="1417963257")&(LNAME="PALACIOS")&(FNAME="CARLOS") SET SWAPPROV=1
  ;"IF SWAPPROV DO
  ;". DO SETPCE^TMGHL7X2("1093726085",.TMGHL7MSG,.TMGU,"ORC",12,1)
  ;". DO SETPCE^TMGHL7X2("KEVIN",.TMGHL7MSG,.TMGU,"ORC",12,2)
  ;". DO SETPCE^TMGHL7X2("TOPPENBERG",.TMGHL7MSG,.TMGU,"ORC",12,3)        
  NEW PROV SET PROV=$$GETPCE^TMGHL7X2(.TMGHL7MSG,"ORC",12)
  NEW DEBUGMSG SET DEBUGMSG=($GET(TMGHL7MSG(1,11))="D")
  IF $PIECE(PROV,TMGU(2),$LENGTH(PROV,"^"))="NPI" DO
  . NEW NPI,ADUZ
  . SET NPI=$PIECE(PROV,TMGU(2),1)
  . IF DEBUGMSG,(NPI="1122334455")&($PIECE(PROV,TMGU(2),2)="ALLAN")&($PIECE(PROV,TMGU(2),3)="JOSEPH") DO
  . . SET NPI="1093726085"
  . IF NPI="" DO  QUIT
  . . SET TMGXERR="In SUPROV^TMGHL75: Provider ID set to 'NPI', but NPI is blank in field #12 of 'ORC' segment in HL7 message"
  . SET ADUZ=+$ORDER(^VA(200,"ANPI",NPI,0))
  . IF ADUZ'>0 DO  QUIT
  . . SET TMGXERR="In SUPROV^TMGHL75: NPI "_NPI_" could not be found in ^VA(200,'ANPI' index"
  . NEW NAME SET NAME=$PIECE($GET(^VA(200,ADUZ,0)),"^",1)
  . IF NAME="" DO  QUIT
  . . SET TMGXERR="In SUPROV^TMGHL75: User "_ADUZ_" did not return a value name."
  . NEW LNAME,FNAME
  . SET LNAME=$PIECE(NAME,",",1)
  . SET FNAME=$PIECE(NAME,",",2)
  . SET FNAME=$PIECE(FNAME," ",1)
  . SET PROV=ADUZ_TMGU(2)_LNAME_TMGU(2)_FNAME
  . SET TMGINFO("PROV")=PROV
  ELSE  DO
  . ;"IF $$UP^XLFSTR(PROV)'["TOPPENBERG" SET PROV="^Doctor^Unspecified^"
  . IF PROV="" DO  QUIT
  . . SET TMGXERR="In SUPROV^TMGHL752: Ordering provider not provided in field #12 or 'ORC' segment in HL7 message"
  . NEW LNAME,FNAME,MNAME
  . SET LNAME=$PIECE(PROV,TMGU(2),2)
  . SET FNAME=$PIECE(PROV,TMGU(2),3)
  . SET MNAME=$PIECE(PROV,TMGU(2),4)
  . NEW NAME SET NAME=LNAME_","_FNAME_" "_MNAME
  . SET NAME=$$TRIM^XLFSTR(NAME)
  . NEW DIC,X,Y
  . SET DIC=200,DIC(0)="M"
  . SET X=NAME
  . DO ^DIC
  . IF Y'>0 DO  QUIT
  . . SET TMGXERR="In SUPROV^TMGHL75: Unable find provider in lookup: '"_NAME_"'"
  . SET PROV=+Y_TMGU(2)_LNAME_TMGU(2)_FNAME
  . SET TMGINFO("PROV")=PROV
SPVDN    QUIT
  ;
INSRTNTE(ARR,TMGHL7MSG,TMGU,SEGNPRIOR)   ;"Insert note segment after SEGNPRIOR from ARR
  ;"Input: ARR -- PASS BY REFERENCE.  The array to add.  Format:
  ;"   ARR(#)=<line of text>
  ;"      TMGHL7MSG -- the array to store in. PASS BY REFERENCE.
  ;"      TMGU -- The array with divisor chars.
  ;"      SEGNPRIOR -- The segment number that the NTE array is to be inserted AFTER
  ;"NOTE: If there is a NTE segment directly after SEGNPRIOR, then that NTE
  ;"      block will be appended to.
  ;"Results: none
  NEW NEXTSEGN SET NEXTSEGN=+$ORDER(TMGHL7MSG(SEGNPRIOR))
  NEW SEGNAME SET SEGNAME=$GET(TMGHL7MSG(NEXTSEGN,"SEG"))
  IF SEGNAME="NTE" DO  GOTO INSNDN
  . NEW LASTNTESEGN SET LASTNTESEGN=NEXTSEGN
  . FOR  SET NEXTSEGN=$ORDER(TMGHL7MSG(NEXTSEGN)) QUIT:(+NEXTSEGN'>0)!($GET(TMGHL7MSG(NEXTSEGN,"SEG"))'="NTE")  DO
  . . SET LASTNTESEGN=NEXTSEGN
  . DO APPNDNTE(.ARR,.TMGHL7MSG,.TMGU,LASTNTESEGN)  ;"APPEND NOTE
  NEW NTENUM SET NTENUM=1
  NEW FIRST SET FIRST=1
  NEW NEWSEGN  
  NEW ARRIDX SET ARRIDX=""
  FOR  SET ARRIDX=$ORDER(ARR(ARRIDX)) QUIT:+ARRIDX'>0  DO
  . NEW LINE SET LINE=$GET(ARR(ARRIDX))
  . IF FIRST SET NEWSEGN=SEGNPRIOR+0.5 SET FIRST=0
  . ELSE  SET NEWSEGN=SEGNPRIOR+0.01
  . DO SETPCE^TMGHL7X2(NTENUM,.TMGHL7MSG,.TMGU,NEWSEGN,1) 
  . SET NTENUM=NTENUM+1 
  . SET $PIECE(TMGHL7MSG(NEWSEGN),TMGU(1),1)="NTE"
  . DO SETPCE^TMGHL7X2(LINE,.TMGHL7MSG,.TMGU,NEWSEGN,3) ;
  . SET $PIECE(TMGHL7MSG(NEWSEGN),TMGU(1),1)="NTE"
  . SET TMGHL7MSG(NEWSEGN,"SEG")="NTE"       
  . SET TMGHL7MSG("B","NTE",NEWSEGN)=""
  . SET SEGNPRIOR=NEWSEGN
INSNDN   QUIT
  ;
PREFIXNT(LINE,TMGHL7MSG,TMGU,SEGN)  ;"PREFIX NOTE (INSERT LINE BEFORE INDEX LINE)
  ;"Input: LINE -- A SINGLE LINE TO PREFIX
  ;"      TMGHL7MSG -- the array to store in. PASS BY REFERENCE.
  ;"      TMGU -- The array with divisor chars.
  ;"      SEGN -- The segment number of the line in the NTE array to insert before
  ;"  that ARR is to be appended to.
  ;"NOTE: This assumes that inputs are valid and that NTE segment exists
  ;"Results: none
  NEW NTEIDX SET NTEIDX=SEGN
  NEW NTENUM SET NTENUM=$GET(TMGHL7MSG(NTEIDX,1))
  NEW NEWSEGN SET NEWSEGN=NTEIDX
  FOR  SET NEWSEGN=NEWSEGN-0.01,NTENUM=NTENUM-0.01 QUIT:$DATA(TMGHL7MSG(NEWSEGN))=0
  DO SETPCE^TMGHL7X2(NTENUM,.TMGHL7MSG,.TMGU,NEWSEGN,1) 
  SET $PIECE(TMGHL7MSG(NEWSEGN),TMGU(1),1)="NTE"
  DO SETPCE^TMGHL7X2(LINE,.TMGHL7MSG,.TMGU,NEWSEGN,3) ;
  SET $PIECE(TMGHL7MSG(NEWSEGN),TMGU(1),1)="NTE"
  SET TMGHL7MSG(NEWSEGN,"SEG")="NTE"       
  SET TMGHL7MSG("B","NTE",NEWSEGN)=""
  QUIT         
  ;
APPNDNTE(ARR,TMGHL7MSG,TMGU,LASTNTESEGN)   ;"APPEND NOTE 
  ;"Input: ARR -- PASS BY REFERENCE.  The array to add.  Format:
  ;"   ARR(#)=<line of text>
  ;"      TMGHL7MSG -- the array to store in. PASS BY REFERENCE.
  ;"      TMGU -- The array with divisor chars.
  ;"      LASTNTESEGN -- The segment number of the **last** line in the NTE array
  ;"  that ARR is to be appended to.
  ;"NOTE: This assumes that inputs are valid and that NTE segment exists
  ;"Results: none
  NEW FIRST SET FIRST=1
  NEW NTEIDX SET NTEIDX=LASTNTESEGN
  NEW ARRIDX SET ARRIDX=""
  FOR  SET ARRIDX=$ORDER(ARR(ARRIDX)) QUIT:+ARRIDX'>0  DO
  . NEW LINE SET LINE=$GET(ARR(ARRIDX))
  . NEW NTENUM SET NTENUM=$GET(TMGHL7MSG(NTEIDX,1))
  . NEW NEWSEGN 
  . IF FIRST SET NEWSEGN=NTEIDX+0.5 SET FIRST=0
  . ELSE  SET NEWSEGN=NTEIDX+0.01
  . FOR  QUIT:($DATA(TMGHL7MSG(NEWSEGN))=0)!(NEWSEGN=NTEIDX)  SET NEWSEGN=NEWSEGN-0.001
  . IF NEWSEGN=NTEIDX QUIT
  . DO SETPCE^TMGHL7X2(NTENUM+1,.TMGHL7MSG,.TMGU,NEWSEGN,1) ;
  . SET $PIECE(TMGHL7MSG(NEWSEGN),TMGU(1),1)="NTE"
  . DO SETPCE^TMGHL7X2(LINE,.TMGHL7MSG,.TMGU,NEWSEGN,3) ;
  . SET $PIECE(TMGHL7MSG(NEWSEGN),TMGU(1),1)="NTE"
  . SET TMGHL7MSG(NEWSEGN,"SEG")="NTE"       
  . SET TMGHL7MSG("B","NTE",NEWSEGN)=""
  . SET NTEIDX=NEWSEGN
  QUIT         
  ;
ADD2NTE(ARR,TMGHL7MSG,TMGU)   ;" Add lines from ARR as extra NTE segments
  ;"Input: ARR -- PASS BY REFERENCE.  The array to add.  Format:
  ;"   ARR(#)=<line of text>
  ;"      TMGHL7MSG -- the array to store in. PASS BY REFERENCE.
  ;"      TMGU -- The array with divisor chars.
  ;"Results: none
  ;"NOTICE: Some messages have separate message sections.  This just adds
  ;"        after last NTE segment.  
  NEW ARRIDX SET ARRIDX=""
  FOR  SET ARRIDX=$ORDER(ARR(ARRIDX)) QUIT:+ARRIDX'>0  DO
  . NEW LINE SET LINE=$GET(ARR(ARRIDX))
  . NEW NTEIDX SET NTEIDX=$ORDER(TMGHL7MSG("B","NTE",""),-1)
  . NEW NTENUM SET NTENUM=$GET(TMGHL7MSG(NTEIDX,1))
  . NEW NEWSEGN SET NEWSEGN=NTEIDX+1
  . FOR  QUIT:($DATA(TMGHL7MSG(NEWSEGN))=0)!(NEWSEGN=NTEIDX)  SET NEWSEGN=NEWSEGN-0.1
  . IF NEWSEGN=NTEIDX QUIT
  . ;"DO SETPCE("NTE",.TMGHL7MSG,.TMGU,NEWSEGN,1) ;
  . DO SETPCE^TMGHL7X2(NTENUM+1,.TMGHL7MSG,.TMGU,NEWSEGN,1) ;
  . SET $PIECE(TMGHL7MSG(NEWSEGN),TMGU(1),1)="NTE"
  . DO SETPCE^TMGHL7X2(LINE,.TMGHL7MSG,.TMGU,NEWSEGN,3) ;
  . SET $PIECE(TMGHL7MSG(NEWSEGN),TMGU(1),1)="NTE"
  . SET TMGHL7MSG(NEWSEGN,"SEG")="NTE"       
  . SET TMGHL7MSG("B","NTE",NEWSEGN)=""       
  QUIT        
  ;