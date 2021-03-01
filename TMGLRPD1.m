TMGLRPD1 ;TMG/kst-Entry point for storing lab result PDFs ; 10/19/20
              ;;1.0;TMG-LIB;**1**;06/20/13
 ;
 ;"TMG LAB RESULTS PDF STORAGE API
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
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"
 ;"DOWNLOADLABPDF(GREF,RELPATH,FNAME)  -- DOWNLOAD LAB PDF
 
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;
STOREPDF(TMGHL7MSG,MSGINFO)  ;"store base-64-encoded PDF
  ;"NOTE: TMGHL7MSG("PDF") is initially set up in ZEF2^TMGHL73
  ;"      It was initially one long line.  Split into shorter lines for easier processing
  ;"      The data is a base-64 encoded PDF
  ;"Input:  TMGHL7MSG -- standard parsed array from HL7 message
  ;"        MSGINFO -- as set up by GETINFO^TMGLRW01.  E.g.
  ;"          }~"DFN" = 70706
  ;"          }~"DOB" = 04-11-47
  ;"          }~"IEN 22720" = 2
  ;"          }~"IEN 4" = 73
  ;"          }~"IEN 62.4" = 204
  ;"          }~"IEN 68.2" = 2
  ;"          }~"IEN 772" = 56783
  ;"          }~"IEN 773" = 56731
  ;"          }~"LAB IEN4" = 73^PATHGROUP
  ;"          }~"LOCATION" = Family Phys Of Greeneville
  ;"          }~"LOCATION IEN" = 69
  ;"          }~"LRDFN" = 557                   
  ;"          }~"NAME" = xxxxx,xxxxxx
  ;"          }~"NPI" = ###########
  ;"          }~"ORDER UID" = 20C2T97004LAB
  ;"          }~"PROV IEN" = 83
  ;"          }~"SEX" = F
  ;"          }~"SSNUM" = xxxxxxxxx
  ;"Result: "1^OK", or "-1^Message"
  NEW TMGRESULT SET TMGRESULT="1^OK"
  NEW TMGFDA,TMGMSG,TMGIEN,TMGERR
  NEW FILEOVERWRITE SET FILEOVERWRITE=0
  ;
  NEW TMGDEBUG SET TMGDEBUG=0
  IF TMGDEBUG=0 DO
  . KILL ^TMP($J,"TMGLRPD1","TMGHL7MSG")
  . KILL ^TMP($J,"TMGLRPD1","MSGINFO")
  . MERGE ^TMP($J,"TMGLRPD1","TMGHL7MSG")=TMGHL7MSG
  . MERGE ^TMP($J,"TMGLRPD1","MSGINFO")=MSGINFO
  IF TMGDEBUG=1 DO
  . KILL TMGHL7MSG,MSGINFO
  . MERGE TMGHL7MSG=^TMP($J,"TMGLRPD1","TMGHL7MSG")
  . MERGE MSGINFO=^TMP($J,"TMGLRPD1","MSGINFO")
  ;"IF TMGDEBUG=0 GOTO STRPDFDN  ;"TURN OFF FOR NOW.
  ;
  NEW TMGARR MERGE TMGARR=TMGHL7MSG("PDF")        
  DO DECODE^TMGRPC1C("TMGARR(1)",1)  ;"Convert base-64 to binary data
  NEW DFN SET DFN=$GET(MSGINFO("DFN"))
  IF DFN'>0 DO  GOTO STRPDFDN
  . SET TMGRESULT="-1^In STOREPDF.TMGLRPD1, no DFN provided"
  NEW ROOTPATH SET ROOTPATH=$$GETROOTPATH()                                
  NEW RELPATH SET RELPATH=$$GETRELPATH(DFN)
  IF +RELPATH=-1 SET TMGRESULT=RELPATH GOTO STRPDFDN
  NEW FULLPATH SET FULLPATH=ROOTPATH_RELPATH
  NEW OUID SET OUID=$GET(MSGINFO("ORDER UID"))
  IF OUID="" DO  GOTO STRPDFDN
  . SET TMGRESULT="-1^In STOREPDF.TMGLRPD1, Unable to determine ORDER UID when saving PDF"  
  NEW TEMP SET TEMP=$$ENSURDIR^TMGKERNL(FULLPATH)  ;"FORCES EXISTANCE
  NEW FNAME SET FNAME=OUID_".pdf"
  IF $$ISFILE^TMGKERNL(FULLPATH_FNAME)=1 DO  
  . SET FILEOVERWRITE=1
  . NEW KILLARR SET KILLARR(FNAME)=""
  . NEW SUCCESS SET SUCCESS=$$DEL^%ZISH(FULLPATH,"KILLARR")
  . IF SUCCESS=1 QUIT
  . SET TMGRESULT="-1^In STOREPDF.TMGLRPD1. Error deleting prior PDF: "_FULLPATH_FNAME
  IF +TMGRESULT=-1 GOTO STRPDFDN
  NEW SUCCESS SET SUCCESS=$$GTBF^TMGBINF("TMGARR(1)",1,FULLPATH,FNAME) ;
  IF SUCCESS'=1 DO  GOTO STRPDFDN
  . SET TMGRESULT="-1^In STOREPDF.TMGLRPD1: Error saving PDF: "_$PIECE(SUCCESS,"^",2)
  ;
  IF FILEOVERWRITE=1 GOTO STRPDFDN  ;"If PDF overwritten, prior metadata should be unchanged.  
  ;"====== Store metadata =========
  IF $DATA(^TMG(22745,DFN)) GOTO L1
  ;"-- make top level record
  SET TMGFDA(22745,"+1,",.01)=DFN
  SET TMGFDA(22745,"+1,",.03)=$GET(MSGINFO("LRDFN"))
  SET TMGIEN(1)=DFN
  DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERROR")) DO  GOTO STRPDFDN
  . SET TMGRESULT="-1^Error: "_$$GETERRST^TMGDEBU2(.TMGMSG)        
L1 ;"-- add subrecord --
  KILL TMGFDA,TMGIEN,TMGMSG
  NEW IENS SET IENS="+1,"_DFN_","
  SET TMGFDA(22745.01,IENS,.01)=OUID
  NEW IEN22720 SET IEN22720=$GET(MSGINFO("IEN 22720"))
  IF IEN22720>0 SET TMGFDA(22745.01,IENS,.02)=IEN22720
  NEW IEN4 SET IEN4=$GET(MSGINFO("IEN 4"))
  IF IEN4>0 SET TMGFDA(22745.01,IENS,.03)=IEN4
  NEW IEN62D4 SET IEN62D4=$GET(MSGINFO("IEN 62.4"))
  IF IEN62D4>0 SET TMGFDA(22745.01,IENS,.04)=IEN62D4
  NEW IEN68D2 SET IEN68D2=$GET(MSGINFO("IEN 68.2"))
  IF IEN68D2>0 SET TMGFDA(22745.01,IENS,.05)=IEN68D2
  NEW ORCSEG SET ORCSEG=$ORDER(TMGHL7MSG("B","ORC",0)) IF ORCSEG'>0 DO  GOTO STRPDFDN
  . SET TMGRESULT="-1^Error: In STOREPDF.TMGLRPD1, ORC segment not found in HL7 message array"
  NEW HL7DT SET HL7DT=$GET(TMGHL7MSG(ORCSEG,9))  ;"ORC collection date  
  IF HL7DT="" DO  GOTO STRPDFDN
  . SET TMGRESULT="-1^Error: In STOREPDF.TMGLRPD1, Unable to get ORC collection date for HL7 message"  
  NEW FMDT SET FMDT=$$HL72FMDT^TMGHL7U3(HL7DT)
  SET TMGFDA(22745.01,IENS,.06)=FMDT
  SET TMGFDA(22745.01,IENS,1)=FNAME
  SET TMGFDA(22745.01,IENS,2)=RELPATH
  DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERROR")) DO  GOTO STRPDFDN
  . SET TMGRESULT="-1^Error: "_$$GETERRST^TMGDEBU2(.TMGMSG)        
STRPDFDN ;
   QUIT TMGRESULT
   ;
GETRELPATH(DFN) ;"get a file path, based on DFN.  Will create path on HFS if doesn't exist
  ;"Result: Returns file path, or -1^Message if error
  NEW PADLEN SET PADLEN=$SELECT($LENGTH(DFN)>9:12,$LENGTH(DFN)>6:9,1:6)
  NEW DFNSTR SET DFNSTR=$$RJ^XLFSTR(DFN,PADLEN,"0")
  NEW PATH SET PATH=""
  NEW IDX FOR IDX=1:3:$LENGTH(DFNSTR) DO
  . SET PATH=PATH_$EXTRACT(DFNSTR,IDX,IDX+2)_"/"
GPDN ;
  QUIT PATH
  ;
GETROOTPATH() ;
  ;"NOTE: Could later make this as a database entry etc.  
  ;"NOTE2: Should end in '/' 
  QUIT "/opt/worldvista/EHR/media/lab_pdf/"
  ;
RPCHASPDF(OUT,DFN,SDT,EDT)  ;"RPC FOR HAS LAB PDF for given date range?
  ;"Purpose: Return array with any PDF's available for patient for date range.
  ;"Input:  OUT -- PASS BY REFERENCE, and out parameter.  See output
  ;"        DFN -- patient IEN
  ;"        SDT -- Starting date-time (Fileman format).  Optional, default is earliest possible date.  
  ;"        EDT -- OPTIONAL -- ending date-time.  Default is last possible date
  ;"Output: OUT is filled as follow:
  ;"          OUT(0)="1^OK", or "-1^Error message"
  ;"          OUT(1)=SUBIEN^<RELATIVE PATH>^<FILE NAME>
  ;"          OUT(2)=SUBIEN^<RELATIVE PATH>^<FILE NAME>   ... etc
  ;"        If none found, then empty array returned.
  ;"        OUT=number found, or -1^Message if problem
  SET OUT(0)="1^OK" ;"default
  SET DFN=$GET(DFN) IF DFN'>0 DO  GOTO RPCDN
  . SET OUT(0)="-1^No DFN provided"
  IF $DATA(^TMG(22745,DFN))=0 GOTO RPCDN
  SET SDT=+$GET(SDT) IF SDT'>0 SET SDT=1
  SET SDT=$P(SDT,".",1)-1_".999999"  ;"2/18/21 BACK UP TO CATCH PDFS WITH DATE AND NO TIME (e.g. 3210218)
  SET EDT=$GET(EDT) IF EDT'>0 SET EDT=9999999.999999
  NEW ADT SET ADT=SDT-0.000001
  NEW IDX SET IDX=1
  FOR  SET ADT=$ORDER(^TMG(22745,DFN,1,"ATMGDT",ADT)) QUIT:(ADT'>0)!(ADT>EDT)  DO
  . IF (ADT<SDT)!(ADT>EDT) QUIT  ;"Added this as extra check because $O above wasn't handling the FMDate time correctly
  . NEW SUBIEN SET SUBIEN=0
  . FOR  SET SUBIEN=$ORDER(^TMG(22745,DFN,1,"ATMGDT",ADT,SUBIEN)) QUIT:SUBIEN'>0  DO
  . . NEW FNAME SET FNAME=$GET(^TMG(22745,DFN,1,SUBIEN,1))
  . . NEW RELPATH SET RELPATH=$GET(^TMG(22745,DFN,1,SUBIEN,2))
  . . SET OUT(IDX)=SUBIEN_"^"_RELPATH_"^"_FNAME_"^"_ADT
  . . SET IDX=IDX+1
RPCDN  ;
  QUIT
  ;
DOWNLOADLABPDF(GREF,RELPATH,FNAME)  ; DOWNLOAD A LAB PDF
  ;"SCOPE: Public
  ;"Purpose: To provide an entry point for a RPC call from a client.  The client
  ;"              will ask for a given pdf file, and it will be passed back in the form
  ;"              of an array (in BASE64 ascii encoding)
  ;"Input: GREF --     OUT PARAM -- the array to pass the result back in (PASSED BY REFERENCE)
  ;"       RELPATH --  the file path up to, but not including, the filename
  ;"       FNAME --    the name of the file to pass back
  ;"Output: results are passed out in @GREF
  ;"       @GREF@(0)=success;    1=success, 0^ErrorMessage if error
  ;"       @GREF@(1..xxx) = actual data
  ;"NOTE: Copied and modified from DOWNLOAD^TMGRPC1C 
  ;
  SET FNAME=$GET(FNAME)
  SET GREF="^TMP(""DOWNLOADPDF^TMGLRPD1"","_$J_")"
  KILL @GREF
  NEW ROOTPATH SET ROOTPATH=$$GETROOTPATH() ;
  NEW FULLPATH SET FULLPATH=ROOTPATH_RELPATH
  NEW TEMP SET TEMP=$$BFTG^TMGBINF(.FULLPATH,.FNAME,$NAME(@GREF@(1)),3)
  SET @GREF@(0)=TEMP
  IF +TEMP=1 DO ENCODE^TMGRPC1C($NAME(@GREF@(1)),3)
  QUIT
  ;
UPLDPDF(TMGRESULT,PDF,FNAME,DFN,LABFMDT)  ;"RPC TO UPLOAD A LAB PDF
  ;"Input:  PDF - The file, in BASE64 ascii encoding
  ;"        PDFNAME - Name of the file
  ;"        DFN - Patient IEN
  ;"        LABDT - Lab date/time
  ;"Result: "1^OK", or "-1^Message"
  NEW TMGTEST SET TMGTEST=0
  IF TMGTEST=1 DO
  . MERGE PDF=^TMP(31514,"UPLDPDF","PDF")
  . SET FNAME=$G(^TMP(31514,"UPLDPDF","FNAME"))
  . SET DFN=$G(^TMP(31514,"UPLDPDF","DFN"))
  . SET LABFMDT=$G(^TMP(31514,"UPLDPDF","LABFMDT"))
  ELSE  DO
  . MERGE ^TMP($J,"UPLDPDF","PDF")=PDF
  . SET ^TMP($J,"UPLDPDF","FNAME")=FNAME
  . SET ^TMP($J,"UPLDPDF","DFN")=DFN
  . SET ^TMP($J,"UPLDPDF","LABFMDT")=LABFMDT	  
  SET TMGRESULT="1^OK"
  NEW TMGFDA,TMGMSG,TMGIEN,TMGERR
  NEW FILEOVERWRITE SET FILEOVERWRITE=0
  ;        
  DO DECODE^TMGRPC1C("PDF(0)",1)  ;"Convert base-64 to binary data
  IF DFN'>0 DO  GOTO UPPDFDN
  . SET TMGRESULT="-1^No DFN provided"
  NEW ROOTPATH SET ROOTPATH=$$GETROOTPATH()                                
  NEW RELPATH SET RELPATH=$$GETRELPATH(DFN)
  IF +RELPATH=-1 DO  GOTO UPPDFDN
  . SET TMGRESULT="-1^COULD NOT GET RELPATH"
  NEW FULLPATH SET FULLPATH=ROOTPATH_RELPATH  
  NEW TEMP SET TEMP=$$ENSURDIR^TMGKERNL(FULLPATH)  ;"FORCES EXISTANCE
  IF $$ISFILE^TMGKERNL(FULLPATH_FNAME)=1 DO  GOTO UPPDFDN
  . SET TMGRESULT="-1^File Exists. Consider autochanging name: "_FULLPATH_FNAME
  NEW SUCCESS SET SUCCESS=$$GTBF^TMGBINF("PDF(0)",1,FULLPATH,FNAME) ;
  IF SUCCESS'=1 DO  GOTO UPPDFDN
  . SET TMGRESULT="-1^Error saving PDF: "_$PIECE(SUCCESS,"^",2)
  ;"
  ;"NEW TMGCONTINUE SET TMGCONTINUE=0
  ;"IF TMGCONTINUE=0 QUIT  ;"TEMP
  ;  
  ;"====== Store metadata =========
  IF $DATA(^TMG(22745,DFN)) GOTO L2
  ;"-- make top level record
  NEW LRDFN SET LRDFN=+$G(^DPT(DFN,"LR"))
  IF LRDFN'>0 DO  GOTO UPPDFDN
  . SET TMGRESULT="-1^LRDFN NOT FOUND FOR PATIENT"
  SET TMGFDA(22745,"+1,",.01)=DFN
  SET TMGFDA(22745,"+1,",.03)=LRDFN     ;"<===GET LRDFN
  SET TMGIEN(1)=DFN
  DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERROR")) DO  GOTO UPPDFDN
  . SET TMGRESULT="-1^Error: "_$$GETERRST^TMGDEBU2(.TMGMSG)        
L2 ;"-- add subrecord --
  KILL TMGFDA,TMGIEN,TMGMSG
  NEW IENS SET IENS="+1,"_DFN_","
  SET TMGFDA(22745.01,IENS,.01)=$P(FNAME,".",1)
  ;"CAN WE DO WITHOUT THIS?? IF IEN22720>0 SET TMGFDA(22745.01,IENS,.02)=IEN22720
  ;"AND WITHOUT WITHOUT THIS?? IF IEN4>0 SET TMGFDA(22745.01,IENS,.03)=IEN4
  ;"ALSO THIS?? IF IEN62D4>0 SET TMGFDA(22745.01,IENS,.04)=IEN62D4
  ;"FINALLY THIS??  IF IEN68D2>0 SET TMGFDA(22745.01,IENS,.05)=IEN68D2
  ;"
  ;"NEW ORCSEG SET ORCSEG=$ORDER(TMGHL7MSG("B","ORC",0)) IF ORCSEG'>0 DO  GOTO STRPDFDN
  ;". SET TMGRESULT="-1^Error: In STOREPDF.TMGLRPD1, ORC segment not found in HL7 message array"
  ;"NEW HL7DT SET HL7DT=$GET(TMGHL7MSG(ORCSEG,9))  ;"ORC collection date  
  ;"IF HL7DT="" DO  GOTO STRPDFDN
  ;". SET TMGRESULT="-1^Error: In STOREPDF.TMGLRPD1, Unable to get ORC collection date for HL7 message"  
  ;"NEW FMDT SET FMDT=$$HL72FMDT^TMGHL7U3(HL7DT)
  ;"
  SET TMGFDA(22745.01,IENS,.06)=LABFMDT
  SET TMGFDA(22745.01,IENS,1)=FNAME
  SET TMGFDA(22745.01,IENS,2)=RELPATH
  DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERROR")) DO  GOTO STRPDFDN
  . SET TMGRESULT="-1^Error: "_$$GETERRST^TMGDEBU2(.TMGMSG)        
UPPDFDN ;
   QUIT
   ;  
  ;"==================================================================
  ;
KILL1() ;"DELETE THIS LATER...
  NEW DEFPATH SET DEFPATH="/mnt/WinServer/LaughlinHL7"
  NEW FILEOPTION 
  SET FILEOPTION("PATH")=DEFPATH
  SET FILEOPTION("SELECT DIR")=1
  NEW SELARR SET SELARR("*.txt")=""
  NEW DIRNAME,FNAME,%,TMGRESULT,DIRARR
  NEW UIDARR
  SET (DIRNAME,FNAME)=""
  SET DIRNAME=$$FBROWSE^TMGIOUT2(.FILEOPTION)
  IF $$LIST^%ZISH(DIRNAME,"SELARR","DIRARR")=0 DO  GOTO TSTDN
  SET FNAME=""
  FOR  SET FNAME=$ORDER(DIRARR(FNAME)) QUIT:FNAME=""  DO
  . WRITE FNAME,!
  . NEW TMGMSG,MSH,OPTION
  . NEW TMGHL7MSG,TMGU
  . NEW TMGENV SET TMGENV("ALERT CODE")="SETALERT^TMGHL7E"  ;"Default, likely overwritten later
  . NEW TMGRESULT 
  . NEW FPNAME SET FPNAME=$$MKTRALDV^TMGIOUTL(DIRNAME)_FNAME
  . SET OPTION("FILEPATHNAME")=FPNAME  ;"Used in HLMSGIMPORT()
  . SET TMGRESULT=$$LOADHL7^TMGHL7U2(FPNAME,.TMGMSG,.MSH) ;"LOAD FILE INTO ARRAY
  . IF TMGRESULT'>0 WRITE TMGRESULT,! QUIT
  . SET TMGRESULT=$$SETUPENV^TMGHL7U(.TMGMSG,.TMGENV)    
  . IF TMGRESULT'>0 WRITE TMGRESULT,! QUIT
  . SET TMGRESULT=$$HL7PROCESS^TMGHL71(.TMGHL7MSG,.TMGENV,.TMGMSG,.OPTION) ;"PARSE, then XFORM
  . IF TMGRESULT'>0 WRITE TMGRESULT,! QUIT
  . NEW HL7DT SET HL7DT=$GET(TMGHL7MSG(1,7))
  . NEW FMDT SET FMDT=$$HL72FMDT^TMGHL7U3(HL7DT),OPTION("FM DATE")=FMDT
  . NEW PIDIDX SET PIDIDX=+$ORDER(TMGHL7MSG("B","PID",""))
  . NEW PTNAME SET PTNAME=$GET(TMGHL7MSG(PIDIDX,5))
  . NEW UID SET UID=$GET(TMGHL7MSG(1,10))
  . WRITE $$FMTE^XLFDT(HL7DT,5)," ",PTNAME," UID=",UID,!
  . SET UIDARR(UID)=+$GET(UIDARR(UID))+1
  ZWR UIDARR
TSTDN ;
  QUIT
  ;
  
  
  