TMGTIUF2 ;TMG/kst/TIU files;7/25/19
       ;;1.0;TMG-LIB;**1**;7/25/19
       ;
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
CHKFILES   ;"Check each patient for files in P:/FPG Charts
  NEW VERBOSE SET VERBOSE=0
  DO CHECK1FILE(22742,"oldrecs",VERBOSE)   
  DO CHECK1FILE(22742.1,"loosedocs",VERBOSE)   
  QUIT  
  ;
  ;"delete later --> NEW DFN SET DFN=0
  ;"delete later --> FOR  SET DFN=$O(^DPT(DFN)) QUIT:DFN'>0  DO
  ;"delete later --> . ;"Check existing entries for deletions
  ;"delete later --> . NEW IEN SET IEN=0
  ;"delete later --> . FOR  SET IEN=$O(^TMG(22742,"B",DFN,IEN)) QUIT:IEN'>0  DO
  ;"delete later --> . . NEW THISPATH
  ;"delete later --> . . SET THISPATH=$P($G(^TMG(22742,IEN,1)),"^",1)
  ;"delete later --> . . NEW HFSROOT SET HFSROOT=$$HFSROOT()
  ;"delete later --> . . SET THISPATH=HFSROOT_$P(THISPATH,"oldrecs/",2)
  ;"delete later --> . . ;"WRITE "CHECKING ",THISPATH,!
  ;"delete later --> . . IF ($$FILEXIST^TMGIOUTL(THISPATH)=0)!($$UP^XLFSTR(THISPATH)'[".PDF") DO
  ;"delete later --> . . . ;"WRITE " ****DELETING ",THISPATH,!
  ;"delete later --> . . . NEW TMGFDA,TMGMSG,TMGIEN
  ;"delete later --> . . . SET TMGFDA(22742,IEN_",",.01)="@"
  ;"delete later --> . . . DO FILE^DIE("E","TMGFDA","TMGMSG")
  ;"delete later --> . . ELSE  DO
  ;"delete later --> . . . ;"WRITE "!!!!EXISTS!!!!",!
  ;"delete later --> . ;"
  ;"delete later --> . ;"Enter new ones
  ;"delete later --> . NEW LIST
  ;"delete later --> . DO PREPRCLK(.LIST,DFN)
  ;"delete later --> . ;"WRITE "== ",DFN," ==",!
  ;"delete later --> . NEW DATE SET DATE=0
  ;"delete later --> . FOR  SET DATE=$O(LIST(DATE)) QUIT:DATE'>0  DO
  ;"delete later --> . . NEW FULLPATH SET FULLPATH=""
  ;"delete later --> . . FOR  SET FULLPATH=$O(LIST(DATE,FULLPATH)) QUIT:FULLPATH=""  DO
  ;"delete later --> . . . NEW FILENAME SET FILENAME=$P(FULLPATH,"^",2)
  ;"delete later --> . . . IF $$UP^XLFSTR(FILENAME)'[".PDF" QUIT
  ;"delete later --> . . . NEW PATH SET PATH=$P(FULLPATH,"^",1)
  ;"delete later --> . . . NEW COMPLETE SET COMPLETE=PATH_FILENAME 
  ;"delete later --> . . . IF $$ISDIR^TMGKERNL(COMPLETE)=1 QUIT  ;"NOT A FOLDER
  ;"delete later --> . . . ;"WRITE "      ->",PATH," ",FILENAME,!
  ;"delete later --> . . . IF $$FEXISTS(DFN,COMPLETE)=1 QUIT ;"W "  *EXISTS",! QUIT
  ;"delete later --> . . . NEW TMGFDA,TMGMSG,TMGIEN
  ;"delete later --> . . . SET TMGFDA(22742,"+1,",.01)="`"_DFN
  ;"delete later --> . . . SET TMGFDA(22742,"+1,",.02)=PATH
  ;"delete later --> . . . SET TMGFDA(22742,"+1,",.03)=FILENAME
  ;"delete later --> . . . SET TMGFDA(22742,"+1,",.04)=DATE
  ;"delete later --> . . . SET TMGFDA(22742,"+1,",1)=COMPLETE
  ;"delete later --> . . . DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
  ;"delete later --> . . . IF $DATA(TMGMSG("DIERR")) DO 
  ;"delete later --> . . . . ;"SET RESULT=-1
  ;"delete later --> . . . . ;"ZWR TMGMSG("DIERR",*)
  ;"delete later --> . . . ELSE  DO
  ;"delete later --> . . . . ;"WRITE "ADDED ",COMPLETE,!
  ;"delete later --> QUIT
  ;
CHECK1FILE(FNUM,DIR,VERBOSE)   ;" Check each patient for files appropriate subfolder in P:/FPG Charts 
  ;"Input: FNUM -- should be 22742 or 22742.1 ONLY!  This only works because data dictionary is identical for both files. 
  ;"       DIR --  should 'oldrecs' or 'loosedocs' ONLY   This matches directories established on HFS
  NEW HFSROOT SET HFSROOT=$$HFSROOT(DIR)
  SET VERBOSE=+$GET(VERBOSE)
  NEW DFN SET DFN=0
  FOR  SET DFN=$O(^DPT(DFN)) QUIT:DFN'>0  DO
  . ;"Check existing entries for deletions
  . NEW IEN SET IEN=0          
  . FOR  SET IEN=$ORDER(^TMG(FNUM,"B",DFN,IEN)) QUIT:IEN'>0  DO
  . . NEW THISPATH SET THISPATH=$P($G(^TMG(FNUM,IEN,1)),"^",1)
  . . SET THISPATH=HFSROOT_$PIECE(THISPATH,DIR_"/",2)
  . . IF VERBOSE WRITE "CHECKING ",THISPATH,!
  . . IF ($$FILEXIST^TMGIOUTL(THISPATH)=0)!($$UP^XLFSTR(THISPATH)'[".PDF") DO
  . . . IF VERBOSE WRITE " ****DELETING ",THISPATH,!
  . . . NEW TMGFDA,TMGMSG,TMGIEN
  . . . SET TMGFDA(FNUM,IEN_",",.01)="@"
  . . . DO FILE^DIE("E","TMGFDA","TMGMSG")
  . . ELSE  DO
  . . . IF VERBOSE WRITE "!!!!EXISTS!!!!",!
  . ;"
  . ;"Enter new ones
  . NEW LIST DO PREPRCLK(.LIST,DFN,DIR)
  . IF VERBOSE WRITE "== ",DFN," ==",!
  . NEW DATE SET DATE=0
  . FOR  SET DATE=$ORDER(LIST(DATE)) QUIT:DATE'>0  DO
  . . NEW FULLPATH SET FULLPATH=""
  . . FOR  SET FULLPATH=$ORDER(LIST(DATE,FULLPATH)) QUIT:FULLPATH=""  DO
  . . . NEW FILENAME SET FILENAME=$P(FULLPATH,"^",2)
  . . . IF $$UP^XLFSTR(FILENAME)'[".PDF" QUIT
  . . . NEW PATH SET PATH=$P(FULLPATH,"^",1)
  . . . NEW COMPLETE SET COMPLETE=PATH_FILENAME 
  . . . IF $$ISDIR^TMGKERNL(COMPLETE)=1 QUIT  ;"NOT A FOLDER
  . . . IF VERBOSE WRITE "      ->",PATH," ",FILENAME,!
  . . . IF $$FEXISTS(DFN,COMPLETE,FNUM)=1 QUIT ;"W "  *EXISTS",! QUIT
  . . . NEW TMGFDA,TMGMSG,TMGIEN
  . . . SET TMGFDA(FNUM,"+1,",.01)="`"_DFN
  . . . SET TMGFDA(FNUM,"+1,",.02)=PATH
  . . . SET TMGFDA(FNUM,"+1,",.03)=FILENAME
  . . . SET TMGFDA(FNUM,"+1,",.04)=DATE
  . . . SET TMGFDA(FNUM,"+1,",1)=COMPLETE
  . . . DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
  . . . IF $DATA(TMGMSG("DIERR")) DO 
  . . . . ;"SET RESULT=-1
  . . . . IF VERBOSE WRITE $$GETERRST^TMGDEBU2(.TMGMSG) ;
  . . . ELSE  DO
  . . . . IF VERBOSE WRITE "ADDED ",COMPLETE,!
  QUIT
  ;
FEXISTS(DFN,FILEPATH,FNUM)  ;"IS FILE AREADY ENTERED IN 22742 OR 22742.1
  NEW TMGRESULT SET TMGRESULT=0
  SET FNUM=$GET(FNUM,22742)  ;"default to 22742
  NEW IEN SET IEN=0
  FOR  SET IEN=$ORDER(^TMG(FNUM,"B",DFN,IEN)) QUIT:IEN'>0  DO
  . NEW THISPATH SET THISPATH=$PIECE($GET(^TMG(FNUM,IEN,1)),"^",1)
  . IF THISPATH=FILEPATH SET TMGRESULT=1
  QUIT TMGRESULT
  ;"
HFSROOT(DIR)  ;"Get the hard coded root in the HFS to the folder holding 'data'
  QUIT "/opt/worldvista/EHR/www/"_DIR_"/"
  ;
PREPRCLK(OUTARR,DFN,DIR)  ;"Prep Record Links listing
  ;"Input: OUTARR -- AN OUT PARAMETER.  PASS BY REFERENCE.  Format:
  ;"            OUTARR(FMDT,URLPATH^FILENAME)=""
  ;"       DFN -- PATIENT IEN
  ;"       DIR --  should 'oldrecs' or 'loosedocs' ONLY.  This matches directories established on HFS
  ;"RESULTS: NONE
  NEW PATHS DO PATH4DFN^TMGRST03(.PATHS,DFN,DIR)   ;"Get one or more filepaths for patient name, including aliases
  NEW APATH SET APATH=""
  FOR  SET APATH=$ORDER(PATHS(APATH)) QUIT:APATH=""  DO
  . NEW HFSPATH SET HFSPATH=$$HFSROOT(DIR)_APATH
  . NEW URLPATH SET URLPATH="/filesystem/"_DIR_"/"_APATH
  . NEW TMGLIST
  . IF $$ISDIR^TMGKERNL(HFSPATH)=0 DO  QUIT
  . . DO ADDLN^TMGRST03(.OUT,"FYI, path: "_HFSPATH_" is not valid<p>")
  . NEW TMGFILTER SET TMGFILTER("*")=""
  . NEW TMP SET TMP=$$LIST^%ZISH(HFSPATH,"TMGFILTER","TMGLIST")
  . IF TMP'=1 DO  QUIT
  . . DO ADDLN^TMGRST03(.OUT,"<P><B>Error reading path: "_HFSPATH)
  . NEW AFILE SET AFILE=""
  . FOR  SET AFILE=$ORDER(TMGLIST(AFILE)) QUIT:AFILE=""  DO
  . . IF $$UP^XLFSTR(AFILE)="THUMBS.DB" QUIT
  . . NEW MODDT SET MODDT=$$FLASTMOD^TMGRST03(HFSPATH_AFILE)  ;"Returns FMDATE, or 0 if problem.
  . . SET OUTARR(+MODDT,URLPATH_"^"_AFILE)=""
  QUIT
  ;
  ;"============================================================================================================
  ;"       FUNCTIONS TO HANDLE LOOSE DOCUMENTS
  ;"============================================================================================================
LOOSEHDL(TMGRESULT,ACTION,DOCIEN,FPATH,TIUDETAILS) 
   ;"RPC ENTRY POINT TO HANDLE ALL THE LOOSE DOCUMENT FUNCTIONS
   ;"ACTION - Which action is being taken
   ;"DOCIEN - IEN FOR THE TMG SCANNED LOOSE DOCUMENT FILE
   ;"FPATH  - PATH OF THE FILE TO HAVE THE ACTION TAKEN ON
   ;"TIUDETAILS - FOR "TIU" ACTION, THE DETAILS FOR ASSIGNING THE NOTE
   ;"             DFN^TIUTITLENAME^AUTHORNAME^DATE^SERVERFNAME
   NEW FULLPATH SET FULLPATH=$$HFSROOT("loosedocs")_$PIECE(FPATH,"loosedocs"_"/",2)
   SET ACTION=$$UP^XLFSTR(ACTION)
   IF ACTION="DELETE" DO DELLOOSE(.TMGRESULT,DOCIEN,FULLPATH)
   ELSE  IF ACTION="MOVE" DO MOVLOOSE(.TMGRESULT,DOCIEN,FULLPATH)
   ELSE  IF ACTION="TIU" DO TIULOOSE(.TMGRESULT,DOCIEN,FULLPATH,TIUDETAILS)
   QUIT
   ;"
DELLOOSE(TMGRESULT,DOCIEN,FPATH)  ;"
   ;"DELETE THE FILE AND REMOVE THE ENTRY IN THE FM FILE
   SET TMGRESULT="1^SUCCESS"
   ;"Delete the file
   SET TMGRESULT=$$DELFILE(FPATH)
   IF $P(TMGRESULT,"^",1)="-1" GOTO DLDN
   ;"Delete the reference
   SET TMGRESULT=$$DELREF(DOCIEN)
DLDN   
   QUIT
   ;"
MOVLOOSE(TMGRESULT,DOCIEN,FPATH)  ;"
   ;"MOVE THE FILE TO SCANNED FILE, REMOVE ENTRY IN FM FILE, ADD TO SCANNED FM FILE
   ;"
   SET TMGRESULT="1^SUCCESS"
   ;"Move the file
   ;"All the "R" path variables below will be with "/filesystem" as is stored in the 22742 and 22742.1 files
   ;"All the "A" path variables will be the absolute path
   NEW OLDRPATH,OLDRFULL,OLDFNAME,OLDRFULL
   NEW NEWRPATH,NEWFNAME,NEWAPATH,NEWAFULL,ZN
   SET ZN=$G(^TMG(22742.1,DOCIEN,0))  ;"GET 0 NODE
   SET OLDRPATH=$P(ZN,"^",2)  ;"GET OLD REL PATH
   SET OLDFNAME=$P(ZN,"^",3)  ;"GET OLD FILE NAME
   SET NEWRPATH=$P(OLDRPATH,"loosedocs",1)_"oldrecs"_$P(OLDRPATH,"loosedocs",2)  ;"CONVERT RPATH TO OLDRECS
   SET NEWAPATH=$$HFSROOT("oldrecs")_$PIECE(NEWRPATH,"oldrecs/",2)  ;"GET NEW ABSOLUTE PATH
   SET TMGRESULT=$$ENSURDIR^TMGKERNL(NEWAPATH) ;"make sure directory exists
   IF $P(TMGRESULT,"^",1)<0 GOTO MLDN  ;"CANNOT MAKE DIRECTORY, QUIT
   ;"
   SET NEWFNAME=$$UNIQUEFN(NEWAPATH,OLDFNAME)   ;"GET UNIQUE FILENAME
   SET NEWAFULL=$$HFSROOT("oldrecs")_$PIECE(NEWRPATH,"oldrecs/",2)_NEWFNAME
   SET OLDAFULL=$$HFSROOT("loosedocs")_$PIECE(OLDRPATH,"loosedocs/",2)_OLDFNAME
   SET TMGRESULT=$$MOVE^TMGKERNL(OLDAFULL,NEWAFULL)
   IF TMGRESULT=0 SET TMGRESULT="1^SUCCESSFUL"
   ELSE  SET TMGRESULT="-1^ERROR MOVING: "_OLDAFULL_" TO "_NEWAFULL GOTO MLDN    	
   ;"Add new reference in the Scanned Document file
   ;"
   NEW TMGFDA,TMGIEN,TMGMSG 
   SET TMGFDA(22742,"+1,",.01)=$P(ZN,"^",1)
   SET TMGFDA(22742,"+1,",.02)=NEWRPATH
   SET TMGFDA(22742,"+1,",.03)=NEWFNAME  
   SET TMGFDA(22742,"+1,",.04)=$P(ZN,"^",4)
   SET TMGFDA(22742,"+1,",1)=NEWRPATH_NEWFNAME
   DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
   IF $DATA(TMGMSG) SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG) QUIT
   ;"Delete the reference
   ;"
   SET TMGRESULT=$$DELREF(DOCIEN)
   IF $P(TMGRESULT,"^",1)="-1" GOTO MLDN
MLDN   
   QUIT
   ;"
TIULOOSE(TMGRESULT,DOCIEN,FPATH,TIUDETAILS)  ;"
   ;"TIUDETAILS - DFN^TIUTITLENAME^AUTHORNAME^DATE^SERVERFNAME
   ;"MOVE TO TIU, DELETE THE REFERENCE, DELETE THE FILE
   ;"
   SET TMGRESULT="1^SUCCESS"
   NEW DFN,TIUTITLE,AUTHORNAME,DOS,NEWFNAME,TIUIEN
   SET DFN=$P(TIUDETAILS,"^",1),TIUTITLE=$P(TIUDETAILS,"^",2)
   SET AUTHORNAME=$P(TIUDETAILS,"^",3),DOS=$P(TIUDETAILS,"^",4)
   SET NEWFNAME=$P(TIUDETAILS,"^",5)
   ;"
   ;"Delete the reference
   SET TMGRESULT=$$DELREF(DOCIEN)
   IF $P(TMGRESULT,"^",1)="-1" GOTO MLDN
   ;"Set the text of the TIU note
   DO BLANKTIU^TMGRPC1(.TIUIEN,DFN,AUTHORNAME,6,DOS,TIUTITLE)
   IF TIUIEN'>0 SET TMGRESULT="-1^COULD NOT CREATE NOTE TITLE" GOTO MLDN
   ;"
   NEW TMGFDA,TMGMSG
   SET TMGFDA(8925,TIUIEN_",",1303)="R"  ;"1303 = capture method. "R" = RPC
   DO FILE^DIE("E","TMGFDA","TMGMSG")  ;"ignore any errors.
   NEW NOTETEXT
   SET NOTETEXT("TEXT",1,0)="<!DOCTYPE HTML PUBLIC>"
   SET NOTETEXT("TEXT",2,0)="<html>"
   SET NOTETEXT("TEXT",3,0)="<head>"
   SET NOTETEXT("TEXT",4,0)="<meta http-equiv=""Content-Type"" content=""text/html; charset=iso-8859-1"">"
   SET NOTETEXT("TEXT",5,0)="<title>VistA HTML Note</title>"
   SET NOTETEXT("TEXT",6,0)="</head>"
   SET NOTETEXT("TEXT",7,0)="<body>"
   SET NOTETEXT("TEXT",8,0)="<p>"
   SET NOTETEXT("TEXT",9,0)="Note created automatically from loose document."
   SET NOTETEXT("TEXT",10,0)="<p>"
   SET NOTETEXT("TEXT",11,0)="<embed src=""$CPRSDIR$\Cache\"_NEWFNAME_""""_"  width=""800px"" height=""1200px"" />"
   SET NOTETEXT("TEXT",12,0)="<p>"
   SET NOTETEXT("TEXT",13,0)="</body>"
   SET NOTETEXT("TEXT",14,0)="</html>"
   SET NOTETEXT("HDR")="1^1"
   DO SETTEXT^TMGTIUS1(.TMGRESULT,TIUIEN,.NOTETEXT,0)
   ;"NOW SIGN THE NOTE
   NEW SIGNATURE SET SIGNATURE=$PIECE($GET(^VA(200,DUZ,20)),"^",2) ;"elh added for signature reasons
       ;"IF SIGNATURE="" SET SIGNATURE=AuthorName  ;"elh added for signature 10/1/15
   SET TMGFDA(8925,TIUIEN_",",.05)="COMPLETED"      ;"field .05 = STATUS
   SET TMGFDA(8925,TIUIEN_",",1501)="NOW"           ;"field 1501 = Signed date
   SET TMGFDA(8925,TIUIEN_",",1502)="`"_DUZ   ;"field 1502 = signed by
   SET TMGFDA(8925,TIUIEN_",",1503)=SIGNATURE    ;"formerly AuthorName  ;"field 1503 = Signature block name
   SET TMGFDA(8925,TIUIEN_",",1504)="[Scanned image auto-signed]" ;"field 1504 = Signature block title
   SET TMGFDA(8925,TIUIEN_",",1505)="C"  ;C=Chart   ;"field 1505 = Signature mode
   DO FILE^DIE("E","TMGFDA","TMGMSG")
   ;"
   SET TMGRESULT=$$DELFILE(FPATH)
   QUIT
   ;"
DELREF(DOCIEN)  ;"DELETE THE REFERENCE IN THE ^TMG(22742.1
   ;"
   NEW TMGFDA,TMGMSG,TMGIEN,RESULT
   SET RESULT="1^SUCCESSFUL"
   SET TMGFDA(22742.1,DOCIEN_",",.01)="@"
   DO FILE^DIE("E","TMGFDA","TMGMSG")
   IF $DATA(TMGMSG("DIERR")) DO
   . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG) 
   QUIT RESULT
   ;"
DELFILE(FPATH)  ;"Delete given file
    ;"
    NEW RESULT 
    SET RESULT=$$DELFILE^TMGIOUTL(FPATH)
    IF RESULT=0 SET RESULT="-1^FAILURE DELETING FILE "_FPATH
    ELSE  SET RESULT="1^SUCCESSFUL"
    QUIT RESULT
    ;"
UNIQUEFN(PATH,FILENAME)  ;"RETURNS A UNIQUE FILENAME
    IF $$FILEXIST^TMGIOUTL(PATH_FILENAME)=0 SET NAME=FILENAME GOTO UFDN
    NEW COUNT,DONE
    SET COUNT=1,DONE=0
    NEW FNAME,FEXT
    SET FEXT=$$EXTNEXTR^TMGIOUTL(FILENAME)
    SET FNAME=$E(FILENAME,1,$L(FILENAME)-$L(FEXT)-1)
    FOR COUNT=1:1:51 QUIT:DONE=1  DO  ;"STOP AT 50 AND ERROR OUT
    . NEW TEMPFILE SET TEMPFILE=FNAME_"-"_COUNT_"."_FEXT
    . IF $$FILEXIST^TMGIOUTL(PATH_TEMPFILE)=0  DO
    . . SET FILENAME=TEMPFILE
    . . SET DONE=1
    IF DONE=0 SET FILENAME="-1^COULD NOT CREATE UNIQUE FILENAME"
UFDN    
    QUIT FILENAME
    ;"
TIUTOLOS(TMGRESULT,DFN,TIUIEN,FILENAME)  ;"RPC ENTRY: TMG TIU NOTE TO LOOSE
   ;"SET FILENAME AND PATH
   SET TMGRESULT="1^SUCCESS"
   NEW PATHS DO PATH4DFN^TMGRST03(.PATHS,DFN,"loosedocs")
   NEW FOLDERNAME SET FOLDERNAME=$$HFSROOT("loosedocs")_$O(PATHS(""))  ;"JUST USE FIRST PATH
   NEW PDFNAME,HTMLNAME
   IF FILENAME["" SET FILENAME=$TR(FILENAME," ","_")
   SET PDFNAME=FILENAME_".pdf"
   SET HTMLNAME=FILENAME_".html"
   ;"GET NOTE TEXT AND RESOLVE PATHS TO IMAGES
   NEW ISHTML SET ISHTML=$$ISHTML^TMGHTM1(TIUIEN)
   NEW REF,NOTEARR
   DO TGET^TIUSRVR1(.REF,TIUIEN,"VIEW;A")
   MERGE NOTEARR=@REF
   SET NOTEARR(1)=$G(NOTEARR)_"<!-- HEADER CENTER ""DOS: "_$$EXTDATE^TMGDATE($P($G(^TIU(8925,TIUIEN,13)),"^",1),1)_""" -->"
   DO EXPHTML^TMGTIUPR(.NOTEARR,FOLDERNAME,HTMLNAME,ISHTML)
   ;"CONVERT TO PDF
   NEW LINUXCMD SET LINUXCMD="htmldoc --webpage "_FOLDERNAME_HTMLNAME_" --outfile "_FOLDERNAME_PDFNAME
   NEW EXPRESULT
   SET EXPRESULT=$$LINUXCMD^TMGKERNL(LINUXCMD)
   IF $P(EXPRESULT,"^",1)'=1 DO  GOTO TOLDN
   . SET TMGRESULT="-1^ERROR CREATING PDF: "_$P(EXPRESULT,"^",2)
   IF $$FILEXIST^TMGIOUTL(FOLDERNAME_PDFNAME)=0 DO  GOTO TOLDN
   . SET TMGRESULT="-1^THERE WAS AN UNKNOWN ERROR. PDF WAS NOT CREATED IN LOOSE DOCUMENTS."
   ;"DELETE THE HTML FILE
   DO DELFILE(FOLDERNAME_HTMLNAME)
   ;"ADD REFERENCE TO THE NEW LOOSE DOCUMENT
   NEW TMGFDA,TMGIEN,TMGMSG 
   SET TMGFDA(22742.1,"+1,",.01)=DFN
   SET TMGFDA(22742.1,"+1,",.02)="/filesystem/loosedocs/"_$O(PATHS(""))
   SET TMGFDA(22742.1,"+1,",.03)=PDFNAME  
   SET TMGFDA(22742.1,"+1,",.04)=$$TODAY^TMGDATE
   SET TMGFDA(22742.1,"+1,",1)="/filesystem/loosedocs/"_$O(PATHS(""))_PDFNAME
   DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
   IF $DATA(TMGMSG) SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG) QUIT
TOLDN   
   QUIT
    ;"