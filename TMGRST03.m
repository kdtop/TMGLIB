TMGRST03 ;TMG/kst/REST web service; 3/3/15
       ;;1.0;TMG-LIB;**1**;3/3/15
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 7/17/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"MAKEURL(DFN)  -- Assemble URL to return to CPRS to get page generated here. 
 ;"DATA(RESULT,ARGS) -- assemble web page
 ; 
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"HFSROOT() --Get the hard coded root in the HFS to the folder holding 'data'
 ;"GETHDR(OUT,RTN)  
 ;"GETBODY(OUT,DFN)
 ;"GETFTR(OUT)
 ;"PREPRCLK(OUTARR,DFN) --Prep listing of record links 
 ;"ADDTABLE(OUT,DFN)  
 ;"ADDROW(OUT,DATA1,DATA2,ISROWEVEN,INDENT)  ;
 ;"PATH4DFN(OUT,DFN) -- Get one or more filepaths for patient name, including aliases
 ;"GETNAMES(OUT,DFN) --Get prepped names, including aliases
 ;"PREPNAME(NAME,OUTLNAME,OUTFNAME) 
 ;"PATH4NAM(OUT,LNAME,FNAME,FMDOB) -- Get one or more filepaths for patient name
 ;"GRPNAME(LNAME) -- Get group directory name
 ;"FLASTMOD(FPNAME) -- Returns FMDATE, or 0 if problem.
 ;"ADDLN(OUT,TXT,ADDBR)  
 ;"GETSTYLE(OUT)  
 ;"STYLE -- html code for styles. 
 ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"=======================================================================
 ;"Uses: XLFSTR, TMGKERNL 
 ;"=======================================================================
 ;
MAKEURL(DFN)  ;"
  QUIT "Old Recs^http://192.168.3.99:9080/data/"_DFN
  ;"
HFSROOT(DIR)  ;"Get the hard coded root in the HFS to the folder holding 'data'
  IF DIR="" SET DIR="oldrecs"
  QUIT "/opt/worldvista/EHR/www/"_DIR_"/"
  ;
DATA(RESULT,ARGS) ; GET Mumps Routine   ;"Modified from R^%W0
  ;"Input: RESULT -- PASSED BY REFRENCE. AN OUT PARAMETER.  Format:
  ;"         RESULT("mime")=mime type
  ;"         RESULT(#)=<line of result>
  ;"       ARGS 
  ;"NOTE: uses HTTPREQ in global scope.
  NEW TMGRST3 SET TMGRST3=0
  IF TMGRST3=1 DO
  . KILL ARGS,HTTPREQ
  . MERGE ARGS=^TMG("TMP","TMGRST03","ARGS")
  . MERGE HTTPREQ=^TMG("TMP","TMGRST03","HTTPREQ")
  ELSE  DO
  . KILL ^TMG("TMP","TMGRST03")
  . MERGE ^TMG("TMP","TMGRST03","ARGS")=ARGS
  . MERGE ^TMG("TMP","TMGRST03","HTTPREQ")=HTTPREQ
  ;
  NEW TMGINDENTLEN SET TMGINDENTLEN=2  ;"Will be used in global scope by event handlers below
  NEW DFN SET DFN=+$PIECE($GET(HTTPREQ("path")),"/data/",2)
  IF DFN'>0 QUIT  
  NEW OUT 
  KILL RESULT SET RESULT=$NAME(^TMP($J))
  KILL @RESULT
  DO GETHDR(.OUT,"SCANNED RECORDS")
  DO GETBODY(.OUT,DFN) 
  DO GETFTR(.OUT) 
  SET RESULT("mime")="text/html; charset=utf-8"
  MERGE @RESULT=OUT
  QUIT
  ;
GETHDR(OUT,RTN)  ;
  DO ADDLN(.OUT,"<!DOCTYPE html>")
  DO ADDLN(.OUT,"<html>")
  DO ADDLN(.OUT,"<head>")
  DO ADDLN(.OUT,"  <title>"_RTN_" </title>")
  DO GETSTYLE(.OUT)
  DO ADDLN(.OUT,"</head>")
  DO ADDLN(.OUT,"<body>")
  QUIT
  ;
GETBODY(OUT,DFN) ;
  NEW NAMES
  DO GETNAMES(.NAMES,DFN) ;"Get prepped names, including aliases
  NEW FIRST SET FIRST=1
  NEW ANAME SET ANAME=""
  FOR  SET ANAME=$ORDER(NAMES(ANAME)) QUIT:ANAME=""  DO
  . NEW LINE SET LINE=""
  . IF 'FIRST SET LINE=LINE_" (alias) "
  . SET LINE=LINE_ANAME
  . DO ADDLN(.OUT,LINE_"<BR>")
  . SET FIRST=0
  DO ADDLN(.OUT,"<P>")
  DO ADDTABLE(.OUT,DFN)  
  DO ADDLN(.OUT,"<P>")
  QUIT
  ;
ADDTABLE(OUT,DFN)  ;
  NEW ARR DO PREPRCLK(.ARR,DFN)  ;"Prep listing of links to records...
  DO ADDLN(.OUT,"<table>")
  DO ADDLN(.OUT,"  <caption>Old Scanned Records to View</caption>")
  DO ADDLN(.OUT,"  <tr>")
  DO ADDLN(.OUT,"    <td class='Heading'>File name</td>")
  DO ADDLN(.OUT,"    <td class='Heading'>Date Last Modified</td>")
  DO ADDLN(.OUT,"  </tr")
  NEW ROWCT SET ROWCT=0
  NEW DT SET DT=9999999
  FOR  SET DT=$ORDER(ARR(DT),-1) QUIT:+DT'>0  DO
  . NEW MODSTR SET MODSTR=$$FMTE^XLFDT(DT,"1D")
  . NEW INFO SET INFO=""
  . FOR  SET INFO=$ORDER(ARR(DT,INFO)) QUIT:INFO=""  DO
  . . NEW URLPATH SET URLPATH=$PIECE(INFO,"^",1)
  . . NEW FNAME SET FNAME=$PIECE(INFO,"^",2)
  . . NEW SAFEFNAME SET SAFEFNAME=FNAME
  . . ;"IF SAFEFNAME[" " SET SAFEFNAME=$$REPLSTR^TMGSTUT3(SAFEFNAME," ","\ ")
  . . NEW LSTR SET LSTR="<a href="""_URLPATH_SAFEFNAME_""" target=""_blank"">"_FNAME_"</a>"
  . . NEW RSTR SET RSTR=MODSTR
  . . DO ADDROW(.OUT,LSTR,RSTR,ROWCT#2,"  ")    
  . . SET ROWCT=ROWCT+1
  DO ADDLN(.OUT,"</table>")
  QUIT;
  ;
GETFTR(OUT)
  DO ADDLN(.OUT,"</body>")
  DO ADDLN(.OUT,"</html>")
  QUIT                    
  ;
PREPRCLK(OUTARR,DFN)  ;"Prep listing of record links 
  ;"Input: OUTARR -- AN OUT PARAMETER.  PASS BY REFERENCE.  Format:
  ;"            OUTARR(FMDT,URLPATH^FILENAME)=""
  ;"       DFN -- PATIENT IEN
  ;"       DIR -- DIRECTORY TO CHECK
  ;"RESULTS: NONE
  NEW PATHS DO PATH4DFN(.PATHS,DFN)   ;"Get one or more filepaths for patient name, including aliases
  NEW APATH SET APATH=""
  FOR  SET APATH=$ORDER(PATHS(APATH)) QUIT:APATH=""  DO
  . NEW HFSPATH SET HFSPATH=$$HFSROOT()_APATH
  . NEW URLPATH SET URLPATH="/filesystem/oldrecs/"_APATH  
  . NEW TMGLIST
  . IF $$ISDIR^TMGKERNL(HFSPATH)=0 DO  QUIT
  . . DO ADDLN(.OUT,"FYI, path: "_HFSPATH_" is not valid<p>")  
  . NEW TMGFILTER SET TMGFILTER("*")=""
  . NEW TMP SET TMP=$$LIST^%ZISH(HFSPATH,"TMGFILTER","TMGLIST")
  . IF TMP'=1 DO  QUIT
  . . DO ADDLN(.OUT,"<P><B>Error reading path: "_HFSPATH)
  . NEW AFILE SET AFILE=""
  . FOR  SET AFILE=$ORDER(TMGLIST(AFILE)) QUIT:AFILE=""  DO
  . . IF $$UP^XLFSTR(AFILE)="THUMBS.DB" QUIT
  . . NEW MODDT SET MODDT=$$FLASTMOD(HFSPATH_AFILE)  ;"Returns FMDATE, or 0 if problem.
  . . SET OUTARR(+MODDT,URLPATH_"^"_AFILE)=""
  QUIT
  ;
ADDROW(OUT,DATA1,DATA2,ISROWEVEN,INDENT)  ;
  SET INDENT=$GET(INDENT)
  NEW CLASS SET CLASS="Row"
  IF +$GET(ISROWEVEN)=1 SET CLASS=CLASS_" Even"
  ELSE  SET CLASS=CLASS_" Odd"
  DO ADDLN(.OUT,INDENT_"<tr class='"_CLASS_"'>")
  DO ADDLN(.OUT,INDENT_"  <td class='LCell'>")
  DO ADDLN(.OUT,INDENT_"    "_DATA1)
  DO ADDLN(.OUT,INDENT_"  </td>")
  DO ADDLN(.OUT,INDENT_"  <td class='RCell'>")
  DO ADDLN(.OUT,INDENT_"    "_DATA2)
  DO ADDLN(.OUT,INDENT_"  </td>")
  DO ADDLN(.OUT,INDENT_"</tr>")
  QUIT
  ;
PATH4DFN(OUT,DFN,DIR)   ;"Get one or more filepaths for patient name, including aliases
  ;"Input: OUT -- PASS BY REFERENCE, an OUT PARAMETER.  Format:
  ;"        OUT(<full filepath>)=""
  ;"       DFN -- PATIENT IEN
  NEW FMDOB SET FMDOB=$PIECE($GET(^DPT(DFN,0)),"^",3)\1
  NEW NAMES DO GETNAMES(.NAMES,DFN)
  NEW ANAME SET ANAME=""
  FOR  SET ANAME=$ORDER(NAMES(ANAME)) QUIT:ANAME=""  DO
  . NEW FNAME,LNAME DO PREPNAME(ANAME,.LNAME,.FNAME)
  . DO PATH4NAM(.OUT,LNAME,FNAME,FMDOB,DIR)  ;"Get one or more filepaths for patient name  
  QUIT
  ;
GETNAMES(OUT,DFN) ;"Get prepped names, including aliases
  ;"Input: OUT -- AN OUT PARAMETER.  Format:
  ;"         OUT(FullPatientName)=""  <-- not prepped. 
  NEW NAME SET NAME=$PIECE($GET(^DPT(DFN,0)),"^",1)
  SET OUT(NAME)=""
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(^DPT(DFN,.01,IDX)) QUIT:+IDX'>0  DO
  . NEW ALIAS SET ALIAS=$PIECE($GET(^DPT(DFN,.01,IDX,0)),"^",1)
  . SET OUT(ALIAS)=""
  QUIT
  ;
PREPNAME(NAME,OUTLNAME,OUTFNAME) ;"Format names into desired format
  SET NAME=$PIECE(NAME," ",1)
  NEW FNAME SET FNAME=$PIECE(NAME,",",2)
  NEW LNAME SET LNAME=$PIECE(NAME,",",1)
  NEW LASTINIT SET LASTINIT=$$UP^XLFSTR($EXTRACT(LNAME,1))
  NEW FIRSTINIT SET FIRSTINIT=$$UP^XLFSTR($EXTRACT(FNAME,1))
  SET OUTLNAME=LASTINIT_$EXTRACT($$LOW^XLFSTR(LNAME),2,$LENGTH(LNAME))  
  SET OUTFNAME=FIRSTINIT_$EXTRACT($$LOW^XLFSTR(FNAME),2,$LENGTH(FNAME))
  QUIT
  ;
PATH4NAM(OUT,LNAME,FNAME,FMDOB,DIR)  ;"Get one or more filepaths for patient name
  ;"Input: OUT -- PASS BY REFERENCE, an OUT PARAMETER.  Format:
  ;"        OUT(<PARTIAL filepath>)=""          
  ;"    LNAME -- last name -- Should be in Camelcase
  ;"    FNAME -- first name -- Should be in Camelcase
  ;"    FMDOB -- DOB in FM format.  Should not include any time values (should be date only)
  NEW LASTINIT SET LASTINIT=$$UP^XLFSTR($EXTRACT(LNAME,1))
  NEW RELPATH SET RELPATH=$$GRPNAME(LNAME)_LASTINIT_"/"  
  NEW HFSPATH SET HFSPATH=$$HFSROOT(DIR)_RELPATH  
  NEW TMGFILTER SET TMGFILTER(LNAME_","_FNAME_"*")=""
  NEW TMGLIST,TMP
  SET TMP=$$LIST^%ZISH(HFSPATH,"TMGFILTER","TMGLIST")
  IF TMP'=1 QUIT
  NEW ADIR SET ADIR=""
  FOR  SET ADIR=$ORDER(TMGLIST(ADIR)) QUIT:ADIR=""  DO
  . NEW X,Y SET X=$PIECE(ADIR,"_",2) DO ^%DT
  . IF Y'=FMDOB QUIT
  . SET OUT(RELPATH_ADIR_"/")=""  
  QUIT
  ;
GRPNAME(LNAME) ;"Get group directory name
  NEW LASTINIT SET LASTINIT=$$UP^XLFSTR($EXTRACT(LNAME,1))
  NEW GROUP SET GROUP=""
  IF "ABCD"[LASTINIT SET GROUP="A-D"
  ELSE  IF "EFGH"[LASTINIT SET GROUP="E-H"
  ELSE  IF "IJKL"[LASTINIT SET GROUP="I-L"
  ELSE  IF "MNOP"[LASTINIT SET GROUP="M-P"
  ELSE  IF "QRST"[LASTINIT SET GROUP="Q-T"
  ELSE  IF "UVWXYZ"[LASTINIT SET GROUP="U-Z"
  IF GROUP'="" SET GROUP=GROUP_"/"            
  QUIT GROUP
  ;
FLASTMOD(FPNAME)  ;"Get date of last modification of file.   
  ;"Returns FMDATE, or 0 if problem.
  NEW RESULT SET RESULT=0
  NEW ARR DO FSTAT^TMGKERNL(.ARR,FPNAME,"--printf=%y")
  NEW LDT SET LDT=$PIECE(ARR(1),".",1) GOTO:LDT="" LMDDN
  NEW TIME SET TIME=$TRANSLATE($PIECE(LDT," ",2),":","")
  NEW X,Y SET X=$PIECE(LDT," ",1) DO ^%DT
  IF Y'>0 GOTO LMDDN
  SET RESULT=Y_"."_TIME
LMDDN ;
  QUIT RESULT
  ;
ADDLN(OUT,TXT,ADDBR)  ;" Add one line to html document being built
  NEW IDX SET IDX=+$ORDER(OUT(""),-1)+1
  SET OUT(IDX)=$GET(TXT)
  IF $GET(ADDBR) SET OUT(IDX)=OUT(IDX)_"<BR>"
  SET OUT(IDX)=OUT(IDX)_$C(13,10)
  QUIT
  ;
GETSTYLE(OUT)  ;" Reads in style data
  DO ADDLN(.OUT,"  <style type=""text/css"">")
  NEW IDX,DONE SET DONE=0
  FOR IDX=1:1 QUIT:DONE  DO
  . NEW LINE SET LINE=$PIECE($TEXT(STYLE+IDX),";;",2)
  . IF LINE["[^^^^]" SET DONE=1 QUIT
  . IF $EXTRACT($$TRIM^XLFSTR(LINE),1,2)="//" QUIT
  . DO ADDLN(.OUT,"    "_LINE)
  DO ADDLN(.OUT,"  </style>")
  QUIT
  ;                                                                  
STYLE ;" html code for styles.
 ;; .Heading {
 ;;     display: table-row;
 ;;     background-color: #FFFF94;  //light yellow
 ;;     font-weight: bold;
 ;;     text-align: left;
 ;;     font-size: 1.1em; 
 ;;     font-weight: bold; 
 ;;     border: solid;                           
 ;;     border-width: thin;                          
 ;; }
 ;; table {
 ;;    width: 90%;
 ;;    border: solid;
 ;;    border-width: thin;
 ;; }
 ;; th {
 ;;    border: solid;
 ;;    border-width: thin;
 ;;     text-align : left
 ;;     background-color: red;
 ;; }
 ;; .LCell:hover {
 ;;     background-color: #FFFF94;  //light yellow
 ;; }
 ;; caption {
 ;;     background-color: #B2F0FF;  //light blue
 ;;     font-size: 1.75em; 
 ;;     font-weight: bold; 
 ;; }
 ;; .Even {
 ;;     background-color: #B2F0FF;  //light blue
 ;; }
 ;; .Odd {
 ;;     background-color: white;
 ;; }
 ;; .LCell {
 ;;     display: table-cell;
 ;;     border-width: thin;
 ;;     padding-left: 10px;
 ;;     padding-right: 10px;
 ;;     width: 30%;
 ;;     text-align: left;
 ;; }
 ;; .RCell {
 ;;     display: table-cell;
 ;;     padding-left: 10px;
 ;;     padding-right: 10px;
 ;;     width: 30%;
 ;;     text-align: left;
 ;; }
 ;; [^^^^]
