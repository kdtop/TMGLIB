TMGRST02 ;TMG/kst/REST web service; 3/3/15
       ;;1.0;TMG-LIB;**1**;3/3/15
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
  ;"=======================================================================
  ;" API -- Public Functions.
  ;"=======================================================================
  ;
  ;"=======================================================================
  ;"PRIVATE API FUNCTIONS
  ;"=======================================================================
  ;
  ;"=======================================================================
  ;"DEPENDENCIES
  ;"=======================================================================
  ;"Uses:  TMGRST*
  ;"=======================================================================
  ;       
R(RESULT,ARGS) ; GET Mumps Routine   ;"Modified from R^%W0
  ;"Input: RESULT -- PASSED BY REFRENCE. AN OUT PARAMETER.  Format:
  ;"         RESULT("mime")=mime type
  ;"         RESULT(#)=<line of result>
  ;"       ARGS 
  ;"NOTE: uses HTTPREQ in global scope.
  NEW TMGRST2 SET TMGRST2=0
  IF TMGRST2=1 DO
  . KILL ARGS,HTTPREQ
  . MERGE ARGS=^TMG("TMP","TMGRST02","ARGS")
  . MERGE HTTPREQ=^TMG("TMP","TMGRST02","HTTPREQ")
  ELSE  DO
  . KILL ^TMG("TMP","TMGRST02")
  . MERGE ^TMG("TMP","TMGRST02","ARGS")=ARGS
  . MERGE ^TMG("TMP","TMGRST02","HTTPREQ")=HTTPREQ
  ;
  NEW TMGINDENTLEN SET TMGINDENTLEN=2  ;"Will be used in global scope by event handlers below
  NEW PATH SET PATH=$PIECE($GET(HTTPREQ("path")),"/r2/",2)
  NEW OUT 
  KILL RESULT SET RESULT=$NAME(^TMP($J))
  KILL @RESULT
  ;
  IF $EXTRACT(PATH,1,9)="[LINK-IN]" DO  GOTO R2L1
  . SET PATH=$PIECE(PATH,"[LINK-IN]",2)
  . DO LNKINPG(.OUT,$PIECE(PATH,";",1),$PIECE(PATH,";",2))
  ;
  IF PATH["/" DO  GOTO R2DN  ;"pass off to FILESYSTEM handling.   
  . KILL ARGS SET ARGS("*")=PATH
  . DO FILESYS^%W0(.RESULT,.ARGS)
  ;
  NEW RTN,FSRTN SET RTN=$G(ARGS("routine"))
  IF RTN="" SET RTN=PATH
  SET RTN=$$URLDEC^VPRJRUT(RTN)  ;"//kt 8/13/17
  SET FSRTN=RTN IF $EXTRACT(FSRTN,1)="%" SET FSRTN=$TRANSLATE(FSRTN,"%","_")
  IF '(RTN]""&($TEXT(^@RTN)]"")) DO  GOTO R2DN
  . DO SETERROR^VPRJRUT(404,"Routine not found")
  NEW TMGRSTRTN SET TMGRSTRTN=RTN  ;"will be used in global scope below
  ;
  NEW TMGRSTBLK,TMGRSTCODE
  NEW OPTIONS,STYLE DO SUOPTS(.OPTIONS,.STYLE)
  DO PARSEPOS^TMGCOD01(,,RTN,"TMGRSTBLK",3)
  DO ASSEMBLE^TMGCOD02("TMGRSTBLK","","TMGRSTCODE",,.OPTIONS)
  DO GETHDR(.OUT,.STYLE,RTN)
  DO GETBODY(.OUT,.TMGRSTCODE) 
  DO GETFTR(.OUT)
R2L1 ;
  SET RESULT("mime")="text/html; charset=utf-8"
  MERGE @RESULT=OUT
  ;
R2DN ; 
  KILL ^TMG("TMP","TMGRST02","RESULT")
  KILL ^TMG("TMP","TMGRST02","@RESULT")
  MERGE ^TMG("TMP","TMGRST02","RESULT")=RESULT
  MERGE ^TMG("TMP","TMGRST02","@RESULT")=@RESULT
  QUIT
  ;
SUOPTS(OPTIONS,STYLE)  ;
  NEW IDX,DONE SET DONE=0
  FOR IDX=1:1 QUIT:DONE  DO
  . NEW LINE SET LINE=$PIECE($TEXT(TYPES+IDX),";;",2)
  . IF LINE["<DONE>" SET DONE=1 QUIT
  . NEW PART SET PART=$$TRIM^XLFSTR($PIECE(LINE,";",1))
  . NEW HNDLFN SET HNDLFN=""
  . IF PART["," DO
  . . SET HNDLFN=$$TRIM^XLFSTR($PIECE(PART,",",2))
  . . SET PART=$$TRIM^XLFSTR($PIECE(PART,",",1))
  . NEW TAG,TYPE SET (TAG,TYPE)="?"
  . IF PART["=" DO
  . . SET TYPE=$PIECE(PART,"=",1)
  . . SET TAG=$$TRIM^XLFSTR($PIECE(PART,"=",2))
  . ELSE  DO
  . . SET (TAG,TYPE)=PART
  . SET OPTIONS(TYPE,"PRE")="<"_TAG_">"
  . SET OPTIONS(TYPE,"POST")="</"_TAG_">"
  . IF HNDLFN'="" SET OPTIONS(TYPE,"FN")=HNDLFN
  . NEW STYLN SET STYLN=$PIECE(LINE,";",2,999)
  . NEW JDX FOR JDX=1:1:$LENGTH(STYLN,";") DO
  . . NEW ASTYLE SET ASTYLE=""
  . . SET ASTYLE=$$TRIM^XLFSTR($PIECE(STYLN,";",JDX)) QUIT:ASTYLE=""
  . . SET STYLE(TAG,ASTYLE)=""
  SET OPTIONS("XPND")=1
  ;"TODO -- add a function handler for type string and comment, to 
  ;"    encode special characters
  QUIT
  ;
TYPES ;
  ;;MUMPS; FONT-FAMILY: "Lucida Console", Monaco, monospace; white-space:nowrap
  ;;$$FN=EXTFN,$$HNDFPLNK^TMGRST02; color: #3366FF
  ;;$FN=INTFN; color: #000099
  ;;$SV=SPVAR; color: #FF9900
  ;;PROC=MPROC,$$HNDFPLNK^TMGRST02; color: #0099FF
  ;;POST COND=PC
  ;;NUM; color: #B20000
  ;;VAR=VARLABEL,$$HNDVAR^TMGRST02; color: #4d704d
  ;;COMP/ASSIGN=EQ
  ;;BOOL
  ;;:=COLON
  ;;@=INDER
  ;;CONCAT=CONCAT
  ;;STRING=STR,$$HNDSTR^TMGRST02; color: #990099;
  ;;MATH
  ;;DOT
  ;;OTHER
  ;;CMD; color: black;
  ;;,=COMMA
  ;;COMMENT,$$HNDSTR^TMGRST02; color: #993333
  ;;TAG,$$HNDTAG^TMGRST02; background-color: #ffff99;
  ;;INDENT,$$HNDINDT^TMGRST02;;
  ;;FULLLINE,$$HNDFULLN^TMGRST02  
  ;;ROUTINE=RTN
  ;;PARENS; color: #1f7a1f
  ;;CARET
  ;;LINEFEED=LF
  ;;TAB
  ;;GLOBAL; color: #FF3399  
  ;;<DONE>
  ; 
GETHDR(OUT,STYLE,RTN)  ;
  DO ADDLN(.OUT,"<!DOCTYPE html>")
  DO ADDLN(.OUT,"<html>")
  DO ADDLN(.OUT,"<head>")
  DO ADDLN(.OUT,"  <title>"_RTN_" routine</title>")
  DO GETSTYLE(.OUT,.STYLE)
  DO ADDLN(.OUT,"</head>")
  DO ADDLN(.OUT,"<body>")
  QUIT
  ;
GETSTYLE(OUT,STYLE)  ;
  IF '$DATA(STYLE) QUIT
  DO ADDLN(.OUT,"<style>")
  NEW TAG SET TAG=""
  FOR  SET TAG=$ORDER(STYLE(TAG)) QUIT:TAG=""  DO
  . DO ADDLN(.OUT,TAG_" {")
  . NEW ASTYLE SET ASTYLE=""
  . FOR  SET ASTYLE=$ORDER(STYLE(TAG,ASTYLE)) QUIT:ASTYLE=""  DO
  . . DO ADDLN(.OUT,"    "_ASTYLE_";")
  . DO ADDLN(.OUT,"}")
  DO ADDLN(.OUT,"</style>")
  QUIT
  ;
GETBODY(OUT,IN) ;
  DO ADDLN(.OUT,"<MUMPS>")
  DO ADDLN(.OUT,"  <h1>ROUTINE: "_RTN_"</h1>")
  ;"DO ADDLN(.OUT,"  <p>This is a paragraph.</p>")
  ;"DO ADDLN(.OUT,"  <p>GOT: "_RTN_".</p>")
  NEW IDX SET IDX=""
  FOR  SET IDX=$ORDER(IN(IDX)) QUIT:+IDX'=IDX  DO
  . NEW LINE SET LINE=$GET(IN(IDX))
  . SET LINE=$$REPLSTR^TMGSTUT3(LINE,"  ","&nbsp &nbsp") 
  . DO ADDLN(.OUT,LINE,1)
  DO ADDLN(.OUT,"</MUMPS>")
  QUIT
  ;
GETFTR(OUT)
  DO ADDLN(.OUT,"</body>")
  DO ADDLN(.OUT,"</html>")
  QUIT 
  ;
ADDLN(OUT,TXT,ADDBR)  ;
  NEW IDX SET IDX=+$ORDER(OUT(""),-1)+1
  SET OUT(IDX)=$GET(TXT)
  IF $GET(ADDBR) SET OUT(IDX)=OUT(IDX)_"<BR>"
  SET OUT(IDX)=OUT(IDX)_$C(13,10)
  QUIT
  ;
HNDFPLNK(VALUE,TYPE,BLK,OPTIONS) ;"Handle tagging of procedures
  ;"
  ;"<a href=""http://www.cnn.com"">"_VALUE_"</a>"
  NEW RESULT SET RESULT=VALUE
  NEW TAG SET TAG=$TRANSLATE($PIECE(VALUE,"^",1),"$","")
  IF TAG'="" SET TAG="#"_TAG
  NEW ROUTINE SET ROUTINE=$PIECE(VALUE,"^",2)
  SET RESULT="<a href="""_ROUTINE_TAG_""">"_VALUE_"</a>"
  QUIT RESULT
  ;
HNDTAG(VALUE,TYPE,BLK,OPTIONS) ;"Handle tagging of MUMPS code labels/tags
  ;"NOTE: uses TMGRSTRTN in global scope, defined above
  SET TYPE=$GET(TYPE)
  NEW PRE SET PRE=$GET(OPTIONS(TYPE,"PRE"))
  NEW POST SET POST=$GET(OPTIONS(TYPE,"POST"))
  SET VALUE=$TRANSLATE($GET(VALUE),"$","")  
  ;"SET VALUE=$$UP^XLFSTR(VALUE)  ;"force tags to be uppercase.  
  NEW PREA SET PREA=$EXTRACT(PRE,1,$LENGTH(PRE)-1)
  NEW PREB SET PREB=$EXTRACT(PRE,$LENGTH(PRE))
  SET PRE=PREA_" id="""_VALUE_""" "_PREB
  NEW RESULT SET RESULT=PRE_VALUE_POST
  NEW LINKS DO GTLNKS(.LINKS,TMGRSTRTN,VALUE)
  NEW IDX,CT SET (IDX,CT)=0 
  FOR  SET IDX=$ORDER(LINKS(IDX)) QUIT:+IDX'>0  SET CT=CT+1
  IF CT>0 SET RESULT=RESULT_"<a href='[LINK-IN]"_TMGRSTRTN_";"_VALUE_"'>&#x1f517;</a>"
  QUIT RESULT
  ;
HNDSTR(VALUE,TYPE,BLK,OPTIONS) ;"Handle tagging of STRINGS
  SET TYPE=$GET(TYPE)
  NEW PRE SET PRE=$GET(OPTIONS(TYPE,"PRE"))
  NEW POST SET POST=$GET(OPTIONS(TYPE,"POST"))
  SET VALUE=$$SYMENC^MXMLUTL($GET(VALUE))
  NEW RESULT SET RESULT=PRE_VALUE_POST
  QUIT RESULT
  ;  
HNDINDT(VALUE,TYPE,BLK,OPTIONS) ;"Handle tagging of MUMPS code indentation space
  ;"Input: VALUE is 2 parameters: <indent string>^<TAG>
  NEW TAG SET TAG=$PIECE(VALUE,"^",2)
  SET VALUE=$PIECE(VALUE,"^",1)
  NEW L SET L=$LENGTH(VALUE) IF L>0 SET TMGINDENTLEN=L
  SET TYPE=$GET(TYPE)
  NEW PRE SET PRE=$GET(OPTIONS(TYPE,"PRE"))
  NEW POST SET POST=$GET(OPTIONS(TYPE,"POST"))
  NEW PREA SET PREA=$EXTRACT(PRE,1,$LENGTH(PRE)-1)
  NEW PREB SET PREB=$EXTRACT(PRE,$LENGTH(PRE))
  NEW TEMP SET TEMP=""
  NEW IDX FOR IDX=1:1:$LENGTH(VALUE) DO
  . NEW CH SET CH=$EXTRACT(VALUE,IDX)
  . IF CH=$CHAR(9) DO   ;"CONVERT TAB INTO 9 SPACES (-TAG LENGTH)
  . . SET CH="" NEW JDX FOR JDX=1:1:9-$LENGTH(TAG) SET CH=CH_"&nbsp;"
  . . SET TAG=""  ;"Use only once.
  . ELSE  SET CH="&nbsp;"
  . SET TEMP=TEMP_CH
  NEW RESULT SET RESULT=TEMP
  IF RESULT'="" SET RESULT=PRE_RESULT_POST
  QUIT RESULT
  ;
HNDFULLN(VALUE,NONE,BLK,OPTIONS) ;"Handle processing of full line after assembly
  NEW RESULT SET RESULT=$GET(VALUE)
  IF $$TRIM^XLFSTR(RESULT)="" DO
  . NEW TEMP SET TEMP=+$GET(TMGINDENTLEN)
  . IF TEMP=0 SET TEMP=2
  . NEW SPC,IDX SET SPC="" FOR IDX=1:1:TEMP SET SPC=SPC_" "
  . SET RESULT=$$HNDINDT(SPC,"INDENT",.BLK,.OPTIONS)
  . SET RESULT=RESULT_$$HNDSTR(";","COMMENT",.BLK,.OPTIONS)
  NEW ORIG SET ORIG=$GET(BLK("@ORIG"))
  NEW TAG SET TAG=$GET(BLK("TAG"))
  NEW COMMENT SET COMMENT=$GET(BLK("COMMENT"))
  IF (TAG'="")&(COMMENT="") DO
  . SET RESULT=RESULT_$$HNDSTR(" ; ","COMMENT",.BLK,.OPTIONS)
  FOR  QUIT:$EXTRACT(RESULT,$LENGTH(RESULT))'=" "  DO   ;"Trim off trailing blank spaces
  . SET RESULT=$EXTRACT(RESULT,1,$LENGTH(RESULT)-1)
  QUIT RESULT
  ;
HNDVAR(VALUE,NONE,BLK,OPTIONS) ;"Handle tagging of variables
  NEW RESULT SET RESULT=$GET(VALUE)
  ;"SET RESULT=$$UP^XLFSTR(VALUE);  "force variables to be upper case.
  NEW PRE SET PRE=$GET(OPTIONS(TYPE,"PRE"))
  NEW POST SET POST=$GET(OPTIONS(TYPE,"POST"))
  IF RESULT'="" SET RESULT=PRE_RESULT_POST
  QUIT RESULT
  ;
  ;"---------------------------------------------------------------------
LNKINPG(OUT,RTN,TAG)  ;"Get page showing links to TAG^ROUTINE
  NEW OPTIONS,STYLE 
  ;"DO SUOPTS(.OPTIONS,.STYLE)
  DO GETHDR(.OUT,.STYLE,RTN)
  NEW LINKS DO SULNKS(.LINKS,RTN,TAG)  
  DO ADDLN(.OUT,"<H1>Links into <b>"_TAG_"&#94;"_RTN_"</b></h1>")
  DO GTLNKBDY(.OUT,.LINKS)  
  DO GETFTR(.OUT)
  QUIT
  ;
SULNKS(OUT,RTN,TAG)  ;"Setup up links for output to page
  NEW LINKS DO GTLNKS(.LINKS,RTN,TAG)
  NEW IEN SET IEN=0 
  FOR  SET IEN=$ORDER(LINKS(IEN)) QUIT:+IEN'>0  DO
  . NEW INRTN SET INRTN=$PIECE($GET(^TMG(22726,IEN,0)),"^",1)
  . NEW STR SET STR="Linked &#x1f517; from routine: <a href='"_INRTN_"'>"_INRTN_"</a> as $$Function() or DO procedure."
  . SET OUT(STR)=""
  KILL LINKS DO GGTLNKS(.LINKS,RTN,TAG)
  NEW IEN SET IEN=0 
  FOR  SET IEN=$ORDER(LINKS(IEN)) QUIT:+IEN'>0  DO
  . NEW INRTN SET INRTN=$PIECE($GET(^TMG(22726,IEN,0)),"^",1)
  . NEW STR SET STR="Linked &#x1f517; from routine: <a href='"_INRTN_"'>"_INRTN_"</a> as GOTO."
  . SET OUT(STR)=""
  KILL LINKS DO GTXLNKS(.LINKS,RTN,TAG)
  NEW IEN SET IEN=0 
  FOR  SET IEN=$ORDER(LINKS(IEN)) QUIT:+IEN'>0  DO
  . NEW INRTN SET INRTN=$PIECE($GET(^TMG(22726,IEN,0)),"^",1)
  . NEW STR SET STR="Linked &#x1f517; from routine: <a href='"_INRTN_"'>"_INRTN_"</a> as $TEXT() reference."
  . SET OUT(STR)=""
  QUIT
  ;  
GTLNKS(OUT,RTN,TAG) ;"GET LINKS IN FOR TAG^RTN
  MERGE OUT=^TMG(22726,"AFNTP",RTN,TAG)
  QUIT
  ;
GGTLNKS(OUT,RTN,TAG) ;"GET GOTO LINKS IN FOR TAG^RTN
  MERGE OUT=^TMG(22726,"AGTP",RTN,TAG)
  QUIT
  ;
GTXLNKS(OUT,RTN,TAG) ;"GET $TEXT() LINKS IN FOR TAG^RTN
  MERGE OUT=^TMG(22726,"ATXP",RTN,TAG)
  QUIT
  ;
GTLNKBDY(OUT,LINKS)  ;"ADD LINKS TO HTML PAGE
  NEW ALINK SET ALINK=""
  FOR  SET ALINK=$ORDER(LINKS(ALINK)) QUIT:ALINK=""  DO
  . DO ADDLN(.OUT,ALINK_"<P>")
  QUIT
