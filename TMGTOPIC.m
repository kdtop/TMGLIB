TMGTOPIC ;ELH - Topic Routines; 4/30/2014
	;;1.0;CONSULT/REQUEST TRACKING;2/18/14;Build 7
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
TOPICRPT(ROOT,TMGDFN,ID,ALPHA,OMEGA,DTRANGE,REMOTE,MAX,ORFHIE)  ;"TOPIC REPORT
  ;"Purpose: Entry point, as called from CPRS REPORT system
  ;"Input: ROOT -- Pass by NAME.  This is where output goes
  ;"       TMGDFN -- Patient DFN ; ICN for foriegn sites
  ;"       ID --
  ;"       ALPHA -- Start date (lieu of DTRANGE)
  ;"       OMEGA -- End date (lieu of DTRANGE)
  ;"       DTRANGE -- # days back from today
  ;"       REMOTE --
  ;"       MAX    --
  ;"       ORFHIE -D- 
  goto T1
  SET @ROOT@(1)="<HTML><HEAD><TITLE>TOPIC THREAD REPORT</TITLE></HEAD><BODY>"
  NEW TOPIC SET TOPIC=""
  NEW TOPICARR
  FOR  SET TOPIC=$ORDER(^TMG(22719.2,TMGDFN,1,"B",TOPIC)) QUIT:TOPIC=""  DO
  . NEW TOPICIDX SET TOPICIDX=0
  . FOR  SET TOPICIDX=$ORDER(^TMG(22719.2,TMGDFN,1,"B",TOPIC,TOPICIDX)) QUIT:TOPICIDX'>0  DO
  . . SET TOPICARR($$UP^XLFSTR(TOPIC),TOPICIDX)=""
  ;"
  SET TOPIC=""
  NEW ROOTIDX SET ROOTIDX=1
  SET @ROOT@($I(ROOTIDX))="<TABLE BORDER=3><TR><TH>TOPIC</TH><TH>THREADS</TH></TR>"
  FOR  SET TOPIC=$ORDER(TOPICARR(TOPIC)) QUIT:TOPIC=""  DO
  . SET @ROOT@($I(ROOTIDX))="<TR><TD>"_TOPIC_"</TD><TD><ul>"
  . NEW TOPIDX SET TOPIDX=0
  . FOR  SET TOPIDX=$ORDER(TOPICARR(TOPIC,TOPIDX)) QUIT:TOPIDX'>0  DO
  . . ;"SET @ROOT@($I(ROOTIDX))="threads go here"
  . . NEW THREADIDX SET THREADIDX=0
  . . FOR  SET THREADIDX=$ORDER(^TMG(22719.2,TMGDFN,1,TOPIDX,1,THREADIDX)) QUIT:THREADIDX'>0  DO
  . . . SET @ROOT@($I(ROOTIDX))="<li>"
  . . . SET @ROOT@($I(ROOTIDX))="<B>"_$$EXTDATE^TMGDATE($GET(^TMG(22719.2,TMGDFN,1,TOPIDX,1,THREADIDX,0)),1)_"</B>"
  . . . NEW LINEIDX SET LINEIDX=0                      
  . . . FOR  SET LINEIDX=$ORDER(^TMG(22719.2,TMGDFN,1,TOPIDX,1,THREADIDX,1,LINEIDX)) QUIT:LINEIDX'>0  DO
  . . . . SET @ROOT@($I(ROOTIDX))=$GET(^TMG(22719.2,TMGDFN,1,TOPIDX,1,THREADIDX,1,LINEIDX,0))
  . . . SET @ROOT@($I(ROOTIDX))="<hr></li>"
  . SET @ROOT@($I(ROOTIDX))="</ul></td></tr>"
  QUIT
  ;
  ;
T1  ;"TEST1
  NEW ADFN SET ADFN=TMGDFN
  NEW DATA DO TOPIC2DATA(ADFN,.DATA)
  NEW INFO DO PREPINFO(.DATA,.INFO)
  NEW HTMDOC DO HTMT2DOC^TMGHTM3("HTMDOC","TOPICRPT","TMGHTMS1",.INFO)
  ;"ZWR HTMDOC
  MERGE @ROOT=HTMDOC
  QUIT
  ;
TOPIC2DATA(TMGDFN,DATA)  ;"Prepare working data array of topics
  ;"INPUT: TMGDFN -- patient IEN
  ;"        DATA -- PASS BY REFERENCE.  SEE OUTPUT
  ;"OUTPUT: DATA filled as follows:
  ;"          DATA("TOPIC",<TOPIC_NAME>,<FMDT>,#)=<line of text>
  ;
  NEW TOPIC SET TOPIC=""
  FOR  SET TOPIC=$ORDER(^TMG(22719.2,TMGDFN,1,"B",TOPIC)) QUIT:TOPIC=""  DO
  . NEW TOPICIEN SET TOPICIEN=0
  . FOR  SET TOPICIEN=$ORDER(^TMG(22719.2,TMGDFN,1,"B",TOPIC,TOPICIEN)) QUIT:TOPICIEN'>0  DO
  . . NEW FULLTOPICNAME SET FULLTOPICNAME=$PIECE($GET(^TMG(22719.2,TMGDFN,1,TOPICIEN,0)),"^",1)
  . . NEW ADT SET ADT=0
  . . FOR  SET ADT=$ORDER(^TMG(22719.2,TMGDFN,1,TOPICIEN,1,"B",ADT)) QUIT:ADT'>0  DO
  . . . NEW DTSUBIEN SET DTSUBIEN=0
  . . . FOR  SET DTSUBIEN=$ORDER(^TMG(22719.2,TMGDFN,1,TOPICIEN,1,"B",ADT,DTSUBIEN)) QUIT:DTSUBIEN'>0  DO
  . . . . NEW IDX,ODX SET (IDX,ODX)=0
  . . . . FOR  SET IDX=$ORDER(^TMG(22719.2,TMGDFN,1,TOPICIEN,1,DTSUBIEN,1,IDX)) QUIT:IDX'>0  DO
  . . . . . NEW LINE SET LINE=$GET(^TMG(22719.2,TMGDFN,1,TOPICIEN,1,DTSUBIEN,1,IDX,0))
  . . . . . SET DATA("TOPIC",FULLTOPICNAME,ADT,$INCR(ODX))=LINE
  QUIT
  ;
PREPINFO(DATA,INFO) ;"Take patient data and prepare INFO for insertion into template
  ;"NOTE: This is designed to work with TOPICRPT^TMGHTMS1 as template, and
  ;"      will use HTM2DOC^TMGHTM3 to merge the two. 
  ;"INPUT: DATA -- PASS BY REFERENCE.  Format:
  ;"                  DATA("TOPIC",<TOPIC_NAME>,<FMDT>,#)=<line of text>
  ;"       INFO -- PASS BY REFERENCE.  Format:
  ;"                  INFO("DATA",<block_name>,#)=line of HTML-valid text to put into template
  ;"Result: None.
  NEW ODX SET ODX=0
  ;
  ;"First, create Table of Contents (TOC) block
  SET INFO("DATA","TOC",$INCR(ODX))="<h3>Topics</h3>"
  SET INFO("DATA","TOC",$INCR(ODX))="<ul>"
  NEW ATOPIC SET ATOPIC=""
  NEW TOCIDX SET TOCIDX=0
  FOR  SET ATOPIC=$ORDER(DATA("TOPIC",ATOPIC)) QUIT:ATOPIC=""  DO
  . SET TOCIDX=TOCIDX+1
  . SET INFO("DATA","TOC",$INCR(ODX))="  <li><a href=""#"_TOCIDX_""" onclick=""navigateTo(event, '"_TOCIDX_"')"">"_ATOPIC_"</a></li>"
  SET INFO("DATA","TOC",$INCR(ODX))="</ul>"
  ;
  ;"Next, create TABLE block
  SET INFO("DATA","TABLE",$INCR(ODX))="<table>"
  SET ATOPIC="",TOCIDX=0
  FOR  SET ATOPIC=$ORDER(DATA("TOPIC",ATOPIC)) QUIT:ATOPIC=""  DO
  . SET TOCIDX=TOCIDX+1
  . SET INFO("DATA","TABLE",$INCR(ODX))="  <tr>"
  . SET INFO("DATA","TABLE",$INCR(ODX))="    <th id="""_TOCIDX_""">"_ATOPIC_"</th>"
  . SET INFO("DATA","TABLE",$INCR(ODX))="  </tr>"
  . SET INFO("DATA","TABLE",$INCR(ODX))="  <tr>"
  . SET INFO("DATA","TABLE",$INCR(ODX))="    <td>"
  . NEW NUMDTS SET NUMDTS=$$LISTCT^TMGMISC2($NAME(DATA("TOPIC",ATOPIC)))
  . NEW BULLETS SET BULLETS=(NUMDTS>1)
  . NEW TEXT SET TEXT=""
  . SET INFO("DATA","TABLE",$INCR(ODX))="    <ul>"
  . NEW ADT SET ADT=0
  . FOR  SET ADT=$ORDER(DATA("TOPIC",ATOPIC,ADT)) QUIT:ADT'>0  DO
  . . IF BULLETS SET TEXT=TEXT_"      <li>"
  . . NEW EDT SET EDT=$$FMTE^XLFDT(ADT,"2D")
  . . SET TEXT=TEXT_"<B>"_EDT_":</B> "
  . . NEW IDX SET IDX=0
  . . FOR  SET IDX=$ORDER(DATA("TOPIC",ATOPIC,ADT,IDX)) QUIT:IDX'>0  DO
  . . . NEW LINE SET LINE=$GET(DATA("TOPIC",ATOPIC,ADT,IDX))
  . . . SET TEXT=TEXT_LINE
  . . . SET INFO("DATA","TABLE",$INCR(ODX))=TEXT,TEXT=""
  . SET INFO("DATA","TABLE",$INCR(ODX))="      </ul>"
  . SET INFO("DATA","TABLE",$INCR(ODX))="    </td>"
  . SET INFO("DATA","TABLE",$INCR(ODX))="  </tr>"
  ;
  SET INFO("DATA","TABLE",$INCR(ODX))="</table>"
  QUIT
  ;
TEST
  ZLINK "TMGTEST"
  DO GETCODE(ROOT,"HTMLTEST1","TMGTEST")
  QUIT
  ;
GETCODE(OUTREF,TAG,ROUTINE) ;
  NEW OFFSET
  NEW IDX SET IDX=1
  NEW DONE SET DONE=0
  FOR OFFSET=1:1 DO  QUIT:DONE
  . NEW LINE SET LINE=$TEXT(@TAG+OFFSET^@ROUTINE)
  . SET LINE=$PIECE(LINE,";;",2)
  . IF LINE["DONE_WITH_HTML" SET DONE=1 QUIT
  . SET @OUTREF@($I(IDX))=LINE
  QUIT
  ;  
  
