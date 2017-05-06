TMGHTM1 ;TMG/kst-HTML utilities ;7/1/15, 5/7/16
         ;;1.0;TMG-LIB;**1,17**;08/10/10
 ;
 ;"Utility functions related to documents with HTML formatting
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"---------------------------------------------------------------------------
 ;"PUBLIC FUNCTIONS
 ;"---------------------------------------------------------------------------
 ;"$$ISHTML(IEN8925) -- determine IF the text held in the REPORT TEXT field is HTML markup
 ;"$$ISHTMLAR(ARRAY) -- determine IF Array text is HTML markup
 ;"$$ISHTMREF(REFARRAY,ZN) -- determine IF @REFARRAY text is HTML markup 
 ;"$$HTMSPLIT(STR,LEN) -- Split HTML string in way that doesn't break tags
 ;"HTML2TXT(ARRAY) -- convert HTML --> text formatted array
 ;"TXT2HTML(ARRAY) -- convert text --> HTML formatted array 
 ;"$$SIGPICT(DUZ,DATE) -- Return HTML tag pointing to signiture image, or '' IF none.
 ;"HTML2TXS(LINESTR) -- text a string that is HTML formatted, and strips out tags
 ;"$$HTMLTRIM(STR,FLAG,CHARS) -- Trim from HTML text
 ;"STRIPSCR(IEN8925)  --Strip out any <SCRIPT> .. </SCRIPT> from documents     
 ;"---------------------------------------------------------------------------
 ;"PRIVATE FUNCTIONS
 ;"---------------------------------------------------------------------------
 ;
 ;"---------------------------------------------------------------------------
 ;"DEPENDENCIES:
 ;
 ;"---------------------------------------------------------------------------
 ;
ISHTML(IEN8925) ;
  ;"Purpose: to determine IF the text held in the REPORT TEXT field is HTML markup
  ;"Input: IEN8925 -- record number in file 8925
  ;"Results: 1 IF HTML markup, 0 otherwise.
  ;"Note: This is not a perfect test.  Also, will fail IF tag is not uppercase
  NEW REF SET REF=$NAME(^TIU(8925,IEN8925,"TEXT"))
  NEW RESULT SET RESULT=$$ISHTMREF(REF,1)
  QUIT RESULT
  ;
ISHTMLAR(TMGARRAY) ;
  ;"Purpose: TO determine IF Array text is HTML markup
  ;"Input: ARRAY.  Format:  ARRAY(#)=text 
  ;"Results: 1 IF HTML markup, 0 otherwise.
  ;"Note: This is not a perfect test.  Also, will fail IF tag is not uppercase
  NEW RESULT SET RESULT=$$ISHTMREF("TMGARRAY")
  QUIT RESULT
  ;        
ISHTMREF(REFARRAY,ZN) ;
  ;"Purpose: To determine IF @REFARRAY text is HTML markup
  ;"Input: ARRAY.  Format:  ARRAY(#)=text
  ;"       ZN -- Optional.  Default is 0.  
  ;"              If 0, then @REFARRAY@(#)=text
  ;"              If 1, then @REFARRAY@(#,0)=text
  ;"Results: 1 IF HTML markup, 0 otherwise.
  ;"Note: This is not a perfect test.  Also, will fail IF tag is not uppercase
  NEW RESULT SET RESULT=0
  SET REFARRAY=$GET(REFARRAY) IF REFARRAY="" GOTO ISHRDN
  SET ZN=+$GET(ZN)
  NEW LINE SET LINE=0
  FOR  SET LINE=$ORDER(@REFARRAY@(LINE)) QUIT:(LINE="")!(RESULT=1)  DO
  . NEW LINESTR
  . IF ZN=1 SET LINESTR=$GET(@REFARRAY@(LINE,0))
  . ELSE  SET LINESTR=$GET(@REFARRAY@(LINE))
  . SET LINESTR=$$UP^XLFSTR(LINESTR)        
  . IF (LINESTR["<!DOCTYPE HTML")!(LINESTR["<HTML>") SET RESULT=1 QUIT
  . IF (LINESTR["<P>")!(LINESTR["<B>")!(LINESTR["<I>") SET RESULT=1 QUIT
  . IF (LINESTR["<BR>")!(LINESTR["<HTML>") SET RESULT=1 QUIT
ISHRDN  ;
  QUIT RESULT
  ;
HTML2TXT(ARRAY,LISUB) ;
  ;"Purpose: text a WP array that is HTML formatted, and strip <P>, and
  ;"         return in a format of 1 line per array node.
  ;"         Actually, strips out ALL other tags too
  ;"Input: ARRAY -- PASS BY REFERENCE.  This array will be altered.
  ;"                Expected format ARRAY(#,0)=<Text>
  ;"       LISUB--  OPTIONAL.  If provided, then the <LI> (bullet entry) will
  ;"                be repleaced with LISUB.  DEFAULT IS ""  ;//kt added 8/12/12
  ;"Results: none
  ;"NOTE: This conversion causes loss of HTML tags, so a round trip
  ;"      conversion back to HTML would fail.
  NEW OUTARRAY,OUTI
  SET OUTI=1
  SET LISUB=$GET(LISUB)
  ;
  ;"//kt original --> NEW BLKLNTAGS SET BLKLNTAGS="<P>^</TABLE>^</TR>^<TABLE^<TBODY>^</LI>"
  NEW BLKLNTAGS SET BLKLNTAGS="<P>^</TABLE>^</TR>^<TABLE^<TBODY>^</LI>^<LI>"  ;"//kt 10/12/16
  NEW S2 SET S2=""
  NEW LINE SET LINE=0
  FOR  SET LINE=$ORDER(ARRAY(LINE)) QUIT:(LINE="")  DO
  . NEW LINESTR SET LINESTR=S2_$GET(ARRAY(LINE,0))
  . SET S2=""
  . FOR  DO  QUIT:(LINESTR'["<")
  . . NEW TAG,SORTPOS,P,NEXTTAG,PARTA,PARTB
  . . FOR TAG="<P>","<BR>","<LI>","</LI>","</TABLE>","</TR>","<TABLE","<TBODY>" DO
  . . . SET SORTPOS($FIND(LINESTR,TAG),TAG)=""
  . . SET NEXTTAG="" SET P=+$ORDER(SORTPOS(0)) IF P>0 SET NEXTTAG=$ORDER(SORTPOS(P,""))
  . . IF NEXTTAG'="",NEXTTAG'[">" DO  ;"//handle open tags (tags that might have properties)
  . . . NEW P1 SET P1=$FIND(LINESTR,NEXTTAG)-$LENGTH(NEXTTAG) IF P1'>0 QUIT
  . . . NEW P2 SET P2=+$FIND(LINESTR,">")-1 IF P2'>0 QUIT
  . . . SET NEXTTAG=$EXTRACT(LINESTR,P1,P2)
  . . IF NEXTTAG'="" SET PARTA=$PIECE(LINESTR,NEXTTAG,1),PARTB=$PIECE(LINESTR,NEXTTAG,2,999)
  . . ELSE  SET PARTA=LINESTR,PARTB=""
  . . NEW TAGSUB SET TAGSUB=$SELECT(NEXTTAG="<LI>":LISUB,1:"")
  . . IF NEXTTAG'="" DO  QUIT
  . . . SET OUTARRAY(OUTI,0)=PARTA
  . . . SET OUTI=OUTI+1
  . . . IF BLKLNTAGS[$PIECE(NEXTTAG," ",1) DO
  . . . . SET OUTARRAY(OUTI,0)=""  ;"Add blank line to create paragraph break.
  . . . . SET OUTI=OUTI+1
  . . . SET LINESTR=TAGSUB_PARTB
  . . SET S2=LINESTR,LINESTR=""
  . . QUIT
  . SET S2=S2_LINESTR
  IF S2'="" DO
  . SET OUTARRAY(OUTI,0)=S2
  . SET OUTI=OUTI+1
  ;
  ;"Strip out all tags other than <P>
  NEW LINE SET LINE=0
  FOR  SET LINE=$ORDER(OUTARRAY(LINE)) QUIT:(LINE="")  DO
  . NEW LINESTR SET LINESTR=$GET(OUTARRAY(LINE,0))
  . ;"SET LINESTR=$$REPLSTR^TMGSTUT3(LINESTR,"<VEFA>","{VEFA}")
  . SET LINESTR=$$REPLACE(LINESTR,"<VEFA>","{VEFA}")
  . FOR  QUIT:(LINESTR'["<")!(LINESTR'[">")  DO  ;" aaa<bbb>ccc  or aaa>bbb<ccc
  . . NEW S1,S2,S3
  . . SET S1=$PIECE(LINESTR,"<",1)
  . . IF S1[">" DO  QUIT
  . . . SET LINESTR=$PIECE(LINESTR,">",1)_"}"_$PIECE($PIECE(LINESTR,">",2,999),"<",1)_"{"_$PIECE(LINESTR,"<",2,999)
  . . SET S2=$PIECE($PIECE(LINESTR,"<",2,999),">",1)
  . . SET S3=$PIECE(LINESTR,">",2,999)
  . . SET LINESTR=S1_S3
  . SET OUTARRAY(LINE,0)=LINESTR
  ;
  ;"Convert special characters
  NEW SPEC
  SET SPEC("&nbsp;")=" "
  SET SPEC("&lt;")="<"
  SET SPEC("&gt;")=">"
  SET SPEC("&amp;")="&"
  SET SPEC("&quot;")=""""
  SET SPEC("{VEFA}")="<VEFA>"
  NEW LINE SET LINE=0
  FOR  SET LINE=$ORDER(OUTARRAY(LINE)) QUIT:(LINE="")  DO
  . NEW LINESTR SET LINESTR=$GET(OUTARRAY(LINE,0))
  . SET OUTARRAY(LINE,0)=$$REPLACE^XLFSTR(LINESTR,.SPEC)
  ;
  KILL ARRAY
  MERGE ARRAY=OUTARRAY
  QUIT
  ;
TXT2HTML(ARRAY,ZN) ;"Convert text --> HTML formatted array
  ;"Note: This is not a comprehensive conversion.  Could be improved in future
  ;"Input: ARRAY -- PASS BY REFERENCE.  This array will be altered.  Format:
  ;"                ARRAY(#)=<Text>  <-- IF ZN=0
  ;"                ARRAY(#,0)=<Text>  <-- IF ZN=1
  ;"       ZN -- OPTIONAL.  Default=0.  See ARRAY above for use. 
  ;"Results: none
  ;"Convert special characters
  SET ZN=+$GET(ZN)
  NEW SPEC
  SET SPEC(" ")="&nbsp;"
  SET SPEC("<")="&lt;"
  SET SPEC(">")="&gt;"
  SET SPEC("&")="&amp;"
  SET SPEC("""")="&quot;"
  NEW LINE SET LINE=0
  FOR  SET LINE=$ORDER(ARRAY(LINE)) QUIT:(LINE="")  DO
  . NEW LINESTR 
  . IF ZN SET LINESTR=$GET(ARRAY(LINE,0))
  . ELSE  SET LINESTR=$GET(ARRAY(LINE))
  . SET LINESTR=$$REPLACE^XLFSTR(LINESTR,.SPEC)_"<BR>"
  . IF ZN SET ARRAY(LINE,0)=LINESTR
  . ELSE  SET ARRAY(LINE)=LINESTR
  QUIT        
  ;
HTML2TXS(LINESTR) ;
  ;"Purpose: text a string that is HTML formatted, and strips out tags
  ;"Input: LINESTR
  ;"Results: none
  ;"NOTE: This conversion causes some loss of HTML tags, so a round trip
  ;"      conversion back to HTML would fail.
  ;"Strip out all tags (except <VEFA>)
  ;"SET LINESTR=$$REPLSTR^TMGSTUT3(LINESTR,"<VEFA>","{VEFA}")
  SET LINESTR=$$REPLACE(LINESTR,"<VEFA>","{VEFA}")
  FOR  QUIT:(LINESTR'["<")!(LINESTR'[">")  DO  ;" aaa<bbb>ccc  or aaa>bbb<ccc
  . NEW S1,S2,S3
  . SET S1=$PIECE(LINESTR,"<",1)
  . IF S1[">" DO  QUIT
  . . SET LINESTR=$PIECE(LINESTR,">",1)_"}"_$PIECE($PIECE(LINESTR,">",2,999),"<",1)_"{"_$PIECE(LINESTR,"<",2,999)
  . SET S2=$PIECE($PIECE(LINESTR,"<",2,999),">",1)
  . SET S3=$PIECE(LINESTR,">",2,999)
  . SET LINESTR=S1_S3
  ;
  ;"Convert special characters
  NEW SPEC
  SET SPEC("&nbsp;")=" "
  SET SPEC("&lt;")="<"
  SET SPEC("&gt;")=">"
  SET SPEC("&amp;")="&"
  SET SPEC("&quot;")=""""
  SET SPEC("{VEFA}")="<VEFA>"
  SET SPEC("{NL}")=""""
  SET LINESTR=$$REPLACE^XLFSTR(LINESTR,.SPEC)
  ;
  QUIT LINESTR
  ;
REPLACE(LINE,MATCHSTR,SUBSTR) ;
  ;"Purpose: wrapper for $$REPLACE^XLFSTR for simpler use
  ;"Result: returns NEW string
  NEW TMGSPEC SET TMGSPEC(MATCHSTR)=SUBSTR
  QUIT $$REPLACE^XLFSTR(LINE,.TMGSPEC)
  ;
SIGPICT(DUZ,DATE) ;
  ;"Purpose: Return HTML tag pointing to signiture image, or '' IF none.
  ;"Input: DUZ -- The user for whom to get sig image
  ;"       DATE -- (Optional) The date of the signed document.  A user may change
  ;"               their current signature image over time.
  ;"               If not provided, then the LAST entered sig image is returned
  ;"Results: An HTML tag with pointer to image, or '' IF none.
  NEW RESULT
  SET RESULT=""
  ;"finish!!!
  QUIT RESULT
  ;
HTMSPLIT(STR,LEN)  ;"Split HTML string in way that doesn't break tags
  ;"Input: STR -- PASS BY REFERENCE.  STR will be changed to residual string after split
  ;"       LEN -- Max length of line.  Returned result may be less than LEN if LEN lands in an HTML tag
  ;"Result: Returns first part of string.  STR will also be modified such that Result+STR=<Original String>
  ;"NOTE: It is required that the beginning of the string NOT be in an html tag.  
  ;"      It IS allowed that first character is "<" (the beginning of a tag) 
  NEW TMGRESULT SET TMGRESULT=0
  ;"to be completed...
  QUIT TMGRESULT
  ;
HTMLTRIM(STR,FLAG,TRIMCHARS,TRIMTAGS) ;"Trim from HTML text
  ;"Purpose: Mirror $$TRIM^XLFSTR, but handle HTML codes
  ;"Input: STR -- This is the input string.  Don't pass by reference
  ;"       Flags -- L = trim from left, R= trim from right, LR = trim from both ends
  ;"                OPTIONAL.  Default is LR
  ;"       TRIMCHARS -- OPTIONAL.  Characters to trim.  NOTE: these should be
  ;"                   characters as they appear on the screen.  E.g. use
  ;"                   '<', not '&lt;'
  ;"                Default value is " " (a space)
  ;"       TRIMTAGS -- OPTIONAL.  Default=0
  ;"                If 1 then tags (e.g. <..>) are trimmed
  ;"Result: Returns modified string
  ;
  NEW CONV
  SET CONV(1,"&nbsp;")=" "
  SET CONV(1,"&lt;")="<"
  SET CONV(1,"&gt;")=">"
  SET CONV(1,"&amp;")="&"
  SET CONV(1,"&quot;")=""""
  SET CONV(1,"<P>")=" "
  SET CONV(1,"</P>")=" "
  SET CONV(1,"<BR>")=" "
  DO
  . NEW CODE SET CODE="" 
  . FOR  SET CODE=$ORDER(CONV(1,CODE)) QUIT:CODE=""  DO
  . . NEW I2,RCODE SET RCODE=""
  . . FOR I2=$LENGTH(CODE):-1:1 SET RCODE=RCODE_$EXTRACT(CODE,I2)
  . . SET CONV(2,RCODE)=$GET(CONV(1,CODE))
  NEW IDX SET IDX=0
  NEW TMGRESULT SET TMGRESULT=""
  SET STR=$GET(STR)
  NEW DONE,LEN
  SET FLAG=$GET(FLAG) IF FLAG="" SET FLAG="LR"
  SET TRIMCHARS=$GET(TRIMCHARS," ")
  SET TRIMTAGS=+$GET(TRIMTAGS)
  NEW CH,INTAG,INSPECL SET (INTAG,INSPECL)=0
  NEW SPECIALCH SET SPECIALCH=""
  IF FLAG["L" DO
  . SET LEN=$LENGTH(STR)
  . SET DONE=0
  . FOR IDX=1:1:LEN DO  QUIT:DONE
  . . SET CH=$EXTRACT(STR,IDX) QUIT:(CH="")
  . . IF CH="<" SET INTAG=1
  . . IF CH="&" SET INSPECL=1
  . . IF INSPECL DO  QUIT
  . . . SET SPECIALCH=SPECIALCH_CH
  . . . IF CH=";" DO
  . . . . SET INSPECL=0
  . . . . NEW ALTCH SET ALTCH=$GET(CONV(1,SPECIALCH))
  . . . . IF TRIMCHARS[ALTCH SET SPECIALCH="" QUIT 
  . . . . SET TMGRESULT=TMGRESULT_SPECIALCH
  . . . . SET SPECIALCH=""
  . . IF INTAG DO  QUIT
  . . . IF 'TRIMTAGS SET TMGRESULT=TMGRESULT_CH ;"include <..> tags in output
  . . . IF CH=">" SET INTAG=0
  . . IF TRIMCHARS[CH QUIT  ;"skip chars to be trimmed 
  . . SET DONE=1
  . . SET TMGRESULT=TMGRESULT_$EXTRACT(STR,IDX,LEN)
  . . SET STR=TMGRESULT  ;"in case also planning R trim
  IF FLAG["R" DO
  . SET TMGRESULT=""
  . SET DONE=0
  . SET LEN=$LENGTH(STR)
  . FOR IDX=LEN:-1:1 DO  QUIT:DONE
  . . SET CH=$EXTRACT(STR,IDX) QUIT:(CH="")
  . . IF CH=">" SET INTAG=1
  . . IF CH=";",$$ISSPECL(STR,IDX) SET INSPECL=1
  . . IF INSPECL DO  QUIT
  . . . SET SPECIALCH=SPECIALCH_CH
  . . . IF CH="&" DO
  . . . . SET INSPECL=0
  . . . . NEW ALTCH SET ALTCH=$GET(CONV(2,SPECIALCH))
  . . . . IF TRIMCHARS[ALTCH SET SPECIALCH="" QUIT 
  . . . . SET TMGRESULT=SPECIALCH_TMGRESULT
  . . . . SET SPECIALCH=""
  . . IF INTAG DO  QUIT
  . . . IF 'TRIMTAGS SET TMGRESULT=CH_TMGRESULT ;"include <..> tags in output
  . . . IF CH="<" SET INTAG=0
  . . IF TRIMCHARS[CH QUIT  ;"skip chars to be trimmed 
  . . SET DONE=1
  . . SET TMGRESULT=$EXTRACT(STR,1,IDX)_TMGRESULT
  QUIT TMGRESULT
  ;
ISSPECL(STR,IDX) ;"PRIVATE
  ;"NOTE: when searching forwards, "&" is unique signal of unique HTML char 
  ;"      but when searching backwards ";" is not unique (can have plain ';').
  ;"Success if finds & before another ;, and length < 10
  ;"Searches backwards
  ;"Input: STR, string to search (backwards)
  ;"       IDX -- starting index.  Should be ; at start
  ;"Result: 1 IF in HTML special character, e.g. "&lt;", 0 otherwise
  NEW I2
  NEW TMGRESULT SET TMGRESULT=0
  NEW DONE SET DONE=0
  FOR I2=IDX-1:-1:IDX-10 DO  QUIT:DONE
  . NEW CH SET CH=$EXTRACT(STR,I2)
  . IF CH=";" SET DONE=1 QUIT
  . IF CH="&" SET TMGRESULT=1,DONE=1 QUIT
  QUIT TMGRESULT
  ;
STRIPSCR(IEN8925)  ;"Strip out any <SCRIPT> .. </SCRIPT> from documents
  ;"NOTE: if, for some reason, there was text inside a string inside
  ;"      the javascript code with ' .... </script ... ', then this
  ;"      function would be confused.  This is because it is not keeping
  ;"      track of status regarding inside/outside strings...
  NEW REF SET REF=$NAME(^TIU(8925,IEN8925,"TEXT"))
SSL1 ;
  NEW SCRIPTSTART SET SCRIPTSTART=0
  NEW FOUND SET FOUND=0        
  NEW LINENUM SET LINENUM=0
  FOR  SET LINENUM=$ORDER(@REF@(LINENUM)) QUIT:(+LINENUM'>0)!FOUND  DO
  . IF (@REF@(LINENUM,0)["<SCRIPT")!(@REF@(LINENUM,0)["<script") DO
  . . SET SCRIPTSTART=LINENUM
  . IF SCRIPTSTART>0,(@REF@(LINENUM,0)["</SCRIPT")!(@REF@(LINENUM,0)["</script") DO        
  . . ;"CUTTING OUT JAVASCRIPT CODE HERE...
  . . ;"NOTES: I just delete the unwanted array elements.  It does not appear to cause 
  . . ;"any problem when there is a gap in the index numbering when loading for CPRS.
  . . ;"LATER: I could pull the array, reorder it, and file it again with Fileman.  This
  . . ;"would also ensure any QWIK xrefs etc on the document are correct.
  . . NEW L1 SET L1=$GET(@REF@(SCRIPTSTART,0))
  . . NEW TAG1 FOR TAG1="<SCRIPT","<script" QUIT:L1[TAG1
  . . SET @REF@(SCRIPTSTART,0)=$PIECE(L1,TAG1,1)
  . . NEW IDX SET IDX=SCRIPTSTART
  . . FOR  SET IDX=$ORDER(@REF@(IDX)) QUIT:(+IDX'>0)!(IDX>=LINENUM)  DO
  . . . KILL @REF@(IDX)  ;"delete javascript lines. 
  . . NEW L2 SET L2=$GET(@REF@(LINENUM,0))
  . . NEW TAG2 FOR TAG2="</SCRIPT","</script" QUIT:L2[TAG2
  . . NEW P SET P=$FIND(L2,TAG2,0) SET P=$FIND(L2,">",P)
  . . SET L2=$EXTRACT(L2,P,$LENGTH(L2))
  . . SET @REF@(LINENUM,0)=L2
  . . SET FOUND=1
  IF FOUND=1 GOTO SSL1  ;"loop back to check for more than one instance of <script> </script>
  QUIT
  ;
RMTAGS(TEXT,TAG) ;"REMOVE TAGS
  FOR  QUIT:TEXT'[TAG  DO
  . SET TEXT=$P(TEXT,TAG,1)_$P(TEXT,TAG,2,999)
  QUIT
  ;"    
RPTAGS(TEXT,TAG,NEWTAG)  ;"REPLACE TAGS
  FOR  QUIT:TEXT'[TAG  DO
  . SET TEXT=$P(TEXT,TAG,1)_NEWTAG_$P(TEXT,TAG,2,999)
  QUIT
  ;  
