TMGHTM2 ;TMG/kst-HTML utilities ; 6/23/17
         ;;1.0;TMG-LIB;**1,17**;08/10/10
 ;
 ;"Utility functions related to documents with HTML formatting
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2017  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"---------------------------------------------------------------------------
 ;"PUBLIC FUNCTIONS
 ;"---------------------------------------------------------------------------
 ;"PROCESS(HTMLIO,CALLBACKFN) -- Parse HTML into DOM, then call CALLBACK Fn, then release DOM
 ;
 ;"---------------------------------------------------------------------------
 ;"PRIVATE FUNCTIONS
 ;"---------------------------------------------------------------------------
 ;
 ;"---------------------------------------------------------------------------
 ;"DEPENDENCIES:
 ;
 ;"---------------------------------------------------------------------------
 ;
PROCESS(HTMLIO,CALLBACKFN)  ;"Parse HTML array into HTML DOM, then call CALLBACK Fn, then release DOM
  ;"Purpose: 1) Parse HTML array into HTML DOM, 
  ;"         2) call user's CALLBACK Fn where they can manipulate DOM
  ;"         3) Convert DOM back into HTML array
  ;"         4) Release/delete DOM
  ;"Input: HTMLIO -- PASS BY REFERENCE.  Format: HTMLIO(#)=<text> or HTMLIO=<text> (but not both)  
  ;"       CALLBACKFN -- pass function for call back. Format: 'MYTAG^MYROUTINE'
  ;"                    The passed function must look like this:
  ;"                       MYTAG(DOM_NAME,ERR) ; <-- in MYROUTINE , with no return result
  ;"                    User function can use %zewd* calls to manipulate DOM.  
  ;"                    ERR should be left empty, or fill will error message if needed.  
  ;"Output: HTMLIO is modified.  
  ;"Result: 1 if OK, or 1^SKIPPED, or -1^Error message if problem.  
  NEW TMGRESULT SET TMGRESULT=1

  NEW IO,POP,SAVEIO SET SAVEIO=$IO
  NEW DOMNAME SET DOMNAME="PROCESS_TMGHTM2_"_$J
  NEW RESULTASONELINE SET RESULTASONELINE=0
  NEW HTML MERGE HTML=HTMLIO
  IF $GET(HTML)'="" DO  GOTO:TMGRESULT<0 PROCDN
  . IF $ORDER(HTML(""))'="" DO  QUIT
  . . SET TMGRESULT="-1^Data found in both HTML= and HTML(x)="
  . SET HTML(1)=HTML,HTML="",RESULTASONELINE=1
  IF $$ISHTMLAR^TMGHTM1(.HTML)=0 SET TMGRESULT="1^SKIPPED" GOTO PROCDN  ;"//kt  If passed text is NOT HTML, then parser will hang on chars like '<', so skip
  NEW ERR SET ERR=$$PARSHTML^TMGEWD01(.HTML,DOMNAME)
  IF ERR'="" SET TMGRESULT="-1^"_ERR GOTO PROCDN
  NEW CODE SET CODE="DO "_CALLBACKFN_"("""_DOMNAME_""",.ERR)"
  ;"DO  
  NEW $ETRAP SET $ETRAP="SET TMGRESULT=""-1^Error with callback function"" set $etrap="""",$ecode="""""
  XECUTE CODE
  IF +TMGRESULT<0 GOTO PROCDN
  IF ERR'="" SET TMGRESULT="-1^"_ERR GOTO PROCDN
  ;"Turn DOM back into HTML for output. 
  NEW FNAME SET FNAME=$$UNIQUE^%ZISUTL("DOM_DUMP")  
  DO OPEN^%ZISH("TEMPFILE","/tmp/",FNAME,"W")
  IF POP DO  GOTO PROCDN
  . SET TMGRESULT="-1^ERROR OPENING FILE: /tmp/"_FNAME
  USE IO
  SET ERR=$$outputDOM^%zewdDOM(DOMNAME,1,1)
  DO CLOSE^%ZISH("TEMPFILE")
  IF ERR'="" SET TMGRESULT="-1^"_ERR GOTO PROCDN
  NEW TMP SET TMP=$$removeDocument^%zewdDOM(DOMNAME)  ;"ignore result 
  KILL ^TMP($J,"DOM_DUMP")
  NEW REF SET REF=$NAME(^TMP($J,"DOM_DUMP",1))
  SET TMP=$$FTG^%ZISH("/tmp/",FNAME,REF,3)  
  NEW FILESPEC SET FILESPEC(FNAME)=""
  SET TMP=$$DEL^%ZISH("/tmp/","FILESPEC")
  IF TMP=0 DO  GOTO PROCDN
  . SET TMGRESULT="-1^ERROR DELETING FILE: /tmp/"_FNAME
  KILL HTML MERGE HTML=^TMP($J,"DOM_DUMP")
  KILL ^TMP($J,"DOM_DUMP")
  NEW ONELINE SET ONELINE=""
  SET IDX=""
  FOR  SET IDX=$ORDER(HTML(IDX)) QUIT:IDX=""  DO
  . NEW STR SET STR=$GET(HTML(IDX)) QUIT:STR=""
  . NEW JDX SET JDX=0
  . FOR  SET JDX=$ORDER(HTML(IDX,"OVF",JDX)) QUIT:JDX'>0  DO
  . . SET STR=STR_$GET(HTML(IDX,"OVF",JDX))
  . KILL HTML(IDX,"OVF")
  . NEW LEN SET LEN=$LENGTH(STR)
  . IF $EXTRACT(STR,LEN)=$CHAR(13) SET STR=$EXTRACT(STR,1,LEN-1)
  . SET HTML(IDX)=STR
  . SET ONELINE=ONELINE_STR
  KILL HTMLIO
  IF RESULTASONELINE SET HTMLIO=ONELINE
  ELSE  MERGE HTMLIO=HTML   
PROCDN  ; 
  USE SAVEIO  ;//$P
  QUIT TMGRESULT
  ;
UPTAGS(HTMLSTR)  ;"Convert all HTML tag names to UPPER CASE
  QUIT $$CASETAGS(HTMLSTR,1)
  ;
LOTAGS(HTMLSTR)  ;"Convert all HTML tag names to LOWER CASE
  QUIT $$CASETAGS(HTMLSTR,2)
  ;
CASETAGS(HTMLSTR,MODE)  ;"Convert all HTML tag names to specified CASE
  ;"INPUT: HTMLStr -- all HTML in 1 long string
  ;"       MODE -- 1 means convert to upper case
  ;"               2 means convert to lower case
  ;"Results: returns converted string
  ;"s ^TMP("EDDIE","HTMLSTR")=HTMLSTR
  NEW POS SET POS=0
  NEW DONE SET DONE=0
  NEW OUTSTR SET OUTSTR=HTMLSTR
  FOR  SET POS=$FIND(OUTSTR,"<",POS) QUIT:(POS=0)!(DONE)  DO
  . NEW NEXT SET NEXT=$$NEXTCH^TMGSTUT3(.OUTSTR,POS,"/"," ",">")
  . IF NEXT="/" DO
  . . SET POS=$FIND(OUTSTR,NEXT,POS)
  . . SET NEXT=$$NEXTCH^TMGSTUT3(.OUTSTR," ",">")
  . NEW P2 SET P2=$FIND(OUTSTR,NEXT,POS) QUIT:P2=0
  . NEW STRA SET STRA=$EXTRACT(OUTSTR,1,POS-1)
  . NEW STRB SET STRB=$EXTRACT(OUTSTR,P2-1,$LENGTH(OUTSTR))
  . NEW TAG SET TAG=$EXTRACT(OUTSTR,POS,P2-2)
  . NEW NEWTAG SET NEWTAG=$SELECT(MODE=1:$$UP^XLFSTR(TAG),2:$$LOW^XLFSTR(TAG),1:TAG)
  . SET OUTSTR=STRA_NEWTAG_STRB
  ;"s ^TMP("EDDIE","OUTSTR")=OUTSTR
  QUIT OUTSTR
  ;"
TIUPASTE(TMGRESULT,HTMLARR)  ;"RPC: TMG CPRS HTML PASTE EVENT
  ;"Purpose: remove unwanted HTML tags from Clipboard prior to pasting
  MERGE ^TMP($J,$$NOW^TMGDATE,"HTMLARR")=HTMLARR
  NEW TMGTEST SET TMGTEST=0
  IF TMGTEST=1 DO
  . MERGE HTMLARR=^TMG("TIUPASTE","HTML")
  ELSE  DO
  . K ^TMG("TIUPASTE","HTML")
  . MERGE ^TMG("TIUPASTE","HTML")=HTMLARR
  ;"MERGE ^TMG($J,$$NOW^TMGDATE,"HTMLARR")=HTMLARR
  NEW IDX SET IDX=0
  NEW HTML SET HTML=""
  FOR  SET IDX=$O(HTMLARR(IDX)) QUIT:IDX'>0  DO
  . SET HTML=HTML_$G(HTMLARR(IDX))
  SET HTML="<"_$P(HTML,"<",2,9999)  ;" Remove Clipboard header info
  DO RMTAG2^TMGHTM1(.HTML,"a")
  DO RMTAG2^TMGHTM1(.HTML,"html")
  DO RMTAG2^TMGHTM1(.HTML,"body")
  DO RMTAG2^TMGHTM1(.HTML,"body")
  DO RMTAG2^TMGHTM1(.HTML,"font")
  DO RMTAG2^TMGHTM1(.HTML,"tt")
  IF HTML["<!--StartFragment-->" SET HTML=$P(HTML,"<!--StartFragment-->",2)
  IF HTML["<!--EndFragment-->" SET HTML=$P(HTML,"<!--EndFragment-->",1)
  ;"SET HTML=$$REPLSTR^TMGSTUT3(HTML,$C(13,10),"")
  SET TMGRESULT(0)=HTML
  QUIT
  ;"
  ;================================================================
  ;
SAMPLEHTML0  ;  DELETE LATER
  ;;"<!DOCTYPE html>
  ;;"<html>
  ;;"<body>
  ;;"<h1>My First Heading</h1>
  ;;"<p>My first paragraph.</p>
  ;;"<li>
  ;;" <font size="3">
  ;;"  <font size="1">
  ;;"   <font size="3">
  ;;"    <font size="1">
  ;;"      <font size="3">
  ;;"       <p>
  ;;"         <u>
  ;;"          Low Vit-D                                  
  ;;"         </u>
  ;;"         :
  ;;"         <em>
  ;;"          New test was OK.&nbsp;
  ;;"         </em>
  ;;"         No new data.
  ;;"       </p>
  ;;"      </font>
  ;;"    </font>
  ;;"   </font>
  ;;"  </font>
  ;;" </font>
  ;;"<li>              
  ;;"</body>
  ;;"</html>
  ;;"{END}
SAMPLEHTML1  ;;  DELETE LATER
  ;;"<!DOCTYPE html>
  ;;"<html>
  ;;" <font size="3" dir="north">
  ;;" Hello World
  ;;" </font>
  ;;"</html>
  ;;"{END}
  ;
SAMPLEHTML2  ;;  DELETE LATER
  ;;"<LI>
  ;;"<FONT size=1><FONT size=3>
  ;;"  <U>Hypertension</U>:<I> </I>[BP] 137/82 &lt;-114/63 &lt;- 110/62 &lt;- 128/61]]
  ;;"  <EM> Stable.&nbsp; </EM>
  ;;"</FONT></FONT>
  ;;"{END}
  
SAMPLEHTML  ;;  DELETE LATER
  ;;"<I>A</I><I> </I>X
  ;;"{END}
  
  ;"SYNTAX
  ;--------------------------------------------------------------------------------------------    
  ;"^zewdDOM("dom",1,"node",2)="html|1|1|3|3"
  ;"$PIECE  #   PURPOSE
  ;"        1   tag name
  ;"        2   node type
  ;"        3   parent number   
  ;"        4   first child number
  ;"        5   last child number
  ;"        6   previous sibling number
  ;"        7   next sibling number
  ;"^zewdDOM("dom",1,"node",7,"data",1)="My first paragraph."  <-- data for node 7 stored here
  ;"^zewdDOM("dom",1,"node",10,"data")=11  <-- data for node 10 is stored in node 11
  ;--------------------------------------------------------------------------------------------    
  ;"^zewdDOM("docNameIndex","TEST")=1
  ;"^zewdDOM("dom",1,"creationDate")="64455,75419"
  ;"^zewdDOM("dom",1,"docName")="TEST"
  ;"^zewdDOM("dom",1,"documentElement")=2
  ;"^zewdDOM("dom",1,"node")=31   <---- INDEX OF LAST NODE
  ;"^zewdDOM("dom",1,"node",1)="|9||2|2"
  ;"^zewdDOM("dom",1,"node",2)="html|1|1|3|3"
  ;"^zewdDOM("dom",1,"node",3)="body|1|2|4|8"
  ;"^zewdDOM("dom",1,"node",4)="h1|1|3|5|5||6"
  ;"^zewdDOM("dom",1,"node",5)="|3|4"
  ;"^zewdDOM("dom",1,"node",5,"data",1)="My First Heading"
  ;"^zewdDOM("dom",1,"node",6)="p|1|3|7|7|4|8"
  ;"^zewdDOM("dom",1,"node",7)="|3|6"
  ;-snip-
  ;"^zewdDOM("dom",1,"node",23,"data",1)=3
  ;"^zewdDOM("dom",1,"node",31)="li|1|8|||9"
  ;"^zewdDOM("dom",1,"nodeNameIndex","body",3)=""
  ;"^zewdDOM("dom",1,"nodeNameIndex","font",9)=""
  ;"^zewdDOM("dom",1,"nodeNameIndex","h1",4)=""
  ;"^zewdDOM("dom",1,"nodeNameIndex","html",2)=""
  ;"^zewdDOM("dom",1,"nodeNameIndex","li",8)=""
  ;"^zewdDOM("dom",1,"nodeNameIndex","li",31)=""
  ;"^zewdDOM("dom",1,"nodeNameIndex","p",6)=""
  ;"^zewdDOM("dom",1,"nodeTypeIndex",1,2)=""
  ;"^zewdDOM("dom",1,"nodeTypeIndex",1,3)=""
  ;-snip-
  ;"^zewdDOM("dom",1,"nodeTypeIndex",3,20)=""
  ;"^zewdDOM("dom",1,"nodeTypeIndex",3,23)=""  
  ;
GETSAMPLEHTML(ARR)  ;  ;  DELETE LATER
  NEW IDX,DONE SET DONE=0
  FOR IDX=1:1 DO  QUIT:DONE
  . NEW S SET S=$PIECE($TEXT(SAMPLEHTML+IDX),";;""",2)
  . SET S=$$TRIM^XLFSTR(S)
  . SET DONE=(S["{END}") QUIT:DONE
  . SET ARR(IDX)=S
  QUIT
  ;
TESTPARSE ; ;  DELETE LATER
  ;
  NEW ARR DO GETSAMPLEHTML(.ARR)  ;
  NEW ERR SET ERR=$$PARSHTML^TMGEWD01(.ARR,"TEST")
  s ERR=$$outputDOM^%zewdDOM("TEST",1,2)
  
  NEW docOID SET docOID=$$getDocumentNode^%zewdDOM("TEST")
  NEW SIZE FOR SIZE=1,3 DO
  . NEW nodes
  . SET TMP=$$select^%zewdXPath("//font[@size="""_SIZE_"""]",docOID,.nodes)
  . SET TMP=$$select^%zewdXPath("//i",docOID,.nodes)
  . DO displayNodes^%zewdXPath(.nodes)  
  . NEW OID,OIDIDX SET OIDIDX=""
  . FOR  SET OIDIDX=$ORDER(nodes(OIDIDX)) QUIT:OIDIDX=""  DO
  . . SET OID=$GET(nodes(OIDIDX)) QUIT:OID=""
  . . SET TMP=$$removeIntermediateNode^%zewdDOM(OID,1)
  . . IF TMP'="" WRITE !,TMP,!
  s ERR=$$outputDOM^%zewdDOM("TEST",1,2)
  
  s ok=$$removeDocument^%zewdDOM("TEST") 
  QUIT
  ; 
TESTPARSE2 ;   ;  DELETE LATER
  NEW DIC,X,Y SET DIC=8925,DIC(0)="MAEQ"
  ;"DO ^DIC WRITE !
  SET Y=360850
  IF Y'>0 QUIT
  NEW ARR,IDX SET IDX=0
  FOR  SET IDX=$ORDER(^TIU(8925,+Y,"TEXT",IDX)) QUIT:IDX'>0  SET ARR(IDX)=$GET(^TIU(8925,+Y,"TEXT",IDX,0))
  NEW TMP SET TMP=$$PARSHTML^TMGEWD01(.ARR,"TEST")
  SET TMP=$$outputDOM^%zewdDOM("TEST",1,2)   
  NEW docOID SET docOID=$$getDocumentNode^%zewdDOM("TEST")  

  NEW SIZE FOR SIZE=1,3 DO
  . NEW nodes
  . SET TMP=$$select^%zewdXPath("//font[@size="""_SIZE_"""]",docOID,.nodes)
  . ;"DO displayNodes^%zewdXPath(.nodes)  
  . NEW OID,OIDIDX SET OIDIDX=""
  . FOR  SET OIDIDX=$ORDER(nodes(OIDIDX)) QUIT:OIDIDX=""  DO
  . . SET OID=$GET(nodes(OIDIDX)) QUIT:OID=""
  . . SET TMP=$$removeIntermediateNode^%zewdDOM(OID,1)
  . . IF TMP'="" WRITE !,TMP,!  
  NEW FNAME SET FNAME=$$UNIQUE^%ZISUTL("DOM_DUMP")
  NEW IO DO OPEN^%ZISH("TEMPFILE","/tmp/",FNAME,"W")
  IF POP DO  QUIT
  . WRITE "ERROR OPENING FILE: ",FNAME,!
  USE IO
  s ERR=$$outputDOM^%zewdDOM("TEST",1,1)
  USE $P
  DO CLOSE^%ZISH("TEMPFILE")
  SET TMP=$$removeDocument^%zewdDOM("TEST") 
  
  KILL ^TMP($J,"DOM_DUMP")
  NEW REF SET REF=$NAME(^TMP($J,"DOM_DUMP",1))
  SET TMP=$$FTG^%ZISH("/tmp/",FNAME,REF,3)  
  NEW FILESPEC SET FILESPEC(FNAME)=""
  SET TMP=$$DEL^%ZISH("/tmp/","FILESPEC")
  IF TMP=0 DO
  . WRITE "ERROR DELETING FILE: ",FNAME,!
  KILL ARR
  MERGE ARR=^TMP($J,"DOM_DUMP")
  KILL ^TMP($J,"DOM_DUMP")
  SET IDX=""
  FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:IDX=""  DO
  . NEW STR SET STR=$GET(ARR(IDX)) QUIT:STR=""
  . NEW LEN SET LEN=$LENGTH(STR)
  . IF $EXTRACT(STR,LEN)'=$CHAR(13) QUIT
  . SET ARR(IDX)=$EXTRACT(STR,1,LEN-1)
  QUIT
 
