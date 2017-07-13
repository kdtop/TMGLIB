TMGC0P03   ; TMG - Web Service main entry points;  1/21/17
        ;;1.0;TMG E LAB ORDERG;;1/21/17
        ;
SOAP(C0PRTN,C0PTID,C0PDUZ,C0PDFN,C0PVOR,SUBTYPE,TMGREFARR)  ; MAKES A SOAP CALL FOR TEMPLATE ID C0PTID
        ;Copied and modified extensively from SOAP^C0PWS1
        ;NOTE: fundamental change was added support for SUBTYPE (SUBSCRIPTION TYPE)
        ;      and removal of debug code and use of EN^TMGC0P01 instead of EN^C0PMAIN
        ;INPUT: C0PRTN -- PASS BY NAME, AN OUT PARAMETER.  Loaded with XML results
        ;       C0PTID -- TemplateID -- Name of record in [C0P XML TEMPLATE] file, OR IEN IN SAME
        ;       C0PDUZ -- IEN 200 FOR USER
        ;       C0PDFN -- IEN 2 FOR PATIENT
        ;       C0PVOR -- OPTIONAL.  THE NAME OF A VARIABLE OVERRIDE ARRAY, WHICH IS APPLIED BEFORE MAPPING
        ;       SUBTYPE -- SUBSCRIPTION TYPE
	;       TMGREFARR -- PASS BY NAME.  An array to received back HL7Message and HtmlMessage arrays.
        ;RESULT: 1^OK, or -1^ErrorMessage
        ;
        NEW C0PARY,C0PF,C0PRXML,C0PRSLT,C0PURL,C0PPURL,C0PIDX,ok       ;//kt added line
        NEW C0PAF,C0PBF,C0PERROR,C0PHEAD,C0PID,C0PLOC,C0PLOCF,C0PSUBF  ;//kt added line
        NEW C0PMIME,C0PNOM,C0PNPIF,C0PR,C0PRHDR,C0PACCT,C0PSID         ;//kt added line
        NEW C0PTMPL,C0PREDUX,C0PWS,C0PXML,C0PSIEN,C0PUTID,C0PXF        ;//kt added line
        NEW TMGRESULT SET TMGRESULT="1^OK"
        ;
        N xml,template,header   ;//kt moved from below
        D INITXPF^C0PWS1("C0PF") ; SET FILE NUMBER AND PARAMATERS
        S C0PXF=C0PF("XML FILE NUMBER") ; FILE NUMBER FOR THE C0P XML TEMPLATE FILE
        I +C0PTID=0 D  ; A STRING WAS PASSED FOR THE TEMPLATE NAME
        . S C0PUTID=$$RESTID^TMGC0P01(C0PDUZ,C0PTID,SUBTYPE) ;RESOLVE TEMPLATE IEN FROM NAME
        E  S C0PUTID=C0PTID ; AN IEN WAS PASSED
        S C0PHEAD=$$GET1^DIQ(C0PXF,C0PUTID_",",2.2,,"header")
        S C0PMIME=$$GET1^DIQ(C0PXF,C0PUTID_",","MIME TYPE")
        S C0PPURL=$$GET1^DIQ(C0PXF,C0PUTID_",","PROXY SERVER")
        N SERVIEN S SERVIEN=$$SETUP^TMGC0P01(SUBTYPE) ; INIT C0PACCT, an IEN OF WS ACCOUNT
        S C0PURL=$$WSURL^TMGC0P02(C0PACCT,C0PUTID) ; RESOLVES PRODUCTION VS TEST
        S C0PXML=$$GET1^DIQ(C0PXF,C0PUTID_",",2.1,,"xml")
        S C0PTMPL=$$GET1^DIQ(C0PXF,C0PUTID_",",3,,"template")
        I C0PTMPL="template" D  ; there is a template to process
        . K xml ; going to replace the xml array
        . D EN^TMGC0P01("xml","url",C0PDUZ,C0PDFN,C0PUTID,$G(C0PVOR),0,SUBTYPE)
        I $G(C0PPROXY) S C0PURL=C0PPURL
        I '$D(C0PERROR) S C0PERROR="0^NO ERRORS" ; to do: start using this gpl
        K C0PRSLT,C0PRHDR
        ;
        NEW ZZDEBUG SET ZZDEBUG=0 
        IF ZZDEBUG=1 DO
        . NEW xml2,header2,url,SAME
        . DO DEBUGXML(.url,.xml2,.header2)
        . SET SAME=$$COMPARRS(.xml,.xml2) 
        . WRITE "xml comp=",SAME,!
        . SET SAME=$$COMPARRS(.header,.header2) 
        . WRITE "header comp=",SAME,!
        . if url'=C0PURL write "URL'S DIFFERENT:",!,C0PURL,url,!
        S ok=$$httpPOST^TMGEWD01(C0PURL,.xml,C0PMIME,.C0PRSLT,.header,2,.gpl5,.C0PRHDR)
        IF ok'="" set TMGRESULT="-1^"_ok GOTO SOAPDN
        KILL xml,template   ;//kt added line to make var table inspection easier
        ;
        ;"below, extract the HL7Message and HtmlMessage blobs into separate array
        NEW TAG,SPEC
        FOR TAG="HL7Message","HtmlMessage" DO
        . SET SPEC("%%"_TAG_"%%")=TAG
        . NEW IDX SET IDX=1
        . FOR  DO  QUIT:ok=0
        . . NEW ARRNAME SET ARRNAME=$NAME(@TMGREFARR@(TAG,IDX))
        . . SET ok=$$XTRTAGVL(TAG,ARRNAME,.C0PRSLT,"%%"_ARRNAME_"%%")  ;Extract 1 HL7Message from C0PRSLT
        . . NEW STR,JDX SET (STR,JDX)=""
        . . FOR  SET JDX=$ORDER(@TMGREFARR@(TAG,IDX,JDX)) QUIT:+JDX'>0  DO
        . . . SET STR=STR_$GET(@TMGREFARR@(TAG,IDX,JDX))
        . . KILL @TMGREFARR@(TAG,IDX)
        . . NEW UNSTR SET UNSTR=$$DECODE^RGUTUU(STR)
        . . IF UNSTR'="" SET @TMGREFARR@(TAG,IDX)=UNSTR
        . . SET IDX=IDX+1
        ;"below fix tag names back to original state
        NEW TMGIDX SET TMGIDX=""
        FOR  SET TMGIDX=$ORDER(C0PRSLT(TMGIDX)) QUIT:+TMGIDX'>0  DO
        . NEW STR SET STR=$GET(C0PRSLT(TMGIDX))
        . SET STR=$$REPLACE^XLFSTR(STR,.SPEC)
        . SET C0PRSLT(TMGIDX)=STR
        ;"Below, I will parse such that each line is 1 XML element
        NEW TMGTEMP SET TMGIDX=""
        NEW TMGJDX SET TMGJDX=1
        FOR  SET TMGIDX=$ORDER(C0PRSLT(TMGIDX)) QUIT:+TMGIDX'>0  DO
        . NEW STR SET STR=$GET(C0PRSLT(TMGIDX))
        . FOR  QUIT:STR=""  DO
        . . NEW STRA,STRB SET (STRA,STRB)=""
        . . IF STR["<" SET STRA=$PIECE(STR,"<",1),STR="<"_$PIECE(STR,"<",2,999)
        . . IF STRA'="" DO  QUIT
        . . . SET TMGTEMP(TMGJDX)=STRA,TMGJDX=TMGJDX+1
        . . . SET (STRA,STRB)=""
        . . IF STR[">" SET STRA=$PIECE(STR,">",1),STRB=$PIECE(STR,">",2,999)
        . . IF STRA'="" DO  QUIT
        . . . SET TMGTEMP(TMGJDX)=STRA_">",TMGJDX=TMGJDX+1
        . . . SET STR=STRB
        . . SET TMGTEMP(TMGJDX)=STR,TMGJDX=TMGJDX+1
        . . SET STR=""
        ;"Below, parse XML into a mumps multinode array
        SET TMGIDX=0
        NEW TMGXML,REFXML SET REFXML="TMGXML"
        FOR  SET TMGIDX=$ORDER(TMGTEMP(TMGIDX)) QUIT:+TMGIDX'>0  DO
        . NEW STR SET STR=$GET(TMGTEMP(TMGIDX)) QUIT:STR=""
        . IF STR["soap:" QUIT
        . IF $EXTRACT(STR,1,2)="</" DO
        . . NEW TAG SET TAG=$PIECE($PIECE(STR,"</",2),">",1)
        . . NEW QL SET QL=$QL(REFXML)
        . . NEW QS SET QS=$QS(REFXML,QL)
        . . IF TAG=$PIECE(QS,".",1) SET TAG=QS
        . . SET REFXML=$GET(@REFXML@("@Parent"))
        . . IF REFXML="" SET REFXML="TMGXML"
        . . KILL @REFXML@(TAG,"@Parent")
        . ELSE  IF $EXTRACT(STR,1,1)="<" DO
        . . IF STR[" />" DO
        . . . NEW TAG SET TAG=$PIECE($PIECE(STR,"<",2)," />",1),STR=""
        . . . NEW PARAM SET PARAM=$PIECE(TAG," ",2,999),TAG=$PIECE(TAG," ",1)
        . . . SET @REFXML@(TAG)=""
        . . ELSE  DO
        . . . NEW TAG SET TAG=$PIECE($PIECE(STR,"<",2),">",1),STR=""
        . . . IF TAG["?xml" QUIT
        . . . NEW PARAM SET PARAM=$PIECE(TAG," ",2,999),TAG=$PIECE(TAG," ",1)
        . . . NEW TEMPREF SET TEMPREF=REFXML
        . . . SET REFXML=$NAME(@REFXML@(TAG))
        . . . NEW CT FOR CT=2:1 QUIT:$DATA(@REFXML)=0  SET REFXML=$NAME(@TEMPREF@(TAG_"."_CT))
        . . . SET @REFXML@("@Parent")=TEMPREF
        . . . IF PARAM'="" SET @REFXML@("@Param")=PARAM
        . ELSE  DO
        . . SET @REFXML=STR
        KILL @C0PRTN MERGE @C0PRTN=TMGXML 
SOAPDN  Q TMGRESULT
        ;
XTRTAGVL(TAG,REFOUT,ARR,REPLNAME)  ;Extract Value, between TAGS, ARR, and return in OUT, modifying ARR
	;"Input: TAG -- the name of the XML to search for (this IS case-sensitive). DON'T include '<' or '>' or '</'
	;"       REFOUT -- AN OUT PARAMETER, PASS BY NAME.  Format: @OUT@(#)=<string>
	;"       ARR -- Input array. PASS BY REFERENCE.  Format: ARR(#)=<string>
	;"       REPLNAME -- OPTIONAL.  Replacement text to put as value for removed block
	;"Result: 1^OK, or -1^Error message, or 0 if not found
	NEW TMGRESULT SET TMGRESULT="1^OK"
	SET REPLNAME=$GET(REPLNAME),TAG=$GET(TAG)
	NEW STAG SET STAG="<"_TAG_">"
	NEW ETAG SET ETAG="</"_TAG_">"
	NEW RPLSTAG SET RPLSTAG="<%%"_TAG_"%%>"
	NEW RPLETAG SET RPLETAG="</%%"_TAG_"%%>"
	NEW IDX SET IDX=""
	NEW ODX SET ODX=1
	NEW SFOUND SET SFOUND=0
	NEW EFOUND SET EFOUND=0
	FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:(+IDX'>0)!EFOUND  DO
	. NEW STR SET STR=$GET(ARR(IDX)) QUIT:STR=""
	. NEW LINEMOD SET LINEMOD=0
	. IF 'SFOUND DO
	. . IF STR'[STAG QUIT
	. . SET SFOUND=1
	. . SET ARR(IDX)=$PIECE(STR,STAG,1)_RPLSTAG_REPLNAME,LINEMOD=1
	. . SET STR=$PIECE(STR,STAG,2,99)
	. IF SFOUND DO
	. . IF 'LINEMOD KILL ARR(IDX)
	. . IF STR[ETAG DO
	. . . SET EFOUND=1
	. . . NEW STRB SET STRB=$PIECE(STR,ETAG,2,99)
	. . . SET STR=$PIECE(STR,ETAG,1)
	. . . IF LINEMOD SET ARR(IDX)=ARR(IDX)_RPLETAG_STRB
	. . . ELSE  SET ARR(IDX)=RPLETAG_STRB
	. . SET @REFOUT@(ODX)=STR,ODX=ODX+1
	IF SFOUND,'EFOUND SET TMGRESULT="-1^Start tag found, but end tag not found"
	ELSE  IF 'SFOUND SET TMGRESULT=0
	QUIT TMGRESULT
	;
DEBUGXML(URL,XML,HEADER)  ;
        KILL XML,HEADER
        NEW IDX 
        SET IDX=1
        SET HEADER(IDX)="User-Agent: Mozilla/4.0 (compatible; MSIE 6.0; MS Web Services Client Protocol 2.0.50727.3074)",IDX=IDX+1
        SET HEADER(IDX)="Expect: 100-continue",IDX=IDX+1
        SET HEADER(IDX)="Connection: Keep-Alive",IDX=IDX+1
        SET IDX=1
        SET XML(IDX)="<?xml version=""1.0"" encoding=""utf-8"" ?>",IDX=IDX+1
        SET XML(IDX)="<soap:Envelope xmlns:soap=""http://www.w3.org/2003/05/soap-envelope"" xmlns:web=""https://secure.newcropaccounts.com/V7/webservices"">",IDX=IDX+1
        SET XML(IDX)="  <soap:Header/>",IDX=IDX+1
        SET XML(IDX)="  <soap:Body>",IDX=IDX+1
        SET XML(IDX)="    <web:GetLabResults>",IDX=IDX+1
        SET XML(IDX)="      <web:credentials>",IDX=IDX+1
        SET XML(IDX)="        <web:PartnerName>newcropdemo</web:PartnerName>",IDX=IDX+1
        SET XML(IDX)="        <web:Name>demo</web:Name>",IDX=IDX+1
        SET XML(IDX)="        <web:Password>demo</web:Password>",IDX=IDX+1
        SET XML(IDX)="      </web:credentials>",IDX=IDX+1
        SET XML(IDX)="      <web:accountRequest>",IDX=IDX+1
        SET XML(IDX)="        <web:AccountId>johntest</web:AccountId>",IDX=IDX+1
        SET XML(IDX)="        <web:SiteId>test</web:SiteId>",IDX=IDX+1
        SET XML(IDX)="      </web:accountRequest>",IDX=IDX+1
        SET XML(IDX)="      <web:locationId>1</web:locationId> <!--In preproduction this is optional-->",IDX=IDX+1
        SET XML(IDX)="      <web:licensedPrescriberId>doctor2</web:licensedPrescriberId> <!--In preproduction this is optional-->",IDX=IDX+1
        SET XML(IDX)="      <web:reportDateCCYYMMDD>20160425</web:reportDateCCYYMMDD>",IDX=IDX+1
        SET XML(IDX)="      <web:includeHL7Result>Y</web:includeHL7Result>",IDX=IDX+1
        SET XML(IDX)="      <web:includeHtmlResult>Y</web:includeHtmlResult>",IDX=IDX+1
        SET XML(IDX)="    </web:GetLabResults>",IDX=IDX+1
        SET XML(IDX)="  </soap:Body>",IDX=IDX+1
        SET XML(IDX)="</soap:Envelope>",IDX=IDX+1
        SET URL="http://preproduction.newcropaccounts.com/V7/WebServices/CI/Lab.asmx?wsdl"
        ;
        QUIT
	;
COMPARRS(ARR1,ARR2)  ;
	NEW IDX1,IDX2  SET (IDX1,IDX2)=""
	NEW SAME SET SAME=1
	NEW S1,S2
L1	SET IDX1=$ORDER(ARR1(IDX1)),IDX2=$ORDER(ARR2(IDX2))
	IF IDX1="",IDX2="" GOTO COMPDN
	IF IDX1="",IDX2'="" DO  GOTO COMPDN
	. WRITE "ARRAY 2 HAS MORE LINES THAN ARRAY 1"
	. SET SAME=0
	IF IDX1'="",IDX2="" DO  GOTO COMPDN
	. WRITE "ARRAY 1 HAS MORE LINES THAN ARRAY 2"
	. SET SAME=0
	SET S1=$GET(ARR1(IDX1)),S2=$GET(ARR2(IDX2))
	IF S1=S2 GOTO L1
	WRITE "LINES DIFFERENT",!
	WRITE "ARR1(",IDX1,")=",S1,!
	WRITE "ARR2(",IDX2,")=",S2,!
	SET SAME=0
COMPDN  QUIT SAME