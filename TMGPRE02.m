TMGPRE02 ;TMG/kst/Pre-visit HTML stuff ;9/15/25
	;;1.0;TMG-LIB;**1**;9/15/25
	;
	;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
	;"Copyright (c) 9/15/25 Kevin S. Toppenberg MD
	;"
	;"This file is part of the TMG LIBRARY, and may only be used in accordence
	;" to license terms outlined in separate file TMGLICNS.m, which should
	;" always be distributed with this file.;
	;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
	;
	;"================================================================================"
	;"
HANDLEFORM(REC,TMGOUTREF,TMGSTYLESREF,TMGINDENT,ERR)	;
	;"INPUT: REC -- the form information array
	;"       GRPI[ -- listing of nodes to be handled.  e.g. 'RX^AWV^OTC'
	NEW NODE SET NODE=$GET(REC("storeNode")) QUIT:NODE=""
	NEW WRITERTAG SET WRITERTAG=$GET(REC("writerFnTag")) QUIT:WRITERTAG=""
	NEW WRITERRTN SET WRITERRTN=$GET(REC("writerFnRoutine")) QUIT:WRITERRTN=""
	NEW TMGFORMREF SET TMGFORMREF=$NAME(@REF@(NODE))
	NEW TMGTITLE SET TMGTITLE=$GET(REC("targetNoteTitleName"))
	NEW TMPERR SET TMPERR=""
	NEW CODE SET CODE="DO "_WRITERTAG_"^"_WRITERRTN_"(.TMGFORMREF,.TMGDFN,.TMGOUTREF,TMGSTYLESREF,.TMGINDENT,.TMPERR)"
	NEW $ETRAP SET $ETRAP="SET $ETRAP="""",$ECODE="""" "  ;"Ignore error.  If fails then DUE not set, so will quit loop
	XECUTE CODE   ;"e.g. 'DO WTCONSENT^TMGPRE02(...)
	IF TMPERR'="" DO
	. IF ERR'="" SET ERR=ERR_"; "
	. SET ERR=ERR_TMPERR
	QUIT
	;
OUTPUTFORMS(REF,FORMARR,TMGDFN,ERR)  ;"Save data (patient responses) from HxUpdate form.;
	NEW RESULT SET RESULT="1^OK"
	SET ERR=""  ;"default to no error"
	NEW GROUP
	SET GROUP(1)="HX^ROS^PHQ9^AWV"
	SET GROUP(2)="MEDS^OTC-MEDS^ALLERGY"
	SET GROUP(3)="CSNT"
	NEW GRPIDX SET GRPIDX=0
	FOR  SET GRPIDX=$ORDER(GROUP(GRPIDX)) QUIT:GRPIDX'>0  DO
	. NEW HTMLARR,STYLEARR
	. NEW INDENT SET INDENT=0,INDENT("delta")=2 DO INDENT(.INDENT)
	. NEW IDX SET IDX=""
	. FOR  SET IDX=$ORDER(FORMARR(IDX)) QUIT:IDX=""  DO
	. . NEW REC MERGE REC=FORMARR(IDX)
	. . NEW NODE SET NODE=$GET(REC("storeNode")) QUIT:NODE=""
	. . IF GROUP(GRPIDX)'[NODE QUIT
	. . DO HANDLEFORM(.REC,"HTMLARR","STYLEARR",.INDENT,.ERR)
	. IF $ORDER(HTMLARR(0))'>0 QUIT  ;"empty array
	. NEW HTMLTOP DO GETHTMLSTART(TMGDFN,"HTMLTOP","STYLEARR",.INDENT)  ;
	. DO PREFIXARR^TMGSTUT2(.HTMLARR,.HTMLTOP) ;"PREFIX HEADER ARRAY ONTO ARR
	. DO GETHTMLEND("HTMLARR",.INDENT)
	. NEW FPNAME SET FPNAME=$$UNIQUE^%ZISUTL("/mnt/WinPublic/Kevin/test_forms")_".html"
	. DO AR2HFSFP^TMGIOUT3("HTMLARR",FPNAME)
	IF ERR'="" SET RESULT="-1^"_ERR
FFDN  ;
	QUIT RESULT
	;
TESTFF  ;"  TESTFINALIZEFORMS
	NEW RESULT SET RESULT="1^OK"
	SET ERR=""  ;"default to no error"
	NEW TMGDFN SET TMGDFN=74592  ;"ZZTEST,BABY"
	NEW REF SET REF=$NAME(^TMG("TMP","KILLTHIS"))
	NEW FORMARR MERGE FORMARR=@REF@("FORMS")
	SET RESULT=$$OUTPUTFORMS(REF,.FORMARR,.TMGDFN,ERR)  ;"Save data (patient responses) from HxUpdate form.;
	QUIT RESULT
	;
	;"================================================================================"
	;
CAMELCASE(STR)	;"convert 'this is a test' --> 'This Is A Test'
	QUIT $$CAPWORDS^TMGSTUT2(.STR)
	;
INDENT(INDENT) ;
		SET INDENT=INDENT+$GET(INDENT("delta"),2)
		QUIT
		;
OUTDENT(INDENT) ;
		SET INDENT=INDENT-$GET(INDENT("delta"),2)
		QUIT
		;
ADDLINE(OUTREF,LINE,INDENT)	;"
	;"INPUT: OUTREF-- pass by NAME. Ref of output array. Expected format: @OUTREF@(#)=<LINE>"
	;"	     LINE -- the string to append to array
	;"       INDENT -- OPTIONAL.  Amount of indented spaces to put in front of LINE
	NEW IDX SET IDX=$ORDER(@OUTREF@("@"),-1)+1
	SET INDENT=+$GET(INDENT)
	NEW SPACE SET SPACE=$EXTRACT("                                               ",1,INDENT)
	SET @OUTREF@(IDX)=SPACE_LINE
	QUIT;
	;
SUBSTYLES(OUTREF,STYLESREF,INDENT)  ;
	NEW ITEM SET ITEM=""
	FOR  SET ITEM=$ORDER(@STYLESREF@(ITEM)) QUIT:ITEM=""  DO
	. NEW HASCHILDREN SET HASCHILDREN=(($DATA(@STYLESREF@(ITEM))#10)=0)
	. IF HASCHILDREN DO
	. . DO ADDLINE(OUTREF,ITEM_" {",INDENT) DO INDENT(.INDENT)
	. . NEW APROP SET APROP=""
	. . FOR  SET APROP=$ORDER(@STYLESREF@(ITEM,APROP)) QUIT:APROP=""  DO
	. . . NEW HASCHILDREN SET HASCHILDREN=(($DATA(@STYLESREF@(ITEM,APROP))#10)=0)
	. . . IF HASCHILDREN DO  QUIT
	. . . . DO ADDLINE(OUTREF,APROP_" {",INDENT) DO INDENT(.INDENT)
	. . . . DO SUBSTYLES(OUTREF,$NAME(@STYLESREF@(ITEM,APROP)),.INDENT)
	. . . . DO OUTDENT(.INDENT) DO ADDLINE(OUTREF,"}",INDENT)
	. . . NEW VALUE SET VALUE=$GET(@STYLESREF@(ITEM,APROP))
	. . . DO ADDLINE(OUTREF,APROP_": "_VALUE_";",INDENT)
	. . DO OUTDENT(.INDENT) DO ADDLINE(OUTREF,"}",INDENT)
	. ELSE  DO
	. . NEW APROP SET APROP=ITEM
	. . NEW VALUE SET VALUE=$GET(@STYLESREF@(APROP))
	. . DO ADDLINE(OUTREF,APROP_": "_VALUE_";",INDENT)
	QUIT
	;
SETUPSTYLES(OUTREF,STYLESREF,INDENT)	;"Convert Styles array to HTML output
	DO ADDLINE(OUTREF,"<style>",INDENT)
	DO INDENT(.INDENT)
	DO SUBSTYLES(OUTREF,STYLESREF,.INDENT)
	DO OUTDENT(.INDENT)
	DO ADDLINE(OUTREF,"</style>",INDENT)
	QUIT;
	;
GETHTMLSTART(TMGDFN,OUTREF,STYLESREF,INDENT)  ;
	DO ADDLINE(OUTREF,"<!DOCTYPE html>",INDENT)
	DO ADDLINE(OUTREF,"<html lang='en'>",INDENT)
	DO ADDLINE(OUTREF,"<head>",INDENT) DO INDENT(.INDENT)
	DO ADDLINE(OUTREF,"<meta charset='UTF-8'>",INDENT)
	DO ADDLINE(OUTREF,"<title>Patient-generated Information</title>",INDENT)
	DO ADDLINE(OUTREF,"<link rel='preconnect' href='https://fonts.googleapis.com'>",INDENT)
	DO ADDLINE(OUTREF,"<link rel='preconnect' href='https://fonts.gstatic.com' crossorigin>",INDENT)
	DO ADDLINE(OUTREF,"<link href='https://fonts.googleapis.com/css2?family=Delius+Swash+Caps&display=swap' rel='stylesheet'>",INDENT)
	;
	DO SETUPSTYLES(OUTREF,STYLESREF,.INDENT)
	DO OUTDENT(.INDENT) DO ADDLINE(OUTREF,"</head>",INDENT)
	DO ADDLINE(OUTREF,"<body>",INDENT) DO INDENT(.INDENT)
	DO GETPATIENTINFO(TMGDFN,OUTREF,STYLESREF,INDENT)
	QUIT
	;
GETHTMLEND(OUTREF,INDENT)  ;
	DO OUTDENT(.INDENT) DO ADDLINE(OUTREF,"</body>",INDENT)
	DO OUTDENT(.INDENT) DO ADDLINE(OUTREF,"</html>",INDENT)
	QUIT
	;
GETPATIENTINFO(TMGDFN,OUTREF,STYLESREF,INDENT)	;
	NEW ZN SET ZN=$GET(^DPT(TMGDFN,0))
	NEW NAME SET NAME=$PIECE(ZN,"^",1)
	DO STDNAME^XLFNAME(.NAME,"GC")
	SET NAME=$GET(NAME("GIVEN"))
	IF $GET(NAME("MIDDLE"))'="" SET NAME=NAME_" "_NAME("MIDDLE")
	SET NAME=NAME_" "_$GET(NAME("FAMILY"))
	IF $GET(NAME("SUFFIX"))'="" SET NAME=NAME_" "_NAME("SUFFIX")
	NEW DOB SET DOB=$PIECE(ZN,"^",3)
	SET DOB=$$FMTE^XLFDT(DOB,"5D")
	DO ADDLINE(OUTREF,"<h1>"_NAME_" (DOB: "_DOB_")</h1>",INDENT)
	DO ADDLINE(OUTREF,"NOTE: This document was generated automatically from patient's responses to pre-visit questionnaires",INDENT)
	QUIT
	;
GETCOLORS(STYLESREF) ;
		;
	SET @STYLESREF@(":root","--lightGreen")="              #00a36c"
	SET @STYLESREF@(":root","--okGreen")="                 #28a745"
	SET @STYLESREF@(":root","--darkerGreen")="             #228b22"
	SET @STYLESREF@(":root","--skyBlue")="                 #cce5ff"
	SET @STYLESREF@(":root","--niceBlue")="                #3498db"
	SET @STYLESREF@(":root","--darkerNiceBlue")="          #2980b9"
	SET @STYLESREF@(":root","--darkerBlue")="              #0505aa"
	SET @STYLESREF@(":root","--lateNightBlue")="           #004085"
	SET @STYLESREF@(":root","--grayBlue")="                #2c3e50"
	SET @STYLESREF@(":root","--redish")="                  #e74c3c"
	SET @STYLESREF@(":root","--red")="                     #b70505"
	SET @STYLESREF@(":root","--darkerRed")="               #790e0e"
	SET @STYLESREF@(":root","--midnightRed")="             #441313"
	SET @STYLESREF@(":root","--incompleteRed")="           #ffe0e0"
	SET @STYLESREF@(":root","--incompleteRedDarker")="     #f0c0c0"
	SET @STYLESREF@(":root","--darkGray")="                #333333"
	SET @STYLESREF@(":root","--lighterDarkGray")="         #7c7c7cff"
	SET @STYLESREF@(":root","--darkShadowColor")="         rgba(0,0,0,0.6)"
	SET @STYLESREF@(":root","--medShadowColor")="          rgba(0,0,0,0.4)"
	SET @STYLESREF@(":root","--shadowColor")="             rgba(0,0,0,0.2)"
	SET @STYLESREF@(":root","--gray")="                    #9d9d9d"
	SET @STYLESREF@(":root","--medGray")="                 #ccc"
	SET @STYLESREF@(":root","--lightGray")="               #e2e2e2"
	SET @STYLESREF@(":root","--lightLightGray")="          #f0f0f0"
	SET @STYLESREF@(":root","--transparentGray")="         rgba(0, 0, 0, 0.05)"
	SET @STYLESREF@(":root","--whiteColor")="              #fcfcfcff"
	SET @STYLESREF@(":root","--offBlueWhite")="            #f8f9fa"
	SET @STYLESREF@(":root","--wispyYellow")="             #ffff004a"
	SET @STYLESREF@(":root","--dirtyYellow")="             #ecea85"
	SET @STYLESREF@(":root","--lightYellow")="             #ffe16e"
	SET @STYLESREF@(":root","--shadowGold")="              #856404"
	;
	SET @STYLESREF@(":root","--textColor")="               var(--grayBlue)"
	SET @STYLESREF@(":root","--darkBlueTextColor")="       var(--lateNightBlue)"
	SET @STYLESREF@(":root","--windowRxBackground")="      linen"
	SET @STYLESREF@(":root","--genericRxColor")="          var(--niceBlue)"
	SET @STYLESREF@(":root","--brandRxColor")="            var(--darkerNiceBlue)"
	SET @STYLESREF@(":root","--strengthRxColor")="         var(--darkerRed)"
	SET @STYLESREF@(":root","--unitsRxColor")="            var(--darkGray)"
	SET @STYLESREF@(":root","--formRxColor")="             var(--darkerRed)"
	SET @STYLESREF@(":root","--sigRxColor")="              var(--gray)"
	SET @STYLESREF@(":root","--noteRxColor")="             var(--darkerBlue)"
	SET @STYLESREF@(":root","--otcRxColor")="              var(--red)"
	SET @STYLESREF@(":root","--prefaceRxColor")="          var(--darkerBlue)"
	SET @STYLESREF@(":root","--unparsedRxColor")="         var(--whiteColor)"
	;
	SET @STYLESREF@(":root","--yesGreen")="                var(--lightGreen)"
	SET @STYLESREF@(":root","--noRed")="                   var(--redish)"
	SET @STYLESREF@(":root","--otherYellow")="             var(--wispyYellow)"
	SET @STYLESREF@(":root","--unansweredGray")="          var(--medGray)"
	;
	SET @STYLESREF@(":root","--bodyBackground")="          var(--offBlueWhite)"
	SET @STYLESREF@(":root","--hrColor")="                 var(--lightGray)"
	SET @STYLESREF@(":root","--tableBackground")="         var(--whiteColor)"
	SET @STYLESREF@(":root","--tableHeaderColor")="        var(--niceBlue)"
	;
	SET @STYLESREF@(":root","--scoreBackground")="         var(--wispyYellow)"
	SET @STYLESREF@(":root","--scoreBorder")="             var(--lightYellow)"
	SET @STYLESREF@(":root","--scoreColor")="              var(--shadowGold)"
	;
	SET @STYLESREF@(":root","--refillYesBackground")="     var(--wispyYellow)"
	SET @STYLESREF@(":root","--refillNoBackground")="      var(--windowRxBackground)"
	SET @STYLESREF@(":root","--refillLocBackground")="     var(--dirtyYellow)"
	;
	SET @STYLESREF@(":root","--commentsBackground")="      var(--windowRxBackground)"
	SET @STYLESREF@(":root","--commentsPresentBackground")="var(--wispyYellow)"
	;
	QUIT
	;
GETCOMMONSTYLES(STYLESREF)  ;
	SET @STYLESREF@(":root","--medium")="0.8em"
	SET @STYLESREF@(":root","--smaller")="0.6em"
	SET @STYLESREF@(":root","--tiny")="0.4em"
	;
	SET @STYLESREF@("body","font-family")="'Segoe UI', Tahoma, Geneva, Verdana, sans-serif"
	SET @STYLESREF@("body","margin")="0"
	SET @STYLESREF@("body","padding")="2rem"
	SET @STYLESREF@("body","background-color")="var(--bodyBackground)"
	SET @STYLESREF@("body","color")="var(--textColor)"
	;
	SET @STYLESREF@("h1","font-size")="2.2rem"
	SET @STYLESREF@("h1","margin-top")="2rem"
	SET @STYLESREF@("h1","margin-bottom")="0.5rem"
	SET @STYLESREF@("h1","color")="var(--darkBlueTextColor)"
	SET @STYLESREF@("h1","border-bottom")="3px solid var(--skyblue)"
	SET @STYLESREF@("h1","padding-bottom")="0.3rem"
	;
	SET @STYLESREF@("h2","font-size")="1.4rem"
	SET @STYLESREF@("h2","font-weight")="normal"
	SET @STYLESREF@("h2","margin-bottom")="1rem"
	SET @STYLESREF@("h2","color")="var(--darkBlueTextColor)"
	;
	SET @STYLESREF@("hr","height")="10px"
	SET @STYLESREF@("hr","background-color")="var(--hrColor)"
		;
		;"Table styling
	SET @STYLESREF@("table","width")="100%"
	SET @STYLESREF@("table","border-collapse")="collapse"
	SET @STYLESREF@("table","margin-bottom")="1.5rem"
	SET @STYLESREF@("table","background")="var(--tableBackground)"
	SET @STYLESREF@("table","box-shadow")="2px 4px 10px black"
	SET @STYLESREF@("table","border-radius")="8px"
	SET @STYLESREF@("table","overflow")="hidden"
	;
	SET @STYLESREF@("th","background-color")="var(--tableHeaderColor)"
	SET @STYLESREF@("th","color")="white"
	SET @STYLESREF@("th","text-align")="left"
	SET @STYLESREF@("th","padding")="0.75rem"
	SET @STYLESREF@("th","font-size")="1rem"
	;
	;"SET @STYLESREF@("td","padding")="0.75rem"
	SET @STYLESREF@("td","border")="10px solid var(--tableBackground)"
	SET @STYLESREF@("td","vertical-align")="top"
	;
	SET @STYLESREF@(".comments","background-color")="var(--commentsBackground)"
	SET @STYLESREF@(".comments","text-align")="center"
	SET @STYLESREF@(".comments","vertical-align")="middle"
	SET @STYLESREF@(".comments","font-family")="'Delius Swash Caps', cursive, sans-serif"
	SET @STYLESREF@(".comments","font-size")="1.25em"
	SET @STYLESREF@(".comments","border-radius")="10px"
	SET @STYLESREF@(".comments:not(:empty)","background-color")="var(--commentsPresentBackground)"
	;
	SET @STYLESREF@(".info-subtable","background-color")="var(--windowRxBackground)"
	SET @STYLESREF@(".info-subtable","width")="100%"
	SET @STYLESREF@(".info-subtable","border-collapse")="collapse"
	SET @STYLESREF@(".info-subtable","margin")="0px"
	SET @STYLESREF@(".info-subtable","overflow")="hidden"
	SET @STYLESREF@(".info-subtable","box-shadow")="none"
	SET @STYLESREF@(".info-subtable","padding")="5px 5px"
	;
	SET @STYLESREF@(".info-subtable td","border")="none"
	SET @STYLESREF@(".info-subtable td","padding")="5px 5px"
	;
	;" Make empty questions less distracting
	SET @STYLESREF@("td.question:empty","background-color")="var(--tableBackground)"
	;
	;" Responsive tweaks
	SET @STYLESREF@("@media (max-width: 768px)","table, th, td","font-size")="0.9rem"
	SET @STYLESREF@("@media (max-width: 768px)",".question","width")="100%"
	SET @STYLESREF@("@media (max-width: 768px)",".question","display")="block"
	SET @STYLESREF@("@media (max-width: 768px)","td, th","display")="block"
	SET @STYLESREF@("@media (max-width: 768px)","td, th","text-align")="left"
	;
	QUIT
	;
QDATA2TABL(OUTREF,DATAREF,STYLESREF,INDENT)	;"Format questionnaire data into HTML table
	;
	DO GETCOLORS(STYLESREF)
	DO GETCOMMONSTYLES(STYLESREF)
	;
	;
	;" Zebra striping
	SET @STYLESREF@("tr:nth-child(even) td:first-child","background-color")="var(--offBlueWhite)"
	;
	;" Question and answer colors
	SET @STYLESREF@(".question","font-weight")="bold"
	SET @STYLESREF@(".question","color")="(var--darkBlueTextColor)"
	SET @STYLESREF@(".question","width")="40%"
	;
	SET @STYLESREF@(".patient-reply","color")="var(--textColor)"
	SET @STYLESREF@(".patient-reply","width")="30%"
	SET @STYLESREF@(".patient-reply","text-align")="center"
	;"SET @STYLESREF@(".patient-reply","font-family")="'Pacifico', cursive, sans-serif"
	SET @STYLESREF@(".patient-reply","font-family")="'Delius Swash Caps', cursive, sans-serif"
	SET @STYLESREF@(".patient-reply","font-weight")="400"
	SET @STYLESREF@(".patient-reply","font-style")="normal"
	SET @STYLESREF@(".patient-reply","font-size")="1.2rem"
	;
	SET @STYLESREF@(".answer-is-none","background-color")="var(--yesGreen)"
	SET @STYLESREF@(".answer-is-other","background-color")="var(--otherYellow)"
	SET @STYLESREF@(".question-unanswered","background-color")="var(--unansweredGray)"
	;
	;" Highlighting total score
	SET @STYLESREF@(".total-score","font-weight")="bold"
	SET @STYLESREF@(".total-score","background-color")="var(--scoreBackground)"
	SET @STYLESREF@(".total-score","border")="1px solid var(--scoreBorder)"
	SET @STYLESREF@(".total-score","color")="var(--scoreColor)"
	SET @STYLESREF@(".total-score","padding")="1rem"
	SET @STYLESREF@(".total-score","border-radius")="8px"
	SET @STYLESREF@(".total-score","box-shadow")="0 1px 3px var(--transparentGray)"
	SET @STYLESREF@(".total-score","margin-bottom")="1.5rem"
	;
	NEW GRPIDX SET GRPIDX=""
	FOR  SET GRPIDX=$ORDER(@DATAREF@("questGroupsResults",GRPIDX)) QUIT:GRPIDX=""  DO
	. NEW HEADINGTXT SET HEADINGTXT=$GET(@DATAREF@("questGroupsResults",GRPIDX,"groupHeadingText"))
	. IF HEADINGTXT="" DO
	. . DO ADDLINE(OUTREF,"<h2>"_HEADINGTXT_"</h2>",INDENT)
	. DO ADDLINE(OUTREF,"<table>",INDENT) DO INDENT(.INDENT)
	. DO ADDLINE(OUTREF,"<th class='question'>Question</th><th class='patient-reply'>Patient Reply</th>",INDENT)
	. NEW QSTIDX SET QSTIDX=""
	. FOR  SET QSTIDX=$ORDER(@DATAREF@("questGroupsResults",GRPIDX,"questionResults",QSTIDX)) QUIT:QSTIDX=""  DO
	. . DO ADDLINE(OUTREF,"<tr>",INDENT) DO INDENT(.INDENT)
	. . NEW QUESTIONTXT SET QUESTIONTXT=$GET(@DATAREF@("questGroupsResults",GRPIDX,"questionResults",QSTIDX,"questionText"))
	. . NEW CLASS SET CLASS="patient-reply"
	. . NEW REPLYTXT SET REPLYTXT=$GET(@DATAREF@("questGroupsResults",GRPIDX,"questionResults",QSTIDX,"value"))
	. . IF REPLYTXT["^" SET REPLYTXT=$$REPLSTR^TMGSTUT3(REPLYTXT,"^",", ")
	. . NEW DETAILS SET DETAILS=$GET(@DATAREF@("questGroupsResults",GRPIDX,"questionResults",QSTIDX,"details"))
	. . IF DETAILS'="" DO
	. . . IF REPLYTXT'="" SET REPLYTXT=REPLYTXT_" -- Details: "
	. . . SET REPLYTXT=REPLYTXT_""""_DETAILS_""""
	. . IF REPLYTXT="" SET REPLYTXT="(Not Answered)",CLASS=CLASS_" question-unanswered"
	. . NEW UPREPLY SET UPREPLY=$$UP^XLFSTR(REPLYTXT)
	. . ELSE  IF "^NO^NONE^N/A^"[("^"_UPREPLY_"^") SET CLASS=CLASS_" answer-is-none"
	. . ELSE  SET CLASS=CLASS_" answer-is-other"
	. . DO ADDLINE(OUTREF,"<td class='question'>"_QUESTIONTXT_"</td>",INDENT)
	. . DO ADDLINE(OUTREF,"<td class='"_CLASS_"'>"_REPLYTXT_"</td>",INDENT)
	. . DO OUTDENT(.INDENT) DO ADDLINE(OUTREF,"</tr>",INDENT)
	. DO OUTDENT(.INDENT) DO ADDLINE(OUTREF,"</table>",INDENT)
	NEW SCORING SET SCORING=$GET(@DATAREF@("scoring"))
	IF SCORING="%%bool%%true" DO
	. NEW TOTALSCORE SET TOTALSCORE=$GET(@DATAREF@("totalScore"))
	. DO ADDLINE(OUTREF,"<div class='total-score'>Total Score: "_TOTALSCORE_"</div>",INDENT)
	DO ADDLINE(OUTREF,"<hr>",INDENT)
	QUIT
	;
QUEST2TABL(OUTREF,DATAREF,STYLESREF,TABLENAME,INDENT)	;"Format questionnaire data into HTML table, with wrappings
	;"NOTE: It is NOT the intent to create a full HTML page here.  Just the table and instructions text etc.;
	DO ADDLINE(OUTREF,"<h1>"_TABLENAME_"</h1>",INDENT)
	NEW INSTRUCT SET INSTRUCT=$GET(@DATAREF@("instructionsText"))
	IF INSTRUCT'=""  DO
	. DO ADDLINE(OUTREF,"<h2>"_INSTRUCT_"</h2>",INDENT)
	DO QDATA2TABL(OUTREF,DATAREF,STYLESREF,INDENT)
	NEW ENDINGTEXT SET ENDINGTEXT=$GET(@DATAREF@("endingText"))
	IF ENDINGTEXT'=""  DO
	. DO ADDLINE(OUTREF,"<h2>"_ENDINGTEXT_"</h2>",INDENT)
	QUIT
	;
RXDATA2TABL(OUTREF,DATAREF,STYLESREF,INDENT,MODE)	;"Format medication data into HTML table
	;"Input:  OUTREF -- PASS BY NAME.  Ref of output array. Expected format: @OUTREF@(#)=<LINE>
	;"	      DATAREF -- PASS BY NAME.  Ref of source data.;
	;"		  STYLESREF -- PASS BY NAME.  Ref of array for building up styles information
	;"		  INDENT -- PASS BY REFERENCE.  Used for indenting text for easier reading
	;"        MODE -- OPTIONAL.  If "OTC" then form is for displaying OTC Rx's.;
	;"NOTE: It is NOT the intent to create a full HTML page here.  Just the table
	;
	NEW OTC SET OTC=($GET(MODE)="OTC")
	;
	DO GETCOLORS(STYLESREF)
	DO GETCOMMONSTYLES(STYLESREF)
	;
	SET @STYLESREF@("rx_generic","color")="var(--genericRxColor)"
	SET @STYLESREF@("rx_generic","font-size")="1.5em"
	SET @STYLESREF@("rx_generic","font-weight")="bold"
	;
	SET @STYLESREF@("rx_brand","color")="var(--brandRxColor)"
	SET @STYLESREF@("rx_brand","font-weight")="bold"
	;
	SET @STYLESREF@("rx_mod","color")="var(--modRxColor)"
	;
	SET @STYLESREF@("rx_strength","color")="var(--strengthRxColor)"
	SET @STYLESREF@("rx_strength","font-size")="1em"
	;
	SET @STYLESREF@("rx_units","color")="var(--unitsRxColor)"
	SET @STYLESREF@("rx_units","font-size")="var(--smaller)"
	;
	SET @STYLESREF@("rx_form","color")="var(--formRxColor)"
	SET @STYLESREF@("rx_form","font-size")="var(--smaller)"
	;
	SET @STYLESREF@("rx_sig","color")="var(--midnightRed)"
	SET @STYLESREF@("rx_sig","font-size")="var(--smaller)"
	SET @STYLESREF@("rx_sig","font-weight")="normal"
	SET @STYLESREF@("rx_sig","display")="block"
	;
	SET @STYLESREF@("rx_note","color")="var(--noteRxColor)"
	SET @STYLESREF@("rx_sig","display")="block"
	SET @STYLESREF@("rx_note","font-size")="var(--smaller)"
	;
	SET @STYLESREF@("rx_otc","color")="var(--otcRxColor)"
	SET @STYLESREF@("rx_otc","font-size")="1em"
	;
	SET @STYLESREF@("rx_preface","color")="var(--prefaceRxColor)"
	SET @STYLESREF@("rx_preface","font-size")="var(--smaller)"
	;
	SET @STYLESREF@("rx_unparsed","background-color")="var(--unparsedRxColor)"
	SET @STYLESREF@("rx_unparsed","color")="var(--textColor)"
	SET @STYLESREF@("rx_unparsed","font-size")="var(--medium)"
	;
	SET @STYLESREF@(".rx-table","background-color")="var(--tableBackground)"
	;
	SET @STYLESREF@(".rx-table-head","background-color")="var(--tableHeaderColor)"
	;
	NEW RXPREFIX SET RXPREFIX=$SELECT(OTC=1:"rxotc",1:"rx")
	NEW WIDTH
	IF OTC DO
	. SET WIDTH(1)="60%"
	. SET WIDTH(2)="20%"
	. SET WIDTH(3)="20%"
	ELSE  DO
	. SET WIDTH(1)="50%"
	. SET WIDTH(2)="16.7%"
	. SET WIDTH(3)="16.7%"
	. SET WIDTH(4)="16.7%"
	. ;
	NEW COLNUM SET COLNUM=0
	SET @STYLESREF@("."_RXPREFIX_"-name-header","width")=WIDTH($I(COLNUM))  ;"1
	;
	SET @STYLESREF@("."_RXPREFIX_"-taking-header","width")=WIDTH($I(COLNUM)) ;"2
	SET @STYLESREF@("."_RXPREFIX_"-taking-header","text-align")="center"
	;
	IF OTC=0 DO
	. SET @STYLESREF@("."_RXPREFIX_"-refill-header","width")=WIDTH($I(COLNUM))  ;"3
	. SET @STYLESREF@("."_RXPREFIX_"-refill-header","text-align")="center"
	;
	SET @STYLESREF@("."_RXPREFIX_"-details-header","width")=WIDTH($I(COLNUM))  ;"3 or 4
	SET @STYLESREF@("."_RXPREFIX_"-details-header","text-align")="center"
	;
	;"-- Contents of rx-info-subtable ---"
	SET @STYLESREF@(".rx-name-formatted","padding")="5px 5px"
	SET @STYLESREF@(".rx-name-formatted","font-size")="1em"
	SET @STYLESREF@(".rx-name-formatted","border-bottom")="none"
	;
	SET @STYLESREF@(".rx-strength","padding")="5px 5px"
	SET @STYLESREF@(".rx-strength","font-size")="1em"
	SET @STYLESREF@(".rx-strength","border-bottom")="none"
	;
	SET @STYLESREF@(".rx-sig","padding")="5px 5px"
	SET @STYLESREF@(".rx-sig","font-size")="0.9em"
	SET @STYLESREF@(".rx-sig","border-bottom")="none"
	;
	SET @STYLESREF@(".rx-original","font-family")="monospace"
	SET @STYLESREF@(".rx-original","font-size")="0.75em"
	SET @STYLESREF@(".rx-original","font-style")="italic"
	SET @STYLESREF@(".rx-original","color")="gray"
	SET @STYLESREF@(".rx-original","padding")="5px 5px"
	SET @STYLESREF@(".rx-original","border-bottom")="none"
	;"-- end of contents of rx-in-subtable ---"
	;
	SET @STYLESREF@(".rx-taking","text-align")="center"
	SET @STYLESREF@(".rx-taking","vertical-align")="middle"
	SET @STYLESREF@(".rx-taking","font-family")="'Delius Swash Caps', cursive, sans-serif"
	SET @STYLESREF@(".rx-taking","font-size")="1.25em"
	SET @STYLESREF@(".rx-taking","border-radius")="10px"
	;
	SET @STYLESREF@(".rx-taking-yes","background-color")="var(--yesGreen)"
	SET @STYLESREF@(".rx-taking-yes","color")="white"
	;
	SET @STYLESREF@(".rx-taking-no","background-color")="var(--noRed)"
	SET @STYLESREF@(".rx-taking-no","color")="white"
	;
	SET @STYLESREF@(".rx-refill","text-align")="center"
	SET @STYLESREF@(".rx-refill","vertical-align")="middle"
	SET @STYLESREF@(".rx-refill","font-family")="'Delius Swash Caps', cursive, sans-serif"
	SET @STYLESREF@(".rx-refill","font-size")="1.25em"
	SET @STYLESREF@(".rx-refill","border-radius")="10px"
	;
	SET @STYLESREF@(".rx-refill-yes","background-color")="var(--refillYesBackground)"
	;
	SET @STYLESREF@(".rx-refill-no","background-color")="var(--refillNoBackground)"
	;
	SET @STYLESREF@(".rx-refill-needed-subtable","margin-bottom")="0px"
	SET @STYLESREF@(".rx-refill-needed-subtable td","border")="none"
	;
	SET @STYLESREF@(".rx-refill-location","background-color")="var(--refillLocBackground)"
	;
	DO ADDLINE(OUTREF,"<table class='rx-table'>",INDENT) DO INDENT(.INDENT)
	DO ADDLINE(OUTREF,"<thead class='rx-table-head'>",INDENT) DO INDENT(.INDENT)
	DO ADDLINE(OUTREF,"<th class='"_RXPREFIX_"-name-header'>Medication</th>",INDENT)
	DO ADDLINE(OUTREF,"<th class='"_RXPREFIX_"-taking-header'>Taking?</th>",INDENT)
	IF OTC=0 DO
	. DO ADDLINE(OUTREF,"<th class='"_RXPREFIX_"-refill-header'>Refill?</th>",INDENT)
	DO ADDLINE(OUTREF,"<th class='"_RXPREFIX_"-details-header'>Details</th>",INDENT)
	DO OUTDENT(.INDENT) DO ADDLINE(OUTREF,"</thead>",INDENT)
	DO ADDLINE(OUTREF,"<tbody class='rx-table-body'>",INDENT) DO INDENT(.INDENT)
	;"Add row for each medication
	NEW RXIDX SET RXIDX=""
	FOR  SET RXIDX=$ORDER(@DATAREF@(RXIDX)) QUIT:RXIDX=""  DO
	. NEW RXINFO MERGE RXINFO=@DATAREF@(RXIDX)
	. ;"FORMAT OF RXINFO:
	. ;"  text: string | null; 				// The original medication name
	. ;"  parsed: string | null; 			// A parsed version of medication, with tags for style.;
	. ;"  otc : number | null;  			//should be 0 for false, or 1 for true
	. ;"  areTaking: AreTakingStatus; 		// 'yes', 'no', 'sometimes', 'unknown'
	. ;"  needsRefill: YesNoStatus; 		// 'yes', 'no', or null (conditional)
	. ;"  refillLocation: RefillLocation; 	// 'local', 'mail' (conditional)
	. ;"  comment: string | null; 			// Any additional comments or notes about the medication
	. ;"  isComplete: boolean | null; 		// Indicates if the allergy review is complete
	. ;
	. DO ADDLINE(OUTREF,"<tr>",INDENT) DO INDENT(.INDENT)
	. ;"--- Start of column 1 ---"
	. DO ADDLINE(OUTREF,"<td>",INDENT) DO INDENT(.INDENT)
	. ;"========================================================"
	. ;"Add sub table inside 1st cell to hold 3 lines for info
	. DO ADDLINE(OUTREF,"<table class='info-subtable'>",INDENT) DO INDENT(.INDENT)
	. NEW TEMP SET TEMP=$GET(RXINFO("parsed"))
	. NEW RXMAIN,RXSIG SET RXMAIN=$PIECE(TEMP,"<rx_sig>",1),RXSIG="<rx_sig>"_$PIECE(TEMP,"<rx_sig>",2,99)
	. NEW RXSTRENGTH SET RXSTRENGTH="<rx_strength>"_$PIECE(RXMAIN,"<rx_strength>",2,99)
	. SET RXMAIN=$PIECE(RXMAIN,"<rx_strength>",1)
	. ;"--- sub-row 1 => Parsed (formatted) medication name ---
	. DO ADDLINE(OUTREF,"<tr>",INDENT) DO INDENT(.INDENT)
	. DO ADDLINE(OUTREF,"<td class='rx-name-formatted'>",INDENT) DO INDENT(.INDENT)
	. DO ADDLINE(OUTREF,RXMAIN,INDENT)
	. DO OUTDENT(.INDENT) DO ADDLINE(OUTREF,"</td>",INDENT)
	. DO OUTDENT(.INDENT) DO ADDLINE(OUTREF,"</tr>",INDENT)
	. ;"--- End of sub-row 1 ------
	. ;"--- sub-row 2 => Strength ---
	. DO ADDLINE(OUTREF,"<tr>",INDENT) DO INDENT(.INDENT)
	. DO ADDLINE(OUTREF,"<td class='rx-strength'>",INDENT) DO INDENT(.INDENT)
	. DO ADDLINE(OUTREF,RXSTRENGTH,INDENT)
	. DO OUTDENT(.INDENT) DO ADDLINE(OUTREF,"</td>",INDENT)
	. DO OUTDENT(.INDENT) DO ADDLINE(OUTREF,"</tr>",INDENT)
	. ;"--- End of sub-row 2 ------
	. ;"--- sub-row 3 => Sig (instructions) ---
	. DO ADDLINE(OUTREF,"<tr>",INDENT) DO INDENT(.INDENT)
	. DO ADDLINE(OUTREF,"<td class='rx-sig'>",INDENT) DO INDENT(.INDENT)
	. DO ADDLINE(OUTREF,RXSIG,INDENT)
	. DO OUTDENT(.INDENT) DO ADDLINE(OUTREF,"</td>",INDENT)
	. DO OUTDENT(.INDENT) DO ADDLINE(OUTREF,"</tr>",INDENT)
	. ;"--- End of sub-row 3 ------
	. ;"--- sub-row 4 => Original Rx line ---
	. DO ADDLINE(OUTREF,"<tr>",INDENT) DO INDENT(.INDENT)
	. DO ADDLINE(OUTREF,"<td class='rx-original'>",INDENT) DO INDENT(.INDENT)
	. NEW ORIGINALRX SET ORIGINALRX=$GET(RXINFO("text"))
	. IF ORIGINALRX'=""  SET ORIGINALRX="Original: "_ORIGINALRX
	. DO ADDLINE(OUTREF,ORIGINALRX,INDENT)
	. DO OUTDENT(.INDENT) DO ADDLINE(OUTREF,"</td>",INDENT)
	. DO OUTDENT(.INDENT) DO ADDLINE(OUTREF,"</tr>",INDENT)
	. ;"--- End of sub-row 4 ------
	. DO OUTDENT(.INDENT) DO ADDLINE(OUTREF,"</table>",INDENT)
	. ;"end of sub table in column 1
	. ;"========================================================"
	. DO OUTDENT(.INDENT) DO ADDLINE(OUTREF,"</td>",INDENT)
	. ;"end of 1st cell (column 1)"
	. NEW VALUE SET VALUE=$GET(RXINFO("areTaking"))
	. NEW TAKINGCLASS SET TAKINGCLASS=$SELECT(VALUE="yes":"rx-taking-yes",1:"rx-taking-no")
	. SET VALUE=$$CAMELCASE(VALUE)
	. ;"Start of column 2
	. DO ADDLINE(OUTREF,"<td class='rx-taking "_TAKINGCLASS_"'>"_VALUE_"</td>",INDENT)
	. ;"End of column 2
	. ;"Start of column 3
	. IF OTC=0 DO
	. . SET VALUE=$GET(RXINFO("needsRefill"))
	. . IF VALUE="%%null%%" SET VALUE=""
	. . NEW REFILLCLASS SET REFILLCLASS=$SELECT(VALUE="yes":"rx-refill-yes",1:"rx-refill-no")
	. . DO ADDLINE(OUTREF,"<td class='rx-refill "_REFILLCLASS_"'>",INDENT) DO INDENT(.INDENT)
	. . IF VALUE="yes" DO
	. . . ;"========================================================"
	. . . ;"Add sub table inside 3rd cell to hold 2 lines for Refill info.;
	. . . DO ADDLINE(OUTREF,"<table class='rx-refill-needed-subtable'>",INDENT) DO INDENT(.INDENT)
	. . . ;"--- sub-row 1 => yes ---
	. . . SET VALUE=$$CAMELCASE(VALUE)
	. . . DO ADDLINE(OUTREF,"<tr><td class='"_REFILLCLASS_"'>"_VALUE_"</td></tr>",INDENT)
	. . . ;"--- End of sub-row 1 ------
	. . . ;"--- sub-row 2 => location---
	. . . DO ADDLINE(OUTREF,"<tr><td class='rx-refill-location'>"_$GET(RXINFO("refillLocation"))_"</td></tr>",INDENT)
	. . . ;"--- End of sub-row 2 ------
	. . . DO OUTDENT(.INDENT) DO ADDLINE(OUTREF,"</table>",INDENT)
	. . . ;"end of sub table in column 3
	. . ELSE  DO  ;"no
	. . . SET VALUE=$$CAMELCASE(VALUE)
	. . . DO ADDLINE(OUTREF,VALUE,INDENT)
	. . DO OUTDENT(.INDENT) DO ADDLINE(OUTREF,"</td>",INDENT)
	. ;"End of column 3
	. ;"Start of column 4  (actually, this will be column 3 if OTC form)
	. SET VALUE=$GET(RXINFO("comment"))
	. DO ADDLINE(OUTREF,"<td class='comments'>"_VALUE_"<td>",INDENT)
	. ;"End of column 4
	. ;"End of row for medication entry
	. DO OUTDENT(.INDENT) DO ADDLINE(OUTREF,"</tr>",INDENT)
	. QUIT  ;"done with one medication entry
	;"End of entire medication table
	DO OUTDENT(.INDENT) DO ADDLINE(OUTREF,"</tbody>",INDENT)
	DO OUTDENT(.INDENT) DO ADDLINE(OUTREF,"</table>",INDENT)
	QUIT
	;
RX2TABL(OUTREF,DATAREF,STYLESREF,TABLENAME,INDENT,MODE)	;"Format questionnaire data into HTML table, with wrappings
	;"NOTE: It is NOT the intent to create a full HTML page here.  Just the table and instructions text etc.;
	;
	DO ADDLINE(OUTREF,"<h1>"_TABLENAME_"</h1>",INDENT)
	DO RXDATA2TABL(OUTREF,DATAREF,STYLESREF,INDENT,.MODE)
	QUIT
	;
ALRGYDATA2TABL(OUTREF,DATAREF,STYLESREF,INDENT)	;"Format allergy data into HTML table
	;"Input:  OUTREF -- PASS BY NAME.  Ref of output array. Expected format: @OUTREF@(#)=<LINE>
	;"	      DATAREF -- PASS BY NAME.  Ref of source data.;
	;"		  STYLESREF -- PASS BY NAME.  Ref of array for building up styles information
	;"		  INDENT -- PASS BY REFERENCE.  Used for indenting text for easier reading
	;"NOTE: It is NOT the intent to create a full HTML page here.  Just the table
	;
	DO GETCOLORS(STYLESREF)
	DO GETCOMMONSTYLES(STYLESREF)
	;
	SET @STYLESREF@(".alrgy-table","background-color")="var(--tableBackground)"
	;
	SET @STYLESREF@(".alrgy-table-head","background-color")="var(--tableHeaderColor)"
	;
	SET @STYLESREF@(".alrgy-name-header","width")="50%"
	;
	SET @STYLESREF@(".alrgy-active-header","width")="25%"
	SET @STYLESREF@(".alrgy-active-header","text-align")="center"
	;
	SET @STYLESREF@(".alrgy-details-header","width")="25%"
	SET @STYLESREF@(".alrgy-details-header","text-align")="center"
	;
	;"-- Contents of alrgy-info-subtable ---"
	SET @STYLESREF@(".alrgy-name","padding")="5px 5px"
	SET @STYLESREF@(".alrgy-name","font-size")="1em"
	SET @STYLESREF@(".alrgy-name","border-bottom")="none"
	;
	SET @STYLESREF@(".alrgy-reaction","padding")="5px 5px"
	SET @STYLESREF@(".alrgy-reaction","font-size")="1em"
	SET @STYLESREF@(".alrgy-reaction","border-bottom")="none"
	;
	SET @STYLESREF@(".alrgy-date","padding")="5px 5px"
	SET @STYLESREF@(".alrgy-date","font-size")="0.9em"
	SET @STYLESREF@(".alrgy-date","border-bottom")="none"
	;
	SET @STYLESREF@(".alrgy-active","text-align")="center"
	SET @STYLESREF@(".alrgy-active","vertical-align")="middle"
	SET @STYLESREF@(".alrgy-active","font-family")="'Delius Swash Caps', cursive, sans-serif"
	SET @STYLESREF@(".alrgy-active","font-size")="1.25em"
	SET @STYLESREF@(".alrgy-active","border-radius")="10px"
	;
	SET @STYLESREF@(".alrgy-active-yes","background-color")="var(--yesGreen)"
	SET @STYLESREF@(".alrgy-active-yes","color")="white"
	;
	SET @STYLESREF@(".alrgy-active-no","background-color")="var(--noRed)"
	SET @STYLESREF@(".alrgy-active-no","color")="white"
	;
	;"--- START HTML OUTPUT ---
	DO ADDLINE(OUTREF,"<table class='alrgy-table'>",INDENT) DO INDENT(.INDENT)
	DO ADDLINE(OUTREF,"<thead class='alrgy-table-head'>",INDENT) DO INDENT(.INDENT)
	DO ADDLINE(OUTREF,"<th class='alrgy-name-header'>Medication/Item</th>",INDENT)
	DO ADDLINE(OUTREF,"<th class='alrgy-active-header'>Still Allergic?</th>",INDENT)
	DO ADDLINE(OUTREF,"<th class='alrgy-details-header'>Details</th>",INDENT)
	DO OUTDENT(.INDENT) DO ADDLINE(OUTREF,"</thead>",INDENT)
	DO ADDLINE(OUTREF,"<tbody>",INDENT) DO INDENT(.INDENT)
	;"Add row for each allergy reaction
	NEW ALRGYIDX SET ALRGYIDX=""
	FOR  SET ALRGYIDX=$ORDER(@DATAREF@(ALRGYIDX)) QUIT:ALRGYIDX=""  DO
	. NEW ALRGYINFO MERGE ALRGYINFO=@DATAREF@(ALRGYIDX)
	. ;"FORMAT of ALRGYINFO:"
	. ;"  text: string | null; 				// The Rx or item that causes allergy
	. ;"  nkda?: boolean; 					// If patient is nkda (no known drug allergies)
	. ;"  neverAssessed?: boolean;			// If allergies have never been asked or assessed
	. ;"  date?: string; 					// The date the allergy was entered or recorded
	. ;"  reaction?: string; 				// Comments (if any) about the reaction
	. ;"  patientResponse: YesNoUnknownStatus; // 'yes', 'no', 'sometimes', 'unknown'
	. ;"  comment: string | null; 			// Any additional comments or notes about the medication
	. ;"  isComplete: boolean | null; 		// Indicates if the allergy review is complete
	. ;
	. ;"TO DO!!: Handle NKDA or never checked..."
	. ;
	. DO ADDLINE(OUTREF,"<tr>",INDENT) DO INDENT(.INDENT)
	. ;"--- Start of column 1 ---"
	. DO ADDLINE(OUTREF,"<td>",INDENT) DO INDENT(.INDENT)
	. ;"========================================================"
	. ;"Add sub table inside 1st cell to hold 3 lines for info
	. DO ADDLINE(OUTREF,"<table class='info-subtable'>",INDENT) DO INDENT(.INDENT)
	. ;"--- sub-row 1 => Allergy medication or item name ---
	. DO ADDLINE(OUTREF,"<tr>",INDENT) DO INDENT(.INDENT)
	. DO ADDLINE(OUTREF,"<td class='alrgy-name'>",INDENT) DO INDENT(.INDENT)
	. DO ADDLINE(OUTREF,$GET(ALRGYINFO("text")),INDENT)
	. DO OUTDENT(.INDENT) DO ADDLINE(OUTREF,"</td>",INDENT)
	. DO OUTDENT(.INDENT) DO ADDLINE(OUTREF,"</tr>",INDENT)
	. ;"--- End of sub-row 1 ------
	. ;"--- sub-row 2 => Reaction ---
	. DO ADDLINE(OUTREF,"<tr>",INDENT) DO INDENT(.INDENT)
	. DO ADDLINE(OUTREF,"<td class='alrgy-reaction'>",INDENT) DO INDENT(.INDENT)
	. DO ADDLINE(OUTREF,$GET(ALRGYINFO("reaction")),INDENT)
	. DO OUTDENT(.INDENT) DO ADDLINE(OUTREF,"</td>",INDENT)
	. DO OUTDENT(.INDENT) DO ADDLINE(OUTREF,"</tr>",INDENT)
	. ;"--- End of sub-row 2 ------
	. ;"--- sub-row 3 => Recorded date ---
	. DO ADDLINE(OUTREF,"<tr>",INDENT) DO INDENT(.INDENT)
	. DO ADDLINE(OUTREF,"<td class='alrgy-date'>",INDENT) DO INDENT(.INDENT)
	. DO ADDLINE(OUTREF,"Recorded Date: "_$GET(ALRGYINFO("date")),INDENT)
	. DO OUTDENT(.INDENT) DO ADDLINE(OUTREF,"</td>",INDENT)
	. DO OUTDENT(.INDENT) DO ADDLINE(OUTREF,"</tr>",INDENT)
	. ;"--- End of sub-row 3 ------
	. DO OUTDENT(.INDENT) DO ADDLINE(OUTREF,"</table>",INDENT)
	. ;"end of sub table in column 1
	. ;"========================================================"
	. DO OUTDENT(.INDENT) DO ADDLINE(OUTREF,"</td>",INDENT)
	. ;"end of 1st cell (column 1)"
	. NEW VALUE SET VALUE=$GET(ALRGYINFO("patientResponse"))
	. NEW ACTIVECLASS SET ACTIVECLASS=$SELECT(VALUE="yes":"alrgy-active-yes",1:"alrgy-active-no")
	. SET VALUE=$$CAMELCASE(VALUE)
	. ;"Start of column 2
	. DO ADDLINE(OUTREF,"<td class='alrgy-active "_ACTIVECLASS_"'>"_VALUE_"</td>",INDENT)
	. ;"End of column 2
	. ;"Start of column 3
	. SET VALUE=$GET(ALRGYINFO("comment"))
	. DO ADDLINE(OUTREF,"<td class='comments'>"_VALUE_"<td>",INDENT)
	. ;"End of column 4
	. ;"End of row for allergy entry
	. DO OUTDENT(.INDENT) DO ADDLINE(OUTREF,"</tr>",INDENT)
	. QUIT  ;"done with one allergy entry
	;"End of entire allergy table
	DO OUTDENT(.INDENT) DO ADDLINE(OUTREF,"</tbody>",INDENT)
	DO OUTDENT(.INDENT) DO ADDLINE(OUTREF,"</table>",INDENT)
	QUIT
	;
ALRGY2TABL(OUTREF,DATAREF,STYLESREF,TABLENAME,INDENT)	;"Format data into HTML table, with wrappings
	;"NOTE: It is NOT the intent to create a full HTML page here.  Just the table and instructions text etc.;
	;
	DO ADDLINE(OUTREF,"<h1>"_TABLENAME_"</h1>",INDENT)
	DO ALRGYDATA2TABL(OUTREF,DATAREF,STYLESREF,INDENT)
	QUIT
	;
	;"================================================================================"
	;
WTHX(REF,TMGDFN,OUTREF,STYLESREF,INDENT,ERR)  ;"
	;"INPUT: REF -- passed by NAME.  Reference for data note holding user replies
	;"       TMGDFN -- patient IEN
	;"       OUTREF -- Name of array to put text into for output.;
	;"	     STYLESREF -- Name of array to hold composit style information
	;"	     INDENT -- pass by REFERENCE.  A variable for holding indent amount for nice formatting of html code
	;"	     ERR -- passed by reference.  An OUT parameter for error messages
	;"RESULT: None
	;"NOTE: It is NOT the intent to create a full HTML page here.  Just the table and instructions text etc.;
	;
	DO QUEST2TABL(OUTREF,REF,STYLESREF,"Medical History Update",.INDENT)
	QUIT
	;
WTROS(REF,TMGDFN,OUTREF,STYLESREF,INDENT,ERR)  ;"
	;"INPUT: REF -- passed by NAME.  Reference for data note holding user replies
	;"       TMGDFN -- patient IEN
	;"       OUTREF -- Name of array to put text into for output.;
	;"	     STYLESREF -- Name of array to hold composit style information
	;"	     INDENT -- pass by REFERENCE.  A variable for holding indent amount for nice formatting of html code
	;"	     ERR -- passed by reference.  An OUT parameter for error messages
	;"RESULT: None
	;"NOTE: It is NOT the intent to create a full HTML page here.  Just the table and instructions text etc.;
	;
	DO QUEST2TABL(OUTREF,REF,STYLESREF,"Review of Systems",.INDENT)
	QUIT ;
	;
WTRX(REF,TMGDFN,OUTREF,STYLESREF,INDENT,ERR)  ;"
	;"INPUT: REF -- passed by NAME.  Reference for data note holding user replies
	;"       TMGDFN -- patient IEN
	;"       OUTREF -- Name of array to put text into for output.;
	;"	     STYLESREF -- Name of array to hold composit style information
	;"	     INDENT -- pass by REFERENCE.  A variable for holding indent amount for nice formatting of html code
	;"	     ERR -- passed by reference.  An OUT parameter for error messages
	;"RESULT: None
	;"NOTE: It is NOT the intent to create a full HTML page here.  Just the table and instructions text etc.;
	;
	DO RX2TABL(OUTREF,REF,STYLESREF,"Patient Medications",.INDENT)	;"Format medication data into HTML table, with wrappings
	QUIT ;
	;
WTOTC(REF,TMGDFN,OUTREF,STYLESREF,INDENT,ERR)  ;"
	;"INPUT: REF -- passed by NAME.  Reference for data note holding user replies
	;"       TMGDFN -- patient IEN
	;"       OUTREF -- Name of array to put text into for output.;
	;"	     STYLESREF -- Name of array to hold composit style information
	;"	     INDENT -- pass by REFERENCE.  A variable for holding indent amount for nice formatting of html code
	;"	     ERR -- passed by reference.  An OUT parameter for error messages
	;"RESULT: None
	;"NOTE: It is NOT the intent to create a full HTML page here.  Just the table and instructions text etc.;
	;
	DO RX2TABL(OUTREF,REF,STYLESREF,"Patient Over-the-counter (OTC) Medications",.INDENT,"OTC")	;"Format medication data into HTML table, with wrappings
	QUIT ;
	;
WTALRGY(REF,TMGDFN,OUTREF,STYLESREF,INDENT,ERR)  ;"
	;"INPUT: REF -- passed by NAME.  Reference for data note holding user replies
	;"       TMGDFN -- patient IEN
	;"       OUTREF -- Name of array to put text into for output.;
	;"	     STYLESREF -- Name of array to hold composit style information
	;"	     INDENT -- pass by REFERENCE.  A variable for holding indent amount for nice formatting of html code
	;"	     ERR -- passed by reference.  An OUT parameter for error messages
	;"RESULT: None
	;"NOTE: It is NOT the intent to create a full HTML page here.  Just the table and instructions text etc.;
	;
	DO ALRGY2TABL(OUTREF,REF,STYLESREF,"Patient Medication Allergies",INDENT)	;"Format data into HTML table, with wrappings
	QUIT ;
	;
WTCONSENT(REF,TMGDFN,OUTREF,STYLESREF,INDENT,ERR)  ;"
	;"INPUT: REF -- passed by NAME.  Reference for data note holding user replies
	;"       TMGDFN -- patient IEN
	;"       OUTREF -- Name of array to put text into for output.;
	;"	     STYLESREF -- Name of array to hold composit style information
	;"	     INDENT -- pass by REFERENCE.  A variable for holding indent amount for nice formatting of html code
	;"	     ERR -- passed by reference.  An OUT parameter for error messages
	;"RESULT: None
	;
	QUIT ;
	;
WTMH(REF,TMGDFN,OUTREF,STYLESREF,INDENT,ERR)  ;"
	;"INPUT: REF -- passed by NAME.  Reference for data note holding user replies
	;"       TMGDFN -- patient IEN
	;"       OUTREF -- Name of array to put text into for output.;
	;"	     STYLESREF -- Name of array to hold composit style information
	;"	     INDENT -- pass by REFERENCE.  A variable for holding indent amount for nice formatting of html code
	;"	     ERR -- passed by reference.  An OUT parameter for error messages
	;"RESULT: None
	;"NOTE: It is NOT the intent to create a full HTML page here.  Just the table and instructions text etc.;
	;
	DO QUEST2TABL(OUTREF,REF,STYLESREF,"Mental Health Questionnaire (PHQ-9)",.INDENT)
	QUIT ;
	;
WTAWV(REF,TMGDFN,OUTREF,STYLESREF,INDENT,ERR)  ;"
	;"INPUT: REF -- passed by NAME.  Reference for data note holding user replies
	;"       TMGDFN -- patient IEN
	;"       OUTREF -- Name of array to put text into for output.;
	;"	     STYLESREF -- Name of array to hold composit style information
	;"	     INDENT -- pass by REFERENCE.  A variable for holding indent amount for nice formatting of html code
	;"	     ERR -- passed by reference.  An OUT parameter for error messages
	;"RESULT: None
	;"NOTE: It is NOT the intent to create a full HTML page here.  Just the table and instructions text etc.;
	;
	DO QUEST2TABL(OUTREF,REF,STYLESREF,"AWV Questionnaire",.INDENT)
	QUIT ;
	;
	;"================================================================================"
	;" DUE DETERMINATION FUNCTIONS
	;"================================================================================"
DUEHX(TMGDFN,FMDT)	;"Is medical Hx update due?"
	QUIT 1
	;
DUEROS(TMGDFN,FMDT)	;"Is ROS update due?"
	QUIT 1
	;
DUERX(TMGDFN,FMDT)	;"Is Medicines review due?"
	QUIT 1
	;
DUEOTC(TMGDFN,FMDT)	;"Is review of OTC Rx's due?"
	QUIT 1
	;
DUEALRGY(TMGDFN,FMDT)	;"Is drug allergies review due?
	QUIT 1
	;
DUECONSENT(TMGDFN,FMDT)	;"Is consent for treatment due?"
	QUIT 1
	;
DUEMH(TMGDFN,FMDT)	;"Is mental health questionaire due?"
	QUIT 1
	;
DUEAWV(TMGDFN,FMDT)	;"Is AWV questionaire due?"
	QUIT 1
	;
	;