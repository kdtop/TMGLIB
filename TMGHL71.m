TMGHL71	;TMG/kst-Entry Point for HL7 processing ;2/17/16, 11/14/16
	             ;;1.0;TMG-LIB;**1**;02/22/11;Build 61
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
  ;"TEST(DEFPATH) --Pick file and manually send through filing process.
  ;"HLDIRIN(DIRNAME,COUNT,MAXERRCT,DONEPATH) -- Import files from a directory
  ;"HL7FIN(FNAME,NOALERT,DONEPATH) -- Entry point for processing HL7 files loaded from HFS
	;"HL7IN -- Entry point, that could be  called from LA7V Process Results from PathGroup.
	;"HL7MSGIN(TMGMSG) -- Entry point to process message, stored in TMGMSG
  ;"=======================================================================
  ;" API - Private Functions
  ;"=======================================================================
  ;"<Many call-back hooks for transformation engine>
  ;"=======================================================================
  ;"Dependancies
  ;"=======================================================================
  ;"TMGSTUTL, all the HL*, LA* code that the HL processing path normally calls.
  ;"=======================================================================
  ;
HL7IN0   ;"NOTE: I don't think this function is in use....
  ;"Purpose: Entry point, as called from LA7V Process Results from LMH
  ;"Input:
  ;
  N HLA,HLL,HLP,X,Y
  N LA76248,LA76249,LA7AAT,LA7AERR,LA7CS,LA7DT,LA7ECH,LA7FS,LA7HLS,LA7HLSA,LA7INTYP,LA7MEDT,LA7MTYP,LA7RAP,LA7PRID,LA7RSITE,LA7SAP,LA7SEQ,LA7SSITE,LA7TYPE,LA7VER,LA7VI,LA7VJ,LA7X
  ;
  S DT=$$DT^XLFDT
  S (LA76248,LA76249,LA7INTYP,LA7SEQ)=0
  ;
  ;K ^TMP("HLA",$J)
  ;
  ; Setup DUZ array to 'non-human' user LRLAB,HL
  ; If user not found - send alert to G.LAB MESSAGING
  S LA7X=$$FIND1^DIC(200,"","OX","LRLAB,HL","B","")
  I LA7X<1 D  Q
  . N MSG
  . S MSG="Lab Messaging - Unable to identify user 'LRLAB,HL' in NEW PERSON file"
  . D XQA^LA7UXQA(0,LA76248,0,0,MSG,"",0)
  D DUZ^XUP(LA7X)
  ;
  ; Set up LA7HLS with HL variables to build ACK message.
  ; Handle situation when systems use different encoding characters.
  D RSPINIT^HLFNC2(HL("EIDS"),.LA7HLS)
  ;"DO ORU^LA7VHL
  QUIT
  ;
TEST(DEFPATH)     ;"Pick file and manually send through filing process.
       SET DEFPATH=$GET(DEFPATH,"/")
       NEW OPTION SET OPTION("PATH")=DEFPATH
       NEW FNAME,%,TMGRESULT 
TSTL1  SET FNAME=$$FBROWSE^TMGIOUT2(.OPTION)
       IF FNAME="" GOTO TSTDN
       NEW NOALERT SET NOALERT=1 
       SET TMGRESULT=$$HL7FIN^TMGHL71(FNAME,NOALERT) 
       IF TMGRESULT'>0 DO
       . WRITE "Filing that HL7 message created an error.  Alert should have been created.",!
       . WRITE "Message: ",$PIECE(TMGRESULT,"^",2,99),!
       ELSE  DO
       . WRITE "HL7 message was successfully processed.",!
       SET %=1
       WRITE !,"Pick another HL7 file to process" DO YN^DICN WRITE !
       IF %=1 GOTO TSTL1
TSTDN  QUIT
       ;
TEST2  ;"Test one particular stored message (MUST BE EDITED TO BE USED)
       NEW HLMTIEN SET HLMTIEN=543
       NEW HLMTIENS SET HLMTIENS=544
       NEW HLREC
       WRITE $$HL7IN^TMGHL71(1)  ;"1=no alert. 
       QUIT
       ;
SHOWALTR  ;"SHOW ALERT RECIPIENT
    NEW OUT DO GETINIVL^TMGRPC1A(.OUT,"DEFAULT","HL7 LAB ERROR RECIPIENT",168)
    WRITE !,"In case of lab errors, alerts will go to: ",$PIECE(OUT,"^",2),!
    QUIT
    ;
SETALRTR ;"SET ALERT RECIPIENT
    NEW X,Y,DIC SET DIC=200,DIC(0)="MAEQ"
    WRITE "Select user to receive alerts in cases of lab errors...",!
    DO ^DIC WRITE !
    IF +Y'>1 QUIT
    NEW VALUE SET VALUE=+Y_";"_$PIECE(Y,"^",2)
    NEW OUT DO SETINIVL^TMGRPC1A(.OUT,"DEFAULT","HL7 LAB ERROR RECIPIENT",VALUE)
    IF +OUT<0 DO
    . WRITE "ERROR: ",$PIECE(OUT,"^",2,99),!
    QUIT
  ;
  ;"+-----------------------------------------------------------------+
  ;"| =============================================================== |
  ;"| |  General HL7 processing functions                           | |
  ;"| =============================================================== |
  ;"+-----------------------------------------------------------------+
  ;"NOTE: This is code for working with labs from both Laughlin lab and Pathgroup Labs
  ;"      i.e. this is common code, usable by multiple labs. 
  ;"      FYI -- Pathgroup code is in TMGHL73
  ;"             Laughlin  code is in TMGHL74
  ;"             Quest     code is in TMGHL75
  ;
HLDIRIN(DIRNAME,COUNT,MAXERRCT,DONEPATH,EXT,OPTION)    ;
           ;"Purpose: Import all matching files from a directory
           ;"Input: DIRNAME -- the directory to read files from 
           ;"       COUNT -- OPTIONAL.  Default=999999
           ;"       MAXERRCT -- OPTIONAL.  Default=999999.  Will stop when this number of errors encountered 
           ;"       DONEPATH -- Optional.  If provided, then specifies root of
           ;"                  folder to moved completed messages.  
           ;"                  NOTE: subfolders ./Processed  and ./Failed_Messages defined.
           ;"                    will be auto-added if not present (if file permissions allow)
           ;"       EXT -- OPTIONAL.  Default = '.txt'.  
           ;"       OPTION -- OPTIONAL.  PASS BY REFERENCE. See HL7PARSE for description
           ;"Result: none
           SET COUNT=+$GET(COUNT,999999)
           SET MAXERRCT=+$GET(MAXERRCT,999999)
           SET EXT=$$TRIM^XLFSTR($GET(EXT,".txt")) 
           IF $EXTRACT(EXT,1)'="." SET EXT="."_EXT
           SET EXT="*"_EXT
           NEW SRCH SET SRCH(EXT)="",SRCH($$UP^XLFSTR(EXT))="",SRCH($$LOW^XLFSTR(EXT))=""
           NEW FILELIST,TMGRESULT SET TMGRESULT=$$LIST^%ZISH(DIRNAME,"SRCH","FILELIST")
           IF TMGRESULT=0 GOTO HLDDN
           NEW FNAME SET FNAME=""
           FOR  SET FNAME=$ORDER(FILELIST(FNAME)) QUIT:(FNAME="")!(COUNT'>0)!(MAXERRCT'>0)  DO
           . WRITE FNAME," --> "
           . SET TMGRESULT=$$HL7FIN^TMGHL71(DIRNAME_"/"_FNAME,0,.DONEPATH,.OPTION)
           . IF TMGRESULT=1 WRITE "Processed OK.",!
           . ELSE  DO
           . . WRITE "ALERT",!
           . . WRITE "  ",$PIECE(TMGRESULT,"^",2,99),!
           . . SET MAXERRCT=MAXERRCT-1
           . SET COUNT=COUNT-1
HLDDN      QUIT        
           ; 
HL7FIN(FNAME,NOALERT,DONEPATH,OPTION)  ;"POC file input HL7 message files from lab
           ;"Purpose: Entry point for processing HL7 files loaded from HFS
           ;"Input: FNAME -- full filepathname of HL7 file to load and file.  
           ;"       NOALERT -- Optional.  If 1 then no alert made.
           ;"       DONEPATH -- Optional.  If provided, then specifies root of
           ;"                  folder to moved completed messages.  must
           ;"                  NOTE: subfolders ./Processed  and ./Failed_Messages defined.
           ;"                    will be auto-added if not present (if file permissions allow)
           ;"       OPTION -- OPTIONAL.  PASS BY REFERENCE. See HL7PARSE for description
           ;"Results: 1 if OK, or -1^Message if error
           NEW HLREC,HL,HLQUIT,HLNODE,HLNEXT,HLHDRO,HLMTIEN,HLMTIENS 
           SET (HLMTIEN,HLMTIENS)=0
           NEW TMGRESULT SET TMGRESULT=$$MKHL7MSG^TMGHL7U2(FNAME,.HLMTIEN,.HLMTIENS) ;"MAKE HL7 MESSAGE 
           IF TMGRESULT<0 DO  GOTO FHL2DN
           . DO SETALRT2^TMGHL7E(TMGRESULT)
           SET TMGRESULT=$$HL7IN(.NOALERT,.OPTION)  ;"returns OPTION("HL7 DATE")
           IF +TMGRESULT=1 SET TMGRESULT=$$KLHL7MSG^TMGHL7U2(HLMTIENS)  ;"kills records in 773 and linked 772
FHL2DN     NEW TEMPRESULT SET TEMPRESULT=$$MOVE(+TMGRESULT,FNAME,.DONEPATH,$GET(OPTION("HL7 DATE")))
           IF TEMPRESULT'>0 DO
           . DO SETALRT2^TMGHL7E(TEMPRESULT)
           . IF +TMGRESULT=1 SET TMGRESULT=TEMPRESULT
           . ELSE  SET TMGRESULT=TMGRESULT_" AND ALSO "_$PIECE(TEMPRESULT,"^",2,99)
           QUIT TMGRESULT
           ;
MOVE(SUCCESS,FNAME,DONEPATH,HL7DATE)  ;"Move HL7 messages to destination folders
           NEW OUTNAME,OUTPATH
           SET HL7DATE=$GET(HL7DATE) 
           IF HL7DATE="" SET HL7DATE=$$FMDT2HL7^TMGHL7U3($$NOW^XLFDT)
           NEW YEAR SET YEAR=$EXTRACT(HL7DATE,1,4)
           NEW MONTH SET MONTH=$EXTRACT(HL7DATE,5,6)
           DO SPLITFPN^TMGIOUTL(FNAME,.OUTPATH,.OUTNAME,"/")
           IF $GET(DONEPATH)'="" SET OUTPATH=DONEPATH
           IF $EXTRACT(OUTPATH,$LENGTH(OUTPATH))'="/" SET OUTPATH=OUTPATH_"/"
           NEW DESTFOLDER SET DESTFOLDER=$SELECT(SUCCESS>0:"Processed",1:"Failed_Messages")
           SET DESTFOLDER=DESTFOLDER_"/"_YEAR_"/"_MONTH_"/"
           SET OUTPATH=OUTPATH_DESTFOLDER
           NEW TMGRESULT SET TMGRESULT=$$ENSURDIR^TMGKERNL(OUTPATH)
           IF TMGRESULT'>0 GOTO MVDN
           NEW TEMPRESULT SET TEMPRESULT=$$MOVE^TMGKERNL(FNAME,OUTPATH_OUTNAME)   ;"result of 0 means OK
           IF TEMPRESULT>0 DO
           . NEW TEMP SET TEMP="Unable to move file '"_OUTNAME_"' to folder '"_OUTPATH_"'"
           . SET TEMP=TEMP_" (exec code="_TEMPRESULT_")"
           . SET TMGRESULT="-1^"_TEMP
MVDN       QUIT TMGRESULT
           ;
HL7IN(NOALERT,OPTION)   ;"Purpose: Entry point, that could be  called from LA7V Process Results from PathGroup.
           ;"Input:  NOALERT -- Optional.  If 1 then no alert made. 
           ;"       OPTION -- OPTIONAL.  PASS BY REFERENCE. See HL7PARSE for description
           ;"            OPTION("HL7 DATE")=<HL7 DATE>  <-- an OUT parameter
           ;"  ALSO -- Several globally-scoped variables are  used:
           ;"        HLMTIEN -- An IEN in 772     
           ;"        HLMTIENS -- in IEN in 773    
           ;"        HLREC                        ??
           ;"Results: 1 if OK, or -1^Message IF error
           NEW TMGHL7MSG,MSGSTORE,TMGRESULT,IEN62D4,IEN22720
           NEW TMGHL7DEBUG SET TMGHL7DEBUG=0
           NEW TMGMSG,TMGENV,TMGINFO
           NEW IEN772,IEN773,TMGHLZZZ
           IF $GET(TMGLOG) DO TMGLOG^TMGHL7U("@@")  ;"kill message log for all processes.
           IF $GET(TMGLOG) DO TMGLOG^TMGHL7U("Starting HL7IN^TMGHL72 to process HL7 message") 
           SET TMGRESULT=$$SETFMENV^TMGHL7U()  ;"SETUP FILE MESSAGE ENVIRONMENT. 
           IF TMGRESULT<0 GOTO HLIERR
           SET TMGRESULT=$$FROM772^TMGHL7U2(IEN772,IEN773,.TMGMSG)
           IF TMGRESULT<0 GOTO HLIERR        
           IF $GET(TMGLOG) DO TMGLOG^TMGHL7U("Here is message array",.TMGMSG) 
           SET TMGRESULT=$$HL7MSGIN^TMGHL71(.TMGMSG,.NOALERT,.OPTION)
           IF $GET(TMGLOG) DO TMGLOG^TMGHL7U("After processing message, result was: "_TMGRESULT)
           GOTO H7IN2DN  ;"NOTE: if HL7MSGIN encountered error, it will file it's own alerts          
HLIERR     IF (TMGRESULT<0),(+$GET(NOALERT)'=1) DO  
           . DO SETALRT2^TMGHL7E(TMGRESULT)
H7IN2DN    QUIT:$QUIT TMGRESULT
           QUIT
           ;        
HL7MSGIN(TMGMSG,NOALERT,OPTION)    ;
           ;"Purpose: Entry point to process message, stored in TMGMSG
           ;"Input: TMGMSG -- PASS BY REFERENCE.  HL7 message to process.  Format
           ;"          TMGMSG(#)=<line of HL7 message>
           ;"       NOALERT -- OPTIONAL.  If 1 then no alert is created for errors
           ;"          NOTE: if OPTION("GUI")=1, then this value is forced to 1.
           ;"       OPTION -- OPTIONAL.  PASS BY REFERENCE. See HL7PARSE for description
           ;"            OPTION("HL7 DATE")=<HL7 DATE>  <-- an OUT parameter
           ;"Results: 1 if OK, or -1^Message if error
           NEW TMGHL7MSG,TMGU,TMGENV
           NEW TMGRESULT SET TMGRESULT=1
           SET NOALERT=+$GET(NOALERT)
           IF +$GET(OPTION("GUI")) SET NOALERT=1
           SET TMGRESULT=$$HL7PARSE^TMGHL71(.TMGHL7MSG,.TMGENV,.TMGMSG,.OPTION)
           SET OPTION("HL7 DATE")=$GET(TMGHL7MSG(1,7))
           IF $GET(TMGLOG) DO TMGLOG^TMGHL7U("After HL7PARSE, here is parsed message array",.TMGHL7MSG) 
           IF TMGRESULT<0 GOTO HLI3ERR
           IF +$GET(OPTION("GUI","NO FILE"))=1 GOTO H7IN3DN
           SET TMGRESULT=$$FILEMSG(.TMGENV,.TMGHL7MSG)
HLI3ERR    IF (TMGRESULT<0),(NOALERT'=1) DO SETALERT(TMGRESULT)
H7IN3DN    QUIT TMGRESULT
           ;
HL7PARSE(TMGHL7MSG,TMGENV,TMGMSG,OPTION)    ;
           ;"Purpose: Entry point to process message, stored in TMGMSG
           ;"Input: TMGHL7MSG -- PASS BY REFERENCE, AN OUT PARAMETER.
           ;"       TMGENV -- PASS BY REFERENCE, AN OUT PARAMETER.
           ;"       TMGMSG -- PASS BY REFERENCE.  HL7 message to process.  Format
           ;"          TMGMSG(#)=<line of HL7 message>
           ;"       OPTION -- OPTIONAL.  PASS BY REFERENCE. Format:
           ;"          OPTION("GUI")=1  <-- signal that GUI is driving this code
           ;"          OPTION("GUI","NO FILE")=1  <-- signal to just test parsing, don't actually file. 
           ;"          OPTION("GUI","MSG",#)=<message from server code to GUI> <-- OUT PARAMETER
           ;"          OPTION("GUI","MSG",#,"REPLY")=<reply from GUI to server code>   <-- IN PARAMETER
           ;"          OPTION("INTERACTIVE MODE")=1  <-- signal to ask user for solutions to problems. 
           ;"          OPTION("AUTO REGISTER MODE")=1  <-- signal to register labs etc automatically.  
           ;"Results: 1 if OK, or -1^Message if error
           NEW IEN62D4,IEN22720,FS,ECH
           NEW TMGRESULT SET TMGRESULT=1
           SET NOALERT=+$GET(NOALERT)
           KILL ^TMG("TMP","TMGHL71") ;"kill message log for all processes.
           SET TMGRESULT=$$SETUPENV^TMGHL7U(.TMGMSG,.TMGENV,0)
           IF TMGRESULT<0 GOTO PARSDN
           IF +$GET(OPTION("GUI")) DO
           . SET NOALERT=1
           . MERGE TMGENV("GUI")=OPTION("GUI")
           MERGE TMGENV("INTERACTIVE MODE")=OPTION("INTERACTIVE MODE")
           MERGE TMGENV("AUTO REGISTER MODE")=OPTION("AUTO REGISTER MODE")
           SET IEN62D4=TMGENV("IEN 62.4")  ;"I think this is used in global scope during transformation.
           SET IEN22720=TMGENV("IEN 22720")
           ;"NOTE: the line below divides up HL7 message into array.  But it
           ;"      ALSO gathers information about each test (i.e. NLT codes etc)
           SET TMGRESULT=$$PARSMSG2^TMGHL7X2(.TMGENV,.TMGMSG,.TMGHL7MSG) 
           IF TMGRESULT<0 GOTO PARSDN
           SET TMGHL7MSG("STAGE")="PRE"
           SET TMGRESULT=$$XFMSG^TMGHL7X(.TMGENV,.TMGHL7MSG)
           IF TMGRESULT<0 GOTO PARSDN
           SET TMGRESULT=$$DOMORE^TMGHL7X2(.TMGENV,.TMGHL7MSG)
           IF TMGRESULT<0 GOTO PARSDN
           SET TMGHL7MSG("STAGE")="FINAL"
           SET TMGRESULT=$$XFMSG^TMGHL7X(.TMGENV,.TMGHL7MSG)
PARSDN     KILL OPTION("GUI") MERGE OPTION("GUI")=TMGENV("GUI")
           QUIT TMGRESULT
           ;
FILEMSG(TMGENV,TMGHL7MSG)  ;"Call filer to handle prepped message           
           ;"Input: TMGENV -- PASS BY REFERENCE
           ;"       TMGHL7MSG -- PASS BY REFERENCE
           ;"Results: 1 if OK, or -1^Message if error
           NEW TMGRESULT SET TMGRESULT=1
           NEW IEN22720 SET IEN22720=+$GET(TMGENV("IEN 22720"))
           IF IEN22720'>0 DO  GOTO FMSGDN
           . SET TMGRESULT="-1^Can't get IEN 22720 in FILEMSG^TMGHL71"
           NEW NODE SET NODE=$GET(^TMG(22720,IEN22720,100))
           NEW TAG SET TAG=$PIECE(NODE,"^",1) IF TAG="" DO  GOTO FMSGDN
           . SET TMGRESULT="-1^Filer TAG (field 100) not defined for record IEN="_IEN22720_" in file 22720."           
           NEW ROUTINE SET ROUTINE=$PIECE(NODE,"^",2) IF ROUTINE="" DO  GOTO FMSGDN
           . SET TMGRESULT="-1^Filer ROUTINE (field 101) not defined for record IEN="_IEN22720_" in file 22720."           
           NEW CODE SET CODE="SET TMGRESULT=$$"_TAG_"^"_ROUTINE_"(.TMGENV,.TMGHL7MSG)"
           NEW TRAPECODE SET TRAPECODE=""
           IF $GET(TMGLOG) DO TMGLOG^TMGHL7U("Starting call to "_TAG_"^"_ROUTINE) 
           DO
           . NEW $ETRAP SET $ETRAP="SET TRAPECODE=$ECODE,$ETRAP="""",$ECODE="""""
           . XECUTE CODE
           IF TRAPECODE'="" DO
           . SET TMGRESULT="-1^Mumps error calling filer: "_TRAPECODE
           ;" SET TMGRESULT=$$FILEMSG^TMGLRW01(.TMGENV,.TMGHL7MSG)
           IF $GET(TMGLOG) DO TMGLOG^TMGHL7U("After call to "_TAG_"^"_ROUTINE_", result was: "_TMGRESULT)
FMSGDN     QUIT TMGRESULT
           ;
SETALERT(TMGRESULT) ;
           NEW IEN22720 SET IEN22720=+$GET(TMGENV("IEN 22720"))
           IF IEN22720'>0 GOTO ALTDN   ;"No way to send error message, so just leave 
           NEW NODE SET NODE=$GET(^TMG(22720,IEN22720,100))
           NEW TAG SET TAG=$PIECE(NODE,"^",3) IF TAG="" GOTO ALTDN  
           NEW ROUTINE SET ROUTINE=$PIECE(NODE,"^",4) IF ROUTINE="" GOTO ALTDN
           NEW CODE SET CODE="SET TMGRESULT=$$"_TAG_"^"_ROUTINE_"(.TMGRESULT)"
           NEW TRAPECODE SET TRAPECODE=""
           DO
           . NEW $ETRAP SET $ETRAP="SET TRAPECODE=$ECODE,$ETRAP="""",$ECODE="""""
           . XECUTE CODE
           ;"REPLACING SETALRT2^TMGHL7E(TMGRESULT)       
ALTDN      QUIT 
           ;
