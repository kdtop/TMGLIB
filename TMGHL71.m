TMGHL71	;TMG/kst-Entry Point for HL7 processing ;11/14/16, 4/1/18, 4/4/19
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
  ;"HL7FIN(DIRNAME,FNAME,NOALERT,DONEPATH,OPTION) -- Entry point for processing HL7 files loaded from HFS
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
TEST(DEFPATH,OPTION)     ;"Pick file and manually send through filing process.
  SET DEFPATH=$GET(DEFPATH,"/")
  NEW FILEOPTION SET FILEOPTION("PATH")=DEFPATH
  NEW DIRNAME,FNAME,%,TMGRESULT 
TSTL1  ;
  SET (DIRNAME,FNAME)=""
  IF $$FBROWSE^TMGIOUT2(.FILEOPTION,.DIRNAME,.FNAME)  ;"DIRNAME AND FNAME are OUT parameters
  IF FNAME="" GOTO TSTDN
  NEW NOALERT SET NOALERT=$GET(OPTION("NO ALERT"),1)
  SET TMGRESULT=$$HL7FIN(DIRNAME,FNAME,NOALERT,,.OPTION)
  WRITE !
  IF TMGRESULT="-1^SPLIT" DO  GOTO TSTL2
  . WRITE "The HL7 message file was split into separate smaller files.  Nothing filed.",!
  IF TMGRESULT'>0 DO
  . WRITE "Filing that HL7 message created an error.  Alert should have been created.",!
  . WRITE "Message: ",$PIECE(TMGRESULT,"^",2,99),!
  ELSE  DO
  . WRITE "HL7 message was successfully processed.",!
TSTL2  ;  
  SET %=1
  WRITE !,"Pick another HL7 file to process" DO YN^DICN WRITE !
  IF %=1 GOTO TSTL1
TSTDN ;
  QUIT
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
HLDR1 ;  
  NEW HLDIRRETRY SET HLDIRRETRY=0
  NEW FILELIST,TMGRESULT SET TMGRESULT=$$LIST^%ZISH(DIRNAME,"SRCH","FILELIST")
  IF TMGRESULT=0 GOTO HLDDN
  NEW FNAME SET FNAME=""
  FOR  SET FNAME=$ORDER(FILELIST(FNAME)) QUIT:(FNAME="")!(COUNT'>0)!(MAXERRCT'>0)!(HLDIRRETRY)  DO
  . WRITE FNAME," --> "
  . SET TMGRESULT=$$HL7FIN^TMGHL71(DIRNAME,FNAME,0,.DONEPATH,.OPTION)
  . IF TMGRESULT="-1^SPLIT" SET HLDIRRETRY=1 QUIT 
  . IF TMGRESULT=1 WRITE "Processed OK.",!
  . ELSE  DO
  . . WRITE "ALERT",!
  . . WRITE "  ",$PIECE(TMGRESULT,"^",2,99),!
  . . SET MAXERRCT=MAXERRCT-1
  . SET COUNT=COUNT-1
  IF HLDIRRETRY=1 GOTO HLDR1
HLDDN ;
  QUIT        
  ; 
HL7FIN(DIRNAME,FNAME,NOALERT,DONEPATH,OPTION)  ;"POC file input HL7 message files from lab
  ;"Purpose: Entry point for processing HL7 files loaded from HFS
  ;"Input: DIRNAME -- file path name
  ;"       FNAME -- filename of HL7 file to load and file.  
  ;"       NOALERT -- Optional.  If 1 then no alert made.
  ;"       DONEPATH -- Optional.  If provided, then specifies root of
  ;"                  folder to moved completed messages.  must
  ;"                  NOTE: subfolders ./Processed  and ./Failed_Messages defined.
  ;"                    will be auto-added if not present (if file permissions allow)
  ;"       OPTION -- OPTIONAL.  PASS BY REFERENCE. See HL7PARSE for description
  ;"          also OPTION("NO MOVE")=1 will prevent file from being moved after processing, AND prevent meta as if "NO META" provided
  ;"          also OPTION("NO META")=1 will prevent record from being added to 22720.5
  ;"Results: 1 if OK, or -1^Message if error  
  NEW HLREC,HL,HLQUIT,HLNODE,HLNEXT,HLHDRO,HLMTIEN,HLMTIENS
  NEW TEMPRESULT SET TEMPRESULT="" 
  SET (HLMTIEN,HLMTIENS)=0  ;"NOTE: I THINK HLMTIEN,HLMTIENS ARE USED IN GLOBAL SCOPE DOWNSTREAM
  NEW TMGRESULT SET TMGRESULT=$$HL7CHECKSPLIT(DIRNAME,FNAME,.DONEPATH)
  IF TMGRESULT="-1^SPLIT" GOTO FHLDN
  NEW FPNAME SET FPNAME=$$MKTRALDV^TMGIOUTL(DIRNAME)_FNAME
  SET OPTION("FILEPATHNAME")=FPNAME  ;"Used in HL7MSGIN()
  SET TMGRESULT=$$MKHL7MSG^TMGHL7U2(FPNAME,.HLMTIEN,.HLMTIENS) ;"MAKE HL7 MESSAGE 
  NEW IEN772 SET IEN772=$GET(HLMTIEN)  
  NEW IEN773 SET IEN773=$GET(HLMTIENS)
  IF TMGRESULT<0 DO  GOTO FHL2
  . DO SETALRT2^TMGHL7E(TMGRESULT)
  SET TMGRESULT=$$HL7IN(.NOALERT,.OPTION)  ;"returns OPTION("HL7 DATE"), and other info.  
  ;"//kt note 4/1/18 -- I have decided to keep copy of HL7 message in database, so commenting line below
  ;"IF +TMGRESULT=1 SET TMGRESULT=$$KLHL7MSG^TMGHL7U2(HLMTIENS)  ;"kills records in 773 and linked 772
FHL2 ;
  IF $GET(OPTION("NO MOVE")) GOTO FHL4  ;//FHLDN
  SET TEMPRESULT=$$MOVE(+TMGRESULT,FPNAME,.DONEPATH,$GET(OPTION("HL7 DATE")))
  IF TEMPRESULT'>0 DO
  . DO SETALRT2^TMGHL7E(TEMPRESULT)
  . IF +TMGRESULT=1 SET TMGRESULT=TEMPRESULT
  . ELSE  SET TMGRESULT=TMGRESULT_" AND ALSO "_$PIECE(TEMPRESULT,"^",2,99)
FHL3 ;  
  IF $GET(OPTION("NO META")) GOTO FHLDN
  IF $$PTNOTFOUND^TMGHL7E(TMGRESULT),$$IGNORPT^TMGHL7E(TMGRESULT) GOTO FHLDN
  SET TEMPRESULT=$$MAKEMETA(+TMGRESULT,FPNAME,.DONEPATH,.OPTION,.IEN772,.IEN773)
FHL4 ;  
  IF (TEMPRESULT'=""),(TEMPRESULT'>0) DO
  . DO SETALRT2^TMGHL7E(TEMPRESULT)
  . IF +TMGRESULT=1 SET TMGRESULT=TEMPRESULT
  . ELSE  SET TMGRESULT=TMGRESULT_" AND ALSO "_$PIECE(TEMPRESULT,"^",2,99)
FHLDN  ;  
  QUIT TMGRESULT
  ;                           
MOVE(SUCCESS,FNAME,DONEPATH,HL7DATE)  ;"Move HL7 messages to destination folders    
  ;"INPUT: SUCCESS--  Value >0: store in success folder, otherwise store in failed folder
  ;"       FNAME -- full filepathname of source HL7 file 
  ;"       DONEPATH -- Optional.  If provided, then specifies root of folder to moved completed messages
  ;"       HL7DATE  -- HL7 date of message, if "", then filled with NOW
  NEW OUTNAME,OUTPATH
  NEW TMGRESULT SET TMGRESULT="1^OK"         
  DO SPLITFPN^TMGIOUTL(FNAME,.OUTPATH,.OUTNAME,"/")
  SET OUTPATH=$$DESTFLDR(SUCCESS,FNAME,.DONEPATH,.HL7DATE)
  IF +OUTPATH<0 SET TMGRESULT=OUTPATH GOTO MVDN  
  HANG 1  ;"for some reason seems to be needed.  
  NEW TEMPRESULT SET TEMPRESULT=$$MOVE^TMGKERNL(FNAME,OUTPATH_OUTNAME)   ;"result of 0 means OK
  IF TEMPRESULT>0 DO
  . NEW TEMP SET TEMP="Unable to move file '"_OUTNAME_"' to folder '"_OUTPATH_"'"
  . SET TEMP=TEMP_" (exec code="_TEMPRESULT_")"
  . SET TMGRESULT="-1^"_TEMP
MVDN ;
  QUIT TMGRESULT
  ;    
MAKEMETA(SUCCESS,FNAME,DONEPATH,OPTION,IEN772,IEN773)  ;"Store metadata about source HL7 file in TMG HL7 MESSAGE STORAGE
  ;"INPUT: SUCCESS--  Value >0: store in success folder, otherwise store in failed folder
  ;"       FNAME -- full filepathname of source HL7 file 
  ;"       DONEPATH -- Optional.  If provided, then specifies root of folder to moved completed messages
  ;"       OPTION --  PASS BY REFERENCE. See HL7PARSE for description
  ;"            OPTION("HL7 DATE")=<HL7 DATE>  
  ;"            OPTION("FM DATE")=<FMDT>       
  ;"            OPTION("DFN")=<patient IEN>          
  ;"       IEN772 -- IEN of stored HL7 message in database, in 772
  ;"       IEN773 -- IEN of stored HL7 message in database, in 773
  NEW TMGRESULT SET TMGRESULT="1^OK"
  NEW OUTNAME,OUTPATH         
  DO SPLITFPN^TMGIOUTL(FNAME,.OUTPATH,.OUTNAME,"/")
  NEW HL7DATE SET HL7DATE=$GET(OPTION("HL7 DATE")) 
  IF HL7DATE="" DO  GOTO MKMTDN  
  . SET TMGRESULT="-1^Unable to find HL7 DATE in OPTION variable."
  SET OUTPATH=$$DESTFLDR(SUCCESS,FNAME,.DONEPATH,HL7DATE)
  IF +OUTPATH<0 SET TMGRESULT=OUTPATH GOTO MKMTDN
  NEW ADFN SET ADFN=$GET(OPTION("DFN")) IF ADFN'>0 DO  GOTO MKMTDN
  . SET TMGRESULT="-1^Unable to find DFN in OPTION variable."
  NEW FMDT SET FMDT=$GET(OPTION("FM DATE")) IF FMDT'>0 DO  GOTO MKMTDN
  . SET TMGRESULT="-1^Unable to find FMDT in OPTION variable."
  SET TMGRESULT=$$MAKEREC^TMGHL7U4(ADFN,FMDT,OUTPATH,OUTNAME,.IEN772,.IEN773)           
MKMTDN  ;
  QUIT TMGRESULT
  ;
DESTFLDR(SUCCESS,FNAME,DONEPATH,HL7DATE)  ;
  ;"INPUT: SUCCESS--  Value >0: store in success folder, otherwise store in failed folder
  ;"                  If Value="SPLIT" then stored in split folder.  
  ;"       FNAME -- full filepathname of source HL7 file 
  ;"       DONEPATH -- Optional.  If provided, then specifies root of folder to moved completed messages
  ;"       HL7DATE  -- Optional.  HL7 date of message.  Default is NOW
  NEW OUTNAME,OUTPATH         
  SET HL7DATE=$GET(HL7DATE) 
  IF HL7DATE="" SET HL7DATE=$$FMDT2HL7^TMGHL7U3($$NOW^XLFDT)
  NEW YEAR SET YEAR=$EXTRACT(HL7DATE,1,4)
  NEW MONTH SET MONTH=$EXTRACT(HL7DATE,5,6)
  DO SPLITFPN^TMGIOUTL(FNAME,.OUTPATH,.OUTNAME,"/")
  IF $GET(DONEPATH)'="" SET OUTPATH=DONEPATH
  IF $EXTRACT(OUTPATH,$LENGTH(OUTPATH))'="/" SET OUTPATH=OUTPATH_"/"
  NEW DESTFOLDER SET DESTFOLDER=$SELECT(SUCCESS>0:"Processed",SUCCESS="SPLIT":"Processed/Split_Messages",1:"Failed_Messages")
  SET DESTFOLDER=DESTFOLDER_"/"_YEAR_"/"_MONTH_"/"
  SET OUTPATH=OUTPATH_DESTFOLDER
  NEW TMGRESULT SET TMGRESULT=$$ENSURDIR^TMGKERNL(OUTPATH)
  IF TMGRESULT>0 SET TMGRESULT=OUTPATH
  QUIT TMGRESULT
  ;  
HL7IN(NOALERT,OPTION)   ;"Purpose: Entry point, that could be  called from LA7V Process Results from PathGroup.
  ;"Input:  NOALERT -- Optional.  If 1 then no alert made. 
  ;"       OPTION -- OPTIONAL.  PASS BY REFERENCE. See HL7PARSE for description
  ;"            OPTION("HL7 DATE")=<HL7 DATE>  <-- an OUT parameter
  ;"            OPTION("FM DATE")=<FMDT>       <-- an OUT parameter
  ;"            OPTION("DFN")=<patient IEN>    <-- an OUT parameter         
  ;"            OPTION("FILEPATHNAME")=<FilePathName> <-- provide for reference.            
  ;"  ALSO -- Several globally-scoped variables are  used:
  ;"        HLMTIEN -- An IEN in 772     
  ;"        HLMTIENS -- in IEN in 773    
  ;"        HLREC                        ??
  ;"Results: 1 if OK, or -1^Message IF error
  ;"//kt 4/26/19 NEW MSGSTORE
  NEW TMGHL7MSG,TMGRESULT,IEN62D4,IEN22720
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
  SET TMGRESULT=$$HL7MSGIN(.TMGMSG,.NOALERT,.OPTION)
  IF $GET(TMGLOG) DO TMGLOG^TMGHL7U("After processing message, result was: "_TMGRESULT)
  GOTO H7IN2DN  ;"NOTE: if HL7MSGIN encountered error, it will file it's own alerts          
HLIERR ;
  IF (TMGRESULT<0),(+$GET(NOALERT)'=1) DO  
  . DO SETALRT2^TMGHL7E(TMGRESULT)
H7IN2DN  ;
  QUIT:$QUIT TMGRESULT
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
  ;"            OPTION("FM DATE")=<FMDT>       <-- an OUT parameter
  ;"            OPTION("DFN")=<patient IEN>    <-- an OUT parameter         
  ;"            OPTION("FILEPATHNAME")=<FilePathName> <-- provided for reference.   
  ;"Uses variables in global scope: IEN772, IEN773 (but OK if not defined)
  ;"Results: 1 if OK, or -1^Message if error
  NEW TMGHL7MSG,TMGU,TMGENV,ADFN
  NEW TMGRESULT SET TMGRESULT=1
  SET NOALERT=+$GET(NOALERT)
  IF +$GET(OPTION("GUI")) SET NOALERT=1
  ;"SET TMGRESULT=$$CKFIX(.TMGHL7MSG)  ;"opportunity to fix entire message or MSH if needed before starting parse.    
  SET TMGRESULT=$$HL7PARSE(.TMGHL7MSG,.TMGENV,.TMGMSG,.OPTION)
  NEW HL7DT SET HL7DT=$GET(TMGHL7MSG(1,7))
  SET OPTION("HL7 DATE")=HL7DT
  NEW FMDT SET FMDT=$$HL72FMDT^TMGHL7U3(HL7DT),OPTION("FM DATE")=FMDT
  NEW PIDIDX SET PIDIDX=+$ORDER(TMGHL7MSG("B","PID",""))
  SET ADFN=+$GET(TMGHL7MSG(PIDIDX,4)),OPTION("DFN")=ADFN
  IF $GET(TMGLOG) DO TMGLOG^TMGHL7U("After HL7PARSE, here is parsed message array",.TMGHL7MSG) 
  IF TMGRESULT<0 GOTO HLI3ERR
  IF +$GET(OPTION("GUI","NO FILE"))=1 GOTO H7IN3DN
  SET TMGRESULT=$$FILEMSG(.TMGENV,.TMGHL7MSG)
  IF TMGRESULT<0 GOTO HLI3ERR
HLI3ERR  ;
  IF (TMGRESULT<0),(NOALERT'=1) DO SETALERT(TMGRESULT,.TMGENV)
H7IN3DN  ;
  QUIT TMGRESULT
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
  ;"          OPTION("SCREEN OUTPUT TO NULL")=1 <-- signal to first open NULL devices
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
  NEW TONULL SET TONULL=$GET(OPTION("SCREEN OUTPUT TO NULL"))
  IF TONULL DO
  . SET TONULL("HANDLE")="TMGHNDL1"
  . DO OPEN^%ZISUTL(TONULL("HANDLE"),"NULL")
  . IF POP>0 SET TONULL=0 QUIT  ;"Unable to open NULL device
  . USE IO
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
PARSDN  ;
  KILL OPTION("GUI") MERGE OPTION("GUI")=TMGENV("GUI")
  IF +$G(TONULL) DO CLOSE^%ZISUTL(TONULL("HANDLE"))  ;"Close NULL device if opened above. 
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
FMSGDN   ;
  QUIT TMGRESULT
  ;
SETALERT(TMGRESULT,TMGENV) ;
  NEW IGNORE SET IGNORE=0
  ;"NOTE: block below shouldn't be needed as supposed to be filtered downstream.  
  IF $$PTNOTFOUND^TMGHL7E(TMGRESULT),$$IGNORPT^TMGHL7E(TMGRESULT) GOTO ALTDN
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
ALTDN  ;
  QUIT 
  ;
TESTCKSPLIT(DEFPATH)     ;"Pick file and manually send through filing process.
  SET DEFPATH=$GET(DEFPATH,"/")
  NEW OPTION SET OPTION("PATH")=DEFPATH
  NEW FNAME,%,TMGRESULT 
TCKSP1  ;
  SET FNAME=$$FBROWSE^TMGIOUT2(.OPTION)
  IF FNAME="" GOTO TSTDN
  NEW NOALERT SET NOALERT=1 
  WRITE !,"Pick another HL7 file to process" DO YN^DICN WRITE !
  IF %=1 GOTO TCKSP1
TSTCKSPDN ;
  QUIT
  ;
HL7CHECKSPLIT(DIRNAME,FNAME,DONEPATH) ;" Check for mulitple patients in 1 HL7 message
 ;"Input: DIRNAME, FNAME -- path and name of file
 ;"Result: 1 if OK, or -1^SPLIT if split, or -1^message if error.
 NEW COMBONAME SET COMBONAME=$$MKTRALDV^TMGIOUTL(DIRNAME)_FNAME 
 NEW TMGRESULT,ARRAY,MSH
 SET TMGRESULT=$$LOADHL7^TMGHL7U2(COMBONAME,.ARRAY,.MSH) ;"LOAD FILE INTO ARRAY
 IF TMGRESULT'>0 GOTO HLCKSPDN
 ;"Check HL7 Array to see if there are more than one MSH or more than one PID
 NEW MSHCOUNT,PIDCOUNT SET (MSHCOUNT,PIDCOUNT)=0
 NEW FILESPLIT SET FILESPLIT=0
 NEW ARRAYTOSAVE
 NEW IDX SET IDX=0
 FOR  SET IDX=$ORDER(ARRAY(IDX)) QUIT:(IDX'>0)!(+TMGRESULT'>0)  DO
 . NEW LINE SET LINE=$GET(ARRAY(IDX)) QUIT:LINE=""
 . NEW SEG SET SEG=$$UP^XLFSTR($EXTRACT(LINE,1,3))
 . IF SEG="MSH" SET MSHCOUNT=MSHCOUNT+1
 . IF SEG="PID" SET PIDCOUNT=PIDCOUNT+1
 . IF (MSHCOUNT>1)!(PIDCOUNT>1) DO  QUIT
 . . SET TMGRESULT=$$SAVESUBMSG(DIRNAME,FNAME,.ARRAYTOSAVE)
 . . SET FILESPLIT=1
 . . KILL ARRAYTOSAVE 
 . . IF (PIDCOUNT>1) DO
 . . . SET ARRAYTOSAVE(IDX-1)=MSH
 . . . SET MSHCOUNT=0
 . . . SET PIDCOUNT=0
 . . ELSE  DO 
 . . . SET (MSHCOUNT,PIDCOUNT)=0
 . . SET IDX=IDX-1  ;"Process line again on next loop. 
 . IF SEG="MSH" SET MSH=LINE
 . SET ARRAYTOSAVE(IDX)=LINE
 IF TMGRESULT'>0 GOTO HLCKSPDN
 IF FILESPLIT=1 DO
 . IF $DATA(ARRAYTOSAVE) DO
 . . IF $$SAVESUBMSG(DIRNAME,FNAME,.ARRAYTOSAVE)  ;"DROP RESULT
 . . ;"DO SPLITFPN^TMGIOUTL(NEWFILENAME,.FPATH,.FNAME)
 . . ;"DO SAVEMSGACTUAL^TMGHL7U2(FPATH,FNAME,.ARRAYTOSAVE)
 . SET TMGRESULT="-1^SPLIT"
 . DO MOVE("SPLIT",COMBONAME,.DONEPATH)
HLCKSPDN ;
 QUIT TMGRESULT
 ;"
SAVESUBMSG(DIRNAME,PARENTFNAME,ARRAY) ;
 ;"Result: 1, or -1^OK
 NEW COMBONAME SET COMBONAME=$$MKTRALDV^TMGIOUTL(DIRNAME)_PARENTFNAME 
 NEW NEWFILENAME SET NEWFILENAME=$$UNIQUE^%ZISUTL(COMBONAME)
 NEW FPATH,FNAME
 IF $ORDER(ARRAY(0))>1 DO
 . NEW TEMPARR,IDX SET IDX=""
 . NEW JDX SET JDX=0
 . FOR  SET IDX=$ORDER(ARRAY(IDX)) QUIT:IDX'>0  DO
 . . SET JDX=JDX+1,TEMPARR(JDX)=$GET(ARRAY(IDX))
 . KILL ARRAY MERGE ARRAY=TEMPARR 
 DO SPLITFPN^TMGIOUTL(NEWFILENAME,.FPATH,.FNAME)
 SET TMGRESULT=$$SAVEMSGACTUAL^TMGHL7U2(FPATH,FNAME,.ARRAY) 
 QUIT TMGRESULT
 ;
HL7CHECKSPLIT0(DIRNAME,FNAME) ;" Check for mulitple patients in 1 HL7 message
 ;"Input: DIRNAME, FNAME -- path and name of file
 ;"Result: 1 if OK, or -1^SPLIT if split, or -1^message if error.
 NEW COMBONAME SET COMBONAME=$$MKTRALDV^TMGIOUTL(DIRNAME)_FNAME
 NEW TMGRESULT,ARRAY,MSH
 SET TMGRESULT=$$LOADHL7^TMGHL7U2(COMBONAME,.ARRAY,.MSH) ;"LOAD FILE INTO ARRAY
 IF TMGRESULT'>0 GOTO HLCKSPDN0
 ;"Check Array to see if there are more than one MSH or one PID
 NEW MSHFOUND,PIDFOUND SET (MSHFOUND,PIDFOUND)=0
 NEW SPLITNEEDED SET SPLITNEEDED=0
 NEW IDX SET IDX=0
 FOR  SET IDX=$ORDER(ARRAY(IDX)) QUIT:(IDX'>0)!(SPLITNEEDED=1)  DO
 . NEW LINE SET LINE=$GET(ARRAY(IDX))
 . ;"HERE WE WILL TEST THE ELEMENTS
 . IF (MSHFOUND=1)&(LINE["MSH") SET SPLITNEEDED=1 QUIT
 . IF (PIDFOUND=1)&(LINE["PID") SET SPLITNEEDED=1 QUIT
 . IF LINE["MSH" SET MSHFOUND=1
 . IF LINE["PID" SET PIDFOUND=1
 IF SPLITNEEDED=1 DO
 . SET TMGRESULT="-1^SPLIT"
 . DO SPLITMSG0(COMBONAME,.ARRAY,.MSH)
 . DO MOVE(0,COMBONAME,.DONEPATH,0)
HLCKSPDN0 ;
 QUIT TMGRESULT
 ;"
SPLITMSG0(COMBONAME,ARRAY,MSH)  ;"SPLIT MESSAGE INTO SEPARATE MESSAGES
 ;"Rules for splitting: Make separate file if duplicate MSH or PID segment found.  
 NEW NEWFILENAME SET NEWFILENAME=$$UNIQUE^%ZISUTL(COMBONAME)
 NEW MSHFOUND,PIDFOUND,MSHNEEDED,COMPLETED,IDX,NEWIDX,ARRAYTOSAVE,FPATH,FNAME
 SET (MSHFOUND,PIDFOUND,MSHNEEDED,COMPLETED,IDX)=0
 SET NEWIDX=1
 ;"
 FOR  SET IDX=$ORDER(ARRAY(IDX)) QUIT:IDX'>0  DO
 . IF (MSHFOUND=1)&($G(ARRAY(IDX))["MSH") SET MSHNEEDED=0,COMPLETED=1
 . IF (PIDFOUND=1)&($G(ARRAY(IDX))["PID") SET MSHNEEDED=1,COMPLETED=1
 . IF COMPLETED=1 DO
 . . SET (MSHFOUND,PIDFOUND,COMPLETED)=0
 . . ;"IF NO MSH WAS FOUND, ADD IT TO THE TOP
 . . IF MSHNEEDED=1 SET ARRAYTOSAVE(0)=MSH
 . . DO SPLITFPN^TMGIOUTL(NEWFILENAME,.FPATH,.FNAME)
 . . DO SAVEMSGACTUAL^TMGHL7U2(FPATH,FNAME,.ARRAYTOSAVE)
 . . SET NEWFILENAME=$$UNIQUE^%ZISUTL(COMBONAME) ;"GET NEXT FILE NAME
 . . SET MSHNEEDED=0,NEWIDX=1
 . . KILL ARRAYTOSAVE
 . . SET IDX=IDX-1 ;"BACK UP ONE ENTRY TO CATCH THE TRIGGER
 . ELSE  DO
 . . IF $G(ARRAY(IDX))["MSH" SET MSHFOUND=1
 . . IF $G(ARRAY(IDX))["PID" SET PIDFOUND=1
 . . ;"SAVE TO MESSAGE TO SAVE
 . . SET ARRAYTOSAVE(NEWIDX)=$G(ARRAY(IDX))
 . . SET NEWIDX=NEWIDX+1
 IF $DATA(ARRAYTOSAVE) DO
 . ;"SAVE FINAL ENTRY HERE
 . DO SPLITFPN^TMGIOUTL(NEWFILENAME,.FPATH,.FNAME)
 . DO SAVEMSGACTUAL^TMGHL7U2(FPATH,FNAME,.ARRAYTOSAVE)
 QUIT 
 ;" 