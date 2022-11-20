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
  ;"HLFILEIMPORT(DIRNAME,FNAME,DONEPATH,OPTION) -- Entry point for processing HL7 files loaded from HFS
  ;"HL772IMPORT(TMGENV,NOALERT,OPTION) -- Entry point, that could be  called from LA7V Process Results from PathGroup.
  ;"HLMSGIMPORT(TMGMSG,NOALERT,OPTION,.TMGENV) -- Entry point to process message, stored in TMGMSG
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
  NEW LOCALOPTION
  MERGE LOCALOPTION=OPTION
  SET TMGRESULT=$$HLFILEIMPORT(DIRNAME,FNAME,,.LOCALOPTION)
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
  ;"WRITE $$HL772IMPORT^TMGHL71(543,544,1)  ;"1=no alert. 
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
  ;"       EXT -- OPTIONAL.  Default = '.txt'  '.hl7' is also always added.  
  ;"       OPTION -- OPTIONAL.  PASS BY REFERENCE. See HL7PROCESS for description
  ;"Result: none
  SET COUNT=+$GET(COUNT,999999)
  SET MAXERRCT=+$GET(MAXERRCT,999999)
  SET EXT=$$TRIM^XLFSTR($GET(EXT,".txt")) 
  IF $EXTRACT(EXT,1)'="." SET EXT="."_EXT
  SET EXT="*"_EXT
  NEW EXT2 SET EXT2="*.HL7"
  NEW SRCH 
  SET SRCH(EXT)="",SRCH($$UP^XLFSTR(EXT))="",SRCH($$LOW^XLFSTR(EXT))=""
  SET SRCH(EXT2)="",SRCH($$UP^XLFSTR(EXT2))="",SRCH($$LOW^XLFSTR(EXT2))=""
  NEW LOG,LOGIDX SET LOGIDX=1
HLDR1 ;  
  NEW HLDIRRETRY SET HLDIRRETRY=0
  NEW FILELIST,TMGRESULT SET TMGRESULT=$$LIST^%ZISH(DIRNAME,"SRCH","FILELIST")
  IF TMGRESULT=0 GOTO HLDDN
  NEW FNAME SET FNAME=""
  FOR  SET FNAME=$ORDER(FILELIST(FNAME)) QUIT:(FNAME="")!(COUNT'>0)!(MAXERRCT'>0)!(HLDIRRETRY)  DO
  . WRITE FNAME," --> "
  . NEW LOCALOPTION
  . MERGE LOCALOPTION=OPTION
  . SET TMGRESULT=$$HLFILEIMPORT^TMGHL71(DIRNAME,FNAME,.DONEPATH,.LOCALOPTION)
  . IF TMGRESULT="-1^SPLIT" SET HLDIRRETRY=1 QUIT
  . SET LOG(LOGIDX)=DIRNAME_"^"_FNAME_"^"_TMGRESULT
  . SET LOGIDX=LOGIDX+1
  . IF +TMGRESULT=1 WRITE "Processed OK.",!
  . ELSE  DO
  . . WRITE "ALERT",!
  . . WRITE "  ",$PIECE(TMGRESULT,"^",2,99),!
  . . SET MAXERRCT=MAXERRCT-1
  . SET COUNT=COUNT-1
  IF HLDIRRETRY=1 GOTO HLDR1
HLDDN ;
  ;"Write log ARRAY to HFS as timestamped log for this batch.  Store in DonePath...  
  QUIT        
  ; 
HLFILEIMPORT(DIRNAME,FNAME,DONEPATH,OPTION)  ;
  ;"Purpose: Entry point for processing HL7 files loaded from HFS
  ;"Input: DIRNAME -- file path name
  ;"       FNAME -- filename of HL7 file to load and file.  
  ;"       DONEPATH -- Optional.  If provided, then specifies root of
  ;"                  folder to moved completed messages.
  ;"                  NOTE: subfolders ./Processed  and ./Failed_Messages defined.
  ;"                    will be auto-added if not present (if file permissions allow)
  ;"       OPTION -- OPTIONAL.  PASS BY REFERENCE. See HL7PROCESS for description
  ;"          also OPTION("NO MOVE")=1 will prevent file from being moved after processing, AND prevent meta as if "NO META" provided
  ;"          also OPTION("NO META")=1 will prevent record from being added to 22720.5
  ;"Results: 1 if OK, or -1^Message if error  
 ;" NEW HLREC,HL,HLQUIT,HLNODE,HLNEXT,HLHDRO,HLMTIEN,HLMTIENS
  IF $GET(TMGLOG) DO TMGLOG^TMGHL7U("@@")  ;"kill message log for all processes.
  IF $GET(TMGLOG) DO TMGLOG^TMGHL7U("Starting HLFILEIMPORT^TMGHL72")
  NEW TEMPRESULT SET TEMPRESULT="" 
  NEW IEN772,IEN773
  NEW TMGMSG,MSH
  NEW NOALERT SET NOALERT=+$GET(OPTION("NO ALERT")) ;"//kt 6/11/20 changed default from 1 -> 0
  NEW TMGENV 
  SET DIRNAME=$$MKTRALDV^TMGIOUTL($GET(DIRNAME))
  SET OPTION("DONEPATH")=$GET(DONEPATH)
  SET OPTION("FNAME")=FNAME
  SET OPTION("FPATH")=DIRNAME
  NEW FPNAME SET FPNAME=DIRNAME_FNAME
  SET OPTION("FILEPATHNAME")=FPNAME  
  SET TMGENV("ALERT CODE")="SETALERT^TMGHL7E"  ;"Default, likely overwritten later
  IF $$UP^XLFSTR(FNAME)="TIU.TXT" DO  GOTO FHLDN
  . SET TMGRESULT=$$APPENDLOG(.OPTION,"Found: "_FPNAME_". Not processing")
  . SET TMGRESULT="-1^IGNORING TIU.TXT"
  NEW TMGRESULT SET TMGRESULT=$$HL7CHECKSPLIT(.OPTION) 
  IF TMGRESULT="-1^SPLIT" GOTO FHLDN
  SET TMGRESULT=$$APPENDLOG(.OPTION,"Processing HL7 file: "_FPNAME)
  IF TMGRESULT'>0 GOTO FHL1
  SET TMGRESULT=$$LOADHL7^TMGHL7U2(FPNAME,.TMGMSG,.MSH) ;"LOAD FILE INTO ARRAY
  IF TMGRESULT'>0 GOTO FHL1
  NEW ZEF DO CHECKLONGZEF(.TMGMSG,.ZEF) ;"Remove any long ZEF segment, which is an embedded file -- was crashing line below
  SET TMGRESULT=$$MKHLMARR^TMGHL7U2(.TMGMSG,MSH,.IEN772,.IEN773)  ;"Store HL7 message in database
  MERGE TMGMSG=ZEF KILL ZEF  ;"Put long ZEF line (if any) back into TMGMSG
  IF TMGRESULT>0 SET TMGRESULT=$$SETUPENV^TMGHL7U(.TMGMSG,.TMGENV)    
  SET TMGENV("IEN 772")=$GET(IEN772) SET TMGENV("IEN 773")=$GET(IEN773)
FHL1 ;  
  IF TMGRESULT<0 DO SETALERT(TMGRESULT,.TMGENV,.OPTION) GOTO FHL2  
  IF $GET(TMGLOG) DO TMGLOG^TMGHL7U("Here is message array",.TMGMSG) 
  SET TMGRESULT=$$HLMSGIMPORT(.TMGMSG,.NOALERT,.OPTION,.TMGENV) ;"<-- MAIN CODE HERE: PARSE -> XFORM -> FILE
  ;"NOTE: if HLMSGIMPORT encountered error, it will set it's own alerts, so don't duplicate here
  IF $GET(TMGLOG) DO TMGLOG^TMGHL7U("After processing message, result was: "_TMGRESULT)
FHL2 ;
  IF $GET(OPTION("NO MOVE")) GOTO FHL4 
  ;"SET TEMPRESULT=$$MOVE(+TMGRESULT,FPNAME,.DONEPATH,$GET(OPTION("HL7 DATE")))
  IF $$PTNOTFOUND^TMGHL7E(TMGRESULT),$$IGNORPT^TMGHL7E(TMGRESULT) DO 
  . SET TEMPRESULT=$$MOVE(2,.OPTION)  ;"2 means move to discard folder. //kt 7/6/21
  . SET TMGRESULT=1  ;"clear error state
  ELSE  DO
  . SET TEMPRESULT=$$MOVE(+TMGRESULT,.OPTION)
  IF TEMPRESULT'>0 DO
  . DO SETALERT(TMGRESULT,.TMGENV,.OPTION)
  . IF +TMGRESULT=1 SET TMGRESULT=TEMPRESULT
  . ELSE  SET TMGRESULT=TMGRESULT_" AND ALSO "_$PIECE(TEMPRESULT,"^",2,99)
FHL3 ;  
  IF $GET(OPTION("NO META")) GOTO FHLDN
  ;"//kt 7/6/21 IF $$PTNOTFOUND^TMGHL7E(TMGRESULT),$$IGNORPT^TMGHL7E(TMGRESULT) DO  GOTO FHLDN
  ;" . SET TMGRESULT=1  ;"clear error state
  SET TEMPRESULT=$$MAKEMETA(+TMGRESULT,FPNAME,.DONEPATH,.OPTION,.IEN772,.IEN773)
FHL4 ;  
  IF (TMGRESULT'>0),(TEMPRESULT'>0) DO
  . IF +TMGRESULT=1 SET TMGRESULT=TEMPRESULT
  . ELSE  SET TMGRESULT=TMGRESULT_" AND ALSO "_$PIECE(TEMPRESULT,"^",2,99)
  . DO SETALERT(TMGRESULT,.TMGENV,.OPTION)
FHLDN  ;  
  QUIT TMGRESULT
  ;                           
HL772IMPORT(TMGENV,NOALERT,OPTION)   ;" -- NOTE: Code included in HLFILEIMPORT, making this redundant
  ;"Purpose: Import of HL7 message already saved to database 
  ;"Input:  NOALERT -- Optional.  If 1 then no alert made. 
  ;"       OPTION -- OPTIONAL.  PASS BY REFERENCE. See HL7PROCESS for description
  ;"            OPTION("HL7 DATE")=<HL7 DATE>  <-- an OUT parameter
  ;"            OPTION("FM DATE")=<FMDT>       <-- an OUT parameter
  ;"            OPTION("DFN")=<patient IEN>    <-- an OUT parameter         
  ;"            OPTION("FILEPATHNAME")=<FilePathName> <-- provide for reference.            
  ;"Results: 1 if OK, or -1^Message IF error
  NEW TMGRESULT,TMGMSG,TMGINFO
  IF $GET(TMGLOG) DO TMGLOG^TMGHL7U("@@")  ;"kill message log for all processes.
  IF $GET(TMGLOG) DO TMGLOG^TMGHL7U("Starting HL772IMPORT^TMGHL72 to process HL7 message")
  SET IEN772=$GET(TMGENV("IEN 772"))
  SET IEN773=$GET(TMGENV("IEN 773"))
  SET TMGRESULT=$$FROM772^TMGHL7U2(IEN772,IEN773,.TMGMSG) ;"Pull HL7 message from database into array
  IF TMGRESULT<0 GOTO HLIERR        
  IF $GET(TMGLOG) DO TMGLOG^TMGHL7U("Here is message array",.TMGMSG) 
  SET TMGRESULT=$$HLMSGIMPORT(.TMGMSG,.NOALERT,.OPTION,.TMGENV)
  IF $GET(TMGLOG) DO TMGLOG^TMGHL7U("After processing message, result was: "_TMGRESULT)
  GOTO H7IN2DN  ;"NOTE: if HLMSGIMPORT encountered error, it will file it's own alerts          
HLIERR ;
  IF (TMGRESULT<0),(+$GET(NOALERT)'=1) DO  
  . DO SETALERT(TMGRESULT,.TMGENV,.OPTION)
H7IN2DN  ;
  QUIT:$QUIT TMGRESULT
  QUIT
  ;        
HLMSGIMPORT(TMGMSG,NOALERT,OPTION,TMGENV)  ;" PARSE, then XFORM, then FILE
  ;"Purpose: Entry point to process message: parse, transform, and file.  
  ;"Input: TMGMSG -- PASS BY REFERENCE.  HL7 message to process.  Format
  ;"          TMGMSG(#)=<line of HL7 message>
  ;"       NOALERT -- OPTIONAL.  If 1 then no alert is created for errors
  ;"          NOTE: if OPTION("GUI")=1, then this value is forced to 1.
  ;"       OPTION -- OPTIONAL.  PASS BY REFERENCE. See HL7PROCESS for description
  ;"            OPTION("HL7 DATE")=<HL7 DATE>  <-- an OUT parameter
  ;"            OPTION("FM DATE")=<FMDT>       <-- an OUT parameter
  ;"            OPTION("DFN")=<patient IEN>    <-- an OUT parameter         
  ;"            OPTION("FILEPATHNAME")=<FilePathName> <-- provided for reference.   
  ;"Uses variables in global scope: IEN772, IEN773 (but OK if not defined)
  ;"Results: 1 if OK, or -1^Message if error
  NEW TMGHL7MSG,TMGU,ADFN
  NEW TMGRESULT SET TMGRESULT=1
  SET NOALERT=+$GET(NOALERT)
  IF $DATA(TMGENV)=0 SET TMGRESULT=$$SETUPENV^TMGHL7U(.TMGMSG,.TMGENV)    
  IF TMGRESULT<0 GOTO HLI3ERR  
  IF +$GET(OPTION("GUI")) SET NOALERT=1
  SET TMGRESULT=$$HL7PROCESS(.TMGHL7MSG,.TMGENV,.TMGMSG,.OPTION) ;"PARSE, then XFORM
  NEW HL7DT SET HL7DT=$GET(TMGHL7MSG(1,7))
  SET OPTION("HL7 DATE")=HL7DT
  NEW FMDT SET FMDT=$$HL72FMDT^TMGHL7U3(HL7DT),OPTION("FM DATE")=FMDT
  NEW PIDIDX SET PIDIDX=+$ORDER(TMGHL7MSG("B","PID",""))
  SET ADFN=+$GET(TMGHL7MSG(PIDIDX,4)),OPTION("DFN")=ADFN
  IF $GET(TMGLOG) DO TMGLOG^TMGHL7U("After HL7PROCESS, here is parsed message array",.TMGHL7MSG) 
  IF TMGRESULT<0 GOTO HLI3ERR
  IF +$GET(OPTION("GUI","NO FILE"))=1 GOTO H7IN3DN
  SET TMGRESULT=$$FILEMSG(.TMGENV,.TMGHL7MSG)
  IF TMGRESULT<0 GOTO HLI3ERR
HLI3ERR  ;
  IF (TMGRESULT<0),(NOALERT'=1) DO SETALERT(TMGRESULT,.TMGENV,.OPTION)
H7IN3DN  ;
  QUIT TMGRESULT
  ;
HL7PROCESS(TMGHL7MSG,TMGENV,TMGMSG,OPTION) ;" PARSE, then XFORM
  ;"Purpose: Entry point to process message, stored in TMGMSG
  ;"Input: TMGHL7MSG -- PASS BY REFERENCE, AN OUT PARAMETER.
  ;"       TMGENV -- PASS BY REFERENCE. Lab environment
  ;"           TMGENV("PREFIX") -- e.g. "LMH"
  ;"           TMGENV("IEN 68.2") -- IEN in LOAD/WORK LIST (holds orderable items)
  ;"           TMGENV("IEN 62.4") -- IEN in AUTO INSTRUMENT (holds resultable items)
  ;"           TMGENV(<other entries>)= etc.              
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
  IF +$GET(OPTION("GUI")) SET NOALERT=1 MERGE TMGENV("GUI")=OPTION("GUI")
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
  NEW TMGU MERGE TMGU=TMGENV("TMGU")
  SET TMGRESULT=$$PRSEARRY^TMGHL7X2(,.TMGMSG,.TMGHL7MSG,.TMGU) ;"Split message into tree structure.  No transformations 
  SET TMGRESULT=$$CYCLEXFMSG^TMGHL7X(.TMGENV,.TMGHL7MSG)  ;"XForm message through "PRE", "FINAL" stages etc.  
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
  IF $GET(TMGLOG) DO TMGLOG^TMGHL7U("After call to "_TAG_"^"_ROUTINE_", result was: "_TMGRESULT)
FMSGDN   ;
  QUIT TMGRESULT
  ;
SETALERT(TMGRESULT,TMGENV,OPTION) ;"Send error alert for HL7 message
  ;"Input:  TMGRESULT
  ;"        TMGENV
  ;"        OPTION("FILEPATHNAME") -- full filepathname of source HL7 file 
  ;"        OPTION("DONEPATH") -- Optional.  If provided, then specifies root of folder to moved completed messages
  ;"        OPTION("HL7DATE")  -- Optional.  HL7 date of message, if "", then filled with NOW
  NEW IGNORE SET IGNORE=0
  ;"NOTE: Line below shouldn't be needed as supposed to be filtered downstream.  
  IF $$PTNOTFOUND^TMGHL7E(TMGRESULT),$$IGNORPT^TMGHL7E(TMGRESULT) GOTO ALTDN
  IF $$DIRSETUP(-1,.OPTION) ;"Set up OPTION vars to be saved for use during alert processing.
  IF $$DIRSETUP(1,.OPTION)  ;"Set up OPTION vars to be saved for use during alert processing.
  IF $$DIRSETUP(2,.OPTION)  ;"Set up OPTION vars to be saved for use during alert processing.
  NEW IEN22720 SET IEN22720=+$GET(TMGENV("IEN 22720"))
  ;"//kt 1/13/21 -- IF IEN22720'>0 GOTO ALTDN   ;"No way to send error message, so just leave
  NEW IEN772 SET IEN772=$GET(TMGENV("IEN 772"))
  NEW IEN773 SET IEN773=$GET(TMGENV("IEN 773"))
  NEW NODE SET NODE=$GET(^TMG(22720,IEN22720,100))
  NEW TAG SET TAG=$PIECE(NODE,"^",3) 
  ;"//kt 1/13/21 -- IF TAG="" GOTO ALTDN
  NEW ROUTINE SET ROUTINE=$PIECE(NODE,"^",4) 
  ;"//kt 1/13/21 -- IF ROUTINE="" GOTO ALTDN
  IF (NODE="")!(TAG="") DO
  . SET TAG="SETALERT",ROUTINE="TMGHL7E"
  NEW CODE SET CODE="SET TMGRESULT=$$"_TAG_"^"_ROUTINE_"(.TMGRESULT,,IEN772,IEN773)"
  NEW TRAPECODE SET TRAPECODE=""
  DO
  . NEW $ETRAP SET $ETRAP="SET TRAPECODE=$ECODE,$ETRAP="""",$ECODE="""""
  . XECUTE CODE
  IF $$APPENDLOG(.OPTION,"Error encountered, alert sent.")  ;"ignore result. 
ALTDN  ;
  QUIT 
  ;
MOVE(MODE,OPTION)  ;"Move HL7 messages to destination folders    
  ;"INPUT: MODE:   -1 --> store in failed folder 
  ;"                0 --> store in LOG folder.  
  ;"                1 --> store in success folder
  ;"                2 --> store in discard folder
  ;"                3 --> stored in split folder.  
  ;"       OPTION("FILEPATHNAME") -- full filepathname of source HL7 file 
  ;"       OPTION("DONEPATH") -- Optional.  If provided, then specifies root of folder to moved completed messages
  ;"       OPTION("HL7DATE")  -- HL7 date of message, if "", then filled with NOW
  ;"       OPTION("SUCCESS STORE FPATH") -- OPTIONAL
  ;"       OPTION("SUCCESS STORE FNAME") -- OPTIONAL
  ;"       OPTION("FAILURE STORE FPATH") -- OPTIONAL
  ;"       OPTION("FAILURE STORE FNAME") -- OPTIONAL
  NEW TMGRESULT SET TMGRESULT="1^OK"
  NEW OUTNAME,OUTPATH  
  NEW STR SET STR=$PIECE($$MODENAME(MODE),"^",1)
  SET OUTPATH=$GET(OPTION(STR_" STORE FPATH"))
  SET OUTNAME=$GET(OPTION(STR_" STORE FNAME"))
  IF (OUTPATH="")!(OUTNAME="") SET TMGRESULT=$$DIRSETUP(.MODE,.OPTION,.OUTPATH,.OUTNAME)
  IF TMGRESULT<1 GOTO MVDN
  NEW FILEPATHNAME SET FILEPATHNAME=$GET(OPTION("FILEPATHNAME"))
  HANG 1  ;"for some reason seems to be needed.  
  NEW TEMPRESULT SET TEMPRESULT=$$MOVE^TMGKERNL(FILEPATHNAME,OUTPATH_OUTNAME)   ;"result of 0 means OK
  KILL OPTION(STR_" STORE FNAME")  ;"2/11/21 - This remained resident and kept the last file name between runs 
  IF TEMPRESULT>0 DO
  . NEW TEMP SET TEMP="Unable to move file '"_OUTNAME_"' to folder '"_OUTPATH_"'"
  . SET TEMP=TEMP_" (exec code="_TEMPRESULT_")"
  . SET TMGRESULT="-1^"_TEMP
MVDN ;
  QUIT TMGRESULT
  ; 
DIRSETUP(MODE,OPTION,OUTPATH,OUTNAME)  ;"Determine destination folders for HL7 move    
  ;"INPUT: MODE:   -1 --> store in failed folder 
  ;"                0 --> store in LOG folder.  
  ;"                1 --> store in success folder
  ;"                2 --> store in discard folder
  ;"                3 --> stored in split folder.  
  ;"       OPTION("FILEPATHNAME")  -- full filepathname of source HL7 file 
  ;"       OPTION("DONEPATH") -- Optional.  If provided, then specifies root of folder to moved completed messages
  ;"       OPTION("HL7DATE")  -- HL7 date of message, if "", then filled with NOW
  ;"       OUTPATH -- PASS BY REF, AN OUT PARAM.  
  ;"       OUTNAME -- PASS BY REF, AN OUT PARAM.  
  ;"RESULT: returns 1^OK, or -1^error message  
  NEW TMGRESULT SET TMGRESULT="1^OK"  
  NEW FILEPATHNAME SET FILEPATHNAME=$GET(OPTION("FILEPATHNAME"))
  NEW DONEPATH SET DONEPATH=$GET(OPTION("DONEPATH"))
  NEW HL7DATE SET HL7DATE=$GET(OPTION("HL7DATE"))
  DO SPLITFPN^TMGIOUTL(FILEPATHNAME,.OUTPATH,.OUTNAME,"/")
  SET OUTPATH=$$DESTFLDR(MODE,FILEPATHNAME,.DONEPATH,.HL7DATE)
  IF +OUTPATH<0 SET TMGRESULT=OUTPATH GOTO MVSUDN
  ;"NEW STR SET STR=$SELECT(MODE=1:"SUCCESS",MODE=0:"LOG",1:"FAILURE")
  NEW STR SET STR=$PIECE($$MODENAME(MODE),"^",1)
  SET OPTION(STR_" STORE FPATH")=OUTPATH
  SET OPTION(STR_" STORE FNAME")=OUTNAME
MVSUDN  ;
  QUIT TMGRESULT
  ;
MODENAME(MODE) ;"Convert mode number to names
  NEW FLDRARR  
  SET FLDRARR(-1)="FAILURE^Failed_Messages"
  SET FLDRARR(0)="LOG^LOG"
  SET FLDRARR(1)="SUCCESS^Processed"
  SET FLDRARR(2)="DISCARD^Discarded"
  SET FLDRARR(3)="SPLIT^Processed/Split_Messages"
  NEW RESULT SET RESULT=$GET(FLDRARR(MODE))
  IF RESULT="" SET RESULT=$GET(FLDRARR(-1))
  QUIT RESULT
  ;
MAKEMETA(MODE,FNAME,DONEPATH,OPTION,IEN772,IEN773)  ;"Store metadata about source HL7 file in TMG HL7 MESSAGE STORAGE
  ;"INPUT: MODE--  -1 --> store in failed folder 
  ;"                0 --> store in LOG folder.  
  ;"                1 --> store in success folder
  ;"                2 --> store in discard folder
  ;"                3 --> stored in split folder.  
  ;"       FNAME -- full filepathname of source HL7 file 
  ;"       DONEPATH -- Optional.  If provided, then specifies root of folder to moved completed messages
  ;"       OPTION --  PASS BY REFERENCE. See HL7PROCESS for description
  ;"            OPTION("HL7 DATE")=<HL7 DATE>  
  ;"            OPTION("FM DATE")=<FMDT>       
  ;"            OPTION("DFN")=<patient IEN>          
  ;"       IEN772 -- IEN of stored HL7 message in database, in 772
  ;"       IEN773 -- IEN of stored HL7 message in database, in 773
  ;"RESULT: TMGRESULT=1^OK OR -1^ERROR MESSAGE
  NEW TMGRESULT SET TMGRESULT="1^OK"
  NEW OUTNAME,OUTPATH         
  DO SPLITFPN^TMGIOUTL(FNAME,.OUTPATH,.OUTNAME,"/")
  NEW HL7DATE SET HL7DATE=$GET(OPTION("HL7 DATE")) 
  IF HL7DATE="" DO  GOTO MKMTDN  
  . SET TMGRESULT="-1^Unable to find HL7 DATE in OPTION variable."
  SET OUTPATH=$$DESTFLDR(MODE,FNAME,.DONEPATH,HL7DATE)
  IF +OUTPATH<0 SET TMGRESULT=OUTPATH GOTO MKMTDN
  NEW ADFN SET ADFN=$GET(OPTION("DFN")) IF ADFN'>0 DO  GOTO MKMTDN
  . SET TMGRESULT="-1^Unable to find DFN in OPTION variable."
  NEW FMDT SET FMDT=$GET(OPTION("FM DATE")) IF FMDT'>0 DO  GOTO MKMTDN
  . SET TMGRESULT="-1^Unable to find FMDT in OPTION variable."
  SET TMGRESULT=$$MAKEREC^TMGHL7U4(ADFN,FMDT,OUTPATH,OUTNAME,.IEN772,.IEN773)           
MKMTDN  ;
  QUIT TMGRESULT
  ;
DESTFLDR(MODE,FNAME,DONEPATH,HL7DATE)  ;
  ;"INPUT: MODE--  -1 --> store in failed folder 
  ;"                0 --> store in LOG folder.  
  ;"                1 --> store in success folder
  ;"                2 --> store in discard folder
  ;"                3 --> stored in split folder.  
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
  ;"NEW DESTFOLDER SET DESTFOLDER=$SELECT(MODE=1:"Processed",MODE=0:"LOG",MODE="SPLIT":"Processed/Split_Messages",1:"Failed_Messages")
  NEW DESTFOLDER SET DESTFOLDER=$PIECE($$MODENAME(MODE),"^",2) 
  IF DESTFOLDER="" SET DESTFOLDER=$GET(FLDRARR(-1))
  SET DESTFOLDER=DESTFOLDER_"/"_YEAR_"/"_MONTH_"/"
  SET OUTPATH=OUTPATH_DESTFOLDER
  NEW TMGRESULT SET TMGRESULT=$$ENSURDIR^TMGKERNL(OUTPATH)
  IF TMGRESULT>0 SET TMGRESULT=OUTPATH
  QUIT TMGRESULT
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
HL7CHECKSPLIT(OPTION) ;" Check for mulitple patients in 1 HL7 message
 ;"Input: DIRNAME, 
 ;"       OPTION("FILEPATHNAME")  -- full filepathname of source HL7 file 
 ;"       OPTION("DONEPATH") -- OPTIONAL.  If provided, then specifies root of folder to moved completed messages.
 ;"       OPTION("FNAME")=FNAME
 ;"       OPTION("FPATH")=DIRNAME
 ;"Result: 1 if OK, or -1^SPLIT if split, or -1^message if error.
 NEW DIRNAME SET DIRNAME=$GET(OPTION("FPATH"))
 NEW FNAME SET FNAME=$GET(OPTION("FNAME"))
 NEW FPNAME SET FPNAME=$GET(OPTION("FILEPATHNAME"))  
 NEW ARRAY,MSH
 NEW TMGRESULT SET TMGRESULT=$$LOADHL7^TMGHL7U2(FPNAME,.ARRAY,.MSH) ;"LOAD FILE INTO ARRAY
 IF TMGRESULT'>0 GOTO HLCKSPDN
 ;"Check HL7 Array to see if there are more than one MSH or more than one PID
 NEW MSHCOUNT,PIDCOUNT SET (MSHCOUNT,PIDCOUNT)=0
 NEW FILESPLIT SET FILESPLIT=0
 NEW LOGWRITTEN SET LOGWRITTEN=0
 NEW ARRAYTOSAVE
 NEW IDX SET IDX=0
 FOR  SET IDX=$ORDER(ARRAY(IDX)) QUIT:(IDX'>0)!(+TMGRESULT'>0)  DO
 . NEW LINE SET LINE=$GET(ARRAY(IDX)) QUIT:LINE=""
 . NEW SEG SET SEG=$$UP^XLFSTR($EXTRACT(LINE,1,3))
 . IF SEG="MSH" SET MSHCOUNT=MSHCOUNT+1
 . IF SEG="PID" SET PIDCOUNT=PIDCOUNT+1
 . IF (MSHCOUNT>1)!(PIDCOUNT>1) DO  QUIT
 . . IF LOGWRITTEN=0 DO  QUIT:TMGRESULT'>0
 . . . SET TMGRESULT=$$APPENDLOG(.OPTION,"Splitting HL7 file because it contains multiple messages:"_FPNAME)
 . . . SET LOGWRITTEN=1
 . . SET TMGRESULT=$$SAVESUBMSG(DIRNAME,FNAME,.ARRAYTOSAVE,.OPTION)
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
 . . IF $$SAVESUBMSG(DIRNAME,FNAME,.ARRAYTOSAVE,.OPTION)  ;"DROP RESULT
 . . ;"DO SPLITFPN^TMGIOUTL(NEWFILENAME,.FPATH,.FNAME)
 . . ;"DO SAVEMSGACTUAL^TMGHL7U2(FPATH,FNAME,.ARRAYTOSAVE)
 . SET TMGRESULT="-1^SPLIT"
 . NEW OPTION2 MERGE OPTION2=OPTION
 . DO MOVE(3,.OPTION2)
HLCKSPDN ;
 QUIT TMGRESULT
 ;"
SAVESUBMSG(DIRNAME,PARENTFNAME,ARRAY,OPTION) ;
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
 IF TMGRESULT>0 SET TMGRESULT=$$APPENDLOG(.OPTION,"Created new HL7 file :"_FPATH_FNAME)  
 QUIT TMGRESULT
 ;
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
 ;
CHECKLONGZEF(TMGMSG,ZEFOUT) ;"Look for a long ZEF segment, which is an embedded file
 ;"NOTE: I was having crashes because ZEF segment was > 32k chars in length
 NEW IDX SET IDX=0
 FOR  SET IDX=$ORDER(TMGMSG(IDX)) QUIT:+IDX'>0  DO
 . NEW SEG SET SEG=$EXTRACT(TMGMSG(IDX),1,3)
 . IF SEG'="ZEF" QUIT
 . MERGE ZEFOUT(IDX)=TMGMSG(IDX) 
 . KILL TMGMSG(IDX)
 QUIT
 ;
APPENDLOG(OPTION,ENTRYSTR) ;"Append log file to add Entrystr
  ;"NOTE: Each entry will be prepended with date-time stamp
  ;"      It is expected that each EntryStr will be just 1 line.
  ;"INPUT: OPTION: Pass by reference
  ;"          OPTION("FILEPATHNAME") -- full filepathname of source HL7 file 
  ;"          OPTION("FNAME")=FNAME
  ;"          OPTION("FPATH")=DIRNAME
  ;"          OPTION("DONEPATH") -- Optional.  If provided, then specifies root of folder to moved completed messages
  ;"          OPTION("LOG STORE FPATH") -- OPTIONAL
  NEW TMGRESULT SET TMGRESULT="1^OK"
  NEW HANDLE SET HANDLE="TMGLOGHANDLE-"_$J
  NEW PATH
  DO DIRSETUP(0,.OPTION,.PATH)
  NEW DTSTR SET DTSTR=$$FMTE^XLFDT($$NOW^XLFDT,"5ZPS")
  NEW MSG SET MSG=DTSTR_": "_ENTRYSTR
  NEW CMD SET CMD="echo '"_MSG_"' >> "_PATH_"LOG.txt"
  SET TMGRESULT=$$LINUXCMD^TMGKERNL(CMD)
  ;" DO OPEN^%ZISH(HANDLE,PATH,"LOG.txt","A")
  ;" IF POP DO  GOTO ALDN
  ;" . SET TMGRESULT="-1^Error opening file for append: "_PATH_FNAME
  ;" 
  ;" USE IO
  ;" WRITE DTSTR,": ",ENTRYSTR,!
  ;" DO CLOSE^%ZISH(HANDLE)
ALDN ;
  QUIT TMGRESULT