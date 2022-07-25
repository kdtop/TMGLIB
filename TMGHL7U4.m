TMGHL7U4 ;TMG/kst-HL7 utility functions ; 3/6/18, 3/27/21
              ;;1.0;TMG-LIB;**1**;3/6/18
 ;
 ;"TMG HL7 UTILITY FUNCTIONS 
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 03/06/18  Kevin S. Toppenberg MD
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
 ;"MENU -- User menu to interact with system
 ;"SCANPATHGRP  -- Scan Pathgroup folder for HL7 messages to process
 ;"SCANLAUGHLN -- Scan Laughlin folder for HL7 messages to process
 ;"PICKSCAN -- Scan arbitrary folder for HL7 messages to process 
 ;"SCANDIR(PATH) --Scan specified folder for HL7 messages to process
 ;"GETMDATA(OUT,ADFN,SDT,EDT,OPTION) -- API to get metadata from file 22720.5 (TMG HL7 MESSAGE STORAGE)
 ;"GTHL7LST(OUT,ADFN,SDT,EDT,FILTER) -- RPC: TMG CPRS LAB HL7 LIST
 ;"GTHL7MSG(OUT,PATH,FILE,HTML) - retrieve HL7 message into array, in HTML format (optional)
 ;
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;"TESTGMD() -- Test caller for GETMDATA()
 ;"FINDNULLPATH  -- find errors from older runs
 ;"HNDL1FIL(PATH,FNAME)  
 ;"MAKEREC(TMGDFN,TMGDT,PATH,FNAME,IEN772,IEN773,OPTION) 
 ;"SELHL7(SELARR,OPTION,TMGDFN)  --Browse to select patient HL7 messages
 ;"PICK1(SELARR) --Pick 1 message from SELARR
 ;"GETINFO(IENS,INFO)  -- Fill INFO array with details of HL7 message
 ;"VIEW1(INFO) 
 ;"EDIT1(INFO) 
 ;"GETFNAMES(SELARR,OUT) 
 ;"LISTCT(SELARR) -- Count number of entries
 ;"COPYFILS(FILESARR,DESTDIR)  
 ;"SHOWDIFF(SELARR)  -- SHOW DIFF OF FILES
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"=======================================================================
 ;
TESTGMD() ;"Test caller for GETMDATA()
  NEW TMG DO GETMDATA^TMGHL7U4(.TMG,164,3220300,,1) ZWR TMG
  QUIT
  ;
GETMDATA(OUT,ADFN,SDT,EDT,OPTION)  ;"API to get metadata from file 22720.5 (TMG HL7 MESSAGE STORAGE)
 ;"INPUT: OUT -- PASS BY REFERENCE, AND OUT PARAMETER.  Format as below
 ;"       ADFN -- patient IEN
 ;"       SDT -- Beginning of date range to retrieve.  FM Format.  OPTIONAL.  Default is 0.
 ;"       EDT -- End of date range to retrieve.  FM Format.  OPTIONAL.  Default is 999999999
 ;"       OPTION -- optional switches:
 ;"         OPTION("FILTERBAD")=1  -- OPTIONAL.  If 1, then each metadata is filtered out if file does not exist on server HFS.
 ;"RESULTS: None. OUT is modified.  Format:
 ;"    OUT(#)=<storage node 0> <-- holds all data at time of this API creation. 
 ;"           Piece #1 = DATE OF MESSAGE 
 ;"           Piece #2 = PATH            
 ;"           Piece #3 = FILE NAME       
 ;"           Piece #4 = LINK to file #772 (HL7 MESSAGE TEXT)        
 ;"           Piece #5 = LINK to file #773  (HL7 MESSAGE ADMINISTRATION) <-- header information etc.      
 ;"           Piece #6 = ADDED DATE      
 ;"     e.g. OUT(123)="3180306^/mnt/WinServer/PathgroupHL7/Processed/2013/08/^12C2054522LAB130826123421.txt^^^3180306.192044"
 ;"          OUT(456)= ...
 ;
 SET SDT=+$GET(SDT) 
 SET EDT=+$GET(EDT) IF EDT=0 SET EDT=999999999
 NEW FILTERBAD SET FILTERBAD=+$GET(OPTION("FILTERBAD"))
 SET ADFN=+$GET(ADFN)
 IF SDT>0 SET SDT=SDT-0.00000001  ;"Backup up so $order() will get first SDT entry
 NEW ADT SET ADT=SDT
 FOR  SET ADT=$ORDER(^TMG(22720.5,"DT",ADT)) QUIT:((ADT>EDT)!(ADT=""))  DO
 . NEW SUBIEN SET SUBIEN=0
 . FOR  SET SUBIEN=$ORDER(^TMG(22720.5,"DT",ADT,ADFN,SUBIEN)) QUIT:SUBIEN'>0  DO
 . . NEW NODE SET NODE=$GET(^TMG(22720.5,ADFN,1,SUBIEN,0)) QUIT:NODE=""
 . . NEW PATH SET PATH=$PIECE(NODE,"^",2)
 . . NEW FNAME SET FNAME=$PIECE(NODE,"^",3)
 . . IF FILTERBAD,($$ISFILE^TMGKERNL(PATH_FNAME)=0) QUIT ;"Don't return metadata if file not found on HFS
 . . SET OUT(SUBIEN)=NODE
 QUIT
 ; 
GTHL7LST(OUT,ADFN,SDT,EDT,FILTER)  ;"RPC: TMG CPRS LAB HL7 LIST
 ;"INPUT: OUT -- PASS BY REFERENCE, AND OUT PARAMETER.  Format as below
 ;"       ADFN -- patient IEN
 ;"       SDT -- Beginning of date range to retrieve.  FM Format.  OPTIONAL.  Default is 0.
 ;"       EDT -- End of date range to retrieve.  FM Format.  OPTIONAL.  Default is 999999999
 ;"       FILTER  -- OPTIONAL.  PASS BY REFERENCE.  Optional switches.  Examples 
 ;"              Changing this from below to this (for ease from RPC):
 ;"                 ADT^RAD^LAB with each being 0 or 1 with 1 filtering. 0 will be assumed.
 ;"             FILTER("ADT")=1  filter out ADT messages
 ;"             FILTER("RAD")=1  filter out RAD messages
 ;"             FILTER("LAB")=1  filter out LAB messages
 ;"RESULTS: None. OUT is modified.  Format:
 ;"    OUT(#)=<storage node 0> <-- holds all data at time of this API creation. 
 ;"           Piece #1 = DATE OF MESSAGE 
 ;"           Piece #2 = PATH            
 ;"           Piece #3 = FILE NAME       
 ;"           Piece #4 = LINK to file #772 (HL7 MESSAGE TEXT)        
 ;"           Piece #5 = LINK to file #773  (HL7 MESSAGE ADMINISTRATION) <-- header information etc.      
 ;"           Piece #6 = ADDED DATE      
 ;"     e.g. OUT(123)="3180306^/mnt/WinServer/PathgroupHL7/Processed/2013/08/^12C2054522LAB130826123421.txt^^^3180306.192044"
 ;"          OUT(456)= ...
 NEW RESULTARR
 ;"NEW FILTERADT SET FILTERADT=+$GET(FILTER("ADT"))
 ;"NEW FILTERLAB SET FILTERLAB=+$GET(FILTER("LAB"))
 ;"NEW FILTERRAD SET FILTERRAD=+$GET(FILTER("RAD"))
 NEW FILTERADT SET FILTERADT=+$PIECE($GET(FILTER),"^",1)
 NEW FILTERRAD SET FILTERRAD=+$PIECE($GET(FILTER),"^",2)
 NEW FILTERLAB SET FILTERLAB=+$PIECE($GET(FILTER),"^",3)
 NEW FILTER SET FILTER=FILTERLAB!FILTERADT!FILTERRAD
 SET EDT=$$ADDDAYS^TMGDATE(1,EDT)
 NEW OPTION SET OPTION("FILTERBAD")=1
 DO GETMDATA(.RESULTARR,ADFN,SDT,EDT,.OPTION)
 IF FILTER=1 DO
 . NEW RESULTIDX SET RESULTIDX=0
 . FOR  SET RESULTIDX=$ORDER(RESULTARR(RESULTIDX)) QUIT:RESULTIDX'>0  DO
 . . NEW LINE SET LINE=$GET(RESULTARR(RESULTIDX))
 . . ;"note: this is specific to file-naming formats from Ballad hospital system.
 . . NEW FNAME SET FNAME=$PIECE(LINE,"^",3)
 . . NEW PATH SET PATH=$PIECE(LINE,"^",2)
 . . IF FILTERADT,FNAME["ADT_" QUIT
 . . IF FILTERADT,FNAME["ToppenbergADT" QUIT   
 . . IF FILTERRAD,PATH["Laughlin",FNAME["RAD_" QUIT   
 . . ;"IF FILTERLAB,PATH["Laughlin",FNAME["LAB_" QUIT   ;"NOTE: Will need to filter both ballad and NON-Ballad names for labs.
 . . IF FILTERLAB,PATH["Laughlin",FNAME["LAB" QUIT   ;"NOTE: Will need to filter both ballad and NON-Ballad names for labs.
 . . IF FILTERLAB,PATH["Pathgroup" QUIT   ;"NOTE: Will need to filter both ballad and NON-Ballad names for labs.
 . . SET OUT(RESULTIDX)=LINE
 ELSE  DO
 . MERGE OUT=RESULTARR
 QUIT
 ;"
GTHL7MSG(OUT,PATH,FILE,HTML)  ;"retrieve HL7 message into array, in HTML format (optional)
 ;"INPUT:  OUT -PASS BY REFERANCE, AN OUT PARAMETER.   
 ;"        PATH - path of file, with trailing divider
 ;"        FILE - filename of file to retrieve.
 ;"        HTML - OPTIONAL.  Default is 1.  If 1 then wrapped in HTMl tabs. 
 ;"RESULT: none.  
 ;"OUTPUT: OUT is filled.  Format:  OUT(#)=<line of hl7 message>
 ;
 ;"SET OUT(0)="PATH IS "_PATH
 ;"SET OUT(1)="FILE IS "_FILE
 NEW RESULT,TMGMSG,MSH
 SET HTML=+$GET(HTML,1)
 SET RESULT=$$LOADHL7^TMGHL7U2(PATH_FILE,.TMGMSG,.MSH)
 ;
 NEW INIDX SET INIDX=0
 NEW OUTIDX SET OUTIDX=1
 FOR  SET INIDX=$O(TMGMSG(INIDX)) QUIT:INIDX'>0  DO
 . IF $PIECE($GET(TMGMSG(INIDX)),"|",1)="ZEF" QUIT  ;"filter out encoded pdf segments. 
 . SET OUT(OUTIDX)=$GET(TMGMSG(INIDX)),OUTIDX=OUTIDX+1
 IF HTML DO
 . SET OUT(0)="<HTML><PRE>"
 . SET OUT(OUTIDX)="</PRE></HTML>"
 QUIT
 ;"
REPROCES(OUT,PATH,FILE)  ;"Send one message back through to be processed
 NEW OPTION SET OPTION("NO MOVE")=1
 SET OUT=$$HLFILEIMPORT^TMGHL71(PATH,FILE,,.OPTION)
 QUIT
 ;"
SCANPATHGRP ;
  DO SCANDIR("/mnt/WinServer/PathgroupHL7/Processed")
  QUIT
  ;
SCANLAUGHLN ;
  DO SCANDIR("/mnt/WinServer/LaughlinHL7/Processed")
  QUIT
  ; 
PICKSCAN ;  
  NEW DEFPATH SET DEFPATH="/mnt/WinServer/"
  NEW OPTION 
  SET OPTION("PATH")=DEFPATH
  SET OPTION("SELECT DIR")=1
  NEW PATH,%,TMGRESULT 
  SET PATH=$$FBROWSE^TMGIOUT2(.OPTION)
  IF PATH="" QUIT
  DO SCANDIR(PATH)
  QUIT
  ;
SCANDIR(PATH) ;"SCAN 1 DIRECTORY (AND ANY CHILDREN DIRECTORIES)
  IF $EXTRACT(PATH,$LENGTH(PATH))'="/" SET PATH=PATH_"/"
  NEW MASK,TMGARR SET MASK("*")=""
  IF $$LIST^%ZISH(PATH,"MASK","TMGARR")  ;"IGNORE RESULT
  NEW FNAME SET FNAME=""
  FOR  SET FNAME=$ORDER(TMGARR(FNAME)) QUIT:FNAME=""  DO
  . NEW PATHFNAME SET PATHFNAME=PATH_FNAME
  . IF $$ISDIR^TMGKERNL(PATHFNAME) DO  QUIT
  . . DO SCANDIR(PATHFNAME)  ;"recursive call
  . NEW RESULT SET RESULT=$$HNDL1FIL(PATH,FNAME)
  . IF RESULT<0 DO
  . . WRITE !,"ERROR filing file: ",PATH,FNAME,!
  . . WRITE "   ",$PIECE(RESULT,"^",2),!
  QUIT
  ;
HNDL1FIL(PATH,FNAME)  ;"
  NEW TMGMSG,MSH,RESULT,TMGDFN,HL7DT,TMGDT
  NEW TMGHL7MSG,TMGU,TMGENV,OPTION
  SET RESULT=$$LOADHL7^TMGHL7U2(PATH_FNAME,.TMGMSG,.MSH)
  IF +RESULT<0 GOTO H1FDN
  SET OPTION("SCREEN OUTPUT TO NULL")=1
  SET RESULT=$$HL7PROCESS^TMGHL71(.TMGHL7MSG,.TMGENV,.TMGMSG,.OPTION)
  IF +RESULT<0 GOTO H1FDN
  NEW PIDIDX SET PIDIDX=+$ORDER(TMGHL7MSG("B","PID",""))
  IF PIDIDX'>0 DO  GOTO H1FDN
  . SET RESULT="-1^Unable to find PID in parsed message array"
  SET TMGDFN=$GET(TMGHL7MSG(PIDIDX,4))
  IF TMGDFN'>0 DO  GOTO H1FDN
  . SET RESULT="-1^Unable to get DFN from parsed message array"
  SET HL7DT=$GET(TMGHL7MSG(1,7))
  IF HL7DT'>0 DO  GOTO H1FDN
  . SET RESULT="-1^Unable to get HL7 message date parsed message array"
  SET TMGDT=$$HL72FMDT^TMGHL7U3(HL7DT)
  IF TMGDT'>0 DO  GOTO H1FDN
  . SET RESULT="-1^Unable to convert HL7 date to FM type date"
  NEW OPTION SET OPTION("NO DUP")=1
  SET RESULT=$$MAKEREC(TMGDFN,TMGDT,PATH,FNAME,0,0,.OPTION)
  IF RESULT<0 GOTO H1FDN
  WRITE "FILED: ",PATH,FNAME,!  
H1FDN ;
  QUIT RESULT
  ;
MAKEREC(TMGDFN,TMGDT,PATH,FNAME,IEN772,IEN773,OPTION) ;
  ;"Input:TMGDFN -- patient IEN
  ;"      TMGDT  -- the DT of the message (i.e. the date provided in the HL7 message)
  ;"      PATH -- the path where the message is stored on the host file system
  ;"      FNAME -- the filename of the message on the host file system
  ;"      IEN772 -- optional.  IEN to file 772 if available
  ;"      IEN773 -- optional.  IEN to file 773 if available
  ;"      OPTION -- optional.  
  ;"          OPTION("NO DUP")=1 means don't store multiple messages with same DT for same patient.
  ;"Result : 1^OK, or -1^Error message  
  NEW RESULT SET RESULT="1^OK"
  SET IEN772=+$GET(IEN772)
  SET IEN773=+$GET(IEN773)
  NEW TMGFDA,TMGIEN,TMGMSG,TMGIENS
  ;"ENSURE TOP LEVEL RECORD FOR DFN EXISTS
  IF $DATA(^TMG(22720.5,TMGDFN))>0 GOTO MR2
  ;"ADD RECORD.  MAKE IEN=TMGDFN
  SET TMGIENS="+1,",TMGIEN(1)=TMGDFN
  SET TMGFDA(22720.5,TMGIENS,.01)="`"_TMGDFN
  DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO MRDN
  . SET RESULT="-1^"_$$GETERSTR^TMGRPC3G(.TMGMSG)
MR2  ;"Check for prior record, if NO DUP
  NEW FOUND SET FOUND=0
  IF $GET(OPTION("NO DUP"))'=1 GOTO MR3
  NEW SUBIEN SET SUBIEN=0
  FOR  SET SUBIEN=$ORDER(^TMG(22720.5,TMGDFN,1,"B",TMGDT,SUBIEN)) QUIT:(SUBIEN'>0)!FOUND  DO
  . NEW ZN SET ZN=$GET(^TMG(22720.5,TMGDFN,1,SUBIEN,0))
  . ;"IF $PIECE(ZN,"^",2)'=PATH QUIT
  . IF $PIECE(ZN,"^",3)'=FNAME QUIT
  . IF +$PIECE(ZN,"^",4)'=IEN772 QUIT
  . IF +$PIECE(ZN,"^",5)'=IEN773 QUIT
  . SET FOUND=SUBIEN
  IF FOUND GOTO MRDN
MR3  ;"ADD SUBRECORDS
  SET TMGIENS="+1,"_TMGDFN_"," KILL TMGIEN
  SET TMGFDA(22720.51,TMGIENS,.01)=TMGDT
  SET TMGFDA(22720.51,TMGIENS,.02)=PATH
  SET TMGFDA(22720.51,TMGIENS,.03)=FNAME
  IF IEN772 SET TMGFDA(22720.51,TMGIENS,.04)="`"_IEN772
  IF IEN773 SET TMGFDA(22720.51,TMGIENS,.05)="`"_IEN773
  SET TMGFDA(22720.51,TMGIENS,.06)=$$NOW^XLFDT
  DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO
  . SET RESULT="-1^"_$$GETERSTR^TMGRPC3G(.TMGMSG)      
MRDN  ;
  QUIT RESULT
  ;
FINDNULLPATH  ;
  NEW TMGDFN SET TMGDFN=0
  FOR  SET TMGDFN=$ORDER(^TMG(22720.5,TMGDFN)) QUIT:TMGDFN'>0  DO
  . NEW NAME SET NAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
  . NEW SUBIEN SET SUBIEN=0
  . FOR  SET SUBIEN=$ORDER(^TMG(22720.5,TMGDFN,1,SUBIEN)) QUIT:SUBIEN'>0  DO
  . . NEW ZN SET ZN=$GET(^TMG(22720.5,TMGDFN,1,SUBIEN,0))
  . . NEW PATH SET PATH=$PIECE(ZN,"^",2)
  . . IF PATH["/201" QUIT
  . . WRITE NAME," ",$$FMTE^XLFDT(+ZN)," ",PATH,!
  . . IF $GET(TMGKILL)'=1 QUIT
  . . NEW DA SET DA=SUBIEN,DA(1)=TMGDFN
  . . NEW DIK SET DIK="^TMG(22720.5,"_TMGDFN_",1,"
  . . DO ^DIK
  QUIT    
  ;"---------------------------------------------------------
  ;"---------------------------------------------------------
  ;"---------------------------------------------------------
  ;"---------------------------------------------------------
MENU(TMGDFN) ;
  NEW MENU,USERPICK,SELARR,SELCT SET SELCT=0
  NEW PLURAL
  WRITE !
  NEW EXCLUDEADT SET EXCLUDEADT=1
  SET TMGDFN=$GET(TMGDFN)
  IF TMGDFN'>0 DO
  . NEW DIC,X,Y
  . SET DIC=22720.5,DIC(0)="MAEQ"
  . DO ^DIC WRITE !,!,!
  . IF Y'>0 QUIT
  . SET TMGDFN=+Y
MN0 ;  
  KILL MENU,IDX SET IDX=0
  SET MENU(0)="Select option for "_$PIECE($GET(^DPT(+TMGDFN,0)),"^",1)_":"
  IF EXCLUDEADT SET MENU(0,1)="Currently EXCLUDING ADT-type HL7 messages"
  SET PLURAL=$SELECT(SELCT>1:"s",1:"")
  IF SELCT=0 DO
  . SET IDX=IDX+1,MENU(IDX)="Select Messages"_$C(9)_"SEL"
  . SET IDX=IDX+1,MENU(IDX)="Toggle Exclude-ADT-Messages mode to "_$SELECT(EXCLUDEADT:"OFF",1:"ON")_$C(9)_"TOGGLEADT"  
  ELSE  DO
  . SET IDX=IDX+1,MENU(IDX)="Unselect "_SELCT_" Message"_PLURAL_$C(9)_"UNSEL"
  . SET IDX=IDX+1,MENU(IDX)="View Message"_PLURAL_$C(9)_"VIEW"
  . SET IDX=IDX+1,MENU(IDX)="View Message"_PLURAL_" Metadata"_$C(9)_"METADATA"
  . SET IDX=IDX+1,MENU(IDX)="Edit Message"_PLURAL_$C(9)_"EDIT"
  . SET IDX=IDX+1,MENU(IDX)="Copy Message"_PLURAL_" File"_PLURAL_$C(9)_"COPY"
  . SET IDX=IDX+1,MENU(IDX)="DIFF Messages"_$C(9)_"DIFF"
  . SET IDX=IDX+1,MENU(IDX)="Setup Lab from Messages"_$C(9)_"SETUP"
  . SET IDX=IDX+1,MENU(IDX)="Debug reprocessing Message"_$C(9)_"DEBUG"  
  . SET IDX=IDX+1,MENU(IDX)="DELETE labs from database"_$C(9)_"DELETE"
  SET IDX=IDX+1,MENU(IDX)="Quit"_$C(9)_"^"
MN1 ;
  SET USERPICK=$$MENU^TMGUSRI2(.MENU)  
  IF USERPICK="^" GOTO MNDN
  IF USERPICK="TOGGLEADT" DO  GOTO MN0
  . SET EXCLUDEADT='EXCLUDEADT
  IF USERPICK="SEL" DO  GOTO MN0
  . NEW OPTION SET OPTION("EXCLUDE ADT")=EXCLUDEADT
  . DO SELHL7(.SELARR,.OPTION,.TMGDFN)
  . SET SELCT=$$LISTCT(.SELARR)
  IF USERPICK="UNSEL" DO  GOTO MN0
  . KILL SELARR SET (SELCT,TMGDFN)=0
  IF USERPICK="VIEW" DO  GOTO MN0
  . NEW INFO,ARR SET INFO=$$PICK1(.SELARR)
  . IF INFO="" QUIT
  . NEW RESULT SET RESULT=$$GETINFO(INFO,.ARR) ;"Fill ARR array with details of HL7 message
  . IF +RESULT'>0 DO  QUIT
  . . WRITE !,$PIECE(RESULT,"^",2),!
  . DO VIEW1(.ARR)
  IF USERPICK="EDIT" DO  GOTO MN0
  . NEW INFO,ARR SET INFO=$$PICK1(.SELARR)
  . IF INFO="" QUIT
  . NEW RESULT SET RESULT=$$GETINFO(INFO,.ARR) ;"Fill ARR array with details of HL7 message
  . IF +RESULT'>0 DO  QUIT
  . . WRITE !,$PIECE(RESULT,"^",2),!
  . DO EDIT1(.ARR)
  IF USERPICK="SETUP" DO  GOTO MN0
  . NEW INFO,ARR SET INFO=$$PICK1(.SELARR)
  . IF INFO="" QUIT
  . NEW RESULT SET RESULT=$$GETINFO(INFO,.ARR) ;"Fill ARR array with details of HL7 message
  . IF +RESULT'>0 DO  QUIT
  . . WRITE !,$PIECE(RESULT,"^",2),!
  . DO SETUP1(.ARR)
  IF USERPICK="DEBUG" DO  GOTO MN0
  . NEW INFO,ARR SET INFO=$$PICK1(.SELARR)
  . IF INFO="" QUIT
  . NEW RESULT SET RESULT=$$GETINFO(INFO,.ARR) ;"Fill ARR array with details of HL7 message
  . IF +RESULT'>0 DO  QUIT
  . . WRITE !,$PIECE(RESULT,"^",2),!
  . DO DEBUG1(.ARR)    
  IF USERPICK="COPY" DO  GOTO MN0
  . NEW PATH,FNAME,OPTION 
  . SET OPTION("SELECT DIR")=1
  . SET OPTION("PATH")="/mnt/WinServer/"
  . NEW DESTDIR SET DESTDIR=$$FBROWSE^TMGIOUT2(.OPTION,.PATH,.FNAME)
  . IF DESTDIR="" QUIT
  . NEW FNAMES DO GETFNAMES(.SELARR,.FNAMES)  
  . NEW CT SET CT=$$LISTCT^TMGMISC2("FNAMES")
  . WRITE "Copy ",CT," file(s) to ",DESTDIR
  . SET %=2 DO YN^DICN 
  . IF %'=1 QUIT
  . NEW TEMP SET TEMP=$$COPYFILS(.FNAMES,DESTDIR) 
  . IF +TEMP=1 WRITE !,"Done.",!,! QUIT
  . WRITE !,"There was a problem:",!,$PIECE(TEMP,"^",2),!
  IF USERPICK="DIFF" DO  GOTO MN1
  . IF SELCT'=2 DO  QUIT
  . . WRITE "Requires exactly 2 messages to be selected for DIFF function",!
  . DO SHOWDIFF(.SELARR)
  IF USERPICK="DELETE" DO  GOTO MN0
  . WRITE !,"NOTE: The HL7 messages selected are different from the actual labs",!
  . WRITE "       stored in the database.  You must select actual labs based on dates.",!
  . DO ASKDELAB^TMGLRWU3(.TMGDFN)
  IF USERPICK="METADATA" DO  GOTO MN0
  . NEW INFO,ARR SET INFO=$$PICK1(.SELARR)
  . DO DUMPREC^TMGDEBU3(22720.51,INFO)
  . DO PRESS2GO^TMGUSRI2
  GOTO MN1
MNDN ;
  QUIT
  ;
SELHL7(SELARR,OPTION,TMGDFN)  ;"Browse to select patient HL7 messages
  ;"Input: SELARR -- pass by reference.  An OUT parameter.  Format:
  ;"            SELARR(Info)=""   e.g. SELARR(<DISPLAY TEXT>,"<IENS>^<DT>")=""  for each selected message
  ;"       OPTION -- optional.  Pass by reference.  Format:
  ;"            OPTION("VERBOSE")=1  if want messages printed out when problems.
  ;"            OPTION("EXCLUDE ADT")=1 if ADT messages should be excluded.  
  ;"       TMGDFN -- an IN and OUT parameter
  ;"Result: none
  NEW MENU,USERPICK
  NEW EXCLUDEADT SET EXCLUDEADT=+$GET(OPTION("EXCLUDE ADT"))
  KILL SELARR 
  IF TMGDFN'>0 DO
  . NEW DIC,X,Y
  . SET DIC=22720.5,DIC(0)="MAEQ"
  . DO ^DIC WRITE !
  . IF Y'>0 QUIT
  . SET TMGDFN=+Y
  IF TMGDFN'>0 GOTO SELHLDN 
  NEW PTNAME SET PTNAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
  NEW DISPARR
  NEW MAX SET MAX=0
  NEW STARTDT,ENDDT SET STARTDT=9999999,ENDDT=0
  NEW TMGDT SET TMGDT=0
  FOR  SET TMGDT=$ORDER(^TMG(22720.5,TMGDFN,1,"B",TMGDT)) QUIT:TMGDT'>0  DO
  . IF TMGDT<STARTDT SET STARTDT=TMGDT
  . IF TMGDT>ENDDT SET ENDDT=TMGDT
  . NEW SUBIEN SET SUBIEN=0
  . FOR  SET SUBIEN=$ORDER(^TMG(22720.5,TMGDFN,1,"B",TMGDT,SUBIEN)) QUIT:SUBIEN'>0  DO
  . . SET MAX=MAX+1  
  SET TMGDT=0
  NEW DELTADAY SET DELTADAY=$$FMDIFF^XLFDT(ENDDT,STARTDT,1)
  NEW DELTAYR SET DELTAYR=DELTADAY\365
  IF DELTAYR<1 GOTO SELHL2
  NEW IDX SET IDX=0
  SET MENU(0)="Select option:"
  NEW T FOR T="1W","1M","3M","6M" DO
  . NEW STR SET STR=$SELECT(T="1W":"1 week",T="1M":"1 month",T="3M":"3 months",T="6M":"6 months",1:"??")
  . SET IDX=IDX+1,MENU(IDX)="Scan last "_STR_" of HL7 messages"_$C(9)_T
  NEW YR FOR YR=1:1:DELTAYR DO
  . SET IDX=IDX+1,MENU(IDX)="Scan last "_YR_" year"_$SELECT(YR>1:"s",1:"")_" of HL7 messages"_$C(9)_YR
  SET IDX=IDX+1,MENU(IDX)="Quit"_$C(9)_"^"
  SET USERPICK=$$MENU^TMGUSRI2(.MENU)  
  IF USERPICK="^" GOTO SELHLDN
  IF +USERPICK'=USERPICK DO
  . NEW T SET T=USERPICK SET USERPICK=$SELECT(T="1W":1/52,T="1M":1/12,T="3M":3/12,T="6M":6/12,1:1)
  SET TMGDT=$$FMADD^XLFDT($$NOW^XLFDT,-USERPICK*365,0,0,1)
  WRITE !,"Scanning from ",$$FMTE^XLFDT(TMGDT,"D")," to present",!
  ;"Count again for smaller range.    
  SET MAX=0
  NEW TEMPDT SET TEMPDT=TMGDT
  FOR  SET TEMPDT=$ORDER(^TMG(22720.5,TMGDFN,1,"B",TEMPDT)) QUIT:TEMPDT'>0  DO
  . NEW SUBIEN SET SUBIEN=0
  . FOR  SET SUBIEN=$ORDER(^TMG(22720.5,TMGDFN,1,"B",TEMPDT,SUBIEN)) QUIT:SUBIEN'>0  DO
  . . SET MAX=MAX+1  
SELHL2 ;
  WRITE "SCANNING & PARSING ",MAX," MESSAGES . . .",!
  IF EXCLUDEADT WRITE "Excluding ADT-type messages",!
  WRITE !
  NEW STARTH SET STARTH=$H
  NEW COUNT SET COUNT=0  
  FOR  SET TMGDT=$ORDER(^TMG(22720.5,TMGDFN,1,"B",TMGDT)) QUIT:TMGDT'>0  DO
  . NEW SUBIEN SET SUBIEN=0
  . FOR  SET SUBIEN=$ORDER(^TMG(22720.5,TMGDFN,1,"B",TMGDT,SUBIEN)) QUIT:SUBIEN'>0  DO
  . . NEW LINE SET LINE="HL7 Msg: "_$$FMTE^XLFDT(TMGDT,"7Z")
  . . NEW IENS SET IENS=SUBIEN_","_TMGDFN_","
  . . NEW ARR,TEMP,TESTNAMES SET TESTNAMES=""
  . . SET COUNT=COUNT+1 DO PROGBAR^TMGUSRI2(COUNT,"%",1,MAX,60,STARTH)
  . . SET TEMP=$$GETINFO(IENS,.ARR)
  . . IF TEMP<1 QUIT
  . . IF EXCLUDEADT=1,($GET(ARR("MESSAGE TYPE"))["ADT") QUIT
  . . SET TESTNAMES=$GET(ARR("TEST NAMES"))
  . . IF TESTNAMES'="" SET LINE=LINE_" ("_TESTNAMES_")"  
  . . NEW INFO SET INFO=IENS_"^"_TMGDT
  . . SET DISPARR(LINE,INFO)=""
  WRITE !
  IF $DATA(DISPARR)=0 DO  QUIT
  . IF $GET(OPTION("VERBOSE")) WRITE !,"No messages found for "_PTNAME,". Aborting.",!
  DO SELECTR2^TMGUSRI3("DISPARR","SELARR","Select HL7 Message(s) for "_PTNAME_" <ESC><ESC> when done.")
  IF $DATA(SELARR)=0 DO  QUIT
  . IF $GET(OPTION("VERBOSE")) WRITE "No messages selected. Aborting.",!
SELHLDN ;  
  QUIT
  ;
GETMETA(RESULT,TMGDFN,SDT,EDT,EXCLUDEADT)  ;"Get metadata IEN's for lab messages
  ;"INPUT: RESULT, PASS BY REFERENCE.  Format as below
  ;"       TMGDFN -- patient IEN
  ;"       SDT -- FMDT format.  Starting time to search for labs.  INCLUSIVE
  ;"       EDT -- FMDT format.  Sending time to search for labs.  INCLUSIVE 
  ;"       EXCLUDEADT -- optional.  DEFAULT = 1.  If true ADT's are excluded. 
  ;"RESULT: none.  Output in RESULT array.  Format:
  ;"            RESULT(DFN,DT,SUBIEN)=IENS^DT^test names
  SET SDT=+$GET(SDT)
  SET EDT=+$GET(EDT) IF EDT'>0 SET EDT="9999999"
  SET EXCLUDEADT=+$GET(EXCLUDEADT,1)
  NEW TMGDT SET TMGDT=SDT-0.0000005  ;"backup up time just before SDT
  FOR  SET TMGDT=$ORDER(^TMG(22720.5,TMGDFN,1,"B",TMGDT)) QUIT:TMGDT'>0  DO
  . NEW SUBIEN SET SUBIEN=0
  . FOR  SET SUBIEN=$ORDER(^TMG(22720.5,TMGDFN,1,"B",TMGDT,SUBIEN)) QUIT:SUBIEN'>0  DO
  . . NEW IENS SET IENS=SUBIEN_","_TMGDFN_","
  . . NEW ARR,TEMP,TESTNAMES SET TESTNAMES=""
  . . SET TEMP=$$GETINFO(IENS,.ARR)
  . . IF TEMP<1 QUIT
  . . IF EXCLUDEADT=1,($GET(ARR("MESSAGE TYPE"))["ADT") QUIT
  . . SET TESTNAMES=$GET(ARR("TEST NAMES"))
  . . NEW INFO SET INFO=IENS_"^"_TMGDT_"^"_TESTNAMES
  . . SET RESULT(TMGDFN,TMGDT,SUBIEN)=INFO
  QUIT
  ;
PICK1(SELARR) ;"Pick 1 message from SELARR
  ;"INPUT: SELARR.  Pass by reference.  Format: SELARR(<DISPLAY TEXT>,"<IENS>^<DT>")="" 
  ;"Result: "" if none selected, or IENS (Format: 'subIEN,ParentIEN,')
  NEW RESULT SET RESULT=""
  NEW MENU,USERPICK,IDX,IENS,SUBIEN,TMGDT,TXT
PMN0 ;  
  KILL MENU,IDX SET IDX=0
  SET MENU(0)="Select HL7 message to act on:"
  SET TXT=""
  FOR  SET TXT=$ORDER(SELARR(TXT)) QUIT:TXT=""  DO
  . NEW INFO SET INFO=""
  . FOR  SET INFO=$ORDER(SELARR(TXT,INFO)) QUIT:INFO=""  DO
  . . SET IENS=$PIECE(INFO,"^",1),TMGDT=$PIECE(INFO,"^",2)
  . . NEW LINE SET LINE="HL7 message date "_$$FMTE^XLFDT(TMGDT,"5ZP")
  . . SET IDX=IDX+1,MENU(IDX)=LINE_$C(9)_IENS
  SET IDX=IDX+1,MENU(IDX)="Quit"_$C(9)_"^"
  IF IDX=2 SET RESULT=$PIECE(MENU(1),$C(9),2) GOTO PMNDN
PMN1 ;
  SET USERPICK=$$MENU^TMGUSRI2(.MENU)  
  IF USERPICK="^" GOTO PMNDN
  SET SUBIEN=$PIECE(USERPICK,",",1)
  SET TMGDFN=$PIECE(USERPICK,",",2)
  IF +SUBIEN=SUBIEN,+TMGDFN=TMGDFN DO  GOTO PMNDN  
  . SET RESULT=USERPICK
  GOTO PMN1  
PMNDN ;
  QUIT RESULT  
  ;
GETINFO(IENS,INFO) ;"Fill INFO array with details of HL7 message
  ;"Input: IENS -- 'subIEN,ParentIEN,'
  ;"       INFO.  Pass by reference.  An OUT parameter.
  ;"Result: 1^OK, or -1^Error Message
  NEW RESULT SET RESULT="1^OK"
  NEW TMGDFN SET TMGDFN=$PIECE(IENS,",",2)
  NEW SUBIEN SET SUBIEN=$PIECE(IENS,",",1)
  NEW PTNAME SET PTNAME=$PIECE($GET(^DPT(TMGDFN,0)),"^",1)
  NEW ZN SET ZN=$GET(^TMG(22720.5,TMGDFN,1,SUBIEN,0))
  SET INFO("PT NAME")=PTNAME
  SET INFO("PATH")=$PIECE(ZN,"^",2)
  SET INFO("FNAME")=$PIECE(ZN,"^",3)
  NEW PATHFNAME SET PATHFNAME=$PIECE(ZN,"^",2)_$PIECE(ZN,"^",3)
  SET INFO("PATHFNAME")=PATHFNAME
  SET INFO("IEN 772")=$PIECE(ZN,"^",4)
  SET INFO("IEN 773")=$PIECE(ZN,"^",5)
  SET INFO("DT ADDED")=$PIECE(ZN,"^",6)
  NEW TMGMSG,MSH
  SET RESULT=$$LOADHL7^TMGHL7U2(PATHFNAME,.TMGMSG,.MSH)
  IF (+RESULT=-1),(RESULT["Error opening file:"),(PATHFNAME["Failed_Messages") DO
  . NEW MOVEDPATHFNAME SET MOVEDPATHFNAME=$$REPLSTR^TMGSTUT3(PATHFNAME,"Failed_Messages","Processed")
  . IF $$ISFILE^TMGKERNL(MOVEDPATHFNAME)'>0 QUIT
  . NEW TEMP SET TEMP=$$LOADHL7^TMGHL7U2(MOVEDPATHFNAME,.TMGMSG,.MSH)
  . IF TEMP'>0 DO  QUIT
  . . SET RESULT="-1^"_$PIECE(RESULT,"^",2)_" AND "_$PIECE(TEMP,"^",2)
  . ;"//found HL7 message, moved to processed folder.  Now updatabase.  
  . NEW PATH,FNAME
  . DO SPLITFPN^TMGIOUTL(MOVEDPATHFNAME,.PATH,.FNAME)
  . IF PATH="" DO  QUIT
  . . SET RESULT="-1^"_$PIECE(RESULT,"^",2)_" AND unable to split to path"
  . NEW TMGFDA,TMGIEN,TMGMSG
  . SET TMGFDA(22720.51,SUBIEN_","_TMGDFN_",",.02)=PATH
  . DO FILE^DIE("","TMGFDA","TMGMSG")
  . IF $DATA(TMGMSG("DIERR")) DO  QUIT
  . . NEW ERR SET ERR=$$GETERRST^TMGDEBU2(.TMGMSG)
  . . SET RESULT="-1^"_$PIECE(RESULT,"^",2)_" AND "_ERR
  . SET RESULT="1^OK"
  MERGE INFO("MSG")=TMGMSG
  SET INFO("MSH")=$GET(MSH)
  IF RESULT<0 GOTO GIDN  
  NEW TMGHL7MSG,TMGU,TMGENV,OPTION
  SET OPTION("SCREEN OUTPUT TO NULL")=1
  SET RESULT=$$SETUPENV^TMGHL7U(.TMGMSG,.TMGENV)    
  IF RESULT<0 GOTO GIDN  
  SET RESULT=$$HL7PROCESS^TMGHL71(.TMGHL7MSG,.TMGENV,.TMGMSG,.OPTION)
  MERGE INFO("TMGHL7MSG")=TMGHL7MSG
  MERGE INFO("ENV")=TMGENV
  SET INFO("MESSAGE TYPE")=$GET(TMGHL7MSG(1,9))
  NEW TESTNAME SET TESTNAME=""
  NEW SEGN SET SEGN=0
  FOR  SET SEGN=$ORDER(TMGHL7MSG("B","OBR",SEGN)) QUIT:SEGN=""  DO
  . NEW ATEST SET ATEST="" 
  . NEW T1 SET T1=$GET(TMGHL7MSG(SEGN,4,1)) IF +T1=T1 SET T1=""
  . NEW T2 SET T2=$GET(TMGHL7MSG(SEGN,4,2)) IF +T2=T2 SET T2=""
  . ;"NEW T0 SET T0=$GET(TMGHL7MSG(SEGN,4))
  . IF T2="" SET ATEST=T1
  . ELSE  IF T1="" SET ATEST=T2
  . IF (T1'="")&(T2'="") DO
  . . IF $LENGTH(T1)<$LENGTH(T2) SET ATEST=T1 QUIT
  . . ELSE  SET ATEST=T2 QUIT
  . IF ATEST="" QUIT
  . IF TESTNAME'="" SET TESTNAME=TESTNAME_", "
  . SET TESTNAME=TESTNAME_ATEST
  IF TESTNAME="" SET TESTNAME="No results.  Message Type="_$GET(TMGHL7MSG(1,9))    
  SET INFO("TEST NAMES")=TESTNAME
GIDN ;  
  QUIT RESULT
  ;
VIEW1(INFO) ;"
  NEW MSG MERGE MSG=INFO("MSG")
  IF $DATA(MSG)=0 DO  QUIT
  . WRITE !,"Nothing to view!",!
  DO VIEWMSG^TMGHL7U2(.MSG)
  QUIT
  ;
EDIT1(INFO) ;"
  NEW MSG MERGE MSG=INFO("MSG")
  IF $DATA(MSG)=0 DO  QUIT
  . WRITE !,"Nothing to edit!",!
  DO EDITMSG^TMGHL7U2(.MSG)
  QUIT
  ;
SETUP1(INFO) ;"
  NEW MSG MERGE MSG=INFO("MSG")
  IF $DATA(MSG)=0 DO  QUIT
  . WRITE !,"Nothing to setup!",!
  DO SETUP^TMGHL70(.MSG)
  QUIT
  ;
DEBUG1(INFO) ;"
  NEW TMGMSG MERGE TMGMSG=INFO("MSG")
  NEW OPTION,TMGRESULT
  WRITE !,"NOTE: This will reprocess the HL7 message, filing lab data into database.",!
  WRITE "But it won't move the HL7 message in the host OS HF, and won't add any new",!
  WRITE "metadata to file 22720.5",!
  WRITE "Continue "
  SET %=1 DO YN^DICN IF %'=1 QUIT
  NEW TMGENV 
  SET TMGRESULT=$$SETUPENV^TMGHL7U(.TMGMSG,.TMGENV)
  IF TMGRESULT<1 DO  QUIT
  . WRITE !,"ERROR: ",$PIECE(TMGRESULT,"^",2,99),!
  NEW CODE SET CODE="SET TMGRESULT=$$HLMSGIMPORT^TMGHL71(.TMGMSG,1,.OPTION,.TMGENV)"
  DO DIRDEBUG^TMGIDE(CODE)
  QUIT
  ;  
GETFNAMES(SELARR,OUT) ;"
  ;"INPUT: SELARR.  Pass by reference.  Format: SELARR(<DISPLAY TEXT>,"<IENS>^<DT>")="" 
  ;"       OUT: Pass by reference.  An OUT parameter.  Format:
  ;"              OUT(#)=Path^FName
  ;"Result: none
  NEW IDX SET IDX=0
  NEW TXT SET TXT=""
  FOR  SET TXT=$ORDER(SELARR(TXT)) QUIT:TXT=""  DO
  . NEW INFO SET INFO=""
  . FOR  SET INFO=$ORDER(SELARR(TXT,INFO)) QUIT:INFO=""  DO
  . . NEW IENS SET IENS=$PIECE(INFO,"^",1)
  . . NEW ARR DO GETINFO(IENS,.ARR)
  . . NEW PATH SET PATH=$GET(ARR("PATH")) QUIT:PATH=""
  . . NEW FNAME SET FNAME=$GET(ARR("FNAME")) QUIT:FNAME=""
  . . IF $EXTRACT(PATH,$LENGTH(PATH))'="/" SET PATH=PATH_"/"
  . . SET IDX=IDX+1,OUT(IDX)=PATH_"^"_FNAME
  QUIT
  ;
LISTCT(SELARR) ;"Count number of entries
  NEW CT SET CT=0
  NEW TXT SET TXT=""
  FOR  SET TXT=$ORDER(SELARR(TXT)) QUIT:TXT=""  DO
  . NEW INFO SET INFO=""
  . FOR  SET INFO=$ORDER(SELARR(TXT,INFO)) QUIT:INFO=""  DO
  . . SET CT=CT+1
  QUIT CT

COPYFILS(FILESARR,DESTDIR)  ;"
  ;"INPUT: FILESARR.  Pass by reference.  Format: FILESARR(#)=PATH^FNAME 
  ;"       DESTDIR: Path of the destination directory.
  ;"Result: 1^OK, or -1^Error Message
  NEW RESULT SET RESULT="1^OK"
  IF $EXTRACT(DESTDIR,$LENGTH(DESTDIR))'="/" SET DESTDIR=DESTDIR_"/"
  NEW IDX SET IDX=""
  FOR  SET IDX=$ORDER(FILESARR(IDX)) QUIT:IDX'>0  DO
  . NEW ENTRY SET ENTRY=$GET(FILESARR(IDX)) QUIT:ENTRY=""
  . NEW PATH SET PATH=$PIECE(ENTRY,"^",1)
  . NEW FNAME SET FNAME=$PIECE(ENTRY,"^",2)
  . IF $EXTRACT(PATH,$LENGTH(PATH))'="/" SET PATH=PATH_"/"
  . NEW SOURCEFILE SET SOURCEFILE=PATH_FNAME
  . NEW DESTFILE SET DESTFILE=DESTDIR_FNAME
  . NEW TEMP SET TEMP=$$Copy^TMGKERNL(SOURCEFILE,DESTFILE)
  . IF TEMP>0 DO
  . . SET RESULT=$SELECT(RESULT="1^OK":"-1^",1:RESULT_" AND ")_"Error copying "_SOURCEFILE_" --> "_DESTFILE
  QUIT RESULT
  ;
SHOWDIFF(SELARR)  ;"SHOW DIFF OF FILES
  NEW FPNAME1,FPNAME2 SET (FPNAME1,FPNAME2)="" 
  NEW MSG1,MSG2
  NEW IDX SET IDX=0
  NEW TXT SET TXT=""
  FOR  SET TXT=$ORDER(SELARR(TXT)) QUIT:(TXT="")!(IDX=2)  DO
  . NEW INFO SET INFO=""
  . FOR  SET INFO=$ORDER(SELARR(TXT,INFO)) QUIT:(INFO="")!(IDX=2)  DO
  . . NEW IENS SET IENS=$PIECE(INFO,"^",1)
  . . NEW ARR DO GETINFO(IENS,.ARR)
  . . NEW PATH SET PATH=$GET(ARR("PATH")) QUIT:PATH=""
  . . NEW FNAME SET FNAME=$GET(ARR("FNAME")) QUIT:FNAME=""
  . . IF $EXTRACT(PATH,$LENGTH(PATH))'="/" SET PATH=PATH_"/"
  . . SET IDX=IDX+1
  . . ;"IF IDX=1 SET FPNAME1=PATH_FNAME MERGE MSG1=ARR("MSG")
  . . ;"IF IDX=2 SET FPNAME2=PATH_FNAME MERGE MSG2=ARR("MSG")
  . . IF IDX=1 MERGE MSG1=ARR("MSG")
  . . IF IDX=2 MERGE MSG2=ARR("MSG")
  ;"NEW OUT 
  ;"NEW PARAMS SET PARAMS=""  ;"SET PARAMS="--side-by-side --text --ignore-trailing-space"
  ;"DO VWMSGDIF^TMGHL7U2(.OUT,.MSG1,.MSG2,PARAMS)
  ;"NEW TEMP SET TEMP=$$EditArray^TMGKERNL(.OUT,"joe")  
  NEW PARAMS SET PARAMS="-o"  
  DO VWMDIF2^TMGHL7U2(.MSG1,.MSG2,PARAMS)
  QUIT
  ;      