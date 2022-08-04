TMGPAT2B  ;TMG/kst/Patching tools Suport;7/24/2022
         ;;1.0;TMG-LIB;**1**;7/24/2022
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 7/24/2022  Kevin S. Toppenberg MD
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
 ;"Private Functions
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;
 ;"NOTE: This file is a rewrite and refactoring of TMGPAT2
 ;
ENSRLOCL(IENS,INFO,MSG,OPTION)  ;"ENSURE LOCAL
  ;"Purpose: Ensure that the files have been downloaded from server and stored locally
  ;"Input: IENS --IENS in 22709.121 (a subfile in 22709.1)
  ;"       INFO --PASS BY REFERENCE, an OUT PARAMETER.
  ;"       MSG -- PASS BY REFERANCE, an OUT PARAMETER.  FORMAT as per ENSRLOCL2 below  
  ;"       OPTION -- optional. OPTION("VERBOSE")=1, means messaages also written directly to output
  ;"Output: INFO will be filled as per ENSRLOCL2 below
  ;"RESULTS: "1^OK", or "-1^Message" if  problem.
  NEW RESULT SET RESULT="1^OK"
  NEW VERBOSE SET VERBOSE=+$GET(OPTION("VERBOSE"))
  NEW DATA DO GETINFO1B^TMGAPT6(.DATA,IENS)
  IF $DATA(DATA)=0 DO  GOTO ELDN
  . DO ADDMSG("Unable to find data in file 227091.121, IENS="_IENS,1,.MSG,VERBOSE)
  . SET RESULT="-1^See MSG"
  SET RESULT=$$ENSRLOCL(.DATA,.INFO,.MSG,.OPTION)  ;"ENSURE LOCAL
ELDN ;
  QUIT RESULT
  ;
ENSRLOCL2(DATA,INFO,MSG,OPTION)  ;"ENSURE LOCAL
  ;"Purpose: Ensure that the files have been downloaded from server and stored locally
  ;"Input: DATA -- PASS BY REFERENCE.  Example:  
  ;"            DATA
  ;"            }~INFOTXT
  ;"            | }~4,3,1,
  ;"            | | }~CONTAINED PATCHES
  ;"            | | | }~"ABSV*4.0*45 SEQ #44" = 0
  ;"            | | }~"NAME" = ABSV-4_SEQ-44_PAT-45.txt
  ;"            | | }~"URL" = foia-vista.worldvista.org/Patches_By_Application/ABSV-Voluntary Service/ABSV-4_SEQ-44_PAT-45.txt
  ;"            | }~CONTAINED PATCHES
  ;"            |   }~"ABSV*4.0*45 SEQ #44" = 0
  ;"            }~INSTALL
  ;"            | }~"INSTALLABLE" = 1
  ;"            | }~"INSTALLED" = 0                    ATA
  ;"            | }~"PENDING DEPENDENCIES" = 0
  ;"            | }~"STATUS" = READY
  ;"            }~KIDS
  ;"            | }~3,3,1,
  ;"            | | }~CONTAINED PATCHES
  ;"            | | | }~"ABSV*4.0*45 SEQ #44" = 0
  ;"            | | }~"NAME" = ABSV-4_SEQ-44_PAT-45.kids
  ;"            | | }~"URL" = foia-vista.worldvista.org/Patches_By_Application/ABSV-Voluntary Service/ABSV-4_SEQ-44_PAT-45.kids
  ;"            | }~CONTAINED PATCHES
  ;"            | | }~"ABSV*4.0*45 SEQ #44" = 0
  ;"            | }~"PENDING DEPENDENCIES" = 0
  ;"            }~"NAME" = ABSV*4.0*45
  ;"            }~"PACKAGE" = ABSV
  ;"            }~"PATCH#" = 45
  ;"            }~"SEQ#" = 44
  ;"            }~"VER" = 4
  ;"            }~"VERSTR" = 4.0
  ;"       INFO --PASS BY REFERENCE, an OUT PARAMETER.
  ;"       MSG -- PASS BY REFERANCE, an OUT PARAMETER
  ;"              Errors are stored in MSG("ERROR",x)=Message
  ;"                                   MSG("ERROR")=COUNT of last error
  ;"              Message are store in MSG(x)=Message
  ;"                                   MSG=COUNT of last message+1
  ;"       OPTION -- optional.  Pass by reference.
  ;"              OPTION("VERBOSE")=1, means messaages also written directly to output
  ;"Output: INFO will be filled as per ENSRLOCL() above
  ;"              INFO("PATH")=PATH in HFS
  ;"              INFO("TEXT ONLY")=1 IF there is a text file, but no .KIDS file
  ;"              INFO("KID URL")=URL on server for KID file
  ;"                  INFO("KID URL",1)=URL on server for KID file  <- same as INFO("KID URL") 
  ;"                  INFO("KID URL",2)=URL on server for KID file  <- present if multiple KIDS files found.   
  ;"              INFO("KID FILE")=HFS FILENAME of .KID patch
  ;"                  INFO("KID FILE",1)=HFS FILENAME <- same as INFO("KID FILE")
  ;"                  INFO("KID FILE",2)=HFS FILENAME <- present if multiple KIDS files found.
  ;"              INFO("TEXT URL")=URL on server for TXT file
  ;"                  INFO("TEXT URL",1)=URL on server <- same as INFO("TEXT URL")
  ;"                  INFO("TEXT URL",2)=URL on server <- present if multiple TEXT files found.
  ;"              INFO("TEXT FILE")=HFS FILENAME of .TXT accompanying patch
  ;"                  INFO("TEXT FILE",1)=HFS FILENAME   <- same as INFO("TEXT FILE")
  ;"                  INFO("TEXT FILE",2)=HFS FILENAME   <- present if multiple TEXT files found.
  ;"              INFO("ALL",<filename>)=""  <-- entry for each file downloaded.  
  ;"RESULTS: "1^OK", or "-1^Message" if  problem.
  ;  
  NEW AIENS,CT
  NEW RESULT SET RESULT="1^OK"
  NEW VERBOSE SET VERBOSE=+$GET(OPTION("VERBOSE"))
  NEW PCK SET PCK=$GET(DATA("PACKAGE"))
  NEW PATH SET PATH=$$HFSPATCHBASE^TMGKERN4()  ;"e.g. /opt/worldvista/EHR/kids
  SET PATH=$$MKTRALDV^TMGIOUTL(PATH)
  SET PATH=PATH_$$PATCHDIRNAME^TMGPAT4(PCK)
  SET PATH=$$MKTRALDV^TMGIOUTL(PATH)
  SET INFO("PATH")=PATH
  NEW MAP SET MAP("INFOTXT")="TEXT",MAP("KIDS")="KID"  
  NEW TARGET FOR TARGET="INFOTXT","KIDS" DO
  . NEW TARGET2 SET TARGET2=MAP(TARGET) 
  . SET AIENS="",CT=1
  . FOR  SET AIENS=$ORDER(DATA(TARGET,AIENS)) QUIT:AIENS=""  DO
  . . NEW URL SET URL=$GET(DATA(TARGET,AIENS,"URL")) QUIT:URL=""
  . . NEW TARGETURL SET TARGETURL=TARGET2_" URL"
  . . NEW TARGETFILE SET TARGETFILE=TARGET2_" FILE"
  . . SET INFO(TARGETURL,CT)=URL
  . . IF CT=1 SET INFO(TARGETURL)=URL
  . . NEW TEMPFN DO SPLITFPN^TMGIOUTL(URL,,.TEMPFN) 
  . . IF TEMPFN'="" DO
  . . . NEW FPATHNAME SET FPATHNAME=PATH_TEMPFN
  . . . IF $DATA(INFO("ALL",FPATHNAME))>0 QUIT  ;"DON'T ADD FILES TWICE
  . . . NEW EXISTS SET EXISTS=$$FILEXIST^TMGIOUTL(FPATHNAME)
  . . . IF 'EXISTS DO
  . . . . IF VERBOSE DO
  . . . . . WRITE !
  . . . . . WRITE " -------------------------------------------------------------",!
  . . . . . WRITE "== DOWNLOAD ===================================================",!
  . . . . . WRITE "Downloading "_TARGET_" file from patch repository server..."
  . . . . IF $$DOWNLOADFILE^TMGKERNL(URL,PATH,0)
  . . . . IF VERBOSE DO
  . . . . . WRITE !,"===============================================================",!
  . . . . . WRITE " -------------------------------------------------------------",!
  . . . . SET EXISTS=$$FILEXIST^TMGIOUTL(FPATHNAME)
  . . . IF EXISTS DO
  . . . . IF $$DOS2UNIX^TMGKERNL(FPATHNAME)
  . . . . SET INFO(TARGETFILE,CT)=FPATHNAME
  . . . . IF CT=1 SET INFO(TARGETFILE)=FPATHNAME
  . . . . SET INFO("ALL",FPATHNAME)=""
  . . . ELSE  DO
  . . . . DO ADDMSG("Problem downloading ["_URL_"] to ["_FPATHNAME_"]",1,.MSG,VERBOSE)
  . . . . SET RESULT="-1^See MSG"
  . . . IF VERBOSE WRITE !
  . . SET CT=CT+1
  ;
  IF $DATA(INFO("KID URL"))=0 DO
  . IF $DATA(INFO("TEXT URL"))>0 SET INFO("TEXT ONLY")=1 QUIT
  . NEW TEMPS SET TEMPS="No URL found for KIDS patch or accompanying Info text file in FM File #22709.121, IENS="_IENS
  . DO ADDMSG(TEMPS,1,.MSG,.OPTION)
  . SET RESULT="-1^See MSG"
ELDONE
  QUIT RESULT
  ;
ADDMSG(STR,ISERR,MSG,VERBOSE)  ;
  ;"Purpose: to add a message to MSG ARRAY
  ;"Input: STR  -- message.  May be a string, or an array (or both) in format of:
  ;"              STR=A line
  ;"              STR(1)=line 1
  ;"              STR(2)=line 2
  ;"       ISERR -- 1 if STR is an error message
  ;"       MSG -- PASS BY REFERENCE, an OUT PARAMETER.
  ;"              Errors are stored in MSG("ERROR",x)=Message
  ;"                                   MSG("ERROR")=COUNT of last error
  ;"              Message are store in MSG(x)=Message
  ;"                                   MSG=COUNT of last message+1
  ;"       VERBOSE -- optional.  1, means messaages also written directly to output
  ;"RESULTS: NONE
  SET VERBOSE=+$GET(VERBOSE)
  SET ISERR=+$GET(ISERR)
  NEW SUBIDX SET SUBIDX=""
  NEW ALINE SET ALINE=$GET(STR)
  FOR  DO  SET SUBIDX=$ORDER(STR(SUBIDX)) QUIT:(SUBIDX="")  SET ALINE=$GET(s(SUBIDX))
  . IF ISERR DO
  . . NEW MSGI SET MSGI=$GET(MSG("ERROR"),0)+1
  . . SET MSG("ERROR",MSGI)=ALINE
  . . SET MSG("ERROR")=MSGI
  . ELSE  DO
  . . SET MSG=+$GET(MSG,1)
  . . SET MSG(MSG)=ALINE,MSG=MSG+1
  IF VERBOSE WRITE STR,!
  QUIT
  ;
SHOWMSG(MSG,NOPAUSE)  ;
  ;"Purpose: to display the message array
  ;"Input: MSG - PASS BY REFERENCE.  The message array to display.
  ;"       NOPAUSE -- OPTIONAL.  If 1, then user not prompted to hit enter to cont.
  ;"RESULTs: 0 IF OK, 1 IF ERROR found in message array.
  NEW ERR SET ERR=0
  IF $DATA(MSG) DO
  . NEW IDX SET IDX=""
  . FOR  SET IDX=$ORDER(MSG(IDX)) QUIT:(+IDX'>0)  WRITE "  ",$GET(MSG(IDX)),!
  . IF $DATA(MSG("ERROR")) DO
  . . WRITE !!,"NOTE: ERRORS ENCOUNTERED:",!
  . . SET IDX=""
  . . FOR  SET IDX=$ORDER(MSG("ERROR",IDX)) QUIT:(+IDX'>0)  WRITE "  ",$GET(MSG("ERROR",IDX)),!
  . . SET ERR=1
  . IF $GET(NOPAUSE)'=1 DO PRESS2GO^TMGUSRI2
  ;
  QUIT ERR  
  ;
MAKEPATCHENTRY(PCK,VER,SEQ,PNUM,MSG)  ;"similar to MACKEPATCHENTRY^TMGPAT2, but split inputs. 
  ;"Purpose: For times when a patch was informational only, and there was
  ;"         no KIDS file to actually install, then this can make pseudo-entries
  ;"         to show that something was processed.
  ;"Input: PCK -- Package initials.  e.g. DI
  ;"       VER -- version number
  ;"       SEQ -- sequence number, e.g. 456
  ;"       PNUM -- patch number, e.g. 123
  ;"       MSG -- PASS BY REFERANCE, an OUT PARAMETER
  ;"              Errors are stored in MSG("ERROR",x)=Message
  ;"                                   MSG("ERROR")=COUNT of last error
  ;"              Message are store in MSG(x)=Message
  ;"                                   MSG=COUNT of last message+1
  ;"RESULTs: 1 if OK, 0 if error
  NEW %,X,RESULT
  NEW TMGMSG,TMGFDA,TMGIEN
  SET RESULT=1     
  NEW VERSTR SET VERSTR=VER IF $PIECE(VERSTR,".",2)="" SET $PIECE(VERSTR,".",2)="0"
  NEW PATCHNAME SET PATCHNAME=$$MAKEPATCHNAME^TMGPAT2(PCK,VERSTR,PNUM,SEQ)
  NEW PATCHNAMENOSEQ SET PATCHNAMENOSEQ=$$MAKEPATCHNAME^TMGPAT2(PCK,VERSTR,PNUM)
  NEW TEMPIEN SET TEMPIEN=+$ORDER(^XPD(9.7,"B",PATCHNAMENOSEQ,""))
  IF TEMPIEN>0 GOTO MPE2 ;"INSTALL entry already made
  ;                        
  DO NOW^%DTC
  SET TMGFDA(9.7,"+1,",.01)=PATCHNAMENOSEQ
  SET TMGFDA(9.7,"+1,",.02)=3                     ;"2 = status
  SET TMGFDA(9.7,"+1,",6)="Text_Only "_PATCHNAME  ;"6 = file comment
  SET TMGFDA(9.7,"+1,",9)=DUZ                     ;"9 = Installed by
  SET TMGFDA(9.7,"+1,",11)=%                      ;"11 = Install start time
  SET TMGFDA(9.7,"+1,",17)=%                      ;"17 = Install completion time
  DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO
  . DO ADDMSG($$GETERRST^TMGDEBU2(.TMGMSG),1,.MSG)
  . SET RESULT=0
MPE2 ;
  SET VER=$PIECE(PATCHNAME,"*",2)
  IF (PCK="")!(VER="") SET RESULT=0 GOTO MPEDONE
  NEW IEN9D4,IEN9D49
  SET IEN9D4=+$ORDER(^DIC(9.4,"C",PCK,""))
  IF IEN9D4'>0 SET RESULT=0 GOTO MPEDONE
  SET IEN9D49=+$ORDER(^DIC(9.4,IEN9D4,22,"B",VER,""))
  IF IEN9D49>0 GOTO MPE2B
  SET IEN9D49=+$ORDER(^DIC(9.4,IEN9D4,22,"B",VERSTR,""))
  IF IEN9D49>0 GOTO MPE2B
  KILL TMGFDA,TMGIEN,TMGMSG
  NEW TEMPIENS SET TEMPIENS="+1,"_IEN9D4_","
  SET TMGFDA(9.49,TEMPIENS,.01)=VERSTR
  SET TMGFDA(9.49,TEMPIENS,1)=$$NOW^XLFDT
  SET TMGFDA(9.49,TEMPIENS,3)=DUZ
  DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO MPEDONE
  . DO ADDMSG($$GETERRST^TMGDEBU2(.TMGMSG),1,.MSG)
  . SET RESULT=0
  SET IEN9D49=$GET(TMGIEN(1))
  IF IEN9D49>0 GOTO MPE2B
  DO ADDMSG("Unable to locate entry for version: ["_VERSTR_"]",1,.MSG)
  SET RESULT=0
  GOTO MPEDONE
  ;
MPE2B  
  NEW PATCHSEQSTR SET PATCHSEQSTR=$PIECE(PATCHNAME,"*",3)  ;"e.g. '10 SEQ# 123'  
  SET TEMPIEN=$ORDER(^DIC(9.4,IEN9D4,22,IEN9D49,"PAH","B",PATCHSEQSTR,""))
  IF TEMPIEN>0 DO  GOTO MPE3
  . SET TMGIEN(1)=TEMPIEN
  NEW IENS SET IENS="+1,"_IEN9D49_","_IEN9D4_","
  KILL TMGFDA,TMGMSG,TMGIEN
  SET TMGFDA(9.4901,IENS,.01)=PATCHSEQSTR  ;".01=Patch Hx, e.g. 10 SEQ #10
  SET TMGFDA(9.4901,IENS,.02)="NOW"        ;".02=date applied
  SET TMGFDA(9.4901,IENS,.03)="`"_DUZ      ;".03=Applied by
  DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO
  . DO ADDMSG($$GETERRST^TMGDEBU2(.TMGMSG),1,.MSG)
  . SET RESULT=0
MPE3 ;
  IF RESULT=0 GOTO MPEDONE
  NEW TMGWP
  KILL TMGFDA,TMGMSG
  SET TMGWP(1)="Patch was informational only.  No installed code etc."
  SET TMGWP(2)="This entry was created as a marker that information was processed."
  SET IENS=TMGIEN(1)_","_IEN9D49_","_IEN9D4_","
  DO WP^DIE(9.4901,IENS,1,"","TMGWP","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO
  . DO ADDMSG($$GETERRST^TMGDEBU2(.TMGMSG),1,.MSG)
  . SET RESULT=0
MPEDONE ;
  QUIT RESULT
  ;
  