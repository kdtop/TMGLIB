TMGIOUTL ;TMG/kst/IO Utilities ;03/25/06, ... 7/22/15
         ;;1.0;TMG-LIB;**1**;07/12/05
 ;
 ;"TMG IO UTILITIES
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"NOTE: THIS CODE IS NOW SACC COMPLIANT.  Keep it that way!  :-)  
 ;"      Code size/length will only allow for small additions in this file.
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"$$FNEXTRCT^TMGIOUTL(FULLNAMEPATH,NODEDIV)
 ;"$$PATHEXTR^TMGIOUTL(FULLNAMEPATH,NODEDIV)
 ;"$$UPPATH^TMGIOUTL(PATH,NODEDIV) -- return a path that is one step up from current path
 ;"$$EXTNEXTR(FULLNAMEPATH,NODEDIV)-- return the extension of the file name from full path+name string
 ;"SPLITFPN^TMGIOUTL(FULLNAMEPATH,OUTNAME,OUTPATH,NODEDIV)
 ;"$$GETFNAME^TMGIOUTL(MSG,DEFPATH,DEFFNAME,NODEDIV,OUTPATH,OUTNAME,PROMPT,FILTER)
 ;"$$GETDIRNM(MSG,DEFPATH,NODEDIV,OUTPATH,PROMPT) -- query user for a directory name
 ;"$$FILEXIST^TMGIOUTL(FULLNAMEPATH)
 ;"PCK1FILE(PARTNAMEPATH) --For  name like 'MyFil*',  display all matches and allow user to pick one
 ;"$$DELFILE^TMGIOUTL(PATHFILENAME)
 ;"$$MKTRALDV^TMGIOUTL(PATH,NODEDIV)
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"TMGUSRIF for showing dialogs.
 ;"TMGDEBUG
 ;"TMGSTUTL
 ;"TMGMISC
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;
FNEXTRCT(FULLNAMEPATH,NODEDIV) ;
  ;"SCOPE: Public
  ;"Purpose: to extract a file name from a full path+name string
  ;"Input: FULLNAMEPATH: String to process.
  ;"                e.g.: "/tmp/myfilename.txt"
  ;"        NODEDIV: [OPTIONAL] -- the character that separates folders (e.g. "/")
  ;"                IF not supplied, then default value is "/"
  ;"Result: the filename, or "" IF not found
  ;"        e.g.: "myfilename.txt"
  ;
  NEW OUTPATH,OUTNAME
  DO SPLITFPN(.FULLNAMEPATH,.OUTPATH,.OUTNAME,.NODEDIV)
  QUIT $GET(OUTNAME)
  ;
PATHEXTR(FULLNAMEPATH,NODEDIV) ;
  ;"SCOPE: Public
  ;"Purpose: to extract a file name from a full path+name string
  ;"Input: FULLNAMEPATH: String to process.
  ;"                e.g.: "/usr/local/myfilename.txt"
  ;"        NODEDIV: [OPTIONAL] -- the character that separates folders (e.g. "/")
  ;"                IF not supplied, then default value is "/"
  ;"Result: the path, or "" IF not found
  ;"        e.g.: "/usr/local/"
  ;
  NEW OUTPATH,OUTNAME
  DO SPLITFPN(.FULLNAMEPATH,.OUTPATH,.OUTNAME,.NODEDIV)
  QUIT $GET(OUTPATH)
  ;
UPPATH(PATH,NODEDIV) ;
  ;"SCOPE: Public
  ;"Purpose: To return a path that is one step up from current path
  ;"Input: PATH  -- NOTE: **MUST NOT** have a file name
  ;"             e.g. RIGHT --> '/var/local/'
  ;"                  WRONG --> '/var/MyFile.txt'   <-- 'MyFile.txt' would be treated as path node
  ;"                            '/var/MyFile.txt' = '/var/MyFile.txt/' ==> UP ==> '/var/'
  ;"       NODEDIV: [OPTIONAL] -- the character that separates folders (e.g. "/")
  ;"                IF not supplied, then default value is "/"
  ;"Results: Returns resulting path.  E.g. '/var/local/' --> '/var/'
  ;"         Note: '/' --> '/'  (i.e. can't go higher than root)
  SET NODEDIV=$GET(NODEDIV,"/")
  NEW TEMPPATH SET TEMPPATH=$$MKTRALDV($GET(PATH),NODEDIV)
  NEW RESULT SET RESULT=NODEDIV
  IF TEMPPATH'=NODEDIV DO
  . SET RESULT=$PIECE(TEMPPATH,"/",1,$LENGTH(TEMPPATH,"/")-2)_NODEDIV
  ELSE  SET RESULT=NODEDIV
  QUIT RESULT
  ;
EXTNEXTR(FULLNAMEPATH,NODEDIV) ;
  ;"Purpose: to return the extension of the file name from full path+name string
  ;"         This will be everything after the last '.'
  ;"Input: FULLNAMEPATH: String to process.
  ;"                e.g.: "/usr/local/myfilename.txt"
  ;"        NODEDIV: [OPTIONAL] -- the character that separates folders (e.g. "/")
  ;"                IF not supplied, then default value is "/"
  ;"Result: the extension or "" IF not found
  ;"        e.g.: "txt"  (doesn't include '.'
  ;
  NEW RESULT
  SET RESULT=$PIECE(FULLNAMEPATH,".",$LENGTH(FULLNAMEPATH,"."))
  QUIT RESULT
  ;
SPLITFPN(FULLNAMEPATH,OUTPATH,OUTNAME,NODEDIV) ;"split PathFileName into Path, Filename
  ;"SCOPE: Public
  ;"Purpose: Take FULLNAMEPATH, and split into name and path.
  ;"Input: FULLNAMEPATH: String to process.
  ;"                e.g.: "/tmp/myfilename.txt"
  ;"                NOTICE: IF PASSED BY REFERENCE, WILL BE CHANGED TO FILENAME!
  ;"        OUTNAME: MUST BE PASSED BY REFERENCE.  This is an OUT parameter
  ;"        OUTPATH: MUST BE PASSED BY REFERENCE.  This is an OUT parameter
  ;"        NODEDIV: [OPTIONAL] -- the character that separates folders (e.g. "/")
  ;"                IF not supplied, then default value is "/"
  ;"Output: The RESULTing file name is put into OUTNAME,
  ;"                e.g.: "myfilename.txt"
  ;"        and the path is put into OUTPATH.
  ;"                e.g.: "/tmp/"
  ;"Result: None.
  ;
  SET OUTPATH=""
  SET OUTNAME=""
  NEW PATHNODE
  SET NODEDIV=$GET(NODEDIV,"/")
  SET FULLNAMEPATH=$GET(FULLNAMEPATH)
SPN1 ;
  IF (FULLNAMEPATH[NODEDIV)=0 SET OUTNAME=FULLNAMEPATH GOTO SPNDONE
  SET PATHNODE=$PIECE(FULLNAMEPATH,NODEDIV,1)
  SET OUTPATH=OUTPATH_PATHNODE_NODEDIV
  SET $PIECE(FULLNAMEPATH,NODEDIV,1)=""
  SET FULLNAMEPATH=$EXTRACT(FULLNAMEPATH,2,255)
  GOTO SPN1
SPNDONE ;
  QUIT
  ;
GETFNAME(MSG,DEFPATH,DEFFNAME,NODEDIV,OUTPATH,OUTNAME,PROMPT,FILTER) ;
  ;"SCOPE: PUBLIC
  ;"Purpose: To query the user, to get a filename back
  ;"          Supplies optional directory listing.
  ;"Input: MSG. [OPTIONAL] A message to show user prior to name prompt.                       
  ;"                May contain "\n" character for line wrapping.
  ;"        DEFPATH: [OPTIONAL] The default path to offer user.
  ;"        DEFFNAME:[OPTIONAL] The default filename to offer user.
  ;"        NODEDIV: [OPTIONAL] -- the character that separates folders (e.g. "/")
  ;"                IF not supplied, then default value is "/"
  ;"        OUTPATH: [OPTIONAL] Pass by reference, filled with selected path
  ;"                //no --> Note: Will return like this: '/home/test'  not '/home/test/'
  ;"                (6-5-05: I think this because $$FTG^%ZISH wants the path like this)
  ;"        OUTNAME: [OPTIONAL] Pass by reference, filled with selected name
  ;"        PROMPT: [OPTIONAL] PROMPT for user to enter filename/directory name
  ;"        FILTER: [OPTIONAL] PASS BY REFERENCE.  An array of filters.
  ;"                 e.g. FILTER("*.kid")=""
  ;"                      FILTER("*.KIDS")=""
  ;"                      FILTER("CASESPEC")=1" <-- means case specific.  Default is NON-specific
  ;"Result: returns user specified filename (with path), or "" if aborted
  NEW RESULT SET RESULT=""
  NEW OPTION
  SET OPTION("MSG")=$GET(MSG)
  SET OPTION("PATH")=$GET(DEFPATH)
  SET OPTION("NAME")=$GET(DEFFNAME)
  SET OPTION("NODEDIV")=$GET(NODEDIV,"/")
  SET OPTION("PROMPT")=$GET(PROMPT)
  IF $DATA(FILTER) MERGE OPTION("MATCH")=FILTER
  SET RESULT=$$FBROWSE^TMGIOUT2(.OPTION,.OUTPATH,.OUTNAME)
  QUIT RESULT
  ;
GETDIRNM(MSG,DEFPATH,NODEDIV,PROMPT) ;        
  ;"SCOPE: PUBLIC
  ;"Purpose: To query the user, to get a directory name back
  ;"Input: MSG. [OPTIONAL] A message to show user prior to name prompt.
  ;"                May contain "\n" character for line wrapping.
  ;"        DEFPATH: [OPTIONAL] The default path to offer user.
  ;"        NODEDIV: [OPTIONAL] -- the character that separates folders (e.g. "/")
  ;"                IF not supplied, then default value is "/"
  ;"        PROMPT: [OPTIONAL] PROMPT for user to enter filename/directory name
  ;"Result: returns user specified filename (with path), or "" IF aborted
  ;"//KT NOTE -- THIS FUNCTION DOESN'T SEEM TO ALLOW YOU TO BROWSE TO A DIR.  FIX...
  NEW OPTION
  NEW RESULT SET RESULT=""
  SET OPTION("MSG")=$GET(MSG)
  SET OPTION("PATH")=$GET(DEFPATH)
  SET OPTION("NODEDIV")=$GET(NODEDIV)
  SET OPTION("PROMPT")=$GET(PROMPT)
  SET OPTION("SELECT DIR")=1
  SET RESULT=$$FBROWSE^TMGIOUT2(.OPTION)
  QUIT RESULT
  ;
FILEXIST(FULLNAMEPATH) ;
  ;"Purpose: To determine IF file exists.
  ;"Input: FULLNAMEPATH -- the full name and path of file to test, e.g. "/tmp/myfiles/a/test.txt"
  ;"Results: 1 IF file exists (and is unique), 0 IF not
  ;"Note: If FULLNAMEPATH indicates a directory, then 0 is returned.
  ;"      Note IF FULLNAMEPATH contains a * pattern, that would cause multiple
  ;"              files to be returned, then filename is not unique, and function
  ;"              will RETURN THAT IT IS NOT A (unique) FILE
  ;
  NEW JUSTNAME,JUSTPATH
  NEW TMGMASK
  NEW TMGFILES
  NEW RESULT SET RESULT=0
  ;
  DO SPLITFPN(FULLNAMEPATH,.JUSTPATH,.JUSTNAME)
  SET TMGMASK(JUSTNAME)=""
  IF $$LIST^%ZISH(JUSTPATH,"TMGMASK","TMGFILES")=1 DO
  . IF $$LISTCT^TMGMISC2("TMGFILES")=1 DO
  . . SET RESULT='$$ISDIR^TMGKERNL(FULLNAMEPATH)
  QUIT RESULT
  ;
PCK1FILE(PARTNAMEPATH) ;
  ;"Purpose: To take a name like "MyFil*", and display all matches and allow user to pick one
  ;"Input: PARTNAMEPATH -- the partial name and path of file to test, e.g. "/tmp/myfiles/a/tes*"
  ;"Results: The FULLNAMEPATH of the chosen file (or "" IF none, or canceled)
  ;"              12-14-05, IF user enters "^", this is returned.
  ;
  NEW JUSTNAME,JUSTPATH
  NEW TMGMASK
  NEW TMGFILES
  NEW RESULT SET RESULT=""
  ;
  DO SPLITFPN(PARTNAMEPATH,.JUSTPATH,.JUSTNAME)
  SET TMGMASK(JUSTNAME)=""
  IF $$LIST^%ZISH(JUSTPATH,"TMGMASK","TMGFILES")=1 DO
  . NEW COUNT SET COUNT=$$LISTCT^TMGMISC2("TMGFILES")
  . IF COUNT=1 SET RESULT=$ORDER(TMGFILES("")) QUIT
  . WRITE COUNT," matches to ",PARTNAMEPATH," found.  Pick one:",!
  . NEW PART,FNAME,NUM
  . SET FNAME=$ORDER(TMGFILES(""))
  . SET NUM=1
  . SET PART=1
  . IF FNAME'="" FOR  DO  QUIT:(FNAME="")!(RESULT="^")
  . . WRITE "   ",NUM,".  ",JUSTPATH,FNAME
  . . IF $$ISDIR^TMGKERNL(JUSTPATH_FNAME) WRITE "/"
  . . WRITE !
  . . SET TMGFILES(NUM)=FNAME
  . . SET FNAME=$ORDER(TMGFILES(FNAME))
  . . IF (PART=10)!(FNAME="") DO
  . . . NEW CHOICE
  . . . SET PART=1
  . . . WRITE "Choose file (1-",NUM,"), '^' to cancel, or [Enter] to continue: "
  . . . READ CHOICE:$GET(DTIME,3600),!!
  . . . IF CHOICE="^" SET FNAME="",RESULT="^" QUIT
  . . . IF (+CHOICE>0)&(+CHOICE<NUM+1) DO
  . . . . SET RESULT=$GET(TMGFILES(+CHOICE))
  . . . . SET FNAME=""
  . . SET PART=PART+1
  . . SET NUM=NUM+1
  ;
  IF RESULT'="" DO
  . IF RESULT'="^" SET RESULT=JUSTPATH_RESULT
  ELSE  DO
  . WRITE "(No file selected.)",!
  ;
  QUIT RESULT
  ;
DELFILE(PATHFILENAME) ;
  ;"Purpose: to delete one file on host file system
  ;"Results: returns 1 if success, 0 if failure
  ;"Note: 2/22/2006 -- If deletion is blocked by OS, then 1 may be returns but file is not deleted.
  NEW PATH,FILENAME,RESULT,TMGFILE
  DO SPLITFPN(.PATHFILENAME,.PATH,.FILENAME)
  SET TMGFILE(FILENAME)=""
  SET RESULT=$$DEL^%ZISH(PATH,"TMGFILE")
  QUIT RESULT
  ;
MKTRALDV(PATH,NODEDIV)  ;"Make Trailing Node Dividor (Was EnsureTrailDiv)
  ;"Purpose: to ensure that a path ends with a node divider.
  ;"         e.g.  /var/local  --> /var/local/
  ;"         and   /var/local/ --> /var/local/
  ;"Input: PATH  -- the path to convert
  ;"       NODEDIV: [OPTIONAL] -- the character that separates folders (e.g. "/")
  ;"                IF not supplied, then default value is "/"
  ;"Result: the converted/verified path.
  ;
  SET PATH=$GET(PATH)
  SET NODEDIV=$GET(NODEDIV,"/")
  NEW RESULT SET RESULT=PATH
  IF $EXTRACT(PATH,$LENGTH(PATH))'=NODEDIV DO
  . SET RESULT=PATH_NODEDIV
  QUIT RESULT
  ;
