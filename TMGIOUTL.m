TMGIOUTL ;TMG/kst/IO Utilities ;7/22/15, 6/25/22
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
 ;"$$FNEXTRCT^TMGIOUTL(FULLPATHNAME,NODEDIV)
 ;"PATH2ARR(FULLPATHNAME,ARR,NODEDIV) -- Split a full path + filename into an array, with 1 entry for each node or file 
 ;"$$PATHEXTR^TMGIOUTL(FULLPATHNAME,NODEDIV)
 ;"$$UPPATH^TMGIOUTL(PATH,NODEDIV) -- return a path that is one step up from current path
 ;"$$EXTNEXTR(FULLPATHNAME,NODEDIV)-- return the extension of the file name from full path+name string
 ;"SPLITFPN^TMGIOUTL(FULLPATHNAME,OUTNAME,OUTPATH,NODEDIV)
 ;"$$GETFNAME^TMGIOUTL(MSG,DEFPATH,DEFFNAME,NODEDIV,OUTPATH,OUTNAME,PROMPT,FILTER)
 ;"$$GETDIRNM(MSG,DEFPATH,NODEDIV,OUTPATH,PROMPT) -- query user for a directory name
 ;"$$FILEXIST^TMGIOUTL(FULLPATHNAME)
 ;"PCK1FILE(PARTNAMEPATH) --For name like 'MyFil*',  display all matches and allow user to pick one
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
FNEXTRACT(FULLPATHNAME,NODEDIV) ;
  QUIT $$FNEXTRCT(.FULLPATHNAME,.NODEDIV)
  ;
FNEXTRCT(FULLPATHNAME,NODEDIV) ;
  ;"SCOPE: Public
  ;"Purpose: to extract a file name from a full path+name string
  ;"Input: FULLPATHNAME: String to process.
  ;"                e.g.: "/tmp/myfilename.txt"
  ;"        NODEDIV: [OPTIONAL] -- the character that separates folders (e.g. "/")
  ;"                IF not supplied, then default value is "/"
  ;"Result: the filename, or "" IF not found
  ;"        e.g.: "myfilename.txt"
  ;
  NEW OUTPATH,OUTNAME
  DO SPLITFPN(.FULLPATHNAME,.OUTPATH,.OUTNAME,.NODEDIV)
  QUIT $GET(OUTNAME)
  ;
PATH2ARR(FULLPATHNAME,ARR,NODEDIV)  ;"Split a full path + filename into an array, with 1 entry for each node or file
  ;"SCOPE: Public
  ;"Purpose: Split a full path + filename into an array, with 1 entry for each node or file
  ;"Input: FULLPATHNAME: String to process.
  ;"                e.g.: "/usr/local/myfilename.txt"
  ;"       ARR -- PASS BY REFERENCE.  See Output below.
  ;"       NODEDIV: [OPTIONAL] -- the character that separates folders (e.g. "/")
  ;"                IF not supplied, then default value is "/"
  ;"Result: none
  ;"Output: ARR is filled.  Format, per example above
  ;"          ARR(1)="/"
  ;"          ARR(2)="usr/"
  ;"          ARR(3)="local/"
  ;"          ARR(4)="myfilename.txt"
  ;
  SET NODEDIV=$GET(NODEDIV,"/")
  NEW L SET L=$LENGTH(FULLPATHNAME,NODEDIV)
  NEW IDX FOR IDX=1:1:L DO
  . NEW NODE SET NODE=$PIECE(FULLPATHNAME,NODEDIV,IDX)
  . IF IDX=L,NODE="" QUIT
  . IF IDX<L SET NODE=NODE_NODEDIV
  . SET ARR(IDX)=NODE
  QUIT
  ;
PATHEXTR(FULLPATHNAME,NODEDIV) ;
  ;"SCOPE: Public
  ;"Purpose: to extract a file name from a full path+name string
  ;"Input: FULLPATHNAME: String to process.
  ;"                e.g.: "/usr/local/myfilename.txt"
  ;"        NODEDIV: [OPTIONAL] -- the character that separates folders (e.g. "/")
  ;"                IF not supplied, then default value is "/"
  ;"Result: the path, or "" IF not found
  ;"        e.g.: "/usr/local/"
  ;
  NEW OUTPATH,OUTNAME
  DO SPLITFPN(.FULLPATHNAME,.OUTPATH,.OUTNAME,.NODEDIV)
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
EXTNEXTR(FULLPATHNAME,NODEDIV) ;
  ;"Purpose: to return the extension of the file name from full path+name string
  ;"         This will be everything after the last '.'
  ;"Input: FULLPATHNAME: String to process.
  ;"                e.g.: "/usr/local/myfilename.txt"
  ;"        NODEDIV: [OPTIONAL] -- the character that separates folders (e.g. "/")
  ;"                IF not supplied, then default value is "/"
  ;"Result: the extension or "" IF not found
  ;"        e.g.: "txt"  (doesn't include '.'
  ;
  NEW RESULT
  SET RESULT=$PIECE(FULLPATHNAME,".",$LENGTH(FULLPATHNAME,"."))
  QUIT RESULT
  ;
SPLITFPN(FULLPATHNAME,OUTPATH,OUTNAME,NODEDIV) ;"split PathFileName into Path, Filename
  ;"SCOPE: Public
  ;"Purpose: Take FULLPATHNAME, and split into name and path.
  ;"Input: FULLPATHNAME: String to process.
  ;"                e.g.: "/tmp/myfilename.txt"
  ;"                NOTICE: IF PASSED BY REFERENCE, WILL BE CHANGED TO FILENAME!
  ;"        OUTPATH: MUST BE PASSED BY REFERENCE.  This is an OUT parameter
  ;"        OUTNAME: MUST BE PASSED BY REFERENCE.  This is an OUT parameter
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
  SET FULLPATHNAME=$GET(FULLPATHNAME)
SPN1 ;
  IF (FULLPATHNAME[NODEDIV)=0 SET OUTNAME=FULLPATHNAME GOTO SPNDONE
  SET PATHNODE=$PIECE(FULLPATHNAME,NODEDIV,1)
  SET OUTPATH=OUTPATH_PATHNODE_NODEDIV
  SET $PIECE(FULLPATHNAME,NODEDIV,1)=""
  SET FULLPATHNAME=$EXTRACT(FULLPATHNAME,2,255)
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
FILEXIST(FULLPATHNAME) ;
  ;"Purpose: To determine IF file exists.
  ;"Input: FULLPATHNAME -- the full name and path of file to test, e.g. "/tmp/myfiles/a/test.txt"
  ;"Results: 1 IF file exists (and is unique), 0 IF not
  ;"Note: If FULLPATHNAME indicates a directory, then 0 is returned.
  ;"      Note IF FULLPATHNAME contains a * pattern, that would cause multiple
  ;"              files to be returned, then filename is not unique, and function
  ;"              will RETURN THAT IT IS NOT A (unique) FILE
  ;
  NEW JUSTNAME,JUSTPATH
  NEW TMGMASK
  NEW TMGFILES
  NEW RESULT SET RESULT=0
  ;
  DO SPLITFPN(FULLPATHNAME,.JUSTPATH,.JUSTNAME)
  SET TMGMASK(JUSTNAME)=""
  IF $$LIST^%ZISH(JUSTPATH,"TMGMASK","TMGFILES")=1 DO
  . IF $$LISTCT^TMGMISC2("TMGFILES")=1 DO
  . . SET RESULT='$$ISDIR^TMGKERNL(FULLPATHNAME)
  QUIT RESULT
  ;
PCK1FILE(PARTNAMEPATH) ;
  ;"Purpose: To take a name like "MyFil*", and display all matches and allow user to pick one
  ;"Input: PARTNAMEPATH -- the partial name and path of file to test, e.g. "/tmp/myfiles/a/tes*"
  ;"Results: The FULLPATHNAME of the chosen file (or "" IF none, or canceled)
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
PCKDELFILE(DEFPATH,MSG)  ;
  ;"Purpose: Show listing of files from PATH and allow user to pick for deletion
  ;"Input:  DEFPATH: [OPTIONAL] The default path to offer user.
  ;"        MSG. [OPTIONAL] A message to show user prior to name prompt.                       
  ;"                May contain "\n" character for line wrapping.
  NEW FNAME,PATH,FPNAME 
  SET FPNAME=$$GETFNAME(.MSG,.DEFPATH,,,.PATH,.FNAME)
  IF FPNAME="" QUIT
  WRITE !,"PATH = ",PATH,!
  WRITE "FILE = ",FNAME,!
  NEW % SET %=2 
  WRITE "PERMANENTLY DELETE file" DO YN^DICN WRITE !
  IF %'=1 QUIT
  NEW RESULT SET RESULT=$$DELFILE(FPNAME)
  IF RESULT=1 DO
  . WRITE "File deleted.",!
  ELSE  DO
  . WRITE "ERROR deleting file.",!
  DO PRESS2GO^TMGUSRI2
  QUIT
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
