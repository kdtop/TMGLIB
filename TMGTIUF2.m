TMGTIUF2 ;TMG/kst/TIU files;7/25/19
       ;;1.0;TMG-LIB;**1**;7/25/19
       ;
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
CHKFILES   ;"Check each patient for files in P:/FPG Charts
  NEW DFN SET DFN=0
  FOR  SET DFN=$O(^DPT(DFN)) QUIT:DFN'>0  DO
  . ;"Check existing entries for deletions
  . NEW IEN SET IEN=0
  . FOR  SET IEN=$O(^TMG(22742,"B",DFN,IEN)) QUIT:IEN'>0  DO
  . . NEW THISPATH
  . . SET THISPATH=$P($G(^TMG(22742,IEN,1)),"^",1)
  . . NEW HFSROOT SET HFSROOT=$$HFSROOT()
  . . SET THISPATH=HFSROOT_$P(THISPATH,"oldrecs/",2)
  . . ;"WRITE "CHECKING ",THISPATH,!
  . . IF ($$FILEXIST^TMGIOUTL(THISPATH)=0)!($$UP^XLFSTR(THISPATH)'[".PDF") DO
  . . . ;"WRITE " ****DELETING ",THISPATH,!
  . . . NEW TMGFDA,TMGMSG,TMGIEN
  . . . SET TMGFDA(22742,IEN_",",.01)="@"
  . . . DO FILE^DIE("E","TMGFDA","TMGMSG")
  . . ELSE  DO
  . . . ;"WRITE "!!!!EXISTS!!!!",!
  . ;"
  . ;"Enter new ones
  . NEW LIST
  . DO PREPRCLK(.LIST,DFN)
  . ;"WRITE "== ",DFN," ==",!
  . NEW DATE SET DATE=0
  . FOR  SET DATE=$O(LIST(DATE)) QUIT:DATE'>0  DO
  . . NEW FULLPATH SET FULLPATH=""
  . . FOR  SET FULLPATH=$O(LIST(DATE,FULLPATH)) QUIT:FULLPATH=""  DO
  . . . NEW FILENAME SET FILENAME=$P(FULLPATH,"^",2)
  . . . IF $$UP^XLFSTR(FILENAME)'[".PDF" QUIT
  . . . NEW PATH SET PATH=$P(FULLPATH,"^",1)
  . . . NEW COMPLETE SET COMPLETE=PATH_FILENAME 
  . . . IF $$ISDIR^TMGKERNL(COMPLETE)=1 QUIT  ;"NOT A FOLDER
  . . . ;"WRITE "      ->",PATH," ",FILENAME,!
  . . . IF $$FEXISTS(DFN,COMPLETE)=1 QUIT ;"W "  *EXISTS",! QUIT
  . . . NEW TMGFDA,TMGMSG,TMGIEN
  . . . SET TMGFDA(22742,"+1,",.01)="`"_DFN
  . . . SET TMGFDA(22742,"+1,",.02)=PATH
  . . . SET TMGFDA(22742,"+1,",.03)=FILENAME
  . . . SET TMGFDA(22742,"+1,",.04)=DATE
  . . . SET TMGFDA(22742,"+1,",1)=COMPLETE
  . . . DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
  . . . IF $DATA(TMGMSG("DIERR")) DO 
  . . . . ;"SET RESULT=-1
  . . . . ;"ZWR TMGMSG("DIERR",*)
  . . . ELSE  DO
  . . . . ;"WRITE "ADDED ",COMPLETE,!
  QUIT
  ;"
FEXISTS(DFN,FILEPATH)  ;"IS FILE AREADY ENTERED IN 22742
  NEW TMGRESULT SET TMGRESULT=0
  NEW IEN SET IEN=0
  FOR  SET IEN=$O(^TMG(22742,"B",DFN,IEN)) QUIT:IEN'>0  DO
  . NEW THISPATH 
  . SET THISPATH=$P($G(^TMG(22742,IEN,1)),"^",1)
  . IF THISPATH=FILEPATH SET TMGRESULT=1
  QUIT TMGRESULT
  ;"
HFSROOT()  ;"Get the hard coded root in the HFS to the folder holding 'data'
  QUIT "/opt/worldvista/EHR/www/oldrecs/"
  ;
PREPRCLK(OUTARR,DFN)  ;"Prep listing of record links
  ;"Input: OUTARR -- AN OUT PARAMETER.  PASS BY REFERENCE.  Format:
  ;"            OUTARR(FMDT,URLPATH^FILENAME)=""
  ;"       DFN -- PATIENT IEN
  ;"RESULTS: NONE
  NEW PATHS DO PATH4DFN^TMGRST03(.PATHS,DFN)   ;"Get one or more filepaths forpatient name, including aliases
  NEW APATH SET APATH=""
  FOR  SET APATH=$ORDER(PATHS(APATH)) QUIT:APATH=""  DO
  . NEW HFSPATH SET HFSPATH=$$HFSROOT()_APATH
  . NEW URLPATH SET URLPATH="/filesystem/oldrecs/"_APATH
  . NEW TMGLIST
  . IF $$ISDIR^TMGKERNL(HFSPATH)=0 DO  QUIT
  . . DO ADDLN^TMGRST03(.OUT,"FYI, path: "_HFSPATH_" is not valid<p>")
  . NEW TMGFILTER SET TMGFILTER("*")=""
  . NEW TMP SET TMP=$$LIST^%ZISH(HFSPATH,"TMGFILTER","TMGLIST")
  . IF TMP'=1 DO  QUIT
  . . DO ADDLN^TMGRST03(.OUT,"<P><B>Error reading path: "_HFSPATH)
  . NEW AFILE SET AFILE=""
  . FOR  SET AFILE=$ORDER(TMGLIST(AFILE)) QUIT:AFILE=""  DO
  . . IF $$UP^XLFSTR(AFILE)="THUMBS.DB" QUIT
  . . NEW MODDT SET MODDT=$$FLASTMOD^TMGRST03(HFSPATH_AFILE)  ;"Returns FMDATE, or 0 if problem.
  . . SET OUTARR(+MODDT,URLPATH_"^"_AFILE)=""
  QUIT
  ;