TMGSEQL2 ;TMG/kst/Interface with SequelSystems PMS (Error Hndlng) ;2/2/14, 1/7/15
         ;;1.0;TMG-LIB;**1**;01/09/06
 ;
 ;"TMG SEQUEL IMPORT ERROR-HANDLING FUNCTIONS
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
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"ALERTERR(ONELINE,PTINFO,ONEERRORARRAY,DUZ,ID) ;
 ;"HANDLE
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"EDT1LINE(LINEIN,LINEOUT)
 ;"MAKERALT(IEN,USER,PTINFO)
 ;"$$STOREERR(ONELINE,PTINFO,ERRORARR)
 ;"ERREFILE(ONELINE,PTINFO,ONEERRORARRAY,DUZ)
 ;
 ;"$$FIXREG(PTINFO,ONELINE,DELERROR)
 ;"$$FIXGENPB(PTINFO,ERRMSG,ONELINE,ERRIEN,DELERROR)
 ;"$$FIXSSN(PTINFO,ERRMSG,ONELINE,DELERROR)
 ;"$$FixDOBProblem(.PTINFO,ERRMSG,.ONELINE,.DELERROR)
 ;
 ;"$$GETSEX(NAME)
 ;"$$SETSEX(NAME,SEX)
 ;"$$NAMEERR(ONEERRORARRAY)
 ;
 ;"$$SEXMISSING(ERRORARR)
 ;"$$FIXSEXM(PTINFO)

 ;"=======================================================================
 ;"DEPENDENCIES
 ;"TMGSEQL1
 ;"TMGSTUTL
 ;"TMGDEBUG
 ;"=======================================================================
 ;"=======================================================================
 ;
EDT1LINE(LINEIN,LINEOUT)
  ;"Purpose: To allow modification of a line to allow filing.
  ;"Input: LINEIN -- The CSV line to modify.
  ;"       LINEOUT -- PASS BY REFERENCE, the variable to receive changes
  ;"Result: 1 IF changes made, 0 IF no changes made, -1 IF abort
  NEW TEMPARR,ONELINE         
  NEW DONE SET DONE=0
  NEW ABORT SET ABORT=0      
  SET LINEOUT=$GET(LINEIN)
  NEW INPUTSAVE SET INPUTSAVE=LINEIN
  NEW RESULT SET RESULT=0
  NEW TEMP
  ;
  IF $GET(LINEIN)="" DO  GOTO EDLDN
  . WRITE !,"?? No data supplied to edit!",!
  ;
  FOR  DO  QUIT:(DONE)!(ABORT)
  . WRITE !,"CSV Line Editor:",!
  . WRITE "------------------",!
  . WRITE "1. Show raw CSV line data.",!
  . WRITE "2. Show resulting parsed array from data.",!
  . WRITE "3. Modify a specified piece (part) of data.",!
  . WRITE "4. Display number of pieces, and current values.",!
  . WRITE "5. Quit.",!
  . WRITE "^. Abort changes.",!
  . READ !,"Enter Choice:  ^// ",TEMP:$GET(DTIME,3600),!
  . IF TEMP="" SET TEMP="^"
  . IF TEMP=1 DO
  . . WRITE ONELINE,!
  . ELSE  IF TEMP=2 DO
  . . NEW ARRAY,TEMPRESULT
  . . SET TEMPRESULT=$$PARSLINE^TMGSEQL1(LINEOUT,.ARRAY)
  . . ;"Result: 1=OK To continue; 2 skip, but don't store as error;  -1^Message -- abort or bad data; 
  . . IF +TEMPRESULT'=-1 DO ZWRITE^TMGZWR("ARRAY")
  . . ;"else  IF PARSERESULT=0 WRITE "There was either a problem parsing this info",!
  . . ;"else  IF PARSERESULT-1 WRITE "This patient is inactive, and should be ignored",!
  . . IF +TEMPRESULT=-1 DO  
  . . . WRITE "Error parsing Alert data into patient data.",!
  . . . WRITE "MESSAGE=",$PIECE(TEMPRESULT,"^",2)
  . ELSE  IF TEMP=3 DO
  . . NEW P,VALUE
  . . WRITE "Which piece DO you want to edit?  (i.e. 1 for first CSV value, 2 for the second etc.)",!
  . . READ "Which piece?: ",P:$GET(DTIME,3600),!
  . . IF P="^" SET ABORT=1 QUIT
  . . IF +P=0 WRITE "Please enter a numeric value.",! QUIT
  . . WRITE "The current value for this piece is: ",$PIECE(LINEOUT,",",P),!
  . . READ "Enter NEW value (^ to abort): ",VALUE:$GET(DTIME,3600),!
  . . IF VALUE="^" QUIT
  . . SET $PIECE(LINEOUT,",",P)=VALUE
  . . SET RESULT=1
  . ELSE  IF TEMP=4 DO
  . . NEW IDX FOR IDX=1:1:20 DO
  . . . WRITE "Piece #",IDX," = ",$PIECE(LINEOUT,",",IDX),!
  . ELSE  IF TEMP=5 DO
  . . SET DONE=1
  . ELSE  IF TEMP="^" DO
  . . SET ABORT=1
  . ELSE  DO  QUIT
  . . WRITE "Please enter a valid choice, or ^ to abort.",!
EDLDN  ;
  IF ABORT DO
  . SET RESULT=-1
  . SET LINEOUT=INPUTSAVE
  QUIT RESULT
  ;
ALERTERR(ONELINE,PTINFO,ONEERRORARRAY,DUZ,ID) ;
  ;"Purpose: To put the error information info into TMG DEMOGRAPHICS IMPORT ERRORS (22706)
  ;"         and to create a corresponding alert
  ;"Input: ONELINE -- The original CVS format data line
  ;"       PTINFO -- PASS BY REFERENCE.  an array containing patient info, as created by PARSLINE()
  ;"       ERRORARR -- PASS BY REFERENCE.  The Array containing the error information,
  ;"          with following format:
  ;"          ERRORARR(0)=local message (if any)
  ;"          ERRORARR("DIERR")=Standard fileman DIERR array.
  ;"       DUZ -- The User IEN in file 200 (i.e. DUZ) of user to receive alert.
  ;"       ID -- Optional.  The Alert ID to set.  Default='TMGSQLIMPORT'
  ;"Output: NEW record is created in file 22706
  ;"Result: none
  NEW IEN SET IEN=$$STOREERR(ONELINE,.PTINFO,.ONEERRORARRAY)
  NEW MSG SET MSG=$GET(ONEERRORARRAY(0),"Problem with upload of Sequel data for:")
  SET MSG=$PIECE(MSG,":",1)
  SET MSG=MSG_" "_$GET(PTINFO("FULL NAME"))
  SET ID=$GET(ID,"TMGSQLIMPORT")
  DO MAKERALT(IEN,DUZ,MSG,ID) 
  QUIT
  ;
STOREERR(ONELINE,PTINFO,ERRORARR) ;
  ;"Purpose: To put the error information info into TMG DEMOGRAPHICS IMPORT ERRORS (22706)
  ;"Input: ONELINE -- The original CVS format data line
  ;"       PTINFO -- PASS BY REFERENCE.  an array containing patient info, as created by PARSLINE()
  ;"       ERRORARR -- PASS BY REFERENCE.  The Array containing the error information. Format:
  ;"          ERRORARR(0)=local message (if any)
  ;"          ERRORARR("DIERR")=Standard fileman DIERR array.
  ;"Output: NEW record is created in file 22706
  ;"Result: IEN of newly created record (or 0 IF error).
  NEW RESULT SET RESULT=0
  NEW TMGFDA,NAME
  SET NAME=$GET(PTINFO("FULL NAME3"))
  SET MSG=$GET(ERRORARR(0))
  ;
  SET TMGFDA(22706,"+1,",.01)=$GET(PTINFO("PMS ACCOUNT NUM"))   ;".01=ACCOUNT NUMBER
  SET TMGFDA(22706,"+1,",.02)="NOW"                                ;".02=CREATION DATE
  SET TMGFDA(22706,"+1,",.03)=NAME                                 ;".03=PATIENT NAME
  IF MSG'="" SET TMGFDA(22706,"+1,",1)=MSG                         ;"1=MESSAGE
  NEW TMGIENA,TMGERR
  DO UPDATE^DIE("E","TMGFDA","TMGIENA","TMGERR")
  NEW IEN SET IEN=$GET(TMGIENA(1))
  NEW TMGWP
  NEW TMGDIERR MERGE TMGDIERR("DIERR")=ERRORARR
  NEW ERRORSTR SET ERRORSTR=$$GETERRST^TMGDEBU2(.TMGDIERR)
  IF ERRORSTR="" SET ERRORSTR=$GET(ERRORARR(0))
  IF ERRORSTR'="" DO
  . DO STR2WP^TMGSTUT2(ERRORSTR,"TMGWP",60," ")
  . IF +IEN>0 DO
  . . DO WP^DIE(22706,IEN_",",3,,"TMGWP","TMGERR")                ;"3=DIERR MESSAGE
  . . NEW PRIORERR SET PRIORERR=0
  . . IF $DATA(TMGERR("DIERR")) DO SHOWDIER^TMGDEBU2(.TMGERR,.PRIORERR)
  KILL TMGWP
  DO STR2WP^TMGSTUT2(ONELINE,"TMGWP",60,",")
  IF +IEN>0 DO
  . DO WP^DIE(22706,IEN_",",2,,"TMGWP","TMGERR")                          ;"2=IMPORT DATA
  . NEW PRIORERR SET PRIORERR=0
  . IF $DATA(TMGERR("DIERR")) DO SHOWDIER^TMGDEBU2(.TMGERR,.PRIORERR)
  SET RESULT=IEN
  QUIT RESULT
  ;
MAKERALT(IEN,USER,Message,ID) ;"MAKE ERROR ALERT
  ;"Purpose: To create an alert regarding upload error
  ;"Input: IEN -- The IEN of the error, stored in file 22706
  ;"       USER -- the IEN in file 200 (i.e. DUZ) of user to receive alert.
  ;"       Message -- the Message of the alert
  ;"       ID -- Optional.  The Alert ID to set.  Default='TMGSQLIMPORT'
  ;"Output: An alert will be created in send to USER
  ;"Result: none
  NEW XQA,XQAMSG,XQAID
  NEW XQAOPT ;" ensure no residual menu option specified
  SET ID=$GET(ID,"TMGSQLIMPORT")
  SET XQA(USER)=""
  SET XQAMSG=Message
  SET XQAID=ID
  SET XQADATA=IEN
  SET XQAROU="HANDLE^TMGSEQL2"
  ;
  DO SETUP^XQALERT
  QUIT
  ;
ERREFILE(ONELINE,PTINFO,ONEERRORARRAY,DUZ)
  ;"Purpose: A common point to process errors encountering errors on refilling
  ;"Input: ONELINE -- the originial CSV data line.
  ;"       PTINFO -- PASS BY REFERENCE -- the Patient Info array, as created by PARSLINE^TMGSEQL1
  ;"       ONEERRORARRAY -- PASS BY REFERENCE -- The error array encountered, returned from Fileman
  ;"          ONEERRORARR(0)=local message (if any)
  ;"          ONEERRORARR("DIERR")=Standard fileman DIERR array.  
  ;"       DUZ -- the user IEN (from file 2) to recieve alert
  ;"Output: A NEW alert will be created, and messages written to screen
  ;"Result : none
  WRITE "There is still an error:",!
  DO ZWRITE^TMGZWR("ONEERRORARRAY")
  WRITE "A NEW alert will be made to handle this NEW error.",!
  SET ONEERRORARRAY(0)=$$NAMEERR(.ONEERRORARRAY)
  WRITE ONEERRORARRAY(0),!
  DO ALERTERR(.ONELINE,.PTINFO,.ONEERRORARRAY,DUZ)
  QUIT
  ;       
ASKHANDL ;
  ;"Purpose: ask user for entry in file 22706, and then handle error
  ;"         This will allow debug walkthrough.
  NEW X,Y,DIC,XQADATA,CQAKILL
  SET DIC=22706,DIC(0)="MAEQ"
  DO ^DIC WRITE !
  IF +Y'>0 GOTO AHDN
  SET XQADATA=+Y        
  DO HANDLE
AHDN ;
  QUIT          
  ;            
HANDLE  ;
  ;"Purpose: This is called by the alert system to handle the error alert
  ;"Input: All the inputs are via variables with global scope.  Details below
  ;"       XQADATA-- the IEN in file 22706
  ;"       XQAKILL-- 1 --> KILL when done.  To alter behavior, this function can change
  ;"              (to prevent deletion when done, then KILL XQAKILL)
  ;"Output: Allows user to edit data and reattempt filing of data
  ;"Result: none.
  NEW FIXED SET FIXED=0
  NEW ONELINE,PTINFO
  NEW TMGWP,TMGMSG
  NEW TEMPRESULT
  NEW ERRIEN
  NEW DELERROR SET DELERROR=0
  ;
  IF $GET(XQADATA)'>0 DO  GOTO HNDLDN
  . WRITE !!,"No value in XQADATA, so QUITting.",!
  . WRITE "(Deleting alert.)",!
  . SET FIXED=1,DELERROR=1
  SET ERRIEN=XQADATA
  ;
  WRITE !!,"Problem with upload of Sequel data.  ",!
  ;"TEMP
  WRITE "IEN in file# 22706=",ERRIEN,!
  ;
  NEW TEMP SET TEMP=$$GET1^DIQ(22706,ERRIEN_",",2,"","TMGWP","TMGMSG")
  IF $DATA(TMGMSG("DIERR"))'=0 DO  GOTO HNDLDN
  . NEW PRIORERR
  . DO SHOWDIER^TMGDEBU2(.TMGMSG,.PRIORERR)
  . SET FIXED=1,DELERROR=1
  SET ONELINE=$$WP2STR^TMGSTUT2("TMGWP","")
  SET TEMPRESULT=$$PARSLINE^TMGSEQL1(ONELINE,.PTINFO)
  IF +TEMPRESULT=-1 DO  
  . NEW MSG SET MSG=$PIECE(TEMPRESULT,"^",2)
  . IF MSG["MISSING ENTRY:" SET TEMPRESULT=2
  IF +TEMPRESULT=-1 DO  GOTO HNDLDN
  . WRITE "Error parsing Alert data into patient data.",!
  . WRITE "MESSAGE=",$PIECE(TEMPRESULT,"^",2)
  WRITE $GET(PTINFO("FULL NAME")),!
  ;
  NEW ERRMSG SET ERRMSG=$$GET1^DIQ(22706,ERRIEN_",",1)
  WRITE ERRMSG,!
  ;
  KILL TMGWP,TMGMSG
  SET TEMP=$$GET1^DIQ(22706,ERRIEN_",",3,"","TMGWP","TMGMSG")
  IF $DATA(TMGMSG("DIERR"))'=0 DO  GOTO HNDLDN
  . NEW PRIORERR
  . DO SHOWDIER^TMGDEBU2(.TMGMSG,.PRIORERR)
  . SET FIXED=1,DELERROR=1
  NEW LONGWPERRORSTR SET LONGWPERRORSTR=""
  IF $DATA(TMGWP) DO
  . DO WriteWP^TMGSTUTL("TMGWP")
  . SET LONGWPERRORSTR=$$WP2STR^TMGSTUT2("TMGWP")
  ;
  IF (ERRMSG["PATIENT NOT IN DATABASE")!(ERRMSG["Sex is not defined") DO
  . SET FIXED=$$FIXREG(.PTINFO,.ONELINE,.DELERROR)
  IF 'FIXED,ERRMSG["INVALID/MISSING GENDER" DO
  . SET FIXED=$$FIXREG(.PTINFO,.ONELINE,.DELERROR)
  IF 'FIXED,(LONGWPERRORSTR["[FIELD]=.09")&(LONGWPERRORSTR["is not valid") DO
  . SET FIXED=$$FIXALIAS(.PTINFO,LONGWPERRORSTR,.ONELINE,.DELERROR)
  IF 'FIXED,ERRMSG["CONFLICTING SS-NUMBERS" DO
  . SET FIXED=$$FIXSSN(.PTINFO,ERRMSG,.ONELINE,.DELERROR)
  IF 'FIXED,ERRMSG["INVALID DOB ERROR" DO
  . WRITE "Date of birth (DOB) is incorrect for this patient.",!
  . WRITE "Note:  The recommended method of correcting this problem is",!
  . WRITE "       to fix the problem in Sequel, not here.  Otherwise",!
  . WRITE "       the same error will be encountered with each demographics",!
  . WRITE "       upload.",!!
  . SET FIXED=$$FIXGENPB(.PTINFO,ERRMSG,.ONELINE,.DELERROR)
  IF 'FIXED DO
  . SET FIXED=$$FIXGENPB(.PTINFO,ERRMSG,.ONELINE,.DELERROR,ERRIEN)
  ;
  IF DELERROR=1 DO
  . NEW TEMP,ERRORARR
  . SET TEMP=$$DelIEN^TMGDBAPI(22706,ERRIEN,.ERRORARR) ;"success, so KILL error entry in 22706
  . WRITE "(Alert deleted)",!
  ;
HNDLDN ;
  ;"if FIXED=1 WRITE !,"SUCCESS!"
  IF (DELERROR=0) DO     ;"<------------- this logic may be off...
  . KILL XQAKILL ;"--> don't delete alert
  . WRITE "(Saving alert...)",!
  ;
  QUIT
  ;
FIXREG(PTINFO,ONELINE,DELERROR)  ;"FIX REGISTRATION PROBLEM.
  ;"Purpose: To fix problems where patient couldn't be added to the database
  ;"Input: PTINFO -- PASS BY REFERENCE -- the Patient Info array, as created by PARSLINE^TMGSEQL1
  ;"       ONELINE -- the originial CSV data line.  Passed to this function in case a NEW Alert
  ;"                  must be created, in which case it is stored in the NEW error message.
  ;"       DELERROR -- and OUT parameter.  Set to 1 will signal the deletion of the error
  ;"              record in file 22706
  ;"Output: Patient may be added to FILE 2, or file updated.  If succesfull, record of error
  ;"      in file 22706 will deleted
  ;"Result: 1=problem fixed, 0=not fixed.
  NEW FIXED SET FIXED=0
  SET DELERROR=0
  NEW TMGREMSEX,INITREMSEX
  SET TMGREMSEX=+$$GET1^DIQ(22711,"1,","PICK GENDER FROM NAME?","I")
  SET INITREMSEX=TMGREMSEX
  ;                                  
  NEW AUTOREG SET AUTOREG=1  ;"automatically add patient to database IF not found
  NEW ONEERRORARRAY,CHGLOG
  NEW DONE SET DONE=0
  FOR  DO  QUIT:(DONE=1)
  . KILL ONEERRORARRAY,CHGLOG
  . NEW TEMPRESULT        
  . SET TEMPRESULT=$$UPDATEDB^TMGSEQL1(.PTINFO,AUTOREG,.ONEERRORARRAY,.CHGLOG) ;"0=error
  . SET DELERROR=1
  . SET FIXED=1
  . SET DONE=1
  . IF TEMPRESULT=0 DO
  . . IF $$SEXMISSING(.ONEERRORARRAY)=1 DO
  . . . IF $$FIXSEXM(.PTINFO,.TMGREMSEX)=0 DO
  . . . . SET DONE=1
  . . . . SET FIXED=0
  . . . ELSE  SET DONE=0  ;"loop for retry of UPDATEDB
  . . ELSE  DO         
  . . . WRITE "There is still an error:",!
  . . . WRITE "A NEW alert will be made to handle this NEW error.",!
  . . . DO ERREFILE(.ONELINE,.PTINFO,.ONEERRORARRAY,DUZ)
  ;
  IF TMGREMSEX'=INITREMSEX DO   ;"if status of auto-pick gender was changed in FIXSEXM, store in settings.
  . NEW TMGFDA,TMGMSG
  . SET TMGFDA(22711,"1,",6)=TMGREMSEX ;"FIELD# 6='PICK GENDER FROM NAME?'
  . DO FILE^DIE("E","TMGFDA","TMGMSG")  ;"note TMGMSG is ignored here...
  ;
  QUIT FIXED
  ;                        
SEXMISSING(ERRORARR)  ;"IS SEX MISSING       
  ;"Purpose: To analyze a Fileman error array and see IF field .02 (SEX) is missing, causing problem
  ;"Input: ERRORARR -- PASS BY REFERENCE, an error message, as created by Fileman while adding patient.
  ;"Result: 1=missing sex (.02 field), other 0
  ;"Note: this only reviews error #1 (ignores other errors, IF present.  So, IF missing sex error
  ;"      was in position #2, this function WOULD RETURN AN ERRORONEOUS ANSWER.
  NEW RESULT SET RESULT=0
  IF $DATA(ERRORARR("DIERR","E",311,1)) DO  ;"311=The record lacks some required identifiers.
  . IF $GET(ERRORARR("DIERR",1,"PARAM","FIELD"))'=.02 QUIT
  . IF $GET(ERRORARR("DIERR",1,"PARAM","FILE"))'=2 QUIT
  . SET RESULT=1
  ELSE  IF $GET(ERRORARR(0))["Sex is not defined" DO
  . SET RESULT=1
  QUIT RESULT
  ;
FIXSEXM(PTINFO,TMGREMSEX) ;"GET / FIX SEX MISSING   
  ;"Purpose: To correct the PTINFO ARRAY so that SEX is supplied answer.
  ;"Input: PTINFO -- PASS BY REFERENCE -- the Patient Info array, as created by PARSLINE^TMGSEQL1
  ;"       TMGREMSEX --PASS BY REFERENCE -- 1 if OK to automatically pick sex based on gender of name
  ;"Output: PTINFO should be filled with SEX of patient
  ;"Result: 1=OK to continue, 0=failed to get SEX
  NEW RESULT SET RESULT=0  ;"default to failure
  NEW TEMP SET TEMP=""
  NEW ABORT SET ABORT=0
  ;
  IF $GET(PTINFO("SEX"))'="" SET RESULT=1 GOTO FSXDN
  IF $GET(PTINFO("FULL NAME"))="" GOTO FSXDN
  NEW FNAME SET FNAME=$GET(PTINFO("FIRST NAME"))
  IF FNAME="" GOTO FSXDN
  ;
  FOR  DO  QUIT:(TEMP'="")!(ABORT=1)
  . NEW PRESUMEDSEX,REMNAME
  . SET CURRENTSEX=""
  . SET TMGREMSEX=$GET(TMGREMSEX,0)
  . WRITE "Trying to determine the SEX of: ",PTINFO("FULL NAME"),!!
  . WRITE "OPTIONS:",!
  . WRITE "-----------------",!
  . WRITE "M  or MALE    --> Name is MALE",!
  . WRITE "M! or MALE!   --> ALWAYS consider this name as MALE",!
  . WRITE "F  or FEMALE  --> Name is FEMALE",!
  . WRITE "F! or FEMALE! --> ALWAYS consider this name as FEMALE",!
  . WRITE "AUTO          --> Turn auto-pick-gender: ",$SELECT(TMGREMSEX=1:"OFF",1:"ON"),!
  . WRITE "^   Abort",!
  . SET PRESUMEDSEX=$$GETSEX(FNAME)
  . WRITE "Is ",FNAME," MALE or FEMALE? ",PRESUMEDSEX,"//"
  . IF (TMGREMSEX=1)&(PRESUMEDSEX'="") SET TEMP=PRESUMEDSEX
  . ELSE  READ TEMP:$GET(DTIME,3600)
  . IF TEMP="" SET TEMP=PRESUMEDSEX
  . SET REMNAME=(TEMP["!")
  . SET TEMP=$TRANSLATE(TEMP,"!","")
  . SET TEMP=$$UP^XLFSTR(TEMP)
  . IF (TEMP="M")!(TEMP="MALE") SET CURRENTSEX="MALE"
  . ELSE  IF (TEMP="F")!(TEMP="FEMALE") SET CURRENTSEX="FEMALE"
  . ELSE  IF TEMP="^" DO  QUIT
  . . WRITE "aborting..",!
  . . SET ABORT=1
  . ELSE  IF TEMP="AUTO" DO
  . . SET TMGREMSEX='(TMGREMSEX)
  . IF CURRENTSEX'="" DO  QUIT
  . . WRITE "  ",CURRENTSEX,!
  . . SET PTINFO("SEX")=CURRENTSEX
  . . SET RESULT=1
  . . IF REMNAME DO
  . . . NEW TEMP SET TEMP=$$SETSEX(FNAME,CURRENTSEX)
  . SET TEMP="" ;" a signal to try again.
  ;
FSXDN ;
  QUIT RESULT
  ;
FIXSSN(PTINFO,ERRMSG,ONELINE,DELERROR)  ;
  ;"Purpose: To fix problems of conflicting SS numbers
  ;"Input: PTINFO -- PASS BY REFERENCE -- the Patient Info array, as created by PARSLINE^TMGSEQL1
  ;"       ERRMSG -- the message that holds the conflicting SSNums
  ;"       ONELINE -- the originial CSV data line.  Passed to this function in case a NEW Alert
  ;"                  must be created, in which case it is stored in the NEW error message.
  ;"       DELERROR -- and OUT parameter.  Set to 1 will signal the deletion of the error
  ;"              record in file 22706
  ;"Output: Patient may be added to FILE 2, or file updated.  If succesfull, record of error
  ;"      in file 22706 will deleted
  ;"Result: 1=problem fixed, 0=not fixed.
  NEW SEQSSN,VISTASSN
  NEW FIXED SET FIXED=0
  NEW DONE SET DONE=0
  SET DELERROR=0                                  
  IF $GET(ERRMSG)="" GOTO FSNDN
  IF ERRMSG["(Sequel#)" DO  ;"old format
  . SET SEQSSN=$PIECE(ERRMSG,"SS-NUMBERS: ",2)
  . SET SEQSSN=$PIECE(SEQSSN," ",1)
  . SET VISTASSN=$PIECE(ERRMSG,"vs. ",2)
  . SET VISTASSN=$PIECE(VISTASSN," ",1)
  ELSE  DO
  . SET SEQSSN=$PIECE(ERRMSG,"Sequel#=",2)
  . SET SEQSSN=$PIECE(SEQSSN," ",1)
  . SET VISTASSN=$PIECE(ERRMSG,"VistA#=",2)
  . SET VISTASSN=$PIECE(VISTASSN," ",1)
  ;
  NEW VFULLNAME
  DO  ;"get actual full name & DOB for VistA SSN
  . NEW VISTANAME,VISTADOB
  . NEW TEMPDFN SET TEMPDFN=$$SSNLKUP^TMGGDFN(VISTASSN)
  . NEW TMGMSG,TMGERR,IENS
  . SET IENS=+TEMPDFN_","
  . DO GETS^DIQ(2,IENS,".01;.03","E","TMGMSG","TMGERR")
  . IF $DATA(TMGERR("DIERR")) DO
  . . NEW PRIORERR
  . . DO SHOWDIER^TMGDEBU2(.TMGMSG,.PRIORERR)
  . SET VISTANAME=$GET(TMGMSG(2,IENS,.01,"E"))
  . SET VISTADOB=$GET(TMGMSG(2,IENS,.03,"E"))
  . SET VFULLNAME=VISTANAME_" ("_VISTADOB_")"
  ;
  WRITE !                                                         
  ;
  FOR  DO  QUIT:(DONE=1)
  . WRITE "There is a conflict between Social Security Numbers (SSN):",!
  . WRITE "1.  ",SEQSSN," is the Sequel SSN for: ",$GET(PTINFO("FULL NAME2")),!
  . WRITE "2.  ",VISTASSN," is the VistA SSN for:  ",$GET(VFULLNAME),!
  . WRITE "3.  (Don't change either one, but remove alert)",!
  . WRITE !,"Which SSN is correct? (1, 2, 3, or ^ to abort)? // "
  . NEW TEMP READ TEMP:$GET(DTIME,3600),!
  . IF TEMP="^" SET DONE=1 QUIT  ;"QUIT, error unfixed.
  . IF TEMP=3 DO  QUIT  ;"keep both
  . . WRITE "OK, no data changes made.  Will delete alert.",!
  . . SET FIXED=1,DONE=1,DELERROR=1
  . IF TEMP=2 DO  QUIT  ;"keep VistA, advice manual fix in Sequel database, delete alert.
  . . WRITE "OK.  Please manually alter the SSN in the Sequel Database.  This should then be",!
  . . WRITE "reflected in the next demographic data upload cycle.",!
  . . SET FIXED=1 ;"This will signal the deletion of the alert
  . . SET DONE=1
  . IF TEMP=1 DO  ;"keep Sequel, delete VistA SSN
  . . SET DONE=1
  . . SET FIXED=1
  . . SET DELERROR=1
  . . NEW DFN SET DFN=$$GETDFN^TMGSEQL1(.PTINFO)
  . . NEW TMGFDA,TMGMSG,TEMPRESULT
  . . SET TMGFDA(2,DFN_",",.09)="@"  ;"delete .09 field (SSN)
  . . SET TEMPRESULT=$$dbWrite^TMGDBAPI(.TMGFDA,1,,,.TMGMSG)
  . . IF TEMPRESULT=0 QUIT  ;"error found, so QUIT
  . . ;"Now try filing again.
  . . NEW ONEERRORARRAY,CHGLOG
  . . NEW AUTOREG SET AUTOREG=0  ;"should need to add patient, as must exist to confilict in first place!
  . . SET TEMPRESULT=$$UPDATEDB^TMGSEQL1(.PTINFO,AUTOREG,.ONEERRORARRAY,.CHGLOG) ;"0=error
  . . IF TEMPRESULT=0 DO
  . . . DO ERREFILE(.ONELINE,.PTINFO,.ONEERRORARRAY,DUZ)
FSNDN ;           
  QUIT FIXED
  ;
GETSEX(NAME) ;
  ;"Purpose: To return gender of Name, as stored in file 22707
  ;"Input: NAME -  a FIRST name
  ;"Result: Returns MALE, FEMALE, or "" IF not found
  NEW RESULT SET RESULT=""
  IF $GET(NAME)="" GOTO GSXDN
  NEW DIC,X,Y
  SET DIC=22707
  SET DIC(0)="M"
  SET X=NAME
  DO ^DIC
  IF +Y'>0 GOTO GSXDN
  SET RESULT=$$GET1^DIQ(22707,+Y_",",1)
GSXDN ;
  QUIT RESULT
  ;
SETSEX(NAME,SEX)  ;
  ;"Purpose: To create a NEW record in file 22707 to store gender of name
  ;"Input: NAME -- a FIRST name to store gender for
  ;"       SEX -- should be "MALE", or "FEMALE"
  ;"Note: Will not DO anything IF a record for name already exists
  ;"Result: 1=OK to continue  0=some error
  NEW RESULT SET RESULT=1
  IF '$DATA(NAME)!'$DATA(SEX) GOTO SSXDN
  IF $$GETSEX(NAME)'="" GOTO SSXDN
  NEW TMGFDA
  SET TMGFDA(22707,"+1,",.01)=NAME
  SET TMGFDA(22707,"+1,",1)=SEX
  SET RESULT=$$dbWrite^TMGDBAPI(.TMGFDA,0)
SSXDN ;
  QUIT RESULT
  ;
NAMEERR(ONEERRORARRAY)
  ;"Purpose: to review a fileman "DIERR" array and pick out common problems
  ;"Input: ONEERRORARRAY -- a fileman array containing "DIERR" message
  ;"Result: return a name for error
  NEW RESULT SET RESULT=""
  NEW ARRAY
  IF $DATA(ONEERRORARRAY("DIERR"))>1 DO
  . MERGE ARRAY=ONEERRORARRAY("DIERR")
  ELSE  DO
  . MERGE ARRAY=ONEERRORARRAY
  NEW FIELD SET FIELD=$GET(ARRAY(1,"PARAM","FIELD"))
  IF $DATA(ARRAY)>0 DO
  . NEW FILENUM SET FILENUM=+$GET(ARRAY(1,"PARAM","FILE"))
  . IF (FILENUM>0)&(FILENUM'=2) QUIT
  . IF FIELD>0 SET RESULT="FILEMAN ERROR:"
  . IF FIELD=.03 DO
  . . SET RESULT="INVALID DOB ERROR:"
  . IF FIELD=.02 DO
  . . SET RESULT="INVALID/MISSING GENDER:"
  . IF $DATA(ARRAY(1,"TEXT")) DO
  . . NEW STR SET STR=$GET(ARRAY(1,"TEXT",1))
  . . SET RESULT=RESULT_$EXTRACT(STR,1,80)_"..."
  . IF RESULT["CONFLICTING SS-NUMBERS" DO
  . . SET RESULT="CONFLICTING SS-NUMBERS: "
  IF RESULT="" SET RESULT=$GET(ARRAY(0),"Sequel Import Error:")
  QUIT RESULT
  ;
FIXGENPB(PTINFO,ERRMSG,ONELINE,DELERROR,ERRIEN) ;"FIX GENERAL PROBLEM
  ;"Purpose: To fix a generic (no specified) error
  ;"Input: PTINFO -- PASS BY REFERENCE -- the Patient Info array, as created by PARSLINE^TMGSEQL1
  ;"       ERRMSG -- the message that holds the conflicting SSNums
  ;"       ONELINE -- the originial CSV data line.  Passed to this function in case a NEW Alert
  ;"                  must be created, in which case it is stored in the NEW error message.
  ;"       DELERROR -- and OUT parameter.  Set to 1 will signal the deletion of the error
  ;"              record in file 22706
  ;"       ERRIEN -- the IEN in file 22706 containing full error info.
  ;"Output: Patient may be added to FILE 2, or file updated.  If succesfull, record of error
  ;"      in file 22706 will deleted
  ;"Result: 1=problem fixed, 0=not fixed.
  NEW FIXED SET FIXED=0
  NEW DONE SET DONE=0
  SET DELERROR=0
  NEW DONE SET DONE=0
  NEW AUTOREG SET AUTOREG=1  ;"automatically add patient to database IF not found
  NEW TEMP SET TEMP="?"
  FOR  DO  QUIT:(DONE=1)
  . IF TEMP="?" DO  QUIT
  . . WRITE "Options:",!
  . . WRITE "-----------------",!
  . . WRITE "D   Show the data line from the other computer (Sequel)",!
  . . WRITE "E   Edit data line.",!
  . . WRITE "R   Retry filing data into database to get more information.",!
  . . WRITE "S   Show parsed patient information.",!
  . . WRITE "X   Delete this Alert.",!
  . . WRITE "Q   Query the database to see existing entries.",!
  . . WRITE "^   Abort.",!
  . . SET TEMP=""
  . ELSE  IF TEMP="Q" DO  QUIT
  . . NEW DIC SET DIC=2
  . . SET DIC(0)="AEQM"
  . . DO ^DIC
  . . SET TEMP=""
  . ELSE  IF TEMP="D" DO  QUIT
  . . WRITE !,ONELINE,!
  . . SET TEMP=""
  . ELSE  IF TEMP="S" DO  QUIT
  . . DO ZWRITE^TMGZWR("PTINFO")
  . . SET TEMP=""
  . ELSE  IF TEMP="E" DO  QUIT
  . . NEW TEMPRESULT,NEWLINE              
  . . SET TEMPRESULT=$$EDT1LINE(ONELINE,.NEWLINE)
  . . IF TEMPRESULT=1 SET ONELINE=NEWLINE ;"NOTE: later I will save old line to keep from having to process each update cycle
  . . KILL PTINFO
  . . SET TEMPRESULT=$$PARSLINE^TMGSEQL1(ONELINE,.PTINFO)
  . . IF +TEMPRESULT=-1 DO  QUIT
  . . . WRITE "There was a problem processing this line after your edit.  Sorry!",!
  . . . WRITE "MESSAGE=",$PIECE(TEMPRESULT,"^",2)
  . . WRITE "OK, now try refilling data into database.",!
  . . SET TEMP="?" 
  . ELSE  IF TEMP="^" DO  QUIT
  . . WRITE "aborting..",!
  . . SET DONE=1
  . ELSE  IF TEMP="X" DO  QUIT
  . . WRITE "OK, will delete this alert.",!
  . . ;"Note: DO something to delete alert.
  . . SET DONE=1,DELERROR=1,FIXED=1
  . ELSE  IF TEMP="R" DO  QUIT         
  . . NEW ONEERRORARRAY,CHGLOG 
  . . SET TEMPRESULT=$$UPDATEDB^TMGSEQL1(.PTINFO,AUTOREG,.ONEERRORARRAY,.CHGLOG) ;"0=error
  . . SET DELERROR=1
  . . SET FIXED=1 ;"consider 'fixed' so alert will be deleted
  . . SET DONE=1
  . . IF TEMPRESULT=0 DO
  . . . DO ERREFILE(.ONELINE,.PTINFO,.ONEERRORARRAY,DUZ)
  . READ !,"Enter Option: ?//",TEMP:$GET(DTIME,3600),!
  . IF TEMP="" SET TEMP="?"
  . SET TEMP=$$UP^XLFSTR(TEMP)
  . QUIT                 
  ;                     
  QUIT FIXED
  ;
FIXALIAS(PTINFO,ERRMSG,ONELINE,DELERROR) ;"FIX ALIAS PROBLEM
  ;"Purpose: To fix alias issues, with SSN collisions
  ;"Input: PTINFO -- PASS BY REFERENCE -- the Patient Info array, as created by PARSLINE^TMGSEQL1
  ;"       ERRMSG -- the message that holds the problem SSNum
  ;"       ONELINE -- the originial CSV data line.  Passed to this function in case a NEW Alert
  ;"                  must be created, in which case it is stored in the NEW error message.
  ;"       DELERROR -- and OUT parameter.  Set to 1 will signal the deletion of the error
  ;"              record in file 22706
  ;"       ERRIEN -- the IEN in file 22706 containing full error info.
  ;"Output: Patient may be added to FILE 2, or file updated.  If succesfull, record of error
  ;"      in file 22706 will deleted
  ;"Result: 1=problem fixed, 0=not fixed.
  NEW FIXED SET FIXED=0
  NEW SSN SET SSN=$PIECE($PIECE(ERRMSG,"[3]=",2)," ",1)
  IF SSN="" GOTO FXAPDN
  NEW X,Y,DIC SET DIC=2
  SET X=SSN              
  DO ^DIC
  IF +Y'>0 DO  GOTO FXAPDN
  . WRITE !,"Tried to fix as an Alias issue...",!
  . WRITE "Apparent SSN collision, but not able to find other record.",!
  NEW DBPATNAME SET DBPATNAME=$$GET1^DIQ(2,+Y_",",.01)
  SET DBPATNAME=DBPATNAME_"  ["_$$GET1^DIQ(2,+Y_",",.02)_"]"
  SET DBPATNAME=DBPATNAME_" ("_$$GET1^DIQ(2,+Y_",",.03)_")"
  ;                        
  WRITE !
  WRITE "1. "_DBPATNAME,"  (in VistA)",!
  WRITE "2. "_$GET(PTINFO("FULL NAME2")),"  (In Sequel)",!
  WRITE "Are these two people the SAME person"
  NEW % SET %=2 DO YN^DICN WRITE !,!
  IF %=-1 GOTO FXAPDN
  SET DELERROR=1
  IF %'=1 GOTO FXAPDN
  WRITE "Add ",PTINFO("FULL NAME3")," as an ALIAS of ",DBPATNAME
  SET %=2 DO YN^DICN WRITE !
  IF %'=1 GOTO FXAPDN
  NEW TMGFDA,TMGIEN,TMGMSG
  SET TMGFDA(2.01,"+1,"_+Y_",",.01)=$GET(PTINFO("FULL NAME3"))
  DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR"))'=0 DO  GOTO FXAPDN
  . NEW PRIORERR
  . DO SHOWDIER^TMGDEBU2(.TMGMSG,.PRIORERR)
  WRITE PTINFO("FULL NAME3")," added as an alias into record of ",DBPATNAME,!
  SET FIXED=1
FXAPDN ;         
  QUIT FIXED 
  ;
