TMGGDFN  ;TMG/kst-Get A Patient's IEN (DFN) ;01/01/04; 7/31/15; 3/1/16
   ;;1.0;TMG-LIB;**1**;06/04/08;Build 7
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"TMG GET DFN (TMGGDFN)
 ;"
 ;"Purpose:  This module will provide functionality for getting a DFN
 ;"        (which is the database record number) for a given patient.
 ;"        If the patient has not been encountered before, then the patient
 ;"        will be added to the database.
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"$$GETDFN(.PatientInfo) -- Ensure that a patient is registered, return IEN
 ;"$$GETDFN2(.ENTRY,AUTOREG) -- Get patient DFN, possibly auto-registering
 ;"PAT2ENTRY(Patient,ENTRY) convert a named-node entry, into numeric 'ENTRY' array:
 ;"$$ADDNEWPAT(ENTRY)
 ;"$$LMH2DFN(LMHMRN) -- Return DFN from Laughlin hospital MR Num
 ;"$$EXTRLKUP(ENTRY,INTENSITY) ;
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"LOOKUPPAT(ENTRY)
 ;"SSNLKUP(SSNUM)
 ;"PMSNLKUP(PMSNUM)
 ;"PARDMLKUP(PMSNUM)
 ;"LMHLookup(MRN)
 ;"COMPARE(TESTDATA,DBDATA,EntryNUM)
 ;"COMPENTRY(TESTDATA,DBDATAENTRY)
 ;"XCOMPENTRY(TESTDATA,DBDATAENTRY,THRESHOLD) -- compare two entries for certain fields, and return a comparison code.
 ;"$$ADDTOPAT(DFN,ENTRY)
 ;"=======================================================================
 ;"PRIVATE FUNCTIONS
 ;"=======================================================================
 ;
GETDFN(PATIENT,AUTOREG)         ;
  ;"Purpose:  This code is to ensure that a patient is registered
  ;"  It is intended for use during upload of old records
  ;"  from another EMR.  As each dictation is processed,
  ;"  this function will be called with the header info.
  ;"  If the patient is already registered, then this function
  ;"  will have no effect other than to return the DFN.
  ;"  Otherwise, the patient will be registered.
  ;"Input: 
  ;"  PATIENT: Array is loaded with PATIENT, like this:
  ;"    PATIENT("SSNUM")="123-45-6789"  <-- I think this is optional when looking up patient
  ;"    PATIENT("NAME")="DOE,JOHN"
  ;"    PATIENT("DOB")="01-04-69"
  ;"    PATIENT("PATIENTNUM")="12345677" <-- Medic account number (optional)
  ;"    PATIENT("SEQUELNUM")="234567890"  <-- SequelMedSystems Account number (optional)
  ;"    PATIENT("PARADIGMNUM")="234567890"  <-- Pardigm Account number (optional)
  ;"    PATIENT("SEX")="M"
  ;"    PATIENT("ALIAS")="DOE,JOHNNY"
  ;"    -Note: The following are optional, only used if adding a patient
  ;"  If adding a patient, and these are not supplied, then defaults of
  ;"  Not a veteran, NON-VETERAN type, Not service connected are used
  ;"    PATIENT("VETERAN")= VETERAN Y/N --For my purposes, use NO -- optional
  ;"    PATIENT("PT_TYPE")= "SERVICE CONNECTED?" -- required field -- optional
  ;"    PATIENT("SERVICE_CONNECTED")= "TYPE" - required field -- optional
  ;"    PATIENT("ADDRESS1")= "100 MAIN STREET" -- optional
  ;"    PATIENT("ADDRESS2")= "APT. 1A"  -- optional
  ;"    PATIENT("CITY")= "NEW YORK" -- optional
  ;"    PATIENT("STATE")= "NY" -- optional
  ;"    PATIENT("ZIP")=12345  --  optional
  ;
  ;" AUTOREG   OPTIONAL.  IF AUTOREG=1, and patient is not found, then
  ;"    patient will be automatically registered as a NEW patient.
  ;"NOTE: Globally-scoped variable TMGFREG is used (esp from from TMGUPLD)
  ;"        If globally-scoped TMGFREG=1, this will be same as AUTOREG=1
  ;"Output:  The patient's info is used to register the patient, if they are
  ;"         are not already registered
  ;"Result: DFN (patient IEN) if prior entry found
  ;"        -1^Message if not found or added.
  ;"        DFN^1 if NEW patient registered.   
  ;"        0 if not found/added,
  ;"        0^Message if AUTOREG=1 (auto-register) specified, but it failed. 
  ;"        NOTE: DFN is the patient's internal entry number
  ;"------------------------------------------------------------------------------
  NEW TMGRESULT,ENTRY
  NEW TMGZZ SET TMGZZ=0
  IF TMGZZ=1 DO
  . KILL PATIENT,AUTOREG
  . MERGE PATIENT=^TMG("TMP","RPC","GETDFN^TMGGDFN","PATIENT")
  . MERGE AUTOREG=^TMG("TMP","RPC","GETDFN^TMGGDFN","AUTOREG")
  ELSE  DO
  . KILL ^TMG("TMP","RPC","GETDFN^TMGGDFN")
  . MERGE ^TMG("TMP","RPC","GETDFN^TMGGDFN","PATIENT")=PATIENT
  . MERGE ^TMG("TMP","RPC","GETDFN^TMGGDFN","AUTOREG")=AUTOREG
  DO PAT2ENTRY(.PATIENT,.ENTRY)
  IF $GET(TMGFREG)=1 SET AUTOREG=1
  SET TMGRESULT=$$GETDFN2(.ENTRY,.AUTOREG)
ERDN ;
  QUIT TMGRESULT
  ;
GETDFN2(ENTRY,AUTOREG)        ;
  ;"Purpose: Get patient DFN (i.e. IEN), possibly registering IF needed.
  ;"         This function is very similar to GETDFN, but slightly streamlined.
  ;"Input: 
  ;"  ENTRY: Array is loaded with Patient, like this:
  ;"    ENTRY(.01)=PatientName, e.g. DOE,JOHN
  ;"    ENTRY(.02)=Sex          e.g. M
  ;"    ENTRY(.03)=DOB          e.g. 01-04-69
  ;"    --Below are optional (depending IF fields have Fileman 'required' status)
  ;"    ENTRY(.09)=SSNUM        e.g. 123-45-6789
  ;"    ENTRY(10,.01)=ALIAS     e.g. DOE,JOHNNY
  ;"    ENTRY(1901)=VETERAN
  ;"    ENTRY(.301)=PT_TYPE
  ;"    ENTRY(391)=SERVICE_CONNECTED
  ;"    ENTRY(22700)=PatientNUM
  ;"    ENTRY(22701)=PMS ACCOUNT NUM
  ;"    ENTRY(22701)=SEQUELNUM
  ;"    ENTRY(22702)=PARADIGM
  ;" AUTOREG: OPTIONAL.  If 1, then patient will be registered IF not found.
  ;"Output:  The patient's info is used to register the patient, IF they are
  ;"      are not already registered
  ;"Result: DFN (patient IEN) if prior entry found
  ;"        DFN^1 if NEW patient registered.   //kt added 2/2/11
  ;"        0 if not found/added,
  ;"        -1^Message if Autoregister specified, and failed.  //kt added 2/2/11
  ;"------------------------------------------------------------------------------
  NEW TMGRESULT SET TMGRESULT=$$LOOKUPPAT(.ENTRY)
  IF (TMGRESULT>0)!($GET(AUTOREG)'=1) GOTO DFN2DN
  SET TMGRESULT=$$ADDNEWPAT(.ENTRY)
  IF TMGRESULT>0 SET TMGRESULT=TMGRESULT_"^1"
DFN2DN  ;
  QUIT TMGRESULT        
  ;
PAT2ENTRY(PATIENT,ENTRY)        ;
  ;"Purpose: to convert a named-node entry, into numeric 'ENTRY' array:
  ;"Input: 
  ;"PATIENT: PASS BY REFERENCE.  Array loaded with patient info:
  ;"  PATIENT("SSNUM")="123-45-6789"
  ;"  PATIENT("NAME")="DOE,JOHN"
  ;"  PATIENT("DOB")="01-04-69"
  ;"  PATIENT("PATIENTNUM")="12345677"   <-- Medic account number
  ;"  PATIENT("SEQUELNUM")="234567890"   <-- SequelMedSystems Account number
  ;"  PATIENT("PARADIGMNUM")="234567890" <-- Pardigm Account number
  ;"  PATIENT("SEX")="M"
  ;"  PATIENT("ALIAS")="DOE,JOHNNY"
  ;"  -Note: The following are optional, only used IF adding a patient
  ;"  If adding a patient, and these are not supplied, then defaults of
  ;"  Not a veteran, NON-VETERAN type, Not service connected are used
  ;"  PATIENT("VETERAN")= VETERAN Y/N --For my purposes, use NO -- optional
  ;"  PATIENT("PT_TYPE")= "SERVICE CONNECTED?" -- required field -- optional
  ;"  PATIENT("SERVICE_CONNECTED")= "TYPE" - required field -- optional
  ;"  PATIENT("ADDRESS1")= "100 MAIN STREET" --  optional
  ;"  PATIENT("ADDRESS2")= "APT. 1A"  -- optional
  ;"  PATIENT("CITY")= "NEW YORK" -- optional
  ;"  PATIENT("STATE")= "NY" -- optional
  ;"  PATIENT("ZIP")=12345  --  optional
  ;"ENTRY; PASS BY REFERENCE, an OUT PARAMETER.
  ;"  ENTRY(.01)=PatientName
  ;"  ENTRY(.02)=Sex
  ;"  ENTRY(.03)=DOB
  ;"  ENTRY(.09)=SSNUM
  ;"  ENTRY(22700)=PatientNUM
  ;"  ENTRY(22701)=PMS ACCOUNT NUM
  ;"  ENTRY(22701)=SEQUELNUM
  ;"  ENTRY(22702)=PARADIGM
  ;"  ENTRY(10,.01)=ALIAS
  ;"  ENTRY(1901)=VETERAN
  ;"  ENTRY(.301)=PT_TYPE
  ;"  ENTRY(391)=SERVICE_CONNECTED
  ;"  ENTRY(.111)=ADDRESS 1
  ;"  ENTRY(.112)=ADDRESS 2
  ;"  ENTRY(.114)=CITY
  ;"  ENTRY(.115)=STATE
  ;"  ENTRY(.116)=ZIP
  ;"Results: None
  ;
  IF $DATA(PATIENT("NAME")) SET ENTRY(.01)=$GET(PATIENT("NAME"))
  IF $DATA(PATIENT("SEX")) SET ENTRY(.02)=$GET(PATIENT("SEX"))
  IF $DATA(PATIENT("DOB")) SET ENTRY(.03)=$GET(PATIENT("DOB"))
  IF $DATA(PATIENT("SSNUM")) SET ENTRY(.09)=$GET(PATIENT("SSNUM"))
  IF $DATA(PATIENT("PATIENTNUM")) SET ENTRY(22700)=$GET(PATIENT("PATIENTNUM"))
  IF $DATA(PATIENT("PMS ACCOUNT NUM")) SET ENTRY(22701)=$GET(PATIENT("PMS ACCOUNT NUM"))
  IF $DATA(PATIENT("SEQUELNUM")) SET ENTRY(22701)=$GET(PATIENT("SEQUELNUM"))
  IF $DATA(PATIENT("PARADIGMNUM")) SET ENTRY(22702)=$GET(PATIENT("PARADIGM"))
  IF $DATA(PATIENT("ALIAS")) SET ENTRY(10,.01)=$GET(PATIENT("ALIAS"))
  ;
  IF $DATA(PATIENT("VETERAN")) SET ENTRY(1901)=PATIENT("VETERAN")
  IF $DATA(PATIENT("PT_TYPE")) SET ENTRY(.301)=PATIENT("PT_TYPE")
  IF $DATA(PATIENT("SERVICE_CONNECTED")) SET ENTRY(391)=PATIENT("SERVICE_CONNECTED")
  IF $DATA(PATIENT("ADDRESS1")) SET ENTRY(.111)=PATIENT("ADDRESS1")
  IF $DATA(PATIENT("ADDRESS2")) SET ENTRY(.112)=PATIENT("ADDRESS2")
  IF $DATA(PATIENT("CITY")) SET ENTRY(.114)=PATIENT("CITY")
  IF $DATA(PATIENT("STATE")) SET ENTRY(.115)=PATIENT("STATE")
  IF $DATA(PATIENT("ZIP")) SET ENTRY(.116)=PATIENT("ZIP")
  QUIT
  ;
LOOKUPPAT(ENTRY)        ;
  ;"Purpose: Search for Patient (an existing entry in the database)
  ;"Input: ENTRY -- Array is loaded with info, like this:
  ;"        SET ENTRY(.01)=Name
  ;"        SET ENTRY(.02)=Sex
  ;"        SET ENTRY(.03)=DOB
  ;"        SET ENTRY(.09)=SSNUM (OPTIONAL)
  ;"        SET ENTRY(22700)=PtNUM  Medic AccountNumber (OPTIONAL)
  ;"        SET ENTRY(22701)=SequelSystems PMS AccountNumber (OPTIONAL)
  ;"        SET ENTRY(22702)=Paradigm PMS AccountNumber (OPTIONAL)
  ;"Result: RETURNS DFN (patient internal entry number), or 0 if not found
  ;"NOTE: For now, I am ignoring any passed Alias info.
  ;"------------------------------------------------------------------------------
  NEW CONFLICT SET CONFLICT=0
  NEW FULLMATCH SET FULLMATCH=1
  NEW EXTRAINFO SET EXTRAINFO=2
  NEW INSUFFICIENT SET INSUFFICIENT=3
  NEW MISSING SET MISSING=0
  NEW BAILOUT SET BAILOUT=0
  NEW TMGRESULT SET TMGRESULT=0   ;"SET default to no match, or conflict found
  NEW TMGMSG,TMGOUT,RECCOMP
  ;"If can find patient by SSNUM, then don't look any further (IF successful)
  NEW ASSN SET ASSN=$$UP^XLFSTR($GET(ENTRY(.09)))
  IF ASSN'["P",+ASSN>0 SET TMGRESULT=$$SSNLKUP(ASSN)
  IF TMGRESULT>0 GOTO LUDN
  ;
  ;"If can find patient by SequelMedSystem account number, then don't look any further (IF successful)
  IF (+$GET(ENTRY(22701))>0),$$FLDEXISTS(22701) SET TMGRESULT=$$PMSNLKUP(ENTRY(22701))
  IF TMGRESULT>0 GOTO LUDN
  ;
  ;"If can find patient by Paradigm account number, then don't look any further (IF successful)
  IF (+$GET(ENTRY(22702))>0),$$FLDEXISTS(22702) SET TMGRESULT=$$PARDMLKUP(ENTRY(22702))
  IF TMGRESULT>0 GOTO LUDN
  ;
  ;"Below specifies fields to get back.
  NEW VALUE SET VALUE=$GET(ENTRY(.01))
  ;"=========================================================
  ;"FIND^DIC(File,IENStr,Fields,Flags,Value,Number,Indexes,Screen,Ident,OutVarP,ErrVarP)
  NEW FIELDS SET FIELDS="@;.01;.02;.03;.09"
  IF $$FLDEXISTS(22700) SET FIELDS=FIELDS_";22700"
  DO FIND^DIC(2,"",FIELDS,"M",VALUE,"*","","","","TMGOUT","TMGMSG")
  ;"-----------------------------------------------------------
  ;"Here is an example of the output of FIND^DIC():
  ;"TMGOUT("DILIST",0)="2^*^0^" <-2 matches
  ;"TMGOUT("DILIST",0,"MAP")=".01^.02^.03^.09^22700"
  ;"TMGOUT("DILIST",2,1)=16
  ;"TMGOUT("DILIST",2,2)=2914
  ;"TMGOUT("DILIST","ID",1,.01)="VIRIATO,ENEAS"
  ;"TMGOUT("DILIST","ID",1,.02)="MALE"
  ;"TMGOUT("DILIST","ID",1,.03)="01/20/1957"
  ;"TMGOUT("DILIST","ID",1,.09)=123237654
  ;"TMGOUT("DILIST","ID",1,22700)=3542340
  ;"TMGOUT("DILIST","ID",2,.01)="VOID,BURT"
  ;"TMGOUT("DILIST","ID",2,.02)="FEMALE"
  ;"TMGOUT("DILIST","ID",2,.03)=""
  ;"TMGOUT("DILIST","ID",2,.09)=""
  ;"TMGOUT("DILIST","ID",1,22700)=000455454
  ;"-----------------------------------------------
  IF $DATA(TMGMSG("DIERR")) DO SHOWDIER^TMGDEBU2(.TMGMSG)
  ;
  IF $DATA(TMGOUT)'=0 DO
  . NEW NUM,NUMMATCH SET NUMMATCH=+$PIECE(TMGOUT("DILIST",0),"^",1)   ;"Get first part of entry like this: '8^*^0^' <-8 matches
  . FOR NUM=1:1:NUMMATCH DO  ;"Compare all entries found.  If NUMMATCH=0-->no 1st loop
  . . SET RECCOMP=$$COMPARE(.ENTRY,.TMGOUT,NUM)
  . . IF (RECCOMP=INSUFFICIENT)&(NUMMATCH=1) DO
  . . . ;"Fileman has said there is 1 (and only 1) match.
  . . . ;"Even if the supplied info is lacking, it is still a match.
  . . . ;"We still needed to call $$COMPARE to check for EXTRAINFO
  . . . SET RECCOMP=FULLMATCH
  . . IF (RECCOMP=FULLMATCH)!(RECCOMP=EXTRAINFO) DO
  . . . SET TMGRESULT=TMGOUT("DILIST",2,NUM) ;"This is DFN (record) number
  . . . IF RECCOMP=EXTRAINFO DO
  . . . . NEW TEMP SET TEMP=$$ADDTOPAT(TMGRESULT,.ENTRY)
  . . . SET NUM=NUMMATCH+1 ;"some value to abort loop
LUDN ;
  QUIT TMGRESULT  ;" return patient internal entry number (DFN)
  ;
FLDEXISTS(FieldNUM)        ;
  ;"Purpose: to ensure a given field exists in File 2
  ;"Input: FieldNUM: NUMBER of field in file 2
  ;"Output: 1=field exists, 0=doesn't exist
  QUIT ($DATA(^DD(2,FieldNUM,0))'=0)
  ;
EXTRLKUP(ENTRY,INTENSITY) ;"EXTRA LOOKUP
  ;"Purpose: Search for Patient (an existing entry in the database)
  ;"Input: ENTRY -- Array is loaded with info, like this:
  ;"          ENTRY(.01)=Name
  ;"          ENTRY(.02)=Sex
  ;"          ENTRY(.03)=DOB
  ;"          ENTRY(.09)=SSNUM
  ;"          ENTRY(22701)=SequelMedSystem Account Number
  ;"       INTENSITY -- How intense to search.
  ;"              NOTE: Because this returns the FIRST match, is it advised that this function
  ;"                    be run with intensity 1 first, then 2 --> 3 --> 4
  ;"Result: returns FIRST matching DFN (patient internal entry number), or 0 if none found
  ;"NOTE: For now, I am ignoring any passed Alias info.
  ;
  ;"Note: I am assuming that LOOKUPPAT(ENTRY) has been called, and failed.
  ;"      Thus I am not going to compare SSNUMs, Medic or SequelMed's account numbers.
  ;"------------------------------------------------------------------------------
  NEW CONFLICT SET CONFLICT=0
  NEW FULLMATCH SET FULLMATCH=1
  NEW EXTRAINFO SET EXTRAINFO=2
  NEW INSUFFICIENT SET INSUFFICIENT=3
  SET INTENSITY=$GET(INTENSITY,1)
  IF INTENSITY=1 SET THRESHOLD=1   ;"(exact match)
  IF INTENSITY=2 SET THRESHOLD=.75 ;"(probable match)
  IF INTENSITY=3 SET THRESHOLD=.5  ;"(possible match)
  IF INTENSITY=4 SET THRESHOLD=.25 ;"(doubtful match)
  ;
  NEW MISSING SET MISSING=0
  NEW BAILOUT SET BAILOUT=0
  NEW TMGRESULT SET TMGRESULT=0   ;"set default to no match, or conflict found
  NEW TMGMSG,TMGOUT
  NEW RECCOMP
  ;
  ;"If can find patient by SSNUM, then don't look any further (if successful)
  IF +$GET(ENTRY(.09))>0 SET TMGRESULT=$$SSNLKUP(ENTRY(.09))
  IF TMGRESULT>0 GOTO LUDN
  ;
  ;"If can find patient by SequelMedSystem account number, then don't look any further (if successful)
  IF (+$GET(ENTRY(22701))>0),$$FLDEXISTS(22701) SET TMGRESULT=$$PMSNLKUP(ENTRY(22701)) 
  IF TMGRESULT>0 GOTO LUDN
  ;
  ;"If can find patient by Paradigm account number, then don't look any further (if successful)
  IF (+$GET(ENTRY(22702))>0),$$FLDEXISTS(22702) SET TMGRESULT=$$PARDMLKUP(ENTRY(22702))
  IF TMGRESULT>0 GOTO LUDN
  ;
  NEW SEARCHNAME SET SEARCHNAME=$GET(ENTRY(.01))
  IF SEARCHNAME="" GOTO XLUDN
  SET SEARCHNAME=$$FormatName^TMGMISC(SEARCHNAME,1)
  DO STDNAME^XLFNAME(.SEARCHNAME,"C",.TMGMSG) ;"parse into component array
  IF INTENSITY>0 KILL SEARCHNAME("SUFFIX")
  IF INTENSITY>1 KILL SEARCHNAME("MIDDLE")
  IF INTENSITY>2 SET SEARCHNAME("GIVEN")=$EXTRACT(SEARCHNAME("GIVEN"),1,3)
  IF INTENSITY>3 DO
  . SET SEARCHNAME("GIVEN")=$EXTRACT(SEARCHNAME("GIVEN"),1,1)
  . SET SEARCHNAME("FAMILY")=$EXTRACT(SEARCHNAME("FAMILY"),1,3)
  ;
  SET SEARCHNAME=$$BLDNAME^XLFNAME(.SEARCHNAME)
  NEW FIELDS SET FIELDS="@;.01;.02;.03"
  DO FIND^DIC(2,"",FIELDS,"M",SEARCHNAME,"*","","","","TMGOUT","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) GOTO XLUDN
  IF $DATA(TMGOUT)'=0 DO
  . NEW NUMMATCH,NUM
  . SET NUMMATCH=+$GET(TMGOUT("DILIST",0),0)   ;"Get first part of entry like this: '8^*^0^' <-8 matches
  . FOR NUM=1:1:NUMMATCH DO  ;"Compare all entries found.  If NUMMATCH=0-->no 1st loop
  . . NEW DBDATAENTRY
  . . MERGE DBDATAENTRY=TMGOUT("DILIST","ID",NUM)
  . . SET RECCOMP=$$XCOMPENTRY(.ENTRY,.DBDATAENTRY,.THRESHOLD)
  . . IF (RECCOMP=INSUFFICIENT)&(NUMMATCH=1) DO
  . . . ;"Fileman has said there is 1 (and only 1) match.
  . . . ;"Even IF the supplied info is lacking, it is still a match.
  . . . SET RECCOMP=FULLMATCH
  . . IF (RECCOMP=FULLMATCH)!(RECCOMP=EXTRAINFO) DO
  . . . SET TMGRESULT=$GET(TMGOUT("DILIST",2,NUM),0) ;"This is DFN (record) number
  . . . SET NUM=NUMMATCH+1 ;"some value to abort loop
XLUDN ;
  QUIT TMGRESULT  ;" return patient internal entry number (DFN)
  ;
XCOMPENTRY(TESTDATA,DBDATAENTRY,THRESHOLD)        ;
  ;"PURPOSE: To compare two entries for certain fields, and return a comparison code.
  ;"INPUT:  TESTDATA -- array holding uploaded data, that is being tested against preexisting data
  ;"   See CompEntry for Format
  ;"        DBDATAENTRY -- array derived from output from FIND^DIC.    See CompEntry for Format
  ;"        THRESHOLD -- OPTIONAL --How strict to be during the comparison
  ;"              default is 1.
  ;"              e.g. 0.5 --> comparison value must >= 0.5
  ;"              Valid values are: .25, .5, .75, 1
  ;"Results:
  ;"        return value = CONFLICT (0)   IF entries conflict
  ;"        return value = FULLMATCH (1)  IF entries match (to the degreee specified by THRESHOLD)
  ;"        return value = EXTRAINFO (2)  IF entries have no conflict, but tEntry has extra info.
  ;"        return value = INSUFFICIENT (3) Insufficient data to make match, but no conflict.
  ;"Note: This function IS DIFFERENT then CompEntry (which this was originally copied from)
  ;"      --It's purpose is to look for matches after a partial fileman search,
  ;"              Smi,Jo for Smith,John
  NEW CONFLICT SET CONFLICT=0
  NEW FULLMATCH SET FULLMATCH=1
  SET THRESHOLD=$GET(THRESHOLD,1)
  NEW INSUFFICIENT SET INSUFFICIENT=3
  ;
  NEW TD,DBD
  ;"NEW CRESULT SET CRESULT=FULLMATCH ;"SET default to match
  NEW TMGRESULT SET TMGRESULT=FULLMATCH  ;"default is Success.
  NEW WORSTSCORE SET WORSTSCORE=1
  NEW EXTRA SET EXTRA=0 ;"0=false
  ;
  IF $DATA(TESTDATA(.01))#10'=0 DO
  . SET TD=$GET(TESTDATA(.01))      ;"field .01 = NAME
  . SET DBD=$GET(DBDATAENTRY(.01))
  . SET TMGRESULT=$$COMPNAME^TMGMISC(TD,DBD)
  IF TMGRESULT=CONFLICT GOTO CMPEDN
  IF TMGRESULT<WORSTSCORE SET WORSTSCORE=TMGRESULT
  ;
  IF $DATA(TESTDATA(.02))#10'=0 DO
  . SET TD=$GET(TESTDATA(.02))      ;"field .02 = SEX
  . SET DBD=$GET(DBDATAENTRY(.02))
  . SET TMGRESULT=$$FLDCOMP^TMGDBAPI(TD,DBD,"SEX")
  IF TMGRESULT=CONFLICT GOTO XCMPDN
  IF TMGRESULT=EXTRAINFO SET EXTRA=1
  ;
  IF $DATA(TESTDATA(.03))#10'=0 DO
  . SET TD=$GET(TESTDATA(.03))      ;"field .03 = DOB
  . SET DBD=$GET(DBDATAENTRY(.03))
  . SET TMGRESULT=$$COMPDOB^TMGMISC(TD,DBD)
  IF TMGRESULT=CONFLICT GOTO XCMPDN
  IF TMGRESULT<WORSTSCORE SET WORSTSCORE=TMGRESULT
  ;
  ;"If we are here, then there is no conflict.
  IF TMGRESULT>WORSTSCORE SET TMGRESULT=WORSTSCORE
  SET TMGRESULT=(TMGRESULT'<THRESHOLD)
  IF TMGRESULT=CONFLICT GOTO XCMPDN
  ;
  ;"If extra info present, reflect this in TMGRESULT
  IF EXTRA=1 SET TMGRESULT=EXTRAINFO
  ;
  ;"OK, no conflict.  But is there sufficient data for a match?
  ;"ensure we check at least Name & DOB-->success
  IF ($DATA(TESTDATA(.01))#10=0)&($DATA(TESTDATA(.03))=0) SET TMGRESULT=INSUFFICIENT
  ;
XCMPDN  ;
  QUIT TMGRESULT
  ;
SSNLKUP(SSNUM)        ;
  ;"PURPOSE: To lookup patient by social security number
  ;"Result: RETURNS DFN (patient internal entry number), or 0 IF not found
  ;"NOTE: 7/21/12 -- There is no XRef on SSN, so can't DO a lookup
  ;"    on full SSN.  There is a "BS" xref on the last
  ;"    4 digits. Will this to try to find patient.
  ;"
  NEW TMGRESULT SET TMGRESULT=0
  NEW LAST4 SET LAST4=$EXTRACT(SSNUM,6,9)
  NEW IEN SET IEN=0
  FOR  SET IEN=$ORDER(^DPT("BS",LAST4,IEN)) QUIT:(+IEN'>0)!(TMGRESULT>0)  DO
  . NEW ONESSN SET ONESSN=$PIECE($GET(^DPT(IEN,0)),"^",9)
  . IF ONESSN=SSNUM SET TMGRESULT=IEN
  QUIT TMGRESULT
  ;
PMSNLKUP(PMSNUM)        ;"PMSNumLookup
  ;"PURPOSE: To lookup patient by SequelSystem account number
  ;"Result: RETURNS DFN (patient internal entry number), or 0 IF not found
  ;"
  NEW TMGRESULT SET TMGRESULT=0   ;"SET default to no match, or conflict found
  NEW TMGMSG,TMGOUT
  ;"Uses custom TMGS index.
  DO FIND^DIC(2,"",".01","",PMSNUM,"*","TMGS","","","TMGOUT","TMGMSG")
  IF '$DATA(TMGMSG("DIERR")) SET TMGRESULT=$GET(TMGOUT("DILIST",2,1),0)
  QUIT TMGRESULT  ;" return patient internal entry number (DFN)
  ;
PARDMLKUP(PMSNUM)        ;"ParadigmNumLookup
  ;"PURPOSE: To lookup patient by Paradigm account number
  ;"Result: RETURNS DFN (patient internal entry number), or 0 IF not found
  NEW TMGRESULT SET TMGRESULT=0   ;"SET default to no match, or conflict found
  NEW TMGMSG,TMGOUT
  ;"Uses custom TMGP index.
  DO FIND^DIC(2,"",".01","",PMSNUM,"*","TMGP","","","TMGOUT","TMGMSG")
  IF '$DATA(TMGMSG("DIERR")) SET TMGRESULT=$GET(TMGOUT("DILIST",2,1),0)
  QUIT TMGRESULT  ;" return patient internal entry number (DFN)
  ;
LMH2DFN(LMHMRN)        ;
  ;"Purpose -- to return patient by Laughlin Hospital MRN
  ;"Note: MRN will be stored in Health Record multiple in the IHS patient file.
  ;"Note: this was originally planned for HL7 message processing.  I then found
  ;"      that the HL7 message included the patient SSN, and this was not
  ;"      really needed.  For now, will just return 0
  ;"      Currently is called from PID^LA7VIN2, It handles 0 reply OK.
  QUIT 0 ;"<-- change later IF implemented.
  ;
COMPARE(TESTDATA,DBDATA,EntryNUM)        ;
  ;"PURPOSE: To compare two entries for certain fields, and return a comparison code.
  ;"INPUT:  TESTDATA -- array holding uploaded data, that is being tested against preexisting data
  ;"   Format is:
  ;"   TESTDATA(FieldNumber)=Value
  ;"   TESTDATA(FieldNumber)=Value
  ;"   TESTDATA(FieldNumber)=Value
  ;"        DBDATA -- array returned from FIND^DIC.
  ;"        EntryNUM -- Entry number in DBDATA
  ;"Results:
  ;"        return value = CONFLICT (0)   IF entries conflict
  ;"        return value = FULLMATCH (1)  IF entries completely match
  ;"        return value = EXTRAINFO (2)  IF entries have no conflict, but tEntry has extra info.
  ;"        return value = INSUFFICIENT (3) Insufficient data to make match, but no conflict.
  ;"Note: The following data sets will be sufficient for a match:
  ;"        1. SSNumber (not a P/pseudo value)
  ;"        2. Patient Identifier (field 22700)
  ;"        3. Name, DOB
  ;
  NEW CONFLICT SET CONFLICT=0
  NEW FULLMATCH SET FULLMATCH=1
  NEW EXTRAINFO SET EXTRAINFO=2
  NEW INSUFFICIENT SET INSUFFICIENT=3
  ;
  NEW DBDATAENTRY,TMGRESULT
  ;
  ;"First, ensure no conflict between TESTDATA and DBDATA
  MERGE DBDATAENTRY=DBDATA("DILIST","ID",EntryNUM)
  SET TMGRESULT=$$COMPENTRY(.TESTDATA,.DBDATAENTRY)
  IF TMGRESULT=CONFLICT GOTO COMPDN
  ;
  IF $GET(TESTDATA(.01))="" KILL TESTDATA(.01)
  IF $GET(TESTDATA(.03))="" KILL TESTDATA(.03)
  IF $GET(TESTDATA(.09))="" KILL TESTDATA(.09)
  IF $GET(TESTDATA(22700))="" KILL TESTDATA(22700)
  IF $GET(TESTDATA(22701))="" KILL TESTDATA(22701)
  ;
  ;"OK, no conflict.  But is there sufficient data for a match?
  IF (+$GET(TESTDATA(.09))>0)&($GET(TESTDATA(.09))'["P") GOTO COMPDN ;".09=SSNUM --> success
  IF ($DATA(TESTDATA(22700))#10'=0) GOTO COMPDN  ;"22700=Pt. Identifier --> success
  IF ($DATA(TESTDATA(.01))#10'=0)&($DATA(TESTDATA(.03))) GOTO COMPDN ;"Name & DOB-->success
  ;
  ;"If here, then we don't have enough data for a match
  SET TMGRESULT=INSUFFICIENT
COMPDN ;
  QUIT TMGRESULT
  ;
COMPENTRY(TESTDATA,DBDATAENTRY)        ;
  ;"PURPOSE: To compare two entries for certain fields, and return a comparison code.
  ;"INPUT:  TESTDATA -- array holding uploaded data, that is being tested against preexisting data
  ;"   Format is:
  ;"   TESTDATA(FieldNumber)=Value
  ;"   TESTDATA(FieldNumber)=Value
  ;"   TESTDATA(FieldNumber)=Value
  ;"        DBDATAENTRY -- array derived from output from FIND^DIC.
  ;"   Format is:
  ;"   DBDATAENTRY(FieldNumber)=Value
  ;"   DBDATAENTRY(FieldNumber)=Value
  ;"   DBDATAENTRY(FieldNumber)=Value
  ;"          EntryNum -- Entry number in DBDATAENTRY
  ;"Results:
  ;"        return value = CONFLICT (0)   IF entries conflict
  ;"        return value = FULLMATCH (1)  IF entries completely match
  ;"        return value = EXTRAINFO (2)  IF entries have no conflict, but tEntry has extra info.
  NEW CONFLICT SET CONFLICT=0
  NEW FULLMATCH SET FULLMATCH=1
  NEW EXTRAINFO SET EXTRAINFO=2
  ;
  NEW TD,DBD
  ;"NEW CRESULT SET CRESULT=FULLMATCH ;"SET default to match (so data won't be entered into database)
  NEW TMGRESULT SET TMGRESULT=FULLMATCH  ;"default is Success.
  NEW EXTRA SET EXTRA=0 ;"0=false
  ;
  ;"I am not going to test field .01 (NAME) because Fileman has already done this, and
  ;"  feels that the names it has returned are compatible.
  ;"  I was having a inappropriate with input like this:
  ;"     TESTDATA(.01)="DOE,JOHN"
  ;"     DBDATAENTRY(.01)="DOE,JOHN J"
  ;"  Also, patient might have an ALIAS, and that wouldn't name match either...
  ;
  IF $DATA(TESTDATA(.09))#10'=0 DO
  . SET TD=$GET(TESTDATA(.09))      ;"field .09 = SSNUM
  . SET DBD=$GET(DBDATAENTRY(.09))
  . SET TMGRESULT=$$FLDCOMP^TMGDBAPI(TD,DBD,"SSNUM")
  IF TMGRESULT=CONFLICT GOTO CMPEDN
  IF TMGRESULT=EXTRAINFO SET EXTRA=1
  ;
  IF $DATA(TESTDATA(.02))#10'=0 DO
  . SET TD=$GET(TESTDATA(.02))      ;"field .02 = SEX
  . SET DBD=$GET(DBDATAENTRY(.02))
  . SET TMGRESULT=$$FLDCOMP^TMGDBAPI(TD,DBD,"SEX")
  IF TMGRESULT=CONFLICT GOTO CMPEDN
  IF TMGRESULT=EXTRAINFO SET EXTRA=1
  ;
  IF $DATA(TESTDATA(.03))#10'=0 DO
  . SET TD=$GET(TESTDATA(.03))      ;"field .03 = DOB
  . SET DBD=$GET(DBDATAENTRY(.03))
  . SET TMGRESULT=$$FLDCOMP^TMGDBAPI(TD,DBD,"DATE")
  IF TMGRESULT=CONFLICT GOTO CMPEDN
  IF TMGRESULT=EXTRAINFO SET EXTRA=1
  ;
  ;"If we are here, then there is no conflict.
  SET TMGRESULT=FULLMATCH
  ;"If extra info present, reflect this in TMGRESULT
  IF EXTRA=1 SET TMGRESULT=EXTRAINFO
  ;
CMPEDN  ;
  QUIT TMGRESULT
  ;
ADDTOPAT(ADFN,ENTRY)        ;
  ;"PURPOSE: Stuffs ENTRY into record number ADFN (RecNum must already exist)
  ;"INPUT:   ADFN -- the record number, in file 2, that is to be updated
  ;"         ENTRY -- PASS BY REFERENCE.  Format: ENTRY(FldNum)=Value
  ;"The following FieldNumbers will be used if avail: .01,.02,.03,.09,22700
  ;"Results: 1 = OK, or 0 for abort
  NEW TMGFDA,TMGRESULT SET TMGRESULT=1  
  NEW FLD FOR FLD=.01,.02,.03,22700 DO    ;".01=NAME,.02=SEX,.03=DOB
  . IF FLD=.09,$GET(ENTRY(.09))["P" QUIT
  . IF $GET(ENTRY(FLD))'="" SET TMGFDA(2,ADFN_",",FLD)=ENTRY(FLD)
  SET TMGRESULT=$$dbWrite^TMGDBAPI(.TMGFDA,1)
ATRDN  ;
  QUIT TMGRESULT
  ;
ADDNEWPAT(ENTRY)        ;
  ;"Purpose: Create a NEW entry in file 2 (Patient File)
  ;"Input: 
  ;" ENTRY. PASS BY REFERENCE.  Format:  
  ;"   ENTRY(.01)=Patient Name
  ;"   ENTRY(.03)=DOB
  ;"   ENTRY(.09)=SSNUM
  ;"   ENTRY(22700)=Medic Pt Identifier -- optional
  ;"   ENTRY(1901)=field 1901 = VETERAN Y/N --For my purposes, use NO -- optional
  ;"   ENTRY(.301)=field .301 = "SERVICE CONNECTED?" -- required field -- optional
  ;"   ENTRY(391)=field 391 = "TYPE" - required field -- optional
  ;"Output: Returns internal entry number (DFN) IF successful, otherwise 0
  ;"Note: The following data sets must be available for a patient to be entered:
  ;"        Patient name (.01) -- always required
  ;"        Patient sex (.02) -- always required
  ;"        And ONE of the following...
  ;"        1. SSNumber (.09) (not a P/pseudo value)
  ;"        2. Patient Identifier (field 22700)
  ;"        3. DOB (.03)
  ;"Results: DFN, or -1^Message if error
  ;"--------------------------------------------
  ;"NOTE: 1/29/11 -- Changing this routine to use that created by Sam Habiel.
  ;"        His routine correct registers the patient in the IHS/PATIEN file,
  ;"        under each avail institution, and also fires and HL-7 message of
  ;"        the addition.
  NEW TMGZZ SET TMGZZ=0
  IF TMGZZ=1 DO
  . KILL ENTRY
  . MERGE ENTRY=^TMG("TMP","PRC","ADDNEWPAT^TMGGDFN","ENTRY")
  ELSE  DO
  . KILL ^TMG("TMP","RPC","ADDNEWPAT^TMGGDFN","ENTRY")
  . MERGE ^TMG("TMP","RPC","ADDNEWPAT^TMGGDFN","ENTRY")=ENTRY
  NEW TMGRESULT
  DO REGP01^UJOPTREG(.TMGRESULT,.ENTRY)
  NEW R0 SET R0=$GET(TMGRESULT(0))  ;"1 for success, 0 for error
  NEW R1 SET R1=$GET(TMGRESULT(1))  ;"DFN, or ERRCODE^MESSAGE
  IF R0=1 DO
  . SET TMGRESULT=+R1
  . IF +TMGRESULT'>0 SET TMGRESULT="-1^Error "_$PIECE(R1,"^",2)
  ELSE  DO
  . SET TMGRESULT="-1^"_$PIECE(R1,"^",2)
  QUIT TMGRESULT
  ;
