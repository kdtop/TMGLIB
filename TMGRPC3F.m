TMGRPC3F ;TMG/kst/Support Functions for GUI_Config ;08/31/08, 2/2/14
         ;;1.02;TMG-LIB;**1**;11/18/08
 ;
 ;"TMG RPC FUNCTIONS for a GUI config program
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
 ;" RPC -- Public Functions.
 ;"=======================================================================
 ;" <none>
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"REGPAT(TMGOUT,TMGPARAMS) -- register a NEW patient into VistA, providing not already been registered.
 ;"SRCHPTEXACT(TMGARRAY) -- search for a preexisting patient, using an exact search
 ;
 ;"=======================================================================
 ;"Dependencies:
 ;"  TMGRPC3* only
 ;
 ;"=======================================================================
 ;
REGPAT(TMGOUT,TMGPARAMS) ;"REGISTER PATIENT
        ;"Purpose: to register a NEW patient into VistA, providing that they have not
        ;"         already been registered.
        ;"Note: The search for preexisting records is exact, meaning that DOE,JOHN
        ;"      would be considered different from DOE,JOHN H
        ;"Input: TMGOUT -- an OUT PARAMETER, PASS BY REFERENCE.
        ;"       TMGPARAMS -- FieldNum1^FieldValue1^FieldNum2^FieldValue2^FieldNum3^FieldValue3^...
        ;"       NOTE: Because I want to be able to specify here the HealthRecordNumber (HRN),
        ;"             even though it is not truly a field in the PATIENT file, I am going
        ;"             to manually allow a field of '0' (HRN) to be specified as a FieldNumber
        ;"Output: TMGOUT is filled as follows:
        ;"          TMGOUT(0)="1^Success^NewIEN" or
        ;"                    "0^BoolAndMessage^NewIEN".  Format:
        ;"                       [Bool1;Bool2;Bool3;Bool4;Bool5*MessageText]  (e.g. '1;0;1;0;0*ErrorMsg')
        ;"                         (for Bool fields, 0=no or 1=yes)
        ;"                         Bool1 -- patient had previously been registered
        ;"                         Bool2 -- patient registered during this Fn
        ;"                         Bool3 -- problem filing data into non-identifier fields
        ;"                         Bool4 -- problem filing data into sub-file fields
        ;"                         Bool5 -- problem with filing HRN
        ;"                    "-1^Message"    Frank failure...
        ;"          TMGOUT(1)=Long Fileman message (if -1 error, or perhaps 0 code)
 ;
        NEW TMGRSLT,TMGRCOD1,TMGRCOD2,TMGRCOD3,TMGRCOD4,TMGRCOD5,TMGRMSG
        SET TMGRSLT=1,(TMGCOD1,TMGCOD2,TMGCOD3,TMGCOD4,TMGCOD5)=0,TMGMSG=""
        NEW TMGARRAY
        NEW TMGFDA,TMGMSG
        FOR  DO  QUIT:($LENGTH(TMGPARAMS,"^"))<2
        . NEW TMGPAIR,TMGFIELD,TMGVALUE
        . SET TMGPAIR=$PIECE(TMGPARAMS,"^",1,2)
        . SET TMGFIELD=$PIECE(TMGPAIR,"^",1)
        . SET TMGVALUE=$PIECE(TMGPAIR,"^",2)
        . SET TMGPARAMS=$EXTRACT(TMGPARAMS,$LENGTH(TMGPAIR)+2,999)
        . IF (TMGFIELD="")!(TMGVALUE="") QUIT
        . SET TMGARRAY(TMGFIELD)=TMGVALUE
 ;
        ;"Prepair list of required identifiers (needed to create record)
        NEW TMGRECID,TMGFLD
        SET TMGFLD=0
        FOR  SET TMGFLD=$O(^DD(2,TMGFLD)) Q:'TMGFLD  DO
        . NEW NODE,REQUIRED,ID
        . SET NODE=$GET(^(TMGFLD,0))
        . IF NODE'="" DO
        . . SET NAME=$P(NODE,"^")
        . . SET REQUIRED=$P(NODE,"^",2)["R"
        . . SET ID=$DATA(^DD(2,0,"ID",TMGFLD))
        . . IF REQUIRED&ID SET TMGRECID(TMGFLD)=1
 ;
        NEW TMGIEN SET TMGIEN=$$SRCHPTEXACT(.TMGARRAY)
        IF +TMGIEN>0 DO  GOTO RP2  ;"Continue to possibly update data in other fields
        . SET TMGRSL=0,TMGCOD1=1
        . ;"SET TMGOUT(0)="-1^Patient already registered^"_TMGIEN
 ;
        ;"Load TMGFDA with elements of basic record first, then add other
        ;"fields on subsequent post (I think I have had problems trying to
        ;"load some fields when the record has not already been created.)
        KILL TMGIEN
        SET TMGFDA(2,"+1,",.01)=TMGARRAY(.01)
        KILL TMGARRAY(.01)
        SET TMGFLD=""
        FOR  SET TMGFLD=$ORDER(TMGRECID(TMGFLD)) Q:TMGFLD=""  DO
        . IF $DATA(TMGARRAY(TMGFLD))=0 QUIT  ;"Required identifier is missing, expect Filman error below
        . NEW TMGVALUE SET TMGVALUE=$GET(TMGARRAY(TMGFLD))
        . KILL TMGARRAY(TMGFLD)
        . IF TMGVALUE="" QUIT
        . SET TMGFDA(2,"+1,",TMGFLD)=TMGVALUE
        ;"Create NEW record
        DO UPDATE^DIE("SE","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO RPTDONE
        . SET TMGOUT(0)="-1^See Fileman message"
        . SET TMGOUT(1)=$$GETERSTR^TMGRPC3G(.TMGMSG)
        . DO ADDFDA^TMGRPC3G(.TMGFDA,.TMGOUT,2)
        SET TMGCOD2=1  ;"patient registered OK
        ;"Get IEN of NEW record
        SET TMGIEN=$GET(TMGIEN(1))
        IF TMGIEN'>0 DO  GOTO RPTDONE
        . SET TMGRSLT=0
        . SET TMGOUT(0)=TMGRSLT_"^"_TMGCOD1_";"_TMGCOD2_";"_TMGCOD3_";"_TMGCOD4_";"_TMGCOD5_"*Unable to find IEN of added record"
 ;
RP2     ;"Now add the other fields not put in on first pass.
        NEW TMGFIELD SET TMGFIELD=""
        KILL TMGFDA,TMGMSG,TMGMVA
        FOR  SET TMGFIELD=$ORDER(TMGARRAY(TMGFIELD)) QUIT:(TMGFIELD="")  DO
        . IF TMGFIELD="0" QUIT   ;" pseudoField '0' from client will hold HRN
        . NEW TMGA
        . DO FIELD^DID(2,TMGFIELD,,"MULTIPLE-VALUED","TMGA","TMGMSG")
        . IF TMGA("MULTIPLE-VALUED")=1 SET TMGMVA(TMGFIELD)=1 QUIT  ;"Process separately later
        . NEW TMGVALUE SET TMGVALUE=$GET(TMGARRAY(TMGFIELD))
        . IF TMGVALUE="" QUIT
        . SET TMGFDA(2,TMGIEN_",",TMGFIELD)=TMGVALUE
 ;
        ;"File additional info in additional fields (Not multi-valued fields)
        IF $DATA(TMGFDA) DO
        . DO FILE^DIE("SE","TMGFDA","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  ;"GOTO RPTDONE  --Keep going, try to file more...
        . SET TMGRSLT=0,TMGCOD3=1 ;"Error adding fields
        . SET TMGOUT(0)=TMGRSLT_"^"_TMGCOD1_";"_TMGCOD2_";"_TMGCOD3_";"_TMGCOD4_";"_TMGCOD5_"*See Fileman message^"_TMGIEN
        . IF $GET(TMGOUT(1))'="" SET TMGOUT(1)=TMGOUT(1)_"// "
        . SET TMGOUT(1)=$GET(TMGOUT(1))_$$GETERSTR^TMGRPC3G(.TMGMSG)
        . DO ADDFDA^TMGRPC3G(.TMGFDA,.TMGOUT,2)
 ;
        ;"Now add multi-valued fields into subfiles.
        ;"It is assumed that the value supplied will go into the .01 field in the subfile.
        SET TMGFIELD=""
        KILL TMGFDA,TMGMSG
        NEW TMGABORT SET TMGABORT=0
        FOR  SET TMGFIELD=$ORDER(TMGMVA(TMGFIELD)) QUIT:(TMGFIELD="")!TMGABORT  DO
        . NEW TMGTEMP,TMGFN SET TMGFN=+$PIECE($GET(^DD(2,TMGFIELD,0)),"^",2)
        . IF TMGFN'>0 QUIT
        . NEW TMGVALUE SET TMGVALUE=$GET(TMGARRAY(TMGFIELD))
        . IF TMGVALUE="" QUIT
        . SET TMGFDA(TMGFN,"?+1,"_TMGIEN_",",.01)=TMGVALUE
        . DO UPDATE^DIE("E","TMGFDA","TMGTEMP","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) DO
        . . SET TMGRSLT=0,TMGCOD4=1 ;"Problem adding to subfiles
        . . SET TMGOUT(0)=TMGRSLT_"^"_TMGCOD1_";"_TMGCOD2_";"_TMGCOD3_";"_TMGCOD4_";"_TMGCOD5_"*See Fileman message^"_TMGIEN
        . . IF $GET(TMGOUT(1))'="" SET TMGOUT(1)=TMGOUT(1)_"// "
        . . SET TMGOUT(1)=$GET(TMGOUT(1))_$$GETERSTR^TMGRPC3G(.TMGMSG)
        . . DO ADDFDA^TMGRPC3G(.TMGFDA,.TMGOUT,2)
        . . ;"SET TMGABORT=1  --keep going...
        IF TMGABORT GOTO RPTDONE
 ;
        IF $GET(TMGOUT(0))="" SET TMGOUT(0)="1^Success^"_TMGIEN
 ;
        NEW TMGHRN SET TMGHRN=$GET(TMGARRAY("0"))=""   ;" pseudoField '0' from client will hold HRN
        IF TMGHRN="" GOTO RPTDONE
        ;"Set TMGHRN field in file 9000001 (^AUPNPAT), linked to Patient entry
 ;
        NEW TMGLOCIEN,TMGINSTIEN
        ;"Get DEFAULT INSTITUTION from KERNEL SYSTEM PARAMETERS.
        SET TMGINSTIEN=$PIECE($GET(^XTV(8989.3,1,"XUS")),"^",17) ;" XUS;17 = DEFAULT INSTITUTION
        IF +TMGINSTIEN'>0 DO  GOTO RPTDONE
        . ;"SET TMGOUT(0)="1^Success (but see message)^"_TMGIEN
        . SET TMGOUT(0)="0^"_TMGCOD1_";"_TMGCOD2_";"_TMGCOD3_";"_TMGCOD4_";"_TMGCOD5_"*See message^"_TMGIEN
        . SET TMGOUT(1)="Unable to add HRN because couldn't find value for DEFAULT INSTITUTION in KERNEL SYSTEM PARAMETERS file."
 ;
        ;"Get LOCATION file entry pointing to this INSTITUTION
        SET TMGLOCIEN=$ORDER(^AUTTLOC("B",TMGINSTIEN,""))
        IF +TMGLOCIEN'>0 DO  GOTO RPTDONE
        . SET TMGOUT(0)="0^"_TMGCOD1_";"_TMGCOD2_";"_TMGCOD3_";"_TMGCOD4_";"_TMGCOD5_"*See message^"_TMGIEN
        . ;"SET TMGOUT(0)="1^Success (but see message)^"_TMGIEN
        . SET TMGOUT(1)="Unable to add HRN because couldn't find `"_TMGINSTIEN_" in LOCATION file"
 ;
        NEW TMGHRNIEN SET TMGHRNIEN=+$ORDER(^AUNPNPAT("B",TMGIEN,""))
        IF TMGHRNIEN'>0 DO  GOTO RPTDONE
        . SET TMGOUT(0)="0^"_TMGCOD1_";"_TMGCOD2_";"_TMGCOD3_";"_TMGCOD4_";"_TMGCOD5_"*See message^"_TMGIEN
        . ;"SET TMGOUT(0)="1^Success (but see message)^"_TMGIEN
        . SET TMGOUT(1)="Unable to add HRN because couldn't find record in PATIENT/IHS file"
 ;
        KILL TMGFDA,TMGMSG
        ;"Now see IF there already is an entry for TMGLOCIEN  (DINUM in play, so subFile record#=TMGLOCIEN)
        IF $GET(^AUPNPAT(TMGHRNIEN,41,TMGLOCIEN,0))="" DO
        . ;"Put HRN into value for current subfile entry
        . SET TMGFDA(9000001.41,TMGLOCIEN_","_TMGHRNIEN_",",.02)=TMGHRN
        . DO FILE^DIE("S","TMGFDA","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) DO
        . . SET TMGOUT(0)="0^"_TMGCOD1_";"_TMGCOD2_";"_TMGCOD3_";"_TMGCOD4_";"_TMGCOD5_"*See message^"_TMGIEN
        . . ;"SET TMGOUT(0)="1^Success (but see message)^"_TMGIEN
        . . IF $GET(TMGOUT(1))'="" SET TMGOUT(1)=TMGOUT(1)_"// "
        . . SET TMGOUT(1)=$GET(TMGOUT(1))_$$GETERSTR^TMGRPC3G(.TMGMSG)
        . . DO ADDFDA^TMGRPC3G(.TMGFDA,.TMGOUT,2)
        ELSE  DO
        . ;"Add NEW subfile entry for HRN
        . NEW TMGSIEN
        . SET TMGFDA(9000001.41,TMGLOCIEN_","_TMGHRNIEN_",",.01)=TMGLOCIEN
        . SET TMGFDA(9000001.41,TMGLOCIEN_","_TMGHRNIEN_",",.02)=TMGHRN
        . DO UPDATE^DIE("S","TMGFDA","TMGSIEN","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) DO
        . . SET TMGOUT(0)="0^"_TMGCOD1_";"_TMGCOD2_";"_TMGCOD3_";"_TMGCOD4_";"_TMGCOD5_"*See message^"_TMGIEN
        . . ;"SET TMGOUT(0)="1^Success (but see message)^"_TMGIEN
        . . IF $GET(TMGOUT(1))'="" SET TMGOUT(1)=TMGOUT(1)_"// "
        . . SET TMGOUT(1)=$GET(TMGOUT(1))_$$GETERSTR^TMGRPC3G(.TMGMSG)
        . . DO ADDFDA^TMGRPC3G(.TMGFDA,.TMGOUT,2)
 ;
RPTDONE ;
        QUIT
 ;
SRCHPTEXACT(TMGARRAY) ;
        ;"Purpose: to search for a preexisting patient, using an exact search
        ;"      By exact search, I mean that DOE,JOHN would be considered different
        ;"      from DOE,JOHN H because it is a different ascii string.  Etc.
        ;"      However, see search description below.
        ;"Input: TMGARRAY -- PASS BY REFERENCE. Search info.  Format:
        ;"              TMGARRAY(.01)=PatientName, e.g. DOE,JOHN
        ;"              TMGARRAY(.02)=Sex          e.g. M
        ;"              TMGARRAY(.03)=DOB          e.g. 01-04-69 (an external date format)
        ;"              TMGARRAY(.09)=SSNum        e.g. 123-45-6789
        ;"Result: returns DFN (patient IEN), or 0^Message IF not found.
        ;"Notes:
        ;"    The following are sufficient for search:
        ;"    -- SSNum only
        ;"    -- or Name-Sex-DOB
        ;"
        ;"    Search technique:
        ;"    1. Search for SSN, IF found then no further comparison made.
        ;"    2. Search for exact name match, no none found, then exit with 0
        ;"    3. ...
 ;
        NEW TMGRESULT SET TMGRESULT=0
 ;
        ;"Search by SSN.  Quit IF match found
        NEW TMGSSN SET TMGSSN=$TRANSLATE($GET(TMGARRAY(.09)),"-","")
        IF TMGSSN'="" SET TMGRESULT=+$ORDER(^DPT("SSN",TMGSSN,""))
        IF TMGRESULT>0 GOTO PSEDONE
 ;
        NEW TMGNAME,TMGSEX,TMGDOB
        SET TMGNAME=$GET(TMGARRAY(.01))
        IF TMGNAME="" DO  GOTO PSEDONE
        . SET TMGRESULT="0^No Name (.01 field) provided"
 ;
        SET TMGSEX=$GET(TMGARRAY(.02))
        IF TMGSEX="" DO  GOTO PSEDONE
        . SET TMGRESULT="0^Sex (.02 field) not specified"
        IF (TMGSEX'="M")&(TMGSEX'="F") DO  GOTO PSEDONE
        . SET TMGRESULT="0^Sex should be 'Y' or 'N'.  Value provided="_TMGSEX
 ;
        SET TMGDOB=$GET(TMGARRAY(.03))
        IF TMGDOB="" DO  GOTO PSEDONE
        . SET TMGRESULT="0^No DOB (.03 field) provided"
        NEW %DT,X,Y SET %DT="P"  ;"P-Post dates assumed
        SET X=TMGDOB DO ^%DT SET TMGDOB=Y  ;"convert external date into internal format
        IF +TMGDOB'>0 DO  GOTO PSEDONE
        . SET TMGRESULT="0^Invalid date: "_$GET(TMGARRAY(.03))
 ;
        NEW TMGMATCHES MERGE TMGMATCHES=^DPT("B",TMGNAME)
        IF $DATA(TMGMATCHES)=0 DO  GOTO PSEDONE
        . SET TMGRESULT="0^No match for name"
 ;
        ;"Now compare each name match for also matching sex and DOB
        NEW TMGIEN SET TMGIEN=""
        FOR  SET TMGIEN=$ORDER(TMGMATCHES(TMGIEN)) QUIT:(+TMGIEN'>0)  DO
        . NEW TMGNODE0 SET TMGNODE0=$GET(^DPT(TMGIEN,0))
        . IF $PIECE(TMGNODE0,"^",2)'=TMGSEX DO  QUIT
        . . KILL TMGMATCHES(TMGIEN)
        . IF $PIECE(TMGNODE0,"^",3)'=TMGDOB DO  QUIT
        . . KILL TMGMATCHES(TMGIEN)
 ;
        ;"TMGMATCHES should contain all entries matching name+sex+DOB
        SET TMGRESULT=+$ORDER(TMGMATCHES(""))
        ;"Now check for more than one match
        IF $ORDER(TMGMATCHES(TMGRESULT))'="" DO  GOTO PSEDONE
        . SET TMGRESULT="0^More than one match found for Name+Sex+DOB"
 ;
PSEDONE ;
        QUIT TMGRESULT
 ;
 ;
