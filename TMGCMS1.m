TMGCMS1 ;TMG/KST - Parse and import HCC codes from CMS file; 4/9/24
        ;;1.0;TMG-LIB;**1**; 4/9/24
       ;
 ;"TMG SEQUEL PMS FUNCTIONS -- Importing appointments. 
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 4/9/24  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"IMPORTHCC(WANTEDVER) -- Import data from CSV file into 22759 (TMG HCC CODES)
 ; 
 ;"=======================================================================
 ;" Private Functions.
 ;"=======================================================================
 ;"ENSUREVER(VER) -- Ensure VERSION record created.
 ;"ENSUREHCC(IEN22759,HCC,DATA) -- Ensure record created for HCC value
 ;"ENSUREICDS(IEN22759,IEN22759D01,ARR) -- ensure ICD code record created
 ;"GETPARSEDHCC(PARSEDARR,WANTEDVER)  --Load CSV file and parse to usable array
 ;"GETHCCNAMES(ARR) -- DEPRECATED
 ;"GETHCCNAMES2(ARR) -- load HCC names into array  
 ;"SETXREF(IEN80,IEN22759,IEN22759D01,IEN22759D11) -- XRef callback for SET
 ;"KILXREF(IEN80,IEN22759,IEN22759D01,IEN22759D11) -- XRef callback for KILL
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;" 
 ;"=======================================================================
 ;
 ;"Parsing into into FILEMAN file #22759 / TMG HCC CODES  <-- STORED IN ^TMG(22759,
 ;"Which has this structure.  
 ;" File: TMG HCC CODES                                                  Branch: 1
 ;" REF  NODE;PIECE     FLD NUM  FIELD NAME
 ;"===============================================================================
 ;"  1  0;1                .01  VERSION                                   [RFJ30]
 ;"  2  0;2                .02  VERSION DESCRIPTION                       [FJ200]
 ;"     1;0                  1  HCC CODE                        <-Mult [22759.01]
 ;"  3   -0;1              .01   -HCC CODE                                [NJ5,0]
 ;"  4   -0;2              .02   -HCC CODE DESCRIPTION                    [FJ200]
 ;"      -1;0                1   -ICD                          <-Mult [22759.11P]
 ;"  5    --0;1            .01    --ICD                            <-Pntr  [P80']
 ;"  6    --0;2            .02    --RAF                                   [NJ6,2]
 ;
 ;"SOURCE FILE:
 ;"NOTE: At time of creation, working off this file:
 ;"   https://www.cms.gov/files/zip/2024-midyear/final-icd-10-mappings.zip
 ;"     This zip file contains: '2024 Midyear_Final ICD-10-CM Mappings.csv'
 ;"   Zip file obtained from this site:
 ;"     https://www.cms.gov/medicare/health-plans/medicareadvtgspecratestats/risk-adjustors/2024-model-software/icd-10-mappings
 ;"
 ;"NOTE: This file links ICD10 codes to a numerical value HCC.  But it is not clear what the name of the HCC category is
 ;"      I have tried to find a mapping between HCC number and HCC description, but I find conflicting values (see HCCNAMES vs HCCNAMES2 below)
 ;"      So for now, I will not provide a description.  
 ;"      UPDATE: This file makes reference to HCC 92, which is in 2023 descriptions but not 2015.  Will use 2023
 ;
 ;"STRUCTURE OF CSV FILE:
 ;"  
 ;"  First, rows with description.  
 ;"  1) ICD-10-CM Codes, ESRD, CMS-HCC and RxHCC Models
 ;"  2) Includes ICD-10 codes valid in FY2023 and FY2024
 ;"  3) <empty row>
 ;"  
 ;"  Next columns
 ;"           
 ;"  1                  2              3             4            5          6          7          8          9          10             11             12             13             14              15              16
 ;"  A                | B           |  C          |  D         |  E        | F        | G        | H        | I        | J            | K            | L            | M            | N             | O             | P 
 ;"  Diagnosis Codes  | Description |  CMS-HCC    |  CMS-HCC   |  CMS-HCC  | CMS-HCC  | CMS-HCC  | RxHCC    | RxHCC    | CMS-HCC      | CMS-HCC      | CMS-HCC      | CMS-HCC      | CMS-HCC       | RxHCC         | RxHCC             
 ;"                   |             |  ESRD       |  ESRD      |  Model    | Model    | Model    | Model    | Model    | ESRD Model   | ESRD Model   | Model        | Model        | Model         | Model         | Model
 ;"                   |             |  Model      |  Model     |  Category | Category | Category | Category | Category | Category     | Category     | Category     | Category     | Category      | Category      | Category
 ;"                   |             |  Category   |  Category  |  V22      | V24      | V28      | V05      | V08      | V21 for 2024 | V24 for 2024 | V22 for 2024 | V24 for 2024 | V28 for 2024  | V05 for 2024  | V08 for 2024
 ;"                   |             |  V21        |  V24       |           |          |          |          |          | Payment Year | Payment Year | Payment Year | Payment Year | Payment Year  | Payment Year  | Payment Year
 ;"                               
 ;" At bottom, there is a blank line, followed by footer lines
 ;" 
 ;" Then 
 ;" Notes:  ......
 ;"  ..     <-- entries
 ;"  ...    <-- entries
 ;"         <-- blank line
 ;" Output: ...
 ;" Source: ...
 ;"
 ;"===============================================================================
 ;
HASHCCCHILD(ICD,OUT,VER) ;"Does HCC have a child (descendant, i.e. more specific) code that is an HCC code? OR is ICD already HCC?
  ;"INPUT: ICD -- should be ICD code with decimal, e.g. Z93.5
  ;"       OUT -- PASS BY REFERENCE.  Format:
  ;"          OUT(<HCC ICD CODE>)=<HCC CODE>^<HCC DESCRIPTION>
  ;"       VER -- HCC version, e.g. 'V28'.  OPTIONAL.  If not passed, then the LAST created
  ;"           version record created will be used.  
  ;"Result: 0 -- no HCC descendant, 
  ;"        or 1 -- has descendant HCC ICD's, to be listed in OUT
  ;"        or 2 -- ICD itself is an HCC ICD
  ;"        or -1^error message
  NEW TMGRESULT SET TMGRESULT=0
  NEW IEN22759
  SET VER=$GET(VER)
  IF VER="" DO
  . SET IEN22759=+$PIECE($GET(^TMG(22759,0)),"^",3)  ;"most recently assigned record number
  ELSE  DO
  . SET IEN22759=+$ORDER(^TMG(22759,"B",VER,""))
  IF IEN22759'>0 DO  GOTO HCCCDN
  . SET TMGRESULT="-1^Unable to find record for version ["_VER_"]"
  SET ICD=$GET(ICD)
  IF ICD="" DO  GOTO HCCCDN
  . SET TMGRESULT="-1^No ICD code provided" 
  IF $DATA(^TMG(22759,IEN22759,"C",ICD)) DO 
  . SET TMGRESULT=2,OUT(ICD)=$$GETHCC(ICD,IEN22759)
  NEW SUBICD SET SUBICD=ICD
  FOR  SET SUBICD=$ORDER(^TMG(22759,IEN22759,"C",SUBICD)) QUIT:(SUBICD="")!(SUBICD'[ICD)  DO
  . SET OUT(SUBICD)=$$GETHCC(SUBICD,IEN22759)
  . SET TMGRESULT=1
HCCCDN ;
  QUIT TMGRESULT
  ;
GETHCC(ICD,IEN22759) ;
  NEW IEN22759D01 SET IEN22759D01=$ORDER(^TMG(22759,IEN22759,"C",ICD,0))
  NEW ZN SET ZN=$GET(^TMG(22759,IEN22759,1,IEN22759D01,0))  
  QUIT $PIECE(ZN,"^",1,2)
  ;
IMPORTHCC(WANTEDVER) ;
  NEW DATA
  SET WANTEDVER=$GET(WANTEDVER,"V28")  ;"<--- LIKELY NEEDS TO CHANGE IN THE FUTURE
  DO GETPARSEDHCC(.DATA,WANTEDVER)
  NEW HCC SET HCC=0
  FOR  SET HCC=$ORDER(DATA("HCC",HCC)) QUIT:HCC'>0  DO
  . NEW TMGRESULT
  . SET TMGRESULT=$$ENSUREVER(WANTEDVER)  ;"Returns 1^OK^IEN 
  . IF TMGRESULT<1 DO  QUIT
  . . WRITE !,$PIECE(TMGRESULT,"^",2)
  . NEW IEN22759 SET IEN22759=+$PIECE($GET(TMGRESULT),"^",3)  
  . SET TMGRESULT=$$ENSUREHCC(IEN22759,HCC,.DATA)  ;"Returns 1^OK^IEN
  . IF TMGRESULT<1 DO  QUIT
  . . WRITE !,$PIECE(TMGRESULT,"^",2)
  . NEW IEN22759D01 SET IEN22759D01=+$PIECE(TMGRESULT,"^",3)
  . NEW ICDARR MERGE ICDARR=DATA("HCC",HCC)
  . SET TMGRESULT=$$ENSUREICDS(IEN22759,IEN22759D01,.ICDARR)
  . IF TMGRESULT<1 DO  QUIT
  . . WRITE !,$PIECE(TMGRESULT,"^",2)
  IF $DATA(DATA("BAD ICD")) DO
  . WRITE !,"NOTE: The following ICD codes could not be found in VistA database",!
  . ZWR DATA("BAD ICD",*)
  . WRITE !
IHCDN ;  
  QUIT
  ;
ENSUREVER(VER) ;"Ensure VERSION record created.
  NEW TMGRESULT SET TMGRESULT="1^OK"
  NEW IEN22759 SET IEN22759=+$ORDER(^TMG(22759,"B",VER,""))
  IF IEN22759>0 GOTO ENVDN
  NEW TMGFDA,TMGMSG,TMGIEN
  SET TMGFDA(22759,"+1,",.01)=VER
  DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG) DO  GOTO EHCDN
  . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)  
  SET IEN22759=+$GET(TMGIEN(1))
  IF IEN22759'>0 SET TMGRESULT="-1^Unable to find IEN of added VERSION subrecord"
ENVDN ;  
  IF +TMGRESULT>0,IEN22759>0 SET TMGRESULT=TMGRESULT_"^"_IEN22759
  QUIT TMGRESULT
  ;  
ENSUREHCC(IEN22759,HCC,DATA) ;"Ensure record created for HCC value
  NEW TMGRESULT SET TMGRESULT="1^OK"
  SET HCC=+$GET(HCC)
  IF HCC'>0 DO  GOTO EHCDN
  . SET TMGRESULT="-1^Did not get valid HCC number.  Got ["_$GET(HCC)_"]"
  NEW IEN22759D01 SET IEN22759D01=+$ORDER(^TMG(22759,IEN22759,"B",HCC,""))
  IF IEN22759D01>0 GOTO EHC2
  NEW TMGFDA,TMGMSG,TMGIEN
  SET TMGFDA(22759.01,"+1,"_IEN22759_",",.01)=HCC
  SET TMGIEN(1)=HCC  
  DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG) DO  GOTO EHCDN
  . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  SET IEN22759D01=TMGIEN(1)
EHC2  
  NEW DESCR SET DESCR=$GET(DATA("HCC DESCR",HCC))
  NEW PRIORDESCR SET PRIORDESCR=$PIECE($GET(^TMG(22759,IEN22759,1,IEN22759D01,0)),"^",2)
  IF DESCR=PRIORDESCR GOTO EHCDN
  SET TMGFDA(22759.01,IEN22759D01_","_IEN22759_",",.02)=DESCR
  DO FILE^DIE("E","TMGFDA","TMGMSG")
  IF $DATA(TMGMSG) DO  GOTO EHCDN
  . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)  
EHCDN ;  
  IF +TMGRESULT>0,IEN22759D01>0 SET TMGRESULT=TMGRESULT_"^"_IEN22759D01
  QUIT TMGRESULT
  ;
ENSUREICDS(IEN22759,IEN22759D01,ARR) ;"ensure ICD code record created
  ;"INPUT: IENS -- IENS FOR 22759.01 (VER multiple, that will hold ICD's, in 22759.11
  ;"       ARR -- PASS BY REFERENCE.  Format:
  ;"          ARR("<ICD_decimal_code>^IEN80")=<index#>  <-- index not needed here. 
  NEW TMGRESULT SET TMGRESULT="1^OK"
  NEW ENTRY SET ENTRY=""
  FOR  SET ENTRY=$ORDER(ARR(ENTRY)) QUIT:(ENTRY="")!(TMGRESULT'>0)  DO
  . NEW IEN80 SET IEN80=+$PIECE(ENTRY,"^",2) QUIT:IEN80'>0
  . NEW SUBSUBIEN SET SUBSUBIEN=$ORDER(^TMG(22759,IEN22759,1,IEN22759D01,1,"B",IEN80,""))
  . IF SUBSUBIEN>0 QUIT  ;"already present. 
  . NEW TMGFDA,TMGIEN,TMGMSG
  . NEW IENS SET IENS="+1,"_IEN22759D01_","_IEN22759_","
  . SET TMGFDA(22759.11,IENS,.01)="`"_IEN80
  . DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
  . IF $DATA(TMGMSG) DO  QUIT
  . . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)    
  QUIT TMGRESULT  
  ;  
GETPARSEDHCC(PARSEDARR,WANTEDVER)  ;"Load CSV file and parse to usable array
  ;"INPUT: PARSEDARR -- PASS BY REFERENCE, AN OUT PARAMETER.  Format:
  ;"          PARSEDARR(<index#>,<COL#>)=<value>  
  ;"          PARSEDARR("A",<COL#>)=<Column title>
  ;"          PARSEDARR("HCC",<HCC#>,"<ICD_decimal_code>^IEN80")=<index#>
  ;"        At time of creation, we have:
  ;"             col#1 <-- ICDCODE^<ICD_decimal_code>^IEN80
  ;"             col#2 <-- ICD description
  ;"             col#7 <-- HCC number  (NOTE: will be blank if col#14 holds 'No') <--- DELETED if 'No'
  ;"             col#14 <-- Yes or No regarding if applicable.    <--- DELETED
  ;"        WANTEDVER -- e.g. 'V28'
  NEW TMGRESULT SET TMGRESULT="1^OK"
  SET WANTEDVER=$GET(WANTEDVER,"V28")  ;"<--- LIKELY NEEDS TO CHANGE IN THE FUTURE
  NEW TMGDATA,OPTION 
  SET OPTION("MATCH","*.csv")=""
  SET OPTION("PATH")="/mnt/WinServer/"
  NEW FULLPATHNAME SET FULLPATHNAME=$$FBROWSE^TMGIOUT2(.OPTION)
  IF $$FILEXIST^TMGIOUTL(FULLPATHNAME)'=1 DO  GOTO ICDN
  . SET TMGRESULT="-1^File not found: '"_FULLPATHNAME_"'"
  NEW HFSOPTION SET HFSOPTION("OVERFLOW")=1
  IF $$HFS2ARFP^TMGIOUT3(FULLPATHNAME,"TMGDATA",.HFSOPTION)=0 DO  GOTO ICDN
  . SET TMGRESULT="-1^Error loading file: '"_FULLPATHNAME_"'"
  NEW IDX SET IDX=$ORDER(TMGDATA(""))
  IF IDX="" DO  GOTO ICDN
  . SET TMGRESULT="-1^No data found in file: '"_FULLPATHNAME_"'"
  ;"First remove header lines
  NEW TEXT1 SET TEXT1="ICD-10-CM Codes, ESRD, CMS-HCC and RxHCC Models"
  NEW TEXT2 SET TEXT2="Includes ICD-10 codes valid in FY"
  NEW LINE SET LINE=$GET(TMGDATA(1))
  IF LINE'[TEXT1 DO  GOTO ICDN
  . SET TMGRESULT="-1^Did not find expected text on line 1: "_TEXT1
  KILL TMGDATA(1)
  SET LINE=$GET(TMGDATA(2))
  IF LINE'[TEXT2 DO  GOTO ICDN
  . SET TMGRESULT="-1^Did not find expected text on line 2: "_TEXT2
  KILL TMGDATA(2)
  SET LINE=$GET(TMGDATA(3))
  SET LINE=$TRANSLATE(LINE,","_$CHAR(13),"")
  IF LINE'="" DO  GOTO ICDN
  . SET TMGRESULT="-1^Did not find expected expected BLANK LINE text on line 3"
  KILL TMGDATA(3)
  ;"Next, scan through all lines, looking for blank line to signal end of entries. 
  NEW IDX,DONE SET (IDX,DONE)=0
  FOR  SET IDX=$ORDER(TMGDATA(IDX)) QUIT:(IDX'>0)!DONE  DO
  . SET LINE=$GET(TMGDATA(IDX))
  . IF LINE="" SET DONE=1 QUIT
  . SET LINE=$TRANSLATE(LINE,","_$CHAR(13),"")
  . IF LINE'="" QUIT
  . SET DONE=1,IDX=IDX-1
  ;"Delete from blank line to end of rows, deleting footer entries.  
  IF IDX>0 SET IDX=IDX-1 FOR  SET IDX=$ORDER(TMGDATA(IDX)) QUIT:(IDX'>0)  DO
  . SET LINE=$GET(TMGDATA(IDX))
  . KILL TMGDATA(IDX)  ;"Delete footer lines
  ;"Now strip $CHAR(13) from end of line
  SET IDX=0
  FOR  SET IDX=$ORDER(TMGDATA(IDX)) QUIT:(IDX'>0)  DO
  . SET LINE=$GET(TMGDATA(IDX))
  . NEW L SET L=$LENGTH(LINE)
  . IF $EXTRACT(LINE,L)'=$CHAR(13) QUIT
  . SET TMGDATA(IDX)=$EXTRACT(LINE,1,L-1)  
  ;"Now parse CSV stuff
  KILL OPTION
  SET OPTION("HANDLE BREAK IN QUOTES")=1
  SET TMGRESULT=$$CSVARR2ARR^TMGIOUT4("PARSEDARR",.TMGDATA,.OPTION)
  ;"Format of PARSEDARR on return
  ;"PARSEDARR("A",#)=<TITLE OF COLUMN>
  ;"PARSEDARR(<Line#>,<Column#>)=<value>
  ;"---
  NEW HCCARR DO GETHCCNAMES2(.HCCARR)
  ;"Next, delete unwanted columns
  NEW COL SET COL=0
  FOR  SET COL=$ORDER(PARSEDARR("A",COL)) QUIT:COL'>0  DO
  . NEW TITLE SET TITLE=$GET(PARSEDARR("A",COL)) QUIT:TITLE=""
  . IF TITLE["""" SET TITLE=$TRANSLATE(TITLE,"""","") SET PARSEDARR("A",COL)=TITLE
  . IF TITLE["Description" QUIT
  . IF TITLE'["Category" QUIT
  . IF TITLE[WANTEDVER QUIT  ;"We want to keep specified version (e.g. V28), and delete all the other categories.  
  . SET IDX=0
  . FOR  SET IDX=$ORDER(PARSEDARR(IDX)) QUIT:IDX'>0  DO    ;"Delete this COL from all records
  . . KILL PARSEDARR(IDX,COL)
  . KILL PARSEDARR("A",COL)
  ;"Next, convert ICD codes to real ICD codes (with decimal), and pointer to records
  ;"NOTE: This assumes that diagnosis code is in column 1.  May need to verify in the future. 
  SET IDX=0
  FOR  SET IDX=$ORDER(PARSEDARR(IDX)) QUIT:IDX'>0  DO
  . NEW CODE SET CODE=$GET(PARSEDARR(IDX,1)) QUIT:CODE=""  ;"<-- This is ICD code WITHOUT decimal
  . NEW DESCR SET DESCR=$GET(PARSEDARR(IDX,2)) 
  . NEW ICD SET ICD=$$FIXICD10^TMGSUSMC(CODE)
  . IF +ICD=-1 DO  QUIT  ;"ignore error message
  . . ;"WRITE "ERROR: ",$PIECE(ICD,"^",2),"  Deleting entry",!
  . . KILL PARSEDARR(IDX)
  . . SET PARSEDARR("BAD ICD",CODE)=""
  . NEW PTR SET PTR=+$ORDER(^ICD9("ABA",30,ICD,""))
  . ;"SET PARSEDARR(IDX,1)=CODE_"^"_$$TRIM^XLFSTR(ICD)_"^"_PTR_"^"_DESCR
  . SET PARSEDARR(IDX,1)=$$TRIM^XLFSTR(ICD)_"^"_PTR
  ;"Next, Add HCC descriptions
  ;"NOTE: This assumes that HCC code is in column 7.  May need to verify in the future. 
  SET IDX=0
  FOR  SET IDX=$ORDER(PARSEDARR(IDX)) QUIT:IDX'>0  DO
  . NEW CODE SET CODE=$GET(PARSEDARR(IDX,7)) QUIT:CODE=""  ;"<---HCC code 
  . NEW DESCR SET DESCR=$GET(HCCARR(CODE)) QUIT:DESCR="" 
  . SET PARSEDARR(IDX,7)=CODE_"^"_DESCR
  ;"Next: delete all entries with null HCC or NO for applibility field (field 14)
  ;"NOTE: This assumes that HCC code is in column 7.  May need to verify in the future. 
  ;"NOTE: This assumes that APPLY boolean is in column 14.  May need to verify in the future. 
  SET IDX=0
  FOR  SET IDX=$ORDER(PARSEDARR(IDX)) QUIT:IDX'>0  DO
  . NEW CODE SET CODE=$GET(PARSEDARR(IDX,7))
  . NEW APPLY SET APPLY=$GET(PARSEDARR(IDX,14))
  . IF (CODE="")!(APPLY="No") KILL PARSEDARR(IDX)
  . KILL PARSEDARR(IDX,14)  ;"since all remaining entries will be 'Yes', it plays no role, so delete.  
  ;"Now make HCC cross reference. 
  SET IDX=0
  FOR  SET IDX=$ORDER(PARSEDARR(IDX)) QUIT:IDX'>0  DO
  . NEW CODE SET CODE=$GET(PARSEDARR(IDX,1)) QUIT:CODE=""
  . ;"NEW ICD SET ICD=$PIECE(CODE,"^",2,3)
  . NEW ICD SET ICD=$PIECE(CODE,"^",1,2)  ;"<-- ICD10^IEN80
  . NEW HCC SET HCC=+$GET(PARSEDARR(IDX,7)) QUIT:HCC=""
  . NEW HCCNAME SET HCCNAME=$GET(HCCARR(HCC))
  . ;"ALREADY DELETED --> NEW APPL SET APPL=$GET(PARSEDARR(IDX,14)) QUIT:APPL="No"
  . SET PARSEDARR("HCC DESCR",HCC)=HCCNAME
  . SET PARSEDARR("HCC",HCC,ICD)=IDX
ICDN ;
  QUIT TMGRESULT
  ;
GETHCCNAMES(ARR) ;"DEPRECATED <--- this is from 2015 data.  OLD, and doesn't seem to match HCC's in mapping file.  WON'T USE.
  NEW CODE,DONE,IDX SET DONE=0
  FOR IDX=1:1 DO  QUIT:DONE
  . NEW LINE SET LINE=$$TRIM^XLFSTR($TEXT(HCCNAMES+IDX))
  . IF LINE="" SET DONE=1 QUIT
  . SET LINE=$PIECE(LINE,";;""",2,99)
  . IF LINE["<DONE>" SET DONE=1 QUIT
  . SET CODE=+$PIECE(LINE,"^",1),LINE=$PIECE(LINE,"^",2,99)
  . SET ARR(CODE)=LINE
  QUIT
  ;
GETHCCNAMES2(ARR) ;  
  NEW CODE,DONE,IDX SET DONE=0
  FOR IDX=1:1 DO  QUIT:DONE
  . NEW LINE SET LINE=$$TRIM^XLFSTR($TEXT(HCCNAMES2+IDX))
  . IF LINE="" SET DONE=1 QUIT
  . SET LINE=$PIECE(LINE,";;""",2,99)
  . IF LINE["<DONE>" SET DONE=1 QUIT
  . SET CODE=+$PIECE(LINE,"^",1),LINE=$PIECE(LINE,"^",2,99)
  . SET ARR(CODE)=LINE
  QUIT
  ;  
HCCNAMES   ;"DEPRECATED <--- this is from 2015 data.  OLD, and doesn't seem to match HCC's in mapping file.  WON'T USE.  
  ;;"1^HIV/AIDS 
  ;;"2^Septicemia, Sepsis, Systemic Inflammatory Response Syndrome/Shock
  ;;"6^Opportunistic Infections 
  ;;"8^Metastatic Cancer and Acute Leukemia 
  ;;"9^Lung and Other Severe Cancers 
  ;;"10^Lymphoma and Other Cancers 
  ;;"11^Colorectal, Bladder, and Other Cancers 
  ;;"12^Breast, Prostate, and Other Cancers and Tumors 
  ;;"17^Diabetes with Acute Complications 
  ;;"18^Diabetes with Chronic Complications 
  ;;"19^Diabetes without Complication 
  ;;"21^Protein-Calorie Malnutrition 
  ;;"22^Morbid Obesity 
  ;;"23^Other Significant Endocrine and Metabolic Disorders 
  ;;"27^End-Stage Liver Disease 
  ;;"28^Cirrhosis of Liver 
  ;;"29^Chronic Hepatitis 
  ;;"33^Intestinal Obstruction/Perforation 
  ;;"34^Chronic Pancreatitis 
  ;;"35^Inflammatory Bowel Disease 
  ;;"39^Bone/Joint/Muscle Infections/Necrosis 
  ;;"40^Rheumatoid Arthritis and Inflammatory Connective Tissue Disease
  ;;"46^Severe Hematological Disorders 
  ;;"47^Disorders of Immunity 
  ;;"48^Coagulation Defects and Other Specified Hematological Disorders
  ;;"54^Drug/Alcohol Psychosis 
  ;;"55^Drug/Alcohol Dependence 
  ;;"57^Schizophrenia 
  ;;"58^Major Depressive, Bipolar, and Paranoid Disorders 
  ;;"70^Quadriplegia 
  ;;"71^Paraplegia 
  ;;"72^Spinal Cord Disorders/Injuries 
  ;;"73^Amyotrophic Lateral Sclerosis and Other Motor Neuron Disease
  ;;"74^Cerebral Palsy 
  ;;"75^Myasthenia Gravis/Myoneural Disorders, Inflammatory and Toxic Neuropathy
  ;;"76^Muscular Dystrophy 
  ;;"77^Multiple Sclerosis 
  ;;"78^Parkinson's and Huntington's Diseases 
  ;;"79^Seizure Disorders and Convulsions 
  ;;"80^Coma, Brain Compression/Anoxic Damage
  ;;"82^Respirator Dependence/Tracheostomy Status
  ;;"83^Respiratory Arrest
  ;;"84^Cardio-Respiratory Failure and Shock
  ;;"85^Congestive Heart Failure
  ;;"86^Acute Myocardial Infarction
  ;;"87^Unstable Angina and Other Acute Ischemic Heart Disease
  ;;"88^Angina Pectoris
  ;;"96^Specified Heart Arrhythmias
  ;;"99^Cerebral Hemorrhage
  ;;"100^Ischemic or Unspecified Stroke
  ;;"103^Hemiplegia/Hemiparesis
  ;;"104^Monoplegia, Other Paralytic Syndromes
  ;;"106^Atherosclerosis of the Extremities with Ulceration or Gangrene
  ;;"107^Vascular Disease with Complications
  ;;"108^Vascular Disease
  ;;"110^Cystic Fibrosis
  ;;"111^Chronic Obstructive Pulmonary Disease
  ;;"112^Fibrosis of Lung and Other Chronic Lung Disorders
  ;;"114^Aspiration and Specified Bacterial Pneumonias
  ;;"115^Pneumococcal Pneumonia, Empyema, Lung Abscess
  ;;"122^Proliferative Diabetic Retinopathy and Vitreous Hemorrhage
  ;;"124^Exudative Macular Degeneration
  ;;"134^Dialysis Status
  ;;"135^Acute Renal Failure
  ;;"136^Chronic Kidney Disease, Stage 5
  ;;"137^Chronic Kidney Disease, Severe (Stage 4)
  ;;"157^Pressure Ulcer of Skin with Necrosis Through to Muscle, Tendon, or Bone
  ;;"158^Pressure Ulcer of Skin with Full Thickness Skin Loss
  ;;"161^Chronic Ulcer of Skin, Except Pressure
  ;;"162^Severe Skin Burn or Condition
  ;;"166^Severe Head Injury
  ;;"167^Major Head Injury
  ;;"169^Vertebral Fractures without Spinal Cord Injury
  ;;"170^Hip Fracture/Dislocation
  ;;"173^Traumatic Amputations and Complications
  ;;"176^Complications of Specified Implanted Device or Graft
  ;;"186^Major Organ Transplant or Replacement Status
  ;;"188^Artificial Openings for Feeding or Elimination
  ;;"189^Amputation Status, Lower Limb/Amputation Complications
  ;;"<DONE>
  ;"SOURCE: https://www.cms.gov/Medicare/Medicare-Fee-for-Service-Payment/PhysicianFeedbackProgram/Downloads/2015-RiskAdj-FactSheet.pdf  
  ;"NOTE: https://codingintel.com/hcc-coding-changes/ This site says that older HCC were modeled on icd9, and that HCC's are goign to be renumbered
  ;;
HCCNAMES2 ;" <-- this is from 2023 source (for year 2024)   
  ;;"1^HIV/AIDS
  ;;"2^Septicemia, Sepsis, Systemic Inflammatory Response Syndrome/Shock
  ;;"6^Opportunistic Infections
  ;;"17^Cancer Metastatic to Lung, Liver, Brain, and Other Organs; Acute Myeloid Leukemia Except Promyelocytic
  ;;"18^Cancer Metastatic to Bone, Other and Unspecified Metastatic Cancer; Acute Leukemia Except Myeloid
  ;;"19^Myelodysplastic Syndromes, Multiple Myeloma, and Other Cancers
  ;;"20^Lung and Other Severe Cancers
  ;;"21^Lymphoma and Other Cancers
  ;;"22^Bladder, Colorectal, and Other Cancers
  ;;"23^Prostate, Breast, and Other Cancers and Tumors
  ;;"35^Pancreas Transplant Status
  ;;"36^Diabetes with Severe Acute Complications
  ;;"37^Diabetes with Chronic Complications
  ;;"38^Diabetes with Glycemic, Unspecified, or No Complications
  ;;"48^Morbid Obesity
  ;;"49^Specified Lysosomal Storage Disorders
  ;;"50^Amyloidosis, Porphyria, and Other Specified Metabolic Disorders
  ;;"51^Addison's and Cushing's Diseases, Acromegaly, and Other Specified Endocrine Disorders
  ;;"62^Liver Transplant Status/Complications
  ;;"63^Chronic Liver Failure/End-Stage Liver Disorders
  ;;"64^Cirrhosis of Liver
  ;;"65^Chronic Hepatitis
  ;;"68^Cholangitis and Obstruction of Bile Duct Without Gallstones
  ;;"77^Intestine Transplant Status/Complications
  ;;"78^Intestinal Obstruction/Perforation
  ;;"79^Chronic Pancreatitis
  ;;"80^Crohn's Disease (Regional Enteritis)
  ;;"81^Ulcerative Colitis
  ;;"92^Bone/Joint/Muscle/Severe Soft Tissue Infections/Necrosis
  ;;"93^Rheumatoid Arthritis and Other Specified Inflammatory Rheumatic Disorders
  ;;"94^Systemic Lupus Erythematosus and Other Specified Systemic Connective Tissue Disorders
  ;;"107^Sickle Cell Anemia (Hb-SS) and Thalassemia Beta Zero
  ;;"108^Sickle Cell Disorders, Except Sickle Cell Anemia (Hb-SS) and Thalassemia Beta Zero; Beta Thalassemia Major
  ;;"109^Acquired Hemolytic, Aplastic, and Sideroblastic Anemias
  ;;"111^Hemophilia, Male
  ;;"112^Immune Thrombocytopenia and Specified Coagulation Defects and Hemorrhagic Conditions
  ;;"114^Common Variable and Combined Immunodeficiencies
  ;;"115^Specified Immunodeficiencies and White Blood Cell Disorders
  ;;"125^Dementia, Severe
  ;;"126^Dementia, Moderate
  ;;"127^Dementia, Mild or Unspecified
  ;;"135^Drug Use with Psychotic Complications
  ;;"136^Alcohol Use with Psychotic Complications
  ;;"137^Drug Use Disorder, Moderate/Severe, or Drug Use with Non-Psychotic Complications
  ;;"138^Drug Use Disorder, Mild, Uncomplicated, Except Cannabis
  ;;"139^Alcohol Use Disorder, Moderate/Severe, or Alcohol Use with Specified Non-Psychotic Complications
  ;;"151^Schizophrenia
  ;;"152^Psychosis, Except Schizophrenia
  ;;"153^Personality Disorders; Anorexia/Bulimia Nervosa
  ;;"154^Bipolar Disorders without Psychosis
  ;;"155^Major Depression, Moderate or Severe, without Psychosis
  ;;"180^Quadriplegia
  ;;"181^Paraplegia
  ;;"182^Spinal Cord Disorders/Injuries
  ;;"190^Amyotrophic Lateral Sclerosis and Other Motor Neuron Disease, Spinal Muscular Atrophy
  ;;"191^Quadriplegic Cerebral Palsy
  ;;"192^Cerebral Palsy, Except Quadriplegic
  ;;"193^Chronic Inflammatory Demyelinating Polyneuritis and Multifocal Motor Neuropathy
  ;;"195^Myasthenia Gravis with (Acute) Exacerbation
  ;;"196^Myasthenia Gravis without (Acute) Exacerbation and Other Myoneural Disorders
  ;;"197^Muscular Dystrophy
  ;;"198^Multiple Sclerosis
  ;;"199^Parkinson and Other Degenerative Disease of Basal Ganglia
  ;;"200^Friedreich and Other Hereditary Ataxias; Huntington Disease
  ;;"201^Seizure Disorders and Convulsions
  ;;"202^Coma, Brain Compression/Anoxic Damage
  ;;"211^Respirator Dependence/Tracheostomy Status/Complications
  ;;"212^Respiratory Arrest
  ;;"213^Cardio-Respiratory Failure and Shock
  ;;"221^Heart Transplant Status/Complications
  ;;"222^End-Stage Heart Failure
  ;;"223^Heart Failure with Heart Assist Device/Artificial Heart
  ;;"224^Acute on Chronic Heart Failure
  ;;"225^Acute Heart Failure (Excludes Acute on Chronic)
  ;;"226^Heart Failure, Except End-Stage and Acute
  ;;"227^Cardiomyopathy/Myocarditis
  ;;"228^Acute Myocardial Infarction
  ;;"229^Unstable Angina and Other Acute Ischemic Heart Disease
  ;;"238^Specified Heart Arrhythmias
  ;;"248^Intracranial Hemorrhage
  ;;"249^Ischemic or Unspecified Stroke
  ;;"253^Hemiplegia/Hemiparesis
  ;;"254^Monoplegia, Other Paralytic Syndromes
  ;;"263^Atherosclerosis of Arteries of the Extremities with Ulceration or Gangrene
  ;;"264^Vascular Disease with Complications
  ;;"267^Deep Vein Thrombosis and Pulmonary Embolism
  ;;"276^Lung Transplant Status/Complications
  ;;"277^Cystic Fibrosis
  ;;"278^Idiopathic Pulmonary Fibrosis and Lung Involvement in Systemic Sclerosis
  ;;"279^Severe Persistent Asthma
  ;;"280^Chronic Obstructive Pulmonary Disease, Interstitial Lung Disorders, and Other Chronic Lung Disorders
  ;;"282^Aspiration and Specified Bacterial Pneumonias
  ;;"283^Empyema, Lung Abscess
  ;;"298^Severe Diabetic Eye Disease, Retinal Vein Occlusion, and Vitreous Hemorrhage
  ;;"300^Exudative Macular Degeneration
  ;;"326^Chronic Kidney Disease, Stage 5 
  ;;"327^Chronic Kidney Disease, Severe (Stage 4)
  ;;"328^Chronic Kidney Disease, Moderate (Stage 3B)
  ;;"329^Chronic Kidney Disease, Moderate (Stage 3, Except 3B)
  ;;"379^Pressure Ulcer of Skin with Necrosis Through to Muscle, Tendon, or Bone
  ;;"380^Chronic Ulcer of Skin, Except Pressure, Through to Bone or Muscle
  ;;"381^Pressure Ulcer of Skin with Full Thickness Skin Loss
  ;;"382^Pressure Ulcer of Skin with Partial Thickness Skin Loss
  ;;"383^Chronic Ulcer of Skin, Except Pressure, Not Specified as Through to Bone or Muscle
  ;;"385^Severe Skin Burn
  ;;"387^Pemphigus, Pemphigoid, and Other Specified Autoimmune Skin Disorders
  ;;"397^Major Head Injury with Loss of Consciousness > 1 Hour
  ;;"398^Major Head Injury with Loss of Consciousness < 1 Hour or Unspecified
  ;;"399^Major Head Injury without Loss of Consciousness
  ;;"401^Vertebral Fractures without Spinal Cord Injury
  ;;"402^Hip Fracture/Dislocation
  ;;"405^Traumatic Amputations and Complications
  ;;"409^Amputation Status, Lower Limb/Amputation Complications
  ;;"454^Stem Cell, Including Bone Marrow, Transplant Status/Complications
  ;;"463^Artificial Openings for Feeding or Elimination
  ;;"<DONE>
  ;"SOURCE: https://www.cms.gov/files/document/2024-announcement-pdf.pdf  page 184
  ;
SETXREF(IEN80,IEN22759,IEN22759D01,IEN22759D11) ;"XRef callback for SET
  ;"Called by Fileman cross referencer.  Link stored at ^DD(22759.11,01,1,2,1)
  NEW ICD SET ICD=$PIECE($GET(^ICD9(IEN80,0)),"^",1) QUIT:ICD=""
  SET ^TMG(22759,IEN22759,"C",ICD,IEN22759D01,IEN22759D11)=""
  QUIT
  ;
KILXREF(IEN80,IEN22759,IEN22759D01,IEN22759D11) ;"XRef callback for KILL
  ;"Called by Fileman cross referencer.  Link stored at ^DD(22759.11,01,1,2,2)
  NEW ICD SET ICD=$PIECE($GET(^ICD9(IEN80,0)),"^",1) QUIT:ICD=""
  KILL ^TMG(22759,IEN22759,"C",ICD,IEN22759D01,IEN22759D11)
  QUIT
  ;"
ICDCOLOR(ICDCODE)  ;"
  ;"This is called by SEARCH^ORWLEX to determine if the 
  ;"   code sent should be colored. If so, the color is sent
  ;"   if not, then it will return a blank.
  ;" The result will be returned in piece 10 of ORY
  NEW TMGRESULT SET TMGRESULT=""
  NEW OUT,HCCTEST
  SET HCCTEST=$$HASHCCCHILD(ICDCODE,.OUT)
  IF HCCTEST=2 SET TMGRESULT="#FFFF00"
  IF HCCTEST=1 SET TMGRESULT="#FF6666"
  QUIT TMGRESULT
  ;"