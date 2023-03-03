TMGLRU1 ;TMG/kst-Utility for managing lab order dialog ;12/15/22
              ;;1.0;TMG-LIB;**1**;12/15/22
 ;
 ;"TMG LAB ORDER DIALOG UTILITY
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 12/15/22  Kevin S. Toppenberg MD
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
 ;"REPORT(DIALOGNAME,OUT) -- Generate flattened report of dialog
 ;"RPTARR(DIALOGNAME,OUT) -- Generate an array report of dialog
 ;"COMPILE(DEFPATH) -- Read in INI file and compile and file into FM file
 ;
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;"REPORTONE(IEN,REF) --Report on 1 element, and all its contained ITEMS
 ;"ENSUREGROUP(ARR,NAME,TYPE) --Ensure GROUP is created (with ARR elements being put into ITEMS subfile)  
 ;"ENSUREARR(ARR,TYPE) --Ensure entries are found in FM file 22751 TMG LAB ORDER DIALOG ELEMENTS  
 ;"GETIEN(NAME) -- Search file 22751 for .01 = NAME
 ;"ENSUREREC(NAME,TYPE)  -- Ensure rec in file 22751 with NAME and TYPE
 ;"ENSURESUBREC(IEN,PARENTIEN) -- Ensure entry in ITEMS subfile inside 22751
 ;"DATA2ARR(TAG,OUTARR) -- Load TEXT data from this file into array
 ;"PARSEELEMENT(ARR,DELIM,OUT) -- parse elements of array into sub elements
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;
REPORT(DIALOGNAME,OUT) ;"Generate flattened report of dialog
  NEW TEMP DO RPTARR(.DIALOGNAME,.TEMP)
  DO ZWR2ARR^TMGZWR("TEMP","OUT") 
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(OUT(IDX)) QUIT:IDX'>0  DO
  . NEW LINE SET LINE=$GET(OUT(IDX))
  . SET LINE=$TRANSLATE(LINE,"""","")
  . NEW VALUE SET VALUE=$PIECE(LINE,"=",2,999)
  . SET LINE=$PIECE($PIECE(LINE,"TEMP(",2),")",1)_"^"_VALUE
  . SET OUT(IDX)=LINE
  QUIT
  ; 
RPTARR(DIALOGNAME,OUT) ;"Generate an array report of dialog
  ;"Input: DIALOGNAME -- the name of the dialog to get report on.  E.g. "TMG LAB ORDER DIALOG"
  ;"       OUT -- PASS BY REFERENCE.  Filled with data from Fileman File
  SET DIALOGNAME=$GET(DIALOGNAME,"TMG LAB ORDER DIALOG")
  NEW IEN SET IEN=$$GETIEN(DIALOGNAME) 
  IF IEN'>0 GOTO RPDN
  DO REPORTONE(IEN,"OUT(1)")
RPDN ;  
  QUIT
  ;
REPORTONE(IEN,REF) ;"Report on 1 element, and all its contained ITEMS
  ;"Input: IEN -- IEN in 22751 
  ;"       REF -- PASS BY NAME.  An OUT PARAMETER
  ;"Output -- @REF is filled, format as follows
  ;"       @REF@(#)=<IEN>^<Name>^<Type>^<Fasting>
  ;"       @REF@(#,#)=<Name>^<Type>^<Fasting>  <-- 1 entry for each element in ITEMS
  ;"Result: None
  NEW ZN SET ZN=$GET(^TMG(22751,IEN,0))
  NEW NAME SET NAME=$PIECE(ZN,"^",1)
  NEW TN SET TN=$GET(^TMG(22751,IEN,10))
  ;"NEW IDX SET IDX=$ORDER(@REF@(""),-1)+1
  SET @REF=IEN_"^"_NAME_"^"_$PIECE(ZN,"^",2)_"^"_$PIECE(TN,"^",1)
  NEW JDX SET JDX=0
  FOR  SET JDX=$ORDER(^TMG(22751,IEN,1,JDX)) QUIT:JDX'>0  DO
  . NEW PTIEN SET PTIEN=+$GET(^TMG(22751,IEN,1,JDX,0))
  . NEW SUBREF SET SUBREF=$NAME(@REF@(JDX))
  . DO REPORTONE(PTIEN,SUBREF)
  QUIT
  ;
COMPILE(DEFPATH)  ;"Read in INI file and compile and file into FM file 22751
  ;"NOTICE!! -- In addition to reading in the INI file, I also have to get
  ;"     data info 22751 to suppor ORDERING PROVIDER, ORDER FLAGS, ORDER OPTIONS etc.
  ;"     Currently, I have added these manually.  But in, in the future, I want
  ;"     to clear all data from 22751, so I can read in INI file again, then 
  ;"     I will need to add functions to programatically add these. I.e. the 
  ;"     following entries...
  ;"            Entry #9
  ;"               .01-ITEMS : TMG LAB ORDER LIST ORDERING PROVIDER (`93 in #22751)
  ;"            Entry #10
  ;"               .01-ITEMS : TMG LAB ORDER LIST ORDER TIMING (`94 in #22751)
  ;"            Entry #11
  ;"               .01-ITEMS : TMG LAB ORDER LIST ORDER FLAGS (`107 in #22751) 
  ;"            Entry #12
  ;"               .01-ITEMS : TMG LAB ORDER LIST ORDER OPTIONS (`108 in #22751)
  ;"            Entry #13
  ;"               .01-ITEMS : TMG LAB ORDER FREE TEXT DX 1 (`109 in #22751)
  ;"            Entry #14
  ;"               .01-ITEMS : TMG LAB ORDER FREE TEXT DX 2 (`110 in #22751)
  ;"            Entry #15
  ;"               .01-ITEMS : TMG LAB ORDER FREE TEXT DX 3 (`111 in #22751)
  ;"            Entry #16
  ;"               .01-ITEMS : TMG LAB ORDER FREE TEXT DX 4 (`112 in #22751)
  ;"            Entry #17
  ;"               .01-ITEMS : TMG LAB ORDER COMMENTS (`113 in #22751)
  ;"            Entry #18
  ;"               .01-ITEMS : TMG LAB ORDER FREE TEXT LAB (`114 in #22751)             
  NEW FILEOPTION SET FILEOPTION("PATH")=$GET(DEFPATH)
  SET (DIRNAME,FNAME)=""
  IF $$FBROWSE^TMGIOUT2(.FILEOPTION,.DIRNAME,.FNAME)  ;"DIRNAME AND FNAME are OUT parameters
  IF FNAME="" QUIT
  NEW INIARR
  DO HFSINI2ARR^TMGIOUT3(DIRNAME,FNAME,"INIARR")
  NEW TEMP MERGE TEMP=INIARR("Items")  ;"Lab order stuff is in Items and BUNDLE sections
  NEW PARSEDARR DO PARSEELEMENT(.TEMP,"|",.PARSEDARR)
  NEW DXARR MERGE DXARR=PARSEDARR("ICD9")  
  ;"Example:
  ;"DXARR
  ;"}~1 = Anemia - D64.9
  ;"}~2 = Low B12 - E53.8
  ;"}~3 = Low Vit-D - E55.9
  ;"}~4 = DM-2 - E11.9
  ;"}~5 = DM-2 Uncontrolled - E11.65  
  NEW PROCARR MERGE PROCARR=PARSEDARR("CPT")
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(PROCARR(IDX)) QUIT:IDX'>0  DO
  . NEW APROC SET APROC=$GET(PROCARR(IDX)) QUIT:APROC=""
  . SET PROCARR("B",APROC,IDX)=""    
  ;"EXAMPLE:
  ;"PROC
  ;"}~1 = OFFICE Urine Drug Screen
  ;"}~2 = OFFICE Check BP today
  ;"}~3 = B12
  ;"}~4 = Vitamin D Level (82306)
  ;"}~5 = Folic Acid
  ;"...
  ;"}~B
  ;"  }~ANA F/U Panel 11 (ANA11)
  ;"  | }~21 = ""
  ;"  }~ANA Titer (ANATP)
  ;"  | }~20 = ""
  ;"  }~Acute Hepatitis Panel (HEPAL)
  ;"  | }~43 = ""
  ;"  }~B12
  ;"  | }~3 = ""
  NEW BUNDLEGRP MERGE BUNDLEGRP=PARSEDARR("Bundles")
  ;"Example:  
  ;"BUNDLEGRP
  ;"}~1 = AnnualDiabetes
  ;"}~2 = Screening
  ;"}~3 = ScreeningWVitamins
  ;"}~4 = ScreeningWVits-UA
  ;"}~5 = Diabetes
  NEW DISPARR DO DATA2ARR("LABDISPAYGRPS",.DISPARR)
  ;"Example:  
  ;"DISPARR
  ;"}~1 = Basic
  ;"}~2 = Rheum
  ;"}~3 = Cardio
  ;"}~4 = Renal
  ;"}~5 = GI
  ;"}~6 = Metab
  ;"}~B
  ;"  }~Basic
  ;"  | }~1 = ""
  ;"  }~Cardio
  ;"  | }~3 = ""
  ;"  }~GI                   
  ;"  | }~5 = ""
  ;"  }~Metab
  ;"  | }~6 = ""
  ;"  }~Renal
  ;"  | }~4 = ""
  ;"  }~Rheum
  ;"    }~2 = ""
  ;"  
  ;"-----
  NEW TEMP MERGE TEMP=INIARR("BUNDLE")
  NEW BUNDLES DO PARSEELEMENT(.TEMP,"^",.BUNDLES)
  NEW ABUNDLE SET ABUNDLE=""
  FOR  SET ABUNDLE=$ORDER(BUNDLES(ABUNDLE)) QUIT:ABUNDLE=""  DO
  . NEW IDX SET IDX=0
  . FOR  SET IDX=$ORDER(BUNDLES(ABUNDLE,IDX)) QUIT:IDX'>0  DO
  . . NEW APROC SET APROC=$GET(BUNDLES(ABUNDLE,IDX)) QUIT:APROC=""
  . . NEW JDX SET JDX=+$ORDER(PROCARR("B",APROC,0)) ;"QUIT:JDX'>0
  . . SET BUNDLES(ABUNDLE,IDX,"IDX")=JDX
  ;"Example:
  ;"BUNDLES
  ;"}~AnnualDiabetes
  ;"| }~1 = B12
  ;"| | }~"IDX" = 3
  ;"| }~2 = CBC-Platelet With Diff.
  ;"| | }~"IDX" = 6
  ;"| }~3 = CMP
  ;"| | }~"IDX" = 11
  ;"| }~4 = Lipids
  ;"| | }~"IDX" = 13
  ;"| }~5 = HgbA1c
  ;"| | }~"IDX" = 14
  ;"| }~6 = Urine Microalbumin/Creatinine
  ;"| | }~"IDX" = 15
  ;"| }~7 = TSH
  ;"| | }~"IDX" = 24
  ;"| }~8 = [D] UA w/ micro - Reflex To Culture
  ;"|   }~"IDX" = 30
  ;"}~Diabetes
  ;"| }~1 = CBC-Platelet With Diff.
  ;"| | }~"IDX" = 6
  ;"| }~2 = CMP
  ;"| | }~"IDX" = 11
  ;"| }~3 = HgbA1c
  ;"|   }~"IDX" = 14
  ;"
  ;"===============
  DO ENSUREARR(.DXARR,"I")      ;"Ensure Dx entries are put into FM file
  DO ENSUREARR(.PROCARR,"L")  ;"Ensure Proc entries are put into FM file
  DO ENSUREARR(.BUNDLEGRP,"L") ;"Ensure Bundle groups entries are put into FM file
  DO ENSUREARR(.DISPARR,"P")   ;"Ensure Display groups entries are put into FM file
  ;
  NEW PROCGRPIEN SET PROCGRPIEN=$$ENSUREGROUP(.PROCARR,"TMG LAB ORDER GROUP PROCS","L") ;"Ensure GROUP is created (with ARR elements being put into ITEMS subfile)  
  NEW DXGRPIEN   SET DXGRPIEN=$$ENSUREGROUP(.DXARR,"TMG LAB ORDER GROUP DXS","I")     ;"Ensure GROUP is created (with ARR elements being put into ITEMS subfile)  
  NEW DSPGRPIEN  SET DSPGRPIEN=$$ENSUREGROUP(.DISPARR,"TMG LAB ORDER GROUP DISPLAY PAGES","P")   ;"Ensure GROUP is created (with ARR elements being put into ITEMS subfile)  
  ;
  NEW TOPARR,TOPIDX SET TOPIDX=1
  SET TOPARR(TOPIDX,"IEN")=DXGRPIEN,TOPIDX=TOPIDX+1   
  SET TOPARR(TOPIDX,"IEN")=PROCGRPIEN,TOPIDX=TOPIDX+1
  SET TOPARR(TOPIDX,"IEN")=DSPGRPIEN,TOPIDX=TOPIDX+1
  ;
  ;"The bundles group stuff is slightly more complicated, do here.
  SET ABUNDLE=""
  FOR  SET ABUNDLE=$ORDER(BUNDLES(ABUNDLE)) QUIT:ABUNDLE=""  DO
  . NEW TEMPARR
  . NEW IDX SET IDX=0
  . FOR  SET IDX=$ORDER(BUNDLES(ABUNDLE,IDX)) QUIT:IDX'>0  DO
  . . NEW APROC SET APROC=$GET(BUNDLES(ABUNDLE,IDX)) QUIT:APROC=""
  . . NEW JDX SET JDX=$GET(BUNDLES(ABUNDLE,IDX,"IDX")) QUIT:JDX'>0
  . . NEW IEN SET IEN=$GET(PROCARR(JDX,"IEN")) QUIT:IEN'>0
  . . SET TEMPARR=ABUNDLE
  . . SET TEMPARR(IDX)=APROC
  . . SET TEMPARR(IDX,"IEN")=IEN
  . NEW BNDGRPIEN  SET BNDGRPIEN=$$ENSUREGROUP(.TEMPARR,"TMG LAB ORDER GROUP BUNDLE "_ABUNDLE,"B")   ;"Ensure GROUP is created (with ARR elements being put into ITEMS subfile)
  . SET TOPARR(TOPIDX,"IEN")=BNDGRPIEN,TOPIDX=TOPIDX+1  
  ;
  ;"Finally, but all groups into top level dialog item entry
  DO ENSUREGROUP(.TOPARR,"TMG LAB ORDER DIALOG","D")   ;"Ensure GROUP is created (with ARR elements being put into ITEMS subfile)
  QUIT;
  ;  
ENSUREGROUP(ARR,NAME,TYPE) ;"Ensure GROUP is created (with ARR elements being put into ITEMS subfile)  
  ;"INPUT: ARR -- the array with info from INI file, with added IEN's. PASS BY REFERENCE
  ;"         ARR
  ;"         }~1 = Anemia - D64.9
  ;"         | }~"IEN" = 7
  ;"         }~2 = Low B12 - E53.8
  ;"         | }~"IEN" = 23
  ;"         }~3 = Low Vit-D - E55.9
  ;"         | }~"IEN" = 56
  ;"         }~4 = DM-2 - E11.9
  ;"         | }~"IEN" = 17
  ;"         }~5 = DM-2 Uncontrolled - E11.65
  ;"         | }~"IEN" = 6
  ;"       NAME -- value for .01 field, the name of the GROUP
  ;"       TYPE -- value for .02 field, the type of the group
  ;"Output: FM file is changed, and any errors are written to consolt
  NEW IEN SET IEN=+$$ENSUREREC(NAME,TYPE)
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:IDX'>0  DO
  . NEW ANIEN SET ANIEN=$GET(ARR(IDX,"IEN")) QUIT:ANIEN=""
  . NEW SUBIEN SET SUBIEN=+$$ENSURESUBREC(ANIEN,IEN)
  QUIT IEN
  ;
ENSUREARR(ARR,TYPE)  ;"Ensure entrIes are found in FM file 22751 TMG LAB ORDER DIALOG ELEMENTS  
  ;"INPUT: ARR -- the array with info from INI file. PASS BY REFERENCE
  ;"         Example:
  ;"         ARR
  ;"         }~1 = Anemia - D64.9
  ;"         }~2 = Low B12 - E53.8
  ;"         }~3 = Low Vit-D - E55.9
  ;"         }~4 = DM-2 - E11.9
  ;"         }~5 = DM-2 Uncontrolled - E11.65
  ;"       TYPE -- type of entry, e.g. L or D etc
  ;"Output -- ARR is modified as follows
  ;"         ARR
  ;"         }~1 = Anemia - D64.9
  ;"         | }~"IEN" = 7
  ;"         }~2 = Low B12 - E53.8
  ;"         | }~"IEN" = 23
  ;"         }~3 = Low Vit-D - E55.9
  ;"         | }~"IEN" = 56
  ;"         }~4 = DM-2 - E11.9
  ;"         | }~"IEN" = 17
  ;"         }~5 = DM-2 Uncontrolled - E11.65
  ;"         | }~"IEN" = 6
  ;"      FM file is changed, and any errors are written to consolt
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:IDX'>0  DO
  . NEW ANENTRY SET ANENTRY=$GET(ARR(IDX)) QUIT:ANENTRY=""
  . NEW IEN SET IEN=+$$ENSUREREC(ANENTRY,TYPE)
  . SET ARR(IDX,"IEN")=IEN
  QUIT;
  ;
GETIEN(NAME) ;"Search file 22751 for .01 = NAME
  NEW TRIMNAME SET TRIMNAME=$EXTRACT(NAME,1,30)
  NEW IEN SET IEN=0
  NEW ANIEN SET ANIEN=0
  FOR  SET ANIEN=$ORDER(^TMG(22751,"B",TRIMNAME,ANIEN)) QUIT:(ANIEN'>0)!(IEN>0)  DO
  . NEW ZN SET ZN=$GET(^TMG(22751,ANIEN,0))
  . IF $PIECE(ZN,"^",1)'=NAME QUIT
  . SET IEN=ANIEN
  QUIT IEN
  ;
ENSUREREC(NAME,TYPE)  ;"Ensure rec in file 22751 with NAME and TYPE
  ;"Input: NAME -- value for .01 field
  ;"       TYPE -- value for .02 field
  ;"Result: Returns IEN or 0 if problem
  ;"Output: FM file is changed, and any errors are written to consolt
  NEW IEN SET IEN=$$GETIEN(NAME)
  IF IEN>0 GOTO ER2
  NEW TMGFDA,TMGIEN,TMGMSG
  SET TMGFDA(22751,"+1,",.01)=NAME
  DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO ERDN
  . WRITE !,$$GETERRST^TMGDEBU2(.TMGMSG),!
  . SET IEN=0
  SET IEN=$GET(TMGIEN(1))
  IF IEN'>0 DO  GOTO ERDN
  . WRITE !,"Unable to locate IEN of added record",!    
ER2  ;
  KILL TMGFDA,TMGIEN,TMGMSG
  SET TMGFDA(22751,IEN_",",.02)=TYPE
  DO FILE^DIE("E","TMGFDA","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO ERDN
  . WRITE !,$$GETERRST^TMGDEBU2(.TMGMSG),!
ERDN
  QUIT IEN
  ;
ENSURESUBREC(IEN,PARENTIEN) ;"Ensure entry in ITEMS subfile inside 22751
  ;"Input: IEN -- value for .01 pointer field for subfile ITEMS
  ;"       PARENTIEN -- IEN for top level record
  ;"Result: Returns SUBFILE IEN or 0 if problem
  ;"Output: FM file is changed, and any errors are written to consolt
  NEW SUBIEN SET SUBIEN=$ORDER(^TMG(22751,PARENTIEN,1,"B",IEN,0))
  IF SUBIEN>0 GOTO ESR2
  NEW TMGFDA,TMGIEN,TMGMSG
  SET TMGFDA(22751.01,"+1,"_PARENTIEN_",",.01)="`"_IEN
  DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")    
  IF $DATA(TMGMSG("DIERR")) DO  GOTO ERDN
  . WRITE !,$$GETERRST^TMGDEBU2(.TMGMSG),!
  . SET SUBIEN=0
  SET SUBIEN=$GET(TMGIEN(1))
  IF SUBIEN'>0 DO  GOTO ERDN
  . WRITE !,"Unable to locate IEN of added record",!    
ESR2 ;
  QUIT SUBIEN
  ;
DATA2ARR(TAG,OUTARR) ;"Load TEXT data from this file into array
  NEW OFFSET SET OFFSET=0
  NEW LINE SET LINE=""
  ;
  FOR  DO  QUIT:(LINE["<END>")!(LINE="")
  . SET OFFSET=OFFSET+1
  . SET LINE=$PIECE($TEXT(@TAG+OFFSET),";;",2)
  . IF LINE["<END>" QUIT
  . NEW DATA SET DATA=$PIECE(LINE,"^",2,999)
  . SET LINE=$PIECE(LINE,"^",1)
  . SET OUTARR(OFFSET)=LINE
  . SET OUTARR("B",LINE,OFFSET)=""
  . IF DATA'="" DO
  . . SET OUTARR(OFFSET,"DATA")=DATA
  . . NEW JDX FOR JDX=1:1:$LENGTH(DATA,"^") DO
  . . . NEW ADATA SET ADATA=$PIECE(DATA,"^",JDX) QUIT:ADATA=""
  . . . SET OUTARR(OFFSET,"DATA",JDX)=ADATA
  QUIT;
  ;
PARSEELEMENT(ARR,DELIM,OUT) ;"parse elements of array into sub elements
  NEW NAME SET NAME=$GET(ARR,"UNKNONW")
  NEW IDX SET IDX=""
  FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:IDX=""  DO
  . NEW LINE SET LINE=$GET(ARR(IDX))  QUIT:LINE=""
  . NEW JDX FOR JDX=1:1:$LENGTH(LINE,DELIM) DO
  . . NEW ONEVALUE SET ONEVALUE=$PIECE(LINE,DELIM,JDX) QUIT:ONEVALUE=""
  . . SET OUT(IDX,JDX)=ONEVALUE
  QUIT
  ;
LABDISPAYGRPS ; "note: The items these groups contain will have to be edited manually via Fileman
  ;;Basic
  ;;Rheum
  ;;Cardio
  ;;Renal
  ;;GI
  ;;Metab
  ;;<END>
  ;
