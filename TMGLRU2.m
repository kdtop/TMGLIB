TMGLRU2 ;TMG/kst-Utility for managing lab order dialog ;12/18/22
              ;;1.0;TMG-LIB;**1**;12/18/22
 ;
 ;"TMG LAB ORDER DIALOG UTILITY2
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 12/18/22  Kevin S. Toppenberg MD
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
 ;"REFRESH() --Delete prior entries and recompile
 ;"COMPILE()  -- Complile from file TMG LAB ORDER DIALOG ELEMENTS (22751) to ORDER DIALOG (101.41)
 ;"KILLRANGE(RANGE) -- Delete all records with SEQ in RANGE  
 ;"KILLALL ;
 ;
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;"ENSUREBUNDLE(TOPIEN,ARR,RANGE)  ;
 ;"ENSURERECS(TOPIEN,ARR,RANGE,TYPE)  ;
 ;"ENSURE1(TOPIEN,LINE,TYPE,ITEMSARR) ;
 ;"PREPVARS(LINE,RESULT,IEN22751,NAME,ATYPE,FASTING) ;
 ;"ENSUREDATA(TOPIEN,SUBIEN,LINE,ITEMSARR) ;
 ;"GETDATASTR(TOPIEN,LINE,ITEMSARR) ;
 ;"SETDATA(TOPIEN,SUBIEN,DATASTR) ;
 ;"MAKEREC(TOPIEN,LINE,NAME,ITEMSARR,INFO) ;
 ;"GETIEN101D412(TOPIEN,NAME,TYPE) --Search file 101.412 for FLD 4 contains NAME
 ;"GETIEN(NAME) --Search file 101.41 for .01 = NAME
 ;"LASTSEQ(RANGE) --Return last used SEQ in RANGE
 ;"SAFENAME(NAME) --Remove and replace disallowed chars
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"Data structures:
 ;"File 22751 has this structure
 ;"     TMG LAB ORDER DIALOG ELEMENTS                                 
 ;"  REF  NODE;PIECE     FLD NUM  FIELD NAME    
 ;"  ===================================================================
 ;"    1  0;1                .01  NAME                          [RFJ64]
 ;"    2  0;2                .02  TYPE                              [S]
 ;"       1;0                  1  ITEMS              <-Mult [22751.01P]
 ;"    3   -0;1              .01   -ITEMS             <-Pntr  [P22751']
 ;"    4   -0;2              .02   -SEQUENCE                    [NJ9,4]
 ;"    5  10;1                10  FASTING                           [S]
 ;"   <> <> <>
 ;"TYPE is a set as follows
 ;"    D:DIALOG
 ;"    L:LAB/PROCEDURE
 ;"    B:BUNDLE
 ;"    I:ICD DX
 ;"    P:PAGE GROUP
 ;"    O:ORDERING PROVIDER
 ;"    T:LAB TIME
 ;"    F:ORDER FLAG
 ;"    N:ORDER OPTIONS
 ;"    X:ITEM DATA            
 ;"    E:TEXT
 ;"    W:WP FIELD
 ;"
 ;"Thus ALL data elements are stored as individual records.  
 ;"Those elements that are logically a child of a parent, will be linked to from a parent.  
 ;"E.g. 
 ;"      PARENT RECORD #233
 ;"        ITEMS: CHILD1  #234
 ;"               CHILD2  #235
 ;"    
 ;"A top-level record was created, to tie everything together -- as below:
 ;"
 ;"  Showing FILE: TMG LAB ORDER DIALOG ELEMENTS (#22751)
 ;"   Entry #88,
 ;"       .01-NAME : TMG LAB ORDER DIALOG
 ;"       .02-TYPE : DIALOG                            
 ;"         1-ITEMS : <MULTIPLE-VALUED>
 ;"            Entry #1
 ;"               .01-ITEMS : TMG LAB ORDER GROUP DXS (`81 in #22751)                               <-- type: ICD DX
 ;"            Entry #2                                                                            
 ;"               .01-ITEMS : TMG LAB ORDER GROUP PROCS (`80 in #22751)                             <-- type: LAB/PROCEDURE
 ;"            Entry #3                                                                            
 ;"               .01-ITEMS : TMG LAB ORDER GROUP DISPLAY PAGES (`82 in #22751)                     <-- type: PAGE GROUP
 ;"            Entry #4                                                                          
 ;"               .01-ITEMS : TMG LAB ORDER GROUP BUNDLE AnnualDiabetes (`83 in #22751)             <-- type: BUNDLE
 ;"            Entry #5                                                                            
 ;"               .01-ITEMS : TMG LAB ORDER GROUP BUNDLE Diabetes (`84 in #22751)                   <-- type: BUNDLE
 ;"            Entry #6                                                                          
 ;"               .01-ITEMS : TMG LAB ORDER GROUP BUNDLE Screening (`85 in #22751)                  <-- type: BUNDLE
 ;"            Entry #7                                                                          
 ;"               .01-ITEMS : TMG LAB ORDER GROUP BUNDLE ScreeningWVitamins (`86 in #22751)         <-- type: BUNDLE
 ;"            Entry #8                                                                             
 ;"               .01-ITEMS : TMG LAB ORDER GROUP BUNDLE ScreeningWVits-UA (`87 in #22751)          <-- type: BUNDLE
 ;"            Entry #9                                                                          
 ;"               .01-ITEMS : TMG LAB ORDER LIST ORDERING PROVIDER (`93 in #22751)                  <-- type: ORDERING PROVIDER
 ;"            Entry #10                                                                           
 ;"               .01-ITEMS : TMG LAB ORDER LIST ORDER TIMING (`94 in #22751)                       <-- type: LAB TIME
 ;"            Entry #11                                                                           
 ;"               .01-ITEMS : TMG LAB ORDER LIST ORDER FLAGS (`107 in #22751)                       <-- type: ORDER FLAG
 ;"            Entry #12                                                                           
 ;"               .01-ITEMS : TMG LAB ORDER LIST ORDER OPTIONS (`108 in #22751)                     <-- type: ORDER OPTIONS
 ;"            Entry #13
 ;"               .01-ITEMS : TMG LAB ORDER FREE TEXT DX 1 (`109 in #22751)  <-- no children items  <-- type: TEXT
 ;"            Entry #14
 ;"               .01-ITEMS : TMG LAB ORDER FREE TEXT DX 2 (`110 in #22751) <-- no children items   <-- type: TEXT
 ;"            Entry #15
 ;"               .01-ITEMS : TMG LAB ORDER FREE TEXT DX 3 (`111 in #22751) <-- no children items   <-- type: TEXT
 ;"            Entry #16
 ;"               .01-ITEMS : TMG LAB ORDER FREE TEXT DX 4 (`112 in #22751) <-- no children items   <-- type: TEXT
 ;"            Entry #17
 ;"               .01-ITEMS : TMG LAB ORDER COMMENTS (`113 in #22751) <-- no children items         <-- type: WP FIELD
 ;"            Entry #18
 ;"               .01-ITEMS : TMG LAB ORDER FREE TEXT LAB (`114 in #22751) <-- no children items    <-- type: TEXT     
 ;"  -------------------------------
 ;"
 ;"Many (but not all) entries in the top level contains grandchildren elements, as below:
 ;"
 ;"  Showing FILE: TMG LAB ORDER DIALOG ELEMENTS (#22751)
 ;"   Entry #81,
 ;"       .01-NAME : TMG LAB ORDER GROUP DXS
 ;"       .02-TYPE : ICD DX
 ;"         1-ITEMS : <MULTIPLE-VALUED>
 ;"            Entry #1
 ;"               .01-ITEMS : Anemia - D64.9 (`2 in #22751)
 ;"            Entry #2
 ;"               .01-ITEMS : Low B12 - E53.8 (`3 in #22751)
 ;"            Entry #3
 ;"               .01-ITEMS : Low Vit-D - E55.9 (`4 in #22751)
 ;"            Entry #4
 ;"               .01-ITEMS : DM-2 - E11.9 (`5 in #22751)
 ;"            (more)...  
 ;"
 ;"When compiling this into file 101.41, so that it can be used in CPRS, the
 ;"  data is added into 101.41 record 'TMG LAB ORDER', as below
 ;"                                               
 ;"  Showing FILE: ORDER DIALOG (#101.41)
 ;"   Entry #16021,
 ;"       .01-NAME : TMG LAB ORDER
 ;"         2-DISPLAY TEXT : TMG Lab Order
 ;"         4-TYPE : dialog
 ;"         5-DISPLAY GROUP : LABORATORY (`5 in #100.98)
 ;"         6-SIGNATURE REQUIRED : ORES
 ;"         7-PACKAGE : ORDER ENTRY/RESULTS REPORTING (`35 in #9.4)
 ;"         9-ASK FOR ANOTHER ORDER : NO
 ;"        10-ITEMS : <MULTIPLE-VALUED>  <-- SUBFILE #101.412       
 ;"            Entry #1
 ;"               .01-SEQUENCE : 1.002
 ;"                 2-ITEM : TMG LAB ORDER Y/N (`16022 in #101.41)
 ;"                 4-DISPLAY TEXT : OFFICE Urine Drug Screen
 ;"                11-HELP MESSAGE : ~IEN=15;TYPE=L;
 ;"            Entry #2
 ;"               .01-SEQUENCE : 1.003
 ;"                 2-ITEM : TMG LAB ORDER Y/N (`16022 in #101.41)
 ;"                 4-DISPLAY TEXT : OFFICE Check BP today
 ;"                11-HELP MESSAGE : ~IEN=16;TYPE=L;
 ;"            Entry #3
 ;"               .01-SEQUENCE : 1.004
 ;"                 2-ITEM : TMG LAB ORDER Y/N (`16022 in #101.41) 
 ;"            (more)... 
 ;"
 ;"Notice: All elements of the dialog, i.e. each entry in the ITEMS sufile, 
 ;"          have to be matched to another element from 101.41, depending
 ;"          on the type of the element from 22751.  For example, a CPRS order dialog 
 ;"          checkbox entrywill be matched to 101.41 record 'TMG LAB ORDER Y/N', 
 ;"          which is defined to be of type Yes/No.
 ;"        All elements are put into subfile ITEMS (#101.412).  There is no
 ;"          ability to put sub-sub-items here, so it is all flat, on one level.  
 ;"        To get around this, I am overloading the data fields in a way that will
 ;"          be interpreted in CPRS.  This extra information is stored in field#11 ('HELP MESSAGE'),
 ;"          with the format as follows:
 ;"          -- First character must be '~' for text to be processed as DATA
 ;"          -- Elements are delimited by ';'
 ;"          -- Defined Element terms as follows:
 ;"             - IEN      e.g. IEN=123              -- This is IEN from file #22751
 ;"             - TYPE     e.g. TYPE=P               -- Allowed types are:
 ;"                                                      D for DIALOG
 ;"                                                      L for LAB/PROCEDURE
 ;"                                                      B for BUNDLE;
 ;"                                                      I for ICD DX;
 ;"                                                      P for PAGE GROUP
 ;"                                                      O for ORDERING PROVIDER
 ;"                                                      T for LAB TIME
 ;"                                                      F for ORDER FLAGS
 ;"                                                      N for ORDER OPTIONS
 ;"                                                      X for ITEM DATA
 ;"                                                      E for Free text fields 
 ;"                                                      W for WordProcessor fields 
 ;"             - ITEMS    e.g. ITEMS=54,28,194,73   -- Used by types with children, a way of defining elements in list
 ;"             - FASTING  e.g. FASTING=1            -- Used by lab/proc elements to show that default for labs should be fasting
 ;"        The matching of types in file 22751 to type elements in 101.41 are as follows: 
 
 ;"           L --> TMG LAB ORDER Y/N  
 ;"           I --> TMG LAB COMMON DX ENTRY
 ;"           P --> TMG LAB DISPLAY GROUP
 ;"           B --> TMG LAB BUNDLE ENTRY
 ;"           O --> TMG LAB LISTBOX GROUP
 ;"           T --> TMG LAB LISTBOX GROUP    
 ;"           F --> TMG LAB ORDER Y/N  
 ;"           N --> TMG LAB ORDER Y/N    
 ;"           X --> TMG LAB ITEM DATA
 ;"           E --> TMG LAB TEXT FIELD 
 ;"           W --> TMG LAB WP FIELD 
 ;"         
 ;"        The SEQUENCE information is not used in my order dialog in CPRS.  But
 ;"        I am putting different types of data into different ranges of seq#'s, based on order in TMG LAB ORDER DIALOG
 ;"          Top level items are in 1+, and any children elements are put into 800+
 ;"          NOTE: If there are more than a 99 top-level entries, this will be a problem.
 ;"        Examples of entries....
 ;"                ...
 ;"                Entry #54
 ;"                  .01-SEQUENCE : 2.002
 ;"                    2-ITEM : TMG LAB COMMON DX ENTRY (`16024 in #101.41)
 ;"                    4-DISPLAY TEXT : Anemia [!DS] D64.9
 ;"                   11-HELP MESSAGE : ~IEN=2;TYPE=I;
 ;"               Entry #55
 ;"                  .01-SEQUENCE : 2.003
 ;"                    2-ITEM : TMG LAB COMMON DX ENTRY (`16024 in #101.41)
 ;"                    4-DISPLAY TEXT : Low B12 [!DS] E53.8
 ;"                   11-HELP MESSAGE : ~IEN=3;TYPE=I;
 ;"               ...
 ;"               Entry #72
 ;"                   .01-SEQUENCE : 3.006
 ;"                     2-ITEM : TMG LAB DISPLAY GROUP (`16026 in #101.41)
 ;"                     4-DISPLAY TEXT : GI
 ;"                    11-HELP MESSAGE : ~IEN=79;TYPE=P;ITEMS=57,46,20;
 ;"                Entry #73
 ;"                   .01-SEQUENCE : 3.007
 ;"                     2-ITEM : TMG LAB DISPLAY GROUP (`16026 in #101.41)
 ;"                     4-DISPLAY TEXT : Metab
 ;"                    11-HELP MESSAGE : ~IEN=74;TYPE=P;ITEMS=28,33,38,53,52,26,43,25,17,18,19,39,40,41,44,45;
 ;"                ...
 ;"                Entry #76
 ;"                  .01-SEQUENCE : 4.003
 ;"                    2-ITEM : TMG LAB BUNDLE ENTRY (`16027 in #101.41)
 ;"                    4-DISPLAY TEXT : Diabetes
 ;"                   11-HELP MESSAGE : ~IEN=84;TYPE=B;ITEMS=20,25,28;
 ;"               Entry #77
 ;"                  .01-SEQUENCE : 4.004
 ;"                    2-ITEM : TMG LAB BUNDLE ENTRY (`16027 in #101.41)
 ;"                    4-DISPLAY TEXT : Screening
 ;"                   11-HELP MESSAGE : ~IEN=85;TYPE=B;ITEMS=20,25,27,38;
 ;
REFRESH() ;"Delete prior entries and recompile
  DO KILLALL 
  DO COMPILE 
  QUIT
  ;
COMPILE()  ;"Complile from file TMG LAB ORDER DIALOG ELEMENTS (22751) to ORDER DIALOG (101.41)
  NEW ARR,IEN,TOPIEN SET TOPIEN=$$GETIEN("TMG LAB ORDER")  ;"Search file 101.41  
  NEW INFO DO SETUPINFO(.INFO)  ;"NOTE: This will be used in global scope in this ROUTINE
  NEW ALLARR DO RPTARR^TMGLRU1("TMG LAB ORDER DIALOG",.ALLARR)
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(ALLARR(1,IDX)) QUIT:IDX'>0  DO
  . NEW ARR MERGE ARR=ALLARR(1,IDX) QUIT:($GET(ARR)="")
  . DO FIXNAME(.ARR) 
  . DO ENSURERECS(TOPIEN,.ARR,IDX,.INFO) 
  QUIT
  ;
FIXNAME(ARR) ;
  NEW LINE SET LINE=$GET(ARR) QUIT:LINE=""
  NEW NAME SET NAME=$PIECE(LINE,"^",2)
  IF NAME["TMG LAB ORDER " SET NAME=$PIECE(NAME,"TMG LAB ORDER ",2)
  IF $PIECE(NAME," ",1)="LIST" DO
  . SET NAME=$PIECE(NAME,"LIST ",2)
  ELSE  IF $PIECE(NAME," ",1)="GROUP" DO
  . IF $PIECE(NAME," ",1,2)="GROUP BUNDLE" SET NAME=$PIECE(NAME,"GROUP BUNDLE ",2) QUIT
  . SET NAME=$PIECE(NAME,"GROUP ",2) QUIT
  SET $PIECE(LINE,"^",2)=NAME
  SET ARR=LINE
  QUIT
  ;
ENSURERECS(TOPIEN,ARR,RANGE,INFO)  ;
  ;"Input: TOPIEN -- IEN in 101.41 (ORDER DIALOG)
  ;"       ARR -- array (as generated by REPORTONE^TMGLRU1.  E.g.  
  ;"         ARR=80^TMG LAB ORDER GROUP PROCS^L    <-- parent element
  ;"         }~1 = 15^OFFICE Urine Drug Screen^L    <-- children ITEMS elements
  ;"         }~2 = 16^OFFICE Check BP today^L
  ;"         }~3 = 17^B12^L
  ;"         }~4 = 18^Vitamin D Level (82306)^L
  ;"         }~5 = 19^Folic Acid^L   ...
  ;"       Range: A number specifying sequence to put parent element into
  ;"               All children elements will be put into 2+ sequence range
  ;"       INFO: Array containing Type map and IEN map
  ;"RESULTS: None -- ERRORS are reported to console
  SET INFO("LASTSEQ")=$$LASTSEQ(RANGE) ;"Return last used SEQ in RANGE
  NEW LINE SET LINE=$GET(ARR) QUIT:LINE=""
  NEW TEMP SET TEMP=0
  ;"Ensure parent element is present or added. Children are listed, but not added to database
  IF $$SAVEPARENT(.ARR) SET TEMP=$$ENSURE1(TOPIEN,LINE,.ARR,.INFO) 
  ;"Next, ensure child element is present or added, can be done recursively if grandchildren found
  IF +TEMP<0 WRITE $PIECE(TEMP,"^",2,99),!  
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:IDX'>0  DO
  . NEW LINE SET LINE=$GET(ARR(IDX)) QUIT:LINE=""
  . NEW ITEMSARR MERGE ITEMSARR=ARR(IDX)
  . DO ENSURERECS(TOPIEN,.ITEMSARR,800,.INFO)
  QUIT
  ;  
ENSURE1(TOPIEN,LINE,ITEMSARR,INFO) ;
  ;"RESULT: 1^OK, OR IEN, or -1^ErrorMessage
  NEW RESULT,NAME,TYPE,IEN22751
  DO PREPVARS(LINE,.RESULT,.IEN22751,.NAME,.TYPE)
  NEW SUBIEN SET SUBIEN=+$GET(INFO("IENMAP",IEN22751))
  IF SUBIEN>0 GOTO E1P1  ;"Already exists...   
  SET SUBIEN=$$GETIEN101D412(TOPIEN,NAME,TYPE)
  IF SUBIEN>0 GOTO E1P1  ;"Already exists...   
  SET RESULT=$$MAKEREC(TOPIEN,LINE,NAME,.ITEMSARR,.INFO) 
  GOTO E1PDN
E1P1 ; 
  SET RESULT="1^"_SUBIEN
  DO ENSUREDATA(TOPIEN,SUBIEN,LINE,.ITEMSARR) 
E1PDN ;  
  QUIT RESULT
  ; 
SAVEPARENT(ARR)  ;"Return if parent should be saved as record
  NEW RESULT SET RESULT=1
  IF $PIECE($GET(ARR),"^",2)="DISPLAY PAGES" SET RESULT=0
  QUIT RESULT;
  ;
PREPVARS(LINE,RESULT,IEN22751,NAME,TYPE,FASTING) ;
  SET RESULT="1^OK"
  SET IEN22751=$PIECE(LINE,"^",1)
  SET NAME=$$SAFENAME($PIECE(LINE,"^",2)) ;"Remove and replace disallowed chars
  SET TYPE=$PIECE(LINE,"^",3)
  SET FASTING=($PIECE(LINE,"^",4)="Y")
  QUIT
  ;
ENSUREDATA(TOPIEN,SUBIEN,LINE,ITEMSARR) ;
  NEW RESULT SET RESULT="1^OK"
  NEW DATASTR SET DATASTR=$$GETDATASTR(TOPIEN,LINE,.ITEMSARR)
  NEW N1 SET N1=$PIECE($GET(^ORD(101.41,TOPIEN,10,SUBIEN,1)),"^",1)
  IF N1'=DATASTR SET RESULT=$$SETDATA(TOPIEN,SUBIEN,DATASTR)
  QUIT RESULT
  ;
GETDATASTR(TOPIEN,LINE,ITEMSARR) ;
  NEW IEN22751,NAME,TYPE,FASTING
  DO PREPVARS(LINE,.RESULT,.IEN22751,.NAME,.TYPE,.FASTING) 
  SET FASTING=+$GET(FASTING)
  NEW IENLIST SET IENLIST=""
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(ITEMSARR(IDX)) QUIT:IDX'>0  DO
  . NEW ENTRY SET ENTRY=$GET(ITEMSARR(IDX)) QUIT:ENTRY=""
  . IF IENLIST'="" SET IENLIST=IENLIST_","
  . SET IENLIST=IENLIST_+ENTRY
  NEW DATA SET DATA="~IEN="_IEN22751_";"
  SET DATA=DATA_"TYPE="_TYPE_";"
  IF FASTING SET DATA=DATA_"FASTING=1;"
  IF IENLIST]"" SET DATA=DATA_"ITEMS="_IENLIST_";"
  QUIT DATA
  ;
SETDATA(TOPIEN,SUBIEN,DATASTR) ;
  ;"RESULT: 1^OK, 1^SUBIEN, OR -1^ErrorMessage
  NEW RESULT SET RESULT="1^OK"
  NEW TMGFDA,TMGMSG
  SET TMGFDA(101.412,SUBIEN_","_TOPIEN_",",11)=DATASTR
  DO FILE^DIE("E","TMGFDA","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO    
  . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  QUIT RESULT
  ;
MAKEREC(TOPIEN,LINE,NAME,ITEMSARR,INFO) ;
  ;"INFO used in GLOBAL SCOPE. DEFINED IN COMPILE()
  ;"Results: IEN, or -1^Error
  ;"NOTE: Items in ITEMSARR will have their pointers put into field 11, but
  ;"        they are NOT added themselves here. ALSO, FYI, their pointers are  
  ;"        IEN222751, NOT an IEN101.41 -- CPRS code will handle this.  
  ;"      The ITEMSARR elements will be added (via this code) in recursive call from above. 
  ;"      This function assumes a check has already been made to ensure records not already added.
  NEW RESULT SET RESULT="1^OK"
  NEW TMGFDA,TMGIEN,TMGMSG,IEN22751,TYPE
  DO PREPVARS(LINE,.RESULT,.IEN22751,.NAME,.TYPE)
  NEW ITEMNAME SET ITEMNAME=$GET(INFO("TYPES",TYPE))
  IF ITEMNAME="" SET RESULT="-1^Can't find ITEMNAME for type: ["_TYPE_"]" GOTO MRDN
  ;"CREATE RECORD
  NEW SEQ SET SEQ=$GET(INFO("LASTSEQ"))+0.001
  NEW IENS SET IENS="+1,"_TOPIEN_","
  SET TMGFDA(101.412,IENS,.01)=SEQ
  SET TMGFDA(101.412,IENS,2)=ITEMNAME
  SET TMGFDA(101.412,IENS,4)=NAME
  SET TMGFDA(101.412,IENS,11)=$$GETDATASTR(TOPIEN,LINE,.ITEMSARR)
  DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  GOTO MRDN  
  . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  SET SUBIEN=+$GET(TMGIEN(1))
  SET INFO("LASTSEQ")=SEQ
  SET INFO("SEQ",SEQ)=NAME_"^"_ITEMNAME
  SET INFO("IENMAP",IEN22751)=SUBIEN
  IF SUBIEN>0 SET RESULT=SUBIEN
  ELSE  SET RESULT="-1^Unable to find IEN of added record"
MRDN  
  QUIT RESULT
  ;
GETIEN101D412(TOPIEN,NAME,TYPE) ;"Search file 101.412 for FLD 4 contains NAME
  ;"TOPIEN -- IEN101.41
  ;"NAME -- name to search for
  NEW TRIMNAME SET TRIMNAME=$EXTRACT(NAME,1,30)
  NEW SUBIEN SET SUBIEN=0
  NEW ANIEN SET ANIEN=0
  FOR  SET ANIEN=$ORDER(^ORD(101.41,TOPIEN,10,ANIEN)) QUIT:(ANIEN'>0)!(SUBIEN>0)  DO
  . NEW ZN SET ZN=$GET(^ORD(101.41,TOPIEN,10,ANIEN,0))
  . NEW DATA SET DATA=$PIECE(ZN,"^",4)
  . IF DATA="" QUIT
  . IF DATA'=NAME QUIT
  . NEW N1 SET N1=$GET(^ORD(101.41,TOPIEN,10,ANIEN,1))
  . NEW DATASTR SET DATASTR=$PIECE(N1,"^",1)
  . IF DATASTR'["TYPE="_TYPE QUIT
  . SET SUBIEN=ANIEN
  QUIT SUBIEN
  ;
GETIEN(NAME) ;"Search file 101.41 for .01 = NAME
  NEW TRIMNAME SET TRIMNAME=$EXTRACT(NAME,1,30)
  NEW IEN SET IEN=0
  NEW ANIEN SET ANIEN=0
  FOR  SET ANIEN=$ORDER(^ORD(101.41,"B",TRIMNAME,ANIEN)) QUIT:(ANIEN'>0)!(IEN>0)  DO
  . NEW ZN SET ZN=$GET(^ORD(101.41,ANIEN,0))
  . IF $PIECE(ZN,"^",1)'=NAME QUIT
  . SET IEN=ANIEN
  QUIT IEN
  ;
LASTSEQ(RANGE) ;"Return last used SEQ in RANGE
  ;"INPUT -- RANGE  eg. "6" for range of 6.000 to 6.999
  NEW LASTSEQ SET LASTSEQ=0
  NEW SEQ SET SEQ=(RANGE\1-0.000001)  ;"just before start of range
  NEW NEXT SET NEXT=RANGE\1+1
  FOR  SET SEQ=$ORDER(^ORD(101.41,TOPIEN,10,"B",SEQ)) QUIT:(SEQ'>0)!(SEQ'<NEXT)  DO
  . SET LASTSEQ=SEQ
  IF LASTSEQ=0 SET LASTSEQ=RANGE\1+0.001
  QUIT LASTSEQ
  ;
SAFENAME(NAME) ;"Remove and replace disallowed chars
  NEW SPEC
  SET SPEC("-")="[!DS]"
  SET SPEC(";")="[!SC]"
  SET SPEC(",")="[!CM]"
  SET SPEC("^")="[!/\]"
  SET SPEC("=")="[!EQ]"
  NEW RESULT SET RESULT=$$REPLACE^XLFSTR(NAME,.SPEC)  
  QUIT RESULT
  ;
RESTORENAME(NAME) ;"Restore replaced disallowed chars
  NEW SPEC
  SET SPEC("[!DS]")="-"
  SET SPEC("[!SC]")=";"
  SET SPEC("[!CM]")=","
  SET SPEC("[!/\]")="^"
  SET SPEC("[!EQ]")="="
  NEW RESULT SET RESULT=$$REPLACE^XLFSTR(NAME,.SPEC)  
  QUIT RESULT
  ;
KILLRANGE(RANGE) ;"Delete all records with SEQ in RANGE  
  NEW TOPIEN SET TOPIEN=$$GETIEN("TMG LAB ORDER")        ;"Search file 101.41
  NEW ANIEN SET ANIEN=0
  FOR  SET ANIEN=$ORDER(^ORD(101.41,TOPIEN,10,ANIEN)) QUIT:(ANIEN'>0)  DO
  . NEW ZN SET ZN=$GET(^ORD(101.41,TOPIEN,10,ANIEN,0))
  . NEW THISSEQ SET THISSEQ=$PIECE(ZN,"^",1)
  . IF THISSEQ\1'=RANGE\1 QUIT
  . SET DIK=$$OREF^DILF($NAME(^ORD(101.41,TOPIEN,10))),DA=ANIEN,DA(1)=TOPIEN
  . DO ^DIK
  QUIT 
  ;
KILLALL ;
  NEW RANGE FOR RANGE=1:1:1000 DO
  . DO KILLRANGE(RANGE)
  QUIT;
  ;
SETUPINFO(INFO) ;
  ;"NOTE: In addition to info below, this INFO
  ;"   will also be used to map IEN's, with format as below:
  ;"     INFO("IENMAP",IEN22751)=IEN101.412  
  ;"     INFO("LASTSEQ")=#
  SET INFO("TYPES","L")="TMG LAB ORDER Y/N"  ;"<-- name of item in 101.41 to use for storage type
  SET INFO("TYPES","I")="TMG LAB COMMON DX ENTRY"
  SET INFO("TYPES","P")="TMG LAB DISPLAY GROUP"
  SET INFO("TYPES","B")="TMG LAB BUNDLE ENTRY"
  SET INFO("TYPES","O")="TMG LAB LISTBOX GROUP"
  SET INFO("TYPES","T")="TMG LAB LISTBOX GROUP"    
  SET INFO("TYPES","F")="TMG LAB ORDER Y/N"  
  SET INFO("TYPES","N")="TMG LAB ORDER Y/N"    
  SET INFO("TYPES","X")="TMG LAB ITEM DATA"
  SET INFO("TYPES","E")="TMG LAB TEXT FIELD" 
  SET INFO("TYPES","W")="TMG LAB WP FIELD"   
  QUIT
  ;