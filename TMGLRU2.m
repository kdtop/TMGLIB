TMGLRU2 ;TMG/kst-Utility for managing lab order dialog ;12/19/2023
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
 ;"EDITORDER  --Edit lab order data
 ;
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;"FIXNAME(ARR) ;
 ;"ENSURERECS(TOPIEN,ARR,RANGE,TYPE)  ;
 ;"ENSURE1(TOPIEN,LINE,TYPE,ITEMSARR) ;
 ;"SAVEPARENT(ARR)  -- Return if parent should be saved as record
 ;"PREPVARS(LINE,RESULT,IEN22751,NAME,ATYPE,FASTING,LINKDX,DXLST,IENS101D42) ;
 ;"ENSUREDATA(TOPIEN,SUBIEN,LINE,ITEMSARR) ;
 ;"GETDATASTR(TOPIEN,LINE,ITEMSARR) ;
 ;"SETDATA(TOPIEN,SUBIEN,DATASTR) ;
 ;"MAKEREC(TOPIEN,LINE,NAME,ITEMSARR,INFO) ;
 ;"GETIEN101D412(TOPIEN,NAME,TYPE) --Search file 101.412 for FLD 4 contains NAME
 ;"GETIEN(NAME) --Search file 101.41 for .01 = NAME
 ;"LASTSEQ(RANGE) --Return last used SEQ in RANGE
 ;"SAFENAME(NAME) --Remove and replace disallowed chars
 ;"RESTORENAME(NAME) -- Restore replaced disallowed chars
 ;"SETUPINFO(INFO) 
 ;"EDITLABPROC() --Edit/manage labs/procedures
 ;"EDITDX() --Edit/manage diagnoses
 ;"EDITGRPS() --Edit/manage GROUPS (Tab Pages)
 ;"MANAGE1(IEN) --EDIT 1 RECORD
 ;"SRCHMANAGE1(TYPE)  --Search for and manage 1 record, of specified type
 ;"EDIT1REC(IEN) --edit 1 record
 ;"ROOTIEN() --Return IEN of top level record
 ;"GRPGRPIEN() --Return IEN of group of groups. 
 ;"LPGRPIEN() --Return IEN of lab/procedure group
 ;"GET1IEN(NAME) ;"
 ;"GETRECS(OUT,TYPE) --RETURN ARRAY OF RECORD WHICH MATCH TYPE OF 'TYPE'
 ;"SETUPINFO2(INFO,MODE) --Setup infor for ADDITEM,DELITEM
 ;"ADDITEM(MODE)  --ADD DX, or GROUP (TAB PAGE)
 ;"DELITEM(MODE,INFO)  --Pick and delete GROUP (TAB PAGE)
 ;
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"Documentation
 ;"=======================================================================
 
 ;"Data structures:
 ;"File: TMG LAB ORDER DIALOG ELEMENTS (#22751)                                Branch: 1
 ;"REF  NODE;PIECE     FLD NUM  FIELD NAME
 ;"===============================================================================
 ;"  1  0;1                .01  NAME                                      [RFJ64]
 ;"  2  0;2                .02  TYPE                                          [S]
 ;"     1;0                  1  ITEMS                          <-Mult [22751.01P]
 ;"  3   -0;1              .01   -ITEMS                        <-Pntr  [MP22751']
 ;"  4   -0;2              .02   -SEQUENCE                                [NJ9,4]
 ;"  5  2;1                  2  ICD DX                             <-Pntr  [P80']
 ;"  6  21;1               2.1  LINKED LAB TEST                    <-Pntr Var [V]
 ;"  7  10;1                10  FASTING                                       [S]
 ;"  8  10;2                11  NEEDS LINKED DX                               [S]
 ;"     20;0                20  ALLOWED LINKED DX'S            <-Mult [22751.02P]
 ;"  9   -0;1              .01   -ALLOWED LINKED DX'S         <-Pntr  [M*P22751']
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
 ;"            NOTE: other elements are likely to have been added over time.  
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
 ;"     ...
 ;"     22700-TMG ORDER TEXT COMPILER TAG : TMGORTX
 ;"     22701-TMG ORDER TEXT COMPILER RTN : TMGORWD1
 ;"
 ;"Notice: All elements of the dialog, i.e. each entry in the ITEMS sufile, 
 ;"          have to be matched to another element from 101.41, depending
 ;"          on the type of the element from 22751.  For example, a CPRS order dialog 
 ;"          checkbox entry will be matched to 101.41 record 'TMG LAB ORDER Y/N', 
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
 ;"             - LINKDX   e.g. LINKDX=1             -- Used by elements that need a specific linked Dx to be attached to them when ordering.
 ;"             - DX       e.g. DX=56,21,19,103      -- List of known Dx's that can be used when LINKDX=1
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
 ;"
 ;"  UPDATE: 11/2024 -- It seems that the VistA ORDER DIALOG system uses sequency positioning as part of
 ;"          element ID.  A listing of all elements are sent to CPRS based on their SEQUENCE.
 ;"          if anything is inserted HIGHER in the list (lower sequence #), then it will change
 ;"          the 'incident #' counting.  This will cause a problem if an order is later edited and
 ;"          prior saved data is reloaded for populating the dialog.  For example, a prior user
 ;"          selection of 'Fasting' could be interepreted as 'Thrombophilia panel' after reload.
 ;"          When the dialog is first saved, it is not a problem, nor is there a problem with the
 ;"          free text order narrative.  It is only with the reloading.
 ;"          Even if sequence numbers are kept the same, e.g. 800.015, and 800.016, then it
 ;"          would be possible to add another between them, e.g. 800.0155, and this would again
 ;"          shift the incident counting of all subsequent dialog elements. 
 ;"          SUGGESTED SOLUTION: All subseqently added labs MUST have a sequence number > last used SEQ#
 ;"  -------------------------------------------
 ;"        Examples of entries....
 ;"                ...
 ;"                Entry #54
 ;"                  .01-SEQUENCE : 2.002                                    <-- Sequence ordering will determine instance counting during save/load
 ;"                    2-ITEM : TMG LAB COMMON DX ENTRY (`16024 in #101.41)  <--- these can be resused.      
 ;"                    4-DISPLAY TEXT : Anemia [!DS] D64.9                   <-- this is unquie
 ;"                   11-HELP MESSAGE : ~IEN=2;TYPE=I;                       <-- this is unique
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
 ;"
 ;"During the saving of an order, after created and saved in CPRS and passed back to server
 ;"  by RPC, the order gets *compiled*.  Standard VistA calls ORDTEXT^ORCSAVE1, and 
 ;"  TMG-VistA has a custom wedge there that calls WEDGE^TMGORWD1. 
 ;"  In WEDGE^TMGORWD1, the code looks for a TMG-custom *field* in file 101.41, 
 ;"  #22700,22701, storing a CUSTOM COMPILER.
 ;"  For this TMG order dialog system, we have TMGORTX^TMGORWD1.
 ;"  This compiler code must compile the text of the order and save into the 
 ;"  ORDER TEXT field (#.1) of the subfile ORDER ACTIONS (FLD #8, subfile#100.008)
 ;"  in the ORDER file. 
 ;"
 ;"Question / Answer
 ;"-------------------
 ;"  -- In ORDER DIALOG 101.41, there is subfile ITEMS, with following example record:
 ;"               .01-SEQUENCE : 1.002
 ;"                 2-ITEM : TMG LAB ORDER Y/N (`16022 in #101.41)
 ;"                 4-DISPLAY TEXT : OFFICE Urine Drug Screen
 ;"                11-HELP MESSAGE : ~IEN=15;TYPE=L;
 ;"      ... What file does the IEN refer to in the HELP MESSAGE field?  
 ;"      ANSWER:  It is to file 22751. 
 ;"          Example. Showing FILE: TMG LAB ORDER DIALOG ELEMENTS (#22751)
 ;"                    Entry #15,
 ;"                        .01-NAME : OFFICE Urine Drug Screen
 ;"                        .02-TYPE : LAB/PROCEDURE
 ;"                   -------------------------------
 ; 
 ;"-----------------------------------------------------------------
 ;"NOTICE: Below is about how the whole system works.  It is not exactly related
 ;"        to the purpose of this file, which is to compile to 101.41
 ;"        But when we encountered, we had to figure the system out again, so I
 ;"        am documenting what I found, for next time...  //kt
 ;"-----------------------------------------------------------------
 ;"
 ;"Below is example of RPC log when dialog is being loaded for CPRS       
 ;"ORWDXM PROMPTS
 ;"Called at: 7:42:20 PM
 ;" 
 ;"Params ------------------------------------------------------------------
 ;"literal	16021
 ;" 
 ;"Results -----------------------------------------------------------------
 ;"//kt NOTE: Below, the instance numbers shown are not provided by RPC call.  
 ;"           One has to manually count them to find #1, #2 etc.  
 ;"           The order shown below is a result of the SEQUENCE field ordering, not the SUBIEN # in ITEMS subfile of 101.41
 ;"~ID16024^0^0^DXS^Y^^^^~IEN=81;TYPE=I;ITEMS=2,3,4,5,6,7,8,9,10,11,12,13,75,14,118,119,120,121,123,124,125,126,127,128,129,130,133,134;^^                     <-- e.g. instance #1 of 16024
 ;"~ID16022^0^0^PROCS^Y^^^^~IEN=80;TYPE=L;ITEMS=15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,76,77,78,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,115,131;^^   <-- e.g. instance #1 of 16022
 ;"~ID16027^0^0^AnnualDiabetes^F^^^^~IEN=83;TYPE=B;ITEMS=17,20,25,27,28,29,38,78;^^
 ;"~ID16027^0^0^Diabetes^F^^^^~IEN=84;TYPE=B;ITEMS=20,25,28;^^
 ;"~ID16027^0^0^Screening^F^^^^~IEN=85;TYPE=B;ITEMS=20,25,27,38;^^
 ;"~ID16027^0^0^ScreeningWVitamins^F^^^^~IEN=86;TYPE=B;ITEMS=17,20,25,27,38;^^
 ;"~ID16027^0^0^ScreeningWVits[!DS]UA^F^^^^~IEN=87;TYPE=B;ITEMS=17,20,25,27,38,78;^^
 ;"~ID16032^0^0^ORDERING PROVIDER^N^^^^~IEN=93;TYPE=O;ITEMS=90,91,92;^^        <-- e.g. instance #01 of 16032
 ;"~ID16032^0^0^ORDER TIMING^N^^^^~IEN=94;TYPE=T;ITEMS=95,96,97,98,99;^^       <-- e.g. instance #02 of 16032
 ;"~ID16022^0^0^ORDER FLAGS^Y^^^^~IEN=107;TYPE=F;ITEMS=102,100,101;^^        <-- e.g. instance #02 of 16022
 ;"~ID16022^0^0^ORDER OPTIONS^Y^^^^~IEN=108;TYPE=N;ITEMS=106,103,104,105;^^  <-- e.g. instance #03 of 16022
 ;"~ID16034^0^0^FREE TEXT DX 1^F^^^^~IEN=109;TYPE=E;^^                           <-- e.g. instance #04 of 16034
 ;"~ID16034^0^0^FREE TEXT DX 2^F^^^^~IEN=110;TYPE=E;^^                           <-- e.g. instance #05 of 16034
 ;"~ID16034^0^0^FREE TEXT DX 3^F^^^^~IEN=111;TYPE=E;^^                           <-- e.g. instance #06 of 16034
 ;"~ID16034^0^0^FREE TEXT DX 4^F^^^^~IEN=112;TYPE=E;^^                           <-- e.g. instance #07 of 16034
 ;"~ID16035^0^0^COMMENTS^W^^^^~IEN=113;TYPE=W;^^                                 
 ;"~ID16034^0^0^FREE TEXT LAB^F^^^^~IEN=114;TYPE=E;^^                            <-- e.g. instance #08 of 16034
 ;"~ID16034^0^0^FREE TEXT OTHER TIME^F^^^^~IEN=116;TYPE=E;^^                     <-- e.g. instance #09 of 16034
 ;"~ID16034^0^0^FREE TEXT LINK TEST TO DX^F^^^^~IEN=117;TYPE=E;^^                <-- e.g. instance #10 of 16034
 ;"~ID16024^0^0^Anemia [!DS] D64.9^Y^^^^~IEN=2;TYPE=I;^^                                  <-- e.g. instance #02 of 16024
 ;"~ID16024^0^0^Low B12 [!DS] E53.8^Y^^^^~IEN=3;TYPE=I;^^                                 <-- e.g. instance #03 of 16024
 ;"~ID16024^0^0^Low Vit[!DS]D [!DS] E55.9^Y^^^^~IEN=4;TYPE=I;^^                           <-- e.g. instance #04 of 16024
 ;"~ID16024^0^0^DM[!DS]2 [!DS] E11.9^Y^^^^~IEN=5;TYPE=I;^^                                <-- e.g. instance #05 of 16024
 ;"~ID16024^0^0^DM[!DS]2 Uncontrolled [!DS] E11.65^Y^^^^~IEN=6;TYPE=I;^^                  <-- e.g. instance #06 of 16024
 ;"~ID16024^0^0^Fatigue [!DS] R53.82^Y^^^^~IEN=7;TYPE=I;^^                                <-- e.g. instance #07 of 16024
 ;"~ID16024^0^0^Gout [!DS] M10.9^Y^^^^~IEN=8;TYPE=I;^^                                    <-- e.g. instance #08 of 16024
 ;"~ID16024^0^0^HTN [!DS] I10.^Y^^^^~IEN=9;TYPE=I;^^                                      <-- e.g. instance #09 of 16024
 ;"~ID16024^0^0^Hypothyroidism [!DS] E03.9^Y^^^^~IEN=10;TYPE=I;^^                         <-- e.g. instance #10 of 16024
 ;"~ID16024^0^0^Lipids[!SC] unspecified [!DS] E78.5^Y^^^^~IEN=11;TYPE=I;^^                <-- e.g. instance #11 of 16024
 ;"~ID16024^0^0^Preventative [!DS] Z00.00^Y^^^^~IEN=12;TYPE=I;^^                          <-- e.g. instance #12 of 16024
 ;"~ID16024^0^0^Prostate/BPH [!DS] N40.0^Y^^^^~IEN=13;TYPE=I;^^                           <-- e.g. instance #13 of 16024
 ;"~ID16024^0^0^Well Child Preventative [!DS] Z00.129^Y^^^^~IEN=75;TYPE=I;^^              <-- e.g. instance #14 of 16024
 ;"~ID16024^0^0^Suspected Exposure to Covid19 [!DS] Z03.818^Y^^^^~IEN=14;TYPE=I;^^        <-- e.g. instance #15 of 16024
 ;"~ID16024^0^0^H/O Prostate CA [!DS] Z85.46^Y^^^^~IEN=118;TYPE=I;^^                      <-- e.g. instance #16 of 16024
 ;"~ID16024^0^0^Screening for Prostate CA [!DS] Z12.5^Y^^^^~IEN=119;TYPE=I;^^             <-- e.g. instance #17 of 16024
 ;"~ID16024^0^0^Mixed Hyperlipidemia [!DS] E78.1^Y^^^^~IEN=120;TYPE=I;FASTING=1;^^        <-- e.g. instance #18 of 16024
 ;"~ID16024^0^0^Other long term drug therapy [!DS] Z79.899^Y^^^^~IEN=121;TYPE=I;^^        <-- e.g. instance #19 of 16024
 ;"~ID16024^0^0^Paresthesia of skin [!DS] R20.2^Y^^^^~IEN=123;TYPE=I;^^                   <-- e.g. instance #20 of 16024            
 ;"~ID16024^0^0^Intestinal malabsorption[!SC] unspec [!DS] K90.9^Y^^^^~IEN=124;TYPE=I;^^  <-- e.g. instance #21 of 16024
 ;"~ID16024^0^0^Vit B deficiency anaemia[!SC] unspec [!DS] D51.9^Y^^^^~IEN=125;TYPE=I;^^  <-- e.g. instance #22 of 16024
 ;"~ID16024^0^0^Unspec dementia w/o disturb [!DS] F03.90^Y^^^^~IEN=126;TYPE=I;^^          <-- e.g. instance #23 of 16024
 ;"~ID16024^0^0^Gait abnormalities[!SC] other [!DS] R26.89^Y^^^^~IEN=127;TYPE=I;^^        <-- e.g. instance #24 of 16024
 ;"~ID16024^0^0^Screening for DM [!DS] Z13.1^Y^^^^~IEN=128;TYPE=I;^^                      <-- e.g. instance #25 of 16024
 ;"~ID16024^0^0^Hyperglycemia [!DS] R73.09^Y^^^^~IEN=129;TYPE=I;^^                        <-- e.g. instance #26 of 16024
 ;"~ID16024^0^0^Palpitations [!DS] R00.2^Y^^^^~IEN=130;TYPE=I;^^                          <-- e.g. instance #27 of 16024
 ;"~ID16024^0^0^Screening for other viral disease [!DS] Z11.59^Y^^^^~IEN=133;TYPE=I;^^    <-- e.g. instance #28 of 16024
 ;"~ID16024^0^0^Pre[!DS]diabetes [!DS] R73.03^Y^^^^~IEN=134;TYPE=I;^^                     <-- e.g. instance #29 of 16024
 ;"~ID16022^0^0^OFFICE Urine Drug Screen^Y^^^^~IEN=15;TYPE=L;^^                               <-- e.g. instance #04 of 16022
 ;"~ID16022^0^0^OFFICE Check BP today^Y^^^^~IEN=16;TYPE=L;^^                                  <-- e.g. instance #05 of 16022
 ;"~ID16022^0^0^B12^Y^^^^~IEN=17;TYPE=L;DX=3,123,127,124,126,125,121;^^                       <-- e.g. instance #06 of 16022
 ;"~ID16022^0^0^Vitamin D Total (82306)^Y^^^^~IEN=18;TYPE=L;^^                                <-- e.g. instance #07 of 16022
 ;"~ID16022^0^0^Folic Acid^Y^^^^~IEN=19;TYPE=L;^^                                             <-- e.g. instance #08 of 16022
 ;"~ID16022^0^0^CBC[!DS]Platelet With Diff.^Y^^^^~IEN=20;TYPE=L;^^                            <-- e.g. instance #09 of 16022
 ;"~ID16022^0^0^Serum Iron^Y^^^^~IEN=21;TYPE=L;^^                                             <-- e.g. instance #10 of 16022
 ;"~ID16022^0^0^TIBC^Y^^^^~IEN=22;TYPE=L;^^                                                   <-- e.g. instance #11 of 16022
 ;"~ID16022^0^0^Ferritin^Y^^^^~IEN=23;TYPE=L;^^                                               <-- e.g. instance #12 of 16022
 ;"~ID16022^0^0^Retic^Y^^^^~IEN=24;TYPE=L;^^                                                  <-- e.g. instance #13 of 16022
 ;"~ID16022^0^0^CMP^Y^^^^~IEN=25;TYPE=L;^^                                                    <-- e.g. instance #14 of 16022
 ;"~ID16022^0^0^Magnesium^Y^^^^~IEN=26;TYPE=L;^^                                              <-- e.g. instance #15 of 16022
 ;"~ID16022^0^0^Lipids^Y^^^^~IEN=27;TYPE=L;FASTING=1;DX=120,11;^^                             <-- e.g. instance #16 of 16022
 ;"~ID16022^0^0^HgbA1c^Y^^^^~IEN=28;TYPE=L;DX=128,129,69,5,6,134;^^                           <-- e.g. instance #17 of 16022
 ;"~ID16022^0^0^Urine Microalbumin/Creatinine^Y^^^^~IEN=29;TYPE=L;^^                          <-- e.g. instance #18 of 16022
 ;"~ID16022^0^0^Hep Fn Panel^Y^^^^~IEN=30;TYPE=L;^^                                           <-- e.g. instance #19 of 16022
 ;"~ID16022^0^0^PSA^Y^^^^~IEN=31;TYPE=L;LINKDX=1;DX=118,119;^^                                <-- e.g. instance #20 of 16022
 ;"~ID16022^0^0^FPG Rheumatoid Panel (ANATP[!CM]CRP[!CM]RF[!CM]Uric Acid)^Y^^^^~IEN=32;TYPE=L;^^  <-- e.g. instance #21 of 16022
 ;"~ID16022^0^0^Uric Acid^Y^^^^~IEN=33;TYPE=L;^^                                              <-- e.g. instance #22 of 16022
 ;"~ID16022^0^0^ANA Screening/Titer (ANATP)^Y^^^^~IEN=34;TYPE=L;^^                            <-- e.g. instance #23 of 16022
 ;"~ID16022^0^0^ANA F/U Panel 11 (ANA11)^Y^^^^~IEN=35;TYPE=L;^^                               <-- e.g. instance #24 of 16022
 ;"~ID16022^0^0^Sed Rate (DRAW EXTRA LAVENDER TUBE)^Y^^^^~IEN=36;TYPE=L;^^                    <-- e.g. instance #25 of 16022
 ;"~ID16022^0^0^C[!DS]reactive protein^Y^^^^~IEN=37;TYPE=L;^^                                 <-- e.g. instance #26 of 16022
 ;"~ID16022^0^0^TSH^Y^^^^~IEN=38;TYPE=L;DX=10,7,130;^^                                        <-- e.g. instance #27 of 16022
 ;"~ID16022^0^0^Thyroid Panel (TSH[!SC] Free T4)^Y^^^^~IEN=39;TYPE=L;^^                       <-- e.g. instance #28 of 16022
 ;"~ID16022^0^0^Triiodothyronine (T3)^Y^^^^~IEN=40;TYPE=L;^^                                  <-- e.g. instance #29 of 16022
 ;"~ID16022^0^0^Total Testosterone^Y^^^^~IEN=41;TYPE=L;^^                                     <-- e.g. instance #30 of 16022
 ;"~ID16022^0^0^[K] UA w/ micro (Use Test Code UA)^Y^^^^~IEN=76;TYPE=L;^^                     <-- e.g. instance #31 of 16022
 ;"~ID16022^0^0^[K] Urine Culture^Y^^^^~IEN=77;TYPE=L;^^                                      <-- e.g. instance #32 of 16022
 ;"~ID16022^0^0^[D] UA w/ micro [!DS] Reflex To Culture^Y^^^^~IEN=78;TYPE=L;^^                <-- e.g. instance #33 of 16022
 ;"~ID16022^0^0^Pro[!DS]BNP^Y^^^^~IEN=42;TYPE=L;^^                                            <-- e.g. instance #34 of 16022
 ;"~ID16022^0^0^Glucose^Y^^^^~IEN=43;TYPE=L;^^                                                <-- e.g. instance #35 of 16022
 ;"~ID16022^0^0^Methylmalonic acid^Y^^^^~IEN=44;TYPE=L;^^                                     <-- e.g. instance #36 of 16022
 ;"~ID16022^0^0^Homocysteine level^Y^^^^~IEN=45;TYPE=L;^^                                     <-- e.g. instance #37 of 16022
 ;"~ID16022^0^0^Stool: WBC[!SC] Guaiac[!SC] C diff Toxin(A&B)[!SC] GDH Ag[!SC] Culture[!SC] O&P^Y^^^^~IEN=46;TYPE=L;^^  <-- e.g. instance #38 of 16022
 ;"~ID16022^0^0^iFOBT^Y^^^^~IEN=47;TYPE=L;^^                                                  <-- e.g. instance #39 of 16022
 ;"~ID16022^0^0^Hep C IgG Ab screen^Y^^^^~IEN=48;TYPE=L;DX=133;^^                             <-- e.g. instance #40 of 16022
 ;"~ID16022^0^0^Hep B Screening (HepB SAg) (CODE[!DS]>HBSAL)^Y^^^^~IEN=49;TYPE=L;DX=133;^^    <-- e.g. instance #41 of 16022
 ;"~ID16022^0^0^HIV Screening^Y^^^^~IEN=50;TYPE=L;^^                                          <-- e.g. instance #42 of 16022
 ;"~ID16022^0^0^GC/Chlamydia urine screen^Y^^^^~IEN=51;TYPE=L;^^                              <-- e.g. instance #43 of 16022
 ;"~ID16022^0^0^FT3^Y^^^^~IEN=52;TYPE=L;^^                                                    <-- e.g. instance #44 of 16022
 ;"~ID16022^0^0^FT4^Y^^^^~IEN=53;TYPE=L;^^                                                    <-- e.g. instance #45 of 16022
 ;"~ID16022^0^0^Acute Hepatitis Panel (HEPAL)^Y^^^^~IEN=54;TYPE=L;^^                          <-- e.g. instance #46 of 16022
 ;"~ID16022^0^0^BMP^Y^^^^~IEN=55;TYPE=L;^^                                                    <-- e.g. instance #47 of 16022
 ;"~ID16022^0^0^Blood Type ABO and Rh^Y^^^^~IEN=56;TYPE=L;^^                                  <-- e.g. instance #48 of 16022
 ;"~ID16022^0^0^Gastrointestinal Parasite Panel by PCR (ARUP Ref Lab)^Y^^^^~IEN=57;TYPE=L;^^  <-- e.g. instance #49 of 16022
 ;"~ID16022^0^0^Flu Covid RSV PCR swab (FLUCOVRSVM)^Y^^^^~IEN=58;TYPE=L;^^                    <-- e.g. instance #50 of 16022
 ;"~ID16022^0^0^Peripheral Smear (Use Test Code MORP)^Y^^^^~IEN=59;TYPE=L;^^                  <-- e.g. instance #51 of 16022
 ;"~ID16022^0^0^SARS[!DS]COV2 Ab Total (Ig G A M)^Y^^^^~IEN=60;TYPE=L;^^                      <-- e.g. instance #52 of 16022
 ;"~ID16022^0^0^SARS[!DS]COV2 Ab [!DS] Spike Protein (COVIDSP)^Y^^^^~IEN=61;TYPE=L;^^         <-- e.g. instance #53 of 16022
 ;"~ID16022^0^0^RADIOLOGY: CXR PA & Lateral^Y^^^^~IEN=62;TYPE=L;^^                            <-- e.g. instance #54 of 16022
 ;"~ID16022^0^0^CARDIO: 12[!DS]LEAD EKG^Y^^^^~IEN=63;TYPE=L;^^                                <-- e.g. instance #55 of 16022
 ;"~ID16022^0^0^PT Eval and Treat^Y^^^^~IEN=64;TYPE=L;^^                                      <-- e.g. instance #56 of 16022
 ;"~ID16022^0^0^Celiac Disease Panel (Test Code: CDE)^Y^^^^~IEN=115;TYPE=L;^^                 <-- e.g. instance #57 of 16022
 ;"~ID16022^0^0^Thrombophilia Panel (FPG)^Y^^^^~IEN=131;TYPE=L;^^                             <-- e.g. instance #58 of 16022
 ;"~ID16026^0^0^Basic^F^^^^~IEN=70;TYPE=P;ITEMS=20,25,55,27,38,28;^^
 ;"~ID16026^0^0^Rheum^F^^^^~IEN=71;TYPE=P;ITEMS=32,33,34,35,36,37;^^
 ;"~ID16026^0^0^Cardio^F^^^^~IEN=72;TYPE=P;ITEMS=20,27,36,37,63,16,42,44,45;^^
 ;"~ID16026^0^0^Renal^F^^^^~IEN=73;TYPE=P;ITEMS=20,25,29,77,78;^^
 ;"~ID16026^0^0^GI^F^^^^~IEN=79;TYPE=P;ITEMS=57,46,20,115;^^
 ;"~ID16026^0^0^Metab^F^^^^~IEN=74;TYPE=P;ITEMS=28,33,38,53,52,26,43,25,17,18,19,39,40,41,44,45;^^
 ;"~ID16026^0^0^Heme^F^^^^~IEN=89;TYPE=P;ITEMS=20,19,21,22,23,24,59;^^
 ;"~ID16033^0^0^As Ordered^Y^^^^~IEN=90;TYPE=X;^^
 ;"~ID16033^0^0^Dr. Kevin T^Y^^^^~IEN=91;TYPE=X;^^
 ;"~ID16033^0^0^Dr. Dee T^Y^^^^~IEN=92;TYPE=X;^^
 ;"~ID16033^0^0^Prior To Next Appt^Y^^^^~IEN=95;TYPE=X;^^
 ;"~ID16033^0^0^ASAP^Y^^^^~IEN=96;TYPE=X;^^
 ;"~ID16033^0^0^Today^Y^^^^~IEN=97;TYPE=X;^^
 ;"~ID16033^0^0^Use lab blood if possible^Y^^^^~IEN=98;TYPE=X;^^
 ;"~ID16033^0^0^Other Time^Y^^^^~IEN=99;TYPE=X;^^
 ;"~ID16022^0^0^Fasting^Y^^^^~IEN=102;TYPE=F;^^
 ;"~ID16022^0^0^Sick Patient^Y^^^^~IEN=100;TYPE=F;^^
 ;"~ID16022^0^0^Standing Order^Y^^^^~IEN=101;TYPE=F;^^
 ;"~ID16022^0^0^AutoSign Order^Y^^^^~IEN=106;TYPE=N;^^
 ;"~ID16022^0^0^Ballad Order^Y^^^^~IEN=103;TYPE=N;^^
 ;"~ID16022^0^0^Outside Order^Y^^^^~IEN=104;TYPE=N;^^
 ;"~ID16022^0^0^Prompt To Print^Y^^^^~IEN=105;TYPE=N;^^ 
 ;" ------------------------------------------------------------------
 ;" ------------------------------------------------------------------
 ;" 
 ;"Below is the RPC log from CPRS after user opens dialog, selects tests CBC, CMP, and Dx: HTN, then closes it, saving it.
 ;" ORWDX SAVE                                              
 ;"Called at: 7:42:33 PM
 ;" 
 ;"Params ------------------------------------------------------------------
 ;"literal	74592
 ;"literal	168                                                                     
 ;"literal	6
 ;"literal	TMG LAB ORDER
 ;"literal	5
 ;"literal	16021
 ;"literal	
 ;"list	                                                                                       BELOW FROM RPC LIST ABOVE
 ;"	(16032,1)=0    <-- TMG LAB LISTBOX GROUP (`16032 in #101.41)    INSTANCE #1  -->   ID16032^0^0^ORDERING PROVIDER^N^^^^~IEN=93;TYPE=O;ITEMS=90,91,92;^^
 ;"	(16032,2)=0    <-- TMG LAB LISTBOX GROUP (`16032 in #101.41)    INSTANCE #2  -->   ID16032^0^0^ORDER TIMING^N^^^^~IEN=94;TYPE=T;ITEMS=95,96,97,98,99;^^
 ;"                  NOTE: Instance# is determined by the SEQUENCE # of the dialog
 ;"                  elements (ITEMS subfile in 101.41)
 ;"                  Thus the 2nd time #16032 is found, it is given INSTANCE #2    
 ;"             See GETDLGINFO^TMGORWD1 for more information        
 ;"	(16022,9)=1    <-- TMG LAB ORDER Y/N (`16022 in #101.41)        INSTANCE #9  -->   ID16022^0^0^CBC[!DS]Platelet With Diff.^Y^^^^~IEN=20;TYPE=L;^^
 ;"	(16022,14)=1   <-- TMG LAB ORDER Y/N (`16022 in #101.41)        INSTANCE #14 -->   ID16022^0^0^CMP^Y^^^^~IEN=25;TYPE=L;^^
 ;"	(16024,9)=1    <-- TMG LAB COMMON DX ENTRY (`16024 in #101.41)  INSTANCE #9  -->   ID16024^0^0^HTN [!DS] I10.^Y^^^^~IEN=9;TYPE=I;^^  
 ;"	("ORCHECK")=0                                    
 ;"	("ORTS")=0
 ;"literal	
 ;"literal	
 ;"literal	
 ;"literal	0
 ;"                                           
 ;"Results -----------------------------------------------------------------
 ;"~361360;1^6^3241109.1942^^^11^2^^^168^TOPPENBERG,KEVIN S^^0^^^^^^Laughlin_Office:6^^0^0   <--- 361360 IS IEN IN #100
 ;"t>>
 ;"tNON-FASTING LABS PRIOR TO NEXT APPT
 ;"t                                        
 ;"tTESTS ORDERED: CBC-Platelet With Diff., CMP
 ;"t                                    
 ;"tDIAG: HTN - I10.
 ;"t  *UNSIGNED*
 ;" 
 ;"Elapsed Time: 260 ms        
 ;"-----------------------------------------------------------------
 ;"-----------------------------------------------------------------
 ;"Below is dump of the record stored after above RPC:
 ;"Showing FILE: ORDER (#100)
 ;" Entry #361360,
 ;"     .01-ORDER # : 361360                                  <--- same IEN  seen in RPC aboev. 
 ;"     .02-OBJECT OF ORDER : ZZTEST,BABY (`74592 in #2)
 ;"      .8-ORDER ACTIONS : <MULTIPLE-VALUED>
 ;"          Entry #1
 ;"             .01-DATE/TIME ORDERED : NOV 09, 2024@19:42
 ;"              .1-ORDER TEXT :                            <--- I think this is result of compilation process.
 ;"                   NON-FASTING LABS PRIOR TO NEXT APPT
 ;"                 
 ;"                   TESTS ORDERED: CBC-Platelet With Diff., CMP
 ;"                 
 ;"                   DIAG: HTN - I10.
 ;"
 ;"               2-ACTION : NEW
 ;"               3-PROVIDER : TOPPENBERG,KEVIN S (`168 in #200)
 ;"               4-SIGNATURE STATUS : NOT SIGNED
 ;"              12-NATURE OF ORDER : ELECTRONICALLY ENTERED (`8 in #100.02)
 ;"              13-ENTERED BY : TOPPENBERG,KEVIN S (`168 in #200)
 ;"              14-TEXT REFERENCE : 1
 ;"              15-RELEASE STATUS : unreleased
 ;"       1-CURRENT AGENT/PROVIDER : TOPPENBERG,KEVIN S (`168 in #200)
 ;"       2-DIALOG : TMG LAB ORDER (`16021 in #101.41)
 ;"       3-WHO ENTERED : TOPPENBERG,KEVIN S (`168 in #200)
 ;"       4-WHEN ENTERED : NOV 09, 2024@19:42
 ;"     4.5-RESPONSES : <MULTIPLE-VALUED>
 ;"          Entry #1
 ;"             .01-ITEM ENTRY : 116
 ;"             .02-DIALOG : TMG LAB ORDER Y/N (`16022 in #101.41)
 ;"             .03-INSTANCE : 9   --> INSTANCE #9  -->   ID16022^0^0^CBC[!DS]Platelet With Diff.^Y^^^^~IEN=20;TYPE=L;^^
 ;"               1-VALUE : 1
 ;"          Entry #2
 ;"             .01-ITEM ENTRY : 116
 ;"             .02-DIALOG : TMG LAB ORDER Y/N (`16022 in #101.41)
 ;"             .03-INSTANCE : 14  --> INSTANCE #14 -->   ID16022^0^0^CMP^Y^^^^~IEN=25;TYPE=L;^^
 ;"               1-VALUE : 1
 ;"          Entry #3
 ;"             .01-ITEM ENTRY : 116
 ;"             .02-DIALOG : TMG LAB ORDER Y/N (`16022 in #101.41)
 ;"             .03-INSTANCE : 18  --> INSTANCE #18 -->  ID16022^0^0^Urine Microalbumin/Creatinine^Y^^^^~IEN=29;TYPE=L;^^
 ;"               1-VALUE : 0  <--- NOTE: In this order, I had edited it, added microalbumin, then removed it.  So data is stored, but value is 0
 ;"          Entry #4
 ;"             .01-ITEM ENTRY : 29
 ;"             .02-DIALOG : TMG LAB COMMON DX ENTRY (`16024 in #101.41)
 ;"             .03-INSTANCE : 9  --> INSTANCE #9  -->   ID16024^0^0^HTN [!DS] I10.^Y^^^^~IEN=9;TYPE=I;^^
 ;"               1-VALUE : 1
 ;"          Entry #5
 ;"             .01-ITEM ENTRY : 102
 ;"             .02-DIALOG : TMG LAB LISTBOX GROUP (`16032 in #101.41)  
 ;"             .03-INSTANCE : 1     --> INSTANCE #1  -->   ID16032^0^0^ORDERING PROVIDER^N^^^^~IEN=93;TYPE=O;ITEMS=90,91,92;^^
 ;"               1-VALUE : 0
 ;"          Entry #6
 ;"             .01-ITEM ENTRY : 102
 ;"             .02-DIALOG : TMG LAB LISTBOX GROUP (`16032 in #101.41)
 ;"             .03-INSTANCE : 2     --> INSTANCE #2  -->   ID16032^0^0^ORDER TIMING^N^^^^~IEN=94;TYPE=T;ITEMS=95,96,97,98,99;^^
 ;"               1-VALUE : 0
 ;"       5-STATUS : UNRELEASED (`11 in #100.01)
 ;"       6-PATIENT LOCATION : Laughlin_Office (`6;SC( in #0)
 ;"       7-ITEM ORDERED : TMG LAB ORDER (`16021;ORD(101.41, in #0)
 ;"     8.1-TYPE : STANDARD
 ;"      10-PATIENT CLASS : OUTPATIENT
 ;"      12-PACKAGE : ORDER ENTRY/RESULTS REPORTING (`35 in #9.4)
 ;"      14-SIGNATURE REQUIRED : ORES
 ;"      23-TO : CHEMISTRY (`6 in #100.98)
 ;"      30-CURRENT ACTION : 1
 ;"      31-DATE OF LAST ACTIVITY : NOV 09, 2024@20:47:25
 ;"      32-GRACE DAYS BEFORE PURGE : 90
 ;"-------------------------------
 ;"
 ;"NOTICE!:
 ;"  The ORDER DIALOG system, as set up by vanilla VistA uses a crazy system of combining
 ;"  Dialog type(IEN101.41) with INSTANCE# to create a unique id for storing data
 ;"  when saving the results of an order after being saved in CPRS. 
 ;"  We have encountered a problem where the dialog elements were edited, and then an older
 ;"    order was changed.  As the prior user selections ('responses') are loaded, they are
 ;"    matched via the DialogType+Instance#.  BUT the problem is that if a new dialog
 ;"    element had been inserted higher in the list, then the instance numbers of subsequent
 ;"    elements will be SHIFTED TO DIFFERENT NUMBERS.  And thus prior data for test A might
 ;"    be now changed to a test B!  Using POSITIONAL ORDERING for test ID seems crazy!
 ;"=======================================================================
 ;"=======================================================================
 ;
REFRESH() ;"Delete prior entries and recompile
  ;"//kt note:  The ORDERING that results from the compilation process is important, and can lead
  ;"  to lab order problems if there are changes to the sequences.  So I am going to remove kill.
  ;"DO KILLALL 
  DO COMPILE 
  WRITE !,"Compilation complete",!
  DO PRESS2GO^TMGUSRI2
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
  NEW RESULT,NAME,TYPE,IEN22751,IENS101D42,SUBIEN
  DO PREPVARS(LINE,.RESULT,.IEN22751,.NAME,.TYPE,,,,.IENS101D42)
  SET SUBIEN=+$GET(INFO("IENMAP",IEN22751))
  IF SUBIEN>0 GOTO E1P1  ;"Already exists...   
  IF +IENS101D42>0 SET SUBIEN=+IENS101D42
  IF SUBIEN>0 GOTO E1P1  ;"Already exists...
  SET SUBIEN=$$GETIEN101D412(TOPIEN,NAME,TYPE)
  IF SUBIEN>0 GOTO E1P1  ;"Already exists...
  SET RESULT=$$MAKEREC(TOPIEN,LINE,NAME,.ITEMSARR,.INFO)   ;"This will set INFO("IENMAP",IEN22751)=IENS
  IF RESULT'>-1 GOTO E1PDN
  SET RESULT=$$ENSURELINK22751(IEN22751,.INFO)
  GOTO E1PDN
E1P1 ; 
  SET INFO("IENMAP",IEN22751)=SUBIEN_","_TOPIEN_","  ;"set again if already set.  
  SET RESULT="1^"_SUBIEN
  NEW TEMP SET TEMP=$$ENSUREDATA(TOPIEN,SUBIEN,LINE,.ITEMSARR) 
  IF TEMP'>-1 SET RESULT=TEMP GOTO E1PDN
  SET TEMP=$$ENSURELINK22751(IEN22751,.INFO)
  IF TEMP'>-1 SET RESULT=TEMP GOTO E1PDN
E1PDN ;  
  QUIT RESULT
  ; 
ENSURELINK22751(IEN22751,INFO)  ;"STORE IENS to 101.412 back into 22751
  ;"FINISH
  NEW RESULT SET RESULT="1^OK"  ;"default
  SET IEN22751=+$GET(IEN22751) IF IEN22751'>0 DO  GOTO EL2DN
  . SET RESULT="-1^Invalid value for IEN22751 in ENSURELINK22751^TMGLRU2"
  NEW TMGFDA,TMGMSG
  NEW IENS101D42 SET IENS101D42=$GET(INFO("IENMAP",IEN22751))
  IF IENS101D42="" DO  GOTO EL2DN
  . SET RESULT="-1^Unable to finding IENS as value in INFO('IENMAP',"_IEN22751_")"
  NEW N21 SET N21=$GET(^TMG(22751,IEN22751,21))
  IF $PIECE(N21,"^",2)=IENS101D42 GOTO EL2DN  ;"already stored, don't store again.  
  SET TMGFDA(22751,IEN22751_",",2.2)=IENS101D42   ;" 2.2  LINKED DLG ITEM (IENS 101.412)
  DO FILE^DIE("E","TMGFDA","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO    
  . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
EL2DN ;  
  QUIT RESULT
  ;
SAVEPARENT(ARR)  ;"Return if parent should be saved as record
  NEW RESULT SET RESULT=1
  IF $PIECE($GET(ARR),"^",2)="DISPLAY PAGES" SET RESULT=0
  QUIT RESULT;
  ;
PREPVARS(LINE,RESULT,IEN22751,NAME,TYPE,FASTING,LINKDX,DXLST,IENS101D42) ;
  SET RESULT="1^OK"
  SET IEN22751=$PIECE(LINE,"^",1)
  SET NAME=$$SAFENAME($PIECE(LINE,"^",2)) ;"Remove and replace disallowed chars
  SET TYPE=$PIECE(LINE,"^",3)
  SET FASTING=($PIECE(LINE,"^",4)="Y")
  SET LINKDX=($PIECE(LINE,"^",5)="Y")
  SET DXLST=$PIECE(LINE,"^",6)  ;"CSV list of IEN's in 22751
  SET IENS101D42=$PIECE(LINE,"^",7) 
  QUIT
  ;
ENSUREDATA(TOPIEN,SUBIEN,LINE,ITEMSARR) ;
  ;"TO DO.  If source record was renamed, should also ensure that gets put into 101.41
  NEW RESULT SET RESULT="1^OK"
  NEW NAME DO PREPVARS(LINE,,,.NAME)
  NEW CURNAME SET CURNAME=$$GET1^DIQ(101.412,SUBIEN_","_TOPIEN_",",4)
  IF (NAME'=""),(NAME'=CURNAME) DO  IF RESULT'>0 GOTO EDTDN 
  . SET RESULT=$$SETNAME(TOPIEN,SUBIEN,NAME)  
  NEW DATASTR SET DATASTR=$$GETDATASTR(TOPIEN,LINE,.ITEMSARR)
  NEW N1 SET N1=$PIECE($GET(^ORD(101.41,TOPIEN,10,SUBIEN,1)),"^",1)
  IF N1'=DATASTR DO  IF RESULT'>0 GOTO EDTDN
  . SET RESULT=$$SETDATA(TOPIEN,SUBIEN,DATASTR)
EDTDN;  
  QUIT RESULT
  ;
GETDATASTR(TOPIEN,LINE,ITEMSARR) ;
  NEW IEN22751,NAME,TYPE,FASTING,LINKDX,DXLST,IENS101D42
  DO PREPVARS(LINE,.RESULT,.IEN22751,.NAME,.TYPE,.FASTING,.LINKDX,.DXLST,.IENS101D42) 
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
  IF LINKDX SET DATA=DATA_"LINKDX=1;"
  IF DXLST'="" SET DATA=DATA_"DX="_DXLST_";"
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
SETNAME(TOPIEN,SUBIEN,NAME) ;
  ;"RESULT: 1^OK, 1^SUBIEN, OR -1^ErrorMessage
  NEW RESULT SET RESULT="1^OK"
  NEW TMGFDA,TMGMSG
  SET TMGFDA(101.412,SUBIEN_","_TOPIEN_",",4)=NAME  
  DO FILE^DIE("E","TMGFDA","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO    
  . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  QUIT RESULT
  ;
MAKEREC(TOPIEN,LINE,NAME,ITEMSARR,INFO) ;
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
  SET INFO("IENMAP",IEN22751)=SUBIEN_","_TOPIEN_","  ;"//kt 11/11/24 changed from just SUBIEN to IENS string
  IF SUBIEN>0 SET RESULT=SUBIEN
  ELSE  SET RESULT="-1^Unable to find IEN of added record"
MRDN  ;  
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
  ;"//kt NOTE 11/2024.  I can't do things this way... The ORDER DIALOG system uses
  ;"      instance counting, so inserting anything anywhere other than at the END of 
  ;"      all sequences can lead to stored USER RESPONSES in file #100 to get shifted and
  ;"      cause errors.
  ;"     FIX: I will just return a value for the last of ALL, irregardless of RANGE
  ;"INPUT -- RANGE  eg. "6" for range of 6.000 to 6.999
  
  NEW LASTSEQ SET LASTSEQ=$ORDER(^ORD(101.41,TOPIEN,10,"B",""),-1)
  IF LASTSEQ=0 SET LASTSEQ=0.001
  ;"//kt 11/11/24
  ;"  NEW LASTSEQ SET LASTSEQ=0
  ;"  NEW SEQ SET SEQ=(RANGE\1-0.000001)  ;"just before start of range
  ;"  NEW NEXT SET NEXT=RANGE\1+1
  ;"  FOR  SET SEQ=$ORDER(^ORD(101.41,TOPIEN,10,"B",SEQ)) QUIT:(SEQ'>0)!(SEQ'<NEXT)  DO
  ;"  . SET LASTSEQ=SEQ
  ;"  IF LASTSEQ=0 SET LASTSEQ=RANGE\1+0.001
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
  ;"     INFO("IENMAP",IEN22751)=IEN101.412  <-- actually IENS string '<IEN101.412>,<IEN101.41>,'
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
  ;"===========================================================================
  ;"===========================================================================
EDITORDER  ;"Edit lab order data
  NEW MENU,IDX,USRPICK
  ;
EOML1 ;                    
  SET IDX=0  
  KILL MENU 
  SET MENU(IDX)="Select Option For Editing TMG Order Dialog"
  SET IDX=IDX+1,MENU(IDX)="Labs/Procedures View/Edit"_$CHAR(9)_"LabProc"
  SET IDX=IDX+1,MENU(IDX)="Group/TabPage for Labs/Procs View/Edit"_$CHAR(9)_"GROUPS"
  SET IDX=IDX+1,MENU(IDX)="Diagnoses View/Edit"_$CHAR(9)_"Dx"
  SET IDX=IDX+1,MENU(IDX)="Dx Bundles View/Edit"_$CHAR(9)_"DXBUNDLE"
  SET IDX=IDX+1,MENU(IDX)="Other View/Edit"_$CHAR(9)_"OTHER"
  SET IDX=IDX+1,MENU(IDX)="Show Final ORDER DIALOG by SEQUENCE#"_$CHAR(9)_"SEQ_VIEW"
  SET IDX=IDX+1,MENU(IDX)="COMPILE to Final ORDER DIALOG (101.41)"_$CHAR(9)_"COMPILE"
  WRITE !
  SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
  IF USRPICK="^" GOTO EODN  
  IF USRPICK="LabProc" DO  GOTO EOML1
  . DO EDITLABPROC()   
  IF USRPICK="Dx" DO  GOTO EOML1
  . DO EDITDX()
  IF USRPICK="DXBUNDLE" DO  GOTO EOML1
  . DO EDITBUNDLE()
  IF USRPICK="GROUPS" DO  GOTO EOML1
  . DO EDITGRPS()
  IF USRPICK="OTHER" DO  GOTO EOML1
  . DO EDITOTHER()
  IF USRPICK="COMPILE" DO  GOTO EOML1
  . DO REFRESH()
  IF USRPICK="SEQ_VIEW" DO  GOTO EOML1
  . DO SEQVIEW()
  GOTO EOML1
EODN  ;
  QUIT
  ;
EDITLABPROC() ;"Edit/manage labs/procedures
  NEW MENU,IDX,USRPICK,IEN,RECS,NAME
ELPL1 ;                    
  KILL RECS DO GETRECS(.RECS,"L")
  IF $DATA(RECS)=0 DO  QUIT
  . WRITE !,!,"Unable find records from top-level record!",!
  . DO PRESS2GO^TMGUSRI2
  SET IDX=0  
  KILL MENU 
  SET MENU(IDX)="Select Labs/Procedure For Viewing/Editing"
  SET NAME="" FOR  SET NAME=$ORDER(RECS("NAME",NAME)) QUIT:NAME=""  DO
  . NEW IEN SET IEN=$GET(RECS("NAME",NAME)) QUIT:IEN'>0
  . SET IDX=IDX+1,MENU(IDX)=NAME_$CHAR(9)_IEN
  IF IDX>0 SET MENU(IDX,1)=$CHAR(8)_"--------------------------------------"
  SET IDX=IDX+1,MENU(IDX)="Search for Record to View/Edit"_$CHAR(9)_"SEARCH"
  SET IDX=IDX+1,MENU(IDX)="Add NEW Lab/Procedure"_$CHAR(9)_"ADD"
  WRITE !
  SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
  IF USRPICK="^" GOTO ELPDN  
  IF USRPICK="SEARCH" DO  GOTO ELPL1
  . DO SRCHMANAGE1("L")
  IF USRPICK="ADD" DO  GOTO ELPL1
  . DO ADDLABPROC()
  IF +USRPICK=USRPICK DO  GOTO ELPL1
  . DO MANAGE1(+USRPICK) 
  GOTO ELPL1
ELPDN  ;
  QUIT
  ;
ADDLABPROC() ;
  NEW NAME SET NAME=""
  NEW MSGARR,OPTION
  SET MSGARR(1)="Enter Name Of Lab/Procedure"
  SET MSGARR(2)=" "
  SET OPTION("INIT VALUE")=""  ;"<SomeTestName>"
  NEW FG,BG IF $$COLORPAIR^TMGUSRI8("WHITE","BLUE",,.FG,.BG)  ;"ignore result
  SET OPTION("COLOR","FG")=FG,OPTION("COLOR","BG")=BG
  SET OPTION("ALT BUFFER")=1
  NEW NAME SET NAME=$$EDITDLG^TMGUSRI6(.MSGARR,.OPTION)
  IF (NAME["^")!(NAME="") WRITE !,"Aborting",!
  ;"Check if name already exists
  IF $ORDER(^TMG(22751,"B",$EXTRACT(NAME,1,30),""))>0 DO  GOTO ALPDN
  . WRITE !,"Sorry.  This name already exists.  Can not add duplicate.",!
  NEW TYPE SET TYPE="L"  ;"Lab/Procedure
  NEW IEN22751 SET IEN22751=$$ENSUREREC^TMGLRU1(NAME,TYPE)
  IF IEN22751'>0  GOTO ALPDN
  NEW PARENTIEN SET PARENTIEN=$$GETIEN^TMGLRU1("TMG LAB ORDER GROUP PROCS")
  IF PARENTIEN'>0 DO  GOTO ALPDN
  . WRITE !,"Unable to find record 'TMG LAB ORDER GROUP PROCS' in file 22751",!
  NEW SUBIEN SET SUBIEN=$$ENSURESUBREC^TMGLRU1(IEN22751,PARENTIEN)
  IF SUBIEN'>0  DO  GOTO ALPDN
  . WRITE !,"Failed to add record #"_IEN22751_" into record 'TMG LAB ORDER GROUP PROCS' in file 22751",!
  WRITE !,"Successfully added record, and included into 'TMG LAB ORDER GROUP PROCS'",!
ALPDN ;
  DO PRESS2GO^TMGUSRI2
  QUIT
  ;
EDITDX() ;"Edit/manage diagnoses
  NEW MENU,IDX,USRPICK,IEN,RECS,NAME
  DO GETRECS(.RECS,"I")
  KILL RECS("TMG LAB ORDER GROUP DXS")  ;"exclude holder record
EDX1 ;                    
  SET IDX=0  
  KILL MENU 
  SET MENU(IDX)="Select Option For Editing Diagnoses"
  SET NAME="" FOR  SET NAME=$ORDER(RECS("NAME",NAME)) QUIT:NAME=""  DO
  . NEW IEN SET IEN=$GET(RECS("NAME",NAME)) QUIT:IEN'>0
  . SET IDX=IDX+1,MENU(IDX)=NAME_" -- View/Edit"_$CHAR(9)_IEN
  IF IDX>0 SET MENU(IDX,1)=$CHAR(8)_"--------------------------------------"
  SET IDX=IDX+1,MENU(IDX)="ADD a Diagnosis"_$CHAR(9)_"ADD"
  SET IDX=IDX+1,MENU(IDX)="DELETE a Diagnosis"_$CHAR(9)_"DEL"
  WRITE !
  SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
  IF USRPICK="^" GOTO EDXDN  
  IF USRPICK="ADD" DO  GOTO EDX1
  . DO ADDITEM("DX")
  IF USRPICK="DEL" DO  GOTO EDX1
  . DO DELITEM("DX")
  IF USRPICK="SEARCH" DO  GOTO EDX1
  . DO SRCHMANAGE1("D")
  IF +USRPICK=USRPICK DO  GOTO EDX1
  . DO MANAGE1(+USRPICK) 
  GOTO EDX1
EDXDN  ;
  QUIT
  ;
EDITBUNDLE() ;"Edit/manage diagnosis Bundles
  NEW MENU,IDX,USRPICK,IEN,RECS,NAME,INFO
  NEW INFO DO SETUPINFO2(.INFO,"BUNDLE")
EBND1 ;                    
  KILL RECS DO GETRECS(.RECS,"B")
  SET IDX=0  
  KILL MENU 
  SET MENU(IDX)="Select Option For Editing Diagnosis Bundles"
  SET NAME="" FOR  SET NAME=$ORDER(RECS("NAME",NAME)) QUIT:NAME=""  DO
  . NEW IEN SET IEN=$GET(RECS("NAME",NAME)) QUIT:IEN'>0
  . NEW SHORTNAME SET SHORTNAME=$PIECE(NAME,INFO("NAME PREFIX"),2)
  . SET IDX=IDX+1,MENU(IDX)=SHORTNAME_" -- View/Edit"_$CHAR(9)_IEN
  IF IDX>0 SET MENU(IDX,1)=$CHAR(8)_"--------------------------------------"
  SET IDX=IDX+1,MENU(IDX)="ADD a Diagnosis Bundle"_$CHAR(9)_"ADD"
  SET IDX=IDX+1,MENU(IDX)="DELETE a Diagnosis Bundle"_$CHAR(9)_"DEL"
  WRITE !
  SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
  IF USRPICK="^" GOTO EBNDN  
  IF USRPICK="ADD" DO  GOTO EBND1
  . DO ADDITEM("BUNDLE")
  IF USRPICK="DEL" DO  GOTO EBND1
  . DO DELITEM("BUNDLE",.INFO)
  IF +USRPICK=USRPICK DO  GOTO EBND1
  . DO MANAGE1(+USRPICK,.INFO) 
  GOTO EBND1
EBNDN  ;
  QUIT
  ;
EDITGRPS() ;"Edit/manage GROUPS (Tab Pages)
  NEW MENU,IDX,USRPICK,IEN,RECS,NAME
EGPS0 ;                    
  KILL RECS DO GETRECS(.RECS,"P")
  IF $DATA(RECS)=0 DO  QUIT
  . WRITE !,!,"Unable find and Page Group records from top-level record!",!
  . DO PRESS2GO^TMGUSRI2
EGPS1 ;                    
  SET IDX=0  
  KILL MENU 
  SET MENU(IDX)="Select Option For Editing Groups / Tab Pages"
  SET NAME="" FOR  SET NAME=$ORDER(RECS("NAME",NAME)) QUIT:NAME=""  DO
  . NEW SUBIEN SET SUBIEN=$GET(RECS("NAME",NAME)) QUIT:SUBIEN'>0
  . SET IDX=IDX+1,MENU(IDX)=$$LJ^XLFSTR(NAME,10)_" -- View/Edit"_$CHAR(9)_SUBIEN
  IF IDX>0 SET MENU(IDX,1)=$CHAR(8)_"--------------------------------------"
  SET IDX=IDX+1,MENU(IDX)="ADD Group/Page"_$CHAR(9)_"ADD"
  SET IDX=IDX+1,MENU(IDX)="DELETE Group/Page"_$CHAR(9)_"DEL"
  WRITE !
  SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
  IF USRPICK="^" GOTO EGPSDN
  IF USRPICK="ADD" DO  GOTO EGPS0
  . DO ADDITEM("GROUP")
  IF USRPICK="DEL" DO  GOTO EGPS0
  . DO DELITEM("GROUP")
  IF +USRPICK=USRPICK DO  GOTO EGPS0
  . DO MANAGE1(+USRPICK) 
  GOTO EGPS1
EGPSDN  ;
  QUIT
  ;
EDITOTHER() ;"Edit other types of records. 
  NEW MENU,IDX,USRPICK,IEN,RECS,NAME,TYPE
  NEW MAP DO SETUPTYPES(.MAP)
  FOR TYPE="L","I","P","B" KILL MAP(TYPE)  ;"These types are managed elsewhere
EO1 ;                    
  SET IDX=0  
  KILL MENU 
  SET MENU(IDX)="Select Option For OTHER records"
  SET TYPE="" FOR  SET TYPE=$ORDER(MAP(TYPE)) QUIT:TYPE=""  DO
  . NEW TYPENAME SET TYPENAME=MAP(TYPE)
  . KILL RECS DO GETRECS(.RECS,TYPE)
  . SET NAME="" FOR  SET NAME=$ORDER(RECS("NAME",NAME)) QUIT:NAME=""  DO
  . . NEW SUBIEN SET SUBIEN=$GET(RECS("NAME",NAME)) QUIT:SUBIEN'>0
  . . NEW S SET S=TYPENAME_" "_NAME
  . . SET IDX=IDX+1,MENU(IDX)=$$LJ^XLFSTR(S,15)_" -- View/Edit"_$CHAR(9)_SUBIEN
  IF IDX>0 SET MENU(IDX,1)=$CHAR(8)_"--------------------------------------"
  WRITE !
  SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
  IF USRPICK="^" GOTO EOTHDN
  IF +USRPICK=USRPICK DO  GOTO EO1
  . DO MANAGE1(+USRPICK) 
  GOTO EO1
EOTHDN  ;
  QUIT
  ;
MANAGE1(IEN,INFO) ;"EDIT 1 RECORD
  ;"Input: IEN -- IEN 22751
  ;"       INFO -- optional
  NEW MENU,IDX,USRPICK
E1LP0 ;  
  NEW NAME SET NAME=$PIECE($GET(^TMG(22751,+IEN,0)),"^",1)
  NEW TYPE SET TYPE=$PIECE($GET(^TMG(22751,IEN,0)),"^",2)
  NEW PREFIX SET PREFIX=$GET(INFO("NAME PREFIX"))
  IF PREFIX'="" SET NAME=$PIECE(NAME,PREFIX,2)
  NEW MAP DO SETUPTYPES(.MAP)
  NEW CANEDIT SET CANEDIT=($GET(MAP(TYPE,"FIELDS"))'="")
  ;
E1LP1 ;                  
  SET IDX=0  
  KILL MENU 
  SET MENU(IDX)="Select Option For: "_NAME
  SET MENU(IDX,1)="  TYPE="_$$GET1^DIQ(22751,IEN,.02)  
  IF 'CANEDIT SET MENU(IDX,2)="  EDITING NOT ALLOWED"
  SET IDX=IDX+1,MENU(IDX)="DUMP record: "_NAME_$CHAR(9)_"DUMP"
  IF CANEDIT SET IDX=IDX+1,MENU(IDX)="EDIT record:"_NAME_$CHAR(9)_"EDIT"
  WRITE !
  SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
  IF USRPICK="^" GOTO E1LPDN
  IF USRPICK="DUMP" DO  GOTO E1LP1
  . DO DUMPREC^TMGDEBU3(22751,+IEN)
  . DO PRESS2GO^TMGUSRI2
  IF USRPICK="EDIT" DO  GOTO E1LP0
  . DO EDIT1REC(+IEN)
  . ;"WRITE !,!,"NOTE: If record was renamed, then please RESTART.",!
  . DO PRESS2GO^TMGUSRI2
  GOTO E1LP1
E1LPDN  ;
  QUIT
  ;  
SRCHMANAGE1(TYPE)  ;"Search for and manage 1 record, of specified type
  ;"TYPE must be value of .02 TYPE field (intervalue)
  NEW DIC,X,Y SET DIC=22751,DIC(0)="MAEQ"
  SET DIC("S")="I $P(^(0),U,2)="""_TYPE_""""
  DO ^DIC WRITE ! 
  IF +Y'>0 DO  QUIT
  . WRITE "No Record Selected.",!
  . DO PRESS2GO^TMGUSRI2
  DO MANAGE1(+Y)
  QUIT
  ;
EDIT1REC(IEN) ;"edit 1 record
  NEW MAP DO SETUPTYPES(.MAP)
  WRITE !,"Edit record",!
  WRITE "-----------",!
  SET IEN=+$GET(IEN) QUIT:IEN'>0
  NEW TYPE SET TYPE=$PIECE($GET(^TMG(22751,IEN,0)),"^",2)
  NEW FIELDS SET FIELDS=MAP(TYPE,"FIELDS")
  IF FIELDS="" DO  QUIT
  . WRITE !,"Sorry, this type of record should NOT be edited. ",!
  . WRITE "Any change will cause problems, so aborting.",!
  . DO PRESS2GO^TMGUSRI2
  WRITE "Record type is '"_$GET(MAP(TYPE))_"', so editing fields '",FIELDS,"'",!
  WRITE "-----------",!
  WRITE "NOTE!!: Records should NOT be deleted via fileman '@' functionality.",!
  WRITE "        This will cause major problems with overall order functioning.",!,!
  NEW DA,DR,DIE
  SET DIE="^TMG(22751,",DA=IEN,DR=FIELDS
  LOCK +^TMG(22751,IEN):0 
  IF $TEST DO
  . DO ^DIE WRITE !
  . LOCK -^TMG(22751,IEN)
  ELSE  DO
  . WRITE !,"Sorry, Another user is editing this entry. Aborting." 
E1RDN ;  
  QUIT
  ;
ROOTIEN() ;"Return IEN of top level record
  QUIT $$GET1IEN("TMG LAB ORDER DIALOG") 
  ;
GRPGRPIEN() ;"Return IEN of group of groups. 
  QUIT $$GET1IEN("TMG LAB ORDER GROUP DISPLAY PAGES") 
  ;
LPGRPIEN() ;"Return IEN of lab/procedure group
  QUIT $$GET1IEN("TMG LAB ORDER GROUP DXS") 
  ;
GET1IEN(NAME) ;"
  NEW IEN SET IEN=$ORDER(^TMG(22751,"B",$E(NAME,1,30),0))
  IF IEN'>0 DO 
  . WRITE "This is required for proper operation.  Aborting.",!
  . DO PRESS2GO^TMGUSRI2  
  QUIT IEN 
  ;
GETRECS(OUT,TYPE) ;"RETURN ARRAY OF RECORD WHICH MATCH TYPE OF 'TYPE'
  KILL OUT
  ;"NEW TOPIEN SET TOPIEN=$$ROOTIEN
  ;"IF TOPIEN'>0 DO  QUIT
  ;". WRITE !,!,"Unable to find IEN of top-level record!",!
  ;". DO PRESS2GO^TMGUSRI2
  NEW IEN SET IEN=0
  ;"FOR  SET IEN=$ORDER(^TMG(22751,TOPIEN,1,IEN)) QUIT:IEN'>0  DO
  ;". NEW ZN SET ZN=$GET(^TMG(22751,TOPIEN,1,IEN,0)) QUIT:ZN=""
  FOR  SET IEN=$ORDER(^TMG(22751,IEN)) QUIT:IEN'>0  DO
  . NEW ZN SET ZN=$GET(^TMG(22751,IEN,0)) QUIT:ZN=""
  . ;"NEW SUBIEN SET SUBIEN=+ZN QUIT:SUBIEN'>0
  . ;"SET ZN=$GET(^TMG(22751,SUBIEN,0))
  . ;"SET ZN=$GET(^TMG(22751,IEN,0))
  . NEW ATYPE SET ATYPE=$PIECE(ZN,"^",2) QUIT:ATYPE'=TYPE
  . ;"SET OUT("IEN",SUBIEN)=""
  . SET OUT("IEN",IEN)=""
  . NEW NAME SET NAME=$PIECE(ZN,"^",1) QUIT:NAME=""
  . ;"SET OUT("NAME",NAME)=SUBIEN
  . SET OUT("NAME",NAME)=IEN
  IF TYPE="P" DO
  . ;"Don't allow manual manipulation of master group.  It is a group of groups. 
  . SET IEN=+$GET(RECS("NAME","TMG LAB ORDER GROUP DISPLAY PAGES"))
  . KILL RECS("NAME","TMG LAB ORDER GROUP DISPLAY PAGES") 
  . KILL RECS("IEN",IEN)
  QUIT
  ;
SETUPINFO2(INFO,MODE) ;"Setup info for ADDITEM,DELITEM
  IF MODE="GROUP" DO
  . SET INFO("TYPE")="P"
  . SET INFO("PROMPT")="Group name (Tab page name)"
  . SET INFO("HOLDERGROUPNAME")="TMG LAB ORDER GROUP DISPLAY PAGES"
  . SET INFO("NOUN")="lab/procedures"
  . SET INFO("ITEM NAME")="lab/procedures"
  . SET INFO("NAME PREFIX")=""
  ELSE  IF MODE="DX" DO
  . SET INFO("TYPE")="I"
  . SET INFO("PROMPT")="Diagnosis"  
  . SET INFO("HOLDERGROUPNAME")="TMG LAB ORDER GROUP DXS"
  . SET INFO("NOUN")="diagnoses"
  . SET INFO("ITEM NAME")=""
  . SET INFO("NAME PREFIX")=""
  ELSE  IF MODE="BUNDLE" DO
  . SET INFO("TYPE")="B"
  . SET INFO("PROMPT")="Diagnosis Bundle"  
  . SET INFO("HOLDERGROUPNAME")="TMG LAB ORDER DIALOG"
  . SET INFO("NOUN")="diagnoses bundle"
  . SET INFO("ITEM NAME")="diagnosis"
  . SET INFO("NAME PREFIX")="TMG LAB ORDER GROUP BUNDLE "
  SET INFO("GRPIEN")=$$GET1IEN(INFO("HOLDERGROUPNAME"))
  IF INFO("GRPIEN")'>0 DO
  . WRITE !,"Unable to find group '"_$GET(INFO("HOLDERGROUPNAME"))_"' to enter new record into.",!
  QUIT
  ;
SETUPTYPES(MAP) ;"Setup info for RECORD TYPES
  NEW TEMP SET TEMP=$PIECE(^DD(22751,.02,0),"^",3)
  NEW IDX FOR IDX=1:1:$LENGTH(TEMP,";") DO
  . NEW ITEM SET ITEM=$PIECE(TEMP,";",IDX) QUIT:ITEM=""
  . NEW KEY,VALUE SET KEY=$PIECE(ITEM,":",1),VALUE=$PIECE(ITEM,":",2)
  . SET MAP(KEY)=VALUE
  SET MAP("D","FIELDS")="1"                               ;"DIALOG"
  SET MAP("L","FIELDS")=".01;10;11;20"                    ;"LAB/PROCEDURE"
  SET MAP("B","FIELDS")="1"                               ;"BUNDLE"
  SET MAP("I","FIELDS")=".01;2"                           ;"ICD DX"
  SET MAP("P","FIELDS")=".01;1"                           ;"PAGE GROUP"
  SET MAP("O","FIELDS")=""                                ;"ORDERING PROVIDER"
  SET MAP("T","FIELDS")=""                                ;"LAB TIME"
  SET MAP("F","FIELDS")=""                                ;"ORDER FLAG"
  SET MAP("N","FIELDS")=""                                ;"ORDER OPTIONS"
  SET MAP("X","FIELDS")=""                                ;"ITEM DATA"
  SET MAP("E","FIELDS")=""                                ;"TEXT"
  SET MAP("W","FIELDS")=""                                ;"WP FIELD"
  QUIT
  ;
ADDITEM(MODE)  ;"ADD DX, or GROUP (TAB PAGE)
  NEW NAME,TMGFDA,TMGIEN,TMGMSG,IEN
  SET MODE=$GET(MODE)
  NEW INFO DO SETUPINFO2(.INFO,MODE)
  IF $GET(INFO("GRPIEN"))'>0 DO  QUIT
  . WRITE "Aborting.",! 
  WRITE !,"Enter name of NEW "_INFO("PROMPT")_": "  
  READ NAME WRITE ! 
  IF (NAME="")!(NAME["^") DO  QUIT
  . WRITE "No name or invalid name.  Quitting.",!
  ;"First make record entry
  SET TMGFDA(22751,"+1,",.01)=INFO("NAME PREFIX")_NAME
  SET TMGFDA(22751,"+1,",.02)=INFO("TYPE")
  DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  QUIT
  . WRITE !,$$GETERRST^TMGDEBU2(.TMGMSG),!
  . DO PRESS2GO^TMGUSRI2
  SET IEN=+$GET(TMGIEN(1))
  IF IEN'>0 DO  QUIT
  . WRITE "No record created, or new record could not be found.  Quitting",!
  ;"Next, put entry into holder group. 
  SET TMGFDA(22751.01,"+1,"_INFO("GRPIEN")_",",.01)="`"_IEN  
  DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
  IF $DATA(TMGMSG("DIERR")) DO  QUIT
  . WRITE !,"Error while adding new record as ITEM in record: "_INFO("HOLDERGROUPNAME"),!
  . WRITE !,$$GETERRST^TMGDEBU2(.TMGMSG),!
  . WRITE "Aborting",!
  . DO PRESS2GO^TMGUSRI2
  WRITE !,"Record has been created.  Now EDIT details.",!
  IF (MODE="GROUP")!(MODE="BUNDLE") DO
  . WRITE "To have ",INFO("PROMPT")," contain ",INFO("ITEM NAME")," entries, they should be",!
  . WRITE "added into ITEMS field (a multiple-type subfile).",!
  . WRITE "NOTE: Entries should be created elsewhere first, so they are ready for addition here.",!
  DO PRESS2GO^TMGUSRI2
  DO EDIT1REC(IEN)
  DO PRESS2GO^TMGUSRI2  
  QUIT
  ;
DELITEM(MODE,INFO)  ;"Pick and delete GROUP (TAB PAGE)
  ;"Input:  MODE  -- name of mode.
  ;"        INFO  -- optional.  
  NEW MENU,IDX,USRPICK,IEN,RECS,NAME,NOUN
  SET MODE=$GET(MODE)
  IF $DATA(INFO)'>0 DO SETUPINFO2(.INFO,MODE)
  IF $GET(INFO("GRPIEN"))'>0 DO  QUIT
  . WRITE "Aborting.",! 
DGPS1 ;                    
  KILL RECS DO GETRECS(.RECS,INFO("TYPE"))
  IF $DATA(RECS)=0 DO  QUIT
  . WRITE !,!,"Unable find records!",!
  . DO PRESS2GO^TMGUSRI2
  SET IDX=0  
  KILL MENU 
  SET MENU(IDX)="Select Option For DELETING "_INFO("PROMPT")
  SET NAME="" FOR  SET NAME=$ORDER(RECS("NAME",NAME)) QUIT:NAME=""  DO
  . NEW IEN SET IEN=$GET(RECS("NAME",NAME)) QUIT:IEN'>0
  . NEW NAME2 SET NAME2=NAME
  . IF $DATA(INFO("NAME PREFIX")) SET NAME2=$PIECE(NAME,INFO("NAME PREFIX"),2)
  . SET IDX=IDX+1,MENU(IDX)="DELETE "_NAME2_$CHAR(9)_IEN_"^"_NAME2
  WRITE !
  SET USRPICK=$$MENU^TMGUSRI2(.MENU,"^")
  IF USRPICK="^" GOTO DGPSDN 
  SET IEN=+USRPICK,NAME=$PIECE(USRPICK,"^",2)
  IF IEN'>0 GOTO DGPS1
  WRITE !,!,"Are you sure you want to DELETE "_INFO("PROMPT")_" '"_NAME_"'?",!
  WRITE "This will delete all contained references to other "_INFO("NOUN"),!
  WRITE "but will NOT delete any "_INFO("NOUN")_" elements themselves.",!
  WRITE "DELETE" SET %=2 DO YN^DICN WRITE !
  IF %'=1 QUIT
  ;"First delete entry from holder group
  IF $DATA(^TMG(22751,INFO("GRPIEN"),1,IEN))=0 DO  GOTO DGPSDN
  . WRITE "Unable to find "_NAME_" as entry.  Aborting",!  ;"Shouldn't happen...
  NEW DA SET DA=IEN,DA(1)=INFO("GRPIEN")
  NEW DIK SET DIK="^TMG(22751,"_DA(1)_",1,"
  DO ^DIK
  ;"Now delete entry itself.
  SET DIK="^TMG(22751,",DA=IEN 
  DO ^DIK
  GOTO DGPS1
DGPSDN  ;
  QUIT
  ;
SEQVIEW()  ;"Display TMG LAB ORDER (101.41) entries in SEQ# order.  
  NEW TOPIEN SET TOPIEN=$$GETIEN("TMG LAB ORDER")  ;"Search file 101.41
  IF TOPIEN'>0 DO  GOTO SVDN
  . WRITE "Could not find 101.41 entry TMG LAB ORDER",!
  NEW %ZIS
  SET %ZIS("A")="Enter Output Device: "
  SET %ZIS("B")="HOME"
  DO ^%ZIS  ;"standard device call
  IF POP DO  GOTO SVDN
  . WRITE "Error opening output.  Aborting.",!
  USE IO
  WRITE !
  WRITE $$CJ^XLFSTR("SEQUENCE#",10," ")
  WRITE $$CJ^XLFSTR("Dlg Element Type",25," ")
  WRITE $$LJ^XLFSTR("INST#",6," ")
  WRITE " Display Text"
  WRITE !
  WRITE $$CJ^XLFSTR("---------",10," ")
  WRITE $$CJ^XLFSTR("-----------------------",25," ")
  WRITE "------ "
  WRITE "------------"
  WRITE !,!
  ;"Do the output
  NEW SEQNUM SET SEQNUM=0
  FOR  SET SEQNUM=$ORDER(^ORD(101.41,TOPIEN,10,"B",SEQNUM)) QUIT:SEQNUM'>0  DO
  . NEW SUBIEN SET SUBIEN=0
  . FOR  SET SUBIEN=$ORDER(^ORD(101.41,TOPIEN,10,"B",SEQNUM,SUBIEN)) QUIT:SUBIEN'>0  DO
  . . NEW ZN SET ZN=$GET(^ORD(101.41,TOPIEN,10,SUBIEN,0))
  . . NEW DISPTEXT SET DISPTEXT=$PIECE(ZN,"^",4)
  . . SET DISPTEXT=$$RESTORENAME(DISPTEXT)
  . . NEW ITEMPTR SET ITEMPTR=$PIECE(ZN,"^",2)
  . . NEW ITEMSTR SET ITEMSTR=""
  . . IF ITEMPTR>0 SET ITEMSTR=$PIECE($GET(^ORD(101.41,ITEMPTR,0)),"^",1)
  . . NEW INSTNUM SET INSTNUM=$$GET1^DIQ(101.412,SUBIEN_","_TOPIEN_",",22700)
  . . NEW SEQA,SEQB SET SEQA=$PIECE(SEQNUM,".",1),SEQB=$PIECE(SEQNUM,".",2)
  . . WRITE $$RJ^XLFSTR(SEQA,4," ")_"."_$$LJ^XLFSTR(SEQB,4," ")_"  "  ;"Decimal aligned SEQ #
  . . WRITE $$LJ^XLFSTR(ITEMSTR,25," ")
  . . WRITE $$CJ^XLFSTR(INSTNUM,6," ")
  . . WRITE DISPTEXT
  . . WRITE !
  WRITE "------------------------------------------------",!
  WRITE "NOTICE!",!
  WRITE "  Because of the way CPRS processes ORDER DIALOG, any insertion into list",!
  WRITE "  anywhere other then the END of the list will cause stored data values to be",!
  WRITE "  shifted.  Data is stored by [Element Type]+[Instance #]",!
  ;" Close the output device
  DO ^%ZISC
SVDN ;
  DO PRESS2GO^TMGUSRI2
  QUIT
  ;
CMPSEQ(DA)  ;"Return computed field for FILE 22751, FLD 2.3
  NEW RESULT SET RESULT=""
  IF +$GET(DA)'>0 GOTO CSDN
  NEW N21 SET N21=$GET(^TMG(22751,+DA,21))
  NEW IENS101D42 SET IENS101D42=$PIECE(N21,"^",2)  
  NEW IEN101D41 SET IEN101D41=$PIECE(IENS101D42,",",2)
  IF IEN101D41'>0 GOTO CSDN
  NEW SUBIEN SET SUBIEN=+IENS101D42 
  IF SUBIEN'>0 GOTO CSDN
  NEW ZN SET ZN=$GET(^ORD(101.41,IEN101D41,10,SUBIEN,0))
  SET RESULT=$PIECE(ZN,"^",1)
CSDN ;  
  QUIT RESULT
  