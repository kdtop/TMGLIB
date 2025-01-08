TMGORWLEX	; ISL/JER - RPCs wrapping Legacy Lexicon APIs;04/11/2024
	;;1.0;ORDER ENTRY/RESULTS REPORTING;**385**;Apr 11, 2024;Build 1
 QUIT
 ;"TMG WRAPPER FUNCTION
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 4/11/2024  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
GETI10DX(ORY,ORX,ORDT)	;" RPC TMG ORWLEX GET10DX
    ;"Purpose: This RPC is a wrapper for ORWLEX GET10DX.
    ;"   After it is called, it will look at each result to see if it is an HCC Code
    DO GETI10DX^ORWLEX(.ORY,ORX,ORDT)
    ;" Look at each result:
    NEW IDX SET IDX=0 
    FOR  SET IDX=$ORDER(ORY(IDX)) QUIT:IDX'>0  DO
    . NEW LINE SET LINE=$G(ORY(IDX))
    . NEW CODE SET CODE=$P(LINE,"^",4)
    . NEW COLOR SET COLOR=$$ICDCOLOR^TMGCMS1(CODE)
    . SET $PIECE(ORY(IDX),"^",15)=COLOR  ;"Note: last used piece by VA code is #9.  We use #15 for future VA expansion
    QUIT
    ;
GETCPT(OUT,SRCHSTR,ADT)  ;"Entry point for RPC TMG CPRS GET CPT
  ;"NOTE: The prior way that CPT was looked up was via RPC ORWPCE4 LEX(x,CodeSys,ADate,ExtInt,True)
  ;"INPUT  OUT   -- OUTPUT ARRAY
  ;"       SRCHSTR  -- terms user has entered for search
  ;"       ADT    -- optional AS OF date
  ;"Output Pieces
  ;"1  ID / LEX IEN       <-- if '+' then shown as a parent node in tree view.  
  ;"2  Code Description   <-- Narrative text
  ;"3  CodeSyst           <-- e.g.  HCPCS for CPT
  ;"4  Code               <-- e.g. 99215
  ;"5  TargetCodeSys      <-- ??
  ;"6  TargetCode         <-- ??
  ;"7  DesignationID      <-- ??
  ;"8  ParentID (if any)  <-- (I this matches parent ID, if this is a child)
  ;"9  CodeIEN            <-- ??
  ;"15 COLOR  //tmg add on.  
  ;"16 ExtendedDescr //tmg add on
  ;"SET OUT(1)="123^SomeCoolCPT code 1^HCPCS^ABC123^^DEF345"
  ;"SET OUT(2)="678^SomeCoolCPT code 2^HCPCS^DEF890^^JKLMNW"
  ;"SET OUT(3)="99213^OfficeVist level 3^HCPS^99214^^^^^^^^^^^^Long Description here\n2nd line"
  NEW IDX SET IDX=1
  NEW ARR DO SEARCHCPT^TMGCPTU1(.ARR,.SRCHSTR)
  NEW IEN SET IEN=0
  FOR  SET IEN=$ORDER(ARR(IEN)) QUIT:IEN'>0  DO
  . NEW ZN SET ZN=$GET(^ICPT(IEN,0))
  . NEW CODE SET CODE=$PIECE(ZN,"^",1)
  . NEW NAME SET NAME=$PIECE(ZN,"^",2)
  . NEW WPSTR,WPIDX SET WPSTR="",WPIDX=0
  . NEW ENTRY SET ENTRY=CODE_"^"_NAME_"^HCPCS^"_CODE
  . FOR  SET WPIDX=$ORDER(^ICPT(IEN,"D",WPIDX)) QUIT:WPIDX'>0  DO
  . . NEW LINE SET LINE=$GET(^ICPT(IEN,"D",WPIDX,0))
  . . SET WPSTR=WPSTR_LINE_"\n"
  . SET $PIECE(ENTRY,"^",16)=WPSTR
  . SET OUT(IDX)=ENTRY,IDX=IDX+1
  QUIT    