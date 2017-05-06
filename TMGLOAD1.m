TMGLOAD1 ;TMG/kst-Functions to load ICD and CPT codes in ;11/20/08, 2/2/14
         ;;1.0;TMG-LIB;**1**;11/20/08

 ;"Kevin Toppenberg MD
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
 ;" Public Functions.
 ;"=======================================================================

CPT
        NEW cptArray
        IF $$LoadCPT("cptArray")=0 GOTO CPTDone
        DO CompCPT("cptArray")

CPTDone
        QUIT


CompCPT(pNewCPT)
        ;"Purpose: to compare NEW CPT file with existing values
        ;"Input: name of array, as made by LoadCPT

        WRITE "Prepping..."

        NEW scrlArray,oneLine,oldCPTName
        NEW lineNum SET lineNum=1
        NEW cptNum SET cptNum=""
        FOR  SET cptNum=$ORDER(@pNewCPT@(cptNum)) QUIT:(cptNum="")  do
        . SET oldCPTName=$PIECE($GET(^ICPT(cptNum,0)),"^",2)
        . SET oneLine=cptNum_": "_$GET(@pNewCPT@(cptNum))_" <-- "_oldCPTName
        . SET scrlArray(lineNum,oneLine)=cptNum
        . SET lineNum=lineNum+1

        SET cptNum=0
        FOR  SET cptNum=$ORDER(^ICPT(cptNum)) QUIT:(+cptNum'>0)  do
        . IF $DATA(@pNewCPT@(cptNum)) QUIT
        . SET oldCPTName=$PIECE($GET(^ICPT(cptNum,0)),"^",2)
        . SET oneLine=cptNum_": DELETED <-- "_oldCPTName
        . SET scrlArray(lineNum,oneLine)=cptNum
        . SET lineNum=lineNum+1

        NEW Option
        SET Option("HEADER")="Display of NEW <-- old CPT's"
        DO SCROLLER^TMGUSRIF("scrlArray",.Option)

        QUIT

LoadCPT(pArray)
        ;"Purpose: load a CSV CPT file into an array
        ;"Input: pArray -- PASS BY NAME, the array to be loaded with the data
        ;"              e.g. "MyArray"
        ;"Output: If no error, then output will be:
        ;"      @pArray@(CPT#)=DescrText
        ;"      @pArray@(CPT#)=DescrText
        ;"Results: 1=success, 0=failure

        NEW tempArray
        NEW FPath,FNAME,Result SET Result=0
        IF $$GETFNAME^TMGIOUTL("Select CPT text file",,,,.FPath,.FNAME)="^" GOTO LCDone
        SET Result=$$FTG^%ZISH(FPath,FNAME,"tempArray(0)",1)
        IF Result=0 GOTO LCDone
        NEW lineNum SET lineNum=""
        FOR  SET lineNum=$ORDER(tempArray(lineNum)) QUIT:(lineNum="")  do
        . NEW cptNum SET cptNum=$PIECE($GET(tempArray(lineNum)),$CHAR(9),1)
        . QUIT:(cptNum="")
        . NEW descrText SET descrText=$PIECE($GET(tempArray(lineNum)),$CHAR(9),2)
        . SET @pArray@(cptNum)=descrText
LCDone
        QUIT Result
