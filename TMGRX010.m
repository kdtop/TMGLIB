TMGRX010 ;TMG/kst/Patient allergy listing code; 8/29/25
       ;;1.0;TMG-LIB;**1**;08/06/25
 ;
 ;"Code for dealing with patients allergies
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 8/29/25  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;" 
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"=======================================================================
 ;"Uses:  
 ;"=======================================================================
 ; 
SRCHALLG(Y,CNT,TERM)  ;"
 ;"Purpose: This function will be called from the ORWDAL32 ALLERGY MATCH RPC
 ;"         to search the TMG MEDICATION FILE for the given TERM 
 ;"Input: Y - array containing data
 ;"     CNT - Index for Y
 ;"    TERM - text to search for
 ;"
 ;" Format Notes: First you must create a Top Layer in the format of:
 ;"                 Index_"^"_Display Title_^^^_"TOP"_"^"_+"
 ;"               Then each entry is:
 ;"                 Index_"^"_Display Name_"^"_File Index_"D"_"^"_TitleIndex
 ;"
 SET TERM=$$UP^XLFSTR(TERM)
 ;" Set heading (Index 9998)
 SET CNT=CNT+1,Y(CNT)="9998"_U_"TMG MEDICATION"_U_U_U_"TOP"_U_"+"
 ;"
 ;" Search 22733 B-Index
 NEW IDX SET IDX=0
 FOR  SET IDX=$O(^TMG(22733,IDX)) QUIT:IDX'>0  DO
 . NEW ONETERM SET ONETERM=$P($G(^TMG(22733,IDX,0)),"^",1)
 . IF $$UP^XLFSTR(ONETERM)[TERM DO
 . . SET CNT=CNT+1,Y(CNT)=IDX_"^"_ONETERM_"^TMG(22733,""B"")^D^9998"
 ;" Search 22733 BRAND-Index
 NEW BRANDNAME SET BRANDNAME=""
 FOR  SET BRANDNAME=$O(^TMG(22733,"BRAND",BRANDNAME)) QUIT:BRANDNAME=""  DO
 . IF $$UP^XLFSTR(BRANDNAME)[TERM DO
 . . SET IDX=0
 . . FOR  SET IDX=$O(^TMG(22733,"BRAND",BRANDNAME,IDX)) QUIT:IDX'>0  DO
 . . . SET CNT=CNT+1,Y(CNT)=IDX_"^"_BRANDNAME_"^TMG(22733,""BRAND"")^D^9998"
 QUIT
 ;"
