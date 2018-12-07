TMGINI01 ;TMG/kst-INI Functions ;5/21/2018
         ;;1.0;TMG-LIB;**1**;5/21/18
 ;
 ;"TMG FUNCTIONS related to TMG APPLICATION PARAMETERS
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 5/21/2018  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"=======================================================================
 ;" RPC -- Public Functions.
 ;"=======================================================================
 ;"GETINIVALUE(DUZ,KEY,DEFAULT) - 
 ;"
GETINIVALUE(DUZ,KEY,DEFAULT)  ;"
 ;"Purpose: This function will return the Section value from 
 NEW VALUE,SECTION
 SET SECTION=$P($G(^VA(200,DUZ,0)),"^",1)
 DO GETINIVL^TMGRPC1A(.VALUE,SECTION,KEY,DEFAULT) 
 SET VALUE=$P(VALUE,"^",2)
 QUIT VALUE
 ;"