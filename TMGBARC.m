TMGBARC ;TMG/kst/Barcode Interface to Linux ;12/20/07, 2/2/14, 2/2/14
         ;;1.0;TMG-LIB;**1**;12/20/07
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
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"$$MAKEBC^TMGBARC(Message) -- shell to Linux, to create barcode image.
 ;"$$READBC^TMGBARC(FPathName) -- shell to Linux, to read barcode image.

 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"Test
 ;"=======================================================================

MAKEBC(Message,Option)  ;"DEPRECIATED
  QUIT $$MAKEBC^TMGKERN7(.Message,.Option)

READBC(FPathName)  ;"DEPRECIATED
  QUIT $$READBC^TMGKERN7(.FPathName)

Test
  GOTO Test^TMGKERN7

