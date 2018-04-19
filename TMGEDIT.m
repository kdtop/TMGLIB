TMGEDIT ;TMG/kst/Interface to allow use of linux editor in Fileman ;03/25/06, 2/2/14
         ;;1.0;TMG-LIB;**1**;7/19/08
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"TMG EDITOR FUNCTIONS
 ;
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"$$EDIT(Editor)
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;
EDIT(Editor) ;"DEPRECIATED
  DO EDIT^TMGKERN8(.Editor)
  QUIT
  ;
EDITARR(REF,EDITOR) ;"DEPRECIATED
  DO EDITARR^TMGKERN8(.REF,.EDITOR) ;
  QUIT
  ;
LinuxEdit(Editor,FullPathName)  ;"DEPRECIATED
  DO LinuxEdit^TMGKERN8(Editor,FullPathName)
  QUIT

