TMGTIUS1 ;TMG/kst-TMG VERSIONS OF TIIU CODE ;11/16/16
              ;;1.0;TMG-LIB;**1**;11/16/16
 ;
 ;"TMG VERSION OF TIU CODE. 
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 8/16/2015  Kevin S. Toppenberg MD
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
 ;"
 ;"=======================================================================
 ;" API - Private Functions
 ;"=======================================================================
 ;" 
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"
 ;"=======================================================================
 ;
SETTEXT(TIUY,TIUDA,TIUX,SUPPRESS)	; Save Text - use Buffered I/O
  ;"NOTE: THIS FILE WAS INITIALLY IN TIUSRVPT
  ;"//kt debugging mod
  SET TMGZZDB=0
  IF TMGZZDB=1 DO
  . K TIUDA MERGE TIUDA=^TMG("TMP","SETTEXT^TIUSRVPT","TIUDA")
  . K TIUX MERGE TIUX=^TMG("TMP","SETTEXT^TIUSRVPT","TIUX")
  . K SUPPRESS MERGE SUPPRESS=^TMG("TMP","SETTEXT^TIUSRVPT","SUPPRESS")
  ELSE  DO
  . KILL ^TMG("TMP","SETTEXT^TIUSRVPT")
  . MERGE ^TMG("TMP","SETTEXT^TIUSRVPT","TIUDA")=TIUDA
  . MERGE ^TMG("TMP","SETTEXT^TIUSRVPT","TIUX")=TIUX
  . MERGE ^TMG("TMP","SETTEXT^TIUSRVPT","SUPPRESS")=SUPPRESS
  ;"//kt end debug mod.
	N PAGES,PAGE S TIUY=0,SUPPRESS=$G(SUPPRESS,0)
	I $S(+$G(TIUDA)'>0:1,'$D(^TIU(8925,+TIUDA,0)):1,1:0) D  Q
	. S TIUY="0^0^0^Attempt to file data in a Nonexistent Entry."
	. D ERROR^TIUSRVPT(TIUY)
	S PAGE=$P($G(TIUX("HDR")),U),PAGES=$P($G(TIUX("HDR")),U,2)
	I $S('PAGE:1,'PAGES:1,1:0) D  Q
	. S TIUY="0^0^0^Invalid text block header"
	. D ERROR^TIUSRVPT(TIUY)
	; I PAGE=1 D MERGTEMP^TIUEDI1(TIUDA) K ^TIU(8925,+TIUDA,"TEMP"),^("TEXT")
	I PAGE=1 K ^TIU(8925,+TIUDA,"TEMP")
	M ^TIU(8925,+TIUDA,"TEMP")=TIUX("TEXT")
        KILL ^TIU(8925,+TIUDA,"TEXT")  ;"//kt kill TEXT field
	MERGE ^TIU(8925,+TIUDA,"TEXT")=TIUX("TEXT")  ;"//kt mod to force storage into TEXT
	;if done, commit changes
	I 'SUPPRESS,(PAGE=PAGES),$D(^TIU(8925,TIUDA,"TEMP")) D
	. N TIUC,TIUI,TIU,TIUD12,TIUAU,TIUEC S (TIUC,TIUI)=0
	. F  S TIUI=$O(^TIU(8925,TIUDA,"TEMP",TIUI)) Q:+TIUI'>0  D
	. . S TIUC=TIUC+1
	. I TIUC>0 S ^TIU(8925,TIUDA,"TEMP",0)="^^"_TIUC_U_TIUC_U_DT_"^^"
	. D GETTIU^TIULD(.TIU,TIUDA)
	. K ^TIU(8925,TIUDA,"TEXT")
	. S TIUC=0 F  S TIUC=$O(^TIU(8925,"DAD",TIUDA,TIUC)) Q:+TIUC'>0  D
	. . I +$$ISADDNDM^TIULC1(+TIUC) Q
	. . K ^TIU(8925,+TIUC,"TEXT")
	. D MERGTEXT^TIUEDI1(+TIUDA,.TIU)
	. K ^TIU(8925,TIUDA,"TEMP")
	. ; If user is neither author or expected cosigner, file VBC Line count
	. S TIUD12=$G(^TIU(8925,TIUDA,12)),TIUAU=$P(TIUD12,U,2),TIUEC=$P(TIUD12,U,8)
	. I (TIUAU]""),(DUZ'=TIUAU) D
	. . I (TIUEC]""),(DUZ=TIUEC) Q
	. . D LINES^TIUSRVPT(TIUDA)
	; Acknowledge success / ask for next page
	S TIUY=TIUDA_U_PAGE_U_PAGES
	Q
 
