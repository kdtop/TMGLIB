TMGPXRU1 ;TMG/kst/TMG Reminder Utilities ;3/8/16
         ;;1.0;TMG-LIB;**1**;3/8/16
 ;
 ;"TMG REMINDER FUNCTIONS
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
 ;"DOREM(OUTPUT,DFN,PXRMITEM,PXRHM,DATE)-- Evaluate a reminder
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"DEPENDENCIES
 ;"
 ;"=======================================================================
 ;
DOREM(OUTPUT,DFN,PXRMITEM,PXRHM,DATE)	;"Evaluate a reminder
  ;"NOTE: This was copied from DOREM^PXRMDEV, so it could be modified.  
	N BOP,DEFARR,FIEVAL,FINDING,IND,JND,NL,NOUT,PNAME
	N PXRMDEBG,PXRMDEFS,PXRMID
        SET PXRMID=0  ;"//kt SET TO DEFAULT VALUE TO KEEP FROM CRASHING  1/19/17
	N REF,TEXTOUT,TFIEVAL,TTEXT,X
	;This is a debugging run so set PXRMDEBG.
	S NL=$ORDER(OUTPUT(""))+1
	S PXRMDEBG=1
	D DEF^PXRMLDR(PXRMITEM,.DEFARR)
	I +$G(DATE)=0 D EVAL^PXRM(DFN,.DEFARR,PXRHM,1,.FIEVAL)
	I +$G(DATE)>0 D EVAL^PXRM(DFN,.DEFARR,PXRHM,1,.FIEVAL,DATE)
	;
	I $D(^TMP(PXRMID,$J,"FFDEB")) M FIEVAL=^TMP(PXRMID,$J,"FFDEB") K ^TMP(PXRMID,$J,"FFDEB")
	;
	S TTEXT=^PXD(811.9,PXRMITEM,0)
	S PNAME=$P(TTEXT,U,2)
	I PNAME="" S PNAME=$P(TTEXT,U,1)
	S NL=NL+1,OUTPUT(NL)="Reminder: "_PNAME
	S NL=NL+1,OUTPUT(NL)="Patient: "_$$GET1^DIQ(2,DFN,.01)
	S NL=NL+1,OUTPUT(NL)=" "
	S NL=NL+1,OUTPUT(NL)="The elements of the FIEVAL array are:"
	S REF="FIEVAL"
	D ACOPY^PXRMUTIL(REF,"TTEXT()")
	S IND=0
	F  S IND=$O(TTEXT(IND)) Q:IND=""  D
	. I $L(TTEXT(IND))<79 S NL=NL+1,OUTPUT(NL)=TTEXT(IND) Q
	. D FORMATS^PXRMTEXT(1,79,TTEXT(IND),.NOUT,.TEXTOUT)
	. F JND=1:1:NOUT S NL=NL+1,OUTPUT(NL)=TEXTOUT(JND)
	;
	I $G(PXRMFFSS) D
	. N FFN,STEP
	. S NL=NL+1,OUTPUT(NL)="",NL=NL+1,OUTPUT(NL)=""
	. S NL=NL+1,OUTPUT(NL)="Step-by-step function finding evaluation:"
	. S FFN=""
	. F  S FFN=$O(^TMP("PXRMFFSS",$J,FFN)) Q:FFN=""  D
	.. S NL=NL+1,OUTPUT(NL)=""
	.. S NL=NL+1,OUTPUT(NL)=" Function finding "_FFN_"="_FIEVAL(FFN)
	.. D FORMATS^PXRMTEXT(1,79,$P(FIEVAL(FFN,"DETAIL"),U,2),.NOUT,.TEXTOUT)
	.. F JND=1:1:NOUT S NL=NL+1,OUTPUT(NL)=TEXTOUT(JND)
	.. S NL=NL+1,OUTPUT(NL)=" = "_^TMP("PXRMFFSS",$J,FFN,0)
	.. S NL=NL+1,OUTPUT(NL)="Step  Result"
	.. S STEP=0
	.. F  S STEP=$O(^TMP("PXRMFFSS",$J,FFN,STEP)) Q:STEP=""  D
	... S NL=NL+1,OUTPUT(NL)=$$RJ^XLFSTR(STEP_".",4," ")_"  "_^TMP("PXRMFFSS",$J,FFN,STEP)
	. K ^TMP("PXRMFFSS",$J)
	I $G(PXRMTDEB) D
	. S NL=NL+1,OUTPUT(NL)="",NL=NL+1,OUTPUT(NL)=""
	. S NL=NL+1,OUTPUT(NL)="Term findings:"
	. S REF="TFIEVAL"
	. S FINDING=0
	. F  S FINDING=$O(^TMP("PXRMTDEB",$J,FINDING)) Q:FINDING=""  D
	.. K TFIEVAL M TFIEVAL(FINDING)=^TMP("PXRMTDEB",$J,FINDING)
	.. S NL=NL+1,OUTPUT(NL)="Finding "_FINDING_":"
	.. K TTEXT
	.. D ACOPY^PXRMUTIL(REF,"TTEXT()")
	.. S IND=0
	.. F  S IND=$O(TTEXT(IND)) Q:IND=""  S NL=NL+1,OUTPUT(NL)=TTEXT(IND)
	. K ^TMP("PXRMTDEB",$J)
	;
	I $D(^TMP(PXRMID,$J)) D
	. S NL=NL+1,OUTPUT(NL)="",NL=NL+1,OUTPUT(NL)=""
	. S NL=NL+1,OUTPUT(NL)="The elements of the ^TMP(PXRMID,$J) array are:"
	. S REF="^TMP(PXRMID,$J)"
	. K TTEXT
	. D ACOPY^PXRMUTIL(REF,"TTEXT()")
	. S IND=0
	. F  S IND=$O(TTEXT(IND)) Q:IND=""  S NL=NL+1,OUTPUT(NL)=TTEXT(IND)
	. K ^TMP(PXRMID,$J)
	;
	I $D(^TMP("PXRHM",$J)) D
	. S NL=NL+1,OUTPUT(NL)="",NL=NL+1,OUTPUT(NL)=""
	. S NL=NL+1,OUTPUT(NL)="The elements of the ^TMP(""PXRHM"",$J) array are:"
	. S REF="^TMP(""PXRHM"",$J)"
	. K TTEXT
	. D ACOPY^PXRMUTIL(REF,"TTEXT()")
	. S IND=0
	. F  S IND=$O(TTEXT(IND)) Q:IND=""  S NL=NL+1,OUTPUT(NL)=TTEXT(IND)
	;
	I $D(^TMP("PXRHM",$J)) D CMOUT^PXRMDEV(PXRHM,.NL,.OUTPUT)
	I $D(^TMP("PXRMMHVC",$J)) D MHVOUT^PXRMDEV(PXRHM,.NL,.OUTPUT)
	K ^TMP("PXRM",$J),^TMP("PXRHM",$J),^TMP("PXRMMHVC",$J)
	;"S BOP=$$BORP^PXRMUTIL("P")
	;"I BOP="B" D
	;". S X="IORESET"
	;". D ENDR^%ZISS
	;". D BROWSE^DDBR("OUTPUT","NR","Reminder Test")
	;". W IORESET
	;". D KILL^%ZISS
	;"I BOP="P" D GPRINT^PXRMUTIL("OUTPUT")
	Q
	;

