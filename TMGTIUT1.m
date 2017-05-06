TMGTIUT1 ;TMG/kst-TIU TEMPLATE Dynamic text support ; 2/2/14, 4/11/17
  ;;1.0;TMG-LIB;**1**;12/30/12
 ;
 ;"Code for handling dynamic TIU TEMPLATES
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
 ;" RPC -- Public Functions.
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;
 ;"=======================================================================
 ;"DEPENDENCIES: TMGDEBU2
 ;"=======================================================================
 ;
 ;"----------------------------------------------------------------------
 ;"----------------------------------------------------------------------
 ;"Discussion
 ;"  Two (2) RPC calls are made for each node: TIU TEMPLATE GETBOIL to get
 ;"    the WP text for the node, and then TIU TEMPLATE GETITEMS to get
 ;"    a list of the node's children.
 ;"  This system is designed such that there will be root node that says
 ;"    "Check here to get started."  This root record in the TIU TEMPLATE
 ;"    file must have code defined in the custom field EXECUTE GET DYNAMIC TEXT,
 ;"    and also the custom field EXECUTE GET DYNAMIC ITEMS.
 ;"  Below is an example output for a parent, child, and grandchild
 ;"    arrangement of nodes.  The example doesn't reflect the output of
 ;"    this code module.  Instead it is output used to design this module.
 
 ;"   TEST <-- #14807
 ;"     Child1 <-- #10334
 ;"       test3 <-- #14813
 ;"       test4 <-- #14814
 ;"     Child2 <-- #14815
 ;"       Grandchild2 <-- #14817
 ;"       Grandchild1 <-- #14816
 ;"   
 ;"   TIU TEMPLATE GETBOIL
 ;"   Params ------------------------------------------------------------------
 ;"   literal 14807
 ;"   Results -----------------------------------------------------------------
 ;"   Here is a parent.
 ;"   
 ;"   
 ;"   TIU TEMPLATE GETITEMS
 ;"   Params ------------------------------------------------------------------
 ;"   literal 14807 
 ;"   Results -----------------------------------------------------------------
 ;"   10334^G^A^Child1^0^^168^1^1^0^0^0^1^0^1^^^0^^^
 ;"   14815^G^A^Child2^0^^168^1^1^0^0^0^1^0^1^^^0^^^
 ;"   
 ;"   
 ;"   TIU TEMPLATE GETBOIL
 ;"   Params ------------------------------------------------------------------
 ;"   literal 10334
 ;"   Results -----------------------------------------------------------------
 ;"   Child 1
 ;"   
 ;"   
 ;"   TIU TEMPLATE GETITEMS
 ;"   Params ------------------------------------------------------------------
 ;"   literal 10334 
 ;"   Results -----------------------------------------------------------------
 ;"   14813^G^A^test3^0^^168^0^1^0^0^0^0^0^0^^^0^^^
 ;"   14814^G^A^test 4^0^^168^0^1^0^0^0^0^0^0^^^0^^^
 ;"   
 ;"   
 ;"   TIU TEMPLATE GETBOIL
 ;"   Params ------------------------------------------------------------------
 ;"   literal 14813 
 ;"   Results -----------------------------------------------------------------
 ;"   This is a test 3
 ;"   
 ;"   
 ;"   TIU TEMPLATE GETBOIL
 ;"   Params ------------------------------------------------------------------
 ;"   literal 14814
 ;"   Results -----------------------------------------------------------------
 ;"   This is a test 4
 ;"   
 ;"   
 ;"   TIU TEMPLATE GETBOIL
 ;"   Params ------------------------------------------------------------------
 ;"   literal 14815
 ;"   Results -----------------------------------------------------------------
 ;"   This is child 2 
 ;"   
 ;"   
 ;"   TIU TEMPLATE GETITEMS
 ;"   Params ------------------------------------------------------------------
 ;"   literal 14815
 ;"   Results -----------------------------------------------------------------
 ;"   14817^G^A^Grandchild2^0^^168^0^1^0^0^0^0^0^0^^^0^^^
 ;"   14816^G^A^Grandchild1^0^^168^0^1^0^0^0^0^0^0^^^0^^^ 
 ;"   
 ;"   
 ;"   TIU TEMPLATE GETBOIL
 ;"   Params ------------------------------------------------------------------
 ;"   literal 14817
 ;"   Results -----------------------------------------------------------------
 ;"   this is grandchild 2 
 ;"   
 ;"   
 ;"   TIU TEMPLATE GETBOIL
 ;"   Params ------------------------------------------------------------------
 ;"   literal 14816
 ;"   Results -----------------------------------------------------------------
 ;"   this is grandchild 1 
 ;"   
 ;"----------------------------------------------------------------------
 ;"----------------------------------------------------------------------
 ;
DODYNTXT(IEN,MODE) ;"DO DYNAMIC TEXT
 ;"PURPOSE: This is event handler code. This is code that will be executed
 ;"  after boilerplate text has been loaded. 
 ;" Called from TIU TEMPLATE GETBOIL rpc call
 ;"Input: IEN -- IEN in 8927 file (TIU TEMPLATE) being worked on
 ;"       MODE -- OPTIONAL.  Default=0
 ;"       If 0, then this is the root node, and children will be prepaired.
 ;"       If 1, then this is a child/grandchild, and data that has been prepaired will be returned 
 ;"
 ;" To modify text that is returned to CPRS, then called 
 ;" code should modify:
 ;"   ^TMP("TIU TEMPLATE",$J)
 ;" Expected format:
 ;"   ^TMP("TIU TEMPLATE",$J,1)=<text of first line>
 ;"   ^TMP("TIU TEMPLATE",$J,2)=<text of second line>
 ;"   ^TMP("TIU TEMPLATE",$J,3)=<text of third line>
 ;"   ^TMP("TIU TEMPLATE",$J,4)= ...
 ;" 
 ;"Result: none
 NEW JOBNUM SET JOBNUM=$J
 SET MODE=+$GET(MODE,0)
 DO DEBUGMSG("IN DODYNTXT^TMGTIUT1. $J="_JOBNUM_", IEN="_IEN_", MODE="_MODE)
 NEW DFN SET DFN=$GET(^DISV(DUZ,"^DPT("))
 IF MODE'=1 GOTO DYNTDN
 NEW NODE SET NODE="TMG DYNAMIC TEMPLATE"
 NEW ARR MERGE ARR=^TMP(NODE,JOBNUM,"USED",IEN)
 IF $DATA(ARR)=0 GOTO DYNTDN
 KILL ^TMP("TIU TEMPLATE",JOBNUM) ;"Delete any non-dynamic text already added prior to this hook code
 NEW TITLE SET TITLE=$GET(ARR("TITLE"))
 NEW LEVEL SET LEVEL=$GET(ARR("LEVEL"))
 DO DEBUGMSG("TITLE="_TITLE_", LEVEL="_LEVEL)
 NEW STR
 IF LEVEL="CHILD" DO
 . DO DEBUGMSG("PROCESSING CHILD")
 . SET STR=TITLE_" -- last review on "_$GET(ARR("DATECOMMENT"))
 . DO ADDTXTLN(STR)
 ELSE  IF LEVEL="GRANDCHILD" DO
 . NEW IEN8925 SET IEN8925=+$GET(ARR("IEN8925"))
 . DO DEBUGMSG("PROCESSING GRANDCHILD. IEN8925="_IEN8925)
 . NEW TEXT MERGE TEXT=^TMP(NODE,JOBNUM,"DOC",IEN8925,"HPI",TITLE)
 . DO DEBUGMSG("$DATA(TEXT)="_$DATA(TEXT))
 . NEW IDX SET IDX=""
 . FOR  SET IDX=$ORDER(TEXT(IDX)) QUIT:(IDX="")  DO
 . . SET STR=$GET(TEXT(IDX))
 . . DO DEBUGMSG("ADDING LINE: "_STR)
 . . DO ADDTXTLN(STR)
 ;
DYNTDN  DO DEBUGMSG("LEAVING DODYNTXT^TMGTIUT1")
 QUIT
 ;
DODYNITM(IEN,MODE) ;"DO DYNAMIC ITEMS
 ;"PURPOSE: This is event handler code. This is code that will be executed
 ;"  after template items (i.e. children templates) have been loaded.
 ;"  Called from TIU TEMPLATE GETITEMS rpc call.
 ;"Input: IEN -- IEN in 8927 file (TIU TEMPLATE)
 ;"       MODE -- OPTIONAL.  Default=0
 ;"       If 0, then this is the root node, and children will be prepaired.
 ;"       If 1, then this is a child, and data that has been prepaired will be returned 
 ;
 ;" To modify list that is returned to CPRS, then called
 ;" code shold modify:
 ;"       ^TMP("TIU TEMPLATE",$J)
 ;" 
 ;" Expected format of output can be complex, and is best
 ;" achieved by calling:
 ;"   ADDNODE^TIUSRVT(.IDX,CHILDIEN)
 ;" 
 ;" Variable IDX: the index to store into array,
 ;"    e.g. ^TMP("TIU TEMPLATE",$J,IDX)=<data>
 ;"    IDX should be 0 on first call when array is empty.
 ;"     (I.e. IDX=0 sets first array index to value of 1).
 ;"
 ;"EXAMPLE, of adding child 10334:  10334^G^A^Child1^0^^168^1^1^0^0^0^1^0^1^^^0^^^
 ;"Description of pieces:
 ;"      1 : IEN
 ;"      2 : Code
 ;"      3 : Active
 ;"      4 : Print name
 ;"      5 : Exclude
 ;"      6 : Gap
 ;"      7 : Personal Owner
 ;"      8 : Children code:
 ;"   1=active
 ;"   2=inactive
 ;"   3=both
 ;"   else=none
 ;"      9 : Dialog
 ;"     10 : Display only
 ;"     11 : First line only
 ;"     12 : One Item Only
 ;"     13 : Hide dialog items
 ;"     14 : Hide items
 ;"     15 : Indent items
 ;"  16,17 : Reminder dialog
 ;"     18 : Lock
 ;"     19 : ComObject
 ;"     20 : ComParams
 ;"     21 : FileLink
 ;" 
 ;" Variable CHILDIEN: the IEN (from file 8927) of the child.
 NEW DFN SET DFN=$GET(^DISV(DUZ,"^DPT("))
 NEW JOBNUM SET JOBNUM=$J
 SET MODE=+$GET(MODE,0)
 KILL ^TMP("TIU TEMPLATE",JOBNUM) ;"Delete any non-dynamic children already added prior to this hook code
 DO DEBUGMSG("IN DODYNITM^TMGTIUT1. $J="_$J_", IEN="_IEN_", MODE="_MODE)
 NEW ENTRIES,CHILDIEN 
 NEW PREPMODE SET PREPMODE=1  ;"1=only most recent instance of given title.
 NEW MINDT SET MINDT=$$GETMINDT()
 NEW CHILDIDX SET CHILDIDX=0
 NEW NODE SET NODE="TMG DYNAMIC TEMPLATE"
 IF MODE'=0 GOTO DYNI2  ;"If not root (parent) branch to DYNI2
 DO PREPDATA(DFN,.ENTRIES,MINDT,PREPMODE)
 NEW IDX SET IDX=""
 FOR  SET IDX=$ORDER(ENTRIES(IDX)) QUIT:(+IDX'>0)  DO
 . NEW STR SET STR=$GET(ENTRIES(IDX)) QUIT:STR=""
 . ;"Format: TITLE|CONTEXT|Date Comment|{{TIUIEN}}
 . SET CHILDIEN=$$GETCHILD(NODE,JOBNUM)
 . DO DEBUGMSG("ADDING CHILD IEN="_CHILDIEN_", STR="_STR)
 . IF CHILDIEN'>0 QUIT  ;"//For now, will ignore any error message.
 . NEW TITLE SET TITLE=$PIECE(STR,"|",1)
 . NEW CONTEXT SET CONTEXT=$PIECE(STR,"|",2)
 . NEW DATEC SET DATEC=$PIECE(STR,"|",3)
 . NEW IEN825 SET IEN8925=$PIECE(STR,"|",4)
 . SET IEN8925=+$PIECE(IEN8925,"{{",2)
 . SET ^TMP(NODE,JOBNUM,"USED",CHILDIEN)=""
 . SET ^TMP(NODE,JOBNUM,"USED",CHILDIEN,"LEVEL")="CHILD"
 . SET ^TMP(NODE,JOBNUM,"USED",CHILDIEN,"TITLE")=TITLE
 . SET ^TMP(NODE,JOBNUM,"USED",CHILDIEN,"PARENT")=IEN
 . SET ^TMP(NODE,JOBNUM,"USED",CHILDIEN,"CONTEXT")=CONTEXT
 . SET ^TMP(NODE,JOBNUM,"USED",CHILDIEN,"DATECOMMENT")=DATEC
 . SET ^TMP(NODE,JOBNUM,"USED",CHILDIEN,"IEN8925")=IEN8925
 . DO ADDNODE^TIUSRVT(.CHILDIDX,CHILDIEN)
 . SET $PIECE(^TMP("TIU TEMPLATE",JOBNUM,CHILDIDX),"^",4)=TITLE
 . SET $PIECE(^TMP("TIU TEMPLATE",JOBNUM,CHILDIDX),"^",7)=DUZ
 . SET $PIECE(^TMP("TIU TEMPLATE",JOBNUM,CHILDIDX),"^",8)=1  ;"children active
 . SET $PIECE(^TMP("TIU TEMPLATE",JOBNUM,CHILDIDX),"^",9)=1  ;"Dialog
 . ;"SET $PIECE(^TMP("TIU TEMPLATE",JOBNUM,CHILDIDX),"^",10)=1  ;"Display only
 . IF $DATA(^TMP(NODE,JOBNUM,"DOC",IEN8925))=0 DO
 . . NEW ARRAY 
 . . DO SUMNOTE^TMGTIUP1(IEN8925,.ARRAY)
 . . MERGE ^TMP(NODE,JOBNUM,"DOC",IEN8925)=ARRAY(IEN8925,"FULL")
 GOTO DYNIDN
 ; 
DYNI2   ;"Handle calls for dynamic items from children and grandchildren nodes.
 NEW LEVEL SET LEVEL=$GET(^TMP(NODE,JOBNUM,"USED",IEN,"LEVEL"))
 DO DEBUGMSG("IN DYNI2^TMGTIUT1.  LEVEL="_LEVEL)
 IF LEVEL'="CHILD" GOTO DYNIDN  ;"Grandchildren don't have any descendent nodes
 ;"Handle creating a grandchild node that uses same info as "child" (the current) node.
 SET CHILDIEN=$$GETCHILD(NODE,JOBNUM)
 IF +CHILDIEN'>0 GOTO DYNIDN
 ;"Mark record as used for this job process and store some metadata
 MERGE ^TMP(NODE,JOBNUM,"USED",CHILDIEN)=^TMP(NODE,JOBNUM,"USED",IEN)
 SET ^TMP(NODE,JOBNUM,"USED",CHILDIEN,"LEVEL")="GRANDCHILD"
 DO DEBUGMSG("IN DYNI2^TMGTIUT1.  ADDED CHILD.  IEN="_CHILDIEN)
 DO ADDNODE^TIUSRVT(.CHILDIDX,CHILDIEN)  ;"add to RPC output
 SET $PIECE(^TMP("TIU TEMPLATE",JOBNUM,CHILDIDX),"^",4)=$GET(^TMP(NODE,JOBNUM,"USED",CHILDIEN,"TITLE"))
 SET $PIECE(^TMP("TIU TEMPLATE",JOBNUM,CHILDIDX),"^",7)=DUZ
 ;
DYNIDN  QUIT
 ;
NEWPT   ;
 ;"Purpose: Event handler when when NEW patient selected.
 ;"  Called from TMGORWPT when patient selected in CPRS
 KILL ^TMP("TIU TEMPLATE",$J)
 QUIT
 ;
 ;"===================================================================
 ;"===================================================================
 ; 
GETMINDT() ;
 ;"Purpose: return minimum date.  E.g. IF only past 2 yrs wanted, return T-2 yr
 ;"  This could later be turned into a stored configuration parameter
 NEW %DT,X
 SET %DT="",X="T-24M"
 DO ^%DT
 QUIT Y
 ;
GETCHILD(NODE,JOBNUM) ;
 ;"Purpose: To return IEN of TIU TEMPLATE record that can be used for 
 ;"  a NEW dynamic child.  
 ;"  The actual dynamic text will NOT be stored in this record,
 ;"  so different processes can make use of the same record
 ;"  at the same time.  The record number will just be a placeholder
 ;"  to reference data stored in ^TMP("TIU TEMPLATE",$J)
 ;"  So this routine just needs to return a template that has not
 ;"  been used by this job process.
 ;"  This routine will take care of ensuring that each child is
 ;"  unique for this job.
 ;"INPUT: NODE -- the node that represents the storage in ^TMP, and the template .01 field name
 ;"RESULT: IEN of record to use, or -1^Message IF error. 
 NEW RESULT SET RESULT=0
 NEW RECNAME SET RECNAME=NODE
 NEW IEN SET IEN=0
 FOR  SET IEN=+$ORDER(^TIU(8927,"B",RECNAME,IEN)) QUIT:(IEN'>0)!(RESULT>0)  DO
 . IF $DATA(^TMP(NODE,JOBNUM,"USED",IEN)) QUIT
 . SET RESULT=IEN
 IF RESULT=0 SET RESULT=$$MAKECHILD(RECNAME)
 IF +RESULT'>0 GOTO GETCHDN
 SET ^TMP(NODE,JOBNUM,"USED",RESULT)=""  ;"Store entry that record is in temp use.  Cleared when patient selected. 
GETCHDN QUIT RESULT
 ;
MAKECHILD(RECNAME) ;"
 ;"Purpose: to make NEW TIU TEMPLATE record that can be used as a scratch
 ;"  record to store dynamic text into.
 ;"Input: RECNAME -- Name of .01 field for newly created record
 ;"Results: IEN of newly created record, or -1^Message IF error.
 NEW RESULT
 NEW TMGFDA,TMGIEN,TMGMSG
 SET TMGFDA(8927,"+1,",.01)=RECNAME    ;"NAME
 SET TMGFDA(8927,"+1,",.03)="GROUP TEMPLATE"  ;"TYPE
 SET TMGFDA(8927,"+1,",.04)="ACTIVE"   ;"STATUS
 SET TMGFDA(8927,"+1,",.05)="NO"       ;"EXCLUDE FROM GROUP BOILERPLATE
 SET TMGFDA(8927,"+1,",.06)="`"_DUZ    ;"PERSONAL OWNDER
 SET TMGFDA(8927,"+1,",.08)="YES"      ;"DIALOG
 SET TMGFDA(8927,"+1,",.09)="NO"       ;"DISPLAY ONLY
 SET TMGFDA(8927,"+1,",.1)="NO"        ;"FIRST LINE
 SET TMGFDA(8927,"+1,",.11)="NO"       ;"ONE ITEM ONLY
 SET TMGFDA(8927,"+1,",.12)="YES"      ;"HIDE DIALOG ITEMS
 SET TMGFDA(8927,"+1,",.13)="NO"       ;"HIDE TREE ITEMS
 SET TMGFDA(8927,"+1,",.14)="YES"      ;"INDENT ITEMS
 SET TMGFDA(8927,"+1,",.16)="NO"       ;"LOCKED
 SET TMGFDA(8927,"+1,",22700)="DO DODYNTXT^TMGTIUT1(TIUDA,1)" ;"EXECUTE GET DYNAMIC TEXT
 SET TMGFDA(8927,"+1,",22701)="DO DODYNITM^TMGTIUT1(TIUDA,1)" ;"EXECUTE GET DYNAMIC ITEMS
 DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
 IF $DATA(TMGMSG) DO  GOTO MKCHDN
 . SET RESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG) 
 SET RESULT=+$GET(TMGIEN(1))
 IF RESULT'>0 SET RESULT="-1^Unable to get IEN of added record"
MKCHDN  QUIT RESULT 
 ;
PREPDATA(DFN,ARRAY,MINDT,MODE) ; 
 ;"Purpose:
 ;"Input: DFN -- IEN in PATIENT file.
 ;"       ARRAY - -PASS BY REFERENCE.  An OUT PARAMETER:  Format:
 ;"    ARRAY(<Index>)=<DISPLAY TEXT>
 ;"       Display text format: TITLE|CONTEXT|Date Comment|{{TIUIEN}}
 ;"       MINDT -- Oldest Date of note to be considered.
 ;"       MODE -- OPTIONAL.  Default=1.  If 1 then only one newest
 ;"   entry for a given title is shown.
 ;"Result: None
 NEW OUT,DT
 SET MODE=+$GET(MODE,1)
 DO GETTOPIC(DFN,.OUT,MINDT)
 NEW TEMPARR
 NEW TITLE SET TITLE="" 
 FOR  SET TITLE=$ORDER(OUT("HPI",TITLE)) QUIT:(TITLE="")  DO
 . NEW DONE SET DONE=0
 . SET DT=""
 . FOR  SET DT=$ORDER(OUT("HPI",TITLE,DT),-1) QUIT:(+DT'>0)!DONE  DO
 . . NEW DATE SET DATE=$$DATECMNT(DT)
 . . NEW CONTEXT SET CONTEXT=$GET(OUT("HPI",TITLE,DT,"CONTEXT"))
 . . NEW TIUIEN SET TIUIEN=+$GET(OUT("HPI",TITLE,DT))
 . . NEW STR SET STR=TITLE_"|"_CONTEXT_"|"_DATE_"|{{"_TIUIEN_"}}"
 . . SET TEMPARR(DT,STR)=""
 . . IF MODE=1 SET DONE=1
 KILL ARRAY
 NEW IDX SET IDX=1
 SET DT=""
 FOR  SET DT=$ORDER(TEMPARR(DT),-1) QUIT:(+DT'>0)  DO
 . NEW STR SET STR=""
 . FOR  SET STR=$ORDER(TEMPARR(DT,STR)) QUIT:(STR="")  DO
 . . SET ARRAY(IDX)=STR
 . . SET IDX=IDX+1 
 QUIT
 ;
DATECMNT(DT) ;
 ;"Purpose: To get a display date string from FM date DT
 NEW RESULT SET RESULT=""
 SET RESULT=$$FMTE^XLFDT(DT,"5D")
 NEW DAYS SET DAYS=$$FMDIFF^XLFDT($$NOW^XLFDT,DT)  ;"returns num of days
 NEW YRS SET YRS=0
 FOR  QUIT:(DAYS'>365)  SET YRS=YRS+1,DAYS=DAYS-365
 NEW MONS SET MONS=0
 FOR  QUIT:(DAYS'>30)  SET MONS=MONS+1,DAYS=DAYS-30
 NEW COMMENT SET COMMENT=""
 IF YRS>0 DO
 . SET COMMENT=COMMENT_YRS_" yr"
 . IF YRS>1 SET COMMENT=COMMENT_"s"
 IF DAYS>14 SET MONS=MONS+1
 IF MONS>0 DO
 . IF COMMENT'="" SET COMMENT=COMMENT_", "
 . SET COMMENT=COMMENT_MONS_" month"
 . IF MONS>1 SET COMMENT=COMMENT_"s"
 ;"IF DAYS>0 DO
 ;". IF COMMENT'="" SET COMMENT=COMMENT_", "
 ;". SET COMMENT=COMMENT_DAYS_" day"
 ;". IF DAYS>1 SET COMMENT=COMMENT_"s"
 SET COMMENT=COMMENT_" ago"
 SET RESULT=RESULT_" (~"_COMMENT_")"
 QUIT RESULT
 ;
GETTOPIC(DFN,OUT,MINDT) ; 
 ;"Purpose: Return array with topics for patient, to display to user
 ;"Input: DFN -- IEN in PATIENT file.
 ;"       OUT - -PASS BY REFERENCE.  An OUT PARAMETER:  Format:
 ;"    OUT("HPI",TITLE,FM-DATETIME)=TIUIEN
 ;"    OUT("HPI",TITLE,FM-DATETIME,"CONTEXT")=<TXT>
 ;"       MINDT -- Oldest Date of note to be considered.
 ;"Result: None
 NEW IEN SET IEN=""
 FOR  SET IEN=+$ORDER(^TMG(22719,"DFN",DFN,IEN)) QUIT:(+IEN'>0)  DO
 . NEW TIUIEN SET TIUIEN=$PIECE($GET(^TMG(22719,IEN,0)),"^",1) 
 . QUIT:(+TIUIEN'>0)
 . NEW DT SET DT=$PIECE($GET(^TIU(8925,TIUIEN,0)),"^",7) QUIT:(DT<MINDT)
 . NEW SUBIEN SET SUBIEN=0
 . FOR  SET SUBIEN=+$ORDER(^TMG(22719,IEN,2,SUBIEN)) QUIT:(SUBIEN'>0)  DO
 . . NEW TITLE SET TITLE=$PIECE($GET(^TMG(22719,IEN,2,SUBIEN,0)),"^",1)
 . . NEW CONTEXT SET CONTEXT=$PIECE($GET(^TMG(22719,IEN,2,SUBIEN,0)),"^",2)
 . . SET OUT("HPI",TITLE,DT)=TIUIEN
 . . SET OUT("HPI",TITLE,DT,"CONTEXT")=CONTEXT
 QUIT
 ;
ADDTXTLN(TXT) ;
 ;"Purpose: to add 1 text line to output
 DO ADDLINE($NAME(^TMP("TIU TEMPLATE",$J)),TXT)
 QUIT
 ;
ADDLINE(REF,TEXT) ;
 ;"Purpose: add text line to end of array.  
 ;"Input: REF -- Close reference, not including index
 NEW CT SET CT=$ORDER(@REF@(""),-1)+1
 SET @REF@(CT)=TEXT
 QUIT
 ;
DEBUGMSG(TXT) ;"Output a debug message
 QUIT  ;"<-- remove this line to restore debug messages
 DO ADDLINE($NAME(^TMG("TMP","DYNAMIC TEMPLATE")),TXT)
 QUIT
 ;
