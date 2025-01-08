TMGOOW5 ;TMG/kst/OO WINDOW MEMOBOX ;12/1/24
         ;;1.0;TMG-LIB;**1**;11/28/24
  ;
  ;"=======================================================================
  ;" API -- Public Functions.
  ;"=======================================================================
  ;  
  ;"       ---------------------------------------------
  ;"       (SEE ALSO INHERITED ITEMS FROM TMGOOW3)
  ;"       ---------------------------------------------
  ;"       @REF@("TYPE")="LISTBOX"
  ;"       @REF@("ON KEY")="HNDONKEY^TMGOOW5(KEY)"
  ;"       @REF@("ON PAINT")="HNDONPAINT^TMGOOW5(PARENTREF,PARENTLEFT,PARENTTOP)"  
  ;"       @REF@("ON DYN PROP SET: TEXT")="HNDSETTEXT^TMGOOW5(ARRREF)   ;"Setter for TEXT   
  ;"       @REF@("ON DYN PROP GET: TEXT")="HNDGETTEXT^TMGOOW5(OUTREF)   ;"Getter for TEXT
  ;"       @REF@("PROP","LIST",...)  ;storage for LIST information
  ;"=======================================================================
  ;"PRIVATE API FUNCTIONS
  ;"=======================================================================
  ;
  ;"=======================================================================
  ;"=======================================================================
  ;
INITMBTYPE(TYPEREF,SCRNW,SCRNH,BUFFREF) ;"MEMO BOX TYPE INITIATOR
  ;"NOTE: The data put into @TYPEREF will ultimately be copied into an INSTANCE,
  ;"      thus creating the actual object.  @TYPEREF is not the final object. 
  ;"-- Everything that will apply to ALL instances of this object should be put here.
  ;"-- Properties etc that will be unique to object should go into constructor code
  ;  
  DO INITLBTYPE^TMGOOW4(TYPEREF,SCRNW,SCRNH,.BUFFREF) ;"initialize via ancestor type
  ;
  SET @TYPEREF@("TYPE")="MEMOBOX"
  ;  
  SET @TYPEREF@("ON KEY")="HNDONKEY^TMGOOW5(KEY)"
  SET @TYPEREF@("ON DYN PROP SET: TEXT")="HNDSETTEXT^TMGOOW5(PROPNAME,ARRREF)"      
  SET @TYPEREF@("ON DYN PROP GET: TEXT")="HNDGETTEXT^TMGOOW5(PROPNAME,OUTREF)"    
  ;  
  QUIT
  ;
NEWMBOBJ(OBJREF,PARENTREF,TYPEREF)  ;"Constructor for MEMO BOX object -- NO PARAMETERS
  ;"INPUT:  OBJREF -- pass by REFERENCE -- A SOMETIMES-IN AND OUT PARAMETER.  VAR HOLDS REFERENCE NAME
  ;"        PARENTREF -- pass by NAME -- OPTIONAL -- name of object that will own this object
  ;"        TYPEREF -- PASS BY NAME. 
  ;"RESULTS: returns reference to object
  SET OBJREF=$$NEWWINOBJ^TMGOOW3(.OBJREF,PARENTREF,TYPEREF)  ;"call inherited constructor without parameters
  QUIT OBJREF
  ;
  
NEWMBOBJ2(OBJREF,PARENTREF,TYPEREF,LEFT,TOP,WT,HT,TEXT)  ;"Constructor for MEMO BOX object -- with parameters
  ;"INPUT:  OBJREF -- pass by REFERENCE -- A SOMETIMES-IN AND OUT PARAMETER.  VAR HOLDS REFERENCE NAME
  ;"        PARENTREF -- pass by NAME -- OPTIONAL -- name of object that will own this object
  ;"        TYPEREF -- PASS BY NAME. 
  ;"        LEFT,TOP,WT,HT -- POSITION AND SIZE
  ;"        LISTREF -- pass by NAME -- OPTIONAL.  Format:
  ;"            @LISTREF@(#)=<line of text>  
  ;"RESULTS: returns reference to object
  SET OBJREF=$$NEWWINOBJ2^TMGOOW3(.OBJREF,PARENTREF,TYPEREF,LEFT,TOP,WT,HT)  ;"call inherited constructor
  DO SETPROP^TMGOOW2(OBJREF,"TEXT",TEXT) ;"NOTE: actually gets stored as 'LIST' property
  DO SETPROP^TMGOOW2(OBJREF,"USE CURSOR",1)  ;"enable Cursor     
  QUIT OBJREF
  ;
HNDSETTEXT(SELFREF,PROPNAME,TEXTREF)  ;"Handle Dynamic property set
  ;"INPUT: TEXT -- very long string representing entire text in memo box. Linefeeds indicated with LF/$CHAR(10)
  NEW TEMPDATA,DESTREF SET DESTREF="TEMPDATA"
  IF $DATA(@TEXTREF)>=10 DO  ;"i.e. @TEXTREF is an array
  . SET DESTREF=TEXTREF
  ELSE  DO
  . NEW TEXT SET TEXT=$GET(@TEXTREF)
  . NEW START SET START=1
  . NEW WT SET WT=$GET(@SELFREF@("PROP","WIDTH"),10)-2
  . NEW IDX FOR IDX=1:1:$LENGTH(TEXT,$CHAR(10)) DO
  . . NEW SUBSTR SET SUBSTR=$PIECE(TEXT,$CHAR(10),IDX)
  . . DO STR2WP^TMGSTUT2(SUBSTR,DESTREF,WT," ",START)  ;"Wrap substr
  . . SET START=$ORDER(@DESTREF@(""),-1)+1
  DO SETPROP^TMGOOW2(SELFREF,"LIST",DESTREF)
  ;"Below is a temp hack
  NEW OLDCURSORX SET OLDCURSORX=$GET(@SELFREF@("PROP","LIST","CURSOR","X"))
  IF OLDCURSORX="" DO
  . SET @SELFREF@("PROP","LIST","CURSOR","X")=1
  NEW OLDCURSORY SET OLDCURSORY=$GET(@SELFREF@("PROP","LIST","CURSOR","Y"))
  IF OLDCURSORY="" DO
  . SET @SELFREF@("PROP","LIST","CURSOR","Y")=1
  SET @SELFREF@("PROP","LIST","SELECTED")=0 ;" -- Y position of cursor.  If "" or 0, not shown  
  QUIT
  ;
HNDGETTEXT(SELFREF,PROPNAME,OUTREF)    ;"Handle Dynamic property get
  NEW TEMPDATA,SRCREF SET SRCREF="TEMPDATA"
  DO GETPROP^TMGOOW2(SELFREF,"LIST",SRCREF)
  NEW RESULT SET RESULT=""  
  IF $GET(OUTREF)="" GOTO HGTDN
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(@SRCREF@(IDX)) QUIT:IDX'>0  DO
  . NEW LINE SET LINE=$GET(@SRCREF@(IDX))
  . NEW LASTCH SET LASTCH=$EXTRACT(LINE,1,$LENGTH(LINE))
  . ;"IF (LASTCH'=" ")&(LASTCH'=$CHAR(10)) SET LINE=LINE_" "
  . SET RESULT=RESULT_LINE
  SET @OUTREF=RESULT
HGTDN ;  
  QUIT RESULT
  ;                                               
HNDONKEY(SELFREF,KEY)  ;"Event Handler -- KEY
  ;"Handle editing
  NEW LISTREF SET LISTREF=$NAME(@SELFREF@("PROP","LIST"))
  NEW CURSORX SET CURSORX=+$GET(@LISTREF@("CURSOR","X"))
  NEW CURSORY SET CURSORY=+$GET(@LISTREF@("CURSOR","Y"))  
  NEW MAX SET MAX=$ORDER(@LISTREF@("DATA",""),-1)
  IF ",UP,DOWN,LEFT,RIGHT,"[(","_KEY_",") DO  QUIT
  . ;"DO HNDONKEY^TMGOOW4(SELFREF,KEY)  ;"inherited handler    
  . NEW TOP SET TOP=+$GET(@LISTREF@("TOP"))
  . NEW SEL SET SEL=+$GET(@LISTREF@("SELECTED"))
  . NEW HT SET HT=+$GET(@SELFREF@("PROP","HEIGHT"))-2  ;"-2 due to borders
  . NEW WT SET WT=+$GET(@SELFREF@("PROP","WIDTH"))-2  ;"-2 due to borders
  . NEW START SET START=$GET(@LISTREF@("TEXT START"))
  . NEW LONGEST SET LONGEST=+$GET(@LISTREF@("META","LONGEST"))
  . IF "UP,DOWN"[KEY DO
  . . IF KEY="UP" DO
  . . . IF CURSORY>1 SET CURSORY=CURSORY-1
  . . . IF CURSORY<TOP DO
  . . . . SET TOP=CURSORY
  . . ELSE  IF KEY="DOWN" DO
  . . . SET CURSORY=CURSORY+1
  . . . ;"IF CURSORY<MAX SET CURSORY=CURSORY+1
  . . . IF CURSORY>(TOP+HT-1) DO
  . . . . SET TOP=(CURSORY-HT+1)
  . . SET @LISTREF@("CURSOR","Y")=CURSORY
  . . SET @LISTREF@("TOP")=TOP
  . ELSE  IF "LEFT,RIGHT"[KEY DO
  . . IF KEY="RIGHT" DO
  . . . IF CURSORX<LONGEST SET CURSORX=CURSORX+1
  . . . IF CURSORX>(START+WT-1) SET START=CURSORX-WT+1
  . . IF KEY="LEFT" DO
  . . . IF CURSORX>1 SET CURSORX=CURSORX-1
  . . . IF CURSORX<START SET START=CURSORX
  . . SET @LISTREF@("CURSOR","X")=CURSORX
  . . SET @LISTREF@("TEXT START")=START
  ELSE  DO
  . ;"First ensure that we have data encompassing cursor
  . FOR  QUIT:CURSORY<=MAX  DO
  . . SET MAX=MAX+1 SET @LISTREF@("DATA",MAX)=""
  . NEW LINE SET LINE=$GET(@LISTREF@("DATA",CURSORY))
  . FOR  QUIT:$LENGTH(LINE)>=CURSORX  SET LINE=LINE_" "
  . NEW PARTA SET PARTA=$EXTRACT(LINE,1,CURSORX-1)
  . NEW PARTB SET PARTB=$EXTRACT(LINE,CURSORX,$LENGTH(LINE))
  . IF KEY="REMOVE" DO
  . . SET PARTA=$EXTRACT(PARTA,1,$LENGTH(PARTA)-1)
  . . SET KEY=""
  . . SET CURSORX=CURSORX-1   ;"WORRY ABOUT WRAPPING CURSOR LATER...
  . ELSE  DO  
  . . SET CURSORX=CURSORX+1   ;"WORRY ABOUT WRAPPING CURSOR LATER...
  . SET LINE=PARTA_KEY_PARTB
  . SET @LISTREF@("CURSOR","X")=CURSORX
  . SET @LISTREF@("DATA",CURSORY)=LINE
  . DO REFRMETA^TMGOOW4(SELFREF,"LIST")
  ELSE  IF 1=0 DO  
  . NEW ATEXT IF $$GETPROP^TMGOOW2(SELFREF,"TEXT","ATEXT")
  . IF KEY="REMOVE" DO
  . . SET ATEXT=$EXTRACT(ATEXT,1,$LENGTH(ATEXT)-1)
  . ELSE  DO
  . . SET ATEXT=ATEXT_KEY
  . DO SETPROP^TMGOOW2(SELFREF,"TEXT","ATEXT")
  QUIT
  ;
