TMGOOW4 ;TMG/kst/OO WINDOW LISTBOX ;11/28/24
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
  ;"       @REF@("ON KEY")="HNDONKEY^TMGOOW2(KEY)"
  ;"       @REF@("ON PAINT")="HNDONPAINT^TMGOOW3(PARENTREF,PARENTLEFT,PARENTTOP)"  
  ;"       @REF@("ON CURSOR")="HNDONCURSOR^TMGOOW4(CURSOR)"    
  ;"       @REF@("ON DYN PROP SET: LIST")="HNDSETLIST^TMGOOW4(ARRREF)   ;"Setter for LIST   
  ;"       @REF@("ON DYN PROP GET: LIST")="HNDGETLIST^TMGOOW4(OUTREF)   ;"Getter for LIST
  ;"       @REF@("PROP","LIST","DATA",#)=<TEXT>  ;storage for LIST information
  ;"       @REF@("PROP","LIST","TOP")   -- Which line number is drawn at top of display window
  ;"       @REF@("PROP","LIST","SELECTED") -- line number of selected line  
  ;"       @REF@("PROP","LIST","TEXT START") -- x offset.  If 3, then first character drawn is at position 3 in string
  ;"       @REF@("PROP","LIST","META","LONGEST") -- length of LONGEST string in list
  ;"       @REF@("PROP","LIST","META","LENGTH")  -- length of list  
  ;"       @REF@("PROP","LIST","CURSOR","X") -- X position of cursor.  If "" or 0, not shown  
  ;"       @REF@("PROP","LIST","CURSOR","Y") -- Y position of cursor.  If "" or 0, not shown  
  ;"       @REF@("PROP","USE CURSOR",0)
  ;"=======================================================================
  ;"PRIVATE API FUNCTIONS
  ;"=======================================================================
  ;
  ;"=======================================================================
  ;"=======================================================================
  ;
INITLBTYPE(TYPEREF,SCRNW,SCRNH,BUFFREF) ;"LIST BOX TYPE INITIATOR
  ;"NOTE: The data put into @TYPEREF will ultimately be copied into an INSTANCE,
  ;"      thus creating the actual object.  @TYPEREF is not the final object. 
  ;"-- Everything that will apply to ALL instances of this object should be put here.
  ;"-- Properties etc that will be unique to object should go into constructor code
  ;  
  DO INITWINTYPE^TMGOOW3(TYPEREF,SCRNW,SCRNH,.BUFFREF) ;"initialize via ancestor type
  ;
  SET @TYPEREF@("TYPE")="LISTBOX"
  ;  
  SET @TYPEREF@("COLORS","BORDER")=$$COLOR24PAIR^TMGUSRI8("Black","LightSkyBlue",.TMPWCLR) 
  SET @TYPEREF@("COLORS","HEADER")=$$COLOR24PAIR^TMGUSRI8("Red","Beige",.TMPWCLR) 
  SET @TYPEREF@("COLORS","TEXT")=$$COLOR24PAIR^TMGUSRI8("Black","Linen",.TMPWCLR) 
  SET @TYPEREF@("COLORS","SELECTED TEXT")=$$COLOR24PAIR^TMGUSRI8("Black","Yellow",.TMPWCLR) 
  SET @TYPEREF@("COLORS","CURSOR")=$$COLOR24PAIR^TMGUSRI8("White","Skyblue",.TMPWCLR) 
  SET @TYPEREF@("ON PAINT")="HNDONPAINT^TMGOOW4(PARENTREF,PARENTLEFT,PARENTTOP)"
  SET @TYPEREF@("ON CURSOR")="HNDONCURSOR^TMGOOW4(CURSOR)"
  SET @TYPEREF@("ON KEY")="HNDONKEY^TMGOOW4(KEY)"
  SET @TYPEREF@("ON DYN PROP SET: LIST")="HNDSETLIST^TMGOOW4(PROPNAME,ARRREF)"      
  SET @TYPEREF@("ON DYN PROP GET: LIST")="HNDGETLIST^TMGOOW4(PROPNAME,OUTREF)"    
  ;  
  SET @TYPEREF@("LINE CHARS","TL")="$250C"   ;"single, corner
  SET @TYPEREF@("LINE CHARS","TOP")="$2500"  ;"single
  SET @TYPEREF@("LINE CHARS","TR")="$2510"   ;"single, corner
  ;
  QUIT
  ;
NEWLBOBJ(OBJREF,PARENTREF,TYPEREF)  ;"Constructor for LISTBOX object -- NO PARAMETERS
  ;"INPUT:  OBJREF -- pass by REFERENCE -- A SOMETIMES-IN AND OUT PARAMETER.  VAR HOLDS REFERENCE NAME
  ;"        PARENTREF -- pass by NAME -- OPTIONAL -- name of object that will own this object
  ;"        TYPEREF -- PASS BY NAME. 
  ;"RESULTS: returns reference to object
  SET OBJREF=$$NEWWINOBJ^TMGOOW3(.OBJREF,PARENTREF,TYPEREF)  ;"call inherited constructor without parameters
  QUIT OBJREF
  ;
  
NEWLBOBJ2(OBJREF,PARENTREF,TYPEREF,LEFT,TOP,WT,HT,DATAREF)  ;"Constructor for LISTBOX object -- with parameters
  ;"INPUT:  OBJREF -- pass by REFERENCE -- A SOMETIMES-IN AND OUT PARAMETER.  VAR HOLDS REFERENCE NAME
  ;"        PARENTREF -- pass by NAME -- OPTIONAL -- name of object that will own this object
  ;"        TYPEREF -- PASS BY NAME. 
  ;"        LEFT,TOP,WT,HT -- POSITION AND SIZE
  ;"        LISTREF -- pass by NAME -- OPTIONAL.  Format:
  ;"            @LISTREF@(#)=<line of text>  
  ;"RESULTS: returns reference to object
  SET OBJREF=$$NEWWINOBJ2^TMGOOW3(.OBJREF,PARENTREF,TYPEREF,LEFT,TOP,WT,HT)  ;"call inherited constructor
  DO SETPROP^TMGOOW2(OBJREF,"LIST",DATAREF)
  DO SETPROP^TMGOOW2(OBJREF,"USE CURSOR",0) ;"Default to NO cursor for listbox.  
  QUIT OBJREF
  ;
HNDSETLIST(SELFREF,PROPNAME,DATAREF)  ;"Handle Dynamic property set   
  NEW PROPREF SET PROPREF=$NAME(@SELFREF@("PROP",PROPNAME))
  IF $GET(DATAREF)'="",$DATA(@DATAREF)>0 DO
  . KILL @PROPREF@("DATA")
  . MERGE @PROPREF@("DATA")=@DATAREF
  . DO REFRMETA(SELFREF,PROPNAME)
  QUIT
  ;
REFRMETA(SELFREF,PROPNAME)  ;"Refresh metadata about PROPNAME array 
  NEW PROPREF SET PROPREF=$NAME(@SELFREF@("PROP",PROPNAME))
  ;
  NEW MAX SET MAX=$ORDER(@PROPREF@("DATA","@"),-1)
  SET @PROPREF@("META","LENGTH")=MAX
  NEW LONGEST SET LONGEST=+$GET(@PROPREF@("META","LONGEST"))
  IF LONGEST=0 DO
  . NEW IDX SET IDX=0
  . FOR  SET IDX=$ORDER(@PROPREF@("DATA",IDX)) QUIT:IDX'>0  DO
  . . NEW LINE SET LINE=$GET(@PROPREF@("DATA",IDX))
  . . NEW LEN SET LEN=$LENGTH(LINE)
  . . IF LEN>LONGEST SET LONGEST=LEN
  . SET @PROPREF@("META","LONGEST")=LONGEST 
  ;  
  NEW OLDTOP SET OLDTOP=$GET(@PROPREF@("TOP"))
  IF (OLDTOP="")!(OLDTOP>MAX) DO
  . NEW VALUE SET VALUE=1
  . IF OLDTOP>MAX SET VALUE=MAX
  . SET @PROPREF@("TOP")=VALUE  ;"Index of first shown line of text
  ;
  NEW OLDSTART SET OLDSTART=$GET(@PROPREF@("TEXT START"))  
  IF (OLDSTART="")!(OLDSTART>LONGEST) DO
  . NEW VALUE SET VALUE=1
  . IF OLDSTART>LONGEST SET VALUE=LONGEST-1
  . SET @PROPREF@("TEXT START")=VALUE  ;"If 3, then first character drawn is at position 3 in string
  ;
  NEW OLDSEL SET OLDSEL=$GET(@PROPREF@("SELECTED"))
  NEW HT SET HT=+$GET(@SELFREF@("PROP","HEIGHT"))-2  ;"-2 due to borders
  NEW TOP SET TOP=+$GET(@PROPREF@("TOP"))
  IF (OLDSEL="")!(OLDSEL>(TOP+HT-1))!(OLDSEL>MAX) DO  
  . NEW VALUE SET VALUE=1
  . IF OLDSEL>(OLDSEL>(TOP+HT-1)) SET VALUE=(TOP+HT-1)
  . IF OLDSEL>MAX SET VALUE=MAX
  . SET @PROPREF@("SELECTED")=VALUE  ;"Line number of selected 
  ;
  ;"NOTE:  Cursor is not used in listbox, but is descendant objects. 
  NEW OLDCURSORX SET OLDCURSORX=$GET(@PROPREF@("CURSOR","X"))
  IF OLDCURSORX>LONGEST DO
  . SET @PROPREF@("CURSOR","X")=LONGEST
  NEW OLDCURSORY SET OLDCURSORY=$GET(@PROPREF@("CURSOR","Y"))
  IF OLDCURSORY>MAX DO
  . SET @PROPREF@("CURSOR","Y")=MAX
  ;
  QUIT
  ;
HNDGETLIST(SELFREF,PROPNAME,OUTREF)    ;"Handle Dynamic property get
  IF $GET(OUTREF)'="" DO
  . MERGE @OUTREF=@SELFREF@("PROP",PROPNAME,"DATA")
  QUIT $GET(@OUTREF)
  ;                                               
HNDONKEY(SELFREF,KEY)  ;"Event Handler -- KEY
  ;"Plan for descentant objects to override this.
  IF "UP,DOWN,LEFT,RIGHT,"'[KEY QUIT
  NEW LISTREF SET LISTREF=$NAME(@SELFREF@("PROP","LIST"))
  NEW MAX SET MAX=$ORDER(@LISTREF@("DATA",""),-1)
  NEW TOP SET TOP=+$GET(@LISTREF@("TOP"))
  NEW SEL SET SEL=+$GET(@LISTREF@("SELECTED"))
  NEW HT SET HT=+$GET(@SELFREF@("PROP","HEIGHT"))-2  ;"-2 due to borders
  NEW WT SET WT=+$GET(@SELFREF@("PROP","WIDTH"))-2  ;"-2 due to borders
  NEW START SET START=$GET(@LISTREF@("TEXT START"))
  IF "UP,DOWN"[KEY DO
  . IF KEY="UP" DO
  . . IF SEL>1 SET SEL=SEL-1
  . . IF SEL<TOP DO
  . . . SET TOP=SEL
  . ELSE  IF KEY="DOWN" DO
  . . IF SEL<MAX SET SEL=SEL+1
  . . IF SEL>(TOP+HT-1) DO
  . . . SET TOP=(SEL-HT+1)
  . SET @LISTREF@("SELECTED")=SEL
  . SET @LISTREF@("TOP")=TOP
  ELSE  IF "LEFT,RIGHT"[KEY DO
  . IF KEY="RIGHT" DO
  . . NEW LONGEST SET LONGEST=+$GET(@LISTREF@("META","LONGEST"))
  . . IF (START+WT-1)<LONGEST SET START=START+1
  . IF KEY="LEFT" DO
  . . IF START>1 SET START=START-1
  . SET @LISTREF@("TEXT START")=START
  QUIT
  ;
HNDONPAINT(SELFREF,PARENTREF,PARENTLEFT,PARENTTOP) ;"Event handler -- PAINT
  ;"INPUT: SELFREF -- PASS BY NAME -- Added parameter from ACTION() function
  ;"       PARENTOPTION -- PASSED BY REFERENCE
  ;"       PARENTLEFT -- this is SCRN coords (not relative to parent's parent)
  ;"       PARENTTOP  -- this is SCRN coords (not relative to parent's parent)
  IF $GET(SELFREF)="" GOTO HOPDN
  DO HNDONPAINT^TMGOOW3(SELFREF,PARENTREF,PARENTLEFT,PARENTTOP) ;"Call inherited method
  ;"FINISH -- do stuff here for showing list.  
  SET PARENTLEFT=+$GET(PARENTLEFT,0)
  SET PARENTTOP=+$GET(PARENTTOP,0)
  NEW PARENTWT SET PARENTWT=+$GET(@PARENTREF@("PROP","WIDTH"),99999999)
  NEW PARENTHT SET PARENTHT=+$GET(@PARENTREF@("PROP","HEIGHT"),99999999)
  NEW LEFT SET LEFT=$GET(@SELFREF@("PROP","LEFT"),1)  ;"coordinates relative to parent
  NEW TOP SET TOP=$GET(@SELFREF@("PROP","TOP"),1)     ;"coordinates relative to parent
  NEW WT SET WT=+$GET(@SELFREF@("PROP","WIDTH"),10)
  NEW HT SET HT=+$GET(@SELFREF@("PROP","HEIGHT"),10)
  NEW FOCUSED SET FOCUSED=+$GET(@SELFREF@("PROP","DRAW FOCUSED"),0)
  NEW COLORREF SET COLORREF=$NAME(@SELFREF@("COLORS")) 
  IF FOCUSED=1 DO
  . NEW FCOLORREF SET FCOLORREF=$NAME(@SELFREF@("COLORS","FOCUSED"))
  . IF $DATA(@FCOLORREF)=0 DO MODCOLORARR^TMGOOW3(COLORREF,FCOLORREF,0.05)
  . SET COLORREF=FCOLORREF
  KILL @SELFREF@("BOX FILL")  ;"This will prevent painting inside borders.  This object handles differently.   
  DO SETBOUNDS^TMGOOW3(SELFREF,PARENTLEFT+2,PARENTLEFT+PARENTWT-2,PARENTTOP+1,PARENTTOP+PARENTHT-2)
  DO DRAWLIST(LEFT+PARENTLEFT+1,TOP+PARENTTOP+1,WT-2,HT-2,COLORREF,SELFREF)
  ;"NOTE: I am not going to draw any children.  Inherited HNDONPAINT^TMGOOW3
  ;"      will attempt to draw any children, but DRAWLIST will overwrite that
  ;"      I am not designing the LISTBOX object to have any children windows.  
HOPDN  ;
  QUIT
  ;
DRAWLIST(LEFT,TOP,WT,HT,COLORREF,OBJREF)  ;"
  ;"INPUT: LEFT, TOP -- Screen coordinates 
  NEW LISTREF SET LISTREF=$NAME(@OBJREF@("PROP","LIST"))
  NEW DATAREF SET DATAREF=$NAME(@LISTREF@("DATA"))
  NEW PADSTR SET $PIECE(PADSTR," ",WT)=" "
  NEW START SET START=$GET(@LISTREF@("TEXT START")) IF START=0 SET START=1
  NEW LISTTOP SET LISTTOP=+$GET(@LISTREF@("TOP"))
  NEW IDX SET IDX=LISTTOP IF $DATA(@DATAREF@(IDX)) SET IDX=IDX-0.01
  NEW CURSORX SET CURSORX=+$GET(@LISTREF@("CURSOR","X"))-START+1
  NEW CURSORY SET CURSORY=+$GET(@LISTREF@("CURSOR","Y"))
  NEW COUNT SET COUNT=1
  FOR  SET IDX=$ORDER(@DATAREF@(IDX)) QUIT:(IDX'>0)!(COUNT>HT)  DO
  . NEW TEXT SET TEXT=$GET(@DATAREF@(IDX))
  . IF START>1 SET TEXT=$$UNICODEMIDSTR^TMGSTUT3(TEXT,START,WT)  ;"if text is shifted, this will clip left, starting chars
  . NEW LEN SET LEN=$$STRLEN^TMGSTUTL(TEXT)  ;"Unicode aware
  . IF LEN<WT DO
  . . SET TEXT=TEXT_$EXTRACT(PADSTR,1,(WT-LEN))
  . ELSE  IF LEN>WT DO
  . . SET TEXT=$$UNICODEMIDSTR^TMGSTUT3(TEXT,1,WT)  
  . NEW FG,BG,WHICHCOLOR
  . NEW SEL SET SEL=+$GET(@LISTREF@("SELECTED"))
  . SET WHICHCOLOR=$SELECT((SEL-LISTTOP+1)=COUNT:"SELECTED TEXT",1:"TEXT")
  . NEW ONCSRLINE SET ONCSRLINE=((CURSORY-LISTTOP+1)=COUNT)
  . IF $$GETCOLOR^TMGOOW2(WHICHCOLOR,COLORREF,.FG,.BG) ;"ignore result
  . IF ONCSRLINE,CURSORX>0 DO   ;"SPLIT LINE UP INTO Pre-cursor, Cursor, and Post-cursor draws.  
  . . ;"NOTE: TEXT and CURSORX have already been adjusted for START (x shift)
  . . IF CURSORX>1 DO  ;"PRE-CURSOR-TEXT
  . . . NEW PRETEXT SET PRETEXT=$EXTRACT(TEXT,1,CURSORX-1) QUIT:PRETEXT=""
  . . . DO PAINTXY^TMGTERM4(LEFT,TOP+COUNT-1,FG,BG,PRETEXT,OBJREF)
  . . ;"CURSOR-TEXT
  . . NEW CURSORTEXT SET CURSORTEXT=$EXTRACT(TEXT,CURSORX)
  . . IF $$GETCOLOR^TMGOOW2("CURSOR",COLORREF,.FG,.BG) ;"ignore result
  . . DO PAINTXY^TMGTERM4(LEFT+CURSORX-1,TOP+COUNT-1,FG,BG,CURSORTEXT,OBJREF)
  . . ;"POST-CURSOR-TEXT
  . . NEW POSTEXT SET POSTTEXT=$EXTRACT(TEXT,CURSORX+1,$LENGTH(TEXT))
  . . IF $$GETCOLOR^TMGOOW2(WHICHCOLOR,COLORREF,.FG,.BG) ;"ignore result
  . . DO PAINTXY^TMGTERM4(LEFT+CURSORX+1-1,TOP+COUNT-1,FG,BG,POSTTEXT,OBJREF)
  . ELSE  DO
  . . DO PAINTXY^TMGTERM4(LEFT,TOP+COUNT-1,FG,BG,TEXT,OBJREF)
  . SET COUNT=COUNT+1
  ;"End with blank lines if needed to fill page.  
  IF COUNT<HT DO
  . NEW BLANKLINE SET BLANKLINE=$EXTRACT(PADSTR,1,WT)
  . FOR  DO  QUIT:COUNT>=HT  
  . . NEW FG,BG,WHICHCOLOR SET WHICHCOLOR="TEXT"
  . . SET WHICHCOLOR=$SELECT(COUNT=+$GET(@LISTREF@("SELECTED")):"SELECTED TEXT",1:"TEXT")
  . . IF $$GETCOLOR^TMGOOW2(WHICHCOLOR,COLORREF,.FG,.BG) ;"ignore result    
  . . DO PAINTXY^TMGTERM4(LEFT,TOP+COUNT,FG,BG,BLANKLINE,OBJREF)
  . . NEW ONCSRLINE SET ONCSRLINE=((CURSORY-LISTTOP+1)=COUNT)
  . . IF ONCSRLINE DO  ;"Draw colored space for cursor.  
  . . . IF $$GETCOLOR^TMGOOW2("CURSOR",COLORREF,.FG,.BG) ;"ignore result
  . . . DO PAINTXY^TMGTERM4(LEFT+CURSORX-1,TOP+COUNT-1,FG,BG,$EXTRACT(PADSTR,1),OBJREF)
  . . SET COUNT=COUNT+1
  . 
  QUIT
  ;
HNDONCURSOR(SELFREF,CURSOR) ;"Event handler -- CURSOR
  ;
  QUIT
  