TMGOOW3 ;TMG/kst/OO GENERIC WINDOW ;11/28/24
         ;;1.0;TMG-LIB;**1**;11/28/24
  ;
  ;"=======================================================================
  ;" API -- Public Functions.
  ;"=======================================================================
  ;  
  ;"NOTE: @REF  (e.g. @OBJREF, @PARENTREF) format:
  ;"
  ;"       ---------------------------------------------
  ;"       (SEE ALSO INHERITED ITEMS FROM TMGOOW2)
  ;"       ---------------------------------------------
  ;"       @REF@("TYPE")="WINDOW"
  ;"       @REF@("WIN") -- ARRAY WITH WINDOWS PROPERTIES.  Format:
  ;"           @REF@("WIN","LEFT")=#    -- X screen position, relative to parent   
  ;"           @REF@("WIN","TOP")=#     -- y screen position, relative to parent
  ;"           @REF@("WIN","WIDTH")=#   -- window width
  ;"           @REF@("WIN","HEIGHT")=#  -- window height
  ;"           @REF@("WIN","THICK") -- 1 means Light  (default)
  ;"                                    2 means Heavy
  ;"                                    3 means Double
  ;"           @REF@("WIN","ARC") -- if 1 then return ARC value, otherwise exclude.  Default=0
  ;"                                    An arc gives a rounded corner
  ;"           @REF@("WIN","DASH") --  0 means mode OFF       Default=0
  ;"                                   1 means double dash
  ;"                                   2 means triple dash
  ;"                                   3 means quadruple dash      
  ;"       @REF@("PROP","CAPTION")=window caption name  
  ;"       @REF@("LINE CHARS")=<array of line drawing chars>  
  ;"       @REF@("UNICODE LINES")=1
  ;"       @REF@("UNICODE LINES","OPTION","THICK")=1
  ;"       @EREF@("BOX FILL","CHAR")=" "
  ;"       @EREF@("BOX FILL","COLORS")=<COLOR_PAIR>                                         
  ;"       @REF@("ON MESSAGE")="HNDONMSG^TMGOOW2(MSGNAME,.DATA)"
  ;"       @REF@("ON KEY")="HNDONKEY^TMGOOW2(KEY)"
  ;"       @REF@("ON SET FOCUS")="HNDONFOCUS^TMGOOW2(OBJREF)"
  ;"       @REF@("ON PAINT")="HNDONPAINT^TMGOOW3(PARENTREF,PARENTLEFT,PARENTTOP)"  
  ;"=======================================================================
  ;"PRIVATE API FUNCTIONS
  ;"=======================================================================
  ;
  ;"=======================================================================
  ;"=======================================================================
  ;
INITWINTYPE(TYPEREF,SCRNW,SCRNH,BUFFREF) ;"WINDOW TYPE INITIATOR
  ;"NOTE: The data put into @TYPEREF will ultimately be copied into an INSTANCE,
  ;"      thus creating the actual object.  @TYPEREF is not the final object. 
  ;"-- Everything that will apply to ALL instances of this object should be put here.
  ;"-- Properties etc that will be unique to object should go into constructor code
  ;  
  DO INITOBJTYPE^TMGOOW2(TYPEREF,SCRNW,SCRNH,.BUFFREF)  ;"initialize ancestor type
  ;
  SET @TYPEREF@("TYPE")="WINDOW"
  ;  
  SET @TYPEREF@("COLORS","BORDER")=$$COLOR24PAIR^TMGUSRI8("Black","LightSkyBlue",.TMPWCLR) 
  SET @TYPEREF@("COLORS","HEADER")=$$COLOR24PAIR^TMGUSRI8("Red","Beige",.TMPWCLR) 
  SET @TYPEREF@("COLORS","CAPTION")=$$COLOR24PAIR^TMGUSRI8("Linen","LightSkyBlue",.TMPWCLR) 
  SET @TYPEREF@("COLORS","MAIN")=$$COLOR24PAIR^TMGUSRI8("Black","LightSkyBlue",.TMPWCLR)
  ;
  SET @TYPEREF@("UNICODE LINES")=1
  SET @TYPEREF@("UNICODE LINES","OPTION","THICK")=1
  ;
  SET @TYPEREF@("LINE CHARS","TL")="$2552"   ;"single vert, double to right
  SET @TYPEREF@("LINE CHARS","TOP")="$2550"  ;"double
  SET @TYPEREF@("LINE CHARS","TR")="$2555"   ;"double to left, single vert
  SET @TYPEREF@("LINE CHARS","SIDE")="$2502" ;"single vert
  SET @TYPEREF@("LINE CHARS","BR")="$2518"   ;"single corner
  SET @TYPEREF@("LINE CHARS","BOT")="$2500"  ;"single
  SET @TYPEREF@("LINE CHARS","BL")="$2514"   ;"single corner
  SET @TYPEREF@("SAVE LINE CHARS")=1  ;"speed up processing on repeat calls to DRAWBOX
  ;
  SET @TYPEREF@("PROP","BOX FILL CH")=" "  ;"default
  SET @TYPEREF@("ON PAINT")="HNDONPAINT^TMGOOW3(PARENTREF,PARENTLEFT,PARENTTOP)"
  ;  
  QUIT  
  ;
NEWWINOBJ(OBJREF,PARENTREF,TYPEREF)  ;"Constructor for WIN object -- NO PARAMETERS
  ;"INPUT:  OBJREF -- pass by REFERENCE -- A SOMETIMES-IN AND OUT PARAMETER.  VAR HOLDS REFERENCE NAME
  ;"        PARENTREF -- pass by NAME -- OPTIONAL -- name of object that will own this object
  ;"        TYPEREF -- PASS BY NAME. 
  ;"RESULTS: returns reference to object
  SET OBJREF=$$NEWGENOBJ^TMGOOW2(.OBJREF,.PARENTREF,TYPEREF)  ;"call inherited constructor
  QUIT OBJREF
  ;  
SETBOUNDS(REF,MINX,MAXX,MINY,MAXY) ;
  SET @REF@("BOUNDS","MINX")=MINX
  SET @REF@("BOUNDS","MAXX")=MAXX
  SET @REF@("BOUNDS","MINY")=MINY
  SET @REF@("BOUNDS","MAXY")=MAXY
  QUIT
  ;  
NEWWINOBJ2(OBJREF,PARENTREF,TYPEREF,LEFT,TOP,WT,HT)  ;"Constructor for WIN object -- WITH INIT PARAMETERS
  ;"INPUT:  OBJREF -- pass by REFERENCE -- A SOMETIMES-IN AND OUT PARAMETER.  VAR HOLDS REFERENCE NAME
  ;"        PARENTREF -- pass by NAME -- OPTIONAL -- name of object that will own this object
  ;"        TYPEREF -- PASS BY NAME. 
  ;"        LEFT,TOP,WT,HT -- POSITION AND SIZE
  ;"RESULTS: returns reference to object
  SET OBJREF=$$NEWWINOBJ(.OBJREF,.PARENTREF,TYPEREF)  ;"call inherited constructor
  SET @OBJREF@("PROP","LEFT")=LEFT  
  SET @OBJREF@("PROP","TOP")=TOP
  SET @OBJREF@("PROP","WIDTH")=WT
  SET @OBJREF@("PROP","HEIGHT")=HT
  QUIT OBJREF
  ;
  ;"------WINDOW OBJECT STUFF --------------------------------------------------
  ;
MODCOLORARR(SOURCEREF,DESTREF,DELTAPCT)  ;"Copy and modify color array
  ;"       DELTAPCT -- percentage, should be +/- 0.0-1.0
  NEW COLORNAME SET COLORNAME=""
  FOR  SET COLORNAME=$ORDER(@SOURCEREF@(COLORNAME)) QUIT:COLORNAME=""  DO
  . NEW COLORPAIR SET COLORPAIR=$GET(@SOURCEREF@(COLORNAME)) QUIT:COLORPAIR=""
  . NEW FG,BG DO SPLITCOLORPAIR^TMGUSRI8(COLORPAIR,.FG,.BG)
  . SET FG=$$DELTACOLOR^TMGUSRI8(FG,DELTAPCT)
  . SET BG=$$DELTACOLOR^TMGUSRI8(BG,DELTAPCT)
  . SET COLORPAIR=FG_"^"_BG
  . SET @DESTREF@(COLORNAME)=COLORPAIR
  QUIT
  ;
HNDONKEY(SELFREF,KEY)  ;"Event Handler -- KEY
  ;"Plan for descentant objects to override this.  
  IF KEY="XYZ" DO  ;"place any KEY handling for this object
  . ;"SOMETHING HERE...
  ELSE  DO  ;"If self doesn't handle KEY event, then pass on to focused child.  
  . NEW FOCUSCHILDREF SET FOCUSCHILDREF=$$FOCUSEDCHILD^TMGOOW2(SELFREF)
  . IF FOCUSCHILDREF="" QUIT
  . IF $$ACTION^TMGOOW2(FOCUSCHILDREF,"KEY",KEY)  ;"ignore result
  QUIT
  ;
HNDONPAINT(SELFREF,PARENTREF,PARENTLEFT,PARENTTOP) ;"Event handler -- PAINT
  ;"INPUT: SELFREF -- PASS BY NAME -- Added parameter from ACTION() function
  ;"       PARENTOPTION -- PASSED BY REFERENCE
  ;"       PARENTLEFT -- this is SCRN coords (not relative to parent's parent)
  ;"       PARENTTOP  -- this is SCRN coords (not relative to parent's parent)
  IF $GET(SELFREF)="" GOTO HOPDN
  NEW %ZZ IF $GET(PARENTREF)="" SET PARENTREF="%ZZ"  ;"something empty
  ;"DRAW SELF WINDOW, with position of self relateive to parent's left and top
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
  . IF $DATA(@FCOLORREF)=0 DO MODCOLORARR(COLORREF,FCOLORREF,0.05)
  . SET COLORREF=FCOLORREF
  DO SETBOUNDS(SELFREF,PARENTLEFT+1,PARENTLEFT+PARENTWT-2,PARENTTOP+1,PARENTTOP+PARENTHT-2)
  DO DRAWWINDOW(LEFT+PARENTLEFT,TOP+PARENTTOP,WT,HT,COLORREF,SELFREF)
  ;
  NEW CHILDIDX SET CHILDIDX=0
  NEW FOCUSIDX SET FOCUSIDX=+$GET(@SELFREF@("CHILDREN","HASFOCUS"))
  FOR  SET CHILDIDX=$ORDER(@SELFREF@("CHILDREN",CHILDIDX)) QUIT:CHILDIDX'>0  DO
  . IF CHILDIDX=FOCUSIDX QUIT  ;"skip for now, so it can be drawn LAST, and appear on top
  . NEW CHILDREF SET CHILDREF=$GET(@SELFREF@("CHILDREN",CHILDIDX)) QUIT:CHILDREF=""
  . IF $$ACTION^TMGOOW2(CHILDREF,"PAINT",SELFREF,LEFT,TOP)  ;"call PAINT for each child not focused 
  IF FOCUSIDX>0 DO
  . NEW CHILDREF SET CHILDREF=$GET(@SELFREF@("CHILDREN",FOCUSIDX)) QUIT:CHILDREF=""
  . IF $$ACTION^TMGOOW2(CHILDREF,"PAINT",SELFREF,LEFT,TOP) 
  
HOPDN ;  
  QUIT "1^OK"
  ;
DRAWWINDOW(LEFT,TOP,WT,HT,COLORREF,OBJREF) ;
  ;"INPUT: LEFT, TOP -- Screen coordinates 
  NEW FG,BG IF $$GETCOLOR^TMGOOW2("BORDER",COLORREF,.FG,.BG) ;"ignore result
  SET @OBJREF@("BOX FILL","CHAR")=$GET(@OBJREF@("PROP","BOX FILL CH"))
  SET @OBJREF@("BOX FILL","COLORS")=$GET(@COLORREF@("MAIN"))
  DO DRAWBOX2^TMGTERM2(LEFT,TOP,WT,HT,FG,BG,OBJREF)
  NEW CAPTION SET CAPTION=$GET(@OBJREF@("PROP","CAPTION"))
  IF CAPTION'="" DO
  . SET CAPTION="%UC%$25C0%UC% "_CAPTION_" %UC%$25B6%UC%"
  . NEW LEN SET LEN=$$STRLEN^TMGSTUTL(CAPTION)
  . IF LEN>(WT-2) DO
  . . SET CAPTION=$$UNICODEMIDSTR^TMGSTUT3(CAPTION,1,(WT-2))
  . NEW X SET X=(LEFT+(WT/2)-(LEN/2)-1)\1
  . IF $$GETCOLOR^TMGOOW2("CAPTION",COLORREF,.FG,.BG)  ;"ignore result
  . DO PAINTXY^TMGTERM4(X,TOP,FG,BG,CAPTION,OBJREF)
  QUIT
  