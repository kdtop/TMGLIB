TMGOOW1 ;TMG/kst/OO WINDOW SYSTEM ;11/23/24
         ;;1.0;TMG-LIB;**1**;11/23/24
  ;
  ;"=======================================================================
  ;" API -- Public Functions.
  ;"=======================================================================
  ;"NEWWOBJ(OBJREF,PARENTREF,OPTION)  ;
  ;"NEWWINOBJ(OBJREF,PARENTREF,OPTION)  ;
  ;"  INPUT:  OBJREF -- pass by NAME -- AN OUT PARAMETER
  ;"          PARENTREF -- pass by NAME.  OPTIONAL -- name of object that will parent and own this object
  ;"          OPTION -- PASS BY REFERENCE.  <-- all elements of OPTION merged into @OBJREF. See @REF above
  
  ;"=======================================================================
  ;"PRIVATE API FUNCTIONS
  ;"=======================================================================
  ;
  ;"=======================================================================
  ;"=======================================================================
  ;
DEMO ;
  ;"NOTE: WINTYPE is kind of like a 'TYPE' in classic ObjOriented language, and it
  ;"      it is also acting as an array that hold OPTIONS for various functions in this system.  
  ;  
  ;"---------------------------------
  ;"--- NEW global scope vars  ------
  ;"---------------------------------
  NEW TMGROOT    ;"<-- this will be used in global scope to hold objects.  
  NEW ROOTREF SET ROOTREF=$NAME(TMGROOT)
  NEW TMPWCLR  ;"<-- used in global scope to hold color info
  NEW TMGSCRNBUF,BUFFREF SET BUFFREF="TMGSCRNBUF" ;"<--this will be used in global scope for holding output buffer.  
  NEW SCRNH,SCRNW IF $$GETSCRSZ^TMGKERNL(.SCRNH,.SCRNW)  ;"Ignore result
  ;
  ;" -- Init output buffer
  DO INITBUF^TMGTERM4("1^1",SCRNW,SCRNH,"",BUFFREF) ;"could store BUFREF into @WINTYPE, but I'll manually do later...
  ;
  ;"---------------------------------
  ;"--- Initialize TYPE arrays ------
  ;"---------------------------------
  ;"-- Window type
  NEW WINTYPE DO GETTYPE^TMGOOW2("WINDOW","WINTYPE",SCRNW,SCRNH,BUFFREF)  ;"Setup Type array
  ;"-- Child window type
  NEW CHILDWINTYPE DO GETTYPE^TMGOOW2("WINDOW","CHILDWINTYPE",SCRNW,SCRNH,BUFFREF)  ;"Setup Type array
  SET CHILDWINTYPE("COLORS","MAIN")=$$COLOR24PAIR^TMGUSRI8("Black","Linen",.TMPWCLR) 
  ;"SET CHILDWINTYPE("COLORS","NORM")=$$COLOR24PAIR^TMGUSRI8("Black","Linen",.TMPWCLR) 
  ;"SET CHILDWINTYPE("COLORS","MAIN")=$$GETCOLOR^TMGOOW2("NORM",$NAME(CHILDWINTYPE("COLORS")))
  ;"-- Listbox type
  NEW LBTYPE DO GETTYPE^TMGOOW2("LISTBOX","LBTYPE",SCRNW,SCRNH,BUFFREF)  ;"Setup Type array
  ;
  ;"--------------------------------------------
  ;" -- Instantiate objects from TYPE arrays ---
  ;"--------------------------------------------
  NEW WINCT SET WINCT=1
  NEW REFS SET REFS(WINCT)=$$NEWOBJ2^TMGOOW2(ROOTREF,"","WINTYPE",10,18,100,20)  ;"no parent because root
  DO SETPROP^TMGOOW2(REFS(WINCT),"CAPTION","Window"_WINCT) 
  ;" -- add multiple instances of child windows into parent.  
  NEW IDX FOR IDX=1:1:3 DO
  . NEW LEFT SET LEFT=$$RAND(1,40)  NEW TOP SET TOP=$$RAND(1,15)
  . NEW WT   SET WT=$$RAND(15,25)   NEW HT  SET HT=$$RAND(4,8)
  . SET WINCT=WINCT+1
  . IF IDX=1 DO
  . . SET LEFT=2,TOP=2,WT=60,HT=10
  . . NEW TEXTARR DO GETLOREMARR^TMGUSRI9(.TEXTARR,500)  ;"Sample text
  . . SET REFS(WINCT)=$$NEWOBJ2^TMGOOW2("",ROOTREF,"LBTYPE",LEFT,TOP,WT,HT,"TEXTARR")
  . . DO SETPROP^TMGOOW2(REFS(WINCT),"CAPTION","Listbox") 
  . ELSE  DO
  . . SET REFS(WINCT)=$$NEWOBJ2^TMGOOW2("",ROOTREF,"CHILDWINTYPE",LEFT,TOP,WT,HT)
  . . DO SETPROP^TMGOOW2(REFS(WINCT),"CAPTION","Window"_WINCT) 
  . SET REFS(WINCT,"DELTA","X")=$$RAND(-3,3)  ;"movement vector x
  . SET REFS(WINCT,"DELTA","Y")=$$RAND(-3,3)  ;"movement vector y
  ;
  KILL WINTYPE,CHILDWINTYPE,LBTYPE  ;"-- clear up VAR table, no longer needed . All stored in TMGROOT
  ;
  ;"---------------
  ;"-- MAIN LOOP --   
  ;"---------------
  ;
  ;"--- render with animation cycle ---
  DO CSRSHOW^TMGTERM(0)
  NEW DONE SET DONE=0
  FOR  DO  QUIT:DONE
  . NEW WIN SET WIN=2
  . IF 1=1 FOR  SET WIN=$ORDER(REFS(WIN)) QUIT:WIN'>0  DO
  . . NEW AREF SET AREF=REFS(WIN)
  . . NEW X SET X=@AREF@("PROP","LEFT")
  . . NEW Y SET Y=@AREF@("PROP","TOP")
  . . DO ANIMATE($NAME(REFS(WIN,"DELTA")),.X,.Y,REFS(1)) 
  . . SET @AREF@("PROP","LEFT")=X
  . . SET @AREF@("PROP","TOP")=Y
  . DO ACTION^TMGOOW2(ROOTREF,"PAINT")
  . DO BUF2SCRINDIRECT^TMGTERM4(ROOTREF)
  . DO CUP^TMGTERM(1,50) DO COLORS^TMGTERM(-1,-1)
  . NEW KEY,ESC SET KEY=$$READKY^TMGUSRI5("e",0,1,,.ESC) SET:(KEY="") KEY=ESC
  . IF KEY="^" SET DONE=1 QUIT
  . IF KEY'="" DO 
  . . DO CUP^TMGTERM(1,50) WRITE "           "  ;"overwrite spurous chars.  
  . . IF $$ACTION^TMGOOW2(REFS(1),"KEY",KEY)  ;"Send key info to top level window.  Can be sent to focused child from there.  
  ;
  ;"---------------
  ;" -- cleanup --
  ;"---------------
  DO ACTION^TMGOOW2(ROOTREF,"FREE")
  ;
  DO COLORS^TMGTERM(-1,-1) ;"reset colors. 
  DO CUP^TMGTERM(1,1)
  DO CSRSHOW^TMGTERM(1)
  QUIT
  ;
RAND(LO,HI) ;"Return an integer number between LO and HI, but exclude 0
  NEW N SET N=$RANDOM(HI-LO+1)+LO
  FOR  QUIT:N'=0  SET N=$$RAND(LO,HI)
  QUIT N
  ;
ANIMATE(VECREF,X,Y,ROOTREF) ;
  NEW MINX,MINY,MAXX,MAXY
  SET MINX=1
  SET MINY=1
  SET MAXX=@ROOTREF@("PROP","WIDTH")-2
  SET MAXY=@ROOTREF@("PROP","HEIGHT")-2
  
  SET X=X+@VECREF@("X")
  SET Y=Y+@VECREF@("Y")
  IF (X<MINX)!(X>MAXX) DO
  . SET @VECREF@("X")=-1*@VECREF@("X")
  . SET X=$SELECT(X<MINX:MINX,X>MAXX:MAXX)
  IF (Y<MINY)!(Y>MAXY) DO
  . SET @VECREF@("Y")=-1*@VECREF@("Y")
  . SET Y=$SELECT(Y<MINY:MINY,Y>MAXY:MAXY)
  QUIT
  ;
  ;"---------------------------------------------------------------------
  ;"---------------------------------------------------------------------
DEMO2 ;
  NEW TMGROOT    ;"<-- this will be used in global scope to hold objects.  
  NEW ROOTREF SET ROOTREF=$NAME(TMGROOT)
  NEW TMPWCLR  ;"<-- used in global scope to hold color info
  NEW TMGSCRNBUF,BUFFREF SET BUFFREF="TMGSCRNBUF" ;"<--this will be used in global scope for holding output buffer.
  ;
  DO INITBYFORM(ROOTREF)  ;"Read form, then instantiate objects based on form.
 ;"---------------
  ;"-- MAIN LOOP --   
  ;"---------------
  ;
  ;"--- render with animation cycle ---
  DO CSRSHOW^TMGTERM(0)
  NEW DONE SET DONE=0
  FOR  DO  QUIT:DONE
  . NEW WIN SET WIN=2
  . DO ACTION^TMGOOW2(ROOTREF,"PAINT")
  . DO BUF2SCRINDIRECT^TMGTERM4(ROOTREF)
  . DO CUP^TMGTERM(1,50) DO COLORS^TMGTERM(-1,-1)
  . NEW KEY,ESC SET KEY=$$READKY^TMGUSRI5("e",0.001,1,,.ESC) SET:(KEY="") KEY=ESC
  . IF KEY'="" WRITE KEY,!
  . IF KEY="^" SET DONE=1 QUIT
  . IF (KEY'=""),(KEY'=-1) DO 
  . . DO CUP^TMGTERM(1,50) WRITE "           "  ;"overwrite spurous chars.  
  . . IF $$ACTION^TMGOOW2(ROOTREF,"KEY",KEY)  ;"Send key info to top level window.  Can be sent to focused child from there.  
  ;
  ;"---------------
  ;" -- cleanup --
  ;"---------------
  DO ACTION^TMGOOW2(ROOTREF,"FREE")
  ;
  DO COLORS^TMGTERM(-1,-1) ;"reset colors. 
  DO CUP^TMGTERM(1,1)
  DO CSRSHOW^TMGTERM(1)
  QUIT
  ;
DEMO3 ;  
  NEW TMGROOT    ;"<-- this will be used in global scope to hold objects.  
  NEW ROOTREF SET ROOTREF=$NAME(TMGROOT)
  NEW TMPWCLR  ;"<-- used in global scope to hold color info
  NEW TMGSCRNBUF,BUFFREF SET BUFFREF="TMGSCRNBUF" ;"<--this will be used in global scope for holding output buffer.
  ;
  DO INITBYFORM(ROOTREF)  ;"Read form, then instantiate objects based on form.
 ;"---------------
  ;"-- MAIN LOOP --   
  ;"---------------
  ;
  ;"--- render with animation cycle ---
  DO CSRSHOW^TMGTERM(0)
  NEW DONE SET DONE=0
  FOR  DO  QUIT:DONE
  . NEW WIN SET WIN=2
  . ;"DO ACTION^TMGOOW2(ROOTREF,"PAINT")
  . ;"DO BUF2SCRINDIRECT^TMGTERM4(ROOTREF)
  . ;"HANG 0.2
  . DO CUP^TMGTERM(1,50) DO COLORS^TMGTERM(-1,-1)
  . NEW KEY,ESC SET KEY=$$READKY^TMGUSRI5("e",0.001,1,,.ESC) SET:(KEY="") KEY=ESC
  . IF KEY'="" WRITE KEY,!
  . IF KEY="^" SET DONE=1 QUIT
  . IF (KEY'=""),(KEY'=-1) DO 
  . . DO CUP^TMGTERM(1,50) WRITE "           "  ;"overwrite spurous chars.  
  . . IF $$ACTION^TMGOOW2(ROOTREF,"KEY",KEY)  ;"Send key info to top level window.  Can be sent to focused child from there.  
  ;
  ;"---------------
  ;" -- cleanup --
  ;"---------------
  DO ACTION^TMGOOW2(ROOTREF,"FREE")
  ;
  DO COLORS^TMGTERM(-1,-1) ;"reset colors. 
  DO CUP^TMGTERM(1,1)
  DO CSRSHOW^TMGTERM(1)
  QUIT
  ;
FORM ;
  ;;"[BUFFER]        ;<--- must have section [BUFFER] if want buffered output (needed!)
  ;;". TOP=1
  ;;". LEFT=1
  ;;". WIDTH=[SCRNW]   ;<-- auto get screen width
  ;;". HEIGHT=[SCRNH]  ;<-- auto get screen height
  ;;". STORE=TMGSCRNBUF   ;<--will NEW variable at runtime. 
  ;;"
  ;;"[MAIN]: [WINDOW]   ; <--- must have [MAIN] section for main window
  ;;". BUFFERED="TMGSCRNBUF"  ;<-- same name as in [BUFFER] above
  ;;". LEFT=10
  ;;". TOP=18
  ;;". WIDTH=100
  ;;". HEIGHT=25
  ;;". CAPTION="Window 1"
  ;;". ;
  ;;". [CHILD]: [LISTBOX]
  ;;". . LEFT=2
  ;;". . TOP=2
  ;;". . WIDTH=60
  ;;". . HEIGHT=10
  ;;". . CAPTION="Listbox"
  ;;". . LIST: [ARRAY]=      
  ;;". . . 1="Lorem ipsum dolor sit amet; consectetur adipiscing elit," 
  ;;". . . 2="sed do eiusmod tempor incididunt ut labore et dolore magna" 
  ;;". . . 3="aliqua. Ut enim ad minim veniam, quis nostrud exercitation" 
  ;;". . . 4="ullamco laboris nisi ut aliquip ex ea commodo consequat." 
  ;;". . . 5="Duis aute irure dolor in reprehenderit in voluptate velit" 
  ;;". . . 6="esse cillum dolore eu fugiat nulla pariatur. Excepteur sint" 
  ;;". . . 7="occaecat cupidatat non proident, sunt in culpa qui officia" 
  ;;". . . 8="deserunt mollit anim id est laborum."
  ;;". ;  
  ;;". [CHILD]: [WINDOW]
  ;;". . LEFT=2
  ;;". . TOP=13
  ;;". . WIDTH=20
  ;;". . HEIGHT=10
  ;;". . CAPTION="Window 2"
  ;;". . [COLORS]
  ;;". . . MAIN="Black^Linen"
  ;;". ;
  ;;". [CHILD]: [MEMOBOX]
  ;;". . LEFT=25
  ;;". . TOP=13
  ;;". . WIDTH=30
  ;;". . HEIGHT=10
  ;;". . CAPTION="Memo Box"
  ;;". . TEXT: [ARRAY]=      
  ;;". . . 1="A quick brown fox" 
  ;;". . . 2="Jumped over the lazy dog" 
  ;;". . [COLORS]
  ;;". . . MAIN="Black^Linen"
  ;;"<DONE>
  ;
INITBYFORM(ROOTREF)  ;"Read form, then instantiate objects based on form.
  ;"INPUT:  ROOTREF -- pass by NAME.  Name of variable where everything will be stored
  ;"        FORMARR -- pass by NAME.  Name of variable holding form.  Format:
  ;"        @FORMARR@(#)=<line of form>  See example FORM for syntax details.
  NEW SCRNW,SCRNH ;"<-- used in global scope downstream.  
  NEW FORM DO READFORM("FORM")
  NEW PARSED DO PARSEFORM(.PARSED,"FORM") KILL FORM
  NEW BUFF MERGE BUFF=PARSED("BUFFER") IF $DATA(BUFF) DO
  . DO INITBUFFER(ROOTREF,.BUFF)
  NEW MAIN MERGE MAIN=PARSED("MAIN") IF $DATA(MAIN) DO
  . DO INITWINDOW(ROOTREF,"",.MAIN)
  QUIT                                                     
  ;
INITWINDOW(ROOTREF,PARENTREF,INITARR) ;
  ;"INPUT:  ROOTREF -- pass by NAME.  Name of variable where everything will be stored
  ;"        INITARR -- parsed array from form.  Format:
  ;"          }~PROP        
  ;"          | }~BUFFERED = TMGSCRNBUF
  ;"          | }~CAPTION = Window 1                                                                                                                                                                      ^[ | }~HEIGHT = 20
  ;"          | }~LEFT = 10
  ;"          | }~TOP = 18
  ;"          | }~WIDTH = 100
  ;"          }~TYPE = [WINDOW]
  ;"          }~CHILD
  ;"          | }~1 
  ;"          | | }~PROP
  ;"          | | | }~CAPTION = Listbox
  ;"          | | | }~LIST = [ARRAY]
  ;"          | | | | }~1 = Lorem ipsum dolor sit amet; consectetur adipiscing elit,
  ;"          | | | | }~2 = sed do eiusmod tempor incididunt ut labore et dolore magna
  ;"          | | | | }~3 = aliqua. Ut enim ad minim veniam, quis nostrud exercitation
  ;"          | | | | }~4 = ullamco laboris nisi ut aliquip ex ea commodo consequat.
  ;"          | | | | }~5 = Duis aute irure dolor in reprehenderit in voluptate velit
  ;"          | | | | }~6 = esse cillum dolore eu fugiat nulla pariatur. Excepteur sint
  ;"          | | | | }~7 = occaecat cupidatat non proident, sunt in culpa qui officia
  ;"          | | | | }~8 = deserunt mollit anim id est laborum.
  ;"          | | | }~HEIGHT = 10
  ;"          | | | }~LEFT = 2
  ;"          | | | }~TOP = 2
  ;"          | | | }~WIDTH = 60
  ;"          | | }~TYPE = [LISTBOX]
  ;"          | }~2   
  ;"          | | }~COLORS
  ;"          | | | }~NORM = 0;0;0^250;240;230  
  ;"          | | }~PROP
  ;"          | | | }~CAPTION = Window 2
  ;"          | | | }~HEIGHT = 10                                                                                                                                                                         ^[ | | | }~LEFT = 2
  ;"          | | | }~TOP = 15
  ;"          | | | }~WIDTH = 20
  ;"          | | }~TYPE = [WINDOW]
  ;"          | }~3 
  ;"          |  }~PROP
  ;"          |  | }~CAPTION = Window 3
  ;"          |  | }~HEIGHT = 10
  ;"          |  | }~LEFT = 2
  ;"          |  | }~TOP = 15
  ;"          |  | }~WIDTH = 20
  ;"          |  }~TYPE = WINDOW
  NEW TOP,LEFT,WIDTH,HEIGHT,TYPE,BUFFREF
  SET TOP=$GET(INITARR("PROP","TOP"),1) 
  SET LEFT=$GET(INITARR("PROP","LEFT"),1) 
  SET WIDTH=$GET(INITARR("PROP","WIDTH"),40)
  SET HEIGHT=$GET(INITARR("PROP","HEIGHT"),20)
  SET BUFFREF=$GET(INITARR("PROP","BUFFERED")) KILL INITARR("PROP","BUFFERED")
  IF BUFFREF="" DO
  . IF $GET(PARENTREF)'="" SET BUFFREF=$GET(@PARENTREF@("BUFFERED"))
  . IF BUFFREF="" SET BUFFREF="TMGZZSCRNBUFFER"
  SET TYPE=$GET(INITARR("TYPE"),"WINDOW") KILL INITARR("TYPE")
  IF $EXTRACT(TYPE,1)="[" SET TYPE=$PIECE($PIECE(TYPE,"[",2),"]",1)
  NEW TYPEARR DO GETTYPE^TMGOOW2(TYPE,"TYPEARR",WIDTH,HEIGHT,BUFFREF)  ;"Setup Type array
  NEW OBJREF SET OBJREF=$$NEWOBJ^TMGOOW2(ROOTREF,.PARENTREF,"TYPEARR")  
  NEW APROP SET APROP=""
  FOR  SET APROP=$ORDER(INITARR("PROP",APROP)) QUIT:APROP=""  DO
  . NEW VALUE,VALUEARR SET VALUE=$GET(INITARR("PROP",APROP))
  . IF VALUE["[ARRAY]" DO
  . . SET VALUE="VALUEARR" MERGE VALUEARR=INITARR("PROP",APROP)
  . DO SETPROP^TMGOOW2(OBJREF,APROP,VALUE)
  . KILL INITARR("PROP",APROP)     
  IF $DATA(INITARR("COLORS")) DO
  . MERGE @OBJREF@("COLORS")=INITARR("COLORS")
  ;
  ;"Now instantiate any children
  NEW CHILDIDX SET CHILDIDX=""
  FOR  SET CHILDIDX=$ORDER(INITARR("CHILD",CHILDIDX)) QUIT:CHILDIDX'>0  DO
  . NEW SUBINITARR MERGE SUBINITARR=INITARR("CHILD",CHILDIDX)
  . DO INITWINDOW(ROOTREF,OBJREF,.SUBINITARR) 
  ;
  QUIT           
  ;
INITBUFFER(ROOTREF,INITARR) ;
  ;"INPUT:  ROOTREF -- pass by NAME.  Name of variable where everything will be stored
  ;"        INITARR -- parsed array from form.  Format:
  ;"           }~PROP
  ;"            }~HEIGHT = 45
  ;"            }~LEFT = 1
  ;"            }~STORE = TMGSCRNBUF
  ;"            }~TOP = 1
  ;"            }~WIDTH = 191
  NEW TOP,LEFT,WIDTH,HEIGHT,BUFFREF
  SET TOP=$GET(INITARR("PROP","TOP"),1)
  SET LEFT=$GET(INITARR("PROP","LEFT"),1)
  SET WIDTH=$GET(INITARR("PROP","WIDTH"),40)
  SET HEIGHT=$GET(INITARR("PROP","HEIGHT"),20)
  SET BUFFREF=$GET(INITARR("PROP","STORE"),"TMGZZSCRNBUFFER")
  DO INITBUF^TMGTERM4(TOP_"^"_LEFT,WIDTH,HEIGHT,"",BUFFREF)
  QUIT
  ;
READFORM(OUTREF) ;"Read from FORM label and output into OUTREF
  IF $GET(OUTREF)="" QUIT
  KILL @OUTREF                          
  NEW DONE SET DONE=0
  NEW IDX FOR IDX=1:1 DO  QUIT:DONE
  . SET LINE=$TEXT(FORM+IDX^TMGOOW1)
  . SET LINE=$PIECE(LINE,";;""",2)
  . IF LINE["<DONE>" SET DONE=1 QUIT
  . SET @OUTREF@(IDX)=LINE
  QUIT
  ;  
PARSEFORM(DEST,ARRREF) ;"Load and instantiate objects based on form.
  ;"INPUT:  DEST -- pass by REFERENCE.  Variable where parsed items will be stored
  ;"        Example output from form found at label FORM above
  ;"            }~BUFFER
  ;"            | }~PROP
  ;"            |  }~HEIGHT = 45
  ;"            |  }~LEFT = 1
  ;"            |  }~STORE = TMGSCRNBUF
  ;"            |  }~TOP = 1
  ;"            |  }~WIDTH = 191
  ;"            }~MAIN
  ;"             }~CHILD
  ;"             | }~1 
  ;"             | | }~PROP
  ;"             | | | }~CAPTION = Listbox
  ;"             | | | }~LIST = [ARRAY]
  ;"             | | | | }~1 = Lorem ipsum dolor sit amet; consectetur adipiscing elit,
  ;"             | | | | }~2 = sed do eiusmod tempor incididunt ut labore et dolore magna
  ;"             | | | | }~3 = aliqua. Ut enim ad minim veniam, quis nostrud exercitation
  ;"             | | | | }~4 = ullamco laboris nisi ut aliquip ex ea commodo consequat.
  ;"             | | | | }~5 = Duis aute irure dolor in reprehenderit in voluptate velit
  ;"             | | | | }~6 = esse cillum dolore eu fugiat nulla pariatur. Excepteur sint
  ;"             | | | | }~7 = occaecat cupidatat non proident, sunt in culpa qui officia
  ;"             | | | | }~8 = deserunt mollit anim id est laborum.
  ;"             | | | }~HEIGHT = 10
  ;"             | | | }~LEFT = 2
  ;"             | | | }~TOP = 2
  ;"             | | | }~WIDTH = 60
  ;"             | | }~TYPE = [LISTBOX]
  ;"             | }~2   
  ;"             | | }~PROP
  ;"             | | | }~CAPTION = Window 2
  ;"             | | | }~HEIGHT = 10                                                                                                                                                                         ^[ | | | }~LEFT = 2
  ;"             | | | }~TOP = 15
  ;"             | | | }~WIDTH = 20
  ;"             | | }~TYPE = [WINDOW]
  ;"             | }~3 
  ;"             |  }~PROP
  ;"             |  | }~CAPTION = Window 3
  ;"             |  | }~HEIGHT = 10
  ;"             |  | }~LEFT = 2
  ;"             |  | }~TOP = 15
  ;"             |  | }~WIDTH = 20
  ;"             |  }~TYPE = WINDOW
  ;"             }~PROP        
  ;"             | }~BUFFERED = TMGSCRNBUF
  ;"             | }~CAPTION = Window 1                                                                                                                                                                      ^[ | }~HEIGHT = 20
  ;"             | }~LEFT = 10
  ;"             | }~TOP = 18
  ;"             | }~WIDTH = 100
  ;"             }~TYPE = [WINDOW]
  ;"            
  ;"        ARRREF -- pass by NAME.  Name of variable holding form.  Format:
  ;"          @ARRREF@(#)=<line of form>  See example FORM for syntax details.
  ;"NOTE: uses SCRNW,SCRNH,TMGWCLR in global scope.  
  IF $GET(ARRREF)="" QUIT
  IF $DATA(@ARRREF)=0 QUIT
  NEW ARR MERGE ARR=@ARRREF
  KILL DEST
  NEW IDX SET IDX=""
  FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:IDX'>0  DO
  . NEW LINE SET LINE=$GET(ARR(IDX)) QUIT:LINE=""
  . SET LINE=$$TRIM^XLFSTR(LINE)
  . IF $EXTRACT(LINE,1)="[" DO
  . . NEW BLOCK DO CUTSECTION(.ARR,IDX,.BLOCK)
  . . NEW PARAMS DO PARSEBLOCK(.BLOCK,.PARAMS)
  . . NEW CMD SET CMD=$PIECE($PIECE(LINE,"[",2),"]",1)
  . . MERGE DEST(CMD)=PARAMS
  QUIT
  ;
PARSEBLOCK(BLOCK,OUT) ;"     
  NEW BLOCKTYPE SET BLOCKTYPE=""
  NEW BLOCKNAME SET BLOCKNAME=""
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(BLOCK(IDX)) QUIT:IDX'>0  DO
  . NEW LINE SET LINE=$GET(BLOCK(IDX)) QUIT:LINE=""
  . NEW LEVEL SET LEVEL=$$NUMDOTS(LINE)
  . IF LEVEL>0 SET LINE=$$STRIPDOTS(LINE,LEVEL)
  . IF LINE[";" SET LINE=$$TRIMCOMMENT(LINE) QUIT:LINE=""
  . NEW KEY SET KEY=$PIECE(LINE,"=",1)
  . NEW TYPE SET TYPE=""
  . IF KEY[":" SET TYPE=$$TRIM^XLFSTR($PIECE(KEY,":",2)),KEY=$PIECE(KEY,":",1)
  . NEW VALUE SET VALUE=$$TRIM^XLFSTR($PIECE(LINE,"=",2,999))
  . IF $EXTRACT(VALUE,1)="""",$EXTRACT(VALUE,$LENGTH(VALUE))="""" SET VALUE=$EXTRACT(VALUE,2,$LENGTH(VALUE)-1)
  . IF LEVEL=0 DO
  . . SET BLOCKNAME=KEY,BLOCKTYPE=TYPE
  . . IF TYPE="[ARRAY]" SET OUT(BLOCKNAME)=BLOCKTYPE QUIT
  . . IF (VALUE'="") SET OUT("VALUE")=VALUE
  . . IF (TYPE'="") SET OUT("TYPE")=TYPE
  . ELSE  DO
  . . IF VALUE="[SCRNW]" SET VALUE=$$GETSCRNW()
  . . IF VALUE="[SCRNH]" SET VALUE=$$GETSCRNH()
  . . IF KEY="[COLORS]" DO  QUIT
  . . . NEW SUBBLOCK DO CUTSECTION(.BLOCK,IDX,.SUBBLOCK)
  . . . NEW SUBPARAMS DO PARSEBLOCK(.SUBBLOCK,.SUBPARAMS)
  . . . MERGE OUT("COLORS")=SUBPARAMS("PROP")
  . . . NEW ACOLOR SET ACOLOR="" 
  . . . FOR  SET ACOLOR=$ORDER(OUT("COLORS",ACOLOR)) QUIT:ACOLOR=""  DO
  . . . . NEW ENTRY SET ENTRY=$GET(OUT("COLORS",ACOLOR)) QUIT:ENTRY=""
  . . . . IF $$ISCOLORPAIR^TMGUSRI8(ENTRY) QUIT
  . . . . SET ENTRY=$$COLOR24PAIR^TMGUSRI8($PIECE(ENTRY,"^",1),$PIECE(ENTRY,"^",2),.TMGWCLR)
  . . . . SET OUT("COLORS",ACOLOR)=ENTRY
  . . IF TYPE="[ARRAY]" DO  QUIT
  . . . NEW SUBBLOCK DO CUTSECTION(.BLOCK,IDX,.SUBBLOCK)
  . . . NEW SUBPARAMS DO PARSEBLOCK(.SUBBLOCK,.SUBPARAMS)
  . . . MERGE OUT("PROP")=SUBPARAMS
  . . IF KEY="[CHILD]" DO  QUIT
  . . . NEW SUBBLOCK DO CUTSECTION(.BLOCK,IDX,.SUBBLOCK)
  . . . NEW SUBPARAMS DO PARSEBLOCK(.SUBBLOCK,.SUBPARAMS)
  . . . NEW CHILDIDX SET CHILDIDX=$ORDER(OUT("CHILD","@"),-1)+1
  . . . ;"SET OUT("CHILD",CHILDIDX)=TYPE
  . . . MERGE OUT("CHILD",CHILDIDX)=SUBPARAMS
  . . ELSE  DO
  . . . IF (BLOCKTYPE="[ARRAY]"),(BLOCKNAME'="") DO  QUIT
  . . . . SET OUT(BLOCKNAME)=BLOCKTYPE
  . . . . SET OUT(BLOCKNAME,KEY)=VALUE
  . . . SET OUT("PROP",KEY)=VALUE
  QUIT
  ;
TRIMCOMMENT(TEXT) ;
  NEW POS SET POS=1
  FOR  QUIT:POS<1  DO
  . SET POS=$FIND(TEXT,";",POS) QUIT:POS<1
  . IF $$INQT^TMGSTUT3(TEXT,POS-1) QUIT
  . SET TEXT=$EXTRACT(TEXT,1,POS-2)  ;"clip comments marked by ';', not inside string quotes
  SET TEXT=$$TRIM^XLFSTR(TEXT)
  QUIT TEXT
  ;
GETSCRNW() ;"
  ;"NOTE: SETS SCRNW,SCRNH in global scope.  
  IF $DATA(SCRNW)=0 IF $$GETSCRSZ^TMGKERNL(.SCRNH,.SCRNW)  ;"Ignore result
  QUIT SCRNW
  ;
GETSCRNH() ;"
  ;"NOTE: SETS SCRNW,SCRNH in global scope.  
  IF $DATA(SCRNH)=0 IF $$GETSCRSZ^TMGKERNL(.SCRNH,.SCRNW)  ;"Ignore result
  QUIT SCRNH
  ;
CUTSECTION(ARR,STARTIDX,OUT) ;"Extract section from array
  ;"INPUT: ARR -- array with form.  PASS BY REFERENCE.  Will be modified.  
  ;"       STARTIDX -- Starting index #.  NOTE: This is the header line. 
  ;"         It is expected that all lines **following** will have additional '.' for indention
  ;"         This function will pull all lines with the same number of '.' indention
  ;"       OUT --Pass by REFERENCE.  Destination array
  NEW IDX SET IDX=STARTIDX
  NEW INITLINE SET INITLINE=$GET(ARR(IDX)) KILL ARR(IDX) 
  NEW INITLEVEL SET INITLEVEL=$$NUMDOTS(INITLINE)
  SET OUT(IDX)=$$STRIPDOTS(INITLINE,INITLEVEL)
  NEW DONE SET DONE=0
  FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:(IDX'>0)!DONE  DO
  . NEW LINE SET LINE=$GET(ARR(IDX))
  . NEW LEVEL SET LEVEL=$$NUMDOTS(LINE)
  . IF LEVEL<=INITLEVEL SET DONE=1 QUIT
  . KILL ARR(IDX) SET OUT(IDX)=$$STRIPDOTS(LINE,INITLEVEL)
  QUIT
  ;
STRIPDOTS(LINE,NUM2REMOVE) ;"
  SET NUM2REMOVE=+$GET(NUM2REMOVE)
  FOR  QUIT:NUM2REMOVE<=0  DO
  . NEW POS SET POS=$FIND(LINE,".") ;"returns position AFTER "."
  . IF POS=0 SET NUM2REMOVE=0 QUIT
  . SET NUM2REMOVE=NUM2REMOVE-1
  . SET LINE=$$TRIM^XLFSTR($EXTRACT(LINE,POS,$LENGTH(LINE)))
  QUIT LINE
  ;
NUMDOTS(LINE) ;"Count how many prefix '.''s prefix LINE
  NEW RESULT SET RESULT=0
  SET LINE=$TRANSLATE(LINE,$CHAR(9)_" ","")  ;"strip any TABs or spaces
  NEW DONE SET DONE=0
  NEW IDX FOR IDX=1:1:$LENGTH(LINE) QUIT:DONE  DO
  . IF $EXTRACT(LINE,IDX)="." SET RESULT=RESULT+1 QUIT
  . SET DONE=1 
  QUIT RESULT
  
  ;"NOT WORKING
  ;"/dev/pts/0 OPEN TERMINAL NOPAST NOESCA NOREADS TYPE TERM=$C(13) WIDTH=124 LENG=43
  ;"WORKING
  ;"/dev/pts/0 OPEN TERMINAL NOPAST NOESCA NOREADS TYPE TERM=$C(13) WIDTH=124 LENG=43  