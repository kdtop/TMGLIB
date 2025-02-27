TMGTEST3 ;TMG/kst/Tests for device parameters; 2/13/25
          ;;1.0;TMG-LIB;**1**;09/01/05
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 2/13/25  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;"
TEST ;"TEST TTY IO SETTINGS
  NEW DEVSTATE,DEVINFO,MENU,IDX,MENUIDX,USER,ZZTEMP
  NEW ORDER DO DISPORDER(.ORDER)
  NEW VERBOSE SET VERBOSE=1
  NEW OPTION 
  NEW INITIALSTATE DO DEV2ARR($IO,.INITIALSTATE,"",.DEVINFO)
  ;
M1 ;  
  KILL DEVSTATE DO DEV2ARR($IO,.DEVSTATE,"",.DEVINFO)
  KILL MENU
  IF VERBOSE WRITE "Prepairing menu in TEST^TMGTEST3",!
  SET MENU(0)="TTY IO SUBSYSTEM INFO"
  SET IDX="",MENUIDX=1
  FOR  SET IDX=$ORDER(ORDER(IDX)) QUIT:IDX'>0  DO
  . NEW NAME SET NAME=$GET(ORDER(IDX)) QUIT:NAME=""
  . QUIT:(NAME="$X")!(NAME="$Y")
  . NEW VALUE SET VALUE=$GET(DEVSTATE("STATE",NAME))
  . SET MENU(MENUIDX)="Adjust "_$$RJ^XLFSTR(NAME,11)_" Now "_VALUE_$CHAR(9)_NAME,MENUIDX=MENUIDX+1
  SET MENU(MENUIDX)="Test entering input."_$CHAR(9)_"INPUT",MENUIDX=MENUIDX+1
  ;
  SET USER=$$MENU(.MENU,"^")
  IF USER["^" GOTO TESTDN
  IF $DATA(DEVSTATE("STATE",USER))>0 DO  GOTO M1
  . DO DETAIL(USER,.DEVSTATE,.DEVINFO,.OPTION)
  IF USER="INPUT" DO  GOTO M1
  . DO TESTINPUT(.DEVSTATE,.OPTION)
  GOTO M1  
TESTDN ;  
  DO RESTORDEV(.INITIALSTATE,.DEVINFO,.OPTION)
  WRITE "Leaving TEST^TMGTEST3.  Goodbye.",!
  QUIT
  ;
DETAIL(NAME,DEVSTATE,DEVINFO,OPTION) ;
  NEW MENU,IDX,MENUIDX,USER,ENTRY,TOGGLE
  SET ENTRY=$GET(DEVINFO("ENTRY",NAME))
  NEW VERBOSE SET VERBOSE=0
DM1 ;  
  KILL DEVSTATE DO DEV2ARR($IO,.DEVSTATE,"",.DEVINFO)  
  KILL MENU
  NEW CURSTATE SET CURSTATE=$GET(DEVSTATE("STATE",NAME))
  NEW ALTSTATE SET ALTSTATE=$SELECT(CURSTATE="ON":"OFF",1:"ON")
  SET TOGGLE("ON")=$PIECE(ENTRY,"^",1)  ;"command to turn ON
  SET TOGGLE("OFF")=$PIECE(ENTRY,"^",3)  ;"command to turn OFF
  SET MENU(0)=NAME_" INFO. Current state = "_CURSTATE
  SET IDX="",MENUIDX=1
  IF TOGGLE("ON")["@" DO
  . SET MENU(MENUIDX)="Enter value for "_NAME_$CHAR(9)_"VALUE",MENUIDX=MENUIDX+1
  . SET MENU(MENUIDX)="Disable "_NAME_$CHAR(9)_"DISABLE",MENUIDX=MENUIDX+1
  ELSE  DO
  . SET MENU(MENUIDX)="Toggle "_NAME_" to "_ALTSTATE_$CHAR(9)_"TOGGLE",MENUIDX=MENUIDX+1
  SET MENU(MENUIDX)="Test entering input."_$CHAR(9)_"INPUT",MENUIDX=MENUIDX+1
  SET MENU(MENUIDX)="Read about "_NAME_$CHAR(9)_"INFO",MENUIDX=MENUIDX+1
  ;
  SET USER=$$MENU(.MENU,"^")
  IF USER["^" GOTO DTDN
  IF USER="VALUE" DO  GOTO DM1
  . NEW PARAMSTR SET PARAMSTR=TOGGLE("ON")
  . WRITE !,"Enter value: " 
  . NEW VALUE READ VALUE WRITE !
  . SET PARAMSTR=$PIECE(PARAMSTR,"@",1)_VALUE
  . WRITE "Planned command:",!
  . WRITE "USE $IO:("_PARAMSTR_")",!
  . SET %=2 WRITE "Is this correct" DO YN^DICN WRITE !
  . IF %'=1 QUIT
  . USE $IO:@PARAMSTR
  IF USER="DISABLE" DO  GOTO DM1
  . NEW PARAMSTR SET PARAMSTR=TOGGLE("OFF")
  . USE $IO:@PARAMSTR
  IF USER="TOGGLE" DO  GOTO DM1
  . NEW PARAMSTR SET PARAMSTR=TOGGLE(ALTSTATE)
  . USE $IO:@PARAMSTR
  IF USER="INPUT" DO  GOTO DM1
  . DO TESTINPUT(.DEVSTATE,.OPTION)
  IF USER="INFO" DO  GOTO DM1
  . NEW INFO DO GETHELP(NAME,.DEVINFO,.INFO)
  . DO SHOWINFO(.INFO)
  GOTO DM1
DTDN ;  
  QUIT;
  ;
TESTINPUT(DEVSTATE,OPTION) ;
  NEW MENU,IDX,MENUIDX,USER,X,X2
  NEW CANONICAL SET CANONICAL=$GET(DEVSTATE("STATE","CANONICAL"))
TI1 ;  
  KILL MENU
  SET MENU(0)="Test READING Input From User"
  SET IDX="",MENUIDX=1
  SET MENU(MENUIDX)="HANG 5 READ X -- Pause, then simple read, any length."_$CHAR(9)_"HANGSIMPLE",MENUIDX=MENUIDX+1
  SET MENU(MENUIDX)="READ X -- simple read, any length."_$CHAR(9)_"SIMPLE",MENUIDX=MENUIDX+1
  SET MENU(MENUIDX)="READ *X -- read 1 char, as ASCII code"_$CHAR(9)_"ASCII",MENUIDX=MENUIDX+1
  SET MENU(MENUIDX)="READ *X:1 -- read 1 ASCII code with 1 sec timeout"_$CHAR(9)_"ASCIITO1",MENUIDX=MENUIDX+1
  SET MENU(MENUIDX)="HANG 2 READ *X:0 -- Pause, then read 1 ASCII code with 0 sec timeout"_$CHAR(9)_"ASCIITOD1",MENUIDX=MENUIDX+1
  SET MENU(MENUIDX)="READ X#1 -- read 1 character"_$CHAR(9)_"READFL1",MENUIDX=MENUIDX+1
  SET MENU(MENUIDX)="READ X#5 -- read 5 chars"_$CHAR(9)_"READFL5",MENUIDX=MENUIDX+1
  SET MENU(MENUIDX)=" W !,""..."" HANG 5 W ""["" READ *X:0 W ""] ..."" HANG 5 W ""["" READ X2:1 W ""]"" SET X=X_X2"_$CHAR(9)_"MIXED",MENUIDX=MENUIDX+1
  ;
  SET USER=$$MENU(.MENU,"^")
  IF USER["^" GOTO TIDN
  SET X=""
  IF CANONICAL="ON" DO
  . WRITE !,"NOTE: CANONICAL is ON.  To complete entry, special keys may be required.",!
  . WRITE "Try Ctrl-J, Ctrl-D or Ctrl-M if [Enter] doesn't work.",!,!
  WRITE "Type here --> ["
  IF USER="HANGSIMPLE" DO  GOTO TI2
  . HANG 5 WRITE !,"[" READ X
  IF USER="SIMPLE" DO  GOTO TI2
  . READ X
  IF USER="ASCII" DO  GOTO TI2
  . READ *X
  IF USER="ASCIITO1" DO  GOTO TI2
  . READ *X:1
  IF USER="ASCIITOD1" DO  GOTO TI2
  . HANG 2 READ *X:0
  IF USER="READFL1" DO  GOTO TI2
  . READ X#1
  IF USER="READFL5" DO  GOTO TI2
  . READ X#5
  IF USER="MIXED" DO  GOTO TI2
  . W !,"..." HANG 5 W "[" READ *X:0 W "] ..." HANG 5 W "[" READ X2:1 W "]" SET X=X_X2  
  GOTO TI1
TI2
  WRITE "]",!,!
  WRITE "The value of X is [",X,"]",!
  WRITE "ZWRITE " ZWRITE X
  IF $ZB'="" ZWRITE $ZB WRITE !
  WRITE !
  DO ENTER2GO
  WRITE !
  GOTO TI1
TIDN ;
  QUIT
  ;  
GETHELP(NAME,DEVINFO,OUT)  ;
  NEW IDX SET IDX=1
  SET OUT(IDX)=NAME_" mode info:                                                              ",IDX=IDX+1
  SET OUT(IDX)="------------------------------------------------------------------------------- ",IDX=IDX+1
  IF NAME="CANONICAL" DO
  . SET OUT(IDX)="When in CANONICAL mode, the TTY IO subsystem performs line editing.  For          ",IDX=IDX+1
  . SET OUT(IDX)="example, if user types 'Dogz[backspace]s' then the buffer will be edited          ",IDX=IDX+1
  . SET OUT(IDX)="to hold 'Dogs'.  If NOCANONICAL, then the characters returned from the IO         ",IDX=IDX+1
  . SET OUT(IDX)="subsystem would be 'Dogz'_$char(127)_'s'.  HOWEVER, yottadb itself would          ",IDX=IDX+1
  . SET OUT(IDX)="then take care of the editing, and the end user would still get back 'Dogs'.      ",IDX=IDX+1
  . SET OUT(IDX)="In straight TTY IO canonical mode, the end of the user's input is signaled by     ",IDX=IDX+1
  . SET OUT(IDX)="NL (usually <LF>, ^J), EOF (usually ^D), and EOL (usually not defined).           ",IDX=IDX+1
  . SET OUT(IDX)="HOWEVER, when enabling CANONICAL mode, yottdab also tells the TTY IO subsystem    ",IDX=IDX+1
  . SET OUT(IDX)="to change <CR> to <LF>, thus [ENTER] key will terminate input.                    ",IDX=IDX+1
  . SET OUT(IDX)="[NO]CANONICAL Applies to: TRM                                                     ",IDX=IDX+1
  IF NAME="ECHO_INPUT" DO                                                                           
  . SET OUT(IDX)="Enables or disables the echo of terminal input. If you disable ECHO, the EDITING  ",IDX=IDX+1 
  . SET OUT(IDX)="functions will be disabled and any input is not available for later recall.       ",IDX=IDX+1
  . SET OUT(IDX)="[NO]ECHO Applies to: TRM                                                          ",IDX=IDX+1
  IF NAME="ESC_PROS" DO                                                                             
  . SET OUT(IDX)="This enables or disables YottaDB processing of escape sequences.                  ",IDX=IDX+1                              
  . SET OUT(IDX)="Escape sequences are a sequence of characters the terminal uses to communicate    ",IDX=IDX+1
  . SET OUT(IDX)="certain info to yottadb.  For example, when the user presses the [Up] key,        ",IDX=IDX+1
  . SET OUT(IDX)="the terminal will send '<Escape>[A'.  Because a read might ask for just one (1)   ",IDX=IDX+1
  . SET OUT(IDX)="character, but when [Up] is comprised of three (3) characters, processing         ",IDX=IDX+1
  . SET OUT(IDX)="may be grouped together or not.                                                   ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="The following events result when a terminal has ESCAPE sequence processing        ",IDX=IDX+1
  . SET OUT(IDX)="enabled.  When an <ESC> or <CSI> arrives in the terminal input, the device        ",IDX=IDX+1
  . SET OUT(IDX)="driver verifies the sequence that follows as a valid ANSI escape sequence,        ",IDX=IDX+1
  . SET OUT(IDX)="terminates the READ, and sets $ZB to contain the entire escape sequence. In       ",IDX=IDX+1
  . SET OUT(IDX)="the case of a READ * when ESCAPE sequence processing is enabled and an escape     ",IDX=IDX+1
  . SET OUT(IDX)="introducer is read, the entire escape sequence is returned in $ZB and the ASCII   ",IDX=IDX+1
  . SET OUT(IDX)="representation of the first character is returned in the argument of the READ *.  ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="When escape processing is disabled, READ *x returns 27 in x for an <ESC>. If the  ",IDX=IDX+1
  . SET OUT(IDX)="escape introducer is also a TERMINATOR, $ZB has a string of length one (1), and   ",IDX=IDX+1
  . SET OUT(IDX)="a value of the $ASCII() representation of the escape introducer; otherwise, $ZB   ",IDX=IDX+1
  . SET OUT(IDX)="holds the empty string. For single character and short fixed reads with NOESCAPE, ",IDX=IDX+1 
  . SET OUT(IDX)="the remaining characters in the escape sequence will be in the input stream for   ",IDX=IDX+1
  . SET OUT(IDX)="subsequent READS regardless of [NO]TYPEAHEAD.                                     ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="An application that operates with (NOESCAPE:TERM=$C(13)) must provide successive  ",IDX=IDX+1
  . SET OUT(IDX)="to remove the remaining characters in the escape sequence from the input stream.  ",IDX=IDX+1
  . SET OUT(IDX)="READ * commands                                                                   ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="By default, ESCAPE processing is disabled.                                        ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="[NO]ESCAPE Applies to: TRM                                                        ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="MODIFICATION:                                                                     ",IDX=IDX+1
  . SET OUT(IDX)="If READ *X is entered and user types an isolated <ESC> with escape processing     ",IDX=IDX+1
  . SET OUT(IDX)="enabled, and if 100 ms passes without further characters encountered, then        ",IDX=IDX+1
  . SET OUT(IDX)="the <ESC> alone will be returned as if an escape sequence was not encountered.    ",IDX=IDX+1
  IF NAME="TERMINATOR" DO                                                                           
  . SET OUT(IDX)="Specifies which of the 256 ASCII characters terminate a READ. For example,        ",IDX=IDX+1
  . SET OUT(IDX)="TERMINATOR=$C(0) makes <NUL> the terminator.                                      ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="When NOESCAPE is in effect, TERMINATOR controls whether or not <ESC> or <CSI>     ",IDX=IDX+1
  . SET OUT(IDX)="are treated as terminators, however, when ESCAPE processing is enabled, the       ",IDX=IDX+1
  . SET OUT(IDX)="entire escape sequence is treated as a terminator regardless of the TERMINATOR    ",IDX=IDX+1
  . SET OUT(IDX)="specification.                                                                    ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="When EDITING is enabled, the control characters used for editing are not          ",IDX=IDX+1
  . SET OUT(IDX)="treated as terminators even if they are in the TERMINATOR list.                   ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="You can define any control character as a terminator, but they are all            ",IDX=IDX+1
  . SET OUT(IDX)="single character.                                                                 ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="NOTERMINATOR eliminates all terminators. When a terminal has all terminators      ",IDX=IDX+1
  . SET OUT(IDX)="disabled, fixed length READ and READ * terminate on receipt of some number of     ",IDX=IDX+1
  . SET OUT(IDX)="characters, and a timed READ terminates on timeout, but any other READ only       ",IDX=IDX+1
  . SET OUT(IDX)="terminates when the input fills the terminal read buffer.                         ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="By default, terminals recognize <CR>, <LF>, and <ESC> as terminators              ",IDX=IDX+1
  . SET OUT(IDX)="(that is, TERMINATOR=$C(10, 13,27)). TERMINATOR="" restores the default.          ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="Example: USE $P:TERM=$C(26,13,11,7)                                               ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="This example enables the ASCII characters <SUB>, <CR>, <VT> and <BEL> as READ     ",IDX=IDX+1
  . SET OUT(IDX)="terminators.                                                                      ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="[NO]TERMINATOR[=expr] Applies to: TRM                                             ",IDX=IDX+1
  IF NAME="TYPEAHEAD" DO
  . SET OUT(IDX)="Enables or disables type-ahead buffering for a terminal. When TYPEAHEAD is        ",IDX=IDX+1       
  . SET OUT(IDX)="disabled, any pending input which has not yet been read will be discarded         ",IDX=IDX+1
  . SET OUT(IDX)="before input is read for each READ argument. When TYPEAHEAD is enabled,           ",IDX=IDX+1
  . SET OUT(IDX)="any input not read by one READ argument will remain available for the next        ",IDX=IDX+1
  . SET OUT(IDX)="READ argument or command.                                                         ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="The size of the type-ahead buffer limits the amount of data entered at the        ",IDX=IDX+1
  . SET OUT(IDX)="terminal that the device driver can store in anticipation of future READs.        ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="By default, TYPEAHEAD is enabled.                                                 ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="[NO]TYPEAHEAD Applies to: TRM                                                     ",IDX=IDX+1
  IF NAME="UPPERCASE" DO
  . SET OUT(IDX)="Enables or disables YottaDB from converting lowercase input to                    ",IDX=IDX+1 
  . SET OUT(IDX)="uppercase during READs.                                                           ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="Example: USE $principal:(convert) READ X                                          ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="This example converts all lowercase to uppercase during READ X.                   ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="By default, the terminal device driver operates NOCONVERT.                        ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="[NO]CONVERT Applies to: TRM                                                       ",IDX=IDX+1
  IF NAME="PASSTHRU" DO
  . SET OUT(IDX)="Enables or disables interpretation of the ERASE character for a terminal.         ",IDX=IDX+1
  . SET OUT(IDX)="PASTHRU shifts management of handling and response to ERASE characters in         ",IDX=IDX+1
  . SET OUT(IDX)="the input stream from YottaDB to the application code.                            ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="Also, this controls interpretation by the operating system of special             ",IDX=IDX+1
  . SET OUT(IDX)="control characters (for example <CTRL-B>).                                        ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="Enabling PASTHRU mode supersedes EDITING mode.                                    ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="Exercise caution with PASTHRU in debugging, because using a PASTHRU               ",IDX=IDX+1
  . SET OUT(IDX)="terminal in Direct Mode is somewhat awkward.                                      ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="[NO]PASTHRU Applies to: TRM (Terminals) and Printers                              ",IDX=IDX+1
  IF NAME="CTRLC_BRK" DO
  . SET OUT(IDX)="Enables or disables the ability to force YottaDB into Direct Mode by              ",IDX=IDX+1
  . SET OUT(IDX)="entering <CTRL-C> at $PRINCIPAL.                                                  ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="If CENABLE is set, <CTRL-C> interrupts process execution. For more                ",IDX=IDX+1
  . SET OUT(IDX)="information on interrupt handling, refer to Interrupt Handling .                  ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="By default, CENABLE is set. If CTRAP contains $C(3), CENABLE is                   ",IDX=IDX+1
  . SET OUT(IDX)="disabled.                                                                         ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="Note that if CENABLE/NOCENABLE is used in M code invoked from a                   ",IDX=IDX+1
  . SET OUT(IDX)="call-in where the main program is not M, this parameter is ignored.               ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="Example: USE $principal:(nocenable:ctrap="":exception="")                         ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="[NO]CENABLE Applies to: TRM                                                       ",IDX=IDX+1
  IF NAME="CTRL_TRAP" DO
  . SET OUT(IDX)="Establishes the <CTRL> characters in the expression as trap characters for        ",IDX=IDX+1
  . SET OUT(IDX)="the current device.                                                               ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="The expression is a comma seperated list of ASCII characters where 0<=intexpr<=31 ",IDX=IDX+1 
  . SET OUT(IDX)="Other than <CTRL_C>, YottaDB recognizes <CTRL> characters only when reading them  ",IDX=IDX+1
  . SET OUT(IDX)="from $IO.                                                                         ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="The behavior for <CTRL-C> is different in the sense that the OS recognizes it as  ",IDX=IDX+1
  . SET OUT(IDX)="an out-of-band interrupt including when it occurs on $PRINCIPAL,                  ",IDX=IDX+1
  . SET OUT(IDX)="i.e., when $IO'=$PRINCIPAL and delivers it immediately; When YottaDB receives a   ",IDX=IDX+1
  . SET OUT(IDX)="trap character in the input from a device, YottaDB issues a run-time exception.   ",IDX=IDX+1
  . SET OUT(IDX)="The device does not have to be the current device, that is $IO.                   ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="The <CTRL> characters are ASCII 0 though 31. However, terminal configuration      ",IDX=IDX+1
  . SET OUT(IDX)="may prevent most <CTRL> characters from ever reaching YottaDB's CTRAP facility.   ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="For example, the command USE $PRINCIPAL:CTRAP=$CHAR(26,30,7,19) sets a trap       ",IDX=IDX+1
  . SET OUT(IDX)="for the ASCII characters <SUB>, <RS>, <BEL> and <DC3>.                            ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="Specifying CTRAP completely replaces the previous CTRAP list. Setting CTRAP to    ",IDX=IDX+1
  . SET OUT(IDX)="the null string ("") disables character trapping.                                 ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="A trap character enabled by CTRAP produces one of the following actions:          ",IDX=IDX+1
  . SET OUT(IDX)="  -- If an EXCEPTION deviceparameter has been issued for the device, the process  ",IDX=IDX+1
  . SET OUT(IDX)="     executes the EXCEPTION argument.                                             ",IDX=IDX+1
  . SET OUT(IDX)="  -- Otherwise, if $ETRAP is not the empty string, execute $ETRAP.                ",IDX=IDX+1
  . SET OUT(IDX)="  -- Otherwise, if $ZTRAP is not the empty string, the process executes $ZTRAP.   ",IDX=IDX+1
  . SET OUT(IDX)="  -- Otherwise, the YottaDB image terminates.                                     ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="When CTRAP includes <CTRL-C>, [NO]CENABLE has no effect.                          ",IDX=IDX+1
  . SET OUT(IDX)="CTRAPping <CTRL-C> also takes precedence over CENABLE.                            ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="CTRAP=$[Z]CHAR(intexpr[,…]) Applies to: TRM                                       ",IDX=IDX+1
  IF NAME="EDIT_MODE" DO
  . SET OUT(IDX)="Enables the EDITING mode for the $PRINCIPAL device. If you enable EDITING,        ",IDX=IDX+1
  . SET OUT(IDX)="YottaDB allows the use of the left and right cursor movement keys and certain     ",IDX=IDX+1
  . SET OUT(IDX)="<CTRL> characters within the current input line. You can recall the last input    ",IDX=IDX+1
  . SET OUT(IDX)="line using the up or down arrow key. The editing functions are the same as        ",IDX=IDX+1
  . SET OUT(IDX)="during direct mode command input as described in the Line Editing section of the  ",IDX=IDX+1
  . SET OUT(IDX)="Operating & Debugging in Direct Mode chapter except that backspace is not         ",IDX=IDX+1
  . SET OUT(IDX)="treated the same as the erase character from terminfo which is usually delete     ",IDX=IDX+1
  . SET OUT(IDX)="(ASCII 127). NOECHO disables EDITING mode.                                        ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="Set the environment variable ydb_principal_editing to specify the mode for        ",IDX=IDX+1
  . SET OUT(IDX)="EDITING. For example, ydb_principal_editing=""EDITING"" enables EDITING mode at     ",IDX=IDX+1
  . SET OUT(IDX)="YottaDB startup. You can also specify the mode for INSERT. For example,           ",IDX=IDX+1
  . SET OUT(IDX)="ydb_principal_editing=""NOINSERT:EDITING"". If you specify both modes then          ",IDX=IDX+1
  . SET OUT(IDX)="separate them with a colon (':') and put them in any order.                       ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="If EDITING mode is enabled, escape sequences do not terminate READs.              ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="Enabling PASTHRU mode supersedes EDITING mode.                                    ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="If any of the EDITING <CTRL> characters are in the CTRAP list, their editing      ",IDX=IDX+1
  . SET OUT(IDX)="functions are not available since CTRAP takes precedence. However the EDITING     ",IDX=IDX+1
  . SET OUT(IDX)="<CTRL> characters takes precedence over the TERMINATOR list.                      ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="By default, EDITING mode is disabled.                                             ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="[NO]EDITING Applies to: TRM                                                       ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="Note:  M READ EDITING depends on the values of $X and $Y being correct. If the    ",IDX=IDX+1
  . SET OUT(IDX)="application sends its own escape sequences or control characters, which change    ",IDX=IDX+1
  . SET OUT(IDX)="the cursor position, it must properly update $X and $Y before doing a M READ      ",IDX=IDX+1
  . SET OUT(IDX)="with EDITING enabled to ensure correct formatting during input.                   ",IDX=IDX+1  
  IF NAME="INSERT" DO
  . SET OUT(IDX)="Enables or disables insert mode for the $PRINCIPAL device. If INSERT mode is      ",IDX=IDX+1
  . SET OUT(IDX)="enabled, YottaDB inserts input characters at the logical position in the input    ",IDX=IDX+1
  . SET OUT(IDX)="stream designated by the virtual cursor as defined by $X and $Y, for example in   ",IDX=IDX+1
  . SET OUT(IDX)="the middle of the line/record. If INSERT mode is disabled, input characters       ",IDX=IDX+1
  . SET OUT(IDX)="overwrite the existing characters in the input stream at the logical position     ",IDX=IDX+1
  . SET OUT(IDX)="designated by the virtual cursor. You can toggle the insert mode within a direct  ",IDX=IDX+1
  . SET OUT(IDX)="mode line or if EDITING is enabled for a single READ argument's input using the   ",IDX=IDX+1
  . SET OUT(IDX)="terminal's INSERT key. The INSERT mode is reset to the default or what was last   ",IDX=IDX+1
  . SET OUT(IDX)="specified with USE at the beginning of each direct mode line or READ argument.    ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="[NO]INSERT Applies to: TRM                                                        ",IDX=IDX+1
  IF NAME="EMPTERM" DO
  . SET OUT(IDX)="Allows an 'Erase' character on an empty input line to terminate a READ or READ #  ",IDX=IDX+1
  . SET OUT(IDX)="command. The default is NOEMPTERM. The ydb_principal_editing environment          ",IDX=IDX+1
  . SET OUT(IDX)="variable specifies the initial setting of [NO]EMPTERM. The TERMINFO specified by  ",IDX=IDX+1
  . SET OUT(IDX)="the current value of the TERM environment variable defines capnames values 'kbs'  ",IDX=IDX+1
  . SET OUT(IDX)="and/or 'kdch1' with character sequences for 'Erase.' If 'kbs' or 'kdch1' are      ",IDX=IDX+1
  . SET OUT(IDX)="multi-character values, you must also specify the ESCAPE or EDIT deviceparameters ",IDX=IDX+1
  . SET OUT(IDX)="for EMPTERM recognition.                                                          ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="The erase character as set and shown by stty also terminates a READ command with  ",IDX=IDX+1
  . SET OUT(IDX)="an empty input line. You can set this erase character to various values using     ",IDX=IDX+1
  . SET OUT(IDX)="the stty shell command. Typical values of an erase character are <CTRL-H> and     ",IDX=IDX+1
  . SET OUT(IDX)="<CTRL-?>. Characters set and shown with stty setting must match what the terminal ",IDX=IDX+1
  . SET OUT(IDX)="emulator sends.                                                                   ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="The environment variable TERM must specify a terminfo entry that matches both     ",IDX=IDX+1
  . SET OUT(IDX)="what the terminal (or terminal emulator) sends and expects.                       ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="[NO]EMPT[ERM] Applies to: TRM                                                     ",IDX=IDX+1
  IF NAME="HOSTSYNC" DO  
  . SET OUT(IDX)="Enables or disables the use of XON/XOFF by the host to throttle input and prevent ",IDX=IDX+1 
  . SET OUT(IDX)="impending buffer overruns for a terminal. This deviceparameter provides a control ",IDX=IDX+1 
  . SET OUT(IDX)="mechanism for the host over asynchronous communication lines to help prevent data ",IDX=IDX+1 
  . SET OUT(IDX)="loss when hardware is slow and/or processing load is high.                        ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="By default, HOSTSYNC is disabled.                                                 ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="[NO]HOSTSYNC Applies to: TRM                                                      ",IDX=IDX+1
  IF NAME="EXCEPTION" DO
  . SET OUT(IDX)="Defines an error handler for an I/O device. The expression must contain a         ",IDX=IDX+1
  . SET OUT(IDX)="fragment of YottaDB code (for example, GOTO ERRFILE) that YottaDB XECUTEs when    ",IDX=IDX+1
  . SET OUT(IDX)="the driver for the device detects an error, or an entryref to which YottaDB       ",IDX=IDX+1
  . SET OUT(IDX)="transfers control, as appropriate for the current ydb_ztrap_form.                 ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="The expression must contain a fragment of YottaDB code (for example,              ",IDX=IDX+1
  . SET OUT(IDX)="GOTO ERRFILE) that YottaDB XECUTEs when the driver for the device detects an      ",IDX=IDX+1
  . SET OUT(IDX)="error, or an entryref to which YottaDB transfers control, as appropriate for the  ",IDX=IDX+1
  . SET OUT(IDX)="current ydb_ztrap_form.                                                           ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="For more information on error handling, refer to Chapter 13: “Error Processing”.  ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="EXCEPTION=expr Applies to: All devices                                            ",IDX=IDX+1
  IF NAME="FILTERXY" DO
  . SET OUT(IDX)="Specifies character filtering for specified cursor movement sequences. Filtering  ",IDX=IDX+1 
  . SET OUT(IDX)="requires character by character examination of all output and reduces I/O         ",IDX=IDX+1
  . SET OUT(IDX)="performance.                                                                      ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="Each FILTER deviceparameter can have only one argument. However, multiple         ",IDX=IDX+1
  . SET OUT(IDX)="FILTER deviceparameters can appear in a single USE command, each with different   ",IDX=IDX+1
  . SET OUT(IDX)="arguments.                                                                        ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="The valid values for expr:                                                        ",IDX=IDX+1
  . SET OUT(IDX)=" -- [NO]CHARACTERS enables or disables maintenance of $X and $Y according to      ",IDX=IDX+1
  . SET OUT(IDX)="    the M ANSI standard for the characters <BS>, <LF>, <CR> and <FF>. CHARACTERS  ",IDX=IDX+1
  . SET OUT(IDX)="    causes the device driver to examine all output for the above characters, and  ",IDX=IDX+1
  . SET OUT(IDX)="    to adjust $X and $Y accordingly. By default, YottaDB performs special         ",IDX=IDX+1
  . SET OUT(IDX)="    maintenance on $X and $Y only for M format control characters, WRAPped        ",IDX=IDX+1
  . SET OUT(IDX)="    records, and certain action deviceparameters.                                 ",IDX=IDX+1
  . SET OUT(IDX)=" -- In UTF-8 mode, FILTER recognizes the line terminators specified by the        ",IDX=IDX+1
  . SET OUT(IDX)="    Unicode standard.                                                             ",IDX=IDX+1
  . SET OUT(IDX)=" -- [NO]ESCAPE alters the effect of ANSI escape sequences on $X and $Y. ESCAPE    ",IDX=IDX+1
  . SET OUT(IDX)="    causes YottaDB to filter the output, searching for ANSI escape sequences and  ",IDX=IDX+1
  . SET OUT(IDX)="    preventing them from updating $X and $Y. By default, YottaDB does not screen  ",IDX=IDX+1
  . SET OUT(IDX)="    output for escape sequences.                                                  ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="By default, YottaDB does not perform output filtering. For YottaDB to maintain    ",IDX=IDX+1
  . SET OUT(IDX)="$X for non-graphic characters as described by the standard, FILTER=""CHARACTERS""   ",IDX=IDX+1
  . SET OUT(IDX)="must be enabled. Output filtering adds additional overhead to I/O processing.     ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="Example: USE tcpdev:filter=""NOESCAPE""                                             ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="This example removes the effect of escape sequences on the maintenance $X and $Y  ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="[NO]FILTER[=expr] Applies to: TRM, SOC, NULL                                      ",IDX=IDX+1
  IF NAME="PAGE_LEN" DO
  . SET OUT(IDX)="Sets the virtual page length for an I/O device to the integer expression. You     ",IDX=IDX+1
  . SET OUT(IDX)="can specify the virtual page length up to 1,048,576. The page length controls     ",IDX=IDX+1
  . SET OUT(IDX)="the point at which the device driver automatically resets $Y to 0.                ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="By default, for terminals, YottaDB uses the terminfo variable lines (which may    ",IDX=IDX+1
  . SET OUT(IDX)="be from the terminal definition or from a stty command) as the initial value for  ",IDX=IDX+1
  . SET OUT(IDX)="LENGTH. The default length for null device and socket device is 66.               ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="Setting LENGTH to zero prevents resetting $Y to zero.                             ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="Example: USE sock:(width=80:znoff:zlength=24)                                     ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="This example sets the virtual page length to 24 for socket device sock.           ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="[Z]LENGTH=intexpr Applies to: TRM, SOC, SD, FIFO, PIPE, NULL                      ",IDX=IDX+1
  IF NAME="PAGE_WIDTH" DO
  . SET OUT(IDX)="Sets the device's logical record size and enables WRAP.                           ",IDX=IDX+1 
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="NOWRAP and WIDTH supersede each other. When WIDTH and NOWRAP appear together on   ",IDX=IDX+1
  . SET OUT(IDX)="the same USE command, the final one controls the device behavior. For a terminal, ",IDX=IDX+1
  . SET OUT(IDX)="WIDTH=0 is equivalent to WIDTH=n:NOWRAP, where n is the default length of a       ",IDX=IDX+1
  . SET OUT(IDX)="logical record on that terminal.                                                  ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="Terminals inherit their default WIDTH in YottaDB from the invoking shell          ",IDX=IDX+1
  . SET OUT(IDX)="environment. The default WIDTH for null and socket device is 255.                 ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="In M mode if WIDTH is set to 0, YottaDB uses the default WIDTH of the TRM and     ",IDX=IDX+1
  . SET OUT(IDX)="SOC devices. USE x:WIDTH=0 is equivalent to USE x:(WIDTH=<device-default>:NOWRAP. ",IDX=IDX+1
  . SET OUT(IDX)="For SOC, SD and FIFO devices in M mode, the device default is the RECORDSIZE.     ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="YottaDB format control characters, FILTER, and the device WIDTH and WRAP also     ",IDX=IDX+1
  . SET OUT(IDX)="have an effect on $X.                                                             ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="[Z]WIDTH=intexpr Applies to: TRM, SOC, NULL, SD, FIFO, PIPE                       ",IDX=IDX+1  
  IF NAME="WRAP_LINE" DO
  . SET OUT(IDX)="Enables or disables automatic record termination. When the current record size    ",IDX=IDX+1
  . SET OUT(IDX)="($X) reaches the maximum WIDTH and the device has WRAP enabled, YottaDB starts a  ",IDX=IDX+1
  . SET OUT(IDX)="new record, as if the routine had issued a WRITE ! command. When reading, WRAP    ",IDX=IDX+1
  . SET OUT(IDX)="only determines whether $X remains within the range of zero to WIDTH.             ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="Note that WRAP is enabled by default for SD, NULL, FIFO, PIPE and SOCKET. For     ",IDX=IDX+1
  . SET OUT(IDX)="TRM, WRAP is enabled by default if the terminfo variable auto_right_margin        ",IDX=IDX+1
  . SET OUT(IDX)="(capname 'am') is set.                                                            ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="NOWRAP causes YottaDB to require a WRITE ! to terminate the record. NOWRAP allows ",IDX=IDX+1
  . SET OUT(IDX)="$X to become greater than the device WIDTH for terminals and null devices.        ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="The combination of STREAM and NOWRAP on disk files allows you to write data of    ",IDX=IDX+1
  . SET OUT(IDX)="arbitrary length without truncation. Without the STREAM option, the WRAP option   ",IDX=IDX+1
  . SET OUT(IDX)="determines the action taken when the record length exceeds the device WIDTH.      ",IDX=IDX+1
  . SET OUT(IDX)="NOWRAP causes YottaDB to truncate the record, while WRAP causes YottaDB to insert ",IDX=IDX+1
  . SET OUT(IDX)="a format control character except for FIXED format.                               ",IDX=IDX+1
  . SET OUT(IDX)="                                                                                  ",IDX=IDX+1
  . SET OUT(IDX)="[NO]WRAP Applies to: TRM SOC NULL SD FIFO PIPE                                    ",IDX=IDX+1
  SET OUT(IDX)="                                                                                 ",IDX=IDX+1
  SET ENTRY=$GET(DEVINFO("ENTRY",NAME))
  SET OUT(IDX)="-------------------------------------------------------------------------------  ",IDX=IDX+1
  SET OUT(IDX)="                                                                                 ",IDX=IDX+1
  SET OUT(IDX)="Command to turn ON: USE $IO:("_$PIECE(ENTRY,"^",1)_")",IDX=IDX+1
  IF $PIECE(ENTRY,"^",1)["@" DO
  . SET OUT(IDX)="  @ above should be replaced with an appropriate value",IDX=IDX+1
  SET OUT(IDX)="Command to turn OFF: USE $IO:("_$PIECE(ENTRY,"^",3)_")",IDX=IDX+1
  SET OUT(IDX)="Display in ZSHOW ""D"" when ON: ["_$PIECE(ENTRY,"^",2)_"]",IDX=IDX+1
  SET OUT(IDX)="Display in ZSHOW ""D"" when OFF: ["_$PIECE(ENTRY,"^",4)_"]",IDX=IDX+1
  SET OUT(IDX)="Default value: ["_$PIECE(ENTRY,"^",5)_"]",IDX=IDX+1
  SET OUT(IDX)="                                                                                 ",IDX=IDX+1
  SET OUT(IDX)="------------------------------------------------------------------------------- ",IDX=IDX+1
  QUIT
  ;
SHOWINFO(INFO) ;
  WRITE !,"-------------------------------------------------------------------------------",!
  WRITE !
  NEW IDX SET IDX=""
  FOR  SET IDX=$ORDER(INFO(IDX)) QUIT:IDX'>0  DO
  . WRITE $GET(INFO(IDX)),!
  WRITE !
  DO ENTER2GO
  QUIT
  ;
DISPORDER(ORDER) ;
  NEW IDX SET IDX=1
  SET ORDER(IDX)="CANONICAL",IDX=IDX+1
  SET ORDER(IDX)="ECHO_INPUT",IDX=IDX+1
  SET ORDER(IDX)="ESC_PROS",IDX=IDX+1
  SET ORDER(IDX)="TERMINATOR",IDX=IDX+1
  SET ORDER(IDX)="TYPEAHEAD",IDX=IDX+1
  SET ORDER(IDX)="UPPERCASE",IDX=IDX+1
  SET ORDER(IDX)="PASSTHRU",IDX=IDX+1
  SET ORDER(IDX)="CTRLC_BRK",IDX=IDX+1
  SET ORDER(IDX)="EDIT_MODE",IDX=IDX+1
  SET ORDER(IDX)="INSERT",IDX=IDX+1
  SET ORDER(IDX)="EMPTERM",IDX=IDX+1
  SET ORDER(IDX)="CTRL_TRAP",IDX=IDX+1
  SET ORDER(IDX)="HOSTSYNC",IDX=IDX+1
  SET ORDER(IDX)="EXCEPTION",IDX=IDX+1
  SET ORDER(IDX)="FILTERXY",IDX=IDX+1
  SET ORDER(IDX)="PAGE_LEN",IDX=IDX+1
  SET ORDER(IDX)="PAGE_WIDTH",IDX=IDX+1
  SET ORDER(IDX)="WRAP_LINE",IDX=IDX+1
  QUIT
  ;
ENTER2GO ;
  WRITE !,"------ Press [ENTER] to continue ---------"
  NEW X READ X
  WRITE !
  QUIT
  ;
MENU(MENUARR,DEFCHOICE,USERRAW)  ;!!! DEPRECIATED !!!
  ;"NOTE: This is a copy from TMGUSRI2.  It is copied here so this routine will not
  ;"      have dependancy on TMGUSRI2.  NO ONE ELSE SHOULD USE THIS!
   ;"Purpose: to provide a simple menuing system
   ;"Input:  MENUARR -- PASS BY REFERENCE
   ;"        Format:
   ;"          MENUARR(0)=Header Text   <--- optional, default is MENU
   ;"          MENUARR(0,n)=Additional lines of header Text   <--- optional
   ;"          MENUARR(DispNumber)=MenuText_$C(9)_ReturnValue <-- _$C(9)_ReturnValue OPTIONAL, default is DispNumber
   ;"          MENUARR(DispNumber,#)=<text> --additional display lines, not separately selectable. No return value
   ;"          MENUARR(DispNumber)=MenuText_$C(9)_ReturnValue
   ;"          MENUARR(-1,"COLOR","FG")=foreground color  (optional)
   ;"          MENUARR(-1,"COLOR","BG")=foreground color  (optional)
   ;"          MENUARR(-1,"COLOR","MODE")="INDEXED" (default), or "256", or "24bit"
   ;"          MENUARR(-1,"INDENT")=# of spaces to add to beginning of each line. 
   ;"        DEFCHOICE: OPTIONAL, the default menu value
   ;"        USERRAW : OPTIONAL, PASS BY REFERENCE, an OUT PARAMETER.  Returns users raw input
   ;"Results: The selected ReturnValue (or DispNumber if no ReturnValue provided), or ^ for abort
   NEW RESULT SET RESULT="^"
   NEW S,FG,BG,HDREXTRA
   NEW WIDTH SET WIDTH=50
   NEW INDENTN SET INDENTN=+$GET(MENUARR(-1,"INDENT"))
   NEW INDENT SET INDENT="" IF INDENTN>0 SET INDENT=$JUSTIFY(" ",INDENTN)
   NEW LINE SET $PIECE(LINE,"=",WIDTH+1)=""
   NEW COLOROPT SET COLOROPT("MODE")="INDEXED" ;"default
MNU1 ;
   IF $DATA(MENUARR(-1,"COLOR")) DO
   . SET FG=$GET(MENUARR(-1,"COLOR","FG"),0)
   . SET BG=$GET(MENUARR(-1,"COLOR","BG"),1)
   . NEW MODE SET MODE=$GET(MENUARR(-1,"COLOR","MODE"),"INDEXED")
   . SET COLOROPT("MODE")=MODE
   . DO COLORS^TMGTERM(FG,BG,.COLOROPT)
   WRITE INDENT,LINE,!
   WRITE INDENT,$GET(MENUARR(0),"MENU"),$$PAD2POS^TMGSTUT2(WIDTH),!
   SET HDREXTRA=0
   FOR  SET HDREXTRA=$ORDER(MENUARR(0,HDREXTRA)) QUIT:HDREXTRA=""  DO
   . WRITE INDENT,MENUARR(0,HDREXTRA),$$PAD2POS^TMGSTUT2(WIDTH),!
   WRITE INDENT,LINE,!
   WRITE INDENT,"OPTIONS:",$$PAD2POS^TMGSTUT2(WIDTH),!
   ;
   NEW DISPNUMBER SET DISPNUMBER=$ORDER(MENUARR(0))
   IF DISPNUMBER'="" FOR  DO  QUIT:(DISPNUMBER="")
   . SET S=$GET(MENUARR(DISPNUMBER))
   . WRITE INDENT,$$RJ^XLFSTR(DISPNUMBER,4),".",$$PAD2POS^TMGSTUT2(6)
   . IF $DATA(MENUARR(DISPNUMBER,"COLOR")) DO
   . . IF $DATA(MENUARR(DISPNUMBER,"COLOR","fg")) SET MENUARR(DISPNUMBER,"COLOR","FG")=$GET(MENUARR(DISPNUMBER,"COLOR","fg"))
   . . IF $DATA(MENUARR(DISPNUMBER,"COLOR","bg")) SET MENUARR(DISPNUMBER,"COLOR","BG")=$GET(MENUARR(DISPNUMBER,"COLOR","bg"))
   . . SET FG=$GET(MENUARR(DISPNUMBER,"COLOR","FG"),0)
   . . SET BG=$GET(MENUARR(DISPNUMBER,"COLOR","BG"),1)
   . . DO COLORS^TMGTERM(FG,BG,.COLOROPT)
   . WRITE " ",$PIECE(S,$CHAR(9),1),$$PAD2POS^TMGSTUT2(WIDTH-1)
   . NEW SUBI SET SUBI=""
   . FOR  SET SUBI=$ORDER(MENUARR(DISPNUMBER,SUBI)) QUIT:SUBI'>0  DO
   . . WRITE !,$$PAD2POS^TMGSTUT2(8),$GET(MENUARR(DISPNUMBER,SUBI))
   . IF $DATA(MENUARR(DISPNUMBER,"COLOR")) DO
   . . DO VTATRIB^TMGTERM(0) ;"Reset colors
   . WRITE " ",!
   . SET DISPNUMBER=$ORDER(MENUARR(DISPNUMBER))
   ;
   WRITE INDENT,LINE,!
   ;
   SET DEFCHOICE=$GET(DEFCHOICE) 
   IF DEFCHOICE="" SET DEFCHOICE="^"
   NEW INPUT
   WRITE INDENT,"Enter selection (^ to abort): ",DEFCHOICE,"// "
   READ INPUT:$GET(DTIME,3600),!
   IF INPUT="" SET INPUT=DEFCHOICE
   SET USERRAW=INPUT
   IF INPUT="^" GOTO MNUDONE
   ;
   IF +INPUT'=INPUT SET INPUT=$$FINDMTCH(INPUT,.MENUARR)
   SET S=$GET(MENUARR(INPUT))
   IF S="" SET S=$GET(MENUARR($$UP^XLFSTR(INPUT)))
   ;"IF S="" WRITE "??",!! GOTO MNU1
   SET RESULT=$PIECE(S,$CHAR(9),2)
   IF RESULT="" SET RESULT=INPUT
   ;
MNUDONE ;
   IF $DATA(MENUARR(-1,"COLOR")) DO VTATRIB^TMGTERM(0) ;"Reset colors
   QUIT RESULT
   ;
FINDMTCH(INPUT,ARR)  ;"Search ARR for matching input
  ;"Input: INPUT -- user response
  ;"       ARR -- by reference.  This is the array that defines the menu
  ;"Result: Number of found match, or "?" IF no match
  NEW TMGRESULT SET TMGRESULT="?"
  NEW IDX SET IDX=0
  NEW OPTTEXT,INPUTLEN
  SET INPUTLEN=$LENGTH(INPUT)
  SET INPUT=$$UP^XLFSTR(INPUT)
  FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:(IDX'>0)!(TMGRESULT'="?")  DO
  . SET OPTTEXT=$GET(ARR(IDX))
  . SET OPTTEXT=$EXTRACT(OPTTEXT,0,INPUTLEN)
  . SET OPTTEXT=$$UP^XLFSTR(OPTTEXT)
  . IF OPTTEXT=INPUT SET TMGRESULT=IDX
FMDN  ;
  QUIT TMGRESULT
  ; 
  
 ;"================================================================
 ;
 ;"DEPRECIATED!!  This is copied from TMGKERN1 so that this file will have no extrnal dependencies. NO ONE ELSE SHOULD USE IT!! 
DEV2ARR(DEVICE,OUT,FILTER,INFO,OPTION)  ;"Store off a device, with all it's parameters
  ;"NOTE: This depends on YottaDB ZSHOW "D" command, which does NOT show all 
  ;"      device parameters (unfortunately).  
  ;"      Older versions of ydb had missing: CONVERT, HOSTSYNC, TTSYNC  --> now fixed
  ;"INPUT:  DEVICE -- the name of the device to query.  Can call with $P or $IO etc. 
  ;"        OUT -- PASS BY REFERENCE.  AN OUT PARAMETER.  Format:
  ;"           OUT("GENERAL","NAME")="/dev/pts/0"
  ;"           OUT("GENERAL","STATUS")="OPEN"
  ;"           OUT("GENERAL","TYPE")="TERMINAL"
  ;"           OUT("NON-DEFAULT","$X")=0
  ;"           OUT("NON-DEFAULT","$Y")=2
  ;"           OUT("RAW PARAMS",1)="NOPAST"
  ;"           OUT("RAW PARAMS",2)="NOESCA"
  ;"           OUT("RAW PARAMS",3)="NOREADS"
  ;"           OUT("RAW PARAMS",4)="TYPE"
  ;"           OUT("RAW PARAMS",5)="WIDTH=147"
  ;"           OUT("RAW PARAMS",6)="LENG=39"
  ;"           OUT("STATE","$X")=0
  ;"           OUT("STATE","$Y")=2
  ;"           OUT("STATE","CANONICAL")="OFF"
  ;"           OUT("STATE","CTRLC_BRK")="ON"
  ;"           OUT("STATE","CTRL_TRAP")="OFF"
  ;"           OUT("STATE","ECHO_INPUT")="ON"
  ;"           OUT("STATE","EDIT_MODE")="OFF"
  ;"           OUT("STATE","EMPTERM")="OFF"
  ;"           OUT("STATE","ESC_PROS")="OFF"
  ;"           OUT("STATE","EXCEPTION")="OFF"
  ;"           OUT("STATE","FILTERXY")="OFF"
  ;"           OUT("STATE","HOSTSYNC")="?"
  ;"           OUT("STATE","INSERT")="ON"
  ;"           OUT("STATE","PAGE_LEN")=39
  ;"           OUT("STATE","PAGE_WIDTH")=147
  ;"           OUT("STATE","PASSTHRU")="OFF"
  ;"           OUT("STATE","READSYNC")="OFF"
  ;"           OUT("STATE","TERMINATOR")="$C(13)"
  ;"           OUT("STATE","TTSYNC")="?"
  ;"           OUT("STATE","TYPEAHEAD")="ON"
  ;"           OUT("STATE","UPPERCASE")="?"
  ;"           OUT("STATE","WRAP_LINE")="ON"
  ;"        FILTER -- OPTIONAL.  If provided, then string of flags for desired info to be return
  ;"                If FILTER ="" then default is "GNRS", to return ALL
  ;"                If FILTER["G" -- return GENERAL info
  ;"                If FILTER["N" -- return NON-DEFAULT info
  ;"                If FILTER["R" -- return RAW PARAMS info
  ;"                If FILTER["S" -- return STATE info
  ;"        INFO -- OPTIONAL.  PASS BY FREFERENCE.  An array that has setup information from prior run (for faster execution)
  ;"        OPTION -- OPTIONAL
  ;"           OPTION("VERBOSE")=1  If found the output verbose debugging comments.  
  ;"RESULT: none
  NEW VERBOSE SET VERBOSE=+$GET(OPTION("VERBOSE"))
  IF VERBOSE WRITE "Starting DEV2ARR^TMGTEST3",!
  SET DEVICE=$GET(DEVICE)
  NEW UPDEV SET UPDEV=$$UP^XLFSTR(DEVICE)
  IF (UPDEV="$IO")!(UPDEV="$P") DO  QUIT
  . WRITE !,"ERROR: DEV2ARR^TMGTEST3() called with device """,DEVICE,""". Should NOT be enclosed in quotes.  Aborting.",!,!
  NEW TEMP ZSHOW "D":TEMP
  NEW DONE SET DONE=0
  SET FILTER=$GET(FILTER) IF FILTER="" SET FILTER="GNRS"
  IF VERBOSE WRITE "FILTER=[",FILTER,"]",!
  NEW APARAM
  KILL OUT
  NEW ARR,TOKEN,ADEVICE,MATCHDEV SET MATCHDEV=0
  NEW IDX,JDX,KDX SET IDX=0,KDX=1
  NEW OPTION SET OPTION("NO SPLIT IN QUOTES")=1
  ;"SPLIT EACH LINE FROM ZSHOW "D" INTO ARR, USING ONLY LINE THAT MATCHES DEVICE
  FOR  SET IDX=$ORDER(TEMP("D",IDX)) QUIT:(IDX'>0)  DO  QUIT:MATCHDEV
  . NEW ENTRY SET ENTRY=$GET(TEMP("D",IDX)) QUIT:ENTRY="" 
  . IF ENTRY["FIL=(" DO   ;"Example: FIL=(CHARACTERS, ESCAPES), convert --> FIL=(CHARACTERS,ESCAPES)
  . . NEW PARTA,PARTB,PARTC SET PARTA=$PIECE(ENTRY,"FIL=(",1),PARTC=$PIECE(ENTRY,"FIL=(",2,999),PARTB=$PIECE(PARTC,")",1),PARTC=$PIECE(PARTC,")",2,999)
  . . SET PARTB=$$REPLSTR^TMGSTUT3(PARTB,", ",",") 
  . . SET ENTRY=PARTA_"FIL=("_PARTB_")"_PARTC
  . KILL ARR SET ADEVICE=""
  . NEW SPLITARR DO SPLIT2AR^TMGSTUT2(ENTRY," ",.SPLITARR,,.OPTION)  ;"<-- can handle spaces inside strings
  . IF VERBOSE WRITE "Entry = [",ENTRY,"]",! 
  . KILL SPLITARR("MAXNODE")
  . SET JDX=0 FOR  SET JDX=$ORDER(SPLITARR(JDX)) QUIT:JDX'>0  DO
  . . SET TOKEN=$GET(SPLITARR(JDX)) QUIT:TOKEN=""
  . . IF JDX=1 SET OUT("GENERAL","NAME")=TOKEN,ADEVICE=TOKEN QUIT
  . . IF JDX=2 SET OUT("GENERAL","STATUS")=TOKEN QUIT
  . . IF JDX=3 SET OUT("GENERAL","TYPE")=TOKEN QUIT
  . . SET ARR(KDX)=TOKEN,KDX=KDX+1
  . SET MATCHDEV=(ADEVICE=DEVICE)
  . IF MATCHDEV=0 KILL ARR QUIT
  IF (MATCHDEV=0)!(IDX'>0) GOTO D2ADN
  MERGE OUT("RAW PARAMS")=ARR
  ;"SETUP A 'B' INDEX
  SET IDX=0 FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:IDX'>0  DO
  . SET APARAM=$GET(ARR(IDX)) QUIT:APARAM=""
  . IF APARAM["=" SET APARAM=$PIECE(APARAM,"=",1)_"="
  . SET OUT("RAW PARAMS","B",APARAM)=IDX
  ;"ENSURE INFO IS POPULATED
  IF $DATA(INFO)=0 DO SETUPSAV(.INFO)
  ;"Now process each element of line, each corresponding to a parameter value
  SET IDX=0
  FOR  SET IDX=$ORDER(ARR(IDX)) QUIT:IDX'>0  DO
  . SET APARAM=$GET(ARR(IDX)) QUIT:APARAM=""
  . NEW TRIMPARAM SET TRIMPARAM=APARAM
  . NEW KEYVAL SET KEYVAL=0
  . IF TRIMPARAM["=" DO  ;"Determine if parameter is a key=value type entry
  . . SET TRIMPARAM=$PIECE(APARAM,"=",1)_"="
  . . SET KEYVAL=1,KEYVAL("VAL")=$PIECE(APARAM,"=",2)
  . ;"First look for easy entries that have a value if ON
  . IF $DATA(INFO("ONDISP",TRIMPARAM)) DO
  . . NEW ENTRY SET ENTRY=$ORDER(INFO("ONDISP",TRIMPARAM,"")) QUIT:ENTRY=""
  . . ;"SET ENTRY=$GET(INFO("ENTRY",ENTRY)) ;"format:  Turn_on^On_disp^Turn_off^Off_disp
  . . SET OUT("STATE",ENTRY)=$SELECT(KEYVAL=1:$GET(KEYVAL("VAL")),1:"ON")
  . ;"First look for easy entries that have a value if OFF
  . IF $DATA(INFO("OFFDISP",APARAM)) DO
  . . NEW ENTRY SET ENTRY=$ORDER(INFO("OFFDISP",TRIMPARAM,"")) QUIT:ENTRY=""
  . . SET OUT("STATE",ENTRY)=$SELECT(KEYVAL=1:$GET(KEYVAL("VAL")),1:"OFF")
  ;"Next check for all the NULL when ON entries.  
  NEW ENTRY SET ENTRY=""
  FOR  SET ENTRY=$ORDER(INFO("NULLWHENON",ENTRY)) QUIT:ENTRY=""  DO
  . NEW DATA SET DATA=$GET(INFO("ENTRY",ENTRY)) QUIT:DATA=""
  . NEW OFFVALUE SET OFFVALUE=$PIECE(DATA,"^",4)
  . IF $DATA(OUT("RAW PARAMS","B",OFFVALUE)) QUIT
  . NEW TURNONCMD SET TURNONCMD=$PIECE(DATA,"^",1)  ;"piece 1 is the TURN ON command.
  . IF TURNONCMD["""""" SET TURNONCMD=""
  . SET OUT("STATE",ENTRY)="ON"  ;"TURNONCMD
  ;"Next check for all the NULL when OFF entries.  
  NEW ENTRY SET ENTRY=""
  FOR  SET ENTRY=$ORDER(INFO("NULLWHENOFF",ENTRY)) QUIT:ENTRY=""  DO
  . NEW DATA SET DATA=$GET(INFO("ENTRY",ENTRY)) QUIT:DATA=""
  . NEW ONVALUE SET ONVALUE=$PIECE(DATA,"^",2)
  . IF $DATA(OUT("RAW PARAMS","B",ONVALUE)) QUIT
  . NEW TURNOFFCMD SET TURNOFFCMD=$PIECE(DATA,"^",3)  ;"piece 1 is the TURN OFF command.  
  . IF TURNOFFCMD["""""" SET TURNOFFCMD=""
  . SET OUT("STATE",ENTRY)="OFF"  ;"TURNOFFCMD
  ;"Next, show the entries for which we can't tell state
  FOR  SET ENTRY=$ORDER(INFO("UNKNOWN",ENTRY)) QUIT:ENTRY=""  DO
  . NEW DATA SET DATA=$GET(INFO("ENTRY",ENTRY)) QUIT:DATA=""
  . NEW TURNONCMD SET TURNONCMD=$PIECE(DATA,"^",1)  ;"piece 1 is the TURN ON command.
  . NEW TURNOFFCMD SET TURNOFFCMD=$PIECE(DATA,"^",3)  ;"piece 1 is the TURN OFF command.  
  . ;"SET OUT("STATE",ENTRY)=TURNONCMD_"/"_TURNOFFCMD_"--> ? STATUS"
  . SET OUT("STATE",ENTRY)="?"
  ;"Check Special cases
  SET ENTRY="TERMINATOR"  ;"Both "" and TERM=$C() can indicate found.  NULL means $C(13)
  IF $DATA(OUT("RAW PARAMS","B","TERM="))=0 DO
  . SET OUT("STATE","TERMINATOR")="$C(13)"
  ;"Now compare to default state
  SET APARAM=""
  FOR  SET APARAM=$ORDER(INFO("ENTRY",APARAM)) QUIT:APARAM=""  DO
  . NEW ENTRY SET ENTRY=$GET(INFO("ENTRY",APARAM)) QUIT:ENTRY=""
  . NEW DEFAULTVAL SET DEFAULTVAL=$PIECE(ENTRY,"^",5)
  . NEW CURRENTVAL SET CURRENTVAL=$GET(OUT("STATE",APARAM))
  . IF CURRENTVAL=DEFAULTVAL QUIT
  . IF DEFAULTVAL="#",CURRENTVAL=+CURRENTVAL QUIT
  . SET OUT("NON-DEFAULT",APARAM)=CURRENTVAL
  ;"Get $x,$y for device
  USE DEVICE
  SET OUT("STATE","$X")=$X,OUT("NON-DEFAULT","$X")=$X
  SET OUT("STATE","$Y")=$Y,OUT("NON-DEFAULT","$Y")=$Y
  USE $IO
  ;"Now filter out undesired nodes
  NEW FLAG FOR FLAG="G,g,GENERAL","N,n,NON-DEFAULT","R,r,RAW PARAMS","S,s,STATE" DO
  . NEW F1,F2 SET F1=$PIECE(FLAG,",",1),F2=$PIECE(FLAG,",",2)
  . IF (FILTER[F1)!(FILTER[F2) QUIT
  . NEW NODE SET NODE=$PIECE(FLAG,",",3)
  . KILL OUT(NODE)
  KILL OUT("RAW PARAMS","B")
D2ADN ;   
  QUIT
  ;
  ;
  ;"DEPRECIATED, SEE ABOVE
DEVDELTA(DEVICE,PRIORARR,OUT,INFO)  ;"Get difference between current device and prior saved array
  ;"INPUT:  DEVICE -- the name of the device to query.  Can call with $P or $IO etc. 
  ;"        PRIORARR -- PASS BY REFERENCE. This should be output from DEV2ARR^TMGTEST3
  ;"           Must have GENERAL and STATE nodes (i.e. these should not be filtered out)
  ;"        OUT -- PASS BY REFERENCE.  AN OUT PARAMETER.  This only returns entries if values DIFFER between current and PRIORARR
  ;"             OUT("STATE","$Y")=13
  ;"             OUT("STATE","WRAP_LINE")="OFF"
  ;"        INFO -- OPTIONAL.  PASS BY FREFERENCE.  An array that has setup information from prior run (for faster execution)
  ;"RESULT: 1^OK, or -1^ERROR MESSAGE
  NEW CURARR,CURVAL,PRIORVAL,LABEL
  NEW RESULT SET RESULT="1^OK"  ;"default
  KILL OUT
  IF $DATA(PRIORARR("STATE"))=0 DO  GOTO DDDN
  . SET RESULT="-1^PRIORARR does not contain STATE node. Aborting"
  IF $DATA(PRIORARR("GENERAL"))=0 DO  GOTO DDDN
  . SET RESULT="-1^PRIORARR does not contain GENERAL node. Aborting"
  SET PRIORVAL=$GET(PRIORARR("GENERAL","NAME"))
  IF PRIORVAL="" DO  GOTO DDDN
  . SET RESULT="-1^No device name in PRIORARR. Aborting."
  DO DEV2ARR(DEVICE,.CURARR,"GS",.INFO)  ;"Store off a device, with all it's parameters   
  ;"Now, compare CURARR and PRIORARR
  SET CURVAL=$GET(CURARR("GENERAL","NAME"))
  IF CURVAL="" DO  GOTO DDDN
  . SET RESULT="-1^Unable to get name of DEVICE. Aborting."
  IF CURVAL'=PRIORVAL DO  GOTO DDDN
  . SET RESULT="-1^Unable to compare different devices"
  FOR LABEL="NAME","STATUS","TYPE" DO
  . SET CURVAL=$GET(CURARR("GENERAL",LABEL))
  . SET PRIORVAL=$GET(PRIORARR("GENERAL",LABEL))
  . IF CURVAL=PRIORVAL KILL CURARR("GENERAL",LABEL)
  SET LABEL=""
  FOR  SET LABEL=$ORDER(CURARR("STATE",LABEL)) QUIT:LABEL=""  DO
  . SET CURVAL=$GET(CURARR("STATE",LABEL))
  . SET PRIORVAL=$GET(PRIORARR("STATE",LABEL))
  . IF CURVAL=PRIORVAL KILL CURARR("STATE",LABEL)
  MERGE OUT=CURARR  
DDDN ;
  QUIT RESULT
  ;
  ;
  ;"DEPRECIATED, SEE ABOVE
RESTORDEV(PRIORARR,INFO,OPTION) ;"Use device specified in PRIORARR, setting parameters to saved values
  ;"NOTICE!! This function is designed for use with YottaDB TERMINAL devices only
  ;"INPUT:  PRIORARR -- PASS BY REFERENCE. This should be output from DEV2ARR^TMGTEST3
  ;"           Must have GENERAL and STATE nodes (i.e. these should not be filtered out)
  ;"        INFO -- OPTIONAL.  PASS BY FREFERENCE.  An array that has setup information from prior run (for faster execution)
  ;"        OPTION -- OPTIONAL. 
  ;"             OPTION("VERBOSE")=1  If found then changes made will be written out.  
  ;"RESULT: 1^OK, or -1^ERROR MESSAGE
  NEW RESULT SET RESULT="1^OK"  ;"default
  NEW VERBOSE SET VERBOSE=+$GET(OPTION("VERBOSE"))
  NEW DEVICE SET DEVICE=$GET(PRIORARR("GENERAL","NAME"))
  IF VERBOSE DO
  . WRITE !,"Starting RESTOREDEV^TMGTEST3 for device: ",DEVICE,!
  IF DEVICE="" DO  GOTO RDDN
  . SET RESULT="-1^Unable to get device name from PRIORARR. Aborting"
  NEW DELTA
  SET RESULT=$$DEVDELTA(DEVICE,.PRIORARR,.DELTA,.INFO)  ;"Get difference between current device and prior saved array
  IF RESULT'>0 GOTO RDDN
  NEW PARAMSTR SET PARAMSTR=""
  NEW LABEL SET LABEL=""
  FOR  SET LABEL=$ORDER(DELTA("STATE",LABEL)) QUIT:(LABEL="")!(+RESULT'>0)  DO  ;"DELTA only contains entries for values that have CHANGED
  . ;"IF VERBOSE WRITE "Checking parameter: ",LABEL,!    
  . NEW DATA SET DATA=$GET(INFO("ENTRY",LABEL)) QUIT:DATA=""   ;"e.g. $Y not in INFO
  . NEW TURNONCMD SET TURNONCMD=$PIECE(DATA,"^",1)  ;"piece 1 is the TURN ON command.
  . NEW TURNOFFCMD SET TURNOFFCMD=$PIECE(DATA,"^",3)  ;"piece 1 is the TURN OFF command.  
  . NEW ISKEYVAL SET ISKEYVAL=$PIECE(DATA,"^",6)
  . NEW OUTPUTPARAM SET OUTPUTPARAM=""
  . NEW PRIORVAL SET PRIORVAL=$GET(PRIORARR("STATE",LABEL))
  . NEW CURVAL SET CURVAL=$GET(DELTA("STATE",LABEL))
  . IF ISKEYVAL=1 DO       
  . . ;"                  Label      Turn_on        Turn_off        KEY:VALUE
  . . ;"HANDLE THESE -> EXCEPTION  / EXCE=@       / EXCEPTION=""  /  1
  . . ;"HANDLE THESE -> CTRL_TRAP  / CTRAP=@      / CTRAP=""      /  1
  . . ;"HANDLE THESE -> TERMINATOR / TERMINATOR=@ / NOTERMINATOR  /  1
  . . ;"HANDLE THESE -> FILTERXY   / FILTER=@     / NOFILTER      /  1
  . . ;"HANDLE THESE -> PAGE_LEN   / LENGTH=@     / LENGTH=0      /  1
  . . ;"HANDLE THESE -> PAGE_WIDTH / WIDTH=@      / WIDTH=0       /  1
  . . IF PRIORVAL="OFF" DO  QUIT                                                              
  . . . SET OUTPARAM=TURNOFFCMD          
  . . IF TURNONCMD["=@" DO
  . . . NEW CMD SET CMD=$PIECE(TURNONCMD,"=@",1)
  . . . IF LABEL="FILTERXY" DO  QUIT
  . . . . IF PRIORVAL["," DO  QUIT  ;"HANDLE PRIORVAL: FIL=(ESCAPES,CHARACTERS) --> FILTER="ESCAPES":FILTER="CHARACTERS"
  . . . . . SET OUTPARAM="NOFILTER"  ;"<-- this should clear prior values, allowing addition of just those wanted. 
  . . . . . IF PRIORVAL["(" SET PRIORVAL=$PIECE(PRIORVAL,"(",2)
  . . . . . IF PRIORVAL[")" SET PRIORVAL=$PIECE(PRIORVAL,")",1)
  . . . . . NEW IDX FOR IDX=1:1:$LENGTH(PRIORVAL,",") DO
  . . . . . . NEW PART SET PART=$PIECE(PRIORVAL,",",IDX) QUIT:PART=""
  . . . . . . IF OUTPARAM'="" SET OUTPARAM=OUTPARAM_":"
  . . . . . . SET OUTPARAM=OUTPARAM_CMD_"="""_PART_""""
  . . . . SET OUTPARAM=CMD_"="""_PART_""""
  . . . ELSE  SET OUTPARAM=CMD_"="_PRIORVAL
  . ELSE  IF ISKEYVAL=0 DO 
  . . ;"                  Label      Turn_on        Turn_off       KEY:VALUE
  . . ;"HANDLE THESE -> CTRLC_BRK  / CENABLE      / NOCENABLE    /  0
  . . ;"HANDLE THESE -> EDIT_MODE  / EDITING      / NOEDITING    /  0
  . . ;"HANDLE THESE -> EMPTERM    / EMPTERM      / NOEMPTERM    /  0
  . . ;"HANDLE THESE -> ESC_PROS   / ESCAPE       / NOESCAPE     /  0
  . . ;"HANDLE THESE -> INSERT     / INSERT       / NOINSERT     /  0
  . . ;"HANDLE THESE -> PASSTHRU   / PASTHRU      / NOPASTHRU    /  0
  . . ;"HANDLE THESE -> READSYNC   / READSYNC     / NOREADSYNC   /  0
  . . ;"HANDLE THESE -> TYPEAHEAD  / TYPEAHEAD    / NOTYPEAHEAD  /  0
  . . ;"HANDLE THESE -> WRAP_LINE  / WRAP         / NOWRAP       /  0
  . . ;"HANDLE THESE -> ECHO_INPUT / ECHO         / NOECHO       /  0
  . . ;"HANDLE THESE -> CANONICAL  / CANONICAL    / NOCANONICAL  /  0
  . . IF VERBOSE DO
  . . . WRITE "For param: ",LABEL," prior value was: ",PRIORVAL,!
  . . . WRITE "For param: ",LABEL," current value is: ",CURVAL,!
  . . . WRITE "Turn ON command=",TURNONCMD,!
  . . . WRITE "Turn OFF command=",TURNOFFCMD,!
  . . IF PRIORVAL="OFF" DO                                              
  . . . SET OUTPARAM=TURNOFFCMD
  . . ELSE  IF PRIORVAL="ON" DO
  . . . SET OUTPARAM=TURNONCMD
  . . ELSE  IF PRIORVAL="?" DO
  . . . WRITE !,"TO DO.  Implement restoration to of param ",LABEL," to ",PRIORVAL," in RESTORDEV^TMGTEST3",!
  . . . ;"Nothing to do here since we don't have these working right now. 
  . IF PARAMSTR'="" SET PARAMSTR=PARAMSTR_":"
  . SET PARAMSTR=PARAMSTR_OUTPARAM
  IF PARAMSTR'="" SET PARAMSTR="("_PARAMSTR_")"
  IF PARAMSTR'="" DO
  . USE DEVICE:@PARAMSTR
  . IF VERBOSE WRITE !,"Command executed by RESTORDEV^TMGTEST3: USE ",DEVICE,":@",PARAMSTR,!
  ELSE  DO   ;"Note If past device and current device are same, no changes are needed, and PARAMSTR will be ""
  . USE DEVICE
  . IF VERBOSE WRITE !,"Command executed by RESTORDEV^TMGTEST3: USE ",DEVICE,!
RDDN ;  
  QUIT RESULT
  ;  
SETUPSAV(ARR)  ;"Get information about TERMINAL device parameters    DEPRECIATED, SEE ABOVE
  ;"INPUT: ARR -- PASS BY REFERENCE, AN OUT PARAMETER. Format
  ;"        ARR("ENTRY",<ParameterLabel>)=<TurnOnCmd>^<OnDisplay>^<TurnOffCmd>^<OffDisplay>^<DefaultValue>^<IsKeyValue>
  ;"        ARR("ONDISP",<OnDisplay>,<ParameterLabel>)=""
  ;"        ARR("OFFDISP",<OffDisplay>,<ParameterLabel>)=""
  ;"        ARR("UNKNOWN",<OffDisplay>,<ParameterLabel>)=""  <-- entries that have NULL OnDisplay and OffDisplay values
  ;"RESULT: none
  ;
  NEW DONE SET DONE=0
  NEW IDX FOR IDX=1:1 DO  QUIT:DONE
  . NEW LINE,LABEL,TURNON,ONDISP,TURNOFF,OFFDISP,DEFAULT   
  . SET LINE=$TEXT(PARAMTABLE+IDX^TMGTEST3)
  . IF LINE["<DONE>" SET DONE=1 QUIT
  . SET LINE=$PIECE(LINE,";;""",2,99)
  . SET LABEL=$$TRIM^XLFSTR($PIECE(LINE,"/",1))
  . SET TURNON=$$TRIM^XLFSTR($PIECE(LINE,"/",2))
  . SET ONDISP=$$TRIM^XLFSTR($PIECE(LINE,"/",3))
  . SET TURNOFF=$$TRIM^XLFSTR($PIECE(LINE,"/",4))
  . SET OFFDISP=$$TRIM^XLFSTR($PIECE(LINE,"/",5))
  . SET DEFAULT=$$TRIM^XLFSTR($PIECE(LINE,"/",6))
  . SET ISKEYVAL=$$TRIM^XLFSTR($PIECE(LINE,"/",7))
  . SET ARR("ENTRY",LABEL)=TURNON_"^"_ONDISP_"^"_TURNOFF_"^"_OFFDISP_"^"_DEFAULT_"^"_ISKEYVAL
  . IF ONDISP'="" SET ARR("ONDISP",ONDISP,LABEL)=""
  . IF OFFDISP'="" SET ARR("OFFDISP",OFFDISP,LABEL)=""
  . IF ONDISP="",OFFDISP'="" SET ARR("NULLWHENON",LABEL)=""
  . IF OFFDISP="",ONDISP'="" SET ARR("NULLWHENOFF",LABEL)=""
  . IF OFFDISP="",ONDISP="" SET ARR("UNKNOWN",LABEL)=""
  QUIT
   ;"------------------------------------------------------------------------------------------
   ;"Devide Parameters Table format: 
   ;"------------------------------------------------------------------------------------------
   ;"               Cmd to        Display     Cmd to          Display      Default
   ;"   Label       Turn_on       if ON       Turn_off        if OFF       Value      KEY:VALUE
   ;"------------------------------------------------------------------------------------------
PARAMTABLE ;                                                
   ;;"EXCEPTION  / EXCE=@       / EXCE=     / EXCEPTION=""  /            / OFF      /  1
   ;;"CTRLC_BRK  / CENABLE      /           / NOCENABLE     / NOCENE     / ON       /  0
   ;;"CTRL_TRAP  / CTRAP=@      / CTRA=     / CTRAP=""      /            / OFF      /  1
   ;;"EDIT_MODE  / EDITING      / EDIT      / NOEDITING     /            / OFF      /  0
   ;;"EMPTERM    / EMPTERM      / EMPTERM   / NOEMPTERM     /            / OFF      /  0
   ;;"ESC_PROS   / ESCAPE       /           / NOESCAPE      / NOESCA     / OFF      /  0
   ;;"INSERT     / INSERT       /           / NOINSERT      / NOINSE     / ON       /  0
   ;;"PASSTHRU   / PASTHRU      / PAST      / NOPASTHRU     / NOPAST     / OFF      /  0
   ;;"TERMINATOR / TERMINATOR=@ / TERM=     / NOTERMINATOR  / TERM=$C()  / $C(13)   /  1
   ;;"UPPERCASE  / CONVERT      / CONVERT   / NOCONVERT     /            / OFF      /  0
   ;;"FILTERXY   / FILTER=@     / FIL=      / NOFILTER      /            / OFF      /  1
   ;;"HOSTSYNC   / HOSTSYNC     / HOSTSYNC  / NOHOSTSYNC    / NOHOSTSYNC / OFF      /  0
   ;;"READSYNC   / READSYNC     / READS     / NOREADSYNC    / NOREADS    / OFF      /  0
   ;;"TTSYNC     / TTSYNC       / TTYSNC    / NOTTSYNC      / NOTTSYNC   / ON       /  0
   ;;"TYPEAHEAD  / TYPEAHEAD    / TYPE      / NOTYPEAHEAD   /            / ON       /  0
   ;;"PAGE_LEN   / LENGTH=@     / LENG=     / LENGTH=0      / LENG=0     / #        /  1
   ;;"PAGE_WIDTH / WIDTH=@      / WIDTH=    / WIDTH=0       / WIDTH=0    / #        /  1
   ;;"WRAP_LINE  / WRAP         /           / NOWRAP        / NOWRAP     / ON       /  0
   ;;"ECHO_INPUT / ECHO         /           / NOECHO        / NOECHO     / ON       /  0
   ;;"CANONICAL  / CANONICAL    / CANONICAL / NOCANONICAL   /            / OFF      /  0
   ;;"<DONE>  
  