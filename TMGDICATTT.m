TMGDICATTT ;KST- Test TMG VERSION OF MODIFY FILE ATTR ;6 JAN,2011
         ;;1.0;TMG-LIB;**1**;1/6/11
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
TEST     ;"  Example code to show use.
         NEW TEST,OUT
         SET TEST("FILE")=227111
         SET TEST("FLD","NUM")=1.6
         SET TEST("FLD","NAME")="PEARS"
         ;
         SET TEST("FLD","MULTIPLE?")="N" ;"Optional
         SET TEST("FLD","MULTIPLE?","ASK FOR ANOTHER?")="N"
         SET TEST("FLD","MULTIPLE?","SUB-DICT NUM")=227111.1234  ;"E.g. 227111.111
         SET TEST("FLD","SEE ADDING NEW?")="Y"
         ;
         SET TEST("FLD","MANDITORY?")="NO" ;Optional
         ;"SET TEST("FLD","HELP PROMPT")="This is a help prompt..."
         SET TEST("FLD","XECUTABLE HELP")=""
         SET TEST("FLD","DESCRIPTION",0)="This is decription line 0"
         SET TEST("FLD","DESCRIPTION",1)="line 1"
         SET TEST("FLD","DESCRIPTION",2)="line 2"
         SET TEST("FLD","STORE","SUBSCRIPT")="1"
         SET TEST("FLD","STORE","PIECE")=1

        ;" 1, or "DATE/TIME"
        ;" 2, or "NUMERIC"
        ;" 3, or "SET OF CODES"
        ;" 4, or "FREE TEXT"
        ;" 5, or "WORD-PROCESSING"
        ;" 6, or "COMPUTED"
        ;" 7, or "POINTER TO A FILE"
        ;" 8, or "VARIABLE-POINTER"
        ;" 9, or "MUMPS"

         NEW TYPE SET TYPE=4
         IF TYPE=1 DO
         . SET TEST("FLD",1,"EARLIEST DATE")="1/1/1980"
         . SET TEST("FLD",1,"LATEST DATE")="2/2/2012"
         . SET TEST("FLD",1,"IMPRECISE DATE OK")="NO"
         . SET TEST("FLD",1,"TIME ALLOWED")="YES"
         . SET TEST("FLD",1,"TIME REQUIRED")="NO"
         . SET TEST("FLD",1,"SEC ALLOWED")="Y"
         IF TYPE=2 DO
         . SET TEST("FLD",2,"INCLUSIVE LOWER BOUND")=-888
         . SET TEST("FLD",2,"INCLUSIVE UPPER BOUND")=999
         . SET TEST("FLD",2,"DOLLAR AMOUNT?")="NO"
         . SET TEST("FLD",2,"MAX NUMBER OF DIGITS")=3
         IF TYPE=3 DO
         . SET TEST("FLD",3,"SET","I")="Incomplete"
         . SET TEST("FLD",3,"SET","S")="Signed"
         . SET TEST("FLD",3,"SET","U")="Unsigned"
         . SET TEST("FLD",3,"SET","P")="Processed"
         IF TYPE=4 DO  ;"FREE TEXT
         . SET TEST("FLD",4,"MIN LEN")=3
         . SET TEST("FLD",4,"MAX LEN")=16
         . SET TEST("FLD",4,"PATTERN MATCH")=""  ;"EXAMPLE: "X?1A.A" OR "X'?.P"
         IF TYPE=5 DO  ;"WP
         . SET TEST("FLD",5,"WORD WRAP MODE")="YES"
         . SET TEST("FLD",5,"IGNORE |")="NO"
         IF TYPE=7 DO  ;"pointer
         . SET TEST("FLD",7,"POINT TO WHAT FILE")=8925
         IF TYPE=8 DO  ;"Variable pointer.
         . SET TEST("FLD",8,1,"POINT TO WHAT FILE")=8925
         . SET TEST("FLD",8,1,"MESSAGE")="Msg for 8925"
         . SET TEST("FLD",8,1,"PREFIX")="TIU"
         . SET TEST("FLD",8,1,"INPUT SCREEN CODE")="SET DIC(""S"")=1"
         . SET TEST("FLD",8,1,"INPUT SCREEN DESCRIPTION")="Descr for input screen."
         . SET TEST("FLD",8,1,"LAYGO")="Y"
         . SET TEST("FLD",8,2,"POINT TO WHAT FILE")=2
         . SET TEST("FLD",8,2,"MESSAGE")="Msg for 2"
         . SET TEST("FLD",8,2,"PREFIX")="DPT"
         . SET TEST("FLD",8,2,"INPUT SCREEN CODE")="SET DIC(""S"")=2"
         . SET TEST("FLD",8,2,"INPUT SCREEN DESCRIPTION")="Descr for input screen #2."
         . SET TEST("FLD",8,2,"LAYGO")="N"
         . SET TEST("FLD",8,3,"POINT TO WHAT FILE")=19
         . SET TEST("FLD",8,3,"MESSAGE")="Msg for 19"
         . SET TEST("FLD",8,3,"PREFIX")="opt"
         . SET TEST("FLD",8,3,"INPUT SCREEN CODE")="SET DIC(""S"")=3"
         . SET TEST("FLD",8,3,"INPUT SCREEN DESCRIPTION")="Descr for input screen #3."
         . SET TEST("FLD",8,3,"LAYGO")="n"

         SET TEST("FLD","DATATYPE")=TYPE
         W $$ALTFIELD^TMGDICATT(.TEST,.OUT),!
         IF $DATA(OUT) DO ZWRITE^TMGZWR("OUT")
         QUIT
         ;


T2     ;
         NEW TEST,OUT
         SET TEST("FILE")=227111
         SET TEST("FLD","NUM")=1.6
         SET TEST("FLD","NAME")="PEARS"
         ;

        ;" 1, or "DATE/TIME"
        ;" 2, or "NUMERIC"
        ;" 3, or "SET OF CODES"
        ;" 4, or "FREE TEXT"
        ;" 5, or "WORD-PROCESSING"
        ;" 6, or "COMPUTED"
        ;" 7, or "POINTER TO A FILE"
        ;" 8, or "VARIABLE-POINTER"
        ;" 9, or "MUMPS"

         NEW TYPE SET TYPE=4
         IF TYPE=1 DO
         . SET TEST("FLD",1,"EARLIEST DATE")="1/1/1980"
         . SET TEST("FLD",1,"LATEST DATE")="2/2/2012"
         . SET TEST("FLD",1,"IMPRECISE DATE OK")="NO"
         . SET TEST("FLD",1,"TIME ALLOWED")="YES"
         . SET TEST("FLD",1,"TIME REQUIRED")="NO"
         . SET TEST("FLD",1,"SEC ALLOWED")="Y"
         IF TYPE=2 DO
         . SET TEST("FLD",2,"INCLUSIVE LOWER BOUND")=-888
         . SET TEST("FLD",2,"INCLUSIVE UPPER BOUND")=999
         . SET TEST("FLD",2,"DOLLAR AMOUNT?")="NO"
         . SET TEST("FLD",2,"MAX NUMBER OF DIGITS")=3
         IF TYPE=3 DO
         . SET TEST("FLD",3,"SET","I")="Incomplete"
         . SET TEST("FLD",3,"SET","S")="Signed"
         . SET TEST("FLD",3,"SET","U")="Unsigned"
         . SET TEST("FLD",3,"SET","P")="Processed"
         IF TYPE=4 DO  ;"FREE TEXT
         . SET TEST("FLD",4,"MIN LEN")=3
         . SET TEST("FLD",4,"MAX LEN")=16
         . SET TEST("FLD",4,"PATTERN MATCH")=""  ;"EXAMPLE: "X?1A.A" OR "X'?.P"
         IF TYPE=5 DO  ;"WP
         . SET TEST("FLD",5,"WORD WRAP MODE")="YES"
         . SET TEST("FLD",5,"IGNORE |")="NO"
         IF TYPE=7 DO  ;"pointer
         . SET TEST("FLD",7,"POINT TO WHAT FILE")=8925
         IF TYPE=8 DO  ;"Variable pointer.
         . SET TEST("FLD",8,1,"POINT TO WHAT FILE")=8925
         . SET TEST("FLD",8,1,"MESSAGE")="Msg for 8925"
         . SET TEST("FLD",8,1,"PREFIX")="TIU"
         . SET TEST("FLD",8,1,"INPUT SCREEN CODE")="SET DIC(""S"")=1"
         . SET TEST("FLD",8,1,"INPUT SCREEN DESCRIPTION")="Descr for input screen."
         . SET TEST("FLD",8,1,"LAYGO")="Y"
         . SET TEST("FLD",8,2,"POINT TO WHAT FILE")=2
         . SET TEST("FLD",8,2,"MESSAGE")="Msg for 2"
         . SET TEST("FLD",8,2,"PREFIX")="DPT"
         . SET TEST("FLD",8,2,"INPUT SCREEN CODE")="SET DIC(""S"")=2"
         . SET TEST("FLD",8,2,"INPUT SCREEN DESCRIPTION")="Descr for input screen #2."
         . SET TEST("FLD",8,2,"LAYGO")="N"
         . SET TEST("FLD",8,3,"POINT TO WHAT FILE")=19
         . SET TEST("FLD",8,3,"MESSAGE")="Msg for 19"
         . SET TEST("FLD",8,3,"PREFIX")="opt"
         . SET TEST("FLD",8,3,"INPUT SCREEN CODE")="SET DIC(""S"")=3"
         . SET TEST("FLD",8,3,"INPUT SCREEN DESCRIPTION")="Descr for input screen #3."
         . SET TEST("FLD",8,3,"LAYGO")="n"

         ;"SET TEST("EFLD","NAME")="@"
         SET TEST("EFLD","TITLE")="A Title of distinction"
         SET TEST("EFLD","AUDIT")="Y"
         SET TEST("EFLD","AUDIT CONDITION")="IF 1"
         SET TEST("EFLD","READ ACCESS")="$"
         SET TEST("EFLD","DELETE ACCESS")=""
         SET TEST("EFLD","WRITE ACCESS")="$"
         SET TEST("EFLD","SOURCE")="Some Source Description"
         ;"// not currently supported --> SET TEST("EFLD","DESTINATION"))
         ;"// not currently supported --> SET TEST("EFLD","GROUP"))
         SET TEST("EFLD","DESCRIPTION",0)="Edit description WP text 0"
         SET TEST("EFLD","DESCRIPTION",1)="Edit description WP text 1"
         SET TEST("EFLD","DESCRIPTION",2)="Edit description WP text 2"
         SET TEST("EFLD","TECHNICAL DESCRIPTION",0)="Technical description WP text 0"
         SET TEST("EFLD","TECHNICAL DESCRIPTION",1)="Technical description WP text 0"
         SET TEST("EFLD","TECHNICAL DESCRIPTION",2)="Technical description WP text 0"
         W $$EDITFLD^TMGDICATT(.TEST,.OUT),!
         IF $DATA(OUT) DO ZWRITE^TMGZWR("OUT")
         QUIT
        ;


T3       ;"Delete field
         NEW TEST,OUT
         SET TEST("FILE")=227111
         SET TEST("FLD","NUM")=1.6
         W $$DELFIELD^TMGDICATT(.TEST,.OUT),!
         IF $DATA(OUT) DO ZWRITE^TMGZWR("OUT")
         QUIT


