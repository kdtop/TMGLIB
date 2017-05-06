TMGKERN2 ;TMG/kst/OS Specific functions ;11/21/09, 2/2/14
         ;;1.0;TMG-LIB;**1**;11/21/09
 ;
 ;"TMG KERNEL FUNCTIONS -- 2
 ;"This module is primarly for functions to support a SOCKET
 ;"    connection between two different VistA instances.  One running
 ;"    as a server, and the other as a client.
 ;"I.e. functions that are OS specific.
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
 ;"RUNSERVER(PORT,TMGMSGFN,TMGVERBOSE) --open up a socket that will listen to requests from a client.
 ;"SEND(MSG) -- funnel all writing back to the client through this, so checksums can be calc'd
 ;"ASK(MSG) -- funnel all writing to server through this function, so that checksums can calc'd
 ;"DEBUGMSG(NOTE) ;
 ;"RUNCLIENT(HOST,PORT) --Establish a connection with specified server.  Then maintain connection, sending queries to server, and returning results.
 ;"MSGCLIENT(JNUM,TMGQUERY,REPLY,ERROR,TIMEOUT) -- send messages to background client.
 ;"CLEARBUF(JNUM,ERROR) -- remove all messages from message buffer.
 ;"RUNMONITOR --Show DEBUG messages as they are added.
 ;"=======================================================================
 ;"Dependancies
 ;"=======================================================================
 ;"TMGUSRIF
 ;"=======================================================================
 ;
RUNSERVER(PORT,TMGMSGFN,TMGVERBOSE) ;
        ;"Purpose:  To open up a socket that will listen to requests from a client.
        ;"Input:  Port -- the port to listen on
        ;"        TMGMSGFN -- the NAME of a function that will handle incoming
        ;"                    messages.  E.g.  'HANDLMSG^MOD1'
        ;"                    This function will be called as follows:
        ;"                    xecute "DO "_TMGMSGFN_"(TMGCLIENT)"
        ;"                    So the function must accept at least 1 parameter.
        ;"                    NOTE: Any output that the handler function wants to go back
        ;"                          to the client should be sent to SEND^TMGKERN2(MSG), so
        ;"                          that error checking and self-correction can urr.
        ;"        TMGVERBOSE -- If 1 then some output will be show to console.
        ;"Results: 1 IF successful, -1^Error Message IF failed.
        ;"NOTE:  This will be messaging protocol.
        ;"   #HELLO# will be sent on startup (possibly preceeded by 2 blank lines)
        ;"   #BYE# will be sent when server is QUITting
        ;"   Server will respond to query of #BYE# by QUITting.
        ;"   Server will turn control over to the message-handler-fn, allowing it to write
        ;"      out as many lines as it wants.
        ;"   After message-handler-fn returns, the server will send #DONE# to signal done.
        ;"
        NEW RESULT,TMGDELIM,TMGTCPDEV,TMGTIMEOUT
        NEW TMGCLIENT,TMGANSWR,TMGCODE
        KILL ^TMG("TMP","LOG","TCP")
        ;
        SET RESULT=1 ;"Default of success
        IF +$GET(PORT)'>0 DO  GOTO RSVRDN
        . SET RESULT="-1^Invalid port number passed. Received: "_$GET(PORT)
        IF $GET(TMGMSGFN)="" DO  GOTO RSVRDN
        . SET RESULT="-1^No Message handling function passed."
        IF $TEXT(@TMGMSGFN)="" DO  GOTO RSVRDN
        . SET RESULT="-1^Message handler ["_TMGMSGFN_"] appears invalid"
        SET PORT=+$GET(PORT)
        SET TMGDELIM=$CHAR(13)
        SET TMGTCPDEV="server$"_$JOB
        SET TMGTIMEOUT=60
        SET TMGCODE="DO "_TMGMSGFN_"(TMGCLIENT)"
        SET TMGVERBOSE=+$GET(TMGVERBOSE)
        ;
        IF TMGVERBOSE DO
        . WRITE "Starting server.  Trying to connect to client..."
        OPEN TMGTCPDEV:(ZLISTEN=PORT_":TCP":attach="server":DELIMITER=TMGDELIM:NOWRAP):TMGTIMEOUT:"SOCKET"
        IF $TEST=0 DO  GOTO RSVRDN
        . SET RESULT="-1^Attempts to open server failed (timedout)"
        USE TMGTCPDEV
        WRITE /listen(1)
        WRITE /wait(TMGTIMEOUT)
        DO SEND("#HELLO#")
        ;
        IF TMGVERBOSE DO
        . USE $P
        . WRITE "  Connected!",!
        . WRITE "Press [ESC] multiple times to abort (and wait up to 60 sec).",!
        . WRITE "Press '?' to see server output.",!
        . WRITE "RUNNING SERVER..."
        . USE TMGTCPDEV
L1      ;"Main Listen-Reply loop
        NEW TMGCLIENT,TMGI,TMGDONE,TMGLEN
        SET TMGDONE=-1,TMGI=1
        NEW TMGSHOWOUT SET TMGSHOWOUT=0
        DO DEBUGMSG("Starting main listen-reply loop")
        FOR  DO  QUIT:(TMGDONE>0)!(TMGCLIENT="#BYE#")
        . USE $P
        . NEW USERKEY
        . READ *USERKEY:0
        . SET TMGDONE=(USERKEY=27)
        . IF TMGDONE DO  QUIT
        . . DO SEND("#BYE#")
        . . USE TMGTCPDEV
        . SET:(USERKEY=63) TMGSHOWOUT=1 ;"63='?' Turn on showing ouput on console.
        . SET:(USERKEY=33) TMGSHOWOUT=0 ;"33='!' Turn off showing ouput on console.
        . USE TMGTCPDEV
        . READ TMGCLIENT:TMGTIMEOUT
        . IF ($TEST=0)!(TMGCLIENT="") DO  QUIT
        . . DO DEBUGMSG("$TEST=0 or TMGCLIENT='', so QUITting")
        . . SET TMGDONE=TMGDONE+1
        . . DO SEND("#BYE#")
        . . SET TMGCLIENT="#BYE#"
        . ;"Check for valud query from client.
        . SET TMGLEN=+$PIECE(TMGCLIENT,$CHAR(255),2)
        . SET TMGCLIENT=$PIECE(TMGCLIENT,$CHAR(255),1)
        . IF TMGLEN'=$LENGTH(TMGCLIENT) DO  QUIT
        . . DO DEBUGMSG("Length doesn't match checksup, so asking for resend")
        . . DO SEND("#RESEND#")
        . DO DEBUGMSG("TMGCLIENT="_TMGCLIENT)
        . IF TMGCLIENT="#ENQ#" DO SEND("#ACK#") QUIT
        . IF TMGCLIENT="#BYE#" DO SEND("#BYE#") QUIT
        . ELSE  DO SEND("#GOTQUERY#")
        . SET TMGI=TMGI+1
        . DO
        . . NEW $ETRAP
        . . SET $ETRAP="W ""<Error in message handler>"",!,$ZSTATUS,!,""#BYE"",! SET $ETRAP="""",$ecode="""""
        . . SET TMGMSGSUM=0
        . . ;"DO DEBUGMSG("About to execute handler code")
        . . XECUTE TMGCODE
        . . ;"DO DEBUGMSG("Back from handler code")
        . USE TMGTCPDEV    ;"Ensure handler didn't redirect $IO
        . ;"Send message to indicate done sending reply (will allow multi line responses)
        . ;"Also append a count of total number of characters that have been sent, for error checking.
        . DO DEBUGMSG("Sending back a DONE and total for amount sent: "_TMGMSGSUM)
        . DO SEND("#DONE#^"_TMGMSGSUM)
        . IF (TMGDONE>0) DO DEBUGMSG("NOTE: TMGDONE is > 0")
        . IF (TMGCLIENT="#BYE#") DO DEBUGMSG("NOTE: TMGCLIENT = '#BYE#'")
        ;
        DO DEBUGMSG("Closing socket")
        CLOSE TMGTCPDEV
        ;
RSVRDN  USE $P
        DO DEBUGMSG("Quitting RUNSERVER")
        IF TMGVERBOSE DO
        . WRITE "Quitting ",$SELECT((RESULT=1):"normally",1:"with errors"),!
        QUIT RESULT
 ;
 ;
SEND(MSG) ;
        ;"Purpose: To funnel all writing back to the client through this function, so that
        ;"         checksums can be calculated for error checking...
        ;"Input: MSG -- The message to WRITE out
        ;"NOTE: Will use globally scoped variable (on server side) TMGMSGSUM
        ;"      It is expected that RUNSERVER will SET this to 0 before passing control
        ;"      over to a message handler.
        ;
        IF 1=0 DO
        . NEW NUM SET NUM=+$GET(^TMG("TMP","LOG","TCP",0))
        . SET NUM=NUM+1
        . SET ^TMG("TMP","LOG","TCP",NUM,"NB")=$H_" SENDING; "_MSG
        . SET ^TMG("TMP","LOG","TCP",0)=NUM
        ;
        WRITE MSG,!
        SET TMGMSGSUM=+$GET(TMGMSGSUM)+$LENGTH(MSG)
        IF $GET(TMGSHOWOUT)=1 DO
        . USE $P
        . WRITE "('!' to hide) ",MSG,!
        . NEW USERKEY
        . READ *USERKEY:0
        . SET TMGDONE=(USERKEY=27)
        . SET:(USERKEY=33) TMGSHOWOUT=0 ;"33='!' Turn off showing ouput on console.
        . USE TMGTCPDEV
        QUIT
 ;
 ;
ASK(MSG) ;
        ;"Purpose: To funnel all writing to server through this function, so that
        ;"         checksums can be maintained for error checking...
        ;"Input: MSG -- The message to WRITE out
        IF 1=0 DO
        . NEW NUM SET NUM=+$GET(^TMG("TMP","LOG","TCP",0))
        . SET NUM=NUM+1
        . SET ^TMG("TMP","LOG","TCP",NUM,"ASK")=$H_"; "_MSG
        . SET ^TMG("TMP","LOG","TCP",0)=NUM
        ;
        WRITE MSG_$CHAR(255)_$LENGTH(MSG),!
        QUIT
 ;
 ;
DEBUGMSG(NOTE) ;f
        IF 1=0 DO
        . NEW NUM SET NUM=+$GET(^TMG("TMP","LOG","TCP",0))
        . SET NUM=NUM+1
        . SET ^TMG("TMP","LOG","TCP",NUM,"NB")=$H_"; "_NOTE
        . SET ^TMG("TMP","LOG","TCP",0)=NUM
        QUIT
 ;
 ;
RUNCLIENT(HOST,PORT) ;"NOTE: meant to be run as a background process
        ;"Purpose: Establish a connection with specified server.  Then maintain connection,
        ;"         sending queries to server, and returning results.  Will take as input
        ;"         a messaging global ^TMG("TMP","TCP",$J,"TS",<index>)=<query>    TS=ToServer
        ;"         And replies will be stored in ^TMG("TMP","TCP",$J,"FS",<index>)=<query>  FS=FromServer
        ;"Input: HOST -- the IP address, (or name for DNS lookup) of the server.
        ;"       PORT -- the port that the server is listening on.
        ;"Result: none
        ;"Output: Results will be stored in ^TMG("TMP","TCP",$J,"RESULT")=<result>
        ;"              1 -- IF successful, -1^Error Message IF failed.
        ;"!!NOTICE!! -- This can't be used to transfer binary files, because $CHAR(255) is used
        ;"              as a signalling character for error checking.
        ;"
        NEW RESULT,TMGDELIM,TMGTCPDEV,TMGTIMEOUT
        ;"Setup vars
        SET TMGTCPDEV="client$"_$JOB
        SET TMGTIMEOUT=30
        KILL ^TMG("TMP","TCP",$J,"RESULT")
        KILL ^TMG("TMP","LOG","TCP")
        SET RESULT=1
        ;"Validate input
        IF +$GET(PORT)'>0 DO  GOTO RCLDN
        . SET RESULT="-1^Valid port number passed. Received: "_$GET(PORT)
        IF $GET(HOST)="" DO  GOTO RCLDN
        . SET RESULT="-1^No Host passed."
        SET PORT=+$GET(PORT)
        IF PORT'>0 DO  GOTO RCLDN
        . SET RESULT="-1^Invalid port: ["_PORT_"]"
        ;"Open up the TCP/IP connection
        DO DEBUGMSG("NOTE: Job number="_$JOB)
        DO DEBUGMSG("Starting to open connection with server")
        OPEN TMGTCPDEV:(CONNECT=HOST_":"_PORT_":TCP":ATTACH="client":DELIMITER=$CHAR(13):NOWRAP):TMGTIMEOUT:"SOCKET"
        IF $TEST=0 DO  GOTO RCLDN
        . SET RESULT="-1^Error on OPEN of SOCKET"
        DO DEBUGMSG("Open succeeded.")
        USE TMGTCPDEV
        ;"Make sure server is ready to send information.
        NEW TMGI,SRVREPLY
        DO DEBUGMSG("Starting read (up to 3 tries), waiting for #HELLO#")
        FOR TMGI=1:1:3 DO  QUIT:(SRVREPLY="#HELLO#")
        . READ SRVREPLY:TMGTIMEOUT
        IF SRVREPLY'="#HELLO#" DO  GOTO RCLDN
        . SET RESULT="-1^Failed to get a '#HELLO#' from server"
        DO DEBUGMSG("We got a #HELLO# alright.  Great!")
        SET ^TMG("TMP","TCP",$J,"RESULT")=$GET(RESULT)
        ;
        ;"Now process messaging.
RC1     NEW TSREF SET TSREF=$NAME(^TMG("TMP","TCP",$J,"TS"))
        NEW FSREF SET FSREF=$NAME(^TMG("TMP","TCP",$J,"FS"))
        NEW NTIME,STIME SET STIME=$PIECE($H,",",2)
        NEW TMGQUERY SET TMGQUERY=""
        NEW TMGIDLE SET TMGIDLE=0
        NEW TMGABORT SET TMGABORT=0
        DO DEBUGMSG("About to start main loop for messaging")
        FOR  DO  QUIT:(TMGQUERY="#BYE#")!(SRVREPLY="#BYE#")!(TMGABORT=1)
        . IF SRVREPLY'="#RESEND#" DO
        . . SET TMGI=$ORDER(@TSREF@(""))
        . . IF TMGI="" DO  ;"Start idle handling
        . . . SET TMGQUERY=""
        . . . SET NTIME=$PIECE($H,",",2)
        . . . IF (NTIME-STIME)<15 DO  QUIT
        . . . . IF TMGIDLE HANG 0.5  ;"This loop was taking 90+% of CPU othewise.
        . . . SET TMGQUERY="#ENQ#"  ;"send an ENQ every 15 seconds of idleness.
        . . . SET STIME=$PIECE($H,",",2)  ;"Reset idle counter
        . . . SET TMGIDLE=1 ;"If idle for 15 seconds, then turn on idle mode.  Will take 0.5 sec to turn off
        . . ELSE  DO
        . . . SET TMGIDLE=0
        . . . SET TMGQUERY=$GET(@TSREF@(TMGI))  ;"Get query from user
        . . . KILL @TSREF@(TMGI)
        . . . SET STIME=$PIECE($H,",",2)  ;"Reset idle counter
        . . . IF $DATA(@TSREF)'=0 DO
        . . . . NEW I SET I=""
        . . . . FOR  SET I=$ORDER(@TSREF@(I)) QUIT:(I="")  DO
        . . . . . DO DEBUGMSG("Left over messages found!: "_$GET(@TSREF@(I)))
        . IF TMGQUERY="" QUIT
        . USE TMGTCPDEV
        . DO ASK(TMGQUERY)  ;"Send out query to server.
        . ;"Check for acknowledgement from server of query.
        . READ SRVREPLY:TMGTIMEOUT ;"read reply.
        . ;"IF ($TEST=0)!(SRVREPLY="")!(SRVREPLY="#BYE#") DO  QUIT
        . IF ($TEST=0)!(SRVREPLY="#BYE#") DO  QUIT
        . . SET TMGABORT=1
        . . DO DEBUGMSG("1: Got bad or #BYE# reply, so QUITting (Setting TMGABORT=1)")
        . IF SRVREPLY="#ACK#" DO  QUIT
        . ;"Now process server reply to query.
        . IF SRVREPLY="#RESEND" QUIT  ;"Server replied with RESEND, so will ask query again
        . IF SRVREPLY="#GOTQUERY#" FOR  DO  QUIT:(SRVREPLY="#BYE#")!(SRVREPLY="#DONE#")!(TMGABORT=1)
        . . READ SRVREPLY:TMGTIMEOUT ;"read reply.
        . . ;"IF ($TEST=0)!(SRVREPLY="")!(SRVREPLY="#BYE#") DO  QUIT
        . . IF ($TEST=0)!(SRVREPLY="#BYE#") DO  QUIT
        . . . DO DEBUGMSG("2: Got bad or #BYE# reply, so QUITting (Setting TMGABORT=1)")
        . . . SET TMGABORT=1  ;"Got NULL or bad or #BYE# reply, so setting QUITting "
        . . IF SRVREPLY="" QUIT  ;"Ignore null replies (i.e. server sent a blank line) ?? good idea ??
        . . IF SRVREPLY["#DONE#" DO  ;"Cut off checksum, but DO store #DONE#
        . . . DO DEBUGMSG("Got an #DONE#.  Later I should check on checksum")
        . . . ;"Later check on checksum
        . . . SET SRVREPLY="#DONE#"
        . . SET TMGI=+$ORDER(@FSREF@(""),-1)
        . . SET @FSREF@(TMGI+1)=SRVREPLY
        DO DEBUGMSG("Done with loop, so sending #BYE#")
        DO DEBUGMSG("TMGQUERY="_TMGQUERY)
        DO DEBUGMSG("SRVREPLY="_SRVREPLY)
        DO DEBUGMSG("TMGABORT="_TMGABORT)
        DO ASK("#BYE#") ;"Done with loop and exiting, so sending #BYE#"
        CLOSE TMGTCPDEV
        ;
RCLDN   USE $P
        KILL ^TMG("TMP","TCP",$J)
        HALT ;"(QUIT background process)
 ;
 ;
MSGCLIENT(JNUM,TMGQUERY,REPLY,ERROR,TIMEOUT) ;
        ;"Purpose: To send messages to background client.  So this will be one function
        ;"        that the programmer may interact with.  The reason for having the client
        ;"        run as a separate job is so that the server and the client can talk back
        ;"        and forth with ENQ<-->ACK upon either timing out, to keep the connection
        ;"        alive.
        ;"Input: JNUM -- The job number of the background client process
        ;"        TMGQUERY -- The message to send to the server.
        ;"        REPLY -- PASS BY REFERENCE, AN OUT PARAMETER.  Prior data killed.
        ;"                  REPLY(1)=<a reply line from server>
        ;"                  REPLY(2)=<a reply line from server>
        ;"                  REPLY(3)=<a reply line from server>
        ;"        ERROR -- PASS BY REFERENCE, AN OUT PARAMETER.  Prior data killed.
        ;"              If error, filled with -1^Message.
        ;"        TIMEOUT -- OPTIONAL.  Default=1 (in seconds)
        ;"Result: none
        ;"Will SET globally-scoped variable TMGABORT=1 IF timeout or other error
        ;
        KILL ERROR,REPLY
        NEW RESULT SET RESULT=""
        SET JNUM=+$GET(JNUM)
        IF JNUM'>0 SET ERROR="-1^BAD JOB NUMBER" GOTO MSGDN
        SET TMGQUERY=$GET(TMGQUERY)
        IF TMGQUERY="" SET ERROR="-1^NO QUERY PROVIDED" GOTO MSGDN
        SET TIMEOUT=+$GET(TIMEOUT,1)
        NEW SHOWPROG SET SHOWPROG=0
        NEW NTIME,STIME SET STIME=$PIECE($H,",",2)
        KILL ^TMG("TMP","TCP",JNUM,"FS") ;"Clear message buffer before communication
        NEW TMGI SET TMGI=+$ORDER(^TMG("TMP","TCP",JNUM,"TS",""),-1)
        SET ^TMG("TMP","TCP",JNUM,"TS",TMGI+1)=TMGQUERY
        IF TMGQUERY="#BYE#" GOTO MSGDN
        NEW LINECT SET LINECT=1
        NEW TMGCT SET TMGCT=0
        NEW REPLYI SET REPLYI=1
        NEW STIME SET STIME=$PIECE($H,",",2)
        NEW USERKEY
        NEW TMGSHOWOUT SET TMGSHOWOUT=0
        NEW ONELINE SET ONELINE=""
        FOR  DO  QUIT:(ONELINE="#DONE#")
        . SET TMGI=$ORDER(^TMG("TMP","TCP",JNUM,"FS",""))
        . READ *USERKEY:0
        . ;"SET TMGDONE=(USERKEY=27) QUIT:TMGDONE
        . SET:(USERKEY=63) TMGSHOWOUT=1 ;"63='?' Turn on showing ouput on console.
        . SET:(USERKEY=33) TMGSHOWOUT=0 ;"33='!' Turn off showing ouput on console.
        . IF TMGI="" DO  QUIT
        . . SET NTIME=$PIECE($H,",",2)
        . . IF (NTIME-STIME)'<TIMEOUT DO
        . . . SET ERROR="-1^TIMED OUT WAITING FOR CLIENT TO GET REPLY FROM SERVER"
        . . . SET ONELINE="#DONE#"
        . . . SET TMGABORT=1
        . SET ONELINE=$GET(^TMG("TMP","TCP",JNUM,"FS",TMGI))
        . SET TMGCT=TMGCT+1
        . IF TMGSHOWOUT=1 WRITE "('!' to hide) ",ONELINE,!
        . IF (ONELINE'["#ERROR TRAPPED#") DO
        . . IF (ONELINE["#THINKING#") DO  QUIT
        . . . NEW MSG SET MSG=$PIECE(ONELINE,"|",2)
        . . . IF MSG="" SET MSG="(Server is working...)"
        . . . WRITE MSG,!
        . . . SET STIME=$PIECE($H,",",2)  ;"Ignore server message to avoid timeout.
        . . IF (ONELINE'="#DONE#") DO
        . . . SET REPLY(REPLYI)=ONELINE
        . . . SET REPLYI=REPLYI+1
        . . . SET LINECT=LINECT+1
        . ELSE  DO
        . . SET ERROR="-1^Error trapped on server side"
        . . SET ERROR=ERROR_": "_$PIECE(ONELINE,"#ERROR TRAPPED#",2)
        . . SET ONELINE="#DONE#"
        . . SET TMGABORT=1
        . KILL ^TMG("TMP","TCP",JNUM,"FS",TMGI)
        . IF (SHOWPROG=0),($$HDIFF^XLFDT($H,STIME,2)>15) DO  ;"Turn on progress bar after 15 seconds.
        . . SET SHOWPROG=1
        . IF (SHOWPROG=1),(TMGCT>1000) DO
        . . DO PROGBAR^TMGUSRI2(100,"Receiving Data ('?' to monitor): "_LINECT,-1,-1,70)
        . . SET TMGCT=0
MSGDN   ;
        KILL ^TMG("TMP","TCP",JNUM,"FS")  ;"Clear message buffer after communication
        QUIT
 ;
 ;
CLEARBUF(JNUM,ERROR) ;
        ;"Purpose: To remove all messages from message buffer.
        ;"Input: JNUM -- The job number of the background client process
        ;"        ERROR -- PASS BY REFERENCE, AN OUT PARAMETER.  Prior data killed.
        ;"              If error, filled with -1^Message.
        ;"Result: None
        ;
        KILL ERROR
        SET JNUM=+$GET(JNUM)
        IF JNUM'>0 SET ERROR="-1^BAD JOB NUMBER" GOTO CLBFDN
        NEW TMGI
        FOR  DO  QUIT:(TMGI="")
        . SET TMGI=$ORDER(^TMG("TMP","TCP",JNUM,"TS",""))
        . IF TMGI="" QUIT
        . KILL ^TMG("TMP","TCP",JNUM,"TS",TMGI)
        FOR  DO  QUIT:(TMGI="")
        . SET TMGI=$ORDER(^TMG("TMP","TCP",JNUM,"FS",""))
        . IF TMGI="" QUIT
        . KILL ^TMG("TMP","TCP",JNUM,"FS",TMGI)
        ;
CLBFDN  QUIT
 ;
 ;
RUNMONITOR ;
        ;"Purpose: This is a debugging routine.  If run in a separate process, it will
        ;"         show DEBUG messages as they are added.
        NEW NUM,TMGDONE,MSG,MAX
        SET TMGDONE=0
        FOR NUM=1:1 DO  QUIT:(TMGDONE>0)
        . READ *TMGDONE:0
        . SET MAX=+$GET(^TMG("TMP","LOG","TCP",0))
        . IF NUM>MAX SET NUM=MAX QUIT
        . NEW NODE SET NODE=$ORDER(^TMG("TMP","LOG","TCP",NUM,""))
        . WRITE NODE,": ",$GET(^TMG("TMP","LOG","TCP",NUM,NODE)),!
        QUIT
        ;
