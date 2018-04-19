TMGPRNTR ;TMG/kst/Printer API Fns ;03/25/06; 3/11/11, 11/28/12, 2/2/14
         ;;1.0;TMG-LIB;**1**;04/25/04

 ;"TMG PRINTER API FUNCTIONS
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

 ;"MatchPrt(Printers)

 ;"=======================================================================
 ;" Functions Used During Printing Process
 ;"=======================================================================
 ;"SETJOB(Filename)
 ;"FINISH(Printer)


 ;"Dependancies
 ;"  TMGXDLG.m
 ;"=======================================================================
 ;"Private Functions
 ;"=======================================================================
 ;"GetPrinters^TMGPRNTR(Printers)
 ;"GetPrtDefs(PrtDefs)
 ;"PickPrtDef(LinuxPrt,PrtDefs,Output)



GetPrinters(Printers)
        ;"Purpose: To interact with Redhat 9 Linux printer system and get a list
        ;"        of defined printers
        ;"Input: (Printers is an OUT variable.  MUST PASS BY REFERENCE
        ;"Output: Printers variable will be filled like this:
        ;"                Printers(0,"COUNT")=2
        ;"                Printers(1)="Deskjet1"
        ;"                Printers(2)="Laser1"
        ;"result: 1=OkToCont  0=Abort

        ;"Notes: Here is a simple way to get the available printers from the CUPS system
        ;"#lpstat -p >/tmp/DefinedPrinters.txt
        ;"#cat DefinedPrinters.txt
        ;"printer Laser is idle.  enabled since Jan 01 00:00
        ;"--notice that in this case "Laser" is the name of the printer.  There is only 1 printer.
        ;"This printer could be used like this:
        ;"lp -d Laser MyFile.txt


        NEW Cmd,HookCmd
        NEW FileHandle
        NEW CmdResult
        NEW lpReport
        NEW index,PrtIndex
        NEW PrinterCount SET PrinterCount=0
        NEW cOKToCont SET cOKToCont=1
        NEW cAbort SET cAbort=0
        IF $DATA(TMGDEBUG)#10=0 NEW TMGDEBUG SET TMGDEBUG=0

        NEW result SET result=cOKToCont

        NEW CommFPath SET CommFPath="/tmp/"
        NEW CommFNAME SET CommFNAME="M_Printer_comm_"_$J_".tmp"
        NEW CommFile SET CommFile=CommFPath_CommFNAME

        SET HookCmd="lpstat -p>"_CommFile
        ;"WRITE "Here is hook command",!,!,HookCmd,!,!
        zsystem HookCmd

        SET CmdResult=$ZSYSTEM&255  ;"get result of execution. (low byte only)
        ;"WRITE "CmdResult=",CmdResult,!  ;"1=error
        IF CmdResult=0 SET result=cOKToCont ELSE  SET result=cAbort GOTO GPDone

        ;"Read output info Results
        SET FileHandle=$$FTG^%ZISH(CommFPath,CommFNAME,$name(lpReport("LIST")),3)
        ;"DO ZWRITE^TMGZWR("lpReport")

        ;"Now KILL the communication file... no longer needed.
        NEW FileSpec
        SET FileSpec(CommFile)=""
        SET result=$$DEL^%ZISH(CommFPath,$name(FileSpec))

        SET index=""
        FOR  DO  QUIT:(index="")
        . NEW s
        . SET s=$GET(lpReport("LIST",index))
        . IF s="" QUIT
        . NEW Prt SET Prt=$PIECE(s," ",2)
        . IF Prt'="" do
        . . SET PrinterCount=PrinterCount+1
        . . SET Printers(PrinterCount)=Prt
        . SET index=$ORDER(lpReport("LIST",index))

        ;"if $DATA(Printers) DO ZWRITE^TMGZWR("Printers")
        ;"w "done"

GPDone
        SET Printers(0,"COUNT")=PrinterCount

        QUIT result


GetPrtDefs(PrtDefs)
        ;"Purpose: To get a list of printer definitions (i.e. TERMINAL TYPES)
        ;"Input: PrtDefs -- SHOULD BE PASSED BY REFERENCE to receive results.
        ;"Output: (PrtDefs is changed)
        ;"                PrtDefs(0,"COUNT")=12
        ;"                PrtDefs(1,"NAME")="P-ANADEX"
        ;"                PrtDefs(1,"DESCRIPTION")="ANADEX PRINTER 10P"
        ;"                PrtDefs(2,"NAME")="P-CENT"
        ;"                PrtDefs(2,"DESCRIPTION")="Centronix printer"
        ;"                ... etc.
        ;"Result: 1=OKToCont 0=Abort

        ;"TERMINAL TYPE IF file 3.2

        NEW cOKToCont SET cOKToCont=1
        NEW cAbort SET cAbort=0
        IF $DATA(TMGDEBUG)#10=0 NEW TMGDEBUG SET TMGDEBUG=0

        NEW Matches,Msg
        IF $DATA(PriorErrorFound)=0 NEW PriorErrorFound
        IF $DATA(TMGDBINDENT)=0 NEW TMGDBINDENT SET TMGDBINDENT=0
        NEW NumMatches,index
        NEW PrtCount SET PrtCount=0
        NEW result SET result=cOKToCont
        NEW MatchValue SET MatchValue="P-"

        ;"======================================================
        ;"Call FIND^DIC
        ;"======================================================
        ;"Params:
        ;"FILE,IENS,FIELDS,FLAGS,VALUE,NUMBER,INDEXES,SCREEN,IDENTIFIER,TARGET_ROOT,MSG_ROOTS
        IF TMGDEBUG>0 DO DEBUGMSG^TMGDEBU4(.TMGDBINDENT,"  MatchValue=",MatchValue)
        DO FIND^DIC("3.2","","@;.01","",MatchValue,"*",,"",,"Matches","Msg")
        ;"======================================================
        ;"======================================================

        IF $DATA(Msg("DIERR"))'=0 DO  GOTO GPDDone
        . DO SHOWDIER^TMGDEBU2(.Msg,.PriorErrorFound)
        . SET result=cAbort

        IF $DATA(Matches) do
        . IF TMGDEBUG>0 DO DEBUGMSG^TMGDEBU4(.TMGDBINDENT,"Here are Matches entries")
        . IF TMGDEBUG>0 DO ZWRITE^TMGZWR("Matches")

        IF $DATA(Matches("DILIST"))=0 GOTO GPDDone

        SET NumMatches=$PIECE(Matches("DILIST",0),"^",1)
        KILL PrtDefs
        SET PrtDefs(0,"COUNT")=NumMatches
        IF NumMatches=0 GOTO GPDDone  ;"keep RecNumIEN default of 0
        IF TMGDEBUG>0 DO DEBUGMSG^TMGDEBU4(.TMGDBINDENT,"Here are Matches entries")
        IF TMGDEBUG>0 DO ZWRITE^TMGZWR("Matches")

        for index=1:1:NumMatches do
        . KILL OneMatch
        . NEW Name,Descr
        . SET Name=$GET(Matches("DILIST","ID",index,.01))
        . SET Descr=$GET(^%ZIS(2,index,9))
        . SET PrtDefs(index,"NAME")=Name
        . SET PrtDefs(index,"DESCRIPTION")=Descr

GPDDone
         QUIT result


PickPrtDef(LinuxPrt,PrtDefs,Output)
        ;"Purpose: To show all the printer types (TERMINAL TYPES), and have user pick one
        ;"Input: LinuxPrt -- name of Linux printer, as retrieved from GetPrinters()
        ;"         PrtDefs -- Array of printer defs, as returned from GetPrtDefs(PrtDefs)
        ;"                        Array will not be changed, even IF passed by reference.
        ;"         Output -- MUST BE PASSED BY REFERENCE.  Will be formated like this:
        ;"                Output(0,"COUNT")=1
        ;"                Output(1,"LINUX")="Laser1"    <----- Prior results
        ;"                Output(1,"TYPE")="P-ANADEX"
        ;"Output: Output -- MUST BE PASSED BY REFERENCE.  Output will be formated like this:
        ;"                Output(0,"COUNT")=2
        ;"                Output(1,"LINUX")="Laser1"    <----- Prior results
        ;"                Output(1,"TYPE")="P-ANADEX"
        ;"                Output(2,"LINUX")="Printer2"    <----- Added results
        ;"                Output(2,"TYPE")="P-CENT"
        ;"Result: 1=OKToCont  0=Abort, OR Cancel pressed.

        NEW cOKToCont SET cOKToCont=1
        NEW cAbort SET cAbort=0
        NEW result SET result=cAbort
        IF $DATA(TMGDEBUG)#10=0 NEW TMGDEBUG SET TMGDEBUG=0
        NEW tPrtDefs
        NEW DefCount,OutCount
        NEW index
        NEW UserPick

        SET DefCount=$GET(PrtDefs(0,"COUNT"),0)
        IF DefCount=0 DO  GOTO PPDefDone
        . WRITE "No printer defs!  Quitting!",!
        SET OutCount=$GET(Output(0,"COUNT"),0)
        Set Output(0,"COUNT")=OutCount  ;"Ensure this is SET before any need to abort

        for index=1:1:DefCount do
        . NEW s,Name,Descr
        . SET s=index_";  "
        . SET Name=$GET(PrtDefs(index,"NAME"))
        . ;"WRITE "converted: ",Name," to "
        . SET Name=$EXTRACT(Name,3,128)
        . ;"WRITE Name,!
        . SET Descr=$GET(PrtDefs(index,"DESCRIPTION"))
        . SET s=s_Name
        . IF Descr'="" SET s=s_Name_" -- "_Descr
        . SET tPrtDefs(index)=s

        NEW s SET s="---- Pick VistA driver for printer '"_LinuxPrt_"' ----\n\n"
        SET s=s_"(Note: If you can not find an corresponding driver for your\n"
        SET s=s_"printer, then see your installer regarding adding an\n"
        SET s=s_"appropriate entry to the TERMINAL TYPE file, then retry.)"
        SET UserPick=$$Combo^TMGXDLG(s,80,15,.tPrtDefs)
        IF UserPick="" GOTO PPDefDone
        SET index=+$PIECE(UserPick,";",1)
        IF index=0 GOTO PPDefDone
        SET OutCount=OutCount+1

        SET Output(OutCount,"LINUX")=LinuxPrt
        SET Output(OutCount,"TYPE")=PrtDefs(index,"NAME")
        Set Output(0,"COUNT")=OutCount

        SET result=cOKToCont
PPDefDone
        QUIT result



MatchPrt(Output)
        ;"Purpose: To create match between Linux printers, and definitions
        ;"Input: Output -- and out parameter. MUST BE PASSED BY REFERENCE
        ;"Output: (Output is changed) as follows
        ;"                Output(0,"COUNT")=2
        ;"                Output(1,"LINUX")="Deskjet1"  <-- suitable name for linux: lp -p PRINTER
        ;"                Output(1,"TYPE")="P-ANADEX"
        ;"                Output(2,"LINUX")="Laser1"    <-- suitable name for linux: lp -p PRINTER
        ;"                Output(2,"TYPE")="P-CENT"

        NEW cOKToCont SET cOKToCont=1
        NEW cAbort SET cAbort=0
        IF $DATA(TMGDEBUG)#10=0 NEW TMGDEBUG SET TMGDEBUG=0
        IF $DATA(DispMode)#10=0 NEW DispMode SET DispMode=1  ;"1=GUI, 3=Roll-n-Scroll
        NEW result SET result=cOKToCont
        NEW PrtDefs,Printers
        NEW PrtCount SET PrtCount=0
        KILL Output  ;"clear any prior entries.

        IF DispMode'=1 DO  GOTO SUPDone
        . WRITE "Currently unable to SET up printers in 'Roll-and-Scroll' mode.  Quitting.",!

        SET result=$$GetPrinters(.Printers)
        IF result=cAbort DO  GOTO SUPDone
        . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Unable to get Printers.")

        SET result=$$GetPrtDefs(.PrtDefs)
        IF result=cAbort DO  GOTO SUPDone
        . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Unable to get Printer definitions.")

        NEW tPrts
        NEW Selected SET Selected=""
        MERGE tPrts=Printers
        KILL tPrts(0)
        ;"set tPrts(2)="TestPrinter"  ;"temp!!!!!
        ;"set tPrts(3)="TestPrinter2"  ;"temp!!!!!
        FOR  DO  QUIT:Selected=""
        . ;"WRITE "loop1, selected=",Selected,!
        . SET Selected=$$Combo^TMGXDLG("Select Printer to Setup",,,.tPrts)
        . IF Selected="" QUIT
        . ;"WRITE "OK, now to SET up printer: ",Selected,!
        . NEW tResult SET tResult=$$PickPrtDef(Selected,.PrtDefs,.Output)
        . ;"Note: I am not doing anything IF user cancels pick of printer type.
        . ;"Now remove that printer from list of printers to install.
        . NEW index SET index=$ORDER(tPrts(""))
        . NEW NextIndex SET NextIndex=""
        . FOR  DO  QUIT:(index="")
        . . ;"WRITE "loop2, index=",index,!
        . . SET NextIndex=1
        . . IF index="" QUIT
        . . IF $GET(tPrts(index))=Selected DO  QUIT
        . . . SET NextIndex=$ORDER(tPrts(index))
        . . . KILL tPrts(index)
        . . . SET index=""
        . . SET index=$ORDER(tPrts(index))
        . IF $DATA(tPrts)=0 DO  QUIT
        . . SET Selected=""  ;"force QUIT
        . ;"Now move all entries below this one UP
        . SET index=NextIndex
        . FOR  DO  QUIT:index=""
        . . ;"WRITE "loop3, index=",index,!
        . . IF index="" QUIT
        . . SET tPrts(index-1)=tPrts(index)
        . . NEW PriorIndex SET PriorIndex=index
        . . SET index=$ORDER(tPrts(index))
        . . KILL tPrts(PriorIndex)
        . . IF $DATA(tPrts)=0 do
        . . . SET Selected=""
        . . . SET index=""

SUPDone
        QUIT result


SetupPrt
        ;"To query linux printer system, and create VistA entries for these.


        NEW cFile SET cFile="FILE"
        NEW cEntries SET cEntries="Entries"

 ;        NEW Data
 ;        SET Data(0,cFile)="3.5"
 ;        SET Data(0,cEntries)=1
 ;        SET Data
 ;
 ;  1  0;1                .01  NAME                                        [RFX]
 ;  2  1;1                .02  LOCATION OF TERMINAL                         [RF]
 ;     MN;0               .03  MNEMONIC                           <-Mult [3.501]
 ;  3   -0;1              .01   -MNEMONIC                                  [MFX]
 ;  4  1;4                .04  LOCAL SYNONYM                                 [F]
 ;  5  0;2                  1  $I                                          [RFX]
 ;  6  0;9                1.9  VOLUME SET(CPU)                              [FX]
 ;  7  0;11              1.95  SIGN-ON/SYSTEM DEVICE                        [SX]
 ; 8  TYPE;1               2  TYPE                                         [RS]
 ; 9  SUBTYPE;1            3  SUBTYPE                           <-Pntr  [RP3.2]
 ; 10  0;3                  4  ASK DEVICE                                    [S]
 ; 11  0;4                  5  ASK PARAMETERS                                [S]
 ; 12  1;5                5.1  ASK HOST FILE                                 [S]
 ; 13  1;6                5.2  ASK HFS I/O OPERATION                         [S]
 ; 14  0;12               5.5  QUEUING                                       [S]
 ; 15  90;1                 6  OUT-OF-SERVICE DATE                           [D]
 ; 17  90;3                 8  KEY OPERATOR                                  [F]
 ;18  91;1                 9  MARGIN WIDTH                              [NJ3,0]
 ; 19  91;3                11  PAGE LENGTH                               [NJ5,0]
 ; 20  1;11              11.2  SUPPRESS FORM FEED AT CLOSE                   [S]
 ; 27  POX;E1,245        19.7  PRE-OPEN EXECUTE                              [K]
 ; 28  PCX;E1,245        19.8  POST-CLOSE EXECUTE                            [K]
 ;
 ;
 ;NAME: TEST-LINUX-PRINTER                $I: <To be SET in PRE-OPEN EXECUTE>
 ;  ASK DEVICE: NO                        ASK PARAMETERS: NO
 ;  SIGN-ON/SYSTEM DEVICE: NO             LOCATION OF TERMINAL: Laughlin_Office
 ;  ASK HOST FILE: NO                     ASK HFS I/O OPERATION: NO
 ;  NEAREST PHONE: 787-7000               PAGE LENGTH: 80
 ;  FORM CURRENTLY MOUNTED: Plain paper
 ;  POST-CLOSE EXECUTE: DO FINISH^TMGPRNTR("laughlin_laser")
 ;  PRE-OPEN EXECUTE: DO SETJOB^TMGPRNTR(.IO) ;Note: Change IO (output file)
 ;  SUBTYPE: P-OTH80                      TYPE: TERMINAL
 ;  ASK DEVICE TYPE AT SIGN-ON: YES, ASK

        QUIT


 ;"=======================================================================
 ;"=======================================================================


GETJOBNM()
        ;"Purpose: To create a unique printer job name.  This will be used during a printing process
        ;"        that writes the printer file to the host file system, then passes file to Linux
        ;"        printing system.
        ;"Output: Returns name of file to put output into

        ;"UNIQUE will generate a filename based on time and job number
        ;"    i.e. 'Print-Job-628233034.tmp

        NEW Filename SET Filename=$$UNIQUE^%ZISUTL("/tmp/Print-Job.tmp")

        ;"Now store Filename for later transfer to Linux lpr
        NEW index SET index=$ORDER(^TMP("TMG",$J,"PRINT JOBS",""))
        IF index="" SET index=1
        SET ^TMP("TMG",$J,"PRINT JOBS",index)=Filename

        ;"WRITE !,"Print job name will be:",Filename,!
        QUIT Filename   ;"result returned by altering Filename
        
FINISH(Printer)
        ;"Purpose: to complete the printing process by sending the now-created file
        ;"        to Linux CUPS (the printing system).
        ;"Note: The lpr system itself will delete this print file when done (option -r)
        ;"Input: Printer OPTIONAL -- the name of the linux printer to send the job to.
        NEW index SET index=$ORDER(^TMP("TMG",$J,"PRINT JOBS",""))
        IF index="" GOTO FINDN
        NEW Filename SET Filename=$GET(^TMP("TMG",$J,"PRINT JOBS",index))
        close IO
        KILL IO(1,IO)
        KILL ^TMP("TMG",$J,"PRINT JOBS",$J,index)
        IF Filename'="" do
        . NEW CmdStr SET CmdStr="lpr "
        . IF $GET(Printer)'="" SET CmdStr=CmdStr_"-P "_Printer_" "
        . SET CmdStr=CmdStr_"-r " ;"option -r --> lpr deletes file after printing done.
        . SET CmdStr=CmdStr_Filename_" &"
        . zsystem CmdStr
FINDN   QUIT
        ;
GETBRWNM() ;
        ;"Purpose: To create a unique file name.  This will be used during a browsing process
        ;"        that writes the file to the host file system, then passes file to Linux
        ;"        editor 
        ;"Output: Returns name of file to put output into
        NEW FNAME SET FNAME=$$UNIQUE^%ZISUTL("/tmp/Browse-Job.tmp")
        ;"Now store Filename for later transfer to Linux editor
        NEW INDEX SET INDEX=+$ORDER(^TMP($J,"TMG BROWSE JOBS",""),-1)+1
        SET ^TMP($J,"TMG BROWSE JOBS",INDEX)=FNAME
        ;"WRITE !,"Using temp file: ",FNAME,!
        QUIT FNAME
        ;
FINBRWSE()
        ;"Purpose: to complete the browsing process by sending the now-created file
        ;"        to a Linux editor
        CLOSE IO
        KILL IO(1,IO)
        NEW INDEX SET INDEX=""
        FOR  SET INDEX=$ORDER(^TMP($J,"TMG BROWSE JOBS",INDEX)) QUIT:(+INDEX'>0)  DO
        . NEW FNAME SET FNAME=$GET(^TMP($J,"TMG BROWSE JOBS",INDEX))
        . KILL ^TMP($J,"TMG BROWSE JOBS",INDEX)
        . QUIT:(FNAME="")
        . NEW CMDSTR SET CMDSTR="/usr/bin/nano "_FNAME
        . ;NEW CMDSTR,TMP 
        . ;SET TMP=$ZTRNLNM("EDITOR")
        . ;SET CMDSTR=$SELECT($LENGTH(TMP):TMP,1:"/usr/bin/vi")_" "_FNAME
        . ZSYSTEM CMDSTR
        . ZSYSTEM "rm "_FNAME  ;delete file when done
        QUIT
        




