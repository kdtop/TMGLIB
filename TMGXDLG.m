TMGXDLG ;TMG/kst/M <--> Xdialog Interface ;03/25/06, 2/2/14
         ;;1.0;TMG-LIB;**1**;09/21/04
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;

 ;"M <--> Xdialog Interface

 ;"+------------------------------------------------------------+
 ;"|           O P E N  -  V I S T A   C O D E                  |..
 ;"+------------------------------------------------------------+ :
 ;"|                                                            | :
 ;"| M <--> Xdialog Interface                                   | :
 ;"|                                                            | :
 ;"| Kevin Toppenberg,MD                                        | :
 ;"| Started 9-21-04                                            | :
 ;"| GNU License Applies                                        | :
 ;"|                                                            | :
 ;"| Purpose: Linux command 'Xdialog' (and 'dialog')            | :
 ;"|          provide a convenient graphic interface that       | :
 ;"|          can be accessed in GT.M via the ZSYSTEM command   | :
 ;"|          This library is a wrapper for Xdialog.            | :
 ;"| Note: Xdialog requires the X display system.  This is a    | :
 ;"|       true GIU interface. 'dialog' provides the same       | :
 ;"|       functionality in a character-based environment       | :
 ;"|       The command Xdialog should be in /usr/bin.  If not,  | :
 ;"|       it may simply be copied into place.                  | :
 ;"|       A good web site that documents Xdialog is:           | :
 ;"|       http://xdialog.dyns.net/        and                  | :
 ;"|       http://thgodef.nerim.net/xdialog/doc/index.html      | :
 ;"|       http://linuxgazette.net/101/sunil.html               | :
 ;"+------------------------------------------------------------+ :
 ;"  :............................................................:

 ;"Note: Some of the following names are longer than 8 characters.
 ;"      However, the first 8 characters are .  You may leave
 ;"      off all characters > 8 -- but I put them in for 'beauty'

 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

 ;"Higher-level Interface (API)
 ;"-------------------------------

 ;"SetupConsts()
 ;"KillConsts()
 ;"ChClrScr()

 ;"$$YesNo^TMGXDLG(Text,width,height)
 ;"$$Msg^TMGXDLG(Title,Text,width,height,Modal,x,y)
 ;"$$Info^TMGXDLG(Text,width,height,timeout,Modal,x,y)
 ;"$$Edit^TMGXDLG(file,width,height,Results,x,y)
 ;"$$Log^TMGXDLG(file,width,height,Modal,x,y)
 ;"$$Text^TMGXDLG(file,width,height,Modal,x,y)
 ;"$$Tail^TMGXDLG(file,width,height,Modal,x,y)
 ;"$$Input^TMGXDLG(Title,width,height,InitText,Result,x,y)
 ;"$$Input2^TMGXDLG(Title,width,height,Label1,Init1Text,Label2,Init2Text,Result2,x,y)
 ;"$$Input3^TMGXDLG(Title,width,height,Label1,Init1Text,Label2,Init2Text,Label3,Init3Text,Result2,Result3,x,y)
 ;"$$RadioList^TMGXDLG(Text,List,width,height,x,y)
 ;"$$FileSel^TMGXDLG(Title,InitFile,width,height,x,y)
 ;"$$DirSel^TMGXDLG(Title,InitDir,width,height,x,y)
 ;"$$DateSel^TMGXDLG(Text,width,height,InitDay,InitMonth,InitYear,x,y)
 ;"$$TimeSel^TMGXDLG(Text,width,height,InitHour,InitMinute,InitSecond,x,y)
 ;"$$FontSel^TMGXDLG(InitFont,width,height,x,y)
 ;"$$Combo^TMGXDLG(Text,width,height,List,x,y)
 ;"$$Range^TMGXDLG(Text,width,height,min,max,init,x,y)
 ;"$$Range2^TMGXDLG(Text,width,height,label1,min1,max1,init1,label2,min2,max2,init2,Result2,x,y)
 ;"$$Range3^TMGXDLG(Text,width,height,label1,min1,max1,init1,label2,min2,max2,init2,label3,min3,max3,init3,Result2,Result3,x,y)
 ;"$$Spin^TMGXDLG(Text,width,height,min,max,label,init,x,y)
 ;"$$Spin2^TMGXDLG(Text,width,height,label1,min1,max1,init1,label2,min2,max2,init2,Result2,x,y)
 ;"$$Spin3^TMGXDLG(Text,width,height,label1,min1,max1,init1,label2,min2,max2,init2,label3,min3,max3,init3,Result2,Result3,x,y)



 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


 ;"Lower-level Interface (API)
 ;"-------------------------------
 ;"xyesno(Options,Results,Modal)
 ;"xmsg(Options,Results,Modal)
 ;"xinfo(Options,Results,Modal)
 ;"xguage(Options,Results,Modal)
 ;"xprogress(Options,Results,Modal)
 ;"xinput(Options,Results,Modal)
 ;"x2inputs(Options,Results,Modal)
 ;"x3inputs(Options,Results,Modal)
 ;"xcombo(Options,Results,Modal)
 ;"xrange(Options,Results,Modal)
 ;"x2range(Options,Results,Modal)
 ;"x3range(Options,Results,Modal)
 ;"xspin(Options,Results,Modal)
 ;"x2spin(Options,Results,Modal)
 ;"x3spin(Options,Results,Modal)
 ;"xlog(Options,Results,Modal)
 ;"xedit(Options,Results,Modal)
 ;"xtext(Options,Results,Modal)
 ;"xtail(Options,Results,Modal)
 ;"xchecklist(Options,Results,Modal)
 ;"xradiolist(Options,Results,Modal)
 ;"xmenu(Options,Results,Modal)
 ;"xtreeview(Options,Results,Modal)
 ;"xfilesel(Options,Results,Modal)
 ;"xdirsel(Options,Results,Modal)
 ;"xcalendarsel(Options,Results,Modal)
 ;"xtimesel(Options,Results,Modal)
 ;"xbuildlist(Options,Results,Modal)
 ;"xcolorsel(Options,Results,Modal)
 ;"xfontsel(Options,Results,Modal)


 ;"Expected format for Options:

 ;"The documentation for these options may be found at:
 ;"http://thgodef.nerim.net/xdialog/doc/index.html

 ;"Options should be an array inthe following format:
 ;"
 ;"  Options(xcCommon,xcWMClass)=<name>
 ;"  Options(xcCommon,xcRxcFile)=<gtkrc filename>
 ;"  Options(xcCommon,xcBackTitle)=<backtitle>
 ;"  Options(xcCommon,xcTitle"=<title>
 ;"  Options(xcCommon,xcAllowClose)=1  } A.
 ;"  Options(xcCommon,xcNoClose)=1     } B.   A & B are opposites
 ;"  Options(xcCommon,xcScreenCenter)=1      } A.
 ;"  Options(xcCommon,xcUnderMouse)=1        } B.
 ;"  Options(xcCommon,xcAutoPlacement)=1     } C.  A,B & C are mutually exclusive options
 ;"  Options(xcCommon,xcCenter)=1  } A.
 ;"  Options(xcCommon,xcRight)=1   } B.
 ;"  Options(xcCommon,xcLeft)=1    } C.
 ;"  Options(xcCommon,xcFill)=1    } D.   A,B,C & D are mutually exclusive options
 ;"  Options(xcCommon,xcNoWrap)=1   } A
 ;"  Options(xcCommon,xcWrap)=1      } B   A & B are opposites
 ;"  Options(xcCommon,xcCRWrap)=1       } A.
 ;"  Options(xcCommon,xcNoCRWrap)=1    } B.    A & B are opposites
 ;"  Options(xcCommon,xcStdErr)=1  } A.
 ;"  Options(xcCommon,xcStdOut)=1  } B. A & B are opposites
 ;"  Options(xcCommon,xcSeparator)=<character>    } A.
 ;"  Options(xcCommon,xcSeparateOutput)=1        } B.  A & B are opposites.
 ;"  Options(xcCommon,xcButtonsStyle)="default" or "icon" or  "text" (only one of these three values)
 ;"  Options(xcTransient,xcFixedFont)=1
 ;"  Options(xcTransient,xcPassword)=1
 ;"  Options(xcTransient,xcEditable)=1
 ;"  Options(xcTransient,xcTimeStamp)=1  } A.
 ;"  Options(xcTransient,xcDateStamp)=1  } B. A & B are mutually exclusive
 ;"  Options(xcTransient,xcReverse)=1
 ;"  Options(xcTransient,xcKeepColors)=1
 ;"  Options(xcTransient,xcInterval)=<timeout>
 ;"  Options(xcTransient,xcNotags)=1
 ;"  Options(xcTransient,xxcItemHelp)=1
 ;"  Options(xcTransient,xxcDefaultItem)=<tag>
 ;"  Options(xcTransient,xcIcon)=<xpm filename>
 ;"  Options(xcTransient,xcNook)=1
 ;"  Options(xcTransient,xcNoCancel)=1
 ;"  Options(xcTransient,xcNoButtons)=1
 ;"  Options(xcTransient,xxcDefaultNo)=1
 ;"  Options(xcTransient,xcWizard)=1
 ;"  Options(xcTransient,xcHelp)=<help>
 ;"  Options(xcTransient,xcPrint)=<printer>
 ;"  Options(xcTransient,xcCheck)=<label [<status>]>
 ;"  Options(xcTransient,xcOKLabel)=<label>
 ;"  Options(xcTransient,xcCancelLabel)=<label>
 ;"  Options(xcTransient,xcBeep)=1
 ;"  Options(xcTransient,xcBeepafter)=1
 ;"  Options(xcTransient,xcBegin)= <Yorg Xorg>
 ;"  Options(xcTransient,xcIgnoreEOF)=1
 ;"  Options(xcTransient,xcSmooth)=1
 ;"  Options(xcBox,xcText)=<value>
 ;"  Options(xcBox,xcHeight)=<value>
 ;"  Options(xcBox,xcWidth)=<value>
 ;"  Options(xcBox,xcTimeOut)=<value>
 ;"  Options(xcBox,xcPercent)=<value>
 ;"  Options(xcBox,xxcMaxDots)=<value>
 ;"  Options(xcBox,xcMsgLen)=<value>
 ;"  Options(xcBox,xcInit)=<value>
 ;"  Options(xcBox,xcLabel,N)=<value>
 ;"  Options(xcBox,xcInit,N)=<value>
 ;"  Options(xcBox,xcMin,N)=<value>
 ;"  Options(xcBox,xcMax,N)=<value>
 ;"  Options(xcBox,xcDefault,N)=<value
 ;"  Options(xcBox,xcFile)=<value>
 ;"  Options(xcBox,xcDirectory)=<value>
 ;"  Options(xcBox,xcFontName)=<value>
 ;"  Options(xcBox,xcDay)=<value>
 ;"  Options(xcBox,xcMonth)=<value>
 ;"  Options(xcBox,xcYear)=<value>
 ;"  Options(xcBox,xcHours)=<value>
 ;"  Options(xcBox,xcMinutes)=<value>
 ;"  Options(xcBox,xcSeconds)=<value>
 ;"  Options(xcBox,xcTag,N)=<value>
 ;"  Options(xcBox,xcItem,N)=<value>
 ;"  Options(xcBox,xcHelp,N)=<value>
 ;"  Options(xcBox,xcStatus,N)=<value>  {"on", "off", or "unavailable"}
 ;"  Options(xcBox,xcListHeight)=<value>
 ;"  Options(xcBox,xcItemdepth,N)=<value>

 ;"Notes:
 ;" - Not all options will apply to all dialogs, but IF the
 ;"   option is desired, it should be in the above format.
 ;" - No syntax checking is performed.  The options are simply
 ;"   passed to the Xdialog command in the proper order.
 ;" - Everything below should be considered CASE-SENSITIVE.
 ;" - Notice that the indexes used are constants (i.e. xcCommon)
 ;"   these are SET up by SetupConsts(), and may later be killed
 ;"   via KillConsts().  Their use will avoid spelling errors
 ;"   resulting in a missed parameter.

 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


SetupConsts()
        SET vDialog="Xdialog"
        SET xcCommon="common"
        SET xcWMClass="wmclass"
        SET xcRxcFile="rxcFile"
        SET xcBackTitle="backtitle"
        SET xcTitle="title"
        SET xcAllowClose="allow-close"
        SET xcNoClose="no-close"
        SET xcScreenCenter="cscreen-center"
        SET xcUnderMouse="under-mouse"
        SET xcAutoPlacement="autoplacement"
        SET xcCenter="center"
        SET xcRight="right"
        SET xcLeft="left"
        SET xcFill="fill"
        SET xcNoWrap="no-wrap"
        SET xcWrap="wrap"
        SET xcCRWrap="cr-wrap"
        SET xcNoCRWrap="no-cr-wrap"
        SET xcStdErr="stderr"
        SET xcStdOut="stdout"
        SET xcSeparator="separator"
        SET xcSeparateOutput="separate-output"
        SET xcButtonsStyle="buttons-style"
        SET xcTransient="transient"
        SET xcFixedFont="fixed-font"
        SET xcPassword="password"
        SET xcEditable="editable"
        SET xcTimeStamp="time-stamp"
        SET xcDateStamp="date-stamp"
        SET xcReverse="reverse"
        SET xcKeepColors="keep-colors"
        SET xcInterval="interval"
        SET xcNotags="no-tags"
        SET xxcItemHelp="item-help"
        SET xxcDefaultItem="default-item"
        SET xcIcon="icon"
        SET xcNook="no-ok"
        SET xcNoCancel="no-cancel"
        SET xcNoButtons="no-buttons"
        SET xxcDefaultNo="default-no"
        SET xcWizard="wizard"
        SET xcHelp="help"
        SET xcPrint="print"
        SET xcCheck="check"
        SET xcOKLabel="ok-label"
        SET xcCancelLabel="cancel-label"
        SET xcBeep="beep"
        SET xcBeepafter="beep-after"
        SET xcBegin="begin"
        SET xcIgnoreEOF="ignore-eof"
        SET xcSmooth="smooth"
        SET xcBox="box"
        SET xcText="text"
        SET xcHeight="height"
        SET xcWidth="width"
        SET xcTimeOut="timeout"
        SET xcPercent="percent"
        SET xxcMaxDots="maxdots"
        SET xcMsgLen="msglen"
        SET xcInit="init"
        SET xcLabel="label"
        SET xcMin="min"
        SET xcMax="max"
        SET xcDefault="default"
        SET xcFile="file"
        SET xcDirectory="directory"
        SET xcFontName="font name"
        SET xcDay="day"
        SET xcMonth="month"
        SET xcYear="year"
        SET xcHours="hours"
        SET xcMinutes="minutes"
        SET xcSeconds="seconds"
        SET xcTag="tag"
        SET xcItem="item"
        SET xcHelp="help"
        SET xcStatus="status"
        SET xcListHeight="list height"
        SET xcItemdepth="item depth"
        SET xcCmdLine="command_line_params"
        SET xcCmdArray="Array"
        SET xcCmdMaxLine="Max_line"
        SET xcDlgResult="Dialog Result"
        SET xcDlgOutput="Dialog Output"
        SET xcModalMode=1
        SET xcNonModal=0
        SET xcOptional=1
        SET xcNotOptional=0
        SET xcAddQuote=1
        SET xcNoQuote=0
        SET mrYes=0
        SET mrOK=0
        SET mrNext=0
        SET mrNo=1
        SET mrCancel=1
        SET mrHelp=2
        SET mrPrev=3
        SET mrError=255
        QUIT


KillConstants()
        KILL vDialog
        KILL xcCommon
        KILL xcWMClass
        KILL xcRxcFile
        KILL xcBackTitle
        KILL xcTitle
        KILL xcAllowClose
        KILL xcNoClose
        KILL xcScreenCenter
        KILL xcUnderMouse
        KILL xcAutoPlacement
        KILL xcCenter
        KILL xcRight
        KILL xcLeft
        KILL xcFill
        KILL xcNoWrap
        KILL xcWrap
        KILL xcCRWrap
        KILL xcNoCRWrap
        KILL xcStdErr
        KILL xcStdOut
        KILL xcSeparator
        KILL xcSeparateOutput
        KILL xcButtonsStyle
        KILL xcTransient
        KILL xcFixedFont
        KILL xcPassword
        KILL xcEditable
        KILL xcTimeStamp
        KILL xcDateStamp
        KILL xcReverse
        KILL xcKeepColors
        KILL xcInterval
        KILL xcNotags
        KILL xxcItemHelp
        KILL xxcDefaultItem
        KILL xcIcon
        KILL xcNook
        KILL xcNoCancel
        KILL xcNoButtons
        KILL xxcDefaultNo
        KILL xcWizard
        KILL xcHelp
        KILL xcPrint
        KILL xcCheck
        KILL xcOKLabel
        KILL xcCancelLabel
        KILL xcBeep
        KILL xcBeepafter
        KILL xcBegin
        KILL xcIgnoreEOF
        KILL xcSmooth
        KILL xcBox
        KILL xcText
        KILL xcHeight
        KILL xcWidth
        KILL xcTimeOut
        KILL xcPercent
        KILL xxcMaxDots
        KILL xcMsgLen
        KILL xcLabel
        KILL xcInit
        KILL xcMin
        KILL xcMax
        KILL xcDefault
        KILL xcFile
        KILL xcDirectory
        KILL xcFontName
        KILL xcDay
        KILL xcMonth
        KILL xcYear
        KILL xcHours
        KILL xcMinutes
        KILL xcSeconds
        KILL xcTag
        KILL xcItem
        KILL xcHelp
        KILL xcStatus
        KILL xcListHeight
        KILL xcItemdepth
        KILL xcCmdLine
        KILL xcCmdMaxLine
        KILL xcCmdArray
        KILL xcDlgResult
        KILL xcModalMode
        KILL xcNonModal
        KILL xcOptional
        KILL xcNotOptional
        KILL xcAddQuote
        KILL xcNoQuote
        KILL xcDlgOutput
        KILL mrYes
        KILL mrOK
        KILL mrNo
        KILL mrAbort
        KILL mrCancel
        KILL mrNext
        KILL mrHelp
        KILL mrPrev
        KILL mrError

        QUIT

SetGUI(UseGUI)
        ;"For those who DO not have an X system (i.e. a graphic display for unix/linux)
        ;"  then there is a backup plan that can DO most of these functions
        ;"  on a text display (cool, eh?)
        ;"Input: UseGUI --  IF 1 (the default), then the graphic method is used
        ;"                  IF 0, then the character (text drawing) based method is used

        IF $DATA(xcCommon)=0 DO SetupConsts()

        SET UseGUI=$GET(UseGUI,1)
        IF UseGUI=0 SET vDialog="dialog"
        ELSE  SET vDialog="Xdialog"
        QUIT

 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

YesNo(Text,width,height,x,y)
        ;"Purpose: To provide an easier access to xyesnot
        ;"Input: Text to display
        ;"       height & width of dialog -- [optional]
        ;"       x,y -- the display location of the dialog [optional]
        ;"Output: (none)
        ;"Results: returns results of box closure.
        ;"Notes: (none)

        NEW Options
        NEW Results,result

        IF $DATA(xcCommon)=0 DO SetupConsts()

        SET Options(xcBox,xcText)=Text
        SET Options(xcBox,xcHeight)=$GET(height,0)
        SET Options(xcBox,xcWidth)=$GET(width,0)
        IF $DATA(x) SET Options(xcTransient,xcBegin)=x_" "_$GET(y,0)

        DO xyesno(.Options,.Results,xcModalMode)  ;"Force won't return until dialog closed.
        SET result=Results(xcDlgResult)

        QUIT result


xyesno(Options,Results,Modal)
 ;" --yesno       <text> <height> <width>
        NEW Added

        DO ParamTextAdd(.Options,vDialog)
        DO SetCommons(.Options)
        DO SetTrans(.Options)
        DO ParamTextAdd(.Options," --yesno ")

        SET Added=$$AddParam(.Options,,xcText,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,,xcHeight)
        SET Added=$$AddParam(.Options,,xcWidth)

        DO LaunchCmd(.Options,.Results,.Modal)

        QUIT

 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

Msg(Title,Text,width,height,Modal,x,y)
        ;"Purpose: To provide an easier access to Xdialog function
        ;"Input: Text to display
        ;"       height & width of dialog -- [optional]
        ;"       x,y -- the display location of the dialog [optional]
        ;"       Modal: IF true, function does not return until dialog is closed.
        ;"              IF false, function returns immediately, and functions DO NOT
        ;"              reflect the user's button press.
        ;"Output: (none)
        ;"Results: Returns results of box closure (see Modal note above)
        ;"Notes: (none)

        NEW Options
        NEW Results,result

        IF $DATA(xcCommon)=0 DO SetupConsts()
        IF $DATA(Title) SET Options(xcCommon,xcTitle)=Title
        SET Options(xcBox,xcText)=Text
        SET Options(xcBox,xcHeight)=$GET(height,0)
        SET Options(xcBox,xcWidth)=$GET(width,0)
        SET Modal=$GET(Modal,xcNonModal)
        IF $DATA(x) SET Options(xcTransient,xcBegin)=x_" "_$GET(y,0)

        DO xmsg(.Options,.Results,Modal)
        SET result=Results(xcDlgResult)

        QUIT result


xmsg(Options,Results,Modal)
 ;" --msgbox      <text> <height> <width>
        NEW Added

        DO ParamTextAdd(.Options,vDialog)
        DO SetCommons(.Options)
        DO SetTrans(.Options)
        DO ParamTextAdd(.Options," --msgbox ")

        SET Added=$$AddParam(.Options,,xcText,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,,xcHeight)
        SET Added=$$AddParam(.Options,,xcWidth)

        DO LaunchCmd(.Options,.Results,.Modal)

        QUIT

 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

Info(Text,width,height,timeout,Modal,x,y)
        ;"Purpose: To provide an easier access to Xdialog function
        ;"Input: Text to display
        ;"       height & width of dialog -- [optional]
        ;"       [timeout]: time (in sec) delay until box automatically closes.
        ;"                      OPTIONAL--default=1
        ;"       [Modal]: IF true, function does not return until dialog is closed.
        ;"              IF false, function returns immediately, and functions DO NOT
        ;"              reflect the user's button press. OPTIONAL -- default=xcNonModal
        ;"       x,y -- the display location of the dialog [optional]
        ;"Output: (none)
        ;"Results: Returns results of box closure (see Modal note above)
        ;"Notes: (none)

        NEW Options
        NEW Results,result

        IF $DATA(xcCommon)=0 DO SetupConsts()

        SET Options(xcBox,xcText)=Text
        SET Options(xcBox,xcHeight)=$GET(height,0)
        SET Options(xcBox,xcWidth)=$GET(width,0)
        SET Modal=$GET(Modal,xcNonModal)
        IF $DATA(timeout) SET Options(xcBox,xcTimeOut)=timeout*1000
        IF $DATA(x) SET Options(xcTransient,xcBegin)=x_" "_$GET(y,0)

        DO xinfo(.Options,.Results,Modal)
        SET result=Results(xcDlgResult)

        QUIT result


xinfo(Options,Results,Modal)
 ;" --infobox     <text> <height> <width> [<timeout>]
        NEW Added

        DO ParamTextAdd(.Options,vDialog)
        DO SetCommons(.Options)
        DO SetTrans(.Options)
        DO ParamTextAdd(.Options," --infobox ")

        SET Added=$$AddParam(.Options,,xcText,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,,xcHeight)
        SET Added=$$AddParam(.Options,,xcWidth)
        SET Added=$$AddParam(.Options,,xcTimeOut,1)

        DO LaunchCmd(.Options,.Results,.Modal)

        QUIT

 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
 ;"NOT WORKING -- SEE NOTES ON GuageUpdate below...

Guage(Text,width,height,Percent,x,y)
        ;"Purpose: To provide an easier access to Xdialog function
        ;"         This is called to first display a guage dialog.
        ;"Input: Text to display
        ;"       height & width of dialog -- [optional]
        ;"       x,y -- the display location of the dialog [optional]
        ;"       Percent -- Percentage of progress bar to show
        ;"Output: (none)
        ;"Results: Returns a handle that is used in GuageUpdate
        ;"Notes: Box is left open unless Percent is > 100%
 ;"NOTICE: This function is not working.
        NEW Options
        NEW Results,result

        IF $DATA(xcCommon)=0 DO SetupConsts()

        SET Options(xcBox,xcText)=$GET(Text)
        SET Options(xcBox,xcHeight)=$GET(height,0)
        SET Options(xcBox,xcWidth)=$GET(width,0)
        SET Options(xcBox,xcPercent)=$GET(Percent,0)
        IF $DATA(x) SET Options(xcTransient,xcBegin)=x_" "_$GET(y,0)

        DO xguage(.Options,.Results,xcNonModal) ;"note: Xdialog will show box as non-modal regardless (I think)
        SET result=$GET(Text)_"^"_$GET(height)_"^"_$GET(width)  ;"This will be used as a handle.

        QUIT result

GuageUpdate(Handle,Percent)
        ;"Purpose: To provide an easier access to Xdialog function
        ;"         This is called to update the percentage on an existing form.
        ;"Input: Handle -- the handle returned from original call to Guage
        ;"       Percent -- Percentage of progress bar to show
        ;"Output: (none)
        ;"Results: 'StillActive' i.e. 1: box still open.  0:box closed
        ;"Notes: Box is left open unless Percent is > 100%
 ;"NOTICE: This function is not working.  To update a guage, the dialog is setup to accept NEW values
 ;"              on stdin.  I'm not sure how to DO this from inside M....
 ;"              Perhaps I could redirect stdin to a file, then WRITE values out to that file...
 ;"              However, when EOF is reached, then box is closed....

        NEW Text
        NEW height
        NEW width
        SET Handle=$GET(Handle)
        SET Percent=$GET(Percent)

        SET Text=$PIECE(Handle,"^",1)
        SET height=$PIECE(Handle,"^",2)
        SET width=$PIECE(Handle,"^",3)

        NEW dump
        SET dump=$$Guage(Text,width,height,Percent)

        QUIT '(Percent>100)


xguage(Options,Results,Modal)
 ;" --gauge       <text> <height> <width> [<percent>]
        NEW Added

        DO ParamTextAdd(.Options,vDialog)
        DO SetCommons(.Options)
        DO SetTrans(.Options)
        DO ParamTextAdd(.Options," --gauge ")

        SET Added=$$AddParam(.Options,,xcText,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,,xcHeight)
        SET Added=$$AddParam(.Options,,xcWidth)
        SET Added=$$AddParam(.Options,,xcPercent)

        DO LaunchCmd(.Options,.Results,.Modal)

        QUIT

 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
 ;"TO BE COMPLETED
 ;"Note: I will have the same problems with this function as I did with Guage...
 ;"      So for now, I WON'T IMPLEMENT THIS...

xprogress(Options,Results,Modal)
        ;"Purpose:
        ;"Input:
        ;"Output:
        ;"Results:
        ;"Notes:
 ;" --progress    <text> <height> <width> [<maxdots> [[-]<msglen>]]

 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
Input(Title,width,height,InitText,Result,x,y)
        ;"Purpose: To provide an easier access to Xdialog function
        ;"Input: Title -- text of input prompt to display
        ;"       height & width of dialog -- [optional]
        ;"       InitText -- default value [optional]
        ;"       Result -- a variable to put input into for return. PASS BY REFERENCE
        ;"       x,y -- the display location of the dialog [optional]
        ;"Output: The user input value is return in Result
        ;"Results: returns results of box closure.
        ;"Notes: (none)

        NEW Options
        NEW Results,result

        IF $DATA(xcCommon)=0 DO SetupConsts()

        SET Options(xcBox,xcText)=$GET(Title)
        SET Options(xcBox,xcHeight)=$GET(height,0)
        SET Options(xcBox,xcWidth)=$GET(width,0)
        IF $DATA(InitText) SET Options(xcBox,xcInit)=InitText
        IF $DATA(x) SET Options(xcTransient,xcBegin)=x_" "_$GET(y,0)

        DO xinput(.Options,.Results,xcModalMode)
        SET result=Results(xcDlgResult)

        SET Result=$GET(Results(xcDlgOutput,""))

        QUIT result

xinput(Options,Results,Modal)
 ;" --inputbox    <text> <height> <width> [<init>]

        NEW Added

        DO ParamTextAdd(.Options,vDialog)
        DO SetCommons(.Options)
        DO SetTrans(.Options)
        DO ParamTextAdd(.Options," --inputbox ")

        SET Added=$$AddParam(.Options,,xcText,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,,xcHeight)
        SET Added=$$AddParam(.Options,,xcWidth)
        SET Added=$$AddParam(.Options,,xcInit)

        DO LaunchCmd(.Options,.Results,.Modal)

        QUIT
 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
Input2(Title,width,height,Label1,Init1Text,Label2,Init2Text,Result1,Result2,x,y)
        ;"Purpose: To provide an easier access to Xdialog function
        ;"Input: Title -- text of input prompt to display
        ;"       height & width of dialog -- [optional]
        ;"       Label1 -- text of label for input 1 [optional]
        ;"       Init1Text -- default value [optional]
        ;"       Label2 -- text of label for input 2 [optional]
        ;"       Init2Text -- default value [optional]
        ;"       Result1 -- a variable to put first input into for return. PASS BY REFERENCE
        ;"       Result2 -- a variable to put second input into for return. PASS BY REFERENCE
        ;"       x,y -- the display location of the dialog [optional]
        ;"Output: The user input value is return in Result1
        ;"      result of 2nd user-input put into Result2
        ;"Results: returns results of box closure.
        ;"Notes: (none)

        NEW Options
        NEW Results,result

        IF $DATA(xcCommon)=0 DO SetupConsts()

        SET Options(xcCommon,xcSeparator)="^"
        SET Options(xcBox,xcText)=$GET(Title)
        SET Options(xcBox,xcHeight)=$GET(height,0)
        SET Options(xcBox,xcWidth)=$GET(width,0)
        SET Options(xcBox,xcLabel,1)=$GET(Label1," ")
        SET Options(xcBox,xcInit,1)=$GET(Init1Text," ")
        SET Options(xcBox,xcLabel,2)=$GET(Label2," ")
        SET Options(xcBox,xcInit,2)=$GET(Init2Text," ")
        IF $DATA(x) SET Options(xcTransient,xcBegin)=x_" "_$GET(y,0)

        DO x2inputs(.Options,.Results,xcModalMode)
        SET result=Results(xcDlgResult)

        SET Result1=$GET(Results(xcDlgOutput,""))
        SET Result2=$GET(Results(xcDlgOutput,1))

        QUIT result


x2inputs(Options,Results,Modal)
 ;" --2inputsbox  <text> <height> <width> <label1> <init1> <label2> <init2>
        NEW Added

        DO ParamTextAdd(.Options,vDialog)
        DO SetCommons(.Options)
        DO SetTrans(.Options)
        DO ParamTextAdd(.Options," --2inputsbox ")

        SET Added=$$AddParam(.Options,,xcText,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,,xcHeight)
        SET Added=$$AddParam(.Options,,xcWidth)
        SET Added=$$AddParam(.Options,1,xcLabel,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,1,xcInit,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,2,xcLabel,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,2,xcInit,xcNotOptional,xcAddQuote)

        DO LaunchCmd(.Options,.Results,.Modal)

        QUIT

 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
Input3(Title,width,height,Label1,Init1Text,Label2,Init2Text,Label3,Init3Text,Result1,Result2,Result3,x,y)
        ;"Purpose: To provide an easier access to Xdialog function
        ;"Input: Title -- text of input prompt to display
        ;"       height & width of dialog -- [optional]
        ;"       Label1 -- text of label for input 1
        ;"       Init1Text -- default value
        ;"       Label2 -- text of label for input 2
        ;"       Init2Text -- default value
        ;"       Label3 -- text of label for input 3
        ;"       Init3Text -- default value
        ;"       Result1 -- a variable to put first input into for return. PASS BY REFERENCE
        ;"       Result2 -- a variable to put second input into for return. PASS BY REFERENCE
        ;"       Result3 -- a variable to put third input into for return. PASS BY REFERENCE
        ;"       x,y -- the display location of the dialog [optional]
        ;"Output: The user input value is return in Result1
        ;"      result of 2nd user-input put into Result2
        ;"      result of 3rd user-input put into Result3
        ;"Results: returns results of box closure.
        ;"Notes: (none)

        NEW Options
        NEW Results,result

        IF $DATA(xcCommon)=0 DO SetupConsts()

        SET Options(xcCommon,xcSeparator)="^"
        SET Options(xcBox,xcText)=$GET(Title)
        SET Options(xcBox,xcHeight)=$GET(height,0)
        SET Options(xcBox,xcWidth)=$GET(width,0)
        SET Options(xcBox,xcLabel,1)=$GET(Label1," ")
        SET Options(xcBox,xcInit,1)=$GET(Init1Text," ")
        SET Options(xcBox,xcLabel,2)=$GET(Label2," ")
        SET Options(xcBox,xcInit,2)=$GET(Init2Text," ")
        SET Options(xcBox,xcLabel,3)=$GET(Label3," ")
        SET Options(xcBox,xcInit,3)=$GET(Init3Text," ")
        IF $DATA(x) SET Options(xcTransient,xcBegin)=x_" "_$GET(y,0)

        DO x3inputs(.Options,.Results,xcModalMode)
        SET result=Results(xcDlgResult)

        SET result=$GET(Results(xcDlgOutput,""))
        SET Result2=$GET(Results(xcDlgOutput,1))
        SET Result3=$GET(Results(xcDlgOutput,2))

        QUIT result


x3inputs(Options,Results,Modal)
 ;" --3inputsbox  <text> <height> <width> <label1> <init1> <label2> <init2> <label3> <init3>
        NEW Added

        DO ParamTextAdd(.Options,vDialog)
        DO SetCommons(.Options)
        DO SetTrans(.Options)
        DO ParamTextAdd(.Options," --3inputsbox ")

        SET Added=$$AddParam(.Options,,xcText,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,,xcHeight)
        SET Added=$$AddParam(.Options,,xcWidth)
        SET Added=$$AddParam(.Options,1,xcLabel,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,1,xcInit,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,2,xcLabel,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,2,xcInit,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,3,xcLabel,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,3,xcInit,xcNotOptional,xcAddQuote)

        DO LaunchCmd(.Options,.Results,.Modal)

        QUIT

 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
Combo(Text,width,height,List,x,y)
        ;"Purpose: To provide an easier access to Xdialog function
        ;"Input: Text -- text of input prompt to display
        ;"       width,height of dialog -- [optional]
        ;"       List -- Best IF passed by reference.  Holds list of options to be displayed as follows:
        ;"         List(1)=<Selection Option>
        ;"         List(2)=<Selection Option>
        ;"         List(3)=<Selection Option>
        ;"         ... etc up to any number N
        ;"       x,y -- the display location of the dialog [optional]
        ;"Output: (none)
        ;"Results: Returns text of selected option.
        ;"Notes: (none)

        NEW Options
        NEW Results
        SET result=""
        NEW i,Done
        NEW status,help

        IF $DATA(xcCommon)=0 DO SetupConsts()

        SET Options(xcBox,xcText)=$GET(Text)

        SET Done=0
        for i=1:1 DO  QUIT:Done
        . IF $DATA(List(i))=0 SET Done=1 QUIT
        . SET Options(xcBox,xcItem,i)=$GET(List(i))

        SET Options(xcBox,xcHeight)=$GET(height,0)
        SET Options(xcBox,xcWidth)=$GET(width,0)
        IF $DATA(x) SET Options(xcTransient,xcBegin)=x_" "_$GET(y,0)

        DO xcombo(.Options,.Results,xcModalMode)

        SET result=$GET(Results(xcDlgOutput,""))
        QUIT result


xcombo(Options,Results,Modal)
 ;" --combobox    <text> <height> <width> <item1> ... <itemN>
        NEW Added,GroupAdded
        NEW N

        DO ParamTextAdd(.Options,vDialog)
        DO SetCommons(.Options)
        DO SetTrans(.Options)
        DO ParamTextAdd(.Options," --combobox ")

        SET Added=$$AddParam(.Options,,xcText,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,,xcHeight)
        SET Added=$$AddParam(.Options,,xcWidth)
        SET N=1
xcl1    IF $DATA(Options(xcBox,xcItem,N))=0 GOTO xcl2
        SET GroupAdded=$$AddParam(.Options,N,xcItem,xcNotOptional,xcAddQuote)
        IF GroupAdded=0 GOTO xcl2
        SET N=N+1 GOTO xcl1
xcl2
        DO LaunchCmd(.Options,.Results,.Modal)

        QUIT


 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
Range(Text,width,height,min,max,init,x,y)
        ;"Purpose: To provide an easier access to Xdialog function
        ;"      A range dialog presents a horizontal slider bar to user
        ;"Input: Text -- text of input prompt to display
        ;"       width,height of dialog -- [optional]
        ;"       min -- the minimum possible range of input value (default = 0)
        ;"       max -- the minimum possible range of input value (default = 100)
        ;"       init -- the initial input value -- (default = 50)
        ;"       x,y -- the display location of the dialog [optional]
        ;"Output: (none)
        ;"Results: Returns input value
        ;"Notes: (none)

        NEW Options
        NEW Results,result

        IF $DATA(xcCommon)=0 DO SetupConsts()

        SET Options(xcBox,xcText)=$GET(Text)
        SET Options(xcBox,xcHeight)=$GET(height,0)
        SET Options(xcBox,xcWidth)=$GET(width,0)
        SET Options(xcBox,xcMin,1)=$GET(min,0)
        SET Options(xcBox,xcMax,1)=$GET(max,100)
        SET Options(xcBox,xcDefault,1)=$GET(init,50)
        IF $DATA(x) SET Options(xcTransient,xcBegin)=x_" "_$GET(y,0)

        DO xrange(.Options,.Results,xcModalMode)

        SET result=$GET(Results(xcDlgOutput,""))

        QUIT result


xrange(Options,Results,Modal)
 ;" --rangebox    <text> <height> <width> <min value> <max value> [<default value>]
        NEW Added

        DO ParamTextAdd(.Options,vDialog)
        DO SetCommons(.Options)
        DO SetTrans(.Options)
        DO ParamTextAdd(.Options," --rangebox ")

        SET Added=$$AddParam(.Options,,xcText,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,,xcHeight)
        SET Added=$$AddParam(.Options,,xcWidth)
        SET Added=$$AddParam(.Options,1,xcMin,xcNotOptional)
        SET Added=$$AddParam(.Options,1,xcMax,xcNotOptional)
        SET Added=$$AddParam(.Options,1,xcDefault,xcOptional)

        DO LaunchCmd(.Options,.Results,.Modal)

        QUIT


 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
Range2(Text,width,height,label1,min1,max1,init1,label2,min2,max2,init2,Result2,x,y)
        ;"Purpose: To provide an easier access to Xdialog function
        ;"Input: Text -- text of input prompt to display
        ;"       width,height of dialog -- [optional]
        ;"       label1 -- the label to show for range
        ;"       min1 -- the minimum possible range of input value (default = 0)
        ;"       max1 -- the minimum possible range of input value (default = 100)
        ;"       init1 -- the initial input value -- (default = 50)
        ;"       label2 -- the label to show for range
        ;"       min2 -- the minimum possible range of input value (default = 0)
        ;"       max2 -- the minimum possible range of input value (default = 100)
        ;"       init2 -- the initial input value -- (default = 50)
        ;"       Result2 -- a variable to put second input into for return. PASS BY REFERENCE
        ;"       x,y -- the display location of the dialog [optional]
        ;"Output: (none)
        ;"Results: returns result of 1st user-input.  result of 2nd user-input put into Result2
        ;"Notes: (none)

        NEW Options
        NEW Results,result

        IF $DATA(xcCommon)=0 DO SetupConsts()

        SET Options(xcCommon,xcSeparator)="^"
        SET Options(xcBox,xcText)=$GET(Text)
        SET Options(xcBox,xcHeight)=$GET(height,0)
        SET Options(xcBox,xcWidth)=$GET(width,0)
        SET Options(xcBox,xcLabel,1)=$GET(label1,"")
        SET Options(xcBox,xcMin,1)=$GET(min1,0)
        SET Options(xcBox,xcMax,1)=$GET(max1,100)
        SET Options(xcBox,xcMax,1)=$GET(max1,100)
        SET Options(xcBox,xcDefault,1)=$GET(init1,50)
        SET Options(xcBox,xcLabel,2)=$GET(label2,"")
        SET Options(xcBox,xcMin,2)=$GET(min2,0)
        SET Options(xcBox,xcMax,2)=$GET(max2,100)
        SET Options(xcBox,xcMax,2)=$GET(max2,100)
        SET Options(xcBox,xcDefault,2)=$GET(init2,50)
        IF $DATA(x) SET Options(xcTransient,xcBegin)=x_" "_$GET(y,0)

        DO x2range(.Options,.Results,xcModalMode)

        SET result=$GET(Results(xcDlgOutput,""))
        SET Result2=$GET(Results(xcDlgOutput,1))

        QUIT result


x2range(Options,Results,Modal)
 ;" --2rangesbox  <text> <height> <width> <label1> <min1> <max1> <def1> <label2> <min2> <max2> <def2>
        NEW Added

        DO ParamTextAdd(.Options,vDialog)
        DO SetCommons(.Options)
        DO SetTrans(.Options)
        DO ParamTextAdd(.Options," --2rangesbox ")

        SET Added=$$AddParam(.Options,,xcText,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,,xcHeight)
        SET Added=$$AddParam(.Options,,xcWidth)
        SET Added=$$AddParam(.Options,1,xcLabel,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,1,xcMin,xcNotOptional)
        SET Added=$$AddParam(.Options,1,xcMax,xcNotOptional)
        SET Added=$$AddParam(.Options,1,xcDefault,xcNotOptional)
        SET Added=$$AddParam(.Options,2,xcLabel,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,2,xcMin,xcNotOptional)
        SET Added=$$AddParam(.Options,2,xcMax,xcNotOptional)
        SET Added=$$AddParam(.Options,2,xcDefault,xcNotOptional)

        DO LaunchCmd(.Options,.Results,.Modal)

        QUIT

 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
Range3(Text,width,height,label1,min1,max1,init1,label2,min2,max2,init2,label3,min3,max3,init3,Result2,Result3,x,y)
        ;"Purpose: To provide an easier access to Xdialog function
        ;"Input: Text -- text of input prompt to display
        ;"       width,height of dialog -- [optional]
        ;"       labelN -- the title to show for the range.
        ;"       minN -- the minimum possible range of input value (default = 0)
        ;"       maxN -- the minimum possible range of input value (default = 100)
        ;"       initN -- the initial input value -- (default = 50)
        ;"       Result2 -- a variable to put second input into for return. PASS BY REFERENCE
        ;"       Result3 -- a variable to put third input into for return. PASS BY REFERENCE
        ;"       x,y -- the display location of the dialog [optional]
        ;"Output: (none)
        ;"Results: returns result of 1st user-input.
        ;"      result of 2nd user-input put into Result2
        ;"      result of 3rd user-input put into Result3
        ;"Notes: (none)

        NEW Options
        NEW Results,result

        IF $DATA(xcCommon)=0 DO SetupConsts()

        SET Options(xcCommon,xcSeparator)="^"
        SET Options(xcBox,xcText)=$GET(Text)
        SET Options(xcBox,xcHeight)=$GET(height,0)
        SET Options(xcBox,xcWidth)=$GET(width,0)
        SET Options(xcBox,xcLabel,1)=$GET(label1,"")
        SET Options(xcBox,xcMin,1)=$GET(min1,0)
        SET Options(xcBox,xcMax,1)=$GET(max1,100)
        SET Options(xcBox,xcMax,1)=$GET(max1,100)
        SET Options(xcBox,xcDefault,1)=$GET(init1,50)
        SET Options(xcBox,xcLabel,2)=$GET(label2,"")
        SET Options(xcBox,xcMin,2)=$GET(min2,0)
        SET Options(xcBox,xcMax,2)=$GET(max2,100)
        SET Options(xcBox,xcMax,2)=$GET(max2,100)
        SET Options(xcBox,xcDefault,2)=$GET(init2,50)
        SET Options(xcBox,xcLabel,3)=$GET(label3,"")
        SET Options(xcBox,xcMin,3)=$GET(min3,0)
        SET Options(xcBox,xcMax,3)=$GET(max3,100)
        SET Options(xcBox,xcMax,3)=$GET(max3,100)
        SET Options(xcBox,xcDefault,3)=$GET(init3,50)
        IF $DATA(x) SET Options(xcTransient,xcBegin)=x_" "_$GET(y,0)

        DO x3range(.Options,.Results,xcModalMode)

        SET result=$GET(Results(xcDlgOutput,""))
        SET Result2=$GET(Results(xcDlgOutput,1))
        SET Result3=$GET(Results(xcDlgOutput,2))

        QUIT result


x3range(Options,Results,Modal)
 ;" --3rangesbox  <text> <height> <width> <label1> <min1> <max1> <def1> <label2> <min2> <max2> <def2> <label3> <min3> <max3> <def3>
        NEW Added

        DO ParamTextAdd(.Options,vDialog)
        DO SetCommons(.Options)
        DO SetTrans(.Options)
        DO ParamTextAdd(.Options," --3rangesbox ")

        SET Added=$$AddParam(.Options,,xcText,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,,xcHeight)
        SET Added=$$AddParam(.Options,,xcWidth)
        SET Added=$$AddParam(.Options,1,xcLabel,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,1,xcMin,xcNotOptional)
        SET Added=$$AddParam(.Options,1,xcMax,xcNotOptional)
        SET Added=$$AddParam(.Options,1,xcDefault,xcNotOptional)
        SET Added=$$AddParam(.Options,2,xcLabel,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,2,xcMin,xcNotOptional)
        SET Added=$$AddParam(.Options,2,xcMax,xcNotOptional)
        SET Added=$$AddParam(.Options,2,xcDefault,xcNotOptional)
        SET Added=$$AddParam(.Options,3,xcLabel,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,3,xcMin,xcNotOptional)
        SET Added=$$AddParam(.Options,3,xcMax,xcNotOptional)
        SET Added=$$AddParam(.Options,3,xcDefault,xcNotOptional)

        DO LaunchCmd(.Options,.Results,.Modal)

        QUIT


 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
Spin(Text,width,height,label,min,max,init,Result,x,y)
        ;"Purpose: To provide an easier access to Xdialog function
        ;"      A spinner is a dialable number input dialog.
        ;"Input: Text -- text of input prompt to display
        ;"       width,height of dialog -- [optional]
        ;"       min -- the minimum possible range of input value (default = 0)
        ;"       max -- the minimum possible range of input value (default = 100)
        ;"       init -- the initial input value -- (default = 50)
        ;"       x,y -- the display location of the dialog [optional]
        ;"Output: The user input value is return in Result
        ;"Results: returns results of box closure.
        ;"Notes: (none)

        NEW Options
        NEW Results,result

        IF $DATA(xcCommon)=0 DO SetupConsts()

        SET Options(xcBox,xcText)=$GET(Text)
        SET Options(xcBox,xcHeight)=$GET(height,0)
        SET Options(xcBox,xcWidth)=$GET(width,0)
        SET Options(xcBox,xcMin,1)=$GET(min,0)
        SET Options(xcBox,xcMax,1)=$GET(max,100)
        SET Options(xcBox,xcMax,1)=$GET(max,100)
        SET Options(xcBox,xcLabel,1)=$GET(label,"")
        SET Options(xcBox,xcDefault,1)=$GET(init,50)
        IF $DATA(x) SET Options(xcTransient,xcBegin)=x_" "_$GET(y,0)

        DO xspin(.Options,.Results,xcModalMode)
        SET result=Results(xcDlgResult)

        SET Result=$GET(Results(xcDlgOutput,""))

        QUIT result


xspin(Options,Results,Modal)
 ;" --spinbox     <text> <height> <width> <min> <max> <def> <label>
        NEW Added

        DO ParamTextAdd(.Options,vDialog)
        DO SetCommons(.Options)
        DO SetTrans(.Options)
        DO ParamTextAdd(.Options," --spinbox ")

        SET Added=$$AddParam(.Options,,xcText,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,,xcHeight)
        SET Added=$$AddParam(.Options,,xcWidth)
        SET Added=$$AddParam(.Options,1,xcMin,xcNotOptional)
        SET Added=$$AddParam(.Options,1,xcMax,xcNotOptional)
        SET Added=$$AddParam(.Options,1,xcDefault,xcOptional)
        SET Added=$$AddParam(.Options,1,xcLabel,xcNotOptional,xcAddQuote)

        DO LaunchCmd(.Options,.Results,.Modal)

        QUIT


 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
Spin2(Text,width,height,label1,min1,max1,init1,label2,min2,max2,init2,Result1,Result2,x,y)
        ;"Purpose: To provide an easier access to Xdialog function
        ;"Input: Text -- text of input prompt to display
        ;"       width,height of dialog -- [optional]
        ;"       label1 -- the label to show for range
        ;"       min1 -- the minimum possible range of input value (default = 0)
        ;"       max1 -- the minimum possible range of input value (default = 100)
        ;"       init1 -- the initial input value -- (default = 50)
        ;"       label2 -- the label to show for range
        ;"       min2 -- the minimum possible range of input value (default = 0)
        ;"       max2 -- the minimum possible range of input value (default = 100)
        ;"       init2 -- the initial input value -- (default = 50)
        ;"       Result1 -- a variable to put first input into for return. PASS BY REFERENCE
        ;"       Result2 -- a variable to put second input into for return. PASS BY REFERENCE
        ;"       x,y -- the display location of the dialog [optional]
        ;"Output: The user input value is return in Result1
        ;"        result of 2nd user-input put into Result2
        ;"Results: returns results of box closure.
        ;"Notes: (none)
        NEW Options
        NEW Results,result

        IF $DATA(xcCommon)=0 DO SetupConsts()

        SET Options(xcCommon,xcSeparator)="^"
        SET Options(xcBox,xcText)=$GET(Text)
        SET Options(xcBox,xcHeight)=$GET(height,0)
        SET Options(xcBox,xcWidth)=$GET(width,0)
        SET Options(xcBox,xcLabel,1)=$GET(label1,"")
        SET Options(xcBox,xcMin,1)=$GET(min1,0)
        SET Options(xcBox,xcMax,1)=$GET(max1,100)
        SET Options(xcBox,xcMax,1)=$GET(max1,100)
        SET Options(xcBox,xcDefault,1)=$GET(init1,50)
        SET Options(xcBox,xcLabel,2)=$GET(label2,"")
        SET Options(xcBox,xcMin,2)=$GET(min2,0)
        SET Options(xcBox,xcMax,2)=$GET(max2,100)
        SET Options(xcBox,xcMax,2)=$GET(max2,100)
        SET Options(xcBox,xcDefault,2)=$GET(init2,50)
        IF $DATA(x) SET Options(xcTransient,xcBegin)=x_" "_$GET(y,0)

        DO x2spin(.Options,.Results,xcModalMode)
        SET result=Results(xcDlgResult)

        SET Result1=$GET(Results(xcDlgOutput,""))
        SET Result2=$GET(Results(xcDlgOutput,1))

        QUIT result


x2spin(Options,Results,Modal)
 ;" --2spinsbox   <text> <height> <width> <min1> <max1> <def1> <label1> <min2> <max2> <def2> <label2>
        NEW Added

        DO ParamTextAdd(.Options,vDialog)
        DO SetCommons(.Options)
        DO SetTrans(.Options)

        SET Added=$$AddParam(.Options,,xcText,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,,xcHeight)
        SET Added=$$AddParam(.Options,,xcWidth)
        SET Added=$$AddParam(.Options,1,xcMin,xcNotOptional)
        SET Added=$$AddParam(.Options,1,xcMax,xcNotOptional)
        SET Added=$$AddParam(.Options,1,xcDefault,xcNotOptional)
        SET Added=$$AddParam(.Options,1,xcLabel,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,2,xcMin,xcNotOptional)
        SET Added=$$AddParam(.Options,2,xcMax,xcNotOptional)
        SET Added=$$AddParam(.Options,2,xcDefault,xcNotOptional)
        SET Added=$$AddParam(.Options,2,xcLabel,xcNotOptional,xcAddQuote)

        DO LaunchCmd(.Options,.Results,.Modal)

        QUIT


 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
Spin3(Text,width,height,label1,min1,max1,init1,label2,min2,max2,init2,label3,min3,max3,init3,Result1,Result2,Result3,x,y)
        ;"Purpose: To provide an easier access to Xdialog function
        ;"Input: Text -- text of input prompt to display
        ;"       width,height of dialog -- [optional]
        ;"       labelN -- the title to show for the range.
        ;"       minN -- the minimum possible range of input value (default = 0)
        ;"       maxN -- the minimum possible range of input value (default = 100)
        ;"       initN -- the initial input value -- (default = 50)
        ;"       Result1 -- a variable to put first input into for return. PASS BY REFERENCE
        ;"       Result2 -- a variable to put second input into for return. PASS BY REFERENCE
        ;"       Result3 -- a variable to put third input into for return. PASS BY REFERENCE
        ;"       x,y -- the display location of the dialog [optional]
        ;"Output: The user input value is return in Result1
        ;"      result of 2nd user-input put into Result2
        ;"      result of 3rd user-input put into Result3
        ;"Results: returns results of box closure.
        ;"Notes: (none)

        NEW Options
        NEW Results,result

        IF $DATA(xcCommon)=0 DO SetupConsts()

        SET Options(xcCommon,xcSeparator)="^"
        SET Options(xcBox,xcText)=$GET(Text)
        SET Options(xcBox,xcHeight)=$GET(height,0)
        SET Options(xcBox,xcWidth)=$GET(width,0)
        SET Options(xcBox,xcLabel,1)=$GET(label1,"")
        SET Options(xcBox,xcMin,1)=$GET(min1,0)
        SET Options(xcBox,xcMax,1)=$GET(max1,100)
        SET Options(xcBox,xcMax,1)=$GET(max1,100)
        SET Options(xcBox,xcDefault,1)=$GET(init1,50)
        SET Options(xcBox,xcLabel,2)=$GET(label2,"")
        SET Options(xcBox,xcMin,2)=$GET(min2,0)
        SET Options(xcBox,xcMax,2)=$GET(max2,100)
        SET Options(xcBox,xcMax,2)=$GET(max2,100)
        SET Options(xcBox,xcDefault,2)=$GET(init2,50)
        SET Options(xcBox,xcLabel,3)=$GET(label3,"")
        SET Options(xcBox,xcMin,3)=$GET(min3,0)
        SET Options(xcBox,xcMax,3)=$GET(max3,100)
        SET Options(xcBox,xcMax,3)=$GET(max3,100)
        SET Options(xcBox,xcDefault,3)=$GET(init3,50)
        IF $DATA(x) SET Options(xcTransient,xcBegin)=x_" "_$GET(y,0)

        DO x3spin(.Options,.Results,xcModalMode)
        SET result=Results(xcDlgResult)

        SET Result1=$GET(Results(xcDlgOutput,""))
        SET Result2=$GET(Results(xcDlgOutput,1))
        SET Result3=$GET(Results(xcDlgOutput,2))

        QUIT result


x3spin(Options,Results,Modal)
 ;" --3spinsbox   <text> <height> <width> <text> <height> <width> <min1> <max1> <def1> <label1> <min2> <max2> <def2> <label2> <min3> <max3> <def3> <label3>
        NEW Added

        DO ParamTextAdd(.Options,vDialog)
        DO SetCommons(.Options)
        DO SetTrans(.Options)
        DO ParamTextAdd(.Options," --2spinsbox ")

        SET Added=$$AddParam(.Options,,xcText,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,,xcHeight)
        SET Added=$$AddParam(.Options,,xcWidth)
        SET Added=$$AddParam(.Options,1,xcMin,xcNotOptional)
        SET Added=$$AddParam(.Options,1,xcMax,xcNotOptional)
        SET Added=$$AddParam(.Options,1,xcDefault,xcNotOptional)
        SET Added=$$AddParam(.Options,1,xcLabel,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,2,xcMin,xcNotOptional)
        SET Added=$$AddParam(.Options,2,xcMax,xcNotOptional)
        SET Added=$$AddParam(.Options,2,xcDefault,xcNotOptional)
        SET Added=$$AddParam(.Options,2,xcLabel,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,3,xcMin,xcNotOptional)
        SET Added=$$AddParam(.Options,3,xcMax,xcNotOptional)
        SET Added=$$AddParam(.Options,3,xcDefault,xcNotOptional)
        SET Added=$$AddParam(.Options,3,xcLabel,xcNotOptional,xcAddQuote)

        DO LaunchCmd(.Options,.Results,.Modal)

        QUIT



 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

Log(file,width,height,Modal,x,y)
        ;"Purpose: To provide an easier access to Xdialog function
        ;"Input: file to display
        ;"       height & width of dialog -- [optional]
        ;"       [Modal]: IF true, function does not return until dialog is closed.
        ;"              IF false, function returns immediately, and functions DO NOT
        ;"              reflect the user's button press. OPTIONAL -- default=xcNonModal
        ;"       x,y -- the display location of the dialog [optional]
        ;"Output: (none)
        ;"Results: Returns results of box closure (see Modal note above)
        ;"Notes: (none)

        NEW Options
        NEW Results,result

        IF $DATA(xcCommon)=0 DO SetupConsts()

        SET Options(xcBox,xcFile)=$GET(file)
        SET Options(xcBox,xcHeight)=$GET(height,0)
        SET Options(xcBox,xcWidth)=$GET(width,0)
        SET Modal=$GET(Modal,xcNonModal)
        IF $DATA(x) SET Options(xcTransient,xcBegin)=x_" "_$GET(y,0)

        DO xlog(.Options,.Results,Modal)
        SET result=Results(xcDlgResult)

        QUIT result


xlog(Options,Results,Modal)
 ;" --logbox      <file> <height> <width>
        NEW Added

        DO ParamTextAdd(.Options,vDialog)
        DO SetCommons(.Options)
        DO SetTrans(.Options)
        DO ParamTextAdd(.Options," --logbox ")


        SET Added=$$AddParam(.Options,,xcFile,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,,xcHeight)
        SET Added=$$AddParam(.Options,,xcWidth)

        DO LaunchCmd(.Options,.Results,.Modal)

        QUIT

 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

Edit(file,width,height,Results,x,y)
        ;"Purpose: To provide an easier access to Xdialog function
        ;"Input: file to display for editing,
        ;"       height & width of dialog -- [optional]
        ;"       Results -- the array to put results into. MUST BE PASSED BY REFERENCE.
        ;"       x,y -- the display location of the dialog [optional]
        ;"Output: The modified text is put into Results
        ;"      Example of returned results after editing a script file.:
        ;"      Results("Dialog Output",1)="<!DOCTYPE INSTALL_SCRIPT>"
        ;"      Results("Dialog Output",2)="<INSTALL_SCRIPT>"
        ;"      Results("Dialog Output",3)="<Script>"
        ;"      Results("Dialog Output",4)="  <Show>This is a test script system.</Show>"
        ;"      Results("Dialog Output",5)="</Script>"
        ;"Results: Returns results of box closure (see Modal note above)
        ;"Notes: If dialog is not closed with an OK, then changes are NOT returned in Results

        NEW Options
        NEW Results,result

        IF $DATA(xcCommon)=0 DO SetupConsts()

        SET Options(xcBox,xcFile)=$GET(file)
        SET Options(xcBox,xcHeight)=$GET(height,0)
        SET Options(xcBox,xcWidth)=$GET(width,0)
        IF $DATA(x) SET Options(xcTransient,xcBegin)=x_" "_$GET(y,0)

        DO xedit(.Options,.Results,xcModalMode)
        SET result=Results(xcDlgResult)

        QUIT result


xedit(Options,Results,Modal)
 ;" --editbox     <file> <height> <width>
        NEW Added

        DO ParamTextAdd(.Options,vDialog)
        DO SetCommons(.Options)
        DO SetTrans(.Options)
        DO ParamTextAdd(.Options," --editbox ")

        SET Added=$$AddParam(.Options,,xcFile,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,,xcHeight)
        SET Added=$$AddParam(.Options,,xcWidth)

        DO LaunchCmd(.Options,.Results,.Modal)

        QUIT

 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

Text(file,width,height,Modal,x,y)
        ;"Purpose: To provide an easier access to Xdialog function
        ;"Input: file to display
        ;"       height & width of dialog -- [optional]
        ;"       [Modal]: IF true, function does not return until dialog is closed.
        ;"              IF false, function returns immediately, and functions DO NOT
        ;"              reflect the user's button press. OPTIONAL -- default=xcNonModal
        ;"       x,y -- the display location of the dialog [optional]
        ;"Output: (none)
        ;"Results: Returns results of box closure (see Modal note above)
        ;"Notes: (none)

        NEW Options
        NEW Results,result

        IF $DATA(xcCommon)=0 DO SetupConsts()

        SET Options(xcBox,xcFile)=$GET(file)
        SET Options(xcBox,xcHeight)=$GET(height,0)
        SET Options(xcBox,xcWidth)=$GET(width,0)
        SET Modal=$GET(Modal,xcNonModal)
        IF $DATA(x) SET Options(xcTransient,xcBegin)=x_" "_$GET(y,0)

        DO xtext(.Options,.Results,Modal)
        SET result=Results(xcDlgResult)

        QUIT result


xtext(Options,Results,Modal)
 ;" --textbox     <file> <height> <width>
        NEW Added

        DO ParamTextAdd(.Options,vDialog)
        DO SetCommons(.Options)
        DO SetTrans(.Options)
        DO ParamTextAdd(.Options," --textbox ")

        SET Added=$$AddParam(.Options,,xcFile,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,,xcHeight)
        SET Added=$$AddParam(.Options,,xcWidth)

        DO LaunchCmd(.Options,.Results,.Modal)

        QUIT

 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

Tail(file,width,height,Modal,x,y)
        ;"Purpose: To provide an easier access to Xdialog function
        ;"         A tailbox is one that keeps at the bottom, updating as the file is updated.
        ;"Input: file to display
        ;"       height & width of dialog -- [optional]
        ;"       [Modal]: IF true, function does not return until dialog is closed.
        ;"              IF false, function returns immediately, and functions DO NOT
        ;"              reflect the user's button press. OPTIONAL -- default=xcNonModal
        ;"       x,y -- the display location of the dialog [optional]
        ;"Output: (none)
        ;"Results: Returns results of box closure (see Modal note above)
        ;"Notes: (none)

        NEW Options
        NEW Results,result

        IF $DATA(xcCommon)=0 DO SetupConsts()

        SET Options(xcBox,xcFile)=$GET(file)
        SET Options(xcBox,xcHeight)=$GET(height,0)
        SET Options(xcBox,xcWidth)=$GET(width,0)
        SET Modal=$GET(Modal,xcNonModal)
        IF $DATA(x) SET Options(xcTransient,xcBegin)=x_" "_$GET(y,0)

        DO xtail(.Options,.Results,Modal)
        SET result=Results(xcDlgResult)

        QUIT result


xtail(Options,Results,Modal)
 ;" --tailbox     <file> <height> <width>
        NEW Added

        DO ParamTextAdd(.Options,vDialog)
        DO SetCommons(.Options)
        DO SetTrans(.Options)
        DO ParamTextAdd(.Options," --tailbox ")

        SET Added=$$AddParam(.Options,,xcFile,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,,xcHeight)
        SET Added=$$AddParam(.Options,,xcWidth)

        DO LaunchCmd(.Options,.Results,.Modal)

        QUIT

 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
 ;"TO BE COMPLETED

xchecklist(Options,Results,Modal)
 ;" --checklist   <text> <height> <width> <list height> <tag1> <item1> <status1> {<help1>}...

 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

RadioList(Text,List,width,height,x,y)
        ;"Purpose: To provide an easier access to Xdialog function
        ;"         A tailbox is one that keeps at the bottom, updating as the file is updated.
        ;"Input: Text -- title text.
        ;"       List -- Best IF passed by reference.  Holds radio list as follows:
        ;"         List(1,xcTag)=<return value>  -- the output the be returned IF selected.
        ;"         List(1,xcItem)=<text of radio item>
        ;"         List(1,xcStatus)=<status> must be: {"on", "off", or "unavailable"}
        ;"         List(1,xcHelp)=<hover tip> -- [optional]
        ;"         List(2,xcTag)=<return value>  -- the output the be returned IF selected.
        ;"         List(2,xcItem)=<text of radio item>
        ;"         List(2,xcStatus)=<status> must be: {"on", "off", or "unavailable"}
        ;"         List(2,xcHelp)=<hover tip> -- [optional]
        ;"         ... etc up to any number N
        ;"       height & width of dialog -- [optional]
        ;"       x,y -- the display location of the dialog [optional]
        ;"Output: (none)
        ;"Results: Returns selected 'tag'. If cancel pressed, then returns ""
        ;"Notes: (none)

        NEW Options
        NEW Results
        SET result=""
        NEW i,Done
        NEW status,help

        IF $DATA(xcCommon)=0 DO SetupConsts()

        SET Options(xcBox,xcText)=$GET(Text)

        SET Done=0
        for i=1:1 DO  QUIT:Done
        . IF $DATA(List(i,xcTag))=0 SET Done=1 QUIT
        . SET Options(xcBox,xcTag,i)=$GET(List(i,xcTag))
        . SET Options(xcBox,xcItem,i)=$GET(List(i,xcItem))
        . SET status=$GET(List(i,xcStatus))
        . IF (status'="on")&(status'="unavailable") SET status="off"
        . SET Options(xcBox,xcStatus,i)=status
        . SET help=$GET(List(i,xcHelp,i))
        . IF help'="" SET Options(xcTransient,xxcItemHelp)=1
        . SET help=($GET(Options(xcTransient,xxcItemHelp))=1)
        . IF help SET Options(xcBox,xcHelp,i)=$GET(List(i,xcHelp))

        SET Options(xcBox,xcHeight)=$GET(height,0)
        SET Options(xcBox,xcWidth)=$GET(width,0)
        IF $DATA(x) SET Options(xcTransient,xcBegin)=x_" "_$GET(y,0)

        DO xradiolist(.Options,.Results,xcModalMode)

        SET result=$GET(Results(xcDlgOutput,""))
        QUIT result


xradiolist(Options,Results,Modal)
 ;" --radiolist   <text> <height> <width> <list height> <tag1> <item1> <status1> {<help1>}...
        NEW Added,GroupAdded
        NEW N

        DO ParamTextAdd(.Options,vDialog)
        DO SetCommons(.Options)
        DO SetTrans(.Options)
        DO ParamTextAdd(.Options," --radiolist ")

        SET Added=$$AddParam(.Options,,xcText,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,,xcHeight)
        SET Added=$$AddParam(.Options,,xcWidth)
        SET Added=$$AddParam(.Options,,xcListHeight)
        SET N=1
xrl1    SET GroupAdded=$$AddParam(.Options,N,xcTag,xcNotOptional,xcAddQuote)
        IF GroupAdded=0 GOTO xrl2
        SET Added=$$AddParam(.Options,N,xcItem,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,N,xcStatus,xcNotOptional,xcAddQuote)
        IF (Added=1)&($GET(Option(xcTransient,xxcItemHelp))=1) do
        . SET Added=$$AddParam(.Options,N,xcHelp,xcNotOptional,xcAddQuote)
        SET N=N+1 GOTO xrl1
xrl2
        DO LaunchCmd(.Options,.Results,.Modal)

        QUIT


 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
 ;"TO BE COMPLETED

xmenu(Options,Results,Modal)
 ;" --menubox     <text> <height> <width> <menu height> <tag1> <item1> {<help1>}...

 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
 ;"TO BE COMPLETED

xtreeview(Options,Results,Modal)
 ;" --treeview    <text> <height> <width> <list height> <tag1> <item1> <status1> <item_depth1> {<help1>}...

 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

FileSel(Title,InitFile,width,height,x,y)
        ;"Purpose: To provide an easier access to Xdialog function
        ;"         A tailbox is one that keeps at the bottom, updating as the file is updated.
        ;"Input: InitFile.  The initial file to select, and the default file. [optional]
        ;"       width,height -- the initial size of box.  [Optional]
        ;"       x,y -- the display location of the dialog [optional]
        ;"Output:(none)
        ;"Results: returns the selected filename
        ;"Notes: (none)

        NEW Options
        NEW Results
        NEW result SET result=""

        IF $DATA(xcCommon)=0 DO SetupConsts()
        IF $DATA(Title) SET Options(xcCommon,xcTitle)=Title
        SET Options(xcBox,xcFile)=$GET(InitFile,"")
        SET Options(xcBox,xcHeight)=$GET(height,0)
        SET Options(xcBox,xcWidth)=$GET(width,0)
        IF $DATA(x) SET Options(xcTransient,xcBegin)=x_" "_$GET(y,0)

        DO xfilesel(.Options,.Results,xcModalMode)

        IF Results(xcDlgResult)=0 SET result=$GET(Results(xcDlgOutput,""))

        QUIT result

xfilesel(Options,Results,Modal)
 ;" --fselect     <file> <height> <width>
        NEW Added

        DO ParamTextAdd(.Options,vDialog)
        DO SetCommons(.Options)
        DO SetTrans(.Options)
        DO ParamTextAdd(.Options," --fselect ")

        SET Added=$$AddParam(.Options,,xcFile,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,,xcHeight)
        SET Added=$$AddParam(.Options,,xcWidth)

        DO LaunchCmd(.Options,.Results,.Modal)

        QUIT

 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

DirSel(Title,InitDir,width,height,x,y)
        ;"Purpose: To provide an easier access to Xdialog function
        ;"         A tailbox is one that keeps at the bottom, updating as the file is updated.
        ;"Input: InitDir:  The initial file to select, and the default file. [optional]
        ;"       width,height -- the initial size of box.  [Optional]
        ;"       x,y -- the display location of the dialog [optional]
        ;"Output:(none)
        ;"Results: returns the selected directory
        ;"Notes: (none)

        NEW Options
        NEW Results
        NEW result SET result=""

        IF $DATA(xcCommon)=0 DO SetupConsts()
        IF $DATA(Title) SET Options(xcCommon,xcTitle)=Title
        SET Options(xcBox,xcDirectory)=$GET(InitDir,"")
        SET Options(xcBox,xcHeight)=$GET(height,0)
        SET Options(xcBox,xcWidth)=$GET(width,0)
        IF $DATA(x) SET Options(xcTransient,xcBegin)=x_" "_$GET(y,0)

        DO xdirsel(.Options,.Results,xcModalMode)

        IF Results(xcDlgResult)=0 SET result=$GET(Results(xcDlgOutput,""))

        QUIT result


xdirsel(Options,Results,Modal)
 ;" --dselect     <directory> <height> <width>
        NEW Added

        DO ParamTextAdd(.Options,vDialog)
        DO SetCommons(.Options)
        DO SetTrans(.Options)
        DO ParamTextAdd(.Options," --dselect ")


        SET Added=$$AddParam(.Options,,xcDirectory,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,,xcHeight)
        SET Added=$$AddParam(.Options,,xcWidth)

        DO LaunchCmd(.Options,.Results,.Modal)

        QUIT

 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

DateSel(Text,width,height,InitDay,InitMonth,InitYear,x,y)
        ;"Purpose: To provide an easier access to Xdialog function
        ;"         Shows a calendar and allows user to select date.
        ;"Input: Text -- a title / msg to show.
        ;"       width,height -- the initial size of box.  [Optional]
        ;"       InitDay/Month/Year -- Initial date to show.
        ;"         NOTE: These three variables are optional BUT IF InitDay given all 3 should be present.
        ;"       x,y -- the display location of the dialog [optional]
        ;"Output:(none)
        ;"Results: returns the selected date
        ;"Notes: (none)

        NEW Options
        NEW Results
        NEW result SET result=""

        IF $DATA(xcCommon)=0 DO SetupConsts()

        SET Options(xcBox,xcText)=$GET(Text,"")
        SET Options(xcBox,xcHeight)=$GET(height,0)
        SET Options(xcBox,xcWidth)=$GET(width,0)
        IF $DATA(InitDay) do
        . SET Options(xcBox,xcDay)=InitDay
        . SET Options(xcBox,xcMonth)=$GET(InitMonth,0)
        . SET Options(xcBox,xcYear)=$GET(InitYear,0)
        IF $DATA(x) SET Options(xcTransient,xcBegin)=x_" "_$GET(y,0)

        DO xcalendarsel(.Options,.Results,xcModalMode)

        IF Results(xcDlgResult)=0 SET result=$GET(Results(xcDlgOutput,""))

        QUIT result

xcalendarsel(Options,Results,Modal)
 ;" --calendar    <text> <height> <width> [<day> <month> <year>]

        DO ParamTextAdd(.Options,vDialog)
        DO SetCommons(.Options)
        DO SetTrans(.Options)
        DO ParamTextAdd(.Options," --calendar ")

        SET Added=$$AddParam(.Options,,xcText,xcAddQuote)
        SET Added=$$AddParam(.Options,,xcHeight)
        SET Added=$$AddParam(.Options,,xcWidth)
        IF $DATA(Options(xcDay)) do
        . SET Added=$$AddParam(.Options,,xcDay,xcNotOptional)
        . SET Added=$$AddParam(.Options,,xcMonth,xcNotOptional)
        . SET Added=$$AddParam(.Options,,xcYear,xcNotOptional)

        DO LaunchCmd(.Options,.Results,.Modal)

        QUIT

 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

TimeSel(Text,width,height,InitHour,InitMinute,InitSecond,x,y)
        ;"Purpose: To provide an easier access to Xdialog function
        ;"         Shows a calendar and allows user to select date.
        ;"Input: Text -- a title / msg to show.
        ;"       width,height -- the initial size of box.  [Optional]
        ;"       InitHour/Minute/Second -- Initial time to show.
        ;"         NOTE: These three variables are optional BUT IF InitDay given all 3 should be present.
        ;"       x,y -- the display location of the dialog [optional]
        ;"Output:(none)
        ;"Results: returns the selected date
        ;"Notes: (none)

        NEW Options
        NEW Results
        NEW result SET result=""

        IF $DATA(xcCommon)=0 DO SetupConsts()

        SET Options(xcBox,xcText)=$GET(Text,"")
        SET Options(xcBox,xcHeight)=$GET(height,0)
        SET Options(xcBox,xcWidth)=$GET(width,0)
        IF $DATA(InitHour) do
        . SET Options(xcBox,xcHours)=InitDay
        . SET Options(xcBox,xcMinutes)=$GET(InitMinute,0)
        . SET Options(xcBox,xcSeconds)=$GET(InitSecond,0)
        IF $DATA(x) SET Options(xcTransient,xcBegin)=x_" "_$GET(y,0)

        DO xtimesel(.Options,.Results,xcModalMode)

        IF Results(xcDlgResult)=0 SET result=$GET(Results(xcDlgOutput,""))

        QUIT result

xtimesel(Options,Results,Modal)
 ;" --timebox     <text> <height> <width> [<hours> <minutes> <seconds>]

        DO ParamTextAdd(.Options,vDialog)
        DO SetCommons(.Options)
        DO SetTrans(.Options)
        DO ParamTextAdd(.Options," --timebox ")

        SET Added=$$AddParam(.Options,,xcText,xcAddQuote)
        SET Added=$$AddParam(.Options,,xcHeight)
        SET Added=$$AddParam(.Options,,xcWidth)
        IF $DATA(Options(xcHours)) do
        . SET Added=$$AddParam(.Options,,xcHours,xcNotOptional)
        . SET Added=$$AddParam(.Options,,xcMinutes,xcNotOptional)
        . SET Added=$$AddParam(.Options,,xcSeconds,xcNotOptional)

        DO LaunchCmd(.Options,.Results,.Modal)

        QUIT

 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
 ;"TO BE COMPLETED

xbuildlist(Options,Results,Modal)
 ;" --buildlist   <text> <height> <width> <list height> <tag1> <item1> <status1> {<help1>}...

 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
 ;"TO BE COMPLETED

xcolorsel(Options,Results,Modal)
 ;" --colorsel    <text> <height> <width>
        NEW Added

        DO ParamTextAdd(.Options,vDialog)
        DO SetCommons(.Options)
        DO SetTrans(.Options)
        DO ParamTextAdd(.Options," --colorsel ")

        SET Added=$$AddParam(.Options,,xcText,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,,xcHeight)
        SET Added=$$AddParam(.Options,,xcWidth)

        DO LaunchCmd(.Options,.Results,.Modal)

        QUIT

 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
FontSel(InitFont,width,height,x,y)
        ;"Purpose: To provide an easier access to Xdialog function
        ;"         Shows a font-pick
        ;"Input: InitFont -- name of initial font to show [Optional]
        ;"       width,height -- the initial size of box.  [Optional]
        ;"       x,y -- the display location of the dialog [optional]
        ;"Output:(none)
        ;"Results: returns the selected date
        ;"Notes: (none)

        NEW Options
        NEW Results
        NEW result SET result=""

        IF $DATA(xcCommon)=0 DO SetupConsts()

        SET Options(xcBox,xcFontName)=$GET(InitFont,"")
        SET Options(xcBox,xcHeight)=$GET(height,0)
        SET Options(xcBox,xcWidth)=$GET(width,0)
        IF $DATA(x) SET Options(xcTransient,xcBegin)=x_" "_$GET(y,0)

        DO xfontsel(.Options,.Results,xcModalMode)

        IF Results(xcDlgResult)=0 SET result=$GET(Results(xcDlgOutput,""))

        QUIT result


xfontsel(Options,Results,Modal)
 ;" --fontsel     <font name> <height> <width>
        NEW Added

        DO ParamTextAdd(.Options,vDialog)
        DO SetCommons(.Options)
        DO SetTrans(.Options)
        DO ParamTextAdd(.Options," --fontsel ")

        SET Added=$$AddParam(.Options,,xcFontName,xcNotOptional,xcAddQuote)
        SET Added=$$AddParam(.Options,,xcHeight)
        SET Added=$$AddParam(.Options,,xcWidth)

        DO LaunchCmd(.Options,.Results,.Modal)

        QUIT

 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
ChClrScr()
        ;"Purpose: When working with text menus, after the dialog exits,
        ;"         it leaves the drawing of the menu on the text screen.
        ;"         So I'll have a function that clears the screen.
        ;"Note:    I can't depend on the VistA system to have SET up
        ;"         variables that will clear the screen.  So I'll DO it quick and dirty
        ;"         by many newline characters.

        NEW count

        for count=1:1:50 WRITE !

        QUIT

 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

SetCommons(Options)
        ;"Purpose: to put common options into a parameter string that will be sent to Xdialog
        ;"Input: Options -- MUST BE PASSED BY REFERENCE
        ;"       See docs re. Options above.
        ;"Output: The Options array will contain an entry containing output string:
        ;"        Options(xcCmdLine)=<composite options>

        NEW i
        NEW s SET s=" "
        NEW AddQuote SET AddQuote=0

        IF $DATA(xcCommon)=0 DO SetupConsts()  ;"Ensure constants created.


        SET i=$ORDER(Options(xcCommon,""))
        FOR  DO  q:i=""
        . IF i=xcCmdLine QUIT
        . IF (i'=xcCmdLine)&($DATA(Options(xcCommon,i))'=0) do
        . . SET s=s_"--"_i_" "
        . . IF $GET(Options(xcCommon,i))'=1 do
        . . . SET s=s_""""_Options(xcCommon,i)_""" "
        . SET i=$ORDER(Options(xcCommon,i))

        ;"set Options(xcCmdLine)=s
        DO ParamTextAdd(.Options,s)

        QUIT


SetTrans(Options)
        ;"Purpose: to put transient options into a parameter string that will be sent to Xdialog
        ;"Input: Options -- MUST BE PASSED BY REFERENCE
        ;"       See docs re. Options above.
        ;"Output: The Options array will contain an entry containing output string:
        ;"        Options(xcCmdLine)=<composite options>
        ;"Note: This function should be called AFTER SetCommons()

        NEW i
        ;"new s SET s=$GET(Options(xcCmdLine))
        NEW s SET s=" "

        SET i=$ORDER(Options(xcTransient,""))
        FOR  DO  q:i=""
        . IF i=xcCmdLine QUIT
        . IF (i'=xcCmdLine)&($DATA(Options(xcTransient,i))'=0) do
        . . SET s=s_"--"_i_" "
        . . IF $GET(Options(xcTransient,i))'=1 SET s=s_Options(xcTransient,i)_" "
        . SET i=$ORDER(Options(xcTransient,i))

        ;"set Options(xcCmdLine)=$GET(Options(xcCmdLine))_s
        DO ParamTextAdd(.Options,s)

        QUIT

 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

AddParam(Options,N,index,optional,AddQuote)
        ;"Purpose: to add index'd box parameter to the composite parameters
        ;"Input: Options -- see above. MUST BE PASSED BY REFERENCE
        ;"       N -- should NOT be passed, unless index item has a 'subscript', i.e.:
        ;"              Options(xcBox,xcItem,1)="Bill"
        ;"              Options(xcBox,xcItem,2)="Bill"
        ;"              Options(xcBox,xcItem,3)="Bill"
        ;"       index -- specifies which parameter to add (if found)
        ;"       optional -- specifies IF parameter is optional
        ;"                    default=not optional (0).  Value of 1=is optional
        ;"       AddQuote -- IF parameter should be in quotes -- default is 0 / no
        ;"results: returns IF data was added. (1=added, 0=not added)

        NEW result SET result=0
        NEW s,sCurrent
        NEW CurLine
        NEW Param

        SET optional=$GET(optional,xcNotOptional)
        IF optional'=xcNotOptional SET optional=xcOptional
        SET AddQuote=$GET(AddQuote,xcNoQuote)

        ;"WRITE "Starting AddParam",!

        SET s=""

        IF $DATA(N) DO  ;"i.e. user is looking for a subscripted element...
        . SET Param=$GET(Options(xcBox,index,N))
        ELSE  DO        ;"i.e. user is NOT looking for a subscripted element...
        . SET Param=$GET(Options(xcBox,index))

        IF Param'="" DO  ;"Parameter found.
        . IF AddQuote SET s=s_""""
        . SET s=s_Param
        . SET result=1
        ELSE  DO   ;"There has not been any parameter found.
        . IF $DATA(N) QUIT   ;"If user was looking for (absent) subscripted param, then ignore NotOptional
        . IF (optional=xcNotOptional) do
        . . IF AddQuote SET s=s_""""
        . . SET s=s_"0"  ;"put in a 0 for non-optional values.
        . . SET result=1

        IF result=1 do
        . IF (AddQuote=xcAddQuote) SET s=s_""" "
        . ELSE  SET s=s_" "

        DO ParamTextAdd(.Options,s)


        QUIT result

ParamTextAdd(Options,Text)
        ;"Purpose: to actually add the text of the NEW parameter etc
        ;"         into the Options variable
        ;"Input: Options .. same as variable used everywhere else
        ;"              MUST BE PASSED BY REFERENCE
        ;"      Text -- the text to add

        NEW sCurrent
        NEW CurLine

        ;"First the simple way -- with max of ~230 characters
        SET Options(xcCmdLine)=$GET(Options(xcCmdLine))_Text

        ;"Next, array method, with unlimited length.
        SET CurLine=$GET(Options(xcCmdLine,xcCmdArray,xcCmdMaxLine),0)
        SET sCurrent=$GET(Options(xcCmdLine,xcCmdArray,CurLine))
        IF $LENGTH(sCurrent)>80 do
        . SET CurLine=CurLine+1
        . SET sCurrent=""

        SET sCurrent=sCurrent_Text
        ;"WRITE "After additions, sCurrent=",sCurrent,!
        SET Options(xcCmdLine,xcCmdArray,CurLine)=sCurrent
        SET Options(xcCmdLine,xcCmdArray,xcCmdMaxLine)=CurLine
        QUIT


LaunchCmd(Options,Results,Modal)
        ;"Purpose: To actually launch the dialog, and to retrieve results
        ;"Input:  Options -- see Docs above.  The only part of the Options array
        ;"                that is used here is Options(xcCmdLine)
        ;"        Results -- an array to pass results back in.
        ;"        Modal -- IF =xcModalMode, then execution does not continue until dialog is closed
        ;"                 IF xcNonModal, then execution immediately continues.  Note in this
        ;"                   case the result of the execution will be 0 (unless an error
        ;"                   occurs creating the dialog.)  It will NOT be the result of
        ;"                   the user's button press.

        NEW Cmd,HookCmd
        NEW FileHandle
        NEW CommFPath SET CommFPath="/tmp/"
        NEW CommFNAME SET CommFNAME="M_xdialog_comm_"_$J_".tmp"
        NEW CommFile SET CommFile=CommFPath_CommFNAME

        ;"set Cmd=vDialog_" "_$GET(Options(xcCmdLine))
        ;"set Cmd=Cmd_" 2>"_CommFile
        DO ParamTextAdd(.Options," 2>"_CommFile)

        SET Modal=$GET(Modal,xcNonModal)
        IF (Modal=xcNonModal) do
        . DO ParamTextAdd(.Options," & ")

        NEW result,killme
        NEW FRef
        ;"WRITE "--------------------------------------------------",!
        ;"DO ZWRITE^TMGZWR($NAME(Options(xcCmdLine,xcCmdArray)))
        SET FRef=$name(Options(xcCmdLine,xcCmdArray,0))
        SET result=$$GTF^%ZISH(FRef,3,CommFPath,CommFNAME)

        ;"set HookCmd="cat "_CommFile
        ;"WRITE "Here is hook command",!,!,HookCmd,!,!
        ;"zsystem HookCmd

        ;"Explaination of following line:
        ;"I can't always pass the command in one string, because of limitation of string length
        ;"So I am writing out the command as a text file (to CommFile)--which will have the long
        ;"string divided up into multiple lines.  However, the bash command shell
        ;"can't deal with the command split up like this.
        ;"I have researched to find this method of stripping newlines from the end of
        ;"a line--there are probably 8 other ways to DO this too. :-)
        ;"    echo `<file` >file
        ;"I then execute the file by typing:
        ;"   sh file
        ;"And the two commands are separated on the line by a ";"
        ;"So the composite is:
        ;"   echo `<file` >file ; sh file
        ;"Note that the instructions contained in 'file' include an instruction to put
        ;"  the output from the dialog back into 'file'.  This is ok, because it won't
        ;"  be overwriten until after the command has started to execute.

        ;"set HookCmd="echo `<"_CommFile_"` >"_CommFile_" ; sh "_CommFile
        ;"WRITE "Here is hook command",!,!,HookCmd,!,!
        ;"zsystem HookCmd

        SET HookCmd="echo `<"_CommFile_"` >"_CommFile
        ;"WRITE "Here is hook command",!,!,HookCmd,!,!
        zsystem HookCmd

        ;"set HookCmd="cat "_CommFile
        ;"WRITE "Here is hook command",!,!,HookCmd,!,!
        ;"zsystem HookCmd

        SET HookCmd="sh "_CommFile
        ;"WRITE "Here is hook command",!,!,HookCmd,!,!
        zsystem HookCmd


        SET Results(xcDlgResult)=$ZSYSTEM&255  ;"get result of execution. (low byte only)

        ;"Read output info Results
        ;"set HookCmd="cat "_CommFile
        ;"WRITE "Here is hook command",!,!,HookCmd,!,!
        ;"zsystem HookCmd
        SET FileHandle=$$FTG^%ZISH(CommFPath,CommFNAME,$name(Results(xcDlgOutput)),3)

        ;"Now KILL the communication file... no longer needed.
        NEW FileSpec
        SET FileSpec(CommFile)=""
        SET result=$$DEL^%ZISH(CommFPath,$name(FileSpec))

        QUIT



 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

Demo
        ;"Purpose: To show the functionality of the library, and
        ;"         to give a programming demo.


        NEW result
        NEW Feedback
        NEW s
        NEW UserPick,filename
        NEW UseGUI

        SET UseGUI=0

        DO SetupConsts()

        DO SetGUI(UseGUI)

        NEW List
        SET List(1,xcTag)="Graphic"
        SET List(1,xcItem)="Select this for full X-system GUI"
        SET List(1,xcStatus)="on"
        SET List(2,xcTag)="Text"
        SET List(2,xcItem)="Select this for character interface"
        SET List(2,xcStatus)="off"
        SET UserPick=$$RadioList("Which type of boxes would you like to use?",.List)

        DO ChClrScr^TMGXDLG()

        IF UserPick="Graphic" do
        . SET UseGUI=1
        . DO SetGUI(UseGUI)

        IF UseGUI=0 GOTO l1

        SET s="Welcome to the Xdialog Demo \nThis box is 'non-modal' "
        SET s=s_"so its program can continue without"
        SET s=s_"waiting for a user response."
        SET result=$$Msg("Welcome",s,0,0,xcNonModal,1,2)  ;"height&width of 0,0 means "auto size"

l1
        SET result=$$YesNo^TMGXDLG("Do you want to see a demo \n of this Xdialog wrapper library?")
        IF result'=mrYes GOTO DemoDone

        ;"Note: This don't seem to work in character mode...
        SET s="OK, Check out this 'Info' box.  It will auto close in 6 seconds"
        SET result=$$Info(s,0,0,6,xcModalMode)

        NEW List
        SET List(1,xcTag)="Edit box"
        SET List(1,xcItem)="Select this for an Edit Box"
        SET List(1,xcStatus)="on"
        SET List(2,xcTag)="Log box"
        SET List(2,xcItem)="Select this for a Log Box"
        SET List(2,xcStatus)="off"
        SET List(3,xcTag)="Text box"
        SET List(3,xcItem)="Select this for a Text Box"
        SET List(3,xcStatus)="off"
        SET List(4,xcTag)="Tail box"
        SET List(4,xcItem)="Select this for a Tail Box"
        SET List(4,xcStatus)="off"
        SET UserPick=$$RadioList("Select Tool to See",.List)

        WRITE "You selected: ",UserPick,!

        ;"Note: This don't seem to work in character mode...
        IF UserPick'="" do
        . SET filename=$$FileSel("Select a file to load")
        . IF UserPick="Edit box" DO  QUIT
        . . SET result=$$Edit(filename,0,0,.Feedback)
        . IF UserPick="Log box" DO  QUIT
        . . SET result=$$Log(filename,0,0,xcModalMode)
        . IF UserPick="Text box" DO  QUIT
        . . SET result=$$Text(filename,0,0,xcModalMode)
        . IF UserPick="Tail box" DO  QUIT
        . . SET result=$$Tail(filename,0,0,xcModalMode)

        NEW FNAME,LName,Zip
        NEW DumpVar

        SET result=$$Input("Enter Name",0,0,"John",.FNAME)
        IF result=mrCancel GOTO GBye
        WRITE "Here is name:",FNAME,!
        ;"read "Press any key to coninue",*DumpVar,!

        ;"Note: This not supported in character mode...
        SET result=$$Input2("Enter Name",0,0,"First","John","Last","Smith",.FNAME,.LName)
        IF result=mrCancel GOTO GBye
        WRITE "Here is name:",FNAME," ",LName,!
        ;"read "Press any key to coninue",*DumpVar,!

        ;"Note: This not supported in character mode...
        SET result=$$Input3("Enter Name",0,0,"First","John","Last","Smith","Zip","12345",.FNAME,.LName,.Zip)
        IF result=mrCancel GOTO GBye
        WRITE "Here is name:",FNAME," ",LName,!
        WRITE "zip: ",Zip,!

        ;"Note: This not supported in character mode...
        KILL List
        SET List(1)="Cookies"
        SET List(2)="Ice Cream"
        SET List(3)="Cake"
        SET result=$$Combo("Pick your favorite dessert:",0,0,.List)
        WRITE "You picked: ",result,!

        NEW Result1,Result2,Result3

        ;"Note: This not supported in character mode...
        SET result=$$Range("Enter some numbers",0,0,25,250,100,.Result1)
        IF result=mrCancel GOTO GBye
        WRITE "$=",Result1,!

        ;"Note: This not supported in character mode...
        SET result=$$Range2("Enter some numbers",0,0,"$",25,250,100,"%",33,66,44,.Result1,.Result2)
        IF result=mrCancel GOTO GBye
        WRITE "$=",Result1," and %=",Result2,!

        ;"Note: This not supported in character mode...
        SET result=$$Range3("Enter some numbers",0,0,"$",25,250,100,"%",33,66,44,"#",1000,2000,1500,.Result1,.Result2,.Result3)
        IF result=mrCancel GOTO GBye
        WRITE "$=",Result1," and %=",Result2," and #=",Result3,!

        ;"Note: This not supported in character mode...
        SET result=$$Spin("Enter a number",0,0,"$",25,250,100,.Result1)
        IF result=mrCancel GOTO GBye
        WRITE "$=",Result1,!

        ;"Note: This not supported in character mode...
        SET result=$$Spin2("Enter some numbers",0,0,"$",25,250,100,"%",33,66,44,.Result1,.Result2)
        IF result=mrCancel GOTO GBye
        WRITE "$=",Result1," and %=",Result2,!

        ;"Note: This not supported in character mode...
        SET result=$$Spin3("Enter some numbers",0,0,"$",25,250,100,"%",33,66,44,"#",1000,2000,1500,.Result1,.Result2,.Result3)
        IF result=mrCancel GOTO GBye
        WRITE "$=",Result1," and %=",Result2," and #=",Result3,!

GBye
        SET result=$$Msg("Goodbye","That''s all for now folks!",0,0,xcModalMode)

        IF UseGUI=0 DO ChClrScr^TMGXDLG()

DemoDone
        QUIT


 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
 ;"Progress Notes:
 ;"9-26-04       On my server, Xdialog was missing. I had to simply copy the
 ;"              Xdialog file into /usr/bin ...  I ought to have some way to
 ;"              check for existance of file and give message IF it is absent.
 ;"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
