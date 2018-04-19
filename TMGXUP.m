TMGXUP   ;TMG/kst/Altered version of XUP ;03/25/06, 2/2/14
         ;;1.0;TMG-LIB;**1**;12/23/05

 ;"Customized version of Vista XUP module
 ;"===================================================================================
 ;"The following section started as essentially a copy of ^XUP code, to allow me to
 ;" use just part of it to SET up the programmers environment
 ;"...As time has gone on, though, I have added more tweaks...
 ;"===================================================================================
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
XUP()
        ;"Purpose: Because this configurator will be working with the database,
        ;"      it must have a proper environment setup.  And user must have
        ;"      proper access.  So this function will SET up everything needed.
        ;"Output: Environmental variables are setup.
        ;"Result: 1=OK to continue.  0=Abort

 ;"Consider:
 ;"DT^DICRW: Required Variables
 ;"Sets up the required variables of VA FileMan. There are no input variables;
 ;"simply call the routine at this entry point.
 ;"NOTE: This entry point kills the variables DIC and DIK.

        NEW result SET result=cOKToCont
        IF $GET(TMGDEBUG)>0 DO DEBUGMSG^TMGDEBU4(.TMGDBINDENT,"Inside XML Scripter, setting up programmer environment.")

        ;"MSC/SGS: added to allow processes to be interrupted
        SET $ZINT="X ^%ZOSF(""INTERRUPT"")"
        Set U="^"

        GOTO XLp2   ;"bypass next section
        ;"--------------------------------------------------------------------
        ;"Set up user info.
        SET DIC=200   ;"file 200 = ^VA(200,*)
        SET DIC(0)="MZ"               ;"   "AEQMZ"
        SET X="TMGXINST,BOT"
        ;"set X="Dodd,Norman"  ;"Note: came pre-installed in OpenVistA
        DO ^DIC
        IF $GET(TMGDEBUG)>0 DO DEBUGMSG^TMGDEBU4(.TMGDBINDENT,"Y=",Y)
        IF Y<0 SET result=cAbort GOTO XUPDone
        KILL DIC
        SET DUZ=+Y
        SET DUZ(0)=$PIECE(Y(0),U,4)
        SET DTIME=600
        IF $GET(TMGDEBUG)>0 DO DEBUGMSG^TMGDEBU4(.TMGDBINDENT,"DUZ(0)=",DUZ(0))
        IF DUZ(0)'="@" DO  GOTO XUPAbort
        . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Unable to setup a user with programmer's access privilages.")
        ;"--------------------------------------------------------------------

XLp2
        NEW User,UName
        SET User=$GET(^VA(200,1,0))
        IF User="" DO  GOTO XUPAbort
        . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Unable to access user #1 (expected to be IRM,MGR).  The installer should be modified to log in as another user.  Sorry.  Quiting.")
        SET UName=$PIECE(User,"^",1)
        IF $GET(TMGDEBUG)>0 DO DEBUGMSG^TMGDEBU4(.TMGDBINDENT,"Logging in as user: ",UName)
        SET LoggedUsr=UName  ;" setup global-scope variable that script can access
        SET UName=$PIECE(User,"^",1)
        KILL DIC
        SET DUZ=1
        SET DUZ(0)=$PIECE(User,"^",4)
        SET DTIME=600
        IF $GET(TMGDEBUG)>0 DO DEBUGMSG^TMGDEBU4(.TMGDBINDENT,"DUZ(0)=",DUZ(0))
        IF DUZ(0)'="@" do
        . IF $GET(TMGDEBUG)>0 DO DEBUGMSG^TMGDEBU4(.TMGDBINDENT,"Temporarily giving install-user '@' privilages.")
        . SET DUZ(0)="@"

XLp3
        DO HOME^%ZIS    ;"Reset Home Device IO Variables

        NEW $ESTACK,$ETRAP
        SET $ECODE="",$ETRAP="" ;"Clear and error trap
        xecute ^%ZOSF("TYPE-AHEAD")

        KILL ^UTILITY($J)
        KILL ^XUTL("XQ",$J)
        DO KILL1  ;"do KILL1^XUSCLEAN

        SET DT=$$DT^XLFDT ;"DT is a system=wide date variable

        SET XUEOFF=^%ZOSF("EOFF")
        SET XUEON=^%ZOSF("EON")
        SET U="^"
        SET XUTT=0
        SET XUIOP=""
        DO GETENV^%ZOSV
        SET XUENV=Y
        SET XUVOL=$PIECE(Y,U,2)
        SET XUCI=$PIECE(Y,U,1)

        ;"Get user info
        IF $GET(DUZ)>0 do
        . KILL XUDUZ
        . IF $DATA(DUZ(0)) SET XUDUZ=DUZ(0)
        . DO DUZ^XUP(DUZ)
        . IF $DATA(XUDUZ) SET DUZ(0)=XUDUZ
        . KILL XUDUZ

        IF ($GET(DUZ)'>0)!(('$DATA(DUZ(0)))) DO ASKDUZ^XUP GOTO:Y'>0 XUPAbort

        IF '$DATA(XQUSER) SET XQUSER=$S($DATA(^VA(200,DUZ,20)):$PIECE(^(20),"^",2),1:"Unk")
        SET DTIME=600 ;Set a temp DTIME

        ;"Getting Terminal Type
        ;"if XUTT DO ENQ^XUS1 G:$D(XUIOP(1)) ZIS2 S Y=0 D TT^XUS3 I Y>0 S XUIOP(1)=$P(XUIOP,";",2) G ZIS2
        IF 'XUTT GOTO ZIS2a
        DO ENQ^XUS1
        IF $DATA(XUIOP(1)) GOTO ZIS2
        SET Y=0
        DO TT^XUS3
        IF Y>0 SET XUIOP(1)=$P(XUIOP,";",2)
        GOTO ZIS2
ZIS2a
        SET X="`"_+$G(^VA(200,DUZ,1.2))
        SET DIC="^%ZIS(2,"
        SET DIC(0)="MQ"_$S(X]"`0":"",1:"AE")
        DO ^DIC
        IF Y'>0 GOTO XUPAbort
        SET XUIOP(1)=$P(Y,U,2)
        IF DIC(0)["A",$GET(^VA(200,+DUZ,0))]"" SET $PIECE(^VA(200,DUZ,1.2),U,1)=+Y

ZIS2
        SET %ZIS="L"  ;"will cause IO("ZIO") to contain static physical port name
        SET IOP="HOME;"_XUIOP(1)
        DO ^%ZIS    ;"Set up device handler
        IF POP GOTO XUPAbort ;"POP has error from ^%ZIS
        IF $GET(TMGDEBUG)>0 DO DEBUGMSG^TMGDEBU4(.TMGDBINDENT,"Using terminal type: ",IOST)
        SET DTIME=$$DTIME^XUP(DUZ,IOS)
        SET DUZ("BUF")=1
        SET XUDEV=IOS

        ;"Save info, Set last sign-on
        DO SAVE^XUS1
        SET $PIECE(^VA(200,DUZ,1.1),"^",1)=$$NOW^XLFDT   ;DT

        ;"Setup error trap
        IF $$GET^XPAR("USR^SYS","XUS-XUP SET ERROR TRAP",1,"Q") SET $ETRAP="D ERR^XUP"
        ;do KILL1  ;"do KILL1^XUSCLEAN
        SET $PIECE(XQXFLG,U,3)="XUP"

        ;"D ^XQ1  ;<----- one major change made to this code...

XUPDone
        QUIT result

XUPAbort
        DO KILL1   ;"do KILL1^XUSCLEAN
        KILL XQY,XQY0
        IF $$GET^XPAR("USR^SYS","XUS-XUP VPE",1,"Q"),$DATA(^%ZVEMS) xecute ^%ZVEMS ;"Run VPE

        SET result=cAbort
        GOTO XUPDone

KILL1
        ;"--------------------------------
        ;"KILL1^XUSCLEAN is included and modified below.
        ;"Purpose: To clean up ALL but kernel variables.
        ;"-------------------------------
        If $$BROKER^XWBLIB do
        . SET %2=$PIECE($text(VARLST^XWBLIB),";;",2)
        . IF %2]"" NEW @%2 ;"Protect Broker variables.

        NEW KWAPI,XGWIN,XGDI,XGEVENT
        NEW XQAEXIT,XQAUSER,XQX1,XQAKILL,XQAID

        KILL IO("C"),IO("Q")

        ;"Note: KILL (x) mean KILL everything EXCEPT x
        ;"I can't KILL everthing because it will crash my script--so I'll just not DO it.
        ;"kill (DISYS,IO,IOBS,IOF,IOM,ION,IOSL,IOST,IOT,IOS,IOXY,XRTL,%ZH0,XQVOL,XQY,XQY0,XQDIC,XQPSM,XQPT,XQAUDIT,XQXFLG,ZTSTOP,ZTQUEUED,ZTREQ,U,DUZ,DUZ,DTIME,DT)

        QUIT

 ;"===================================================================================
