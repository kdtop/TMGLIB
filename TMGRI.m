TMGRI ;TMG/kst/ GT.M RI ;6/24/15
         ;;1.0;TMG-LIB;**1**;6/24/15 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;                                                                 ;
 ;        Copyright 1985, 2007 Fidelity Information Services, Inc  ;
 ;                                                                 ;
 ;        This source code contains the intellectual property      ;
 ;        of its copyright holder(s), and is made available        ;
 ;        under a license.  If you DO not know the terms of        ;
 ;        the license, please stop and DO not read further.        ;
 ;                                                                 ;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;
 ;//kt Modified for file input dialog.  Additional version: 2/2/14
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Changes Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
 ;
%RI        ;service@greystone.com %RI;19920722 07:40;routine input
        ;Converts mumps routines from a standard routine output (RO)
        ;file to individual *.m files.
        ;possible enhancements:
        ;selection and/or exclusion by list, range and/or wildcard
        ;optional confirmation by routine name
        ;callable entry point
        ;
        w !,"GT.M Routine Input Utility - Converts RO file to *.m files.",!
        w "Modified to use TMG file selectors.",!
        i '$d(%zdebug) n $et s $et="zg "_$zl_":ERR^%RI" u $p:(ctrap=$c(3):exc="zg "_$zl_":EXIT^%RI")
        n d,dir,ff,l,r,x,y,%ZD,ff
        r !,"Formfeed delimited <No>? ",x
        i x="^" q
        s ff=$s($e($tr(x,"u","U"))="Y":$c(13,12),1:"")
        f  d  q:$l(%ZD)
        . ;"read !,"Press ENTER to select input file (or ^ to abort):",%ZD,!  ;//kt
        . ;"i %ZD="^" q                                                     ;//kt
        . NEW TMGFILT SET TMGFILT("*.rsa")=""  ;//kt        
        . SET %ZD=$$GETFNAME^TMGIOUTL("Pick input file.",,,,,,,.TMGFILT) ;//kt
        . ;"r !,"Input device: <terminal>: ",%ZD,!
        . ;"i '$l(%ZD) s %ZD=$p q
        . i '$l(%ZD) s %ZD="^"
        . i %ZD="^" q
        . IF $$Dos2Unix^TMGKERNL(%ZD)
        . i %ZD="?" d  q
         . . w !!,"Select the device you want for input"
         . . w !,"If you wish to exit enter a carat (^)",!
         . . s %ZD=""
        . i $zparse(%ZD)="" w "  no such device" s %ZD="" q
        . o %ZD:(readonly:block=2048:record=2044:exception="g noopen"):0
        . i '$t  w !,%ZD," is not available" s %ZD="" q
        . q
noopen        . w !,$p($ZS,",",2,999),! c %ZD s %ZD=""
        q:%ZD="^"
        u %ZD:(exception="zg "_$zl_":eof":ctrap=$C(3,$s($zver["VMS":26,1:4)))
        if %ZD'=$P r x,y u $p w !,x,!,y,!!
        NEW msg SET msg="Pick output directory\nRoutines will be saved here." ;//kt
        NEW defPath SET defPath=$p($p($p($ZRO,")",1),"(",2)," ",1)  ;//kt
        NEW l SET l=$LENGTH(defPath,"/")
        set defPath=$PIECE(defPath,"/",1,l-1)
        set defPath=$$MKTRALDV^TMGIOUTL(defPath)
        set dir=$$GETDIRNM^TMGIOUTL(msg,defPath)  ;//kt
        if dir="" SET dir="^"                                                                 ;//kt
        else  IF $EXTRACT(dir,$LENGTH(dir))'="/" SET dir=dir_"/"                              ;//kt
        ;//kt r !,"Output directory : ",dir,!!
        i dir="^" c:%ZD'=$p %ZD u $p:(ctrap="":exc="") q
        s (l,r)=0
        f  u %ZD w:$P=%ZD !,"Routine: " r x q:'$l(x)  s x=$p(x,"^") i $l(x),$e(x)?1a!($e(x)="%"),$e(x,2,99)?.an d
        . w:$P=%ZD !,"Enter routine "
        . ;warning - loop terminated by exception
        . u $p w:$x>70 ! w x,?$x\10+1*10
        . s x=dir_$tr($e(x),"%","_")_$e(x,2,9999)_".m",r=r+1        ;convert % to _
        . o x:(newversion:noreadonly:blocksize=2048:recordsize=2044)
        . f  u %ZD w:$P=%ZD ! r y q:y=ff  s l=l+1 u x w $s(y="":" ",1:y),!
        . c x
eof        u $p
        i $l(x) c x
        w !!!,"Restored ",l," line",$s(l=1:"",1:"s")
        w " in ",r," routine",$s(r=1:".",1:"s.")
        c:%ZD'=$p %ZD u $p:(ctrap="":exc="")
        q
        ;
ERR        u $p w !,$p($zs,",",2,99),!
        s $ec=""
        ; Warning - Fall-though
EXIT        i $d(%ZD),%ZD'=$p c %ZD
        u $p:(ctrap="":exc="")
        q
