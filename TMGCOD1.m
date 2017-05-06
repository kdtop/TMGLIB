TMGCOD1 ;TMG/kst/A Code Analysis Tools ;08/17/08, 2/2/14
         ;;1.0;TMG-LIB;**1**;08/17/08
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
ScanAll  ;
        NEW pArray
        SET pArray=$name(^TMG("MODULE CALLS"))
        ;
        NEW Itr,index
        NEW abort SET abort=0
        SET index=$$ItrAInit^TMGITR($name(^DIC(9.8,"B")),.Itr)
        DO PrepProgress^TMGITR(.Itr,20,1,"index")
        SET index="TMGKIDS" ;"temp!
        IF index'="" FOR  DO  QUIT:($$ItrANext^TMGITR(.Itr,.index)="")!abort
        . IF $$USRABORT^TMGUSRI2 SET abort=1 QUIT
        . ;"WRITE index,!
        . DO GetCalls(index,pArray)
        DO ProgressDone^TMGITR(.Itr)
        ; 
        QUIT
        ;
GetCalls(Module,pArray)  ;
        ;"Purpose: To scan one code module and look for *static* calls to other
        ;"         modules.  It will NOT detect indirection (i.e. DO @X).
        ;"         Call MUST have a tag.  MyTag^MyMod is detected, but ^MyMod is NOT
        ;"Input: Module -- the name of the module to scan, as would be used
        ;"                 in $text().  I.e. XUP, but not ^XUP, not XUP.m
        ;"       Array -- PASS BY NAME, an OUT PARAMETER.  Format below.
        ;"Output:  Array filled as follows:
        ;"              Array(Module,Addr)=OutSideAddr  (Example of Addr=A+5^MYFUNCT)
        NEW curModule SET curModule=$GET(Module)
        NEW blankLines SET blankLines=0
        NEW offset SET offset=1
        NEW curLabel SET curLabel=""
        ;
        FOR  DO  QUIT:(blankLines>30)
        . NEW Addr SET Addr=curLabel_"+"_offset_"^"_curModule
        . NEW oneLine SET oneLine=$text(@Addr)
        . NEW firstChar SET firstChar=$EXTRACT(oneLine,1,1)
        . SET offset=offset+1
        . IF oneLine="" SET blankLines=blankLines+1 QUIT
        . SET oneLine=$$CutComment(oneLine)
        . NEW tempLabel SET tempLabel=$$GetLabel(oneLine)
        . IF tempLabel'="" SET curLabel=tempLabel,offset=1
        . NEW tempS SET tempS=oneLine
        . IF tempS'["^" QUIT
        . NEW done SET done=0
        . FOR  QUIT:(tempS'["^")!(done=1)  do
        . . IF $$InQt^TMGSTUTL(tempS,$find(tempS,"^")-1)=1 SET done=1 QUIT
        . . NEW preS,postS
        . . DO GetPrePost(.tempS,.preS,.postS)
        . . IF (preS="")!(postS="") QUIT
        . . ;"WRITE curLabel,"+",offset,"--> "
        . . ;"WRITE preS,"^",postS,"   [",$$Trim^TMGSTUTL(oneLine),"]",!
        . . ;"if preS["$" QUIT
        . . NEW tempAddr SET tempAddr=preS_"^"_postS
        . . ;"WRITE "==> ",$text(@tempAddr),!
        . . ;"set @pArray@("CALLS OUT",curModule,curLabel,offset,tempAddr)=""
        . . SET @pArray@("CALLS IN",postS_"::"_preS,curModule,curLabel,offset)=""
        QUIT
        ;
CutComment(oneLine) ;
        ;"Purpose: to cut off any comments from line, indicated by ;
        ;"Input: oneLine : string to alter
        ;"Result: modified inputline
        ;
        NEW i,L,result
        SET result=$GET(oneLine)
        IF result'[";" GOTO CCmDone
        ;
        NEW inStr,done,ch
        SET inStr=0,done=0
        SET L=$LENGTH(oneLine)
        for i=1:1:L DO  QUIT:done
        . SET ch=$EXTRACT(oneLine,i)
        . IF ch="""" do
        . . IF inStr=0 SET inStr=1 QUIT
        . . IF $EXTRACT(oneLine,i+1)="""" SET i=i+1 QUIT
        . . SET inStr=0
        . IF (ch=";")&(inStr=0) DO  QUIT
        . . SET result=$EXTRACT(oneLine,1,i-1)
        . . SET done=1
        ;
CCmDone QUIT result
        ;
GetLabel(oneLine) ;
        ;"Purpose: Get label for a line, IF one exists
        ;"Input: oneLine : string to scan
        ;"Result: Label, or "" IF none
        ;
        NEW result SET result=$GET(oneLine)  ;"default to entire line is label
        NEW ch SET ch=$EXTRACT(oneLine,1)
        IF (ch=" ")!(ch=$CHAR(9)) DO  GOTO GLDone
        . SET result=""
        ;
        NEW i,done SET done=0
        for i=1:1:$LENGTH(oneLine) DO  QUIT:done
        . SET ch=$EXTRACT(oneLine,i)
        . IF (ch=" ")!(ch=$CHAR(9))!(ch="(")!(ch=";") DO  QUIT
        . . SET result=$EXTRACT(oneLine,1,i-1)
        . . SET done=1
        ;
GLDone  QUIT result
        ;
GetPrePost(oneLine,preS,postS) ;
        ;"Purpose: To take a string like 'write $$MyFunct^MyModule(123)' and return
        ;"         '$$MyFunct' in preS, and 'MyModule' in postS.  Also shorten
        ;"         input string, oneLine, so that other calls on the same line
        ;"         can be processed on a separate run.
        ;"Input: oneLine -- the string to process.  PASS BY REFERENCE to get back
        ;"                  shortened line with first instance cut off.
        ;"       preS -- PASS BY REFERENCE, an OUT PARAMETER
        ;"       postS --  PASS BY REFERENCE, an OUT PARAMETER
        ;"Output: See purpose above
        ;"Result: none
        ;
        NEW done,p
        ;"Work first on preS
        SET preS=$PIECE(oneLine,"^",1)
        SET p=$LENGTH(preS)
        SET done=0
        IF p>0 FOR  DO  QUIT:(p=0)!(done=1)
        . NEW ch SET ch=$EXTRACT(preS,p)
        . SET p=p-1
        . IF ch?1(1A,1N) QUIT
        . SET p=p+2,done=1
        SET preS=$EXTRACT(preS,p,999)
        ;
        ;"weed out entries like '_^DIC'  or '_U_^TMP'
        ;"if ($EXTRACT(preS,1)="_")!($EXTRACT(preS,$LENGTH(preS))="_" SET preS=""
        ;
        ;"Now work on postS
        SET postS=$PIECE(oneLine,"^",2,$LENGTH(oneLine,"^"))
        SET p=1,done=0
        FOR  DO  QUIT:(p>$LENGTH(postS))!(done=1)
        . NEW ch SET ch=$EXTRACT(postS,p)
        . SET p=p+1
        . IF ch?1(1A,1N,1"_") QUIT
        . SET p=p-2,done=1
        ;
        SET oneLine=$EXTRACT(postS,p+1,999)
        SET postS=$EXTRACT(postS,1,p)
        ;
        QUIT
        ;
 ;"=======================================================
test ;
        NEW array
        NEW DIC,X,Y
        SET DIC=9.8
        SET DIC(0)="MAEQ"
t1 ;
        DO ^DIC WRITE !
        IF +Y'>0 GOTO testDone
        ;"kill array
        ;"Commented because tag doesn't exist -> DO GetCallsOut($PIECE(Y,"^",2),"array")
        IF $DATA(array) DO ZWRITE^TMGZWR("array")
        GOTO t1
        ;
testDone ;
        QUIT