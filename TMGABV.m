TMGABV   ;TMG/kst/Abbreviation code ; 03/25/06, 2/2/14
         ;;1.0;TMG-LIB;**1**;12/23/05
 ;
 ;"  ABBREVIATION code
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"=======================================================================
 ;" API -- Public Functions.
 ;"=======================================================================
 ;"$$Read(OrigName,LenCat,DefValue)
 ;"Write(OrigName,ShortName,LenCat,AskConfirm)
 ;"Del(OrigName,LenCat,AskConfirm)
 ;"GetAbvr(Name,AskUser,UseSR)
 ;"Fix(ShortName) -- provides a way to fix erroneous abbreviations.
 ;"ShowDiff -- scan and show changes.  This is not very useful (a testing function)
 ;"ScanDel(Text) -- scan for text and allow deletions.
 ;
 ;"=======================================================================
 ;" Private Functions.
 ;"=======================================================================
 ;"CheckDel(longName,DiffArray,DiffStr,lenCat)
 ;"Fix1(ShortName) -- provide a way to fix erroneous abbreviations.
 ;"ShowLinks(ShortName,LenCat,array) -- show a chain of abbreviations.
 ;"GetDiff(longName,LenCat) -- for longName, return what changes for it's abbreviation
 ;"GetDiffStr(longName,shortName) -- given longName and it's shortname abbreviation, return what changes
 ;"ScanAbvs(xstr,showProgress) -- scan abbreviations and execute code
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;
Read(OrigName,LenCat,DefValue) ;
        ;"Purpose: To read from the ABBREV array and return an abbreviation
        ;"Input:  OrigName -- the name to look up
        ;"        LenCat -- OPTIONAL.  If specified, then results returned from that category
        ;"              IF LenCat="ALL" then all categories are scanned until some value found.
        ;"        DefValue -- OPTIONAL.  If specified, a default value IF not found
        ;"Results: Returns the found abbreviation, or "" IF not found
        ;
        SET DefValue=$GET(DefValue)
        NEW result SET result=DefValue
        IF $GET(OrigName)="" GOTO RdDone
        IF $GET(LenCat)'="" DO
        . IF LenCat="ALL" DO
        . . SET result=$GET(^TMG("ABBREV",OrigName),DefValue) QUIT:(result'="")
        . . SET LenCat=""
        . . FOR  SET LenCat=$ORDER(^TMG("ABBREV",LenCat),-1) QUIT:(+LenCat'=LenCat)!(result'="")  DO
        . . . SET result=$GET(^TMG("ABBREV",LenCat,OrigName),DefValue)
        . ELSE  DO
        . . SET result=$GET(^TMG("ABBREV",LenCat,OrigName),DefValue)
        ELSE  DO
        . SET result=$GET(^TMG("ABBREV",OrigName),DefValue)
RdDone  ;
        IF result'="" DO
        . IF ($GET(TMGDBABV)=1)&(result'=OrigName) DO
        . . WRITE OrigName,"-->",!,result,"  OK"
        . . NEW % SET %=1 DO YN^DICN WRITE !
        . . IF %=1 QUIT
        . . SET result=""
        . . IF %=-1 QUIT
        . . IF %=2 DO Del(OrigNameName,.LenCat,1)
        QUIT result
        ;
Write(OrigName,ShortName,LenCat,AskConfirm) ;
        ;"Purpose: To provide a unified writer for ABBREV array
        ;"Input: OrigName -- the longer name that the abbreviation will stand for
        ;"       ShortName -- the shorter abbreviation of OrigName
        ;"       LenCat -- OPTIONAL -- If supplied, then abbreviation will be stored in this category
        ;"       AskConfirm -- OPTIONAL -- IF 1 then user asked to confirm save.
        ;"results: none
        ;"Note: Assigning a NULL ShortName is not currently allowed.
        ;
        IF $GET(OrigName)="" GOTO WtDone
        IF $GET(ShortName)="" GOTO WtDone
        SET AskConfirm=$GET(AskConfirm,0)
        IF $$Read(OrigName,.LenCat)=ShortName GOTO WtDone ;"Skip WRITE IF already there
        NEW % SET %=1
        IF AskConfirm=1 DO
W1      . WRITE "[",OrigName,"] --> [",ShortName,"]",!
        . WRITE "Save for future use"
        . DO YN^DICN WRITE !
        IF %'=1 GOTO WtDone
        IF $GET(LenCat)'="" DO
        . SET ^TMG("ABBREV",LenCat,OrigName)=ShortName
        . SET ^TMG("ABBREV",LenCat,"XREF",ShortName)=OrigName
        ELSE  DO
        . SET ^TMG("ABBREV",OrigName)=ShortName
        . SET ^TMG("ABBREV","XREF",ShortName)=OrigName
WtDone  QUIT
        ;
Del(OrigName,LenCat,AskConfirm) ;
        ;"Purpose: To delete a value from the ABBREV array
        ;"Input:  OrigName -- the name to look up
        ;"        LenCat -- OPTIONAL.  If specified, then category to delete from
        ;"        AskConfirm -- OPTIONAL -- IF 1 then user asked to confirm save.
        ;"Results: none
        ;
        IF $GET(OrigName)="" GOTO DelDone
        SET AskConfirm=$GET(AskConfirm,0)
        NEW CurValue
        IF $GET(LenCat)'="" SET CurValue=$GET(^TMG("ABBREV",LenCat,OrigName))
        ELSE  SET CurValue=$GET(^TMG("ABBREV",OrigName))
        NEW % SET %=1
        IF AskConfirm=1 DO
        . WRITE "[",OrigName,"] -->",!,"[",CurValue,"]",!
        . WRITE "OK to DELETE" DO YN^DICN WRITE !
        IF %'=1 GOTO DelDone
        IF $GET(LenCat)'="" DO
        . KILL ^TMG("ABBREV",LenCat,OrigName)
        . KILL ^TMG("ABBREV",LenCat,"XREF",CurValue)
        ELSE  DO
        . KILL ^TMG("ABBREV",OrigName)
        . KILL ^TMG("ABBREV","XREF",CurValue)
        IF AskConfirm'=1 GOTO DelDone
        ;
        ;"Now see IF this same problem needs to be fixed in other abbreviations.
        NEW tempS SET tempS=$$GetDiffStr(OrigName,CurValue)
        NEW DiffArray,count SET count=1
        WRITE "That association had the following difference(s):",!
        FOR  QUIT:(tempS'["^")  DO
        . NEW OneDiff SET OneDiff=$PIECE(tempS,"^",1)
        . SET DiffArray(count)=OneDiff,count=count+1
        . WRITE "  ",$PIECE(OneDiff,">",1)," --> ",$PIECE(OneDiff,">",2),!
        . SET tempS=tempS=$PIECE(tempS,"^",3,999)
        SET DiffArray("MAXNODE")=$$LISTCT^TMGMISC2("DiffArray")
        SET %=1
        WRITE "Delete all other abbreviations that have these difference(s)"
        DO YN^DICN WRITE !
        IF %'=1 GOTO DelDone
Del1    NEW xstr SET xstr="do CheckDel(longName,.DiffArray,DiffStr,lenCat)"
        DO ScanAbvs(xstr,1)
        ;
DelDone  QUIT
        ;
CheckDel(longName,DiffArray,DiffStr,lenCat)  ;
        ;"Purpose: this is a callback function for a ScanAbvs run
        ;"       it will be called for each abbreviation
        ;"Input: DiffArray -- PASS BY REFERENCE.  Format:
        ;"                  DiffArray(1)="Long1>short1"
        ;"                  DiffArray(2)="Long2>short2"
        ;"                  DiffArray(3)="Long3>short3"
        ;"                  DiffArray("MAXNODE")=3
        ;"      DiffStr -- a difference string, as created by $$GetDiff
        ;"      lenCat -- the category that eval is from, or "" IF none
        NEW shouldDel SET shouldDel=1
        NEW i for i=1:1:+$GET(DiffArray("MAXNODE")) DO  QUIT:(shouldDel=0)
        . SET shouldDel=DiffStr[DiffArray(i)
        IF shouldDel=1 DO Del(longName,lenCat,0)
        QUIT
        ;
GetAbvr(Name,AskUser,UseSR) ;
        ;"Purpose: To get an abbreviation for one word
        ;"Input: Name -- name to shorten
        ;"       AskUser -- IF 1, then user will be asked to supply abbreviations
        ;"       UseSR -- OPTIONAL, default=0.  If 0, then ^DIR won't be used
        ;"Note: The name returned here may be longer than desired, no testing of length done.
        ;"Results: Returns abreviated name, or original name IF not found, or "" IF deleted
        ;
        SET UseSR=$GET(UseSR,0)
        NEW result,Y
        SET result=$GET(Name)
        IF Name="" GOTO GADone
        IF $GET(AskUser)=1 DO
        . WRITE "Enter a shorter form of '"_Name_"' (^ to delete)",!
        . IF UseSR DO
        . . NEW DIR
        . . SET DIR(0)="F"
        . . SET DIR("A")="New Name"
        . . SET DIR("B")=result
        . . DO ^DIR WRITE !
        . ELSE  DO
        . . read "New Name: ",Y:($GET(DTIME,3600)),!
        . IF Y="^" DO  QUIT
        . . WRITE "Delete word from name"
        . . NEW % SET %=1 DO YN^DICN WRITE !
        . . IF %=1 SET result=""
        . IF Y'=result DO
        . . DO Write(Name,Y,,1)  ;"1=> confirm save
        . . SET result=Y
        ELSE  DO
        . SET result=$$Read(Name,,Name)
        . IF result="^" SET result="" DO Del(Name)
        . IF result="" QUIT
        . IF ($GET(TMGDBABV)=1)&(result'=Name) DO
        . . WRITE Name,"-->",!,result,!,"  OK"
        . . NEW % SET %=1 DO YN^DICN WRITE !
        . . IF %=1 QUIT
        . . IF %=-1 SET result="" QUIT
        . . IF %=2 DO
        . . . WRITE "Delete abbreviation" DO YN^DICN WRITE !
        . . . IF %=1 DO Del(Name) SET result=""
        ;
GADone  QUIT result
        ;
Fix(ShortName,Context) ;
        ;"Purpose: To provide a way to fix erroneous abbreviations.
        ;"Input: ShortName -- the abbreviation to fix.
        ;"       Context -- OPTIONAL.  The sentence ShortName is found in.
        ;"Result: Returns new name after fixing mislinked abbreviations,
        ;"        or 0 for requested retry
        NEW Menu,Option
        SET Context=$GET(Context)
        NEW result SET result=""
        ;
FL1     IF Context="" GOTO FL2
        ;
        SET Menu(0)="Pick Which to Fix"
        SET Menu(1)=ShortName
        SET Menu(2)=Context
        WRITE #
        SET Option=$$MENU^TMGUSRI2(.Menu,"^")
        IF Option="^" GOTO FixDone
        ;
FL2     IF (Option=1)!(Context="") DO  GOTO:(Context'="") FL1 GOTO FixDone
        . SET ShortName=$$Fix1(ShortName)
        . IF ShortName'="" SET result=ShortName
        IF (Option=2) DO  GOTO FixDone
        . NEW temp SET temp=$$Fix1(Context)
        . SET result=0
        IF (Option="^") GOTO FixDone
        GOTO FL1
        ;
FixDone QUIT result
        ;
Fix1(ShortName)  ;
        ;"Purpose: To provide a way to fix erroneous abbreviations.
        ;"Input: ShortName -- the abbreviation to fix.
        ;"Result: Returns new name after fixing mislinked abbreviations.
        NEW array,Option
        NEW Name,LenCat
        NEW result SET result=""
        NEW max
Fix1Loop ;
        KILL array
        DO ShowLinks(ShortName,,.array)
        ;"Return Format
        ;"      array(x)=ShortName <-- LongerName[TAB]LongerName^LenCat
        ;
        SET max=+$GET(array("MAX"))
        KILL array("MAX")
        SET array(0)="Pick item to DELETE"
        WRITE #
        SET Option=$$MENU^TMGUSRI2(.array,"^")
        IF Option="^" GOTO Fix1Done
        SET Name=$PIECE(Option,"^",1)
        SET LenCat=$PIECE(Option,"^",2)
        DO Del(Name,LenCat,1)
        GOTO Fix1Loop
        ;
Fix1Done ;
        NEW s SET s=$GET(array(max))
        SET s=$PIECE(s,$CHAR(9),2)
        SET s=$PIECE(s,"^",1)
        SET result=s
        QUIT result
        ;
ShowLinks(ShortName,LenCat,array)  ;
        ;"Purpose: To show a chain of abbreviations.
        ;"Input: ShortName -- the abbreviation to check.
        ;"       LenCat -- the category to look in
        ;"       Array -- PASS BY REFERENCE.  an OUT PARAMETER. Format
        ;"              array("MAX")=maxCount (e.g. 2)
        ;"              array(1)=ShortName <-- LongerName[TAB]LongerName^LenCat
        ;"              array(2)=ShortName <-- LongerName[TAB]LongerName^LenCat
        ;
        NEW i SET i=""
        NEW max SET max=$GET(array("MAX"),0)
        NEW value SET value=""
        IF $GET(LenCat)="" DO
        . FOR  SET i=$ORDER(^TMG("ABBREV",i)) QUIT:(+i'>0)  DO
        . . DO ShowLinks(ShortName,i,.array)
        . SET value=$GET(^TMG("ABBREV","XREF",ShortName))
        ELSE  DO
        . SET value=$GET(^TMG("ABBREV",LenCat,"XREF",ShortName))
        IF value'="" DO
        . SET max=max+1
        . WRITE max,". ",ShortName," <-- ",value,!
        . SET array(max)=ShortName_" <-- "_value_$CHAR(9)_value_"^"_$GET(LenCat)
        . SET array("MAX")=max
        . DO ShowLinks(value,.LenCat,.array)
        ;
        QUIT
        ;
GetDiff(longName,LenCat) ;
        ;"Purpose: for a given longName, return what changes for it's abbreviation
        ;"Input: longName -- the original name to search for
        ;"       LenCat -- OPTIONAL.  Default is "ALL"
        ;"Results: returns difference between longName and its abbreviation, or "" IF none.
        ;"Results:  DiffLong1>DiffShort1^pos1>pos2^DiffLong2>DiffShort2^pos1>pos2^...
        ;
        NEW result SET result=""
        SET LenCat=$GET(LenCat,"ALL")
        NEW shortName SET shortName=$$Read(longName,LenCat)
        IF shortName'="" SET result=$$GetDiffStr(longName,shortName)
        QUIT result
        ;
GetDiffStr(longName,shortName) ;
        ;"Purpose: for a given longName and it's shortname abbreviation,
        ;"         return what changes for it's abbreviation
        ;"Results: returns difference between longName and shortName, or "" IF none.
        ;"Results:  DiffLong1>DiffShort1^pos1>pos2^DiffLong2>DiffShort2^pos1>pos2^...
        ;
        NEW DiffStr SET DiffStr=""
        ;"if $GET(shortName)="" GOTO GDSDone
        NEW longWords,shortWords
        NEW DivCh SET DivCh=" "
        IF $LENGTH(longName,"/")>3 SET DivCh="/"
        DO CleaveToArray^TMGSTUTL(longName,DivCh,.longWords)
        DO CleaveToArray^TMGSTUTL(shortName,DivCh,.shortWords)
        NEW temp,i
        SET temp=$$DiffWords^TMGSTUTL(.longWords,.shortWords)
        FOR  DO  QUIT:(temp="")
        . NEW origS,destNum
        . SET origS=$PIECE(temp,"^",1)
        . SET temp=$PIECE(temp,"^",3,999)
        . IF DiffStr'="" SET DiffStr=DiffStr_"^"
        . SET DiffStr=DiffStr_origS
GDSDone QUIT DiffStr
        ;
ScanAbvs(xstr,showProgress) ;
        ;"Purpose: scan abbreviations and execute code
        ;"Input: xstr -- OPTIONAL.  m code to execute for each entry.
        ;"       showProgress -- OPTIONAL. IF 1, progress bar is shown.
        ;"Note: The following variables will be defined, for use in xstr:
        ;"        longName,shortName,DiffStr,lenCat
        NEW longName,shortName,lenCat,DiffStr
        SET longName="",lenCat=""
        NEW Itr
        ;"FOR  SET longName=$ORDER(^TMG("ABBREV",longName),-1) QUIT:(+longName>0)  DO
        SET longName=$$ItrAInit^TMGITR($name(^TMG("ABBREV")),.Itr,-1)
        IF $GET(showProgress)=1 DO PrepProgress^TMGITR(.Itr,20,1,"longName")
        IF longName'="" FOR  DO  QUIT:(+$$ItrANext^TMGITR(.Itr,.longName,-1)>0)!(longName="")
        . NEW shortName
        . SET shortName=$GET(^TMG("ABBREV",longName))
        . SET DiffStr=$$GetDiffStr(longName,shortName)
        . IF xstr'="" xecute xstr
        ;
        SET lenCat=0
        FOR  SET lenCat=$ORDER(^TMG("ABBREV",lenCat)) QUIT:(+lenCat'=lenCat)  DO
        . IF $GET(showProgress)=1 WRITE !
        . ;"set longName=""
        . ;"FOR  SET longName=$ORDER(^TMG("ABBREV",lenCat,longName),-1) QUIT:(+longName>0)!(longName="")  DO
        . SET longName=$$ItrAInit^TMGITR($name(^TMG("ABBREV",lenCat)),.Itr,-1)
        . IF $GET(showProgress)=1 DO PrepProgress^TMGITR(.Itr,20,1,"longName")
        . IF longName'="" FOR  DO  QUIT:(+$$ItrANext^TMGITR(.Itr,.longName,-1)>0)!(longName="")
        . . NEW shortName SET shortName=$GET(^TMG("ABBREV",longName))
        . . SET DiffStr=$$GetDiffStr(longName,shortName)
        . . IF xstr'="" xecute xstr
        ;
        QUIT
        ;
ShowDiff ;
        ;"Purpose: scan and show changes
        NEW xstr
        SET xstr="write longName,"" --> ["",DiffStr,""] "",shortName,!"
        DO ScanAbvs(xstr,1)
        QUIT
        ;
ScanDel(Text) ;
        ;"Purpose: scan for text and allow deletions.
        NEW xstr
        SET xstr="if DiffStr[Text DO Del(longName,,1)"
        DO ScanAbvs(xstr)
        QUIT
