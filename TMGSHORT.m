TMGSHORT ;TMG/kst/Code to Shorten Names ;03/25/06, 2/2/14
         ;;1.0;TMG-LIB;**1**;12/23/06
 ;"  SHORTEN NAMES code
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
 ;"ShortNetName(GenericName,TradeName,Strength,Units,MaxLen)
 ;"$$ShortenArray(Names,Dividers,MaxLen,AllowCut) -- core menus for shortening name
 ;"$$PShortName(Name,Length,AskUser) -- shorten the drug smartly, using abbreviations
 ;"$$ShortName(Name,Length,AskUser,DivStr) -- shorten the drug smartly, using abbreviations
 ;"$$Short2Name(Name,Div1,Div2,.Words,.Dividers) -- Shorten a name to shortest form possible
 ;"$$Short1Name(Name,MaxLen,Div1,Div2,Words,Dividers) -- An interactive editing of one name
 ;"$$Cut1Name(Name,MaxLen,Div1,Div2,Words,Dividers) -- A non-interactive cut of one name

 ;"=======================================================================
 ;" Private Functions.
 ;"=======================================================================
 ;"$$ReadJoin(JoinNum,Len,Words,Dividers) -- read out a phrase of joined words, Len words long
 ;"SetJoin(JoinNum,Len,Words,Dividers) -- reform the Word and Dividers arrays such that
 ;"         words are joined together.  E.g. #1='One' #2='Minute' ==> #1='One Minute'
 ;"SubDivArray(Words,Dividers,Div1,Div2) -- check and handle IF words in Words array need subdivision
 ;"PackArrays(pNames,pDividers) -- pack the arrays, after items had been deleted.
 ;"CompArray(Names,Dividers) -- reconstruct the resulting sentence from words in array.
 ;"AutoShortenArray(.Names,.Dividers,MaxLen,Div1,Div2) -- automatically shorten the words in the array
 ;"$$CutName(.Names,.Dividers,MaxLen) -- return a non-interactive shortened ('cut') name

 ;"=======================================================================
 ;"=======================================================================

ShortNetName(GenericName,TradeName,Strength,Units,MaxLen,AllowCut)
        ;"Purpose: to create a shortened name from parts, not longer than MaxLen
        ;"Input: GenericName -- Generic portion of name
        ;"       TradeName -- Tradename portion of name
        ;"       Strength -- OPTIONAL Strength portion of name
        ;"       Units -- OPTIONAL units portion of name
        ;"       MaxLen -- the maximum length
        ;"       AllowCut -- OPTIONAL If 1 then name may be cut off with ... to reach target length
        ;"                              and user will not be asked for input
        ;"                            If 2 then name wil be shortened as far as possible, but it
        ;"                              wil not be cut off
        ;"Result: Returns NEW shortened name, or "^" for user abort

        NEW result,temp
        SET GenericName=$GET(GenericName)
        SET TradeName=$GET(TradeName)
        SET Strength=$GET(Strength)
        SET Units=$GET(Units)
        SET MaxLen=$GET(MaxLen,16)
        SET AllowCut=$GET(AllowCut,0)

        NEW Names,Dividers
        NEW unitsIdx,GenericIdx SET GenericIdx=0,unitsIdx=0
        ;"sometimes 'Trade Name' is actually an expanded form of the Generic name
        ;"e.g. ACETAZOLAMIDE (ACETAZOLAMIDE CAP USP) 250
        ;"In these cases I will delete the duplication
SNN0    IF $EXTRACT(TradeName,1,$LENGTH(GenericName))=GenericName SET GenericName=""
        IF (TradeName="")!(GenericName="") do
        . NEW i SET i=0
        . IF TradeName'="" SET i=i+1,Names(i)=TradeName,Dividers(i)=" "
        . IF GenericName'="" SET i=i+1,Names(i)=GenericName,Dividers(i)=" ",GenericIdx=i
        . ;"set Names(i)=TradeName,Dividers(i)=" ",i=i+1
        . IF Strength'="" SET i=i+1,Names(i)=Strength,Dividers(i)=" "
        . IF Units'="" SET i=i+1,Names(i)=Units,unitsIdx=i,Dividers(i)=""
        . SET Names("MAXNODE")=i,Dividers("MAXNODE")=i
        ELSE  do
        . NEW i SET i=0
        . SET i=i+1,Names(i)=TradeName,Dividers(i)=" ("
        . SET i=i+1,Names(i)=GenericName,GenericIdx=i,Dividers(i)=") "
        . ;"set i=i+1,Names(i)=GenericName,GenericIdx=i,Dividers(i)=" ("  ;changed 10-30-07
        . ;"set i=i+1,Names(i)=TradeName,Dividers(i)=") "
        . IF Strength'="" SET i=i+1,Names(i)=Strength,Dividers(i)=" "
        . IF Units'="" SET i=i+1,Names(i)=Units,unitsIdx=i,Dividers(i)=""
        . SET Names("MAXNODE")=i,Dividers("MAXNODE")=i

        for i=1:1:Names("MAXNODE")-1 DO     ;"don't cleave units (e.g. MG/ML)
        . SET:(i>1) Names(i)=$TRANSLATE(Names(i),"/","|")
        DO SubDivArray(.Names,.Dividers," ","/")

        SET result=$$ShortenArray(.Names,.Dividers,MaxLen,AllowCut)
        IF result=0 KILL Names,Dividers GOTO SNN0  ;"honor requested retry

        ;"If shortening required "...", see IF removing parts of name allow goal.
        IF (AllowCut=1)&(result["...") do
SNN1    . ;"try removing units first
        . KILL Names(unitsIdx),Dividers(unitsIdx)
        . DO PackArrays("Names","Dividers")
        . SET result=$$ShortenArray(.Names,.Dividers,MaxLen,AllowCut)
        . IF result'["..." QUIT
        . IF GenericIdx'=0 do
        . . KILL Names(GenericIdx)
        . . IF Dividers(GenericIdx)=" (" SET Dividers(GenericIdx+1)=" "
        . . KILL Dividers(GenericIdx)
        . . DO PackArrays("Names","Dividers")
        . . SET result=$$ShortenArray(.Names,.Dividers,MaxLen,AllowCut)
        . IF result'["..." QUIT
        . ;"more later... ?

SNNDone
        SET result=$$Trim^TMGSTUTL(result)
        IF $EXTRACT(result,1,1)="(" DO   ;"Input transform doesn't allow first chart to be '('
        . ;"NOTE: I should WRITE better code to change only the LAST ) to "", i.e. not cut out ALL ()'s
        . SET result=$TRANSLATE(result,"(","")
        . SET result=$TRANSLATE(result,")","")
        IF (result[")")&(result'["(") SET result=$TRANSLATE(result,")","")
        SET result=$TRANSLATE(result,"|","/")
        QUIT result


ShortenArray(Names,Dividers,MaxLen,AllowCut)
        ;"Purpose: shorten name
        ;"Input: Names -- PASS BY REFERENCE.  An array containing the words
        ;"       Dividers -- PASS BY REFERENCE.  An array containing the bits between words
        ;"       MaxLen -- OPTIONAL. Default=1.  The length that words must fit within
        ;"       AllowCut -- OPTIONAL.  Default=0.  Set 1 IF automatic shortening is allowed.
        ;"                  If 1, MaxLen value SHOULD BE supplied
        ;"                  If 2 then name wil be shortened as far as possible, but it
        ;"                       wil not be cut off.  User will not be asked.

        ;"Result: returns the shortened name, or "^" for abort, or 0 for requested retry.

        NEW result SET result=""
        SET MaxLen=$GET(MaxLen,1)
        SET AllowCut=$GET(AllowCut,0)
        NEW UserAsked SET UserAsked=0
        NEW StartOver SET StartOver=0
        NEW OrigName SET OrigName=$$CompArray(.Names,.Dividers)

        ;"First try a non-interactive shortening
        SET result=$$AutoShortenArray(.Names,.Dividers,MaxLen,"/"," ")
        IF (AllowCut'=1)&(result["...") GOTO SNA0
        IF $LENGTH(result)'>MaxLen GOTO SNA1Done

SNA0    IF AllowCut=1 SET result=$$CutName(.Names,.Dividers,MaxLen) GOTO SNA1Done
        IF AllowCut=2 SET result=$$CompArray(.Names,.Dividers) GOTO SNA1Done

SNA1    IF result=0 GOTO SNA2Done  ;"requesting retry.
        SET result=$$Trim^TMGSTUTL($$CompArray(.Names,.Dividers))
        IF $LENGTH(result)'>MaxLen GOTO SNA1Done

        WRITE OrigName,"-->",!
        WRITE "Current Name:",!
        WRITE result,!
        IF MaxLen>1 do
        . NEW tempS SET tempS="Shorten to ---> |"
        . for i=1:1:MaxLen-$LENGTH(tempS) WRITE " "
        . WRITE tempS
        . for i=1:1:$LENGTH(result)-MaxLen WRITE "x"
        . WRITE !

        WRITE "-----------------------",!
        for i=1:1:Names("MAXNODE") do
        . IF $GET(Names(i))="" QUIT
        . WRITE i,".  ",Names(i)
        . NEW temp SET temp=$$GetAbvr^TMGABV(Names(i),0)
        . IF (temp'="")&(temp'=Names(i)) WRITE "   (<-- Quick Fix: ",temp,")"
        . WRITE !
        WRITE "-----------------------",!
        WRITE " # (or #-#) -- Shorten name(s)     Q# (or #-#) -- Use Quick FiX",!
        WRITE " S# -- Sub-edit name               T  -- Free text for ALL",!
        WRITE " S?# -- Sub-edit name (ask for divider character)",!
        WRITE " Sx# -- Sub-edit name (use any character (i.e. replace 'x') as divider)",!
        WRITE " J# -- Join word # to word #+1     F# -- Fix erroneous abbrev",!
        WRITE " D# (or D#-#) -- Delete #          X# -- Kill Quick Fix",!
        WRITE " !  -- toggle debug mode ",$SELECT(($GET(TMGDBABV)=1):"OFF",1:"ON"),!
        WRITE " C  -- cut to: ",$$CutName(.Names,.Dividers,MaxLen),!
        ;"WRITE " ^^ -- Abort",!
        WRITE "(^ to QUIT, ^^ to abort): ^//"
        SET UserAsked=1
        read temp:$GET(DTIME,3600),!
        SET temp=$$UP^XLFSTR(temp)
        IF temp="" SET temp="^" DO  GOTO SNA1Done
        . SET result=$$CompArray(.Names,.Dividers)
        IF temp="^^" SET result="^" GOTO SNA2Done
        IF temp="C" SET AllowCut=1 GOTO SNA0
        IF "S"[$EXTRACT(temp,1) do
        . NEW num1,s
        . NEW nodeDiv SET nodeDiv=" "
        . SET s=$EXTRACT(temp,2)
        . IF +s'=s DO  QUIT:(nodeDiv="^")
        . . IF s="?" DO  QUIT:(nodeDiv="^")
        . . . WRITE "Enter character that divides words (e.g. '/'  ','  '|'  ';'  ' ' etc.)",!
        . . . read "Divider character? ' '// ",nodeDiv:$GET(DTIME,3600),!
        . . . IF nodeDiv="" SET nodeDiv=" "
        . . ELSE  SET nodeDiv=s
        . . SET num1=+$EXTRACT(temp,3,99)
        . ELSE  SET num1=+$EXTRACT(temp,2,99)
        . IF num1=0 read "Enter NUMBER of name to edit: ",num1:$GET(DTIME,3600),!
        . SET num1=+num1
        . IF (num1'>0)!(num1>Names("MAXNODE")) QUIT
        . NEW temp SET temp=$$Short1Name(Names(num1),$LENGTH(Names(num1))-1,nodeDiv)
        . IF (temp="^")!(temp="")!(temp=Names(num1)) QUIT
        . DO Write^TMGABV(Names(num1),temp,,1)  ;"1=> confirm
        . SET Names(num1)=temp
        IF temp="T" DO  GOTO SNA1Done
TX1     . WRITE "Enter text for ENTIRE name (combining all shown parts) (^ to abort):",!
        . read "> ",input:$GET(DTIME,3600),!
        . IF input="^" QUIT
        . ;"kill Words,Dividers
        . KILL Names,Dividers
        . ;"set Words(1)=input,Words("MAXNODE")=1,Dividers(1)=""
        . SET Names(1)=input,Names("MAXNODE")=1,Dividers(1)=""
        IF "J"[$EXTRACT(temp,1) do
        . NEW JoinNum
        . SET JoinNum=+$EXTRACT(temp,2,99)
        . IF JoinNum'>0 read "Enter # to join: ",JoinNum:$GET(DTIME,3600),!
        . IF +JoinNum'>0 QUIT
        . ;"if JoinNum=Words("MAXNODE") DO  QUIT
        . IF JoinNum=Names("MAXNODE") DO  QUIT
        . . WRITE "Enter the # of the FIRST word to be joined.",!
JL1     . ;"do SetJoin(JoinNum,2,.Words,.Dividers)
        . DO SetJoin(JoinNum,2,.Names,.Dividers)
        IF (temp="D")!(temp?1"D".N)!(temp?1"D".N1"-".N) DO  GOTO SNA1
JL2     . NEW delNum,delNum2,i
        . SET temp=$EXTRACT(temp,2,99)
        . ;"if Words("MAXNODE")=1 SET delNum=1,delNum2=1
        . IF $GET(Names("MAXNODE"))=1 SET delNum=1,delNum2=1
        . ELSE  do
        . . SET delNum=+$PIECE(temp,"-",1)
        . . SET delNum2=+$PIECE(temp,"-",2)
        . . IF delNum2<delNum SET delNum2=delNum
        . . IF delNum>0 QUIT
        . . read "Enter # (or #-#) to delete: ",temp:$GET(DTIME,3600),!
        . . SET delNum=+$PIECE(temp,"-",1)
        . . SET delNum2=+$PIECE(temp,"-",2)
        . . IF delNum2<delNum SET delNum2=delNum
        . for i=delNum:1:delNum2 do
        . . ;"if +i>0 KILL Words(i),Dividers(i)
        . . IF +i>0 KILL Names(i),Dividers(i)
        . ;"do PackArrays("Words","Dividers")
        . DO PackArrays("Names","Dividers")
        IF "X"[$EXTRACT(temp,1) do
        . NEW delNum
        . ;"if Words("MAXNODE")=1 SET delNum=1
        . IF Names("MAXNODE")=1 SET delNum=1
        . ELSE  do
        . . SET delNum=+$EXTRACT(temp,2,99)
        . . IF delNum>0 QUIT
        . . read "Enter # of Quick Fix to delete: ",delNum:$GET(DTIME,3600),!
        . ;"if +delNum>0 DO Del^TMGABV(Words(delNum))
        . IF +delNum>0 DO Del^TMGABV(Names(delNum))
        IF (temp?.N)!(temp?.N1"-".N) DO  GOTO SNA1
        . NEW num1,num2
        . SET num1=+$PIECE(temp,"-",1)
        . SET num2=+$PIECE(temp,"-",2)
        . IF num2=0 SET num2=num1
        . NEW tempS SET tempS=""
        . for i=num1:1:num2 SET tempS=tempS_Names(i)_" "
        . SET tempS=$$Trim^TMGSTUTL(tempS)
        . SET tempS=$$GetAbvr^TMGABV(tempS,1)
        . for i=num1+1:1:num2 KILL Names(i)
        . for i=num1:1:(num2-1) KILL Dividers(i)
        . SET Names(num1)=tempS
        . DO PackArrays("Names","Dividers")
        IF (temp="Q")!(temp?1"Q".N)!(temp?1"Q".N1"-".N) DO  GOTO SNA1
        . NEW num1,num2
        . SET num1=+$EXTRACT(temp,2,99)
        . IF num1=0 DO  QUIT:(+num1=0)
        . . read "Enter NUMBER(S) of Quick Fix to use: ",temp:$GET(DTIME,3600),!
        . . SET num1=+$PIECE(temp,"-",1)
        . . SET num2=+$PIECE(temp,"-",2)
        . IF +$GET(num2)=0 SET num2=num1
        . for i=num1:1:num2 do
        . . SET Names(i)=$$GetAbvr^TMGABV(Names(i),0)
        IF (temp="F")!(temp?1"F"1N) DO  GOTO SNA1
        . NEW num1 SET num1=+$EXTRACT(temp,2,99)
        . IF num1=0 DO  QUIT:(+num1=0)
        . . read "Enter NUMBER of abbreviation to fix: ",temp:$GET(DTIME,3600),!
        . . SET num1=+temp
        . NEW s SET s=$$Fix^TMGABV(Names(num1),OrigName)
        . IF s=0 SET result=0 QUIT  ;"signal retry
        . SET Names(num1)=s
        . IF Names(num1)="" do
        . . KILL Names(num1)
        . . ;"do PackArrays("Words","Dividers")
        . . DO PackArrays("Names","Dividers")
        IF (temp="!") DO  GOTO SNA1
JL5     . IF $GET(TMGDBABV)=1 KILL TMGDBABV
        . ELSE  SET TMGDBABV=1
        . SET result=0 ;"signal request for retry.
        GOTO SNA1

SNA1Done SET result=$$Trim^TMGSTUTL(result)
SNA2Done
        IF (UserAsked=1)&(+result'=0) WRITE "Using: ",result,!
        QUIT result


ReadJoin(JoinNum,Len,Words,Dividers)
        ;"Purpose: To read out a phrase of joined words, Len words long
        ;"Input: JoinNum -- the index in Words where joining begins
        ;"       Len -- the length to return.  e.g. 2 --> two words joined
        ;"       Words -- PASS BY REFERENCE.  Array holding words
        ;"       Dividers -- PASS BY REFERENCE.  Array holding dividers between words
        ;"Results: returns string of joined words

        NEW result SET result=""
        IF (JoinNum+Len-1)>Words("MAXNODE") GOTO RJDone
        SET result=$GET(Words(JoinNum))
        NEW i for i=JoinNum:1:(JoinNum+Len-2) do
        . SET result=result_Dividers(i)_$GET(Words(i+1))
RJDone  QUIT result


SetJoin(JoinNum,Len,Words,Dividers)
        ;"Purpose: To reform the Word and Dividers arrays such that words are
        ;"         joined together.  E.g. #1='One' #2='Minute' ==> #1='One Minute'
        ;"Input: JoinNum -- the index in Words where joining begins
        ;"       Len -- the length to return.  e.g. 2 --> two words joined
        ;"       Words -- PASS BY REFERENCE.  Array holding words
        ;"       Dividers -- PASS BY REFERENCE.  Array holding dividers between words
        ;"Results: None

        NEW temp SET temp=$$ReadJoin^TMGSHORT(JoinNum,Len,.Words,.Dividers)
        NEW i for i=JoinNum:1:(JoinNum+Len-1) do
        . IF i'=JoinNum KILL Words(i)
        . IF i'=(JoinNum+Len-1) KILL Dividers(i)

        SET Words(JoinNum)=temp
        DO PackArrays("Words","Dividers")

        QUIT


Short1Name(Name,MaxLen,Div1,Div2,Words,Dividers)
        ;"Purpose: An interactive editing of one name
        ;"Input: Name -- the name (string) to shorten.
        ;"       MaxLen -- OPTIONAL.  The Max length of the string.
        ;"       Div1 -- OPTIONAL.  The first character used to separate words. Default is " "
        ;"       Div2 -- OPTIONAL.  The second character used to separate words. Default is "/"
        ;"       Words -- OPTIONAL.  PASS BY REFERENCE, an OUT PARAMETER.  Returns Name divided up into words
        ;"       Dividers -- OPTIONAL.  PASS BY REFERENCE, an OUT PARAMETER.  Returns dividers between words
        ;"Results: returns shortened name, or "^" for user abort

        SET Div1=$GET(Div1," ")
        SET Div2=$GET(Div2)

S1N0    DO CleaveToArray^TMGSTUTL(Name,Div1,.Words)
        for i=1:1:Words("MAXNODE") SET Dividers(i)=Div1
        SET Dividers(Words("MAXNODE"))=""
        IF Div2'="" DO SubDivArray(.Words,.Dividers,Div1,Div2)

        SET result=$$ShortenArray^TMGSHORT(.Words,.Dividers,MaxLen,0)
        IF result=0 KILL Words,Dividers GOTO S1N0

        QUIT result


Cut1Name(Name,MaxLen,Div1,Div2,Words,Dividers)
        ;"Purpose: A non-interactive cut of one name
        ;"Input: Name -- the name (string) to shorten.
        ;"       MaxLen -- The length of the string to cut to.
        ;"       Div1 -- OPTIONAL.  The first character used to separate words. Default is " "
        ;"       Div2 -- OPTIONAL.  The second character used to separate words. Default is "/"
        ;"       Words -- OPTIONAL.  PASS BY REFERENCE, an OUT PARAMETER.  Returns Name divided up into words
        ;"       Dividers -- OPTIONAL.  PASS BY REFERENCE, an OUT PARAMETER.  Returns dividers between words
        ;"Results: returns cut name

        SET Div1=$GET(Div1," ")
        SET Div2=$GET(Div2)

        DO CleaveToArray^TMGSTUTL(Name,Div1,.Words)
        for i=1:1:Words("MAXNODE") SET Dividers(i)=Div1
        SET Dividers(Words("MAXNODE"))=""
        IF Div2'="" DO SubDivArray(.Words,.Dividers,Div1,Div2)

        SET result=$$CutName(.Words,.Dividers,MaxLen)

        QUIT result


Short2Name(Name,Div1,Div2,Words,Dividers,Category)
        ;"Purpose: Shorten a name, using abbreviations etc. to shortest form possible
        ;"              Will separate name into individual words, separated by spaces
        ;"              and try to abbreviate each one.
        ;"Input: Name -- name to shorten
        ;"       Div1 -- OPTIONAL.  The first character used to separate words. Default is " "
        ;"       Div2 -- OPTIONAL.  The second character used to separate words. Default is "/"
        ;"       Words -- OPTIONAL.  PASS BY REFERENCE, an OUT PARAMETER.  Returns Name divided up into words
        ;"       Dividers -- OPTIONAL.  PASS BY REFERENCE, an OUT PARAMETER.  Returns dividers between words
        ;"       Category -- OPTIONAL.  a category to look for phrases in
        ;"Result: returns a shortened form of name
        ;"Note: no testing of length done.
        ;"Note: this function is NOT interactive with the user
        ;"Note: This functions should be called repetatively,using the output from
        ;"      the last run as the input for the next run, until there is not further
        ;"      change, to get the best results.

        NEW temp,result,i
        SET result=""
        IF $GET(Name)="" GOTO SN2Don2

        SET result=$$GetAbvr^TMGABV(Name,0)
        IF (result'="")&(result'=Name) GOTO SN2Done

        SET Div1=$GET(Div1," ") IF Div1="" SET Div1="@@@@"
        SET Div2=$GET(Div2,"/") IF Div2="" SET Div2="@@@@"

        KILL Words,Dividers
        DO CleaveToArray^TMGSTUTL(Name,Div1,.Words)
        for i=1:1:Words("MAXNODE") SET Dividers(i)=Div1
        SET Dividers(Words("MAXNODE"))=""  ;"//kt added 10/27/06

        ;"Note: This purposefully does not keep rechecking for ever shortening
        ;"      Abreviations (or abrv of abrv's) so that the calling function
        ;"      can concat the results from this onto others and determine a
        ;"      total length, and then recall IF needed.
        NEW count SET count=Words("MAXNODE")
        for i=1:1:count do
        . NEW temp,temp2
        . IF Words(i)[Div2 SET temp=$$Short2Name(Words(i),Div2)
        . ELSE  SET temp=$$GetAbvr^TMGABV(Words(i),0)
        . SET Words(i)=temp

        ;"Now look for double word matches
        SET Category=$GET(Category,0)
SNL0    for i=1:1:count do
        . NEW temp,temp2
        . SET temp=$$ReadJoin^TMGSHORT(i,2,.Words,.Dividers)
        . SET temp2=$$GetAbvr^TMGABV(temp,Category)
        . IF (temp2'="")&(temp'=temp2) do
SNL1    . . ;"WRITE "Found double word match: ",temp,"-->",temp2,!
        . . DO SetJoin^TMGSHORT(i,2,.Words,.Dividers)
        . . SET Words(i)=temp2
        . . ;"DO ZWRITE^TMGZWR("Words")
        . . SET i=0,count=Words("MAXNODE")

        SET result=$$CompArray(.Words,.Dividers)

SN2Done  SET result=$$Trim^TMGSTUTL(result)
        IF (Name'=result) DO Write^TMGABV(Name,result)

SN2Don2 QUIT result


SubDivArray(Words,Dividers,Div1,Div2)
        ;"Purpose: To see IF any words in Words array needs to be subdivided,
        ;"         and to handle IF needed.
        ;"Input: Words -- PASS BY REFERENCE. Array of words
        ;"       Dividers  -- PASS BY REFERENCE. Array of dividing parts
        ;"       Div1 -- the first division character, e.g. "/" or " "
        ;"       Div2 -- the second division character, e.g. " " or "/"
        ;"Results: none

        NEW i
        for i=1:1:Words("MAXNODE") do
        . IF Words(i)[Div2 do
        . . NEW tempWords,j
        . . DO CleaveToArray^TMGSTUTL(Words(i),Div2,.tempWords)
        . . for j=1:1:tempWords("MAXNODE") do
        . . . SET Words(+(i_"."_j))=tempWords(j)
        . . . IF j'=tempWords("MAXNODE") SET Dividers(+(i_"."_j))=Div2
        . . . ELSE  SET Dividers(+(i_"."_j))=Div1
        . . KILL Words(i),Dividers(i)
        DO PackArrays("Words","Dividers")

        QUIT


PackArrays(pNames,pDividers)
        ;"Purpose: to pack the arrays, after items had been deleted.
        ;"Input: Names -- PASS BY NAME. Array of words
        ;"       Dividers  -- PASS BY NAME. Array of dividing parts
        ;"Result: none

        DO ListPack^TMGMISC(pNames)
        DO ListPack^TMGMISC(pDividers)
        SET @pNames@("MAXNODE")=$$LISTCT^TMGMISC2(pNames)
        SET @pDividers@("MAXNODE")=$$LISTCT^TMGMISC2(pDividers)
        QUIT


CompArray(Names,Dividers)
        ;"Purpose: to reconstruct the resulting sentence from words in array.
        ;"Input: Names -- PASS BY REFERENCE. Array of words
        ;"       Dividers  -- PASS BY REFERENCE. Array of dividing parts
        ;"Result: returns the compiled result

        NEW result,j
        SET result=""
        for j=1:1:Names("MAXNODE") do
        . SET result=result_Names(j)
        . IF Names(j)'="" SET result=result_Dividers(j)
        QUIT result


AutoShortenArray(Names,Dividers,MaxLen,Div1,Div2)
        ;"Purpose: To automatically shorten the words in the array
        ;"Input: Names -- PASS BY REFERENCE. Array of words
        ;"       Dividers  -- PASS BY REFERENCE. Array of dividing parts
        ;"       Div1 -- the first division character, e.g. "/" or " "
        ;"       Div2 -- the second division character, e.g. " " or "/"

        NEW result,newName,changeMade
        SET result=""

        NEW temp SET temp=$$CompArray(.Names,.Dividers)
        SET result=$$GetAbvr^TMGABV(temp,0)
        IF result="^" SET result="" DO Del^TMGABV(temp)
        IF (result'="")&($LENGTH(result)'>MaxLen) GOTO ASADone

        FOR  DO  QUIT:(changeMade=0)!($LENGTH(result)'>MaxLen)
        . SET changeMade=0
        . for i=1:1:Names("MAXNODE") do
        . . SET newName=$$Short2Name(Names(i),.Div1,.Div2)
        . . ;"there was a loop where a name was repeatitively being replace with longer names --> crash
        . . IF (newName'=Names(i))&($LENGTH(newName)<$LENGTH(Names(i))) do
        . . . SET Names(i)=newName
        . . . SET changeMade=1
        . SET result=$$CompArray(.Names,.Dividers)

ASADone
        QUIT result


CutName(Names,Dividers,MaxLen)
        ;"Purpose: To return a non-interactive shortened ('cut') name
        ;"Input: Names - PASS BY REFERENCE.  As created in ShortNetName
        ;"              This is an array with the various words in the name
        ;"       Dividers -- PASS BY REFERENCE  As created in ShortNetName
        ;"              This is an array with the spaces or punctiation separating words
        ;"       MaxLen -- The target length for result
        ;"Result: returns the shortened name

        NEW partA,partB,Max,i,lenA
        NEW result

        SET Max=$GET(Names("MAXNODE"))

        IF Max'>3 DO  GOTO CutDone
        . SET result=$$CompArray(.Names,.Dividers)
        . SET result=$EXTRACT(result,1,MaxLen)

        SET partB=$GET(Dividers(Max-3))
        for i=Max-2:1:Max do
        . SET partB=partB_Names(i)
        . IF Names(i)'="" SET partB=partB_Dividers(i)
        SET partB=$$Trim^TMGSTUTL(partB)
        SET partA=""
        for i=1:1:Max-3 SET partA=partA_Names(i) SET:(i<(Max-3))&(Names(i)'="") partA=partA_Dividers(i)
        NEW allowedALen SET allowedALen=MaxLen-$LENGTH(partB)
        SET lenA=$LENGTH(partA)
        IF lenA>allowedALen do
        . SET allowedALen=allowedALen-4
        . IF lenA=0 SET partA="" QUIT
        . IF (allowedALen/lenA)<0.4 SET partA="" QUIT
        . IF allowedALen<4 SET partA="" QUIT
        . SET partA=$EXTRACT(partA,1,allowedALen)_"... "
        SET result=$$Trim^TMGSTUTL(partA_partB)
        IF $LENGTH(result)>MaxLen do
        . IF partA="" do
        . . SET partB="" ;"$GET(Dividers(Max-2))
        . . for i=Max-1:1:Max do
        . . . SET partB=partB_Names(i)
        . . . IF Names(i)'="" SET partB=partB_Dividers(i)
        . . SET partB=$$Trim^TMGSTUTL(partB)
        . . SET partA=Names(Max-2)
        . . NEW allowedALen SET allowedALen=MaxLen-$LENGTH(partB)-4
        . . SET partA=$EXTRACT(partA,1,allowedALen)_"... "
        . . SET result=partA_partB
        . ELSE  SET result=$EXTRACT(result,1,MaxLen)

CutDone
        QUIT result


PShortName(Name,Length,AskUser)
        ;"Purpose: To shorten the drug smartly, using abbreviations
        ;"         This function differs from ShortName (see below) because it smartly
        ;"         'P'icks whether to use '/' or ' ' as a divider str.
        ;"Input: Name -- the drug name to shorten
        ;"              Expected format is that found in file 50.6 field .01,
        ;"              i.e. INGREDIENT/INGREDIENT/INGREDIENT...
        ;"       Length -- The desired string length
        ;"       AskUser -- OPTIONAL.  Default=0.
        ;"                  If 1 then user is asked to supply abreviations IF needed.
        ;"                  If 2 then name is shortened as much as possible, but it
        ;"                    might be longer than Length, it is not cut, and user is
        ;"                    not asked.
        ;"Result : returns shortened name, "^" for abort.

        NEW DivStr,result
        IF $LENGTH(Name,"/")>2 SET DivStr="/"
        ELSE  SET DivStr=" "

        SET result=$$ShortName(.Name,.Length,.AskUser,DivStr)
        QUIT result

ShortName(Name,Length,AskUser,DivStr)
        ;"Purpose: To shorten the drug smartly, using abbreviations
        ;"Input: Name -- the drug name to shorten
        ;"              Expected format is that found in file 50.6 field .01,
        ;"              i.e. INGREDIENT/INGREDIENT/INGREDIENT...
        ;"       Length -- The desired string length
        ;"       AskUser -- OPTIONAL.  Default=0.
        ;"                  If 1 then user is asked to supply abreviations IF needed.
        ;"                  If 2 then name is shortened as much as possible, but it
        ;"                    might be longer than Length, it is not cut, and user is
        ;"                    not asked.
        ;"       DivStr -- the divider that separates parts. Default="/"
        ;"Result : returns shortened name, "^" for abort.

        NEW temp,Words,Dividers
        SET AskUser=$GET(AskUser,0)
        SET DivStr=$GET(DivStr,"/")

        IF Name="" SET temp="^" GOTO SNDone
        SET temp=$$Read^TMGABV(Name,Length)

        IF (temp'="")&($LENGTH(temp)'>Length) GOTO SNDone

        ;"Note: $$ShortName does NOT check length
        NEW oldTemp,done
        SET temp=Name,done=0
        FOR  DO  QUIT:done!($LENGTH(temp)'>Length)
        . SET oldTemp=temp
        . SET temp=$$Short2Name(temp,DivStr,"",.Words,.Dividers,Length)
        . IF temp=oldTemp SET done=1 QUIT
        . IF ($LENGTH(temp)'>Length) SET done=1  ;"don't QUIT yet
        . IF (temp["...")&(AskUser=1) WRITE !,"Remove '...' from name",! SET done=0

        IF (($LENGTH(temp)>Length)&(AskUser=1)) do
SNm0    . NEW killthis SET killthis=0
        . WRITE "IEN 50.6=",$GET(IEN50d6,"?")," IEN 50.606=",$GET(IEN50d606,"?")
        . WRITE " Dose=",$GET(Dose,"?")," IEN 50=",$GET(IEN50,"?"),!
        . WRITE Name,!
SNm1    . SET temp=$$Short1Name(temp,Length,DivStr,"",.Words,.Dividers)
        . IF (temp'="")&(temp'="^")&(temp'=Name) do
        . . DO Write^TMGABV(Name,temp,Length,(AskUser=1))
        . WRITE !

        IF ($LENGTH(temp)>Length)&(AskUser'=2) do
        . IF ($DATA(Words)=0)!($DATA(Dividers)=0) DO  QUIT
        . . SET temp=$EXTRACT(temp,1,Length)
        . SET temp=$$CutName(.Words,.Dividers,Length)
SNDone
        IF $EXTRACT(temp,1)="/" SET temp=$EXTRACT(temp,2,Length)
        QUIT temp


