TMGDBAP2 ;TMG/kst/Database API library 2 ; 2/10/13, 2/2/14
         ;;1.0;TMG-LIB;**1**;07/12/05

 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;

 ;"This module holds moved functions from TMGDBAPI (moved due to size constraints)

ConvertFDA(FDA,MarkerArray)
        ;"Purpose: To convert all the IENS's in a FDA via ConvertIENS
        ;"Input: FDA -- An FDA that need conversion.  MUST PASS BY REFERENCE
        ;"                Expected FDA is as follows.  I.e., expecting that
        ;"                there will only be ONE filenumber (the 19.01) part:
        ;"                FDA(*)
        ;"                  }~19.01
        ;"                    }~?+4,?+2,
        ;"                    | }~.01 = DIUSER
        ;"                    | }~2 = FM2
        ;"                     | }~3 = 1
        ;"                    |
        ;"                    }~?+5,?+2,
        ;"                    | }~.01 = XMMGR
        ;"                    | }~2 = X2
        ;"                    | }~3 = 1
        ;"                    |
        ;"                    }~?+6,?+2,
        ;"                      }~.01 = DIEDIT
        ;"                      }~2 = Edit
        ;"                      }~3 = 2
        ;"         MarkerArray -- see documentation in ConvertIENS
        ;"Output: FDA is changed
        ;"Result: 1=OKToContinue, 0=Abort


        IF $DATA(TMGDEBUG)#10=0 NEW TMGDEBUG SET TMGDEBUG=0
        NEW cOKToCont SET cOKToCont=1
        NEW cAbort SET cAbort=0
        NEW cParentIENS SET cParentIENS="ParentIENS"
        NEW cRef SET cRef="Ref"


        NEW result SET result=1
        IF $DATA(FDA)=0 SET result=0 GOTO CvFDAQ
        NEW FILENum
        NEW Index
        NEW IENS,OldIENS

        SET FILENum=$ORDER(FDA(""))
        IF +FILENum=0 SET result=0 GOTO CvFDAQ
        SET IENS=$ORDER(FDA(FILENum,""))
        FOR  DO  QUIT:(IENS="")
        . IF IENS="" DO  QUIT
        . . SET result=0
        . SET OldIENS=IENS
        . IF $$ConvertIENS(.IENS,.MarkerArray)=0 DO  QUIT
        . . SET IENS=""
        . . SET result=0
        . IF IENS'=OldIENS do
        . . MERGE FDA(FILENum,IENS)=FDA(FILENum,OldIENS)
        . . SET IENS=$ORDER(FDA(FILENum,OldIENS))
        . . KILL FDA(FILENum,OldIENS)
        . ELSE  do
        . . SET IENS=$ORDER(FDA(FILENum,OldIENS))
CvFDAQ
        QUIT result


ConvertIENS(IENS,MarkerArray)
        ;"Purpose: to convert an IENS such as "?+4,?+2," into "?+4,12345,", given
        ;"        the MarkerArray that corelates "2" to #"12345"
        ;"Input: IENS -- the IENS string to convert. MUST PASS BY REFERENCE
        ;"         MarkerArray -- a composite array composed of results returned
        ;"                 by database server, like below. SHOULD PASS BY REFERENCE
        ;"                MarkerArray(*)
        ;"                }~2 = 10033
        ;"                  }~0 = +
        ;"                }~4 = 12345
        ;"                  }~0 = +
        ;"Output: IENS will be changed
        ;"Result: 1=OkToContinue, 0=Abort

        IF $DATA(TMGDEBUG)#10=0 NEW TMGDEBUG SET TMGDEBUG=0
        NEW cOKToCont SET cOKToCont=1
        NEW cAbort SET cAbort=0
        NEW cParentIENS SET cParentIENS="ParentIENS"

        NEW result SET result=1
        NEW S SET S=""

        IF $DATA(IENS)#10=0 SET result=0 GOTO CvIENSQ
        IF $DATA(MarkerArray)=0 SET result=0 GOTO CvIENSQ

        NEW I SET I=1
        FOR  DO  QUIT:(I=-1)
        . NEW Part,RecMarker
        . SET Part=$PIECE(IENS,",",I)
        . IF Part="" SET I=-1 QUIT
        . SET RecMarker=+$TRANSLATE(Part,"?+","")
        . NEW tS SET tS=$GET(MarkerArray(RecMarker),Part)
        . SET S=S_tS_","
        . SET I=I+1

        SET IENS=S

CvIENSQ
        QUIT result


SetupFDA(Data,FDA,parentIENS,SrchType,MarkNum,MsgArray,Minimal,RecNum)
        ;"Purpose: to transfer from Data format to FDA format
        ;"Input: Data - Data array should be in format output from GetRInfo
        ;"         FDA -- SHOULD BE PASSED BY REFERENCE (to receive results)
        ;"         parentIENS -- initial IENS.. the IENS of any PARENT record, or "" IF no parent record
        ;"         SrchType -- should be "?", "+", or "?+"
        ;"         MarkNum -- -- SHOULD BE PASSED BY REFERENCE.  A variable to ensure
        ;"                "?X" search term always has unique number.  On first call, should=0
        ;"         MsgArray -- SHOULD BE PASSED BY REFERENCE.  An array that can accept
        ;"                messages back from function.
        ;"                -- One such type of message is a list of needed hackWRITEs.
        ;"                        Format as follows:
        ;"                        MsgArray(cHack,0,Entries)=2
        ;"                        MsgArray(cHack,1)="^VA(;200;?+1;.01;SomeData"
        ;"                        MsgArray(cHack,1,cFlags)="H"
        ;"                        MsgArray(cHack,2)="^VA(;200;?+1;.02;SomeMoreData"
        ;"                        MsgArray(cHack,2,cFlags)="H"
        ;"                        i.e.         MsgArray(cHack,0,Entries)=Number of Entries
        ;"                                MsgArray(cHack,n) = Global;FILENumber;IENS;FieldNum;Data
        ;"                                MsgArray(n,cFlags)=User specified Flags for field.
        ;"                 -- MsgArray(cRef,SubFILENumber)=Reference to Part of Data that created this.
        ;"                        MsgArray(*)
        ;"                        }~cRef
        ;"                          }~1234.21 = "Data(6,".07")
        ;"                          }~1234.2101 = "Data(6,".07",2,".04")
        ;"         Minimal -- OPTIONAL.  1=fill only .01 fields and subfile .01 fields
        ;"         RecNum -- OPTIONAL.  If FDA is to be SETup such that data is put into
        ;"                a specified record number, put that number here.
        ;"              !!! Note: I believe this is used erroneously here.  A record number
        ;"              is not specified in the FDA.  For calls to UPDATE^DIE to a specific
        ;"              record number, the FDA should have an IENS that is like "+1,", and then
        ;"              put the desired record number into the IEN_ROOT, like TMGIEN(1)=1234
        ;"              with the "1" matching the "1" in TMGIEN(1)
        ;"Output: FDA is changed IF passed by reference.
        ;"Returns: If should continue execution:  1=OK to continue.  0=abort.

        ;"Note: input Data array will be formated like this:
        ;"                Data(0,cFILE)="1234.1" <-- "NEW PERSON" Note conversion
        ;"                Data(0,cFILE,cGlobal)="^DIC(200)"  <-- note, NOT "^DIC(200,"
        ;"                Data(0,cRecNum)=2  <-- only IF user-specified.
        ;"                Data(0,cEntries)=1
        ;"                Data(1,".01")="MyData1"
        ;"                Data(1,".01",cMatchValue)="MyData1"
        ;"                Data(1,".02")="Bill"
        ;"                Data(1,".02",cMatchValue)="John"
        ;"                Data(1,".03")="MyData3"
        ;"                Data(1,".04")="MyData4"
        ;"                Data(1,".06")="MyData5"  <-- note "NAME" was converted to ".06"
        ;"                Data(1,".07",0,cEntries)=2    <-- "ITEM" converted to ".07"
        ;"                Data(1,".07",1,".01")="SubEntry1"
        ;"                Data(1,".07",1,".02")="SE1"
        ;"                Data(1,".07",1,".03")="'Some Info'"
        ;"                Data(1,".07",2,".01")="SubEntry2"
        ;"                Data(1,".07",2,".02")="SE2"
        ;"                Data(1,".07",2,".04",0,cEntries)=1    ;"TEXT converted to .04
        ;"                Data(1,".07",2,".04",1,".01")="JD"
        ;"                Data(1,".07",2,".04",1,".02")="DOE,JOHN"
        ;"                ADDENDUM
        ;"                Data(1,".01",cFlags)=any flags specified for given field.
        ;"                        only present IF user specified.

        ;"Resulting FDA will look like this.
        ;"        i.e. FDA(1234,"?+1,10024,",.01)="MyData1"
        ;"        i.e. FDA(1234,"?+1,10024,",.02)="Bill"
        ;"        i.e. FDA(1234,"?+1,10024,",.03)="MyData3"
        ;"        i.e. FDA(1234,"?+1,10024,",.04)="MyData4"
        ;"        i.e. FDA(1234,"?+1,10024,",.06)="MyData5"
        ;"        i.e. FDA(1234.21,"?+2,?+1,10024,",.01)="SubEntry1"
        ;"        i.e. FDA(1234.21,"?+2,?+1,10024,",.02)="SE1"
        ;"        i.e. FDA(1234.21,"?+2,?+1,10024,",.03)="'Some Info'"
        ;"        i.e. FDA(1234.21,"?+3,?+1,10024,",.01)="SubEntry2"
        ;"        i.e. FDA(1234.21,"?+3,?+1,10024,",.02)="SE2"
        ;"        i.e. FDA(1234.21,"?+3,?+1,10024,",.03)="'Some Info'"
        ;"        i.e. FDA(1234.2101,"?+4,?+3,?+1,10024,",.01)="JD"
        ;"        i.e. FDA(1234.2101,"?+4,?+3,?+1,10024,",.02)="DOE,JOHN"
        ;"(OR... reformat of above)
        ;"        FDA(*)
        ;"        }~1234
        ;"          }~?+1,10024
        ;"            }~.01 = MyData1
        ;"            }~.02 = Bill
        ;"            }~.03 = MyData3
        ;"            }~.04 = MyData4
        ;"            }~.06 = MyData5
        ;"        }~1234.21
        ;"          }~?+2,?+1,10024
        ;"            }~.01 = SubEntry1
        ;"            }~.02 = SE1
        ;"            }~.03 = 'Some Info'
        ;"          }~?+3,?+1,10024
        ;"            }~.01 = SubEntry2
        ;"            }~.02 = SE2
        ;"            }~.03 = 'Some Info'
        ;"        }~1234.2101
        ;"          }~?+4,?+3,?+1,10024
        ;"            }~.01 = JD
        ;"            }~.02 = DOE,JOHN

        ;"MsgArray will hold the following
        ;"        MsgArray(*)
        ;"        }~"H"
        ;"        }~"Ref"
        ;"          }~1234.21 = "Data(6,".07")
        ;"          }~1234.2101 = "Data(6,".07",2,".04")

        IF $DATA(TMGDEBUG)#10=0 NEW TMGDEBUG SET TMGDEBUG=0
        NEW cOKToCont SET cOKToCont=1
        NEW cAbort SET cAbort=0
        NEW cFILE SET cFILE="FILE"                                ;"FILE"
        NEW cHack SET cHack="H"
        NEW cFlags SET cFlags="FLAGS"                                ;"Flags"
        NEW cEntries SET cEntries="Entries"
        NEW cNoOverWRITE SET cNoOverWRITE="N"

        NEW result SET result=cOKToCont
        NEW index
        NEW FieldNum
        NEW FILENumber
        NEW SubMarkNum SET SubMarkNum=0
        NEW IENS SET IENS=""
        IF $GET(RecNum)="" KILL RecNum

        SET FILENumber=$GET(Data(0,cFILE))
        IF +FILENumber=0 GOTO SFDAQ
        SET index=$ORDER(Data(0))
        ;"Cycle through all entries (i.e. 1, 2, 3)
        FOR  DO  QUIT:(index="")!(result=cAbort)
        . SET FieldNum=$ORDER(Data(index,""))
        . ;"Cycle through all fields (i.e. .01, .02, ,1223)
        . FOR  DO  QUIT:(FieldNum="")!(result=cAbort)
        . . NEW NextFieldNum SET NextFieldNum=$ORDER(Data(index,FieldNum))
        . . IF ($GET(Data(index,FieldNum,cFlags))[cNoOverWRITE)&(SrchType["?") DO  QUIT
        . . . SET FieldNum=NextFieldNum
        . . IF (FieldNum=.01)!(IENS="") do
        . . . IF $DATA(RecNum)#10=0 do
        . . . . SET MarkNum=+$GET(MarkNum)+1
        . . . . SET IENS=SrchType_MarkNum_","_$GET(parentIENS)
        . . . ELSE  do
        . . . . SET IENS=$GET(RecNum)_","_$GET(parentIENS)
        . . IF $GET(Data(index,FieldNum,cFlags))[cHack DO   ;"HACK PROCESSING
        . . . ;"Load hacks into a message array for later processing
        . . . NEW NumHacks SET NumHacks=$GET(MsgArray(cHack,0,cEntries))+1
        . . . NEW Entry SET Entry=Data(index,FieldNum)
        . . . IF $GET(Data(index,FieldNum,cFlags))[cEncrypt do
        . . . . SET Entry=$$EN^XUSHSH(Entry) ;"encrypt data
        . . . NEW Global SET Global=$GET(Data(0,cFILE,cGlobal))
        . . . IF Global="" DO  QUIT
        . . . . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Unable to local global name for file")
        . . . . SET result=cAbort
        . . . SET MsgArray(cHack,NumHacks)=Global_";"_FILENumber_";"_IENS_";"_FieldNum_";"_Entry
        . . . SET MsgArray(cHack,NumHacks,cFlags)=Data(index,FieldNum,cFlags)
        . . ELSE  IF $DATA(Data(index,FieldNum,0,cEntries)) DO  ;"SUB-FILE PROCESSING
        . . . NEW tempData MERGE tempData=Data(index,FieldNum)
        . . . NEW SubFILENum SET SubFILENum=$GET(Data(index,FieldNum,0,cFILE),0)
        . . . SET MsgArray(cRef,SubFILENum)=$name(Data(index,FieldNum))
        . . . ;"call self recursively to handle subfile.
        . . . NEW SubMarkNum SET SubMarkNum=MarkNum
        . . . SET result=$$SetupFDA(.tempData,.FDA,IENS,SrchType,.SubMarkNum,.MsgArray,.Minimal)
        . . . IF SubMarkNum>MarkNum SET MarkNum=SubMarkNum
        . . ELSE  DO  ;"THE-USUAL-CASE PROCESSING
        . . . IF (FieldNum=.01)!($GET(Minimal)'=1) do
        . . . . NEW ts SET ts="Setting: FDA("_FILENumber_","""_IENS_""","_FieldNum_")="_$GET(Data(index,FieldNum))
        . . . . SET FDA(FILENumber,IENS,FieldNum)=$GET(Data(index,FieldNum))
        . . . IF $DATA(Data(index,FieldNum,"WP")) do
        . . . . MERGE FDA(FILENumber,IENS,FieldNum,"WP")=Data(index,FieldNum,"WP")
        . . . . ;"IF $GET(TMGDEBUG)>0 do
        . . . . ;". NEW ts SET ts="Setting: FDA("_FILENumber_","""_IENS_""","_FieldNum_")="
        . . . . ;". ;"NOTE: the "TMGFDA" MUST!! match the FDA name passed to UPDATE^DIE, FILE^DIE
        . . . . ;". SET ts=ts_$name(TMGFDA(FILENumber,IENS,FieldNum,"WP"))
        . . . . ;"NOTE: the "TMGFDA" MUST!! match the FDA name passed to UPDATE^DIE, FILE^DIE
        . . . . SET FDA(FILENumber,IENS,FieldNum)=$name(TMGFDA(FILENumber,IENS,FieldNum,"WP"))
        . . SET FieldNum=NextFieldNum
        . SET index=$ORDER(Data(index))

SFDAQ
        QUIT result



OverwriteRec(RecNum,Data)
        ;"Purpose: To stuff data from data array into record specified by RecNum.
        ;"        This function will not directly put any data into subfiles, but will
        ;"        call UploadData to handle this.
        ;"Input: RecNum -- the record number (as returned by GetRecMatch) to put data into
        ;"       Data - Should be in format output from GetRInfo
        ;"Output: database will be modified by changing record
        ;"Returns: If should continue execution:  1=OK to continue.  0=abort.

        IF $DATA(TMGDEBUG)#10=0 NEW TMGDEBUG SET TMGDEBUG=0
        IF $DATA(cOKToCont)#10=0 NEW cOKToCont SET cOKToCont=1
        IF $DATA(cAbort)#10=0 NEW cAbort SET cAbort=0
        NEW cParentIENS SET cParentIENS="ParentIENS"

        NEW result SET result=cOKToCont
        NEW Flags
        NEW FILENumber,FieldNum,SubFILENum
        NEW FieldFlags
        NEW tmgFDA,TMGFDA,TMGMsg
        NEW index
        NEW IENS SET IENS=$GET(Data(0,cParentIENS))
        NEW FDAIndex
        NEW MarkerArray
        NEW MsgArray

        IF $GET(RecNum)=0 SET result=cAbort GOTO OWQuit

        SET FILENumber=Data(0,cFILE)
        SET Flags="KE" ;"E=External format values; K=Func locks file during use.

        SET IENS=$GET(Data(0,cParentIENS))

        NEW MarkNum SET MarkNum=0
        SET result=$$SetupFDA(.Data,.tmgFDA,IENS,"?",.MarkNum,.MsgArray,0,RecNum)
        IF result=cAbort GOTO OWQuit
        SET FILENum=$GET(Data(0,cFILE),0) IF FILENum=0 SET result=cAbort GOTO OWQuit

        ;"//kt 1/5/25 original Why 'do'? --> IF $DATA(tmgFDA)=0 DO  GOTO OWPast  ;"This can happen with single records with NoOverWRITE flag
        IF $DATA(tmgFDA)=0 GOTO OWPast  ;"This can happen with single records with NoOverWRITE flag

        SET FDAIndex=FILENum
        KILL TMGFDA
        MERGE TMGFDA(FDAIndex)=tmgFDA(FDAIndex)
        ;
        SET Flags="E"  ;"E=External format values
        ;
        ;"======================================================
        ;"Call FILE^DIE
        ;"======================================================
        IF $DATA(TMGFDA)=0 SET result=cAbort QUIT
        do
        . NEW $ETRAP SET $ETRAP="do ErrTrp^TMGDBAPI"
        . SET ^TMP("TMG",$J,"ErrorTrap")=result
        . SET ^TMP("TMG",$J,"Caller")="FILE^DIE"
        . DO FILE^DIE(Flags,"TMGFDA","TMGMsg")
        . SET result=^TMP("TMG",$J,"ErrorTrap")
        . KILL ^TMP("TMG",$J,"ErrorTrap")
        . KILL ^TMP("TMG",$J,"Caller")
        ;"======================================================
        ;"======================================================
        ;"
        IF $DATA(TMGMsg("DIERR")) DO  GOTO OWQuit
        . DO SHOWDIER^TMGDEBU2(.TMGMsg,.PriorErrorFound)
        . SET result=cAbort

        IF result=cAbort GOTO OWQuit

        KILL tmgFDA(FDAIndex)
        SET FDAIndex=""  ;"I don't want to loop through rest of tmgFDA, will handle below.

OWPast
        SET result=$$HandleHacksArray^TMGDBAPI(.MsgArray)
        IF result=cAbort DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error in writing record") GOTO OWQuit

        ;"Now we handle possible subfile entries.  Info regarding these is in MsgArray
        IF $DATA(MsgArray(cRef))'=0 do
        . SET SubFILENum=$ORDER(MsgArray(cRef,""))
        . FOR  DO  QUIT:(+SubFILENum=0)!(result=cAbort)
        . . IF +SubFILENum=0 QUIT
        . . NEW SubData,DataP
        . . SET DataP=MsgArray(cRef,SubFILENum)
        . . MERGE SubData=@DataP
        . . SET SubData(0,cParentIENS)=RecNum_","_IENS
        . . SET result=$$UploadData^TMGDBAPI(.SubData)
        . . SET SubFILENum=$ORDER(MsgArray(cRef,SubFILENum))

OWQuit
        QUIT result


GETFNAME(FILENumber)    ;"updated code avail in TMGXMLT2
        ;"Purpose: Convert a file number into a file name
        ;"Input: The number of a file
        ;"Result: The file name, or "" IF not found.

        NEW result SET result=""

        IF $GET(FILENumber)=0 GOTO GtFNmDone

        SET result=$GET(^DIC(FILENumber,0))
        IF (result="")&(FILENumber[".") do
        . SET result=$GET(^DD(FILENumber,0))
        SET result=$PIECE(result,"^",1)
GtFNmDone
        QUIT result


EXPFNAME(FILENUM)  ;"(NOTE: copy also at EXPFNAM2^TMGRPC3G)
        ;"Purpose: Convert file number info expanded name, PARENTNAME:SUBNAME:...
        ;"Input: FILENUM -- FILE or subfile number
        ;"Result: If FILENUM is a normal file number, then simply file name is returned
        ;"        IF FILENUM is a subfile number, then an expanded name is returned:
        ;"            GRANDPARENTFILENAME:PARENTILENAME:FILENAME
        ;"            For subfiles, then the name of the file will be considered the
        ;"              field name in the parent file.
        NEW RESULT SET RESULT=""
        NEW NUM SET NUM=FILENUM
        NEW NAME SET NAME=""
        NEW DONE SET DONE=0
        FOR  DO  QUIT:DONE
        . IF RESULT'="" SET RESULT=":"_RESULT
        . IF $DATA(^DIC(NUM)) DO  QUIT
        . . SET RESULT=$PIECE($GET(^DIC(NUM,0)),"^",1)_RESULT
        . . SET DONE=1
        . IF $DATA(^DD(NUM,0)) DO
        . . NEW SUBNAME SET SUBNAME=$PIECE($GET(^DD(NUM,0)),"^",1)
        . . SET SUBNAME=$PIECE(SUBNAME," SUB-FIELD",1)
        . . SET RESULT=SUBNAME_RESULT
        . . SET NUM=+$GET(^DD(NUM,0,"UP"))
        . ELSE  DO  QUIT
        . . SET RESULT="??"_RESULT
        . . SET DONE=1
        QUIT RESULT
        ;

GetFldName(FILE,FieldNumber)  ;"Updated code at GTFLDNAM^TMGXMLT2
        ;"Purpose: Convert a field number into a field name
        ;"Input: FILE -- name or number of file
        ;"         FieldNumber -- the number of the field to convert
        ;"Result: The field name, or "" IF not found.

        NEW result SET result=""
        NEW array
        DO GFLDINFO^TMGDBAP3(.FILE,.FieldNumber,"array","LABEL")
        SET result=$GET(array("LABEL"))

GFldNmDone
        QUIT result


GetFldList(FILE,pArray)  ;"Updated code avail in GTFLDLST^TMGXMLT2
        ;"Purpose: Get list of all fields for a file.
        ;"Input: FILE -- File name or number to look query.  May be a sub file number
        ;"        pArray -- pointer to (i.e. name of) array to put data into
        ;"                      Any preexisting data in pArray will be killed.
        ;"Output: Array will be fille with info like this:
        ;"     example: Array(.01)=""<---    shows that field .01 exists
        ;"                  Array(1)=""   <---    shows that field 1 exists
        ;"                  Array(2)=""   <---    shows that field 2 exists
        ;"Results:  1=OK to continue.  0=error

        NEW result SET result=1
        NEW FILENumber,FILEName
        IF ($GET(FILE)="")!($GET(pArray)="") SET result=0 GOTO GFdLDone
        KILL @pArray

        IF +FILE=FILE do
        . SET FILENumber=FILE
        . SET FILEName=$$GETFNAME(FILE)
        ELSE  do
        . SET FILEName=FILE
        . SET FILENumber=$$GETFNUM^TMGDBAP3(FILE)
        IF FILENumber'>0 DO  GOTO GFdLDone
        . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error: Requested file, "_FILE_", doesn't exist.")
        . SET result=0

        NEW index SET index=$ORDER(^DD(FILENumber,0))
        IF +index>0 FOR  DO  QUIT:(+index'>0)
        . SET @pArray@(index)=""
        . SET index=$ORDER(^DD(FILENumber,index))

GFdLDone
        QUIT result


SetupFileNum(Data)
        ;"Purpose: To Ensure that Data(0,cFILE) contains valid file number
        ;"Input: Data-- should be passed by reference, Array SETup by GetRInfo
        ;"        Specifically, Data(0,cFILE) should have file name OR number
        ;"Output: Data is changed:
        ;"          Data(0,cFILE)=FILENumber
        ;"          Data(0,cFILE,cGlobal)=Global reference name  ;i.e. "^VA(200)"
        ;"          Data(0,cFILE,cGlobal,cOpen)=Open Global reference name  ;i.e. "^VA(200,"
        ;"Returns: If should continue execution:  1=OK to continue.  0=abort.

        IF $DATA(TMGDEBUG)#10=0 NEW TMGDEBUG SET TMGDEBUG=0
        NEW cOKToCont SET cOKToCont=1
        NEW cAbort SET cAbort=0
        NEW cFILE SET cFILE="FILE"                                ;"FILE"
        NEW cGlobal SET cGlobal="GLOBAL"
        NEW cOpen SET cOpen="OPEN"

        NEW result SET result=cOKToCont
        NEW FILENumber,FILEName,FILE

        SET FILE=$GET(Data(0,cFILE)," ")
        IF +FILE'=0 DO  GOTO CKFileNum
        . SET FILENumber=FILE
        SET FILEName=FILE
        IF FILEName=" " DO  GOTO SFNDone
        . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"No file specifier (either name or number) given!")
        . SET result=cAbort ;"0=Abort

        ;"Note: I could replace this code with GETFNUM^TMGDBAP3(FILEName)
        ;"----------------
        SET DIC=1  ;"FILE 1=Global file reference (the file listing info for all files)
        SET DIC(0)="M"
        SET X=FILEName   ;"i.e. "AGENCY"
        DO ^DIC  ;"lookup filename  Result comes back in Y ... i.e. "4.11^AGENCY"
        SET FILENumber=$PIECE(Y,"^",1)
        ;"----------------

CKFileNum
        SET Data(0,cFILE)=FILENumber
        IF FILENumber=-1 DO  GOTO SFNDone
        . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Unable to locate file specified as #"_FILENumber_" or '"_FILEName_"'.")
        . SET result=cAbort ;"0=Abort
        IF $$VFILE^DILFD(FILENumber)=0 DO  GOTO SFNDone
        . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Error: Requested file, #"_FILENumber_", doesn't exist.")
        . SET result=cAbort ;"0=Abort

        SET Global=$GET(^DIC(FILENumber,0,"GL"),"INVALID")  ;"^DIC is file 1/FILE
        SET Data(0,cFILE,cGlobal,cOpen)=Global
        ;"Convert global form of ^VA(200,  into ^VA(200)
        NEW Len
        SET Len=$LENGTH(Global)
        IF $EXTRACT(Global,Len)="," do
        . SET $EXTRACT(Global,Len)=")"
        SET Data(0,cFILE,cGlobal)=Global

SFNDone
        QUIT result



RecFind(Params)
        ;"Purpose: To look through a file and find matching record
        ;"Input -- Params(cFILE)=FILE name or number
        ;"         Params(FieldNumber)=LookupValue
        ;"         Params(FieldNumber)=LookupValue
        ;"
        ;"        e.g.    Params(0,cFILE)="PERSON CLASS"
        ;"                Params(.01)="Physicians (M.D. and D.O.)"
        ;"                Params(1)="Physician/Osteopath"
        ;"                Params(2)="Family Practice"
        ;"
        ;"Note: Does not support searching based on subfile data.
        ;"Output -- (via results)
        ;"Result -- Returns record number file, OR 0 IF not found

        IF $DATA(cFILE)=0 NEW cFILE SET cFILE="FILE"
        IF $DATA(cEntries)=0 NEW cEntries SET cEntries="Entries"
        IF $DATA(cMatchValue)=0 NEW cMatchValue SET cMatchValue="MATCHVALUE"
        NEW result SET result=0
        NEW Data
        NEW RecNum
        NEW FieldNum

        SET Data(0,cFILE)=$GET(Params(0,cFILE))
        IF Data(0,cFILE)="" GOTO RFDone
        IF $$SetupFileNum(.Data)=0 GOTO RFDone
        SET Data(0,cEntries)=1

        SET FieldNum=$ORDER(Params(0))
        FOR  DO  QUIT:(+FieldNum=0)
        . IF +FieldNum=0 QUIT
        . SET Data(1,FieldNum,cMatchValue)=$GET(Params(FieldNum))
        . SET FieldNum=$ORDER(Params(FieldNum))

        IF $$GetRecMatch^TMGDBAPI(.Data,.RecNum)=0 GOTO RFDone
        SET result=RecNum

RFDone
        QUIT result



FLDCOMP(TestField,dbField,Type)
        ;"PURPOSE: To compare two fields and return a comparison code
        ;"INPUT: TestField -- User input to be tested (in "external format"). **Don't pass by Ref**
        ;"         dbField -- data from database to be tested. **Don't pass by Ref
        ;"         Type -- (Optional) The type of data being compared:
        ;"                "NORMAL" or "" -- Simple comparison carried out (i.e. 'IF A=B')
        ;"                "DATE" -- the two values are date/time values
        ;"                "SSNUM"-- the two values are social security numbers
        ;"                "SEX" -- the two values are Sex descriptors.
        ;"                "NUMBER" -- the two values are numbers
        ;"Results:
        ;"         return value = cConflict (0)  IF entries conflict
        ;"                i.e. TestField="John" vs dbField="Bill"
        ;"         return value = cFullMatch (1)  IF entries completely match
        ;"                ie. TestField="John" vs dbField="John"
        ;"                or TestField="" vs. dbField=""
        ;"         return value = cExtraInfo (2)  IF entries have no conflict, but TestField has extra info.
        ;"                i.e. TestField="John" vs. dbField=""
        ;"         return value = cdbExtraInfo (3) IF entries have no conflict, but dbField has extra info.
        ;"                i.e. TestField="" vs. dbField="12345"

        IF $DATA(cConflict)#10=0 NEW cConflict SET cConflict=0
        IF $DATA(cFullMatch)#10=0 NEW cFullMatch SET cFullMatch=1
        IF $DATA(cExtraInfo)#10=0 NEW cExtraInfo SET cExtraInfo=2
        IF $DATA(cdbExtraInfo)#10=0 NEW cdbExtraInfo SET cdbExtraInfo=3

        SET TestField=$GET(TestField)
        SET dbField=$GET(dbField)
        SET Type=$GET(Type)

        NEW result SET result=cConflict

        IF Type="DATE" do
        . SET TestField=$$IDATE^TIULC(TestField)
        . SET dbField=$$IDATE^TIULC(dbField)
        ELSE  IF Type="SSNUM" do
        . SET TestField=$TRANSLATE(TestField," /-","")  ;"Clean delimiters
        . IF TestField["P" SET TestField="P"
        . IF dbField["P" SET dbField="P"
        ELSE  IF Type="SEX" do
        . IF (TestField="m")!(TestField="M") SET TestField="MALE"
        . IF (TestField="f")!(TestField="F") SET TestField="FEMALE"

        IF TestField'="" do
        . IF ($DATA(dbField)#10=0)!($GET(dbField)="") SET result=cExtraInfo
        . ELSE  do
        . . IF Type="NUMBER" do
        . . . IF +TestField=+dbField SET result=cFullMatch
        . . ELSE  do
        . . . IF TestField=dbField SET result=cFullMatch
        ELSE  DO  ;"i.e. test case when TestField=""
        . IF $GET(dbfield)="" SET result=cFullMatch
        . ELSE  SET result=cdbExtraInfo

        QUIT result


EnsureWrite(FILE,Field,IENS,Value,Flags,MsgArray)
        ;"Purpose: To provide code to that will ensure that data is written to
        ;"         the database, but it will not add duplicate records IF the value
        ;"         is already there.  So a FIND is done first, and added IF not found.
        ;"         Note: This is primarly targeted at adding entries in a subfile.
        ;"Input: FILE -- FILE name or number
        ;"       Field -- Field name or number
        ;"       IENS -- standard IENS string describing IEN in FILE, or IEN path to subfile
        ;"       Value -- The value to be filed
        ;"       Flags -- Flags to be passed
        ;"       MsgArray -- PASS BY REFERENCE.  Messages to pass back out.
        ;"Results : 1=Writen OK, 0=Already present so not written, -1=error

        NEW result SET result=-1


        QUIT result



dbWrite(FDA,OverWRITE,TMGIEN,Flags,ErrArray)
        ;"Purpose: To provide a unified interface for writing a FDA to the database
        ;"Input:  FDA -- PASS BY REFERENCE.  A standard FDA structure. (won't be changed)
        ;"        OverWRITE -- specifies IF records already exist in database
        ;"                IF = 1, then FILE^DIE used to WRITE into pre-existing records
        ;"                IF = 0, then UPDATE^DIE used to WRITE NEW records
        ;"        TMGIEN (OPTIONAL)-- an array to receive back records added (only applies if
        ;"                OverWRITE=0)
        ;"                It can also be used to pass info to UPDATE^DIE recarding requested record numbers
        ;"        Flags (OPTIONAL) -- Flags to pass to UPDATE^DIE or FILE^DIE.
        ;"                      default is "E".  If "E" is not wanted, then pass a " "
        ;"        ErrArray (OPTIONAL) -- an OUT parameter to receive fileman "DIERR" results, IF any
        ;"Results --1 if OK, or  0 IF error

        MERGE ^TMG("TMP","EDDIE","FDA")=FDA  ;"TEMP!!

        SET OverWRITE=$GET(OverWRITE,0)
        NEW TMGFDA MERGE TMGFDA=FDA
        NEW TMGMsg
        NEW TMGFlags SET TMGFlags=$GET(Flags,"E")  ;"E=External values
        IF TMGFlags=" " SET TMGFlags=""
        IF (OverWRITE=1)&($GET(Flags)'="") SET TMGFlags=TMGFlags_"K"  ;"K means filer does file locking.

        NEW result SET result=1  ;"Default to success
        IF $DATA(TMGFDA)=0 SET result=-1 GOTO DBWDone

        SET ^TMP("TMG",$J,"ErrorTrap")=result
        ;"======================================================
        ;"======================================================
        IF OverWRITE=1 DO  ;"i.e. FILE^DIE used to WRITE into pre-existing records
        . NEW $ETRAP SET $ETRAP="do ErrTrp^TMGDBAPI"
        . SET ^TMP("TMG",$J,"Caller")="FILE^DIE"
        . DO FILE^DIE(TMGFlags,"TMGFDA","TMGMsg")
        ELSE  IF OverWRITE=0 DO  ;"i.e. UPDATE^DIE used to WRITE NEW records
        . NEW $ETRAP SET $ETRAP="do ErrTrp^TMGDBAPI"
        . SET ^TMP("TMG",$J,"Caller")="UPDATE^DIE"
        . DO UPDATE^DIE(TMGFlags,"TMGFDA","TMGIEN","TMGMsg")
        ;"======================================================
        ;"======================================================
        SET result=^TMP("TMG",$J,"ErrorTrap")
        KILL ^TMP("TMG",$J,"ErrorTrap")
        KILL ^TMP("TMG",$J,"Caller")

        IF $DATA(TMGMsg("DIERR")) do
        . ;"TMGDEBUG=-1 --> extra quiet mode
        . IF $GET(TMGDEBUG)>-1 DO SHOWDIER^TMGDEBU2(.TMGMsg,.PriorErrorFound)
        . SET result=0
        . MERGE ErrArray("DIERR")=TMGMsg("DIERR")

DBWDone
        QUIT result


DelIEN(FILE,RecNumIEN,ErrArray)
        ;"Purpose: To delete record# RecNumIEN from file FILE
        ;"Input: FILE -- FILE name or number to delete from
        ;"       RecNumIEN -- the IEN to delete
        ;"       ErrArray --OPTIONAL, PASS BY REFERENCE.
        ;"              an OUT parameter to receive fileman "DIERR" results, IF any
        ;"Output: will cause deletion from database
        ;"Results -- IF error occured
        ;"        cOKToCont (i.e. 1) IF no error
        ;"        cAbort (i.e. 0) IF error

        NEW TMGFDA,result
        SET result=0

        IF $GET(FILE)="" GOTO DIENDone
        IF +$GET(RecNumIEN)'>0 GOTO DIENDone
        IF +FILE'>0 SET FILE=$$GETFNUM^TMGDBAP3(FILE)

        SET TMGFDA(FILE,+RecNumIEN_",",.01)="@"
        SET result=$$dbWrite(.TMGFDA,1,,,.ErrArray)

DIENDone
        QUIT result


WriteWP(FILE,RecNumIEN,Field,TMGArray)
        ;"Purpose: To provide a shell around WP^DIE with error trap, error reporting
        ;"Note: This does not support subfiles or multiples.  Does not support appending
        ;"Input: FILE: a number or name
        ;"         RecNumIEN: The record number, in FILE, to use
        ;"        Field: a name or number
        ;"        TMGArray: The array that contains WP data.  Must be in Fileman acceptible format.
        ;"Results -- IF error occured
        ;"        cOKToCont (i.e. 1) IF no error
        ;"        cAbort (i.e. 0) IF error

        IF $DATA(TMGDEBUG)#10=0 NEW TMGDEBUG SET TMGDEBUG=0
        IF $DATA(cOKToCont)#10=0 NEW cOKToCont SET cOKToCont=1
        IF $DATA(cAbort)#10=0 NEW cAbort SET cAbort=0

        NEW IENS
        NEW TMGMsg
        NEW FILENumber,FieldNumber
        NEW result SET result=cAbort
        NEW TMGFlags SET TMGFlags="K"

        SET FILENumber=+$GET(FILE)
        IF FILENumber=0 SET FILENumber=$$GETFNUM^TMGDBAP3(.FILE)
        IF FILENumber=0 DO  GOTO WWPDone
        . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Can't convert file '"_$GET(FILE)_", to a number.")

        SET FieldNumber=$GET(Field)
        IF FieldNumber=0 SET FieldNumber=$$GTNUMFLD^TMGDBAP3(FILENumber,.Field)
        IF FieldNumber=0 DO  GOTO WWPDone
        . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"Can't convert field '"_$GET(Field)_", to a number.")

        IF +$GET(RecNumIEN)=0 DO  GOTO WWPDone
        . DO SHOWERR^TMGDEBU2(.PriorErrorFound,"No numeric record number supplied.")

        SET IENS=RecNumIEN_","

        do
        . ;"======================================================
        . ;"Call WP^DIE
        . ;"======================================================
        . NEW $ETRAP SET $ETRAP="do ErrTrp^TMGDBAPI"
        . SET ^TMP("TMG",$J,"ErrorTrap")=result
        . SET ^TMP("TMG",$J,"Caller")="WP^DIE"
        . DO WP^DIE(FILENumber,IENS,FieldNumber,TMGFlags,"TMGArray","TMGMsg")
        . SET result=^TMP("TMG",$J,"ErrorTrap")
        . KILL ^TMP("TMG",$J,"ErrorTrap")
        . KILL ^TMP("TMG",$J,"Caller")
        . ;"======================================================
        . ;"======================================================

        IF $DATA(TMGMsg("DIERR"))'=0 DO  GOTO WWPDone
        . DO SHOWDIER^TMGDEBU2(.TMGMsg,.PriorErrorFound)
        . SET result=cAbort

        SET result=cOKToCont

        ;"zbreak WWPDone

WWPDone
        QUIT result


ShowIfError(TMGMsg,PriorErrorFound)
        ;"Purpose: to show DIERR IF preesnt in pTMGMsg
        ;"Input: pTMGMsg -- PASS BY REFERENCE, holds message route, as SET up by Fileman
        ;"       PriorErrroFound -- OPTIONAL, a variable holding IF a prior error has been found
        ;"Output: 1 IF ERROR found, 0 otherwise

        NEW result SET result=0
        IF $DATA(TMGMsg("DIERR"))'=0 do
        . DO SHOWDIER^TMGDEBU2(.TMGMsg,.PriorErrorFound)
        . SET result=1
        QUIT result


DataImport(Info,ProgressFN)
        ;"Purpose: to provide a generic loading utility.
        ;"     Note: this is more specific than code found in DDMP.m
        ;"Assumptions: that all data for one record is found on one line, with a given
        ;"              number of columns for each field.
        ;"Input:  Info, an array with relevent info.  PASS BY REFERENCE
        ;"              Format as follows:
        ;"              Info("HFS DIR")=<directory name in HFS to load from>
        ;"              Info("HFS FILE")=<file name in HFS to load from>
        ;"              Info("DEST FILE")=<file name or number>
        ;"              Info(x)=field#  (or "IEN" IF data should be used to determine record number
        ;"              Info(x,"START")=starting column
        ;"              Info(x,"END")=ending column
        ;"      ProgressFN: optional.  If not "", then this will be XECUTED after each line
        ;"Result: 1 if OK to continue, 0 IF error

        ;"Note: input Data array will be formated like this:
        ;"                Data(0,cFILE)="1234.1" <-- "NEW PERSON" Note conversion
        ;"                Data(0,cFILE,cGlobal)="^DIC(200)"  <-- note, NOT "^DIC(200,"
        ;"                Data(0,cRecNum)=2  <-- only IF user-specified.
        ;"                Data(0,cEntries)=1
        ;"                Data(1,".01")="MyData1"
        ;"                Data(1,".01",cMatchValue)="MyData1"
        ;"                Data(1,".02")="Bill"
        ;"                Data(1,".02",cMatchValue)="John"
        ;"                Data(1,".03")="MyData3"
        ;"                Data(1,".04")="MyData4"
        ;"                Data(1,".06")="MyData5"  <-- note "NAME" was converted to ".06"
        ;"                Data(1,".07",0,cEntries)=2    <-- "ITEM" converted to ".07"
        ;"                Data(1,".07",1,".01")="SubEntry1"
        ;"                Data(1,".07",1,".02")="SE1"
        ;"                Data(1,".07",1,".03")="'Some Info'"
        ;"                Data(1,".07",2,".01")="SubEntry2"
        ;"                Data(1,".07",2,".02")="SE2"
        ;"                Data(1,".07",2,".04",0,cEntries)=1    ;"TEXT converted to .04
        ;"                Data(1,".07",2,".04",1,".01")="JD"
        ;"                Data(1,".07",2,".04",1,".02")="DOE,JOHN"
        ;"                ADDENDUM
        ;"                Data(1,".01",cFlags)=any flags specified for given field.
        ;"                        only present IF user specified.

        NEW cFILE SET cFILE="FILE"
        NEW cRecNum SET cRecNum="RECNUM"
        NEW result SET result=1

        NEW GREF SET GREF=$name(^TMP("TMG","DATAIMPORT",$J))
        NEW GREF1 SET GREF1=$name(@GREF@(1))  ;"I have to use this to load file
        KILL @GREF

        NEW result
        NEW dir SET dir=$GET(Info("HFS DIR"))
        NEW HFSfile SET HFSfile=$GET(Info("HFS FILE"))
        SET result=$$FTG^%ZISH(dir,HFSfile,GREF1,4)
        IF result=0 GOTO DIDone
        NEW file SET file=$GET(Info("DEST FILE"))
        IF +file=0 SET file=$$GETFNUM^TMGDBAP3(file)

        NEW index
        SET index=$ORDER(@GREF@(""))
        FOR  DO  QUIT:(+index=0)!(result=0)
        . NEW RecData,FDA
        . SET RecData(0,cFILE)=file
        . NEW line SET line=$GET(@GREF@(index))
        . NEW fields SET fields=$ORDER(Info(""))
        . NEW IEN SET IEN=""
        . FOR  DO  QUIT:(+fields=0)!(result=0)
        . . NEW fieldNum SET fieldNum=$GET(Info(fields)) ;"could be number or 'IEN'
        . . NEW oneField
        . . SET oneField=$EXTRACT(line,$GET(Info(fields,"START")),$GET(Info(fields,"END")))
        . . SET oneField=$$Trim^TMGSTUTL(oneField)
        . . IF fieldNum="IEN" do
        . . . SET RecData(0,cRecNum)=fieldNum
        . . . SET IEN=fieldNum
        . . ELSE  do
        . . . SET RecData(1,fieldNum)=oneField
        . . SET fields=$ORDER(Info(fields))
        . NEW MarkNum SET MarkNum=0
        . NEW MsgArray
        . SET result=$$SetupFDA(.RecData,.FDA,,"+",.MarkNum,.MsgArray,IEN)
        . IF result=0 QUIT
        . SET result=$$dbWrite(.FDA,0,," ")
        . IF result=0 QUIT
        . IF $GET(ProgressFN)'="" do
        . . NEW $ETRAP SET $ETRAP="w ""??Progress function -- error trapped??"",!"
        . . xecute ProgressFN
       . SET index=$ORDER(@GREF@(index))

DIDone
        KILL @GREF
        QUIT result


Set1(FILE,IEN,Field,Value,Flag)
        ;"Purpose: to be the reverse of GET1^DIQ (i.e. a SETter instead of a getter)
        ;"         It will SET the value for 1 field in 1 record in 1 file.
        ;"         Note: only to be used in existing files.
        ;"Input: FILE -- the Filename or number
        ;"       IEN -- the record number to SET into
        ;"       Field -- the field name or number
        ;"       Value -- the value to SET it to (WP not currently supported)
        ;"       Flag -- OPTIONAL.  Combinations of below:
        ;"              'I' -- values are in internal format
        ;"              'E' -- values are in external format  (this is the DEFAULT)
        ;"Results: 1 if OKtoCont, 0 IF error

        NEW FILENumber,FieldNumber
        NEW result SET result=0 ;"default to error

        ;"NEW tempDebug SET tempDebug=$GET(TMGDEBUG)
        ;"SET TMGDEBUG=-1 ;"Extra quiet mode

        IF $$SETFFNUM^TMGDBAP3(.FILE,.Field,.FILENumber,.FieldNumber)=0 GOTO S1Done
        IF (+FILENumber=0)!(+FieldNumber=0) GOTO S1Done
        IF ($GET(Value)="")!(+IEN=0) GOTO S1Done

        NEW result SET result=1 ;"default to success.

        NEW TMGFDA,FMFlag,TMGMSG
        SET FMFlag="E"
        IF $GET(Flag)["I" SET FMFlag=""
        SET FMFlag=FMFlag_"K"
        SET TMGFDA(FILENumber,IEN_",",FieldNumber)=Value
        DO FILE^DIE(FMFlag,"TMGFDA","TMGMSG")
        IF $DATA(TMGMSG("DIERR"))'=0 DO  GOTO S1Done
        . DO SHOWDIER^TMGDEBU2(.TMGMSG,.PriorErrorFound)

S1Done
        ;"SET TMGDEBUG=tempDebug
        QUIT result


GetValidInput(FILE,Field)
        ;"Purpose: Gets a valid input for field in file, asking user from console
        ;"Input: FILE: File number or name of file to use
        ;"       Field: Field number or name in file.
        ;"Results: returns valid input, or ""

        NEW FILENum,FldNum
        NEW DIR,X,Y
        SET Y=""

        SET FILENum=+$GET(FILE)
        IF FILENum=0 SET FILENum=$$GETFNUM^TMGDBAP3(.FILE)
        IF FILENum=0 GOTO GVIDone

        SET FldNum=$GET(Field)
        IF FldNum=0 SET FldNum=$$GTNUMFLD^TMGDBAP3(FILENum,.Field)
        IF FldNum=0 GOTO GVIDone

        SET DIR(0)=FILENum_","_FldNum
        DO ^DIR WRITE !
GVIDone
        QUIT Y

FIENS2Root(FIENS)  ;"depreciated
        ;"Purpose: to convert a Files^IENS string into a root reference
        ;"Input: FIENS: format: FILENumber^StandardIENS
        ;"Output: A global root in open format
        QUIT


GetRef(file,IENS,field)
        ;"Purpose: to return the global reference for a given record
        ;"Input: file -- FILE or subfile number
        ;"       IENS -- an IEN, or an IENS for record
        ;"       field -- OPTIONAL.
        ;"Results:  IF field is NOT supplied, or
        ;"              OPEN global ref
        ;"          IF field IS supplied
        ;"              CLOSED global ref@piece
        ;"              e.g.  ^TMG(22706.9,3,2,IEN,0)@1  <-- note 'IEN' placeholder

        ;"Note: This function really needs to be fleshed out some more...
        ;"Note: this only will work for normal files, or subfiles ONE (1) level deep...

        NEW ref SET ref=""
        NEW parentFILE SET parentFILE=$$IsSubFile^TMGDBAPI(file)
        IF parentFILE=0 GOTO GRF1 ;"handle non-subfiles separately.

        SET fieldInParent=$PIECE(parentFILE,"^",2)
        SET ref=$GET(^DIC(+parentFILE,0,"GL"))
        NEW IENinParent SET IENinParent=$PIECE(IENS,",",2)
        SET ref=ref_IENinParent_","
        NEW storeLoc SET storeLoc=$PIECE($GET(^DD(+parentFILE,fieldInParent,0)),"^",4)
        ;"Note: works only with storeLoc in Node;Piece format... not all fields follow this...
        SET ref=ref_+storeLoc_","
        NEW IENinSubRec SET IENinSubRec=$PIECE(IENS,",",1)
        IF IENinSubRec="" SET IENinSubRec="IEN"
        SET ref=ref_IENinSubRec_","

        IF $GET(field)="" GOTO GRF2 ;"done
        SET storeLoc=$PIECE($GET(^DD(file,field,0)),"^",4)
        SET ref=ref_+storeLoc_")@"_$PIECE(storeLoc,";",2)
        GOTO GRF2

GRF1
        SET ref=$GET(^DIC(file,0,"GL"))
        SET ref=ref_+IENS_","
        IF $GET(field)="" GOTO GRF2 ;"done
        NEW storeLoc SET storeLoc=$PIECE($GET(^DD(file,field,0)),"^",4)
        SET ref=ref_+storeLoc_")@"_$PIECE(storeLoc,";",2)
        ;"Note: works only with storeLoc in Node;Piece format... not all fields follow this...
GRF2
        QUIT ref


        
TrimFDA(FDA,Quiet)
        ;"Purpose: To take an FDA, and compare it to data already present in the
        ;"         record specified by the FDA.  If any values already in the record
        ;"         match those in the FDA, then those entries will be removed from the
        ;"         FDA array.
        ;"Input: FDA -- PASS BY REFERENCE.  A standard Fileman FDA.
        ;"       Quiet -- OPTIONAL.  If 1, then error messages will be supressed
        ;"              (These would be messages generated on READING existing
        ;"              data, not writing NEW data.)
        ;"              default value=1
        ;"Output: Values from FDA may be removed.
        ;"Results: final IENS (i.e. '+1,3,' --> '5,3,'  IF prev value found)
        ;"Note: match will be made base on INTERNAL, or EXTERNAL forms
        ;"Note: Fields should be specified by numbers, NOT NAMES.

        NEW tempIENS SET tempIENS=""
        IF $DATA(FDA)'>0 GOTO TFDDone
        NEW TMGDATA,TMGMSG
        NEW file,IENS
        SET file=$ORDER(FDA(""))
        SET IENS=$ORDER(FDA(file,""))
        SET tempIENS=IENS
        SET Quiet=$GET(Quiet,1)

        NEW fieldsS SET fieldsS=""
        NEW field SET field=""
        FOR  SET field=$ORDER(FDA(file,IENS,field)) QUIT:(field="")  do
        . SET fieldsS=fieldsS_field_";"

        NEW parentFILE SET parentFILE=$$IsSubFile^TMGDBAPI(file)
        IF parentFILE=0 GOTO TFD0 ;"handle non-subfiles separately.

        ;"e.g.  FDA(22706.9001,"+1,3",.01)=1
        ;"      FDA(22706.9001,"+1,3",.02)=2
        ;"Note: The .01 field is used to find a matching subrecord, which is then
        ;"      check for preexisting data.  If multiple matches for .01 are found,
        ;"      then the process is aborted, and the FDA will NOT BE TRIMMED.

        SET $PIECE(tempIENS,",",1)=""  ;"leave first piece blank in IENS
        NEW value SET value=$GET(FDA(file,IENS,.01))

        ;"NEW i FOR i=1:1:$LENGTH(fieldsS,",") DO  ;"append 'E' to each field number
        ;". NEW field SET field=$PIECE(fieldsS,";",i)
        ;". SET field=field_"E"
        ;". SET $PIECE(fieldsS,";",i)=field
        ;"
        ;"NEW TMGFIND
        ;"
        ;"I can't get this part to work... so will work around
        ;"do FIND^DIC(file,tempIENS,fieldsS,"BMU",value,"*",,,,"TMGFIND","TMGMSG")
        ;"do SHOWDIER^TMGDEBU2(.TMGMSG)
        ;"IF +$GET(TMGFIND(0))'=1 GOTO TFDDone  ;"abort
        ;"MERGE TMGDATA(file,IENS)=TMGDATA("ID",1)
        ;"GOTO TFD1

        NEW ref SET ref=$$GetRef(file,tempIENS,.01)  ;"returns ref with 'IEN' built in...
        NEW ref2 SET ref2=$$CREF^DILF($PIECE(ref,"IEN",1))
        NEW ref3 SET ref3=$PIECE(ref,"@",1)
        NEW p SET p=$PIECE(ref,"@",2)
        NEW found SET found=0
        NEW IEN SET IEN=0
        FOR  SET IEN=$ORDER(@ref2@(IEN)) QUIT:(+IEN'>0)!(found>0)  do
        . NEW valueFound SET valueFound=$PIECE($GET(@ref3),"^",p)
        . IF valueFound=value SET found=IEN
        IF found=0 SET tempIENS=IENS GOTO TFDDone
        SET tempIENS=found_tempIENS
TFD0
        DO GETS^DIQ(file,tempIENS,fieldsS,"EI","TMGDATA","TMGMSG")
        IF 'Quiet DO SHOWDIER^TMGDEBU2(.TMGMSG)

TFD1
        FOR  SET field=$ORDER(FDA(file,IENS,field)) QUIT:(field="")  do
        . NEW found SET found=0
        . NEW FDAvalue SET FDAvalue=$GET(FDA(file,IENS,field))
        . IF $GET(TMGDATA(file,tempIENS,field,"I"))=FDAvalue SET found=1
        . IF $GET(TMGDATA(file,tempIENS,field,"E"))=FDAvalue SET found=1
        . IF (FDAvalue="@")&($DATA(TMGDATA(file,tempIENS,field))=0) SET found=1
        . IF found=1 KILL FDA(file,IENS,field)
        GOTO TFDDone
TFDDone
        QUIT tempIENS



GetPtrsOUT(FILE,Info)
        ;"Purpose: to get a list of pointers out from the file.
        ;"Input: FILE -- FILE Name or Number of file to investigate
        ;"       Info -- PASS BY REFERENCE.  An OUT PARAMETER.  Format:
        ;"            Info(Field#)=PointedToFileNum
        ;"            Info(Field#,"GL")=an open global ref to pointed-to file
        ;"results: none

        IF $GET(FILE)="" GOTO GPODone
        IF +FILE'=FILE SET FILE=$$GETFNUM^TMGDBAP3(FILE)
        NEW field SET field=0
        NEW done SET done=0
        FOR  SET field=$ORDER(^DD(FILE,field)) QUIT:(+field'>0)!(done=1)  do
        . NEW array
        . DO FIELD^DID(FILE,field,"N","POINTER","array")
        . IF $GET(array("POINTER"))="" QUIT
        . IF array("POINTER")[";" QUIT
        . SET Info(field,"GL")=array("POINTER")
        . NEW temp SET temp=$PIECE($GET(^DD(FILE,field,0)),"^",2)
        . SET temp=+$PIECE(temp,"P",2)
        . SET Info(field)=temp
        . IF $DATA(array) WRITE field," " DO ZWRITE^TMGZWR("array")
GPODone
        QUIT

