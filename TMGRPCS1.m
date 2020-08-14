TMGRPCS1 ;TMG/kst/RPC entry points for Search PT DOCS API ; 6/20/10, 2/2/14
        ;;1.0;TMG-LIB;**1**;05/20/10
        ;
 ;"RPC ENTRY POINTS FOR TMG PATIENT DOCUMENTS SEARCH
 ;
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
 ;" RPC -- Public Functions.
 ;"=======================================================================
 ;"PDSRCH(TMGRESULT,TMGPARAMS) -- launch a background search in documents for 1 patient
 ;"PDSTATUS(TMGRESULT,TMGPARAMS) -- Get status of background search
 ;"PDRESULT(TMGRESULT,TMGPARAMS) -- Get result from background search
 ;"PDCLEAR(TMGRESULT,TMGPARAMS) -- Tell background task to stop, and clear data array
 ;"PDSTOP(TMGRESULT,TMGPARAMS) -- Tell background task to stop searching
 ;"PDPREPSS^TMGRPCS1(TMGRESULT,TMGPARAMS) -- Prep for Subset of List for TORCombobox
 ;"PDGETSS^TMGRPCS1(TMGRESULT,TMGPARAMS) -- Get a subset of list for TORCombobox

 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"
 ;"=======================================================================
 ;"=======================================================================
 ;"Dependencies:  TMGSRCH2
 ;"=======================================================================
 ;"=======================================================================
 ;
PDSRCH(TMGRESULT,TMGPARAMS) ;
        ;"Purpose: Handle PT DOCS SEARCH -- launch a background search in documents for 1 patient
        ;"Input: TMGRESULT -- PASS BY REFERENCE.  AN OUT PARAMETER
        ;"       TMGPARAMS -- Input: PatientEIN^SearchString
        ;"           Search string notes:
        ;"             Each word (or partial word) to look up is separated by spaces
        ;"             All words are combined in AND fashion
        ;"             Search is NOT case sensitive.
        ;"             Exact pharases can be specified by quotes.
        ;"             Example: 'dog cat monkey "in a barrel"
        ;"Note: this function can ALSO be used to allow the CHANGE the search.
        ;"      This will allow the search to begin while the user is still
        ;"      entering the search terms.  If the NEW search is just an
        ;"      extension to the prior search, then the prior search will be
        ;"      added on rather than starting over.
        ;
        ;"Results: TMGRESULT(0)="1^Success", OR -1^ErrorMsg
        NEW DFN SET DFN=+$PIECE(TMGPARAMS,"^",1)
        IF DFN'>0 DO  GOTO PDSDN
        . SET TMGRESULT(0)="-1^Invalid Patient IEN"
        NEW TMGSRCH SET TMGSRCH=$PIECE(TMGPARAMS,"^",2)
        IF TMGSRCH="" DO  GOTO PDSDN
        . SET TMGRESULT(0)="-1^No search terms provided."
        DO LAUNCHSR^TMGSRCH2(DFN,TMGSRCH) ;
        SET TMGRESULT(0)="1^SUCCESS"
PDSDN   QUIT
        ;
PDSTATUS(TMGRESULT,TMGPARAMS) ;
        ;"Purpose: Handle PT DOCS STATUS -- Get status of background search
        ;"Input: TMGRESULT -- PASS BY REFERENCE.  AN OUT PARAMETER
        ;"       TMGPARAMS -- Input: NOT USED
        ;"Results: TMGRESULT(0)=1^Status or -1^ErrorMessage
        SET TMGRESULT(0)=$$STATUS^TMGSRCH2() ;
        QUIT
        ;
PDRESULT(TMGRESULT,TMGPARAMS) ;
        ;"Purpose: Handle PT DOCS GET RESULTS -- get result from background search
        ;"Input: TMGRESULT -- PASS BY REFERENCE.  AN OUT PARAMETER
        ;"       TMGPARAMS -- Input: NOT USED
        ;"Results: TMGRESULT(0)=FoundCount^Success, or -1^Message
        ;"         TMGRESULT(1)=IEN1
        ;"         TMGRESULT(2)=IEN2 ... etc.
        DO RESULTS^TMGSRCH2(.TMGRESULT)
        QUIT
        ;
PDCLEAR(TMGRESULT,TMGPARAMS) ;
        ;"Purpose: PT DOCS CLEAR -- Tell background task to stop, and clear data array
        ;"Input: TMGRESULT -- PASS BY REFERENCE.  AN OUT PARAMETER
        ;"       TMGPARAMS -- Input: NOT USED
        ;"Results: TMGRESULT(0)="1^Success
        DO CLEAR^TMGSRCH2
        SET TMGRESULT(0)="1^Success"
        QUIT
        ;
PDSTOP(TMGRESULT,TMGPARAMS) ;
        ;"Purpose: Handle PT DOCS STOP  -- Tell background task to stop searching
        ;"Input: TMGRESULT -- PASS BY REFERENCE.  AN OUT PARAMETER
        ;"       TMGPARAMS -- Input: NOT USED
        ;"Results: TMGRESULT(0)=
        ;"Results: TMGRESULT(0)="1^Success
        DO STOP^TMGSRCH2
        SET TMGRESULT(0)="1^Success"
        QUIT
        ;
PDPREPSS(TMGRESULT,TMGPARAMS) ;
        ;"Purpose -- Prep for Subset of List for TORCombobox
        ;"Input: TMGRESULT -- PASS BY REFERENCE.  AN OUT PARAMETER
        ;"       TMGPARAMS -- Input: NOT USED
        ;"Results: TMGRESULT(0)="1^Success", OR -1^ErrorMsg
        NEW LIST
        NEW CT SET CT=1
        NEW ABORT SET ABORT=0
        DO PDRESULT(.LIST) ;
        NEW REF SET REF=$NAME(^TMG("TMP","SEARCH","SRCHTIU",$J,"OR LIST"))
        KILL @REF
        IF +$GET(LIST(0))'>0 SET TMGRESULT(0)=LIST(0) GOTO PSSDN
        NEW I SET I=""
        FOR  SET I=$ORDER(LIST(I),-1) QUIT:(+I'>0)!ABORT  DO
        . NEW IEN SET IEN=+$GET(LIST(I)) QUIT:IEN'>0
        . NEW NAME SET NAME=$$FORMAT(IEN)
        . IF +NAME=-1 SET ABORT=1,TMGRESULT(0)=NAME QUIT
        . SET NAME=$$RJ^XLFSTR(CT,3," ")_".  "_NAME
        . SET CT=CT+1
        . SET @REF@(NAME,IEN)=""
        IF ABORT=0 SET TMGRESULT(0)="1^Success"
PSSDN   QUIT
        ;
        ;
FORMAT(IEN) ;
        ;"Purpose: Return a string describing note title
        ;"Input: IEN -- The IEN in file 8925
        ;"Result: July 15, 2010 -- NOTE TITLE -- AUTHOR
        SET IEN=+$GET(IEN)
        NEW RESULT
        IF (IEN'>0)!($DATA(^TIU(8925,IEN))=0) DO  GOTO FMTDN
        . SET RESULT="-1^<Note "_IEN_" Doesn't Exist>"
        NEW TMGOUT,TMGMSG,TMGIENS
        SET TMGIENS=IEN_","
        DO GETS^DIQ(8925,TMGIENS,".01;1202","","TMGOUT","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO FMTDN
        . SET RESULT="-1^<Error getting information for record "_IEN_">"
        NEW TITLE SET TITLE=$GET(TMGOUT(8925,TMGIENS,.01),"? TITLE")
        NEW AUTHOR SET AUTHOR=$GET(TMGOUT(8925,TMGIENS,1202),"? AUTHOR")
        NEW DATE SET DATE=$PIECE($GET(^TIU(8925,IEN,0)),"^",7) ;"Custom formatting
        SET DATE=$$FMTE^XLFDT(DATE,7)  ;"7 --> YYYY/MM/DD@time
        SET DATE=$PIECE(DATE,"@",1) ;"remove time
        NEW MONTH SET MONTH=$PIECE(DATE,"/",2)
        IF $LENGTH(MONTH)=1 SET MONTH="0"_MONTH SET $PIECE(DATE,"/",2)=MONTH
        NEW DAY SET DAY=$PIECE(DATE,"/",3)
        IF $LENGTH(DAY)=1 SET DAY="0"_DAY SET $PIECE(DATE,"/",3)=DAY
        SET RESULT=DATE_"; "_TITLE_" - "_AUTHOR
FMTDN   QUIT RESULT
        ;
PDGETSS(TMGRESULT,TMGPARAMS) ;
        ;"Purpose -- Get a subset of list for TORCombobox
        ;"    NOTE: This should only be called after a successful call to
        ;"         PDPREPSS^TMGRPCS1 which will prepair the list.
        ;"Input: TMGRESULT -- PASS BY REFERENCE.  AN OUT PARAMETER
        ;"       TMGPARAMS -- Input:  StartFrom^Direction^MaxCount
        ;"              StartFrom -- OPTIONAL -- text to $ORDER() from
        ;"              Direction -- $ORDER(xx,Direction) direction (should be 1 or -1) -- OPTIONAL
        ;"              MaxCount -- OPTIONAL.  Default is 44 values returned.
        ;"Results: TMGRESULT(0)="1^Success" or "-1^Message"
        ;"         TMGRESULT(1)=IEN^2010/6/10; OFFICE NOTE - TOPPENBERG,KEVIN   <-- Example
        ;"         TMGRESULT(2)=IEN^A_Note_Identifier
        ;
        KILL TMGRESULT
        NEW TMGFROM SET TMGFROM=$PIECE(TMGPARAMS,"^",1)
        NEW TMGDIR SET TMGDIR=$PIECE(TMGPARAMS,"^",2)
        IF TMGDIR'=-1 SET TMGDIR=1
        NEW TMGMAXCT SET TMGMAXCT=+$PIECE(TMGPARAMS,"^",3)
        IF TMGMAXCT=0 SET TMGMAXCT=44
        ;
        NEW TMGREF SET TMGREF=$NAME(^TMG("TMP","SEARCH","SRCHTIU",$J,"OR LIST"))
        NEW TMGI SET TMGI=0
        FOR  SET TMGFROM=$ORDER(@TMGREF@(TMGFROM),TMGDIR) QUIT:(TMGFROM="")!(TMGI'<TMGMAXCT)  DO
        . NEW TMGIEN SET TMGIEN=""
        . FOR  SET TMGIEN=$ORDER(@TMGREF@(TMGFROM,TMGIEN),TMGDIR) QUIT:(+TMGIEN'>0)!(TMGI'<TMGMAXCT)  DO
        . . SET TMGI=TMGI+1
        . . SET TMGRESULT(TMGI)=TMGIEN_"^"_TMGFROM
        ;
        IF $DATA(TMGRESULT)=0 SET TMGRESULT(0)="-1^NO RESULTS"
        ELSE  SET TMGRESULT(0)="1^Success"
        QUIT
        ;


