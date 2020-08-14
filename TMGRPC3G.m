TMGRPC3G ;TMG/kst/Support Functions for GUI_Config ;08/31/08, 2/10/13, 2/2/14
         ;;1.0;TMG-LIB;**1**;08/31/08
 ;
 ;"TMG RPC FUNCTIONS for a GUI config program
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
 ;" <none>
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"GETERSTR(TMGEARRAY) -- convert a standard DIERR array into a string for output
 ;"ADDFDA(TMGFDA,TMGOUT,INDX) -- output the TMGFDA into the TMGOUT variable
 ;"GETDLGIF(TMGOUT,TMGPARAMS) -- Get reminder dialog information
 ;"GETSRC(TMGOUT) -- Get linked Reminder definition for reminder dialogs.
 ;"MOVDLGEL(TMGOUT,TMGPARAMS) -- Move Reminder Dialog Element
 ;"RPTPXRM(TMGRESULT,TMGIEN) -- HANDLE RPC CHANNEL REQUEST FOR TAXONOMY INQUIRE
 ;"GDLGROOT(TMGRESULT,TMGIEN) -- Return list of root items of any reminder dialog tree that items is part of
 ;"EXPFNAME(TMGRESULT,TMGFNUM) -- Handle Channel command: GET EXPANDED FILENAME
 ;"TESTREM(TMGRESULT,PARAMS) -- HANDLE RPC CHANNEL REQUEST FOR: GET REMINDER TEST
 ;"FIND1(TMGRESULT,TMGPARAMS) --HANDLE RPC CHANNEL REQUEST FOR: FIND ONE RECORD
 ;"DOCPYRMD(TMGRESULT,TMGPARAMS) --HANDLE RPC CHANNEL REQUEST FOR: COPY REMINDER DIALOG
 ;"GETCHLST(TMGRESULT,TMGPARAMS) --HANDLE RPC CHANNEL REQUEST FOR: GET REMINDER DIALOG CHILDREN
 ;"GETFFNSG(TMGRESULT,NAME) -- HANDLE RPC CHANNEL REQUEST FOR: GET FUNC FINDING ARG SIGNATURE
 ;
 ;"=======================================================================
 ;"Utility functions
 ;"=======================================================================
 ;"RPT1PXRM(TMGREF,TMGIEN) -- GET INQUIRE REPORT FOR REMINDER TAXONOMY 
 ;"LOADPRNT(TMGIEN,TMGARR) --Gather list of parents in 801.41.  
 ;"IFRMDLGO(TMGIEN,TMGPARENTIEN) -- IF REMINDER DIALOG element item is compatable with parent
 ;"IFREMFLG(TMGIEN,TMGFLAGS) --screen entries from 811.9 (REMINDER DEFINITION) for matching flags.
 ;"EXPFNAM2(FILENUM) -- Convert file number info expanded name, PARENTNAME:SUBNAME:...
 ;"TEST1REM(TMGREF,TMGIEN,TMGPAT,TMGDT,TMGALLTERMS) -- GET REMINDER TEST
 ;"FIND1(TMGRESULT,TMGPARAMS) --WRAPPER FOR $$FIND1^DIC
 ;"CPYRMDLG(SOURCEIEN,NAMESPACE,ACCEPT) -- Copy a reminder dialog tree (including all descendents)
 ;"CHILDLST(IEN,ARRAY)  --Compile list of child (and grandchildren etc) elements
 ;
 ;"=======================================================================
 ;"Private functions
 ;"=======================================================================
 ;"SHDACEPT(IEN,ACCEPT) -- See CPYRMDLG for discussion of ACCEPT array
 ;"TRNMRGEX(DIFLG,DIFFNO,DITFNO,DIFIEN,DITIEN,DIERRROOT) -- wrapper for Fileman functionality. 
 ; 
 ;"=======================================================================
 ;"=======================================================================
 ;"Dependencies:
 ;"  DILF,%ZISH,PXRMRPCA, DIC, XLFDT, DIT3 
 ;"  Designed to be called FROM TMGRPC3* only
 ;"     (Other calls might work, but not specifically tested)
 ;"  TMGZWR
 ;
 ;"=======================================================================
 ;"=======================================================================
 ;
GETERSTR(TMGEARRAY) ;
        ;"Purpose: convert a standard DIERR array into a string for output
        ;"Input: TMGEARRAY -- PASS BY REFERENCE.  example:
        ;"      array("DIERR")="1^1"
        ;"      array("DIERR",1)=311
        ;"      array("DIERR",1,"PARAM",0)=3
        ;"      array("DIERR",1,"PARAM","FIELD")=.02
        ;"      array("DIERR",1,"PARAM","FILE")=2
        ;"      array("DIERR",1,"PARAM","IENS")="+1,"
        ;"      array("DIERR",1,"TEXT",1)="The NEW record '+1,' lacks some required identifiers."
        ;"      array("DIERR","E",311,1)=""
        ;"Results: returns one long equivalent string from above array.
        ;"Note: This is a copy of the function GETERRST^TMGDEBU2
        ;"      I copied it here so that this file has no TMG* dependencies.
 ;
        NEW TMGESTR,TMGIDX,TMGENUM
        NEW TMGRESULT SET TMGRESULT=""
        FOR TMGENUM=1:1:+$GET(TMGEARRAY("DIERR")) DO
        . NEW TMGESTR SET TMGESTR="Fileman says: '"
        . ;"SET TMGESTR=TMGESTR_"Fileman says: '"
        . IF TMGENUM'=1 SET TMGESTR=TMGESTR_"(Error# "_TMGENUM_") "
        . SET TMGIDX=$ORDER(TMGEARRAY("DIERR",TMGENUM,"TEXT",""))
        . IF TMGIDX'="" FOR  DO  QUIT:(TMGIDX="")
        . . SET TMGESTR=TMGESTR_$GET(TMGEARRAY("DIERR",TMGENUM,"TEXT",TMGIDX))_" "
        . . SET TMGIDX=$ORDER(TMGEARRAY("DIERR",TMGENUM,"TEXT",TMGIDX))
        . IF $GET(TMGEARRAY("DIERR",TMGENUM,"PARAM",0))>0 DO
        . . SET TMGIDX=$ORDER(TMGEARRAY("DIERR",TMGENUM,"PARAM",0))
        . . SET TMGESTR=TMGESTR_"Details: "
        . . FOR  DO  QUIT:(TMGIDX="")
        . . . IF TMGIDX="" QUIT
        . . . SET TMGESTR=TMGESTR_"["_TMGIDX_"]="_$GET(TMGEARRAY("DIERR",1,"PARAM",TMGIDX))_"  "
        . . . SET TMGIDX=$ORDER(TMGEARRAY("DIERR",TMGENUM,"PARAM",TMGIDX))
        . IF TMGRESULT'[TMGESTR SET TMGRESULT=TMGRESULT_TMGESTR
 ;
        QUIT TMGRESULT
 ;
ADDFDA(TMGFDA,TMGOUT,INDX) ;
        ;"Purpose: To output the TMGFDA into the TMGOUT variable (so show erroneous
        ;"         FDA.
        ;"Input: TMGFDA -- the FDA as send to fileman.  PASS BY REFERENCE
        ;"       TMGOUT -- the variable that will be passed back as the result of
        ;"                 the RPC call.  PASS BY REFERENCE.
        ;"       INDX -- the index to start adding the TMGFDA at.
        ;"Results: none.
        ;"
        NEW TMGI SET TMGI=""
        FOR  SET TMGI=$ORDER(TMGFDA(TMGI)) QUIT:TMGI=""  DO
        . SET TMGOUT(INDX)="FDA("_TMGI_")="_$GET(TMGFDA(TMGI))
        . SET INDX=INDX+1
        QUIT
        ;
GETDLGIF(TMGOUT,TMGPARAMS) ;
        ;"Purpose: to return information about every REMINDER DIALOG (#801.41)
        ;"         entry in system, provided type = R (reminder dialog)
        ;"Input:  TMGOUT -- the is the result OUT PARAMETER. Pass by reference
        ;"        TMGPARAMS -- Not used. Include for future expansion
        ;"Output: TMGOUT is filled as follows:
        ;"          TMGOUT(0)="1^Success" or "-1^Message"
        ;"          TMGOUT(1)= (as below)
        ;"             piece#   Description
        ;"              1       IEN801.4
        ;"              2       Reminder dialog name
        ;"              ----- Below to match output from ORWCV START and ORWCV POL ----
        ;"              ----- only included IF there is a matching reminder definition ----
        ;"              3       IEN of linked REMINDER DEF (#811.9)    matches piece 1
        ;"              4       print name of REMINDER DEF             matches piece 2
        ;"              5-8     not used here                          matches piece 3-6
        ;"              9       Has linked active Rem Dlg (0 or 1)     matches piece 7
        ;"              10-12   not used here                          matches piece 8-10
        ;"              13      Additional check "DLGWIPE" (0 or 1)    matches piece 11
        NEW TMGI SET TMGI=0
        KILL TMGOUT
        NEW TMGLINK
        DO GETSRC(.TMGLINK)
        SET TMGOUT(0)="1^Success" ;"default
        NEW TMGIEN811D9
        NEW TMGIEN SET TMGIEN=0
        FOR  SET TMGIEN=$ORDER(^PXRMD(801.41,TMGIEN)) QUIT:(+TMGIEN'>0)  DO
        . NEW S
        . SET S=TMGIEN_"^"
        . NEW TMGZN SET TMGZN=$GET(^PXRMD(801.41,TMGIEN,0))
        . IF $PIECE(TMGZN,"^",4)'="R" QUIT
        . SET S=S_$PIECE(TMGZN,"^",1)_"^"
        . SET TMGIEN811D9=+$ORDER(TMGLINK(TMGIEN,""))
        . SET S=S_TMGIEN811D9_"^"
        . IF TMGIEN811D9>0 DO
        . . ;"below based on code from AVAL^PXRMRPCA
        . . SET S=S_$PIECE($GET(^PXD(811.9,TMGIEN811D9,0)),"^",3)
        . . SET S=S_"^^^^^"
        . . SET S=S_$$DLG^PXRMRPCA(TMGIEN811D9)_"^^^^"
        . . SET S=S_$$DLGWIPE^PXRMRPCA(TMGIEN811D9)
        . SET TMGI=TMGI+1
        . SET TMGOUT(TMGI)=S
        QUIT
        ;
GETSRC(TMGOUT) ;
        ;"Purpose: Get linked Reminder definition for reminder dialogs.
        ;"Input: TMGOUT -- AN OUT PARAMETER
        ;"Result:  TMGOUT(ReminderDlgIEN801.41,ReminderDef811.9)=""
        ;"Note: the REMINDER DIALOG field SOURCE REMINDER doesn't seem reliably populated
        KILL TMGOUT
        NEW TMGIEN801D41
        NEW TMGIEN811D9 SET TMGIEN811D9=0
        FOR  SET TMGIEN811D9=$ORDER(^PXD(811.9,TMGIEN811D9)) QUIT:(+TMGIEN811D9'>0)  DO
        . SET TMGIEN801D41=+$PIECE($GET(^PXD(811.9,TMGIEN811D9,51)),"^",1)
        . QUIT:(TMGIEN801D41'>0)
        . SET TMGOUT(TMGIEN801D41,TMGIEN811D9)=""
        ;"SET TMGIEN801D41=0
        ;"FOR  SET TMGIEN801D41=$ORDER(^PXRMD(801.41,TMGIEN801D41)) QUIT:(+TMGIEN801D41'>0)  DO
        ;". IF $DATA(TMGOUT(TMGIEN801D41))>0 QUIT
        ;". SET TMGOUT(TMGIEN801D41,0)=""
        QUIT
        ;
MOVDLGEL(TMGOUT,TMGPARAMS) ;Move Reminder Dialog Element
        ;"Purpose: To move a reminder dialog element from child list of one parent into the child list of another parent
        ;"Input:  TMGOUT -- the is the result OUT PARAMETER. Pass by reference
        ;"        TMGPARAMS -- ElementIEN^CurrentParentIEN^NewParentIEN^[NewSeqNum] -- all IEN's are from file 801.41
        ;"              Note: NewSeqNum is optional.  If provided, element will be put into
        ;"                    parent's elements (children) list at indicated sequence.
        ;"                    If requested sequence number is already used, then child
        ;"                    will be inserted AFTER this number.
        ;"Output: TMGOUT is filled as follows:
        ;"          TMGOUT(0)="1^Success^IENS801d42" or "-1^Message"
        NEW TMGRESULT SET TMGRESULT="1^Success"
        SET TMGPARAMS=$GET(TMGPARAMS)
        NEW TMGIEN SET TMGIEN=+$PIECE(TMGPARAMS,"^",1)
        IF '$DATA(^PXRMD(801.41,TMGIEN)) DO  GOTO MVDN
        . SET TMGRESULT="-1^Invalid dialog element IEN.  Got '"_$PIECE(TMGPARAMS,"^",1)_"'"
        NEW TMGPIEN SET TMGPIEN=+$PIECE(TMGPARAMS,"^",2)
        IF '$DATA(^PXRMD(801.41,TMGPIEN)) DO  GOTO MVDN
        . SET TMGRESULT="-1^Invalid parent dialog element IEN.  Got '"_$PIECE(TMGPARAMS,"^",2)_"'"
        NEW TMGNEWPIEN SET TMGNEWPIEN=+$PIECE(TMGPARAMS,"^",3)
        IF '$DATA(^PXRMD(801.41,TMGNEWPIEN)) DO  GOTO MVDN
        . SET TMGRESULT="-1^Invalid NEW parent dialog element IEN.  Got '"_$PIECE(TMGPARAMS,"^",3)_"'"
        NEW TMGSUBIEN SET TMGSUBIEN=+$ORDER(^PXRMD(801.41,TMGPIEN,10,"D",TMGIEN,0))
        IF TMGSUBIEN'>0 DO  GOTO MVDN
        . SET TMGRESULT="-1^Element (IEN #"_TMGIEN_") not found as child of parent (IEN #"_TMGPIEN_")."
        NEW TMGNEWSEQ SET TMGNEWSEQ=+$PIECE(TMGPARAMS,"^",4)
        IF (TMGNEWSEQ'>0) DO
        . SET TMGNEWSEQ=+$ORDER(^PXRMD(801.41,TMGNEWPIEN,"B",""),-1)
        IF (TMGNEWSEQ=0)!(+$ORDER(^PXRMD(801.41,TMGNEWPIEN,10,"B",TMGNEWSEQ,""))>0) DO
        . FOR  DO  QUIT:(+$ORDER(^PXRMD(801.41,TMGNEWPIEN,10,"B",TMGNEWSEQ,""))=0)
        . . SET TMGNEWSEQ=TMGNEWSEQ+5
        ;"Try to add to NEW parent, into child elements list
        NEW TMGFDA,TMGMSG,TMGIENOUT
        NEW TMGIENS SET TMGIENS="+1,"_TMGNEWPIEN_","
        SET TMGFDA(801.412,TMGIENS,.01)=TMGNEWSEQ
        SET TMGFDA(801.412,TMGIENS,2)="`"_TMGIEN
        DO UPDATE^DIE("E","TMGFDA","TMGIENOUT","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO MVDN
        . SET TMGRESULT="-1^"_$$GETERSTR(.TMGMSG)
        SET TMGIENS=$GET(TMGIENOUT("1"),"???")_","_TMGNEWPIEN_","
        SET TMGRESULT=TMGRESULT_"^"_TMGIENS
        ;"Now remove from source record
        KILL TMGFDA
        SET TMGIENS=TMGSUBIEN_","_TMGPIEN_","
        SET TMGFDA(801.412,TMGIENS,.01)="@"
        DO UPDATE^DIE("E","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO MVDN
        . SET TMGRESULT="-1^"_$$GETERSTR(.TMGMSG)
        ;
MVDN    SET TMGOUT(0)=TMGRESULT
        QUIT
        ;
RPTPXRM(TMGRESULT,TMGIEN)  ; 
        ;"Purpose: HANDLE RPC CHANNEL REQUEST FOR TAXONOMY INQUIRE
        NEW TEMP SET TEMP=$$RPT1PXRM("TMGRESULT",.TMGIEN)
        SET TMGRESULT(0)=TEMP
        QUIT
        ;
RPT1PXRM(TMGREF,TMGIEN) ;
        ;"Purpose: GET INQUIRE REPORT FOR REMINDER TAXONOMY
        ;"Input:  TMGREF -- Location to store report.  Closed format.
        ;"                  E.g. "TMP(5)" --> text will be stored as follows
        ;"                    TMP(5,1)="first line"
        ;"                    TMP(5,2)="second line"  etc.
        ;"        TMGIEN -- IEN IN FILE 811.2, or name to look up.
        ;"Result: 1 IF success, OR -1^Message IF error
        NEW RESULT,X,Y,DC
        SET TMGIEN=$GET(TMGIEN)
        IF +TMGIEN>0 SET TMGIEN=+TMGIEN
        ELSE  DO
        . SET X=TMGIEN
        . IF X["TX." SET X=$PIECE(X,"TX.",2)
        . NEW DIC SET DIC=811.2,DIC(0)="M"
        . DO ^DIC
        . IF +Y>0 SET TMGIEN=+Y
        IF +TMGIEN'>0 DO  GOTO RPDN
        . SET RESULT="-1^Expected entry in file 811.2.  Got: "_TMGIEN
        NEW FLDS SET FLDS="[PXRM TAXONOMY INQUIRY]"                                          
        NEW HEADER SET HEADER="REMINDER TAXONOMY INQUIRY"                                      
        NEW DIC SET DIC="^PXD(811.2,"                                                  
        NEW BY SET BY="NUMBER"
        NEW FR,TO SET (FR,TO)=+TMGIEN
        NEW NOW SET NOW=$$FMTE^XLFDT($$NOW^XLFDT,"1P")
        NEW DHD SET DHD="W ?0 D HEADER^PXRMINQ("""_HEADER_""")"
        NEW L SET L=0
        NEW IOP SET IOP="HFS"
        NEW %ZIS SET %ZIS("HFSMODE")="W"
        NEW H SET H=$H
        NEW PATH SET PATH="/tmp/"
        NEW FNAME SET FNAME="TMG_"_$J_"_"_$P(H,",",1)_"_"_$P(H,",",2)_".txt"
        SET %ZIS("HFSNAME")=PATH_FNAME
        DO EN1^DIP  ;"<--- Here is where the report is generated
        SET TMGREF=$$OREF^DILF(TMGREF)_"1)"
        SET RESULT=$$FTG^%ZISH(PATH,FNAME,TMGREF,$QLENGTH(TMGREF))
        IF RESULT'>0 DO  GOTO RPDN
        . SET RESULT="-1^Failed to open report file: "_PATH_FNAME
        NEW TMGDEL SET TMGDEL(FNAME)=""
        SET RESULT=$$DEL^%ZISH(PATH,"TMGDEL")
        IF RESULT'>0 DO  GOTO RPDN
        . SET RESULT="-1^Report succeded, but failed to delete report file: "_PATH_FNAME
        SET RESULT="1^Success"
RPDN    QUIT RESULT        
        ;
LOADPRNT(TMGIEN,TMGARR) ;"Gather list of parents in 801.41.  
        ;"NOTE: Will make endless loop is child is also ancestor to itself (loop situation) 
        ;"TMGIEN -- IEN IN 801.41
        ;"TMGARR -- PASS BY REFERENCE.  Resulting array
        NEW TEMP,IEN
        NEW FOUND SET FOUND=0
        SET IEN=0 FOR  SET IEN=$ORDER(^PXRMD(801.41,"AD",TMGIEN,IEN)) QUIT:(+IEN'>0)  DO
        . SET TMGARR(IEN)="",FOUND=1
        . DO LOADPRNT(IEN,.TMGARR)
        IF FOUND=0 SET TMGARR("TOP",TMGIEN)=""
        QUIT
        ;  
GDLGROOT(TMGRESULT,TMGIEN)  ; 
        ;"Purpose: Return list of root items of any reminder dialog tree that items is part of
        ;"Input: TMGIEN -- IEN in 801.41
        ;"Output: TMGOUT is filled as follows:
        ;"        TMGOUT(0)="1^Success" or "-1^Message"
        ;"        TMGOUT(#)=IEN801d41^External name <-- a root item
        NEW TEMP,IEN,IDX
        KILL TMGRESULT
        DO LOADPRNT(TMGIEN,.TEMP)   
        KILL TEMP("TOP",TMGIEN)
        SET IDX=1,IEN=0 
        FOR  SET IEN=$ORDER(TEMP("TOP",IEN)) QUIT:(+IEN'>0)  DO
        . NEW NAME SET NAME=$PIECE($GET(^PXRMD(801.41,IEN,0)),"^",1)
        . SET TMGRESULT(IDX)=IEN_"^"_NAME
        . SET IDX=IDX+1
        SET TMGRESULT(0)="1^Success"
        QUIT
        ;        
IFREMFLG(TMGIEN,TMGFLAGS) ; 
        ;"Purpose: screen entries from 811.9 (REMINDER DEFINITION) for matching flags.
        ;"Input: TMGIEN -- IEN in 811.9
        ;"       TMGFLAGS -- Flags as defined in data dictionary for USAGE field in file 811.9
        ;"                   Only those records compatible with these flags should be acceptable.
        ;"  ...This field must contain C IF the reminder is to be
        ;"  selected in CPRS. The L value will override all other
        ;"  values. For example, IF L and C are defined in the usage
        ;"  field, the Reminder will not show on the cover sheet in
        ;"  CPRS, because L is in the Usage field.
        ;"  This is free text field and can contain any combination of
        ;"  the following codes: 
        ;"  Code Usage 
        ;"  C    CPRS  
        ;"  L    Reminder Patient List
        ;"  P    Patient
        ;"  R    Reminder Reports  
        ;"  X    Reminder Extracts  
        ;"  *    All of the above, except L and P.                            
        ;"Result: 1 IF record USAGE flags match input TMGFLAGS, otherwise 0
        ;
        NEW ISOK SET ISOK=1  ;"default is OK
        SET TMGIEN=+$GET(TMGIEN)
        SET TMGFLAGS=$GET(TMGFLAGS)
        NEW RECFLAGS SET RECFLAGS=$PIECE($GET(^PXD(811.9,TMGIEN,100)),"^",4)
        IF (RECFLAGS["*")&(TMGFLAGS'["L")&(TMGFLAGS'["P") GOTO IRFDN
        NEW CH,IDX
        ;"FOR IDX=1:1:$LENGTH(RECFLAGS) DO  QUIT:(ISOK=0)
        FOR IDX=1:1:$LENGTH(TMGFLAGS) DO  QUIT:(ISOK=0)
        . ;"SET THISCH=$EXTRACT(RECFLAGS,IDX)
        . ;"IF TMGFLAGS[THISCH QUIT  ;"still OK
        . SET CH=$EXTRACT(TMGFLAGS,IDX)  QUIT:(CH="")  
        . IF RECFLAGS[CH QUIT  ;"still OK
        . IF (CH'="L")&(CH'="P")&(TMGFLAGS["*") QUIT  ;"still OK.
        . IF (CH="*")&(TMGFLAGS'["L")&(TMGFLAGS'["P") QUIT
        . SET ISOK=0  ;"not a match, break look
IRFDN   QUIT ISOK
        ;
IFRMDLGO(TMGIEN,TMGPARENTIEN) ;"IF REMINDER DIALOG OK
        ;"Purpose: determine IF REMINDER DIALOG element item is compatable with parent
        ;"Input: TMGIEN, TMGPARENTIEN -- IEN's from file 801.41
        ;"NOTE: Also uses TMGTEMP1 in global scope.  List of IEN's of ancestors of parent  
        ;"      Was NEW'd in GFLSUBST. Setup in GFSSCRN^TMGRPC3B  
        ;"NOTE: Most of logic taken from SCREEN^PXRMDD41
        ;"Type options: P:prompt, E:dialog element, R:reminder dialog, F:forced value
        ;"              G:dialog group, S:result group (MH), T:result element (MH)
        ;"Result: 1 IF record is compatable, otherwise 0
        NEW ISOK SET ISOK=0
        SET TMGPARENTIEN=+$GET(TMGPARENTIEN) IF TMGPARENTIEN'>0 GOTO IFDGODN
        NEW PTYPE SET PTYPE=$PIECE($GET(^PXRMD(801.41,TMGPARENTIEN,0)),"^",4) ;"Parent type
        NEW ITYPE SET ITYPE=$PIECE($GET(^PXRMD(801.41,TMGIEN,0)),"^",4)       ;"Item type  
        IF PTYPE="E" SET ISOK=("FP"[ITYPE)       ;"Dialog (E)lements can only contain prompts/forced values
        IF PTYPE="R" SET ISOK=("EG"[ITYPE) ;"(R)eminder dialogs can contain only Elements and Groups
        IF PTYPE="S" SET ISOK=("T"[ITYPE)  ;"MH Re(S)ult Groups can only contain MH Result Elements
        IF PTYPE="G" SET ISOK=(TMGIEN'=TMGPARENTIEN) ;"Dialog (G)roups cannot point to themselves
        IF "PFT"[PTYPE SET ISOK=("RST"'[ITYPE)   ;"MH re(s)ul(t)s and (R)eminder dialogs dissallowed
        IF ISOK SET ISOK=($DATA(TMGTEMP1(TMGIEN))=0) ;"Make sure item in question is not already an ancestor to parent
IFDGODN QUIT ISOK 
        ;
EXPFNAME(TMGRESULT,TMGFNUM) ;
        ;"Purpose: Handle Channel command: GET EXPANDED FILENAME
        ;"Input:  TMGRESULT -- An OUT parameter.  Pass by ref.
        ;"        TMGFNUM -- Filenumber to expand
        ;"          NOTE: IF a file NAME is passed, and NAME is valid,
        ;"               then NAME is converted to number, and then processed as before
        ;"Output: TMGRESULT(0)=1^Success  or -1^Error message
        ;"        TMGRESULT(1)=ExpandedFilename^FileNumber
        KILL TMGRESULT
        IF +$GET(TMGFNUM)'>0 DO
        . IF $GET(TMGFNUM)="" QUIT
        . NEW X,Y,DIC SET DIC=1,DIC(0)="M"
        . SET X=TMGFNUM
        . DO ^DIC
        . IF +Y>0 SET TMGFNUM=+Y QUIT
        . SET TMGRESULT(0)="-1^Valid unique file name not found.  Got: "_$GET(TMGFNUM)
        IF $DATA(TMGRESULT) GOTO EFNDN 
        IF +$GET(TMGFNUM)'>0 DO  GOTO EFNDN
        . SET TMGRESULT(0)="-1^Numeric file number not found.  Got: "_$GET(TMGFNUM)
        SET TMGFNUM=+$GET(TMGFNUM)
        NEW TEMP SET TEMP=$$EXPFNAM2(TMGFNUM)
        IF TEMP="??" DO  GOTO EFNDN
        . SET TMGRESULT(0)="-1^Unable to expand file number: "_TMGFNUM
        SET TMGRESULT(0)="1^Success"
        SET TMGRESULT(1)=TEMP_"^"_TMGFNUM
EFNDN   QUIT  
        ;
EXPFNAM2(FILENUM)  ;Copied from EXPFNAME^TMGDBAP2 to remove dependency
        ;"Purpose: Convert file number info expanded name, PARENTNAME:SUBNAME:...
        ;"Input: FILENUM -- File or subfile number
        ;"Result: If FILENUM is a normal file number, then simply file name is returned
        ;"        IF FILENUM is a subfile number, then an expanded name is returned:
        ;"            GRANDPARENTFILENAME:PARENTILENAME:FILENAME
        ;"            For subfiles, then the name of the file will be considered the
        ;"              field name in the parent file.
        ;"Result: returns expanded name, or "??" IF problem
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
TESTREM(TMGRESULT,PARAMS)  ; 
        ;"Purpose: HANDLE RPC CHANNEL REQUEST FOR: GET REMINDER TEST
        ;"Input:  TMGRESULT -- The result parameter
        ;"        PARAMS -- ReminderIEN^DFNorPatientName^AsOfDate(External format)^DisplayAllTerms(Y/N)
        NEW TMGIEN,TMGPAT,TMGDT,TMGALLTERMS
        SET TMGIEN=$PIECE(PARAMS,"^",1)
        SET TMGPAT=$PIECE(PARAMS,"^",2)
        SET TMGDATE=$PIECE(PARAMS,"^",3)
        SET TMGALLTERMS=($PIECE(PARAMS,"^",4)="Y")
        NEW TEMP SET TEMP=$$TEST1REM("TMGRESULT",.TMGIEN,TMGPAT,TMGDATE,TMGALLTERMS)
        SET TMGRESULT(0)=TEMP
        QUIT
        ;
TEST1REM(TMGREF,TMGIEN,TMGPAT,TMGDT,TMGALLTERMS) ;
        ;"Purpose: GET REMINDER TEST
        ;"Input:  TMGREF -- Location to store report.  Closed format.
        ;"                  E.g. "TMP(5)" --> text will be stored as follows
        ;"                    TMP(5,1)="first line"
        ;"                    TMP(5,2)="second line"  etc.
        ;"        TMGIEN -- IEN IN FILE 811.9 (REMINDER DEFINITION)
        ;"        TMGPAT -- DFN or patient name to look up
        ;"        TMGDT -- As of Date (external format)
        ;"        TMGALLTERMS -- 1 IF to return all terms
        ;"Result: 1 IF success, OR -1^Message IF error
        ;"---------------------------------------
        NEW RESULT,X,Y,DIC
        SET TMGIEN=$GET(TMGIEN)
        IF +TMGIEN'>0 DO  GOTO T1RMDN  ;"Ensure TMGIEN is numeric...
        . SET RESULT="-1^Numeric reminder IEN not found.  Got: "_TMGIEN
        NEW TMGDFN SET TMGDFN=$GET(TMGPAT)
        IF +TMGDFN'=TMGDFN DO
        . SET DIC=2,DIC(0)="M"
        . SET X=TMGDFN
        . DO ^DIC
        . IF +Y>0 SET TMGDFN=+Y
        IF +TMGDFN'>0 DO  GOTO T1RMDN
        . SET RESULT="-1^Unable to find DFN for patient.  Got: "_TMGDFN
        NEW %DT SET %DT="TP" SET X=TMGDT DO ^%DT
        IF Y>0 SET TMGDT=Y
        ELSE  SET RESULT="-1^Unable to process date.  Got: "_TMGDT
        NEW PXRMTDEB SET PXRMTDEB=TMGALLTERMS  
        SET PXRHM=5
        ;"NEW H SET H=$H
        ;"NEW PATH SET PATH="/tmp/"
        ;"NEW FNAME SET FNAME="TMG_"_$J_"_"_$P(H,",",1)_"_"_$P(H,",",2)_".txt"
        ;"NEW IOP SET IOP="HFS"
        ;"NEW %ZIS SET %ZIS("HFSMODE")="W"
        ;"SET %ZIS("HFSNAME")=PATH_FNAME
        ;"DO ^%ZIS
        ;"IF POP>0 DO  GOTO T1RMDN
        ;". SET RESULT="-1^Error opening host file for writing.  POP="_POP
        ;"USE IO
        ;";"NEW I FOR I=1:1:100 WRITE I,!
        ;"DO DOREM^PXRMDEV(TMGDFN,TMGIEN,PXRHM,TMGDT)
        NEW ARR
        DO DOREM^TMGPXRU2(.ARR,TMGDFN,TMGIEN,PXRHM,TMGDT)
        MERGE @TMGREF=ARR
        ;"DO ^%ZISC
        ;"SET TMGREF=$$OREF^DILF(TMGREF)_"1)"
        ;"SET RESULT=$$FTG^%ZISH(PATH,FNAME,TMGREF,$QLENGTH(TMGREF))
        ;"IF RESULT'>0 DO  GOTO RPDN
        ;". SET RESULT="-1^Failed to open report file: "_PATH_FNAME
        ;"NEW TMGDEL SET TMGDEL(FNAME)=""
        ;"SET RESULT=$$DEL^%ZISH(PATH,"TMGDEL")
        ;"IF RESULT'>0 DO  GOTO RPDN
        ;". SET RESULT="-1^Report succeded, but failed to delete report file: "_PATH_FNAME
        SET RESULT="1^Success"
T1RMDN  QUIT RESULT        
        ;
FIND1(TMGRESULT,TMGPARAMS) ;"WRAPPER FOR $$FIND1^DIC
        ;"Purpose: HANDLE RPC CHANNEL REQUEST FOR: FIND ONE RECORD
        ;"Input:  TMGRESULT -- The result parameter
        ;"        PARAMS -- File^IENS^Value^Flags^Indexes^Screen        
        ;"                   See Fileman reference for details of input values.
        ;"                   Note: Indexes should be delimited with ";", not "^" -- e.g. B;C;D 
        ;"                         Screen code can not contain "^"
        ;"Result: 1^FoundRecordIEN IF success, OR -1^Message IF error
        NEW TMGFILE,TMGIENS,TMGVALUE,TMGFLAGS,TMGINDEXES,TMGSCREEN
        SET TMGFILE=$PIECE(TMGPARAMS,"^",1)
        SET TMGIENS=$PIECE(TMGPARAMS,"^",2)
        SET TMGVALUE=$PIECE(TMGPARAMS,"^",3)
        SET TMGFLAGS=$PIECE(TMGPARAMS,"^",4)
        SET TMGINDEXES=$PIECE(TMGPARAMS,"^",5)
        SET TMGINDEXES=$TRANSLATE(TMGINDEXES,";","^")
        SET TMGSCREEN=$PIECE(TMGPARAMS,"^",6)
        NEW FOUNDIEN,TMGMSG
        SET FOUNDIEN=$$FIND1^DIC(TMGFILE,TMGIENS,TMGFLAGS,TMGVALUE,TMGINDEXES,TMGSCREEN,"TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO F1DN
        . SET TMGRESULT(0)="-1^"_$$GETERSTR(.TMGMSG)
        SET TMGRESULT(0)="1^"_FOUNDIEN
F1DN    QUIT
        ;
FIND(TMGRESULT,TMGPARAMS) ;"WRAPPER FOR FIND^DIC
        ;"Purpose: HANDLE RPC CHANNEL REQUEST FOR: FIND RECORDS
        ;"Input:  TMGRESULT -- The result parameter
        ;"        PARAMS -- File^IENS^Value^Fields^Flags^Number^Indexes^Screen^Identifier        
        ;"                   See Fileman reference for details of input values.
        ;"                   Note: Indexes should be delimited with ";", not "^" -- e.g. B;C;D 
        ;"                         Screen code can not contain "^"
        ;"Result: 1^FoundRecordIEN IF success, OR -1^Message IF error
        NEW TMGFILE,TMGIENS,TMGVALUE,TMGFIELDS,TMGFLAGS
        NEW TMGNUMBER,TMGINDEXES,TMGSCREEN,TMGIDENTIFIER
        SET TMGFILE=$PIECE(TMGPARAMS,"^",1)
        SET TMGIENS=$PIECE(TMGPARAMS,"^",2)
        SET TMGVALUE=$PIECE(TMGPARAMS,"^",3)
        SET TMGFIELDS=$PIECE(TMGPARAMS,"^",4)
        SET TMGFLAGS=$PIECE(TMGPARAMS,"^",5)
        SET TMGNUMBER=$PIECE(TMGPARAMS,"^",6)
        SET TMGINDEXES=$PIECE(TMGPARAMS,"^",7)
        SET TMGINDEXES=$TRANSLATE(TMGINDEXES,";","^")
        SET TMGSCREEN=$PIECE(TMGPARAMS,"^",8)
        SET TMGIDENTIFIER=$PIECE(TMGPARAMS,"^",9)
        NEW TMGOUT,TMGMSG
        DO FIND^DIC(TMGFILE,TMGIENS,TMGFIELDS,TMGFLAGS,TMGVALUE,TMGNUMBER,TMGINDEXES,TMGSCREEN,TMGIDENTIFIER,"TMGOUT","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO FNDDN
        . SET TMGRESULT(0)="-1^"_$$GETERSTR(.TMGMSG)
        SET TMGRESULT(0)="1^Success"
        NEW SRCH MERGE SRCH=TMGOUT("DILIST")
        DO ZWR2ARR^TMGZWR("SRCH","TMGRESULT")
FNDDN   QUIT                
        ;
DOCPYRMD(TMGRESULT,TMGPARAMS) ;
        ;"Purpose: HANDLE RPC CHANNEL REQUEST FOR: COPY REMINDER DIALOG
        ;"Input:  TMGRESULT -- The result parameter
        ;"        PARAMS -- IEN^Namespace^AcceptList
        ;"              IEN - IEN in REMINDER DIALOG (801.41) file.
        ;"              NAMESPACE -- See CPYRMDLG for examples.
        ;"              SponsorIEN -- IEN in SPONSOR file.
        ;"              AcceptList -- semicolon delimited.  See CPYRMDLG for meaning
        ;"                  e.g. DG PH;ED;VA-HDL
        ;"Output: TMGRESULT(0)=1^Success^IENOfNewDialogTreeRoot^NewName IF success, OR -1^Message IF error
        ;"Result : none.
        NEW IEN SET IEN=$PIECE(TMGPARAMS,"^",1)
        NEW NS SET NS=$PIECE(TMGPARAMS,"^",2)
        NEW SPONIEN SET SPONIEN=$PIECE(TMGPARAMS,"^",3)
        NEW TEMP SET TEMP=$PIECE(TMGPARAMS,"^",4)
        NEW ACCEPT,IDX,DONELIST
        FOR IDX=0:1:$LENGTH(TEMP,";") DO
        . NEW ONE SET ONE=$PIECE(TEMP,";",IDX) QUIT:ONE=""
        . SET ACCEPT(ONE)=""
        SET TEMP=$$CPYRMDLG(IEN,NS,SPONIEN,.ACCEPT,.DONELIST)
        IF +TEMP=-1 SET TMGRESULT(0)=TEMP
        ELSE  SET TMGRESULT(0)="1^Success^"_TEMP
        QUIT
        ;
CPYRMDLG(SOURCEIEN,NAMESPACE,SPONIEN,ACCEPT,DONELIST) ;"COPY REMINDER DIALOG
        ;"Purpose: To copy a reminder dialog tree (including all descendents)
        ;"         into NEW namespace, with LOCAL status.
        ;"Input: SOURCEIEN -- IEN in REMINDER DIALOG (801.41) file.
        ;"       NAMESPACE -- The NEW namespace to put records into.  Should be 2-4 chars
        ;"         NAMESPACE is prefixed to .01 field (name).  E.g. IF NAMESPACE="ZZ", and
        ;"         prior name was "VA-HTN", then NEW name will be "ZZ-VA-HTN"
        ;"       SPONIEN -- IEN IN SPONSOR file.
        ;"       ACCEPT.  PASS BY REFERENCE.  List of component names spaces to accept
        ;"          If Reminder dialog subcomponents were to refer to a standard
        ;"          library, and when copying a parent, these library items should
        ;"          not also be copied, then these items should be noted in the
        ;"          ACCEPT array.  E.g.
        ;"              ACCEPT("DG PH")="" <-- will cause all subcomponents with
        ;"                                     names starting with "DG PH" to not be copied
        ;"              ACCEPT("ED") <-- all with name starting with "ED" not copied.
        ;"       DONELIST -- PASS BY REFERENCE.  A list of records that have 
        ;"          already been copied, to prevent recursive endless loops. 
        ;"          Format: DONELIST(SOURCEIEN)=1  <-- means already copied.
        ;"Output: NEW records are added to file 801.41, copies of specified tree
        ;"Result: IENOfNewDialogTreeRoot^NewItemName, 
        ;"         or -1^Message IF error. or "0^Already done" IF already copied
        NEW TMGRESULT SET TMGRESULT=1
        SET NAMESPACE=$GET(NAMESPACE)
        NEW L SET L=$LENGTH(NAMESPACE)
        IF (NAMESPACE="")!(L<2)!(L>4) DO  GOTO CRDDN
        . SET TMGRESULT="-1^Invalid Namespace parameter.  Got ["_NAMESPACE_"]"
        NEW IEN SET IEN=+$GET(SOURCEIEN)
        IF IEN'>0 DO  GOTO CRDDN
        . SET TMGRESULT="-1^Invalid IEN.  Got ["_SOURCEIEN_"]"
        IF $GET(DONELIST(IEN))=1 DO  GOTO CRDDN
        . SET TMGRESULT="0^Already done"
        NEW OLDNAME SET OLDNAME=$PIECE($GET(^PXRMD(801.41,IEN,0)),"^",1)
        IF OLDNAME="" DO  GOTO CRDDN
        . SET TMGRESULT="-1^Source IEN record doesn't exist."
        NEW NEWIEN SET NEWIEN="ADD,"  ;"Will be changed to true NEW IEN
        NEW TMGMSG
        DO TRNMRGEX("A",801.41,801.41,IEN,.NEWIEN,"TMGMSG")  ;"Copy source record to new
        IF $DATA(TMGMSG("DIERR")) DO  GOTO CRDDN
        . SET TMGRESULT="-1^"_$$GETERSTR(.TMGMSG)
        IF +NEWIEN'>0 DO  GOTO CRDDN
        . SET TMGRESULT="-1^Unable to get IEN of newly added record"
        NEW NEWNAME,NAMEOK,INSTANCECOUNT SET (NAMEOK,INSTANCECOUNT)=0
        FOR  DO  QUIT:NAMEOK  ;"Setup NEW name, ensure unique and length OK
        . SET NEWNAME=NAMESPACE_"-"_OLDNAME
        . IF INSTANCECOUNT>0 SET NEWNAME=NEWNAME_INSTANCECOUNT
        . IF $LENGTH(NEWNAME)>64 DO  QUIT
        . . SET OLDNAME=$EXTRACT(OLDNAME,1,$LENGTH(OLDNAME)-1)
        . NEW X,Y,DIC SET DIC=801.41,DIC(0)="MX",X=NEWNAME
        . DO ^DIC  ;"Check IF NEWNAME has already been used.
        . IF +Y>0 SET INSTANCECOUNT=INSTANCECOUNT+1 QUIT
        . SET NAMEOK=1
        NEW TMGFDA KILL TMGMSG        
        SET TMGFDA(801.41,NEWIEN,.01)=NEWNAME
        SET TMGFDA(801.41,NEWIEN,100)="LOCAL"
        IF SPONIEN'="" SET TMGFDA(801.41,NEWIEN,101)="`"_SPONIEN
        DO FILE^DIE("ET","TMGFDA","TMGMSG")  ;"Rename NEW record and SET to LOCAL
        IF $DATA(TMGMSG("DIERR")) DO  GOTO CRDDN
        . SET TMGRESULT="-1^"_$$GETERSTR(.TMGMSG)
        SET DONELIST(IEN)=1  ;"Remember that record successfully copied. 
        ;"Next copy all components of NEW dialog, and put them into namespace too.
        NEW SUBIEN SET SUBIEN=0
        FOR  SET SUBIEN=$ORDER(^PXRMD(801.41,+NEWIEN,10,SUBIEN)) QUIT:(+SUBIEN'>0)!(+TMGRESULT=-1)  DO
        . NEW SUBPTR SET SUBPTR=$PIECE($GET(^PXRMD(801.41,+NEWIEN,10,SUBIEN,0)),"^",2)
        . IF $$SHDACEPT(SUBPTR,.ACCEPT) QUIT
        . NEW NEWSUBPTR SET NEWSUBPTR=$$CPYRMDLG(SUBPTR,NAMESPACE,SPONIEN,.ACCEPT,.DONELIST) ;"Recursive COPY REMINDER DIALOG ELEMENT
        . IF +NEWSUBPTR'>0 SET TMGRESULT=NEWSUBPTR QUIT
        . KILL TMGFDA,TMGMSG
        . NEW IENS SET IENS=SUBIEN_","_+NEWIEN_","
        . SET TMGFDA(801.412,IENS,2)="`"_+NEWSUBPTR
        . DO FILE^DIE("E","TMGFDA","TMGMSG")  ;"Point subrecord to newly copied record
        . IF $DATA(TMGMSG("DIERR")) DO  QUIT
        . . SET TMGRESULT="-1^"_$$GETERSTR(.TMGMSG)        
        IF +TMGRESULT=-1 GOTO CRDDN
        SET TMGRESULT=+NEWIEN_"^"_NEWNAME                
CRDDN   QUIT TMGRESULT     
        ;
SHDACEPT(IEN,ACCEPT) ;"SHOULD ACCEPT (i.e. not copy).   See CPYRMDLG for discussion of ACCEPT array
        NEW TMGRESULT SET TMGRESULT=0
        SET IEN=$GET(IEN) IF IEN'>0 GOTO SADN
        NEW NAME SET NAME=$PIECE($GET(^PXRMD(801.41,IEN,0)),"^",1)
        NEW ONE SET ONE=""
        FOR  SET ONE=$ORDER(ACCEPT(ONE)) QUIT:(ONE="")!(TMGRESULT=1)  DO
        . IF $EXTRACT(NAME,1,$LENGTH(ONE))=ONE SET TMGRESULT=1 QUIT
SADN    QUIT TMGRESULT
        ;
TRNMRGEX(DIFLG,DIFFNO,DITFNO,DIFIEN,DITIEN,DIERRROOT)        ;"//kt added
        ;"//NOTE: This is a wrapper for Fileman functionality.  A formal API for
        ;"        future version of Fileman has proposed.  If approved, then change
        ;"        call to use that functionality.  
        ;"Purpose: Extended SILENT TRANSFER/MERGE OF SINGLE RECORDS IN FILE OR SUBFILE
        ;"See TMGDIT for documentation of parameters. 
        SET DITIEN=$G(DITIEN)
        IF DITIEN,$E(DITIEN)="+" SET DITIEN="ADD"_$P(DITIEN,"+",2,999) ;"+1," --> "ADD1,"
        IF DIFIEN'["," SET DIFIEN=DIFIEN_","
        IF DITIEN'["," SET DITIEN=DITIEN_","
        KILL ^TMP("DIERR",$J)            
        DO TRNMRG^DIT3  ;"modifies DITIEN IF successful
        IF $G(DIERRROOT)'="" M @DIERRROOT@("DIERR")=^TMP("DIERR",$J) K ^TMP("DIERR",$J)
        IF DIFLG'["X" DO
        . NEW DA,IDX,J,DIK,DIERR
        . FOR IDX=1:1 SET J=$P(DITIEN,",",IDX) QUIT:'J  DO
        . . SET:IDX=1 DA=J 
        . . IF IDX>1 SET DA(IDX-1)=J
        . SET DIK=$$ROOT^DIQGU(DITFNO,DITIEN,"",1) Q:$D(DIERR)        
        . DO IX1^DIK ;"Just SET logic for 1 entry. 
        QUIT
        ;
GETCHLST(TMGRESULT,TMGPARAMS) ;"GET CHILD LIST
        ;"Purpose: HANDLE RPC CHANNEL REQUEST FOR: GET REMINDER DIALOG CHILDREN
        ;"Input:  TMGRESULT -- The result parameter
        ;"        PARAMS -- IEN^
        ;"              IEN - IEN in REMINDER DIALOG (801.41) file.
        ;"Output: TMGRESULT(0)=1^Success or -1^Message
        NEW IEN SET IEN=$PIECE(TMGPARAMS,"^",1)
        SET IEN=$GET(IEN) IF +IEN'>0 DO  GOTO GCHLDN
        . SET TMGRESULT(0)="-1^Invalid IEN. Got ["_IEN_"]"
        NEW ARRAY DO CHILDLST(+IEN,.ARRAY)
        NEW ONE SET ONE=""
        NEW IDX SET IDX=0
        FOR  SET ONE=$ORDER(ARRAY(ONE)) QUIT:(ONE="")  DO
        . SET IDX=IDX+1,TMGRESULT(IDX)=ONE
        SET TMGRESULT(0)="1^Success"
GCHLDN  QUIT                
        ;
CHILDLST(IEN,ARRAY)  ;"GET LIST OF CHILD ELEMENTS        
        ;"Purpose: Compile list of child (and grandchildren etc) elements
        ;"Input: IEN -- IEN in REMINDER DIALOG (801.41) file.
        ;"       ARRAY -- PASS BY REFERENCE.  An OUT parameter
        ;"Result: none.
        SET IEN=+$GET(IEN) IF IEN'>0 GOTO CLDN
        NEW SUBIEN SET SUBIEN=0
        FOR  SET SUBIEN=$ORDER(^PXRMD(801.41,IEN,10,SUBIEN)) QUIT:(+SUBIEN'>0)  DO
        . NEW PTR SET PTR=+$PIECE($GET(^PXRMD(801.41,IEN,10,SUBIEN,0)),"^",2) QUIT:PTR'>0
        . NEW NAME SET NAME=$PIECE($GET(^PXRMD(801.41,PTR,0)),"^",1) QUIT:NAME=""
        . SET ARRAY(NAME)=""
        . DO CHILDLST(PTR,.ARRAY)
CLDN    QUIT        
        ;
GETFFNSG(TMGRESULT,NAME)  ;"GET FUNC FINDING ARG SIGNATURE
        ;"Purpose:  HANDLE RPC CHANNEL REQUEST FOR: GET FUNC FINDING ARG SIGNATURE
        ;"Input:  TMGRESULT -- The result parameter
        ;"        NAME -- Name of function (.01 value for record in 802.4)
        ;"Output: TMGRESULT(0)=1^Success or -1^Message
        ;"        TMGRESULT(1)=<result>
        ;"<result> format:  X^X^X^X^X^....
        ;"          <X> is one of following codes: F=FINDING, N=NUMBER, S=STRING
        ;"          Piece position of code matches argument position.
        ;"          e.g. F^N^S  means 1st param is function, 2nd is number, 3rd is string.
        ;"NOTE: IF NAME is not found, then "" is returned.
        ;"Result : none.
        SET TMGRESULT(0)="1^Success",TMGRESULT(1)=""
        IF +$ORDER(^PXRMD(802.4,"B",NAME,0))'>0 GOTO GFFNSDN 
        NEW AN,CODE SET CODE=""
        FOR AN=1:1:100 DO  QUIT:CODE="U"
        . SET CODE=$$ARGTYPE^PXRMFFAT(NAME,AN)
        . IF (CODE="")!("FNS"'[CODE) SET CODE="U" QUIT
        . SET TMGRESULT(1)=TMGRESULT(1)_CODE_"^"        
GFFNSDN QUIT        
        
