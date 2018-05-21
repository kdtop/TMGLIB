TMGTIU11 ;TMG/kst-TIU PROBLEM LINK STUFF ; 06/22/15
         ;;1.0;TMG-LIB;**1,17**;05/15/15
 ;"
 ;"Kevin Toppenberg MD
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
 ;"PUBLIC FUNCTIONS
 ;"=======================================================================
 ;"
 ;"=======================================================================
 ;"PRIVATE FUNCTIONS
 ;"=======================================================================
 ;"
 ;"=======================================================================
 ;"=======================================================================
 ;"
SET(IEN8925,RECIEN)  ;"Set logic for XRef in file TIU PROBLEM LINK
  ;"Input: IEN8925 -- this is the IEN being set into .01 field
  ;"       RECIEN -- This is the IEN of the record in TIU PROBLEM LINK (IEN8925.9)
  NEW ADFN SET ADFN=+$PIECE($GET(^TIU(8925,IEN8925,0)),"^",2)
  IF ADFN>0 SET ^TIU(8925.9,"ATMGDOC",ADFN,IEN8925,RECIEN)=""
  QUIT
 ;"
KILL(IEN8925,RECIEN)  ;"Kill logic for XRef in file TIU PROBLEM LINK
  ;"Input: IEN8925 -- this is the IEN being REMOVED from .01 field
  ;"       RECIEN -- This is the IEN of the record in TIU PROBLEM LINK (IEN8925.9)
  NEW ADFN SET ADFN=+$PIECE($GET(^TIU(8925,IEN8925,0)),"^",2)
  KILL ^TIU(8925.9,"ATMGDOC",ADFN,IEN8925,RECIEN)
  QUIT
 ;" 
SETP(IENPROB,RECIEN)  ;"Set logic for XRef in file TIU PROBLEM LINK
  ;"Input: IENPROB -- this is the IEN being set into .02 field, from PROBLEM file, 9000011
  ;"       RECIEN -- This is the IEN of the record in TIU PROBLEM LINK (IEN8925.9)
  NEW IEN8925 SET IEN8925=+$PIECE($GET(^TIU(8925.9,RECIEN,0)),"^",1)
  NEW ADFN SET ADFN=+$PIECE($GET(^TIU(8925,IEN8925,0)),"^",2)
  IF ADFN>0 SET ^TIU(8925.9,"ATMGPROB",ADFN,IENPROB,RECIEN)=""
  QUIT
 ;"
KILLP(IENPROB,RECIEN)  ;"Kill logic for XRef in file TIU PROBLEM LINK
  ;"Input: IENPROB -- this is the IEN being REMOVED from .02 field, from PROBLEM file, 9000011
  ;"       RECIEN -- This is the IEN of the record in TIU PROBLEM LINK (IEN8925.9)
  NEW IEN8925 SET IEN8925=+$PIECE($GET(^TIU(8925.9,RECIEN,0)),"^",1)
  NEW ADFN SET ADFN=+$PIECE($GET(^TIU(8925,IEN8925,0)),"^",2)
  KILL ^TIU(8925.9,"ATMGPROB",ADFN,IENPROB,RECIEN)
  QUIT
 ;"
SETDP(IENPROB,RECIEN)  ;"Set logic for XRef in file TIU PROBLEM LINK
  ;"Input: IENPROB -- this is the IEN being set into .02 field, from PROBLEM file, 9000011
  ;"       RECIEN -- This is the IEN of the record in TIU PROBLEM LINK (IEN8925.9)
  NEW IEN8925 SET IEN8925=+$PIECE($GET(^TIU(8925.9,RECIEN,0)),"^",1)
  NEW ADFN SET ADFN=+$PIECE($GET(^TIU(8925,IEN8925,0)),"^",2)
  IF ADFN>0 SET ^TIU(8925.9,"ATMGDOCPROB",ADFN,IEN8925,IENPROB,RECIEN)=""
  QUIT
 ;"
KILLDP(IENPROB,RECIEN)  ;"Kill logic for XRef in file TIU PROBLEM LINK
  ;"Input: IENPROB -- this is the IEN being REMOVED from .02 field, from PROBLEM file, 9000011
  ;"       RECIEN -- This is the IEN of the record in TIU PROBLEM LINK (IEN8925.9)
  NEW IEN8925 SET IEN8925=+$PIECE($GET(^TIU(8925.9,RECIEN,0)),"^",1)
  NEW ADFN SET ADFN=+$PIECE($GET(^TIU(8925,IEN8925,0)),"^",2)
  KILL ^TIU(8925.9,"ATMGDOCPROB",ADFN,IEN8925,IENPROB,RECIEN)
  QUIT
 ;"  
SETPOV(IENPOV,RECIEN)  ;"Set logic for XRef in file TIU PROBLEM LINK
  ;"Input: IENPOV -- this is the IEN being set into .03 field, from PURPOSE OF VISIT file 9000010.07
  ;"       RECIEN -- This is the IEN of the record in TIU PROBLEM LINK (IEN8925.9)
  NEW IEN8925 SET IEN8925=+$PIECE($GET(^TIU(8925.9,RECIEN,0)),"^",1)
  NEW ADFN SET ADFN=+$PIECE($GET(^TIU(8925,IEN8925,0)),"^",2)
  IF ADFN>0 SET ^TIU(8925.9,"ATMGPOV",ADFN,IENPOV,RECIEN)=""
  QUIT
 ;"
KILLPOV(IENPOV,RECIEN)  ;"Kill logic for XRef in file TIU PROBLEM LINK
  ;"Input: IENPOV -- this is the IEN being REMOVED from .03 field, from PURPOSE OF VISIT file 9000010.07
  ;"       RECIEN -- This is the IEN of the record in TIU PROBLEM LINK (IEN8925.9)
  NEW IEN8925 SET IEN8925=+$PIECE($GET(^TIU(8925.9,RECIEN,0)),"^",1)
  NEW ADFN SET ADFN=+$PIECE($GET(^TIU(8925,IEN8925,0)),"^",2)
  KILL ^TIU(8925.9,"ATMGPOV",ADFN,IENPOV,RECIEN)
  QUIT
 ;" 
PROBLINK(OUT,IN)  ;"Read or ADD problem links.  Code for RPC: TMG CPRS PROBLEM LINK
  ;"This deals with read and writing to file TIU PROBLEM LINK 8925.9
  ;"Input: OUT -- PASS BY REFERENCE.  AN OUT PARAMETER.  SEE FORMAT BELOW
  ;"       IN -- PASS BY REFERENCE.  Format:
  ;"            IN("PATIENT")=<DFN>   <-- required
  ;"            IN("READ","PROB",<IEN>)="" <-- Request for return of links to problem. <IEN> = IENIEN9000011
  ;"                 - and / or -
  ;"            IN("READ","DOC",<IEN>)=""  <-- Request for return of links to document. <INE> = IEN8925
  ;"                 - and / or -
  ;"            IN("READ","POV",<IEN>)=""  <-- Request for return of links to Problem Of Visit (POV). <IEN> = IEN9000010.07 
  ;"                 - and / or -
  ;"            IN("ADD",<AnIndex#>,"DOC")=<DOC IEN>    <-- required
  ;"            IN("ADD",<AnIndex#>,"PROB")=<PROB IEN>  <-- required
  ;"            IN("ADD",<AnIndex#>,"POV")=<POV IEN>    <-- optional
  ;"Output.  OUT is filled as follows, depending on input parameters. 
  ;"   OUT(0) = results of call.  Format:  1^OK  or -1^Error message
  ;"    --in case when READ was given in input --
  ;"   OUT(#)= 'READ^<LINK IEN>^<0 node of record> (which is: IEN8925^ProbIEN^POVIEN^TermIEN^Narrative^ICDIEN)
  ;"   OUT(#)= 'DOC^<IEN8925>^FMDate of Episode Begin Date^AuthorName^Subject
  ;"    --in case when ADD was given in input --
  ;"   OUT(#)= 'ADD^<PassedIndex#>^<Result: 1 or -1>^<OK, or error message>
  ;"Result: none
  ;
  NEW TMGZZDB SET TMGZZDB=0
  IF TMGZZDB=1 DO
  . KILL IN
  . MERGE IN=^TMG("TMP","RPC","PROBLINK^TMGTIU11","IN")
  ELSE  DO
  . KILL ^TMG("TMP","RPC","PROBLINK^TMGTIU11")
  . MERGE ^TMG("TMP","RPC","PROBLINK^TMGTIU11","IN")=IN
  NEW PLIEN  
  NEW COUNT SET COUNT=0
  KILL OUT 
  NEW RESULT SET RESULT="1^OK"
  NEW DFN SET DFN=+$GET(IN("PATIENT"))
  IF DFN'>0 SET RESULT="-1^DFN NOT PROVIDED" GOTO PLDN
  ;"CYCLE THROUGH READ SECTION
  NEW PROBIEN,DOCIEN,POVIEN SET (PROBIEN,DOCIEN,POVIEN)=0
  FOR  SET PROBIEN=$ORDER(IN("READ","PROB",PROBIEN)) QUIT:PROBIEN'>0  DO
  . DO READSEC(DFN,PROBIEN,"ATMGPROB",.COUNT,.OUT)
  FOR  SET DOCIEN=$ORDER(IN("READ","DOC",DOCIEN)) QUIT:DOCIEN'>0  DO
  . DO READSEC(DFN,DOCIEN,"ATMGDOC",.COUNT,.OUT)
  FOR  SET POVIEN=$ORDER(IN("READ","POV",POVIEN)) QUIT:POVIEN'>0  DO
  . DO READSEC(DFN,POVIEN,"ATMGPOV",.COUNT,.OUT)
  ;"CYCLE THROUGH ADD SECTION
  NEW TMGFDA,TMGMSG,TMGIENS,PLIEN
  NEW IDX SET IDX=0
  FOR  SET IDX=$ORDER(IN("ADD",IDX)) QUIT:IDX'>0  DO
  . SET COUNT=COUNT+1
  . KILL TMGFDA
  . SET TMGIENS="+1,"
  . NEW IEN8925 SET IEN8925=+$GET(IN("ADD",IDX,"DOC"))
  . IF IEN8925'>0 DO  QUIT
  . . SET OUT(COUNT)="ADD^"_IDX_"^-1^IEN 8925 not provided."
  . . SET RESULT="-1^Error, see details."
  . NEW ADFN SET ADFN=$PIECE($GET(^TIU(8925,IEN8925,0)),"^",2)
  . IF ADFN'>0 DO  QUIT
  . . SET OUT(COUNT)="ADD^"_IDX_"^-1^Unable to find patient IEN in file# 8925, Rec# "_IEN8925
  . . SET RESULT="-1^Error, see details."
  . SET TMGFDA(8925.9,TMGIENS,.01)=IEN8925
  . NEW IENPROB SET IENPROB=+$GET(IN("ADD",IDX,"PROB"))
  . IF IENPROB'>0 DO  QUIT
  . . SET OUT(COUNT)="ADD^"_IDX_"^-1^IEN for PROBLEM file not provided."
  . . SET RESULT="-1^Error, see details."
  . SET TMGFDA(8925.9,TMGIENS,.02)=IENPROB
  . NEW ADFN2 SET ADFN2=$PIECE($GET(^AUPNPROB(IENPROB,0)),"^",2)
  . IF ADFN2'=ADFN DO  QUIT
  . . SET OUT(COUNT)="ADD^"_IDX_"^-1^Patient for document (DFN="_ADFN_") not same as patient for problem (DFN="_ADFN2_")"
  . . SET RESULT="-1^Error, see details."
  . NEW PRIORREC SET PRIORREC=+$ORDER(^TIU(8925.9,"ATMGDOCPROB",ADFN,IEN8925,IENPROB,0)) 
  . IF PRIORREC>0 DO  QUIT
  . . SET OUT(COUNT)="ADD^"_IDX_"^-1^Record already exists (#"_PRIORREC_")"
  . . SET RESULT="-1^Error, see details."
  . NEW IENPOV SET IENPOV=+$GET(IN("ADD",IDX,"POV"))
  . IF IENPOV>0 SET TMGFDA(8925.9,TMGIENS,.03)=IENPOV
  . NEW TMGIEN
  . DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
  . IF $DATA(TMGMSG("DIERR")) DO  QUIT
  . . SET OUT(COUNT)="ADD^"_IDX_"^-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
  . . SET RESULT="-1^Error, see details."
  . SET OUT(COUNT)="ADD^"_IDX_"^1^OK"
PLDN ;
  MERGE ^TMG("TMP","RPC","PROBLINK^TMGTIU11","OUT")=OUT
  SET OUT(0)=RESULT
  QUIT
  ;"
READSEC(DFN,IEN,XREF,COUNT,OUT)  ;
  ;"IEN can be PROBIEN, POVIEN, OR DOCIEN
  ;"If XREF="ATMGPROB", then have ^TIU(8925.9,"ATMGPROB",ADFN,IEN9000011,IEN8925.9)
  ;"If XREF="ATMGDOC", then have ^TIU(8925.9,"ATMGDOC",ADFN,IEN8925,IEN8925.9)=""
  ;"If XREF="ATMGPOV", then have ^TIU(8925.9,"ATMGPOV",ADFN,IEN9000010.07,IEN8925.9)=""
  NEW PLIEN SET PLIEN=0  ;"PLIEN is IEN8925.9
  FOR  SET PLIEN=$ORDER(^TIU(8925.9,XREF,DFN,IEN,PLIEN)) QUIT:PLIEN'>0  DO
  . NEW ZN SET ZN=$GET(^TIU(8925.9,PLIEN,0))
  . NEW IEN8925 SET IEN8925=+ZN
  . NEW PTR2PAT SET PTR2PAT=+$PIECE($GET(^TIU(8925,IEN8925,0)),"^",2)
  . IF ($DATA(^TIU(8925,IEN8925))'>0)!(IEN8925=0)!(PTR2PAT'=DFN) DO  QUIT  ;"Linked document may be invalid
  . . NEW DIK,DA SET DIK="^TIU(8925.9,",DA=PLIEN
  . . DO ^DIK   ;"kill record linking to missing document.
  . . KILL ^TIU(8925.9,XREF,DFN,IEN,PLIEN)  ;"for some reason ^DIK didn't kill this XRef entry 
  . SET COUNT=COUNT+1
  . SET OUT(COUNT)="READ^"_PLIEN_"^"_ZN
  . NEW SDATE SET SDATE=$PIECE($GET(^TIU(8925,IEN8925,0)),"^",7)
  . NEW AUTHIEN SET AUTHIEN=+$PIECE($GET(^TIU(8925,IEN8925,12)),"^",2)
  . NEW AUTHOR SET AUTHOR=$PIECE($GET(^VA(200,AUTHIEN,0)),"^",1)
  . NEW SUBJECT SET SUBJECT=$PIECE($GET(^TIU(8925,IEN8925,17)),"^",1)  
  . SET COUNT=COUNT+1
  . SET OUT(COUNT)="DOC^"_IEN8925_"^"_SDATE_"^"_AUTHOR_"^"_SUBJECT
  QUIT
  ;