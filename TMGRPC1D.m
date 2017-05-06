TMGRPC1D ;TMG/kst-RPC Functions ;05/09/12, 2/2/14
         ;;1.0;TMG-LIB;**1**;07/21/10

 ;"TMG RPC FUNCTIONS especially related to imaging.
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
 ;"CONFIG -- Set up imaging site parameters, so that TMG-CPRS works.
 ;"TESTCFG -- Test configuration
 ;"CHKUSRS -- Check users to ensure they are in correct, default, instituion
 ;"ASTROIMG -- Entry point for Astronaut installer, to SET the images folder path to an appropriate entry, depending on the particular flavor Astronaut
 ;"PINST1 - entry point for POST-INSTALL routine for patch TMG-CPRS-IMAGING*1.0*1
 ;
 ;"=======================================================================
 ;"PRIVATE API FUNCTIONS
 ;"=======================================================================
 ;"GETINST(TMGAUTO) -- return IEN of INSTITUTION to use.
 ;"GETISP(INSTPTR,TMGAUTO) --return IMAGING SITE PARAMETERS (file #2006.1) for Institution Name
 ;"GETNL(IMGSPPTR,TMGAUTO) -get NETWORK LOCATION (file #2005.2) stored in IMAGING SITE PARAMETERS (#2006.1) record
 ;"CHKNL(LOCPTR,IMGSPPTR,TMGDIV,TMGSTORE,TMGDROP) -- Check record in NETWORK LOCATION (file #2005.2)
 ;"GETND(LOCPTR,TMGAUTO) - Set up NODE DIVIDER. the symbol used to separt folders in a path
 ;"GETDBX(LOCPTR,TMGAUTO) -- Set up Drop box path
 ;"GETSTORE(LOCPTR,TMGAUTO) -- Set up server STORE path
 ;"CHK1USR(TMGUSRIEN,TMGFIXINST) -- Check config for 1 user
 ;"NEEDSFIX(TMGIEN200,TMGINSTPTR) --determine IF given user needs to be fixed to match specified INSTITUTION
 ;"ASKFIX(TMGIEN200,TMGUSRNAME,TMGINSTPTR) --see how user wants to handle changes to members of file 200
 ;"FIX1USR(TMGIEN200,TMGINSTPTR) --Force user to have one, and only one INSTITUTION defined in DIVISION multiple
 ;
 ;"=======================================================================
 ;"Dependancies: TMGKERNL,TMGUSRIF,TMGDEBUG
 ;"=======================================================================
 ;
 ;"NOTICE:
 ;"  User's INSTITUTION (file #4) is determined by VistA and stored in DUZ(2)
 ;"    This comes from either their DIVISION entry in file 200, or IF none
 ;"    is specified, from default instituion in KERNEL SYSTEM PARAMETERS.
 ;"  From INSTITUTION (file #4) record --> IMAGING SITE PARAMETERS (file #2006.1) record
 ;"  From IMAGING SITE PARAMETERS (file #2006.1) record --> NETWORK LOCATION (#2005.2)
 ;"  From NETWORK LOCATION (#2005.2) --> actual storage locations on HFS
 ;
CONFIG ;
        ;"Purpose: Set up imaging site parameters, so that TMG-CPRS works.
        ;"Input: None
        ;"Globally-scoped vars:  If TMGAUTO=1 then process is not interactive. -- Killed on exit.
        ;"Results: none
        WRITE "   ------------------------------------------",!
        WRITE "   -      Configuration of TMG Imaging      -",!
        WRITE "   ------------------------------------------",!,!
        ;
        NEW %,DA,DR,DIE,DIC,X,Y,DIK
        NEW TMGFDA,TMGMSG,TMGIEN,TMGDIV,TMGDROP,TMGSTORE,TMGNODIV,TMGZN
        NEW INSTPTR,IMGSPPTR
        NEW TMGABORT SET TMGABORT=0
        SET TMGAUTO=+$GET(TMGAUTO)
        ;
        ;"Get default INSTITUTION, stored in KERNEL SYSTEM PARAMETERS file (8989.3)
        SET INSTPTR=$$GETINST(TMGAUTO)
        IF +INSTPTR=-1 WRITE $PIECE(INSTPTR,"^",2),! GOTO ABORT
        ;
        ;"Get IMAGING SITE PARAMETERS (file #2006.1) for Institution
        SET IMGSPPTR=$$GETISP(INSTPTR,TMGAUTO) ;
        IF +IMGSPPTR=-1 WRITE $PIECE(INSTPTR,"^",2),! GOTO ABORT
        ;
        ;"Get NETWORK LOCATION (file #2005.2) stored from IMAGING SITE PARAMETERS (#2006.1)
        NEW LOCPTR SET LOCPTR=$$GETNL(IMGSPPTR,TMGAUTO) ;
        IF +LOCPTR=-1 WRITE $PIECE(LOCPTR,"^",2),! GOTO ABORT
        ;
        SET TMGDIV=$$GETND(LOCPTR,TMGAUTO) ;
        IF +TMGDIV=-1 WRITE $PIECE(TMGDIV,"^",2),! GOTO ABORT
        ;
        SET TMGDROP=$$GETDBX(LOCPTR,TMGAUTO) ;
        IF +TMGDROP=-1 WRITE $PIECE(TMGDROP,"^",2),! GOTO ABORT
        ;
        SET TMGSTORE=$$GETSTORE(LOCPTR,TMGAUTO) ;
        IF +TMGSTORE=-1 WRITE $PIECE(TMGSTORE,"^",2),! GOTO ABORT
        ;
       ;"Next force field 1 (PHYSICAL REFERENCE) to be same as TMGDIV
        ;"IF $PIECE($GET(^MAG(2005.2,LOCPTR,0)),"^",2)=TMGDIV GOTO CF9
        ;"SET DIK="^MAG(2005.2,"
        ;"SET DA=LOCPTR
        ;"DO ^DIK  ;"Kill prior entry.  Leaves DIK and DA unchanged
        ;"Note: Input transform doesn't allow the value I put in here.
        ;"SET $PIECE(^MAG(2005.2,LOCPTR,0),"^",2)=TMGDIV ;"NOTE!! Low-level write
        ;"SET DIK(1)=1 ;"Field 1 = PHYSICAL REFERENCE
        ;"DO EN^DIK ;"Reindex field, to populate crossrefences with NEW value.
        ;
        SET TMGABORT=$$CHKNL(LOCPTR,IMGSPPTR,TMGDIV,TMGSTORE,TMGDROP)
        IF +TMGABORT'=0 WRITE $PIECE(TMGABORT,"^",2) GOTO ABORT
        ;
        WRITE !,"Done with configuration.",!,!
        DO TESTCFG
        GOTO CFDN
        ;
ABORT   WRITE "Aborting configuration process.",!
        WRITE "Try again later, using 'DO CONFIG^TMGRPC1D'",!
CFDN    KILL TMGAUTO
        QUIT
        ;
        ;
GETINST(TMGAUTO) ;
        ;"Purpose: return IEN of INSTITUTION to use.
        ;"Input: TMGAUTO.  Optional.  If 1, then no user interaction.
        ;"Results: IEN in file 4, or -1^Message IF problem.
        NEW TMGRESULT SET TMGRESULT="-1^Unknown"
GI1     SET TMGRESULT=+$PIECE($GET(^XTV(8989.3,1,"XUS")),"^",17)  ;"Ptr to file $4 (Institution)
        IF TMGRESULT>0 DO  GOTO GISTDN
        . WRITE "Setting up using DEFAULT INSTITUTION: ",$$GET1^DIQ(4,TMGRESULT,.01),!
        . WRITE " from file KERNEL SYSTEM PARAMETERS",!
        WRITE "No value for DEFAULT INSTITUTION found in field 217 in file KERNEL SYSTEM PARAMETERS",!
        IF $GET(TMGAUTO) DO  GOTO ABORT
        . SET TMGRESULT="-1^Unable to configure TMG-CPRS imaging due to DEFAULT INSTITUTION not SET up"
        . WRITE $PIECE(TMGRESULT,"^",2),!
        . HANG 5
        WRITE "Edit settings now to correct this"
        SET %=1 DO YN^DICN WRITE !
        IF %'=1 SET TMGRESULT="-1^User aborted" GOTO GISTDN
        SET DA=1,DR="[XUSITEPARM]",DIE=8989.3
        DO XUDIE^XUS5  ;"Launch screenman form to edit KERNEL SYSTEM PARAMETERS.
        GOTO GI1
GISTDN  QUIT TMGRESULT
        ;
        ;
GETISP(INSTPTR,TMGAUTO) ;
        ;"Purpose: IMAGING SITE PARAMETERS (file #2006.1) for Institution Name
        ;"Input: INSTPTR -- IEN in file #4 (INSTITUTION)
        ;"       TMGAUTO.  Optional.  If 1, then no user interaction.
        ;"Results: IEN in file 2006.1, or -1^Message IF problem.
        ;
        NEW X,Y,DIC,DIE
        NEW TMGFDA,TMGMSG,TMGZN
        NEW TMGRESULT SET TMGRESULT="-1^Unknown error"
GISP1   SET TMGRESULT=+$ORDER(^MAG(2006.1,"B",INSTPTR,0))
        IF TMGRESULT>0 DO
        . WRITE "Using IMAGING SITE PARAMETERS, IEN #",TMGRESULT,": ",$$GET1^DIQ(2006.1,TMGRESULT,.01),!
        . KILL TMGFDA,TMGMSG
        . SET TMGZN=$GET(^MAG(2006.1,TMGRESULT,0))
        . IF $PIECE(TMGZN,"^",2)="" SET TMGFDA(2006.1,TMGRESULT_",",.02)="ZZZ"
        . IF $PIECE(TMGZN,"^",9)="" SET TMGFDA(2006.1,TMGRESULT_",",.09)="ZZZ"
        . IF $DATA(TMGFDA)=0 QUIT
        . DO FILE^DIE("K","TMGFDA","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) DO
        . . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        IF +TMGRESULT'=0 GOTO GIPDN
        ;"Try to fix IMAGING SITE PARAMETERS problem
        IF $GET(TMGAUTO) DO  GOTO GIPDN
        . WRITE "!!! Unable to configure TMG-CPRS imaging because",!
        . WRITE "a valid IMAGING SITE PARAMATERS was not found for INSTITUTION !!!"
        . HANG 5
        . SET TMGRESULT="-1^Unable to auto-configure"
        ;
GI1B    WRITE !,"Next, a entry in IMAGING SITE PARAMENTERS file must be linked in.",!
        WRITE "Please select entry to use, or add a NEW one IF needed.",!
        DO PRESS2GO^TMGUSRI2
        SET DIC=2006.1,DIC(0)="MAEQL"
        DO ^DIC WRITE !
        IF Y>0 SET TMGRESULT=+Y GOTO GI2
        WRITE "Valid entry in IMAGING SITE PARAMETERS file not selected.",!
        SET %=1  ;"Yes
        WRITE "Try Again" DO YN^DICN WRITE !
        IF %=1 GOTO GI1B
        SET TMGRESULT="-1^User aborted"
        GOTO GIPDN
GI2     KILL TMGFDA,TMGMSG
        SET TMGFDA(2006.1,TMGRESULT_",",.01)=INSTPTR
        DO FILE^DIE("K","TMGFDA","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO GIPDN
        . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        GOTO GISP1 ;"loop back just be sure the B index is setup.
        ;
GIPDN   QUIT TMGRESULT
        ;
        ;
GETNL(IMGSPPTR,TMGAUTO) ;
        ;"Purpose: get NETWORK LOCATION (file #2005.2) stored in IMAGING SITE PARAMETERS (#2006.1) record
        ;"Input: IMGSPPTR -- IEN in IMAGING SITE PARAMETERS (#2006.1) record
        ;"       TMGAUTO --  Optional.  If 1, then no user interaction.
        ;"Results: IEN in file 2005.2, or -1^Message IF problem.

        NEW TMGRESULT,TMGFDA,TMGMSG,TMGIEN
GNL0    SET TMGRESULT=+$PIECE($GET(^MAG(2006.1,IMGSPPTR,0)),"^",3)
        IF TMGRESULT>0 DO  GOTO GNLDN
        . WRITE "Using NETWORK LOCATION, IEN #",TMGRESULT,": ",$$GET1^DIQ(2005.2,TMGRESULT,.01),!
        ;"Try to fix missing network location
        SET DIC=2005.2,DIC(0)="M"  ;"NETWORK LOCATION (file #2005.2)
        SET X="TMG IMAGING NET LOCATION"
        DO ^DIC WRITE !
        IF Y>-1 SET (TMGRESULT,LOCPTR)=+Y GOTO GNL2
        ;"Create NEW NETWORK LOCATION record
        SET TMGFDA(2005.2,"+1,",.01)="TMG IMAGING NET LOCATION"
        SET TMGFDA(2005.2,"+1,",.04)=IMGSPPTR
        SET TMGFDA(2005.2,"+1,",1)="/"
        SET TMGFDA(2005.2,"+1,",5)=1   ;"1=On-line
        SET TMGFDA(2005.2,"+1,",6)="MAG"
        SET TMGFDA(2005.2,"+1,",22701)="/"
        DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO GNLDN
        . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        SET TMGRESULT=+$GET(TMGIEN(1))
        IF TMGRESULT>0 GOTO GNL2
        SET TMGRESULT="-1^Unable to automatically add NEW NETWORK LOCATION."
        IF TMGAUTO GOTO GNLDN
        WRITE $PIECE(TMGRESULT,"^",2),!
        SET %=1
        WRITE "Try to manually pick one" DO YN^DICN WRITE !
        IF %'=1 SET TMGRESULT="-1^User Aborted" GOTO GNLDN
GNL1    WRITE "An entry in NETWORK LOCATION file must be linked in.",!
        WRITE "Please select entry to use, or add a NEW one IF needed.",!
        SET DIC=2005.2,DIC(0)="MAEQL"
        DO ^DIC WRITE !
        IF Y>-1 SET (TMGRESULT,LOCPTR)=+Y GOTO GNL2
        WRITE "Valid entry in NETWORK LOCATION file not selected.",!
        SET %=1
        WRITE "Try Again" DO YN^DICN WRITE !
        IF %'=1 SET TMGRESULT="-1^User Aborted" GOTO GNLDN
        GOTO GNL1
GNL2    ;"Edit IMAGING SITE PARAMETERS (#2006.1) record so that it points to NEW NETWORK LOCATION (file #2005.2)
        KILL TMGFDA,TMGMSG
        SET TMGRESULT=+TMGRESULT
        SET TMGZN=$GET(^MAG(2006.1,IMGSPPTR,0))
        IF $PIECE(TMGZN,"^",2)="" SET TMGFDA(2006.1,IMGSPPTR_",",.02)="ZZZ"
        IF $PIECE(TMGZN,"^",9)="" SET TMGFDA(2006.1,IMGSPPTR_",",.09)="ZZZ"
        SET TMGFDA(2006.1,IMGSPPTR_",",.03)=TMGRESULT
        SET TMGFDA(2006.1,IMGSPPTR_",",.08)=TMGRESULT
        SET TMGFDA(2006.1,IMGSPPTR_",",2.03)=TMGRESULT
        DO FILE^DIE("K","TMGFDA","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO GNLDN
        . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        GOTO GNL0 ;"loop back just be sure SET up correctly
        ;
GNLDN   QUIT TMGRESULT
        ;
CHKNL(LOCPTR,IMGSPPTR,TMGDIV,TMGSTORE,TMGDROP) ;
        ;"Purpose: Check record in NETWORK LOCATION (file #2005.2)
        ;"Input: LOCPTR -- record in NETWORK LOCATION (file #2005.2)
        ;"       IMGSPPTR -- IEN in IMAGING SITE PARAMETERS (#2006.1) record
        ;"       TMGDIV -- Node divider symbol
        ;"       TMGSTORE -- Private physical reference
        ;"       TMGDROP -- dropbox physical refeerence
        ;"Results: 0 IF OK, or -1^Message IF problem.
        NEW TMGRESULT SET TMGRESULT=0
        WRITE "Using NETWORK LOCATION IEN #",LOCPTR,", ",$$GET1^DIQ(2005.2,LOCPTR,.01),!
        NEW TMGFDA,TMGMSG,TMGZN
        NEW TMGIENS SET TMGIENS=LOCPTR_","
        SET TMGZN=$GET(^MAG(2005.2,LOCPTR,0))
        IF $PIECE(TMGZN,"^",10)'=IMGSPPTR SET TMGFDA(2005.2,TMGIENS,.04)=IMGSPPTR
        IF $PIECE(TMGZN,"^",6)'=1 SET TMGFDA(2005.2,TMGIENS,5)=1  ;"1=On-Line
        IF $PIECE(TMGZN,"^",7)'="MAG" SET TMGFDA(2005.2,TMGIENS,6)="MAG"   ;"MAGNETIC
        IF $PIECE(TMGZN,"^",2)'=TMGDIV SET TMGFDA(2005.2,TMGIENS,1)=TMGDIV
        IF $PIECE($GET(^MAG(2005.2,LOCPTR,22700)),"^",1)'=TMGSTORE DO
        . SET TMGFDA(2005.2,TMGIENS,22700)=TMGSTORE
        IF $PIECE($GET(^MAG(2005.2,LOCPTR,22701)),"^",1)'=TMGDIV DO
        . SET TMGFDA(2005.2,TMGIENS,22701)=TMGDIV
        IF $PIECE($GET(^MAG(2005.2,LOCPTR,22702)),"^",1)'=TMGDROP DO
        . SET TMGFDA(2005.2,TMGIENS,22702)=TMGDROP
        IF $DATA(TMGFDA)=0 GOTO CNLDN
        DO FILE^DIE("K","TMGFDA","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO  GOTO CNLDN
        . SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
CNLDN   QUIT TMGRESULT
        ;
GETND(LOCPTR,TMGAUTO) ;
        ;"Purpose: Set up NODE DIVIDER. the symbol used to separt folders in a path
        ;"Input:LOCPTR -- record in NETWORK LOCATION (file #2005.2)
        ;"       TMGAUTO --  Optional.  If 1, then no user interaction.
        ;"Result: Returns node divider, or "" IF no change, or -1^Message IF error
        NEW TMGRESULT SET TMGRESULT=""
        WRITE !,"A NODE DIVIDER is the symbol used to separt folders in a path",!
        WRITE "  UNIX server uses '/'  (e.g. '/opt/var/me')",!
        WRITE "  Windows server uses '\' (e.g. 'c:\temp\me')",!
        NEW % SET %=1
        ;"NEW TMGNODIV SET TMGNODIV=0
        SET TMGDIV=$$GET1^DIQ(2005.2,LOCPTR,22701)
        IF TMGDIV="",TMGAUTO DO  GOTO GNDDN
        . SET TMGDIV="/"  ;"Defaults to linux system in AUTO mode (Astronaut)
        IF TMGDIV'="" DO  GOTO:(TMGAUTO) GNDDN
        . WRITE "Current Node divider= '",TMGDIV,"'"
        . SET %=2
        . IF TMGDIV="/" WRITE " (UNIX filesystem)",!
        . ELSE  IF TMGDIV="\" WRITE " (WINDOWS filesystem)",!
        . ELSE  WRITE " (?? filesystem)",! SET %=1
        WRITE "Do you want to specify a NODE DIVIDER" DO YN^DICN WRITE !
        IF %=-1 SET TMGRESULT="-1^User Aborted" GOTO GNDDN
        ;"IF %=2 SET TMGNODIV=1 GOTO GNDDN
        IF %=2 GOTO GNDDN
        ;
        WRITE "Is the server running on a Linux/Unix box" DO YN^DICN WRITE !
        IF %=-1 SET TMGRESULT="-1^User Aborted" GOTO GNDDN
        IF %=1 SET TMGDIV="/"
        ELSE  SET TMGDIV="\"
GNDDN   IF +TMGRESULT'=-1 SET TMGRESULT=TMGDIV
        ;"IF (TMGNODIV'=0),(TMGRESULT="") SET TMGRESULT=TMGDIV
        QUIT TMGRESULT
        ;
GETDBX(LOCPTR,TMGAUTO) ;
        ;"Purpose: Set up Drop box path
        ;"Input:LOCPTR -- record in NETWORK LOCATION (file #2005.2)
        ;"       TMGAUTO --  Optional.  If 1, then no user interaction.
        ;"Result: Returns drop box path, or "" IF none chose, or -1^Message IF error
        ;
        NEW %,TMGDROP
        NEW TMGRESULT SET TMGRESULT=""
        IF TMGAUTO SET TMGDROP="" GOTO GDBXDN
GDBX0   WRITE !,"A DROPBOX is a file folder where the server may place files for",!
        WRITE "pick up by a client (i.e. CPRS).  This folder could be on a ",!
        WRITE "separate file system (e.g. a windows file system mounted into",!
        WRITE "the server file system.)  This is a security measure that negates",!
        WRITE "a need for the client to have read access to the entire images",!
        WRITE "folder.  A dropbox path is only required IF client is configured",!
        WRITE "to use it.",!
        SET %=1
        SET TMGDROP=$$GET1^DIQ(2005.2,LOCPTR,22702)
        IF TMGDROP'="" DO
        . WRITE "Current DROPBOX: ",TMGDROP,!,!
        . SET %=2
        WRITE "Do you want to specify a DROPBOX FOLDER" DO YN^DICN WRITE !
        IF %=-1 SET TMGRESULT="-1^User Aborted" GOTO GDBXDN
        ;"IF %=2 SET TMGDROP="" GOTO GDBXDN
        IF %=2 GOTO GDBXDN
        ;
GDBX1   WRITE "Enter full path of the DROPBOX is it would be accessed on the ",!
        WRITE "server (**NOT the path that the client would use**)",!
        READ "Enter full DROPBOX path (^ to abort): ",TMGDROP:DTIME,!
        IF TMGDROP="^" SET TMGRESULT="-1^User Aborted" GOTO GDBXDN
        IF TMGDROP="" WRITE ! GOTO GDBX0
        IF $$ISDIR^TMGKERNL(TMGDROP,TMGDIV)=1 GOTO GDBX1
        WRITE "ERROR: Path specified is not valid.  Does folder exist?",!,!
        GOTO GDBX0
GDBXDN  IF +TMGRESULT'=-1 SET TMGRESULT=TMGDROP
        QUIT TMGRESULT
        ;
GETSTORE(LOCPTR,TMGAUTO) ;
        ;"Purpose: Set up server STORE path
        ;"Input:LOCPTR -- record in NETWORK LOCATION (file #2005.2)
        ;"       TMGAUTO --  Optional.  If 1, then no user interaction.
        ;"Result: Returns storage path, or "" IF none chose, or -1^Message IF error
        ;
        NEW TMGRESULT SET TMGRESULT=""
        NEW %,TMGSTORE
GSR0    WRITE !,"A STORE PATH is the file folder that the server will use to",!
        WRITE "store images.  This should be a complete and valid path.",!
        SET %=1
        SET TMGSTORE=$$GET1^DIQ(2005.2,LOCPTR,22700)
        IF TMGSTORE'="" DO  GOTO:(TMGAUTO) GETSRDN
        . WRITE "Current image file storage path: ",TMGSTORE,!
        . SET %=2
        . IF TMGAUTO SET TMGSTORE="" ;"Signal to NOT store on record write.
        ELSE  IF TMGAUTO GOTO GSR1
        WRITE "Do you want to specify a STORE FOLDER" DO YN^DICN WRITE !
        IF %=-1 SET TMGRESULT="-1^User Aborted" GOTO GETSRDN
        ;"IF %=2 SET TMGSTORE="" GOTO GETSRDN
        IF %=2 GOTO GETSRDN
        ;
GSR1    SET X="",%=0,TMGSTORE=""
        IF TMGDIV="/" DO  GOTO:(%=-1) ABORT
        . SET X=$ZROUTINES
        . SET X=$PIECE(X,"(",1)
        . SET X=$PIECE(X,"/",1,$LENGTH(X,"/")-1)_"/images/"
        . IF $$ISDIR^TMGKERNL(X)=0 SET X="" QUIT
        . SET %=1
        . IF 'TMGAUTO WRITE "Use recommended path: ",X DO YN^DICN WRITE !
        . IF %=1 SET TMGSTORE=X QUIT
        . IF %=-1 SET TMGRESULT="-1^User Aborted" QUIT
        IF TMGSTORE'="" GOTO GETSRDN
        IF TMGAUTO DO  GOTO GETSRDN
        . WRITE "!!! Unable to configure TMG-CPRS imaging because",!
        . WRITE "unable to determine appropriate STORE FOLDER.",!
        . HANG 5
        . SET TMGRESULT="-1^Unable to auto-config."
        READ "Enter store path (^ to abort): ",TMGSTORE:DTIME,!
        IF TMGSTORE="^" SET TMGRESULT="-1^User Aborted" GOTO GETSRDN
        IF TMGSTORE="" WRITE ! GOTO GSR0
        IF $$ISDIR^TMGKERNL(TMGSTORE,TMGDIV)=1 GOTO GETSRDN
        WRITE "ERROR: Path specified is not valid.  Does folder exist?",!,!
        GOTO GSR0
GETSRDN IF +TMGRESULT'=-1 SET TMGRESULT=TMGSTORE
        QUIT TMGRESULT
        ;
ASTROIMG ;
        ;"Purpose: Entry point for Astronaut installer, to SET the images
        ;"         folder path to an appropriate entry, depending on the particular
        ;"         flavor Astronaut
        ;"NOTE: This assumes that everything ELSE has already been configured
        ;"      in the distributed database file. Also, assumes Linux OS
        NEW X,TMGSTORE,TMGFDA,TMGIEN,TMGMSG,DIC,X,Y,LOCPTR
        SET DIC=2005.2,DIC(0)="M"
        SET X="TMG IMAGING NET LOCATION"
        DO ^DIC
        IF Y'>-1 DO  QUIT
        . WRITE "FAILED: Unable to find record in NETWORK LOCATION file 'TMG IMAGING NET LOCATION'",!
        SET LOCPTR=+Y
        SET X=$ZROUTINES
        SET X=$PIECE(X,"(",1)
        SET X=$PIECE(X,"/",1,$LENGTH(X,"/")-1)_"/images/"
        IF $$ISDIR^TMGKERNL(X)=0 DO  QUIT
        . WRITE "FAILED: directory does not exist -- ",X,!
        . SET %=1
        WRITE "Checking TMG IMAGING NET LOCATION record --> "
        SET TMGFDA(2005.2,LOCPTR_",",1)="/"
        SET TMGFDA(2005.2,LOCPTR_",",22700)=X
        SET TMGFDA(2005.2,LOCPTR_",",22701)="/"
        DO FILE^DIE("K","TMGFDA","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) DO
        . WRITE "FAILED:",!
        . DO SHOWDIER^TMGDEBU2(.TMGMSG)
        ELSE  WRITE "SUCCESS",!
        QUIT
        ;
        ;
TESTCFG ;"Purpose: Test configuration
        NEW LOCPTR SET LOCPTR=$$GETDEFNL^TMGRPC1C()
        IF LOCPTR'>0 DO  QUIT
        . WRITE "ERROR: Can't find NETWORK LOCATION to use",!
        WRITE "Storage path: ",$$GETLOCFPATH^TMGRPC1C("/"),!
        NEW DROPPATH
        IF $$GETDROPPATH^TMGRPC1C(LOCPTR,.DROPPATH)=-1 DO  ;"QUIT
        . WRITE "Unable to determine Dropbox path.  May not have been configured.",!
        ELSE  WRITE "Dropbox path: ",DROPPATH,!
        DO CHKUSRS
        QUIT
        ;
CHKUSRS ;"Purpose: Check users to ensure they are in correct, default, instituion
        NEW TMGUSRIEN SET TMGUSRIEN=0
        NEW TMGFIXINST SET TMGFIXINST=0
        IF $GET(TMGAUTO)=1 SET TMGFIXINST=1
        WRITE "Checking all users to ensure that they are connected to correct",!
        WRITE "INSTITUTION, which will allow correct IMAGING SITE PARAMETERS entry.",!
        WRITE "--------------------------",!
        FOR  SET TMGUSRIEN=$ORDER(^VA(200,TMGUSRIEN)) QUIT:(+TMGUSRIEN'>0)!(TMGFIXINST="^")  DO
        . NEW TMGRESULT SET TMGRESULT=$$CHK1USR(TMGUSRIEN,.TMGFIXINST)
CKUDN   QUIT
        ;
CHK1USR(TMGUSRIEN,TMGFIXINST) ;
        ;"Purpose: Check config for 1 user
        ;"Input: TMGUSRIEN -- IEN in file 200 (NEW PERSON) to check.
        ;"       TMGFIXINST -- PASS BY REFERENCE
        ;"                     If -1 --> don't fix, don't ask
        ;"                     IF 0 --> ask IF should fix
        ;"                     IF 1 --> auto fix.
        ;"Result: 1^OK IF OK, or -1^Message IF problem.,  or -2^Message IF user not in default institution
        NEW TMGRESULT SET TMGRESULT="1^OK"
        SET TMGUSRIEN=$GET(TMGUSRIEN)
        IF +TMGUSRIEN'>0 DO  GOTO C1UDN2
        . SET RESULT="-1^Valid user IEN not passed.  Got ["_TMGUSRIEN_"]"
        SET TMGUSRIEN=+TMGUSRIEN
        NEW TMGUSRNAME SET TMGUSRNAME=$PIECE($GET(^VA(200,TMGUSRIEN,0)),"^",1)
        ;"Above will first try to get INSTITUION  from file 200.  If not found, then
        ;"  pulls in default INSTITUTION from file 8989.3 (KERNEL SYSTEM PARAMETERS)
        NEW TMGINSTPTR SET TMGINSTPTR=+$PIECE($GET(^XTV(8989.3,1,"XUS")),"^",17)  ;"Ptr to file $4 (Institution)
        IF TMGINSTPTR'>0 DO  GOTO C1UDN
        . WRITE "No default instituion defined in KERNEL SYSTEM PARAMETERS.",!
        . WRITE "Try DO CONFIG^TMGRPC1D to work on this.",!
        . SET TMGFIXINST="^"
        ;"IF TMGINSTPTR'=$GET(DUZ(2)) DO  GOTO C1UDN
        IF $$NEEDSFIX(TMGUSRIEN,TMGINSTPTR) DO  GOTO C1UDN
        . WRITE "User [",TMGUSRNAME,"] in NEW PERSON file (#",TMGUSRIEN,") not SET up properly.",!
        . WRITE "  Should have only ONE institution defined",!
        . WRITE "  Institution should be: ",$PIECE($GET(^DIC(4,TMGINSTPTR,0)),"^",1),!
        . SET TMGRESULT="-2^Person has INSTITUTION defined that doesn't match default from KERNEL SYSTEM PARAMETERS"
        . IF TMGFIXINST=0 SET TMGFIXINST=$$ASKFIX(TMGUSRIEN,TMGUSRNAME,TMGINSTPTR)
        . IF (+TMGFIXINST<0)!(TMGFIXINST="^") QUIT
        . IF TMGFIXINST="0.5" SET TMGFIXINST=0  ;"ask again next time
        . SET TMGRESULT=$$FIX1USR(TMGUSRIEN,TMGINSTPTR)
        . IF +TMGRESULT=-1 DO  QUIT
        . . WRITE "ERROR when trying to fix user ",TMGUSRNAME,!
        . . WRITE "  ",$PIECE(TMGRESULT,"^",2),!
        . WRITE TMGUSRNAME," --> FIXED",!
        NEW TMGZ SET TMGZ=$$DRIVE^MAGGTU1(IMGSPPTR,TMGINSTPTR)
        IF +TMGZ=0 DO  GOTO C1UDN
        . WRITE $PIECE(TMGZ,"^",2),!
        WRITE TMGUSRNAME," --> OK",!
C1UDN   ;"KILL DUZ MERGE DUZ=TMGDUZSAVE
C1UDN2  QUIT TMGRESULT
        ;
NEEDSFIX(TMGIEN200,TMGINSTPTR) ;
        ;"Purpose: to determine IF given user needs to be fixed to match specified INSTITUTION
        ;"Input: TMGIEN200 -- IEN in 200 to check
        ;"       TMGINSTPTR -- IEN in file 4 that user should be linked to.
        ;"Result: 1 IF not properly matched, 0 IF OK
        NEW TMGRESULT SET TMGRESULT=1
        NEW TMGZN SET TMGZN=$GET(^VA(200,TMGIEN200,2,0)) IF TMGZN="" GOTO NFDN
        IF $PIECE(TMGZN,"^",4)'=1 GOTO NFDN ;"Check number of subfile records.  Should be exactly 1
        NEW TMGSUBIEN SET TMGSUBIEN=+$ORDER(^VA(200,TMGIEN200,2,0)) IF TMGSUBIEN'>0 GOTO NFDN
        SET TMGZN=$GET(^VA(200,TMGIEN200,2,TMGSUBIEN,0)) IF TMGZN="" GOTO NFDN
        IF +TMGZN'=TMGSUBIEN GOTO NFDN  ;"should be DINUMED (value of .01 field = record number)
        IF +TMGZN=TMGINSTPTR SET TMGRESULT=0
NFDN    QUIT TMGRESULT
        ;
ASKFIX(TMGIEN200,TMGUSRNAME,TMGINSTPTR) ;
        ;"Purpse: see how user wants to handle changes to members of file 200
        ;"Input: TMGIEN200: IEN in file 200, user that needs to be fixed
        ;"       TMGUSRNAME: Name of user that needs to be fixed
        ;"       TMGINSTPTR: IEN in file #4 that user should be linked to
        ;"Result.  Returns TMGFIXINST:  -1   --> don't fix, don't ask
        ;"                               0.5 --> fix just one user.
        ;"                               1   --> auto fix.
        ;"                               ^   --> abort
        NEW TMGRESULT SET TMGRESULT="^"
        NEW TMGINSTNAME SET TMGINSTNAME=$PIECE($GET(^DIC(4,TMGINSTPTR,0)),"^",1)
        NEW TMGMENU
        SET TMGMENU(0)=TMGUSRNAME_" not linked to "_TMGINSTNAME
        SET TMGMENU(1)="Don't fix (for ALL users during this run)."_$C(9)_"-1"
        SET TMGMENU(2)="Fix this one user."_$C(9)_"0.5"
        SET TMGMENU(3)="Automatically fix all users."_$C(9)_"1"
        SET TMGMENU(4)="Abort user check"_$C(9)_"^"
        SET TMGRESULT=$$MENU^TMGUSRI2(.TMGMENU,2)
        QUIT TMGRESULT
        ;
        ;
FIX1USR(TMGIEN200,TMGINSTPTR) ;
        ;"Purpose: Force user to have one, and only one INSTITUTION defined in DIVISION multiple
        ;"Input: TMGIEN200, user in file 200 to fix
        ;"       TMGINSTPTR -- the INSTITUTION to change user to.
        ;"Results: 1^OK IF OK, or -1^Message IF problem.
        NEW TMGRESULT SET TMGRESULT="1^OK"
        NEW TMGFDA,TMGMSG,TMGIEN
        SET TMGIEN=0
        ;"First, delete all prior entries
        FOR  SET TMGIEN=$ORDER(^VA(200,TMGIEN200,2,TMGIEN)) QUIT:(+TMGIEN'>0)!(+TMGRESULT=-1)  DO
        . SET TMGFDA(200.02,TMGIEN_","_TMGIEN200_",",.01)="@"
        . DO FILE^DIE("E","TMGFDA","TMGMSG")
        . IF $DATA(TMGMSG("DIERR")) SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
        IF +TMGRESULT=-1 GOTO F1UDN
        ;"Now add proper entry
        KILL TMGMSG,TMGIEN
        SET TMGFDA(200.02,"+1,"_TMGIEN200_",",.01)=TMGINSTPTR
        SET TMGFDA(200.02,"+1,"_TMGIEN200_",",1)=1 ;"YES" for default value
        SET TMGIEN(1)=TMGINSTPTR
        DO UPDATE^DIE("","TMGFDA","TMGIEN","TMGMSG")
        IF $DATA(TMGMSG("DIERR")) SET TMGRESULT="-1^"_$$GETERRST^TMGDEBU2(.TMGMSG)
F1UDN   QUIT TMGRESULT
        ;
PINST1 ;
        ;"Purpose: This is an entry point for POST-INSTALL routine for patch
        ;"         TMG-CPRS-IMAGING*1.0*1
        DO ENSUREAL^TMGRPC1B
        NEW TMGAUTO SET TMGAUTO=1
        DO CONFIG
        QUIT
        ;
