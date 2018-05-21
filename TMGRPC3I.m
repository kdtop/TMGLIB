TMGRPC3I ; COPY NEW PERSON SECONDARY OPTION SUBFILE RECORDS , 2/2/14
         ;;1.0;TMG-LIB;**1**;04/16/12
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
EN1(SOURCEIEN,DESTIEN)  ;
 ;
 ; SOURCEIEN   AS FROM SOURCE IEN IN NEW PERSON FILE
 ; DESTIEN     AS TO   DESTINATION IEN IN NEW PERSON FILE
 ;
 ; OUTPUT AS COPY OF "SECONDARY OPTION" FIELD 203 SUBFILE 200.03 RECORDS IN
 ; SOURCEIEN RECORD TO DESTIEN RECORD
 ;
 ;                              VA(200,D0,203,0)=^200.03IP^^  (#203)
 ; SECONDARY MENU OPTIONS
 ;                              ^VA(200,D0,203,D1,0)= (#.01) SECONDARY MENU
 ;                               OPTIONS [1P:19] ^ (#2) SYNONYM
 ;                              ==>[2F] ^
 ;
 ; EXAMPLE:
 ;                              ^VA(200,11,203,0)="^200.03IP^1^1"
 ;                              ^VA(200,11,203,1,0)=8552 <-----FOR EXAMPLE OR CPRS GUI CHART
 ;                              ^VA(200,11,203,"B",8552,1)=""
 ;                              ^VA(200,11,203.1)="62356,53649"
 ;
 N COUNT,OPTION,SYNONYM,OPTIONE,COUNT1,FOUND,NAME,IEN
 N TMGREC
 S TMGREC=$P(SOURCEIEN,",",1)
 S OPTION=0
 S COUNT1=0
 S FOUND=0
 ;Determine IF DESTIEN has cross refeences (B  cross-ref)
 ;;;;S ^XTMP("WELCH")=FOUND_"^"_DUZ_"^"
 S ^VA(200,DESTIEN,203,0)="^200.03P^^0"
 F  S OPTION=$O(^VA(200,TMGREC,203,"B",OPTION)) Q:OPTION=""  D
 .S COUNT1=COUNT1+1
 .S COUNT=0 S COUNT=$O(^VA(200,TMGREC,203,"B",OPTION,COUNT))
 .S OPTIONE=$P($G(^DIC(19,OPTION,0)),"^",1)
 .S SYNONYM=$P($G(^VA(200,TMGREC,203,COUNT,0)),"^",2)
 .S TMGFILE=200.03
 .K FDA,OROUT,FDAIEN
 .K FDAIEN
 .S FDAIEN(1)=DESTIEN
 .;;;;S ^XTMP("WELCH",5,COUNT)=OPTIONE
 .S FDA(45+COUNT1,TMGFILE,"?+2,"_FDAIEN(1)_",",.01)=OPTIONE
 .S FDA(45+COUNT1,TMGFILE,"?+2,"_FDAIEN(1)_",",2)=SYNONYM
 .;D UPDATE^DIE("E","FDA(45+COUNT1)","FDAIEN","OROUT(45+COUNT1)")
 .;S ^XTMP("WELCH",4)=FDAIEN(1)
 .;S ^XTMP("WELCH",3)=FDAIEN(2)
 .;
 .;MANUALLY PUT IN GLOBAL AND B CROSS REF AND ZERO NODE
 .S ^VA(200,DESTIEN,203,COUNT,0)=OPTION_"^"_SYNONYM
 .S ^VA(200,DESTIEN,203,"B",OPTION,COUNT)=""
 .;S ^XTMP("WELCH",6,COUNT)=^VA(200,DESTIEN,203,COUNT,0)
 .;I $D(OROUT(45+COUNT1,"DIERR")) S ^XTMP("WELCH")=OROUT(45+COUNT1,"DIERR",1,"TEXT",1) Q  ;K FDA(45+COUNT1),OROUT(45+COUNT1),FDAIEN Q
 .;OROUT(45+COUNT1,"DIERR",1,"TEXT",1) K FDA(45+COUNT1),OROUT(45+COUNT1) Q ;W !,"DATE
 .;ERROR=",OPTION,"",OROUT(45+COUNT1,"DIERR",1,"TEXT",1) Q
 .K FDA(45+COUNT1),OROUT(45+COUNT1),FDAIEN
 I COUNT1'=0 S ^VA(200,DESTIEN,203,0)="^200.03IP^"_COUNT1_"^"_COUNT1
 ;;;;S ^XTMP("WELCH",7)=$G(^VA(200,DESTIEN,203,0))
 Q