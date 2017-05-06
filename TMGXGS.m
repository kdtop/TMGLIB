TMGXGS  ;SFISC/VYD - SCREEN PRIMITIVES ;03/16/95  11:00, 2/2/14
        ;;1.0;TMG-LIB;**1**;6/24/15
        ;;8.0;KERNEL;;5/7/07 by //kt
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
SAY(R,C,S,A)    ;use this for coordinate output instead of WRITE
        ;output to screen and update virtual screen (XGSCRN)
        ;params: Row (0-IOSL),Col (0-IOM),string,
        ;scrn attrib ie. I1R0B1 (optional)
        N XGSAVATR,XGESC,XGOUTPUT ;save attribute,escape str,output stream
        N %
        ;set output stream to either XGSCRN (virtual screen) or some window
        S XGOUTPUT=$S($G(XGFLAG("PAINT"),21)=21:"XGSCRN",1:$NA(^TMP("XGS",$J,XGW1)))
        S XGSAVATR=XGCURATR     ;preserve current attribute to restore later
        S $X=C+$L(S)
        S XGESC=$S($L($G(A)):$$CHG^XGSA(A),1:"")
        S $E(@XGOUTPUT@(R,0),(C+1),$X)=S
        S $E(@XGOUTPUT@(R,1),(C+1),$X)=$TR($J("",$L(S))," ",XGCURATR)
        ;S $P(%,XGCURATR,$L(S)+1)="",$E(@XGOUTPUT@(R,1),(C+1),$X)=%
        I XGOUTPUT="XGSCRN" D  I 1 ;if screen painting is to occur
        . ;output string in a proper place in proper attribute and restore attr
        . ;;W $$IOXY(R,C)_XGESC_S_$S($L($G(A)):$$SET^XGSA(XGSAVATR),1:"")
        . ;W $$IOXY(R,C)_XGESC_S_$S(XGSAVATR'=XGCURATR:$$SET^XGSA(XGSAVATR),1:"")
        . DO CLIOXY(R,C,XGESC_S_$S(XGSAVATR'=XGCURATR:$$SET^XGSA(XGSAVATR),1:""))
        . S $Y=R,$X=C+$L(S)-1
        E  S XGCURATR=XGSAVATR
        Q
        ;
        ;
VSAY(R,C,S,A)  ;"//kt added 5/10/07
        ;use this for coordinate output instead of WRITE ("Vertical write")
        ;output to screen and update virtual screen (XGSCRN)
        ;params: Row (0-IOSL),Col (0-IOM),string,
        ;scrn attrib ie. I1R0B1 (optional)
        ;"Note: WRITE is from top to bottom
        N XGSAVATR,XGESC,XGOUTPUT ;save attribute,escape str,output stream
        N %
        ;set output stream to either XGSCRN (virtual screen) or some window
        S XGOUTPUT=$S($G(XGFLAG("PAINT"),21)=21:"XGSCRN",1:$NA(^TMP("XGS",$J,XGW1)))
        S XGSAVATR=XGCURATR     ;preserve current attribute to restore later
        NEW TMGi
        for TMGi=1:1:$L(S) DO  ;"WRITE each character sequentially
        . NEW SS SET SS=$E(S,TMGi)
        . S XGESC=$S($L($G(A)):$$CHG^XGSA(A),1:"")
        . S $X=C+1
        . S $E(@XGOUTPUT@(R,0),(C+1),$X)=SS
        . S $E(@XGOUTPUT@(R,1),(C+1),$X)=$TR(" "," ",XGCURATR)  ;"<-- '??'
        . I XGOUTPUT="XGSCRN" D  I 1 ;if screen painting is to occur
        . . ;output string in a proper place in proper attribute and restore attr
        . . DO CLIOXY(R,C,XGESC_SS_$S(XGSAVATR'=XGCURATR:$$SET^XGSA(XGSAVATR),1:""))
        . . IF TMGi'=$L(S) S R=R+1
        . . SET $X=C,$Y=R
        . E  S XGCURATR=XGSAVATR
        Q
        ;
        ;
SAYU(R,C,S,A)   ;use this for coordinate output instead of WRITE
        ;output to screen and update virtual screen (XGSCRN)
        ;params: Row (0-IOSL),Col (0-IOM),string,
        ;scrn attrib ie. I1R0B1 (optional)
        N XGSAVATR,XGESC,XGOUTPUT ;save attribute,escape str,output stream
        N %,%S,P,P1,P2,X ;P1:piece before &, P2:piece from & to the end
        N XGATR
        ;set output stream to either XGSCRN (virtual screen) or some window
        S XGOUTPUT=$S($G(XGFLAG("PAINT"),21)=21:"XGSCRN",1:$NA(^TMP("XGS",$J,XGW1)))
        S P=$L(S,"&&")
        F %=1:1:P S $P(X,$C(1),%)=$P(S,"&&",%) ;replace all && with $C(1)
        I X["&",$G(A)'["U1",'$$STAT^XGSA("U")!($G(A)["U0") D  I 1
        . S XGSAVATR=XGCURATR     ;preserve current attribute to restore later
        . S XGESC=$S($L($G(A)):$$CHG^XGSA(A),1:"")
        . S XGATR=XGCURATR        ;get pre-underline attributes
        . S $X=C+$L(X)-1 ;adjust for a single &, which is not printable
        . ;S $E(XGSCRN(R,0),(C+1),$X)=$TR($TR(X,"&",""),$C(1),"&")
        . S $E(@XGOUTPUT@(R,0),(C+1),$X)=$TR($P(X,"&")_$P(X,"&",2,999),$C(1),"&")
        . S $E(@XGOUTPUT@(R,1),(C+1),$X)=$TR($J("",$X-C)," ",XGCURATR)
        . S P1=$TR($P(X,"&"),$C(1),"&"),P2=$TR($P(X,"&",2,999),$C(1),"&")
        . S %S=P1_$$CHG^XGSA("U1")_$E(P2) ;preunderline_underlinechar
        . S $E(@XGOUTPUT@(R,1),(C+1+$L(P1)))=XGCURATR ;record underlinechar
        . ;S %S=%S_$$CHG^XGSA("U0")_$E(P2,2,999) ;%S_postunderline
        . S %S=%S_$$SET^XGSA(XGATR)_$E(P2,2,999) ;%S_postunderline
        . I XGOUTPUT="XGSCRN" D  I 1
        . . ;output string in a proper place in proper attribute and restore attr
        . . ;;W $$IOXY(R,C)_XGESC_%S_$S($L($G(A)):$$SET^XGSA(XGSAVATR),1:"")
        . . ;W $$IOXY(R,C)_XGESC_%S_$S(XGCURATR'=XGSAVATR:$$SET^XGSA(XGSAVATR),1:"")
        . . DO CLIOXY(R,C,XGESC_%S_$S(XGCURATR'=XGSAVATR:$$SET^XGSA(XGSAVATR),1:""))
        . . S $Y=R,$X=C+$L(X)-2
        . E  S XGCURATR=XGSAVATR
        E  D SAY(R,C,$TR(S,"&"),A):$D(A),SAY(R,C,$TR(S,"&")):'$D(A)
        Q
        ;
        ;
IOXY(R,C)       ;cursor positioning WRITE argument instead of execute
        ;Row,Col
        Q $C(27,91)_((R+1))_$C(59)_((C+1))_$C(72)
        ;
        ;
CLIOXY(R,C,S)  ;"5/5/07 //kt added
        ;Purpose: a unified function for writing to screen, that also handles clipping
        ;Input: R,C -- row and column
        ;       S -- TEXT to put to screen.
        I (R<TMGCLT)!(R>TMGCLB) GOTO CLDONE
        I (C>TMGCLR) GOTO CLDONE
        I (C<TMGCLL) DO  ;clip leftward
        . NEW ESC SET ESC=""
        . IF $EXTRACT(S,1)=$CHAR(27) do
CL1     . . DO CLIPESC(.S,.ESC)  ;"remove leading escape sequences prior to clipping.
        . NEW TMGCLIP SET TMGCLIP=TMGCLL-C
        . SET S=ESC_$EXTRACT(S,1+TMGCLIP,9999)
        . SET C=TMGCLL

        WRITE $$IOXY(R,C) ;position to R,C
        NEW TMGSPL S TMGSPL=TMGCLR-C+1 ;find space left to clipping margin
        WRITE $EXTRACT(S,1,TMGSPL)
CLDONE
        QUIT

CLIPESC(S,ESC)  ;"5/26/07 //kt added
        ;Purpose: to separate an escape sequence from the beginning of a string
        ;Input: S -- the string to work on
        ;       ESC -- PASS BY REFERENCE, an OUT PARAMETER
        ;          Note: prior entries in ESC will NOT be killed.  Results will be appended
        ;Output: IF S has one more leading escape sequences, these will be removed
        ;results: none
        ;Note: The rule that will be used to determine the end of the escape sequence
        ;     will be when an uppercase letter is encountered, or another ESC(#27) is found

        IF $EXTRACT(S,1)'=$CHAR(27) GOTO CEDone
        SET ESC=$GET(ESC)_$CHAR(27)
        NEW p SET p=2
        NEW done SET done=0
        FOR  DO  QUIT:(done=1)
        . NEW ch,chNum SET ch=$EXTRACT(S,p),chNum=$ASCII(ch)
        . IF chNum=27 SET done=1 QUIT
        . IF (chNum'<$ASCII("A"))&(chNum'>$ASCII("Z")) SET done=1 QUIT
        . SET ESC=ESC_ch
        . SET p=p+1
        SET S=$EXTRACT(S,p,9999)
        DO CLIPESC(.S,.ESC) ;"check for further escape sequences
CEDone
        QUIT
