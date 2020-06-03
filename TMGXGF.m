TMGXGF  ;SFISC/VYD - Graphics Functions ;11/06/2002  11:10, 2/2/14
        ;;1.0;TMG-LIB;**1**;03/28/11
        ;;8.0;KERNEL;**269**;5/5/07 by kt
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
PREP    ;prepair graphics environment
        D PREP^XGSETUP
        D CLRCLIP    ;"//kt 5/5/07 added
        Q
        ;
        ;
IOXY(R,C)       ;cursor positioning R:row, C:col
        D ADJRC
        ;"//kt 5/6/07 modification.
        ;"Although this XGF system allows for off-screen coordinates, the underlying
        ;"  M systems will not.  So trying to position cursor to (-4,-5) MUST result
        ;"  in cursor being put at (0,0).  This may be worked around by not depending
        ;"  on the current $X,$Y for writing etc.  Instead, always specify coordinates.
        S:R<0 R=0  ;"//kt
        S:C<0 C=0  ;"//kt
        d CLIOXY^TMGXGS(R,C,"")
        S $Y=R,$X=C
        Q
        ;
        ;
SAY(R,C,S,A)    ;coordinate output instead of WRITE
        D ADJRC
        ;"//kt 5/6/07 mod.  Clipping to occur in CLIOXY^TMGXGS()
        ;"S:C+$L(S)>IOM S=$E(S,1,IOM-C) ;truncate IF longer than screen
        I $L($G(A)) S A=$$UP^XLFSTR(A) D SAY^TMGXGS(R,C,S,$S($$ATRSYNTX(A):A,1:"")) I 1
        E  D SAY^TMGXGS(R,C,S)
        Q
        ;
        ;
VSAY(R,C,S,A)    ;coordinate output instead of WRITE: Vertical WRITE ;"//kt added 5/10/07
        D ADJRC
        I $L($G(A)) S A=$$UP^XLFSTR(A) D VSAY^TMGXGS(R,C,S,$S($$ATRSYNTX(A):A,1:"")) I 1
        E  D VSAY^TMGXGS(R,C,S)
        Q
        ;
        ;
SAYU(R,C,S,A)   ;coordinate output w/ underline instead of WRITE
        D ADJRC
        I $L($G(A)) S A=$$UP^XLFSTR(A) D SAYU^TMGXGS(R,C,S,$S($$ATRSYNTX(A):A,1:"")) I 1
        E  D SAYU^TMGXGS(R,C,S)
        Q
        ;
        ;
ADJRC   ;adjust row and column R and C are assumed to exist
        S R=$S($G(R)="":$Y,1:R),C=$S($G(C)="":$X,1:C) ;use current coords IF none are passed
        ;"//kt 5/6/07 modified.  NOTE: it seems that code was written to allow coords
        ;"  to be specified as relative to $X,$Y.  E.g. SAY(+4,-2,'HELLO').
        ;"  I must remove this functionality so that I can allow specifying coordinates that
        ;"  are offscreen.  Thus IF the left-hand part of a window is a bit off the left
        ;"  side of the screen, then C will be -2 etc.
        ;"S:"+-"[$E(R) R=$Y+$S(R="+":1,R="-":-1,1:R) ;increment/decrement
        ;"S:"+-"[$E(C) C=$X+$S(C="+":1,C="-":-1,1:C)
        ;"S R=$S(R<0:0,1:R\1),C=$S(C<0:0,1:C\1) ;make sure only pos int
        ;"//kt modified line below
        S R=R\1,C=C\1 ;"make sure only integer values (clipping will occur in CLIOXY())
        Q
        ;
        ;
SETA(XGATR)     ;set screen attribute(s) regardless of previous state
        ;XGATR=1 char when converted to binary represents all NEW attr
        N XGOLDX,XGOLDY
        S XGOLDX=$X,XGOLDY=$Y ;save $X $Y
        W $$SET^XGSA(XGATR)
        S $X=XGOLDX,$Y=XGOLDY ;restore $X $Y
        Q
        ;
        ;
CHGA(XGATR)     ;change screen attribute(s) w/ respect to previous state
        ;XGNEWATR=string of attr to change eg. "B0U1" or "E1"
        N XGOLDX,XGOLDY,XGSYNTX,XGACODE,%
        S XGATR=$$UP^XLFSTR(XGATR) ;make sure all attr codes are in upper case
        D:$$ATRSYNTX(XGATR)
        . S XGOLDX=$X,XGOLDY=$Y ;save $X $Y
        . W $$CHG^XGSA(XGATR)
        . S $X=XGOLDX,$Y=XGOLDY ;restore $X $Y
        Q
        ;
        ;
ATRSYNTX(XGATR) ;check attribute code syntax
        ;proper attr is 1 or more (char from {BIRGUE} concat w/ 1 or 0)
        N XGSYNTX,%
        S XGSYNTX=$S($L(XGATR)&($L(XGATR)#2=0):1,1:0) ;even # of chars
        F %=1:2:$L(XGATR) S:"B1B0I1I0R1R0G1G0U1U0E1"'[$E(XGATR,%,%+1) XGSYNTX=0
        Q XGSYNTX
        ;
        ;
RESTORE(S)      ;restore screen region TOP,LEFT,BOTTOM,RIGHT,SAVE ROOT
        D RESTORE^TMGXGSW(S) Q
        K @S
        ;
        ;
SAVE(T,L,B,R,S) ;save screen region TOP,LEFT,BOTTOM,RIGHT,SAVE ROOT
        D SAVE^TMGXGSW(T,L,B,R,S) Q
        ;
        ;
WIN(T,L,B,R,S)  ;put up a window TOP,LEFT,BOTTOM,RIGHT[,SAVE ROOT]
        ;window style is not yet implemented
        I $L($G(S)) D WIN^TMGXGSW(T,L,B,R,S) I 1
        E  D WIN^TMGXGSW(T,L,B,R)
        Q
        ;
        ;
FRAME(T,L,B,R)  ;put a frame without clearing the inside TOP,LEFT,BOTTOM,RIGHT
        D FRAME^TMGXSBOX(T,L,B,R) Q
        ;
        ;
CLEAR(T,L,B,R)  ;clear screen portion TOP,LEFT,BOTTOM,RIGHT
        D CLEAR^TMGXSBOX(T,L,B,R) Q
        ;
        ;
CLEAN   ;clean up and destroy graphics environment
        D CLEAN^XGSETUP Q
        ;
        ;
INITKB(XGTRM)   ;initialize keyboard
        ;turn escape processing on, turn on passed terminators (if any)
        D INIT^XGKB($G(XGTRM)) Q
        ;
        ;
READ(XGCHARS,XGTO)      ;read the keyboard
        ;XGCHARS:number of chars to read, XGTO:timeout
        ;"//kt 5/5/07 modified to allow putting characters back.
        NEW TMGRESLT SET TMGRESLT=""
        IF ($GET(TMGWCBUF)="")&($GET(TMGWXGRT)="") do
        . SET TMGRESLT=$$READ^XGKB($G(XGCHARS),$G(XGTO))
        ELSE  do
        . SET TMGRESLT=$GET(TMGWCBUF) SET TMGWCBUF=""
        . SET XGRT=$GET(TMGWXGRT) SET TMGWXGRT=""
        QUIT TMGRESLT
        ;
        ;
UNREAD(XGCHARS,XGRT)  ;"//kt 5/5/07 added.
        ;Purpose: to put characters back into read stream after a READ
        ;       Note: may only be called once before a subsequent READ, or will overwrite
        ;Input: XGCHARS -- the character(s) to put back into stream
        ;       XGRT -- the command characters to put back into stream (i.e. XGRT)
        SET TMGWCBUF=XGCHARS
        SET TMGWXGRT=XGRT
        QUIT
        ;
        ;
RESETKB ;reset keyboard(escape processing off, terminators off)
        D EXIT^XGKB Q
        ;
        ;
SETCLIP(T,L,B,R)  ;"//kt 5/5/07 added
        ;Pupose: define a clipping area.  XGF writes clipped to area
        ;Input: TOP,LEFT,BOTTOM,ROGHT
        SET TMGCLT=+$GET(T),TMGCLL=$GET(L)
        SET TMGCLB=+$GET(B),TMGCLR=$GET(R)
        QUIT
        ;
        ;
CLRCLIP    ;"//kt 5/5/07 added
        ;Pupose: clear clipping area.
        SET TMGCLT=0,TMGCLL=0
        SET TMGCLB=IOSL-1,TMGCLR=IOM-1
        QUIT