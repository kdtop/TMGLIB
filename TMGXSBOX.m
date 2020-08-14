TMGXGSBOX ;SFISC/VYD - screen rectengular region primitives ;10/31/94  15:38, 2/2/14
          ;;1.0;TMG-LIB;**1**;6/23/15
          ;;8.0;KERNEL;;5/5/2007 by //kt
 ;
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;"Copyright (c) 6/23/2015  Kevin S. Toppenberg MD
 ;"
 ;"This file is part of the TMG LIBRARY, and may only be used in accordence
 ;" to license terms outlined in separate file TMGLICNS.m, which should 
 ;" always be distributed with this file.
 ;"~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--~--
 ;
FRAME(T,L,B,R,A,C)      ;draw a border
        ;TOP,LEFT,BOTTOM,RIGHT,ATTRIBUTE,frame character
        N %,%L2,%R2,M,S,X,Y ;M=middle S=string
        N XGSAVATR
        I B'>T N IOBLC,IOBRC S (IOBLC,IOBRC)=IOHL ;to draw horizontal line
        I R'>L N IOTRC,IOBRC S (IOTRC,IOBRC)=IOVL ;to draw vertical line
        S M=R-L-1
        S %L2=L+1,%R2=R+1
        ;if frame character passed SET frame parts to it, disable graphics
        S:$L($G(C)) (IOBLC,IOBRC,IOHL,IOTLC,IOTRC,IOVL)=C
        S XGSAVATR=XGCURATR                     ;save current screen attributes
        W $$CHG^XGSA($G(A)_$S($L($G(C)):"",1:"G1")) ;turn on gr attr & leave on
        S S=IOTLC_$TR($J("",M)," ",IOHL)_IOTRC
        S $E(XGSCRN(T,0),%L2,%R2)=S
        S $E(XGSCRN(T,1),%L2,%R2)=$TR($J("",(R-L+1))," ",XGCURATR)
        ;W $$IOXY^TMGXGS(T,L)_S ;top line with corners ;"//kt
        DO CLIOXY^TMGXGS(T,L,S) ;top line with corners ;"//kt
        F Y=T+1:1:B-1 D
        . F X=%L2,%R2 S $E(XGSCRN(Y,0),X)=IOVL,$E(XGSCRN(Y,1),X)=XGCURATR
        . ;W $$IOXY^TMGXGS(Y,L)_IOVL_$$IOXY^TMGXGS(Y,R)_IOVL  ;"//kt
        . DO CLIOXY^TMGXGS(Y,L,IOVL) DO CLIOXY^TMGXGS(Y,R,IOVL) ;"//kt
        S S=IOBLC_$TR($J("",M)," ",IOHL)_IOBRC
        S $E(XGSCRN(B,0),%L2,%R2)=S
        S $E(XGSCRN(B,1),%L2,%R2)=$TR($J("",(R-L+1))," ",XGCURATR)
        ;W $$IOXY^TMGXGS(B,L)_S ;bottom line with corners  ;"//kt
        DO CLIOXY^TMGXGS(B,L,S) ;bottom line with corners  ;"//kt
        W $$SET^XGSA(XGSAVATR)      ;restore previous attributes
        D:$L($G(C)) GSET^%ZISS      ;restore line drawing characters
        S $Y=B,$X=R
        Q
        ;
CLEAR(T,L,B,R)  ;clear a portion of the screen
        N %L2,%R2,I,M ;M=length of middle
        S %L2=L+1,%R2=R+1,M=R-L+1
        F I=T:1:B D
        . S $E(XGSCRN(I,0),%L2,%R2)=$J("",M)
        . S $E(XGSCRN(I,1),%L2,%R2)=$TR($J("",M)," ",XGCURATR)
        . ;W $$IOXY^TMGXGS(I,L)_$J("",M)  ;"//kt
        . DO CLIOXY^TMGXGS(I,L,$J("",M))  ;"//kt
        S $Y=B,$X=R
        Q
