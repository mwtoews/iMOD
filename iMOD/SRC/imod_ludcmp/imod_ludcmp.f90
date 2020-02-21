!!  Copyright (C) Stichting Deltares, 2005-2019.
!!
!!  This file is part of iMOD.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License as published by
!!  the Free Software Foundation, either version 3 of the License, or
!!  (at your option) any later version.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
!!
!!  Contact: imod.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands.
!!
MODULE MOD_LUDCMP

USE IMODVAR, ONLY : DP_KIND,SP_KIND
CONTAINS

 !###====================================================================
 SUBROUTINE LUDCMP_CALC_SQRTROOTINVERSE(N,COV,ISQRTCOV,SQRTCOV)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 REAL(KIND=DP_KIND),DIMENSION(N,N),INTENT(IN) :: COV
 REAL(KIND=DP_KIND),DIMENSION(N,N),INTENT(OUT) :: ISQRTCOV,SQRTCOV
 REAL(KIND=DP_KIND),DIMENSION(:,:),ALLOCATABLE :: A,IE
 REAL(KIND=DP_KIND),DIMENSION(:),ALLOCATABLE :: V,E
 INTEGER :: I,J,II
 REAL(KIND=DP_KIND) :: ET
 
 ALLOCATE(A(N,N),IE(N,N),E(N),V(N))
 !## copy covariance matrix
 A=COV
 
 !## perform eigenvalue decomposition
 CALL LUDCMP_TRED2(A,N,N,V,E)
 !## a are the eigenvectors
 CALL LUDCMP_TQLI(V,E,N,N,A)

 DO I=1,N
  V(I)=(V(I)+ABS(V(I)))/2.0D0
 ENDDO

 DO II=1,2

  DO I=1,N
   IF(V(I).GT.0.0D0)THEN
    IF(II.EQ.1)V(I)=SQRT(V(I))
    IF(II.EQ.2)V(I)=1.0D0/V(I)
   ELSE
    V(I)=0.0D0
   ENDIF
  ENDDO
  DO I=1,N
   DO J=1,N
    IE(I,J)=A(J,I)*V(I)
   ENDDO
  ENDDO
  !## transpose E
  DO I=1,N; DO J=1,N; ET=A(I,J); A(I,J)=A(J,I); A(J,I)=ET; ENDDO; ENDDO
  IF(II.EQ.1)SQRTCOV =MATMUL(A,IE)
  IF(II.EQ.2)ISQRTCOV=MATMUL(A,IE)
 ENDDO 
 
 DEALLOCATE(A,E,IE,V)
 
 END SUBROUTINE LUDCMP_CALC_SQRTROOTINVERSE
 
 !###========================================================================
 SUBROUTINE LUDCMP_EIGSRT(D,A,N,NP)
 !###========================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N,NP
 DOUBLE PRECISION,DIMENSION(NP),INTENT(INOUT) :: D
 DOUBLE PRECISION,DIMENSION(NP,NP),INTENT(INOUT) :: A
 INTEGER :: I,J,K
 DOUBLE PRECISION :: P

 DO I=1,N-1
  K=I
  P=D(I)
  DO J=I+1,N
   IF(D(J).GE.P)THEN
    K=J
    P=D(J)
   ENDIF
  END DO
  IF(K.NE.I)THEN
   D(K)=D(I)
   D(I)=P
   DO J=1,N
    P=A(J,I)
    A(J,I)=A(J,K)
    A(J,K)=P
   END DO
  ENDIF
 END DO

 END SUBROUTINE LUDCMP_EIGSRT
 
 !###========================================================================
 SUBROUTINE LUDCMP_TRED2(A,N,NP,D,E)
 !###========================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N,NP
 DOUBLE PRECISION,DIMENSION(NP,NP),INTENT(INOUT) :: A
 DOUBLE PRECISION,DIMENSION(NP),INTENT(INOUT) :: D,E
 INTEGER :: I,J,K,L
 DOUBLE PRECISION :: F,G,H,HH,SCALE

 DO I=N,2,-1
  L=I-1
  H=0.
  SCALE=0.0D0
  IF(L.GT.1)THEN
   DO K=1,L
    SCALE=SCALE+ABS(A(I,K))
   ENDDO
   IF(SCALE.EQ.0.0D0)THEN
    E(I)=A(I,L)
   ELSE
    DO K=1,L
     A(I,K)=A(I,K)/SCALE
     H=H+A(I,K)**2.0D0
    ENDDO
    F=A(I,L)
    G=-SIGN(SQRT(H),F)
    E(I)=SCALE*G
    H=H-F*G
    A(I,L)=F-G
    F=0.
    DO J=1,L
     A(J,I)=A(I,J)/H
     G=0.
     DO K=1,J
 G=G+A(J,K)*A(I,K)
     ENDDO
     DO K=J+1,L
 G=G+A(K,J)*A(I,K)
     ENDDO
     E(J)=G/H
     F=F+E(J)*A(I,J)
    ENDDO
    HH=F/(H+H)
    DO J=1,L
     F=A(I,J)
     G=E(J)-HH*F
     E(J)=G
     DO K=1,J
 A(J,K)=A(J,K)-F*E(K)-G*A(I,K)
     ENDDO
    ENDDO
   ENDIF
  ELSE
   E(I)=A(I,L)
  ENDIF
  D(I)=H
 ENDDO

 D(1)=0.0D0
 E(1)=0.0D0
 DO I=1,N
  L=I-1
  IF(D(I).NE.0.0D0)THEN
   DO J=1,L
    G=0.0D0
    DO K=1,L
     G=G+A(I,K)*A(K,J)
    END DO
    DO K=1,L
     A(K,J)=A(K,J)-G*A(K,I)
    END DO
   END DO
  ENDIF
  D(I)=A(I,I)
  A(I,I)=1.0D0
  DO J=1,L
   A(I,J)=0.0D0
   A(J,I)=0.0D0
  END DO
 END DO

 END SUBROUTINE LUDCMP_TRED2

 !###========================================================================
 REAL FUNCTION LUDCMP_PYTHAG(A,B)
 !###========================================================================
 IMPLICIT NONE
 DOUBLE PRECISION,INTENT(IN) :: A,B
 DOUBLE PRECISION :: ABSA,ABSB

 ABSA=ABS(A)
 ABSB=ABS(B)
 IF(ABSA.GT.ABSB)THEN
  LUDCMP_PYTHAG=ABSA*SQRT(1.0D0+(ABSB/ABSA)**2.0D0)
 ELSE
  IF(ABSB.EQ.0.0D0)THEN
   LUDCMP_PYTHAG=0.0D0
  ELSE
   LUDCMP_PYTHAG=ABSB*SQRT(1.0D0+(ABSA/ABSB)**2.0D0)
  ENDIF
 ENDIF

 END FUNCTION LUDCMP_PYTHAG

 !###========================================================================
 SUBROUTINE LUDCMP_TQLI(D,E,N,NP,A)
 !###========================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N,NP
 DOUBLE PRECISION,DIMENSION(NP),INTENT(INOUT) :: D,E
 DOUBLE PRECISION,DIMENSION(NP,NP),INTENT(INOUT) :: A
 INTEGER :: I,ITER,K,L,M
 DOUBLE PRECISION :: B,C,DD,F,G,P,R,S

 DO I=2,N
  E(I-1)=E(I)
 ENDDO
 E(N)=0.
 DO L=1,N
  ITER=0
1  DO M=L,N-1
   DD=ABS(D(M))+ABS(D(M+1))
   IF(ABS(E(M))+DD.EQ.DD)GOTO 2
  END DO
  M=N
2  IF(M.NE.L)THEN
   IF(ITER.EQ.100)WRITE(*,'(A)') 'TOO MANY ITERATIONS IN TQLI - CONTINUING'
   ITER=ITER+1
   G=(D(L+1)-D(L))/(2.0D0*E(L))
   R=LUDCMP_PYTHAG(G,1.0D0)
   G=D(M)-D(L)+E(L)/(G+SIGN(R,G))
   S=1.0D0
   C=1.0D0
   P=0.0D0
   DO I=M-1,L,-1
    F=S*E(I)
    B=C*E(I)
    R=LUDCMP_PYTHAG(F,G)
    E(I+1)=R
    IF(R.EQ.0.0D0)THEN
     D(I+1)=D(I+1)-P
     E(M)=0.0D0
     GOTO 1
    ENDIF
    S=F/R
    C=G/R
    G=D(I+1)-P
    R=(D(I)-G)*S+2.0D0*C*B
    P=S*R
    D(I+1)=G+P
    G=C*R-B
    DO K=1,N
     F=A(K,I+1)
     A(K,I+1)=S*A(K,I)+C*F
     A(K,I)=C*A(K,I)-S*F
    END DO
   END DO
   D(L)=D(L)-P
   E(L)=G
   E(M)=0.0D0
   GOTO 1
  ENDIF
 END DO

 IF(ITER.GT.100)WRITE(*,'(/A/)') 'TQLI NEEDED ',ITER,'ITERATIONS'
 
 END SUBROUTINE LUDCMP_TQLI
 
 !###====================================================================
 SUBROUTINE CHOLESKYDECOMPOSITION(A,N) 
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 REAL(KIND=DP_KIND),DIMENSION(N,N),INTENT(INOUT)  :: A
 INTEGER :: I,J,K
 REAL(KIND=DP_KIND) :: S
 
 DO K=1,N
  !## Evaluate off-diagonal terms
  DO I=1,K-1
   S=0.0D0
   DO J=1,I-1
    S=S+A(I,J)*A(K,J)
   ENDDO
   IF(A(I,I).EQ.0.0D0)THEN
    WRITE(*,*) A(I,I),I
   ENDIF
   A(K,I)=(A(K,I)-S)/A(I,I)
  ENDDO
! for k = 1 : n
!% evaluate off-diagonal terms
!for i = 1 : k-1
!s=0
! for j = 1 : i -1
!s = s + aij * akj
!end
!aki = (aki - s) / aii
!end
!## Evaluate diagonal term
  S=0.0D0
  DO J=1,K-1
   S=S+(A(K,J)**2.0D0)
  ENDDO
  IF(A(K,K)-S.LT.0.0D0)THEN
   WRITE(*,*) K,A(K,K)-S,A(K,K),S
   A(K,K)=0.0D0
  ELSE
   A(K,K)=SQRT(A(K,K)-S)
  ENDIF
 ENDDO
!s=0
!for j = 1 : k-1
!s = s + (akj)^2
!end
!akk = (akk - s)^(0.5)
!end]
 !# clean matrix
 DO I=1,N
  DO J=1,I-1
   A(J,I)=0.0D0
  ENDDO
 ENDDO
 
 END SUBROUTINE CHOLESKYDECOMPOSITION

 !###====================================================================
 SUBROUTINE LUDCMP_CALC(N,M,A,AI,B) !,IINV)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N,M
 REAL(KIND=DP_KIND),DIMENSION(M,M),INTENT(IN) :: A
 REAL(KIND=DP_KIND),DIMENSION(M,M),INTENT(OUT),OPTIONAL :: AI
 REAL(KIND=DP_KIND),DIMENSION(M),INTENT(INOUT),OPTIONAL :: B
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: L,U
 INTEGER :: I,II,J,K,IINV
 REAL(KIND=DP_KIND) :: X

 IINV=0; IF(PRESENT(AI))IINV=1

 ALLOCATE(L(N,N),U(N,N)); L=0.0D0; U=0.0D0
 
 !## transform first column
 DO I=1,N; L(I,1)=A(I,1); ENDDO
 !## transform first row
 DO I=1,N; U(1,I)=A(1,I)/A(1,1); ENDDO
 DO I=1,N; U(I,I)=1.0D0; ENDDO
 
 DO J=2,N-1
  DO I=J,N
   X=0.0D0
   DO K=1,J-1
    X=X+L(I,K)*U(K,J)
   ENDDO
   L(I,J)=A(I,J)-X
  ENDDO
  DO K=J+1,N
   X=0.0D0
   DO I=1,J-1
    X=X+L(J,I)*U(I,K)
   ENDDO
   IF(L(J,J).EQ.0.0D0)THEN
    WRITE(*,*) 'STOP CANNOT HAPPEN MATRIX SINGULIER'
   ENDIF
   U(J,K)=(A(J,K)-X)/L(J,J)
  ENDDO
 
 ENDDO
 
 X=0.0D0
 DO K=1,N-1
  X=X+L(N,K)*U(K,N)
 ENDDO
 L(N,N)=A(N,N)-X
 
 !## compute inverse matrix of A called AI
 IF(IINV.EQ.1)THEN

  AI=0.0D0; DO II=1,N; AI(II,II)=1.0D0; ENDDO
  DO II=1,N

   !## forward substitution
   AI(II,1)=AI(II,1)/L(1,1)
   DO I=2,N
    X=0.0D0
    DO J=1,I-1
     X=X+L(I,J)*AI(II,J)
    ENDDO
    AI(II,I)=(AI(II,I)-X)/L(I,I)
   ENDDO

   !## backward substitution
   DO I=N-1,1,-1
    X=0.0D0
    DO J=I+1,N
     X=X+U(I,J)*AI(II,J)
    ENDDO
    AI(II,I)=AI(II,I)-X
   ENDDO
  
  ENDDO
  
 ELSE

  !## forward substitution
  B(1)=B(1)/L(1,1)
  DO I=2,N
   X=0.0D0
   DO J=1,I-1
    X=X+L(I,J)*B(J)
   ENDDO
   B(I)=(B(I)-X)/L(I,I)
  ENDDO

  !## backward substitution
  DO I=N-1,1,-1
   X=0.0D0
   DO J=I+1,N
    X=X+U(I,J)*B(J)
   ENDDO
   B(I)=B(I)-X
  ENDDO

 ENDIF
 
 DEALLOCATE(L,U)
 
 END SUBROUTINE LUDCMP_CALC
 
 !###====================================================================
 SUBROUTINE LUDCMP(A,B,N) 
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN)   :: N
 REAL(KIND=DP_KIND),DIMENSION(N,N),INTENT(INOUT)  :: A
 REAL(KIND=DP_KIND),DIMENSION(N),INTENT(INOUT)  :: B
 REAL(KIND=DP_KIND),DIMENSION(:,:),ALLOCATABLE :: L,U
 INTEGER :: I,J
 REAL(KIND=DP_KIND) :: X

 ALLOCATE(L(N,N),U(N,N))

 !## perform lu-decomposition
 CALL LUDCMP_GETLU(A,L,U,N)
 
 !## forward substitution
 B(1)=B(1)/L(1,1)
 DO I=2,N
  X=0.0D0
  DO J=1,I-1
   X=X+L(I,J)*B(J)
  ENDDO
  B(I)=(B(I)-X)/L(I,I)
 ENDDO

 !## backward substitution
 DO I=N-1,1,-1
  X=0.0D0
  DO J=I+1,N
   X=X+U(I,J)*B(J)
  ENDDO
  B(I)=B(I)-X
 ENDDO

 DEALLOCATE(L,U)
 
 END SUBROUTINE LUDCMP

 !###====================================================================
 SUBROUTINE LUDCMP_GETLU(A,L,U,N) 
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 REAL(KIND=DP_KIND),DIMENSION(N,N),INTENT(IN)  :: A
 REAL(KIND=DP_KIND),DIMENSION(N,N),INTENT(OUT) :: L,U
 INTEGER :: I,J,K
 REAL(KIND=DP_KIND) :: X
 
 L=0.0D0; U=0.0D0
 
 !## transform first column
 DO I=1,N; L(I,1)=A(I,1); ENDDO
 !## transform first row
 DO I=1,N; U(1,I)=A(1,I)/A(1,1); ENDDO
 DO I=1,N; U(I,I)=1.0D0; ENDDO
 
 DO J=2,N-1

  DO I=J,N
   X=0.0D0
   DO K=1,J-1
    X=X+L(I,K)*U(K,J)
   ENDDO
   L(I,J)=A(I,J)-X
  ENDDO
  DO K=J+1,N
   X=0.0D0
   DO I=1,J-1
    X=X+L(J,I)*U(I,K)
   ENDDO
   IF(L(J,J).EQ.0.0D0)THEN
    L(J,J)=1.0D-10 !WRITE(*,*) 'DSDS'
   ENDIF
   U(J,K)=(A(J,K)-X)/L(J,J)
  ENDDO

 ENDDO
 
 X=0.0D0
 DO K=1,N-1
  X=X+L(N,K)*U(K,N)
 ENDDO
 L(N,N)=A(N,N)-X
 
 END SUBROUTINE LUDCMP_GETLU
 
 !###====================================================================
 SUBROUTINE LUDCMP_SVD_MAIN(SPACEDIM,TIMEDIM,MAXDIM,A,U,W,V) 
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: SPACEDIM,TIMEDIM,MAXDIM
 REAL(KIND=DP_KIND),INTENT(IN),DIMENSION(TIMEDIM,SPACEDIM) :: A
 REAL(KIND=DP_KIND),INTENT(OUT),DIMENSION(TIMEDIM,SPACEDIM) :: V
 REAL(KIND=DP_KIND),INTENT(OUT),DIMENSION(MAXDIM) :: W
 REAL(KIND=DP_KIND),INTENT(OUT),DIMENSION(SPACEDIM,SPACEDIM) :: U
 INTEGER :: MINDIM,M,N,Q,I,J,PASS
 REAL(KIND=DP_KIND) :: TEMP1
 REAL(KIND=DP_KIND),DIMENSION(TIMEDIM) :: TEMP2 
 REAL(KIND=DP_KIND),DIMENSION(SPACEDIM) :: TEMP3

! real at(spacedim,timedim)
! real a(timedim,spacedim),v(spacedim,spacedim)
! real w(maxdim)
! c  m = timedim
!c  n = spacedim
!c  q = smaller of timedim or spacedim (mindim)
!c  i,j,pass = counters

 !c  Fortran program to perform singular value decomposition on 
!c  a space-time array
!c
!c  Input file is the space-time array in binary format (AT array)
!c
!c  Output files are:
!c     V array (spacedim x spacedim): Normalized vectors (var=1)
!c     U array (timedim x timedim): Normalized vectors (var=1)
!c     Singular values (sigma) (maxdim) 
!c
!c

! !###  maxdim is the larger of timedim or spacedim
! MAXDIM=MAX(TIMEDIM,SPACEDIM)

! ALLOCATE(W(MAXDIM))

!c  
!c    Arrays 
!c
!c  at = original data in format space-time
!d  a = original data in format time-space
!c  v = V array (**Note it's not VT) 
!c  w = Sigma array: Singular values
!c 
!c  temp arrays used for data sorting

 !## call svdcmp subroutine
 V=A
 CALL LUDCMP_SVDCMP(V,TIMEDIM,SPACEDIM,MAXDIM,W,U)

!###  Sort the data from highest to lowest eigenvalue
!###  Null space excluded

 !###  mindim is the smaller of timedim or spacedim
 MINDIM=MIN(TIMEDIM,SPACEDIM)

 DO PASS=1,MINDIM
  DO J=1,(MINDIM-PASS)
   IF(W(J).LE.W(J+1))THEN
    TEMP1=W(J)
    DO I=1,TIMEDIM
     TEMP2(I)=V(I,J)
    ENDDO
    DO I=1,SPACEDIM
     TEMP3(I)=U(I,J)
    ENDDO
    W(J)=W(J+1)
    DO I=1,TIMEDIM
     V(I,J)=V(I,J+1)
    ENDDO
    DO I=1,SPACEDIM
     U(I,J)=U(I,J+1)
    ENDDO
    W(J+1)=TEMP1
    DO I=1,TIMEDIM
     V(I,J+1)=TEMP2(I)
    ENDDO
    DO I=1,SPACEDIM
     U(I,J+1)=TEMP3(I)
    ENDDO
   ENDIF
  ENDDO
 ENDDO

 END SUBROUTINE LUDCMP_SVD_MAIN

 !###====================================================================
 !  SVDcmp subroutine
 !    Given a matrix (1:m,1:n) with physical dimensions mp by np,
 !    this routine computes its singular value decomposition,
 !    A = U W VT.  The matrix U replaces a on output.  The diagonal
 !    matrix of singular values W is output as a vector w(1:n)
 !    The matrix V (not the transpose VT) is the output as v(1:n,1:n) 
 SUBROUTINE LUDCMP_svdcmp(a,mp,np,maxdim,w,v)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,PARAMETER :: NMAX=10000
 INTEGER,INTENT(IN) :: MP,NP,MAXDIM
 REAL(KIND=DP_KIND),DIMENSION(MP,NP),INTENT(INOUT) :: A
 REAL(KIND=DP_KIND),DIMENSION(NP,NP),INTENT(OUT) :: V
 REAL(KIND=DP_KIND),DIMENSION(MAXDIM),INTENT(OUT) :: W
 INTEGER :: I,ITS,J,JJ,K,L,NM,N,M
 REAL(KIND=DP_KIND) :: ANORM,C,F,G,H,S,SCALE,X,Y,Z !,RV1(NMAX)
 REAL(KIND=DP_KIND),DIMENSION(NMAX) :: RV1
 
 N=NP
 M=MP
 
 G=0.0D0
 SCALE=0.0D0
 ANORM=0.0D0
 DO 25 I=1,N
   L=I+1
   RV1(I)=SCALE*G
   G=0.0D0
   S=0.0D0
   SCALE=0.0D0
   IF(I.LE.M)THEN
     DO 11 K=I,M
  SCALE=SCALE+ABS(A(K,I))
11   CONTINUE
     IF(SCALE.NE.0.0D0)THEN
  DO 12 K=I,M
    A(K,I)=A(K,I)/SCALE
    S=S+A(K,I)*A(K,I)
12     CONTINUE
  F=A(I,I)
  G=-SIGN(SQRT(S),F)
  H=F*G-S
  A(I,I)=F-G
  DO 15 J=L,N
    S=0.0D0
    DO 13 K=I,M
      S=S+A(K,I)*A(K,J)
13  CONTINUE
    F=S/H
    DO 14 K=I,M
      A(K,J)=A(K,J)+F*A(K,I)
14  CONTINUE
15     CONTINUE
  DO 16 K=I,M
    A(K,I)=SCALE*A(K,I)
16     CONTINUE
     ENDIF
   ENDIF
   W(I)=SCALE *G
   G=0.0D0
   S=0.0D0
   SCALE=0.0D0
   IF((I.LE.M).AND.(I.NE.N))THEN
     DO 17 K=L,N
  SCALE=SCALE+ABS(A(I,K))
17   CONTINUE
     IF(SCALE.NE.0.0D0)THEN
  DO 18 K=L,N
    A(I,K)=A(I,K)/SCALE
    S=S+A(I,K)*A(I,K)
18     CONTINUE
  F=A(I,L)
  G=-SIGN(SQRT(S),F)
  H=F*G-S
  A(I,L)=F-G
  DO 19 K=L,N
    RV1(K)=A(I,K)/H
19     CONTINUE
  DO 23 J=L,M
    S=0.0
    DO 21 K=L,N
      S=S+A(J,K)*A(I,K)
21  CONTINUE
    DO 22 K=L,N
      A(J,K)=A(J,K)+S*RV1(K)
22  CONTINUE
23     CONTINUE
  DO 24 K=L,N
    A(I,K)=SCALE*A(I,K)
24     CONTINUE
     ENDIF
   ENDIF
   ANORM=MAX(ANORM,(ABS(W(I))+ABS(RV1(I))))
25    CONTINUE
 DO 32 I=N,1,-1
   IF(I.LT.N)THEN
     IF(G.NE.0.0D0)THEN
  DO 26 J=L,N
    V(J,I)=(A(I,J)/A(I,L))/G
26     CONTINUE
  DO 29 J=L,N
    S=0.0D0
    DO 27 K=L,N
      S=S+A(I,K)*V(K,J)
27  CONTINUE
    DO 28 K=L,N
      V(K,J)=V(K,J)+S*V(K,I)
28  CONTINUE
29     CONTINUE
     ENDIF
     DO 31 J=L,N
  V(I,J)=0.0D0
  V(J,I)=0.0D0
31   CONTINUE
   ENDIF
   V(I,I)=1.0D0
   G=RV1(I)
   L=I
32    CONTINUE
 DO 39 I=MIN(M,N),1,-1
   L=I+1
   G=W(I)
   DO 33 J=L,N
     A(I,J)=0.0D0
33 CONTINUE
   IF(G.NE.0.0D0)THEN
     G=1.0/G
     DO 36 J=L,N
  S=0.0
  DO 34 K=L,M
    S=S+A(K,I)*A(K,J)
34     CONTINUE
  F=(S/A(I,I))*G
  DO 35 K=I,M
    A(K,J)=A(K,J)+F*A(K,I)
35     CONTINUE
36   CONTINUE
     DO 37 J=I,M
  A(J,I)=A(J,I)*G
37   CONTINUE
   ELSE
     DO 38 J= I,M
  A(J,I)=0.0D0
38   CONTINUE
   ENDIF
   A(I,I)=A(I,I)+1.0D0
39    CONTINUE
 DO 49 K=N,1,-1
   DO 48 ITS=1,30
     DO 41 L=K,1,-1
  NM=L-1
  IF((ABS(RV1(L))+ANORM).EQ.ANORM)  GOTO 2
  IF((ABS(W(NM))+ANORM).EQ.ANORM)  GOTO 1
41   CONTINUE
1    C=0.0D0
     S=1.0D0
     DO 43 I=L,K
  F=S*RV1(I)
  RV1(I)=C*RV1(I)
  IF((ABS(F)+ANORM).EQ.ANORM) GOTO 2
  G=W(I)
  H=LUDCMP_PYTHAG(F,G)
  W(I)=H
  H=1.0/H
  C= (G*H)
  S=-(F*H)
  DO 42 J=1,M
    Y=A(J,NM)
    Z=A(J,I)
    A(J,NM)=(Y*C)+(Z*S)
    A(J,I)=-(Y*S)+(Z*C)
42     CONTINUE
43   CONTINUE
2    Z=W(K)
     IF(L.EQ.K)THEN
  IF(Z.LT.0.0D0)THEN
    W(K)=-Z
    DO 44 J=1,N
      V(J,K)=-V(J,K)
44  CONTINUE
  ENDIF
  GOTO 3
     ENDIF
     IF(ITS.EQ.30) PAUSE 'NO CONVERGENCE IN SVDCMP'
     X=W(L)
     NM=K-1
     Y=W(NM)
     G=RV1(NM)
     H=RV1(K)
     F=((Y-Z)*(Y+Z)+(G-H)*(G+H))/(2.0D0*H*Y)
     G=LUDCMP_PYTHAG(F,1.0D0)
     F=((X-Z)*(X+Z)+H*((Y/(F+SIGN(G,F)))-H))/X
     C=1.0
     S=1.0
     DO 47 J=L,NM
  I=J+1
  G=RV1(I)
  Y=W(I)
  H=S*G
  G=C*G
  Z=LUDCMP_PYTHAG(F,H)
  RV1(J)=Z
  C=F/Z
  S=H/Z
  F= (X*C)+(G*S)
  G=-(X*S)+(G*C)
  H=Y*S
  Y=Y*C
  DO 45 JJ=1,N
    X=V(JJ,J)
    Z=V(JJ,I)
    V(JJ,J)= (X*C)+(Z*S)
    V(JJ,I)=-(X*S)+(Z*C)
45     CONTINUE
  Z=LUDCMP_PYTHAG(F,H)
  W(J)=Z
  IF(Z.NE.0.0D0)THEN
    Z=1.0D0/Z
    C=F*Z
    S=H*Z
  ENDIF
  F= (C*G)+(S*Y)
  X=-(S*G)+(C*Y)
  DO 46 JJ=1,M
    Y=A(JJ,J)
    Z=A(JJ,I)
    A(JJ,J)= (Y*C)+(Z*S)
    A(JJ,I)=-(Y*S)+(Z*C)
46     CONTINUE
47   CONTINUE
     RV1(L)=0.0D0
     RV1(K)=F
     W(K)=X
48 CONTINUE
3  CONTINUE
49    CONTINUE

END SUBROUTINE LUDCMP_SVDCMP
 
 !
 !FUNCTION pythag(a,b)
 !REAL a,b,pythag
 !REAL absa,absb
 !absa=abs(a)
 !absb=abs(b)
 !if(absa.gt.absb)then
 !  pythag=absa*sqrt(1.+(absb/absa)**2)
 !else
 !  if(absb.eq.0.)then
 !    pythag=0.
 !  else
 !    pythag=absb*sqrt(1.+(absa/absb)**2)
 !  endif
 !endif
 !return
 !END
 
! !###====================================================================
! SUBROUTINE LUDCMP_INDX(AA,INDX,N)
! !###====================================================================
! IMPLICIT NONE
! INTEGER,PARAMETER :: NMAX=2000
! REAL(KIND=DP_KIND),PARAMETER :: TINY=1.0E-20
! INTEGER,INTENT(IN) :: N
! INTEGER,DIMENSION(N),INTENT(OUT) :: INDX
! REAL(KIND=DP_KIND),DIMENSION(N,N),INTENT(INOUT)  :: AA
! REAL(KIND=DP_KIND),DIMENSION(NMAX) :: VV
! INTEGER :: I,IMAX,J,K
! REAL(KIND=DP_KIND) :: AAMAX,DUM,SUM
!
! INDX=0
!
! DO I=1,N
!  AAMAX=0.
!  DO J=1,N
!   IF(ABS(AA(I,J)).GT.AAMAX)AAMAX=ABS(AA(I,J))
!  ENDDO
!  IF(AAMAX.EQ.0.)THEN
!   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'TENSOR: MATRIX=SINGULAR','Error'
!   RETURN
!  ENDIF
!  VV(I)=1./AAMAX
! ENDDO
! DO J=1,N
!  DO I=1,J-1 
!   SUM=AA(I,J)
!   DO K=1,I-1
!    SUM=SUM-AA(I,K)*AA(K,J)
!   ENDDO
!   AA(I,J)=SUM
!  ENDDO
!  AAMAX=0.
!  DO I=J,N
!   SUM=AA(I,J)
!   DO K=1,J-1
!    SUM=SUM-AA(I,K)*AA(K,J)
!   ENDDO
!   AA(I,J)=SUM
!   DUM=VV(I)*ABS(SUM)
!   IF(DUM.GE.AAMAX)THEN
!    IMAX=I
!    AAMAX=DUM
!   ENDIF
!  ENDDO
!  IF(J.NE.IMAX)THEN
!   DO K=1,N
!    DUM=AA(IMAX,K)
!    AA(IMAX,K)=AA(J,K)
!    AA(J,K)=DUM
!   ENDDO
!   VV(IMAX)=VV(J)
!  ENDIF
!  INDX(J)=IMAX
!  IF(AA(J,J).EQ.0.)AA(J,J)=TINY
!  IF(J.NE.N)THEN
!   DUM=1./AA(J,J)
!   DO I=J+1,N
!    AA(I,J)=AA(I,J)*DUM
!   ENDDO
!  ENDIF
! ENDDO
!
! END SUBROUTINE LUDCMP_INDX
!
! !###====================================================================
! SUBROUTINE LUBKSB_INDX(AA,INDX,BB,N)
! !###====================================================================
! USE MODFLOW
! IMPLICIT NONE
! INTEGER,INTENT(IN) :: N
! REAL(KIND=DP_KIND),DIMENSION(N,N),INTENT(INOUT) :: AA
! REAL(KIND=DP_KIND),DIMENSION(N),INTENT(INOUT) :: BB
! INTEGER,DIMENSION(N),INTENT(IN) :: INDX
! INTEGER :: I,II,J,LL
! REAL(KIND=DP_KIND) :: SUM
!
! II=0
! DO I=1,N
!  LL=INDX(I)
!  SUM=BB(LL)
!  BB(LL)=BB(I)
!  IF(II.NE.0)THEN
!   DO J=II,I-1
!    SUM=SUM-AA(I,J)*BB(J)
!   ENDDO
!  ELSE IF(SUM.NE.0.)THEN
!   II=I
!  ENDIF
!  BB(I)=SUM
! ENDDO
! DO I=N,1,-1
!  SUM=BB(I)
!  DO J=I+1,N
!   SUM=SUM-AA(I,J)*BB(J)
!  ENDDO
!  BB(I)=SUM/AA(I,I)
! ENDDO
!
!END SUBROUTINE LUBKSB_INDX

END MODULE MOD_LUDCMP

