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
 !# clean matrix - delivering upper-triangle
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
 INTEGER :: MINDIM,I,J,PASS
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


 !###====================================================================
 SUBROUTINE IPEST_NORMAL_MS_SAMPLE(MU,SIGMA,SEED,X)
 !###====================================================================
!NORMAL_MS_SAMPLE samples the Normal PDF.
! Discussion:
!   The Box-Muller method is used.
! Licensing:
!   This code is distributed under the GNU LGPL license.
! Author:
!   John Burkardt
! Parameters:
!   Input, real ( kind = 8 ) MU, SIGMA, the parameters.
!   0.0D+00 < SIGMA.
!   Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!   generator.
!   Output, real ( kind = 8 ) X, a sample of the PDF.
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: MU,SIGMA
 REAL(KIND=DP_KIND),INTENT(OUT) :: X
 REAL(KIND=DP_KIND) :: X01
 INTEGER,INTENT(INOUT) :: SEED

 CALL IPEST_NORMAL_01_SAMPLE(SEED,X01)

 X=MU+SIGMA*X01

 END SUBROUTINE IPEST_NORMAL_MS_SAMPLE

 !###====================================================================
 SUBROUTINE IPEST_NORMAL_01_SAMPLE(SEED,X)
 !###====================================================================
 IMPLICIT NONE
!*****************************************************************************80
!! NORMAL_01_SAMPLE samples the standard normal probability distribution.
!  Discussion:
!    The standard normal probability distribution function (PDF) has
!    mean 0 and standard deviation 1.
!    The Box-Muller method is used, which is efficient, but
!    generates two values at a time.
!  Licensing:
!    This code is distributed under the GNU LGPL license.
!  Author:
!    John Burkardt
!  Parameters:
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!    Output, real ( kind = 8 ) X, a sample of the standard normal PDF.
  REAL(KIND=DP_KIND) :: R1,R2 !,IPEST_R8_UNIFORM_01
  REAL(KIND=DP_KIND), PARAMETER :: R8_PI = 3.141592653589793D+00
  INTEGER,INTENT(INOUT) :: SEED
  INTEGER,SAVE :: USED = -1
  REAL(KIND=DP_KIND),INTENT(OUT) :: X
  REAL(KIND=DP_KIND), SAVE :: Y = 0.0D+00

  IF ( USED == -1 ) THEN
    USED = 0
  END IF
!
!  If we've used an even number of values so far, generate two more,
!  return one and save one.
!
  IF ( MOD ( USED, 2 ) == 0 ) THEN

    DO

      R1 = IPEST_R8_UNIFORM_01 ( SEED )

      IF ( R1 /= 0.0D+00 ) THEN
        EXIT
      END IF

    END DO

    R2 = IPEST_R8_UNIFORM_01 ( SEED )

    X = SQRT ( - 2.0D+00 * LOG ( R1 ) ) * COS ( 2.0D+00 * R8_PI * R2 )
    Y = SQRT ( - 2.0D+00 * LOG ( R1 ) ) * SIN ( 2.0D+00 * R8_PI * R2 )
!
!  Otherwise, return the second, saved, value.
!
  ELSE

    X = Y

  END IF

  USED = USED + 1

 end SUBROUTINE IPEST_NORMAL_01_SAMPLE
 
 !###====================================================================
 DOUBLE PRECISION FUNCTION IPEST_R8_UNIFORM_01 ( SEED )
 !###====================================================================

!*****************************************************************************80
!
!! IPEST_R8_UNIFORM_01 returns a unit pseudorandom R8.
!
!  Discussion:
!
!    An R8 is a real ( kind = 8 ) value.
!
!    For now, the input quantity SEED is an integer variable.
!
!    This routine implements the recursion
!
!      seed = 16807 * seed mod ( 2^31 - 1 )
!      IPEST_R8_UNIFORM_01 = seed / ( 2^31 - 1 )
!
!    The integer arithmetic never requires more than 32 bits,
!    including a sign bit.
!
!    If the initial seed is 12345, then the first three computations are
!
!      Input     Output      IPEST_R8_UNIFORM_01
!      SEED      SEED
!
!         12345   207482415  0.096616
!     207482415  1790989824  0.833995
!    1790989824  2035175616  0.947702
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 July 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Springer Verlag, pages 201-202, 1983.
!
!    Pierre L'Ecuyer,
!    Random Number Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley Interscience, page 95, 1998.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, pages 362-376, 1986.
!
!    Peter Lewis, Allen Goodman, James Miller
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, pages 136-143, 1969.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which should
!    NOT be 0. On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) IPEST_R8_UNIFORM_01, a new pseudorandom variate,
!    strictly between 0 and 1.
!
  implicit none

  integer, parameter :: i4_huge = 2147483647
  integer,intent(inout) :: seed 
  integer :: k
!  real ( kind = 8 ) IPEST_R8_UNIFORM_01
!  integer ( kind = 4 ) seed

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_UNIFORM_01 - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop 1
  end if

  k = seed / 127773

  seed = 16807 * ( seed - k * 127773 ) - k * 2836

  if ( seed < 0 ) then
    seed = seed + i4_huge
  end if

  IPEST_R8_UNIFORM_01 = real ( seed, kind = 8 ) * 4.656612875D-10

end function IPEST_R8_UNIFORM_01

subroutine log_normal_cdf ( x, mu, sigma, cdf )

!*****************************************************************************80
!
!! LOG_NORMAL_CDF evaluates the Log Normal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0.0 < X.
!
!    Input, real ( kind = 8 ) MU, SIGMA, the parameters of the PDF.
!    0.0 < SIGMA.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) cdf
  real ( kind = 8 ) logx
  real ( kind = 8 ) mu
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x

  if ( x <= 0.0D+00 ) then

    cdf = 0.0D+00

  else

    logx = log ( x )

    call normal_cdf ( logx, mu, sigma, cdf )

  end if

  return
end
subroutine log_normal_cdf_inv ( cdf, mu, sigma, x )

!*****************************************************************************80
!
!! LOG_NORMAL_CDF_INV inverts the Log Normal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) MU, SIGMA, the parameters of the PDF.
!    0.0 < SIGMA.
!
!    Input, real ( kind = 8 ) X, the corresponding argument.
!
  implicit none

  real ( kind = 8 ) cdf
  real ( kind = 8 ) logx
  real ( kind = 8 ) mu
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'LOG_NORMAL_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
    stop 1
  end if

  call normal_cdf_inv ( cdf, mu, sigma, logx )

  x = exp ( logx )

  return
end
subroutine log_normal_cdf_values ( n_data, mu, sigma, x, fx )

!*****************************************************************************80
!
!! LOG_NORMAL_CDF_VALUES returns some values of the Log Normal CDF.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      dist = LogNormalDistribution [ mu, sigma ]
!      CDF [ dist, x ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 August 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    LC: QA47.A34,
!    ISBN: 0-486-61272-4.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) MU, the mean of the distribution.
!
!    Output, real ( kind = 8 ) SIGMA, the shape parameter of the distribution.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 12

  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    0.2275013194817921D-01, &
    0.2697049307349095D+00, &
    0.5781741008028732D+00, &
    0.7801170895122241D+00, &
    0.4390310097476894D+00, &
    0.4592655190218048D+00, &
    0.4694258497695908D+00, &
    0.4755320473858733D+00, &
    0.3261051056816658D+00, &
    0.1708799040927608D+00, &
    0.7343256357952060D-01, &
    0.2554673736161761D-01 /)
  real ( kind = 8 ) mu
  real ( kind = 8 ), save, dimension ( n_max ) :: mu_vec = (/ &
    0.1000000000000000D+01, &
    0.1000000000000000D+01, &
    0.1000000000000000D+01, &
    0.1000000000000000D+01, &
    0.1000000000000000D+01, &
    0.1000000000000000D+01, &
    0.1000000000000000D+01, &
    0.1000000000000000D+01, &
    0.2000000000000000D+01, &
    0.3000000000000000D+01, &
    0.4000000000000000D+01, &
    0.5000000000000000D+01 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) sigma
  real ( kind = 8 ), save, dimension ( n_max ) :: sigma_vec = (/ &
    0.5000000000000000D+00, &
    0.5000000000000000D+00, &
    0.5000000000000000D+00, &
    0.5000000000000000D+00, &
    0.2000000000000000D+01, &
    0.3000000000000000D+01, &
    0.4000000000000000D+01, &
    0.5000000000000000D+01, &
    0.2000000000000000D+01, &
    0.2000000000000000D+01, &
    0.2000000000000000D+01, &
    0.2000000000000000D+01 /)
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
    0.1000000000000000D+01, &
    0.2000000000000000D+01, &
    0.3000000000000000D+01, &
    0.4000000000000000D+01, &
    0.2000000000000000D+01, &
    0.2000000000000000D+01, &
    0.2000000000000000D+01, &
    0.2000000000000000D+01, &
    0.3000000000000000D+01, &
    0.3000000000000000D+01, &
    0.3000000000000000D+01, &
    0.3000000000000000D+01 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    mu = 0.0D+00
    sigma = 0.0D+00
    x = 0.0D+00
    fx = 0.0D+00
  else
    mu = mu_vec(n_data)
    sigma = sigma_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
function log_normal_check ( mu, sigma )

!*****************************************************************************80
!
!! LOG_NORMAL_CHECK checks the parameters of the Log Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) MU, SIGMA, the parameters of the PDF.
!    0.0 < SIGMA.
!
!    Output, logical LOG_NORMAL_CHECK, is true if the parameters are legal.
!
  implicit none

  logical log_normal_check
  real ( kind = 8 ) mu
  real ( kind = 8 ) sigma

  log_normal_check = .true.

  if ( sigma <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'LOG_NORMAL_CHECK - Fatal error!'
    write ( *, '(a)' ) '  B <= 0.'
    log_normal_check = .false.
  end if

  return
end
subroutine log_normal_mean ( mu, sigma, mean )

!*****************************************************************************80
!
!! LOG_NORMAL_MEAN returns the mean of the Log Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) MU, SIGMA, the parameters of the PDF.
!    0.0 < SIGMA.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  real ( kind = 8 ) mean
  real ( kind = 8 ) mu
  real ( kind = 8 ) sigma

  mean = exp ( mu + 0.5D+00 * sigma * sigma )

  return
end
subroutine log_normal_pdf ( x, mu, sigma, pdf )

!*****************************************************************************80
!
!! LOG_NORMAL_PDF evaluates the Log Normal PDF.
!
!  Discussion:
!
!    PDF(A,B;X)
!      = exp ( - 0.5 * ( ( log ( X ) - MU ) / SIGMA )^2 )
!        / ( SIGMA * X * sqrt ( 2 * PI ) )
!
!    The Log Normal PDF is also known as the Cobb-Douglas PDF,
!    and as the Antilog_normal PDF.
!
!    The Log Normal PDF describes a variable X whose logarithm
!    is normally distributed.
!
!    The special case MU = 0, SIGMA = 1 is known as Gilbrat's PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0.0 < X
!
!    Input, real ( kind = 8 ) MU, SIGMA, the parameters of the PDF.
!    0.0 < SIGMA.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) mu
  real ( kind = 8 ) pdf
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x

  if ( x <= 0.0D+00 ) then
    pdf = 0.0D+00
  else
    pdf = exp ( - 0.5D+00 * ( ( log ( x ) - mu ) / sigma ) ** 2 ) &
      / ( sigma * x * sqrt ( 2.0D+00 * r8_pi ) )
  end if

  return
end
subroutine log_normal_sample ( mu, sigma, seed, x )

!*****************************************************************************80
!
!! LOG_NORMAL_SAMPLE samples the Log Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) MU, SIGMA, the parameters of the PDF.
!    0.0 < SIGMA.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) cdf
  real ( kind = 8 ) mu
!  real ( kind = 8 ) IPEST_R8_UNIFORM_01
  integer ( kind = 4 ) seed
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x

  cdf = IPEST_R8_UNIFORM_01 ( seed )

  call log_normal_cdf_inv ( cdf, mu, sigma, x )

  return
end
subroutine log_normal_variance ( mu, sigma, variance )

!*****************************************************************************80
!
!! LOG_NORMAL_VARIANCE returns the variance of the Log Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) MU, SIGMA, the parameters of the PDF.
!    0.0 < SIGMA.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  real ( kind = 8 ) mu
  real ( kind = 8 ) sigma
  real ( kind = 8 ) variance

  variance = exp ( 2.0D+00 * mu + sigma * sigma ) &
    * ( exp ( sigma * sigma ) - 1.0D+00 )

  return
end
subroutine normal_01_cdf ( x, cdf )

!*****************************************************************************80
!
!! NORMAL_01_CDF evaluates the Normal 01 CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    AG Adams,
!    Algorithm 39,
!    Areas Under the Normal Curve,
!    Computer Journal,
!    Volume 12, pages 197-198, 1969.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ), parameter :: a1 = 0.398942280444D+00
  real ( kind = 8 ), parameter :: a2 = 0.399903438504D+00
  real ( kind = 8 ), parameter :: a3 = 5.75885480458D+00
  real ( kind = 8 ), parameter :: a4 = 29.8213557808D+00
  real ( kind = 8 ), parameter :: a5 = 2.62433121679D+00
  real ( kind = 8 ), parameter :: a6 = 48.6959930692D+00
  real ( kind = 8 ), parameter :: a7 = 5.92885724438D+00
  real ( kind = 8 ), parameter :: b0 = 0.398942280385D+00
  real ( kind = 8 ), parameter :: b1 = 3.8052D-08
  real ( kind = 8 ), parameter :: b2 = 1.00000615302D+00
  real ( kind = 8 ), parameter :: b3 = 3.98064794D-04
  real ( kind = 8 ), parameter :: b4 = 1.98615381364D+00
  real ( kind = 8 ), parameter :: b5 = 0.151679116635D+00
  real ( kind = 8 ), parameter :: b6 = 5.29330324926D+00
  real ( kind = 8 ), parameter :: b7 = 4.8385912808D+00
  real ( kind = 8 ), parameter :: b8 = 15.1508972451D+00
  real ( kind = 8 ), parameter :: b9 = 0.742380924027D+00
  real ( kind = 8 ), parameter :: b10 = 30.789933034D+00
  real ( kind = 8 ), parameter :: b11 = 3.99019417011D+00
  real ( kind = 8 ) cdf
  real ( kind = 8 ) q
  real ( kind = 8 ) x
  real ( kind = 8 ) y
!
!  |X| <= 1.28.
!
  if ( abs ( x ) <= 1.28D+00 ) then

    y = 0.5D+00 * x * x

    q = 0.5D+00 - abs ( x ) * ( a1 - a2 * y / ( y + a3 - a4 / ( y + a5 &
      + a6 / ( y + a7 ) ) ) )
!
!  1.28 < |X| <= 12.7
!
  else if ( abs ( x ) <= 12.7D+00 ) then

    y = 0.5D+00 * x * x

    q = exp ( - y ) * b0 / ( abs ( x ) - b1 &
      + b2 / ( abs ( x ) + b3 &
      + b4 / ( abs ( x ) - b5 &
      + b6 / ( abs ( x ) + b7 &
      - b8 / ( abs ( x ) + b9 &
      + b10 / ( abs ( x ) + b11 ) ) ) ) ) )
!
!  12.7 < |X|
!
  else

    q = 0.0D+00

  end if
!
!  Take account of negative X.
!
  if ( x < 0.0D+00 ) then
    cdf = q
  else
    cdf = 1.0D+00 - q
  end if

  return
end
subroutine normal_01_cdf_inv ( p, x )

!*****************************************************************************80
!
!! NORMAL_01_CDF_INV inverts the standard normal CDF.
!
!  Discussion:
!
!    The result is accurate to about 1 part in 10^16.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 June 2007
!
!  Author:
!
!    Original FORTRAN77 version by Michael Wichura.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Michael Wichura,
!    Algorithm AS241:
!    The Percentage Points of the Normal Distribution,
!    Applied Statistics,
!    Volume 37, Number 3, pages 477-484, 1988.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P, the value of the cumulative probability
!    densitity function.  0 < P < 1.  If P is outside this range, an
!    "infinite" value will be returned.
!
!    Output, real ( kind = 8 ) X, the normal deviate value
!    with the property that the probability of a standard normal deviate being
!    less than or equal to the value is P.
!
  implicit none

  real ( kind = 8 ), parameter, dimension ( 8 ) :: a = (/ &
    3.3871328727963666080D+00, &
    1.3314166789178437745D+02, &
    1.9715909503065514427D+03, &
    1.3731693765509461125D+04, &
    4.5921953931549871457D+04, &
    6.7265770927008700853D+04, &
    3.3430575583588128105D+04, &
    2.5090809287301226727D+03 /)
  real ( kind = 8 ), parameter, dimension ( 8 ) :: b = (/ &
    1.0D+00, &
    4.2313330701600911252D+01, &
    6.8718700749205790830D+02, &
    5.3941960214247511077D+03, &
    2.1213794301586595867D+04, &
    3.9307895800092710610D+04, &
    2.8729085735721942674D+04, &
    5.2264952788528545610D+03 /)
  real ( kind = 8 ), parameter, dimension ( 8 ) :: c = (/ &
    1.42343711074968357734D+00, &
    4.63033784615654529590D+00, &
    5.76949722146069140550D+00, &
    3.64784832476320460504D+00, &
    1.27045825245236838258D+00, &
    2.41780725177450611770D-01, &
    2.27238449892691845833D-02, &
    7.74545014278341407640D-04 /)
  real ( kind = 8 ), parameter :: const1 = 0.180625D+00
  real ( kind = 8 ), parameter :: const2 = 1.6D+00
  real ( kind = 8 ), parameter, dimension ( 8 ) :: d = (/ &
    1.0D+00, &
    2.05319162663775882187D+00, &
    1.67638483018380384940D+00, &
    6.89767334985100004550D-01, &
    1.48103976427480074590D-01, &
    1.51986665636164571966D-02, &
    5.47593808499534494600D-04, &
    1.05075007164441684324D-09 /)
  real ( kind = 8 ), parameter, dimension ( 8 ) :: e = (/ &
    6.65790464350110377720D+00, &
    5.46378491116411436990D+00, &
    1.78482653991729133580D+00, &
    2.96560571828504891230D-01, &
    2.65321895265761230930D-02, &
    1.24266094738807843860D-03, &
    2.71155556874348757815D-05, &
    2.01033439929228813265D-07 /)
  real ( kind = 8 ), parameter, dimension ( 8 ) :: f = (/ &
    1.0D+00, &
    5.99832206555887937690D-01, &
    1.36929880922735805310D-01, &
    1.48753612908506148525D-02, &
    7.86869131145613259100D-04, &
    1.84631831751005468180D-05, &
    1.42151175831644588870D-07, &
    2.04426310338993978564D-15 /)
  real ( kind = 8 ) p
  real ( kind = 8 ) q
  real ( kind = 8 ) r
!  real ( kind = 8 ) r8poly_value
  real ( kind = 8 ), parameter :: split1 = 0.425D+00
  real ( kind = 8 ), parameter :: split2 = 5.0D+00
  real ( kind = 8 ) x

  if ( p <= 0.0D+00 ) then
    x = - huge ( x )
    return
  end if

  if ( 1.0D+00 <= p ) then
    x = huge ( x )
    return
  end if

  q = p - 0.5D+00

  if ( abs ( q ) <= split1 ) then

    r = const1 - q * q
    x = q * r8poly_value ( 7, a, r ) / r8poly_value ( 7, b, r )

  else

    if ( q < 0.0D+00 ) then
      r = p
    else
      r = 1.0D+00 - p
    end if

    if ( r <= 0.0D+00 ) then

      x = huge ( x )

    else

      r = sqrt ( - log ( r ) )

      if ( r <= split2 ) then

        r = r - const2
        x = r8poly_value ( 7, c, r ) / r8poly_value ( 7, d, r )

      else

        r = r - split2
        x = r8poly_value ( 7, e, r ) / r8poly_value ( 7, f, r )

      end if

    end if

    if ( q < 0.0D+00 ) then
      x = -x
    end if

  end if

  return
end
subroutine normal_cdf ( x, mu, sigma, cdf )

!*****************************************************************************80
!
!! NORMAL_CDF evaluates the Normal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) MU, SIGMA, the parameters of the PDF.
!    0.0 < SIGMA.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) cdf
  real ( kind = 8 ) mu
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  y = ( x - mu ) / sigma

  call normal_01_cdf ( y, cdf )

  return
end
subroutine normal_cdf_inv ( cdf, mu, sigma, x )

!*****************************************************************************80
!
!! NORMAL_CDF_INV inverts the Normal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) MU, SIGMA, the parameters of the PDF.
!    0.0 < SIGMA.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
  implicit none

  real ( kind = 8 ) cdf
  real ( kind = 8 ) mu
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'NORMAL_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
    stop 1
  end if

  call normal_01_cdf_inv ( cdf, x2 )

  x = mu + sigma * x2

  return
end
!!function IPEST_R8_UNIFORM_01 ( seed )
!
!!*****************************************************************************80
!!
!!! IPEST_R8_UNIFORM_01 returns a unit pseudorandom R8.
!!
!!  Discussion:
!!
!!    An R8 is a real ( kind = 8 ) value.
!!
!!    For now, the input quantity SEED is an integer variable.
!!
!!    This routine implements the recursion
!!
!!      seed = 16807 * seed mod ( 2^31 - 1 )
!!      IPEST_R8_UNIFORM_01 = seed / ( 2^31 - 1 )
!!
!!    The integer arithmetic never requires more than 32 bits,
!!    including a sign bit.
!!
!!    If the initial seed is 12345, then the first three computations are
!!
!!      Input     Output      IPEST_R8_UNIFORM_01
!!      SEED      SEED
!!
!!         12345   207482415  0.096616
!!     207482415  1790989824  0.833995
!!    1790989824  2035175616  0.947702
!!
!!  Licensing:
!!
!!    This code is distributed under the GNU LGPL license.
!!
!!  Modified:
!!
!!    05 July 2006
!!
!!  Author:
!!
!!    John Burkardt
!!
!!  Reference:
!!
!!    Paul Bratley, Bennett Fox, Linus Schrage,
!!    A Guide to Simulation,
!!    Springer Verlag, pages 201-202, 1983.
!!
!!    Pierre L'Ecuyer,
!!    Random Number Generation,
!!    in Handbook of Simulation,
!!    edited by Jerry Banks,
!!    Wiley Interscience, page 95, 1998.
!!
!!    Bennett Fox,
!!    Algorithm 647:
!!    Implementation and Relative Efficiency of Quasirandom
!!    Sequence Generators,
!!    ACM Transactions on Mathematical Software,
!!    Volume 12, Number 4, pages 362-376, 1986.
!!
!!    Peter Lewis, Allen Goodman, James Miller
!!    A Pseudo-Random Number Generator for the System/360,
!!    IBM Systems Journal,
!!    Volume 8, pages 136-143, 1969.
!!
!!  Parameters:
!!
!!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which should
!!    NOT be 0. On output, SEED has been updated.
!!
!!    Output, real ( kind = 8 ) IPEST_R8_UNIFORM_01, a new pseudorandom variate,
!!    strictly between 0 and 1.
!!
!  implicit none
!
!  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
!  integer ( kind = 4 ) k
!  real ( kind = 8 ) IPEST_R8_UNIFORM_01
!  integer ( kind = 4 ) seed
!
!  if ( seed == 0 ) then
!    write ( *, '(a)' ) ' '
!    write ( *, '(a)' ) 'R8_UNIFORM_01 - Fatal error!'
!    write ( *, '(a)' ) '  Input value of SEED = 0.'
!    stop 1
!  end if
!
!  k = seed / 127773
!
!  seed = 16807 * ( seed - k * 127773 ) - k * 2836
!
!  if ( seed < 0 ) then
!    seed = seed + i4_huge
!  end if
!
!  IPEST_R8_UNIFORM_01 = real ( seed, kind = 8 ) * 4.656612875D-10
!
!  return
!end
DOUBLE PRECISION function r8poly_value ( m, c, x )
!function r8poly_value_horner ( m, c, x )

!*****************************************************************************80
!
!! R8POLY_VALUE_HORNER evaluates a polynomial using Horner's method.
!
!  Discussion:
!
!    The polynomial 
!
!      p(x) = c0 + c1 * x + c2 * x^2 + ... + cm * x^m
!
!    is to be evaluated at the value X.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the degree.
!
!    Input, real ( kind = 8 ) C(0:M), the polynomial coefficients.  
!    C(I) is the coefficient of X^I.
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) R8POLY_VALUE_HORNER, the polynomial value.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) c(0:m)
  integer ( kind = 4 ) i
!  real ( kind = 8 ) r8poly_value !_horner
  real ( kind = 8 ) value
  real ( kind = 8 ) x

  value = c(m)
  do i = m - 1, 0, -1
    value = value * x + c(i)
  end do

  r8poly_value = value
!  r8poly_value_horner = value

  return
end
subroutine r8vec_max ( n, a, amax )

!*****************************************************************************80
!
!! R8VEC_MAX returns the maximum value in an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, real ( kind = 8 ) A(N), the array.
!
!    Output, real ( kind = 8 ) AMAX, the value of the largest entry.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) amax

  amax = maxval ( a(1:n) )

  return
end
subroutine r8vec_mean ( n, x, mean )

!*****************************************************************************80
!
!! R8VEC_MEAN returns the mean of an R8VEC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) X(N), the vector whose mean is desired.
!
!    Output, real ( kind = 8 ) MEAN, the mean, or average,
!    of the vector entries.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) mean
  real ( kind = 8 ) x(n)

  mean = sum ( x(1:n) ) / real ( n, kind = 8 )

  return
end
subroutine r8vec_min ( n, a, amin )

!*****************************************************************************80
!
!! R8VEC_MIN returns the minimum value of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, real ( kind = 8 ) A(N), the array.
!
!    Output, real ( kind = 8 ) AMIN, the value of the smallest entry.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) amin

  amin = minval ( a(1:n) )

  return
end
subroutine r8vec_variance ( n, x, variance )

!*****************************************************************************80
!
!! R8VEC_VARIANCE returns the variance of an R8VEC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 May 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) X(N), the vector whose variance is desired.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the vector entries.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) mean
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(n)

  call r8vec_mean ( n, x, mean )

  variance = sum ( ( x(1:n) - mean ) ** 2 )

  if ( 1 < n ) then
    variance = variance / real ( n - 1, kind = 8 )
  else
    variance = 0.0D+00
  end if

  return
end
subroutine timestamp ( )

!*****************************************************************************80
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!  Example:
!
!    31 May 2001   9:45:54.872 AM
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    None
!
  implicit none

  character ( len = 8 ) ampm
  integer ( kind = 4 ) d
  integer ( kind = 4 ) h
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) s
  integer ( kind = 4 ) values(8)
  integer ( kind = 4 ) y

  call date_and_time ( values = values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  if ( h < 12 ) then
    ampm = 'AM'
  else if ( h == 12 ) then
    if ( n == 0 .and. s == 0 ) then
      ampm = 'Noon'
    else
      ampm = 'PM'
    end if
  else
    h = h - 12
    if ( h < 12 ) then
      ampm = 'PM'
    else if ( h == 12 ) then
      if ( n == 0 .and. s == 0 ) then
        ampm = 'Midnight'
      else
        ampm = 'AM'
      end if
    end if
  end if

  write ( *, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end

END MODULE MOD_LUDCMP

