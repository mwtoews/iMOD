!!  Copyright (C) Stichting Deltares, 2005-2014.
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
MODULE MOD_LUDCMP

CONTAINS

 !###====================================================================
 SUBROUTINE LUDCMP(AA,INDX,N)
 !###====================================================================
 !###====================================================================
 IMPLICIT NONE
 REAL,PARAMETER :: TINY=1.0E-20
 INTEGER,INTENT(IN)   :: N
 INTEGER,DIMENSION(N),INTENT(OUT) :: INDX
 REAL,DIMENSION(N,N),INTENT(INOUT)  :: AA
 REAL,ALLOCATABLE,DIMENSION(:) :: VV
 INTEGER  :: I,IMAX,J,K,NMAX
 REAL     :: AAMAX,DUM,SUM

 NMAX=N; ALLOCATE(VV(NMAX))
 
 INDX=0

 DO I=1,N
  AAMAX=0.
  DO J=1,N
   IF(ABS(AA(I,J)).GT.AAMAX)AAMAX=ABS(AA(I,J))
  ENDDO
  IF(AAMAX.EQ.0.)THEN
   write(*,*) 'TENSOR: MATRIX=SINGULAR'
   STOP
  ENDIF
  VV(I)=1./AAMAX
 ENDDO
 DO J=1,N
  DO I=1,J-1
   SUM=AA(I,J)
   DO K=1,I-1
    SUM=SUM-AA(I,K)*AA(K,J)
   ENDDO
   AA(I,J)=SUM
  ENDDO
  AAMAX=0.
  DO I=J,N
   SUM=AA(I,J)
   DO K=1,J-1
    SUM=SUM-AA(I,K)*AA(K,J)
   ENDDO
   AA(I,J)=SUM
   DUM=VV(I)*ABS(SUM)
   IF(DUM.GE.AAMAX)THEN
    IMAX=I
    AAMAX=DUM
   ENDIF
  ENDDO
  IF(J.NE.IMAX)THEN
   DO K=1,N
    DUM=AA(IMAX,K)
    AA(IMAX,K)=AA(J,K)
    AA(J,K)=DUM
   ENDDO
   VV(IMAX)=VV(J)
  ENDIF
  INDX(J)=IMAX
  IF(AA(J,J).EQ.0.)AA(J,J)=TINY
  IF(J.NE.N)THEN
   DUM=1./AA(J,J)
   DO I=J+1,N
    AA(I,J)=AA(I,J)*DUM
   ENDDO
  ENDIF
 ENDDO

 DEALLOCATE(VV)
 
 END SUBROUTINE LUDCMP

 !###====================================================================
 SUBROUTINE LUBKSB(AA,INDX,BB,N)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN)   :: N
 REAL,DIMENSION(N,N),INTENT(INOUT)  :: AA
 REAL,DIMENSION(N),INTENT(INOUT) :: BB
 INTEGER,DIMENSION(N),INTENT(IN) :: INDX
 INTEGER  :: I,II,J,LL
 REAL     :: SUM

 II=0
 DO I=1,N
  LL=INDX(I)
  SUM=BB(LL)
  BB(LL)=BB(I)
  IF(II.NE.0)THEN
   DO J=II,I-1
    SUM=SUM-AA(I,J)*BB(J)
   ENDDO
  ELSE IF(SUM.NE.0.)THEN
   II=I
  ENDIF
  BB(I)=SUM
 ENDDO
 DO I=N,1,-1
  SUM=BB(I)
  DO J=I+1,N
   SUM=SUM-AA(I,J)*BB(J)
  ENDDO
  BB(I)=SUM/AA(I,I)
 ENDDO

 END SUBROUTINE LUBKSB

END MODULE MOD_LUDCMP

