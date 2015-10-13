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
!!
MODULE MOD_INTERSECT

USE MOD_POLINT, ONLY : POL1LOCATE
USE MOD_INTERSECT_PAR
USE MOD_QKSORT

PRIVATE :: INTERSECT_EQUATION,INTERSECT_SORT

DOUBLE PRECISION,PRIVATE :: XBEGIN,YBEGIN,A,B
INTEGER,PRIVATE :: IHOR,IVER

CONTAINS

 !###======================================================================
 SUBROUTINE INTERSECT_EQUI(XMIN,XMAX,YMIN,YMAX,CSZ,X1,X2,Y1,Y2,N)
 !###======================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: XMIN,XMAX,YMIN,YMAX,CSZ
 REAL,INTENT(INOUT) :: X1,X2,Y1,Y2
 INTEGER,INTENT(INOUT) :: N
 DOUBLE PRECISION :: X,Y,XMN,XMX,YMN,YMX,DX,DY,LENG,CS !,BLENG
 INTEGER :: I,ICOL,IROW,N_IN

 IF(.NOT.ASSOCIATED(XA))ALLOCATE(XA(1000))
 IF(.NOT.ASSOCIATED(YA))ALLOCATE(YA(1000))
 IF(.NOT.ASSOCIATED(LN))ALLOCATE(LN(1000))

 N_IN=N
 
 CS=CSZ

 IF(.NOT.INTERSECT_EQUATION(XMIN,XMAX,YMIN,YMAX,X1,X2,Y1,Y2,N))RETURN

 !## find search box - result can be negative, does not matter!
 I=1
 XMN=XMIN+CS*(INT((X1-XMIN)/CS)+I)
 I=0
 DX=X2-XMIN
 IF(MOD(DX,CS).EQ.0.0)I=-1
 XMX=XMIN+CS*(INT((X2-XMIN)/CS)+I)

 Y =MIN(Y1,Y2)
 I =0
 DY=YMAX-Y
 IF(MOD(DY,CS).EQ.0.0)I=-1
 YMN=YMAX-CS*(INT((YMAX-Y)/CS)+I)
 Y =MAX(Y1,Y2)
 I =1
 YMX=YMAX-CS*(INT((YMAX-Y)/CS)+I)

 !## continue seach rest of intersections
 !## try intersections with y-axes firstly
 Y=YMN-CS
 DO
  Y=Y+CS
  IF(Y.GT.YMX)EXIT
  N=N+1
  CALL INTERSECT_RESIZEVECTORS(N)
  YA(N)=Y
  IF(IVER.EQ.1)THEN
   XA(N)=X1 !## same as xmx
  ELSE
   XA(N)=(Y-B)/A
  ENDIF
 ENDDO
 !## try intersections with x-axes secondly
 X=XMN-CS
 DO
  X=X+CS
  IF(X.GT.XMX)EXIT
  N=N+1
  CALL INTERSECT_RESIZEVECTORS(N)
  XA(N)=X
  IF(IHOR.EQ.1)THEN
   YA(N)=Y1 !MN !## same as ymx
  ELSE
   YA(N)=A*X+B
  ENDIF
 ENDDO

 DX=X1-X2; DY=Y2-Y1
 CALL INTERSECT_SORT(DX,DY,N_IN+1,N)

 !## sample each of the point to determine irow/icol, overwrite point with this
 !## skip first and last, they represented already by the second and one-last point
 DO I=N_IN+2,N
  !## mid point
  X      =(XA(I-1)+XA(I))/2.0
  Y      =(YA(I-1)+YA(I))/2.0

  ICOL   = INT((X-XMIN)/CS)+1
  IROW   = INT((YMAX-Y)/CS)+1

  DX     = XA(I)-XA(I-1)
  DY     = YA(I)-YA(I-1)
  LENG=DX**2.0+DY**2.0; IF(LENG.GT.0.0)LENG=SQRT(LENG)

  !## store results
  XA(I-1)= REAL(ICOL)
  YA(I-1)= REAL(IROW)
  LN(I-1)= LENG
 END DO
 N=N-1

 END SUBROUTINE INTERSECT_EQUI

 !###======================================================================
 SUBROUTINE INTERSECT_NONEQUI(DELR,DELC,NROW,NCOL,X1,X2,Y1,Y2,N)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NROW,NCOL
 REAL,INTENT(IN),DIMENSION(0:NCOL) :: DELR
 REAL,INTENT(IN),DIMENSION(0:NROW) :: DELC
 REAL,INTENT(INOUT) :: X1,X2,Y1,Y2
 INTEGER,INTENT(INOUT) :: N
 DOUBLE PRECISION :: X,Y,DX,DY,LENG
 INTEGER :: I,ICOL,IROW,IMN,IMX,N_IN

 IF(.NOT.ASSOCIATED(XA))ALLOCATE(XA(1000))
 IF(.NOT.ASSOCIATED(YA))ALLOCATE(YA(1000))
 IF(.NOT.ASSOCIATED(LN))ALLOCATE(LN(1000))

 N_IN=N
 
 IF(.NOT.INTERSECT_EQUATION(DELR(0),DELR(NCOL),DELC(NROW),DELC(0),X1,X2,Y1,Y2,N))RETURN

 !## continue seach rest of intersections
 !## try intersections with y-axes firstly
 IMN=NROW
 IF(MIN(Y1,Y2).GT.DELC(NROW))CALL POL1LOCATE(DELC,NROW+1,REAL(MIN(Y1,Y2),8),IMN)
 IMX=1
 IF(MAX(Y1,Y2).LT.DELC(0))CALL POL1LOCATE(DELC,NROW+1,REAL(MAX(Y1,Y2),8),IMX)
 IMX=IMX-1

 !## need only in between lines!
 DO I=IMX+1,IMN-1
  N    =N+1
  CALL INTERSECT_RESIZEVECTORS(N)
  Y    =DELC(I)
  YA(N)=Y
  IF(IVER.EQ.1)THEN
   XA(N)=X1  !## same as xmx
  ELSE
   XA(N)=(Y-B)/A
  ENDIF
 END DO

 !## try intersections with x-axes secondly
 IMN=1
 IF(MIN(X1,X2).GT.DELR(0))CALL POL1LOCATE(DELR,NCOL+1,REAL(MIN(X1,X2),8),IMN)
 IMN=IMN-1
 IMX=NCOL
 IF(MAX(X1,X2).LT.DELR(NCOL))CALL POL1LOCATE(DELR,NCOL+1,REAL(MAX(X1,X2),8),IMX)

 !## need only in between lines
 DO I=IMN+1,IMX-1
  N    =N+1
  CALL INTERSECT_RESIZEVECTORS(N)
  X    =DELR(I)
  XA(N)=X
  IF(IHOR.EQ.1)THEN
   YA(N)=Y1 !MN !## same as ymx
  ELSE
   YA(N)=A*X+B
  ENDIF
 END DO

 DX=X1-X2; DY=Y2-Y1
 CALL INTERSECT_SORT(DX,DY,N_IN+1,N)

 !## sample each of the point to determine irow/icol, overwrite point with this
 !## skip first and last, they represented already by the second and one-last point
 DO I=2,N
 !## mid point
  X   =(XA(I-1)+XA(I))/2.0
  Y   =(YA(I-1)+YA(I))/2.0

  CALL POL1LOCATE(DELR,NCOL+1,REAL(X,8),ICOL)
  CALL POL1LOCATE(DELC,NROW+1,REAL(Y,8),IROW)

  DX  =XA(I)-XA(I-1)
  DY  =YA(I)-YA(I-1)
  LENG=SQRT(DX**2.0+DY**2.0)
  !## store results
  XA(I-1)=REAL(ICOL)
  YA(I-1)=REAL(IROW)
  LN(I-1)=LENG

 END DO
 N=N-1

 END SUBROUTINE INTERSECT_NONEQUI

 !###======================================================================
 SUBROUTINE INTERSECT_NULLIFY()
 !###======================================================================
 IMPLICIT NONE
 
 NULLIFY(XA,YA,LN)
  
 END SUBROUTINE INTERSECT_NULLIFY

 !###======================================================================
 SUBROUTINE INTERSECT_DEALLOCATE()
 !###======================================================================
 IMPLICIT NONE
 
 IF(ASSOCIATED(XA))DEALLOCATE(XA)
 IF(ASSOCIATED(YA))DEALLOCATE(YA) 
 IF(ASSOCIATED(LN))DEALLOCATE(LN)
  
 END SUBROUTINE INTERSECT_DEALLOCATE
 
 !###======================================================================
 SUBROUTINE INTERSECT_RESIZEVECTORS(N)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,PARAMETER :: DN=100
 INTEGER,INTENT(IN) :: N
 INTEGER :: MM,NN
 
 IF(N.LE.SIZE(XA))RETURN
 
 NN=SIZE(XA)
 MM=NN+DN
 
 ALLOCATE(XA_DUMMY(MM),YA_DUMMY(MM),LN_DUMMY(MM))
 XA_DUMMY(1:NN)=XA(1:NN); YA_DUMMY(1:NN)=YA(1:NN); LN_DUMMY(1:NN)=LN(1:NN)  
 DEALLOCATE(XA,YA,LN)
 XA=>XA_DUMMY; YA=>YA_DUMMY; LN=>LN_DUMMY
 
 END SUBROUTINE INTERSECT_RESIZEVECTORS
 
 !###======================================================================
 LOGICAL FUNCTION INTERSECT_EQUATION(XMIN,XMAX,YMIN,YMAX,X1,X2,Y1,Y2,N)
 !###======================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: XMIN,XMAX,YMIN,YMAX
 REAL,INTENT(INOUT) :: X1,X2,Y1,Y2
 INTEGER,INTENT(INOUT) :: N
 DOUBLE PRECISION :: X,Y,DX,DY

 INTERSECT_EQUATION=.FALSE.

 IF((MIN(X1,X2).GT.XMAX.OR.MAX(X1,X2).LT.XMIN).OR. &
    (MIN(Y1,Y2).GT.YMAX.OR.MAX(Y1,Y2).LT.YMIN))RETURN

 XBEGIN=X1; YBEGIN=Y1

 !## arrange x1,x2,y1,y2 such that x1<x2
 IF(X1.GT.X2)THEN
  X =X1; Y =Y1
  X1=X2; Y1=Y2
  X2=X;  Y2=Y
 ENDIF

 !## adjust perfect 45/135/215,305 aanpassen
 DX=X2-X1; DY=Y2-Y1
 IF(ABS(DX).EQ.ABS(DY))X1=X1+1.0

 !## use always mid between point x1,y1 and x2,y2 as first position
 N=N+1; CALL INTERSECT_RESIZEVECTORS(N) 
 XA(N)= X1; YA(N)= Y1
 N=N+1; CALL INTERSECT_RESIZEVECTORS(N) 
 XA(N)= X2; YA(N)= Y2

 !## find mathematical expression for line: y=ax+b
 DX=X2-X1; DY=Y2-Y1
 IVER=0; IHOR=0
 IF(DX.EQ.0.0)IVER=1
 IF(DY.EQ.0.0)IHOR=1
 IF(DX.NE.0.0)THEN 
  A=DY/DX
  B=Y1-A*X1
 ENDIF

 INTERSECT_EQUATION=.TRUE.

 END FUNCTION INTERSECT_EQUATION

 !###======================================================================
 SUBROUTINE INTERSECT_SORT(DX,DY,N_IN,N)
 !###======================================================================
 IMPLICIT NONE
 DOUBLE PRECISION,INTENT(IN) :: DX,DY
 INTEGER,INTENT(IN) :: N,N_IN
 DOUBLE PRECISION :: X,Y
 INTEGER :: I

 !## sort intersections, determined by the one with the largest difference
 IF(ABS(DX).GE.ABS(DY))THEN
  CALL QKSORTDOUBLE2(XA(N_IN:),YA(N_IN:),(N-N_IN)+1)
 ELSE
  CALL QKSORTDOUBLE2(YA(N_IN:),XA(N_IN:),(N-N_IN)+1)
 ENDIF

 !## resort - if neccessary
 IF(XA(N_IN).NE.XBEGIN.OR.YA(N_IN).NE.YBEGIN)THEN
!  DO I=N_IN,N/2
  DO I=N_IN,N_IN+((N-N_IN)/2)
   X        =XA(I)
   XA(I)    =XA(N-I+N_IN) !+1)
   XA(N-I+N_IN)=X
!   XA(N-I+N_IN+1)=X
   Y        =YA(I)
   YA(I)    =YA(N-I+N_IN) !+1)
   YA(N-I+N_IN)=Y
!   YA(N-I+N_IN+1)=Y
  END DO
 ENDIF

 END SUBROUTINE INTERSECT_SORT

END MODULE
